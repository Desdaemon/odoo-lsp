__all__ = [
    'fixture_test',
]

from collections import defaultdict
from pathlib import Path
from pytest_lsp import LanguageClient
from tree_sitter import Parser, Query, Tree
from deepdiff import DeepDiff  # type: ignore
from deepdiff.diff import DiffLevel
from deepdiff.operator import BaseOperator
from tree_sitter import Language
import tree_sitter_language_pack as tslangs
from lsprotocol.types import (
    DidOpenTextDocumentParams,
    TextDocumentItem,
    CompletionParams,
    TextDocumentIdentifier,
    Position,
    CompletionList,
    Diagnostic,
    TextDocumentPositionParams,
)
from typing import Literal

def prepare_query(glob: str, lang: Language, query: str):
    return (glob, lang, Query(lang, query))

QUERIES = {
    'python': prepare_query('**/*.py', tslangs.get_language('python'), r"""
        ((comment) @diag
        (#match? @diag "\\^diag "))

        ((comment) @complete
        (#match? @complete "\\^complete "))

        ((comment) @type
        (#match? @type "\\^type "))
    """),
    'xml': prepare_query('**/*.xml', tslangs.get_language('xml'), """
        ((Comment) @diag
        (#match? @diag "\\\\^diag "))

        ((Comment) @complete
        (#match? @complete "\\\\^complete "))
    """)
}

def splay_diag(diags: list[Diagnostic]):
    return ((d.range.start, d.message) for d in diags)

class Expected:
    diag: list[tuple[Position, str]]
    complete: list[tuple[Position, list[str]]]
    type_: list[tuple[Position, str]]

    def __init__(self):
        self.diag = []
        self.complete = []
        self.type_ = []

class PositionOperator(BaseOperator):
    def give_up_diffing(self, level: DiffLevel, diff_instance: DeepDiff) -> bool:
        if isinstance(level.t1, Position) and isinstance(level.t2, Position):
            if level.t1 != level.t2:
                diff_instance.custom_report_result('values_changed', level)  # type: ignore
            return True
        return False

def inc(position: Position):
    return Position(position.line + 1, position.character + 1)

async def fixture_test(client: LanguageClient, rootdir: str, language: Literal['python', 'xml'] = 'python'):
    glob, lang, query = QUERIES[language]
    files = {
        file: file.read_text() for file in Path(rootdir).rglob(glob)
    }
    expected = defaultdict[Path, Expected](Expected)
    asts = dict[Path, Tree]()
    for file, text in files.items():
        parser = Parser(lang)
        ast = parser.parse(text.encode())
        asts[file] = ast
        for node in (node for nodes in query.captures(ast.root_node).values() for node in nodes):
            assert (text := node.text), "node has no text"
            text = text.decode()
            if (offset := text.find("^diag ")) != -1:
                msg = text[offset + 6 :].strip()
                pos = Position(
                    node.start_point.row - 1, node.start_point.column + offset
                )
                expected[file].diag.append((pos, msg))
            elif (offset := text.find("^complete ")) != -1:
                raw_completions = text[offset + 10 :].strip()
                if language == 'xml':
                    raw_completions = raw_completions.removesuffix('-->').rstrip()
                completions = raw_completions.split(" ")
                pos = Position(
                    node.start_point.row - 1, node.start_point.column + offset
                )
                expected[file].complete.append((pos, completions))
            elif (offset := text.find("^type ")) != -1:
                type_ = text[offset + 6 :].strip()
                pos = Position(
                    node.start_point.row - 1, node.start_point.column + offset
                )
                expected[file].type_.append((pos, type_))

    unexpected: list[str] = []
    for file, text in files.items():
        client.text_document_did_open(
            DidOpenTextDocumentParams(
                TextDocumentItem(
                    uri=file.as_uri(),
                    language_id=language,
                    version=1,
                    text=text,
                )
            )
        )
        await client.wait_for_notification("textDocument/publishDiagnostics")
        actual_diagnostics = list(splay_diag(client.diagnostics[file.as_uri()]))
        if diff := DeepDiff(expected[file].diag, actual_diagnostics, custom_operators=[PositionOperator(types=[Position])], ignore_order=True):
            for extra in diff.pop("iterable_item_added", {}).values():  # type: ignore
                unexpected.append(f"diag: extra {extra}\n  at {file}:{inc(extra[0])}")  # type: ignore
            for missing in diff.pop("iterable_item_removed", {}).values():  # type: ignore
                unexpected.append(f"diag: missing {missing}\n  at {file}:{inc(missing[0])}")  # type: ignore
            for mismatch in diff.pop("values_changed", {}).values():  # type: ignore
                unexpected.append(
                    f"diag: expected={mismatch['old_value']!r} actual={mismatch['new_value']!r}\n  at {file}"
                )
            if diff:
                unexpected.append(f"diag: unexpected {diff}\n  at {file}")

        for pos, expected_completion in expected[file].complete:
            results = await client.text_document_completion_async(
                CompletionParams(
                    TextDocumentIdentifier(uri=file.as_uri()),
                    pos,
                )
            )
            if not expected_completion:
                assert not results
                continue
            if not isinstance(results, CompletionList):
                unexpected.append(f"complete: unexpected empty list\n  at {file}:{inc(pos)}")
                continue
            actual = [e.label for e in results.items]
            if actual != expected_completion:
                node = asts[file].root_node.named_descendant_for_point_range(
                    (pos.line, pos.character), (pos.line, 9999)
                )
                assert node
                if text := node.text:
                    node_text = text.decode()
                else:
                    node_text = ""
                unexpected.append(
                    f"complete: actual={' '.join(actual)}\n"
                    f"  at {file}:{inc(pos)}\n"
                    f"{' ' * node.start_point.column}{node_text}"
                )

        for pos, expected_type in expected[file].type_:
            type_ = await client.protocol.send_request_async('odoo-lsp/inspect-type',
                TextDocumentPositionParams(TextDocumentIdentifier(file.as_uri()), pos))
            if type_ != expected_type:
                node = asts[file].root_node.named_descendant_for_point_range(
                    (pos.line, pos.character), (pos.line, 9999)
                )
                assert node
                if text := node.text:
                    node_text = text.decode()
                else:
                    node_text = ""
                unexpected.append(
                    f"model: actual={type_!r}\n"
                    f"  at {file}:{inc(pos)}\n"
                    f"{' ' * node.start_point.column}{node_text}"
                )
    return unexpected
