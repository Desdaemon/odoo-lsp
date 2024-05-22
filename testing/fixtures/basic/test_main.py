import pytest
from collections import defaultdict
from pathlib import Path
from pytest_lsp import LanguageClient
from tree_sitter import Parser, Query, Tree
from deepdiff import DeepDiff  # type: ignore
from deepdiff.diff import DiffLevel
from deepdiff.operator import BaseOperator
from tree_sitter import Language
import tree_sitter_python as tspython
from lsprotocol.types import (
    DidOpenTextDocumentParams,
    TextDocumentItem,
    CompletionParams,
    TextDocumentIdentifier,
    Position,
    CompletionList,
    Diagnostic,
)


LANG_PY = Language(tspython.language())
QUERY_PY = Query(
    LANG_PY,
    """
((comment) @diag
  (#match? @diag "\\\\^diag "))

((comment) @complete
  (#match? @complete "\\\\^complete "))
""",
)


class Expected:
    __slots__ = ("diag", "complete")
    diag: list[tuple[Position, str]]
    complete: list[tuple[Position, list[str]]]

    def __init__(self):
        self.diag = []
        self.complete = []

class PositionOperator(BaseOperator):
    def give_up_diffing(self, level: DiffLevel, diff_instance: DeepDiff) -> bool:
        if isinstance(level.t1, Position) and isinstance(level.t2, Position):
            if level.t1 != level.t2:
                diff_instance.custom_report_result('values_changed', level)  # type: ignore
            return True
        return False

def inc(position: Position):
    return Position(position.line + 1, position.character + 1)

@pytest.mark.asyncio(scope="module")
async def test_python(client: LanguageClient, rootdir: str):
    files = {
        file: file.read_text() for file in Path(rootdir).joinpath("foo").rglob("*.py")
    }
    expected = defaultdict[Path, Expected](Expected)
    asts = dict[Path, Tree]()
    for file, text in files.items():
        parser = Parser(LANG_PY)
        ast = parser.parse(text.encode())
        asts[file] = ast
        for node, _ in QUERY_PY.captures(ast.root_node):
            assert (text := node.text), "node has no text"
            text = text.decode()
            if (offset := text.find("^diag ")) != -1:
                msg = text[offset + 6 :].strip()
                pos = Position(
                    node.start_point.row - 1, node.start_point.column + offset
                )
                expected[file].diag.append((pos, msg))
            elif (offset := text.find("^complete ")) != -1:
                completions = text[offset + 10 :].strip().split(" ")
                pos = Position(
                    node.start_point.row - 1, node.start_point.column + offset
                )
                expected[file].complete.append((pos, completions))

    unexpected: list[str] = []
    for file, text in files.items():
        client.text_document_did_open(
            DidOpenTextDocumentParams(
                TextDocumentItem(
                    uri=file.as_uri(),
                    language_id="python",
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
            assert isinstance(results, CompletionList)
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
                    f"complete: actual={' '.join(actual)}\n  at {file}:{inc(pos)}\n{' ' * node.start_point.column}{node_text}"
                )
    unexpected_len = len(unexpected)
    assert not unexpected_len, "\n".join(unexpected)


@pytest.mark.asyncio(scope="module")
async def test_xml_completions(client: LanguageClient, rootdir: str):
    client.text_document_did_open(
        DidOpenTextDocumentParams(
            TextDocumentItem(
                uri=f"file://{rootdir}/foo/records.xml",
                language_id="xml",
                version=1,
                text=Path(f"{rootdir}/foo/records.xml").read_text(),
            )
        )
    )
    await client.wait_for_notification("textDocument/publishDiagnostics")

    results = await client.text_document_completion_async(
        CompletionParams(
            TextDocumentIdentifier(uri=f"file://{rootdir}/foo/records.xml"),
            Position(2, 17),
        )
    )
    assert isinstance(results, CompletionList)
    assert len(results.items) == 1
    assert [e.label for e in results.items] == ["bar"]


def splay_diag(diags: list[Diagnostic]):
    return ((d.range.start, d.message) for d in diags)
