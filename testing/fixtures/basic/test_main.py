import pytest
from pathlib import Path
from pytest_lsp import LanguageClient
from lsprotocol.types import (
    DidOpenTextDocumentParams,
    TextDocumentItem,
    CompletionParams,
    TextDocumentIdentifier,
    Position,
    CompletionList,
    Diagnostic,
)


@pytest.mark.asyncio(scope="module")
async def test_completions(client: LanguageClient, rootdir: str):
    """Test various basic completions."""

    client.text_document_did_open(
        DidOpenTextDocumentParams(
            TextDocumentItem(
                uri=f"file://{rootdir}/foo/models.py",
                language_id="python",
                version=1,
                text=Path(f"{rootdir}/foo/models.py").read_text(),
            )
        )
    )
    await client.wait_for_notification("textDocument/publishDiagnostics")

    results = await client.text_document_completion_async(
        CompletionParams(
            TextDocumentIdentifier(uri=f"file://{rootdir}/foo/models.py"),
            Position(4, 18),
        )
    )
    if isinstance(results, CompletionList):
        results = results.items
    assert results
    assert [e.label for e in results] == ["bar", "derived.bar", "foo", "foob"]


def splay_diag(diags: list[Diagnostic]):
    return (
        (
            d.range.start.line,
            d.range.start.character,
            d.range.end.line,
            d.range.end.character,
            d.message,
        )
        for d in diags
    )


@pytest.mark.asyncio(scope="module")
async def test_diagnostics(client: LanguageClient, rootdir: str):
    client.text_document_did_open(
        DidOpenTextDocumentParams(
            TextDocumentItem(
                uri=f"file://{rootdir}/foo/models.py",
                language_id="python",
                version=1,
                text=Path(f"{rootdir}/foo/models.py").read_text(),
            )
        )
    )
    await client.wait_for_notification("textDocument/publishDiagnostics")
    diagnostics = client.diagnostics[Path(f"{rootdir}/foo/models.py").as_uri()]
    assert list(splay_diag(diagnostics)) == [
        (7, 13, 7, 16, "Model `foo` has no field `foo`"),
        (8, 24, 8, 27, "Model `foo` has no field `foo`"),
        (9, 21, 9, 24, "Model `foo` has no field `foo`"),
        (10, 18, 10, 20, "`fo` is not a valid model name"),
    ]
