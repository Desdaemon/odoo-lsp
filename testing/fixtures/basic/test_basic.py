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
)
from testing import common

@pytest.mark.asyncio(loop_scope="module")
async def test_python(client: LanguageClient, rootdir: str):
    unexpected = await common.fixture_test(client, rootdir)
    assert not unexpected, '\n'.join(unexpected)


@pytest.mark.asyncio(loop_scope="module")
async def test_xml_completions(client: LanguageClient, rootdir: str):
    # client.text_document_did_open(
    #     DidOpenTextDocumentParams(
    #         TextDocumentItem(
    #             uri=f"file://{rootdir}/foo/records.xml",
    #             language_id="xml",
    #             version=1,
    #             text=Path(f"{rootdir}/foo/records.xml").read_text(),
    #         )
    #     )
    # )
    # await client.wait_for_notification("textDocument/publishDiagnostics")

    # results = await client.text_document_completion_async(
    #     CompletionParams(
    #         TextDocumentIdentifier(uri=f"file://{rootdir}/foo/records.xml"),
    #         Position(2, 17),
    #     )
    # )
    # assert isinstance(results, CompletionList)
    # assert len(results.items) == 1
    # assert [e.label for e in results.items] == ["bar"]
    unexpected = await common.fixture_test(client, rootdir, language='xml')
    assert not unexpected, '\n'.join(unexpected)
