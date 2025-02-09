import pytest
from pytest_lsp import LanguageClient
from testing.common import fixture_test

# from lsprotocol.types import ()


@pytest.mark.asyncio(loop_scope="module")
async def test_main(client: LanguageClient, rootdir: str):
    unexpected = await fixture_test(client, rootdir)
    assert not unexpected, '\n'.join(unexpected)
