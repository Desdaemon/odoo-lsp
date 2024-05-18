import pytest
from pytest_lsp import LanguageClient

# from lsprotocol.types import ()


@pytest.mark.asyncio
async def test_sanity(client: LanguageClient):
    """Ensure that the server implements completions correctly."""
    pass
