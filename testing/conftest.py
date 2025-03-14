# pyright: strict

from lsprotocol.types import (
    ClientCapabilities,
    CompletionClientCapabilities,
    InitializeParams,
    PublishDiagnosticsClientCapabilities,
    TextDocumentClientCapabilities,
    WindowClientCapabilities,
    WorkspaceFolder,
)

import pytest
import pytest_lsp
import os
import subprocess
from pathlib import Path
from pytest_lsp import ClientServerConfig
from pytest_lsp import LanguageClient
from shutil import which


__dirname = os.path.dirname(os.path.realpath(__file__))

if lsp_devtools := which("lsp-devtools"):
    odoocmd = [lsp_devtools, "agent", "--", f"{__dirname}/../target/debug/odoo-lsp"]
else:
    # odoocmd = [f"{__dirname}/../target/debug/odoo-lsp"]
    odoocmd = ["cargo", "run"]
ODOO_ENV = dict(os.environ)
ODOO_ENV.setdefault('RUST_LOG', 'trace')
ODOO_ENV.setdefault('ODOO_LSP_LOG', '1')


@pytest.fixture
def rootdir():
    return __dirname


@pytest.fixture(autouse=True, scope="session")
def setup():
    subprocess.run(["cargo", "build"], env=dict(os.environ, CARGO_TERM_COLOR="always"))


assert os.getcwd().startswith(__dirname), "Tests must be executed from within /testing"


@pytest_lsp.fixture(
    scope="module",
    config=ClientServerConfig(server_command=odoocmd, server_env=ODOO_ENV),
)
async def client(lsp_client: LanguageClient, rootdir: str):
    params = InitializeParams(
        workspace_folders=[
            WorkspaceFolder(uri=Path(rootdir).as_uri(), name="odoo-lsp")
        ],
        capabilities=ClientCapabilities(
            window=WindowClientCapabilities(work_done_progress=True),
            text_document=TextDocumentClientCapabilities(
                publish_diagnostics=PublishDiagnosticsClientCapabilities(),
                completion=CompletionClientCapabilities(),
            ),
        ),
    )
    await lsp_client.initialize_session(params)
    yield
    await lsp_client.shutdown_session()
