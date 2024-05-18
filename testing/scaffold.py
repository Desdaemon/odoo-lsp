#!/usr/bin/env python3
# pyright: strict
import sys
import os
from pathlib import Path
from shutil import copyfile

assert __name__ == "__main__", "Not supported as a library."


def die(msg: str):
    print(msg, file=sys.stderr)
    exit(1)


args = list(sys.argv)
if args and args[0] == "./scaffold.py":
    args.pop(0)

if len(args) < 1:
    die("Usage: scaffold.py fixtures/fixture_dir [addon1 addon2 ..]")

fixture_dir = Path(args.pop(0))
os.makedirs(fixture_dir, exist_ok=True)
copyfile("template.py", fixture_dir.joinpath("test_main.py"))
with open(fixture_dir.joinpath(".odoo_lsp"), "w+") as config:
    config.write('{"module":{"roots":["."]}}')
for addon in args:
    os.mkdir(fixture_dir.joinpath(addon))
    with open(fixture_dir.joinpath(addon, "__manifest__.py"), "w+") as manifest:
        manifest.write('{"name": "%s"}' % addon)
