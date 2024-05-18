import os
import pytest

@pytest.fixture(scope="module")
def rootdir():
    return os.path.dirname(os.path.realpath(__file__))
