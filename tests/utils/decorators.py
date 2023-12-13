# pylint: disable=redefined-outer-name
import pytest


def n_len_program(n: int) -> pytest.MarkDecorator:
    """Decorator parametrizing `ok_len_program` fixture's with provided value"""
    return pytest.mark.parametrize("ok_len_program", [n], indirect=True)
