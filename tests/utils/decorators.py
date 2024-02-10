"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Test decorators.
"""
import pytest


def n_len_program(n: int) -> pytest.MarkDecorator:
    """Decorator changing `expected_stmts_len` fixture's return value to provided value."""
    return pytest.mark.parametrize("expected_stmts_len", [n], indirect=True)
