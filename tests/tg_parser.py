import pytest

from src.ast import ast_nodes
from tests.static.parser_tests_data import INFIX_OPERATIONS_PRECEDENCE_AND_EXPECTED


class TestParserTg:
    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        INFIX_OPERATIONS_PRECEDENCE_AND_EXPECTED,
        indirect=["lexer_mock"],
    )
    def test_infix_operations_precedence(
        self, ok_len_program: ast_nodes.Program, expected: str
    ) -> None:
        """
        Tests parser parsing single prefix operation correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: str(program) equals expected.
        """
        assert str(ok_len_program) == expected
