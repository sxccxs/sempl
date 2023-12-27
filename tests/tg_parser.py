import pytest

from src.ast import ast_nodes
from tests.static.parser_tests_data import (
    GROUPED_EXPRESSION_AND_EXPECTED,
    INFIX_OPERATIONS_PRECEDENCE_AND_EXPECTED,
)
from tests.utils.decorators import n_len_program


class TestParserTg:
    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        INFIX_OPERATIONS_PRECEDENCE_AND_EXPECTED,
        indirect=["lexer_mock"],
    )
    @n_len_program(1)
    def test_infix_operations_precedence(
        self, ok_len_program: ast_nodes.Program, expected: str
    ) -> None:
        """
        Tests parser parsing single prefix operation correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: str(statement) equals expected.
        """
        stmt = ok_len_program.statements[0]
        assert str(stmt) == expected

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        GROUPED_EXPRESSION_AND_EXPECTED,
        indirect=["lexer_mock"],
    )
    @n_len_program(1)
    def test_grouped_expression(self, ok_len_program: ast_nodes.Program, expected: str) -> None:
        """
        Tests parser parsing grouped expressions (expressions in braces).

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: str(program) equals expected.
        """
        stmt = ok_len_program.statements[0]
        assert str(stmt) == expected
