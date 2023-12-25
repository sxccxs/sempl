import pytest

from src.ast import ast_nodes
from src.lexer.tokens import Token, TokenType


class TestParserLiteralExpressionTg:
    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        [
            ([Token(TokenType.IDENT, "abc_")], "abc_"),
            (
                [
                    Token(TokenType.IDENT, "x_1a2"),
                    Token(TokenType.ENDL, "\n"),
                ],
                "x_1a2",
            ),
            ([Token(TokenType.IDENT, "q")], "q"),
        ],
        indirect=["lexer_mock"],
    )
    def test_single_identifier_expression(
        self, expression_stmt: ast_nodes.ExpressionStatement, expected: str
    ) -> None:
        """
        Tests parser parsing single identifier expression correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ExpressionStatement.
        Assert: Underlying expression is Identifier.
        Assert: Indetifier value is equal to the expected value.
        """
        expr = expression_stmt.expression
        assert isinstance(
            expr, ast_nodes.Identifier
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."

        assert expr.value == expected, "Invalid identifier value."

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        [
            (
                [
                    Token(TokenType.INT, "5"),
                    Token(TokenType.ENDL, "\n"),
                ],
                5,
            ),
            ([Token(TokenType.INT, "10123")], 10123),
        ],
        indirect=["lexer_mock"],
    )
    def test_single_integer_literal_expression(
        self, expression_stmt: ast_nodes.ExpressionStatement, expected: int
    ) -> None:
        """
        Tests parser parsing single integer literal correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ExpressionStatement.
        Assert: Underlying expression is IntegerLiteral.
        Assert: IntegerLiteral value is equal to the expected value.
        """
        expr = expression_stmt.expression
        assert isinstance(
            expr, ast_nodes.IntegerLiteral
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."

        assert expr.value == expected, "Invalid integer literal value."

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        [
            ([Token(TokenType.FLOAT, "5.")], 5.0),
            ([Token(TokenType.FLOAT, "10.25")], 10.25),
            (
                [
                    Token(TokenType.FLOAT, "0.3"),
                    Token(TokenType.ENDL, "\n"),
                ],
                0.3,
            ),
        ],
        indirect=["lexer_mock"],
    )
    def test_single_float_literal_expression(
        self, expression_stmt: ast_nodes.ExpressionStatement, expected: float
    ) -> None:
        """
        Tests parser parsing single float literal correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ExpressionStatement.
        Assert: Underlying expression is FloatLiteral.
        Assert: FloatLiteral value is equal to the expected value.
        """
        expr = expression_stmt.expression
        assert isinstance(
            expr, ast_nodes.FloatLiteral
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."

        assert expr.value == expected, "Invalid float literal value."

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        [
            ([Token(TokenType.TRUE, "True")], True),
            ([Token(TokenType.FALSE, "False")], False),
        ],
        indirect=["lexer_mock"],
    )
    def test_single_boolean_literal_expression(
        self, expression_stmt: ast_nodes.ExpressionStatement, expected: bool
    ) -> None:
        """
        Tests parser parsing single integer literal correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ExpressionStatement.
        Assert: Underlying expression is BooleanLiteral.
        Assert: BooleanLiteral value is equal to the expected value.
        """
        expr = expression_stmt.expression
        assert isinstance(
            expr, ast_nodes.BooleanLiteral
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."

        assert expr.value == expected, "Invalid boolean literal value."
