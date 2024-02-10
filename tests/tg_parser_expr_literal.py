"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Test group parsing literal expressions.
"""
import pytest

from src.ast import ast_nodes
from src.ast.abstract import Expression
from src.lexer.tokens import Token, TokenType


class TestParserLiteralExpressionTg:
    """Test group for parsing of literal expressions."""

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
    def test_single_valid_identifier_expression(
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
    def test_single_valid_integer_literal_expression(
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
    def test_single_valid_float_literal_expression(
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
    def test_single_valid_boolean_literal_expression(
        self, expression_stmt: ast_nodes.ExpressionStatement, expected: bool
    ) -> None:
        """
        Tests parser parsing single boolean literal correctly.

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

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        [
            ([Token(TokenType.STRING, "True")], "True"),
            ([Token(TokenType.STRING, "False False")], "False False"),
        ],
        indirect=["lexer_mock"],
    )
    def test_single_valid_string_literal_expression(
        self, expression_stmt: ast_nodes.ExpressionStatement, expected: str
    ) -> None:
        """
        Tests parser parsing single string literal correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ExpressionStatement.
        Assert: Underlying expression is StringLiteral.
        Assert: StringLiteral value is equal to the expected.
        """
        expr = expression_stmt.expression
        assert isinstance(
            expr, ast_nodes.StringLiteral
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."

        assert expr.value == expected, "Invalid string literal value."

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        [
            (
                [
                    Token(TokenType.LSQUARE, "["),
                    Token(TokenType.INT, "1"),
                    Token(TokenType.COMA, ","),
                    Token(TokenType.INT, "2"),
                    Token(TokenType.COMA, ","),
                    Token(TokenType.IDENT, "f"),
                    Token(TokenType.LPAREN, "("),
                    Token(TokenType.RPAREN, ")"),
                    Token(TokenType.RSQUARE, "]"),
                ],
                [
                    ast_nodes.IntegerLiteral(1),
                    ast_nodes.IntegerLiteral(2),
                    ast_nodes.CallExpression(ast_nodes.Identifier("f"), []),
                ],
            ),
            (
                [
                    Token(TokenType.LSQUARE, "["),
                    Token(TokenType.RSQUARE, "]"),
                ],
                [],
            ),
        ],
        indirect=["lexer_mock"],
    )
    def test_single_valid_array_literal_expression(
        self, expression_stmt: ast_nodes.ExpressionStatement, expected: list[Expression]
    ) -> None:
        """
        Tests parser parsing single array literal correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ExpressionStatement.
        Assert: Underlying expression is ArrayLiteral.
        Assert: ArrayLiteral elements are equal to the expected.
        """
        expr = expression_stmt.expression
        assert isinstance(
            expr, ast_nodes.ArrayLiteral
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."

        assert expr.elements == expected, "Invalid array literal elements."
