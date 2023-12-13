import pytest

from src.ast import ast_nodes
from src.lexer.tokens import Token, TokenType
from tests.utils.payloads import (ExpectedInfixOperation,
                                  ExpectedPrefixOperation)
from tests.utils.decorators import n_len_program

VALID_SINGLE_INFIX_OPERATIONS_AND_EXPECTED: list[tuple[list[Token], ExpectedInfixOperation]] = [
    (
        [Token(TokenType.INT, "5"), Token(TokenType.PLUS, "+"), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "+", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.MINUS, "-"), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "-", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [
            Token(TokenType.INT, "5"),
            Token(TokenType.ASTERIX, "*"),
            Token(TokenType.INT, "5"),
        ],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "*", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.SLASH, "/"), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "/", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.GT, ">"), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), ">", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.GTEQ, ">="), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), ">=", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.LT, "<"), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "<", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.LTEQ, "<="), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "<=", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.EQ, "=="), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "==", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [
            Token(TokenType.INT, "5"),
            Token(TokenType.NOT_EQ, "!="),
            Token(TokenType.INT, "5"),
        ],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "!=", ast_nodes.IntegerLiteral(5)),
    ),
]


class TestParserComplexExpressionTg:
    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        [
            (
                [Token(TokenType.MINUS, "-"), Token(TokenType.INT, "5")],
                ExpectedPrefixOperation("-", ast_nodes.IntegerLiteral(5)),
            ),
            (
                [Token(TokenType.MINUS, "+"), Token(TokenType.INT, "20")],
                ExpectedPrefixOperation("+", ast_nodes.IntegerLiteral(20)),
            ),
            (
                [Token(TokenType.MINUS, "-"), Token(TokenType.FLOAT, "5.2")],
                ExpectedPrefixOperation("-", ast_nodes.FloatLiteral(5.2)),
            ),
            (
                [Token(TokenType.MINUS, "+"), Token(TokenType.FLOAT, "0.3")],
                ExpectedPrefixOperation("+", ast_nodes.FloatLiteral(0.3)),
            ),
        ],
        indirect=["lexer_mock"],
    )
    @n_len_program(1)
    def test_single_valid_prefix_operation(
        self, ok_len_program: ast_nodes.Program, expected: ExpectedPrefixOperation
    ) -> None:
        """
        Tests parser parsing single prefix operation correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ExpressionStatement.
        Assert: Underlying expression is PrefixOperation.
        Assert: PrefixOperation operator is equal to the expected.
        Assert: PrefixOperation operand is equal to the expected.
        Assert: PrefixOperation token_literal is equal to expected operator.
        Assert: Statement token_literal is equal to PrefixOperation token_literal.
        """
        stmt = ok_len_program.statements[0]
        assert isinstance(
            stmt, ast_nodes.ExpressionStatement
        ), f"Unexpected statement of type `{type(stmt)}`."
        expr = stmt.expression
        assert isinstance(
            expr, ast_nodes.PrefixOperation
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."
        assert expr.operator == expected.operator, "Invalid Prefix operation operator."
        assert expr.right == expected.operand, "Invalid Prefix operation operand."
        assert expr.token_literal == expected.operator, "Invalid Prefix operation token_literal."

        assert (
            expr.token_literal == stmt.token_literal
        ), "Invalid ExpressionStatement token_literal."

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        VALID_SINGLE_INFIX_OPERATIONS_AND_EXPECTED,
        indirect=["lexer_mock"],
    )
    @n_len_program(1)
    def test_single_valid_infix_operation(
        self, ok_len_program: ast_nodes.Program, expected: ExpectedInfixOperation
    ) -> None:
        """
        Tests parser parsing single prefix operation correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ExpressionStatement.
        Assert: Underlying expression is InfixOperation.
        Assert: InfixOperation operator is equal to the expected.
        Assert: InfixOperation left operand is equal to the expected.
        Assert: InfixOperation right operand is equal to the expected.
        Assert: InfixOperation token_literal is equal to expected operator.
        Assert: Statement token_literal is equal to InfixOperation token_literal.
        """
        stmt = ok_len_program.statements[0]
        assert isinstance(
            stmt, ast_nodes.ExpressionStatement
        ), f"Unexpected statement of type `{type(stmt)}`."
        expr = stmt.expression
        assert isinstance(
            expr, ast_nodes.InfixOperation
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."
        assert expr.operator == expected.operator, "Invalid Infix operation operator."
        assert expr.left == expected.left_operand, "Invalid Infix operation left operand."
        assert expr.right == expected.right_operand, "Invalid Infix operation right operand."
        assert expr.token_literal == expected.operator, "Invalid Infix operation token_literal."

        assert (
            expr.token_literal == stmt.token_literal
        ), "Invalid ExpressionStatement token_literal."
