import pytest

from src.ast import ast_nodes
from src.lexer.tokens import Token, TokenType
from src.parser.types import Operator
from tests.static.parser_expr_complex_tests_data import VALID_SINGLE_INFIX_OPERATIONS_AND_EXPECTED
from tests.utils.payloads import (
    ExpectedCallExpression,
    ExpectedInfixOperation,
    ExpectedPrefixOperation,
)


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
    def test_single_valid_prefix_operation(
        self, expression_stmt: ast_nodes.ExpressionStatement, expected: ExpectedPrefixOperation
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
        """
        expr = expression_stmt.expression
        assert isinstance(
            expr, ast_nodes.PrefixOperation
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."
        assert expr.operator == expected.operator, "Invalid Prefix operation operator."
        assert expr.right == expected.operand, "Invalid Prefix operation operand."

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        VALID_SINGLE_INFIX_OPERATIONS_AND_EXPECTED,
        indirect=["lexer_mock"],
    )
    def test_single_valid_infix_operation(
        self, expression_stmt: ast_nodes.ExpressionStatement, expected: ExpectedInfixOperation
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
        """
        expr = expression_stmt.expression
        assert isinstance(
            expr, ast_nodes.InfixOperation
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."
        assert expr.operator == expected.operator, "Invalid Infix operation operator."
        assert expr.left == expected.left_operand, "Invalid Infix operation left operand."
        assert expr.right == expected.right_operand, "Invalid Infix operation right operand."

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        [
            (
                [
                    Token(TokenType.IDENT, "func"),
                    Token(TokenType.LPAREN, "("),
                    Token(TokenType.RPAREN, ")"),
                    Token(TokenType.ENDL, "\n"),
                ],
                ExpectedCallExpression("func", []),
            ),
            (
                [
                    Token(TokenType.IDENT, "abs"),
                    Token(TokenType.LPAREN, "("),
                    Token(TokenType.MINUS, "-"),
                    Token(TokenType.INT, "10"),
                    Token(TokenType.RPAREN, ")"),
                    Token(TokenType.ENDL, "\n"),
                ],
                ExpectedCallExpression(
                    "abs", [ast_nodes.PrefixOperation(Operator.MINUS, ast_nodes.IntegerLiteral(10))]
                ),
            ),
            (
                [
                    Token(TokenType.IDENT, "sum"),
                    Token(TokenType.LPAREN, "("),
                    Token(TokenType.FLOAT, "10.5"),
                    Token(TokenType.COMA, ","),
                    Token(TokenType.MINUS, "-"),
                    Token(TokenType.INT, "1"),
                    Token(TokenType.COMA, ","),
                    Token(TokenType.IDENT, "a"),
                    Token(TokenType.PLUS, "+"),
                    Token(TokenType.IDENT, "b"),
                    Token(TokenType.RPAREN, ")"),
                    Token(TokenType.ENDL, "\n"),
                ],
                ExpectedCallExpression(
                    "sum",
                    [
                        ast_nodes.FloatLiteral(10.5),
                        ast_nodes.PrefixOperation(Operator.MINUS, ast_nodes.IntegerLiteral(1)),
                        ast_nodes.InfixOperation(
                            ast_nodes.Identifier("a"), Operator.PLUS, ast_nodes.Identifier("b")
                        ),
                    ],
                ),
            ),
        ],
        indirect=["lexer_mock"],
    )
    def test_single_valid_function_call(
        self, expression_stmt: ast_nodes.ExpressionStatement, expected: ExpectedCallExpression
    ) -> None:
        """
        Tests parser parsing single call expression correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ExpressionStatement.
        Assert: Underlying expression is CallExpression.
        Assert: CallExpression function name is equal to the expected.
        Assert: Number of arguments in CallExpression is equal to the expected.
        For: Each argument in CallExpression.
            Assert: Argument is equal to the expected.
        """
        expr = expression_stmt.expression
        assert isinstance(
            expr, ast_nodes.CallExpression
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."
        assert expr.func_name == expected.func_name, "Invalid function name in call expression."
        assert len(expr.arguments) == len(
            expected.args
        ), "Invalid number of parameters if call expression."
        for i, (arg, exp) in enumerate(zip(expr.arguments, expected.args)):
            assert arg == exp, f"Invalid argument #{i} in call epxression."
