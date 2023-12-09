# pylint: disable=redefined-outer-name
import pytest
from result import Ok

from src.ast import ast_nodes
from src.lexer.interfaces import ILexer
from src.lexer.tokens import Keyword, Token, TokenType
from src.parser.parser import Parser
from tests.mock.lexer_mock import LexerMock
from tests.static.parser_tests_data import (
    INFIX_OPERATIONS_PRECEDENCE_AND_EXPECTED,
    VALID_LET_STATEMENT_TOKENS_AND_EXPECTED,
    VALID_SINGLE_INFIX_OPERATIONS_AND_EXPECTED,
)
from tests.utils.payloads import (
    ExpectedInfixOperation,
    ExpectedLetStatement,
    ExpectedPrefixOperation,
)
from tests.utils.types import YieldFixture


@pytest.fixture
def lexer(request: pytest.FixtureRequest) -> YieldFixture[ILexer]:
    """Creates lexer mock object and provides tokens from request to it + EOF tokens."""
    lexer = LexerMock(strict=True)
    lexer.set_data(request.param)
    lexer.add_data(
        [Token(TokenType.EOF, "\0"), Token(TokenType.EOF, "\0")]
    )  # Add extra token as parser ends on current token, not peek token.
    yield lexer


@pytest.fixture
def parser(lexer: ILexer) -> YieldFixture[Parser]:
    """Creates parser from lexer fixture."""
    yield Parser(lexer)


def parse_ok_program_and_assert(parser: Parser, expected_stmts_len: int) -> ast_nodes.Program:
    """Parses program with provided parser and checks the result.
    The program is expected to be valid.

    Act: Parse program.
    Assert: No error returned.
    Assert: Program contains only `expected_stmts_len` statement.

    Args:
        parser (Parser): Parser.
        expected_stmts_len (int): Expected number of statements in result program.

    Returns:
        ast_nodes.Program: Parsed Program node.
    """
    program_result = parser.parse_program()
    assert isinstance(
        program_result, Ok
    ), f"Unexpected error returned: `{repr(program_result.err())}`."

    program = program_result.ok_value
    assert (
        len(program.statements) == expected_stmts_len
    ), "Invalid number of statementes in program."

    return program


class TestParser:
    @pytest.mark.parametrize(
        ("lexer", "expected_result"),
        VALID_LET_STATEMENT_TOKENS_AND_EXPECTED,
        indirect=["lexer"],
    )
    def test_single_correct_let_statement(
        self,
        parser: Parser,
        expected_result: ExpectedLetStatement,
    ) -> None:
        """Tests parser parsing single valid let statement correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement literal is `let`.
        Assert: Statement is LetStatement.
        Assert: Let statement mutability equals to expected.
        Assert: Let statement var_type.value and var_type.token_literal equals to expected type.
        Assert: Let statement var_name.value and var_name.token_literal equals to expected name.
        """
        program = parse_ok_program_and_assert(parser, 1)
        stmt = program.statements[0]
        assert stmt.token_literal == Keyword.LET.value, "Invalid let statement token literal."
        assert isinstance(
            stmt, ast_nodes.LetStatement
        ), f"Unexpected statement of type `{type(stmt)}`."

        assert stmt.is_mut == expected_result.mut, "Invalid mutability."

        assert stmt.var_type.value == expected_result.type, "Invalid var_type value."
        assert (
            stmt.var_type.token_literal == expected_result.type
        ), "Invalid var_type token literal."

        assert stmt.var_name.value == expected_result.name, "Invalid var_name value."
        assert (
            stmt.var_name.token_literal == expected_result.name
        ), "Invalid var_name token literal."

        # TODO: check for var_value

    @pytest.mark.parametrize(
        "lexer",
        [
            [
                Token(TokenType.RETURN, literal="return"),
            ],
            [
                Token(TokenType.RETURN, literal="return"),
                Token(TokenType.INT, literal="10"),
                Token(TokenType.ENDL, literal="\n"),
            ],
            [
                Token(TokenType.RETURN, literal="return"),
                Token(TokenType.IDENT, literal="a"),
                Token(TokenType.PLUS, literal="+"),
                Token(TokenType.IDENT, literal="b"),
                Token(TokenType.ENDL, literal="\n"),
            ],
        ],
        indirect=True,
    )
    def test_single_correct_return_statement(self, parser: Parser) -> None:
        """Tests parser parsing single valid return statement correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement literal is `return`.
        Assert: Statement is ReturnStatement.
        """
        program = parse_ok_program_and_assert(parser, 1)
        stmt = program.statements[0]

        assert stmt.token_literal == Keyword.RETURN.value, "Invalid return statement token literal."
        assert isinstance(
            stmt, ast_nodes.ReturnStatement
        ), f"Unexpected statement of type `{type(stmt)}`."

    @pytest.mark.parametrize(
        ("lexer", "expected"),
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
        indirect=["lexer"],
    )
    def test_single_identifier_expression(self, parser: Parser, expected: str) -> None:
        """Tests parser parsing single identifier expression correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ExpressionStatement.
        Assert: Underlying expression is Identifier.
        Assert: Indetifier value and token_literal are equal to the expected value.
        Assert: Statement token_literal is equal to Identifier token_literal.
        """
        program = parse_ok_program_and_assert(parser, 1)
        stmt = program.statements[0]
        assert isinstance(
            stmt, ast_nodes.ExpressionStatement
        ), f"Unexpected statement of type `{type(stmt)}`."
        expr = stmt.expression
        assert isinstance(
            expr, ast_nodes.Identifier
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."

        assert expr.value == expected, "Invalid identifier value."
        assert expr.token_literal == expected, "Invalid identifier token_literal."

        assert (
            expr.token_literal == stmt.token_literal
        ), "Invalid ExpressionStatement token_literal."

    @pytest.mark.parametrize(
        ("lexer", "expected"),
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
        indirect=["lexer"],
    )
    def test_single_integer_literal_expression(self, parser: Parser, expected: int) -> None:
        """Tests parser parsing single integer literal correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ExpressionStatement.
        Assert: Underlying expression is IntegerLiteral.
        Assert: IntegerLiteral value is equal to the expected value.
        Assert: IntegerLiteral token_literal is equal to str(expected value).
        Assert: Statement token_literal is equal to IntegerLiteral token_literal.
        """
        program = parse_ok_program_and_assert(parser, 1)
        stmt = program.statements[0]
        assert isinstance(
            stmt, ast_nodes.ExpressionStatement
        ), f"Unexpected statement of type `{type(stmt)}`."
        expr = stmt.expression
        assert isinstance(
            expr, ast_nodes.IntegerLiteral
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."

        assert expr.value == expected, "Invalid integer literal value."
        assert expr.token_literal == str(expected), "Invalid integer literal token_literal."

        assert (
            expr.token_literal == stmt.token_literal
        ), "Invalid ExpressionStatement token_literal."

    @pytest.mark.parametrize(
        ("lexer", "expected"),
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
        indirect=["lexer"],
    )
    def test_single_float_literal_expression(self, parser: Parser, expected: float) -> None:
        """Tests parser parsing single float literal correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ExpressionStatement.
        Assert: Underlying expression is FloatLiteral.
        Assert: FloatLiteral value is equal to the expected value.
        Assert: FloatLiteral token_literal is equal to str(expected value).
        Assert: Statement token_literal is equal to FloatLiteral token_literal.
        """
        program = parse_ok_program_and_assert(parser, 1)
        stmt = program.statements[0]
        assert isinstance(
            stmt, ast_nodes.ExpressionStatement
        ), f"Unexpected statement of type `{type(stmt)}`."
        expr = stmt.expression
        assert isinstance(
            expr, ast_nodes.FloatLiteral
        ), f"Unexpected expression in ExpressionStatement of type `{type(expr)}`."

        assert expr.value == expected, "Invalid float literal value."
        assert expr.token_literal == str(expected), "Invalid float literal token_literal."

        assert (
            expr.token_literal == stmt.token_literal
        ), "Invalid ExpressionStatement token_literal."

    @pytest.mark.parametrize(
        ("lexer", "expected"),
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
        indirect=["lexer"],
    )
    def test_single_valid_prefix_operation(
        self, parser: Parser, expected: ExpectedPrefixOperation
    ) -> None:
        """Tests parser parsing single prefix operation correctly.

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
        program = parse_ok_program_and_assert(parser, 1)
        stmt = program.statements[0]
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
        ("lexer", "expected"),
        VALID_SINGLE_INFIX_OPERATIONS_AND_EXPECTED,
        indirect=["lexer"],
    )
    def test_single_valid_infix_operation(
        self, parser: Parser, expected: ExpectedInfixOperation
    ) -> None:
        """Tests parser parsing single prefix operation correctly.

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
        program = parse_ok_program_and_assert(parser, 1)
        stmt = program.statements[0]
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

    @pytest.mark.parametrize(
        ("lexer", "expected"), INFIX_OPERATIONS_PRECEDENCE_AND_EXPECTED, indirect=["lexer"]
    )
    def test_infix_operations_precedence(self, parser: Parser, expected: str) -> None:
        """Tests parser parsing single prefix operation correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: str(program) equals expected.
        """
        program_result = parser.parse_program()
        assert isinstance(
            program_result, Ok
        ), f"Unexpected error returned: `{repr(program_result.err())}`."

        program = program_result.ok_value

        assert str(program) == expected
