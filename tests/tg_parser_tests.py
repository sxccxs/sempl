import pytest
from result import Ok

from src.ast import ast_nodes
from src.lexer.interfaces import ILexer
from src.lexer.tokens import Keyword, Token, TokenType
from src.parser.parser import Parser
from tests.mock.lexer_mock import LexerMock
from tests.utils.payloads import ExpectedLetStatement
from tests.utils.types import YieldFixture

VALID_LET_STATEMENT_TOKENS_AND_EXPECTED: list[
    tuple[list[Token], ExpectedLetStatement]
] = [
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.IDENT, "x"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.INT, "10"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.EOF, "\0"),
        ],
        ExpectedLetStatement(False, "int", "x"),
    ),
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "str"),
            Token(TokenType.IDENT, "_abcdef11_"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.ILLEGAL, '"'),
            Token(TokenType.ILLEGAL, '"'),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.EOF, "\0"),
        ],
        ExpectedLetStatement(False, "str", "_abcdef11_"),
    ),
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.MUT, "mut"),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.IDENT, "y"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.FLOAT, "20."),
            Token(TokenType.EOF, "\0"),
        ],
        ExpectedLetStatement(True, "int", "y"),
    ),
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.MUT, "mut"),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.IDENT, "word"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.INT, "25"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.FLOAT, "34."),
            Token(TokenType.ILLEGAL, "^"),
            Token(TokenType.INT, "2"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.EOF, "\0"),
        ],
        ExpectedLetStatement(True, "int", "word"),
    ),
]

VALID_RETURN_STATEMENT_TOKENS_AND_EXPECTED: list[list[Token]] = [
    [
        Token(TokenType.RETURN, literal="return"),
        Token(TokenType.EOF, "\0"),
    ],
    [
        Token(TokenType.RETURN, literal="return"),
        Token(TokenType.INT, literal="10"),
        Token(TokenType.ENDL, literal="\n"),
        Token(TokenType.EOF, "\0"),
    ],
    [
        Token(TokenType.RETURN, literal="return"),
        Token(TokenType.IDENT, literal="a"),
        Token(TokenType.PLUS, literal="+"),
        Token(TokenType.IDENT, literal="b"),
        Token(TokenType.ENDL, literal="\n"),
        Token(TokenType.EOF, "\0"),
    ],
]


class TestParser:
    @pytest.fixture
    def lexer(self, request: pytest.FixtureRequest) -> YieldFixture[ILexer]:
        """Creates lexer mock object and provides data from request to it."""
        lexer = LexerMock(strict=True)
        lexer.set_data(request.param)
        lexer.add_data(
            [Token(TokenType.EOF, "\0")]
        )  # Add extra token as parser checks current token, not peek
        yield lexer

    @pytest.fixture
    def parser(self, lexer: ILexer) -> YieldFixture[Parser]:
        """Creates parser from lexer fixture."""
        yield Parser(lexer)

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
        assert (
            stmt.token_literal == Keyword.LET.value
        ), "Invalid let statement token literal."
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
        VALID_RETURN_STATEMENT_TOKENS_AND_EXPECTED,
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

        assert (
            stmt.token_literal == Keyword.RETURN.value
        ), "Invalid return statement token literal."
        assert isinstance(
            stmt, ast_nodes.ReturnStatement
        ), f"Unexpected statement of type `{type(stmt)}`."

    @pytest.mark.parametrize(
        ("lexer", "expected"),
        [
            ([Token(TokenType.IDENT, "abc_"), Token(TokenType.EOF, "\0")], "abc_"),
            ([Token(TokenType.IDENT, "x_1a2"), Token(TokenType.EOF, "\0")], "x_1a2"),
            ([Token(TokenType.IDENT, "q"), Token(TokenType.EOF, "\0")], "q"),
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
            ([Token(TokenType.INT, "5"), Token(TokenType.EOF, "\0")], 5),
            ([Token(TokenType.INT, "10123"), Token(TokenType.EOF, "\0")], 10123),
        ],
        indirect=["lexer"],
    )
    def test_single_integer_literal_expression(
        self, parser: Parser, expected: int
    ) -> None:
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
        assert expr.token_literal == str(
            expected
        ), "Invalid integer literal token_literal."

        assert (
            expr.token_literal == stmt.token_literal
        ), "Invalid ExpressionStatement token_literal."


def parse_ok_program_and_assert(
    parser: Parser, expected_stmts_len: int
) -> ast_nodes.Program:
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
