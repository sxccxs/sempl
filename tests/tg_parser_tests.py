import pytest
from result import Ok

from src.ast_ import ast_nodes
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
]


class TestParser:
    @pytest.fixture
    def lexer(self, request: pytest.FixtureRequest) -> YieldFixture[ILexer]:
        """Creates lexer mock object and provides data from request to it."""
        lexer = LexerMock(strict=True)
        lexer.set_data(request.param)
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
