import pytest
from result import Ok

from src.ast import ast_nodes
from src.lexer.interfaces import ILexer
from src.lexer.tokens import Token, TokenType
from src.parser.parser import Parser
from tests.mock.lexer_mock import LexerMock
from tests.utils.types import YieldFixture


@pytest.fixture
def lexer_mock(request: pytest.FixtureRequest) -> YieldFixture[ILexer]:
    """Creates lexer mock object and provides tokens from request to it + EOF tokens."""
    lexer = LexerMock(strict=True)
    lexer.set_data(request.param)
    lexer.add_data(
        [Token(TokenType.EOF, "\0"), Token(TokenType.EOF, "\0")]
    )  # Add extra token as parser ends on current token, not peek token.
    yield lexer


@pytest.fixture
def parser(lexer_mock: ILexer) -> YieldFixture[Parser]:
    """Creates parser from lexer fixture."""
    yield Parser(lexer_mock)


@pytest.fixture
def expected_stmts_len() -> YieldFixture[int | None]:
    """Fixture for providing an expected number of statements in program."""
    yield None


@pytest.fixture(params=[None])
def ok_len_program(
    request: pytest.FixtureRequest,
    parser: Parser,
) -> YieldFixture[ast_nodes.Program]:
    """
    Parses program with provided parser and checks the result.
    The program is expected to be valid.

    Act: Parse program.
    Assert: No error returned.
    If `expected_stmts_len` is provided (not None)
        Assert: Program contains only `expected_stmts_len` statement.

    Args:
        parser (Parser): Parser.
        expected_stmts_len (int): Expected number of statements in result program. Defaults to None.

    Returns:
        ast_nodes.Program: Parsed Program node.
    """
    expected_stmts_len: int | None = request.param
    program_result = parser.parse_program()
    assert isinstance(
        program_result, Ok
    ), f"Unexpected error returned: `{repr(program_result.err())}`."

    program = program_result.ok_value
    if expected_stmts_len is not None:
        assert (
            len(program.statements) == expected_stmts_len
        ), "Invalid number of statementes in program."

    yield program
