"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Conftest.
"""
# pylint: disable=redefined-outer-name
import pytest
from result import Ok

from src.ast import ast_nodes
from src.lexer.interfaces import ILexer
from src.lexer.tokens import Token, TokenType
from src.parser.parser import Parser
from tests.mock.lexer_mock import LexerMock
from tests.utils.decorators import n_len_program
from tests.utils.types import YieldFixture


@pytest.fixture
def lexer_mock(request: pytest.FixtureRequest) -> YieldFixture[ILexer]:
    """Creates lexer mock object and provides tokens from request to it + EOF tokens."""
    lexer = LexerMock(strict=True)
    lexer.set_data(request.param)
    lexer.add_data(
        [Token(TokenType.EOF, "\0"), Token(TokenType.EOF, "\0")]
    )  # Add EOF and extra token as parser ends on current token, not peek token.
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
    parser: Parser,
    expected_stmts_len: int | None,
) -> YieldFixture[ast_nodes.Program]:
    """
    Parses program with provided parser and checks the result.
    The program is expected to be valid.

    Act: Parse program.
    Assert: No error returned.
    If `expected_stmts_len` is provided (not None)
        Assert: Program contains only `expected_stmts_len` statement.

    Returns:
        ast_nodes.Program: Parsed Program node.
    """
    program_result = parser.parse_program()
    assert isinstance(program_result, Ok), f"Unexpected error returned: `{program_result.err()!r}`."

    program = program_result.ok_value
    if expected_stmts_len is not None:
        assert (
            len(program.statements) == expected_stmts_len
        ), "Invalid number of statementes in program."

    yield program


@pytest.fixture
@n_len_program(1)
def expression_stmt(
    ok_len_program: ast_nodes.Program,
) -> YieldFixture[ast_nodes.ExpressionStatement]:
    """
    Checks if ok_program contains only one statement which is an expression statement.

    Assert: ok_program contains 1 statement.
    Assert: That 1 statement is an ExpressionStatement.

    Returns:
        ast_nodes.ExpressionStatement: That one ExpressionStatement from parsed program.
    """
    stmt = ok_len_program.statements[0]
    assert isinstance(
        stmt, ast_nodes.ExpressionStatement
    ), f"Unexpected statement of type `{type(stmt)}`."
    yield stmt
