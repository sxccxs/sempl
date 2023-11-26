from typing import Iterable

import pytest
from result import Ok

from src.ast_ import ast_nodes
from src.lexer.tokens import Keyword, Token, TokenType
from src.parser.parser import Parser
from tests.mock.lexer_mock import LexerMock
from tests.utils.payloads import ExpectedLetStatement

VALID_LET_STATEMENT_TOKENS_AND_EXPECTED: list[tuple[list[Token], ExpectedLetStatement]] = [
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.IDENT, "x"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.SPACE, " "),
            Token(TokenType.INT, "10"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.EOF, "\0"),
        ],
        ExpectedLetStatement(False, "int", "x"),
    ),
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.IDENT, "str"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.IDENT, "_abcdef11_"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.SPACE, " "),
            Token(TokenType.ILLEGAL, '"'),
            Token(TokenType.ILLEGAL, '"'),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.EOF, "\x00"),
        ],
        ExpectedLetStatement(False, "str", "_abcdef11_"),
    ),
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.MUT, "mut"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.IDENT, "y"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.SPACE, " "),
            Token(TokenType.FLOAT, "20."),
            Token(TokenType.SPACE, " "),
            Token(TokenType.SPACE, " "),
            Token(TokenType.SPACE, " "),
            Token(TokenType.SPACE, " "),
            Token(TokenType.SPACE, " "),
            Token(TokenType.SPACE, " "),
            Token(TokenType.EOF, "\0"),
        ],
        ExpectedLetStatement(True, "int", "y"),
    ),
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.MUT, "mut"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.IDENT, "word"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.SPACE, " "),
            Token(TokenType.INT, "25"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.FLOAT, "34."),
            Token(TokenType.SPACE, " "),
            Token(TokenType.ILLEGAL, "^"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.INT, "2"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.SPACE, " "),
            Token(TokenType.SPACE, " "),
            Token(TokenType.EOF, "\x00"),
        ],
        ExpectedLetStatement(True, "int", "word"),
    ),
]


class TestParser:
    @pytest.mark.parametrize(
        ("lexer_tokens", "expected_result"),
        VALID_LET_STATEMENT_TOKENS_AND_EXPECTED,
    )
    def test_single_correct_let_statement(
        self, lexer_tokens: Iterable[Token], expected_result: ExpectedLetStatement
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
        lexer = LexerMock(lexer_tokens, strict=True)
        parser = Parser(lexer)
        program_result = parser.parse_program()
        assert isinstance(
            program_result, Ok
        ), f"Unexpected error returned: `{repr(program_result.err())}`."

        program = program_result.value
        assert len(program.statements) == 1, "Invalid program statement length."

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
