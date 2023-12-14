import pytest

from src.ast import ast_nodes
from src.lexer.tokens import Keyword, Token, TokenType
from tests.static.parser_stmt_tests_data import VALID_LET_STATEMENT_TOKENS_AND_EXPECTED
from tests.utils.payloads import ExpectedLetStatement
from tests.utils.decorators import n_len_program



class TestParserStatementsTg:
    @pytest.mark.parametrize(
        ("lexer_mock", "expected_result"),
        VALID_LET_STATEMENT_TOKENS_AND_EXPECTED,
        indirect=["lexer_mock"],
    )
    @n_len_program(1)
    def test_single_correct_let_statement(
        self, ok_len_program: ast_nodes.Program, expected_result: ExpectedLetStatement
    ) -> None:
        """
        Tests parser parsing single valid let statement correctly.

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
        stmt = ok_len_program.statements[0]
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
        "lexer_mock",
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
    @n_len_program(1)
    def test_single_correct_return_statement(self, ok_len_program: ast_nodes.Program) -> None:
        """
        Tests parser parsing single valid return statement correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement literal is `return`.
        Assert: Statement is ReturnStatement.
        """
        stmt = ok_len_program.statements[0]

        assert stmt.token_literal == Keyword.RETURN.value, "Invalid return statement token literal."
        assert isinstance(
            stmt, ast_nodes.ReturnStatement
        ), f"Unexpected statement of type `{type(stmt)}`."

        # TODO: check for expression
