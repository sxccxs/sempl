import pytest

from src.ast import ast_nodes
from src.ast.abstract import Expression, Statement
from src.lexer.tokens import Keyword, Token, TokenType
from tests.static.parser_stmt_tests_data import (
    VALID_BLOCK_STATEMENT_AND_EXPECTED,
    VALID_IF_STATEMENT_AND_EXPECTED,
    VALID_LET_STATEMENT_TOKENS_AND_EXPECTED,
)
from tests.utils.decorators import n_len_program
from tests.utils.payloads import ExpectedIfStatement, ExpectedLetStatement


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
        Assert: Let statement var_value is correct.
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

        assert stmt.var_value == expected_result.value, "Invalid variable value."

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        [
            (
                [
                    Token(TokenType.RETURN, literal="return"),
                    Token(TokenType.INT, literal="10"),
                    Token(TokenType.ENDL, literal="\n"),
                ],
                ast_nodes.IntegerLiteral(10),
            ),
            (
                [
                    Token(TokenType.RETURN, literal="return"),
                    Token(TokenType.IDENT, literal="a"),
                    Token(TokenType.PLUS, literal="+"),
                    Token(TokenType.IDENT, literal="b"),
                    Token(TokenType.ENDL, literal="\n"),
                ],
                ast_nodes.InfixOperation(ast_nodes.Identifier("a"), "+", ast_nodes.Identifier("b")),
            ),
        ],
        indirect=["lexer_mock"],
    )
    @n_len_program(1)
    def test_single_correct_return_statement(
        self, ok_len_program: ast_nodes.Program, expected: Expression
    ) -> None:
        """
        Tests parser parsing single valid return statement correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement literal is `return`.
        Assert: Statement is ReturnStatement.
        Assert: ReturnStatement expression is correct.
        """
        stmt = ok_len_program.statements[0]

        assert stmt.token_literal == Keyword.RETURN.value, "Invalid return statement token literal."
        assert isinstance(
            stmt, ast_nodes.ReturnStatement
        ), f"Unexpected statement of type `{type(stmt)}`."

        assert stmt.return_value == expected, "Invalid return value."

    @pytest.mark.parametrize(
        ("lexer_mock", "expected_stmts"),
        VALID_BLOCK_STATEMENT_AND_EXPECTED,
        indirect=["lexer_mock"],
    )
    @n_len_program(1)
    def test_single_valid_block_statement(
        self, ok_len_program: ast_nodes.Program, expected_stmts: list[Statement]
    ) -> None:
        """
        Tests parser parsing single valid block statement correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is BlockStatement.
        Assert: BlockStatement contains correct statements.
        Assert: BlockStatement token literal is it's first statement token_literal
                or empty string if no statements present.
        """
        stmt = ok_len_program.statements[0]
        assert isinstance(
            stmt, ast_nodes.BlockStatement
        ), f"Unexpected statement of type `{type(stmt)}`."
        assert stmt.statements == expected_stmts
        if expected_stmts:
            assert (
                stmt.token_literal == expected_stmts[0].token_literal
            ), "Invalid block statement token literal."
        else:
            assert stmt.token_literal == "", "Invalid block statement token literal."

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        VALID_IF_STATEMENT_AND_EXPECTED,
        indirect=["lexer_mock"],
    )
    @n_len_program(1)
    def test_single_valid_if_statement(
        self, ok_len_program: ast_nodes.Program, expected: ExpectedIfStatement
    ) -> None:
        """
        Tests parser parsing single valid if statement correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is IfStatement.
        Assert: If statement literal is `if`.
        Assert: IfStatement contains correct condition.
        Assert: IfStatement contains correct then-clause.
        Assert: If else-clause is present, IfStatement contains correct else-clause.
        """
        stmt = ok_len_program.statements[0]
        assert isinstance(
            stmt, ast_nodes.IfStatement
        ), f"Unexpected statement of type `{type(stmt)}`."
        assert stmt.token_literal == Keyword.IF, "Invalid if statement token literal."
        assert stmt.condition == expected.condition, "Invalid condition in if statement."
        assert (
            stmt.then.statements == expected.then_statements
        ), "Invalid statements in then-clause."
        if expected.else_statements is None:
            assert stmt.else_ is None, "Unexpected else-clause."
        else:
            assert stmt.else_ is not None, "Else-clause is missing."
            assert (
                stmt.else_.statements == expected.else_statements
            ), "Invalid statements in else-clause."
