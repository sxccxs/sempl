"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Test group parsing statements.
"""
import pytest

from src.ast import ast_nodes
from src.ast.abstract import Expression, Statement
from src.lexer.tokens import Token, TokenType
from src.parser.types import Operator
from tests.static.parser_stmt_tests_data import (
    VALID_BLOCK_STATEMENT_AND_EXPECTED,
    VALID_FUNC_AND_EXPECTED,
    VALID_IF_STATEMENT_AND_EXPECTED,
    VALID_LET_STATEMENT_TOKENS_AND_EXPECTED,
    VALID_WHILE_STATEMENT_AND_EXPECTED,
)
from tests.utils.decorators import n_len_program
from tests.utils.payloads import (
    ExpectedFunc,
    ExpectedIfStatement,
    ExpectedLetStatement,
    ExpectedWhileStatement,
)


class TestParserStatementsTg:
    """Test group for parsing of statements."""

    @pytest.mark.parametrize(
        ("lexer_mock", "expected_result"),
        VALID_LET_STATEMENT_TOKENS_AND_EXPECTED,
        indirect=["lexer_mock"],
    )
    @n_len_program(1)
    def test_single_valid_let_statement(
        self, ok_len_program: ast_nodes.Program, expected_result: ExpectedLetStatement
    ) -> None:
        """
        Tests parser parsing single valid let statement correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is LetStatement.
        Assert: Let statement mutability equals to expected.
        Assert: Let statement var_type.value equals to the expected type.
        Assert: Let statement var_name.value equals to the expected name.
        Assert: Let statement var_value is equal to the expected value.
        """
        stmt = ok_len_program.statements[0]
        assert isinstance(
            stmt, ast_nodes.LetStatement
        ), f"Unexpected statement of type `{type(stmt)}`."
        assert stmt.is_mut == expected_result.mut, "Invalid mutability."
        assert stmt.var_type.value == expected_result.type, "Invalid var_type value."
        assert stmt.var_name.value == expected_result.name, "Invalid var_name value."
        assert stmt.var_value == expected_result.value, "Invalid variable value."

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        [
            (
                [
                    Token(TokenType.RETURN, literal="return"),
                    Token(TokenType.ENDL, literal="\n"),
                ],
                None,
            ),
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
                ast_nodes.InfixOperation(
                    ast_nodes.Identifier("a"), Operator.PLUS, ast_nodes.Identifier("b")
                ),
            ),
        ],
        indirect=["lexer_mock"],
    )
    @n_len_program(1)
    def test_single_valid_return_statement(
        self, ok_len_program: ast_nodes.Program, expected: Expression | None
    ) -> None:
        """
        Tests parser parsing single valid return statement correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is ReturnStatement.
        Assert: ReturnStatement expression is equal to the expected.
        """
        stmt = ok_len_program.statements[0]
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
        Assert: BlockStatement contains the expected number of statements.
        For: each statement in BlockStatement:
            Assert: Statement is equal to the expected.
        """
        stmt = ok_len_program.statements[0]
        assert isinstance(
            stmt, ast_nodes.BlockStatement
        ), f"Unexpected statement of type `{type(stmt)}`."
        assert len(stmt.statements) == len(
            expected_stmts
        ), "Invalid number of parameters if block statement."
        for i, (st, exp) in enumerate(zip(stmt.statements, expected_stmts, strict=True)):
            assert st == exp, f"Invalid statement #{i} in a block statement."

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
        Assert: IfStatement condition is equal to the expected.
        Assert: IfStatement then-clause is equal to the expected.
        Assert: If else-clause is present, IfStatement else-clause is equal to the expected.
        """
        stmt = ok_len_program.statements[0]
        assert isinstance(
            stmt, ast_nodes.IfStatement
        ), f"Unexpected statement of type `{type(stmt)}`."
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

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        VALID_FUNC_AND_EXPECTED,
        indirect=["lexer_mock"],
    )
    @n_len_program(1)
    def test_single_valid_function(
        self, ok_len_program: ast_nodes.Program, expected: ExpectedFunc
    ) -> None:
        """
        Tests parser parsing single valid if statement correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is FuncStatement.
        Assert: FuncStatement name is is equal to the expected.
        Assert: FuncStatement contains expected number of parameters.
        For: each parameter in FuncStatement:
            Assert: Parameter name is equal to the expected.
            Assert: Parameter type is equal to the expected.
            Assert: Parameter default value is equal to the expected.
        Assert: FuncStatement body is equal to the expected.
        """
        stmt = ok_len_program.statements[0]
        assert isinstance(
            stmt, ast_nodes.FuncStatement
        ), f"Unexpected statement of type `{type(stmt)}`."
        assert stmt.name.value == expected.name, "Invalid condition in function statement."
        assert len(stmt.parameters) == len(
            expected.parameters
        ), "Invalid number of parameters if function."
        for i, (param, expected_param) in enumerate(
            zip(stmt.parameters, expected.parameters, strict=True)
        ):
            assert param.name.value == expected_param.name, f"Invalid name of parameter #{i}."
            assert param.type.value == expected_param.type, f"Invalid type of parameter #{i}."
            assert (
                param.default_value == expected_param.default_value
            ), f"Invalid default value of parameter #{i}."
        assert stmt.body.statements == expected.body, "Invalid statements in function body."

    @pytest.mark.parametrize(
        ("lexer_mock", "expected"),
        VALID_WHILE_STATEMENT_AND_EXPECTED,
        indirect=["lexer_mock"],
    )
    @n_len_program(1)
    def test_single_valid_while_statement(
        self, ok_len_program: ast_nodes.Program, expected: ExpectedWhileStatement
    ) -> None:
        """
        Tests parser parsing single valid if statement correctly.

        Arrange: Provide tokens to Lexer Mock.
        Arrange: Create Parser with Lexer Mock.

        Act: Parse program.
        Assert: No error returned.
        Assert: Program contains only one statement.
        Assert: Statement is WhileStatement.
        Assert: WhileStatement condition is equal to the expected.
        Assert: WhileStatement actions is equal to the expected.
        """
        stmt = ok_len_program.statements[0]
        assert isinstance(
            stmt, ast_nodes.WhileStatement
        ), f"Unexpected statement of type `{type(stmt)}`."
        assert stmt.condition == expected.condition, "Invalid condition in while statement."
        assert (
            stmt.actions.statements == expected.actions_statements
        ), "Invalid statements in while actions."
