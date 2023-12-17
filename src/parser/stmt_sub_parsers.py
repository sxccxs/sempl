from __future__ import annotations

from result import Err, Ok, Result

import src.parser.errors as p_errors
from src.ast import ast_nodes
from src.ast.abstract import Statement
from src.lexer.tokens import TokenType
from src.parser.errors import StatementValidationError
from src.parser.expr_sub_parsers import parse_expression
from src.parser.interfaces import IParser
from src.parser.types import Precedence


def parse_let_statement(
    parser: IParser,
) -> Result[ast_nodes.LetStatement, StatementValidationError]:
    """
    Parses Let statement from current position of provided parser.
    Expected, but not checked parser.current_token is `let`.
    After the successful read, parser.current_token is the last token of the statement.

    Args:
        parser (IParser): Provided parser.

    Returns:
        Result[ast_nodes.LetStatement, StatementValidationError]: parsing result.
    """
    parser.next_token()
    is_mut = False
    if parser.cur_token_is(TokenType.MUT):
        is_mut = True
        parser.next_token()

    if (res := _validate_parser_cur_and_peek(parser, TokenType.IDENT, TokenType.IDENT)).is_err():
        return res  # type: ignore

    var_type = ast_nodes.Identifier(parser.current_token.literal)
    var_name = ast_nodes.Identifier(parser.peek_token.literal)

    parser.next_token()

    if not parser.peek_token_is(TokenType.ASSIGN):
        return Err(p_errors.InvalidTokenTypeInStatement(TokenType.ASSIGN, parser.peek_token.type))
    parser.next_token()

    stmt = ast_nodes.LetStatement(
        is_mut=is_mut, var_type=var_type, var_name=var_name, var_value=None  # type: ignore
    )  # TODO: fix

    while not parser.cur_token_is(TokenType.ENDL) and not parser.cur_token_is(TokenType.EOF):
        parser.next_token()

    return Ok(stmt)


def parse_return_statement(
    parser: IParser,
) -> Result[ast_nodes.ReturnStatement, StatementValidationError]:
    """
    Parses Return statement from current position of provided parser.
    Expected, but not checked parser.current_token is `return`.
    After the successful read, parser.current_token is the last token of the statement.

    Args:
        parser (IParser): Provided parser.

    Returns:
        Result[ast_nodes.ReturnStatement, StatementValidationError]: parsing result.
    """
    stmt = ast_nodes.ReturnStatement(return_value=None)  # type: ignore  # TODO: fix

    while not parser.cur_token_is(TokenType.ENDL) and not parser.cur_token_is(TokenType.EOF):
        parser.next_token()

    return Ok(stmt)


def parse_block_statement(
    parser: IParser,
) -> Result[ast_nodes.BlockStatement, StatementValidationError]:
    """
    Parses Block statement from current position of provided parser.
    Expected, but not checked parser.current_token is `{`.
    After the successful read, parser.current_token is the `}`.

    Args:
        parser (IParser): Provided parser.

    Returns:
        Result[ast_nodes.BlockStatement, StatementValidationError]: parsing result.
    """
    statements: list[Statement] = []
    parser.next_token()

    while not parser.cur_token_is(TokenType.RCURLY):
        if parser.cur_token_is(TokenType.EOF):
            return Err(StatementValidationError("Unexpected EOF. Brace was not closed."))
        match parser.parse_statement():
            case Err(err):
                return Err(StatementValidationError(err))
            case Ok(stmt):
                if stmt is not None:
                    statements.append(stmt)
        parser.next_token()

    return Ok(ast_nodes.BlockStatement(statements))


def parse_expression_statement(
    parser: IParser,
) -> Result[ast_nodes.ExpressionStatement, StatementValidationError]:
    """
    Parses expression statement from current position of provided parser.
    After the successful read, parser.current_token is the last token of the statement.

    Args:
        parser (IParser): Provided parser.

    Returns:
        Result[ast_nodes.ExpressionStatement, StatementValidationError]: Parsing result.
    """
    match parse_expression(parser, Precedence.LOWEST):
        case Err(err):
            return Err(StatementValidationError(err))
        case Ok(expr):
            return Ok(ast_nodes.ExpressionStatement(expression=expr))


def _validate_parser_cur_and_peek(
    parser: IParser, cur_tt: TokenType, peek_tt: TokenType
) -> Result[None, p_errors.InvalidTokenTypeInStatement]:
    """
    Checks if current token of parser is `cur_tt`.
    If not, returns corresponding error.
    If yes, makes the same check for peek token and `peek_tt`.

    Args:
        parser (IParser): Provided parser
        cur_tt (TokenType): Expected current token type.
        peek_tt (TokenType): Expected peek token type.

    Returns:
        Result[None, errors.InvalidTokenTypeInStatement]: Validation result.
    """
    if not parser.cur_token_is(cur_tt):
        return Err(
            p_errors.InvalidTokenTypeInStatement(
                cur_tt,
                parser.current_token.type,
            )
        )
    if not parser.peek_token_is(peek_tt):
        return Err(
            p_errors.InvalidTokenTypeInStatement(
                peek_tt,
                parser.peek_token.type,
            )
        )

    return Ok(None)
