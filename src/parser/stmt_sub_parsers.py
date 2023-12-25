from __future__ import annotations

from result import Err, Ok, Result, is_err

from src.ast import ast_nodes
from src.ast.abstract import Statement
from src.lexer.tokens import TokenType
from src.parser import errors
from src.parser.errors import StatementValidationError
from src.parser.expr_sub_parsers import parse_expression
from src.parser.interfaces import IParser
from src.parser.types import Precedence


def parse_let_statement(
    parser: IParser,
) -> Result[ast_nodes.LetStatement, StatementValidationError]:
    """
    Parses Let statement from current position of provided parser.
    Expected and checked parser.current_token is `let`.
    After the successful read, parser.current_token is the last token of the statement.

    Args:
        parser (IParser): Provided parser.

    Returns:
        Result[ast_nodes.LetStatement, StatementValidationError]: parsing result.
    """
    if is_err(res := _check_cur_token(parser, TokenType.LET)):
        return res
    parser.next_token()
    is_mut = False
    if parser.cur_token_is(TokenType.MUT):
        is_mut = True
        parser.next_token()

    if is_err(res := _check_cur_and_peek_tokens(parser, TokenType.IDENT, TokenType.IDENT)):
        return res

    var_type = ast_nodes.Identifier(parser.current_token.literal)
    var_name = ast_nodes.Identifier(parser.peek_token.literal)

    parser.next_token()

    if not parser.peek_token_is(TokenType.ASSIGN):
        return Err(errors.InvalidTokenTypeInStatement(TokenType.ASSIGN, parser.peek_token.type))

    parser.next_token()
    parser.next_token()

    match parse_expression(parser, Precedence.LOWEST):
        case Err(err):
            return Err(StatementValidationError(err))
        case Ok(expr):
            var_value = expr

    stmt = ast_nodes.LetStatement(
        is_mut=is_mut, var_type=var_type, var_name=var_name, var_value=var_value
    )

    if parser.peek_token_is(TokenType.ENDL):
        parser.next_token()

    return Ok(stmt)


def parse_return_statement(
    parser: IParser,
) -> Result[ast_nodes.ReturnStatement, StatementValidationError]:
    """
    Parses Return statement from current position of provided parser.
    Expected and checked parser.current_token is `return`.
    After the successful read, parser.current_token is the last token of the statement.

    Args:
        parser (IParser): Provided parser.

    Returns:
        Result[ast_nodes.ReturnStatement, StatementValidationError]: parsing result.
    """
    if is_err(res := _check_cur_token(parser, TokenType.RETURN)):
        return res
    parser.next_token()
    match parse_expression(parser, Precedence.LOWEST):
        case Err(err):
            return Err(StatementValidationError(err))
        case Ok(expr):
            return_value = expr

    stmt = ast_nodes.ReturnStatement(return_value=return_value)

    if parser.peek_token_is(TokenType.ENDL):
        parser.next_token()

    return Ok(stmt)


def parse_block_statement(
    parser: IParser,
) -> Result[ast_nodes.BlockStatement, StatementValidationError]:
    """
    Parses Block statement from current position of provided parser.
    Expected and checked checked parser.current_token is `{`.
    After the successful read, parser.current_token is the `}`.

    Args:
        parser (IParser): Provided parser.

    Returns:
        Result[ast_nodes.BlockStatement, StatementValidationError]: parsing result.
    """
    if is_err(res := _check_cur_token(parser, TokenType.LCURLY)):
        return res
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


def parse_if_statement(
    parser: IParser,
) -> Result[ast_nodes.IfStatement, StatementValidationError]:
    """
    Parses If statement from current position of provided parser.
    Expected and checked parser.current_token is `if`.
    After the successful read, parser.current_token is the `}` - end of last block of if statement.

    Args:
        parser (IParser): Provided parser.

    Returns:
        Result[ast_nodes.IfStatement, StatementValidationError]: parsing result.
    """
    if is_err(res := _check_cur_token(parser, TokenType.IF)):
        return res
    parser.next_token()
    match parse_expression(parser, Precedence.LOWEST):
        case Err(err):
            return Err(StatementValidationError(err))
        case Ok(expr):
            condition = expr

    if not parser.move_to_next_if_peek_is(TokenType.LCURLY):
        return Err(errors.InvalidTokenTypeInStatement(TokenType.LCURLY, parser.peek_token.type))

    match parse_block_statement(parser):
        case Err() as err:
            return err
        case Ok(stmt):
            then_clause = stmt

    else_clause: ast_nodes.BlockStatement | None = None
    if parser.move_to_next_if_peek_is(TokenType.ELSE):  # if there is else-clause
        if parser.move_to_next_if_peek_is(TokenType.LCURLY):  # if there is `else {`
            match parse_block_statement(parser):
                case Err() as err:
                    return err
                case Ok(stmt):
                    else_clause = stmt
        else:  # if there is "else <smth>"
            if not parser.move_to_next_if_peek_is(TokenType.IF):  # if <smth> is not "if"
                return Err(
                    errors.InvalidTokenTypeInStatement(TokenType.LCURLY, parser.peek_token.type)
                )
            match parse_if_statement(parser):  # if there is "else if"
                case Err() as err:
                    return err
                case Ok(stmt):
                    else_clause = ast_nodes.BlockStatement([stmt])

    return Ok(ast_nodes.IfStatement(condition, then_clause, else_clause))


def parse_func_statement(
    parser: IParser,
) -> Result[ast_nodes.FuncStatement, StatementValidationError]:
    """
    Parses function statement from current position of provided parser.
    Expected and checked parser.current_token is `fn`.
    After the successful read, parser.current_token is the `}` - end of function body.

    Args:
        parser (IParser): Provided parser.

    Returns:
        Result[ast_nodes.FuncStatement, StatementValidationError]: parsing result.
    """
    if is_err(res := _check_cur_token(parser, TokenType.FN)):
        return res

    parser.next_token()
    if is_err(res := _check_cur_and_peek_tokens(parser, TokenType.IDENT, TokenType.LPAREN)):
        return res

    func_name = ast_nodes.Identifier(parser.current_token.literal)
    parser.next_token()

    if is_err(res := _check_cur_token(parser, TokenType.LPAREN)):
        return res

    print(parser.current_token, parser.peek_token)
    match parse_func_parameters(parser):
        case Err() as err:
            return err
        case Ok(params):
            func_params = params

    if is_err(res := _check_cur_token(parser, TokenType.RPAREN)):
        return res

    parser.next_token()
    if is_err(res := _check_cur_and_peek_tokens(parser, TokenType.ARROW, TokenType.IDENT)):
        return res

    parser.next_token()
    func_ret_type = ast_nodes.Identifier(parser.current_token.literal)
    parser.next_token()

    if is_err(res := _check_cur_token(parser, TokenType.LCURLY)):
        return res

    match parse_block_statement(parser):
        case Err() as err:
            return err
        case Ok(block):
            func_body = block

    return Ok(ast_nodes.FuncStatement(func_name, func_params, func_ret_type, func_body))


def parse_func_parameters(
    parser: IParser,
) -> Result[list[ast_nodes.FuncParameter], StatementValidationError]:
    """
    Parses all function parameters from current position of provided parser.
    Expected and checked parser.current_token is `(`.
    After the successful read, parser.current_token is `)`.

    Args:
        parser (IParser): Provided parser.

    Returns:
        Result[list[ast_nodes.FuncParameter], StatementValidationError]: parsing result.
    """
    if is_err(res := _check_cur_token(parser, TokenType.LPAREN)):
        return res

    if parser.peek_token_is(TokenType.RPAREN):
        parser.next_token()
        return Ok([])

    params: list[ast_nodes.FuncParameter] = []
    while not parser.cur_token_is(TokenType.RPAREN):
        parser.next_token()
        match parse_func_parameter(parser):
            case Err() as err:
                return err
            case Ok(param):
                params.append(param)
        if not parser.cur_token_is(TokenType.RPAREN) and is_err(
            res := _check_cur_token(parser, TokenType.COMA)
        ):
            return res

    return Ok(params)


def parse_func_parameter(
    parser: IParser,
) -> Result[ast_nodes.FuncParameter, StatementValidationError]:
    """
    Parses one function parameter from current position of provided parser.
    Expected and checked parser.current_token is parameter name.
    After the successful read, parser.current_token is
    the token after parameter (usually `,` or `)`).

    Args:
        parser (IParser): Provided parser.

    Returns:
        Result[ast_nodes.FuncParameter, StatementValidationError]: parsing result.
    """
    if is_err(res := _check_cur_token(parser, TokenType.IDENT)):
        return res

    param_name = ast_nodes.Identifier(parser.current_token.literal)
    parser.next_token()

    if is_err(res := _check_cur_and_peek_tokens(parser, TokenType.COLON, TokenType.IDENT)):
        return res
    parser.next_token()
    param_type = ast_nodes.Identifier(parser.current_token.literal)
    parser.next_token()
    if not parser.cur_token_is(TokenType.ASSIGN):
        return Ok(ast_nodes.FuncParameter(param_name, param_type, None))

    parser.next_token()
    match parse_expression(parser, Precedence.LOWEST):
        case Err(err):
            return Err(StatementValidationError(err))
        case Ok(expr):
            default_value = expr
    parser.next_token()

    return Ok(ast_nodes.FuncParameter(param_name, param_type, default_value))


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


def _check_cur_and_peek_tokens(
    parser: IParser, cur_tt: TokenType, peek_tt: TokenType
) -> Result[None, errors.InvalidTokenTypeInStatement]:
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
            errors.InvalidTokenTypeInStatement(
                cur_tt,
                parser.current_token.type,
            )
        )
    if not parser.peek_token_is(peek_tt):
        return Err(
            errors.InvalidTokenTypeInStatement(
                peek_tt,
                parser.peek_token.type,
            )
        )

    return Ok(None)


def _check_cur_token(
    parser: IParser, expected_tt: TokenType
) -> Result[None, StatementValidationError]:
    """
    If parser current token is not of expected token type, constructs corresponding error,
    else returns None.
    """
    if not parser.cur_token_is(expected_tt):
        return Err(errors.InvalidTokenTypeInStatement(expected_tt, parser.current_token.type))
    return Ok(None)
