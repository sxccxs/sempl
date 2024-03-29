"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Concrete expressions parsers.
"""
from result import Err, Ok, Result, is_err

from src.ast import ast_nodes
from src.ast.abstract import Expression
from src.errors.parser_errors import (
    ExpressionValidationError,
    InvalidTokenTypeInExpressionError,
    UnsupportedExpressionError,
)
from src.helpers.enum_helpers import enum_contains
from src.helpers.result_helpers import results_gather
from src.lexer.tokens import TokenType
from src.parser.parser_base import BaseParser
from src.parser.types import Operator, Precedence


def parse_expression(
    parser: BaseParser, precedence: Precedence
) -> Result[Expression, ExpressionValidationError]:
    """
    Parses expression with given parser and its
    registered subparsers and precedences.

    Args:
        parser (BaseParser): Parser
        precedence (Precedence): Current expression precedence.

    Returns:
        Result[Expression, ExpressionValidationError]: Parsing result.
    """
    prefix_parser = parser.prefix_parsers.get(parser.current_token.type)
    if prefix_parser is None:
        return Err(UnsupportedExpressionError(parser.current_token.type))

    expr: Expression
    match prefix_parser(parser):
        case Err() as err:
            return err
        case Ok(value):
            expr = value

    while not parser.peek_token_is(TokenType.ENDL) and precedence < parser.peek_token_precedence:
        infix_parser = parser.infix_parsers.get(parser.peek_token.type)
        if infix_parser is None:
            return Ok(expr)

        parser.next_token()
        match infix_parser(parser, expr):  # type: ignore
            case Err() as err:
                return err
            case Ok(value):
                expr = value

    return Ok(expr)


def parse_identifier(parser: BaseParser) -> Result[ast_nodes.Identifier, ExpressionValidationError]:
    """
    Parses an Identifier expression from current token of provided parser.
    Expected and checked parser.current_token is an identifier.
    After the successful read, parser.current_token does not change.
    """
    if is_err(res := _check_cur_token(parser, TokenType.IDENT)):
        return res
    return Ok(ast_nodes.Identifier(value=parser.current_token.literal))


def parse_integer_literal(
    parser: BaseParser,
) -> Result[ast_nodes.IntegerLiteral, ExpressionValidationError]:
    """
    Parses an IntegerLiteral expression from current token of provided parser if possible.
    Expected and checked parser.current_token is an INT.
    After the successful read, parser.current_token does not change.
    """
    if is_err(res := _check_cur_token(parser, TokenType.INT)):
        return res
    try:
        value = int(parser.current_token.literal)
    except ValueError as err:
        return Err(ExpressionValidationError(f"Unexpected error in integer literal: {err}"))
    return Ok(ast_nodes.IntegerLiteral(value))


def parse_float_literal(
    parser: BaseParser,
) -> Result[ast_nodes.FloatLiteral, ExpressionValidationError]:
    """
    Parses an FloatLiteral expression from current token of provided parser if possible.
    Expected and checked parser.current_token is a FLOAT.
    After the successful read, parser.current_token does not change.
    """
    if is_err(res := _check_cur_token(parser, TokenType.FLOAT)):
        return res
    try:
        value = float(parser.current_token.literal)
    except ValueError as err:
        return Err(ExpressionValidationError(f"Unexpected error in float literal: {err}"))
    return Ok(ast_nodes.FloatLiteral(value))


def parse_boolean_literal(
    parser: BaseParser,
) -> Result[ast_nodes.BooleanLiteral, ExpressionValidationError]:
    """
    Parses an BooleanLiteral expression from current token of provided parser.
    Expected and checked parser.current_token is TRUE or FALSE.
    After the successful read, parser.current_token does not change.
    """
    if not parser.cur_token_is(TokenType.FALSE) and not parser.cur_token_is(TokenType.TRUE):
        return Err(
            ExpressionValidationError(
                f"Token in expressin was expected to be {TokenType.TRUE!r} "
                f"or {TokenType.FALSE!r}, but actually was {parser.current_token.type}."
            )
        )
    return Ok(ast_nodes.BooleanLiteral(value=parser.cur_token_is(TokenType.TRUE)))


def parse_string_literal(
    parser: BaseParser,
) -> Result[ast_nodes.StringLiteral, ExpressionValidationError]:
    """
    Parses an StringLiteral expression from current token of provided parser.
    Expected and checked parser.current_token is a STRING.
    After the successful read, parser.current_token does not change.
    """
    if is_err(res := _check_cur_token(parser, TokenType.STRING)):
        return res
    return Ok(ast_nodes.StringLiteral(parser.current_token.literal))


def parse_array_literal(
    parser: BaseParser,
) -> Result[ast_nodes.ArrayLiteral, ExpressionValidationError]:
    """
    Parses an ArrayLiteral expression from current token of provided parser.
    Expected and checked parser.current_token is a `[`.
    After the successful read, parser.current_token is `]`.
    """
    match _parse_list_of_expression_from_to(parser, start=TokenType.LSQUARE, end=TokenType.RSQUARE):
        case Err() as err:
            return err
        case Ok(elements):
            return Ok(ast_nodes.ArrayLiteral(elements))


def parse_prefix_operation(
    parser: BaseParser,
) -> Result[ast_nodes.PrefixOperation, ExpressionValidationError]:
    """
    Parses a PrefixOperation with operator from current token of provided parser
    and operand from next token of provided parser, if possible.
    Expected and checked parser.current_token is an Operator.
    After the successful read, parser.current_token is the last token of the operation.
    """
    if is_err(res := _check_cur_token_is_operator(parser)):
        return res

    operator = Operator(parser.current_token.literal)
    parser.next_token()
    match parse_expression(parser, Precedence.PREFIX):
        case Err() as err:
            return err
        case Ok(operand):
            return Ok(ast_nodes.PrefixOperation(operator, operand))


def parse_inifix_operation(
    parser: BaseParser, left: ast_nodes.Expression
) -> Result[ast_nodes.InfixOperation, ExpressionValidationError]:
    """
    Parses an InfixOperation with provided left operand,
    operator from current token of provided parser
    and right operand parsed from next token of provided parser, if possible.
    Expected and checked parser.current_token is an Operator.
    After the successful read, parser.current_token is the last token of the operation.
    """
    if is_err(res := _check_cur_token_is_operator(parser)):
        return res

    operator = Operator(parser.current_token.literal)
    precedence = parser.cur_token_precedence
    parser.next_token()
    match parse_expression(parser, precedence):
        case Err() as err:
            return err
        case Ok(right):
            return Ok(ast_nodes.InfixOperation(left, operator, right))


def parse_assignment(
    parser: BaseParser, assignee: ast_nodes.Expression
) -> Result[ast_nodes.Assignment, ExpressionValidationError]:
    """
    Parses an Assignment with provided assignee,
    and value parsed from next token of provided parser, if possible.
    Expected and checked parser.current_token is an `=`.
    After the successful read, parser.current_token is the last token of the assignment value.
    """
    if is_err(res := _check_cur_token(parser, TokenType.ASSIGN)):
        return res

    parser.next_token()
    match parse_expression(parser, Precedence.LOWEST):
        case Err() as err:
            return err
        case Ok(value):
            return Ok(ast_nodes.Assignment(assignee, value))


def parse_index_operation(
    parser: BaseParser, left: ast_nodes.Expression
) -> Result[ast_nodes.IndexOperation, ExpressionValidationError]:
    """
    Parses an IndexOperation with provided left part,
    and value parsed from next token of provided parser, if possible.
    Expected and checked parser.current_token is an `[`.
    After the successful read, parser.current_token is `]`.
    """
    if is_err(res := _check_cur_token(parser, TokenType.LSQUARE)):
        return res

    parser.next_token()
    match parse_expression(parser, Precedence.LOWEST):
        case Err() as err:
            return err
        case Ok(value):
            index = value

    parser.next_token()
    if is_err(res := _check_cur_token(parser, TokenType.RSQUARE)):
        return res
    return Ok(ast_nodes.IndexOperation(left, index))


def parse_grouped_expression(parser: BaseParser) -> Result[Expression, ExpressionValidationError]:
    """
    Parses grouped expression from current token of provided parser, if possible.
    Expected and checked parser.current_token is a `(`.
    After the successful read, parser.current_token is `)`.
    """
    if is_err(res := _check_cur_token(parser, TokenType.LPAREN)):
        return res

    parser.next_token()
    match parse_expression(parser, Precedence.LOWEST):
        case Err() as err:
            return err
        case Ok(value):
            expr = value

    if not parser.peek_token_is(TokenType.RPAREN):
        return Err(ExpressionValidationError(f"Brace was not closed after expression=<{expr}>"))

    parser.next_token()
    return Ok(expr)


def parse_call_expression(
    parser: BaseParser, callable_: Expression
) -> Result[ast_nodes.CallExpression, ExpressionValidationError]:
    """
    Parses a Call Expression with provided callable
    from current token of provided parser, if possible.
    Expected and checked parser.current_token is a `(`.
    After the successful read, parser.current_token is `)`.
    """
    if is_err(res := _check_cur_token(parser, TokenType.LPAREN)):
        return res

    match parse_call_argument(parser):
        case Err() as err:
            return err
        case Ok(args):
            call_args = args

    if is_err(res := _check_cur_token(parser, TokenType.RPAREN)):
        return res

    return Ok(ast_nodes.CallExpression(callable_, call_args))


def parse_call_argument(parser: BaseParser) -> Result[list[Expression], ExpressionValidationError]:
    """
    Parses all call arguments from current token of provided parser, if possible.
    Expected and checked parser.current_token is a `(`.
    After the successful read, parser.current_token is `)`.
    """
    return _parse_list_of_expression_from_to(parser, start=TokenType.LPAREN, end=TokenType.RPAREN)


def _parse_list_of_expression_from_to(
    parser: BaseParser, *, start: TokenType, end: TokenType
) -> Result[list[Expression], ExpressionValidationError]:
    """
    Parses comma separated expression between given token types
    from current token of provided parser, if possible.
    Expected and checked parser.current_token is `start` parameter.
    After the successful read, parser.current_token is `end` parameter.
    """
    if is_err(res := _check_cur_token(parser, start)):
        return res

    if parser.peek_token_is(end):
        parser.next_token()
        return Ok([])

    results: list[Result[Expression, ExpressionValidationError]] = []
    parser.next_token()
    results.append(parse_expression(parser, Precedence.LOWEST))

    while parser.peek_token_is(TokenType.COMA):
        parser.next_token()
        parser.next_token()
        results.append(parse_expression(parser, Precedence.LOWEST))

    match results_gather(results):
        case Err() as err:
            return err
        case Ok(value):
            expr_list = value

    parser.next_token()
    if is_err(res := _check_cur_token(parser, end)):
        return res

    return Ok(expr_list)


def _check_cur_token(
    parser: BaseParser, expected_tt: TokenType
) -> Result[None, ExpressionValidationError]:
    """
    If parser current token is not of expected token type, constructs corresponding error,
    else returns None.
    """
    if parser.cur_token_is(expected_tt):
        return Ok(None)
    return Err(InvalidTokenTypeInExpressionError(expected_tt, parser.current_token.type))


def _check_cur_token_is_operator(parser: BaseParser) -> Result[None, ExpressionValidationError]:
    if enum_contains(Operator, parser.current_token.literal):
        return Ok(None)
    return Err(
        ExpressionValidationError(
            f"Token in expressin was expected to be a valid Operator, "
            f"but actually was `{parser.current_token.type}`."
        )
    )
