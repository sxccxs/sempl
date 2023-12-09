from enum import IntEnum

from result import Err, Ok, Result

from src.ast import ast_nodes
from src.ast.abstract import Expression
from src.parser.errors import ExpressionValidationError, UnsupportedExpressionError
from src.parser.interfaces import IParser


class Precedence(IntEnum):
    LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, PREFIX, CALL = range(7)


def parse_expression(
    parser: IParser, precedence: Precedence
) -> Result[Expression, ExpressionValidationError]:
    prefix_parser = parser.prefix_parsers.get(parser.current_token.type)
    if prefix_parser is None:
        return Err(UnsupportedExpressionError(parser.current_token.type))

    return prefix_parser(parser)


def parse_identifier(parser: IParser) -> Ok[ast_nodes.Identifier]:
    """Creates an Identifier expression from current token of provided parser."""
    return Ok(ast_nodes.Identifier(value=parser.current_token.literal))


def parse_integer_literal(
    parser: IParser,
) -> Result[ast_nodes.IntegerLiteral, ExpressionValidationError]:
    """Create an IntegerLiteral expression from current token
    of provided parser if possible.
    """
    try:
        value = int(parser.current_token.literal)
    except ValueError as err:
        return Err(ExpressionValidationError(f"Unexpected error in integer literal: {err}"))
    return Ok(ast_nodes.IntegerLiteral(value))


def parse_float_literal(
    parser: IParser,
) -> Result[ast_nodes.FloatLiteral, ExpressionValidationError]:
    """Create an FloatLiteral expression from current token
    of provided parser if possible.
    """
    try:
        value = float(parser.current_token.literal)
    except ValueError as err:
        return Err(ExpressionValidationError(f"Unexpected error in float literal: {err}"))
    return Ok(ast_nodes.FloatLiteral(value))


def parse_prefix_operation(
    parser: IParser,
) -> Result[ast_nodes.PrefixOperation, ExpressionValidationError]:
    """Create a PrefixOperation with operator from current token of provided parser
    and operand from next token of provided parser, if possible.
    """
    operator = parser.current_token.literal
    parser.next_token()
    match parse_expression(parser, Precedence.PREFIX):
        case Err() as err:
            return err
        case Ok(operand):
            return Ok(ast_nodes.PrefixOperation(operator, operand))
