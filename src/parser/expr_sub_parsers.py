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

    left_expr = prefix_parser(parser)

    return Ok(left_expr)


def parse_identifier(parser: IParser) -> ast_nodes.Identifier:
    """Creates an Identifier expression from current token of provided parser."""
    return ast_nodes.Identifier(value=parser.current_token.literal)


def parser_integer_literal(
    parser: IParser,
) -> ast_nodes.IntegerLiteral:
    """Create an IntegerLiteral expression from current token of provided parser."""
    return ast_nodes.IntegerLiteral(int(parser.current_token.literal))
