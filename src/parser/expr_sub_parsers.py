from result import Err, Ok, Result

from src.ast import ast_nodes
from src.ast.abstract import Expression
from src.lexer.tokens import TokenType
from src.parser.errors import ExpressionValidationError, UnsupportedExpressionError
from src.parser.interfaces import IParser
from src.parser.types import Precedence


def parse_expression(
    parser: IParser, precedence: Precedence
) -> Result[Expression, ExpressionValidationError]:
    """
    Parses expression with given parser and its
    registered subparsers and precedences.

    Args:
        parser (IParser): Parser
        precedence (Precedence): Current expression precedence.

    Returns:
        Result[Expression, ExpressionValidationError]: Parsing result.
    """
    prefix_parser = parser.prefix_parsers.get(parser.current_token.type)
    if prefix_parser is None:
        return Err(UnsupportedExpressionError(parser.current_token.type))

    match prefix_parser(parser):
        case Err() as err:
            return err
        case Ok(exp):
            expr = exp

    while not parser.peek_token_is(TokenType.ENDL) and precedence < parser.peek_token_precedence:
        infix_parser = parser.infix_parsers.get(parser.peek_token.type)
        if infix_parser is None:
            return Ok(expr)

        parser.next_token()
        match infix_parser(parser, expr):  # type: ignore
            case Err() as err:
                return err
            case Ok(exp):
                expr = exp

    return Ok(expr)


def parse_identifier(parser: IParser) -> Ok[ast_nodes.Identifier]:
    """Creates an Identifier expression from current token of provided parser."""
    return Ok(ast_nodes.Identifier(value=parser.current_token.literal))


def parse_boolean_literal(parser: IParser) -> Ok[ast_nodes.BooleanLiteral]:
    """Creates an BooleanLiteral expression from current token of provided parser."""
    return Ok(ast_nodes.BooleanLiteral(value=parser.cur_token_is(TokenType.TRUE)))


def parse_integer_literal(
    parser: IParser,
) -> Result[ast_nodes.IntegerLiteral, ExpressionValidationError]:
    """
    Create an IntegerLiteral expression from current token
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
    """
    Create an FloatLiteral expression from current token
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
    """
    Create a PrefixOperation with operator from current token of provided parser
    and operand from next token of provided parser, if possible.
    """
    operator = parser.current_token.literal
    parser.next_token()
    match parse_expression(parser, Precedence.PREFIX):
        case Err() as err:
            return err
        case Ok(operand):
            return Ok(ast_nodes.PrefixOperation(operator, operand))


def parse_inifix_operation(
    parser: IParser, left: ast_nodes.Expression
) -> Result[ast_nodes.InfixOperation, ExpressionValidationError]:
    """
    Create a InfixOperation with provided left operand,
    operator from current token of provided parser
    and right operand from next token of provided parser, if possible.
    """
    operator = parser.current_token.literal
    precedence = parser.cur_token_precedence
    parser.next_token()
    match parse_expression(parser, precedence):
        case Err() as err:
            return err
        case Ok(right):
            return Ok(ast_nodes.InfixOperation(left, operator, right))
