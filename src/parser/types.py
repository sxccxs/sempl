"""Types required in parsing."""
from enum import IntEnum, StrEnum
from typing import Callable

from result import Result

from src.ast.abstract import Expression
from src.parser.errors import ExpressionValidationError
from src.parser.parser_base import BaseParser

PrefixParserType = Callable[[BaseParser], Result[Expression, ExpressionValidationError]]
InfixParserType = Callable[[BaseParser, Expression], Result[Expression, ExpressionValidationError]]


class Precedence(IntEnum):
    """Expression precedence."""

    LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, PREFIX, CALL, ASSIGN = range(8)


class Operator(StrEnum):
    """Operator."""

    PLUS = "+"
    MINUS = "-"
    MULT = "*"
    DIV = "/"
    GT = ">"
    LT = "<"
    EQ = "=="
    NOT_EQ = "!="
    GTEQ = ">="
    LTEQ = "<="
