"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Types required in parsing.
"""
from enum import IntEnum, StrEnum, auto
from typing import Callable

from result import Result

from src.ast.abstract import Expression
from src.errors.parser_errors import ExpressionValidationError
from src.lexer.tokens import Keyword
from src.parser.parser_base import BaseParser

PrefixParserType = Callable[[BaseParser], Result[Expression, ExpressionValidationError]]
InfixParserType = Callable[[BaseParser, Expression], Result[Expression, ExpressionValidationError]]


class Precedence(IntEnum):
    """Expression precedence."""

    LOWEST = 0
    ASSIGN = auto()
    OR = auto()
    AND = auto()
    NOT = auto()
    COMPARE = auto()
    SUM = auto()
    PRODUCT = auto()
    PREFIX = auto()
    CALL = auto()


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
    AND = Keyword.AND.value
    OR = Keyword.OR.value
    NOT = Keyword.NOT.value
