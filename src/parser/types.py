from typing import Callable

from result import Result

from src.ast.abstract import Expression
from src.parser.errors import ExpressionValidationError
from src.parser.interfaces import IParser

PrefixParserType = Callable[[IParser], Result[Expression, ExpressionValidationError]]
InfixParserType = Callable[[IParser, Expression], Result[Expression, ExpressionValidationError]]
