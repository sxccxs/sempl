from typing import Callable

from src.ast.abstract import Expression
from src.parser.interfaces import IParser

PrefixParserType = Callable[[IParser], Expression]
InfixParserType = Callable[[IParser, Expression], Expression]
