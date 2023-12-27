# pylint: disable=W2301

from typing import Protocol

from result import Result

from src.ast import ast_nodes
from src.parser.errors import ParsingError


class IParser(Protocol):
    def parse_program(self) -> Result[ast_nodes.Program, ParsingError]:
        """Parses the whole program."""
        ...
