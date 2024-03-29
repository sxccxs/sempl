"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Parser interface.
"""
from abc import abstractmethod
from typing import Protocol

from result import Result

from src.ast import ast_nodes
from src.errors.parser_errors import ParsingError


# pylint: disable=too-few-public-methods
class IParser(Protocol):
    """Parser interface."""

    @abstractmethod
    def parse_program(self) -> Result[ast_nodes.Program, ParsingError]:
        """Parses the whole program."""
