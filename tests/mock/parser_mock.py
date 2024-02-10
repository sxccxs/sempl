"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Parser mock.
"""
from typing import Iterable

from result import Ok

from src.ast import ast_nodes
from src.ast.abstract import Statement
from src.parser.interfaces import IParser


class ParserMock(IParser):
    """Parser mock."""

    def __init__(self) -> None:
        self.statements: list[Statement] = []

    def parse_program(self) -> Ok[ast_nodes.Program]:
        """Parses the whole program."""
        return Ok(ast_nodes.Program(self.statements))

    def set_data(self, expected_stmts: Iterable[Statement]) -> None:
        """
        Sets mock token data to provided statements.
        All previously provided data is erased.
        """
        self.statements = list(expected_stmts)

    def add_data(self, expected_stmts: Iterable[Statement]) -> None:
        """Adds provided statements after previously provided data."""
        self.statements.extend(expected_stmts)
