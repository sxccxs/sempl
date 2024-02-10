"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

REPL payloads.
"""
from dataclasses import dataclass
from typing import NamedTuple

from src.repl.structures.enums import BraceType


@dataclass(slots=True)
class BracesCount:
    """Brace count payload."""

    open_count: int = 0
    close_count: int = 0


class Brace(NamedTuple):
    """Brace object."""

    type: BraceType
    open: str
    close: str


DEFAULT_BRACES: list[Brace] = [
    Brace(BraceType.ROUND_BRACE, "(", ")"),
    Brace(BraceType.CURLY_BRACE, "{", "}"),
]


class BracesStorage:
    """Brace storage counting open and close braces."""

    __slots__ = ("storage",)

    # pylint: disable=dangerous-default-value
    def __init__(self, braces: list[Brace] = DEFAULT_BRACES) -> None:
        """
        Args:
            braces (list[Brace], optional): List of used braces. Will not be modified.
            Defaults to DEFAULT_BRACES.
        """
        self.storage = {brace: BracesCount() for brace in braces}

    def all_closed(self) -> bool:
        """Returns True if all stored brace were closed, else False."""
        return all(
            brace_count.open_count == brace_count.close_count
            for brace_count in self.storage.values()
        )

    def add_line(self, line: str) -> None:
        """Finds all braces in given line and updates counts."""
        for char in line:
            for brace, count in self.storage.items():
                if char == brace.open:
                    count.open_count += 1
                elif char == brace.close:
                    count.close_count += 1
