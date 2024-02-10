"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Lexer interface.
"""
from abc import abstractmethod
from typing import Protocol

from src.lexer.tokens import Token


# pylint: disable=too-few-public-methods
class ILexer(Protocol):
    """Lexer interface."""

    @abstractmethod
    def next_token(self) -> Token:
        """Reads a token."""
