# pylint: disable=W2301
from typing import Protocol

from src.lexer.tokens import Token


class ILexer(Protocol):
    def next_token(self) -> Token:
        """Reads a token."""
        ...
