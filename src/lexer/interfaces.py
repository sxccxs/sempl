from typing import Protocol

from src.lexer.tokens import Token


class ILexer(Protocol):
    def next_token(self) -> Token:  # pylint: disable=C0116
        ...
