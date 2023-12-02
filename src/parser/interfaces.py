# pylint: disable=W2301
from typing import Protocol

from src.lexer.tokens import Token, TokenType


class IParser(Protocol):
    @property
    def current_token(self) -> Token:
        """Current parser token."""
        ...

    @property
    def peek_token(self) -> Token:
        """Next parser token."""
        ...

    def next_token(self) -> None:
        """Moves parser to the next token."""
        ...

    def cur_token_is(self, token_t: TokenType) -> bool:
        """True if parser's current token equals to token_t else False."""
        ...

    def peek_token_is(self, token_t: TokenType) -> bool:
        """True if parser's peek token equals to token_t else False."""
        ...

    def move_to_next_if_peek_is_expected(self, expected_t: TokenType) -> bool:
        """If parser's peek token equals to token_t, moves to next token and returns True.
        Otherwise just returns False.
        """
        ...
