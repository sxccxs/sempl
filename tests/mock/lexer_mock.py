"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Lexer mock.
"""
import logging
from itertools import chain
from typing import Iterable, Iterator

from src.lexer.interfaces import ILexer
from src.lexer.tokens import Token, TokenType
from tests.mock.exceptions import UnexpectedMockCallError


class LexerMock(ILexer):
    """Lexer mock."""

    def __init__(self, *, strict: bool = False) -> None:
        """
        Args:
            strict (bool, optional): Strict mode flag. Defaults to False.
        """
        self.strict = strict
        self.token_iter: Iterator[Token] = iter([])  # set initially to empty iterator

    def next_token(self) -> Token:
        """
        Returns the next token from provided sequence of tokens.
        If the sequence is exhausted:
            - raises UnexpectedMockCallException if strict mode is on.
            - returns EOF token otherwise.
        """
        try:
            return next(self.token_iter)
        except StopIteration:
            pass

        if not self.strict:
            logging.info(
                "`LexerMock.next_token()` is called after the provided sequence is exhausted. "
                'Returning `Token(TokenType.EOF, "\0")`.'
            )
            return Token(TokenType.EOF, "\0")

        raise UnexpectedMockCallError(
            "Unexpected call of `LexerMock.next_token()`: "
            "the provided sequence of tokens is already exhausted."
        )

    def set_data(self, expected_tokens: Iterable[Token]) -> None:
        """
        Sets mock token data to provided tokens.
        All previously provided data is erased.
        """
        self.token_iter = iter(expected_tokens)

    def add_data(self, extra_tokens: Iterable[Token]) -> None:
        """Adds provided tokens after previously provided data."""
        self.token_iter = chain(self.token_iter, extra_tokens)
