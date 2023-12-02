import logging
from typing import Iterable, Iterator

from src.lexer.tokens import Token, TokenType
from tests.mock.exceptions import UnexpectedMockCallException


class LexerMock:
    def __init__(self, expected_tokens: Iterable[Token], *, strict: bool = False) -> None:
        self.token_iter = self._create_token_iter(expected_tokens)
        self.strict = strict

    def next_token(self) -> Token:
        """Returns the next token from provided sequence of tokens.
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

        raise UnexpectedMockCallException(
            "Unexpected call of `LexerMock.next_token()`: "
            "the provided sequence of tokens is already exhausted."
        )

    def _create_token_iter(self, expected_tokens: Iterable[Token]) -> Iterator[Token]:
        for token in expected_tokens:
            yield token
        yield Token(TokenType.EOF, "\0")
