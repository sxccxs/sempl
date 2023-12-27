from __future__ import annotations

from abc import ABC, abstractmethod
from typing import TYPE_CHECKING

from result import Result

from src.ast import ast_nodes
from src.lexer.tokens import Token, TokenType
from src.parser.errors import ParsingError

if TYPE_CHECKING:
    from src.parser.types import InfixParserType, Precedence, PrefixParserType


class BaseParser(ABC):
    @property
    @abstractmethod
    def current_token(self) -> Token:
        """Current parser token."""

    @property
    @abstractmethod
    def peek_token(self) -> Token:
        """Next parser token."""

    @property
    @abstractmethod
    def prefix_parsers(self) -> dict[TokenType, PrefixParserType]:
        """Map of registered expression prefix parsers."""

    @property
    @abstractmethod
    def infix_parsers(self) -> dict[TokenType, InfixParserType]:
        """Map of registered expression infix parsers."""

    @property
    @abstractmethod
    def cur_token_precedence(self) -> Precedence:
        """Precedence of current token."""

    @property
    @abstractmethod
    def peek_token_precedence(self) -> Precedence:
        """Precedence of peek token."""

    @abstractmethod
    def next_token(self) -> None:
        """Moves parser to the next token."""

    @abstractmethod
    def parse_program(self) -> Result[ast_nodes.Program, ParsingError]:
        """Parses the whole program."""

    def cur_token_is(self, token_t: TokenType) -> bool:
        """True if parser's current token equals to token_t else False."""
        return self.current_token.type == token_t

    def peek_token_is(self, token_t: TokenType) -> bool:
        """True if parser's peek token equals to token_t else False."""
        return self.peek_token.type == token_t

    def move_to_next_if_peek_is(self, expected_t: TokenType) -> bool:
        """
        If parser's peek token equals to token_t, moves to next token and returns True.
        Otherwise just returns False.
        """
        if not self.peek_token_is(expected_t):
            return False
        self.next_token()
        return True
