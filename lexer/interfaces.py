# pylint: disable=C0116,R0903
from typing import Protocol, TextIO

from lexer.tokens import Token


class ILexer(Protocol):
    def __init__(self, input_io: TextIO) -> None:
        ...

    def next_token(self) -> Token:
        ...
