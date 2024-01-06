"""Tokens related objects."""
from enum import StrEnum, auto
from typing import NamedTuple


class Keyword(StrEnum):
    """Key words."""

    TRUE = "True"
    FALSE = "False"
    LET = "let"
    IF = "if"
    ELSE = "else"
    MUT = "mut"
    RETURN = "return"
    FN = "fn"
    WHILE = "while"
    AND = "and"
    OR = "or"
    NOT = "not"


class TokenType(StrEnum):
    """Token type."""

    ILLEGAL = auto()
    EOF = auto()
    ENDL = auto()

    # literals
    INT = auto()
    FLOAT = auto()
    IDENT = auto()

    LCURLY = "{"
    RCURLY = "}"
    LPAREN = "("
    RPAREN = ")"
    COLON = ":"
    ARROW = "->"
    COMA = ","
    APOSTROPHE = "'"
    ASSIGN = "="

    # operators
    PLUS = "+"
    MINUS = "-"
    ASTERIX = "*"
    SLASH = "/"

    # comparison
    EQ = "=="
    NOT_EQ = "!="
    GT = ">"
    LT = "<"
    GTEQ = ">="
    LTEQ = "<="

    # keywords
    FN = Keyword.FN.value
    LET = Keyword.LET.value
    IF = Keyword.IF.value
    ELSE = Keyword.ELSE.value
    MUT = Keyword.MUT.value
    RETURN = Keyword.RETURN.value
    WHILE = Keyword.WHILE.value
    AND = Keyword.AND.value
    TRUE = Keyword.TRUE.value
    FALSE = Keyword.FALSE.value
    OR = Keyword.OR.value
    NOT = Keyword.NOT.value


class Token(NamedTuple):
    """Token object."""

    type: TokenType
    literal: str
