from enum import StrEnum, auto
from typing import NamedTuple


class TokenType(StrEnum):
    ILLEGAL = auto()
    EOF = auto()
    ENDL = auto()
    SPACE = auto()

    # literals
    INT = auto()
    FLOAT = auto()
    IDENT = auto()

    LPAREN = "("
    RPAREN = ")"
    LCURLY = "{"
    RCURLY = "}"
    COLON = ":"

    # operators
    ASSIGN = "="
    PLUS = "+"
    MINUS = "-"
    ASTERIX = "*"
    SLASH = "/"
    CIRCUMFLEX = "^"
    APOSTROPHE = "'"

    # comparison
    EQ = "=="
    NOT_EQ = "!="
    GT = ">"
    LT = "<"
    GTEQ = ">="
    LTEQ = "<="

    # keywords
    TRUE = auto()
    FALSE = auto()
    LET = auto()
    IF = auto()
    ELSE = auto()
    MUT = auto()
    RETURN = auto()


class Token(NamedTuple):
    type: TokenType
    literal: str
