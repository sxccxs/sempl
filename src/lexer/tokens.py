from enum import StrEnum, auto
from typing import NamedTuple


class TokenType(StrEnum):
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
    FN = auto()
    LET = auto()
    IF = auto()
    ELSE = auto()
    MUT = auto()
    RETURN = auto()


class Token(NamedTuple):
    type: TokenType
    literal: str


class Keyword(StrEnum):
    LET = "let"
    IF = "if"
    ELSE = "else"
    MUT = "mut"
    RETURN = "return"
    FN = "fn"


KEYWORDS: dict[str, TokenType] = {
    Keyword.LET.value: TokenType.LET,
    Keyword.IF.value: TokenType.IF,
    Keyword.ELSE.value: TokenType.ELSE,
    Keyword.MUT.value: TokenType.MUT,
    Keyword.RETURN.value: TokenType.RETURN,
    Keyword.FN.value: TokenType.FN,
}
