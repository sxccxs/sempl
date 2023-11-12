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
    COLON = ":"
    ARROW = "->"

    # operators
    ASSIGN = "="
    PLUS = "+"
    MINUS = "-"
    ASTERIX = "*"
    SLASH = "/"
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


class Keyword(StrEnum):
    TRUE = "True"
    FALSE = "False"
    LET = "let"
    IF = "if"
    ELSE = "else"
    MUT = "mut"
    RETURN = "return"


KEYWORDS: dict[str, TokenType] = {
    Keyword.TRUE.value: TokenType.TRUE,
    Keyword.FALSE.value: TokenType.FALSE,
    Keyword.LET.value: TokenType.LET,
    Keyword.IF.value: TokenType.IF,
    Keyword.ELSE.value: TokenType.ELSE,
    Keyword.MUT.value: TokenType.MUT,
    Keyword.RETURN.value: TokenType.RETURN,
}
