"""Parsing errors."""
from src.lexer.tokens import TokenType


class ParsingError(Exception):
    """Base exception type for parsing."""


class StatementValidationError(ParsingError):
    """Base statement parsing error."""


class ExpressionValidationError(ParsingError):
    """Base expression parsing error."""


class UnsupportedExpressionError(ExpressionValidationError):
    """Error for expression which is not supported."""
    def __init__(self, token_type: TokenType) -> None:
        """
        Args:
            token_type (TokenType): Token type with which no expression begins.
        """
        super().__init__(f"Expression from token of type={repr(token_type)} does not exist.")


class InvalidTokenTypeInStatement(StatementValidationError):
    """Error for invalid token in statement."""
    def __init__(self, expected_tt: TokenType, received_tt: TokenType) -> None:
        """
        Args:
            expected_tt (TokenType): Expected token type.
            received_tt (TokenType): Actual token type.
        """
        super().__init__(
            f"Token in statement was expected to be {repr(expected_tt)}, "
            f"but actually was {repr(received_tt)}."
        )


class InvalidTokenTypeInExpression(ExpressionValidationError):
    """Error for invalid token in expression."""
    def __init__(self, expected_tt: TokenType, received_tt: TokenType) -> None:
        """
        Args:
            expected_tt (TokenType): Expected token type.
            received_tt (TokenType): Actual token type.
        """
        super().__init__(
            f"Token in expression was expected to be {repr(expected_tt)}, "
            f"but actually was {repr(received_tt)}."
        )
