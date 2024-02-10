"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Parsing errors.
"""
from src.errors.error import Error
from src.lexer.tokens import TokenType


class ParsingError(Error):
    """Base error type for parsing."""


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
        super().__init__(f"Expression from token of type={token_type!r} does not exist.")


class InvalidTokenTypeInStatementError(StatementValidationError):
    """Error for invalid token in statement."""

    def __init__(self, expected_tt: TokenType, received_tt: TokenType) -> None:
        """
        Args:
            expected_tt (TokenType): Expected token type.
            received_tt (TokenType): Actual token type.
        """
        super().__init__(
            f"Token in statement was expected to be `{expected_tt!r}`, "
            f"but actually was `{received_tt!r}`."
        )


class InvalidTokenTypeInExpressionError(ExpressionValidationError):
    """Error for invalid token in expression."""

    def __init__(self, expected_tt: TokenType, received_tt: TokenType) -> None:
        """
        Args:
            expected_tt (TokenType): Expected token type.
            received_tt (TokenType): Actual token type.
        """
        super().__init__(
            f"Token in expression was expected to be `{expected_tt!r}`, "
            f"but actually was `{received_tt!r}`."
        )
