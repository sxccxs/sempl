from dataclasses import dataclass, field

from src.ast.abstract import ASTNode, Expression, Statement
from src.lexer.tokens import Keyword


@dataclass(slots=True)
class Program(ASTNode):
    statements: list[Statement] = field(default_factory=list)

    @property
    def token_literal(self) -> str:
        return self.statements[0].token_literal if self.statements else ""


@dataclass(slots=True)
class Identifier(Expression):
    value: str

    @property
    def token_literal(self) -> str:
        return self.value


@dataclass(slots=True)
class LetStatement(Statement):
    is_mut: bool
    var_type: Identifier
    var_name: Identifier
    var_value: Expression

    @property
    def token_literal(self) -> str:
        return Keyword.LET.value


@dataclass(slots=True)
class ReturnStatement(Statement):
    return_value: Expression

    @property
    def token_literal(self) -> str:
        return Keyword.RETURN.value


@dataclass(slots=True)
class ExpressionStatement(Statement):
    expression: Expression

    @property
    def token_literal(self) -> str:
        return self.expression.token_literal
