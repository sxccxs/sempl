from dataclasses import dataclass, field

from src.ast_.abstract import ASTNode, Expression, Statement
from src.lexer.tokens import Token


@dataclass(slots=True)
class Program(ASTNode):
    statements: list[Statement] = field(default_factory=list)

    @property
    def token_literal(self) -> str:
        return self.statements[0].token_literal if self.statements else ""


class Identifier(Statement):
    token: Token
    value: str

    @property
    def token_literal(self) -> str:
        return self.token.literal


class LetStatement(Statement):
    token: Token
    mut_token: Token | None
    type: Identifier
    name: Identifier
    value: Expression

    @property
    def token_literal(self) -> str:
        return self.token.literal
