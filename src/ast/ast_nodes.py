from dataclasses import dataclass, field
from io import StringIO

from src.ast.abstract import ASTNode, Expression, Statement
from src.lexer.tokens import Keyword


@dataclass(slots=True)
class Program(ASTNode):
    statements: list[Statement] = field(default_factory=list)

    @property
    def token_literal(self) -> str:
        return self.statements[0].token_literal if self.statements else ""

    def __str__(self) -> str:
        ss = StringIO()
        for stmt in self.statements:
            ss.write(str(stmt))
        return ss.getvalue()


@dataclass(slots=True)
class Identifier(Expression):
    value: str

    @property
    def token_literal(self) -> str:
        return self.value

    def __str__(self) -> str:
        return self.value


@dataclass(slots=True)
class IntegerLiteral(Expression):
    value: int

    @property
    def token_literal(self) -> str:
        return str(self.value)

    def __str__(self) -> str:
        return str(self.value)


@dataclass(slots=True)
class FloatLiteral(Expression):
    value: float

    @property
    def token_literal(self) -> str:
        return str(self.value)

    def __str__(self) -> str:
        return str(self.value)


@dataclass(slots=True)
class PrefixOperation(Expression):
    operator: str
    right: Expression

    @property
    def token_literal(self) -> str:
        return self.operator

    def __str__(self) -> str:
        return f"({self.operator}{self.right})"


@dataclass(slots=True)
class InfixOperation(Expression):
    left: Expression
    operator: str
    right: Expression

    @property
    def token_literal(self) -> str:
        return self.operator

    def __str__(self) -> str:
        return f"({self.left}{self.operator}{self.right})"


@dataclass(slots=True)
class LetStatement(Statement):
    is_mut: bool
    var_type: Identifier
    var_name: Identifier
    var_value: Expression

    @property
    def token_literal(self) -> str:
        return Keyword.LET

    def __str__(self) -> str:
        ss = StringIO()
        ss.write(Keyword.LET)
        ss.write(" ")
        if self.is_mut:
            ss.write(Keyword.MUT)
            ss.write(" ")
        ss.write(str(self.var_type))
        ss.write(" ")
        ss.write(str(self.var_name))
        ss.write(" = ")
        ss.write(str(self.var_value))

        return ss.getvalue()


@dataclass(slots=True)
class ReturnStatement(Statement):
    return_value: Expression

    @property
    def token_literal(self) -> str:
        return Keyword.RETURN

    def __str__(self) -> str:
        return f"{Keyword.RETURN} {self.return_value}"


@dataclass(slots=True)
class ExpressionStatement(Statement):
    expression: Expression

    @property
    def token_literal(self) -> str:
        return self.expression.token_literal

    def __str__(self) -> str:
        return str(self.expression)
