"""Concrete nodes of AST."""
from dataclasses import dataclass, field
from io import StringIO

from src.ast.abstract import ASTNode, Expression, Statement
from src.ast.to_str_helpers import sio_write_with_after_value, sio_write_with_sep
from src.lexer.tokens import Keyword
from src.parser.types import Operator


@dataclass(slots=True)
class Program(ASTNode):
    """Program AST node."""

    statements: list[Statement] = field(default_factory=list)

    def __str__(self) -> str:
        ss = StringIO()
        ss.writelines(str(stmt) for stmt in self.statements)
        return ss.getvalue()


@dataclass(slots=True)
class Identifier(Expression):
    """Identifier AST node."""

    value: str

    def __str__(self) -> str:
        return self.value


@dataclass(slots=True)
class IntegerLiteral(Expression):
    """Integer literal AST node."""

    value: int

    def __str__(self) -> str:
        return str(self.value)


@dataclass(slots=True)
class FloatLiteral(Expression):
    """Float literal AST node."""

    value: float

    def __str__(self) -> str:
        return str(self.value)


@dataclass(slots=True)
class BooleanLiteral(Expression):
    """Bool literal AST node."""

    value: bool

    def __str__(self) -> str:
        return Keyword.TRUE.value if self.value else Keyword.FALSE.value


@dataclass(slots=True)
class StringLiteral(Expression):
    """String literal AST node."""

    value: str

    def __str__(self) -> str:
        return f'"{self.value}"'


@dataclass(slots=True)
class Assignment(Expression):
    """Assignment literal AST node."""

    assignee: Expression
    value: Expression

    def __str__(self) -> str:
        return f"({self.assignee} = {self.value})"


@dataclass(slots=True)
class PrefixOperation(Expression):
    """Prefix operation AST node."""

    operator: Operator
    right: Expression

    def __str__(self) -> str:
        return f"({self.operator} {self.right})"


@dataclass(slots=True)
class InfixOperation(Expression):
    """Infix (Binary) operation AST node."""

    left: Expression
    operator: Operator
    right: Expression

    def __str__(self) -> str:
        return f"({self.left} {self.operator} {self.right})"


@dataclass(slots=True)
class CallExpression(Expression):
    """Call AST node."""

    callable: Expression
    arguments: list[Expression]

    def __str__(self) -> str:
        ss = StringIO()
        ss.write(f"{self.callable}(")
        sio_write_with_sep(ss, sep=", ", values=(str(arg) for arg in self.arguments))
        ss.write(")")
        return ss.getvalue()


@dataclass(slots=True)
class LetStatement(Statement):
    """Let statement AST node."""

    is_mut: bool
    var_type: Identifier
    var_name: Identifier
    var_value: Expression

    def __str__(self) -> str:
        ss = StringIO()
        values = [Keyword.LET, Keyword.MUT, str(self.var_type), str(self.var_name)]
        if not self.is_mut:
            values.remove(Keyword.MUT)
        sio_write_with_after_value(ss, after_value=" ", values=values)
        sio_write_with_sep(ss, sep=" ", values=("=", str(self.var_value)))

        return ss.getvalue()


@dataclass(slots=True)
class ReturnStatement(Statement):
    """Return statement AST node."""

    return_value: Expression | None

    def __str__(self) -> str:
        if self.return_value is None:
            return f"{Keyword.RETURN}"
        return f"{Keyword.RETURN} {self.return_value}"


@dataclass(slots=True)
class ExpressionStatement(Statement):
    """Expression statement AST node."""

    expression: Expression

    def __str__(self) -> str:
        return str(self.expression)


@dataclass(slots=True)
class BlockStatement(Statement):
    """Block statement AST node."""

    statements: list[Statement]

    def __str__(self) -> str:
        ss = StringIO()
        ss.write("{\n")
        ss.writelines(f"{stmt}" for stmt in self.statements)
        ss.write("\n}")
        return ss.getvalue()


@dataclass(slots=True)
class IfStatement(Statement):
    """If statement AST node."""

    condition: Expression
    then: BlockStatement
    else_: BlockStatement | None = None

    def __str__(self) -> str:
        str_ = f"{Keyword.IF} {self.condition} {self.then}"
        if self.else_ is None:
            return str_
        return f"{str_} {Keyword.ELSE} {self.else_}"


@dataclass(slots=True)
class WhileStatement(Statement):
    """While statement AST node."""

    condition: Expression
    actions: BlockStatement

    def __str__(self) -> str:
        return f"{Keyword.WHILE} {self.condition} {self.actions}"


@dataclass(slots=True)
class FuncParameter:
    """Function parameter payload."""

    name: Identifier
    type: Identifier
    default_value: Expression | None

    def __str__(self) -> str:
        str_ = f"{self.name}: {self.type}"
        if self.default_value is None:
            return str_
        return f"{str_} = {self.default_value}"


@dataclass(slots=True)
class FuncStatement(Statement):
    """Function statement AST node."""

    name: Identifier
    parameters: list[FuncParameter]
    return_type: Identifier
    body: BlockStatement

    def __str__(self) -> str:
        ss = StringIO()
        sio_write_with_sep(ss, sep=" ", values=(Keyword.FN, str(self.name), "("))
        sio_write_with_sep(ss, sep=", ", values=(str(param) for param in self.parameters))
        sio_write_with_sep(ss, sep=" ", values=(")", "->", str(self.return_type), str(self.body)))
        return ss.getvalue()
