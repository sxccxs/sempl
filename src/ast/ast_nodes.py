from dataclasses import dataclass, field
from io import StringIO

from src.ast.abstract import ASTNode, Expression, Statement
from src.ast.to_str_helpers import sio_write_with_after_value, sio_write_with_sep
from src.lexer.tokens import Keyword
from src.parser.types import Operator


@dataclass(slots=True)
class Program(ASTNode):
    statements: list[Statement] = field(default_factory=list)

    def __str__(self) -> str:
        ss = StringIO()
        ss.writelines(str(stmt) for stmt in self.statements)
        return ss.getvalue()


@dataclass(slots=True)
class Identifier(Expression):
    value: str

    def __str__(self) -> str:
        return self.value


@dataclass(slots=True)
class BooleanLiteral(Expression):
    value: bool

    def __str__(self) -> str:
        return Keyword.TRUE.value if self.value else Keyword.FALSE.value


@dataclass(slots=True)
class IntegerLiteral(Expression):
    value: int

    def __str__(self) -> str:
        return str(self.value)


@dataclass(slots=True)
class FloatLiteral(Expression):
    value: float

    def __str__(self) -> str:
        return str(self.value)


@dataclass(slots=True)
class PrefixOperation(Expression):
    operator: Operator
    right: Expression

    def __str__(self) -> str:
        return f"({self.operator}{self.right})"


@dataclass(slots=True)
class InfixOperation(Expression):
    left: Expression
    operator: Operator
    right: Expression

    def __str__(self) -> str:
        return f"({self.left} {self.operator} {self.right})"


@dataclass(slots=True)
class CallExpression(Expression):
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
    return_value: Expression

    def __str__(self) -> str:
        return f"{Keyword.RETURN} {self.return_value}"


@dataclass(slots=True)
class ExpressionStatement(Statement):
    expression: Expression

    def __str__(self) -> str:
        return str(self.expression)


@dataclass(slots=True)
class BlockStatement(Statement):
    statements: list[Statement]

    def __str__(self) -> str:
        ss = StringIO()
        ss.write("{\n")
        ss.writelines(f"{4 * ' '}{stmt}" for stmt in self.statements)
        ss.write("}")
        return ss.getvalue()


@dataclass(slots=True)
class IfStatement(Statement):
    condition: Expression
    then: BlockStatement
    else_: BlockStatement | None = None

    def __str__(self) -> str:
        str_ = f"{Keyword.IF} {self.condition} {self.then}"
        if self.else_ is None:
            return str_
        return f"{str_} {Keyword.ELSE} {self.else_}"


@dataclass(slots=True)
class FuncParameter(Statement):
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
