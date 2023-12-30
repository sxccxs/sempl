from typing import Iterable, Literal

from result import Err, Ok, Result, do

from src.ast import ast_nodes
from src.ast.abstract import ASTNode, Statement
from src.evaluation import consts, errors
from src.evaluation.errors import EvaluationError
from src.evaluation.values import value_types
from src.evaluation.values.types import Scope, VarEntry
from src.evaluation.values.value_base import NumericValue, Value, ValuedValue
from src.helpers.result_helpers import create_err, results_gather
from src.parser.types import Operator


def evaluate(node: ASTNode, scope: Scope) -> Result[Value, EvaluationError]:
    """
    Evalueates given AST node to a Value.

    Args:
        node (ast_nodes.ASTNode): AST node.
        scope (Scope): Evaluation scope.

    Returns:
        Value: Evaluated value.
    """
    match node:
        case ast_nodes.IntegerLiteral():
            return Ok(value_types.Integer(node.value))
        case ast_nodes.FloatLiteral():
            return Ok(value_types.Float(node.value))
        case ast_nodes.BooleanLiteral():
            return Ok(consts.TrueFalse.from_bool(node.value).value)
        case ast_nodes.Program():
            return evaluate_statements(node.statements, scope)
        case ast_nodes.ExpressionStatement():
            return evaluate(node.expression, scope)
        case ast_nodes.PrefixOperation():
            return do(
                Ok(val)
                for operand in evaluate(node.right, scope)
                for val in evaluate_prefix_expression(node.operator, operand)
            )
        case ast_nodes.InfixOperation():
            return do(
                Ok(val)
                for left in evaluate(node.left, scope)
                for right in evaluate(node.right, scope)
                for val in evaluate_infix_expression(left, node.operator, right)
            )
        case ast_nodes.Identifier():
            return evaluate_indetifier(node, scope)
        case ast_nodes.LetStatement():
            return evaluate_let_statement(node, scope)
        case _:
            return create_err(errors.UnsuportedNodeError(node))


def evaluate_statements(stmts: Iterable[Statement], scope: Scope) -> Result[Value, EvaluationError]:
    """
    Evaluates all provided statements.

    Args:
        stmts (Iterable[Statement]): Statements to evaluate.
        scope (Scope): Scope of evaluation.

    Returns:
        Result[Value, EvaluationError]: Value of last evaluated statement or first occured error.
    """
    results: list[Result[Value, EvaluationError]] = [evaluate(stmt, scope) for stmt in stmts]
    match results_gather(results):
        case Err() as err:
            return err
        case Ok(vals):
            return Ok(vals[-1])


def evaluate_let_statement(
    node: ast_nodes.LetStatement, scope: Scope
) -> Result[Value, EvaluationError]:
    """Evaluates given let statement in provided scope."""
    match results_gather(evaluate(node.var_value, scope), evaluate(node.var_type, scope)):
        case Err() as err:
            return err
        case Ok(vals):
            var_value, var_type = vals
    match var_type:
        case value_types.Type():
            pass
        case _:
            return create_err(
                EvaluationError(f"Expected type identifier, received {type(var_type)}")
            )
    if not isinstance(var_value, var_type.value):
        return create_err(errors.InvalidType(var_value, var_type.value))
    scope[node.var_name.value] = VarEntry(var_value, node.is_mut, var_type)
    return evaluate(node.var_name, scope)


def evaluate_indetifier(node: ast_nodes.Identifier, scope: Scope) -> Result[Value, EvaluationError]:
    """Evaluates given identifier if it is defined in provided scope."""
    if (entry := scope.get(node.value)) is not None:
        return Ok(entry.value)
    return create_err(EvaluationError(f"Identifier `{node.value}` not found."))


def evaluate_prefix_expression(
    operator: Operator, operand: Value
) -> Result[Value, EvaluationError]:
    """Evaluates given prefix operation."""
    match operator:
        case Operator.MINUS:
            return evaluate_prefix_minus_expression(operand)
        case Operator.PLUS:
            return evaluate_prefix_plus_expression(operand)
        case _:
            return create_err(errors.UnsuportedPrefixOperator(operator))


def evaluate_prefix_minus_expression(operand: Value) -> Result[Value, EvaluationError]:
    """Applies unary minus on given operand if possible."""
    match operand:
        case value_types.Integer() as i:
            return Ok(value_types.Integer(-1 * i.value))
        case value_types.Float() as f:
            return Ok(value_types.Float(-1 * f.value))
        case _:
            return create_err(errors.UnsuportedPrefixOperation(Operator.MINUS, operand))


def evaluate_prefix_plus_expression(operand: Value) -> Result[Value, EvaluationError]:
    """Applies unary plus on given operand if possible."""
    match operand:
        case value_types.Integer() as i:
            return Ok(i)
        case value_types.Float() as f:
            return Ok(f)
        case _:
            return create_err(errors.UnsuportedPrefixOperation(Operator.PLUS, operand))


def evaluate_infix_expression(
    left_operand: Value, operator: Operator, right_operand: Value
) -> Result[Value, EvaluationError]:
    """Applies given binary operator on given operands if possible."""
    match operator:
        case Operator.EQ | Operator.NOT_EQ:
            return evaluate_equality_expression(left_operand, operator, right_operand)
        case Operator.GT | Operator.GTEQ | Operator.LT | Operator.LTEQ:
            return evaluate_comparison_expression(left_operand, operator, right_operand)
        case _:
            pass
    match (left_operand, right_operand):
        case (value_types.Integer(), value_types.Integer()):
            return evaluate_integer_infix_expression(left_operand, operator, right_operand)
        case (
            value_types.Integer() | value_types.Float(),
            value_types.Integer() | value_types.Float(),
        ):
            return evaluate_float_infix_expression(left_operand, operator, right_operand)
        case _:
            return create_err(
                errors.UnsuportedInfixOperation(left_operand, operator, right_operand)
            )


def evaluate_integer_infix_expression(
    left_operand: value_types.Integer, operator: Operator, right_operand: value_types.Integer
) -> Result[Value, EvaluationError]:
    """
    Applies given binary operator on given integer operands,
    with result of an integer value if possible.
    """
    left = left_operand.value
    right = right_operand.value
    match operator:
        case Operator.PLUS:
            return Ok(value_types.Integer(left + right))
        case Operator.MINUS:
            return Ok(value_types.Integer(left - right))
        case Operator.MULT:
            return Ok(value_types.Integer(left * right))
        case Operator.DIV:
            return Ok(value_types.Integer(left // right))
        case _:
            return create_err(
                errors.UnsuportedInfixOperation(left_operand, operator, right_operand)
            )


def evaluate_float_infix_expression(
    left_operand: value_types.Float | value_types.Integer,
    operator: Operator,
    right_operand: value_types.Float | value_types.Integer,
) -> Result[Value, EvaluationError]:
    """
    Applies given binary operator on given integer/float operands,
    with result of a float value if possible.
    """
    left = left_operand.value
    right = right_operand.value
    match operator:
        case Operator.PLUS:
            return Ok(value_types.Float(left + right))
        case Operator.MINUS:
            return Ok(value_types.Float(left - right))
        case Operator.MULT:
            return Ok(value_types.Float(left * right))
        case Operator.DIV:
            return Ok(value_types.Float(left / right))
        case _:
            return create_err(
                errors.UnsuportedInfixOperation(left_operand, operator, right_operand)
            )


def evaluate_equality_expression(
    left: Value, operator: Literal[Operator.EQ] | Literal[Operator.NOT_EQ], right: Value
) -> Result[value_types.Boolean, EvaluationError]:
    """Checks equality/not-equalit of to given operands if possible."""
    match (left, right):
        case (ValuedValue(), ValuedValue()):
            pass
        case _:
            return create_err(errors.UnsuportedInfixOperation(left, operator, right))

    match operator:
        case Operator.EQ:
            return Ok(consts.TrueFalse.from_bool(left.value == right.value).value)
        case Operator.NOT_EQ:
            return Ok(consts.TrueFalse.from_bool(left.value != right.value).value)


def evaluate_comparison_expression(
    left: Value,
    operator: Literal[Operator.GT]
    | Literal[Operator.GTEQ]
    | Literal[Operator.LT]
    | Literal[Operator.LTEQ],
    right: Value,
) -> Result[value_types.Boolean, EvaluationError]:
    """Compares two given operands with given operator if possible."""
    match (left, right):
        case (NumericValue(), NumericValue()):
            pass
        case _:
            return create_err(errors.UnsuportedInfixOperation(left, operator, right))

    match operator:
        case Operator.GT:
            return Ok(consts.TrueFalse.from_bool(left.value > right.value).value)
        case Operator.LT:
            return Ok(consts.TrueFalse.from_bool(left.value < right.value).value)
        case Operator.GTEQ:
            return Ok(consts.TrueFalse.from_bool(left.value >= right.value).value)
        case Operator.LTEQ:
            return Ok(consts.TrueFalse.from_bool(left.value <= right.value).value)
