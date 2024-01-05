"""Concrete AST nodes evaluators."""
from functools import partial
from itertools import zip_longest
from typing import Iterable, Literal, OrderedDict, cast

from result import Err, Ok, Result, do, is_err

from src.ast import ast_nodes
from src.ast.abstract import ASTNode, Statement
from src.evaluation import errors
from src.evaluation.errors import EvaluationError
from src.evaluation.values import consts, value_types
from src.evaluation.values.scope import FuncEntry, FuncParam, Scope, VarEntry
from src.evaluation.values.value_base import NumericValue, Value, ValuedValue
from src.helpers.result_helpers import err_with_note, results_gather
from src.parser.types import Operator


# pylint: disable=too-many-return-statements
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
            return Ok(value_types.Int(node.value))
        case ast_nodes.FloatLiteral():
            return Ok(value_types.Float(node.value))
        case ast_nodes.Program():
            match evaluate_statements(node.statements, scope):
                case Err() as err:
                    return err
                case Ok(value_types.ReturnValue()):
                    return Err(EvaluationError("Unexpected return outside of the function."))
                case Ok() as ok:
                    return ok
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
        case ast_nodes.BlockStatement():
            block_scope = Scope.from_outer(scope)
            match evaluate_statements(node.statements, block_scope):
                case Err() as err:
                    return err
                case Ok(value_types.ReturnValue()) as ok:
                    return ok
                case _:
                    return Ok(consts.NO_EFFECT)
        case ast_nodes.Identifier():
            return evaluate_indetifier(node, scope)
        case ast_nodes.ReturnStatement():
            if node.return_value is None:
                return Ok(value_types.ReturnValue(consts.SINGULARITY))
            match evaluate(node.return_value, scope):
                case Err() as err:
                    return err_with_note(err, "return statement")
                case Ok(value):
                    return Ok(value_types.ReturnValue(value))
        case ast_nodes.LetStatement():
            return evaluate_let_statement(node, scope)
        case ast_nodes.FuncStatement():
            return evaluate_function_statement(node, scope)
        case ast_nodes.CallExpression():
            return evaluate_function_call(node, scope)
        case ast_nodes.Assignment():
            return evaluate_assignment(node, scope)
        case _:
            return Err(errors.UnsuportedNodeError(node))


def evaluate_statements(stmts: Iterable[Statement], scope: Scope) -> Result[Value, EvaluationError]:
    """
    Evaluates all provided statements.

    Args:
        stmts (Iterable[Statement]): Statements to evaluate.
        scope (Scope): Scope of evaluation.

    Returns:
        Result[Value, EvaluationError]: Value of last evaluated statement or first occured error.
    """
    last: Value = consts.NO_EFFECT
    for stmt in stmts:
        match evaluate(stmt, scope):
            case Err() as err:
                return err
            case Ok(value_types.ReturnValue() as value):
                return Ok(value)
            case Ok(value):
                last = value
    return Ok(last)


def evaluate_let_statement(
    node: ast_nodes.LetStatement, scope: Scope
) -> Result[value_types.NoEffect, EvaluationError]:
    """Evaluates given let statement in provided scope if possible.

    Args:
        node (ast_nodes.LetStatement): Let statement.
        scope (Scope): Scope in which the variable is defined.

    Returns:
        Result[value_types.NoEffect, EvaluationError]: Ok(consts.NO_EFFECT) constant
        if evaluation succeeded, else corresponding error.
    """
    err_with_note_ = partial(
        err_with_note, note=f"let statement of variable `{node.var_name.value}`"
    )
    match results_gather(evaluate(node.var_value, scope), evaluate(node.var_type, scope)):
        case Err() as err:
            return err_with_note_(err)
        case Ok(vals):
            var_value, var_type = vals
    match var_type:
        case value_types.Type():
            pass
        case _:
            return err_with_note_(
                EvaluationError(f"Expected type identifier, received {type(var_type)}")
            )
    if not isinstance(var_value, var_type.value):
        return err_with_note_(errors.InvalidType(var_value, var_type.value))
    scope[node.var_name.value] = VarEntry(var_value, node.is_mut, var_type)
    return Ok(consts.NO_EFFECT)


def evaluate_indetifier(node: ast_nodes.Identifier, scope: Scope) -> Result[Value, EvaluationError]:
    """Evaluates given identifier if it is defined in provided scope."""
    if (entry := scope.get(node.value)) is None:
        return Err(EvaluationError(f"Identifier `{node.value}` not found."))
    if entry.value is None:
        return Err(EvaluationError(f"Identifier `{node.value}` can not be used as a value."))
    return Ok(entry.value)


def evaluate_assignment(node: ast_nodes.Assignment, scope: Scope) -> Result[Value, EvaluationError]:
    """Evaluates given assignment in provided scope if possible."""
    if not isinstance(node.assignee, ast_nodes.Identifier):
        return Err(EvaluationError("Left part of assignment must be an identifier."))
    name = node.assignee.value
    match var := scope.get(name):
        case None:
            return Err(EvaluationError(f"Identifier `{name}` not found."))
        case VarEntry():
            pass
        case _:
            return Err(EvaluationError(f"Identifier `{name}` is not assignable."))
    if not var.is_mut:
        return Err(EvaluationError(f"Identifier `{name}` is not mutable."))
    match evaluate(node.value, scope):
        case Err() as err:
            return err
        case Ok(value):
            new_value = value
    if not isinstance(new_value, var.type_value.value):
        return Err(errors.InvalidType(new_value, var.type_value.value))
    var.var_value = new_value
    return Ok(new_value)


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
            return Err(errors.UnsuportedPrefixOperator(operator))


def evaluate_prefix_minus_expression(operand: Value) -> Result[Value, EvaluationError]:
    """Applies unary minus on given operand if possible."""
    match operand:
        case value_types.Int() as i:
            return Ok(value_types.Int(-1 * i.value))
        case value_types.Float() as f:
            return Ok(value_types.Float(-1 * f.value))
        case _:
            return Err(errors.UnsuportedPrefixOperation(Operator.MINUS, operand))


def evaluate_prefix_plus_expression(operand: Value) -> Result[Value, EvaluationError]:
    """Applies unary plus on given operand if possible."""
    match operand:
        case value_types.Int() as i:
            return Ok(i)
        case value_types.Float() as f:
            return Ok(f)
        case _:
            return Err(errors.UnsuportedPrefixOperation(Operator.PLUS, operand))


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
        case (value_types.Int(), value_types.Int()):
            return evaluate_integer_infix_expression(left_operand, operator, right_operand)
        case (
            value_types.Int() | value_types.Float(),
            value_types.Int() | value_types.Float(),
        ):
            return evaluate_float_infix_expression(left_operand, operator, right_operand)
        case _:
            return Err(errors.UnsuportedInfixOperation(left_operand, operator, right_operand))


def evaluate_integer_infix_expression(
    left_operand: value_types.Int, operator: Operator, right_operand: value_types.Int
) -> Result[Value, EvaluationError]:
    """
    Applies given binary operator on given integer operands,
    with result of an integer value if possible.
    """
    left = left_operand.value
    right = right_operand.value
    match operator:
        case Operator.PLUS:
            return Ok(value_types.Int(left + right))
        case Operator.MINUS:
            return Ok(value_types.Int(left - right))
        case Operator.MULT:
            return Ok(value_types.Int(left * right))
        case Operator.DIV:
            if right != 0:
                return Ok(value_types.Int(left // right))
            return Err(EvaluationError("Can't divide by zero."))
        case _:
            return Err(errors.UnsuportedInfixOperation(left_operand, operator, right_operand))


def evaluate_float_infix_expression(
    left_operand: value_types.Float | value_types.Int,
    operator: Operator,
    right_operand: value_types.Float | value_types.Int,
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
            if right != 0:
                return Ok(value_types.Float(left / right))
            return Err(EvaluationError("Can't divide by zero."))
        case _:
            return Err(errors.UnsuportedInfixOperation(left_operand, operator, right_operand))


def evaluate_equality_expression(
    left: Value, operator: Literal[Operator.EQ] | Literal[Operator.NOT_EQ], right: Value
) -> Result[value_types.Bool, EvaluationError]:
    """Checks equality/not-equalit of to given operands if possible."""
    match (left, right):
        case (ValuedValue(), ValuedValue()):
            pass
        case _:
            return Err(errors.UnsuportedInfixOperation(left, operator, right))

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
) -> Result[value_types.Bool, EvaluationError]:
    """Compares two given operands with given operator if possible."""
    match (left, right):
        case (NumericValue(), NumericValue()):
            pass
        case _:
            return Err(errors.UnsuportedInfixOperation(left, operator, right))

    match operator:
        case Operator.GT:
            return Ok(consts.TrueFalse.from_bool(left.value > right.value).value)
        case Operator.LT:
            return Ok(consts.TrueFalse.from_bool(left.value < right.value).value)
        case Operator.GTEQ:
            return Ok(consts.TrueFalse.from_bool(left.value >= right.value).value)
        case Operator.LTEQ:
            return Ok(consts.TrueFalse.from_bool(left.value <= right.value).value)


def evaluate_function_statement(
    node: ast_nodes.FuncStatement, scope: Scope
) -> Result[value_types.NoEffect, EvaluationError]:
    """
    Evaluates given function statemenet and
    adds it to given scope if possible.


    Args:
        node (ast_nodes.FuncStatement): Function statement.
        scope (Scope): Scope when function is being defined.

    Returns:
        Result[value_types.NoEffect, EvaluationError]: Ok(consts.NO_EFFECT) constant
        if evaluation succeeded, else corresponding error.
    """
    err_with_note_ = partial(err_with_note, note=f"function `{node.name.value}` definition")
    match evaluate_function_parameters(node, scope):
        case Err() as err:
            return err_with_note_(err)
        case Ok(values):
            params = values
    match evaluate(node.return_type, scope):
        case Err() as err:
            return err_with_note_(err)
        case Ok(value_types.Type() as value):
            type_ = value
        case Ok(value):
            return err_with_note_(
                EvaluationError(f"Expected type identifier, received {type(value)}")
            )
    scope[node.name.value] = FuncEntry(params, node.body, type_)
    return Ok(consts.NO_EFFECT)


def evaluate_function_parameters(
    node: ast_nodes.FuncStatement, scope: Scope
) -> Result[list[FuncParam], EvaluationError]:
    """
    Evaluates parameters of given function statement if possible.

        Args:
        node (ast_nodes.FuncStatement): Function statement.
        scope (Scope): Scope when function is being defined.
    """
    err_with_note_ = partial(
        err_with_note, note=f"function `{node.name.value}` parameters definition"
    )
    must_have_default = False
    params = OrderedDict[str, FuncParam]()
    for param in node.parameters:
        match evaluate_function_parameter(param, scope, must_have_default):
            case Err() as err:
                return err_with_note_(err)
            case Ok(param):
                if param.name in params:
                    return err_with_note_(
                        EvaluationError(
                            "Function parameters names must be unique. "
                            f"Non-unique name: `{param.name}`."
                        )
                    )
                params[param.name] = param
                must_have_default = (
                    param.default_value is not None
                )  # if param has a default value, next ones also must
    return Ok(list(params.values()))


def evaluate_function_parameter(
    parameter: ast_nodes.FuncParameter, scope: Scope, must_have_default: bool
) -> Result[FuncParam, EvaluationError]:
    """
    Evaluates given function parameter if possible.

        Args:
        node (ast_nodes.FuncStatement): Function statement.
        scope (Scope): Scope when function is being defined.
    """
    err_with_note_ = partial(
        err_with_note, note=f"function parameter `{parameter.name.value}` definition"
    )
    name = parameter.name.value
    match evaluate(parameter.type, scope):
        case Err() as err:
            return err_with_note_(err)
        case Ok(value_types.Type() as value):
            type_ = value
        case Ok(value):
            return err_with_note_(
                EvaluationError(f"Expected type identifier, received {type(value)}")
            )

    if must_have_default and parameter.default_value is None:
        return err_with_note_(
            EvaluationError(f"Parameter `{parameter.name.value}` must have a default value.")
        )

    if parameter.default_value is None:
        return Ok(FuncParam(name, type_, default_value=None))

    match evaluate(parameter.default_value, scope):
        case Err() as err:
            return err_with_note_(err)
        case Ok(value):
            default = value

    if not isinstance(default, type_.value):
        return err_with_note_(errors.InvalidType(value, type_.value))

    return Ok(FuncParam(name, type_, default))


def evaluate_function_call(
    node: ast_nodes.CallExpression, scope: Scope
) -> Result[Value, EvaluationError]:
    """
    Evaluates function call in current scope if possible.

    Args:
        node (ast_nodes.CallExpression): Call expression.
        scope (Scope): Scope in which function was called.
    """
    match node.callable:
        case ast_nodes.Identifier():
            name = node.callable.value
        case _:
            return Err(errors.EvaluationError(f"Expression `{node.callable}` is not callable."))

    err_with_note_ = partial(err_with_note, note=f"function `{name}` call")
    if (func := scope.get(name)) is None or not isinstance(func, FuncEntry):
        return err_with_note_(errors.EvaluationError(f"Function `{name}` was not defined."))

    match results_gather(evaluate(arg, scope) for arg in node.arguments):
        case Err() as err:
            return err_with_note_(err)
        case Ok(val):
            args = val

    func_scope = Scope.from_outer(scope)
    if is_err(res := bind_call_arguments(func.parameters, args, func_scope)):
        return err_with_note_(res)

    match evaluate_statements(func.body.statements, func_scope):
        case Err() as err:
            return err_with_note_(err)
        case Ok(value_types.ReturnValue(value)):
            ret_value = value
        case Ok(value):
            match evaluate_no_return(func, value):
                case Err() as err:
                    return err_with_note_(err)
                case Ok(no_return_val):
                    ret_value = no_return_val

    if not isinstance(ret_value, func.ret_type.value):
        return err_with_note_(errors.InvalidType(ret_value, func.ret_type.value))

    return Ok(ret_value)


def bind_call_arguments(
    params: list[FuncParam], args: list[Value], func_scope: Scope
) -> Result[None, EvaluationError]:
    """Binds given arguments to given parameters in provided scope if possible.

    Args:
        params (list[FuncParam]): Function parameters to bind to.
        args (list[Value]): Argument values to bind.
        func_scope (Scope): Function scope where parameters has to be bind.
    """
    if len(args) > len(params):
        return Err(
            EvaluationError(
                f"Invalid number of parameters: {len(params)} expected, {len(args)} was given."
            )
        )
    for param, arg in zip_longest(params, args, fillvalue=None):
        param = cast(FuncParam, param)
        match arg:
            case None if param.default_value is None:
                return Err(EvaluationError("Not enough arguments provided."))
            case None:
                param_value: Value = param.default_value  # type: ignore
            case Value() as value:
                param_value: Value = value

        if not isinstance(param_value, param.type_value.value):
            return Err(errors.InvalidType(param_value, param.type_value.value))
        func_scope[param.name] = VarEntry(param_value, True, param.type_value)

    return Ok(None)


def evaluate_no_return(func: FuncEntry, value: Value) -> Result[Value, EvaluationError]:
    """
    Checks if given function is valid without a return statement.
    If so, evaluates it's return.
    """
    if isinstance(value, value_types.ReturnValue):
        return Ok(value)

    match func.ret_type.value:
        case value_types.Singularity:
            return Ok(consts.SINGULARITY)
        case _:
            return Err(
                EvaluationError(f"Function has to return a value of type `{func.ret_type.value}`")
            )
