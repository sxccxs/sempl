from itertools import chain
from typing import Iterable, TypeVar, overload

from result import Err, Ok, Result

T = TypeVar("T")
E = TypeVar("E")

Ex = TypeVar("Ex", bound=Exception)


@overload
def results_gather(*results_it: Result[T, E]) -> Result[list[T], E]:
    """
    Returns ok values of provided results as a list, if all results were OK.
    Else returns first encountered Err result.
    Ok values order corresponds to the order of results.

    Args:
        *results (Result[T, E]): Results to gather.

    Returns:
        Result[list[T], E]: Result of gathering.
    """


@overload
def results_gather(*results_it: Iterable[Result[T, E]]) -> Result[list[T], E]:
    """
    Returns ok values of provided results iterables as a list, if all results were OK.
    Else returns first encountered Err result.
    Ok values order corresponds to the order of results.

    Args:
        *results_it (Iterable[Result[T, E]]): Results iterables to gather.

    Returns:
        Result[list[T], E]: Result of gathering.
    """


@overload
def results_gather(results: Iterable[Result[T, E]]) -> Result[list[T], E]:
    """
    Returns ok values of provided results as a list, if all results were OK.
    Else returns first encountered Err result.
    Ok values order corresponds to the order of results.

    Args:
        results (Iterable[Result[T, E]]): Results to gather.

    Returns:
        Result[list[T], E]: Result of gathering.
    """


def results_gather(
    results: Iterable[Result[T, E]] | Result[T, E],
    *results_it: Iterable[Result[T, E]] | Result[T, E],
) -> Result[list[T], E]:
    """
    Returns ok values of provided results as a list, if all results were OK.
    Else returns first encountered Err result.
    Ok values order corresponds to the order of results.
    """
    results_iterable: Iterable[Result[T, E]]
    if isinstance(results, (Ok, Err)):
        results_iterable = (results, *results_it)  # type: ignore
    else:
        results_iterable = chain(results, *results_it)  # type: ignore

    values: list[T] = []
    for res in results_iterable:
        match res:
            case Err() as err:
                return err
            case Ok(value):
                values.append(value)
    return Ok(values)


def create_err(ex: Ex) -> Err[Ex]:
    """Creates error with exception with traceback."""
    try:
        raise ex
    except type(ex) as err:
        return Err(err)
