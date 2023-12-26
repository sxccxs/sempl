from typing import Iterable, TypeVar

from result import Err, Ok, Result

T = TypeVar("T")
E = TypeVar("E")


def results_gather(results: Iterable[Result[T, E]]) -> Result[list[T], E]:
    """
    Returns ok values of provided results as a list, if all results were OK.
    Else returns first encountered Err result.

    Args:
        results (Iterable[Result[T, E]]): Results to gather.

    Returns:
        Result[list[T], E]: Result of gathering.
    """
    values: list[T] = []
    for res in results:
        match res:
            case Err() as err:
                return err
            case Ok(value):
                values.append(value)
    return Ok(values)
