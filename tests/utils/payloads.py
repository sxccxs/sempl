from typing import NamedTuple


class ExpectedLetStatement(NamedTuple):
    mut: bool
    type: str
    name: str
