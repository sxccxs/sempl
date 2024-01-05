from dataclasses import dataclass, field
from io import StringIO


@dataclass(slots=True)
class Error:
    """Base error type."""

    msg: str
    notes: list[str] = field(init=False, default_factory=list)

    def add_note(self, note: str) -> None:
        """Adds note to an error."""
        self.notes.append(note)

    def __str__(self) -> str:
        ss = StringIO()
        for note in reversed(self.notes):
            ss.write(f"{note}\n")
        ss.write(f"{self.msg}")
        return ss.getvalue()
