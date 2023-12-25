from abc import ABC, abstractmethod


class ASTNode(ABC):
    @abstractmethod
    def __str__(self) -> str:
        ...


class Statement(ASTNode, ABC):
    ...


class Expression(ASTNode, ABC):
    ...
