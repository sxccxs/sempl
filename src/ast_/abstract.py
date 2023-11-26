from abc import ABC, abstractmethod


class ASTNode(ABC):
    @property
    @abstractmethod
    def token_literal(self) -> str:
        """The literal value of the token the node is associated with."""


class Statement(ASTNode, ABC):
    ...


class Expression(ASTNode, ABC):
    ...
