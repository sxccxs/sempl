"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Tests types definitions.
"""
from typing import Generator, TypeVar

T = TypeVar("T")

YieldFixture = Generator[T, None, None]
