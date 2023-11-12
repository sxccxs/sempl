import re


def is_identifier_char(char: str) -> bool:
    """Returns True if char can be any character of an identifier else False."""
    return char.isalpha() or char == "_"


def is_number_char(char: str) -> bool:
    """Returns True if char can be any (except first) character of a number else False.
    Does not say anything if character can be a first symbol of a number."""
    return char.isdigit() or char == "."


def is_valid_unsigned_float(num: str) -> bool:
    """Returns True if given string is a valid unsigned float number."""
    return bool(re.fullmatch(r"[0-9]+\.[0-9]*", num))