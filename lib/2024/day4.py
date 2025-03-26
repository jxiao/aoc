import sys
from typing import Optional
from enum import Enum

lines = open(0).readlines()
matrix = [list(line.strip()) for line in lines]
ROWS, COLS = len(matrix), len(matrix[0])
KEYWORD = "XMAS"


class Direction(Enum):
    NORTH = (-1, 0)
    SOUTH = (1, 0)
    WEST = (0, -1)
    EAST = (0, 1)
    NORTH_WEST = (-1, -1)
    NORTH_EAST = (-1, 1)
    SOUTH_WEST = (1, -1)
    SOUTH_EAST = (1, 1)


def is_out_of_bounds(r: int, c: int, dir: Optional[Direction] = None) -> bool:
    dr, dc = dir.value if dir else (0, 0)
    new_r, new_c = r + dr, c + dc
    return new_r < 0 or new_r >= ROWS or new_c < 0 or new_c >= COLS


def get_char_in_dir(r: int, c: int, dir: Direction) -> Optional[str]:
    dr, dc = dir.value
    new_r, new_c = r + dr, c + dc
    if is_out_of_bounds(new_r, new_c):
        return None
    return matrix[new_r][new_c]


def count_dir(row: int, col: int, dir: Direction, prefix: str) -> int:
    dr, dc = dir.value
    for i, c in enumerate(prefix):
        new_r, new_c = row + i * dr, col + i * dc
        if (
            is_out_of_bounds(new_r, new_c)
            or matrix[new_r][new_c] != c
        ):
            return 0
    return 1


def count_occurrences_in_direction(dir: Direction) -> int:
    count = 0
    for r in range(ROWS):
        for c in range(COLS):
            count += count_dir(r, c, dir, KEYWORD)
    return count


def part_one():
    return sum(count_occurrences_in_direction(d) for d in Direction)


def part_two():
    count = 0
    for r in range(ROWS):
        for c in range(COLS):
            if matrix[r][c] == "A":
                chars = set(
                    [
                        get_char_in_dir(r, c, Direction.NORTH_WEST),
                        get_char_in_dir(r, c, Direction.SOUTH_EAST),
                    ]
                )
                if chars != set(["M", "S"]):
                    continue
                chars = set(
                    [
                        get_char_in_dir(r, c, Direction.NORTH_EAST),
                        get_char_in_dir(r, c, Direction.SOUTH_WEST),
                    ]
                )
                if chars != set(["M", "S"]):
                    continue
                count += 1
    return count


if not sys.argv or len(sys.argv) < 2 or sys.argv[1] == "1":
    print(part_one())

if not sys.argv or len(sys.argv) < 2 or sys.argv[1] == "2":
    print(part_two())
