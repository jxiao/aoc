import sys
from enum import Enum
from typing import List

lines = open(0).readlines()


class Slope(Enum):
    INC = 1
    DEC = 2
    ZERO = 3
    UNKNOWN = 4


def calc_slope(x: int, y: int) -> Slope:
    diff = y - x
    if diff > 0:
        return Slope.INC
    elif diff < 0:
        return Slope.DEC
    return Slope.ZERO


def is_safe(vals: List[int]) -> bool:
    if len(set(vals)) < len(vals):
        return False
    slope = calc_slope(vals[0], vals[1])
    for i in range(1, len(vals)):
        if calc_slope(vals[i - 1], vals[i]) != slope or abs(vals[i - 1] - vals[i]) > 3:
            return False
    return True


def part_one():
    return sum(1 for line in lines if is_safe([int(v) for v in line.split()]))


def is_safe_splice_one(vals: List[int]) -> bool:
    for i in range(len(vals)):
        spliced = vals[:i] + vals[i + 1 :]
        if is_safe(spliced):
            return True
    return False


def part_two():
    return sum(1 for l in lines if is_safe_splice_one([int(v) for v in l.split()]))


if not sys.argv or len(sys.argv) < 2 or sys.argv[1] == "1":
    print(part_one())

if not sys.argv or len(sys.argv) < 2 or sys.argv[1] == "2":
    print(part_two())
