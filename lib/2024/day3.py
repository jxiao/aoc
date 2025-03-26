import sys

lines = open(0).readlines()
from typing import Optional

PREFIX = "mul("


# Returns the number up to the first comma, None if one DNE
def extract_num(seq: str) -> tuple[Optional[int], str]:
    sum = 0
    for i, c in enumerate(seq):
        if not c.isnumeric():
            return sum, seq[i:]
        sum = sum * 10 + int(c)
    return sum, ""


# Returns the sum of the decoded sequence starting with this value as well as the remainder
def decode_mul(sequence: str) -> tuple[int, str]:
    try:
        seq_start_idx = sequence.index(PREFIX)
    except ValueError:
        return (0, "")

    start = sequence[seq_start_idx + len(PREFIX) :]
    first_num_opt, remainder = extract_num(start)
    if first_num_opt == None or not remainder or remainder[0] != ",":
        return 0, remainder

    second_num_opt, remainder2 = extract_num(remainder[1:])
    if second_num_opt == None or not remainder2 or remainder2[0] != ")":
        return 0, remainder2

    return first_num_opt * second_num_opt, remainder2


def part_one():
    sum = 0
    for line in lines:
        seq = line
        while seq:
            res, remainder = decode_mul(seq)
            sum += res
            seq = remainder
    return sum


def part_two():
    sum = 0
    for seq in "".join(lines).split("do()"):
        seq = seq.split("don't()")[0]
        while seq:
            res, remainder = decode_mul(seq)
            sum += res
            seq = remainder
    return sum


if not sys.argv or len(sys.argv) < 2 or sys.argv[1] == "1":
    print(part_one())

if not sys.argv or len(sys.argv) < 2 or sys.argv[1] == "2":
    print(part_two())
