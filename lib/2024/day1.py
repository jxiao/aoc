import sys
from collections import Counter

lines = open(0).readlines()


def part_one():
    left, right = zip(
        *[(int(line.split()[0]), int(line.split()[-1])) for line in lines]
    )
    l, r = sorted(list(left)), sorted(list(right))
    return sum(abs(v1 - v2) for v1, v2 in zip(l, r))


def part_two():
    left, right = zip(
        *[(int(line.split()[0]), int(line.split()[-1])) for line in lines]
    )
    freq = Counter(right)
    return sum(v * freq[v] for v in left)


if not sys.argv or len(sys.argv) < 2 or sys.argv[1] == "1":
    print(part_one())

if not sys.argv or len(sys.argv) < 2 or sys.argv[1] == "2":
    print(part_two())
