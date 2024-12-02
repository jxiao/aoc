import sys, re
lines = open(0).readlines()

def part_one():
    total = 0
    for s in lines:
        digits = [c for c in s if c.isdigit()]
        total += int(digits[0]+digits[-1])
    return total

def part_two():
    nums = "one two three four five six seven eight nine".split()
    def to_digit_str(s):
        if s in nums:
            return str(nums.index(s)+1)
        return s
    total = 0
    pattern = f"(?=({'|'.join(nums)}|\\d))"
    for s in lines:
        digits = [*map(to_digit_str, re.findall(pattern, s))]
        total += int(digits[0]+digits[-1])
    return total

if not sys.argv or len(sys.argv) < 2 or sys.argv[1] == '1':
    print(part_one())

if not sys.argv or len(sys.argv) < 2 or sys.argv[1] == '2':
    print(part_two())
