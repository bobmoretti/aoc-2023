def part1():
    with open('../../haskell/data/day01-input.txt') as f:

        def parse_int(line: str):
            digits = [c for c in line if c.isdigit()]
            return int(digits[0] + digits[-1])

        return sum([parse_int(line) for line in f])


digit_by_name = {"one": 1, "two": 2, "three": 3, "four": 4,
                 "five": 5, "six": 6, "seven": 7, "eight": 8, "nine": 9}


def match_digit_name(s: str):
    for name in digit_by_name:
        if s[:len(name)] == name:
            return digit_by_name[name]
    return -1


def part2():
    with open('../../haskell/data/day01-input.txt') as f:
        def parse_int(line: str):
            def try_parse(substr: str) -> int:
                if substr[0].isdigit():
                    return int(substr[0])
                return match_digit_name(substr)

            digits = [try_parse(line[idx:]) for idx, _ in enumerate(line)]
            digits = [d for d in digits if d >= 0]
            return int(digits[0]*10 + digits[-1])
        return sum([parse_int(line) for line in f])


if __name__ == '__main__':
    print(f"part 1: {part1()}")
    print(f"part 2: {part2()}")
