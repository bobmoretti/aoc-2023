from collections import defaultdict
from math import prod


def part1():
    with open('../../haskell/data/day02-input.txt') as f:
        games = [parse_game(line) for line in f]
        return sum(game_score(*g) for g in games)


def game_score(game_id, trials):
    def is_valid(trial):
        return all((trial['red'] <= 12,
                    trial['green'] <= 13,
                    trial['blue'] <= 14))
    if all(is_valid(t) for t in trials):
        return game_id
    else:
        return 0


def parse_trial(text: str):
    groups = text.split(',')
    group_record = defaultdict(int)

    def parse_group(group_input: str):
        num, color = group_input.strip().split()
        group_record[color] += int(num)

    for group_input in groups:
        parse_group(group_input)
    return group_record


def parse_game(line: str):
    first, rest = line.split(':')
    game_id = int(first.split()[-1])
    trials = [parse_trial(trial) for trial in rest.split(';')]
    return game_id, trials


def product_of_maxes(trials):
    maxes = defaultdict(int)
    for trial in trials:
        maxes['red'] = max(maxes['red'], trial['red'])
        maxes['green'] = max(maxes['green'], trial['green'])
        maxes['blue'] = max(maxes['blue'], trial['blue'])
    return prod(maxes.values())


def part2():
    with open('../../haskell/data/day02-input.txt') as f:
        games = [parse_game(line) for line in f]
        return sum(product_of_maxes(trials) for game_id, trials in games)


if __name__ == '__main__':
    print(f"part 1: {part1()}")
    print(f"part 2: {part2()}")
