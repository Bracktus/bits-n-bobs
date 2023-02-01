import sys
import random
import math
import datetime
from collections import defaultdict


def kemeny_inital(tourney):
    """
    The inital ranking is 1,2 ... n
    Therefore we can look at the pairings (p1, p2) where p1 > p2 and sum them
    """
    score = 0
    for p1, p2 in tourney.keys():
        if p1 > p2:
            score += tourney[(p1, p2)]

    return score


def kemeny_naive(ranking, tourney):
    """Naive way of calculating scores"""
    score = 0
    for i in range(len(ranking)):
        for j in range(i + 1, len(ranking)):
            pair = (ranking[j], ranking[i])
            if pair in tourney:
                score += tourney[pair]
    return score


def kemeny_quick(ranking, tourney):
    """Maybe a faster way of calculating scores"""
    score = 0
    player_index_map = {}
    for idx, player in enumerate(ranking):
        player_index_map[player] = idx

    for p1, p2 in tourney:
        if player_index_map[p1] > player_index_map[p2]:
            score += tourney[(p1, p2)]

    return score


def print_ranking(ranking):
    for idx, playerID in enumerate(ranking):
        print(f"{idx + 1}.\t {players[playerID]}")


def get_neighbour(ranking, tourney, orig_score):
    idx_1 = random.randrange(0, len(ranking) - 1)
    idx_2 = idx_1 + 1

    new_rnk = ranking.copy()

    new_rnk[idx_1], new_rnk[idx_2] = new_rnk[idx_2], new_rnk[idx_1]
    score = orig_score

    l_plr = new_rnk[idx_1]
    h_plr = new_rnk[idx_2]

    pair_1 = (l_plr, h_plr)
    pair_2 = (h_plr, l_plr)

    score -= tourney[pair_1]
    score += tourney[pair_2]

    return score, new_rnk


def simulated_annealing(inital_solution,
                        tourney,
                        initial_temperature,
                        temperature_length,
                        cooling_ratio,
                        num_non_improve):

    # Set the inital and the best solutions
    curr_sol = inital_solution
    best_sol = inital_solution
    curr_score = kemeny_inital(tourney)
    best_score = curr_score

    non_improve_count = 0
    temperature = initial_temperature
    uphill_count = 0

    # While the stopping criterion is not in effect
    while non_improve_count < num_non_improve:
        # Run inner loop for temperature_length iterations
        for _ in range(temperature_length):
            # Get a neighbour
            new_score, new_sol = get_neighbour(curr_sol,
                                               tourney,
                                               curr_score)
            # Get delta_C
            score_diff = new_score - curr_score

            # If the neighbours score is better than the current solution
            if score_diff <= 0:
                # Then set the current neighbourhood to it
                curr_score = new_score
                curr_sol = new_sol

                # If it's the best solution found so far
                if new_score < best_score:
                    # Then set it to the best solution
                    non_improve_count = 0
                    best_score = new_score
                    best_sol = new_sol.copy()
                else:
                    non_improve_count += 1

            else:
                non_improve_count += 1
                # Generate a number between 0 and 1
                q = random.random()
                # If it's greater than e^{-deltaC/T}
                if q < math.exp(-score_diff/temperature):
                    # Then make an uphill move
                    uphill_count += 1
                    curr_sol = new_sol
                    curr_score = new_score

        # set T to f(T), where f(T) = a * T
        temperature *= cooling_ratio

    return best_score, best_sol, uphill_count


time_start = datetime.datetime.now()
players = {}
tourney = defaultdict(int)

num_participants = 0
c = 1

with open(sys.argv[1], "r") as file:
    for line in file:
        if c == 1:
            num_participants = int(line)

        elif c > 1 and c < num_participants + 2:
            match = line.split(",")
            p_num, p_name = match
            players[int(p_num)] = p_name.strip()

        elif c > num_participants + 2:
            match = line.split(",")
            w, p1, p2 = match
            pair = (int(p1), int(p2))
            tourney[pair] = int(w)

        c += 1

inital_ranking = [i for i in range(1, num_participants + 1)]
score, ranking, uphill_count = simulated_annealing(
                        inital_solution=inital_ranking,
                        tourney=tourney,
                        initial_temperature=0.45,
                        temperature_length=162500,
                        cooling_ratio=0.99,
                        num_non_improve=1
                    )


print_ranking(ranking)
time_end = datetime.datetime.now()
time_diff = time_end - time_start
print(f"Elapsed time: {time_diff.total_seconds()*1000}ms")
print(f"Kemeny Score: {score}")
print(f"uphill moves: {uphill_count}")
