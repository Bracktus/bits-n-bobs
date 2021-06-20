import matplotlib.pyplot as plt
import random

def pars_game(ra, rb):
    prob = ra / (ra + rb)

    a_score = 0
    b_score = 0
    while not((((a_score - b_score) >= 2) or (b_score - a_score >= 1)) and ((a_score >= 11) or (b_score >= 11))):
        #while NOT (either player a OR b is leading by 2 points AND either a OR b has at least 11 points):
        random_num = random.random()
        if prob > random_num:
            a_score += 1
        else:
            b_score += 1

    return (a_score, b_score)

def english_game(ra,rb):
    prob = ra/(ra+rb)
    player_scores = {"a":0, "b":0}
    starting_player = random.randint(-1,1)

    if starting_player == 0:
        server = "a"
    else:
        server = "b"

    while ((player_scores["a"] <= 9) and (player_scores["b"] <= 9)):
        #While a player hasn't reached 9 points
        random_num = random.random()
        if server == "a":
            if prob > random_num:
                player_scores[server] = player_scores[server] + 1
            else:
                server = "b"
        elif server == "b":
            if prob < random_num:
                player_scores[server] = player_scores[server] + 1
            else:
                server = "a"
        #print(f"{server}'s serve {(player_scores['a'], player_scores['b'])}")

    return(player_scores["a"], player_scores["b"])

def winProbability(ra, rb, n, game):
    """This simulates a match n times
       Each win and loss is recorded
       The probability of winning is then calculated and returned"""
    win_loss = [0,0]
    for i in range(0, n):
        if game == "pars":
            game_result = pars_game(ra, rb)
            if game_result[0] > game_result[1]:
                win_loss[0] = win_loss[0] + 1
            else:
                win_loss[1] = win_loss[1] + 1
        elif game == "english":
            game_result = english_game(ra, rb)
            if game_result[0] > game_result[1]:
                win_loss[0] = win_loss[0] + 1
            else:
                win_loss[1] = win_loss[1] + 1
        else:
            return -1

    return win_loss[0] / (win_loss[0] + win_loss[1])

values_pars = []
nums =  [i for i in range(0, 100)]
values_english = []
for i in range(0, 100):
    values_pars.append(winProbability(i, 100 - i, 10000, "pars"))
    values_english.append(winProbability(i, 100 - i, 10000, "english"))

plt.plot(nums,values_pars , label = "PARS")
plt.plot(nums,values_english, label = "English")

plt.legend(loc="upper left")
plt.xlabel("Skill Level")
plt.ylabel("probability")
plt.show()


def calculate_time(ra, rb, game):
    num_of_matches = 0
    game_result = pars_game(ra, rb)
    if game == "pars":
        num_of_matches += game_result[0]
        num_of_matches += game_result[1]
    elif game == "english":
        game_result = english_game(ra, rb)
        num_of_matches += game_result[0]
        num_of_matches += game_result[1]
    else:
        return -1

    return (num_of_matches) * 5

time_pars = 0
time_eng = 0
n = 100000
for i in range(n):
    time_pars += calculate_time(50,50, "pars")
    time_eng += calculate_time(50,50, "english")

print(f"Average Time for PARS ruleset (mins): {time_pars/n}")
print(f"Average Time for English ruleset (mins):{time_eng/n}")
