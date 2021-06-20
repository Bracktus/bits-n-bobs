import random, csv, itertools
import matplotlib.pyplot as plt

def game(ra, rb):

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

random.seed(57)
print("game(70,30): " + str(game(70,30)))

def winProbability(ra, rb, n):
    """This simulates a match n times
       Each win and loss is recorded
       The probability of winning is then calculated and returned"""
    win_loss = [0,0]
    for i in range(0, n):
        game_result = game(ra, rb)
        if game_result[0] > game_result[1]:
            win_loss[0] = win_loss[0] + 1
        else:
            win_loss[1] = win_loss[1] + 1
    return win_loss[0] / (win_loss[0] + win_loss[1])

print("winProbability(70,30,100000): " + str(winProbability(70,30,100000))) 

def winProbability_up_to_k(ra, rb, n, k):
    """This simulates a game up to k matches n times
       Then it returns the probability of A winning
    """
    #If A or B wins k games then we should start another match
    #until we reach n matches
    win_loss_games = [0,0]
    for i in range(0,n):
        win_loss_matches = [0,0]
        while ((win_loss_matches[0] < k) and (win_loss_matches[1] < k)):
            # neither a or b have exceeded k wins
            game_result = game(ra, rb)
            if game_result[0] > game_result[1]:
                win_loss_matches[0] = win_loss_matches[0] + 1
            else:
                win_loss_matches[1] = win_loss_matches[1] + 1

        print("Match", i+1, win_loss_matches)

        if win_loss_matches[0]> win_loss_matches[1]:
            win_loss_games[0] = win_loss_games[0] + 1
        else:
            win_loss_games[1] = win_loss_games[1] + 1

    return win_loss_games[0] / (win_loss_games[0] + win_loss_games[1])


# probability = 0
# count = 0
# while probability < 0.9:
#      count += 1
#      probability = winProbability_up_to_k(60, 40,500 ,count)
#      print(count,"", probability)
#
# print(count)


def csvReader(filename):
    tuple_list = []
    with open(filename) as csvfile:
        rdr = csv.reader(csvfile)
        next(rdr)
        for row in rdr:
            tuple_list.append((row[0],row[1]))

    return tuple_list

print(csvReader("test.csv"))

def plot_graph(x_name, y_name, values):
    #plot prob as x, ra/rb as y
    probablilities = []
    skill_ratios = []
    for pair in values:
        a_skill = int(pair[0])
        b_skill = int(pair[1])
        probablilities.append(a_skill/(b_skill + a_skill))
        skill_ratios.append(a_skill/b_skill)

    plt.plot(probablilities, skill_ratios)
    plt.xlabel(x_name)
    plt.ylabel(y_name)
    plt.show()

plot_graph("probablilty", "skill ratio", csvReader("test.csv"))
