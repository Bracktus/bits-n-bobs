from copy import deepcopy

class BoardTree:
    
    def __init__(self, board, score, children, move):
        self.board = board
        self.score = score
        self.children = children
        self.move = move

def make_move(player_char, x, y, board):
    new_board = deepcopy(board)
    new_board[x][y] = player_char
    return new_board

def valid_move(x, y, board):
    if x > 2 or x < 0:
        return False
    elif y > 2 or y < 0:
        return False
    elif board[x][y] != " ":
        return False
    else:
        return True

def check_rows(player_char, board):
    for row in board:
        if len([i for i in row if i == player_char]) == 3:
            return True

    return False

def check_cols(player_char, board):
    for i in range(3):
        if all([board[0][i] == player_char,
                board[1][i] == player_char,
                board[2][i] == player_char]):
            return True
    return False

def check_diags(player_char, board):
    diag = []
    anti_diag = []
    for i in range(3):
        diag.append(board[i][i] == player_char)
        anti_diag.append(board[i][2 - i] == player_char)

    return all(diag) or all(anti_diag)
        
def check_win(player_char, board):
    #check horizontals
    return any([check_rows(player_char, board),
                check_cols(player_char, board), 
                check_diags(player_char, board)])

def check_draw(board):
    for i in range(3):
        for j in range(3):
            if board[i][j] == " ":
                return False
    return True

def print_board(board):
    print("    0    1    2")
    for i in range(len(board)):
        print(i , board[i])

def get_total_games(game_tree):

    total = 0
    if game_tree.children == []:
        return 1

    for child_board in game_tree.children:
        total += get_total_games(child_board)
    
    return total


def get_depth(game_tree):
    if game_tree.children == []:
        return 0
    else:
        max_depth = max(map(lambda x: get_depth(x), game_tree.children)) + 1

    return max_depth

def make_ai_move(ai_char, player_char, board):
    #using the minmax algo
    root_board = BoardTree(board, 0, [], None)
    game_tree = build_tree(ai_char, ai_char, player_char, root_board, 0)
    move = get_best_move(game_tree)
    return make_move(ai_char, move[0], move[1], board)

def build_tree(current_char, ai_char, player_char, root_board, depth):
    board = root_board.board

    if check_win(ai_char, board):
        root_board.score = 10 - depth
        return root_board

    elif check_win(player_char, board):
        root_board.score = depth - 10
        return root_board

    elif check_draw(board):
        root_board.score = depth
        return root_board

    else:
        for move in get_possible_moves(board):
            #get the corresponding board
            poss_board = make_move(current_char, move[0], move[1], board)

            #create a child board
            child_board = BoardTree(poss_board, 0, [], move)
            #populate the child boards's children
            child_board = build_tree(flip_player(current_char),
                                     ai_char,
                                     player_char,
                                     child_board,
                                     depth + 1)
             
            #populate the current board's children
            root_board.children.append(child_board)

    return root_board

def get_minmax_score(game_tree):
    """Takes in a game tree and returns the minmax score"""

    score = 0
    if game_tree.children == []:
        return game_tree.score

    for child_board in game_tree.children:
        score += get_minmax_score(child_board)
    
    return score



def get_best_move(game_tree):
    scores = []
    best_move = None
    best_score = -1000000

    deepest_move = None
    best_depth = 0
   
    for child in game_tree.children:
        child_score = get_minmax_score(child)
        # print(child_score)
        # print(child.move)

        if child_score > best_score:
            best_score = child_score
            best_move = child.move

    return best_move

def get_possible_moves(board):
    moves = []
    for x in range(3):
        for y in range(3):
                if board[x][y] == " ":
                    moves.append([x,y])
    return moves

def flip_player(curr_player):
    return "X" if curr_player == "O" else "O"
    
def main():
    board = [[" ", " ", " "],
             [" ", " ", " "],
             [" ", " ", " "]]

    fin = False
    curr_player = "O"    
    while not fin:
        if curr_player == "O":
            print_board(board)
            x = int(input("Enter a row "))
            y = int(input("Enter a column "))

            if not valid_move(x, y, board):
                print("invalid move!")
                continue
            else:
                board = make_move(curr_player, x, y, board)

        elif curr_player == "X":
            board = make_ai_move("X", "O", board)

        if check_win(curr_player, board):
            print_board(board)
            print(f"{curr_player} wins!")
            fin = True
        elif check_draw(board):
            print_board(board)
            print("Draw!")
            fin = True
        else:
            curr_player = flip_player(curr_player)
            print(f"Player {curr_player}'s turn")


# board = [["X", " ", " "],
#          ["O", "O", " "],
#          [" ", " ", " "]]

# print_board(make_ai_move("X", "O", board))


if __name__ == "__main__":
    main()

