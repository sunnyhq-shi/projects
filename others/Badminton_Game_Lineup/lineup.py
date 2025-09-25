import numpy as np
import random
import heapq
from myClass import Game, Board

# global counter
ecount = 0

def main():
    # list of game objects
    all_games = list()
    # open file
    ncols = int(input("enter number of courts: "))
    nrows = int(input("enter number of time slots: "))
    filename = input("enter game .txt file here: ") 
    
    init = np.zeros((nrows, ncols), dtype= int)
    counter = 0

    with open(filename, "r") as f:
        for line in f: 
            if(counter >= ncols * nrows):
                print("more games than can be accomdated")
                return 
            new_game = Game(line)
            all_games.append(new_game)
            row = counter//ncols
            col = counter%ncols
            init[row, col] = counter
            counter += 1

    # adjacency matrix between games 1 if has overlap player; 0 otherwise
    ngames = len(all_games)
    adj_matrix = np.zeros((ngames, ngames), dtype=int)
    for i in range(ngames):
        curr_game = all_games[i]
        for j in range(i+1, ngames):
            comp_game = all_games[j]
            for player in curr_game.players:
                if player in comp_game.players:
                    adj_matrix[i][j] = 1
                    adj_matrix[j][i] = 1

    # initialize
    start = Board(init, adj_matrix)
    score = start.score_value
    pq = []
    heapq.heappush(pq, (score, ecount, start))
 
    # check if solution is found
    check = 0
    final = None
    while(ecount <= 5000 and len(pq)>0):
        # print(ecount)
        curr = heapq.heappop(pq)
        curr_score = curr[0]
        curr_board = curr[2]
        if(curr_score == 0):
            check = 1
            final = curr_board
            break
        createChild(curr_board, pq)
        del curr_board
    
    if(check == 0):
        print("no solution found")
        print(curr_score)
    else:
        print("solution found")
    
    return
    
    
def createChild(board, this_pq):
    global ecount
    for slice in board.conflicts:
        for idx in slice:
            time = idx[0] 
            court_idx = random.randint(0, len(board.schedule[0])-1)
            time_idx = random.randint(0, len(board.schedule)-1)
            while(time_idx == time):
                time_idx = random.randint(0, len(board.schedule)-1)
            new_schedule = board.swapGames(idx, [time_idx, court_idx])  
            new_board = Board(new_schedule, board.adj)  
            score = new_board.score_value
            if(score <= board.score_value + 2):
                ecount += 1
                heapq.heappush(this_pq, (score, ecount, new_board))
            else:
                # delete the new board
                del new_board
    return 

def printSchedule(schedule, games):
    table = np.empty((len(schedule), len(schedule[0])), dtype=object)
    for i in range(len(schedule)):
        for j in range(len(schedule[i])):
            idx = schedule[i][j]
            table[i][j] = games[idx].title
    header_info = "court 1, court 2, court 3"
    np.savetxt('schedule.csv', table, delimiter = ',', fmt = '%s', header= header_info, comments="")
    return


if __name__ == "__main__":
    main()

    





