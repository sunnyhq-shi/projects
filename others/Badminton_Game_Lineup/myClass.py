import numpy as np
import random
import copy
# Game class represents a game with a title and players
# Title: string of the format "player1/player2/player3/player4" or "plaer1/player2"
# Players: list of strings representing player names
class Game:
    def __init__(self, name):
        self.title = name.strip()
        self.players = self.title.split('/')

# Board class represents the current schedule
# schedule: 3 * 8 array, with each element being an integer representation of a game
# adj: adjacency matrix for the games
class Board:

    def __init__(self, schedule, adj):
        self.schedule = schedule
        self.adj = adj
        self.score_value = 0
        self.conflicts = None
        self.score()

    # return an array of array of index for games with conflict
    def check_conflict(self, time1):
        conflicts = list()
        time1_games = self.schedule[time1]
        withinAdj = self.adj[time1_games][:,time1_games]
        if time1 == len(self.schedule)-1:
            idx = np.where(withinAdj == 1)[0]
            if(len(idx) != 0):
                curr_conf = list([list([time1,x]) for x in idx])
                conflicts.extend(curr_conf)
        else: 
            time2_games = self.schedule[time1+1]
            crossAdj = self.adj[time1_games][:,time2_games]
            for i in [withinAdj, crossAdj]:
                idx = np.where(i == 1)[0]
                if(len(idx) != 0):
                    curr_conf = list([list([time1,x]) for x in idx])
                    conflicts.extend(curr_conf)
    
        return np.unique(conflicts, axis = 0)

                
    def score(self):
        score = 0
        tmp_lst = list()
        for i in range(len(self.schedule)):
            t1 = i
            curr_conf = self.check_conflict(t1)
            score += len(curr_conf)
            if len(curr_conf!=0):   
                tmp_lst.append(curr_conf)
        self.score_value = score
        self.conflicts = tmp_lst

        return
        
    
    def swapGames(self, idx1, idx2):
        new = copy.deepcopy(self.schedule)
        tmp = new[idx1[0]][idx1[1]] 
        new[idx1[0]][idx1[1]] = new[idx2[0]][idx2[1]] 
        new[idx2[0]][idx2[1]] = tmp

        return new
        

                
    
    
    




    




