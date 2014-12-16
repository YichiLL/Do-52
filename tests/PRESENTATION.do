configure numberOfPlayers: 2 

new Number testNum : 3

setup:
        Player has Number called score     
round:
        do increaseMyScore with player1
        do output with "The score of player1 before: " + player1.score
        do output with "The score of player1 after: " + player1.score
        do output with "testNum: " + testNum
        do output with "number of players: " + numberOfPlayers
        do quit

increaseMyScore with Player p:
                if p.score = 0:
                   p.score : p.score + 1
                   do quit