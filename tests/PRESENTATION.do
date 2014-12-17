// war in do
configure numberOfPlayers: 2 
configure highestCard: ace
configure ascendingOrder: true

Player has Number called x // makes player.table available

new Number warCount: 0

setup:
	// deal cards
	do turn with player1
	do output with "The score of player1 before: " + player1.x
	do output with "The score of player1 after: " + player1.x
	do output with "testNum: " + x

round:
	do output with "number of players: " + numberOfPlayers
	do quit

turn with Player player:
	if p.x = 0:
		p.x : p.x + 1
		do quit
