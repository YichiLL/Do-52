configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	new Number x : 0
	while (x < 5):
		if (x / 2 < 10):
			x : x + 1
			continue	
		else:
			do output with x
	do quit
