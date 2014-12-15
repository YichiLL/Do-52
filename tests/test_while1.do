configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	new Number a : 5
	while (a >= 0):
		a : a - 1
		do output with a
