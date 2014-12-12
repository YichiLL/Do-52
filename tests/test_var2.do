configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	new Number x : 4
	new Number y : 9 + 3 * 2
	do output with (x + y)
