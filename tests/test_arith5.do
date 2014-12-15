configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	new Number n : (5 * 4) / ((1 + 1) * (2 - 1))
	do output with n
	do quit
