configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	new Number n : (10 / 2) * ((10 + 2) - (4 - 2))
	do output with n
	do quit
