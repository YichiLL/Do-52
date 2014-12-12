configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	do output with (4 >= 5)
