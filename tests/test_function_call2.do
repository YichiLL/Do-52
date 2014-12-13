configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	do bar with 4 and (5 + 3) * 2

bar with Number x and Number y:
	do output with (x + y)