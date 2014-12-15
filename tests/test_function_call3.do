
configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	do baz with 4 and 2 and 6
	do quit

baz with Number x and Number y and Number z:
	do output with (x + y + z)
