configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	new Number x : 1
	{
	x : x + 1
	do output with x
	} until (x != 5)
	do quit
