configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	if 7 < 6:
		if 5 > 4:
			do foo
		else:
			do bar
	else:
		do baz
	do quit

baz:
	do output with "Baz"
