configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	for new Number x: 0; x < 10; x: x + 1:
		if (x / 5 > 10):
			continue
		else:
			do output with x
