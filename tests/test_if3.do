configure playerCount: 1
configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	new Boolean f : false
	if (f):
		do output with false
	else if (!f):
		do output with true
	else:
		do output with false
