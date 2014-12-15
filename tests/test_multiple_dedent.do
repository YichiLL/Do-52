configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

round:
	new Boolean isCar : true
	new Boolean fits : true
	if isCat:
		if fits:
			do output with true
	else:
		do output with false
	do quit
