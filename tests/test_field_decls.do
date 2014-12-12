configure playerCount: 1
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)
	
new Number turn : 0

round:
	do increment with turn
	do output with turn

increment with Number num:
	num : num + 1
