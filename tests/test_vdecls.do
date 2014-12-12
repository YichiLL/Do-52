configure playerCount: 2
configure timeLimit: 256
configure acesHigh: false

Player has Set called table

setup:
	{ player1.hand <t deck } * (deck.size / 2)

new Number chips : 10
new String loseMessage : "You lost!"

round:
	do print with chips
	do output with loseMessage

print with Number num:
	do output with num
