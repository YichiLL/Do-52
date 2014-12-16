setup:
	new Number chips : 10
	new String loseMessage : "You lost!"

round:
	do print with chips
	do output with loseMessage
	do quit

print with Number num:
	do output with num
