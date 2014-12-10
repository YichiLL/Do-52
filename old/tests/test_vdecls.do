configure players : 2
configure timeLimit : 256

new Number chips : 10
new String loseMessage : "You lost!"

round:
	do print with chips
	do output with loseMessage

print with Number num:
	do output with num
