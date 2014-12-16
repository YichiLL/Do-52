setup: 
	deck t> player1.hand
	deck t> player2.hand
	if player1.hand.top < player2.hand.top:
		do output with "P1 wins!"
	else:
		do output with "P2 wins!"

round:
	do quit
