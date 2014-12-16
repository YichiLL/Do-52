setup: 
	deck t> player1.hand
	deck t> player2.hand
	if player1.hand.top < player2.hand.top:
		do output with "Somebody won!"
	else:
		do output with "Somebody won!"

round:
	do quit
