configure playerCount: 2 
configure acesHigh: false

Player has Set called table

new Number warCount: 0

setup:
	{ player1.hand <t deck } * (deck.size / 2)
	{ player2.hand <t deck } * (deck.size / 2)

round:
	do turn with player1
	do turn with player2
	do output with "Player 1 played: " + player1.table 
	do output with "Player 2 played: " + player2.table 
	do evaluate
	
turn with Player player:
	do output with player + "'s turn."
	if player.hand.size = 0:
		if player = player1:
			do output with player + " has lost. Player2 wins!"
			do quit 
		else:
			do output with player + " has lost. Player1 wins!"
			do quit

	do output with "Play card?" 

	new String in : ""
	do input with in
	if in = "y":
		player.hand t> player.table
	else:
		do output with player + " has decided not to play anymore."
		do quit
		
evaluate:
	if player1.table > player2.table:
		do output with "Player 1's card is higher."
		{ player1.hand <t player1.table } * player1.table.size
		{ player1.hand <t player2.table } * player2.table.size
	else:
		if player1.table < player2.table:
			do output with "Player 2's card is higher."
			{ player2.hand <t player1.table } * player1.table.size
			{ player2.hand <t player2.table } * player2.table.size
		else:
			do output with "It's a tie. That means WAR!"
			warCount: warCount + 1
			
			{ player1.hand t> player1.table } * 4
			{ player2.hand t> player2.table } * 4
			do output with "Player 1 and 2 put down 4 cards."
			
			do evaluate
