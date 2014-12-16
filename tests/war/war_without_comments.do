configure numberOfPlayers: 2 
configure highestCard: Ace
configure ascend: true

Player has Set called table

new Number warCount: 0

setup:
	new Number deckSize : deck.size
	{ player1.hand <t deck } * (deckSize / 2)
	{ player2.hand <t deck } * (deckSize / 2)

round:
	do turn with player1
	do turn with player2
	do output with "Player 1 played: " + player1.table.top.desc
	do output with "Player 2 played: " + player2.table.top.desc
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
	new Boolean done : false

	while !done:
		if player1.table.top > player2.table.top:
			do output with "Player 1's card is higher."
			{ player1.hand <t player1.table } * player1.table.size
			{ player1.hand <t player2.table } * player2.table.size
			done : true
		else:
			if player1.table.top < player2.table.top:
				do output with "Player 2's card is higher."
				{ player2.hand <t player1.table } * player1.table.size
				{ player2.hand <t player2.table } * player2.table.size
				done : true
			else:
				do output with "It's a tie. That means WAR!"
				warCount: warCount + 1
				
				{ player1.hand t> player1.table } * 4
				{ player2.hand t> player2.table } * 4
				do output with "Player 1 and 2 put down 4 cards."
