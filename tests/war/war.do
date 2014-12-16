// war in do
configure numberOfPlayers: 2 
configure highestCard: ace
configure ascendingOrder: true

Player has Set called table // makes player.table available

new Number warCount: 0

setup:
	// deal cards
	new Number deckSize : deck.size
	{ player1.hand <t deck } * (deckSize / 2) 	// loop
	{ player2.hand <t deck } * (deckSize / 2)	// loop

round:
	do turn with player1
	do turn with player2
	do output with "Player 1 played: " + player1.table.top.desc
	do output with "Player 2 played: " + player2.table.top.desc
	do evaluate
	
turn with Player player:
	do output with player.desc + "'s turn."
	if player.hand.size = 0:
		if player = player1:
			do output with player.desc + " has lost. Player2 wins!"
			do quit 
		else:
			do output with player.desc + " has lost. Player1 wins!"
			do quit

	do output with "Play card?" 

	new String in : ""
	do input with in
	if in = "y":
		player.hand t> player.table
	else:
		do output with player.desc + " has decided not to play anymore."
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
				
				// if a set runs out of cards >> and << don't do anything
				{ player1.hand t> player1.table } * 4
				{ player2.hand t> player2.table } * 4
				do output with "Player 1 and 2 put down 4 cards."
