// war in do
configure playerCount: 2 
configure acesHigh: false

new Number warCount: 0

Player has Set called table 	// makes player_table available

setup:
	// deal cards
	{ player1_hand << deck_top } * (deck_size / 2) 	// loop
	{ player2_hand << deck_top } * (deck_size / 2)	// loop


round:
	do turn with player1
	do turn with player2
	do output with "Player 1 played: " + player1_table_top 
	do output with "Player 2 played: " + player2_table_top 
	do evaluate
	
turn with Player player:
	do output with player + "'s turn."
	if (player_hand_size = 0)
		if (player = player1) 
			do output with player + " has lost. Player2 wins!"
			do quit 
		else
			do output with player + " has lost. Player1 wins!"
			do quit

	do output with "Play card?" 
	do input with new String in 
	if (in = "y")
		player_hand_top >> player_table
	else
		do output with player + " has decided not to play anymore."
		do quit
		
evaluate:
	if (player1_table_top > player2_table_top)
		do output with "Player 1's card is higher."
		{ player1_hand << player1_table_top } * player1_table_size
		{ player1_hand << player2_table_top } * player2_table_size
	else if (player1_table_top < player2_table_top )
		do output with "Player 2's card is higher."
		{ player2_hand << player1_table_top } * player1_table_size
		{ player2_hand << player2_table_top } * player2_table_size
	else
		do output with "It's a tie. That means WAR!"
		warCount: warCount + 1
		
		// if a set runs out of cards >> and << don't do anything
		{ player1_hand_top >> player1_table } * 4
		{ player2_hand_top >> player2_table } * 4
		do output with "Player 1 and 2 put down 4 cards."
		
		// recursive call
		do evaluate
		