// this is comment at the very beginning of the file
configure playerCount: 1
configure acesHigh: false

Player has Set called table // adding table set to players

setup:
	{ player1.hand <t deck } * (deck.size / 2)

// this comment is way over here

// this is the main method
round:
	// a comment
	do output with "Comment"
	
	// another comment
	do output with "Over"
	do quit
