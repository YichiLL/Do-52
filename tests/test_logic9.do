configure playerCount: 1
configure acesHigh: false

setup:
	new Boolean b : false & true | true & !false & true
	do output with b