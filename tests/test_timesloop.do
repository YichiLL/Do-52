configure playerCount: 1
configure acesHigh: false

setup:
	new Number x : 0
	{ x : x + 1 } * 20
	do output with x
