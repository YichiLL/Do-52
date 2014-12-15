setup:
	new Number turn : 0

round:
	do increment with turn
	do quit
	
increment with Number num:
	num : num + 1
	do output with num
