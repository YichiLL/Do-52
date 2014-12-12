4 < 5
4 > 5
4 <= 5
4 >= 5
4 != 5
5 = 5
// this should fail to compile (what does bool < int mean?):
// 5 < 6 < 7
// but this is okay (should group from left and evaluate to true):
5 != 6 = 7
