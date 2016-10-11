function mult2(a) {
	return a*2;
}
var fat = 1;
function factorial(i) {
	if(i == 2) return 2;
	return i * factorial(i-1);
}
function test(i) {
	if(i >= 5) {
		factorial(i);
		break;
	} else {
		mult2(i);
	}
	return i++;
}
test(5);