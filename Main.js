var i = [1,2,3,4,5];
len(i);
--
var i = [1,2,3];
var j = [4,5];
var k = ((i.tail()).head()).concat(j);

function len(l) {
	if(l == []) {
		return 0;
	} else {
		return 1 + (len(l.tail()))
	}
}

len(k);