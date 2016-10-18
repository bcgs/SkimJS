// 1. IF-ELSE

// var x = 2;

// function mult2(y) {
//     return y*2;
// }
// if(x < 3) {
//     if(x == 2) return x;
// } else {
//     if(x == 3) mult2(x);
//     else mult2(x*x);
// }


// 2. WHILE & DO-WHILE

// var x = 0, y = 5, l = [1,1,1,1,1];
// while (x < y) {
//     do {
//         x++;
//     } while(x < 2);
//     l[y-1] = 0;
//     y--;
// }

// l;


// 3. FOR

// var count = 0, i;
// for(i = 0; i < 10; i++) {
//     count++;
// }

// count;


// 4. FOR-FOR

// var incr = 0;
// for (var i = 0; i < 2; i++) {
//     for(var j = 0; j < 2; j++) {
//         incr--;
//     }
//     incr++;
// }

// incr;


// 5. RECURSIVE FUNCTION

// var fat = 1;

// function factorial(i) {
//     if(i == 2) return 2;
//     return i * factorial(i-1);
// }

// factorial(6);


// 6. LEN (Javascript)

// function len(l) {
//     if(l == []) {
//         return 0;
//     } else {
//         return 1 + (len(l.tail()))
//     }
// }

// var i = [1,2,3,4,5];
// len(i);


// 7. HEAD, TAIL, CONCAT

// var i = [1,2,3], j = [4,5];
// var k = [(i.tail()).head()].concat(j);

// len(k);


// 8. ARRAY GET

// var i = [1,2,3,4,5,6,7,8,9,10];
// i[6];


// 9. ARRAY SET

// var j = [0,0,0,0,0];
// j[4] = 1;


// 10. Mergesort (NOT WORKING)

// var left = [], right = [];

// function getLeft(a) {
//     if(len(a) == 1) return a;

//     var mid = len(a)/2;
//     for (var i = 0; i < mid; i++) {
//      left[i] = a[i];
//     }
//     return left;
// }
// function getRight(b) {
//     if(len(b) == 1) return b;

//     var mid = len(b)/2;
//     for (var i = mid; i < len(b); i++) {
//      right[(i-mid)] = b[i];
//     }
//     return right;
// }

// function mergesort(c) {
//     if(len(c) == 1) {
//         return c;
//     } else {
//         sorting(mergesort(getLeft(c)), mergesort(getRight(c)));
//     }
// }

// function sorting(d,e) {
//     if(len(d)==0 && len(e)!=0) return e;
//     if(len(d)!=0 && len(e)==0) return d;
//     if(len(d)!=0 && len(e)!=0) {
//         if(d.head() < e.head()) {
//             return [d.head()].concat(sorting(d.tail(),e));
//         } else {
//             return [e.head()].concat(sorting(d,e.tail()));
//         }
//     } else {
//         return;
//     }
// }

// // var i = [5,1,4,2,6,3];
// // mergesort(i);

// var i = [5,1], j = [2];
// sorting(i,j);
// // i.fst().concat(j);
