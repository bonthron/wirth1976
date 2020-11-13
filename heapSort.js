
/*
  The Fisher-Yates shuffle, in its original form, was described in 1938 by Ronald Fisher and Frank Yates in their book Statistical tables for biological, agricultural and medical research.
  The modern version of the Fisher-Yates shuffle, designed for computer use, was introduced by Richard Durstenfeld in 1964 and popularized by Donald E. Knuth in The Art of Computer Programming.
  O(n)
*/
function fisherYatesShuffle(arr){

    for(var i = (arr.length - 1); i >= 1; i--){
        let rand = Math.floor(Math.random() * i);
        let a = arr[i];
        let b = arr[rand];
        arr[i] = b;
        arr[rand] = a;                 
    };
    
    return arr;
};


var a = [
    {key:0, value:"A"},
    {key:1, value:"B"},
    {key:2, value:"C"},
    {key:3, value:"D"},
    {key:4, value:"E"},
    {key:5, value:"F"},
    {key:6, value:"G"},
    {key:7, value:"H"},
    {key:8, value:"I"},
    {key:9, value:"J"}
];


// --------------------------------------------------------------------------- sift
//  sift, will "heapify" when called repeatedly

function sift(arr, L, R){

    let a = arr.slice(0); //clone
    let i = L;
    let j = (2 * i) + 1;
    let x = a[i];

    while (j <= R) {

	if (j < R) {
            if (a[j].key < a[j + 1].key){ j = j + 1; }; // pick the larger of the pair
	};
	
	if (a[i].key >= a[j].key){ return a; };

	a[i] = a[j]; // swap
	a[j] = x;	

	i = j;
	j = (2 * i) + 1;
    }

    return a;
}


// --------------------------------------------------------------------------- heapSort
/*  
    O(n log n)
    Space complexity: O(1) 
    This is not a stable sort.

    This is a faithful translation from Wirth's original Pascal. But..
    I think some better variable names and some helper functions could improve its readability.
*/

function heapSort(arr){

    let a = arr.slice(0); //clone
    let n = a.length;
    
    let L = Math.floor(n/2);
    let R = n - 1;

    while (L > 0){
      L = L - 1;
      a = sift(a, L, R);
    };

    while (R > 0) {
      x = a[0];
      a[0] = a[R];
      a[R] = x;
      R = R - 1;
      a = sift(a, 0, R);
    }
    
    return a;
}


var shuffled = fisherYatesShuffle(a);
console.log(shuffled);

var sorted = heapSort(shuffled);
console.log(sorted);




