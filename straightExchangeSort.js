/*
  Straight Exchange Sort
  a.k.a. Bubblesort 

If we view the array to be in a vertical instead of a horizontal position, and the items as bubbles in a water tank with "weights" according to their keys, then each pass over the array results in the ascention of a bubble to its appropriate level of weight.

Exchange sort is inferior to both straight insertion and straight selection; in fact, the bubblesort has hardly anything to recommend it except its catchy name!

Unlike modern versions, Wirth loops from right to left; I assume to fit the analogy of bubbles rising.

O(n^2) 
worst case: O(n^2) swaps 
best case: O(1) swaps 
*/



/*
  The Fisher-Yates shuffle, in its original form, was described in 1938 by Ronald Fisher and Frank Yates
  in their book Statistical tables for biological, agricultural and medical research.
  The modern version of the Fisher-Yates shuffle, designed for computer use, was introduced by Richard Durstenfeld
  in 1964 and popularized by Donald E. Knuth in The Art of Computer Programming.
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


/*
  straight exchange, aka bubblesort
  O(n2)
  Exchange sort is inferior to both straight insertion and straight selection;
  in fact, the bubblesort has hardly anything to recommend it except its catchy name!
*/

function exchangeSort(arr){

    let a = arr.slice(0); //clone
    let n = a.length-1;
    
    for(let i=1; i <= n; i++){
	for(let j=n; j > 0; j--){
	    if(a[j-1].key > a[j].key){
		let x = a[j];

		a[j] = a[j-1];
		a[j-1] = x;
	    };
	};
    };
    return a;
}


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

var shuffled = fisherYatesShuffle(a);
console.log(shuffled);

var sorted = exchangeSort(shuffled.slice(0));
console.log(sorted);

