
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

/*
  not a stable sort
  O(n2)
*/

function selectionSort(arr){

    let a = arr.slice(0); //clone
    let n = a.length-1;
    
    for(let i=0; i <= n; i++){
	let x = a[i];
	let k = i;

	for(let j=i+1; j <= n; j++){
	    if(a[j].key < x.key){
		k = j;
		x = a[j];
	    };
	};

	a[k] = a[i];
	a[i] = x;
    };
    return a;
}


var shuffled = fisherYatesShuffle(a);
console.log(shuffled);

var sorted = selectionSort(shuffled.slice(0));
console.log(sorted);

