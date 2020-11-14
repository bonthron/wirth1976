/*
Insertion Sort

This method is widely used by card players. It is a stable sort, it leaves the order of items with equal keys unchanged.

The number of key comparisons in the i-th sift is at most i - 1, at least 1, and -assuming that all permutations of the n keys are equally probable- 1/2 in average. 
C_min = n - 1 
C_max = 1/2(n^2 + n) -1 

The least numbers occur if the items are originally in order; the worst case occurs if the items are originally in reverse order.

O(n^2) 

Divergence from Wirth:
Because there are two termination conditions, Wirth uses the "sentinel technique" of setting the current item in the iteration as a terminating value at index zero. This increases the index range by one. This seems unnecessarily complicated than simply checking both termination conditions and introduces a duplicate value in the data, which, itself could lead to subsequent errors.
*/


/*
  ------------------------------------------------------------------------ fisherYatesShuffle

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
  ------------------------------------------------------------------------ insertionSort
  stable sort
  O(n2)
*/

function insertionSort(arr){

    for(var i=0; i < arr.length; i++){

        let x = a[i]
        let j = i - 1;

        while((j >= 0) && (arr[j].key > x.key)){
            
            arr[j + 1] = arr[j];
            j = j - 1;
        };
        
        arr[j + 1] = x;
    };

    return arr;
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

var sorted = insertionSort(shuffled.slice(0));
console.log(sorted);

