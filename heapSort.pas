program Heapsort;

{*
Heapsort 

The method of sorting by straight selection is based on the repeated selection of the least key among n items. Straight selection can be improved by retaining from each scan more information than just the identification of the single least item. With n/2 comparisons it is possible to determine the smaller key of each pair of items, with another n/4 comparison the smaller of each pair can be selected, and so on; the second step now consists of descending down along the path marked by the least key and eliminating it. Each of the n selection steps requires only log n comparisons. Therefore, the selection process requires only on the order of n log n elementary operations in addition to the n steps required by the construction of the tree. This is a very significant improvement over the straight methods requiring n^2 steps.

The heap is defined as a binary tree that can be constructed in situ as an array. Heapsort is an in-place algorithm, but it is not a stable sort.

Although somewhat slower than quicksort, it has the advantage of a more favorable worst-case runtime. Heapsort was invented by J. W. J. Williams in 1964. In the same year, R. W. Floyd published an improved version that could sort the array in-place. 

O(n log n) 
*}


{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  TypeUtils,
  Classes;

type
  item = record
    key: integer;
    Value: string;
  end;

type
  itemArray = array of item;

type
  index = integer;


{* ------------------------------------------------------------------ shuffle
   The Fisher-Yates shuffle, in its original form, was described in 1938
   by Ronald Fisher and Frank Yates in their book Statistical tables for
   biological, agricultural and medical research.
   The modern version of the Fisher-Yates shuffle, designed for computer use,
   was introduced by Richard Durstenfeld in 1964 and popularized by
   Donald E. Knuth in The Art of Computer Programming.
   O(n)
*}
  function shuffle(arr: itemArray): itemArray;
  var
    i, j: index;
    r: integer;
    a: item;
    b: item;

  begin
    for i := (length(arr) - 1) downto 1 do
    begin
      r := random(i);
      a := arr[i];
      b := arr[r];
      arr[i] := b;
      arr[r] := a;
    end;

    Result := arr;
  end;


  { ---------------------------------------------------- sift }
  function sift(a: itemArray; l: index; r: index): itemArray;

  label
    666;
  var
    i, j: index;
  var
    x: item;

  begin
    i := l;
    j := (2 * i) + 1;
    x := a[i];

    while (j <= r) do
    begin
      if (j < r) then
        if (a[j].key < a[j + 1].key) then
          j := j + 1;
      if (x.key >= a[j].key) then
        goto 666;
      a[i] := a[j];
      i := j;
      j := (2 * i) + 1;
    end;

    666:
      a[i] := x;

    Result := a;
  end;


  { ---------------------------------------------------- HeapSort }
  { O(n log n) }

  function HeapSort(a: itemArray): itemArray;

  var
    n: index;
  var
    l, r: index;
  var
    x: item;

  begin
    n := length(a);
    l := (n div 2);
    r := n - 1;

    while (l > 0) do
    begin
      l := l - 1;
      a := sift(a, l, r);
    end;

    while (r > 0) do
    begin
      x := a[0];
      a[0] := a[r];
      a[r] := x;
      r := r - 1;
      a := sift(a, 0, r);
    end;

    Result := a;
  end;


var
  a: array[0..9] of item = ((key: 0; Value: 'A'), (key: 1; Value: 'B'), (key: 2;
    Value: 'C'), (key: 3; Value: 'D'), (key: 4; Value: 'E'), (key: 5;
    Value: 'F'), (key: 6; Value: 'G'), (key: 7; Value: 'H'), (key: 8;
    Value: 'I'), (key: 9; Value: 'J'));

  b: itemArray;


begin

  b := shuffle(a);
  Writeln(ToStr(b, TypeInfo(b)));

  b := HeapSort(a);
  Writeln(ToStr(b, TypeInfo(b)));

end.
