program straightSelection;

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


  { ---------------------------------------------------- straightSelectionSort }
  function straightSelectionSort(a: itemArray): itemArray;
  var
    i, j, k, n: index;
  var
    x: item;

  begin
    n := (length(a) - 1);
    for i := 0 to n do
    begin
      x := a[i];
      k := i;
      for j := i + 1 to n do
        if a[j].key < x.key then
        begin
          k := j;
          x := a[j];
        end;
      a[k] := a[i];
      a[i] := x;
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

  b := straightSelectionSort(b);
  Writeln(ToStr(b, TypeInfo(b)));

end.
