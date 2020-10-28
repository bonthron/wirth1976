program straightExchange;

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


  { ---------------------------------------------------- straightExchangeSort }
  { aka bubblesort
  Exchange sort is inferior to both straight insertion and straight selection;
  in fact, the bubblesort has hardly anything to recommend it except its catchy name!
  }
  function straightExchangeSort(a: itemArray): itemArray;
  var
    i, j, n: index;
  var
    x: item;

  begin
    n := (length(a) - 1);
    begin
      for i := 1 to n do
      begin
        for j := n downto i do
          if a[j - 1].key > a[j].key then
          begin
            x := a[j - 1];
            a[j - 1] := a[j];
            a[j] := x;
          end;
      end;
      Result := a;
    end;
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

  b := straightExchangeSort(b); // bubblesort
  Writeln(ToStr(b, TypeInfo(b)));

end.
