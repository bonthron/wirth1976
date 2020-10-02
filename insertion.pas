program wirth;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  TypeUtils,
  Classes { you can add units after this };

type
  item = record
    key: integer;
    Value: string;
  end;

type
  index = integer;


var
  r1: item = (key: 1; Value: 'A');
var
  r2: item = (key: 2; Value: 'B');
var
  r3: item = (key: 3; Value: 'C');
var
  r4: item = (key: 4; Value: 'D');

var
  a: array [0..3] of item;

  procedure straightinsertion;
  var
    i, j: index;
  var
    x: item;

  begin
    for i := 1 to 3 do
    begin
      x := a[i];
      j := i - 1;
      while ((j >= 0) and (a[j].key > x.key)) do
      begin
        a[j + 1] := a[j];
        j := j - 1;
      end;
      a[j + 1] := x;
    end;
  end;


begin
  a[0] := r4;
  a[1] := r3;
  a[2] := r2;
  a[3] := r1;

  Writeln(ToStr(a, TypeInfo(a)));

  straightinsertion();

  Writeln(ToStr(a, TypeInfo(a)));

end.
