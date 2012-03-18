unit callbackCollection;

interface

{$mode objfpc}{$H+}
{$UNITPATH .}

uses Classes, signal, sysutils;

const MAXBOUND = 128;

type
tSignalCallback = procedure(x: oSignal) of object;
tCallbackCollection = array [0 .. MAXBOUND - 1] of tSignalCallback;
oCallbackCollection = class(tObject)
    protected
        o: tCallbackCollection;
        n: integer;
    public
        constructor create();
        //destructor destroy(); override;
        procedure push(c: tSignalCallback);
        function get(i: integer): tSignalCallback;
        function count(): integer;
end;


implementation

constructor oCallbackCollection.create();
begin
	n := 0;
end;

procedure oCallbackCollection.push(c: tSignalCallback);
begin
    if (n + 1) >= MAXBOUND then
        raise Exception.create('No more slots available');
    
    o[n] := c;
    n := n + 1;
end;

function oCallbackCollection.get(i: integer) : tSignalCallback;
begin
    if i > n then raise Exception.create('No object indexed ' + intToStr(i));

    get := o[i];
end;

function oCallbackCollection.count(): integer;
begin
    count := n;
end;


end.