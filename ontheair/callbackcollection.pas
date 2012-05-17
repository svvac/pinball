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

// create(void)
// Creates a collection to store callback methods
constructor oCallbackCollection.create();
begin
    n := 0;
end;

// push(c: tSignalCallback)
// Adds the callback `c' to the collection
procedure oCallbackCollection.push(c: tSignalCallback);
begin
    if n >= MAXBOUND then
        raise Exception.create('No more slots available');
    
    o[n] := c;
    n := n + 1;
end;

// tSignalCallback(i: integer)
// returns the callback identified by index `i'
function oCallbackCollection.get(i: integer) : tSignalCallback;
begin
    if i > n then raise Exception.create('No object indexed ' + intToStr(i));

    get := o[i];
end;

// integer count(void)
// Returns the object count for the collection
function oCallbackCollection.count(): integer;
begin
    count := n;
end;


end.