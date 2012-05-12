unit objectcollection;

interface

{$mode objfpc}{$H+}
{$UNITPATH .}

uses Classes, uobject, sysutils;

const MAXOBJ = 128;

type
tObjectCollection = array [0 .. MAXOBJ - 1] of aObject;
oObjectCollection = class(tObject)
    protected
        o: tObjectCollection;
        n: integer;
    public
        constructor create();
        //destructor destroy(); override;
        procedure push(c: aObject);
        function get(i: integer): aObject;
        function count(): integer;
end;


implementation

// create(void)
// Creates a collection to store objects
constructor oObjectCollection.create();
begin
    n := 0;
end;

// push(c: aObject)
// Adds the object `c' to the collection
procedure oObjectCollection.push(c: aObject);
begin
    if (n + 1) >= MAXOBJ then
        raise Exception.create('No more slots available');
    
    o[n] := c;
    n := n + 1;
end;

// aObject(i: integer)
// returns the object identified by index `i'
function oObjectCollection.get(i: integer) : aObject;
begin
    if i > n then raise Exception.create('No object indexed ' + intToStr(i));

    get := o[i];
end;

// integer count(void)
// Returns the object count for the collection
function oObjectCollection.count(): integer;
begin
    count := n;
end;


end.