unit objectcollection;

interface

{$mode objfpc}{$H+}{$M+}
{$UNITPATH .}

uses Classes, uobject, sysutils;

const MAXOBJ = 128;

type
oObjectCollection = class(tObject)
    protected
        _objects: array [0 .. MAXOBJ - 1] of aObject;
        _count: integer;
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
    _count := 0;
end;

// push(c: aObject)
// Adds the object `c' to the collection
procedure oObjectCollection.push(c: aObject);
begin
    if count() >= MAXOBJ then
        raise Exception.create('No more slots available');
    
    _objects[_count] := c;
    _count += 1;
end;

// aObject(i: integer)
// returns the object identified by index `i'
function oObjectCollection.get(i: integer) : aObject;
begin
    if i > count() then raise Exception.create('No object indexed ' + intToStr(i));

    get := _objects[i];
end;

// integer count(void)
// Returns the object count for the collection
function oObjectCollection.count(): integer;
begin
    count := _count;
end;


end.