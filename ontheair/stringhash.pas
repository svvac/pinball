unit stringhash;

interface

{$mode objfpc}{$H+}{$M+}
{$UNITPATH .}

uses Classes, sysutils;

const STRINGHASH_MAXOBJ = 128;

type
oStringHash = class(tObject)
    protected
        _indexes: array[0 .. STRINGHASH_MAXOBJ - 1] of string;
        _objects: array[0 .. STRINGHASH_MAXOBJ - 1] of tObject;
        _n: integer;

        function indexFor(s: string) : integer;
        function exists(s: string; var i: integer) : boolean;
    public
        constructor create();
        //destructor destroy(); override;
        procedure setValue(i: string; c: tObject);
        function getValue(i: string) : tObject;
        function exists(i: string) : boolean;
        function count() : integer;
end;


implementation

// create(void)
// Creates a table to store objects identified by a string
constructor oStringHash.create();
begin
    _n := 0;
end;

// indexFor(s: string)
// Returns the numeric index associated with a string index
function oStringHash.indexFor(s: string) : integer;
var i: integer;
begin
    indexFor := -1;
    for i := 0 to count() - 1 do
        if _indexes[i] = s then begin
            indexFor := i;
            break;
        end;
end;

// exists(i: string)
// Returns true if a value is registered with key `i'
function oStringHash.exists(i: string) : boolean;
begin
    exists := indexFor(i) >= 0;
end;

// exists(s: string; var i: integer)
// Returns true if a value is registered with key `s', and stores the numeric ID in i
function oStringHash.exists(s: string; var i: integer) : boolean;
var idx: integer;
begin
    idx := indexFor(s);
    if idx >= 0 then i := idx;
    exists := idx >= 0;
end;

// setValue(c: tObject)
// Adds the object `c' to the table
procedure oStringHash.setValue(i: string; c: tObject);
var idx: integer;
begin
    if count() >= STRINGHASH_MAXOBJ then
        raise Exception.create('No more slots available');

    idx := count();

    if exists(i, idx) then begin end;
    
    _indexes[idx] := i;
    _objects[idx] := c;
    _n += 1;
end;

// getValue(i: string)
// returns the object identified by index `i'
function oStringHash.getValue(i: string) : tObject;
var idx: integer;
begin
    idx := -1;
    if not exists(i, idx) then raise Exception.create('No object indexed ' + i);

    getValue := _objects[idx];
end;

// integer count(void)
// Returns the object count for the table
function oStringHash.count(): integer;
begin
    count := _n;
end;


end.