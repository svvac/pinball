unit AbstractCollection;

interface

uses classes, sysutils;

const COLLECTION_MAX_SIZE = 512;

type TAbstractCollection = class(TObject)
    protected 
        _keys   = array [0 .. COLLECTION_MAX_SIZE - 1] of string;
        {_values = array [0 .. COLLECTION_MAX_SIZE - 1] of tObject;}
        _nextk  = integer;
        
        function lookfor(k: string) : integer;

    public
        constructor create(); virtual;
        destructor destroy(); virtual; override;

        function len() : integer;
        function isKey(k: string) : boolean;
        
end;

KeyNotFoundException = Class(Exception);
KeyOverrideException = Class(Exception);


implementation

constructor AbstractCollection.create();
begin
    _count := 0;
end;

destructor AbstractCollection.destroy();
begin
    inherited;
end;

{
  lookfor -- returns the numeric index associated to a
             string key. Returns -1 if not found.
}
function AbstractCollection.lookfor(k: key) : integer;
var i: integer;
begin
    lookfor := -1;
    for i := 0 to len() do
        if _keys[i] = k then begin
            lookfor := i;
            break;
        end;
end;


{
  len -- returns the number of objects stored
}
function AbstractCollection.len() : integer;
begin
    len := _nextk;
end;


{
  isKey -- returns true if an object is stored with given
           string key
}
function AbstractCollection.isKey(k: string) : boolean;
begin
    isKey := lookfor(k) = -1;
end;









end.
