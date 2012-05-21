unit signal;

interface

{$mode objfpc}{$H+}
{$UNITPATH .}

uses
    // home-baked units
    printable, utils,
    // stdlib
    Classes
    ;

type

// Core implementation ; shouldn't be used
_oSignal = class
    // Core imlplementation of the class
    protected _sender: TObject;
    public
        constructor create(sender: tObject); virtual;

        function getName() : string; virtual;
        function getSender() : tObject; virtual;
end;

// Vendor class, implementing the Printable interface
oSignal = class(_oSignal, iPrintable)
    function toString() : string; virtual;
end;

implementation

// create(sender: tObject)
// Creates a signal with `sender' defined as the sender object.
constructor _oSignal.create(sender: tObject);
begin
    _sender := sender;
end;

// string getName(void)
// Returns the signal's name, used as ID (we use the class name)
function _oSignal.getName() : string;
begin
    getName := self.ClassName;
end;

// tObject getSender(void)
// Returns the object defined as emitter of the signal
function _oSignal.getSender() : tObject;
begin
    getSender := _sender;
end;

// string toString()
// returns a string representation of the signal
function oSignal.toString() : string;
begin
    toString := getName() + ': Emited';
end;


end.