unit signal;

interface

{$mode objfpc}{$H+}
{$UNITPATH .}

uses Classes, utils, printable;

type
_oSignal = class
    //protected pSignalName: string;
    protected _sender: TObject;
    public
        constructor create(sender: tObject); virtual;

        function getName() : string; virtual;
        function getSender() : tObject; virtual;
end;

oSignal = class(_oSignal, iPrintable)
    function toString() : string; virtual;
end;

implementation

// create(sender: tObject)
// Creates a signal with `sender' defined as the sender object.
constructor _oSignal.create(sender: tObject);
begin
    //inherited create(false);
    //pSignalName := signame;
    //setValue('sender', sender);
    _sender := sender;
end;

// string getName(void)
// Returns the signal's name, used as ID (we use the class name)
function _oSignal.getName() : string;
begin
    //getName := pSignalName;
    getName := self.ClassName;
end;

// tObject getSender(void)
// Returns the object defined as emitter of the signal
function _oSignal.getSender() : tObject;
begin
    getSender := _sender; //getValue('sender');
end;

function oSignal.toString() : string;
begin
    toString := getName() + ': Emited';
end;


end.