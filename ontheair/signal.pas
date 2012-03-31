unit signal;

interface

{$mode objfpc}{$H+}
{$UNITPATH .}

uses Classes;

type
oSignal = class //(tStringHash)
    //protected pSignalName: string;
    protected _sender: TObject;
    public
        constructor create(sender: tObject);

        function getName() : string;
        function getSender() : tObject;
end;

implementation

// create(sender: tObject)
// Creates a signal with `sender' defined as the sender object.
constructor oSignal.create(sender: tObject);
begin
    //inherited create(false);
    //pSignalName := signame;
    //setValue('sender', sender);
    _sender := sender;
end;

// string getName(void)
// Returns the signal's name, used as ID (we use the class name)
function oSignal.getName() : string;
begin
    //getName := pSignalName;
    getName := self.ClassName;
end;

// tObject getSender(void)
// Returns the object defined as emitter of the signal
function oSignal.getSender() : tObject;
begin
    getSender := _sender; //getValue('sender');
end;


end.