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

constructor oSignal.create(sender: tObject);
begin
    //inherited create(false);
    //pSignalName := signame;
    //setValue('sender', sender);
    _sender := sender;
end;

function oSignal.getName() : string;
begin
    //getName := pSignalName;
    getName := self.ClassName;
end;

function oSignal.getSender() : tObject;
begin
    getSender := _sender; //getValue('sender');
end;


end.