unit uniquesignal;

{$mode objfpc}{$H+}

interface

uses
	// ontheair
	signal,
	// stdlib
	Classes
	;

type

oUniqueSignal = class(oSignal)
    protected _id: string;
    public
        constructor create(sender: tObject; id: string);
        function getName() : string; override;
end;


implementation

// create(sender: tObject; id: string)
// Basic definitions for a signal differentiated by a string ID, on the fly
constructor oUniqueSignal.create(sender: tObject; id: string);
begin
	// Normalize identifier
    _id := self.ClassName + '/' + id;
    inherited create(sender);
end;

// string getName()
// Name of the signal isn't the class name anymore, but the UID
function oUniqueSignal.getName() : string;
begin
    getName := _id;
end;


end.
