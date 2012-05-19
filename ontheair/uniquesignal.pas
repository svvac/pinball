unit uniquesignal;

{$mode objfpc}{$H+}

interface

uses Classes, signal;

type

oUniqueSignal = class(oSignal)
    protected _id: string;
    public
        constructor create(sender: TObject; id: string);
        function getName() : string; override;
end;


implementation

constructor oUniqueSignal.create(sender: TObject; id: string);
begin
    _id := self.ClassName + '/' + id;
    inherited create(sender);
end;

function oUniqueSignal.getName() : string;
begin
    getName := _id;
end;


end.
