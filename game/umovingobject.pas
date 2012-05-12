unit umovingobject;

{$mode objfpc}{$H+}

interface

uses Classes, Graphics, eventhandler, signal, uobject, uvector, upoint, ushape, ugamesignals;


type aMovingObject = class(aObject)
    protected
        _speed: oVector;

    public
        constructor create(position: oPoint; mask: oShape; face: TBitmap; dispatcher: oEventHandler); virtual;

        procedure onCollision(s: oSignal); override;

        function getSpeed() : oVector;

end;

implementation

constructor aMovingObject.create(position: oPoint; mask: oShape; face: TBitmap; dispatcher: oEventHandler);
begin
    _speed := oVector.createCartesian(0, 0);
    inherited create(position, mask, face, dispatcher);
end;


procedure aMovingObject.onCollision(s: oSignal);
begin

end;



function aMovingObject.getSpeed() : ovector;
begin
    getSpeed := oVector.clone(_speed);
end;


end.
