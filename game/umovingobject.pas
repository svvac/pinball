unit umovingobject;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics, eventhandler, signal, uobject, upoint, uvector, ushape, ugamesignals, objectcollection, BGRABitmap;

const GEE = +9.8;


type aMovingObject = class(aObject)
    protected
        _speed: oVector;
        _oldpos: oPoint;

    public
        constructor create(position: oPoint; mask: oShape; face: TBGRABitmap; dispatcher: oEventHandler); virtual;

        procedure onCollision(s: oSignal); override;

        procedure elementaryMove(zone: oObjectCollection); virtual;

        procedure setSafePosition();
        procedure revertToSafePosition();

        procedure onTick(s: oSignal); virtual;

        function getSpeed() : oVector;
        procedure setSpeed(v: oVector);

end;

implementation

constructor aMovingObject.create(position: oPoint; mask: oShape; face: TBGRABitmap; dispatcher: oEventHandler);
var s: oSignal;
begin
    _speed := oVector.createCartesian(0, 0);
    inherited create(position, mask, face, dispatcher);
    _oldpos := oPoint.clone(_position);

    //s := TickSignal.create(self.getDispatcher());
    //self.getDispatcher().bind(s, @self.onTick);
end;


procedure aMovingObject.onCollision(s: oSignal);
begin

end;

procedure aMovingObject.setSafePosition();
begin
    _oldpos.free();
    _oldpos := oPoint.clone(_position);
end;

procedure aMovingObject.revertToSafePosition();
begin
    _position.free();
    _position := oPoint.clone(_oldpos);
end;

// elementaryMove(zone: oObjectCollection)
// Performs an elementary move of the object, triggering collisions and so.
// Note that this method WON'T bother looking wether or not it should move, nor will
// manage to discretize the path.
procedure aMovingObject.elementaryMove(zone: oObjectCollection);
var ev: oVector;
    i: integer;
    p: oPoint;
begin
    // We create an elementary vector based on current speed indications
    ev := oVector.createPolar(1, _speed.getArgument());
    // Move the object accordingly
    _position.apply(ev);
    p := oPoint.create(0, 0);

    writeln(_id + ': Found ' + IntToStr(zone.count()) + ' objects to check for collision');

    // Check for collision with objects in the zone, and triggers collision signals if needed
    for i := 0 to zone.count() - 1 do begin
        write(_id + ':       * ' + zone.get(i).getId());
        if self.isColliding(zone.get(i), p) then begin
            writeln(' [COLLISION]');
            getDispatcher().emit(zone.get(i).collisionSignalFactory(self, p));
            revertToSafePosition();
        end else begin
            writeln(' [PASS]');
            setSafePosition();
        end;
    end;

end;

procedure aMovingObject.onTick(s: oSignal);
begin
    // Gravity :
    _speed.setY(round(_speed.getY() + GEE));
end;



function aMovingObject.getSpeed() : oVector;
begin
    getSpeed := oVector.clone(_speed);
end;

procedure aMovingObject.setSpeed(v: oVector);
var old: oVector;
begin
    old := _speed;
    _speed := oVector.clone(v);
    old.free();
end;


end.
