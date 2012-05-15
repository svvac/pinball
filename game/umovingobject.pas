unit umovingobject;

{$mode objfpc}{$H+}

interface

uses Classes, Graphics, eventhandler, signal, uobject, upoint, uvector, ushape, ugamesignals, objectcollection;

const GEE = -9.8;


type aMovingObject = class(aObject)
    protected
        _speed: oVector;

    public
        constructor create(position: oPoint; mask: oShape; face: TBitmap; dispatcher: oEventHandler); virtual;

        procedure onCollision(s: oSignal); override;

        procedure elementaryMove(zone: oPlayground); virtual;

        procedure onTick(s: oSignal); virtual;

        function getSpeed() : oVector;

end;

implementation

constructor aMovingObject.create(position: oPoint; mask: oShape; face: TBitmap; dispatcher: oEventHandler);
var s: oSignal;
begin
    _speed := oVector.createCartesian(0, 0);
    inherited create(position, mask, face, dispatcher);

    s := TickSignal.create(self.getDispatcher());
    self.getDispatcher().bind(s, @self.onTick);
end;


procedure aMovingObject.onCollision(s: oSignal);
begin

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
    p.create(0, 0);

    // Check for collision with objects in the zone, and triggers collision signals if needed
    for i := 0 to zone.count() - 1 do
        if self.isColliding(zone.get(i), p) then
            getDispatcher().emit(zone.get(i).collisionSignalFactory(self, p));

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


end.
