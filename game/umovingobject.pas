unit umovingobject;

{$mode objfpc}{$H+}

interface

uses Classes, Graphics, eventhandler, signal, uobject, upoint, uvector, ushape, ugamesignals, uplayground, objectcollection;

const GEE = -9.8;


type aMovingObject = class(aObject)
    protected
        _speed: oVector;

    public
        constructor create(position: oPoint; mask: oShape; face: TBitmap; dispatcher: oEventHandler); virtual;

        procedure onCollision(s: oSignal); override;

        procedure move(playground: oPlayground); virtual;

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

procedure aMovingObject.move(playground: oPlayground);
var v: oVector;
    i, j: integer;
    l: oObjectCollection;
    s: oSignal;
    p: oPoint;

    col: boolean;
begin
    v := oVector.clone(_speed);
    col := false;
    p := oPoint.create(0, 0);
    // Discretization of path (we don't want to pass through walls, don't we ?)
    // BUT: this implementation should cause a slowdown around objects. The path traveled 
    //      won't be equal between two given instants cause of the changes in the speed vector.
    //      In this algorithm, we stop moving the ball as soon as we encountered an object
    //      (i.e. change in speed)
    for i := 1 to round(_speed.getModule()) do begin
        if col then break;
        v.setModule(i);
        _position.setXY(_position.getX() + v.getX(), _position.getY() + v.getY());

        l := playground.getObjectsInZone(_position, self.getMask().getWidth(), self.getMask().getHeight());
        for j := 0 to l.count() - 1 do begin
            if isColliding(l.get(i), p) then begin
                col := true;
                s := l.get(i).collisionSignalFactory(self, p);
                getDispatcher().emit(s);

                // Should we break() here? Yay, I'm asking ya
            end;
        end;


        l.free();
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


end.
