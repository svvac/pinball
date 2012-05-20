unit umovingobject;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics, eventhandler, utils, signal, uobject, upoint, uvector, ushape, ugamesignals, objectcollection, BGRABitmap, drawspeed;

const GEE = +1;
      MAX_REVERTS = 3;


type aMovingObject = class(aObject)
    protected
        _speed: oVector;
        _oldpos: oPoint;
        _succ_reverts: integer;

        _speeddrawer: Test_SpeedDrawer;

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
    _succ_reverts := 0;

    s := TickSignal.create(self.getDispatcher());
    self.getDispatcher().bind(s, @self.onTick);
    s.free();

    _speeddrawer := Test_SpeedDrawer.create(_dispatcher);

end;


procedure aMovingObject.onCollision(s: oSignal);
begin

end;

procedure aMovingObject.setSafePosition();
begin
    _oldpos.free();
    _oldpos := oPoint.clone(_position);
    d(7, _id, 'Position ' + s(_oldpos) + ' tagged safe');
    _succ_reverts := 0;
end;

procedure aMovingObject.revertToSafePosition();
begin
    if _succ_reverts <= MAX_REVERTS then begin
        d(7, _id, 'Reverting to last safe position. ' + s(_position) + s('->') + s(_oldpos));
        _position.free();
        _position := oPoint.clone(_oldpos);
        _succ_reverts += 1;
    end else begin
        d(7, _id, 'NOT reverting to last safe position : too much reverts.');
        //_speed.factor(-1);  // TODO: This is a temporary fix
    end;
end;

// elementaryMove(zone: oObjectCollection)
// Performs an elementary move of the object, triggering collisions and so.
// Note that this method WON'T bother looking wether or not it should move, nor will
// manage to discretize the path.
procedure aMovingObject.elementaryMove(zone: oObjectCollection);
var ev: oVector;
    i: integer;
    p: oPoint;
    colliding: boolean;
    t: Test_SpeedDrawer;
begin
    // We create an elementary vector based on current speed indications
    ev := oVector.createPolar(1, _speed.getArgument());
    // Move the object accordingly
    _position.apply(ev);
    p := oPoint.create(0, 0);
    colliding := false;


    d(4, _id, 'Found ' + s(zone.count()) + ' objects to check for collision');

    // Check for collision with objects in the zone, and triggers collision signals if needed
    for i := 0 to zone.count() - 1 do begin
        d(5, _id, '       * ' + zone.get(i).getId(), '');
        if self.isColliding(zone.get(i), p) then begin
            d(5, '', ' [COLLISION]');
            getDispatcher().emit(zone.get(i).collisionSignalFactory(self, p));

            colliding := not zone.get(i).isCollisionSafe();
            if colliding then d(5, _id, 'Collision was safe');
        end else d(5, '', ' [PASS]');
    end;

    if not colliding then setSafePosition() else revertToSafePosition();

    p := oPoint.clone(_position);
    p.apply(oVector.createCartesian(7, 7));
    ev := getSpeed();
    ev.factor(10);

    _dispatcher.emit(_speeddrawer.signalFactory(self, ev, p));

end;

procedure aMovingObject.onTick(s: oSignal);
begin
    // Gravity :
    d(7, _id, 'Hello, this is gravity');
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
    d(12, _id, 'Speed updated. ' + s(old) + s('->') + s(_speed));
    old.free();
end;


end.
