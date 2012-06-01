unit umovingobject;

{$mode objfpc}{$H+}

interface

uses
    // ontheair
    eventhandler, signal,
    // home-baked units
    drawspeed, ugamesignals,
        // utils
        objectcollection, uobject, upoint, ushape, utils, uvector,
    // custom graphics library
    BGRABitmap,
    // stdlib
    Classes, SysUtils
    ;

const
    // Gravity
    GEE = +1;
    // Maximum successive position rollbacks
    MAX_REVERTS = 3;
    // Maximal speed
    MAX_SPEED = 100.0;
    // History size
    HISTORY_SIZE = 10;


type aMovingObject = class(aObject)
    protected
        _speed: oVector;
        _oldpos: array [0 .. HISTORY_SIZE - 1] of oPoint;
        _succ_reverts: integer;
        _revpow: integer;

        _speeddrawer: Test_SpeedDrawer;

        procedure tagSafePosition();
        procedure rollbackToSafePosition(sticky: boolean);
        procedure rollbackToSafePosition();
        procedure do_rollbackToSafePosition();

    public
        constructor create(position: oPoint; mask: oShape; face: TBGRABitmap;
                           dispatcher: oEventHandler); virtual;

        procedure onCollision(s: oSignal); override;

        procedure elementaryMove(zone: oObjectCollection); virtual;
        procedure elementaryMove(zone: oObjectCollection; norm: real); virtual;

        procedure gravity(s: oSignal); virtual;

        function getSpeed() : oVector;
        procedure setSpeed(v: oVector);
        procedure setPosition(p: oPoint);

end;

implementation

// create(position: oPoint; mask: oShape; face: TBGRABitmap;
//        dispatcher: oEventHandler)
// Well, create the shit
constructor aMovingObject.create(position: oPoint; mask: oShape;
                                 face: TBGRABitmap;
                                 dispatcher: oEventHandler);
var s: oSignal;
    i: integer;
begin
    _speed := oVector.createCartesian(0, 0);
    inherited create(position, mask, face, dispatcher);

    // Will stor last "safe" position to rollback when colliding
    for i := 0 to HISTORY_SIZE - 1 do _oldpos[i] := oPoint.clone(_position);
    _succ_reverts := 0;
    _revpow := 0;

    _sticky := false;

    // Apply gravity on each clock tick
    s := TickSignal.create(self.getDispatcher());
    self.getDispatcher().bind(s, @self.gravity);
    s.free();

    // Registers a speed drawer to debug speed vector
    _speeddrawer := Test_SpeedDrawer.create(_dispatcher);
end;

// onCollision(s: oSignal)
// Callback handling collisions
procedure aMovingObject.onCollision(s: oSignal);
begin
    // Nothing to do here ; collisions handled on other objects
    // Would only need it if it could collide with another moving object, on
    // the case of multiple balls for instance
end;

// tagSafePosition()
// Tags the current position as "safe" (i.e. collision-free)
procedure aMovingObject.tagSafePosition();
var i: integer;
begin
    _oldpos[HISTORY_SIZE - 1].free();
    for i := 1 to max(1, HISTORY_SIZE - 1) do _oldpos[i] := _oldpos[i - 1];
    
    _oldpos[0] := oPoint.clone(_position);
    d(7, _id, 'Position ' + s(_oldpos[0]) + ' tagged safe');
    _succ_reverts := 0; // Resets the successive rollbacks counter
    _revpow := 0;
end;

procedure aMovingObject.rollbackToSafePosition(sticky: boolean);
begin
    if _succ_reverts <= MAX_REVERTS then begin
        d(7, _id, 'Reverting to last (' + s(_revpow) + ') safe position. '
                + s(_position) + s('->') + s(_oldpos[_revpow]));
        do_rollbackToSafePosition();
    end else begin
        d(7, _id, 'NOT reverting to last safe position : too much reverts.');
        if _revpow < HISTORY_SIZE - 1 then _revpow += 1;
        _succ_reverts := 0;
    end;
end;

procedure aMovingObject.rollbackToSafePosition();
begin
    rollbackToSafePosition(isSticky());
end;

// do_rollbackToSafePosition()
// Rollback to the last "safe" position
procedure aMovingObject.do_rollbackToSafePosition();
begin
    _position.free();
    _position := oPoint.clone(_oldpos[_revpow]);
    _succ_reverts += 1;
end;

procedure aMovingObject.elementaryMove(zone: oObjectCollection);
begin
    elementaryMove(zone, _speed.getModule);
end;

// elementaryMove(zone: oObjectCollection)
// Performs an elementary move of the object, triggering collisions and so.
// Note that this method WON'T bother looking wether or not it should move,
// nor will manage to discretize the path. See oPlayground for that matter.
procedure aMovingObject.elementaryMove(zone: oObjectCollection; norm: real);
var ev: oVector;
    i, j: integer;
    p: oPoint;
    colliding: boolean;
    t: Test_SpeedDrawer;
    sticky: boolean;
    a: boolean;
begin
    // We create an elementary vector based on current speed indications
    //ev := oVector.createPolar(_speed.getModule() / norm, _speed.getArgument());
    // Move the object accordingly
    //_position.apply(ev);

    p := oPoint.create(0, 0);
    colliding := false;

    sticky := isSticky();


    d(4, _id, 'Found ' + s(zone.count()) + ' objects to check for collision');

    // Check for collision with objects in the zone, and triggers collision
    // signals if needed
    for i := 0 to zone.count() - 1 do begin
        if self.isColliding(zone.get(i), p) then begin
            d(5, _id, '       * ' + zone.get(i).getId() + ' [COLLISION]');
            getDispatcher().emit(zone.get(i).collisionSignalFactory(self, p));
            colliding := not zone.get(i).isCollisionSafe();
            if zone.get(i).isSticky() then sticky := true;

            if not colliding then d(5, _id, 'Collision was safe');
        end else d(5, _id, '       * ' + zone.get(i).getId() + ' [PASS]');
    end;

    // Tag position if safe, or rollback
    if not colliding then tagSafePosition()
    else for j := _revpow to HISTORY_SIZE - 1 do begin
        _revpow := j;
        a := false;
        for i := 0 to zone.count() - 1 do
            if self.isColliding(zone.get(i), p) then begin
                a := true;
                break;
            end;
        if not a then break;
        rollbackToSafePosition(sticky);
    end;

    // We create an elementary vector based on current speed indications
    ev := oVector.createPolar(_speed.getModule() / norm, _speed.getArgument());
    // Move the object accordingly
    _position.apply(ev);

    // Display speed vector. We need a little more computation than usual to
    // display its origin at the center of the ball. Otherwise, it'll be
    // anchored to the top-left corner of the ball
    p := oPoint.clone(_position);
    p.apply(oVector.createCartesian(7, 7));
    ev := getSpeed();
    ev.factor(10);
    _dispatcher.emit(_speeddrawer.signalFactory(self, ev, p));
end;

// gravity(s: oSignal)
// Callback handling gravity influence on the speed of the object
procedure aMovingObject.gravity(s: oSignal);
var v: oVector;
begin
    // Gravity :
    d(7, _id, 'Hello, this is gravity');
    v := oVector.clone(_speed);
    v.setY(round(_speed.getY() + GEE));
    setSpeed(v);
    v.free();
end;

// oVector getSpeed()
// Returns a copy of the current speed vector
function aMovingObject.getSpeed() : oVector;
begin
    getSpeed := oVector.clone(_speed);
end;

// setSpeed(v: ovector)
// changes the speed vector of the object
procedure aMovingObject.setSpeed(v: oVector);
var old: oVector;
begin
    old := _speed;
    _speed := oVector.clone(v);
    if abs(_speed.getModule()) > MAX_SPEED then _speed.setModule(MAX_SPEED);
    d(12, _id, 'Speed updated. ' + s(old) + s('->') + s(_speed));
    old.free();
end;

// setPosition(p: oPoint)
// changes the position of the object
procedure aMovingObject.setPosition(p: oPoint);
var old: oPoint;
begin
    old := _position;
    _position := oPoint.clone(p);
    d(12, _id, 'Position updated. ' + s(old) + s('->') + s(_position));
    old.free();
end;


end.
