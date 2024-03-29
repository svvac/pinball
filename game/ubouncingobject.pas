unit ubouncingobject;

{$mode objfpc}{$H+}

interface

uses
    // ontheair
    eventhandler, signal,
    // home-baked units
    drawspeed, ugamesignals,
        // utils
        upoint, ushape, utils, uvector,
        // objects
        umovingobject, uobject,
    // custom graphics lib
    BGRABitmap, BGRABitmapTypes,
    // stdlib
    Classes, Math, SysUtils
    ;

const
    // Points basis
    MEXICAN_GOLD = 200;


type aBouncingObject = class(aObject)
    protected
        _bounce_factor: real;
        _speeddrawer1: Test_SpeedDrawer;
        _speeddrawer2: Test_SpeedDrawer;

    public
        constructor create(position: oPoint; mask: oShape; face: TBGRABitmap;
                           dispatcher: oEventHandler; factor: real);
        function getBounceFactor() : real; virtual;
        procedure onCollision(si: oSignal); override;

end;

implementation

// create(position: oPoint; mask: oShape; face: TBGRABitmap;
//        dispatcher: oEventHandler; factor: real)
// Creates a bouncing object
constructor aBouncingObject.create(position: oPoint; mask: oShape;
                                   face: TBGRABitmap;
                                   dispatcher: oEventHandler; factor: real);
begin
    inherited create(position, mask, face, dispatcher);
    _bounce_factor := factor;
    _collision_safe := false;

    // Registers debug speed drawers. Technically, they won't display vectors,
    // but normals. So we use two, to draw on both sides of the edge.
    _speeddrawer1 := Test_SpeedDrawer.create(_dispatcher);
    _speeddrawer2 := Test_SpeedDrawer.create(_dispatcher);
    _speeddrawer1.color := BGRA(0, 0, 255);
    _speeddrawer2.color := BGRA(0, 0, 255);
end;

// onCollision(si: oSignal)
// Callback handling the bouncing of a moving object
procedure aBouncingObject.onCollision(si: oSignal);
var sig: CollisionSignal;
    o: aMovingObject;
    v, w: oVector;
    alpha: real;
    pos: oPoint;
begin
    sig := si as CollisionSignal;
    o := sig.getSender() as aMovingObject;
    v := o.getSpeed();

    // oShape only uses relative coordinates, i.e. the top left corner of the
    // object is the point (0, 0).
    // Conversion is siple enough, given the absolute point of collision.
    //    relative = absolute - origin
    pos := oPoint.clone(sig.position);
    w := self.getPosition().position();
    w.factor(-1);
    pos.apply(w);
    w.free();

    //if (v.getModule() > 3) {or not _sticky} then begin
    if getBounceFactor() > 1 then begin
        // Give some points 
        si := ScoreChangeSignal.create(
            self, round(MEXICAN_GOLD * getBounceFactor()));
        _dispatcher.emit(si);
        si.free();
    end;

    // Compute the angle of the normal at the collision point
    alpha := self.getMask().getNormalAngleAt(pos);

    // Sometimes, the angle returned by the fuction is Nan (aka oo), so we
    // need to handle this case
    if isNan(alpha) then alpha := v.getArgument() + 4*arctan(1); // Dirty hack

    // Compute the new angle of the speed vector.
    //   outgoing = 2 normal - incoming - pi
    // Basically, this comes from a change of reference. Can't really draw the
    // details here.
    w := oVector.createPolar(v.getModule() * getBounceFactor(),
                             2 * alpha - v.getArgument() - 4 * arctan(1));
    // Updates speed
    o.setSpeed(w);

    d(4, _id, 'Handling bouncing at ' + s(sig.position) + ': ' + s('alpha')
            + '=' + s(alpha) + '; speed: ' + s(v) + s('->') + s(w));

    v.free();
    w.free();
{end else begin
    alpha := self.getMask().getSecantAngleAt(pos);
    if isNan(alpha) then alpha := v.getArgument() + 2*arctan(1);
    w := oVector.createPolar(v.getModule(), alpha);
    o.setSpeed(w);}
    //end else begin

    // Draws debug vectors
    _dispatcher.emit(_speeddrawer1.signalFactory(self,
        oVector.createPolar(20, alpha), sig.position));
    _dispatcher.emit(_speeddrawer2.signalFactory(self,
        oVector.createPolar(-20, alpha), sig.position));
end;

// real getBounceFactor()
// Returns the bouncing factor
function aBouncingObject.getBounceFactor() : real;
begin
    getBounceFactor := _bounce_factor;
end;


end.
