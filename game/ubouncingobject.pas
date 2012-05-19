unit ubouncingobject;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics, signal, uobject, upoint, ushape, eventhandler, uvector, umovingobject, ugamesignals, BGRABitmap, BGRABitmapTypes, drawspeed;


type aBouncingObject = class(aObject)
    protected
        _bounce_factor: real;
        _speeddrawer1: Test_SpeedDrawer;
        _speeddrawer2: Test_SpeedDrawer;

    public
        constructor create(position: oPoint; mask: oShape; face: TBGRABitmap; dispatcher: oEventHandler; factor: real);
        function getBounceFactor() : real; virtual;
        procedure onCollision(s: oSignal); override;

end;

implementation

constructor aBouncingObject.create(position: oPoint; mask: oShape; face: TBGRABitmap; dispatcher: oEventHandler; factor: real);
begin
    inherited create(position, mask, face, dispatcher);
    _bounce_factor := factor;
    _collision_safe := false;

    _speeddrawer1 := Test_SpeedDrawer.create(_dispatcher);
    _speeddrawer2 := Test_SpeedDrawer.create(_dispatcher);
    _speeddrawer1.color := BGRA(0, 0, 255);
    _speeddrawer2.color := BGRA(0, 0, 255);
end;


procedure aBouncingObject.onCollision(s: oSignal);
var sig: CollisionSignal;
    o: aMovingObject;
    v, w: oVector;
    alpha: real;
    pos: oPoint;
begin
    sig := s as CollisionSignal;
    o := sig.getSender() as aMovingObject;
    v := o.getSpeed();

    // Compute relative position
    pos := oPoint.clone(sig.position);
    w := o.getPosition().position();
    w.factor(-1);
    pos.apply(w);
    w.free();

    alpha := o.getMask().getNormalAngleAt(pos);

    _dispatcher.emit(_speeddrawer1.signalFactory(self, oVector.createPolar(20, alpha), sig.position));
    _dispatcher.emit(_speeddrawer2.signalFactory(self, oVector.createPolar(-20, alpha), sig.position));

    w := oVector.createPolar(v.getModule() * getBounceFactor(), 2 * alpha - v.getArgument() - 4 * arctan(1));
    o.setSpeed(w);

    writeln(_id + ': Handling bouncing at ' + sig.position.toString() + ': α=' + FloatToStr(alpha) + '; speed: ' + v.toString() + ' -> ' + w.toString());

    v.free();
    w.free();
end;


function aBouncingObject.getBounceFactor() : real;
begin
    getBounceFactor := _bounce_factor;
end;


end.
