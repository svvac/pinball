unit ubouncingobject;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics, signal, uobject, upoint, ushape, eventhandler, uvector, umovingobject, ugamesignals, BGRABitmap;


type aBouncingObject = class(aObject)
    protected
        _bounce_factor: real;

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

    alpha := o.getMask().getTangentAngleAt(pos);

    w := oVector.createPolar(v.getModule() * getBounceFactor(), -v.getArgument() - 2 * alpha);
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
