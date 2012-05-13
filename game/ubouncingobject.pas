unit ubouncingobject;

{$mode objfpc}{$H+}

interface

uses Classes, signal, uobject, uvector, umovingobject, ugamesignals;


type aBouncingObject = class(aObject)
    protected
        _bounce_factor: real;

    public
        function getBounceFactor() : real; virtual;
        procedure onCollision(s: oSignal); override;

end;

implementation


procedure aBouncingObject.onCollision(s: oSignal);
var sig: CollisionSignal;
    o: aMovingObject;
    v: oVector;
    alpha: real;
begin
    sig := s as CollisionSignal;
    o := sig.getSender() as aMovingObject;
    v := oVector.clone(o.getSpeed());

    alpha := o.getMask().getTangentAngleAt(sig.position);

    o.getSpeed().setModule(v.getModule() * getBounceFactor());
    o.getSpeed().setArgument(- v.getArgument() - 2 * alpha);

    v.free();

end;


function aBouncingObject.getBounceFactor() : real;
begin
    getBounceFactor := _bounce_factor;
end;


end.
