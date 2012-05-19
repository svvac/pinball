unit uguide;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics, signal, uobject, upoint, ushape, eventhandler, uvector, umovingobject, ugamesignals, BGRABitmap;


type oGuide = class(aObject)
    protected
        _steps: integer;

    public
        constructor create(position: oPoint; line: string; dispatcher: oEventHandler; steps: integer);
        procedure onCollision(s: oSignal); override;

end;

implementation

constructor oGuide.create(position: oPoint; line: string; dispatcher: oEventHandler; steps: integer);
var bm: TBGRABitmap;
     s: oShape;
begin
    bm := TBGRABitmap.create(line + '.png');
    s := oShape.create(line + '.bmp');
    inherited create(position, s, bm, dispatcher);
    _steps := steps;
end;


procedure oGuide.onCollision(s: oSignal);
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

    alpha := o.getMask().getTangentAngleAt(pos, abs(_steps));

    if _steps < 0 then alpha += 4*arctan(1);

    v.setArgument(alpha);
    o.setSpeed(v);

    writeln(_id + ': Handling guiding at ' + sig.position.toString() + ': α=' + FloatToStr(alpha));

    v.free();
end;


end.
