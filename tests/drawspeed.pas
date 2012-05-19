unit drawspeed;

interface

{$mode objfpc}{$H+}

{$UNITPATH .}

uses Classes, signal, eventhandler, uniquesignal, sysutils, uvector, upoint, ugamesignals, BGRABitmap, BGRABitmapTypes;

type

DrawSpeedSignal = class(oUniqueSignal)
    v: oVector;
    p: oPoint;
end;

Test_SpeedDrawer = class(tObject)
    protected
        _speed: oVector;
        _point: oPoint;
        _dispatcher: oEventHandler;
        _set: boolean;
        _id: string;
    public
        color: TBGRAPixel;
        thickness: 1..10;
        constructor create(dispatcher: oEventHandler);

        function signalFactory(s: TObject; v: oVector; p: oPoint) : DrawSpeedSignal;

        procedure updateVector(s: oSignal);
        procedure drawVector(s: oSignal);
end;


implementation

var __object_count: integer = 0;

constructor Test_SpeedDrawer.create(dispatcher: oEventHandler);
var s: oSignal;
begin
    _dispatcher := dispatcher;
    _id := IntToStr(__object_count);
    _speed := oVector.createCartesian(0, 0);
    _point := oPoint.create(0, 0);
    _set := false;

    color := BGRA(255, 0, 0);
    thickness := 2;

    s := self.signalFactory(_dispatcher, _speed, _point);
    _dispatcher.register(s);
    _dispatcher.bind(s, @self.updateVector);
    s.free();

    s := RedrawSignal.create(_dispatcher);
    _dispatcher.bind(s, @self.drawVector);
    s.free();

    __object_count += 1;
end;

function Test_SpeedDrawer.signalFactory(s: TObject; v: oVector; p: oPoint) : DrawSpeedSignal;
begin
    signalFactory := DrawSpeedSignal.create(s, _id);
    signalFactory.v := v;
    signalFactory.p := p;
end;

procedure Test_SpeedDrawer.updateVector(s: oSignal);
var sig: DrawSpeedSignal;
begin
    sig := s as DrawSpeedSignal;
    _speed.free();
    _point.free();
    _speed := oVector.clone(sig.v);
    _point := oPoint.clone(sig.p);
    _set := true;
end;

procedure Test_SpeedDrawer.drawVector(s: oSignal);
var sig: RedrawSignal;
    p2: oPoint;
begin
    sig := s as RedrawSignal;
    p2 := oPoint.clone(_point);
    p2.apply(_speed);
    sig.bm.drawPolyLineAntialias([PointF(_point.getX(), _point.getY()), PointF(p2.getX(), p2.getY())], color, thickness);
end;


end.