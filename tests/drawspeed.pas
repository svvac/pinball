unit drawspeed;

interface

{$mode objfpc}{$H+}

{$UNITPATH .}

uses
    // ontheair
    eventhandler, signal, uniquesignal,
    // home-baked units
    ugamesignals, upoint, utils, uvector,
    // Custom graphics lib
    BGRABitmap, BGRABitmapTypes,
    // stdlib
    Classes
    ;

type

// (unique) signal emitted by the drawer
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

        function signalFactory(o: TObject; v: oVector;
                               p: oPoint) : DrawSpeedSignal;

        procedure updateVector(si: oSignal);
        procedure drawVector(si: oSignal);
end;


implementation
// Object counter, to generate signal UIDs
var __object_count: integer = 0;

// create(dispatcher: oEventHandler)
// Creates a drawer and register/binds the stuff to the dispatcher
constructor Test_SpeedDrawer.create(dispatcher: oEventHandler);
var sig: oSignal;
begin
    _dispatcher := dispatcher;
    _id := s(__object_count);
    _speed := oVector.createCartesian(0, 0);
    _point := oPoint.create(0, 0);
    _set := false; // No data set yet, don't draw anything

    // Public values, to allow user to change line color and thickness
    color := BGRA(255, 0, 0);
    thickness := 2;

    // registers the unique signal, and binds it to updateVector
    sig := self.signalFactory(_dispatcher, _speed, _point);
    _dispatcher.register(sig);
    _dispatcher.bind(sig, @self.updateVector);
    sig.free();

    // Bind drawVector to the RedrawSignal
    sig := RedrawSignal.create(_dispatcher);
    _dispatcher.bind(sig, @self.drawVector);
    sig.free();

    __object_count += 1;
end;

// DrawSpeedSignal signalFactory(s: tObject; v: oVector; p: oPoint)
// Create a unique signal associated to this object
function Test_SpeedDrawer.signalFactory(o: TObject; v: oVector;
                                        p: oPoint) : DrawSpeedSignal;
begin
    signalFactory := DrawSpeedSignal.create(o, _id);
    signalFactory.v := v;
    signalFactory.p := p;
end;

// updateVector(s: oSignal)
// Callback used to update the vector to draw
procedure Test_SpeedDrawer.updateVector(si: oSignal);
var sig: DrawSpeedSignal;
begin
    sig := si as DrawSpeedSignal;
    // Copy data to the object
    _speed.free();
    _point.free();
    _speed := oVector.clone(sig.v);
    _point := oPoint.clone(sig.p);
    _set := true;
end;

// drawVector(s: oSignal)
// Callback used to draw the vectors to the display
procedure Test_SpeedDrawer.drawVector(si: oSignal);
var sig: RedrawSignal;
    p2: oPoint;
begin
    sig := si as RedrawSignal;
    p2 := oPoint.clone(_point);
    p2.apply(_speed);
    // Not much to say, this is how we draw a line with a BGRABitmap
    sig.bm.drawPolyLineAntialias([PointF(_point.getX(), _point.getY()),
                                  PointF(p2.getX(), p2.getY())],
                                  color, thickness);
end;


end.