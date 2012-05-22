unit uplunger;

{$mode objfpc}{$H+}

interface

uses
    // ontheair
    eventhandler, signal,
    // home-baked units
    ugamesignals,
        // objects
        ubouncingobject, umovingobject,
        // utils
        uobject, upoint, ushape, utils, uvector,
    // custom graphics library
    BGRABitmap,
    // stdlib
    Classes, SysUtils
    ;

const
    // Threshold level for auto-plung
    PLUNG_THRESHOLD = 1.0;
    // Factor for power load
    LOAD_FACTOR = 1.8;
    BREATH_LEVEL = 0.1; // 4 steps
    // Number of ticks to wait before being able to pull again
    REST_STEPS = 10;

type oPlunger = class(aBouncingObject)
    protected
        _kick: ovector;
        _pow: real;
        _plung: boolean;
        _wait: integer;
    public
        constructor create(position: oPoint; kick: oVector;
                           dispatcher: oEventHandler); virtual;

        procedure onPull(si: oSignal);
        procedure onRelease(si: oSignal);
        procedure onTick(si: oSignal);
        procedure onCollision(si: oSignal); override;
end;

implementation

// create(dispatcher: oEventHandler)
// Well, create the shit
constructor oPlunger.create(position: oPoint; kick: oVector;
                            dispatcher: oEventHandler);
var m: oShape;
    bm: TBGRABitmap;
    sig: oSignal;
begin
    m := oShape.create('bitmaps/plunger.bmp');
    bm := TBGRABitmap.create('bitmaps/blank.png');
    
    inherited create(position, m, bm, dispatcher, 0.1);

    _wait := 0;
    _plung := false;
    _pow := BREATH_LEVEL;
    _kick := oVector.clone(kick);
    _sticky := true;

    sig := PlungerPullSignal.create(_dispatcher);
    _dispatcher.bind(sig, @self.onPull);
    sig.free();

    sig := PlungerReleaseSignal.create(_dispatcher);
    _dispatcher.bind(sig, @self.onRelease);
    sig.free();

    sig := TickSignal.create(_dispatcher);
    _dispatcher.bind(sig, @self.onTick);
    sig.free();

    d(4, _id, 'Added at ' + s(self.getPosition()));
end;

procedure oPlunger.onPull(si: oSignal);
begin
    if (_wait = 0) and not _plung then begin
        _pow *= LOAD_FACTOR;

        if (_pow >= PLUNG_THRESHOLD) then begin
            _pow := PLUNG_THRESHOLD;
            
            _dispatcher.emit(PlungerReleaseSignal.create(self));
        end;
    end;
end;

procedure oPlunger.onRelease(si: oSignal);
begin
    if _wait = 0 then _plung := true;
end;

procedure oPlunger.onTick(si: oSignal);
begin
    if _wait > 0 then _wait -= 1;

end;

procedure oPlunger.onCollision(si: oSignal);
var sig: CollisionSignal;
    o: aMovingObject;
    v: oVector;
begin
    sig := si as CollisionSignal;
    o := sig.getSender() as aMovingObject;
    inherited onCollision(sig);

    if _plung then begin
        v := oVector.clone(_kick);
        v.factor(_pow);
        o.setSpeed(v);
        v.free();

        _plung := false;
        _wait := REST_STEPS;
        _pow := BREATH_LEVEL;
    end;
end;


end.
