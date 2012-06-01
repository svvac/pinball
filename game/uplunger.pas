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
        _working: boolean;
    public
        constructor create(position: oPoint; kick: oVector;
                           dispatcher: oEventHandler); virtual;

        procedure onPull(si: oSignal);
        procedure onRelease(si: oSignal);
        procedure onTick(si: oSignal);
        procedure onCollision(si: oSignal); override;
        procedure onGameOver(si: oSignal);
        procedure onGameStart(si: oSignal);
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
    _sticky := false;
    _working := true;

    sig := PlungerPullSignal.create(_dispatcher);
    _dispatcher.bind(sig, @self.onPull);
    sig.free();

    sig := PlungerReleaseSignal.create(_dispatcher);
    _dispatcher.bind(sig, @self.onRelease);
    sig.free();

    sig := TickSignal.create(_dispatcher);
    _dispatcher.bind(sig, @self.onTick);
    sig.free();

    sig := GameOverSignal.create(_dispatcher);
    _dispatcher.bind(sig, @self.onGameOver);
    sig.free();

    sig := GameOverSignal.create(_dispatcher);
    _dispatcher.bind(sig, @self.onGameStart);
    sig.free();

    d(4, _id, 'Added at ' + s(self.getPosition()));
end;

procedure oPlunger.onPull(si: oSignal);
begin
    if (_wait = 0) and not _plung then begin
        _pow *= LOAD_FACTOR;

        d(5, _id, 'Plunger pulled at '
                + s(int(_pow * 100 / PLUNG_THRESHOLD)) + '%');

        if (_pow >= PLUNG_THRESHOLD) then begin
            _pow := PLUNG_THRESHOLD;
            
            _dispatcher.emit(PlungerReleaseSignal.create(self));
        end;
    end;
end;

procedure oPlunger.onRelease(si: oSignal);
begin
    if _wait = 0 then begin
        _plung := true;
        d(5, _id, 'Releasing');
    end;
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

    if _plung and _working then begin
        v := oVector.clone(_kick);
        v.factor(_pow);

        d(5, _id, 'Kicking with ' + s(v));

        o.setSpeed(v);
        o.setPosition(oPoint.create(256, 39));
        v.free();

        _plung := false;
        _wait := REST_STEPS;
        _pow := BREATH_LEVEL;
    end;
end;

procedure oPlunger.onGameOver(si: oSignal);
begin
    _working := false;
end;

procedure oPlunger.onGameStart(si: oSignal);
begin
    _working := true;
end;


end.