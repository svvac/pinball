unit uflip;

{$mode objfpc}{$H+}

interface

uses
    // ontheair
    eventhandler, signal,
    // home-baked units
    ugamesignals,
        // objects
        ubouncingobject,
        // utils
        uobject, upoint, ushape, utils, uvector,
    // custom graphics library
    BGRABitmap,
    // stdlib
    Classes, SysUtils
    ;

type aFlip = class(aBouncingObject)
    protected
        _masks: array of oShape;
        _faces: array of TBGRABitmap;
        _pos: integer;
        _maxpos: integer;
        _update: integer;
    public
        constructor create(position: oPoint; pattern: string; n: integer;
                           bindto: oSignal; dispatcher: oEventHandler); virtual;

        procedure nextPos();
        procedure prevPos();

        procedure onFlipUp(si: oSignal);
        procedure onFlipDown(si: oSignal);
        procedure onTick(si: oSignal);
end;

implementation

// create(dispatcher: oEventHandler)
// Well, create the shit
constructor aFlip.create(position: oPoint; pattern: string; n: integer; 
                           bindto: oSignal; dispatcher: oEventHandler);
var m: oShape;
    bm: TBGRABitmap;
    p : oPoint;
    i: integer;
    sig: oSignal;
begin
    setlength(_masks, n);
    setlength(_faces, n);

    for i := 0 to n - 1 do begin
        _masks[i] := oShape.create('bitmaps/' + pattern + s(i) + '.bmp');
        _faces[i] := TBGRABitmap.create('bitmaps/' + pattern + s(i) + '.png');
    end;

    _pos := 0;
    _maxpos := n - 1;
    _update := 0;

    inherited create(position, _masks[0], _faces[0], dispatcher, 1);

    _dispatcher.bind(bindto, @self.onFlipUp);

    sig := TickSignal.create(_dispatcher);
    _dispatcher.bind(sig, @self.onTick);
    sig.free();

    d(4, _id, 'Added at ' + s(self.getPosition()));
end;

procedure aFlip.nextPos();
begin
    if _pos < _maxpos then begin
        _pos := min(_pos + _update, _maxpos);
        _mask := _masks[_pos];
        _face := _faces[_pos];
    end else _update *= -1;
end;

procedure aFlip.prevPos();
begin
    if _pos > 0 then begin
        _pos := max(0, _pos + _update);
        _mask := _masks[_pos];
        _face := _faces[_pos];
    end else _update := 0;
end;

procedure aFlip.onFlipUp(si: oSignal);
begin
    _update := +5;
end;

procedure aFlip.onFlipDown(si: oSignal);
begin
    _update := -5;
end;

procedure aFlip.onTick(si: oSignal);
begin
    if _update > 0      then nextPos()
    else if _update < 0 then prevPos();
end;


end.
