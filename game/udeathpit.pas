unit udeathpit;

{$mode objfpc}{$H+}

interface

uses
    // ontheair
    eventhandler, signal,
    // home-baked units
    ugamesignals,
        // utils
        uobject, upoint, ushape, utils,
    // custom graphics library
    BGRABitmap,
    // stdlib
    Classes, SysUtils
    ;

type oDeathPit = class(aObject)
    public
        constructor create(position: oPoint;
                           dispatcher: oEventHandler); virtual;

        procedure onCollision(si: oSignal); override;

end;

implementation

// create(dispatcher: oEventHandler)
// Well, create the shit
constructor oDeathPit.create(position: oPoint; dispatcher: oEventHandler);
var m: oShape;
    bm: TBGRABitmap;
begin
    m := oShape.create('bitmaps/bottom.bmp');
    bm := TBGRABitmap.create('bitmaps/blank.png');

    inherited create(position, m, bm, dispatcher);

    d(4, _id, 'Added at ' + s(self.getPosition()));
end;

procedure oDeathPit.onCollision(si: oSignal);
begin
    _dispatcher.emit(DeathSignal.create(self));
end;

end.
