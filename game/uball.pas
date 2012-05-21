unit uball;

{$mode objfpc}{$H+}

interface

uses
    // ontheair
    eventhandler,
    // home-baked units
        // objects
        umovingobject,
        // utils
        uobject, upoint, ushape, utils,
    // custom graphics library
    BGRABitmap,
    // stdlib
    Classes, SysUtils
    ;

type oBall = class(aMovingObject)
    public
        constructor create(position: oPoint;
                           dispatcher: oEventHandler); virtual;

end;

implementation

// create(dispatcher: oEventHandler)
// Well, create the shit
constructor oBall.create(position: oPoint; dispatcher: oEventHandler);
var m: oShape;
    bm: TBGRABitmap;
begin
    m := oShape.create('bitmaps/ball.bmp');
    bm := TBGRABitmap.create('bitmaps/ball.png');

    inherited create(position, m, bm, dispatcher);
end;


end.
