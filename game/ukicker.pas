unit ukicker;

{$mode objfpc}{$H+}

interface

uses
    // ontheair
    eventhandler,
    // home-baked units
        // objects
        ubouncingobject,
        // utils
        uobject, upoint, ushape, utils, uvector,
    // custom graphics library
    BGRABitmap,
    // stdlib
    Classes, SysUtils
    ;

type oKickerLeft = class(aBouncingObject)
    public
        constructor create(position: oPoint;
                           dispatcher: oEventHandler); virtual;

end;

type oKickerRight = class(aBouncingObject)
    public
        constructor create(position: oPoint;
                           dispatcher: oEventHandler); virtual;

end;

implementation

// create(dispatcher: oEventHandler)
// Well, create the shit
constructor oKickerLeft.create(position: oPoint; dispatcher: oEventHandler);
var m: oShape;
    bm: TBGRABitmap;
    p : oPoint;
begin
    m := oShape.create('bitmaps/kicker-left.bmp');
    bm := TBGRABitmap.create('bitmaps/kicker-left.png');

    inherited create(position, m, bm, dispatcher, 1.5);

    d(4, _id, 'Added at ' + s(self.getPosition()));
end;

// create(dispatcher: oEventHandler)
// Well, create the shit
constructor oKickerRight.create(position: oPoint; dispatcher: oEventHandler);
var m: oShape;
    bm: TBGRABitmap;
    p : oPoint;
begin
    m := oShape.create('bitmaps/kicker-right.bmp');
    bm := TBGRABitmap.create('bitmaps/kicker-right.png');

    inherited create(position, m, bm, dispatcher, 1.5);

    d(4, _id, 'Added at ' + s(self.getPosition()));
end;

end.
