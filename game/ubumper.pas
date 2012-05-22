unit ubumper;

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

type oBumper = class(aBouncingObject)
    public
        constructor create(position: oPoint;
                           dispatcher: oEventHandler); virtual;

end;

implementation

// create(dispatcher: oEventHandler)
// Well, create the shit
constructor oBumper.create(position: oPoint; dispatcher: oEventHandler);
var m: oShape;
    bm: TBGRABitmap;
    p : oPoint;
begin
    m := oShape.create('bitmaps/bumper.bmp');
    bm := TBGRABitmap.create('bitmaps/bumper.png');
    p := position.clone(position);
    p.apply(oVector.createCartesian(round(-m.getWidth() / 2), round(-m.getHeight() / 2)));

    inherited create(position, m, bm, dispatcher, 1.5);

    d(4, _id, 'Added at ' + s(self.getPosition()));
end;


end.
