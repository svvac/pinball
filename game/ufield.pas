unit ufield;

{$mode objfpc}{$H+}

interface

uses
    // ontheair
    eventhandler,
    // home-baked units
        // objects
        ubouncingobject,
        // utils
        uobject, upoint, ushape, utils,
    // custom graphics library
    BGRABitmap,
    // stdlib
    Classes, SysUtils
    ;

type oField = class(aBouncingObject)
    public
        constructor create(position: oPoint;
                           dispatcher: oEventHandler); virtual;

end;

implementation

// create(dispatcher: oEventHandler)
// Well, create the shit
constructor oField.create(position: oPoint; dispatcher: oEventHandler);
var m: oShape;
    bm: TBGRABitmap;
begin
    m := oShape.create('bitmaps/field.bmp');
    bm := TBGRABitmap.create('bitmaps/field.png');

    inherited create(position, m, bm, dispatcher, 0.9);

    d(4, _id, 'Added at ' + s(self.getPosition()));
end;


end.
