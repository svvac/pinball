unit uflipleftright;

{$mode objfpc}{$H+}

interface

uses
    // ontheair
    eventhandler, signal,
    // home-baked units
    ugamesignals,
        // objects
        uflip,
        // utils
        uobject, upoint, ushape, utils, uvector,
    // custom graphics library
    BGRABitmap,
    // stdlib
    Classes, SysUtils
    ;

type
oFlipRight = class(aFlip)
    public
        constructor create(position: oPoint;
                           dispatcher: oEventHandler); virtual;
end;

oFlipLeft = class(aFlip)
    public
        constructor create(position: oPoint;
                           dispatcher: oEventHandler); virtual;
end;

implementation

// create(dispatcher: oEventHandler)
// Well, create the shit
constructor oFlipRight.create(position: oPoint; dispatcher: oEventHandler);
var sig: oSignal;
begin
    sig := FlipRightSignal.create(dispatcher);
    inherited create(position, 'flip-right/flip', 70, sig, dispatcher);
    sig.free();
end;

// create(dispatcher: oEventHandler)
// Well, create the shit
constructor oFlipLeft.create(position: oPoint; dispatcher: oEventHandler);
var sig: oSignal;
begin
    sig := FlipLeftSignal.create(dispatcher);
    inherited create(position, 'flip-left/flip', 70, sig, dispatcher);
    sig.free();
end;



end.
