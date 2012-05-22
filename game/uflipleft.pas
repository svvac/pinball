unit uflipleft;

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

type oFlipLeft = class(aFlip)
    public
        constructor create(position: oPoint;
                           dispatcher: oEventHandler); virtual;

end;

implementation

// create(dispatcher: oEventHandler)
// Well, create the shit
constructor oFlipLeft.create(position: oPoint; dispatcher: oEventHandler);
var sig: oSignal;
begin
    sig := FlipLeftSignal.create(dispatcher);
    inherited create(position, 'lflip', 13, sig, dispatcher);
    sig.free();
end;



end.
