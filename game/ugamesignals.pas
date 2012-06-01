unit ugamesignals;

{$mode objfpc}{$H+}

interface

uses
    // ontheair
    signal, uniquesignal,
    // home-baked units
    upoint, utils,
    // cutsom graphics library
    BGRABitmap,
    // stdlib
    Classes
    ;

type

// Signal triggered on collision
CollisionSignal = class(oUniqueSignal)
    public
        position: oPoint;
        constructor create(sender: TObject; id: string; p: oPoint);
        destructor destroy(); override;
        function toString() : string; override;
end;

// Signal triggered on score variation
ScoreChangeSignal = class(oSignal)
    points: integer;
    constructor create(sender: TObject; p: integer);
end;

// Signal triggered when the ball dies
DeathSignal = class(oSignal);

// Signal triggered when the game is over
GameOverSignal = class(oSignal);
GameStartSignal = class(oSignal);

// Signal triggered when we need to redraw
RedrawSignal = class(oSignal)
    bm: TBGRABitmap;
end;

// Signal triggered at every tick of our discretized time
TickSignal = class(oSignal);

NanoTickSignal = class(oSignal)
    public
        len: integer;
        i: integer;
        constructor create(sender: tObject; a, b: integer);
        function toString() : string;
end;

NanoTickRegisterSignal = class(oSignal)
    public
        n: integer;
        constructor create(sender: tObject; a: integer);
        function toString() : string;
end;

FlipLeftSignal = class(oSignal);
FlipRightSignal= class(oSignal);

PlungerPullSignal = class(oSignal);
PlungerReleaseSignal = class(oSignal);










implementation

constructor NanoTickSignal.create(sender: tObject; a, b: integer);
begin
    i := a;
    len := b;
    inherited create(sender);
end;

function NanoTickSignal.toString() : string;
begin
    toString := inherited toString() + ' ' + s(i) + '/' + s(len);
end;


constructor NanoTickRegisterSignal.create(sender: tObject; a: integer);
begin
    n := a;
    inherited create(sender);
end;

function NanoTickRegisterSignal.toString() : string;
begin
    toString := inherited toString() + ' ' + s(n) + ' nanoticks registered';
end;

constructor CollisionSignal.create(sender: TObject; id: string; p: oPoint);
begin
    position := oPoint.clone(p);
    inherited create(sender, id);
end;

destructor CollisionSignal.destroy();
begin
    position.free();
    inherited;
end;

function CollisionSignal.toString() : string;
begin
    toString := inherited toString() + ' at ' + position.toString();
end;

constructor ScoreChangeSignal.create(sender: TObject; p: integer);
begin
    points := p;
    inherited create(sender);
end;


end.
