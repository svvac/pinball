unit ugamesignals;

{$mode objfpc}{$H+}

interface

uses Classes, Graphics, signal, uniquesignal, upoint, BGRABitmap;

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

// Signal triggered when the player dies
DeathSignal = class(oSignal);

// Signal triggered when we need to redraw
RedrawSignal = class(oSignal)
    bm: TBGRABitmap;
end;

// Signal triggered at every tick of our discretized time
TickSignal = class(oSignal);










implementation

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
