unit ugamesignals;

{$mode objfpc}{$H+}

interface

uses Classes, Graphics, signal, upoint, BGRABitmap;

type

// Signal triggered on collision
CollisionSignal = class(oSignal)
    protected _id: string;
    public
        position: oPoint;
        constructor create(sender: TObject; id: string; p: oPoint);
        destructor destroy(); override;
        function getName() : string; override;
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
    _id := self.ClassName + '/' + id;
    inherited create(sender);
end;

destructor CollisionSignal.destroy();
begin
    position.free();
    inherited;
end;

function CollisionSignal.getName() : string;
begin
    getName := _id;
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
