unit ugamesignals;

{$mode objfpc}{$H+}

interface

uses Classes, Graphics, signal, upoint;

type

// Signal triggered on collision
CollisionSignal = class(oSignal)
    protected _id: string;
    public
        position: oPoint;
        constructor create(sender: TObject; id: string; p: oPoint);
        destructor destroy(); override;
        function getName() : string; override;
end;

ScoreChangeSignal = class(oSignal)
    points: integer;
    constructor create(sender: TObject; p: integer);
end;

DeathSignal = class(oSignal);

RedrawSignal = class(oSignal)
    bm: TBitmap;
end;










implementation

constructor CollisionSignal.create(sender: TObject; id: string; p: oPoint);
begin
    position := oPoint.clone(p);
    _id := self.ClassName + '::' + id;
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

constructor ScoreChangeSignal.create(sender: TObject; p: integer);
begin
    points := p;
    inherited create(sender);
end;


end.
