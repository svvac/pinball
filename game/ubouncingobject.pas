unit ubouncingobject;

{$mode objfpc}{$H+}

interface

uses Classes, signal, uobject, umovingobject, ugamesignals;


type aBouncingObject = class(aObject)
    protected
        _bounce_factor: real;

    public
        function getBounceFactor() : real; virtual;
        procedure onCollision(s: oSignal); override;

end;

implementation


procedure aBouncingObject.onCollision(s: oSignal);
var sig: CollisionSignal;
    o: aMovingObject;
begin
    sig := s as CollisionSignal;
    
end;


function aBouncingObject.getBounceFactor() : real;
begin
    getBounceFactor := _bounce_factor;
end;


end.
