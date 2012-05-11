unit upoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

Type oPoint = class

    protected
        _x, _y: integer;

    public

    constructor create(aPx, aPy : integer);
    constructor clone(p: oPoint);

    function distanceTo(p:oPoint) : real;

    function getX() : integer;
    function getY() : integer;
    
    procedure setX(aPx: integer);
    procedure setY(aPy: integer);
    procedure setXY(aPx, aPy: integer);
    
    function sameAs(p: oPoint) : boolean;
    
    function toStr() : string;
  end;



implementation


// create(x, y: integer)
// creates a point of coordinates (x, y)
constructor oPoint.create(aPx, aPy: integer);
begin
    _x := aPx;
    _y := aPy;
end;

// clone(p: oPoint)
// creates a new point by cloning p
constructor oPoint.clone(p: oPoint);
begin
    _x := p.getX();
    _y := p.getY();
end;

// real distanceTo(p: oPoint)
// computes the distance to the point `p'
function oPoint.distanceTo(p:oPoint):real;
begin
    distanceTo := sqrt((p.getX - _x) * (p.getX - _x) + (p.getY - _y) * (p.getY - _y));
end;

// boolean sameAs(p: oPoint)
// returns true if `p' has the same coordinates
function oPoint.sameAs(p: oPoint) : boolean;
begin
    sameAs := (p.getX = getX) and (p.getY = getY);
end;

// integer getX(void)
// returns the X coordinate
function oPoint.getX() : integer;
begin
    getX := _x;
end;

// integer getY(void)
// returns the Y coordinate
function oPoint.getY() : integer;
begin
    getY := _y;
end;

// setX(x: integer)
// changes the X coordinate to x
procedure oPoint.setX(aPx: integer);
begin
    _x := aPx;
end;

// setY(y: integer)
// changes the Y coordinate to y
procedure oPoint.setY(aPy: integer);
begin
    _y := aPy;
end;

// setXY(x, y: integer)
// changes the X and Y coordinates, resp. to x and y
procedure oPoint.setXY(aPx, aPy: integer);
begin
    setX(aPx);
    setY(aPy);
end;

// string toStr(void)
// returns a string representation of the point as (X, Y)
function oPoint.toStr() : string;
begin
    toStr := '(' + IntToStr(getX) + ', ' + IntToStr(getY) + ')';
end;

end.

