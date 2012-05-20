unit upoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uvector, printable; 

Type _oPoint = class

    protected
        _x, _y: real;

    public

    constructor create(aPx, aPy : integer);

    function distanceTo(p: _oPoint) : real;

    function getX() : integer;
    function getY() : integer;
    
    procedure setX(aPx: integer);
    procedure setY(aPy: integer);
    procedure setXY(aPx, aPy: integer);

    procedure apply(v: oVector);

    function position() : oVector;
    
    function sameAs(p: _oPoint) : boolean;
end;

oPoint = class(_oPoint, iPrintable)
    constructor clone(p: oPoint);
    function toString() : string;
end;



implementation


// create(x, y: integer)
// creates a point of coordinates (x, y)
constructor _oPoint.create(aPx, aPy: integer);
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

// apply(v: oVector)
// Moves the point according to the vector `v'
procedure _oPoint.apply(v: oVector);
begin
    _x += v.getRX();
    _y += v.getRY();
end;

// real distanceTo(p: oPoint)
// computes the distance to the point `p'
function _oPoint.distanceTo(p: _oPoint):real;
begin
    distanceTo := sqrt((p.getX - _x) * (p.getX - _x) + (p.getY - _y) * (p.getY - _y));
end;

// boolean sameAs(p: oPoint)
// returns true if `p' has the same coordinates
function _oPoint.sameAs(p: _oPoint) : boolean;
begin
    sameAs := (p.getX = getX) and (p.getY = getY);
end;

// integer getX(void)
// returns the X coordinate
function _oPoint.getX() : integer;
begin
    getX := round(_x);
end;

// integer getY(void)
// returns the Y coordinate
function _oPoint.getY() : integer;
begin
    getY := round(_y);
end;

// setX(x: integer)
// changes the X coordinate to x
procedure _oPoint.setX(aPx: integer);
begin
    _x := aPx;
end;

// setY(y: integer)
// changes the Y coordinate to y
procedure _oPoint.setY(aPy: integer);
begin
    _y := aPy;
end;

// setXY(x, y: integer)
// changes the X and Y coordinates, resp. to x and y
procedure _oPoint.setXY(aPx, aPy: integer);
begin
    setX(aPx);
    setY(aPy);
end;

function _oPoint.position() : oVector;
begin
    position := oVector.createCartesian(getX(), getY());
end;

// string toString(void)
// returns a string representation of the point as (X, Y)
function oPoint.toString() : string;
begin
    toString := '(' + IntToStr(getX) + ', ' + IntToStr(getY) + ')';
end;


end.

