unit uvector;

{$mode objfpc}{$H+}

interface

uses
    // home-baked units
    printable, utils,
    // stdlib
    Classes, Math, SysUtils
    ;

const
    ROUND_THRESHOLD = 0.2;

type
// Core implementation (not public)
_oVector=class
    protected
        _x, _y: real;
        _radius, _angle: real;

        procedure updatePolar();
        procedure updateCartesian();

    public

        constructor createCartesian(ax, ay : integer);
        constructor createPolar(m, a : real);

        procedure setX(ax : integer);
        procedure setY(ay : integer);
        procedure setXY(x, y: integer);
        procedure setModule(m : real);
        procedure setArgument(a : real);

        procedure sum(v: _oVector);
        procedure diff(v: _oVector);
        procedure factor(k: real);


        function getX() : integer;
        function getY() : integer;
        function getRX() : real;
        function getRY() : real;
        function getModule() : real;
        function getArgument() : real;
end;

// Vendor class, implements Printable interface
oVector = class(_oVector, iPrintable)
    constructor clone(o: oVector);
    function toString() : string;
end;


implementation

// createCartesian(x, y: integer)
// Creates a vector by giving its cartesian coordinates
constructor _oVector.createCartesian(ax, ay: integer);
begin
    _x := ax;
    _y := ay;
    updatePolar();  // Updates polar coordinates accordingly
end;

// createPolar(m, a: real)
// Creates a vector by giving its polar coordinates (angle, module)
constructor _oVector.createPolar(m, a : real);
begin
    _radius := m;
    _angle := a;
    updateCartesian();  // Updates cartesians accordingly
end;

// clone(o: oVector)
// Creates a new vector by cloning o
constructor oVector.clone(o: oVector);
begin
    _radius := o.getModule();
    _angle := o.getArgument();
    updateCartesian();
end;

// setX(x: integer)
// Updates X cartesian coordinate
procedure _oVector.setX(ax : integer);
begin
    _x := ax;
    updatePolar();
end;

// setY(x: integer)
// Updates Y cartesian coordinate
procedure _oVector.setY(ay : integer);
begin
    _y := ay;
    updatePolar();
end;

// setXY(x, y: integer)
// Updates X and Y cartesian coordinates
procedure _oVector.setXY(x, y : integer);
begin
    _x := x;
    _y := y;
    updatePolar();
end;

// setModule(x: integer)
// Updates module of polar coordinates
procedure _oVector.setModule(m : real) ;
begin
    _radius := m - int(m / (8 * arctan(1))) * 8 * arctan(1);
    updateCartesian();
end;

// setArgument(x: integer)
// Updates argument of polar coordinates
procedure _oVector.setArgument(a : real);
begin
    _angle := a;
    updateCartesian();
end;

// sum(v: oVector)
// Adds v to the vector
procedure _oVector.sum(v: _oVector);
begin
    _x += v.getRX();
    _y += v.getRY();
    updatePolar();
end;

// diff(v: oVector)
// Substracts v to the vector
procedure _oVector.diff(v: _oVector);
begin
    _x -= v.getRX();
    _y -= v.getRY();
    updatePolar();
end;

// factor(k: real)
// Apply the factor k to the vector's norm
procedure _oVector.factor(k: real);
begin
    _radius *= k;
    updateCartesian();
end;

// updatePolar()
// Updates polar coordinates after a change in cartesians
procedure _oVector.updatePolar() ;
begin
    // Boring math stuff. See OMSI or shit
    _radius := sqrt(_x*_x + _y*_y);
    _angle := arctan2(_y, _x);
end;

// updateCartesian()
// Updates cartesian coordinates after a change in polars
procedure  _oVector.updateCartesian();
begin
    // Boring math stuff. See OMSI or shit
    _x := _radius * cos(_angle);
    _y := _radius * sin(_angle);
end;

// integer getX()
// returns X cartesian coordinate
function _oVector.GetX() : integer;
begin
    getX := smartround(_x, ROUND_THRESHOLD);
end;

// integer getY()
// returns Y cartesian coordinate
function _oVector.getY() : integer ;
begin
    getY := smartround(_y, ROUND_THRESHOLD);
end;

// integer getRX()
// returns X cartesian real coordinate
function _oVector.GetRX() : real;
begin
    getRX := _x;
end;

// integer getRY()
// returns Y cartesian real coordinate
function _oVector.getRY() : real ;
begin
    getRY := _y;
end;

// real getModule()
// returns module from polar coord
function _oVector.getModule() : real ;
begin
    getModule := _radius;
end;

// getArgument() : real
// returns argument from polar coord
function _oVector.getArgument() : real  ;
begin
    getArgument := _angle;
end;

// string toString()
// returns the string representation of the vector
function oVector.toString() : string;
begin
    toString := '[(x=' + s(getX()) + ', y=' + s(getY()) + ') = (' + s('rho')
              + '=' + s(getModule) + ', ' + s('theta') + '='
              + s(getArgument()) + ' rad)]';
end;

end.
