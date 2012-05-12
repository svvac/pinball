unit uvector;

{$mode objfpc}{$H+}

interface

uses Classes, math;

type oVector=class
    protected
        _x, _y: integer;
        _radius, _angle: real;

        procedure updatePolar();
        procedure updateCartesian();

    public

        constructor createCartesian(ax, ay : integer);
        constructor createPolar(m, a : real);
        constructor clone(o: oVector);

        procedure setX(ax : integer);
        procedure setY(ay : integer);
        procedure setModule(m : real);
        procedure setArgument(a : real);


        function getX() : integer;
        function getY() : integer;
        function getModule() : real;
        function getArgument() : real;
end;


implementation

// createCartesian(x, y: integer)
// Creates a vector by giving its cartesian coordinates
constructor oVector.createCartesian(ax, ay: integer);
begin
    _x := ax;
    _y := ay;
    updatePolar();  // Updates polar coordinates accordingly
end;

// createPolar(m, a: real)
// Creates a vector by giving its polar coordinates (angle, module)
constructor oVector.createPolar(m, a : real);
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
procedure oVector.setX(ax : integer);
begin
    _x := ax;
    updateCartesian();
end;

// setY(x: integer)
// Updates Y cartesian coordinate
procedure oVector.setY(ay : integer);
begin
    _y := ay;
    updateCartesian();
end;

// setModule(x: integer)
// Updates module of polar coordinates
procedure oVector.setModule(m : real) ;
begin
    _radius := m;
    updatePolar();
end;

// setArgument(x: integer)
// Updates argument of polar coordinates
procedure oVector.setArgument(a : real);
begin
    _angle := a;
    updatePolar();
end;

// updatePolar()
// Updates polar coordinates after a change in cartesians
procedure ovector.updatePolar() ;
begin
    // Boring math stuff. See OMSI or shit
    _radius := sqrt(_x*_x + _y*_y);
    _angle := arctan2(_y, _x);
end;

// updateCartesian()
// Updates cartesian coordinates after a change in polars
procedure  ovector.updateCartesian();
begin
    // Boring math stuff. See OMSI or shit
    _x := round(_radius * cos(_angle));
    _y := round(_radius * sin(_angle));
end;

// getX() : integer
// returns X cartesian coordinate
function ovector.GetX() : integer;
begin
    getX := _x;
end;

// getY() : integer
// returns Y cartesian coordinate
function ovector.getY() : integer ;
begin
    getY := _y;
end;

// getModule() : real
// returns module from polar coord
function ovector.getModule() : real ;
begin
    getModule:=_radius;
end;

// getArgument() : real
// returns argument from polar coord
function ovector.getArgument() : real  ;
begin
    getArgument:=_angle;
end;

end.
