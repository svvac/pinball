unit ushape;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics, upoint, utils, math, BGRABitmap, BGRABitmapTypes;

const TAN_SAMPLE_HALF_WIDTH = 5;
      PATHFIND_DEPTH = 5;
      SHAPE_COLOR_FULL = 65536;
      ON_EDGE = 1;

Type oShape = Class
    protected
        _bm: TBGRABitmap;

    public
        constructor create(path:string);
        destructor destroy; override;

        function getPoint(p: oPoint) : boolean;
        function getPoint(x, y: integer) : boolean;

        function getSecantAngleAt(p: oPoint) : real;
        function getSecantAngleAt(p: oPoint; steps: integer) : real;
        function getNormalAngleAt(p: oPoint) : real;
        function getNormalAngleAt(p: oPoint; steps: integer) : real;

        function isOnEdge(p: oPoint) : boolean;
        function edgeCoefficient(p: oPoint) : real;

        function edgePathFind(where, src: oPoint; dir: integer; depth: integer) : oPoint;

        procedure rawDebugDump();

        function getWidth() : integer;
        function getHeight() : integer;
        //Procedure merge (s: oShape, p: oPoint);
        //Function hasoverlap(s: oShape);
  end;


implementation

// create(path: string)
// Loads a shape from bitmap file at `path'
constructor oShape.create(path:string);
begin
    _bm := TBGRABitmap.Create(path);
end;

// destroy(void)
// Deletes the bitmap from memory
destructor oShape.destroy;
begin
    _bm.free()
end;


// boolean getPoint(p: oPoint)
// returns true if the point `p' is solid on the shape, false otherwise.
function oShape.getPoint(p: oPoint) : boolean;
begin
    getPoint := getPoint(p.getX(), p.getY());
end;

// boolean getPoint(x, y: integer)
// returns true if the point at (`x', `y') is solid on the shape, false otherwise.
function oShape.getPoint(x, y: integer) : boolean;
var p: TBGRAPixel;
begin
    getPoint := false; // We return false if we're outside of the box defined by the shape
    if  (x >= 0) and (x <= getWidth() - 1)
    and (y >= 0) and (y <= getHeight() - 1)
        //then getPoint := (_bm.Canvas.Pixels[x, y] = SHAPE_COLOR_FULL);  // A point is solid if the associated pixel is white
        then begin
            //p := _bm.getPixel(x, y);
            //getPoint := ((p.red = BGRAWhite.red) and (p.green = BGRAWhite.green) and (p.blue = BGRAWhite.blue) and (p.alpha = BGRAWhite.alpha));
            getPoint := (_bm.getPixel(x, y) = BGRAWhite);
        end;
end;

// integer getWidth(void)
// returns the width of the shape
function oShape.getWidth() : integer;
begin
     getWidth := _bm.width;
end;

// integer getHeight(void)
// returns the height of the shape
function oShape.getHeight() : integer;
begin
     getHeight := _bm.height;
end;

// boolean isOnEdge(p: oPoint)
// returns true if the point p is considered on the edge of the shape.
function oShape.isOnEdge(p: oPoint) : boolean;
begin
    // The best way I found is to count the solid points in the direct neighbourhood of the point
    if edgeCoefficient(p) < ON_EDGE then isOnEdge := true
                                    else isOnEdge := false;
end;

function oShape.edgeCoefficient(p: oPoint) : real;
var s, i, j: integer;
begin
    s := 0;
    for i := p.getX() - 1 to p.getX() + 1 do
        for j := p.getY() - 1 to p.getY() + 1 do
            if getPoint(i, j) then s += 1;

    edgeCoefficient := abs(s - 4.5) / 2;
end;

// real getSecantAngleAt(p: oPoint)
// returns the angle of the tangent with the vertical at point p
function oShape.getSecantAngleAt(p: oPoint; steps: integer) : real;
var q, r: oPoint;
    i: integer;

    a1, a2: real;
begin
    getSecantAngleAt := 0.0;
    a1 := 0;
    a2 := 0;

    q := oPoint.clone(p); r := oPoint.clone(p);
    for i := 1 to steps do begin
        q := edgePathFind(q, p, +1, PATHFIND_DEPTH);
        r := edgePathFind(r, p, -1, PATHFIND_DEPTH);
        a1 += (q.getX() - p.getX()) * (q.getY() - p.getY());
        a1 += (r.getX() - p.getX()) * (r.getY() - p.getY());
        a2 += (q.getX() - p.getX()) * (q.getX() - p.getX());
        a2 += (r.getX() - p.getX()) * (r.getX() - p.getX());
    end;

    //writeln('shape: Computing tangent angle at ' + p.toString() + ' with points ' + q.toString() + ' and ' + r.toString());

    getSecantAngleAt := a1 / a2;
    
    //if not q.sameAs(r) then getSecantAngleAt := arctan2(q.getX() - r.getX(), q.getY() - r.getY());
end;

function oShape.getNormalAngleAt(p: oPoint; steps: integer) : real;
begin
    getNormalAngleAt := getSecantAngleAt(p, steps) - 2 * arctan(1);
end;

function oShape.getSecantAngleAt(p: oPoint) : real;
begin
    getSecantAngleAt := getSecantAngleAt(p, TAN_SAMPLE_HALF_WIDTH);
end;

function oShape.getNormalAngleAt(p: oPoint) : real;
begin
    getNormalAngleAt := getNormalAngleAt(p, TAN_SAMPLE_HALF_WIDTH);
end;

// oPoint edgePathFind(where, src: oPoint, dir: integer, depth: integer)
// Walks the edge of the figure in the direction `dir' from `where' and returns a solid point.
//    where: the point from where we search
//    src:   the original origin of the walk (we'll manage to return an other point than this one)
//    dir:   the direction of the walk. If positive, we search on the right of `where', and on the left otherwise
//    depth: the number of recursive calls to make if a given point isn't on the edge
function oShape.edgePathFind(where, src: oPoint; dir: integer; depth: integer) : oPoint;
var p, q: oPoint;
    list:  array [0 .. 3] of oPoint;
    coef, k: real;
    i: integer;
begin
    // If n point found (e.g. not on the edge), we default to the origin of the search
    edgePathFind := where;
    coef := 10000;

    // We only walk the shape if we've got some levels of recursion left
    if depth > 0 then begin
        // Instead of looping through the adjacent points, we define the ones inspected depending on the direction.
        if dir > 0 then begin
            // O = where ; X = inspected points ; . ignored points
            //    . . . . .
            //    . . . X .
            //    . . 0 X .
            //    . . X X .
            //    . . . . . 
            list[0] := oPoint.create(where.getX + 1, where.getY - 1);
            list[1] := oPoint.create(where.getX + 1, where.getY);
            list[2] := oPoint.create(where.getX + 1, where.getY + 1);
            list[3] := oPoint.create(where.getX, where.getY + 1);
        end else begin
            // O = where ; X = inspected points ; . ignored points
            //    . . . . .
            //    . X X . .
            //    . X 0 . .
            //    . X . . .
            //    . . . . .
            list[0] := oPoint.create(where.getX - 1, where.getY - 1);
            list[1] := oPoint.create(where.getX - 1, where.getY);
            list[2] := oPoint.create(where.getX - 1, where.getY + 1);
            list[3] := oPoint.create(where.getX, where.getY - 1);
        end;
        for p in list do begin
            // ignore the source and the origin
            if p.sameAs(where) or p.sameAs(src) then continue;
            k := edgeCoefficient(p);
            // if we're not on the edge of the shape, make a recursive call
            //if not isOnEdge(p) then begin
                //q := edgePathFind(p, where, dir, depth - 1);
                //k *= edgeCoefficient(q);

                //writeln('shape:                 k''=' + FloatToStr(k) + ' at ' + q.toString());

                // If the result is on the edge and not the subserach point, we keep it
                if (k < coef) and not src.sameAs(p) then begin
                    coef := k;
                    edgePathFind := p;
               end;
            // If we're on the edge, we got a winner!
            //end else if k < coef then begin
            //    coef := k;
            //    edgePathFind := p;
            //end;
        end;

        writeln('shape: Got a winner at ' + edgePathFind.toString() + ' with coef k=' + FloatToStr(coef));
    end;
end;

procedure oShape.rawDebugDump();
var i, j: integer;
begin
    for j := 0 to getHeight() - 1 do begin
        for i := 0 to getWidth() - 1 do
            if getPoint(i, j) then write('#') else write(' ');
        writeln;
    end;
end;

end.


