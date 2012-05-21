unit ushape;

{$mode objfpc}{$H+}

interface

uses
    // custom graphics lib
    BGRABitmap, BGRABitmapTypes,
    // Home-baked units
    utils, upoint,
    // std lib
    Classes, Graphics, Math, SysUtils
    ;

const
    // Sample to consider when approximating secant 
    TAN_SAMPLE_HALF_WIDTH = 5;
    // Maximum recursion level for edgePathFind
    PATHFIND_DEPTH = 5;
    // Boundary for being considered "on edge" (see isOnEdge)
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

        function edgePathFind(where, src: oPoint;
                              dir: integer;
                              depth: integer) : oPoint;

        procedure rawDebugDump();

        function getWidth() : integer;
        function getHeight() : integer;
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
// returns true if the point at (`x', `y') is solid on the shape, false
// otherwise.
function oShape.getPoint(x, y: integer) : boolean;
var p: TBGRAPixel;
begin
    getPoint := false; // We return false if we're outside of the box defined
                       // by the shape
    // Ensure we're on the box (no need to check otherwise)
    if  true
        and (x >= 0)    and (x <= getWidth() - 1)
        and (y >= 0)    and (y <= getHeight() - 1)
        // A point is solid if the associated pixel on the mask is white
        then getPoint := (_bm.getPixel(x, y) = BGRAWhite);
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
    // We consider the point on the edge if the edge coefficient is lower than
    // the defined boundary
    if edgeCoefficient(p) < ON_EDGE then isOnEdge := true
                                    else isOnEdge := false;
end;

// real edgeCoefficient(p: oPoint)
// returns the (float) coefficient characterizing the "proximity" of `p' to an
// edge of a shape
function oShape.edgeCoefficient(p: oPoint) : real;
var s, i, j: integer;
begin
    // Basically, count the number of solid pixels in the direct neighbourhood
    // of the point
    s := 0;
    for i := p.getX() - 1 to p.getX() + 1 do
        for j := p.getY() - 1 to p.getY() + 1 do
            if getPoint(i, j) then s += 1;

    // Then apply a magic formula. The factor is mostly arbitrary, and 4.5 is
    // the count of solid points if half the neighbours were solid.
    edgeCoefficient := abs(s - 4.5) / (2 * ON_EDGE);
end;

// real getSecantAngleAt(p: oPoint; steps: integer)
// returns the angle of the secant with the horizontal at point p, with a
// sample of +/- `steps'
function oShape.getSecantAngleAt(p: oPoint; steps: integer) : real;
var q, r: oPoint;
    i: integer;
    a1, a2: real;
begin
    getSecantAngleAt := 0.0;
    a1 := 0.0;
    a2 := 0.0;

    // Magic tangent formula on a curve :
    // Discovered a sunday morning, while eating cornflakes and reading a
    // paper wrote by three chinese students of don't-know-where university
    // (somewhere lost in the middle of a rice field).
    // From the bottom of my heart: thanks guys.
    //
    //            k+s
    //           \‾‾‾
    //            >    (x_j - x_k)(y_j - y_k)
    //           /___
    //           j=k-s
    // α(k) = ──────────────────────────────────
    //                k+s
    //               \‾‾‾
    //                >    (x_j - x_k)²
    //               /___
    //               j=k-s
    //
    q := oPoint.clone(p); r := oPoint.clone(p);
    for i := 1 to steps do begin
        // We walk on the edge of the shape
        q := edgePathFind(q, p, +1, PATHFIND_DEPTH);
        r := edgePathFind(r, p, -1, PATHFIND_DEPTH);

        // Apply the yellow formula
        a1 += (q.getX() - p.getX()) * (q.getY() - p.getY());
        a1 += (r.getX() - p.getX()) * (r.getY() - p.getY());
        a2 += (q.getX() - p.getX()) * (q.getX() - p.getX());
        a2 += (r.getX() - p.getX()) * (r.getX() - p.getX());

        d(15, 'shape', s(i) + '/' + s(steps) + ':' + s(p) + ' ' + s(q) + ' '
                     + s(r) + ' ' + s('alpha') + '=' + s(a1) + '/' + s(a2)
                     + '=' + s(getSecantAngleAt));
    end;

    // Finally divide the two sums
    getSecantAngleAt := a1 / a2;

    d(9, 'shape', 'Computing secant angle at ' + s(p) + ' with points ' + s(q)
                + ' and ' + s(r) + ' ; α=' + s(a1) + '/' + s(a2) + '='
                + s(getSecantAngleAt));
end;

// real getNormalAngleAt(p: oPoint; steps: integer)
// returns the angle of the normal with the horizontal at point p, with a
// sample of +/- `steps'.
function oShape.getNormalAngleAt(p: oPoint; steps: integer) : real;
begin
    // Simply enough, we get the secant, and add π/2
    getNormalAngleAt := getSecantAngleAt(p, steps) + 2 * arctan(1);
end;

// real getNormalAngleAt(p: oPoint)
// returns the angle of the normal with the horizontal at point p, with the
// default sample width.
function oShape.getSecantAngleAt(p: oPoint) : real;
begin
    getSecantAngleAt := getSecantAngleAt(p, TAN_SAMPLE_HALF_WIDTH);
end;

// real getNormalAngleAt(p: oPoint)
// returns the angle of the normal with the horizontal at point p, with the
// default sample width.
function oShape.getNormalAngleAt(p: oPoint) : real;
begin
    getNormalAngleAt := getNormalAngleAt(p, TAN_SAMPLE_HALF_WIDTH);
end;

// oPoint edgePathFind(where, src: oPoint, dir: integer, depth: integer)
// Walks the edge of the figure in the direction `dir' from `where' and
// returns a solid point.
//    where: the point from where we search ;
//    src:   the original origin of the walk (we'll manage to return an other
//           point than this one) ;
//    dir:   the direction of the walk. If positive, we search on the right of
//           `where', and on the left otherwise ;
//    depth: the number of recursive calls to make if a given point isn't on
//           the edge.
//           NOT USED ANYMORE. Will be removed.
function oShape.edgePathFind(where, src: oPoint;
                             dir: integer;
                             depth: integer) : oPoint;
var p, q: oPoint;
    list:  array [0 .. 3] of oPoint;
    coef, k: real;
    i: integer;
begin
    // If n point found (e.g. not on the edge), we default to the origin of
    // the search
    edgePathFind := where;
    coef := 10000;

    // We only walk the shape if we've got some levels of recursion left
    if depth > 0 then begin
        // Instead of looping through the adjacent points, we define the ones
        // inspected depending on the direction.
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

            // We get the edge coef of the point, and we keep the lowest score
            k := edgeCoefficient(p);
            if (k < coef) and not src.sameAs(p) then begin
                coef := k;
                edgePathFind := p;
            end;
        end;

        d(13, 'shape', 'Got a winner at ' + s(edgePathFind) + ' with coef k='
                     + s(coef));
    end;
end;

// rawDebugDump()
// Prints a ASCII representation of the binary mask. Used for debug purposes
// only.
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


