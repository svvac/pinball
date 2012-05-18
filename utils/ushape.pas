unit ushape;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics, upoint, utils, math, BGRABitmap, BGRABitmapTypes;

const TAN_SAMPLE_HALF_WIDTH = 2;
      PATHFIND_DEPTH = 2;
      SHAPE_COLOR_FULL = 65536;

Type oShape = Class
    protected
        _bm: TBGRABitmap;

        function edgePathFind(where, src: oPoint; dir: integer; depth: integer) : oPoint;

    public
        constructor create(path:string);
        destructor destroy; override;

        function getPoint(p: oPoint) : boolean;
        function getPoint(x, y: integer) : boolean;
        function getTangentAngleAt(p: oPoint) : real;
        function isOnEdge(p: oPoint) : boolean;

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
var s, i, j: integer;
begin
    isOnEdge := false;

    // To be on the edge, a point must be solid
    if getPoint(p) then begin
        s := 0;
        for i := p.getX() - 1 to p.getX() + 1 do
            for j := p.getY() - 1 to p.getY() + 1 do
                if getPoint(i, j) then s += 1;
        
        // The best way I found is to count the solid points in the direct neighbourhood of the point
        if (s > 2) and (s < 8) then isOnEdge := true
                               else isOnEdge := false;
    end;
end;

// real getTangentAngleAt(p: oPoint)
// returns the angle of the tangent with the vertical at point p
function oShape.getTangentAngleAt(p: oPoint) : real;
var q, r: oPoint;
    i: integer;
begin
    getTangentAngleAt := 0.0;

    q := edgePathFind(p, p, +1, PATHFIND_DEPTH);
    r := edgePathFind(p, p, -1, PATHFIND_DEPTH);

    for i := 1 to TAN_SAMPLE_HALF_WIDTH do begin
        q := edgePathFind(q, p, +1, PATHFIND_DEPTH);
        r := edgePathFind(r, p, -1, PATHFIND_DEPTH);
    end;
    
    if not q.sameAs(r) then getTangentAngleAt := arctan2(q.getX() - r.getX(), q.getY() - r.getY()) + 2 * arctan(1);
end;

// oPoint edgePathFind(where, src: oPoint, dir: integer, depth: integer)
// Walks the edge of the figure in the direction `dir' from `where' and returns a solid point.
//    where: the point from where we search
//    src:   the original origin of the walk (we'll manage to return an other point than this one)
//    dir:   the direction of the walk. If positive, we search on the right of `where', and on the left otherwise
//    depth: the number of recursive calls to make if a given point isn't on the edge
function oShape.edgePathFind(where, src: oPoint; dir: integer; depth: integer) : oPoint;
var p, q: oPoint;
    list: array [0 .. 3] of oPoint;
begin
    // If n point found (e.g. not on the edge), we default to the origin of the search
    edgePathFind := where;

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
            // if we're not on the edge of the shape, make a recursive call
            if not isOnEdge(p) then begin
               q := edgePathFind(p, where, dir, depth - 1);
               // If the result is on the edge and not the subserach point, we keep it
               if isOnEdge(q) and not q.sameAs(p) then begin
                   edgePathFind := q;
                   break;
               end;
            // If we're on the edge, we got a winner!
            end else begin
                edgePathFind := p;
                break;
            end;
        end;
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


