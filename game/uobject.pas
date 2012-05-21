unit uobject;

{$mode objfpc}{$H+}

interface

uses
    // ontheair
    eventhandler, signal,
    // Home-baked classes
    utils,
    ugamesignals, upoint, ushape, uvector,
    // custom graphics lib
    BGRABitmap, BGRABitmapTypes,
    // stdlib
    Classes, SysUtils
    ;


type aObject = class
    protected
        _position:      oPoint;
        _mask:          oShape;
        _face:          TBGRABitmap;
        _score:         integer;
        _dispatcher:    oEventHandler;
        _id:            string;
        _collision_safe:boolean;

        function getDispatcher() : oEventHandler;

    public
        constructor create(position: oPoint; mask: oShape; face: TBGRABitmap;
                           dispatcher: oEventHandler);
        destructor destroy(); override;

        function isCollisionSafe() : boolean;

        function isColliding(o: aObject; var p: oPoint) : boolean; virtual;
        function isColliding(o: aObject) : boolean;

        procedure onCollision(si: oSignal); virtual; abstract;
        procedure onRedraw(si: oSignal); virtual;

        function collisionSignalFactory(sender: TObject;
                                        p: oPoint) : CollisionSignal;

        procedure draw(bm: TBGRABitmap); virtual;

        function getMask() : oShape;
        function getPosition() : oPoint;
        function getFace() : TBGRABitmap;

        function getId() : string;
end;

implementation

// Instance counter for object ID
// Dirty, but don't see how to do that in a clean way without too much hassle
var __object_count: integer = 0;

// create(position: oPoint, mask: oShape, face: TBGRABitmap;
//        dispatcher: oEventHandler)
// Creates an abstract object and sets position/mask/dispatcher accordingly
constructor aObject.create(position: oPoint; mask: oShape; face: TBGRABitmap;
                           dispatcher: oEventHandler);
var s: oSignal;
begin
    _position := oPoint.clone(position);
    _mask := mask;
    _score := 0;
    _dispatcher := dispatcher;
    _face := face;

    // Whether or not a collision with this object is considered "safe"
    // (i.e. doesn't influence the speed/position)
    _collision_safe := true;

    // Generate object ID, used to create a separate collision signal per
    // object instance (see ontheair/uniquesignal)
    _id := self.ClassName + '/' + IntToStr(__object_count);
    __object_count += 1;

    // Generate a CollisionSignal for this object, register it in the
    // dispatcher, and bind it to self.onCollision
    s := self.collisionSignalFactory(_dispatcher, position);
    _dispatcher.register(s);
    _dispatcher.bind(s, @self.onCollision);
    s.free();

    // Binds onRedraw to the Redraw signal. We assume it is already
    // registered.
    s := redrawSignal.create(_dispatcher);
    _dispatcher.bind(s, @self.onRedraw);
    s.free();
end;

// destroy()
// Well, destroy the shit
destructor aObject.destroy();
begin
    _face.free();
    _mask.free();
    _position.free();
    inherited;
end;

// boolean isCollisionSafe()
// Returns true if a collision with this object may update speed and/or
// position
function aObject.isCollisionSafe() : boolean;
begin
    isCollisionSafe := _collision_safe;
end;

// CollisionSignal collisionSignalFactory(sender: TObject; p: oPoint)
// Generates a CollisionSignal for this object (using the ID trick) at point p
function aObject.collisionSignalFactory(sender: TObject;
                                        p: oPoint) : CollisionSignal;
begin
    collisionSignalFactory := CollisionSignal.create(sender, _id, p);
end;

// draw(bm: BGRABitmap)
// Draws the object on the bitmap, at the right position
procedure aObject.draw(bm: TBGRABitmap);
begin
    // See BGRABitmap doc for more info
    bm.putImage(self.getPosition().getX(), self.getPosition().getY(),
                self.getFace(), dmDrawWithTransparency);
end;

// onRedraw(s: oSignal)
// Callback listening on RedrawSignal, handling display of the object
procedure aObject.onRedraw(si: oSignal);
var sig: RedrawSignal;
begin
    sig := si as RedrawSignal;
    d(5, _id, 'Redrawing');
    self.draw(sig.bm);
end;

// boolean isColliding(o: aObject, var p: oPoint)
// Returns true if o and self phisical matrixes overlap anywhere, given their
// respective positions in space (probably used in oPlayground).
// Put the coordinates of collision in `p'
function aObject.isColliding(o: aObject; var p: oPoint) : boolean;
var ich, dich: oShape;
    dx, dy: integer;
    i, j: integer;
    ref, sol, p1, p2: oPoint;
    k, q: real;
begin
    // Cartesian position deltas between the two relative referentials
    dx := o.getPosition().getX() - self.getPosition().getX();
    dy := o.getPosition().getY() - self.getPosition().getY();

    // To reduce computation time, we loop through the smallest object (in
    // area) Once we know which one it is, put its shape on ich (and the
    // other's in dich)
    // Also, ensure the deltas have the correct sign (they're relative)
    if   (o.getMask().getWidth() * o.getMask().getHeight())
       > (self.getMask().getWidth() * self.getMask().getHeight())
    then begin
        ich := self.getMask();
        dich := o.getMask();

        // Compute the coordinates deltas between the two object (they're
        // probably not at the same position)
        dx := +dx; 
        dy := +dy;

        // Reference to return the collision point with absolute coordinates
        ref := self.getPosition();
    end else begin
        ich := o.getMask();
        dich := self.getMask();

        // Compute the coordinates deltas between the two object (they're
        // probably not at the same position)
        dx := -dx;
        dy := -dy;

        // Reference to return the collision point with absolute coordinates
        ref := o.getPosition();
    end;

    // Assume there's no collision
    isColliding := false;

    q := 100; // edgeCoefficient to compare with

    sol := oPoint.clone(ref);
    p1 := oPoint.create(0, 0);
    p2 := oPoint.create(0, 0);

    // Not only check whether or not there's a collision, but also return the
    // "best" collision point by comparing edge coefficients
    // `ich' is the smalles object in area, so we loop through it
    for j := 0 to ich.getHeight() do begin  // Loop through ich's lines
        // If we're out of the other mask, no need to continue (we won't
        // collide with vacuum)
        if (j - dy) > dich.getHeight() then break;

        for i := 0 to ich.getWidth() do begin
            p1.setXY(i, j);
            p2.setXY(i - dx, j - dy);
            
            // If we're out of the other mask, no need to continue (again, we
            // won't collide with vacuum)
            if (i - dx) > dich.getWidth() then break;

            // Check if there's a collision at point (i, j)
            // Note that (i, j) are the coordinates in the relative ich's base
            // We need to translate the point to get the matching point in
            // dich (see the deltas calculated at the beginning)
            if ich.getPoint(p1) and dich.getPoint(p2) then begin
                // We got a collision, so we need to return true
                isColliding := true;

                // Here comes the second part of the job. We need to get the
                // best candidate for collision (the more "edgy")
                k := ich.edgeCoefficient(p1) * dich.edgeCoefficient(p2);
                if k < q then begin
                    // If this one looks better, we keep it
                    q := k;
                    sol.free();

                    // Copy the point on sol
                    sol := oPoint.clone(p1);
                end;
            end;
        end;
    end;

    // The collision point is on sol, we just need to return it with absolute
    // coordinates
    ref.apply(sol.position());
    p := ref;
end;

// boolean isColliding(o: aObject)
// Same as above, but drops tracking of collision point (mainly for backwards
// compatibility with the interface)
function aObject.isColliding(o: aObject) : boolean;
var p: oPoint;
begin
    isColliding := isColliding(o, p);
    p.free();
end;


// oShape getMask()
// Accessor for mask
function aObject.getMask() : oShape;
begin
    getMask := _mask;
end;


// oPoint getPosition()
// Accessor for position (returns a clone)
function aObject.getPosition() : oPoint;
begin
    getPosition := oPoint.clone(_position);
end;

// TBGRABitmap getFace()
// Accessor for face
function aObject.getFace() : TBGRABitmap;
begin
    getFace := _face;
end;

// oEventHandler getDispatcher()
// Accessor for dispatcher
function aObject.getDispatcher() : oEventHandler;
begin
    getDispatcher := _dispatcher;
end;

// string getId()
// Accessor for the object's unique identifier
function aObject.getId() : string;
begin
    getId := _id;
end;


end.
