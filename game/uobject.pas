unit uobject;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics, eventhandler, upoint, ushape, signal, ugamesignals;


type aObject = class
    protected
        _position:      oPoint;
        _mask:          oShape;
        _face:          TBitmap;
        _score:         integer;
        _dispatcher:    oEventHandler;
        _id:            string;

        function getDispatcher() : oEventHandler;

    public
        constructor create(position: oPoint; mask: oShape; face: TBitmap; dispatcher: oEventHandler);
        destructor destroy(); override;

        function isColliding(o: aObject) : boolean; virtual;

        procedure onCollision(s: oSignal); virtual; abstract;
        procedure onRedraw(s: oSignal); virtual;

        function collisionSignalFactory(sender: TObject; p: oPoint) : oSignal;

        procedure draw(cv: TCanvas); virtual;

        function getMask() : oShape;
        function getPosition() : oPoint;
        function getFace() : TBitmap;
end;

implementation

// Instance counter for object ID
// Dirty, but don't see how to do that in a clean way without too much hassle
var __object_count: integer = 0;

// create(position: oPoint, mask: oShape, dispatcher: oEventHandler)
// Creates an abstract object and sets position/mask/dispatcher accordingly
constructor aObject.create(position: oPoint; mask: oShape; face: TBitmap; dispatcher: oEventHandler);
var s: oSignal;
begin
    _position := position;
    _mask := _mask;
    _score := 0;
    _dispatcher := dispatcher;
    _face := face;

    // Generate object ID, used to create a separate collision signal per object instance
    _id := self.ClassName + IntToStr(__object_count);
    __object_count += 1;

    // Generate a CollisionSignal for this object, register it in the dispatcher, and
    // bind it to self.onCollision
    s := self.collisionSignalFactory(_dispatcher, position);
    _dispatcher.register(s);
    _dispatcher.bind(s, @self.onCollision);
    s.free();

    // Binds onRedraw to the Redraw signal. We assume it is already registered.
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

// Generates a CollisionSignal for this object (using the ID trick)
function aObject.collisionSignalFactory(sender: TObject; p: oPoint) : oSignal;
begin
    collisionSignalFactory := CollisionSignal.create(sender, _id, p);
end;

// draw(cv: TCanvas)
// Draws the object on cv
procedure aObject.draw(cv: TCanvas);
begin
    cv.draw(self.getPosition().getX(), self.getPosition().getY(), self.getFace());
end;

// onRedraw(s: oSignal)
// Redraws the object on the canvas
procedure aObject.onRedraw(s: oSignal);
var sig: RedrawSignal;
begin
    sig := s as RedrawSignal;
    self.draw(sig.bm.canvas);
end;

// isColliding(o: aObject) : boolean
// Returns true if o and self phisical matrixes overlap anywhere, given their respective
// positions in space (probably used in oPlayground)
function aObject.isColliding(o: aObject) : boolean;
var ich, dich: oShape;
    dx, dy: integer;
    i, j: integer;
begin
    // Cartesian position deltas
    dx := o.getPosition().getX() - self.getPosition().getX();
    dy := o.getPosition().getY() - self.getPosition().getY();

    // To reduce computation time, we loop through the smallest object (in area)
    // Once we know which one it is, put its shape on ich (and the other's in dich)
    // Also, ensure the deltas have the correct sign (they're relative)
    if (o.getMask().getWidth() * o.getMask().getHeight())
     > (self.getMask().getWidth() * self.getMask().getHeight())
    then begin
        ich := self.getMask(); dich := o.getMask();
        dx := +dx;
        dy := +dy;
    end else begin
        ich := o.getMask(); dich := self.getMask();
        dx := -dx;
        dy := -dy;
    end;

    // Assume there's no collision
    isColliding := false;

    // We exit the loop as soon as we got a collision : no need to look further
    for j := 0 to ich.getHeight() do begin  // Loop through ich's lines
        if isColliding then break;  // Exit if collision detected
        if (j + dy) > dich.getHeight() then break;  // Exit if we're out of dich's mask (there won't be any collisions)
        for i := 0 to ich.getWidth() do begin  // Loop through ich's columns
            if isColliding then break;  // Exit if collision detected
            if (j + dy) > dich.getWidth() then break;  // Exit if we're out of dich's mask (there won't be any collisions)

            isColliding := (ich.getPoint(i, j) = dich.getPoint(i, j));  // Check for collision at (i, j)
        end;
    end;
    
end;


// getMask() : oShape
// Accessor for mask
function aObject.getMask() : oShape;
begin
    getMask := _mask;
end;


// getPosition() : oPoint
// Accessor for position
function aObject.getPosition() : oPoint;
begin
    getPosition := oPoint.clone(_position);
end;

// getFace() : TBitmap
// Accessor for face
function aObject.getFace() : TBitmap;
begin
    getFace := _face;
end;

// getDispatcher() : oEventHandler
// Accessor for dispatcher
function aObject.getDispatcher() : oEventHandler;
begin
    getDispatcher := _dispatcher;
end;



end.
