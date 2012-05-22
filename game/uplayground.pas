unit uplayground;

{$mode objfpc}{$H+}{$M+}

interface

uses
    // ontheair
    eventhandler, signal,
    // home-baked classes
    ugamesignals,
        // objects
        uball, ubouncingobject, ubumper, ufield, uflipleft, uflipright, umovingobject, uobject, 
        // utils
        objectcollection, upoint, ushape, utils, uvector,
    // custom graphics lib
    BGRABitmap, BGRABitmapTypes,
    // stdlib
    Classes, Math, SysUtils
    ;

const
    // Number of balls on a game
    NB_LIFES = 3;

type oPlayground = class
    protected
        _world: TBGRABitmap;
        _ball: aMovingObject;
        _score: integer;
        _lifes: integer;
        _dispatcher: oEventHandler;

        _bottomright: oPoint;

        _objects: oObjectCollection;

        procedure populate();
        procedure move();

        function _isInZone(zp: oPoint; zv: oVector;
                           op: oPoint; ov: oVector) : boolean;

    public
        constructor create();
        destructor destroy(); override;

        function getObjectsInZone(p:oPoint; w,h: integer) : oObjectCollection;
        function isInZone(zp: oPoint; zv: oVector;
                          op: oPoint; ov: oVector) : boolean;

        procedure onScoreChange(si: oSignal);
        procedure onTick(si: oSignal);

        procedure tick();
        procedure flipLeft();
        procedure flipRight();

        procedure init();
        procedure start();
        procedure redraw();

        function getDispatcher() : oEventHandler;

end;


implementation

// create()
// Creates the playground. Registers signals, initializes values, populate the
// field, ...
constructor oPlayground.create();
var s: oSignal;
begin
    // The playground is in charge of creating the dispatcher we'll use among
    // the whole game
    _dispatcher := oEventHandler.create();
    // We'll store all the objects in an "object collection"
    _objects := oObjectCollection.create();

    /////////////////////////  REGISTER SIGNALS  /////////////////////////////

    // Registers and binds ScoreChangeSignal
    s := ScoreChangeSignal.create(_dispatcher, 0);
    _dispatcher.register(s);
    _dispatcher.bind(s, @self.onScoreChange);
    s.free();

    // registers DeathSignal
    s := DeathSignal.create(_dispatcher);
    _dispatcher.register(s);
    s.free();

    // Registers RedrawSignal
    s := RedrawSignal.create(_dispatcher);
    _dispatcher.register(s);
    s.free();

    // Registers FlipLeftSignal
    s := FlipLeftSignal.create(_dispatcher);
    _dispatcher.register(s);
    s.free();

    // Registers FlipRightSignal
    s := FlipRightSignal.create(_dispatcher);
    _dispatcher.register(s);
    s.free();

    // Registers TickSignal
    s := TickSignal.create(_dispatcher);
    _dispatcher.register(s);
    _dispatcher.bind(s, @self.onTick);
    s.free();

    // Initializes vars
    init();

    // Create objects
    populate();

    // Create the canvas
    _world := TBGRABitmap.create(_bottomright.getX(), _bottomright.getY(),
                                 BGRABlack);
end;

// destroy()
// Clean the mess before destroying the playground
destructor oPlayground.destroy();
var i: integer;
begin
    _dispatcher.free();
    for i := 0 to _objects.count() - 1 do _objects.get(i).free();

    _objects.free();
    _ball.free();
end;

// populate()
// Creates and adds the objects to the playground
procedure oPlayground.populate();
var o: aObject;
    p: oPoint;
begin
    // Map
    p := oPoint.create(0, 0);
    o := oField.create(p, _dispatcher);
    _objects.push(o);
    
    // We'll need the dimensions of the playground, so we get it from the
    // field size. This avoids hardcoding it
    _bottomright := oPoint.create(o.getMask().getWidth(),
                                  o.getMask().getHeight());


    p.setXY(340, 319);
    _objects.push(oBumper.create(p, _dispatcher));
    p.setXY(126, 375);
    _objects.push(oBumper.create(p, _dispatcher));
    //p.setXY(127, 40);
    //_objects.push(oBumper.create(p, _dispatcher));

    p.setXY(150, 390);
    _objects.push(oFlipLeft.create(p, _dispatcher));
    p.setXY(220, 390);
    _objects.push(oFlipRight.create(p, _dispatcher));

    p.setXY(380, 416);
    //p.setXY(235, 416);
    _ball := oBall.create(p, _dispatcher);
    //randomize();
    //_ball.setSpeed(oVector.createPolar(5, random(round(8*arctan(1)))));
    _ball.setSpeed(oVector.createPolar(60, -1.72052494347881));
    d(4, 'playground:populate', 'Added ball at ' + s(_ball.getPosition())
                              + ', with speed ' + s(_ball.getSpeed()));


    p.free();
end;

// init()
// (re-?)Initializes game values
procedure oPlayground.init();
begin
    _score := 0;
    _lifes := NB_LIFES;
end;

// move()
// Performs a move of the ball. This procedure is in charge of discretizing
// the path of the ball to avoid "passing through" objecs due to great speed
procedure oPlayground.move();
var i, n: integer;
    v: oVector;
begin
    // The dimensions are discrete (-> pixels), so the norm of the speed
    // vector gives the number of elementary moves to perform.
    n := round(_ball.getSpeed().getModule());
    d(5, 'playground', 'Path discretization, v = ' + s(_ball.getSpeed())
                     + '  (' + s(n) + ' steps)');
    for i := 1 to n do begin
        d(6, 'playground',  'Path discretization (' + s(i) + '/' + s(n)
                         + '). Ball at ' + s(_ball.getPosition()));

        // Performs the move
        // We could basically pass the whole objectcollection to the
        // procedure, bit this implies a lot of unnecessary computation sice
        // we can round it down at first by knowing the objects having
        // influence (well, matter) at the neighbourhood of the ball.
        _ball.elementaryMove(self.getObjectsInZone(
            _ball.getPosition(),
            _ball.getMask().getWidth(),
            _ball.getMask.getHeight()
        ));
    end;
end;

// onScoreChange(si: oSignal)
// callback triggered on score variations. Basically, it keeps the score
procedure oPlayground.onScoreChange(si: oSignal);
var sig: ScoreChangeSignal;
begin
    sig := si as ScoreChangeSignal;
    _score += sig.points;
end;

// redraw()
// Triggers a redraw of the whole playground
procedure oPlayground.redraw();
var sig: RedrawSignal;
    p: oPoint;
begin
    // First of all, we clear the canvas (may be unnecessary if the field
    // doesn't have transparent zones)
    _world.fillRect(0, 0, _bottomright.getX(), _bottomright.getY(),
                    BGRABlack, dmSet);

    // Create a RedrawSignal and emit it
    sig := RedrawSignal.create(self);
    sig.bm := _world;
    _dispatcher.emit(sig);

    sig.free();
end;

// oEventHandler getDispatcher()
// accessor for the dispatcher
function oPlayground.getDispatcher() : oEventHandler;
begin
    getDispatcher := _dispatcher;
end;

// onTick(si: oSignal)
// Callback signal triggered at every tick of the clock
procedure oPlayground.onTick(si: oSignal);
begin
    // Move the ball
    move();
    // Redraw the canvas
    redraw();
end;

// tick()
// Triggers a tick of they clock
procedure oPlayground.tick();
var s: oSignal;
begin
    // Well, create the signal and emit it
    s := TickSignal.create(self);
    _dispatcher.emit(s);
end;

// flipLeft()
// Toggle left flip up
procedure oPlayground.flipLeft();
var s: oSignal;
begin
    // Well, create the signal and emit it
    s := FlipLeftSignal.create(self);
    _dispatcher.emit(s);
end;

// flipRight()
// Toggle right flip up
procedure oPlayground.flipRight();
var s: oSignal;
begin
    // Well, create the signal and emit it
    s := FlipRightSignal.create(self);
    _dispatcher.emit(s);
end;

// start()
// Start the game
procedure oPlayground.start();
begin
    // Well, not sure whether or this is needed after all
end;

// oObjectCollection getObjectsInZone(p: oPoint; w, h: integer)
// Returns a collection of the objects having their influence zone on the
// rectangle defined by the three params
function oPlayground.getObjectsInZone(p: oPoint;
                                      w, h: integer) : oObjectCollection;
var i: integer;
    zv, zo: oVector;
begin
    // Create the collection
    getObjectsInZone := oObjectCollection.create();
    zv := oVector.createCartesian(w, h);
    zo := oVector.createCartesian(0, 0);

    // Add all objects overlaping the ball the the collection
    for i := 0 to _objects.count() - 1 do begin
        zo.setXY(_objects.get(i).getMask().getWidth(),
                 _objects.get(i).getMask().getHeight());
        if isInZone(p, zv, _objects.get(i).getPosition(), zo) then
            getObjectsInZone.push(_objects.get(i));
    end;
end;

// boolean isInZone(zp: oPoint; zv: oVector; op: oPoint; ov: oVector)
// Return true if the rectangle defined by the couple of points (op, op + ov)
// overlaps the rectangle defined by (zp, zp + zv)
function oPlayground.isInZone(zp: oPoint; zv: oVector;
                              op: oPoint; ov: oVector) : boolean;
begin
    isInZone := _isInZone(zp, zv, op, ov) or _isInZone(op, ov, zp, zv);
end;

// boolean _isInZone(zp: oPoint; zv: oVector; op: oPoint; ov: oVector)
// Return true if any of the corners of the rectangle defined by (op, op + ov)
// is in the rectangle (zp, zp + zv).
// To check if the two zones are overlaping, we need to check the corners of
// the first against the second, and the corners of the second agains the
// first. For the sake of readability, concision and maintainance, this
// convinience function does half the work needed, and the actual test is done
// in isInZone()
function oPlayground._isInZone(zp: oPoint; zv: oVector;
                   op: oPoint; ov: oVector) : boolean;
begin
    _isInZone := (false
        or (true  // Case 1: top left corner in zone
            and (op.getX() >= zp.getX())
            and (op.getX() <= zp.getX() + zv.getX())
            and (op.getY() >= zp.getY())
            and (op.getY() <= zp.getY() + zv.getY())
        )
        or (true  // Case 2: bottom right corner in zone
            and (op.getX() + ov.getX() >= zp.getX())
            and (op.getX() + ov.getX() <= zp.getX() + zv.getX())
            and (op.getY() + ov.getY() >= zp.getY())
            and (op.getY() + ov.getY() <= zp.getY() + zv.getY())
        )
        or (true  // Case 3: top right corner in zone
            and (op.getX() + ov.getX() >= zp.getX())
            and (op.getX() + ov.getX() <= zp.getX() + zv.getX())
            and (op.getY()             >= zp.getY())
            and (op.getY()             <= zp.getY() + zv.getY())
        )
        or (true  // Case 4: bottom left corner in zone
            and (op.getX()             >= zp.getX())
            and (op.getX()             <= zp.getX() + zv.getX())
            and (op.getY() + ov.getY() >= zp.getY())
            and (op.getY() + ov.getY() <= zp.getY() + zv.getY())
        )
    );
end;


end.