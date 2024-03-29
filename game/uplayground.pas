unit uplayground;

{$mode objfpc}{$H+}{$M+}

interface

uses
    // ontheair
    eventhandler, signal,
    // home-baked classes
    ugamesignals,
        // objects
        uball, ubouncingobject, ubumper, udeathpit, ufield, uflipleftright,
        ukicker, umovingobject, uobject, uplunger,
        // utils
        objectcollection, upoint, ushape, utils, uvector,
    // custom graphics lib
    BGRABitmap, BGRABitmapTypes,
    // stdlib
    Classes, Math, SysUtils
    ;

const
    // Number of balls on a game
    NB_LIFES = 300;
    NANO_REDRAW_TICKS = 20;

type oPlayground = class
    protected
        _world: TBGRABitmap;
        _ball: aMovingObject;
        _score: integer;
        _lifes: integer;
        _dispatcher: oEventHandler;

        _nanoticks: integer;

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
        procedure onDeath(si: oSignal);
        procedure onNanoRegister(si: oSignal);

        procedure tick();
        procedure flipLeft();
        procedure flipRight();
        procedure plungPull();
        procedure plungRelease();

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

    _nanoticks := 0;

    /////////////////////////  REGISTER SIGNALS  /////////////////////////////

    // Registers and binds ScoreChangeSignal
    s := ScoreChangeSignal.create(_dispatcher, 0);
    _dispatcher.register(s);
    _dispatcher.bind(s, @self.onScoreChange);
    s.free();

    // registers DeathSignal
    s := DeathSignal.create(_dispatcher);
    _dispatcher.register(s);
    _dispatcher.bind(s, @onDeath);
    s.free();

    // registers GameOverSignal
    s := GameOverSignal.create(_dispatcher);
    _dispatcher.register(s);
    s.free();

    // registers GameStartSignal
    s := GameStartSignal.create(_dispatcher);
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

    // Registers PlungerPullSignal
    s := PlungerPullSignal.create(_dispatcher);
    _dispatcher.register(s);
    s.free();

    // Registers PlungerReleaseSignal
    s := PlungerReleaseSignal.create(_dispatcher);
    _dispatcher.register(s);
    s.free();

    // Registers TickSignal
    s := TickSignal.create(_dispatcher);
    _dispatcher.register(s);
    _dispatcher.bind(s, @self.onTick);
    s.free();

    // Registers NanoTickSignal
    s := NanoTickRegisterSignal.create(_dispatcher, 0);
    _dispatcher.register(s);
    _dispatcher.bind(s, @self.onNanoRegister);
    s.free();

    // Registers NanoTickSignal
    s := NanoTickSignal.create(_dispatcher, 0, 0);
    _dispatcher.register(s);
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


    p.setXY(100, 100);
    _objects.push(oBumper.create(p, _dispatcher));
    p.setXY(278, 127);
    _objects.push(oBumper.create(p, _dispatcher));
    p.setXY(127, 40);
    _objects.push(oBumper.create(p, _dispatcher));
    p.setXY(354, 426);
    _objects.push(oBumper.create(p, _dispatcher));
    p.setXY(134, 129);
    _objects.push(oBumper.create(p, _dispatcher));


    p.setXY(122, 328);
    _objects.push(oKickerLeft.create(p, _dispatcher));
    p.setXY(260, 328);
    _objects.push(oKickerRight.create(p, _dispatcher));

    p.setXY(150, 400);
    _objects.push(oFlipLeft.create(p, _dispatcher));
    p.setXY(220, 400);
    _objects.push(oFlipRight.create(p, _dispatcher));

    p.setXY(0, 470);
    _objects.push(oDeathPit.create(p, _dispatcher));

    {p.setXY(260, 34);
    _objects.push(oGuide.create(p, 'bitmaps/kick-guide', _dispatcher, 10));
    p.setXY(243, 34);
    _objects.push(oGuide.create(p, 'bitmaps/kick-guide', _dispatcher, 10));}

    p.setXY(371, 435);
    //_objects.push(oPlunger.create(p, oVector.createPolar(60, -1.720524943478),
    _objects.push(oPlunger.create(p, oVector.createPolar(15, 2.967059),
                                  _dispatcher));

    p.setXY(374, 416);
    //p.setXY(235, 416);
    _ball := oBall.create(p, _dispatcher);
    d(4, 'playground:populate', 'Added ball at ' + s(_ball.getPosition())
                              + ', with speed ' + s(_ball.getSpeed()));


    p.free();
end;

// init()
// (re-?)Initializes game values
procedure oPlayground.init();
var sig: oSignal;
begin
    _score := 0;
    _lifes := NB_LIFES;
    sig := GameStartSignal.create(self);
    _dispatcher.emit(sig);
    sig.free();
end;

// move()
// Performs a move of the ball. This procedure is in charge of discretizing
// the path of the ball to avoid "passing through" objecs due to great speed
procedure oPlayground.move();
var i, ii, n, ticks, f: integer;
    v: oVector;
    sig: NanoTickSignal;
begin
    // The dimensions are discrete (-> pixels), so the norm of the speed
    // vector gives the number of elementary moves to perform.
    n := round(_ball.getSpeed().getModule());

    ticks := max(n, _nanoticks);
    sig := NanoTickSignal.create(self, 0, ticks);

    f := floor(ticks / n);


    d(5, 'playground', 'Path discretization, v = ' + s(_ball.getSpeed())
                     + '  (' + s(n) + ' steps)');
    ii := 0;
    for i := 1 to ticks do begin
        sig.i := i;
        _dispatcher.emit(sig);

        if (i mod f) = 0 then begin
            ii += 1;
            d(6, 'playground',  'Path discretization (' + s(ii) + '/' + s(n)
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

        //if (i mod NANO_REDRAW_TICKS) = 0 then redraw();
    end;

    sig.free();
end;

procedure oPlayground.onNanoRegister(si: oSignal);
var sig: NanoTickRegisterSignal;
begin
    sig := si as NanoTickRegisterSignal;

    if sig.n > _nanoticks then _nanoticks := sig.n;
end;

// onScoreChange(si: oSignal)
// callback triggered on score variations. Basically, it keeps the score
procedure oPlayground.onScoreChange(si: oSignal);
var sig: ScoreChangeSignal;
begin
    sig := si as ScoreChangeSignal;
    _score += sig.points;
end;

// onDeath(si: oSignal)
// callback triggered when ball dies
procedure oPlayground.onDeath(si: oSignal);
begin
    if _lifes > 0 then begin
        _lifes -= 1;
        _ball.setSpeed(oVector.createCartesian(0, 0));

        _ball.setPosition(oPoint.create(376, 348));
        //_ball.setPosition(oPoint.create(244, 123));
        if _lifes = 0 then begin
            _dispatcher.emit(GameOverSignal.create(self));
        end;
    end;
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
    s.free();

    _nanoticks := 0;
end;

// flipLeft()
// Toggle left flip up
procedure oPlayground.flipLeft();
var s: oSignal;
begin
    // Well, create the signal and emit it
    s := FlipLeftSignal.create(self);
    _dispatcher.emit(s);
    s.free();
end;

// flipRight()
// Toggle right flip up
procedure oPlayground.flipRight();
var s: oSignal;
begin
    // Well, create the signal and emit it
    s := FlipRightSignal.create(self);
    _dispatcher.emit(s);
    s.free();
end;

// plungPull()
// pulls the plunger
procedure oPlayground.plungPull();
var s: oSignal;
begin
    // Well, create the signal and emit it
    s := PlungerPullSignal.create(self);
    _dispatcher.emit(s);
    s.free();
end;

// plungRelease()
// releases the plunger
procedure oPlayground.plungRelease();
var s: oSignal;
begin
    // Well, create the signal and emit it
    s := PlungerReleaseSignal.create(self);
    _dispatcher.emit(s);
    s.free();
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