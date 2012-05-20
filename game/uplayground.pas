unit uplayground;

{$mode objfpc}{$H+}{$M+}

interface

uses Classes, SysUtils, Graphics, utils, upoint, uobject, math, objectcollection, eventhandler, signal, ugamesignals, ubouncingobject, uvector, uguide, umovingobject, ushape, BGRABitmap, BGRABitmapTypes;

CONST NB_LIFES = 3;

type oPlayground = class
    protected
        _world: TBGRABitmap;
        _ball: aMovingObject;
        _score: integer;
        _lifes: integer;
        _dispatcher: oEventHandler;

        _objects: oObjectCollection;

        procedure populate();
        procedure move();

    public
        constructor create();
        destructor destroy(); override;

        function getObjectsInZone(p:oPoint; w,h: integer) : oObjectCollection;

        procedure onScoreChange(si: oSignal);
        procedure onTick(si: oSignal);

        procedure tick();

        procedure init();
        procedure start();
        procedure redraw();

        function getDispatcher() : oEventHandler;

end;


implementation


constructor oPlayground.create();
var s: oSignal;
begin
    _dispatcher := oEventHandler.create();
    _world := TBGRABitmap.create(440, 490, BGRABlack);
    _objects := oObjectCollection.create();

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

    // Registers TickSignal
    s := TickSignal.create(_dispatcher);
    _dispatcher.register(s);
    _dispatcher.bind(s, @self.onTick);
    s.free();

    init();

    populate();

end;

destructor oPlayground.destroy();
begin
end;

procedure oPlayground.populate();
var bo: aBouncingObject;
    shape: oShape;
    p: oPoint;
    bm: TBGRABitmap;
    g: oGuide;
    s1, s2: integer;
begin
    // Map
    p := oPoint.create(0, 0);
    shape := oShape.create('bitmaps/canvas.bmp');
    bm := TBGRABitmap.create('bitmaps/canvas.png');
    bo := aBouncingObject.create(p, shape, bm, _dispatcher, 1);
    d(4, 'playground:populate', 'Added canvas at ' + bo.getPosition().toString());
    _objects.push(bo);

    //p.setXY(384, 416);
    p.setXY(235, 416);
    shape := oShape.create('bitmaps/ball.bmp');
    bm := TBGRABitmap.create('bitmaps/ball.png');
    _ball := aMovingObject.create(p, shape, bm, _dispatcher);
    randomize();
    _ball.setSpeed(oVector.createPolar(5, random(round(8*arctan(1)))));
    //_ball.setSpeed(oVector.createPolar(1, -2*arctan(1)));
    d(4, 'playground:populate', 'Ball at ' + s(_ball.getPosition()) + ', with speed ' + s(_ball.getSpeed()));

    p.setXY(260, 34);
    g := oGuide.create(p, 'bitmaps/kick-guide', _dispatcher, -10);
    _objects.push(g);

    p.free();
end;

procedure oPlayground.init();
begin
    _score := 0;
    _lifes := NB_LIFES;
end;

procedure oPlayground.move();
var i, n: integer;
    v: oVector;
begin
    n := round(_ball.getSpeed().getModule());
    d(5, 'playground', 'Path discretization, v = ' + s(_ball.getSpeed()) + '  (' + s(n) + ' steps)');
    for i := 1 to n do begin
        d(6, 'playground',  'Path discretization (' + s(i) + '/' + s(n) + '). Ball at ' + s(_ball.getPosition()));
        _ball.elementaryMove(self.getObjectsInZone(_ball.getPosition(), _ball.getMask().getWidth(), _ball.getMask.getHeight()));
    end;
end;

procedure oPlayground.onScoreChange(si: oSignal);
var sig: ScoreChangeSignal;
begin
    sig := si as ScoreChangeSignal;
    _score += sig.points;
end;

procedure oPlayground.redraw();
var sig: RedrawSignal;
    p: oPoint;
begin
    sig := RedrawSignal.create(self);
    //sig.bm := TBGRABitmap.create(350, 600, BGRABlack);
    _world.fillRect(0, 0, 440, 490, BGRABlack, dmSet);
    sig.bm := _world;
    _dispatcher.emit(sig);

    //sig.bm.free();
    sig.free();
end;

function oPlayground.getDispatcher() : oEventHandler;
begin
    getDispatcher := _dispatcher;
end;

procedure oPlayground.onTick(si: oSignal);
begin
    move();
    redraw();
end;

procedure oPlayground.tick();
var s: oSignal;
begin
    s := TickSignal.create(self);
    _dispatcher.emit(s);
end;

procedure oPlayground.start();
begin
end;

function oPlayground.getObjectsInZone(p: oPoint; w,h: integer) : oObjectCollection;
var i:integer;
begin
    getObjectsInZone := oObjectCollection.create();
    for i := 0 to _objects.count() - 1 do begin
        if ( false
            // Cas 1 : coin supérieur gauche image dans la zone
            or (true and (_objects.get(i).getPosition().getX() > p.getX())      // coin image à droite limite gauche de la zone
                     and (_objects.get(i).getPosition().getX() < p.getX() + w)  // coin image à gauche limite droite de la zone
                     and (_objects.get(i).getPosition().getY() < p.getY())      // coin image en dessous limite sup zone
                     and (_objects.get(i).getPosition().getY() > p.getY() + h)) // coin image au dessus limite inf zone

            // cas 2 : coin inférieur droit image dans la zone
            or (true and (_objects.get(i).getPosition().getX() + _objects.get(i).getMask().getWidth  > p.getX())      // coin image à droite lim gauche zone
                     and (_objects.get(i).getPosition().getX() + _objects.get(i).getMask().getWidth  < p.getX() + w)  // coin image à droite lim droite zone
                     and (_objects.get(i).getPosition().getY() + _objects.get(i).getMask().getHeight < p.getY())      // coin image en dessous lim sup zone
                     and (_objects.get(i).getPosition().getY() + _objects.get(i).getMask().getWidth  > p.getY() + h)) // coin image au dessus lim in zone

            // cas 3 : coin supérieur droit image dans la zone
            or (true and (_objects.get(i).getPosition().getX() + _objects.get(i).getMask().getWidth() > p.getX())      // coin image à droite limite gauche de la zone
                     and (_objects.get(i).getPosition().getX() + _objects.get(i).getMask().getWidth() < p.getX() + w)  // coin image à gauche lim droite de la zone
                     and (_objects.get(i).getPosition().getY()                                        < p.getY())      // coin image en dessous lim sup zone
                     and (_objects.get(i).getPosition().getY()                                        > p.getY() + h)) // coin image au dessus lim inf zone

            // cas 4 : coin inférieur gauche image dans la zone
            or (true and (_objects.get(i).getPosition().getX()                                         > p.getX())      // coin image à droite lim gauche zone
                     and (_objects.get(i).getPosition().getX()                                         < p.getX() + w)  // coin image à gauche lim droite zone
                     and (_objects.get(i).getPosition().getY() + _objects.get(i).getMask().getHeight() < p.getY())      // coin image en dessous lim sup zone
                     and (_objects.get(i).getPosition().getY() + _objects.get(i).getMask().getWidth()  > p.getY() + h)) // coin image au dessus lim inf zone

            // cas 5 : coin sup gauche zone dans l'image
            or  (true and (p.getX() > _objects.get(i).getPosition().getX)                                          // coin zone à droite limite gauche de l'image
                      and (p.getX() < _objects.get(i).getPosition().getX + _objects.get(i).getMask().getWidth())   // coin zone à gauche limite droite de l'image
                      and (p.getY() > _objects.get(i).getPosition().getY)                                          // coin zone en dessous lim sup image
                      and (p.getY() < _objects.get(i).getPosition().getY + _objects.get(i).getMask().getHeight())) // coin zone au dessus lim inf image

            // cas 6 : coin inf droit zone dans l'image
            or  (true and (p.getX() + w > _objects.get(i).getPosition().getX())                                          // coin zone à droite limite gauche de l'image
                      and (p.getX() + w < _objects.get(i).getPosition().getX() + _objects.get(i).getMask().getWidth())   // coin zone à gauche limite droite de l'image
                      and (p.getY() + h > _objects.get(i).getPosition().getY())                                          // coin zone en dessous lim sup image
                      and (p.getY() + h < _objects.get(i).getPosition().getY() + _objects.get(i).getMask().getHeight())) // coin zone au dessus lim inf image

            // cas 7 : coin sup droit zone dans l'image
            or  (true and (p.getX() + w > _objects.get(i).getPosition().getX())                                          // coin zone à droite limite gauche de l'image
                      and (p.getX() + w < _objects.get(i).getPosition().getX() + _objects.get(i).getMask().getWidth())   // coin zone à gauche limite droite de l'image
                      and (p.getY()     > _objects.get(i).getPosition().getY())                                          // coin zone en dessous lim sup image
                      and (p.getY()     < _objects.get(i).getPosition().getY() + _objects.get(i).getMask().getHeight())) // coin zone au dessus lim inf image

            //cas 8 coin inf gauche zone dans l'image
            or  (true and (p.getx     > _objects.get(i).getPosition().getX())                                          // coin zone à droite limite gauche de l'image
                      and (p.getx     < _objects.get(i).getPosition().getX() + _objects.get(i).getMask().getWidth())   // coin zone à gauche limite droite de l'image
                      and (p.gety + h > _objects.get(i).getPosition().getY())                                          // coin zone en dessous lim sup image
                      and (p.gety + h < _objects.get(i).getPosition().getY() + _objects.get(i).getMask().getHeight())) // coin zone au dessus lim inf image
        )
        then getObjectsInZone.push(_objects.get(i)); //ajoute l'objet dans l'objectcollection s'il est superposé avec la zone
    end;
end;


end.

