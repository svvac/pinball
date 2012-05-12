unit uplayground;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics, upoint, uobject, math, objectcollection, eventhandler, signal, ugamesignals;

CONST NB_LIFES = 3;

type oPlayground = class
    protected
        _world: TBitmap;
        _ball: aObject;
        _score: integer;
        _lifes: integer;
        _dispatcher: oEventHandler;

        _objects: oObjectCollection;

        procedure redraw();
        function getObjectsInZone(p:oPoint; w,h: integer) : oObjectCollection;

    public
        constructor create();
        destructor destroy(); override;

        procedure init();
        procedure start();

end;


implementation


constructor oPlayground.create();
begin
    _dispatcher := oEventHandler.create()
end;

destructor oPlayground.destroy();
begin
end;

procedure oPlayground.init();
var i:integer;
    count:integer;
begin
    _world := Tbitmap.create();
    //_world.loadFromFile('/path/to/image');//TODO: mettre nom fichier image
    
    //_ball := oBall.create();
    
    _score := 0;
    _lifes := NB_LIFES;
end;

procedure oPlayground.redraw();
var sig: RedrawSignal;
begin
    sig := RedrawSignal.create(self);
    sig.bm := _world;
    _dispatcher.emit(sig);
end;

procedure oPlayground.start();
begin
end;

function oPlayground.getObjectsInZone(p: oPoint; w,h: integer) : oObjectCollection;
var i:integer;
begin
    getObjectsInZone := oObjectCollection.create();
    for i := 0 to _objects.count() do begin
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
                      and (p.getY() < _objects.get(i).getPosition().getX + _objects.get(i).getMask().getHeight())) // coin zone au dessus lim inf image

            // cas 6 : coin inf droit zone dans l'image
            or  (true and (p.getX() + w > _objects.get(i).getPosition().getX())                                          // coin zone à droite limite gauche de l'image
                      and (p.getX() + w < _objects.get(i).getPosition().getX() + _objects.get(i).getMask().getWidth())   // coin zone à gauche limite droite de l'image
                      and (p.getY() + h > _objects.get(i).getPosition().getY())                                          // coin zone en dessous lim sup image
                      and (p.getY() + h < _objects.get(i).getPosition().getX() + _objects.get(i).getMask().getHeight())) // coin zone au dessus lim inf image

            // cas 7 : coin sup droit zone dans l'image
            or  (true and (p.getX() + w > _objects.get(i).getPosition().getX())                                          // coin zone à droite limite gauche de l'image
                      and (p.getX() + w < _objects.get(i).getPosition().getX() + _objects.get(i).getMask().getWidth())   // coin zone à gauche limite droite de l'image
                      and (p.getY()     > _objects.get(i).getPosition().getY())                                          // coin zone en dessous lim sup image
                      and (p.getY()     < _objects.get(i).getPosition().getX() + _objects.get(i).getMask().getHeight())) // coin zone au dessus lim inf image

            //cas 8 coin inf gauche zone dans l'image
            or  (true and (p.getx     > _objects.get(i).getPosition().getX())                                          // coin zone à droite limite gauche de l'image
                      and (p.getx     < _objects.get(i).getPosition().getX() + _objects.get(i).getMask().getWidth())   // coin zone à gauche limite droite de l'image
                      and (p.gety + h > _objects.get(i).getPosition().getY())                                          // coin zone en dessous lim sup image
                      and (p.gety + h < _objects.get(i).getPosition().getX() + _objects.get(i).getMask().getHeight())) // coin zone au dessus lim inf image
        )
        then getObjectsInZone.push(_objects.get(i)); //ajoute l'objet dans l'objectcollection s'il est superposé avec la zone
    end;
end;


end.

