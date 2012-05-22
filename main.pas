unit main;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, uplayground, ugamesignals, signal, utils;

type

 { TForm1 }

 TForm1 = class(TForm)
    Image1: TImage;
    playground: oPlayground;
    Timer1: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure redraw(si: oSignal);
    procedure Tick(Sender: TObject);
    
    private
        { private declarations }
        autoanim: boolean;
        _flipleft: boolean;
        _flipright : boolean;
    public
        { public declarations }
  end;

var Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var s: oSignal;
begin
    verbosity(15);
    
    autoanim := true;
    _flipleft := false;
    _flipright := false;
    playground := oPlayground.create();

    s := RedrawSignal.create(self);
    playground.getDispatcher().bind(s, @self.redraw);
    s.free();

    playground.redraw();

    //playground.tick();
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    //playground.tick();
    //autoanim := not autoanim;

    d(12, 'form', 'key pressed: ' + s(key));

    case key of 
        32: playground.plungPull();
        37: _flipleft := true;
        39: _flipright := true;
    end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    case key of 
        32: playground.plungRelease();
        37: _flipleft := false;
        39: _flipright := false;
    end;
end;

procedure TForm1.redraw(si: oSignal);
var sig: RedrawSignal;
begin
    sig := si as RedrawSignal;

    d(5, 'form', 'Updating image');

    Image1.Canvas.FillRect(0, 0, 440, 490);
    sig.bm.draw(Image1.Canvas, 0, 0, true);
end;

procedure TForm1.Tick(Sender: TObject);
begin
    if _flipleft then playground.flipLeft();
    if _flipright then playground.flipRight();

    if autoanim then playground.tick();
end;

end.

