unit main;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, uplayground, ugamesignals, signal;

type

 { TForm1 }

 TForm1 = class(TForm)
    Image1: TImage;
    playground: oPlayground;
    Timer1: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure redraw(s: oSignal);
    procedure Tick(Sender: TObject);
    
    private
        { private declarations }
        autoanim: boolean;
    public
        { public declarations }
  end;

var Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var s: oSignal;
begin
    playground := oPlayground.create();
    autoanim := false;

    s := RedrawSignal.create(self);
    playground.getDispatcher().bind(s, @self.redraw);
    s.free();

    playground.tick();
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    //playground.tick();
    autoanim := not autoanim;
end;

procedure TForm1.redraw(s: oSignal);
var sig: RedrawSignal;
begin
    sig := s as RedrawSignal;

    writeln('form: updating image');

    Image1.Canvas.FillRect(0, 0, 350, 600);
    sig.bm.draw(Image1.Canvas, 0, 0, true);
end;

procedure TForm1.Tick(Sender: TObject);
begin
    if autoanim then playground.tick();
end;

end.

