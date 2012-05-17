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

    procedure redraw(s: oSignal);
    procedure Timer1Timer(Sender: TObject);
    
    private
        { private declarations }
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

    s := RedrawSignal.create(self);
    playground.getDispatcher().bind(s, @self.redraw);
    s.free();

    playground.tick();
end;

procedure TForm1.redraw(s: oSignal);
var sig: RedrawSignal;
begin
    sig := s as RedrawSignal;

    sig.bm.draw(Image1.Canvas, 0, 0, true);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
    playground.tick();
end;

end.

