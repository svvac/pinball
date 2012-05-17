unit main;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, uplayground;

type

 { TForm1 }

 TForm1 = class(TForm)
    Image1: TImage;
    playground: oPlayground;

    procedure FormCreate(Sender: TObject);
    
    private
        { private declarations }
    public
        { public declarations }
  end;

var Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
    playground := oPlayground.create(Image1.Canvas);

    playground.tick();

    //playground.free();
end;

end.

