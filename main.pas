unit main;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ushape, upoint, math;

type

 TForm1 = class(TForm)
    Memo1: TMemo;
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
var a: oShape;
    p, q, r, t: oPoint;
    b: boolean;
    i, j: integer;
    s: string;
begin
    a := oShape.create('bitmaps/shape-test.bmp');
    p := oPoint.create(124, 38);
    //p := oPoint.Create(41, 78);
    //p := oPoint.create(57, 45);
    //p := oPoint.create(137, 45);
    t := oPoint.Create(0, 0);

    if (a.getPoint(p)) then Memo1.Append('p is solid')
                       else Memo1.Append('p is void');
    if (a.isOnEdge(p)) then Memo1.Append('p is on edge of a')
                       else Memo1.Append('p is NOT on edge of a');
    
    Memo1.Append('Tan at p = ' + p.toStr() + ' is ' + FloatToStr(a.getTangentAngleAt(p)) + 'rad.');

    for j := 0 to a.getHeight - 1 do begin
        s := '';
        for i := 0 to a.getWidth - 1 do begin
            t.setXY(i, j);
            if t.sameAs(p) then s += 'O'
            //else if t.sameAs(q) or t.sameAs(r) then s += '.''
            else if a.getPoint(t) then s += '#'
            else s += ' ';
        end;
        Memo1.Append(s);
    end;
end;

end.

