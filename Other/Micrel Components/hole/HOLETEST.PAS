unit Holetest;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Hole, ExtCtrls;

type
  TForm1 = class(TForm)
    cbNoClip: TButton;
    Image1: TImage;
    Button1: TButton;
    Hole1: THole;
    procedure cbNoClipClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.cbNoClipClick(Sender: TObject);
var i, j:integer;
begin
   i := 0;
   j := Hole1.Top + (Hole1.height div 2);
   repeat
       form1.canvas.draw(i,j,image1.picture.bitmap);
       inc(i, image1.width);
   until i > form1.width;
end;

procedure TForm1.Button1Click(Sender: TObject);
var i, j:integer;
begin
   Hole1.ClippingOn;
   i := 0;
   j := Hole1.Top + (Hole1.height div 2) - 42;
   repeat
       form1.canvas.draw(i,j,image1.picture.bitmap);
       inc(i, image1.width);
   until i > form1.width;
   Hole1.ClippingOff;
end;

end.
