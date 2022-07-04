unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, PNGButton, pngimage, jpeg, ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    PNGButton1: TPNGButton;
    Image1: TImage;
    Shape1: TShape;
    PNGButton2: TPNGButton;
    PNGButton3: TPNGButton;
    PNGButton4: TPNGButton;
    PNGButton5: TPNGButton;
    procedure FormCreate(Sender: TObject);
    procedure PNGButton1Click(Sender: TObject);
    procedure PNGButton1DblClick(Sender: TObject);
    procedure PNGButton1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PNGButton1MouseEnter(Sender: TObject);
    procedure PNGButton1MouseLeave(Sender: TObject);
    procedure PNGButton1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PNGButton1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PNGButton2Click(Sender: TObject);
    procedure PNGButton2DblClick(Sender: TObject);
    procedure PNGButton2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PNGButton2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PNGButton2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PNGButton2MouseEnter(Sender: TObject);
    procedure PNGButton2MouseLeave(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
end;

procedure TForm1.PNGButton1Click(Sender: TObject);
begin
  Memo1.Lines.Add('Click');
end;

procedure TForm1.PNGButton1DblClick(Sender: TObject);
begin
  Memo1.Lines.Add('DBLClick');
end;

procedure TForm1.PNGButton1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Memo1.Lines.Add('MouseDown');
end;

procedure TForm1.PNGButton1MouseEnter(Sender: TObject);
begin
  Memo1.Lines.Add('MouseEnter');
end;

procedure TForm1.PNGButton1MouseLeave(Sender: TObject);
begin
  Memo1.Lines.Add('MouseLeave');
end;

procedure TForm1.PNGButton1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  Memo1.Lines.Add('MouseMove');
end;

procedure TForm1.PNGButton1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Memo1.Lines.Add('MouseUp');
end;

procedure TForm1.PNGButton2Click(Sender: TObject);
begin
  memo1.lines.Add('Click');
end;

procedure TForm1.PNGButton2DblClick(Sender: TObject);
begin
  memo1.lines.Add('DBLClick');
end;

procedure TForm1.PNGButton2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  memo1.lines.Add('MouseDown');
end;

procedure TForm1.PNGButton2MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  memo1.lines.Add('MouseMove');
end;

procedure TForm1.PNGButton2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  memo1.lines.Add('MouseUp');
end;

procedure TForm1.PNGButton2MouseEnter(Sender: TObject);
begin
  memo1.lines.Add('MouseEnter');
end;

procedure TForm1.PNGButton2MouseLeave(Sender: TObject);
begin
  memo1.lines.Add('MouseLeave');
end;

end.
