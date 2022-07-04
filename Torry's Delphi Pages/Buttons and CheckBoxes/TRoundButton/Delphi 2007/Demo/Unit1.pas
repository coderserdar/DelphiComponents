unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ExtCtrls, XPMan, RoundButton;

type
  TForm1 = class(TForm)
    bTop: TRoundButton;
    bVCenter: TRoundButton;
    bBottom: TRoundButton;
    bLeft: TRoundButton;
    bHCenter: TRoundButton;
    bRight: TRoundButton;
    Panel1: TPanel;
    rRight: TShape;
    rHCenter: TShape;
    rLeft: TShape;
    rVCenter: TShape;
    rTop: TShape;
    rBottom: TShape;
    bAll: TRoundButton;
    bNone: TRoundButton;
    bBorder: TRoundButton;
    btnOK: TButton;
    btnCancel: TButton;
    ImageList1: TImageList;
    Label1: TLabel;
    XPButton1: TRoundButton;
    XPManifest1: TXPManifest;
    procedure btnCancelClick(Sender: TObject);
    procedure bRightClick(Sender: TObject);
    procedure rRightMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOKClick(Sender: TObject);
    procedure bHCenterClick(Sender: TObject);
    procedure rHCenterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bLeftClick(Sender: TObject);
    procedure bTopClick(Sender: TObject);
    procedure bVCenterClick(Sender: TObject);
    procedure bBottomClick(Sender: TObject);
    procedure rBottomMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rVCenterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rTopMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rLeftMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bAllClick(Sender: TObject);
    procedure bBorderClick(Sender: TObject);
    procedure bNoneClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.bRightClick(Sender: TObject);
begin
  if rRight.Brush.Color = clBlack then
    begin
      rRight.Brush.Color := clWhite;
      bRight.ColorFace := clBtnFace;
    end
  else
    begin
      rRight.Brush.Color := clBlack;
      bRight.ColorFace := clWhite;
    end;
end;

procedure TForm1.bHCenterClick(Sender: TObject);
begin
  if rHCenter.Brush.Color = clBlack then
    begin
      rHCenter.Brush.Color := clWhite;
      bHCenter.ColorFace := clBtnFace;
    end
  else
    begin
      rHCenter.Brush.Color := clBlack;
      bHCenter.ColorFace := clWhite;
    end;
end;

procedure TForm1.bLeftClick(Sender: TObject);
begin
  if rLeft.Brush.Color = clBlack then
    begin
      rLeft.Brush.Color := clWhite;
      bLeft.ColorFace := clBtnFace;
    end
  else
    begin
      rLeft.Brush.Color := clBlack;
      bLeft.ColorFace := clWhite;
    end;
end;

procedure TForm1.bTopClick(Sender: TObject);
begin
  if rTop.Brush.Color = clBlack then
    begin
      rTop.Brush.Color := clWhite;
      bTop.ColorFace := clBtnFace;
    end
  else
    begin
      rTop.Brush.Color := clBlack;
      bTop.ColorFace := clWhite;
    end;
end;

procedure TForm1.bVCenterClick(Sender: TObject);
begin
  if rVCenter.Brush.Color = clBlack then
    begin
      rVCenter.Brush.Color := clWhite;
      bVCenter.ColorFace := clBtnFace;
    end
  else
    begin
      rVCenter.Brush.Color := clBlack;
      bVCenter.ColorFace := clWhite;
    end;
end;

procedure TForm1.bBottomClick(Sender: TObject);
begin
  if rBottom.Brush.Color = clBlack then
    begin
      rBottom.Brush.Color := clWhite;
      bBottom.ColorFace := clBtnFace;
    end
  else
    begin
      rBottom.Brush.Color := clBlack;
      bBottom.ColorFace := clWhite;
    end;
end;

procedure TForm1.rRightMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  bRightClick(Self);
end;

procedure TForm1.rHCenterMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  bHCenterClick(Self);
end;

procedure TForm1.rBottomMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  bBottomClick(Self);
end;

procedure TForm1.rVCenterMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  bVCenterClick(Self);
end;

procedure TForm1.rTopMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  bTopClick(Self);
end;

procedure TForm1.rLeftMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  bLeftClick(Self);
end;

procedure TForm1.bAllClick(Sender: TObject);
begin
  rRight.Brush.Color := clBlack;
  bRight.ColorFace := clWhite;

  rBottom.Brush.Color := clBlack;
  bBottom.ColorFace := clWhite;

  rHCenter.Brush.Color := clBlack;
  bHCenter.ColorFace := clWhite;

  rVCenter.Brush.Color := clBlack;
  bVCenter.ColorFace := clWhite;

  rTop.Brush.Color := clBlack;
  bTop.ColorFace := clWhite;

  rLeft.Brush.Color := clBlack;
  bLeft.ColorFace := clWhite;
end;

procedure TForm1.bBorderClick(Sender: TObject);
begin
  rHCenter.Brush.Color := clWhite;
  bHCenter.ColorFace := clBtnFace;

  rVCenter.Brush.Color := clWhite;
  bVCenter.ColorFace := clBtnFace;
  rRight.Brush.Color := clBlack;
  bRight.ColorFace := clWhite;

  rBottom.Brush.Color := clBlack;
  bBottom.ColorFace := clWhite;

  rTop.Brush.Color := clBlack;
  bTop.ColorFace := clWhite;

  rLeft.Brush.Color := clBlack;
  bLeft.ColorFace := clWhite;
end;

procedure TForm1.bNoneClick(Sender: TObject);
begin
  rRight.Brush.Color := clWhite;
  bRight.ColorFace := clBtnFace;

  rBottom.Brush.Color := clWhite;
  bBottom.ColorFace := clBtnFace;

  rHCenter.Brush.Color := clWhite;
  bHCenter.ColorFace := clBtnFace;

  rVCenter.Brush.Color := clWhite;
  bVCenter.ColorFace := clBtnFace;

  rTop.Brush.Color := clWhite;
  bTop.ColorFace := clBtnFace;

  rLeft.Brush.Color := clWhite;
  bLeft.ColorFace := clBtnFace;
end;

end.
