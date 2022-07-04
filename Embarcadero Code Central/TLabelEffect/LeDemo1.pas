unit LeDemo1;

{
  TLabelEffect Demonstration.
  Show off the capabilities of the TLabelEffect component.

  Written by Keith Wood - 3 Jan 1997.
}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, LblEffct, TabNotBk, ComCtrls, ImgList;

type
  TForm1 = class(TForm)
    TabbedNotebook1: TTabbedNotebook;
    LabelEffect1: TLabelEffect;
    LabelEffect2: TLabelEffect;
    LabelEffect3: TLabelEffect;
    LabelEffect4: TLabelEffect;
    LabelEffect5: TLabelEffect;
    LabelEffect6: TLabelEffect;
    LabelEffect7: TLabelEffect;
    LabelEffect8: TLabelEffect;
    LabelEffect9: TLabelEffect;
    LabelEffect10: TLabelEffect;
    LabelEffect11: TLabelEffect;
    LabelEffect12: TLabelEffect;
    LabelEffect13: TLabelEffect;
    LabelEffect14: TLabelEffect;
    LabelEffect15: TLabelEffect;
    LabelEffect16: TLabelEffect;
    Shape1: TShape;
    Shape2: TShape;
    lblRotate: TLabelEffect;
    tmrTimer: TTimer;
    LabelEffect18: TLabelEffect;
    LabelEffect19: TLabelEffect;
    LabelEffect20: TLabelEffect;
    LabelEffect21: TLabelEffect;
    lblButton: TLabelEffect;
    lblSundial: TLabelEffect;
    LabelEffect24: TLabelEffect;
    LabelEffect17: TLabelEffect;
    LabelEffect23: TLabelEffect;
    LabelEffect25: TLabelEffect;
    LabelEffect26: TLabelEffect;
    LabelEffect27: TLabelEffect;
    LabelEffect28: TLabelEffect;
    LabelEffect29: TLabelEffect;
    LabelEffect30: TLabelEffect;
    LabelEffect31: TLabelEffect;
    LabelEffect32: TLabelEffect;
    LabelEffect33: TLabelEffect;
    LabelEffect34: TLabelEffect;
    LabelEffect22: TLabelEffect;
    imlScanner: TImageList;
    LabelEffect35: TLabelEffect;
    imlOnFire: TImageList;
    LabelEffect36: TLabelEffect;
    imlRainbow: TImageList;
    LabelEffect37: TLabelEffect;
    procedure lblButtonClick(Sender: TObject);
    procedure tmrTimerTimer(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.lblButtonClick(Sender: TObject);
begin
  MessageDlg('TLabelEffect version ' + lblButton.Version, mtInformation, [mbOK], 0);
end;

procedure TForm1.tmrTimerTimer(Sender: TObject);
begin
  { Rotate highlight and shadow one direction clockwise }
  if lblSundial.DirectionHighlight = edUpLeft then
    lblSundial.DirectionHighlight := edUp
  else
    lblSundial.DirectionHighlight := Succ(lblSundial.DirectionHighlight);
  if lblSundial.DirectionShadow = edUpLeft then
    lblSundial.DirectionShadow := edUp
  else
    lblSundial.DirectionShadow := Succ(lblSundial.DirectionShadow);

  { Rotate label 10 degrees clockwise }
  if lblRotate.Angle = 360 then
    lblRotate.Angle := 10
  else
    lblRotate.Angle := lblRotate.Angle + 10;
end;

end.
