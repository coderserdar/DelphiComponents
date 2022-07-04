unit unit_Button;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls, ToolEdit, Mask,
  CHButton;

type
  TfrmCHButton = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    CHButton2: TCHButton;
    CHButton5: TCHButton;
    CHButton11: TCHButton;
    CHButton10: TCHButton;
    CHButton21: TCHButton;
    CHButton19: TCHButton;
    CHButton6: TCHButton;
    CHButton8: TCHButton;
    CHButton4: TCHButton;
    CHButton9: TCHButton;
    CHButton7: TCHButton;
    CHButton3: TCHButton;
    CHButton12: TCHButton;
    GroupBox1: TGroupBox;
    CHButton13: TCHButton;
    CHButton14: TCHButton;
    CHButton15: TCHButton;
    CHButton16: TCHButton;
    CHButton17: TCHButton;
    CHButton18: TCHButton;
    CHButton1: TCHButton;
    CHButton20: TCHButton;
    CHButton22: TCHButton;
    CHButton23: TCHButton;
    CHButton24: TCHButton;
    CHButton25: TCHButton;
    CHButton26: TCHButton;
    CHButton27: TCHButton;
    CHButton28: TCHButton;
    procedure Info1Click(Sender: TObject);
    procedure close1Click(Sender: TObject);
    procedure CHButton24MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CHButton25MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CHButton26MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CHButton27MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHButton: TfrmCHButton;

implementation

uses unit_About, _CHTypes;

{$R *.dfm}


procedure TfrmCHButton.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;

procedure TfrmCHButton.close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHButton.CHButton24MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CHButton24.Gradient.Color1 := clRed;
  CHButton25.Gradient.Color1 := clBlue;
  CHButton26.Gradient.Color1 := clBlue;
  CHButton27.Gradient.Color1 := clBlue;
end;

procedure TfrmCHButton.CHButton25MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CHButton24.Gradient.Color1 := clBlue;
  CHButton25.Gradient.Color1 := clRed;
  CHButton26.Gradient.Color1 := clBlue;
  CHButton27.Gradient.Color1 := clBlue;
end;

procedure TfrmCHButton.CHButton26MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CHButton24.Gradient.Color1 := clBlue;
  CHButton25.Gradient.Color1 := clBlue;
  CHButton26.Gradient.Color1 := clRed;
  CHButton27.Gradient.Color1 := clBlue;
end;

procedure TfrmCHButton.CHButton27MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CHButton24.Gradient.Color1 := clBlue;
  CHButton25.Gradient.Color1 := clBlue;
  CHButton26.Gradient.Color1 := clBlue;
  CHButton27.Gradient.Color1 := clRed;
end;

end.
