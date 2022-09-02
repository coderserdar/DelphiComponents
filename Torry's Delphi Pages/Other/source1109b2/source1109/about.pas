Unit about;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Stdctrls,
  jpeg,
  fsllbase,
  ExtCtrls,
  Buttons;

Type
  TfsAboutForm = Class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    Label2: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label5: TLabel;
    Label3: TLabel;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    Bevel2: TBevel;
    Label9: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Procedure FormActivate(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    Procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure Label7MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure Label1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure Label7Click(Sender: TObject);
    Procedure Label1Click(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
  End;

Var
  fsAboutForm: TfsAboutForm;

Implementation

{$R *.DFM}
Uses
  ShellAPI;
Resourcestring
  cBrowserError = 'Unable to start web browser. Make sure you have it properly setup on your system.';

Procedure TfsAboutForm.FormActivate(Sender: TObject);
Begin
  label2.Caption := Format('32-bit Version %5.3f', [fsVersionNumber / 1000.0]);
  If fsVersionBeta > 0 Then
    label2.Caption := label2.Caption + ' Beta: ' + IntToStr(fsVersionBeta);
  label8.Caption := 'Build: ' + Format('%4d-%2.2d-%2.2d %2.2d:%2.2d',
    [fsBuildYear, fsBuildMonth, fsBuildDay, fsBuildHour, fsBuildMinute]);
End;

Procedure TfsAboutForm.SpeedButton1Click(Sender: TObject);
Begin
  Close;
End;

Procedure TfsAboutForm.Image1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Begin
  label1.Font.Style := [];
  label7.Font.Style := [];
End;

Procedure TfsAboutForm.Label7MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Begin
  label1.Font.Style := [];
  label7.Font.Style := [fsUnderline];
End;

Procedure TfsAboutForm.Label1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Begin
  label1.Font.Style := [fsUnderline];
  label7.Font.Style := [];
End;

Procedure TfsAboutForm.Label7Click(Sender: TObject);
Begin
  If ShellExecute(0, 'open', 'http://www.fssql.com', '',
    '', SW_SHOWNORMAL) <= 32 Then
    ShowMessage(cBrowserError);
End;

Procedure TfsAboutForm.Label1Click(Sender: TObject);
Begin
  If ShellExecute(0, 'open', 'http://www.fssql.com/forum', '',
    '', SW_SHOWNORMAL) <= 32 Then
    ShowMessage(cBrowserError);
End;

End.

