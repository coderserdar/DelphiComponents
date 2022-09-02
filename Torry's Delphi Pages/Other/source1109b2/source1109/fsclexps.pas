{$I fsdefine.inc}

Unit fsclexps;

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
  Stdctrls;

Type
  TfsSelectProtocols = Class(TForm)
    GroupBox1: TGroupBox;
    Button1: TButton;
    chkSU: TCheckBox;
    chkIPX: TCheckBox;
    chkTCP: TCheckBox;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
  Private
    { Private declarations }
  Public
    { Public declarations }
  End;

Var
  fsSelectProtocols: TfsSelectProtocols;

Implementation

{$R *.DFM}

Procedure TfsSelectProtocols.FormCloseQuery(Sender: TObject;
  Var CanClose: Boolean);
Resourcestring
  RError = 'You must select at least one protocol before you continue.';
Begin
  CanClose := chkTCP.Checked Or chkIPX.Checked Or chkSU.Checked;
  If Not CanClose Then
    MessageDlg(RError, mtInformation, [mbOK], 0);
End;

End.

