Unit fspasswd1;

Interface

{$I fsdefine.inc}

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
  ExtCtrls,
  Buttons;

Type
  TFormPasswd = Class(TForm)
    Bevel1: TBevel;
    BtnCancel: TButton;
    BtnOk: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label2: TLabel;
  Private
  Protected
  Public
  End;
Function ShowTablePasswd(Var aOldSeed, aNewSeed: String): TModalResult;
Var
  FormPasswd: TFormPasswd;

Implementation

{$R *.DFM}

Function ShowTablePasswd(Var aOldSeed, aNewSeed: String): TModalResult;
Begin
  With TFormPasswd.Create(Nil) Do
    Try
      Result := ShowModal;
      If Result = mrOK Then
        Begin
          aOldSeed := Trim(Edit1.Text);
          aNewSeed := Trim(Edit2.Text);
        End;
    Finally
      Free;
    End;
End;

End.

