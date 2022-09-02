Unit fspasswd;

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
  TFormPasswds = Class(TForm)
    Bevel1: TBevel;
    BtnCancel: TButton;
    BtnOk: TButton;
    Memo1: TMemo;
    Label1: TLabel;
  Private
  Protected
  Public
  End;
Function ShowTablesPasswd(S: TStrings): TModalResult; {!!.10}
Var
  FormPasswds: TFormPasswds;

Implementation

{$R *.DFM}

Function ShowTablesPasswd(S: TStrings): TModalResult;
Begin
  With TFormPasswds.Create(Nil) Do
    Try
      Memo1.Lines.Assign(S);
      Result := ShowModal;
      If Result = mrOK Then
        Begin
        s.Assign(Memo1.Lines);
        End;
    Finally
      Free;
    End;
End;

End.

