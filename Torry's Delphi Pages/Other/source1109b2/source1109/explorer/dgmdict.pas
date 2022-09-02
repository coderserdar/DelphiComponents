Unit dgmdict;

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
  Printers,
  Menus;

Type
  TfrmDict = Class(TForm)
    Memo1: TMemo;
    PrintDialog1: TPrintDialog;
    MainMenu1: TMainMenu;
    Print1: TMenuItem;
    Print2: TMenuItem;
    Procedure Print2Click(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
  End;

Var
  frmDict: TfrmDict;

Implementation

{$R *.DFM}

Procedure TfrmDict.Print2Click(Sender: TObject);
Var
  I: Integer;
  F: TextFile;
Begin
  If PrintDialog1.Execute Then
    Begin
      AssignPrn(F);
      Try
        Rewrite(F);
        Printer.Canvas.Font.assign(memo1.font);
        Writeln(F, '');
        For I := 0 To Memo1.Lines.Count - 1 Do
          Writeln(F, Memo1.Lines[I]);
      Finally
        CloseFile(F);
      End;
    End;
End;

End.

