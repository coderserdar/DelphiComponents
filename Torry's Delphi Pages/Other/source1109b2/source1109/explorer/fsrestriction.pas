Unit fsrestriction;

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
  TFormObjRestrictions = Class(TForm)
    CB1: TCheckBox;
    CB2: TCheckBox;
    Cb3: TCheckBox;
    Bevel1: TBevel;
    BtnCancel: TButton;
    BtnOk: TButton;
    cb4: TCheckBox;
    cb5: TCheckBox;
    cb6: TCheckBox;
    cb7: TCheckBox;
    cb8: TCheckBox;
  Private
  Protected
    FTableName: String;
    FNewSeed: Word;
  Public
    Property NewSeed: Word Read FNewSeed Write FNewSeed;
    Property TableName: String Read FTableName Write FTableName;

  End;
Function ShowTableFlagsDlg(Const aTableName: String;
  Var aNewSeed: Word): TModalResult; {!!.10}
Var
  FormObjRestrictions: TFormObjRestrictions;

Implementation

{$R *.DFM}

Function ShowTableFlagsDlg(Const aTableName: String;
  Var aNewSeed: Word): TModalResult; {!!.10}
Begin
  With TFormObjRestrictions.Create(Nil) Do
    Try
      FTableName := aTableName;
      NewSeed := aNewSeed;
      CB1.Checked := (NewSeed And 8) <> 0; //fsTableDontChangeAutoInc
      CB2.Checked := (NewSeed And 32) <> 0; //fsTableDontInsertRecord
      CB3.Checked := (NewSeed And 64) <> 0; //fsTableDontModifyRecord
      CB4.Checked := (NewSeed And 128) <> 0; //fsTableDontDeleteRecord
      CB5.Checked := (NewSeed And 256) <> 0; //fsTableDontChangeMaxRecords
      CB6.Checked := (NewSeed And 2) <> 0; //fsTableDontRestructure
      CB7.Checked := (NewSeed And 512) <> 0; //fsTableDontEmptyTable
      CB8.Checked := (NewSeed And 1024) <> 0; //fsTableDontProtectRow
      Result := ShowModal;
      If Result = mrOK Then
        Begin
          NewSeed :=
            Word(CB1.Checked) * 8 +
            Word(CB2.Checked) * 32 +
            Word(CB3.Checked) * 64 +
            Word(CB4.Checked) * 128 +
            Word(CB5.Checked) * 256 +
            Word(CB7.Checked) * 512 +
            Word(CB8.Checked) * 1024 +
            Word(CB6.Checked) * 2;
          aNewSeed := NewSeed;
        End;
    Finally
      Free;
    End;
End;

End.

