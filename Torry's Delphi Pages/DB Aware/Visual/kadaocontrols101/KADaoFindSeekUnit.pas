unit KADaoFindSeekUnit;
{$I KADaoControlsCommonDirectives.pas}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, KDaoTable;

type
  TKadaoFindSeekDialog = class(TForm)
    ScrollBox1: TScrollBox;
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    Ok: TButton;
    Cancel: TButton;
    Button1: TButton;
    procedure CancelClick(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    CheckBoxes   : Array[0..300] of TCheckBox;
    EditControls : Array[0..300] of TEdit;
    Buttons      : Array[0..300] of TButton;
    Table        : TKADaoTable;
    SeekMethod   : Boolean;
    Procedure OnCheckBoxClick(Sender: TObject);
    Procedure OnEditKeyPress(Sender: TObject; var Key: Char);
    Function  FindCheckBox(CB : TCheckBox):Integer;
    Function  FindButton(CB : TButton):Integer;
    Procedure DBClick(Sender: TObject);
  public
    { Public declarations }
    Function Execute(DaoTable : TKADaoTable; Seek : Boolean):Boolean;
  end;

var
  KADaoFindSeekDialog: TKADaoFindSeekDialog;

implementation
{$R *.DFM}
Uses DB, ComCtrls, KADaoDTPickerUnit {$IFDEF D6UP}, Variants{$ENDIF};

Function  TKADaoFindSeekDialog.FindCheckBox(CB : TCheckBox):Integer;
Var
 X : Integer;
Begin
 Result := -1;
 For X := 0 To 300 do
     Begin
       if CheckBoxes[X]=CB Then
          Begin
            Result := X;
            Exit;
          End;
     End;
End;

Function  TKADaoFindSeekDialog.FindButton(CB : TButton):Integer;
Var
 X : Integer;
Begin
 Result := -1;
 For X := 0 To 300 do
     Begin
       if Buttons[X]=CB Then
          Begin
            Result := X;
            Exit;
          End;
     End;
End;

Procedure TKADaoFindSeekDialog.OnEditKeyPress(Sender: TObject; var Key: Char);
Begin
 if Key=#13 Then
    Begin
      Button1.Click;
      Key:=#0;
    End;
End;

Procedure TKADaoFindSeekDialog.OnCheckBoxClick(Sender: TObject);
Var
  I : Integer;
Begin
  I := FindCheckBox(Sender as TCheckBox);
  if I =-1 Then Exit;
  if CheckBoxes[I].Checked Then
     Begin
      EditControls[I].Enabled   := True;
      EditControls[I].Color     := clWindow;
      EditControls[I].SetFocus;
      EditControls[I].SelStart  := 0;
      EditControls[I].SelLength := 0;
      if Buttons[I].Caption='...' Then Buttons[I].Enabled      := True;
      if Buttons[I].Caption='..'  Then Buttons[I].Enabled      := True;
     End
  Else
     Begin
       EditControls[I].Text    := '';
       EditControls[I].Enabled := False;
       EditControls[I].Color   := clInactiveCaption;
       Buttons[I].Enabled      := False;
     End;
End;

Function TKADaoFindSeekDialog.Execute(DaoTable : TKADaoTable; Seek : Boolean):Boolean;
Const
  XPOS : Integer = 10;
  YPOS : Integer = 2;
  YINC : Integer = 30;
Var
  X  : Integer;
  BR : Integer;
  ML : Integer;
  C1 : TLabel;
  C2 : TLabel;
Begin
 Result := False;
 C1 := TLabel.Create(ScrollBox1);
 ScrollBox1.InsertControl(C1);
 C1.Left := XPOS;
 C1.Top  := YPOS;
 C1.Caption := 'Search field';
 C1.Font.Style:=[fsBold];
 C1.Font.Color:=clRed;

 if NOT Seek Then
    Begin
     Self.Caption := ' Find a Record based on data contained in the selected fields';
     BR := 0;
     ML := 0;
     For X := 0 to DaoTable.FieldCount-1 do
      Begin
       if (DaoTable.Fields[X].FieldKind=fkData) And (DaoTable.Fields[X].Visible) Then
         Begin
          CheckBoxes[BR]:=TCheckBox.Create(ScrollBox1);
          ScrollBox1.InsertControl(CheckBoxes[BR]);
          CheckBoxes[BR].Caption:=DaoTable.Fields[BR].FieldName+':';
          CheckBoxes[BR].Top  := YPOS+C1.Height+5+(BR*YINC);
          CheckBoxes[BR].Left := XPOS;
          CheckBoxes[BR].OnClick:=OnCheckBoxClick;
          if (Self.Canvas.TextWidth(CheckBoxes[BR].Caption)+CheckBoxes[BR].Left) > ML Then
             ML := Self.Canvas.TextWidth(CheckBoxes[BR].Caption)+CheckBoxes[BR].Left;
          Inc(BR);
         End;
      End;
     BR := 0;
     For X := 0 to DaoTable.FieldCount-1 do
         Begin
          if (DaoTable.Fields[X].FieldKind=fkData)  And (DaoTable.Fields[X].Visible) Then
             Begin
               EditControls[BR]         := TEdit.Create(ScrollBox1);
               ScrollBox1.InsertControl(EditControls[BR]);
               EditControls[BR].Text    := '';
               EditControls[BR].Enabled := False;
               EditControls[BR].Color   := clInactiveCaption;
               EditControls[BR].Top     := CheckBoxes[BR].Top-((EditControls[BR].Height-CheckBoxes[BR].Height) Div 2);
               EditControls[BR].Left    := XPOS*3+ML;
               EditControls[BR].Width   := Round(EditControls[BR].Width*1.5);

               EditControls[BR].OnKeyPress :=OnEditKeyPress;
               Inc(BR);
             End;
         End;
     BR := 0;
     For X := 0 to DaoTable.FieldCount-1 do
         Begin
          if (DaoTable.Fields[X].FieldKind=fkData)  And (DaoTable.Fields[X].Visible) Then
             Begin
              Buttons[BR]         := TButton.Create(ScrollBox1);
              ScrollBox1.InsertControl(Buttons[BR]);
              Buttons[BR].Top   := EditControls[BR].Top;
              Buttons[BR].Left  := EditControls[BR].Left+EditControls[BR].Width+10;
              Buttons[BR].Width := Self.Canvas.TextWidth(' ... ')+10;
              Buttons[BR].Height:= EditControls[BR].Height;
              if DaoTable.Fields[X].DataType=ftDate     Then
                 Begin
                   Buttons[BR].Caption := '...';
                   EditControls[BR].Text := 'dd'+DateSeparator+'mm'+DateSeparator+'yyyy';
                 End
              Else
              if DaoTable.Fields[X].DataType=ftTime     Then
                 Begin
                   Buttons[BR].Caption := '..';
                   EditControls[BR].Text := 'hh'+TimeSeparator+'mm'+TimeSeparator+'ss';
                 End
              Else
                Begin
                  Buttons[BR].Caption := 'Select';
                  Buttons[BR].Visible := False;
                End;
              Buttons[BR].OnClick := DBClick;
              Buttons[BR].Enabled := False;
              Inc(BR);
             End;
         End;
    End
 Else
    Begin
      Self.Caption := ' Seek a Record on data in the selected fields part of an Index';
      BR := 0;
      ML := 0;
      For X := 0 to DaoTable.IndexFieldCount-1 do
      Begin
       if DaoTable.IndexFields[X].FieldKind=fkData Then
         Begin
          CheckBoxes[BR]:=TCheckBox.Create(ScrollBox1);
          ScrollBox1.InsertControl(CheckBoxes[BR]);
          CheckBoxes[BR].Caption:=DaoTable.IndexFields[BR].FieldName+':';
          CheckBoxes[BR].Top  := YPOS+C1.Height+5+(BR*YINC);
          CheckBoxes[BR].Left := XPOS;
          CheckBoxes[BR].OnClick:=OnCheckBoxClick;
          if (Self.Canvas.TextWidth(CheckBoxes[BR].Caption)+CheckBoxes[BR].Left) > ML Then
             ML := Self.Canvas.TextWidth(CheckBoxes[BR].Caption)+CheckBoxes[BR].Left;
          Inc(BR);
         End;
      End;
      BR := 0;
      For X := 0 to DaoTable.IndexFieldCount-1 do
         Begin
          if DaoTable.IndexFields[X].FieldKind=fkData Then
             Begin
               EditControls[BR]         := TEdit.Create(ScrollBox1);
               ScrollBox1.InsertControl(EditControls[BR]);
               EditControls[BR].Text    := '';
               EditControls[BR].Enabled := False;
               EditControls[BR].Color   := clInactiveCaption;
               EditControls[BR].Top     := CheckBoxes[BR].Top-((EditControls[BR].Height-CheckBoxes[BR].Height) Div 2);
               EditControls[BR].Left    := XPOS*3+ML;
               EditControls[BR].Width   := Round(EditControls[BR].Width*1.5);
               Inc(BR);
             End;
         End;
     BR := 0;
     For X := 0 to DaoTable.IndexFieldCount-1 do
         Begin
          if DaoTable.IndexFields[X].FieldKind=fkData Then
             Begin
              Buttons[BR]         := TButton.Create(ScrollBox1);
              ScrollBox1.InsertControl(Buttons[BR]);
              Buttons[BR].Top   := EditControls[BR].Top;
              Buttons[BR].Left  := EditControls[BR].Left+EditControls[BR].Width+10;
              Buttons[BR].Width := Self.Canvas.TextWidth(' ... ')+10;
              Buttons[BR].Height:= EditControls[BR].Height;
              if DaoTable.IndexFields[X].DataType=ftDate     Then
                 Begin
                   Buttons[BR].Caption := '...';
                   EditControls[BR].Text := 'dd'+DateSeparator+'mm'+DateSeparator+'yyyy';
                 End
              Else
              if DaoTable.IndexFields[X].DataType=ftTime     Then
                 Begin
                   Buttons[BR].Caption := '..';
                   EditControls[BR].Text := 'hh'+TimeSeparator+'mm'+TimeSeparator+'ss';
                 End
              Else
                Begin
                  Buttons[BR].Caption := 'Select';
                  Buttons[BR].Visible := False;
                End;
              Buttons[BR].OnClick := DBClick;
              Buttons[BR].Enabled := False;
              Inc(BR);
             End;
         End;
    End;

 C2 := TLabel.Create(ScrollBox1);
 ScrollBox1.InsertControl(C2);
 C2.Left := XPOS*3+ML;
 C2.Top  := YPOS;
 C2.Caption := 'Search data';
 C2.Font.Style:=[fsBold];
 C2.Font.Color:=clRed;

 Table      := DaoTable;
 SeekMethod := Seek;
 ShowModal;
 if ModalResult=mrOK Then
    Begin
      Result := True;
    End;
End;

procedure TKadaoFindSeekDialog.CancelClick(Sender: TObject);
begin
 Modalresult := mrCancel;
end;

procedure TKadaoFindSeekDialog.OkClick(Sender: TObject);
begin
 Button1.Click;
 Modalresult := mrOK;
end;

procedure TKadaoFindSeekDialog.DBClick(Sender: TObject);
Var
  DT  : TDateTime;
  P   : TPoint;
  Res : Boolean;
  BN  : Integer;
Begin
  BN := FindButton(Sender as TButton);
  if BN < 0 Then Exit;
  Application.CreateForm(TDTPicker, DTPicker);
  P.X := (Sender as TButton).Left;
  P.Y := (Sender as TButton).Top;
  P   := ScrollBox1.ClientToScreen(P);
  DTPicker.Left  := P.X-((DTPicker.Width-(Sender as TButton).Width));
  DTPicker.Top   := P.Y+(Sender as TButton).Height+2;
  if (Sender as TButton).Caption='..' Then
    Begin
      Res := DTPicker.Execute(DT,dtkTime);
      if Res Then EditControls[BN].Text := FormatDateTime('hh'+TimeSeparator+'nn'+TimeSeparator+'ss',DT);
    End
  Else
    Begin
      Res := DTPicker.Execute(DT,dtkDate);
      if Res Then EditControls[BN].Text := FormatDateTime('dd'+DateSeparator+'mm'+DateSeparator+'yyyy',DT);
    End;
  DTPicker.Free;
End;

procedure TKadaoFindSeekDialog.Button1Click(Sender: TObject);
Var
 KeyFields : string;
 KeyValues : Variant;
 Options   : TLocateOptions;
 BR        : Integer;
 NumFields : Integer;
 NF        : Integer;
 X         : Integer;
begin
 if NOt SeekMethod Then
    Begin
     BR         := 0;
     NumFields  := 0;
     KeyFields := '';
     For X := 0 to Table.FieldCount-1 do
      Begin
       if (Table.Fields[X].FieldKind=fkData)  And (Table.Fields[X].Visible) Then
          Begin
            if EditControls[BR].Enabled Then
               Begin
                 KeyFields := KeyFields+Table.Fields[X].FieldName+';';
                 Inc(NumFields);
               End;
            Inc(BR);
          End;
      End;
      if NumFields=0 Then Exit;
      if NumFields > 1 Then KeyValues := VarArrayCreate([0,NumFields-1],varVariant);
      NF := 0;
      BR := 0;
      For X := 0 to Table.FieldCount-1 do
      Begin
       if (Table.Fields[X].FieldKind=fkData)  And (Table.Fields[X].Visible) Then
          Begin
            if EditControls[BR].Enabled Then
               Begin
                if NumFields = 1 Then
                   KeyValues     := EditControls[BR].Text
                Else
                   KeyValues[NF] := EditControls[BR].Text;
                Inc(NF);
               End;
            Inc(BR);
          End;
      End;
      if KeyFields[Length(KeyFields)]=';' Then System.Delete(KeyFields,Length(KeyFields),1);
      Options  := [loCaseInsensitive, loPartialKey];
      if Not Table.Find_Next (KeyFields, KeyValues, Options) Then
             Table.Find_First(KeyFields, KeyValues, Options);
      VarClear(KeyValues);
    End
 Else
    Begin
      if Table.IndexFieldCount > 1 Then KeyValues := VarArrayCreate([0,Table.IndexFieldCount-1],varVariant);
      KeyFields := '';
      For X := 0 to Table.IndexFieldCount-1 do
      Begin
       if (Table.IndexFields[X].FieldKind=fkData) And (Table.IndexFields[X].Visible) Then
          Begin
            KeyFields := KeyFields+Table.IndexFields[X].FieldName+';';
          End;
      End;
      BR := 0;
      For X := 0 to Table.IndexFieldCount-1 do
      Begin
       if (Table.IndexFields[X].FieldKind=fkData)  And (Table.IndexFields[X].Visible) Then
          Begin
            if EditControls[X].Enabled Then
               Begin
                if Table.IndexFieldCount = 1 Then
                   KeyValues    := EditControls[BR].Text
                Else
                   KeyValues[X] := EditControls[BR].Text;
                Inc(BR);
               End;
          End;
      End;
      if KeyFields[Length(KeyFields)]=';' Then System.Delete(KeyFields,Length(KeyFields),1);
      Options  := [loCaseInsensitive, loPartialKey];
      Table.Locate(KeyFields, KeyValues, Options);
      VarClear(KeyValues);
    End;
end;

end.
