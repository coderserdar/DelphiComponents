unit KADaoCVFUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, DB;

type
  TCVF = class(TForm)
    Label1: TLabel;
    ListBox1: TListBox;
    Label2: TLabel;
    ListBox2: TListBox;
    RemoveBtn: TBitBtn;
    AddBtn: TBitBtn;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    procedure AddBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
    Table : TDataset;
    SLFN1  : TStringList;
    SLFN2  : TStringList;
  public
    { Public declarations }
    Function Execute(DaoTable : TDataset):Boolean;
  end;

var
  CVF: TCVF;

implementation

{$R *.DFM}

procedure TCVF.AddBtnClick(Sender: TObject);
Var
 FF : TField;
 II : Integer;
begin
 II := ListBox2.ItemIndex;
 if II <> -1 Then
    Begin
      ListBox1.Items.Add(ListBox2.Items[II]);
      SLFN1.Add(SLFN2.Strings[II]);
      if CheckBox1.Checked Then
         Begin
           FF := Table.FindField(SLFN2.Strings[II]);
           if FF <> Nil Then
              Begin
               FF.Visible := True;
              End;
         End;
      ListBox2.Items.Delete(II);
      SLFN2.Delete(II);
      Dec(II);
      if II < 0 Then
         if ListBox2.Items.Count > 0 Then
           II :=0;
      ListBox2.ItemIndex:=II;
    End;
end;

procedure TCVF.RemoveBtnClick(Sender: TObject);
Var
 FF : TField;
 II : Integer;
begin
 if ListBox1.Items.Count < 2 Then Exit;
 II := ListBox1.ItemIndex;
 if II <> -1 Then
    Begin
      ListBox2.Items.Add(ListBox1.Items[II]);
      SLFN2.Add(SLFN1.Strings[II]);
      if CheckBox1.Checked Then
         Begin
           FF := Table.FindField(SLFN1.Strings[II]);
           if FF <> Nil Then
              Begin
               FF.Visible := False;
              End;
         End;
      ListBox1.Items.Delete(II);
      SLFN1.Delete(II);
      Dec(II);
      if II < 0 Then
         if ListBox1.Items.Count > 0 Then
           II :=0;
      ListBox1.ItemIndex:=II;
    End;
end;

procedure TCVF.Button1Click(Sender: TObject);
begin
 ModalResult := mrOK;
end;

procedure TCVF.Button2Click(Sender: TObject);
begin
 ModalResult := mrCancel;
end;

Function TCVF.Execute(DaoTable : TDataset):Boolean;
Var
 X  : Integer;
 FF : TField;
 DL : String;
 SL1: TStringList;
 SL2: TStringList;
Begin
 Result := False;
 SL1   := TStringList.Create;
 SL2   := TStringList.Create;
 SLFN1 := TStringList.Create;
 SLFN2 := TStringList.Create;
 Try
  For X := 0 To DaoTable.FieldCount-1 do
      Begin
       if DaoTable.Fields[X].Visible Then
          SL1.Add(DaoTable.Fields[X].FieldName)
       Else
          SL2.Add(DaoTable.Fields[X].FieldName);
      End;
  SLFN1.Assign(SL1);
  SLFN2.Assign(SL2);
  ListBox1.Clear;
  ListBox2.Clear;
  For X := 0 To DaoTable.FieldCount-1 do
     Begin
       DL  := DaoTable.Fields[X].DisplayLabel;
       if DL = '' Then DL := DaoTable.Fields[X].FieldName;
       if DaoTable.Fields[X].Visible Then
          Begin
            ListBox1.Items.Add(DL)
          End
       Else
          Begin
            ListBox2.Items.Add(DL);
          End;
     End;
  Table := DaoTable;
  ShowModal;
  If ModalResult=mrOK Then
    Begin
     For X := 0 To SLFN1.Count-1 do
         Begin
          FF := DaoTable.FindField(SLFN1.Strings[X]);
          if FF <> NIL Then FF.Visible := True;
         End;                                                    
     For X := 0 To SLFN2.Count-1 do
         Begin
          FF := DaoTable.FindField(SLFN2.Strings[X]);
          if FF <> NIL Then FF.Visible := False;
         End;
     Result := True;
    End
 Else
    Begin
      For X := 0 To SL1.Count-1 do
         Begin
          FF := DaoTable.FindField(SL1.Strings[X]);
          if FF <> NIL Then FF.Visible := True;
         End;
     For X := 0 To SL2.Count-1 do
         Begin
          FF := DaoTable.FindField(SL2.Strings[X]);
          if FF <> NIL Then FF.Visible := False;
         End;
    End;
  Finally
   SL1.Free;
   SL2.Free;
   SLFN1.Free;
   SLFN2.Free;
  End;
End;

procedure TCVF.CheckBox1Click(Sender: TObject);
Var
 X  : Integer;
 FF : TField;
begin
 if CheckBox1.Checked Then
    Begin
       For X := 0 To SLFN1.Count-1 do
         Begin
          FF := Table.FindField(SLFN1.Strings[X]);
          if FF <> NIL Then FF.Visible := True;
         End;
       For X := 0 To SLFN2.Count-1 do
         Begin
          FF := Table.FindField(SLFN2.Strings[X]);
          if FF <> NIL Then FF.Visible := False;
         End;
    End;
end;

end.
