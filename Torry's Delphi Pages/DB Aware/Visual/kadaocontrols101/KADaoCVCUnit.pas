unit KADaoCVCUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, DBGrids;

type
  TCVC = class(TForm)
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
    Grid   : TDBGrid;
    SLFN1  : TStringList;
    SLFN2  : TStringList;
    Function  FindColumn(Caption:String):TColumn;
  public
    { Public declarations }
    Function Execute(AGrid:TDBGrid):Boolean;                           
  end;

var
  CVC: TCVC;

implementation

{$R *.DFM}

Function  TCVC.FindColumn(Caption:String):TColumn;
Var
 X : Integer;
Begin
 Result := Nil;
 For X := 0 To Grid.Columns.Count-1 do
     Begin
       if Grid.Columns.Items[X].Title.Caption=Caption Then
          Begin
            Result := Grid.Columns.Items[X];
          End;
     End;
End;

procedure TCVC.AddBtnClick(Sender: TObject);
Var
 FF : TColumn;
 II : Integer;
begin
 II := ListBox2.ItemIndex;
 if II <> -1 Then
    Begin
      ListBox1.Items.Add(ListBox2.Items[II]);
      SLFN1.Add(SLFN2.Strings[II]);
      if CheckBox1.Checked Then
         Begin
           FF := FindColumn(SLFN2.Strings[II]);
           if FF <> Nil Then FF.Visible := True;
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

procedure TCVC.RemoveBtnClick(Sender: TObject);
Var
 FF : TColumn;
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
           FF := FindColumn(SLFN1.Strings[II]);
           if FF <> Nil Then FF.Visible        := False;
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

procedure TCVC.Button1Click(Sender: TObject);
begin
 ModalResult := mrOK;
end;

procedure TCVC.Button2Click(Sender: TObject);
begin
 ModalResult := mrCancel;
end;

Function TCVC.Execute(AGrid:TDBGrid):Boolean;
Var
 X  : Integer;
 FF : TColumn;
 DL : String;
 SL1: TStringList;
 SL2: TStringList;
Begin
 Result := False;
 Grid   := AGrid;
 SL1    := TStringList.Create;
 SL2    := TStringList.Create;
 SLFN1  := TStringList.Create;
 SLFN2  := TStringList.Create;
 Try
  For X := 0 To Grid.Columns.Count-1 do
      Begin
       if Grid.Columns[X].Visible Then
          SL1.Add(Grid.Columns[X].Title.Caption)
       Else
          SL2.Add(Grid.Columns[X].Title.Caption);
      End;
  SLFN1.Assign(SL1);
  SLFN2.Assign(SL2);
  ListBox1.Clear;
  ListBox2.Clear;
  For X := 0 To Grid.Columns.Count-1 do
     Begin
       DL := Grid.Columns[X].Title.Caption;
       if Grid.Columns[X].Visible Then
          Begin
            ListBox1.Items.Add(DL)
          End
       Else
          Begin
            ListBox2.Items.Add(DL);
          End;
     End;
  ShowModal;
  If ModalResult=mrOK Then
    Begin
     For X := 0 To SLFN1.Count-1 do
         Begin
          FF := FindColumn(SLFN1.Strings[X]);
          if FF <> NIL Then FF.Visible := True;
         End;
     For X := 0 To SLFN2.Count-1 do
         Begin
          FF := FindColumn(SLFN2.Strings[X]);
          if FF <> NIL Then FF.Visible := False;
         End;
     Result := True;
    End
 Else
    Begin
      For X := 0 To SL1.Count-1 do
         Begin
          FF := FindColumn(SL1.Strings[X]);
          if FF <> NIL Then FF.Visible := True;
         End;
     For X := 0 To SL2.Count-1 do
         Begin
          FF := FindColumn(SL2.Strings[X]);
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

procedure TCVC.CheckBox1Click(Sender: TObject);
Var
 X  : Integer;
 FF : TColumn;
begin
 if CheckBox1.Checked Then
    Begin
       For X := 0 To SLFN1.Count-1 do
         Begin
          FF := FindColumn(SLFN1.Strings[X]);
          if FF <> NIL Then FF.Visible := True;
         End;
       For X := 0 To SLFN2.Count-1 do
         Begin
          FF := FindColumn(SLFN2.Strings[X]);
          if FF <> NIL Then FF.Visible := False;
         End;
    End;
end;

end.
