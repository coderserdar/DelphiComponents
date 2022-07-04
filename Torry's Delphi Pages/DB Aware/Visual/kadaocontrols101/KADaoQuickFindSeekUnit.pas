unit KADaoQuickFindSeekUnit;
{$I KADaoControlsCommonDirectives.pas}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, KDaoTable;

type
  TQFS = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    Table        : TKADaoTable;
    SeekMethod   : Boolean;
  public
    { Public declarations }
    Function Execute(DaoTable : TKADaoTable; Seek : Boolean; FieldName : String):Boolean;
  end;

var
  QFS: TQFS;

implementation
{$R *.DFM}
Uses DB, KADaoDTPickerUnit, ComCtrls;

Function TQFS.Execute(DaoTable : TKADaoTable; Seek : Boolean; FieldName : String):Boolean;
Var
 X : Integer;
Begin
 Result := False;
 ComboBox1.Clear;
 if NOT Seek Then
    Begin
     Self.Caption := ' Find a Record based on data contained in the field';
     For X := 0 to DaoTable.FieldCount-1 do
      Begin
       if (DaoTable.Fields[X].FieldKind=fkData) And (DaoTable.Fields[X].Visible)  Then
         Begin
          ComboBox1.Items.Add(DaoTable.Fields[X].FieldName);
         End;
      End;
      ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(FieldName);
    End
 Else
    Begin
     Self.Caption := ' Seek a Record on data in the field part of an Index';
     For X := 0 to DaoTable.IndexFieldCount-1 do
      Begin
       if (DaoTable.IndexFields[X].FieldKind=fkData)  And (DaoTable.IndexFields[X].Visible) Then
         Begin
          ComboBox1.Items.Add(DaoTable.IndexFields[X].FieldName);
         End;
      End;
      ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(FieldName);
      if ComboBox1.ItemIndex = -1 Then ComboBox1.ItemIndex:=0;
    End;
 Table      := DaoTable;
 SeekMethod := Seek;
 ComboBox1Change(ComboBox1);
 ShowModal;
 if ModalResult = mrOK Then Result := True;
End;

procedure TQFS.Button1Click(Sender: TObject);
Var
  DT  : TdateTime;
  P   : TPoint;
  Res : Boolean;
begin
  Application.CreateForm(TDTPicker, DTPicker);
  P.X := (Sender as TButton).Left;
  P.Y := (Sender as TButton).Top;
  P   := Self.ClientToScreen(P);
  DTPicker.Left  := P.X-((DTPicker.Width-(Sender as TButton).Width));
  DTPicker.Top   := P.Y+(Sender as TButton).Height+2;
  if (Sender as TButton).Caption='..' Then
    Begin
      Res := DTPicker.Execute(DT,dtkTime);
      if Res Then Edit1.Text := FormatDateTime('hh'+TimeSeparator+'nn'+TimeSeparator+'ss',DT);
    End
  Else
    Begin
      Res := DTPicker.Execute(DT,dtkDate);
      if Res Then Edit1.Text := FormatDateTime('dd'+DateSeparator+'mm'+DateSeparator+'yyyy',DT);
    End;
  DTPicker.Free;
end;

procedure TQFS.Button4Click(Sender: TObject);
Var
 KeyFields : string;
 KeyValues : Variant;
 Options   : TLocateOptions;
begin
 KeyFields := ComboBox1.Text;
 KeyValues := Edit1.Text;
 Options   := [loCaseInsensitive, loPartialKey];
 if NOt SeekMethod Then
    Begin
      if Not Table.Find_Next (KeyFields, KeyValues, Options) Then
             Table.Find_First(KeyFields, KeyValues, Options);
      VarClear(KeyValues);
    End
 Else
    Begin
      Table.Locate(KeyFields, KeyValues, Options);
      VarClear(KeyValues);
    End;
end;

procedure TQFS.Button3Click(Sender: TObject);
begin
 ModalResult := mrCancel;
end;

procedure TQFS.Button2Click(Sender: TObject);
begin
 Button4.Click;
 ModalResult := mrOk;
end;

procedure TQFS.ComboBox1Change(Sender: TObject);
Var
 FF : TField;
begin
 Edit1.Text := '';
 if ComboBox1.ItemIndex=-1 Then Exit;
 FF := Table.FindField(ComboBox1.Text);
 if FF=Nil Then Exit;
 if FF.DataType=ftDate     Then
    Begin
       Button1.Enabled := True;
       Button1.Caption := '...';
       Edit1.Text := 'dd'+DateSeparator+'mm'+DateSeparator+'yyyy';
    End
 Else
 if FF.DataType=ftTime     Then
    Begin
       Button1.Enabled := True;
       Button1.Caption := '..';
       Edit1.Text := 'hh'+TimeSeparator+'mm'+TimeSeparator+'ss';
    End
 Else
    Begin
      Edit1.Text := '';
      Button1.Caption := '...';
      Button1.Enabled := False;
    End;
end;

procedure TQFS.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
 if Key=#13 then
    Begin
     Button4.Click;
     Key:=#0;
    End;
end;

end.
