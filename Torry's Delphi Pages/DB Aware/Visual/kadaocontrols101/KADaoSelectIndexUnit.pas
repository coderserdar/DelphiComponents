unit KADaoSelectIndexUnit;
{$I KADaoControlsCommonDirectives.pas}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, KDaoDatabase, KDaoTable, DaoApi;

type
  TKADaoSelectIndex = class(TForm)
    Label1: TLabel;
    StringGrid1: TStringGrid;
    ComboBox1: TComboBox;
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    procedure ComboBox1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    Database  : TKADaoDatabase;
    Table     : TKADaoTable;
  public
    { Public declarations }
    Function Execute(DaoTable : TKAdaoTable;Var IndexName:String):Boolean;
  end;


var
  KADaoSelectIndex: TKADaoSelectIndex;

implementation
{$R *.dfm}
Const
  EmtyIndex : String = 'Don''t use an Index';

Function TKADaoSelectIndex.Execute(DaoTable : TKAdaoTable;Var IndexName:String):Boolean;
Var
 X : Integer;
 S : String;
Begin
 Result   := False;
 Table    := DaoTable;
 Database := DaoTable.Database;
 ComboBox1.Clear;
 ComboBox1.Items.Add(EmtyIndex);
 For X := 0 To Database.CoreDatabase.TableDefs.Item[Table.TableName].Indexes.Count-1 do
     Begin
      ComboBox1.Items.Add(Database.CoreDatabase.TableDefs.Item[Table.TableName].Indexes.Item[X].Name);
     End;
 ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf(IndexName);
 if ComboBox1.ItemIndex=-1 Then ComboBox1.ItemIndex := 0;
 StringGrid1.ColWidths[0]:=((StringGrid1.Width-5) div 2);
 StringGrid1.ColWidths[1]:=StringGrid1.ColWidths[0];
 StringGrid1.Cells[0,0]:='Field Name';
 StringGrid1.Cells[1,0]:='Sort Order';
 StringGrid1.RowCount:=2;
 StringGrid1.Rows[1].Clear;
 ComboBox1Change(ComboBox1);
 ShowModal;
 if ModalResult=mrOK Then
    Begin
      Result    := True;
      S         := ComboBox1.Items[ComboBox1.ItemIndex];
      if S = EmtyIndex Then S := '';
      IndexName := S;
    End;
End;

procedure TKADaoSelectIndex.ComboBox1Change(Sender: TObject);
Var
  Prop : OleVariant;
  RC   : Integer;
  X    : Integer;
  S    : String;
begin
 StringGrid1.RowCount:=2;
 StringGrid1.Rows[1].Clear;
 if ComboBox1.ItemIndex<>-1 Then
    Begin
      S:= ComboBox1.Items[ComboBox1.ItemIndex];
      if S=EmtyIndex Then S := '';
      If CheckBox1.Checked Then Table.IndexName := S;
      if S <> '' Then
         Begin
          Prop := OleVariant(Database.CoreDatabase.TableDefs.Item[Table.TableName].Indexes.Item[S]);
          RC := Prop.Fields.Count;
          if RC=0 Then RC := 1;
          StringGrid1.RowCount := RC+1;
          For X := 0 To Prop.Fields.Count-1 do
            Begin
              StringGrid1.Cells[0,X+1]:=Prop.Fields.Item[X].Name;
              if (Prop.Fields.Item[X].Attributes And dbDescending) > 0 Then
                  StringGrid1.Cells[1,X+1]:='Descending'
              Else
                  StringGrid1.Cells[1,X+1]:='Ascending';
            End;
         End;
    End;
End;

procedure TKADaoSelectIndex.Button2Click(Sender: TObject);
begin
 ModalResult := mrCancel;
end;

procedure TKADaoSelectIndex.Button1Click(Sender: TObject);
begin
 ModalResult := mrOK;
end;



end.
