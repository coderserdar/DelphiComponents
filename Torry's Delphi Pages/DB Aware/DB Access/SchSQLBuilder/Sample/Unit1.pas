unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uSchSQLBuilder, ExtCtrls;

type
  TForm1 = class(TForm)
    mFields: TMemo;
    mTables: TMemo;
    mWhere: TMemo;
    l1: TLabel;
    l2: TLabel;
    l3: TLabel;
    btn1: TButton;
    mResults: TMemo;
    l4: TLabel;
    mOrderBy: TMemo;
    l5: TLabel;
    cbDistinct: TCheckBox;
    cbUseTableAlias: TCheckBox;
    SchSQLBuilder1: TSchSQLBuilder;
    cbUseFieldAlias: TCheckBox;
    cbUseGenFieldAlias: TCheckBox;
    mJoin: TMemo;
    l6: TLabel;
    rg1: TRadioGroup;
    procedure btn1Click(Sender: TObject);
    procedure cbDistinctClick(Sender: TObject);
    procedure cbUseTableAliasClick(Sender: TObject);
    procedure cbUseFieldAliasClick(Sender: TObject);
    procedure cbUseGenFieldAliasClick(Sender: TObject);
    procedure SchSQLBuilder1Error(Sender: TObject;
      ErrorCode: TSchSimpleSQLBuilderError; ErrMessage: String);
    procedure rg1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  iTemp: Integer;
begin
  mResults.Lines.Clear;
  SchSQLBuilder1.ResetLists;

  for iTemp:=0 to mFields.Lines.Count-1 do
  begin
    case iTemp of
      0: SchSQLBuilder1.AddField(mFields.Lines[iTemp],sbsaCount);
      1: SchSQLBuilder1.AddField(mFields.Lines[iTemp],sbsaNone,'This is an alias');
      else SchSQLBuilder1.AddField(mFields.Lines[iTemp],sbsaNone);
    end;
  end;

  for iTemp:=0 to mTables.Lines.Count-1 do
    SchSQLBuilder1.AddTable(mTables.Lines[iTemp],'T'+IntToStr(iTemp+1));

  for iTemp:=0 to mJoin.Lines.Count-1 do
    SchSQLBuilder1.AddJoin(mJoin.Lines[iTemp]);

  for iTemp:=0 to mWhere.Lines.Count-1 do
    SchSQLBuilder1.AddWhere(mWhere.Lines[iTemp]);

  for iTemp:=0 to mOrderBy.Lines.Count-1 do
    if iTemp=0
      then SchSQLBuilder1.AddOrderBy(mOrderBy.Lines[iTemp], sbsoDesc)
      else SchSQLBuilder1.AddOrderBy(mOrderBy.Lines[iTemp]);

  SchSQLBuilder1.BuildSQL;
  mResults.Lines.Assign(SchSQLBuilder1.SQLResult);
end;

procedure TForm1.cbDistinctClick(Sender: TObject);
begin
  SchSQLBuilder1.Distinct:=cbDistinct.Checked;
end;

procedure TForm1.cbUseTableAliasClick(Sender: TObject);
begin
  SchSQLBuilder1.UseTableAlias:=cbUseTableAlias.Checked;
end;

procedure TForm1.cbUseFieldAliasClick(Sender: TObject);
begin
  SchSQLBuilder1.UseFieldAlias:=cbUseFieldAlias.Checked;
end;

procedure TForm1.cbUseGenFieldAliasClick(Sender: TObject);
begin
  SchSQLBuilder1.UseGenFieldAlias:=cbUseGenFieldAlias.Checked;
end;

procedure TForm1.SchSQLBuilder1Error(Sender: TObject;
  ErrorCode: TSchSimpleSQLBuilderError; ErrMessage: String);
begin
  if ErrorCode<>sbecOK then ShowMessage(ErrMessage);
end;

procedure TForm1.rg1Click(Sender: TObject);
begin
  case rg1.ItemIndex of
    0: SchSQLBuilder1.Language:=sblaEnglish;
    1: SchSQLBuilder1.Language:=sblaHungarian;
  end;
end;

end.
