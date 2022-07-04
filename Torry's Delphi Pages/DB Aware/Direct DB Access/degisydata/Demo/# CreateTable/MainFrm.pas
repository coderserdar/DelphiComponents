{----------------------------------------------------------------}
{                                                                }
{  Degisy Data Demo Unit                                         }
{  Writen by Alexander Momot                                     }
{  http://www.degisy.com                                         }
{  (c)2001-2004 Degisy Software                                  }
{                                                                }
{----------------------------------------------------------------}
unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DsPdxTable, DsDbfTable, DsDdaTable, DB, DsClrTable,
  DsDatabase;

type
  TMainForm = class(TForm)
    CB_TABLETYPE: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    DemoDb: TDsDatabase;
    TBL_CLR_1: TDsClrTable;
    TBL_DDA_1: TDsDdaTable;
    TBL_DBF_1: TDsDbfTable;
    TBL_PDX_1: TDsPdxTable;
    TBL_CLR_2: TDsClrTable;
    TBL_DDA_2: TDsDdaTable;
    TBL_DBF_2: TDsDbfTable;
    TBL_PDX_2: TDsPdxTable;
    TBL_CLR_1IDENT: TIntegerField;
    TBL_CLR_1CODE: TStringField;
    TBL_CLR_1PHONE: TStringField;
    TBL_CLR_1COUNTRY: TStringField;
    TBL_CLR_1STATE: TStringField;
    TBL_DDA_1IDENT: TIntegerField;
    TBL_DDA_1CODE: TStringField;
    TBL_DDA_1PHONE: TStringField;
    TBL_DDA_1COUNTRY: TStringField;
    TBL_DDA_1STATE: TStringField;
    TBL_DDA_1AUTO: TAutoIncField;
    TBL_DBF_1IDENT: TIntegerField;
    TBL_DBF_1CODE: TStringField;
    TBL_DBF_1PHONE: TStringField;
    TBL_DBF_1COUNTRY: TStringField;
    TBL_DBF_1STATE: TStringField;
    TBL_PDX_1IDENT: TIntegerField;
    TBL_PDX_1CODE: TStringField;
    TBL_PDX_1PHONE: TStringField;
    TBL_PDX_1COUNTRY: TStringField;
    TBL_PDX_1STATE: TStringField;
    TBL_PDX_1AUTO: TAutoIncField;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    procedure LoadTypesList;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

type
  TDsTableType = (
    ttClarion2,
    ttdBase3,
    ttdBase4,
    ttdBase5,
    ttdBase7,
    ttDegisy,
    ttParadox3,
    ttParadox4,
    ttParadox5,
    ttParadox7
    );

const
  TableTypeNames: array[TDsTableType]of PChar = (
    'Clarion 2.1',
    'dBase III',
    'dBase IV',
    'dBase V',
    'dBase VII',
    'Degisy 1.0',
    'Paradox 3.5',
    'Paradox 4.0',
    'Paradox 5.0',
    'Paradox 7.0'
    );

  TableTypeLevels: array[TDsTableType]of Word = (
    2,  //ttClarion2
    3,  //ttdBase3
    4,  //ttdBase4
    5,  //ttdBase5
    7,  //ttdBase7
    1,  //ttDegisy
    3,  //ttParadox3
    4,  //ttParadox4
    5,  //ttParadox5
    7   //ttParadox7
    );

{ TMainForm }

procedure TMainForm.LoadTypesList;
var
  I: TDsTableType;
begin
  CB_TABLETYPE.Clear;
  for I := Low(TDsTableType)to High(TDsTableType)do
   CB_TABLETYPE.Items.Add(StrPas(TableTypeNames[I]));
  CB_TABLETYPE.ItemIndex := 0; 
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LoadTypesList();
end;

//------------------------------------------------------------------------------

procedure TMainForm.Button1Click(Sender: TObject);
begin
  case( TDsTableType(CB_TABLETYPE.ItemIndex))of
  ttClarion2:
    begin
     TBL_CLR_1.CreateTable;
    end;
  ttdBase3..ttdBase7:
    begin
     TBL_DBF_1.TableLevel := TableTypeLevels[TDsTableType(CB_TABLETYPE.ItemIndex)];
     TBL_DBF_1.CreateTable;
    end;
  ttDegisy:
    begin
     TBL_DDA_1.CreateTable;
    end;
  ttParadox3..ttParadox7:
    begin
     TBL_PDX_1.TableLevel := TableTypeLevels[TDsTableType(CB_TABLETYPE.ItemIndex)];
     TBL_PDX_1.CreateTable;
    end;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  case( TDsTableType(CB_TABLETYPE.ItemIndex))of
  ttClarion2:
    begin
     TBL_CLR_2.FieldDefs.Clear;
     with TBL_CLR_2.FieldDefs.AddFieldDef do
     begin
      Name := 'IDENT';
      DataType := ftInteger;
     end;
     with TBL_CLR_2.FieldDefs.AddFieldDef do
     begin
      Name := 'CODE';
      DataType := ftString;
      Size := 10;
     end;
     with TBL_CLR_2.FieldDefs.AddFieldDef do
     begin
      Name := 'PHONE';
      DataType := ftString;
      Size := 20;
     end;
     with TBL_CLR_2.FieldDefs.AddFieldDef do
     begin
      Name := 'COUNTRY';
      DataType := ftString;
      Size := 30;
     end;
     with TBL_CLR_2.FieldDefs.AddFieldDef do
     begin
      Name := 'STATE';
      DataType := ftString;
      Size := 20;
     end;
     TBL_CLR_2.CreateTable;
    end;
  ttdBase3..ttdBase7:
    begin
     TBL_DBF_2.FieldDefs.Clear;
     with TBL_DBF_2.FieldDefs.AddFieldDef do
     begin
      Name := 'IDENT';
      DataType := ftInteger;
     end;
     with TBL_DBF_2.FieldDefs.AddFieldDef do
     begin
      Name := 'CODE';
      DataType := ftString;
      Size := 10;
     end;
     with TBL_DBF_2.FieldDefs.AddFieldDef do
     begin
      Name := 'PHONE';
      DataType := ftString;
      Size := 20;
     end;
     with TBL_DBF_2.FieldDefs.AddFieldDef do
     begin
      Name := 'COUNTRY';
      DataType := ftString;
      Size := 30;
     end;
     with TBL_DBF_2.FieldDefs.AddFieldDef do
     begin
      Name := 'STATE';
      DataType := ftString;
      Size := 20;
     end;
     TBL_DBF_2.TableLevel := TableTypeLevels[TDsTableType(CB_TABLETYPE.ItemIndex)];
     TBL_DBF_2.CreateTable;
    end;
  ttDegisy:
    begin
     TBL_DDA_2.FieldDefs.Clear;
     with TBL_DDA_2.FieldDefs.AddFieldDef do
     begin
      Name := 'AUTO';
      DataType := ftAutoInc;
     end;
     with TBL_DDA_2.FieldDefs.AddFieldDef do
     begin
      Name := 'IDENT';
      DataType := ftInteger;
     end;
     with TBL_DDA_2.FieldDefs.AddFieldDef do
     begin
      Name := 'CODE';
      DataType := ftString;
      Size := 10;
     end;
     with TBL_DDA_2.FieldDefs.AddFieldDef do
     begin
      Name := 'PHONE';
      DataType := ftString;
      Size := 20;
     end;
     with TBL_DDA_2.FieldDefs.AddFieldDef do
     begin
      Name := 'COUNTRY';
      DataType := ftString;
      Size := 30;
     end;
     with TBL_DDA_2.FieldDefs.AddFieldDef do
     begin
      Name := 'STATE';
      DataType := ftString;
      Size := 20;
     end;
     TBL_DDA_2.CreateTable;
    end;
  ttParadox3..ttParadox7:
    begin
     TBL_PDX_2.FieldDefs.Clear;
     with TBL_PDX_2.FieldDefs.AddFieldDef do
     begin
      Name := 'AUTO';
      DataType := ftAutoInc;
     end;
     with TBL_PDX_2.FieldDefs.AddFieldDef do
     begin
      Name := 'IDENT';
      DataType := ftInteger;
     end;
     with TBL_PDX_2.FieldDefs.AddFieldDef do
     begin
      Name := 'CODE';
      DataType := ftString;
      Size := 10;
     end;
     with TBL_PDX_2.FieldDefs.AddFieldDef do
     begin
      Name := 'PHONE';
      DataType := ftString;
      Size := 20;
     end;
     with TBL_PDX_2.FieldDefs.AddFieldDef do
     begin
      Name := 'COUNTRY';
      DataType := ftString;
      Size := 30;
     end;
     with TBL_PDX_2.FieldDefs.AddFieldDef do
     begin
      Name := 'STATE';
      DataType := ftString;
      Size := 20;
     end;
     TBL_PDX_2.TableLevel := TableTypeLevels[TDsTableType(CB_TABLETYPE.ItemIndex)];
     TBL_PDX_2.CreateTable;
    end;
  end;
end;

end.
