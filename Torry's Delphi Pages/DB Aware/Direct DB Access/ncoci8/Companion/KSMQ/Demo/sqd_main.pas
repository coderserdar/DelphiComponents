{*************************************************************}
{              Simple Query pack version 2.0                  }
{             Copyright © 1998,99 Korzh company               }
{            http://www.korzh.com/simplequery.htm             }
{                   mailto:info@korzh.com                     }
{-------------------------------------------------------------}
{                        Demo project                         }
{                  last updated: Mar-23-2000                  }
{*************************************************************}


unit sqd_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, StdCtrls, kdbstruc, kquerpnl, Menus, DDLabel, Grids,
  DBGrids, ComCtrls, NCOci, NCOciWrapper, NCOciDB, NCOciKSMQInfo, Db,
  kdbinfo;

type
  TMainForm = class(TForm)
    Toolbar: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Savestructure1: TMenuItem;
    N2: TMenuItem;
    Savefilter1: TMenuItem;
    N3: TMenuItem;
    Filter1: TMenuItem;
    miActiveFilter: TMenuItem;
    BuildQuery1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Splitter1: TSplitter;
    Bevel1: TBevel;
    Bevel2: TBevel;
    sbActivateFilter: TSpeedButton;
    sbBuildQuery: TSpeedButton;
    sbOpenFilter: TSpeedButton;
    sbSaveFilter: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    sbSQL: TSpeedButton;
    pcSQL: TPageControl;
    tshSQL: TTabSheet;
    tshResult: TTabSheet;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Panel1: TPanel;
    memSQL: TMemo;
    Editresultfields1: TMenuItem;
    ddlRFields: TDropDownLabel;
    KDBStructure1: TKDBStructure;
    N4: TMenuItem;
    GroupBox1: TGroupBox;
    mDesc: TMemo;
    KQueryPanel1: TKQueryPanel;
    OciKSMQInfo1: TOciKSMQInfo;
    OCIQuery1: TOCIQuery;
    OCIDatabase1: TOCIDatabase;
    procedure sbEditStructureClick(Sender: TObject);
    procedure sbBuildQueryClick(Sender: TObject);
    procedure sbActivateFilterClick(Sender: TObject);
    procedure sbSQLClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure sbOpenFilterClick(Sender: TObject);
    procedure sbSaveFilterClick(Sender: TObject);
    procedure KQueryPanel1Active(Sender: TObject);
    procedure sbEditRFieldsClick(Sender: TObject);
    procedure KQueryPanel1CustomEdit(CurField: TDSField; var AValue: string);
    procedure ddlRFieldsCustomDropDown(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure KQueryPanel1ChangeValue(Field: TDSField; AValue: String;
      var NewCaption: String);
  private
    { Private declarations }
    procedure InitRFieldsLabel(var ACaption : String);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses About, DateDlg;

{$R *.DFM}

procedure TMainForm.sbEditStructureClick(Sender: TObject);
var
  Strings : TStrings;
begin
  // you have to save current query before structure changing
  // and then load it to prevent links loss to TDSTable, TDSField
  // and TDSOperat objects stored in TKDBStructure

  Strings := TStringList.Create;
  try
    KQueryPanel1.SaveToStrings(Strings);
    KDBStructure1.EditStructure;
    KQueryPanel1.LoadFromStrings(Strings);
    if KDBStructure1.DefaultMainTable <> nil then
      KQueryPanel1.MainTableName := KDBStructure1.DefaultMainTable.TableName;
  finally
    Strings.Free;
  end;
end;

procedure TMainForm.sbBuildQueryClick(Sender: TObject);
var
  i : integer;
begin
  Screen.Cursor := crHourGlass;
  try
    KQueryPanel1.BuildQuery;
    memSQL.Lines.Assign(KQueryPanel1.QResult.SQL);
    with OCIQuery1 do
    begin
      if Active then Close;
      SQL.Assign(KQueryPanel1.QResult.SQL);
      Open;
      for i := 0 to KQueryPanel1.QResult.RFields.Count - 1 do
        Fields[i].DisplayLabel := KQueryPanel1.QResult.RFields[i].Caption;
    end;
  finally;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.sbActivateFilterClick(Sender: TObject);
begin
  KQueryPanel1.Active := not KQueryPanel1.Active;
end;

procedure TMainForm.sbSQLClick(Sender: TObject);
begin
  pcSQL.Visible := (Sender as TSpeedButton).Down;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.sbOpenFilterClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then exit;
  KQueryPanel1.LoadFromFile(OpenDialog1.FileName);
  mDesc.Lines.Assign(KQueryPanel1.Description);
end;

procedure TMainForm.sbSaveFilterClick(Sender: TObject);
begin
  if not SaveDialog1.Execute then exit;
  KQueryPanel1.Description.Assign(mDesc.Lines);
  KQueryPanel1.SaveToFile(SaveDialog1.FileName);
end;


procedure TMainForm.KQueryPanel1Active(Sender: TObject);
var
  strLabel : String;
begin
  sbActivateFilter.Down  := KQueryPanel1.Active;
  miActiveFilter.Checked := KQueryPanel1.Active;
  InitRFieldsLabel(strLabel);
  ddlRFields.Caption := strLabel;
end;

procedure TMainForm.sbEditRFieldsClick(Sender: TObject);
var
  strLabel : String;
begin
  KQueryPanel1.EditQResult;
  InitRFieldsLabel(strLabel);
  ddlRFields.Caption := strLabel;
end;

procedure TMainForm.KQueryPanel1CustomEdit(CurField: TDSField;
  var AValue: string);
begin
  if (CompareText(CurField.DisplayName, 'SaleDate') = 0) or
     (CompareText(CurField.DisplayName, 'ShipDate') = 0) then
  begin
    DateDialog := TDateDialog.Create(nil);
    if AValue = '' then
      DateDialog.Date := Date
    else
      DateDialog.Date := StrToDate(AValue);
    DateDialog.ShowModal;

    if DateDialog.ModalResult = mrOK then
      AValue := DateToStr(DateDialog.Date);
    DateDialog.Free;
  end;
end;

procedure TMainForm.ddlRFieldsCustomDropDown(Sender: TObject);
var
  SCaption : string;
begin
  KQueryPanel1.EditQResult;
  InitRFieldsLabel(SCaption);
  (Sender as TDropDownLabel).Caption := SCaption;
end;

procedure TMainForm.InitRFieldsLabel(var ACaption : String);
var
  i : integer;
begin
  ACaption := '';
  with KQueryPanel1.Qresult do
  for i := 0 to RFields.Count - 1 do
    ACaption := ACaption + RFields[i].Caption + ' , ';
  if ACaption <> '' then Delete(ACaption,Length(ACaption)-2,3);
  if ACaption = '' then ACaption := '*';
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  KQueryPanel1.Active := True;
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  AboutBox := TAboutBox.Create(Application);
  AboutBox.ShowModal;
  AboutBox.Free;
end;


procedure TMainForm.KQueryPanel1ChangeValue(Field: TDSField;
  AValue: String; var NewCaption: String);
begin
 if (CompareText(Field.DisplayName, 'SaleDate') = 0) or
    (CompareText(Field.DisplayName, 'ShipDate') = 0) then
  try
    NewCaption := FormatDateTime('dddddd', StrToDate(AValue));
  except
    NewCaption := LabelSpace;
  end;
end;


end.
   