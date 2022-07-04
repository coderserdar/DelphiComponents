{--------------------------------------------------------------------------------
* Description : Examples how to use the TscExcelExport component
* Author : Stefan Cruysberghs
* Email : stefancr@scip.be
* Website : http://www.scip.be
--------------------------------------------------------------------------------}

unit MainExcelExport;

interface

{$Include scExcelExportConfig.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, DBTables, scExcelExport, Grids, DBGrids, ExtCtrls, Buttons,
  ComCtrls, OleServer, ImgList, DBClient,
  {$IFNDEF DELPHI5}
  Variants, jpeg,
  {$ENDIF}
  {$IFDEF EXCEL97}
  Excel97;
  {$ENDIF}
  {$IFDEF EXCEL2000}
  Excel2000;
  {$ENDIF}
  {$IFDEF EXCELXP}
  ExcelXP;
  {$ENDIF}

type
  TFormExcelExport = class(TForm)
    TableOrders: TTable;
    DataSourceOrders: TDataSource;
    TableAnimals: TTable;
    TableBiolife: TTable;
    StatusBar: TStatusBar;
    PanelTitle: TPanel;
    scExcelExport1: TscExcelExport;
    QuerySortShipVia: TQuery;
    TableOrdersOrderNo: TFloatField;
    TableOrdersCustNo: TFloatField;
    TableOrdersSaleDate: TDateTimeField;
    TableOrdersShipDate: TDateTimeField;
    TableOrdersEmpNo: TIntegerField;
    TableOrdersShipToContact: TStringField;
    TableOrdersShipToAddr1: TStringField;
    TableOrdersShipToAddr2: TStringField;
    TableOrdersShipToCity: TStringField;
    TableOrdersShipToState: TStringField;
    TableOrdersShipToZip: TStringField;
    TableOrdersShipToCountry: TStringField;
    TableOrdersShipToPhone: TStringField;
    TableOrdersShipVIA: TStringField;
    TableOrdersPO: TStringField;
    TableOrdersTerms: TStringField;
    TableOrdersPaymentMethod: TStringField;
    TableOrdersItemsTotal: TCurrencyField;
    TableOrdersTaxRate: TFloatField;
    TableOrdersFreight: TCurrencyField;
    TableOrdersAmountPaid: TCurrencyField;
    DataSourceAnimals: TDataSource;
    TableAnimalsNAME: TStringField;
    TableAnimalsSIZE: TSmallintField;
    TableAnimalsWEIGHT: TSmallintField;
    TableAnimalsAREA: TStringField;
    TableAnimalsBMP: TBlobField;
    TableAnimalsCalcField: TFloatField;
    PageControlExcelExport: TPageControl;
    DataSourceBiolife: TDataSource;
    TableAnimalsAreaText: TStringField;
    TableBiolifeSpeciesNo: TFloatField;
    TableBiolifeCategory: TStringField;
    TableBiolifeCommon_Name: TStringField;
    TableBiolifeSpeciesName: TStringField;
    TableBiolifeLengthcm: TFloatField;
    TableBiolifeLength_In: TFloatField;
    TableBiolifeNotes: TMemoField;
    TableBiolifeGraphic: TGraphicField;
    ImageExcelExportLogo: TImage;
    LabelTitle1: TLabel;
    LabelAuthor: TLabel;
    LabelWebsite: TLabel;
    LabelRegister: TLabel;
    LabelDelphiVersions: TLabel;
    LabelBDE: TLabel;
    scExcelExport2: TscExcelExport;
    TableAnimalsSizeText: TStringField;
    ImageListTabs: TImageList;
    TableAnimalsWeight2: TFloatField;
    TableAnimalsWeight3: TFloatField;
    TableAnimalsWeight4: TStringField;
    TableAnimalsWeight5: TCurrencyField;
    TableAnimalsWeight6: TCurrencyField;
    LabelExtraInfo: TLabel;
    TableAnimalsTime1: TDateTimeField;
    TableAnimalsTime2: TDateTimeField;
    TableAnimalsTime3: TDateTimeField;
    TableAnimalsWeight7: TFloatField;
    TableAnimalsTime4: TDateTimeField;
    TableAnimalsTime5: TDateTimeField;
    TableAnimalsTime6: TDateField;
    LabelExcelVersions: TLabel;
    TabSheetDemos: TTabSheet;
    TabSheetDatasets: TTabSheet;
    PageControlDemos: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    PageControlDatasets: TPageControl;
    TabSheetTableOrders: TTabSheet;
    TabSheetTableAnimals: TTabSheet;
    PanelOrders: TPanel;
    DBGridOrders: TDBGrid;
    DBGridAnimals: TDBGrid;
    PanelAnimals: TPanel;
    TabSheetTableBiolife: TTabSheet;
    PanelBiolife: TPanel;
    DBGridBiolife: TDBGrid;
    TabSheetQuerySortShipVia: TTabSheet;
    PanelSortShipVia: TPanel;
    DBGridSortShipVia: TDBGrid;
    DataSourceSortShipVia: TDataSource;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    TabSheet12: TTabSheet;
    TabSheet13: TTabSheet;
    TabSheet14: TTabSheet;
    TabSheet15: TTabSheet;
    TabSheet16: TTabSheet;
    TabSheet17: TTabSheet;
    TabSheet18: TTabSheet;
    PanelNumber1: TPanel;
    PanelTitle1: TPanel;
    Memo1: TMemo;
    BitBtnExecute1: TBitBtn;
    PanelNumber2: TPanel;
    PanelTitle2: TPanel;
    Memo2: TMemo;
    BitBtnExecute2: TBitBtn;
    PanelNumber3: TPanel;
    PanelTitle3: TPanel;
    Memo3: TMemo;
    BitBtnExecute3: TBitBtn;
    PanelNumber4: TPanel;
    PanelTitle4: TPanel;
    Memo4: TMemo;
    BitBtnExecute4: TBitBtn;
    PanelNumber5: TPanel;
    PanelTitle5: TPanel;
    Memo5: TMemo;
    BitBtnExecute5: TBitBtn;
    Panel9: TPanel;
    PanelTitle6: TPanel;
    Memo6: TMemo;
    BitBtnExecute6: TBitBtn;
    Panel11: TPanel;
    PanelTitle7: TPanel;
    Memo7: TMemo;
    BitBtnExecute7: TBitBtn;
    Panel13: TPanel;
    PanelTitle8: TPanel;
    Memo8: TMemo;
    BitBtnExecute8: TBitBtn;
    PanelNumber9: TPanel;
    PanelTitle9: TPanel;
    Memo9: TMemo;
    BitBtnExecute9: TBitBtn;
    PanelNumber10: TPanel;
    PanelTitle10: TPanel;
    Memo10: TMemo;
    BitBtnExecute10: TBitBtn;
    PanelNumber11: TPanel;
    PanelTitle11: TPanel;
    Memo11: TMemo;
    BitBtnExecute11: TBitBtn;
    PanelNumber12: TPanel;
    PanelTitle12: TPanel;
    Memo12: TMemo;
    BitBtnExecute12: TBitBtn;
    PanelNumber13: TPanel;
    PanelTitle13: TPanel;
    Memo13: TMemo;
    BitBtnExecute13: TBitBtn;
    PanelNumber14: TPanel;
    PanelTitle14: TPanel;
    Memo14: TMemo;
    BitBtnExecute14: TBitBtn;
    Panel27: TPanel;
    PanelTitle15: TPanel;
    Memo15: TMemo;
    BitBtnExecute15: TBitBtn;
    Panel29: TPanel;
    PanelTitle16: TPanel;
    Memo16: TMemo;
    BitBtnExecute16: TBitBtn;
    Panel31: TPanel;
    PanelTitle17: TPanel;
    Memo17: TMemo;
    BitBtnExecute17: TBitBtn;
    Panel33: TPanel;
    Panel34: TPanel;
    Memo18: TMemo;
    BitBtnExecute18: TBitBtn;
    Image13: TImage;
    Image2: TImage;
    Image6: TImage;
    Image7: TImage;
    Image12: TImage;
    Image9: TImage;
    Bevel9: TBevel;
    Bevel2: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel12: TBevel;
    Bevel13: TBevel;
    DatabaseDBDemos: TDatabase;
    Bevel14: TBevel;
    Image14: TImage;
    Bevel15: TBevel;
    Image15: TImage;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BitBtnExecute18Click(Sender: TObject);
    procedure BitBtnExecute17Click(Sender: TObject);
    procedure BitBtnExecute15Click(Sender: TObject);
    procedure BitBtnExecute16Click(Sender: TObject);
    procedure BitBtnExecute14Click(Sender: TObject);
    procedure BitBtnExecute13Click(Sender: TObject);
    procedure BitBtnExecute12Click(Sender: TObject);
    procedure BitBtnExecute11Click(Sender: TObject);
    procedure BitBtnExecute10Click(Sender: TObject);
    procedure BitBtnExecute9Click(Sender: TObject);
    procedure BitBtnExecute8Click(Sender: TObject);
    procedure BitBtnExecute7Click(Sender: TObject);
    procedure BitBtnExecute6Click(Sender: TObject);
    procedure BitBtnExecute5Click(Sender: TObject);
    procedure BitBtnExecute4Click(Sender: TObject);
    procedure BitBtnExecute3Click(Sender: TObject);
    procedure BitBtnExecute2Click(Sender: TObject);
    procedure BitBtnExecute1Click(Sender: TObject);
    procedure scExcelExport2GetFieldCellStyleEvent(Sender: TObject; const IntFieldIndex: Integer;
      var ColorBackground: TColor; FontCell: TxlFont);
    procedure scExcelExport1ExportRecords(Sender: TObject; IntRecordNumber: Integer);
    procedure scExcelExport2GetEOF(Sender: TObject; var BlnEOF: Boolean);
    procedure scExcelExport2GetFieldCount(Sender: TObject;
      var IntFieldCount: Integer);
    procedure scExcelExport2GetFieldDataSize(Sender: TObject;
      const FieldIndex: Integer; var IntFieldDataSize: Integer);
    procedure scExcelExport2GetFieldDataType(Sender: TObject;
      const FieldIndex: Integer; var FieldDataType: TFieldType);
    procedure scExcelExport2GetFieldDisplayName(Sender: TObject;
      const FieldIndex: Integer; var StrFieldDisplayName: String);
    procedure scExcelExport2GetFieldName(Sender: TObject;
      const FieldIndex: Integer; var StrFieldName: String);
    procedure scExcelExport2GetFieldValue(Sender: TObject;
      const FieldIndex: Integer; var VarValue: Variant);
    procedure scExcelExport2GetFieldVisible(Sender: TObject;
      const FieldIndex: Integer; var BlnFieldVisible: Boolean);
    procedure scExcelExport2ExportRecords(Sender: TObject;
      IntRecordNumber: Integer);
    procedure scExcelExport2GotoFirstRecord(Sender: TObject);
    procedure scExcelExport2GotoNextRecord(Sender: TObject);
    procedure TableAnimalsCalcFields(DataSet: TDataSet);
    procedure LabelWebsiteClick(Sender: TObject);
  private
    procedure TableBiolifeNotesGetText(Sender: TField; var Text: String;
      DisplayText: Boolean);
    procedure ChangeCellColors(Sender: TObject; Field: TField; var ColorBackground : TColor; FontCell : TxlFont);
  public
  end;

var
  FormExcelExport: TFormExcelExport;

implementation

uses ShellAPI, {$IFNDEF DELPHI5}DateUtils,{$ENDIF} ActiveX;

{$R *.DFM}

//-----------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport1ExportRecords(Sender: TObject;
  IntRecordNumber: Integer);
begin
  StatusBar.Panels[0].Text := 'Records : '+IntToStr(IntRecordNumber);
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.ChangeCellColors(Sender: TObject; Field: TField; var ColorBackground : TColor; FontCell : TxlFont);
begin
  if Field.FieldName = 'CustNo' then
  begin
    if Field.Value > 2000 then
    begin
      FontCell.Color := clRed;
      FontCell.Name  := 'Times New Roman';
      FontCell.Size := 14;
    end;
    if Field.Value > 3000 then
    begin
      FontCell.Color := clGreen;
      FontCell.Style := [fsBold];
    end;
  end;

  if Field.FieldName = 'EmpNo' then
  begin
    if Field.Dataset.FieldByName('CustNo').Value > 2000 then
      ColorBackground := clRed;
  end;

  if Field.DataSet.FieldByName('EmpNo').Value > 100 then
    ColorBackground := clYellow;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.FormCreate(Sender: TObject);
begin
  try
    if not DatabaseDBDemos.Connected then
      DatabaseDBDemos.Open;
    if not TableOrders.Active then
      TableOrders.Open;
    if not TableAnimals.Active then
      TableAnimals.Open;
    if not TableBiolife.Active then
      TableBiolife.Open;
    if not QuerySortShipVia.Active then
      QuerySortShipVia.Open;

    StatusBar.Panels[2].Text := 'Connection with BDE ' + DatabaseDBDemos.AliasName +' tables';
  except
    on E: Exception do
    begin
      BitBtnExecute1.Enabled := False;
      BitBtnExecute2.Enabled := False;
      BitBtnExecute3.Enabled := False;
      BitBtnExecute4.Enabled := False;
      BitBtnExecute5.Enabled := False;
      BitBtnExecute6.Enabled := False;
      BitBtnExecute7.Enabled := False;
      BitBtnExecute8.Enabled := False;
      BitBtnExecute9.Enabled := False;
      BitBtnExecute10.Enabled := False;
      BitBtnExecute11.Enabled := False;
      BitBtnExecute12.Enabled := False;
      BitBtnExecute13.Enabled := False;
      BitBtnExecute14.Enabled := False;
      BitBtnExecute15.Enabled := False;
      BitBtnExecute16.Enabled := False;

      StatusBar.Panels[2].Text := 'NO connection with BDE ' + DatabaseDBDemos.AliasName +' tables !';

      MessageDlg('NO connection with the BDE ' + DatabaseDBDemos.AliasName +' tables could be established !'+#13+#10+#13+#10+E.Message, mtError, [mbOK], 0);
    end;
  end;

  PageControlExcelExport.ActivePage := TabSheetDemos;
  PageControlDemos.ActivePage := TabSheet1;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if DatabaseDBDemos.Connected then
    DatabaseDBDemos.Close;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport2GetEOF(Sender: TObject; var BlnEOF: Boolean);
begin
  BlnEOF := QuerySortShipVia.Eof;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport2GetFieldCount(Sender: TObject;
  var IntFieldCount: Integer);
begin
  IntFieldCount := QuerySortShipVia.FieldCount;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport2GetFieldDataSize(Sender: TObject;
  const FieldIndex: Integer; var IntFieldDataSize: Integer);
begin
  IntFieldDataSize := QuerySortShipVia.Fields[FieldIndex].Size;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport2GetFieldDataType(Sender: TObject;
  const FieldIndex: Integer; var FieldDataType: TFieldType);
begin
  FieldDataType := QuerySortShipVia.Fields[FieldIndex].DataType;

  if SameText(QuerySortShipVia.Fields[FieldIndex].FieldName,'NewOrderNo') then
    FieldDataType := ftString;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport2GetFieldDisplayName(
  Sender: TObject; const FieldIndex: Integer;
  var StrFieldDisplayName: String);
begin
  StrFieldDisplayName := '* '+QuerySortShipVia.Fields[FieldIndex].FieldName+' *';
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport2GetFieldName(Sender: TObject;
  const FieldIndex: Integer; var StrFieldName: String);
begin
  StrFieldName := QuerySortShipVia.Fields[FieldIndex].FieldName;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport2GetFieldValue(Sender: TObject;
  const FieldIndex: Integer; var VarValue: Variant);
begin
  VarValue := QuerySortShipVia.Fields[FieldIndex].Value;

  if SameText(QuerySortShipVia.Fields[FieldIndex].FieldName,'NewOrderNo') then
    VarValue := '00'+QuerySortShipVia.Fields[FieldIndex].AsString;
end;

//------------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport2GotoFirstRecord(Sender: TObject);
begin
  QuerySortShipVia.First;
end;

//------------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport2GotoNextRecord(Sender: TObject);
begin
  QuerySortShipVia.Next;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport2GetFieldVisible(Sender: TObject;
  const FieldIndex: Integer; var BlnFieldVisible: Boolean);
begin
  BlnFieldVisible := True;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport2ExportRecords(Sender: TObject;
  IntRecordNumber: Integer);
begin
  StatusBar.Panels[0].Text := 'Records : '+IntToStr(IntRecordNumber);
end;

//------------------------------------------------------------------------------
procedure TFormExcelExport.scExcelExport2GetFieldCellStyleEvent(Sender: TObject;
  const IntFieldIndex: Integer; var ColorBackground: TColor; FontCell: TxlFont);
begin
  if QuerySortShipVia.FieldByName('OrderNo').Value < 1010 then
    ColorBackground := clYellow;

  if IntFieldIndex = 4 then
  begin
    if QuerySortShipVia.FieldByName('CustNo').Value > 2000 then
    begin
      FontCell.Color := clRed;
      FontCell.Size  := 14;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFormExcelExport.TableAnimalsCalcFields(DataSet: TDataSet);
var
  StrSize : String;
  IntLength : Integer;
begin
  Dataset.FieldByName('WeightSize').AsFloat :=
    Dataset.FieldByName('Weight').AsFloat * Dataset.FieldByName('Size').AsFloat;
  Dataset.FieldByName('AreaText').AsString :=
    LowerCase(Dataset.FieldByName('Area').AsString)+
    '-'+
    LowerCase(Dataset.FieldByName('Area').AsString);

  Dataset.FieldByName('Weight2').AsFloat := Dataset.FieldByName('Weight').AsFloat * 12.34;
  Dataset.FieldByName('Weight3').AsFloat := Dataset.FieldByName('Weight').AsFloat * 12.34;
  Dataset.FieldByName('Weight4').AsFloat := Dataset.FieldByName('Weight').AsFloat * 12.34;
  Dataset.FieldByName('Weight5').AsCurrency := Dataset.FieldByName('Weight').AsFloat * 12.34;
  Dataset.FieldByName('Weight6').AsCurrency := Dataset.FieldByName('Weight').AsFloat * 12.34;
  Dataset.FieldByName('Weight7').AsFloat := Dataset.FieldByName('Weight').AsFloat * 1234.56;

  {$IFDEF DELPHI5}
  Dataset.FieldByName('Time1').AsDateTime :=
    EncodeDate(2004,01,20)
    +EncodeTime(10,11,12,0)
    +Dataset.FieldByName('Weight').AsFloat*1.1;
  {$ELSE}
  Dataset.FieldByName('Time1').AsDateTime := EncodeDateTime(2004,01,20,10,11,12,0)
    +Dataset.FieldByName('Weight').AsFloat*1.1;
  {$ENDIF}

  Dataset.FieldByName('Time2').AsDateTime := Dataset.FieldByName('Time1').AsDateTime;
  Dataset.FieldByName('Time3').AsDateTime := Dataset.FieldByName('Time1').AsDateTime;
  Dataset.FieldByName('Time4').AsDateTime := Dataset.FieldByName('Time1').AsDateTime;
  Dataset.FieldByName('Time5').AsDateTime := Dataset.FieldByName('Time1').AsDateTime;
  Dataset.FieldByName('Time6').AsDateTime := Dataset.FieldByName('Time1').AsDateTime;

  StrSize := Dataset.FieldByName('Size').AsString;
  IntLength := Length(StrSize);
  StrSize := StringOfChar('0',10-IntLength)+StrSize;
  Dataset.FieldByName('SizeText').AsString := StrSize;
end;

//------------------------------------------------------------------------------
procedure TFormExcelExport.TableBiolifeNotesGetText(Sender: TField;
  var Text: String; DisplayText: Boolean);
begin
  Text := UpperCase(Copy(Sender.AsString,1,50));
end;

//------------------------------------------------------------------------------
procedure TFormExcelExport.LabelWebsiteClick(Sender: TObject);
begin
  ShellExecute(Handle,'open','http://www.scip.be',nil,nil,SW_NORMAL);
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute1Click(Sender: TObject);
var
  Duration : TDateTime;
begin
  try
    scExcelExport1.ExcelVisible:=True;
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.Dataset:=TableOrders;
    scExcelExport1.WorksheetName:='TscExcelExport DEMO 1';
    Duration := Now();
    scExcelExport1.ExportDataset;

    StatusBar.Panels[1].Text := 'Duration : '+TimeToStr(Now() - Duration);
  finally
    scExcelExport1.Disconnect;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute2Click(Sender: TObject);
var
  Duration : TDateTime;
begin
  try
    scExcelExport1.ExcelVisible:=True;
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.Dataset:=TableAnimals;
    scExcelExport1.WorksheetName:='TscExcelExport DEMO 2';
    Duration := Now();
    scExcelExport1.ExportDataset;
    StatusBar.Panels[1].Text := 'Duration : '+TimeToStr(Now() - Duration);
  finally
    scExcelExport1.Disconnect;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute3Click(Sender: TObject);
var
  Duration : TDateTime;
begin
  try
    scExcelExport1.ExcelVisible:=True;
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.Dataset:=TableBiolife;
    scExcelExport1.WorksheetName:='TscExcelExport DEMO 3';
    scExcelExport1.HeaderText.Text:= 'DEMO 3 - Animals - Memo';
    scExcelExport1.StyleColumnWidth := cwAutoFit;
    scExcelExport1.AutoFilter := True;
    Duration := Now();
    TableBiolifeNotes.OnGetText := nil;
    scExcelExport1.ExportDataset;
    StatusBar.Panels[1].Text := 'Duration : '+TimeToStr(Now() - Duration);
  finally
    scExcelExport1.Disconnect;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute4Click(Sender: TObject);
var
  Duration : TDateTime;
begin
  try
    scExcelExport1.ExcelVisible:=True;
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.Dataset:=TableBiolife;
    scExcelExport1.WorksheetName:='TscExcelExport DEMO 4';
    Duration := Now();
    TableBiolifeNotes.OnGetText := TableBiolifeNotesGetText;
    scExcelExport1.ExportDataset;
    StatusBar.Panels[1].Text := 'Duration : '+TimeToStr(Now() - Duration);

    TableOrdersShipToPhone.Index := 10;
  finally
    scExcelExport1.Disconnect;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute5Click(Sender: TObject);
var
  Duration : TDateTime;
begin
  try
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.WorksheetName := 'TscExcelExport DEMO 5';
    scExcelExport1.Dataset:=TableOrders;

    scExcelExport1.SummarySelection := ssValues;
    scExcelExport1.SummaryCalculation := scSUM;
    scExcelExport1.ExcelVisible:=True;
    Duration := Now();
    scExcelExport1.ExportDataset;
    StatusBar.Panels[1].Text := 'Duration : '+TimeToStr(Now() - Duration);
  finally
    scExcelExport1.Disconnect;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute6Click(Sender: TObject);
var
  Duration : TDateTime;
begin
  try
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.WorksheetName := 'TscExcelExport DEMO 6';
    scExcelExport1.Dataset:=TableOrders;
    scExcelExport1.StyleColumnWidth:=cwOwnerWidth;
    scExcelExport1.ColumnWidth := 20;

    scExcelExport1.HeaderText.Text:= 'Header';
    scExcelExport1.HeaderText.Add('Header - Line2');
    scExcelExport1.HeaderText.Add('Header - Line3');
    scExcelExport1.FooterText.Add('Footer - Line1');
    scExcelExport1.FooterText.Add('Footer - Line2');
    scExcelExport1.BeginRowHeader := 5;
    scExcelExport1.BorderHeader.BackColor := clGreen;
    scExcelExport1.FontHeader.Size := 14;
    scExcelExport1.FontHeader.Color := clRed;
    scExcelExport1.MergeHeaderCells := False;
    scExcelExport1.MergeFooterCells := False;

    scExcelExport1.FontData.Size := 12;

    scExcelExport1.BeginRowTitles := 5;
    scExcelExport1.FontTitles := TxlFont(PanelTitle6.Font);
    scExcelExport1.FontTitles.Orientation := 45;
    scExcelExport1.BorderTitles.BackColor := clBlue;
    scExcelExport1.BorderTitles.BorderColor := clRed;
    scExcelExport1.BorderTitles.LineStyle := blDouble;
    scExcelExport1.BorderTitles.Weight := bwThick;

    scExcelExport1.BeginRowData := 8;
    scExcelExport1.SummarySelection := ssValues;
    scExcelExport1.SummaryCalculation := scMAX;
    scExcelExport1.BorderSummary.BackColor := clYellow;
    scExcelExport1.BorderSummary.BorderColor := clRed;
    scExcelExport1.BorderSummary.LineStyle := blLine;
    scExcelExport1.BorderSummary.Weight := bwThick;

    scExcelExport1.BorderData.LineStyle := blDouble;

    scExcelExport1.ExcelVisible:=True;
    Duration := Now();
    scExcelExport1.ExportDataset;
    StatusBar.Panels[1].Text := 'Duration : '+TimeToStr(Now() - Duration);

    MessageDlg(
      'First empty row in column A : ' + IntToStr(scExcelExport1.FindFirstEmptyRow('A')) + #13+#10 +
      'First empty row after row 5 in column A : ' + IntToStr(scExcelExport1.FindFirstEmptyRow('A',5)) + #13+#10 +
      'First empty row after row 10 in column A : ' + IntToStr(scExcelExport1.FindFirstEmptyRow('A',10)) + #13+#10 +
      'First empty row after row 10 in column C : ' + IntToStr(scExcelExport1.FindFirstEmptyRow('C',10))
      , mtInformation, [mbOK], 0);
  finally
    scExcelExport1.Disconnect;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute7Click(Sender: TObject);
var
  Duration : TDateTime;
begin
  try
    scExcelExport1.ExcelVisible:=True;
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.Dataset:=TableOrders;
    scExcelExport1.WorksheetName:='TscExcelExport DEMO 7';
    scExcelExport1.FontData.Color := clBlue;
    scExcelExport1.OnGetCellStyleEvent := ChangeCellColors;
    Duration := Now();
    scExcelExport1.ExportDataset;
    StatusBar.Panels[1].Text := 'Duration : '+TimeToStr(Now() - Duration);
  finally
    scExcelExport1.Disconnect;
    scExcelExport1.OnGetCellStyleEvent := nil;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute8Click(Sender: TObject);
var
  Duration : TDateTime;
begin
  try
    scExcelExport1.ExcelVisible:=True;
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.Dataset:=TableOrders;
    scExcelExport1.WorksheetName:='TscExcelExport DEMO 8';
    scExcelExport1.FontData.Color := clBlue;
    scExcelExport1.VisibleFieldsOnly := True;
    TableOrdersShipToContact.Visible := False;
    TableOrdersShipToAddr1.Visible := False;
    TableOrdersShipToAddr2.Visible := False;
    TableOrdersShipToCity.Visible := False;
    TableOrdersShipToState.Visible := False;
    TableOrdersShipToZip.Visible := False;
    TableOrdersShipToCountry.Visible := False;
    TableOrdersShipToPhone.Visible := False;
    TableOrdersShipVIA.Visible := False;
    TableOrdersPO.Visible := False;
    TableOrdersTerms.Visible := False;
    Duration := Now();
    scExcelExport1.ExportDataset;
    StatusBar.Panels[1].Text := 'Duration : '+TimeToStr(Now() - Duration);
  finally
    TableOrdersShipToContact.Visible := True;
    TableOrdersShipToAddr1.Visible := True;
    TableOrdersShipToAddr2.Visible := True;
    TableOrdersShipToCity.Visible := True;
    TableOrdersShipToState.Visible := True;
    TableOrdersShipToZip.Visible := True;
    TableOrdersShipToCountry.Visible := True;
    TableOrdersShipToPhone.Visible := True;
    TableOrdersShipVIA.Visible := True;
    TableOrdersPO.Visible := True;
    TableOrdersTerms.Visible := True;
    scExcelExport1.Disconnect;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute9Click(Sender: TObject);
begin
  try
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.ExcelVisible:=False;
    scExcelExport1.WorksheetName := 'TscExcelExport DEMO 9';
    scExcelExport1.Dataset:=TableOrders;
    StatusBar.Panels[1].Text := '';
    scExcelExport1.ExportDataset;

    scExcelExport1.SaveAs(ExtractFilePath(Application.ExeName)+'ExcelExportDefault',ffDefault); //without file extension

    // Excel 2007 bèta Open XML format (file extension XLSX)
    if scExcelExport1.ExcelVersion = 12 then
      scExcelExport1.SaveAs(ExtractFilePath(Application.ExeName)+'ExcelExport2007.xlsx',ffXLSX);

    // Excel 2000/XP/2003 format (file extension XLS)
    scExcelExport1.SaveAs(ExtractFilePath(Application.ExeName)+'ExcelExport2003.xls',ffXLS);

    // Excel 95 and 97 compatible format
    // Does not work in Excel 2007
    if scExcelExport1.ExcelVersion <> 12 then
      scExcelExport1.SaveAs(ExtractFilePath(Application.ExeName)+'ExcelExport97.xls',ffXL97);

    scExcelExport1.SaveAs(ExtractFilePath(Application.ExeName)+'ExcelExportCSV.csv',ffCSV);

    // HTML
    // Only works with Excel 2000/XP/2003/2007
    if scExcelExport1.ExcelVersion >= 10 then
      scExcelExport1.SaveAs(ExtractFilePath(Application.ExeName)+'ExcelExportHTM.htm',ffHTM);

    // XML spreadsheet
    // Only works with Excel XP/2003/2007
    if scExcelExport1.ExcelVersion >= 11 then
      scExcelExport1.SaveAs(ExtractFilePath(Application.ExeName)+'ExcelExportXML.xml',ffXML);
  finally
    scExcelExport1.Disconnect(True);
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute10Click(Sender: TObject);
begin
  try
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.ExcelVisible:=False;
    scExcelExport1.WorksheetName := 'TscExcelExport DEMO 10';
    scExcelExport1.Dataset:=TableOrders;
    scExcelExport1.ExportDataset;
    StatusBar.Panels[1].Text := '';
    scExcelExport1.ExcelWorkSheet.PageSetup.Orientation := xlLandscape;
    scExcelExport1.PrintPreview(True);
  finally
    scExcelExport1.Disconnect(True);
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute11Click(Sender: TObject);
var
  Duration : TDateTime;
begin
  try
    // Only use ctNewWorkbook and ctNewWorksheet when performance is
    // important and when an Excel instance has been started first
    // with ctNewExcel
    scExcelExport1.ExcelVisible:=True;
    scExcelExport1.LoadDefaultProperties;

    // Start excel and create new workbook and worksheet Orders
    scExcelExport1.Dataset:=TableOrders;
    scExcelExport1.WorksheetName:='Orders';
    scExcelExport1.ConnectTo := ctNewExcel;
    scExcelExport1.ShowTitles := False;
    scExcelExport1.ExportDataset;
    scExcelExport1.Disconnect;

    // Create new workbook and new worksheet Animals in active excel
    scExcelExport1.Dataset:=TableAnimals;
    scExcelExport1.WorksheetName:='Animals';
    scExcelExport1.ConnectTo := ctNewWorkBook;
    scExcelExport1.ShowTitles := False;
    scExcelExport1.ExportDataset;
    scExcelExport1.Disconnect;

    // Create new worksheet Biolife in active workbook in active excel
    scExcelExport1.Dataset:=TableBiolife;
    scExcelExport1.WorksheetName:='Biolife';
    scExcelExport1.ConnectTo := ctNewWorksheet;
    scExcelExport1.ShowTitles := True;
    scExcelExport1.ExportDataset;
    scExcelExport1.Disconnect;

    // Add data (of Biolife) in existing worksheet Animals
    scExcelExport1.Dataset:=TableBiolife;
    scExcelExport1.WorksheetName:='Animals';
    scExcelExport1.ConnectTo := ctNewWorksheet;
    scExcelExport1.BeginRowTitles := 14;
    scExcelExport1.BeginRowData := 15;
    scExcelExport1.BeginColumnData := 2;
    scExcelExport1.ShowTitles := True;

    Duration := Now();
    scExcelExport1.ExportDataset;
    StatusBar.Panels[1].Text := 'Duration : '+TimeToStr(Now() - Duration);
  finally
    scExcelExport1.Disconnect;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute12Click(Sender: TObject);
begin
  // Create new workbook, add worksheet with Orders and save it
  scExcelExport1.LoadDefaultProperties;
  scExcelExport1.FontData.Color := clRed;
  scExcelExport1.ExcelVisible:=False;
  scExcelExport1.Dataset:=TableOrders;
  scExcelExport1.WorksheetName:='Orders1';
  scExcelExport1.HeaderText.Text := 'ORDERS';
  scExcelExport1.BeginRowHeader := 1;
  scExcelExport1.BeginRowTitles := 3;
  scExcelExport1.BeginRowData := 4;
  scExcelExport1.FontHeader.Size := 12;
  scExcelExport1.FontHeader.Style := [fsBold];
  scExcelExport1.AutoFilter := True;
  scExcelExport1.ExportDataset;

  // Do not disconnect, so set other font color and other worksheet
  // and export dataset again
  scExcelExport1.FontData.Color := clGreen;
  scExcelExport1.WorksheetName:='Orders2';
  scExcelExport1.ExportDataset;

  // Save worksheet
  scExcelExport1.SaveAs(ExtractFilePath(Application.ExeName)+'ExcelExport.xls',ffXLS);
  scExcelExport1.Disconnect;

  // Load workbook, add worksheet with Orders again to new worksheet and save it
  // Clear all properties (font, position, text, ...)
  scExcelExport1.LoadDefaultProperties;
  scExcelExport1.ExcelVisible:=False;
  scExcelExport1.Dataset:=TableOrders;
  scExcelExport1.WorksheetName:='Orders3';
  scExcelExport1.Filename:=ExtractFilePath(Application.ExeName)+'ExcelExport.xls';
  scExcelExport1.HeaderText.Text := 'ORDERS';
  scExcelExport1.ExportDataset;
  scExcelExport1.SaveAs(ExtractFilePath(Application.ExeName)+'ExcelExport.xls',ffXLS);
  scExcelExport1.Disconnect;

  // Load workbook, add worksheet with Animals and save it
  // Set font, beginrow and header
  scExcelExport1.LoadDefaultProperties;
  scExcelExport1.FontData.Name := 'Times New Roman';
  scExcelExport1.FontData.Color := clRed;
  scExcelExport1.FontHeader.Size := 16;
  scExcelExport1.BeginRowHeader := 7;
  scExcelExport1.BeginRowTitles := 9;
  scExcelExport1.BeginRowData := 10;
  scExcelExport1.HeaderText.Text := 'ANIMALS';
  scExcelExport1.StyleColumnWidth := cwAutoFit;

  scExcelExport1.ExcelVisible:=False;
  scExcelExport1.Dataset:=TableAnimals;
  scExcelExport1.WorksheetName:='Animals';
  scExcelExport1.Filename:=ExtractFilePath(Application.ExeName)+'ExcelExport.xls';
  scExcelExport1.ExportDataset;
  scExcelExport1.SaveAs(ExtractFilePath(Application.ExeName)+'ExcelExport.xls',ffXLS);
  scExcelExport1.Disconnect;

  // Load workbook, add worksheet with Biolife and save it
  // Clear only properties of position and text. Do not clear font
  scExcelExport1.LoadDefaultProperties([pgPositions,pgText]);
  scExcelExport1.ExcelVisible:=False;
  scExcelExport1.Dataset:=TableBiolife;
  scExcelExport1.WorksheetName:='Biolife';
  scExcelExport1.Filename:=ExtractFilePath(Application.ExeName)+'ExcelExport.xls';
  scExcelExport1.ExportDataset;
  scExcelExport1.SaveAs(ExtractFilePath(Application.ExeName)+'ExcelExport.xls',ffXLS);
  scExcelExport1.Disconnect;

  // Load workbook, use existing worksheet Biolife, add Animals to
  // it at row 35 and column 3. Rename worksheet after exporting
  // and show the result
  // Clear all properties
  scExcelExport1.LoadDefaultProperties;
  scExcelExport1.ExcelVisible:=True; // Last action, so show the result
  scExcelExport1.Dataset:=TableAnimals;
  scExcelExport1.WorksheetName:='Biolife'; //Existing worksheet
  scExcelExport1.Filename:=ExtractFilePath(Application.ExeName)+'ExcelExport.xls';
  scExcelExport1.BeginRowTitles := 35;
  scExcelExport1.BeginRowData := 38;
  scExcelExport1.BeginColumnHeader := 3;
  scExcelExport1.BeginColumnData := 3;
  scExcelExport1.ExportDataset;
  scExcelExport1.ExcelWorkSheet.Name := 'Biolife and Animals'; // Rename worksheet
  scExcelExport1.Disconnect;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute13Click(Sender: TObject);
var
  Duration : TDateTime;
begin
  try
    scExcelExport1.ExcelVisible:=True;
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.Dataset:=QuerySortShipVia;

    scExcelExport1.SummarySelection := ssValues;
    scExcelExport1.SummaryCalculation := scAVG;
    scExcelExport1.SummaryDisplayFormat := '###0.000';
    scExcelExport1.BorderSummary.BackColor := clGreen;
    scExcelExport1.BorderSummary.BorderColor := clRed;
    scExcelExport1.BorderSummary.LineStyle := blLine;
    scExcelExport1.BorderSummary.Weight := bwThick;

    scExcelExport1.FontGroup := TxlFont(PanelTitle13.Font);
    scExcelExport1.BorderGroup.BackColor := clGray;
    scExcelExport1.WorksheetName:='TscExcelExport DEMO 13';
    scExcelExport1.StyleColumnWidth := cwEnhAutoFit;
    scExcelExport1.GroupFields.Clear;
    scExcelExport1.GroupFields.Add('ShipVia');
    scExcelExport1.GroupFields.Add('Terms');
    //scExcelExport1.GroupFields.Add('OrderNo');

    scExcelExport1.BorderHeader.BackColor := clBlue;
    scExcelExport1.FontHeader := TxlFont(PanelTitle13.Font);
    scExcelExport1.FontHeader.Alignment := haCenter;
    scExcelExport1.FontFooter.Alignment := haCenter;
    scExcelExport1.HeaderText.Text:= 'Header';
    scExcelExport1.HeaderText.Add('Header - Line2');
    scExcelExport1.HeaderText.Add('Header - Line3');
    scExcelExport1.FooterText.Add('Footer - Line1');
    scExcelExport1.FooterText.Add('Footer - Line2');
    scExcelExport1.MergeHeaderCells := True;
    scExcelExport1.MergeFooterCells := True;
    scExcelExport1.AutoFilter := True;
    scExcelExport1.BeginRowHeader := 3;

    Duration := Now();
    scExcelExport1.ExportDataset;
    StatusBar.Panels[1].Text := 'Duration : '+TimeToStr(Now() - Duration);
  finally
    scExcelExport1.Disconnect;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute14Click(Sender: TObject);
begin
  try
    scExcelExport1.ExcelVisible:=True;
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.Dataset:=TableAnimals;
    scExcelExport1.WorksheetName:='TscExcelExport DEMO 14';
    scExcelExport1.ConnectTo := ctNewExcel;
    scExcelExport1.Connect;
    scExcelExport1.ExcelWorkSheet.Range['A2','C8'].Borders.Color := clRed;

    scExcelExport1.ExportDataset;

    scExcelExport1.ExcelWorkSheet.Range['B5','E7'].Cells.Clear;

    scExcelExport1.ExcelWorkSheet.Range[Format('A%d',[scExcelExport1.EndRowData+3]),
      Format('A%d',[scExcelExport1.EndRowData+3])].Font.Size := 16;
    scExcelExport1.ExcelWorkSheet.Range[Format('A%d',[scExcelExport1.EndRowData+3]),
      Format('A%d',[scExcelExport1.EndRowData+3])].Value2 := 'Adding extra information to Excel worksheet';

    scExcelExport1.ExcelWorkSheet.Range['M1','M1'].Value2 := 10;
    scExcelExport1.ExcelWorkSheet.Range['M2','M2'].Value2 := 5;
    scExcelExport1.ExcelWorkSheet.Range['M3','M3'].Value2 := '=M1+M2';
    scExcelExport1.ExcelWorkSheet.Range['M3','M3'].Font.Color := clRed;

    scExcelExport1.ExcelWorkSheet.Range['N1','N20'].Value2 := 'Filling extra column with autofit';
    scExcelExport1.ExcelWorkSheet.Range['N1','N20'].Font.Size := 12;
    scExcelExport1.ExcelWorkSheet.Range['N1','N20'].Font.Color := clBlue;
    scExcelExport1.ExcelWorkSheet.Range['N1','N20'].EntireColumn.Autofit;

    scExcelExport1.ExcelWorkSheet.Range['B2','B2'].AddComment('This is comment for a cell');

    StatusBar.Panels[1].Text := '';
  finally
    scExcelExport1.Disconnect;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute15Click(Sender: TObject);
begin
  try
    scExcelExport1.ExcelVisible:=True;
    scExcelExport1.LoadDefaultProperties;
    scExcelExport1.Dataset:=TableAnimals;
    scExcelExport1.WorksheetName:='TscExcelExport DEMO 15';
    scExcelExport1.ConnectTo := ctNewExcel;
    scExcelExport1.BorderTitles.BackColor := clSilver;
    scExcelExport1.AutoFilter := True;

    scExcelExport1.ExportDataset;

    scExcelExport1.ExcelWorkSheet.Range['A2','A'+IntToStr(scExcelExport1.EndRowData)].Interior.Color := clSilver;

    scExcelExport1.ExcelWorkSheet.Range['B2','B2'].Select;
    scExcelExport1.ExcelApplication.ActiveWindow.FreezePanes := True;

    {$IFDEF EXCELXP}
    scExcelExport1.ExcelWorkSheet.Range['C1','C'+IntToStr(scExcelExport1.EndRowData)].AutoFilter(3,'>=8',xlAnd,EmptyParam,True);
    {$ENDIF}

    {$IFNDEF EXCEL97}
    scExcelExport1.ExcelWorkBook.WebPagePreview;
    {$ENDIF}

    scExcelExport1.ExcelWorkBook.PivotTableWizard;
  finally
    scExcelExport1.Disconnect;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute16Click(Sender: TObject);
var
  Duration : TDateTime;
begin
  try
    TableOrders.DisableControls;
    TableOrders.First;
    scExcelExport2.ExcelVisible:=True;
    scExcelExport2.ConnectTo := ctNewExcel;
    scExcelExport2.DataPipe := dpCustom;
    scExcelExport2.GroupFields.Clear;
    scExcelExport2.GroupFields.Add('ShipVia');
    Duration := Now();
    scExcelExport2.ExportDataset;
    StatusBar.Panels[1].Text := 'Duration : '+TimeToStr(Now() - Duration);
  finally
    scExcelExport2.Disconnect;
    TableOrders.EnableControls;
  end;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute17Click(Sender: TObject);
begin
  scExcelExport1.CloseAllExcelApps;
end;

//-----------------------------------------------------------------------------
procedure TFormExcelExport.BitBtnExecute18Click(Sender: TObject);
begin
  MessageDlg('Excel version = '+IntToStr(scExcelExport1.ExcelVersion), mtInformation, [mbOK], 0);
end;

end.




