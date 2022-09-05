unit MAIN;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, Buttons, StdCtrls, ZReport, ZRCtrls, ExtCtrls, ZRPrntr;

type
  TfmMain = class(TForm)
    taCustomers: TTable;
    zrList: TZReport;
    btPreview: TButton;
    btQuit: TBitBtn;
    zrListDetail: TZRBand;
    zrvCustNo: TZRField;
    ZRLabel1: TZRLabel;
    ZRLabel2: TZRLabel;
    dsCustomers: TDataSource;
    taOrders: TTable;
    rgPreviewMode: TRadioGroup;
    rgPaperType: TRadioGroup;
    rgReport: TRadioGroup;
    zrListHeader: TZRBand;
    ZRLabel3: TZRLabel;
    zrListColumnHeader: TZRBand;
    zrListColumnFooter: TZRBand;
    zrListFooter: TZRBand;
    zrListPageHeader: TZRBand;
    ZRSystemLabel1: TZRSystemLabel;
    ZRLabel4: TZRLabel;
    ZRLabel5: TZRLabel;
    zrvCompany: TZRField;
    zrvTotal: TZRAggregator;
    ZRLabel6: TZRLabel;
    ZRTotalLabel1: TZRTotalLabel;
    zrGroups: TZReport;
    zrGroupsHeader: TZRBand;
    zrGroupsFooter: TZRBand;
    zrGroupsDetail: TZRBand;
    zrGroupsColumnHeader: TZRBand;
    ZRLabel7: TZRLabel;
    zrvOrderNo: TZRField;
    zrvCustNo0: TZRField;
    zrvAmountPaid: TZRField;
    zrGroupsGroup: TZRGroup;
    zrGroupsGroupFooter: TZRBand;
    zrGroupsGroupHeader: TZRBand;
    ZRLabel8: TZRLabel;
    ZRLabel9: TZRLabel;
    ZRLabel10: TZRLabel;
    ZRLabel11: TZRLabel;
    ZRLabel12: TZRLabel;
    zrvTotal0: TZRAggregator;
    ZRLabel13: TZRLabel;
    ZRLabel14: TZRLabel;
    ZRLabel15: TZRLabel;
    ZRLabel16: TZRLabel;
    ZRTotalLabel2: TZRTotalLabel;
    ZRLabel17: TZRLabel;
    ZRTotalLabel3: TZRTotalLabel;
    zrGroupsColumnFooter: TZRBand;
    ZRTotalLabel4: TZRTotalLabel;
    ZRTotalLabel5: TZRTotalLabel;
    zrMaster: TZReport;
    zrMasterHeader: TZRBand;
    zrMasterFooter: TZRBand;
    zrMasterDetail: TZRBand;
    ZRLabel18: TZRLabel;
    zrMasterColumnHeader: TZRBand;
    ZRLabel19: TZRLabel;
    ZRLabel20: TZRLabel;
    zrMasterDetailFooter: TZRBand;
    zrMasterDetailHeader: TZRBand;
    zrvCustNo1: TZRField;
    zrvCompany0: TZRField;
    zrvTotal1: TZRAggregator;
    ZRLabel21: TZRLabel;
    ZRLabel22: TZRLabel;
    ZRLabel23: TZRLabel;
    zrMasterSubDetail: TZRSubDetail;
    zrMasterSubDetailFooter: TZRBand;
    ZRLabel25: TZRLabel;
    zrvOrderNo0: TZRField;
    zrvItemsTotal: TZRField;
    zrvTotal2: TZRAggregator;
    ZRTotalLabel7: TZRTotalLabel;
    ZRLabel26: TZRLabel;
    ZRLabel27: TZRLabel;
    zrMasterSubDetailColumnHeader: TZRBand;
    ZRLabel24: TZRLabel;
    ZRLabel28: TZRLabel;
    ZRLabel29: TZRLabel;
    zrComposite: TZRComposite;
    procedure btPreviewClick(Sender: TObject);
    procedure zrvExpressionGetValue(Sender: TZRVariable;
      var Value: Variant);
    procedure zrMasterBeforePrint(Sender: TObject; var DoPrint: Boolean);
    procedure ZRLabel27AfterPrint(Sender: TObject);
    procedure zrMasterSubDetailFooterAfterPrint(Sender: TObject);
    procedure zrCompositeAddReports(Sender: TObject);
    procedure zrDataStart(Sender: TZRCustomController;
      var HasData: Boolean);
  private
    { Private declarations }
    TotalAmount: Currency;
    function GetReport: IZReport;
    function GetPaperType: TZRPaperType;
    function GetPreviewMode: TZRPreviewMode;
  public
    { Public declarations }
    property PaperType: TZRPaperType read GetPaperType;
    property PreviewMode: TZRPreviewMode read GetPreviewMode;
    property Report: IZReport read GetReport;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.DFM}

function TfmMain.GetReport: IZReport;
begin
  Result := nil;
  case rgReport.ItemIndex of
    0: Result := zrList;
    1: Result := zrGroups;
    2: Result := zrMaster;
    3: Result := zrComposite;
  end;
end;

function TfmMain.GetPaperType: TZRPaperType;
begin
  Result := TZRPaperType(rgPaperType.ItemIndex);
end;

function TfmMain.GetPreviewMode: TZRPreviewMode;
begin
  Result := TZRPreviewMode(rgPreviewMode.ItemIndex);
end;

procedure TfmMain.btPreviewClick(Sender: TObject);
begin
  taCustomers.Open;
  taOrders   .Open;
  {with taOrders do begin
    Close;
    if Report = zrMaster then begin
      MasterSource := dsCustomers;
      MasterFields := 'CustNo';
    end else begin
      MasterSource := nil;
      MasterFields := '';
    end;
    Open;
  end;}
  if Report <> nil then with Report do begin
    Options.PaperType   := PaperType;
    Options.PreviewMode := PreviewMode;
    Preview;
  end;
  taOrders   .Close;
  taCustomers.Close;
end;

procedure TfmMain.zrvExpressionGetValue(Sender: TZRVariable; var Value: Variant);
begin
  Value := 0;
end;

procedure TfmMain.zrMasterBeforePrint(Sender: TObject; var DoPrint: Boolean);
begin
  TotalAmount := 0;
end;

procedure TfmMain.ZRLabel27AfterPrint(Sender: TObject);
begin
  TotalAmount := TotalAmount + zrvItemsTotal.Value;
end;

procedure TfmMain.zrMasterSubDetailFooterAfterPrint(Sender: TObject);
begin
  ZRLabel29.Caption := FloatToStrF(TotalAmount, ffCurrency, 12, CurrencyDecimals);
end;

procedure TfmMain.zrCompositeAddReports(Sender: TObject);
begin
  with Sender as TZRComposite do begin
    Reports.Add(zrList);
    Reports.Add(zrGroups);
    Reports.Add(zrMaster);
  end;
end;

procedure TfmMain.zrDataStart(Sender: TZRCustomController; var HasData: Boolean);
begin
  with taOrders do begin
    Close;
    if Sender = zrMaster then begin
      MasterSource := dsCustomers;
      MasterFields := 'CustNo';
    end else begin
      MasterSource := nil;
      MasterFields := '';
    end;
    Open;
  end;
  HasData := True;
end;

end.

