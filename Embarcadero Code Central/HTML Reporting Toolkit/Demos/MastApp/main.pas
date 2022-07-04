unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OleCtrls, SHDocVw, ie5, ToolWin, ComCtrls, Menus, ImgList, StdCtrls,
  ExtCtrls, WebProtocol, ComObj, mastapp_TLB;

type
  TMainFrm = class(TForm)
    ImageList1: TImageList;
    StatusBar1: TStatusBar;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    FileNewOrder: TMenuItem;
    N5: TMenuItem;
    FilePrinterSetup: TMenuItem;
    N4: TMenuItem;
    FileExit: TMenuItem;
    ViewMenu: TMenuItem;
    ViewLocal: TMenuItem;
    ViewRemote: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    WebBrowserControl1: TWebBrowserControl;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    DateTimePicker2: TDateTimePicker;
    DateTimePicker1: TDateTimePicker;
    ToolButton7: TToolButton;
    ComboBox1: TComboBox;
    Print1: TMenuItem;
    Preview1: TMenuItem;
    WebProvider1: TWebProvider;
    CaptionPanel: TPanel;
    ImageList2: TImageList;
    Panel1: TPanel;
    TreeView1: TTreeView;
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure ViewMenuClick(Sender: TObject);
    procedure ViewLocalClick(Sender: TObject);
    procedure ViewRemoteClick(Sender: TObject);
    procedure WebBrowserControl1NavigateComplete2(Sender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
    procedure WebBrowserControl1CommandStateChange(Sender: TObject;
      Command: Integer; Enable: WordBool);
    procedure WebBrowserControl1DownloadBegin(Sender: TObject);
    procedure WebBrowserControl1DownloadComplete(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure WebBrowserControl1TitleChange(Sender: TObject;
      const Text: WideString);
    procedure WebBrowserControl1StatusTextChange(Sender: TObject;
      const Text: WideString);
    procedure FilePrinterSetupClick(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Preview1Click(Sender: TObject);
    procedure FileNewOrderClick(Sender: TObject);
    procedure WebProvider1Items0Action(Sender: TObject;
      Request: TRequestObject; Response: TResponseObject;
      var Handled: Boolean);
    procedure WebProvider1Items1Action(Sender: TObject;
      Request: TRequestObject; Response: TResponseObject;
      var Handled: Boolean);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure WebBrowserControl1BeforeNavigate2(Sender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    procedure WebBrowserControl1GetExternal(Sender: TObject;
      out ppDisp: IDispatch);
    procedure ToolButton4Click(Sender: TObject);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DateTimePicker1CloseUp(Sender: TObject);
    procedure WebProvider1Items2Action(Sender: TObject;
      Request: TRequestObject; Response: TResponseObject;
      var Handled: Boolean);
  private
    FDrawPanel: TPanel;
    FDateTimePickerEnabled: Boolean;
    procedure SetDateTimePickerEnabled(Value: Boolean);
    procedure SetupDrawPanel;
    procedure EnterNode(Node: TTreeNode);
  end;

var
  MainFrm: TMainFrm;

implementation

uses ComServ,
     DataMod,
     EDOrders;

{$R *.DFM}

type
  TDrawPanel = class(TPanel)
  protected
    procedure Paint; override;
  end;

procedure TDrawPanel.Paint;
var
  rc: TRect;
  ci: NONCLIENTMETRICS;
  S: String;
  ImageIndex: Integer;
begin
  if MainFrm.TreeView1.Selected <> nil then
    begin
      S := MainFrm.TreeView1.Selected.Text;
      ImageIndex := MainFrm.TreeView1.Selected.SelectedIndex;
    end
  else
    begin
      S := Application.Title;
      ImageIndex := 0;
    end;
  ci.cbSize := sizeof(ci);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, ci.cbSize, @ci, 0);
  rc := GetClientRect;
  InflateRect(rc, -2, -3);
  Canvas.Brush.Color := GetSysColor(COLOR_INACTIVECAPTION);
  Canvas.FillRect(rc);
  MainFrm.ImageList2.Draw(Canvas, rc.Left + 3, rc.Top +
    (rc.Bottom - rc.Top) div 2 - 7, ImageIndex);
  Canvas.Font.Handle := CreateFontIndirect(ci.lfCaptionFont);
  Canvas.Font.Color := clWhite;
  Canvas.Font.Size := Canvas.Font.Size + 2;
  Inc(rc.Left, 23);
  DrawText(Canvas.Handle, PChar(S), -1,
    rc, DT_VCENTER + DT_SINGLELINE);
end;

type
  TWindowExternal = class(TAutoIntfObject, IWindowExternal)
  protected
    function  Get_FromDate: TDateTime; safecall;
    procedure Set_FromDate(Value: TDateTime); safecall;
    function  Get_ToDate: TDateTime; safecall;
    procedure Set_ToDate(Value: TDateTime); safecall;
    function  Get_DateTimeTickerEnabled: WordBool; safecall;
    procedure Set_DateTimeTickerEnabled(Value: WordBool); safecall;
    function  Get_SelectedNode: Integer; safecall;
    procedure Set_SelectedNode(Value: Integer); safecall;
    procedure EditOrder(OrderNo: Double); safecall;
  end;

function  TWindowExternal.Get_FromDate: TDateTime;
begin
  Result := MainFrm.DateTimePicker1.Date;
end;

procedure TWindowExternal.Set_FromDate(Value: TDateTime);
begin
  MainFrm.DateTimePicker1.Date := Value;
end;

function  TWindowExternal.Get_ToDate: TDateTime;
begin
  Result := MainFrm.DateTimePicker2.Date;
end;

procedure TWindowExternal.Set_ToDate(Value: TDateTime);
begin
  MainFrm.DateTimePicker2.Date := Value;
end;

function  TWindowExternal.Get_DateTimeTickerEnabled: WordBool;
begin
  Result := MainFrm.FDateTimePickerEnabled;
end;

procedure TWindowExternal.Set_DateTimeTickerEnabled(Value: WordBool);
begin
  MainFrm.SetDateTimePickerEnabled(Value);
end;

function  TWindowExternal.Get_SelectedNode: Integer;
begin
  if MainFrm.TreeView1.Selected = nil then
    Result := -1 else
    Result := MainFrm.TreeView1.Selected.Index;
end;

procedure TWindowExternal.Set_SelectedNode(Value: Integer);
begin
  if Value <> -1 then
    MainFrm.TreeView1.Selected := MainFrm.TreeView1.Items[Value] else
    MainFrm.TreeView1.Selected := nil;
end;

procedure TWindowExternal.EditOrder(OrderNo: Double);
begin
  EdOrderForm.Edit(OrderNo);
  MainFrm.WebBrowserControl1.Refresh;
end;

// TMainFrm
procedure TMainFrm.FormCreate(Sender: TObject);
begin
  FDateTimePickerEnabled := True;
  DateTimePicker2.Date := Date;
  DateTimePicker1.Date := EncodeDate(1995, 01, 01);
  WebProvider1.BasePath := ExtractFilePath(ParamStr(0));
  WebProvider1.RegisterNameSpace;
  SetupDrawPanel;
  TreeView1.Items.GetFirstNode.Expand(True);
  WebBrowserControl1.Navigate(WebProvider1.BasePath + 'welcome.htm'); 
end;

procedure TMainFrm.SetupDrawPanel;
begin
  FDrawPanel := TDrawPanel.Create(self);
  FDrawPanel.Parent := CaptionPanel;
  FDrawPanel.Align := alClient;
end;

procedure TMainFrm.SetDateTimePickerEnabled(Value: Boolean);
begin
  if Value <> FDateTimePickerEnabled then
    begin
      if Value then
        begin
          DateTimePicker1.Enabled := True;
          DateTimePicker2.Enabled := True;
          DateTimePicker1.Color := clWhite;
          DateTimePicker2.Color := clWhite;
        end
      else
        begin
          DateTimePicker1.Enabled := False;
          DateTimePicker2.Enabled := False;
          DateTimePicker1.Color := clBtnFace;
          DateTimePicker2.Color := clBtnFace;
        end;
      FDateTimePickerEnabled := Value;
    end;
end;

procedure TMainFrm.ViewMenuClick(Sender: TObject);
begin
  ViewRemote.Enabled := FindWindow(NIL, 'InterBase Server') <> 0;
end;

procedure TMainFrm.ViewLocalClick(Sender: TObject);
begin
  MastData.UseLocalData;
  ViewLocal.Checked := True;
  Caption := Application.Title + ' (Paradox Data)';
end;

procedure TMainFrm.ViewRemoteClick(Sender: TObject);
begin
  MastData.UseRemoteData;
  ViewRemote.Checked := True;
  Caption := Application.Title + ' (Local Interbase)';
end;

procedure TMainFrm.EnterNode(Node: TTreeNode);
begin
  if Node.Text = 'Welcome' then
    WebBrowserControl1.Navigate(WebProvider1.BasePath + 'welcome.htm') else
  if Node.Text = 'Order History' then
    WebBrowserControl1.Navigate(Format('abc://./orders?date1=%s&date2=%s',
      [DateToStr(DateTimePicker1.Date), DateToStr(DateTimePicker2.Date)])) else
  if Node.Text = 'Customer List' then
    WebBrowserControl1.Navigate('abc://./customers') else
  if Node.Text = 'Browse Parts' then
    WebBrowserControl1.Navigate(WebProvider1.BasePath + 'parts.html');
end;

procedure TMainFrm.TreeView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  Node := TreeView1.GetNodeAt(X, Y);
  if Node <> nil then
    EnterNode(Node);
end;

procedure TMainFrm.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  if Msg.CharCode = VK_RETURN then
    begin
      if TreeView1.Selected <> nil then
        EnterNode(TreeView1.Selected) else
        WebBrowserControl1.Refresh;
      Handled := True;
    end;
end;

procedure TMainFrm.DateTimePicker1CloseUp(Sender: TObject);
begin
  EnterNode(TreeView1.Selected);
end;

procedure TMainFrm.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  FDrawPanel.Refresh;
end;

procedure TMainFrm.WebBrowserControl1BeforeNavigate2(Sender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
begin
  TreeView1.Selected := nil;
  SetDateTimePickerEnabled(False);
end;

procedure TMainFrm.WebBrowserControl1NavigateComplete2(Sender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  ComboBox1.Text := URL;
  ActiveControl := WebBrowserControl1;
end;

procedure TMainFrm.WebBrowserControl1CommandStateChange(Sender: TObject;
  Command: Integer; Enable: WordBool);
begin
  if Command = CSC_NAVIGATEFORWARD then
    ToolButton2.Enabled := Enable
  else if Command = CSC_NAVIGATEBACK then
    ToolButton1.Enabled := Enable;
end;

procedure TMainFrm.WebBrowserControl1DownloadBegin(Sender: TObject);
begin
  ToolButton3.Enabled := True;
end;

procedure TMainFrm.WebBrowserControl1DownloadComplete(Sender: TObject);
begin
  ToolButton3.Enabled := False;
end;

procedure TMainFrm.ToolButton1Click(Sender: TObject);
begin
  WebBrowserControl1.GoBack;
end;

procedure TMainFrm.ToolButton2Click(Sender: TObject);
begin
  WebBrowserControl1.GoForward;
end;

procedure TMainFrm.WebBrowserControl1TitleChange(Sender: TObject;
  const Text: WideString);
begin
  if Text <> '' then
    Caption := 'Marine Adventures Order Entry - '+ Text else
    Caption := 'Marine Adventures Order Entry';
end;

procedure TMainFrm.ToolButton4Click(Sender: TObject);
begin
  WebBrowserControl1.Refresh2;
end;

procedure TMainFrm.WebBrowserControl1StatusTextChange(Sender: TObject;
  const Text: WideString);
begin
  StatusBar1.SimpleText := Text;
end;

procedure TMainFrm.WebBrowserControl1GetExternal(Sender: TObject;
  out ppDisp: IDispatch);
begin
  ppDisp := TWindowExternal.Create(ComServer.TypeLib, IID_IWindowExternal);
end;

procedure TMainFrm.FilePrinterSetupClick(Sender: TObject);
begin
  WebBrowserControl1.ExecWB(OLECMDID_PAGESETUP, 0);
end;

procedure TMainFrm.Print1Click(Sender: TObject);
begin
  WebBrowserControl1.ExecWB(OLECMDID_PRINT, OLECMDEXECOPT_PROMPTUSER);
end;

procedure TMainFrm.Preview1Click(Sender: TObject);
begin
  WebBrowserControl1.ExecWB(OLECMDID_PRINTPREVIEW, OLECMDEXECOPT_PROMPTUSER);
end;

procedure TMainFrm.FileNewOrderClick(Sender: TObject);
begin
  EdOrderForm.Enter;
end;

procedure TMainFrm.WebProvider1Items0Action(Sender: TObject;
  Request: TRequestObject; Response: TResponseObject;
  var Handled: Boolean);
begin
  Response.head('iso-8859-1');
  Response.createElement('body', 'xmlns:dt', 'urn:schemas-microsoft-com:datatypes',
    'date1', Request.Params.Values['date1'], 'date2', Request.Params.Values['date2']);
  MastData.OrdersByDateQuery.ParamByName('FromDate').AsDateTime :=
    StrToDate(Request.Params.Values['date1']);
  MastData.OrdersByDateQuery.ParamByName('ToDate').AsDateTime :=
    StrToDate(Request.Params.Values['date2']);
  MastData.OrdersByDateQuery.Open;
  while not MastData.OrdersByDateQuery.Eof do
    with Response, MastData do
    begin
      createElement('row');
      Element('OrderNo', OrdersByDateQueryOrderNo.AsString);
      Element('SaleDate', OrdersByDateQuerySaleDate.AsString);
      Element('CustNo', OrdersByDateQueryCustNo.AsString);
      Element('Company', OrdersByDateQueryCompany.AsString);
      Element('ShipDate', OrdersByDateQueryShipDate.AsString);
      Element('ShipVIA', OrdersByDateQueryShipVIA.AsString);
      Element('Terms', OrdersByDateQueryTerms.AsString);
      Element('PaymentMethod', OrdersByDateQueryPaymentMethod.AsString);
      Element('ItemsTotal', OrdersByDateQueryItemsTotal.AsFloat, 'dt:dt', 'float');
      endElement('row');
      OrdersByDateQuery.Next;
    end;
  MastData.OrdersByDateQuery.Close;
  Response.endElement('body');
end;

procedure TMainFrm.WebProvider1Items1Action(Sender: TObject;
  Request: TRequestObject; Response: TResponseObject;
  var Handled: Boolean);
begin
  Response.head('iso-8859-1');
  Response.createElement('body', 'xmlns:dt', 'urn:schemas-microsoft-com:datatypes');
  MastData.CustByLastInvQuery.Open;
  while not MastData.CustByLastInvQuery.EOF do
    with Response, MastData do
    begin
      createElement('row');
      Element('LastInvoiceDate', DateToStr(CustByLastInvQueryLastInvoiceDate.AsDateTime));
      Element('OrderNo', OrdersByDateQueryOrderNo.AsString);
      Element('Company', CustByLastInvQueryCompany.AsString);
      Element('Address', CustByLastInvQueryAddr.AsString);
      Element('Phone', CustByLastInvQueryPhone.AsString);
      Element('Fax', CustByLastInvQueryFAX.AsString);
      Element('CustNo', CustByLastInvQueryCustNo.AsString);
      endElement('row');
      CustByLastInvQuery.Next;
    end;
  MastData.CustByLastInvQuery.Close;
  Response.endElement('body');
end;

procedure TMainFrm.WebProvider1Items2Action(Sender: TObject;
  Request: TRequestObject; Response: TResponseObject;
  var Handled: Boolean);
begin
  Response.head('iso-8859-1');
  Response.createElement('body', 'xmlns:dt', 'urn:schemas-microsoft-com:datatypes');
  MastData.Cust.EditKey;
  MastData.CustCustNo.AsString := Request.Params.Values['CustNo'];
  if MastData.Cust.GotoKey then
    with Response, MastData do
    begin
      createElement('Info');
        Element('CustNo', CustCustNo.AsString);
        Element('Company', CustCompany.AsString);
        Element('Addr', CustAddr.AsString);
        Element('Phone', CustPhone.AsString);
        Element('Fax', CustFax.AsString);
        Element('LastInvoiceDate', CustLastInvoiceDate.AsString);
        Element('TaxRate', CustTaxRate.AsString);
        Element('Contact', CustContact.AsString);
      endElement('Info');
      MastData.OrdByCust.Close;
      MastData.OrdByCust.Open;
      while not MastData.OrdByCust.EOF do
        begin
          createElement('row');
          Element('OrderNo', OrdByCustOrderNo.AsString);
          Element('CustNo', OrdByCustCustNo.AsString);
          Element('SaleDate', OrdByCustSaleDate.AsString);
          Element('ShipDate', OrdByCustShipDate.AsString);
          Element('ItemsTotal', OrdByCustItemsTotal.AsFloat, 'dt:dt', 'float');
          Element('TaxRate', OrdByCustTaxRate.AsFloat, 'dt:dt', 'float');
          Element('Freight', OrdByCustFreight.AsFloat, 'dt:dt', 'float');
          Element('AmountPaid', OrdByCustAmountPaid.AsFloat, 'dt:dt', 'float');
          Element('AmountDue', OrdByCustAmountPaid.AsFloat, 'dt:dt', 'float');
          endElement('row');
          MastData.OrdByCust.Next;
        end;
    end;
  Response.endElement('body');
end;

end.
