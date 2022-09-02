(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}                                                               {!!.05}
unit uMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, ToolWin, ffdb, ExtCtrls, Grids, DBGrids,
  StdCtrls, db, dMythic, Buttons
  {$IFDEF DCC4OrLater}
  ,ImgList
  {$ENDIF}
  ;

type
  TfrmMain = class(TForm)
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuExit: TMenuItem;
    barToolbar: TToolBar;
    spdNewCustomer: TToolButton;
    mnuConfiguration: TMenuItem;
    mnuEmployees: TMenuItem;
    imgToolBar: TImageList;
    pnlBackAlClient: TPanel;
    pnlCustomerListTitle: TPanel;
    grdCustomer: TDBGrid;
    splCustomer_Recent: TSplitter;
    pnlOrderAlClient: TPanel;
    pnlRecentOrdersTitle: TPanel;
    grdOrders: TDBGrid;
    Label1: TLabel;
    cboOrderRange: TComboBox;
    mnuNewCustomer: TMenuItem;
    mnuDeleteCustomer: TMenuItem;
    N1: TMenuItem;
    Orders1: TMenuItem;
    Show1: TMenuItem;
    mnuOrderRangeAll: TMenuItem;
    mnuOrderRangeLast7: TMenuItem;
    mnuOrderRangeLast30: TMenuItem;
    mnuOrderRange1Year: TMenuItem;
    N2: TMenuItem;
    mnuCreateOrder: TMenuItem;
    spdDeleteCustomer: TToolButton;
    ToolButton2: TToolButton;
    spdNewOrder: TToolButton;
    Label2: TLabel;
    edtCompanySearch: TEdit;
    spdFindByCompany: TSpeedButton;
    procedure mnuExitClick(Sender: TObject);
    procedure mnuEmployeesClick(Sender: TObject);
    procedure btnDeleteCustomerClick(Sender: TObject);
    procedure btnNewCustClick(Sender: TObject);
    procedure btnNewOrderClick(Sender: TObject);
    procedure cboOrderRangeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuOrderRange1YearClick(Sender: TObject);
    procedure spdFindByCompanyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetCustDataset: TDataset;
    function GetOrderDataset: TDataset;
  public
    { property used to simplify access to the customer table }
    property CustDataset : TDataset
       read GetCustDataset;
    { property used to simplify access to the order table }
    property OrderDataset : TDataset
       read GetOrderDataset;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uEmpCfg,
  uCustEnt,
  uOrdEnt
  {$IFDEF DCC6OrLater}                                                          {!!.05}
  ,Variants                                                                     {!!.05}
  {$ENDIF}                                                                      {!!.05}
  ;

{$R *.DFM}

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuEmployeesClick(Sender: TObject);
begin
  { display the employee configuration dialog }
  with TfrmEmployees.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end
end;

function TfrmMain.GetCustDataset: TDataset;
begin
  Result := grdCustomer.Datasource.Dataset;
end;

procedure TfrmMain.btnDeleteCustomerClick(Sender: TObject);
begin
  { make sure the dataset is in browse mode }
  CustDataset.CheckBrowseMode;

  { confirm before deleting the record }
  if MessageDlg(Format('Delete %s?', [CustDataset.FieldByName('Company').AsString]),
                mtConfirmation, mbOKCancel, 0) = mrOK then
    CustDataset.Delete;
end;

procedure TfrmMain.btnNewCustClick(Sender: TObject);
var
  NameStr : string;
begin
  { make sure the dataset is in browse mode }
  CustDataset.CheckBrowseMode;

  { Retrieve the company name, and open the customer entry dialog }
  if InputQuery('Create new customer', 'Company Name', NameStr) then begin
    CustDataset.AppendRecord([null, NameStr]);
    with TfrmCustomerEntry.Create(nil) do
      try
        ShowModal;
      finally
        Free;
      end;
  end;
end;

procedure TfrmMain.btnNewOrderClick(Sender: TObject);
begin
  { Insert a new record, and open the order entry screen. We call InsertRecord
    because we want to insert an empty record and immediately post it. This
    is to make sure the Order record has an autoinc key assigned. We need
    a proper autoinc value before we can add order items. The order entry screen
    will take care of posting or cancelling the record changes as necessary }
  OrderDataset.InsertRecord(['']);
  with TfrmOrderEntry.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

function TfrmMain.GetOrderDataset: TDataset;
begin
  Result := grdOrders.Datasource.Dataset;
end;

procedure TfrmMain.cboOrderRangeChange(Sender: TObject);
begin
  dtmMythic.CustomerOrderRange := TOrderRange(cboOrderRange.ItemIndex);
  if (Sender is TComboBox) then
    { update the menu item to reflect the current range selection }
    case cboOrderRange.ItemIndex of
      0 : mnuOrderRangeAll.Checked := True;
      1 : mnuOrderRangeLast7.Checked := True;
      2 : mnuOrderRangeLast30.Checked := True;
      3 : mnuOrderRange1Year.Checked := True;
    end;  
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cboOrderRange.ItemIndex := 0;
end;

procedure TfrmMain.mnuOrderRange1YearClick(Sender: TObject);
var
  MI : TMenuItem;
begin
  if (Sender is TMenuItem) then begin
    MI := TMenuItem(Sender);
    MI.Checked := True;
    cboOrderRange.ItemIndex := MI.Tag;
    cboOrderRangeChange(Self);
  end;
end;

procedure TfrmMain.spdFindByCompanyClick(Sender: TObject);
begin
  TffTable(CustDataset).FindNearest([edtCompanySearch.Text]);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  dtmMythic.Connect;
end;

end.
