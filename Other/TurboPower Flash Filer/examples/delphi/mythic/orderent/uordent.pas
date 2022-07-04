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
unit uOrdEnt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, DBCtrls, Grids, DBGrids, Mask, Db, FFDB;

type
  TfrmOrderEntry = class(TForm)
    pnlTopLeft: TPanel;
    grpBillTo: TGroupBox;
    dbeBillToName: TDBEdit;
    dbeBillToID: TDBEdit;
    dbeBillToAddress1: TDBEdit;
    dbeBillToAddress2: TDBEdit;
    dbeBillToCity: TDBEdit;
    dbeBillToState: TDBEdit;
    dbeBillToZip: TDBEdit;
    grpShipTo: TGroupBox;
    dbeShipToContact: TDBEdit;
    dbeShipToAddress1: TDBEdit;
    dbeShipToAddress2: TDBEdit;
    dbeShipToCity: TDBEdit;
    dbeShipToState: TDBEdit;
    dbeShiptoZip: TDBEdit;
    pnlTopRight: TPanel;
    pnlTopRight_Bottom: TPanel;
    btnRemoveItem: TButton;
    grdOrderItems: TDBGrid;
    grpPaymentInformation: TGroupBox;
    lblSoldBy: TLabel;
    dbeSoldBy: TDBLookupComboBox;
    lblTerms: TLabel;
    dbeTerms: TDBComboBox;
    lblPaymentMethod: TLabel;
    dbePaymentMethod: TDBComboBox;
    lblShipvia: TLabel;
    dbeShipVIA: TDBComboBox;
    lblPO: TLabel;
    dbePO: TDBEdit;
    lblSubtotal: TLabel;
    lblDue: TLabel;
    lblPaid: TLabel;
    lblFreight: TLabel;
    lblTax: TLabel;
    dbePaid: TDBEdit;
    dbeSubtotal: TDBEdit;
    dbeTax: TDBEdit;
    dbeFreight: TDBEdit;
    dbeDue: TDBEdit;
    dbeTaxRate: TDBEdit;
    btnLogOrder: TButton;
    btnCancelOrder: TButton;
    bvlPayment: TBevel;
    tblEmployees: TffTable;
    dtsemployees: TDataSource;
    pnlItemSearch: TPanel;
    edtOEM: TEdit;
    lblOEMNumber: TLabel;
    btnAddItem: TButton;
    tblProducts: TffTable;
    tblProductsID: TAutoIncField;
    tblProductsName: TStringField;
    tblProductsUOM: TStringField;
    tblProductsOEM: TStringField;
    tblProductsSalePrice: TCurrencyField;
    lblCCNumber: TLabel;
    dbeCCNumber: TDBEdit;
    lblCCExpMonth: TLabel;
    dbeCCExpMonth: TDBComboBox;
    lblCCExpYear: TLabel;
    dbeCCExpYear: TDBComboBox;
    procedure btnLogOrderClick(Sender: TObject);
    procedure btnCancelOrderClick(Sender: TObject);
    procedure btnAddItemClick(Sender: TObject);
    procedure btnRemoveItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetOrderDataset: TDataset;
    function GetOrderItemsDataset: TDataset;
  public
    property OrderDataset : TDataset
       read GetOrderDataset;
    property OrderItemsDataset : TDataset
       read GetOrderItemsDataset;
  end;

var
  frmOrderEntry: TfrmOrderEntry;

implementation

uses
  dMythic,
  uItemSel
  {$IFDEF DCC6OrLater}                                                          {!!.05}
  ,Variants                                                                     {!!.05}
  {$ENDIF}                                                                      {!!.05}
  ;

{$R *.DFM}

procedure TfrmOrderEntry.btnLogOrderClick(Sender: TObject);
begin
  dtmMythic.ffdbMythic.StartTransaction;
  try
    { Mark order as New, save changes and close the dialog }
    with OrderDataset do begin
      if not (State in [dsEdit]) then
        Edit;
      FieldByName('Status').Value := 'N';
      OrderDataset.Post;
    end;
    dtmMythic.ffdbMythic.Commit;
  except
    dtmMythic.ffdbMythic.Rollback;
    Raise;
  end;
  Close;
end;

procedure TfrmOrderEntry.btnCancelOrderClick(Sender: TObject);
begin
  dtmMythic.ffdbMythic.StartTransaction;

  { Remove detail records }
  OrderItemsDataset.First;
  while not OrderItemsDataset.Eof do
    OrderItemsDataset.Delete;

  { Remove master record }
  OrderDataset.Delete;

  dtmMythic.ffdbMythic.Commit;
  
  Close;
end;

function TfrmOrderEntry.GetOrderDataset: TDataset;
begin
  Result := dbeShipToContact.Datasource.Dataset;
end;

procedure TfrmOrderEntry.btnAddItemClick(Sender: TObject);
var
  OEM : string;
  CancelItem : Boolean;
  RecCount : Integer;
begin
  { Make sure the Order items dataset is in browse mode }
  OrderItemsDataset.CheckBrowseMode;

  { Grab the OEM from the edit box for easy access }
  OEM := edtOEM.Text;

  { perform a filter against the product table based on item id.
    Filters are in the following format
        FieldName = Value
    FieldNames that contain spaces must be surrounded by brackets. Brackets
    are optional for field names without spaces. When a field is a string
    field, the value must be surrounded by quotes. The QuotedStr call from
    SysUtils can be used to do this. We add the '*' to the end of the item
    ID to retrieve partial matches of OEM numbers}

  tblProducts.Filter := '[OEM] = ' + QuotedStr(OEM + '*');
  tblProducts.Filtered := True;

  CancelItem := False;
  { We only want to retrieve record count once. When filters are active, record
    count can be a slow operation }
  RecCount := tblProducts.RecordCount;
  if RecCount > 1 then begin
    { If more then 1 records are found, display selection dialog with the result
      set. This will place the cursor on the correct item so we can add it to
      the order items table. }
    with TfrmItemSelection.Create(nil) do
      try
        ProductsDataset := tblProducts;
        CancelItem := ShowModal <> mrOK;
      finally
        Free;
      end;
  end else if (RecCount = 0) then begin
    MessageDlg('No products found!', mtError, [mbOK], 0);
    CancelItem := True;
  end;

  if not CancelItem then begin
    { Add the item to thee orditems table and change focus to the the grid's
      quantity option. }
    with OrderItemsDataset do begin
      InsertRecord([null, OrderDataset.FieldByName('OrderNo').Value,
                    tblProductsID.Value, 1, tblProductsSalePrice.Value,1]);
    end;
    grdOrderItems.SetFocus;
  end;
end;

function TfrmOrderEntry.GetOrderItemsDataset: TDataset;
begin
  Result := dtmMythic.tblOrderItems;
end;

procedure TfrmOrderEntry.btnRemoveItemClick(Sender: TObject);
begin
  { Make sure we have a record to delete }
  if OrderItemsDataset.RecordCount > 0 then
    OrderItemsDataset.Delete;
end;

procedure TfrmOrderEntry.FormShow(Sender: TObject);
begin
  tblEmployees.Open;
  tblProducts.Open;
end;

end.

