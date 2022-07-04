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
 
unit dMythic;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, FFDB, FFDBBase, FFLLBase, ffllcomm, fflllgcy, ffllcomp, fflleng,
  ffsrintm, ffclreng;

type
  { What orders should be made available to the GUI }
  TOrderRange = (rtAll, rtLast7, rtLast30, rt1Year);

  TdtmMythic = class(TDataModule)
    ffssMythic: TffSession;
    ffdbMythic: TffDatabase;
    tblCustomer: TffTable;
    dtsCustomer: TDataSource;
    dtsCustomerOrders: TDataSource;
    tblCustomerOrders: TffTable;
    tblCustomerOrdersCustNo: TIntegerField;
    tblCustomerOrdersSaleDate: TDateTimeField;
    tblCustomerOrdersShipDate: TDateTimeField;
    tblCustomerOrdersEmpNo: TIntegerField;
    tblCustomerOrdersShipToContact: TStringField;
    tblCustomerOrdersShipToAddr1: TStringField;
    tblCustomerOrdersShipToAddr2: TStringField;
    tblCustomerOrdersShipToCity: TStringField;
    tblCustomerOrdersShipToState: TStringField;
    tblCustomerOrdersShipToZip: TStringField;
    tblCustomerOrdersShipToCountry: TStringField;
    tblCustomerOrdersShipToPhone: TStringField;
    tblCustomerOrdersShipVIA: TStringField;
    tblCustomerOrdersPO: TStringField;
    tblCustomerOrdersTerms: TStringField;
    tblCustomerOrdersPaymentMethod: TStringField;
    tblCustomerOrdersItemsTotal: TCurrencyField;
    tblCustomerOrdersTaxRate: TFloatField;
    tblCustomerOrdersFreight: TCurrencyField;
    tblCustomerOrdersAmountPaid: TCurrencyField;
    tblCustomerOrdersTaxTotal: TCurrencyField;
    tblCustomerOrdersAmountDue: TCurrencyField;
    tblCustomerOrdersOrderNo: TAutoIncField;
    tblOrderItems: TffTable;
    dtsOrderItems: TDataSource;
    tblOrderItemsProductLookup: TffTable;
    tblOrderItemsID: TAutoIncField;
    tblOrderItemsOrderNo: TIntegerField;
    tblOrderItemsPartNo: TIntegerField;
    tblOrderItemsQty: TIntegerField;
    tblOrderItemsOEM: TStringField;
    tblOrderItemsName: TStringField;
    tblCustomerOrdersStatus: TStringField;
    tblCustomerOrdersCCNumber: TStringField;
    tblCustomerOrdersCCExpMonth: TSmallintField;
    tblCustomerOrdersCCExpYear: TSmallintField;
    tblOrderItemsSalePrice: TCurrencyField;
    tblOrderItemsProductLookupID: TAutoIncField;
    tblOrderItemsProductLookupName: TStringField;
    tblOrderItemsProductLookupUOM: TStringField;
    tblOrderItemsProductLookupOEM: TStringField;
    tblOrderItemsProductLookupSalePrice: TCurrencyField;
    ffClient: TffClient;
    FFRemoteServerEngine: TFFRemoteServerEngine;
    ffTransport: TffLegacyTransport;
    tblCustomerID: TAutoIncField;
    tblCustomerCompany: TStringField;
    tblCustomerAddress1: TStringField;
    tblCustomerAddress2: TStringField;
    tblCustomerCity: TStringField;
    tblCustomerState: TStringField;
    tblCustomerZip: TStringField;
    tblCustomerCountry: TStringField;
    tblCustomerPhone: TStringField;
    tblCustomerFAX: TStringField;
    tblCustomerTaxRate: TFloatField;
    tblCustomerContact: TStringField;
    tblCustomerLastInvoiceDate: TDateTimeField;
    tblCustomerDeliveryMethod: TStringField;
    procedure tblCustomerOrdersCalcFields(DataSet: TDataSet);
    procedure tblCustomerOrdersNewRecord(DataSet: TDataSet);
    procedure tblCustomerAfterScroll(DataSet: TDataSet);
    procedure tblOrderItemsAfterPost(DataSet: TDataSet);
  private
    FCustomerOrderRange: TOrderRange;
    procedure SetCustomerOrderRange(const Value: TOrderRange);
  public
    procedure Connect;
    procedure Disconnect;
    property CustomerOrderRange : TOrderRange
      read FCustomerOrderRange
      write SetCustomerOrderRange;
  end;

var
  dtmMythic: TdtmMythic;

implementation

{$R *.DFM}

procedure TdtmMythic.tblCustomerOrdersCalcFields(DataSet: TDataSet);
begin
  { These calculated fields are used to generate information that is
    displayed on the OrderEntry dialog. }

  tblCustomerOrdersTaxTotal.Value := (tblCustomerOrdersTaxRate.Value / 100) *
    tblCustomerOrdersItemsTotal.Value;

  tblCustomerOrdersAmountDue.Value :=
    tblCustomerOrdersItemsTotal.Value +
    tblCustomerOrdersFreight.Value -
    tblCustomerOrdersAmountPaid.Value +
    tblCustomerOrdersTaxTotal.Value;
end;

procedure TdtmMythic.tblCustomerOrdersNewRecord(DataSet: TDataSet);
begin
  { Set some default values for the fields in the new record }

  { Since we are emulating a master-detail relationship between customer and
    orders, we must manually set the CustNo value for this record }
  tblCustomerOrdersCustNo.Value := tblCustomerID.Value;

  tblCustomerOrdersSaleDate.Value := Date;
  tblCustomerOrdersTerms.Value := 'net 30';
  tblCustomerOrdersPaymentMethod.Value := 'Check';
  tblCustomerOrdersItemsTotal.Value := 0;
  tblCustomerOrdersTaxRate.Value := 0;
  tblCustomerOrdersFreight.Value := 0;
  tblCustomerOrdersAmountPaid.Value := 0;

  { Mark the record with a status of 'C' (for creating). This will keep the
    back end from processing the order. Once the order is complete, this status
    field will be set to 'N' for new so the backend will know to process it.}
  tblCustomerOrdersStatus.Value := 'C';

  { Now we pull in some information from the customer table to act as
    defaults for the shipping information. }
  tblCustomerOrdersShipToContact.Value := tblCustomerContact.Value;
  tblCustomerOrdersShipToAddr1.Value := tblCustomerAddress1.Value;
  tblCustomerOrdersShipToAddr2.Value := tblCustomerAddress2.Value;
  tblCustomerOrdersShipToCity.Value := tblCustomerCity.Value;
  tblCustomerOrdersShipToState.Value := tblCustomerState.Value;
  tblCustomerOrdersShipToZip.Value := tblCustomerZip.Value;
  tblCustomerOrdersShipToCountry.Value := tblCustomerCountry.Value;
  tblCustomerOrdersShipToPhone.Value := tblCustomerPhone.Value;
  tblCustomerOrdersShipVia.Value := tblCustomerDeliveryMethod.Value;
end;

procedure TdtmMythic.tblCustomerAfterScroll(DataSet: TDataSet);
var
  { variable to store customer ID for easy access }
  ID : Longint;
begin
  { The customer table has completed a scroll operation, and we must set a
    range on the order table to emulate a mater-detail relationship. The
    range is based on the ID of the customer, and the date-range of orders
    to display based on the internal CustomerOrderRange property. The property
    is set by the GUI's main form whenever the show combobox is changed. }

  ID := tblCustomerID.Value;

  { Set range on Orders table }
  with tblCustomerOrders do
    case CustomerOrderRange of
      rtLast7 : SetRange([ID, Date - 7], [ID, Date]);
        { include orders from the last 7 days that match ID }
      rtLast30 : SetRange([ID, Date - 30], [ID, Date]);
        { include orders from the last 30 days that match ID }
      rt1Year : SetRange([ID, Date - 365], [ID, Date]);
        {include orders for 1 year }
    else
      SetRange([ID], [ID]);
        {Include all orders }
    end;
end;

procedure TdtmMythic.SetCustomerOrderRange(const Value: TOrderRange);
begin
  { This method is called when the TfrmMain.cboOrderRangeChange event is
    triggered. Since we need to update the range we call the
    data module's tblCustomerAfterScroll method. Before we do this, though
    we check to make sure that a different range option was passed in via
    the Value property. }
  if FCustomerOrderRange <> Value then begin
    FCustomerOrderRange := Value;
    tblCustomerAfterScroll(tblCustomer);
  end;
end;

procedure TdtmMythic.tblOrderItemsAfterPost(DataSet: TDataSet);
var
  BM : TBookmark;
  Total : Currency;
begin
  { Update order itemstotal }
  Dataset.DisableControls;
  try
    BM := Dataset.GetBookmark;
    try
      Dataset.First;
      Total := 0;
      while not Dataset.EOF do begin
        Total := Total + (Dataset.FieldByName('SalePrice').AsCurrency *
                          Dataset.FieldByName('Qty').AsInteger);
        Dataset.Next;
      end;
    finally
      Dataset.GotoBookmark(BM);
      Dataset.FreeBookmark(BM);
    end;
  finally
    Dataset.EnableControls;
  end;
  if not (tblCustomerOrders.State in [dsEdit]) then
    tblCustomerOrders.Edit;
  tblCustomerOrdersItemsTotal.Value := Total;
end;

procedure TdtmMythic.Connect;
{Begin !!.02}
var
  aInx : integer;
  anAlias, aPath : string;
  aSession : TffSession;
begin
  aSession := nil;
  anAlias := '';

  { Scan for session and database components. We need to activate the
    session in order to test for an alias. We need to obtain the required
    alias from the database component. }
  for aInx := 0 to pred(ComponentCount) do
    if Components[aInx] is TffSession then begin
      aSession := TffSession(Components[aInx]);
      aSession.Open;
    end
    else if Components[aInx] is TffDatabase then
      anAlias := TffDatabase(Components[aInx]).AliasName;

  { Did we find an alias? Do we have a session? }
  if (anAlias <> '') and assigned(aSession) then
    { Make sure the alias exists on the server. }
    if not aSession.IsAlias(anAlias) then begin
      aPath := ExtractFilePath(Application.ExeName);
      if aPath[Length(aPath)] <> '\' then
        aPath := aPath + '\';
      { Path should point to the folder containing the Mythic tables. }
      aSession.AddAlias(anAlias, aPath + '..\..\..\MythicDB', False);
    end;

  { Open the tables. }
  for aInx := 0 to pred(ComponentCount) do
    if Components[aInx] is TffTable then
      TffTable(Components[aInx]).Open;
{End !!.02}
end;

procedure TdtmMythic.Disconnect;
begin
  ffssMythic.Close;
end;

end.
