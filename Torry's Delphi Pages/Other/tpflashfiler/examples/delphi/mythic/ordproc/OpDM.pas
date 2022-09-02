{*********************************************************}
{* FlashFiler: Order Processing example data module      *}
{*********************************************************}

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

unit OpDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ffdb, ffllbase, fflleng, ffdbbase, Db;

type
  TOnMsg = procedure(Sender : TObject; Msg : string) of object;

  TOpData = class(TDataModule)
    ffClient: TffClient;
    ffSess: TffSession;
    ffDB: TffDatabase;
    tblOrders: TffTable;
    tblOrderItems: TffTable;
    tblInventory: TffTable;
  private
    { Private declarations }
    FOnMsg : TOnMsg;

    function AllocItems(const anOrderID, aDC : Longint) : Boolean;
    function Plural(const aCount : Longint; const singular, plural : string) : string;
    procedure ProcessBackOrder;
    procedure ProcessOrder;
    procedure ShipOrder;
  public
    { Public declarations }
    procedure Connect(anEngine : TffBaseServerEngine);
    procedure Disconnect;
    procedure Msg(const aMsg : string);
    procedure ProcessOrders;

    property OnMsg : TOnMsg read FOnMsg write FOnMsg;
  end;

var
  OpData: TOpData;

implementation

{$R *.DFM}

const
  csAlias = 'Mythic';
  csDashes = '====================';
  csStatusNew = 'N';
  csStatusFailed = 'F';
  csStatusProcessed = 'P';
  csStatusShipped = 'S';
  csStatusBackOrdered = 'B';

procedure TOpData.Connect(anEngine : TffBaseServerEngine);
var
  index : Integer;
begin
  ffClient.ServerEngine := anEngine;
  ffSess.Open;
  if not ffSess.IsAlias(csAlias) then
    ffSess.AddAlias(csAlias, '..\..\..\MythicDB', False);
  ffDB.AliasName := csAlias;

  { Set sessionname and databasename on each table. }
  for index := 0 to ComponentCount - 1 do
    if Components[index] is TffDataSet then begin
      TffDataSet(Components[index]).SessionName := ffSess.SessionName;
      TffDataSet(Components[index]).DatabaseName := ffDB.DatabaseName;
      TffDataSet(Components[index]).Open;
    end;

end;
{--------}
procedure TOpData.Disconnect;
begin
  ffClient.Close;
end;
{--------}
function TOpData.AllocItems(const anOrderID, aDC : Longint) : Boolean;
var
  aInvQtyOnHandField, aInvQtyOnOrderField, aProductIDField, aQtyField : TField;
  aProductID, aQty : Longint;
begin
  Result := False;

  { Find the line items associated with the order & obtain a handle on
    frequently-used fields. }
  with tblOrderItems do begin
    IndexName := 'ByOrder';
    SetRange([anOrderID],[anOrderID]);
    aProductIDField := FieldByName('PartNo');
    aQtyField := FieldByName('Qty');
  end;

  { Orders without any line items are automatically considered as shipped.
    In real life a person would have to review the order. }
  if tblOrderItems.RecordCount = 0 then begin
    Result := True;
    Exit;
  end;

  { Loop through the line items. If the distrib center does not have
    inventory available for any one item then the order must be placed on
    backorder. }
  with tblInventory do begin
    IndexName := 'primary';
    aInvQtyOnHandField := FieldByName('QtyOnHand');
    aInvQtyOnOrderField := FieldByName('QtyOnOrder');
  end;

  while not tblOrderItems.EOF do begin
    aProductID := aProductIDField.asInteger;
    aQty := aQtyField.asInteger;
    tblInventory.SetKey;
    tblInventory.FieldByName('DistribCenterID').asInteger := aDC;
    tblInventory.FieldByName('ProductID').asInteger := aProductID;
    if tblInventory.Gotokey then begin
      { Enough inventory to fill the order? }
      if (aInvQtyOnHandField.AsInteger - aInvQtyOnOrderField.AsInteger) > aQty then begin
        { Allocate the product to the order. }
        tblInventory.Edit;
        aInvQtyOnOrderField.AsInteger := aInvQtyOnOrderField.AsInteger + aQty;
        tblInventory.Post;
        Result := True;
      end
      else begin
        { Not enough quantity available. Place on backorder. }
        Result := False;
        break;
      end;
    end
    else begin
      { Could not find the product at the distrib center. Place on backorder. }
      Result := False;
      break;
    end;
    tblOrderItems.Next;
  end;
end;
{--------}
procedure TOpData.ProcessBackOrder;
var
  aDC, anOrderID : Longint;
begin
  { Determine which distribution center will fill the order. This is entirely
    random but in a real world application, it would might be based upon
    location of customer and/or which warehouse has available inventory. }
  with tblOrders do begin
    anOrderID := FieldByName('OrderNo').asInteger;
    aDC := FieldByName('DistribCenterID').asInteger;
  end;

  { Allocate quantities in the warehouse. }
  ffDB.StartTransaction;
  try
    if AllocItems(anOrderID, aDC) then begin
      tblOrders.Edit;
      tblOrders.FieldByName('Status').asString := csStatusProcessed;
      tblOrders.Post;
      ffDB.Commit;
      Msg(format('Order %d allocated', [anOrderID]));
    end
    else
      { If couldn't allocate items then leave status as backordered.
        Make sure that any previous allocated items are not allocated. }
      ffDB.Rollback;
  except
    on E:Exception do begin
      ffDB.Rollback;
      Msg(format('Exception while processing order %d: %s',
                 [anOrderID, E.Message]));
    end;
  end;
end;
{--------}
procedure TOpData.ProcessOrder;
var
  aDC, anOrderID : Longint;
begin
  { Determine which distribution center will fill the order. This is entirely
    random but in a real world application, it would might be based upon
    location of customer and/or which warehouse has available inventory. }
  with tblOrders do begin
    anOrderID := FieldByName('OrderNo').asInteger;
    Edit;
    aDC := random(3) + 1;
    FieldByName('DistribCenterID').asInteger := aDC;
    Post;
  end;

  { Allocate quantities in the warehouse. }
  ffDB.StartTransaction;
  try
    if AllocItems(anOrderID, aDC) then begin
      tblOrders.Edit;
      tblOrders.FieldByName('Status').asString := csStatusProcessed;
      tblOrders.Post;
      ffDB.Commit;
      Msg(format('Order %d allocated', [anOrderID]));
    end
    else begin
      { If couldn't allocate items then mark the order as backordered.
        Make sure that any previous allocated items are not allocated. }
      ffDB.Rollback;
      ffDB.StartTransaction;
      tblOrders.Edit;
      tblOrders.FieldByName('Status').asString := csStatusBackordered;
      tblOrders.Post;
      ffDB.Commit;
      Msg(format('Order %d backordered', [anOrderID]));
    end;
  except
    on E:Exception do begin
      ffDB.Rollback;
      Msg(format('Exception while processing order %d: %s',
                 [anOrderID, E.Message]));
    end;
  end;
end;
{--------}
procedure TOpData.ShipOrder;
begin
  { Mark the order as shipped. }
  with tblOrders do begin
    Edit;
    FieldByName('Status').asString := csStatusShipped;
    FieldByName('ShipDate').asDateTime := Now;
    Post;
    Msg(format('Order %d shipped', [FieldByName('OrderNo').asInteger]));
  end;
end;
{--------}
procedure TOpData.ProcessOrders;
var
  aCount : Longint;
begin

  Msg(csDashes);

  { Ship processed orders. We do it at this point to simulate a lag between
    allocating inventory and actually shipping. }
  with tblOrders do begin
    IndexName := 'Status';
    SetRange([csStatusProcessed], [csStatusProcessed]);
    aCount := RecordCount;
    if aCount = 0 then
      Msg('No processed orders found')
    else
      Msg(format('Found %d %s ready to ship',
                 [aCount, Plural(aCount, 'order', 'orders')]));
    while not Eof do begin
      ShipOrder;
      Next;
    end;
  end;

  { Process back-ordered orders first.
    In real life, we would want to process them when inventory becomes
    available. But that can be too complicated for this example. }
  with tblOrders do begin
    IndexName := 'Status';
    SetRange([csStatusBackOrdered], [csStatusBackOrdered]);
    aCount := RecordCount;
    if aCount = 0 then
      Msg('No backorders found')
    else
      Msg(format('Found %d %s',
                 [aCount, Plural(aCount, 'backorder', 'backorders')]));
    while not Eof do begin
      ProcessBackOrder;
      Next;
    end;
  end;

  { Allocate inventory to new orders. }
  with tblOrders do begin
    IndexName := 'Status';
    SetRange([csStatusNew], [csStatusNew]);
    aCount := RecordCount;
    if aCount = 0 then
      Msg('No new orders found')
    else
      Msg(format('Found %d new %s',
                 [aCount, Plural(aCount, 'order', 'orders')]));
    while not Eof do begin
      ProcessOrder;
      Next;
    end;
  end;

end;
{--------}
procedure TOpData.Msg(const aMsg : string);
begin
  if Assigned(FOnMsg) then
    FOnMsg(Self, format('%s: %s', [DateTimeToStr(Now), aMsg]));
end;
{--------}
function TOpData.Plural(const aCount : Longint; const singular, plural : string) : string;
begin
  if aCount = 1 then
    Result := singular
  else
    Result := plural;
end;

end.
