{*********************************************************}
{* FlashFiler: ISAPI example inventory data module       *}
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

unit ffInvCon;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FFDB, FFDBBase, Db, FFLLBase, ffConn;

type
  {This class inherits from TffISAPIConnection.  The only difference is
   that it contains the TffTable components specific to the application. }
  TffInvConn = class(TffBaseConn)
    tblDC: TffTable;
    tblProduct: TffTable;
    tblInv: TffTable;
    tblDCID: TAutoIncField;
    tblDCName: TStringField;
    tblProductID: TAutoIncField;
    tblProductName: TStringField;
    tblProductUOM: TStringField;
    tblProductOEM: TStringField;
    tblInvDistribCenterID: TIntegerField;
    tblInvProductID: TIntegerField;
    tblInvQtyOnHand: TIntegerField;
    tblInvQtyOnOrder: TIntegerField;
    tblInvProductName: TStringField;
    tblInvProductOEM: TStringField;
    tblInvQtyAvail: TIntegerField;
    tblInvDCName: TStringField;
    procedure tblInvCalcFields(DataSet: TDataSet);
  private
    { Private declarations }
  protected
    { Protected declarations }
    function GetAliasName : string; override;
    function GetAliasPath : string; override;
  public
    { Public declarations }
  end;

implementation

uses ffServer, ffclreng, fflleng;

{$R *.DFM}

{--------}
function TffInvConn.GetAliasName : string;
begin
  Result := 'Mythic';
end;
{--------}
function TffInvConn.GetAliasPath : string;
begin
  if TffBaseServerEngine(dmServer.ServerEngine) is TffRemoteServerEngine then
    Result := '..\..\..\MythicDB'
  else
    Result := 'd:\dev\tp\ff2\examples\MythicDB';
end;
{--------}
procedure TffInvConn.tblInvCalcFields(DataSet: TDataSet);
begin
  inherited;
  { The quantity available is the quantity on hand minus what has been
    committed to unfulfilled sales orders. }
  tblInvQtyAvail.AsInteger := tblInvQtyOnHand.AsInteger - tblInvQtyOnOrder.AsInteger;
end;

end.
