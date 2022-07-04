{*********************************************************}
{* FlashFiler: ISAPI example web module                  *}
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

unit ffInvWM;

{ This web module can serve as a simple model for your own FF ISAPI web
  modules. }

interface

uses
  Windows, Messages, SysUtils, Classes, HTTPApp, Db, FFDB, FFDBBase,
  FFConn, ffllprot, fflleng;

type

  { The following class is used to identify what type of connection
    is to be created.  Leave as is. }
  TffBaseConnClass = class of TffBaseConn;

  TwmInv = class(TWebModule)

    procedure wmInvwaDCPostAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure wmInvwaDCGetAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleDestroy(Sender: TObject);

  protected

    function Connect(const aConnType : TffBaseConnClass) : Boolean;
      {-Obtain a connection to the database. }

    { Private declarations }

    function GetInventory(const productID : longInt) : string;
      {-Build HTML that displays the inventory for a specific distribution
        center. }

    function GetProducts(const fullPage : Boolean;
                         const defaultProductID : longInt) : string;
      {-Build an HTML string containing a form that allows user to select
        a product.  If fullPage is True then an entire HTML page is
        returned.  Otherwise, only the form-related code is returned.
        if defaultProductID > 0 then the product combobox defaults to that
        product.
      }

    function HandleGet(Request : TWebRequest) : string;
      {-Handle a get request. }

    function HandlePost(Request : TWebRequest) : string;
      {-Handle a post request. }

  public
    { Public declarations }
  end;

var
  wmInv: TwmInv;

implementation

uses ffInvCon, ffServer;

const
  csDataSep    = '@';
  csHTMLbreak  = '<br>';
  csHTMLhead   = '<html><head><title>FlashFiler 2 ISAPI Example: Details</title></head>';
  csHTMLbodyStart = '<body topmargin="10" leftmargin="10" ' +
                    'bgcolor="#FFFFFF" text="#000000" vlink="#808080" ' +
                    'alink="#FF00FF"><p align="center"><b><font size="6">' +
                    'Mythic Proportions Inc.</font></b><br><br>';
  csHTMLinvCellStart = '<td width="17%" bgcolor="#FFFFCC"><p align="center">' +
                       '<font color="" face="Arial" size="3">';
  csHTMLinvCellEnd = '</font></td>';
  csHTMLinvHeadCellStart = '<td width="17%" bgcolor="#93DCFF"><p align="center"><font size="3"><b>';
  csHTMLinvHeadCellEnd = '</b></font></td>';
  csHTMLrowStart = '<tr>';
  csHTMLrowEnd  = '</tr>';
  csHTMLtail   = '</body></html>';
  csPrimary    = 'primary';
  csProductKey = 'productID';
  csServerName = 'FFISAPI@iii.iii.iii.iii';
  csThisDLL    = '/Scripts/FFWebInv.dll';
  csUnknownDC  = '<unknown distribution center>';

{$R *.DFM}

threadvar
  fConn : TffInvConn;
    {-The isapi connection used by each thread.  We put it in a threadvar
      so that each thread will have its own connection.  Required so that
      IIS can cache web modules. }

{====================================================================}
function TwmInv.Connect(const aConnType : TffBaseConnClass) : Boolean;
begin
  try
    { If we have an existing connection but it is no longer connected
      then get rid of it. }
    if assigned(fConn) and (not fConn.isConnected) then begin
      fConn.free;
      fConn := nil;
    end;

    if not assigned(fConn) then
      try
        fConn := TffInvConn(aConnType.Create(nil));
        fConn.Connect(dmServer.ServerEngine);
      except
        if assigned(fConn) then begin
          fConn.Free;
          fConn := nil;
        end;
        raise;
      end;
    Result := True;
  except
    raise;
  end;
end;
{--------}
function TwmInv.GetInventory(const productID : longInt) : string;
begin
  Result := '';
  try
    with fConn.tblInv do begin

      IndexName := 'byProduct';
      SetRange([productID],[productID]);
      First;

      { Build the page header and 1st row of the table. }
      Result := csHTMLhead + csHTMLbodyStart;
      Result := Result + '<center><font face="Arial"><font size = "5">Inventory Detail</font></font></center>';
      Result := Result +
                format('<p align="center"><font face="Arial" size="3">OEM %s, %s</font>',
                       [fConn.tblInvProductOEM.AsString, fConn.tblInvProductName.AsString]);
      Result := Result + '<font face="Arial"><br></p><div align="Center"><Center>';
      Result := Result + '<table border="3" cellpadding ="3" cellspacing="3" width="100%" bgcolor="#FFFFFF">';
      Result := Result + csHTMLrowStart;
      Result := Result + csHTMLinvHeadCellStart + 'Distrib. Center' + csHTMLinvHeadCellEnd;
      Result := Result + csHTMLinvHeadCellStart + 'Qty On Hand' + csHTMLinvHeadCellEnd;
      Result := Result + csHTMLinvHeadCellStart + 'Qty Committed' + csHTMLinvHeadCellEnd;
      Result := Result + csHTMLinvHeadCellStart + 'Qty Available' + csHTMLinvHeadCellEnd;
      Result := Result + csHTMLrowEnd;

      { Fill in the details table. }
      while not EOF do begin
        Result := Result + csHTMLrowStart;
        Result := Result + csHTMLinvCellStart + fConn.tblInvDCName.AsString + csHTMLinvCellEnd;
        Result := Result + csHTMLinvCellStart + fConn.tblInvQtyOnHand.AsString + csHTMLinvCellEnd;
        Result := Result + csHTMLinvCellStart + fConn.tblInvQtyOnOrder.AsString + csHTMLinvCellEnd;
        Result := Result + csHTMLinvCellStart + fConn.tblInvQtyAvail.AsString + csHTMLinvCellEnd;
        Result := Result + csHTMLrowEnd;
        Next;
      end;
    end;

    Result := Result + '</table></center></div></font>';

    Result := Result + csHTMLbreak + csHTMLbreak +
              GetProducts(False, productID) + csHTMLtail;

  except
    on E:Exception do
      Result := Result + csHTMLbreak + 'GetInventory Error: ' + E.message;
  end;
end;
{--------}
function TwmInv.GetProducts(const fullPage : Boolean;
                            const defaultProductID : longInt) : string;
begin
  Result := '';
  try
    if fullPage then begin
      { Build the page header. }
      Result := csHTMLhead + csHTMLbodyStart;
      Result := Result + '<font size = "5">Inventory Search</font></p>';
    end;

    { Start the form.}
    Result := Result + '<form method="POST" action="' + csThisDLL + '">';
    Result := Result + '<p align="center">Choose a product <select size="1" name="';
    Result := Result + csProductKey + '">';

    { Add the products to the combobox. }
    with fConn.tblProduct do begin
      indexName := 'byOEM';
      first;
      while not EOF do begin
        Result := Result + '<option value="' + fConn.tblProductID.asString + '"';
        if fConn.tblProductID.asInteger = defaultProductID then
          Result := Result + ' selected';
        Result := Result + '>' + fConn.tblProductOEM.AsString + '</option>';
        next;
      end;
    end;

    { Finish off the form. }
    Result := Result + '</select><input type="submit" value="Submit" name="B1">';
    Result := Result + '<input type="reset" value="Reset" name="B2"></p></form>';
    Result := Result + '<p align="center">&nbsp;</p>';
    if fullPage then
      Result := Result + csHTMLtail;
  except
    on E:Exception do begin
      Result := E.message;
    end;
  end;
end;
{--------}
function TwmInv.HandleGet(Request : TWebRequest) : string;
begin
  Result := '';
  try
    { Initial request for distribution center inventory search. }
    Result := GetProducts(True, 0);
  except
    on E:Exception do begin
      Result := 'HandleGet Error: ' +E.message;
    end;
  end;
end;
{--------}
function TwmInv.HandlePost(Request : TWebRequest) : string;
var
  ProductID : string;
begin
  Result := '';
  try
    { Client is requesting inventory for a specific product.
      Grab the product ID. }
    ProductID := Request.ContentFields.Values[csProductKey];
    if ProductID <> '' then begin
      Result := GetInventory(strToInt(ProductID))
  end
    else
      raise Exception.Create('Invalid product ID');
  except
    on E:Exception do
      Result := Result + E.message;
  end;
end;
{--------}
procedure TwmInv.wmInvwaDCGetAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  try
    { Obtain a connection to the FlashFiler server.  If you re-use this code,
      you would change the first parameter of Connect to be your custom
      form class. }
    if Connect(TffInvConn) then
      Response.Content := HandleGet(Request)
    else
      Response.Content := csHTMLhead + 'Could not establish connection to database.' +
                          csHTMLbreak;
  except
    on E:Exception do
      Response.Content := csHTMLhead + 'Error: ' + E.message + csHTMLbreak;
  end;
end;
{--------}
procedure TwmInv.wmInvwaDCPostAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  try
    { Obtain a connection to the FlashFiler server.  If you re-use this code,
      you would change the first parameter of Connect to be your custom
      form class. }
    if Connect(TffInvConn) then
      Response.Content := HandlePost(Request)
    else
      Response.Content := csHTMLhead + 'Could not establish connection to database.' +
                          csHTMLbreak;
  except
    on E:Exception do
      Response.Content := csHTMLhead + 'Error: ' + E.message + csHTMLbreak;
  end;
end;
{====================================================================}
procedure TwmInv.WebModuleDestroy(Sender: TObject);
begin
  { Free the connection associated with the same thread. }
  fConn.free;
end;

initialization
  { Create one instance of ffServer to provide a server engine for all
    instances of this DLL. }
  dmServer := TdmServer.Create(nil);

finalization
  { Free the server data module. }
  dmServer.Free;

end.
