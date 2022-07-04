{*********************************************************}
{* FlashFiler: Main form for Client Extender example     *}
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

unit ExClMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ffllEng, ffllbase, ffsreng;

type
  TffClientMonitor = class;  { forward declaration }

  TffClientExtender = class(TffBaseEngineExtender)
  public

    function notify(serverObject : TffObject;
                    action : TffEngineAction) : TffResult; override;
  end;

  TffClientMonitor = class(TffBaseEngineMonitor)
  private
    FClients : longInt;
    FMaxClients : longInt;
  protected
    procedure bemSetServerEngine(anEngine : TffBaseServerEngine); override;
    function cmAddClient : TffResult;
    procedure cmRemoveClient;
  public
    constructor Create(aOwner : TComponent); override;
    function Interested(serverObject : TffObject) : TffBaseEngineExtender; override;

    property Clients : longInt read FClients;
    property MaxClients : longInt read FMaxClients write FMaxClients;
  end;

  TfrmMain = class(TForm)
    pbAddClient: TButton;
    pbRemoveClient: TButton;
    lblClients: TLabel;
    efMax: TEdit;
    lblMax: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure pbAddClientClick(Sender: TObject);
    procedure pbRemoveClientClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure efMaxChange(Sender: TObject);
  private
    { Private declarations }
    FClientMonitor : TffClientMonitor;
    FClients : TffList;
    FEngine : TffServerEngine;

    procedure UpdateClientCount;

  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

{===UI Stuff=========================================================}
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FEngine := TffServerEngine.Create(nil);
  FClientMonitor := TffClientMonitor.Create(nil);
  FClientMonitor.ServerEngine := FEngine;
end;
{--------}
procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FEngine.Free;
  FClientMonitor.Free;
  FClients.Free;
end;
{--------}
procedure TfrmMain.pbAddClientClick(Sender: TObject);
var
  aClientID : TffClientID;
  anItem : TffIntListItem;
  Error : TffResult;
  hash : TffWord32;
begin

  aClientID := 0;
  hash := 0;

  if not assigned(FClients) then
    FClients := TffList.Create;

  if assigned(FEngine) then begin
    Error := FEngine.ClientAdd(aClientID, 'test', 'test', 5000, hash);
    if Error = 0 then begin
      anItem := TffIntListItem.Create(aClientID);
      FClients.Insert(anItem);
    end else
      showMessage('Client connection rejected.');
  end;
  updateClientCount;
end;
{--------}
procedure TfrmMain.pbRemoveClientClick(Sender: TObject);
var
  anItem : TffIntListItem;
begin
  if assigned(FClients) then begin
    if assigned(FEngine) then begin
      anItem := TffIntListItem(FClients[0]);
      FEngine.ClientRemove(anItem.keyAsInt);
      FClients.DeleteAt(0);
      UpdateClientCount;
      if FClients.Count = 0 then begin
        FClients.Free;
        FClients := nil;
      end;
    end;
  end;
end;
{--------}
procedure TfrmMain.UpdateClientCount;
begin
  lblClients.Caption := format('Active connections: %d',[FClients.Count]);
end;
{====================================================================}

{===TffClientExtender================================================}
function TffClientExtender.notify(serverObject : TffObject;
                                        action : TffEngineAction) : TffResult;
begin
  Result := 0;
  if (serverObject is TffSrClient) and assigned(FParent) then
    if action = ffeaAfterCreateClient then
      Result := TffClientMonitor(FParent).cmAddClient
    else if action = ffeaBeforeRemoveClient then
      TffClientMonitor(FParent).cmRemoveClient;
end;
{====================================================================}

{===TffClientMonitor=================================================}
constructor TffClientMonitor.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FClients := 0;
  FMaxClients := 5;
end;
{--------}
function TffClientMonitor.cmAddClient : TffResult;
begin
  if FClients < FMaxClients then begin
    Result := 0;
    inc(FClients);
  end else
    Result := -1;  { or some error code for too many clients }
end;
{--------}
procedure TffClientMonitor.cmRemoveClient;
begin
  if FClients > 0 then
    dec(FClients);
end;
{--------}
function TffClientMonitor.Interested(serverObject : TffObject) : TffBaseEngineExtender;
begin
  Result := nil;
  if (serverObject is TffSrClient) then
    Result := TffClientExtender.Create(Self);
end;
{--------}
procedure TffClientMonitor.bemSetServerEngine(anEngine : TffBaseServerEngine);
begin
  inherited bemSetServerEngine(anEngine);
  AddInterest(TffSrClient);
end;
{====================================================================}

procedure TfrmMain.efMaxChange(Sender: TObject);
begin
  if assigned(FClientMonitor) then
    FClientMonitor.MaxClients := StrToInt(efMax.Text);
end;

end.
