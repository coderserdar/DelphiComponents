{*********************************************************}
{* FlashFiler: ISAPI example base connection data module *}
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
 
unit ffConn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, FFDB, FFDBBase, FFLLBase, FFLLProt, FFLLEng;

type
  { This is the base class for our connection to FlashFiler.
    It contains a TffClient, TffSession, and TffDatabase.
    Inherit from this class to create your own custom connections. }
  TffBaseConn = class(TDataModule)
    ffSess: TffSession;
    ffDB: TffDatabase;
    ffClient: TffClient;
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  protected
    { Protected declarations }
    function GetAliasName : string; virtual;
    function GetAliasPath : string; virtual;
  public
    { Public declarations }

    procedure Connect(const anEngine : TffBaseServerEngine);
      {-Use this method to connect to an embedded or remote FlashFiler server. }

    procedure Disconnect;
      {-Use this method to disconnect from the FlashFiler server. }

    function IsConnected : boolean;
      {-Tests the connection with the server.  Returns True if the
        connection is still good otherwise returns False. }

    property Client : TffClient read ffClient;
      {-The client used by this connection. }

    property Database : TffDatabase read ffDB;
      {-The database used by this connection. }

    property Session : TffSession read ffSess;
      {-The session used by this connection. }

  end;

implementation

{$R *.DFM}


{====================================================================}
procedure TffBaseConn.Connect(const anEngine : TffBaseServerEngine);
var
  index : integer;
begin
  try
    { Because there may be multiple instances of this web module,
      the client, session, and database components must have unique
      names. }

    { Set up the client. }
    with ffClient do begin
      Active := False;
      AutoClientName := True;
      ServerEngine := anEngine;
      Active := True;
    end;

    { Set up the session. }
    with ffSess do begin
      ClientName := ffClient.ClientName;
      AutoSessionName := True;
      Active := True;
    end;

    { Has the alias been defined on the server? }
    if not ffSess.IsAlias(GetAliasName) then
      ffSess.AddAlias(GetAliasName, GetAliasPath, False);

    { Set up the database. }
    with ffDB do begin
      SessionName := ffSess.SessionName;
      AutoDatabaseName := True;
      AliasName := GetAliasName;
      Open;
    end;

    { Set sessionname and databasename on each table. }
    for index := 0 to ComponentCount - 1 do
      if Components[index] is TffDataSet then begin
        TffDataSet(Components[index]).SessionName := ffSess.SessionName;
        TffDataSet(Components[index]).DatabaseName := ffDB.DatabaseName;
//        TffDataSet(Components[index]).Open;                         {removed !!.06}
      end;

    for index := 0 to ComponentCount - 1 do                           {!!.06}
      if Components[index] is TffDataSet then                         {!!.06}
        TffDataSet(Components[index]).Open;                           {!!.06}
  except
    on E:Exception do begin
      if ffClient.Active then
        ffClient.Active := False;
      raise;
    end;
  end;

end;
{--------}
procedure TffBaseConn.Disconnect;
var
  Index : integer;
begin
  try
    if ffDB.Connected then begin
      for Index := (ComponentCount - 1) downto 0 do
        if Components[Index] is TffDataSet then
          TffDataSet(Components[Index]).Close;
      ffDB.Connected := False;
    end;
  except
  end;
  ffSess.Close;
  ffClient.Close;
end;
{--------}
procedure TffBaseConn.FormDestroy(Sender: TObject);
begin
  Disconnect;
end;
{--------}
function TffBaseConn.GetAliasName : string;
begin
  Result := '';
end;
{--------}
function TffBaseConn.GetAliasPath : string;
begin
  Result := '';
end;
{--------}
function TffBaseConn.IsConnected : boolean;
var
  aTime : TDateTime;
begin
  Result := False;
  try
    { If we can successfully grab the date and time from the server then
      we are still connected. }
    Result := (ffSess.GetServerDateTime(aTime) = 0);
  except
  end;
end;
{====================================================================}

end.
