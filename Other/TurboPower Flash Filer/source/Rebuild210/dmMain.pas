{*********************************************************}
{* FlashFiler: Data module for FFRebuild210              *}
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

{$I ffdefine.inc}

unit dmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ffdb, ffdbbase, ffllbase, ffllcomp, fflleng, ffsrintm, ffsreng;

type
  TdmRebuild = class(TDataModule)
    ServerEngine: TffServerEngine;
    Client: TffClient;
    Session: TffSession;
    DB: TffDatabase;
  private
    { Private declarations }
    function GetActive : Boolean;
    function GetDatabase : TffDatabase;
    function GetPath : string;
    function GetServerDatabase : TffSrDatabase;

    procedure SetActive(const Value : Boolean);
    procedure SetPath(const Value : string);
  public
    { Public declarations }
    procedure GetTables(TableList : TStringList);
      { Returns a string list containing one entry per table in the
        database path. The string portion contains the name of the table.
        The object portion contains an inactive TffTable tied to the
        session, database, and table names. }

    property Active : Boolean
      read GetActive
      write SetActive;
      
    property Database : TffDatabase
      read GetDatabase;

    property Path : string
      read GetPath
      write SetPath;

    property ServerDatabase : TffSrDatabase
      read GetServerDatabase;
  end;

var
  dmRebuild: TdmRebuild;

implementation

{$R *.DFM}

{====================================================================}
function TdmRebuild.GetActive : Boolean;
begin
  Result := DB.Connected;
end;
{--------}
function TdmRebuild.GetDatabase : TffDatabase;
begin
  Result := DB;
end;
{--------}
function TdmRebuild.GetPath : string;
begin
  Result := DB.AliasName;
end;
{--------}
function TdmRebuild.GetServerDatabase : TffSrDatabase;
begin
  Result := TffSrDatabase(dmRebuild.Database.DatabaseID);
end;
{--------}
procedure TdmRebuild.GetTables(TableList : TStringList);
var
  Inx : Integer;
  Table : TffTable;
begin
  if DB.AliasName = '' then
    ShowMessage('Source directory not specified')
  else begin
    DB.Connected := True;
    TableList.Clear;
    DB.GetTableNames(TableList);
    for Inx := 0 to Pred(TableList.Count) do begin
      Table := TffTable.Create(nil);
      with Table do begin
        SessionName := Self.Session.SessionName;
        DatabaseName := DB.DatabaseName;
        TableName := TableList[Inx];
        TableList.Objects[Inx] := Table;
      end;
    end;  { for }
  end;

end;
{--------}
procedure TdmRebuild.SetActive(const Value : Boolean);
begin
  DB.Connected := Value;
end;
{--------}
procedure TdmRebuild.SetPath(const Value : string);
begin
  if Value <> DB.AliasName then begin
    DB.Connected := False;
    DB.AliasName := Value;
  end;
end;
{====================================================================}

end.
