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
 * Eivind Bakkestuen
 * Used with permission.
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit FRFFEReportEngine;

interface

uses
  ffdb,
  ffllbase,
  ffllprot,
  SysUtils;

type
  TRangeFieldValues = Array[0..Pred(ffcl_MaxIndexFlds)] of Variant;


procedure SingleTableReport(aProtocol : TffProtocolType;
                            aServerName : TffNetAddress;
                            aUserName,
                            aPassword : TffName;
                            aAliasName : PChar;
                            aTableName : TffTableName;
                            aFilter,
                            aIndexName : PChar;
                            aRangeStart,
                            aRangeEnd : TRangeFieldValues);
{ called from the table browse window (dgTable.pas) to
  view a table with the selected filter and range }


procedure SingleQueryReport(aProtocol : TffProtocolType;
                            aServerName : TffNetAddress;
                            aUserName,
                            aPassword : TffName;
                            aAliasName : PChar;
                            aSQL,
                            aFilter : PChar);
{ called from the query browse window (dgQuery.pas) to
  view a query resultset }


procedure DesignReport(aProtocol : TffProtocolType;
                       aServerName : TffNetAddress;
                       aUserName,
                       aPassword : TffName;
                       aAliasName : PChar);
{ called to open a general design view }


implementation

Uses
  classes,
  variants,
  ffclbase,
  FR_DBSet,
  fmFRFFEEngine;

{ utility functions }

procedure SetupDatabaseConnection(aProtocol : TffProtocolType;
                                  aServerName : TffNetAddress;
                                  aUserName,
                                  aPassword : TffName;
                                  aAliasName : PChar);
var
  OldPass, OldUser : string;
begin
  with dmFRFFEEngine do begin
    ffLegacyTransport.Protocol := aProtocol;
    ffLegacyTransport.ServerName := aServername;
    OldPass := ffclPassword;
    OldUser := ffclUserName;
    try
      if aPassword <> '' then begin
        ffclPassword := aPassword;
        ffclUserName := aUserName;
      end;
      ffSession.Open;
    finally
      ffclPassword := OldPass;
      ffclUserName := OldUser;
    end;
    ffDatabase.AliasName := aAliasName;
  end;
end;

procedure SingleTableReport(aProtocol : TffProtocolType;
                            aServerName : TffNetAddress;
                            aUserName,
                            aPassword : TffName;
                            aAliasName : PChar;
                            aTableName : TffTableName;
                            aFilter,
                            aIndexName : PChar;
                            aRangeStart,
                            aRangeEnd : TRangeFieldValues);
var
  ffTable : TffTable;
  i : Integer;
begin
  dmFRFFEEngine := TdmFRFFEEngine.Create(NIL);
  try
    try
      SetupDatabaseConnection(aProtocol, aServerName, aUserName, aPassword, aAliasName);
      ffTable := TffTable.Create(dmFRFFEEngine);
      with ffTable do begin
        SessionName := dmFRFFEEngine.ffSession.SessionName;
        DatabaseName := dmFRFFEEngine.ffDatabase.DatabaseName;
        TableName := aTableName;
        Filter := aFilter;
        if Filter<>'' then
          Filtered := True;
        IndexName := aIndexName;
        Open;
        if (aRangeStart[0]<>NULL) and
           (aRangeEnd[0]<>NULL) then begin
          SetRangeStart;
          for i := 0 to IndexFieldCount-1 do
            IndexFields[i].Value := aRangeStart[i];
          SetRangeEnd;
          for i := 0 to IndexFieldCount-1 do
            IndexFields[i].Value := aRangeEnd[i];
          ApplyRange;
        end;
      end;
      with dmFRFFEEngine.frPrintTable do begin
        DataSet := ffTable;
        ShowReport;
      end;

    except
      on E:Exception do
        dmFRFFEEngine.ffEventLog.WriteString(E.Message);
    end;
  finally
    dmFRFFEEngine.Free;
  end;
end;


procedure SingleQueryReport(aprotocol : TffProtocolType;
                            aServerName : TffNetAddress;
                            aUserName,
                            aPassword : TffName;
                            aAliasName : PChar;
                            aSQL,
                            aFilter : PChar);
var
  ffQuery : TffQuery;
begin
  dmFRFFEEngine := TdmFRFFEEngine.Create(NIL);
  try
    try
      SetupDatabaseConnection(aProtocol, aServerName, aUserName, aPassword, aAliasName);
      ffQuery := TffQuery.Create(dmFRFFEEngine);
      with ffQuery do begin
        SessionName := dmFRFFEEngine.ffSession.SessionName;
        DatabaseName := dmFRFFEEngine.ffDatabase.DatabaseName;
        SQL.Text := aSQL;
        Filter := aFilter;
        if Filter<>'' then
          Filtered := True;
        Open;
      end;
      with dmFRFFEEngine, frPrintTable do begin
        DataSet := ffQuery;
        ShowReport;
      end;

    except
      on E:Exception do
        dmFRFFEEngine.ffEventLog.WriteString(E.Message);
    end;
  finally
    dmFRFFEEngine.Free;
  end;
end;


procedure DesignReport(aProtocol : TffProtocolType;
                       aServerName : TffNetAddress;
                       aUserName,
                       aPassword : TffName;
                       aAliasName : PChar);
{var
  i : Integer;
  Tables : TStringList;
  ffTable : TffTable;}
begin
  dmFRFFEEngine := TdmFRFFEEngine.Create(NIL);
{  Tables := TStringList.Create;}
  try
    try
      SetupDatabaseConnection(aProtocol, aServerName, aUserName, aPassword, aAliasName);
(*    the code below is problematic since it is not possible
      to choose indexes etc for runtime-created tables.
      use dialogforms and TfrffTables/TfrffQueries inside
      the FastReport designer instead.

      dmFRFFEEngine.ffDatabase.GetTableNames(Tables);
      for i := 0 to Tables.Count-1 do begin
        ffTable := TffTable.Create(dmFRFFEEngine);
        with ffTable do begin
          try
            Name := Tables[i];
          except
            Name := Tables[i]+IntToStr(Random(1000));
          end;
          SessionName := dmFRFFEEngine.ffSession.SessionName;
          DatabaseName := dmFRFFEEngine.ffDatabase.DatabaseName;
          TableName := Tables[i];
        end;
        with TfrDBDataset.Create(dmFRFFEEngine) do begin
          try
            Name := 'frds'+Tables[i];
          except
            Name := 'frds'+Tables[i]+IntToStr(Random(1000));
          end;
          DataSet := ffTable;
        end;
      end;*)
      dmFRFFEEngine.frReport.DesignReport;

    except
      on E:Exception do
        dmFRFFEEngine.ffEventLog.WriteString(E.Message);
    end;
  finally
    dmFRFFEEngine.Free;
  end;
end;


end.
