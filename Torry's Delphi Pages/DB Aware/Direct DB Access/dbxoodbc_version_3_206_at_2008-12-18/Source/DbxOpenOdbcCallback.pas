{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 3.203 2008-11-03

  Copyright (c) 2001-2009 Vadim V.Lopushansky

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
unit DbxOpenOdbcCallback;

interface

uses
  {$IF CompilerVersion < 18.50}
  DBXpress,
  {$ELSE}
  DBXCommon,
  {$IFEND}
  {$IF CompilerVersion <= 18.00}
  DB, SqlExpr,
  {$IFEND}
  SysUtils;


{$IF CompilerVersion <= 18.00}
type
  TDBXTraceInfo = pSQLTRACEDesc;
{$IFEND}

const
//
//  TXDBXTraceFlag:
//
    cTDBXTraceFlags_none        = $0000; // == trUNKNOWN
  ///<summary>prepared query statements</summary>
    cTDBXTraceFlags_Prepare     = $0001; // == trQPREPARE
  ///<summary>executed query statements</summary>
    cTDBXTraceFlags_Execute     = $0002; // == trQEXECUTE
  ///<summary>errors</summary>
    cTDBXTraceFlags_Error       = $0004; // == trERROR
  ///<summary>command related operations.</summary>
    cTDBXTraceFlags_Command     = $0008; // == trSTMT
  ///<summary>connect and disconnect</summary>
    cTDBXTraceFlags_Connect     = $0010; // == trCONNECT
  ///<summary>transaction commit, rollback</summary>
    cTDBXTraceFlags_Transact    = $0020; // == trTRANSACT
  ///<summary>blob access</summary>
    cTDBXTraceFlags_Blob        = $0040; // == trBLOB
  ///<summary>Miscellaneos</summary>
    cTDBXTraceFlags_Misc        = $0080; // == trMISC
  ///<summary>Vendor specific operations</summary>
    cTDBXTraceFlags_Vendor      = $0100; // == trVENDOR
  ///<summary>TDBXParamter access</summary>
    cTDBXTraceFlags_Parameter   = $0200; // == trDATAIN
  ///<summary>TDBXReader operations</summary>
    cTDBXTraceFlags_Reader      = $0400; // == trDATAOUT

{.$IF CompilerVersion >= 18.50} // Delphi 2007 UP

  ///<summary>Driver loading operations.</summary>
    cTDBXTraceFlags_DriverLoad = $0800;
  ///<summary>Meta data access operations.</summary>
    cTDBXTraceFlags_MetaData    = $1000;
  ///<summary>Driver operations.</summary>
    cTDBXTraceFlags_Driver      = $2000;

{.$IF CompilerVersion >= 19.00} // Delphi 2009 UP

  ///<summary>Allow the trace handler to filter the <c>TDBXTraceEvent</c>.</summary>
    cTDBXTraceFlags_Custom      = $4000; // Delphi 2009 UP

{.$IFEND}
{.$IFEND}

procedure DecodeTraceInfo(const TraceInfo: TDBXTraceInfo; var TraceFlag: Integer; var TraceFlagName, TraceMessage: string);

function GetTraceFlag(const TraceInfo: TDBXTraceInfo): Integer;
function GetTraceFlagName(TraceFlag: Word): string; overload;
function GetTraceFlagName(const TraceInfo: TDBXTraceInfo): string; overload;
function GetTraceMessage(const TraceInfo: TDBXTraceInfo): string;

implementation

function GetTraceFlag;//(const TraceInfo: TDBXTraceInfo): Integer;
begin
  Result := TraceInfo.{$IF CompilerVersion >= 18.50}TraceFlag{$ELSE}eTraceCat{$IFEND};
end;

function GetTraceFlagName(TraceFlag: Word): string;
begin
  case TraceFlag of
    cTDBXTraceFlags_none: Result := 'none';
    cTDBXTraceFlags_Prepare: Result := 'Prepare';
    cTDBXTraceFlags_Execute: Result := 'Execute';
    cTDBXTraceFlags_Error: Result := 'Error';
    cTDBXTraceFlags_Command: Result := 'Command';
    cTDBXTraceFlags_Connect: Result := 'Connect';
    cTDBXTraceFlags_Transact: Result := 'Transact';
    cTDBXTraceFlags_Blob: Result := 'Blob';
    cTDBXTraceFlags_Misc: Result := 'Misc';
    cTDBXTraceFlags_Vendor: Result := 'Vendor';
    cTDBXTraceFlags_Parameter: Result := 'Parameter';
    cTDBXTraceFlags_Reader: Result := 'Reader';
    {.$IF CompilerVersion >= 18.50} // Delphi 2007 UP
    cTDBXTraceFlags_DriverLoad: Result := 'DriverLoad';
    cTDBXTraceFlags_MetaData: Result := 'MetaData';
    cTDBXTraceFlags_Driver: Result := 'Driver';
    {.$IF CompilerVersion >= 19.00} // Delphi 2009 UP
    cTDBXTraceFlags_Custom: Result := 'Custom';
    else
      Result := 'unknown';
  end;
end;

function GetTraceFlagName(const TraceInfo: TDBXTraceInfo): string;
begin
  Result := GetTraceFlagName(GetTraceFlag(TraceInfo));
end;

function GetTraceMessage;//(const TraceInfo: {$IF CompilerVersion >= 18.50}TDBXTraceInfo{$ELSE}pSQLTRACEDesc{$IFEND}): string;
{$IF CompilerVersion < 18.50}
var
  ws: {$IF CompilerVersion >= 18.00}WideString{$ELSE}string{$IFEND};
{$IFEND}
begin
  {$IF CompilerVersion > 18.00}
  Result := TraceInfo.Message;
  {$ELSE IF 1=1}
  SetLength(ws, TraceInfo.uTotalMsgLen);
  Move(TraceInfo.pszTrace[0], ws[1], TraceInfo.uTotalMsgLen {$IF CompilerVersion >= 18.00}* SizeOf(WideChar){$IFEND});
  Result := TrimRight(ws);
  {$IFEND}
end;

procedure DecodeTraceInfo;//(const TraceInfo: TDBXTraceInfo; var TraceFlag: Integer; var TraceFlagName, TraceMessage: string);
begin
  TraceFlag := GetTraceFlag(TraceInfo);
  TraceFlagName := GetTraceFlagName(TraceFlag);
  TraceMessage := GetTraceMessage(TraceInfo);
end;

end.
