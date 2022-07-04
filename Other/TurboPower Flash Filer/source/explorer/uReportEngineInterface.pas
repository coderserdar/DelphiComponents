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
 * Portions created by the Initial Developer are Copyright (C) 2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit uReportEngineInterface;

interface

{$I FFDEFINE.INC}

uses
  ffllbase,
  ffllprot,
  ffdb;

type
  TRangeFieldValues = Array[0..Pred(ffcl_MaxIndexFlds)] of Variant;

var
  ReportEngineDLLLoaded : Boolean;

{ take care to ensure that the method declarations here and
  in the reportengine DLL match! }

  SingleTableReport : procedure(aProtocol   : TffProtocolType;
                                aServerName : TffNetAddress;
                                aUserName,
                                aPassword   : TffName;
                                aAliasName  : PChar;
                                aTableName  : TffTableName;
                                aFilter,
                                aIndexName  : PChar;
                                aRangeStart,
                                aRangeEnd   : TRangeFieldValues);

  SingleQueryReport : procedure(aProtocol : TffProtocolType;
                                aServerName : TffNetAddress;
                                aUserName,
                                aPassword : TffName;
                                aAliasName : PChar;
                                aSQL,
                                aFilter : PChar);


  DesignReport : procedure(aProtocol : TffProtocolType;
                           aServerName : TffNetAddress;
                           aUserName,
                           aPassword : TffName;
                           aAliasName : PChar);

implementation

uses
  Windows,
  SysUtils,
  Forms;

var
  hDLL : THandle;


function LoadReportEngineDLL : Boolean;
var
  DllPath : String;
begin
  Result := False;
  hDLL := 0;
  DllPath := ExtractFilePath(Application.ExeName)+'\FFEReportEngine.DLL';
  if FileExists(DllPath) then begin
    hDLL := LoadLibrary(PChar(DllPath));
    if hDLL<>0 then begin
      @SingleTableReport := GetProcAddress(hDLL, 'SingleTableReport');
      @SingleQueryReport := GetProcAddress(hDLL, 'SingleQueryReport');
      @DesignReport := GetProcAddress(hDLL, 'DesignReport');
      { add new routines above, and tests for NIL below }
      if (@SingleTableReport<>NIL) and
         (@SingleQueryReport<>NIL) and
         (@DesignReport<>NIL) then
        Result := True;
    end;
  end;
end;

initialization
  ReportEngineDLLLoaded := LoadReportEngineDLL;

finalization
  if hDLL<>0 then
    FreeLibrary(hDLL);
end.
