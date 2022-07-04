{**************************************************************************************************}
{                                                                                                  }
{ Perforce for Delphi plugin (P4Delphi)                                                            }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Chris Fairall. Portions created by                 }
{ Chris Fairall are Copyright (C) Chris Fairall (cfairall at bigpond dot net dot au)               }
{ All rights reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit UnitP4Misc;

{----------------------------------------------------------------------------

   Unit Name     :  UnitP4Misc
   Date Created  :  21 May 2002
   Author        :  Chris Fairall
   Description   :  Miscellaneous functions and objects for Perforce expert.
                    Contains the Options object which holds the current
                    settings of the user from the Options dialogue.

   $File: //depot/Delphi/Toolkit/Experts/Perforce/UnitP4Misc.pas $
   $Revision: #7 $
   $DateTime: 2007/04/09 19:53:02 $
   $Author: Chris $

 ----------------------------------------------------------------------------}

interface

{$I P4Define.inc}

uses
  SysUtils, Windows, Classes, Forms, ShellAPI;

const
  MULTI_CHAR = ['*'];
  SINGLE_CHAR = ['?'];
  WILD_CARDS = MULTI_CHAR + SINGLE_CHAR;
  MAX_BUFFER_SIZE = 255;
  REG_KEY = '\Software\DGE Pty Ltd\Experts\Perforce';

type
  TWaitOption = (woNoWait, woUntilStart, woUntilFinish);
  EatsWindowsError = class(Exception);

  TP4Options = class(TObject)
  private
    FShowMsg: Boolean;
    FAddDDP: Boolean;
    FUseP4Diff: Boolean;
    FDiffProg: String;
    FAutoLock: Boolean;
    FCheckLatest: Boolean;
    FPreserveBkmk: Boolean;
    FPreserveSel: Boolean;
    FAddTodo: Boolean;
    FLastChangeList: Integer;
    FPassword: string;
    FscAddNewArchive: TShortcut;
    FscSubmit: TShortcut;
    FscOptions: TShortcut;
    FscAddAll: TShortcut;
    FscDiffHead: TShortcut;
    FscDiffCurrent: TShortcut;
    FscRevert: TShortcut;
    FscLock: TShortcut;
    FscAbout: TShortcut;
    FscOpenForEdit: TShortcut;
    FscProperties: TShortcut;
    FscSync: TShortcut;
    FscUnlock: TShortcut;
    FAutoCheckServer: Boolean;
    FCheckServerTime: Integer;
    FscRevHistory: TShortcut;
    FOpenForEditPrompt: boolean;
    FscVisualClient: TShortcut;
  public
    constructor Create; virtual;
    { Methods }
    procedure LoadFromReg(psRegKey : String);
    procedure SaveToReg(psRegKey : String);
    function ShowDialogue : Boolean;
    { Properties }
    property ShowMessages : Boolean read FShowMsg write FShowMsg;
    property AddDDPFiles : Boolean read FAddDDP write FAddDDP;
    property AddTodoFiles : Boolean read FAddTodo write FAddTodo;
    property DiffProg : String read FDiffProg write FDiffProg;
    property UseP4DiffProg : Boolean read FUseP4Diff write FUseP4Diff;
    property AutoLock : Boolean read FAutoLock write FAutoLock;
    property CheckForLatest : Boolean read FCheckLatest write FCheckLatest;
    property PreserveBookmarks : Boolean read FPreserveBkmk write FPreserveBkmk;
    property PreserveSelection : Boolean read FPreserveSel write FPreserveSel;
    property LastChangeList: Integer read FLastChangeList write FLastChangeList;
    property AutoCheckServer : Boolean read FAutoCheckServer write FAutoCheckServer;
    property CheckServerTime : Integer read FCheckServerTime write FCheckServerTime;
    property Password: string read FPassword write FPassword;
    property OpenForEditPrompt : boolean read FOpenForEditPrompt write FOpenForEditPrompt;
    { Shortcuts... }
    property SC_AddNewArchive : TShortcut read FscAddNewArchive write FscAddNewArchive;
    property SC_OpenForEdit : TShortcut read FscOpenForEdit write FscOpenForEdit;
    property SC_Revert : TShortcut read FscRevert write FscRevert;
    property SC_Submit : TShortcut read FscSubmit write FscSubmit;
    property SC_AddAll : TShortcut read FscAddAll write FscAddAll;
    property SC_Sync : TShortcut read FscSync write FscSync;
    property SC_Lock : TShortcut read FscLock write FscLock;
    property SC_Unlock : TShortcut read FscUnlock write FscUnlock;
    property SC_DiffCurrent : TShortcut read FscDiffCurrent write FscDiffCurrent;
    property SC_DiffHead : TShortcut read FscDiffHead write FscDiffHead;
    property SC_RevisionHistory : TShortcut read FscRevHistory write FscRevHistory;
    property SC_Properties : TShortcut read FscProperties write FscProperties;
    property SC_Options : TShortcut read FscOptions write FscOptions;
    property SC_About : TShortcut read FscAbout write FscAbout;
    property SC_VisualClient: TShortcut read FscVisualClient write FscVisualClient;
  end;

function PosBack(psSearch, psSource  :  String)  :  Integer;
function PosFrom(psSearch, psSource  :  String; piStartPos : Integer) : Integer;
function GetLastWindowsError : String;
function RunProgram(psCmdLine : String; WaitUntil : TWaitOption) : TProcessInformation;
function WildcardCompare(psWildCard, psCompare : String; pbCaseSens : Boolean = false) : Boolean;
function ExtractInfo(AInfo: TStringList; ASubStr: string): string;

implementation

uses
  Registry, FormOptions;

const
  REG_OPTIONS = 'Options';
  REG_SHORTCUTS = REG_OPTIONS + '\Shortcuts';

function PosBack(psSearch, psSource  :  String)  :  Integer;
var
  blnFound  :  Boolean;
begin
  Result := Length(psSource) - Length(psSearch) + 1;
  blnFound := false;
  while (Result > 0) and (not blnFound) do
    begin
      if psSearch = Copy(psSource, Result, Length(psSearch)) then
        blnFound := true
      else
        Result := Result - Length(psSearch);
    end;
  if Result < 0 then
    Result := 0;
end;

function PosFrom(psSearch, psSource  :  String; piStartPos : Integer) : Integer;
begin
  Dec(piStartPos);
  if piStartPos < 0 then
    piStartPos := 0;
  Delete(psSource, 1, piStartPos);
  Result := Pos(psSearch, psSource);
  if Result > 0 then
    Result := piStartPos + Result;
end;

function GetLastWindowsError : String;
var
  dwrdError          :  DWord;
  pchrBuffer         :  PChar;
begin
  dwrdError := GetLastError;
  GetMem(pchrBuffer, MAX_BUFFER_SIZE);
  try
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, dwrdError, 0, pchrBuffer, MAX_BUFFER_SIZE, nil);
    Result := String(pchrBuffer);
  finally
    FreeMem(pchrBuffer, MAX_BUFFER_SIZE);
  end;
end;

function RunProgram(psCmdLine : String; WaitUntil : TWaitOption) : TProcessInformation;
var
  sErrorMsg,
  sCmdLine       :  String;
  StartInfo      :  TStartupInfo;
  iExitCode      :  Cardinal;
begin
  sCmdLine := psCmdLine + #0;

  with StartInfo do
    begin
      lpReserved := nil;
      lpDesktop := nil;
      lpTitle := nil;
      dwFlags := 0;
      cbReserved2 := 0;
      lpReserved2 := nil;
    end;
  StartInfo.cb := SizeOf(StartInfo);

  if CreateProcess(nil, PChar(sCmdLine), nil, nil, true, 0, nil, nil,
                         StartInfo, Result) then
    begin
      if WaitUntil in [woUntilStart, woUntilFinish] then
        WaitForInputIdle(Result.hProcess, INFINITE);

      if WaitUntil = woUntilFinish then
        repeat
          Application.ProcessMessages;
          GetExitCodeProcess(Result.hProcess, iExitCode);
        until iExitCode <> STILL_ACTIVE;
    end
  else
    begin
      sErrorMsg := GetLastWindowsError;
      raise EatsWindowsError.Create(sErrorMsg);
    end;
end;   { RunProgram }

function WildcardCompare(psWildCard, psCompare : String; pbCaseSens : Boolean) : Boolean;
var
  strlstWild : TStringList;
  intPos,
  intStart,
  intCounter : Integer;
  strWork    : String;
  blnAtStart : Boolean;
begin
  { If it's not case sensitive, convert both strings to all uppercase. }
  if not pbCaseSens then
    begin
      psWildCard := UpperCase(psWildCard);
      psCompare := UpperCase(psCompare);
    end;

  { If either string is empty, return false immediately. }
  if (psWildCard = '') or (psCompare = '') then
    begin
      Result := false;
      exit;
    end;

  { ------------------------------------------------------------------------- }

  strlstWild := TStringList.Create;
  try
    { ----------------------------------------------------------------------- }
    { First, we split the wildcard string up into sections - text vs wild
      cards with a line in a string list for each.

      So, the wildcard "abc*def?ghi" would be broken up into a string list
      like this:
             abc
             *
             def
             ?
             ghi

      }
    intStart := 1;
    for intCounter := 1 to Length(psWildCard) do
      begin
        {$IFDEF UNICODE}
        if CharInSet(psWildCard[intCounter], WILD_CARDS) then
        {$ELSE}
        if psWildCard[intCounter] in WILD_CARDS then
        {$ENDIF}
          begin
            if intStart < intCounter then
              begin
                strWork := Copy(psWildCard, intStart, intCounter - intStart);
                strlstWild.Add(strWork);
              end;
            strlstWild.Add(psWildCard[intCounter]);
            intStart := intCounter + 1;
          end;
      end;
    { If there's still some characters left over after the last wildcard has been
      found, add them to the end of the string list. This is for wildcard strings
      like "*bob". }
    if intStart <= Length(psWildCard) then
      begin
        strWork := Copy(psWildCard, intStart, Length(psWildCard));
        strlstWild.Add(strWork);
      end;

    { ----------------------------------------------------------------------- }

    Result := true;
    blnAtStart := true;
    intStart := 1;
    intCounter := 0;
    while (intCounter < strlstWild.Count) and Result do
      begin
        strWork := strlstWild[intCounter];
        {$IFDEF UNICODE}
        if (Length(strWork) = 1) and (CharInSet(strWork[1], WILD_CARDS)) then
        {$ELSE}
        if (Length(strWork) = 1) and (strWork[1] in WILD_CARDS) then
        {$ENDIF}
          begin
            {$IFDEF UNICODE}
            if CharInSet(strWork[1], MULTI_CHAR) then
            {$ELSE}
            if strWork[1] in MULTI_CHAR then
            {$ENDIF}
              { A multi-character wildcard (eg "*") }
              blnAtStart := false
            else
              begin
                { A single-character wildcard (eg "?") }
                blnAtStart := true;
                if intStart > Length(psCompare) then
                  Result := false;
                Inc(intStart);
              end;
          end
        else
          begin
            if blnAtStart then
              begin
                { Text after a "?" }
                if Copy(psCompare, intStart, Length(strWork)) = strWork then
                  intStart := intStart + Length(strWork)
                else
                  Result := false;
              end
            else
              begin
                { Text after a "*" }
                if intCounter = strlstWild.Count - 1 then
                  intPos := PosBack(strWork, psCompare)
                else
                  intPos := PosFrom(strWork, psCompare, intStart);
                if intPos > 0 then
                  intStart := intPos + Length(strWork)
                else
                  Result := false;
              end;
            blnAtStart := true;
          end;
        Inc(intCounter);
      end;
    if Result and (blnAtStart) and (intStart <= Length(psCompare)) then
      Result := false;
  finally
    strlstWild.Free;
  end;
end;

function ExtractInfo(AInfo: TStringList; ASubStr: string): string;
var
  APos, I: integer;
begin
  Result := EmptyStr;
  for I := 0 to AInfo.Count - 1 do
  begin
    if Pos(ASubStr, AInfo[I]) > 0 then
    begin
      APos := Pos(': ', AInfo[I]);
      Result := Trim(Copy(AInfo[I], APos + 2, Length(AInfo[I])));
    end;
  end;
end;

{ TP4Options }

constructor TP4Options.Create;
begin
  inherited Create;
  FShowMsg := true;
  FAddDDP := false;
  FAddTodo := false;
  FUseP4Diff := true;
  FAutoLock := false;
  FDiffProg := '';
  FCheckLatest := true;

  FPreserveBkmk := true;
  FPreserveSel := true;

  FLastChangeList := -1;
  FPassword := '';

  FOpenForEditPrompt := true;

  FscAddNewArchive := 0;
  FscSubmit := 0;
  FscOptions := 0;
  FscAddAll := 0;
  FscDiffHead := 0;
  FscDiffCurrent := 0;
  FscRevert := 0;
  FscLock := 0;
  FscAbout := 0;
  FscOpenForEdit := 0;
  FscRevHistory := 0;
  FscProperties := 0;
  FscSync := 0;
  FscUnlock := 0;
  FscVisualClient := 0;

  FCheckServerTime := 5;
  FAutoCheckServer := true;

  LoadFromReg(REG_KEY);
end;

procedure TP4Options.LoadFromReg(psRegKey: String);
begin
  with TRegIniFile.Create(psRegKey) do
    try
      FShowMsg := ReadBool(REG_OPTIONS, 'Show Messages', FShowMsg);
      FAddDDP := ReadBool(REG_OPTIONS, 'Add DDP Files', FAddDDP);
      FAddTodo := ReadBool(REG_OPTIONS, 'Add Todo Files', FAddTodo);
      FUseP4Diff := ReadBool(REG_OPTIONS, 'Use P4 Diff', FUseP4Diff);
      FDiffProg := ReadString(REG_OPTIONS, 'Alternate Diff Program', FDiffProg);
      FAutoLock := ReadBool(REG_OPTIONS, 'Auto Lock', FAutoLock);
      FCheckLatest := ReadBool(REG_OPTIONS, 'Check For Latest Version', FCheckLatest);
      FPreserveBkmk := ReadBool(REG_OPTIONS, 'Preserve Bookmarks', FPreserveBkmk);
      FPreserveSel := ReadBool(REG_OPTIONS, 'Preserve Selection', FPreserveSel);
      FLastChangeList := ReadInteger(REG_OPTIONS, 'Last Changelist', FLastChangeList);
      FAutoCheckServer := ReadBool(REG_OPTIONS, 'Automatically check server', FAutoCheckServer);
      FCheckServerTime := ReadInteger(REG_OPTIONS, 'Check server time', FCheckServerTime);
      FPassword := ReadString(REG_OPTIONS, 'Password', FPassword);
      FOpenForEditPrompt := ReadBool(REG_OPTIONS, 'Open For Edit Prompt', FOpenForEditPrompt);
      { Shortcuts }
      FscAddNewArchive := ReadInteger(REG_SHORTCUTS, 'Add New Archive', FscAddNewArchive);
      FscOpenForEdit := ReadInteger(REG_SHORTCUTS, 'Open For Edit', FscOpenForEdit);
      FscSubmit := ReadInteger(REG_SHORTCUTS, 'Submit', FscSubmit);
      FscRevert := ReadInteger(REG_SHORTCUTS, 'Revert Changes', FscRevert);
      FscAddAll := ReadInteger(REG_SHORTCUTS, 'Add All Files In Project', FscAddAll);
      FscSync := ReadInteger(REG_SHORTCUTS, 'Sync', FscSync);
      FscLock := ReadInteger(REG_SHORTCUTS, 'Lock', FscLock);
      FscUnlock := ReadInteger(REG_SHORTCUTS, 'Unlock', FscUnlock);
      FscDiffCurrent := ReadInteger(REG_SHORTCUTS, 'Diff Against Current Revision', FscDiffCurrent);
      FscDiffHead := ReadInteger(REG_SHORTCUTS, 'Diff Against Head Revision', FscDiffHead);
      FscRevHistory := ReadInteger(REG_SHORTCUTS, 'Revision History', FscRevHistory);
      FscProperties := ReadInteger(REG_SHORTCUTS, 'File Properties', FscProperties);
      FscOptions := ReadInteger(REG_SHORTCUTS, 'Options', FscOptions);
      FscAbout := ReadInteger(REG_SHORTCUTS, 'About', FscAbout);
    finally
      Free;
    end;
end;

procedure TP4Options.SaveToReg(psRegKey: String);
begin
  with TRegIniFile.Create(psRegKey) do
    try
      WriteBool(REG_OPTIONS, 'Show Messages', FShowMsg);
      WriteBool(REG_OPTIONS, 'Add DDP Files', FAddDDP);
      WriteBool(REG_OPTIONS, 'Add Todo Files', FAddTodo);
      WriteBool(REG_OPTIONS, 'Use P4 Diff', FUseP4Diff);
      WriteString(REG_OPTIONS, 'Alternate Diff Program', FDiffProg);
      WriteBool(REG_OPTIONS, 'Auto Lock', FAutoLock);
      WriteBool(REG_OPTIONS, 'Check For Latest Version', FCheckLatest);
      WriteBool(REG_OPTIONS, 'Preserve Bookmarks', FPreserveBkmk);
      WriteBool(REG_OPTIONS, 'Preserve Selection', FPreserveSel);
      WriteInteger(REG_OPTIONS, 'Last Changelist', FLastChangeList);
      WriteBool(REG_OPTIONS, 'Automatically check server', FAutoCheckServer);
      WriteInteger(REG_OPTIONS, 'Check server time', FCheckServerTime);
      WriteString(REG_OPTIONS, 'Password', FPassword);
      WriteBool(REG_OPTIONS, 'Open For Edit Prompt', FOpenForEditPrompt);
      { Shortcuts }
      WriteInteger(REG_SHORTCUTS, 'Add New Archive', FscAddNewArchive);
      WriteInteger(REG_SHORTCUTS, 'Open For Edit', FscOpenForEdit);
      WriteInteger(REG_SHORTCUTS, 'Submit', FscSubmit);
      WriteInteger(REG_SHORTCUTS, 'Revert Changes', FscRevert);
      WriteInteger(REG_SHORTCUTS, 'Add All Files In Project', FscAddAll);
      WriteInteger(REG_SHORTCUTS, 'Sync', FscSync);
      WriteInteger(REG_SHORTCUTS, 'Lock', FscLock);
      WriteInteger(REG_SHORTCUTS, 'Unlock', FscUnlock);
      WriteInteger(REG_SHORTCUTS, 'Diff Against Current Revision', FscDiffCurrent);
      WriteInteger(REG_SHORTCUTS, 'Diff Against Head Revision', FscDiffHead);
      WriteInteger(REG_SHORTCUTS, 'Revision History', FscRevHistory);
      WriteInteger(REG_SHORTCUTS, 'File Properties', FscProperties);
      WriteInteger(REG_SHORTCUTS, 'Options', FscOptions);
      WriteInteger(REG_SHORTCUTS, 'About', FscAbout);
    finally
      Free;
    end;
end;

function TP4Options.ShowDialogue: Boolean;
begin
  with TfrmOptions.Create(nil) do
    try
      Result := Execute(Self);
      if Result then
        SaveToReg(REG_KEY);
    finally
      Free;
    end;
end;

end.
