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
{ Contributor(s): Jaimy Azle                                                                       }
{                                                                                                  }
{**************************************************************************************************}
unit UnitP4Engine;
interface

{$I P4Define.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, Dialogs;

type
  EPipeError     = class(Exception);
  EPerforceError = class(Exception);

  TPerforceEngine = class(TObject)
  private
    FLastCmd,
    FLastOutput            : String;
    FServerUp              : Boolean;
    FLoggedIn              : Boolean;
    FLastServerCheck       : TDateTime;
    function GetIsServerUp : Boolean;
    function OffsetFromUTC: TDateTime;
  protected
    function RunPipe(psCommandLine, psWorkDir : String; phInputHandle : THandle) : String; overload;
    function RunPipe(psCommandLine, psWorkDir : String) : String; overload;
    function OpenInputFile(FileName : String) : THandle;
    procedure CloseInputFile(phHandle : THandle);
    function QuotedFileList(FileList : TStrings): String;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Command(const Params : String) : String; overload;
    function Command(const Params, RunDir: String): String; overload;
    function Command(const Params, RunDir: String; InputData : TStrings) : String; overload;
    function Command(const Params, RunDir: String; InputData : String) : String; overload;

    { Login / Logout }
    function Login(const Password: string): string;
    function Logout: string;
    function LoggedIn: Boolean;

    { Server connection }
    procedure CheckServerStatus;
    property IsServerUp : Boolean read GetIsServerUp;

    { General }
    procedure Info(InfoData : TStrings; const Format : Boolean);
    function CurrentUser : String;
    function P4DateToDateTime(piP4Date : Int64) : TDateTime;

    property LastCmd : String read FLastCmd;
    property LastOutput : String read FLastOutput;

    { Changelists }
    function CreateChangeList(const Description : String) : Integer;
    procedure UpdateChangeList(const ChangeListNbr : Integer; const Description : String);
    function ChangeListInfo(ChangeListNbr : Integer; pslDesc, pslFiles, pslJobs : TStrings) : Boolean;
    function PendingChangeLists(pslLists : TStrings; const IncludeText : Boolean = true) : Integer;

    { Files }
    function GetFileInfo(const FileName : String; Info : TStrings) : Boolean;
    function Sync(FileList : TStrings; const Force : Boolean) : String;
    function FileArchived(const FileName : String) : Boolean;
    function HaveHeadRevision(psFileName : String) : Boolean;
    function AddArchive(FileList : TStrings) : String; overload;
    function AddArchive(FileList : TStrings; const ChangeListNbr : Integer) : String; overload;
    function OpenForEdit(FileList : TStrings) : String; overload;
    function OpenForEdit(FileList : TStrings; const ChangeListNbr : Integer): String; overload;
    function LockFiles(FileList : TStrings) : String;
    function UnlockFiles(FileList : TStrings) : String;
    function RevertFiles(FileList : TStrings) : String;
    procedure ShowRevisionHistory(const FileName : String);
    procedure ShowVisualClient;
  end;

var
  P4Engine : TPerforceEngine;

implementation

uses
  UnitP4Misc, DateUtils, Registry, UnitP4Expert;

const
  P4_CMD    = 'p4 ';
  BUFFER_SIZE = 4096;
  { Use this template to generate an input file for the creation of a new change
    list. Set the text property of a string list to it, and then Add description
    lines (using DESC_LINE constant as a format template), save the file, pass
    it to the RunPipe command. This all done automatically by the BuildChangeList
    method. }
  CHANGE_FILE_TEMPLATE = 'Change:'#9'new'#13#10 +
                           'Status:'#9'new'#13#10 +
                           'Description:';
  { Use this template to generate an input file for the creation of a new label.
    Set the text property of a string list to it, and then Add description
    lines (using DESC_LINE constant as a format template), save the file, pass
    it to the RunPipe command. This all done automatically by the AddFileLabel
    method. }
  LABEL_FILE_TEMPLATE = 'Label:'#9'%s'#13#10 +
                          'Options:'#9'unlocked'#13#10 +
                          'View:'#13#10 +
                          #9'//depot/...'#13#10 +
                          'Description:';
  ADD_CLIENT_TEMPLATE = 'Client:'#9'%s'#13#10 +
                          'Owner:'#9'%s'#13#10 +
                          'Host:'#9'%s'#13#10 +
                          'Root:'#9'%s'#13#10 +
                          'Description:'#13#10#9'%s';
  ADD_USER_TEMPLATE   = 'User:'#9'%s'#13#10 +
                          'Email:'#9'%s'#13#10 +
                          'FullName:'#9'%s'#13#10;
  { Server Down Message }
  SERVER_DOWN_ERROR_MSG = 'error: Failed to connect to Peforce server.';

{ TPerforceEngine }

procedure TPerforceEngine.CloseInputFile(phHandle: THandle);
begin
  CloseHandle(phHandle);
end;

constructor TPerforceEngine.Create;
begin
  inherited Create;
  FLastCmd := '';
  FLastOutput := '';
  CheckServerStatus;
end;

destructor TPerforceEngine.Destroy;
begin
  inherited Destroy;
end;

function TPerforceEngine.RunPipe(psCommandLine, psWorkDir: String; phInputHandle: THandle): String;
var
  iError,
  iBytesRead,
  hReadHandle,
  hWriteHandle  : Cardinal;
  Security      : TSecurityAttributes;
  ProcInfo      : TProcessInformation;
  Buf           : PByte;
  StartupInfo   : TStartupInfo;
  bDone         : Boolean;
  iCounter      : Integer;
  ACmdLine, AWorkDir: Array[0..512] of char;
begin
  Result := '';

  Security.lpSecurityDescriptor := nil;
  Security.bInheritHandle := true;
  Security.nLength := SizeOf(Security);

  if CreatePipe(hReadHandle, hWriteHandle, @Security, BUFFER_SIZE) then
    try
      { Startup Info for command process }
      with StartupInfo do
        begin
          lpReserved := nil;
          lpDesktop := nil;
          lpTitle := nil;
          dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
          cbReserved2 := 0;
          lpReserved2 := nil;
          { Prevent the command window being displayed }
          wShowWindow := SW_HIDE;
          { Standard Input - Default handle }
          if phInputHandle = 0 then
            hStdInput := GetStdHandle(STD_INPUT_HANDLE)
          else
            hStdInput := phInputHandle;
          { Standard Output - Point to Write end of pipe }
          hStdOutput := hWriteHandle;
          { Standard Error - Default handle }
          hStdError := GetStdHandle(STD_ERROR_HANDLE);
        end;
      StartupInfo.cb := SizeOf(StartupInfo);

      { Create the command as a process }
      StrPCopy(ACmdLine, psCommandLine);
      StrPCopy(AWorkDir, psWorkDir);
      if CreateProcess(nil, ACmdLine, nil, nil, true, 0, nil, AWorkDir, StartupInfo, ProcInfo) then
        try
          { We don't need this handle any more, and keeping it open on this end
            will cause errors. It remains open for the child process though. }
          CloseHandle(hWriteHandle);
          { Allocate memory to the buffer }
          GetMem(Buf, BUFFER_SIZE * SizeOf(Char));
          try
            bDone := false;
            while not bDone do
              begin
                Application.ProcessMessages;
                if not Windows.ReadFile(hReadHandle, Buf^, BUFFER_SIZE, iBytesRead, nil) then
                  begin
                    iError := GetLastError;
                    case iError of
                      ERROR_BROKEN_PIPE:
                        begin
                          // Broken pipe means client app has ended.
                          bDone := true;
                        end;
                      ERROR_INVALID_HANDLE:
                        raise EPipeError.Create('Error: Invalid Handle');
                      ERROR_HANDLE_EOF:
                        raise EPipeError.Create('Error: End of file');
                      ERROR_IO_PENDING:
                        begin
                          // Do nothing... just waiting
                        end;
                    else
                      raise EPipeError.Create('Error #' + IntToStr(iError));
                    end;
                  end;
                if iBytesRead > 0 then
                  begin
                    for iCounter := 0 to iBytesRead - 1 do
                      Result := Result + Char(PAnsiChar(Buf)[iCounter]);
                  end;
              end;
          finally
            FreeMem(Buf, BUFFER_SIZE);
          end;
        finally
          CloseHandle(ProcInfo.hThread);
          CloseHandle(ProcInfo.hProcess);
        end
      else
        begin
          CloseHandle(hWriteHandle);
          raise EPipeError.Create('Failed to start. (Command="' + psCommandLine + '").');
        end;
    finally
      CloseHandle(hReadHandle);
    end;
end;

function TPerforceEngine.OpenInputFile(FileName: String): THandle;
var
  Security   : TSecurityAttributes;
begin
  Security.lpSecurityDescriptor := nil;
  Security.bInheritHandle := true;
  Security.nLength := SizeOf(Security);

  Result := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, @Security,
             OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
end;

function TPerforceEngine.RunPipe(psCommandLine, psWorkDir: String): String;
begin
  Result := RunPipe(psCommandLine, psWorkDir, 0);
end;

function TPerforceEngine.Command(const Params, RunDir: String): String;
begin
  FLastCmd := P4_CMD + Params;

  FLastOutput := RunPipe(FLastCmd, RunDir);
  Result := FLastOutput;
end;

function TPerforceEngine.Command(const Params: String): String;
begin
  Result := Command(Params, 'C:\');
end;

function TPerforceEngine.Command(const Params, RunDir: String; InputData: String): String;
var
  slInput : TStringList;
begin
  if Self.IsServerUp then
    begin
      slInput := TStringList.Create;
      try
        slInput.Text := InputData;
        Result := Command(Params, RunDir, slInput);
      finally
        slInput.Free;
      end;
    end
  else
    Result := SERVER_DOWN_ERROR_MSG;
end;

function TPerforceEngine.Command(const Params, RunDir: String; InputData: TStrings): String;

  function GetTempFile: string;
  var
    PathName: array[0..MAX_PATH] of Char;
  begin
    Windows.GetTempPath(MAX_PATH, @PathName);
    Result := string(PathName);
    if AnsiLastChar(Result)^ <> '\' then Result := Result + '\';
    Result := Result + 'p4_temp_file.txt';
  end;

var
  hInput : THandle;
begin
  FLastCmd := P4_CMD + Params;

  Result := '';

  if Self.IsServerUp then
    begin
      InputData.SaveToFile(GetTempFile);
      try
        hInput := OpenInputFile(GetTempFile);
        try
          FLastOutput := RunPipe(FLastCmd, RunDir, hInput);
          Result := FLastOutput;
        finally
          CloseInputFile(hInput);
        end;
      finally
        DeleteFile(GetTempFile);
      end;
    end
  else
    Result := SERVER_DOWN_ERROR_MSG;
end;

procedure TPerforceEngine.Info(InfoData: TStrings; const Format: Boolean);
var
  i: Integer;
begin
  InfoData.Text := Command('info');
  if Format then
    begin
      for i := 0 to InfoData.Count - 1 do
        InfoData[i] := StringReplace(InfoData[i], ': ', '=', []);
    end;

end;

function TPerforceEngine.CreateChangeList(const Description: String): Integer;
var
  slFile,
  slDesc   : TStringList;
  iCounter : Integer;
  sDesc,
  sResult  : String;
begin
  Result := -1;

  sDesc := Trim(Description);
  if sDesc = '' then
    sDesc := '(no comment supplied)';

  slFile := TStringList.Create;
  slDesc := TStringList.Create;
  try
    slDesc.Text := sDesc;

    slFile.Text := CHANGE_FILE_TEMPLATE;
    if slDesc.Count = 0 then
      raise EPerforceError.Create('Error: Invalid description for changelist.');

    for iCounter := 0 to slDesc.Count - 1 do
      begin
        slFile.Add(#9 + slDesc[iCounter]);
      end;

    sResult := Command('-s change -i ', 'c:\', slFile);
    if WildcardCompare('*Change * created*', sResult) then
      begin
        Delete(sResult, 1, Pos('Change', sResult) + 6);
        Delete(sResult, Pos('created', sResult), Length(sResult));
        sResult := Trim(sResult);
        Result := StrToInt(sResult);
      end
    else
      raise EPerforceError.Create('Couldn''t create changelist: "' + sResult + '".');
  finally
    slDesc.Free;
    slFile.Free;
  end;
end;

function TPerforceEngine.OffsetFromUTC: TDateTime;
var
  iBias: Integer;
  tmez: TTimeZoneInformation;
begin
  Case GetTimeZoneInformation(tmez) of
    TIME_ZONE_ID_INVALID:
      raise Exception.Create('Failed attempting to retrieve time zone information.');
    TIME_ZONE_ID_UNKNOWN  :
       iBias := tmez.Bias;
    TIME_ZONE_ID_DAYLIGHT :
      iBias := tmez.Bias + tmez.DaylightBias;
    TIME_ZONE_ID_STANDARD :
      iBias := tmez.Bias + tmez.StandardBias;
    else
      raise Exception.Create('Failed attempting to retrieve time zone information.');
  end;
  {We use ABS because EncodeTime will only accept positve values}
  Result := EncodeTime(Abs(iBias) div 60, Abs(iBias) mod 60, 0, 0);
  {The GetTimeZone function returns values oriented towards convertin
   a GMT time into a local time.  We wish to do the do the opposit by returning
   the difference between the local time and GMT.  So I just make a positive
   value negative and leave a negative value as positive}
  if iBias > 0 then begin
    Result := 0 - Result;
  end;
end;


procedure TPerforceEngine.UpdateChangeList(const ChangeListNbr: Integer;
  const Description: String);
var
  i,
  Index     : Integer;
  NewText,
  Current   : TStrings;
  sResult   : String;
begin
  Current := TStringList.Create;
  NewText := TStringList.Create;
  try
    Current.Text := Command('change -o ' + IntToStr(ChangeListNbr));
    Index := Current.IndexOf('Description:') + 1;
    while (Index < Current.Count) and (Length(Current[Index]) > 0) and (Current[Index][1] = #9) do
      Current.Delete(Index);

    NewText.Text := Description;
    for i := 0 to NewText.Count - 1 do
      begin
        Current.Insert(Index, #9 + NewText[i]);
        Inc(Index);
      end;
    sResult := Command('-s change -i ', 'C:\', Current);
    if not WildcardCompare('*Change * updated*', sResult) then
      raise EPerforceError.Create('Failed to update changelist: "' + sResult + '".');
  finally
    Current.Free;
    NewText.Free;
  end;
end;

function TPerforceEngine.ChangeListInfo(ChangeListNbr: Integer; pslDesc,
  pslFiles, pslJobs: TStrings): Boolean;
var
  slData     : TStringList;
  iCounter   : Integer;
  sWork      : String;
  slCurrent  : TStrings;
begin
  Result := true;

  if pslDesc <> nil then
    pslDesc.Clear;
  if pslFiles <> nil then
    pslFiles.Clear;
  if pslJobs <> nil then
    pslJobs.Clear;

  slData := TStringList.Create;
  try
    slData.Text := Command('change -o ' + IntToStr(ChangeListNbr));
    slCurrent := nil;
    for iCounter := 0 to slData.Count - 1 do
      begin
        sWork := slData[iCounter];
        if WildcardCompare('Change*unknown', sWork) then
          Result := false
        else if sWork = 'Description:' then
          slCurrent := pslDesc
        else if sWork = 'Files:' then
          slCurrent := pslFiles
        else if sWork = 'Jobs:' then
          slCurrent := pslJobs
        else if slCurrent <> nil then
          begin
            if (Length(sWork) > 0) and (sWork[1] = #9) then
              slCurrent.Add(Trim(sWork));
          end;
      end;
  finally
    slData.Free;
  end;
end;

function TPerforceEngine.PendingChangeLists(pslLists: TStrings; const IncludeText: Boolean): Integer;
var
  slTemp    : TStringList;
  iCounter  : Integer;
  sLine,
  sUser,
  sWork     : String;
begin
  pslLists.Clear;
  sUser := CurrentUser;

  slTemp := TStringList.Create;
  try
    slTemp.Text := Command('changes -s pending');
    for iCounter := 0 to slTemp.Count - 1 do
      begin
        sWork := slTemp[iCounter];
        if (Copy(sWork, 1, 6) = 'Change') and (Pos(sUser, sWork) > 0) then
          begin
            sLine := Copy(sWork, 8, PosFrom(' ', sWork, 8) - 8);
            if IncludeText then
              begin
                sLine := sLine + ' - ' + Copy(sWork, Pos(#39, sWork), Length(sWork));
              end;
            pslLists.Add(sLine);
          end;
      end;
  finally
    slTemp.Clear;
  end;
  Result := pslLists.Count;
end;

function TPerforceEngine.CurrentUser: String;
var
  slData : TStringList;
  sWork  : String;
begin
  slData := TStringList.Create;
  try
    slData.Text := Command('set');

    sWork := slData.Values['p4user'];
    if Pos(' ', sWork) > 0 then
      Delete(sWork, Pos(' ', sWork), Length(sWork));
    Result := sWork;

    sWork := slData.Values['p4client'];
    if Pos(' ', sWork) > 0 then
      Delete(sWork, Pos(' ', sWork), Length(sWork));
    Result := Result + '@' + sWork;

  finally
    slData.Free;
  end;
end;

function TPerforceEngine.GetFileInfo(const FileName: String; Info: TStrings): Boolean;
var
  iCounter   : Integer;
  sWork      : String;
begin
  Result := true;
  Info.Clear;

  if not FileExists(FileName) then
    Result := false
  else
    begin
      Info.Text := Command('-s fstat "' + ExtractFileName(FileName) + '"',
                     ExtractFilePath(FileName));

      iCounter := 0;
      while iCounter < Info.Count do
        begin
          if Trim(Info[iCounter]) = '' then
            Info.Delete(iCounter)
          else if Copy(Info[iCounter], 1, 5) = 'exit:' then
            Info.Delete(iCounter)
          else
            begin
              if Copy(Info[iCounter], 1, 7) = 'error: ' then
                begin
                  sWork := Info[iCounter];
                  Delete(sWork, 1, 7);
                  Info[iCounter] := sWork;
                  Result := false;
                end;
              if Copy(Info[iCounter], 1, 4) = 'info' then
                begin
                  sWork := Info[iCounter];
                  Delete(sWork, 1, Pos(' ', sWork));
                  sWork := Trim(sWork);
                  sWork := StringReplace(sWork, ' ', '=', []);
                  Info[iCounter] := sWork;
                end;
              Inc(iCounter);
            end;
        end;
    end;
end;

function TPerforceEngine.Sync(FileList: TStrings; const Force : Boolean) : String;
var
  Cmd: String;
begin
  Result := '';
  if FileList.Count > 0 then
    begin
      Cmd := 'sync';
      if Force then
        Cmd := Cmd + ' -f';
      Cmd := Cmd + QuotedFileList(FileList);
      Result := Command(Cmd);
    end;
end;

function TPerforceEngine.P4DateToDateTime(piP4Date: Int64): TDateTime;
begin
  { Perforce Dates are based from 0:00:00, 1 January 1970
            (source: Perforce 2000.2 Command Reference PDF file - page 43) }
  Result := EncodeDate(1970, 1, 1);
  { Perforce Date/time is measured in seconds }
  Result := IncSecond(Result, piP4Date);

  { Dates in Perforce are stored in UTC (Universal Time Co-ordinate), so we need
    to convert it to local time. This function is in the Indy package. }
  Result := Result + OffsetFromUTC;
end;

function TPerforceEngine.FileArchived(const FileName: String): Boolean;
var
  slData   : TStringList;
  sPath,
  sName    : String;
  iCounter : Integer;
begin
  slData := TStringList.Create;
  try
    sPath := ExtractFilePath(FileName);
    sName := ExtractFileName(FileName);
    slData.Text := Command('-s fstat "' + sName + '"', ExtractFilePath(sPath));
    Result := true;

    iCounter := 0;
    while (iCounter < slData.Count) and Result do
      begin
        if Uppercase(Copy(slData[iCounter], 1, 7)) = 'ERROR: ' then
          Result := false;
        Inc(iCounter);
      end;

  finally
    slData.Free;
  end;
end;

function TPerforceEngine.HaveHeadRevision(psFileName: String): Boolean;
var
  slData   : TStringList;
begin
  slData := TStringList.Create;
  try
    if GetFileInfo(psFileName, slData) then
      Result := slData.Values['headRev'] = slData.Values['haveRev']
    else
      Result := true;
  finally
    slData.Free;
  end;
end;

function TPerforceEngine.AddArchive(FileList: TStrings): String;
begin
  Result := AddArchive(FileList, -1);
end;

function TPerforceEngine.AddArchive(FileList: TStrings; const ChangeListNbr: Integer): String;
var
  Cmd : String;
begin
  Cmd := 'add';
  if ChangeListNbr <> -1 then
    Cmd := Cmd + ' -c ' + IntToStr(ChangeListNbr);

  Cmd := Cmd + QuotedFileList(FileList);

  Result := Command(Cmd);
end;

function TPerforceEngine.OpenForEdit(FileList: TStrings): String;
begin
  Result := OpenForEdit(FileList, -1);
end;

function TPerforceEngine.OpenForEdit(FileList: TStrings; const ChangeListNbr: Integer): String;
var
  Cmd : String;
begin
  Cmd := 'edit';
  if ChangeListNbr <> -1 then
    Cmd := Cmd + ' -c ' + IntToStr(ChangeListNbr);

  Cmd := Cmd + QuotedFileList(FileList);

  Result := Command(Cmd);
end;

function TPerforceEngine.QuotedFileList(FileList: TStrings): String;
var
  i : Integer;
begin
  Result := '';

  for i := 0 to FileList.Count - 1 do
    begin
      Result := Result + Format(' "%s"', [FileList[i]]);
    end;
end;

function TPerforceEngine.LockFiles(FileList: TStrings): String;
var
  Cmd : String;
begin
  Cmd := 'lock' + QuotedFileList(FileList);

  Result := Command(Cmd);
end;

function TPerforceEngine.RevertFiles(FileList: TStrings): String;
var
  Cmd : String;
begin
  Cmd := 'revert' + QuotedFileList(FileList);

  Result := Command(Cmd);
end;

function TPerforceEngine.UnlockFiles(FileList: TStrings): String;
var
  Cmd : String;
begin
  Cmd := 'unlock' + QuotedFileList(FileList);

  Result := Command(Cmd);
end;

function TPerforceEngine.Login(const Password: string): string;
begin
  Result := Command('login', 'C:\', Password);
end;

function TPerforceEngine.Logout: string;
begin
  Result := Command('logout');
end;

function TPerforceEngine.LoggedIn: Boolean;
begin
  if PerforceExpert.Options.AutoCheckServer
    and (MinutesBetween(Now, FLastServerCheck) >= PerforceExpert.Options.CheckServerTime) then
    CheckServerStatus;
  Result := FLoggedIn;
end;

procedure TPerforceEngine.CheckServerStatus;
var
  s : String;
begin
  s := RunPipe('p4 info', 'c:\');
  FServerUp := (s <> '');
  FLastServerCheck := Now;
  FLoggedIn := FServerUp and (Length(Command('changelists -m 1')) > 0);
end;

function TPerforceEngine.GetIsServerUp: Boolean;
begin
  if PerforceExpert.Options.AutoCheckServer
    and (MinutesBetween(Now, FLastServerCheck) >= PerforceExpert.Options.CheckServerTime) then
    CheckServerStatus;
  Result := FServerUp;
end;

procedure TPerforceEngine.ShowRevisionHistory(const FileName: String);
begin
  RunProgram('p4win -H "' + FileName + '"', woUntilStart);
end;

procedure TPerforceEngine.ShowVisualClient;
var
  AInfo: TStringList;
  AUserName, AClient, APort: string;
begin
  AInfo := TStringList.Create;
  try
    Info(AInfo, false);
    AUserName := ExtractInfo(AInfo, 'User name');
    AClient   := ExtractInfo(AInfo, 'Client name');
    APort     := ExtractInfo(AInfo, 'Server address');
  finally
    AInfo.Free;
  end;
  RunProgram('p4v -u "' + AUserName +
                '" -c "' + AClient +
                '" -p "' + APort +
                '"', woUntilStart);
end;

initialization
  P4Engine := TPerforceEngine.Create;

finalization
  FreeAndNil(P4Engine);

end.
