{*********************************************************}
{* FlashFiler: Command line conversion utility           *}
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

program FFCnvrtC;
{$APPTYPE CONSOLE}
uses
  FFMemMgr,
  {$IFDEF USETeDEBUG}
  TeDebug,
  {$ENDIF}
  Classes,
  SysUtils,
  FileCtrl,
  Windows,
  FFConvrt,
  FFSrEng,
  FFLLEng,
  FFLLComp;

{$R *.RES}

type
  FF2CvtErrorCode = (cecNone,
                       {No errors}
                     cecNoDestination,
                       {Target parameter doesn't exist}
                     cecNoSource,
                       {One of the source files does not exist}
                     cecTooManySources,
                       {Only 1 source parameter is allowed}
                     cecNoTables,
                       {no tables were listed or in the source directory}
                     cecInvalidTable,
                       {The table doesn't exist in the source directory}
                     cecOverwrite,
                       {There are file(s) of the same name as a source
                        file in the destination directory}
                     cecInvalidSource,
                       {No valid source directory were given}
                     cecInvalidDestination,
                       {No valid target directory was given}
                     cecDataConvertFailed,
                       {The data conversion failed}
                     cecNoParameters,
                       {No parameters given}
                     cecUnknownFailure);
                       {Conversion Failed: unknown reason}

  {This class is only here to provide a event handler for
   TffDataConverter.OnProgress event}
  TFFConvUtil = class
  public
    procedure OnProgress(aSender : TffDataConverter);
  end;

var
  FF2Server      : TffServerEngine;
  TableConverter : TffDataConverter;
  Utility        : TFFConvUtil;
  SourceTables   : TStringList;
  Destination    : string;
  SourceDir      : string;
  ScreenPos      : TCoord;
  CurrentTable   : Integer;
  GoodSource     : Boolean;
  GoodDest       : Boolean;

{--------}
function WillOverwrite : boolean;
var
  i : integer;
begin
  Result := False;
  {check if any of the selected files in srcFiles have the same name
   as any files in the destination directory.}
  for i := 0 to pred(SourceTables.Count) do begin
    {Ensure this file isn't in the destination directory.}
    if FileExists((Destination + '\' + ChangeFileExt(SourceTables[i], '.FF2'))) then begin
      writeln(format('*** ERROR: %s already in destination ***', [ExtractFileName(SourceTables[i])]));
      writeln;
      ExitCode := integer(cecOverwrite);
      Result := True;
      Exit;
    end;
  end;
end;
{--------}
procedure DisplayHelp;
begin
  writeln('Converts a FlashFiler 1 table to a FlashFiler 2 table.');
  writeln;
  writeln('FFCnvrtC -s<complete source path> -d<complete destination path> [-t<table name>]');
  writeln('Multiple tables can be listed or leave off the table parameter');
  writeln('to convert all tables in the source.');
  writeln('Example:');
  writeln('   FFCnvrtC -sC:\MyApp -dC:\MyNewApp -tTable1 -tTable2');
  writeln;
end;
{--------}
procedure BuildTableList;
var
  ATable    : TSearchRec;
  CurrTable : string;
  i         : integer;
begin
  {are table parameters given?}
  if (StrPos(CmdLine, '-t') <> nil) then begin
    {yes. we need to add each table to SourceTables}
    for i := 1 to ParamCount do begin
      {if the second letter in the parameter is an t, it's a table}
      if IsDelimiter('t', ParamStr(i), 2) then begin
        CurrTable := ParamStr(i);
        {strip delimiter off table parameter}
        Delete(CurrTable, 1, 2);
        {does the target file actually exist?}
        if FileExists(SourceDir + '\' + CurrTable + '.FFD') then begin
          {add the table to SourceTables}
          SourceTables.Add(CurrTable + '.FFD');
        end else begin
          writeln(format('*** ERROR: %s doesn''t exist ***', [CurrTable + '.FFD']));
          writeln;
          ExitCode := integer(cecInvalidTable);
          Exit;
        end;
      end;
    end;
  end else begin
    {add all tables in the source directory to the SourceTables list}

    {are there any tables in the source directory}
    if FindFirst(SourceDir + '\*.FFD', faAnyFile, ATable) = 0 then begin
      {yes. good, add each of them to the SourceTables list}
      SourceTables.Add(ATable.Name);
      while FindNext(ATable) = 0 do begin
        SourceTables.Add(ATable.Name);
      end;
    end else begin
      {no. Not good, we were expecting at least 1 FlashFiler table
       here.}
      ExitCode := integer(cecNoTables);
      Exit;
    end;
    SysUtils.FindClose(ATable);
  end;
end;
{--------}
function IsSameDatabase : boolean;
begin
  {ensure that we are not trying to put our new file in the same
   directory as the old file.}

  {Assumption: local paths - No UNCs}
  Result := UpperCase(Destination) = UpperCase(SourceDir);
end;
{--------}
function IsValidDest : boolean;
var
  i         : integer;
begin
  Result := False;
  {Does the command line contain a source parameter?}
  if (StrPos(CmdLine, '-d') <> nil) then begin
    {yes. we need to parse out the target directory name}
    for i := 1 to ParamCount do begin
      {if the second letter in the parameter is an d, it's a source}
      if IsDelimiter('d', ParamStr(i), 2) then begin
        Destination := ParamStr(i);
        {strip delimiter off string}
        Delete(Destination, 1, 2);
        {does the target file actually exist?}
        if DirectoryExists(Destination) then begin
          Result := True;
          {Remove the trailing "\" if it's there}
          if Destination[Length(Destination)] = '\' then
            Delete(Destination, Length(Destination), 1);
          {we're exiting if we get a valid target because there can
           only be a single destination}
          exit;
        end else begin
          writeln(format('*** ERROR: %s doesn''t exist ***', [Destination]));
          writeln;
          ExitCode := integer(cecInvalidDestination);
          Result := False;
          Exit;
        end;
      end; {if}
    end; {for}
  end else
    ExitCode := integer(cecNoDestination);
  if ((not Result) and (ExitCode = 0))  then
    ExitCode := integer(cecInvalidDestination);
end;
{--------}
function IsValidSource : Boolean;
var
  CurrParam   : string;
  i           : Integer;
  FirstSource : Boolean;
begin
  FirstSource := False;
  Result := False;
  {Does the command line contain a source parameter?}
  if (StrPos(CmdLine, '-s') <> nil) then begin
    {if so we need to parse out each source}
    for i := 1 to ParamCount do begin
      {if the second letter in the parameter is an s, it's a source}
      if IsDelimiter('s', ParamStr(i), 2) then begin
        {ensure only 1 source parameter is listed}
        if not FirstSource then
          FirstSource := True
        else begin
          ExitCode := integer(cecTooManySources);
          Exit;
        end;
        CurrParam := ParamStr(i);
        {strip delimiter off parameter}
        Delete(CurrParam, 1, 2);
        {does the source file actually exist?}
        if DirectoryExists(CurrParam) then begin
          SourceDir := CurrParam;
          if SourceDir[Length(SourceDir)] = '\' then
            Delete(SourceDir, Length(SourceDir), 1);
          Result := True;
        end else begin
          writeln(format('*** ERROR: %s doesn''t exist ***', [CurrParam]));
          writeln;
          Result := False;
          ExitCode := (integer(cecInvalidSource));
          Exit;
        end; {if..else}
      end; {if}
    end; {for}
  end else
    ExitCode := integer(cecNoSource);
  {if the source parameters aren't all valid and we haven't already
   set an exit code, we will set the ExitCode to 'Invalid Source'}
  if ((not Result) and (ExitCode = integer(cecNone))) then
    ExitCode := integer(cecInvalidSource);
end;
{--------}
procedure ShowExitCodeMessage;
begin
  case ExitCode of
    integer(cecNone):                  writeln('*** Conversion successful ***');
    integer(cecNoDestination):         writeln('*** ERROR: No destination parameter ***');
    integer(cecNoSource):              writeln('*** ERROR: No source parameters ***');
    integer(cecTooManySources):        writeln('*** ERROR: Too many source directories ***');
    integer(cecNoTables):              writeln('*** ERROR: No tables to convert ***');
    integer(cecDataConvertFailed):     writeln('*** ERROR: Conversion failed ***');
    integer(cecUnknownFailure):        writeln('*** ERROR: Unknown failure ***');
  end;
end;
{--------}
procedure DisplayStatus;
var
  ConsoleOutputHandle : THandle;
begin
  {reposition cursor}
  ConsoleOutputHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  SetConsoleCursorPosition(ConsoleOutputHandle, ScreenPos);

  Write(format('Table %d of %d - %d percent complete.',
               [Succ(CurrentTable),
                SourceTables.Count,
                ((TableConverter.RecordsProcessed * 100) div
                 TableConverter.TotalRecords)]));
end;
{--------}
procedure SetScreenPos;
var
  ConsoleOutputHandle : THandle;
  ScreenInfo          : TConsoleScreenBufferInfo;
begin
  { get screen pos}
  ConsoleOutputHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(ConsoleOutputHandle, ScreenInfo);
  ScreenPos.X := ScreenInfo.dwCursorPosition.X;
  ScreenPos.Y := ScreenInfo.dwCursorPosition.Y;
end;
{--------}
procedure ConvertTables;
var
  i          : integer;
begin
  {Ensure we are not overwriting any tables that the user doesn't want
   overwritten. If this isn't a problem, continue.}
  if ExitCode = 0 then begin
    if not WillOverwrite then begin
      CurrentTable := -1;
      for i := 0 to pred(SourceTables.Count) do begin
        inc(CurrentTable);
        {build the complete path to the table we're updating}
        {convert the table}
        try
          Write(SourceTables[i] + ' ');
          SetScreenPos;
          Write(format('Table %d of %d - 100 percent complete.',
                       [Succ(CurrentTable), SourceTables.Count]));
          TableConverter.Convert((SourceDir + '\' + SourceTables[i]), Destination);
          Writeln;
        except
          on E: Exception do begin
            writeln;
            writeln(format('*** ERROR: Conversion of %s failed ***' + #13#10 +
                           '*** %s ***', [SourceTables[i], E.Message]));
            ExitCode := integer(cecDataConvertFailed);
          end;
        end;
      end;
    end else
      ExitCode := integer(cecOverwrite);
  end;
end;
{--------}
procedure TFFConvUtil.OnProgress;
begin
  DisplayStatus;
end;
{--------}
procedure InitializeUnit;
begin
  ExitCode := 0;
  {startup our server engine}
  FF2Server := TffServerEngine.Create(nil);
  FF2Server.Configuration.GeneralInfo.giNoAutoSaveCfg := True;
  FF2Server.State := ffesStarted;
  {setup our table converter and its events}
  TableConverter := TffDataConverter.Create(FF2Server);
  TableConverter.ProgressFrequency := 100;
  {give ourself a 5 meg buffer for the FF2 server}
  TableConverter.BufferSize := 1024 * 1024;
  Utility := TFFConvUtil.Create;
  TableConverter.OnProgress := Utility.OnProgress;
  SourceTables := TStringList.Create;
end;
{--------}
procedure FinalizeUnit;
begin
  SourceTables.Free;
  TableConverter.Free;
  FF2Server.State := ffesShuttingDown;
  FF2Server.Free;
  Utility.Free;
end;
{====================================================================}
begin
  InitializeUnit;
  try
    if ParamCount > 0 then begin
      GoodSource := IsValidSource;
      if ExitCode = 0 then begin
        GoodDest := IsValidDest;
        if GoodSource and GoodDest and (ExitCode = 0) then begin
          BuildTableList;
          if ExitCode = 0 then
            ConvertTables;
          writeln;
        end else
          DisplayHelp;
      end;
    end else begin
      ExitCode := integer(cecNoParameters);
      DisplayHelp;
    end;
  finally
    FinalizeUnit;
    ShowExitCodeMessage;
  end;
{====================================================================}
end.
