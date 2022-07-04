{------------------------------------------------------------------------------
  Updater.exe
  (c) 2006, Kidmoses.com, http://www.kidmoses.com
  NOTE: must be compiled to Win32 platform and linked as GUI application.
------------------------------------------------------------------------------}
{$O+}         // Optimisation enabled
{$A-}         // Don't align data
{$H+}         // Huge strings (PChar) allowed
{$X+}         // Extended syntax allowed (discard results of functions)
{$D-}         // Debug information disabled
{$S-}         // Stack overflow checking disabled
program Updater;

uses Windows;

var
  exeFile: string = '';
  tmpFile: string = '';
  params1,params2: string;
  exSetup,launch,closing,okay: boolean;

  iFile: TextFile;
  sFile,dFile,eFile: string;

procedure WaitForTermination(waitApp:string);
var
  msg: TMsg;
  fHandle: THandle;
  iPriority: Integer;
begin
  iPriority := GetThreadPriority(GetCurrentThread);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_LOWEST);
  repeat
    Sleep(1);
    { 'Application.ProcessMessages' emulation }
    while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
    fHandle := CreateFile(PChar(waitApp),
                          GENERIC_WRITE, FILE_SHARE_WRITE, nil,
                          OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
   until fHandle <> INVALID_HANDLE_VALUE;
  SetThreadPriority(GetCurrentThread, iPriority);
  CloseHandle(fHandle);
end; { WaitForTermination }

{ ------------------------------------------ }

function ExecNewProcess(ProgramName:string;wait:boolean): dword;
var
  StartInfo : TStartupInfo;
  ProcInfo : TProcessInformation;
begin
  FillChar(StartInfo,SizeOf(TStartupInfo),#0);
  FillChar(ProcInfo,SizeOf(TProcessInformation),#0);
  StartInfo.cb := SizeOf(TStartupInfo);
  if CreateProcess(nil, PChar(ProgramName), nil, nil,False,
                   CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS,
                   nil, nil, StartInfo, ProcInfo) then begin
    if wait then begin
      WaitForSingleObject(ProcInfo.hProcess,INFINITE);
      GetExitCodeProcess(ProcInfo.hProcess,result);
      end
    else result := 0;
    end
  else result := 1;
  CloseHandle(ProcInfo.hThread);
  CloseHandle(ProcInfo.hProcess);
end; { ExecNewProcess }

{ ------------------------------------------------------------------------ }

{ main program starts here }
begin
  if ParamCount = 0 then begin
    MessageBox(0, PChar('Self-updating application for Net Update.'#13#10 +
                        '(c) 2004-2006, Kidmoses.com, All rights reserved.'#13#10 +
                        'http://www.kidmoses.com'),
                        'Updater v1.01', MB_OK or MB_ICONINFORMATION);
    Halt;
    end;

  { get parameters }
  exeFile := ParamStr(1);       { main app }
  tmpFile := ParamStr(2);       { info file or external setup file }
  exSetup := ParamStr(3)='1';   { do external setup }
  launch  := ParamStr(4)='1';   { launch main app }
  closing := ParamStr(5)='1';   { wait for main app to close }
  params1 := Copy(ParamStr(6),2,Length(ParamStr(6))-1); { remove + sign }
  params2 := Copy(ParamStr(7),2,Length(ParamStr(7))-1); { remove + sign }

  if closing then WaitForTermination(exeFile);        { wait for main app to finish }

  try
  AssignFile(iFile,tmpFile);                          { open info file }
  Reset(iFile);                                       { for reading }
  ReadLn(iFile,sFile);                                { skip 1st line - update version }
  ReadLn(iFile,dFile);                                { skip 2nd line - update date }
  if exSetup then begin                               { use external setup file }
    ReadLn(iFile,eFile);                              { set file name }
    eFile := eFile + ' ' + params2;                   { is external setup file }
    okay := ExecNewProcess(eFile,True) = 0;;          { launch external file and wait }
    end
  else begin                                          { else copy files }
    while not Eof(iFile) do begin
      ReadLn(iFile,sFile);                            { get source file }
      ReadLn(iFile,dFile);                            { get destination file }
      CopyFile(PChar(sFile),PChar(dFile),false);      { copy over }
      DeleteFile(PChar(sFile));                       { delete source file... no longer needed }
      end;
    okay := true;                                     { everything copied okay }
    end;
  except
    okay := false;                                    { something went wrong somewhere }
  end;

  if okay then begin                                  { if everything successful }
    CloseFile(iFile);                                 { close info file }
    Append(iFile);                                    { reopen for write access }
    Writeln(iFile,'Completed');                       { mark completed }
    end;
  CloseFile(iFile);                                   { and close info file for good }

  if launch then
    ExecNewProcess(exeFile + ' ' + params1,false);    { launch main app but don't wait }
end.

