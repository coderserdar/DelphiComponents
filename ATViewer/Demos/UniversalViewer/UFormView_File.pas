//------------------------
procedure MsgCopyMoveError(const OldName, NewName: WideString);
begin
  MsgError(SFormatW(MsgViewerCopyMoveError, [SExtractFileName(OldName), NewName]));
end;

procedure MsgDeleteError(const OldName: WideString);
begin
  MsgError(SFormatW(MsgViewerDeleteError, [SExtractFileName(OldName)]));
end;

//------------------------
function FExecShellA(const cmd, params, dir: AnsiString; ShowCmd: integer; fWait: boolean): boolean;
var
  si: TShellExecuteInfoA;
begin
  FillChar(si, SizeOf(si), 0);
  si.cbSize := SizeOf(si);
  si.fMask := SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  si.lpFile := PAnsiChar(cmd);
  si.lpParameters := PAnsiChar(params);
  si.lpDirectory := PAnsiChar(dir);
  si.nShow := ShowCmd;
  Result := ShellExecuteExA(@si);
  if Result then
    if fWait then
      WaitForSingleObject(si.hProcess, INFINITE);
end;

function FExecShellW(const cmd, params, dir: WideString; ShowCmd: integer; fWait: boolean): boolean;
var
  si: TShellExecuteInfoW;
begin
  FillChar(si, SizeOf(si), 0);
  si.cbSize := SizeOf(si);
  si.fMask := SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  si.lpFile := PWChar(cmd);
  si.lpParameters := PWChar(params);
  si.lpDirectory := PWChar(dir);
  si.nShow := ShowCmd;
  Result := ShellExecuteExW(@si);
  if Result then
    if fWait then
      WaitForSingleObject(si.hProcess, INFINITE);
end;

function FExecShell(const cmd, params, dir: WideString; ShowCmd: integer; fWait: boolean): boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := FExecShellW(cmd, params, dir, ShowCmd, fWait)
  else
    Result := FExecShellA(cmd, params, dir, ShowCmd, fWait);
end;

//------------------------
{$ifdef CMDLINE}
procedure NavOp(const Op: string;
  const S1, S2: WideString;
  const S3: WideString = ''; const S4: WideString = ''; fWait: boolean = True);
var
  S: WideString;
begin
  S := SParamDir + '\Nav.exe';
  if IsFileExist(S) then
    FExecShell(S, SFormatW('op%s "%s" "%s" "%s" "%s"', [Op, S1, S2, S3, S4]), '', SW_SHOW, fWait)
  else
    MsgError(MsgViewerNavMissed);
end;
{$endif}

//------------------------
procedure TFormViewUV.DoFileRename;
var
  OldName, NewName,
  NewNameFull: WideString;
begin
  OldName:= FFileName;
  NewName:= '';

  with TFormViewRename.Create(nil) do
    try
      edFilename.Text:= SExtractFileName(OldName);
      if ShowModal = mrOk then
        NewName:= edFilename.Text;
    finally
      Release;
    end;

  if NewName <> '' then
    begin
    CloseFile;

    //Wait for PDF
    repeat Sleep(FFileMoveDelay) until not Viewer.WebBusy;

    NewNameFull:= SExtractFilePath(OldName) + NewName;

    {$ifdef CMDLINE}
    NavOp('move', OldName, NewNameFull);
    {$endif}
    if IsFileExist(NewNameFull) then
      begin
      LoadFile(NewNameFull);
      end
    else
      begin
      MsgCopyMoveError(OldName, NewName);
      LoadFile(OldName);
      end;
    end;
end;

//---------------------------------------------------------------
procedure TFormViewUV.DoFileCopy;
var
  OldName, NewName, DestDir: WideString;
begin
  DestDir:= WideExtractFileDir(FFileName);
  if WideSelectDirectory(mnuFileCopy.Caption, '', DestDir) then
    begin
    OldName:= FFileName;
    NewName:= DestDir + '\' + WideExtractFileName(OldName);

    {$ifdef CMDLINE}
    NavOp('copy', OldName, NewName);
    {$endif}
    if not IsFileExist(NewName) then
      MsgCopyMoveError(OldName, NewName);
    end;
end;

//---------------------------------------------------------------
procedure TFormViewUV.DoFileMove;
var
  OldName, NewName,
  NextName, DestDir: WideString;
begin
  DestDir:= WideExtractFileDir(FFileName);
  if WideSelectDirectory(mnuFileMove.Caption, '', DestDir) then
    begin
    OldName:= FFileName;
    NewName:= DestDir + '\' + WideExtractFileName(OldName);

    NextName:= FFileList.GetNext(OldName, nfNext);
    if SCompareIW(NextName, OldName) = 0 then
      NextName:= '';

    CloseFile;

    //Wait for PDF
    repeat Sleep(FFileMoveDelay) until not Viewer.WebBusy;

    {$ifdef CMDLINE}
    NavOp('move', OldName, NewName);
    {$endif}
    if IsFileExist(NewName) then
      begin
      LoadFile(NextName);
      end
    else
      begin
      MsgCopyMoveError(OldName, NewName);
      LoadFile(OldName);
      end;
    end;
end;
