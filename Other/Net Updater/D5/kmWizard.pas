unit kmWizard;

interface

uses
  Windows,Classes,Controls,ComCtrls,nuServer,Registry,
  ExtCtrls,StdCtrls,Graphics,Forms,Messages,SysUtils,
  ShellAPI,kmUtils,kmWinInet,kmTypes;

const
  WM_AFTER_CREATE = WM_USER + 301; { custom message }

type
  TnuState = (sDownloading,sInstalling);

  TfmWizard = class(TForm)
    Bevel1: TBevel;
    CancelBtn: TButton;
    Image1: TImage;
    lblDownloadFile: TLabel;
    lblFileName: TLabel;
    lblFileStatus: TEdit;
    ProgressCurrentFile: TProgressBar;
    CheckTimer: TTimer;
    procedure AboutClick(Sender:TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblFileStatusEnter(Sender: TObject);
    procedure OnCheckTimer(Sender: TObject);
  protected
  private
    FCurDLFile: string;
    FCurDLFileNo: integer;
    isChecking: Boolean;
    isTimer: boolean;
    lblLocation: TellLabel;
    nuServer1: TnuServer;
    function CheckUpdateCompleted():boolean;
    function ConfirmUpdate():boolean;
    function DownloadCompleted():boolean;
    function GetDestDirectory(dDir:TnuTargetDir):string;
    function RedirectToURL():boolean;
    function SelfUpdate():boolean;
    function UpdateDIR():boolean;
    function UpdateURL():boolean;
    function UseExternalSetup():boolean;
    procedure CheckUpdate();
    procedure CleanUpAfterCancel();
    procedure GetVersionReg();
    procedure InitializeStatusWindow(sFile:string);
    procedure LaunchUpdater(const f1,f2,x1,x2,x3,p1,p2: String);
    procedure MarkUpdateCompleted(isComplete:Boolean;nvDate,nvNumber:string);
    procedure SetCheckTimer();
    procedure SetClientDownloadParams();
    procedure SetDownloadParams();
    procedure SetServerDownloadParams();
    procedure SetVersionReg(nvDate,nvNumber:string);
  public
    dirPath: string;
    procedure ProcessAgent();
  end;

var
  fmWizard: TfmWizard;
  dlState: TnuState;
  UpgraderFileName: String; { updater.exe }

implementation

uses kmNetUpdate;

{$R *.DFM}
{$R Updater.RES}

procedure FastFileCopyCallBack(Position,Size,sTime:Longint);
var
  speed:double;
  eTime,elapsedTime,remainTime: longint;
  eMinutes,eSeconds,rMinutes,rSeconds: longint;
begin
  eTime := timegettime();
  elapsedTime := eTime - sTime + 1;
  eMinutes := trunc(elapsedTime/60000);
  eSeconds := trunc(frac(elapsedTime/60000)*60);
  speed := Position/elapsedTime;
  remainTime := trunc((Size - Position)/(speed));
  rMinutes := trunc(remainTime/60000);
  rSeconds := trunc(frac(remainTime/60000)*60);

  fmWizard.ProgressCurrentFile.Max := Size;
  fmWizard.ProgressCurrentFile.Position := Position;

  if dlState = sDownloading then
    fmWizard.lblDownloadFile.Caption := Format(nuClient1.MsgStrs[10],
                                        [fmWizard.FCurDLFileNo,fmWizard.nuServer1.ServerFiles.Count])
  else
    fmWizard.lblDownloadFile.Caption := Format(nuClient1.MsgStrs[11],
                                        [fmWizard.FCurDLFileNo,fmWizard.nuServer1.ServerFiles.Count]);

  fmWizard.lblFileStatus.Text := Format(nuClient1.MsgStrs[9],
                                       [eMinutes,eSeconds,rMinutes,rSeconds,Position/1024,Size/1024]);

  Application.ProcessMessages;
end; { FastFileCopyCallBack }

{ ----------------------------------------------------------------- }

procedure TfmWizard.FormCreate(Sender: TObject);
begin
  nuServer1 := TnuServer.Create(self);
  lblLocation := TellLabel.Create(self);
  lblLocation.Parent := self;
  lblLocation.AutoSize := false;
  lblLocation.EllipseType := etPathEllipse;
  lblLocation.Left := 64;
  lblLocation.Top := 80;
  lblLocation.Height := 13;
  lblLocation.Width := 364;
  isTimer := false;
  isChecking := false;
  dirPath := ExtractFilePath(Application.ExeName);
  FCurDLFile := '';
  FCurDLFileNo := 0;
  AboutClick(self);
  CheckTimer.Enabled := true;
end; { FormCreate }

{ ----------------------------------------------------------------- }

procedure TfmWizard.FormDestroy(Sender: TObject);
begin
  lblLocation.Free;
  nuServer1.Free;
end; { FormDestroy }

{ ----------------------------------------------------------------- }

procedure TfmWizard.SetDownloadParams();
begin
  AppHandle := Application.Handle;
  INetLocalFolder := GetTempDir();  { nuWinInet }
  LogFile := dirPath + 'nu.log';    { nuWinInet }
  DoLog := nuClient1.LogFile;       { nuWinInet }
  if nuClient1.RunMode <> rmHide then begin
    if nuClient1.StayOnTop then
      FormStyle := fsStayOnTop else
      FormStyle := fsNormal;
    if nuClient1.RunMode = rmSilent then
      CancelBtn.Enabled := false else
      CancelBtn.Enabled := true;
    end;
  CancelBtn.Caption := nuClient1.MsgStrs[18];
  case nuClient1.ThreadPriority of
    tpLow:    SetPriorityClass(GetCurrentProcess,IDLE_PRIORITY_CLASS);
    tpNormal: SetPriorityClass(GetCurrentProcess,NORMAL_PRIORITY_CLASS);
    tpHigh:   SetPriorityClass(GetCurrentProcess,HIGH_PRIORITY_CLASS);
  end;
end; { SetDownloadParams }

{ ----------------------------------------------------------------- }

procedure TfmWizard.SetCheckTimer();
begin
  if nuClient1.Schedule = schStart then
    CheckTimer.Interval := nuClient1.AutoCheckDelay
  else CheckTimer.Interval := 60000;
end; { SetCheckTimer }

{ ----------------------------------------------------------------- }

procedure TfmWizard.OnCheckTimer(Sender: TObject);
var
  h1,h2,m1,m2,s,l:word;
begin
  if IsTimer then exit;
  if not nuClient1.Active then exit;
  IsTimer := true;
  if CheckTimer.Interval = 250 then begin    { don't change timer internal directly }
    CheckTimer.Enabled := false;             { need to do some checks first }
    if not CheckUpdateCompleted() then GetVersionReg();
    SetCheckTimer();
    DeleteFile(UpgraderFileName);    { delete self upgrade file from previous update }
    DeleteFile(dirPath + 'nu.log');  { delete log file & update completed file }
    DeleteFile(GetTempDir() + ChangeFileExt(nuClient1.URLFile,'.nui'));
    if nuClient1.Schedule <> schNone then
      CheckTimer.Enabled := true;
    IsTimer := false;
    Exit;
    end;
  if nuClient1.Schedule <> schStart then begin
    DecodeTime(Now(),h1,m1,s,l);
    DecodeTime(nuClient1.FCheckTime,h2,m2,s,l);
    if (h1<>h2) or (m1<>m2) then exit;
    if nuClient1.Schedule = schWeek then begin
      if nuClient1.CheckDay <> TnuDay(DayOfWeek(Now())-1) then exit;
      end;
    end;

  CheckTimer.Enabled := false;
  if nuClient1.Schedule <> schNone then ProcessAgent(); { could get here if active }
  if nuClient1.Schedule <> schStart then                { is set at runtime }
    CheckTimer.Enabled := true;
  IsTimer := false;
end; { OnCheckTimer }

{ ----------------------------------------------------------------- }

procedure TfmWizard.SetClientDownloadParams();
{ updates variables in the nuWinInet unit }
begin
  AccessType := nuClient1.AccessType;
  RunMode := nuClient1.RunMode;
  AskCancel := nuClient1.WarnOnCancel;
  INetCancelCopy := false;
  INetBusy := false;
  INetLocalFolder := GetTempDir();
  INetPort := nuClient1.Port;
  INetProxyBypass := nuClient1.ProxyBypass;
  INetProxyPassword := EncryptDecrypt(nuClient1.ProxyPassword);
  INetProxyPort := IntToStr(nuClient1.ProxyPort);
  INetProxyServer := nuClient1.ProxyServer;
  INetProxyUsername := nuClient1.ProxyUsername;
  INetUsername := nuClient1.UserName;
  INetPassword := EncryptDecrypt(nuClient1.Password);

  {cache settings}
  ForceReload := (coAlwaysReload in nuClient1.CacheOptions);
  HyperLink := (coReloadIfNoExpireInformation in nuClient1.CacheOptions);
  Resynchronize := (coReloadUpdatedObjects in nuClient1.CacheOptions);
  Pragma := (coPragmaNoCache in nuClient1.CacheOptions);
  NoCacheWrite := (coNoCacheWrite in nuClient1.CacheOptions);
  CreateTemp := (coCreateTempFilesIfCantCache in nuClient1.CacheOptions);
  CacheIfNetFail := (coUseCacheIfNetFail in nuClient1.CacheOptions);

  {internet settings }
  IgnoreInvalidCert := (ioIgnoreCertificateInvalid in nuClient1.InternetOptions);
  IgnoreInvalidCertDate := (ioIgnoreCertificateDateInvalid in nuClient1.InternetOptions);
  IgnoreRedirectHttp := (ioIgnoreRedirectToHTTP in nuClient1.InternetOptions);
  IgnoreRedirectHttps := (ioIgnoreRedirectToHTTPS in nuClient1.InternetOptions);
  KeepConnection := (ioKeepConnection in nuClient1.InternetOptions);
  NoAuthentication := (ioNoAuthentication in nuClient1.InternetOptions);
  NoAutoRedirect := (ioNoAutoRedirect in nuClient1.InternetOptions);
  NoCookies := (ioNoCookies in nuClient1.InternetOptions);
  NoCookieDialog := (ioNoCookieDialog in nuClient1.InternetOptions);
  SecureTransaction := (ioSecure in nuClient1.InternetOptions);

  {miscellaneous}
  NotifyConnectionLost := (mConnLost in nuClient1.ShowMessages);
  NotifyHostUnreachable := (mHostUnreachable in nuClient1.ShowMessages);
  Notify404Error := (mNoFile in nuClient1.ShowMessages);
  NotifyPasswordNeeded := (mPasswordRequest in nuClient1.ShowMessages);
  ThreadPriority := nuClient1.ThreadPriority;

  {messages}
  MsgTitle := nuClient1.MsgStrs[0];
  MsgHostUnreachable := nuClient1.MsgStrs[31];
  MsgFileNotFound := nuClient1.MsgStrs[33];
  MsgConnectionLost := nuClient1.MsgStrs[34];
  MsgCancelUpdate := nuClient1.MsgStrs[32];

  FMsgTitle := nuClient1.MsgStrs[0];
  FCancelMsg := nuClient1.MsgStrs[32];
  
  URLSize := 0;
end; { SetClientDownloadParams }

{ ----------------------------------------------------------------- }

procedure TfmWizard.SetServerDownloadParams();
begin
  INetCancelCopy := false;
  INetBusy := false;
  INetLocalFolder := GetTempDir();
  INetPassword := EncryptDecrypt(nuServer1.Password);
  INetPort := nuServer1.Port;
  INetUsername := nuServer1.UserID;
  ProgressCurrentFile.Position := 0;
end; { SetServerDownloadParams }

{ ----------------------------------------------------------------- }

procedure TfmWizard.ProcessAgent();
var
  fileSource,fileDest: string;
begin
  if not nuClient1.Active then exit;
  if Assigned(nuClient1.OnCheck) then nuClient1.OnCheck(self);
  isChecking := true;
  SetDownloadParams();
  WriteLog(Format('Update check started at: %s',[FormatDateTime('c',now)]));
  WriteLog('Thread priority set to ' + IntToStr(integer(nuClient1.ThreadPriority)));
  WriteLog(Format('Check for update at: %s',[FormatDateTime('c',now)]));
  if nuClient1.URLProtocol = pFILE then begin
    with nuClient1 do begin
      fileSource := trim(URLPath + URLFile);
      fileDest := trim(GetTempDir + URLFile);
      if not FileExists(fileSource) then begin
        WriteLog(Format('Error: cannot read file for downloading.'#13#10'%s',[fileSource]));
        exit;
        end;
      CopyFile(PChar(fileSource),PChar(fileDest),false);
      if FileExists(fileDest) then begin
        nuServer1.Load(fileDest);
        WriteLog(Format('Update file downloaded successfully: %s',[URLFile]));
        CheckUpdate();
        DeleteFile(fileDest);
        end
      else WriteLog(Format('Update file not found: %s. Error copying file.',[URLFile]));
      end;
    end
  else begin
    with nuClient1 do begin
      if not IsOnline then begin
        WriteLog('Error: no Internet connection.');
        exit;
        end;
      fileSource := trim(FURLPrefix + URLPath + URLFile);
      fileDest := trim(GetTempDir + URLFile);
      SetClientDownloadParams();
      Pragma := true; { force reload from internet server... not cache }
      if INetStart(fileSource,fileDest,@FastFileCopyCallBack) then begin
        if FileExists(fileDest) then begin
          nuServer1.Load(fileDest);
          WriteLog(Format('Update file downloaded successfully: %s',[URLFile]));
          CheckUpdate();
          DeleteFile(fileDest);
          end
        else WriteLog(Format('Update file not found: %s. Error downloading file.',[URLFile]));
        end;
      end;
    end;
  isChecking := false;
end; { ProcessAgent }

{ ----------------------------------------------------------------- }

procedure TfmWizard.CheckUpdate();
var
  vc: integer;
begin
  if nuClient1.VersionControl = byVersion then
    vc := VersionCheck(nuServer1.VersionNumber,nuClient1.VersionNumber) else
    vc := VersionCheck(nuServer1.VersionDate,nuClient1.VersionDate);

  if vc = 1 then begin
    if nuServer1.Protocol = 'file://' then
      UpdateDIR() else
      UpdateURL();
    CheckUpdateCompleted();  { may not make it this far if CloseEXE is true }
    end
  else begin
    if (mNoUpdateAvailable in nuClient1.ShowMessages) then begin
      MessageBeep(MB_ICONASTERISK);
      MessageBox(Handle,PChar(nuClient1.MsgStrs[27]),PChar(nuClient1.MsgStrs[0]),MB_OK);
      end;
    end;
end; { CheckUpdate }

{ ----------------------------------------------------------------- }

function TfmWizard.CheckUpdateCompleted():boolean;
var iFile: TextFile;
    tFile,newVerDate,newVerNumber,tmpStr: string;
    isComplete: boolean;
begin
  if Assigned(nuClient1.OnCheckCompleted) then nuClient1.OnCheckCompleted(Self);
  Result := false;
  isComplete := false;
  newVerDate := '';
  newVerNumber := '';
  tFile := GetTempDir() + ChangeFileExt(nuClient1.URLFile,'.nui');
  if FileExists(tFile) then begin
    AssignFile(iFile,tFile);
    try
    Reset(iFile);
    ReadLn(iFile,newVerNumber);           { get version number from 1st line }
    ReadLn(iFile,newVerDate);             { get version date from 2nd line }
    while not Eof(iFile) do begin         { now look for completed }
      ReadLn(iFile,tmpStr);
      if tmpStr = 'Completed' then
        isComplete := True;
      end;
    finally
    CloseFile(iFile);
    end;
    if Assigned(nuClient1.OnCompleted) then nuClient1.OnCompleted(Self);
    if (newVerDate <> '') and (newVerNumber <> '') then begin
      MarkUpdateCompleted(isComplete,newVerDate,newVerNumber);
      result := true;
      end;
  end;
end; { CheckUpdateCompleted }

{ ----------------------------------------------------------------- }

procedure TfmWizard.MarkUpdateCompleted(isComplete:Boolean;nvDate,nvNumber:string);
{ Marks the update completed so it doesn't fire again }
begin
  if isComplete or not (mPromptCancel in nuClient1.ShowMessages) then begin
    SetVersionReg(nvDate,nvNumber);
    nuClient1.VersionNumber := nvNumber;
    nuClient1.VersionDate := nvDate;
    end;
end; { CheckUpdate }

{ ----------------------------------------------------------------- }

procedure TfmWizard.SetVersionReg(nvDate,nvNumber:string);
var
  p: integer;
  reg : TRegIniFile;
  sKey,appTitle,urlFile: string;
begin
  if Assigned(nuClient1.OnSetRegistry) then nuClient1.OnSetRegistry(Self,nvDate,nvNumber);
  if not nuClient1.UseRegistry then Exit;
  reg := nil;
  appTitle := Application.Title;
  p := Pos('-',appTitle);
  if p > 0 then appTitle := Trim(Copy(appTitle,1,p-1));
  urlFile := Copy(nuClient1.URLFile,0,Pos('.',nuClient1.URLFile)-1);
  try
  sKey := '\SOFTWARE\' + appTitle + '\NetUpdate\' + urlFile +'\';
  reg := TRegIniFile.Create('');
  reg.RootKey := HKEY_LOCAL_MACHINE;
  reg.WriteString(sKey,'InstallDate',nuClient1.VersionDate);
  reg.WriteString(sKey,'InstallVersion',nuClient1.VersionNumber);
  reg.WriteString(sKey,'CheckedDate',nvDate);
  reg.WriteString(sKey,'CheckedVersion',nvNumber);
  except { use Try Except so that if value cannot be placed in registry }
  end;   { we do not get an error }
  reg.Free;
end; { SetVersionReg }

{ ----------------------------------------------------------------- }

procedure TfmWizard.GetVersionReg();
var
  p: integer;
  reg : TRegIniFile;
  sKey,appTitle,urlFile: string;
begin
  if not nuClient1.UseRegistry then Exit;
  reg := nil;
  appTitle := Application.Title;
  p := Pos('-',appTitle);
  if p > 0 then appTitle := Trim(Copy(appTitle,1,p-1));
  urlFile := Copy(nuClient1.URLFile,0,Pos('.',nuClient1.URLFile)-1);
  try
  sKey := '\SOFTWARE\' + appTitle + '\NetUpdate\' + urlFile +'\';
  reg := TRegIniFile.Create('');
  reg.RootKey := HKEY_LOCAL_MACHINE;
  { don't load if not set up or different installed version }
  if reg.ReadString(sKey,'InstallDate','') = nuClient1.VersionDate then begin
    if reg.ReadString(sKey,'InstallVersion','') = nuClient1.VersionNumber then begin
      nuClient1.VersionDate := reg.ReadString(sKey,'CheckedDate',nuClient1.VersionDate);
      nuClient1.VersionNumber := reg.ReadString(sKey,'CheckedVersion',nuClient1.VersionNumber);
    end;
  end;
  except { use Try Except so that if value cannot be placed in registry }
  end;   { we do not get an error }
  reg.Free;
end; { SetVersionReg }

{ ----------------------------------------------------------------- }

procedure TfmWizard.InitializeStatusWindow(sFile:string);
begin
  ProgressCurrentFile.Max := 100;
  ProgressCurrentFile.Position := 0;

  lblDownloadFile.Caption := nuClient1.MsgStrs[8];
  lblFileName.Caption := Format(nuClient1.MsgStrs[7],[FCurDLFile]);
  lblFileStatus.Text := '';
  if nuClient1.HideFileLocation then
    lblLocation.Caption := '' else begin
    lblLocation.Caption := Format(nuClient1.MsgStrs[6],[sFile]);
    end;
  Application.ProcessMessages;
end; { InitializeStatusWindow }

{ ----------------------------------------------------------------- }

function TfmWizard.UpdateURL():boolean;
var
  i: integer;
  fileSource,fileDest: string;
begin
  result := false;
  if not ConfirmUpdate() then exit;
  if nuServer1.UpgradeMethod = umRedirectToURL then begin
    WriteLog(Format('Redirecting to URL: %s.',[nuServer1.Protocol + nuServer1.FilePath]));
    result := RedirectToURL();
    if result then
      WriteLog('Redirection to URL successful.') else
      WriteLog('Redirection to URL failed.');
    exit;
    end;

  FCancelCopy := false;
  if nuClient1.RunMode <> rmHide then Show();
  Application.ProcessMessages;

  SetServerDownloadParams();
  dlState := sDownloading;
  for i := 0 to nuServer1.ServerFiles.Count - 1 do begin
    fileSource := trim(nuServer1.Protocol + nuServer1.FilePath + nuServer1.ServerFiles[i].FileName);
    fileDest := GetTempDir + nuServer1.ServerFiles[i].FileName;
    WriteLog(Format('Downloading: %s to %s',[fileSource,fileDest]));
    FCurDLFile := nuServer1.ServerFiles[i].FileName;
    FCurDLFileNo := i+1;

    InitializeStatusWindow(fileSource);
    URLSize := nuServer1.ServerFiles[i].FileSize;

    Pragma := (coPragmaNoCache in nuClient1.CacheOptions);  { reset to user selection }
    if not INetStart(fileSource,fileDest,@FastFileCopyCallBack) then begin
      WriteLog('Update did not complete. Stopped at file:'+#13#10+fileDest);
      if INetCancelCopy then
        WriteLog('Update cancelled by user.');
      CleanUpAfterCancel();
      exit;
      end;
    end;

  if Visible then Hide();
  if DownloadCompleted() then begin
    if nuClient1.CloseEXE and nuClient1.WarnOnRestart then begin
      MessageBeep(MB_ICONASTERISK);
      if MessageBox(Handle,PChar(nuClient1.MsgStrs[12]),PChar(nuClient1.MsgStrs[0]),MB_OKCANCEL or MB_ICONINFORMATION) = IDCANCEL then begin
        WriteLog('Restart of application cancelled by user. Update cancelled.');
        CleanUpAfterCancel();
        exit;
        end;
      end;

    case nuServer1.UpgradeMethod of
      umSelfUpgrade:      Result := SelfUpdate();
      umUseExternalSetup: Result := UseExternalSetup();
      end;
    { never gets here if nuClient.CloseEXE is true and update is successfull }
    end;
end; { UpdateURL }

{ ----------------------------------------------------------------- }

function TfmWizard.UpdateDIR():boolean;
var
  i: integer;
  fileSource,fileDest: string;
begin
  result := false;
  if not ConfirmUpdate() then exit;
  FCancelCopy := false;
  if nuClient1.RunMode <> rmHide then Show();
  for i := 0 to nuServer1.ServerFiles.Count - 1 do begin
    fileSource := trim(nuServer1.FilePath + nuServer1.ServerFiles[i].FileName);
    fileDest := GetTempDir + nuServer1.ServerFiles[i].FileName;
    WriteLog(Format('Downloading: %s to %s',[fileSource,fileDest]));
    FCurDLFile := nuServer1.ServerFiles[i].FileName;
    FCurDLFileNo := i+1;

    InitializeStatusWindow(fileSource);

    if FileExists(fileSource) then
      FastFileCopy(fileSource,fileDest,@FastFileCopyCallBack,nuClient1.WarnOnCancel)
    else begin
      WriteLog(Format(nuClient1.MsgStrs[28],[fileSource])); { Update cancelled. File does not exist. }
      if nuClient1.RunMode = rmNormal then begin
        if mNoFile in nuClient1.ShowMessages then begin
          MessageBeep(MB_ICONASTERISK);
          MessageBox(Handle,PChar(Format(nuClient1.MsgStrs[28],[fileSource])),PChar(nuClient1.MsgStrs[0]),MB_OK);
          CleanUpAfterCancel();
          exit;
          end;
        end;
      end;

    if FCancelCopy then begin
      WriteLog('Update cancelled by user.');
      CleanUpAfterCancel();
      exit;
      end;
    end;

  if Visible then Hide();
  if DownloadCompleted() then begin
    if nuClient1.CloseEXE and nuClient1.WarnOnRestart then begin
      MessageBeep(MB_ICONASTERISK);
      if MessageBox(Handle,PChar(nuClient1.MsgStrs[12]),PChar(nuClient1.MsgStrs[0]),MB_OKCANCEL or MB_ICONINFORMATION) = IDCANCEL then begin
        WriteLog('Restart of application cancelled by user. Update cancelled.');
        CleanUpAfterCancel();
        exit;
        end;
      end;

    case nuServer1.UpgradeMethod of
      umSelfUpgrade:      Result := SelfUpdate();
      umUseExternalSetup: Result := UseExternalSetup();
      end;
    { never gets here if nuClient.CloseEXE is true and update is successfull }
    end;
end; { UpdateDIR }

{ ----------------------------------------------------------------- }

function TfmWizard.ConfirmUpdate():boolean;
var
  s: string;
begin
  result := true;
  WriteLog('New version found.');
  if nuClient1.RunMode = rmNormal then begin
    if mAskUpgrade in nuClient1.ShowMessages then begin
      s := Format(nuClient1.MsgStrs[25],[Application.MainForm.Caption]);
      s := s + #13 + nuServer1.UpgradeMsg + #13 + nuClient1.MsgStrs[26];
      MessageBeep(MB_ICONASTERISK);
      if MessageBox(Handle,PChar(s),PChar(nuClient1.MsgStrs[0]),MB_YESNO or MB_ICONINFORMATION) = IDNO then begin
        s := nuClient1.URLFile;
        WriteLog(Format('Update %s cancelled by user.',[s]));
        result := false;
        exit;
        end;
      end;
    end;
end; { ConfirmUpdate }

{ ----------------------------------------------------------------- }

function TfmWizard.SelfUpdate():boolean;
var
  i:integer;
  fileSource,fileDest,fileBackup: string;
  iFile: TextFile;
  tFile,lApp,wait: string;
begin
  result := true;
  lApp := '0';
  wait := '0';
  dlState := sInstalling;
  tFile := GetTempDir() + ChangeFileExt(nuClient1.URLFile,'.nui');
  WriteLog('Creating installation information file: ' + tFile);
  AssignFile(iFile,tFile);
  try
  ReWrite(iFile);
  Writeln(iFile,nuServer1.VersionNumber);
  Writeln(iFile,nuServer1.VersionDate);
  for i := 0 to nuServer1.ServerFiles.Count - 1 do begin
    fileSource := ConvertToLocalPath(GetTempDir + nuServer1.ServerFiles[i].FileName);
    fileDest := ConvertToLocalPath(GetDestDirectory(nuServer1.ServerFiles[i].TargetDir) +
                                   IncludeTrailingBackslash(nuServer1.ServerFiles[i].SubDir) +
                                   nuServer1.ServerFiles[i].FileName);

    if nuClient1.CreateBackup then begin
      fileBackup := ConvertToLocalPath(dirPath + 'backup\' +
                                       IncludeTrailingBackslash(nuServer1.ServerFiles[i].SubDir) +
                                       nuServer1.ServerFiles[i].FileName);
      if FileExists(fileDest) then begin
        if not (DirectoryExists(ExtractFilePath(fileBackup))) then
          CreatePath(ExtractFilePath(fileBackup));
        WriteLog('Backing up file: ' + fileDest);
        CopyFile(PChar(fileDest),PChar(fileBackup),false);
        end;
      end;

    { force new directory if it does not exist }
    if not DirectoryExists(ExtractFilePath(fileDest)) then
      CreatePath(ExtractFilePath(fileDest));

    Writeln(iFile,fileSource);
    Writeln(iFile,fileDest);
    end;
  except
    result := false;
  end;

  Closefile(iFile);

  if not result then Exit;
  WriteLog('Installation information file created successfully.');
  WriteLog('Launching updater.');
  if nuClient1.CloseEXE then WriteLog('Closing application.');

  if nuClient1.LaunchApp then lApp := '1';
  if nuClient1.CloseEXE then wait := '1';
  LaunchUpdater(Application.ExeName,tFile,'0',lApp,wait,nuClient1.LaunchParams,nuServer1.ExtractParams);
  if nuClient1.CloseEXE then PostMessage(handle,WM_QUIT,0,0);
end; { SelfUpdate }

{ ----------------------------------------------------------------- }

function TfmWizard.UseExternalSetup():boolean;
var
  iFile: TextFile;
  fileSource,tFile,lApp,wait,s: string;
begin
  result := true;
  lApp := '0';
  wait := '0';
  fileSource := GetTempDir + nuServer1.ExeFile;
  if not FileExists(fileSource) then begin
    s := Format(nuClient1.MsgStrs[29],[fileSource]); { lost file }
    WriteLog(s);
    if nuClient1.RunMode = rmNormal then begin
      if mNoFile in nuClient1.ShowMessages then begin
        MessageBeep(MB_ICONASTERISK);
        MessageBox(Handle,PChar(s),PChar(nuClient1.MsgStrs[0]),MB_OK);
        CleanUpAfterCancel();
        Result := False;
        exit;
        end;
      end;
    end;
  WriteLog('External setup file downloaded successfully.');

  tFile := GetTempDir() + ChangeFileExt(nuClient1.URLFile,'.nui');
  WriteLog('Creating installation information file: ' + tFile);
  try
  AssignFile(iFile,tFile);
  ReWrite(iFile);
  Writeln(iFile,nuServer1.VersionNumber);
  Writeln(iFile,nuServer1.VersionDate);
  Writeln(iFile,fileSource);
  except
    Result := False;
  end;
  CloseFile(iFile);

  if not result then Exit;
  WriteLog('Installation information file created successfully.');
  WriteLog('Launching setup file / updater.');
  if nuClient1.CloseEXE then WriteLog('Closing application.');

  if nuClient1.LaunchApp then lApp := '1';
  if nuClient1.CloseEXE then wait := '1';
  LaunchUpdater(Application.ExeName,tFile,'1',lApp,wait,nuClient1.LaunchParams,nuServer1.ExtractParams);
  if nuClient1.CloseEXE then PostMessage(handle,WM_QUIT,0,0);
  result := true;
end; { UseExternalSetup }

{ ----------------------------------------------------------------- }

function TfmWizard.RedirectToURL():boolean;
begin
  if ShellExecute(Handle,'open',PChar(nuServer1.FilePath),nil,nil,SW_SHOWNORMAL) <= 32 then
    result := false else
    result := true;
end; { RedirectToURL }

{ ----------------------------------------------------------------- }

function TfmWizard.DownloadCompleted():boolean;
{ check to see that all the files downloaded successfully }
var
  i:integer;
begin
  result := true;
  for i := 0 to nuServer1.ServerFiles.Count - 1 do begin
    if not FileExists(GetTempDir + nuServer1.ServerFiles[i].FileName) then begin
      WriteLog('Error downloading installation files.');
      CleanUpAfterCancel();
      result := false;
      exit;
      end;
    end;
  WriteLog('Download of installation files completed successfully');
end; { DownloadCompleted }

{ ----------------------------------------------------------------- }

procedure TfmWizard.CleanUpAfterCancel();
var
  i: integer;
  tFile: string;
begin
  if Visible then Hide();
  { delete all downloaded files from temp directory }
  for i := 0 to nuServer1.ServerFiles.Count - 1 do begin
    tFile := GetTempDir + nuServer1.ServerFiles[i].FileName;
    WriteLog(Format('Deleting: %s',[tFile]));
    if FileExists(tFile) then
      DeleteFile(tFile);
    end;
end; { CleanUpAfterCancel }

{ ----------------------------------------------------------------- }

procedure TfmWizard.lblFileStatusEnter(Sender: TObject);
{ prevents TEdit control from showing cursor if clicked on }
begin
  ProgressCurrentFile.SetFocus();
end; { lblFileStatusEnter }

{ ----------------------------------------------------------------- }

procedure TfmWizard.CancelBtnClick(Sender: TObject);
begin
  if INetBusy then
    INetCancelCopy := true
  else
    FCancelCopy := true;
end; { CancelBtnClick }

{ ----------------------------------------------------------------- }

function TfmWizard.GetDestDirectory(dDir:TnuTargetDir):string;
begin
  case dDir of
    tdApp:  result := dirPath;
    tdProgramFiles: result := GetProgramFilesDir();
    tdWindows: result := GetWindowsDir();
    tdSystem: result := GetSystemDir();
    tdCommonAppData: result := GetCommonAppData();
    tdCommonDesktop: result := GetCommonDesktop();
    tdCommonProgramFiles: result := IncludeTrailingBackslash(GetEnvVarValue('CommonProgramFiles'));
    tdTempDir: result := GetTempDir();
  else result := dirPath;
  end;
end; { GetDestDirectory }

{ ----------------------------------------------------------------- }

procedure TfmWizard.LaunchUpdater(const f1,f2,x1,x2,x3,p1,p2: String);
var
  Dummy: DWord;
  MS: TMemoryStream;
  FileHandle: hFile;
  Launch: Boolean;
begin
  Launch := true;
  if Assigned(nuClient1.OnLaunchUpdater) then nuClient1.OnLaunchUpdater(Self,Launch);
  if not Launch then Exit;
  { stream updater.exe }
  FileHandle := CreateFile(PChar(UpgraderFileName), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
                           CREATE_ALWAYS, FILE_ATTRIBUTE_TEMPORARY, 0);
  if FileHandle <> INVALID_HANDLE_VALUE then
   begin
    MS := TMemoryStream.Create;
    try
      LoadResourceToStream(hInstance, 'NUEXE', 'EXE', MS);
      WriteFile(FileHandle, MS.Memory^, MS.Size, Dummy, nil);
    finally
      MS.Free;
    end;
    CloseHandle(FileHandle);
   end;

  { start updater program }
  WinExec(PChar(UpgraderFileName +' "'+f1+'" "'+f2+'" '+x1+' '+x2+' '+x3+' +'+p1+' +'+p2), SW_SHOWNORMAL);
end; { LaunchUpdater }

{ ----------------------------------------------------------------- }

procedure TfmWizard.AboutClick(Sender:TObject);
begin
  MessageBox(0, PChar('Net Update trial component installed.'#13#10 +
                      '(c) 2004-2006, Kidmoses.com, All rights reserved.'#13#10 +
                      'http://www.kidmoses.com'),
                      'Net Update v1.01', MB_OK);
end; { AboutClick }

{ ----------------------------------------------------------------- }

initialization
  UpgraderFileName := GetTempDir + 'updater.exe';

end.
