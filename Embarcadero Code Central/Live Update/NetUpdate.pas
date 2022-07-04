unit NetUpdate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, ShellAPI, HttpProt;

const
  WM_DownloadFinished = wm_User + 1;

type
  TTargetDir = (TempDir, AppDir, SaveAsDir);

// NetDialog form --------------------------------------------------------------
  TRequest = (_None, _Info, _Update);

  TNetDialog = class(TForm)
    Panel1: TPanel;
    btnInfo: TSpeedButton;
    btnDownload: TSpeedButton;
    lblCurrVersion: TLabel;
    btnAbort: TSpeedButton;
    pbSize: TProgressBar;
    gbHomeSite: TGroupBox;
    lblNewVer: TLabel;
    lblFSize: TLabel;
    lblModified: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure HttpCliHeaderData(Sender: TObject);
    procedure HttpCliDocData(Sender: TObject; Buffer: Pointer; Len: Integer);
    procedure HttpCliRequestDone(Sender: TObject; RqType: THttpRequest; Error: Word);
  private
    fAbort,
    fHeaders      : Boolean;
    HTTPResult,
    UpdateSize,
    BytesRecieved : Integer;
    aRequest      : TRequest;
    InfoLine,
    InfFName,
    Modified,
    NewVersion,
    CurrVersion   : String;
    HeadersList,
    VerList,
    InfoList      : TStringList;
    HttpCli       : ThttpCli;
    dlFile        : File;
    procedure ChangeButtonsState(fEnabled: Boolean);
  public
    CompHandle : HWnd;
    DoExtract  : boolean;
    dlFName,
    SourceURL  : string;
    aTargetDir : TTargetDir;
  end;
//------------------------------------------------------------------------------
  TUpdateEndEvent = procedure (Sender : TObject;
                               FName  : String;
                               Error  : Integer) of object;

  TNetUpdate= class(TComponent)
  private
    fHWnd: HWnd;
    fAutoExtract : boolean;
    fTargetDir   : TTargetDir;
    fURL         : string;
    fOnUpdateEnd : TUpdateEndEvent;
    NetDialog    : TNetDialog;
  protected
    procedure CompWndProc(var Message: TMessage);
    procedure WMDownloadFinished(var Message: TMessage); message WM_DownloadFinished;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  published
    property AutoExtract : boolean read fAutoExtract write fAutoExtract default true;
    property TargetDir   : TTargetDir read fTargetDir write fTargetDir default TempDir;
    property SourceURL   : string read fURL write fURL;

    property OnUpdateEnd : TUpdateEndEvent read fOnUpdateEnd write fOnUpdateEnd;
  end;

procedure Register;


implementation

{$R *.DFM}

const
  HTTP_OK       = 200;
  HTTP_NOTFOUND = 404;

function FileVersion(FileName: string) : String;

Var
  Info         : Pointer;
  Temp,
  InfoSize,
  FileInfoSize : DWORD;
  FileInfo     : PVSFixedFileInfo;
  MJ1, MJ2,
  MI1, MI2     : Integer;

begin
  Result := '';
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Temp);
  if InfoSize > 0 then
  begin
    GetMem(Info, InfoSize);
    FileInfoSize := SizeOf(FileInfo^);
    try
      GetFileVersionInfo(PChar(FileName), 0, InfoSize, Info);
      VerQueryValue(Info, '\', Pointer(FileInfo), FileInfoSize);
      with FileInfo^ do
      begin
        MJ1 := dwFileVersionMS shr 16;
        MJ2 := dwFileVersionMS and $FFFF;
        MI1 := dwFileVersionLS shr 16;
        MI2 := dwFileVersionLS and $FFFF;
      end;
    finally
      FreeMem(Info, InfoSize);
    end;
    Result := IntToStr(MJ1) + '.' + IntToStr(MJ2) + '.' + IntToStr(MI1) + '.' + IntToStr(MI2);
  end;
end;

function ProcessHeaders(Headers: TstringList) : Integer;

const
  Digits : String = '0123456789';

var
  I, X : Integer;
  D, S : String;

begin
  with headers do
  begin
    for I := 0 to Count - 1 do
    begin
      S := UpperCase(Strings[I]);
      X := Pos(':', S);
      if X > 0 then
      begin
        S[X] := '=';
        if S[X + 1] = #32 then
          System.Delete(S, X + 1, 1);
      end;
      Strings[I] := S;
    end;
    D := '';
    S := Strings[0];
    System.Delete(S, 1, Pos(#32, S));  // 'HTTP/x.x '
    I := 1;
    while (I < Length(S)) and (Pos(S[I], Digits) > 0) do
    begin
      D := D + S[I];
      Inc(I);
    end;
    Result := StrToInt(D);
  end;
end;

function GetSizeFromHeaders(Headers: TstringList) : Integer;

var S : String;

begin
  S := Headers.Values['CONTENT-LENGTH'];
  if S <> '' then
    Result := StrToInt(S)
  else
    Result := 0;
end;

function GetDateFromHeaders(Headers: TstringList) : String;

begin
  Result := Headers.Values['LAST-MODIFIED'];
end;

function ExtractDomain(Domain: String) : String;

begin
  System.Delete(Domain, 1, 7);  // 'http://'
  Result := Copy(Domain, 1, Pos('/', Domain) - 1);
end;

// NetDialog form --------------------------------------------------------------

procedure TNetDialog.FormCreate(Sender: TObject);

var
  I : Integer;
  S : String;

begin
  fAbort := False;
  fHeaders := False;
  aRequest := _None;
  HTTPResult := HTTP_OK;
  UpdateSize := 0;
  BytesRecieved := 0;
  Modified := '';
  InfoLine := '';
  NewVersion := '';
  btnDownload.Enabled := False;
  btnAbort.Enabled := False;
  HttpCli := ThttpCli.Create(Self);
  with HttpCli do
  begin
    OnHeaderData := HttpCliHeaderData;
    OnDocData := HttpCliDocData;
    OnRequestDone := HttpCliRequestDone;
  end;
  VerList := TStringList.Create;
  InfoList := TStringList.Create;
  HeadersList := TStringList.Create;
  CurrVersion := FileVersion(Application.ExeName);
  S := LowerCase(ExtractFileName(Application.ExeName));
  System.Delete(S, Pos('.', S), 6);
  Caption := S + '   Update';
  lblCurrVersion.Caption := 'Local  ' + S + '  version:  ' + CurrVersion;
end;

procedure TNetDialog.FormDestroy(Sender: TObject);
begin
  with HttpCli do
  begin
    Abort;
    Free;
  end;
  HeadersList.Free;
  VerList.Free;
  InfoList.Free;
end;

procedure TNetDialog.FormShow(Sender: TObject);

var S : String;

begin
  InfFName := ChangeFileExt(Application.ExeName, '.inf');
  if FileExists(InfFName) then
    begin
      InfoList.LoadFromFile(InfFName);
      S := ExtractFileName(Application.ExeName);
      System.Delete(S, Pos('.', S), 6);
      InfoLine := InfoList.Values[S];
    end
  else
    if SourceURL = '' then
      begin
        MessageDlg('Information file: ' + InfFName + ' doesn' + #39 + 't exist,' + #13 + #10 +
                   'Source URL not defined,' + #13 + #10 +
                   'NetUpdate aborted.', mtError, [mbOK], 0);
        PostMessage(Handle, WM_CLOSE, 0, 0);
      end
    else
      InfoLine := SourceURL;
  if InfoLine <> '' Then
    gbHomeSite.Caption := 'Home site:  ' + ExtractDomain(InfoLine);
end;

procedure TNetDialog.ChangeButtonsState(fEnabled: Boolean);

begin
  btnInfo.Enabled := fEnabled;
  btnDownLoad.Enabled := fEnabled;
  btnAbort.Enabled := not fEnabled;
end;

procedure TNetDialog.btnInfoClick(Sender: TObject);

begin
  HTTPResult := HTTP_OK;
  UpdateSize := 0;
  Modified := '';
  NewVersion := '';
  with HttpCli do
  begin
    aRequest := _Info;
    HeadersList.Clear;
    URL := InfoLine + '.zip';
    fHeaders := True;
    HeadAsync;
    While fHeaders Do
      Application.ProcessMessages;
    if HTTPResult = HTTP_NOTFOUND then
      exit;
    aRequest := _Info;
    VerList.Clear;
    URL := InfoLine + '.ver';
    GetAsync;
  end;
end;

procedure TNetDialog.btnDownloadClick(Sender: TObject);

var
  I       : Integer;
  S1, S2  : String;
  T       : Array[0..127] of char;
  sDialog : TSaveDialog;

begin
  aRequest := _Update;
  fAbort := False;
  S1 := '';
  BytesRecieved := 0;
  I := Length(InfoLine);
  while (I > 1) and (InfoLine[I] <> '/') do
  begin
    S1 := InfoLine[I] + S1;
    Dec(I);
  end;
  dlFName := ExtractFilePath(Application.ExeName) + S1 + '.zip';
  if aTargetDir = TempDir then
  begin
    I := GetTempPath(127, T);
    if I > 0 then
    begin
      S2 := StrPas(T);
      if S2[Length(S2)] <> '\' then
        S2 := S2 + '\';
      dlFName := S2 + S1 + '.zip';
    end;
  end;
  if aTargetDir = SaveAsDir then
  begin
    sDialog := TSaveDialog.Create(Self);
    With sDialog do
    begin
      InitialDir := ExtractFilePath(dlFName);
      Filter := 'Zip file|*.ZIP|Any file|*.*';
      DefaultExt := '.zip';
      FileName := ExtractFileName(dlFName);
      Title := 'Save downloaded file as';
      if Execute then
        dlFName := FileName
      else
        begin
          Free;
          Exit;
        end;
    end;
  end;
  AssignFile(dlFile, dlFName);
  I := IOResult;
  ReWrite(dlFile, 1);
  I := IOResult;
  ChangeButtonsState(False);
  with HttpCli do
  begin
    URL := InfoLine + '.zip';
    GetAsync;
  end;
end;

procedure TNetDialog.btnAbortClick(Sender: TObject);
begin
  fAbort := True;
end;

procedure TNetDialog.HttpCliHeaderData(Sender: TObject);

begin
  HeadersList.Text := HeadersList.Text + THttpCli(Sender).LastResponse;
end;

procedure TNetDialog.HttpCliDocData(Sender: TObject; Buffer: Pointer; Len: Integer);

var I : Integer;

begin
  case aRequest of
      _Info : VerList.Text := VerList.Text + StrPas(Buffer);
    _Update : begin
                if fAbort then
                begin
                  ThttpCli(Sender).Abort;
                  ChangeButtonsState(True);
                  Exit;
                end;
                BlockWrite(dlFile, Buffer^, Len);
                I := IOResult;
                BytesRecieved := BytesRecieved + Len;
                if UpdateSize > 0 Then
                  pbSize.Position := BytesRecieved div 1024;
              end;
  end;
end;

procedure TNetDialog.HttpCliRequestDone(Sender: TObject; RqType: THttpRequest; Error: Word);

var
  sTemp,
  sError : String;

begin
  if Error <> 0 then
    sError := ThttpCli(Sender).ReasonPhrase;
  fHeaders := False;
  case aRequest of
      _Info : if Error = 0 then
                begin
                  if rqType = httpHEAD then
                  begin
                    HTTPResult := ProcessHeaders(HeadersList);
                    if HTTPResult <> HTTP_NOTFOUND then
                      begin
                        UpdateSize := GetSizeFromHeaders(HeadersList) div 1024;
                        Modified := GetDateFromHeaders(HeadersList);
                      end
                    else
                      begin
                        MessageDlg('HTTP error: ' + IntToStr(HTTPResult) + #13#10 +
                                   'Request aborted.', mtError, [mbNo], 0);
                        Exit;
                      end;
                  end;
                  NewVersion := VerList.Values['version'];
                  if NewVersion = '' then
                    NewVersion := 'Unknown';
                  lblNewVer.Caption := 'Program file version:  ' + NewVersion;
                  if (UpdateSize = 0) and (VerList.Count = 2) then  // 2 lines means proper ver file
                  begin
                    sTemp := VerList.Values['size'];
                    If sTemp <> '' then
                      UpdateSize := StrToInt(sTemp) div 1024
                    else
                      UpdateSize := 0;
                  end;
                  lblFSize.Caption := 'ZIP file size:  ' + IntToStr(UpdateSize) + ' Kb';
                  if Modified <> '' then
                    lblModified.Caption := 'Last modified:  ' + LowerCase(Modified);
                  btnDownload.Enabled := (NewVersion <> CurrVersion);
                  with pbSize do
                  begin
                    Max := UpdateSize;
                    Position := 0;
                  end;
                end
              else
                MessageDlg('Get version error:' + #13#10 + sError, mtError, [mbOK], 0);
    _Update : begin
                CloseFile(dlFile);
                ChangeButtonsState(True);
                if Error = 0 then
                  begin
                    if DoExtract then
                      if ShellExecute(Handle, 'open', PChar(dlFName), nil, nil, SW_SHOW) <= 32 then
                        MessageDlg('Cannot open ' + dlFName + #13#10 +
                                   'Try to extract it manually.', mtError, [mbOK], 0)
                      else  // ShelExecute
                    else    // DoExtract
                      MessageDlg('Update file saved to:  ' + dlFName, mtInformation, [mbOK], 0);
                  end
                else
                  begin
                    DeleteFile(dlFName);
                    pbSize.Position := 0;
                    btnDownload.Enabled := False;
                    MessageDlg('Download error:' + #13#10 + sError, mtError, [mbOK], 0);
                  end;
                PostMessage(CompHandle, WM_DownloadFinished, Error, 0);
              end;
  end;
  aRequest := _None;
end;

//------------------------------------------------------------------------------

constructor TNetUpdate.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  fHWnd := AllocateHWnd(CompWndProc);
  fAutoExtract := true;
  fURL := '';
  fTargetDir := TempDir;
  NetDialog := TNetDialog.Create(Self);
  NetDialog.Visible := False;
end;

destructor TNetUpdate.Destroy;

begin
  DeallocateHWnd(fHWnd);
  NetDialog.Free;
  Inherited Destroy;
end;

procedure TNetUpdate.CompWndProc(var Message: TMessage);

begin
  Dispatch(Message);
end;

procedure TNetUpdate.WMDownloadFinished(var Message: TMessage);

begin
  if Assigned(FOnUpdateEnd) then
    FOnUpdateEnd(Self, NetDialog.dlFName, Message.wParam);
end;

procedure TNetUpdate.Execute;

begin
  with NetDialog do
  begin
    CompHandle := fHWnd;
    DoExtract := fAutoExtract;
    SourceURL := fURL;
    aTargetDir := fTargetDir;
    Show;
  end;
End;

procedure Register;
begin
  RegisterComponents('LGM', [TNetUpdate]);
end;

end.
