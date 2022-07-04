unit kmWinInet;

interface

uses Windows,Classes,WinInet,SysUtils,kmUtils,kmTypes;

const
  ERROR_SUCCESS = 0;
  ERROR_CANCELLED = 1223;

var
  AppHandle: THandle;
  AccessType: TnuAccessType;
  AskCancel: boolean;
  RunMode: TnuRunMode;
  DoLog: boolean;
  INetCancelCopy: boolean;
  INetBusy: boolean;
  INetLocalFolder: string;
  INetPassword: string;
  INetPort: integer;
  INetProxyBypass: string;
  INetProxyPassword: string;
  INetProxyPort: string;
  INetProxyServer: string;
  INetProxyUsername: string;
  INetUsername: string;
  LogFile: string;
  URLComponents: TURLComponents;
  URLDir: string;
  URLFile: string;
  URLFull: string;
  URLHost: string;
  URLSize: longint;     { size of remote file }
  URLPath: string;
  URLScheme: string;

  {cache settings}
  CacheIfNetFail: boolean;
  CreateTemp: boolean;
  ForceReload: boolean;
  HyperLink: boolean;
  NoCacheWrite: boolean;
  Pragma: boolean;
  Reload: boolean;
  Resynchronize: boolean;

  {internet settings }
  IgnoreInvalidCert: boolean;
  IgnoreInvalidCertDate: boolean;
  IgnoreRedirectHttp: boolean;
  IgnoreRedirectHttps: boolean;
  KeepConnection: boolean;
  NoAuthentication: boolean;
  NoAutoRedirect: boolean;
  NoCookieDialog: boolean;
  NoCookies: boolean;
  SecureTransaction: boolean;

  {miscellaneous}
  Notify404Error: boolean;
  NotifyConnectionLost: boolean;
  NotifyHostUnreachable: boolean;
  NotifyPasswordNeeded: boolean;
  ThreadPriority: TThreadPriority;    // currently not used
  TransferBufferSize: integer;        // currently not used

  {messages}
  MsgTitle: string;
  MsgHostUnreachable: string;
  MsgFileNotFound: string;
  MsgConnectionLost: string;
  MsgCancelUpdate: string;
  
{ External Function timeGetTime }
function timeGetTime: DWORD; stdcall;
function timeGetTime; external 'winmm.dll' name 'timeGetTime';

{ Internal Functions }
function GetDirFromUrl(const URL: string): string;
function GetFileNameFromUrl(const URL: string): string;
function INetConnect(hSession: HInternet): HInternet;
function INetCrackURL(aURL: string): boolean;
function INetGetFileSize(hResource: HInternet): DWORD;
function INetHttpOpenRequest(hConnect: HInternet): HInternet;
function INetHttpSendRequest(hResource: HInternet): boolean;
function INetOpen(): HInternet;
function INetStart(sFile,dFile:string;CallBack:TCallBack): boolean;
function IsOnline(): Boolean;
function IsProxy(): boolean;
procedure LogError(eCode:DWORD);
procedure WriteLog(aLog:string);

implementation

procedure WriteLog(aLog:string);
var
  myFile: TextFile;
begin
  if not DoLog then exit;
  AssignFile(myFile,LogFile);
  if FileExists(LogFile) then
    Append(myFile) else
    Rewrite(myFile);
  Writeln(myFile,aLog);
  Flush(myFile);
  CloseFile(myFile);
end; { WriteLog }

{ ----------------------------------------------------------------- }

procedure LogError(eCode:DWORD);
var
  errStr: string;
  errLen: DWORD;
begin
  errLen:= 1024;
  SetLength(errStr,errLen);
  InternetGetLastResponseInfo(eCode,PChar(errStr),errLen);
  WriteLog('Error code: ' + IntToStr(eCode));
  WriteLog(errStr);
end; { WriteLog }

{ ----------------------------------------------------------------- }

function GetFileNameFromUrl(const URL: string): string;
var 
i: Integer; 
begin 
   result := URL;
   i := Length(url); 
   while (i>0) and ((url[i] <> '/') and (url[i] <> '\')) do
   dec(i); 
   If i=0 Then Exit;
   result := Copy(url, i+1, maxint);
end; {GetFileNameFromUrl}

{ ----------------------------------------------------------------- }

function GetDirFromUrl(const URL: string): string;
var 
i: Integer; 
begin 
   result := '';        { assume no directory }
   i := Length(url); 
   while (i>0) and ((url[i] <> '/') and (url[i] <> '\')) do
   dec(i); 
   If i=0 Then Exit;
   result := Copy(url, 1, i);
end; {GetFileNameFromUrl}

{ ----------------------------------------------------------------- }

function IsOnline(): Boolean;
const
  C1 = INTERNET_CONNECTION_MODEM;
  C2 = INTERNET_CONNECTION_LAN;
  C4 = INTERNET_CONNECTION_PROXY;
  C8 = INTERNET_CONNECTION_MODEM_BUSY;
var
  dwFlags: DWORD;
begin
  dwFlags := C1+C2+C4;
  result := InternetGetConnectedState(@dwFlags, 0);
end; {IsOnline}

{ ----------------------------------------------------------------- }

function IsProxy(): boolean;
var ProxyInfo : PInternetProxyInfo;
    Len       : LongWord;
begin
  Result := false;
  Len := 4096;
  GetMem(ProxyInfo, Len);
  try
    if InternetQueryOption(nil, INTERNET_OPTION_PROXY, ProxyInfo, Len) then
      if ProxyInfo^.dwAccessType = INTERNET_OPEN_TYPE_PROXY then
        Result := true;
  finally
    FreeMem(ProxyInfo);
  end;
end; {IsProxy}

{ ----------------------------------------------------------------- }

function INetCrackURL(aURL: string): boolean;
var
  dwBuffer: DWORD;
  aBuffer: string;
begin
  result := false;
  FillChar(UrlComponents, SizeOf(TUrlComponents), 0);

  URLComponents.lpszScheme := nil;
  URLComponents.dwSchemeLength := 1;
  URLComponents.lpszHostName := nil;
  URLComponents.dwHostNameLength := 1;
  URLComponents.lpszUrlPath := nil;
  URLComponents.dwUrlPathLength := 1;
  URLComponents.lpszUserName := nil;
  URLComponents.dwUserNameLength := 1;
  URLComponents.lpszPassword := nil;
  URLComponents.dwPasswordLength := 1;
  URLComponents.lpszExtraInfo := nil;
  URLComponents.dwExtraInfoLength := 1;

  UrlComponents.dwStructSize := SizeOf(UrlComponents);

  dwBuffer := 1;  {using 0 causes an error }
  SetLength(aBuffer,Length(aURL)+1);
  InternetCanonicalizeUrl(PChar(aURL),PChar(aBuffer),dwBuffer,0); { Get the required buffer size in dwBuffer }
  if (dwBuffer > 0) then begin  { will always be true }
    SetLength(aBuffer,dwBuffer);
    if InternetCanonicalizeUrl(PChar(aURL),PChar(aBuffer),dwBuffer,0) then begin
      if InternetCrackUrl(PChar(aBuffer),Length(aBuffer),0,UrlComponents) then begin
        result := true;
        URLScheme := Copy(UrlComponents.lpszScheme,0,UrlComponents.dwSchemeLength);
        URLHost := Copy(UrlComponents.lpszHostName,0,UrlComponents.dwHostNameLength);
        URLPath := Copy(UrlComponents.lpszUrlPath,0,UrlComponents.dwUrlPathLength);
        URLFile := GetFilenameFromURL(URLPath);
        URLDir  := ExcludeLeadingSlash(GetDirFromURL(URLPath));
        end;
      end;
    end;
end; {INetCrackURL}

{ ----------------------------------------------------------------- }

function INetOpen(): HInternet;
var
  dwAccessType: DWORD;
  lpszProxy,lpszProxyBypass: string;
begin
  dwAccessType := INTERNET_OPEN_TYPE_PRECONFIG;  { initialize }
  case AccessType of
    atPreConfig: begin
                 lpszProxy := '';
                 lpszProxyBypass := '';
                 dwAccessType := INTERNET_OPEN_TYPE_PRECONFIG;
                 end;
    atUseProxy:  begin
                 lpszProxy := INetProxyServer + ':' + INetProxyPort;;
                 lpszProxyBypass := INetProxyBypass;
                 dwAccessType := INTERNET_OPEN_TYPE_PROXY;
                 end;
    atDirect:    begin
                 lpszProxy := '';
                 lpszProxyBypass := '';
                 dwAccessType := INTERNET_OPEN_TYPE_DIRECT;
                 end;
    end;

  result := InternetOpen(PChar('Net Update'),
                         dwAccessType,
                         PChar(lpszProxy), PChar(lpszProxyBypass), 0);

  if not boolean(result) then begin
    WriteLog('Failed to open http session.');
    LogError(GetLastError());
    end;
end; {INetOpen}

{ ----------------------------------------------------------------- }

function INetConnect(hSession: HInternet): HInternet;
var
  nServerPort: integer;
  lpszUsername,lpszPassword: string;
  dwService,dwFlags,dwContext: DWORD;
begin
  if (URLScheme = 'http') or (URLScheme = 'https') then begin
    nServerPort := INetPort;
    lpszUsername := '';
    lpszPassword := '';
    dwService := INTERNET_SERVICE_HTTP;
    dwFlags := 0;
    dwContext := 0;
    end
  else begin
    nServerPort := INetPort;
    lpszUsername := INetUsername;
    lpszPassword := INetPassword;
    dwService := INTERNET_SERVICE_FTP;
    dwFlags := INTERNET_FLAG_PASSIVE;
    dwContext := 0;
    end;

  result := InternetConnect(hSession,
                            PChar(URLHost),
                            nServerPort,
                            PChar(lpszUsername),
                            PChar(lpszPassword),
                            dwService,
                            dwFlags,
                            dwContext);
  if not boolean(result) then begin
    WriteLog('Failed to connect to resource.');
    LogError(GetLastError());
    if NotifyHostUnreachable then begin
      if RunMode <> rmNormal then exit;
      MessageBeep(MB_ICONASTERISK);
      MessageBox(AppHandle,
                 PChar(MsgHostUnreachable + #13 + URLHost),
                 PChar(MsgTitle),MB_OK);
      end;
    end;
end; {INetConnect}

{ ----------------------------------------------------------------- }

function INetHttpOpenRequest(hConnect: HInternet): HInternet;
var
  dwFlags: DWORD;
begin
  dwFlags := 0;
  { cache settings }
  if ForceReload then dwFlags := dwFlags or INTERNET_FLAG_RELOAD;
  if HyperLink then dwFlags := dwFlags or INTERNET_FLAG_HYPERLINK;
  if Resynchronize then dwFlags := dwFlags or INTERNET_FLAG_RESYNCHRONIZE;
  if Pragma then dwFlags := dwFlags or INTERNET_FLAG_PRAGMA_NOCACHE;
  if NoCacheWrite then dwFlags := dwFlags or INTERNET_FLAG_NO_CACHE_WRITE;
  if CreateTemp then dwFlags := dwFlags or INTERNET_FLAG_NEED_FILE;
  if CacheIfNetFail then dwFlags := dwFlags or INTERNET_FLAG_CACHE_IF_NET_FAIL;

  { internet settings }
  if IgnoreInvalidCert then dwFlags := dwFlags or INTERNET_FLAG_IGNORE_CERT_CN_INVALID;
  if IgnoreInvalidCertDate then dwFlags := dwFlags or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;
  if IgnoreRedirectHttp then dwFlags := dwFlags or INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP; 
  if IgnoreRedirectHttps then dwFlags := dwFlags or INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS;
  if KeepConnection then dwFlags := dwFlags or INTERNET_FLAG_KEEP_CONNECTION;
  if NoAuthentication then dwFlags := dwFlags or INTERNET_FLAG_NO_AUTH;
  if NoAutoRedirect then dwFlags := dwFlags or INTERNET_FLAG_NO_AUTO_REDIRECT;
  if NoCookies then dwFlags := dwFlags or INTERNET_FLAG_NO_COOKIES;
  if NoCookieDialog then dwFlags := dwFlags or INTERNET_FLAG_NO_UI;
  if URLScheme = 'https' then
    dwFlags := dwFlags or INTERNET_FLAG_SECURE;
  { don't set for http or you will get an error: 12157 }
  //else if SecureTransaction then dwFlags := dwFlags or INTERNET_FLAG_SECURE;

  result := HttpOpenRequest(hConnect, 'GET',
                            PChar(URLPath),
                            nil, nil, nil,
                            dwFlags, 0);
  if not boolean(result) then begin
    WriteLog('Failed to open http request.');
    LogError(GetLastError());
    end;
end; {INetHttpOpenRequest}

{ ----------------------------------------------------------------- }

function INetFtpOpenFile(hConnect: HInternet): HInternet;
var
  dwFlags: DWORD;
begin
  dwFlags := FTP_TRANSFER_TYPE_BINARY;
  { cache settings }
  if ForceReload then dwFlags := dwFlags or INTERNET_FLAG_RELOAD;
  if HyperLink then dwFlags := dwFlags or INTERNET_FLAG_HYPERLINK;
  if Resynchronize then dwFlags := dwFlags or INTERNET_FLAG_RESYNCHRONIZE;
  if Pragma then dwFlags := dwFlags or INTERNET_FLAG_PRAGMA_NOCACHE;
  if NoCacheWrite then dwFlags := dwFlags or INTERNET_FLAG_NO_CACHE_WRITE;
  if CreateTemp then dwFlags := dwFlags or INTERNET_FLAG_NEED_FILE;
  if CacheIfNetFail then dwFlags := dwFlags or INTERNET_FLAG_CACHE_IF_NET_FAIL;

  { internet settings }
  if KeepConnection then dwFlags := dwFlags or INTERNET_FLAG_KEEP_CONNECTION;
  if NoAuthentication then dwFlags := dwFlags or INTERNET_FLAG_NO_AUTH;

  result := FtpOpenFile(hConnect,
                        PChar(URLFile),
                        GENERIC_READ,
                        dwFlags,
                        0);
  if not boolean(result) then begin
    WriteLog('Failed to open ftp file.');
    LogError(GetLastError());
    if Notify404Error then begin
      if RunMode <> rmNormal then exit;
      MessageBeep(MB_ICONASTERISK);
      MessageBox(AppHandle,
                 PChar(MsgFileNotFound+URLFile),
                 PChar(MsgTitle),MB_OK);
      end;
    end;
end; {INetHttpOpenRequest}

{ ----------------------------------------------------------------- }

function INetHttpSendRequest(hResource: HInternet): boolean;
label send;
var
  dwErrorCode,dwIndex: DWORD;
  dwStatus,dwStatusSize,dwLastStatus: DWORD;
begin
  result := false;
  dwIndex := 0;
  dwLastStatus := 0;
  dwStatusSize := sizeof(dwStatus);

  send:
  if HttpSendRequest(hResource, nil, 0, nil, 0) then
    dwErrorCode := ERROR_SUCCESS else
    dwErrorCode := GetLastError();

  if (not HttpQueryInfo(hResource,
                        HTTP_QUERY_STATUS_CODE or
                        HTTP_QUERY_FLAG_NUMBER,
                        @dwStatus, dwStatusSize, dwIndex)) then exit;
  if dwStatus = dwLastStatus then exit;         { if same as last time then user cancelled or other error }

  case dwStatus of
    HTTP_STATUS_OK:              begin          { error code 200 }
                                 result := true;
                                 end;
    HTTP_STATUS_REDIRECT:        begin          { error code 302 }
                                 if NoAutoRedirect then exit;
                                 { should automatically handle okay }
                                 end;
    HTTP_STATUS_BAD_REQUEST:     begin          { error code 400 }
                                 WriteLog('Error: HTTP_STATUS_BAD_REQUEST, Error code: 400.');
                                 WriteLog('The request could not be processed by the server due to invalid syntax.');
                                 end;
    HTTP_STATUS_DENIED:          begin          { error code 401 }
                                 dwLastStatus := HTTP_STATUS_DENIED;
                                 if (INetUsername = '') then begin { Assume not preset }
                                   if not NotifyPasswordNeeded then goto send; { force exit }
                                   InternetErrorDlg(GetDesktopWindow(), hResource, dwErrorCode,
                                                    FLAGS_ERROR_UI_FILTER_FOR_ERRORS or
                                                    FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS or
                                                    FLAGS_ERROR_UI_FLAGS_GENERATE_DATA,
                                                    Pointer(nil^));
                                   end
                                 else begin
                                   InternetSetOption(hResource, INTERNET_OPTION_USERNAME,
                                                     PChar(INetUsername), Length(INetUsername)+1);
                                   InternetSetOption(hResource, INTERNET_OPTION_PASSWORD,
                                                     PChar(INetPassword), Length(INetPassword)+1);
                                   end;
                                 goto send;
                                 end;
    HTTP_STATUS_NOT_FOUND:       begin          { error code 404 }
                                 dwLastStatus := HTTP_STATUS_NOT_FOUND;
                                 if not Notify404Error then goto send;
                                 if RunMode <> rmNormal then goto send;
                                 MessageBeep(MB_ICONASTERISK);
                                 MessageBox(AppHandle,
                                            PChar(MsgFileNotFound+URLFile),
                                            PChar(MsgTitle),MB_OK);
                                 end;
    HTTP_STATUS_PROXY_AUTH_REQ : begin          { error code 407 }
                                 dwLastStatus := HTTP_STATUS_PROXY_AUTH_REQ;
                                 if (INetProxyUsername = '') then begin { Assume not preset }
                                   if not NotifyPasswordNeeded then goto send; { force exit }
                                   InternetErrorDlg(GetDesktopWindow(), hResource, dwErrorCode,
                                                    FLAGS_ERROR_UI_FILTER_FOR_ERRORS or
                                                    FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS or
                                                    FLAGS_ERROR_UI_FLAGS_GENERATE_DATA,
                                                    Pointer(nil^));
                                   end
                                 else begin
                                   InternetSetOption(hResource, INTERNET_OPTION_PROXY_USERNAME,
                                                     PChar(INetProxyUsername), Length(INetProxyUsername)+1);
                                   InternetSetOption(hResource, INTERNET_OPTION_PROXY_PASSWORD,
                                                     PChar(INetProxyPassword), Length(INetProxyPassword)+1);
                                   end;
                                 goto send;
                                 end;
   end;
end; {INetHttpSendRequest}

{ ----------------------------------------------------------------- }

function INetGetFileSize(hResource: HInternet): DWORD;
var
  dwStatus,dwStatusSize,dwIndex: DWORD;
  lpdwFileSizeHigh: DWORD;
begin
  result := 0;
  dwIndex := 0;
  dwStatusSize := sizeof(dwStatus);
  if (URLScheme = 'http') or (URLScheme = 'https') then begin
    if HttpSendRequest(hResource, nil, 0, nil, 0) then begin
      if not (HttpQueryInfo(hResource,
                            HTTP_QUERY_CONTENT_LENGTH or
                            HTTP_QUERY_FLAG_NUMBER,
                            @dwStatus, dwStatusSize, dwIndex))
                            then begin
        WriteLog('Cannot retrieve file size information for ' + URLFULL);
        LogError(GetLastError());
        exit;
        end;
      result := dwStatus;
      end
    else begin
      WriteLog('Error sending HTTP request for file size.');
      LogError(GetLastError());
      exit
      end;
    end
  else begin
    result := FtpGetFileSize(hResource,@lpdwFileSizeHigh);
    end;
end; {INetGetFileSize}

{ ----------------------------------------------------------------- }

function INetStart(sFile,dFile:string;CallBack:TCallBack): boolean;
label fastexit;
const BufferSize = 1024 * 12;  { seems to be best /smoothest size }
var
  hSession,hConnect,hResource: HInternet;
  iFile: File;
  filePath: string;
  Buffer: array[1..BufferSize] of char;
  BufferLen: LongWord;
  SizeDone,SizeFile,TimeStart: DWORD;
begin
  result := false;
  if not IsOnline() then exit;
  INetBusy := true;
  hConnect := nil;
  hResource := nil;
  hSession := INetOpen();
  if not boolean(hSession) then goto fastexit;

  URLFull := sFile;
  if not INetCrackURL(URLFull) then goto fastexit;

  hConnect := INetConnect(hSession);
  if not boolean(hConnect) then goto fastexit;

  if (URLScheme = 'http') or (URLScheme = 'https') then begin
    hResource := INetHttpOpenRequest(hConnect);
    if not boolean(hResource) then goto fastexit;
    if not INetHttpSendRequest(hResource) then goto fastexit;
    if URLSize = 0 then
      SizeFile := INetGetFileSize(hResource)
    else SizeFile := URLSize;
    end
  else begin
    FtpSetCurrentDirectory(hConnect,PChar(URLDir));
    hResource := INetFtpOpenFile(hConnect);
    if not boolean(hResource) then goto fastexit;
    if URLSize = 0 then
      SizeFile := INetGetFileSize(hResource)
    else SizeFile := URLSize;
    end;

  BufferLen := BufferSize;
  filePath := ExtractFilePath(dFile);
  if not (DirectoryExists(filePath)) then
    CreatePath(filePath);
  AssignFile(iFile,dFile);
  try
    SizeDone := 0;
    Rewrite(iFile,1);

    TimeStart := timegettime();
    repeat
      if not InternetReadFile(hResource,@Buffer,SizeOf(Buffer),BufferLen) then begin
        if NotifyConnectionLost then begin
          WriteLog('Connection lost downloading file: '+URLFile);
          LogError(GetLastError());
          if RunMode = rmNormal then begin
            MessageBeep(MB_ICONASTERISK);
            MessageBox(AppHandle,
                       PChar(MsgConnectionLost+URLFile),
                       PChar(MsgTitle),MB_OK);
            end;
          BufferLen := 0;  { forces exit }
          result := true;  { should force negative result }
          end;
        end;
      Inc(SizeDone, BufferLen);
      BlockWrite(iFile, Buffer, BufferLen);
      if INetCancelCopy then begin { the user pressed Cancel }
        if AskCancel then begin
          MessageBeep(MB_ICONASTERISK);
          if MessageBox(AppHandle,PChar(MsgCancelUpdate),PChar(MsgTitle),
              MB_YESNO or MB_ICONQUESTION) = IDYES then begin
            BufferLen := 0;  { forces exit }
            result := true;  { should force negative result }
            end
          else INetCancelCopy := false;
          end
        else begin
          BufferLen := 0;  { forces exit }
          result := true;
          end;
        end;
      CallBack(SizeDone,SizeFile,TimeStart);
    until (BufferLen = 0);
  finally
    CloseFile(iFile);
  end;
  result := not result;

fastexit:
  if not result then begin
    if not INetCancelCopy then begin
      WriteLog('INet download terminated due to error.');
      LogError(GetLastError());
      end;
    end;
  InternetCloseHandle(hResource);
  InternetCloseHandle(hConnect);
  InternetCloseHandle(hSession);
  INetBusy := false;
end; {INetStart}

end.
