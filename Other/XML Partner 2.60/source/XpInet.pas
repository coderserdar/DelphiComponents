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
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{* XMLPartner: XpInit.PAS                                *}
{*********************************************************}
{* XMLPartner: Internet-related utility routines         *}
{*********************************************************}
unit XpInet;

{$I XpDefine.inc}

interface

uses
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
{$IFDEF LINUX}
  Types,
  XpFtpSh,
  QDialogs,
{$ENDIF}
  classes,
  XpChrFlt,
  XpBase;

function XpLoadHTTP(const sSrcName, sUserName, sPassword : string;     {!!.58}
                          oErrors : TStringList) : TXpFileStream;
  { Load an XML file via HTTP. }

function XpLoadFTP(const sSrcName, sUserName, sPassword : string;
                         oErrors : TStringList) : TXpFileStream;
  { Load an XML file via FTP. }

function XpSaveToFTP(const sSrcName, sUserName, sPassword : string;
                     const sLocalFile : string;                        {!!.51}
                     oErrors : TStringList) : Boolean;
  { Save an XML file via FTP. }

{Begin !!.51}
function XpGenTemporaryFile : string;
  { Generate a temporary filename. }
{End !!.51}

implementation

uses
  sysutils,
  xpexcept;

{$IFDEF MSWINDOWS}
type
  {Internet-related event types}
  TXpFtpOpenFile = function(hConnect     : Pointer;
                            lpszFileName : PChar;
                            dwAccess     : DWORD;
                            dwFlags      : DWORD;
                            dwContext    : DWORD) : Pointer; stdcall;

{Begin !!.51}
  TXpFtpPutFile = function(hConnect       : Pointer;
                           lpszLocalFile  : PChar;
                           lpszRemoteFile : PChar;
                           dwFlags        : DWORD;
                           dwContext      : Pointer) : Boolean; stdcall;

  TXpHttpOpenRequest = function(hConnect          : Pointer;
                                lpszVerb          : PChar;
                                lpszObjectName    : PChar;
                                lpszVersion       : PChar;
                                lpszReferrer      : PChar;
                                lplpszAcceptTypes : PChar;
                                dwFlags           : DWORD;
                                dwContext         : DWORD) : Pointer; stdcall;

  TXpHttpSendRequest = function(hRequest         : Pointer;
                                lpszHeaders      : PChar;
                                dwHeadersLength  : DWORD;
                                lpOptional       : Pointer;
                                dwOptionalLength : DWORD) : boolean; stdcall;

  TxpFtpSetCurrentDirectory = function(hConnect      : Pointer;
                                       lpszDirectory : PChar) : boolean; stdcall;

  TXpInetCloseHandle = function(hInet : Pointer): boolean; stdcall;

  TXpInetConnect = function(hInet          : Pointer;
                            lpszServerName : PChar;
                            nServerPort    : Word;
                            lpszUsername   : PChar;
                            lpszPassword   : PChar;
                            dwService      : DWORD;
                            dwFlags        : DWORD;
                            dwContext      : DWORD) : Pointer; stdcall;

  TXpInetOpen = function(lpszAgent       : PChar;
                        dwAccessType    : DWORD;
                        lpszProxy,
                        lpszProxyBypass : PChar;
                        dwFlags         : DWORD): Pointer; stdcall;

  TXpInetQueryDataAvailable = function(hFile : Pointer;
                                   var lpdwNumberOfBytesAvailable : DWORD;
                                       dwFlags,
                                       dwContext : DWORD) : boolean; stdcall;

  TXpInetReadFile = function(hFile                 : Pointer;
                             lpBuffer              : Pointer;
                             dwNumberOfBytesToRead : DWORD;
                         var lpdwNumberOfBytesRead : DWORD) : boolean; stdcall;

  TXpInetWriteFile = function(hFile                    : Pointer;
                              lpBuffer                 : Pointer;
                              dwNumberOfBytesToWrite   : DWORD;
                          var lpdwNumberOfBytesWritten : DWORD) : boolean; stdcall;
var
  XphWininet : THandle;
    { Handle to WININET.DLL }
{$ENDIF}

{====================================================================}
function XpGenTemporaryFile : string;
{$IFDEF MSWINDOWS}
var
  PathName : array[0..MAX_PATH] of char;
begin
  GetTempPath(MAX_PATH, @PathName);
  GetTempFileName(PathName, xpsXMLPartner, 0, @PathName);
  Result := PathName;
end;
{$ENDIF}
{$IFDEF LINUX}
const
  id : integer = 1;
begin
  Result := '/tmp/xpsXMLPartner' + IntToStr( id );
  inc( id );
end;
{$ENDIF}

{--------}
{$IFDEF MSWINDOWS}
procedure XpInetInit(oErrors : TStringList);
begin
  if XphWininet = 0 then begin
    XphWininet := LoadLibrary('WININET.DLL');
    if XphWininet = 0 then
      oErrors.Add(sIENotInstalled);
  end;
end;
{$ENDIF}
{--------}
function XpLoadHTTP(const sSrcName, sUserName, sPassword : string;     {!!.58}
                          oErrors  : TStringList) : TXpFileStream;
{$IFDEF MSWINDOWS}
var
  hInter,
  hConn,
  hRequest : Pointer;
  dwTmp,
  dwSize   : DWORD;
  sServer,
  sFile    : string;
  wPos     : Integer;
  cpTmp    : PChar;
  lInternetOpen    : TXpInetOpen;
  lInternetConnect : TXpInetConnect;
  lHttpOpenRequest : TXpHttpOpenRequest;
  lHttpSendRequest : TXpHttpSendRequest;
  lInternetQueryDataAvailable : TXpInetQueryDataAvailable;
  lInternetReadFile : TXpInetReadFile;
  lInternetCloseHandle : TXpInetCloseHandle;
  FirstLoop        : Boolean;
begin
  Result := nil;
  XpInetInit(oErrors);
  if XphWininet = 0 then
    Exit;

  wPos := Pos('/', sSrcName);
  if wPos > 0 then begin
    sServer := Copy(sSrcName, 1, wPos - 1);
    sFile := Copy(sSrcName, wPos + 1, 2048);
  end else begin
    sServer := sSrcName;
    sFile := sSrcName;
  end;

  { Make Internet connection and load file }
  @lInternetCloseHandle := GetProcAddress(XphWininet,
                                          'InternetCloseHandle');
  @lInternetOpen := GetProcAddress(XphWininet, 'InternetOpenA');
  hInter := lInternetOpen(xpsXMLPartner, 0, nil, nil, 0);
  if hInter <> nil then begin
    @lInternetConnect := GetProcAddress(XphWininet,
                                        'InternetConnectA');
{Begin !!.58}
    if (sUserName <> 'anonymous') and (sPassword <> '') then
      hConn := lInternetConnect(hInter,
                                PChar(sServer),
                                0,
                                PChar(sUserName),
                                PChar(sPassword),
                                Xpc_INTERNET_SERVICE_HTTP,
                                0,
                                0)
    else
{End !!.58}
      hConn := lInternetConnect(hInter,
                                PChar(sServer),
                                0,
                                nil,
                                nil,
                                Xpc_INTERNET_SERVICE_HTTP,
                                0,
                                0);
    if hConn <> nil then begin
      @lHttpOpenRequest := GetProcAddress(XphWininet,
                                          'HttpOpenRequestA');
      hRequest := lHttpOpenRequest(hConn,
                                   'GET',
                                   PChar(sFile),
                                   Xpc_HTTP_VERSION,
                                   nil,
                                   nil,
                                   Xpc_INTERNET_FLAG_RELOAD or
                                     Xpc_INTERNET_FLAG_DONT_CACHE,
                                   0);
      if hRequest <> nil then begin
        @lHttpSendRequest := GetProcAddress(XphWininet,
                                            'HttpSendRequestA');
        if lHttpSendRequest(hRequest, nil, 0, nil, 0) then begin
          @lInternetQueryDataAvailable :=
            GetProcAddress(XphWininet,
                           'InternetQueryDataAvailable');
          FirstLoop := True;
          Result := TXpFileStream.CreateEx(fmCreate or fmShareExclusive,
                                           XpGenTemporaryFile);
          try
            while lInternetQueryDataAvailable(hRequest,
                                              dwSize,
                                              0,
                                              0) do begin
              if dwSize = 0 then
                Break;
              GetMem(cpTmp, dwSize);
              @lInternetReadFile := GetProcAddress(XphWininet,
                                                   'InternetReadFile');
              lInternetReadFile(hRequest, cpTmp, dwSize, dwTmp);
              if (FirstLoop) and
                 (Pos('404 Not Found', cpTmp) > 0) then begin
                Result.Free;
                raise EXpParserError.CreateError(0,
                                                 0,
                                                 sSrcName,
                                                 sHttpDataNotAvail);
              end;
              Result.Write(cpTmp^, dwSize);
              FreeMem(cpTmp, dwSize);
              FirstLoop := False;
            end;
          except
            Result.Free;
            raise;
          end;
        end else
          oErrors.Add(sHttpReqSendFailed);
        lInternetCloseHandle(hRequest);
      end else
        oErrors.Add(sHttpReqOpenFailed);
      lInternetCloseHandle(hConn);
    end else
      oErrors.Add(sInetConnectFailed);
    lInternetCloseHandle(hInter);
  end else
    oErrors.Add(sInetOpenFailed);
end;
{$ENDIF}
{$IFDEF LINUX}
const
  cNotFound = 'Not Found';
var
  ftp : TXpFtpShell;
  tmpName :string;
  x : integer;
begin
  ftp := TXpFtpShell.Create;
  try
    tmpName := XpGenTemporaryFile;
    Result := TXpFileStream.CreateEx( fmCreate or fmShareExclusive, tmpName );
    ftp.OutputFile := tmpName;
    ftp.SaveOutput := true;
    ftp.CaptureStdErr := true;
    ftp.Http := true;
    if( not ftp.Get( sSrcName ))then
      result := nil;
    if ftp.CommandOutput.Count <> 0 then
      for x := 0 to pred(ftp.CommandOutput.Count) do begin
         if(Pos(cNotFound, ftp.CommandOutput.Strings[x]) <> 0)then begin
                raise EXpParserError.CreateError(0,
                                                 0,
                                                 sSrcName,
                                                 sHttpDataNotAvail);
           result := nil;
         end;
      end;
  finally
    ftp.Free;
  end;
end;
{$ENDIF}

{--------}
function XpLoadFTP(const sSrcName, sUserName, sPassword : String;
                         oErrors : TStringList) : TXpFileStream;
{$IFDEF MSWINDOWS}
var
  hInter,
  hConn,
  hRequest : Pointer;
  dwTmp,
  dwSize   : DWORD;
  sServer,
  sFile,
  sDir     : string;
  wPos     : Integer;
  cpTmp    : PChar;
  lInternetOpen    : TXpInetOpen;
  lInternetConnect : TXpInetConnect;
  lInternetQueryDataAvailable : TXpInetQueryDataAvailable;
  lInternetReadFile : TXpInetReadFile;
  lInternetCloseHandle : TXpInetCloseHandle;
  lFtpSetCurrentDirectory : TxpFtpSetCurrentDirectory;
  lFtpOpenFile     : TXpFtpOpenFile;
begin
  Result := nil;
  XpInetInit(oErrors);
  if XphWininet = 0 then
    Exit;

  wPos := Pos('/', sSrcName);
  if wPos > 0 then begin
    sServer := Copy(sSrcName, 1, wPos - 1);
    sFile := Copy(sSrcName, wPos + 1, 2048);
    sDir := '';
    wPos := Pos('/', sFile);
    if wPos > 0 then begin
{Begin !!.51}
      sDir := Copy(sFile, 1, wPos);
      Delete(sFile, 1, wPos);
      wPos := Pos('/', sFile);
      while wPos > 0 do begin
        sDir := sDir + Copy(sFile, 1, wPos);
        Delete(sFile, 1, wPos);
        wPos := Pos('/', sFile);
      end;
      if sDir[Length(sDir)] = '/' then
        Delete(sDir, Length(sDir),1);
      if Length(sDir) > 0 then
        if sDir[1] <> '/' then
          sDir := '/' + sDir;
    end;
{End !!.51}
  end else begin
    oErrors.Add(sInvalidFtpLoc);
    Exit;
  end;

  { Make Internet connection and load file }
  @lInternetCloseHandle := GetProcAddress(XphWininet,
                                          'InternetCloseHandle');
  @lInternetOpen := GetProcAddress(XphWininet, 'InternetOpenA');
  hInter := lInternetOpen('TurboPower XMLPartner', 0, nil, nil, 0);
  if hInter <> nil then begin
    @lInternetConnect := GetProcAddress(XphWininet,
                                        'InternetConnectA');
    hConn := lInternetConnect(hInter,
                              PChar(sServer),
                              0,
                              PChar(sUserName),
                              PChar(sPassword),
                              Xpc_INTERNET_SERVICE_FTP,
                              0,
                              0);
    if hConn <> nil then begin
      if sDir <> '' then begin
        @lFtpSetCurrentDirectory :=
          GetProcAddress(XphWininet,
                         'FtpSetCurrentDirectoryA');
        if not lFtpSetCurrentDirectory(hConn, PChar(sDir)) then
          oErrors.Add(sInvalidFtpDir);
      end;
      @lFtpOpenFile := GetProcAddress(XphWininet, 'FtpOpenFileA');
      hRequest := lFtpOpenFile(hConn,
                               PChar(sFile),
                               GENERIC_READ,
                               Xpc_FTP_TRANSFER_TYPE_ASCII or
                                 Xpc_INTERNET_FLAG_DONT_CACHE,
                               0);
      if hRequest <> nil then begin
        @lInternetQueryDataAvailable :=
          GetProcAddress(XphWininet,
                         'InternetQueryDataAvailable');
        Result := TXpFileStream.Create(XpGenTemporaryFile,
                                       fmCreate or fmShareExclusive);
        try
          while lInternetQueryDataAvailable(hRequest,
                                            dwSize,
                                            0,
                                            0) do begin
            if dwSize = 0 then
              Break;
            GetMem(cpTmp, dwSize);
            @lInternetReadFile := GetProcAddress(XphWininet,
                                                 'InternetReadFile');
            lInternetReadFile(hRequest, cpTmp, dwSize, dwTmp);
            Result.Write(cpTmp^, dwSize);
            FreeMem(cpTmp, dwSize);
          end;
          lInternetCloseHandle(hRequest);
        except
          Result.Free;
          raise;
        end;
      end else
        oErrors.Add(sFtpOpenFileFailed);
      lInternetCloseHandle(hConn);
    end else
      oErrors.Add(sInetConnectFailed);
    lInternetCloseHandle(hInter);
  end else
    oErrors.Add(sInetOpenFailed);
end;
{$ENDIF}
{$IFDEF LINUX}
const
  cNotFound = 'cannot find';
var
  ftp : TXpFtpShell;
  tmpName :string;
  x : integer;
begin
  ftp := TXpFtpShell.Create;
  try
    tmpName := XpGenTemporaryFile;
    Result := TXpFileStream.CreateEx( fmCreate or fmShareExclusive, tmpName );
    ftp.User := sUserName;
    ftp.Password := sPassword;
    ftp.OutPutFile := tmpName;
    ftp.SaveOutput := true;
    ftp.CaptureStdErr := true;
    ftp.ftp := true;
    if( not ftp.Get( sSrcName ))then
      result := nil;
    if ftp.CommandOutput.Count <> 0 then
      for x := 0 to pred(ftp.CommandOutput.Count) do begin
         if(Pos(cNotFound, ftp.CommandOutput.Strings[x]) <> 0)then begin
                raise EXpParserError.CreateError(0,
                                                 0,
                                                 sSrcName,
                                                 sFtpDataNotAvail);
           result := nil;
         end;
      end;
  finally
    ftp.Free;
  end;
end;
{$ENDIF}
{--------}
{Begin !!.51}
function XpSaveToFTP(const sSrcName, sUserName, sPassword : string;
                     const sLocalFile : string;
                     oErrors : TStringList) : Boolean;
{$IFDEF MSWINDOWS}
var
  sFile    : string;
  sDir     : string;
  sServer  : string;
  wPos     : Integer;
  hInter   : Pointer;
  hConn    : Pointer;
  lInternetOpen           : TXpInetOpen;
  lInternetConnect        : TXpInetConnect;
  lInternetCloseHandle    : TXpInetCloseHandle;
  lFtpSetCurrentDirectory : TXpFtpSetCurrentDirectory;
  lFtpPutFile             : TXpFtpPutFile;
begin
  Result := false;
  XpInetInit(oErrors);
  if XphWininet = 0 then
    Exit;

  wPos := Pos('/', sSrcName);
  if wPos > 0 then begin
    sServer := Copy(sSrcName, 1, wPos - 1);
    sFile := Copy(sSrcName, wPos + 1, 2048);
    sDir := '';
    wPos := XpRPos('/', sFile);
    if wPos > 0 then begin
      sDir := Copy(sFile, 1, wPos - 1);
      sFile := Copy(sFile, wPos + 1, Length(sFile));
    end;
  end else begin
    oErrors.Add(sInvalidFtpLoc);
    Exit;
  end;

  { Make Internet connection and save file }
  @lInternetCloseHandle := GetProcAddress(XphWininet, 'InternetCloseHandle');
  @lInternetOpen := GetProcAddress(XphWininet, 'InternetOpenA');
  hInter := lInternetOpen('TurboPower XMLPartner', 0, nil, nil, 0);
  if Assigned(hInter) then begin
    @lInternetConnect := GetProcAddress(XphWininet, 'InternetConnectA');
    hConn := lInternetConnect(hInter, PChar(sServer), 0, PChar(sUserName),
                              PChar(sPassword), Xpc_INTERNET_SERVICE_FTP, 0, 0);
    if Assigned(hConn) then begin
      if sDir <> '' then begin
        @lFtpSetCurrentDirectory := GetProcAddress(XphWininet, 'FtpSetCurrentDirectoryA');
        if not lFtpSetCurrentDirectory(hConn, PChar(sDir)) then
          oErrors.Add(sInvalidFtpDir);
      end;
      @lFtpPutFile := GetProcAddress(XphWininet, 'FtpPutFileA');
      Result := lFtpPutFile(hConn, PChar(sLocalFile), PChar(sFile),
                            Xpc_FTP_TRANSFER_TYPE_BINARY, nil);
      if not Result then
        oErrors.Add(format(sFtpPutFileFailed, [sSrcName]));
      lInternetCloseHandle(hConn);
    end else
      oErrors.Add(sInetConnectFailed);
    lInternetCloseHandle(hInter);
  end else
    oErrors.Add(sInetOpenFailed);
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  ShowMessage('Ftp put does not work under Kylix');
  result := false;
end;
{$ENDIF}
{End !!.51}
{====================================================================}

{$IFDEF MSWINDOWS}
initialization

  XphWininet := 0;

finalization

  if XphWininet <> 0 then
    FreeLibrary(XphWininet);
{$ENDIF}
end.
