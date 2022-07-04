{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     March 2007
Version:      0.99 ALPHA CODE
Description:  TMultipartHttpDownloader is a component to download files using
              simultaneous connections to speedup download. The demo make
              also use of the TMultiProgressBar (included in ICS) which is
              a segmented progress bar.
EMail:        francois.piette@overbyte.be         http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2007 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Updates:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMultipartHttpDownloader;

{$B-}                  { Enable partial boolean evaluation   }
{$T-}                  { Untyped pointers                    }
{$X+}                  { Enable extended syntax              }
{$I OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF COMPILER2_UP}  { Not for Delphi 1                    }
    {$H+}              { Use long strings                    }
    {$J+}              { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
    Windows, Messages, SysUtils, Classes, ExtCtrls, IniFiles,
    OverbyteIcsHttpProt, OverbyteIcsUrl, OverbyteIcsWndControl;

const
    MultipartHttpDownloaderVersion = 600;
    CopyRight : String             = ' TMultipartHttpDownloader ' +
                                     '(c) 2007 F. Piette V6.00 ';

type
    TDisplayEvent             = procedure (Sender       : TObject;
                                           const Msg    : String) of object;
    TRequestDoneEvent         = procedure (Sender       : TObject;
                                           ErrorCode    : Integer;
                                           const Reason : String) of object;
    TProgressAddSegmentEvent  = procedure (Sender       : TObject;
                                           StartOffset  : Int64;
                                           ASpan        : Int64;
                                           InitPos      : Int64) of Object;
    TProgressSetPositionEvent = procedure (Sender       : TObject;
                                           Index        : Integer;
                                           Position     : Int64) of object;

    TMyHttpCli = class(THttpCli)
    protected
        FDataCount   : THttpBigInt;
        FDataMax     : THttpBigInt;
        FStartOffset : THttpBigInt;
        FEndOffset   : THttpBigInt;
        FIndex       : Integer;
        FDone        : Boolean;
    end;

    TMultipartHttpDownloader = class(TIcsWndControl)
    protected
        FHttp                  : array of TMyHttpCli;
        FContentLength         : THttpBigInt;
        FPartCount             : Integer;
        FTotalCount            : THttpBigInt;
        FPrevCount             : THttpBigInt;
        FPrevTick              : Cardinal;
        FFileStream            : TStream;
        FStartTime             : TDateTime;
        FElapsedTime           : TDateTime;
        FCurSpeed              : Double;
        FPercentDone           : Double;
        FURL                   : String;
        FUsername              : String;
        FPassword              : String;
        FProxy                 : String;
        FProxyPort             : String;
        FSocksServer           : String;
        FSocksLevel            : String;
        FSocksPort             : String;
        FServerAuth            : THttpAuthType;
        FProxyAuth             : THttpAuthType;
        FProgressCaption       : String;
        FAbortFlag             : Boolean;
        FPauseFlag             : Boolean;
        FStateFileName         : String;
        FOnDisplay             : TDisplayEvent;
        FOnRequestDone         : TRequestDoneEvent;
        FOnProgressAddSegment  : TProgressAddSegmentEvent;
        FOnProgressSetPosition : TProgressSetPositionEvent;
        FOnShowStats           : TNotifyEvent;
        FMsg_WM_START_MULTI    : UINT;
        Timer1                 : TIcsTimer;
        procedure AllocateMsgHandlers; override;
        procedure FreeMsgHandlers; override;
        function  MsgHandlersCount: Integer; override;
        procedure GetASyncRequestDone(Sender    : TObject;
                                      Request   : THttpRequest;
                                      ErrorCode : Word);
        procedure GetAsyncHeaderEnd(Sender    : TObject);
        procedure Display(const Msg : String);
        procedure WMStartMulti(var Msg: TMessage);
        procedure WndProc(var MsgRec: TMessage); override;
        procedure DownloadRequestDone(Sender    : TObject;
                                      Request   : THttpRequest;
                                      ErrorCode : Word);
        procedure DownloadDocData(Sender : TObject;
                                  Data   : Pointer;
                                  Len    : Integer);
        procedure CheckDone(ErrCode : Integer; const Reason : String);
        procedure LocationChange(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure RestartDownload(MyHttp : TMyHttpCli);
        procedure TriggerShowStats; virtual;
        procedure TriggerProgressSetPosition(Index    : Integer;
                                             Position : Int64); virtual;
        procedure TriggerProgressAddSegment(StartOffset, ASpan,
                                            InitPos: Int64); virtual;
        procedure LoadStatus;
        procedure SaveStatus;
        procedure SetStateFileName(const Value: String);
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        procedure Start;
        procedure Abort;
        procedure Pause;
        procedure Resume;
        property  TotalCount            : THttpBigInt read FTotalCount;
        property  ContentLength         : THttpBigInt read FContentLength;
        property  CurSpeed              : Double      read FCurSpeed;
        property  ElapsedTime           : TDateTime   read FElapsedTime;
        property  PercentDone           : Double      read FPercentDone;
    published
        property URL           : String            read  FURL
                                                   write FURL;
        property Username      : String            read  FUsername
                                                   write FUsername;
        property Password      : String            read  FPassword
                                                   write FPassword;
        property Proxy         : String            read  FProxy
                                                   write FProxy;
        property ProxyPort     : String            read  FProxyPort
                                                   write FProxyPort;
        property SocksServer   : String            read  FSocksServer
                                                   write FSocksServer;
        property SocksPort     : String            read  FSocksPort
                                                   write FSocksPort;
        property SocksLevel    : String            read  FSocksLevel
                                                   write FSocksLevel;
        property PartCount     : Integer           read  FPartCount
                                                   write FPartCount;
        property FileStream    : TStream           read  FFileStream
                                                   write FFileStream;
        property ServerAuth    : THttpAuthType     read  FServerAuth
                                                   write FServerAuth;
        property ProxyAuth     : THttpAuthType     read  FProxyAuth
                                                   write FProxyAuth;
        property StateFileName : String            read  FStateFileName
                                                   write SetStateFileName;
        property OnDisplay     : TDisplayEvent     read  FOnDisplay
                                                   write FOnDisplay;
        property OnRequestDone : TRequestDoneEvent read  FOnRequestDone
                                                   write FOnRequestDone;
        property OnProgressAddSegment  : TProgressAddSegmentEvent
                                                   read  FOnProgressAddSegment
                                                   write FOnProgressAddSegment;
        property OnProgressSetPosition : TProgressSetPositionEvent
                                                   read  FOnProgressSetPosition
                                                   write FOnProgressSetPosition;
        property OnShowStats           : TNotifyEvent
                                                   read  FOnShowStats
                                                   write FOnShowStats;
    end;

implementation

const
    SectionData  = 'Data';
    KeyPart      = 'Part';
    KeyUrl       = 'Url';
    KeyPartCount = 'PartCount';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMultipartHttpDownloader.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    AllocateHWnd;
    Timer1          := TIcsTimer.Create(Self);
    Timer1.Enabled  := FALSE;
    Timer1.Interval := 1000;
    Timer1.OnTimer  := Timer1Timer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMultipartHttpDownloader.Destroy;
var
    I : Integer;
begin
    for I := 0 to Length(FHttp) - 1 do
        FreeAndNil(FHttp[I]);
    SetLength(FHttp, 0);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.Display(const Msg: String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.Start;
var
    I : Integer;
begin
    if FPartCount <= 1 then
        raise ERangeError.Create('PartCount must be positive');
    FAbortFlag     := FALSE;
    FPauseFlag     := FALSE;
    FContentLength := 0;
    FTotalCount    := 0;
    FPrevCount     := 0;
    FPrevTick      := GetTickCount;
    FStartTime     := Now;
    FElapsedTime   := 0;
    TriggerShowStats;

    for I := 0 to Length(FHttp) - 1 do
        FreeAndNil(FHttp[I]);

    SetLength(FHttp, 1);
    FHttp[0]                  := TMyHttpCli.Create(Self);
    FHttp[0].FIndex           := 0;
    FHttp[0].URL              := FURL;
    FHttp[0].Username         := FUsername;
    FHttp[0].Password         := FPassword;
    FHttp[0].Proxy            := FProxy;
    FHttp[0].ProxyPort        := FProxyPort;
    FHttp[0].SocksServer      := FSocksServer;
    FHttp[0].SocksPort        := FSocksPort;
    FHttp[0].SocksLevel       := FSocksLevel;
    FHttp[0].OnRequestDone    := GetASyncRequestDone;
    FHttp[0].OnHeaderEnd      := GetAsyncHeaderEnd;
    FHttp[0].ServerAuth       := FServerAuth;
    FHttp[0].GetASync;
    Display('GetASync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.GetASyncRequestDone(
    Sender    : TObject;
    Request   : THttpRequest;
    ErrorCode : Word);
var
    HttpCli : TMyHttpCli;
begin
    Display('GetASyncRequestDone');
    if FContentLength > 0 then begin
        // We are happy with a document to get
        PostMessage(Handle, FMsg_WM_START_MULTI, 0, 0);
        Exit;
    end;

    HttpCli := Sender as TMyHttpCli;
    if ErrorCode <> 0 then begin
        Display('ErrorCode = ' + IntToStr(ErrorCode));
        if Assigned(FOnRequestDone) then
            FOnrequestDone(Self, ErrorCode, 'Download failed');
        Exit;
    end;
    if HttpCli.StatusCode <> 200 then begin
        Display('StatusCode = ' + IntToStr(HttpCli.StatusCode) + ' ' +
                HttpCli.ReasonPhrase);
        if Assigned(FOnRequestDone) then
            FOnrequestDone(Self, HttpCli.StatusCode, HttpCli.ReasonPhrase);
        Exit;
    end;
    Display('RequestDone DataCount = ' + IntToStr(HttpCli.FDataCount));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.GetAsyncHeaderEnd(Sender: TObject);
var
    HttpCli : TMyHttpCli;
begin
    HttpCli := Sender as TMyHttpCli;
    if HttpCli.ContentLength > 0 then begin
    Display('HeaderEnd ContentLength = ' + IntToStr(HttpCli.ContentLength));
        Display('HeaderEnd AcceptRanges = ' + HttpCli.AcceptRanges);
        Display('HeaderEnd DocName = ' + UrlDecode(HttpCli.DocName));
    end;
    Display('HeaderEnd StatusCode = ' + IntToStr(HttpCli.StatusCode) + ' ' +
            HttpCli.ReasonPhrase );
    if HttpCli.StatusCode <> 200 then
        Exit;
    FContentLength := HttpCli.ContentLength;
    TriggerShowStats;
    HttpCli.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.LocationChange(Sender: TObject);
var
    HttpCli : TMyHttpCli;
begin
    HttpCli := TMyHttpCli(Sender);
    Display('LocationChange = ' + HttpCli.Location);
    HttpCli.ContentRangeBegin := IntToStr(HttpCli.FStartOffset + HttpCli.FDataCount);
    HttpCli.ContentRangeEnd   := IntToStr(HttpCli.FEndOffset);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultipartHttpDownloader.MsgHandlersCount : Integer;
begin
    Result := 1 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_START_MULTI := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_START_MULTI);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.WndProc(var MsgRec: TMessage);
begin
     with MsgRec do begin
         if Msg = FMsg_WM_START_MULTI then
             WMStartMulti(MsgRec)
         else
             inherited WndProc(MsgRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.WMStartMulti(var msg: TMessage);
var
    Chunk  : THttpBigInt;
    Offset : THttpBigInt;
    I      : Integer;
    MyHttp : TMyHttpCli;
begin
    Chunk  := FContentLength div FPartCount;
    Offset := 0;
    SetLength(FHttp, FPartCount);
    for I := 0 to FPartCount - 1 do begin
        if I > 0 then begin
            // First component already created !
            FHttp[I]             := TMyHttpCli.Create(Self);
        end
        else
          FHttp[I].OnHeaderEnd := nil;

        MyHttp                   := FHttp[I];
        MyHttp.FStartOffset      := Offset;
        if I < (FPartCount - 1) then
            MyHttp.FEndOffset    := Offset + Chunk
        else
            MyHttp.FEndOffset    := FContentLength;
        Offset                   := Offset + Chunk + 1;
        MyHttp.ContentRangeBegin := IntToStr(MyHttp.FStartOffset);
        MyHttp.ContentRangeEnd   := IntToStr(MyHttp.FEndOffset);
        TriggerProgressAddSegment(MyHttp.FStartOffset,
                                  MyHttp.FEndOffset - MyHttp.FStartOffset,
                                  MyHttp.FStartOffset);
        MyHttp.FIndex            := I;
        MyHttp.URL               := FURL;
        MyHttp.Username          := FUsername;
        MyHttp.Password          := FPassword;
        MyHttp.Proxy             := FProxy;
        MyHttp.ProxyPort         := FProxyPort;
        MyHttp.SocksServer      := FSocksServer;
        MyHttp.SocksPort        := FSocksPort;
        MyHttp.SocksLevel       := FSocksLevel;
        MyHttp.OnLocationChange  := LocationChange;
        MyHttp.OnRequestDone     := DownloadRequestDone;
        MyHttp.OnDocData         := DownloadDocData;
        MyHttp.ServerAuth        := FServerAuth;
        MyHttp.FDataCount        := 0;
        MyHttp.FDone             := FALSE;
        //ListBox1.Items.Add('0');
    end;
    for I := 0 to Length(FHttp) - 1 do
        FHttp[I].GetASync;
    Timer1.Enabled := TRUE;
    Display('GetASync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.LoadStatus;
const
    Default = '0';
var
    SectionNames  : TStringList;
    Cnt           : Integer;
    I             : Integer;
    MyHttp        : TMyHttpCli;
    IniFile       : TIniFile;
    //Dls           : TDownloadState;
begin
    for I := 0 to Length(FHttp) - 1 do
        FreeAndNil(FHttp[I]);
    SetLength(FHttp, 0);

    FTotalCount := 0;
    IniFile     := TIniFile.Create(FStateFileName);
    try
        //FFileName := IniFile.ReadString('GLOBAL', 'FileName', '');
        FContentLength := StrToInt64Def(IniFile.ReadString('GLOBAL', 'ContentLength', Default), 0);
        FUrl           := IniFile.ReadString('GLOBAL', 'Url', '');
        FPartCount     := IniFile.ReadInteger('GLOBAL', 'PartCount', 1);
        SetLength(FHttp, FPartCount);
        if FPartCount > 0 then begin
            SectionNames := TStringList.Create;
            try
                IniFile.ReadSections(SectionNames);
                for I := 0 to SectionNames.Count - 1 do begin
                    if Copy(SectionNames[I], 1, 5) <> 'PART_' then
                        Continue;
                    Cnt                     := StrToInt(Copy(SectionNames[I], 6, 8));
                    FHttp[Cnt]              := TMyHttpCli.Create(Self);
                    MyHttp                  := FHttp[Cnt];
                    MyHttp.FIndex           := Cnt;
                    MyHttp.FStartOffset     := StrToInt64Def(IniFile.ReadString(SectionNames[I], 'StartOffset', Default), 0);
                    MyHttp.FEndOffset       := StrToInt64Def(IniFile.ReadString(SectionNames[I], 'EndOffset',   Default), 0);
                    MyHttp.FDataCount       := StrToInt64Def(IniFile.ReadString(SectionNames[I], 'DataCount',   Default), 0);
//                  MyHttp.FDone            := IniFile.ReadBool(SectionNames[I], 'Done', FALSE);
                    MyHttp.URL              := IniFile.ReadString(SectionNames[I], 'URL', '');
                    MyHttp.OnLocationChange := LocationChange;
                    MyHttp.OnRequestDone    := DownloadRequestDone;
                    MyHttp.OnDocData        := DownloadDocData;
                    MyHttp.ServerAuth       := FServerAuth;
                    Inc(FTotalCount, MyHttp.FDataCount);
                end;
                FPrevCount    := FTotalCount;
                FPrevTick     := GetTickCount;
                FStartTime    := Now;
                FElapsedTime  := 0;
            finally
                FreeAndNil(SectionNames);
            end;
        end;
    finally
        FreeAndNil(IniFile);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.SaveStatus;
var
    SectionNames  : TStringList;
    I             : Integer;
    IniFile       : TIniFile;
begin
    IniFile := TIniFile.Create(FStateFileName);
    try
        SectionNames := TStringList.Create;
        try
            IniFile.ReadSections(SectionNames);
            for I := 0 to SectionNames.Count - 1 do begin
                if Copy(SectionNames[I], 1, 5) = 'PART_' then
                    IniFile.EraseSection(SectionNames[I]);
            end;
        finally
            FreeAndNil(SectionNames);
        end;
        IniFile.EraseSection('GLOBAL');
        IniFile.WriteString('GLOBAL',  'Url', FUrl);
        IniFile.WriteString('GLOBAL',  'ContentLength', IntToStr(FContentLength));
        IniFile.WriteInteger('GLOBAL', 'PartCount', Length(FHttp));
        for I := 0 to Length(FHttp) - 1 do begin
            IniFile.WriteString('PART_' + IntToStr(I), 'StartOffset', IntToStr(FHttp[I].FStartOffset));
            IniFile.WriteString('PART_' + IntToStr(I), 'EndOffset', IntToStr(FHttp[I].FEndOffset));
            IniFile.WriteString('PART_' + IntToStr(I), 'DataCount', IntToStr(FHttp[I].FDataCount));
//            IniFile.WriteBool('PART_' + IntToStr(I), 'Done', FHttp[I].FDone);
            IniFile.WriteString('PART_' + IntToStr(I), 'URL', FHttp[I].URL);
            FHttp[I].FDone := (FHttp[I].FDataCount >= (FHttp[I].FEndOffset - FHttp[I].FStartOffset));
        end;
    finally
        FreeAndNil(IniFile);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.DownloadRequestDone(
    Sender    : TObject;
    Request   : THttpRequest;
    ErrorCode : Word);
var
    HttpCli   : TMyHttpCli;
    ErrCode   : Integer;
    Reason    : String;
begin
    HttpCli       := Sender as TMyHttpCli;
    HttpCli.FDone := TRUE;
    if FAbortFlag then begin
        // We are aborting transfert, just ignore any error
        Display('Download done index = ' + IntToStr(HttpCli.FIndex) + ' Aborted');
        ErrCode := 503;  // 503 is service unavailable
        Reason  := 'Transfert aborted';
    end
    else if FPauseFlag then begin
        // We are aborting transfert, just ignore any error
        Display('Download done index = ' + IntToStr(HttpCli.FIndex) + ' Paused');
        ErrCode := 204;
        Reason  := 'Transfert paused';
    end
    else begin
        if ErrorCode <> 0 then begin
            Display('Download done index = ' + IntToStr(HttpCli.FIndex) +
                    '  ErrorCode = ' + IntToStr(ErrorCode));
            RestartDownload(HttpCli);
            Exit;
        end
        else if HttpCli.StatusCode <> 206 then begin
            Display('Download done index = ' + IntToStr(HttpCli.FIndex) +
                    '  Status = ' + IntToStr(HttpCli.StatusCode));
            RestartDownload(HttpCli);
            Exit;
        end;
        Display('Download done index = ' + IntToStr(HttpCli.FIndex) + ' OK');
        ErrCode := 200;
        Reason  := 'OK';
    end;
    CheckDone(ErrCode, Reason);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.CheckDone(
    ErrCode      : Integer;
    const Reason : String);
var
    Done : Boolean;
    I    : Integer;
begin
    Done := TRUE;
    for I := 0 to Length(FHttp) - 1 do
        Done := Done and (FHttp[I].FDone);
    if Done then begin
        Timer1.Enabled := FALSE;
        Timer1.OnTimer(nil);
        Display('All done');
        if FPauseFlag then
            SaveStatus;
        if Assigned(FOnRequestDone) then
            FOnrequestDone(Self, ErrCode, Reason);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.DownloadDocData(
    Sender : TObject;
    Data   : Pointer;
    Len    : Integer);
var
    HttpCli : TMyHttpCli;
begin
    if Len <= 0 then
        Exit;
    if FPauseFlag then
        Exit;
    HttpCli            := Sender as TMyHttpCli;
    FFilestream.Seek(HttpCli.FStartOffset + HttpCli.FDataCount,
                     soFromBeginning);
    FFilestream.WriteBuffer(Data^, Len);
    HttpCli.FDataCount := HttpCli.FDataCount + Len;
    FTotalCount        := FTotalCount + Len;
    TriggerProgressSetPosition(HttpCli.FIndex,
                               HttpCli.FStartOffset + HttpCli.FDataCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.TriggerProgressSetPosition(
    Index    : Integer;
    Position : Int64);
begin
    if Assigned(FOnProgressSetPosition) then
        FOnProgressSetPosition(Self, Index, Position);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.TriggerProgressAddSegment(
    StartOffset : Int64;
    ASpan       : Int64;
    InitPos     : Int64);
begin
    if Assigned(FOnProgressAddSegment) then
        FOnProgressAddSegment(Self, StartOffset, ASpan, InitPos);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.TriggerShowStats;
begin
    if Assigned(FOnShowStats) then
        FOnShowStats(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.Timer1Timer(Sender: TObject);
var
    HttpCli : TMyHttpCli;
    I       : Integer;
    Done    : Boolean;
    Tick    : Cardinal;
begin
    Done := TRUE;
    for I := 0 to Length(FHttp) - 1 do begin
        HttpCli := FHttp[I];
        Done    := Done and (HttpCli.FDone);
        //MultipartDownloadForm.ListBox1.Items[HttpCli.FIndex] := IntToStr(HttpCli.FDataCount);
    end;

    Tick         := GetTickCount;
    FCurSpeed    := 8 * (FTotalCount - FPrevCount) / (Tick - FPrevTick);
    FElapsedTime := Now - FStartTime;
    if FContentLength = 0 then
        FPercentDone := 0
    else
        FPercentDone := 100.0 * FTotalCount / FContentLength;
    TriggerShowStats;
    FPrevTick    := Tick;
    FPrevCount   := FTotalCount;
    if not Done then
        Exit;

    // Download is finished
    Timer1.Enabled := FALSE;
    FCurSpeed := 8 * FTotalCount / (FElapsedTime * 86400000);
    TriggerShowStats;
    FreeAndNil(FFilestream);
    Display('Done');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.RestartDownload(MyHttp : TMyHttpCli);
begin
    Display('Restarting ' + IntToStr(MyHttp.FIndex));
    TriggerProgressSetPosition(MyHttp.FIndex, MyHttp.FStartOffset);
    MyHttp.ContentRangeBegin := IntToStr(MyHttp.FStartOffset);
    MyHttp.ContentRangeEnd   := IntToStr(MyHttp.FEndOffset);
    MyHttp.Username          := FUsername;
    MyHttp.Password          := FPassword;
    MyHttp.Proxy             := FProxy;
    MyHttp.ProxyPort         := FProxyPort;
    MyHttp.SocksServer       := FSocksServer;
    MyHttp.SocksPort         := FSocksPort;
    MyHttp.SocksLevel        := FSocksLevel;
    MyHttp.ServerAuth        := FServerAuth;
    MyHttp.FDataCount        := 0;
    MyHttp.FDone             := FALSE;
    MyHttp.GetASync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.Abort;
var
    HttpCli : TMyHttpCli;
    I       : Integer;
begin
    FAbortFlag := TRUE;
    for I := 0 to Length(FHttp) - 1 do begin
        HttpCli := FHttp[I];
        HttpCli.Abort;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.Pause;
var
    HttpCli : TMyHttpCli;
    I       : Integer;
begin
    if StateFileName = '' then
        raise Exception.Create('TMultipartHttpDownloader.Pause: ' +
                               'No file name specified');
    FPauseFlag := TRUE;
    for I := 0 to Length(FHttp) - 1 do begin
        HttpCli := FHttp[I];
        HttpCli.Abort;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.Resume;
var
    HttpCli : TMyHttpCli;
    I       : Integer;
begin
    if StateFileName = '' then
        raise Exception.Create('TMultipartHttpDownloader.Resume: ' +
                               'No file name specified');

    FAbortFlag     := FALSE;
    FPauseFlag     := FALSE;
    LoadStatus;
    for I := 0 to Length(FHttp) - 1 do begin
        HttpCli                   := FHttp[I];
        HttpCli.Username          := FUsername;
        HttpCli.Password          := FPassword;
        HttpCli.Proxy             := FProxy;
        HttpCli.ProxyPort         := FProxyPort;
        HttpCli.SocksServer       := FSocksServer;
        HttpCli.SocksPort         := FSocksPort;
        HttpCli.SocksLevel        := FSocksLevel;
        HttpCli.ServerAuth        := FServerAuth;
        HttpCli.ContentRangeBegin := IntToStr(HttpCli.FStartOffset +
                                              HttpCli.FDataCount);
        HttpCli.ContentRangeEnd   := IntToStr(HttpCli.FEndOffset);
        Display('Resuming ' + IntToStr(I) +
                ' Offset=' + HttpCli.ContentRangeBegin +
                ' DataCount=' + IntToStr(HttpCli.FDataCount));
        TriggerProgressAddSegment(HttpCli.FStartOffset,
                                  HttpCli.FEndOffset - HttpCli.FStartOffset,
                                  HttpCli.FStartOffset + HttpCli.FDataCount);
    end;
    // Start all download which is not done yet
    for I := 0 to Length(FHttp) - 1 do begin
        if not FHttp[I].FDone then
            FHttp[I].GetASync;
    end;
    Timer1.Enabled := TRUE;
    CheckDone(0, 'OK');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloader.SetStateFileName(const Value: String);
begin
    // We use TIniFile which create a file with no path into Windows directory
    // Add the current directory specifier to the filename if required.
    if ExtractFilePath(Value) = '' then
        FStateFileName := '.\' + Value
    else
        FStateFileName := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
