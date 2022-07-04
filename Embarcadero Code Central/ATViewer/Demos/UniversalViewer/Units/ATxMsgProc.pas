{$I ViewerOptions.inc}

unit ATxMsgProc;

interface

uses
  Windows;

function MsgCaption(N: integer): string;
function MsgString(N: integer): string; overload;
function MsgString(const Section, Key: string): string; overload;
function MsgStrip(const S: string): string;
procedure MsgInstall(sAdded, sDups: string; FromZip: boolean; Handle: THandle);
function SMsgLanguage: string;
procedure SetMsgLanguage(const LangName: string);
procedure ShowHelp(AHandle: THandle; const ATopic: string = '');
function SPluginName(const fn: string): string;


implementation

uses
  SysUtils, Forms,
  ATxIniFile, ATxSProc, ATxFProc, ATxParamStr, ATxMsg, ATViewerMsg,
  {$ifdef HELP} HtmlHlp, {$endif}
  TntForms;

var
  FLangFile,
  FLangFileEn: WideString;
  FIniFile: TATIniFile = nil;
  FIniFileEn: TATIniFile = nil;

//----------------------------------------------------------
function MsgCaption(N: integer): string;
begin
  Result:= FIniFile.ReadString('Captions', IntToStr(N), '');
  if Result = '' then
    if Assigned(FIniFileEn) then
      Result:= FIniFileEn.ReadString('Captions', IntToStr(N), '');
end;

//----------------------------------------------------------
function MsgString(N: integer): string; overload;
begin
  Result:= FIniFile.ReadString('Messages', IntToStr(N), '');
  if Result = '' then
    if Assigned(FIniFileEn) then
      Result:= FIniFileEn.ReadString('Messages', IntToStr(N), '');

  SReplaceAll(Result, '\n', #13);
  SReplaceAll(Result, '\r', #10);
  SReplaceAll(Result, '\t', #9);
end;

//----------------------------------------------------------
function MsgString(const Section, Key: string): string; overload;
begin
  Result:= FIniFile.ReadString(Section, Key, '');
  if Result = '' then
    if Assigned(FIniFileEn) then
      Result:= FIniFileEn.ReadString(Section, Key, '');
end;

//----------------------------------------------------------
function MsgStrip(const S: string): string;
begin
  Result:= S;

  //Delete '&' and '...'
  SReplaceAll(Result, '&', '');
  SReplaceAll(Result, '...', '');

  //Delete trailing ':'
  if (Result <> '') and (Result[Length(Result)] = ':') then
    SetLength(Result, Length(Result) - 1);

  //Delete digit + 2 spaces for View menu
  if Pos('  ', Result) = 2 then
    Delete(Result, 1, 3);
end;

//----------------------------------------------------------
function SMsgLanguage: string;
begin
  Result:= ChangeFileExt(ExtractFileName(FLangFile), '');
end;

//----------------------------------------------------------
procedure SetMsgLanguage(const LangName: string);
var
  FFirst: WideString;
begin
  FLangFile:= SLangFN(LangName);
  FLangFileEn:= SLangFN('English');

  if not IsFileExist(FLangFile) then
    begin 
    FFirst:= FFindFirstFile(SParamDir + '\Language', '*.lng');
    if IsFileExist(FFirst) then
      FLangFile:= FFirst;
    end;

  if IsFileExist(FLangFile) then
    begin
    if Assigned(FIniFile) then
      FreeAndNil(FIniFile);
    FIniFile:= TATIniFile.Create(FLangFile);
    end
  else
    begin
    MsgError(SFormatW(MsgViewerLangMissed, [FLangFile]));
    Application.Terminate;
    end;

  if IsFileExist(FLangFileEn) then
    begin
    if Assigned(FIniFileEn) then
      FreeAndNil(FIniFileEn);
    FIniFileEn:= TATIniFile.Create(FLangFileEn);
    end;
end;

//----------------------------------------------------------
{$ifdef HELP}
procedure ShowHelp(AHandle: THandle; const ATopic: string = '');
const
  SuffixEn = '.English';
var
  Filename, FilenameEn, Suffix: string;
begin
  Suffix:= '.' + SMsgLanguage;
  Filename:= SExtractFilePath(TntApplication.ExeName) + 'Help\Viewer' + Suffix + '.chm';
  FilenameEn:= SExtractFilePath(TntApplication.ExeName) + 'Help\Viewer' + SuffixEn + '.chm';

  if not IsFileExist(Filename) then
    Filename:= FilenameEn;

  if IsFileExist(Filename) then
    begin
    //If HTMLHELP_DYNAMIC_LINK_EXPLICIT defined in HtmlHlp.inc:
    //LoadHtmlHelp;

    if ATopic = '' then
      HtmlHelp(AHandle, PChar(Filename), HH_DISPLAY_TOC, 0)
    else
      HtmlHelp(AHandle, PChar(Filename + '::/' + ATopic), HH_DISPLAY_TOPIC, 0)
    end
  else
    MsgError(SFormatW(MsgString(101), [Filename]), AHandle);
end;

{$else}
procedure ShowHelp(AHandle: THandle; const ATopic: string = '');
begin
end;
{$endif}


function SPluginName(const fn: string): string;
begin
  if IsFileExist(fn) then
    Result:= ChangeFileExt(ExtractFileName(fn), '')
  else
    Result:= MsgViewerPluginsNameNotFound;
end;


procedure MsgInstall(sAdded, sDups: string; FromZip: boolean; Handle: THandle);
var
  Msg: string;
begin
  if sAdded = '' then sAdded:= MsgViewerPluginsNone;
  if sDups = '' then sDups:= MsgViewerPluginsNone;
  if FromZip then
    Msg:= MsgViewerPluginsInstalledZip
  else
    Msg:= MsgViewerPluginsInstalled;
  MsgInfo(Format(Msg, [sAdded, sDups]), Handle);
end;


initialization

finalization

  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);
  if Assigned(FIniFileEn) then
    FreeAndNil(FIniFileEn);

end.
