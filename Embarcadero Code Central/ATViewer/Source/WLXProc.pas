{**************************************************}
{                                                  }
{  WLXProc - Total Commander Lister API Wrapper    }
{  Copyright (C) 2006-2008 Alexey Torgashin        }
{  http://atorg.net.ru                             }
{  support@uvviewsoft.com                          }
{                                                  }
{**************************************************}

{$BOOLEVAL OFF} //Short boolean evaluation required.

{$I ATViewerOptions.inc} //ATViewer options.

unit WLXProc;

interface

uses
  Windows, Messages, SysUtils, Controls,
  {$ifdef WLX_FORM} Forms, {$endif}
  WLXPlugin;

const
  WlxPluginsMaxCount = 200;

type
  TListLoad = function(ParentWin: THandle; FileToLoad: PAnsiChar; ShowFlags: Integer): THandle; stdcall;
  TListLoadNext = function(ParentWin, PluginWin: THandle; FileToLoad: PAnsiChar;
                           ShowFlags: Integer): Integer; stdcall;
  TListCloseWindow = procedure(ListWin: THandle); stdcall;
  TListSetDefaultParams = procedure(dps: pListDefaultParamStruct); stdcall;
  TListGetDetectString = procedure(DetectString: PAnsiChar; maxlen: Integer); stdcall;
  TListSendCommand = function(ListWin: THandle; Command, Parameter: Integer): Integer; stdcall;
  TListPrint = function(ListWin: THandle; FileToPrint, DefPrinter: PAnsiChar;
                        PrintFlags: Integer; const {was var!} Margins: TRect): Integer; stdcall;
  TListSearchText = function(ListWin: THandle; SearchString: PAnsiChar;
                             SearchParameter: Integer): Integer; stdcall;
  TListSearchDialog = function(ListWin: THandle; FindNext: Integer): Integer; stdcall;

type
  TWlxFileName = string; //No support for WideString
  TWlxDetectString = string;
  TWlxNameEvent = procedure(const APluginName: string) of object;

  TWlxPluginRecord = record
    FileName: TWlxFileName;
    DetectStr: TWlxDetectString;
    HLib: THandle;
    HWnd: THandle;
    ListLoad: TListLoad;
    ListLoadNext: TListLoadNext;
    ListCloseWindow: TListCloseWindow;
    ListSetDefaultParams: TListSetDefaultParams;
    ListSendCommand: TListSendCommand;
    ListPrint: TListPrint;
    ListSearchText: TListSearchText;
    ListSearchDialog: TListSearchDialog;
  end;

  TWlxPlugins = class
  private
    FPlugins: array[1 .. WlxPluginsMaxCount] of TWlxPluginRecord;
    FCount: Word;
    FActive: Word;
    FActiveFileName: AnsiString;
    FActivePosPercent: Word;
    FActiveForceMode: Boolean;
    FParent: TWinControl;
    FIniFileName: AnsiString;
    {$ifdef WLX_FORM}
    FTCWindow: TForm;
    {$endif}
    FFocused: Boolean;
    FBeforeLoading: TWlxNameEvent;
    FAfterLoading: TWlxNameEvent;
    function IsIndexValid(N: Word): Boolean;
    function Present(const AFileName: TWlxFileName): Boolean;
    function Load(N: Word): Boolean;
    procedure Unload(N: Word);
    function OpenWnd(N: Word; const AFileName: AnsiString; AFlags: Integer): Boolean;
    function ReopenWnd(const AFileName: AnsiString; AFlags: Integer): Boolean;
    procedure CloseWnd(N: Word);
    procedure ResizeWnd(N: Word; const ARect: TRect);
    procedure SendCommandWnd(N: Word; ACmd, AParam: Integer);
    procedure SetActivePosPercent(AValue: Word);
    function GetName(N: Word): AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure InitParams(AParent: TWinControl; const AIniFileName: AnsiString);
    function AddPlugin(const AFileName: TWlxFileName; const ADetectStr: TWlxDetectString): Boolean;
    function GetPlugin(N: Word; var AFileName: TWlxFileName; var ADetectStr: TWlxDetectString): Boolean;
    procedure ShowDebugInfo;
    function OpenMatched(const AFileName: WideString; AFit, AFitLargeOnly, ACenter, ATextWrap, AAnsiCP, ANewFile: Boolean): Boolean;
    procedure CloseActive;
    procedure ResizeActive(const ARect: TRect);
    procedure SendCommandToActive(ACmd, AParam: Integer);
    procedure SendParamsToActive(AFit, AFitLargeOnly, ACenter, ATextWrap, AAnsiCP: Boolean);
    function SendMessageToActive(const AMessage: TMessage): LRESULT;
    function PrintActive(const ARect: TRect): Boolean;
    function SearchActive(const AString: AnsiString; AFindFirst, AWholeWords, ACaseSens, ABackwards: Boolean): Boolean;
    function SearchDialogActive(AFindNext: Boolean): Boolean;
    function GetActiveName: AnsiString;
    procedure SetFocusToActive;
    property ActivePosPercent: Word read FActivePosPercent write SetActivePosPercent;
    function ActiveSupportsSearch: Boolean;
    function ActiveSupportsPrint: Boolean;
    function ActiveSupportsCommands: Boolean;
    function ActiveWindowHandle: THandle;
    property IsFocused: Boolean read FFocused write FFocused;
    property OnBeforeLoading: TWlxNameEvent read FBeforeLoading write FBeforeLoading;
    property OnAfterLoading: TWlxNameEvent read FAfterLoading write FAfterLoading;
  end;

function WlxGetDetectString(const AFileName: TWlxFileName): TWlxDetectString;


implementation

uses
  ATxSProc, ATxFProc, ATViewerMsg;

{ Not published Lister API constants }

const
  cWlxFindFirst = 0;
  cWlxFindNext = 1;


{ Helper fake TC window }

{$ifdef WLX_FORM}

{$R WLXProc_FormTC.dfm}

type
  TTOTAL_CMD = class(TForm)
  public
  end;

{$endif}


{ Helper functions }

// Currently we use simplified detect-string_ checking:
//
// 1) If string_ is empty (i.e. it doesn't contain 'EXT=' part), it's matched.
// 2) If it's not empty, it's matched when corresponding 'EXT=' part is present.
// 3) 'FORCE' special word is supported now. But it is expected only in its special
//    meaning, i.e. not as a part of file extension.
//
// To-do: add full parsing of detect-strings.
//

function WlxDetectMatch(const AFileName: string; const ADetectStr: TWlxDetectString; AForceMode: Boolean): Boolean;
var
  FN, Str, Ext: AnsiString;
  Empty, ExtMatch, ForceMatch: Boolean;
begin
  //Delete last slash from folder names:
  FN := AFileName;
  SDelLastSlash(FN);

  Str := UpperCase(ADetectStr);
  Ext := UpperCase(ExtractFileExt(FN));
  Delete(Ext, 1, 1);

  Empty := Pos('EXT=', Str) = 0;
  ExtMatch := Pos(Format('EXT="%s"', [Ext]), Str) > 0;
  ForceMatch := Pos('FORCE', Str) > 0;

  if AForceMode and ForceMatch then
    begin Result := True; Exit end;

  if Empty then 
    begin Result := True; Exit end;

  Result := ExtMatch;
end;


procedure MsgErrorWlx(const AFileName: TWlxFileName; const AFuncName: AnsiString);
begin
  MsgError(Format(MsgViewerWlxException, [ExtractFileName(AFileName), AFuncName]));
end;


function WlxGetDetectString(const AFileName: TWlxFileName): TWlxDetectString;
const
  cBufSize = 4 * 1024;
var
  HLib: THandle;
  Buffer: array[0 .. Pred(cBufSize)] of AnsiChar;
  ListGetDetectString: TListGetDetectString;
begin
  Result := '';

  HLib := LoadLibraryA(PAnsiChar(AnsiString(AFileName)));
  if HLib <> 0 then
  begin
    ListGetDetectString := GetProcAddress(HLib, 'ListGetDetectString');

    if Assigned(ListGetDetectString) then
      try
        FillChar(Buffer, SizeOf(Buffer), 0);
        ListGetDetectString(Buffer, SizeOf(Buffer));
        Result := AnsiString(Buffer);
      except
        MsgErrorWlx(AFileName, 'ListGetDetectString');
      end;

    FreeLibrary(HLib);
  end;
end;

procedure InitPluginRecord(var Rec: TWlxPluginRecord);
begin
  with Rec do
  begin
    HLib := 0;
    HWnd := 0;
    ListLoad := nil;
    ListLoadNext := nil;
    ListCloseWindow := nil;
    ListSetDefaultParams := nil;
    ListSendCommand := nil;
    ListPrint := nil;
    ListSearchText := nil;
    ListSearchDialog := nil;
  end;
end;

function ParamsToFlags(AFit, AFitLargeOnly, ACenter, ATextWrap, AAnsiCP, AForceMode: Boolean): Integer;
begin
  Result := 0;

  if AFit then
    Inc(Result, lcp_fittowindow);

  if AFit and AFitLargeOnly then
    Inc(Result, lcp_fitlargeronly);

  if ACenter then
    Inc(Result, lcp_center);

  if ATextWrap then
    Inc(Result, lcp_wraptext);

  if AAnsiCP then
    Inc(Result, lcp_ansi)
  else
    Inc(Result, lcp_ascii);

  if AForceMode then
    Inc(Result, lcp_forceshow);
end;


{ Helper cracker class }

type
  TWinControlCracker = class(TWinControl);


{ TWlxPlugins }

constructor TWlxPlugins.Create;
begin
  inherited Create;

  FillChar(FPlugins, SizeOf(FPlugins), 0);
  FCount := 0;
  FActive := 0;
  FActiveFileName := '';
  FActivePosPercent := 0;
  FActiveForceMode := False;
  FParent := nil;
  FIniFileName := '';

  {$ifdef WLX_FORM}
  FTCWindow := nil;
  {$endif}

  FFocused := False;
  FBeforeLoading := nil;
  FAfterLoading := nil;
end;

destructor TWlxPlugins.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TWlxPlugins.Clear;
var
  i: Word;
begin
  CloseActive;

  for i := FCount downto 1 do
    with FPlugins[i] do
    begin
      FileName := '';
      DetectStr := '';
    end;

  FCount := 0;
  FActive := 0;
  FActiveFileName := '';
  FActivePosPercent := 0;
  FActiveForceMode := False;
end;

function TWlxPlugins.IsIndexValid(N: Word): Boolean;
begin
  Result := (N > 0) and (N <= FCount);
end;

function TWlxPlugins.Present(const AFileName: TWlxFileName): Boolean;
var
  i: Word;
begin
  Result := False;
  for i := 1 to FCount do
    if UpperCase(AFileName) = UpperCase(FPlugins[i].FileName) then
      begin Result := True; Break end;
end;

function TWlxPlugins.AddPlugin(const AFileName: TWlxFileName; const ADetectStr: TWlxDetectString): Boolean;
begin
  Result := (FCount < WlxPluginsMaxCount) and (not Present(AFileName));
  if Result then
  begin
    Inc(FCount);
    with FPlugins[FCount] do
    begin
      FileName := AFileName;
      DetectStr := ADetectStr;
      SReplaceAll(DetectStr, ' = ', '='); //Some plugins give string_ with 'EXT = "extension"'
      InitPluginRecord(FPlugins[FCount]);
    end;
  end;
end;

function TWlxPlugins.GetPlugin(N: Word; var AFileName: TWlxFileName; var ADetectStr: TWlxDetectString): Boolean;
begin
  Result := IsIndexValid(N);
  if Result then
  begin
    AFileName := FPlugins[N].FileName;
    ADetectStr := FPlugins[N].DetectStr;
  end
  else
  begin
    AFileName := '';
    ADetectStr := '';
  end;
end;

procedure TWlxPlugins.InitParams(AParent: TWinControl; const AIniFileName: AnsiString);
begin
  FParent := AParent;
  FIniFileName := AIniFileName;
end;

procedure TWlxPlugins.Unload(N: Word);
begin
  if not IsIndexValid(N) then Exit;

  with FPlugins[N] do
    if HLib <> 0 then
    begin
      CloseWnd(N);
      {$ifdef WLX_UNLOAD}
      FreeLibrary(HLib);
      InitPluginRecord(FPlugins[N]);
      {$endif}
    end;

  {$ifdef WLX_FORM}
  if Assigned(FTCWindow) then
  begin
    FTCWindow.Release;
    FTCWindow := nil;
  end;
  {$endif}
end;

function TWlxPlugins.Load(N: Word): Boolean;
var
  dps: TListDefaultParamStruct;
begin
  Result := False;
  if not IsIndexValid(N) then Exit;

  {$ifdef WLX_FORM}
  if not Assigned(FTCWindow) then
  begin
    FTCWindow := TTOTAL_CMD.Create(nil);
    FTCWindow.Visible := False;
  end;
  {$endif}

  with FPlugins[N] do
  begin
    if HLib <> 0 then
      begin Result := True; Exit end;

    Unload(N);
    
    HLib := LoadLibraryA(PAnsiChar(FileName));
    if HLib = 0 then Exit;

    ListLoad := GetProcAddress(HLib, 'ListLoad');
    ListLoadNext := GetProcAddress(HLib, 'ListLoadNext');
    ListCloseWindow := GetProcAddress(HLib, 'ListCloseWindow');
    ListSetDefaultParams := GetProcAddress(HLib, 'ListSetDefaultParams');
    ListSendCommand := GetProcAddress(HLib, 'ListSendCommand');
    ListPrint := GetProcAddress(HLib, 'ListPrint');
    ListSearchText := GetProcAddress(HLib, 'ListSearchText');
    ListSearchDialog := GetProcAddress(HLib, 'ListSearchDialog');

    if Assigned(ListSetDefaultParams) then
      try
        FillChar(dps, SizeOf(dps), 0);
        dps.Size := SizeOf(dps);
        dps.PluginInterfaceVersionLow := 50;
        dps.PluginInterfaceVersionHi := 1;
        lstrcpyA(dps.DefaultIniName, PAnsiChar(FIniFileName));
        ListSetDefaultParams(@dps);
      except
        MsgErrorWlx(FileName, 'ListSetDefaultParams');
        Exit;
      end;
  end;

  Result := True;
end;

procedure TWlxPlugins.CloseWnd(N: Word);
begin
  if IsIndexValid(N) then
    with FPlugins[N] do
      if HLib <> 0 then
        if HWnd <> 0 then
        begin
          try
            if Assigned(ListCloseWindow) then
              ListCloseWindow(HWnd)
            else
              DestroyWindow(HWnd);
          except
            MsgErrorWlx(FileName, 'ListCloseWindow/DestroyWindow');
          end;
          HWnd := 0;
        end;
end;

function TWlxPlugins.OpenWnd(N: Word; const AFileName: AnsiString; AFlags: Integer): Boolean;
begin
  Result := False;
  if not IsIndexValid(N) then Exit;

  CloseWnd(N);

  if Assigned(FBeforeLoading) then
    FBeforeLoading(GetName(N));

  with FPlugins[N] do
    if Assigned(ListLoad) then
      try
        HWnd := ListLoad(FParent.Handle, PAnsiChar(AFileName), AFlags);
        Result := HWnd <> 0;
        if Result then
        begin
          FActive := N;
          FActiveFileName := AFileName;
          FActivePosPercent := 0;

          SetParent(HWnd, FParent.Handle);

          if FFocused then
            SetFocus(HWnd);

          if Assigned(TWinControlCracker(FParent).OnResize) then
            TWinControlCracker(FParent).OnResize(FParent);
        end;
      except
        MsgErrorWlx(FileName, 'ListLoad');
      end;

  if Assigned(FAfterLoading) then
    FAfterLoading(GetName(N));
end;

function TWlxPlugins.ReopenWnd(const AFileName: AnsiString; AFlags: Integer): Boolean;
begin
  Result := False;

  if IsIndexValid(FActive) then
    with FPlugins[FActive] do
      if Assigned(ListLoadNext) and
        WlxDetectMatch(AFileName, DetectStr, False{ForceMode}) and
        (ListLoadNext(FParent.Handle, HWnd, PAnsiChar(AFileName), AFlags) = LISTPLUGIN_OK) then
          Result := True;
end;

procedure TWlxPlugins.ResizeWnd(N: Word; const ARect: TRect);
begin
  if IsIndexValid(N) then
    with FPlugins[N] do
      if (HLib <> 0) and (HWnd <> 0) then
        with ARect do
          MoveWindow(HWnd, Left, Top, Right - Left, Bottom - Top, True{bRepaint});
end;

procedure TWlxPlugins.SendCommandWnd(N: Word; ACmd, AParam: Integer);
begin
  if IsIndexValid(N) then
    with FPlugins[N] do
      if (HLib <> 0) and (HWnd <> 0) then
        if Assigned(ListSendCommand) then
          try
            ListSendCommand(HWnd, ACmd, AParam);
          except
            MsgErrorWlx(FileName, 'ListSendCommand');
          end;
end;

procedure TWlxPlugins.ResizeActive(const ARect: TRect);
begin
  ResizeWnd(FActive, ARect);
end;

procedure TWlxPlugins.CloseActive;
begin
  Unload(FActive);
  FActive := 0;
  FActiveFileName := '';
  FActivePosPercent := 0;
end;

procedure TWlxPlugins.SendCommandToActive(ACmd, AParam: Integer);
begin
  SendCommandWnd(FActive, ACmd, AParam);
  SetFocusToActive;
end;

procedure TWlxPlugins.SendParamsToActive(AFit, AFitLargeOnly, ACenter, ATextWrap, AAnsiCP: Boolean);
begin
  SendCommandToActive(lc_newparams,
    ParamsToFlags(AFit, AFitLargeOnly, ACenter, ATextWrap, AAnsiCP, FActiveForceMode));
end;

procedure TWlxPlugins.SetActivePosPercent(AValue: Word);
begin
  FActivePosPercent := AValue;
  if FActivePosPercent > 100 then
    FActivePosPercent := 100;
end;

function TWlxPlugins.SendMessageToActive(const AMessage: TMessage): LRESULT;
begin
  Result := 0;
  if IsIndexValid(FActive) then
    with FPlugins[FActive] do
      Result := SendMessage(HWnd, AMessage.Msg, AMessage.WParam, AMessage.LParam);
end;


function TWlxPlugins.PrintActive(const ARect: TRect): Boolean;
begin
  Result := True;
  if IsIndexValid(FActive) and (FActiveFileName <> '') then
    with FPlugins[FActive] do
      if Assigned(ListPrint) then
        try
          Result := ListPrint(HWnd, PAnsiChar(FActiveFileName), nil, 0, ARect) = LISTPLUGIN_OK;
        except
          MsgErrorWlx(FileName, 'ListPrint');
        end;
end;

function TWlxPlugins.SearchActive(const AString: AnsiString; AFindFirst, AWholeWords, ACaseSens, ABackwards: Boolean): Boolean;
var
  AFlags: Integer;
begin
  Result := True;
  if IsIndexValid(FActive) then
    with FPlugins[FActive] do
      if Assigned(ListSearchText) then
        try
          AFlags := 0;
          if AFindFirst then Inc(AFlags, lcs_findfirst);
          if AWholeWords then Inc(AFlags, lcs_wholewords);
          if ACaseSens then Inc(AFlags, lcs_matchcase);
          if ABackwards then Inc(AFlags, lcs_backwards);
          Result := ListSearchText(HWnd, PAnsiChar(AString), AFlags) = LISTPLUGIN_OK;
        except
          MsgErrorWlx(FileName, 'ListSearchText');
        end;
end;

function TWlxPlugins.SearchDialogActive(AFindNext: Boolean): Boolean;
const
  cModes: array[Boolean] of Integer = (cWlxFindFirst, cWlxFindNext);
begin
  Result := False;
  if IsIndexValid(FActive) then
    with FPlugins[FActive] do
      if Assigned(ListSearchDialog) then
        try
          Result := ListSearchDialog(HWnd, cModes[AFindNext]) = LISTPLUGIN_OK;
        except
          MsgErrorWlx(FileName, 'ListSearchDialog');
        end;
end;

function TWlxPlugins.GetName(N: Word): AnsiString;
begin
  Result := '';
  if IsIndexValid(N) then
    Result := ChangeFileExt(ExtractFileName(FPlugins[N].FileName), '');
end;

function TWlxPlugins.GetActiveName: AnsiString;
begin
  Result := GetName(FActive);
end;

procedure TWlxPlugins.SetFocusToActive;
begin
  if IsIndexValid(FActive) then
    with FPlugins[FActive] do
      if HWnd <> 0 then
        if GetFocus <> HWnd then
          SetFocus(HWnd);
end;

function TWlxPlugins.OpenMatched(
  const AFileName: WideString;
  AFit, AFitLargeOnly, ACenter, ATextWrap, AAnsiCP, ANewFile: Boolean): Boolean;
var
  OldActive, i: Word;
  AFlags: Integer;
  fn: AnsiString;
begin
  Result := False;
  if FCount = 0 then Exit;

  if not Assigned(FParent) then
    begin MsgError(MsgViewerWlxParentNotSpecified); Exit end;


  //Convert Unicode name to plugin-acceptable form

  fn := FFileNameWideToAnsi(AFileName);
  if fn = '' then Exit;
  
  //Try to load file in active plugin (TC 7 feature)

  FActiveForceMode := not ANewFile;
  AFlags := ParamsToFlags(AFit, AFitLargeOnly, ACenter, ATextWrap, AAnsiCP, FActiveForceMode);

  if ANewFile and ReopenWnd(fn, AFlags) then
    begin Result := True; Exit end;


  //Calculate OldActive: plugin to start cycling from

  if FActive = 0 then
    OldActive := 0
  else
  begin
    if not ANewFile then
      OldActive := FActive
    else
      OldActive := FActive - 1;
  end;

  CloseActive;

  //Cycling through all plugins from OldActive

  i := OldActive;
  repeat
    Inc(i);

    if i > FCount then i := 1;
    if i > FCount then Break;

    with FPlugins[i] do
    begin
      //1. Test for DetectString
      if WlxDetectMatch(fn, DetectStr, FActiveForceMode) then
       //2. Load
       if Load(i) then
        //3. Test for opening
        if OpenWnd(i, fn, AFlags) then
          Break
        else
          Unload(i);
    end;

    //Stop cycling at last plugin, if OldActive=0
    if (i = FCount) and (OldActive = 0) then Break;

    //Stop cycling at OldActive plugin
    if i = OldActive then Break;
  until False;

  Result := IsIndexValid(FActive);
end;

procedure TWlxPlugins.ShowDebugInfo;
var
  S: AnsiString;
  i: Word;
begin
  S := '';
  for i := 1 to FCount do
    with FPlugins[i] do
      S := S + Format('%d: FileName: "%s", DetectString: "%s"',
        [i, FileName, DetectStr]) + #13;
  MsgInfo(S);
end;

function TWlxPlugins.ActiveSupportsSearch: Boolean;
begin
  Result :=
    IsIndexValid(FActive) and
    ( Assigned(FPlugins[FActive].ListSearchText) or
      Assigned(FPlugins[FActive].ListSearchDialog) );
end;

function TWlxPlugins.ActiveSupportsPrint: Boolean;
begin
  Result :=
    IsIndexValid(FActive) and
    Assigned(FPlugins[FActive].ListPrint);
end;

function TWlxPlugins.ActiveSupportsCommands: Boolean;
begin
  Result :=
    IsIndexValid(FActive) and
    Assigned(FPlugins[FActive].ListSendCommand);
end;

function TWlxPlugins.ActiveWindowHandle: THandle;
begin
  if IsIndexValid(FActive) then
    Result := FPlugins[FActive].HWnd
  else
    Result := 0;
end;

end.

