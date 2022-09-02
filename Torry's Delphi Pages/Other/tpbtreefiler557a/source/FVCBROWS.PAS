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
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *   Rob Roberts  robr@pcisys.net
 *
 * ***** END LICENSE BLOCK ***** *)

{$C MOVEABLE,DEMANDLOAD,DISCARDABLE}

{--Conditional defines and compiler options that affect this unit}
  {$I BTDefine.Inc}
  {$IFNDEF UsingDelphi}
    !! This browser requires Delphi
  {$ENDIF}
  {$X+,F-,V+,B-,S-,I+,R-,Q-}

unit Fvcbrows;

interface

uses
  SysUtils,
  WinTypes,
  WinProcs,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Menus,
  Dialogs,
  BTBase,
  BTIsBase,
  Filer,
  StdCtrls,
  LowBrows,
  MedBrows,
  HiBrows;

const
  HardError        = 4;
  ProgrammingError = 5;

type
  TFvcBrowser = class;

  PLowWinBrowser = ^TLowWinBrowser;

  TLowWinBrowser = object(BRHBrowser)
    Owner  : TFvcBrowser;
    OnHeap : Boolean;

    constructor Init (     ParOnHeap           : Boolean;
                           ADrvOrFileBlockPtr  : Pointer;
                           ANumberOfEltsPerRow : Word;
                           ANumberOfRows       : Word;
                           AKeyNr              : Word;
                           ALKey,
                           AHKey               : GenKeyStr;
                       var ADatS;
                           AIsVarRec           : Boolean );

    destructor Done; virtual;
    function BuildRow(var RR : RowRec): Integer; virtual;
    function PerformFilter(var RR: RowRec;
                           var UseIt: Boolean): Integer; virtual;

    procedure ShowErrorOccured(EClass : Integer); virtual;
    function PreCompletePage : Integer; virtual;
    function PostCompletePage : Integer; virtual;
  end;

  TBuildRowEvent = function(Sender: TObject; var RR: RowRec): Integer of object;

  TFilterEvent = function(Sender: TObject; var RR: RowRec;
    var UseIt: Boolean): Integer of object;

  TFilterWorkingEvent = procedure(Sender: TObject; CallState: Integer;
    Rejected: Boolean) of object;

  TPrePostCompletePage = function(Sender : TObject): Integer of object;

  FontInfo = record
    ChHeightExtra,
    ChHeight,
    ChWidth,
    ChRefWidth    : Word;
    FixedPitch    : Boolean;
  End;

  TFvcBrowser = class(TCustomControl)
  protected {private}
    BrowserPtr: PLowWinBrowser;
    FilterInProgress: Boolean;
    FontDescr: FontInfo;
    FirstRow,
    FullPage: word;
    FBIsClosed : boolean;                                      {!!.54}
    FBorderStyle: TBorderStyle;
    FFilterEnabled: Boolean;
    FDoMouseMove: Boolean;
    FFooterColor: TColor;
    FFooter: BRLRowEltString;
    FFooterTxtColor: TColor;
    FHeaderColor: TColor;
    FHeader: BRLRowEltString;
    FHeaderTxtColor: TColor;
    FHighlightColor: TColor;
    FHighlightTxtColor: TColor;
    FHorizOfs: integer;
    FInitDone: Boolean;
    FMaxHorizOfs: word;
    FNoPaintHFCg: Boolean;
    FOnBuildRow: TBuildRowEvent;
    FOnFirstUserInit: TNotifyEvent;
    FOnPerformFilter: TFilterEvent;
    FOnPreCompletePage: TPrePostCompletePage;
    FOnPostCompletePage: TPrePostCompletePage;
    FOnRowChanged: TNotifyEvent;
    FOnShowFilterWorking : TFilterWorkingEvent;
    FPosClientCorruption: Boolean;
    FScrollBars: TScrollStyle;
    FTextMargin: TRect;
    FThumbHTrack,
    FThumbVTrack: word;
    FUseSeparator: Boolean;
    FVertScale: word;
    function  AdjustHorizOfs(Delta : integer): integer;
    function  BuildRow(var RR: RowRec): integer;
    procedure DisplayRow(I: integer; Inverse: Boolean);
    procedure EndFilter;
    procedure FirstInit;
    procedure FirstUserInit;
    function  GetCurrentKeyNr: word;
    function  GetHighKey: GenKeyStr;
    function  GetLowKey: GenKeyStr;
    procedure GetRowAreaRect(var Rect: TRect);
    procedure InvalidateBrowserScreen;
    procedure JustFiltered(Rejected: Boolean);
    procedure lwNewSize;
    procedure lwSetAndUpdateBrowserScreen(NewKeyStr: GenKeyStr; NewRef: LongInt);
    procedure lwUpdateBrowserScreen;
    procedure lwUpdateHorzScrollBar;
    procedure lwUpdateVertScrollBar;
    procedure NewSize;
    function  PerformFilter(var RR: RowRec; var UseIt: Boolean): integer;
    function  PreCompletePage: integer;
    function  PostCompletePage: integer;
    procedure RowChanged;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCharValues;
    procedure SetFilterEnabled(Value: Boolean);
    procedure SetFooter(Value: BRLRowEltString);
    procedure SetFooterColor(Value: TColor);
    procedure SetFooterTxtColor(Value: TColor);
    procedure SetHeader(Value: BRLRowEltString);
    procedure SetHeaderColor(Value: TColor);
    procedure SetHeaderTxtColor(Value: TColor);
    procedure SetHighKey(Value: GenKeyStr);
    procedure SetHighlightColor(Value: TColor);
    procedure SetHighlightTxtColor(Value: TColor);
    procedure SetHorizOfs(Value: integer);
    procedure SetKeyNr(Value: Word);
    procedure SetLowKey(Value: GenKeyStr);
    procedure SetUseSeparator(Value: Boolean);
    procedure ShowFilterWorking(CallState: Integer; Rejected: Boolean); virtual;
    function  TotalSpaceForLines(Rect : TRect): Word;
    procedure CMFontChanged(var Message: TMessage);                    {!!.52}
      message CM_FONTCHANGED;                                          {!!.52}
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode);
      message WM_GETDLGCODE;
    procedure WMHScroll(var Msg: TWMHScroll);
      message WM_HSCROLL;
    procedure WMKillFocus(var Msg: TWMKillFocus);
      message WM_KILLFOCUS;
    {$IFDEF Delphi4Plus}                                               {!!.57}
    procedure CMMouseWheel(var Msg: TCMMouseWheel);                    {!!.57}
      message CM_MOUSEWHEEL;                                           {!!.57}
    {$ENDIF}                                                           {!!.57}
    procedure WMNCMouseMove(var Msg: TWMNCMouseMove);
      message WM_NCMOUSEMOVE;
    procedure WMSize(var Msg: TWMSize);
      message WM_SIZE;
    procedure WMVScroll(var Msg: TWMVScroll);
      message WM_VSCROLL;
  protected
    function  CanCallLowBrowser: Boolean; virtual;
    function  ClientPosCorrupted: Boolean; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DrawSeparator(Pos: word); virtual;
    procedure EraseRowAreaRect; virtual;
    procedure GetBrowserTextRect(var TextRect : TRect); virtual;
    function  GetLineNrFromY(Y : Integer): word; virtual;
    function  GetTextOutPosY(LineNr: word): integer; virtual;
    function  GetVersion: string;                                        {!!.56}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MarkClientCorruptablePhase; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MoveToHorizPos(Pos : Word); virtual;
    procedure MoveToRelPos(Pos : Word); virtual;
    procedure Paint; override;
    procedure Reinit; virtual;
    procedure RightHome; virtual;
    procedure SetMargins; dynamic;
    procedure SetVersion(const Value: string);                           {!!.56}
    procedure ShowErrorOccured(EClass: Integer); virtual;
    procedure WriteDataLine(var DataLine: BRLRowEltString; LineNr: Word); virtual;
    procedure WriteFooter; virtual;
    procedure WriteHeader; virtual;
    function  WriteStringOut(var S      : BRLRowEltString;
                                 LineNr : word;
                                 XOfs   : integer): word; virtual;
    function  XYPosInRect(X, Y: Integer; Rect: TRect): Boolean;
  public
    function  BuildBrowScreenRow(var RR : RowRec): integer; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;                                      {!!.52}
    function  CalcMaxWidth: Integer; virtual;
    function  ConnectLowBrowser(ABrowserPtr: PLowWinBrowser): Boolean; virtual;
    function  GetCurrentDatRef: LongInt;
    function  GetCurrentKeyStr: string;
    function  GetCurNrOfLines: word;
    function  GetCurrentRec(var Match: Boolean): integer;
    function  GetThisRec(var RR : RowRec): integer;
    procedure FirstPage; virtual;                                      {!!.52}
    procedure LastPage; virtual;                                       {!!.52}
    procedure LeftHome; virtual;                                       {!!.52}
    procedure LineDown; virtual;                                       {!!.52}
    procedure LineLeft; virtual;                                       {!!.52}
    procedure LineRight; virtual;                                      {!!.52}
    procedure LineUp; virtual;                                         {!!.52}
    procedure PageDown; virtual;                                       {!!.52}
    procedure PageLeft; virtual;                                       {!!.52}
    procedure PageRight; virtual;                                      {!!.52}
    procedure PageUp; virtual;                                         {!!.52}
    procedure PosClientCorruption;
    procedure SetAndUpdateBrowserScreen(NewKeyStr: GenKeyStr; NewRef: LongInt);
    procedure SetKeys(KeyNr : Word; Low, High : GenKeyStr);            {!!.52}
    function  TotalCharHeight : Word;
    procedure UpdateBrowserScreen;

    procedure FileblockHasClosed;                              {!!.54}

    property Canvas;

    property HighKey: GenKeyStr
      read GetHighKey
      write SetHighKey;

    property KeyNumber: Word
      read GetCurrentKeyNr
      write SetKeyNr;

    property LowKey: GenKeyStr
      read GetLowKey
      write SetLowKey;

  published

    property Align;

    property BorderStyle: TBorderStyle
      read FBorderStyle
      write SetBorderStyle
      default bsSingle;

    property Color
      default clWindow;

    property Ctl3D;

    property Enabled;

    property FilterEnabled: Boolean
      read FFilterEnabled
      write SetFilterEnabled
      default False;

    property Font;

    property Footer: BRLRowEltString
      read FFooter
      write SetFooter;

    property FooterColor: TColor
      read FFooterColor
      write SetFooterColor
      default clBtnFace;

    property FooterTxtColor: TColor
      read FFooterTxtColor
      write SetFooterTxtColor
      default clBtnText;

    property Header: BRLRowEltString
      read FHeader
      write SetHeader;

    property HeaderColor: TColor
      read FHeaderColor
      write SetHeaderColor
      default clBtnFace;

    property HeaderTxtColor: TColor
      read FHeaderTxtColor
      write SetHeaderTxtColor
      default clBtnText;

    property HighlightColor: TColor
      read FHighlightColor
      write SetHighlightColor
      default clHighlight;

    property HighlightTxtColor: TColor
      read FHighlightTxtColor
      write SetHighlightTxtColor
      default clHighlightText;

    property HorizOfs: integer
      read FHorizOfs
      write SetHorizOfs
      default 0;

    property OnBuildRow: TBuildRowEvent
      read FOnBuildRow
      write FOnBuildRow;

    property OnDblClick;

    property OnEnter;

    property OnExit;

    property OnFirstUserInit: TNotifyEvent
      read FOnFirstUserInit
      write FOnFirstUserInit;

    property OnKeyDown;

    property OnKeyPress;

    property OnKeyUp;

    property OnMouseDown;

    property OnMouseMove;

    property OnMouseUp;

    property OnPerformFilter: TFilterEvent
      read FOnPerformFilter
      write FOnPerformFilter;

    property OnPreCompletePage: TPrePostCompletePage
      read FOnPreCompletePage
      write FOnPreCompletePage;

    property OnPostCompletePage: TPrePostCompletePage
      read FOnPostCompletePage
      write FOnPostCompletePage;

    property OnRowChanged: TNotifyEvent
      read FOnRowChanged
      write FOnRowChanged;

    property OnShowFilterWorking: TFilterWorkingEvent
      read FOnShowFilterWorking
      write FOnShowFilterWorking;

    property ParentColor;

    property ParentCtl3D;

    property ParentFont;

    property ParentShowHint;

    property PopupMenu;

    property ShowHint;

    property TabOrder;

    property TabStop;

    property UseSeparator: Boolean
      read FUseSeparator
      write SetUseSeparator
      default True;

    property Version: string                                             {!!.56}
      read GetVersion
      write SetVersion;

    property Visible;
  end;

implementation

const
  VertScrollScale = 63;

constructor TLowWinBrowser.Init
                         (    ParOnHeap           : Boolean;
                              ADrvOrFileBlockPtr  : Pointer;
                              ANumberOfEltsPerRow : Word;
                              ANumberOfRows       : Word;
                              AKeyNr              : Word;
                              ALKey,
                              AHKey               : GenKeyStr;
                          var ADatS;
                              AIsVarRec           : Boolean );
begin
  Owner := nil;
  OnHeap := ParOnHeap;
  if not BRHBrowser.Init(ADrvOrFileBlockPtr, ANumberOfEltsPerRow,
      ANumberOfRows, ALKey, AHKey, False, ADatS, AIsVarRec) then Fail;
  KeyNr := AKeynr;
end;

destructor TLowWinBrowser.Done;
begin
  {inherited Done;}                                                    {!!.52}
  BRHBrowser.Done;
  if Assigned(Owner) then
    Owner.BrowserPtr := nil;
  Owner := nil;
end;

function TLowWinBrowser.BuildRow (var RR  : RowRec): Integer;
begin
  Result := Owner.BuildRow(RR);
end;

function TLowWinBrowser.PerformFilter (var RR    : RowRec;
                                       var UseIt : Boolean): Integer;
begin
  Result := Owner.PerformFilter(RR, UseIt);
  if (Result = NoError) and Owner.FilterEnabled then begin
    Owner.JustFiltered (not UseIt);
  end;
end;

function TLowWinBrowser.PostCompletePage: Integer;
begin
  Result := Owner.PostCompletePage;
  Owner.EndFilter;
end;

function TLowWinBrowser.PreCompletePage: Integer;
begin
  Result := Owner.PreCompletePage;
end;

procedure TLowWinBrowser.ShowErrorOccured (EClass : Integer);
begin
  Owner.ShowErrorOccured (EClass);
  Owner.EndFilter;
end;

(************************************************************************)

function GetLeftButton: Byte;
const
  RLButton : array[Boolean] of Word = (VK_LBUTTON, VK_RBUTTON);
begin
  Result := RLButton[GetSystemMetrics(SM_SWAPBUTTON) <> 0];
end;

constructor TFvcBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csFramed, csOpaque, csDoubleClicks];
  FInitDone := False;
  BrowserPtr := nil;
  FScrollBars := ssBoth;
  FBorderStyle := bsSingle;
  Color := clWindow;
  FDoMouseMove := False;
  FFilterEnabled := False;
  FFooterColor := clBtnFace;
  FFooterTxtColor := clBtnText;
  FHeaderColor := clBtnFace;
  FHeaderTxtColor := clBtnText;
  FHighlightColor := clHighlight;
  FHighlightTxtColor := clHighlightText;
  FHorizOfs := 0;
  FUseSeparator := True;
  ParentColor := False;
  Height := 100;
  Width := 200;
end;

destructor TFvcBrowser.Destroy;                                        {!!.52}
begin                                                                  {!!.52}
  if Assigned(BrowserPtr) then                                         {!!.52}
    if BrowserPtr^.OnHeap then                                         {!!.52}
      Dispose(BrowserPtr, Done)                                        {!!.52}
    else                                                               {!!.52}
      BrowserPtr^.Done;                                                {!!.52}
  BrowserPtr := nil;                                                   {!!.52}
  inherited Destroy;                                                   {!!.52}
end;                                                                   {!!.52}

procedure TFvcBrowser.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if FBorderStyle = bsSingle then Style := Style or WS_BORDER;
    if FScrollBars in [ssVertical, ssBoth] then Style := Style or WS_VSCROLL;
    if FScrollBars in [ssHorizontal, ssBoth] then Style := Style or WS_HSCROLL;
  end;
end;

procedure TFvcBrowser.CreateWnd;
begin
  inherited CreateWnd;
  if Assigned(BrowserPtr) then FirstInit;
end;

function TFvcBrowser.AdjustHorizOfs(Delta : integer): integer;
var
  Offset : LongInt;
begin
  Offset := LongInt(FHorizOfs) + Delta;
  if Offset < 0 then begin
    Offset := 0;
  end else begin
    if OffSet > FMaxHorizOfs then begin
      Offset := FMaxHorizOfs;
    end;
  end;
  Result := Offset;
end;

function TFvcBrowser.BuildBrowScreenRow(var RR : RowRec): integer;
begin
  Result := ProgrammingError;
  if BrowserPtr = nil then Exit;
  Result := BrowserPtr^.BuildBrowScreenRow(RR);
end;

function TFvcBrowser.BuildRow(var RR: RowRec): integer;
begin
  if Assigned(FOnBuildRow) then
    Result := FOnBuildRow(Self, RR)
  else begin
    RR.Row := 'You do not have an OnBuildRow event defined';
    Result := NoError;
  end;
end;

function TFvcBrowser.CalcMaxWidth : Integer;
begin
  Result := MaxCols * FontDescr.ChWidth;
end;

function TFvcBrowser.CanCallLowBrowser: Boolean;
begin
                                                     {rewritten !!.54}
  Result := Assigned(BrowserPtr) and FInitDone and IsWindow(Handle)
    and BrowserPtr^.BrowserCallAllowed and (not FilterInProgress) and
    (not FBIsClosed);
end;

function TFvcBrowser.ClientPosCorrupted: Boolean;
begin
  Result := FPosClientCorruption;
end;

function TFvcBrowser.ConnectLowBrowser(ABrowserPtr: PLowWinBrowser): Boolean;
begin
  Result := False;
  FInitDone := False;
  if not Assigned(ABrowserPtr) then Exit;
  if Assigned(BrowserPtr) then begin
    if BrowserPtr^.OnHeap then begin
      Dispose (BrowserPtr, Done);
    end else begin
      BrowserPtr^.Done;
    end;
  end;
  BrowserPtr := ABrowserPtr;
  BrowserPtr^.Owner := Self;
  FirstInit;
  Result := True;
end;

procedure TFvcBrowser.DisplayRow(I: Integer; Inverse: Boolean);
begin
  Canvas.Brush.Color := Color;
  Canvas.Font.Color := Font.Color;

  if Inverse then begin
    Canvas.Brush.Color := FHighlightColor;
    Canvas.Font.Color := FHighlightTxtColor;
  end;
  WriteDataLine (BrowserPtr^.BSAPtr^ [I]^.Row, I);
  if I = 1 then begin
    if ClientPosCorrupted or not FNoPaintHFCg then begin
      MarkClientCorruptablePhase;
      if (FHeader <> '') or (FFooter <> '') then begin
        if FHeader <> '' then begin
          Canvas.Brush.Color := FHeaderColor;
          Canvas.Font.Color := FHeaderTxtColor;
          WriteHeader;
        end;
        if FFooter <> '' then begin
          Canvas.Brush.Color := FFooterColor;
          Canvas.Font.Color := FFooterTxtColor;
          WriteFooter;
        end;
      end
    end else begin
      if BrowserPtr^.GetCurRow <> 1 then FNoPaintHFCg := False;
    end;
  end;
end;

procedure TFvcBrowser.DrawSeparator(Pos: word);
var
  TR : TRect;
begin
  GetBrowserTextRect(TR);
  Canvas.MoveTo(TR.Left, Pos);
  Canvas.LineTo(TR.Right, Pos);
end;

procedure TFvcBrowser.EndFilter;
begin
  if FilterInProgress then begin
    {FilterInProgress := False;}                                       {!!.53}
    ShowFilterWorking(1, False);
    FilterInProgress := False;                                         {!!.53}
    {InvalidateBrowserScreen;  removed !!.54}                          {!!.53}
  end;
end;

procedure TFvcBrowser.EraseRowAreaRect;
var
  Rect : TRect;
begin
  Rect.Top := GetTextOutPosY(1);
  Rect.Bottom := Rect.Top + TotalCharHeight * FullPage;
  Rect.Left := FTextMargin.Left;
  Rect.Right := Rect.Left + Width;
  InvalidateRect(Handle, @Rect, True);
end;

procedure TFvcBrowser.FirstInit;
begin
  if FInitDone then Exit;
  if not HandleAllocated then Exit;
  SetMargins;
  Reinit;
  {BrowserPtr^.SetNrOfRows(FullPage);}                                 {!!.52}
  FVertScale := VertScrollScale;
  FThumbVTrack := $FFFF;
  FThumbHTrack := $FFFF;
  if FScrollBars in [ssHorizontal, ssBoth] then begin
    SetScrollRange (Handle, SB_Horz, 0, FMaxHorizOfs, True);
  end;
  if FScrollBars in [ssVertical, ssBoth] then begin
    SetScrollRange (Handle, SB_Vert, 0, FVertScale, True);
  end;
  FInitDone := True;
  FirstUserInit;
end;

procedure TFvcBrowser.FirstPage;
var
  BST     : BrowScreenState;
  Update,
  Changed : Boolean;
begin
  Update := False;
  with BrowserPtr^ do begin
    GetBrowScreenState(BST);
    HBuildFirstPage(Changed);
    if StatusOK then begin
      if Changed or BrowScreenStateChanged(BST) then begin
        InvalidateBrowserScreen;
        Update := True;
      end;
    end;
  end;
  if Update then lwUpdateVertScrollBar;
end;

procedure TFvcBrowser.FirstUserInit;
begin
  if Assigned(FOnFirstUserInit) then FOnFirstUserInit(Self);
end;

procedure TFvcBrowser.GetBrowserTextRect(var TextRect : TRect);
begin
  TextRect := GetClientRect;
  Inc(TextRect.Left, FTextMargin.Left);
  Inc(TextRect.Top, FTextMargin.Top);
  Dec(TextRect.Right, FTextMargin.Right);
  if TextRect.Right < TextRect.Left then begin
    TextRect.Right := TextRect.Left;
  end;
  Dec(TextRect.Bottom, FTextMargin.Bottom);
  {if TextRect.Bottom < TextRect.Bottom then begin}                    {!!.53}
    {TextRect.Bottom := TextRect.Bottom;}                              {!!.53}
  if TextRect.Bottom < TextRect.Top then begin                         {!!.53}
    TextRect.Bottom := TextRect.Top;                                   {!!.53}
  end;
end;

function TFvcBrowser.GetCurrentDatRef : LongInt;
begin
  Result := 0;
  if not CanCallLowBrowser then Exit;
  Result := BrowserPtr^.GetCurrentDatRef;
end;

function TFvcBrowser.GetCurrentKeyNr : Word;
begin
  Result := $FFFF;
  if BrowserPtr = nil then Exit;
  Result := BrowserPtr^.KeyNr;
end;

function TFvcBrowser.GetCurrentKeyStr: string;
begin
  Result := '';
  if not CanCallLowBrowser then Exit;
  Result := BrowserPtr^.GetCurrentKeyStr;
end;

function TFvcBrowser.GetHighKey: GenKeyStr;
begin
  Result := '';
  if not CanCallLowBrowser then Exit;
  Result := BrowserPtr^.HighKey;
end;

function TFvcBrowser.GetLowKey: GenKeyStr;
begin
  Result := '';
  if not CanCallLowBrowser then Exit;
  Result := BrowserPtr^.LowKey;
end;

function TFvcBrowser.GetLineNrFromY(Y : Integer): Word;
begin
  Result := (Y - FTextMargin.Top) div TotalCharHeight + 2 - FirstRow;
end;

function TFvcBrowser.GetCurrentRec(var Match : Boolean): integer;
begin
  Match := False;
  Result := ProgrammingError;
  if not CanCallLowBrowser then Exit;
  with BrowserPtr^ do begin
    Result := GetRowMatchingRec(BSAPtr^[GetCurRow]^, True, True, Match);
  end;
end;

function TFvcBrowser.GetCurNrOfLines : Word;
begin
  Result := FullPage;
end;

procedure TFvcBrowser.GetRowAreaRect(var Rect : TRect);
var
  Lines : Word;
begin
  Rect.Top := GetTextOutPosY(1);
  if Assigned(BrowserPtr) then begin
    Lines := BrowserPtr^.GetLastRow;
  end else begin
    Lines := FullPage;
  end;
  Rect.Bottom := Rect.Top + TotalCharHeight * Lines;
  Rect.Left := FTextMargin.Left;
  Rect.Right := Rect.Left + Width;
end;

function TFvcBrowser.GetTextOutPosY(LineNr: word): integer;
var
  TR: TRect;
begin
  Result := (LineNr + FirstRow - 2) * TotalCharHeight + FTextMargin.Top;
  if LineNr > FullPage then begin
    TR := GetClientRect;
    Result := TR.Bottom - TotalCharHeight - FontDescr.ChHeightExtra
      - FTextMargin.Bottom;
  end;
end;

function TFvcBrowser.GetVersion: string;                                {!!.56 - new}
begin
  Result := VersionStr;
end;

function TFvcBrowser.GetThisRec(var RR : RowRec): integer;
begin
  Result := ProgrammingError;
  if BrowserPtr = nil then Exit;
  Result := BrowserPtr^.BRGetRec(RR, False, False);
end;

procedure TFvcBrowser.InvalidateBrowserScreen;
begin
  FNoPaintHFCg := False;
  InvalidateRect(Handle, nil, False);
end;

procedure TFvcBrowser.JustFiltered(Rejected: Boolean);
begin
  if FilterInProgress then begin                                       {!!.52}
    ShowFilterWorking (0, Rejected);                                   {!!.52}
  end else begin                                                       {!!.52}
    FilterInProgress := True;                                          {!!.52}
    ShowFilterWorking ( -1, Rejected );                                {!!.52}
  end;                                                                 {!!.52}
end;

procedure TFvcBrowser.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if CanCallLowBrowser then begin
    if ssCtrl in Shift then
      case Key of
        vk_Home  : FirstPage;
        vk_End   : LastPage;
        vk_Right : PageRight;
        vk_Left  : PageLeft;
      end else
      case Key of
        vk_Down  : LineDown;
        vk_Up    : LineUp;
        vk_Next  : PageDown;
        vk_Prior : PageUp;
        vk_Home  : LeftHome;
        vk_End   : RightHome;
        vk_Right : LineRight;
        vk_Left  : LineLeft;
      end;
  end;
end;

procedure TFvcBrowser.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if not CanCallLowBrowser then Exit;
  case UpCase(Key) of
    '0'..'9', 'A'..'Z', 'ä', 'Ä', 'ö', 'Ö', 'ü', 'Ü', 'ß' : begin
      SetAndUpdateBrowserScreen((Key), 0);
    end;
    '+' : UpdateBrowserScreen;
  end;
end;

procedure TFvcBrowser.LastPage;
var
  BST     : BrowScreenState;
  Update,
  Changed : Boolean;
begin
  Update := False;
  with BrowserPtr^ do begin
    GetBrowScreenState(BST);
    HBuildLastPage(Changed);
    if StatusOK then begin
      if Changed or BrowScreenStateChanged(BST) then begin
        InvalidateBrowserScreen;
        Update := True;
      end;
    end;
  end;
  if Update then lwUpdateVertScrollBar;
end;

procedure TFvcBrowser.LeftHome;
var
  OldHOfs : Integer;
begin
  OldHOfs := FHorizOfs;
  FHorizOfs := AdjustHorizOfs(-FHorizOfs);
  if OldHOfs <> FHorizOfs then begin
    InvalidateBrowserScreen;
    lwUpdateHorzScrollBar;
  end;
end;

procedure TFvcBrowser.LineDown;
var
  LRow   : Word;
  Moved  : Word;
  CR     : Word;
  R      : TRect;
  Dummy,
  Update : Boolean;
  TRR    : RowRec;
begin
  Update := True;
  FNoPaintHFCg := True;
  with BrowserPtr^ do begin
    LRow := GetLastRow;
    CR := GetCurRow;
    if (CR = LRow) or (LRow = 0) then begin
      MarkClientCorruptablePhase;
      HBuildNextPage(1, Moved, True, 0, Dummy);
      if StatusOK then begin
        if OtherAction then begin
          InvalidateBrowserScreen;
        end else begin
          if Moved = 1 then begin
            if FullPage <> 1 then begin
              if LRow = GetLastRow then begin
                if ClientPosCorrupted then begin
                  InvalidateBrowserScreen;
                end else begin
                  GetRowAreaRect(R);
                  Dec(R.Bottom, FontDescr.ChHeightExtra);
                  CopyRowRec(BSAPtr^ [LRow]^, TRR);
                  CopyRowRec(BSAPtr^ [Pred (LRow)]^, BSAPtr^ [LRow]^);
                  DisplayRow(LRow, False);
                  CopyRowRec(TRR, BSAPtr^ [LRow]^);
                  {-Copy RowRec to avoid scrolling the highlight bar}
                  ScrollWindow(Handle, 0, -TotalCharHeight, @R, @R);
                  ValidateRect(Handle, nil);
                end;
              end else begin
                SetCurRow(Succ(CR));
                DisplayRow(LRow, False);
              end;
            end;
            DisplayRow(GetCurRow, True);
          end else begin
            Update := False;
          end;
        end;
      end else begin
        Update := False;
      end;
    end else begin
      DisplayRow(CR, False);
      SetCurRow(Succ(CR));
      DisplayRow(GetCurRow, True);
    end;
  end;
  if Update then lwUpdateVertScrollBar;
end;

procedure TFvcBrowser.LineUp;
var
  Moved  : Word;
  CR     : Word;
  R      : TRect;
  Dummy,
  Update : Boolean;
  TRR    : RowRec;
begin
  Update := True;
  FNoPaintHFCg := True;
  with BrowserPtr^ do begin
    CR := GetCurRow;
    if CR = 1 then begin
      MarkClientCorruptablePhase;
      HBuildPrevPage(1, Moved, True, 0, Dummy);
      if StatusOK then begin
        if OtherAction then begin
          InvalidateBrowserScreen;
        end else begin
          if Moved = 1 then begin
            if FullPage <> 1 then begin
              if ClientPosCorrupted then begin
                InvalidateBrowserScreen;
              end else begin
                GetRowAreaRect(R);
                Dec(R.Bottom, FontDescr.ChHeightExtra);
                CopyRowRec(BSAPtr^ [1]^, TRR);
                CopyRowRec(BSAPtr^ [2]^, BSAPtr^ [1]^);
                DisplayRow(1, False);
                CopyRowRec(TRR, BSAPtr^ [1]^);
                {-Copy RowRec to avoid scrolling the highlight bar}
                ScrollWindow(Handle, 0, TotalCharHeight, @R, @R);
                ValidateRect(Handle, nil);
              end;
            end;
            DisplayRow(1, True);
          end else begin
            Update := False;
          end;
        end;
      end else begin
        Update := False;
      end;
    end else begin
      DisplayRow(CR, False);
      SetCurRow(Pred(CR));
      FNoPaintHFCg := True;
      DisplayRow(GetCurRow, True);
    end;
  end;
  if Update then lwUpdateVertScrollBar;
end;

procedure TFvcBrowser.LineRight;
var
  OldHOfs : Integer;
begin
  OldHOfs := FHorizOfs;
  FHorizOfs := AdjustHorizOfs(FontDescr.ChWidth);
  if OldHOfs <> FHorizOfs then begin
    InvalidateBrowserScreen;
    lwUpdateHorzScrollBar;
  end;
end;

procedure TFvcBrowser.LineLeft;
var
  OldHOfs : Integer;
begin
  OldHOfs := FHorizOfs;
  FHorizOfs := AdjustHorizOfs(-FontDescr.ChWidth);
  if OldHOfs <> FHorizOfs then begin
    InvalidateBrowserScreen;
    lwUpdateHorzScrollBar;
  end;
end;

procedure TFvcBrowser.lwNewSize;
var
  OldNrOfRows : Word;
begin
  OldNrOfRows := FullPage;
  Reinit;
  with BrowserPtr^ do begin
    if OldNrOfRows > FullPage then begin
      HShrinkPage(FullPage);
    end else begin
      if OldNrOfRows < FullPage then begin
        HExpandPage(FullPage);
      end;
    end;
  end;
  InvalidateBrowserScreen;
end;

procedure TFvcBrowser.lwSetAndUpdateBrowserScreen (NewKeyStr : GenKeyStr;
                                                   NewRef    : LongInt );
begin
  with BrowserPtr^ do begin
    HBuildNewPage(KeyNr, NewKeyStr, NewRef, GetCurRow, NrOfRows);
    {InvalidateBrowserScreen;}                                         {!!.52}
    EraseRowAreaRect;                                                  {!!.52}
    if StatusOK then lwUpdateVertScrollBar;
  end;
end;

procedure TFvcBrowser.lwUpdateBrowserScreen;
var
  Changed : Boolean;
begin
  with BrowserPtr^ do begin
    HBuildThisPage(Changed);
    if Changed then begin
      EraseRowAreaRect;                                                {!!.52}
      InvalidateBrowserScreen;
      if StatusOK then lwUpdateVertScrollBar;
    end;
  end;
end;

procedure TFvcBrowser.lwUpdateHorzScrollBar;
begin
  if FMaxHorizOfs > 0 then begin
    if FScrollBars in [ssHorizontal, ssBoth] then begin                {!!.53}
      SetScrollPos(Handle, SB_Horz, FHorizOfs, True);
    end;
  end;
end;

procedure TFvcBrowser.lwUpdateVertScrollBar;
var
  RelPos : Word;
begin
  RowChanged;
  BrowserPtr^.HGetApprRelPos(RelPos, FVertScale, GetCurrentKeyStr,
    GetCurrentDatRef);
  if not BrowserPtr^.StatusOK then RelPos := 0;
  if FScrollBars in [ssVertical, ssBoth] then begin
    SetScrollPos(Handle, SB_Vert, RelPos, True);
  end;
end;

procedure TFvcBrowser.MarkClientCorruptablePhase;
begin
  FPosClientCorruption := False;
end;

procedure TFvcBrowser.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  OldCurrow : Integer;
  Rect      : TRect;
begin
  if not (csDesigning in ComponentState) and CanFocus then
  begin
    SetFocus;
    if ValidParentForm(Self).ActiveControl <> Self then
    begin
      MouseCapture := False;
      Exit;
    end;
  end;
  if (Button = mbLeft) and (ssDouble in Shift) then
    DblClick
  else if Button = mbLeft then
  begin
    if not CanCallLowBrowser then Exit;
    OldCurRow := BrowserPtr^.GetCurRow;
    if OldCurRow <> 0 then begin
      GetRowAreaRect(Rect);
      if not XYPosInRect(X, Y, Rect) then Exit;
      BrowserPtr^.SetCurRow(GetLineNrFromY(Y));
      if (OldCurRow <> BrowserPtr^.GetCurRow)
          and (BrowserPtr^.GetCurRow <> 0) then begin
        DisplayRow(OldCurRow, False);
        DisplayRow(BrowserPtr^.GetCurRow, True);
        lwUpDateVertScrollBar;
      end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TFvcBrowser.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldCurRow  : Integer;
  Rect       : TRect;
  CPos, SPos : TPoint;
  LButton    : Byte;
const
  Scrolling : Boolean = False;
begin
  if Scrolling then Exit;
  if not FDoMouseMove then begin
    if GetFocus = Handle then FDoMouseMove := True;
    Exit;
  end;
  if CanCallLowBrowser then begin
    if ssLeft in Shift then begin
      with BrowserPtr^ do begin
        OldCurRow := GetCurRow;
        if OldCurRow <> 0 then begin
          GetRowAreaRect(Rect);
          if not XYPosInRect(X, Y, Rect) then begin
            LButton := GetLeftButton;
            Scrolling := True;
            repeat
              GetCursorPos(SPos);
              CPos := ScreenToClient(SPos);
              if CPos.Y > Rect.Bottom then Linedown;
              if CPos.Y < Rect.Top then LineUp;
              Application.ProcessMessages;
            until (GetAsyncKeyState(LButton) and $8000 = 0) or
              XYPosInRect(CPos.X, CPos.Y, Rect);
            Scrolling := False;
          end else begin
            SetCurRow(GetLineNrFromY(Y));
            if GetCurRow > GetLastRow then begin
              SetCurRow(GetLastRow);
            end;
            if (OldCurRow <> GetCurRow) and (GetCurRow <> 0) then begin
              FNoPaintHFCg := True;
              DisplayRow(OldCurRow, False);
              FNoPaintHFCg := True;
              DisplayRow(CurRow, True);
              lwUpdateVertScrollBar;
            end;
          end;
        end;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TFvcBrowser.MoveToHorizPos(Pos: Word);
var
  OldHOfs : Integer;
begin
  OldHOfs := FHorizOfs;
  FHorizOfs := Pos;
  FHorizOfs := AdjustHorizOfs(0);
  if OldHOfs <> FHorizOfs then begin
    InvalidateBrowserScreen;
    lwUpdateHorzScrollBar;
  end;
end;

procedure TFvcBrowser.MoveToRelPos(Pos: Word);
var
  Key : GenKeyStr;
  Ref : LongInt;
begin
  FNoPaintHFCg := True;
  BrowserPtr^.HGetApprKeyAndRef(Pos, FVertScale, Key, Ref);
  SetAndUpdateBrowserScreen(Key, Ref);
  lwUpdateVertScrollBar;                                               {!!.52}
  FNoPaintHFCg := False;                                               {!!.53}
end;

procedure TFvcBrowser.NewSize;
begin
  if CanCallLowBrowser then begin
    lwNewSize;
  end;
end;

procedure TFvcBrowser.PageDown;
var
  BST     : BrowScreenState;
  Moved   : Word;
  Update,
  Changed : Boolean;
begin
  Update := False;
  with BrowserPtr^ do begin
    GetBrowScreenState(BST);
    HBuildNextPage(FullPage, Moved, True, 1, Changed);
    if StatusOK then begin
      if OtherAction or (Moved > 0) or Changed
          or BrowScreenStateChanged(BST) then begin
        InvalidateBrowserScreen;
        Update := True;
      end;
    end;
  end;
  if Update then lwUpdateVertScrollBar;
end;


procedure TFvcBrowser.PageUp;
var
  BST     : BrowScreenState;
  Moved   : Word;
  Update,
  Changed : Boolean;
begin
  Update := False;
  with BrowserPtr^ do begin
    GetBrowScreenState(BST);
    HBuildPrevPage(FullPage, Moved, True, 1, Changed);
    if StatusOK then begin
      if OtherAction or (Moved > 0) or Changed
          or BrowScreenStateChanged(BST) then begin
        InvalidateBrowserScreen;
        Update := True;
      end;
    end;
  end;
  if Update then lwUpdateVertScrollBar;
end;

procedure TFvcBrowser.PageRight;
var
  OldHOfs : Integer;
begin
  OldHOfs := FHorizOfs;
  FHorizOfs := AdjustHorizOfs(FontDescr.ChWidth * 10);
  if OldHOfs <> FHorizOfs then begin
    InvalidateBrowserScreen;
    lwUpdateHorzScrollBar;
  end;
end;

procedure TFvcBrowser.PageLeft;
var
  OldHOfs : Integer;
begin
  OldHOfs := FHorizOfs;
  FHorizOfs := AdjustHorizOfs(-FontDescr.ChWidth * 10);
  if OldHOfs <> FHorizOfs then begin
    InvalidateBrowserScreen;
    lwUpdateHorzScrollBar;
  end;
end;

function TFvcBrowser.PerformFilter(var RR: RowRec;
                                   var UseIt: Boolean): integer;
begin
  if Assigned(FOnPerformFilter) and FFilterEnabled then
    Result := FOnPerformFilter(Self, RR, UseIt)
  else begin
    UseIt := True;
    Result := NoError;
  end;
end;

function TFvcBrowser.PreCompletePage: integer;
begin
  if Assigned(FOnPreCompletePage) then
    Result := FOnPreCompletePage(Self)
  else Result := NoError;
end;

procedure TFvcBrowser.PosClientCorruption;
begin
  FPosClientCorruption := True;
end;

function TFvcBrowser.PostCompletePage: integer;
begin
  if Assigned(FOnPostCompletePage) then
    Result := FOnPostCompletePage(Self)
  else Result := NoError;
end;

procedure TFvcBrowser.Reinit;
var
  R         : TRect;
  MaxWidth  : Word;
begin
  SetCharValues;
  {--Horizontal stuff in pixels}              {!!.52 Moved from end of method}
  MaxWidth := CalcMaxWidth;
  if Width > MaxWidth then begin
    FMaxHorizOfs := 0;
  end else begin
    FMaxHorizOfs := MaxWidth - Width;
  end;
  FHorizOfs := AdjustHorizOfs(0);
  if FScrollBars in [ssHorizontal, ssBoth] then begin
    SetScrollRange (Handle, SB_Horz, 0, FMaxHorizOfs, False);
    SetScrollPos (Handle, SB_Horz, FHorizOfs, True);
  end;
  {--Get Rectangle to use}
  GetBrowserTextRect(R);
  {--Vertical stuff in rows}
  FullPage := TotalSpaceForLines(R) div TotalCharHeight;
  FirstRow := 1;
  if FHeader <> '' then begin
    Dec (FullPage);
    Inc (FirstRow);
  end;
  if FFooter <> '' then Dec (FullPage);
  if (FullPage > $FFF0) or (FullPage = 0) then FullPage := 1;
  {-Holds functionality of this browser when resized below 1}
  BrowserPtr^.SetNrOfRows(FullPage);                                   {!!.52}
(*  {--Horizontal stuff in pixels}        {!!.52 Moved to beginning of method}
  MaxWidth := CalcMaxWidth;
  if Width > MaxWidth then begin
    FMaxHorizOfs := 0;
  end else begin
    FMaxHorizOfs := MaxWidth - Width;
  end;
  FHorizOfs := AdjustHorizOfs(0);
  if FScrollBars in [ssHorizontal, ssBoth] then begin
    SetScrollRange (Handle, SB_Horz, 0, FMaxHorizOfs, False);
    SetScrollPos (Handle, SB_Horz, FHorizOfs, True);
  end;  *)
end;

procedure TFvcBrowser.RightHome;
var
  OldHOfs : Integer;
begin
  OldHOfs := FHorizOfs;
  FHorizOfs := AdjustHorizOfs(FMaxHorizOfs - FHorizOfs);
  if OldHOfs <> FHorizOfs then begin
    InvalidateBrowserScreen;
    lwUpdateHorzScrollBar;
  end;
end;

procedure TFvcBrowser.RowChanged;
begin
  if Assigned(FOnRowChanged) then FOnRowChanged(Self);
end;

procedure TFvcBrowser.SetAndUpdateBrowserScreen( NewKeyStr : GenKeyStr;
                                                    NewRef    : LongInt );
begin
  if CanCallLowBrowser then begin
    lwSetAndUpdateBrowserScreen ( NewKeyStr, NewRef );
  end;
end;

procedure TFvcBrowser.SetCharValues;
var
  TM: TTextMetric;
begin
  with FontDescr do begin
    ChHeight := 1;
    ChWidth:= 1;
    ChRefWidth := 1;
    ChHeightExtra := 0;
    Canvas.Font := Font;
    if GetTextMetrics(Canvas.Handle, TM) then begin
      with TM do begin
        ChHeight := tmHeight + tmExternalLeading;
        ChWidth := (tmMaxCharWidth + tmAveCharWidth) shr 1;
        ChRefWidth := tmAveCharWidth;
      end;
    end;
  end;
end;

procedure TFvcBrowser.SetHighKey(Value: GenKeyStr);
begin
  if not CanCallLowBrowser then Exit;
  BrowserPtr^.HighKey := Value;
  {EraseRowAreaRect;}                                                   {!!.53}
  SetAndUpdateBrowserScreen((Value), 0);
end;

procedure TFvcBrowser.SetKeyNr(Value : Word);
begin
  if CanCallLowBrowser then begin
    BrowserPtr^.KeyNr := Value;
    UpdateBrowserScreen;
  end;
end;

procedure TFvcBrowser.SetKeys(KeyNr : Word; Low, High : GenKeyStr);    {!!.52}
begin
  if not CanCallLowBrowser then Exit;
  BrowserPtr^.KeyNr := KeyNr;
  BrowserPtr^.LowKey := Low;
  BrowserPtr^.HighKey := High;
  {EraseRowAreaRect;}                                                  {!!.53}
  SetAndUpdateBrowserScreen((Low), 0);
end;

procedure TFvcBrowser.SetLowKey(Value: GenKeyStr);
begin
  if not CanCallLowBrowser then Exit;
  BrowserPtr^.LowKey := Value;
  {EraseRowAreaRect;}                                                  {!!.53}
  SetAndUpdateBrowserScreen((Value), 0);
end;

procedure TFvcBrowser.SetMargins;
begin
  FillChar(FTextMargin, SizeOf(FTextMargin), 0);
end;

procedure TFvcBrowser.SetVersion(const Value: string);                 {!!.56}
begin
  { do nothing. Necessary since most versions of Delphi do not support
    read only properties. }
end;

procedure TFvcBrowser.ShowErrorOccured(EClass: Integer);
begin
  if EClass > DialogError then                                         {!!.52}
    raise Exception.Create(Format('Class %d error.  Isam Error %d',
      [EClass, IsamError]));
end;

procedure TFvcBrowser.ShowFilterWorking ( CallState : Integer;
                                          Rejected  : Boolean );
begin
  if Assigned(FOnShowFilterWorking) then
    FOnShowFilterWorking(Self, CallState, Rejected);
end;

procedure TFvcBrowser.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TFvcBrowser.SetFooterColor(Value: TColor);
begin
  if FFooterColor <> Value then begin
    FFooterColor := Value;
    InvalidateBrowserScreen;                                           {!!.53}
  end;
end;

procedure TFvcBrowser.SetFooter(Value: BRLRowEltString);
begin
  if FFooter <> Value then begin
    FFooter := Value;
    FNoPaintHFCg := False;                                             {!!.52}
    InvalidateRect(Handle, nil, True);                                 {!!.52}
    if Assigned(BrowserPtr) then ReInit;                               {!!.52}
    UpdateBrowserScreen;
  end;
end;

procedure TFvcBrowser.SetFooterTxtColor(Value: TColor);
begin
  if FFooterTxtColor <> Value then begin
    FFooterTxtColor := Value;
    InvalidateBrowserScreen;                                           {!!.53}
  end;
end;

procedure TFvcBrowser.SetHeaderColor(Value: TColor);
begin
  if FHeaderColor <> Value then begin
    FHeaderColor := Value;
    InvalidateBrowserScreen;                                           {!!.53}
  end;
end;

procedure TFvcBrowser.SetHeader(Value: BRLRowEltString);
begin
  if FHeader <> Value then begin
    FHeader := Value;
    FNoPaintHFCg := False;                                             {!!.52}
    InvalidateRect(Handle, nil, True);                                 {!!.52}
    if Assigned(BrowserPtr) then ReInit;                               {!!.52}
    UpdateBrowserScreen;
  end;
end;

procedure TFvcBrowser.SetHeaderTxtColor(Value: TColor);
begin
  if FHeaderTxtColor <> Value then begin
    FHeaderTxtColor := Value;
    InvalidateBrowserScreen;                                           {!!.53}
  end;
end;

procedure TFvcBrowser.SetHighlightColor(Value: TColor);
begin
  if FHighlightColor <> Value then begin
    FHighlightColor := Value;
    InvalidateBrowserScreen;                                           {!!.53}
  end;
end;

procedure TFvcBrowser.SetHighlightTxtColor(Value: TColor);
begin
  if FHighlightTxtColor <> Value then begin
    FHighlightTxtColor := Value;
    InvalidateBrowserScreen;                                           {!!.53}
  end;
end;

procedure TFvcBrowser.SetHorizOfs(Value: integer);
begin
  if FHorizOfs <> Value then begin
    FHorizOfs := AdjustHorizOfs(Value - FHorizOfs);
    UpdateBrowserScreen;
  end;
end;

procedure TFvcBrowser.SetFilterEnabled(Value: Boolean);
begin
  if FFilterEnabled <> Value then begin
    FFilterEnabled := Value;
    if FilterInProgress and not Value then EndFilter;
    EraseRowAreaRect;
    UpdateBrowserScreen;
  end;
end;

procedure TFvcBrowser.SetUseSeparator(Value: Boolean);
begin
  if FUseSeparator <> Value then begin
    FUseSeparator := Value;
    InvalidateBrowserScreen;                                           {!!.53}
  end;
end;

function TFvcBrowser.TotalCharHeight : Word;
begin
  Result := FontDescr.ChHeight + FontDescr.ChHeightExtra;
  if Result = 0 then Result := 1;
end;

function TFvcBrowser.TotalSpaceForLines(Rect: TRect): Word;
begin
  Result := Rect.Bottom - Rect.Top;
end;

procedure TFvcBrowser.UpdateBrowserScreen;
begin
  if CanCallLowBrowser then lwUpdateBrowserScreen;
end;

procedure TFvcBrowser.CMFontChanged(var Message: TMessage);           {!!.52}
begin                                                                 {!!.52}
  inherited;                                                          {!!.52}
  NewSize;                                                            {!!.52}
end;                                                                  {!!.52}

procedure TFvcBrowser.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TFvcBrowser.WMHScroll(var Msg: TWMHScroll);
begin
  if not CanCallLowBrowser then Exit;
  if not FilterInProgress then SetFocus;                               {!!.53}
  if not FDoMouseMove then begin
    if GetFocus = Handle then
      FDoMouseMove := True;
    {Exit;}                                                           {!!.52}
  end;
  case Msg.ScrollCode of
    SB_ThumbPosition,                                                 {!!.53}
    SB_EndScroll : begin
      if FThumbHTrack <> $FFFF then begin
        MoveToHorizPos(FThumbHTrack);
        FThumbHTrack := $FFFF;
      end;
    end;
    SB_ThumbTrack : FThumbHTrack := Msg.Pos;
    SB_LineRight  : LineRight;
    SB_LineLeft   : LineLeft;
    SB_PageRight  : PageRight;
    SB_PageLeft   : PageLeft;
    SB_Left       : LeftHome;
    SB_Right      : RightHome;
  end;
end;

procedure TFvcBrowser.WMKillFocus(var Msg: TWMKillFocus);
begin
  FDoMouseMove := False;
  FNoPaintHFCg := False;
  inherited;
end;

{$IFDEF Delphi4Plus}                                                   {!!.57}
procedure TFvcBrowser.CMMouseWheel(var Msg: TCMMouseWheel);            {!!.57}
begin                                                                  {!!.57}
  if not CanCallLowBrowser then Exit;                                  {!!.57}
  if not FilterInProgress then SetFocus;                               {!!.57}
  if not FDoMouseMove then begin                                       {!!.57}
    if GetFocus = Handle then FDoMouseMove := True;                    {!!.57}
  end;                                                                 {!!.57}
  if Msg.WheelDelta > 0 then                                           {!!.57}
    LineUp                                                             {!!.57}
  else                                                                 {!!.57}
    LineDown;                                                          {!!.57}
end;                                                                   {!!.57}
{$ENDIF}                                                               {!!.57}

procedure TFvcBrowser.WMNCMouseMove(var Msg : TWMNCMouseMove);
begin
  if not FDoMouseMove then begin
    if GetFocus = Handle then FDoMouseMove := True;
  end;
  inherited;
end;

procedure TFvcBrowser.WMSize(var Msg: TWMSize);
begin
  if CanCallLowBrowser then
    if Msg.SizeType <> SizeIconic then
      NewSize;
  inherited;
end;

procedure TFvcBrowser.WMVScroll(var Msg: TWMVScroll);
begin
  if not CanCallLowBrowser then Exit;
  if not FilterInProgress then SetFocus;                               {!!.53}
  if not FDoMouseMove then begin
    if GetFocus = Handle then FDoMouseMove := True;
    {Exit;}                                                            {!!.52}
  end;
  case Msg.ScrollCode of
    SB_ThumbPosition,                                                  {!!.53}
    SB_EndScroll  : begin
      if FThumbVTrack <> $FFFF then begin
        MoveToRelPos(FThumbVTrack);
        FThumbVTrack := $FFFF;
      end;
    end;
    SB_LineDown   : LineDown;
    SB_LineUp     : LineUp;
    SB_PageDown   : PageDown;
    SB_PageUp     : PageUp;
    SB_ThumbTrack : FThumbVTrack := Msg.Pos;
    SB_Top        : FirstPage;
    SB_Bottom     : LastPage;
  end;
end;

procedure TFvcBrowser.WriteDataLine(var DataLine : BRLRowEltString;
                                           LineNr   : Word);
begin
  WriteStringOut(DataLine, LineNr, -FHorizOfs);
end;

procedure TFvcBrowser.WriteFooter;
var
  SepPos   : Word;
begin
  SepPos := WriteStringOut(FFooter, Succ(FullPage), -FHorizOfs);       {!!.53}
  if FUseSeparator then DrawSeparator(SepPos - 1);
end;

procedure TFvcBrowser.WriteHeader;
var
  SepPos: Word;
begin
  SepPos := WriteStringOut(FHeader, 0, -FHorizOfs);
  if FUseSeparator then DrawSeparator(SepPos + FontDescr.ChHeight - 1);
end;

function TFvcBrowser.WriteStringOut(var S: BRLRowEltString; LineNr: word; XOfs: integer): word;
begin
  Result := GetTextOutPosY(LineNr);
  Canvas.TextOut(XOfs + FTextMargin.Left, Result, S);
end;

function TFvcBrowser.XYPosInRect(X, Y: Integer; Rect: TRect): Boolean;
begin
  Result := (X >= Rect.Left) and (X < Rect.Right)
    and (Y >= Rect.Top) and (Y < Rect.Bottom);
end;

procedure TFvcBrowser.Paint;
var
  I,
  CR : Word;
begin
  Canvas.Font := Font;
  if Assigned(BrowserPtr) then begin
    CR := BrowserPtr^.GetCurRow;
    for I := 1 to FullPage do begin
      DisplayRow (I, I = CR);
    end;
  end;
end;

procedure TFvcBrowser.FileblockHasClosed;                      {!!.54}
begin
  FBIsClosed := true;
end;

end.
