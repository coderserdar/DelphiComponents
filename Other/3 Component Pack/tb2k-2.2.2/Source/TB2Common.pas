unit TB2Common;

{
  Toolbar2000
  Copyright (C) 1998-2008 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2Common.pas,v 1.48 2008/09/17 19:46:30 jr Exp $
}

interface

{$I TB2Ver.inc}

uses
  Windows, Classes, SysUtils, Messages, Controls, Forms;

type
  THandleWMPrintNCPaintProc = procedure(Wnd: HWND; DC: HDC; AppData: TObject);
  TPaintHandlerProc = procedure(var Message: TWMPaint) of object;

  { The type of item a TList holds; it differs between Win32 and .NET VCL }
  TListItemType = {$IFNDEF CLR} Pointer {$ELSE} TObject {$ENDIF};

  {$IFNDEF CLR}
  ClipToLongint = Longint;
  {$ENDIF}

function AddToFrontOfList(var List: TList; Item: TObject): Boolean;
function AddToList(var List: TList; Item: TObject): Boolean;
function ApplicationIsActive: Boolean;
function AreFlatMenusEnabled: Boolean;
function AreKeyboardCuesEnabled: Boolean;
procedure CallLockSetForegroundWindow(const ALock: Boolean);
function CallTrackMouseEvent(const Wnd: HWND; const Flags: DWORD): Boolean;
function CharToLower(const C: Char): Char;
{$IFDEF CLR}
function ClipToLongint(const I: Int64): Longint; inline;
{$ENDIF}
function CreateHalftoneBrush: HBRUSH;
function CreateMonoBitmap(const AWidth, AHeight: Integer;
  const ABits: array of Byte): HBITMAP;
function CreateRotatedFont(DC: HDC): HFONT;
procedure DoubleBufferedRepaint(const Wnd: HWND);
procedure DrawHalftoneInvertRect(const DC: HDC; const NewRect, OldRect: TRect;
  const NewSize, OldSize: TSize);
procedure DrawRotatedText(const DC: HDC; AText: String; const ARect: TRect;
  const AFormat: Cardinal);
procedure DrawSmallWindowCaption(const Wnd: HWND; const DC: HDC;
  const ARect: TRect; const AText: String; const AActive: Boolean);
function DrawTextStr(const DC: HDC; const AText: String; var ARect: TRect;
  const AFormat: UINT): Integer;
function EscapeAmpersands(const S: String): String;
procedure FillRectWithGradient(const DC: HDC; const R: TRect;
  const StartColor, EndColor: TColorRef; const HorizontalDirection: Boolean);
function FindAccelChar(const S: String): Char;
{$IFNDEF JR_D5}
procedure FreeAndNil(var Obj);
{$ENDIF}
function GetInputLocaleCodePage: UINT;
function GetMenuShowDelay: Integer;
function GetMessagePosAsPoint: TPoint;
function GetRectOfMonitorContainingPoint(const P: TPoint; const WorkArea: Boolean): TRect;
function GetRectOfMonitorContainingRect(const R: TRect; const WorkArea: Boolean): TRect;
function GetRectOfMonitorContainingWindow(const W: HWND; const WorkArea: Boolean): TRect;
function GetRectOfPrimaryMonitor(const WorkArea: Boolean): TRect;
function GetSystemNonClientMetrics(var Metrics: TNonClientMetrics): Boolean;
function GetSystemParametersInfoBool(const Param: UINT; const Default: BOOL): BOOL;
function GetTextExtentPoint32Str(const DC: HDC; const AText: String;
  out ASize: TSize): BOOL;
function GetTextHeight(const DC: HDC): Integer;
function GetTextWidth(const DC: HDC; S: String; const Prefix: Boolean): Integer;
procedure HandleWMPrint(const Wnd: HWND; var Message: TMessage;
  const NCPaintFunc: THandleWMPrintNCPaintProc; const AppData: TObject);
procedure HandleWMPrintClient(const PaintHandlerProc: TPaintHandlerProc;
  const Message: {$IFNDEF CLR} TMessage {$ELSE} TWMPrintClient {$ENDIF});
function IsWindowsXP: Boolean;
procedure InitTrackMouseEvent;
{$IFNDEF JR_D6}
function InvalidPoint(const At: TPoint): Boolean;
{$ENDIF}
function IsFillRectWithGradientAvailable: Boolean;
function Max(A, B: Integer): Integer;
function Min(A, B: Integer): Integer;
{$IFNDEF CLR}
function MethodsEqual(const M1, M2: TMethod): Boolean;
{$ENDIF}
function NeedToPlaySound(const Alias: String): Boolean;
procedure PlaySystemSound(const Alias: String);
procedure ProcessPaintMessages;
{$IFNDEF JR_D6}
procedure RaiseLastOSError;
{$ENDIF}
procedure RemoveMessages(const AMin, AMax: Integer);
procedure RemoveFromList(var List: TList; Item: TObject);
procedure SelectNCUpdateRgn(Wnd: HWND; DC: HDC; Rgn: HRGN);
function StripAccelChars(const S: String): String;
function StripTrailingPunctuation(const S: String): String;
function TextOutStr(const DC: HDC; const X, Y: Integer;
  const AText: String): BOOL;
function UsingMultipleMonitors: Boolean;

const
  PopupMenuWindowNCSize = 3;
  DT_HIDEPREFIX = $00100000;

implementation

uses
  {$IFDEF CLR} Types, System.Security, System.Runtime.InteropServices,
    System.Text, MultiMon, {$ENDIF}
  MMSYSTEM, TB2Version;

function ApplicationIsActive: Boolean;
{ Returns True if the application is in the foreground }
begin
  Result := GetActiveWindow <> 0;
end;

type
  {$IFNDEF CLR}
  PPrintEnumProcData = ^TPrintEnumProcData;
  TPrintEnumProcData = record
  {$ELSE}
  TPrintEnumProcData = class
  private
  {$ENDIF}
    PrintChildren: Boolean;
    ParentWnd: HWND;
    DC: HDC;
    PrintFlags: LPARAM;
    {$IFDEF CLR}
    function PrintEnumProc(Wnd: HWND; LParam: LPARAM): BOOL;
    {$ENDIF}
  end;

{$IFNDEF CLR}
function PrintEnumProc(Wnd: HWND; LParam: LPARAM): BOOL; stdcall;
{$ELSE}
function TPrintEnumProcData.PrintEnumProc(Wnd: HWND; LParam: LPARAM): BOOL;
{$ENDIF}
var
  R: TRect;
  SaveIndex: Integer;
begin
  Result := True;  { continue enumerating }
  {$IFNDEF CLR}
  with PPrintEnumProcData(LParam)^ do
  {$ENDIF}
  begin
    { Skip window if it isn't a child/owned window of ParentWnd or isn't visible }
    if (HWND(GetWindowLong(Wnd, GWL_HWNDPARENT)) <> ParentWnd) or
       (GetWindowLong(Wnd, GWL_STYLE) and WS_VISIBLE = 0) then
         { ^ don't use IsWindowVisible since it returns False if the window's
           parent window is not visible }
      Exit;
    GetWindowRect(Wnd, R);
    MapWindowPoints(0, ParentWnd, R, 2);
    SaveIndex := SaveDC(DC);
    { Like Windows, offset the window origin to the top-left coordinates of
      the child/owned window }
    MoveWindowOrg(DC, R.Left, R.Top);
    { Like Windows, intersect the clipping region with the entire rectangle of
      the child/owned window }
    OffsetRect(R, -R.Left, -R.Top);
    IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
    { Send a WM_PRINT message to the child/owned window }
    SendMessage(Wnd, WM_PRINT, WPARAM(DC), PrintFlags);
    { Restore the DC's state, in case the WM_PRINT handler didn't put things
      back the way it found them }
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure HandleWMPrint(const Wnd: HWND; var Message: TMessage;
  const NCPaintFunc: THandleWMPrintNCPaintProc; const AppData: TObject);
{ note: AppData is an application-defined value which is passed to NCPaintFunc }
var
  DC: HDC;
  SaveIndex, SaveIndex2: Integer;
  R: TRect;
  P: TPoint;
  Data: TPrintEnumProcData;
begin
  if (Message.LParam and PRF_CHECKVISIBLE = 0) or IsWindowVisible(Wnd) then begin
    DC := HDC(Message.WParam);
    SaveIndex2 := SaveDC(DC);
    try
      if Message.LParam and PRF_NONCLIENT <> 0 then begin
        SaveIndex := SaveDC(DC);
        if Assigned(NCPaintFunc) then
          NCPaintFunc(Wnd, DC, AppData);
        RestoreDC(DC, SaveIndex);
      end;
      { Calculate the difference between the top-left corner of the window
        and the top-left corner of its client area }
      GetWindowRect(Wnd, R);
      P.X := 0;  P.Y := 0;
      ClientToScreen(Wnd, P);
      Dec(P.X, R.Left);  Dec(P.Y, R.Top);
      if Message.LParam and PRF_CLIENT <> 0 then begin
        { Like Windows, the flags PRF_ERASEBKGND, PRF_CHILDREN, and PRF_OWNED
          are ignored if PRF_CLIENT isn't also specified }
        if Message.LParam and PRF_ERASEBKGND <> 0 then begin
          { Send WM_ERASEBKGND }
          SaveIndex := SaveDC(DC);
          if Message.LParam and PRF_NONCLIENT <> 0 then
            MoveWindowOrg(DC, P.X, P.Y);
          SendMessage(Wnd, WM_ERASEBKGND, Message.WParam, 0);
          RestoreDC(DC, SaveIndex);
        end;
        { Send WM_PRINTCLIENT }
        SaveIndex := SaveDC(DC);
        if Message.LParam and PRF_NONCLIENT <> 0 then
          MoveWindowOrg(DC, P.X, P.Y);
        SendMessage(Wnd, WM_PRINTCLIENT, Message.WParam, 0);
        RestoreDC(DC, SaveIndex);
        { Like Windows, always offset child/owned windows by the size of the
          client area even if PRF_NONCLIENT isn't specified (a bug?) }
        MoveWindowOrg(DC, P.X, P.Y);
        {$IFDEF CLR}
        Data := TPrintEnumProcData.Create;
        {$ENDIF}
        Data.ParentWnd := Wnd;
        Data.DC := DC;
        { Send WM_PRINT to child/owned windows }
        if Message.LParam and PRF_CHILDREN <> 0 then begin
          Data.PrintChildren := True;
          Data.PrintFlags := (Message.LParam or (PRF_NONCLIENT or PRF_CLIENT or
            PRF_ERASEBKGND or PRF_CHILDREN)) and not PRF_CHECKVISIBLE;
          {$IFNDEF CLR}
          EnumChildWindows(Wnd, @PrintEnumProc, LPARAM(@Data));
          {$ELSE}
          EnumChildWindows(Wnd, Data.PrintEnumProc, 0);
          {$ENDIF}
        end;
        if Message.LParam and PRF_OWNED <> 0 then begin
          Data.PrintChildren := False;
          Data.PrintFlags := Message.LParam;
          {$IFNDEF CLR}
          EnumWindows(@PrintEnumProc, LPARAM(@Data));
          {$ELSE}
          EnumWindows(Data.PrintEnumProc, 0);
          {$ENDIF}
        end;
      end;
    finally
      RestoreDC(DC, SaveIndex2);
    end;
    Message.Result := 1;
  end
  else begin
    { Like Windows, return 0 when the PRF_CHECKVISIBLE flag is specified and
      the window is not visible }
    Message.Result := 0;
  end;
end;

procedure HandleWMPrintClient(const PaintHandlerProc: TPaintHandlerProc;
  const Message: {$IFNDEF CLR} TMessage {$ELSE} TWMPrintClient {$ENDIF});
var
  DC: HDC;
  Msg: TWMPaint;
  SaveIndex: Integer;
begin
  {$IFNDEF CLR}
  DC := HDC(Message.WParam);
  {$ELSE}
  DC := Message.DC;
  Msg := TWMPaint.Create;
  {$ENDIF}
  Msg.Msg := WM_PAINT;
  Msg.DC := DC;
  {$IFNDEF CLR}
  Msg.Unused := 0;
  {$ENDIF}
  Msg.Result := 0;
  SaveIndex := SaveDC(DC);
  try
    PaintHandlerProc(Msg);
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;

function GetTextHeight(const DC: HDC): Integer;
var
  TextMetric: TTextMetric;
begin
  GetTextMetrics(DC, TextMetric);
  Result := TextMetric.tmHeight;
end;

function StripAccelChars(const S: String): String;
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do begin
    {$IFNDEF JR_WIDESTR}
    if not(Result[I] in LeadBytes) then begin
    {$ENDIF}
      if Result[I] = '&' then
        Delete(Result, I, 1);
      Inc(I);
    {$IFNDEF JR_WIDESTR}
    end
    else
      Inc(I, 2);
    {$ENDIF}
  end;
end;

function EscapeAmpersands(const S: String): String;
{ Replaces any '&' characters with '&&' }
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do begin
    {$IFNDEF JR_WIDESTR}
    if not(Result[I] in LeadBytes) then begin
    {$ENDIF}
      if Result[I] = '&' then begin
        Inc(I);
        Insert('&', Result, I);
      end;
      Inc(I);
    {$IFNDEF JR_WIDESTR}
    end
    else
      Inc(I, 2);
    {$ENDIF}
  end;
end;

function StripTrailingPunctuation(const S: String): String;
{ Removes any colon (':') or ellipsis ('...') from the end of S and returns
  the resulting string }
var
  L: Integer;
begin
  Result := S;
  L := Length(Result);
  if (L > 1) and (Result[L] = ':') {$IFNDEF JR_WIDESTR} and (ByteType(Result, L) = mbSingleByte) {$ENDIF} then
    SetLength(Result, L-1)
  else if (L > 3) and (Result[L-2] = '.') and (Result[L-1] = '.') and
     (Result[L] = '.') {$IFNDEF JR_WIDESTR} and (ByteType(Result, L-2) = mbSingleByte) {$ENDIF} then
    SetLength(Result, L-3);
end;

function GetTextWidth(const DC: HDC; S: String; const Prefix: Boolean): Integer;
{ Returns the width of the specified string using the font currently selected
  into DC. If Prefix is True, it first removes "&" characters as necessary. }
var
  Size: TSize;
begin
  { This procedure is 10x faster than using DrawText with the DT_CALCRECT flag }
  if Prefix then
    S := StripAccelChars(S);
  GetTextExtentPoint32Str(DC, S, Size);
  Result := Size.cx;
end;

procedure ProcessPaintMessages;
{ Dispatches all pending WM_PAINT messages. In effect, this is like an
  'UpdateWindow' on all visible windows }
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, 0, WM_PAINT, WM_PAINT, PM_NOREMOVE) do begin
    case Integer(GetMessage(Msg, 0, WM_PAINT, WM_PAINT)) of
      -1: Break; { if GetMessage failed }
      0: begin
           { Repost WM_QUIT messages }
           PostQuitMessage(ClipToLongint(Msg.wParam));
           Break;
         end;
    end;
    DispatchMessage(Msg);
  end;
end;

procedure RemoveMessages(const AMin, AMax: Integer);
{ Removes any messages with the specified ID from the queue }
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, 0, AMin, AMax, PM_REMOVE) do begin
    if Msg.message = WM_QUIT then begin
      { Repost WM_QUIT messages }
      PostQuitMessage(ClipToLongint(Msg.wParam));
      Break;
    end;
  end;
end;

procedure SelectNCUpdateRgn(Wnd: HWND; DC: HDC; Rgn: HRGN);
var
  R: TRect;
  NewClipRgn: HRGN;
begin
  if (Rgn <> 0) and (Rgn <> 1) then begin
    GetWindowRect(Wnd, R);
    if SelectClipRgn(DC, Rgn) = ERROR then begin
      NewClipRgn := CreateRectRgnIndirect(R);
      SelectClipRgn(DC, NewClipRgn);
      DeleteObject(NewClipRgn);
    end;
    OffsetClipRgn(DC, -R.Left, -R.Top);
  end;
end;

function AddToList(var List: TList; Item: TObject): Boolean;
{ Returns True if Item didn't already exist in the list }
begin
  if List = nil then
    List := TList.Create;
  Result := List.IndexOf(Item) = -1;
  if Result then
    List.Add(Item);
end;

function AddToFrontOfList(var List: TList; Item: TObject): Boolean;
{ Returns True if Item didn't already exist in the list }
begin
  if List = nil then
    List := TList.Create;
  Result := List.IndexOf(Item) = -1;
  if Result then
    List.Insert(0, Item);
end;

procedure RemoveFromList(var List: TList; Item: TObject);
begin
  if Assigned(List) then begin
    List.Remove(Item);
    if List.Count = 0 then begin
      List.Free;
      List := nil;
    end;
  end;
end;

const
  DefaultMenuShowDelay = 400;
{$IFNDEF CLR}
var
  RegMenuShowDelay: Integer;
  RegMenuShowDelayInited: BOOL = False;
function GetMenuShowDelay: Integer;
  function ReadMenuShowDelayFromRegistry: Integer;
  var
    K: HKEY;
    Typ, DataSize: DWORD;
    Data: array[0..31] of Char;
    Res: Longint;
    E: Integer;
  begin
    Result := DefaultMenuShowDelay;
    if RegOpenKeyEx(HKEY_CURRENT_USER, 'Control Panel\Desktop', 0,
       KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
      DataSize := SizeOf(Data);
      Res := RegQueryValueEx(K, 'MenuShowDelay', nil, @Typ, @Data, @DataSize);
      RegCloseKey(K);
      if Res <> ERROR_FILE_NOT_FOUND then begin
        if (Res <> ERROR_SUCCESS) or (Typ <> REG_SZ) then
          Result := 0
        else begin
          Val(Data, Result, E);
          if E <> 0 then Result := 0;
        end;
      end;
    end;
  end;
begin
  if Lo(GetVersion) >= 4 then begin
    if not SystemParametersInfo(106{SPI_GETMENUSHOWDELAY}, 0, @Result, 0) then begin
      { SPI_GETMENUSHOWDELAY is only supported by Windows NT 4.0 and Windows 98.
        On Windows 95, it must use the registry to retrieve this setting. }
      if not RegMenuShowDelayInited then begin
        RegMenuShowDelay := ReadMenuShowDelayFromRegistry;
        InterlockedExchange(Integer(RegMenuShowDelayInited), Ord(True));
      end;
      Result := RegMenuShowDelay;
    end;
    if Result < 0 then Result := 0;
  end
  else
    Result := DefaultMenuShowDelay;
end;
{$ELSE}
function GetMenuShowDelay: Integer;
begin
  { Since .NET requires Windows 98 or later, we can assume that
    SPI_GETMENUSHOWDELAY is available }
  if not SystemParametersInfo(SPI_GETMENUSHOWDELAY, 0, Result, 0) then
    Result := DefaultMenuShowDelay;
end;
{$ENDIF}

function AreFlatMenusEnabled: Boolean;
{ Returns True if "flat menus" are enabled. Always returns False on pre-XP
  Windows versions. }
const
  SPI_GETFLATMENU = $1022;
begin
  { Interestingly, on Windows 2000, SystemParametersInfo(SPI_GETFLATMENU, ...)
    succeeds and can return True in pvParam^ if the proper bit is set in
    UserPreferencesMask. Since flat menus are not really used on Windows
    2000, call IsWindowsXP first to see if we're running at least XP. }
  Result := IsWindowsXP and GetSystemParametersInfoBool(SPI_GETFLATMENU, False);
end;

function AreKeyboardCuesEnabled: Boolean;
{ Returns True if "keyboard cues" are enabled. Always returns True on
  pre-2000 Windows versions. }
const
  SPI_GETKEYBOARDCUES = $100A;
begin
  Result := (Win32MajorVersion < 5) or
    GetSystemParametersInfoBool(SPI_GETKEYBOARDCUES, True);
end;

function CreateFrameRectRgn(const ARect: TRect; const ASize: TSize): HRGN;
var
  R: TRect;
  InsideRgn: HRGN;
begin
  if IsRectEmpty(ARect) then begin
    { The rectangle is empty, so simply return a normalized empty region }
    SetRectEmpty(R);
    Result := CreateRectRgnIndirect(R);
  end
  else begin
    Result := CreateRectRgnIndirect(ARect);
    if Result <> 0 then begin
      { Now hollow out the resulting region so that only a frame is left }
      R := ARect;
      InflateRect(R, -ASize.cx, -ASize.cy);
      { If ASize is greater than the size of ARect, then InflateRect will
        return a non-normalized rectangle larger than ARect. Test for this
        condition by calling IsRectEmpty. }
      if not IsRectEmpty(R) then begin
        InsideRgn := CreateRectRgnIndirect(R);
        if InsideRgn <> 0 then begin
          CombineRgn(Result, Result, InsideRgn, RGN_XOR);
          DeleteObject(InsideRgn);
        end;
      end;
    end;
  end;
end;

procedure DrawInvertRect(const DC: HDC; const NewRect, OldRect: TRect;
  const NewSize, OldSize: TSize; const Brush: HBRUSH);
{ Draws a dragging outline, hiding the old one if necessary. NewRect and/or
  OldRect may be empty. }
var
  SaveIndex: Integer;
  UpdateRgn, OldRgn: HRGN;
  R: TRect;
begin
  { Create region containing the new rectangle }
  UpdateRgn := CreateFrameRectRgn(NewRect, NewSize);
  if UpdateRgn <> 0 then begin
    { Combine that region with a region containing the old rectangle }
    OldRgn := CreateFrameRectRgn(OldRect, OldSize);
    if OldRgn <> 0 then begin
      CombineRgn(UpdateRgn, OldRgn, UpdateRgn, RGN_XOR);
      DeleteObject(OldRgn);
    end;

    { Save the DC state so that the clipping region can be restored }
    SaveIndex := SaveDC(DC);
    try
      { Draw the updated region }
      SelectClipRgn(DC, UpdateRgn);
      GetClipBox(DC, R);
      SelectObject(DC, Brush);
      PatBlt(DC, R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top, PATINVERT);
    finally
      RestoreDC(DC, SaveIndex);
    end;

    DeleteObject(UpdateRgn);
  end;
end;

function CreateMonoBitmap(const AWidth, AHeight: Integer;
  const ABits: array of Byte): HBITMAP;
begin
  {$IFNDEF CLR}
  Result := CreateBitmap(AWidth, AHeight, 1, 1, @ABits[0]);
  {$ELSE}
  { For some reason there isn't an overloaded version of CreateBitmap that
    takes a TBytes parameter, so we have to use two calls }
  Result := CreateBitmap(AWidth, AHeight, 1, 1, nil);
  SetBitmapBits(Result, Length(ABits), ABits);
  {$ENDIF}
end;

function CreateHalftoneBrush: HBRUSH;
const
  GrayPattern: array[0..15] of Byte = (
    $55, 0, $AA, 0, $55, 0, $AA, 0, $55, 0, $AA, 0, $55, 0, $AA, 0);
var
  GrayBitmap: HBITMAP;
begin
  GrayBitmap := CreateMonoBitmap(8, 8, GrayPattern);
  Result := CreatePatternBrush(GrayBitmap);
  DeleteObject(GrayBitmap);
end;

procedure DrawHalftoneInvertRect(const DC: HDC; const NewRect, OldRect: TRect;
  const NewSize, OldSize: TSize);
var
  Brush: HBRUSH;
begin
  Brush := CreateHalftoneBrush;
  try
    DrawInvertRect(DC, NewRect, OldRect, NewSize, OldSize, Brush);
  finally
    DeleteObject(Brush);
  end;
end;

var
  GradientFillAvailable: Boolean;
{$IFNDEF CLR}
type
  { Note: TTriVertex is unusable on Delphi 7 and earlier (COLOR16 is
    misdeclared as a Shortint instead of a Word). }
  TNewTriVertex = record
    x: Longint;
    y: Longint;
    Red: Word;
    Green: Word;
    Blue: Word;
    Alpha: Word;
  end;
var
  GradientFillFunc: function(DC: HDC; var Vertex: TNewTriVertex;
    NumVertex: ULONG; Mesh: Pointer; NumMesh, Mode: ULONG): BOOL; stdcall;
{$ENDIF}

procedure InitGradientFillFunc;
{$IFNDEF CLR}
var
  M: HMODULE;
{$ENDIF}
begin
  if (Win32MajorVersion >= 5) or
     ((Win32MajorVersion = 4) and (Win32MinorVersion >= 10)) then begin
    {$IFNDEF CLR}
    M := {$IFDEF JR_D5} SafeLoadLibrary {$ELSE} LoadLibrary {$ENDIF} ('msimg32.dll');
    if M <> 0 then begin
      GradientFillFunc := GetProcAddress(M, 'GradientFill');
      if Assigned(GradientFillFunc) then
        GradientFillAvailable := True;
    end;
    {$ELSE}
    GradientFillAvailable := True;
    {$ENDIF}
  end;
end;

function IsFillRectWithGradientAvailable: Boolean;
begin
  Result := GradientFillAvailable;
end;

procedure FillRectWithGradient(const DC: HDC; const R: TRect;
  const StartColor, EndColor: TColorRef; const HorizontalDirection: Boolean);
var
  Vertexes: array[0..1] of {$IFNDEF CLR} TNewTriVertex {$ELSE} TTriVertex {$ENDIF};
  GradientRect: TGradientRect;
  Mode: ULONG;
begin
  if not GradientFillAvailable then
    Exit;
  Vertexes[0].x := R.Left;
  Vertexes[0].y := R.Top;
  Vertexes[0].Red := GetRValue(StartColor) shl 8;
  Vertexes[0].Blue := GetBValue(StartColor) shl 8;
  Vertexes[0].Green := GetGValue(StartColor) shl 8;
  Vertexes[0].Alpha := 0;
  Vertexes[1].x := R.Right;
  Vertexes[1].y := R.Bottom;
  Vertexes[1].Red := GetRValue(EndColor) shl 8;
  Vertexes[1].Blue := GetBValue(EndColor) shl 8;
  Vertexes[1].Green := GetGValue(EndColor) shl 8;
  Vertexes[1].Alpha := 0;
  GradientRect.UpperLeft := 0;
  GradientRect.LowerRight := 1;
  if HorizontalDirection then
    Mode := GRADIENT_FILL_RECT_H
  else
    Mode := GRADIENT_FILL_RECT_V;
  {$IFNDEF CLR}
  GradientFillFunc(DC, Vertexes[0], 2, @GradientRect, 1, Mode);
  {$ELSE}
  GradientFill(DC, Vertexes, 2, GradientRect, 1, Mode);
  {$ENDIF}
end;

procedure DrawSmallWindowCaption(const Wnd: HWND; const DC: HDC;
  const ARect: TRect; const AText: String; const AActive: Boolean);
{ Draws a (non-themed) small window caption bar.
  On Windows Vista, a custom routine is used to work around an ugly bug in
  DrawCaption that causes the text to be painted at the wrong coordinates.
  Note: The value of the AText parameter may be ignored depending on which
  routine is chosen. }

  procedure FillBackground;
  const
    CaptionBkColors: array[Boolean, Boolean] of Integer =
      ((COLOR_INACTIVECAPTION, COLOR_ACTIVECAPTION),
       (COLOR_GRADIENTINACTIVECAPTION, COLOR_GRADIENTACTIVECAPTION));
  var
    LeftColor, RightColor: TColorRef;
  begin
    if GetSystemParametersInfoBool(SPI_GETGRADIENTCAPTIONS, False) and
       IsFillRectWithGradientAvailable then begin
      LeftColor := GetSysColor(CaptionBkColors[False, AActive]);
      RightColor := GetSysColor(CaptionBkColors[True, AActive]);
      if LeftColor <> RightColor then begin
        FillRectWithGradient(DC, ARect, LeftColor, RightColor, True);
        Exit;
      end;
    end;
    FillRect(DC, ARect, GetSysColorBrush(CaptionBkColors[False, AActive]));
  end;

const
  CaptionTextColors: array[Boolean] of Integer =
    (COLOR_INACTIVECAPTIONTEXT, COLOR_CAPTIONTEXT);
var
  Flags: UINT;
  TextRect: TRect;
  NonClientMetrics: TNonClientMetrics;
  CaptionFont, SaveFont: HFONT;
  SaveBkMode: Integer;
  SaveTextColor: TColorRef;
begin
  if ARect.Right <= ARect.Left then
    Exit;

  { Prior to Windows Vista, continue to use DrawCaption. Don't want to risk
    introducing new bugs on old OSes, plus on Windows 98, it's several times
    faster than our custom routine. }
  if Win32MajorVersion < 6 then begin
    Flags := DC_TEXT or DC_SMALLCAP;
    if AActive then
      Flags := Flags or DC_ACTIVE;
    if GetSystemParametersInfoBool(SPI_GETGRADIENTCAPTIONS, False) then
      Flags := Flags or DC_GRADIENT;
    DrawCaption(Wnd, DC, ARect, Flags);
  end
  else begin
    FillBackground;
    TextRect := ARect;
    Inc(TextRect.Left, GetSystemMetrics(SM_CXEDGE));
    if (TextRect.Right > TextRect.Left) and
       GetSystemNonClientMetrics(NonClientMetrics) then begin
      CaptionFont := CreateFontIndirect(NonClientMetrics.lfSmCaptionFont);
      if CaptionFont <> 0 then begin
        SaveFont := SelectObject(DC, CaptionFont);
        SaveBkMode := SetBkMode(DC, TRANSPARENT);
        SaveTextColor := SetTextColor(DC, GetSysColor(CaptionTextColors[AActive]));
        try
          DrawTextStr(DC, AText, TextRect, DT_SINGLELINE or DT_NOPREFIX or
            DT_VCENTER or DT_END_ELLIPSIS);
        finally
          SetTextColor(DC, SaveTextColor);
          SetBkMode(DC, SaveBkMode);
          SelectObject(DC, SaveFont);
          DeleteObject(CaptionFont);
        end;
      end;
    end;
  end;
end;

procedure DoubleBufferedRepaint(const Wnd: HWND);
var
  ClientRect, ClipRect, R: TRect;
  WndDC, BmpDC: HDC;
  Bmp: HBITMAP;
  SaveIndex: Integer;
begin
  if IsWindowVisible(Wnd) and GetClientRect(Wnd, ClientRect) and
     not IsRectEmpty(ClientRect) then begin
    ValidateRect(Wnd, nil);
    BmpDC := 0;
    Bmp := 0;
    WndDC := GetDC(Wnd);
    if WndDC <> 0 then begin
      try
        { Only repaint the area that intersects the clipping rectangle }
        if (GetClipBox(WndDC, ClipRect) <> Windows.ERROR) and
           IntersectRect(R, ClientRect, ClipRect) then begin
          Bmp := CreateCompatibleBitmap(WndDC, R.Right - R.Left, R.Bottom - R.Top);
          if Bmp <> 0 then begin
            BmpDC := CreateCompatibleDC(WndDC);
            if BmpDC <> 0 then begin
              SelectObject(BmpDC, Bmp);
              SaveIndex := SaveDC(BmpDC);
              SetWindowOrgEx(BmpDC, R.Left, R.Top, nil);
              SendMessage(Wnd, WM_ERASEBKGND, WPARAM(BmpDC), 0);
              SendMessage(Wnd, WM_PAINT, WPARAM(BmpDC), 0);
              RestoreDC(BmpDC, SaveIndex);
              BitBlt(WndDC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                BmpDC, 0, 0, SRCCOPY);
              Exit;
            end;
          end;
        end;
      finally
        if BmpDC <> 0 then DeleteDC(BmpDC);
        if Bmp <> 0 then DeleteObject(Bmp);
        ReleaseDC(Wnd, WndDC);
      end;
    end;
  end;
  { Fall back to invalidating if we didn't or couldn't double-buffer }
  InvalidateRect(Wnd, nil, True);
end;

{$IFNDEF CLR}
function MethodsEqual(const M1, M2: TMethod): Boolean;
begin
  Result := (M1.Code = M2.Code) and (M1.Data = M2.Data);
end;
{$ENDIF}

function GetRectOfPrimaryMonitor(const WorkArea: Boolean): TRect;
begin
  if not WorkArea or not SystemParametersInfo(SPI_GETWORKAREA, 0,
     {$IFNDEF CLR}@{$ENDIF} Result, 0) then
    Result := Rect(0, 0, Screen.Width, Screen.Height);
end;

{$IFNDEF CLR}
{ On Delphi for Win32, we don't use the MultiMon unit because its stubs for
  MonitorFromRect and MonitorFromPoint are seriously bugged on Delphi 4. }
type
  HMONITOR = type THandle;
  TMonitorInfo = record
    cbSize: DWORD;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWORD;
  end;
const
  MONITOR_DEFAULTTONEAREST = $2;
var
  MultiMonApis: record
    MonitorFromRect: function(const lprcScreenCoords: TRect; dwFlags: DWORD): HMONITOR; stdcall;
    MonitorFromPoint: function(ptScreenCoords: TPoint; dwFlags: DWORD): HMONITOR; stdcall;
    MonitorFromWindow: function(hWnd: HWND; dwFlags: DWORD): HMONITOR; stdcall;
    GetMonitorInfo: function(hMonitor: HMONITOR; var lpMonitorInfo: TMonitorInfo): BOOL; stdcall;
  end;
  MultiMonApisAvailable: Boolean;

procedure InitMultiMonApis;
var
  User32Handle: THandle;
begin
  User32Handle := GetModuleHandle(user32);
  MultiMonApis.MonitorFromRect := GetProcAddress(User32Handle, 'MonitorFromRect');
  MultiMonApis.MonitorFromPoint := GetProcAddress(User32Handle, 'MonitorFromPoint');
  MultiMonApis.MonitorFromWindow := GetProcAddress(User32Handle, 'MonitorFromWindow');
  MultiMonApis.GetMonitorInfo := GetProcAddress(User32Handle, 'GetMonitorInfoA');
  MultiMonApisAvailable := Assigned(MultiMonApis.MonitorFromRect) and
    Assigned(MultiMonApis.MonitorFromPoint) and
    Assigned(MultiMonApis.MonitorFromWindow) and
    Assigned(MultiMonApis.GetMonitorInfo);
end;
{$ENDIF}

function UsingMultipleMonitors: Boolean;
{ Returns True if the system has more than one display monitor configured. }
const
  SM_CMONITORS = 80;
begin
  { Note: On a single monitor Windows 95 or NT 4 system, GetSystemMetrics will
    return 0 since those OSes do not support multiple monitors. On later
    versions it returns 1. }
  Result := {$IFNDEF CLR} MultiMonApisAvailable and {$ENDIF}
    (GetSystemMetrics(SM_CMONITORS) > 1);
end;

function GetRectOfMonitor(const M: HMONITOR; const WorkArea: Boolean;
  var R: TRect): Boolean;
var
  MonitorInfo: TMonitorInfo;
begin
  {$IFNDEF CLR}
  MonitorInfo.cbSize := SizeOf(MonitorInfo);
  {$ELSE}
  MonitorInfo.cbSize := Marshal.SizeOf(TypeOf(TMonitorInfo));
  {$ENDIF}
  Result := {$IFNDEF CLR}MultiMonApis.{$ENDIF} GetMonitorInfo(M, MonitorInfo);
  if Result then begin
    if not WorkArea then
      R := MonitorInfo.rcMonitor
    else
      R := MonitorInfo.rcWork;
  end;
end;

function GetRectOfMonitorContainingRect(const R: TRect;
  const WorkArea: Boolean): TRect;
{ Returns the work area of the monitor which the rectangle R intersects with
  the most, or the monitor nearest R if no monitors intersect. }
var
  M: HMONITOR;
begin
  if UsingMultipleMonitors then begin
    M := {$IFNDEF CLR}MultiMonApis.{$ENDIF} MonitorFromRect(R, MONITOR_DEFAULTTONEAREST);
    if GetRectOfMonitor(M, WorkArea, Result) then
      Exit;
  end;
  Result := GetRectOfPrimaryMonitor(WorkArea);
end;

function GetRectOfMonitorContainingPoint(const P: TPoint;
  const WorkArea: Boolean): TRect;
{ Returns the screen area of the monitor containing the point P, or the monitor
  nearest P if P isn't in any monitor's work area. }
var
  M: HMONITOR;
begin
  if UsingMultipleMonitors then begin
    M := {$IFNDEF CLR}MultiMonApis.{$ENDIF} MonitorFromPoint(P, MONITOR_DEFAULTTONEAREST);
    if GetRectOfMonitor(M, WorkArea, Result) then
      Exit;
  end;
  Result := GetRectOfPrimaryMonitor(WorkArea);
end;

function GetRectOfMonitorContainingWindow(const W: HWND;
  const WorkArea: Boolean): TRect;
var
  M: HMONITOR;
begin
  if UsingMultipleMonitors then begin
    M := {$IFNDEF CLR}MultiMonApis.{$ENDIF} MonitorFromWindow(W, MONITOR_DEFAULTTONEAREST);
    if GetRectOfMonitor(M, WorkArea, Result) then
      Exit;
  end;
  Result := GetRectOfPrimaryMonitor(WorkArea);
end;

{$IFNDEF CLR}
var
  TrackMouseEventInited: BOOL;
  TrackMouseEventFunc: function(var EventTrack: TTrackMouseEvent): BOOL; stdcall;

procedure InitTrackMouseEvent;
var
  TrackMouseEventComCtlModule: THandle;
begin
  { First look for TrackMouseEvent which is available on Windows 98 & NT 4 only.
    If it doesn't exist, look for _TrackMouseEvent which is available on
    Windows 95 if IE 3.0 or later is installed. }
  if not TrackMouseEventInited then begin
    TrackMouseEventFunc := GetProcAddress(GetModuleHandle(user32),
      'TrackMouseEvent');
    if @TrackMouseEventFunc = nil then begin
      TrackMouseEventComCtlModule :=
        {$IFDEF JR_D5} SafeLoadLibrary {$ELSE} LoadLibrary {$ENDIF} (comctl32);
      if TrackMouseEventComCtlModule <> 0 then
        TrackMouseEventFunc := GetProcAddress(TrackMouseEventComCtlModule,
          '_TrackMouseEvent');
    end;
    InterlockedExchange(Integer(TrackMouseEventInited), Ord(True));
  end;
end;
{$ELSE}
procedure InitTrackMouseEvent;
begin
end;
{$ENDIF}

function CallTrackMouseEvent(const Wnd: HWND; const Flags: DWORD): Boolean;
var
  Track: TTrackMouseEvent;
begin
  {$IFNDEF CLR}
  Result := False;
  if Assigned(TrackMouseEventFunc) then begin
    Track.cbSize := SizeOf(Track);
    Track.dwFlags := Flags;
    Track.hwndTrack := Wnd;
    Track.dwHoverTime := 0;
    Result := TrackMouseEventFunc(Track);
  end;
  {$ELSE}
  { .NET doesn't run on 95, so we can assume TrackMouseEvent is available }
  Track.cbSize := Marshal.SizeOf(TypeOf(TTrackMouseEvent));
  Track.dwFlags := Flags;
  Track.hwndTrack := Wnd;
  Track.dwHoverTime := 0;
  Result := TrackMouseEvent(Track);
  {$ENDIF}
end;

{$IFNDEF CLR}
var
  LockSetForegroundWindowFunc: function(uLockCode: UINT): BOOL; stdcall;
{$ELSE}
[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'LockSetForegroundWindow')]
function LockSetForegroundWindowFunc(uLockCode: UINT): BOOL; external;
{$ENDIF}

procedure CallLockSetForegroundWindow(const ALock: Boolean);
const
  LSFW_LOCK = 1;
  LSFW_UNLOCK = 2;
begin
  {$IFNDEF CLR}
  if Assigned(LockSetForegroundWindowFunc) then begin
  {$ELSE}
  if (Win32MajorVersion >= 5) or
     ((Win32MajorVersion = 4) and (Win32MinorVersion >= 90)) then begin
  {$ENDIF}
    if ALock then
      LockSetForegroundWindowFunc(LSFW_LOCK)
    else
      LockSetForegroundWindowFunc(LSFW_UNLOCK);
  end;
end;

{$IFNDEF JR_D5}
procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;
  P.Free;
end;
{$ENDIF}

{$IFNDEF JR_D6}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF}

{$IFDEF CLR}
{ On .NET, when calling DrawText, GetTextExtentPoint32, or TextOut we can't
  rely on the marshaller's automatic A/W function selection because they take
  a character count. If we passed the result of Length(), as the VCL
  incorrectly does in many places, the behavior would be incorrect on DBCS
  Windows 9x/Me systems because when a Unicode string is downconverted to ANSI
  the character count can increase (i.e. one Unicode character can become two
  ANSI characters).
  Below we define our own "A" function prototypes that take byte array
  parameters, allowing us to pass the result of AnsiEncoding.GetBytes straight
  to the functions without any conversion. (Borland's "A" prototypes use
  "string" type parameters.) }
[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'DrawTextA')]
function _DrawTextA(hDC: HDC; [in] lpString: TBytes; nCount: Integer;
  var lpRect: TRect; uFormat: UINT): Integer; external;
[SuppressUnmanagedCodeSecurity, DllImport(gdi32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'GetTextExtentPoint32A')]
function _GetTextExtentPoint32A(DC: HDC; [in] Str: TBytes; Count: Integer;
  out Size: TSize): BOOL; external;
[SuppressUnmanagedCodeSecurity, DllImport(gdi32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'TextOutA')]
function _TextOutA(DC: HDC; X, Y: Integer; [in] Str: TBytes;
  Count: Integer): BOOL; external;
{$ENDIF}

function DrawTextStr(const DC: HDC; const AText: String; var ARect: TRect;
  const AFormat: UINT): Integer;
{$IFNDEF CLR}
begin
  Result := DrawText(DC, PChar(AText), Length(AText), ARect, AFormat);
end;
{$ELSE}
var
  AnsiStr: TBytes;
begin
  if Marshal.SystemDefaultCharSize = 1 then begin
    AnsiStr := AnsiEncoding.GetBytes(AText);
    Result := _DrawTextA(DC, AnsiStr, Length(AnsiStr), ARect, AFormat);
  end
  else
    Result := DrawTextW(DC, AText, Length(AText), ARect, AFormat);
end;
{$ENDIF}

function GetTextExtentPoint32Str(const DC: HDC; const AText: String;
  out ASize: TSize): BOOL;
{$IFNDEF CLR}
begin
  Result := GetTextExtentPoint32(DC, PChar(AText), Length(AText), ASize);
end;
{$ELSE}
var
  AnsiStr: TBytes;
begin
  if Marshal.SystemDefaultCharSize = 1 then begin
    AnsiStr := AnsiEncoding.GetBytes(AText);
    Result := _GetTextExtentPoint32A(DC, AnsiStr, Length(AnsiStr), ASize);
  end
  else
    Result := GetTextExtentPoint32W(DC, AText, Length(AText), ASize);
end;
{$ENDIF}

function TextOutStr(const DC: HDC; const X, Y: Integer;
  const AText: String): BOOL;
{$IFNDEF CLR}
begin
  Result := TextOut(DC, X, Y, PChar(AText), Length(AText));
end;
{$ELSE}
var
  AnsiStr: TBytes;
begin
  if Marshal.SystemDefaultCharSize = 1 then begin
    AnsiStr := AnsiEncoding.GetBytes(AText);
    Result := _TextOutA(DC, X, Y, AnsiStr, Length(AnsiStr));
  end
  else
    Result := TextOutW(DC, X, Y, AText, Length(AText));
end;
{$ENDIF}

threadvar
  FontExistsResult: Boolean;

{$IFNDEF CLR}
function FontExistsCallback(const lplf: TLogFont; const lptm: TTextMetric;
  dwType: DWORD; lpData: LPARAM): Integer; stdcall;
{$ELSE}
function FontExistsCallback([in] var lplf: TLogFont; [in] var lptm: TTextMetric;
  dwType: DWORD; lpData: LPARAM): Integer;
{$ENDIF}
begin
  FontExistsResult := True;
  Result := 0;
end;

function FontExists(const DC: HDC; const FaceName: String): Boolean;
begin
  FontExistsResult := False;
  EnumFonts(DC, {$IFNDEF CLR}PChar{$ENDIF}(FaceName), @FontExistsCallback,
    {$IFNDEF CLR} nil {$ELSE} 0 {$ENDIF});
  Result := FontExistsResult;
end;

function CreateRotatedFont(DC: HDC): HFONT;
{ Creates a font based on the DC's current font, but rotated 270 degrees }
var
  LogFont: TLogFont;
  TM: TTextMetric;
  VerticalFontName: String;
begin
  if GetObject(GetCurrentObject(DC, OBJ_FONT),
     {$IFNDEF CLR}
     SizeOf(LogFont), @LogFont
     {$ELSE}
     Marshal.SizeOf(TypeOf(TLogFont)), LogFont
     {$ENDIF}
     ) = 0 then begin
    { just in case... }
    Result := 0;
    Exit;
  end;
  LogFont.lfEscapement := 2700;
  LogFont.lfOrientation := 2700;
  LogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;  { needed for Win9x }

  { Don't let a random TrueType font be substituted when MS Sans Serif or
    Microsoft Sans Serif are used. On Windows 2000 and later, hard-code Tahoma
    because Arial can't display Japanese or Thai Unicode characters (on Windows
    2000 at least). On earlier versions, hard-code Arial since NT 4.0 doesn't
    ship with Tahoma, and 9x doesn't do Unicode. }
  {$IFNDEF CLR}
  if (StrIComp(LogFont.lfFaceName, 'MS Sans Serif') = 0) or
     (StrIComp(LogFont.lfFaceName, 'Microsoft Sans Serif') = 0) then begin
    if Win32MajorVersion >= 5 then
      StrPCopy(LogFont.lfFaceName, 'Tahoma')
    else
      StrPCopy(LogFont.lfFaceName, 'Arial');
  {$ELSE}
  if SameText(LogFont.lfFaceName, 'MS Sans Serif', loInvariantLocale) or
     SameText(LogFont.lfFaceName, 'Microsoft Sans Serif', loInvariantLocale) then begin
    if Win32MajorVersion >= 5 then
      LogFont.lfFaceName := 'Tahoma'
    else
      LogFont.lfFaceName := 'Arial';
  {$ENDIF}
    { Set lfHeight to the actual height of the current font. This is needed
      to work around a Windows 98 issue: on a clean install of the OS,
      SPI_GETNONCLIENTMETRICS returns -5 for lfSmCaptionFont.lfHeight. This is
      wrong; it should return -11 for an 8 pt font. With normal, unrotated text
      this actually displays correctly, since MS Sans Serif doesn't support
      sizes below 8 pt. However, when we change to a TrueType font like Arial,
      this becomes a problem because it'll actually create a font that small. }
    if GetTextMetrics(DC, TM) then begin
      { If the original height was negative, keep it negative }
      if LogFont.lfHeight <= 0 then
        LogFont.lfHeight := -(TM.tmHeight - TM.tmInternalLeading)
      else
        LogFont.lfHeight := TM.tmHeight;
    end;
  end;

  { Use a vertical font if available so that Asian characters aren't drawn
    sideways }
  VerticalFontName := String('@') + LogFont.lfFaceName;
  if FontExists(DC, VerticalFontName) then begin
    {$IFNDEF CLR}
    StrPLCopy(LogFont.lfFaceName, VerticalFontName,
      (SizeOf(LogFont.lfFaceName) div SizeOf(LogFont.lfFaceName[0])) - 1);
    {$ELSE}
    LogFont.lfFaceName := VerticalFontName;
    {$ENDIF}
  end;

  Result := CreateFontIndirect(LogFont);
end;

procedure DrawRotatedText(const DC: HDC; AText: String; const ARect: TRect;
  const AFormat: Cardinal);
{ Like DrawText, but draws the text at a 270 degree angle.
  The only format flag this function respects is DT_HIDEPREFIX. Text is always
  drawn centered. }
var
  RotatedFont, SaveFont: HFONT;
  TextMetrics: TTextMetric;
  X, Y, P, I, SU, FU: Integer;
  SaveAlign: UINT;
  SavePen, Pen: HPEN;
begin
  RotatedFont := CreateRotatedFont(DC);
  SaveFont := SelectObject(DC, RotatedFont);

  GetTextMetrics(DC, TextMetrics);
  X := ARect.Left + ((ARect.Right - ARect.Left) - TextMetrics.tmHeight) div 2;
  Y := ARect.Top + ((ARect.Bottom - ARect.Top) - GetTextWidth(DC, AText, True)) div 2;

  { Find the index of the character that should be underlined. Delete '&'
    characters from the string. Like DrawText, only the last prefixed character
    will be underlined. }
  P := 0;
  I := 1;
  while I <= Length(AText) do begin
    {$IFNDEF JR_WIDESTR}
    if AText[I] in LeadBytes then
      Inc(I)
    else
    {$ENDIF}
    if AText[I] = '&' then begin
      Delete(AText, I, 1);
      { If the '&' was the last character, don't underline anything }
      if I > Length(AText) then
        P := 0
      else if AText[I] <> '&' then
        P := I;
    end;
    Inc(I);
  end;

  SaveAlign := SetTextAlign(DC, TA_BOTTOM);
  TextOutStr(DC, X, Y, AText);
  SetTextAlign(DC, SaveAlign);
  { Underline }
  if (P > 0) and (AFormat and DT_HIDEPREFIX = 0) then begin
    SU := GetTextWidth(DC, Copy(AText, 1, P-1), False);
    FU := SU + GetTextWidth(DC, AText[P], False);
    Inc(X, TextMetrics.tmDescent - 2);
    Pen := CreatePen(PS_SOLID, 1, GetTextColor(DC));
    SavePen := SelectObject(DC, Pen);
    MoveToEx(DC, X, Y + SU, nil);
    LineTo(DC, X, Y + FU);
    SelectObject(DC, SavePen);
    DeleteObject(Pen);
  end;

  SelectObject(DC, SaveFont);
  DeleteObject(RotatedFont);
end;

function NeedToPlaySound(const Alias: String): Boolean;
{ This function checks the registry to see if the specified sound event alias
  is assigned to a file.
  The purpose of having this function is so it can avoid calls to PlaySound if
  possible, because on Windows 2000 there is an annoying 1/3 second delay on
  the first call to PlaySound.
  Windows Explorer actually uses this same technique when playing sounds for
  the Start menu. }
var
  KeyName: String;
  K: HKEY;
  {$IFNDEF CLR}
  Data: array[0..3] of WideChar;
  {$ELSE}
  Data: TBytes;
  DataType: DWORD;
  {$ENDIF}
  DataSize: DWORD;
  ErrorCode: Longint;
begin
  if (Win32MajorVersion < 5) or (Win32Platform <> VER_PLATFORM_WIN32_NT) then begin
    { No need to check pre-Windows 2000 versions since their PlaySound
      functions don't have the delay; always return True. }
    Result := True;
    Exit;
  end;
  Result := False;
  KeyName := 'AppEvents\Schemes\Apps\.Default\' + Alias + '\.Current';
  if RegOpenKeyEx(HKEY_CURRENT_USER, {$IFNDEF CLR}PChar{$ENDIF}(KeyName),
     0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
    try
      {$IFNDEF CLR}
      DataSize := SizeOf(Data);
      { Note: Use the 'W' version of RegQueryValueEx for more speed }
      ErrorCode := RegQueryValueExW(K, nil, nil, nil, @Data, @DataSize);
      if ((ErrorCode = ERROR_SUCCESS) and (Data[0] <> #0)) or
         (ErrorCode = ERROR_MORE_DATA) then
        Result := True;
      {$ELSE}
      DataSize := 4 * SizeOf(WideChar);
      SetLength(Data, DataSize);
      ErrorCode := RegQueryValueExW(K, nil, nil, DataType, Data, DataSize);
      if ((ErrorCode = ERROR_SUCCESS) and (Data[0] or Data[1] <> 0)) or
         (ErrorCode = ERROR_MORE_DATA) then
        Result := True;
      {$ENDIF}
    finally
      RegCloseKey(K);
    end;
  end;
end;

procedure PlaySystemSound(const Alias: String);
const
  SND_SYSTEM = $00200000;
var
  Flags: DWORD;
begin
  Flags := SND_ALIAS or SND_ASYNC or SND_NODEFAULT;
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Flags := Flags or SND_NOSTOP;  { On 9x, native menus' sounds are NOSTOP } 
  if Win32MajorVersion >= 6 then
    Flags := Flags or SND_SYSTEM;
  PlaySound({$IFNDEF CLR}PChar{$ENDIF}(Alias), 0, Flags);
end;

function Max(A, B: Integer): Integer;
begin
  if A >= B then
    Result := A
  else
    Result := B;
end;

function Min(A, B: Integer): Integer;
begin
  if A <= B then
    Result := A
  else
    Result := B;
end;

function FindAccelChar(const S: String): Char;
{ Finds the last accelerator key in S. Returns #0 if no accelerator key was
  found. '&&' is ignored. }
{$IFNDEF CLR}
var
  P: PChar;
begin
  P := PChar(S);
  Result := #0;
  while True do begin
    P := AnsiStrScan(P, '&');
    if P = nil then Break;
    Inc(P);
    if P^ <> '&' then begin
      if P^ = #0 then Break;
      Result := P^;
    end;
    Inc(P);
  end;
end;
{$ELSE}
var
  Len, I: Integer;
begin
  Result := #0;
  Len := Length(S);
  if Len > 0 then begin  { ensures S isn't nil }
    I := 1;
    while True do begin
      I := System.String(S).IndexOf('&', I - 1) + 1;
      if (I = 0) or (I >= Len) then
        Break;
      Inc(I);
      if S[I] <> '&' then
        Result := S[I];
      Inc(I);
    end;
  end;
end;
{$ENDIF}

function IsWindowsXP: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
    ((Win32MajorVersion > 5) or
     ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1)));
end;

function GetInputLocaleCodePage: UINT;
{ Returns the code page identifier of the active input locale, or CP_ACP if
  for some unknown reason it couldn't be determined. }
var
  {$IFNDEF CLR}
  Buf: array[0..15] of Char;
  {$ELSE}
  Buf: StringBuilder;
  {$ENDIF}
  ErrorCode: Integer;
begin
  {$IFNDEF CLR}
  if GetLocaleInfo(GetKeyboardLayout(0) and $FFFF, LOCALE_IDEFAULTANSICODEPAGE,
     Buf, SizeOf(Buf) div SizeOf(Buf[0])) > 0 then begin
    Buf[High(Buf)] := #0;  { ensure null termination, just in case... }
    Val(Buf, Result, ErrorCode);
  {$ELSE}
  Buf := StringBuilder.Create(16);
  if GetLocaleInfo(GetKeyboardLayout(0) and $FFFF, LOCALE_IDEFAULTANSICODEPAGE,
     Buf, Buf.Capacity) > 0 then begin
    Val(Buf.ToString, Result, ErrorCode);
  {$ENDIF}
    { Just to be *completely* safe, verify that the code page returned by
      GetLocaleInfo actually exists. The result of this function may be fed
      into WideCharToMultiByte, and we don't want WideCharToMultiByte to fail
      entirely because of a bad code page. }
    if (ErrorCode <> 0) or not IsValidCodePage(Result) then
      Result := CP_ACP;
  end
  else
    Result := CP_ACP;
end;

function GetMessagePosAsPoint: TPoint;
var
  Pos: DWORD;
begin
  Pos := GetMessagePos;
  Result.X := Smallint(Pos and $FFFF);
  Result.Y := Smallint(Pos shr 16);
end;

function GetSystemNonClientMetrics(var Metrics: TNonClientMetrics): Boolean;
{$IFNDEF CLR}
begin
  Metrics.cbSize := SizeOf(Metrics);
  Result := SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(Metrics),
    @Metrics, 0);
end;
{$ELSE}
begin
  {$IFDEF JR_D11}
  { On Delphi.NET 2007, Forms.GetNonClientMetrics is marked deprecated }
  Metrics.cbSize := Marshal.SizeOf(TypeOf(TNonClientMetrics));
  Result := SystemParametersInfo(SPI_GETNONCLIENTMETRICS, Metrics.cbSize,
    Metrics, 0);
  {$ELSE}
  Result := Forms.GetNonClientMetrics(Metrics);
  {$ENDIF}
end;
{$ENDIF}

function GetSystemParametersInfoBool(const Param: UINT; const Default: BOOL): BOOL;
{ Returns the value of the specified BOOL-type system parameter, or Default
  if the function fails }
begin
  if not SystemParametersInfo(Param, 0, {$IFNDEF CLR}@{$ENDIF} Result, 0) then
    Result := Default;
end;

{$IFDEF CLR}
{ Use our own declaration for CharLowerBuffA that takes a byte array directly
  instead of StringBuilder }
[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'CharLowerBuffA')]
function _CharLowerBuffA([in, out] lpsz: TBytes; cchLength: DWORD): DWORD; external;
{$ENDIF}

function CharToLower(const C: Char): Char;
{ Converts a single character to lowercase using the current code page }
{$IFNDEF CLR}
begin
  Result := Char(CharLower(Pointer(Word(C))));
end;
{$ELSE}
var
  AnsiBytes: TBytes;
begin
  { Note: On .NET we can't use LowerCase()/String.ToLower() because it uses
    linguistic casing rules -- on a Turkish locale "I" is NOT mapped to "i".
    This would break accelerator keys when running English apps. With
    CharLower, "I" is always mapped to "i". }
  if Marshal.SystemDefaultCharSize = 1 then begin
    { On Windows 9x/Me we have to use CharLowerBuff since the character may be
      two bytes when downconverted to ANSI. And we have to handle the
      Unicode->ANSI conversion ourself so that we know the correct length to
      pass to the function. }
    AnsiBytes := AnsiEncoding.GetBytes(C);
    _CharLowerBuffA(AnsiBytes, Length(AnsiBytes));
    Result := AnsiEncoding.GetChars(AnsiBytes)[0];
  end
  else
    Result := Char(Word(CharLowerW(IntPtr(Word(C)))));
end;
{$ENDIF}

{$IFNDEF JR_D6}
function InvalidPoint(const At: TPoint): Boolean;
begin
  Result := (At.X = -1) and (At.Y = -1);
end;
{$ENDIF}

{$IFDEF CLR}
function ClipToLongint(const I: Int64): Longint; inline;
{ On Delphi.NET 2007, casting a LPARAM (THandle) directly into a Longint can
  raise an overflow exception (possibly a bug?). By passing the LPARAM to
  this function, which acts like a Longint(Int64()) cast, the exception can
  be avoided. }
begin
  Result := Longint(I);
end;
{$ENDIF}

initialization
  InitGradientFillFunc;
  {$IFNDEF CLR}
  InitMultiMonApis;
  LockSetForegroundWindowFunc := GetProcAddress(GetModuleHandle(user32),
    'LockSetForegroundWindow');
  {$ENDIF}
end.
