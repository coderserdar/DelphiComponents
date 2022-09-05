unit TB2Anim;

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

  $jrsoftware: tb2k/Source/TB2Anim.pas,v 1.13 2008/09/19 16:41:00 jr Exp $
}

interface

{$I TB2Ver.inc}
{$Q-}

uses
  Windows, Messages, SysUtils, Classes;

const
  WM_TB2K_ANIMATIONENDED = WM_USER + $556;

type
  TTBAnimationDirection = set of (tbadLeft, tbadRight, tbadDown, tbadUp);

procedure TBStartAnimation(const AWnd: HWND; const ABlend: Boolean;
  const ADirection: TTBAnimationDirection);
procedure TBUpdateAnimation;
procedure TBEndAnimation(const Wnd: HWND);
function TBIsAnimationInProgress: Boolean;

implementation

uses
  {$IFDEF CLR} System.Security, System.Runtime.InteropServices, System.Threading, {$ENDIF}
  TB2Common;

{ Notes to self:
  - It originally had the NOMIRRORBITMAP flag on the BitBlt calls, because
    Windows 2000's AnimateWindow function has it. But it had to be removed
    because on Windows 98 with the Standard VGA or VMware video driver, it
    caused no bits to be blitted, even though Windows 98 is supposed to
    support NOMIRRORBITMAP according to the documentation. I don't think it's
    necessary anyway.
}

const
  DCX_USESTYLE = $10000;
  WS_EX_LAYERED = $80000;
  NOMIRRORBITMAP = $80000000;
  ULW_ALPHA = 2;

type
  PAnimateThreadFuncData = ^TAnimateThreadFuncData;
  TAnimateThreadFuncData = record
    FRunning: Boolean;
    FWnd: HWND;
    FTime: Integer;
    FBlending: Boolean;
    FStartStep, FCurStep: Integer;
    FStartTime, FLastFrameTime: DWORD;
    FWndDC, FBmpDC: HDC;
    FBmp: HBITMAP;
    FScreenClientRect: TRect;
    FSize: TSize;
    FLastPos: TPoint;
    FDirection: TTBAnimationDirection;
  end;
  { Delphi.NET 2007 note: Because TRect/TSize/TPoint are wrongly declared as
    'packed', fields of these types must be preceded by an Integer- or
    IntPtr-sized field to ensure correct alignment and avoid an alignment
    fault on IA-64. }

{$IFNDEF CLR}
var
  UpdateLayeredWindowProc: function(Handle: HWND; hdcDest: HDC;
    pptDst: PPoint; _psize: PSize; hdcSrc: HDC; pptSrc: PPoint;
    crKey: COLORREF; var pblend: TBLENDFUNCTION; dwFlags: DWORD): BOOL; stdcall;
{$ELSE}
{ We can't use Borland.Vcl.Windows' UpdateLayeredWindow because the "pblend"
  parameter is misdeclared (see QC #25130) }
[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'UpdateLayeredWindow')]
function UpdateLayeredWindowProc(Handle: HWND; hdcDest: HDC; const pptDst: TPoint;
  const _psize: TSize; hdcSrc: HDC; const pptSrc: TPoint;
  crKey: COLORREF; [in] var pblend: TBLENDFUNCTION; dwFlags: DWORD): BOOL; overload; external;
[SuppressUnmanagedCodeSecurity, DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'UpdateLayeredWindow')]
function UpdateLayeredWindowProc(Handle: HWND; hdcDest: HDC; pptDst: IntPtr;
  _psize: IntPtr; hdcSrc: HDC; pptSrc: IntPtr;
  crKey: COLORREF; [in] var pblend: TBLENDFUNCTION; dwFlags: DWORD): BOOL; overload; external;
{$ENDIF}

threadvar
  AnimateData: TAnimateThreadFuncData;

procedure FinalizeAnimation;
begin
  {$IFNDEF CLR}
  with PAnimateThreadFuncData(@AnimateData)^ do begin
  {$ELSE}
  with AnimateData do begin
  {$ENDIF}
    FRunning := False;
    if FBmpDC <> 0 then begin
      if FBlending then
        SetWindowLong(FWnd, GWL_EXSTYLE,
          GetWindowLong(FWnd, GWL_EXSTYLE) and not WS_EX_LAYERED)
      else
        SetWindowRgn(FWnd, 0, False);
      BitBlt(FWndDC, 0, 0, FSize.cx, FSize.cy, FBmpDC, 0, 0, SRCCOPY);
      DeleteDC(FBmpDC);
      FBmpDC := 0;
    end;
    if FBmp <> 0 then begin
      DeleteObject(FBmp);
      FBmp := 0;
    end;
    if FWndDC <> 0 then begin
      ReleaseDC(FWnd, FWndDC);
      FWndDC := 0;
    end;
    if FWnd <> 0 then begin
      SendNotifyMessage(FWnd, WM_TB2K_ANIMATIONENDED, 0, 0);
      FWnd := 0;
    end;
  end;
end;

function TBIsAnimationInProgress: Boolean;
begin
  Result := AnimateData.FRunning;
end;

procedure TBEndAnimation(const Wnd: HWND);
begin
  if AnimateData.FRunning and
     ((Wnd = 0) or (AnimateData.FWnd = Wnd)) then
    FinalizeAnimation;
end;

procedure TBStartAnimation(const AWnd: HWND; const ABlend: Boolean;
  const ADirection: TTBAnimationDirection);
var
  ZeroPt: TPoint;
  R: TRect;
  Blend: TBlendFunction;
  Rgn: HRGN;
begin
  FinalizeAnimation;

  ZeroPt.X := 0;
  ZeroPt.Y := 0;

  try
    {$IFNDEF CLR}
    { Note: The pointer cast avoids GetTls calls for every field access }
    with PAnimateThreadFuncData(@AnimateData)^ do begin
    {$ELSE}
    with AnimateData do begin
    {$ENDIF}
      FWnd := AWnd;
      FBlending := ABlend and {$IFNDEF CLR} Assigned(UpdateLayeredWindowProc)
        {$ELSE} (Win32MajorVersion >= 5) {$ENDIF};
      FDirection := ADirection;
      GetCursorPos(FLastPos);
      GetClientRect(FWnd, FScreenClientRect);
      MapWindowPoints(FWnd, 0, FScreenClientRect, 2);
      GetWindowRect(FWnd, R);
      FWndDC := GetDCEx(FWnd, 0, DCX_WINDOW or DCX_CACHE {or DCX_USESTYLE ?});
      if FWndDC = 0 then
        RaiseLastOSError;
      FSize.cx := R.Right - R.Left;
      FSize.cy := R.Bottom - R.Top;
      FBmp := CreateCompatibleBitmap(FWndDC, FSize.cx, FSize.cy {or $01000000 ?});
      if FBmp = 0 then
        RaiseLastOSError;
      FBmpDC := CreateCompatibleDC(FWndDC);
      if FBmpDC = 0 then
        RaiseLastOSError;
      // AnimateWindow calls SetLayout, but I'm not sure that we need to.
      //if Assigned(SetLayoutProc) then
      //  SetLayoutProc(FBmpDC, 0);
      SelectObject(FBmpDC, FBmp);
      //SetBoundsRect(FBmpDC, nil, DCB_RESET or DCB_ENABLE);
      SendMessage(FWnd, WM_PRINT, WPARAM(FBmpDC), PRF_NONCLIENT or PRF_CLIENT or
        PRF_ERASEBKGND or PRF_CHILDREN);
      //GetBoundsRect
      if FBlending then begin
        SetWindowLong(FWnd, GWL_EXSTYLE, GetWindowLong(FWnd, GWL_EXSTYLE) or WS_EX_LAYERED);
        FTime := 175;  { actually more like ~147 because FCurStep starts at 40 }
        FCurStep := 40;
        Blend.BlendOp := AC_SRC_OVER;
        Blend.BlendFlags := 0;
        Blend.SourceConstantAlpha := FCurStep;
        Blend.AlphaFormat := 0;
        Win32Check(UpdateLayeredWindowProc(FWnd, 0, {$IFNDEF CLR}@{$ENDIF} R.TopLeft,
          {$IFNDEF CLR}@{$ENDIF} FSize, FBmpDC, {$IFNDEF CLR}@{$ENDIF} ZeroPt,
          0, Blend, ULW_ALPHA));
      end
      else begin
        FTime := 150;
        FCurStep := 0;
        Rgn := CreateRectRgn(0, 0, 0, 0);
        if not BOOL(SetWindowRgn(FWnd, Rgn, False)) then
          DeleteObject(Rgn);  { just in case }
      end;
      FStartStep := FCurStep;
      FStartTime := GetTickCount;
      FLastFrameTime := FStartTime;
      { These are the same flags AnimateWindow uses. SWP_ASYNCWINDOWPOS is
        needed or else it doesn't "save bits" properly.
        Note: SWP_ASYNCWINDOWPOS seems to have no effect on Windows 95 & NT 4.0,
        so bits behind the window are not saved & restored correctly. }
      SetWindowPos(FWnd, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or
        SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOREDRAW or
        SWP_NOOWNERZORDER or SWP_ASYNCWINDOWPOS);
      FRunning := True;
    end;
  except
    FinalizeAnimation;
    raise;
  end;
end;

procedure TBUpdateAnimation;
var
  ThisFrameTime: DWORD;
  ElapsedTime, NewStep: Integer;
  P: TPoint;
  Blend: TBlendFunction;
  X, Y: Integer;
  Rgn: HRGN;
begin
  {$IFNDEF CLR}
  with PAnimateThreadFuncData(@AnimateData)^ do begin
  {$ELSE}
  with AnimateData do begin
  {$ENDIF}
    if not FRunning then
      Exit;

    { If 10 msec hasn't passed since the last call, exit. We don't want to
      monopolize the CPU. }
    ThisFrameTime := GetTickCount;
    if ThisFrameTime - FLastFrameTime < 10 then
      Exit;
    FLastFrameTime := ThisFrameTime;

    ElapsedTime := ThisFrameTime - FStartTime;
    if (ElapsedTime < 0) or (ElapsedTime >= FTime) then begin
      FinalizeAnimation;
      Exit;
    end;
    NewStep := FStartStep + ((255 * ElapsedTime) div FTime);
    if (NewStep < 0) or (NewStep >= 255) then begin
      FinalizeAnimation;
      Exit;
    end;

    GetCursorPos(P);
    if (P.X <> FLastPos.X) or (P.Y <> FLastPos.Y) then begin
      if PtInRect(FScreenClientRect, P) then begin
        FinalizeAnimation;
        Exit;
      end;
      FLastPos := P;
    end;

    if NewStep > FCurStep then begin
      FCurStep := NewStep;
      if FBlending then begin
        Blend.BlendOp := AC_SRC_OVER;
        Blend.BlendFlags := 0;
        Blend.SourceConstantAlpha := NewStep;
        Blend.AlphaFormat := 0;
        UpdateLayeredWindowProc(FWnd, 0, nil, nil, 0, nil, 0, Blend, ULW_ALPHA);
      end
      else begin
        if tbadDown in FDirection then
          Y := MulDiv(FSize.cy, NewStep, 255) - FSize.cy
        else if tbadUp in FDirection then
          Y := FSize.cy - MulDiv(FSize.cy, NewStep, 255)
        else
          Y := 0;
        if tbadRight in FDirection then
          X := MulDiv(FSize.cx, NewStep, 255) - FSize.cx
        else if tbadLeft in FDirection then
          X := FSize.cx - MulDiv(FSize.cx, NewStep, 255)
        else
          X := 0;
        Rgn := CreateRectRgn(X, Y, X + FSize.cx, Y + FSize.cy);
        if not BOOL(SetWindowRgn(FWnd, Rgn, False)) then
          DeleteObject(Rgn);  { just in case }
        BitBlt(FWndDC, X, Y, FSize.cx, FSize.cy, FBmpDC, 0, 0, SRCCOPY);
      end;
    end;
  end;
end;

initialization
  {$IFNDEF CLR}
  UpdateLayeredWindowProc := GetProcAddress(GetModuleHandle(user32),
    'UpdateLayeredWindow');
  {$ENDIF}
finalization
  FinalizeAnimation;
end.
