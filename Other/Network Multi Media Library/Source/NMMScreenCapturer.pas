(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMScreenCapturer;

interface

uses NMMCustomImageSource, NMMScrnCap, SyncObjs, SysUtils,
     Windows, Graphics;

const ScreenHandle= 0;

type
 TNMMScreenCapturer= class(TNMMCustomImageSource)
 protected
  FHandle: HWND;
  FCS: TCriticalSection;
  procedure SetHandle(AHandle: HWND);
 public
  constructor Create; overload;
  constructor Create(ACaptureRect: TRect; AHandle: HWND= ScreenHandle); overload;
  destructor Destroy; override;
  procedure GetImage(ABitmap: TBitmap); override;
  procedure SetCaptureRect(ALeft, ATop, ARight, ABottom: Integer);
  property Handle: HWND read FHandle write SetHandle;
 end;


implementation

uses Forms;

constructor TNMMScreenCapturer.Create;
begin
 inherited Create;
 FCS:= TCriticalSection.Create;
 SetCaptureRect( 0, 0, Screen.Width, Screen.Height);
end;

constructor TNMMScreenCapturer.Create(ACaptureRect: TRect; AHandle: HWND= ScreenHandle);
begin
 Create;
 FHandle:= AHandle;
 FCaptureRect:= ACaptureRect;
end;

destructor TNMMScreenCapturer.Destroy;
begin
 FreeAndNil(FCS);
 inherited;
end;

procedure TNMMScreenCapturer.GetImage(ABitmap: TBitmap);
var LTmpBitmap: TBitmap;
begin
 FCS.Enter;
 try
   LTmpBitmap:= CaptureWindowRect(FHandle,FCaptureRect);
   try
     ABitmap.Assign(LTmpBitmap);
   finally
     FreeAndNil(LTmpBitmap);
   end;
 finally
   FCS.Leave;
 end;
end;

procedure TNMMScreenCapturer.SetCaptureRect(ALeft, ATop, ARight, ABottom: Integer);
begin
 FCS.Enter;
 try
   FCaptureRect.Left:= ALeft;
   FCaptureRect.Top := ATop;
   FCaptureRect.Right:= ARight;
   FCaptureRect.Bottom:= ABottom;
 finally
   FCS.Leave;
 end;
end;

procedure TNMMScreenCapturer.SetHandle(AHandle: HWND);
begin
 if AHandle<>FHandle then
 begin
   FCS.Enter;
   try
     FHandle:= AHandle;
   finally
     FCS.Leave;
   end;
 end;
end;

end.
