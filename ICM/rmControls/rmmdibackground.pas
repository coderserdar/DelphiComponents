{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmMDIBackground
Purpose  : To allow an image to be placed with in the workspace area of an
           MDI Form.  Background colors are also available.
Date     : 04-24-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmMDIBackground;

interface

{$I CompilerDefines.INC}

uses
   Windows, Messages, Classes, Forms, graphics;

type
   TrmBMPDisplayStyle = (dsTiled, dsStretched, dsCentered, dsNone) ;

   TrmMDIBackground = class(TComponent)
   private
      OldWndProc: TFarProc;
      NewWndProc: Pointer;

      OldMDIWndProc: TFarProc;
      NewMDIWndProc: Pointer;

      fBitmap: TBitmap;
      fstyle: TrmBMPDisplayStyle;
      fColor: TColor;

      fBuffer: TBitmap;
      fLastRect: TRect;

      procedure SetBitmap(const Value: tBitmap) ;
      procedure SetDStyle(const Value: TrmBMPDisplayStyle) ;
      procedure SetMDIColor(const Value: TColor) ;

    { Private declarations }
   protected
    { Protected declarations }
      procedure HookWndProc(var AMsg: TMessage) ;
      procedure HookWnd;
      procedure UnHookWnd;

      procedure HookMDIWndProc(var AMsg: TMessage) ;
      procedure HookMDIWin;
      procedure UnhookMDIWin;

      procedure PaintImage;
   public
    { Public declarations }
      constructor create(AOwner: TComponent) ; override;
      destructor destroy; override;
   published
    { Published declarations }
      property Bitmap: tBitmap read fBitmap write SetBitmap;
      property DisplayStyle: TrmBMPDisplayStyle read fstyle write SetDStyle default dsNone;
      property Color: TColor read fColor write SetMDIColor default clappWorkspace;
   end;

implementation

uses rmGlobalComponentHook;

{ TrmMDIBackground }

constructor TrmMDIBackground.create(AOwner: TComponent) ;
begin
   inherited;

   NewWndProc := nil;
   OldWndProc := nil;

   OldMDIWndProc := nil;
   NewMDIWndProc := nil;

   fBitmap := tBitmap.create;
   fbuffer := tbitmap.create;

   fColor := clAppWorkSpace;
   fstyle := dsNone;

   fLastRect := rect(0, 0, 0, 0) ;

   HookWnd;
end;

destructor TrmMDIBackground.destroy;
begin
   UnHookWnd;

   fBitmap.free;
   fbuffer.free;

   inherited;
end;

procedure TrmMDIBackground.HookMDIWin;
begin
   if csdesigning in componentstate then exit;
   if not assigned(NewMDIWndProc) then
   begin
      OldMDIWndProc := TFarProc(GetWindowLong(TForm(Owner) .ClientHandle, GWL_WNDPROC) ) ;
      {$ifdef D6_or_higher}
      NewMDIWndProc := Classes.MakeObjectInstance(HookMDIWndProc) ;
      {$else}
      NewMDIWndProc := MakeObjectInstance(HookMDIWndProc) ;
      {$endif}
      SetWindowLong(TForm(Owner) .ClientHandle, GWL_WNDPROC, LongInt(NewMDIWndProc) ) ;
   end;
end;

procedure TrmMDIBackground.HookMDIWndProc(var AMsg: TMessage) ;
begin
   with AMsg do
   begin
      Result := CallWindowProc(OldMDIWndProc, TForm(Owner) .ClientHandle, Msg, wParam, lParam) ;
      if (msg = WM_NCPaint) or (msg = wm_Paint) then
         PaintImage;
   end;
end;

procedure TrmMDIBackground.HookWnd;
begin
   if csdesigning in componentstate then exit;
   if TForm(Owner) .formstyle <> fsMDIForm then exit;
   if not assigned(NewWndProc) then
   begin
      OldWndProc := TFarProc(GetWindowLong(TForm(Owner) .handle, GWL_WNDPROC) ) ;
      {$ifdef D6_or_higher}
      NewWndProc := Classes.MakeObjectInstance(HookWndProc) ;
      {$else}
      NewWndProc := MakeObjectInstance(HookWndProc) ;
      {$endif}
      SetWindowLong(TForm(Owner) .handle, GWL_WNDPROC, LongInt(NewWndProc) ) ;
      PushOldProc(TForm(Owner) , OldWndProc) ;
      HookMDIWin;
   end;
end;

procedure TrmMDIBackground.HookWndProc(var AMsg: TMessage) ;
begin
   case AMsg.msg of
      WM_DESTROY:
         begin
            AMsg.Result := CallWindowProc(OldWndProc, Tform(Owner) .handle, AMsg.Msg, AMsg.wParam, AMsg.lParam) ;
            UnHookWnd;
            exit;
         end;
      wm_EraseBKGND:
         begin
            aMsg.Result := 1;
            exit;
         end;
   end;

   AMsg.Result := CallWindowProc(OldWndProc, Tform(Owner) .handle, AMsg.Msg, AMsg.wParam, AMsg.lParam) ;

   case aMsg.Msg of
      WM_PAINT, // WM_ERASEBKGND,
         WM_NCPaint: PaintImage;
   end;
end;

procedure TrmMDIBackground.PaintImage;
var
   DC: HDC;
   Brush: HBrush;
   cx, cy: integer;
   wRect: TRect;
   x, y: integer;
begin
   if csdesigning in componentstate then exit;
   if TForm(Owner) .FormStyle <> fsMDIForm then exit;

   GetWindowRect(TForm(Owner) .ClientHandle, wRect) ;

   DC := GetDC(TForm(Owner) .clienthandle) ;
   try
      case fstyle of
         dsTiled, dsStretched, dsCentered:
            begin
               case fStyle of
                  dsTiled:
                     begin
                        cx := (wRect.right - wRect.left) ;
                        cy := (wRect.bottom - wRect.top) ;

                        y := 0;
                        while y < cy do
                        begin
                           x := 0;
                           while x < cx do
                           begin
                              bitBlt(DC, x, y, fBitmap.width, fBitmap.height,
                                 fBitmap.canvas.Handle, 0, 0, srccopy) ;

                              inc(x, fBitmap.width) ;
                           end;
                           inc(y, fBitmap.Height) ;
                        end;
                     end;

                  dsStretched:
                     begin
                        cx := (wRect.right - wRect.left) ;
                        cy := (wRect.bottom - wRect.top) ;

                        StretchBlt(DC, 0, 0, cx, cy, fBitmap.Canvas.Handle, 0, 0, fBitmap.width, fBitmap.height, srccopy) ;
                     end;

                  dsCentered:
                     begin
                        fBuffer.width := wRect.right - wRect.left;
                        fBuffer.height := wRect.bottom - wRect.top;

                        Brush := CreateSolidBrush(ColorToRGB(fcolor) ) ;
                        try
                           FillRect(fBuffer.canvas.handle, rect(0, 0, fBuffer.width, fBuffer.height) , brush) ;
                        finally
                           DeleteObject(Brush) ;
                        end;

                        cx := (fBuffer.width div 2) - (fBitmap.width div 2) ;
                        cy := (fBuffer.height div 2) - (fbitmap.height div 2) ;

                        bitBlt(fBuffer.Canvas.handle, cx, cy, fBitmap.width, fBitmap.height,
                           fBitmap.Canvas.Handle, 0, 0, srccopy) ;

                        bitBlt(DC, 0, 0, fBuffer.width, fBuffer.height,
                           fBuffer.Canvas.Handle, 0, 0, srccopy) ;
                     end;
               end;
            end;
         dsNone:
            begin
               Brush := CreateSolidBrush(ColorToRGB(fcolor) ) ;
               try
                  FillRect(DC, TForm(Owner) .ClientRect, brush) ;
               finally
                  DeleteObject(Brush) ;
               end;
            end;
      end;

      fLastRect := wRect;

   finally
      ReleaseDC(TForm(Owner) .clienthandle, DC) ;
   end;
end;

procedure TrmMDIBackground.SetBitmap(const Value: tBitmap) ;
begin
   fBitmap.assign(Value) ;
end;

procedure TrmMDIBackground.SetDStyle(const Value: TrmBMPDisplayStyle) ;
begin
   if fstyle <> Value then
   begin
      fstyle := Value;
      PaintImage;
   end;
end;

procedure TrmMDIBackground.SetMDIColor(const Value: TColor) ;
begin
   if fColor <> Value then
   begin
      fColor := Value;
      PaintImage;
   end;
end;

procedure TrmMDIBackground.UnhookMDIWin;
begin
   if csdesigning in componentstate then exit;
   if assigned(NewMDIWndProc) then
   begin
      SetWindowLong(TForm(Owner) .ClientHandle, GWL_WNDPROC, LongInt(OldMDIWndProc) ) ;
      if assigned(NewMDIWndProc) then
      {$ifdef D6_or_higher}
         Classes.FreeObjectInstance(NewMDIWndProc) ;
      {$else}
         FreeObjectInstance(NewMDIWndProc) ;
      {$endif}
      NewMDIWndProc := nil;
      OldMDIWndProc := nil;
   end;
end;

procedure TrmMDIBackground.UnHookWnd;
begin
   if csdesigning in componentstate then exit;
   if assigned(NewWndProc) then
   begin
      SetWindowLong(TForm(Owner) .handle, GWL_WNDPROC, LongInt(PopOldProc(TForm(Owner) ) ) ) ;
      if assigned(NewWndProc) then
      {$ifdef D6_or_higher}
         Classes.FreeObjectInstance(NewWndProc) ;
      {$else}
         FreeObjectInstance(NewWndProc) ;
      {$endif}
      NewWndProc := nil;
      OldWndProc := nil;
   end;
   UnHookMDIWin;
end;

end.

