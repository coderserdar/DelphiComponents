{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmToolWin
Purpose  : This is a alternate form for all forms that use the bsSizableToolWin
           borderstyle.  This window does not suffer from the M$ ALT-Tab bug. 
Date     : 04-29-2001
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmToolWin;

interface

{$I CompilerDefines.INC}

uses windows, messages, classes, forms, Graphics;

type
   TMoveSize = (msEntered, msMoving, msSizing, msExited) ;

   TWMSizing = packed record
      Msg: Cardinal;
      SizingSide: Longint;
      WindowRect: PRect;
      Result: Longint;
   end;
   TWMMoving = TWMSizing;

   TrmCustomToolWinForm = class(TCustomForm)
   private
    { Private }
      fInNCPaint: boolean;
      fActive: boolean;
      fMoveSize: TMoveSize;
      fFrameRect, fLastFrameRect, FPosRect: TRect;
      fCloseBtnDown, fCloseBtnPressed: boolean;
      fOnMove: TNotifyEvent;
      fWindowBMP: TBitmap;
      fStandardMoving: boolean;

      function AdjustFormFrameRect(wRect: TRect) : TRect;
      procedure wmEnterSizeMove(var msg: TMessage) ; message WM_ENTERSIZEMOVE;
      procedure wmExitSizeMove(var msg: TMessage) ; message WM_EXITSIZEMOVE;
      procedure wmMoving(var msg: TWMMoving) ; message WM_MOVING;
      procedure wmSizing(var msg: TWMSizing) ; message WM_SIZING;
      procedure wmMove(Var msg: TWMMove) ; message wm_move;
      procedure wmWindowPosChanging(var msg: TWMWindowPosChanging) ; message WM_WINDOWPOSCHANGING;
      procedure WMNCActivate(var Message: TWMNCActivate) ; message WM_NCActivate;
      procedure WMNCCalcSize(var Message: TWMNCCalcSize) ; message WM_NCCALCSIZE;
      procedure WMNCHitTest(var Message: TWMNCHitTest) ; message WM_NCHITTEST;
      procedure WMNCPaint(var Message: TMessage) ; message WM_NCPAINT;
      procedure WMNCLButtonDown(var Message: TWMNCLButtonDown) ; message WM_NCLBUTTONDOWN;
      procedure WMNCLButtonUp(var Message: TWMNCLButtonUp) ; message WM_NCLBUTTONUP;
      procedure WMNCMouseMove(var Message: TWMNCMouseMove) ; message WM_NCMOUSEMOVE;
      procedure WMLButtonUp(var Message: TWMLButtonUp) ; message WM_LBUTTONUP;
      procedure WMMouseMove(var Message: TWMMouseMove) ; message WM_MOUSEMOVE;
      procedure WMKillFocus(var msg: TWMKillFocus) ; message WM_KillFocus;
      procedure SetInternalFrameRect(const Value: TRect) ;
      procedure setncactive(const Value: boolean);
   protected
    { Protected }
      function FormCaptionRect(Screen: boolean) : TRect;
      function FormCaptionTextRect(Screen: boolean) : TRect;
      function FormBtnRect(Screen: boolean) : TRect;
      function FormFrameRect(Screen: boolean) : TRect;
      function FormClientRect(screen: boolean) : TRect;
      property InternalFrameRect: TRect read fFrameRect write SetInternalFrameRect;
      property OnMove: TNotifyEvent read fonMove write fOnMove;
      property StandardMoving: boolean read fStandardMoving write fStandardMoving default true;
      property MoveSize : TMoveSize read fMoveSize;
      property NCActive : boolean read factive write setncactive;
   public
    { Public }
      constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0) ; override;
      destructor destroy; override;
   end;

   TrmToolWinForm = class(TrmCustomToolWinForm)
   published
    { Published }
      property Action;
      property ActiveControl;
      property Align;
      property BiDiMode;
      property BorderWidth;
      property Caption;
      property ClientHeight;
      property ClientWidth;
      property Color;
      property Ctl3D;
      property DefaultMonitor;
      property Enabled;
      property ParentFont default False;
      property Font;
      property Height;
      property HelpFile;
      property KeyPreview;
      property Menu;
      property OldCreateOrder;
      property ParentBiDiMode;
      property PixelsPerInch;
      property PopupMenu;
      property Position;
      property PrintScale;
      property Scaled;
      property ShowHint;
      property Visible;
      property Width;
      property OnActivate;
      property OnCanResize;
      property OnClick;
      property OnClose;
      property OnCloseQuery;
      property OnContextPopup;
      property OnCreate;
      property OnDblClick;
      property OnDestroy;
      property OnDeactivate;
      property OnDragDrop;
      property OnDragOver;
      property OnHide;
      property OnHelp;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnMouseWheel;
      property OnMouseWheelDown;
      property OnMouseWheelUp;
      property OnMove;
      property OnPaint;
      property OnResize;
      property OnShortCut;
      property OnShow;
   end;

function WindowCaptionHeight: integer;
function WindowButtonHeight: integer;
function WindowButtonWidth: integer;
function WindowBorderWidth: integer;
function WindowBorderHeight: integer;
function WindowUseGradientCaption: Boolean;
function WindowCaptionFontName: string;
function WindowCaptionFontSize: integer;
function WindowCaptionFontStyle: TFontStyles;


implementation

uses rmLibrary, ExtCtrls;

const
   PenSize = 3;

var
   NewBrush: TBrush;

function WindowCaptionHeight: integer;
begin
   result := GetSystemMetrics(SM_CYSMCAPTION) ; //Small Caption Height
end;

function WindowButtonHeight: integer;
begin
   result := WindowCaptionHeight - 5;
end;

function WindowButtonWidth: integer;
begin
   result := WindowButtonHeight + 2;
end;

function WindowBorderWidth: integer;
begin
   result := GetSystemMetrics(SM_CXSIZEFRAME); //Sizeable Frame Width
end;

function WindowBorderHeight: integer;
begin
   result := GetSystemMetrics(SM_CYSIZEFRAME); //Sizeable Frame Height
end;

function WindowUseGradientCaption: Boolean;
begin
   SystemParametersInfo(SPI_GETGRADIENTCAPTIONS, 0, @Result, 0) ;
end;

function WindowCaptionFontName: string;
var
   wMetrics: TNONCLIENTMETRICS;
begin
   wMetrics.cbSize := sizeof(TNONCLIENTMETRICS) ;
   SystemParametersInfo(SPI_GETNONCLIENTMETRICS, sizeof(TNONCLIENTMETRICS) , @wMetrics, 0) ;
   result := wMetrics.lfSmCaptionFont.lfFaceName;
end;

function WindowCaptionFontSize: integer;
var
   wMetrics: TNONCLIENTMETRICS;
begin
   wMetrics.cbSize := sizeof(TNONCLIENTMETRICS) ;
   SystemParametersInfo(SPI_GETNONCLIENTMETRICS, sizeof(TNONCLIENTMETRICS) , @wMetrics, 0) ;
   result := wMetrics.lfSmCaptionFont.lfHeight;
end;

function WindowCaptionFontStyle: TFontStyles;
var
   wMetrics: TNONCLIENTMETRICS;
begin
   wMetrics.cbSize := sizeof(TNONCLIENTMETRICS) ;
   SystemParametersInfo(SPI_GETNONCLIENTMETRICS, sizeof(TNONCLIENTMETRICS) , @wMetrics, 0) ;

   result := [];

   if wMetrics.lfSmCaptionFont.lfWeight = fw_bold then
      result := result + [fsbold];

   if wMetrics.lfSmCaptionFont.lfItalic > 0 then
      result := result + [fsItalic];

   if wMetrics.lfSmCaptionFont.lfUnderline > 0 then
      result := result + [fsUnderLine];

   if wMetrics.lfSmCaptionFont.lfStrikeOut > 0 then
      result := result + [fsStrikeOut];
end;

procedure DrawFrameRect(FrameRect: TRect) ;
var
   DC: hDC; { device context for the window       }
   DesktopWindow: THandle;
   OldHBrush: HBrush;
begin
   DesktopWindow := GetDesktopWindow;
   DC := GetDCEx(DesktopWindow, 0, DCX_CACHE or DCX_LOCKWINDOWUPDATE) ;
   try
      if NewBrush = nil then
      begin
         NewBrush := TBrush.Create;
         NewBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite) ;
      end;
      OldHBrush := SelectObject(DC, NewBrush.Handle) ;

      with FrameRect do
      begin
         PatBlt(DC, Left + PenSize, Top, Right - Left - PenSize, PenSize, PATINVERT) ;
         PatBlt(DC, Right - PenSize, Top + PenSize, PenSize, Bottom - Top - PenSize, PATINVERT) ;
         PatBlt(DC, Left, Bottom - PenSize, Right - Left - PenSize, PenSize, PATINVERT) ;
         PatBlt(DC, Left, Top, PenSize, Bottom - Top - PenSize, PATINVERT) ;
      end;

      SelectObject(DC, OldHBrush) ;
   finally
      ReleaseDC(DesktopWindow, DC) ;
   end;
end;

{ TrmToolWinForm }

constructor TrmCustomToolWinForm.CreateNew(AOwner: TComponent; Dummy: Integer) ;
begin
   inherited CreateNew(AOwner, Dummy) ;
   if csDesigning in componentstate then exit;
   fStandardMoving := true;
   fWindowBMP := tbitmap.create;
   AutoScroll := false;
   VertScrollBar.Visible := false;
   HorzScrollBar.Visible := false;
   fActive := false;
   fInNCPaint := false;
   KeyPreview := true;
   BorderStyle := bsNone;
   fCloseBtnDown := false;
   fCloseBtnPressed := false;
end;

procedure TrmCustomToolWinForm.wmEnterSizeMove(var msg: tmessage) ;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   inherited;
   FPosRect := BoundsRect;
   fMoveSize := msEntered;
end;

procedure TrmCustomToolWinForm.wmExitSizeMove(var msg: tmessage) ;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   if (fMoveSize = msMoving) then
   begin
      fMoveSize := msExited;
      if not (fStandardMoving) then
      begin
         DrawFrameRect(fLastFrameRect) ;
         SetBounds(fLastFrameRect.left, fLastFrameRect.top, width, height) ;
         msg.Result := integer(true) ;
         fLastFrameRect := Rect(0, 0, 0, 0) ;
      end
      else
         inherited;
   end
   else
   begin
      fMoveSize := msExited;
      inherited;
   end;

   Invalidate;
end;

procedure TrmCustomToolWinForm.wmMoving(var msg: TWMMoving) ;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   inherited;

   if fMoveSize = msEntered then
      fMoveSize := msMoving;

   if (fMoveSize = msMoving) then
   begin
      if not (fStandardMoving) then
      begin
         if not IsRectEmpty(fLastFrameRect) then
            DrawFrameRect(fLastFrameRect) ;

         fFrameRect := msg.WindowRect^;

         try
            DrawFrameRect(fFrameRect) ;
         finally
            fLastFrameRect := fFrameRect;
         end;
      end
      else
         fLastFrameRect := rect(0, 0, 0, 0) ;
   end;
end;

procedure TrmCustomToolWinForm.wmSizing(var msg: TWMSizing) ;
var
   xofs, yofs: integer;
   wRect: TRect;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   inherited;

   if fMoveSize = msEntered then
      fMoveSize := msSizing;

   if (fMoveSize = msSizing) then
   begin
      wRect := msg.WindowRect^;

      if not (((wRect.left <> 0) and (wRect.top <> 0) ) and
         (((wrect.top <> top) and (wRect.bottom = height) ) or
         ((wrect.Left <> Left) and (wRect.right = width) ) ) ) then
      begin
         xofs := wRect.Left;
         yofs := wRect.Top;
         offsetrect(wRect, -xofs, -yofs) ;
         try
            wRect := AdjustFormFrameRect(wRect) ;
         finally
            offsetrect(wRect, xofs, yofs) ;
         end;
      end
      else
      begin
         wRect := rect(left, top, width, height) ;
      end;
      InternalFrameRect := wRect;
   end;
end;

procedure TrmCustomToolWinForm.wmWindowPosChanging(var msg: TWMWindowPosChanging) ;
var
   wRect: trect;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   if fMoveSize = msMoving then
   begin
      if fStandardMoving then
         inherited
      else
      begin
         msg.WindowPos.x := left;
         msg.WindowPos.y := top;
         Msg.Result := 0
      end;
   end
   else if (fMoveSize = msSizing) then
   begin
      inherited;

      wrect := AdjustFormFrameRect(rect(msg.windowpos.x, msg.windowpos.y, msg.windowpos.cx, msg.windowpos.cy) ) ;

      msg.windowpos.x := wrect.left;
      msg.windowpos.y := wrect.top;
      msg.windowpos.cx := wrect.right;
      msg.windowpos.cy := wrect.bottom;
   end;
end;

procedure TrmCustomToolWinForm.WMNCCalcSize(var Message: TWMNCCalcSize) ;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

  //Adjust the size of the clientwidth rect for the drawing of the
  //Borders
   inherited;

   with Message.CalcSize_Params^ do
   begin
      InflateRect(rgrc[0], -WindowBorderWidth, -WindowBorderHeight);
      rgrc[0].top := rgrc[0].top + WindowCaptionHeight;
   end;
end;

procedure TrmCustomToolWinForm.WMNCHitTest(var Message: TWMNCHitTest) ;
var
   wpt: TPoint;
   wRect: TRect;

   BorderWidth, BorderHeight: integer;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   inherited;

  //Figure out where the hell the mouse is in relation to
  //what's on the window....

   BorderWidth := WindowBorderWidth;
   BorderHeight := WindowBorderHeight;

   wpt := Point(Message.XPos, Message.YPos) ;
   wRect := FormFrameRect(true) ;

   if (PtInRect(Rect(wRect.left, wRect.top, wRect.Left + 10 + borderwidth, wRect.top + borderheight) , wpt) or
      PtInRect(Rect(wRect.Left, wRect.top, wRect.Left + BorderWidth, wRect.top + 10 + borderheight) , wpt) ) then //TopLeft
   begin
      Message.Result := htTopLeft;
   end
   else if (PtInRect(Rect(wRect.right - (10 + borderwidth) , wRect.bottom - borderheight, wRect.right, wRect.bottom) , wpt) or
      PtInRect(Rect(wRect.right - BorderWidth, wRect.bottom - (10 + borderheight) , wRect.right, wRect.bottom) , wpt) ) then //BottomRight
   begin
      Message.Result := htBottomRight;
   end
   else if (PtInRect(Rect(wRect.right - (10 + borderwidth) , wRect.top, wRect.right, wRect.top + borderheight) , wpt) or
      PtInRect(Rect(wRect.right - BorderWidth, wRect.top, wRect.right, wRect.top + (10 + borderheight) ) , wpt) ) then //TopRight
   begin
      Message.Result := htTopRight;
   end
   else if (PtInRect(Rect(wRect.Left, wRect.bottom - (10 + borderheight) , wRect.left + BorderWidth, wRect.bottom) , wpt) or
      PtInRect(Rect(wRect.Left, wRect.bottom - borderheight, wRect.left + (10 + borderwidth) , wRect.bottom) , wpt) ) then //BottomRight
   begin
      Message.Result := htBottomLeft;
   end
   else if PtInRect(Rect(wRect.left + 10 + borderWidth, wRect.top, wRect.right - (10 + borderWidth) , wRect.top + borderheight) , wpt) then //Top
   begin
      Message.Result := htTop;
   end
   else if PtInRect(Rect(wRect.Left, wRect.top + 10 + borderheight, wRect.Left + BorderWidth, wRect.bottom - (10 + borderheight) ) , wpt) then //Left
   begin
      Message.Result := htLeft;
   end
   else if PtInRect(Rect(wRect.left + 10 + borderWidth, wRect.Bottom - borderheight, wRect.right - (10 + borderWidth) , wRect.Bottom) , wpt) then //bottom
   begin
      Message.Result := htBottom;
   end
   else if PtInRect(Rect(wRect.right - BorderWidth, wRect.top + 10 + borderheight, wRect.right, wRect.bottom - (10 + borderheight) ) , wpt) then //Right
   begin
      Message.Result := htRight;
   end
   else if PtInRect(FormBtnRect(true) , wpt) then //CloseButton
   begin
      Message.Result := htClose;
   end
   else if PtInRect(FormCaptionRect(true) , wpt) then //Caption
   begin
      Message.Result := htCaption;
   end
   else if PtInRect(FormClientRect(true) , wpt) then //Client
   begin
      Message.Result := htclient;
   end
   else
      Message.result := HTNOWHERE;
end;

procedure TrmCustomToolWinForm.WMNCPaint(var Message: TMessage) ;
var
   DC: HDC;
   wRect: TRect;
   Rgn1, Rgn2, Rgn3: HRGN;
   cLeft, cRight: TColor;
   wFrameRect, wCaptionRect, wBtnRect, wCaptionTextRect, wClientRect: TRect;

begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

  //This is where the magic of the whole thing comes into play....

   wFrameRect := FormFrameRect(false) ;
   wCaptionRect := FormCaptionRect(false) ;
   wBtnRect := FormBtnRect(false) ;
   wCaptionTextRect := FormCaptionTextRect(false) ;
   wClientRect := FormClientRect(false) ;

   fInNCPaint := true;
   try
      fWindowBMP.Width := wFrameRect.right - wFrameRect.left;
      fWindowBMP.height := wFrameRect.bottom - wFrameRect.Top;
      fWindowBMP.canvas.Brush.Color := Color;
      fWindowBMP.Canvas.FillRect(wFrameRect) ;

      if WinOSVersion in [wosWin98, wosWinNT2k] then
      begin
         if WindowUseGradientCaption then
         begin
            if fActive or Self.Focused then
            begin
               cLeft := clActiveCaption;
               cRight := clGradientActiveCaption;
               fWindowBMP.Canvas.font.Color := clCaptionText;
            end
            else
            begin
               cLeft := clInActiveCaption;
               cRight := clGradientInactiveCaption;
               fWindowBMP.Canvas.font.Color := clInactiveCaptionText;
            end;
            GradientFill(fWindowBMP.canvas, cLeft, cRight, wCaptionRect) ;
         end
         else
         begin
            if fActive or Self.Focused then
            begin
               fWindowBMP.Canvas.brush.color := clActiveCaption;
               fWindowBMP.Canvas.font.Color := clCaptionText;
            end
            else
            begin
               fWindowBMP.Canvas.brush.color := clInActiveCaption;
               fWindowBMP.Canvas.font.Color := clInactiveCaptionText;
            end;
            fWindowBMP.Canvas.fillrect(wCaptionRect) ;
         end;
      end
      else
      begin
         if fActive or Self.Focused then
         begin
            fWindowBMP.Canvas.brush.color := clActiveCaption;
            fWindowBMP.Canvas.font.Color := clCaptionText;
         end
         else
         begin
            fWindowBMP.Canvas.brush.color := clInActiveCaption;
            fWindowBMP.Canvas.font.Color := clInactiveCaptionText;
         end;
         fWindowBMP.Canvas.fillrect(wCaptionRect) ;
      end;
      fWindowBMP.Canvas.Pen.Color := clBtnFace;
      fWindowBMP.Canvas.MoveTo(wCaptionRect.Left, wCaptionRect.Bottom - 1) ;
      fWindowBMP.Canvas.LineTo(wCaptionRect.Right, wCaptionRect.Bottom - 1) ;

      fWindowBMP.Canvas.font.name := WindowCaptionFontName;
      fWindowBMP.Canvas.font.height := WindowCaptionFontSize;
      fWindowBMP.Canvas.Brush.Style := bsClear;
      fWindowBMP.Canvas.Font.Style := WindowCaptionFontStyle;

      wRect := wCaptionTextRect;
      DrawText(fWindowBMP.Canvas.handle, pchar(caption) , length(caption) , wRect, DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS) ;
      DrawFrameControl(fWindowBMP.canvas.handle, wBtnRect, DFC_Caption, DFCS_CAPTIONCLOSE) ;

      wRect := wFrameRect;
      if Parent = nil then
      begin
         Frame3D(fWindowBMP.Canvas, wRect, cl3DLight, cl3DDkShadow, 1) ;
         Frame3D(fWindowBMP.Canvas, wRect, clBtnHighlight, clBtnShadow, 1) ;
      end
      else
      begin
         Frame3D(fWindowBMP.Canvas, wRect, clBtnface, clBtnface, 2) ;
      end;
      Frame3D(fWindowBMP.Canvas, wRect, clBtnface, clBtnface, 2) ;

      Rgn1 := CreateRectRgn(wFrameRect.Left, wFrameRect.Top, wFrameRect.Right, wFrameRect.Bottom) ;

      GetWindowRgn(handle, Rgn1) ;

      Rgn2 := CreateRectRgn(wClientRect.Left, wClientRect.Top, wClientRect.Right, wClientRect.Bottom) ;
      Rgn3 := CreateRectRgn(0, 0, width, height) ;
      CombineRgn(Rgn3, Rgn1, Rgn2, Rgn_XOR) ;
      try
         if Rgn3 <> 0 then
            SetWindowRgn(handle, Rgn3, false) ;

         DC := GetWindowDC(Handle) ;
         try
            BitBlt(DC, 0, 0, fWindowBMP.width, fWindowBMP.height, fWindowBMP.Canvas.Handle, 0, 0, SRCCOPY) ;
         finally
            ReleaseDC(Handle, DC) ;
         end;

      finally
         SetWindowRgn(handle, 0, false) ;
         DeleteObject(Rgn1) ;
         DeleteObject(Rgn2) ;
         DeleteObject(Rgn3) ;
      end;
   finally
      fInNCPaint := false;
   end;
   Message.result := 0;
end;

function TrmCustomToolWinForm.FormFrameRect(Screen: boolean) : TRect;
begin
   if Screen then
      result := BoundsRect
   else
   begin
      if fMoveSize = msSizing then
      begin
         result := InternalFrameRect;
         offsetrect(result, -result.left, -result.Top) ;
      end
      else
         result := rect(0, 0, width, height) ;
   end;
end;

function TrmCustomToolWinForm.FormCaptionRect(screen: boolean) : TRect;
begin
   result := FormFrameRect(screen) ;
   InflateRect(result, -WindowBorderWidth, -WindowBorderHeight) ;
   Result.Bottom := Result.top + WindowCaptionHeight;
end;

function TrmCustomToolWinForm.FormCaptionTextRect(Screen: boolean) : TRect;
begin
   result := FormCaptionRect(screen) ;
   Result.left := Result.Left + 2;
   Result.right := Result.right - WindowButtonWidth - 2;
end;

function TrmCustomToolWinForm.FormBtnRect(screen: boolean) : TRect;
begin
   Result := FormCaptionRect(screen) ;
   Result.Right := Result.Right - 2;
   Result.Left := Result.Right - WindowButtonWidth;
   Result.top := Result.top + 2;
   Result.bottom := Result.top + WindowButtonHeight;
end;

function TrmCustomToolWinForm.FormClientRect(screen: boolean) : TRect;
var
   wRect: TRect;
begin
   if screen then
      wRect := rect(ClientOrigin.x, ClientOrigin.y, ClientOrigin.x + clientwidth, ClientOrigin.y + clientheight)
   else
   begin
      wRect := ClientRect;
      OffsetRect(wRect, WindowBorderWidth, WindowBorderheight + WindowCaptionHeight) ;
   end;
   result := wRect;
end;

procedure TrmCustomToolWinForm.WMNCLButtonDown(var Message: TWMNCLButtonDown) ;
var
   DC: HDC;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   DC := GetWindowDC(handle) ;
   try
      if Message.HitTest = htClose then
      begin
         SendCancelMode(Self) ;
         MouseCapture := true;
         DrawFrameControl(DC, FormBtnRect(false) , DFC_Caption, DFCS_CAPTIONCLOSE or DFCS_PUSHED) ;
         fCloseBtnPressed := true;
         Message.Result := 0;
      end
      else
         inherited;
   finally
      if DC <> 0 then
         ReleaseDC(handle, DC) ;
   end;
end;

procedure TrmCustomToolWinForm.WMNCLButtonUp(var Message: TWMNCLButtonUp) ;
var
   DC: HDC;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   DC := GetWindowDC(handle) ;
   try
      DrawFrameControl(DC, FormBtnRect(false) , DFC_Caption, DFCS_CAPTIONCLOSE) ;
      if fCloseBtnPressed and (Message.HitTest = htClose) then
      begin
         Message.Result := 0;
         close;
      end
      else
         inherited;
   finally
      fCloseBtnPressed := false;
      if DC <> 0 then
         ReleaseDC(handle, DC) ;
   end;
end;

procedure TrmCustomToolWinForm.WMNCMouseMove(var Message: TWMNCMouseMove) ;
var
   DC: HDC;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   try
      DC := GetWindowDC(handle) ;

      try
         if fCloseBtnPressed then
         begin
            if Message.HitTest = htClose then
               DrawFrameControl(DC, FormBtnRect(false) , DFC_Caption, DFCS_PUSHED or DFCS_CAPTIONCLOSE)
            else
               DrawFrameControl(DC, FormBtnRect(false) , DFC_Caption, DFCS_CAPTIONCLOSE) ;
            message.result := 0;
         end
         else
            inherited;
      finally
         if DC <> 0 then
            ReleaseDC(handle, DC) ;
      end;
   except
      //for some reason we occasionally get a Range Checking error here.
   end;

end;

procedure TrmCustomToolWinForm.WMLButtonUp(var Message: TWMLButtonUp) ;
var
   DC: HDC;
   pt: TPoint;
   WasBtnPressed: boolean;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   WasBtnPressed := fCloseBtnPressed;
   fCloseBtnPressed := false;
   MouseCapture := false;
   DC := GetWindowDC(handle) ;
   try
      DrawFrameControl(DC, FormBtnRect(false) , DFC_Caption, DFCS_CAPTIONCLOSE) ;
      pt := clienttoscreen(point(message.XPos, Message.YPos) ) ;
      if WasBtnPressed and ptInRect(FormBtnRect(true) , pt) then
      begin
         Message.Result := 0;
         close;
      end
      else
         inherited;
   finally
      if DC <> 0 then
         ReleaseDC(handle, DC) ;
   end;
end;

procedure TrmCustomToolWinForm.WMMouseMove(var Message: TWMMouseMove) ;
var
   DC: HDC;
   pt: TPoint;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   DC := GetWindowDC(handle) ;
   try
      if fCloseBtnPressed then
      begin
         pt := clienttoscreen(point(message.XPos, Message.YPos) ) ;
         if ptInRect(FormBtnRect(true) , pt) then
            DrawFrameControl(DC, FormBtnRect(false) , DFC_Caption, DFCS_PUSHED or DFCS_CAPTIONCLOSE)
         else
            DrawFrameControl(DC, FormBtnRect(false) , DFC_Caption, DFCS_CAPTIONCLOSE) ;
         message.result := 0;
      end
      else
         inherited;
   finally
      if DC <> 0 then
         ReleaseDC(handle, DC) ;
   end;
end;

procedure TrmCustomToolWinForm.WMNCActivate(var Message: TWMNCActivate) ;
begin
   inherited;
   //Your supposed to pass the handle of the region to paint according to the Win32 API
   //But because I'm handling the NCPainting myself, I figure that I can skip passing the
   //handle of the rgn.  Mostly because I'm not paying attention to it in the first place.
   fActive := Message.active;
   SendMessage(self.handle, wm_ncPaint, 0, 0) ;
end;

procedure TrmCustomToolWinForm.WMKillFocus(var msg: TWMKillFocus) ;
begin
   inherited;
   fActive := false;
   SendMessage(self.handle, wm_ncPaint, 0, 0) ;
end;

function TrmCustomToolWinForm.AdjustFormFrameRect(wRect: TRect) : TRect;
var
   fixed: boolean;
   wPosRect: TRect;
begin
   wPosRect := fPosRect;

   fixed := false;

   if wRect.right <= 40 + (WindowButtonWidth + (WindowBorderWidth * 2) + 6) then
   begin
      wRect.right := 40 + (WindowButtonWidth + (WindowBorderWidth * 2) + 6) ;
      fixed := true;
   end;

   if wRect.bottom <= (WindowCaptionHeight + (WindowBorderWidth * 2) ) then
   begin
      wRect.bottom := (WindowCaptionHeight + (WindowBorderWidth * 2) ) ;
      fixed := true;
   end;

   if fixed then
   begin
      if wRect.left > wPosRect.left then
         wRect.left := wPosRect.right - wRect.right;

      if wRect.top > wPosRect.Top then
         wRect.top := wPosRect.bottom - wRect.bottom;
   end;
   result := wRect;
end;

procedure TrmCustomToolWinForm.SetInternalFrameRect(const Value: TRect) ;
begin
   fFrameRect := Value;
end;

procedure TrmCustomToolWinForm.wmMove(var msg: TwmMove) ;
begin
   inherited;
   if assigned(fonMove) then
      fOnMove(self) ;
end;

destructor TrmCustomToolWinForm.destroy;
begin
   fWindowBMP.free;
   inherited;
end;

procedure TrmCustomToolWinForm.setncactive(const Value: boolean);
begin
  factive := Value;
  SendMessage(self.handle, wm_ncPaint, 0, 0) ;
end;

initialization
   NewBrush := TBrush.Create;
   NewBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite) ;

finalization
   if assigned(NewBrush) then
      NewBrush.free;
end.

