{ Copyright (C) 1998-2003, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29

  This component allow to limit sizes for form (max/min height and/or width

  Also you may define the background image with predefined style:
    psStretch, psCenter, psTile
  Background image will work correctly even for MDIChild forms

  To use:
   - drop/create on formó
   - set Enabled proeprty in True
   - specify HeightMin & HeightMax and WidthMin & WidthMax proeprties
}
unit LimitFrm;

interface

uses Windows, Messages, Classes, Graphics, Controls, SysUtils;

type
  ENonFormOwner = class(Exception);
  TPictureStyle = (psStretch, psCenter, psTile);

  TsohoOnPaint = procedure (Sender : TObject; Canvas : TCanvas) of object;

  TWallpaperControl = class(TComponent)
  private
    { Private declarations }
    DefaultWndProc: TFarProc;
    OverrideWndProc: TFarProc;
    FGrabbed: boolean;
    FBackground: TPicture;
    FPictureStyle: TPictureStyle;
    FHookedHandle: THandle;
    FSet: boolean;
    FOnPaint: TsohoOnPaint;
    FDraw: Boolean;

    FLimitEnabled: Boolean; {on/off}
    FHeightMin, FHeightMax: Integer; {Min & Max sizes for form height}
    FWidthMin, FWidthMax: Integer; {Min & Max ðàçìåð for form width}

    procedure SetBackground(Value: TPicture);
    procedure SetLimitEnabled(Value: Boolean);
    procedure SetHeightMin(Value: Integer);
    procedure SetHeightMax(Value: Integer);
    procedure SetWidthMin(Value: Integer);
    procedure SetWidthMax(Value: Integer);

    procedure BitmapChanged(Sender: TObject);
    procedure SetDraw(Value: Boolean);
    procedure SetLimits;
  protected
    { Protected declarations }
    procedure DoPaint(Canvas: TCanvas); virtual;
    procedure DrawTile(Dest: TBitmap); virtual;
    procedure DrawCenter(Dest: TBitmap); virtual;
    procedure DrawStretch(Dest: TBitmap); virtual;
    procedure FreeHandlerHook; virtual;
    procedure WNDPROC(var Msg: TMessage); virtual;
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    procedure WMSize(var Msg: TWMSize); message WM_Size;
    procedure WMMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMEraseBkgnd(var message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint (var message: TMessage); message WM_Paint;
    procedure Loaded; override;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { hook the paint for form owner }
    procedure HookPaint; virtual;
    procedure DefaultHandler(var Msg); override;

    { draw the background or not}
    property Draw: Boolean read FDraw write SetDraw;
  published
    property PictureStyle: TPictureStyle read FPictureStyle write FPictureStyle default psCenter;
    property Background: TPicture read FBackground write SetBackground;
    property OnPaint: TsohoOnPaint read FOnPaint write FOnPaint;

    property LimitEnabled: Boolean read FLimitEnabled write SetLimitEnabled default True;
    property HeightMin: Integer read FHeightMin write SetHeightMin default 0;
    property HeightMax: Integer read FHeightMax write SetHeightMax;
    property WidthMin: Integer read FWidthMin write SetWidthMin default 0;
    property WidthMax: Integer read FWidthMax write SetWidthMax;
  end;

procedure Register;

implementation
uses Forms;

procedure TWallpaperControl.SetDraw(Value: Boolean);
begin
  if FDraw = Value then exit;
  FDraw := Value;
  InvalidateRect(FHookedHandle, nil, True);
end;

procedure TWallpaperControl.WMPaint(var message: TMessage);
begin
  if FGrabbed and
     ((Owner as TForm).FormStyle = fsMDIForm) then
    InvalidateRect(FHookedHandle, nil, True);
  inherited;
end;

procedure TWallpaperControl.WMSize(var Msg: TWMSize);
begin
  inherited;

  if FGrabbed then
    InvalidateRect(FHookedHandle, nil, True);
end;

procedure TWallpaperControl.WMMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  if LimitEnabled then
  begin
    with Msg.MinMaxInfo^ do
    begin
      with ptMinTrackSize do
      begin
        if WidthMin > 0 then X := WidthMin;
        if HeightMin > 0 then Y := HeightMin;
      end;
      with ptMaxTrackSize do
      begin
        if WidthMax > 0 then X := WidthMax;
        if HeightMax > 0 then Y := HeightMax;
      end;
    end;
  end;
//     SetLimits;

  inherited;
end;

procedure TWallpaperControl.BitmapChanged(Sender: TObject);
begin
  if csLoading in ComponentState then exit;
  if FGrabbed then
    InvalidateRect(FHookedHandle, nil, True);
end;

procedure TWallpaperControl.SetBackground(Value: TPicture);
begin
  FBackground.Assign(Value);
  if Value <> nil then
    FSet := True;
end;

procedure TWallpaperControl.SetLimitEnabled(Value: Boolean);
begin
  if FLimitEnabled <> Value then
    FLimitEnabled := Value;
  if FLimitEnabled then
    SetLimits;
end;

procedure TWallpaperControl.SetHeightMin(Value: Integer);
begin
  if FHeightMin <> Value then
  begin
    FHeightMin := Value;
    if FLimitEnabled then
    begin
      if (TForm(Owner).Height < FHeightMin) then TForm(Owner).Height := FHeightMin;
      TForm(Owner).RePaint;
    end;
  end;
end;

procedure TWallpaperControl.SetHeightMax(Value: Integer);
begin
  if FHeightMax <> Value then
  begin
    FHeightMax := Value;
    if FLimitEnabled then
    begin
      if TForm(Owner).Height > FHeightMax then TForm(Owner).Height := FHeightMax;
      TForm(Owner).RePaint;
    end;
  end;
end;

procedure TWallpaperControl.SetWidthMin(Value: Integer);
begin
  if FWidthMin <> Value then
  begin
    FWidthMin := Value;
    if FLimitEnabled then
    begin
      if TForm(Owner).Width < FWidthMin then TForm(Owner).Width := FWidthMin;
      TForm(Owner).RePaint;
    end;
  end;
end;

procedure TWallpaperControl.SetWidthMax(Value: Integer);
begin
  if FWidthMax <> Value then
  begin
    FWidthMax := Value;
    if FLimitEnabled then
    begin
      if TForm(Owner).Width > FWidthMax then TForm(Owner).Width := FWidthMax;
      TForm(Owner).RePaint;
    end;
  end;
end;

procedure TWallpaperControl.SetLimits;
begin
  if TForm(Owner).Height < FHeightMin then
    TForm(Owner).Height := FHeightMin;
  if TForm(Owner).Height > FHeightMax then
    TForm(Owner).Height := FHeightMax;
  if TForm(Owner).Width < FWidthMin then
    TForm(Owner).Width := FWidthMin;
  if TForm(Owner).Width > FWidthMax then
    TForm(Owner).Width := FWidthMax;
end;

constructor TWallpaperControl.Create(AOwner: TComponent);
begin
  if not (AOwner is TForm) then
    raise ENonFormOwner.Create('Owner must be a form!');
  inherited Create(AOwner);

  FGrabbed := False;
  FSet := False;
  FPictureStyle := psCenter;
  FBackground := TPicture.Create;
  FBackground.OnChange := BitmapChanged;
  FDraw := True;

  FLimitEnabled := True;
end;

procedure TWallpaperControl.HookPaint;
begin
  if FGrabbed then exit;
  with (Owner as TForm) do
  begin
    if FormStyle = fsMDIForm then
      FHookedHandle := ClientHandle
    else
      FHookedHandle := Handle;
  end;
  DefaultWndProc := TFarProc(GetWindowLong(FHookedHandle, GWL_WNDPROC));
  OverrideWndProc := MakeObjectInstance(WNDPROC);
  SetWindowLong(FHookedHandle, GWL_WNDPROC, Longint(OverrideWndProc));
  FGrabbed := True;
end;

procedure TWallpaperControl.Loaded;
begin
  inherited Loaded;

  HookPaint;

  HeightMax := TForm(Owner).Height;
  WidthMax := TForm(Owner).Width;
end;

procedure TWallpaperControl.FreeHandlerHook;
begin
  SetWindowLong(FHookedHandle, GWL_WNDPROC, Longint(DefaultWndProc));
  FGrabbed := False;
end;

destructor TWallpaperControl.Destroy;
begin
  if FGrabbed then
    FreeHandlerHook;
  FreeObjectInstance(OverrideWndProc);
  FBackground.Free;

  inherited Destroy;
end;

procedure TWallpaperControl.WMDestroy(var Msg: TWMDestroy);
begin
  if FGrabbed then
    FreeHandlerHook;
  inherited;
end;

procedure TWallpaperControl.WNDPROC(var Msg: TMessage);
begin
  if Msg.Result = 0 then Dispatch(Msg);
end;

procedure TWallpaperControl.DefaultHandler(var Msg);
begin
  with TMessage(Msg) do
    Result := CallWindowProc(DefaultWndProc, (Owner as TWinControl).Handle, Msg, WPARAM, LPARAM);
end;

procedure TWallpaperControl.DrawTile(Dest: TBitmap);
var i, j: Integer;
    CopyRect: TRect;
begin
  for i := 0 to Round(Dest.Width / FBackground.Width) do
    for j := 0 to Round(Dest.Height / FBackground.Height) do
    begin
      with CopyRect do
      begin
        Left := i * FBackground.Width;
        Top := j * FBackground.Height;
        Right := Left + FBackground.Width;
        if Right > Dest.Width then
          Right := Dest.Width;
        Bottom := Top + FBackground.Height;
        if Bottom > Dest.Height then
          Bottom := Dest.Height;
      end;
      Dest.Canvas.CopyMode := cmSrcCopy;
      Dest.Canvas.CopyRect(CopyRect, FBackground.Bitmap.Canvas,
                           Bounds(0, 0, CopyRect.Right - CopyRect.Left,
                           CopyRect.Bottom - CopyRect.Top));
    end;
end;

procedure TWallpaperControl.DrawCenter(Dest: TBitmap);
var X, Y: Integer;
    CopyRect: TRect;
begin
  CopyRect := (Owner as TForm).ClientRect;
  Dest.Canvas.Brush.Color := (Owner as TForm).Color;
  Dest.Canvas.FillRect(CopyRect);
  X := (Dest.Width - FBackground.Width) div 2;
  Y := (Dest.Height - FBackground.Height) div 2;
  Dest.Canvas.Draw(X, Y, FBackground.Graphic);
end;

procedure TWallpaperControl.DrawStretch(Dest: TBitmap);
var CopyRect: TRect;
begin
  Dest.Canvas.CopyMode := cmSrcCopy;
  CopyRect := (Owner as TForm).ClientRect;
  Dest.Canvas.StretchDraw(CopyRect, FBackground.Graphic);
end;

procedure TWallpaperControl.DoPaint(Canvas: TCanvas);
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self, Canvas);
end;

procedure TWallpaperControl.WMEraseBkgnd(var message: TWMEraseBkgnd);
var DestRect: TRect;
    MemBmp: TBitmap;
    TmpCanvas: TCanvas;
begin
  MemBmp := TBitmap.Create;
  try
    if ((FBackground.Width = 0) or
        (FBackground.Height = 0)) or
        (not FDraw) then
    begin
      inherited;

      TmpCanvas := TCanvas.Create;
      TmpCanvas.Handle := Message.DC;
      DoPaint(TmpCanvas);
      TmpCanvas.Free;
    end
    else
    begin
      if (Owner as TForm).FormStyle <> fsMDIForm then
        with DestRect do
        begin
          Left := 0;
          Top := 0;
          Right := (Owner as TForm).ClientWidth;
          Bottom := (Owner as TForm).ClientHeight;
        end
      else
        GetClipBox(Message.DC, DestRect);
      MemBmp.Width := (DestRect.Right - DestRect.Left);
      MemBmp.Height := (DestRect.Bottom - DestRect.Top);
      case FPictureStyle of
        psTile: DrawTile(MemBmp);
        psCenter: DrawCenter(MemBmp);
        psStretch: DrawStretch(MemBmp);
      end;
      DoPaint(MemBmp.Canvas);
      BitBlt(Message.DC, DestRect.Left, DestRect.Top,
            (DestRect.Right - DestRect.Left), (DestRect.Bottom - DestRect.Top),
             MemBmp.Canvas.Handle, DestRect.Left, DestRect.Top, SRCCOPY);
      Message.Result := 1;
    end;
  finally
    MemBmp.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('SMComponents', [TWallpaperControl]);
end;

end.
