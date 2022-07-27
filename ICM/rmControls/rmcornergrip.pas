{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmCornerGrip
Purpose  : Allow for a working "Size Grip" to be placed on the components
           owner form.
Date     : 10-26-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================ }

unit rmCornerGrip;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, Classes, forms, Controls;

type
  TrmCornerGrip = class(TComponent)
  private
    { Private declarations }
    OldWndProc: TFarProc;
    NewWndProc: Pointer;

    fHeight : integer;
    fWidth : integer;
    fCanPaint : boolean;

    procedure HookWndProc(var AMsg: TMessage);
    procedure HookWin;
    procedure UnhookWin;
    procedure setHeight(const Value: integer);
    procedure setwidth(const Value: integer);
  protected
    { Protected declarations }
    procedure Paint;
    function GripRect:TRect;
    function GripTestRect:TRect;
  public
    { Public declarations }
    constructor create(AOwner : TComponent); override;
    destructor destroy; override;
  published
    { Published declarations }
    property Height:integer read fheight write setHeight default 15;
    property Width :integer read fWidth write setwidth default 20;
  end;

implementation

Uses rmglobalComponentHook, Graphics;

type
    TWinControlInvasion = class(TWinControl)
    end;

{ TrmCornerGrip }

constructor TrmCornerGrip.create(AOwner: TComponent);
begin
  inherited;
  fHeight := 15;
  fwidth := 20;
  fCanPaint := true;
  HookWin;
end;

destructor TrmCornerGrip.destroy;
begin
  UnhookWin;
  inherited;
end;

procedure TrmCornerGrip.HookWin;
begin
  if csdesigning in componentstate then exit;
  if not assigned(NewWndProc) then
  begin
    OldWndProc := TFarProc(GetWindowLong(TForm(Owner).handle, GWL_WNDPROC));
    {$ifdef D6_or_higher}
    NewWndProc := Classes.MakeObjectInstance(HookWndProc);
    {$else}
    NewWndProc := MakeObjectInstance(HookWndProc);
    {$endif}
    SetWindowLong(TForm(Owner).handle, GWL_WNDPROC, LongInt(NewWndProc));
    PushOldProc(TForm(Owner), OldWndProc);
  end;
end;

procedure TrmCornerGrip.UnhookWin;
begin
  if csdesigning in componentstate then exit;
  if assigned(NewWndProc) then
  begin
    SetWindowLong(TForm(Owner).handle, GWL_WNDPROC, LongInt(PopOldProc(TForm(Owner))));
    if assigned(NewWndProc) then
    {$ifdef D6_or_higher}
       Classes.FreeObjectInstance(NewWndProc);
    {$else}
       FreeObjectInstance(NewWndProc);
    {$endif}
    NewWndProc := nil;
    OldWndProc := nil;
  end;
end;

procedure TrmCornerGrip.HookWndProc(var AMsg: TMessage);
var
   wPt : TPoint;
   wRect : TRect;
begin
  case AMsg.msg of
    wm_destroy:
       begin
          AMsg.Result := CallWindowProc(OldWndProc, Tform(Owner).handle, AMsg.Msg, AMsg.wParam, AMsg.lParam);
          UnHookWin;
          exit;
       end;
    wm_NCHitTest:
       begin
          wPt.x := aMsg.lParam and $0000FFFF;
          wPT.y := (amsg.lparam and $FFFF0000) shr 16;
          wRect := GripTestRect;
          if ptInRect(wRect,wPT) then
          begin
             AMsg.Result := HTBOTTOMRIGHT;
             Paint;
             exit;
          end
       end;
    WM_ENTERSIZEMOVE:
       begin
          fCanPaint := False;
          Tform(Owner).Repaint;
       end;
  end;

  AMsg.Result := CallWindowProc(OldWndProc, Tform(Owner).handle, AMsg.Msg, AMsg.wParam, AMsg.lParam);

  case AMsg.msg of
    WM_EXITSIZEMOVE:
    begin
       fCanPaint := true;
       Paint;
    end;
    WM_ERASEBKGND:
     begin
        paint;
     end;
  end;
end;

procedure TrmCornerGrip.Paint;
var
   wRect : TRect;
begin
   if csdesigning in componentstate then exit;
   if not fCanPaint then exit;

   wrect := GripRect;
   wRect.Left := wRect.Left;
   wRect.Top := wRect.top;
   Tform(Owner).Update;
   DrawFrameControl(Tform(Owner).Canvas.handle, wRect, DFC_SCROLL, DFCS_SCROLLSIZEGRIP)
end;

procedure TrmCornerGrip.setHeight(const Value: integer);
begin
  fheight := Value;
  Paint;
end;

procedure TrmCornerGrip.setwidth(const Value: integer);
begin
  fWidth := Value;
  Paint;
end;

function TrmCornerGrip.GripRect: TRect;
begin
   result := TForm(Owner).ClientRect;
   result.top := result.bottom - fHeight;
   result.Left := result.right - fWidth;
end;

function TrmCornerGrip.GripTestRect: TRect;
begin
   result := TForm(Owner).BoundsRect;
   result.top := result.bottom - fHeight;
   result.Left := result.right - fWidth;
end;

end.
