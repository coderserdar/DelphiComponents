{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmScrnCtrls
Purpose  : This is a basic unit containing the controls that are used by the
           rmControls for various types of dropdowns.
Date     : 09-24-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmScrnCtrls;

interface

{$I CompilerDefines.INC}

uses Windows, Messages, Controls, Classes, StdCtrls, ComCtrls, rmPathTreeView;

const
   WM_CaptureKeyDown = WM_USER + $B900; //These constants were found using WinSight.....
   WM_CaptureKeyUP = WM_CaptureKeyDown + 1;

type
  TrmCustomScreenListBox = class(TListBox)
  private
   { Private declarations }
   procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
   procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
   procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
   procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
  protected
   { Protected declarations }
   procedure CreateParams(var Params: TCreateParams); override;
   procedure CreateWnd; override;
   procedure VisibleChanging; override;
  public
   { Public declarations }
   procedure WndProc(var Message: TMessage); override;
  end;

  TrmCustomScreenTreeView = class(TTreeView)
  private
   { Private declarations }
   procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
   procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
   procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
   procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
  protected
   { Protected declarations }
   procedure CreateParams(var Params: TCreateParams); override;
   procedure CreateWnd; override;
   procedure VisibleChanging; override;
  public
   { Public declarations }
   procedure WndProc(var Message: TMessage); override;
  end;

  TrmCustomScreenPathTreeView = class(TrmPathTreeView)
  private
   { Private declarations }
   procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
   procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
   procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
   procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
  protected
   { Protected declarations }
   procedure CreateParams(var Params: TCreateParams); override;
   procedure CreateWnd; override;
   procedure VisibleChanging; override;
  public
   { Public declarations }
   procedure WndProc(var Message: TMessage); override;
  end;

implementation

{ TrmCustomScreenListBox }

procedure TrmCustomScreenListBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  ReleaseCapture;
end;

procedure TrmCustomScreenListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  SetCaptureControl(self);
end;

procedure TrmCustomScreenListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TrmCustomScreenListBox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TrmCustomScreenListBox.VisibleChanging;
begin
  if Visible = false then
    SetCaptureControl(self)
  else
    ReleaseCapture;
  inherited;
end;

procedure TrmCustomScreenListBox.WMLButtonDown(
  var Message: TWMLButtonDown);
begin
  if not ptInRect(clientrect, point(message.xpos, message.ypos)) then
    Visible := false;
  inherited;
end;

procedure TrmCustomScreenListBox.WMMouseMove(var Message: TWMMouse);
begin
  if ptInRect(clientrect, point(message.xpos, message.ypos)) then
    ReleaseCapture;
  inherited;
end;

procedure TrmCustomScreenListBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_CaptureKeyDown:
      begin
        Message.msg := wm_KeyDown;
      end;
    WM_CaptureKeyup:
      begin
        Message.msg := wm_KeyUp;
      end;
  end;
  inherited WndProc(Message);
end;

{ TrmCustomScreenTreeView }

procedure TrmCustomScreenTreeView.CMMouseEnter(var Message: TMessage);
begin
   inherited;
   ReleaseCapture;
end;

procedure TrmCustomScreenTreeView.CMMouseLeave(var Message: TMessage);
begin
   inherited;
   SetCaptureControl(self);
end;

procedure TrmCustomScreenTreeView.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   with Params do
   begin
      Style := Style or WS_BORDER;
      ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
      WindowClass.Style := CS_SAVEBITS;
   end;
end;

procedure TrmCustomScreenTreeView.CreateWnd;
begin
   inherited CreateWnd;
   Windows.SetParent(Handle, 0);
   CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TrmCustomScreenTreeView.VisibleChanging;
begin
   if Visible = false then
      SetCaptureControl(self)
   else
      ReleaseCapture;
   inherited;
end;

procedure TrmCustomScreenTreeView.WMLButtonDown(
   var Message: TWMLButtonDown);
begin
   if not ptInRect(clientrect, point(message.xpos, message.ypos)) then
      Visible := false;
   inherited;
end;

procedure TrmCustomScreenTreeView.WMMouseMove(var Message: TWMMouse);
begin
   if ptInRect(clientrect, point(message.xpos, message.ypos)) then
      ReleaseCapture;
   inherited;
end;

procedure TrmCustomScreenTreeView.WndProc(var Message: TMessage);
begin
   case Message.Msg of
      WM_CaptureKeyDown:
         begin
            Message.msg := wm_KeyDown;
         end;
      WM_CaptureKeyup:
         begin
            Message.msg := wm_KeyUp;
         end;
   end;
   inherited WndProc(Message);
end;

{ TrmCustomScreenPathTreeView }

procedure TrmCustomScreenPathTreeView.CMMouseEnter(var Message: TMessage);
begin
   inherited;
   ReleaseCapture;
end;

procedure TrmCustomScreenPathTreeView.CMMouseLeave(var Message: TMessage);
begin
   inherited;
   SetCaptureControl(self);
end;

procedure TrmCustomScreenPathTreeView.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   with Params do
   begin
      Style := Style or WS_BORDER;
      ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
      WindowClass.Style := CS_SAVEBITS;
   end;
end;

procedure TrmCustomScreenPathTreeView.CreateWnd;
begin
   inherited CreateWnd;
   Windows.SetParent(Handle, 0);
   CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TrmCustomScreenPathTreeView.VisibleChanging;
begin
   if Visible = false then
      SetCaptureControl(self)
   else
      ReleaseCapture;
   inherited;
end;

procedure TrmCustomScreenPathTreeView.WMLButtonDown(
   var Message: TWMLButtonDown);
begin
   if not ptInRect(clientrect, point(message.xpos, message.ypos)) then
      Visible := false;
   inherited;
end;

procedure TrmCustomScreenPathTreeView.WMMouseMove(var Message: TWMMouse);
begin
   if ptInRect(clientrect, point(message.xpos, message.ypos)) then
      ReleaseCapture;
   inherited;
end;

procedure TrmCustomScreenPathTreeView.WndProc(var Message: TMessage);
begin
   case Message.Msg of
      WM_CaptureKeyDown:
         begin
            Message.msg := wm_KeyDown;
         end;
      WM_CaptureKeyup:
         begin
            Message.msg := wm_KeyUp;
         end;
   end;
   inherited WndProc(Message);
end;

end.
