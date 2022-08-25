unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cefvcl, ceflib, cefgui, GR32_Image, AppEvnts;

type
  TMainform = class(TForm)
    PaintBox: TPaintBox32;
    chrmosr: TChromiumOSR;
    AppEvents: TApplicationEvents;
    procedure chrmosrPaint(Sender: TObject; const browser: ICefBrowser;
      kind: TCefPaintElementType; const dirtyRect: PCefRect;
      const buffer: Pointer);
    procedure PaintBoxResize(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure chrmosrCursorChange(Sender: TObject; const browser: ICefBrowser;
      cursor: HICON);
  end;

var
  Mainform: TMainform;

implementation

{$R *.dfm}

procedure TMainform.AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);
begin
  case Msg.message of
    WM_CHAR: chrmosr.Browser.SendKeyEvent(KT_CHAR, Msg.wParam, Msg.lParam, False, False);
    WM_KEYDOWN: chrmosr.Browser.SendKeyEvent(KT_KEYDOWN, Msg.wParam, Msg.lParam, False, False);
    WM_KEYUP: chrmosr.Browser.SendKeyEvent(KT_KEYUP, Msg.wParam, Msg.lParam, False, False);

    WM_SYSCHAR: chrmosr.Browser.SendKeyEvent(KT_CHAR, Msg.wParam, Msg.lParam, True, False);
    WM_SYSKEYDOWN: chrmosr.Browser.SendKeyEvent(KT_KEYDOWN, Msg.wParam, Msg.lParam, True, False);
    WM_SYSKEYUP: chrmosr.Browser.SendKeyEvent(KT_KEYUP, Msg.wParam, Msg.lParam, True, False);

    WM_IME_CHAR: chrmosr.Browser.SendKeyEvent(KT_CHAR, Msg.wParam, Msg.lParam, False, True);
    WM_IME_KEYDOWN: chrmosr.Browser.SendKeyEvent(KT_KEYDOWN, Msg.wParam, Msg.lParam, False, True);
    WM_IME_KEYUP: chrmosr.Browser.SendKeyEvent(KT_KEYUP, Msg.wParam, Msg.lParam, False, True);

    WM_MOUSEWHEEL:
      with TWMMouseWheel(Pointer(@Msg.message)^) do
        chrmosr.Browser.SendMouseWheelEvent(XPos, YPos, WheelDelta);
  end;
end;

procedure TMainform.chrmosrCursorChange(Sender: TObject;
  const browser: ICefBrowser; cursor: HICON);
begin
  SetCursor(cursor)
end;

procedure TMainform.chrmosrPaint(Sender: TObject; const browser: ICefBrowser;
  kind: TCefPaintElementType; const dirtyRect: PCefRect; const buffer: Pointer);
var
  src, dst: PByte;
  offset, i, w: Integer;
  vw, vh: Integer;
begin
  chrmosr.Browser.GetSize(PET_VIEW, vw, vh);
  with PaintBox.Buffer do
    if (vw = Width) and (vh = Height) then
    begin
      PaintBox.Canvas.Lock;
      Lock;
      try
        w := Width * 4;
        offset := ((dirtyRect.y * Width) + dirtyRect.x) * 4;
        src := @PByte(buffer)[offset];
        dst := @PByte(Bits)[offset];
        offset := dirtyRect.width * 4;
        for i := 0 to dirtyRect.height - 1 do
        begin
          Move(src^, dst^, offset);
          Inc(dst, w);
          Inc(src, w);
        end;
        PaintBox.Flush(Rect(dirtyRect.x, dirtyRect.y,
          dirtyRect.x + dirtyRect.width,  dirtyRect.y + dirtyRect.height));
      finally
        Unlock;
        PaintBox.Canvas.Unlock;
      end;
    end;
end;

procedure TMainform.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft: chrmosr.Browser.SendMouseClickEvent(X, Y, MBT_LEFT, False, 1);
    mbRight: chrmosr.Browser.SendMouseClickEvent(X, Y, MBT_RIGHT, False, 1);
    mbMiddle: chrmosr.Browser.SendMouseClickEvent(X, Y, MBT_MIDDLE, False, 1);
  end;
end;

procedure TMainform.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  chrmosr.Browser.SendMouseMoveEvent(X, Y, not PaintBox.MouseInControl);
end;

procedure TMainform.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft: chrmosr.Browser.SendMouseClickEvent(X, Y, MBT_LEFT, True, 1);
    mbRight: chrmosr.Browser.SendMouseClickEvent(X, Y, MBT_RIGHT, True, 1);
    mbMiddle: chrmosr.Browser.SendMouseClickEvent(X, Y, MBT_MIDDLE, True, 1);
  end;
end;

procedure TMainform.PaintBoxResize(Sender: TObject);
begin
  PaintBox.Buffer.SetSize(PaintBox.Width, PaintBox.Height);
  chrmosr.browser.SetSize(PET_VIEW, PaintBox.Width, PaintBox.Height);
  chrmosr.Browser.SendFocusEvent(True);
  Application.ProcessMessages;
end;

end.
