
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit FullScrn;

interface

{$I STD.INC}

uses
  Classes, Controls, SysUtils, Forms, Messages, Windows, WinTools;

type
  TFullScreenForm = class(TForm)
  private
    FPriorBounds: TRect;
    FFullScreen: Boolean;
    FHideTaskBar: Boolean;
    procedure ReleaseTaskBar;
    procedure SetFullScreen(Value: Boolean);
  public
    destructor Destroy; override;
    property FullScreen: Boolean read FFullScreen write SetFullScreen;
  end;

implementation

var
  TaskBarRef: Integer;

destructor TFullScreenForm.Destroy;
begin
  ReleaseTaskBar;
  inherited Destroy;
end;

procedure TFullScreenForm.ReleaseTaskBar;
begin
  if FHideTaskBar then
  begin
    Dec(TaskBarRef);
    if TaskBarRef = 0 then
      ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_SHOW);
  end;
  FHideTaskBar := False;
end;

procedure TFullScreenForm.SetFullScreen(Value: Boolean);
var
  Wnd: HWND;
  Position: TWindowPosition;
begin
  if Value <> FFullScreen then
  begin
    FFullScreen := Value;
    Align := alNone;
    BorderStyle := bsNone;
    if FFullScreen then
    begin
      FPriorBounds := BoundsRect;
      with Monitor do
      begin
        Wnd := FindWindow('Shell_TrayWnd', nil);
        GetWindowPosition(Wnd, Position);
        FHideTaskBar := (Left >= Position.Left) and (Top >= Position.Top) and
          (Left + Width <= Position.Left + Position.Width) and (Top + Height <=
          Position.Top + Position.Height);
        if FHideTaskBar then
        begin
          ShowWindow(Wnd, SW_HIDE);
          Inc(TaskBarRef);
        end;
        SetBounds(Left, Top, Width, Height);
      end;
    end
    else
    begin
      BorderStyle := bsSizeable;
      with FPriorBounds do
        SetBounds(Left, Top, Right - Left, Bottom - Top);
      ReleaseTaskBar;
    end;
  end;
end;

end.
