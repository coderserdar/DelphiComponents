
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ClipNotify;

interface

{$I STD.INC}

uses
  Windows, Messages, Classes, SysUtils, WinTools;

type
  TClipboardNotifier = class(TObject)
  private
    FEvents: TList;
    FLinkWindow: HWND;
    FUtilityWindow: TUtilityWindow;
    procedure WMChangeCBChain(var Message: TWMChangeCBChain); message WM_CHANGECBCHAIN;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMDrawClipboard(var Message: TWMDrawClipboard); message WM_DRAWCLIPBOARD;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;
    procedure RegisterChanges(Event: TNotifyEvent);
    procedure UnregisterChanges(Event: TNotifyEvent);
  end;

// function ClipboardNotifier: TClipboardNotifier;

implementation

constructor TClipboardNotifier.Create;
begin
  inherited Create;
  FEvents := TList.Create;
  FUtilityWindow := TUtilityWindow.Create(Self);
  FLinkWindow := SetClipboardViewer(FUtilityWindow.Handle);
end;

destructor TClipboardNotifier.Destroy;
begin
  FEvents.Free;
  FUtilityWindow.Free;
  inherited Destroy;
end;

procedure TClipboardNotifier.DefaultHandler(var Message);
var
  Msg: TMessage;
begin
  with Msg do
    DefWindowProc(FUtilityWindow.Handle, Msg, WParam, LParam);
end;

procedure TClipboardNotifier.RegisterChanges(Event: TNotifyEvent);
var
  Method: TMethod absolute Event;
begin
  FEvents.Add(Method.Code);
  FEvents.Add(Method.Data);
end;

procedure TClipboardNotifier.UnregisterChanges(Event: TNotifyEvent);
var
  Method: TMethod absolute Event;
  I: Integer;
begin
  for I := 0 to FEvents.Count div 2 - 1 do
    if (Method.Code = FEvents[I * 2]) and (Method.Data = FEvents[I * 2 + 1]) then
    begin
      FEvents.Delete(I * 2 + 1);
      FEvents.Delete(I * 2);
    end;
end;

procedure TClipboardNotifier.WMChangeCBChain(var Message: TWMChangeCBChain);
begin
  with Message do
  begin
    if FLinkWindow = Remove then
      FLinkWindow := Next
    else if FLinkWindow <> 0 then
      SendMessage(FLinkWindow, Remove, Next, WM_CHANGECBCHAIN);
    Result := 0;
  end;
end;

procedure TClipboardNotifier.WMDestroy(var Message: TWMDestroy);
begin
  ChangeClipboardChain(FUtilityWindow.Handle, FLinkWindow);
  FLinkWindow := 0;
  inherited;
end;

procedure TClipboardNotifier.WMDrawClipboard(var Message: TWMDrawClipboard);
var
  Method: TMethod;
  Event: TNotifyEvent absolute Method;
  I: Integer;
begin
  for I := 0 to FEvents.Count div 2 - 1 do
  begin
    Method.Code := FEvents[I * 2];
    Method.Data := FEvents[I * 2 + 1];
    Event(Self);
  end;
  if FLinkWindow <> 0 then
    with Message do
      Result := SendMessage(FLinkWindow, WM_DRAWCLIPBOARD, 0, 0)
end;

var
  InternalClipboardNotifier: TObject;

function ClipboardNotifier: TClipboardNotifier;
begin
  if InternalClipboardNotifier = nil then
    InternalClipboardNotifier := TClipboardNotifier.Create;
  Result := TClipboardNotifier(InternalClipboardNotifier);
end;

initialization
  InternalClipboardNotifier := nil;
finalization
  InternalClipboardNotifier.Free;
end.
