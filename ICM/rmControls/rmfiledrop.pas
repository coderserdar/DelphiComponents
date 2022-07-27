{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmFileDrop
Purpose  : Allows for files to be dropped from the Shell on target WinControl
Date     : 08-02-1997
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmFileDrop;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, Classes, Controls, rmGlobalComponentHook;

type
  TrmFileDrop = class(TComponent)
  private
    { Private declarations }
    fXPoint: integer;
    fYPoint: integer;
    fFileList: tstringlist;
    fOnFileDrop: TNotifyEvent;
    fDropLocation: TWinControl;
    fActive: boolean;
    fNeedReactivate: TNotifyEvent;
    procedure Activate;
    procedure Deactivate;
    procedure GetFileList(fhnd: integer);
    procedure SetDropLocation(location: TWinControl);
  protected
    { Protected declarations }
    OldWndProc: TFarProc;
    NewWndProc: Pointer;
    procedure HookWin;
    procedure UnhookWin;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function DropLocationHandle:THandle;
  public
    { Public declarations }
    procedure HookWndProc(var AMsg: TMessage);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetFileList;
    procedure Reparented;
    property Active : boolean read fActive default false;
    property FileList: tstringlist read ffilelist;
    property X: integer read fxpoint default -1;
    property Y: integer read fypoint default -1;
  published
    { Published declarations }
    property DropLocation: TWinControl read fDropLocation write SetDropLocation;
    property OnFileDroped: TNotifyEvent read fOnFileDrop write fOnFileDrop;
    property OnReactivateNeeded: TNotifyEvent read fNeedReactivate write fNeedReactivate;
  end;

implementation

uses SysUtils, Forms, ShellAPI;

procedure TrmFileDrop.activate;
begin
  if (csDesigning in componentstate) then exit;
  if fDropLocation = nil then exit;
  if factive = true then exit;
  if DropLocationHandle = 0 then exit;
  DragAcceptFiles(DropLocationHandle, true);
  Hookwin;
  factive := true;
end;

procedure TrmFileDrop.deactivate;
begin
  if (csDesigning in componentstate) then exit;
  if fDropLocation = nil then exit;
  if factive = false then exit;
  if DropLocationHandle = 0 then exit;
  DragAcceptFiles(DropLocationHandle, false);
  unhookwin;
  factive := false;
end;

procedure TrmFileDrop.getfilelist(fhnd: integer);
var
  fname: pchar;
  fnsize, fcount, index: integer;
  fdroppoint: tpoint;
begin
  ffilelist.Clear;
  DragQueryPoint(fhnd, fdroppoint);
  fxpoint := fdroppoint.x;
  fypoint := fdroppoint.y;
  fcount := dragqueryfile(fhnd, $FFFFFFFF, nil, 0);
  index := 0;
  while index < fcount do
  begin
    fnsize := DragQueryFile(fhnd, index, nil, 0);
    fname := stralloc(fnsize + 1);
    DragQueryFile(fhnd, index, fname, fnsize + 1);
    ffilelist.Add(strpas(fname));
    strdispose(fname);
    inc(index);
  end;
  dragfinish(fhnd);
end;

procedure TrmFileDrop.SetDropLocation(location: TWinControl);
begin
  if location <> fdroplocation then
  begin
    fdroplocation := location;
    deactivate;
    activate;
  end;
end;

procedure TrmFileDrop.HookWin;
begin
  if csDesigning in componentstate then exit;
  if not assigned(NewWndProc) then
  begin
    OldWndProc := TFarProc(GetWindowLong(DroplocationHandle, GWL_WNDPROC));
    {$ifdef D6_or_higher}
    NewWndProc := Classes.MakeObjectInstance(HookWndProc);
    {$else}
    NewWndProc := MakeObjectInstance(HookWndProc);
    {$endif}
    SetWindowLong(DroplocationHandle, GWL_WNDPROC, LongInt(NewWndProc));
    PushOldProc(fDroplocation, OldWndProc);
  end;
end; { HookWin }

procedure TrmFileDrop.UnhookWin;
begin
  if csDesigning in componentstate then exit;
  if assigned(NewWndProc) then
  begin
    SetWindowLong(DroplocationHandle, GWL_WNDPROC, LongInt(PopOldProc(fDroplocation)));
    if assigned(NewWndProc) then
    {$ifdef D6_or_higher}
       Classes.FreeObjectInstance(NewWndProc);
    {$else}
       FreeObjectInstance(NewWndProc);
    {$endif}
    NewWndProc := nil;
  end;
end; { UnHookWin }

procedure TrmFileDrop.loaded;
begin
  inherited loaded;
  activate;
end;

procedure TrmFileDrop.HookWndProc(var AMsg: TMessage);
begin
  if (AMsg.msg = WM_Destroy) then
  begin
    Deactivate;
    SendMessage(DropLocationHandle, AMsg.msg, AMsg.WParam, AMsg.LParam);
    AMsg.Result := 1;
    exit;
  end;
  if (AMsg.Msg = WM_DROPFILES) then
  begin
    if assigned(fonfiledrop) then
    begin
      getfilelist(AMsg.wparam);
      fonfiledrop(self);
    end;
    AMsg.Result := 0;
  end
  else
    AMsg.Result := CallWindowProc(OldWndProc, DropLocationHandle, AMsg.Msg, AMsg.wParam, AMsg.lParam);
end; { HookWndProc }

constructor TrmFileDrop.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ffilelist := tstringlist.create;
  factive := false;
  fdroplocation := nil;
end;

destructor TrmFileDrop.Destroy;
begin
  ffilelist.free;
  deactivate;
  inherited destroy; {Call default processing.}
end;

procedure TrmFileDrop.ResetFileList;
begin
  fFileList.Clear;
  fxPoint := -1;
  fyPoint := -1;
end;

procedure TrmFileDrop.Reparented;
begin
  Deactivate;
  Activate;
end;

procedure TrmFileDrop.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if aComponent = fDropLocation then
  begin
    if operation = opRemove then
    begin
      fDropLocation := nil;
      if not (csDestroying in ComponentState) and assigned(fNeedReactivate) then
        fNeedReactivate(self);
    end;
  end;
end;

function TrmFileDrop.DropLocationHandle: THandle;
begin
   if assigned(FDropLocation) then
   begin
      if fDropLocation <> owner then
         result := fDroplocation.handle
      else
      begin
         if (fDropLocation is TForm) and (TForm(fDropLocation).FormStyle = fsMDIForm) then
         begin
            if TForm(fDroplocation).Clienthandle > 0 then
               result := TForm(fDroplocation).Clienthandle
            else
               result := THandle(0);
         end
         else
           result := fDroplocation.handle;
      end;
   end
   else
   result := THandle(0);
end;

end.

