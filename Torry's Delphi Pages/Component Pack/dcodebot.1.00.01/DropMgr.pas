
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit DropMgr;

interface

{$I STD.INC}

uses
  Classes, Controls, Windows, Messages, ShellAPI, SysUtils;

{ TDropManager class }

type
  TDropFilesEvent = procedure(Sender: TObject; Files: TStrings) of object;

  TDropManager = class
  private
    FList: TList;
    FUseUNC: Boolean;
    FOnDropFiles: TDropFilesEvent;
    procedure SetUseUNC(Value: Boolean);
    function GetUseUNC: Boolean;
    procedure SetOnDropFiles(const Value: TDropFilesEvent);
    function GetOnDropFiles: TDropFilesEvent;
    function FindDropControl(const AWinControl: TWinControl): TObject;
  protected
    procedure DoDropFiles(DropControl: TObject; Drop: THandle);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterControl(const AWinControl: TWinControl; DoDropFiles: TDropFilesEvent);
    procedure UnregisterControl(const AWinControl: TWinControl);
    function ControlRegistered(const AWinControl: TWinControl): Boolean;
    property UseUNC: Boolean read GetUseUNC write SetUseUNC;
    property OnDropFiles: TDropFilesEvent read GetOnDropFiles write SetOnDropFiles;
  end;

{ DropManager function }

function DropManager: TDropManager;

implementation

type
  TDropControl = class
  private
    FWndMethod: TWndMethod;
    FWinControl: TWinControl;
    FOnDropFiles: TDropFilesEvent;
  protected
    procedure ProcessMessage(var Msg: TMessage);
  public
    constructor Create(AWinControl: TWinControl; DoDropFiles: TDropFilesEvent);
    destructor Destroy; override;
    property Control: TWinControl read FWinControl;
  end;

{ TDropControl }

constructor TDropControl.Create(AWinControl: TWinControl; DoDropFiles: TDropFilesEvent);
begin
  FWinControl := AWinControl;
  FWndMethod := FWinControl.WindowProc;
  FWinControl.WindowProc := ProcessMessage;
  FOnDropFiles := DoDropFiles;
  DragAcceptFiles(FWinControl.Handle, True);
end;

destructor TDropControl.Destroy;
begin
  FWinControl.WindowProc := FWndMethod;
  DropManager.FList.Remove(Self);
  DragAcceptFiles(FWinControl.Handle, False);
end;

procedure TDropControl.ProcessMessage(var Msg: TMessage);
var
  DestroyMsgProc: TWndMethod;
begin
  case Msg.Msg of
    WM_CREATE:
      DragAcceptFiles(FWinControl.Handle, True);
    WM_DROPFILES:
    begin
      DropManager.DoDropFiles(Self, Msg.wParam);
      Msg.Result := 0;
    end;
    WM_DESTROY:
    begin
      DestroyMsgProc := FWndMethod;
      Destroy;
      DestroyMsgProc(Msg);
    end
    else
      FWndMethod(Msg);
  end;
end;

{ TDropManager }

constructor TDropManager.Create;
begin
  FList := TList.Create;
end;

destructor TDropManager.Destroy;
begin
  FList.Free;
end;

procedure TDropManager.SetUseUNC(Value: Boolean);
begin
  FUseUNC := Value;
end;

function TDropManager.GetUseUNC: Boolean;
begin
  Result := FUseUNC;
end;

procedure TDropManager.SetOnDropFiles(const Value: TDropFilesEvent);
begin
  FOnDropFiles := Value;
end;

function TDropManager.GetOnDropFiles: TDropFilesEvent;
begin
  Result := FOnDropFiles;
end;

function TDropManager.FindDropControl(const AWinControl: TWinControl): TObject;
var
  j: Integer;
begin
  Result := nil;
  for j := 0 to FList.Count-1 do
  if TDropControl(FList[j]).Control = AWinControl then
  begin
    Result := FList[j];
    Break;
  end;
end;

procedure TDropManager.DoDropFiles(DropControl: TObject; Drop: THandle);
var
  j: Integer;
  Files: TStrings;
  Buffer: array [0..MAX_PATH] of Char;
begin
  Files := TStringList.Create;
  try
    for j := 0 to DragQueryFile(Drop, $FFFFFFFF, Buffer, MAX_PATH)-1 do
    begin
      DragQueryFile(Drop, j, Buffer, MAX_PATH);
      if FUseUNC then
        Files.Add(ExpandUNCFilename(StrPas(Buffer)))
      else
        Files.Add(StrPas(Buffer));
    end;
    with TDropControl(DropControl) do
      if Assigned(FOnDropFiles) then
        FOnDropFiles(Control, Files)
      else if Assigned(Self.FOnDropFiles) then
        Self.FOnDropFiles(Control, Files)
      else
        SetWindowText(Control.Handle, PChar(Files[0]));
  finally
    Files.Free;
    DragFinish(Drop);
  end;
end;

procedure TDropManager.RegisterControl(const AWinControl: TWinControl; DoDropFiles: TDropFilesEvent);
var
  DropControl: TObject;
begin
  DropControl := FindDropControl(AWinControl);
  if not Assigned(DropControl) then
  begin
    DropControl := TDropControl.Create(AWinControl, DoDropFiles);
    FList.Add(DropControl);
  end;
end;

procedure TDropManager.UnregisterControl(const AWinControl: TWinControl);
var
  DropControl: TObject;
begin
  DropControl := FindDropControl(AWinControl);
  if Assigned(DropControl) then
    DropControl.Free;
end;

function TDropManager.ControlRegistered(const AWinControl: TWinControl): Boolean;
begin
  Result := Assigned(FindDropControl(AWinControl));
end;

var
  Manager: TObject;

function DropManager: TDropManager;
begin
  if Manager = nil then
    Manager := TDropManager.Create;
  Result := TDropManager(Manager);
end;

initialization
  Manager := nil;
finalization
  Manager.Free;
end.



