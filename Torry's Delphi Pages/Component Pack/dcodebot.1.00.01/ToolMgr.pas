
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ToolMgr;

interface

{$I STD.INC}

uses
  Classes, Controls, SysUtils;

{ TToolbarItem }

type
  TToolbarItem = class(TCollectionItem)
  private
    FControl: TControl;
    FName: string;
  public
    property Control: TControl read FControl write FControl;
    property Name: string read FName write FName;
  end;

{ TToolbarManager }

  TToolbarManager = class(TCollection)
  protected
    function Get(Index: Integer): TToolbarItem;
    procedure Put(Index: Integer; Value: TToolbarItem);
  public
    constructor Create;
    function Add: TToolbarItem;
    function Insert(Index: Integer): TToolbarItem;
    property Items[Index: Integer]: TToolbarItem read Get write Put; default;
  end;

function ToolbarManager: TToolbarManager;

implementation

constructor TToolbarManager.Create;
begin
  inherited Create(TToolbarItem);
end;

function TToolbarManager.Add: TToolbarItem;
begin
  Result := inherited Add as TToolbarItem;
end;

function TToolbarManager.Insert(Index: Integer): TToolbarItem;
begin
  Result := inherited Insert(Index) as TToolbarItem;
end;

function TToolbarManager.Get(Index: Integer): TToolbarItem;
begin
  Result := GetItem(Index) as TToolbarItem;
end;

procedure TToolbarManager.Put(Index: Integer; Value: TToolbarItem);
begin
  SetItem(Index, Value);
end;

var
  InternalToolbarManager: TObject;

function ToolbarManager: TToolbarManager;
begin
  if InternalToolbarManager = nil then
    InternalToolbarManager := TToolbarManager.Create;
  Result := TToolbarManager(InternalToolbarManager);
end;

initialization
  InternalToolbarManager := nil;
finalization
  InternalToolbarManager.Free;
end.
