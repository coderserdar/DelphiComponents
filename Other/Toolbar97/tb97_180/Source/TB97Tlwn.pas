unit TB97Tlwn;

{
  Toolbar97
  Copyright (C) 1998-2004 by Jordan Russell
  http://www.jrsoftware.org/

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.


  TToolWindow97

  $jrsoftware: tb97/Source/TB97Tlwn.pas,v 1.3 2004/02/23 22:53:00 jr Exp $
}

interface

{$I TB97Ver.inc}

uses
  Windows, Classes, Controls, TB97;

type
  { TToolWindow97 }

  TToolWindow97 = class(TCustomToolWindow97)
  private
    FMinClientWidth, FMinClientHeight: Integer;
    FBarHeight, FBarWidth: Integer;
    function GetClientAreaWidth: Integer;
    procedure SetClientAreaWidth (Value: Integer);
    function GetClientAreaHeight: Integer;
    procedure SetClientAreaHeight (Value: Integer);
    procedure SetClientAreaSize (AWidth, AHeight: Integer);
  protected
    procedure GetBarSize (var ASize: Integer; const DockType: TDockType); override;
    procedure GetMinimumSize (var AClientWidth, AClientHeight: Integer); override;
    function OrderControls (CanMoveControls: Boolean; PreviousDockType: TDockType;
      DockingTo: TDock97): TPoint; override;
    procedure SizeChanging (const AWidth, AHeight: Integer); override;
  public
    constructor Create (AOwner: TComponent); override;

    procedure ReadPositionData (const ReadIntProc: TPositionReadIntProc;
      const ReadStringProc: TPositionReadStringProc; const ExtraData: Pointer); override;
    procedure WritePositionData (const WriteIntProc: TPositionWriteIntProc;
      const WriteStringProc: TPositionWriteStringProc; const ExtraData: Pointer); override;
  published
    property ActivateParent;
    property BorderStyle;
    property Caption;
    property Color;
    property CloseButton;
    property CloseButtonWhenDocked;
    property ClientAreaHeight: Integer read GetClientAreaHeight write SetClientAreaHeight;
    property ClientAreaWidth: Integer read GetClientAreaWidth write SetClientAreaWidth;
    property DefaultDock;
    property DockableTo;
    property DockedTo;
    property DockMode;
    property DockPos;
    property DockRow;
    property DragHandleStyle;
    property FloatingMode;
    property Font;
    property FullSize;
    property HideWhenInactive;
    property LastDock;
    property MinClientHeight: Integer read FMinClientHeight write FMinClientHeight default 32;
    property MinClientWidth: Integer read FMinClientWidth write FMinClientWidth default 32;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Resizable;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property UseLastDock;
    property Version;
    property Visible;

    property OnClose;
    property OnCloseQuery;
    property OnDragDrop;
    property OnDragOver;
    property OnDockChanged;
    property OnDockChanging;
    property OnDockChangingEx;
    property OnDockChangingHidden;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMove;
    property OnRecreated;
    property OnRecreating;
    property OnResize;
    property OnVisibleChanged;
  end;

implementation

const
  { Constants for TToolWindow97-specific registry values. Do not localize! }
  rvClientWidth = 'ClientWidth';
  rvClientHeight = 'ClientHeight';


{ TToolWindow97 }

constructor TToolWindow97.Create (AOwner: TComponent);
begin
  inherited;
  FMinClientWidth := 32;
  FMinClientHeight := 32;
  { Initialize the client size to 32x32 }
  SetBounds (Left, Top, 32, 32);
end;

procedure TToolWindow97.ReadPositionData (const ReadIntProc: TPositionReadIntProc;
  const ReadStringProc: TPositionReadStringProc; const ExtraData: Pointer);
begin
  inherited;
  { Restore ClientAreaWidth/ClientAreaHeight variables }
  if Resizable then
    SetClientAreaSize (ReadIntProc(Name, rvClientWidth, FBarWidth, ExtraData),
      ReadIntProc(Name, rvClientHeight, FBarHeight, ExtraData));
end;

procedure TToolWindow97.WritePositionData (const WriteIntProc: TPositionWriteIntProc;
  const WriteStringProc: TPositionWriteStringProc; const ExtraData: Pointer);
begin
  inherited;
  { Write values of FBarWidth/FBarHeight }
  WriteIntProc (Name, rvClientWidth, FBarWidth, ExtraData);
  WriteIntProc (Name, rvClientHeight, FBarHeight, ExtraData);
end;

procedure TToolWindow97.GetMinimumSize (var AClientWidth, AClientHeight: Integer);
begin
  AClientWidth := FMinClientWidth;
  AClientHeight := FMinClientHeight;
end;

procedure TToolWindow97.SizeChanging (const AWidth, AHeight: Integer);
begin
  FBarWidth := AWidth;
  if Parent <> nil then Dec (FBarWidth, Width - ClientWidth);
  FBarHeight := AHeight;
  if Parent <> nil then Dec (FBarHeight, Height - ClientHeight);
end;

procedure TToolWindow97.GetBarSize (var ASize: Integer; const DockType: TDockType);
begin
  if DockType <> dtLeftRight then
    ASize := FBarHeight
  else
    ASize := FBarWidth;
end;

function TToolWindow97.OrderControls (CanMoveControls: Boolean;
  PreviousDockType: TDockType; DockingTo: TDock97): TPoint;
begin
  Result.X := FBarWidth;
  Result.Y := FBarHeight;
end;

function TToolWindow97.GetClientAreaWidth: Integer;
begin
  if Parent = nil then
    Result := Width
  else
    Result := ClientWidth;
end;

procedure TToolWindow97.SetClientAreaWidth (Value: Integer);
begin
  SetClientAreaSize (Value, ClientAreaHeight);
end;

function TToolWindow97.GetClientAreaHeight: Integer;
begin
  if Parent = nil then
    Result := Height
  else
    Result := ClientHeight;
end;

procedure TToolWindow97.SetClientAreaHeight (Value: Integer);
begin
  SetClientAreaSize (ClientAreaWidth, Value);
end;

procedure TToolWindow97.SetClientAreaSize (AWidth, AHeight: Integer);
var
  Client: TRect;
begin
  if Parent = nil then
    SetBounds (Left, Top, AWidth, AHeight)
  else begin
    Client := GetClientRect;
    SetBounds (Left, Top, Width - Client.Right + AWidth,
      Height - Client.Bottom + AHeight);
  end;
end;

end.
