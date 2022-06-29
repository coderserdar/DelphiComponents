{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit DBUTypes;

interface

uses
  Classes,
{$ifndef LINUX}
  Windows, Controls, Messages, Graphics, Grids;
{$else LINUX}
  QControls, QGrids, QGraphics, Qt, Types;
{$endif LINUX}

type
  PEmptyItem = ^TEmptyItem;
  TEmptyItem = (eiNoEmpty, eiEmptyFirst, eiEmptyLast);

  TVerticalAlignment = (vaTop, vaMiddle, vaBottom);

  TDBUAlign = record
    V : TVerticalAlignment;
    H : TAlignment;
  end;

  TDBUMargin = record
    V : Integer;
    H : Integer;
  end;

  TDBUKeyComb = record
    Key: Word;
    Shift: TShiftState;
  end;

  PDBUKeyComb = ^TDBUKeyComb;

  TKeyState = record
    Key: Word;
    Shift: TShiftState;
    Col, Row: Integer;
  end;

  TMouseBtnStates = set of TMouseButton;

  TMouseState = record
    Button: TMouseButton;
    Shift: TShiftState;
    X, Y: Integer;
    Col, Row: Integer;
  end;

  TMouseMoveState = record
    Shift: TShiftState;
    X, Y: Integer;
    Col, Row: Integer;
  end;

  {/** To help the porting to Kylix we send TDrawEditorParams records
       as parameters to DrawEditor, since we then can specify an own
       format under Kylix, containing the information it needs.

       The windows parameters:
          DC : HDC;                     // Handle to use, comes from PaintWindow
          Col, Row: Longint;            // Col and Row indexes in the Grid for this cell
          Rect: TRect;                  // The rectangle that the editor lies in
          InhEvent : TPaintWindowEvent; // Pointer to the inherited Paint function usually called
         */}

  PDrawParams = ^TDrawParams;

  TDrawParams = record
    Canvas : TCanvas;               // The grids canvas
    Col, Row: Longint;              // Col and Row indexes in the Grid for this cell
    Rect: TRect;                    // The rectangle that the editor lies in
    DrawState: TGridDrawState;              // Is this cell fixed, focused or selected
//    RunInhEvent : Boolean;        // Should the inherited Paint function be called
//    InhEvent : TPaintWindowEvent; // Pointer to the inherited Paint function usually called
//    DC : THandle;                 // Handle to use, comes from PaintWindow
  end;

  TDBUCustomizer = class
  public
    procedure Reset; virtual;
  end;

  TDBUEditCustomizer = class( TDBUCustomizer )
  public
    procedure KillFocus; virtual;
  end;

  TWMType = ( CancelMode, KillFocus, AnyMessage );

  THandleWMParams = record
    WMType : TWMType;
    Message : TMessage;
    Col, Row: Longint;            // Col and Row indexes in the Grid for this cell
    RunInh : Boolean;             // Should we run the inherited method
  end;

  THandleCMParams = record
{$ifndef LINUX}
    Message : TCMCancelMode;
{$endif LINUX}
    Col, Row: Longint;            // Col and Row indexes in the Grid for this cell
  end;

//function DrawEditorParams(AGridCanvas : TCanvas; ADC : THandle; ACol, ARow: Longint; ARect: TRect; AInhEvent : TPaintWindowEvent) : TDrawEditorParams;
function DBUAlign( AV : TVerticalAlignment; AH : TAlignment ) : TDBUAlign;
function DBUMargin( AV, AH : Integer ) : TDBUMargin;
function DBUKeyComb( AKey: Word; AShift: TShiftState ) : TDBUKeyComb;
function DrawParams(ACanvas : TCanvas; ACol, ARow: Longint; ARect: TRect; ADrawState : TGridDrawState) : TDrawParams;
function KeyState( AKey : Word; AShift: TShiftState; ACol, ARow: Integer ) : TKeyState;
function MouseState(AButton: TMouseButton; AShift: TShiftState; AX, AY, ACol, ARow: Integer) : TMouseState;
function MouseMoveState(AShift: TShiftState; AX, AY, ACol, ARow: Integer) : TMouseMoveState;
function HandleWMParams( AWMType : TWMType; AMessage : TMessage; ACol, ARow: Integer ) : THandleWMParams;
{$ifndef LINUX}
function HandleCMParams( AMessage : TCMCancelMode; ACol, ARow: Integer ) : THandleCMParams;
{$else}
function HandleCMParams( ACol, ARow: Integer ) : THandleCMParams;
{$endif LINUX}

{$ifndef Linux}
const
  Key_Tab = VK_TAB;
  Key_Up = VK_UP;
  Key_Down = VK_DOWN;
  Key_Return = VK_RETURN;
  Key_Escape = VK_ESCAPE;
  Key_Left = VK_LEFT;
  Key_Right = VK_RIGHT;
  Key_Next = VK_NEXT;
  Key_Prior = VK_PRIOR;
  Key_Home = VK_HOME;
  Key_End = VK_END;
  Key_Space = VK_SPACE;
{  Key_0 = $30;
  Key_9 = $39;
  Key_A = $41;
  Key_Z = $5A;}

{$endif}
  
implementation

function DBUAlign( AV : TVerticalAlignment; AH : TAlignment ) : TDBUAlign;
begin
  with Result do
  begin
    V := AV;
    H := AH;
  end;
end;

function DBUMargin ( AV, AH : Integer ) : TDBUMargin;
begin
  with Result do
  begin
    V := AV;
    H := AH;
  end;
end;

function DBUKeyComb( AKey: Word; AShift: TShiftState ) : TDBUKeyComb;
begin
  with Result do
  begin
    Key := AKey;
    Shift := AShift;
  end;
end;

function DrawParams(ACanvas : TCanvas; ACol, ARow: Longint; ARect: TRect; ADrawState : TGridDrawState) : TDrawParams;
begin
  with Result do
  begin
    Canvas := ACanvas;
    Col := ACol;
    Row := ARow;
    Rect := ARect;
    DrawState := ADrawState;
  end;
end;

function KeyState( AKey : Word; AShift: TShiftState; ACol, ARow: Integer ) : TKeyState;
begin
  with Result do
  begin
    Key := AKey;
    Shift := AShift;
    Col := ACol;
    Row := ARow;
  end;
end;

function MouseState(AButton: TMouseButton; AShift: TShiftState; AX, AY, ACol, ARow: Integer) : TMouseState;
begin
  if (AX < 0) or (AY < 0) then
  begin
    ACol := -1;
    ARow := -1;
  end;

  with Result do
  begin
    Button := AButton;
    Shift := AShift;
    X := AX;
    Y := AY;
    Col := ACol;
    Row := ARow;
  end;
end;

function MouseMoveState(AShift: TShiftState; AX, AY, ACol, ARow: Integer) : TMouseMoveState;
begin
  if (AX < 0) or (AY < 0) then
  begin
    ACol := -1;
    ARow := -1;
  end;

  with Result do
  begin
    Shift := AShift;
    X := AX;
    Y := AY;
    Col := ACol;
    Row := ARow;
  end;
end;

function HandleWMParams( AWMType : TWMType; AMessage : TMessage; ACol, ARow: Integer ) : THandleWMParams;
begin
  with Result do
  begin
    WMType := AWMType;
    Message := AMessage;
    Col := ACol;
    Row := ARow;
    RunInh := True;
  end;
end;

{$ifndef Linux}
function HandleCMParams( AMessage : TCMCancelMode; ACol, ARow: Integer ) : THandleCMParams;
{$else}
function HandleCMParams( ACol, ARow: Integer ) : THandleCMParams;
{$endif Linux}
begin
  with Result do
  begin
{$ifndef Linux}
    Message := AMessage;
{$endif Linux}
    Col := ACol;
    Row := ARow;
  end;
end;

{ TDBUCustomizer }

procedure TDBUCustomizer.Reset;
begin
  // Nothing
end;

{ TDBUEditCustomizer }

procedure TDBUEditCustomizer.KillFocus;
begin
  // Nothing
end;

end.

