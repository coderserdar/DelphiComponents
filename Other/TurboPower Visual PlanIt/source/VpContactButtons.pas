{*********************************************************}
{*              VPCONTACTBUTTONS.PAS 1.03                *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*        Steve Forbes                                                        *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{ TurboPower Software Company wishes to thank Steve Forbes for }
{ providing this component, and allowing us to include it.     }
{ Thanks Steve!                                                }

{$I Vp.INC}

unit VpContactButtons;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VpBase, VpContactGrid, VpMisc;

const
        VP_MIN_BUTTONS = 2;
  VP_LETTERS_IN_ALPHABET = 26;
  VP_MAX_BUTTONS = VP_LETTERS_IN_ALPHABET + 1;
        VP_LETTER_A = Ord('a');

type
  TVpButtonRec = packed record
    Rect: TRect;
    Caption: String;
  end;

  TVpButtonArray = array[0..VP_MAX_BUTTONS - 1] of TVpButtonRec;

  TVpButtonBarOrientation = (baHorizontal, baVertical);

  TVpButtonBarClickEvent = procedure(Sender: TObject; ButtonIndex: Integer;
    SearchString: String) of object;

  TVpContactButtonBar = class(TVPCustomControl)
  protected {private}
    FBarOrientation: TVpButtonBarOrientation;
    FBorderWidth: Integer;
    FButtonPressed: Integer;
    FButtonColor: TColor;
    FButtonCount: Integer;
    FButtonHeight: Integer;
    FButtonsArray: TVpButtonArray;
    FButtonWidth: Integer;
    FContactGrid: TVpContactGrid;
    FDrawingStyle: TVpDrawingStyle;
    FOnButtonClick: TVpButtonBarClickEvent;
    FShowNumberButton: Boolean;
    FRadioStyle: Boolean;
    {internal variables}
    bbSearchString: string;

    {internal methods}
    procedure bbPopulateSearchString;
    procedure CreateButtons;
    procedure DrawButton(Index: Integer; Pressed: Boolean);
    procedure SelectContact;

    { Property setter methods }
    procedure SetBarOrientation(const Value: TVpButtonBarOrientation);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetButtonColor(const Value: TColor);
    procedure SetButtonHeight(const Value: Integer);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetContactGrid(Value: TVpContactGrid);
    procedure SetDrawingStyle(const Value: TVpDrawingStyle);
    procedure SetShowNumberButton(const Value: Boolean);

    { Overridden methods }
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BarOrientation: TVpButtonBarOrientation
      read FBarOrientation write SetBarOrientation default baVertical;
    property BorderWidth: Integer
      read FBorderWidth write SetBorderWidth default 2;
    property ButtonColor: TColor
      read FButtonColor write SetButtonColor default clBtnFace;
    property ButtonHeight: Integer
      read FButtonHeight write SetButtonHeight default 18;
    property ButtonWidth: Integer
      read FButtonWidth write SetButtonWidth default 34;
    property ContactGrid: TVpContactGrid
      read FContactGrid write SetContactGrid;
    property DrawingStyle: TVpDrawingStyle
      read FDrawingStyle write SetDrawingStyle default ds3d;
    property ShowNumberButton: Boolean
      read FShowNumberButton write SetShowNumberButton default True;
    property OnButtonClick: TVpButtonBarClickEvent
      read FOnButtonClick write FOnButtonClick;
    property RadioStyle: Boolean
      read FRadioStyle write FRadioStyle;

    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    {events}
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation
{ TVpContactButtonBar }

constructor TVpContactButtonBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 40;
  Height := 280;

  {$IFDEF VERSION4}
  DoubleBuffered := True;
  {$ENDIF}

  FBarOrientation := baVertical;
  FBorderWidth := 2;
  FButtonColor := clBtnFace;
  FButtonHeight := 18;
  FButtonWidth := 34;
  FDrawingStyle := ds3d;
  FShowNumberButton := True;
end;
{=====}

destructor TVpContactButtonBar.Destroy;
begin
  inherited Destroy;
end;
{=====}

procedure TVpContactButtonBar.CreateButtons;
var
  I: Integer;
  TotalXY: Integer;
  StartLetter, EndLetter: Char;
  ButtonLetters: Single;
  ButtonCaption: String;
  Offset: Integer;
  MaxButtons: Integer;
        MinButtons: Integer;
begin
  I := 0;

  if FShowNumberButton then begin
    MaxButtons := VP_MAX_BUTTONS;
                MinButtons := VP_MIN_BUTTONS + 1;
        end else begin
    MaxButtons := VP_LETTERS_IN_ALPHABET;
                MinButtons := VP_MIN_BUTTONS;
        end;

  if FBarOrientation = baVertical then begin
    TotalXY := FBorderWidth;

    while ((TotalXY + FButtonHeight + FBorderWidth < ClientHeight)
           and (I < MaxButtons))
    or (I < MinButtons)
    do begin
      FButtonsArray[I].Rect := Rect(FBorderWidth, TotalXY,
        ClientWidth - FBorderWidth, TotalXY + FButtonHeight);
      Inc(I);
      TotalXY := TotalXY + FButtonHeight + FBorderWidth;
    end;

    FButtonCount := I;
  end else begin
    TotalXY := FBorderWidth;

    while ((TotalXY + FButtonWidth + FBorderWidth < ClientWidth)
           and (I < MaxButtons))
    or (I < MinButtons)
    do begin
      FButtonsArray[i].Rect := Rect(TotalXY, FBorderWidth,
        TotalXY + FButtonWidth, ClientHeight - FBorderWidth);
      Inc(I);
      TotalXY := TotalXY + FButtonWidth + FBorderWidth;
    end;

    FButtonCount := I;
  end;

  Offset := 0;

  if FShowNumberButton then begin
    ButtonLetters := VP_LETTERS_IN_ALPHABET / (FButtonCount - 1);
    FButtonsArray[0].Caption := '123';
    Offset := 1;
  end else
    ButtonLetters := VP_LETTERS_IN_ALPHABET / FButtonCount;

  for i := 0 to FButtonCount - Offset - 1 do begin
    StartLetter := Chr(Round(VP_LETTER_A + ButtonLetters * I));
    EndLetter := Chr(Round(VP_LETTER_A + ButtonLetters * (I + 1)) - 1);

    if Ord(EndLetter) = Ord(StartLetter) then
      ButtonCaption := StartLetter
    else begin
      if Ord(EndLetter) = Ord(StartLetter) + 1 then
        ButtonCaption := StartLetter + EndLetter
      else begin
        if Ord(EndLetter) = Ord(StartLetter) + 2 then
          ButtonCaption := StartLetter + Succ(StartLetter) + EndLetter
        else
          ButtonCaption := StartLetter + '-' + EndLetter;
      end;
    end;

    FButtonsArray[I + Offset].Caption := ButtonCaption;
  end;
end;
{=====}

procedure TVpContactButtonBar.DrawButton(Index: Integer; Pressed: Boolean);
var
  ButtonRect: TRect;
begin
  with Canvas do begin
    Font := Self.Font;
    ButtonRect := FButtonsArray[Index].Rect;
    Brush.Color := FButtonColor;
    FillRect(ButtonRect);
    if (FDrawingStyle = dsFlat) then begin

      if Pressed then
        Pen.Color := clBtnShadow
      else
        Pen.Color := clBtnHighlight;

      PolyLine([Point(ButtonRect.Right - 1, ButtonRect.Top),
        Point(ButtonRect.Left, ButtonRect.Top),
        Point(ButtonRect.Left, ButtonRect.Bottom - 1)]);

      if Pressed then
        Pen.Color := clBtnHighlight
      else
        Pen.Color := clBtnShadow;

      PolyLine([Point(ButtonRect.Left, ButtonRect.Bottom - 1),
        Point(ButtonRect.Right - 1, ButtonRect.Bottom - 1),
        Point(ButtonRect.Right - 1, ButtonRect.Top)]);

      InflateRect(ButtonRect, -2, -2);
    end else begin
      if Pressed then
        DrawFrameControl(Handle, ButtonRect,
          DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED)
      else
        DrawFrameControl(Handle, ButtonRect, DFC_BUTTON, DFCS_BUTTONPUSH);

      InflateRect(ButtonRect, -2, -2);
      FillRect(ButtonRect);
    end;

    if Pressed then begin
      ButtonRect.Left := ButtonRect.Left + 2;
      ButtonRect.Top := ButtonRect.Top + 2;
    end;

    DrawText(Handle, PChar(FButtonsArray[Index].Caption),
      Length(FButtonsArray[Index].Caption), ButtonRect,
      DrawTextBiDiModeFlagsReadingOnly or DT_SINGLELINE or DT_CENTER
      or DT_VCENTER);
  end;
end;
{=====}

procedure TVpContactButtonBar.SelectContact;
var
  I: Integer;
begin
  if FContactGrid <> nil then begin
    FContactGrid.SetFocus;
    for I := 1 to Length(bbSearchString) do
      if FContactGrid.SelectContactByName(bbSearchString[I]) then
        Break;
  end;
end;
{=====}

procedure TVpContactButtonBar.Loaded;
begin
  inherited Loaded;
  CreateButtons;
end;
{=====}

procedure TVpContactButtonBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  P: TPoint;
  R: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then begin
    P := Point(X, Y);
    for I := 0 to pred(FButtonCount) do begin
      R := FButtonsArray[I].Rect;
      if PointInRect(P, R) then begin
        { if RadioStyle then un-press the last clicked button. }
        if RadioStyle then
          DrawButton(FButtonPressed, False);
        FButtonPressed := I;
        bbPopulateSearchString;
        DrawButton(I, True);
        Break;
      end;
    end;

    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self, FButtonPressed, bbSearchString)
    else
      SelectContact;
  end;
end;
{=====}

procedure TVpContactButtonBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if not RadioStyle then
    DrawButton(FButtonPressed, False);
end;
{=====}

procedure TVpContactButtonBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FContactGrid) and (Operation = opRemove) then
    FContactGrid := nil;
end;
{=====}

procedure TVpContactButtonBar.Paint;
var
  I: Integer;
begin
  for I := 0 to FButtonCount - 1 do begin
    if RadioStyle and (FButtonPressed = I) then
      DrawButton(I, True)
    else
      DrawButton(I, False);
  end;
end;
{=====}

procedure TVpContactButtonBar.Resize;
begin
  inherited Resize;
  CreateButtons;
end;
{=====}

procedure TVpContactButtonBar.SetDrawingStyle(const Value: TVpDrawingStyle);
begin
  if FDrawingStyle <> Value then begin
    FDrawingStyle := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpContactButtonBar.SetContactGrid(Value: TVpContactGrid);
begin
  if (FContactGrid <> Value) then begin
    FContactGrid := Value;
    if FContactGrid <> nil then begin
      Height := FContactGrid.Height;
      Repaint;
    end;
  end;
end;
{=====}

procedure TVpContactButtonBar.bbPopulateSearchString;
var
  BC: string; // button caption
  I: integer;
begin
  bc := FButtonsArray[FButtonPressed].Caption;
  if FButtonPressed = 0 then
    bbSearchString := '0123456789'
  else if (pos('-', BC) > 0) then begin
    bbSearchString := '';
    for I := ord(BC[1]) to ord(BC[Length(BC)]) do
      bbSearchString := bbSearchString + chr(I);
  end else
    bbSearchString := FButtonsArray[FButtonPressed].Caption;
end;
{=====}

procedure TVpContactButtonBar.SetBarOrientation(const Value: TVpButtonBarOrientation);
begin
  if (FBarOrientation <> Value) then begin
    FBarOrientation := Value;
    CreateButtons;
    Repaint;
  end;
end;
{=====}

procedure TVpContactButtonBar.SetBorderWidth(const Value: Integer);
begin
  if (FBorderWidth <> Value) then begin
    FBorderWidth := Value;
    if FBorderWidth < 0 then
      FBorderWidth := 0;
    CreateButtons;
    Repaint;
  end;
end;
{=====}

procedure TVpContactButtonBar.SetButtonColor(const Value: TColor);
begin
  if (FButtonColor <> Value) then begin
    FButtonColor := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpContactButtonBar.SetButtonHeight(const Value: Integer);
begin
  if (FButtonHeight <> Value) then begin
    FButtonHeight := Value;
    if FButtonHeight < 18 then
      FButtonHeight := 18;
    CreateButtons;
    Repaint;
  end;
end;
{=====}

procedure TVpContactButtonBar.SetButtonWidth(const Value: Integer);
begin
  if (FButtonWidth <> Value) then begin
    FButtonWidth := Value;
    if FButtonWidth < 34 then
      FButtonWidth := 34;
    CreateButtons;
    Repaint;
  end;
end;
{=====}

procedure TVpContactButtonBar.SetShowNumberButton(const Value: Boolean);
begin
  if (FShowNumberButton <> Value) then begin
    FShowNumberButton := Value;
    CreateButtons;
    Repaint;
  end;
end;
{=====}

end.
