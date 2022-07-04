
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Essentials Vol I
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ES.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
  {$G+} {286 Instructions}
  {$N+} {Numeric Coprocessor}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsTile;
  {-tile component}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Classes, Controls, Forms, Graphics, Messages,
  EsConst, EsData;

type
  TEsCustomTile = class(TGraphicControl)
  protected {private}
    {.Z+}
    {property variables}
    FBitmap : TBitmap;

    {internal methods}
    procedure tiBitmapChange(Sender : TObject);

    {property methods}
    function GetVersion : string;
    procedure SetBitmap(Value : TBitmap);
    procedure SetVersion(const Value : string);
    {.Z-}
  protected
    {.Z+}
    procedure Paint;
      override;
    {.Z-}

    property Bitmap : TBitmap
      read FBitmap
      write SetBitmap;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

  public
    {.Z+}
    constructor Create(AComponent : TComponent);
      override;
    destructor Destroy;
      override;
    procedure PaintTo(DC : TEsHdc; R : TRect);
    {.Z-}
  end;

  TEsTile = class(TEsCustomTile)
  published
    {$IFDEF VERSION4}                                                {!!.06}
    property Anchors;                                                {!!.06}
    property Constraints;                                            {!!.06}
    property DragKind;                                               {!!.06}
    {$ENDIF}                                                         {!!.06}
    property Align;
    property Bitmap;
    property Version;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF WIN32}
  end;


implementation


constructor TEsCustomTile.Create(AComponent : TComponent);
begin
  inherited Create(AComponent);

  ControlStyle := ControlStyle + [csOpaque];                           {!!.04}

  Width       := 100;
  Height      := 100;

  FBitmap := TBitmap.Create;
  FBitmap.OnChange := tiBitmapChange;

end;

destructor TEsCustomTile.Destroy;
begin
  FBitmap.Free;
  FBitmap := nil;

  inherited Destroy;
end;

function TEsCustomTile.GetVersion : string;
begin
  Result := EsVersionStr;
end;

procedure TEsCustomTile.Paint;
begin
  if not Assigned(FBitmap) or FBitmap.Empty then begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
    Exit;
  end;

  PaintTo(Canvas.Handle, ClientRect);
end;

procedure TEsCustomTile.PaintTo(DC : TEshDC; R : TRect);
var
  HTiles  : Integer;
  VTiles  : Integer;
  X, Y    : Integer;
  CW, CH  : Integer;
  TmpDC   : hDC;
  Bmp     : hBitmap;
  OldBmp  : hBitmap;
  OldPal  : hPalette;
  OldPal2 : hPalette;
begin
  if FBitmap.Handle > 0 then {force handle creation};

  {get the width and height}
  CW := R.Right-R.Left;
  CH := R.Bottom-R.Top;

  OldPal := 0;
  if FBitmap.Palette <> 0 then begin
    {if the bitmap has a palette, use it}
    OldPal := SelectPalette(DC, FBitmap.Palette, True);
    RealizePalette(DC);
  end;
  try
    {create a temporary device context}
    TmpDC := CreateCompatibleDC(DC);
    try
      {create a bitmap to draw on}
      Bmp := CreateCompatibleBitmap(DC, CW, CH);
      try
        {select the bitmap into the temporary DC}
        OldBmp := SelectObject(TmpDC, Bmp);
        try
          OldPal2 := 0;
          if FBitmap.Palette <> 0 then begin
            OldPal2 := SelectPalette(TmpDC, FBitmap.Palette, True);
            RealizePalette(TmpDC);
          end;
          try
            {compute needed tiles}
            HTiles := CW div FBitmap.Width;
            if CW mod FBitmap.Width <> 0 then
              Inc(HTiles);
            VTiles := CH div FBitmap.Height;
            if CH mod FBitmap.Height <> 0 then
              Inc(VTiles);

            {paint the tiles}
            for X := 0 to Pred(HTiles) do
              for Y := 0 to Pred(VTiles) do
                BitBlt(TmpDC, X*FBitmap.Width, Y*FBitmap.Height,
                  FBitmap.Width, FBitmap.Height, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);

            {copy temporary device context to ours}
            BitBlt(DC, 0, 0, CW, CH, TmpDC, 0, 0, SRCCOPY);
          finally
            if OldPal2 <> 0 then
              SelectPalette(TmpDC, OldPal2, True);
          end;
        finally
          SelectObject(TmpDC, OldBmp);
        end;
      finally
        DeleteObject(Bmp);
      end;
    finally
      DeleteDC(TmpDC);
    end;
  finally
    if OldPal > 0 then
      SelectPalette(DC, OldPal, True);
  end;
end;

procedure TEsCustomTile.SetBitmap(Value : TBitmap);
begin
  if Assigned(Value) then
    FBitmap.Assign(Value)
  else
    FBitmap.ReleaseHandle;
  Invalidate;
end;

procedure TEsCustomTile.SetVersion(const Value : string);
begin
end;

procedure TEsCustomTile.tiBitmapChange(Sender : TObject);
begin
  Invalidate;
end;

end.
