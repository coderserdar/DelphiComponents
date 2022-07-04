{ Copyright (C) 1998-2008, written by Shkolnik Mike
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  This components allows to create a
  TButton, TCheckBox and TRadioButton with multi-line captions.
  To use drop a component on form and set a WordWrap property.
}
unit SMultiBtn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TSMButton = class(TButton)
  private
    FWordWrap: Boolean;
    FLayout: TTextLayout;
    FAlignment: TAlignment;

    procedure SetWordWrap(Value: Boolean);
    procedure SetLayout(Value: TTextLayout);
    procedure SetAlignment(Value: TAlignment);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
  end;

  TSMCheckBox = class(TCheckBox)
  private
    FTransparent: Boolean;
    FWordWrap: Boolean;

    FLayout: TTextLayout;
    FAlignment: TAlignment;

    procedure SetLayout(Value: TTextLayout);
    procedure SetAlignment(Value: TAlignment);

    procedure SetTransparent(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);

    procedure DrawCheckBox(R: TRect; AState: TCheckBoxState; al: TAlignment);

    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;

    procedure Invalidate; override;
  published
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
  end;

  TSMRadioButton = class(TRadioButton)
  private
    FWordWrap: Boolean;
    FLayout: TTextLayout;
    FAlignment: TAlignment;

    procedure SetWordWrap(Value: Boolean);
    procedure SetLayout(Value: TTextLayout);
    procedure SetAlignment(Value: TAlignment);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
  end;

procedure Register;

implementation

{$R SMultiBtn.Res}

var
  FCheckWidth, FCheckHeight: Integer;

procedure GetCheckBoxSize;
begin
  with TBitmap.Create do
    try
      Handle := LoadBitmap(0, PChar(32759));
      FCheckWidth := Width div 4;
      FCheckHeight := Height div 3;
    finally
      Free;
    end;
end;

{ TSMButton }
constructor TSMButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAlignment := taCenter;
  FLayout := tlCenter;
  FWordWrap := True;
end;

procedure TSMButton.SetWordWrap(Value: Boolean);
begin
  if (FWordWrap <> Value) then
  begin
    FWordWrap := Value;
    ReCreateWnd;
  end;
end;

procedure TSMButton.SetLayout(Value: TTextLayout);
begin
  if (FLayout <> Value) then
  begin
    FLayout := Value;
    ReCreateWnd;
  end;
end;

procedure TSMButton.SetAlignment(Value: TAlignment);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    ReCreateWnd;
  end;
end;

procedure TSMButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  case Alignment of
    taLeftJustify: Params.Style := Params.Style or BS_LEFT;
    taRightJustify: Params.Style := Params.Style or BS_RIGHT;
    taCenter: Params.Style := Params.Style or BS_CENTER;
  end;

  case Layout of
    tlTop: Params.Style := Params.Style or BS_TOP;
    tlCenter: Params.Style := Params.Style or BS_VCENTER;
    tlBottom: Params.Style := Params.Style or BS_BOTTOM;
  end;

  if FWordWrap then
    Params.Style := Params.Style or BS_MULTILINE
  else
    Params.Style := Params.Style and not BS_MULTILINE;
end;

{ TSMCheckBox }
constructor TSMCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAlignment := taCenter;
  FLayout := tlCenter;
  FTransparent := False;
  FWordWrap := True;
end;

procedure TSMCheckBox.SetLayout(Value: TTextLayout);
begin
  if (FLayout <> Value) then
  begin
    FLayout := Value;
    ReCreateWnd;
  end;
end;

procedure TSMCheckBox.SetAlignment(Value: TAlignment);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    ReCreateWnd;
  end;
end;

procedure TSMCheckBox.SetTransparent(Value: Boolean);
begin
  if (FTransparent <> Value) then
  begin
    FTransparent := Value;

    if FTransparent then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    ReCreateWnd;
  end;
end;

procedure TSMCheckBox.SetWordWrap(Value: Boolean);
begin
  if (FWordWrap <> Value) then
  begin
    FWordWrap := Value;
    ReCreateWnd;
  end;
end;

procedure TSMCheckBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  case Alignment of
    taLeftJustify: Params.Style := Params.Style or BS_LEFT;
    taRightJustify: Params.Style := Params.Style or BS_RIGHT;
    taCenter: Params.Style := Params.Style or BS_CENTER;
  end;

  case Layout of
    tlTop: Params.Style := Params.Style or BS_TOP;
    tlCenter: Params.Style := Params.Style or BS_VCENTER;
    tlBottom: Params.Style := Params.Style or BS_BOTTOM;
  end;

  if FWordWrap then
    Params.Style := Params.Style or BS_MULTILINE or BS_TOP
  else
    Params.Style := Params.Style and not BS_MULTILINE or not BS_TOP;

  if Transparent then
     Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT {or not WS_CLIPCHILDREN or not WS_CLIPSIBLINGS};
end;

procedure TSMCheckBox.CreateWnd;
begin
  inherited CreateWnd;

  if Transparent and
     Assigned(Parent) and
     Parent.HandleAllocated then
  begin
    SetWindowLong(Parent.Handle, GWL_STYLE,
                  GetWindowLong(Parent.Handle, GWL_STYLE) and not WS_CLIPCHILDREN);
  end;
end;

procedure TSMCheckBox.WndProc(var Message: TMessage);
//var
//  Rect: TRect;
begin
  with Message do
    case Msg of
      WM_ERASEBKGND,
      WM_Move: begin
                 if Transparent then
                 begin
//                   Invalidate;
                   Message.Result := 1;

                   exit;
                 end
               end;
      CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
        begin
//          SetTextColor(WParam, RGB(0, 0, 255));
          SetBkMode(WParam, Windows.TRANSPARENT);
{
          GetClipBox(WParam, Rect);
          OffsetRect(Rect, Left, Top);
          RedrawWindow(Parent.Handle, @Rect, 0, RDW_ERASE or RDW_INVALIDATE or RDW_NOCHILDREN or RDW_UPDATENOW);
}
          Result := Parent.Brush.Handle;
          Exit;
        end;
    end;
  inherited WndProc(Message);
end;


procedure TSMCheckBox.DrawCheckBox(R: TRect; AState: TCheckBoxState; al: TAlignment);
var
  DrawState: Integer;
  DrawRect: TRect;
begin
  case AState of
    cbChecked: DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED;
    cbUnchecked: DrawState := DFCS_BUTTONCHECK;
    else // cbGrayed
      DrawState := DFCS_BUTTON3STATE or DFCS_CHECKED;
  end;
  case al of
    taRightJustify: begin
                      DrawRect.Left := R.Right - FCheckWidth;
                      DrawRect.Right := R.Right;
                    end;
    taCenter: begin
                DrawRect.Left := R.Left + (R.Right - R.Left - FCheckWidth) div 2;
                DrawRect.Right := DrawRect.Left + FCheckWidth;
              end;
  else // taLeftJustify
    DrawRect.Left := R.Left;
    DrawRect.Right := DrawRect.Left + FCheckWidth;
  end;
  DrawRect.Top := R.Top + (R.Bottom - R.Top - FCheckWidth) div 2;
  DrawRect.Bottom := DrawRect.Top + FCheckHeight;

  DrawFrameControl({Canvas.}Handle, DrawRect, DFC_BUTTON, DrawState);
end;

procedure TSMCheckBox.WMPaint(var Message: TWMPaint);
var
  Rect: TRect;
begin
inherited;
exit;


  if Transparent and
     Assigned(Parent) and
     Parent.HandleAllocated then
  begin
    PaintHandler(Message);

    GetClipBox({Canvas.}Handle, Rect);
//    OffsetRect(Rect, Left, Top);
//    RedrawWindow(Parent.Handle, @Rect, 0, RDW_ERASE or RDW_INVALIDATE or RDW_NOCHILDREN or RDW_UPDATENOW)
    DrawCheckBox(Rect, State, Alignment);
  end
  else
    PaintHandler(Message);
end;

procedure TSMCheckBox.Invalidate;
var
  Rect: TRect;
//  i: Integer;
  DC: HDC;
begin
inherited;
exit;

  if Transparent and
     Assigned(Parent) and
     Parent.HandleAllocated then
  begin
    DC := GetDC(Handle);
    SetBkMode(DC, Windows.TRANSPARENT);

    GetClipBox(DC {Canvas.Handle}, Rect);
    OffsetRect(Rect, Left, Top);
    RedrawWindow(Parent.Handle, @Rect, 0, RDW_ERASE or RDW_INVALIDATE or RDW_NOCHILDREN or RDW_UPDATENOW);

    ReleaseDC(Handle, DC);

{    Rect := BoundsRect;
    InvalidateRect(Parent.Handle, @Rect, True);
    for i := 0 to ControlCount - 1 do
      Controls[i].Invalidate;
}  end
  else
    inherited Invalidate;
end;

{ TSMRadioButton }
constructor TSMRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAlignment := taCenter;
  FLayout := tlCenter;
  FWordWrap := True;
end;

procedure TSMRadioButton.SetLayout(Value: TTextLayout);
begin
  if (FLayout <> Value) then
  begin
    FLayout := Value;
    ReCreateWnd;
  end;
end;

procedure TSMRadioButton.SetAlignment(Value: TAlignment);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    ReCreateWnd;
  end;
end;

procedure TSMRadioButton.SetWordWrap(Value: Boolean);
begin
  if (FWordWrap <> Value) then
  begin
    FWordWrap := Value;
    ReCreateWnd;
  end;
end;

procedure TSMRadioButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  case Alignment of
    taLeftJustify: Params.Style := Params.Style or BS_LEFT;
    taRightJustify: Params.Style := Params.Style or BS_RIGHT;
    taCenter: Params.Style := Params.Style or BS_CENTER;
  end;

  case Layout of
    tlTop: Params.Style := Params.Style or BS_TOP;
    tlCenter: Params.Style := Params.Style or BS_VCENTER;
    tlBottom: Params.Style := Params.Style or BS_BOTTOM;
  end;

  if FWordWrap then
    Params.Style := Params.Style or BS_MULTILINE or BS_TOP
  else
    Params.Style := Params.Style and not BS_MULTILINE or not BS_TOP;
end;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMButton, TSMCheckBox, TSMRadioButton]);
end;

initialization
  GetCheckBoxSize;

end.
