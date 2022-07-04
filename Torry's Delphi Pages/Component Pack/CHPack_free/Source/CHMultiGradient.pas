unit CHMultiGradient;

{ ##############################################################################
  TCHMultiGradient

  Version   		:   1.0.3
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 21.07.2002    - First Release
  1.0.1 - 14.10.2002    - ADD: Popupmenu
  1.0.2 - 15.12.2002    - BUG: repair some memory leaks
  1.0.3 - 09.03.2003    - reorganize "uses" for more performance and less memory needed

  ############################################################################ }

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics,
  _CHClassProperty, _CHClassFunction;

type
  TCHMultiGradient = class;

  TCHMultiGradient = class(TGraphicControl)
  private
    FGradient : TCHGradient;
    FBuffered: Boolean;
    FBufferBitmap : TBitmap;
    FOnMouseEnter : TNotifyEvent;
    FOnMouseLeave : TNotifyEvent;

    procedure SetBuffered(Value: Boolean);

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_EraseBkgnd;
    procedure UpdateChanges(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property Align;
    property Anchors;
    property Buffered : Boolean read FBuffered write SetBuffered;
    property Enabled;
    property Gradient : TCHGradient read FGradient write FGradient;
    property Visible;
    property PopupMenu;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHMultiGradient]);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHMultiGradient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGradient := TCHGradient.Create;
  FGradient.OnChange := UpdateChanges;

  FBufferBitmap := TBitmap.Create;
  FBuffered := False;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHMultiGradient.Destroy;
begin
  FBufferBitmap.Free;
  FGradient.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMultiGradient.UpdateChanges(Sender: TObject);
begin
  if csLoading in ComponentState then
  begin
    Exit;
  end;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMultiGradient.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMultiGradient.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMultiGradient.Paint;
var
  nActive : Integer;
  Rect : TRect;
  aColor : array of TColor;
begin
  inherited Paint;

  nActive := -1;
  Rect := ClientRect;


  if FGradient.Active0 = True then begin
    Inc(nActive);
    SetLength(aColor, nActive + 1);
    aColor[nActive] := FGradient.Color0;
  end;
  if FGradient.Active1 = True then begin
    Inc(nActive);
    SetLength(aColor, nActive + 1);
    aColor[nActive] := FGradient.Color1;
  end;
  if FGradient.Active2 = True then begin
    Inc(nActive);
    SetLength(aColor, nActive + 1);
    aColor[nActive] := FGradient.Color2;
  end;
  if FGradient.Active3 = True then begin
    Inc(nActive);
    SetLength(aColor, nActive + 1);
    aColor[nActive] := FGradient.Color3;
  end;
  if FGradient.Active4 = True then begin
    Inc(nActive);
    SetLength(aColor, nActive + 1);
    aColor[nActive] := FGradient.Color4;
  end;
  if FGradient.Active5 = True then begin
    Inc(nActive);
    SetLength(aColor, nActive + 1);
    aColor[nActive] := FGradient.Color5;
  end;
  if FGradient.Active6 = True then begin
    Inc(nActive);
    SetLength(aColor, nActive + 1);
    aColor[nActive] := FGradient.Color6;
  end;
  if FGradient.Active7 = True then begin
    Inc(nActive);
    SetLength(aColor, nActive + 1);
    aColor[nActive] := FGradient.Color7;
  end;
  if FGradient.Active8 = True then begin
    Inc(nActive);
    SetLength(aColor, nActive + 1);
    aColor[nActive] := FGradient.Color8;
  end;
  if FGradient.Active9 = True then begin
    Inc(nActive);
    SetLength(aColor, nActive + 1);
    aColor[nActive] := FGradient.Color9;
  end;

  if High(aColor) > 0 then
  begin
    if FBuffered then
    begin
      FBufferBitmap.Width := Self.Width;
      FBufferBitmap.Height := Self.Height;
      BitBlt(Self.Canvas.Handle, 0, 0, Self.Width, Self.Height, FBufferBitmap.Canvas.Handle, 0, 0, SRCCOPY);
      DrawGradient(FBufferBitmap.Canvas, Rect, aColor, FGradient.Style, FGradient.Rotation);
      BitBlt(Self.Canvas.Handle, 0, 0, Self.Width, Self.Height, FBufferBitmap.Canvas.Handle, 0, 0, SRCCOPY);
    end
    else
      DrawGradient(Self.Canvas, Rect, aColor, FGradient.Style, FGradient.Rotation);
  end
  // nur eine Farbe = True --> einfarbig
  else if High(aColor) = 0 then
  begin
    Canvas.Brush.Color := aColor[nActive];
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect);
  end
  // alle Farben = False --> ohne Füllung
  else
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.Brush.Style := bsClear;
    Canvas.FillRect(Rect);
  end;

end;



{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMultiGradient.SetBuffered(Value: Boolean);
begin
  if FBuffered <> Value then
  begin
    FBuffered := Value;
    try
      if (FBuffered = True) then
        Parent.DoubleBuffered := True
      else
        Parent.DoubleBuffered := False;
    except
      Abort;
    end;

    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMultiGradient.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := -1;
end;






end.
