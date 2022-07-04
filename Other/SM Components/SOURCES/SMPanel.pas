{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29
}
unit SMPanel;

interface

uses
  Classes, Windows, Messages, Controls, Graphics, ExtCtrls;

type
  TSMGrabberPlace = (gpTop, gpLeft, gpBottom, gpRight);

  TSMFramePanel = class(TPanel)
  private
    FIcon: TIcon;
    FGrabberSize: Integer;
    FGrabberPlace: TSMGrabberPlace;
    FMoveable: Boolean;

    FOnClose: TNotifyEvent;
    FOnCaptionClick: TNotifyEvent;

    IsCloseClicked, IsCaptionClicked: Boolean;

    procedure SetGrabberSize(Value: Integer);
    procedure SetGrabberPlace(Value: TSMGrabberPlace);
    procedure SetIcon(Value: TIcon);

    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    property Canvas;
  published
    property GrabberSize: Integer read FGrabberSize write SetGrabberSize default 12;
    property GrabberPlace: TSMGrabberPlace read FGrabberPlace write SetGrabberPlace default gpTop;
    property Moveable: Boolean read FMoveable write FMoveable default False;
    property Icon: TIcon read FIcon write SetIcon;

    property OnCaptionClick: TNotifyEvent read FOnCaptionClick write FOnCaptionClick;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMFramePanel]);
end;

constructor TSMFramePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGrabberSize := 12;
  FGrabberPlace := gpTop;
  FMoveable := False;
  BevelOuter := bvNone;
  FIcon := TIcon.Create;
end;

destructor TSMFramePanel.Destroy;
begin
  FIcon.Free;

  inherited Destroy;
end;

procedure TSMFramePanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  case GrabberPlace of
    gpLeft,
    gpRight: Inc(Rect.Left, 10);

    gpTop,
    gpBottom: Inc(Rect.Top, 10);
  end;

  InflateRect(Rect, -1, -1);
  if Ctl3d then
    InflateRect(Rect, -1, -1);

  inherited AlignControls(AControl, Rect)
end;

procedure TSMFramePanel.SetGrabberSize(Value: Integer);
begin
  if (Value <> FGrabberSize) then
  begin
    FGrabberSize := Value;

    Invalidate;
  end;
end;

procedure TSMFramePanel.SetGrabberPlace(Value: TSMGrabberPlace);
begin
  if (Value <> FGrabberPlace) then
  begin
    FGrabberPlace := Value;

    Invalidate;
  end;
end;

procedure TSMFramePanel.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
  Invalidate;
end;

procedure TSMFramePanel.Paint;

  procedure DrawCloseButton(Left, Top: Integer);
  begin
    DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left+FGrabberSize-2,
      Top+FGrabberSize-2), DFC_CAPTION, DFCS_CAPTIONCLOSE);
  end;

  procedure DrawGrabberLine(Left, Top, Right, Bottom: Integer);
  begin
    with Canvas do
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(Right, Top);
      LineTo(Left, Top);
      LineTo(Left, Bottom);
      Pen.Color := clBtnShadow;
      LineTo(Right, Bottom);
      LineTo(Right, Top-1);
    end;
  end;

var
  ARect: TRect;
begin
  inherited Paint;

  ARect := ClientRect;

  with ARect do
    case GrabberPlace of
      gpLeft,
      gpRight: begin
                 if not FIcon.Empty then
                 begin
                   Bottom := Bottom - GrabberSize - 2;
                   DrawIconEx(Canvas.Handle, 0, Bottom+1, Icon.Handle, GrabberSize, GrabberSize, 0, 0, {DI_DEFAULTSIZE or} DI_NORMAL);
                 end;
                 DrawCloseButton(Left+1, Top+1);
                 DrawGrabberLine(Left+3, Top+GrabberSize+1, Left+5, Bottom-2);
                 DrawGrabberLine(Left+6, Top+GrabberSize+1, Left+8, Bottom-2);
               end;
      gpTop,
      gpBottom: begin
                  if not FIcon.Empty then
                  begin
                    Left := Left + GrabberSize + 1;
                    DrawIconEx(Canvas.Handle, 0, 0, Icon.Handle, GrabberSize, GrabberSize, 0, 0, {DI_DEFAULTSIZE or} DI_NORMAL);
                  end;

                  DrawCloseButton(Right-GrabberSize+1, Top+1);
                  DrawGrabberLine(Left+2, Top+3, Right-GrabberSize-2, Top+5);
                  DrawGrabberLine(Left+2, Top+6, Right-GrabberSize-2, Top+8);
                end
    end;
end;

procedure TSMFramePanel.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;

  IsCloseClicked := False;
  IsCaptionClicked := False;
  case GrabberPlace of
    gpLeft,
    gpRight: begin
               if (Message.YPos < 15) then
                 IsCloseClicked := True;

               if (Message.XPos < 15) then
                 IsCaptionClicked := True;
             end;
    gpTop,
    gpBottom: begin
                if (Message.XPos > Width - 15) then
                  IsCloseClicked := True;

                if (Message.YPos < 15) then
                  IsCaptionClicked := True;
              end;
  end;

  ReleaseCapture;
  if IsCaptionClicked and not IsCloseClicked then
  begin
    if Assigned(OnCaptionClick) then
      OnCaptionClick(Self);

    if Moveable then
      Perform(WM_SysCommand, $F012, 0)
  end
end;

procedure TSMFramePanel.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;

  if IsCloseClicked and IsCaptionClicked then
  begin
    if Assigned(OnClose) then
      OnClose(Self);

    Visible := False;
  end;

  IsCloseClicked := False;
  IsCaptionClicked := False;
end;

end.
