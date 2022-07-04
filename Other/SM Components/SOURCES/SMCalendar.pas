{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29
}
unit SMCalendar;

interface
uses Windows, Messages, Classes, Graphics, Grids, Calendar;

type
  TGetCellParams = procedure(Sender: TObject; ACol, ARow: Longint; AFont: TFont; var Background: TColor) of object;

  TSMCalendar = class(TCalendar)
  private
{    FCaption: string;
    FCaptionHeight: Integer;
}
    FOnDrawCell: TDrawCellEvent;
    FGetCellParams: TGetCellParams;

{    procedure SetCaption(Value: string);
    procedure SetCaptionHeight(Value: Integer);
}  protected
{    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateWnd; override;

    procedure Paint; override;
    function GetClientRect: TRect; override;

    procedure SetEditRect;
}
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  published
{    property Caption: string read FCaption write SetCaption;
    property CaptionHeight: Integer read FCaptionHeight write SetCaptionHeight default 21;
}
    property Options;

    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnGetCellParams: TGetCellParams read FGetCellParams write FGetCellParams;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMCalendar]);
end;

{ TSMCalendar }
{constructor TSMCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaptionHeight := 21;
end;

destructor TSMCalendar.Destroy;
begin
  inherited Destroy;
end;

procedure TSMCalendar.CreateWnd;
begin
  inherited CreateWnd;

  SetEditRect;
end;

procedure TSMCalendar.SetEditRect;
var
  Loc: TRect;
begin
  SetRect(Loc, 0, FCaptionHeight, ClientWidth, ClientHeight-FCaptionHeight);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

procedure TSMCalendar.SetCaption(Value: string);
begin
  if (Value <> FCaption) then
  begin
    FCaption := Value;
    Invalidate
  end;
end;

procedure TSMCalendar.SetCaptionHeight(Value: Integer);
begin
  if (Value <> FCaptionHeight) then
  begin
    FCaptionHeight := Value;
    Invalidate
  end;
end;
}
procedure TSMCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  TheText: string;
  AColor: TColor;
  AFont: TFont;
begin
  TheText := CellText[ACol, ARow];
  with ARect, Canvas do
  begin
    AFont := TFont.Create;
    {save a current font of canvas}
    AFont.Assign(Font);

    if (gdSelected in AState) then
    begin
      Brush.Color := Color;
      Font.Assign(Self.Font);
    end;
    if Assigned(FGetCellParams) then
    begin
      AColor := Brush.Color;
      FGetCellParams(Self, ACol, ARow, Font, AColor);

      Brush.Color := AColor;
    end;

    TextRect(ARect, Left + (Right - Left - TextWidth(TheText)) div 2,
      Top + (Bottom - Top - TextHeight(TheText)) div 2, TheText);

    {restore a saved font of canvas}
    Font.Assign(AFont);
    AFont.Free;
  end;

//  inherited;

  if Assigned(FOnDrawCell) then
    FOnDrawCell(Self, ACol, ARow, ARect, AState);
end;
{
function TSMCalendar.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;

  Result.Top := Result.Top + CaptionHeight
end;

procedure TSMCalendar.Paint;
var
  ARect: TRect;
begin
  inherited Paint;

  if (Caption = '') or
     (CaptionHeight = 0) then exit;

  ARect := Bounds(0, 0, Width, CaptionHeight);
  DrawText(Canvas.Handle, PChar(Caption), -1, ARect,
           DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX or DT_VCENTER)
end;
}
end.
