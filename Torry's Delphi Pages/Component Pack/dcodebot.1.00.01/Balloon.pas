
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit Balloon;

interface

{$I STD.INC}

uses
  Forms, Windows, Graphics, Messages, Controls, Classes, GraphTools, BtnCtrls;

type
  TBalloonKind = (bkInformation, bkConfirmation, bkWarning, bkError, bkCustom);
	TBalloonPosition = (bpTopLeft, bpTopRight, bpBottomLeft, bpBottomRight);

procedure PreviewBalloonHint(DC: HDC; const Rect: TRect; const Caption, Text: string;
	Icon: HICON; Kind: TBalloonKind; Position: TBalloonPosition);

procedure BalloonHint(const Caption, Text: string;
	Kind: TBalloonKind; Position: TBalloonPosition; X, Y: Integer;
  Duration: Cardinal = 0); overload;

procedure BalloonHint(const Caption, Text: string;
	Icon: HICON; Position: TBalloonPosition; X, Y: Integer;
  Duration: Cardinal = 0); overload;

type
	TBalloonHint = class(TComponent)
  private
    FCaption: string;
    FDuration: Cardinal;
    FIcon: TIcon;
    FKind: TBalloonKind;
    FPosition: TBalloonPosition;
    FText: string;
    FX: Integer;
    FY: Integer;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetIcon(Value: TIcon);
	protected
		procedure SetName(const Value: TComponentName); override;
  public
  	constructor Create(AOwner: TComponent); override;
  	destructor Destroy; override;
  published
  	property Active: Boolean read GetActive write SetActive;
  	property Caption: string read FCaption write FCaption;
    property Duration: Cardinal read FDuration write FDuration;
    property Icon: TIcon read FIcon write SetIcon;
    property Kind: TBalloonKind read FKind write FKind;
    property Position: TBalloonPosition read FPosition write FPosition;
    property Text: string read FText write FText;
  	property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
  end;

function BalloonHintActive: Boolean;

implementation

const
	DefaultDuration = 15000;
  Icons: array[TBalloonKind] of PChar = (IDI_ASTERISK, IDI_QUESTION,
  	IDI_EXCLAMATION, IDI_HAND, IDI_APPLICATION);
  COLOR_TOOLTIP = $E1FFFF;
	DT_FORMAT = DT_TOP or DT_LEFT or DT_NOCLIP or DT_NOPREFIX;

function CreateBalloonRgn(const Rect: TRect; Placement: TBalloonPosition): HRGN;
var
  Points: array[0..2] of TPoint;
	A, B, C: HRGN;
begin
  with Rect do
  begin
    A := CreateRectRgn(Left, Top, Right, Bottom);
    case Placement of
			bpTopLeft, bpTopRight:
				B := CreateRoundRectRgn(Left, Top, Right, Bottom - 16, 11, 11);
      else
				B := CreateRoundRectRgn(Left, Top + 16, Right, Bottom, 11, 11);
		end;
    case Placement of
			bpTopLeft:
      	begin
			    Points[0] := Point(Right - 16, Bottom - 20);
    			Points[1] := Point(Right - 16, Bottom);
			    Points[2] := Point(Right - 36, Bottom - 20);
        end;
      bpTopRight:
      	begin
			    Points[0] := Point(Left + 16, Bottom - 20);
    			Points[1] := Point(Left + 16, Bottom);
			    Points[2] := Point(Left + 36, Bottom - 20);
        end;
      bpBottomLeft:
      	begin
			    Points[0] := Point(Right - 16, Top + 20);
    			Points[1] := Point(Right - 16, Top);
			    Points[2] := Point(Right - 36, Top + 20);
        end;
      bpBottomRight:
      	begin
			    Points[0] := Point(Left + 16, Top + 20);
    			Points[1] := Point(Left + 16, Top);
			    Points[2] := Point(Left + 36, Top + 20);
        end;
		end;
    C := CreatePolygonRgn(Points, 3, WINDING);
  end;
  CombineRgn(A, B, C, RGN_OR);
  Result := A;
  DeleteObject(B);
  DeleteObject(C);
end;

procedure FillBalloonRgn(DC: HDC; Rgn: HRGN);
var
  Brush: HBRUSH;
begin
  Brush := CreateSolidBrush(ColorToRGB(COLOR_TOOLTIP));
  FillRgn(DC, Rgn, Brush);
  DeleteObject(Brush);
  FrameRgn(DC, Rgn, GetStockObject(BLACK_BRUSH), 1, 1);
end;

procedure SetBalloonWindow(Wnd: HWND; const Rect: TRect; Placement: TBalloonPosition);
begin
  SetWindowRgn(Wnd, CreateBalloonRgn(Rect, Placement), True);
end;

function CalculateBallonRect(BoldFont, Font: HFONT;
	const Caption, Text: string): TRect;
var
	DC: HDC;
  PriorFont: HFONT;
  Size: TSize;
  X, Y: Integer;
  Strings: TStrings;
  I: Integer;
begin
	DC := GetDC(0);
  PriorFont := SelectObject(DC, BoldFont);
  if Caption = '' then
		Size := CalculateCaptionSize(DC, ' ')
  else
		Size := CalculateCaptionSize(DC, Caption);
  X := Size.cx + 32;
  Y := Size.cy;
  if Text <> '' then
  begin
	  SelectObject(DC, Font);
    Strings := TStringList.Create;
    try
      Strings.Text := Text;
      if Strings.Count > 0 then Y := Y + 8;
      for I := 0 to Strings.Count - 1 do
      begin
        Size := CalculateCaptionSize(DC, Strings[I] + ' ');
        if Size.cx - 16 > X then
          X := Size.cx - 16;
        Y := Y + Size.cy;
      end;
    finally
      Strings.Free;
    end;
	end;
  SelectObject(DC, PriorFont);
  ReleaseDC(0, DC);
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := X + 32;
  Result.Bottom := Y + 36;
end;

{ TBalloonForm }

type
  TBalloonForm = class(TCustomForm)
  private
    FIcon: HICON;
    FFont: HFONT;
    FBoldFont: HFONT;
  	FDuration: Cardinal;
    FPosition: TBalloonPosition;
    FKind: TBalloonKind;
    FRect: TRect;
    FRegion: HRGN;
    FText: string;
    FTimer: Cardinal;
    FThemeButton: TThemeGlyphButton;
    procedure ButtonClick(Sender: TObject);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup(X, Y: Integer);
    property Position: TBalloonPosition read FPosition write FPosition;
    property Icon: HICON read FIcon write FIcon;
    property Text: string read FText write FText;
    property Kind: TBalloonKind read FKind write FKind;
    property Duration: Cardinal read FDuration write FDuration;
  end;

var
	BalloonForm: TBalloonForm;

function BalloonHintActive: Boolean;
begin
	Result := BalloonForm <> nil;
end;

constructor TBalloonForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  if BalloonForm <> nil then
  	BalloonForm.Release;
  BalloonForm := Self;
  Color := COLOR_TOOLTIP;
	Top := 0;
  Left := 0;
  Width := 200;
  Height := 100;
	HandleNeeded;
  FThemeButton := TThemeGlyphButton.Create(Self);
  with FThemeButton do
  begin
  	Parent := Self;
    OnClick := ButtonClick;
  end;
end;

procedure TBalloonForm.CreateParams(var Params: TCreateParams);
begin
	inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP;
    ExStyle := WS_EX_TOOLWINDOW;
  end;
end;

destructor TBalloonForm.Destroy;
begin
  if BalloonForm = Self then
		BalloonForm := nil;
  if FRegion <> 0 then
  	DeleteObject(FRegion);
	if FTimer <> 0 then
  	KillTimer(Handle, FTimer);
  if FFont <> 0 then
  	DeleteObject(FFont);
  if FBoldFont <> 0 then
  	DeleteObject(FBoldFont);
  inherited Destroy;
end;

procedure TBalloonForm.Popup(X, Y: Integer);
var
	LogFont: TLogFont;
begin
	if FIcon = 0 then
		FIcon := LoadIcon(0, Icons[FKind]);
  SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(LogFont), @LogFont, 0);
  FFont := CreateFontIndirect(LogFont);
  LogFont.lfWeight := FW_BOLD;
  FBoldFont := CreateFontIndirect(LogFont);
  FRect := CalculateBallonRect(FBoldFont, FFont, Caption, Text);
  Width := WidthOf(FRect);
  Height := HeightOf(FRect);
  case FPosition of
    bpTopLeft, bpTopRight:
    	begin
      	FThemeButton.Top := 4;
				FThemeButton.Left := Width - FThemeButton.Width - 4;
        Y := Y - Height;
        if Position = bpTopLeft then
        	X := X - Width + 16
        else
        	X := X - 16;
      end;
	else
		FThemeButton.Top := 20;
    FThemeButton.Left := Width - FThemeButton.Width - 4;
		if Position = bpBottomLeft then
    	X := X - Width + 16
		else
    	X := X - 16;
  end;
  if FRegion <> 0 then
  	DeleteObject(FRegion);
  FRegion := CreateBalloonRgn(FRect, FPosition);
  SetBalloonWindow(Handle, FRect, FPosition);
	SetWindowPos(Handle, HWND_TOPMOST, X, Y, Width, Height,
		SWP_SHOWWINDOW or SWP_NOACTIVATE);
	if FDuration = 0 then
  	FDuration := DefaultDuration;
	FTimer := SetTimer(Handle, 1, FDuration, nil);
end;

procedure TBalloonForm.Paint;
var
	DC: HDC;
  PriorFont: HFONT;
	R: TRect;
begin
	DC := Canvas.Handle;
  FillBalloonRgn(DC, FRegion);
	R := FRect;
  case FPosition of
    bpTopLeft, bpTopRight: R.Top := 8;
	else
		R.Top := 24;
  end;
  Inc(R.Left, 8);
  SetBkMode(DC, TRANSPARENT);
  PriorFont := SelectObject(DC, FBoldFont);
  DrawIconEx(DC, R.Left, R.Top, FIcon, 16, 16, 0, 0, DI_NORMAL);
  Inc(R.Left, 24);
  DrawText(DC, PChar(Caption), -1, R, DT_FORMAT or DT_SINGLELINE);
  Dec(R.Left, 24);
  Inc(R.Top, GraphTools.CalculateCaptionSize(DC, ' ').cy + 8);
  if FText <> '' then
  begin
  	SelectObject(DC, FFont);
		DrawText(DC, PChar(FText), -1, R, DT_FORMAT or DT_WORDBREAK);
  end;
	SelectObject(DC, PriorFont);
end;

procedure TBalloonForm.ButtonClick(Sender: TObject);
begin
  Release;
end;

procedure TBalloonForm.CMShowingChanged(var Message: TMessage);
begin
	SetWindowPos(Handle, HWND_TOPMOST, 8, 8, 200, 100,
		SWP_SHOWWINDOW or SWP_NOACTIVATE);
end;

procedure TBalloonForm.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TBalloonForm.WMTimer(var Message: TWMTimer);
begin
	KillTimer(Handle, FTimer);
	FTimer := 0;
  Release;
end;

procedure PreviewBalloonHint(DC: HDC; const Rect: TRect; const Caption, Text: string;
	Icon: HICON; Kind: TBalloonKind; Position: TBalloonPosition);
var
	LogFont: TLogFont;
	Font, BoldFont, PriorFont: HFont;
	HintIcon: HICON;
	R, B: TRect;
  W, H: Integer;
  Rgn: HRGN;
begin
  SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(LogFont), @LogFont, 0);
  Font := CreateFontIndirect(LogFont);
  LogFont.lfWeight := FW_BOLD;
  BoldFont := CreateFontIndirect(LogFont);
  R := CalculateBallonRect(BoldFont, Font, Caption, Text);
  W := WidthOf(R);
  H := HeightOf(R);
  R.Left := Rect.Left + (WidthOf(Rect) - W) div 2;
  R.Top := Rect.Top + (HeightOf(Rect) - H) div 2;
  R.Right := R.Left + W;
  R.Bottom := R.Top + H;
  Rgn := CreateBalloonRgn(R, Position);
  FillBalloonRgn(DC, Rgn);
  DeleteObject(Rgn);
  case Position of
    bpTopLeft, bpTopRight: Inc(R.Top, 8);
	else
		Inc(R.Top, 24);
  end;
  Inc(R.Left, 8);
  SetBkMode(DC, TRANSPARENT);
  PriorFont := SelectObject(DC, BoldFont);
  if (Kind = bkCustom) and (Icon <> 0) then
  	HintIcon := Icon
 	else
		HintIcon := LoadIcon(0, Icons[Kind]);
  DrawIconEx(DC, R.Left, R.Top, HintIcon, 16, 16, 0, 0, DI_NORMAL);
  B := R;
  with B do
  begin
    Dec(Top, 4);
    Dec(Right, 4);
    Left := Right - 22;
    Bottom := Top + 22;
  end;
  DrawThemeToolClose(DC, B, []);
  Inc(R.Left, 24);
  DrawText(DC, PChar(Caption), -1, R, DT_FORMAT or DT_SINGLELINE);
  Dec(R.Left, 24);
  Inc(R.Top, CalculateCaptionSize(DC, ' ').cy + 8);
  if Text <> '' then
  begin
  	SelectObject(DC, Font);
		DrawText(DC, PChar(Text), -1, R, DT_FORMAT or DT_WORDBREAK);
  end;
	SelectObject(DC, PriorFont);
end;

procedure BalloonHint(const Caption, Text: string;
	Kind: TBalloonKind; Position: TBalloonPosition; X, Y: Integer;
  Duration: Cardinal = 0);
var
  B: TBalloonForm;
begin
	B := TBalloonForm.Create(Application);
  B.Caption := Caption;
  B.Text := Text;
  B.Kind := Kind;
  B.Position := Position;
	B.Duration := Duration;
  B.Popup(X, Y);
end;

procedure BalloonHint(const Caption, Text: string;
	Icon: HICON; Position: TBalloonPosition; X, Y: Integer;
  Duration: Cardinal = 0);
var
  B: TBalloonForm;
begin
	B := TBalloonForm.Create(Application);
  B.Caption := Caption;
  B.Text := Text;
  B.Kind := bkCustom;
  B.Icon := Icon;
  B.Position := Position;
	B.Duration := Duration;
  B.Popup(X, Y);
end;

{ TBalloonHint }

constructor TBalloonHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
	FIcon := TIcon.Create;
  FX := Screen.Width div 2;
  FY := Screen.Height div 2;
end;

destructor TBalloonHint.Destroy;
begin
	FIcon.Free;
  inherited Destroy;
end;

function TBalloonHint.GetActive: Boolean;
begin
  Result := False;
end;

procedure TBalloonHint.SetActive(Value: Boolean);
begin
	if csLoading in ComponentState then Exit;
	if FKind = bkCustom then
  	BalloonHint(Caption, Text, Icon.Handle, Position, X, Y, Duration)
  else
  	BalloonHint(Caption, Text, Kind, Position, X, Y, Duration)
end;

procedure TBalloonHint.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText :=
    not (csLoading in ComponentState) and (Caption = Text) and
    ((Owner = nil) or not (Owner is TControl) or
    not (csLoading in TControl(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText then Caption := Value;
end;

procedure TBalloonHint.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
end;

end.


