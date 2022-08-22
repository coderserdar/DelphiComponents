{*> Ver: V2.3 *********      History      ***************************}
// -----------------------------------------------------------------------------
// Base: BMSpinEdit.pas received by email from Boian on 19.3.2001
//
// Description of changes:
// 11.03.2001 - Karsten Lehnart, lehnart@device.de
// - TrackBarEnabled
//   Added a Boolean property to en- or disable the trackbar functionality
// - IncrementGauge
//   Added a Double property to define the step interval for changes of the value
//   using the trackbar or the gauge. IncrementGauge = 0 disables this ability.
// - Precsion = -1:
//   The number of decimals is variable. You can enter as much you want.
// - Relaxed handling of decimalseparators (See ValueToText)
//   In countries with decimal separators <> ',' sometimes the keyboard has
//   on the numeric keypad either a "," or a "."
//   So a relaxed handling of decimalseparators for those users is very useful.
// - CheckValue
//   The Check of NewV against Value can be faulty: Value itself may contain
//   NewV already. Change: Save Value first and check against the stored value
// - Change
//   If ValueUnit is > '' and after entering the edit field you enter the first
//   character (AutoSelect=True), normally you expect to see the character
//   you entered and the caret should stand át the right side of the character.
//   Because SelStart was not restored the caret was placed on the left side.
// - Destructor added
//   There was no destructor defined - I added it
// 12.03.2001 - Boian Mitov
// - Changes made by Boian Mitov / Mauro Venturini
//   e.g. GuageEndColor, GuageHeight
// 14.03.2001 - Karsten Lehnart
// - TextToValue: If ValueUnit was > '' and inside the edit field the value
//   including the trailing blank was selected and then a new value was entered
//   it was not detected correctly because TextToValue always deleted the
//   trailing unit string plus one character to the left: "Copy(S,1,P-2)".
//   Changed to "S := Trim(Copy(S,1,P-1))"
// - New method: RawValueStr: Returns the text property without the UnitValue
//   This can be used to check if the current raw string can be converted in
//   a float string.
// - Change: If ValueUnit is > '' it make sense to reformat the text every time
//   the edit field changes. Otherwise it could be that the space character
//   between the parts "value" and "unit" was missing.
//   I deleted the condition line "if Pos(FValueUnit,Text) = 0 then begin"
// - TBMAGauge.WMMouseMove:
//   Pos changed from Integer to SmallInt. Otherwise Pos got the value MaxInt
//   if the mouse moved to the left side of the gauge. In this case the edit
//   field got the value MaxValue.
//   Changed the condition "if Pos <= 0" to "if Pos <= 1"
// 19.03.2001 - Boian Mitov
//   Integrated with the latest changes by Boian Mitov
// 20.03.2001 - Karsten Lehnart
// - New properties: GaugeMinValue and GaugeMaxValue.
//   If these properties are set to different values than 0, the range you can
//   define using the progressbar is limited to GaugeMinValue..GaugeMaxValue.
//   Sample: MinValue..MaxValue = -100..+100, GaugeMinValue..GaugeMaxValue =-10..10
//   Using the UpDown-Buttons, the trackbar or entering the value by keyboard
//   you can enter a value in the range form -100..100. Using the progressbar
//   at the bottom of the edit field ou can only define the value -10..10
//   Advantage: You have a better resolution by ragging the progressbar with the mouse
// - New property: GaugeAroundCenter
//   If this property is inside the range GaugeMinValue..GaugeMaxValue the
//   value of the new property is the center of the bar. If the value
//   becomes smaller than GaugeAroundCenter the bar is displayed to the left
//   else to the right.
// 21.03.2001 - Boian Mitov
// - Gauge colorisation in case of useing GaugeAroundCenter fix.
// 05.05.2001 - Boian Mitov
// - Keeps focus on the parent form when shows popup.
// 12.04.2003 - Boian Mitov
// - Small bug fix thanks to Mateo Zanotelli
// 01.29.2006 - Boian Mitov
// - Delphi 2006, 2007, and 2009 support
// 11.21.2008 - Boian Mitov
// - Numerous fixes.
// 11.22.2009 - Boian Mitov
// - Delphi 2010 support
// 11.28.2011 - Boian Mitov
// - Delphi XE and XE2 support
// - 64 bit support
// 05.29.2014 - Boian Mitov
// - Delphi XE3, XE4, XE5 and XE6 support
// -----------------------------------------------------------------------------

{$IFDEF VER240} // Delphi XE3
  {$DEFINE XE3Up}
{$ENDIF}

{$IFDEF VER250} // Delphi XE4
  {$DEFINE XE3Up}
{$ENDIF}

{$IFDEF VER260} // Delphi XE5
  {$DEFINE XE3Up}
{$ENDIF}

{$IFDEF VER270} // Delphi XE6
  {$DEFINE XE3Up}
{$ENDIF}

unit BMDSpinEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, CommCtrl, Buttons, ExtCtrls;

const
  WM_NEEDS_UPDATE = WM_USER + $600;


type
{ Forward declarations }
  TBMDGauge = class;

  TBMDSpinEdit = class(TCustomEdit)
  private
    Form: TForm;
    TrackBar: TTrackBar;
    ProgressBar: TBMDGauge;
    FMinValue: Double;
    FMaxValue: Double;
    FGaugeMinValue: Double;
    FGaugeMaxValue: Double;
    FValueUnit: string;
    FIncrement: Double;
    FEditorEnabled  : Boolean;
    InChanging: Boolean;
    FTrackBarOrientation: TTrackBarOrientation;
    FWrap: Boolean;
    FPrecision: Integer;
    FGuageHeight: Integer;
    FTrackBarWidth: Integer;
    FTrackBarHeight: Integer;
    FUpDown: TUpDown;
    FSpeedButton: TSpeedButton;
    FTrackBarEnabled : Boolean;
    FPanel: TPanel;
    MoveFormToPoint: TPoint;
  private
    function ValueToText(const V: Double): string;
    function RawValueStr(const S : string):string;
    function TextToValue(S: string): Double;
    function GetMinHeight: Integer;
    procedure SetEditRect;
    function GetValue: Double;
    function CheckValue(NewV: Double): Double;
    procedure SetValue(NewV: Double);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure SpeedButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure SpeedButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure SpeedButtonMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure UpdateFormView;
    procedure SetMaxValue(V: Double);
    procedure SetMinValue(V: Double);
    procedure SetGaugeMaxValue(V: Double);
    procedure SetGaugeMinValue(V: Double);
    procedure SetValueUnit(S: string);
    procedure SetIncrement(V: Double);
    procedure SetGaugeAroundCenter(V:Double);
    function  GetGaugeAroundCenter:Double;
    procedure SetIncrementGauge(V: Double);
    function  GetIncrementGauge() : Double;
    procedure SetGuageBeginColor(V: TColor);
    function  GetGuageBeginColor: TColor;
    procedure SetGuageEndColor(V: TColor);
    function  GetGuageEndColor: TColor;
    procedure SetTrackBarWidth(V: Integer);
    procedure SetPrecision(V: Integer);
    procedure SetGuageHeight(V: Integer);
    procedure SetTrackBarOrientation(Orientation: TTrackBarOrientation);
    procedure SetTrackBarEnabled(const Value: Boolean);   
    procedure SetButtonHandle;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Change; override;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure CMEnter(var Message: TMessage); message CM_ENTER;
    procedure CMExit(var Message: TMessage); message CM_EXIT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMNeedsUpdate(var Message: TMessage); message WM_NEEDS_UPDATE;
  public
    procedure SetBounds(ALeft,ATop,AWidth,AHeight: Integer); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property ValueUnit: string read FValueUnit write SetValueUnit;
    property Increment: Double read FIncrement write SetIncrement;
    property IncrementGauge : Double read GetIncrementGauge write SetIncrementGauge;
    property GaugeAroundCenter : Double read GetGaugeAroundCenter write SetGaugeAroundCenter;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property GaugeMaxValue: Double read FGaugeMaxValue write SetGaugeMaxValue;
    property GaugeMinValue: Double read FGaugeMinValue write SetGaugeMinValue;
    property Value: Double read GetValue write SetValue;
    property GuageBeginColor: TColor read GetGuageBeginColor write SetGuageBeginColor default clHighlight;
    property GuageEndColor: TColor read GetGuageEndColor write SetGuageEndColor default clRed;
    property TrackBarWidth: Integer read FTrackBarWidth write SetTrackBarWidth default 150;
    property Precision: Integer read FPrecision write SetPrecision default 2;
    property TrackBarOrientation: TTrackBarOrientation read FTrackBarOrientation write SetTrackBarOrientation default trHorizontal;
    property TrackBarEnabled : Boolean read FTrackBarEnabled write SetTrackBarEnabled default True;
    property GuageHeight : Integer read FGuageHeight write SetGuageHeight default 5;
  end;

  TBMDGauge = class(TCustomControl)
  private
		FEdit: TBMDSpinEdit;
  	FGuageBeginColor:       TColor;
  	FGuageEndColor:         TColor;
  private
    FIncrement : Double;                                         
    FGaugeAroundCenter:Double;
    procedure SetGuageBeginColor(V: TColor);
    procedure SetGuageEndColor(V: TColor);
    procedure PaintBackground(var AnImage: TBitmap);
    procedure PaintAsBar(var AnImage: TBitmap; const PaintRect: TRect);
    
  protected
    procedure Paint; override;
    procedure WMLBtnDown(var Message: TMessage); message WM_LBUTTONDOWN;
    procedure WMLBtnUp(var Message: TMessage); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message: TMessage); message WM_MOUSEMOVE;
    procedure WMEraseBkg(var Message: TMessage); message WM_ERASEBKGND;
    
  public
    property Increment: Double read FIncrement write FIncrement;
    property GaugeAroundCenter : Double read FGaugeAroundCenter write FGaugeAroundCenter; 
    property GuageBeginColor: TColor read FGuageBeginColor write SetGuageBeginColor;
    property GuageEndColor: TColor read FGuageEndColor write SetGuageEndColor;
    
  public
    constructor Create(AOwner: TComponent); override;
  end;


procedure Register;

implementation

{$R BMSpinEditInt.res}

uses
	Math;

const
	BORDER_WIDTH = 4;

type
  TPopupForm = class( TForm )
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd(); override;
  end;

procedure Register;
begin
  RegisterComponents('BMitov', [TBMDSpinEdit]);
end;

procedure TBMDGauge.Paint;
var
  PaintTRect: TRect;
  TheImage,OverlayImage: TBitmap;
begin
  TheImage := TBitmap.Create;
  OverlayImage := TBitmap.Create;
  TheImage.Height := Height;
  TheImage.Width := Width;
  PaintBackground(TheImage);
  PaintTRect := ClientRect;
  OverlayImage.Width := TheImage.Width;
  OverlayImage.Height := TheImage.Height;
  OverlayImage.Canvas.Brush.Color := TColor(clWindowFrame);
  OverlayImage.Canvas.Brush.Style := bsSolid;
  OverlayImage.Canvas.FillRect(Rect(0,0,Width,Height));
  PaintBackground(OverlayImage);
  PaintAsBar(OverlayImage,PaintTRect);
  OverlayImage.Canvas.Brush.Color := clBtnFace;
  OverlayImage.Canvas.FrameRect(Rect(0,0,Width,Height));
  TheImage.Canvas.CopyMode := cmSrcInvert;
  TheImage.Canvas.Draw(0,0,OverlayImage);
  TheImage.Canvas.CopyMode := cmSrcCopy;
  Canvas.CopyMode := cmSrcCopy;
  Canvas.Draw(0,0,TheImage);
  OverlayImage.Free;
  TheImage.Free;
end;

procedure TBMDGauge.SetGuageBeginColor(V: TColor);
begin
  if FGuageBeginColor <> V  then begin
    FGuageBeginColor := V;
    Invalidate;
  end;
end;

procedure TBMDGauge.SetGuageEndColor(V: TColor);
begin
  if FGuageEndColor <> V  then begin
    FGuageEndColor := V;
    Invalidate;
  end;
end;

procedure TBMDGauge.PaintBackground(var AnImage: TBitmap);
var
  ARect: TRect;
begin
  ARect := Rect(0, 0, Width, Height);
	with AnImage.Canvas do begin
  	CopyMode := cmBlackness;
	  CopyRect(ARect,AnImage.Canvas,ARect);
  	CopyMode := cmSrcCopy;
  end;
end;

procedure TBMDGauge.PaintAsBar(var AnImage: TBitmap; const PaintRect: TRect);
var
  CenterPos, FillSize: Longint;       
  W,H: Integer;
  Diff: Double;
  BeginColor : TColor;
  EndColor : TColor;
  Red, Green, Blue : BYTE;
  Correction : Integer;
  ResultColor : TColor;
  LocalPaintRect : TRect;
  EditValue, LocalMinValue, LocalMaxValue, LocalCenterValue : Double; // <- BM, 21.3.2001 
  IsCentered, DoFillRect : Boolean;                 
begin
	with PaintRect do begin
		W := Right - Left + 1 - 2;
  	H := Bottom - Top + 1;
  end;

  with AnImage.Canvas do begin
  	Brush.Color := FEdit.Color;
  	FillRect(PaintRect);
  end;

  LocalMinValue := FEdit.GaugeMinValue;                      
  LocalMaxValue := FEdit.GaugeMaxValue;                      
  LocalCenterValue := FEdit.GaugeAroundCenter;
  if (LocalMinValue=0) and (LocalMaxValue=0) then
    begin
    LocalMinValue := FEdit.MinValue;
    LocalMaxValue := FEdit.MaxValue;
    end;                                                       

  Diff := LocalMaxValue - LocalMinValue;
  if (Diff <= 0) then
    Diff := 100;

  EditValue := FEdit.Value;           
  FillSize := Min(Round((EditValue - LocalMinValue) * W / Diff + 1.0),W); 

  LocalPaintRect := PaintRect;        
  LocalPaintRect.Bottom := H;         
  if (GaugeAroundCenter > LocalMinValue) and (GaugeAroundCenter<LocalMaxValue) then
    begin
    CenterPos := Min(Round((GaugeAroundCenter - LocalMinValue) * W / Diff + 1.0),W);
    LocalPaintRect.Left := Min(CenterPos,FillSize);
    LocalPaintRect.Right := Max(CenterPos,FillSize);
    DoFillRect := EditValue <> GaugeAroundCenter;
    IsCentered := True;
    end

  else
    begin
    LocalPaintRect.Right := FillSize;
    DoFillRect := FillSize > 0;
    IsCentered := False;
    CenterPos := 0;   // only to initialize it
    end;

  if ( EditValue < LocalMinValue ) then
    EditValue := LocalMinValue;

  if ( EditValue > LocalMaxValue ) then
    EditValue := LocalMaxValue;

  if ( IsCentered and ( LocalCenterValue > LocalMinValue ) and ( LocalCenterValue < LocalMaxValue ))then                // <- BM, 21.3.2001
    begin                                                                                                               // <- BM, 21.3.2001
    if( EditValue > LocalCenterValue ) then                                                                           // <- BM, 21.3.2001
      Correction := Round( ( EditValue - LocalCenterValue ) * 256 / ( LocalMaxValue - LocalCenterValue ) + 1.0 )      // <- BM, 21.3.2001

    else                                                                                                              // <- BM, 21.3.2001
      Correction := Round(( LocalCenterValue - EditValue ) * 256 / ( LocalCenterValue - LocalMinValue ) + 1.0 );      // <- BM, 21.3.2001

    end                                                                                                                 // <- BM, 21.3.2001

  else                                                                                                                  // <- BM, 21.3.2001
    Correction := Round(( EditValue - LocalMinValue ) * 256 / Diff + 1.0 );                                             // <- BM, 21.3.2001

  BeginColor := TColor(ColorToRGB( FGuageBeginColor ));
  EndColor := TColor(ColorToRGB( FGuageEndColor ));

  Red   := GetRValue( BeginColor ) + ((GetRValue( EndColor ) - GetRValue( BeginColor )) * Correction div 256 );
  Green := GetGValue( BeginColor ) + ((GetGValue( EndColor ) - GetGValue( BeginColor )) * Correction div 256 );
  Blue  := GetBValue( BeginColor ) + ((GetBValue( EndColor ) - GetBValue( BeginColor )) * Correction div 256 );

  ResultColor := TColor( RGB( Red, Green, Blue ));

  with AnImage.Canvas do begin
  	Pen.Color := ResultColor;
  	Pen.Width := 1;
  	Brush.Color := ResultColor;
  end;
  if DoFillRect then                                   
    AnImage.Canvas.FillRect(LocalPaintRect);           
    
  if IsCentered then                                   
    with AnImage.Canvas do begin                       
      Pen.Color := clWindowFrame;                      
      Pen.Style := psSolid;                            
      MoveTo(CenterPos,LocalPaintRect.Top);            
      LineTo(CenterPos,LocalPaintRect.Bottom);         
    end;                                               
end;

procedure TBMDGauge.WMLBtnDown(var Message: TMessage);
begin
  MouseCapture := true;
  WMMouseMove(Message);
  FEdit.SetFocus;
  FEdit.SelLength := 0;
  inherited;
end;

procedure TBMDGauge.WMEraseBkg(var Message: TMessage);
begin
end;

procedure TBMDGauge.WMLBtnUp(var Message: TMessage);
begin
  WMMouseMove(Message);
  MouseCapture := false;
  if FEdit.AutoSelect and not (csLButtonDown in FEdit.ControlState) then
    FEdit.SelectAll;
  inherited;
end;

procedure TBMDGauge.WMMouseMove(var Message: TMessage);
var
  Pos    : Integer;
  Diff, V: Double;
  LocalMinValue, LocalMaxValue : Double; 
begin
  if (not MouseCapture) then
    Exit;

  Pos := SmallInt( Message.LParamLo );
  LocalMinValue := FEdit.GaugeMinValue;                      
  LocalMaxValue := FEdit.GaugeMaxValue;                      
  if (LocalMinValue = 0) and (LocalMaxValue = 0) then
    begin
    LocalMinValue := FEdit.MinValue;
    LocalMaxValue := FEdit.MaxValue;
    end;

{
  if Pos <= 1 then
    begin
    FEdit.Value := LocalMinValue;
    Exit;
    end;
}
    
  Diff := LocalMaxValue - LocalMinValue;
  if Diff <= 0 then
    Exit;

  V := ((Pos * Diff) / (Width - 2)) + LocalMinValue;
  if FIncrement > 0 then
    V := FIncrement * Round(V / FIncrement);
    
  FEdit.Value := V;
  inherited;
end;

constructor TBMDGauge.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  FGaugeAroundCenter := 0;
  FGuageBeginColor := clHighlight;
  FGuageEndColor := clRed;
  FEdit := AOwner as TBMDSpinEdit;
end;

function TBMDSpinEdit.ValueToText(const V: Double): string;
begin
  if FPrecision < 0 then begin
    Result := FloatToStrF(V,ffGeneral,4,4);
  end else                                             
    Result := FloatToStrF(V,ffFixed,15,FPrecision);
  if FValueUnit <> '' then
   	Result := Result + ' ' + FValueUnit;
end;

function TBMDSpinEdit.RawValueStr(const S : string):string;
var
	P: Integer;
begin
  Result := S;
  if FValueUnit <> '' then
    begin
    P := Pos(FValueUnit,S);
    if P > 2 then
      Result := Trim(Copy(S,1,P-1))
      
    else
      begin
      P := Pos(' ',S);
      if P > 1 then
        Result := Copy(S,1,P - 1);
        
      end;
      
    end;
    
end;

function TBMDSpinEdit.TextToValue(S: string): Double;
var
	P: Integer;
begin
  S := RawValueStr(S); 
  // Try relaxed handling of decimalseparators.
  // Enable "." as well as "," as decimalseparator:
  if (S > '') and (S <> '-') then
    begin
    for P := Length(S) downto 1 do
      if S[P] = #32 then
        Delete(S,1,1)

      else
{$IFDEF XE3Up}
        if not ( CharInSet( S[P], ['0'..'9','-',FormatSettings.DecimalSeparator])) then
          Delete(S,P,1);

    if( Length( S ) > 0 ) then
      begin
      if S[1] = FormatSettings.DecimalSeparator then
        S := '0'+S;

      if S[Length(S)]= FormatSettings.DecimalSeparator then
        S := S + '0';

{$ELSE}
        if not (S[P] in ['0'..'9','-',DecimalSeparator]) then
          Delete(S,P,1);

    if( Length( S ) > 0 ) then
      begin
      if S[1] = DecimalSeparator then
        S := '0'+S;

      if S[Length(S)]= DecimalSeparator then
        S := S + '0';

{$ENDIF}
      end;

    Result := StrToFloat(S);
    end
    
  else
    Result := 0;
                                                   
end;

function TBMDSpinEdit.GetMinHeight: Integer;
var
	DC: HDC;
  SaveFont: HFONT;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC,SysMetrics);
  SaveFont := SelectObject(DC,Font.Handle);
  GetTextMetrics(DC,Metrics);
  SelectObject(DC,SaveFont);
  ReleaseDC(0,DC);
  Result := Metrics.tmHeight + Min(SysMetrics.tmHeight,Metrics.tmHeight) div 4 +
  					GetSystemMetrics(SM_CYBORDER) * 4 + 2 + GuageHeight + 1;
end;

procedure TBMDSpinEdit.SetEditRect;
var
	Loc: TRect;
begin
  SendMessage(Handle,EM_GETRECT,0,Longint(@Loc));
  Loc.Bottom := ClientHeight + 1;  // +1 is workaround for windows paint bug
  Loc.Right := ClientWidth; //- FUpDown->Width - 2;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle,EM_SETRECTNP,0,Longint(@Loc));
  SendMessage(Handle,EM_GETRECT,0,Longint(@Loc));  // debug
end;

function TBMDSpinEdit.GetValue: Double;
begin
  if Text = '' then begin
  	Text := ValueToText(MinValue);
    Result := MinValue;
    Exit;
	end;
  try
    Result := TextToValue(Text);
  except
  	Text := ValueToText(MinValue);
    Result := MinValue;
  end;
end;

function TBMDSpinEdit.CheckValue(NewV: Double): Double;
begin
  Result := NewV;
  if MaxValue <> MinValue then
    Result := Min(Max(NewV,MinValue),MaxValue);
end;

procedure TBMDSpinEdit.SetValue(NewV: Double);
var
  OldValue : Double;
begin
  OldValue := Value;
  Text := ValueToText(CheckValue(NewV));
  // Check NewV against OldValue and not Value because Value itself
  // could be already NewV:
  if NewV = OldValue then                                  
    Exit;
    
  ProgressBar.Repaint;
  if Form.Visible then
    UpdateFormView();

  if not (csLoading in ComponentState) and Assigned(OnChange) then
    OnChange(Self);
end;

procedure TBMDSpinEdit.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  SetFocus();
  
  if( Button = btNext ) then
    begin
    if( Value >= (MaxValue - Increment)) then
      begin
      if( FWrap ) then
        Value := MinValue
        
      else
        Value := MaxValue;
        
      end

    else
      Value := Value + Increment;
      
    end
    
  else
    begin
    if Value <= (MinValue + Increment) then
      begin
      if FWrap then
        Value := MaxValue
        
      else
        Value := MinValue;
      end

    else
      Value := Value - Increment;
      
    end;
    
  PostMessage(Handle,WM_NEEDS_UPDATE,0,0);
  if( AutoSelect and not (csLButtonDown in ControlState)) then
    SelectAll();
    
  ProgressBar.Invalidate();

end;

procedure TBMDSpinEdit.SpeedButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
	APoint,Pos: Tpoint;
begin
  Form.Handle;
  if Button <> mbLeft then
    Exit;
  APoint.X := X;
  APoint.Y := Y;
  MoveFormToPoint := FSpeedButton.ClientToScreen(APoint);
  if FTrackBarOrientation = trVertical then begin
	  APoint.X := FSpeedButton.Left + FSpeedButton.Width;
  	APoint.Y := FSpeedButton.Top;
    Pos := FPanel.ClientToScreen(APoint);
	end
  else begin
	  APoint.X := FSpeedButton.Left;
  	APoint.Y := FSpeedButton.Top;
    Pos := FPanel.ClientToScreen(APoint);
  end;
  Form.Left := Pos.x;
  Form.Top := Pos.y + FSpeedButton.Height;
  UpdateFormView;
end;

procedure TBMDSpinEdit.SpeedButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if Button <> mbLeft then
    Exit;
  Form.Visible := false;
//  FSpeedButton.Enabled := True;
  FSpeedButton.Down := false;
  FSpeedButton.GroupIndex := 0;
  FSpeedButton.Enabled := false;
  FSpeedButton.Enabled := True;
  
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
end;

procedure TBMDSpinEdit.SpeedButtonMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
	APoint,MousePoint,SliderLeftTop,SliderRightBottom: TPoint;
  Rect,Rect1: TRect;
  SliderSize,Position: Longint;
  TumbHeight,TumbWidth: Integer;
  V: Double;                                               
begin
  if not Form.Visible then
  	Exit;
  APoint.X := X;
  APoint.Y := Y;
  MousePoint := FSpeedButton.ClientToScreen(APoint);
  TrackBar.Perform(TBM_GETCHANNELRECT,0,Integer(@Rect));
  TrackBar.Perform(TBM_GETTHUMBRECT,0,Integer(@Rect1));
  if FTrackBarOrientation = trVertical then begin
  	APoint.X := Rect.Top;
    APoint.Y := Rect.Left;
    SliderLeftTop := Form.ClientToScreen(APoint);
  	APoint.X := Rect.Bottom;
    APoint.Y := Rect.Right;
    SliderRightBottom := Form.ClientToScreen(APoint);
    TumbHeight := Rect1.Bottom - Rect1.Top;
    SliderSize := SliderRightBottom.y - SliderLeftTop.y - TumbHeight;
    Position := SliderSize - (MousePoint.y - SliderLeftTop.y - TumbHeight);
  end
  else begin
  	APoint.X := Rect.Left;
    APoint.Y := Rect.Top;
    SliderLeftTop := Form.ClientToScreen (APoint);
  	APoint.X := Rect.Right;
    APoint.Y := Rect.Bottom;
    SliderRightBottom := Form.ClientToScreen(APoint);
    TumbWidth := Rect1.Right - Rect1.Left;
    SliderSize := SliderRightBottom.x - SliderLeftTop.x - TumbWidth;
    Position := MousePoint.x - SliderLeftTop.x - TumbWidth;
  end;
  if Position > SliderSize then
    Position := SliderSize;
  if Position < 0 then
    Position := 0;
  V := (Position * (FMaxValue - MinValue)) / SliderSize + MinValue;
  if IncrementGauge > 0 then
    V := IncrementGauge * Round(V/IncrementGauge);
  Value := V;                                                       
end;

procedure TBMDSpinEdit.FormShow(Sender: TObject);
var
	Rect: TRect;
begin
  TrackBar.Perform(TBM_GETTHUMBRECT,0,Integer(@Rect));
  if (FTrackBarOrientation = trVertical) then
    Form.Top := MoveFormToPoint.y - Rect.Top - ((Rect.Bottom - Rect.Top) div 2 ) - BORDER_WIDTH

  else
    Form.Left := MoveFormToPoint.x - Rect.Left - ((Rect.Right - Rect.Left) div 2) - BORDER_WIDTH;

//  FSpeedButton.Enabled := False;
  FSpeedButton.GroupIndex := 333553;
  FSpeedButton.Down := True;

end;

procedure TBMDSpinEdit.UpdateFormView;
var
	Diff: Double;
  FillSize: Longint;
begin
  Diff := MaxValue - MinValue;
  if Diff <= 0 then
    Diff := 100;
  FillSize := Min(Round((Value - MinValue) * 1000 / Diff),1000);
  if FTrackBarOrientation = trVertical then
    begin
    TrackBar.Position := TrackBar.Max - FillSize;
    Form.Height := FTrackBarWidth;
    end

  else
    begin
    TrackBar.Position := FillSize;
    Form.Width := FTrackBarWidth;
    end;
  
  SetFocus;
  SelLength := 0;
  Form.Visible := true;
end;

procedure TBMDSpinEdit.SetMaxValue(V: Double);
begin
  if FMaxValue <> V then begin
    FMaxValue := V;
    Value := Min(Value,FMaxValue);
    ProgressBar.Repaint;
	end;
end;

procedure TBMDSpinEdit.SetMinValue(V: Double);
begin
  if MinValue <> V then begin
    FMinValue := V;
    Value := Max(Value,FMinValue);
    ProgressBar.Repaint;
  end;
end;

procedure TBMDSpinEdit.SetGaugeMaxValue(V: Double);
begin
  if FGaugeMaxValue <> V then begin
    FGaugeMaxValue := V;
    ProgressBar.Repaint;
	end;
end;

procedure TBMDSpinEdit.SetGaugeMinValue(V: Double);
begin
  if FGaugeMinValue <> V then begin
    FGaugeMinValue := V;
    ProgressBar.Repaint;
  end;
end;

procedure TBMDSpinEdit.SetValueUnit(S: string);
begin
	FValueUnit := S;
  SetValue(Value);
end;

procedure TBMDSpinEdit.SetIncrement(V: Double);
begin
  FIncrement := V;
end;

procedure TBMDSpinEdit.SetIncrementGauge(V: Double);
begin
  ProgressBar.Increment := V;
end;

function TBMDSpinEdit.GetIncrementGauge() : Double;
begin
  Result := ProgressBar.Increment;
end;

procedure TBMDSpinEdit.SetGaugeAroundCenter(V: Double);
begin
  ProgressBar.GaugeAroundCenter := V;
  ProgressBar.Repaint;
end;

function TBMDSpinEdit.GetGaugeAroundCenter: Double;
begin
  Result := ProgressBar.GaugeAroundCenter;
end;

procedure TBMDSpinEdit.SetGuageBeginColor(V: TColor);
begin
  ProgressBar.GuageBeginColor := V;
end;

function TBMDSpinEdit.GetGuageBeginColor: TColor;
begin
  Result := ProgressBar.GuageBeginColor;
end;

procedure TBMDSpinEdit.SetGuageEndColor(V: TColor);
begin
  ProgressBar.GuageEndColor := V;
end;

function TBMDSpinEdit.GetGuageEndColor: TColor;
begin
  Result := ProgressBar.GuageEndColor;
end;

procedure TBMDSpinEdit.SetTrackBarWidth(V: Integer);
begin
  FTrackBarWidth := Min(Max(V,50),300);
end;

// Added by Boian Mitov >>>>
procedure TBMDSpinEdit.SetGuageHeight(V: Integer);
begin
  if FGuageHeight <> V then
    begin
    FGuageHeight := V;
    SetBounds( Left, Top, Width, Height ); 
    end;
    
end;
// <<<< Added by Boian Mitov

procedure TBMDSpinEdit.SetPrecision(V: Integer);
begin
  V := Min(Max(V,-1),6);                                     
  if FPrecision <> V then begin
    FPrecision := V;
    SetValue(Value);
	end;
end;

procedure TBMDSpinEdit.SetTrackBarOrientation(Orientation: TTrackBarOrientation);
begin
  if FTrackBarOrientation <> Orientation then begin
    FTrackBarOrientation := Orientation;
    SetButtonHandle;
  end;
end;

procedure TBMDSpinEdit.SetTrackBarEnabled(const Value: Boolean);
begin                                                             
  if Value <> FTrackBarEnabled then begin                         
    FTrackBarEnabled := Value;                                    
    FSpeedButton.Visible := Value;                                
    SetBounds(Left, Top, Width, Height);                          
    SetEditRect;                                                  
    FSpeedButton.Visible := Value;                                
    FSpeedButton.Refresh;                                         
    Invalidate;                                                   
  end;                                                            
end;                                                              

procedure TBMDSpinEdit.SetButtonHandle;
begin
  TrackBar.Orientation := TrackBarOrientation;
  if FTrackBarOrientation = trVertical then
    begin
    FSpeedButton.Glyph.Handle := LoadBitmap(HInstance,'DropLeft');
    Form.Width := FTrackBarHeight;
    Form.Height := FTrackBarWidth;
    end

  else
    begin
    FSpeedButton.Glyph.Handle := LoadBitmap(HInstance,'DropDown');
    Form.Height := FTrackBarHeight;
    Form.Width := FTrackBarWidth;
  	end;
end;

procedure TBMDSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if( Key = VK_UP ) then
    begin
    FUpDown.OnClick( Self, btNext );
    Key := 0;
    end

  else if( Key = VK_DOWN ) then
    begin
    FUpDown.OnClick( Self, btPrev );
    Key := 0;
    end;
    
  if( not FEditorEnabled ) then
    Key := 0;

  inherited KeyDown(Key, Shift);
end;

procedure TBMDSpinEdit.KeyPress(var Key: Char);
begin
  if( not FEditorEnabled ) then
    Key := #0;

  if( Key > ' ' ) then
    if(( Key < '0' ) or ( Key > '9' )) then
{$IFDEF XE3Up}
      if( ( Key <> '-' ) and ( Key <> '+' ) and ( Key <> FormatSettings.DecimalSeparator )) then
{$ELSE}
      if( ( Key <> '-' ) and ( Key <> '+' ) and ( Key <> DecimalSeparator )) then
{$ENDIF}
        Key := #0;

  inherited;
end;

procedure TBMDSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TBMDSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TBMDSpinEdit.Change;
var
  SelS : Integer;
  S    : string;
begin
	if FValueUnit <> '' then begin
    S := RawValueStr(Text);
{$IFDEF XE3Up}
    if (S > '') and (CharInSet( S[Length(S)], ['0'..'9'])) then begin
{$ELSE}
    if (S > '') and (S[Length(S)] in ['0'..'9']) then begin
{$ENDIF}
      SelS := SelStart;
      SetValue(Value);
      SelStart := SelS;
    end;
  end;
  TBMDGauge(ProgressBar).Repaint;
	if not (csLoading in ComponentState) and Assigned(OnChange) then
    OnChange(Self);
end;

procedure TBMDSpinEdit.WMSize(var Message: TMessage);
var
  MinHeight: Integer;
begin
  MinHeight := GetMinHeight;
  if Height < MinHeight then
    Height := MinHeight
  else if Assigned(FUpDown) and Assigned(FSpeedButton) then
    SetEditRect;
  inherited;
end;

procedure TBMDSpinEdit.CMEnter(var Message: TMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TBMDSpinEdit.CMExit(var Message: TMessage);
begin
  if CheckValue(Value) <> Value then
    SetValue(CheckValue(Value));
  inherited;
end;

procedure TBMDSpinEdit.WMPaste(var Message: TMessage);
begin
  if not FEditorEnabled or ReadOnly then
    Exit;
  inherited;
end;

procedure TBMDSpinEdit.WMCut(var Message: TMessage);
begin
  if not FEditorEnabled or ReadOnly then
    Exit;
  inherited;
end;

procedure TBMDSpinEdit.WMNeedsUpdate(var Message: TMessage);
begin
  FUpDown.Position := 50;
  inherited;
end;

procedure TBMDSpinEdit.SetBounds(ALeft,ATop,AWidth,AHeight: Integer);
var                                                                
  FSpW : Integer;                                                  
begin
  if Assigned(FPanel) then begin
    FSpW := Ord(FSpeedButton.Visible)* (FSpeedButton.Width);       
    FPanel.SetBounds(AWidth - 4 - FUpDown.Width - FSpW - 3,        
    								 0,FUpDown.Width + FSpW + 3,AHeight - 3 - GuageHeight - 1 );     
    FUpDown.Height := FPanel.Height - 2;
    FSpeedButton.Height := FPanel.Height - 2;
  end;
  if Assigned(ProgressBar) then
    ProgressBar.SetBounds(0,AHeight - 4 - GuageHeight - 1,AWidth - 4, GuageHeight + 2 );
	inherited SetBounds(ALeft,ATop,AWidth,AHeight);
end;

constructor TBMDSpinEdit.Create(AOwner: TComponent);
var
	Panel: TPanel;
begin
  inherited;
  FTrackBarEnabled := True;
  FEditorEnabled := True;
  FTrackBarOrientation := trHorizontal;
  FTrackBarWidth := 150;
  FMaxValue := 100;
  FMinValue := 0;
  FPrecision := 2;
  FGuageHeight := 5;
  FGaugeMinValue:=0.0;
  FGaugeMaxValue:=0.0;
  InChanging := false;
  FWrap := false;
  FValueUnit := '';
  FPanel := TPanel.Create(Self);
  with FPanel do begin
    Parent := Self;
    BevelOuter := bvNone;
    Color := clBtnFace;
  end;
  ProgressBar := TBMDGauge.Create(Self);
  ProgressBar.Parent := Self;
  FUpDown := TUpDown.Create(FPanel);
  with FUpDown do begin
  	Left := 1;
  	Top := 1;
  	Width := 13;
  	Height := 17;
  	Visible := true;
  	Parent := FPanel;
  	OnClick := UpDownClick;
  	Wrap := true;
  	Min := 0;
  	Max := 100;
  	Position := 1;
  	Increment := 1;
  end;
  FSpeedButton := TSpeedButton.Create(FPanel);
  with FSpeedButton do begin
  	Parent := FPanel;
  	Top := 1;
  	Left := 13 + 1 + 1;
  	Visible := true;
  	Width := 13;
  	NumGlyphs := 1;
  	Invalidate;
  	OnMouseDown := SpeedButtonMouseDown;
  	OnMouseUp := SpeedButtonMouseUp;
  	OnMouseMove := SpeedButtonMouseMove;
  end;
  Form := TPopupForm.CreateNew(Self);
  Form.OnShow := FormShow;
  Form.Position := poDesigned;
  Panel := TPanel.Create(Form);
  with Panel do begin
  	Parent := Form;
  	BorderWidth := BORDER_WIDTH;
  	Align := alClient;
  end;
  TrackBar := TTrackBar.Create(Panel);
  with TrackBar do begin
  	Parent := Panel;
  	Max := 1000;
  	Min := 0;
  	Frequency := 50;
  	TabStop := false;
	  FTrackBarHeight := Height - 10;
  	Align := alClient;
  	ThumbLength := 15;
  	TickMarks := tmTopLeft;
  end;
//  Form.BorderStyle := bsNone;
  Form.FormStyle := fsStayOnTop;
  SetButtonHandle;
  Cursor := crArrow;
  Value := 0;
  Increment := 1;
end;

destructor TBMDSpinEdit.Destroy();
begin
  TrackBar.Free();
  Form.Free();
  FSpeedButton.Free();
  FUpDown.Free();
  ProgressBar.Free();
  FPanel.Free();
  inherited;
end;                                             

procedure TPopupForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
    Params.Style := Params.Style and not ( WS_POPUP or WS_CAPTION or WS_SIZEBOX );
    Params.Style := Params.Style or WS_CHILD;
    Params.ExStyle := Params.ExStyle or WS_EX_PALETTEWINDOW;
end;

procedure TPopupForm.CreateWnd();
begin
  inherited;
  Windows.SetParent( Handle, GetDesktopWindow());
end;

end.

