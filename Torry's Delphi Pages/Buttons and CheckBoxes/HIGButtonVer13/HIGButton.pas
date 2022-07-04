//****************************************************************************//
//  THIGButton is a direct descendant of TButton                              //
//  THIGSpeedButton is a direct descendant of TSpeedButton                    //
//  The purpose is to make an alternative to existing TBitBtn and TSpeedButton//
//                                                                            //
//  made by Indra Gunawan, 2005-2007                                          //
//                                                                            //
//  Feel free to use or distribute,                                           //
//  as long as it is still recognize as THIGButton and TSppedButton           //
//                                                                            //
//  If you made any modification, please don't forget to give it comment      //
//  and send me the modified source to                                        //
//                                                                            //
//  indrahighlander@yahoo.com                                                 //
//                                                                            //
//  Any comment and suggestion can also be sent to that email address         //
//                                                                            //
//  Currently, it is for Delphi 6, 7 and 2006,                                //
//  if anyone can make it for other versions of Delphi,                       //
//  don't hesitate to do it, and don't forget to notice me                    //
//                                                                            //
//  Let's make a difference to the better                                     //
//****************************************************************************//

//****************************************************************************//
//  THIGButton                                                                //
//  Current Version: 1.3                                                      //
//  - support gradient color                                                  //
//  - in 3D mode, the border color follows button face color                  //
//  - added ControlOnTop, ControlOnLeft, ControlOnRight and ControlOnBottom.  //
//    This is useful for changing fokus to next control with arrow keys       //

//  Current Version: 1.2                                                      //
//  Changes in version 1.2 :                                                  //
//  - drop down menu alignment property determines                            //
//    its popup position on button                                            //
//  - (Suggested by Nick Kitchener; I thought it looks cooler that way)       //
//    when Kind = bkDropDownButton and themes are enabled,                    //
//    the drop down button is drawn like a combo box down arrow               //
//    inside the button                                                       //
//                                                                            //
//  Changes in version 1.1 :                                                  //
//  - EnterAsTab : Pressing Enter will change focus to next control,          //
//                 use Space for clicking                                     //
//  - LookStyle:                                                              //
//    - DownColor, DownTextColor : color change when button pressed           //
//    - HighlightOnFocused : button become highlighted when focused           //
//  - DropdownMenu, 2 new Kinds: bkDropDown, bkDropDownButton                 //
//      DropDownMenu will be popup when button pressed                        //
//****************************************************************************//

//****************************************************************************//
//  THIGSpeedButton                                                           //
//  Current version: 1                                                        //
//  - New style, bfsBorderless. No border drawn in this style.                //
//    Style will be in effect with combination Flat = true                    //
//  - Drop down menu                                                          //
//  - Hot Glyph                                                               //
//****************************************************************************//

unit HIGButton;

{$S-,W-,R-,H+,X+}
{$C PRELOAD}

interface

uses
  Windows, Messages, Classes, Controls, Forms, Graphics, StdCtrls,
  ExtCtrls, CommCtrl, Buttons, Menus, HIGUtils, SysUtils, ActnList, ImgList,
  Themes;

type
  THIGStyle = (hls3D, hlsFlat, hlsUltraFlat);
  THIGBtnKind = (bkCustom, bkOK, bkCancel, bkHelp, bkYes, bkNo, bkClose,
    bkAbort, bkRetry, bkIgnore, bkAll, bkDropDown, bkDropDownButton);
  THIGFocusRectStyle = (frsStandard, frsRectangle);

  THIGCustomLookStyle = class(TPersistent)
  private
    FOwner: TPersistent;
    FHighlightColor: TColor;
    FButtonColor: TColor;
    FBorderColor: TColor;
    FHighlightTextColor: TColor;
    FStyle: THIGStyle;
    FLightColor: TColor;
    FDarkColor: TColor;
    FDownColor: TColor;
    FDownTextColor: TColor;
    FBorderLightColor: TColor;
    FHighlightTrack: boolean;
    FHighlightOnFocused: boolean;
    FGradient: Boolean;
    FAlwaysShowBorder: Boolean;
    FGradStartColor: TColor;
    FGradStartHighlightColor: TColor;
    FGradStartDownColor: TColor;
    procedure SetButtonColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetStyle(const Value: THIGStyle);
    procedure SetGradient(const Value: Boolean);
    procedure SetAlwaysShowBorder(const Value: Boolean);
    procedure SetGradStartColor(const Value: TColor);
    procedure SetGradStartDownColor(const Value: TColor);
    procedure SetGradStartHighlightColor(const Value: TColor);
  protected
    function GetOwner: TPersistent; override;
    property AlwaysShowBorder: Boolean read FAlwaysShowBorder write SetAlwaysShowBorder default False;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clWindowFrame;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default clBtnFace;
    property DownColor: TColor read FDownColor write FDownColor default clDefault;
    property DownTextColor: TColor read FDownTextColor write FDownTextColor default clDefault;
    property Gradient: Boolean read FGradient write SetGradient default False;
    property GradStartColor: TColor read FGradStartColor write SetGradStartColor default clDefault;
    property GradStartDownColor: TColor read FGradStartDownColor write SetGradStartDownColor default clDefault;
    property GradStartHighlightColor: TColor read FGradStartHighlightColor write SetGradStartHighlightColor default clDefault;
    property HighlightColor: TColor read FHighlightColor write FHighlightColor default clDefault;
    property HighlightOnFocused: boolean read FHighlightOnFocused write FHighlightOnFocused default True;
    property HighlightTextColor: TColor read FHighlightTextColor write FHighlightTextColor default clDefault;
    property HighlightTrack: boolean read FHighlightTrack write FHighlightTrack default True;
    property Style: THIGStyle read FStyle write SetStyle default hlsUltraFlat;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TPersistent); virtual;
    property DarkColor: TColor read FDarkColor write FDarkColor;
    property LightColor: TColor read FLightColor write FLightColor;
    property BorderLightColor: TColor read FBorderLightColor write FBorderLightColor;
  end;

  THIGLookStyle = class(THIGCustomLookStyle)
  published
    property AlwaysShowBorder;
    property BorderColor;
    property ButtonColor;
    property DownColor;
    property DownTextColor;
    property Gradient;
    property GradStartColor;
    property GradStartDownColor;
    property GradStartHighlightColor;
    property HighlightColor;
    property HighlightOnFocused;
    property HighlightTextColor;
    property HighlightTrack;
    property Style;
  end;

  THIGButton = class(TButton)
  private
    FCanvas: TCanvas;
    FGlyph: Pointer;
    FKind: THIGBtnKind;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    IsFocused: Boolean;
    FModifiedGlyph: Boolean;
    FMouseInControl: Boolean;
    FLookStyle: THIGLookStyle;
    FEnterAsTab: boolean;
    FDropDownMenu: TPopupMenu;
    FMenuVisible: Boolean;
    FIsMouseClick: Boolean;
    FLockDown: Boolean;
    FDropDownButtonWidth: integer;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnBeforeDropDownMenu: THIGBeforeDropDownMenu;
    FCtrlOnRight: TWinControl;
    FCtrlOnLeft: TWinControl;
    FCtrlOnTop: TWinControl;
    FCtrlOnBottom: TWinControl;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure SetGlyph(Value: TBitmap);
    function GetGlyph: TBitmap;
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure GlyphChanged(Sender: TObject);
    function IsCustom: Boolean;
    function IsCustomCaption: Boolean;
    procedure SetKind(Value: THIGBtnKind);
    function GetKind: THIGBtnKind;
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);
      message WM_LBUTTONDBLCLK;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CMCloseUpDropDownMenu(var Message: TMessage); message IG_CLOSEUPDROPDOWNMENU;
    procedure CMDropDownMenuPopup(var Message: TMessage); message IG_DROPDOWNMENUPOPUP;
    procedure WMLButtonDown(var Message: TMessage); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TMessage); message WM_LBUTTONUP;
    procedure CMButtonInvalidate(var Message: TMessage); message IG_BTNINVALIDATE;
    procedure CMButtonColorChange(var Message: TMessage); message IG_BTNCOLORCHANGE;
    procedure SetLookStyle(const Value: THIGLookStyle);
    procedure DoDropDownMenu;
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure SetCtrlOnBottom(const Value: TWinControl);
    procedure SetCtrlOnLeft(const Value: TWinControl);
    procedure SetCtrlOnRight(const Value: TWinControl);
    procedure SetCtrlOnTop(const Value: TWinControl);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPalette: HPALETTE; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Action;
    property Anchors;
    property BiDiMode;
    property Cancel stored IsCustom;
    property Caption stored IsCustomCaption;
    property Constraints;
    property ControlOnBottom: TWinControl read FCtrlOnBottom write SetCtrlOnBottom;
    property ControlOnLeft: TWinControl read FCtrlOnLeft write SetCtrlOnLeft;
    property ControlOnRight: TWinControl read FCtrlOnRight write SetCtrlOnRight;
    property ControlOnTop: TWinControl read FCtrlOnTop write SetCtrlOnTop;
    property Default stored IsCustom;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property Enabled;
    property EnterAsTab: boolean read FEnterAsTab write FEnterAsTab default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustom;
    property Kind: THIGBtnKind read GetKind write SetKind default bkCustom;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property LookStyle: THIGLookStyle read FLookStyle write SetLookStyle;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ModalResult stored IsCustom;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs stored IsCustom default 1;
    property ParentShowHint;
    property ParentBiDiMode;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnBeforeDropDownMenu: THIGBeforeDropDownMenu read FOnBeforeDropDownMenu
      write FOnBeforeDropDownMenu;
    property OnEnter;
    property OnExit;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

  THIGSpeedBtnFlatStyle = (bfsStandard, bfsBorderless);

  THIGSpeedButton = class(TSpeedButton)
  private
    FFlatStyle: THIGSpeedBtnFlatStyle;
    FGlyph: Pointer;
    FHotGlyph: Pointer;
    FDropDownMenu: TPopupMenu;
    FMenuVisible: boolean;
    FOnBeforeDropDownMenu: THIGBeforeDropDownMenu;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);
      message WM_LBUTTONDBLCLK;
    procedure HotGlyphChanged(Sender: TObject);
    procedure SetFlatStyle(const Value: THIGSpeedBtnFlatStyle);
    function GetHotGlyph: TBitmap;
    function GetNumHotGlyphs: TNumGlyphs;
    procedure SetHotGlyph(const Value: TBitmap);
    procedure SetNumHotGlyphs(Value: TNumGlyphs);
    procedure DoDropDownMenu;
    procedure SetDropDownMenu(const Value: TPopupMenu);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property FlatStyle: THIGSpeedBtnFlatStyle read FFlatStyle write SetFlatStyle
      default bfsStandard;
    property HotGlyph: TBitmap read GetHotGlyph write SetHotGlyph;
    property NumHotGlyphs: TNumGlyphs read GetNumHotGlyphs write SetNumHotGlyphs default 1;
    property OnBeforeDropDownMenu: THIGBeforeDropDownMenu read FOnBeforeDropDownMenu
      write FOnBeforeDropDownMenu;
  end;

procedure Register;

implementation

{$R HIGButton.res}

procedure Register;
begin
  RegisterComponents('HIG', [THIGButton, THIGSpeedButton]);
end;

{ TBitBtn data }
var
  BitBtnResNames: array[THIGBtnKind] of PChar = (
    nil, 'HIGBBOK', 'HIGBBCANCEL', 'HIGBBHELP', 'HIGBBYES', 'HIGBBNO', 'HIGBBCLOSE',
    'HIGBBABORT', 'HIGBBRETRY', 'HIGBBIGNORE', 'HIGBBALL', nil, nil);
  BitBtnCaptions: array[THIGBtnKind] of ShortString = (
    '', 'OK', 'Cancel', '&Help', '&Yes', '&No', '&Close', 'Abort', '&Retry',
    '&Ignore', '&All', '', '');
  BitBtnModalResults: array[THIGBtnKind] of TModalResult = (
    0, mrOk, mrCancel, 0, mrYes, mrNo, 0, mrAbort, mrRetry, mrIgnore,
    mrAll, 0, 0);

var
  BitBtnGlyphs: array[THIGBtnKind] of TBitmap;

function GetBitBtnGlyph(Kind: THIGBtnKind): TBitmap;
begin
  if BitBtnGlyphs[Kind] = nil then
  begin
    BitBtnGlyphs[Kind] := TBitmap.Create;
    BitBtnGlyphs[Kind].LoadFromResourceName(HInstance, BitBtnResNames[Kind]);
  end;
  Result := BitBtnGlyphs[Kind];
end;

type
  TGlyphList = class(TImageList)
  private
    Used: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;
    
  TGlyphCache = class
  private
    GlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
    function Empty: Boolean;
  end;
    
  TButtonGlyph = class
  private
    FOriginal: TBitmap;
    FGlyphList: TGlyphList;
    FIndexs: array[TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    FBackColor: TColor;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
    function CreateButtonGlyph(State: TButtonState;
      DisabledDarkColor, DisabledLightColor: TColor): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean;
      DisabledDarkColor, DisabledLightColor: TColor);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState; BiDiFlags: Longint;
      WordWrap: boolean; ADisabledTextColor: TColor);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
      BiDiFlags: Longint; WordWrap: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean; BiDiFlags: Longint;
      WordWrap: boolean; ADisabledTextColor: TColor;
      DisabledDarkColor, DisabledLightColor: TColor): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property BackColor: TColor read FBackColor write FBackColor; //by HIG
  end;

{ TGlyphList }

constructor TGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
end;
    
destructor TGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;

function TGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;
    
function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;
    
procedure TGlyphList.Delete(Index: Integer);
begin
  if Used[Index] then
  begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;
    
{ TGlyphCache }
    
constructor TGlyphCache.Create;
begin
  inherited Create;
  GlyphLists := TList.Create;
end;
    
destructor TGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited Destroy;
end;
    
function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do
  begin
    Result := GlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  GlyphLists.Add(Result);
end;
    
procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then
  begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;
    
function TGlyphCache.Empty: Boolean;
begin
  Result := GlyphLists.Count = 0;
end;
    
var
  GlyphCache: TGlyphCache = nil;
  ButtonCount: Integer = 0;

{ TButtonGlyph }
    
constructor TButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FBackColor:= clBtnFace;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
end;
    
destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;
    
procedure TButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexs[I] <> -1 then FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;
    
procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;
    
procedure TButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;

procedure TButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
  end;
end;
    
function TButtonGlyph.CreateButtonGlyph(State: TButtonState;
  DisabledDarkColor, DisabledLightColor: TColor): Integer;
const
  ROP_DSPDxax = $00E20746;
var
  TmpImage, DDB, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TButtonState;
  DestDC: HDC;
begin
  if (State = bsDown) and (NumGlyphs < 3) then State := bsUp;
  Result := FIndexs[State];
  if (State <> bsDisabled) and (Result <> -1) then Exit;
  if (State = bsDisabled) and (Result <> -1) and (FGlyphList <> nil) then
     FGlyphList.Delete(Result);
  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := FBackColor;//clBtnFace;
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    I := State;
    if Ord(I) >= NumGlyphs then I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown,
      bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          if FOriginal.TransparentMode = tmFixed then
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor)
          else
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
      bsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(FOriginal);
            DDB.HandleType := bmDDB;
            if NumGlyphs > 1 then
            with TmpImage.Canvas do
            begin    { Change white & gray to clBtnHighlight and clBtnShadow }
              CopyRect(IRect, DDB.Canvas, ORect);
              MonoBmp.Monochrome := True;
              MonoBmp.Width := IWidth;
              MonoBmp.Height := IHeight;

              { Convert white to clBtnHighlight }
              DDB.Canvas.Brush.Color := clWhite;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := DisabledLightColor;//clBtnHighlight;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert gray to clBtnShadow }
              DDB.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := DisabledDarkColor;//clBtnShadow;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert transparent color to clBtnFace }
              DDB.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := FBackColor;//clBtnFace;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
                HandleType := bmDDB;
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := FBackColor;//clBtnFace;
                FillRect(IRect);
                Brush.Color := DisabledLightColor;//clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := DisabledDarkColor;//clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

procedure TButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean;
  DisabledDarkColor, DisabledLightColor: TColor);
var
  Index: Integer;
  Details: TThemedElementDetails;
  R: TRect;
  Button: TThemedButton;
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State, DisabledDarkColor, DisabledLightColor);
  with GlyphPos do
  begin
    if ThemeServices.ThemesEnabled then
    begin
      R.TopLeft := GlyphPos;
      R.Right := R.Left + FOriginal.Width div FNumGlyphs;
      R.Bottom := R.Top + FOriginal.Height;
      case State of
        bsDisabled:
          Button := tbPushButtonDisabled;
        bsDown,
        bsExclusive:
          Button := tbPushButtonPressed;
      else
        // bsUp
        Button := tbPushButtonNormal;
      end;
      Details := ThemeServices.GetElementDetails(Button);
      ThemeServices.DrawIcon(Canvas.Handle, Details, R, FGlyphList.Handle, Index);
    end
    else
      if Transparent or (State = bsExclusive) then
      begin
        ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
          clNone, clNone, ILD_Transparent)
      end
      else
        ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
          ColorToRGB(FBackColor), clNone, ILD_Normal);
  end;
end;

procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; BiDiFlags: LongInt; WordWrap: boolean;
  ADisabledTextColor: TColor);
var
  aFlag: LongInt;
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if WordWrap then
      aFlag:= DT_CENTER or DT_VCENTER or BiDiFlags or DT_WORDBREAK
    else
      aFlag:= DT_CENTER or DT_VCENTER or BiDiFlags;
    if State = bsDisabled then
      Font.Color := ADisabledTextColor;
    DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, aFlag);
  end;
end;

procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
  BiDiFlags: LongInt; WordWrap: boolean);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
  aFlag: Longint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else
      if Layout = blGlyphRight then Layout := blGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height) else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    if WordWrap then
      aFlag:= DT_CALCRECT or BiDiFlags or DT_WORDBREAK
    else
      aFlag:= DT_CALCRECT or BiDiFlags;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, aFlag);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;
    
  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;
    
  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;
    
  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;
    
  { fixup the result variables }
  with GlyphPos do
  begin
    Inc(X, Client.Left + Offset.X);
    Inc(Y, Client.Top + Offset.Y);
  end;

  { Themed text is not shifted, but gets a different color. }
  if ThemeServices.ThemesEnabled then
    OffsetRect(TextBounds, TextPos.X + Client.Left, TextPos.Y + Client.Top)
  else
    OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.Y);

end;
    
function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
  BiDiFlags: LongInt; WordWrap: boolean; ADisabledTextColor: TColor;
  DisabledDarkColor, DisabledLightColor: TColor): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result, BiDiFlags, WordWrap);
  DrawButtonGlyph(Canvas, GlyphPos, State, Transparent, DisabledDarkColor, DisabledLightColor);
  DrawButtonText(Canvas, Caption, Result, State, BiDiFlags, WordWrap, ADisabledTextColor);
end;

{ THIGButton }
type
  TMenuItemAccess = class(TMenuItem);

procedure THIGButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;//! for lack of a better color
      Canvas.FillRect(Rect(0,0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
    end;
  end;

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      { Copy image from action's imagelist }
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;

procedure DestroyLocals; far;
var
  I: THIGBtnKind;
begin
  for I := Low(THIGBtnKind) to High(THIGBtnKind) do
    BitBtnGlyphs[I].Free;
end;

procedure THIGButton.Click;
var
  Form: TCustomForm;
  Control: TWinControl;
begin
  if (Kind = bkDropDown) and not FMenuVisible then
  begin
    if not FIsMouseClick then DoDropDownMenu;
    FIsMouseClick := False;
  end
  else
  begin
    if FIsMouseClick then
      FIsMouseClick:= False
    else
    case FKind of
      bkClose:
        begin
          Form := GetParentForm(Self);
          if Form <> nil then Form.Close
          else inherited Click;
        end;
      bkHelp:
        begin
          Control := Self;
          while (Control <> nil) and (Control.HelpContext = 0) do
            Control := Control.Parent;
          if Control <> nil then Application.HelpContext(Control.HelpContext)
          else inherited Click;
        end;
      else
        inherited Click;
    end;
  end;
end;

procedure THIGButton.CMButtonColorChange(var Message: TMessage);
begin
  SetGlyph(Glyph);
  Invalidate;
end;

procedure THIGButton.CMButtonInvalidate(var Message: TMessage);
begin
  Invalidate;
end;

procedure THIGButton.CMCloseUpDropDownMenu(var Message: TMessage);
var
  P: TPoint;
begin
  GetCursorPos(P);
  FMouseInControl := FLookStyle.HighlightTrack and (WindowFromPoint(P) = Handle);
  FMenuVisible := False;
  Repaint;
  FLockDown := False;
end;

procedure THIGButton.CMDropDownMenuPopup(var Message: TMessage);

  function GetDropDownMenuHeight: Integer;
  var
    ANonClientMetrics: TNonClientMetrics;
    I, AHeight, AWidth: Integer;
    MI: TMenuItemAccess;
    B: Boolean;
    ACanvas: TCanvas;
  begin
    ANonClientMetrics.cbSize := SizeOf(TNonClientMetrics);
    B := SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @ANonClientMetrics, 0);
    if B then
      Result := ANonClientMetrics.iBorderWidth * 2
    else
      Result := 2;
    ACanvas:= TCanvas.Create;
    with ACanvas do
    try
      Font.Assign(Screen.MenuFont);
      for I := 0 to FDropDownMenu.Items.Count - 1 do
      begin
        MI := TMenuItemAccess(FDropDownMenu.Items[i]);
        AHeight:= ANonClientMetrics.iMenuHeight;
        if FDropDownMenu.OwnerDraw and MI.Visible then
          MI.MeasureItem(ACanvas, AWidth, AHeight);
        Inc(Result, AHeight);
      end;
    finally
      ACanvas.Free;
    end;
  end;

var
  P: TPoint;
  H, X, Y: Integer;
begin
  if (Kind in [bkDropDown, bkDropDownButton]) and (FDropDownMenu <> nil) then
  begin
    FMenuVisible := True;
    Repaint;
    H:= GetDropDownMenuHeight;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    X := P.X;
    if FDropDownMenu.Alignment = paRight then
      X:= X + Width
    else if FDropDownMenu.Alignment = paCenter then
      X:= X + (Width div 2);
    if X < 0 then X:= 0;
    if Y + H > Screen.Height then Y := P.Y - H;
    if Y < 0 then Y:= 0;
    FDropDownMenu.Popup(X, Y);
    PostMessage(Self.Handle, IG_CLOSEUPDROPDOWNMENU, 0, 0);
  end;
end;

procedure THIGButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then FMouseInControl := False;
  Invalidate;
end;

procedure THIGButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure THIGButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (ThemeServices.ThemesEnabled or FLookStyle.HighlightTrack) and
     not FMouseInControl and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    Repaint;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure THIGButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (ThemeServices.ThemesEnabled or FLookStyle.HighlightTrack) and FMouseInControl then
  begin
    FMouseInControl := False;
    Repaint;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure THIGButton.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;

procedure THIGButton.CNKeyDown(var Message: TWMKeyDown);
begin
  if (Message.CharCode = VK_RETURN) and FEnterAsTab then
    Message.CharCode:= VK_TAB;
  case Message.CharCode of
    VK_RETURN:
      if FEnterAsTab then
        Message.CharCode:= VK_TAB;
    VK_LEFT:
      if (FCtrlOnLeft <> nil) and FCtrlOnLeft.CanFocus then
      begin
        FCtrlOnLeft.SetFocus;
        Message.Result := 1;
        Exit;
      end;
    VK_RIGHT:
      if (FCtrlOnRight <> nil) and FCtrlOnRight.CanFocus then
      begin
        FCtrlOnRight.SetFocus;
        Message.Result := 1;
        Exit;
      end;
    VK_UP:
      if (FCtrlOnTop <> nil) and FCtrlOnTop.CanFocus then
      begin
        FCtrlOnTop.SetFocus;
        Message.Result := 1;
        Exit;
      end;
    VK_DOWN:
      if (FCtrlOnBottom <> nil) and FCtrlOnBottom.CanFocus then
      begin
        FCtrlOnBottom.SetFocus;
        Message.Result := 1;
        Exit;
      end;
  end;
  inherited;
end;

procedure THIGButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
end;

constructor THIGButton.Create(AOwner: TComponent);
begin
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  inherited Create(AOwner);
  FCanvas := TCanvas.Create;
  FKind := bkCustom;
  FLayout := blGlyphLeft;
  FSpacing := 4;
  FMargin := -1;
  FEnterAsTab:= False;
  ControlStyle := ControlStyle + [csReflector];
  DoubleBuffered := True;
  FLookStyle:= THIGLookStyle.Create(Self);
  FMenuVisible:= False;
  FIsMouseClick:= False;
  FLockDown := False;
  FDropDownButtonWidth:= 15;
  FCtrlOnBottom:= nil;
  FCtrlOnLeft:= nil;
  FCtrlOnRight:= nil;
  FCtrlOnTop:= nil;
end;

procedure THIGButton.CreateHandle;
var
  State: TButtonState;
begin
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  inherited CreateHandle;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State, FLookStyle.DarkColor, FLookStyle.LightColor);
end;

procedure THIGButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end;

destructor THIGButton.Destroy;
begin
  inherited Destroy;
  TButtonGlyph(FGlyph).Free;
  FreeAndNil(FCanvas);
  FreeAndNil(FLookStyle);
end;

procedure THIGButton.DoDropDownMenu;
var
  ADropDown: boolean;
begin
  ADropDown:= True;
  if Assigned(FOnBeforeDropDownMenu) then
    FOnBeforeDropDownMenu(Self, ADropDown);
  if not ADropDown then
    Exit;   
  FLockDown := True;
  PostMessage(Self.Handle, IG_DROPDOWNMENUPOPUP, 0, 0);
end;

procedure THIGButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown, IsDefault: Boolean;
  State: TButtonState;
  R, BtnR: TRect;
  Details, DetArrow: TThemedElementDetails;
  Button: TThemedButton;
  DDArrow: TThemedComboBox;
  Offset: TPoint;
  AColor, ABorderColor, AHighlightColor, AHighlightTextColor: TColor;
  ADownColor, ADownTextColor: TColor;
  ABtnKind: THIGBtnKind;
  DivX, DivY: Integer;
  AFlatLightColor, AFlatDarkColor: TColor;
  ATempColor: TColor;
  ATempR: TRect;
  A3DLighter, A3DDarker: TColor;
  AGSColor, AGSDownColor, AGSHighLightColor: TColor;
begin
  ABtnKind:= Kind;
  FCanvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;
  with DrawItemStruct do
  begin
    FCanvas.Handle := hDC;
    FCanvas.Font := Self.Font;
    if ABtnKind = bkDropDown then
      IsDown:= FMenuVisible
    else
    begin
      if not FLockDown then
        IsDown := (ItemState and ODS_SELECTED <> 0)
      else
        IsDown := False;
    end;
    IsDefault := (ItemState and ODS_FOCUS <> 0);

    if not Enabled then State := bsDisabled
    else if (not FLockDown and IsDown) then State := bsDown
    else State := bsUp;
  end;

  if (FLookStyle.ButtonColor = clDefault) or (FLookStyle.ButtonColor = clNone) then
    AColor:= clBtnFace
  else
    AColor:= FLookStyle.ButtonColor;
  if (FLookStyle.BorderColor = clDefault) or (FLookStyle.BorderColor = clNone) then
    ABorderColor:= clWindowFrame
  else
    ABorderColor:= FLookStyle.BorderColor;
  if FLookStyle.HighlightTrack or FLookStyle.HighlightOnFocused then
  begin
    if (FLookStyle.HighlightColor = clDefault) or (FLookStyle.HighlightColor = clNone) then
      AHighlightColor:= FLookStyle.LightColor
    else
      AHighlightColor:= FLookStyle.HighlightColor;
    if (FLookStyle.HighlightTextColor = clDefault) or
       (FLookStyle.HighlightTextColor = clNone) then
      AHighlightTextColor:= Self.Font.Color
    else
      AHighlightTextColor:= FLookStyle.HighlightTextColor;
  end
  else
  begin
    AHighlightColor:= AColor;
    AHighlightTextColor:= Self.Font.Color;
  end;
  if (FLookStyle.DownColor = clDefault) or (FLookStyle.DownColor = clNone) then
    ADownColor:= FLookStyle.DarkColor
  else
    ADownColor:= FLookStyle.DownColor;
  if (FLookStyle.DownTextColor = clDefault) or (FLookStyle.DownTextColor = clNone) then
    ADownTextColor:= FLookStyle.LightColor
  else
    ADownTextColor:= FLookStyle.DownTextColor;
  if AColor = clBtnFace then
  begin
    AFlatLightColor:= clBtnHighlight;
    AFlatDarkColor:= clBtnShadow;
  end
  else
  begin
    AFlatLightColor:= FLookStyle.LightColor;
    AFlatDarkColor:= FLookStyle.DarkColor;
  end;

  if (FLookStyle.GradStartColor = clDefault) or (FLookStyle.GradStartColor = clNone) then
    AGSColor:= GetLightenColor(GetLightenColor(AColor))
  else
    AGSColor:= FLookStyle.GradStartColor;
  if (FLookStyle.GradStartDownColor = clDefault) or (FLookStyle.GradStartDownColor = clNone) then
    AGSDownColor:= GetLightenColor(GetLightenColor(ADownColor))
  else
    AGSDownColor:= FLookStyle.GradStartDownColor;
  if (FLookStyle.GradStartHighlightColor = clDefault) or (FLookStyle.GradStartHighlightColor = clNone) then
    AGSHighLightColor:= GetLightenColor(GetLightenColor(AHighlightColor))
  else
    AGSHighLightColor:= FLookStyle.GradStartHighlightColor;

  if ThemeServices.ThemesEnabled then
  begin
    if ABtnKind = bkDropDownButton then
      BtnR := Rect(R.Right - FDropDownButtonWidth - 3, R.Top + 4, R.Right - 3, R.Bottom - 4);

    if not Enabled then
      Button := tbPushButtonDisabled
    else
      if IsDown then
        Button := tbPushButtonPressed
      else
        if FMouseInControl then
          Button := tbPushButtonHot
        else
          if IsFocused or IsDefault then
            Button := tbPushButtonDefaulted
          else
            Button := tbPushButtonNormal;

    Details := ThemeServices.GetElementDetails(Button);
    // Parent background.
    ThemeServices.DrawParentBackground(Handle, DrawItemStruct.hDC, @Details, True);
    // Button shape.
    ThemeServices.DrawElement(DrawItemStruct.hDC, Details, R);
    R := ThemeServices.ContentRect(FCanvas.Handle, Details, R);

    if (ABtnKind = bkDropDownButton) then
    begin
      //suggested by Nick Kitchener
      if not Enabled then
        DDArrow := tcDropDownButtonDisabled
      else if IsDown or FMenuVisible then
        DDArrow := tcDropDownButtonPressed
      else if FMouseInControl then
        DDArrow := tcDropDownButtonHot
      else
        DDArrow := tcDropDownButtonNormal;
      DetArrow := ThemeServices.GetElementDetails(DDArrow);

      ThemeServices.DrawElement(DrawItemStruct.hDC, DetArrow, BtnR);
      BtnR := ThemeServices.ContentRect(FCanvas.Handle, DetArrow, BtnR);
    end;

    if Button = tbPushButtonPressed then
      Offset := Point(1, 0)
    else
      Offset := Point(0, 0);

    if ABtnKind = bkDropDownButton then
    begin
      CopyRect(ATempR, R);
      R.Right:= BtnR.Left + 2;
    end;
    TButtonGlyph(FGlyph).BackColor:= AColor;
    TButtonGlyph(FGlyph).Draw(FCanvas, R, Offset, Caption, FLayout, FMargin, FSpacing, State, False,
      DrawTextBiDiModeFlags(0), WordWrap, FLookStyle.DarkColor, FLookStyle.DarkColor, FLookStyle.LightColor);
    if ABtnKind = bkDropDownButton then
      CopyRect(R, ATempR);

    if ((ABtnKind <> bkDropDownButton) or not FMenuVisible)
       and IsFocused and IsDefault then
    begin
      FCanvas.Pen.Color := ABorderColor;
      FCanvas.Brush.Color := AColor;
      DrawFocusRect(FCanvas.Handle, R);
    end;
  end
  else
  begin
    if ABtnKind = bkDropDownButton then
    begin
      BtnR := Rect(R.Right - FDropDownButtonWidth, R.Top, R.Right, R.Bottom);
      if FLookStyle.Style = hls3D then
        R.Right := BtnR.Left + 1
      else
        R.Right := BtnR.Left + 2;
      if (FLookStyle.Style <> hlsUltraFlat) and FLookStyle.AlwaysShowBorder then
        R.Right := R.Right + 1;
    end;

    {Default rectangle drawed here}
    if (IsFocused or IsDefault) or
       ((FLookStyle.Style <> hlsUltraFlat) and FLookStyle.AlwaysShowBorder) then
    begin
      FCanvas.Pen.Color := ABorderColor;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Color:= AColor;
      FCanvas.Brush.Style := bsClear;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
      if ABtnKind = bkDropDownButton then
      begin
        FCanvas.Rectangle(BtnR.Left, BtnR.Top, BtnR.Right, BtnR.Bottom);
        InflateRect(BtnR, -1, -1);
      end;
    end;

    if (ABtnKind = bkDropDownButton) and FMenuVisible then
    begin
      if FLookStyle.Style = hlsFlat then
      begin
        Frame3D(FCanvas, BtnR, AFlatDarkColor, AFlatLightColor, 1);
        FCanvas.Pen.Color := ADownColor;
      end
      else
        FCanvas.Pen.Color := FLookStyle.BorderLightColor;
      FCanvas.Brush.Color := ADownColor;
      FCanvas.Pen.Width := 1;
      FCanvas.Rectangle(BtnR.Left, BtnR.Top, BtnR.Right, BtnR.Bottom);
      if FLookStyle.Style <> hlsFlat then
        InflateRect(BtnR, -1, -1);
      if FLookStyle.Gradient then
      begin
        CopyRect(ATempR, BtnR);
        DrawGradientColor(FCanvas, ATempR, ADownColor, AGSDownColor);
      end;
    end;

    if IsDown then
    begin
      if FLookStyle.Style = hlsFlat then
      begin
        Frame3D(FCanvas, R, AFlatDarkColor, AFlatLightColor, 1);
        if ABtnKind = bkDropDownButton then
          Frame3D(FCanvas, BtnR, AFlatDarkColor, AFlatLightColor, 1);
        FCanvas.Pen.Color := ADownColor;
      end
      else
        FCanvas.Pen.Color := FLookStyle.BorderLightColor;
      FCanvas.Brush.Color := ADownColor;
      FCanvas.Pen.Width := 1;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      if FLookStyle.Style <> hlsFlat then
        InflateRect(R, -1, -1);
      if FLookStyle.Gradient then
      begin
        CopyRect(ATempR, R);
        DrawGradientColor(FCanvas, ATempR, ADownColor, AGSDownColor);
      end;
      if ABtnKind = bkDropDownButton then
      begin
        FCanvas.Rectangle(BtnR.Left, BtnR.Top, BtnR.Right, BtnR.Bottom);
        if FLookStyle.Style <> hlsFlat then
          InflateRect(BtnR, -1, -1);
        if FLookStyle.Gradient then
        begin
          CopyRect(ATempR, BtnR);
          DrawGradientColor(FCanvas, ATempR, ADownColor, AGSDownColor);
        end;
      end;
    end
    else
    begin
      with FCanvas do
      begin
        case FLookStyle.Style of
          hls3D: begin
              A3DLighter:= GetLightenColor(AColor, 25);
              A3DDarker:= GetDarkenColor(AFlatDarkColor);
              Frame3D(FCanvas, R, AFlatLightColor, A3DDarker, 1);
              Frame3D(FCanvas, R, A3DLighter, AFlatDarkColor, 1);
              //DrawEdge(Handle, R, BDR_RAISEDINNER or BDR_RAISEDOUTER, BF_RECT or BF_ADJUST);
              if (ABtnKind = bkDropDownButton) and not FMenuVisible then
              begin
                Frame3D(FCanvas, BtnR, A3DLighter, A3DDarker, 1);
                Frame3D(FCanvas, BtnR, AFlatLightColor, AFlatDarkColor, 1);
                //DrawEdge(Handle, BtnR, BDR_RAISEDINNER or BDR_RAISEDOUTER, BF_RECT or BF_ADJUST);
              end;
            end;
          hlsFlat: begin
              Frame3D(FCanvas, R, AFlatLightColor, AFlatDarkColor, 1);
              if (ABtnKind = bkDropDownButton) and not FMenuVisible then
                Frame3D(FCanvas, BtnR, AFlatLightColor, AFlatDarkColor, 1);
            end;
          hlsUltraFlat: begin
              //DrawEdge(Handle, R, BDR_RAISEDINNER or BDR_RAISEDOUTER, BF_RECT or BF_MIDDLE or BF_FLAT);
              Frame3D(FCanvas, R, ABorderColor, ABorderColor, 1);
              if not (IsFocused or IsDefault) then
                InflateRect(R, 1, 1);
              if (ABtnKind = bkDropDownButton) and not FMenuVisible then
              begin
                Frame3D(FCanvas, BtnR, ABorderColor, ABorderColor, 1);
                //DrawEdge(Handle, BtnR, BDR_RAISEDINNER or BDR_RAISEDOUTER, BF_RECT or BF_MIDDLE or BF_FLAT);
                if not (IsFocused or IsDefault) then
                  InflateRect(BtnR, 1, 1);
              end;
            end;
        end;
        if ((ABtnKind <> bkDropDownButton) or not FMenuVisible)
           and (FLookStyle.HighlightTrack or FLookStyle.HighlightOnFocused) then
        begin
          if FMouseInControl or (FLookStyle.HighlightOnFocused and IsFocused) then
          begin
            if IsFocused or IsDefault then
              FCanvas.Pen.Color := AHighlightColor
            else
            begin
              if FLookStyle.Style = hlsUltraFlat then
                FCanvas.Pen.Color := ABorderColor
              else
                FCanvas.Pen.Color := AHighlightColor;
            end;
            FCanvas.Brush.Color := AHighlightColor;
            ATempColor:= AGSHighLightColor;
          end
          else
          begin
            if FLookStyle.Style = hlsUltraFlat then
              FCanvas.Pen.Color := ABorderColor
            else
              FCanvas.Pen.Color := AColor;
            FCanvas.Brush.Color := AColor;
            ATempColor:= AGSColor;
          end;
        end
        else
        begin
          if FLookStyle.Style = hlsUltraFlat then
          begin
            if (ABtnKind = bkDropDownButton) and FMenuVisible then
              FCanvas.Pen.Color := AColor
            else
              FCanvas.Pen.Color := ABorderColor;
          end
          else
            FCanvas.Pen.Color := AColor;
          FCanvas.Brush.Color := AColor;
          ATempColor:= AGSColor;
        end;
        FCanvas.Pen.Width := 1;
        if (ABtnKind = bkDropDownButton) and not FMenuVisible then
        begin
          FCanvas.Rectangle(BtnR.Left, BtnR.Top, BtnR.Right, BtnR.Bottom);
          if FLookStyle.Gradient and Enabled then
          begin
            Windows.CopyRect(ATempR, BtnR);
            if FLookStyle.Style = hlsUltraFlat then
              InflateRect(ATempR, -1, -1);
            DrawGradientColor(FCanvas, ATempR, ATempColor, FCanvas.Brush.Color);
          end;
          if IsFocused then
          begin
            FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
            if FLookStyle.Gradient and Enabled then
            begin
              Windows.CopyRect(ATempR, R);
              if FLookStyle.Style = hlsUltraFlat then
                InflateRect(ATempR, -1, -1);
              DrawGradientColor(FCanvas, ATempR, ATempColor, FCanvas.Brush.Color);
            end;
          end
          else
          begin
            //FCanvas.Rectangle(R.Left, R.Top, R.Right-1, R.Bottom);
            if (FLookStyle.Style = hls3D) or
               ((FLookStyle.Style = hlsFlat) and FLookStyle.AlwaysShowBorder) then
              FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom)
            else
              FCanvas.Rectangle(R.Left, R.Top, R.Right-1, R.Bottom);
            if FLookStyle.Gradient and Enabled then
            begin
              Windows.CopyRect(ATempR, R);
              if FLookStyle.Style = hlsUltraFlat then
                InflateRect(ATempR, -1, -1);
              if (FLookStyle.Style <> hls3D) and
                 not ((FLookStyle.Style = hlsFlat) and FLookStyle.AlwaysShowBorder) then
                ATempR.Right:= ATempR.Right - 1;
              DrawGradientColor(FCanvas, ATempR, ATempColor, FCanvas.Brush.Color);
            end;
          end;
          InflateRect(BtnR, -1, -1);
        end
        else
        begin
          FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          if FLookStyle.Gradient and Enabled then
          begin
            Windows.CopyRect(ATempR, R);
            if (FLookStyle.Style = hlsUltraFlat) and
               ((ABtnKind <> bkDropDownButton) or not FMenuVisible) and
               //(FLookStyle.HighlightTrack or FLookStyle.HighlightOnFocused) and
               not (IsFocused or IsDefault) then
              InflateRect(ATempR, -1, -1);
            DrawGradientColor(FCanvas, ATempR, ATempColor, FCanvas.Brush.Color);
          end;
        end;
        InflateRect(R, -1, -1);
      end;
    end;

    if IsFocused then
    begin
      R := ClientRect;
      if ABtnKind = bkDropDownButton then
      begin
        BtnR := Rect(R.Right - FDropDownButtonWidth, R.Top, R.Right, R.Bottom);
        if (FLookStyle.Style = hls3D) and not FLookStyle.AlwaysShowBorder then
          R.Right := BtnR.Left + 1
        else
          R.Right := BtnR.Left + 2;
        if (FLookStyle.Style <> hlsUltraFlat) and FLookStyle.AlwaysShowBorder then
          R.Right := R.Right + 1;
      end;
      InflateRect(R, -1, -1);
    end;

    FCanvas.Font := Self.Font;
    if ((ABtnKind <> bkDropDownButton) or not FMenuVisible) and
       (IsDown or (FLookStyle.HighlightTrack and FMouseInControl)
        or (FLookStyle.HighlightOnFocused and IsFocused)) then
    begin
      if IsDown then
      begin
        OffsetRect(R, 1, 1);
        FCanvas.Font.Color:= ADownTextColor;
      end
      else
        FCanvas.Font.Color:= AHighlightTextColor;
    end
    else
      FCanvas.Font.Color:= Self.Font.Color;

    TButtonGlyph(FGlyph).BackColor:= AColor;
    TButtonGlyph(FGlyph).Draw(FCanvas, R, Point(0,0), Caption, FLayout, FMargin,
      FSpacing, State, FLookStyle.HighlightTrack or IsDown or FLookStyle.HighlightOnFocused,
      DrawTextBiDiModeFlags(0), WordWrap, FLookStyle.DarkColor, FLookStyle.DarkColor, FLookStyle.LightColor);

    if ((ABtnKind <> bkDropDownButton) or not FMenuVisible) and IsFocused and IsDefault then
    begin
      R := ClientRect;
      if ABtnKind = bkDropDownButton then
      begin
        BtnR:= Rect(R.Right - FDropDownButtonWidth, R.Top, R.Right, R.Bottom);
        if FLookStyle.Style = hls3D then
          R.Right := BtnR.Left + 1
        else
          R.Right := BtnR.Left + 2;
        if (FLookStyle.Style <> hlsUltraFlat) and FLookStyle.AlwaysShowBorder then
          R.Right := R.Right + 1;
      end;

      if FLookStyle.Style = hls3D then
        InflateRect(R, -4, -4)
      else
        InflateRect(R, -3, -3);
        
      FCanvas.Pen.Color := ABorderColor;
      if IsDown then
        FCanvas.Brush.Color := ADownColor
      else if (FLookStyle.HighlightTrack and FMouseInControl)
               or (FLookStyle.HighlightOnFocused and IsFocused) then
        FCanvas.Brush.Color := AHighlightColor
      else
        FCanvas.Brush.Color := AColor;
      DrawFocusRect(FCanvas.Handle, R);
    end;
  end;

  if (ABtnKind = bkDropDown) or
     ((ABtnKind = bkDropDownButton) and not ThemeServices.ThemesEnabled) then
  begin
    R := ClientRect;
    BtnR:= Rect(R.Right - FDropDownButtonWidth, R.Top, R.Right, R.Bottom);
    InflateRect(BtnR, -1, -1);
    if ThemeServices.ThemesEnabled then
      Dec(BtnR.Left);
    with BtnR do
    begin
      DivX := Right - Left;
      DivX := DivX div 2;
      DivY := Bottom - Top;
      DivY := DivY div 2;
      Bottom := Bottom - (DivY + DivX div 2) + 1;
      Top := Top + (DivY + DivX div 2) + 1;
      Left := Left + (DivX div 2);
      Right := (Right - DivX div 2);
    end;

    if Enabled then
    begin
      if not ThemeServices.ThemesEnabled and
         ((IsDown or (FLookStyle.HighlightTrack and FMouseInControl)
           or (FLookStyle.HighlightOnFocused and IsFocused))
          or FMenuVisible) then
      begin
        if IsDown or FMenuVisible then
          FCanvas.Pen.Color := ADownTextColor
        else
          FCanvas.Pen.Color := AHighlightTextColor;
      end
      else
        FCanvas.Pen.Color := Self.Font.Color;
    end
    else
      FCanvas.Pen.Color := FLookStyle.DarkColor;
    if not ThemeServices.ThemesEnabled and IsDown then
      OffsetRect(BtnR, 1, 1);
    while BtnR.Left < BtnR.Right + 1 do
    begin
      FCanvas.MoveTo(BtnR.Left, BtnR.Bottom);
      FCanvas.LineTo(BtnR.Right, BtnR.Bottom);
      InflateRect(BtnR, -1, 1);
    end;
  end;

  FCanvas.Handle := 0;
end;

function THIGButton.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

function THIGButton.GetKind: THIGBtnKind;
begin
  if (FKind <> bkCustom) and (not (FKind in [bkDropDown, bkDropDownButton])) then
    if ((FKind in [bkOK, bkYes]) xor Default) or
      ((FKind in [bkCancel, bkNo]) xor Cancel) or
      (ModalResult <> BitBtnModalResults[FKind]) or
      FModifiedGlyph then
      FKind := bkCustom;
  Result := FKind;
end;

function THIGButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;

function THIGButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

procedure THIGButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

function THIGButton.IsCustom: Boolean;
begin
  Result := (Kind in [bkCustom, bkDropDown, bkDropDownButton]);
end;

function THIGButton.IsCustomCaption: Boolean;
begin
  Result := AnsiCompareStr(Caption, BitBtnCaptions[FKind]) <> 0;
end;

procedure THIGButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Kind in [bkDropDown, bkDropDownButton]) and Assigned(FDropDownMenu)
     and (Key in [VK_UP, VK_DOWN])
     and ((ssAlt in Shift) or (ssShift in Shift)) then
  begin
    if not FMenuVisible then DoDropDownMenu;
    Key := 0;
    Exit;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure THIGButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  R: TRect;
begin
  if (Button = mbLeft) and (Kind in [bkDropDown, bkDropDownButton])
     and Assigned(FDropDownMenu) then
  begin
    if not FMenuVisible then
    begin
      R := ClientRect;
      if Kind = bkDropDownButton then
        R.Left := R.Right - FDropDownButtonWidth;
      if PtInRect(R, Point(X, Y)) then
        DoDropDownMenu
    end
    else
    begin
      FIsMouseClick := True;
      FMenuVisible := False;
      Repaint;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure THIGButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = FDropDownMenu) then
      FDropDownMenu := nil
    else if (AComponent = FCtrlOnBottom) then
      FCtrlOnBottom:= nil
    else if (AComponent = FCtrlOnLeft) then
      FCtrlOnLeft:= nil
    else if (AComponent = FCtrlOnRight) then
      FCtrlOnRight:= nil
    else if (AComponent = FCtrlOnTop) then
      FCtrlOnTop:= nil;
  end;
end;

procedure THIGButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Refresh;
  end;
end;

procedure THIGButton.SetCtrlOnBottom(const Value: TWinControl);
begin
  if Value = Self then
    FCtrlOnBottom := nil
  else
    FCtrlOnBottom := Value;
end;

procedure THIGButton.SetCtrlOnLeft(const Value: TWinControl);
begin
  if Value = Self then
    FCtrlOnLeft := nil
  else
    FCtrlOnLeft := Value;
end;

procedure THIGButton.SetCtrlOnRight(const Value: TWinControl);
begin
  if Value = Self then
    FCtrlOnRight := nil
  else
    FCtrlOnRight := Value;
end;

procedure THIGButton.SetCtrlOnTop(const Value: TWinControl);
begin
  if Value = Self then
    FCtrlOnTop := nil
  else
    FCtrlOnTop := Value;
end;

procedure THIGButton.SetDropDownMenu(const Value: TPopupMenu);
begin
  FDropDownMenu := Value;
  if Value <> nil then
  begin
    Value.ParentBiDiModeChanged(Self);
    Value.FreeNotification(Self);
  end;
end;

procedure THIGButton.SetGlyph(Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value as TBitmap;
  FModifiedGlyph := True;
  Invalidate;
end;

procedure THIGButton.SetKind(Value: THIGBtnKind);
begin
  if Value <> FKind then
  begin
    if (Value <> bkCustom) and (not(Value in [bkDropDown, bkDropDownButton])) then
    begin
      Default := Value in [bkOK, bkYes];
      Cancel := Value in [bkCancel, bkNo];

      if ((csLoading in ComponentState) and (Caption = '')) or
        (not (csLoading in ComponentState)) then
      begin
        if BitBtnCaptions[Value] <> '' then
          Caption := BitBtnCaptions[Value];
      end;

      ModalResult := BitBtnModalResults[Value];
      TButtonGlyph(FGlyph).Glyph := GetBitBtnGlyph(Value);
      NumGlyphs := 2;
      FModifiedGlyph := False;
    end;
    FKind := Value;
    Invalidate;
  end;
end;

procedure THIGButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure THIGButton.SetLookStyle(const Value: THIGLookStyle);
begin
  FLookStyle.Assign(Value);
end;

procedure THIGButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= - 1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure THIGButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then Value := 1
  else if Value > 4 then Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

procedure THIGButton.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure THIGButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;

procedure THIGButton.WMLButtonDown(var Message: TMessage);
var
  R: TRect;
begin
  if Assigned(FDropDownMenu) then
  begin
    R := ClientRect;
    R.Left := R.Right - FDropDownButtonWidth;
    if PtInRect(R, Point(Message.LParamLo, Message.LParamHi)) then
      FLockDown:= True;
  end;
  inherited;
end;

procedure THIGButton.WMLButtonUp(var Message: TMessage);
begin
  FLockDown:= False;
  inherited;
end;

{ THIGCustomLookStyle }

procedure THIGCustomLookStyle.Assign(Source: TPersistent);
begin
  if Source is THIGCustomLookStyle then
  begin
    with Source as THIGCustomLookStyle do
    begin
      Self.AlwaysShowBorder:= AlwaysShowBorder;
      Self.BorderColor:= BorderColor;
      Self.ButtonColor:= ButtonColor;
      Self.DownColor:= DownColor;
      Self.DownTextColor:= DownTextColor;
      Self.Gradient:= Gradient;
      Self.GradStartColor:= GradStartColor;
      Self.GradStartDownColor:= GradStartDownColor;
      Self.GradStartHighLightColor:= GradStartHighlightColor;
      Self.HighlightColor:= HighlightColor;
      Self.HighlightOnFocused:= HighlightOnFocused;
      Self.HighlightTextColor:= HighlightTextColor;
      Self.HighlightTrack:= HighlightTrack;
      Self.Style:= Style;
    end;
  end
  else
    inherited Assign(Source);
end;

constructor THIGCustomLookStyle.Create(AOwner: TPersistent);
begin
  FOwner:= AOwner;
  FHighlightTrack:= True;
  FHighlightOnFocused:= True;
  FHighlightColor:= clDefault;
  FButtonColor:= clBtnFace;
  FBorderColor:= clWindowFrame;
  FHighlightTextColor:= clDefault;
  FStyle:= hlsUltraFlat;
  FLightColor:= GetLightenColor(FButtonColor);
  FDarkColor:= GetDarkenColor(FButtonColor);
  FBorderLightColor:= GetLightenColor(FBorderColor);
  FDownColor:= clDefault;
  FDownTextColor:= clDefault;
  FGradient:= False;
  FAlwaysShowBorder:= False;
  FGradStartColor:= clDefault;
  FGradStartDownColor:= clDefault;
  FGradStartHighLightColor:= clDefault;
end;

function THIGCustomLookStyle.GetOwner: TPersistent;
begin
  Result:= FOwner;
end;

procedure THIGCustomLookStyle.SetAlwaysShowBorder(const Value: Boolean);
var
  AMsg: TMessage;
begin
  if FAlwaysShowBorder <> Value then
  begin
    FAlwaysShowBorder:= Value;
    aMsg.Msg:= IG_BTNINVALIDATE;
    FOwner.Dispatch(aMsg);
  end
end;

procedure THIGCustomLookStyle.SetBorderColor(const Value: TColor);
var
  AMsg: TMessage;
begin
  if FBorderColor <> Value then
  begin
    FBorderColor:= Value;
    FBorderLightColor:= GetLightenColor(FBorderColor);
    aMsg.Msg:= IG_BTNINVALIDATE;
    FOwner.Dispatch(aMsg);
  end;
end;

procedure THIGCustomLookStyle.SetButtonColor(const Value: TColor);
var
  AMsg: TMessage;
begin
  if FButtonColor <> Value then
  begin
    FButtonColor:= Value;
    FLightColor:= GetLightenColor(FButtonColor);
    FDarkColor:= GetDarkenColor(FButtonColor);
    aMsg.Msg:= IG_BTNCOLORCHANGE;
    FOwner.Dispatch(aMsg);
  end;
end;

procedure THIGCustomLookStyle.SetGradient(const Value: Boolean);
var
  AMsg: TMessage;
begin
  if FGradient <> Value then
  begin
    FGradient:= Value;
    aMsg.Msg:= IG_BTNINVALIDATE;
    FOwner.Dispatch(aMsg);
  end;
end;

procedure THIGCustomLookStyle.SetGradStartColor(const Value: TColor);
var
  AMsg: TMessage;
begin
  if FGradStartColor <> Value then
  begin
    FGradStartColor:= Value;
    aMsg.Msg:= IG_BTNINVALIDATE;
    FOwner.Dispatch(aMsg);
  end;
end;

procedure THIGCustomLookStyle.SetGradStartDownColor(const Value: TColor);
var
  AMsg: TMessage;
begin
  if FGradStartDownColor <> Value then
  begin
    FGradStartDownColor:= Value;
    aMsg.Msg:= IG_BTNINVALIDATE;
    FOwner.Dispatch(aMsg);
  end;
end;

procedure THIGCustomLookStyle.SetGradStartHighlightColor(
  const Value: TColor);
var
  AMsg: TMessage;
begin
  if FGradStartHighlightColor <> Value then
  begin
    FGradStartHighlightColor:= Value;
    aMsg.Msg:= IG_BTNINVALIDATE;
    FOwner.Dispatch(aMsg);
  end;
end;

procedure THIGCustomLookStyle.SetStyle(const Value: THIGStyle);
var
  AMsg: TMessage;
begin
  if FStyle <> Value then
  begin
    FStyle:= Value;
    aMsg.Msg:= IG_BTNINVALIDATE;
    FOwner.Dispatch(aMsg);
  end;
end;

{ THIGSpeedButton }

constructor THIGSpeedButton.Create(AOwner: TComponent);
begin
  FGlyph := TButtonGlyph.Create;
  FHotGlyph := TButtonGlyph.Create;
  TButtonGlyph(FHotGlyph).OnChange:= HotGlyphChanged;
  inherited Create(AOwner);
  FFlatStyle:= bfsStandard;
  FMenuVisible:= False;
end;

destructor THIGSpeedButton.Destroy;
begin
  inherited;
  TButtonGlyph(FGlyph).Free;
  TButtonGlyph(FHotGlyph).Free;
end;

procedure THIGSpeedButton.DoDropDownMenu;

  function GetDropDownMenuHeight: Integer;
  var
    ANonClientMetrics: TNonClientMetrics;
    I, AHeight, AWidth: Integer;
    MI: TMenuItemAccess;
    B: Boolean;
    ACanvas: TCanvas;
  begin
    ANonClientMetrics.cbSize := SizeOf(TNonClientMetrics);
    B := SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @ANonClientMetrics, 0);
    if B then
      Result := ANonClientMetrics.iBorderWidth * 2
    else
      Result := 2;
    ACanvas:= TCanvas.Create;
    with ACanvas do
    try
      Font.Assign(Screen.MenuFont);
      for I := 0 to FDropDownMenu.Items.Count - 1 do
      begin
        MI := TMenuItemAccess(FDropDownMenu.Items[i]);
        AHeight := ANonClientMetrics.iMenuHeight;
        if FDropDownMenu.OwnerDraw and MI.Visible then
          MI.MeasureItem(ACanvas, AWidth, AHeight);
        Inc(Result, AHeight);
      end;
    finally
      ACanvas.Free;
    end;
  end;

var
  ADropDown: boolean;
  P: TPoint;
  H: Integer;
//  Msg: TMsg;
begin
  if FDropDownMenu = nil then Exit;
  ADropDown:= True;
  if Assigned(FOnBeforeDropDownMenu) then
    FOnBeforeDropDownMenu(Self, ADropDown);
  if not ADropDown then
    Exit;

  if (FDropDownMenu <> nil) then
  begin
    FMenuVisible := True;
    if FDropDownMenu.Alignment = paRight then
      P := ClientToScreen(Point(Width, Height))
    else if FDropDownMenu.Alignment = paCenter then
      P := ClientToScreen(Point(Width div 2, Height))
    else
      P := ClientToScreen(Point(0, Height));
    H := GetDropDownMenuHeight;
    if P.Y + H > Screen.Height then Dec(P.Y, Height + H + 2);
    if P.X < 0 then P.X := 0;
    FDropDownMenu.Popup(P.X, P.Y);
    //while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do ;
    FMenuVisible := False;
  end;
  Invalidate;
end;

function THIGSpeedButton.GetHotGlyph: TBitmap;
begin
  Result := TButtonGlyph(FHotGlyph).Glyph;
end;

function THIGSpeedButton.GetNumHotGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FHotGlyph).NumGlyphs;
end;

procedure THIGSpeedButton.HotGlyphChanged(Sender: TObject);
begin
  if MouseInControl then
    Invalidate;
end;

procedure THIGSpeedButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Assigned(FDropDownMenu) and (FFlatStyle = bfsBorderless) then
  begin
    if not FMenuVisible then
    begin
      R := ClientRect;
      if PtInRect(R, Point(X, Y)) then
      begin
        SetCaptureControl(Self);
        DoDropDownMenu;
        MouseUp(Button, Shift, X, Y);
      end;
    end
    else
    begin
      FMenuVisible := False;
      Repaint;
    end;
  end;
end;

procedure THIGSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if GetCaptureControl = Self then
    ReleaseCapture;
end;

procedure THIGSpeedButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDropDownMenu) then
    FDropDownMenu := nil;
end;

procedure THIGSpeedButton.Paint;
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  Offset: TPoint;
begin
  if not Flat or (FFlatStyle = bfsStandard) or ThemeServices.ThemesEnabled then
    inherited
  else
  begin
    if not Enabled then
    begin
      FState := bsDisabled;
    end
    else if FState = bsDisabled then
      if Down and (GroupIndex <> 0) then
        FState := bsExclusive
      else
        FState := bsUp;

    Canvas.Font := Self.Font;
    PaintRect := Rect(0, 0, Width, Height);
    if (FState in [bsDown, bsExclusive]) or
       (MouseInControl and (FState <> bsDisabled)) or
       (csDesigning in ComponentState) then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := Color;
      if Transparent then
      begin
        Canvas.Brush.Style:= bsClear;
        Canvas.Pen.Style:= psClear;
      end;
      Canvas.FillRect(PaintRect);
      if Transparent then
      begin
        Canvas.Brush.Style:= bsSolid;
        Canvas.Pen.Style:= psSolid;
      end;
    end
    else if not Transparent then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(PaintRect);
    end;
    //InflateRect(PaintRect, -1, -1);
    if (FState in [bsDown, bsExclusive]) or FMenuVisible then
    begin
      if (FState = bsExclusive) and (not Flat or not MouseInControl) then
      begin
        Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
        Canvas.FillRect(PaintRect);
      end;
      Offset.X := 1;
      Offset.Y := 1;
    end
    else
    begin
      Offset.X := 0;
      Offset.Y := 0;
    end;
    if not HotGlyph.Empty and //not (FState in [bsUp, bsDisabled]) and
       ((FState in [bsDown, bsExclusive]) or MouseInControl or FMenuVisible) and not
       (csDesigning in ComponentState) then
      TButtonGlyph(FHotGlyph).Draw(Canvas, PaintRect, Offset, Caption, Layout, Margin,
        Spacing, FState, Transparent, DrawTextBiDiModeFlags(0), False, clGrayText,
        clBtnShadow, clBtnHighlight)
    else
    begin
      TButtonGlyph(FGlyph).Glyph:= Glyph;
      TButtonGlyph(FGlyph).NumGlyphs:= NumGlyphs;
      TButtonGlyph(FGlyph).Draw(Canvas, PaintRect, Offset, Caption, Layout, Margin,
        Spacing, FState, Transparent, DrawTextBiDiModeFlags(0), False, clGrayText,
        clBtnShadow, clBtnHighlight);
    end;
  end;
end;

procedure THIGSpeedButton.SetDropDownMenu(const Value: TPopupMenu);
begin
  FDropDownMenu := Value;
  if Value <> nil then
  begin
    Value.ParentBiDiModeChanged(Self);
    Value.FreeNotification(Self);
  end;
end;

procedure THIGSpeedButton.SetFlatStyle(const Value: THIGSpeedBtnFlatStyle);
begin
  if FFlatStyle <> Value then
  begin
    FFlatStyle := Value;
    Invalidate;
  end;
end;

procedure THIGSpeedButton.SetHotGlyph(const Value: TBitmap);
begin
  TButtonGlyph(FHotGlyph).Glyph := Value as TBitmap;
  if MouseInControl then
    Invalidate;
end;

procedure THIGSpeedButton.SetNumHotGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then Value := 1
  else if Value > 4 then Value := 4;
  if Value <> TButtonGlyph(FHotGlyph).NumGlyphs then
  begin
    TButtonGlyph(FHotGlyph).NumGlyphs := Value;
    if MouseInControl then
      Invalidate;
  end;
end;

procedure THIGSpeedButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;

initialization
  FillChar(BitBtnGlyphs, SizeOf(BitBtnGlyphs), 0);
finalization
  DestroyLocals;
end.
