
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit GraphTools;

interface

{$I STD.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Math, GraphThemes,
  StrTools, MathTools, WinTools;

type
  TFastBitmap = record
    DC: HDC;
    Handle: HBITMAP;
    Bits: Pointer;
    Width: Integer;
    Height: Integer;
  end;

function CreateFastBitmap(Width, Height: Integer): TFastBitmap;
procedure DestroyFastBitmap(const Bitmap: TFastBitmap);

const
  CLEARTYPE_QUALITY = 5;

{ New color constants defined by Internet Explorer }

const
  clAliceBlue = TColor($FFF8F0);
  clAntiqueWhite = TColor($D7EBFA);
  clAquamarine = TColor($D4FF7F);
  clAzure = TColor($FFFFF0);
  clBeige = TColor($DCF5F5);
  clBisque = TColor($C4E4FF);
  clBlanchedAlmond = TColor($CDEBFF);
  clBlueViolet = TColor($E22B8A);
  clBrown = TColor($2A2AA5);
  clBurlywood = TColor($87B8DE);
  clCadetBlue = TColor($A09E5F);
  clChartreuse = TColor($00FF7F);
  clChocolate = TColor($1E69D2);
  clCoral = TColor($507FFF);
  clCornFlowerBlue = TColor($ED9564);
  clCornSilk = TColor($DCF8FF);
  clCrimson = TColor($3C14DC);
  clCyan = TColor($FFFF00);
  clDarkBlue = TColor($8B0000);
  clDarkCyan = TColor($8B8B00);
  clDarkGoldenrod = TColor($0B86B8);
  clDarkGray = TColor($A9A9A9);
  clDarkGreen = TColor($006400);
  clDarkKhaki = TColor($6BB7BD);
  clDarkMagenta = TColor($8B008B);
  clDarkOliveGreen = TColor($2F6B55);
  clDarkOrange = TColor($008CFF);
  clDarkOrchid = TColor($CC3299);
  clDarkRed = TColor($00008B);
  clDarkSalmon = TColor($7A96E9);
  clDarkseaGreen = TColor($8BBC8F);
  clDarkslateBlue = TColor($8B3D48);
  clDarkSlateGray = TColor($4F4F2F);
  clDarkTurquoise = TColor($D1CE00);
  clDarkViolet = TColor($D30094);
  clDeepPink = TColor($9314FF);
  clDeepSkyBlue = TColor($FFBF00);
  clDimGray = TColor($696969);
  clDodgerBlue = TColor($FF901E);
  clFireBrick = TColor($2222B2);
  clFloralWhite = TColor($F0FAFF);
  clForestGreen = TColor($228B22);
  clGainsboro = TColor($DCDCDC);
  clGhostWhite = TColor($FFF8F8);
  clGold = TColor($00D7FF);
  clGoldenrod = TColor($20A5DA);
  clGreenYellow = TColor($2FFFAD);
  clHoneydew = TColor($F0FFF0);
  clHotPink = TColor($B469FF);
  clIndianRed = TColor($5C5CCD);
  clIndigo = TColor($82004B);
  clIvory = TColor($F0FFFF);
  clKhaki = TColor($8CE6F0);
  clLavender = TColor($FAE6E6);
  clLavenderBlush = TColor($F5F0FF);
  clLawnGreen = TColor($00FC7C);
  clLemonChiffon = TColor($CDFAFF);
  clLightBlue = TColor($E6D8AD);
  clLightCoral = TColor($8080F0);
  clLightCyan = TColor($FFFFE0);
  clLightGoldenrodYellow = TColor($D2FAFA);
  clLightGreen = TColor($90EE90);
  clLightGray = TColor($D3D3D3);
  clLightPink = TColor($C1B6FF);
  clLightSalmon = TColor($7AA0FF);
  clLightSeaGreen = TColor($AAB220);
  clLightSkyBlue = TColor($FACE87);
  clLightSlategray = TColor($998877);
  clLightSteelBlue = TColor($DEC4B0);
  clLightYellow = TColor($E0FFFF);
  clLimeGreen = TColor($32CD32);
  clLinen = TColor($E6F0FA);
  clMagenta = TColor($FF00FF);
  clMediumAquamarine = TColor($AACD66);
  clMediumBlue = TColor($CD0000);
  clMediumOrchid = TColor($D355BA);
  clMediumPurple = TColor($DB7093);
  clMediumSeaGreen = TColor($71B33C);
  clMediumSlateBlue = TColor($EE687B);
  clMediumSpringGreen = TColor($9AFA00);
  clMediumTurquoise = TColor($CCD148);
  clMediumVioletRed = TColor($8515C7);
  clMidnightBlue = TColor($701919);
  clMintCream = TColor($FAFFF5);
  clMistyRose = TColor($E1E4FF);
  clMoccasin = TColor($B5E4FF);
  clNavajoWhite = TColor($ADDEFF);
  clOldLace = TColor($E6F5FD);
  clOliveDrab = TColor($238E6B);
  clOrange = TColor($00A5FF);
  clOrangeRed = TColor($0045FF);
  clOrchid = TColor($D670DA);
  clPaleGoldenrod = TColor($AAE8EE);
  clPaleGreen = TColor($98FB98);
  clPaleTurquoise = TColor($EEEEAF);
  clPalevioletRed = TColor($9370DB);
  clPapayawhip = TColor($D5EFFF);
  clPeachPuff = TColor($B9DAFF);
  clPeru = TColor($3F85CD);
  clPink = TColor($CBC0FF);
  clPlum = TColor($DDA0DD);
  clPowderBlue = TColor($E6E0B0);
  clRosyBrown = TColor($8F8FBC);
  clRoyalBlue = TColor($E16941);
  clSaddleBrown = TColor($13458B);
  clSalmon = TColor($7280FA);
  clSandyBrown = TColor($60A4F4);
  clSeaGreen = TColor($578B2E);
  clSeaShell = TColor($EEF5FF);
  clSienna = TColor($2D52A0);
  clSkyBlue = TColor($EBCE87);
  clSlateBlue = TColor($CD5A6A);
  clSlateGray = TColor($908070);
  clSnow = TColor($FAFAFF);
  clSpringGreen = TColor($7FFF00);
  clSteelBlue = TColor($B48246);
  clTan = TColor($8CB4D2);
  clThistle = TColor($D8BFD8);
  clTomato = TColor($4763FF);
  clTurquoise = TColor($D0E040);
  clViolet = TColor($EE82EE);
  clWheat = TColor($B3DEF5);
  clWhiteSmoke = TColor($F5F5F5);
  clYellowGreen = TColor($32CD9A);

  EnabledColors: array[Boolean] of TColor = (clBtnFace, clWindow);

type
  TDirection = (drLeft, drUp, drRight, drDown, drCenter, drFill, drWrap);
  TDirections = set of TDirection;
  TCorner = (cnTopLeft, cnTopRight, cnBottomRight, cnBottomLeft);
  TCorners = set of TCorner;

function DirectionToAlignment(Direction: TDirection): TAlignment;
function AlignmentToDirection(Alignment: TAlignment): TDirection;

{ Context functions used to translate between pixel coordinates and device space
  coordinates }

function InitializeDevice(DC: HDC): HDC;
procedure FinalizeDevice(DC: HDC);

{ Device space coordinate translation routines }

function IntToDevice(Value: Integer; Angle: Integer = 0): Double;
function PointToDevice(const Point: TPoint): TFloatPoint; overload;
function PointToDevice(X, Y: Integer): TFloatPoint; overload;
function RectToDevice(const Rect: TRect): TFloatRect; overload;
function RectToDevice(ALeft, ATop, ARight, ABottom: Integer): TFloatRect; overload;
function PolygonToDevice(const Polygon: TPolygon): TFloatPolygon;
function DeviceToInt(const Value: Double; Angle: Integer = 0): Integer;
function DeviceToPoint(const Point: TFloatPoint): TPoint; overload;
function DeviceToPoint(const X, Y: Double): TPoint; overload;
function DeviceToRect(const Rect: TFloatRect): TRect; overload;
function DeviceToRect(const ALeft, ATop, ARight, ABottom: Double): TRect; overload;
function DeviceToPolygon(const Polygon: TFloatPolygon): TPolygon;

{ Decive compatible rect querying and transofrmation routines }

procedure OffsetRect(var Rect: TRect; X, Y: Integer); overload;
procedure OffsetRect(var Rect: TFloatRect; const X, Y: Double); overload;
procedure InflateRect(var Rect: TRect; X, Y: Integer); overload;
procedure InflateRect(var Rect: TFloatRect; const X, Y: Double); overload;
function HeightOf(const Rect: TRect): Integer; overload;
function HeightOf(const Rect: TFloatRect): Double; overload;
function WidthOf(const Rect: TRect): Integer; overload;
function WidthOf(const Rect: TFloatRect): Double; overload;
function MoveRect(const Rect: TRect; X, Y: Integer): TRect;
procedure Slide(var Rect: TRect; Direction: TDirection = drDown;
  Distance: Integer = 0); overload;
procedure Slide(var Rect: TFloatRect; Direction: TDirection = drDown;
  Distance: Double = 0); overload;
function Chamfer(const Rect: TFloatRect; const Size: Double;
  Corners: TCorners): TFloatPolygon; overload;

function GetTextAscent: Double;
function GetTextDescent: Double;
function GetTextBaseline: Double;

{ Color manipulation functions }

function Blend(ForeColor: TColor; BackColor: TColor; Percent: Byte = 50): TColor;
function Delta(Color: TColor; Value: Integer): TColor;
function Scale(Color: TColor; Value: Integer): TColor;

{ The TCalculateRectEvent datatype is a generic method pointer that delegates
  the resizing of the Rect parameter to external objects }

type
  TCalculateRectEvent = procedure(Sender: TObject; var Rect: TRect) of object;

function GetBorder: Integer;
function GetTextColor(Background: TColor): TColor;

{ These get object functions all return dynamically allocated GDI objects. It is
  the caller's responsibility to manage the lifetime of these objects. Summary
  of the function are as follows:

    The GetBitmap function returns an eight pixel by eight pixel TBitmap object
    with alternating colors specified by the ForeColor and BackColor parameters }

function GetBitmap(ForeColor: TColor; BackColor: TColor): TBitmap; overload;
function GetBitmap(Resource: Integer): TBitmap; overload;
function GetBrush(Bitmap: TBitmap): HBRUSH; overload;
function GetBrush(Color: TColor; Style: TBrushStyle = bsSolid): HBRUSH; overload;
function GetPen(Color: TColor; Width: Integer = 0; Square: Boolean = False): HPEN; overload;
function GetPen(Color: TColor; Style: TPenStyle): HPEN; overload;
function GetRegion(const Polygon: TPolygon): HRGN; overload;
function GetRegion(DC: HDC): HRGN; overload;
function GetFont(const Name: string; Size: Integer; Italic: Boolean = False;
  Underlined: Boolean = False; Weight: Integer = 0;  Charset: TFontCharset = 0): HFONT;


{ The SwapFont proceure }

procedure SwapFont(A, B: TFont);

{ The OverwriteObject helper routine selects and deletes GDI objects from a
  device context in a single call }

procedure OverwriteObject(DC: HDC; Obj: HGDIOBJ);

{ The SelectRectPath procedure selects a clipping rectangle into a device
  context use the mode passed in the Mode parameter }

procedure SelectClipRect(DC: HDC; const Rect: TRect; Mode: Integer);

{ Non-themed drawing routines }

{ Helper draw format flags }

const
  DR_FORMAT = DT_SINGLELINE or DT_END_ELLIPSIS;
  DR_LEFT = DR_FORMAT or DT_VCENTER or DT_LEFT;
  DR_TOP = DR_FORMAT or DT_TOP or DT_LEFT;
  DR_RIGHT = DR_FORMAT or DT_VCENTER or DT_RIGHT;
  DR_BOTTOM = DR_FORMAT or DT_BOTTOM or DT_LEFT;
  DR_CENTER = DR_FORMAT or DT_VCENTER or DT_CENTER;
  DR_FILL = DT_SINGLELINE or DT_VCENTER or DT_CENTER or DT_NOCLIP;
  DR_WRAP = DT_TOP or DT_WORDBREAK or DT_END_ELLIPSIS;
  DR_WRAP_CENTER = DT_VCENTER or DT_CENTER or DT_WORDBREAK or DT_END_ELLIPSIS;

{ Element sizes }

  NodeSize = 16;

{ The DrawArrow procedure draws a small triangle centered in a rectangle
  pointing in the direction specified by the Direction parameter }

procedure DrawArrow(DC: HDC; Rect: TRect; Direction: TDirection;
  ForeColor, BackColor: TColor; Size: Integer = 0; Enabled: Boolean = True); overload;

procedure DrawArrow(DC: HDC; Rect: TRect; Direction: TDirection;
	Color: TColor = clWindowFrame; Enabled: Boolean = True); overload;

{ The DrawBox procedure }

procedure DrawBox(DC: HDC; Color: TColor; var Rect: TRect);

{ The DrawCheckBox procedure }

procedure DrawCheckBox(DC: HDC; Rect: TRect; Checked: Boolean;
  ForeColor, BackColor: TColor; Flat: Boolean = True);

{ The DrawCheckText procedure }

procedure DrawCheckText(DC: HDC; const Text: string; Rect: TRect;
  Direction: TDirection; Checked: Boolean; ForeColor, BackColor: TColor;
  Flat: Boolean = True);

{ The DrawSortArrow procedure draws an etched arrow similar to the ones
  found in an explorer in detail view }

procedure DrawSortArrow(DC: HDC; Rect: TRect; Direction: TDirection);

{ The CalculateCaptionRect function returns the calculated rectangle which exactly
  bounds the text given in the Caption parameter. See also DrawCaption }

const
  Directions: array[TDirection] of Integer = (DR_LEFT or DT_VCENTER, DR_TOP,
    DT_RIGHT or DT_VCENTER, DR_BOTTOM, DR_CENTER, DR_FILL, DR_WRAP_CENTER);

function CalculateCaptionRect(DC: HDC; const Text: string; Rect: TRect;
  Direction: TDirection): TRect;

{ The CalculateCaptionSize function }

function CalculateCaptionSize(DC: HDC; const Text: string): TSize;

{ The CalculateMemoHeight function }

function CalculateMemoHeight(DC: HDC; const Text: string; Width: Integer): Integer;

{ The DrawCaption procedure draws a vertically centered line of text truncated
  to fit in a rectangle, Directioned according to its Direction parameter }

procedure DrawCaption(DC: HDC; const Caption: string; Rect: TRect;
  Direction: TDirection; Enabled: Boolean = True);

procedure DrawFocus(DC: HDC; Rect: TRect; BorderX: Integer = 0; BorderY: Integer = 0);

{ The DrawClose procedure draws a close symbol in a device context centered
  about the Rect parameter	}

procedure DrawClose(DC: HDC; Rect: TRect; Color: TColor = clWindowFrame);

{ The DrawDivider procedure draws a beveled line in a device context }

type
  TDrawDividerKind = (ddHorz, ddVert);

procedure DrawDivider(DC: HDC; Rect: TRect; Kind: TDrawDividerKind);

{ The DrawEllipsis procedure }

procedure DrawEllipsis(DC: HDC; Rect: TRect; Enabled: Boolean = True);

{ The DrawFrame procedure draws border inside a rectangle using the following
  states:

    dfFocus:   A thin single pixel line simulating a window border.

    dfFramed:  A framed border two pixels thick that simulates the look of a
               three dimensional button.
    dfHover:   A thin single pixel line border that appears as a control border
               when drawn against an application workspace background.
    dfRaised:  A thin single pixel line border that has a three dimensional
               appearance of being raised.
    dfFlat:    No border drawn at all. Provided as a convience to handle a
               request to draw nothing.
    dfLowered: A thin single line border that has the three dimensional
               appearance of being sunken.
    dfPressed: A thin single pixel inverted border for items that appear as
               controls when drawn against an application workspace background.
    dfSunken:  The sunken border of a client edge window with a three
               dimensional look.
    dfPushed:  An inverted framed border two pixels thick that simluates the
               look of a Pushed button. }

type
  TDrawFrameState = (dfFocus, dfFramed, dfHover, dfRaised, dfFlat,
  	dfLowered, dfSunken, dfPressed, dfPushed);

procedure DrawFrame(DC: HDC; Rect: TRect; State: TDrawFrameState);

{ The DrawGradient procedure fills the Rect parameter with a series of colored
  bands that range in gradient value from StartColor to EndColor. The number of
  color bands is specified by the Colors parameter, and the direction of the
  gradiant fill is determined by the Direction parameter  }

procedure DrawGradient(DC: HDC; Rect: TRect; StartColor, EndColor: TColor;
  Colors: Byte; Direction: TDirection);

{ The DrawGrip procedure draws a size grip image }

procedure DrawGrip(DC: HDC; Rect: TRect; Clipped: Boolean = True);

{ The DrawNode procedure draws a visual representation of a tree node. The
  default SolidColor parameter paints the node using system colors }

procedure DrawNode(DC: HDC; Rect: TRect; Expanded: Boolean; SolidColor: TColor = cl3DDkShadow);

{ The DrawToolGrip procedure draws a grip on dockable windows }

procedure DrawToolGrip(DC: HDC; const Rect: TRect);

{ The DrawPolygon procedure is a simple wrapper around the Polygon GDI function }

procedure DrawPolygon(DC: HDC; const P: TPolygon); overload;
procedure DrawPolygon(DC: HDC; const P: TFloatPolygon); overload;

{ The DrawAngledText procedure }

procedure DrawAngledText(DC: HDC; const S: string; const Angle: Double;
  const Point: TPoint);

{ The DrawRect procedure }

procedure DrawRect(DC: HDC; const Rect: TRect; Color: TColor);

{ The DrawRectOutline procedure }

procedure DrawRectOutline(DC: HDC; const Rect: TRect; Color: TColor);

{ The DrawSlantRect procedure }

procedure DrawSlantRect(DC: HDC; const Rect: TRect; const A, B: TPoint);

{ The DrawSlantText procedure }

procedure DrawSlantText(DC: HDC; const S: string; const A, B: TPoint);

{ *NEW* The DrawBevelRect procedure }

procedure DrawBevelRect(DC: HDC; Rect: TRect; Bevel: Integer; Color: TColor);

{ The DrawBubble procedure }

procedure DrawBubble(DC: HDC; const Caption: string; Rect: TRect;
  Direction: TDirection; Bevel: Integer; ForeColor, BackColor: TColor);

{ The DrawCapsule procedures }

type
  TCapsuleProc = procedure(DC: HDC; Rect: TRect; ForeColor, BackColor: TColor;
    Data: Pointer);

{ The DrawCapsuleAdvanced procedure }

procedure DrawCapsuleAdvanced(DC: HDC; const Caption: string; Rect: TRect;
  Direction: TDirection; Bevel: Integer; Width: Integer;
  CapsuleProc: TCapsuleProc; ForeColor, BackColor: TColor; Data: Pointer);

{ The DrawCapsuleText procedure }

procedure DrawCapsuleText(DC: HDC; const Caption, Body: string; Rect: TRect;
  CaptionDirection, BodyDirection: TDirection; Bevel: Integer; Width: Integer;
  ForeColor, BackColor: TColor);


{ The DrawCapsuleCheckBox procedure }

procedure DrawCapsuleCheckBox(DC: HDC; const Text: string; Rect: TRect;
  Direction: TDirection; Bevel: Integer; Width: Integer; Checked: Boolean;
  ForeColor, BackColor: TColor; Flat: Boolean = True);

{ Themed drawing states }

type
  TDrawState = set of (dsDisabled, dsPressed, dsHot, dsFocused, dsChecked,
  	dsExpanded, dsDefaulted, dsThin, dsFlat);

{ Glyphs }

procedure BlitInvert(DC: HDC; const Rect: TRect);
procedure BlitAnd(Dest: HDC; const Rect: TRect; Source: HDC);
procedure BlitOr(Dest: HDC; const Rect: TRect; Source: HDC);

type
	TGlyphKind = (gkClose, gkPin, gkPinPushed, gkChevronUp, gkChevronDown,
  	gkArrowLeft, gkArrowRight, gkArrowUp, gkArrowDown,
    gkArrowLeftDisabled, gkArrowRightDisabled, gkArrowUpDisabled,
    gkArrowDownDisabled, gkEllipse, gkQuestion);

function GlyphFind(Kind: TGlyphKind): TBitmap;
procedure GlyphBlendBlt(DC: HDC; Glyph: TGlyphKind; X, Y: Integer; Color: TColor);
procedure GlyphDraw(DC: HDC; const Rect: TRect; Glyph: TGlyphKind;  Color: TColor); overload;
procedure GlyphFrame(DC: HDC; Rect: TRect; Glyph: TGlyphKind; State: TDrawState; Color: TColor);

{ These are all elements which can be themed. }

type
  TThemedElement = (
    teButton,
    teClock,
    teComboBox,
    teEdit,
    teExplorerBar,
    teHeader,
    teListView,
    teMenu,
    tePage,
    teProgress,
    teRebar,
    teScrollBar,
    teSpin,
    teStartPanel,
    teStatus,
    teTab,
    teTaskBand,
    teTaskBar,
    teToolBar,
    teToolTip,
    teTrackBar,
    teTrayNotify,
    teTreeview,
    teWindow
  );

{ The root part of each element is sometimes used for special painting and does
	not belong to a certain state. }

{ Button }

  TThemedButton = (
    tbButtonDontCare,
    tbButtonRoot,
    tbPushButtonNormal, tbPushButtonHot, tbPushButtonPressed, tbPushButtonDisabled, tbPushButtonDefaulted,
    tbRadioButtonUncheckedNormal, tbRadioButtonUncheckedHot, tbRadioButtonUncheckedPressed, tbRadioButtonUncheckedDisabled,
    tbRadioButtonCheckedNormal, tbRadioButtonCheckedHot, tbRadioButtonCheckedPressed, tbRadioButtonCheckedDisabled,
    tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot, tbCheckBoxUncheckedPressed, tbCheckBoxUncheckedDisabled,
    tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot, tbCheckBoxCheckedPressed, tbCheckBoxCheckedDisabled,
    tbCheckBoxMixedNormal, tbCheckBoxMixedHot, tbCheckBoxMixedPressed, tbCheckBoxMixedDisabled,
    tbGroupBoxNormal, tbGroupBoxDisabled,
    tbUserButton
  );

{ Clock }

  TThemedClock = (
    tcClockDontCare,
    tcClockRoot,
    tcTimeNormal
  );

{ ComboBox }

  TThemedComboBox = (
    tcComboBoxDontCare,
    tcComboBoxRoot,
    tcDropDownButtonNormal, tcDropDownButtonHot, tcDropDownButtonPressed, tcDropDownButtonDisabled
  );

{ Edit }

  TThemedEdit = (
    teEditDontCare,
    teEditRoot,
    teEditTextNormal, teEditTextHot, teEditTextSelected, teEditTextDisabled, teEditTextFocused, teEditTextReadOnly, teEditTextAssist,
    teEditCaret
  );

{ ExplorerBar }

  TThemedExplorerBar = (
    tebExplorerBarDontCare,
    tebExplorerBarRoot,
    tebHeaderBackgroundNormal, tebHeaderBackgroundHot, tebHeaderBackgroundPressed,
    tebHeaderCloseNormal, tebHeaderCloseHot, tebHeaderClosePressed,
    tebHeaderPinNormal, tebHeaderPinHot, tebHeaderPinPressed,
    tebHeaderPinSelectedNormal, tebHeaderPinSelectedHot, tebHeaderPinSelectedPressed,
    tebIEBarMenuNormal, tebIEBarMenuHot, tebIEBarMenuPressed,
    tebNormalGroupBackground,
    tebNormalGroupCollapseNormal, tebNormalGroupCollapseHot, tebNormalGroupCollapsePressed,
    tebNormalGroupExpandNormal, tebNormalGroupExpandHot, tebNormalGroupExpandPressed,
    tebNormalGroupHead,
    tebSpecialGroupBackground,
    tebSpecialGroupCollapseSpecial, tebSpecialGroupCollapseHot, tebSpecialGroupCollapsePressed,
    tebSpecialGroupExpandSpecial, tebSpecialGroupExpandHot, tebSpecialGroupExpandPressed,
    tebSpecialGroupHead
  );

{ Header }

  TThemedHeader = (
    thHeaderDontCare,
    thHeaderRoot,
    thHeaderItemNormal, thHeaderItemHot, thHeaderItemPressed,
    thHeaderItemLeftNormal, thHeaderItemLeftHot, thHeaderItemLeftPressed,
    thHeaderItemRightNormal, thHeaderItemRightHot, thHeaderItemRightPressed,
    thHeaderSortArrowSortedUp, thHeaderSortArrowSortedDown
  );

{ ListView }

  TThemedListview = (
    tlListviewDontCare,
    tlListviewRoot,
    tlListItemNormal, tlListItemHot, tlListItemSelected, tlListItemDisabled, tlListItemSelectedNotFocus,
    tlListGroup,
    tlListDetail,
    tlListSortDetail,
    tlEmptyText
  );

{ Menu }

  TThemedMenu = (
    tmMenuDontCare,
    tmMenuRoot,
    tmMenuItemNormal, tmMenuItemSelected, tmMenuItemDemoted,
    tmMenuDropDown,
    tmMenuBarItem,
    tmMenuBarDropDown,
    tmChevron,
    tmSeparator
  );

{ Page }

  TThemedPage = (
    tpPageDontCare,
    tpPageRoot,
    tpUpNormal, tpUpHot, tpUpPressed, tpUpDisabled,
    tpDownNormal, tpDownHot, tpDownPressed, tpDownDisabled,
    tpUpHorzNormal, tpUpHorzHot, tpUpHorzPressed, tpUpHorzDisabled,
    tpDownHorzNormal, tpDownHorzHot, tpDownHorzPressed, tpDownHorzDisabled
  );

{ Progress }

  TThemedProgress = (
    tpProgressDontCare,
    tpProgressRoot,
    tpBar,
    tpBarVert,
    tpChunk,
    tpChunkVert
  );

{ Rebar }

  TThemedRebar = (
    trRebarDontCare,
    trRebarRoot,
    trGripper,
    trGripperVert,
    trBandNormal, trBandHot, trBandPressed, trBandDisabled, trBandChecked, trBandHotChecked,
    trChevronNormal, trChevronHot, trChevronPressed, trChevronDisabled,
    trChevronVertNormal, trChevronVertHot, trChevronVertPressed, trChevronVertDisabled
  );

{ ScrollBar }

  TThemedScrollBar = (
    tsScrollBarDontCare,
    tsScrollBarRoot,
    tsArrowBtnUpNormal, tsArrowBtnUpHot, tsArrowBtnUpPressed, tsArrowBtnUpDisabled,
    tsArrowBtnDownNormal, tsArrowBtnDownHot, tsArrowBtnDownPressed, tsArrowBtnDownDisabled,
    tsArrowBtnLeftNormal, tsArrowBtnLeftHot, tsArrowBtnLeftPressed, tsArrowBtnLeftDisabled,
    tsArrowBtnRightNormal, tsArrowBtnRightHot, tsArrowBtnRightPressed, tsArrowBtnRightDisabled,
    tsThumbBtnHorzNormal, tsThumbBtnHorzHot, tsThumbBtnHorzPressed, tsThumbBtnHorzDisabled,
    tsThumbBtnVertNormal, tsThumbBtnVertHot, tsThumbBtnVertPressed, tsThumbBtnVertDisabled,
    tsLowerTrackHorzNormal, tsLowerTrackHorzHot, tsLowerTrackHorzPressed, tsLowerTrackHorzDisabled,
    tsUpperTrackHorzNormal, tsUpperTrackHorzHot, tsUpperTrackHorzPressed, tsUpperTrackHorzDisabled,
    tsLowerTrackVertNormal, tsLowerTrackVertHot, tsLowerTrackVertPressed, tsLowerTrackVertDisabled,
    tsUpperTrackVertNormal, tsUpperTrackVertHot, tsUpperTrackVertPressed, tsUpperTrackVertDisabled,
    tsGripperHorzNormal, tsGripperHorzHot, tsGripperHorzPressed, tsGripperHorzDisabled,
    tsGripperVertNormal, tsGripperVertHot, tsGripperVertPressed, tsGripperVertDisabled,
    tsSizeBoxRightAlign, tsSizeBoxLeftAlign
  );

{ Spin }

  TThemedSpin = (
    tsSpinDontCare,
    tsSpinRoot,
    tsUpNormal, tsUpHot, tsUpPressed, tsUpDisabled,
    tsDownNormal, tsDownHot, tsDownPressed, tsDownDisabled,
    tsUpHorzNormal, tsUpHorzHot, tsUpHorzPressed, tsUpHorzDisabled,
    tsDownHorzNormal, tsDownHorzHot, tsDownHorzPressed, tsDownHorzDisabled
  );

{ StartPanel }

  TThemedStartPanel = (
    tspStartPanelDontCare,
    tspStartPanelRoot,
    tspUserPane,
    tspMorePrograms,
    tspMoreProgramsArrowNormal, tspMoreProgramsArrowHot, tspMoreProgramsArrowPressed,
    tspProgList,
    tspProgListSeparator,
    tspPlacesList,
    tspPlacesListSeparator,
    tspLogOff,
    tspLogOffButtonsNormal, tspLogOffButtonsHot, tspLogOffButtonsPressed,
    tspUserPicture,
    tspPreview
  );

{ Status }

  TThemedStatus = (
    tsStatusDontCare,
    tsStatusRoot,
    tsPane,
    tsGripperPane,
    tsGripper
  );

{ Tab }

  TThemedTab = (
    ttTabDontCare,
    ttTabRoot,
    ttTabItemNormal, ttTabItemHot, ttTabItemSelected, ttTabItemDisabled, ttTabItemFocused,
    ttTabItemLeftEdgeNormal, ttTabItemLeftEdgeHot, ttTabItemLeftEdgeSelected, ttTabItemLeftEdgeDisabled, ttTabItemLeftEdgeFocused,
    ttTabItemRightEdgeNormal, ttTabItemRightEdgeHot, ttTabItemRightEdgeSelected, ttTabItemRightEdgeDisabled, ttTabItemRightEdgeFocused,
    ttTabItemBothEdgeNormal, ttTabItemBothEdgeHot, ttTabItemBothEdgeSelected, ttTabItemBothEdgeDisabled, ttTabItemBothEdgeFocused,
    ttTopTabItemNormal, ttTopTabItemHot, ttTopTabItemSelected, ttTopTabItemDisabled, ttTopTabItemFocused,
    ttTopTabItemLeftEdgeNormal, ttTopTabItemLeftEdgeHot, ttTopTabItemLeftEdgeSelected, ttTopTabItemLeftEdgeDisabled, ttTopTabItemLeftEdgeFocused,
    ttTopTabItemRightEdgeNormal, ttTopTabItemRightEdgeHot, ttTopTabItemRightEdgeSelected, ttTopTabItemRightEdgeDisabled, ttTopTabItemRightEdgeFocused,
    ttTopTabItemBothEdgeNormal, ttTopTabItemBothEdgeHot, ttTopTabItemBothEdgeSelected, ttTopTabItemBothEdgeDisabled, ttTopTabItemBothEdgeFocused,
    ttPane,
    ttBody
  );

{ TaskBand }

  TThemedTaskBand = (
    ttbTaskBandDontCare,
    ttbTaskBandRoot,
    ttbGroupCount,
    ttbFlashButton,
    ttpFlashButtonGroupMenu
  );

{ TaskBar }

  TThemedTaskBar = (
    ttTaskBarDontCare,
    ttTaskBarRoot,
    ttbTimeNormal
  );

{ ToolBar }

  TThemedToolBar = (
    ttbToolBarDontCare,
    ttbToolBarRoot,
    ttbButtonNormal, ttbButtonHot, ttbButtonPressed, ttbButtonDisabled, ttbButtonChecked, ttbButtonCheckedHot,
    ttbDropDownButtonNormal, ttbDropDownButtonHot, ttbDropDownButtonPressed, ttbDropDownButtonDisabled, ttbDropDownButtonChecked, ttbDropDownButtonCheckedHot,
    ttbSplitButtonNormal, ttbSplitButtonHot, ttbSplitButtonPressed, ttbSplitButtonDisabled, ttbSplitButtonChecked, ttbSplitButtonCheckedHot,
    ttbSplitButtonDropDownNormal, ttbSplitButtonDropDownHot, ttbSplitButtonDropDownPressed, ttbSplitButtonDropDownDisabled, ttbSplitButtonDropDownChecked, ttbSplitButtonDropDownCheckedHot,
    ttbSeparatorNormal, ttbSeparatorHot, ttbSeparatorPressed, ttbSeparatorDisabled, ttbSeparatorChecked, ttbSeparatorCheckedHot,
    ttbSeparatorVertNormal, ttbSeparatorVertHot, ttbSeparatorVertPressed, ttbSeparatorVertDisabled, ttbSeparatorVertChecked, ttbSeparatorVertCheckedHot
  );

{ ToolTip }

  TThemedToolTip = (
    tttToolTipDontCare,
    tttToolTipRoot,
    tttStandardNormal, tttStandardLink,
    tttStandardTitleNormal, tttStandardTitleLink,
    tttBaloonNormal, tttBaloonLink,
    tttBaloonTitleNormal, tttBaloonTitleLink,
    tttCloseNormal, tttCloseHot, tttClosePressed
  );

{ TrackBar }

  TThemedTrackBar = (
    ttbTrackBarDontCare,
    ttbTrackBarRoot,
    ttbTrack,
    ttbTrackVert,
    ttbThumbNormal, ttbThumbHot, ttbThumbPressed, ttbThumbFocused, ttbThumbDisabled,
    ttbThumbBottomNormal, ttbThumbBottomHot, ttbThumbBottomPressed, ttbThumbBottomFocused, ttbThumbBottomDisabled,
    ttbThumbTopNormal, ttbThumbTopHot, ttbThumbTopPressed, ttbThumbTopFocused, ttbThumbTopDisabled,
    ttbThumbVertNormal, ttbThumbVertHot, ttbThumbVertPressed, ttbThumbVertFocused, ttbThumbVertDisabled,
    ttbThumbLeftNormal, ttbThumbLeftHot, ttbThumbLeftPressed, ttbThumbLeftFocused, ttbThumbLeftDisabled,
    ttbThumbRightNormal, ttbThumbRightHot, ttbThumbRightPressed, ttbThumbRightFocused, ttbThumbRightDisabled,
    ttbThumbTics,
    ttbThumbTicsVert
  );

{ TrayNotify }

  TThemedTrayNotify = (
    ttnTrayNotifyDontCare,
    ttnTrayNotifyRoot,
    ttnBackground,
    ttnAnimBackground
  );

{ Treeview }

  TThemedTreeview = (
    ttTreeviewDontCare,
    ttTreeviewRoot,
    ttItemNormal, ttItemHot, ttItemSelected, ttItemDisabled, ttItemSelectedNotFocus,
    ttGlyphClosed, ttGlyphOpened,
    ttBranch
  );

{ Window }

  TThemedWindow = (
    twWindowDontCare,
    twWindowRoot,
    twCaptionActive, twCaptionInactive, twCaptionDisabled,
    twSmallCaptionActive, twSmallCaptionInactive, twSmallCaptionDisabled,
    twMinCaptionActive, twMinCaptionInactive, twMinCaptionDisabled,
    twSmallMinCaptionActive, twSmallMinCaptionInactive, twSmallMinCaptionDisabled,
    twMaxCaptionActive, twMaxCaptionInactive, twMaxCaptionDisabled,
    twSmallMaxCaptionActive, twSmallMaxCaptionInactive, twSmallMaxCaptionDisabled,

    twFrameLeftActive, twFrameLeftInactive,
    twFrameRightActive, twFrameRightInactive,
    twFrameBottomActive, twFrameBottomInactive,
    twSmallFrameLeftActive, twSmallFrameLeftInactive,
    twSmallFrameRightActive, twSmallFrameRightInactive,
    twSmallFrameBottomActive, twSmallFrameBottomInactive,

    twSysButtonNormal, twSysButtonHot, twSysButtonPushed, twSysButtonDisabled,
    twMDISysButtonNormal, twMDISysButtonHot, twMDISysButtonPushed, twMDISysButtonDisabled,
    twMinButtonNormal, twMinButtonHot, twMinButtonPushed, twMinButtonDisabled,
    twMDIMinButtonNormal, twMDIMinButtonHot, twMDIMinButtonPushed, twMDIMinButtonDisabled,
    twMaxButtonNormal, twMaxButtonHot, twMaxButtonPushed, twMaxButtonDisabled,
    twCloseButtonNormal, twCloseButtonHot, twCloseButtonPushed, twCloseButtonDisabled,
    twSmallCloseButtonNormal, twSmallCloseButtonHot, twSmallCloseButtonPushed, twSmallCloseButtonDisabled,
    twMDICloseButtonNormal, twMDICloseButtonHot, twMDICloseButtonPushed, twMDICloseButtonDisabled,
    twRestoreButtonNormal, twRestoreButtonHot, twRestoreButtonPushed, twRestoreButtonDisabled,
    twMDIRestoreButtonNormal, twMDIRestoreButtonHot, twMDIRestoreButtonPushed, twMDIRestoreButtonDisabled,
    twHelpButtonNormal, twHelpButtonHot, twHelpButtonPushed, twHelpButtonDisabled,
    twMDIHelpButtonNormal, twMDIHelpButtonHot, twMDIHelpButtonPushed, twMDIHelpButtonDisabled,

    twHorzScrollNormal, twHorzScrollHot, twHorzScrollPushed, twHorzScrollDisabled,
    twHorzThumbNormal, twHorzThumbHot, twHorzThumbPushed, twHorzThumbDisabled,
    twVertScrollNormal, twVertScrollHot, twVertScrollPushed, twVertScrollDisabled,
    twVertThumbNormal, twVertThumbHot, twVertThumbPushed, twVertThumbDisabled,

    twDialog,
    twCaptionSizingTemplate,
    twSmallCaptionSizingTemplate,
    twFrameLeftSizingTemplate,
    twSmallFrameLeftSizingTemplate,
    twFrameRightSizingTemplate,
    twSmallFrameRightSizingTemplate,
    twFrameBottomSizingTemplate,
    twSmallFrameBottomSizingTemplate
  );

  TThemeData = array[TThemedElement] of HTHEME;

  PThemedDetails = ^TThemedDetails;
  TThemedDetails = record
    Element: TThemedElement;
    Part: Integer;
    State: Integer;
  end;

	TThemeSize = THEMESIZE;

const
  tsMin = TS_MIN;
  tsTrue = TS_TRUE;
  tsDraw = TS_DRAW;

{ TThemePainter is a small foot print class to provide the user with pure
	Windows XP theme related abilities like	painting elements and text or
  retrieving certain info. }

type
  TThemePainter = class(TObject)
  private
    FAvailable: Boolean;
    FControlsEnabled: Boolean;
    FWindow: TUtilityWindow;
    FThemeData: TThemeData;
    FOnThemeChange: TNotifyEvent;
    function GetTheme(Element: TThemedElement): HTHEME;
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
  protected
    procedure DoOnThemeChange; virtual;
    procedure UnloadThemeData;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDetails(Widget: TThemedButton): TThemedDetails; overload;
    function GetDetails(Widget: TThemedClock): TThemedDetails; overload;
    function GetDetails(Widget: TThemedComboBox): TThemedDetails; overload;
    function GetDetails(Widget: TThemedEdit): TThemedDetails; overload;
    function GetDetails(Widget: TThemedExplorerBar): TThemedDetails; overload;
    function GetDetails(Widget: TThemedHeader): TThemedDetails; overload;
    function GetDetails(Widget: TThemedListView): TThemedDetails; overload;
    function GetDetails(Widget: TThemedMenu): TThemedDetails; overload;
    function GetDetails(Widget: TThemedPage): TThemedDetails; overload;
    function GetDetails(Widget: TThemedProgress): TThemedDetails; overload;
    function GetDetails(Widget: TThemedRebar): TThemedDetails; overload;
    function GetDetails(Widget: TThemedScrollBar): TThemedDetails; overload;
    function GetDetails(Widget: TThemedSpin): TThemedDetails; overload;
    function GetDetails(Widget: TThemedStartPanel): TThemedDetails; overload;
    function GetDetails(Widget: TThemedStatus): TThemedDetails; overload;
    function GetDetails(Widget: TThemedTab): TThemedDetails; overload;
    function GetDetails(Widget: TThemedTaskBand): TThemedDetails; overload;
    function GetDetails(Widget: TThemedTaskBar): TThemedDetails; overload;
    function GetDetails(Widget: TThemedToolBar): TThemedDetails; overload;
    function GetDetails(Widget: TThemedToolTip): TThemedDetails; overload;
    function GetDetails(Widget: TThemedTrackBar): TThemedDetails; overload;
    function GetDetails(Widget: TThemedTrayNotify): TThemedDetails; overload;
    function GetDetails(Widget: TThemedTreeview): TThemedDetails; overload;
    function GetDetails(Widget: TThemedWindow): TThemedDetails; overload;
    function ColorToRGB(Color: TColor; Details: PThemedDetails = nil): COLORREF;
    function ContentRect(DC: HDC; const Details: TThemedDetails; BoundingRect: TRect): TRect;
    procedure DrawEdge(DC: HDC; const Details: TThemedDetails; const R: TRect; Edge, Flags: Cardinal;
      ContentRect: PRect = nil);
    procedure DrawElement(DC: HDC; const Details: TThemedDetails; const R: TRect; ClipRect: PRect = nil);
    procedure DrawIcon(DC: HDC; const Details: TThemedDetails; const R: TRect; himl: HIMAGELIST; Index: Integer);
    procedure DrawParentBackground(Window: HWND; Target: HDC; Details: PThemedDetails; OnlyIfTransparent: Boolean;
      Bounds: PRect = nil);
    procedure DrawText(DC: HDC; const Details: TThemedDetails; const S: WideString; R: TRect; Flags, Flags2: Cardinal);
    function HasTransparentParts(Details: TThemedDetails): Boolean;
    function PartSize(DC: HDC; const Details: TThemedDetails; Size: TThemeSize = tsDraw): TSize;
    procedure UpdateThemes;
    property Theme[Element: TThemedElement]: HTHEME read GetTheme;
    property Available: Boolean read FAvailable;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property OnThemeChange: TNotifyEvent read FOnThemeChange write FOnThemeChange;
  end;

function ThemePainter: TThemePainter;

{ Themed drawing routines }

procedure DrawThemeBorder(DC: HDC; Color: TColor; const Rect: TRect; State: TDrawState);
procedure DrawThemeButton(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeThinButton(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeNode(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeClose(DC: HDC; const Rect: TRect; State: TDrawState;
  Color: TColor = clWindowFrame);
procedure DrawThemeToolClose(DC: HDC; const Rect: TRect; State: TDrawState;
  Color: TColor = clWindowFrame);
procedure DrawThemePin(DC: HDC; const Rect: TRect; State: TDrawState;
  Color: TColor = clWindowFrame);
procedure DrawThemeArrow(DC: HDC; Direction: TDirection; const Rect: TRect; State: TDrawState;
  Color: TColor = clWindowFrame; Adjust: Integer = 0);
procedure DrawThemeScroll(DC: HDC; Direction: TDirection; const Rect: TRect; State: TDrawState);
procedure DrawThemeGrip(DC: HDC; const Rect: TRect);
procedure DrawThemeGroupBox(DC: HDC; const Text: string; const Rect: TRect; State: TDrawState);
procedure DrawThemeExpandableBox(DC: HDC; const Text: string; const Rect: TRect;	State: TDrawState);
procedure DrawThemeHorzThumb(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeHorzSplit(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeVertThumb(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeVertSplit(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeCloseBox(DC: HDC; const Text: string; const Rect: TRect; State: TDrawState);
procedure DrawThemeStatus(DC: HDC; const Text: string; const Rect: TRect);

{ Alpha blending suport routines }

function CreateBlendSection(DC: HDC; Width: Integer; Height: Integer): HBITMAP;

function BlendImages(Source1, Source2, Dest: HBITMAP; Weight: Byte): Boolean;

procedure AlphaBlend(Dest: HDC; DestX, DestY, DestWidth,
  DestHeight: Integer; Source: HDC; SourceX, SourceY, SourceWidth,
  SourceHeight: Integer; SourceWeight: Byte);

{ Resource conversion routines }

function HexToStream(const HexData: string): TStream;
function StreamToHex(Stream: TStream): string;
procedure LoadGraphicResource(Graphic: TGraphic; Ident: Integer);

implementation

{$R GLYPHS.RES}

function CreateFastBitmap(Width, Height: Integer): TFastBitmap;
var
  BitmapInfo: TBitmapinfo;
begin
  Result.DC := CreateCompatibleDC(0);
  FillChar(BitmapInfo, SizeOf(BitmapInfo), #0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(BitmapInfo.bmiHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := 24;
    biCompression := BI_RGB;
  end;
  with Result do
    Handle := CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, Bits, 0, 0);
  Result.Width := Width;
  Result.Height := Height;
  with Result do
    SelectObject(DC, Handle);
end;

procedure DestroyFastBitmap(const Bitmap: TFastBitmap);
begin
  DeleteDC(Bitmap.DC);
  DeleteObject(Bitmap.Handle);
end;

function DirectionToAlignment(Direction: TDirection): TAlignment;
begin
  Result := taLeftJustify;
  case Direction of
    drLeft, drUp, drDown: Result := taLeftJustify;
    drCenter: Result := taCenter;
    drRight: Result := taRightJustify;
  end;
end;

function AlignmentToDirection(Alignment: TAlignment): TDirection;
begin
  Result := drLeft;
  case Alignment of
    taLeftJustify: Result := drLeft;
    taCenter: Result := drRight;
    taRightJustify: Result := drCenter;
  end;
end;

var
  DeviceContext: HDC;

function InitializeDevice(DC: HDC): HDC;
begin
  Result := DeviceContext;
  DeviceContext := DC;
end;

procedure FinalizeDevice(DC: HDC);
begin
  DeviceContext := DC;
end;

function IntToDevice(Value: Integer; Angle: Integer = 0): Double;
var
  Ratio: Double;
begin
  case Angle of
    0:
      begin
        Value := Value + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
        Ratio := 1 / GetDeviceCaps(DeviceContext, LOGPIXELSX);
      end;
    90:
      begin
        Value := Value + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
        Ratio := 1 / GetDeviceCaps(DeviceContext, LOGPIXELSY);
      end;
  else
    Ratio := 0;
  end;
  Result := Value * Ratio;
end;

function PointToDevice(const Point: TPoint): TFloatPoint;
begin
  Result.x := (Point.x + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.y := (Point.y + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function PointToDevice(X, Y: Integer): TFloatPoint;
begin
  Result.x := (X + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.y := (Y + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function RectToDevice(const Rect: TRect): TFloatRect;
begin
  Result.Left := (Rect.Left + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.Top := (Rect.Top + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
  Result.Right := (Rect.Right + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.Bottom := (Rect.Bottom + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function RectToDevice(ALeft, ATop, ARight, ABottom: Integer): TFloatRect;
begin
  Result.Left := (ALeft + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.Top := (ATop + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
  Result.Right := (ARight + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.Bottom := (ABottom + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function PolygonToDevice(const Polygon: TPolygon): TFloatPolygon;
var
  I: Integer;
begin
  SetLength(Result, Length(Polygon));
  for I := Low(Polygon) to High(Polygon) do
    Result[I] := PointToDevice(Polygon[I]);
end;

function DeviceToInt(const Value: Double; Angle: Integer = 0): Integer;
var
  Delta: Integer;
  Ratio: Double;
begin
  case Angle of
    0:
      begin
        Delta := GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
        Ratio := GetDeviceCaps(DeviceContext, LOGPIXELSX);
      end;
    90:
      begin
        Delta := GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
        Ratio := GetDeviceCaps(DeviceContext, LOGPIXELSY);
      end;
  else
    Delta := 0;
    Ratio := 0;
  end;
  Result := Trunc(Value * Ratio) - Delta;
end;

function DeviceToPoint(const Point: TFloatPoint): TPoint;
begin
  Result.x := Trunc(Point.x * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.y := Trunc(Point.y * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
end;

function DeviceToPoint(const X, Y: Double): TPoint;
begin
  Result.x := Trunc(X * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.y := Trunc(Y * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
end;

function DeviceToRect(const Rect: TFloatRect): TRect;
begin
  Result.Left := Trunc(Rect.Left * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.Top := Trunc(Rect.Top * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
  Result.Right := Trunc(Rect.Right * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.Bottom := Trunc(Rect.Bottom * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
end;

function DeviceToRect(const ALeft, ATop, ARight, ABottom: Double): TRect;
begin
  Result.Left := Trunc(ALeft * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.Top := Trunc(ATop * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
  Result.Right := Trunc(ARight * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.Bottom := Trunc(ABottom * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
end;

function DeviceToPolygon(const Polygon: TFloatPolygon): TPolygon;
var
  I: Integer;
begin
  SetLength(Result, Length(Polygon));
  for I := Low(Polygon) to High(Polygon) do
    Result[I] := DeviceToPoint(Polygon[I]);
end;

procedure OffsetRect(var Rect: TRect; X, Y: Integer);
begin
  Rect.Left := Rect.Left + X;
  Rect.Top := Rect.Top + Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

procedure OffsetRect(var Rect: TFloatRect; const X, Y: Double);
begin
  Rect.Left := Rect.Left + X;
  Rect.Top := Rect.Top + Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

procedure InflateRect(var Rect: TRect; X, Y: Integer);
begin
  Rect.Left := Rect.Left - X;
  Rect.Top := Rect.Top - Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

procedure InflateRect(var Rect: TFloatRect; const X, Y: Double);
begin
  Rect.Left := Rect.Left - X;
  Rect.Top := Rect.Top - Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

function HeightOf(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function HeightOf(const Rect: TFloatRect): Double;
begin
  Result := Rect.Bottom - Rect.Bottom;
end;

function WidthOf(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function WidthOf(const Rect: TFloatRect): Double;
begin
  Result := Rect.Right - Rect.Left;
end;

function MoveRect(const Rect: TRect; X, Y: Integer): TRect;
begin
  Result := Rect;
  with Result do
  begin
    Right := Right - Left + X;
    Bottom := Bottom - Top + Y;
    Left := X;
    Top := Y;
  end;
end;

procedure Slide(var Rect: TRect; Direction: TDirection = drDown;
  Distance: Integer = 0);
begin
  case Direction of
    drLeft: OffsetRect(Rect, -WidthOf(Rect) - Distance, 0);
    drUp: OffsetRect(Rect, 0, -HeightOf(Rect) - Distance);
    drRight: OffsetRect(Rect, WidthOf(Rect) + Distance, 0);
    drDown: OffsetRect(Rect, 0, HeightOf(Rect) + Distance);
  end;
end;

procedure Slide(var Rect: TFloatRect; Direction: TDirection = drDown;
  Distance: Double = 0);
begin
  case Direction of
    drLeft: OffsetRect(Rect, -WidthOf(Rect) - Distance, 0);
    drUp: OffsetRect(Rect, 0, -HeightOf(Rect) - Distance);
    drRight: OffsetRect(Rect, WidthOf(Rect) + Distance, 0);
    drDown: OffsetRect(Rect, 0, HeightOf(Rect) + Distance);
  end;
end;

function Chamfer(const Rect: TFloatRect; const Size: Double;
  Corners: TCorners): TFloatPolygon;
var
  Corner: TCorner;
  I: Integer;
begin
  I := 0;
  for Corner := Low(TCorner) to High(TCorner) do
    if Corner in Corners then
      Inc(I);
  SetLength(Result, 4 + I);
  I := 0;
  for Corner := Low(TCorner) to High(TCorner) do
  begin
    case Corner of
      cnTopLeft:
        begin
          Result[I] := Rect.TopLeft;
          if Corner in Corners then
          begin
            Result[I].Y := Rect.Top + Size;
            Inc(I);
            Result[I].X := Rect.Left + Size;
            Result[I].Y := Rect.Top;
          end;
        end;
      cnTopRight:
        begin
          Result[I].X := Rect.Right;
          Result[I].Y := Rect.Top;
          if Corner in Corners then
          begin
            Result[I].X := Rect.Right - Size;
            Inc(I);
            Result[I].X := Rect.Right;
            Result[I].Y := Rect.Top + Size;
          end;
        end;
      cnBottomRight:
        begin
          Result[I] := Rect.BottomRight;
          if Corner in Corners then
          begin
            Result[I].Y := Rect.Bottom - Size;
            Inc(I);
            Result[I].X := Rect.Right - Size;
            Result[I].Y := Rect.Bottom;
          end;
        end;
      cnBottomLeft:
        begin
          Result[I].X := Rect.Left;
          Result[I].Y := Rect.Bottom;
          if Corner in Corners then
          begin
            Result[I].X := Rect.Left + Size;
            Inc(I);
            Result[I].X := Rect.Left;
            Result[I].Y := Rect.Bottom - Size;
          end;
        end;
    end;
    Inc(I);
  end;
end;

function GetTextAscent: Double;
var
  TextMetric: TTextMetric;
begin
  GetTextMetrics(DeviceContext, TextMetric);
  Result := (TextMetric.tmHeight - TextMetric.tmDescent) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function GetTextDescent: Double;
var
  TextMetric: TTextMetric;
begin
  GetTextMetrics(DeviceContext, TextMetric);
  Result := TextMetric.tmDescent / GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function GetTextBaseline: Double;
var
  TextMetric: TTextMetric;
begin
  GetTextMetrics(DeviceContext, TextMetric);
  with TextMetric do
    Result := (tmHeight + tmExternalLeading) /
      GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

{ Color manipulation functions }

type
  TQuad = array[0..3] of Byte;

function QuadRange(Value: Integer): Byte;
begin
  if Value > 255 then
    Result := 255
  else if Value < 0 then
    Result := 0
  else
    Result := Value;
end;

function Blend(ForeColor: TColor; BackColor: TColor; Percent: Byte = 50): TColor;
var
  RGB: TQuad absolute Result;
  F: TQuad;
  B: TQuad;
begin
  F := TQuad(ColorToRGB(ForeColor));
  B := TQuad(ColorToRGB(BackColor));
  RGB[0] := (F[0] * Percent div 100) + (B[0] * (100 - Percent) div 100);
  RGB[1] := (F[1] * Percent div 100) + (B[1] * (100 - Percent) div 100);
  RGB[2] := (F[2] * Percent div 100) + (B[2] * (100 - Percent) div 100);
end;

function Delta(Color: TColor; Value: Integer): TColor;
var
  RGB: TRGBQuad absolute Result;
begin
  RGB := TRGBQuad(ColorToRGB(Color));
  with RGB do
  begin
     rgbBlue := QuadRange(rgbBlue + Value);
     rgbGreen := QuadRange(rgbGreen + Value);
     rgbRed := QuadRange(rgbRed + Value);
  end;
end;

function Scale(Color: TColor; Value: Integer): TColor;
var
  RGB: TRGBQuad absolute Result;
  Factor: Double;
begin
  RGB := TRGBQuad(ColorToRGB(Color));
  Factor := Value / 100;
  with RGB do
  begin
     rgbBlue := QuadRange(Trunc(rgbBlue * Factor));
     rgbGreen := QuadRange(Trunc(rgbGreen * Factor));
     rgbRed := QuadRange(Trunc(rgbRed * Factor));
  end;
end;

function GetBorder: Integer;
begin
	if ThemePainter.Enabled then
  	Result := 1
  else
  	Result := GetSystemMetrics(SM_CXEDGE);
end;

function AverageColor(Color: TColor): Byte;
var
	RGB: TRGBQuad absolute Color;
begin
  Color := ColorToRGB(Color);
  Result := (RGB.rgbBlue + RGB.rgbGreen + RGB.rgbRed) div 3;
end;

function GetTextColor(Background: TColor): TColor;
var
  L, H: TColor;
begin
  if AverageColor(clWindow) > AverageColor(clWindowFrame) then
  begin
	  L := ColorToRGB(clWindowFrame);
  	H := ColorToRGB(clWindow);
	end
  else
  begin
	  H := ColorToRGB(clWindowFrame);
  	L := ColorToRGB(clWindow);
	end;
  if AverageColor(Background) > 128 then
  	Result := L
	else
  	Result := H;
end;

{ Get object functions }

function GetBitmap(ForeColor: TColor; BackColor: TColor): TBitmap;
var
  PixelColors: array[Boolean] of TColor;
  Col: Integer;
  Row: Integer;
begin
  PixelColors[False] := ForeColor;
  PixelColors[True] := BackColor;
  Result := TBitmap.Create;
  with Result do
  begin
    Height := 8;
    Width := 8;
    for Col := 0 to Width - 1 do
      for Row := 0 to Height - 1 do
        Canvas.Pixels[Col, Row] := PixelColors[Odd(Col + Row)];
    HandleType := bmDDB;
  end;
end;

function GetBitmap(Resource: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.LoadFromResourceID(hInstance, Resource);
  except
    Result.Free;
    raise;
  end;
end;

function GetBrush(Bitmap: TBitmap): HBRUSH;
var
  LogBrush: TLogBrush;
begin
  LogBrush.lbStyle := BS_PATTERN;
  LogBrush.lbColor := 0;
  LogBrush.lbHatch := Bitmap.Handle;
  Result := CreateBrushIndirect(LogBrush);
end;

function GetBrush(Color: TColor; Style: TBrushStyle = bsSolid): HBRUSH;
var
  LogBrush: TLogBrush;
begin
  Result := 0;
  LogBrush.lbStyle := BS_HATCHED;
  LogBrush.lbColor := ColorToRGB(Color);
  with LogBrush do
    case Style of
      bsSolid: Result := CreateSolidBrush(lbColor);
      bsClear: lbStyle := BS_HOLLOW;
    else
      lbHatch := Ord(Style) - Ord(bsHorizontal);
    end;
  if Result = 0 then
    Result := CreateBrushIndirect(LogBrush);
end;

function GetPen(Color: TColor; Width: Integer = 0; Square: Boolean = False): HPEN;
var
  LogBrush: TLogBrush;
begin
  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := ColorToRGB(Color);
  if Square then
    Result := ExtCreatePen(PS_GEOMETRIC or PS_ENDCAP_SQUARE or PS_JOIN_BEVEL, Width,
      LogBrush, 0, nil)
  else
    Result := CreatePen(PS_SOLID, Width, LogBrush.lbColor)
end;

function GetPen(Color: TColor; Style: TPenStyle): HPEN;
const
  PenStyles: array[psSolid..psInsideFrame] of Cardinal =
    (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL,
     PS_INSIDEFRAME);
begin
  if Style > psInsideFrame then
  begin
    Result := 0;
    Exit;
  end;
  Result := CreatePen(PenStyles[Style], 0, ColorToRGB(Color));
end;

function GetRegion(DC: HDC): HRGN;
var
  Wnd: HWND;
  Rect: TRect;
begin
  Wnd := WindowFromDC(DC);
  if Wnd <> 0 then
  begin
    GetWindowRect(Wnd, Rect);
    with Rect do
      Result := CreateRectRgn(Left, Top, Right, Bottom);
    GetClipRgn(DC, Result);
  end
  else
    Result := 0;
end;

function GetRegion(const Polygon: TPolygon): HRGN;
begin
  Result := 0;
  if Length(Polygon) > 2 then
    Result := CreatePolygonRgn(Pointer(Polygon)^, Length(Polygon), WINDING);
end;

function GetFont(const Name: string; Size: Integer; Italic: Boolean = False;
  Underlined: Boolean = False; Weight: Integer = 0;  Charset: TFontCharset = 0): HFONT;
const
  BoolBytes: array[Boolean] of Byte = (0, 1);
var
  LogFont: TLogFont;
  DC: HDC;
  S: string;
begin
  FillChar(LogFont, SizeOf(TLogFont), #0);
  DC := GetDC(0);
  LogFont.lfHeight := -MulDiv(Size, GetDeviceCaps(DC, LOGPIXELSY), 72);
  ReleaseDC(0, DC);
  LogFont.lfItalic := BoolBytes[Italic];
  LogFont.lfUnderline := BoolBytes[Underlined];
  LogFont.lfWeight := Weight;
  LogFont.lfCharSet := Charset;
  LogFont.lfQuality := CLEARTYPE_QUALITY or PROOF_QUALITY;
  S := Name;
  if Length(S) > 31 then
    SetLength(S, 31);
  StrPCopy(LogFont.lfFaceName, PChar(S));
  Result := CreateFontIndirect(LogFont);
end;

{ Scratch object management }

var
  ScratchFont: TFont;
  ScratchBitmap: TBitmap;

procedure SwapFont(A, B: TFont);
begin
  if ScratchFont = nil then
  	ScratchFont := TFont.Create;
  ScratchFont.Assign(A);
  A.Assign(B);
  B.Assign(ScratchFont);
end;

procedure ColorMaskBlt(DC: HDC; const Rect, Mask: TRect; ForeColor, BackColor: TColor);
const
  Rop: array[Boolean] of Cardinal = (SRCCOPY, SRCAND);
var
  MemDC: HDC;
  A, B: TRect;
  Brush: HBrush;
  W, H: Integer;
begin
  if IsRectEmpty(Rect) or IsRectEmpty(Mask) then Exit;
  MemDC := ScratchBitmap.Canvas.Handle;
  BitBlt(MemDC, 0, 0, WidthOf(Mask), HeightOf(Mask), 0, 0, 0, DSTINVERT);
  A := Mask;
  OffsetRect(A, WidthOf(Mask), 0);
  Brush := CreateSolidBrush(ColorToRGB(ForeColor));
  FillRect(MemDC, A, Brush);
  DeleteObject(Brush);
  BitBlt(MemDC, A.Left, A.Top, WidthOf(Mask), HeightOf(Mask), MemDC, 0, 0,
    SRCAND);
  BitBlt(MemDC, 0, 0, WidthOf(Mask), HeightOf(Mask), 0, 0, 0, DSTINVERT);
  B := A;
  OffsetRect(B, WidthOf(Mask), 0);
  Brush := CreateSolidBrush(ColorToRGB(BackColor));
  FillRect(MemDC, B, Brush);
  DeleteObject(Brush);
  BitBlt(MemDC, B.Left, B.Top, WidthOf(Mask), HeightOf(Mask), MemDC, 0, 0,
    SRCAND);
  BitBlt(MemDC, A.Left, A.Top, WidthOf(Mask), HeightOf(Mask), MemDC, B.Left,
    B.Top, SRCPAINT);
  B := Mask;
  W := WidthOf(Rect);
  if WidthOf(B) > W then
  begin
    Inc(A.Left, (WidthOf(B) - W) div 2 + 1);
    B.Right := B.Left + W;
  end;
  H := HeightOf(Rect);
  if HeightOf(B) > H then
  begin
    Inc(A.Top, (HeightOf(B) - H) div 2 + 1);
    B.Bottom := B.Top + H;
  end;
  BitBlt(DC, Rect.Left + W div 2 - WidthOf(B) div 2, Rect.Top +
    H div 2 - HeightOf(B) div 2, WidthOf(B), HeightOf(B),
    MemDC, A.Left, A.Top, Rop[BackColor = $FFFFFF]);
end;

procedure FillColorMaskBlt(DC: HDC; const Rect, Mask: TRect;
  ForeColor, BackColor: TColor);
var
  MemDC: HDC;
  Brush: HBRUSH;
begin
  MemDC := ScratchBitmap.Canvas.Handle;
  Brush := SelectObject(MemDC, GetStockObject(BLACK_BRUSH));
  with Mask do
  begin
    FloodFill(MemDC, Left + WidthOf(Mask) div 2, Top + HeightOf(Mask) div 2,
      GetSysColor(COLOR_BTNFACE));
    SelectObject(MemDC, GetStockObject(WHITE_BRUSH));
    FloodFill(MemDC, Left + 1, Top + 1, 0);
    FloodFill(MemDC, Left, Top, $FFFFFF);
  end;
  SelectObject(DC, Brush);
  ColorMaskBlt(DC, Rect, Mask, ForeColor, BackColor);
end;

{ Utility procedures }

procedure OverwriteObject(DC: HDC; Obj: HGDIOBJ);
begin
  DeleteObject(SelectObject(DC, Obj));
end;

procedure SelectClipRect(DC: HDC; const Rect: TRect; Mode: Integer);
var
  Region: HRGN;
begin
  with Rect do
    Region := CreateRectRgn(Left, Top, Right, Bottom);
  ExtSelectClipRgn(DC, Region, Mode);
  DeleteObject(Region);
end;

{ Draw routines }

type
  TPaintBitmapProc = procedure (DC: HDC; Width, Height: Integer;
    Data: Pointer);

procedure PaintBitmap(Width, Height: Integer; Data: Pointer;
  Proc: TFarProc);
var
  DC: HDC;
  Bitmap: HBITMAP;
  PriorBitmap: HBITMAP;
begin
  DC := GetDC(0);
  Bitmap := CreateCompatibleBitmap(DC, Width, Height);
  ReleaseDC(0, DC);
  DC := CreateCompatibleDC(0);
  PriorBitmap := SelectObject(DC, Bitmap);
  try
    if Proc <> nil then
      TPaintBitmapProc(Proc)(DC, Width, Height, Data);
  finally
    SelectObject(DC, PriorBitmap);
    DeleteDC(DC);
    DeleteObject(Bitmap);
  end;
end;

procedure DrawArrow(DC: HDC; Rect: TRect; Direction: TDirection;
  ForeColor, BackColor: TColor; Size: Integer = 0; Enabled: Boolean = True);
var
  MemDC: HDC;
  Mask: TRect;
  Style: Cardinal;
  Brush: HBRUSH;
begin
  with ScratchBitmap do
  begin
    MemDC := Canvas.Handle;
    BitBlt(MemDC, 0, 0, Width, Height, 0, 0, 0, WHITENESS);
  end;
  if Size > 0 then
    Mask := GetRect(0, 0, Size, Size)
  else
    Mask := GetRect(0, 0, GetSystemMetrics(SM_CXVSCROLL),
      GetSystemMetrics(SM_CYVSCROLL));
  case Direction of
    drLeft: Style := DFCS_SCROLLLEFT;
    drUp: Style := DFCS_SCROLLUP;
    drRight: Style := DFCS_SCROLLRIGHT;
  else
    Style := DFCS_SCROLLDOWN;
  end;
  if Enabled then
  begin
    Style := Style or DFCS_MONO or DFCS_FLAT;
    DrawFrameControl(MemDC, Mask, DFC_SCROLL, Style);
    FillColorMaskBlt(DC, Rect, Mask, ForeColor, BackColor);
  end
  else
  begin
    ExcludeClipRect(DC, Rect.Right, Rect.Top, Rect.Right + 2, Rect.Bottom);
    OffsetRect(Rect, -1, 0);
    Style := Style or DFCS_INACTIVE or DFCS_FLAT;
    DrawFrameControl(MemDC, Mask, DFC_SCROLL, Style);
    Brush := SelectObject(MemDC, GetSysColorBrush(COLOR_BTNFACE));
    FloodFill(MemDC, 0, 0, GetSysColor(COLOR_BTNFACE));
    OverwriteObject(MemDC, Brush);
    BitBlt(DC, Rect.Left + WidthOf(Rect) div 2 - WidthOf(Mask) div 2, Rect.Top +
      HeightOf(Rect) div 2 - HeightOf(Mask) div 2, WidthOf(Mask), HeightOf(Mask),
      MemDC, 0, 0, SRCCOPY);
  end;
end;

procedure DrawBox(DC: HDC; Color: TColor; var Rect: TRect);
var
  Pen: HPEN;
  Point: TPoint;
begin
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 1, ColorToRGB(Color)));
  with Rect do
  begin
    MoveToEx(DC, Left, Top, @Point);
    LineTo(DC, Right - 1, Top);
    LineTo(DC, Right - 1, Bottom - 1);
    LineTo(DC, Left, Bottom - 1);
    LineTo(DC, Left, Top);
    MoveToEx(DC, Point.x, Point.y, nil);
  end;
  InflateRect(Rect, -1, -1);
  OverwriteObject(DC, Pen);
end;

procedure DrawCheckBox(DC: HDC; Rect: TRect; Checked: Boolean;
  ForeColor, BackColor: TColor; Flat: Boolean = True);
const
  CheckedState: array[Boolean] of Cardinal = (0, DFCS_CHECKED);
var
  Mask: TRect;
  MemDC: HDC;
  I: Integer;
begin
	I := Round(GetDeviceCaps(DC, LOGPIXELSX) / 96 * 13);
  // Mask := GetRect(0, 0, 13, 13);
  Mask := GetRect(0, 0, I, I);
  if Flat then
  begin
    MemDC := ScratchBitmap.Canvas.Handle;
    DrawFrameControl(MemDC, Mask, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_MONO or
      CheckedState[Checked]);
    ColorMaskBlt(DC, Rect, Mask, ForeColor, BackColor);
  end
  else
  begin
    OffsetRect(Mask, Rect.Left + WidthOf(Rect) div 2 - WidthOf(Mask) div 2,
      Rect.Top + HeightOf(Rect) div 2 - HeightOf(Mask) div 2);
    DrawFrameControl(DC, Mask, DFC_BUTTON, DFCS_BUTTONCHECK or
      CheckedState[Checked]);
  end;
end;

procedure DrawCheckText(DC: HDC; const Text: string; Rect: TRect;
  Direction: TDirection; Checked: Boolean; ForeColor, BackColor: TColor;
  Flat: Boolean = True);
var
  PriorColor: COLORREF;
begin
  PriorColor := SetTextColor(DC, ColorToRGB(ForeColor));
  OffsetRect(Rect, 0, -1);
  case Direction of
    drLeft:
      begin
        DrawCaption(DC, Text, Rect, drLeft);
        SetTextColor(DC, PriorColor);
        Inc(Rect.Left, CalculateCaptionSize(DC, Text).cx + 5);
        Rect.Right := Rect.Left + 13;
      end;
    drRight:
      begin
        Inc(Rect.Left, 18);
        DrawCaption(DC, Text, Rect, drLeft);
        Dec(Rect.Left, 18);
        SetTextColor(DC, PriorColor);
        Rect.Right := Rect.Left + 13;
      end;
  end;
  DrawCheckBox(DC, Rect, Checked, ForeColor, BackColor, Flat);
end;

procedure DrawSortArrow(DC: HDC; Rect: TRect; Direction: TDirection);
var
  Point: TPoint;
  X, Y: Integer;
  Pen: HPEN;
begin
  with Rect do
  begin
    Pen := SelectObject(DC, CreatePen(PS_SOLID, 0,
      GetSysColor(COLOR_BTNHIGHLIGHT)));
    Point := Rect.TopLeft;
    X := Left + (Right - Left) div 2 + 3;
    Y := Top + (Bottom - Top) div 2 + 3;
    case Direction of
      drUp:
        begin
         MoveToEx(DC, X, Y, @Point);
         LineTo(DC, X + 7, Y);
         LineTo(DC, X + 4, Y - 6);
         LineTo(DC, X + 4, Y - 7);
       end;
      drDown:
        begin
         MoveToEx(DC, X + 4, Y, @Point);
         LineTo(DC, X + 7, Y - 6);
        end;
    end;
    OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW)));
    case Direction of
      drUp:
        begin
          MoveToEx(DC, X + 3, Y - 6, nil);
          LineTo(DC, X, Y);
          MoveToEx(DC, Point.X, Point.Y, nil);
        end;
      drDown:
        begin
          LineTo(DC, X, Y - 6);
          LineTo(DC, X + 3, Y);
          LineTo(DC, X + 3, Y + 1);
        end;
    end;
    OverwriteObject(DC, Pen);
  end;
end;

function CalculateCaptionRect(DC: HDC; const Text: string; Rect: TRect;
  Direction: TDirection): TRect;
var
  Size: TSize;
begin
  FillChar(Size, SizeOf(Size), #0);
  if Text = '' then
	  GetTextExtentPoint32(DC, ' ', 1, Size)
  else
	  GetTextExtentPoint32(DC, PChar(Text), Length(Text), Size);
  Result := Rect;
  with Result do
  begin
    { if Right - Left < Size.cx then
      Size.cx := Right - Left; }
    Top := Top + (Bottom - Top - Size.cy) div 2;
    Bottom := Top + Size.cy;
    case Direction of
      drLeft:
        Right := Rect.Left + Size.cx;
      drCenter:
        begin
          Left := (Rect.Right - Rect.Left - Size.cx) div 2;
          Right := Left + Size.cx;
        end;
      drRight:
        Left := Rect.Right - Size.cx;
      drFill:
      	begin
        	Left := Left + WidthOf(Rect) div 2 - Size.cx div 2;
          Right := Left + Size.cx;
        end;
    end;
  end;
end;

function CalculateCaptionSize(DC: HDC; const Text: string): TSize;
begin
  FillChar(Result, SizeOf(TSize), #0);
  GetTextExtentPoint32(DC, PChar(Text), Length(Text), Result);
end;

function CalculateMemoHeight(DC: HDC; const Text: string; Width: Integer): Integer;
var
  Rect: TRect;
begin
  Rect := GetRect(0, 0, Width, 1);
  Result := DrawText(DC, PChar(Text), -1, Rect, DT_LEFT or DT_TOP or
    DT_WORDBREAK or DT_CALCRECT);
end;

procedure DrawCaption(DC: HDC;  const Caption: string; Rect: TRect;
  Direction: TDirection; Enabled: Boolean = True);
var
  DrawRect: TRect;
  PriorMode: Integer;
  PriorColor: COLORREF;
begin
  DrawRect := Rect;
  PriorMode := SetBkMode(DC, TRANSPARENT);
  if Enabled then
    DrawText(DC, PChar(Caption), -1, DrawRect, Directions[Direction])
  else
  begin
    OffsetRect(DrawRect, 1, 1);
    PriorColor := SetTextColor(DC, GetSysColor(COLOR_BTNHIGHLIGHT));
    DrawText(DC, PChar(Caption), -1, DrawRect, Directions[Direction]);
    OffsetRect(DrawRect, -1, -1);
    SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT));
    DrawText(DC, PChar(Caption), -1, DrawRect, Directions[Direction]);
    SetTextColor(DC, PriorColor);
  end;
  SetBkMode(DC, PriorMode);
end;

procedure DrawFocus(DC: HDC; Rect: TRect; BorderX: Integer = 0; BorderY: Integer = 0);
var
  PriorColor: COLORREF;
begin
	InflateRect(Rect, BorderX, BorderY);
  PriorColor := SetTextColor(DC, 0);
  DrawFocusRect(DC, Rect);
  SetTextColor(DC, PriorColor);
end;

procedure DrawArrow(DC: HDC; Rect: TRect; Direction: TDirection;
	Color: TColor = clWindowFrame; Enabled: Boolean = True);
var
	Glyph: TGlyphKind;
begin
	case Direction of
  	drLeft: Glyph := gkArrowLeft;
    drRight:Glyph := gkArrowRight;
  	drUp: Glyph := gkArrowUp;
  	drDown: Glyph := gkArrowDown;
  else
  	Exit;
  end;
  if not Enabled then
    Inc(Glyph, 4);
	GlyphDraw(DC, Rect, Glyph, Color);
end;

procedure DrawClose(DC: HDC; Rect: TRect; Color: TColor = clWindowFrame);
begin
	GlyphDraw(DC, Rect, gkClose, Color);
end;

procedure DrawDivider(DC: HDC; Rect: TRect; Kind: TDrawDividerKind);
var
  PriorPen: HPEN;
  PriorPoint: TPoint;
begin
  with Rect do
  begin
    MoveToEx(DC, Left, Top, @PriorPoint);
    PriorPen := SelectObject(DC, CreatePen(PS_SOLID, 0,
      GetSysColor(COLOR_BTNSHADOW)));
    if Kind = ddVert then
    begin
      LineTo(DC, Right, Top);
      MoveToEx(DC, Left, Top + 1, nil);
    end
    else
    begin
      LineTo(DC, Left, Bottom);
      MoveToEx(DC, Left + 1, Top, nil);
    end;
    OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNHILIGHT)));
    if Kind = ddVert then
      LineTo(DC, Right, Top + 1)
    else
      LineTo(DC, Left + 1, Bottom);
    OverwriteObject(DC, PriorPen);
    with PriorPoint do
      MoveToEx(DC, X, Y, nil);
  end;
end;

procedure DrawEllipsis(DC: HDC; Rect: TRect; Enabled: Boolean = True);

  procedure DrawDots(Dot: TRect; Color: TColor);
  var
    I: Integer;
  begin
    with Rect do
      OffsetRect(Dot, Left + (WidthOf(Rect) - 10) div 2, Top + HeightOf(Rect) - 6);
    for I := 0 to 2 do
    begin
      FillRect(DC, Dot, Color + 1);
      OffsetRect(Dot, 4, 0);
    end;
  end;

begin
  FillRect(DC, Rect, COLOR_BTNFACE + 1);
  if Enabled then
    DrawDots(GetRect(0, 0, 2, 2), COLOR_WINDOWTEXT)
  else
  begin
    DrawDots(GetRect(1, 1, 3, 3), COLOR_BTNHIGHLIGHT);
    DrawDots(GetRect(0, 0, 2, 2), COLOR_BTNSHADOW);
  end;
end;

procedure DrawFrame(DC: HDC; Rect: TRect; State: TDrawFrameState);
var
  PriorPen: HPEN;
  PriorPoint: TPoint;
begin
  if State = dfFlat then
    Exit;
  with Rect do
  begin
    Dec(Right);
    Dec(Bottom);
    MoveToEx(DC, Left, Top, @PriorPoint);
    PriorPen := SelectObject(DC, CreatePen(PS_SOLID, 0,
      GetSysColor(COLOR_BTNFACE)));
    case State of
      dfFocus:
        begin
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_WINDOWFRAME)));
          LineTo(DC, Right, Top);
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Top);
        end;
      dfFramed:
        begin
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Left, Top);
          InflateRect(Rect, -1, -1);
          MoveToEx(DC, Left, Top, nil);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DLIGHT)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DLIGHT)));
          LineTo(DC, Left, Top);
        end;
     dfHover:
        begin
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNFACE)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNFACE)));
          LineTo(DC, Left, Top);
        end;
     dfRaised:
        begin
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Left, Top);
        end;
     dfLowered:
        begin
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Left, Top);
        end;
     dfPressed:
        begin
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNFACE)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Left, Top);
        end;
     dfSunken:
       begin
          MoveToEx(DC, Left, Bottom, nil);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Left, Top);
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Left, Top);
          InflateRect(Rect, -1, -1);
          MoveToEx(DC, Left, Bottom, nil);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Left, Top);
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DLIGHT)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
       end;
     dfPushed:
        begin
          InflateRect(Rect, -1, -1);
          MoveToEx(DC, Left, Top, nil);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DLIGHT)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Left, Top);
          InflateRect(Rect, 1, 1);
          MoveToEx(DC, Left, Top, nil);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Left, Top);
        end;
    end;
    OverwriteObject(DC, PriorPen);
    with PriorPoint do
      MoveToEx(DC, x, y, nil);
  end;
end;

procedure DrawGradient(DC: HDC; Rect: TRect; StartColor, EndColor: TColor;
  Colors: Byte; Direction: TDirection);
var
  Delta: Integer;
  StartRGB: array[0..2] of Byte;
  DeltaRGB: array[0..2] of Integer;
  ColorBand: TRect;
  Brush: HBrush;
  I: Integer;
begin
  if IsRectEmpty(Rect) then Exit;
  if Colors < 2 then
  begin
    Brush := CreateSolidBrush(ColorToRGB(StartColor));
    FillRect(DC, Rect, Brush);
    DeleteObject(Brush);
    Exit;
  end;
  StartColor := ColorToRGB(StartColor);
  EndColor := ColorToRGB(EndColor);
  case Direction of
    drDown, drRight:
      begin
        StartRGB[0] := GetRValue(StartColor);
        StartRGB[1] := GetGValue(StartColor);
        StartRGB[2] := GetBValue(StartColor);
        DeltaRGB[0] := GetRValue(EndColor) - StartRGB[0];
        DeltaRGB[1] := GetGValue(EndColor) - StartRGB[1];
        DeltaRGB[2] := GetBValue(EndColor) - StartRGB[2];
      end;
    drUp, drLeft:
      begin
        StartRGB[0] := GetRValue(EndColor);
        StartRGB[1] := GetGValue(EndColor);
        StartRGB[2] := GetBValue(EndColor);
        DeltaRGB[0] := GetRValue(StartColor) - StartRGB[0];
        DeltaRGB[1] := GetGValue(StartColor) - StartRGB[1];
        DeltaRGB[2] := GetBValue(StartColor) - StartRGB[2];
      end;
  end;
  ColorBand := Rect;
  if Direction in [drDown, drUp] then
  begin
    Colors := Max(2, Min(Colors, HeightOf(Rect)));
    Delta := HeightOf(Rect) div Colors;
  end
  else
  begin
    Colors := Max(2, Min(Colors, WidthOf(Rect)));
    Delta := WidthOf(Rect) div Colors;
  end;
  if Delta > 0 then
    for I := 0 to Colors do
    begin
      case Direction of
        drDown, drUp:
          begin
            ColorBand.Top := Rect.Top + I * Delta;
            ColorBand.Bottom := ColorBand.Top + Delta;
          end;
        drRight, drLeft:
          begin
            ColorBand.Left := Rect.Left + I * Delta;
            ColorBand.Right := ColorBand.Left + Delta;
          end;
      end;
      Brush := CreateSolidBrush(RGB(
      StartRGB[0] + MulDiv(I, DeltaRGB[0], Colors - 1),
      StartRGB[1] + MulDiv(I, DeltaRGB[1], Colors - 1),
      StartRGB[2] + MulDiv(I, DeltaRGB[2], Colors - 1)));
      FillRect(DC, ColorBand, Brush);
      DeleteObject(Brush);
    end;
  if Direction in [drDown, drUp] then
    Delta := HeightOf(Rect) mod Colors
  else
    Delta := WidthOf(Rect) mod Colors;
  if Delta > 0 then
  begin
    case Direction of
      drDown, drUp:
        begin
          ColorBand.Top := Rect.Bottom - Delta;
          ColorBand.Bottom := ColorBand.Top + Delta;
        end;
      drRight, drLeft:
        begin
          ColorBand.Left := Rect.Right - Delta;
          ColorBand.Right := ColorBand.Left + Delta;
        end;
    end;
    case Direction of
      drDown, drRight:
          Brush := CreateSolidBrush(EndColor);
        else
          Brush := CreateSolidBrush(StartColor);
    end;
    FillRect(DC, ColorBand, Brush);
    DeleteObject(Brush);
  end;
end;

type
  TDrawGripData = record
    DC: HDC;
    Rect: TRect;
    Clipped: Boolean;
  end;
  PDrawGripData = ^TDrawGripData;

procedure DrawGripProc(DC: HDC; Width, Height: Integer; Data: PDrawGripData);
const
  RasterOps: array[Boolean] of DWORD = (SRCCOPY, SRCAND);
var
  PriorBrush: HBRUSH;
  PriorPen: HPEN;
  P: TPolygon;
  I: Integer;
begin
  PriorBrush := SelectObject(DC, GetStockObject(WHITE_BRUSH));
  PriorPen := SelectObject(DC, GetStockObject(WHITE_PEN));
  if Data.Clipped then
  begin
    BitBlt(DC, 0, 0, Width, Height, 0, 0, 0, BLACKNESS);
    SetLength(P, 3);
    P[0].X:= 0;
    P[0].X:= Height;
    P[1].X:= Width;
    P[1].Y := 0;
    P[2].X:= Width;
    P[2].Y := Height;
    DrawPolygon(DC, P);
    with Data.Rect do
      BitBlt(Data.DC, Right - Width, Bottom - Height, Width, Height, DC, 0, 0,
        SRCPAINT);
    BitBlt(DC, 0, 0, Width, Height, 0, 0, 0, WHITENESS);
  end
  else
    FillRect(DC, GetRect(0, 0, Width, Height), COLOR_BTNFACE + 1);
  SelectObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNHIGHLIGHT)));
  for I := 0 to (Width div 4) - 1 do
  begin
    OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNHIGHLIGHT)));
    MoveToEx(DC, Width - (I * 4) - 5, Height, nil);
    LineTo(DC, Width, Height - (I * 4) - 5);
    OverwriteObject(DC, CreatePen(PS_SOLID, 2, GetSysColor(COLOR_BTNSHADOW)));
    MoveToEx(DC, Width - (I * 4) - 3, Height, nil);
    LineTo(DC, Width, Height - (I * 4) - 3);
    OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNFACE)));
    MoveToEx(DC, Width - (I * 4) - 2, Height, nil);
    LineTo(DC, Width, Height - (I * 4) - 2);
  end;
  OverwriteObject(DC, PriorPen);
  SelectObject(DC, PriorBrush);
  with Data.Rect do
    BitBlt(Data.DC, Right - Width, Bottom - Height, Width, Height, DC, 0, 0,
      RasterOps[Data.Clipped]);
end;

procedure DrawGrip(DC: HDC; Rect: TRect; Clipped: Boolean = True);
var
  Data: TDrawGripData;
begin
  Data.DC := DC;
  Data.Rect := Rect;
  Data.Clipped := Clipped;
  PaintBitmap(GetSystemMetrics(SM_CXHSCROLL), GetSystemMetrics(SM_CXVSCROLL),
    @Data, @DrawGripProc);
end;

procedure DrawNode(DC: HDC; Rect: TRect; Expanded: Boolean; SolidColor: TColor = cl3DDkShadow);
var
  PriorPen: HPEN;
  PriorPoint: TPoint;
begin
  with Rect do
  begin
    Left := Left + (Right - Left - 9) div 2;
    Rect := GetRect(Left, Top + (Bottom - Top - 9) div 2, Left + 8, 0);
  end;
  Rect.Bottom := Rect.Top + 8;
  PriorPen := SelectObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(SolidColor)));
  with Rect do
  begin
    MoveToEx(DC, Left, Top, @PriorPoint);
    LineTo(DC, Right, Top);
    LineTo(DC, Right, Bottom);
    LineTo(DC, Left, Bottom);
    LineTo(DC, Left, Top);
    if SolidColor <> cl3DDkShadow then
      OverwriteObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(SolidColor)))
    else
      OverwriteObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(clWindowText)));
    MoveToEx(DC, Left + 2, Top + 4, nil);
    LineTo(DC, Right - 1, Top + 4);
    if not Expanded then
    begin
      MoveToEx(DC, Left + 4, Top + 2, nil);
      LineTo(DC, Left + 4, Bottom - 1);
    end;
  end;
  with PriorPoint do
    MoveToEx(DC, x, y, nil);
  OverwriteObject(DC, PriorPen);
end;

procedure DrawToolGrip(DC: HDC; const Rect: TRect);
const
  GripSize = 2;
var
  R: TRect;
begin
  R := Rect;
  if HeightOf(Rect) < WidthOf(Rect) then
  begin
    R.Left := Rect.Left + GripSize;
    R.Right := R.Left + GripSize;
    R.Top := R.Top + (HeightOf(Rect) - (GripSize + 1)) div 2;
    R.Bottom := R.Top + GripSize;
    while R.Right + GripSize * 2 < Rect.Right + 1 do
    begin
      OffsetRect(R, 1, 1);
      FillRect(DC, R, COLOR_BTNHIGHLIGHT + 1);
      OffsetRect(R, -1, -1);
      FillRect(DC, R, COLOR_BTNSHADOW + 1);
      OffsetRect(R, GripSize * 2, 0);
    end;
  end
  else
  begin
    R.Left := R.Left + (WidthOf(Rect) - (GripSize + 1)) div 2;
    R.Right := R.Left + GripSize;
    R.Top := Rect.Top + GripSize;
    R.Bottom := R.Top + GripSize;
    while R.Bottom + GripSize * 2 < Rect.Bottom + 1 do
    begin
      OffsetRect(R, 1, 1);
      FillRect(DC, R, COLOR_BTNHIGHLIGHT + 1);
      OffsetRect(R, -1, -1);
      FillRect(DC, R, COLOR_BTNSHADOW + 1);
      OffsetRect(R, 0, GripSize * 2);
    end;
  end;
end;

procedure DrawPolygon(DC: HDC; const P: TPolygon);
begin
  Polygon(DC, Pointer(P)^, Length(P));
end;

procedure DrawPolygon(DC: HDC; const P: TFloatPolygon);
var
  PriorDC: HDC;
begin
  PriorDC := InitializeDevice(DC);
  DrawPolygon(DC, DeviceToPolygon(P));
  FinalizeDevice(PriorDC);
end;

procedure DrawAngledText(DC: HDC; const S: string; const Angle: Double;
  const Point: TPoint);
var
  Font: HFONT;
  LogFont: TLogFont;
  PriorMode: Integer;
begin
  Font := GetCurrentObject(DC, OBJ_FONT);
  GetObject(Font, SizeOf(TLogFont), @LogFont);
  LogFont.lfEscapement := -Round(Angle * 10);
  SelectObject(DC, CreateFontIndirect(LogFont));
  PriorMode := SetBkMode(DC, TRANSPARENT);
  TextOut(DC, Point.X, Point.Y, PChar(S), Length(S));
  SetBkMode(DC, PriorMode);
  OverwriteObject(DC, Font);
end;

procedure DrawRect(DC: HDC; const Rect: TRect; Color: TColor);
var
  Brush: HBRUSH;
begin
  Brush := CreateSolidBrush(ColorToRGB(Color));
  FillRect(DC, Rect, Brush);
  DeleteObject(Brush);
end;

procedure DrawRectOutline(DC: HDC; const Rect: TRect; Color: TColor);
var
  Pen: HBRUSH;
  Point: TPoint;
begin
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 1, ColorToRGB(Color)));
  with Rect do
  begin
    MoveToEx(DC, Left, Top, @Point);
    LineTo(DC, Right - 1, Top);
    LineTo(DC, Right - 1, Bottom - 1);
    LineTo(DC, Left, Bottom - 1);
    LineTo(DC, Left, Top);
    MoveToEx(DC, Point.x, Point.y, nil);
  end;
  DeleteObject(SelectObject(DC, Pen));
end;

procedure DrawSlantRect(DC: HDC; const Rect: TRect; const A, B: TPoint);
var
  Rectangle: TRectangle;
begin
  Rectangle := Rotate(Rect, A, B);
  Polygon(DC, Rectangle, 4);
end;

procedure DrawSlantText(DC: HDC; const S: string; const A, B: TPoint);
var
  SlantAngle: Double;
  Font: HFONT;
  LogFont: TLogFont;
  Rectangle: TRectangle;
  PriorMode: Integer;
begin
  SlantAngle := Angle(A, B);
  Font := GetCurrentObject(DC, OBJ_FONT);
  GetObject(Font, SizeOf(TLogFont), @LogFont);
  LogFont.lfEscapement := -Round(SlantAngle * 10);
  SelectObject(DC, CreateFontIndirect(LogFont));
  with CalculateCaptionSize(DC, S) do
    Rectangle := Rotate(GetRect(0, 0, cx, cy), A, B);
  PriorMode := SetBkMode(DC, TRANSPARENT);
  TextOut(DC, Rectangle[2].x, Rectangle[2].Y, PChar(S), Length(S));
  SetBkMode(DC, PriorMode);
  OverwriteObject(DC, Font);
end;

procedure DrawBevelRect(DC: HDC; Rect: TRect; Bevel: Integer; Color: TColor);
var
  Pen: HPEN;
  Point: TPoint;
begin
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(Color)));
  with Rect do
  begin
    MoveToEx(DC, Left, Top + Bevel, @Point);
    LineTo(DC, Left + Bevel, Top);
    LineTo(DC, Right - Bevel - 1, Top);
    LineTo(DC, Right - 1, Top + Bevel);
    LineTo(DC, Right - 1, Bottom - Bevel - 1);
    LineTo(DC, Right - Bevel - 1, Bottom - 1);
    LineTo(DC, Left + Bevel, Bottom - 1);
    LineTo(DC, Left, Bottom - Bevel - 1);
    LineTo(DC, Left, Top + Bevel);
    MoveToEx(DC, Point.x, Point.y, nil);
  end;
  OverwriteObject(DC, Pen);
end;

{ DrawBubble }

procedure DrawBubble(DC: HDC; const Caption: string; Rect: TRect;
  Direction: TDirection; Bevel: Integer; ForeColor, BackColor: TColor);
var
  BubbleArea: array[0..7] of TPoint;
  Pen: HPEN;
  Brush: HBRUSH;
  Rgn: HRGN;
begin
  with Rect do
  begin
    BubbleArea[0].X := Left;
    BubbleArea[0].Y := Top + Bevel;
    BubbleArea[1].X := Left + Bevel;
    BubbleArea[1].Y := Top;
    BubbleArea[2].X := Right - Bevel - 1;
    BubbleArea[2].Y := Top;
    BubbleArea[3].X := Right - 1;
    BubbleArea[3].Y := Top + Bevel;
    BubbleArea[4].X := Right - 1;
    BubbleArea[4].Y := Bottom - Bevel - 1;
    BubbleArea[5].X := Right - Bevel - 1;
    BubbleArea[5].Y := Bottom - 1;
    BubbleArea[6].X := Left + Bevel;
    BubbleArea[6].Y := Bottom - 1;
    BubbleArea[7].X := Left;
    BubbleArea[7].Y := Bottom - Bevel - 1;
  end;
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(ForeColor)));
  Brush := SelectObject(DC, CreateSolidBrush(ColorToRGB(BackColor)));
  Polygon(DC, BubbleArea, 8);
  OverwriteObject(DC, Brush);
  OverwriteObject(DC, Pen);
  InflateRect(Rect, -2, 0);
  DrawCaption(DC, Caption, Rect, Direction);
  Rgn := CreatePolygonRgn(BubbleArea, 8, WINDING);
  OffsetRgn(Rgn, 1, 1);
  ExtSelectClipRgn(DC, Rgn, RGN_DIFF);
  DeleteObject(Rgn);
end;

{ DrawCapsuleAdvanced }

procedure DrawCapsuleAdvanced(DC: HDC; const Caption: string; Rect: TRect;
  Direction: TDirection; Bevel: Integer; Width: Integer;
  CapsuleProc: TCapsuleProc; ForeColor, BackColor: TColor; Data: Pointer);
var
  CapsuleArea: array[0..7] of TPoint;
  CaptionArea, BodyArea: array[0..5] of TPoint;

  procedure BuildAreas;
  begin
    with Rect do
    begin
      CapsuleArea[0].X := Left;
      CapsuleArea[0].Y := Top + Bevel;
      CapsuleArea[1].X := Left + Bevel;
      CapsuleArea[1].Y := Top;
      CapsuleArea[2].X := Right - Bevel - 1;
      CapsuleArea[2].Y := Top;
      CapsuleArea[3].X := Right - 1;
      CapsuleArea[3].Y := Top + Bevel;
      CapsuleArea[4].X := Right - 1;
      CapsuleArea[4].Y := Bottom - Bevel - 1;
      CapsuleArea[5].X := Right - Bevel - 1;
      CapsuleArea[5].Y := Bottom - 1;
      CapsuleArea[6].X := Left + Bevel;
      CapsuleArea[6].Y := Bottom - 1;
      CapsuleArea[7].X := Left;
      CapsuleArea[7].Y := Bottom - Bevel - 1;
      CaptionArea[0] := CapsuleArea[0];
      CaptionArea[1] := CapsuleArea[1];
      CaptionArea[0] := CapsuleArea[0];
      CaptionArea[2].X := CaptionArea[0].X + Width;
      CaptionArea[2].Y := CapsuleArea[1].Y;
      CaptionArea[3].X := CapsuleArea[7].X + Width;
      CaptionArea[3].Y := CapsuleArea[6].Y;
      CaptionArea[4] := CapsuleArea[6];
      CaptionArea[5] := CapsuleArea[7];
      BodyArea[0] := CaptionArea[2];
      BodyArea[1] := CapsuleArea[2];
      BodyArea[2] := CapsuleArea[3];
      BodyArea[3] := CapsuleArea[4];
      BodyArea[4] := CapsuleArea[5];
      BodyArea[5] := CaptionArea[3];
    end;
  end;

var
  Pen: HPEN;
  Brush: HBRUSH;
  DrawRect: TRect;
  Rgn: HRGN;
begin
  BuildAreas;
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(ForeColor)));
  Brush := SelectObject(DC, CreateSolidBrush(ColorToRGB(BackColor)));
  Polygon(DC, CaptionArea, 6);
  Polygon(DC, BodyArea, 6);
  OverwriteObject(DC, Brush);
  OverwriteObject(DC, Pen);
  DrawRect := Rect;
  DrawRect.Right := DrawRect.Left + Width;
  InflateRect(DrawRect, -2, 0);
  DrawCaption(DC, Caption, DrawRect, Direction);
  DrawRect := Rect;
  DrawRect.Left := DrawRect.Left + Width;
  if @CapsuleProc <> nil then
    CapsuleProc(DC, DrawRect, ForeColor, BackColor, Data);
  Rgn := CreatePolygonRgn(CapsuleArea, 8, WINDING);
  OffsetRgn(Rgn, 1, 1);
  ExtSelectClipRgn(DC, Rgn, RGN_DIFF);
  DeleteObject(Rgn);
end;

{ DrawCapsuleText }

type
  TDrawCapsuleTextParams = record
    Text: string;
    Direction: TDirection;
  end;
  PDrawCapsuleTextParams = ^TDrawCapsuleTextParams;

procedure DrawCapsuleTextProc(DC: HDC; Rect: TRect; ForeColor, BackColor: TColor;
  Data: Pointer);
var
  Params: PDrawCapsuleTextParams absolute Data;
begin
  InflateRect(Rect, -4, 0);
  if FieldCount(Params.Text, '|') = 2 then
  begin
    DrawCaption(DC, FieldValue(Params.Text, '|', 1), Rect, drRight);
    DrawCaption(DC, FieldValue(Params.Text, '|', 0), Rect, drLeft);
  end
  else
    DrawCaption(DC, Params.Text, Rect, Params.Direction);
end;

procedure DrawCapsuleText(DC: HDC; const Caption, Body: string; Rect: TRect;
  CaptionDirection, BodyDirection: TDirection; Bevel: Integer; Width: Integer;
  ForeColor, BackColor: TColor);
var
  Params: TDrawCapsuleTextParams;
begin
  Params.Text := Body;
  Params.Direction := BodyDirection;
  DrawCapsuleAdvanced(DC, Caption, Rect, CaptionDirection, Bevel, Width,
    @DrawCapsuleTextProc, ForeColor, BackColor, @Params);
end;

procedure DrawCapsuleCheckBox(DC: HDC; const Text: string; Rect: TRect;
  Direction: TDirection; Bevel: Integer; Width: Integer; Checked: Boolean;
  ForeColor, BackColor: TColor; Flat: Boolean = True);
var
  CapsuleArea: array[0..7] of TPoint;
  CaptionArea: array[0..5] of TPoint;

  procedure BuildAreas;
  begin
    with Rect do
    begin
      CapsuleArea[0].X := Left;
      CapsuleArea[0].Y := Top + Bevel;
      CapsuleArea[1].X := Left + Bevel;
      CapsuleArea[1].Y := Top;
      CapsuleArea[2].X := Right - Bevel - 1;
      CapsuleArea[2].Y := Top;
      CapsuleArea[3].X := Right - 1;
      CapsuleArea[3].Y := Top + Bevel;
      CapsuleArea[4].X := Right - 1;
      CapsuleArea[4].Y := Bottom - Bevel - 1;
      CapsuleArea[5].X := Right - Bevel - 1;
      CapsuleArea[5].Y := Bottom - 1;
      CapsuleArea[6].X := Left + Bevel;
      CapsuleArea[6].Y := Bottom - 1;
      CapsuleArea[7].X := Left;
      CapsuleArea[7].Y := Bottom - Bevel - 1;
      CaptionArea[0] := CapsuleArea[0];
      CaptionArea[1] := CapsuleArea[1];
      CaptionArea[0] := CapsuleArea[0];
      CaptionArea[2].X := CaptionArea[0].X + Width;
      CaptionArea[2].Y := CapsuleArea[1].Y;
      CaptionArea[3].X := CapsuleArea[7].X + Width;
      CaptionArea[3].Y := CapsuleArea[6].Y;
      CaptionArea[4] := CapsuleArea[6];
      CaptionArea[5] := CapsuleArea[7];
    end;
  end;

var
  Pen: HPEN;
  Brush: HBRUSH;
  DrawRect: TRect;
  Rgn: HRGN;
begin
  BuildAreas;
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(ForeColor)));
  Brush := SelectObject(DC, CreateSolidBrush(ColorToRGB(BackColor)));
  Polygon(DC, CapsuleArea, 8);
  Polygon(DC, CaptionArea, 6);
  OverwriteObject(DC, Brush);
  OverwriteObject(DC, Pen);
  DrawRect := Rect;
  DrawRect.Right := DrawRect.Left + Width;
  InflateRect(DrawRect, -2, 0);
  DrawCaption(DC, Text, DrawRect, Direction);
  DrawRect := Rect;
  DrawRect.Left := DrawRect.Left + Width;
  DrawCheckBox(DC, DrawRect, Checked, ForeColor, BackColor, Flat);
  Rgn := CreatePolygonRgn(CapsuleArea, 8, WINDING);
  OffsetRgn(Rgn, 1, 1);
  ExtSelectClipRgn(DC, Rgn, RGN_DIFF);
  DeleteObject(Rgn);
end;

procedure BlitInvert(DC: HDC; const Rect: TRect);
begin
	with Rect do
		BitBlt(DC, Left, Top, Right - Left, Bottom - Top, 0, 0, 0, DSTINVERT);
end;

procedure BlitAnd(Dest: HDC; const Rect: TRect; Source: HDC);
begin
	with Rect do
		BitBlt(Dest, Left, Top, Right - Left, Bottom - Top, Source, 0, 0, SRCAND);
end;

procedure BlitOr(Dest: HDC; const Rect: TRect; Source: HDC);
begin
	with Rect do
		BitBlt(Dest, Left, Top, Right - Left, Bottom - Top, Source, 0, 0, SRCPAINT);
end;

var
	InternalGlyphs: array[TGlyphKind] of TBitmap;

const
	GlyphBase = 1001;
  GlyphDim = 12;
  GlyphHalf = GlyphDim div 2;
  GlyphBits = GlyphDim * GlyphDim;

function GlyphFind(Kind: TGlyphKind): TBitmap;
begin
  if InternalGlyphs[Kind] = nil then
	  InternalGlyphs[Kind] := GetBitmap(GlyphBase + Ord(Kind));
  Result := InternalGlyphs[Kind];
end;

procedure GlyphBlendBlt(DC: HDC; Glyph: TGlyphKind; X, Y: Integer; Color: TColor);
var
  RGB: TRGBQuad absolute Color;
  S: TRGBTriple absolute Color;
  T: TRGBTriple;
  A, B: TFastBitmap;
  C, D: PRGBTriple;
  I: Integer;
begin
  Color := ColorToRGB(Color);
  I := S.rgbtBlue;
  S.rgbtBlue := S.rgbtRed;
  S.rgbtRed := I;
  T.rgbtBlue := S.rgbtBlue div 3;
  T.rgbtGreen := S.rgbtGreen div 3;
  T.rgbtRed := S.rgbtRed div 3;
  A := CreateFastBitmap(GlyphDim, GlyphDim);
  B := CreateFastBitmap(GlyphDim, GlyphDim);
  try
    BitBlt(A.DC, 0, 0, GlyphDim, GlyphDim, DC, X, Y, SRCCOPY);
    BitBlt(B.DC, 0, 0, GlyphDim, GlyphDim, GlyphFind(Glyph).Canvas.Handle, 0, 0, SRCCOPY);
    C := A.Bits;
    D := B.Bits;
    for I := 0 to GlyphBits - 1 do
    begin
      case D.rgbtBlue of
        0: C^ := S;
        128:
          begin
            C.rgbtBlue := (C.rgbtBlue + S.rgbtBlue) shr 1;
            C.rgbtGreen := (C.rgbtGreen + S.rgbtGreen) shr 1;
            C.rgbtRed := (C.rgbtRed + S.rgbtRed) shr 1;
          end;
        192:
          begin
            C.rgbtBlue := (C.rgbtBlue div 3) shl 1 + T.rgbtBlue;
            C.rgbtGreen := (C.rgbtGreen div 3) shl 1 + T.rgbtGreen;
            C.rgbtRed := (C.rgbtRed div 3) shl 1  + T.rgbtRed;
          end;
      end;
      Inc(C);
      Inc(D);
    end;
    BitBlt(DC, X, Y, GlyphDim, GlyphDim, A.DC, 0, 0, SRCCOPY);
  finally
    DestroyFastBitmap(A);
    DestroyFastBitmap(B);
  end;
end;

procedure GlyphDraw(DC: HDC; const Rect: TRect; Glyph: TGlyphKind; Color: TColor);
var
	H, W: Integer;
begin
  H := Rect.Bottom - Rect.Top;
  W := Rect.Right - Rect.Left;
  with Rect do
		GlyphBlendBlt(DC, Glyph, Left + (W - GlyphDim) div 2,
      Top + (H - GlyphDim) div 2, Color);
end;

procedure GlyphFrame(DC: HDC; Rect: TRect; Glyph: TGlyphKind; State: TDrawState;
  Color: TColor);
var
	X, Y, H, W: Integer;
  Pen: HPEN;
  Brush: HBRUSH;
begin
	X := GlyphDim;
  Y := GlyphDim;
  H := Rect.Bottom - Rect.Top;
  W := Rect.Right - Rect.Left;
  with Rect do
  begin
		Left := Left + (W - X) div 2;
    Top := Top + (H - Y) div 2;
    Right := Left + X + 1;
    Bottom := Top + Y;
 	end;
  if dsPressed in State then
  	OffsetRect(Rect, 1, 1);
	GlyphBlendBlt(DC, Glyph, Rect.Left, Rect.Top, Color);
  if dsFlat in State then Exit;
	InflateRect(Rect, 2, 2);
  if dsPressed in State then
  begin
  	OffsetRect(Rect, -1, -1);
    Inc(Rect.Right);
    Inc(Rect.Bottom);
    if dsThin in State then
	  	DrawFrame(DC, Rect, dfLowered)
    else
      DrawFrame(DC, Rect, dfSunken);

  end
  else if not (dsThin in State) then
  begin
    Pen := SelectObject(DC, CreatePen(PS_SOLID, 2, ColorToRGB(Color)));
    Brush := SelectObject(DC, GetStockObject(HOLLOW_BRUSH));
    with Rect do
    	Rectangle(DC, Left, Top, Right, Bottom);
		OverwriteObject(DC, Pen);
    SelectObject(DC, Brush);
  end;
end;

const
  ThemeDataNames: array[TThemedElement] of PWideChar = (
    'button',      // teButton
    'clock',       // teClock
    'combobox',    // teComboBox
    'edit',        // teEdit
    'explorerbar', // teExplorerBar
    'header',      // teHeader
    'listview',    // teListView
    'menu',        // teMenu
    'page',        // tePage
    'progress',    // teProgress
    'rebar',       // teRebar
    'scrollbar',   // teScrollBar
    'spin',        // teSpin
    'startpanel',  // teStartPanel
    'status',      // teStatus
    'tab',         // teTab
    'taskband',    // teTaskBand
    'taskbar',     // teTaskBar
    'toolbar',     // teToolBar
    'tooltip',     // teToolTip
    'trackbar',    // teTrackBar
    'traynotify',  // teTrayNotify
    'treeview',    // teTreeview
    'window'       // teWindow
  );

var
  InternalThemePainter: TThemePainter;

function ThemePainter: TThemePainter;
begin
  if InternalThemePainter = nil then
    InternalThemePainter := TThemePainter.Create;
  Result := InternalThemePainter;
end;

constructor TThemePainter.Create;
begin
	FWindow := TUtilityWindow.Create(Self);
  FAvailable := ThemesLoaded;
  UpdateThemes;
end;

destructor TThemePainter.Destroy;
begin
  UnloadThemeData;
	FWindow.Free;
  inherited;
end;

function TThemePainter.GetTheme(Element: TThemedElement): HTHEME;
begin
  if FAvailable and (FThemeData[Element] = 0) then
    FThemeData[Element] := OpenThemeData(FWindow.Handle, ThemeDataNames[Element]);
  Result := FThemeData[Element];
end;

function TThemePainter.GetEnabled: Boolean;
begin
  Result := FAvailable and FControlsEnabled;
end;

function EnumThemesChildProc(Wnd: HWND; lParam: Integer): BOOL; stdcall;
begin
  SendMessage(Wnd, WM_THEMECHANGED, 0, 0);
  if IsWindowVisible(Wnd) then
  begin
    ShowWindow(Wnd, SW_HIDE);
    ShowWindow(Wnd, SW_SHOW);
  end;
  Result := True;
end;

function EnumThemesProc(Wnd: HWND; lParam: Integer): BOOL; stdcall;
begin
  if IsProcessWindow(Wnd) then
  begin
    EnumChildWindows(Wnd, @EnumThemesChildProc, 0);
    InvalidateRect(Wnd, nil, True);
  end;
  Result := True;
end;

procedure TThemePainter.SetEnabled(Value: Boolean);
const
  Flags: array[Boolean] of DWORD = (
    STAP_ALLOW_NONCLIENT, STAP_ALLOW_NONCLIENT or STAP_ALLOW_CONTROLS or STAP_ALLOW_WEBCONTENT);
begin
  if not FAvailable then Exit;
  if Value = Enabled then Exit;
  SetThemeAppProperties(Flags[Value]);
  UpdateThemes;
  EnumWindows(@EnumThemesProc, 0);
end;

procedure TThemePainter.WMThemeChanged(var Message: TMessage);
begin
  UpdateThemes;
	DoOnThemeChange;
end;

procedure TThemePainter.DoOnThemeChange;
begin
  if Assigned(FOnThemeChange) then
    FOnThemeChange(Self);
end;

procedure TThemePainter.UnloadThemeData;
var
  Entry: TThemedElement;
begin
  if (FWindow.Handle = 0) or IsWindow(FWindow.Handle) then
  begin
    for Entry := Low(TThemeData) to High(TThemeData) do
      if FThemeData[Entry] <> 0 then
      begin
        CloseThemeData(FThemeData[Entry]);
        FThemeData[Entry] := 0;
      end;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedButton): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teButton;
  with Result do
  begin
    case Widget of
      tbPushButtonNormal..tbPushButtonDefaulted:
        begin
          Part := BP_PUSHBUTTON;
          Base := Ord(tbPushButtonNormal);
        end;
      tbRadioButtonUncheckedNormal..tbRadioButtonCheckedDisabled:
        begin
          Part := BP_RADIOBUTTON;
          Base := Ord(tbRadioButtonUncheckedNormal);
        end;
      tbCheckBoxUncheckedNormal..tbCheckBoxMixedDisabled:
        begin
          Part := BP_CHECKBOX;
          Base := Ord(tbCheckBoxUncheckedNormal);
        end;
      tbGroupBoxNormal..tbGroupBoxDisabled:
        begin
          Part := BP_GROUPBOX;
          Base := Ord(tbGroupBoxNormal);
        end;
      tbUserButton:
        begin
          Part := BP_USERBUTTON;
          Base := Ord(tbUserButton);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedClock): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teClock;
  with Result do
  begin
    case Widget of
      tcTimeNormal:
        begin
          Part := CLP_TIME;
          Base := Ord(tcTimeNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedComboBox): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teComboBox;
  with Result do
  begin
    case Widget of
      tcDropDownButtonNormal..tcDropDownButtonDisabled:
        begin
          Part := CP_DROPDOWNBUTTON;
          Base := Ord(tcDropDownButtonNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedEdit): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teEdit;
  with Result do
  begin
    case Widget of
      teEditTextNormal..teEditTextAssist:
        begin
          Part := EP_EDITTEXT;
          Base := Ord(teEditTextNormal);
        end;
      teEditCaret:
        begin
          Part := EP_CARET;
          Base := Ord(teEditCaret);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedExplorerBar): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teExplorerBar;
  with Result do
  begin
    case Widget of
      tebHeaderBackgroundNormal..tebHeaderBackgroundPressed:
        begin
          Part := EBP_HEADERBACKGROUND;
          Base := Ord(tebHeaderBackgroundNormal);
        end;
      tebHeaderCloseNormal..tebHeaderClosePressed:
        begin
          Part := EBP_HEADERCLOSE;
          Base := Ord(tebHeaderCloseNormal);
        end;
      tebHeaderPinNormal..tebHeaderPinSelectedPressed:
        begin
          Part := EBP_HEADERPIN;
          Base := Ord(tebHeaderPinSelectedNormal);
        end;
      tebIEBarMenuNormal..tebIEBarMenuPressed:
        begin
          Part := EBP_IEBARMENU;
          Base := Ord(tebIEBarMenuNormal);
        end;
      tebNormalGroupBackground:
        begin
          Part := EBP_NORMALGROUPBACKGROUND;
          Base := Ord(tebNormalGroupBackground);
        end;
      tebNormalGroupCollapseNormal..tebNormalGroupCollapsePressed:
        begin
          Part := EBP_NORMALGROUPCOLLAPSE;
          Base := Ord(tebNormalGroupCollapseNormal);
        end;
      tebNormalGroupExpandNormal..tebNormalGroupExpandPressed:
        begin
          Part := EBP_NORMALGROUPEXPAND;
          Base := Ord(tebNormalGroupExpandNormal);
        end;
      tebNormalGroupHead:
        begin
          Part := EBP_NORMALGROUPHEAD;
          Base := Ord(tebNormalGroupHead);
        end;
      tebSpecialGroupBackground:
        begin
          Part := EBP_SPECIALGROUPBACKGROUND;
          Base := Ord(tebSpecialGroupBackground);
        end;
      tebSpecialGroupCollapseSpecial..tebSpecialGroupCollapsePressed:
        begin
          Part := EBP_SPECIALGROUPCOLLAPSE;
          Base := Ord(tebSpecialGroupCollapseSpecial);
        end;
      tebSpecialGroupExpandSpecial..tebSpecialGroupExpandPressed:
        begin
          Part := EBP_SPECIALGROUPEXPAND;
          Base := Ord(tebSpecialGroupExpandSpecial);
        end;
      tebSpecialGroupHead:
        begin
          Part := EBP_SPECIALGROUPHEAD;
          Base := Ord(tebSpecialGroupHead);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedHeader): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teHeader;
  with Result do
  begin
    case Widget of
      thHeaderItemNormal..thHeaderItemPressed:
        begin
          Part := HP_HEADERITEM;
          Base := Ord(thHeaderItemNormal);
        end;
      thHeaderItemLeftNormal..thHeaderItemLeftPressed:
        begin
          Part := HP_HEADERITEMLEFT;
          Base := Ord(thHeaderItemLeftNormal);
        end;
      thHeaderItemRightNormal..thHeaderItemRightPressed:
        begin
          Part := HP_HEADERITEMRIGHT;
          Base := Ord(thHeaderItemRightNormal);
        end;
      thHeaderSortArrowSortedUp..thHeaderSortArrowSortedDown:
        begin
          Part := HP_HEADERSORTARROW;
          Base := Ord(thHeaderSortArrowSortedUp);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedListview): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teListView;
  with Result do
  begin
    case Widget of
      tlListItemNormal..tlListItemSelectedNotFocus:
        begin
          Part := LVP_LISTITEM;
          Base := Ord(tlListItemNormal);
        end;
      tlListGroup:
        begin
          Part := LVP_LISTGROUP;
          Base := Ord(tlListGroup);
        end;
      tlListDetail:
        begin
          Part := LVP_LISTDETAIL;
          Base := Ord(tlListDetail);
        end;
      tlListSortDetail:
        begin
          Part := LVP_LISTSORTEDDETAIL;
          Base := Ord(tlListSortDetail);
        end;
      tlEmptyText:
        begin
          Part := LVP_EMPTYTEXT;
          Base := Ord(tlEmptyText);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedMenu): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teMenu;
  with Result do
  begin
    case Widget of
      tmMenuItemNormal..tmMenuItemDemoted:
        begin
          Part := MP_MENUITEM;
          Base := Ord(tmMenuItemNormal);
        end;
      tmMenuDropDown:
        begin
          Part := MP_MENUDROPDOWN;
          Base := Ord(tmMenuDropDown);
        end;
      tmMenuBarItem:
        begin
          Part := MP_MENUBARITEM;
          Base := Ord(tmMenuBarItem);
        end;
      tmMenuBarDropDown:
        begin
          Part := MP_MENUBARDROPDOWN;
          Base := Ord(tmMenuBarDropDown);
        end;
      tmChevron:
        begin
          Part := MP_CHEVRON;
          Base := Ord(tmChevron);
        end;
      tmSeparator:
        begin
          Part := MP_SEPARATOR;
          Base := Ord(tmSeparator);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedPage): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := tePage;
  with Result do
  begin
    case Widget of
      tpUpNormal..tpUpDisabled:
        begin
          Part := PGRP_UP;
          Base := Ord(tpUpNormal);
        end;
      tpDownNormal..tpDownDisabled:
        begin
          Part := PGRP_DOWN;
          Base := Ord(tpDownNormal);
        end;
      tpUpHorzNormal..tpUpHorzDisabled:
        begin
          Part := PGRP_UPHORZ;
          Base := Ord(tpUpHorzNormal);
        end;
      tpDownHorzNormal..tpDownHorzDisabled:
        begin
          Part := PGRP_DOWNHORZ;
          Base := Ord(tpDownHorzNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedProgress): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teProgress;
  with Result do
  begin
    case Widget of
      tpBar:
        begin
          Part := PP_BAR;
          Base := Ord(tpBar);
        end;
      tpBarVert:
        begin
          Part := PP_BARVERT;
          Base := Ord(tpBarVert);
        end;
      tpChunk:
        begin
          Part := PP_CHUNK;
          Base := Ord(tpChunk);
        end;
      tpChunkVert:
        begin
          Part := PP_CHUNKVERT;
          Base := Ord(tpChunkVert);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedRebar): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teRebar;
  with Result do
  begin
    case Widget of
      trGripper:
        begin
          Part := RP_GRIPPER;
          Base := Ord(trGripper);
        end;
      trGripperVert:
        begin
          Part := RP_GRIPPERVERT;
          Base := Ord(trGripperVert);
        end;
      trBandNormal..trBandHotChecked:
        begin
          Part := RP_BAND;
          Base := Ord(trBandNormal);
        end;
      trChevronNormal..trChevronDisabled:
        begin
          Part := RP_CHEVRON;
          Base := Ord(trChevronNormal);
        end;
      trChevronVertNormal..trChevronVertDisabled:
        begin
          Part := RP_CHEVRONVERT;
          Base := Ord(trChevronVertNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedScrollBar): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teScrollBar;
  with Result do
  begin
    case Widget of
      tsArrowBtnUpNormal..tsArrowBtnRightDisabled:
        begin
          Part := SBP_ARROWBTN;
          Base := Ord(tsArrowBtnUpNormal);
        end;
      tsThumbBtnHorzNormal..tsThumbBtnHorzDisabled:
        begin
          Part := SBP_THUMBBTNHORZ;
          Base := Ord(tsThumbBtnHorzNormal);
        end;
      tsThumbBtnVertNormal..tsThumbBtnVertDisabled:
        begin
          Part := SBP_THUMBBTNVERT;
          Base := Ord(tsThumbBtnVertNormal);
        end;
      tsLowerTrackHorzNormal..tsLowerTrackHorzDisabled:
        begin
          Part := SBP_LOWERTRACKHORZ;
          Base := Ord(tsLowerTrackHorzNormal);
        end;
      tsUpperTrackHorzNormal..tsUpperTrackHorzDisabled:
        begin
          Part := SBP_UPPERTRACKHORZ;
          Base := Ord(tsUpperTrackHorzNormal);
        end;
      tsLowerTrackVertNormal..tsLowerTrackVertDisabled:
        begin
          Part := SBP_LOWERTRACKVERT;
          Base := Ord(tsLowerTrackVertNormal);
        end;
      tsUpperTrackVertNormal..tsUpperTrackVertDisabled:
        begin
          Part := SBP_UPPERTRACKVERT;
          Base := Ord(tsUpperTrackVertNormal);
        end;
      tsGripperHorzNormal..tsGripperHorzDisabled:
        begin
          Part := SBP_GRIPPERHORZ;
          Base := Ord(tsGripperHorzNormal);
        end;
      tsGripperVertNormal..tsGripperVertDisabled:
        begin
          Part := SBP_GRIPPERVERT;
          Base := Ord(tsGripperVertNormal);
        end;
      tsSizeBoxRightAlign..tsSizeBoxLeftAlign:
        begin
          Part := SBP_SIZEBOX;
          Base := Ord(tsSizeBoxRightAlign);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedSpin): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teSpin;
  with Result do
  begin
    case Widget of
      tsUpNormal..tsUpDisabled:
        begin
          Part := SPNP_UP;
          Base := Ord(tsUpNormal);
        end;
      tsDownNormal..tsDownDisabled:
        begin
          Part := SPNP_DOWN;
          Base := Ord(tsDownNormal);
        end;
      tsUpHorzNormal..tsUpHorzDisabled:
        begin
          Part := SPNP_UPHORZ;
          Base := Ord(tsUpHorzNormal);
        end;
      tsDownHorzNormal..tsDownHorzDisabled:
        begin
          Part := SPNP_DOWNHORZ;
          Base := Ord(tsDownHorzNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedStartPanel): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teStartPanel;
  with Result do
  begin
    case Widget of
      tspUserPane:
        begin
          Part := SPP_USERPANE;
          Base := Ord(tspUserPane);
        end;
      tspMorePrograms:
        begin
          Part := SPP_MOREPROGRAMS;
          Base := Ord(tspMorePrograms);
        end;
      tspMoreProgramsArrowNormal..tspMoreProgramsArrowPressed:
        begin
          Part := SPP_MOREPROGRAMSARROW;
          Base := Ord(tspMoreProgramsArrowNormal);
        end;
      tspProgList:
        begin
          Part := SPP_PROGLIST;
          Base := Ord(tspProgList);
        end;
      tspProgListSeparator:
        begin
          Part := SPP_PROGLISTSEPARATOR;
          Base := Ord(tspProgListSeparator);
        end;
      tspPlacesList:
        begin
          Part := SPP_PLACESLIST;
          Base := Ord(tspPlacesList);
        end;
      tspPlacesListSeparator:
        begin
          Part := SPP_PLACESLISTSEPARATOR;
          Base := Ord(tspPlacesListSeparator);
        end;
      tspLogOff:
        begin
          Part := SPP_LOGOFF;
          Base := Ord(tspLogOff);
        end;
      tspLogOffButtonsNormal..tspLogOffButtonsPressed:
        begin
          Part := SPP_LOGOFFBUTTONS;
          Base := Ord(tspLogOffButtonsNormal);
        end;
      tspUserPicture:
        begin
          Part := SPP_USERPICTURE;
          Base := Ord(tspUserPicture);
        end;
      tspPreview:
        begin
          Part := SPP_PREVIEW;
          Base := Ord(tspPreview);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedStatus): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teStatus;
  with Result do
  begin
    case Widget of
      tsPane:
        begin
          Part := SP_PANE;
          Base := Ord(tsPane);
        end;
      tsGripperPane:
        begin
          Part := SP_GRIPPERPANE;
          Base := Ord(tsGripperPane);
        end;
      tsGripper:
        begin
          Part := SP_GRIPPER;
          Base := Ord(tsGripper);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedTab): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teTab;
  with Result do
  begin
    case Widget of
      ttTabItemNormal..ttTabItemFocused:
        begin
          Part := TABP_TABITEM;
          Base := Ord(ttTabItemNormal);
        end;
      ttTabItemLeftEdgeNormal..ttTabItemLeftEdgeFocused:
        begin
          Part := TABP_TABITEMLEFTEDGE;
          Base := Ord(ttTabItemLeftEdgeNormal);
        end;
      ttTabItemRightEdgeNormal..ttTabItemRightEdgeFocused:
        begin
          Part := TABP_TABITEMRIGHTEDGE;
          Base := Ord(ttTabItemRightEdgeNormal);
        end;
      ttTabItemBothEdgeNormal..ttTabItemBothEdgeFocused:
        begin
          Part := TABP_TABITEMBOTHEDGE;
          Base := Ord(ttTabItemBothEdgeNormal);
        end;
      ttTopTabItemNormal..ttTopTabItemFocused:
        begin
          Part := TABP_TOPTABITEM;
          Base := Ord(ttTopTabItemNormal);
        end;
      ttTopTabItemLeftEdgeNormal..ttTopTabItemLeftEdgeFocused:
        begin
          Part := TABP_TOPTABITEMLEFTEDGE;
          Base := Ord(ttTopTabItemLeftEdgeNormal);
        end;
      ttTopTabItemRightEdgeNormal..ttTopTabItemRightEdgeFocused:
        begin
          Part := TABP_TOPTABITEMRIGHTEDGE;
          Base := Ord(ttTopTabItemRightEdgeNormal);
        end;
      ttTopTabItemBothEdgeNormal..ttTopTabItemBothEdgeFocused:
        begin
          Part := TABP_TOPTABITEMBOTHEDGE;
          Base := Ord(ttTopTabItemBothEdgeNormal);
        end;
      ttPane:
        begin
          Part := TABP_PANE;
          Base := Ord(ttPane);
        end;
      ttBody:
        begin
          Part := TABP_BODY;
          Base := Ord(ttBody);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedTaskBand): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teTaskBand;
  with Result do
  begin
    case Widget of
      ttbGroupCount:
        begin
          Part := TDP_GROUPCOUNT;
          Base := Ord(ttbGroupCount);
        end;
      ttbFlashButton:
        begin
          Part := TDP_FLASHBUTTON;
          Base := Ord(ttbFlashButton);
        end;
      ttpFlashButtonGroupMenu:
        begin
          Part := TDP_FLASHBUTTONGROUPMENU;
          Base := Ord(ttpFlashButtonGroupMenu);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedTaskBar): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teTaskBar;
  with Result do
  begin
    case Widget of
      ttbTimeNormal:
        begin
          Part := CLP_TIME;
          Base := Ord(ttbTimeNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedToolBar): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teToolBar;
  with Result do
  begin
    case Widget of
      ttbButtonNormal..ttbButtonCheckedHot:
        begin
          Part := TP_BUTTON;
          Base := Ord(ttbButtonNormal);
        end;
      ttbDropDownButtonNormal..ttbDropDownButtonCheckedHot:
        begin
          Part := TP_DROPDOWNBUTTON;
          Base := Ord(ttbDropDownButtonNormal);
        end;
      ttbSplitButtonNormal..ttbSplitButtonCheckedHot:
        begin
          Part := TP_SPLITBUTTON;
          Base := Ord(ttbSplitButtonNormal);
        end;
      ttbSplitButtonDropDownNormal..ttbSplitButtonDropDownCheckedHot:
        begin
          Part := TP_SPLITBUTTONDROPDOWN;
          Base := Ord(ttbSplitButtonDropDownNormal);
        end;
      ttbSeparatorNormal..ttbSeparatorCheckedHot:
        begin
          Part := TP_SEPARATOR;
          Base := Ord(ttbSeparatorNormal);
        end;
      ttbSeparatorVertNormal..ttbSeparatorVertCheckedHot:
        begin
          Part := TP_SEPARATORVERT;
          Base := Ord(ttbSeparatorVertNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedToolTip): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teToolTip;
  with Result do
  begin
    case Widget of
      tttStandardNormal..tttStandardLink:
        begin
          Part := TTP_STANDARD;
          Base := Ord(tttStandardNormal);
        end;
      tttStandardTitleNormal..tttStandardTitleLink:
        begin
          Part := TTP_STANDARDTITLE;
          Base := Ord(tttStandardTitleNormal);
        end;
      tttBaloonNormal..tttBaloonLink:
        begin
          Part := TTP_BALLOON;
          Base := Ord(tttBaloonNormal);
        end;
      tttBaloonTitleNormal..tttBaloonTitleLink:
        begin
          Part := TTP_BALLOONTITLE;
          Base := Ord(tttBaloonTitleNormal);
        end;
      tttCloseNormal..tttClosePressed:
        begin
          Part := TTP_CLOSE;
          Base := Ord(tttCloseNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedTrackBar): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teTrackBar;
  with Result do
  begin
    case Widget of
      ttbTrack:
        begin
          Part := TKP_TRACK;
          Base := Ord(ttbTrack);
        end;
      ttbTrackVert:
        begin
          Part := TKP_TRACKVERT;
          Base := Ord(ttbTrackVert);
        end;
      ttbThumbNormal..ttbThumbDisabled:
        begin
          Part := TKP_THUMB;
          Base := Ord(ttbThumbNormal);
        end;
      ttbThumbBottomNormal..ttbThumbBottomDisabled:
        begin
          Part := TKP_THUMBBOTTOM;
          Base := Ord(ttbThumbBottomNormal);
        end;
      ttbThumbTopNormal..ttbThumbTopDisabled:
        begin
          Part := TKP_THUMBTOP;
          Base := Ord(ttbThumbTopNormal);
        end;
      ttbThumbVertNormal..ttbThumbVertDisabled:
        begin
          Part := TKP_THUMBVERT;
          Base := Ord(ttbThumbVertNormal);
        end;
      ttbThumbLeftNormal..ttbThumbLeftDisabled:
        begin
          Part := TKP_THUMBLEFT;
          Base := Ord(ttbThumbLeftNormal);
        end;
      ttbThumbRightNormal..ttbThumbRightDisabled:
        begin
          Part := TKP_THUMBRIGHT;
          Base := Ord(ttbThumbRightNormal);
        end;
      ttbThumbTics:
        begin
          Part := TKP_TICS;
          Base := Ord(ttbThumbTics);
        end;
      ttbThumbTicsVert:
        begin
          Part := TKP_TICSVERT;
          Base := Ord(ttbThumbTicsVert);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedTrayNotify): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teTrayNotify;
  with Result do
  begin
    case Widget of
      ttnBackground:
        begin
          Part := TNP_BACKGROUND;
          Base := Ord(ttnBackground);
        end;
      ttnAnimBackground:
        begin
          Part := TNP_ANIMBACKGROUND;
          Base := Ord(ttnAnimBackground);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedTreeview): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teTreeView;
  with Result do
  begin
    case Widget of
      ttItemNormal..ttItemSelectedNotFocus:
        begin
          Part := TVP_TREEITEM;
          Base := Ord(ttItemNormal);
        end;
      ttGlyphClosed..ttGlyphOpened:
        begin
          Part := TVP_GLYPH;
          Base := Ord(ttGlyphClosed);
        end;
      ttBranch:
        begin
          Part := TVP_BRANCH;
          Base := Ord(ttBranch);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedWindow): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teWindow;
  with Result do
  begin
    case Widget of
      twCaptionActive..twCaptionDisabled:
        begin
          Part := WP_CAPTION;
          Base := Ord(twCaptionActive);
        end;
      twSmallCaptionActive..twSmallCaptionDisabled:
        begin
          Part := WP_SMALLCAPTION;
          Base := Ord(twSmallCaptionActive);
        end;
      twMinCaptionActive..twMinCaptionDisabled:
        begin
          Part := WP_MINCAPTION;
          Base := Ord(twMinCaptionActive);
        end;
      twSmallMinCaptionActive..twSmallMinCaptionDisabled:
        begin
          Part := WP_SMALLMINCAPTION;
          Base := Ord(twSmallMinCaptionActive);
        end;
      twMaxCaptionActive..twMaxCaptionDisabled:
        begin
          Part := WP_MAXCAPTION;
          Base := Ord(twMaxCaptionActive);
        end;
      twSmallMaxCaptionActive..twSmallMaxCaptionDisabled:
        begin
          Part := WP_SMALLMAXCAPTION;
          Base := Ord(twSmallMaxCaptionActive);
        end;
      twFrameLeftActive..twFrameLeftInactive:
        begin
          Part := WP_FRAMELEFT;
          Base := Ord(twFrameLeftActive);
        end;
      twFrameRightActive..twFrameRightInactive:
        begin
          Part := WP_FRAMERIGHT;
          Base := Ord(twFrameRightActive);
        end;
      twFrameBottomActive..twFrameBottomInactive:
        begin
          Part := WP_FRAMEBOTTOM;
          Base := Ord(twFrameBottomActive);
        end;
      twSmallFrameLeftActive..twSmallFrameLeftInactive:
        begin
          Part := WP_SMALLFRAMELEFT;
          Base := Ord(twSmallFrameLeftActive);
        end;
      twSmallFrameRightActive..twSmallFrameRightInactive:
        begin
          Part := WP_SMALLFRAMERIGHT;
          Base := Ord(twSmallFrameRightActive);
        end;
      twSmallFrameBottomActive..twSmallFrameBottomInactive:
        begin
          Part := WP_SMALLFRAMEBOTTOM;
          Base := Ord(twSmallFrameBottomActive);
        end;
      twSysButtonNormal..twSysButtonDisabled:
        begin
          Part := WP_SYSBUTTON;
          Base := Ord(twSysButtonNormal);
        end;
      twMDISysButtonNormal..twMDISysButtonDisabled:
        begin
          Part := WP_MDISYSBUTTON;
          Base := Ord(twMDISysButtonNormal);
        end;
      twMinButtonNormal..twMinButtonDisabled:
        begin
          Part := WP_MINBUTTON;
          Base := Ord(twMinButtonNormal);
        end;
      twMDIMinButtonNormal..twMDIMinButtonDisabled:
        begin
          Part := WP_MDIMINBUTTON;
          Base := Ord(twMDIMinButtonNormal);
        end;
      twMaxButtonNormal..twMaxButtonDisabled:
        begin
          Part := WP_MAXBUTTON;
          Base := Ord(twMaxButtonNormal);
        end;
      twCloseButtonNormal..twCloseButtonDisabled:
        begin
          Part := WP_CLOSEBUTTON;
          Base := Ord(twCloseButtonNormal);
        end;
      twSmallCloseButtonNormal..twSmallCloseButtonDisabled:
        begin
          Part := WP_SMALLCLOSEBUTTON;
          Base := Ord(twSmallCloseButtonNormal);
        end;
      twMDICloseButtonNormal..twMDICloseButtonDisabled:
        begin
          Part := WP_MDICLOSEBUTTON;
          Base := Ord(twMDICloseButtonNormal);
        end;
      twRestoreButtonNormal..twRestoreButtonDisabled:
        begin
          Part := WP_RESTOREBUTTON;
          Base := Ord(twRestoreButtonNormal);
        end;
      twMDIRestoreButtonNormal..twMDIRestoreButtonDisabled:
        begin
          Part := WP_MDIRESTOREBUTTON;
          Base := Ord(twMDIRestoreButtonNormal);
        end;
      twHelpButtonNormal..twHelpButtonDisabled:
        begin
          Part := WP_HELPBUTTON;
          Base := Ord(twHelpButtonNormal);
        end;
      twMDIHelpButtonNormal..twMDIHelpButtonDisabled:
        begin
          Part := WP_MDIHELPBUTTON;
          Base := Ord(twMDIHelpButtonNormal);
        end;
      twHorzScrollNormal..twHorzScrollDisabled:
        begin
          Part := WP_HORZSCROLL;
          Base := Ord(twHorzScrollNormal);
        end;
      twHorzThumbNormal..twHorzThumbDisabled:
        begin
          Part := WP_HORZTHUMB;
          Base := Ord(twHorzThumbNormal);
        end;
      twVertScrollNormal..twVertScrollDisabled:
        begin
          Part := WP_VERTSCROLL;
          Base := Ord(twVertScrollNormal);
        end;
      twVertThumbNormal..twVertThumbDisabled:
        begin
          Part := WP_VERTTHUMB;
          Base := Ord(twVertThumbNormal);
        end;
      twDialog:
        begin
          Part := WP_DIALOG;
          Base := Ord(twDialog);
        end;
      twCaptionSizingTemplate:
        begin
          Part := WP_CAPTIONSIZINGTEMPLATE;
          Base := Ord(twCaptionSizingTemplate);
        end;
      twSmallCaptionSizingTemplate:
        begin
          Part := WP_SMALLCAPTIONSIZINGTEMPLATE;
          Base := Ord(twSmallCaptionSizingTemplate);
        end;
      twFrameLeftSizingTemplate:
        begin
          Part := WP_FRAMELEFTSIZINGTEMPLATE;
          Base := Ord(twFrameLeftSizingTemplate);
        end;
      twSmallFrameLeftSizingTemplate:
        begin
          Part := WP_SMALLFRAMELEFTSIZINGTEMPLATE;
          Base := Ord(twSmallFrameLeftSizingTemplate);
        end;
      twFrameRightSizingTemplate:
        begin
          Part := WP_FRAMERIGHTSIZINGTEMPLATE;
          Base := Ord(twFrameRightSizingTemplate);
        end;
      twSmallFrameRightSizingTemplate:
        begin
          Part := WP_SMALLFRAMERIGHTSIZINGTEMPLATE;
          Base := Ord(twSmallFrameRightSizingTemplate);
        end;
      twFrameBottomSizingTemplate:
        begin
          Part := WP_FRAMEBOTTOMSIZINGTEMPLATE;
          Base := Ord(twFrameBottomSizingTemplate);
        end;
      twSmallFrameBottomSizingTemplate:
        begin
          Part := WP_SMALLFRAMEBOTTOMSIZINGTEMPLATE;
          Base := Ord(twSmallFrameBottomSizingTemplate);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.ColorToRGB(Color: TColor; Details: PThemedDetails = nil): COLORREF;
begin
  if (Color and $80000000 = 0) or (Details = nil) then
    Result := Color
  else
    Result := GetThemeSysColor(Theme[Details.Element], Color and not $80000000);
end;

function TThemePainter.ContentRect(DC: HDC; const Details: TThemedDetails; BoundingRect: TRect): TRect;
begin
  with Details do
    GetThemeBackgroundContentRect(Theme[Element], DC, Part, State, BoundingRect, @Result);
end;

procedure TThemePainter.DrawEdge(DC: HDC; const Details: TThemedDetails; const R: TRect; Edge, Flags: Cardinal;
  ContentRect: PRect = nil);
begin
  with Details do
    DrawThemeEdge(Theme[Element], DC, Part, State, R, Edge, Flags, ContentRect);
end;

procedure TThemePainter.DrawElement(DC: HDC; const Details: TThemedDetails; const R: TRect; ClipRect: PRect = nil);
begin
  with Details do
    DrawThemeBackground(Theme[Element], DC, Part, State, R, ClipRect);
end;

procedure TThemePainter.DrawIcon(DC: HDC; const Details: TThemedDetails; const R: TRect; himl: HIMAGELIST;
  Index: Integer);
begin
  with Details do
    DrawThemeIcon(Theme[Element], DC, Part, State, R, himl, Index);
end;

procedure TThemePainter.DrawParentBackground(Window: HWND; Target: HDC; Details: PThemedDetails;
  OnlyIfTransparent: Boolean; Bounds: PRect = nil);
var
  DoDraw: Boolean;
begin
  if OnlyIfTransparent and Assigned(Details) then
  begin
    with Details^ do
      DoDraw := IsThemeBackgroundPartiallyTransparent(Theme[Element], Part, State);
  end
  else
    DoDraw := True;
  if DoDraw then
    DrawThemeParentBackground(Window, Target, Bounds);
end;

procedure TThemePainter.DrawText(DC: HDC; const Details: TThemedDetails; const S: WideString; R: TRect; Flags,
  Flags2: Cardinal);
begin
  with Details do
    DrawThemeText(Theme[Element], DC, Part, State, PWideChar(S), Length(S), Flags, Flags2, R);
end;

function TThemePainter.HasTransparentParts(Details: TThemedDetails): Boolean;
begin
  with Details do
    Result := IsThemeBackgroundPartiallyTransparent(Theme[Element], Part, State);
end;

function TThemePainter.PartSize(DC: HDC; const Details: TThemedDetails;
	Size: TThemeSize = tsDraw): TSize;
begin
  with Details do
  	GetThemePartSize(Theme[Element], DC, Part, State, nil, Size, Result);
end;

procedure TThemePainter.UpdateThemes;
begin
  if FAvailable then
    UnloadThemeData;
  FAvailable := ThemesLoaded;
  if FAvailable then
    FControlsEnabled := GetThemeAppProperties and STAP_ALLOW_CONTROLS = STAP_ALLOW_CONTROLS
  else
    FControlsEnabled := False;
end;

procedure DrawThemeBorder(DC: HDC; Color: TColor; const Rect: TRect; State: TDrawState);
var
	Brush: HBRUSH;
  Rgn: HRGN;
  R: TRect;
begin
  Brush := CreateSolidBrush(ColorToRGB(Color));
  FillRect(DC, Rect, Brush);
  with ThemePainter do
    if Enabled then
    begin
      R := Rect;
      Rgn := CreateRectRgn(0, 0, 1, 1);
      if GetClipRgn(DC, Rgn) <> 1 then
      begin
      	DeleteObject(Rgn);
        Rgn := 0;
      end;
			ExcludeClipRect(DC, Rect.Left + 1, Rect.Top + 1, Rect.Right - 1, Rect.Bottom - 1);
      InflateRect(R, -1, -1);
      DrawElement(DC, GetDetails(tcComboBoxRoot), Rect);
      SelectClipRgn(DC, Rgn);
      if Rgn <> 0 then
      	DeleteObject(Rgn);
    end
    else if dsThin in State then
      DrawFrame(DC, Rect, dfLowered)
    else
      DrawFrame(DC, Rect, dfSunken);
  Brush := SelectObject(DC, Brush);
  {if (Rect.Left + 3 < Rect.Right) and (Rect.Top + 3 < Rect.Bottom) then
	  ExtFloodFill(DC, Rect.Top + 3, Rect.Left + 3, GetSysColor(COLOR_WINDOW),
    	FLOODFILLSURFACE);}
	OverwriteObject(DC, Brush);
end;

procedure DrawThemeButton(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedButton;
  R: TRect;
begin
	if dsFlat in State then
		DrawThemeThinButton(DC, Rect, State)
  else if ThemePainter.Enabled then
  begin
    Theme := tbPushButtonNormal;
    if dsDisabled in State then
      Theme := tbPushButtonDisabled
    else if dsPressed in State then
      Theme := tbPushButtonPressed
    else if dsHot in State then
      Theme := tbPushButtonHot
    else if dsFocused in State then
      Theme := tbPushButtonDefaulted;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  begin
    R := Rect;
    if dsFocused in State then
    begin
	    DrawFrame(DC, R, dfFocus);
      InflateRect(R, -1, -1);
    end;
    if dsPressed in State then
	    DrawFrame(DC, R, dfPressed)
    else
	    DrawFrame(DC, R, dfFramed);
  end;
end;

procedure DrawThemeThinButton(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedToolBar;
begin
  if ThemePainter.Enabled then
  begin
    Theme := ttbButtonNormal;
    if dsDisabled in State then
      Theme := ttbButtonDisabled
    else if dsPressed in State then
      Theme := ttbButtonPressed
    else if dsHot in State then
      Theme := ttbButtonHot;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  begin
		if dsDisabled in State then Exit;
    if dsPressed in State then
	    DrawFrame(DC, Rect, dfLowered)
    else if dsHot in State then
	    DrawFrame(DC, Rect, dfRaised);
  end;
end;

procedure DrawThemeNode(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedTreeview;
begin
  if ThemePainter.Enabled then
  begin
    if dsExpanded in State then
      Theme := ttGlyphOpened
    else
      Theme:= ttGlyphClosed;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
    DrawNode(DC, Rect, dsExpanded in State);
end;

procedure DrawThemeGroupBox(DC: HDC; const Text: string; const Rect: TRect;
	State: TDrawState);
var
  GroupRect: TRect;
  CaptionRect: TRect;
	Details: TThemedDetails;
begin
  GroupRect := Rect;
  CaptionRect := Rect;
  with CalculateCaptionSize(DC, Text), CaptionRect  do
  begin
    Bottom := Top + cY;
    Right := Left + cX + 4;
    OffsetRect(CaptionRect, HeightOf(CaptionRect) div 4 * 3, 0);
    ExcludeClipRect(DC, Left, Top, Right, Bottom);
  end;
  GroupRect.Top := GroupRect.Top + HeightOf(CaptionRect) div 2;
  if ThemePainter.Enabled then
	  with ThemePainter do
    begin
    	if dsDisabled in State then
			  Details := GetDetails(tbGroupBoxDisabled)
      else
			  Details := GetDetails(tbGroupBoxNormal);
      DrawElement(DC, Details, GroupRect);
      SelectClipRgn(DC, 0);
      DrawText(DC, Details, Text, CaptionRect, DT_CENTER or DT_SINGLELINE, 0);
    end
  else
  begin
    DrawFrame(DC, GroupRect, dfLowered);
    InflateRect(GroupRect, -1, -1);
    DrawFrame(DC, GroupRect, dfRaised);
    SelectClipRgn(DC, 0);
    DrawCaption(DC, Text, CaptionRect, drCenter)
  end;
end;

procedure DrawThemeExpandableBox(DC: HDC; const Text: string; const Rect: TRect;
	State: TDrawState);
var
  GroupRect: TRect;
  CaptionRect: TRect;
  NodeRect: TRect;
  ClipRect: TRect;
	Details: TThemedDetails;
begin
  GroupRect := Rect;
  CaptionRect := Rect;
  NodeRect := Rect;
  NodeRect.Right := NodeRect.Left + NodeSize;
  NodeRect.Bottom := NodeRect.Top + NodeSize;
  with CalculateCaptionSize(DC, Text), CaptionRect  do
  begin
    Inc(Top);
    Left := Left + 2;
    Bottom := Top + cY;
    Right := Left + cX + 4;
    OffsetRect(CaptionRect, NodeSize + 2, 0);
  end;
  if HeightOf(CaptionRect) > NodeSize then
    OffsetRect(NodeRect, 0, (HeightOf(CaptionRect) - NodeSize) div 2);
  ClipRect := CaptionRect;
  with ClipRect do
  begin
    Left := Rect.Left;
    ExcludeClipRect(DC, Left, Top, Right, Bottom);
  end;
  GroupRect.Top := GroupRect.Top + HeightOf(CaptionRect) div 2 + 1;
  GroupRect.Left := GroupRect.Left + NodeSize div 2;
  if not (dsExpanded in State) then
	  with GroupRect do
    begin
  	  ExcludeClipRect(DC, Right - 4, Top, Right, Bottom);
  	  ExcludeClipRect(DC, ClipRect.Left, Top + 2, Right, Bottom);
    end;
  if ThemePainter.Enabled then
	  with ThemePainter do
    begin
      if dsDisabled in State then
        Details := GetDetails(tbGroupBoxNormal)
      else
        Details := GetDetails(tbGroupBoxDisabled);
	    DrawElement(DC, Details, GroupRect);
      SelectClipRgn(DC, 0);
      DrawText(DC, Details, Text, CaptionRect, DT_CENTER or DT_SINGLELINE, 0);
      if dsPressed in State then
			  Details := GetDetails(ttbButtonPressed)
      else
			  Details := GetDetails(ttbButtonHot);
      DrawElement(DC, Details, NodeRect);
      if dsExpanded in State then
			  Details := GetDetails(ttGlyphOpened)
      else
			  Details := GetDetails(ttGlyphClosed);
      DrawElement(DC, Details, NodeRect);
    end
  else
  begin
    DrawFrame(DC, GroupRect, dfLowered);
    InflateRect(GroupRect, -1, -1);
    DrawFrame(DC, GroupRect, dfRaised);
    SelectClipRgn(DC, 0);
    DrawCaption(DC, Text, CaptionRect, drCenter);
    FilLRect(DC, NodeRect, COLOR_BTNFACE + 1);
    if dsPressed in State then
	    DrawFrame(DC, NodeRect, dfLowered)
    else
	    DrawFrame(DC, NodeRect, dfRaised);
    DrawNode(DC, NodeRect, dsExpanded in State);
  end;
end;

procedure DrawThemeHorzThumb(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedTrackBar;
begin
  if ThemePainter.Enabled then
  begin
    Theme := ttbThumbNormal;
    if dsDisabled in State then
      Theme := ttbThumbDisabled
    else if dsPressed in State then
      Theme := ttbThumbPressed
    else if dsHot in State then
      Theme := ttbThumbHot
    else if dsFocused in State then
      Theme := ttbThumbFocused;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
		DrawFrame(DC, Rect, dfRaised);
end;

procedure DrawThemeHorzSplit(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedToolBar;
  R: TRect;
begin
  if ThemePainter.Enabled then
  begin
		R := Rect;
    Dec(R.Left, 2);
    Inc(R.Right, 2);
    Theme := ttbSeparatorNormal;
    if dsDisabled in State then
      Theme := ttbSeparatorDisabled
    else if dsPressed in State then
      Theme := ttbSeparatorPressed
    else if dsHot in State then
      Theme := ttbSeparatorHot;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), R);
  end
  else
		DrawDivider(DC, Rect, ddHorz);
end;

procedure DrawThemeVertThumb(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedTrackBar;
begin
  if ThemePainter.Enabled then
  begin
    Theme := ttbThumbVertNormal;
    if dsDisabled in State then
      Theme := ttbThumbVertDisabled
    else if dsPressed in State then
      Theme := ttbThumbVertPressed
    else if dsHot in State then
      Theme := ttbThumbVertHot
    else if dsFocused in State then
      Theme := ttbThumbVertFocused;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
		DrawFrame(DC, Rect, dfRaised);
end;

procedure DrawThemeVertSplit(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedToolBar;
  R: TRect;
begin
  if ThemePainter.Enabled then
  begin
		R := Rect;
    Dec(R.Top, 2);
    Inc(R.Bottom, 2);
    Theme := ttbSeparatorVertNormal;
    if dsDisabled in State then
      Theme := ttbSeparatorVertDisabled
    else if dsPressed in State then
      Theme := ttbSeparatorVertPressed
    else if dsHot in State then
      Theme := ttbSeparatorVertHot;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), R);
  end
  else
		DrawDivider(DC, Rect, ddVert);
end;

procedure DrawThemeClose(DC: HDC; const Rect: TRect; State: TDrawState; Color: TColor = clWindowFrame);
var
  Theme: TThemedExplorerBar;
begin
	if ThemePainter.Enabled then
  begin
		if dsDisabled in State then
    	Theme := tebHeaderCloseNormal
		else if dsPressed in State then
    	Theme := tebHeaderClosePressed
		else if dsHot in State then
    	Theme := tebHeaderCloseHot
		else
    	Theme := tebHeaderCloseNormal;
		ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  	GlyphFrame(DC, Rect, gkClose, State, Color);
end;

procedure DrawThemeToolClose(DC: HDC; const Rect: TRect; State: TDrawState; Color: TColor = clWindowFrame);
var
  Theme: TThemedToolTip;
begin
	if ThemePainter.Enabled then
  begin
		if dsDisabled in State then
    	Theme := tttCloseNormal
		else if dsPressed in State then
    	Theme := tttClosePressed
		else if dsHot in State then
    	Theme := tttCloseHot
		else
    	Theme := tttCloseNormal;
		ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  	GlyphFrame(DC, Rect, gkClose, State, Color);
end;

procedure DrawThemePin(DC: HDC; const Rect: TRect; State: TDrawState; Color: TColor = clWindowFrame);
var
  Theme: TThemedExplorerBar;
begin
	if ThemePainter.Enabled then
  begin
		if dsDisabled in State then
    	Theme := tebHeaderPinSelectedNormal
		else if dsPressed in State then
    	Theme := tebHeaderPinSelectedPressed
		else if dsHot in State then
    	Theme := tebHeaderPinSelectedHot
		else
    	Theme := tebHeaderPinNormal;
		ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else if dsPressed in State then
  	GlyphFrame(DC, Rect, gkPinPushed, State, Color)
  else
  	GlyphFrame(DC, Rect, gkPin, State, Color);
end;

procedure DrawThemeArrow(DC: HDC; Direction: TDirection; const Rect: TRect; State: TDrawState;
 Color: TColor = clWindowFrame; Adjust: Integer = 0);
const
	GlyphSize = 17;
var
	Theme: TThemedScrollBar;
	R: TRect;
  I: Integer;
begin
  if ThemePainter.Enabled then
  begin
  	case Direction of
      drLeft: Theme := tsArrowBtnLeftNormal;
			drUp: Theme := tsArrowBtnUpNormal;
      drRight: Theme := tsArrowBtnRightNormal;
      drDown: Theme := tsArrowBtnDownNormal;
    else
    	Exit;
		end;
    if dsDisabled in State then
			Inc(Theme, 3)
    else if dsPressed in State then
			Inc(Theme, 2)
    else if dsHot in State then
			Inc(Theme, 1);
    R := Rect;
    I := (WidthOf(Rect) - GlyphSize) div 2;
    Inc(R.Left, I); //  + Adjust
    I := (HeightOf(Rect) - GlyphSize) div 2;
    Inc(R.Top, I + Adjust);
    R.Right := R.Left + GlyphSize;
    R.Bottom := R.Top + GlyphSize;
  	ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), R);
  end
  else
		DrawArrow(DC, Rect, Direction, Color, not (dsDisabled in State));
end;

procedure DrawScroll(DC: HDC; Theme: TThemedScrollBar; Rect: TRect);
var
 W, H, X, Y: Integer;
 A: TFastBitmap;
begin
  W := WidthOf(Rect);
  H := HeightOf(Rect);
  if (W < 1) or (H < 1) then Exit;
  X := Rect.Left;
  Y := Rect.Top;
  OffsetRect(Rect, -X, -Y);
  A := CreateFastBitmap(W, H);
  ThemePainter.DrawElement(A.DC, ThemePainter.GetDetails(Theme), Rect);
  StretchBlt(A.DC, 3, 3, W - 6, H - 6, A.DC, 3, 3, 1, H - 6, SRCCOPY);
  BitBlt(DC, X, Y, W, H, A.DC, 0, 0, SRCCOPY);
  DestroyFastBitmap(A);
end;

procedure DrawThemeScroll(DC: HDC; Direction: TDirection; const Rect: TRect; State: TDrawState);
var
	Theme: TThemedScrollBar;
  FrameState: Cardinal;
begin
  if ThemePainter.Enabled then
  begin
  	Theme := tsArrowBtnLeftNormal;
  	case Direction of
      drLeft: Theme := tsArrowBtnLeftNormal;
			drUp: Theme := tsArrowBtnUpNormal;
      drRight: Theme := tsArrowBtnRightNormal;
      drDown: Theme := tsArrowBtnDownNormal;
      drCenter: Theme := tsArrowBtnDownNormal;
		end;
    if dsDisabled in State then
			Inc(Theme, 3)
    else if dsPressed in State then
			Inc(Theme, 2)
    else if dsHot in State then
			Inc(Theme, 1);
    if Direction = drCenter then
      DrawScroll(DC, Theme, Rect)
    else
    	ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  begin
		FrameState := 0;
  	case Direction of
      drLeft: FrameState := DFCS_SCROLLLEFT;
			drUp: FrameState := DFCS_SCROLLUP;
      drRight: FrameState := DFCS_SCROLLRIGHT;
      drDown: FrameState := DFCS_SCROLLDOWN;
      drCenter: FrameState := DFCS_BUTTONPUSH;
		end;
    if dsThin in State then
			FrameState := FrameState or DFCS_FLAT;
    if dsDisabled in State then
			FrameState := FrameState or DFCS_INACTIVE
    else if dsPressed in State then
			FrameState := FrameState or DFCS_PUSHED;
    if Direction = drCenter then
	    DrawFrameControl(DC, Rect, DFC_BUTTON, FrameState)
    else
	    DrawFrameControl(DC, Rect, DFC_SCROLL, FrameState);
  end;
end;

procedure DrawThemeGrip(DC: HDC; const Rect: TRect);
var
  DrawRect: TRect;
begin
  DrawRect := Rect;
  DrawRect.Left := DrawRect.Right - GetSystemMetrics(SM_CXVSCROLL);
  with ThemePainter do
    if Enabled then
      DrawElement(DC, GetDetails(tsGripper), DrawRect)
    else
      DrawGrip(DC, DrawRect);
end;

procedure DrawThemeCloseBox(DC: HDC; const Text: string; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedExplorerBar;
  R: TRect;
begin
	R := Rect;
	with ThemePainter do
	  if ThemePainter.Enabled then
	    DrawElement(DC, GetDetails(teEditTextNormal), R)
    else
    begin
    	DrawFrame(DC, R, dfSunken);
			InflateRect(R, -2, -2);
      FillRect(DC, R, COLOR_WINDOW + 1);
    end;
	R := Rect;
	InflateRect(R, -2, -2);
  R.Bottom := Rect.Top + 29;
	with ThemePainter do
    if Enabled then
    begin
      DrawElement(DC, GetDetails(trRebarRoot), R);
    end
    else
    begin
      DrawFrame(DC, R, dfRaised);
      Dec(R.Bottom);
      FillRect(DC, R, COLOR_BTNFACE + 1);
      Inc(R.Bottom);
    end;
	Inc(R.Left, 5);
  DrawCaption(DC, Text, R, drLeft);
  R.Left := R.Right - HeightOf(R);
  if ThemePainter.Enabled then
  begin
    Theme := tebHeaderCloseNormal;
    if dsPressed in State then
      Theme := tebHeaderClosePressed
    else if dsHot in State then
	    Theme := tebHeaderCloseHot;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), R);
    Slide(R, drLeft);
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(tebHeaderPinNormal), R);
	end
  else
  begin
    DrawClose(DC, R);
    Slide(R, drLeft);
		GlyphDraw(DC, R, gkPin, clWindowFrame);
  end;
end;

procedure DrawThemeStatus(DC: HDC; const Text: string; const Rect: TRect);
var
	R: TRect;
begin
 	with ThemePainter do
	  if Enabled then
		begin
    	R := Rect;
  		DrawElement(DC, GetDetails(tsStatusRoot), R);
	    R.Left := R.Right - HeightOf(R);
	  	DrawElement(DC, GetDetails(tsGripper), R);
	  end
    else
    begin
  		FillRect(DC, Rect, COLOR_BTNFACE + 1);
			DrawGrip(DC, Rect, False);
      DrawDivider(DC, Rect, ddVert);
    end;
	R := Rect;
  R.Right := R.Right - HeightOf(Rect);
	Inc(R.Left, HeightOf(R) div 2);
	DrawCaption(DC,Text, R, drLeft)
end;

function CreateBlendSection(DC: HDC; Width: Integer; Height: Integer): HBITMAP;
var
  BitmapInfo: TBitmapInfo;
  Bits: Pointer;
begin
  FillChar(BitmapInfo, SizeOf(TBitmapInfo), #0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := 24;
    biCompression := BI_RGB;
  end;
  Result := CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, Bits, 0, 0);
end;

function BlendImages(Source1, Source2, Dest: HBITMAP; Weight: Byte): Boolean;

  function BitmapsCompatible(const B1, B2: Windows.TBitmap): Boolean;
  begin
    Result := (B1.bmBitsPixel = B2.bmBitsPixel) and (B1.bmPlanes = B2.bmPlanes) and
     (B1.bmWidth = B2.bmWidth) and (B1.bmHeight = B2.bmHeight);
  end;

type
  TRGBTripleArray = array[0..0] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;
var
  B1, B2, D: Windows.TBitmap;
  RGBSrc1, RGBSrc2, RGBDest: PRGBTripleArray;
  WidthBytes: Cardinal;
  Compliment: Byte;
  X, Y: Integer;
begin
  Result := False;
  if not ((GetObject(Source1, SizeOf(Windows.TBitmap), @B1) <> 0) and
    (GetObject(Source2, SizeOf(Windows.TBitmap), @B2) <> 0) and
    (GetObject(Dest, SizeOf(Windows.TBitmap), @D) <> 0) and BitmapsCompatible(B1, B2) and
    BitmapsCompatible(B1, D) and (B1.bmBitsPixel = 24) and (B1.bmPlanes = 1) and
    (B1.bmBits <> nil) and (B2.bmBits <> nil) and (D.bmBits <> nil)) then
    Exit;
  Compliment := 255 - Weight;
  WidthBytes := D.bmWidthBytes;
  RGBSrc1 := B1.bmBits;
  RGBSrc2 := B2.bmBits;
  RGBDest := D.bmBits;
  for Y := 0 to D.bmHeight - 1 do
  begin
    for X := 0 to D.bmWidth - 1 do
    begin
      RGBDest^[X].rgbtRed := Byte(((Cardinal(RGBSrc1^[X].rgbtRed) * Weight) +
        (Cardinal(RGBSrc2^[X].rgbtRed) * Compliment)) shr 8);
      RGBDest^[X].rgbtGreen := Byte(((Cardinal(RGBSrc1^[X].rgbtGreen) * Weight) +
        (Cardinal(RGBSrc2^[X].rgbtGreen) * Compliment)) shr 8);
      RGBDest^[X].rgbtBlue := Byte(((Cardinal(RGBSrc1^[X].rgbtBlue) * Weight) +
        (Cardinal(RGBSrc2^[X].rgbtBlue) * Compliment)) shr 8);
    end;
    RGBSrc1 := PRGBTripleArray(Cardinal(RGBSrc1) + WidthBytes);
    RGBSrc2 := PRGBTripleArray(Cardinal(RGBSrc2)+ WidthBytes);
    RGBDest := PRGBTripleArray(Cardinal(RGBDest) + WidthBytes);
  end;
  Result := True;
end;

procedure AlphaBlend(Dest: HDC; DestX, DestY, DestWidth,
  DestHeight: Integer; Source: HDC; SourceX, SourceY, SourceWidth,
  SourceHeight: Integer; SourceWeight: Byte);
var
  S1, S2, D1: HDC;
  B1, B2, B3: HBITMAP;
begin
  B1 := CreateBlendSection(Dest, DestWidth, DestHeight);
  B2 := CreateBlendSection(Dest, DestWidth, DestHeight);
  B3 := CreateBlendSection(Dest, DestWidth, DestHeight);
  S1 := CreateCompatibleDC(Dest);
  S2 := CreateCompatibleDC(Dest);
  D1 := CreateCompatibleDC(Dest);
  SelectObject(S1, B1);
  SelectObject(S2, B2);
  SelectObject(D1, B3);
  SetStretchBltMode(S1, COLORONCOLOR);
  SetStretchBltMode(S2, COLORONCOLOR);
  StretchBlt(S1, 0, 0, DestWidth, DestHeight, Source, SourceX,
    SourceY, SourceWidth, SourceHeight, SRCCOPY);
  StretchBlt(S2, 0, 0, DestWidth, DestHeight,Dest, DestX, DestY,
    DestWidth, DestHeight, SRCCOPY);
  BlendImages(B1, B2, B3, SourceWeight);
  DeleteDC(S1);
  DeleteDC(S2);
  DeleteObject(B1);
  DeleteObject(B2);
  BitBlt(Dest, DestX, DestY, DestWidth, DestHeight, D1, 0, 0, SRCCOPY);
  DeleteDC(D1);
  DeleteObject(B3);
end;

function HexToStream(const HexData: string): TStream;
var
  P: Pointer;
  I: Integer;
begin
  I := Length(HexData) div 2;
  GetMem(P, I);
  try
    HexToBin(PChar(HexData), PChar(P), I);
    Result := TMemoryStream.Create;
    Result.Read(P^, I);
  finally
    FreeMem(P);
  end;
end;

function StreamToHex(Stream: TStream): string;
var
  PriorPosition: Integer;
  P: Pointer;
  I: Integer;
begin
  PriorPosition := Stream.Position;
  Stream.Seek(0, 0);
  I := Stream.Size;
  GetMem(P, I);
  try
    Stream.Write(P^, I);
    SetLength(Result, I * 2);
    BinToHex(PChar(P), PChar(Result), I);
  finally
    Stream.Seek(PriorPosition, 0);
    FreeMem(P);
  end;
end;

procedure LoadGraphic(Graphic: TGraphic; const HexData: string);
var
  Stream: TStream;
begin
  Stream := HexToStream(HexData);
  try
    Graphic.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure LoadGraphicResource(Graphic: TGraphic; Ident: Integer);
var
  Format: PChar;
  Stream: TStream;
begin
  Format := nil;
  if Graphic.ClassType = TBitmap then
    Format := RT_BITMAP
  else if Graphic.ClassType = TIcon then
    Format := RT_ICON
  else if Graphic.ClassType = TMetaFile then
    Format := RT_RCDATA;
  Stream := TResourceStream.CreateFromID(HInstance, Ident, Format);
  try
    Graphic.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitializeGlyphs;
var
	I: TGlyphKind;
begin
	for I := Low(InternalGlyphs) to High(InternalGlyphs) do
  	InternalGlyphs[I] := nil;
end;

procedure FinalizeGlyphs;
var
	I: TGlyphKind;
begin
	for I := Low(InternalGlyphs) to High(InternalGlyphs) do
  	InternalGlyphs[I].Free;
end;

initialization
	InternalThemePainter := nil;
  ScratchFont := nil;
  ScratchBitmap := TBitmap.Create;
  with ScratchBitmap do
  begin
    Width := 150;
    Height := 50;
  end;
  InitializeGlyphs;
finalization
	FinalizeGlyphs;
  ScratchBitmap.Free;
  ScratchFont.Free;
  InternalThemePainter.Free;
end.
