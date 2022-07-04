unit SpTBXSkins;

{==============================================================================
Version 2.4

The contents of this file are subject to the SpTBXLib License; you may
not use or distribute this file except in compliance with the
SpTBXLib License.
A copy of the SpTBXLib License may be found in SpTBXLib-LICENSE.txt or at:
  http://www.silverpointdevelopment.com/sptbxlib/SpTBXLib-LICENSE.htm

Alternatively, the contents of this file may be used under the terms of the
Mozilla Public License Version 1.1 (the "MPL v1.1"), in which case the provisions
of the MPL v1.1 are applicable instead of those in the SpTBXLib License.
A copy of the MPL v1.1 may be found in MPL-LICENSE.txt or at:
  http://www.mozilla.org/MPL/

If you wish to allow use of your version of this file only under the terms of
the MPL v1.1 and not to allow others to use your version of this file under the
SpTBXLib License, indicate your decision by deleting the provisions
above and replace them with the notice and other provisions required by the
MPL v1.1. If you do not delete the provisions above, a recipient may use your
version of this file under either the SpTBXLib License or the MPL v1.1.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The initial developer of this code is Robert Lee.

Requirements:
For Delphi/C++Builder 2009 or newer:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
For Delphi/C++Builder 7-2007:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
  - Troy Wolbrink's TNT Unicode Controls
    http://www.tntware.com/delphicontrols/unicode/

Development notes:
  - All the Windows and Delphi bugs fixes are marked with '[Bugfix]'.
  - All the theme changes and adjustments are marked with '[Theme-Change]'.
  - DT_END_ELLIPSIS and DT_PATH_ELLIPSIS doesn't work with rotated text
    http://support.microsoft.com/kb/249678

History:
17 January 2009 - version 2.4
  - Minor Fixes.

26 September 2008 - version 2.3
  - Fixed incorrect skin loading when the Aluminum skin was used,
    thanks to Costas Stergiou for reporting this.

29 July 2008 - version 2.2
  - Fixed incorrect menu items painting on Vista when the Windows
    themes was disabled, thanks to Arvid for reporting this.

26 June 2008 - version 2.1
  - Added Windows Vista specific constants to support Vista
    themes on Delphi versions prior to 2007, thanks to Wolf B.
    for his contribution.

3 May 2008 - version 2.0
  - Renamed TSpTBXSkinOptions.TitleBarBorderSize to
    FloatingWindowBorderSize.

2 April 2008 - version 1.9.5
  - No changes.

3 February 2008 - version 1.9.4
  - Added TitleBarBorderSize to the skins options.

19 January 2008 - version 1.9.3
  - No changes.

26 December 2007 - version 1.9.2
  - New gradient skin style added to mimic Vista toolbar gradients, use
    9 or 10 gradient style to paint vertically or horizontally.

1 December 2007 - version 1.9.1
  - Added Header and Tabs Toolbar skinning. skncHeader and skncTabToolbar
    skin elements were added to the skin components type.
  - Added SpDrawXPHeader utility function to paint the header controls.

20 November 2007 - version 1.9
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Graphics, Controls, StdCtrls,
  ImgList, IniFiles;

const
  WM_SPSKINCHANGE = WM_APP + 2007;   // Skin change notification message

  { Windows Vista theme painting constants }
  MENU_POPUPBACKGROUND = 9;
  MENU_POPUPBORDERS = 10;
  MENU_POPUPCHECK = 11;
  MENU_POPUPCHECKBACKGROUND = 12;
  MENU_POPUPGUTTER = 13;
  MENU_POPUPITEM = 14;
  MENU_POPUPSEPARATOR = 15;
  MPI_NORMAL = 1;
  MPI_HOT = 2;
  MPI_DISABLED = 3;
  MPI_DISABLEDHOT = 4;
  MCB_DISABLED = 1;
  MCB_NORMAL = 2;
  MCB_BITMAP = 3;
  MC_CHECKMARKNORMAL = 1;
  MC_CHECKMARKDISABLED = 2;
  MC_BULLETNORMAL = 3;
  MC_BULLETDISABLED = 4;

type
  { Skins }

  TSpTBXSkinType = (
    sknNone,         // No themes
    sknWindows,      // Use Windows themes
    sknSkin          // Use Skins
  );

  TSpTBXLunaScheme = (
    lusBlue,
    lusMetallic,
    lusGreen,
    lusUnknown
  );

  TSpTBXSkinComponentsType = (
    skncDock,
    skncDockablePanel,
    skncDockablePanelTitleBar,
    skncGutter,
    skncMenuBar,
    skncOpenToolbarItem,    
    skncPanel,
    skncPopup,
    skncSeparator,
    skncSplitter,
    skncStatusBar,
    skncStatusBarGrip,
    skncTabBackground,
    skncTabToolbar,
    skncToolbar,
    skncToolbarGrip,
    skncWindow,
    skncWindowTitleBar,

    // Multiple States
    skncMenuBarItem,
    skncMenuItem,
    skncToolbarItem,
    skncButton,
    skncCheckBox,
    skncEditButton,
    skncEditFrame,
    skncHeader,
    skncLabel,
    skncListItem,
    skncProgressBar,
    skncRadioButton,
    skncTab,
    skncTrackBar,
    skncTrackBarButton
  );
  TSpTBXSkinStatesType = (sknsNormal, sknsDisabled, sknsHotTrack, sknsPushed, sknsChecked, sknsCheckedAndHotTrack);

  TSpTBXSkinStatesSet = set of TSpTBXSkinStatesType;

  TSpTBXSkinPartsType = (sknpBody, sknpBorders, sknpText);

  TSpTBXSkinComponentsIdentEntry = record
    Name: string;
    States: TSpTBXSkinStatesSet;
  end;

const
  SpTBXSkinMultiStateComponents: set of TSpTBXSkinComponentsType = [skncMenuBarItem..High(TSpTBXSkinComponentsType)];

  CSpTBXSkinAllStates = [Low(TSpTBXSkinStatesType)..High(TSpTBXSkinStatesType)];
  CSpTBXSkinComponents: array [TSpTBXSkinComponentsType] of TSpTBXSkinComponentsIdentEntry = (
    // Single state Components
    (Name: 'Dock';                  States: [sknsNormal]),
    (Name: 'DockablePanel';         States: [sknsNormal]),
    (Name: 'DockablePanelTitleBar'; States: [sknsNormal]),
    (Name: 'Gutter';                States: [sknsNormal]),
    (Name: 'MenuBar';               States: [sknsNormal]),
    (Name: 'OpenToolbarItem';       States: [sknsNormal]),
    (Name: 'Panel';                 States: [sknsNormal]),
    (Name: 'Popup';                 States: [sknsNormal]),
    (Name: 'Separator';             States: [sknsNormal]),
    (Name: 'Splitter';              States: [sknsNormal]),
    (Name: 'StatusBar';             States: [sknsNormal]),
    (Name: 'StatusBarGrip';         States: [sknsNormal]),
    (Name: 'TabBackground';         States: [sknsNormal]),
    (Name: 'TabToolbar';            States: [sknsNormal]),
    (Name: 'Toolbar';               States: [sknsNormal]),
    (Name: 'ToolbarGrip';           States: [sknsNormal]),
    (Name: 'Window';                States: [sknsNormal]),
    (Name: 'WindowTitleBar';        States: [sknsNormal]),
    // Multi state Components
    (Name: 'MenuBarItem';           States: CSpTBXSkinAllStates),
    (Name: 'MenuItem';              States: CSpTBXSkinAllStates),
    (Name: 'ToolbarItem';           States: CSpTBXSkinAllStates),
    (Name: 'Button';                States: CSpTBXSkinAllStates),
    (Name: 'CheckBox';              States: CSpTBXSkinAllStates),
    (Name: 'EditButton';            States: CSpTBXSkinAllStates),
    (Name: 'EditFrame';             States: [sknsNormal, sknsDisabled, sknsHotTrack]),
    (Name: 'Header';                States: [sknsNormal, sknsDisabled, sknsHotTrack, sknsPushed]),
    (Name: 'Label';                 States: [sknsNormal, sknsDisabled]),
    (Name: 'ListItem';              States: [sknsNormal, sknsHotTrack]),
    (Name: 'ProgressBar';           States: [sknsNormal, sknsHotTrack]),
    (Name: 'RadioButton';           States: CSpTBXSkinAllStates),
    (Name: 'Tab';                   States: CSpTBXSkinAllStates),
    (Name: 'TrackBar';              States: [sknsNormal, sknsHotTrack]),
    (Name: 'TrackBarButton';        States: [sknsNormal, sknsPushed])
  );

  SSpTBXSkinStatesString: array [TSpTBXSkinStatesType] of string = ('Normal', 'Disabled', 'HotTrack', 'Pushed', 'Checked', 'CheckedAndHotTrack');
  SSpTBXSkinDisplayStatesString: array [TSpTBXSkinStatesType] of string = ('Normal', 'Disabled', 'Hot', 'Pushed', 'Checked', 'Checked && Hot');  

type
  { Text }

  TSpTextRotationAngle = (
    tra0,                      // No rotation
    tra90,                     // 90 degree rotation
    tra270                     // 270 degree rotation
  );

  TSpTBXTextInfo = record
    Text: WideString;
    TextAngle: TSpTextRotationAngle;
    TextFlags: Cardinal;
    TextSize: TSize;
    IsCaptionShown: Boolean;
    IsTextRotated: Boolean;
  end;

  TSpGlyphLayout = (
    ghlGlyphLeft,                 // Glyph icon on the left of the caption
    ghlGlyphTop                   // Glyph icon on the top of the caption
  );

  TSpGlowDirection = (
    gldNone,                      // No glow
    gldAll,                       // Glow on Left, Top, Right and Bottom of the text
    gldTopLeft,                   // Glow on Top-Left of the text
    gldBottomRight                // Glow on Bottom-Right of the text
  );

  { MenuItem }

  TSpTBXComboPart = (cpNone, cpCombo, cpSplitLeft, cpSplitRight);
  TSpTBXMenuItemMarginsInfo = record
    Margins: TRect;               // MenuItem margins
    GutterSize: Integer;          // Size of the gutter
    LeftCaptionMargin: Integer;   // Left margin of the caption
    RightCaptionMargin: Integer;  // Right margin of the caption
    ImageTextSpace: Integer;      // Space between the Icon and the caption
  end;

  TSpTBXMenuItemInfo = record
    Enabled: Boolean;
    HotTrack: Boolean;
    Pushed: Boolean;
    Checked: Boolean;
    HasArrow: Boolean;
    ImageShown: Boolean;
    ImageOrCheckShown: Boolean;
    ImageSize: TSize;
    RightImageSize: TSize;
    IsDesigning: Boolean;
    IsOnMenuBar: Boolean;
    IsOnToolbox: Boolean;
    IsOpen: Boolean;
    IsSplit: Boolean;
    IsSunkenCaption: Boolean;
    IsVertical: Boolean;
    MenuMargins: TSpTBXMenuItemMarginsInfo; // Used only on menu items
    ComboPart: TSpTBXComboPart;
    ComboRect: TRect;
    ComboState: TSpTBXSkinStatesType;
    ToolbarStyle: Boolean;
    State: TSpTBXSkinStatesType;
    SkinType: TSpTBXSkinType;
  end;

  { Colors }

  TSpTBXColorTextType = (cttDefault, cttHTML, cttIdentAndHTML);

  { TSpTBXSkinOptions }

  TSpTBXSkinOptionEntry = class(TPersistent)
  private
    FSkinType: Integer;
    FColor1, FColor2, FColor3, FColor4: TColor;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure Fill(ASkinType: Integer; AColor1, AColor2, AColor3, AColor4: TColor);
    procedure ReadFromString(S: string);
    function WriteToString: string;
    function IsEmpty: Boolean;
    procedure Reset;
  published
    property SkinType: Integer read FSkinType write FSkinType;
    property Color1: TColor read FColor1 write FColor1;
    property Color2: TColor read FColor2 write FColor2;
    property Color3: TColor read FColor3 write FColor3;
    property Color4: TColor read FColor4 write FColor4;
  end;

  TSpTBXSkinOptionCategory = class(TPersistent)
  private
    FBody: TSpTBXSkinOptionEntry;
    FBorders: TSpTBXSkinOptionEntry;
    FTextColor: TColor;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsEmpty: Boolean;
    procedure Reset;
    procedure LoadFromIni(MemIni: TMemIniFile; Section, Ident: string);
    procedure SaveToIni(MemIni: TMemIniFile; Section, Ident: string);
  published
    property Body: TSpTBXSkinOptionEntry read FBody write FBody;
    property Borders: TSpTBXSkinOptionEntry read FBorders write FBorders;
    property TextColor: TColor read FTextColor write FTextColor;
  end;

  TSpTBXSkinOptions = class(TPersistent)
  private
    FColorBtnFace: TColor;
    FOptions: array [TSpTBXSkinComponentsType, TSpTBXSkinStatesType] of TSpTBXSkinOptionCategory;
    FOfficeIcons: Boolean;
    FOfficeMenuSeparator: Boolean;
    FOfficeStatusBar: Boolean;
    FFloatingWindowBorderSize: Integer;
    FSkinName: string;
    function GetOfficeIcons: Boolean;
    function GetOfficeMenuSeparator: Boolean;
    function GetOfficePopup: Boolean;
    function GetOfficeStatusBar: Boolean;
    function GetFloatingWindowBorderSize: Integer;
    procedure SetFloatingWindowBorderSize(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure FillOptions; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CopyOptions(AComponent, ToComponent: TSpTBXSkinComponentsType);
    function Options(Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType): TSpTBXSkinOptionCategory; overload;
    function Options(Component: TSpTBXSkinComponentsType): TSpTBXSkinOptionCategory; overload;
    procedure LoadFromFile(Filename: WideString);
    procedure LoadFromStrings(L: TStrings); virtual;
    procedure SaveToFile(Filename: WideString);
    procedure SaveToStrings(L: TStrings); virtual;
    procedure SaveToMemIni(MemIni: TMemIniFile); virtual;
    procedure Reset;

    // Metrics
    procedure GetDropDownArrowSize(out DropDownArrowSize, DropDownArrowMargin, SplitBtnArrowSize: Integer); virtual;
    procedure GetMenuItemMargins(ACanvas: TCanvas; ImgSize: Integer; out MarginsInfo: TSpTBXMenuItemMarginsInfo); virtual;
    function GetState(Enabled, Pushed, HotTrack, Checked: Boolean): TSpTBXSkinStatesType;
    function GetTextColor(Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType; SkinType: TSpTBXSkinType = sknSkin): TColor; virtual;

    // Skin Paint
    procedure PaintBackground(ACanvas: TCanvas; ARect: TRect; Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType; Background, Borders: Boolean; Vertical: Boolean = False; ForceRectBorders: TAnchors = []); virtual;

    // Element Paint
    procedure PaintMenuCheckMark(ACanvas: TCanvas; ARect: TRect; Checked, Grayed, MenuItemStyle: Boolean; State: TSpTBXSkinStatesType); virtual;
    procedure PaintMenuRadioMark(ACanvas: TCanvas; ARect: TRect; Checked, MenuItemStyle: Boolean; State: TSpTBXSkinStatesType); virtual;
    procedure PaintWindowFrame(ACanvas: TCanvas; ARect: TRect; IsActive, DrawBody: Boolean; BorderSize: Integer = 4); virtual;

    property FloatingWindowBorderSize: Integer read GetFloatingWindowBorderSize write SetFloatingWindowBorderSize;
    property OfficeMenuSeparator: Boolean read GetOfficeMenuSeparator write FOfficeMenuSeparator;
    property OfficePopup: Boolean read GetOfficePopup;
    property OfficeStatusBar: Boolean read GetOfficeStatusBar write FOfficeStatusBar;
    property OfficeIcons: Boolean read GetOfficeIcons write FOfficeIcons;
    property ColorBtnFace: TColor read FColorBtnFace write FColorBtnFace;
    property SkinName: string read FSkinName write FSkinName;
  end;

  TSpTBXSkinOptionsClass = class of TSpTBXSkinOptions;

  { TSpTBXSkinsList }

  TSpTBXSkinsListEntry = class
  public
    SkinClass: TSpTBXSkinOptionsClass;
    SkinStrings: TStringList;
    destructor Destroy; override;
  end;

  TSpTBXSkinsList = class(TStringList)
  private
    function GetSkinOption(Index: Integer): TSpTBXSkinsListEntry;
  public
    procedure Delete(Index: Integer); override;
    destructor Destroy; override;
    function AddSkin(SkinName: string; SkinClass: TSpTBXSkinOptionsClass): Integer; overload;
    function AddSkin(SkinOptions: TStrings): Integer; overload;
    function AddSkinFromFile(Filename: WideString): Integer;
    procedure AddSkinsFromFolder(Folder: WideString);
    procedure GetSkinNames(SkinNames: TStrings);
    property SkinOptions[Index: Integer]: TSpTBXSkinsListEntry read GetSkinOption;
  end;

  { TSpTBXSkinManager }

  TSpTBXSkinManager = class
  private
    FCurrentSkin: TSpTBXSkinOptions;
    FNotifies: TList;
    FSkinsList: TSpTBXSkinsList;
    procedure Broadcast;
    function GetCurrentSkinName: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetSkinType: TSpTBXSkinType;
    function IsDefaultSkin: Boolean;
    function IsXPThemesEnabled: Boolean;

    procedure AddSkinNotification(AObject: TObject);
    procedure RemoveSkinNotification(AObject: TObject);
    procedure BroadcastSkinNotification;

    procedure LoadFromFile(Filename: WideString);
    procedure SaveToFile(Filename: WideString);

    procedure SetToDefaultSkin;
    procedure SetSkin(SkinName: string);
    procedure ChangeControlSkinType(Control: TWinControl; SkinType: TSpTBXSkinType; Recursive: Boolean = True);

    property CurrentSkin: TSpTBXSkinOptions read FCurrentSkin;
    property CurrentSkinName: string read GetCurrentSkinName;
    property SkinsList: TSpTBXSkinsList read FSkinsList;
  end;

  { TSpTBXSkinSwitcher }

  TSpTBXSkinSwitcher = class(TComponent)
  private
    FOnSkinChange: TNotifyEvent;
    function GetSkin: string;
    procedure SetSkin(const Value: string);
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Skin: string read GetSkin write SetSkin;
    property OnSkinChange: TNotifyEvent read FOnSkinChange write FOnSkinChange;
  end;

  { TSpPrintWindow }
  // Use SpPrintWindow instead of PaintTo as many controls will not render
  // properly (no text on editors, no scrollbars, incorrect borders, etc)
  // http://msdn2.microsoft.com/en-us/library/ms535695.aspx
  TSpPrintWindow = function(Hnd: HWND; HdcBlt: HDC; nFlags: UINT): BOOL; stdcall;

{ Themes }
function SkinManager: TSpTBXSkinManager;
function CurrentSkin: TSpTBXSkinOptions;
function SpTBXSkinType(T: TSpTBXSkinType): TSpTBXSkinType;
function SpGetLunaScheme: TSpTBXLunaScheme;
procedure SpDrawParentBackground(Control: TControl; DC: HDC; R: TRect);

{ WideString helpers }
function SpCreateRotatedFont(DC: HDC; Orientation: Integer = 2700): HFONT;
function SpDrawRotatedText(const DC: HDC; AText: WideString; var ARect: TRect; const AFormat: Cardinal; RotationAngle: TSpTextRotationAngle = tra270): Integer;
function SpCalcXPText(ACanvas: TCanvas; ARect: TRect; Caption: WideString; CaptionAlignment: TAlignment; Flags: Cardinal; GlyphSize, RightGlyphSize: TSize; Layout: TSpGlyphLayout; PushedCaption: Boolean; out ACaptionRect, AGlyphRect, ARightGlyphRect: TRect; RotationAngle: TSpTextRotationAngle = tra0): Integer;
function SpDrawXPText(ACanvas: TCanvas; Caption: WideString; var ARect: TRect; Flags: Cardinal; CaptionGlow: TSpGlowDirection = gldNone; CaptionGlowColor: TColor = clYellow; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
function SpDrawXPText(ACanvas: TCanvas; ARect: TRect; Caption: WideString; CaptionGlow: TSpGlowDirection; CaptionGlowColor: TColor; CaptionAlignment: TAlignment; Flags: Cardinal; GlyphSize: TSize; Layout: TSpGlyphLayout; PushedCaption: Boolean; out ACaptionRect, AGlyphRect: TRect; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
function SpDrawXPText(ACanvas: TCanvas; ARect: TRect; Caption: WideString; CaptionGlow: TSpGlowDirection; CaptionGlowColor: TColor; CaptionAlignment: TAlignment; Flags: Cardinal; IL: TCustomImageList; ImageIndex: Integer; Layout: TSpGlyphLayout; Enabled, PushedCaption, DisabledIconCorrection: Boolean; out ACaptionRect, AGlyphRect: TRect; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
function SpGetTextSize(DC: HDC; WS: WideString; NoPrefix: Boolean): TSize;
function SpGetControlTextHeight(AControl: TControl; AFont: TFont): Integer;
function SpGetControlTextSize(AControl: TControl; AFont: TFont; WS: WideString): TSize;
function SpSameText(W1, W2: WideString): Boolean;
function SpStripAccelChars(S: WideString): WideString;
function SpStripShortcut(S: WideString): WideString;
function SpStripTrailingPunctuation(S: WideString): WideString;
function SpRectToString(R: TRect): string;
function SpStringToRect(S: string; out R: TRect): Boolean;

{ Color helpers }
function SpColorToHTML(const Color: TColor): string;
function SpColorToString(const Color: TColor; TextType: TSpTBXColorTextType = cttDefault): string;
function SpStringToColor(S: string; out Color: TColor): Boolean;
procedure SpGetRGB(Color: TColor; out R, G, B: Integer);
function SpRGBToColor(R, G, B: Integer): TColor;
function SpLighten(Color: TColor; Amount: Integer): TColor;
function SpBlendColors(TargetColor, BaseColor: TColor; Percent: Integer): TColor;
function SpMixColors(TargetColor, BaseColor: TColor; Amount: Byte): TColor;

{ Painting helpers }
function SpCenterRect(Parent: TRect; ChildWidth, ChildHeight: Integer): TRect; overload;
function SpCenterRect(Parent, Child: TRect): TRect; overload;
function SpCenterRectHoriz(Parent: TRect; ChildWidth: Integer): TRect;
function SpCenterRectVert(Parent: TRect; ChildHeight: Integer): TRect;
procedure SpFillRect(ACanvas: TCanvas; const ARect: TRect; BrushColor: TColor; PenColor: TColor = clNone);
procedure SpDrawLine(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor);
procedure SpDrawRectangle(ACanvas: TCanvas; ARect: TRect; CornerSize: Integer; ColorTL, ColorBR: TColor; ColorTLInternal: TColor = clNone; ColorBRInternal: TColor = clNone; ForceRectBorders: TAnchors = []); overload;
procedure SpDrawRectangle(ACanvas: TCanvas; ARect: TRect; CornerSize: Integer; ColorL, ColorT, ColorR, ColorB, InternalColorL, InternalColorT, InternalColorR, InternalColorB: TColor; ForceRectBorders: TAnchors = []); overload;
procedure SpAlphaBlend(SrcDC, DstDC: HDC; SrcR, DstR: TRect; Alpha: Byte; SrcHasAlphaChannel: Boolean = False);
procedure SpPaintTo(WinControl: TWinControl; ACanvas: TCanvas; X, Y: Integer);

{ ImageList painting }
procedure SpDrawIconShadow(ACanvas: TCanvas; const ARect: TRect; ImageList: TCustomImageList; ImageIndex: Integer);
procedure SpDrawImageList(ACanvas: TCanvas; const ARect: TRect; ImageList: TCustomImageList; ImageIndex: Integer; Enabled, DisabledIconCorrection: Boolean);

{ Gradients }
procedure SpGradient(ACanvas: TCanvas; const ARect: TRect; StartPos, EndPos, ChunkSize: Integer; C1, C2: TColor; const Vertical: Boolean);
procedure SpGradientFill(ACanvas: TCanvas; const ARect: TRect; const C1, C2: TColor; const Vertical: Boolean);
procedure SpGradientFillMirror(ACanvas: TCanvas; const ARect: TRect; const C1, C2, C3, C4: TColor; const Vertical: Boolean);
procedure SpGradientFillMirrorTop(ACanvas: TCanvas; const ARect: TRect; const C1, C2, C3, C4: TColor; const Vertical: Boolean);
procedure SpGradientFillGlass(ACanvas: TCanvas; const ARect: TRect; const C1, C2, C3, C4: TColor; const Vertical: Boolean);

{ Element painting }
procedure SpDrawArrow(ACanvas: TCanvas; X, Y: Integer; AColor: TColor; Vertical, Reverse: Boolean; Size: Integer);
procedure SpDrawDropMark(ACanvas: TCanvas; DropMark: TRect);
procedure SpDrawFocusRect(ACanvas: TCanvas; const ARect: TRect);
procedure SpDrawGlyphPattern(DC: HDC; const R: TRect; Width, Height: Integer; const PatternBits; PatternColor: TColor); overload;
procedure SpDrawGlyphPattern(ACanvas: TCanvas; ARect: TRect; PatternIndex: Integer; PatternColor: TColor); overload;
procedure SpDrawXPButton(ACanvas: TCanvas; ARect: TRect; Enabled, Pushed, HotTrack, Checked, Focused, Defaulted: Boolean; SkinType: TSpTBXSkinType);
procedure SpDrawXPCheckBoxGlyph(ACanvas: TCanvas; ARect: TRect; Enabled: Boolean; State: TCheckBoxState; HotTrack, Pushed: Boolean; SkinType: TSpTBXSkinType);
procedure SpDrawXPRadioButtonGlyph(ACanvas: TCanvas; ARect: TRect; Enabled, Checked, HotTrack, Pushed: Boolean; SkinType: TSpTBXSkinType);
procedure SpDrawXPEditFrame(ACanvas: TCanvas; ARect: TRect; Enabled, HotTrack: Boolean; SkinType: TSpTBXSkinType; ClipContent: Boolean = False; AutoAdjust: Boolean = False); overload;
procedure SpDrawXPEditFrame(AWinControl: TWinControl; HotTracking: Boolean; SkinType: TSpTBXSkinType; AutoAdjust: Boolean = False); overload;
procedure SpDrawXPGrip(ACanvas: TCanvas; ARect: TRect; LoC, HiC: TColor);
procedure SpDrawXPHeader(ACanvas: TCanvas; ARect: TRect; IsFirstItem, HotTrack, Pushed: Boolean; SkinType: TSpTBXSkinType);
procedure SpDrawXPListItemBackground(ACanvas: TCanvas; ARect: TRect; HotTrack, Pushed, Focused: Boolean; SkinType: TSpTBXSkinType);

{ Skins painting }
procedure SpPaintSkinBackground(ACanvas: TCanvas; ARect: TRect; SkinOption: TSpTBXSkinOptionCategory; Vertical: Boolean);
procedure SpPaintSkinBorders(ACanvas: TCanvas; ARect: TRect; SkinOption: TSpTBXSkinOptionCategory; ForceRectBorders: TAnchors = []);

{ Misc }
function SpIsWinVista: Boolean;
function SpGetDirectories(Path: WideString; L: TStringList): Boolean;

{ Stock Objects }
var
  StockBitmap: TBitmap;
  SpPrintWindow: TSpPrintWindow = nil;

implementation

uses
  UxTheme, Themes, Forms, Math, TypInfo, SpTBXDefaultSkins;

var
  FInternalSkinManager: TSpTBXSkinManager = nil;

const
  ROP_DSPDxax = $00E20746;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Skin Notification }

function SkinManager: TSpTBXSkinManager;
begin
  if not Assigned(FInternalSkinManager) then
    FInternalSkinManager := TSpTBXSkinManager.Create;
  Result := FInternalSkinManager;
end;

function CurrentSkin: TSpTBXSkinOptions;
begin
  Result := SkinManager.CurrentSkin;
end;

function SpTBXSkinType(T: TSpTBXSkinType): TSpTBXSkinType;
begin
  Result := T;
  if (Result = sknSkin) and SkinManager.IsDefaultSkin then
    Result := sknWindows;
  if (Result = sknWindows) and not SkinManager.IsXPThemesEnabled then
    Result := sknNone;
end;

function SpGetLunaScheme: TSpTBXLunaScheme;
const
  MaxChars = 1024;
var
  pszThemeFileName, pszColorBuff, pszSizeBuf: PWideChar;
  S: string;
begin
  Result := lusUnknown;

  if SkinManager.IsXPThemesEnabled then begin
    GetMem(pszThemeFileName, 2 * MaxChars);
    GetMem(pszColorBuff,     2 * MaxChars);
    GetMem(pszSizeBuf,       2 * MaxChars);
    try
      if not Failed(GetCurrentThemeName(pszThemeFileName, MaxChars, pszColorBuff, MaxChars, pszSizeBuf, MaxChars)) then
        if UpperCase(ExtractFileName(pszThemeFileName)) = 'LUNA.MSSTYLES' then begin
          S := UpperCase(pszColorBuff);
          if S = 'NORMALCOLOR' then
            Result := lusBlue
          else if S = 'METALLIC' then
            Result := lusMetallic
          else if S = 'HOMESTEAD' then
            Result := lusGreen;
        end;
    finally
      FreeMem(pszSizeBuf);
      FreeMem(pszColorBuff);
      FreeMem(pszThemeFileName);
    end;
  end;
end;

procedure SpDrawParentBackground(Control: TControl; DC: HDC; R: TRect);
// Delphi 2007 and Vista compatible
var
  Parent: TWinControl;
  P: TPoint;
  Brush: HBRUSH;
begin
  Parent := Control.Parent;
  if Parent = nil then begin
    Brush := CreateSolidBrush(ColorToRGB(clBtnFace));
    Windows.FillRect(DC, R, Brush);
  end
  else if Parent.HandleAllocated then begin
    if not Parent.DoubleBuffered and (Control is TWinControl) and SkinManager.IsXPThemesEnabled then
      UxTheme.DrawThemeParentBackground(TWinControl(Control).Handle, DC, @R)
    else begin
      // Same as Controls.PerformEraseBackground
      GetWindowOrgEx(DC, P);
      SetWindowOrgEx(DC, P.X + Control.Left, P.Y + Control.Top, nil);
      Parent.Perform(WM_ERASEBKGND, Integer(DC), Integer(DC));
      SetWindowOrgEx(DC, P.X, P.Y, nil);
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ WideString helpers}

function EnumFontsProc(const lplf: TLogFont; const lptm: TTextMetric;
  dwType: DWORD; lpData: LPARAM): Integer; stdcall;
begin
  Boolean(Pointer(lpData)^) := True;
  Result := 0;
end;

function SpCreateRotatedFont(DC: HDC; Orientation: Integer = 2700): HFONT;
var
  LogFont: TLogFont;
  TM: TTextMetric;
  VerticalFontName: array[0..LF_FACESIZE-1] of Char;
  VerticalFontExists: Boolean;
begin
  if GetObject(GetCurrentObject(DC, OBJ_FONT), SizeOf(LogFont),
     @LogFont) = 0 then begin
    { just in case... }
    Result := 0;
    Exit;
  end;
  LogFont.lfEscapement := Orientation;
  LogFont.lfOrientation := Orientation;
  LogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;  { needed for Win9x }

  { Don't let a random TrueType font be substituted when MS Sans Serif or
    Microsoft Sans Serif are used. On Windows 2000 and later, hard-code Tahoma
    because Arial can't display Japanese or Thai Unicode characters (on Windows
    2000 at least). On earlier versions, hard-code Arial since NT 4.0 doesn't
    ship with Tahoma, and 9x doesn't do Unicode. }
  if (StrIComp(LogFont.lfFaceName, 'MS Sans Serif') = 0) or
     (StrIComp(LogFont.lfFaceName, 'Microsoft Sans Serif') = 0) then begin
    if Win32MajorVersion >= 5 then
      StrPCopy(LogFont.lfFaceName, 'Tahoma')
    else
      StrPCopy(LogFont.lfFaceName, 'Arial');
    { Set lfHeight to the actual height of the current font. This is needed
      to work around a Windows 98 issue: on a clean install of the OS,
      SPI_GETNONCLIENTMETRICS returns -5 for lfSmCaptionFont.lfHeight. This is
      wrong; it should return -11 for an 8 pt font. With normal, unrotated text
      this actually displays correctly, since MS Sans Serif doesn't support
      sizes below 8 pt. However, when we change to a TrueType font like Arial,
      this becomes a problem because it'll actually create a font that small. }
    if GetTextMetrics(DC, TM) then begin
      { If the original height was negative, keep it negative }
      if LogFont.lfHeight <= 0 then
        LogFont.lfHeight := -(TM.tmHeight - TM.tmInternalLeading)
      else
        LogFont.lfHeight := TM.tmHeight;
    end;
  end;

  { Use a vertical font if available so that Asian characters aren't drawn
    sideways }
  if StrLen(LogFont.lfFaceName) < SizeOf(VerticalFontName)-1 then begin
    VerticalFontName[0] := '@';
    StrCopy(@VerticalFontName[1], LogFont.lfFaceName);
    VerticalFontExists := False;
    EnumFonts(DC, VerticalFontName, @EnumFontsProc, @VerticalFontExists);
    if VerticalFontExists then
      StrCopy(LogFont.lfFaceName, VerticalFontName);
  end;

  Result := CreateFontIndirect(LogFont);
end;

function SpDrawRotatedText(const DC: HDC; AText: WideString; var ARect: TRect; const AFormat: Cardinal; RotationAngle: TSpTextRotationAngle = tra270): Integer;
{ The format flag this function respects are
  DT_CALCRECT, DT_NOPREFIX, DT_HIDEPREFIX, DT_CENTER, DT_END_ELLIPSIS, DT_NOCLIP }
var
  RotatedFont, SaveFont: HFONT;
  TextMetrics: TTextMetric;
  X, Y, P, I, SU, FU, W: Integer;
  SaveAlign: UINT;
  Clip: Boolean;
  Pen, SavePen: HPEN;
  Sz: TSize;
  Orientation: Integer;
begin
  Result := 0;
  if Length(AText) = 0 then Exit;

  Orientation := 0;
  case RotationAngle of
    tra90: Orientation := 900;   // 90 degrees
    tra270: Orientation := 2700; // 270 degrees
  end;
  RotatedFont := SpCreateRotatedFont(DC, Orientation);
  SaveFont := SelectObject(DC, RotatedFont);

  GetTextMetrics(DC, TextMetrics);
  X := ARect.Left + ((ARect.Right - ARect.Left) - TextMetrics.tmHeight) div 2;

  Clip := AFormat and DT_NOCLIP = 0;

  { Find the index of the character that should be underlined. Delete '&'
    characters from the string. Like DrawText, only the last prefixed character
    will be underlined. }
  P := 0;
  I := 1;
  if AFormat and DT_NOPREFIX = 0 then
    while I <= Length(AText) do
    begin
      if AText[I] = '&' then
      begin
        Delete(AText, I, 1);
        if PWideChar(AText)[I - 1] <> '&' then P := I;
      end;
      Inc(I);
    end;

  if AFormat and DT_END_ELLIPSIS <> 0 then
  begin
    if (Length(AText) > 1) and (SpGetTextSize(DC, AText, False).cx > ARect.Bottom - ARect.Top) then
    begin
      W := ARect.Bottom - ARect.Top;
      if W > 2 then
      begin
        Delete(AText, Length(AText), 1);
        while (Length(AText) > 1) and (SpGetTextSize(DC, AText + '...', False).cx > W) do
          Delete(AText, Length(AText), 1);
      end
      else AText := AText[1];
      if P > Length(AText) then P := 0;
      AText := AText + '...';
    end;
  end;

  Sz := SpGetTextSize(DC, AText, False);
  Result := Sz.cy;

  if AFormat and DT_CALCRECT <> 0 then begin
    ARect.Right := ARect.Left + Sz.cy;
    ARect.Bottom := ARect.Top + Sz.cx;
  end
  else begin
    if AFormat and DT_CENTER <> 0 then
      Y := ARect.Top + ((ARect.Bottom - ARect.Top) - Sz.cx) div 2
    else
      Y := ARect.Top;

    if Clip then
    begin
      SaveDC(DC);
      with ARect do IntersectClipRect(DC, Left, Top, Right, Bottom);
    end;

    case RotationAngle of
      tra90: SaveAlign := SetTextAlign(DC, TA_RIGHT);
      tra270: SaveAlign := SetTextAlign(DC, TA_BOTTOM);
    else
      SaveAlign := SetTextAlign(DC, TA_LEFT);
    end;

    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
      Windows.TextOutA(DC, X, Y, PAnsiChar(AnsiString(AText)), Length(AnsiString(AText)))
    else
      Windows.TextOutW(DC, X, Y, PWideChar(AText), Length(AText));

    SetTextAlign(DC, SaveAlign);

    { Underline }
    if (P > 0) and (AFormat and DT_HIDEPREFIX = 0) then
    begin
      SU := SpGetTextSize(DC, Copy(AText, 1, P - 1), False).cx;
      FU := SU + SpGetTextSize(DC, PWideChar(AText)[P - 1], False).cx;
      Inc(X, TextMetrics.tmDescent - 2);
      Pen := CreatePen(PS_SOLID, 1, GetTextColor(DC));
      SavePen := SelectObject(DC, Pen);
      MoveToEx(DC, X, Y + SU, nil);
      LineTo(DC, X, Y + FU);
      SelectObject(DC, SavePen);
      DeleteObject(Pen);
    end;

    if Clip then RestoreDC(DC, -1);
  end;

  SelectObject(DC, SaveFont);
  DeleteObject(RotatedFont);
end;

function SpCalcXPText(ACanvas: TCanvas; ARect: TRect; Caption: WideString;
  CaptionAlignment: TAlignment; Flags: Cardinal; GlyphSize, RightGlyphSize: TSize;
  Layout: TSpGlyphLayout; PushedCaption: Boolean; out ACaptionRect, AGlyphRect, ARightGlyphRect: TRect;
  RotationAngle: TSpTextRotationAngle = tra0): Integer;
var
  R: TRect;
  TextOffset, Spacing, RightSpacing: TPoint;
  CaptionSz: TSize;
begin
  Result := 0;
  ACaptionRect := Rect(0, 0, 0, 0);
  AGlyphRect := Rect(0, 0, 0, 0);
  ARightGlyphRect := Rect(0, 0, 0, 0);
  TextOffset := Point(0, 0);
  Spacing := Point(0, 0);
  RightSpacing := Point(0, 0);
  if (Caption <> '') and (GlyphSize.cx > 0) and (GlyphSize.cy > 0) then
    Spacing := Point(4, 1);
  if (Caption <> '') and (RightGlyphSize.cx > 0) and (RightGlyphSize.cy > 0) then
    RightSpacing := Point(4, 1);

  Flags := Flags and not DT_CENTER;
  Flags := Flags and not DT_VCENTER;
  if CaptionAlignment = taRightJustify then
    Flags := Flags or DT_RIGHT;

  // DT_END_ELLIPSIS and DT_PATH_ELLIPSIS doesn't work with rotated text
  // http://support.microsoft.com/kb/249678
  // Revert the ARect if the text is rotated, from now on work on horizontal text !!!
  if RotationAngle <> tra0 then
    ARect := Rect(ARect.Top, ARect.Left, ARect.Bottom, ARect.Right);

  // Get the caption size
  if ((Flags and DT_WORDBREAK) <> 0) or ((Flags and DT_END_ELLIPSIS) <> 0) or ((Flags and DT_PATH_ELLIPSIS) <> 0) then begin
    if Layout = ghlGlyphLeft then  // Glyph on left or right side
      R := Rect(0, 0, ARect.Right - ARect.Left - GlyphSize.cx - Spacing.X - RightGlyphSize.cx - RightSpacing.X + 2, 1)
    else  // Glyph on top
      R := Rect(0, 0, ARect.Right - ARect.Left + 2, 1);
  end
  else
    R := Rect(0, 0, 1, 1);

  if (fsBold in ACanvas.Font.Style) and (RotationAngle = tra0) and (((Flags and DT_END_ELLIPSIS) <> 0) or ((Flags and DT_PATH_ELLIPSIS) <> 0)) then begin
    // [Bugfix] Windows bug:
    // When the Font is Bold and DT_END_ELLIPSIS or DT_PATH_ELLIPSIS is used
    // DrawTextW returns an incorrect size if the string is unicode.
    // The R.Right is reduced by 3 which cuts down the string and
    // adds the ellipsis.
    // We have to obtain the real size and check if it fits in the Rect.
    CaptionSz := SpGetTextSize(ACanvas.Handle, Caption, True);
    if CaptionSz.cx <= R.Right then begin
      R := Rect(0, 0, CaptionSz.cx, CaptionSz.cy);
      Result := CaptionSz.cy;
    end;
  end;

  if Result <= 0 then begin
    Result := SpDrawXPText(ACanvas, Caption, R, Flags or DT_CALCRECT, gldNone, clYellow);
    CaptionSz.cx := R.Right;
    CaptionSz.cy := R.Bottom;
  end;

  // ACaptionRect
  if Result > 0 then begin
    R.Top := (ARect.Top + ARect.Bottom - CaptionSz.cy) div 2; // Vertically centered
    R.Bottom := R.Top + CaptionSz.cy;
    case CaptionAlignment of
      taCenter:
        R.Left := (ARect.Right + ARect.Left - CaptionSz.cx) div 2; // Horizontally centered
      taLeftJustify:
        R.Left := ARect.Left;
      taRightJustify:
        R.Left := ARect.Right - CaptionSz.cx;
    end;
    R.Right := R.Left + CaptionSz.cx;

    // Since DT_END_ELLIPSIS and DT_PATH_ELLIPSIS doesn't work with rotated text
    // try to fix it by padding the text 8 pixels to the right
    if (RotationAngle <> tra0) and (R.Right + 8 < ARect.Right) then
      if ((Flags and DT_END_ELLIPSIS) <> 0) or ((Flags and DT_PATH_ELLIPSIS) <> 0) then
        R.Right := R.Right + 8;

    if PushedCaption then
      OffsetRect(R, 1, 1);

    ACaptionRect := R;
  end;

  // AGlyphRect
  if (GlyphSize.cx > 0) and (GlyphSize.cy > 0) then begin
    R := ARect;

    // If ghlGlyphTop is used the glyph should be centered
    if Layout = ghlGlyphTop then
      CaptionAlignment := taCenter;

    case CaptionAlignment of
      taCenter:
        begin
          // Total width = Icon + Space + Text
          if Layout = ghlGlyphLeft then begin
            AGlyphRect.Left := (R.Right + R.Left - (GlyphSize.cx + Spacing.X + CaptionSz.cx)) div 2;
            TextOffset.X := (GlyphSize.cx + Spacing.X) div 2;
          end
          else
            AGlyphRect.Left := (R.Right + R.Left - GlyphSize.cx) div 2;
        end;
      taLeftJustify:
        begin
          AGlyphRect.Left := R.Left;
          TextOffset.X := GlyphSize.cx + Spacing.X;
        end;
      taRightJustify:
        begin
          AGlyphRect.Left := R.Right - GlyphSize.cx;
          TextOffset.X := - Spacing.X - GlyphSize.cx;
        end;
    end;

    if Layout = ghlGlyphLeft then
      AGlyphRect.Top := (R.Top + R.Bottom - GlyphSize.cy) div 2
    else begin
      AGlyphRect.Top := (R.Top + R.Bottom - (GlyphSize.cy + Spacing.Y + CaptionSz.cy)) div 2;
      Inc(TextOffset.Y, (GlyphSize.cy + Spacing.Y) div 2);
    end;

    AGlyphRect.Right := AGlyphRect.Left + GlyphSize.cx;
    AGlyphRect.Bottom := AGlyphRect.Top + GlyphSize.cy;

    if PushedCaption then
      OffsetRect(AGlyphRect, 1, 1);
  end;

  // Move the text according to the icon position
  if Result > 0 then
    OffsetRect(ACaptionRect, TextOffset.X, TextOffset.Y);

  // ARightGlyphRect, it's valid only when using taLeftJustify
  if (RightGlyphSize.cx > 0) and (RightGlyphSize.cy > 0) then
    if CaptionAlignment = taLeftJustify then begin
      R := ARect;
      ARightGlyphRect.Left := R.Right - RightGlyphSize.cx;
      ARightGlyphRect.Right := ARightGlyphRect.Left + RightGlyphSize.cx;
      ARightGlyphRect.Top := (R.Top + R.Bottom - RightGlyphSize.cy) div 2;
      ARightGlyphRect.Bottom := ARightGlyphRect.Top + RightGlyphSize.cy;
      if (Result > 0) and (ACaptionRect.Right > ARightGlyphRect.Left - RightSpacing.X) then
        ACaptionRect.Right := ARightGlyphRect.Left - RightSpacing.X;
    end;

  // Revert back, normalize when the text is rotated
  if RotationAngle <> tra0 then begin
    ACaptionRect := Rect(ACaptionRect.Top, ACaptionRect.Left, ACaptionRect.Bottom, ACaptionRect.Right);
    AGlyphRect := Rect(AGlyphRect.Top, AGlyphRect.Left, AGlyphRect.Bottom, AGlyphRect.Right);
    ARightGlyphRect := Rect(ARightGlyphRect.Top, ARightGlyphRect.Left, ARightGlyphRect.Bottom, ARightGlyphRect.Right);
  end;
end;

function SpDrawXPText(ACanvas: TCanvas; Caption: WideString; var ARect: TRect;
  Flags: Cardinal; CaptionGlow: TSpGlowDirection = gldNone;
  CaptionGlowColor: TColor = clYellow; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;

  function InternalDraw(var R: TRect): Integer;
  begin
    Result := 0;
    case RotationAngle of
      tra0:
        if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
          Result := Windows.DrawTextA(ACanvas.Handle, PAnsiChar(AnsiString(Caption)), -1, R, Flags)
        else
          Result := Windows.DrawTextW(ACanvas.Handle, PWideChar(Caption), -1, R, Flags);
      tra90, tra270:
        Result := SpDrawRotatedText(ACanvas.Handle, Caption, R, Flags, RotationAngle);
    end;
  end;

var
  BS: TBrushStyle;
  GlowR: TRect;
  C, FC: TColor;
begin
  BS := ACanvas.Brush.Style;
  C := ACanvas.Brush.Color;
  try
    ACanvas.Brush.Style := bsClear;

    if (Flags and DT_CALCRECT = 0) and (CaptionGlow <> gldNone) then begin
      FC := ACanvas.Font.Color;
      ACanvas.Font.Color := CaptionGlowColor;
      case CaptionGlow of
        gldAll:
          begin
            GlowR := ARect; OffsetRect(GlowR, 0, -1);
            InternalDraw(GlowR);
            GlowR := ARect; OffsetRect(GlowR, 0, 1);
            InternalDraw(GlowR);
            GlowR := ARect; OffsetRect(GlowR, -1, 0);
            InternalDraw(GlowR);
            GlowR := ARect; OffsetRect(GlowR, 1, 0);
          end;
        gldTopLeft:
          begin
            GlowR := ARect; OffsetRect(GlowR, -1, -1);
            InternalDraw(GlowR);
          end;
        gldBottomRight:
          begin
            GlowR := ARect; OffsetRect(GlowR, 1, 1);
            InternalDraw(GlowR);
          end;
      end;
      ACanvas.Font.Color := FC;
    end;

    Result := InternalDraw(ARect);

    if IsRectEmpty(ARect) then
      Result := 0
    else
      if Flags and DT_CALCRECT <> 0 then begin
        // [Bugfix] Windows bug:
        // When DT_CALCRECT is used and the font is italic the
        // resulting rect is incorrect
        if fsItalic in ACanvas.Font.Style then
          ARect.Right := ARect.Right + 1 + (ACanvas.Font.Size div 8) * 2;
      end;

  finally
    ACanvas.Brush.Style := BS;
    ACanvas.Brush.Color := C;
  end;
end;

function SpDrawXPText(ACanvas: TCanvas; ARect: TRect; Caption: WideString;
  CaptionGlow: TSpGlowDirection; CaptionGlowColor: TColor; CaptionAlignment: TAlignment;
  Flags: Cardinal; GlyphSize: TSize; Layout: TSpGlyphLayout; PushedCaption: Boolean;
  out ACaptionRect, AGlyphRect: TRect;
  RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
var
  DummyRightGlyphSize: TSize;
  DummyRightGlyphRect: TRect;
begin
  DummyRightGlyphSize.cx := 0;
  DummyRightGlyphSize.cy := 0;
  DummyRightGlyphRect := Rect(0, 0, 0, 0);
  Result := SpCalcXPText(ACanvas, ARect, Caption, CaptionAlignment, Flags, GlyphSize, DummyRightGlyphSize,
    Layout, PushedCaption, ACaptionRect, AGlyphRect, DummyRightGlyphRect, RotationAngle);
  SpDrawXPText(ACanvas, Caption, ACaptionRect, Flags and not DT_CALCRECT, CaptionGlow, CaptionGlowColor, RotationAngle);
end;

function SpDrawXPText(ACanvas: TCanvas; ARect: TRect; Caption: WideString;
  CaptionGlow: TSpGlowDirection; CaptionGlowColor: TColor; CaptionAlignment: TAlignment;
  Flags: Cardinal; IL: TCustomImageList; ImageIndex: Integer; Layout: TSpGlyphLayout;
  Enabled, PushedCaption, DisabledIconCorrection: Boolean; out ACaptionRect, AGlyphRect: TRect;
  RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
var
  GlyphSize, DummyRightGlyphSize: TSize;
  DummyRightGlyphRect: TRect;
begin
  GlyphSize.cx := 0;
  GlyphSize.cy := 0;
  DummyRightGlyphSize.cx := 0;
  DummyRightGlyphSize.cy := 0;
  DummyRightGlyphRect := Rect(0, 0, 0, 0);

  if Assigned(IL) and (ImageIndex > -1) and (ImageIndex < IL.Count) then begin
    GlyphSize.cx := IL.Width;
    GlyphSize.cy := IL.Height;
  end;

  Result := SpCalcXPText(ACanvas, ARect, Caption, CaptionAlignment, Flags, GlyphSize, DummyRightGlyphSize,
    Layout, PushedCaption, ACaptionRect, AGlyphRect, DummyRightGlyphRect, RotationAngle);

  SpDrawXPText(ACanvas, Caption, ACaptionRect, Flags and not DT_CALCRECT, CaptionGlow, CaptionGlowColor, RotationAngle);

  if Assigned(IL) and (ImageIndex > -1) and (ImageIndex < IL.Count) then
    SpDrawImageList(ACanvas, AGlyphRect, IL, ImageIndex, Enabled, DisabledIconCorrection);
end;

function SpGetTextSize(DC: HDC; WS: WideString; NoPrefix: Boolean): TSize;
// Returns the size of the string, if NoPrefix is True, it first removes "&"
// characters as necessary.
// This procedure is 10x faster than using DrawText with the DT_CALCRECT flag
begin
  Result.cx := 0;
  Result.cy := 0;
  if NoPrefix then
    WS := SpStripAccelChars(WS);
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    Windows.GetTextExtentPoint32A(DC, PAnsiChar(AnsiString(WS)), Length(AnsiString(WS)), Result)
  else
    Windows.GetTextExtentPoint32W(DC, PWideChar(WS), Length(WS), Result);
end;

function SpGetControlTextHeight(AControl: TControl; AFont: TFont): Integer;
// Returns the control text height based on the font
var
  Sz: TSize;
begin
  Sz := SpGetControlTextSize(AControl, AFont, 'WQqJ');
  Result := Sz.cy;
end;

function SpGetControlTextSize(AControl: TControl; AFont: TFont; WS: WideString): TSize;
// Returns the control text size based on the font
var
  ACanvas: TControlCanvas;
begin
  ACanvas := TControlCanvas.Create;
  try
    ACanvas.Control := AControl;
    ACanvas.Font.Assign(AFont);
    Result := SpGetTextSize(ACanvas.Handle, WS, False);
  finally
    ACanvas.Free;
  end;
end;

function SpSameText(W1, W2: WideString): Boolean;
begin
  {$IFNDEF UNICODE}
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    Result := AnsiSameText(AnsiString(W1), AnsiString(W2))
  else
    Result := WideSameText(W1, W2);
  {$ELSE}
  Result := WideSameText(W1, W2);
  {$ENDIF}
end;

function SpStripAccelChars(S: WideString): WideString;
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do begin
    if Result[I] = '&' then
      System.Delete(Result, I, 1);
    Inc(I);
  end;
end;

function SpStripShortcut(S: WideString): WideString;
var
  P: Integer;
begin
  Result := S;
  P := Pos(#9, Result);
  if P <> 0 then
    SetLength(Result, P - 1);
end;

function SpStripTrailingPunctuation(S: WideString): WideString;
// Removes any colon (':') or ellipsis ('...') from the end of S and returns
// the resulting string
var
  L: Integer;
begin
  Result := S;
  L := Length(Result);
  if (L > 1) and (Result[L] = ':') then
    SetLength(Result, L-1)
  else if (L > 3) and (Result[L-2] = '.') and (Result[L-1] = '.') and
     (Result[L] = '.') then
    SetLength(Result, L-3);
end;

function SpRectToString(R: TRect): string;
begin
  Result := Format('%d, %d, %d, %d', [R.Left, R.Top, R.Right, R.Bottom]);
end;

function SpStringToRect(S: string; out R: TRect): Boolean;
var
  L: TStringList;
begin
  Result := False;
  R := Rect(0, 0, 0, 0);
  L := TStringList.Create;
  try
    L.CommaText := S;
    if L.Count = 4 then begin
      R.Left := StrToIntDef(L[0], 0);
      R.Top := StrToIntDef(L[1], 0);
      R.Right := StrToIntDef(L[2], 0);
      R.Bottom := StrToIntDef(L[3], 0);
      Result := True;
    end;
  finally
    L.Free;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Color Helpers }

function SpColorToHTML(const Color: TColor): string;
var
  R: TColorRef;
begin
  R := ColorToRGB(Color);
  Result := Format('#%.2x%.2x%.2x', [GetRValue(R), GetGValue(R), GetBValue(R)]);
end;

function SpColorToString(const Color: TColor; TextType: TSpTBXColorTextType = cttDefault): string;
begin
  case TextType of
    cttDefault:
      Result := ColorToString(Color);
    cttHTML:
      Result := SpColorToHTML(Color);
    cttIdentAndHTML:
      begin
        Result := ColorToString(Color);
        if (Length(Result) > 0) and (Result[1] = '$') then
          Result := SpColorToHTML(Color);
      end;
  end;
end;

function SpStringToColor(S: string; out Color: TColor): Boolean;
var
  E, L: Integer;
begin
  Result := False;
  Color := clDefault;
  L := Length(S);
  if L < 2 then Exit;

  if (S[1] = '#') and (L = 7) then begin
    Delete(S, 1, 1); // strip the # char
    S := Format('$00%s%s%s', [Copy(S, 5, 2), Copy(S, 3, 2), Copy(S, 1, 2)]);
    Color := StringToColor(S);
    Result := True;
  end
  else begin
    Result := IdentToColor(S, Longint(Color));
    if not Result and (L > 6) and (L < 10) and (S[1] = '$') then begin
      Val(S, Color, E);
      Result := E = 0;
    end;
  end;
end;

procedure SpGetRGB(Color: TColor; out R, G, B: Integer);
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
end;

function SpRGBToColor(R, G, B: Integer): TColor;
begin
  if R < 0 then R := 0 else if R > 255 then R := 255;
  if G < 0 then G := 0 else if G > 255 then G := 255;
  if B < 0 then B := 0 else if B > 255 then B := 255;
  Result := TColor(RGB(R, G, B));
end;

function SpLighten(Color: TColor; Amount: Integer): TColor;
var
  R, G, B: Integer;
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color) + Amount;
  G := GetGValue(Color) + Amount;
  B := GetBValue(Color) + Amount;
  Result := SpRGBToColor(R, G, B);
end;

function SpBlendColors(TargetColor, BaseColor: TColor; Percent: Integer): TColor;
// Blend 2 colors with a predefined percent (0..100 or 0..1000)
// If Percent is 0 the result will be BaseColor,
// If Percent is 100 the result will be TargetColor.
// Any other value will return a color between base and target.
// For example if you want to add 70% of yellow ($0000FFFF) to a color:
// NewColor := SpBlendColor($0000FFFF, BaseColor, 70);
// The result will have 70% of yellow and 30% of BaseColor
var
  Percent2, D, F: Integer;
  R, G, B, R2, G2, B2: Integer;
begin
  SpGetRGB(TargetColor, R, G, B);
  SpGetRGB(BaseColor, R2, G2, B2);

  if Percent >= 100 then D := 1000
  else D := 100;
  Percent2 := D - Percent;
  F := D div 2;

  R := (R * Percent + R2 * Percent2 + F) div D;
  G := (G * Percent + G2 * Percent2 + F) div D;
  B := (B * Percent + B2 * Percent2 + F) div D;

  Result := SpRGBToColor(R, G, B);
end;

function SpMixColors(TargetColor, BaseColor: TColor; Amount: Byte): TColor;
// Mix 2 colors with a predefined amount (0..255).
// If Amount is 0 the result will be BaseColor,
// If Amount is 255 the result will be TargetColor.
// Any other value will return a color between base and target.
// For example if you want to add 50% of yellow ($0000FFFF) to a color:
// NewColor := SpMixColors($0000FFFF, BaseColor, 128);
// The result will be BaseColor + 50% of yellow
var
  R1, G1, B1: Integer;
  R2, G2, B2: Integer;
begin
  SpGetRGB(BaseColor, R1, G1, B1);
  SpGetRGB(TargetColor, R2, G2, B2);

  R1 := (R2 - R1) * Amount div 255 + R1;
  G1 := (G2 - G1) * Amount div 255 + G1;
  B1 := (B2 - B1) * Amount div 255 + B1;

  Result := SpRGBToColor(R1, G1, B1);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Painting Helpers }

function SpCenterRect(Parent: TRect; ChildWidth, ChildHeight: Integer): TRect;
begin
  Result.Left := (Parent.Left + Parent.Right - ChildWidth) div 2;
  Result.Right := Result.Left + ChildWidth;
  Result.Top := (Parent.Top + Parent.Bottom - ChildHeight) div 2;
  Result.Bottom := Result.Top + ChildHeight;
end;

function SpCenterRect(Parent, Child: TRect): TRect;
begin
  Result := SpCenterRect(Parent, Child.Right - Child.Left, Child.Bottom - Child.Top);
end;

function SpCenterRectHoriz(Parent: TRect; ChildWidth: Integer): TRect;
begin
  Result.Left := (Parent.Left + Parent.Right - ChildWidth) div 2;
  Result.Right := Result.Left + ChildWidth;
  Result.Top := Parent.Top;
  Result.Bottom := Parent.Bottom;
end;

function SpCenterRectVert(Parent: TRect; ChildHeight: Integer): TRect;
begin
  Result.Left := Parent.Left;
  Result.Right := Parent.Right;
  Result.Top := (Parent.Top + Parent.Bottom - ChildHeight) div 2;
  Result.Bottom := Result.Top + ChildHeight;
end;

procedure SpFillRect(ACanvas: TCanvas; const ARect: TRect; BrushColor: TColor; PenColor: TColor = clNone);
var
  C, C2: TColor;
begin
  if BrushColor <> clNone then begin
    C := ACanvas.Brush.Color;
    C2 := ACanvas.Pen.Color;
    ACanvas.Brush.Color := BrushColor;
    ACanvas.Pen.Color := PenColor;
    if PenColor = clNone then
      ACanvas.FillRect(ARect)
    else
      ACanvas.Rectangle(ARect);
    ACanvas.Brush.Color := C;
    ACanvas.Pen.Color := C2;
  end;
end;

procedure SpDrawLine(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor);
var
  C: TColor;
begin
  if Color <> clNone then begin
    C := ACanvas.Pen.Color;
    ACanvas.Pen.Color := Color;
    ACanvas.MoveTo(X1, Y1);
    ACanvas.LineTo(X2, Y2);
    ACanvas.Pen.Color := C;
  end;
end;

procedure SpDrawRectangle(ACanvas: TCanvas; ARect: TRect; CornerSize: Integer;
  ColorL, ColorT, ColorR, ColorB,
  InternalColorL, InternalColorT, InternalColorR, InternalColorB: TColor;
  ForceRectBorders: TAnchors = []);
// Draws 2 beveled borders.
// CornerSize can be 0, 1 or 2.
// Color: left, top, right, bottom external border color
// InternalColor: left, top, right, bottom internal border color
// ForceRectBorders: forces the borders to be rect
var
  Color: TColor;
  CornerSizeTL, CornerSizeTR, CornerSizeBL, CornerSizeBR: Integer;
begin
  Color := ACanvas.Pen.Color;

  if CornerSize < 0 then CornerSize := 0;
  if CornerSize > 2 then CornerSize := 2;
  CornerSizeTL := CornerSize;
  CornerSizeTR := CornerSize;
  CornerSizeBL := CornerSize;
  CornerSizeBR := CornerSize;
  if akLeft in ForceRectBorders then begin
    CornerSizeTL := 0;
    CornerSizeBL := 0;
  end;
  if akRight in ForceRectBorders then begin
    CornerSizeTR := 0;
    CornerSizeBR := 0;
  end;
  if akTop in ForceRectBorders then begin
    CornerSizeTL := 0;
    CornerSizeTR := 0;
  end;
  if akBottom in ForceRectBorders then begin
    CornerSizeBL := 0;
    CornerSizeBR := 0;
  end;

  with ARect do begin
    Dec(Right);
    Dec(Bottom);

    // Internal borders
    InflateRect(ARect, -1, -1);
    if InternalColorL <> clNone then begin
      ACanvas.Pen.Color := InternalColorL;
      ACanvas.PolyLine([Point(Left, Bottom), Point(Left, Top)]);
    end;
    if InternalColorT <> clNone then begin
      ACanvas.Pen.Color := InternalColorT;
      ACanvas.PolyLine([Point(Left, Top), Point(Right, Top)]);
    end;
    if InternalColorR <> clNone then begin
      ACanvas.Pen.Color := InternalColorR;
      ACanvas.PolyLine([Point(Right, Bottom), Point(Right, Top - 1)]);
    end;
    if InternalColorB <> clNone then begin
      ACanvas.Pen.Color := InternalColorB;
      ACanvas.PolyLine([Point(Left, Bottom), Point(Right, Bottom)]);
    end;

    // External borders
    InflateRect(ARect, 1, 1);
    if ColorL <> clNone then begin
      ACanvas.Pen.Color := ColorL;
      ACanvas.PolyLine([
        Point(Left, Bottom - CornerSizeBL),
        Point(Left, Top + CornerSizeTL)
      ]);
    end;
    if ColorT <> clNone then begin
      ACanvas.Pen.Color := ColorT;
      ACanvas.PolyLine([
        Point(Left, Top + CornerSizeTL),
        Point(Left + CornerSizeTL, Top),
        Point(Right - CornerSizeTR + 1, Top),
        Point(Right, Top + CornerSizeTR)
      ]);
    end;
    if ColorR <> clNone then begin
      ACanvas.Pen.Color := ColorR;
      ACanvas.PolyLine([
        Point(Right, Top + CornerSizeTR),
        Point(Right , Bottom - CornerSizeBR)
      ]);
    end;
    if ColorB <> clNone then begin
      ACanvas.Pen.Color := ColorB;
      ACanvas.PolyLine([
        Point(Right, Bottom - CornerSizeBR),
        Point(Right - CornerSizeBR, Bottom),
        Point(Left + CornerSizeBL, Bottom),
        Point(Left, Bottom - CornerSizeBL)
      ]);
    end;
  end;

  ACanvas.Pen.Color := Color;
end;


procedure SpDrawRectangle(ACanvas: TCanvas; ARect: TRect;
  CornerSize: Integer; ColorTL, ColorBR, ColorTLInternal, ColorBRInternal: TColor;
  ForceRectBorders: TAnchors);
// Draws 2 beveled borders.
// CornerSize can be 0, 1 or 2.
// TLColor, ColorBR: external border color
// InternalTL, ColorBRInternal: internal border color
// ForceRectBorders: forces the borders to be rect
var
  Color: TColor;
  CornerSizeTL, CornerSizeTR, CornerSizeBL, CornerSizeBR: Integer;
begin
  Color := ACanvas.Pen.Color;

  if CornerSize < 0 then CornerSize := 0;
  if CornerSize > 2 then CornerSize := 2;
  CornerSizeTL := CornerSize;
  CornerSizeTR := CornerSize;
  CornerSizeBL := CornerSize;
  CornerSizeBR := CornerSize;
  if akLeft in ForceRectBorders then begin
    CornerSizeTL := 0;
    CornerSizeBL := 0;
  end;
  if akRight in ForceRectBorders then begin
    CornerSizeTR := 0;
    CornerSizeBR := 0;
  end;
  if akTop in ForceRectBorders then begin
    CornerSizeTL := 0;
    CornerSizeTR := 0;
  end;
  if akBottom in ForceRectBorders then begin
    CornerSizeBL := 0;
    CornerSizeBR := 0;
  end;

  with ARect do begin
    Dec(Right);
    Dec(Bottom);

    // Internal borders
    InflateRect(ARect, -1, -1);
    if ColorTLInternal <> clNone then begin
      ACanvas.Pen.Color := ColorTLInternal;
      ACanvas.PolyLine([
        Point(Left, Bottom),
        Point(Left, Top),
        Point(Right, Top)
      ]);
    end;
    if ColorBRInternal <> clNone then begin
      ACanvas.Pen.Color := ColorBRInternal;
      ACanvas.PolyLine([
        Point(Left, Bottom),
        Point(Right, Bottom),
        Point(Right, Top - 1)
      ]);
    end;

    // External borders
    InflateRect(ARect, 1, 1);
    if ColorTL <> clNone then begin
      ACanvas.Pen.Color := ColorTL;
      ACanvas.PolyLine([
        Point(Left + CornerSizeBL, Bottom),
        Point(Left, Bottom - CornerSizeBL),
        Point(Left, Top + CornerSizeTL),
        Point(Left + CornerSizeTL, Top),
        Point(Right - CornerSizeTR, Top),
        Point(Right, Top + CornerSizeTR)
      ]);
    end;
    if ColorBR <> clNone then begin
      ACanvas.Pen.Color := ColorBR;
      ACanvas.PolyLine([
        Point(Right, Top + CornerSizeTR),
        Point(Right , Bottom - CornerSizeBR),
        Point(Right - CornerSizeBR, Bottom),
        Point(Left + CornerSizeBL - 1, Bottom)
      ]);
    end;
  end;

  ACanvas.Pen.Color := Color;
end;

procedure SpAlphaBlend(SrcDC, DstDC: HDC; SrcR, DstR: TRect; Alpha: Byte;
  SrcHasAlphaChannel: Boolean = False);
// NOTE: AlphaBlend does not work on Windows 95 and Windows NT
var
  BF: TBlendFunction;
begin
  BF.BlendOp := AC_SRC_OVER;
  BF.BlendFlags := 0;
  BF.SourceConstantAlpha := Alpha;
  if SrcHasAlphaChannel then
    BF.AlphaFormat := AC_SRC_ALPHA
  else
    BF.AlphaFormat := 0;
  Windows.AlphaBlend(DstDC, DstR.Left, DstR.Top, DstR.Right - DstR.Left, DstR.Bottom - DstR.Top,
    SrcDC, SrcR.Left, SrcR.Top, SrcR.Right - SrcR.Left, SrcR.Bottom - SrcR.Top, BF);
end;

procedure SpPaintTo(WinControl: TWinControl; ACanvas: TCanvas; X, Y: Integer);
// NOTE: PrintWindow does not work if the control is not visible
var
  B: TBitmap;
  PrevTop: Integer;
begin
  // Use SpPrintWindow instead of PaintTo as many controls will not render
  // properly (no text on editors, no scrollbars, incorrect borders, etc)
  // http://msdn2.microsoft.com/en-us/library/ms535695.aspx
  if Assigned(SpPrintWindow) then begin
    ACanvas.Lock;
    try
      // It doesn't work if the control is not visible !!!
      // Show it and move it offscreen
      if not WinControl.Visible then begin
        PrevTop := WinControl.Top;
        WinControl.Top := 10000;  // Move it offscreen
        WinControl.Visible := True;
        SpPrintWindow(WinControl.Handle, ACanvas.Handle, 0);
        WinControl.Visible := False;
        WinControl.Top := PrevTop;
      end
      else
        SpPrintWindow(WinControl.Handle, ACanvas.Handle, 0);
    finally
      ACanvas.UnLock;
    end;
  end
  else begin
    // If SpPrintWindow is not available use PaintTo
    // If the Control is a Form use GetFormImage instead
    if WinControl is TCustomForm then begin
      B := TCustomForm(WinControl).GetFormImage;
      try
        ACanvas.Draw(X, Y, B);
      finally
        B.Free;
      end;
    end
    else
      WinControl.PaintTo(ACanvas, X, Y);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ ImageList painting }

procedure SpDrawIconShadow(ACanvas: TCanvas; const ARect: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer);
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  Src, Dst: ^Cardinal;
  S, C, CBRB, CBG: Cardinal;
  B1, B2: TBitmap;
begin
  ImageWidth := ARect.Right - ARect.Left;
  ImageHeight := ARect.Bottom - ARect.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  B1 := TBitmap.Create;
  B2 := TBitmap.Create;
  try
    B1.PixelFormat := pf32bit;
    B2.PixelFormat := pf32bit;
    B1.Width := ImageWidth;
    B1.Height := ImageHeight;
    B2.Width := ImageWidth;
    B2.Height := ImageHeight;

    BitBlt(B1.Canvas.Handle, 0, 0, ImageWidth, ImageHeight, ACanvas.Handle, ARect.Left, ARect.Top, SRCCOPY);
    BitBlt(B2.Canvas.Handle, 0, 0, ImageWidth, ImageHeight, ACanvas.Handle, ARect.Left, ARect.Top, SRCCOPY);
    ImageList.Draw(B2.Canvas, 0, 0, ImageIndex, True);

    for J := 0 to ImageHeight - 1 do
    begin
      Src := B2.ScanLine[J];
      Dst := B1.ScanLine[J];
      for I := 0 to ImageWidth - 1 do
      begin
        S := Src^;
        if S <> Dst^ then
        begin
          CBRB := Dst^ and $00FF00FF;
          CBG  := Dst^ and $0000FF00;
          C := ((S and $00FF0000) shr 16 * 29 + (S and $0000FF00) shr 8 * 150 +
            (S and $000000FF) * 76) shr 8;
          C := (C div 3) + (255 - 255 div 3);
          Dst^ := ((CBRB * C and $FF00FF00) or (CBG * C and $00FF0000)) shr 8;
        end;
        Inc(Src);
        Inc(Dst);
      end;
    end;
    BitBlt(ACanvas.Handle, ARect.Left, ARect.Top, ImageWidth, ImageHeight, B1.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    B1.Free;
    B2.Free;
  end;
end;

procedure SpDrawImageList(ACanvas: TCanvas; const ARect: TRect; ImageList: TCustomImageList;
  ImageIndex: Integer; Enabled, DisabledIconCorrection: Boolean);
begin
  if Assigned(ImageList) and (ImageIndex > -1) and (ImageIndex < ImageList.Count) then
    if Enabled then
      ImageList.Draw(ACanvas, ARect.Left, ARect.Top, ImageIndex)
    else
      if DisabledIconCorrection then
        SpDrawIconShadow(ACanvas, ARect, ImageList, ImageIndex)
      else
        ImageList.Draw(ACanvas, ARect.Left, ARect.Top, ImageIndex, False);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Gradients }

procedure SpGradient(ACanvas: TCanvas; const ARect: TRect;
  StartPos, EndPos, ChunkSize: Integer; C1, C2: TColor; const Vertical: Boolean);
// StartPos: start position relative to ARect, usually 0
// EndPos: end position relative to ARect, usually ARect.Bottom - ARect.Top
// ChunkSize: size of the chunk of the gradient we need to paint
var
  I: Integer;
  r, g, b: Integer;
  rc1, gc1, bc1: Integer;
  rc2, gc2, bc2: Integer;
  PrevColor: TColor;
begin
  PrevColor := ACanvas.Brush.Color;
  if ChunkSize = 0 then ChunkSize := 1;
  SpGetRGB(C1, rc1, gc1, bc1);
  SpGetRGB(C2, rc2, gc2, bc2);
  for I := StartPos to EndPos do begin
    r := rc1 + (((rc2 - rc1) * (I - StartPos)) div ChunkSize);
    g := gc1 + (((gc2 - gc1) * (I - StartPos)) div ChunkSize);
    b := bc1 + (((bc2 - bc1) * (I - StartPos)) div ChunkSize);

    ACanvas.Brush.Color := SpRGBToColor(r, g, b);
    if Vertical then
      ACanvas.FillRect(Rect(ARect.Left, ARect.Top + I, ARect.Right, ARect.Top + I + 1))
    else
      ACanvas.FillRect(Rect(ARect.Left + I, ARect.Top, ARect.Left + I + 1, ARect.Bottom));
  end;
  ACanvas.Brush.Color := PrevColor;  
end;

procedure SpGradientFill(ACanvas: TCanvas; const ARect: TRect;
  const C1, C2: TColor; const Vertical: Boolean);
var
  GSize: Integer;
begin
  if Vertical then
    GSize := (ARect.Bottom - ARect.Top) - 1
  else
    GSize := (ARect.Right - ARect.Left) - 1;

  SpGradient(ACanvas, ARect, 0, GSize, GSize, C1, C2, Vertical);
end;

procedure SpGradientFillMirror(ACanvas: TCanvas; const ARect: TRect;
  const C1, C2, C3, C4: TColor; const Vertical: Boolean);
var
  GSize, ChunkSize, d1, d2: Integer;
begin
  if Vertical then
    GSize := (ARect.Bottom - ARect.Top) - 1
  else
    GSize := (ARect.Right - ARect.Left) - 1;

  ChunkSize := GSize div 2;
  if ChunkSize = 0 then ChunkSize := 1;
  d1 := ChunkSize;
  d2 := GSize;

  SpGradient(ACanvas, ARect, 0, d1, ChunkSize, C1, C2, Vertical);
  SpGradient(ACanvas, ARect, d1, d2, ChunkSize, C3, C4, Vertical);
end;

procedure SpGradientFillMirrorTop(ACanvas: TCanvas; const ARect: TRect;
  const C1, C2, C3, C4: TColor; const Vertical: Boolean);
var
  GSize, d1, d2: Integer;
begin
  if Vertical then
    GSize := (ARect.Bottom - ARect.Top) - 1
  else
    GSize := (ARect.Right - ARect.Left) - 1;

  d1 := GSize div 3;
  d2 := GSize;

  SpGradient(ACanvas, ARect, 0, d1, d1, C1, C2, Vertical);
  SpGradient(ACanvas, ARect, d1, d2, d2 - d1, C3, C4, Vertical);
end;

procedure SpGradientFillGlass(ACanvas: TCanvas; const ARect: TRect;
  const C1, C2, C3, C4: TColor; const Vertical: Boolean);
var
  GSize, ChunkSize, d1, d2, d3: Integer;
begin
  if Vertical then
    GSize := (ARect.Bottom - ARect.Top) - 1
  else
    GSize := (ARect.Right - ARect.Left) - 1;

  ChunkSize := GSize div 3;
  if ChunkSize = 0 then ChunkSize := 1;
  d1 := ChunkSize;
  d2 := ChunkSize * 2;
  d3 := GSize;

  SpGradient(ACanvas, ARect, 0, d1, ChunkSize, C1, C2, Vertical);
  SpGradient(ACanvas, ARect, d1, d2, ChunkSize, C2, C3, Vertical);
  SpGradient(ACanvas, ARect, d2, d3, ChunkSize, C3, C4, Vertical);
end;

procedure SpGradientFill9pixels(ACanvas: TCanvas; const ARect: TRect;
  const C1, C2, C3, C4: TColor; const Vertical: Boolean);
// Mimics Vista menubar/toolbar blue gradient
var
  GSize, d1, d2: Integer;
begin
  if Vertical then
    GSize := (ARect.Bottom - ARect.Top) - 1
  else
    GSize := (ARect.Right - ARect.Left) - 1;

  d1 := GSize div 3;
  if d1 > 9 then d1 := 9;
  d2 := GSize;

  SpGradient(ACanvas, ARect, 0, d1, d1, C1, C2, Vertical);
  SpGradient(ACanvas, ARect, d1, d2, d2 - d1, C3, C4, Vertical);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Element painting }

procedure SpDrawArrow(ACanvas: TCanvas; X, Y: Integer; AColor: TColor; Vertical, Reverse: Boolean; Size: Integer);
var
  C1, C2: TColor;
begin
  C1 := ACanvas.Pen.Color;
  C2 := ACanvas.Brush.Color;
  ACanvas.Pen.Color := AColor;
  ACanvas.Brush.Color := AColor;

  if Vertical then
    if Reverse then
      ACanvas.Polygon([Point(X, Y), Point(X - Size, Y + Size), Point(X + Size, Y + Size)])
    else
      ACanvas.Polygon([Point(X - Size, Y), Point(X + Size, Y), Point(X, Y + Size)])
  else
    if Reverse then
      ACanvas.Polygon([Point(X, Y), Point(X + Size, Y + Size), Point(X + Size, Y - Size)])
    else
      ACanvas.Polygon([Point(X, Y - Size), Point(X, Y + Size), Point(X + Size, Y)]);

  ACanvas.Pen.Color := C1;
  ACanvas.Brush.Color := C2;
end;

procedure SpDrawDropMark(ACanvas: TCanvas; DropMark: TRect);
var
  C: TColor;
  R: TRect;
begin
  if IsRectEmpty(DropMark) then Exit;
  C := ACanvas.Brush.Color;

  R := Rect(DropMark.Left + 1, DropMark.Top, DropMark.Right - 1, DropMark.Top + 2);
  ACanvas.Rectangle(R);
  R := Rect(DropMark.Left + 1, DropMark.Bottom - 2, DropMark.Right - 1, DropMark.Bottom);
  ACanvas.Rectangle(R);

  R := Rect(DropMark.Left, DropMark.Top + 1, DropMark.Right, DropMark.Top + 3);
  ACanvas.Rectangle(R);
  R := Rect(DropMark.Left, DropMark.Bottom - 3, DropMark.Right, DropMark.Bottom - 1);
  ACanvas.Rectangle(R);

  R := Rect(DropMark.Left + 1, DropMark.Top + 4, DropMark.Right - 1, DropMark.Bottom - 4);
  ACanvas.Rectangle(R);

    {
    // Standard DropMark

    R := Rect(DropMark.Left, DropMark.Top, DropMark.Right - 1, DropMark.Bottom - 1);
    if IsRectEmpty(R) then Exit;
    ACanvas.Brush.Color := clBlack;
    ACanvas.Polygon([
      Point(R.Left, R.Top),
      Point(R.Left + 2, R.Top + 2),
      Point(R.Left + 2, R.Bottom - 2),
      Point(R.Left, R.Bottom),
      Point(R.Right, R.Bottom),
      Point(R.Right - 2, R.Bottom - 2),
      Point(R.Right - 2, R.Top + 2),
      Point(R.Right, R.Top)
    ]);
    }
  ACanvas.Brush.Color := C;
end;

procedure SpDrawFocusRect(ACanvas: TCanvas; const ARect: TRect);
var
  DC: HDC;
  C1, C2: TColor;
begin
  if not IsRectEmpty(ARect) then begin
    DC := ACanvas.Handle;
    C1 := SetTextColor(DC, clBlack);
    C2 := SetBkColor(DC, clWhite);
    ACanvas.DrawFocusRect(ARect);
    SetTextColor(DC, C1);
    SetBkColor(DC, C2);
  end;
end;

procedure SpDrawGlyphPattern(DC: HDC; const R: TRect; Width, Height: Integer;
  const PatternBits; PatternColor: TColor);
var
  B: TBitmap;
  OldTextColor, OldBkColor: Longword;
  OldBrush, Brush: HBrush;
  BitmapWidth, BitmapHeight: Integer;
begin
  OldTextColor := SetTextColor(DC, clBlack);
  OldBkColor := SetBkColor(DC, clWhite);
  B := TBitmap.Create;
  try
    BitmapWidth := 8;
    if Width > BitmapWidth then BitmapWidth := Width;
    BitmapHeight := 8;
    if Height > BitmapHeight then BitmapHeight := Height;
    
    B.Handle := CreateBitmap(BitmapWidth, BitmapHeight, 1, 1, @PatternBits);
    if PatternColor < 0 then Brush := GetSysColorBrush(PatternColor and $FF)
    else Brush := CreateSolidBrush(PatternColor);
    OldBrush := SelectObject(DC, Brush);
    BitBlt(DC, (R.Left + R.Right + 1 - Width) div 2, (R.Top + R.Bottom  + 1 - Height) div 2,
      Width, Height, B.Canvas.Handle, 0, 0, ROP_DSPDxax);
    SelectObject(DC, OldBrush);
    if PatternColor >= 0 then DeleteObject(Brush);
  finally
    SetTextColor(DC, OldTextColor);
    SetBkColor(DC, OldBkColor);
    B.Free;
  end;
end;

procedure SpDrawGlyphPattern(ACanvas: TCanvas; ARect: TRect; PatternIndex: Integer; PatternColor: TColor);
// The pattern is a 8x8 bitmap
// The array has 16 elements, only the odd elements are used
// The first value of an element represents the bits from the 4 first horizontal pixels,
// and the next value represents the bits of the 4 last horizontal pixels.
// For example: 0   represents --------
//              $FF represents xxxxxxxx
//              $C6 represents xx---xx-
const
  ClosePattern: array [0..15] of Byte    = ($C6, 0, $EE, 0, $7C, 0, $38, 0, $7C, 0, $EE, 0, $C6, 0, 0, 0);
  MaximizePattern: array [0..15] of Byte = ($FF, 0, $FF, 0, $81, 0, $81, 0, $81, 0, $81, 0, $81, 0, $FF, 0);
  MinimizePattern: array [0..15] of Byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $7E, 0, $7E, 0, 0, 0);
  RestorePattern: array [0..17] of Byte = ($3F, 0, $3F, 0, $21, 0, $FD, 0, $FD, 0, $87, 0, $84, 0, $84, 0, $FC, 0);
begin
  case PatternIndex of
    0: SpDrawGlyphPattern(ACanvas.Handle, ARect, 8, 8, ClosePattern[0], PatternColor);
    1: SpDrawGlyphPattern(ACanvas.Handle, ARect, 8, 8, MaximizePattern[0], PatternColor);
    2: SpDrawGlyphPattern(ACanvas.Handle, ARect, 8, 8, MinimizePattern[0], PatternColor);
    3: SpDrawGlyphPattern(ACanvas.Handle, ARect, 8, 9, RestorePattern[0], PatternColor);
  end;
end;

procedure SpDrawXPButton(ACanvas: TCanvas; ARect: TRect; Enabled, Pushed, HotTrack, Checked, Focused, Defaulted: Boolean;
  SkinType: TSpTBXSkinType);
var
  Flags: Cardinal;
  C: TColor;
  State: TSpTBXSkinStatesType;
begin
  SkinType := SpTBXSkinType(SkinType);
  case SkinType of
    sknNone:
      begin
        C := ACanvas.Brush.Color;
        ACanvas.Brush.Color := clBtnFace;
        ACanvas.FillRect(ARect);
        if Defaulted or Focused then begin
          ACanvas.Brush.Color := clWindowFrame;
          ACanvas.FrameRect(ARect);
          InflateRect(ARect, -1, -1);  // Reduce the Rect for the focus rect
        end;
        if Pushed or Checked then begin
          ACanvas.Brush.Color := clBtnShadow;
          ACanvas.FrameRect(ARect);
        end
        else
          DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH);
        ACanvas.Brush.Color := C;
      end;
    sknWindows:
      begin
        if not Enabled then Flags := PBS_DISABLED
        else if Pushed or Checked then Flags := PBS_PRESSED
        else if HotTrack then Flags := PBS_HOT
        else if Defaulted or Focused then Flags := PBS_DEFAULTED
        else Flags := PBS_NORMAL;
        DrawThemeBackground(ThemeServices.Theme[teButton], ACanvas.Handle, BP_PUSHBUTTON, Flags, ARect, nil);
      end;
    sknSkin:
      begin
        State := CurrentSkin.GetState(Enabled, Pushed, HotTrack, Checked);
        CurrentSkin.PaintBackground(ACanvas, ARect, skncButton, State, True, True);
      end;
  end;

  if Focused then begin
    InflateRect(ARect, -3, -3);
    SpDrawFocusRect(ACanvas, ARect);
  end;
end;

procedure SpDrawXPCheckBoxGlyph(ACanvas: TCanvas; ARect: TRect; Enabled: Boolean;
  State: TCheckBoxState; HotTrack, Pushed: Boolean; SkinType: TSpTBXSkinType);
var
  Flags: Integer;
  SknState: TSpTBXSkinStatesType;
begin
  SkinType := SpTBXSkinType(SkinType);
  Flags := 0;
  case SkinType of
    sknNone:
      begin
        case State of
          cbChecked: Flags := DFCS_BUTTONCHECK or DFCS_CHECKED;
          cbGrayed: Flags := DFCS_BUTTON3STATE or DFCS_CHECKED;
          cbUnChecked: Flags := DFCS_BUTTONCHECK;
        end;
        if not Enabled then
          Flags := Flags or DFCS_INACTIVE;
        if Pushed then
          Flags := Flags or DFCS_PUSHED;
        DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, Flags);
      end;
    sknWindows:
      begin
        case State of
          cbChecked: Flags := CBS_CHECKEDNORMAL;
          cbGrayed: Flags := CBS_MIXEDNORMAL;
          cbUnChecked: Flags := CBS_UNCHECKEDNORMAL;
        end;

        if not Enabled then Inc(Flags, 3)
        else
          if Pushed then Inc(Flags, 2)
          else if HotTrack then Inc(Flags);
        DrawThemeBackground(ThemeServices.Theme[teButton], ACanvas.Handle, BP_CHECKBOX, Flags, ARect, nil);
      end;
    sknSkin:
      begin
        SknState := CurrentSkin.GetState(Enabled, Pushed, HotTrack, State in [cbChecked, cbGrayed]);
        CurrentSkin.PaintMenuCheckMark(ACanvas, ARect, State = cbChecked, State = cbGrayed, False, SknState);
      end;
  end;
end;

procedure SpDrawXPRadioButtonGlyph(ACanvas: TCanvas; ARect: TRect; Enabled: Boolean;
  Checked, HotTrack, Pushed: Boolean; SkinType: TSpTBXSkinType);
var
  Flags: Integer;
  SknState: TSpTBXSkinStatesType;
begin
  SkinType := SpTBXSkinType(SkinType);
  case SkinType of
    sknNone:
      begin
        Flags := DFCS_BUTTONRADIO;
        if Checked then
          Flags := Flags or DFCS_CHECKED;
        if not Enabled then
          Flags := Flags or DFCS_INACTIVE;
        if Pushed then
          Flags := Flags or DFCS_PUSHED;
        DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, Flags);
      end;
    sknWindows:
      begin
        if Checked then Flags := RBS_CHECKEDNORMAL
        else Flags := RBS_UNCHECKEDNORMAL;

        if not Enabled then Inc(Flags, 3)
        else
          if Pushed then Inc(Flags, 2)
          else if HotTrack then Inc(Flags);
        DrawThemeBackground(ThemeServices.Theme[teButton], ACanvas.Handle, BP_RADIOBUTTON, Flags, ARect, nil);
      end;
    sknSkin:
      begin
        SknState := CurrentSkin.GetState(Enabled, Pushed, HotTrack, Checked);
        CurrentSkin.PaintMenuRadioMark(ACanvas, ARect, Checked, False, SknState);
      end;
  end;
end;

procedure SpDrawXPEditFrame(ACanvas: TCanvas; ARect: TRect; Enabled, HotTrack: Boolean;
  SkinType: TSpTBXSkinType; ClipContent: Boolean; AutoAdjust: Boolean);
var
  PartID, Flags: Integer;
  BorderR: TRect;
  State: TSpTBXSkinStatesType;
  Entry: TSpTBXSkinOptionEntry;
const
  CP_BORDER = 4; // Available only on Vista with Delphi 2007 
begin
  SkinType := SpTBXSkinType(SkinType);

  if ClipContent then begin
    BorderR := ARect;
    if HotTrack then
      InflateRect(BorderR, -1, -1)
    else
      InflateRect(BorderR, -2, -2);
    ExcludeClipRect(ACanvas.Handle, BorderR.Left, BorderR.Top, BorderR.Right, BorderR.Bottom);
  end;
  try
    case SkinType of
      sknNone:
        if HotTrack then
          SpDrawRectangle(ACanvas, ARect, 0, clBtnShadow, clBtnHighlight, clBtnFace, clBtnFace)
        else
          SpDrawRectangle(ACanvas, ARect, 0, clBtnFace, clBtnFace, clBtnFace, clBtnFace);
      sknWindows:
        begin
          if SpIsWinVista then begin
            PartID := CP_BORDER;
            if not Enabled then Flags := CBXS_DISABLED
            else if HotTrack then Flags := CBXS_HOT
            else Flags := CBXS_NORMAL;
          end
          else begin
            PartID := 0;
            Flags := 0;
          end;
          DrawThemeBackground(ThemeServices.Theme[teComboBox], ACanvas.Handle, PartID, Flags, ARect, nil);
        end;
      sknSkin:
        begin
          State := CurrentSkin.GetState(Enabled, False, HotTrack, False);
          // Try to adjust the borders if only the internal borders are specified
          if AutoAdjust then begin
            Entry := SkinManager.CurrentSkin.Options(skncEditFrame, State).Borders;
            if (Entry.Color1 = clNone) and (Entry.Color2 = clNone) and
              (Entry.Color3 <> clNone) and (Entry.Color4 <> clNone) then
            begin
              CurrentSkin.PaintBackground(ACanvas, ARect, skncEditFrame, State, True, False);
              SpDrawRectangle(ACanvas, ARect, Entry.SkinType, Entry.Color3, Entry.Color4);
              Exit;
            end;
          end;

          CurrentSkin.PaintBackground(ACanvas, ARect, skncEditFrame, State, True, True);
        end;
    end;
  finally
    if ClipContent then
      SelectClipRgn(ACanvas.Handle, 0);
  end;
end;

procedure SpDrawXPEditFrame(AWinControl: TWinControl; HotTracking: Boolean;
  SkinType: TSpTBXSkinType; AutoAdjust: Boolean);
var
  R: TRect;
  DC: HDC;
  ACanvas: TCanvas;
begin
  DC := GetWindowDC(AWinControl.Handle);
  try
    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;
      GetWindowRect(AWinControl.Handle, R);
      OffsetRect(R, -R.Left, -R.Top);
      with R do
        ExcludeClipRect(DC, Left + 2, Top + 2, Right - 2, Bottom - 2);
      SpDrawParentBackground(AWinControl, ACanvas.Handle, R);
      SpDrawXPEditFrame(ACanvas, R, AWinControl.Enabled, HotTracking, SkinType, False, AutoAdjust);
    finally
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  finally
    ReleaseDC(AWinControl.Handle, DC);
  end;
end;

procedure SpDrawXPGrip(ACanvas: TCanvas; ARect: TRect; LoC, HiC: TColor);
var
  I, J: Integer;
  XCellCount, YCellCount: Integer;
  R: TRect;
  C: TColor;
begin
  //  4 x 4 cells (Grey, White, Null)
  //  GG--
  //  GGW-
  //  -WW-
  //  ----

  C := ACanvas.Brush.Color;
  XCellCount := (ARect.Right - ARect.Left) div 4;
  YCellCount := (ARect.Bottom - ARect.Top) div 4;
  if XCellCount = 0 then XCellCount := 1;
  if YCellCount = 0 then YCellCount := 1;
  
  for J := 0 to YCellCount - 1 do
    for I := 0 to XCellCount - 1 do begin
      R.Left := ARect.Left + (I * 4) + 1;
      R.Right := R.Left + 2;
      R.Top := ARect.Top + (J * 4) + 1;
      R.Bottom := R.Top + 2;

      ACanvas.Brush.Color := HiC;
      ACanvas.FillRect(R);
      OffsetRect(R, -1, -1);
      ACanvas.Brush.Color := LoC;
      ACanvas.FillRect(R);
    end;
  ACanvas.Brush.Color := C;
end;

procedure SpDrawXPHeader(ACanvas: TCanvas; ARect: TRect; IsFirstItem, HotTrack, Pushed: Boolean; SkinType: TSpTBXSkinType);
var
  Flags: Cardinal;
  State: TSpTBXSkinStatesType;
begin
  SkinType := SpTBXSkinType(SkinType);
  case SkinType of
    sknNone:
      begin
        DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDINNER or BDR_RAISEDOUTER, BF_RECT or BF_SOFT);
      end;
    sknWindows:
      begin
        if Pushed then Flags := HIS_PRESSED
        else if HotTrack then Flags := HIS_HOT
        else Flags := HIS_NORMAL;
        DrawThemeBackground(ThemeServices.Theme[teHeader], ACanvas.Handle, HP_HEADERITEM, Flags, ARect, nil);
      end;
    sknSkin:
      begin
        State := CurrentSkin.GetState(True, Pushed, HotTrack, False);
        if (State = sknsPushed) and CurrentSkin.Options(skncHeader, State).IsEmpty then
          State := sknsHotTrack;
        CurrentSkin.PaintBackground(ACanvas, ARect, skncHeader, State, True, True);
      end;
  end;
end;

procedure SpDrawXPListItemBackground(ACanvas: TCanvas; ARect: TRect; HotTrack, Pushed, Focused: Boolean;
  SkinType: TSpTBXSkinType);
var
  State: TSpTBXSkinStatesType;
begin
  if SpTBXSkinType(SkinType) = sknSkin then begin
    ACanvas.FillRect(ARect);
    if HotTrack then begin
      State := CurrentSkin.GetState(True, Pushed, HotTrack, False);
      ACanvas.Font.Color := CurrentSkin.GetTextColor(skncListItem, State);
      CurrentSkin.PaintBackground(ACanvas, ARect, skncListItem, State, True, True);
    end;
  end
  else begin
    if HotTrack then begin
      ACanvas.Brush.Color := clHighlight;
      ACanvas.Font.Color := clHighlightText;
    end;
    ACanvas.FillRect(ARect);
    if Focused then
      SpDrawFocusRect(ACanvas, ARect);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Skins painting }

procedure SpPaintSkinBackground(ACanvas: TCanvas; ARect: TRect; SkinOption: TSpTBXSkinOptionCategory; Vertical: Boolean);
var
  Part: TSpTBXSkinOptionEntry;
  SkinType: Integer;
begin
  Part := SkinOption.Body;
  SkinType := SkinOption.Body.SkinType;

  if Vertical then
    case SkinType of
      1: SkinType := 2;  // Vertical Gradient to Horizontal
      2: SkinType := 1;  // Horizontal Gradient to Vertical

      3: SkinType := 4;  // Vertical Glass Gradient to Horizontal
      4: SkinType := 3;  // Horizontal Glass Gradient to Vertical

      5: SkinType := 6;  // Vertical Mirror Gradient to Horizontal
      6: SkinType := 5;  // Horizontal Mirror Gradient to Vertical

      7: SkinType := 8;  // Vertical MirrorTop Gradient to Horizontal
      8: SkinType := 7;  // Horizontal MirrorTop Gradient to Vertical

      9: SkinType := 10; // Vertical 9Pixels Gradient to Horizontal
      10: SkinType := 9; // Horizontal 9Pixels Gradient to Vertical
    end;

  case SkinType of
    0: begin  // Solid
         SpFillRect(ACanvas, ARect, Part.Color1);
       end;
    1: begin  // Vertical Gradient
         SpGradientFill(ACanvas, ARect, Part.Color1, Part.Color2, True);
       end;
    2: begin  // Horizontal Gradient
         SpGradientFill(ACanvas, ARect, Part.Color1, Part.Color2, False);
       end;
    3: begin  // Vertical Glass Gradient
         SpGradientFillGlass(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, True);
       end;
    4: begin  // Horizontal Glass Gradient
         SpGradientFillGlass(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, False);
       end;
    5: begin  // Vertical Mirror Gradient
         SpGradientFillMirror(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, True);
       end;
    6: begin  // Horizontal Mirror Gradient
         SpGradientFillMirror(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, False);
       end;
    7: begin  // Vertical MirrorTop Gradient
         SpGradientFillMirrorTop(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, True);
       end;
    8: begin  // Horizontal MirrorTop Gradient
         SpGradientFillMirrorTop(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, False);
       end;
    9: begin  // Vertical 9Pixels Gradient
         SpGradientFill9Pixels(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, True);
       end;
    10:begin  // Horizontal 9Pixels Gradient
         SpGradientFill9Pixels(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, False);
       end;
  end;
end;

procedure SpPaintSkinBorders(ACanvas: TCanvas; ARect: TRect; SkinOption: TSpTBXSkinOptionCategory;
  ForceRectBorders: TAnchors = []);
var
  Part: TSpTBXSkinOptionEntry;
begin
  Part := SkinOption.Borders;
  case Part.SkinType of
    0, 1, 2: // Rectangle, Simple Rounded and Double Rounded Border
      begin
        SpDrawRectangle(ACanvas, ARect, Part.SkinType, Part.Color1, Part.Color2, Part.Color3, Part.Color4, ForceRectBorders);
      end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Misc }

function SpIsWinVista: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion = 6) and (Win32MinorVersion >= 0)
end;

function SpGetDirectories(Path: WideString; L: TStringList): Boolean;
var
  SearchRec: TSearchRec;
begin
  Result := False;
  if DirectoryExists(Path) then begin
    Path := IncludeTrailingPathDelimiter(Path) + '*.*';
    if FindFirst(Path, faDirectory, SearchRec) = 0 then begin
      try
        repeat
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            L.Add(SearchRec.Name);
        until FindNext(SearchRec) <> 0;
        Result := True;
      finally
        FindClose(SearchRec);
      end;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSkinOptionEntry }

procedure TSpTBXSkinOptionEntry.AssignTo(Dest: TPersistent);
begin
  if Dest is TSpTBXSkinOptionEntry then
    with TSpTBXSkinOptionEntry(Dest) do begin
      SkinType := Self.SkinType;
      Color1 := Self.Color1;
      Color2 := Self.Color2;
      Color3 := Self.Color3;
      Color4 := Self.Color4;
    end
  else inherited AssignTo(Dest);
end;

constructor TSpTBXSkinOptionEntry.Create;
begin
  inherited;
  Reset;
end;

procedure TSpTBXSkinOptionEntry.Fill(ASkinType: Integer; AColor1, AColor2,
  AColor3, AColor4: TColor);
begin
  FSkinType := ASkinType;
  FColor1 := AColor1;
  FColor2 := AColor2;
  FColor3 := AColor3;
  FColor4 := AColor4;
end;

function TSpTBXSkinOptionEntry.IsEmpty: Boolean;
begin
  Result := (FColor1 = clNone) and (FColor2 = clNone) and (FColor3 = clNone) and (FColor4 = clNone);
end;

procedure TSpTBXSkinOptionEntry.Reset;
begin
  FSkinType := 0;
  FColor1 := clNone;
  FColor2 := clNone;
  FColor3 := clNone;
  FColor4 := clNone;
end;

procedure TSpTBXSkinOptionEntry.ReadFromString(S: string);
var
  L: TStringList;
begin
  Reset;
  L := TStringList.Create;
  try
    L.CommaText := S;
    try
      if L.Count > 0 then FSkinType := StrToIntDef(L[0], 0);
      if L.Count > 1 then FColor1 := StringToColor(L[1]);
      if L.Count > 2 then FColor2 := StringToColor(L[2]);
      if L.Count > 3 then FColor3 := StringToColor(L[3]);
      if L.Count > 4 then FColor4 := StringToColor(L[4]);
    except
      // do nothing
    end;
  finally
    L.Free;
  end;
end;

function TSpTBXSkinOptionEntry.WriteToString: string;
begin
  Result := Format('%d, %s, %s, %s, %s', [FSkinType,
    SpColorToString(FColor1), SpColorToString(FColor2),
    SpColorToString(FColor3), SpColorToString(FColor4)]);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXThemeOptionCategory }

procedure TSpTBXSkinOptionCategory.AssignTo(Dest: TPersistent);
begin
  if Dest is TSpTBXSkinOptionCategory then
    with TSpTBXSkinOptionCategory(Dest) do begin
      Body.Assign(Self.Body);
      Borders.Assign(Self.Borders);
      TextColor := Self.TextColor;
    end
  else inherited AssignTo(Dest);
end;

constructor TSpTBXSkinOptionCategory.Create;
begin
  inherited;
  FBody := TSpTBXSkinOptionEntry.Create;
  FBorders := TSpTBXSkinOptionEntry.Create;
  FTextColor := clNone;
end;

destructor TSpTBXSkinOptionCategory.Destroy;
begin
  FreeAndNil(FBody);
  FreeAndNil(FBorders);
  inherited;
end;

function TSpTBXSkinOptionCategory.IsEmpty: Boolean;
begin
  Result := FBody.IsEmpty and FBorders.IsEmpty and (FTextColor = clNone);
end;

procedure TSpTBXSkinOptionCategory.Reset;
begin
  FBody.Reset;
  FBorders.Reset;
  FTextColor := clNone;
end;

procedure TSpTBXSkinOptionCategory.SaveToIni(MemIni: TMemIniFile; Section, Ident: string);
begin
  if not IsEmpty then begin
    MemIni.WriteString(Section, Ident + '.Body', Body.WriteToString);
    MemIni.WriteString(Section, Ident + '.Borders', Borders.WriteToString);
    MemIni.WriteString(Section, Ident + '.TextColor', SpColorToString(TextColor));
  end;
end;

procedure TSpTBXSkinOptionCategory.LoadFromIni(MemIni: TMemIniFile; Section, Ident: string);
begin
  Reset;
  if Ident = '' then Ident := SSpTBXSkinStatesString[sknsNormal];

  Body.ReadFromString(MemIni.ReadString(Section, Ident + '.Body', ''));
  Borders.ReadFromString(MemIni.ReadString(Section, Ident + '.Borders', ''));
  TextColor := StringToColor(MemIni.ReadString(Section, Ident + '.TextColor', 'clNone'));
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXThemeOptions }

procedure TSpTBXSkinOptions.AssignTo(Dest: TPersistent);
var
  C: TSpTBXSkinComponentsType;
  S: TSpTBXSkinStatesType;
  DestOp: TSpTBXSkinOptions;
begin
  if Dest is TSpTBXSkinOptions then begin
    DestOp := TSpTBXSkinOptions(Dest);
    for C := Low(C) to High(C) do
      for S := Low(S) to High(S) do
        DestOp.FOptions[C, S].Assign(Options(C, S));
    DestOp.OfficeStatusBar := FOfficeStatusBar;
    DestOp.OfficeIcons := FOfficeIcons;
    DestOp.FSkinName := FSkinName;
    DestOp.ColorBtnFace := FColorBtnFace;
  end
  else inherited AssignTo(Dest);
end;

procedure TSpTBXSkinOptions.CopyOptions(AComponent, ToComponent: TSpTBXSkinComponentsType);
var
  S: TSpTBXSkinStatesType;
begin
  for S := Low(S) to High(S) do
    FOptions[AComponent, S].AssignTo(FOptions[ToComponent, S]);
end;

constructor TSpTBXSkinOptions.Create;
var
  C: TSpTBXSkinComponentsType;
  S: TSpTBXSkinStatesType;
begin
  inherited;
  FSkinName := 'Default';
  FColorBtnFace := clBtnFace;
  FFloatingWindowBorderSize := 4;
  for C := Low(C) to High(C) do
    for S := Low(S) to High(S) do
      FOptions[C, S] := TSpTBXSkinOptionCategory.Create;

  FillOptions;
end;

destructor TSpTBXSkinOptions.Destroy;
var
  C: TSpTBXSkinComponentsType;
  S: TSpTBXSkinStatesType;
begin
  for C := Low(C) to High(C) do
    for S := Low(S) to High(S) do
      FreeAndNil(FOptions[C, S]);

  inherited;
end;

procedure TSpTBXSkinOptions.FillOptions;
begin
  // Used by descendants to fill the skin options
end;

procedure TSpTBXSkinOptions.Reset;
var
  C: TSpTBXSkinComponentsType;
  S: TSpTBXSkinStatesType;
begin
  FSkinName := 'Default';
  FColorBtnFace := clBtnFace;
  FFloatingWindowBorderSize := 4;
  FOfficeStatusBar := False;
  FOfficeIcons := False;
  for C := Low(C) to High(C) do
    for S := Low(S) to High(S) do
      FOptions[C, S].Reset;
end;

function TSpTBXSkinOptions.Options(Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType): TSpTBXSkinOptionCategory;
begin
  if CSpTBXSkinComponents[Component].States = [sknsNormal] then
    State := sknsNormal;
  Result := FOptions[Component, State];
end;

function TSpTBXSkinOptions.Options(Component: TSpTBXSkinComponentsType): TSpTBXSkinOptionCategory;
begin
  Result := FOptions[Component, sknsNormal];
end;

procedure TSpTBXSkinOptions.SaveToFile(Filename: WideString);
var
  MemIni: TMemIniFile;
begin
  MemIni := TMemIniFile.Create(Filename);
  try
    SaveToMemIni(MemIni);
    MemIni.UpdateFile;
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXSkinOptions.SaveToMemIni(MemIni: TMemIniFile);
var
  C: TSpTBXSkinComponentsType;
  S: TSpTBXSkinStatesType;
begin
  MemIni.WriteString('Skin', 'Name', FSkinName);
  MemIni.WriteString('Skin', 'ColorBtnFace', SpColorToString(FColorBtnFace));
  MemIni.WriteBool('Skin', 'OfficeStatusBar', FOfficeStatusBar);
  MemIni.WriteBool('Skin', 'OfficeIcons', FOfficeIcons);
  MemIni.WriteInteger('Skin', 'FloatingWindowBorderSize', FFloatingWindowBorderSize);

  for C := Low(C) to High(C) do begin
    for S := Low(S) to High(S) do
      if S in CSpTBXSkinComponents[C].States then
        FOptions[C, S].SaveToIni(MemIni, CSpTBXSkinComponents[C].Name, SSpTBXSkinStatesString[S]);
  end;
end;

procedure TSpTBXSkinOptions.SaveToStrings(L: TStrings);
var
  MemIni: TMemIniFile;
begin
  MemIni := TMemIniFile.Create('');
  try
    MemIni.SetStrings(L); // Transfer L contents to MemIni
    SaveToMemIni(MemIni);
    L.Clear;
    MemIni.GetStrings(L); // Transfer MemIni contents to L
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXSkinOptions.LoadFromFile(Filename: WideString);
var
  L: TStringList;
begin
  if FileExists(Filename) then begin
    L := TStringList.Create;
    try
      L.LoadFromFile(Filename);
      LoadFromStrings(L);
    finally
      L.Free;
    end;
  end;
end;

procedure TSpTBXSkinOptions.LoadFromStrings(L: TStrings);
var
  MemIni: TMemIniFile;
  C: TSpTBXSkinComponentsType;
  S: TSpTBXSkinStatesType;
begin
  Reset;
  MemIni := TMemIniFile.Create('');
  try
    MemIni.SetStrings(L);

    FSkinName := MemIni.ReadString('Skin', 'Name', '');
    FColorBtnFace := StringToColor(MemIni.ReadString('Skin', 'ColorBtnFace', 'clBtnFace'));
    FOfficeStatusBar := MemIni.ReadBool('Skin', 'OfficeStautsBar', False);
    FOfficeIcons := MemIni.ReadBool('Skin', 'OfficeIcons', False);
    FFloatingWindowBorderSize := MemIni.ReadInteger('Skin', 'FloatingWindowBorderSize', 4);

    for C := Low(C) to High(C) do begin
      for S := Low(S) to High(S) do
        if S in CSpTBXSkinComponents[C].States then
          FOptions[C, S].LoadFromIni(MemIni, CSpTBXSkinComponents[C].Name, SSpTBXSkinStatesString[S]);
    end;
  finally
    MemIni.Free;
  end;
end;

function TSpTBXSkinOptions.GetOfficeIcons: Boolean;
// OfficeIcons is used to paint the menu items icons with Office XP shadows.
begin
  Result := FOfficeIcons and (SkinManager.GetSkinType = sknSkin);
end;

function TSpTBXSkinOptions.GetOfficeMenuSeparator: Boolean;
// When OfficeMenuSeparator is True the height of the separators on popup menus
// is 6 pixels, otherwise the size is 10 pixels.
begin
  Result := FOfficeMenuSeparator and (SkinManager.GetSkinType = sknSkin);
end;

function TSpTBXSkinOptions.GetOfficePopup: Boolean;
// OfficePopup is used to paint the PopupWindow with Office XP style.
// It is also used to paint the opened toolbar item with shadows.
begin
  Result := (SkinManager.GetSkinType = sknSkin) and not Options(skncOpenToolbarItem).IsEmpty;
end;

function TSpTBXSkinOptions.GetOfficeStatusBar: Boolean;
// OfficeStatusBar is used to paint the StatusBar panels with Office XP style.
var
  T: TSpTBXSkinType;
begin
  T := SkinManager.GetSkinType;
  Result := (FOfficeStatusBar and (T = sknSkin)) or (T = sknNone);
end;

function TSpTBXSkinOptions.GetFloatingWindowBorderSize: Integer;
begin
  if SkinManager.GetSkinType = sknSkin then
    Result := FFloatingWindowBorderSize
  else
    Result := 4;
end;

procedure TSpTBXSkinOptions.SetFloatingWindowBorderSize(const Value: Integer);
begin
  FFloatingWindowBorderSize := Value;
  if FFloatingWindowBorderSize < 0 then FFloatingWindowBorderSize := 0;
  if FFloatingWindowBorderSize > 4 then FFloatingWindowBorderSize := 4;
end;

procedure TSpTBXSkinOptions.GetDropDownArrowSize(out DropDownArrowSize,
  DropDownArrowMargin, SplitBtnArrowSize: Integer);
begin
  DropDownArrowSize := 8; // TB2Item.tbDropdownArrowWidth
  DropDownArrowMargin := 3; // TB2Item.tbDropdownArrowMargin

  SplitBtnArrowSize := 12; // TB2Item.tbDropdownComboArrowWidth + 1
  if SkinManager.GetSkinType = sknWindows then
    SplitBtnArrowSize := SplitBtnArrowSize + 1;
end;

procedure TSpTBXSkinOptions.GetMenuItemMargins(ACanvas: TCanvas; ImgSize: Integer;
  out MarginsInfo: TSpTBXMenuItemMarginsInfo);
var
  TextMetric: TTextMetric;
  H, M2: Integer;
begin
  if ImgSize = 0 then
    ImgSize := 16;

  FillChar(MarginsInfo, SizeOf(MarginsInfo), 0);

  if (SkinManager.GetSkinType = sknSkin) or ((SkinManager.GetSkinType = sknWindows) and SpIsWinVista) then begin
    // Office like spacing, used by Windows Vista and all the skins
    MarginsInfo.Margins := Rect(1, 4, 1, 4); // MID_MENUITEM
    MarginsInfo.ImageTextSpace := 5;         // TMI_MENU_IMGTEXTSPACE
    MarginsInfo.LeftCaptionMargin := 3;      // TMI_MENU_LCAPTIONMARGIN
    MarginsInfo.RightCaptionMargin := 3;     // TMI_MENU_RCAPTIONMARGIN
  end
  else begin
    MarginsInfo.Margins := Rect(0, 3, 0, 3); // MID_MENUITEM
    MarginsInfo.ImageTextSpace := 1;         // TMI_MENU_IMGTEXTSPACE
    MarginsInfo.LeftCaptionMargin := 2;      // TMI_MENU_LCAPTIONMARGIN
    MarginsInfo.RightCaptionMargin := 2;     // TMI_MENU_RCAPTIONMARGIN
  end;

  GetTextMetrics(ACanvas.Handle, TextMetric);
  M2 := MarginsInfo.Margins.Top + MarginsInfo.Margins.Bottom;
  MarginsInfo.GutterSize := TextMetric.tmHeight + TextMetric.tmExternalLeading + M2;
  H := ImgSize + M2;
  if H > MarginsInfo.GutterSize then MarginsInfo.GutterSize := H;
  MarginsInfo.GutterSize := (ImgSize + M2) * MarginsInfo.GutterSize div H - 2;  // GutterSize = GetPopupMargin = ItemInfo.PopupMargin
end;

function TSpTBXSkinOptions.GetState(Enabled, Pushed, HotTrack, Checked: Boolean): TSpTBXSkinStatesType;
begin
  Result := sknsNormal;
  if not Enabled then Result := sknsDisabled
  else begin
    if Pushed then Result := sknsPushed
    else
      if HotTrack and Checked then Result := sknsCheckedAndHotTrack
      else
        if HotTrack then Result := sknsHotTrack
        else
          if Checked then Result := sknsChecked;
  end;
end;

function TSpTBXSkinOptions.GetTextColor(Component: TSpTBXSkinComponentsType;
  State: TSpTBXSkinStatesType; SkinType: TSpTBXSkinType = sknSkin): TColor;
var
  Flags: Integer;
  VistaColor: Cardinal;
begin
  Result := clNone;
  SkinType := SpTBXSkinType(SkinType);

  if SkinType = sknSkin then begin
    if State in CSpTBXSkinComponents[Component].States then begin
      Result := Options(Component, State).TextColor;
      if Result <> clNone then
        Exit; // Text color is specified by the skin
    end
    else
      Exit; // Exit if the State is not valid
  end;

  if State = sknsDisabled then Result := clGrayText
  else Result := clBtnText;

  case Component of
    skncMenuItem:
      if SpIsWinVista and (SkinType = sknWindows) then begin
        Flags := MPI_NORMAL;
        if State = sknsDisabled then Flags := MPI_DISABLED
        else if State in [sknsHotTrack, sknsCheckedAndHotTrack] then Flags := MPI_HOT;
        GetThemeColor(ThemeServices.Theme[teMenu], MENU_POPUPITEM, Flags, TMT_TEXTCOLOR, VistaColor);
        Result := TColor(VistaColor);
      end
      else
        if State <> sknsDisabled then begin
          Result := clMenuText;
          if SkinType <> sknSkin then
            if State in [sknsHotTrack, sknsCheckedAndHotTrack, sknsPushed] then
              Result := clHighlightText;
        end;
    skncMenuBarItem:
      if State <> sknsDisabled then begin
        Result := clMenuText;
        if SkinType = sknWindows then
          if State in [sknsHotTrack, sknsPushed, sknsChecked, sknsCheckedAndHotTrack] then
            Result := clHighlightText;
      end;
    skncToolbarItem:
      if State <> sknsDisabled then Result := clMenuText;
    skncListItem:
      if SkinType <> sknSkin then
        if State in [sknsHotTrack, sknsCheckedAndHotTrack] then
          Result := clHighlightText;
    skncDockablePanelTitleBar, skncStatusBar, skncTabToolbar:
      if SkinType = sknSkin then
        Result := clNone; // Reset Result to clNone so the Toolbar can override the items text color
    skncWindowTitleBar:
      if SkinType = sknSkin then
        Result := clNone  // Reset Result to clNone so the Toolbar can override the items text color
      else
        if State = sknsDisabled then Result := clInactiveCaptionText
        else Result := clCaptionText;
  end;
end;

procedure TSpTBXSkinOptions.PaintBackground(ACanvas: TCanvas; ARect: TRect;
  Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType;
  Background, Borders: Boolean; Vertical: Boolean = False;
  ForceRectBorders: TAnchors = []);
var
  BackgroundRect: TRect;
  Op: TSpTBXSkinOptionCategory;
begin
  Op := Options(Component, State);

  if Op.Borders.IsEmpty then
    Borders := False;
  if Op.Body.IsEmpty then
    Background := False;

  if Background then begin
    BackgroundRect := ARect;
    if Borders then
      InflateRect(BackgroundRect, -1, -1);
    SpPaintSkinBackground(ACanvas, BackgroundRect, Op, Vertical);
  end;

  if Borders then
    SpPaintSkinBorders(ACanvas, ARect, Op, ForceRectBorders);
end;

procedure TSpTBXSkinOptions.PaintMenuCheckMark(ACanvas: TCanvas; ARect: TRect;
  Checked, Grayed, MenuItemStyle: Boolean; State: TSpTBXSkinStatesType);
var
  X, Y: Integer;
  PenC, BrushC, CheckColor: TColor;
  Flags: Integer;
  VistaCheckSize: TSize;
begin
  if MenuItemStyle and SpIsWinVista and (SkinManager.GetSkinType = sknWindows) then begin
    if State = sknsDisabled then Flags := MC_CHECKMARKDISABLED
    else Flags := MC_CHECKMARKNORMAL;
    VistaCheckSize.cx := 0;
    VistaCheckSize.cy := 0;
    GetThemePartSize(ThemeServices.Theme[teMenu], ACanvas.Handle, MENU_POPUPCHECK, Flags, nil, TS_TRUE, VistaCheckSize);
    ARect := SpCenterRect(ARect, VistaCheckSize.cx, VistaCheckSize.cy);
    DrawThemeBackground(ThemeServices.Theme[teMenu], ACanvas.Handle, MENU_POPUPCHECK, Flags, ARect, nil);
  end
  else begin
    X := (ARect.Left + ARect.Right) div 2 - 1;
    Y := (ARect.Top + ARect.Bottom) div 2 + 1;

    PenC := ACanvas.Pen.Color;
    BrushC := ACanvas.Brush.Color;
    try
      if MenuItemStyle then begin
        CheckColor := clMenuText; // On sknNone it's clMenuText even when disabled
        case SkinManager.GetSkinType of
          sknWindows:
            CheckColor := GetTextColor(skncCheckBox, State);
          sknSkin:
            CheckColor := GetTextColor(skncMenuItem, State);
        end;
        ACanvas.Brush.Color := CheckColor;
        ACanvas.Pen.Color := CheckColor;
        ACanvas.Polygon([Point(X - 3, Y - 2), Point(X - 1, Y), Point(X + 3, Y - 4),
          Point(X + 3, Y - 3), Point(X - 1, Y + 1), Point(X - 3, Y - 1), Point(X - 3, Y -2)]);
      end
      else begin
        CheckColor := GetTextColor(skncCheckBox, State);
        ACanvas.Brush.Color := CheckColor;
        ACanvas.Pen.Color := CheckColor;
        PaintBackground(ACanvas, ARect, skncCheckBox, State, True, True);
        if Checked then
          ACanvas.Polygon([Point(X - 2, Y), Point(X, Y + 2), Point(X + 4, Y - 2),
            Point(X + 4, Y - 4), Point(X, Y), Point(X - 2, Y - 2), Point(X - 2, Y)])
        else
          if Grayed then begin
            InflateRect(ARect, -3, -3);
            // ACanvas.Brush.Color := Options(skncCheckBox, sknsChecked).Borders.Color1;
            ACanvas.FillRect(ARect);
          end;
      end;
    finally
      ACanvas.Pen.Color := PenC;
      ACanvas.Brush.Color := BrushC;
    end;
  end;
end;

procedure TSpTBXSkinOptions.PaintMenuRadioMark(ACanvas: TCanvas; ARect: TRect;
  Checked, MenuItemStyle: Boolean; State: TSpTBXSkinStatesType);
var
  X, Y: Integer;
  PenC, BrushC, CheckColor, FrameColor: TColor;
  Flags: Integer;
  VistaCheckSize: TSize;
begin
  if MenuItemStyle and SpIsWinVista and (SkinManager.GetSkinType = sknWindows) then begin
    if State = sknsDisabled then Flags := MC_BULLETDISABLED
    else Flags := MC_BULLETNORMAL;
    VistaCheckSize.cx := 0;
    VistaCheckSize.cy := 0;
    GetThemePartSize(ThemeServices.Theme[teMenu], ACanvas.Handle, MENU_POPUPCHECK, Flags, nil, TS_TRUE, VistaCheckSize);
    ARect := SpCenterRect(ARect, VistaCheckSize.cx, VistaCheckSize.cy);
    DrawThemeBackground(ThemeServices.Theme[teMenu], ACanvas.Handle, MENU_POPUPCHECK, Flags, ARect, nil);
  end
  else begin
    PenC := ACanvas.Pen.Color;
    BrushC := ACanvas.Brush.Color;
    try
      if MenuItemStyle then begin
        CheckColor := GetTextColor(skncRadioButton, State);
        ACanvas.Brush.Color := CheckColor;
        ACanvas.Pen.Color := CheckColor;
        X := (ARect.Left + ARect.Right) div 2 - 1;
        Y := (ARect.Top + ARect.Bottom) div 2 + 1;
        ACanvas.RoundRect(X - 2, Y - 4, X + 4, Y + 2, 2, 2);
      end
      else begin
        CheckColor := GetTextColor(skncRadioButton, State);
        FrameColor := Options(skncRadioButton, State).Borders.Color1;
        if State = sknsDisabled then FrameColor := CheckColor;
        if not Checked then CheckColor := clNone;

        // Keep it simple make the radio 13x13
        ARect.Left := (ARect.Left + ARect.Right - 13) div 2;
        ARect.Right := ARect.Left + 13;
        ARect.Top := (ARect.Top + ARect.Bottom - 13) div 2;
        ARect.Bottom := ARect.Top + 13;
        X := ARect.Left;
        Y := ARect.Top;

        // Background
        BeginPath(ACanvas.Handle);
        ACanvas.Polyline([Point(X, Y + 8), Point(X, Y + 4), Point(X + 1, Y + 3),
            Point(X + 1, Y + 2), Point(X + 2, Y + 1), Point(X + 3, Y + 1),
              Point(X + 4, Y), Point(X + 8, Y), Point(X + 9, Y + 1),
              Point(X + 10, Y + 1), Point(X + 11, Y + 2), Point(X + 11, Y + 3),
              Point(X + 12, Y + 4), Point(X + 12, Y + 8), Point(X + 11, Y + 9),
              Point(X + 11, Y + 10), Point(X + 10, Y + 11), Point(X + 9, Y + 11),
              Point(X + 8, Y + 12), Point(X + 4, Y + 12), Point(X + 3, Y + 11),
              Point(X + 2, Y + 11), Point(X + 1, Y + 10), Point(X + 1, Y + 8)]);
        EndPath(ACanvas.Handle);
        SelectClipPath(ACanvas.Handle, RGN_COPY);
        PaintBackground(ACanvas, ARect, skncRadioButton, State, True, False);
        SelectClipPath(ACanvas.Handle, 0);
        SelectClipRgn(ACanvas.Handle, 0);

        // Frame
        ACanvas.Brush.Color := FrameColor;
        ACanvas.Pen.Color := FrameColor;
        ACanvas.Polyline([Point(X, Y + 8), Point(X, Y + 4), Point(X + 1, Y + 3),
            Point(X + 1, Y + 2), Point(X + 2, Y + 1), Point(X + 3, Y + 1),
              Point(X + 4, Y), Point(X + 8, Y), Point(X + 9, Y + 1),
              Point(X + 10, Y + 1), Point(X + 11, Y + 2), Point(X + 11, Y + 3),
              Point(X + 12, Y + 4), Point(X + 12, Y + 8), Point(X + 11, Y + 9),
              Point(X + 11, Y + 10), Point(X + 10, Y + 11), Point(X + 9, Y + 11),
              Point(X + 8, Y + 12), Point(X + 4, Y + 12), Point(X + 3, Y + 11),
              Point(X + 2, Y + 11), Point(X + 1, Y + 10), Point(X + 1, Y + 8)]);

        // Radio
        if CheckColor <> clNone then begin
          ACanvas.Brush.Color := CheckColor;
          ACanvas.Pen.Color := CheckColor;
          X := (ARect.Left + ARect.Right) div 2;
          Y := (ARect.Top + ARect.Bottom) div 2 + 1;
          ACanvas.RoundRect(X - 2, Y - 3, X + 3, Y + 2, 2, 2);
        end;
      end;
    finally
      ACanvas.Pen.Color := PenC;
      ACanvas.Brush.Color := BrushC;
    end;
  end;
end;

procedure TSpTBXSkinOptions.PaintWindowFrame(ACanvas: TCanvas; ARect: TRect;
  IsActive, DrawBody: Boolean; BorderSize: Integer = 4);
var
  C: TColor;
  R: TRect;
  I: Integer;
  State: TSpTBXSkinStatesType;
  Op: TSpTBXSkinOptionEntry;
begin
  if IsActive then
    State := sknsNormal
  else
    if Options(skncWindow, sknsDisabled).IsEmpty then
      State := sknsNormal
    else
      State := sknsDisabled;

  C := ACanvas.Brush.Color;
  if DrawBody then
    PaintBackground(ACanvas, ARect, skncWindow, State, True, False);
  R := ARect;
  Op := Options(skncWindow, State).Borders;
  for I := 1 to BorderSize do begin
    if I = 1 then ACanvas.Brush.Color := Op.Color1
    else if I = 2 then ACanvas.Brush.Color := Op.Color2
    else if I = 3 then ACanvas.Brush.Color := Op.Color3
    else if I >= 4 then ACanvas.Brush.Color := Op.Color4;
    ACanvas.FrameRect(R);
    InflateRect(R, -1, -1);
  end;
  ACanvas.Brush.Color := C;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSkinsListEntry }

destructor TSpTBXSkinsListEntry.Destroy;
begin
  SkinClass := nil;
  FreeAndNil(SkinStrings);

  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSkinsList }

destructor TSpTBXSkinsList.Destroy;
begin
  // Free all the skins options
  while Count > 0 do
    Delete(0);
  inherited;
end;

function TSpTBXSkinsList.AddSkin(SkinName: string; SkinClass: TSpTBXSkinOptionsClass): Integer;
var
  K: TSpTBXSkinsListEntry;
begin
  Result := -1;
  if (SkinName <> '') and (IndexOf(SkinName) = -1) then begin
    K := TSpTBXSkinsListEntry.Create;
    try
      K.SkinClass := SkinClass;
      Result := AddObject(SkinName, K);  // the list owns K
    except
      K.Free;
    end;
  end;
end;

function TSpTBXSkinsList.AddSkin(SkinOptions: TStrings): Integer;
var
  K: TSpTBXSkinsListEntry;
  S: string;
begin
  Result := -1;
  K := TSpTBXSkinsListEntry.Create;
  try
    K.SkinStrings := TStringList.Create;
    S := SkinOptions.Values['Name '];
    if S = '' then
      S := SkinOptions.Values['Name'];
    S := Trim(S);
    if (S <> '') and  (IndexOf(S) = -1) then begin
      K.SkinStrings.Assign(SkinOptions);
      Result := AddObject(S, K);  // the list owns K
    end
    else
      K.Free;
  except
    K.Free;
  end;
end;

function TSpTBXSkinsList.AddSkinFromFile(Filename: WideString): Integer;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.LoadFromFile(Filename);
    Result := AddSkin(L);
  finally
    L.Free;
  end;
end;

procedure TSpTBXSkinsList.AddSkinsFromFolder(Folder: WideString);
var
  L: TStringList;
  I: Integer;
  S: string;
begin
  L := TStringList.Create;
  try
    if SpGetDirectories(Folder, L) then begin
      for I := 0 to L.Count - 1 do begin
        S := IncludeTrailingPathDelimiter(Folder) + L[I] + '\Skin.ini';
        if FileExists(S) then
          AddSkinFromFile(S);
      end;
    end;
  finally
    L.Free;
  end;
end;

procedure TSpTBXSkinsList.Delete(Index: Integer);
begin
  if (Index > -1) and (Index < Count) then
    SkinOptions[Index].Free;
  inherited Delete(Index);
end;

procedure TSpTBXSkinsList.GetSkinNames(SkinNames: TStrings);
var
  I: Integer;
begin
  SkinNames.BeginUpdate;
  try
    SkinNames.Clear;
    SkinNames.Add('Default');
    for I := 0 to Count - 1 do
      SkinNames.Add(Strings[I]);
  finally
    SkinNames.EndUpdate;
  end;
end;

function TSpTBXSkinsList.GetSkinOption(Index: Integer): TSpTBXSkinsListEntry;
begin
  Result := TSpTBXSkinsListEntry(Objects[Index]);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSkinManager }

constructor TSpTBXSkinManager.Create;
begin
  FNotifies := TList.Create;
  FCurrentSkin := TSpTBXSkinOptions.Create;
  FSkinsList := TSpTBXSkinsList.Create;
end;

destructor TSpTBXSkinManager.Destroy;
begin
  FreeAndNil(FNotifies);
  FreeAndNil(FCurrentSkin);
  FreeAndNil(FSkinsList);
  inherited;
end;

procedure TSpTBXSkinManager.AddSkinNotification(AObject: TObject);
begin
  if FNotifies.IndexOf(AObject) < 0 then FNotifies.Add(AObject);
end;

procedure TSpTBXSkinManager.RemoveSkinNotification(AObject: TObject);
begin
  FNotifies.Remove(AObject);
end;

procedure TSpTBXSkinManager.Broadcast;
var
  Msg: TMessage;
  I: Integer;
begin
  if FNotifies.Count > 0 then begin
    Msg.Msg := WM_SPSKINCHANGE;
    Msg.WParam := 0;
    Msg.LParam := 0;
    Msg.Result := 0;
    for I := 0 to FNotifies.Count - 1 do
      TObject(FNotifies[I]).Dispatch(Msg);
  end;
end;

procedure TSpTBXSkinManager.BroadcastSkinNotification;
begin
  Broadcast;
end;

procedure TSpTBXSkinManager.LoadFromFile(Filename: WideString);
begin
  FCurrentSkin.LoadFromFile(Filename);
  Broadcast;
end;

procedure TSpTBXSkinManager.SaveToFile(Filename: WideString);
begin
  FCurrentSkin.SaveToFile(Filename);
end;

function TSpTBXSkinManager.GetCurrentSkinName: string;
begin
  Result := FCurrentSkin.SkinName;
end;

function TSpTBXSkinManager.GetSkinType: TSpTBXSkinType;
begin
  Result := SpTBXSkinType(sknSkin);
end;

function TSpTBXSkinManager.IsDefaultSkin: Boolean;
begin
  Result := CurrentSkinName = 'Default';
end;

function TSpTBXSkinManager.IsXPThemesEnabled: Boolean;
begin
  Result := ThemeServices.ThemesAvailable and UxTheme.UseThemes;
end;

procedure TSpTBXSkinManager.SetSkin(SkinName: string);
var
  I: Integer;
  K: TSpTBXSkinsListEntry;
begin
  if SameText(SkinName, 'Default') then
    SetToDefaultSkin
  else begin
    I := FSkinsList.IndexOf(SkinName);
    if I > -1 then begin
      K := FSkinsList.SkinOptions[I];
      if Assigned(K.SkinClass) then begin
        FCurrentSkin.Free;
        FCurrentSkin := K.SkinClass.Create;
        Broadcast;
      end
      else
        if Assigned(K.SkinStrings) then begin
          FCurrentSkin.Free;
          FCurrentSkin := TSpTBXSkinOptions.Create;
          FCurrentSkin.LoadFromStrings(K.SkinStrings);
          Broadcast;
        end;
    end;
  end;
end;

procedure TSpTBXSkinManager.SetToDefaultSkin;
begin
  FCurrentSkin.Free;
  FCurrentSkin := TSpTBXSkinOptions.Create;
  Broadcast;
end;

procedure TSpTBXSkinManager.ChangeControlSkinType(Control: TWinControl;
  SkinType: TSpTBXSkinType; Recursive: Boolean = True);

  procedure ChangeSkinTypeProperty(Component: TComponent; TM: TSpTBXSkinType);
  var
    S: string;
    PropInfo: PPropInfo;
  begin
    if Length(Component.ClassName) > 6 then begin
      S := Copy(Component.ClassName, 1, 6);
      if SameText(S, 'TSpTBX') then begin
        PropInfo := GetPropInfo(Component, 'SkinType');
        if (PropInfo <> nil) and (PropInfo.PropType^.Kind = tkEnumeration) then
          SetOrdProp(Component, PropInfo, Integer(TM));
      end;
    end;
  end;

var
  I: Integer;
  C: TControl;
begin
  for I := 0 to Control.ControlCount - 1 do begin
    C := Control.Controls[I];
    ChangeSkinTypeProperty(C, SkinType);
    if Recursive and (C is TWinControl) then
      ChangeControlSkinType(C as TWinControl, SkinType, Recursive);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSkinSwitcher }

constructor TSpTBXSkinSwitcher.Create(AOwner: TComponent);
begin
  inherited;
  SkinManager.AddSkinNotification(Self);
end;

destructor TSpTBXSkinSwitcher.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

function TSpTBXSkinSwitcher.GetSkin: string;
begin
  Result := SkinManager.CurrentSkinName;
end;

procedure TSpTBXSkinSwitcher.SetSkin(const Value: string);
begin
  SkinManager.SetSkin(Value);
end;

procedure TSpTBXSkinSwitcher.WMSpSkinChange(var Message: TMessage);
begin
  if Assigned(FOnSkinChange) then FOnSkinChange(Self);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Stock Objects }

procedure InitializeStock;
begin
  StockBitmap := TBitmap.Create;
  StockBitmap.Width := 8;
  StockBitmap.Height := 8;

  @SpPrintWindow := GetProcAddress(GetModuleHandle(user32), 'PrintWindow');

  if not Assigned(FInternalSkinManager) then
    FInternalSkinManager := TSpTBXSkinManager.Create;
end;

procedure FinalizeStock;
begin
  FreeAndNil(StockBitmap);
  FreeAndNil(FInternalSkinManager);
end;

initialization
  InitializeStock;

finalization
  FinalizeStock;

end.
