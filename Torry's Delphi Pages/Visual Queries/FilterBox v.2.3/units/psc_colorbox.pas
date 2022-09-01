{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
Unit psc_colorbox;

Interface
{$I psc_defines.inc}

Uses
  dialogs,
  ImgList,
  graphics,
  winapi.commctrl,
  winapi.messages,
  classes,
  controls,
  winapi.windows,
  forms,
  SysUtils,

  myla_system,
  myla_interfaces,

  psc_wrapper,
  psc_procs,
  psc_const;

Const
  BS_UP = 1;
  BS_DISABLED = 2;
  BS_DOWN = 4;
  BS_EXCLUSIVE = 8;

  BA_DIALOG = 1;
  BA_SETBUTTONCOLOR = 2;
  BA_SETSELECTEDCOLOR = 4;
  BA_SETCUSTOMCOLOR = 8;

  EV_ITEMCHANGED = 1;
  EV_COLORCHANGED = 2;
  EV_ITEMDELETED = 4;
  EV_ITEMSTATECHANGED = 8;
  EV_SECTIONCHANGED = 16;
  EV_OWNERCHANGED = 32;

  PRM_OWNER = -1;
  PRM_VISIBLE = 0;
  PRM_COLOR = 1;
  PRM_BCKCOLOR = 2;
  PRM_BORDERCOLOR = 3;
  PRM_ALIGNMENT = 4;
  PRM_CAPTION = 5;
  PRM_HINT = 6;
  PRM_ENABLED = 7;
  PRM_SHOWCAPTION = 8;
  PRM_SHOWSEPARATOR = 9;
  PRM_SHOWCOLORS = 10;
  PRM_SLOTWIDTH = 11;
  PRM_SLOTHEIGHT = 12;
  PRM_COLUMNCOUNT = 13;
  PRM_HIDESELECTION = 14;
  PRM_HINTKIND = 15;
  PRM_ACTIVESLOT = 16;
  PRM_SELECTEDSLOT = 17;
  PRM_FONT = 18;
  PRM_CANVAS = 19;
  PRM_STATES = 20;
  PRM_BUTTONACTION = 21;
  PRM_DESIGNTIME = 22;
  PRM_KIND = 23;
  PRM_DEFAULTKIND = 24;
  PRM_DOWNACTIVESLOT = 25;
  PRM_CANREARANGE = 26;
  PRM_AUTOSIZE = 27;
  PRM_SELECTSAMECOLORS = 28;
  PRM_PRESSEDCOLOR = 29;
  PRM_HIGHLIGHT = 30;
  PRM_UPACTIVESLOT = 31;

  clPSCLimeStd = TPSCColor($00FF00);
  clPSCAquaStd = TPSCColor($FFFF00);

  clPSCTurquoise = clPSCAquaStd;
  clPSCGray25 = clPSCLtGray;
  clPSCDarkBlue = clPSCNavy;
  clPSCDarkRed = clPSCMaroon;
  clPSCDarkYellow = clPSCOlive;
  clPSCGray50 = clPSCDkGray;
  clPSCViolet = clPSCPurple;
  clPSCPink = clPSCFuchsia;
  clPSCBrightGreen = clPSCLimeStd;
  clPSCBrown = TPSCColor($003090);
  clPSCOliveGreen = TPSCColor($003030);
  clPSCDarkGreen = TPSCColor($003000);
  clPSCDarkTeal = TPSCColor($603000);
  clPSCIndigo = TPSCColor($903030);
  clPSCGray80 = TPSCColor($303030);
  clPSCOrange = TPSCColor($0068FF);
  clPSCBlueGray = TPSCColor($906060);
  clPSCLightOrange = TPSCColor($0098FF);
  clPSCLime = TPSCColor($00C8A0);
  clPSCSeaGreen = TPSCColor($609830);
  clPSCAqua = TPSCColor($C0C830);
  clPSCLightBlue = TPSCColor($FF6830);
  clPSCGray40 = TPSCColor($909890);
  clPSCGold = TPSCColor($00C8FF);
  clPSCSkyBlue = TPSCColor($FFC800);
  clPSCPlum = TPSCColor($603090);
  clPSCRose = TPSCColor($D098FF);
  clPSCTan = TPSCColor($A0C8FF);
  clPSCLightYellow = TPSCColor($90FFFF);
  clPSCLightGreen = TPSCColor($D0FFD0);
  clPSCLightTurquoise = TPSCColor($FFFFC0);
  clPSCPaleBlue = TPSCColor($FFC890);
  clPSCLavender = TPSCColor($FF98C0);

  clPSCEmptySlotColor = clPSCWhite;

  clPSCHotLight = TPSCColor(COLOR_HOTLIGHT Or $80000000);
  clPSCGradientActiveCaption = TPSCColor(COLOR_GRADIENTACTIVECAPTION Or $80000000)
    ;
  clPSCGradientInactiveCaption =
    TPSCColor(COLOR_GRADIENTINACTIVECAPTION Or $80000000);

  cPSCColors: Array[0..39] Of TPSCColor = (
    clPSCBlack,clPSCBrown,clPSCOliveGreen,clPSCDarkGreen,clPSCDarkTeal,
    clPSCDarkBlue,clPSCIndigo,clPSCGray80,clPSCDarkRed,clPSCOrange,
    clPSCDarkYellow,clPSCGreen,clPSCTeal,clPSCBlue,clPSCBlueGray,clPSCGray50,
    clPSCRed,clPSCLightOrange,clPSCLime,clPSCSeaGreen,clPSCAqua,clPSCLightBlue,
    clPSCViolet,clPSCGray40,clPSCPink,clPSCGold,clPSCYellow,clPSCBrightGreen,
    clPSCTurquoise,clPSCSkyBlue,clPSCPlum,clPSCGray25,clPSCRose,clPSCTan,
    clPSCLightYellow,clPSCLightGreen,clPSCLightTurquoise,clPSCPaleBlue,
    clPSCLavender,clPSCWhite
  );

  cPSCStdColorCount = 16;
  cPSCStdColors: Array[0..cPSCStdColorCount - 1] Of TPSCColor = (
    clPSCBlack,clPSCWhite,clPSCGreen,clPSCMaroon,clPSCOlive,clPSCNavy,
    clPSCPurple,clPSCDkGray,clPSCYellow,clPSCLime,clPSCAqua,clPSCFuchsia,
    clPSCLtGray,clPSCRed,clPSCBlue,clPSCTeal);

  cPSCBkgndColorCount = 15;
  cPSCBkgndColors: Array[0..cPSCBkgndColorCount - 1] Of TPSCColor = (
    clPSCYellow,clPSCLime,clPSCAqua,clPSCFuchsia,clPSCBlue,
    clPSCRed,clPSCNavy,clPSCGreen,clPSCTeal,clPSCPurple,
    clPSCMaroon,clPSCOlive,clPSCDkGray,clPSCLtGray,clPSCBlack);

  cPSCMaxIdentMapArraySize = MaxInt Div SizeOf(TPSCIdentMapEntry);

  CM_FLATCHANGED = CM_BASE + 101;
  CM_ACTIVECHANGED = CM_BASE + 102;

type
  PPSCColorData = ^TPSCColorData;
  TPSCColorData = Packed Record
    Size: Integer;
    Color: TPSCColor;
    Hint: String;
    Caption: String;
    Cookie: Integer;
  End;

Const
  ColorDataMaxSize = MaxInt Div SizeOf(TPSCColorData);

Type
  TPSCColorDatas = Array[0..ColorDataMaxSize - 1] Of TPSCColorData;

  TPSCColorStrKind = (ckDecimal,ckHex,ckRGBPercents,ckRGBIntPercents,
    ckRGBDecimal,ckRGBHex,ckHexTriple);

  TPSCColorsEntry = class
  public
    MapArray: Array of TPSCIdentMapEntry;
  End;

  TPSCColors = Class(TPersistent)
  private
    FList: IPSCObjectList;
  public
    Constructor Create;

    Procedure RegisterIdentMap(Const Map: Array Of TPSCIdentMapEntry);
  public
    Procedure GetColors(Proc: TPSCGetStrProc);

    Function ColorToString(Color: TPSCColor; Kind: TPSCColorStrKind): String;
    Function StringToColor(Const S: String): TPSCColor;
    Function ColorToRGBStr(Color: TPSCColor; Kind:TPSCColorStrKind): String;
    Function RGBStrToColor(Const S: String): TPSCColor;
  End;

  TPSCDirection = (sdLeft,sdUp,sdRight,sdDown);

  IPSCObject = Interface(IPSCInterface)
    ['{1349F140-E796-11D3-8E81-0040332EFA64}']
    Function GetInstance: TPersistent;
  End;

  IPSCDraw = Interface(IPSCInterface)
    ['{F6C619D2-AD83-11D3-BCE0-00E07D743236}']
    Procedure Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect);
  End;

  IPSCDataEnumerator = Interface(IPSCInterface)
    ['{0E8611C2-CC25-11D3-8E80-0040332EFA64}']
    Function Next(Count: Integer; var Buffer:TPSCColorDatas): Integer;
    Procedure Reset;
  End;

  IPSCColorsAccess = Interface(IPSCObject)
    ['{1EBCC201-C5D1-11D3-931B-0040332EBEE7}']
    Procedure SetEnumColor(Color: TPSCColor; Value: Boolean);
    Function GetEnumColor(Color: TPSCColor): Boolean;
    Function GetColorsEnum: IPSCDataEnumerator;
    Property Colors[Color: TPSCColor]: Boolean read GetEnumColor write
      SetEnumColor;
  End;

  IPSCColors = Interface(IPSCObject)
    ['{1EBCC202-C5D1-11D3-931B-0040332EBEE7}']
    Function GetColorsCount: Integer;
    Function GetColorItem(Index: Integer): IPSCColorsAccess;
    Property Count: Integer read GetColorsCount;
    Property Items[Index: Integer]: IPSCColorsAccess read GetColorItem;
  End;

  IPSCBounds = Interface(IPSCObject)
    ['{9AC19980-CF62-11D3-8E81-0040332EFA64}']
    Function GetBounds: TRect;
    Procedure SetBounds(Const V: TRect);
    Function GetVisible: boolean;
    Function GetAlign: TAlign;
    Function Measure(Var NewWidth,NewHeight: Integer): Boolean;
    Property Align: TAlign read GetAlign;
    Property BoundsRect: TRect read GetBounds write SetBounds;
    Property Visible: Boolean read GetVisible;
  End;

  IPSCList = Interface(IPSCInterface)
    ['{9AC19981-CF62-11D3-8E81-0040332EFA64}']
    Function GetItem(Index: Integer): IPSCInterface;
    Function GetCount: Integer;
  End;

  IPSCDrawList = Interface(IPSCList)
    ['{9AC19983-CF62-11D3-8E81-0040332EFA64}']
    Function GetItemAsDraw(Index: Integer): IPSCDraw;
  End;

  IPSCBoundsList = Interface(IPSCList)
    ['{9AC19984-CF62-11D3-8E81-0040332EFA64}']
    Function GetItemAsBounds(Index: Integer): IPSCBounds;
    Function GetItemAt(P: TPoint): IPSCInterface;
  End;

  IPSCBoundsAligner = Interface(IPSCInterface)
    ['{0E8611C4-CC25-11D3-8E80-0040332EFA64}']
    Procedure Realign(Container: IPSCObject; BoundsList: IPSCBoundsList);
  End;

  IPSCFocused = Interface(IPSCObject)
    ['{F916F7E0-DFD0-11D3-8E81-0040332EFA64}']
    Procedure SetFocused(Value: Boolean);
    Function GetFocused: Boolean;
    Property Focused: Boolean read GetFocused write SetFocused;
  End;

  IPSCSelect = Interface(IPSCFocused)
    ['{68DE7D60-E340-11D3-8E81-0040332EFA64}']
    Procedure Select;
    Procedure MoveSelection(Dir: TPSCDirection);
  End;

  IPSCKeyboardAccess = Interface(IPSCInterface)
    ['{0E8611C5-CC25-11D3-8E80-0040332EFA64}']
    Procedure KeyDown(Sender: IPSCObject; Var Key: Word; Shift: TShiftState);
    Procedure KeyUp(Sender: IPSCObject; Var Key: Word; Shift: TShiftState);
    Procedure KeyPress(Sender: IPSCObject; Var Key: Char);
  End;

  IPSCMouseAccess = Interface(IPSCInterface)
    ['{0E8611C6-CC25-11D3-8E80-0040332EFA64}']
    Procedure MouseEnter(Sender: IPSCObject);
    Procedure MouseLeave(Sender: IPSCObject);
    Procedure MouseDown(Sender: IPSCObject; Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer);
    Procedure MouseUp(Sender: IPSCObject; Button: TMouseButton;
      Shift: TShiftState;
      X,Y: Integer);
    Procedure MouseMove(Sender: IPSCObject; Shift: TShiftState; X,Y: Integer);
    Procedure MouseWheel(Sender: IPSCObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint);
  End;

  IPSCValidator = Interface(IPSCObject)
    ['{88EFBE60-D31A-11D3-8E81-0040332EFA64}']
    Function GetCanvas: TPSCCanvas;
    Procedure InvalidateRect(Rect: TRect; Erase: Boolean);
    Procedure Invalidate;
  End;

  IPSCColorDialog = Interface(IPSCObject)
    ['{913372A1-CC59-11D3-931B-0040332EBEE7}']
    Function Execute(Var AColor: TPSCColor): Boolean;
    procedure AddCustomColor(Color:TPSCColor);
    procedure ClearCustomColors;
  End;

  IPSCDataReader = Interface(IPSCObject)
    ['{079D6420-DFE0-11D3-8E81-0040332EFA64}']
    Function Get_Boolean(IDParam: integer): Boolean;
    Function Get_Cardinal(IDParam: integer): Cardinal;
    Function Get_Integer(IDParam: integer): longint;
    Function Get_String(IDParam: integer): String;
    Function Get_Object(IDParam: integer): TObject;
    Function Get_Double(IDParam: integer): Double;
    Function GetParent: IPSCDataReader;
    Procedure Event(Sender: IPSCObject; EvType:integer;LowData:TObject;HighData: integer);
  End;

  IPSCDataAccess = Interface(IPSCDataReader)
    ['{60A3B520-DFC1-11D3-8E81-0040332EFA64}']
    Procedure Set_Boolean(IDParam: integer; Value: Boolean);
    Procedure Set_Cardinal(IDParam: integer; Value: Cardinal);
    Procedure Set_Integer(IDParam: integer; Value: longint);
    Procedure Set_String(IDParam: integer; Const Value: String);
    Procedure Set_Object(IDParam: integer; Value: TObject);
    Procedure Set_Double(IDParam: integer; Value: Double);

    Property AsBoolean[IDParam: integer]: Boolean read Get_Boolean
    write Set_Boolean;
    Property AsCardinal[IDParam: integer]: Cardinal read Get_Cardinal
    write Set_Cardinal;
    Property AsInteger[IDParam: integer]: longint read Get_Integer
    write Set_Integer;
    Property AsString[IDParam: integer]: String read Get_String
    write Set_String;
    Property AsObject[IDParam: integer]: TObject read Get_Object
    write Set_Object;
    Property AsDouble[IDParam: integer]: Double read Get_Double
    write Set_Double;
    Property Parent: IPSCDataReader read GetParent;
  End;

  TPSCButtonState = (
    ButtonState_Up,
    ButtonState_Disabled,
    ButtonState_Down,
    ButtonState_Exclusive
  );

  TPSCButtonStates = Set Of TPSCButtonState;

  TPSCInterfacedPersistent = Class(TPSCIntfPersistent,IPSCObject,IPSCInterface)
  private
  protected
    Function GetInstance: TPersistent;
  public
    Constructor Create(AOwner: TPersistent); virtual;
  End;

  TPSCButtonsAction = (baDialog,baSetButtonColor,baSetSelectedColor,
    baSetColor,baSetCustomColor);
  TPSCButtonsActions = Set Of TPSCButtonsAction;

  TPSCHideSelection = (hsNotHide,hsHideSelection,hsHideExclusive);

  TPSCCustomSection = Class;
  TPSCGraphicItem = Class(TPSCNamedItem)
  private
    EventType: integer;
    FImage: IPSCObject;
    Function GetAsDraw: IPSCDraw;
    Function GetAsData: IPSCDataAccess;
    Function GetAsBounds: IPSCBounds;
    Function GetSection: TPSCCustomSection;
  protected
    Procedure DoCreateItem; virtual; abstract;
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
    Function CanSelect: Boolean;

    Property Bounds: IPSCBounds read GetAsBounds;
    Property Data: IPSCDataAccess read GetAsData;
    Property Image: IPSCDraw read GetAsDraw;
    Property Section: TPSCCustomSection read GetSection;
  End;

  TPSCColorSlot = Class(TPSCGraphicItem)
  private
    FDefaultHint: Word;
    Function GetColor: TPSCColor;
    Function GetHint: String;
    Function GetState: TPSCButtonStates;
    Function GetVisible: Boolean;
    Function IsHintStored: Boolean;
    Function GetSlotEnabled: Boolean;

    Procedure SetColor(Value: TPSCColor);
    Procedure SetHint(Const Value: String);
    Procedure SetState(Value: TPSCButtonStates);
    Procedure SetVisible(Value: Boolean);
    Procedure ReadHint(Reader: TReader);
    Procedure WriteHint(Writer: TWriter);
  protected
    Procedure DoCreateItem; override;
    Function GetDisplayName: String; override;
    Procedure DefineProperties(Filer: TFiler); override;
  public
    Property State: TPSCButtonStates read GetState write SetState;
    Property Enabled: Boolean read GetSlotEnabled;
  published
    Property Color: TPSCColor read GetColor write SetColor default clPSCNone;
    Property Hint: String read GetHint write SetHint stored IsHintStored;
    Property Visible: Boolean read GetVisible write SetVisible
      default true;
  End;

  TPSCDefaultSlot = Class(TPSCColorSlot)
  private
    FDefaultCaption: Word;
    Function GetCaption: String;
    Procedure SetCaption(Const Value: String);
    Procedure ReadCaption(Reader: TReader);
    Procedure WriteCaption(Writer: TWriter);
    Function IsCaptionStored: Boolean;
  protected
    Procedure DoCreateItem; override;
    Function GetDisplayName: String; override;
    Procedure DefineProperties(Filer: TFiler); override;
  published
    Property Caption: String read GetCaption write SetCaption
      stored IsCaptionStored;
  End;

  TPSCButtonSlot = Class(TPSCDefaultSlot)
  private
    Function GetAction: TPSCButtonsActions;
    Procedure SetAction(Value: TPSCButtonsActions);
  protected
    Procedure DoCreateItem; override;
  published
    Property SelectAction: TPSCButtonsActions read GetAction
      write SetAction default [baDialog,baSetColor,baSetCustomColor];
  End;

  TPSCGraphicItems = Class(TPSCNamedItems)
  protected
    Procedure Update(Item: TCollectionItem); override;
  public
  End;

  TPSCCustomSection = Class(
      TPSCInterfacedPersistent,IPSCDraw,IPSCBounds,
      IPSCBoundsList,IPSCDrawList,IPSCColorsAccess,
      IPSCDataReader,IPSCDataAccess
      )
  private
    FSection: IPSCObject;
    FParent: IPSCDataAccess;
    FValidator: IPSCValidator;
    FAligner: IPSCBoundsAligner;
    FSlots: TPSCGraphicItems;
    Function GetAlignment: TPSCAlignment;
    Function GetBorderColor: TPSCColor;
    Function GetEnabled: Boolean;
    Function GetShowSeparator: Boolean;
    Function GetHint: String;
    Function GetKind: Word;
    Function GetDefaultKind: Word;
    Function IsSlotsStored: Boolean;
    Function IsHintStored: Boolean;
    Function GetItemAsData(Index: Integer): IPSCDataAccess;

    Procedure SetAlignment(Value: TPSCAlignment);
    Procedure SetBorderColor(Value: TPSCColor);
    Procedure SetEnabled(Value: boolean);
    Procedure SetShowSeparator(Value: boolean);
    Procedure SetVisible(Value: boolean);
    Procedure SetHint(Const Value: String);
    Procedure SetKind(Value: Word);
    Procedure ReadDefaultKind(Reader: TReader);
    Procedure WriteDefaultKind(Writer: TWriter);
    Procedure SetDefaultKind(Value: Word);

    // IPSCDrawList
    Function GetCount: Integer;
    Function GetItem(Index: Integer): IPSCInterface;
    Function GetItemAsDraw(Index: Integer): IPSCDraw;
    // IPSCColorsAccess
    Function GetEnumColor(AColor: TPSCColor): Boolean;
    Function GetColorsEnum: IPSCDataEnumerator;
    Procedure SetEnumColor(AColor: TPSCColor; Value: Boolean);
    // IPSCBounds
    Function GetBounds: TRect;
    Function GetAlign: TAlign;
    Function GetVisible: Boolean;
    Procedure SetBounds(Const Value: TRect);
    // IPSCDraw
    Procedure Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect);
  protected
    // IPSCDataAccess
    Function Get_Boolean(IDParam: integer): Boolean; virtual;
    Function Get_Cardinal(IDParam: integer): Cardinal; virtual;
    Function Get_Integer(IDParam: integer): longint; virtual;
    Function Get_String(IDParam: integer): String; virtual;
    Function Get_Object(IDParam: integer): TObject; virtual;
    Function Get_Double(IDParam: integer): Double; virtual;
    Function GetParent: IPSCDataReader;
    Function GetDefaultCaption: String; virtual;
    Function IsKindStored: Boolean;

    Procedure Set_Boolean(IDParam: integer; Value: Boolean); virtual;
    Procedure Set_Cardinal(IDParam: integer; Value: Cardinal); virtual;
    Procedure Set_Integer(IDParam: integer; Value: longint); virtual;
    Procedure Set_String(IDParam: integer; Const Value: String); virtual;
    Procedure Set_Object(IDParam: integer; Value: TObject); virtual;
    Procedure Set_Double(IDParam: integer; Value: Double); virtual;
    Procedure Event(Sender: IPSCObject; EvType:integer;LowData:TObject;HighData: integer);
    Procedure CreateSlots; dynamic; abstract;
    Procedure CreateDefaultSlots; dynamic; abstract;
    Procedure DefineProperties(Filer: TFiler); override;

    Property Hint: String read GetHint write SetHint stored IsHintStored;
    Property ShowSeparator: Boolean read GetShowSeparator
      write SetShowSeparator default false;
    Property DefaultKind: Word read GetDefaultKind write SetDefaultKind;
  public
    Constructor Create(AOwner: TPersistent); override;
    Destructor Destroy; override;

    Procedure Assign(Source: TPersistent); override;
    Procedure BeginUpdate;
    Procedure EndUpdate;

    Function Add: TPSCGraphicItem;
    Function GetNextItem(Slot: IPSCDataAccess;
      Dir: TPSCDirection): IPSCDataAccess;virtual;
    Function IsDesignTime: Boolean;
    // IPSCBoundsList
    Function GetItemAsBounds(Index: Integer): IPSCBounds;
    Function GetItemAt(P: TPoint): IPSCInterface;
    Function Measure(Var NewWidth,NewHeight: Integer): Boolean;

    Property Alignment: TPSCAlignment read GetAlignment write SetAlignment
      default taLeftJustify;
    Property BorderColor: TPSCColor read GetBorderColor write SetBorderColor
      default clPSCBtnShadow;
    Property Aligner: IPSCBoundsAligner read FAligner write FAligner;
    Property Parent: IPSCDataAccess read FParent write FParent;
    Property Kind: Word read GetKind write SetKind;
    Property Section: IPSCObject read FSection write FSection;
    Property Count: integer read GetCount;
    Property Items[Index: integer]: IPSCDataAccess read GetItemAsData;
    Property Colors[Color: TPSCColor]: Boolean read GetEnumColor write
      SetEnumColor;
  published
    Property Enabled: boolean read GetEnabled write SetEnabled
      default true;
    Property Slots: TPSCGraphicItems read FSlots write FSlots
      stored IsSlotsStored;
    Property Visible: boolean read GetVisible write SetVisible
      default true;
  End;

  TPSCColorSlotsKind = (cskCustom,cskCustomColors,cskStdColors,
    cskDocColors,cskWinColors,cskBkgndColors,cskFontColors);

  TPSCColorSection = Class(TPSCCustomSection)
  private
    Function GetCaption: String;
    Procedure SetCaption(Const Value: String);
    Function GetShowCaption: Boolean;
    Procedure SetShowCaption(Value: boolean);
    Function GetKind: TPSCColorSlotsKind;
    Procedure SetKind(Value: TPSCColorSlotsKind);
    Function IsCaptionStored: Boolean;
  protected
    Procedure CreateSlots; override;
    Procedure CreateDefaultSlots; override;
    // IPSCDataAccess
    Function Get_Boolean(IDParam: integer): Boolean; override;
    Procedure Set_Boolean(IDParam: integer; Value: Boolean); override;
    Function Get_String(IDParam: integer): String; override;
    Procedure Set_String(IDParam: integer; Const Value: String); override;
    Function GetDefaultCaption: String; override;
  public
    Constructor Create(AOwner: TPersistent); override;
    Function GetNextItem(Slot: IPSCDataAccess; Dir: TPSCDirection)
      : IPSCDataAccess;
      override;
  published
    Property ShowCaption: Boolean read GetShowCaption write SetShowCaption
      default true;
    Property Kind: TPSCColorSlotsKind read GetKind write SetKind
      stored IsKindStored;
    Property Caption: String read GetCaption write SetCaption
      stored IsCaptionStored;

    Property Alignment;
    Property BorderColor;
    Property Hint;
    Property ShowSeparator;
  End;

  TPSCDefaultSlotsKind = (dskCustom,dskAuto,dskNone);

  TPSCDefaultSection = Class(TPSCCustomSection)
  private
    Function GetShowColors: Boolean;
    Procedure SetShowColors(Value: boolean);
    Procedure SetKind(Value: TPSCDefaultSlotsKind);
    Function GetKind: TPSCDefaultSlotsKind;
  protected
    Procedure CreateSlots; override;
    Procedure CreateDefaultSlots; override;
    // IPSCDataAccess
    Function Get_Boolean(IDParam: integer): Boolean; override;
    Procedure Set_Boolean(IDParam: integer; Value: Boolean); override;
  public
    Constructor Create(AOwner: TPersistent); override;
  published
    Property ShowColors: boolean read GetShowColors write SetShowColors
      default true;
    Property Kind: TPSCDefaultSlotsKind read GetKind write SetKind
      stored IsKindStored;

    Property Alignment;
    Property BorderColor;
    Property Hint;
    Property ShowSeparator;
  End;

  TPSCButtonSlotsKind = (bskCustom,bskMoreColors);

  TPSCButtonSection = Class(TPSCDefaultSection)
  private
    Procedure SetKind(Value: TPSCButtonSlotsKind);
    Function GetKind: TPSCButtonSlotsKind;
  protected
    Procedure CreateSlots; override;
    Procedure CreateDefaultSlots; override;
  public
    Constructor Create(AOwner: TPersistent); override;
  published
    Property Kind: TPSCButtonSlotsKind read GetKind write SetKind
      stored IsKindStored;
  End;

  TPSCCustomSectionControl = Class(TControl,IPSCInterface,IPSCValidator)
  private
    FCanvas: TPSCCanvas;
    Procedure WMPaint(Var Message: TWMPaint); message WM_PAINT;
    Procedure IPSCValidator.InvalidateRect = InvalidateRegion;
    Procedure IPSCValidator.Invalidate = InvalidateControl;
    Procedure InvalidateRegion(Rect: TRect; Erase: Boolean);
    Procedure InvalidateControl; virtual;
  protected
    FSection: IPSCObject;
    Procedure Paint; virtual;
    Procedure CreateSection; dynamic; abstract;
    Procedure RequestAlign; override;
    Function GetInstance: TPersistent;
    Function GetCanvas: TPSCCanvas;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  published
    Property Left stored false;
    Property Top stored false;
    Property Width stored false;
    Property Height stored false;
  End;

  TPSCDefaultSectionControl = Class(TPSCCustomSectionControl)
  private
    Function GetSection: TPSCDefaultSection;
    Procedure SetSection(Value: TPSCDefaultSection);
  protected
    Procedure CreateSection; override;
  published
    Property Section: TPSCDefaultSection read GetSection write SetSection;
  End;

  TPSCButtonSectionControl = Class(TPSCCustomSectionControl)
  private
    Function GetSection: TPSCButtonSection;
    Procedure SetSection(Value: TPSCButtonSection);
  protected
    Procedure CreateSection; override;
  published
    Property Section: TPSCButtonSection read GetSection write SetSection;
  End;

  TPSCColorSectionControl = Class(TPSCCustomSectionControl)
  private
    Function GetSection: TPSCColorSection;
    Procedure SetSection(Value: TPSCColorSection);
  protected
    Procedure CreateSection; override;
  published
    Property Section: TPSCColorSection read GetSection write SetSection;
  End;

  TPSCButtonClickStage = (bcsBeforeExecute,bcsAfterExecute);

  TPSCCustomColorBox = Class;
  TPSCColorSlotEvent = Procedure(Sender: TObject;
    ASlot: TPSCColorSlot) Of Object;
  TPSCButtonSlotEvent = Procedure(Sender: TObject;
    ASlot: TPSCColorSlot; AColor: TPSCColor; AStage: TPSCButtonClickStage) Of
      Object;

  TPSCColorBoxOption = (cboShowDefaultAuto,cboShowDefaultNone,
    cboShowCustomColors,cboShowStdColors,cboShowDocColors,
    cboShowWinColors,cboShowBkgndColors,cboShowFontColors,
    cboShowMoreColors);
  TPSCColorBoxOptions = Set Of TPSCColorBoxOption;

  TPSCColorBoxState = Set Of (cbsUpdatingNeeded,cbsAdjustingNeeded,
    cbsFastHint,cbsSkipMouseMove,cbsSizing);

  TPSCColorBoxStyle = (cbsCustom,cbsWordBk,cbsWordFont,cbsFrontPage,
    cbsSysColors,cbsFrontPageBtn);

  TPSCCustomColorBox = Class(
      TWinControl,IPSCInterface,IPSCObject,
      IPSCDraw,IPSCBounds,IPSCBoundsList,
      IPSCSelect,IPSCColors,IPSCDataReader,
      IPSCDataAccess
      )
  private
    FMouse: IPSCMouseAccess;
    FKeyboard: IPSCKeyboardAccess;
    FAligner: IPSCBoundsAligner;
    FColorDialog: IPSCColorDialog;
    FIntfParent: IPSCDataReader;
    FReadOnly: Boolean;
    FUpdateCount: integer;
    FStatus: String;
    FState: TPSCColorBoxState;
    FPressedColor: TPSCColor;
    FSelectedColor: TPSCColor;
    FSelectedSlot: TPSCColorSlot;
    FActiveSlot: TPSCColorSlot;
    FColumnCount: Integer;
    FSlotWidth: Integer;
    FSlotHeight: Integer;
    FCanRearrange: Boolean;
    FSelectSameColors: Boolean;
    FHintKind: TPSCColorStrKind;
    FHideSelection: TPSCHideSelection;
    FOnChange: TPSCColorSlotEvent;
    FOnActiveChange: TPSCColorSlotEvent;
    FOnSelected: TPSCColorSlotEvent;
    FOnButtonClick: TPSCButtonSlotEvent;
    FOnSlotDeletion: TPSCColorSlotEvent;
    FHighlightActive: Boolean;
    FHighlightColor: TPSCColor;
    FStyle: TPSCColorBoxStyle;
    FDialogWasExecuted: Boolean;

    Function GetNextItem(Slot: IPSCDataAccess;
      Dir: TPSCDirection): IPSCDataAccess;
    Function GetItemAsSection(Index: Integer): TPSCCustomSection;
    Function GetDefaultDialog: TPersistent;
    Function GetStyle: TPSCColorBoxStyle;
    Function GetOptions: TPSCColorBoxOptions;
    Function GetInstance: TPersistent;

    Procedure SetHighlightActive(Value: Boolean);
    Procedure SetHighlightColor(Value: TPSCColor);
    Procedure SetCanRearrange(Value: Boolean);
    Procedure SetActiveSlot(Value: TPSCColorSlot);
    Procedure SetSelectedSlot(Value: TPSCColorSlot);
    Procedure SetReadOnly(Value: Boolean);
    Procedure SetPressedColor(Value: TPSCColor);
    Procedure SetDefaultDialog(Value: TPersistent);
    Procedure SetStyle(Style: TPSCColorBoxStyle);
    Procedure SetOptions(Value: TPSCColorBoxOptions);
    Procedure SetColumnCount(Value: Integer);
    Procedure SetSlotWidth(Value: Integer);
    Procedure SetSlotHeight(Value: Integer);
    Procedure SetSelectSameColors(Value: Boolean);
    Procedure SetSelectedColor(Value: TPSCColor);
    Procedure SetHideSelection(Value: TPSCHideSelection);
    Procedure CMControlListChange(Var Message: TCMControlListChange);
      message CM_CONTROLLISTCHANGE;
    Procedure CMMouseEnter(Var Message: TMessage); message CM_MOUSEENTER;
    Procedure CMMouseLeave(Var Message: TMessage); message CM_MOUSELEAVE;
    Procedure CMFontChanged(Var Message: TMessage); message CM_FONTCHANGED;
    Procedure CMColorChanged(Var Message: TMessage); message CM_COLORCHANGED;
    Procedure CMCancelMode(Var Message: TWMCancelMode); message CM_CANCELMODE;
    Procedure CMHintShowPause(Var Message: TCMHintShowPause);
      message CM_HINTSHOWPAUSE;
    Procedure CMHintShow(Var Message: TCMHintShow); message CM_HINTSHOW;
    Procedure WMNCHitTest(Var Message: TWMNCHitTest); message WM_NCHITTEST;
    Procedure WMGetDlgCode(Var Message: TMessage); message WM_GETDLGCODE;
    Procedure WMEnable(Var Message: TWMEnable); message WM_ENABLE;
    Procedure WMPaint(Var Message: TWMPaint); message WM_PAINT;
    Procedure WMWindowPosChanging(Var Message: TWMWindowPosChanging);
      message WM_WINDOWPOSCHANGING;

    // IPSCDraw
    Procedure Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect);
    // IPSCBounds
    Function IPSCBounds.GetAlign = GetIntfAlign;
    Function IPSCBounds.GetVisible = GetIntfVisible;
    Function IPSCBounds.GetBounds = GetIntfBounds;
    Procedure IPSCBounds.SetBounds = SetIntfBounds;
    Function GetIntfAlign: TAlign;
    Function GetIntfVisible: Boolean;
    Function GetIntfBounds: TRect;
    Procedure SetIntfBounds(Const Value: TRect);
    // IPSCBoundsList
    Function GetItem(Index: Integer): IPSCInterface;
    Function GetCount: Integer;
    Function GetItemAsBounds(Index: Integer): IPSCBounds;
    Function GetItemAt(P: TPoint): IPSCInterface;
    // IPSCSelect
    Procedure SetFocused(Value: Boolean);
    Function GetFocused: Boolean;
    Procedure Select;
    Procedure MoveSelection(Dir: TPSCDirection);
    // IPSCColors
    Function GetColorsCount: Integer;
    Function GetColorItem(Index: Integer): IPSCColorsAccess;

    Procedure SetUpdating(Value: Boolean);
    Procedure UpdateSelection;
  protected
    // IPSCDataAccess
    Function Get_Boolean(IDParam: integer): Boolean; virtual;
    Function Get_Cardinal(IDParam: integer): Cardinal; virtual;
    Function Get_Integer(IDParam: integer): longint; virtual;
    Function Get_String(IDParam: integer): String; virtual;
    Function Get_Object(IDParam: integer): TObject; virtual;
    Function Get_Double(IDParam: integer): Double; virtual;
    Function GetParent: IPSCDataReader; virtual;
    function GetDialogWasExecuted: Boolean;

    Procedure Event(Sender: IPSCObject; EvType:integer;LowData:TObject;HighData: integer);
    Procedure Set_Boolean(IDParam: integer; Value: Boolean); virtual;
    Procedure Set_Cardinal(IDParam: integer; Value: Cardinal); virtual;
    Procedure Set_Integer(IDParam: integer; Value: longint); virtual;
    Procedure Set_String(IDParam: integer; Const Value: String); virtual;
    Procedure Set_Object(IDParam: integer; Value: TObject); virtual;
    Procedure Set_Double(IDParam: integer; Value: Double); virtual;

    Procedure Loaded; override;
    Procedure CreateWnd; override;
    Procedure WndProc(Var Message: TMessage); override;
    Function CanResize(Var NewWidth,NewHeight: Integer): Boolean; override;
    Function CanAutoSize(Var NewWidth,NewHeight: Integer): Boolean; override;
    Procedure AdjustSize; override;
    Procedure AlignControls(AControl: TControl; Var Rect: TRect); override;
    Procedure DoChanged(Slot: TPSCColorSlot);
    Procedure DoButtonClick(Slot: TPSCColorSlot; Action: TPSCButtonsActions);

    Procedure DoEnter; override;
    Procedure DoExit; override;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    Procedure KeyUp(Var Key: Word; Shift: TShiftState); override;
    Procedure KeyPress(Var Key: Char); override;
    Procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer); override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer); override;
    Procedure PaintWindow(DC: HDC); override;

    Property DefaultDialog: TPersistent read GetDefaultDialog
      write SetDefaultDialog;
    Property HideSelection: TPSCHideSelection read FHideSelection
      write SetHideSelection default hsNotHide;


  public
    Property AutoSize;
    Property HintKind: TPSCColorStrKind read FHintKind write FHintKind
      default ckHexTriple;
    Property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure Initialize; dynamic;
    Procedure BeginUpdate;
    Procedure EndUpdate;
    Function Measure(Var NewWidth,NewHeight: Integer): Boolean; virtual;
    Function IndexOfSlots(Section: TPSCCustomSection): Integer;

    Property Mouse: IPSCMouseAccess read FMouse write FMouse;
    Property Keyboard: IPSCKeyboardAccess read FKeyboard write FKeyboard;
    Property Aligner: IPSCBoundsAligner read FAligner write FAligner;
    Property ColorDialog: IPSCColorDialog read FColorDialog write FColorDialog;
    Property IntfParent: IPSCDataReader read FIntfParent write FIntfParent;

    Property UpdateCount: integer read FUpdateCount;
    Property SectionsCount: Integer read GetCount;
    Property SelectedSlot: TPSCColorSlot read FSelectedSlot
      write SetSelectedSlot;
    Property ActiveSlot: TPSCColorSlot read FActiveSlot write SetActiveSlot;
    Property Sections[Index: Integer]: TPSCCustomSection read GetItemAsSection;
  published
    Property PressedColor: TPSCColor read FPressedColor write SetPressedColor
      default clPSCBtnHighlight;
    Property SelectSameColors: Boolean read FSelectSameColors
      write SetSelectSameColors default true;
    Property Style: TPSCColorBoxStyle read GetStyle write SetStyle stored false;
    Property SectionsSet: TPSCColorBoxOptions read GetOptions write SetOptions
      stored false;
    Property SlotWidth: Integer read FSlotWidth write SetSlotWidth
      default 18;
    Property SlotHeight: Integer read FSlotHeight write SetSlotHeight
      default 18;
    Property ColumnCount: Integer read FColumnCount write SetColumnCount
      default 8;
    Property CanRearrangeSlots: Boolean read FCanRearrange
      write SetCanRearrange default true;
    Property HighlightActive: Boolean read FHighlightActive
      write SetHighlightActive default false;
    Property HighlightColor: TPSCColor read FHighlightColor
      write SetHighlightColor default clPSCHighlight;
    Property SelectedColor: TPSCColor read FSelectedColor write SetSelectedColor
      default clPSCNone;
    Property OnChange: TPSCColorSlotEvent read FOnChange write FOnChange;
    Property OnActiveChange: TPSCColorSlotEvent read FOnActiveChange
      write FOnActiveChange;
    Property OnSelected: TPSCColorSlotEvent read FOnSelected
      write FOnSelected;
    Property OnButtonClick: TPSCButtonSlotEvent read FOnButtonClick
      write FOnButtonClick;
    Property OnSlotDeletion: TPSCColorSlotEvent read FOnSlotDeletion
      write FOnSlotDeletion;
    Property OnClick;
  End;

  TPSCColorBox = Class(TPSCCustomColorBox)
  published
    Property Align;
    Property Action;
    Property AutoSize default true;
    Property Anchors;
    Property BiDiMode;
    Property BorderWidth default 2;
    Property Constraints;
    Property DockSite;
    Property DragKind;
    Property ParentBiDiMode;
    Property Color;
    Property DragCursor;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property ParentShowHint default false;
    Property ShowHint default true;
    Property TabStop default true;
    Property ParentColor;
    Property ParentFont;
    Property PopupMenu;
    Property TabOrder;
    Property Visible;
    Property DefaultDialog;
    Property HintKind;
    Property HideSelection;
    Property ReadOnly;
    Property OnClick;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnDockDrop;
    Property OnDockOver;
    Property OnEndDock;
    Property OnResize;
    Property OnGetSiteInfo;
    Property OnStartDock;
    Property OnUnDock;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnStartDrag;
  End;

{-------------------------------------}

Function PSCColorToBorderColor(AColor: TPSCColor; ASelected: Boolean): TPSCColor;
Procedure PSCDrawButton(Canvas: TPSCCanvas; Const Rect: TRect;
  BackgroundColor,ForegroundColor: TPSCColor; State: TPSCButtonStates);
Function PSCColorsManager: TPSCColors;

{-------------------------------------}

Implementation

{-------------------------------------}

type
  TPSCColorDlg = Class(TPSCInterfacedPersistent,IPSCColorDialog)
  private
    FDialog: TPSCColorDialog;
    Function GetOptions: TColorDialogOptions;
    Procedure SetOptions(Value: TColorDialogOptions);
    Function GetHelpContext: THelpContext;
    Procedure SetHelpContext(Value: THelpContext);
  public
    procedure AddCustomColor(Color:TPSCColor);
    procedure ClearCustomColors;
    Destructor Destroy; override;
    Function GetProp: TPSCColorDialog;
    Function Execute(Var AColor: TPSCColor): Boolean;
  published
    Property HelpContext: THelpContext read GetHelpContext
      write SetHelpContext default 0;
    Property Options: TColorDialogOptions read GetOptions
      write SetOptions default [];
  End;

  TPSCEnumColors = Class(TPSCInterfacedPersistent,IPSCDataEnumerator)
  private
    FIndex: integer;
  public
    Constructor Create(Section: TPersistent; Index: integer); reintroduce;
    Function Next(Count: Integer; var Buffer:TPSCColorDatas): Integer;
    Procedure Reset;
  End;

  TPSCAlignerSink = Class(TPSCInterfacedPersistent,IPSCBoundsAligner)
  public
    Procedure Realign(Container: IPSCObject; BoundsList: IPSCBoundsList);
      virtual;
  End;

  TPSCAlignerSink2 = Class(TPSCAlignerSink)
  public
    Procedure Realign(Container: IPSCObject; BoundsList: IPSCBoundsList);
      override;
  End;

  TPSCAlignerSink3 = Class(TPSCAlignerSink)
  public
    Procedure Realign(Container: IPSCObject; BoundsList: IPSCBoundsList);
      override;
  End;

  TPSCKeyboardSink = Class(TPSCInterfacedPersistent,IPSCKeyboardAccess)
  public
    Procedure KeyDown(Sender: IPSCObject; Var Key: Word; Shift: TShiftState);
    Procedure KeyUp(Sender: IPSCObject; Var Key: Word; Shift: TShiftState);
    Procedure KeyPress(Sender: IPSCObject; Var Key: Char);
  End;

  TPSCMouseSink = Class(TPSCInterfacedPersistent,IPSCMouseAccess)
  public
    Procedure MouseEnter(Sender: IPSCObject);
    Procedure MouseLeave(Sender: IPSCObject);
    Procedure MouseDown(Sender: IPSCObject; Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer);
    Procedure MouseUp(Sender: IPSCObject; Button: TMouseButton;
      Shift: TShiftState;
      X,Y: Integer);
    Procedure MouseMove(Sender: IPSCObject; Shift: TShiftState; X,Y: Integer);
    Procedure MouseWheel(Sender: IPSCObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint);
  End;

  TPSCDrawSlot = Class(
      TPSCInterfacedPersistent,IPSCDraw,IPSCBounds,
      IPSCDataReader,IPSCDataAccess,IPSCInterface
      )
  private
    FLinkObject: TObject;
    FSource: IPSCDataReader;
    FIndex: LongInt;
    FBounds: TRect;
    FState: TPSCButtonStates;
    FVisible: Boolean;
    FColor: TPSCColor;
    FHint: String;
    Function GetBorderColor: TPSCColor;
    Function GetBackgroundColor: TPSCColor;
    Function GetForegroundColor: TPSCColor;
  protected
    // IPSCDraw
    Procedure Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect); virtual;
    // IPSCBounds
    Function GetBounds: TRect; virtual;
    Procedure SetBounds(Const V: TRect);
    Function GetAlign: TAlign;
    Function GetVisible: Boolean;
    Function Measure(Var NewWidth,NewHeight: Integer): Boolean; virtual;
    // IPSCDataAccess
    Function Get_Boolean(IDParam: integer): Boolean; virtual;
    Function Get_Cardinal(IDParam: integer): Cardinal; virtual;
    Function Get_Integer(IDParam: integer): longint; virtual;
    Function Get_String(IDParam: integer): String; virtual;
    Function Get_Object(IDParam: integer): TObject; virtual;
    Function Get_Double(IDParam: integer): Double; virtual;
    Function GetParent: IPSCDataReader; virtual;
    Procedure Event(Sender: IPSCObject; EvType:integer;LowData:TObject;HighData: integer);virtual;
    Procedure Set_Boolean(IDParam: integer; Value: Boolean); virtual;
    Procedure Set_Cardinal(IDParam: integer; Value: Cardinal); virtual;
    Procedure Set_Integer(IDParam: integer; Value: longint); virtual;
    Procedure Set_String(IDParam: integer; Const Value: String); virtual;
    Procedure Set_Object(IDParam: integer; Value: TObject); virtual;
    Procedure Set_Double(IDParam: integer; Value: Double); virtual;

    Function GetEnabled: Boolean; virtual;
  public
    Constructor Create(AOwner: TPersistent); override;
    Property BackgroundColor: TPSCColor read GetBackgroundColor;
    Property BorderColor: TPSCColor read GetBorderColor;
    Property Color: TPSCColor read FColor;
    Property Enabled: Boolean read GetEnabled;
    Property ForegroundColor: TPSCColor read GetForegroundColor;
    Property Hint: String read FHint;
    Property State: TPSCButtonStates read FState;
    Property Visible: Boolean read FVisible;
  End;

  TPSCDrawSection = Class(TPSCDrawSlot)
  private
    FUpdateCount: integer;
    FList: IPSCDrawList;
    FAlignment: TAlignment;
    FBorderColor: TPSCColor;
    FBackgroundColor: TPSCColor;
    FEnabled: Boolean;
    FShowSeparator: Boolean;
    FCaption: String;
    FVisibleCount: integer;
    FKind,FDefaultKind: Cardinal;
    Function GetFont: TPSCFont;
    Function GetCanvas: TPSCCanvas;
    Function GetVisibleCount: integer;
    Function GetVisible: Boolean;
  protected
    // IPSCBounds
    Function GetBounds: TRect; override;
    Function Measure(Var NewWidth,NewHeight: Integer): Boolean; override;
    // IPSCDataAccess
    Function Get_Boolean(IDParam: integer): Boolean; override;
    Function Get_Cardinal(IDParam: integer): Cardinal; override;
    Function Get_String(IDParam: integer): String; override;
    Function Get_Object(IDParam: integer): TObject; override;
    Procedure Set_Boolean(IDParam: integer; Value: Boolean); override;
    Procedure Set_Cardinal(IDParam: integer; Value: Cardinal); override;
    Procedure Set_String(IDParam: integer; Const Value: String); override;
    Procedure Event(Sender: IPSCObject; EvType:integer;LowData:TObject;HighData: integer);override;
    Function GetParent: IPSCDataReader; override;
    // IPSCDraw
    Procedure Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect); override;
    Procedure DrawSeparator(Canvas: TPSCCanvas; Const BoundsRect: TRect);

    Function GetEnabled: Boolean; override;
  public
    Constructor Create(AOwner: TPersistent); override;
    Function IsDesignTime: Boolean;
    Property Alignment: TAlignment read FAlignment;
    Property BackgroundColor: TPSCColor read FBackgroundColor;
    Property Caption: String read FCaption;
    Property Enabled: Boolean read GetEnabled;
    Property ShowSeparator: Boolean read FShowSeparator;
    Property Visible: Boolean read GetVisible;
    Property VisibleCount: integer read GetVisibleCount;
  End;

  TPSCDrawColorSection = Class(TPSCDrawSection)
  private
    FShowCaption: Boolean;
    Function GetSlotWidth: integer;
    Function GetSlotHeight: integer;
    Function GetColumnCount: Integer;
  protected
    // IPSCBounds
    Function Measure(Var NewWidth,NewHeight: Integer): Boolean; override;
    // IPSCDataAccess
    Function Get_Boolean(IDParam: integer): Boolean; override;
    Procedure Set_Boolean(IDParam: integer; Value: Boolean); override;
    // IPSCDraw
    Procedure Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect); override;
  public
    Property ColumnCount: Integer read GetColumnCount;
    Property SlotHeight: integer read GetSlotHeight;
    Property SlotWidth: integer read GetSlotWidth;
    Property ShowCaption: Boolean read FShowCaption;
  End;

  TPSCDrawButtonSection = Class(TPSCDrawSection)
  private
    FShowColors: Boolean;
  protected
    // IPSCBounds
    Function Measure(Var NewWidth,NewHeight: Integer): Boolean; override;
    // IPSCDataAccess
    Function Get_Boolean(IDParam: integer): Boolean; override;
    Procedure Set_Boolean(IDParam: integer; Value: Boolean); override;
    // IPSCDraw
    Procedure Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect); override;
  public
    Property ShowColors: Boolean read FShowColors;
  End;

  TPSCDrawDefault = Class(TPSCDrawSlot)
  private
    FCaption: String;
    Function GetAlignment: TAlignment;
    Function GetFont: TPSCFont;
    Function GetCanvas: TPSCCanvas;
    Function GetShowColors: Boolean;
  protected
    // IPSCBounds
    Function Measure(Var NewWidth,NewHeight: Integer): Boolean; override;
    // IPSCDataAccess
    Function Get_Cardinal(IDParam: integer): Cardinal; override;
    Function Get_String(IDParam: integer): String; override;
    Procedure Set_String(IDParam: integer; Const Value: String); override;
  public
    Procedure Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect); override;
    Property Caption: String read FCaption;
    Property Alignment: TAlignment read GetAlignment;
    Property ShowColors: Boolean read GetShowColors;
  End;

  TPSCDrawButton = Class(TPSCDrawDefault)
  private
    FAction: TPSCButtonsActions;
  protected
    // IPSCBounds
    Function Measure(Var NewWidth,NewHeight: Integer): Boolean; override;
    // IPSCDataAccess
    Function Get_Cardinal(IDParam: integer): Cardinal; override;
    Procedure Set_Cardinal(IDParam: integer; Value: Cardinal); override;
  public
    Constructor Create(AOwner: TPersistent); override;
    Procedure Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect); override;
    Property Action: TPSCButtonsActions read FAction;
  End;

Var
  KeyboardSink: IPSCKeyboardAccess;
  MouseSink: IPSCMouseAccess;

{-------------------------------------}

Type
  TintPersistent = Class(TPersistent)
  End;

Function PSCGetParentControl(Instance: TPersistent): TControl;
Begin
  If Instance = Nil Then
    Begin
      Result := Nil;
      exit
    End
  Else
  Try
    If Instance Is TControl Then
      Result := TControl(Instance)
    Else
      Result := PSCGetParentControl(TintPersistent(Instance).GetOwner)
  Except
    Result := Nil
  End
End;

{-------------------------------------}

Procedure PSCDrawButton(Canvas: TPSCCanvas; Const Rect: TRect;
  BackgroundColor,ForegroundColor: TPSCColor; State: TPSCButtonStates);
Var
  R: TRect;
Begin
  R := Rect;
  If ButtonState_Down In State Then
    State := [ButtonState_Down]
  Else
    If ButtonState_Up In State Then
      State := [ButtonState_Up]
    Else
      If ButtonState_Exclusive In State Then
        State := [ButtonState_Down,ButtonState_Exclusive];
  With Canvas Do
    Begin
      If ButtonState_Down In State Then
        DrawEdge(Handle,R,BDR_SUNKENOUTER,BF_RECT Or BF_ADJUST)
      Else
        If ButtonState_Up In State Then
          DrawEdge(Handle,R,BDR_RAISEDINNER,BF_RECT Or BF_ADJUST);
      If BackgroundColor = clPSCNone Then
        exit;
      With Brush Do
        If ButtonState_Exclusive In State Then
          Bitmap := PSCAllocPatternBitmap(BackgroundColor,ForegroundColor)
        Else
          Begin
            Color := BackgroundColor;
            Style := BrushStyle_Solid;
          End;
      FillRect(R)
    End
End;

{-------------------------------------}

Constructor TPSCInterfacedPersistent.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner);
End;

{----------------------------------------------------------}

Function TPSCInterfacedPersistent.GetInstance: TPersistent;
Begin
  Result := Self;
End;

{----------------------------------------------------------}

Constructor TPSCDrawSlot.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner);
  If AOwner <> Nil Then
    AOwner.GetInterface(IPSCDataReader,FSource);
  FVisible := true;
  FColor := clPSCNone;
  FIndex := -1;
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.GetEnabled: Boolean;
Begin
  Result := Not (ButtonState_Disabled In FState);
  If GetParent <> Nil Then
    Result := Result And GetParent.Get_Boolean(PRM_ENABLED)
End;

{----------------------------------------------------------}

Procedure TPSCDrawSlot.Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect);
Var
  Rect: TRect;
  BColor: TPSCColor;
  PColor: TPSCColor;
  BStyle: TBrushStyle;
  AState: TPSCButtonStates;
Begin
  If Visible And RectVisible(Canvas.Handle,BoundsRect) Then
    With Canvas Do
      Begin
        BColor := Brush.Color;
        BStyle := Brush.Style;
        PColor := Pen.Color;
        Try
          If Not Enabled Then
            AState := []
          Else
            AState := State;
          PSCDrawButton(
            Canvas,BoundsRect,BackgroundColor,ForegroundColor,AState
            );
          Rect := BoundsRect;
          If Enabled Then
            Begin
              InflateRect(Rect, -3, -3);
              If Color = clPSCDefault Then
                Brush.Style := BrushStyle_Clear
              Else
                Begin
                  Brush.Style := BrushStyle_Solid;
                  If Color = clPSCNone Then
                    Brush.Color := clPSCEmptySlotColor
                  Else
                    Brush.Color := Color
                End;
              Pen.Color := BorderColor;
              Rectangle(Rect.Left,Rect.Top,Rect.Right,Rect.Bottom)
            End
          Else
            Begin
              InflateRect(Rect, -2, -2);
              Rect.Left := Rect.Left + 1;
              Rect.Top := Rect.Top + 1;
              DrawEdge(Handle,Rect,BDR_SUNKENOUTER,BF_BOTTOMRIGHT);
              DrawEdge(Handle,Rect,BDR_RAISEDINNER,BF_TOPLEFT);
              OffsetRect(Rect, -1, -1);
              DrawEdge(Handle,Rect,BDR_SUNKENOUTER,BF_RECT Or BF_FLAT)
            End
        Finally
          Brush.Color := BColor;
          Brush.Style := BStyle;
          Pen.Color := PColor
        End
      End
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.GetBounds: TRect;
Begin
  Result := FBounds
End;

{----------------------------------------------------------}

Procedure TPSCDrawSlot.SetBounds(Const V: TRect);
Begin
  FBounds := V
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.GetAlign: TAlign;
Begin
  Result := alNone
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.GetVisible: Boolean;
Begin
  Result := FVisible;
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.Measure(Var NewWidth,NewHeight: Integer): Boolean;
Begin
  If GetParent <> Nil Then
    Begin
      NewWidth := GetParent.Get_Cardinal(PRM_SLOTWIDTH);
      NewHeight := GetParent.Get_Cardinal(PRM_SLOTHEIGHT);
      Result := Visible
    End
  Else
    Result := false
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.Get_Boolean(IDParam: integer): Boolean;
Begin
  Case IDParam Of
    PRM_VISIBLE:
      Result := FVisible;
    PRM_ENABLED:
      Result := GetEnabled;
  Else
    Result := false
  End
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.Get_Cardinal(IDParam: integer): Cardinal;
Begin
  Case IDParam Of
    PRM_COLOR:
      Result := Cardinal(FColor);
    PRM_BCKCOLOR:
      Result := Cardinal(GetBackgroundColor);
    PRM_PRESSEDCOLOR:
      Result := Cardinal(GetForegroundColor);
    PRM_BORDERCOLOR:
      Result := Cardinal(GetBorderColor);
    PRM_STATES:
      Result := byte(FState);
    PRM_COLUMNCOUNT:
      If GetParent <> Nil Then
        Result := GetParent.Get_Cardinal(IDParam)
      Else
        Result := 8;
    PRM_SLOTHEIGHT,
      PRM_SLOTWIDTH:
      If GetParent <> Nil Then
        Result := GetParent.Get_Cardinal(IDParam)
      Else
        Result := 18;
    PRM_ALIGNMENT:
      Begin
        If GetParent <> Nil Then
          Result := GetParent.Get_Cardinal(IDParam)
        Else
          Result := 0
      End
  Else
    Result := 0;
  End
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.Get_Integer(IDParam: integer): longint;
Begin
  Result := -1
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.Get_String(IDParam: integer): String;
Begin
  If IDParam = PRM_HINT Then
    Begin
      Result := FHint;
      If (GetParent <> Nil) And (Color <> clPSCNone) And
        (Color <> clPSCDefault) And (Result = '') Then
        Result := PSCColorsManager.ColorToString(
          Color,TPSCColorStrKind(GetParent.Get_Cardinal(PRM_HINTKIND))
          )
    End
  Else
    Result := ''
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.Get_Object(IDParam: integer): TObject;
Begin
  If IDParam = PRM_OWNER Then
    Result := FLinkObject
  Else
    Result := Nil
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.Get_Double(IDParam: integer): Double;
Begin
  Result := 0
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.GetParent: IPSCDataReader;
Begin
  Result := FSource
End;

{----------------------------------------------------------}

Procedure TPSCDrawSlot.Event(Sender: IPSCObject; EvType:integer;LowData:TObject;HighData: integer);
Begin
  If GetParent <> Nil Then
    GetParent.Event(GetParent,EvType,LowData,HighData)
End;

{----------------------------------------------------------}

Procedure TPSCDrawSlot.Set_Boolean(IDParam: integer; Value: Boolean);
Begin
  If (IDParam = PRM_VISIBLE) And (FVisible <> Value) Then
    Begin
      FVisible := Value;
      Event(Nil,EV_ITEMCHANGED,nil,0)
    End
End;

{----------------------------------------------------------}

Procedure TPSCDrawSlot.Set_Cardinal(IDParam: integer; Value: Cardinal);
Var
  AState: TPSCButtonStates;
Begin
  Case IDParam Of
    PRM_COLOR:
      If FColor <> TPSCColor(Value) Then
        Begin
          FColor := TPSCColor(Value);
          Event(Nil,EV_COLORCHANGED,Self,0)
        End;
    PRM_STATES:
      Begin
        AState := TPSCButtonStates(byte(Value));
        If FState <> AState Then
          Begin
            FState := AState;
            Event(Nil,EV_ITEMSTATECHANGED,Self,0)
          End
      End
  End
End;

{----------------------------------------------------------}

Procedure TPSCDrawSlot.Set_Integer(IDParam: integer; Value: longint);
Begin
End;

{----------------------------------------------------------}

Procedure TPSCDrawSlot.Set_String(IDParam: integer; Const Value: String);
Begin
  If IDParam = PRM_HINT Then
    FHint := Value
End;

{----------------------------------------------------------}

Procedure TPSCDrawSlot.Set_Object(IDParam: integer; Value: TObject);
Begin
  If (IDParam = PRM_OWNER) And (FLinkObject <> Value) Then
    Begin
      FLinkObject := Value;
      If Value <> Nil Then
        Event(Nil,EV_ITEMCHANGED + EV_OWNERCHANGED,Self,0)
      Else
        FSource := Nil
    End
End;

{----------------------------------------------------------}

Procedure TPSCDrawSlot.Set_Double(IDParam: integer; Value: Double);
Begin
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.GetBorderColor: TPSCColor;
Begin
  If GetParent <> Nil Then
    Result := TPSCColor(GetParent.Get_Cardinal(PRM_BORDERCOLOR))
  Else
    Result := clPSCGray
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.GetBackgroundColor: TPSCColor;
Begin
  If GetParent <> Nil Then
    Result := TPSCColor(GetParent.Get_Cardinal(PRM_BCKCOLOR))
  Else
    Result := clPSCBtnFace
End;

{----------------------------------------------------------}

Function TPSCDrawSlot.GetForegroundColor: TPSCColor;
Begin
  If GetParent <> Nil Then
    Result := TPSCColor(GetParent.Get_Cardinal(PRM_PRESSEDCOLOR))
  Else
    Result := clPSCBtnHighlight
End;

{----------------------------------------------------------}

Function TPSCDrawDefault.GetAlignment: TAlignment;
Begin
  If GetParent = Nil Then
    Result := taLeftJustify
  Else
    Result := TAlignment(GetParent.Get_Cardinal(PRM_ALIGNMENT))
End;

{----------------------------------------------------------}

Function TPSCDrawDefault.GetFont: TPSCFont;
Begin
  If GetParent = Nil Then
    Result := Nil
  Else
    Result := TPSCFont(GetParent.Get_Object(PRM_FONT))
End;

{----------------------------------------------------------}

Function TPSCDrawDefault.GetCanvas: TPSCCanvas;
Begin
  If GetParent = Nil Then
    Result := Nil
  Else
    Result := TPSCCanvas(GetParent.Get_Object(PRM_CANVAS));
End;

{----------------------------------------------------------}

Function TPSCDrawDefault.GetShowColors: Boolean;
Begin
  If GetParent = Nil Then
    Result := false
  Else
    Result := GetParent.Get_Boolean(PRM_SHOWCOLORS)
End;

{----------------------------------------------------------}

const
  SPSCMg = 'Mg';//don't resource

Function TPSCDrawDefault.Measure(Var NewWidth,NewHeight: Integer): Boolean;
Var
  Canvas: TPSCCanvas;
  H,L: Integer;
Begin
  Result := false;
  Canvas := GetCanvas;
  If Canvas = Nil Then
    exit;
  If GetFont <> Nil Then
    Canvas.Font := GetFont;
  H := Canvas.TextHeight(SPSCMg);
  If GetParent <> Nil Then
    With GetParent Do
      L := Get_Cardinal(PRM_COLUMNCOUNT) * Get_Cardinal(PRM_SLOTWIDTH)
  Else
    L := H * 2 + 10;
  If L < H * 2 + 10 Then
    L := H * 2 + 10;
  If NewWidth < L Then
    NewWidth := L;
  NewHeight := H + 12;
  Result := Visible
End;

{----------------------------------------------------------}

Function TPSCDrawDefault.Get_Cardinal(IDParam: integer): Cardinal;
Begin
  If IDParam = PRM_HIGHLIGHT Then
    Begin
      If GetParent = Nil Then
        Result := Cardinal(clPSCNone)
      Else
        Result := GetParent.Get_Cardinal(IDParam)
    End
  Else
    Result := Inherited Get_Cardinal(IDParam)
End;

{----------------------------------------------------------}

Function TPSCDrawDefault.Get_String(IDParam: integer): String;
Begin
  If IDParam = PRM_CAPTION Then
    Result := FCaption
  Else
    Result := Inherited Get_String(IDParam)
End;

{----------------------------------------------------------}

Procedure TPSCDrawDefault.Set_String(IDParam: integer; Const Value: String);
Begin
  If IDParam = PRM_CAPTION Then
    Begin
      If FCaption <> Value Then
        Begin
          FCaption := Value;
          Event(Nil,EV_ITEMCHANGED,Self,0)
        End
    End
  Else
    Inherited Set_String(IDParam,Value)
End;

{----------------------------------------------------------}

Procedure TPSCDrawDefault.Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect);
Var
  X,X1,CL: Integer;
  Rect: TRect;
  AState: TPSCButtonStates;
  Size: TSize;
  BStyle: TBrushStyle;
  BColor: TPSCColor;
  FColor: TPSCColor;
  PColor: TPSCColor;
  Highlight: TPSCColor;
  IsHighlight: Boolean;
Begin
  If Visible And RectVisible(Canvas.Handle,FBounds) Then
    With Canvas Do
      Begin
        Rect := FBounds;
        BStyle := Brush.Style;
        BColor := Brush.Color;
        FColor := Font.Color;
        PColor := Pen.Color;
        Highlight := TPSCColor(Get_Cardinal(PRM_HIGHLIGHT));
        Try
          If Not Enabled Then
            AState := []
          Else
            AState := State;
          If GetFont <> Nil Then
            Font := GetFont;
          IsHighlight := (Highlight <> clPSCNone) And
            (AState * [ButtonState_Up,ButtonState_Down] <> []);
          If IsHighlight Then
            Begin
              Brush.Color := Highlight;
              Font.Color := clPSCHighlightText;
              With Rect Do
                FillRect(PSCRect(Left,Top + 3,Right,Bottom - 3))
            End;
          If Highlight <> clPSCNone Then
            Begin
              CL := Get_Cardinal(PRM_COLUMNCOUNT) * Get_Cardinal(PRM_SLOTWIDTH);
              With Rect Do
                Begin
                  Case TAlignment(Get_Cardinal(PRM_ALIGNMENT)) Of
                    taLeftJustify: Left := 0;
                    taRightJustify: Left := Right - CL;
                  Else
                    Left := (Right + Left - CL) Div 2
                  End;
                  Right := Left + CL
                End
            End;
          If Not IsHighlight Then
            PSCDrawButton(
              Canvas,Rect,BackgroundColor,ForegroundColor,AState
              );
          InflateRect(Rect, -3, -3);
          Size := TextExtent(Caption);
          If Enabled Then
            Begin
              If Not IsHighlight Then
                Begin
                  DrawEdge(Handle,Rect,BDR_SUNKENINNER,BF_TOPLEFT Or BF_SOFT);
                  DrawEdge(Handle,Rect,BDR_RAISEDINNER,BF_BOTTOMRIGHT Or
                    BF_SOFT);
                End;
              Brush.Style := BrushStyle_Clear;
              X := (Rect.Right + Rect.Left - Size.cx) Div 2;
              X1 := Rect.Left + Size.cy + 6;
              If X < X1 Then
                X := X1;
              TextRect(Rect,X,(Rect.Top + Rect.Bottom - Size.cy) Div
                2,Caption);
              If ShowColors And (Color <> clPSCNone) And (Color <> clPSCDefault) Then
                Begin
                  With Rect Do
                    Begin
                      Left := Left + 3;
                      Top := Top + 3;
                      Bottom := Bottom - 3;
                      Right := Left + (Bottom - Top)
                    End;
                  Brush.Color := Color;
                  Brush.Style := BrushStyle_Solid;
                  Pen.Color := BorderColor;
                  Rectangle(Rect.Left,Rect.Top,Rect.Right,Rect.Bottom)
                End
            End
          Else
            Begin
              Rect.Left := Rect.Left + 1;
              Rect.Top := Rect.Top + 1;
              DrawEdge(Handle,Rect,BDR_SUNKENOUTER,BF_BOTTOMRIGHT);
              OffsetRect(Rect, -1, -1);
              DrawEdge(Handle,Rect,BDR_RAISEDOUTER,BF_RECT Or BF_FLAT);
              Brush.Style := BrushStyle_Clear;
              OffsetRect(Rect,1,1);
              Font.Color := clPSCBtnHighlight;
              TextRect(Rect,(Rect.Left + Rect.Right - Size.cx) Div 2,
                (Rect.Top + Rect.Bottom - Size.cy) Div 2,Caption);
              OffsetRect(Rect, -1, -1);
              Font.Color := clPSCBtnShadow;
              TextRect(Rect,(Rect.Left + Rect.Right - Size.cx) Div 2,
                (Rect.Top + Rect.Bottom - Size.cy) Div 2,Caption);
            End
        Finally
          Brush.Style := BStyle;
          Brush.Color := BColor;
          Font.Color := FColor;
          Pen.Color := PColor
        End
      End
End;

{----------------------------------------------------------}

Constructor TPSCDrawButton.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner);
  FAction := [baDialog,baSetColor,baSetCustomColor]
End;

{----------------------------------------------------------}

Function TPSCDrawButton.Measure(Var NewWidth,NewHeight: Integer): Boolean;
Var
  Canvas: TPSCCanvas;
  H,L: Integer;
Begin
  Result := false;
  Canvas := GetCanvas;
  If Canvas = Nil Then
    exit;
  If GetFont <> Nil Then
    Canvas.Font := GetFont;
  H := Canvas.TextHeight(SPSCMg);
  If GetParent <> Nil Then
    With GetParent Do
      L := Get_Cardinal(PRM_COLUMNCOUNT) * Get_Cardinal(PRM_SLOTWIDTH)
  Else
    L := H + 12;
  NewHeight := H + 12;
  If L < NewHeight Then
    L := NewHeight;
  If NewWidth < L Then
    NewWidth := L;
  Result := Visible
End;

{----------------------------------------------------------}

Function TPSCDrawButton.Get_Cardinal(IDParam: integer): Cardinal;
Begin
  If IDParam = PRM_BUTTONACTION Then
    Result := byte(FAction)
  Else
    Result := Inherited Get_Cardinal(IDParam)
End;

{----------------------------------------------------------}

Procedure TPSCDrawButton.Set_Cardinal(IDParam: integer; Value: Cardinal);
Var
  AAction: TPSCButtonsActions;
Begin
  If IDParam = PRM_BUTTONACTION Then
    Begin
      AAction := TPSCButtonsActions(byte(Value));
      If FAction <> AAction Then
        Begin
          FAction := AAction;
          Event(Nil,EV_ITEMCHANGED,Self,0)
        End
    End
  Else
    Inherited Set_Cardinal(IDParam,Value)
End;

{----------------------------------------------------------}

Procedure TPSCDrawButton.Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect);
Var
  Rect,R: TRect;
  AState: TPSCButtonStates;
  H,L,X,CL: Integer;
  BStyle: TBrushStyle;
  BColor: TPSCColor;
  FColor: TPSCColor;
  PColor: TPSCColor;
  Highlight: TPSCColor;
  IsHighlight: Boolean;
Begin
  If Visible And RectVisible(Canvas.Handle,FBounds) Then
    With Canvas Do
      Begin
        Rect := FBounds;
        BStyle := Brush.Style;
        BColor := Brush.Color;
        FColor := Font.Color;
        PColor := Pen.Color;
        Highlight := TPSCColor(Get_Cardinal(PRM_HIGHLIGHT));
        Try
          If GetFont <> Nil Then
            Font := GetFont;
          H := TextHeight(SPSCMg);
          If ShowColors Then
            L := H * 2 + 6
          Else
            L := 3;
          If Not Enabled Then
            AState := []
          Else
            AState := State;
          IsHighlight := (Highlight <> clPSCNone) And
            (AState * [ButtonState_Up,ButtonState_Down] <> []);
          If IsHighlight Then
            Begin
              Brush.Color := Highlight;
              Font.Color := clPSCHighlightText;
              With Rect Do
                FillRect(PSCRect(Left,Top + 3,Right,Bottom - 3))
            End;
          If Highlight <> clPSCNone Then
            Begin
              CL := Get_Cardinal(PRM_COLUMNCOUNT) * Get_Cardinal(PRM_SLOTWIDTH);
              With Rect Do
                Begin
                  Case TAlignment(Get_Cardinal(PRM_ALIGNMENT)) Of
                    taLeftJustify: Left := 0;
                    taRightJustify: Left := Right - CL;
                  Else
                    Left := (Right + Left - CL) Div 2
                  End;
                  Right := Left + CL
                End
            End;
          If Not IsHighlight Then
            PSCDrawButton(
              Canvas,BoundsRect,BackgroundColor,ForegroundColor,AState
              );
          InflateRect(Rect, -1, -1);
          Case Alignment Of
            taLeftJustify: X := Rect.Left + L;
            taRightJustify: X := Rect.Right - 4 - TextWidth(Caption)
          Else
            X := (Rect.Right + Rect.Left - TextWidth(Caption)) Div 2
          End;
          If X < Rect.Left + L Then
            X := Rect.Left + L;
          If (ButtonState_Down In AState) And (Highlight = clPSCNone) Then
            Begin
              OffsetRect(Rect,1,1);
              X := X + 1
            End;
          If Enabled Then
            Begin
              Brush.Style := BrushStyle_Clear;
              With Rect Do
                R := PSCRect(X,Top,Right,Bottom);
              PSCDrawText(Canvas,Caption,Length(Caption),R,DT_SINGLELINE
                Or DT_VCENTER);
              If ShowColors And (Color <> clPSCNone) Then
                Begin
                  If Color = clPSCDefault Then
                    Brush.Style := BrushStyle_Clear
                  Else
                    Begin
                      Brush.Color := Color;
                      Brush.Style := BrushStyle_Solid
                    End;
                  Pen.Color := BorderColor;
                  With Rect Do
                    Rectangle(Left + 3,Top + 5,Left + L - 6,Bottom - 5);
                  If Highlight <> clPSCNone Then
                    Begin
                      Pen.Color := clPSCBtnShadow;
                      Brush.Style := BrushStyle_Clear;
                      With Rect Do
                        Rectangle(Left + 4,Top + 6,Left + L - 7,Bottom - 6)
                    End
                End
            End
          Else
            Begin
              Brush.Style := BrushStyle_Clear;
              OffsetRect(Rect,1,1);
              Font.Color := clPSCBtnHighlight;
              With Rect Do
                R := PSCRect(X,(Top + Bottom - H) Div 2,Right,Bottom);
              PSCDrawText(Canvas,Caption,Length(Caption),R,DT_SINGLELINE);
              OffsetRect(R, -1, -1);
              Font.Color := clPSCBtnShadow;
              PSCDrawText(Canvas,Caption,Length(Caption),R,DT_SINGLELINE);
              Font.Color := FColor
            End
        Finally
          Brush.Style := BStyle;
          Brush.Color := BColor;
          Font.Color := FColor;
          Pen.Color := PColor
        End
      End
End;

{----------------------------------------------------------}

Constructor TPSCDrawSection.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner);
  If AOwner <> Nil Then
    AOwner.GetInterface(IPSCDrawList,FList)
End;

{----------------------------------------------------------}

Function TPSCDrawSection.GetBounds: TRect;
Begin
  Result := PSCRect(0,0,0,0);
  If FSource <> Nil Then
    Result := (FSource As IPSCBounds).GetBounds
End;

{----------------------------------------------------------}

Function TPSCDrawSection.Measure(Var NewWidth,NewHeight: Integer): Boolean;
Begin
  Result := false
End;

{----------------------------------------------------------}

Function TPSCDrawSection.GetEnabled: Boolean;
Begin
  Result := FEnabled;
  If (GetParent <> Nil) And Not IsDesignTime Then
    Result := Result And GetParent.Get_Boolean(PRM_ENABLED)
End;

{----------------------------------------------------------}

Function TPSCDrawSection.Get_Boolean(IDParam: integer): Boolean;
Begin
  Case IDParam Of
    PRM_ENABLED:
      Result := Enabled;
    PRM_SHOWSEPARATOR:
      Result := FShowSeparator;
  Else
    Result := Inherited Get_Boolean(IDParam)
  End
End;

{----------------------------------------------------------}

Function TPSCDrawSection.Get_Cardinal(IDParam: integer): Cardinal;
Begin
  Case IDParam Of
    PRM_ALIGNMENT:
      Result := Cardinal(FAlignment);
    PRM_BCKCOLOR:
      Result := Cardinal(FBackgroundColor);
    PRM_BORDERCOLOR:
      Result := Cardinal(FBorderColor);
    PRM_HIGHLIGHT:
      If GetParent <> Nil Then
        Result := GetParent.Get_Cardinal(IDParam)
      Else
        Result := Cardinal(clPSCNone);
    PRM_COLUMNCOUNT:
      If GetParent <> Nil Then
        Result := GetParent.Get_Cardinal(IDParam)
      Else
        Result := 8;
    PRM_SLOTWIDTH,
      PRM_SLOTHEIGHT:
      If GetParent <> Nil Then
        Result := GetParent.Get_Cardinal(IDParam)
      Else
        Result := 18;
    PRM_HINTKIND:
      If GetParent <> Nil Then
        Result := GetParent.Get_Cardinal(IDParam)
      Else
        Result := 0;
    PRM_KIND:
      Result := FKind;
    PRM_DEFAULTKIND:
      Result := FDefaultKind;
  Else
    Result := 0;
  End
End;

{----------------------------------------------------------}

Function TPSCDrawSection.Get_String(IDParam: integer): String;
Begin
  Case IDParam Of
    PRM_CAPTION:
      Result := FCaption;
    PRM_HINT:
      Result := FHint;
  Else
    Result := ''
  End
End;

{----------------------------------------------------------}

Function TPSCDrawSection.Get_Object(IDParam: integer): TObject;
Begin
  Case IDParam Of
    PRM_FONT:
      Begin
        If GetParent <> Nil Then
          Result := GetParent.Get_Object(IDParam)
        Else
          Result := Nil
      End;
    PRM_CANVAS:
      Begin
        If FSource <> Nil Then
          Result := FSource.Get_Object(IDParam)
        Else
          Result := Nil
      End;
  Else
    Result := Nil
  End
End;

{----------------------------------------------------------}

Procedure TPSCDrawSection.Set_Boolean(IDParam: integer; Value: Boolean);
Begin
  Case IDParam Of
    PRM_ENABLED:
      If FEnabled <> Value Then
        Begin
          FEnabled := Value;
          Event(Self,EV_SECTIONCHANGED,nil,0)
        End;
    PRM_SHOWSEPARATOR:
      If FShowSeparator <> Value Then
        Begin
          FShowSeparator := Value;
          Event(Self,EV_SECTIONCHANGED,nil,0)
        End;
  Else
    Inherited Set_Boolean(IDParam,Value)
  End;
End;

{----------------------------------------------------------}

Procedure TPSCDrawSection.Set_Cardinal(IDParam: integer; Value: Cardinal);
Begin
  Case IDParam Of
    PRM_BCKCOLOR:
      If FBackgroundColor <> TPSCColor(Value) Then
        Begin
          FBackgroundColor := TPSCColor(Value);
          Event(Self,EV_SECTIONCHANGED,nil,0)
        End;
    PRM_BORDERCOLOR:
      If FBorderColor <> TPSCColor(Value) Then
        Begin
          FBorderColor := TPSCColor(Value);
          Event(Self,EV_SECTIONCHANGED,nil,0)
        End;
    PRM_ALIGNMENT:
      If FAlignment <> TAlignment(Value) Then
        Begin
          FAlignment := TAlignment(Value);
          Event(Self,EV_SECTIONCHANGED,nil,0)
        End;
    PRM_KIND:
      FKind := Value;
    PRM_DEFAULTKIND:
      FDefaultKind := Value;
  End
End;

{----------------------------------------------------------}

Procedure TPSCDrawSection.Set_String(IDParam: integer; Const Value: String);
Begin
  Case IDParam Of
    PRM_CAPTION:
      Begin
        If FCaption <> Value Then
          Begin
            FCaption := Value;
            Event(Self,EV_SECTIONCHANGED,nil,0)
          End
      End
  Else
    Inherited Set_String(IDParam,Value)
  End
End;

{----------------------------------------------------------}

Procedure TPSCDrawSection.Event(Sender: IPSCObject; EvType:integer;LowData:TObject;HighData: integer);
Begin
  If (FUpdateCount = 0) And (FSource <> Nil) Then
    FSource.Event(Self,EV_SECTIONCHANGED,nil,HighData)
End;

{----------------------------------------------------------}

Function TPSCDrawSection.GetParent: IPSCDataReader;
Begin
  Result := FSource.GetParent
End;

{----------------------------------------------------------}

Function TPSCDrawSection.GetVisible: Boolean;
Begin
  Result := FVisible;
  If GetParent <> Nil Then
    Result := Result And GetParent.Get_Boolean(PRM_VISIBLE);
  Result := Result Or IsDesignTime
End;

{----------------------------------------------------------}

Procedure TPSCDrawSection.DrawSeparator(Canvas: TPSCCanvas;
  Const BoundsRect: TRect);
Var
  R: TRect;
Begin
  With BoundsRect Do
    R := PSCRect(0,0,Right - Left,Bottom - Top - 1);
  With Canvas Do
    If Visible And ShowSeparator And RectVisible(Handle,R) Then
      DrawEdge(Handle,R,EDGE_ETCHED,BF_BOTTOM)
End;

{----------------------------------------------------------}

Procedure TPSCDrawSection.Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect);
Begin
End;

{----------------------------------------------------------}

Function TPSCDrawSection.GetVisibleCount: integer;
Var
  i: integer;
Begin
  FVisibleCount := 0;
  With FList Do
    For i := 0 To GetCount - 1 Do
      If (GetItem(i) As IPSCBounds).GetVisible Then
        inc(FVisibleCount);
  Result := FVisibleCount
End;

{----------------------------------------------------------}

Function TPSCDrawSection.IsDesignTime: Boolean;
Begin
  If FSource <> Nil Then
    Result := FSource.Get_Boolean(PRM_DESIGNTIME)
  Else
    Result := false
End;

{----------------------------------------------------------}

Function TPSCDrawSection.GetFont: TPSCFont;
Var
  ptr: TObject;
Begin
  ptr := Get_Object(PRM_FONT);
  If ptr <> Nil Then
    Result := TPSCFont(ptr)
  Else
    Result := Nil
End;

{----------------------------------------------------------}

Function TPSCDrawSection.GetCanvas: TPSCCanvas;
Var
  ptr: TObject;
Begin
  ptr := Get_Object(PRM_CANVAS);
  If ptr <> Nil Then
    Result := TPSCCanvas(ptr)
  Else
    Result := Nil
End;

{----------------------------------------------------------}

Function TPSCDrawColorSection.GetSlotWidth: integer;
Begin
  Result := Get_Cardinal(PRM_SLOTWIDTH)
End;

{----------------------------------------------------------}

Function TPSCDrawColorSection.GetSlotHeight: integer;
Begin
  Result := Get_Cardinal(PRM_SLOTHEIGHT)
End;

{----------------------------------------------------------}

Function TPSCDrawColorSection.GetColumnCount: Integer;
Begin
  Result := Get_Cardinal(PRM_COLUMNCOUNT)
End;

{----------------------------------------------------------}

Function TPSCDrawColorSection.Get_Boolean(IDParam: integer): Boolean;
Begin
  Case IDParam Of
    PRM_SHOWCAPTION:
      Result := FShowCaption;
  Else
    Result := Inherited Get_Boolean(IDParam)
  End
End;

{----------------------------------------------------------}

Procedure TPSCDrawColorSection.Set_Boolean(IDParam: integer; Value: Boolean);
Begin
  Case IDParam Of
    PRM_SHOWCAPTION:
      Begin
        If FShowCaption <> Value Then
          Begin
            FShowCaption := Value;
            Event(Self,EV_SECTIONCHANGED,nil,0)
          End
      End;
  Else
    Inherited Set_Boolean(IDParam,Value)
  End
End;

{----------------------------------------------------------}

Function TPSCDrawColorSection.Measure(Var NewWidth,NewHeight: Integer): Boolean;
Var
  cx,cy: integer;
Begin
  Result := IsDesignTime;
  If ShowCaption Then
    Begin
      If GetCanvas <> Nil Then
        Begin
          If GetFont <> Nil Then
            GetCanvas.Font := GetFont;
          cy := GetCanvas.TextHeight(SPSCMg) + 8
        End
      Else
        cy := 16
    End
  Else
    cy := 0;
  cx := ColumnCount * SlotWidth;
  cy := cy + (VisibleCount + ColumnCount - 1) Div ColumnCount * SlotHeight;
  If ShowSeparator And Visible Then
    cy := cy + 4;
  If (cy = 0) And Result Then
    cy := 25;
  If (NewWidth < cx) Or (NewHeight < cy) Then
    Begin
      If NewWidth < cx Then
        NewWidth := cx;
      If NewHeight < cy Then
        NewHeight := cy;
      Result := Result Or Visible
    End
End;

{----------------------------------------------------------}

Procedure TPSCDrawColorSection.Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect);
Var
  i,cc: integer;
  Rect: TRect;
  BColor: TPSCColor;
  BStyle: TBrushStyle;
  FColor: TPSCColor;
  ACaption: String;
Begin
  If Visible Then
    With Canvas Do
      Begin
        BColor := Brush.Color;
        BStyle := Brush.Style;
        FColor := Font.Color;
        Try
          If GetFont <> Nil Then
            Font := GetFont;
          With BoundsRect Do
            Rect := PSCRect(0,0,Right - Left,Bottom - Top);
          ACaption := Caption;
          If ShowCaption Then
            Begin
              cc := 0;
              With FList Do
                For i := 0 To GetCount - 1 Do
                  If (GetItem(i) As IPSCBounds).Visible Then
                    inc(cc);
              If (cc > 0) And (cc > ColumnCount) Then
                cc := ColumnCount;
              i := TextHeight(SPSCMg);
              With Rect Do
                Begin
                  Top := Top + 4;
                  Case Alignment Of
                    taLeftJustify: Left := 0;
                    taRightJustify: Left := Right - Left - cc * SlotWidth;
                    taCenter: Left := (Right - Left - cc * SlotWidth) Div 2;
                  End;
                  Bottom := Top + i
                End;
              InflateRect(Rect, -2,0);
              If RectVisible(Handle,Rect) Then
                Begin
                  Brush.Style := BrushStyle_Clear;
                  If Not Enabled Then
                    Begin
                      Font.Color := clPSCBtnHighlight;
                      OffsetRect(Rect,1,1);
                      PSCDrawText(Canvas,ACaption,Length(ACaption),Rect,DT_SINGLELINE);
                      OffsetRect(Rect, -1, -1);
                      Font.Color := clPSCBtnShadow;
                    End;
                  PSCDrawText(Canvas,ACaption,Length(ACaption),Rect,DT_SINGLELINE);
                  If Not Enabled Then
                    Font.Color := FColor
                End
            End;
          With FList Do
            For i := 0 To GetCount - 1 Do
              If (GetItem(i) As IPSCBounds).Visible Then
                Begin
                  Rect := (GetItem(i) As IPSCBounds).GetBounds;
                  GetItemAsDraw(i).Draw(Canvas,Rect)
                End;
          DrawSeparator(Canvas,BoundsRect)
        Finally
          Brush.Color := BColor;
          Brush.Style := BStyle;
          Font.Color := FColor
        End
      End
End;

{----------------------------------------------------------}

Function TPSCDrawButtonSection.Get_Boolean(IDParam: integer): Boolean;
Begin
  Case IDParam Of
    PRM_SHOWCOLORS:
      Result := FShowColors;
  Else
    Result := Inherited Get_Boolean(IDParam)
  End
End;

{----------------------------------------------------------}

Procedure TPSCDrawButtonSection.Set_Boolean(IDParam: integer; Value: Boolean);
Begin
  If IDParam = PRM_SHOWCOLORS Then
    Begin
      If FShowColors <> Value Then
        Begin
          FShowColors := Value;
          Event(Self,EV_SECTIONCHANGED,nil,0)
        End
    End
  Else
    Inherited Set_Boolean(IDParam,Value)
End;

{----------------------------------------------------------}

Function TPSCDrawButtonSection.Measure(Var NewWidth,NewHeight: Integer)
  : Boolean;
Var
  W,cx,cy: integer;
Begin
  Result := IsDesignTime;
  If GetCanvas <> Nil Then
    Begin
      If GetFont <> Nil Then
        GetCanvas.Font := GetFont;
      W := GetCanvas.TextHeight(SPSCMg)
    End
  Else
    W := 12;
  cx := Get_Cardinal(PRM_COLUMNCOUNT) * Get_Cardinal(PRM_SLOTWIDTH);
  cy := W + 12;
  If cx < cy Then
    cx := cy;
  cy := cy * VisibleCount;
  If ShowSeparator And Visible Then
    cy := cy + 4;
  If (cy = 0) And Result Then
    cy := 25;
  If (NewWidth < cx) Or (NewHeight < cy) Then
    Begin
      If NewWidth < cx Then
        NewWidth := cx;
      If NewHeight < cy Then
        NewHeight := cy;
      Result := Result Or Visible
    End
End;

{----------------------------------------------------------}

Procedure TPSCDrawButtonSection.Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect);
Var
  i: integer;
  Rect: TRect;
Begin
  If Visible Then
    Begin
      With FList Do
        For i := 0 To GetCount - 1 Do
          If (GetItem(i) As IPSCBounds).Visible Then
            Begin
              Rect := (GetItem(i) As IPSCBounds).GetBounds;
              GetItemAsDraw(i).Draw(Canvas,Rect)
            End;
      DrawSeparator(Canvas,BoundsRect)
    End
End;

{----------------------------------------------------------}

Procedure TPSCAlignerSink.Realign(
  Container: IPSCObject;
  BoundsList: IPSCBoundsList
  );
Var
  i,W,H,W2,H2: integer;
  R: TRect;
  IsDesignTime: Boolean;
Begin
  If (BoundsList = Nil) Or (Container = Nil) Then
    exit;
  With Container As IPSCDataAccess Do
    IsDesignTime := AsBoolean[PRM_DESIGNTIME];
  With Container As IPSCBounds Do
    R := GetBounds;
  W2 := R.Right - R.Left;
  H2 := 0;
  For i := 0 To BoundsList.GetCount - 1 Do
    With BoundsList.GetItemAsBounds(i) Do
      Begin
        W := W2;
        H := 0;
        If Visible Or IsDesignTime Then
          Measure(W,H);
        SetBounds(PSCRect(0,H2,W,H2 + H));
        H2 := H2 + H;
        If W > W2 Then
          W2 := W
      End
End;

{----------------------------------------------------------}

Procedure TPSCAlignerSink2.Realign(
  Container: IPSCObject;
  BoundsList: IPSCBoundsList
  );
Var
  i,W,H,W2,H2: integer;
  SW,SH,CL,idx: integer;
  R: TRect;
  ptr1,ptr2: TObject;
Begin
  If (BoundsList = Nil) Or (Container = Nil) Then
    exit;
  With Container As IPSCBounds Do
    Begin
      R := GetBounds;
      W2 := R.Right - R.Left;
      H2 := 0;
      Measure(W2,H2);
      R := PSCRect(0,0,W2,H2);
    End;
  With Container As IPSCDataReader Do
    Begin
      SW := Get_Cardinal(PRM_SLOTWIDTH);
      SH := Get_Cardinal(PRM_SLOTHEIGHT);
      CL := Get_Cardinal(PRM_COLUMNCOUNT);
      idx := 0;
      For i := 0 To BoundsList.GetCount - 1 Do
        If BoundsList.GetItemAsBounds(i).Visible Then
          inc(idx);
      If (idx > 0) And (idx < CL) Then
        CL := idx;
      Case TAlignment(Get_Cardinal(PRM_ALIGNMENT)) Of
        taLeftJustify: W2 := 0;
        taRightJustify: W2 := W2 - SW * CL;
      Else
        W2 := (W2 - SW * CL) Div 2
      End;
      H2 := 0;
      If Get_Boolean(PRM_SHOWCAPTION) Then
        Begin
          ptr1 := Get_Object(PRM_FONT);
          ptr2 := Get_Object(PRM_CANVAS);
          If ptr2 <> Nil Then
            Begin
              If ptr1 <> Nil Then
                TPSCCanvas(ptr2).Font := TPSCFont(ptr1);
              H2 := TPSCCanvas(ptr2).TextHeight(SPSCMg) + 8
            End
        End
    End;
  idx := -1;
  For i := 0 To BoundsList.GetCount - 1 Do
    With BoundsList.GetItemAsBounds(i) Do
      If Measure(SW,SH) Then
        Begin
          inc(idx);
          W := W2 + (idx Mod CL) * SW;
          H := H2 + (idx Div CL) * SH;
          SetBounds(PSCRect(W,H,W + SW,H + SH))
        End
End;

{----------------------------------------------------------}

Procedure TPSCAlignerSink3.Realign(
  Container: IPSCObject;
  BoundsList: IPSCBoundsList
  );
Var
  i,W2,H2: integer;
  SW,SH: integer;
  R: TRect;
  Highlight: Boolean;
Begin
  If (BoundsList = Nil) Or (Container = Nil) Then
    exit;
  With Container As IPSCBounds Do
    Begin
      R := GetBounds;
      W2 := R.Right - R.Left;
      H2 := 0;
      Measure(W2,H2);
      R := PSCRect(0,0,W2,H2);
    End;
  With Container As IPSCDataReader Do
    Begin
      Highlight := TPSCColor(Get_Cardinal(PRM_HIGHLIGHT)) <> clPSCNone;
      If Not Highlight Then
        Begin
          SW := Get_Cardinal(PRM_SLOTWIDTH) * Get_Cardinal(PRM_COLUMNCOUNT);
          Case TAlignment(Get_Cardinal(PRM_ALIGNMENT)) Of
            taLeftJustify: W2 := 0;
            taRightJustify: W2 := W2 - SW;
          Else
            W2 := (W2 - SW) Div 2
          End
        End;
      H2 := 0
    End;
  For i := 0 To BoundsList.GetCount - 1 Do
    With BoundsList.GetItemAsBounds(i) Do
      If Measure(SW,SH) Then
        Begin
          If Highlight Then
            SetBounds(PSCRect(0,H2,R.Right,H2 + SH))
          Else
            SetBounds(PSCRect(W2,H2,W2 + SW,H2 + SH));
          H2 := H2 + SH
        End
End;

{----------------------------------------------------------}

Procedure TPSCKeyboardSink.KeyDown(Sender: IPSCObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Sender <> Nil Then
    With Sender As IPSCSelect Do
      Case Key Of
        VK_LEFT: MoveSelection(sdLeft);
        VK_RIGHT: MoveSelection(sdRight);
        VK_UP: MoveSelection(sdUp);
        VK_DOWN: MoveSelection(sdDown);
        VK_RETURN: Select
      End
End;

{----------------------------------------------------------}

Procedure TPSCKeyboardSink.KeyUp(Sender: IPSCObject; Var Key: Word;
  Shift: TShiftState);
Begin
End;

{----------------------------------------------------------}

Procedure TPSCKeyboardSink.KeyPress(Sender: IPSCObject; Var Key: Char);
Begin
End;

{----------------------------------------------------------}

Procedure TPSCMouseSink.MouseEnter(Sender: IPSCObject);
Begin
End;

{----------------------------------------------------------}

Procedure TPSCMouseSink.MouseLeave(Sender: IPSCObject);
Var
  P: TPoint;
  Ctrl1,Ctrl2: TControl;
Begin
  If Sender <> Nil Then
    Begin
      Ctrl1 := PSCGetParentControl(Sender.GetInstance);
      With Ctrl1 Do
        Begin
          GetCursorPos(P);
          Ctrl2 := FindDragTarget(P,True);
          If (Ctrl2 = Nil) Or (Ctrl2 <> Ctrl1) And (Ctrl2.Parent <> Ctrl1) Then
            With Sender As IPSCDataAccess Do
              AsObject[PRM_ACTIVESLOT] := Nil
        End
    End
End;

{----------------------------------------------------------}

Procedure TPSCMouseSink.MouseMove(Sender: IPSCObject; Shift: TShiftState;
  X,Y: Integer);
Var
  Data: IPSCDataAccess;
  ptr: TObject;
Begin
  If Sender <> Nil Then
    Begin
      With Sender As IPSCBoundsList Do
        Data := GetItemAt(PSCPoint(X,Y)) As IPSCDataAccess;
      If Data <> Nil Then
        ptr := Data.AsObject[PRM_OWNER]
      Else
        ptr := Nil;
      With Sender As IPSCDataAccess Do
        If AsObject[PRM_ACTIVESLOT] <> ptr Then
          Begin
            If AsObject[PRM_ACTIVESLOT] = Nil Then
              Application.CancelHint;
            AsObject[PRM_ACTIVESLOT] := ptr
          End
    End
End;

{----------------------------------------------------------}

Procedure TPSCMouseSink.MouseDown(Sender: IPSCObject; Button: TMouseButton;
  Shift: TShiftState; X,Y: Integer);
Var
  Ctrl: TControl;
  IData: IPSCDataAccess;
Begin
  If (Sender <> Nil) And (Button = mbLeft) Then
    Begin
      With Sender As IPSCBoundsList Do
        IData := GetItemAt(PSCPoint(X,Y)) As IPSCDataAccess;
      With Sender As IPSCDataAccess Do
        If (IData <> Nil) And IData.AsBoolean[PRM_ENABLED] And
          IData.AsBoolean[PRM_VISIBLE] Then
          AsObject[PRM_ACTIVESLOT] := IData.AsObject[PRM_OWNER]
        Else
          AsObject[PRM_ACTIVESLOT] := Nil;
      Ctrl := PSCGetParentControl(Sender.GetInstance);
      If Not (ssDouble In Shift) And Not (csDesigning In Ctrl.ComponentState)
        And (Ctrl Is TWinControl) And TWinControl(Ctrl).CanFocus Then
        Begin
          TWinControl(Ctrl).SetFocus;
          With Sender As IPSCDataAccess Do
            If (AsObject[PRM_ACTIVESLOT] <> Nil) Then
              AsBoolean[PRM_DOWNACTIVESLOT] := true
        End
    End
End;

{----------------------------------------------------------}

Procedure TPSCMouseSink.MouseUp(Sender: IPSCObject; Button: TMouseButton;
  Shift: TShiftState; X,Y: Integer);
Var
  IData: IPSCDataAccess;
Begin
  If (Sender <> Nil) And (Button = mbLeft) Then
    With Sender As IPSCDataAccess Do
      Begin
        If (AsObject[PRM_ACTIVESLOT] <> Nil) And
          (AsObject[PRM_ACTIVESLOT] <> AsObject[PRM_SELECTEDSLOT]) Then
          AsBoolean[PRM_UPACTIVESLOT] := True;
        With Sender As IPSCBoundsList Do
          IData := GetItemAt(PSCPoint(X,Y)) As IPSCDataAccess;
        If (IData <> Nil) And (AsObject[PRM_ACTIVESLOT] =
          IData.AsObject[PRM_OWNER]) Then
          AsObject[PRM_SELECTEDSLOT] := AsObject[PRM_ACTIVESLOT]
      End
End;

{----------------------------------------------------------}

Procedure TPSCMouseSink.MouseWheel(Sender: IPSCObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint);
Begin
End;

{----------------------------------------------------------}

Var
  PSCAlignerSink,PSCColorAlignerSink,PSCButtonAlignerSink: IPSCBoundsAligner;

{-------------------------------------}

Constructor TPSCGraphicItem.Create(Collection: TCollection);
Begin
  Inherited Create(Collection);
  DoCreateItem;
  Data.AsObject[PRM_OWNER] := Self
End;

{-------------------------------------}

Destructor TPSCGraphicItem.Destroy;
Begin
  Data.Parent.Event(Nil,EV_ITEMDELETED,Data.GetInstance,0);
  Data.AsObject[PRM_OWNER] := Nil;
  Inherited Destroy
End;

{-------------------------------------}

Procedure TPSCGraphicItem.Assign(Source: TPersistent);
Begin
  If Source Is TPSCGraphicItem Then
    TPSCGraphicItem(Source).FImage := FImage
  Else
    Inherited Assign(Source)
End;

{-------------------------------------}

Function TPSCGraphicItem.GetAsDraw: IPSCDraw;
Begin
  Result := FImage As IPSCDraw
End;

{-------------------------------------}

Function TPSCGraphicItem.GetAsData: IPSCDataAccess;
Begin
  Result := FImage As IPSCDataAccess
End;

{-------------------------------------}

Function TPSCGraphicItem.GetAsBounds: IPSCBounds;
Begin
  Result := FImage As IPSCBounds
End;

{-------------------------------------}

Function TPSCGraphicItem.GetSection: TPSCCustomSection;
Begin
  If (Collection <> Nil) And
    (TPSCGraphicItems(Collection).Owner Is TPSCCustomSection) Then
    Result := TPSCGraphicItems(Collection).Owner As TPSCCustomSection
  Else
    Result := Nil
End;

{-------------------------------------}

Function TPSCGraphicItem.CanSelect: Boolean;
Begin
  Result := Data.AsBoolean[PRM_ENABLED] And Data.AsBoolean[PRM_VISIBLE]
End;

{-------------------------------------}

Procedure TPSCColorSlot.DoCreateItem;
Begin
  FImage := TPSCDrawSlot.Create(TPSCGraphicItems(Collection).Owner)
End;

{-------------------------------------}

Function TPSCColorSlot.GetDisplayName: String;
Var
  S: String;
Begin
  S := GetHint;
  If S = '' Then
    Result := Inherited GetDisplayName
  Else
    Result := S
End;

{-------------------------------------}

Function TPSCColorSlot.GetColor: TPSCColor;
Begin
  Result := TPSCColor(Data.AsCardinal[PRM_COLOR])
End;

{-------------------------------------}

Procedure TPSCColorSlot.SetColor(Value: TPSCColor);
Begin
  Data.AsCardinal[PRM_COLOR] := Cardinal(Value)
End;

{-------------------------------------}

Function TPSCColorSlot.GetHint: String;
Begin
  Result := Data.AsString[PRM_HINT]
End;

{-------------------------------------}

var
  FDefaultCaptions:IPSCStrings;

function PSCGetDefaultCaptions:IPSCStrings;
begin
  If FDefaultCaptions=nil then
  begin
    FDefaultCaptions:=PSCCreateStringList(ioOwned);
    FDefaultCaptions.AddObject(PSCConsts.MoreColors,TPSCValuesContainer.Create(1));
    FDefaultCaptions.AddObject(PSCConsts.Auto,TPSCValuesContainer.Create(2));
    FDefaultCaptions.AddObject(PSCConsts.AutoColor,TPSCValuesContainer.Create(3));
    FDefaultCaptions.AddObject(PSCConsts.None,TPSCValuesContainer.Create(4));
    FDefaultCaptions.AddObject(PSCConsts.NoHighLight,TPSCValuesContainer.Create(5));
    FDefaultCaptions.AddObject(PSCConsts.MoreColorsCapt,TPSCValuesContainer.Create(6));
    FDefaultCaptions.AddObject(PSCConsts.StdColors,TPSCValuesContainer.Create(7));
    FDefaultCaptions.AddObject(PSCConsts.WindowsColors,TPSCValuesContainer.Create(8));
    FDefaultCaptions.AddObject(PSCConsts.DocColors,TPSCValuesContainer.Create(9));
    FDefaultCaptions.AddObject(PSCConsts.FontColors,TPSCValuesContainer.Create(10));
    FDefaultCaptions.AddObject(PSCConsts.CustomColors,TPSCValuesContainer.Create(11));
    FDefaultCaptions.AddObject(PSCConsts.CustomColorsCapt,TPSCValuesContainer.Create(12));
  end;
  Result:=FDefaultCaptions;
end;

{-------------------------------------}

function PSCConstDefaultCaptionFromID(AIndex:Integer):String;
var
  i:Integer;
begin
  With PSCGetDefaultCaptions do
    for i:=0 To Count-1 do
      if TPSCValuesContainer(Objects[i]).IntValue=AIndex then
      begin
        Result:=Strings[i];
        exit;
      end;
  Result:='';
end;

{-------------------------------------}

function PSCConstDefaultCaptionToID(const Value:String):Integer;
begin
  Result:=PSCGetDefaultCaptions.IndexOf(Value);
  If Result>=0 then
    Result:=Integer(TPSCValuesContainer(PSCGetDefaultCaptions.Objects[Result]).IntValue)
  else
    Result:=0;
end;

{-------------------------------------}

Procedure TPSCColorSlot.SetHint(Const Value: String);
Begin
  Data.AsString[PRM_HINT] := Value;
  FDefaultHint := PSCConstDefaultCaptionToID(Value);
End;

{-------------------------------------}

Procedure TPSCColorSlot.ReadHint(Reader: TReader);
Begin
  FDefaultHint := Reader.ReadInteger;
  If Not IsHintStored Then
    SetHint(PSCConstDefaultCaptionFromID(FDefaultHint));
End;

{-------------------------------------}

Procedure TPSCColorSlot.WriteHint(Writer: TWriter);
Begin
  Writer.WriteInteger(FDefaultHint)
End;

{-------------------------------------}

Procedure TPSCColorSlot.DefineProperties(Filer: TFiler);
Begin
  Inherited DefineProperties(Filer);
  Filer.DefineProperty('DefaultHint',ReadHint,WriteHint, //don't resource
    FDefaultHint <> 0)
End;

{-------------------------------------}

Function TPSCColorSlot.GetState: TPSCButtonStates;
Begin
  Result := TPSCButtonStates(byte(Data.AsCardinal[PRM_STATES]))
End;

{-------------------------------------}

Procedure TPSCColorSlot.SetState(Value: TPSCButtonStates);
Begin
  Data.AsCardinal[PRM_STATES] := byte(Value)
End;

{-------------------------------------}

Function TPSCColorSlot.GetVisible: Boolean;
Begin
  Result := Data.AsBoolean[PRM_VISIBLE]
End;

{-------------------------------------}

Procedure TPSCColorSlot.SetVisible(Value: Boolean);
Begin
  Data.AsBoolean[PRM_VISIBLE] := Value
End;

{-------------------------------------}

Function TPSCColorSlot.IsHintStored: Boolean;
Var
  Hint1,Hint2: String;
Begin
  With Data Do
    Begin
      Hint1 := AsString[PRM_HINT];
      AsString[PRM_HINT] := '';
      Hint2 := AsString[PRM_HINT];
      AsString[PRM_HINT] := Hint1;
      Result := (Hint1 <> Hint2) And (FDefaultHint = 0)
    End
End;

{-------------------------------------}

Function TPSCColorSlot.GetSlotEnabled: Boolean;
Begin
  Result := Data.AsBoolean[PRM_ENABLED]
End;

{-------------------------------------}

Procedure TPSCDefaultSlot.DoCreateItem;
Begin
  FImage := TPSCDrawDefault.Create(TPSCGraphicItems(Collection).Owner)
End;

{-------------------------------------}

Function TPSCDefaultSlot.GetDisplayName: String;
Var
  S: String;
Begin
  S := GetCaption;
  If S = '' Then
    Result := Inherited GetDisplayName
  Else
    Result := S
End;

{-------------------------------------}

Function TPSCDefaultSlot.GetCaption: String;
Begin
  Result := Data.AsString[PRM_CAPTION]
End;

{-------------------------------------}

Procedure TPSCDefaultSlot.SetCaption(Const Value: String);
Begin
  Data.AsString[PRM_CAPTION] := Value;
  FDefaultCaption:=PSCConstDefaultCaptionToID(Value);
End;

{-------------------------------------}

Procedure TPSCDefaultSlot.ReadCaption(Reader: TReader);
Begin
  FDefaultCaption := Reader.ReadInteger;
  If Not IsCaptionStored Then
    SetCaption(PSCConstDefaultCaptionFromID(FDefaultCaption));
End;

{-------------------------------------}

Procedure TPSCDefaultSlot.WriteCaption(Writer: TWriter);
Begin
  Writer.WriteInteger(FDefaultCaption)
End;

{-------------------------------------}

Procedure TPSCDefaultSlot.DefineProperties(Filer: TFiler);
Begin
  Inherited DefineProperties(Filer);
  Filer.DefineProperty(
    'DefaultCaption',ReadCaption,WriteCaption, //don't resource
      FDefaultCaption <> 0
    )
End;

{-------------------------------------}

Function TPSCDefaultSlot.IsCaptionStored: Boolean;
Begin
  Result := FDefaultCaption = 0
End;

{-------------------------------------}

Procedure TPSCButtonSlot.DoCreateItem;
Begin
  FImage := TPSCDrawButton.Create(TPSCGraphicItems(Collection).Owner);
End;

{-------------------------------------}

Function TPSCButtonSlot.GetAction: TPSCButtonsActions;
Begin
  Result := TPSCButtonsActions(byte(Data.AsCardinal[PRM_BUTTONACTION]))
End;

{-------------------------------------}

Procedure TPSCButtonSlot.SetAction(Value: TPSCButtonsActions);
Begin
  Data.AsCardinal[PRM_BUTTONACTION] := byte(Value)
End;

{-------------------------------------}

Procedure TPSCGraphicItems.Update(Item: TCollectionItem);
Var
  Section: TPSCCustomSection;
  Slot: TPSCGraphicItem;
  R: TRect;
Begin
  Section := TPSCCustomSection(GetOwner);
  If Section.FValidator <> Nil Then
    Begin
      If Item = Nil Then
        With Section Do
          Begin
            FValidator.Invalidate;
            If Parent <> Nil Then
              Parent.Event(Section,EV_SECTIONCHANGED,nil,0)
          End
      Else
        Begin
          Slot := TPSCGraphicItem(Item);
          If Slot.EventType And EV_ITEMCHANGED <> 0 Then
            With Section Do
              Begin
                SetKind(0);
                FValidator.Invalidate
              End
          Else
            With Slot.Bounds Do
              Begin
                R := GetBounds;
                With Section.GetBounds Do
                  OffsetRect(R,Left,Top);
                Section.FValidator.InvalidateRect(R,true)
              End;
          If Section.Parent <> Nil Then
            Section.Parent.Event(
              Section,Slot.EventType,Slot.Data.GetInstance,0);
        End
    End
End;

{-------------------------------------}

Constructor TPSCEnumColors.Create(Section: TPersistent; Index: integer);
Begin
  Inherited Create(Section);
  FIndex := Index
End;

{-------------------------------------}

Function TPSCEnumColors.Next(Count: Integer; var Buffer:TPSCColorDatas): Integer;
Var
  Slot: IPSCDataAccess;
  Section: TPSCCustomSection;
Begin
  Result := 0;
  If Not (GetOwner Is TPSCCustomSection) Then
    exit;
  Section := TPSCCustomSection(GetOwner);
  While (Result < Count) And (FIndex < Section.Count) Do
    Begin
      Slot := Section.Items[FIndex];
      With TPSCColorDatas(Buffer)[Result] Do
        Begin
          Size := SizeOf(TPSCColorData);
          inc(Result);
          inc(FIndex);
          Color := TPSCColor(Slot.AsCardinal[PRM_COLOR]);
          Hint := Slot.AsString[PRM_HINT];
          Caption := Slot.AsString[PRM_CAPTION];
          Cookie := FIndex
        End
    End
End;

{-------------------------------------}

Procedure TPSCEnumColors.Reset;
Begin
  FIndex := 0
End;

{-------------------------------------}

Constructor TPSCCustomSection.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner);
  If AOwner <> Nil Then
    AOwner.GetInterface(IPSCValidator,FValidator);
  CreateSlots;
  With FSection As IPSCDataAccess Do
    Begin
      AsObject[PRM_OWNER] := Self;
      AsCardinal[PRM_ALIGNMENT] := Cardinal(taLeftJustify);
      AsCardinal[PRM_BORDERCOLOR] := Cardinal(clPSCBtnShadow);
      AsBoolean[PRM_VISIBLE] := true;
      AsBoolean[PRM_ENABLED] := true
    End
End;

{-------------------------------------}

Destructor TPSCCustomSection.Destroy;
Begin
  FSlots.Free;
  Inherited Destroy
End;

{-------------------------------------}

Procedure TPSCCustomSection.Assign(Source: TPersistent);
Begin
  If Source Is TPSCCustomSection Then
    With Source As TPSCCustomSection Do
      Begin
        FSection := Self.FSection;
        FParent := Self.FParent;
        FValidator := Self.FValidator;
        FSlots := Self.FSlots;
      End
  Else
    Inherited Assign(Source)
End;

{-------------------------------------}

Function TPSCCustomSection.GetAlignment: TAlignment;
Begin
  With FSection As IPSCDataAccess Do
    Result := TAlignment(AsCardinal[PRM_ALIGNMENT])
End;

{-------------------------------------}

Procedure TPSCCustomSection.SetAlignment(Value: TPSCAlignment);
Begin
  With FSection As IPSCDataAccess Do
    AsCardinal[PRM_ALIGNMENT] := ord(Value)
End;

{-------------------------------------}

Function TPSCCustomSection.GetBorderColor: TPSCColor;
Begin
  With FSection As IPSCDataAccess Do
    Result := TPSCColor(AsCardinal[PRM_BORDERCOLOR])
End;

{-------------------------------------}

Procedure TPSCCustomSection.SetBorderColor(Value: TPSCColor);
Begin
  With FSection As IPSCDataAccess Do
    AsCardinal[PRM_BORDERCOLOR] := ord(Value)
End;

{-------------------------------------}

Function TPSCCustomSection.GetEnabled: Boolean;
Begin
  With FSection As IPSCDataAccess Do
    Result := AsBoolean[PRM_ENABLED]
End;

{-------------------------------------}

Procedure TPSCCustomSection.SetEnabled(Value: boolean);
Begin
  With FSection As IPSCDataAccess Do
    AsBoolean[PRM_ENABLED] := Value
End;

{-------------------------------------}

Function TPSCCustomSection.GetShowSeparator: Boolean;
Begin
  With FSection As IPSCDataAccess Do
    Result := AsBoolean[PRM_SHOWSEPARATOR]
End;

{-------------------------------------}

Procedure TPSCCustomSection.SetShowSeparator(Value: boolean);
Begin
  With FSection As IPSCDataAccess Do
    AsBoolean[PRM_SHOWSEPARATOR] := Value
End;

{-------------------------------------}

Procedure TPSCCustomSection.SetVisible(Value: boolean);
Begin
  With FSection As IPSCDataAccess Do
    AsBoolean[PRM_VISIBLE] := Value
End;

{-------------------------------------}

Function TPSCCustomSection.GetHint: String;
Begin
  With FSection As IPSCDataAccess Do
    Result := AsString[PRM_HINT]
End;

{-------------------------------------}

Procedure TPSCCustomSection.SetHint(Const Value: String);
Begin
  With FSection As IPSCDataAccess Do
    AsString[PRM_HINT] := Value
End;

{-------------------------------------}

Function TPSCCustomSection.GetKind: Word;
Begin
  With Section As IPSCDataAccess Do
    Result := WORD(AsCardinal[PRM_KIND])
End;

{-------------------------------------}

Procedure TPSCCustomSection.SetKind(Value: Word);
Begin
  If GetKind <> Value Then
    Begin
      With Section As IPSCDataAccess Do
        AsCardinal[PRM_KIND] := Value;
      If Value <> 0 Then
        Begin
          With Section As IPSCDataAccess Do
            AsCardinal[PRM_DEFAULTKIND] := Value;
          BeginUpdate;
          Try
            FSlots.Clear;
            With Section As IPSCDataAccess Do
              AsCardinal[PRM_KIND] := GetDefaultKind;
            CreateDefaultSlots
          Finally
            EndUpdate
          End;
          With Section As IPSCDataAccess Do
            AsCardinal[PRM_KIND] := GetDefaultKind;
          If FValidator <> Nil Then
            FValidator.Invalidate
        End
    End
End;

{-------------------------------------}

Function TPSCCustomSection.GetDefaultKind: Word;
Begin
  With Section As IPSCDataAccess Do
    Result := WORD(AsCardinal[PRM_DEFAULTKIND])
End;

{-------------------------------------}

Procedure TPSCCustomSection.SetDefaultKind(Value: Word);
Begin
  If GetDefaultKind <> Value Then
    If GetKind = 0 Then
      With Section As IPSCDataAccess Do
        AsCardinal[PRM_DEFAULTKIND] := Value
    Else
      SetKind(Value)
End;

{-------------------------------------}

Procedure TPSCCustomSection.ReadDefaultKind(Reader: TReader);
Begin
  SetDefaultKind(Reader.ReadInteger)
End;

{-------------------------------------}

Procedure TPSCCustomSection.WriteDefaultKind(Writer: TWriter);
Begin
  Writer.WriteInteger(GetDefaultKind)
End;

{-------------------------------------}

Procedure TPSCCustomSection.DefineProperties(Filer: TFiler);
Begin
  Inherited DefineProperties(Filer);
  Filer.DefineProperty(
    'DefaultKind',ReadDefaultKind,WriteDefaultKind, //don't resource
    (GetKind = 0) And (GetDefaultKind <> GetKind)
    )
End;

{-------------------------------------}

Function TPSCCustomSection.IsSlotsStored: Boolean;
Begin
  Result := (GetKind = 0)
End;

{-------------------------------------}

Function TPSCCustomSection.IsHintStored: Boolean;
Begin
  Result := (GetHint <> '')
End;

{-------------------------------------}

Function TPSCCustomSection.GetItemAsData(Index: Integer): IPSCDataAccess;
Begin
  Result := GetItem(Index) As IPSCDataAccess
End;

{-------------------------------------}

Function TPSCCustomSection.IsDesignTime: Boolean;
Var
  Ctrl: TControl;
Begin
  Ctrl := PSCGetParentControl(GetOwner);
  If Ctrl <> Nil Then
    Result := csDesigning In Ctrl.ComponentState
  Else
    Result := false
End;

{-------------------------------------}

Function TPSCCustomSection.Get_Boolean(IDParam: integer): Boolean;
Begin
  Case IDParam Of
    PRM_VISIBLE:
      Result := GetVisible;
    PRM_ENABLED:
      Result := GetEnabled;
    PRM_SHOWSEPARATOR:
      Result := GetShowSeparator;
    PRM_DESIGNTIME:
      Result := IsDesignTime;
  Else
    Result := false
  End
End;

{-------------------------------------}

Function TPSCCustomSection.Get_Cardinal(IDParam: integer): Cardinal;
Begin
  Result := 0;
  Case IDParam Of
    PRM_ALIGNMENT:
      Result := Cardinal(GetAlignment);
    PRM_BORDERCOLOR:
      Result := Cardinal(GetBorderColor);
    PRM_BCKCOLOR,PRM_PRESSEDCOLOR:
      If FParent <> Nil Then
        Result := FParent.Get_Cardinal(IDParam)
      Else
        Begin
          If IDParam = PRM_BCKCOLOR Then
            Result := Cardinal(clPSCBtnFace)
          Else
            Result := Cardinal(clPSCBtnHighlight)
        End;
    PRM_SLOTWIDTH,
      PRM_SLOTHEIGHT:
      If FParent <> Nil Then
        Result := FParent.Get_Cardinal(IDParam)
      Else
        Result := 18;
    PRM_COLUMNCOUNT:
      If FParent <> Nil Then
        Result := FParent.Get_Cardinal(IDParam)
      Else
        Result := 8;
    PRM_HINTKIND:
      If FParent <> Nil Then
        Result := FParent.Get_Cardinal(IDParam);
    PRM_KIND:
      Result := GetKind;
    PRM_DEFAULTKIND:
      Result := GetDefaultKind;
    PRM_HIGHLIGHT:
      Begin
        If FParent <> Nil Then
          Result := FParent.Get_Cardinal(IDParam)
        Else
          Result := Cardinal(clPSCNone);
      End
  End
End;

{-------------------------------------}

Function TPSCCustomSection.Get_Integer(IDParam: integer): longint;
Begin
  Result := -1
End;

{-------------------------------------}

Function TPSCCustomSection.Get_String(IDParam: integer): String;
Begin
  If IDParam = PRM_HINT Then
    Result := GetHint
  Else
    Result := ''
End;

{-------------------------------------}

Function TPSCCustomSection.Get_Object(IDParam: integer): TObject;
Begin
  Case IDParam Of
    PRM_FONT:
      If FParent <> Nil Then
        Result := FParent.Get_Object(IDParam)
      Else
        Result := Nil;
    PRM_CANVAS:
      Begin
        If FValidator <> Nil Then
          Result := FValidator.GetCanvas
        Else
          Result := Nil
      End
  Else
    Result := Nil
  End
End;

{-------------------------------------}

Function TPSCCustomSection.Get_Double(IDParam: integer): Double;
Begin
  Result := 0
End;

{-------------------------------------}

Procedure TPSCCustomSection.Set_Boolean(IDParam: integer; Value: Boolean);
Begin
  Case IDParam Of
    PRM_VISIBLE:
      SetVisible(Value);
    PRM_ENABLED:
      SetEnabled(Value);
    PRM_SHOWSEPARATOR:
      SetShowSeparator(Value);
  End
End;

{-------------------------------------}

Procedure TPSCCustomSection.Set_Cardinal(IDParam: integer; Value: Cardinal);
Begin
  Case IDParam Of
    PRM_ALIGNMENT:
      SetAlignment(TAlignment(Value));
    PRM_BORDERCOLOR:
      SetBorderColor(TPSCColor(Value));
    PRM_KIND:
      SetKind(Value);
    PRM_DEFAULTKIND:
      SetDefaultKind(Value);
  End
End;

{-------------------------------------}

Procedure TPSCCustomSection.Set_Integer(IDParam: integer; Value: longint);
Begin
End;

{-------------------------------------}

Procedure TPSCCustomSection.Set_String(IDParam: integer; Const Value: String);
Begin
  If IDParam = PRM_HINT Then
    SetHint(Value)
End;

{-------------------------------------}

Procedure TPSCCustomSection.Set_Object(IDParam: integer; Value: TObject);
Begin
End;

{-------------------------------------}

Procedure TPSCCustomSection.Set_Double(IDParam: integer; Value: Double);
Begin
End;

{-------------------------------------}

Function TPSCCustomSection.GetParent: IPSCDataReader;
Begin
  Result := Parent
End;

{-------------------------------------}

Function TPSCCustomSection.GetCount: Integer;
Begin
  Result := FSlots.Count
End;

{-------------------------------------}

Function TPSCCustomSection.GetItem(Index: Integer): IPSCInterface;
Begin
  Result := TPSCGraphicItem(FSlots.Items[Index]).Data
End;

{-------------------------------------}

Function TPSCCustomSection.GetItemAsDraw(Index: Integer): IPSCDraw;
Begin
  Result := TPSCGraphicItem(FSlots.Items[Index]).Image
End;

{-------------------------------------}

Function TPSCCustomSection.GetItemAsBounds(Index: Integer): IPSCBounds;
Begin
  Result := TPSCGraphicItem(FSlots.Items[Index]).Bounds
End;

{-------------------------------------}

Function TPSCCustomSection.GetItemAt(P: TPoint): IPSCInterface;
Var
  i: integer;
Begin
  For i := 0 To GetCount - 1 Do
    With GetItemAsBounds(i) Do
      If GetVisible And PtInRect(GetBounds,P) Then
        Begin
          Result := GetItem(i);
          exit
        End;
  Result := Nil
End;

{-------------------------------------}

Function TPSCCustomSection.GetEnumColor(AColor: TPSCColor): Boolean;
Var
  I: Integer;
Begin
  For I := 0 To Count - 1 Do
    With Items[I] Do
      If (AColor = TPSCColor(AsCardinal[PRM_COLOR])) And
        AsBoolean[PRM_VISIBLE] And AsBoolean[PRM_ENABLED] Then
        Begin
          Result := true;
          exit
        End;
  Result := false
End;

{-------------------------------------}

Procedure TPSCCustomSection.SetEnumColor(AColor: TPSCColor; Value: Boolean);
Var
  I: Integer;
Begin
  For I := 0 To Count - 1 Do
    With Items[I] Do
      If (Cardinal(AColor) = AsCardinal[PRM_COLOR]) And
        AsBoolean[PRM_VISIBLE] And AsBoolean[PRM_ENABLED] Then
        If Not Value Then
          FSlots.Items[I].Free
        Else
          exit;
  If Value Then
    Add.Data.AsCardinal[PRM_COLOR] := Cardinal(AColor)
End;

{-------------------------------------}

Function TPSCCustomSection.GetColorsEnum: IPSCDataEnumerator;
Begin
  Result := TPSCEnumColors.Create(Self,0)
End;

{-------------------------------------}

Function TPSCCustomSection.GetBounds: TRect;
Var
  Ctrl: TControl;
Begin
  Ctrl := PSCGetParentControl(GetOwner);
  If Ctrl <> Nil Then
    Result := Ctrl.BoundsRect
  Else
    Result := PSCRect(0,0,0,0);
End;

{-------------------------------------}

Procedure TPSCCustomSection.SetBounds(Const Value: TRect);
Var
  Ctrl: TControl;
Begin
  Ctrl := PSCGetParentControl(Self);
  If Ctrl <> Nil Then
    With Value Do
      Ctrl.SetBounds(Left,Top,Right - Left,Bottom - Top);
  With Section As IPSCBounds Do
    SetBounds(Value)
End;

{-------------------------------------}

Function TPSCCustomSection.GetAlign: TAlign;
Var
  Ctrl: TControl;
Begin
  Ctrl := PSCGetParentControl(GetOwner);
  If Ctrl <> Nil Then
    Result := Ctrl.Align
  Else
    Result := alTop
End;

{-------------------------------------}

Function TPSCCustomSection.GetVisible: Boolean;
Begin
  With FSection As IPSCDataAccess Do
    Result := AsBoolean[PRM_VISIBLE]
End;

{-------------------------------------}

Function TPSCCustomSection.Measure(Var NewWidth,NewHeight: Integer): Boolean;
Begin
  With FSection As IPSCBounds Do
    Result := Measure(NewWidth,NewHeight)
End;

{-------------------------------------}

Procedure TPSCCustomSection.Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect);
Begin
  With FSection As IPSCDraw Do
    Draw(Canvas,BoundsRect)
End;

{-------------------------------------}

Function TPSCCustomSection.Add: TPSCGraphicItem;
Begin
  With FSlots Do
    Begin
      BeginUpdate;
      Try
        Result := Add As TPSCGraphicItem
      Finally
        EndUpdate
      End
    End
End;

{-------------------------------------}

Procedure TPSCCustomSection.BeginUpdate;
Begin
  FSlots.BeginUpdate
End;

{-------------------------------------}

Procedure TPSCCustomSection.EndUpdate;
Begin
  FSlots.EndUpdate
End;

{-------------------------------------}

Function TPSCCustomSection.GetNextItem(Slot: IPSCDataAccess;
  Dir: TPSCDirection): IPSCDataAccess;
Var
  Index: Integer;
  ASlot: TPSCColorSlot;
Begin
  Result := Nil;
  If Slot = Nil Then
    exit;
  ASlot := TPSCColorSlot(Slot.AsObject[PRM_OWNER]);
  If ASlot = Nil Then
    exit;
  Index := ASlot.Index;
  Repeat
    Case Dir Of
      sdLeft,sdUp:
        Begin
          If Index >= 0 Then
            Dec(Index)
        End
    Else
      If Index <= Count - 1 Then
        Inc(Index)
    End;
  Until (Index < 0) Or (Index > Count - 1) Or
    (Items[Index].AsBoolean[PRM_VISIBLE] And
    Items[Index].AsBoolean[PRM_ENABLED]);
  If (Index >= 0) And (Index < Count) Then
    Result := Items[Index]
End;

{-------------------------------------}

Procedure TPSCCustomSection.Event(Sender: IPSCObject; EvType:integer;LowData:TObject;HighData: integer);
Var
  Item: TPSCGraphicItem;
Begin
  If LowData <> nil Then
    With TPSCDrawSlot(LowData) As IPSCDataAccess Do
      Begin
        Item := TPSCGraphicItem(AsObject[PRM_OWNER]);
        If Item = Nil Then
          exit;
        Item.EventType := EvType;
        If EvType And EV_ITEMSTATECHANGED = 0 Then
          SetKind(0);
        If EvType And EV_ITEMDELETED <> 0 Then
          Begin
            If FParent <> Nil Then
              FParent.Event(Self,EvType,LowData,0)
          End
        Else
          Item.Changed(false)
      End
  Else
    Begin
      If EvType And EV_ITEMCHANGED <> 0 Then
        SetKind(0);
      FSlots.Changed
    End
End;

{-------------------------------------}

Function TPSCCustomSection.GetDefaultCaption: String;
Begin
  Result := ''
End;

{-------------------------------------}

Function TPSCCustomSection.IsKindStored: Boolean;
Begin
  Result := Kind <> 0
End;

{-------------------------------------}

Constructor TPSCColorSection.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner);
  ShowCaption := true;
  If IsDesignTime Then
    Kind := cskFontColors
End;

{-------------------------------------}

Function TPSCColorSection.GetNextItem(Slot: IPSCDataAccess;
  Dir: TPSCDirection): IPSCDataAccess;
Var
  Index,ColumnCount: integer;
  ASlot: TPSCColorSlot;
Begin
  Result := Nil;
  If Slot = Nil Then
    exit;
  If Dir In [sdLeft,sdRight] Then
    Result := Inherited GetNextItem(Slot,Dir)
  Else
    Begin
      ASlot := TPSCColorSlot(Slot.AsObject[PRM_OWNER]);
      If ASlot = Nil Then
        exit;
      Index := ASlot.Index;
      With Section As IPSCDataAccess Do
        ColumnCount := AsCardinal[PRM_COLUMNCOUNT];
      If Dir = sdUp Then
        Begin
          If Index >= ColumnCount Then
            Result := Items[Index - ColumnCount]
        End
      Else
        If Index < Count - 1 - ColumnCount Then
          Result := Items[Index + ColumnCount];
      If (Result = Nil) Or Not Result.AsBoolean[PRM_VISIBLE] Or
        Not Result.AsBoolean[PRM_ENABLED] Then
        Begin
          If Dir = sdUp Then
            Dir := sdLeft
          Else
            Dir := sdRight;
          Result := Inherited GetNextItem(Result,Dir)
        End
    End
End;

{-------------------------------------}

Function TPSCColorSection.GetCaption: String;
Begin
  With Section As IPSCDataAccess Do
    Result := AsString[PRM_CAPTION]
End;

{-------------------------------------}

Procedure TPSCColorSection.SetCaption(Const Value: String);
Begin
  With Section As IPSCDataAccess Do
    AsString[PRM_CAPTION] := Value
End;

{-------------------------------------}

Function TPSCColorSection.GetShowCaption: Boolean;
Begin
  With Section As IPSCDataAccess Do
    Result := AsBoolean[PRM_SHOWCAPTION]
End;

{-------------------------------------}

Procedure TPSCColorSection.SetShowCaption(Value: boolean);
Begin
  With Section As IPSCDataAccess Do
    AsBoolean[PRM_SHOWCAPTION] := Value
End;

{-------------------------------------}

Function TPSCColorSection.GetDefaultCaption: String;
Begin
  Case Cardinal(DefaultKind) Of
    1: Result := PSCConsts.CustomColors;
    2: Result := PSCConsts.StdColors;
    3: Result := PSCConsts.DocColors;
    4: Result := PSCConsts.WindowsColors;
    6: Result := PSCConsts.FontColors;
  Else
    Result := Inherited GetDefaultCaption
  End
End;

{-------------------------------------}

Function TPSCColorSection.GetKind: TPSCColorSlotsKind;
Begin
  Result := TPSCColorSlotsKind(Inherited Kind)
End;

{-------------------------------------}

Procedure TPSCColorSection.SetKind(Value: TPSCColorSlotsKind);
Begin
  If (Value <> Kind) And (Value = cskBkgndColors) Then
    ShowCaption := false;
  Inherited SetKind(WORD(Value))
End;

{-------------------------------------}

Function TPSCColorSection.IsCaptionStored: Boolean;
Begin
  Result := (Caption <> GetDefaultCaption);
End;

{-------------------------------------}

Procedure TPSCColorSection.CreateSlots;
Begin
  FSection := TPSCDrawColorSection.Create(Self);
  FSlots := TPSCGraphicItems.Create(Self,TPSCColorSlot);
  If PSCColorAlignerSink = Nil Then
    PSCColorAlignerSink := TPSCAlignerSink2.Create(Nil);
  FAligner := PSCColorAlignerSink
End;

{-------------------------------------}

Procedure TPSCColorSection.CreateDefaultSlots;
Var
  I: Integer;
  S: String;

  Procedure AddColor(AColor: TPSCColor; Const AHint: String);
  Begin
    With TPSCColorSlot(Add) Do
      Begin
        Color := AColor;
        Hint := AHint
      End
  End;

Begin
  Case Kind Of
    cskCustomColors:
      For I := 0 To 15 Do
        TPSCColorSlot(Add).Color := clPSCWhite;
    cskStdColors:
      For I := 0 To cPSCStdColorCount - 1 Do
        TPSCColorSlot(Add).Color := TPSCColor(cPSCStdColors[I]);
    cskWinColors:
      Begin
        For I := 0 To 30 Do
          Begin
            if (I = 25) or
              ((not PSCWindowsXPOrHigher) and (I in [COLOR_MENUHILIGHT, COLOR_MENUBAR])) then
              Continue;
            S := PSCColorToString(TColor(UINT(I) or clSystemColor));
            System.Delete(S,1,2);
            AddColor(TPSCColor(UINT(I) Or $80000000),S)
          End;
      End;
    cskBkgndColors:
      For I := 0 To cPSCBkgndColorCount - 1 Do
        TPSCColorSlot(Add).Color := cPSCBkgndColors[I];
    cskFontColors:
      For I := Low(cPSCColors) To High(cPSCColors) Do
        TPSCColorSlot(Add).Color := TPSCColor(cPSCColors[I]);
  End;
  Caption := GetDefaultCaption
End;

{-------------------------------------}

Function TPSCColorSection.Get_Boolean(IDParam: integer): Boolean;
Begin
  If IDParam = PRM_SHOWCAPTION Then
    Result := GetShowCaption
  Else
    Result := Inherited Get_Boolean(IDParam)
End;

{-------------------------------------}

Procedure TPSCColorSection.Set_Boolean(IDParam: integer; Value: Boolean);
Begin
  If IDParam = PRM_SHOWCAPTION Then
    SetShowCaption(Value)
  Else
    Inherited Set_Boolean(IDParam,Value)
End;

{-------------------------------------}

Function TPSCColorSection.Get_String(IDParam: integer): String;
Begin
  If IDParam = PRM_CAPTION Then
    Result := GetCaption
  Else
    Result := Inherited Get_String(IDParam)
End;

{-------------------------------------}

Procedure TPSCColorSection.Set_String(IDParam: integer; Const Value: String);
Begin
  If IDParam = PRM_CAPTION Then
    SetCaption(Value)
  Else
    Inherited Set_String(IDParam,Value)
End;

{-------------------------------------}

Constructor TPSCDefaultSection.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner);
  ShowColors := true;
  If IsDesignTime Then
    Kind := dskAuto
End;

{-------------------------------------}

Function TPSCDefaultSection.GetShowColors: Boolean;
Begin
  With Section As IPSCDataAccess Do
    Result := AsBoolean[PRM_SHOWCOLORS]
End;

{-------------------------------------}

Procedure TPSCDefaultSection.SetShowColors(Value: boolean);
Begin
  With Section As IPSCDataAccess Do
    AsBoolean[PRM_SHOWCOLORS] := Value
End;

{-------------------------------------}

Procedure TPSCDefaultSection.SetKind(Value: TPSCDefaultSlotsKind);
Begin
  Inherited SetKind(WORD(Value))
End;

{-------------------------------------}

Function TPSCDefaultSection.GetKind: TPSCDefaultSlotsKind;
Begin
  Result := TPSCDefaultSlotsKind(Inherited Kind)
End;

{-------------------------------------}

Procedure TPSCDefaultSection.CreateSlots;
Begin
  FSection := TPSCDrawButtonSection.Create(Self);
  FSlots := TPSCGraphicItems.Create(Self,TPSCDefaultSlot);
  If PSCButtonAlignerSink = Nil Then
    PSCButtonAlignerSink := TPSCAlignerSink3.Create(Nil);
  FAligner := PSCButtonAlignerSink
End;

{-------------------------------------}

Procedure TPSCDefaultSection.CreateDefaultSlots;
Begin
  Case Kind Of
    dskAuto:
      With TPSCDefaultSlot(Add) Do
        Begin
          Color := clPSCBlack;
          Caption := PSCConsts.Auto;
          Hint := PSCConsts.AutoColor
        End;
    dskNone:
      With TPSCDefaultSlot(Add) Do
        Begin
          Caption := PSCConsts.None;
          Hint := PSCConsts.NoHighLight;
          Color := clPSCDefault;
        End
  End
End;

{-------------------------------------}

Function TPSCDefaultSection.Get_Boolean(IDParam: integer): Boolean;
Begin
  If IDParam = PRM_SHOWCOLORS Then
    Result := GetShowColors
  Else
    Result := Inherited Get_Boolean(IDParam)
End;

{-------------------------------------}

Procedure TPSCDefaultSection.Set_Boolean(IDParam: integer; Value: Boolean);
Begin
  If IDParam = PRM_SHOWCOLORS Then
    SetShowColors(Value)
  Else
    Set_Boolean(IDParam,Value)
End;

{-------------------------------------}

Constructor TPSCButtonSection.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner);
  ShowColors := true;
  If IsDesignTime Then
    Kind := bskMoreColors
End;

{-------------------------------------}

Procedure TPSCButtonSection.SetKind(Value: TPSCButtonSlotsKind);
Begin
  Inherited SetKind(TPSCDefaultSlotsKind(ord(Value)))
End;

{-------------------------------------}

Function TPSCButtonSection.GetKind: TPSCButtonSlotsKind;
Begin
  Result := TPSCButtonSlotsKind(ord(Inherited Kind))
End;

{-------------------------------------}

Procedure TPSCButtonSection.CreateSlots;
Begin
  FSection := TPSCDrawButtonSection.Create(Self);
  FSlots := TPSCGraphicItems.Create(Self,TPSCButtonSlot);
  If PSCButtonAlignerSink = Nil Then
    PSCButtonAlignerSink := TPSCAlignerSink3.Create(Nil);
  FAligner := PSCButtonAlignerSink
End;

{-------------------------------------}

Procedure TPSCButtonSection.CreateDefaultSlots;
Begin
  BorderColor := clPSCBtnHighlight;
  With TPSCButtonSlot(Add) Do
    Begin
      Caption := PSCConsts.MoreColorsCapt;
      Hint := PSCConsts.MoreColors;
      SelectAction := SelectAction + [baSetButtonColor];
      Color := clPSCBlack;
    End
End;

{-------------------------------------}

Constructor TPSCCustomSectionControl.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FCanvas := TPSCCanvas(TPSCControlCanvas.Create);
  TControlCanvas(FCanvas).Control := Self;
  CreateSection;
  Height := 25;
  ParentShowHint := false;
  ShowHint := false;
  Align := alTop
End;

{-------------------------------------}

Destructor TPSCCustomSectionControl.Destroy;
Begin
  With TPSCCustomSection(FSection.GetInstance) Do
    Begin
      FValidator := Nil;
      While Count > 0 Do
        Slots.Items[Count - 1].Free;
      FSection := Nil;
    End;
  FCanvas.Free;
  Inherited Destroy
End;

{-------------------------------------}

Procedure TPSCCustomSectionControl.WMPaint(Var Message: TWMPaint);
Begin
  If Message.DC <> 0 Then
    Begin
      FCanvas.Lock;
      Try
        FCanvas.Handle := Message.DC;
        Try
          Paint
        Finally
          FCanvas.Handle := 0
        End
      Finally
        FCanvas.Unlock
      End
    End
End;

{-------------------------------------}

Procedure TPSCCustomSectionControl.Paint;
Begin
  (FSection As IPSCDraw).Draw(FCanvas,BoundsRect)
End;

{-------------------------------------}

Procedure TPSCCustomSectionControl.InvalidateRegion(Rect: TRect; Erase: Boolean);
Begin
  If HasParent And Parent.HandleAllocated Then
    InvalidateRect(Parent.Handle,@Rect,Erase)
End;                              

{-------------------------------------}

Procedure TPSCCustomSectionControl.InvalidateControl;
Begin
  RequestAlign;
  Invalidate;
End;

{-------------------------------------}

Procedure TPSCCustomSectionControl.RequestAlign;
Var
  Section: TPSCCustomSection;
Begin
  If FSection <> Nil Then
    Begin
      Section := FSection.GetInstance As TPSCCustomSection;
      If Section.Aligner <> Nil Then
        Begin
          Section.Aligner.Realign(Section,Section);
          Inherited RequestAlign;
        End;
    End;
End;

{-------------------------------------}

Function TPSCCustomSectionControl.GetInstance: TPersistent;
Begin
  Result := Self;
End;

{-------------------------------------}

Function TPSCCustomSectionControl.GetCanvas: TPSCCanvas;
Begin
  If HasParent And Parent.HandleAllocated Then
    Result := FCanvas
  Else
    Result := Nil
End;

{-------------------------------------}

Function TPSCDefaultSectionControl.GetSection: TPSCDefaultSection;
Begin
  Result := FSection.GetInstance As TPSCDefaultSection
End;

{-------------------------------------}

Procedure TPSCDefaultSectionControl.CreateSection;
Begin
  FSection := TPSCDefaultSection.Create(Self)
End;

{-------------------------------------}

Procedure TPSCDefaultSectionControl.SetSection(Value: TPSCDefaultSection);
Begin
  FSection := Value
End;

{-------------------------------------}

Function TPSCButtonSectionControl.GetSection: TPSCButtonSection;
Begin
  Result := FSection.GetInstance As TPSCButtonSection
End;

{-------------------------------------}

Procedure TPSCButtonSectionControl.CreateSection;
Begin
  FSection := TPSCButtonSection.Create(Self)
End;

{-------------------------------------}

Procedure TPSCButtonSectionControl.SetSection(Value: TPSCButtonSection);
Begin
  FSection := Value
End;

{-------------------------------------}

Function TPSCColorSectionControl.GetSection: TPSCColorSection;
Begin
  Result := FSection.GetInstance As TPSCColorSection
End;

{-------------------------------------}

Procedure TPSCColorSectionControl.CreateSection;
Begin
  FSection := TPSCColorSection.Create(Self)
End;

{-------------------------------------}

Procedure TPSCColorSectionControl.SetSection(Value: TPSCColorSection);
Begin
  FSection := Value
End;

{-------------------------------------}

Type
  TPSCColorBoxHint = Class(THintWindow)
  private
    FColorBox: TPSCCustomColorBox;
  protected
    Procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    Procedure ActivateHintData(Rect: TRect; Const AHint: String;
      AData: Pointer); override;
    Function IsHintMsg(Var Msg: TMsg): Boolean; override;
  End;

  TSectionAccess = Class(TPSCCustomSectionControl);
  TSection = Class(TPSCCustomSection);
  TSlotAccess = Class(TPSCColorSlot);

  {-------------------------------------}

Procedure TPSCColorBoxHint.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  Inherited Notification(AComponent,Operation);
  If (Operation = opRemove) And (FColorBox = AComponent) Then
    FColorBox := Nil;
End;

{-------------------------------------}

Procedure TPSCColorBoxHint.ActivateHintData(Rect: TRect; Const AHint: String;
  AData: Pointer);
Begin
  If TObject(AData) Is TPSCCustomColorBox Then
    Begin
      FColorBox := TPSCCustomColorBox(AData);
      FColorBox.FState := FColorBox.FState + [cbsFastHint];
      FreeNotification(FColorBox)
    End;
  Inherited ActivateHintData(Rect,AHint,AData)
End;

{-------------------------------------}

Function TPSCColorBoxHint.IsHintMsg(Var Msg: TMsg): Boolean;
Begin
  Result := Inherited IsHintMsg(Msg);
  If Result And (FColorBox <> Nil) Then
    With FColorBox Do
      Begin
        FState := FState - [cbsFastHint];
        If (Msg.Message = WM_KEYDOWN) And (Msg.hwnd = Handle) Then
          FState := FState + [cbsSkipMouseMove];
      End;
End;

{-------------------------------------}

procedure TPSCColorDlg.AddCustomColor(Color:TPSCColor);
var
  S:String;
begin
  With GetProp.CustomColors do
  begin
    If Count=16 then
      exit;
    S:='Color'+Char(Count+Ord('A'))+'='; //don't resource
    S:=S+PSCColorsManager.ColorToRGBStr(Color,ckHex);
    Add(S);
  end;
end;

{-------------------------------------}

procedure TPSCColorDlg.ClearCustomColors;
begin
  If FDialog<>nil then
    GetProp.CustomColors.Clear;
end;

{-------------------------------------}

Destructor TPSCColorDlg.Destroy;
Begin
  FDialog.Free;
  Inherited Destroy
End;

{-------------------------------------}

Function TPSCColorDlg.GetProp: TPSCColorDialog;
Begin
  If FDialog = Nil Then
    FDialog := TPSCColorDialog.Create(Nil);
  Result := FDialog;
End;

{-------------------------------------}

Function TPSCColorDlg.GetOptions: TColorDialogOptions;
Begin
  Result := GetProp.Options
End;

{-------------------------------------}

Procedure TPSCColorDlg.SetOptions(Value: TColorDialogOptions);
Begin
  GetProp.Options := Value
End;

{-------------------------------------}

Function TPSCColorDlg.GetHelpContext: THelpContext;
Begin
  Result := GetProp.HelpContext
End;

{-------------------------------------}

Procedure TPSCColorDlg.SetHelpContext(Value: THelpContext);
Begin
  With TColorDialog(GetProp) Do
    HelpContext := Value
End;

{-------------------------------------}

Function TPSCColorDlg.Execute(Var AColor: TPSCColor): Boolean;
Var
  K,I: Integer;
  ASlots: TPSCCustomSection;
  ASlot: TPSCColorSlot;
  ColorBox: TPSCCustomColorBox;
Begin
  With TColorDialog(GetProp) Do
    Begin
      If GetOwner Is TPSCCustomColorBox Then
        Begin
          ColorBox := (GetOwner As TPSCCustomColorBox);
          ASlots := Nil;
          For K := 0 To ColorBox.ControlCount - 1 Do
            Begin
              ASlots := ColorBox.Sections[K];
              If ASlots.Visible And (ASlots Is TPSCColorSection)
                And (ASlots.DefaultKind = WORD(cskCustomColors)) Then
                Break;
              ASlots := Nil
            End;
          If ASlots <> Nil Then
            Begin
              CustomColors.BeginUpdate;
              Try
                CustomColors.Clear;
                K := 0;
                For I := 0 To ASlots.Slots.Count - 1 Do
                  Begin
                    ASlot := TPSCColorSlot(ASlots.Slots.Items[I]);
                    If Not (ButtonState_Disabled In ASlot.State) And ASlot.Visible And
                      (ASlot.Color <> clPSCNone) Then
                      Begin
                        CustomColors.Add(PSCFormat(
                          'Color%s=%.6x', [Char(Ord('A') + K), //don't resource
                          ASlot.Color]));
                        K := K + 1;
                        If K = 16 Then
                          Break
                      End
                  End
              Finally
                CustomColors.EndUpdate
              End
            End
        End;
      Result := Execute;
      AColor := Color
    End
End;

{-------------------------------------}

Constructor TPSCCustomColorBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls,csFixedHeight] -
    [csClickEvents];
  FPressedColor := clPSCBtnHighlight;
  FColumnCount := 8;
  FSlotWidth := 18;
  FSlotHeight := 18;
  BorderWidth := 2;
  Width := 8 * 18 + 2 * 2;
  FCanRearrange := true;
  FHintKind := ckHexTriple;
  FSelectSameColors := true;
  AutoSize := true;
  ShowHint := true;
  TabStop := true;
  FHighlightColor := clPSCHighlight;
  FSelectedColor := clPSCNone;
  Initialize
End;

{-------------------------------------}

Destructor TPSCCustomColorBox.Destroy;
Begin
  While ControlCount > 0 Do
    Controls[ControlCount - 1].Free;
  Inherited Destroy
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.Initialize;
Begin
  If PSCAlignerSink = Nil Then
    PSCAlignerSink := TPSCAlignerSink.Create(Nil);
  If KeyboardSink = Nil Then
    KeyboardSink := TPSCKeyboardSink.Create(Nil);
  If MouseSink = Nil Then
    MouseSink := TPSCMouseSink.Create(Nil);
  FAligner := PSCAlignerSink;
  FKeyboard := KeyboardSink;
  FMouse := MouseSink;
  FColorDialog := TPSCColorDlg.Create(Self);
End;

{-------------------------------------}

Function TPSCCustomColorBox.IndexOfSlots(Section: TPSCCustomSection): Integer;
Var
  i: Integer;
Begin
  Result := -1;
  For i := 0 To ControlCount - 1 Do
    If Sections[i] = Section Then
      Begin
        Result := i;
        break
      End
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetNextItem(
  Slot: IPSCDataAccess; Dir: TPSCDirection
  ): IPSCDataAccess;
Var
  I,J,C: Integer;
  Section: TPSCCustomSection;
Begin
  If Slot <> Nil Then
    Begin
      Section := Slot.Parent.GetInstance As TPSCCustomSection;
      Slot := Section.GetNextItem(Slot,Dir)
    End
  Else
    Section := Nil;
  If Slot = Nil Then
    Begin
      C := ControlCount;
      If (C > 1) Or (Section = Nil) And (C <> 0) Then
        Begin
          I := IndexOfSlots(Section);
          If I = -1 Then
            I := C - 1;
          J := I;
          If Dir In [sdLeft,sdUp] Then
            Dir := sdLeft
          Else
            Dir := sdRight;
          Repeat
            If Dir = sdLeft Then
              If J = 0 Then
                J := C - 1
              Else
                J := J - 1
            Else
              If J = C - 1 Then
                J := 0
              Else
                J := J + 1;
            Section := Sections[J];
            If CanFocus And Section.Visible And Section.Enabled
              And (Section.Count > 0) Then
              Begin
                If Dir = sdLeft Then
                  Slot := Section.Items[Section.Count - 1]
                Else
                  Slot := Section.Items[0];
                If Not Slot.AsBoolean[PRM_ENABLED] Or
                  Not Slot.AsBoolean[PRM_VISIBLE] Then
                  Slot := Section.GetNextItem(Slot,Dir);
                If Slot <> Nil Then
                  Break
              End
          Until I = J
        End
    End;
  Result := Slot
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetItemAsSection(Index: Integer): TPSCCustomSection;
Begin
  With TSectionAccess(Controls[Index]) Do
    Result := FSection.GetInstance As TPSCCustomSection
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetActiveSlot(Value: TPSCColorSlot);
Begin
  If FReadOnly Then
    exit;
  If (Value <> Nil) And
    Not (Value.Section.Enabled And Value.Visible And Value.Enabled) Then
    Value := Nil;
  If FActiveSlot <> Value Then
    Begin
      If FActiveSlot <> Nil Then
        FActiveSlot.State := FActiveSlot.State - [ButtonState_Up,ButtonState_Down];
      FActiveSlot := Value;
      If Value <> Nil Then
        Begin
          If (ButtonState_Exclusive In Value.State) Or MouseCapture Then
            Value.State := Value.State - [ButtonState_Up] + [ButtonState_Down]
          Else
            Value.State := Value.State - [ButtonState_Down] + [ButtonState_Up];
        End;
      If Assigned(FOnActiveChange) Then
        FOnActiveChange(Self,Value)
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetSelectedSlot(Value: TPSCColorSlot);
Var
  State: TPSCButtonStates;
Begin
  If (Value <> Nil) And Not Value.Section.Enabled And
    Value.Visible And Value.Enabled Then
    exit;
  If FSelectedSlot <> Value Then
    Begin
      If Value <> Nil Then
        Begin
          State := Value.State;
          If Value Is TPSCButtonSlot Then
            Begin
              ActiveSlot := Value;
              Value.State := State - [ButtonState_Up] + [ButtonState_Down];
              DoButtonClick(Value,TPSCButtonSlot(Value).SelectAction);
              Value.State := State - [ButtonState_Down] + [ButtonState_Up];
              Exit
            End
          Else
            If Value = FActiveSlot Then
              State := State - [ButtonState_Up] + [ButtonState_Down];
        End;
      If FSelectedSlot <> Nil Then
        Begin
          State := FSelectedSlot.State - [ButtonState_Exclusive,ButtonState_Up,
            ButtonState_Down];
          If FSelectedSlot = FActiveSlot Then
            State := State + [ButtonState_Up];
          FSelectedSlot.State := State
        End;
      FSelectedSlot := Value;
      If Value <> Nil Then
        Value.State := State + [ButtonState_Exclusive];
      If Not (csLoading In ComponentState) Then
        If Value = Nil Then
          FSelectedColor := clPSCNone
        Else
          FSelectedColor := Value.Color;
      If FSelectSameColors Then
        UpdateSelection;
      If (ComponentState * [csLoading,csDestroying] = []) And Assigned
        (FOnSelected) Then
        FOnSelected(Self,Value)
    End;
   If Value <> Nil Then 
    Click
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetDefaultDialog: TPersistent;
Begin
  If FColorDialog <> Nil Then
    Result := FColorDialog.GetInstance
  Else
    Result := Nil
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetDefaultDialog(Value: TPersistent);
Begin
  If FColorDialog <> Nil Then
    FColorDialog.GetInstance.Assign(Value)
End;

{-------------------------------------}

Type
  TPSCStyle = cbsWordBk..cbsFrontPageBtn;
  TPSCStyleOptions = Procedure(ColorBox: TPSCCustomColorBox);

  TPSCStyleRec = Packed Record
    Class_Type: TControlClass;
    Kind: Word
  End;

  TPSCStyleDef = Array[0..MaxInt Div SizeOf(TPSCStyleRec) - 1] Of TPSCStyleRec;
  PPSCStyleDef = ^TPSCStyleDef;

Procedure FrontPageOptions(ColorBox: TPSCCustomColorBox);
Begin
  With ColorBox Do
    Begin
      HighlightActive := true;
      HighlightColor := clPSCBtnShadow;
      With Sections[2] As TPSCButtonSection Do
        Begin
          ShowColors := false;
          Alignment := taLeftJustify;
          With Slots.Items[0] As TPSCButtonSlot Do
            Begin
              Caption := PSCConsts.CustomColors;
              Color := clPSCNone;
              Hint := '';
              SelectAction := SelectAction - [baSetButtonColor]
            End
        End;
      With Sections[3] As TPSCColorSection Do
        ShowCaption := false;
      With Sections[4] As TPSCButtonSection Do
        With Slots.Items[0] As TPSCButtonSlot Do
          SelectAction := SelectAction - [baSetCustomColor]
    End
End;

Const
  StyleMap: Array[TPSCColorBoxOption] Of TPSCStyleRec = (
    (Class_Type: TPSCDefaultSectionControl; Kind: WORD(dskAuto)),
    (Class_Type: TPSCDefaultSectionControl; Kind: WORD(dskNone)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskCustomColors)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskStdColors)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskDocColors)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskWinColors)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskBkgndColors)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskFontColors)),
    (Class_Type: TPSCButtonSectionControl; Kind: WORD(bskMoreColors))
    );

  WordBkStyleCount = 2;
  WordBkStyle: Array[0..WordBkStyleCount - 1] Of TPSCStyleRec =
  (
    (Class_Type: TPSCDefaultSectionControl; Kind: WORD(dskNone)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskBkgndColors))
    );

  WordFontStyleCount = 3;
  WordFontStyle: Array[0..WordFontStyleCount - 1] Of TPSCStyleRec =
  (
    (Class_Type: TPSCDefaultSectionControl; Kind: WORD(dskAuto)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskFontColors)),
    (Class_Type: TPSCButtonSectionControl; Kind: WORD(bskMoreColors))
    );

  FrontPageStyleCount = 5;
  FrontPageStyle: Array[0..FrontPageStyleCount - 1] Of TPSCStyleRec =
  (
    (Class_Type: TPSCDefaultSectionControl; Kind: WORD(dskAuto)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskStdColors)),
    (Class_Type: TPSCButtonSectionControl; Kind: WORD(bskMoreColors)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskCustomColors)),
    (Class_Type: TPSCButtonSectionControl; Kind: WORD(bskMoreColors))
    );

  FrontPageStyleBtnCount = 4;
  FrontPageStyleBtn: Array[0..FrontPageStyleBtnCount - 1] Of TPSCStyleRec =
  (
    (Class_Type: TPSCDefaultSectionControl; Kind: WORD(dskAuto)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskStdColors)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskCustomColors)),
    (Class_Type: TPSCButtonSectionControl; Kind: WORD(bskMoreColors))
    );

  SysColorsStyleCount = 2;
  SysColorsStyle: Array[0..SysColorsStyleCount - 1] Of TPSCStyleRec =
  (
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskStdColors)),
    (Class_Type: TPSCColorSectionControl; Kind: WORD(cskWinColors))
    );

  Styles: Array[TPSCStyle] Of
  Packed Record
    Count: Integer;
    StyleDef: PPSCStyleDef;
    Proc: TPSCStyleOptions
  End = (
    (Count: WordBkStyleCount; StyleDef: @WordBkStyle; Proc: Nil),
    (Count: WordFontStyleCount; StyleDef: @WordFontStyle; Proc: Nil),
    (Count: FrontPageStyleCount; StyleDef: @FrontPageStyle; Proc:
      FrontPageOptions)
    ,
    (Count: SysColorsStyleCount; StyleDef: @SysColorsStyle; Proc: Nil),
    (Count: FrontPageStyleBtnCount; StyleDef: @FrontPageStyleBtn; Proc: Nil)
    );

Function UniqueName(AOwner: TComponent; Const Name: String): String;
Var
  i: integer;

  Function IsUnique(AOwner: TComponent; Const AName: String): boolean;
  Begin
    Result := true;
    While AOwner <> Nil Do
      Begin
        Result := AOwner.FindComponent(AName) = Nil;
        If Not Result Then
          exit;
        AOwner := AOwner.Owner
      End
  End;

Begin
  i := 0;
  Repeat
    inc(i);
    result := name + PSCIntToStr(i);
  Until (AOwner = Nil) Or IsUnique(AOwner,result)
End;

Procedure TPSCCustomColorBox.SetStyle(Style: TPSCColorBoxStyle);
Const
  Coulnms: Array[Boolean] Of Integer = (5,8);
Var
  I: Integer;
  Section: TPSCCustomSection;
  Form: TCustomForm;
  OldColor: TPSCColor;
Begin
  If Style = cbsCustom Then
    Exit;
  DisableAlign;
  Try
    OldColor := FSelectedColor;
    While ControlCount > 0 Do
      Controls[ControlCount - 1].Free;
    With Styles[Style] Do
      Begin
        For I := 0 To Count - 1 Do
          With PPSCStyleDef(StyleDef)^[I] Do
            Begin
              With TSectionAccess(Class_Type.Create(Owner)) Do
                Begin
                  Section := FSection.GetInstance As TPSCCustomSection;
                  If Section Is TPSCColorSection Then
                    TPSCColorSection(Section).Kind := TPSCColorSlotsKind
                      (Kind)
                  Else
                    Section.Kind := Kind;
                  Parent := Self;
                  Form := GetParentForm(Self);
                  If (csDesigning In Self.ComponentState) And (Form <> Nil) Then
                    Name := UniqueName(Form,PSCTrimSeparatorsLeft(ClassName,['T']));
                  If (I < Count - 1) And (Class_Type = TPSCColorSectionControl)
                    Then
                    TSection(Section).ShowSeparator := true;
                End
            End;
        If Assigned(Proc) Then
          Proc(Self)
      End;
    ColumnCount := Coulnms[Style <> cbsWordBk];
    FSelectedColor := clPSCNone;
    SelectedColor := OldColor;
    FStyle:= Style;
  Finally
    EnableAlign
  End
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetStyle: TPSCColorBoxStyle;
Begin
  Result := FStyle;
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetOptions(Value: TPSCColorBoxOptions);
Var
  Options: TPSCColorBoxOptions;
  Option: TPSCColorBoxOption;
  Control: TPSCCustomSectionControl;
  Section: TPSCCustomSection;
  Form: TCustomForm;
Begin
  Options := Value;
  DisableAlign;
  Try
// SAG
    While ControlCount > 0 Do
      Controls[ControlCount - 1].Free;
// SAG
{ SAG For I := 0 To ControlCount - 1 Do
      Begin
        Section := Sections[I];
        For Option := Low(Option) To High(Option) Do
          With StyleMap[Option] Do
            If (Controls[I].ClassType = Class_Type) And
              (Section.DefaultKind = Kind) Then
              Begin
                Section.Visible := Option In Value;
                Options := Options - [Option]
              End
      End;}
    If Options <> [] Then
      For Option := Low(Option) To High(Option) Do
        If Option In Options Then
          Begin
            Control :=
              TPSCCustomSectionControl(StyleMap[Option].Class_Type.Create
              (Owner));
            Section := TSectionAccess(Control).FSection.GetInstance As
              TPSCCustomSection;
            With StyleMap[Option] Do
              Begin
                If Section Is TPSCColorSection Then
                  TPSCColorSection(Section).Kind := TPSCColorSlotsKind(Kind)
                Else
                  Section.Kind := Kind;
                Control.Parent := Self;
                Form := GetParentForm(Control);
                If (Form <> Nil) And (csDesigning In Form.ComponentState) Then
                  Control.Name := UniqueName(Form,PSCTrimSeparatorsLeft(Control.ClassName,['T']));
                If (Control <> Self.Controls[Self.ControlCount - 1]) And
                  (Control Is TPSCColorSectionControl) Then
                  TSection(Section).ShowSeparator := true
              End
          End
  Finally
    EnableAlign
  End;
  FStyle:= cbsCustom;
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetOptions: TPSCColorBoxOptions;
Var
  Option: TPSCColorBoxOption;
  I: Integer;
  Section: TPSCCustomSection;
Begin
  Result := [];
  For I := 0 To ControlCount - 1 Do
    Begin
      Section := Sections[I];
      If Section.Visible Then
        For Option := Low(Option) To High(Option) Do
          With StyleMap[Option] Do
            If (Controls[I].ClassType = Class_Type) And
              (Section.DefaultKind = WORD(Kind)) Then
              Result := Result + [Option]
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetColumnCount(Value: integer);
Begin
  If Value < 1 Then
    exit;
  If FColumnCount <> Value Then
    Begin
      FColumnCount := Value;
      If Not (csLoading In ComponentState) And Not (cbsSizing In FState) Then
        Width := FSlotWidth * Value + BorderWidth * 2
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetSlotWidth(Value: integer);
Begin
  If Value < 7 Then
    Value := 18;
  If FSlotWidth <> Value Then
    Begin
      FSlotWidth := Value;
      AdjustSize;
      Realign
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetSlotHeight(Value: integer);
Begin
  If Value < 7 Then
    Value := 18;
  If FSlotHeight <> Value Then
    Begin
      FSlotHeight := Value;
      AdjustSize;
      Realign
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetSelectSameColors(Value: Boolean);
Begin
  If FSelectSameColors <> Value Then
    Begin
      FSelectSameColors := Value;
      UpdateSelection
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetHideSelection(Value: TPSCHideSelection);
Begin
  If FHideSelection <> Value Then
    Begin
      FHideSelection := Value;
      UpdateSelection
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetSelectedColor(Value: TPSCColor);
Var
  I,J: Integer;
  Section: TPSCCustomSection;
Begin
  If Value <> FSelectedColor Then
    Begin
      FSelectedColor := Value;
      If csLoading In ComponentState Then
        exit;
      If Value = clPSCNone Then
        SelectedSlot := Nil
      Else
        Begin
          For I := 0 To ControlCount - 1 Do
            Begin
              Section := Sections[I];
              If Not (Section Is TPSCButtonSection) Then
                For J := 0 To Section.Count - 1 Do
                  If TPSCColorSlot(Section.Slots.Items[J]).Color = Value Then
                    Begin
                      SelectedSlot := TPSCColorSlot(Section.Slots.Items[J]);
                      exit
                    End
            End;
          SelectedSlot := Nil;
          FSelectedColor := clPSCNone;
        End;
    End;
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.CMControlListChange(Var
  Message: TCMControlListChange);
Var
  Section: TPSCCustomSection;
Begin
  With Message Do
    If Not Inserting Then
      Begin
        If Control Is TPSCCustomSectionControl Then
          Begin
            If TSectionAccess(Control).FSection <> Nil Then
              Begin
                With TSectionAccess(Control).FSection As IPSCDataReader Do
                  Section := TPSCCustomSection(GetInstance);
                If (ActiveSlot <> Nil) And (ActiveSlot.Section = Section) Then
                  ActiveSlot := Nil;
                If (SelectedSlot <> Nil) And (SelectedSlot.Section = Section)
                  Then
                  SelectedSlot := Nil;
                FState := FState + [cbsAdjustingNeeded];
                SetUpdating(true)
              End
          End
      End
    Else
      If Control Is TPSCCustomSectionControl Then
        Begin
          With TSectionAccess(Control) Do
            TSection(FSection.GetInstance).Parent := Self;
          FState := FState + [cbsAdjustingNeeded];
          SetUpdating(true)
        End
      Else
        PSCErrorFmt(PSCConsts.InvalidClass,[Control.ClassName]);
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.CMMouseEnter(Var Message: TMessage);
Begin
  Inherited
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.CMMouseLeave(Var Message: TMessage);
Begin
  Inherited;
  If FMouse <> Nil Then
    FMouse.MouseLeave(Self)
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.CMFontChanged(Var Message: TMessage);
Begin
  Inherited;
  AdjustSize;
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.CMColorChanged(Var Message: TMessage);
Begin
  Inherited;
  Perform(CM_BORDERCHANGED,0,0);
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.CMHintShow(Var Message: TCMHintShow);
Var
  Intf: IPSCObject;
  Ctrl: TControl;
Begin
  With Message,HintInfo^ Do
    Begin
      If ActiveSlot <> Nil Then
        Begin
          HintStr := PSCTrim(ActiveSlot.Hint);
          If Not ActiveSlot.Visible Or (HintStr = '') Or Not ActiveSlot.Enabled
            Then
            Result := 1
          Else
            Begin
              CursorRect := ActiveSlot.Bounds.GetBounds;
              With (ActiveSlot.Data.Parent As IPSCBounds).GetBounds Do
                OffsetRect(CursorRect,Left,Top)
            End
        End
      Else
        Begin
          If GetItemAt(CursorPos) <> Nil Then
            Result := 1
          Else
            Begin
              Ctrl := ControlAtPos(CursorPos,true);
              If (Ctrl <> Nil) And (Ctrl Is TPSCCustomSectionControl) Then
                Begin
                  Intf := TSectionAccess(Ctrl).FSection;
                  If (Intf As IPSCDataAccess).AsString[PRM_HINT] <> '' Then
                    Begin
                      HintStr := (Intf As IPSCDataAccess).AsString[PRM_HINT];
                      CursorRect := (Intf As IPSCBounds).GetBounds
                    End
                End
            End
        End;
      If Result = 0 Then
        Begin
          HintWindowClass := TPSCColorBoxHint;
          HintData := Pointer(Self);
        End
    End;
  Inherited
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.WMNCHitTest(Var Message: TWMNCHitTest);
Var
  Rect: TRect;
Begin
  Inherited;
  With Message Do
    If Result = htNowhere Then
      Begin
        Rect := ClientRect;
        InflateRect(Rect,BorderWidth,BorderWidth);
        If PtInRect(Rect,ScreenToClient(SmallPointToPoint(Pos))) Then
          Result := HTCLIENT
      End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.WMGetDlgCode(Var Message: TMessage);
Begin
  Inherited;
  With Message Do
    Result := Result Or DLGC_WANTARROWS
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.WMEnable(Var Message: TWMEnable);
Begin
  Inherited;
  Invalidate
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.WMPaint(Var Message: TWMPaint);
Begin
  ControlState := ControlState + [csCustomPaint];
  Inherited;
  ControlState := ControlState - [csCustomPaint];
  SetUpdating(false);
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.PaintWindow(DC: HDC);
Begin
  If (csDesigning In ComponentState) And (ControlCount = 0) Then
    Begin
      With TPSCCanvas.Create Do
      Try
        Handle := DC;
        Brush.Style := BrushStyle_Clear;
        Pen.Style := PenStyle_Dot;
        Pen.Color := clPSCBlack;
        With ClientRect Do
          Rectangle(Left,Top,Right,Bottom)
      Finally
        Free
      End
    End;
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.WMWindowPosChanging(Var
  Message: TWMWindowPosChanging);
Begin
  Inherited;
  With Message.WindowPos^ Do
    If (ComponentState * [csReading,csDestroying] = []) And
      (flags And SWP_NOSIZE = 0) Then
      SetUpdating(true)
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetInstance: TPersistent;
Begin
  Result := Self
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.Draw(Canvas: TPSCCanvas; Const BoundsRect: TRect);
Var
  Rect: TRect;
Begin
  Rect := Self.BoundsRect;
  Try
    UpdateBoundsRect(BoundsRect);
    Perform(WM_PAINT,Canvas.Handle,0);
  Finally
    UpdateBoundsRect(Rect)
  End
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetIntfAlign: TAlign;
Begin
  Result := Align
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetIntfVisible: Boolean;
Begin
  Result := Visible
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetIntfBounds: TRect;
Begin
  Result := BoundsRect;
  With Result Do
    Begin
      Left := Left + BorderWidth;
      Top := Top + BorderWidth;
      Right := Right - BorderWidth;
      Bottom := Bottom - BorderWidth
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetIntfBounds(Const Value: TRect);
Begin
  SetBounds(
    Value.Left - BorderWidth,Value.Top - BorderWidth,
    Value.Right - Value.Left + BorderWidth * 2,
    Value.Bottom - Value.Top + BorderWidth * 2
    )
End;

{-------------------------------------}

Function TPSCCustomColorBox.Measure(Var NewWidth,NewHeight: Integer): Boolean;
Var
  W,H,W2,H2: Integer;

  Function TestAutoSize(Var NewWidth,NewHeight: Integer): Boolean;
  Var
    W,H: Integer;
  Begin
    If Align <> alClient Then
      Begin
        W := NewWidth;
        H := NewHeight;
        Result := CanAutoSize(W,H);
        If Align In [alNone,alLeft,alRight] Then
          NewWidth := W;
        If Align In [alNone,alTop,alBottom] Then
          NewHeight := H
      End
    Else
      Result := true
  End;

Begin
  W := NewWidth;
  H := NewHeight;
  Result := False;
  If CanResize(W,H) Then
    Begin
      W2 := W;
      H2 := H;
      Result := Not AutoSize Or
        (TestAutoSize(W2,H2) And (W2 = W) And (H2 = H)) Or
        CanResize(W2,H2);
      If Result Then
        Begin
          NewWidth := W2;
          NewHeight := H2
        End
    End;
  Result := Result Or (csDesigning In ComponentState)
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetItem(Index: Integer): IPSCInterface;
Begin
  Result := Sections[Index]
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetCount: Integer;
Begin
  Result := ControlCount
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetItemAsBounds(Index: Integer): IPSCBounds;
Begin
  Result := Sections[Index]
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetItemAt(P: TPoint): IPSCInterface;
Var
  Control: TControl;
Begin
  Result := Nil;
  Control := ControlAtPos(P,true);
  If Control <> Nil Then
    Begin
      P := Control.ScreenToClient(ClientToScreen(P));
      With TSectionAccess(Control).FSection As IPSCBoundsList Do
        Result := GetItemAt(P)
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetFocused(Value: Boolean);
Var
  P: TPoint;
  Slot: IPSCDataAccess;
Begin
  If Value <> GetFocused Then
    Begin
      If HideSelection <> hsNotHide Then
        UpdateSelection;
      If Value And CanFocus And Visible Then
        Begin
          GetCursorPos(P);
          P := ScreenToClient(P);
          Slot := GetItemAt(P) As IPSCDataAccess;
          If (Slot <> Nil) And Slot.AsBoolean[PRM_ENABLED] And
            Slot.AsBoolean[PRM_VISIBLE] Then
            ActiveSlot := TPSCColorSlot(Slot.AsObject[PRM_OWNER])
          Else
            If SelectedSlot <> Nil Then
              ActiveSlot := SelectedSlot
            Else
              Begin
                Slot := GetNextItem(Nil,sdRight);
                If Slot <> Nil Then
                  ActiveSlot := TPSCColorSlot(Slot.AsObject[PRM_OWNER])
              End
        End
      Else
        ActiveSlot := Nil
    End
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetFocused: Boolean;
Begin
  Result := (ActiveSlot <> Nil)
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.Select;
Begin
  SelectedSlot := ActiveSlot
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.MoveSelection(Dir: TPSCDirection);
Var
  Slot: IPSCDataAccess;
Begin
  If (ActiveSlot = Nil) And (SelectedSlot <> Nil) Then
    ActiveSlot := SelectedSlot
  Else
    Begin
      If ActiveSlot <> Nil Then
        Slot := ActiveSlot.Data
      Else
        Slot := Nil;
      Slot := GetNextItem(Slot,Dir);
      If Slot <> Nil Then
        ActiveSlot := TPSCColorSlot(Slot.AsObject[PRM_OWNER])
    End
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetColorsCount: Integer;
Begin
  Result := ControlCount
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetColorItem(Index: Integer): IPSCColorsAccess;
Begin
  Result := (Sections[Index] As IPSCColorsAccess)
End;

{-------------------------------------}

Function TPSCCustomColorBox.Get_Boolean(IDParam: integer): Boolean;
Begin
  Case IDParam Of
    PRM_VISIBLE:
      Result := Visible;
    PRM_ENABLED:
      Result := Enabled;
    PRM_DESIGNTIME:
      Result := csDesigning In ComponentState;
    PRM_DOWNACTIVESLOT:
      Result := (ActiveSlot <> Nil) And (ButtonState_Down In ActiveSlot.State);
    PRM_UPACTIVESLOT:
      Result := (ActiveSlot <> Nil) And (ButtonState_Up In ActiveSlot.State);
    PRM_CANREARANGE:
      Result := CanRearrangeSlots;
    PRM_AUTOSIZE:
      Result := AutoSize;
    PRM_SELECTSAMECOLORS:
      Result := SelectSameColors;
  Else
    Result := false
  End
End;

{-------------------------------------}

Function TPSCCustomColorBox.Get_Cardinal(IDParam: integer): Cardinal;
Begin
  Case IDParam Of
    PRM_COLOR:
      Result := Cardinal(SelectedColor);
    PRM_BCKCOLOR:
      Result := Cardinal(Color);
    PRM_SLOTWIDTH:
      Result := SlotWidth;
    PRM_SLOTHEIGHT:
      Result := SlotHeight;
    PRM_COLUMNCOUNT:
      Result := ColumnCount;
    PRM_HIDESELECTION:
      Result := Cardinal(HideSelection);
    PRM_HINTKIND:
      Result := Cardinal(HintKind);
    PRM_PRESSEDCOLOR:
      Result := Cardinal(FPressedColor);
    PRM_HIGHLIGHT:
      If HighlightActive Then
        Result := Cardinal(HighlightColor)
      Else
        Result := Cardinal(clPSCNone)
    Else
      Result := 0
  End
End;

{-------------------------------------}

Function TPSCCustomColorBox.Get_Integer(IDParam: integer): longint;
Begin
  Result := -1
End;

{-------------------------------------}

Function TPSCCustomColorBox.Get_String(IDParam: integer): String;
Begin
  Case IDParam Of
    PRM_HINT: Result := Hint;
    PRM_OWNER: Result := FStatus;
  Else
    Result := ''
  End
End;

{-------------------------------------}

Function TPSCCustomColorBox.Get_Object(IDParam: integer): TObject;
Begin
  Case IDParam Of
    PRM_ACTIVESLOT:
      Result := ActiveSlot;
    PRM_SELECTEDSLOT:
      Result := SelectedSlot;
    PRM_FONT:
      Result := Font;
  Else
    Result := Nil
  End
End;

{-------------------------------------}

Function TPSCCustomColorBox.Get_Double(IDParam: integer): Double;
Begin
  Result := 0
End;

{-------------------------------------}

Function TPSCCustomColorBox.GetParent: IPSCDataReader;
Begin
  Result := FIntfParent
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.Event(Sender: IPSCObject; EvType:integer;LowData:TObject;HighData: integer);
Var
  Slot: TPSCColorSlot;

  Procedure SendMessageToParent;
  Begin
    If GetParent <> Nil Then
      GetParent.Event(Sender,EvType,LowData,Integer(Self));
    Set_String(PRM_OWNER, '');
  End;

Begin
  If (LowData = nil) Or (EvType And EV_OWNERCHANGED <> 0) Then
    DoChanged(Nil)
  Else
    Begin
      With TPSCDrawSlot(LowData) As IPSCDataAccess Do
        Slot := TPSCColorSlot(AsObject[PRM_OWNER]);
      If EvType And EV_ITEMDELETED <> 0 Then
        Begin
          If SelectedSlot = Slot Then
            SelectedSlot := Nil;
          If ActiveSlot = Slot Then
            ActiveSlot := Nil;
          If Assigned(FOnSlotDeletion) Then
            FOnSlotDeletion(Self,Slot);
          exit
        End;
      If (EvType And EV_COLORCHANGED) <> 0 Then
        Begin
          If FSelectSameColors And (FSelectedSlot <> Nil) And (Slot <> Nil) Then
            Begin
              If Slot = FSelectedSlot Then
                UpdateSelection
              Else
                If Not (Sender.GetInstance Is TPSCButtonSection) Then
                  If (Slot.Color <> clPSCNone) And (Slot.Color = SelectedSlot.Color)
                    Then
                    Slot.State := Slot.State + [ButtonState_Exclusive]
                  Else
                    Slot.State := Slot.State - [ButtonState_Exclusive]
            End
        End
      Else
        DoChanged(Slot)
    End;
  SendMessageToParent
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.Set_Boolean(IDParam: integer; Value: Boolean);
Var
  i: integer;
Begin
  Case IDParam Of
    PRM_VISIBLE:
      Begin
        Visible := Value;
        BeginUpdate;
        Try
          For i := 0 To ControlCount - 1 Do
            Sections[i].Visible := Value
        Finally
          EndUpdate
        End
      End;
    PRM_ENABLED:
      Begin
        Enabled := Value;
        BeginUpdate;
        Try
          For i := 0 To ControlCount - 1 Do
            Sections[i].Enabled := Value
        Finally
          EndUpdate
        End
      End;
    PRM_UPACTIVESLOT:
      If ActiveSlot <> Nil Then
        ActiveSlot.State := ActiveSlot.State - [ButtonState_Down] + [ButtonState_Up];
    PRM_DOWNACTIVESLOT:
      If ActiveSlot <> Nil Then
        ActiveSlot.State := ActiveSlot.State + [ButtonState_Down] - [ButtonState_Up];
    PRM_CANREARANGE:
      CanRearrangeSlots := Value;
    PRM_AUTOSIZE:
      AutoSize := Value;
    PRM_SELECTSAMECOLORS:
      SelectSameColors := Value;
  End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.Set_Cardinal(IDParam: integer; Value: Cardinal);
Begin
  Case IDParam Of
    PRM_COLOR:
      SelectedColor := TPSCColor(Value);
    PRM_BCKCOLOR:
      Color := TPSCColor(Value);
    PRM_SLOTWIDTH:
      SlotWidth := Value;
    PRM_SLOTHEIGHT:
      SlotHeight := Value;
    PRM_COLUMNCOUNT:
      ColumnCount := Value;
    PRM_HIDESELECTION:
      HideSelection := TPSCHideSelection(Value);
    PRM_HINTKIND:
      HintKind := TPSCColorStrKind(Value);
  End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.Set_Integer(IDParam: integer; Value: longint);
Begin
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.Set_String(IDParam: integer; Const Value: String);
Begin
  Case IDParam Of
    PRM_OWNER: FStatus := #65#38#86;
    PRM_HINT: Hint := Value;
  End                                
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.Set_Object(IDParam: integer; Value: TObject);
Begin
  Case IDParam Of
    PRM_ACTIVESLOT:
      ActiveSlot := TPSCColorSlot(Value);
    PRM_SELECTEDSLOT:
      SelectedSlot := TPSCColorSlot(Value);
  End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.Set_Double(IDParam: integer; Value: Double);
Begin
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.Loaded;
Var
  Color: TPSCColor;
Begin
  Inherited Loaded;
  DoChanged(Nil);
  Color := FSelectedColor;
  FSelectedColor := clPSCNone;
  SetSelectedColor(Color);
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.CreateWnd;
Begin
  Inherited CreateWnd;
  If Not AutoSize Then
    AdjustSize
End;

{-------------------------------------}

Type
  TControlAccess = Class(TControl);
  TWndProc = Procedure(Var Message: TMessage) Of Object;

Procedure TPSCCustomColorBox.WndProc(Var Message: TMessage);
Var
  Method: TPSCMethod;
Begin
  If Not (csDesigning In ComponentState) Then
    Case Message.Msg Of
      WM_MOUSEFIRST..WM_MOUSELAST:
        Begin
          Method.Code := @TControlAccess.WndProc;
          Method.Data := Self;
          TWndProc(Method)(Message);
          Exit
        End
    End;
  Inherited WndProc(Message)
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.CMHintShowPause(Var Message: TCMHintShowPause);
Begin
  Inherited;
  If cbsFastHint In FState Then
    Message.Pause^ := 0;
End;

{-------------------------------------}

Function TPSCCustomColorBox.CanResize(Var NewWidth,NewHeight: Integer): Boolean;
Var
  I,H,W,Ow,Oh: Integer;
  Size: TSize;
Begin
  FState := FState + [cbsSizing];
  Try
    Ow := NewWidth;
    Oh := NewHeight;
    NewWidth := NewWidth - BorderWidth * 2;
    H := 0;
    W := 0;
    If CanRearrangeSlots Then
      ColumnCount := NewWidth Div SlotWidth;
    For I := 0 To ControlCount - 1 Do
      With Sections[I] Do
        If Visible Or (csDesigning In ComponentState) Then
          Begin
            Size.cx := 0;
            Size.cy := 0;
            (Self.Sections[I] As IPSCBounds).Measure(Size.cx,Size.cy);
            If W < Size.cx Then
              W := Size.cx;
            H := H + Size.cy
          End;
    If (W = 0) And (csDesigning In ComponentState) Then
      W := 150;
    If NewWidth < W Then
      NewWidth := W;
    NewWidth := NewWidth + BorderWidth * 2;
    If (H = 0) And (csDesigning In ComponentState) Then
      H := 50;
    If (NewHeight < H) Or AutoSize Then
      NewHeight := H;
    NewHeight := NewHeight + BorderWidth * 2;
    If Align In [alTop,alBottom,alClient] Then
      NewWidth := Ow;
    If Align In [alLeft,alRight,alClient] Then
      NewHeight := Oh;
    Result := true
  Finally
    FState := FState - [cbsSizing]
  End
End;

{-------------------------------------}

Function TPSCCustomColorBox.CanAutoSize(Var NewWidth,NewHeight: Integer)
  : Boolean;
Var
  W: Integer;
Begin
  If ControlCount > 0 Then
    Begin
      If CanRearrangeSlots Then
        Begin
          W := (NewWidth - BorderWidth * 2) Mod SlotWidth;
          If W * 2 >= SlotWidth Then
            W := W - SlotWidth;
          NewWidth := NewWidth - W
        End
      Else
        NewWidth := ColumnCount * SlotWidth;
    End;
  Result := true
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.AdjustSize;
Begin
  Inherited AdjustSize;
  FState := FState - [cbsAdjustingNeeded];
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetCanRearrange(Value: Boolean);
Begin
  If FCanRearrange <> Value Then
    Begin
      FCanRearrange := Value;
      AdjustSize
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.AlignControls(AControl: TControl; Var Rect: TRect);
Var
  I: Integer;
Begin
  If (AControl <> Nil) And AControl.Visible Then
    Begin
      For I := 0 To ControlCount - 1 Do
        With Controls[I] Do
          If (Controls[I] <> AControl) And
            PtInRect(BoundsRect,PSCPoint(AControl.Left,AControl.Top)) Then
            Begin
              SetChildOrder(AControl,I);
              Break;
            End
          Else
            If (Controls[I] = AControl) And (Height = 0) Then
              Break;
      FState := FState + [cbsAdjustingNeeded];
      SetUpdating(true)
    End;
  If cbsAdjustingNeeded In FState Then
    AdjustSize;
  If FAligner <> Nil Then
    FAligner.Realign(Self,Self)
  Else
    Inherited AlignControls(Nil,Rect);
  Invalidate
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.DoChanged(Slot: TPSCColorSlot);
Begin
  If (ComponentState * [csLoading,csDestroying] = []) And (FUpdateCount = 0)
    Then
    Begin
      If Slot = Nil Then
        Begin
          If csAlignmentNeeded In ControlState Then
            Begin
              FState := FState + [cbsAdjustingNeeded];
              Exit
            End;
          If FSelectSameColors Then
            UpdateSelection;
          SetUpdating(true);
          AdjustSize;
          Invalidate;
        End;
      If Assigned(FOnChange) Then
        FOnChange(Self,Slot)
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.DoButtonClick(Slot: TPSCColorSlot;
  Action: TPSCButtonsActions);
Var
  Color: TPSCColor;
  I,J: Integer;
  CustomSlot,ASlot: TPSCColorSlot;
  Section: TPSCCustomSection;
  SelectCustomColor: Boolean;
Begin
  If (ComponentState * [csLoading,csDestroying] = []) And (Slot <> Nil) Then
    Begin
      Color := Slot.Color;
      If Assigned(FOnButtonClick) Then
        FOnButtonClick(Self,Slot,Color,bcsBeforeExecute);
      If (baDialog In Action) And (FColorDialog <> Nil) And
        Not FColorDialog.Execute(Color) Then
        begin
          FDialogWasExecuted:= False;
          exit;
        end;
      FDialogWasExecuted:= True;
      If Color <> clPSCNone Then
        Begin
          If baSetButtonColor In Action Then
            Slot.Color := Color;
          If (baSetSelectedColor In Action) And (SelectedSlot <> Nil) Then
            SelectedSlot.Color := Color;
          SelectCustomColor := true;
          If baSetCustomColor In Action Then
            Begin
              SelectedColor := Color;
              If SelectedColor = Color Then
                SelectCustomColor := false
            End;
          If SelectCustomColor And (baSetCustomColor In Action) Then
            Begin
              CustomSlot := Nil;
              If SelectedSlot <> Nil Then
                Begin
                  Section := SelectedSlot.Section As TPSCCustomSection;
                  If (Section Is TPSCColorSection) And
                    (Section.DefaultKind = WORD(cskCustomColors)) Then
                    CustomSlot := SelectedSlot
                End;
              For I := 0 To ControlCount - 1 Do
                Begin
                  Section := Sections[I];
                  If (Section Is TPSCColorSection) And
                    (Section.DefaultKind = WORD(cskCustomColors)) Then
                    For J := 0 To Section.Count - 1 Do
                      Begin
                        ASlot := TPSCColorSlot(Section.Slots.Items[J]);
                        If ASlot.Color = Color Then
                          Begin
                            SelectedSlot := ASlot;
                            Exit
                          End
                        Else
                          If (CustomSlot = Nil) Or (CustomSlot <> Nil) And
                            (CustomSlot.Color <> clPSCWhite) And (ASlot.Color =
                            clPSCWhite) Then
                            CustomSlot := ASlot
                      End
                End;
              If CustomSlot <> Nil Then
                Begin
                  CustomSlot.Color := Color;
                  CustomSlot.Section.Visible := true;
                  SelectedSlot := CustomSlot
                End
            End
        End;
      If Assigned(FOnButtonClick) Then
        FOnButtonClick(Self,Slot,Color,bcsAfterExecute)
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetUpdating(Value: Boolean);
Begin
  If Value Then
    Begin
      If Not FDoubleBuffered Then
        Begin
          FDoubleBuffered := true;
          FState := FState + [cbsUpdatingNeeded]
        End
    End
  Else
    If cbsUpdatingNeeded In FState Then
      Begin
        FDoubleBuffered := false;
        FState := FState - [cbsUpdatingNeeded]
      End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.UpdateSelection;
Var
  I,J: Integer;
  SelectedColor: TPSCColor;
  Section: TPSCCustomSection;
  Slot: TPSCColorSlot;
Begin
  If SelectedSlot <> Nil Then
    SelectedColor := SelectedSlot.Color
  Else
    SelectedColor := clPSCNone;
  For I := 0 To ControlCount - 1 Do
    Begin
      Section := Sections[I];
      If Not (Section Is TPSCButtonSection) Then
        For J := 0 To Section.Count - 1 Do
          Begin
            Slot := TPSCColorSlot(Section.Slots.Items[J]);
            If Slot <> SelectedSlot Then
              Begin
                If FSelectSameColors And (SelectedColor <> clPSCNone) And
                  (Slot.Color = SelectedColor) Then
                  Slot.State := Slot.State + [ButtonState_Exclusive]
                Else
                  Slot.State := Slot.State - [ButtonState_Exclusive]
              End
          End
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.DoEnter;
Begin
  SetFocused(true);
  Inherited DoEnter
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.DoExit;
Begin
  SetFocused(false);
  Inherited DoExit
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.KeyDown(Var Key: Word; Shift: TShiftState);
Begin
  Inherited KeyDown(Key,Shift);
  If FKeyboard <> Nil Then
    FKeyboard.KeyDown(Self,Key,Shift)
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.KeyUp(Var Key: Word; Shift: TShiftState);
Begin
  Inherited KeyUp(Key,Shift);
  If FKeyboard <> Nil Then
    FKeyboard.KeyUp(Self,Key,Shift)
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.KeyPress(Var Key: Char);
Begin
  Inherited KeyPress(Key);
  If FKeyboard <> Nil Then
    FKeyboard.KeyPress(Self,Key)
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Begin
  If FMouse <> Nil Then
    FMouse.MouseDown(Self,Button,Shift,X,Y);
  Inherited MouseDown(Button,Shift,X,Y)
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.MouseMove(Shift: TShiftState; X,Y: Integer);
Begin
  If cbsSkipMouseMove In FState Then
    FState := FState - [cbsSkipMouseMove]
  Else
    If FMouse <> Nil Then
      FMouse.MouseMove(Self,Shift,X,Y);
  Inherited MouseMove(Shift,X,Y)
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Begin
  If FMouse <> Nil Then
    FMouse.MouseUp(Self,Button,Shift,X,Y);
  Inherited MouseUp(Button,Shift,X,Y)
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.BeginUpdate;
Begin
  inc(FUpdateCount)
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.EndUpdate;
Begin
  dec(FUpdateCount);
  If FUpdateCount = 0 Then
    DoChanged(Nil)
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetReadOnly(Value: Boolean);
Begin
  If FReadOnly <> Value Then
    Begin
      If Value Then
        ActiveSlot := Nil;
      FReadOnly := Value;
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetPressedColor(Value: TPSCColor);
Begin
  If FPressedColor <> Value Then
    Begin
      FPressedColor := Value;
      Invalidate
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetHighlightActive(Value: Boolean);
Begin
  If FHighlightActive <> Value Then
    Begin
      FHighlightActive := Value;
      If ActiveSlot <> Nil Then
        TSlotAccess(ActiveSlot).Changed(false)
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.SetHighlightColor(Value: TPSCColor);
Begin
  If FHighlightColor <> Value Then
    Begin
      FHighlightColor := Value;
      If (ActiveSlot <> Nil) And HighlightActive Then
        TSlotAccess(ActiveSlot).Changed(false)
    End
End;

{-------------------------------------}

Procedure TPSCCustomColorBox.CMCancelMode(Var Message: TWMCancelMode);
Begin
  Inherited;
  Perform(WM_CANCELMODE,0,0)
End;

{-------------------------------------}

const
  cPSCSysColors: Array[0..2] Of TPSCIdentMapEntry = (
    (Value: clPSCHotLight; Name: SPSCHotLight),
    (Value: clPSCGradientActiveCaption; Name: SPSCGradientActiveCaption),
    (Value: clPSCGradientInactiveCaption; Name: SPSCGradientInactiveCaption));

  cPSCColorsIdentMap: Array[0..39] Of TPSCIdentMapEntry = (
    (Value: clPSCBlack; Name: SPSCBlack),
    (Value: clPSCBrown; Name: SPSCBrown),
    (Value: clPSCOliveGreen; Name: SPSCOliveGreen),
    (Value: clPSCDarkGreen; Name: SPSCDarkGreen),
    (Value: clPSCDarkTeal; Name: SPSCDarkTeal),
    (Value: clPSCDarkBlue; Name: SPSCDarkBlue),
    (Value: clPSCIndigo; Name: SPSCIndigo),
    (Value: clPSCGray80; Name: SPSCGray80),
    (Value: clPSCDarkRed; Name: SPSCDarkRed),
    (Value: clPSCOrange; Name: SPSCOrange),
    (Value: clPSCDarkYellow; Name: SPSCDarkYellow),
    (Value: clPSCGreen; Name: SPSCGreen),
    (Value: clPSCTeal; Name: SPSCTeal),
    (Value: clPSCBlue; Name: SPSCBlue),
    (Value: clPSCBlueGray; Name: SPSCBlueGray),
    (Value: clPSCGray50; Name: SPSCGray50),
    (Value: clPSCRed; Name: SPSCRed),
    (Value: clPSCLightOrange; Name: SPSCLightOrange),
    (Value: clPSCLime; Name: SPSCLime),
    (Value: clPSCSeaGreen; Name: SPSCSeaGreen),
    (Value: clPSCAqua; Name: SPSCAqua),
    (Value: clPSCLightBlue; Name: SPSCLightBlue),
    (Value: clPSCViolet; Name: SPSCViolet),
    (Value: clPSCGray40; Name: SPSCGray40),
    (Value: clPSCPink; Name: SPSCPink),
    (Value: clPSCGold; Name: SPSCGold),
    (Value: clPSCYellow; Name: SPSCYellow),
    (Value: clPSCBrightGreen; Name: SPSCBrightGreen),
    (Value: clPSCTurquoise; Name: SPSCTurquoise),
    (Value: clPSCSkyBlue; Name: SPSCSkyBlue),
    (Value: clPSCPlum; Name: SPSCPlum),
    (Value: clPSCGray25; Name: SPSCGray25),
    (Value: clPSCRose; Name: SPSCRose),
    (Value: clPSCTan; Name: SPSCTan),
    (Value: clPSCLightYellow; Name: SPSCLightYellow),
    (Value: clPSCLightGreen; Name: SPSCLightGreen),
    (Value: clPSCLightTurquoise; Name: SPSCLightTurquoise),
    (Value: clPSCPaleBlue; Name: SPSCPaleBlue),
    (Value: clPSCLavender; Name: SPSCLavender),
    (Value: clPSCWhite; Name: SPSCWhite)
  );

Var
  FPSCColors: TPSCColors = Nil;

Function PSCColorsManager: TPSCColors;
Begin
  If FPSCColors = Nil Then
    Begin
      FPSCColors := TPSCColors.Create;
      With FPSCColors Do
        Begin
          RegisterIdentMap(cPSCSysColors);
          RegisterIdentMap(cPSCColorsIdentMap);
        End;
    End;
  Result := FPSCColors;
End;

{-------------------------------------}

Constructor TPSCColors.Create;
Begin
  Inherited Create;
  FList := PSCCreateObjectList(ioOwned);
End;

{-------------------------------------}

Procedure TPSCColors.RegisterIdentMap(Const Map: Array Of TIdentMapEntry);
Var
  ColorsEntry: TPSCColorsEntry;
  i:Integer;
Begin
  With Flist Do
  begin
    ColorsEntry:= TPSCColorsEntry.Create;
    SetLength(ColorsEntry.MapArray,Length(Map));
    for i:=Low(Map) to High(Map) do
      ColorsEntry.MapArray[i]:=Map[i];
    FList.Add(ColorsEntry);
  end;
End;

{-------------------------------------}

Procedure TPSCColors.GetColors(Proc: TPSCGetStrProc);
Var
  I,J: Integer;
Begin
  With Flist Do
    For I := 0 To Count - 1 Do
      With TPSCColorsEntry(Items[I]) Do
        For J := Low(MapArray) To High(MapArray) Do
          Proc(MapArray[J].Name);
End;

{-------------------------------------}

Function TPSCColors.ColorToString(Color: TPSCColor;
  Kind: TPSCColorStrKind): String;
Var
  I: Integer;
Begin
  Case Color Of
    clPSCNone: Result := '';
    clPSCDefault: Result := PSCConsts.Transparent
  Else
    Begin
      With Flist Do
      Try
        For I := 0 To Count - 1 Do
          With TPSCColorsEntry(Items[I]) Do
            If IntToIdent(Color,Result,MapArray) Then
              Exit
      Finally
      End;
      Result := ColorToRGBStr(Color,Kind);
    End
  End;
End;

{-------------------------------------}

Function TPSCColors.StringToColor(Const S: String): TPSCColor;
Var
  I: Integer;
Begin
  If S = '' Then
    Result := clPSCNone
  Else
    If S = PSCConsts.Transparent Then
      Result := clPSCDefault
    Else
      Begin
        If (Length(S) > 7) And ((Pos('RGB',PSCUpperCase(S)) > 0)// don't resource
          Or
          (Pos('HEX',PSCUpperCase(S)) > 0)) Then// don't resource
          Begin
            Result := RGBStrToColor(S);
            Exit
          End
        Else
          With Flist Do
          Try
            For I := 0 To Count - 1 Do
              With TPSCColorsEntry(Items[I]) Do
                If IdentToInt(S,Integer(Result),MapArray) Then
                  Exit
          Finally
          End;
        Result := TPSCColor(PSCStrToInt(S))
      End
End;

{-------------------------------------}

Function TPSCColors.ColorToRGBStr(Color: TPSCColor;
  Kind: TPSCColorStrKind): String;

  Function StrValue(Value: Double): String;
  Begin
    Result := PSCFormat('%f', [Value / 2.55]); // don't resource
    If Kind = ckRGBIntPercents Then
      Delete(Result,Length(Result) - 2,3)
    Else
      Begin
        Result[Length(Result) - 2] := '.';
        While Result[Length(Result)] = '0' Do
          SetLength(Result,Length(Result) - 1);
        If Result[Length(Result)] = '.' Then
          SetLength(Result,Length(Result) - 1);
      End;
    If Result <> '0' Then
      Result := Result + '%'
  End;

Begin
  Color := PSCColorToRGB(Color);
  With TRGBQuad(Color) Do
    Case Kind Of
      ckHex: Result := PSCIntToHex(Color,6);
      ckRGBPercents,ckRGBIntPercents:
        Result := PSCFormat('RGB{%s,%s,%s}', //don't resource
          [StrValue(rgbBlue),StrValue(rgbGreen),StrValue(rgbRed)]);
      ckRGBDecimal:
        Result := PSCFormat('RGB{%d,%d,%d}', //don't resource
          [rgbBlue,rgbGreen,rgbRed]);
      ckRGBHex:
        Result := PSCFormat('RGB{%2x,%2x,%2x}', //don't resource
          [rgbBlue,rgbGreen,rgbRed]);
      ckHexTriple:
        Result := PSCFormat('Hex={%2x,%2x,%2x}', //don't resource
          [rgbBlue,rgbGreen,rgbRed])
    Else
      Result := PSCIntToStr(Color);
    End
End;

{-------------------------------------}

Function TPSCColors.RGBStrToColor(Const S: String): TPSCColor;
Var
  C: String;
  P: Integer;

  Procedure Error;
  Begin
    PSCErrorFmt(PSCConsts.InvalidColor, [S]);
  End;

  Function StrToColorValue(Const S: String): Integer;
  Begin
    If S[Length(S)] = '%' Then
      Result := Round(PSCStrToFloat(Copy(S,1,Length(S) - 1)) * 2.55)
    Else
      Begin
        Result := PSCStrToIntDef(S, -1);
        If Result = -1 Then
          Result := PSCStrToInt('$' + PSCTrim(S))
      End;
    If Result < 0 Then
      Result := 0
    Else
      If Result > 255 Then
        Result := 255
  End;

Begin
  C := PSCUpperCase(PSCTrim(S));
  With TRGBQuad(Result) Do
    Begin
      Delete(C,1,3);
      P := Pos('{',C);
      If P = 0 Then
        Error;
      Delete(C,1,P);
      P := Pos(',',C);
      If P = 0 Then
        Error;
      rgbBlue := StrToColorValue(Copy(C,1,P - 1));
      Delete(C,1,P);
      P := Pos(',',C);
      If P = 0 Then
        Error;
      rgbGreen := StrToColorValue(Copy(C,1,P - 1));
      Delete(C,1,P);
      P := Pos('}',C);
      If P = 0 Then
        Error;
      rgbRed := StrToColorValue(Copy(C,1,P - 1));
      Delete(C,1,P);
      If C <> '' Then
        Error
    End
End;

{-------------------------------------}

Function PSCColorToBorderColor(AColor: TPSCColor; ASelected: Boolean): TPSCColor;
Type
  TColorQuad = Record
    Red,Green,Blue,Alpha: Byte;
  End;
Begin
  With TColorQuad(AColor) Do
    If (Red > 192) Or (Green > 192) Or (Blue > 192) Then
      Result := clPSCBlack
    Else
      If ASelected Then
        Result := clPSCWhite
      Else
        Result := AColor;
End;

{-------------------------------------}

function TPSCCustomColorBox.GetDialogWasExecuted: Boolean;
begin
  Result:= FDialogWasExecuted;
end;

Initialization
  PSCSafeRegisterClasses([TPSCColorBox,TPSCDefaultSectionControl,
    TPSCColorSectionControl,TPSCButtonSectionControl]);
Finalization
  UnRegisterClasses([TPSCColorBox,TPSCDefaultSectionControl,
    TPSCColorSectionControl,TPSCButtonSectionControl]);

  FPSCColors.Free;
  FPSCColors:=nil;
End.

