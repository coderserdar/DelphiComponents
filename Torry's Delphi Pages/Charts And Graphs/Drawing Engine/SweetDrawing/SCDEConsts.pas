{*******************************************************}
{                                                       }
{         CA SweetDrawing Component Library             }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDEConsts;

{$I SweetDrawing.inc}

{$P+,S-,W-,R-,T-,X+,H+}
{$C PRELOAD}

interface

uses
  Windows, SysUtils, Classes, Messages, Graphics;

type
  {$IFDEF SCDE_DELPHI4_AND_EARLY}
    TImageIndex = Integer;
  {$ENDIF}

  TSCDeAboutString = String;

  TSCDeImageLayout = (scdilBottom, scdilLeft, scdilRight, scdilTop);
  TSCDeEditLayout = (scdelBottom, scdelCenter, scdelTop);

  TSCDeLayout = (scdlaBottom, scdlaMiddle, scdlaTop);
  TSCDeRotation = (scdroLeft, scdroRight, scdroTop);

  TSCDeEllipsis = (scdelNone, scdelEndEllipsis, scdelPathEllipsis);

  TSCDeArrowKeys = set of 0..255;

  TSCDeBevelWidth = 0..MaxInt;

  TSCDeBorder = (scdcbNone, scdcb3DRaised, scdcb3DLowered, scdcbRaised, scdcbLowered,
    scdcbBumped, scdcbEtched, scdcbMacLowered, scdcbMacRaised, scdcbMetal,
    scdcbSoftLowered, scdcbSoftRaised, scdcbColor, scdcbFlat, scdcbFlatBold,
    scdcbFlatRounded, scdcbFlatBoldRounded);

  TSCDeControlBevel = TSCDeBorder;

  TSCDeControlBorder = scdcbNone..scdcbFlatBold;

  TSCDeBorderEdge = (scdbeLeft, scdbeRight, scdbeTop, scdbeBottom);

  TSCDeBorderEdges = set of TSCDeBorderEdge;

  TSCDePictureOrient = (scdpoTopLeft, scdpoTopRight, scdpoBottomLeft,
    scdpoBottomRight, scdpoCenter, scdpoTiled);

  TSCDeScrollButtonLayout = (scdsbDefault, scdsbLeftTop, scdsbRightBottom);

  TSCDeScrollbarKind = (scdskHorizontal, scdskVertical);

  TSCDeScrollbarHitKind = (scshkNone, scshkHorizontal, scshkVertical);
  TSCDeScrollerHitPart = (scspNone, scspLeftSpare, scspRightSpare, scspThumb,
    scspExtraButton, scspLeftButton, scspRightButton);

  TSCDeScrollbarInc = 1..32767;

  TSCDeScrollbarStyle = (scssDefault, scssDefaultEx, scssFlat, scssFlatEx,
    scssFlatHover, scssOffice12, scssOffice2k, scssMetal, scssXP, scssXP2,
    scss3D, scss3DX, scssNew, scssNewX, scssMac, scssSports);

  TSCDeScrollThumbline = (sctlNone, sctlLowered, sctlRised, sctlDots, sctlDash,
    sctlWideDash);

  TSCDeScrollEvent = procedure(Sender: TObject; Kind: TSCDeScrollbarKind;
    CurPos: Integer; var ScrollPos: Integer; var CanScroll: Boolean) of object;

  TSCDeShapePictureStyle = (scdspCenter, scdspDefault, scdspTiled, scdspStretch);

var
  scdNegativeSign: Char = '-';
  scdNegativeFormat: Byte = 1;
  scdNegativeCurFormat: Byte = 1;
  scdDefaultLCID: LCID;
  scdSystemLCID: LCID;
  scdSysDecimalSeparator: Char = '.';
  scdCurThousandSeparator: Char = ',';
  scdCurDecimalSeparator: Char = '.';

const
  SCDE_VersionStr = 'CA Sweet Drawing Engine v2.00';

  SCDE_BUTTON_REPEATTIMERID = 4321;
  SCDE_HTBASE = $0500;

  SCDE_HTNONE            = SCDE_HTBASE;
  SCDE_HTOUTTERBORDER    = SCDE_HTBASE +  1;
  SCDE_HTBORDER          = SCDE_HTBASE +  2;
  SCDE_HTINNERBORDER     = SCDE_HTBASE +  3;

  SCDE_HTSTATUSBAR       = SCDE_HTBASE +  4;
  SCDE_HTSIZEGRIP        = SCDE_HTBASE +  5;

  SCDE_HTHScrollBAR      = SCDE_HTBASE +  6;
  SCDE_HTHSB_EXTRABTN    = SCDE_HTBASE +  7;
  SCDE_HTHSB_LEFTBTN     = SCDE_HTBASE +  8;
  SCDE_HTHSB_RIGHTBTN    = SCDE_HTBASE +  9;
  SCDE_HTHSB_THUMB       = SCDE_HTBASE + 10;
  SCDE_HTHSB_LEFTSPARE   = SCDE_HTBASE + 11;
  SCDE_HTHSB_RIGHTSPARE  = SCDE_HTBASE + 12;

  SCDE_HTVScrollBAR      = SCDE_HTBASE + 13;
  SCDE_HTVSB_EXTRABTN    = SCDE_HTBASE + 14;
  SCDE_HTVSB_LEFTBTN     = SCDE_HTBASE + 15;
  SCDE_HTVSB_RIGHTBTN    = SCDE_HTBASE + 16;
  SCDE_HTVSB_THUMB       = SCDE_HTBASE + 17;
  SCDE_HTVSB_LEFTSPARE   = SCDE_HTBASE + 18;
  SCDE_HTVSB_RIGHTSPARE  = SCDE_HTBASE + 19;


  CM_SCDEMESSAGEBASE     = WM_APP + 1919;
  CM_SCDEDROPDOWNPOPUP   = CM_SCDEMESSAGEBASE + 1;
  CM_SCDEJUMPTONEXT      = CM_SCDEMESSAGEBASE + 2;
  CM_SCDEISVISIBLECHILD  = CM_SCDEMESSAGEBASE + 3;

  SCDE_AllBorderEdges = [scdbeLeft, scdbeRight, scdbeTop, scdbeBottom];

  SCDE_MenuUndo   = 100;
  SCDE_MenuRedo   = 101;
  SCDE_MenuCopy   = 102;
  SCDE_MenuCut    = 103;
  SCDE_MenuPaste  = 104;
  SCDE_MenuDelete = 105;
  SCDE_MenuSelectAll = 106;

  SCDE_HighlightColor = $00FCA070;
  SCDE_HottrackColor  = $0000ACFF;
  SCDE_AlertColor     = $00A8A8FF;
  SCDE_FlashColor     = $0063CFFE;

  { operating system (OS)constants }
  scdOsUnknown  = -1;
  scdOsBelow95  =  0;
  scdOsWin95    =  1;
  scdOsWin98    =  2;
  scdOsWin98SE  =  3;
  scdOsWinME    =  4;
  scdOsWinNT    =  5;
  scdOsWin2000  =  6;
  scdOsXP       =  7;
  scdOsOverXP   =  8;

var
  scdCurrentOperatingSystem: Integer = scdOsUnknown;

const
  scdPenModes: array[TPenMode] of Word =
    (R2_BLACK, R2_WHITE, R2_NOP, R2_NOT, R2_COPYPEN, R2_NOTCOPYPEN, R2_MERGEPENNOT,
     R2_MASKPENNOT, R2_MERGENOTPEN, R2_MASKNOTPEN, R2_MERGEPEN, R2_NOTMERGEPEN,
     R2_MASKPEN, R2_NOTMASKPEN, R2_XORPEN, R2_NOTXORPEN);

  scdArrowKeys: TSCDeArrowKeys = [VK_UP, VK_LEFT, VK_DOWN, VK_RIGHT,
    VK_PRIOR, VK_NEXT, VK_END, VK_HOME];

{$IFDEF SCDE_DELPHI6_AND_EARLY}
  {$EXTERNALSYM SCDE_COLOR_MENUHILIGHT}
  SCDE_COLOR_MENUHILIGHT = 29;
  {$EXTERNALSYM SCDE_COLOR_MENUBAR}
  SCDE_COLOR_MENUBAR = 30;

  clHotLight                = TColor(COLOR_HOTLIGHT or $80000000);

  clMenuHighlight           = TColor(SCDE_COLOR_MENUHILIGHT or $80000000);
  clMenuBar                 = TColor(SCDE_COLOR_MENUBAR or $80000000);

  {$IFDEF SCDE_DELPHI5_AND_EARLY}
  clGradientActiveCaption   = TColor(COLOR_GRADIENTACTIVECAPTION or $80000000);
  clGradientInactiveCaption = TColor(COLOR_GRADIENTINACTIVECAPTION or $80000000);
  {$ENDIF}
{$ENDIF}

  AlignStyle : array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT), (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));

  scdskNone = 'None';
  scdskBkSp = 'BkSp';
  scdskTab = 'Tab';
  scdskEsc = 'Esc';
  scdskEnter = 'Enter';
  scdskSpace = 'Space';
  scdskPgUp = 'PgUp';
  scdskPgDn = 'PgDn';
  scdskEnd = 'End';
  scdskHome = 'Home';
  scdskLeft = 'Left';
  scdskUp = 'Up';
  scdskRight = 'Right';
  scdskDown = 'Down';
  scdskIns = 'Ins';
  scdskDel = 'Del';
  scdskShift = 'Shift+';
  scdskCtrl = 'Ctrl+';
  scdskAlt = 'Alt+';

  {$EXTERNALSYM SCDE_LBUTTON}
  SCDE_LBUTTON = 1;
  {$EXTERNALSYM SCDE_RBUTTON}
  SCDE_RBUTTON = 2;
  {$EXTERNALSYM SCDE_SHIFT}
  SCDE_SHIFT = 4;
  {$EXTERNALSYM SCDE_CONTROL}
  SCDE_CONTROL = 8;
  {$EXTERNALSYM SCDE_MBUTTON}
  SCDE_MBUTTON = 16;
  {$EXTERNALSYM SCDE_MENU}
  SCDE_MENU = 32;

{$IFDEF SCDE_DELPHI5_AND_EARLY}
var
  clActiveCaptionGradient: TColor = Word(clActiveCaption);
  clInactiveCaptionGradient: TColor = Word(clInactiveCaption);
{$ENDIF}

implementation

{$IFDEF SCDE_DELPHI5_AND_EARLY}
uses
  SCDECommon;
{$ENDIF}

{$IFDEF SCDE_DELPHI5_AND_EARLY}
procedure SetInitializeValues;
var
  S: String;
begin
  scdDefaultLCID := GetThreadLocale;
  scdSystemLCID := GetSystemDefaultLCID;

  {$IFDEF MSWINDOWS}
  try
    scdNegativeFormat := 1;
    scdNegativeFormat := StrToIntDef(GetLocaleStr(scdDefaultLCID, LOCALE_INEGNUMBER, '0'), 1);
  except
    scdNegativeFormat := 1;
  end;

  try
    scdNegativeSign := '-';
    S := GetLocaleStr(scdDefaultLCID, LOCALE_SNEGATIVESIGN, '-');
    if Length(S) > 0 then scdNegativeSign := S[1];
  except
    scdNegativeSign := '-';
  end;

  try
    scdNegativeCurFormat := 1;
    scdNegativeCurFormat := StrToIntDef(GetLocaleStr(scdDefaultLCID, LOCALE_INEGCURR, '0'), 1);
  except
    scdNegativeCurFormat := 1;
  end;

  try
    scdSysDecimalSeparator := '.';
    S := GetLocaleChar(scdSystemLCID, LOCALE_SDECIMAL, '.');
    if Length(S) = 0 then
      scdSysDecimalSeparator := '.'
    else
      scdSysDecimalSeparator := S[1];
  except
    scdSysDecimalSeparator := '.';
  end;

  try
    scdCurThousandSeparator := ',';
    S := GetLocaleChar(scdDefaultLCID, LOCALE_SMONTHOUSANDSEP, ',');
    if Length(S) = 0 then
      scdCurThousandSeparator := ','
    else
      scdCurThousandSeparator := S[1];
  except
    scdCurThousandSeparator := ',';
  end;

  try
    scdCurDecimalSeparator := '.';
    S := GetLocaleChar(scdDefaultLCID, LOCALE_SMONDECIMALSEP, '.');
    if Length(S) = 0 then
      scdCurDecimalSeparator := '.'
    else
      scdCurDecimalSeparator := S[1];
  except
    scdCurDecimalSeparator := '.';
  end;
  {$ENDIF}
end;

initialization
  SetInitializeValues;
{$ENDIF}

end.
