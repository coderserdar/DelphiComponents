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
unit myla_system;

interface
{$I psc_defines.inc}

uses
  psc_const, system.SysUtils, system.AnsiStrings,

  myla_interfaces;

const
  PSC_COLOR_SCROLLBAR = 0;
  PSC_COLOR_BACKGROUND = 1;
  PSC_COLOR_ACTIVECAPTION = 2;
  PSC_COLOR_INACTIVECAPTION = 3;
  PSC_COLOR_MENU = 4;
  PSC_COLOR_WINDOW = 5;
  PSC_COLOR_WINDOWFRAME = 6;
  PSC_COLOR_MENUTEXT = 7;
  PSC_COLOR_WINDOWTEXT = 8;
  PSC_COLOR_CAPTIONTEXT = 9;
  PSC_COLOR_ACTIVEBORDER = 10;
  PSC_COLOR_INACTIVEBORDER = 11;
  PSC_COLOR_APPWORKSPACE = 12;
  PSC_COLOR_HIGHLIGHT = 13;
  PSC_COLOR_HIGHLIGHTTEXT = 14;
  PSC_COLOR_BTNFACE = 15;
  PSC_COLOR_BTNSHADOW = $10;
  PSC_COLOR_GRAYTEXT = 17;
  PSC_COLOR_BTNTEXT = 18;
  PSC_COLOR_INACTIVECAPTIONTEXT = 19;
  PSC_COLOR_BTNHIGHLIGHT = 20;
  PSC_COLOR_3DDKSHADOW = 21;
  PSC_COLOR_3DLIGHT = 22;
  PSC_COLOR_INFOTEXT = 23;
  PSC_COLOR_INFOBK = 24;
  PSC_COLOR_HOTLIGHT = 26;
  PSC_COLOR_GRADIENTACTIVECAPTION = 27;
  PSC_COLOR_GRADIENTINACTIVECAPTION = 28;
  PSC_COLOR_MENUHILIGHT = 29;
  PSC_COLOR_MENUBAR = 30;
  PSC_COLOR_DESKTOP = PSC_COLOR_BACKGROUND;
  PSC_COLOR_3DFACE = PSC_COLOR_BTNFACE;
  PSC_COLOR_3DSHADOW = PSC_COLOR_BTNSHADOW;
  PSC_COLOR_3DHIGHLIGHT = PSC_COLOR_BTNHIGHLIGHT;
  PSC_COLOR_3DHILIGHT = PSC_COLOR_BTNHIGHLIGHT;
  PSC_COLOR_BTNHILIGHT = PSC_COLOR_BTNHIGHLIGHT;

{$IFDEF D7}
  clPSCSystemColor = $FF000000;
{$ELSE}
  clPSCSystemColor = $80000000;
{$ENDIF}

  clPSCScrollBar = TPSCColor(clPSCSystemColor or PSC_COLOR_SCROLLBAR);
  clPSCBackground = TPSCColor(clPSCSystemColor or PSC_COLOR_BACKGROUND);
  clPSCActiveCaption = TPSCColor(clPSCSystemColor or PSC_COLOR_ACTIVECAPTION);
  clPSCInactiveCaption = TPSCColor(clPSCSystemColor or PSC_COLOR_INACTIVECAPTION);
  clPSCMenu = TPSCColor(clPSCSystemColor or PSC_COLOR_MENU);
  clPSCWindow = TPSCColor(clPSCSystemColor or PSC_COLOR_WINDOW);
  clPSCWindowFrame = TPSCColor(clPSCSystemColor or PSC_COLOR_WINDOWFRAME);
  clPSCMenuText = TPSCColor(clPSCSystemColor or PSC_COLOR_MENUTEXT);
  clPSCWindowText = TPSCColor(clPSCSystemColor or PSC_COLOR_WINDOWTEXT);
  clPSCCaptionText = TPSCColor(clPSCSystemColor or PSC_COLOR_CAPTIONTEXT);
  clPSCActiveBorder = TPSCColor(clPSCSystemColor or PSC_COLOR_ACTIVEBORDER);
  clPSCInactiveBorder = TPSCColor(clPSCSystemColor or PSC_COLOR_INACTIVEBORDER);
  clPSCAppWorkSpace = TPSCColor(clPSCSystemColor or PSC_COLOR_APPWORKSPACE);
  clPSCHighlight = TPSCColor(clPSCSystemColor or PSC_COLOR_HIGHLIGHT);
  clPSCHighlightText = TPSCColor(clPSCSystemColor or PSC_COLOR_HIGHLIGHTTEXT);
  clPSCBtnFace = TPSCColor(clPSCSystemColor or PSC_COLOR_BTNFACE);
  clPSCBtnShadow = TPSCColor(clPSCSystemColor or PSC_COLOR_BTNSHADOW);
  clPSCGrayText = TPSCColor(clPSCSystemColor or PSC_COLOR_GRAYTEXT);
  clPSCBtnText = TPSCColor(clPSCSystemColor or PSC_COLOR_BTNTEXT);
  clPSCInactiveCaptionText = TPSCColor(clPSCSystemColor or PSC_COLOR_INACTIVECAPTIONTEXT);
  clPSCBtnHighlight = TPSCColor(clPSCSystemColor or PSC_COLOR_BTNHIGHLIGHT);
  clPSC3DDkShadow = TPSCColor(clPSCSystemColor or PSC_COLOR_3DDKSHADOW);
  clPSC3DLight = TPSCColor(clPSCSystemColor or PSC_COLOR_3DLIGHT);
  clPSCInfoText = TPSCColor(clPSCSystemColor or PSC_COLOR_INFOTEXT);
  clPSCInfoBk = TPSCColor(clPSCSystemColor or PSC_COLOR_INFOBK);
  clPSCHotLight = TPSCColor(clPSCSystemColor or PSC_COLOR_HOTLIGHT);
  clPSCGradientActiveCaption = TPSCColor(clPSCSystemColor or PSC_COLOR_GRADIENTACTIVECAPTION);
  clPSCGradientInactiveCaption = TPSCColor(clPSCSystemColor or PSC_COLOR_GRADIENTINACTIVECAPTION);
  clPSCMenuHighlight = TPSCColor(clPSCSystemColor or PSC_COLOR_MENUHILIGHT);
  clPSCMenuBar = TPSCColor(clPSCSystemColor or PSC_COLOR_MENUBAR);

  clPSCBlack = TPSCColor($000000);
  clPSCMaroon = TPSCColor($000080);
  clPSCGreen = TPSCColor($008000);
  clPSCOlive = TPSCColor($008080);
  clPSCNavy = TPSCColor($800000);
  clPSCPurple = TPSCColor($800080);
  clPSCTeal = TPSCColor($808000);
  clPSCGray = TPSCColor($808080);
  clPSCSilver = TPSCColor($C0C0C0);
  clPSCRed = TPSCColor($0000FF);
  clPSCLime = TPSCColor($00FF00);
  clPSCYellow = TPSCColor($00FFFF);
  clPSCBlue = TPSCColor($FF0000);
  clPSCFuchsia = TPSCColor($FF00FF);
  clPSCAqua = TPSCColor($FFFF00);
  clPSCLtGray = TPSCColor($C0C0C0);
  clPSCDkGray = TPSCColor($808080);
  clPSCWhite = TPSCColor($FFFFFF);
  clPSCMoneyGreen = TPSCColor($C0DCC0);
  clPSCSkyBlue = TPSCColor($F0CAA6);
  clPSCCream = TPSCColor($F0FBFF);
  clPSCMedGray = TPSCColor($A4A0A0);
  clPSCNone = TPSCColor($1FFFFFFF);
  clPSCDefault = TPSCColor($20000000);

  PSC_VK_LBUTTON = 1;
  PSC_VK_RBUTTON = 2;
  PSC_VK_CANCEL = 3;
  PSC_VK_MBUTTON = 4;
  PSC_VK_BACK = 8;
  PSC_VK_TAB = 9;
  PSC_VK_CLEAR = 12;
  PSC_VK_RETURN = 13;
  PSC_VK_SHIFT = $10;
  PSC_VK_CONTROL = 17;
  PSC_VK_MENU = 18;
  PSC_VK_PAUSE = 19;
  PSC_VK_CAPITAL = 20;
  PSC_VK_KANA = 21;
  PSC_VK_HANGUL = 21;
  PSC_VK_JUNJA = 23;
  PSC_VK_FINAL = 24;
  PSC_VK_HANJA = 25;
  PSC_VK_KANJI = 25;
  PSC_VK_CONVERT = 28;
  PSC_VK_NONCONVERT = 29;
  PSC_VK_ACCEPT = 30;
  PSC_VK_MODECHANGE = 31;
  PSC_VK_ESCAPE = 27;
  PSC_VK_SPACE = $20;
  PSC_VK_PRIOR = 33;
  PSC_VK_NEXT = 34;
  PSC_VK_END = 35;
  PSC_VK_HOME = 36;
  PSC_VK_LEFT = 37;
  PSC_VK_UP = 38;
  PSC_VK_RIGHT = 39;
  PSC_VK_DOWN = 40;
  PSC_VK_SELECT = 41;
  PSC_VK_PRINT = 42;
  PSC_VK_EXECUTE = 43;
  PSC_VK_SNAPSHOT = 44;
  PSC_VK_INSERT = 45;
  PSC_VK_DELETE = 46;
  PSC_VK_HELP = 47;
  PSC_VK_LWIN = 91;
  PSC_VK_RWIN = 92;
  PSC_VK_APPS = 93;
  PSC_VK_NUMPAD0 = 96;
  PSC_VK_NUMPAD1 = 97;
  PSC_VK_NUMPAD2 = 98;
  PSC_VK_NUMPAD3 = 99;
  PSC_VK_NUMPAD4 = 100;
  PSC_VK_NUMPAD5 = 101;
  PSC_VK_NUMPAD6 = 102;
  PSC_VK_NUMPAD7 = 103;
  PSC_VK_NUMPAD8 = 104;
  PSC_VK_NUMPAD9 = 105;
  PSC_VK_MULTIPLY = 106;
  PSC_VK_ADD = 107;
  PSC_VK_SEPARATOR = 108;
  PSC_VK_SUBTRACT = 109;
  PSC_VK_DECIMAL = 110;
  PSC_VK_DIVIDE = 111;
  PSC_VK_F1 = 112;
  PSC_VK_F2 = 113;
  PSC_VK_F3 = 114;
  PSC_VK_F4 = 115;
  PSC_VK_F5 = 116;
  PSC_VK_F6 = 117;
  PSC_VK_F7 = 118;
  PSC_VK_F8 = 119;
  PSC_VK_F9 = 120;
  PSC_VK_F10 = 121;
  PSC_VK_F11 = 122;
  PSC_VK_F12 = 123;
  PSC_VK_F13 = 124;
  PSC_VK_F14 = 125;
  PSC_VK_F15 = 126;
  PSC_VK_F16 = 127;
  PSC_VK_F17 = 128;
  PSC_VK_F18 = 129;
  PSC_VK_F19 = 130;
  PSC_VK_F20 = 131;
  PSC_VK_F21 = 132;
  PSC_VK_F22 = 133;
  PSC_VK_F23 = 134;
  PSC_VK_F24 = 135;
  PSC_VK_NUMLOCK = 144;
  PSC_VK_SCROLL = 145;
  PSC_VK_LSHIFT = 160;
  PSC_VK_RSHIFT = 161;
  PSC_VK_LCONTROL = 162;
  PSC_VK_RCONTROL = 163;
  PSC_VK_LMENU = 164;
  PSC_VK_RMENU = 165;
  PSC_VK_PROCESSKEY = 229;
  PSC_VK_ATTN = 246;
  PSC_VK_CRSEL = 247;
  PSC_VK_EXSEL = 248;
  PSC_VK_EREOF = 249;
  PSC_VK_PLAY = 250;
  PSC_VK_ZOOM = 251;
  PSC_VK_NONAME = 252;
  PSC_VK_PA1 = 253;
  PSC_VK_OEM_CLEAR = 254;

  PSC_DT_TOP = 0;
  PSC_DT_LEFT = 0;
  PSC_DT_CENTER = 1;
  PSC_DT_RIGHT = 2;
  PSC_DT_VCENTER = 4;
  PSC_DT_BOTTOM = 8;
  PSC_DT_WORDBREAK = $10;
  PSC_DT_SINGLELINE = $20;
  PSC_DT_EXPANDTABS = $40;
  PSC_DT_TABSTOP = $80;
  PSC_DT_NOCLIP = $100;
  PSC_DT_EXTERNALLEADING = $200;
  PSC_DT_CALCRECT = $400;
  PSC_DT_NOPREFIX = $800;
  PSC_DT_INTERNAL = $1000;
  PSC_DT_HIDEPREFIX = $00100000;
  PSC_DT_PREFIXONLY = $00200000;

  PSC_DT_EDITCONTROL = $2000;
  PSC_DT_PATH_ELLIPSIS = $4000;
  PSC_DT_END_ELLIPSIS = $8000;
  PSC_DT_MODIFYSTRING = $10000;
  PSC_DT_RTLREADING = $20000;
  PSC_DT_WORD_ELLIPSIS = $40000;

  PSC_ODS_SELECTED = 1;
  PSC_ODS_GRAYED = 2;
  PSC_ODS_DISABLED = 4;
  PSC_ODS_CHECKED = 8;
  PSC_ODS_FOCUS = $10;
  PSC_ODS_DEFAULT = $20;
  PSC_ODS_COMBOBOXEDIT = $1000;
  PSC_ODS_HOTLIGHT = $40;
  PSC_ODS_INACTIVE = $80;

  PSC_DFC_CAPTION = 1;
  PSC_DFC_MENU = 2;
  PSC_DFC_SCROLL = 3;
  PSC_DFC_BUTTON = 4;
  PSC_DFC_POPUPMENU = 5;

  PSC_DFCS_CAPTIONCLOSE = 0;
  PSC_DFCS_CAPTIONMIN = 1;
  PSC_DFCS_CAPTIONMAX = 2;
  PSC_DFCS_CAPTIONRESTORE = 3;
  PSC_DFCS_CAPTIONHELP = 4;

  PSC_DFCS_MENUARROW = 0;
  PSC_DFCS_MENUCHECK = 1;
  PSC_DFCS_MENUBULLET = 2;
  PSC_DFCS_MENUARROWRIGHT = 4;

  PSC_DFCS_SCROLLUP = 0;
  PSC_DFCS_SCROLLDOWN = 1;
  PSC_DFCS_SCROLLLEFT = 2;
  PSC_DFCS_SCROLLRIGHT = 3;
  PSC_DFCS_SCROLLCOMBOBOX = 5;
  PSC_DFCS_SCROLLSIZEGRIP = 8;
  PSC_DFCS_SCROLLSIZEGRIPRIGHT = $10;

  PSC_DFCS_BUTTONCHECK = 0;
  PSC_DFCS_BUTTONRADIOIMAGE = 1;
  PSC_DFCS_BUTTONRADIOMASK = 2;
  PSC_DFCS_BUTTONRADIO = 4;
  PSC_DFCS_BUTTON3STATE = 8;
  PSC_DFCS_BUTTONPUSH = $10;

  PSC_DFCS_INACTIVE = $100;
  PSC_DFCS_PUSHED = $200;
  PSC_DFCS_CHECKED = $400;
  PSC_DFCS_TRANSPARENT = $800;
  PSC_DFCS_HOT = $1000;
  PSC_DFCS_ADJUSTRECT = $2000;
  PSC_DFCS_FLAT = $4000;
  PSC_DFCS_MONO = $8000;

  PSC_BDR_RAISEDOUTER = 1;
  PSC_BDR_SUNKENOUTER = 2;
  PSC_BDR_RAISEDINNER = 4;
  PSC_BDR_SUNKENINNER = 8;

  PSC_BDR_OUTER = 3;
  PSC_BDR_INNER = 12;
  PSC_BDR_RAISED = 5;
  PSC_BDR_SUNKEN = 10;

  PSC_EDGE_RAISED = (PSC_BDR_RAISEDOUTER or PSC_BDR_RAISEDINNER);
  PSC_EDGE_SUNKEN = (PSC_BDR_SUNKENOUTER or PSC_BDR_SUNKENINNER);
  PSC_EDGE_ETCHED = (PSC_BDR_SUNKENOUTER or PSC_BDR_RAISEDINNER);
  PSC_EDGE_BUMP = (PSC_BDR_RAISEDOUTER or PSC_BDR_SUNKENINNER);

  PSC_BF_LEFT = 1;
  PSC_BF_TOP = 2;
  PSC_BF_RIGHT = 4;
  PSC_BF_BOTTOM = 8;
  PSC_BF_TOPLEFT = (PSC_BF_TOP or PSC_BF_LEFT);
  PSC_BF_TOPRIGHT = (PSC_BF_TOP or PSC_BF_RIGHT);
  PSC_BF_BOTTOMLEFT = (PSC_BF_BOTTOM or PSC_BF_LEFT);
  PSC_BF_BOTTOMRIGHT = (PSC_BF_BOTTOM or PSC_BF_RIGHT);
  PSC_BF_RECT = (PSC_BF_LEFT or PSC_BF_TOP or PSC_BF_RIGHT or PSC_BF_BOTTOM);
  PSC_BF_DIAGONAL = $10;
  PSC_BF_DIAGONAL_ENDTOPRIGHT = (PSC_BF_DIAGONAL or PSC_BF_TOP or PSC_BF_RIGHT);
  PSC_BF_DIAGONAL_ENDTOPLEFT = (PSC_BF_DIAGONAL or PSC_BF_TOP or PSC_BF_LEFT);
  PSC_BF_DIAGONAL_ENDBOTTOMLEFT = (PSC_BF_DIAGONAL or PSC_BF_BOTTOM or PSC_BF_LEFT);
  PSC_BF_DIAGONAL_ENDBOTTOMRIGHT = (PSC_BF_DIAGONAL or PSC_BF_BOTTOM or PSC_BF_RIGHT);
  PSC_BF_MIDDLE = $800;   { Fill in the middle }
  PSC_BF_SOFT = $1000;    { For softer buttons }
  PSC_BF_ADJUST = $2000;  { Calculate the space left over }
  PSC_BF_FLAT = $4000;    { For flat rather than 3D borders }
  PSC_BF_MONO = $8000;    { For monochrome borders }

  PSC_SORT_DEFAULT                         = $0;     { sorting default }
  PSC_SORT_JAPANESE_XJIS                   = $0;     { Japanese XJIS order }
  PSC_SORT_JAPANESE_UNICODE                = $1;     { Japanese Unicode order }
  PSC_SORT_CHINESE_BIG5                    = $0;     { Chinese BIG5 order }
  PSC_SORT_CHINESE_PRCP                    = $0;     { PRC Chinese Phonetic order }
  PSC_SORT_CHINESE_UNICODE                 = $1;     { Chinese Unicode order }
  PSC_SORT_CHINESE_PRC                     = $2;     { PRC Chinese Stroke Count order }
  PSC_SORT_KOREAN_KSC                      = $0;     { Korean KSC order }
  PSC_SORT_KOREAN_UNICODE                  = $1;     { Korean Unicode order }
  PSC_SORT_GERMAN_PHONE_BOOK               = $1;     { German Phone Book order }

  PSC_SUBLANG_NEUTRAL                      = $00;    { language neutral }
  PSC_SUBLANG_DEFAULT                      = $01;    { user default }
  PSC_SUBLANG_SYS_DEFAULT                  = $02;    { system default }
  PSC_SUBLANG_ARABIC_SAUDI_ARABIA          = $01;    { Arabic (Saudi Arabia) }
  PSC_SUBLANG_ARABIC_IRAQ                  = $02;    { Arabic (Iraq) }
  PSC_SUBLANG_ARABIC_EGYPT                 = $03;    { Arabic (Egypt) }
  PSC_SUBLANG_ARABIC_LIBYA                 = $04;    { Arabic (Libya) }
  PSC_SUBLANG_ARABIC_ALGERIA               = $05;    { Arabic (Algeria) }
  PSC_SUBLANG_ARABIC_MOROCCO               = $06;    { Arabic (Morocco) }
  PSC_SUBLANG_ARABIC_TUNISIA               = $07;    { Arabic (Tunisia) }
  PSC_SUBLANG_ARABIC_OMAN                  = $08;    { Arabic (Oman) }
  PSC_SUBLANG_ARABIC_YEMEN                 = $09;    { Arabic (Yemen) }
  PSC_SUBLANG_ARABIC_SYRIA                 = $0a;    { Arabic (Syria) }
  PSC_SUBLANG_ARABIC_JORDAN                = $0b;    { Arabic (Jordan) }
  PSC_SUBLANG_ARABIC_LEBANON               = $0c;    { Arabic (Lebanon) }
  PSC_SUBLANG_ARABIC_KUWAIT                = $0d;    { Arabic (Kuwait) }
  PSC_SUBLANG_ARABIC_UAE                   = $0e;    { Arabic (U.A.E) }
  PSC_SUBLANG_ARABIC_BAHRAIN               = $0f;    { Arabic (Bahrain) }
  PSC_SUBLANG_ARABIC_QATAR                 = $10;    { Arabic (Qatar) }
  PSC_SUBLANG_CHINESE_TRADITIONAL          = $01;    { Chinese (Taiwan) }
  PSC_SUBLANG_CHINESE_SIMPLIFIED           = $02;    { Chinese (PR China) }
  PSC_SUBLANG_CHINESE_HONGKONG             = $03;    { Chinese (Hong Kong) }
  PSC_SUBLANG_CHINESE_SINGAPORE            = $04;    { Chinese (Singapore) }
  PSC_SUBLANG_DUTCH                        = $01;    { Dutch }
  PSC_SUBLANG_DUTCH_BELGIAN                = $02;    { Dutch (Belgian) }
  PSC_SUBLANG_ENGLISH_US                   = $01;    { English (USA) }
  PSC_SUBLANG_ENGLISH_UK                   = $02;    { English (UK) }
  PSC_SUBLANG_ENGLISH_AUS                  = $03;    { English (Australian) }
  PSC_SUBLANG_ENGLISH_CAN                  = $04;    { English (Canadian) }
  PSC_SUBLANG_ENGLISH_NZ                   = $05;    { English (New Zealand) }
  PSC_SUBLANG_ENGLISH_EIRE                 = $06;    { English (Irish) }
  PSC_SUBLANG_ENGLISH_SOUTH_AFRICA         = $07;    { English (South Africa) }
  PSC_SUBLANG_ENGLISH_JAMAICA              = $08;    { English (Jamaica) }
  PSC_SUBLANG_ENGLISH_CARIBBEAN            = $09;    { English (Caribbean) }
  PSC_SUBLANG_ENGLISH_BELIZE               = $0a;    { English (Belize) }
  PSC_SUBLANG_ENGLISH_TRINIDAD             = $0b;    { English (Trinidad) }
  PSC_SUBLANG_FRENCH                       = $01;    { French }
  PSC_SUBLANG_FRENCH_BELGIAN               = $02;    { French (Belgian) }
  PSC_SUBLANG_FRENCH_CANADIAN              = $03;    { French (Canadian) }
  PSC_SUBLANG_FRENCH_SWISS                 = $04;    { French (Swiss) }
  PSC_SUBLANG_FRENCH_LUXEMBOURG            = $05;    { French (Luxembourg) }
  PSC_SUBLANG_GERMAN                       = $01;    { German }
  PSC_SUBLANG_GERMAN_SWISS                 = $02;    { German (Swiss) }
  PSC_SUBLANG_GERMAN_AUSTRIAN              = $03;    { German (Austrian) }
  PSC_SUBLANG_GERMAN_LUXEMBOURG            = $04;    { German (Luxembourg) }
  PSC_SUBLANG_GERMAN_LIECHTENSTEIN         = $05;    { German (Liechtenstein) }
  PSC_SUBLANG_ITALIAN                      = $01;    { Italian }
  PSC_SUBLANG_ITALIAN_SWISS                = $02;    { Italian (Swiss) }
  PSC_SUBLANG_KOREAN                       = $01;    { Korean (Extended Wansung) }
  PSC_SUBLANG_KOREAN_JOHAB                 = $02;    { Korean (Johab) }
  PSC_SUBLANG_NORWEGIAN_BOKMAL             = $01;    { Norwegian (Bokmal) }
  PSC_SUBLANG_NORWEGIAN_NYNORSK            = $02;    { Norwegian (Nynorsk) }
  PSC_SUBLANG_PORTUGUESE                   = $02;    { Portuguese }
  PSC_SUBLANG_PORTUGUESE_BRAZILIAN         = $01;    { Portuguese (Brazilian) }
  PSC_SUBLANG_SERBIAN_LATIN                = $02;    { Serbian (Latin) }
  PSC_SUBLANG_SERBIAN_CYRILLIC             = $03;    { Serbian (Cyrillic) }
  PSC_SUBLANG_SPANISH                      = $01;    { Spanish (Castilian) }
  PSC_SUBLANG_SPANISH_MEXICAN              = $02;    { Spanish (Mexican) }
  PSC_SUBLANG_SPANISH_MODERN               = $03;    { Spanish (Modern) }
  PSC_SUBLANG_SPANISH_GUATEMALA            = $04;    { Spanish (Guatemala) }
  PSC_SUBLANG_SPANISH_COSTA_RICA           = $05;    { Spanish (Costa Rica) }
  PSC_SUBLANG_SPANISH_PANAMA               = $06;    { Spanish (Panama) }
  PSC_SUBLANG_SPANISH_DOMINICAN_REPUBLIC     = $07;  { Spanish (Dominican Republic) }
  PSC_SUBLANG_SPANISH_VENEZUELA            = $08;    { Spanish (Venezuela) }
  PSC_SUBLANG_SPANISH_COLOMBIA             = $09;    { Spanish (Colombia) }
  PSC_SUBLANG_SPANISH_PERU                 = $0a;    { Spanish (Peru) }
  PSC_SUBLANG_SPANISH_ARGENTINA            = $0b;    { Spanish (Argentina) }
  PSC_SUBLANG_SPANISH_ECUADOR              = $0c;    { Spanish (Ecuador) }
  PSC_SUBLANG_SPANISH_CHILE                = $0d;    { Spanish (Chile) }
  PSC_SUBLANG_SPANISH_URUGUAY              = $0e;    { Spanish (Uruguay) }
  PSC_SUBLANG_SPANISH_PARAGUAY             = $0f;    { Spanish (Paraguay) }
  PSC_SUBLANG_SPANISH_BOLIVIA              = $10;    { Spanish (Bolivia) }
  PSC_SUBLANG_SPANISH_EL_SALVADOR          = $11;    { Spanish (El Salvador) }
  PSC_SUBLANG_SPANISH_HONDURAS             = $12;    { Spanish (Honduras) }
  PSC_SUBLANG_SPANISH_NICARAGUA            = $13;    { Spanish (Nicaragua) }
  PSC_SUBLANG_SPANISH_PUERTO_RICO          = $14;    { Spanish (Puerto Rico) }
  PSC_SUBLANG_SWEDISH                      = $01;    { Swedish }
  PSC_SUBLANG_SWEDISH_FINLAND              = $02;    { Swedish (Finland) }

  PSC_LANG_NEUTRAL                         = $00;
  PSC_LANG_AFRIKAANS                       = $36;
  PSC_LANG_ALBANIAN                        = $1c;
  PSC_LANG_ARABIC                          = $01;
  PSC_LANG_BASQUE                          = $2d;
  PSC_LANG_BELARUSIAN                      = $23;
  PSC_LANG_BULGARIAN                       = $02;
  PSC_LANG_CATALAN                         = $03;
  PSC_LANG_CHINESE                         = $04;
  PSC_LANG_CROATIAN                        = $1a;
  PSC_LANG_CZECH                           = $05;
  PSC_LANG_DANISH                          = $06;
  PSC_LANG_DUTCH                           = $13;
  PSC_LANG_ENGLISH                         = $09;
  PSC_LANG_ESTONIAN                        = $25;
  PSC_LANG_FAEROESE                        = $38;
  PSC_LANG_FARSI                           = $29;
  PSC_LANG_FINNISH                         = $0b;
  PSC_LANG_FRENCH                          = $0c;
  PSC_LANG_GERMAN                          = $07;
  PSC_LANG_GREEK                           = $08;
  PSC_LANG_HEBREW                          = $0d;
  PSC_LANG_HUNGARIAN                       = $0e;
  PSC_LANG_ICELANDIC                       = $0f;
  PSC_LANG_INDONESIAN                      = $21;
  PSC_LANG_ITALIAN                         = $10;
  PSC_LANG_JAPANESE                        = $11;
  PSC_LANG_KOREAN                          = $12;
  PSC_LANG_LATVIAN                         = $26;
  PSC_LANG_LITHUANIAN                      = $27;
  PSC_LANG_NORWEGIAN                       = $14;
  PSC_LANG_POLISH                          = $15;
  PSC_LANG_PORTUGUESE                      = $16;
  PSC_LANG_ROMANIAN                        = $18;
  PSC_LANG_RUSSIAN                         = $19;
  PSC_LANG_SERBIAN                         = $1a;
  PSC_LANG_SLOVAK                          = $1b;
  PSC_LANG_SLOVENIAN                       = $24;
  PSC_LANG_SPANISH                         = $0a;
  PSC_LANG_SWEDISH                         = $1d;
  PSC_LANG_THAI                            = $1e;
  PSC_LANG_TURKISH                         = $1f;
  PSC_LANG_UKRAINIAN                       = $22;
  PSC_LANG_VIETNAMESE                      = $2a;

//BeginSkipConst
  SPSCCopyrightStr='Copyright (c) 1999-2002 %s.';

  SPSCTocExt = '.toc';
  SPSCPasExt = '.pas';
  SPSCIncExt = '.inc';
  SPSCintExt = '.int';
  SPSCRESExt = '.res';
  SPSCLogExt ='.log';
  SPSCDPRExt ='.dpr';
  SPSCDPKExt ='.dpk';
  SPSCBPKExt ='.bpk';
  SPSCBPRExt ='.bpr';
  SPSCDFMExt ='.dfm';
  SPSCBPLExt ='.bpl';
  SPSCDPLExt ='.dpl';
  SPSCDCUExt ='.dcu';
  SPSCDCPExt ='.dcp';
  SPSCtdsExt ='.tds';
  SPSCObjExt ='.obj';
  SPSCexeExt ='.exe';
  SPSCMakExt ='.mak';
  SPSCCPPExt ='.cpp';
  SPSCCFGExt ='.cfg';
  SPSCDOFExt ='.dof';
  SPSCRarExt = '.rar';
  SPSCIniExt = '.ini';
  SPSCHlpExt = '.hlp';
  SPSCCntExt = '.cnt';
  SPSCCHMExt = '.chm';
  SPSCDCRExt ='.dcr';
  SPSCGifExt ='.gif';
  SPSCDizExt ='.diz';
  SPSCTxtExt ='.txt';
  sPSChhkExt = '.hhk';
  sPSChhcExt = '.hhc';
  sPSChhpExt = '.hhp';
  sPSCRTFExt = '.rtf';
  sPSChpjExt = '.hpj';
  sPSCbmpExt = '.bmp';
  SPSCWavExt = '.wav';
  SPSCHtmlExt ='.html';
  SPSCPasMask = '*'+SPSCPasExt;
  SPSCAnyFileMask='*.*';
  SPSCUnrarDll='unrar.dll';
//EndSkipConst

  SPSC1310=#13#10;

  cPSCCountrySouth_Africa = (PSC_SORT_DEFAULT Shl 16) Or
    (PSC_SUBLANG_ENGLISH_SOUTH_AFRICA Shl 10) Or PSC_LANG_ENGLISH;

  cPSCCountryAlbania = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_ALBANIAN;
  cPSCCountrySaudi_Arabia = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_SAUDI_ARABIA
    Shl 10) Or PSC_LANG_ARABIC;
  cPSCCountryIraq = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_IRAQ Shl 10) Or
    PSC_LANG_ARABIC;
  cPSCCountryEgypt = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_EGYPT Shl 10) Or
    PSC_LANG_ARABIC;
  cPSCCountryLibya = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_LIBYA Shl 10) Or
    PSC_LANG_ARABIC;
  cPSCCountryAlgeria = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_ALGERIA Shl 10)
    Or PSC_LANG_ARABIC;
  cPSCCountryMorocco = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_MOROCCO Shl 10)
    Or PSC_LANG_ARABIC;
  cPSCCountryTunisia = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_TUNISIA Shl 10)
    Or PSC_LANG_ARABIC;
  cPSCCountryOman = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_OMAN Shl 10) Or
    PSC_LANG_ARABIC;
  cPSCCountryYemen = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_YEMEN Shl 10) Or
    PSC_LANG_ARABIC;
  cPSCCountrySyria = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_SYRIA Shl 10) Or
    PSC_LANG_ARABIC;
  cPSCCountryJordan = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_JORDAN Shl 10) Or
    PSC_LANG_ARABIC;
  cPSCCountryLebanon = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_LEBANON Shl 10)
    Or PSC_LANG_ARABIC;
  cPSCCountryKuwait = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_KUWAIT Shl 10) Or
    PSC_LANG_ARABIC;
  cPSCCountryUAE = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_UAE Shl 10) Or
    PSC_LANG_ARABIC;
  cPSCCountryBahrain = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_BAHRAIN Shl 10)
    Or PSC_LANG_ARABIC;
  cPSCCountryQatar = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ARABIC_QATAR Shl 10) Or
    PSC_LANG_ARABIC;
  cPSCCountryBelarus = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_BELARUSIAN;
  cPSCCountryBulgaria = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_BULGARIAN;
  cPSCCountryTaiwan = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_CHINESE_TRADITIONAL Shl
    10) Or PSC_LANG_CHINESE;
  cPSCCountryChina = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_CHINESE_SIMPLIFIED Shl 10)
    Or PSC_LANG_CHINESE;
  cPSCCountryHong_Kong = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_CHINESE_HONGKONG Shl
    10) Or PSC_LANG_CHINESE;
  cPSCCountrySingapore = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_CHINESE_SINGAPORE Shl
    10) Or PSC_LANG_CHINESE;
  cPSCCountryCroatia = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_CROATIAN;
  cPSCCountryCzech = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_CZECH;
  cPSCCountryDenmark = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_DANISH;
  cPSCCountryNetherlands = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DUTCH Shl 10) Or
    PSC_LANG_DUTCH;
  cPSCCountryBelgium = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DUTCH_BELGIAN Shl 10) Or
    PSC_LANG_DUTCH;
  cPSCCountryUS = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ENGLISH_US Shl 10) Or
    PSC_LANG_ENGLISH;
  cPSCCountryUK = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ENGLISH_UK Shl 10) Or
    PSC_LANG_ENGLISH;
  cPSCCountryAustralia = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ENGLISH_AUS Shl 10) Or
    PSC_LANG_ENGLISH;
  cPSCCountryCanada = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ENGLISH_CAN Shl 10) Or
    PSC_LANG_ENGLISH;
  cPSCCountryN_Zealand = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ENGLISH_NZ Shl 10) Or
    PSC_LANG_ENGLISH;
  cPSCCountryIreland = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ENGLISH_EIRE Shl 10) Or
    PSC_LANG_ENGLISH;
  cPSCCountryJamaica = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ENGLISH_JAMAICA Shl 10)
    Or PSC_LANG_ENGLISH;
  cPSCCountryCaribbean = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ENGLISH_CARIBBEAN Shl
    10) Or PSC_LANG_ENGLISH;
  cPSCCountryBelize = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ENGLISH_BELIZE Shl 10) Or
    PSC_LANG_ENGLISH;
  cPSCCountryTrinidad = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ENGLISH_TRINIDAD Shl
    10) Or PSC_LANG_ENGLISH;
  cPSCCountryEstonia = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_ESTONIAN;
  cPSCCountryFaeroe_Islands = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10)
    Or PSC_LANG_FAEROESE;
  cPSCCountryIran = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_FARSI;
  cPSCCountryFinland = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_FINNISH;
  cPSCCountryFrance = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_FRENCH Shl 10) Or
    PSC_LANG_FRENCH;
  cPSCCountryGermany = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_GERMAN Shl 10) Or
    PSC_LANG_GERMAN;
  cPSCCountrySwitzerland = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_GERMAN_SWISS Shl 10)
    Or PSC_LANG_GERMAN;
  cPSCCountryAustria = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_GERMAN_AUSTRIAN Shl 10)
    Or PSC_LANG_GERMAN;
  cPSCCountryLuxembourg = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_GERMAN_LUXEMBOURG Shl
    10) Or PSC_LANG_GERMAN;
  cPSCCountryLiechtenstein = (PSC_SORT_DEFAULT Shl 16) Or
    (PSC_SUBLANG_GERMAN_LIECHTENSTEIN Shl 10) Or PSC_LANG_GERMAN;
  cPSCCountryGreece = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_GREEK;
  cPSCCountryIsrael = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_HEBREW;
  cPSCCountryHungary = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_HUNGARIAN;
  cPSCCountryIceland = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_ICELANDIC;
  cPSCCountryIndonesia = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_INDONESIAN;
  cPSCCountryItaly = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_ITALIAN Shl 10) Or
    PSC_LANG_ITALIAN;
  cPSCCountryJapan = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_JAPANESE;
  cPSCCountryKorea = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_KOREAN Shl 10) Or
    PSC_LANG_KOREAN;
  cPSCCountryLatvia = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_LATVIAN;
  cPSCCountryLithuania = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_LITHUANIAN;
  cPSCCountryNorway = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_NORWEGIAN_BOKMAL Shl 10)
    Or PSC_LANG_NORWEGIAN;
  cPSCCountryPoland = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_POLISH;
  cPSCCountryPortugal = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_PORTUGUESE Shl 10) Or
    PSC_LANG_PORTUGUESE;
  cPSCCountryBrazil = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_PORTUGUESE_BRAZILIAN Shl
    10) Or PSC_LANG_PORTUGUESE;
  cPSCCountryRomania = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_ROMANIAN;
  cPSCCountryRussia = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_RUSSIAN;
  cPSCCountrySerbia = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SERBIAN_LATIN Shl 10) Or
    PSC_LANG_SERBIAN;
  cPSCCountrySlovak = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_SLOVAK;
  cPSCCountrySlovenia = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_SLOVENIAN;
  cPSCCountrySpain = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH Shl 10) Or
    PSC_LANG_SPANISH;
  cPSCCountryMexico = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_MEXICAN Shl 10)
    Or PSC_LANG_SPANISH;
  cPSCCountryGuatemala = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_GUATEMALA Shl
    10) Or PSC_LANG_SPANISH;
  cPSCCountryCosta_Rica = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_COSTA_RICA
    Shl 10) Or PSC_LANG_SPANISH;
  cPSCCountryPanama = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_PANAMA Shl 10) Or
    PSC_LANG_SPANISH;
  cPSCCountryDominican = (PSC_SORT_DEFAULT Shl 16) Or
    (PSC_SUBLANG_SPANISH_DOMINICAN_REPUBLIC Shl 10) Or PSC_LANG_SPANISH;
  cPSCCountryVenezuela = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_VENEZUELA Shl
    10) Or PSC_LANG_SPANISH;
  cPSCCountryColombia = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_COLOMBIA Shl
    10) Or PSC_LANG_SPANISH;
  cPSCCountryPeru = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_PERU Shl 10) Or
    PSC_LANG_SPANISH;
  cPSCCountryArgentina = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_ARGENTINA Shl
    10) Or PSC_LANG_SPANISH;
  cPSCCountryEcuador = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_ECUADOR Shl 10)
    Or PSC_LANG_SPANISH;
  cPSCCountryChile = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_CHILE Shl 10) Or
    PSC_LANG_SPANISH;
  cPSCCountryUruguay = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_URUGUAY Shl 10)
    Or PSC_LANG_SPANISH;
  cPSCCountryParaguay = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_PARAGUAY Shl
    10) Or PSC_LANG_SPANISH;
  cPSCCountryBolivia = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_BOLIVIA Shl 10)
    Or PSC_LANG_SPANISH;
  cPSCCountryEl_Salvador = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_EL_SALVADOR
    Shl 10) Or PSC_LANG_SPANISH;
  cPSCCountryHonduras = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_HONDURAS Shl
    10) Or PSC_LANG_SPANISH;
  cPSCCountryNicaragua = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_NICARAGUA Shl
    10) Or PSC_LANG_SPANISH;
  cPSCCountryPuerto_Rico = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SPANISH_PUERTO_RICO
    Shl 10) Or PSC_LANG_SPANISH;
  cPSCCountrySweden = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_SWEDISH Shl 10) Or
    PSC_LANG_SWEDISH;
  cPSCCountryThailand = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_THAI;
  cPSCCountryTurkey = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_TURKISH;
  cPSCCountryUkraine = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_UKRAINIAN;
  cPSCCountryVietNam = (PSC_SORT_DEFAULT Shl 16) Or (PSC_SUBLANG_DEFAULT Shl 10) Or
    PSC_LANG_VIETNAMESE;

  cPSCCountryArray: Array[0..91] Of TPSCCountryID = (
    cPSCCountrySouth_Africa,cPSCCountryAlbania,cPSCCountrySaudi_Arabia,
    cPSCCountryIraq,cPSCCountryEgypt,cPSCCountryLibya,
    cPSCCountryAlgeria,cPSCCountryMorocco,cPSCCountryTunisia,
    cPSCCountryOman,cPSCCountryYemen,cPSCCountrySyria,
    cPSCCountryJordan,cPSCCountryLebanon,cPSCCountryKuwait,
    cPSCCountryUAE,cPSCCountryBahrain,cPSCCountryQatar,
    cPSCCountryBelarus,cPSCCountryBulgaria,cPSCCountryTaiwan,
    cPSCCountryChina,cPSCCountryHong_Kong,cPSCCountrySingapore,
    cPSCCountryCroatia,cPSCCountryCzech,cPSCCountryDenmark,
    cPSCCountryNetherlands,cPSCCountryBelgium,cPSCCountryUS,
    cPSCCountryUK,cPSCCountryAustralia,cPSCCountryCanada,
    cPSCCountryN_Zealand,cPSCCountryIreland,
    cPSCCountryJamaica,cPSCCountryCaribbean,cPSCCountryBelize,
    cPSCCountryTrinidad,cPSCCountryEstonia,cPSCCountryFaeroe_Islands,
    cPSCCountryIran,cPSCCountryFinland,cPSCCountryFrance,
    cPSCCountryGermany,cPSCCountrySwitzerland,cPSCCountryAustria,
    cPSCCountryLuxembourg,cPSCCountryLiechtenstein,cPSCCountryGreece,
    cPSCCountryIsrael,cPSCCountryHungary,cPSCCountryIceland,
    cPSCCountryIndonesia,cPSCCountryItaly,cPSCCountryJapan,
    cPSCCountryKorea,cPSCCountryLatvia,cPSCCountryLithuania,
    cPSCCountryNorway,cPSCCountryPoland,cPSCCountryPortugal,
    cPSCCountryBrazil,cPSCCountryRomania,cPSCCountryRussia,
    cPSCCountrySerbia,cPSCCountrySlovak,cPSCCountrySlovenia,
    cPSCCountrySpain,cPSCCountryMexico,cPSCCountryGuatemala,
    cPSCCountryCosta_Rica,cPSCCountryPanama,cPSCCountryDominican,
    cPSCCountryVenezuela,cPSCCountryColombia,cPSCCountryPeru,
    cPSCCountryArgentina,cPSCCountryEcuador,cPSCCountryChile,
    cPSCCountryUruguay,cPSCCountryParaguay,cPSCCountryBolivia,
    cPSCCountryEl_Salvador,cPSCCountryHonduras,cPSCCountryNicaragua,
    cPSCCountryPuerto_Rico,cPSCCountrySweden,cPSCCountryThailand,
    cPSCCountryTurkey,cPSCCountryUkraine,cPSCCountryVietNam);

  CPSCDefEscapeChar: Char = '^';

  tok_like_Str = 1;
  tok_like_AnyChar = 2;
  tok_like_AnyChars = 3;
  tok_like_CharSet = 4;
  tok_like_NotCharSet = 5;
  tok_like_EscapeChar = 6;

type
  TPSCValuesContainer=class
  public
    IntValue:Integer;
    constructor Create(AIntValue:Integer);overload;
  end;

  TPSCPreparedMaskItem = Class(TObject)
  private
    FItemType: Integer;      
    FStrData: String;
    FCharSetData: TPSCCharSet;
    FUserData: Integer;
  public
    Property ItemType: Integer read FItemType write FItemType;
    Property StrData: String read FStrData write FStrData;
    Property CharSetData: TPSCCharSet read FCharSetData write FCharSetData;
    Property UserData: Integer read FUserData write FUserData;
  End;

  TPSCStringAndID=class(TObject)
  public
    ID:Integer;
    S:String;
  end;

  TPSCStringAndIDCompare=class(TInterfacedObject,IPSCCompareObjects)
  protected
    function CompareObjects(AObject1,AObject2:TObject):Integer;
  end;

  TPSCInterfacedObject=class(TInterfacedObject,IPSCInstanceID)
  private
    FInstanceID:Integer;
  protected
    function GetInstanceId:Integer;
  public
    constructor Create; 
  end;

  TPSCHandleEventProc=Procedure(const AParams:TPSCEventParams) of object;

  TPSCEvents=class(TPSCInterfacedObject,IPSCEvents,IPSCInterface)
  private
    FEventList:IPSCObjectList;
  protected
    procedure RegisterHandler(const AEventHandler:IPSCEventHandler);
    procedure UnregisterHandler(const AEventHandler:IPSCEventHandler);
    Procedure HandleEvent(const AParams:TPSCEventParams);
    Procedure HandleSimpleEvent(AEventType:TPSCEventType);
  public
    constructor Create;
  end;

{------------------------------------------------------------------------------}

function PSCGetArrayGrowDelta(ACapacity:Integer):Integer;
function PSCCreateObjectList(AOwnerShip:TPSCItemOwnership=ioReferenced):IPSCObjectList;
Function PSCParseLikeCharSetDef(Const inputS: String): TPSCCharSet;
Function PSCTimeOf(Const ADateTime: TDateTime): TDateTime;
Function PSCDateOf(Const ADateTime: TDateTime): TDateTime;
Function PSCDatesCompare(Const A,B: TDateTime;AKind:TPSCDateTimeKind): Integer;
function PSCSetStrCharsTo(const S:String;Index,Count:Integer;C:Char):String;
Function PSCTrimSeparators(Const S: String; Separators: TPSCCharSet): String;
Function PSCTrimSeparatorsRight(Const S: String; Separators: TPSCCharSet):
  String;
Function PSCTrimSeparatorsLeft(Const S: String; Separators: TPSCCharSet):
  String;
Function PSCTrim(Const S: String): String;
function PSCTrimNonTextChars(const S:String):String;
function PSCRemoveSpaces(const Str: string): string;
function PSCStrArrayToStr(const StrArray:Array of String;
  const Separator:String):String;
function PSCGetPasswordChar:AnsiChar;
Function PSCExtractValuePartEx(Const S,Separator: String): String;
Function PSCExtractNamePartEx(Const S,Separator: String): String;
Function PSCMax(A,B: Integer): Integer;overload;
function PSCMax(const A,B:TDateTime):TDateTime;overload;
Function PSCMin(A,B: Integer): Integer;overload;
function PSCMin(const A,B:TDateTime):TDateTime;overload;
Procedure PSCReplace(Var s: String; index: integer; Const OldStr,NewStr:
  String);
procedure PSCSeparateStrEx(S:String;
  const Separator:String;var NamePart,ValuePart:String);
procedure PSCSeparateStr(const S:String;var NamePart,ValuePart:String);
Function PSCPosEx(Const SubStr,S: String; FromChar: Integer=1): Integer;
function PSCAddCharBefore(const AStr: string; AChar: Char): string;
Function PSCBackPosEx(Const SubStr,S: String; FromChar: Integer): Integer;
Function PSCReplaceAllOccur(Var S: String; Const FromStr,ToStr: String):Integer;
Function PSCIntIsInArray(Value: Integer; Const A: Array Of Integer): boolean;
Function PSCStrIsInArray(const AValue: String;
  Const A: Array Of String): boolean;
Function PSCApplyLimits(Const AMinValue,AMaxValue: integer;
  Var AValue: integer): boolean;
function PSCRemoveExtraChars(const S: string; C: Char): string;
function PSCCharSetToString(CharSet:TPSCCharSet):AnsiString;
function PSCRandomString(ASize: Integer) : String;
Procedure PSCEnsureBound(Var EnsureLeft,EnsureRight: Integer;
  BaseLeft,BaseRight: Integer);
Function PSCExtractNamePart(Const S: String): String;
Function PSCRemoveSlash(Const Path: String): String;
Function PSCStringOfSpace(Num: Integer): String;
Function PSCBackPos(Const SubStr,S: String): Integer;
Function PSCGetOpenDlgFilter(Const FileCat,FileExt: String): String;
Function PSCRemoveCharSet(Const CharSet: TPSCCharSet; Const S: String):String;
Function PSCRemoveSpChars(Const S: String): String;
Function PSCRemoveColons(Const S: String): String;
Function PSCAddCRLF(Const S: String {AnsiString}): String; //AnsiString;
Function PSCTrimStringRight(Const S,PosStr: String): String;
Function PSCTrimStringLeft(Const S,PosStr: String): String;
Function PSCExtractValuePart(Const S: String): String;
function PSCGetOpenDlgFilters(const Categories,Extensions:Array of String):String;
Function PSCListsInterSect(const l1,l2: IPSCObjectList): boolean;
Function PSCLeaveCharSet(Const CharSet: TPSCCharSet; Const S: String):
  String;
Procedure PSCExtractPickListValues(Const PickListStr: String; Var
  DisplayValue,Value: String);
Function PSCStrInCharSet(Const S: String {AnsiString}; Const CharSet: TPSCCharSet): boolean;
Function PSCIncWeek(Const Date: TDateTime; NumberOfWeeks: Integer): TDateTime;
Function PSCAddChar(Const AStr: String; AChar: Char): String;
Function PSCMaxInArray(Const A: Array Of Integer): Integer;
Function PSCAddSlash(Const Path: String): String;
Function PSCCheckedToCheckState(Checked: boolean): TPSCCheckBoxState;
function PSCGetNewInstanceID:Integer;
function PSCCreateEventHandler(AProc:TPSCHandleEventProc):IPSCEventHandler;
Procedure PSCStringsToProc(const S: IPSCStrings; Proc: TPSCGetStrProc);
Function PSCFieldStringsToStr(const FieldList: IPSCStrings): String;
Procedure PSCStrArrayToStrings(const Strings: IPSCStrings; Const A: Array Of String);
Procedure PSCRemoveStringsPartEx(const Strings: IPSCStrings;
  Const Separator: String;NamePart: boolean);
Procedure PSCRemoveStrings(const FromStrings,StringsToRemove: IPSCStrings);
Procedure PSCRemoveStringsPart(const Strings: IPSCStrings; NamePart: boolean);
Function PSCAreObjNotEqualNotNil(Obj1,Obj2: TObject): Boolean;
procedure PSCTrimSeparatorsInStrings(const Strings: IPSCStrings;
  const Separators: TPSCCharSet);
procedure PSCRemoveEmptyStrings(const S:IPSCStrings);
function PSCUnParseStringEx(const Strings:IPSCStrings;const Separator:String;
  AddEndSeparator:boolean):String;
function PSCUnParseString(const Strings : IPSCStrings;const Separator : String):String;
function PSCRemoveExtraSlash(const FileName:String):String;
procedure PSCIntersectStrings(const Source, Dest : IPSCStrings);
function PSCFileNameHasWildCards(const FileName:String):boolean;

{------------------------------------------------------------------------------}

const
//BeginSkipConst

  SPSCDEFCMDParam   = 'DEF';
  SPSCIDEFCMDParam  = 'IDEF';
  SPSCINCCMDParam   = 'INC';
  SPSCTRIALCMDParam = 'TRIAL';
  SPSCPRODCMDParam  = 'PROD';

  SPSCProductVerNo = '2.3';
  SPSCOperationFltAnd                        = 'and';
  SPSCOperationFltOr                         = 'or';
  SPSCOperationFltAndNot                     = 'and not';
  SPSCOperationFltOrNot                      = 'or not';
  cPSCBoolToIntStr : array [Boolean] of char = ('0', '1');
  SPSCCompanyURL = '';
  SPSCProductVersion = 'V' + SPSCProductVerNo;
  cPSCShortDateFormatHelpList: Array[0..6] Of String =
  ('yyyy-MM-dd', 'M/d/yy', 'M/d/yyyy', 'MM/dd/yy', 'MM/dd/yyyy', 'yy/MM/dd',
    'dd-MMM-yy');
  cPSCLongDateFormatHelpList: Array[0..3] Of String =
  ('MMMM dd, yyyy', 'dddd, MMMM dd, yyyy', 'dddd, dd MMMM, yyyy',
    'dd MMMM, yyyy');
  cPSCTimeFormatHelpList: Array[0..3] Of String =
  ('h:mm:ss tt', 'hh:mm:ss tt', 'H:mm:ss', 'HH:mm:ss');

  cPSCAllowedFieldChars :TPSCCharSet =
  ['A'..'Z', 'a'..'z', '0'..'9', '_', '.'];
  cPSCAnyChar = [#0..#255];
  cPSCFirstIdentChar :TPSCCharSet= ['A'..'Z', 'a'..'z', '_'];
  cPSCOtherIdentChars :TPSCCharSet= ['A'..'Z', 'a'..'z', '_', '0'..'9'];
  cPSCHexDigit = ['0'..'9', 'A'..'F', 'a'..'f'];
  cPSCDigit = ['0'..'9'];
  cPSCFloatDigit = ['0'..'9', '.', 'e', 'E', '+', '-'];
  cPSCWhiteChar = [#33..#255];
  cPSCValidChar = [#9,#32..#255];

  SPSCStdFieldNameMask : String = '"%s"."%s"';
  SPSCStdFieldNameMaskShort : String = '"%s"';
  SPSCAdoFieldNameMask : String = '[%s].[%s]';
  SPSCAdoFieldNameMaskShort : String = '[%s]';
  SPSCUnderlineBeg = '<u>';
  SPSCUnderlineEnd = '</u>';
  SPSCSQLFalse = 'False';
  SPSCSQLTrue = 'True';

  PSCSqlResWords: Array[0..151] Of String = (
    'ACTIVE', 'ALL', 'AFTER', 'ALTER ', 'AND', 'ANY', 'AS', 'ASC', 'ASCENDING',
      'AT',
    'AUTO', 'AUTOINC', 'BASE_NAME', 'BEFORE', 'BEGIN', 'BETWEEN', 'BLOB',
      'BOOLEAN', 'BOTH',
    'BY', 'BYTES', 'CACHE', 'CAST', 'CHARACTER', 'CHECK', 'COLUMN', 'COMMIT',
      'COMMITTED',
    'COMPUTED', 'CONDITIONAL', 'CONNECT', 'CONSTRAINT', 'CONTAINING', 'COUNT',
      'CREATE',
    'CURRENT', 'CURSOR', 'DATABASE', 'DATE', 'DEBUG', 'DECIMAL', 'DECLARE',
      'DEFAULT', 'DELETE',
    'DESC', 'DESCENDING', 'DISTINCT', 'DOMAIN', 'DO', 'DROP', 'ELSE', 'END',
      'ENTRY_POINT',
    'ESCAPE', 'EXCEPTION', 'EXECUTE', 'EXISTS', 'EXIT', 'EXTERNAL', 'EXTRACT',
      'FILTER', 'FLOAT',
    'FOR', 'FOREIGN', 'FROM', 'FULL', 'FUNCTION', 'GENERATOR', 'GO', 'GRANT',
      'GROUP', 'HAVING',
    'IF', 'IN', 'INACTIVE', 'INDEX', 'INNER', 'INSERT', 'INTEGER', 'INTO', 'IS',
      'ISOLATION',
    'JOIN', 'KEY', 'NULL', 'LEADING', 'LEFT', 'LEVEL', 'LIKE', 'MERGE', 'MONEY',
      'NAMES', 'NO',
    'NOT', 'NUMERIC', 'OF', 'ON', 'ONLY', 'OR', 'ORDER', 'OUTER', 'PARAMETER',
      'PASSWORD', 'PIVOT',
    'PLAN', 'POSITION', 'PROCEDURE', 'PROTECTED', 'PRIMARY', 'PRIVILEGES',
      'READ', 'REFERENCES',
    'RETAIN', 'RETURNING_VALUES', 'RETURNS', 'REVOKE', 'RIGHT', 'ROLLBACK',
      'SCHEMA', 'SELECT',
    'SET', 'SHARED', 'SHADOW', 'SMALLINT', 'SNAPSHOT', 'SOME', 'SUSPEND',
      'TABLE', 'THEN', 'TIME',
    'TIMESTAMP', 'TO', 'TRAILING', 'TRANSFORM', 'TRANSACTION', 'TRIGGER',
      'UNCOMMITTED', 'UNION',
    'UNIQUE', 'UPDATE', 'USER', 'USING', 'VALUES', 'VARCHAR', 'VARIABLE', 'VIEW',
      'WAIT', 'WHEN',
    'WHERE', 'WHILE', 'WITH', 'WORK');
//EndSkipConst

Const
  CPSCElementHour=1;
  CPSCElementMinute=2;
  CPSCElementSecond=3;
  CPSCElementMSec=4;

  CPSCElementYear=1;
  CPSCElementMonth=2;
  CPSCElementDay=3;

{-------------------------------------------------------------------------}

Const
  cQuoteParamTypes: Set Of TPSCFieldType =
  [FT_STRING,FT_DATE,FT_TIME,FT_DATETIME,FT_MEMO,FT_WIDESTR];
  cNoDispValueTypes: Set Of TPSCFieldType = [FT_UNK,
  FT_BOOL,FT_CURRENCY,FT_DATE,FT_TIME,FT_DATETIME];

{------------------------------------------------------------------------------}

procedure PSCPerformForStrings(const Strings:IPSCStrings;Proc:TPSCStrConvertProc);
procedure PSCEnumDelphiResWords_NeverUse(const AStrings:IPSCStrings);
procedure PSCEnumDelphiResWords_All(const AStrings:IPSCStrings);

type
  IPSCExternalProcs=interface
    ['{BC965D6B-375D-4459-A8B7-4D38030E90D4}']
    function UpperCase(const S:String):String;
    function LowerCase(const S:String):String;
    function Now:TDateTime;

    function ColorToString(AColor:TPSCColor):String;
    function StringToColor(const S:String):TPSCColor;

    function DayOfWeek(ADate: TDateTime): Integer;
    function IsLeapYear(Year: Word): Boolean;
    function IntToStr(AValue: Integer): string;
    function EncodeDate(Year, Month, Day: Word): TDateTime;
    function StrToFloat(const S: string): Extended;
    function Format(const AFormat: string; const AArgs: array of const): string;
    function FloatToStr(AValue:Extended):String;
    function StrToIntDef(const S: string; Default: Integer): Integer;
    function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
    function IntToHex(Value: Integer; Digits: Integer): string;
    function StrToInt(const S: string): Integer;
    function IncMonth(const ADate: TDateTime;
      ANumberOfMonths: Integer = 1): TDateTime;
    procedure DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);
    procedure ShowMessage(const S:String);
    procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Word);
    procedure Beep;
  end;

procedure PSCSetExternalProcs(const V:IPSCExternalProcs);
function PSCCompareText(const S1, S2: string): Integer;
function PSCCompareTextCaseSens(const S1, S2: string): Integer;
function PSCNow:TDateTime;
function PSCUpperCase(const S:String):String;
function PSCLowerCase(const S:String):String;

function PSCStringToCharSet(InitValue:TPSCCharSet;const S:AnsiString;
  CaseSens:Boolean):TPSCCharSet;
function PSCBackPosIgnoreCase(const SubStr,S:String):Integer;
function PSCGetStrBetweenTagsEx(const Source: string; SearchFromPos: Integer;
  const TagBegin, TagEnd: string; var PosBegin, PosEnd: Integer): string;
function PSCGetStrBetweenTags(const Source: string; SearchFromPos: Integer;
  const TagBegin, TagEnd: string): string;
Function PSCYearFormatToYear2k(Const DateFormat: String): String;
Function PSCPosIgnoreCase(Const SubStr,S: String): Integer;
function PSCReplaceAllOccurEx(Var S: String; Const FromStr,ToStr: String;
  IgnoreCase: boolean):Integer;
Function PSCIncDay(Const Date: TDateTime; NumberOfDays: Integer): TDateTime;
Function PSCGetYesterday: TDateTime;
Function PSCGetTomorrow: TDateTime;
Function PSCGetTodayM7: TDateTime;
Function PSCGetTodayM8: TDateTime;
Function PSCGetToday: TDateTime;
Function PSCGetTodayP7: TDateTime;
Function PSCGetTodayP8: TDateTime;
procedure PSCChangeParamsInStringsEx(const S,Params:IPSCStrings;ParamSep:Char);
function PSCChangeParamsEx(const S:String;const Params:IPSCStrings;
  ParamSep:Char):String;
procedure PSCChangeParamsInStrings(const S,Params:IPSCStrings);
Function PSCIsSQLKeyWord(Const S: String): boolean;
Function PSCPrepareFieldName(Const FieldName: String;
  PrepareType: TPSCFieldPrepareType): String;
Function PSCIsSQLKeywordOrWithSpace(Const FieldName: String): boolean;

function PSCCreateStringList(AObjectsOwned:TPSCItemOwnership=ioReferenced):IPSCStrings;
Function PSCCreateSortedStringList(AObjectsOwned:TPSCItemOwnership=ioReferenced): IPSCStrings;

function PSCDayOfWeek(ADate: TDateTime): Integer;
function PSCIsLeapYear(Year: Word): Boolean;
function PSCIntToStr(AValue: Integer): string;
function PSCEncodeDate(Year, Month, Day: Word): TDateTime;
function PSCStrToFloat(const S: string): Extended;
function PSCFormat(const AFormat: string; const AArgs: array of const): string;
function PSCFloatToStr(AValue:Extended):String;
function PSCStrToIntDef(const S: string; Default: Integer): Integer;
function PSCEncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
function PSCIntToHex(Value: Integer; Digits: Integer): string;
function PSCStrToInt(const S: string): Integer;
function PSCIncMonth(const ADate: TDateTime;
  ANumberOfMonths: Integer = 1): TDateTime;
procedure PSCDecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);
procedure PSCShowMessage(const S:String);
procedure PSCDecodeDate(Date: TDateTime; var Year, Month, Day: Word);
procedure PSCBeep;
function PSCColorToString(AColor:TPSCColor):String;
function PSCStringToColor(const S:String):TPSCColor;

function PSCPoint(AX, AY: Integer): TPSCPoint;
function PSCRect(ALeft, ATop, ARight, ABottom: Integer): TPSCRect;

Function PSCChangeWeekDay(Const BaseDate: TDateTime; WeekDay: TPSCWeekDay):
  TDateTime;
Function PSCNextDay1(Const BaseDate: TDateTime; WeekDay: TPSCWeekDay):
  TDateTime;
Function PSCPriorDay1(Const BaseDate: TDateTime; WeekDay: TPSCWeekDay):
  TDateTime;
Function PSCChangeWeekDayEx(Const BaseDate: TDateTime; WeekDay: TPSCWeekDay;
  WeekDayNum: Integer): TDateTime;
Function PSCCorrectAndEncodeDate(Year,Month,Day: Integer): TDateTime;
Function PSCDayConstToDateTime(Const BaseDate: TDateTime; Const DayConst:
  TPSCDayConst): TDateTime;
Function PSCChangeDay(Const Date: TDateTime; NewDay: Word): TDateTime;
Function PSCGetMonthStart(Const Date: TDateTime): TDateTime;
Function PSCGetMonthEnd(Const Date: TDateTime): TDateTime;
Function PSCGetDateYear(Const ADate: TDateTime): Word;
Function PSCGetDateMonth(Const ADate: TDateTime): Word;
Function PSCGetDateDay(Const ADate: TDateTime): Word;
Function PSCDaysPerMonthDate(Const Date: TDateTime): Integer;
Function PSCDaysPerMonth(AYear,AMonth: Integer): Integer;
Function PSCGetThisMon1: TDateTime;
Function PSCGetThisMon31: TDateTime;
Function PSCGetLastMon1: TDateTime;
Function PSCGetLastMon31: TDateTime;
Function PSCGetNextMon1: TDateTime;
Function PSCGetNextMon31: TDateTime;
Function PSCColorToHTString(C: TPSCColor;const ATagName:String='c'): String;
function PSCChangeDateElement(const ADate:TDateTime;
  AIndex,ANewValue:Integer):TDateTime;
function PSCGetDateElement(const ADate: TDateTime; Index: Integer): Integer;
function PSCChangeTimeElement(const ATime:TDateTime;
  AIndex,ANewValue:Integer):TDateTime;
Function PSCGetTimeElement(Const ATime: TDateTime; Index: Integer): Integer;
procedure PSCInflateRect(var ARect: TPSCRect; X, Y: Integer);
procedure PSCOffsetRect(var ARect: TPSCRect; X, Y: Integer);
function PSCPtInRect(const Rect: TPSCRect; const P: TPSCPoint): Boolean;
function PSCIsRectEmpty(const Rect: TPSCRect): Boolean;
procedure PSCDelete(var S: string; Index, Count:Integer);
function PSCCombineDateTime(const ADate,ATime:TDateTime):TDateTime;
function PSCCreateMemField(const AName:String;AType:TPSCFieldType;
  const ATypeConvert:IPSCTypeConvert=nil):IPSCDataField;
function PSCCreateInterfaceList:IPSCInterfaceList;

function PSCCreateMemDataFields(const ADefs:IPSCDataFieldDefs;
  const ATypeConvert:IPSCTypeConvert=nil):IPSCDataFields;overload;
function PSCCreateMemDataFields(const ANames:Array of String;
  const ATypes:Array of TPSCFieldType;
  const ATypeConvert:IPSCTypeConvert=nil):IPSCDataFields;overload;

function PSCCreateMemDataFieldDefs:IPSCDataFieldDefsEx;overload;
function PSCCreateMemDataFieldDefs(const ANames:Array of String;
  const ATypes:Array of TPSCFieldType):IPSCDataFieldDefsEx;overload;
procedure PSCAnsiUpperCaseStrings(const S : IPSCStrings);
procedure PSCOperateStrings(const S:IPSCStrings;
  StringsProc:TPSCStringsOperateProc;UserData:Cardinal=0);
procedure PSCSortAndRemoveDups(const S:IPSCStrings);
Function PSCGetYear: Word;
{------------------------------------------------------------------------------}

implementation

{-------------------------------------------}

Function PSCGetYear: Word;
Begin
  Result := PSCGetDateYear(PSCNow);
End;

{-------------------------------------------------------------------------}

procedure PSCSortAndRemoveDups(const S:IPSCStrings);
var
  Temp:IPSCStrings;
begin
  Temp:=PSCCreateSortedStringList;
  Temp.AddStrings(S);
  S.Assign(Temp);
end;

{-------------------------------------------}

function PSCCombineDateTime(const ADate,ATime:TDateTime):TDateTime;
begin
  Result:=PSCDateOf(ADate)+PSCTimeOf(ATime);
end;

{-------------------------------------------}

Var
  DummyWord: Word;

{-------------------------------------------}

function PSCIsRectEmpty(const Rect: TPSCRect): Boolean;
begin
  Result := (Rect.Right <= Rect.Left) or (Rect.Bottom <= Rect.Top);
end;

{-------------------------------------------}

function PSCPtInRect(const Rect: TPSCRect; const P: TPSCPoint): Boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X < Rect.Right) and (P.Y >= Rect.Top)
    and (P.Y < Rect.Bottom);
end;

{-------------------------------------------}

Function PSCGetDateYear(Const ADate: TDateTime): Word;
Begin
  PSCDecodeDate(ADate,Result,DummyWord,DummyWord);
End;

{-------------------------------------------}

Function PSCGetDateMonth(Const ADate: TDateTime): Word;
Begin
  PSCDecodeDate(ADate,DummyWord,Result,DummyWord);
End;

{-------------------------------------------}

Function PSCGetDateDay(Const ADate: TDateTime): Word;
Begin
  PSCDecodeDate(ADate,DummyWord,DummyWord,Result);
End;

{-------------------------------------------}

Function PSCDaysPerMonthDate(Const Date: TDateTime): Integer;
Begin
  Result := PSCDaysPerMonth(PSCGetDateYear(Date),PSCGetDateMonth(Date));
End;

{-------------------------------------------}

Function PSCDaysPerMonth(AYear,AMonth: Integer): Integer;
Const
  DaysInMonth: Array[1..12] Of Integer = (31,28,31,30,31,30,31,31,30,31,30,31);
Begin
  Result := DaysInMonth[AMonth];
  If (AMonth = 2) And PSCIsLeapYear(AYear) Then
    Inc(Result);
End;

{-------------------------------------------}

Function PSCChangeDay(Const Date: TDateTime; NewDay: Word): TDateTime;
Begin
  Result := PSCIncDay(Date, -PSCGetDateDay(Date) + NewDay);
End;

{-------------------------------------------}

Function PSCGetMonthStart(Const Date: TDateTime): TDateTime;
Begin
  Result := PSCChangeDay(Date,1);
End;

{-------------------------------------------}

Function PSCGetMonthEnd(Const Date: TDateTime): TDateTime;
Begin
  Result := PSCChangeDay(Date,PSCDaysPerMonthDate(Date));
End;

{------------------------------------------------------------------------------}

Function PSCChangeWeekDay(Const BaseDate: TDateTime; WeekDay: TPSCWeekDay):
  TDateTime;
Begin
  Result := BaseDate - PSCDayOfWeek(BaseDate) + Integer(WeekDay);
End;

{------------------------------------------------------------------------------}

Function PSCNextDay1(Const BaseDate: TDateTime; WeekDay: TPSCWeekDay):
  TDateTime;
Begin
  Result := PSCChangeWeekDay(BaseDate,WeekDay);
  If PSCDateOf(BaseDate) > PSCDateOf(Result) Then
    Result := Result + 7;
End;

{------------------------------------------------------------------------------}

Function PSCPriorDay1(Const BaseDate: TDateTime; WeekDay: TPSCWeekDay):
  TDateTime;
Begin
  Result := PSCChangeWeekDay(BaseDate,WeekDay);
  If PSCDateOf(BaseDate) < PSCDateOf(Result) Then
    Result := Result - 7;
End;

{------------------------------------------------------------------------------}

Function PSCChangeWeekDayEx(Const BaseDate: TDateTime; WeekDay: TPSCWeekDay;
  WeekDayNum: Integer): TDateTime;
Begin
  If (WeekDayNum <= 0) Then
    Result := PSCPriorDay1(PSCGetMonthEnd(BaseDate),WeekDay)
  Else
    Begin
      Result := PSCNextDay1(PSCGetMonthStart(BaseDate),WeekDay);
      If WeekDayNum > 1 Then
        Result := Result + 7 * (WeekDayNum - 1);
      If Result > PSCGetMonthEnd(Result) Then
        Result := 1;
    End;
End;

{-------------------------------------------}

Function PSCCorrectAndEncodeDate(Year,Month,Day: Integer): TDateTime;
Begin
  If Month > 12 Then
    Begin
      Year := Year + (Month - 1) Div 12;
      Month := (Month - 1) Mod 12 + 1
    End
  Else
    If Month <= 0 Then
      Begin
        Year := Year + Month Div 12 - 1;
        Month := (Month - 1) Mod 12 + 12
      End;
  Result := PSCEncodeDate(Year,Month,1) + Day - 1
End;

{------------------------------------------------------------------------------}

Function PSCDayConstToDateTime(Const BaseDate: TDateTime; Const DayConst:
  TPSCDayConst): TDateTime;
Var
  Y,M,D: word;
Begin
  PSCDecodeDate(BaseDate,Y,M,D);
  With DayConst Do
    Case Kind Of
      dckDate:
        Result := PSCCorrectAndEncodeDate(Y,ord(Month),Day);
      dckProc:
        If Assigned(DateProc) Then
          Result := TPSCGetHolidayDateProc(DateProc)(BaseDate)
        Else
          Result := 1;
      dckDayNumber:
        Begin
          Result := PSCCorrectAndEncodeDate(Y,ord(Month),1);
          Result := PSCChangeWeekDayEx(Result,WeekDay,Day);
        End;
    Else
      Result := 1;
    End;
End;

{-------------------------------------------------------------}

function PSCRect(ALeft, ATop, ARight, ABottom: Integer): TPSCRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Bottom := ABottom;
  Result.Right := ARight;
end;

{-------------------------------------------------------------}

function PSCPoint(AX, AY: Integer): TPSCPoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;

{-------------------------------------------------------------}

type
  TPSCStrings=class;

  TPSCStringsItem=class(TObject)
  private
    FStrings:TPSCStrings;
  public
    S:String;
    AnObject:TObject;
    constructor Create(AStrings:TPSCStrings);
    destructor Destroy;override;
  end;

  TPSCStringsItemCompare=class(TInterfacedObject,IPSCCompareObjects)
  protected
    function CompareObjects(AObject1,AObject2:TObject):Integer;
    function CompareStrings(const S1,S2:String):Integer;virtual;
  end;

  TPSCStrings=class(TInterfacedObject,IPSCStrings)
  private
    FList:IPSCObjectList;
    FCompare:IPSCCompareObjects;
    FObjectsOwned:TPSCItemOwnership;

  protected
    function GetItem(AIndex: Integer):TPSCStringsItem;
    function Add(const S: string): Integer;
    function GetObject(AIndex: Integer):TObject;
    function GetString(AIndex: Integer): string;
    function AddObject(const S: string; AObject: TObject): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    function GetCount:Integer;
    function GetName(AIndex: Integer):String;
    function GetCapacity:Integer;
    function GetValue(const AName: string): string;
    function GetCommaText:String;
    function GetTextStr:String;
    function IndexOf(const S: string): Integer;
    function IndexOfName(const Name: string): Integer;
    function Find(const S: string; var Index: Integer): Boolean;
    function GetDuplicates: TPSCDuplicates;
    function GetSorted:Boolean;

    procedure PutObject(AIndex: Integer;AObject:TObject);
    procedure BeginUpdate;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure EndUpdate;
    procedure SetCapacity(AValue:Integer);
    procedure Assign(const AStrings:IPSCStrings);
    procedure AssignTo(const AStrings:IPSCStrings);
    procedure AddStrings(const AStrings:IPSCStrings);
    procedure SetValue(const AName,AValue: string);
    procedure PutString(AIndex: Integer;const AValue: string);
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject);
    procedure SetDuplicates(AValue:TPSCDuplicates);
    procedure SetSorted(AValue:Boolean);
    procedure Sort;
    procedure SetCount(V:Integer);

    procedure SetTextStr(const AValue:String);

    property Objects[Index: Integer]: TObject read GetObject write PutObject;
  public
    constructor Create(AObjectsOwned:TPSCItemOwnership);
  end;

{----------------------------------------}

procedure TPSCStrings.SetTextStr(const AValue:String);
var
  P, Start: Integer;
  S: string;
begin
  BeginUpdate;
  try
    Clear;
    P:=1;
    while P <= Length(AValue) do
    begin
      Start := P;
      // while not (AValue[P] in [Char(#0), Char(#10), Char(#13)]) do Inc(P);
      while not CharInSet(AValue[P], [#0,#10,#13]) do Inc(P);
      S:=Copy(AValue,Start,P - Start);
      Add(S);
      if AValue[P] = #13 then Inc(P);
      if AValue[P] = #10 then Inc(P);
    end;
  finally
    EndUpdate;
  end;
end;

{----------------------------------------}

function PSCCreateStringList(AObjectsOwned:TPSCItemOwnership=ioReferenced):IPSCStrings;
begin
  Result:=TPSCStrings.Create(AObjectsOwned);
end;

{----------------------------------------}

Function PSCCreateSortedStringList(AObjectsOwned:TPSCItemOwnership=ioReferenced): IPSCStrings;
begin
  Result:=TPSCStrings.Create(AObjectsOwned);
  Result.Sorted:=True;
end;

{----------------------------------------}

constructor TPSCStringsItem.Create(AStrings:TPSCStrings);
begin
  inherited Create;
  FStrings:=AStrings;
end;

{----------------------------------------}

destructor TPSCStringsItem.Destroy;
begin
  If FStrings.FObjectsOwned=ioOwned then
  begin
    AnObject.Free;
    AnObject:=nil;
  end;
  inherited;
end;

{----------------------------------------}

function TPSCStrings.GetDuplicates: TPSCDuplicates;
begin
  Result:=FList.Duplicates;
end;

{----------------------------------------}

procedure TPSCStrings.SetDuplicates(AValue:TPSCDuplicates);
begin
  FList.Duplicates:=AValue;
end;

{----------------------------------------}

function TPSCStrings.GetSorted:Boolean;
begin
  Result:=FList.Sorted;
end;

{----------------------------------------}

procedure TPSCStrings.SetSorted(AValue:Boolean);
begin
  If GetSorted<>AValue then
  begin
    If AValue then
      FList.SetSortCriteria(FCompare)
    else
      FList.SetSortCriteria(nil);
  end;
end;

{----------------------------------------}

procedure TPSCStrings.Sort;
begin
  FList.Sort(FCompare);
end;

{----------------------------------------}

function TPSCStrings.IndexOf(const S: string): Integer;
var
  i:Integer;
  MyObject:TPSCStringsItem;
begin
  If GetSorted then
    begin
      MyObject:=TPSCStringsItem.Create(Self);
      MyObject.S:=S;
      Result:=FList.IndexOf(MyObject);
      MyObject.Free;
    end
  else
    begin
      for i:=0 to GetCount-1 do
        If PSCCompareText(GetString(i),S)=0 then
        begin
          Result:=i;
          exit;
        end;
      Result:=-1;
    end;  
end;

{----------------------------------------}

function TPSCStrings.Find(const S: string; var Index: Integer): Boolean;
begin
  Index:=IndexOf(S);
  Result:=Index>=0;
end;

{----------------------------------------}

procedure TPSCStrings.InsertObject(Index: Integer; const S: string; AObject: TObject);
var
  MyItem:TPSCStringsItem;
begin
  MyItem:=TPSCStringsItem.Create(Self);
  MyItem.S:=S;
  MyItem.AnObject:=AObject;
  FList.Insert(Index,MyItem);
end;

{----------------------------------------}

function TPSCStrings.IndexOfName(const Name: string): Integer;
var
  i:Integer;
begin
  for i:=0 to GetCount-1 do
    If PSCCompareText(GetName(i),Name)=0 then
    begin
      Result:=i;
      exit;
    end;
  Result:=-1;
end;

{----------------------------------------}

function TPSCStringsItemCompare.CompareObjects(AObject1,AObject2:TObject):Integer;
begin
  Result:=CompareStrings(TPSCStringsItem(AObject1).S,TPSCStringsItem(AObject2).S);
end;

{----------------------------------------}

function TPSCStringsItemCompare.CompareStrings(const S1,S2:String):Integer;
begin
  Result:=PSCCompareText(S1,S2);
end;

{----------------------------------------}

function TPSCStrings.GetCommaText:String;
begin
  Result:=PSCUnParseStringEx(Self,',',False);
end;

{----------------------------------------}

function TPSCStrings.GetTextStr:String;
begin
  Result:=PSCUnParseStringEx(Self,#13#10,True);
end;

{----------------------------------------}

procedure TPSCStrings.PutString(AIndex: Integer; const AValue: string);
var
  MyObject: TObject;
begin
  MyObject := Objects[AIndex];
  Delete(AIndex);
  InsertObject(AIndex, AValue, MyObject);
end;

{----------------------------------------}

procedure TPSCStrings.SetValue(const AName,AValue: string);
var
  MyIndex:Integer;
  S:String;
begin
  MyIndex:=IndexOfName(AName);
  S:=AName+'='+AValue;
  If MyIndex>=0 then
    PutString(MyIndex,S)
  else
    Add(S)
end;

{----------------------------------------}

function TPSCStrings.GetValue(const AName: string): string;
var
  MyIndex:Integer;
begin
  MyIndex:=IndexOfName(AName);
  If MyIndex>=0 then
    Result:=PSCExtractValuePart(GetItem(MyIndex).S)
  else
    Result:='';
end;

{----------------------------------------}

procedure TPSCStrings.AddStrings(const AStrings:IPSCStrings);
var
  i:Integer;
begin
  BeginUpdate;
  try
    for i:=0 to AStrings.GetCount-1 do
      AddObject(AStrings.Strings[i],AStrings.Objects[i]);
  finally
    EndUpdate;
  end;
end;

{----------------------------------------}

procedure TPSCStrings.Assign(const AStrings:IPSCStrings);
begin
  BeginUpdate;
  try
    Clear;
    AddStrings(AStrings);
  finally
    EndUpdate;
  end;
end;

{----------------------------------------}

procedure TPSCStrings.AssignTo(const AStrings:IPSCStrings);
begin
  AStrings.Assign(Self);
end;

{----------------------------------------}

function TPSCStrings.GetName(AIndex: Integer):String;
begin
  Result:=PSCExtractNamePart(GetItem(AIndex).S);
end;

{----------------------------------------}

function TPSCStrings.GetCapacity:Integer;
begin
  Result:=FList.Capacity;
end;

{----------------------------------------}

procedure TPSCStrings.SetCount(V:Integer);
begin
  If (V=GetCount) or (V<0) then
    exit;
  While V<GetCount do
    Delete(GetCount-1);
  While V>GetCount do
    Add('');
end;

{----------------------------------------}

procedure TPSCStrings.SetCapacity(AValue:Integer);
begin
  FList.SetCapacity(AValue);
end;

{----------------------------------------}

procedure TPSCStrings.BeginUpdate;
begin
end;

{----------------------------------------}

procedure TPSCStrings.Clear;
begin
  FList.Clear;
end;

{----------------------------------------}

procedure TPSCStrings.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

{----------------------------------------}

procedure TPSCStrings.EndUpdate;
begin
end;

{----------------------------------------}

function TPSCStrings.AddObject(const S: string; AObject: TObject): Integer;
var
  MyItem:TPSCStringsItem;
begin
  MyItem:=TPSCStringsItem.Create(Self);
  MyItem.S:=S;
  MyItem.AnObject:=AObject;
  Result:=FList.Add(MyItem);
end;

{----------------------------------------}

function TPSCStrings.IndexOfObject(AObject: TObject): Integer;
var
  i:Integer;
begin
  for i:=0 to GetCount-1 do
    If GetItem(i).AnObject=AObject then
    begin
      Result:=i;
      exit;
    end;
  Result:=-1;
end;

{----------------------------------------}

function TPSCStrings.GetCount:Integer;
begin
  Result:=FList.GetCount;
end;

{----------------------------------------}

procedure TPSCStrings.PutObject(AIndex: Integer;AObject:TObject);
begin
  GetItem(AIndex).AnObject:=AObject;
end;

{----------------------------------------}

constructor TPSCStrings.Create(AObjectsOwned:TPSCItemOwnership);
begin
  inherited Create;
  FObjectsOwned:=AObjectsOwned;
  FList:=PSCCreateObjectList(ioOwned);
  FCompare:=TPSCStringsItemCompare.Create;
end;

{----------------------------------------}

function TPSCStrings.GetItem(AIndex: Integer):TPSCStringsItem;
begin
  Result:=TPSCStringsItem(FList.GetItem(AIndex))
end;

{----------------------------------------}

function TPSCStrings.GetObject(AIndex: Integer):TObject;
begin
  Result:=GetItem(AIndex).AnObject;
end;

{----------------------------------------}

function TPSCStrings.GetString(AIndex: Integer): string;
begin
  Result:=GetItem(AIndex).S;
end;

{----------------------------------------}

function TPSCStrings.Add(const S: string): Integer;
begin
  Result:=AddObject(S,nil);
end;

{-------------------------------------------}

Function PSCIsSQLKeywordOrWithSpace(Const FieldName: String): boolean;
Begin
  If Length(FieldName) = 0 Then
    Result := False
  Else
    Result := (Pos(' ',FieldName) > 0) Or (Pos('#',FieldName) > 0)
      // Or (FieldName[1] In [Char('0')..Char('9')])
      Or CharInSet(FieldName[1], [AnsiChar('0')..AnsiChar('9')])
      Or PSCIsSQLKeyword(FieldName) Or Not
        PSCStrInCharSet(FieldName,cPSCAllowedFieldChars);
End;

{-------------------------------------------}

Function PSCPrepareFieldName(Const FieldName: String;
  PrepareType: TPSCFieldPrepareType): String;
Begin
  Case PrepareType Of
    fptEncloseInBrackets:
      If PSCIsSQLKeywordOrWithSpace(FieldName) Then
        Result := '[' + FieldName + ']'
      Else
        Result := FieldName;
  Else
    Result := FieldName;

  End;
End;

{-------------------------------------------}

Function PSCIsSQLKeyWord(Const S: String): boolean;
Var
  i: Integer;
Begin
  For i := Low(PSCSqlResWords) To High(PSCSqlResWords) Do
    If PSCCompareText(S,PSCSqlResWords[i]) = 0 Then
      Begin
        Result := True;
        exit;
      End;
  Result := False;
End;

{-------------------------------------------------------------------------}

procedure PSCChangeParamsInStrings(const S,Params:IPSCStrings);
begin
  PSCChangeParamsInStringsEx(S,Params,'%');
end;

{-------------------------------------------------------------------------}

function PSCChangeParamsEx(const S:String;const Params:IPSCStrings;
  ParamSep:Char):String;
var
  i:Integer;
  FromStr,ToStr:String;
begin
  Result:=S;
  If S='' then
    exit;
  With Params do
    for i:=0 to Count-1 do
    begin
      FromStr:=ParamSep+PSCTrim(PSCExtractNamePart(Strings[i]))+ParamSep;
      ToStr:=PSCTrim(PSCExtractValuePart(Strings[i]));
      PSCReplaceAllOccurEx(Result,FromStr,ToStr,True);
    end;
end;

{-------------------------------------------------------------------------}

procedure PSCChangeParamsInStringsEx(const S,Params:IPSCStrings;ParamSep:Char);
var
  i:Integer;
begin
  With S do
  begin
    BeginUpdate;
    try
      for i:=0 to Count-1 do
        Strings[i]:=PSCChangeParamsEx(Strings[i],Params,ParamSep);
    finally
      EndUpdate;
    end;
  end;
end;

{-------------------------------------------}

Function PSCGetYesterday: TDateTime;
Begin
  Result := PSCDateOf(PSCIncDay(PSCDateOf(PSCNow), -1));
End;

{-------------------------------------------}

Function PSCGetTomorrow: TDateTime;
Begin
  Result := PSCDateOf(PSCIncDay(PSCDateOf(PSCNow),1));
End;

{-------------------------------------------}

Function PSCGetTodayM7: TDateTime;
Begin
  Result := PSCDateOf(PSCIncDay(PSCDateOf(PSCNow), -7));
End;

{-------------------------------------------}

Function PSCGetTodayM8: TDateTime;
Begin
  Result := PSCDateOf(PSCIncDay(PSCDateOf(PSCNow), -8));
End;

{-------------------------------------------}

Function PSCGetToday: TDateTime;
Begin
  Result := PSCDateOf(PSCNow);
End;

{-------------------------------------------}

Function PSCGetTodayP7: TDateTime;
Begin
  Result := PSCDateOf(PSCIncDay(PSCGetToday,7));
End;

{-------------------------------------------}

Function PSCGetTodayP8: TDateTime;
Begin
  Result := PSCDateOf(PSCIncDay(PSCGetToday,8));
End;

{-------------------------------------------}

Function PSCIncDay(Const Date: TDateTime; NumberOfDays: Integer): TDateTime;
Begin
  Result := Date + NumberOfDays
End;

{------------------------------------------------------------}

function PSCReplaceAllOccurEx(Var S: String; Const FromStr,ToStr: String;
  IgnoreCase: boolean):Integer;
Var
  PosChar: Integer;
  WorkStr,WorkFromStr: String;
Begin
  If IgnoreCase = False Then
    Begin
      Result:=PSCReplaceAllOccur(S,FromStr,ToStr);
      exit;
    End;

  PosChar := 1;
  Result  := 0;

  If IgnoreCase Then
    Begin
      WorkStr := PSCUpperCase(S);
      WorkFromStr := PSCUpperCase(FromStr);
    End;

  Repeat
    PosChar := PSCPosEx(WorkFromStr,WorkStr,PosChar);
    If PosChar = 0 Then
      break;
    PSCReplace(WorkStr,PosChar,WorkFromStr,ToStr);
    PSCReplace(S,PosChar,FromStr,ToStr);
    inc(Result);
    Inc(PosChar,Length(ToStr));
  Until false;
End;

{-------------------------------------------}

Function PSCPosIgnoreCase(Const SubStr,S: String): Integer;
Begin
  Result := Pos(PSCUpperCase(SubStr),PSCUpperCase(S));
End;

{------------------------------------------------------------------}

Function PSCYearFormatToYear2k(Const DateFormat: String): String;
Var
  Uppercased: String;
  P: Integer;
Begin
  Result := DateFormat;
  Uppercased := PSCUpperCase(Result);
  If Pos('YYYY',Uppercased) = 0 Then //don't resource
    Begin
      P := Pos('YY',Uppercased); //don't resource
      If P <> 0 Then
        Insert('yy',Result,P); //don't resource
    End;
End;

{--------------------------------------}

function PSCGetStrBetweenTags(const Source: string; SearchFromPos: Integer;
  const TagBegin, TagEnd: string): string;
var
  PosBegin, PosEnd: Integer;
begin
  Result := PSCGetStrBetweenTagsEx(Source, SearchFromPos, TagBegin, TagEnd,
    PosBegin, PosEnd);
end;

{-------------------------------------------------------------}

function PSCExternalProcs:IPSCExternalProcs;forward;

{-------------------------------------------------------------}

function PSCColorToString(AColor:TPSCColor):String;
begin
  Result:=PSCExternalProcs.ColorToString(AColor);
end;

{-------------------------------------------------------------}

function PSCStringToColor(const S:String):TPSCColor;
begin
  Result:=PSCExternalProcs.StringToColor(S);
end;

{-------------------------------------------------------------}

function PSCDayOfWeek(ADate: TDateTime): Integer;
begin
  Result:=PSCExternalProcs.DayOfWeek(ADate);
end;

{-------------------------------------------------------------}

function PSCIsLeapYear(Year: Word): Boolean;
begin
  Result:=PSCExternalProcs.IsLeapYear(Year);
end;

{-------------------------------------------------------------}

function PSCIntToStr(AValue: Integer): string;
begin
  Result:=PSCExternalProcs.IntToStr(AValue);
end;

{-------------------------------------------------------------}

function PSCEncodeDate(Year, Month, Day: Word): TDateTime;
begin
  Result:=PSCExternalProcs.EncodeDate(Year, Month, Day);
end;

{-------------------------------------------------------------}

function PSCStrToFloat(const S: string): Extended;
begin
  Result:=PSCExternalProcs.StrToFloat(S);
end;

{-------------------------------------------------------------}

function PSCFormat(const AFormat: string; const AArgs: array of const): string;
begin
  Result:=PSCExternalProcs.Format(AFormat,AArgs);
end;

{-------------------------------------------------------------}

function PSCFloatToStr(AValue:Extended):String;
begin
  Result:=PSCExternalProcs.FloatToStr(AValue);
end;

{-------------------------------------------------------------}

function PSCStrToIntDef(const S: string; Default: Integer): Integer;
begin
  Result:=PSCExternalProcs.StrToIntDef(S,Default);
end;

{-------------------------------------------------------------}

function PSCEncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
begin
  Result:=PSCExternalProcs.EncodeTime(Hour, Min, Sec, MSec);
end;

{-------------------------------------------------------------}

function PSCIntToHex(Value: Integer; Digits: Integer): string;
begin
  Result:=PSCExternalProcs.IntToHex(Value,Digits);
end;

{-------------------------------------------------------------}

function PSCStrToInt(const S: string): Integer;
begin
  Result:=PSCExternalProcs.StrToInt(S);
end;

{-------------------------------------------------------------}

function PSCIncMonth(const ADate: TDateTime;
  ANumberOfMonths: Integer = 1): TDateTime;
begin
  Result:=PSCExternalProcs.IncMonth(ADate,ANumberOfMonths);
end;

{-------------------------------------------------------------}

procedure PSCDecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);
begin
  PSCExternalProcs.DecodeTime(Time,Hour, Min, Sec, MSec);
end;

{-------------------------------------------------------------}

procedure PSCShowMessage(const S:String);
begin
  PSCExternalProcs.ShowMessage(S);
end;

{-------------------------------------------------------------}

procedure PSCDecodeDate(Date: TDateTime; var Year, Month, Day: Word);
begin
  PSCExternalProcs.DecodeDate(Date,Year, Month, Day);
end;

{-------------------------------------------------------------}

procedure PSCBeep;
begin
  PSCExternalProcs.Beep;
end;

{-------------------------------------------------------------}

function PSCUpperCase(const S:String):String;
begin
  Result:=PSCExternalProcs.UpperCase(S);
end;

{-------------------------------------------------------------}

function PSCLowerCase(const S:String):String;
begin
  Result:=PSCExternalProcs.LowerCase(S);
end;

{-------------------------------------------------------------}

function PSCNow:TDateTime;
begin
  Result:=PSCExternalProcs.Now;
end;

{-------------------------------------------------------------}

function PSCCompareTextCaseSens(const S1, S2: string): Integer;
var
  i:Integer;
  L1,L2:Integer;
begin
  L1:=Length(S1);
  L2:=Length(S2);
  Result:=0;
  for i:=1 to PSCMin(L1,L2) do
    begin
      Result:=Ord(S1[i])-Ord(S2[i]);
      If Result<>0 then
        exit;
    end;
  If Result=0 then
    Result:=L1-L2;
end;

{-------------------------------------------------------------}

function PSCCompareText(const S1, S2: string): Integer;
begin
  Result:=PSCCompareTextCaseSens(PSCExternalProcs.UpperCase(S1),
    PSCExternalProcs.UpperCase(S2));
end;

{-------------------------------------------------------------}

var
  FExternalProcs:IPSCExternalProcs;

function PSCExternalProcs:IPSCExternalProcs;
begin
  Result:=FExternalProcs;
end;

{-------------------------------------------------------------}

procedure PSCSetExternalProcs(const V:IPSCExternalProcs);
begin
  FExternalProcs:=V;
end;

{-------------------------------------------------------------}

procedure PSCEnumDelphiResWords_All(const AStrings:IPSCStrings);
begin
  With AStrings do
  begin
    Add(SPSC_PAS_absolute);
    Add(SPSC_PAS_abstract);
    Add(SPSC_PAS_and);
    Add(SPSC_PAS_array);
    Add(SPSC_PAS_as);
    Add(SPSC_PAS_asm);
    Add(SPSC_PAS_assembler);
    Add(SPSC_PAS_automated);
    Add(SPSC_PAS_begin);
    Add(SPSC_PAS_break);
    Add(SPSC_PAS_case);
    Add(SPSC_PAS_cdecl);
    Add(SPSC_PAS_class);
    Add(SPSC_PAS_const);
    Add(SPSC_PAS_constructor);
    Add(SPSC_PAS_continue);
    Add(SPSC_PAS_default);
    Add(SPSC_PAS_destructor);
    Add(SPSC_PAS_dispid);
    Add(SPSC_PAS_dispinterface);
    Add(SPSC_PAS_div);
    Add(SPSC_PAS_do);
    Add(SPSC_PAS_downto);
    Add(SPSC_PAS_dynamic);
    Add(SPSC_PAS_else);
    Add(SPSC_PAS_end);
    Add(SPSC_PAS_except);
    Add(SPSC_PAS_exit);
    Add(SPSC_PAS_export);
    Add(SPSC_PAS_exports);
    Add(SPSC_PAS_external);
    Add(SPSC_PAS_far);
    Add(SPSC_PAS_file);
    Add(SPSC_PAS_finalization);
    Add(SPSC_PAS_finally);
    Add(SPSC_PAS_for);
    Add(SPSC_PAS_forward);
    Add(SPSC_PAS_function);
    Add(SPSC_PAS_goto);
    Add(SPSC_PAS_if);
    Add(SPSC_PAS_implementation);
    Add(SPSC_PAS_in);
    Add(SPSC_PAS_index);
    Add(SPSC_PAS_inherited);
    Add(SPSC_PAS_initialization);
    Add(SPSC_PAS_inline);
    Add(SPSC_PAS_interface);
    Add(SPSC_PAS_is);
    Add(SPSC_PAS_label);
    Add(SPSC_PAS_library);
    Add(SPSC_PAS_message);
    Add(SPSC_PAS_mod);
    Add(SPSC_PAS_near);
    Add(SPSC_PAS_nil);
    Add(SPSC_PAS_nodefault);
    Add(SPSC_PAS_not);
    Add(SPSC_PAS_object);
    Add(SPSC_PAS_of);
    Add(SPSC_PAS_or);
    Add(SPSC_PAS_out);
    Add(SPSC_PAS_overload);
    Add(SPSC_PAS_override);
    Add(SPSC_PAS_packed);
    Add(SPSC_PAS_pascal);
    Add(SPSC_PAS_platform);
    Add(SPSC_PAS_private);
    Add(SPSC_PAS_procedure);
    Add(SPSC_PAS_program);
    Add(SPSC_PAS_property);
    Add(SPSC_PAS_protected);
    Add(SPSC_PAS_public);
    Add(SPSC_PAS_published);
    Add(SPSC_PAS_raise);
    Add(SPSC_PAS_read);
    Add(SPSC_PAS_readonly);
    Add(SPSC_PAS_record);
    Add(SPSC_PAS_register);
    Add(SPSC_PAS_reintroduce);
    Add(SPSC_PAS_repeat);
    Add(SPSC_PAS_resident);
    Add(SPSC_PAS_resourcestring);
    Add(SPSC_PAS_safecall);
    Add(SPSC_PAS_set);
    Add(SPSC_PAS_shl);
    Add(SPSC_PAS_shr);
    Add(SPSC_PAS_stdcall);
    Add(SPSC_PAS_stored);
    Add(SPSC_PAS_string);
    Add(SPSC_PAS_stringresource);
    Add(SPSC_PAS_then);
    Add(SPSC_PAS_threadvar);
    Add(SPSC_PAS_to);
    Add(SPSC_PAS_try);
    Add(SPSC_PAS_type);
    Add(SPSC_PAS_unit);
    Add(SPSC_PAS_until);
    Add(SPSC_PAS_uses);
    Add(SPSC_PAS_var);
    Add(SPSC_PAS_virtual);
    Add(SPSC_PAS_while);
    Add(SPSC_PAS_with);
    Add(SPSC_PAS_write);
    Add(SPSC_PAS_writeonly);
    Add(SPSC_PAS_xor);
  end;
end;

{-------------------------------------------------------------}

procedure PSCEnumDelphiResWords_NeverUse(const AStrings:IPSCStrings);
begin
  With AStrings do
  begin
    Add(SPSC_PAS_until);
    Add(SPSC_PAS_stringresource);
    Add(SPSC_PAS_absolute);
    Add(SPSC_PAS_asm);
    Add(SPSC_PAS_assembler);
    Add(SPSC_PAS_export);
    Add(SPSC_PAS_far);
    Add(SPSC_PAS_near);
    Add(SPSC_PAS_default);
    Add(SPSC_PAS_nodefault);
    Add(SPSC_PAS_automated);
    Add(SPSC_PAS_file);
    Add(SPSC_PAS_finalization);
    Add(SPSC_PAS_dynamic);
    Add(SPSC_PAS_goto);
    Add(SPSC_PAS_pascal);
    Add(SPSC_PAS_resident);
    Add(SPSC_PAS_initialization);
    Add(SPSC_PAS_label);
    Add(SPSC_PAS_repeat);
    Add(SPSC_PAS_platform);
    Add(SPSC_PAS_as);
    Add(SPSC_PAS_inline);
    Add(SPSC_PAS_is);
    Add(SPSC_PAS_safecall);
  end;
end;

{------------------------------------------------------------------}

procedure PSCPerformForStrings(const Strings:IPSCStrings;Proc:TPSCStrConvertProc);
var
  i:Integer;
begin
  for i:=0 to Strings.Count-1 do
    Strings[i]:=Proc(Strings[i]);
end;

{-------------------------------------------------------------}

function PSCFileNameHasWildCards(const FileName:String):boolean;
begin
  Result:=(Pos('*',FileName)>0) or (Pos('?',FileName)>0);
end;

{--------------------------------------}

procedure PSCIntersectStrings(const Source, Dest : IPSCStrings);
var
  i : integer;
begin
  for i := Dest.Count - 1 downto 0 do
   if Source.IndexOf(Dest[i]) < 0
    then Dest.Delete(i);
end;

{-------------------------------------------------------------------------}

function PSCRemoveExtraSlash(const FileName:String):String;
begin
  Result:=FileName;
  PSCReplaceAllOccur(Result,'\\','\');
end;

{--------------------------------------}

function PSCUnParseString(const Strings : IPSCStrings;const Separator : String):String;
begin
  Result:=PSCUnParseStringEx(Strings,Separator,True);
end;

{--------------------------------------}

function PSCUnParseStringEx(const Strings:IPSCStrings;const Separator:String;
  AddEndSeparator:boolean):String;
var
  i: Integer;
begin
  Result := '';
  if not Assigned(Strings)
   then Exit;

  for i:= (Strings.Count-1) downto 0 do
  begin
    If (Result='') and (not AddEndSeparator) then
      Result:=Strings[i]
    else
      Result:=Strings[i]+Separator+Result;
  end;
end;

{-------------------------------------------------------------------------}

procedure PSCRemoveEmptyStrings(const S:IPSCStrings);
var
  i:Integer;
begin
  With S do
    for i:=Count-1 downto 0 do
      if PSCTrim(Strings[i])='' then
        Delete(i);
end;

{--------------------------------------}

procedure PSCTrimSeparatorsInStrings(const Strings: IPSCStrings;
  const Separators: TPSCCharSet);
var
  i: Integer;
begin
  if not Assigned(Strings)
   then Exit;

  for i:=0 to Strings.Count-1 do
    Strings[i] := PSCTrimSeparators(Strings[i], Separators);
end;

{------------------------------------------------------------------}

Function PSCAreObjNotEqualNotNil(Obj1,Obj2: TObject): Boolean;
Begin
  Result := (Obj2 <> Obj1) And (Obj1 <> Nil) And (Obj2 <> Nil)
End;

{-------------------------------------------}

Procedure PSCRemoveStringsPart(const Strings: IPSCStrings; NamePart: boolean);
Begin
  PSCRemoveStringsPartEx(Strings, '=',NamePart);
End;

{------------------------------------------------------------------}

Procedure PSCRemoveStrings(const FromStrings,StringsToRemove: IPSCStrings);
Var
  i: Integer;
Begin
  With FromStrings Do
    For i := Count - 1 Downto 0 Do
      If StringsToRemove.IndexOf(Strings[i]) >= 0 Then
        Delete(i);
End;

{------------------------------------------------------------------}

Procedure PSCRemoveStringsPartEx(const Strings: IPSCStrings;
  Const Separator: String;NamePart: boolean);
Var
  i: Integer;
  Temp: String;
Begin
  If Strings = Nil Then
    exit;
  With Strings Do
    For i := 0 To Count - 1 Do
      Begin
        If NamePart Then
          Temp := PSCExtractValuePartEx(Strings[i],Separator)
        Else
          Temp := PSCExtractNamePartEx(Strings[i],Separator);
        If Temp <> '' Then
          Strings[i] := Temp;
      End;
End;

{-------------------------------------------------------------}

Procedure PSCStrArrayToStrings(const Strings: IPSCStrings; Const A: Array Of String);
Var
  i: Integer;
Begin
  With Strings Do
    Begin
      BeginUpdate;
      Clear;
      For i := Low(A) To High(A) Do
        Add(A[i]);
      EndUpdate;
    End;
End;

{-------------------------------------------------------------------------}

Function PSCFieldStringsToStr(const FieldList: IPSCStrings): String;
Var
  I: Integer;
Begin
  Result := '';
  For I := 0 To FieldList.Count - 1 Do
    Begin
      If Pos(' ',FieldList[I]) <> 0 Then
        Result := Result + ', [' + FieldList[I] + ']'
      Else
        Result := Result + ', ' + FieldList[I];
    End;
  Delete(Result,1,2);
End;

{------------------------------------------------------------------}

type
  TPSCEventHandler=class(TPSCInterfacedObject,IPSCEventHandler)
  private
    FProc:TPSCHandleEventProc;
  protected
    Procedure HandleEvent(const AParams:TPSCEventParams);
  public
    constructor Create(AProc:TPSCHandleEventProc);
  end;

{------------------------------------------------------------------}

constructor TPSCEventHandler.Create(AProc:TPSCHandleEventProc);
begin
  inherited Create;
  FProc:=AProc;
end;

{------------------------------------------------------------------}

function PSCCreateEventHandler(AProc:TPSCHandleEventProc):IPSCEventHandler;
begin
  Result:=TPSCEventHandler.Create(AProc);
end;

{--------------------------------------------}

function TPSCStringAndIDCompare.CompareObjects(AObject1,AObject2:TObject):Integer;
begin
  Result:=TPSCStringAndID(AObject1).ID-TPSCStringAndID(AObject2).ID;
end;

{--------------------------------------------}

Function PSCCheckedToCheckState(Checked: boolean): TPSCCheckBoxState;
Const
  ConstArray: Array[boolean] Of TPSCCheckBoxState =
    (CheckBox_Unchecked,CheckBox_Checked);
Begin
  Result := ConstArray[Checked];
End;

{-------------------------------------------}

Function PSCMaxInArray(Const A: Array Of Integer): Integer;
Var
  i: Integer;
Begin
  Result := A[Low(A)];
  For i := Low(A) + 1 To High(A) Do
    Result := PSCMax(Result,A[i]);
End;

{------------------------------------------------------------}

Function PSCAddSlash(Const Path: String): String;
Begin
  Result := PSCAddChar(Path, '\');
End;

{----------------------------------------------------------}

Function PSCAddChar(Const AStr: String; AChar: Char): String;
Begin
  Result := AStr;
  If (Length(Result) > 0) And (Result[Length(Result)] <> AChar) Then
    Result := Result + AChar;
End;

{-------------------------------------------}

Function PSCIncWeek(Const Date: TDateTime; NumberOfWeeks: Integer): TDateTime;
Begin
  Result := Date + NumberOfWeeks * 7;
End;

{-------------------------------------------}

Function PSCStrInCharSet(Const S: String {AnsiString}; Const CharSet: TPSCCharSet):
  boolean;
Var
  i: Integer;
Begin
  Result := True;
  If Length(S) = 0 Then
    exit;
  For i := 1 To Length(S) Do
    // If Not (S[i] in CharSet) Then
    If Not CharInSet(S[i], CharSet) Then
      Begin
        Result := False;
        exit;
      End;
End;

{------------------------------------------------------------------}

Procedure PSCExtractPickListValues(Const PickListStr: String; Var
  DisplayValue,Value: String);
Begin
  If Pos('=',PickListStr) > 0 Then
    Begin
      Value := PSCExtractValuePart(PickListStr);
      DisplayValue := PSCExtractNamePart(PickListStr);
    End
  Else
    Begin
      Value := PickListStr;
      DisplayValue := '';
    End;
End;

{------------------------------------------------------------------}

Function PSCListsInterSect(const l1,l2: IPSCObjectList): boolean;
Var
  i: Integer;
Begin
  result := true;

  For i := 0 To l1.Count - 1 Do
    If l2.IndexOf(l1[i]) >= 0 Then
      exit;

  result := false;
End;

{--------------------------------------}

function PSCGetOpenDlgFilters(const Categories,Extensions:Array of String):String;
var
  i:Integer;
begin
  Result:='';
  for i:=Low(Categories) to High(Categories) do
  begin
    If Result<>'' then
      Result:=Result+'|';
    Result:=Result+PSCGetOpenDlgFilter(Categories[i],Extensions[i]);
  end;
end;

{--------------------------------------------------}

Function PSCExtractValuePart(Const S: String): String;
Begin
  Result := PSCExtractValuePartEx(S, '=');
End;

{-------------------------------------------}

Function PSCTrimStringLeft(Const S,PosStr: String): String;
Begin
  Result := S;
  If (PosStr = '') Or (Result = '') Then
    exit;
  While Pos(PosStr,Result) = 1 Do
    Result := Copy(Result,Length(PosStr) + 1,MaxInt);
End;

{-------------------------------------------}

Function PSCTrimStringRight(Const S,PosStr: String): String;
Var
  a: Integer;
Begin
  Result := S;
  If (PosStr = '') Or (Result = '') Then
    exit;
  While True Do
    Begin
      A := Length(Result) - Length(PosStr) + 1;
      If PSCBackPos(PosStr,Result) = A Then
        Result := Copy(Result,1,A - 1)
      Else
        break;
    End;
End;

{-------------------------------------------}

Function PSCAddCRLF(Const S: String {AnsiString}): String; //AnsiString;
Var
  L: Integer;

  Function ValidChar(i: Integer): boolean;
  Begin
    //Result := S[i] In [#13,#10];
    Result := CharInSet(S[i],[#13,#10]);
  End;

Begin
  Result := S;
  L := Length(Result);
  If (L < 2) Or
    (Not ValidChar(L)) Or
    (Not ValidChar(L - 1)) Then
    Result := Result + #13#10;
End;

{------------------------------------------------------------------}

Function PSCRemoveColons(Const S: String): String;
Begin
  Result := PSCRemoveCharSet([':'],S);
End;

{------------------------------------------------------------------}

Function PSCRemoveSpChars(Const S: String): String;
Begin
  Result := PSCRemoveCharSet(['~'],S);
End;

{-------------------------------------------}

Function PSCGetOpenDlgFilter(Const FileCat,FileExt: String): String;
Begin
  Result := FileCat + ' (*.' + FileExt + ')|*.' + FileExt; //don't resource
  PSCReplaceAllOccur(Result, '..', '.');
End;

{-------------------------------------------}

Function PSCBackPos(Const SubStr,S: String): Integer;
Begin
  Result := PSCBackPosEx(SubStr,S,Length(S));
End;

{-----------------------------------------------------------}

Function PSCStringOfSpace(Num: Integer): String;
Begin
  result := StringOfChar(' ',Num);
End;

{------------------------------------------------------------}

Function PSCRemoveSlash(Const Path: String): String;
Var
  rlen: integer;
Begin
  result := Path;
  rlen := length(result);
  If (rlen > 0) And (result[rlen] = '\') Then
    Delete(result,rlen,1);
End;

{------------------------------------------------------------}

Function PSCExtractNamePart(Const S: String): String;
Begin
  Result := PSCExtractNamePartEx(S, '='); //don't resource
End;

{-------------------------------------------}

Procedure PSCEnsureBound(Var EnsureLeft,EnsureRight: Integer;
  BaseLeft,BaseRight: Integer);
Var
  Width: Integer;
Begin
  Width := PSCMin(EnsureRight - EnsureLeft,BaseRight - BaseLeft);
  If EnsureLeft < BaseLeft Then
    EnsureLeft := BaseLeft;

  If EnsureLeft + Width > BaseRight Then
    EnsureLeft := BaseRight - Width;

  EnsureRight := EnsureLeft + Width;
End;

{-------------------------------------}

function PSCRandomString(ASize: Integer) : String;
var
  MyIndex: Integer;
begin
  Result:=Chr(Random(26)+65);
  for MyIndex:=0 to Random(ASize) do
    Result:=Result+Chr(Random(26)+97);
end;

{--------------------------------------------}

function PSCCharSetToString(CharSet:TPSCCharSet):AnsiString;
var
  A:AnsiChar;
begin
  Result:='';
  for A:=#0 to #255 do
    if A in CharSet then
      Result:=Result+A;
end;

{--------------------------------------}

function PSCRemoveExtraChars(const S: string; C: Char): string;
begin
  Result := S;
  PSCReplaceAllOccur(Result, C + C, C);
end;

{-------------------------------------}

Function PSCApplyLimits(Const AMinValue,AMaxValue: integer;
  Var AValue: integer): boolean;
Begin
  result := false;
  If AValue > AMaxValue Then
    Begin
      AValue := AMinValue;
      result := true;
    End;
  If AValue < AMinValue Then
    Begin
      AValue := AMaxValue;
      result := true;
    End;
End;

{-------------------------------------------}

Function PSCStrIsInArray(const AValue: String;
  Const A: Array Of String): boolean;
Var
  i: Integer;
Begin
  For i := Low(A) To High(A) Do
    If AValue = A[i] Then
      Begin
        Result := True;
        exit;
      End;
  Result := False;
End;

{-------------------------------------------}

Function PSCIntIsInArray(Value: Integer; Const A: Array Of Integer): boolean;
Var
  i: Integer;
Begin
  For i := Low(A) To High(A) Do
    If Value = A[i] Then
      Begin
        Result := True;
        exit;
      End;
  Result := False;
End;

{------------------------------------------------------------}

Function PSCReplaceAllOccur(Var S: String; Const FromStr,ToStr: String):Integer;
Var
  PosChar: Integer;
Begin
  PosChar := 1;
  Result := 0;
  Repeat
    PosChar := PSCPosEx(FromStr,S,PosChar);
    If PosChar = 0 Then
      break;
    PSCReplace(S,PosChar,FromStr,ToStr);
    inc(Result);
    Inc(PosChar,Length(ToStr));
  Until false;
End;

{----------------------------------------------------------}

function PSCAddCharBefore(const AStr: string; AChar: Char): string;
begin
  Result := AStr;
  if (Length(Result) > 0) and (Result[1] <> AChar) then
    Result := AChar + Result;
end;

{------------------------------------------------------------------}

Function PSCBackPosEx(Const SubStr,S: String; FromChar: Integer): Integer;
var
  MySubLen:Integer;
  A:Integer;
begin
  MySubLen:=Length(SubStr);

  If (MySubLen=0) or (S='') then
  begin
    Result:=0;
    exit;
  end;

  FromChar:=PSCMin(FromChar,Length(S)-MySubLen+1);
  While FromChar>0 do
  begin
    If S[FromChar]=SubStr[1] then
    begin
      A := 1;
      while (A < MySubLen) and (S[FromChar + A] = SubStr[A + 1]) do
        Inc(A);
      if A = MySubLen then
      begin
        Result:=FromChar;
        exit;
      end;
    end;
    dec(FromChar);
  end;
  Result:=0;
end;

{------------------------------------------------------------------}

Function PSCPosEx(Const SubStr,S: String; FromChar: Integer=1): Integer;
var
  MyLen,MySubLen:Integer;
  A:Integer;
begin
  MySubLen:=Length(SubStr);

  If (MySubLen=0) or (S='') or (FromChar<1) then
  begin
    Result:=0;
    exit;
  end;

  MyLen:=Length(S)-MySubLen+1;
  While FromChar<=MyLen do
  begin
    If S[FromChar]=SubStr[1] then
    begin
      A := 1;
      while (A < MySubLen) and (S[FromChar + A] = SubStr[A + 1]) do
        Inc(A);
      if A = MySubLen then
      begin
        Result:=FromChar;
        exit;
      end;
    end;
    inc(FromChar);
  end;
  Result := 0;
end;

{-------------------------------------------}

procedure PSCSeparateStr(const S:String;var NamePart,ValuePart:String);
begin
  PSCSeparateStrEx(S,'=',NamePart,ValuePart);
end;

{-------------------------------------------}

procedure PSCSeparateStrEx(S:String;
  const Separator:String;var NamePart,ValuePart:String);
begin
  NamePart:=PSCExtractNamePartEx(S,Separator);
  ValuePart:=PSCExtractValuePartEx(S,Separator);
  If NamePart='' then
    NamePart:=S;
end;

{------------------------------------------------------------}

Procedure PSCReplace(Var s: String; index: integer; Const OldStr,NewStr:
  String);
Begin
  Delete(s,index,length(OldStr));
  Insert(NewStr,s,index);
End;

{-----------------------------------------------------------}

function PSCMin(const A,B:TDateTime):TDateTime;
begin
  If A<B
  then
    Result:=A
  else
    Result:=B;

end;

{-----------------------------------------------------------}

Function PSCMin(A,B: Integer): Integer;
Begin
  If A < B Then
    Result := A
  Else
    Result := B;

End;

{-----------------------------------------------------------}

function PSCMax(const A,B:TDateTime):TDateTime;
begin
  If A>B
  then
    Result:=A
  else
    Result:=B;
end;

{-----------------------------------------------------------}

Function PSCMax(A,B: Integer): Integer;
Begin
  If A > B Then
    Result := A
  Else
    Result := B;
End;

{--------------------------------------------------}

Function PSCExtractNamePartEx(Const S,Separator: String): String;
Var
  P: Integer;
Begin
  P := Pos(Separator,S);
  If P > 0 Then
    Result := Copy(S,1,P - 1)
  Else
    Result := '';
End;

{--------------------------------------------------}

Function PSCExtractValuePartEx(Const S,Separator: String): String;
Var
  P: Integer;
Begin
  P := Pos(Separator,S);
  If P > 0 Then
    Result := Copy(S,P + Length(Separator),MaxInt)
  Else
    Result := '';
End;

{-------------------------------}

function PSCGetPasswordChar:AnsiChar;
const
  CValidPaswordChars=['A'..'Z','a'..'z','1'..'9','0']-['o','O','0'];
begin
  While True do
  begin
    Result:=AnsiChar(Random(256));
    If Result in CValidPaswordChars then
      exit;
  end;
end;

{-------------------------------------------}

function PSCStrArrayToStr(const StrArray:Array of String;
  const Separator:String):String;
var
  i:Integer;
begin
  Result:='';
  for i := Low(StrArray) to High(StrArray) do
   if StrArray[i] <> '' then
     if result = '' then
       result := StrArray[i]
     else
       result := result + Separator + StrArray[i];
end;

{--------------------------------------}

function PSCRemoveSpaces(const Str: string): string;
var
  SpacePos:Integer;
begin
  Result := Str;
  repeat
    SpacePos := Pos(' ', Result);
    if SpacePos = 0 then
      Break;
    Delete(Result, SpacePos, 1);
  until False;
end;

{-------------------------------------------}

function PSCTrimNonTextChars(const S:String):String;
begin
  Result:=PSCTrimSeparators(S,[#0..#32]);
end;

{------------------------------------------------------------}

Function PSCTrim(Const S: String): String;
begin
  Result:=PSCTrimSeparators(S,[' ']);
end;

{------------------------------------------------------------}

Function PSCTrimSeparatorsLeft(Const S: String; Separators: TPSCCharSet):
  String;
Var
  I,L: Integer;
Begin
  L := Length(S);
  I := 1;
  While (I <= L) And CharInSet(S[I],Separators) Do
    Inc(I);
  Result := Copy(S,I,Maxint);
End;

{------------------------------------------------------------}

Function PSCTrimSeparatorsRight(Const S: String; Separators: TPSCCharSet):
  String;
Var
  I: Integer;
Begin
  I := Length(S);
  While (I > 0) And CharInSet(S[I],Separators) Do
    Dec(I);
  Result := Copy(S,1,I);
End;

{------------------------------------------------------------}

Function PSCTrimSeparators(Const S: String; Separators: TPSCCharSet): String;
Begin
  Result :=
    PSCTrimSeparatorsRight(PSCTrimSeparatorsLeft(S,Separators),Separators);
End;

{-------------------------------------------}

function PSCSetStrCharsTo(const S:String;Index,Count:Integer;C:Char):String;
begin
  Result:=S;
  Delete(Result,Index,Count);
  Insert(StringOfChar(c,Count),Result,Index);
end;

{------------------------------------------------------------------------------}

Function PSCDatesCompare(Const A,B: TDateTime;AKind:TPSCDateTimeKind): Integer;
Var
  Delta: TDateTime;
Begin
  Case AKind of
    cpkDateTime:
      Delta := A - B;
    cpkDate:
      Delta := PSCDateOf(A) - PSCDateOf(B);
    else
      Delta := PSCTimeOf(A) - PSCTimeOf(B);
  end;
  If Delta > 0 Then
    Result := 1
  Else
    If Delta < 0 Then
      Result := -1
    Else
      Result := 0;
End;

{-----------------------------------------------------------}

Function PSCDateOf(Const ADateTime: TDateTime): TDateTime;
Begin
  Result := Trunc(ADateTime + 1E-11);
End;

{-----------------------------------------------------------}

Function PSCTimeOf(Const ADateTime: TDateTime): TDateTime;
Begin
  Result := Frac(ADateTime);
End;

{-------------------------------------------------------------}

Function PSCParseLikeCharSetDef(Const inputS: String): TPSCCharSet;
Var
  i: Integer;
  j: AnsiChar;
  S: AnsiString;
Begin
  Result := [];
  S := AnsiString(inputS);
  i := 1;
  While I <= Length(S) Do
    Begin
      Include(Result,S[i]);
      inc(i);
      If i > Length(s) Then
        break;
      If S[i] = '-' Then
        Begin
          inc(i);
          If i > Length(s) Then
            break;
          For j := S[i - 2] To S[i] Do
            Include(Result,j);
        End;
    End;
End;

{------------------------------------------------------------------------------}

function PSCGetArrayGrowDelta(ACapacity:Integer):Integer;
begin
  if ACapacity > 64 then
    Result := ACapacity div 4
  else
    if ACapacity > 8 then
      Result := 16
    else
      Result := 4;
end;

{--------------------------------------}

type
  TPSCObjectList=class(TInterfacedObject,IPSCObjectList)
  private
    FOwnerShip:TPSCItemOwnership;
    FItems:Array of TObject;
    FCount:Integer;
    FCriteria:IPSCCompareObjects;
    FDuplicates:TPSCDuplicates;
  protected
    function Add(AObject:TObject):Integer;overload;
    function GetItem(AIndex:Integer):TObject;overload;
    function IndexOf(AObject:TObject):Integer;
    function GetCount:Integer;
    function GetDuplicates:TPSCDuplicates;
    Function InternalFind(AItem: TObject; Var Index: Integer;
      const ACriteria:IPSCCompareObjects): Boolean;
    function Find(AObject:TObject; var Index: Integer): Boolean;
    function GetCapacity:Integer;
    function GetSorted:Boolean;

    procedure InternalMove(AFromIndex,AToIndex,ANumberOfItems:Integer);
    procedure SetCapacity(V:Integer);
    procedure SetItem(AIndex:Integer;AObject:TObject);
    procedure InternalInsert(AIndex:Integer;AObject:TObject);
    procedure QuickSort(L, R: Integer;const ACriteria:IPSCCompareObjects);
    procedure Delete(AIndex:Integer);
    procedure Remove(AObject:TObject);
    procedure SetCount(V:Integer);
    procedure Clear;
    procedure SetDuplicates(V:TPSCDuplicates);
    procedure Sort(const ACriteria:IPSCCompareObjects);
    procedure Insert(AIndex: Integer; AItem: TObject);
    procedure SetSortCriteria(const V:IPSCCompareObjects);
  public
    constructor Create(AOwnerShip:TPSCItemOwnership);
    destructor Destroy;override;
  end;

  TPSCInterfaceListItem=class
  public
    AnInterface:IPSCInterface;
  end;

  TPSCInterfaceList=class(TPSCObjectList,IPSCInterfaceList)
  private
  protected
    function Add(const AItem:IPSCInterface):Integer;overload;
    function GetItem(AIndex:Integer):IPSCInterface;overload;
  public
    constructor Create;
  end;

{--------------------------------------}

function TPSCInterfaceList.Add(const AItem:IPSCInterface):Integer;
var
  MyItem:TPSCInterfaceListItem;
begin
  MyItem:=TPSCInterfaceListItem.Create;
  MyItem.AnInterface:=AItem;
  Result:=inherited Add(MyItem);
end;

{--------------------------------------}

function TPSCInterfaceList.GetItem(AIndex:Integer):IPSCInterface;
begin
  Result:=TPSCInterfaceListItem(inherited GetItem(AIndex)).AnInterface;
end;

{--------------------------------------}

constructor TPSCInterfaceList.Create;
begin
  inherited Create(ioOwned);
end;

{--------------------------------------}

function PSCCreateInterfaceList:IPSCInterfaceList;
begin
  Result:=TPSCInterfaceList.Create;
end;

{--------------------------------------}

function TPSCObjectList.GetDuplicates:TPSCDuplicates;
begin
  Result:=FDuplicates;
end;

{--------------------------------------}

procedure TPSCObjectList.SetDuplicates(V:TPSCDuplicates);
begin
  If FDuplicates<>V then
  begin
    FDuplicates:=V;    
  end;
end;

{--------------------------------------}

procedure TPSCObjectList.SetSortCriteria(const V:IPSCCompareObjects);
begin
  If FCriteria<>V then
  begin
    FCriteria:=V;
    Sort(FCriteria);
  end;
end;

{--------------------------------------}

procedure TPSCObjectList.QuickSort(L, R: Integer;
  const ACriteria:IPSCCompareObjects);
var
  I, J: Integer;
  P, T: TObject;
begin
  repeat
    I := L;
    J := R;
    P := FItems[(L + R) shr 1];
    repeat
      while ACriteria.CompareObjects(FItems[I], P) < 0 do
        Inc(I);
      while ACriteria.CompareObjects(FItems[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := FItems[I];
        FItems[I] := FItems[J];
        FItems[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, ACriteria);
    L := I;
  until I >= R;
end;

{--------------------------------------}

procedure TPSCObjectList.Sort(const ACriteria:IPSCCompareObjects);
begin
  If (ACriteria<>nil) and (GetCount>0) then
    QuickSort(0, GetCount - 1, ACriteria);
end;

{--------------------------------------}

function TPSCObjectList.GetCount:Integer;
begin
  Result:=FCount;
end;

{--------------------------------------}

function TPSCObjectList.GetItem(AIndex:Integer):TObject;
begin
  If (AIndex>=FCount) or (AIndex<0) then
    Result:=nil
  else
    Result:=FItems[AIndex];
end;

{--------------------------------------}

procedure TPSCObjectList.Remove(AObject:TObject);
var
  MyIndex:Integer;
begin
  MyIndex:=IndexOf(AObject);
  If MyIndex>=0 then
    Delete(MyIndex);
end;

{--------------------------------------}

Function TPSCObjectList.InternalFind(AItem: TObject; Var Index: Integer;
  const ACriteria:IPSCCompareObjects): Boolean;
Var
  L,H,I,C: Integer;
Begin
  Result := False;
  L := 0;
  H := FCount - 1;
  While L <= H Do
    Begin
      I := (L + H) Shr 1;
      C := ACriteria.CompareObjects(FItems[I],AItem);
      If C < 0 Then
        L := I + 1
      Else
        Begin
          H := I - 1;
          If C = 0 Then
            Begin
              Result := True;
              If FDuplicates <> DUP_Accept Then
                L := I;
            End;
        End;
    End;
  Index := L;
End;

{--------------------------------------}

function TPSCObjectList.Find(AObject:TObject; var Index: Integer): Boolean;
begin
  Index:=IndexOf(AObject);
  Result:=Index>=0;
end;

{--------------------------------------}

function TPSCObjectList.GetSorted:Boolean;
begin
  Result:=FCriteria<>nil;
end;

{--------------------------------------}

function TPSCObjectList.IndexOf(AObject:TObject):Integer;
var
  i:Integer;
begin
  If FCriteria=nil then
    begin
      for i:=FCount-1 downto Low(FItems) do
        if FItems[i]=AObject then
        begin
          Result:=i;
          exit;
        end;
      Result:=-1;
    end
  else
    If not InternalFind(AObject,Result,FCriteria) then
      Result:=-1;
end;

{--------------------------------------}

function TPSCObjectList.GetCapacity:Integer;
begin
  Result:=Length(FItems);
end;

{--------------------------------------}

procedure TPSCObjectList.SetCapacity(V:Integer);
begin
  SetLength(FItems,V);
end;

{--------------------------------------}

procedure TPSCObjectList.Insert(AIndex: Integer; AItem: TObject);
begin
  If (AIndex<=FCount) and (AIndex>=0) then
    InternalInsert(AIndex,AItem);
end;

{--------------------------------------}

procedure TPSCObjectList.SetItem(AIndex:Integer;AObject:TObject);
begin
  If (AIndex<FCount) and (AIndex>=0) then
  begin
    Delete(AIndex);
    If FCriteria=nil then
      InternalInsert(AIndex,AObject)
    else
      Add(AObject);
  end;
end;

{--------------------------------------}

function TPSCObjectList.Add(AObject:TObject):Integer;
begin
  If FCriteria=nil then
    Result:=FCount
  else
    If InternalFind(AObject,Result,FCriteria) Then
      Case FDuplicates Of
        Dup_Ignore:
          begin
            If FOwnerShip=ioOwned then
              AObject.Free;
            Exit;
          end;
      End;
  InternalInsert(Result,AObject);
end;

{--------------------------------------}

procedure TPSCObjectList.Clear;
begin
  While GetCount>0 do Delete(GetCount-1);
end;

{--------------------------------------}

destructor TPSCObjectList.Destroy;
begin
  If FOwnerShip=ioOwned then
    Clear;
  inherited;
end;

{--------------------------------------}

procedure TPSCObjectList.SetCount(V:Integer);
begin
  If (V=GetCount) or (V<0) then
    exit;
  While V<GetCount do
    Delete(GetCount-1);
  While V>GetCount do
    Add(nil);
end;

{--------------------------------------}

constructor TPSCObjectList.Create(AOwnerShip:TPSCItemOwnership);
begin
  inherited Create;
  FOwnerShip:=AOwnerShip;
end;

{--------------------------------------}

function PSCCreateObjectList(AOwnerShip:TPSCItemOwnership=ioReferenced):IPSCObjectList;
begin
  Result:=TPSCObjectList.Create(AOwnerShip);
end;

{------------------------------------------------------------------}

constructor TPSCValuesContainer.Create(AIntValue:Integer);
begin
  inherited Create;
  IntValue:=AIntValue;
end;

{------------------------------}

var
  FInstanceID:Integer=1;

function PSCGetNewInstanceID:Integer;
begin
  Result:=FInstanceID;
  inc(FInstanceID);
end;

{------------------------------------------------------------------}

Procedure TPSCEventHandler.HandleEvent(const AParams:TPSCEventParams);
begin
  If Assigned(FProc) then
    FProc(AParams);
end;

{------------------------------------------------------------------}

type
  TPSCEventData=class
  public
    FEvent:IPSCEventHandler;
  end;

  TPSCEventDataCompare=class(TInterfacedObject,IPSCCompareObjects)
  public
    function CompareObjects(AObject1,AObject2:TObject):Integer;
  end;

{------------------------------------------------------------------}

function TPSCEventDataCompare.CompareObjects(AObject1,AObject2:TObject):Integer;
begin
  Result:=TPSCEventData(AObject1).FEvent.GetInstanceID-
    TPSCEventData(AObject2).FEvent.GetInstanceID;
end;

{------------------------------------------------------------------}

procedure TPSCEvents.RegisterHandler(const AEventHandler:IPSCEventHandler);
var
  MyData:TPSCEventData;
begin
  If AEventHandler=nil then
    exit;
  MyData:=TPSCEventData.Create;
  MyData.FEvent:=AEventHandler;
  FEventList.Add(MyData);
end;

{------------------------------------------------------------------}

procedure TPSCEvents.UnregisterHandler(const AEventHandler:IPSCEventHandler);
var
  MyData:TPSCEventData;
begin
  If AEventHandler=nil then
    exit;
  MyData:=TPSCEventData.Create;
  MyData.FEvent:=AEventHandler;
  FEventList.Remove(MyData);
  MyData.Free;
end;

{------------------------------------------------------------------}

Procedure TPSCEvents.HandleSimpleEvent(AEventType:TPSCEventType);
var
  MyParams:TPSCEventParams;
begin
  MyParams.ASender:=Self;
  MyParams.AEventType:=AEventType;
  HandleEvent(MyParams);
end;

{------------------------------------------------------------------}

Procedure TPSCEvents.HandleEvent(const AParams:TPSCEventParams);
var
  i:Integer;
begin
  for i:=FEventList.Count-1 downto 0 do
    TPSCEventData(FEventList.Items[i]).FEvent.HandleEvent(AParams);
end;

{------------------------------------------------------------------}

constructor TPSCEvents.Create;
begin
  inherited;
  FEventList:=PSCCreateObjectList(ioOwned);
  FEventList.Duplicates:=dup_Ignore;
  FEventList.SetSortCriteria(TPSCEventDataCompare.Create);
end;

{------------------------------------------------------------------}

constructor TPSCInterfacedObject.Create;
begin
  inherited;
  FInstanceID:=PSCGetNewInstanceID;
end;

{------------------------------------------------------------------}

function TPSCInterfacedObject.GetInstanceId:Integer;
begin
  Result:=FInstanceID;
end;

{------------------------------------------------------------------}

Procedure PSCStringsToProc(const S: IPSCStrings; Proc: TPSCGetStrProc);
Var
  i: integer;
Begin
  If Assigned(Proc) Then
    For i := 0 To S.Count - 1 Do
      Proc(S[i]);
End;

{--------------------------------------------}

function PSCStringToCharSet(InitValue:TPSCCharSet;const S:AnsiString;
  CaseSens:Boolean):TPSCCharSet;

  procedure Work(Const TempStr:AnsiString);
  var
    i:Integer;
  begin
    For i:=1 to Length(TempStr) do
      Include(Result,TempStr[i]);
  end;

begin
  Result:=InitValue;
  If CaseSens
  then
    Work(S)
  else
    begin
      Work(system.AnsiStrings.AnsiUpperCase(S));  //Work(PSCUpperCase(S));
      Work(system.AnsiStrings.AnsiLowerCase(S));  //Work(PSCLowerCase(S));
    end;
end;

{-------------------------------------------}

function PSCBackPosIgnoreCase(const SubStr,S:String):Integer;
begin
  Result:=PSCBackPos(PSCUpperCase(SubStr),PSCUpperCase(S));
end;

{--------------------------------------}

function PSCGetStrBetweenTagsEx(const Source: string; SearchFromPos: Integer;
  const TagBegin, TagEnd: string; var PosBegin, PosEnd: Integer): string;
var
  TmpStr: string;
begin
  PosEnd := 0;
  TmpStr := PSCUpperCase(Source);
  PosBegin := PSCPosEx(PSCUpperCase(TagBegin), TmpStr, SearchFromPos);
  if (PosBegin < 1) then
    Exit;
  Inc(PosBegin, Length(TagBegin));
  PosEnd := PSCPosEx(PSCUpperCase(TagEnd), TmpStr, PosBegin);
  if PosBegin >= PosEnd then
    Exit;
  Result := Copy(Source, PosBegin, PosEnd - PosBegin);
end;

{--------------------------------------}

procedure TPSCObjectList.InternalInsert(AIndex:Integer;AObject:TObject);
var
  MyLength:Integer;
begin
  MyLength := GetCapacity;
  If MyLength<=FCount then
    SetCapacity(PSCGetArrayGrowDelta(FCount) + MyLength);
  if AIndex < FCount then
    InternalMove(AIndex, AIndex + 1,FCount - AIndex);
  FItems[AIndex]:=AObject;
  inc(FCount);
end;

{--------------------------------------}

procedure TPSCObjectList.Delete(AIndex:Integer);
var
  MyItem:TObject;
begin
  If (AIndex<FCount) and (AIndex>=0) then
  begin
    MyItem:=FItems[AIndex];
    dec(FCount);
    If AIndex<FCount then
      InternalMove(AIndex+1, AIndex,FCount - AIndex);
    If FOwnerShip=ioOwned then
      MyItem.Free;
  end;
end;

{-------------------------------------------}

Function PSCGetThisMon1: TDateTime;
Begin
  Result := PSCGetMonthStart(PSCGetToday);
End;

{-------------------------------------------}

Function PSCGetThisMon31: TDateTime;
Begin
  Result := PSCGetMonthEnd(PSCGetToday);
End;

{-------------------------------------------}

Function PSCGetLastMon1: TDateTime;
Begin
  Result := PSCGetMonthStart(PSCIncMonth(PSCGetToday, -1));
End;

{-------------------------------------------}

Function PSCGetLastMon31: TDateTime;
Begin
  Result := PSCGetMonthEnd(PSCIncMonth(PSCGetToday, -1));
End;

{-------------------------------------------}

Function PSCGetNextMon1: TDateTime;
Begin
  Result := PSCGetMonthStart(PSCIncMonth(PSCGetToday,1));
End;

{-------------------------------------------}

Function PSCGetNextMon31: TDateTime;
Begin
  Result := PSCGetMonthEnd(PSCIncMonth(PSCGetToday,1));
End;

{--------------------------------------------}

Function PSCColorToHTString(C: TPSCColor;const ATagName:String='c'): String;
Begin
  Result := PSCColorToString(C);
  If Result[1] <> '$' Then
    Result := Copy(Result,3,MaxInt);
  Result := '<'+ATagName+':' + Result + '>'; //don't resource
End;

{-------------------------------------------}

function PSCChangeDateElement(const ADate:TDateTime;
  AIndex,ANewValue:Integer):TDateTime;
var
  AYear, AMonth, ADay: Word;
begin
  PSCDecodeDate(ADate, AYear, AMonth, ADay);
  case AIndex of
    1: AYear:=ANewValue;
    2: AMonth:=ANewValue;
    3: ADay:=ANewValue;
  end;
  Result:=PSCTimeOf(ADate)+PSCEncodeDate(AYear, AMonth, ADay);
end;

{-------------------------------------------}

function PSCGetDateElement(const ADate: TDateTime; Index: Integer): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  PSCDecodeDate(ADate, AYear, AMonth, ADay);
  case Index of
    CPSCElementYear: Result := AYear;
    CPSCElementMonth: Result := AMonth;
    CPSCElementDay: Result := ADay;
    else Result := -1;
  end;
end;

{-------------------------------------------}

function PSCChangeTimeElement(const ATime:TDateTime;
  AIndex,ANewValue:Integer):TDateTime;
Var
  MyHour,MyMin,MySec,MyMSec: Word;
Begin
  PSCDecodeTime(ATime,MyHour,MyMin,MySec,MyMSec);
  Case AIndex Of
    CPSCElementHour:
      MyHour:=ANewValue;
    CPSCElementMinute:
      MyMin:=ANewValue;
    CPSCElementSecond:
      MySec:=ANewValue;
    CPSCElementMSec:
      MyMSec:=ANewValue;
  End;
  Result:=PSCCombineDateTime(ATime,PSCEncodeTime(MyHour,MyMin,MySec,MyMSec));
End;

{-------------------------------------------------------------------------}

Function PSCGetTimeElement(Const ATime: TDateTime; Index: Integer): Integer;
Var
  Hour,Min,Sec,MSec: Word;
Begin
  PSCDecodeTime(ATime,Hour,Min,Sec,MSec);
  Case Index Of
    CPSCElementHour:
      Result := Hour;
    CPSCElementMinute:
      Result := Min;
    CPSCElementSecond:
      Result := Sec;
    CPSCElementMSec:
      Result := MSec;
  Else
    Result := -1;
  End;
End;

{--------------------------------------}

procedure PSCInflateRect(var ARect: TPSCRect; X, Y: Integer);
begin
  with ARect do
  begin
    Dec(Top, Y);
    Inc(Bottom, Y);
    Dec(Left, X);
    Inc(Right, X);
  end;
end;

{--------------------------------------}

procedure PSCOffsetRect(var ARect: TPSCRect; X, Y: Integer);
begin
  with ARect do
  begin
    Inc(Left, X);
    Inc(Top, Y);
    Inc(Right, X);
    Inc(Bottom, Y);
  end;
end;

{------------------------------------------------------------------}

procedure PSCDelete(var S: string; Index, Count:Integer);
var
  L:Integer;
begin
  L:=Length(S);
  If (Index>0) and (Index<=L) then
    S:=Copy(S,1,Index-1)+Copy(S,Index+Count,MaxInt);
end;

{------------------------------------------------------------------}

Function PSCLeaveCharSet(Const CharSet: TPSCCharSet; Const S: String):
  String;
Var
  i: Integer;
Begin
  Result := S;
  For i := Length(Result) Downto 1 Do
    If Not CharInSet(Result[i],CharSet) Then
      PSCDelete(Result,i,1);
End;

{------------------------------------------------------------------}

Function PSCRemoveCharSet(Const CharSet: TPSCCharSet; Const S: String):
  String;
Var
  i: Integer;
Begin
  Result := S;
  For I := Length(Result) Downto 1 Do
//    If Result[I] In CharSet Then
    If CharInSet(Result[I],CharSet) Then
      PSCDelete(Result,I,1);
End;

{--------------------------------------}

procedure TPSCObjectList.InternalMove(AFromIndex,AToIndex,ANumberOfItems:Integer);
begin
  System.Move(FItems[AFromIndex], FItems[AToIndex],
    ANumberOfItems*SizeOf(TObject));
end;

{--------------------------------------}

type
  TPSCCustomField = class(TPSCEvents,IPSCDataField)
  private
    FTypeConvert:IPSCTypeConvert;
  protected
    function GetAsObject: TObject;
    function GetAsInterface: IPSCInterface;
    function GetAsBoolean : boolean;
    function GetAsCurrency : Currency;
    function GetAsDateTime : TDateTime;
    function GetAsDouble : Double;
    function GetAsInteger : Integer;
    function GetAsString : String;  //Ansistring;
    function GetAsWideString: WideString;

    procedure SetAsBoolean(Value : boolean);
    procedure SetAsDateTime(const Value : TDateTime);
    procedure SetAsDouble(const Value : Double);
    procedure SetAsInteger(Value : Integer);
    procedure SetAsString(const Value : String);  // Ansistring);
    procedure SetAsObject(Value:TObject);
    procedure SetAsInterface(const Value:IPSCInterface);
    procedure SetAsCurrency(const Value : Currency);
    procedure SetAsWideString(const Value: WideString);

    procedure Clear;
    procedure Edit;
    procedure Post;
    procedure Cancel;

    procedure BeforeChange;
    procedure AfterChange;
  protected
    function CanModify:Boolean;virtual;
    function GetDisplayLabel:String;virtual;
    function GetRequired:Boolean;virtual;
    function GetVisible:Boolean;virtual;

    function GetTypeConvert:IPSCTypeConvert;virtual;

    procedure InternalClear;virtual;
    procedure InternalEdit;virtual;
    procedure InternalPost;virtual;
    procedure InternalCancel;virtual;
    function IsNull:Boolean;virtual;
    function GetFieldType : TPSCFieldType;virtual;
    function GetName: String;virtual;

    function InternalGetAsObject: TObject;virtual;
    function InternalGetAsInterface: IPSCInterface;virtual;
    function InternalGetAsBoolean : boolean;virtual;
    function InternalGetAsCurrency : Currency;virtual;
    function InternalGetAsDateTime : TDateTime;virtual;
    function InternalGetAsDouble : Double;virtual;
    function InternalGetAsInteger : Integer;virtual;
    function InternalGetAsString : String;virtual;  //Ansistring;virtual;
    function InternalGetAsWideString : Widestring;virtual;

    procedure InternalSetAsBoolean(Value : boolean);virtual;
    procedure InternalSetAsDateTime(const Value : TDateTime);virtual;
    procedure InternalSetAsDouble(const Value : Double);virtual;
    procedure InternalSetAsInteger(Value : Integer);virtual;
    procedure InternalSetAsString(const Value : String {AnsiString});virtual;
    procedure InternalSetAsWideString(const Value : WideString);virtual;
    procedure InternalSetAsObject(Value:TObject);virtual;
    procedure InternalSetAsInterface(const Value:IPSCInterface);virtual;
    procedure InternalSetAsCurrency(const Value : Currency);virtual;
  public
    constructor Create(const ATypeConvert:IPSCTypeConvert);
  end;

  TPSCMemField=class(TPSCCustomField,IPSCDataField)
  private
    FData:Array[Boolean] of TPSCVariantRec;
    FEditing:Boolean;
    FName:String;
  protected
    function InternalGetAsObject: TObject;override;
    function InternalGetAsInterface: IPSCInterface;override;
    function InternalGetAsBoolean : boolean;override;
    function InternalGetAsCurrency : Currency;override;
    function InternalGetAsDateTime : TDateTime;override;
    function InternalGetAsDouble : Double;override;
    function InternalGetAsInteger : Integer;override;
    function InternalGetAsString : String;override; //Ansistring;override;
    function InternalGetAsWideString : Widestring;override;

    function GetFieldType : TPSCFieldType;override;

    procedure InternalSetAsObject(Value:TObject);override;
    procedure InternalSetAsInterface(const Value:IPSCInterface);override;
    procedure InternalSetAsBoolean(Value : boolean);override;
    procedure InternalSetAsDateTime(const Value : TDateTime);override;
    procedure InternalSetAsDouble(const Value : Double);override;
    procedure InternalSetAsCurrency(const Value : Currency);override;
    procedure InternalSetAsInteger (Value : Integer);override;
    procedure InternalSetAsString(const Value : String {Ansistring});override;
    procedure InternalSetAsWideString(const Value : WideString);override;

    function IsNull:Boolean;override;
    function GetName: String;override;
    procedure InternalClear;override;

    procedure InternalEdit;override;
    procedure InternalPost;override;
    procedure InternalCancel;override;
  public
    constructor Create(const AName:String;AType:TPSCFieldType;
      const ATypeConvert:IPSCTypeConvert);
  end;

  TPSCMemDataFieldDef = class(TInterfacedObject,IPSCDataFieldDef)
  private
    FName:String;
    FType:TPSCFieldType;
  protected
    function GetName:String;
    function GetType:TPSCFieldType;
  public
    constructor Create(const AName:String; AType:TPSCFieldType);
  end;

  TPSCMemDataFieldDefs = class(TInterfacedObject,IPSCDataFieldDefs,
    IPSCDataFieldDefsEx)
  private
    FList:IPSCInterfaceList;
  protected
    function GetFieldDefCount:Integer;
    function GetFieldDef(const AIndex: Integer): IPSCDataFieldDef;
    function AddFieldDef(const AName:String; AType:TPSCFieldType):IPSCDataFieldDef;
  public
    constructor Create;
  end;

  TPSCMemDataFields = class(TPSCEvents,IPSCDataFields)
  private
    FList:IPSCInterfaceList;
    FTypeConvert:IPSCTypeConvert;
  protected
    function GetField(const AIndex: integer): IPSCDataField;
    function FindField(const AFieldName:String): IPSCDataField;
    function GetFieldCount:Integer;

    procedure CreateFields(const ADefs:IPSCDataFieldDefs);
  public
    constructor Create(const ADefs:IPSCDataFieldDefs;
      const ATypeConvert:IPSCTypeConvert);
  end;

{----------------------------------------}

procedure TPSCMemDataFields.CreateFields(const ADefs:IPSCDataFieldDefs);
var
  i:Integer;
begin
  If ADefs=nil then
    exit;
  With ADefs do
    for i:=0 to GetFieldDefCount-1 do
      With GetFieldDef(i) do
        FList.Add(PSCCreateMemField(GetName,GetType,FTypeConvert));
end;

{----------------------------------------}

constructor TPSCMemDataFields.Create(const ADefs:IPSCDataFieldDefs;
  const ATypeConvert:IPSCTypeConvert);
begin
  inherited Create;
  FTypeConvert:=ATypeConvert;
  FList:=PSCCreateInterfaceList;
  CreateFields(ADefs);
end;

{----------------------------------------}

function TPSCMemDataFields.GetField(const AIndex: integer): IPSCDataField;
begin
  Result:=IPSCDataField(FList.GetItem(AIndex));
end;

{----------------------------------------}

function TPSCMemDataFields.GetFieldCount:Integer;
begin
  result:=FList.Count;
end;

{----------------------------------------}

function TPSCMemDataFields.FindField(const AFieldName:String): IPSCDataField;
var
  i:Integer;
begin
  for i:=0 to GetFieldCount-1 do
    If PSCCompareText(AFieldName,GetField(i).GetName)=0 then
    begin
      Result:=GetField(i);
      exit;
    end;
  Result:=nil;
end;

{----------------------------------------}

constructor TPSCMemDataFieldDef.Create(const AName:String; AType:TPSCFieldType);
begin
  inherited Create;
  FName:=AName;
  FType:=AType;
end;

{----------------------------------------}

function TPSCMemDataFieldDef.GetName:String;
begin
  Result:=FName;
end;

{----------------------------------------}

function TPSCMemDataFieldDef.GetType:TPSCFieldType;
begin
  Result:=FType;
end;

{----------------------------------------}

function PSCCreateMemDataFields(const ADefs:IPSCDataFieldDefs;
  const ATypeConvert:IPSCTypeConvert):IPSCDataFields;
begin
  Result:=TPSCMemDataFields.Create(ADefs,ATypeConvert);
end;

{----------------------------------------}

function PSCCreateMemDataFields(const ANames:Array of String;
  const ATypes:Array of TPSCFieldType;
  const ATypeConvert:IPSCTypeConvert=nil):IPSCDataFields;
var
  MyDefs:IPSCDataFieldDefs;
begin
  MyDefs:=PSCCreateMemDataFieldDefs(ANames,ATypes);
  Result:=PSCCreateMemDataFields(MyDefs,ATypeConvert);
end;

{----------------------------------------}

function PSCCreateMemDataFieldDefs:IPSCDataFieldDefsEx;
begin
  Result:=TPSCMemDataFieldDefs.Create;
end;

{----------------------------------------}

function PSCCreateMemDataFieldDefs(const ANames:Array of String;
  const ATypes:Array of TPSCFieldType):IPSCDataFieldDefsEx;
var
  i:Integer;
begin
  Result:=PSCCreateMemDataFieldDefs;

  With Result do
    for i:=Low(ANames) to High(ANames) do
      AddFieldDef(ANames[i],ATypes[i]);
end;

{----------------------------------------}

function TPSCMemDataFieldDefs.GetFieldDefCount:Integer;
begin
  Result:=FList.Count;
end;

{----------------------------------------}

constructor TPSCMemDataFieldDefs.Create;
begin
  inherited Create;
  FList:=PSCCreateInterfaceList;
end;

{----------------------------------------}

function TPSCMemDataFieldDefs.AddFieldDef(const AName:String; AType:TPSCFieldType):IPSCDataFieldDef;
begin
  Result:=TPSCMemDataFieldDef.Create(AName,AType);
  FList.Add(Result);
end;

{----------------------------------------}

function TPSCMemDataFieldDefs.GetFieldDef(const AIndex: Integer): IPSCDataFieldDef;
begin
  Result:=IPSCDataFieldDef(FList.GetItem(AIndex));
end;

{----------------------------------------}

function TPSCMemField.GetName: String;
begin
  Result:=FName;
end;

{----------------------------------------}

function PSCCreateMemField(const AName:String;AType:TPSCFieldType;
  const ATypeConvert:IPSCTypeConvert):IPSCDataField;
begin
  Result:=TPSCMemField.Create(AName,AType,ATypeConvert);
end;

{----------------------------------------}

procedure TPSCCustomField.Edit;
begin
  HandleSimpleEvent(EVENT_BEFORE_EDIT);
  InternalEdit;
  HandleSimpleEvent(EVENT_AFTER_EDIT);
end;

{----------------------------------------}

procedure TPSCCustomField.Post;
begin
  HandleSimpleEvent(EVENT_BEFORE_POST);
  InternalPost;
  HandleSimpleEvent(EVENT_AFTER_POST);
end;

{----------------------------------------}

procedure TPSCCustomField.Cancel;
begin
  HandleSimpleEvent(EVENT_BEFORE_CANCEL);
  InternalCancel;
  HandleSimpleEvent(EVENT_AFTER_CANCEL);
end;

{----------------------------------------}

procedure TPSCCustomField.Clear;
begin
  BeforeChange;
  InternalClear;
  AfterChange;
end;

{----------------------------------------}

procedure TPSCCustomField.BeforeChange;
begin
  HandleSimpleEvent(EVENT_BEFORE_CHANGE);
end;

{----------------------------------------}

procedure TPSCCustomField.AfterChange;
begin
  HandleSimpleEvent(EVENT_AFTER_CHANGE);
end;

{----------------------------------------}

constructor TPSCCustomField.Create(const ATypeConvert:IPSCTypeConvert);
begin
  inherited Create;
  FTypeConvert:=ATypeConvert;
end;

{----------------------------------------}

procedure TPSCCustomField.InternalClear;
begin
end;

{----------------------------------------}

procedure TPSCCustomField.InternalEdit;
begin
end;

{----------------------------------------}

procedure TPSCCustomField.InternalPost;
begin
end;

{----------------------------------------}

procedure TPSCCustomField.InternalCancel;
begin
end;

{----------------------------------------}

procedure TPSCCustomField.InternalSetAsBoolean(Value : boolean);
begin
end;

{----------------------------------------}

procedure TPSCCustomField.InternalSetAsDateTime(const Value : TDateTime);
begin
end;

{----------------------------------------}

procedure TPSCCustomField.InternalSetAsDouble(const Value : Double);
begin
end;

{----------------------------------------}

procedure TPSCCustomField.InternalSetAsInteger(Value : Integer);
begin
end;

{----------------------------------------}

procedure TPSCCustomField.InternalSetAsString(const Value : String {AnsiString});
begin
end;

{----------------------------------------}

procedure TPSCCustomField.InternalSetAsWideString(const Value : WideString);
begin
end;

{----------------------------------------}

procedure TPSCCustomField.InternalSetAsObject(Value:TObject);
begin
end;

{----------------------------------------}

procedure TPSCCustomField.InternalSetAsInterface(const Value:IPSCInterface);
begin
end;

{----------------------------------------}

procedure TPSCCustomField.InternalSetAsCurrency(const Value : Currency);
begin
end;

{----------------------------------------}

function TPSCCustomField.IsNull:Boolean;
begin
  Result:=True;
end;

{----------------------------------------}

function TPSCCustomField.GetFieldType : TPSCFieldType;
begin
  Result:=FT_UNK;
end;

{----------------------------------------}

function TPSCCustomField.GetName: String;
begin
  Result:='';
end;

{----------------------------------------}

function TPSCCustomField.InternalGetAsObject: TObject;
begin
  Result:=nil;
end;

{----------------------------------------}

function TPSCCustomField.InternalGetAsInterface: IPSCInterface;
begin
  Result:=nil;
end;

{----------------------------------------}

function TPSCCustomField.InternalGetAsBoolean : boolean;
begin
  Result:=False;
end;

{----------------------------------------}

function TPSCCustomField.InternalGetAsCurrency : Currency;
begin
  Result:=0;
end;

{----------------------------------------}

function TPSCCustomField.InternalGetAsDateTime : TDateTime;
begin
  Result:=0;
end;

{----------------------------------------}

function TPSCCustomField.InternalGetAsDouble : Double;
begin
  Result:=0;
end;

{----------------------------------------}

function TPSCCustomField.InternalGetAsInteger : Integer;
begin
  Result:=0;
end;

{----------------------------------------}

function TPSCCustomField.InternalGetAsString : String; //Ansistring;
begin
  Result:='';
end;

{----------------------------------------}

function TPSCCustomField.InternalGetAsWideString : Widestring;
begin
  Result:='';
end;

{----------------------------------------}

function TPSCCustomField.GetAsBoolean : boolean;
begin
  case GetFieldType of
    FT_BOOL:
      Result:=InternalGetAsBoolean;
    FT_STRING:
       Result:=GetTypeConvert.StrToBoolean(InternalGetAsString);
    FT_INT:
      If InternalGetAsInteger<>0 then
        Result:=True
      else
        Result:=False;
    FT_CURRENCY:
      If InternalGetAsCurrency<>0 then
        Result:=True
      else
        Result:=False;
    FT_FLOAT:
      If InternalGetAsDouble<>0 then
        Result:=True
      else
        Result:=False;
    else
      Result:=False;
  end;
end;

{----------------------------------------}

procedure TPSCCustomField.SetAsWideString(const Value: WideString);
begin
  case GetFieldType of
    FT_WIDESTR:
     begin
       BeforeChange;
       InternalSetAsWideString(Value);
       AfterChange;
     end;
  else
    SetAsString(Value);
  end;
end;

{----------------------------------------}

function TPSCCustomField.GetAsWideString: WideString;
begin
  case GetFieldType of
    FT_WIDESTR:
      Result:=InternalGetAsWideString;
  else
    Result:=GetAsString;
  end;
end;

{----------------------------------------}

function TPSCCustomField.GetAsString : String;  // AnsiString;
begin
  case GetFieldType of
    FT_INT:
      Result:=GetTypeConvert.IntToStr(InternalGetAsInteger);
    FT_DATETIME:
      Result:=GetTypeConvert.DateTimeToStr(InternalGetAsDateTime);
    FT_DATE:
      Result:=GetTypeConvert.DateToStr(InternalGetAsDateTime);
    FT_TIME:
      Result:=GetTypeConvert.TimeToStr(InternalGetAsDateTime);
    FT_CURRENCY:
      Result:=GetTypeConvert.CurrencyToStr(InternalGetAsCurrency);
    FT_FLOAT:
      Result:=GetTypeConvert.DoubleToStr(InternalGetAsDouble);
    FT_STRING:
      Result:=InternalGetAsString;
    FT_BOOL:
      Result:=GetTypeConvert.BooleanToStr(InternalGetAsBoolean);
    else
      Result:='';
  end;
end;

{----------------------------------------}

function TPSCCustomField.GetAsDateTime : TDateTime;
begin
  case GetFieldType of
    FT_DATETIME:
      Result:=InternalGetAsDateTime;
    FT_DATE:
      Result:=PSCDateOf(InternalGetAsDateTime);
    FT_TIME:
      Result:=PSCTimeOf(InternalGetAsDateTime);
    FT_STRING:
      Result:=GetTypeConvert.StrToDateTime(InternalGetAsString);
    else
      Result:=0;
  end;
end;

{----------------------------------------}

function TPSCCustomField.GetAsDouble : Double;
begin
  case GetFieldType of
    FT_INT:
      Result:=InternalGetAsInteger;
    FT_DATETIME:
      Result:=InternalGetAsDateTime;
    FT_DATE:
      Result:=PSCDateOf(InternalGetAsDateTime);
    FT_TIME:
      Result:=PSCTimeOf(InternalGetAsDateTime);
    FT_CURRENCY:
      Result:=InternalGetAsCurrency;
    FT_FLOAT:
      Result:=InternalGetAsDouble;
    FT_STRING:
      Result:=GetTypeConvert.StrToDouble(InternalGetAsString);
    FT_BOOL:
      If InternalGetAsBoolean then
        Result:=1
      else
        Result:=0;
  else
    Result:=0;
  end;
end;

{----------------------------------------}

function TPSCCustomField.GetAsInteger : Integer;
begin
  case GetFieldType of
    FT_INT:
      Result:=InternalGetAsInteger;
    FT_STRING:
      Result:=GetTypeConvert.StrToInt(InternalGetAsString);
    FT_BOOL:
      If InternalGetAsBoolean then
        Result:=1
      else
        Result:=0;
    else
      Result:=0;
  end;
end;

{----------------------------------------}

function TPSCCustomField.GetAsCurrency : Currency;
begin
  case GetFieldType of
    FT_INT:
      Result:=InternalGetAsInteger;
    FT_CURRENCY:
      Result:=InternalGetAsCurrency;
    FT_FLOAT:
      Result:=InternalGetAsDouble;
    FT_STRING:
      Result:=GetTypeConvert.StrToCurrency(InternalGetAsString);
    else
      Result:=0;
  end;
end;

{----------------------------------------}

function TPSCCustomField.GetTypeConvert:IPSCTypeConvert;
begin
  Result:=FTypeConvert;
end;

{----------------------------------------}

function TPSCCustomField.GetAsObject: TObject;
begin
  case GetFieldType of
    FT_OBJECT:
      Result:=InternalGetAsObject;
    else
      Result:=nil;
  end;
end;

{----------------------------------------}

function TPSCCustomField.GetAsInterface: IPSCInterface;
begin
  case GetFieldType of
    FT_INTERFACE:
      Result:=InternalGetAsInterface;
    else
      Result:=nil;
  end;
end;

{----------------------------------------}

procedure TPSCCustomField.SetAsBoolean(Value : boolean);
begin
  BeforeChange;
  Case GetFieldType of
    FT_INT:
      If Value then
        InternalSetAsInteger(1)
      else
        InternalSetAsInteger(0);
    FT_FLOAT:
      If Value then
        InternalSetAsDouble(1)
      else
        InternalSetAsDouble(0);
    FT_STRING:
      InternalSetAsString(GetTypeConvert.BooleanToStr(Value));
    FT_BOOL:
      InternalSetAsBoolean(Value);
  end;
  AfterChange;
end;

{----------------------------------------}

procedure TPSCCustomField.SetAsDateTime(const Value : TDateTime);
begin
  BeforeChange;
  case GetFieldType of
    FT_DATETIME:
      InternalSetAsDateTime(Value);
    FT_DATE:
      InternalSetAsDateTime(PSCDateOf(Value));
    FT_TIME:
      InternalSetAsDateTime(PSCTimeOf(Value));
    FT_FLOAT:
      InternalSetAsDouble(Value);
    FT_STRING:
      InternalSetAsString(GetTypeConvert.DateTimeToStr(Value));
  end;
  AfterChange;
end;

{----------------------------------------}

procedure TPSCCustomField.SetAsCurrency(const Value : Currency);
begin
  BeforeChange;
  case GetFieldType of
    FT_CURRENCY:
      InternalSetAsCurrency(Value);
    FT_FLOAT:
      InternalSetAsDouble(Value);
    FT_STRING:
      InternalSetAsString(GetTypeConvert.CurrencyToStr(Value));
  end;
  AfterChange;
end;

{----------------------------------------}

procedure TPSCCustomField.SetAsDouble(const Value : Double);
begin
  BeforeChange;
  case GetFieldType of
    FT_DATETIME:
      InternalSetAsDateTime(Value);
    FT_DATE:
      InternalSetAsDateTime(PSCDateOf(Value));
    FT_TIME:
      InternalSetAsDateTime(PSCTimeOf(Value));
    FT_CURRENCY:
      InternalSetAsCurrency(Value);
    FT_FLOAT:
      InternalSetAsDouble(Value);
    FT_STRING:
      InternalSetAsString(GetTypeConvert.DoubleToStr(Value));
    FT_BOOL:
      InternalSetAsBoolean(Value<>0);
  end;
  AfterChange;
end;

{----------------------------------------}

procedure TPSCCustomField.SetAsInteger(Value : Integer);
begin
  BeforeChange;
  case GetFieldType of
    FT_INT:
      InternalSetAsInteger(Value);
    FT_CURRENCY:
      InternalSetAsCurrency(Value);
    FT_FLOAT:
      InternalSetAsDouble(Value);
    FT_STRING:
      InternalSetAsString(GetTypeConvert.IntToStr(Value));
    FT_BOOL:
      InternalSetAsBoolean(Value<>0);
  end;
  AfterChange;
end;

{----------------------------------------}

procedure TPSCCustomField.SetAsString(const Value : String {AnsiString});
begin
  BeforeChange;
  case GetFieldType of
    FT_INT:
      InternalSetAsInteger(GetTypeConvert.StrToInt(Value));
    FT_DATETIME:
      InternalSetAsDateTime(GetTypeConvert.StrToDateTime(Value));
    FT_DATE:
      InternalSetAsDateTime(GetTypeConvert.StrToDate(Value));
    FT_TIME:
      InternalSetAsDateTime(GetTypeConvert.StrToTime(Value));
    FT_CURRENCY:
      InternalSetAsCurrency(GetTypeConvert.StrToCurrency(Value));
    FT_FLOAT:
      InternalSetAsDouble(GetTypeConvert.StrToDouble(Value));
    FT_STRING:
      InternalSetAsString(Value);
    FT_BOOL:
      InternalSetAsBoolean(GetTypeConvert.StrToBoolean(Value));
  end;
  AfterChange;
end;

{----------------------------------------}

procedure TPSCCustomField.SetAsObject(Value:TObject);
begin
  If GetFieldType=FT_OBJECT then
    begin
      BeforeChange;
      InternalSetAsObject(Value);
      AfterChange;
    end;
end;

{----------------------------------------}

procedure TPSCCustomField.SetAsInterface(const Value:IPSCInterface);
begin
  If GetFieldType=FT_INTERFACE then
    begin
      BeforeChange;
      InternalSetAsInterface(Value);
      AfterChange;
    end;
end;

{----------------------------------------}

function TPSCCustomField.CanModify:Boolean;
begin
  Result:=True;
end;

{----------------------------------------}

function TPSCCustomField.GetDisplayLabel:String;
begin
  Result:='';
end;

{----------------------------------------}

function TPSCCustomField.GetRequired:Boolean;
begin
  Result:=False;
end;

{----------------------------------------}

function TPSCCustomField.GetVisible:Boolean;
begin
  Result:=True;
end;

{----------------------------------------}

function TPSCMemField.InternalGetAsObject: TObject;
begin
  Result:=FData[FEditing].AsObject;
end;

{----------------------------------------}

function TPSCMemField.InternalGetAsInterface: IPSCInterface;
begin
  Result:=FData[FEditing].AsInterface;
end;

{----------------------------------------}

function TPSCMemField.InternalGetAsBoolean : boolean;
begin
  Result:=FData[FEditing].AsBoolean;
end;

{----------------------------------------}

function TPSCMemField.InternalGetAsCurrency : Currency;
begin
  Result:=FData[FEditing].AsCurrency;
end;

{----------------------------------------}

function TPSCMemField.InternalGetAsDateTime : TDateTime;
begin
  Result:=FData[FEditing].AsDateTime;
end;

{----------------------------------------}

function TPSCMemField.InternalGetAsDouble : Double;
begin
  Result:=FData[FEditing].AsDouble;
end;

{----------------------------------------}

function TPSCMemField.InternalGetAsInteger : Integer;
begin
  Result:=FData[FEditing].AsInteger;
end;

{----------------------------------------}

function TPSCMemField.InternalGetAsString : String; //Ansistring;
begin
  Result:=FData[FEditing].AsString;
end;

{----------------------------------------}

function TPSCMemField.InternalGetAsWideString : Widestring;
begin
  Result:=FData[FEditing].AsWideStr;
end;

{----------------------------------------}

function TPSCMemField.GetFieldType : TPSCFieldType;
begin
  Result:=FData[FEditing].DataType;
end;

{----------------------------------------}

constructor TPSCMemField.Create(const AName:String;AType:TPSCFieldType;
  const ATypeConvert:IPSCTypeConvert);
begin
  inherited Create(ATypeConvert);
  FName:=AName;
  FData[False].DataType:=AType;
  FData[True].DataType:=AType;
end;

{----------------------------------------}

procedure TPSCMemField.InternalEdit;
begin
  If not FEditing then
  begin
    FData[True]:=FData[False];
    FEditing:=True;
  end;
end;

{----------------------------------------}

procedure TPSCMemField.InternalPost;
begin
  If FEditing then
  begin
    FData[False]:=FData[True];
    FEditing:=False;
  end;
end;

{----------------------------------------}

procedure TPSCMemField.InternalCancel;
begin
  FEditing:=False;
end;

{----------------------------------------}

procedure TPSCMemField.InternalClear;
begin
  FData[FEditing].AsObject:=nil;
  FData[FEditing].AsInterface:=nil;
  FData[FEditing].AsBoolean:=False;
  FData[FEditing].AsCurrency:=0;
  FData[FEditing].AsDateTime:=0;
  FData[FEditing].AsDouble:=0;
  FData[FEditing].AsInteger:=0;
  FData[FEditing].AsString:='';
  FData[FEditing].HasValue:=False;
end;

{----------------------------------------}

procedure TPSCMemField.InternalSetAsObject(Value:TObject);
begin
  FData[FEditing].AsObject:=Value;
  FData[FEditing].HasValue:=Value<>nil;
end;

{----------------------------------------}

procedure TPSCMemField.InternalSetAsInterface(const Value:IPSCInterface);
begin
  FData[FEditing].AsInterface:=Value;
  FData[FEditing].HasValue:=Value<>nil;
end;

{----------------------------------------}

procedure TPSCMemField.InternalSetAsBoolean(Value : boolean);
begin
  FData[FEditing].AsBoolean:=Value;
  FData[FEditing].HasValue:=True;
end;

{----------------------------------------}

procedure TPSCMemField.InternalSetAsDateTime(const Value : TDateTime);
begin
  FData[FEditing].AsDateTime:=Value;
  FData[FEditing].HasValue:=True;
end;

{----------------------------------------}

procedure TPSCMemField.InternalSetAsDouble(const Value : Double);
begin
  FData[FEditing].AsDouble:=Value;
  FData[FEditing].HasValue:=True;
end;

{----------------------------------------}

procedure TPSCMemField.InternalSetAsCurrency(const Value : Currency);
begin
  FData[FEditing].AsCurrency:=Value;
  FData[FEditing].HasValue:=True;
end;

{----------------------------------------}

procedure TPSCMemField.InternalSetAsInteger(Value : Integer);
begin
  FData[FEditing].AsInteger:=Value;
  FData[FEditing].HasValue:=True;
end;

{----------------------------------------}

procedure TPSCMemField.InternalSetAsString(const Value : String {Ansistring});
begin
  FData[FEditing].AsString:=Value;
  FData[FEditing].HasValue:=Value<>'';
end;

{----------------------------------------}

procedure TPSCMemField.InternalSetAsWideString(const Value : WideString);
begin
  FData[FEditing].AsWideStr:=Value;
  FData[FEditing].HasValue:=Value<>'';
end;

{----------------------------------------}

function TPSCMemField.IsNull:Boolean;
begin
  Result:=not FData[FEditing].HasValue;
end;

{------------------------------------------------------------------}

procedure PSCOperateStrings(const S:IPSCStrings;
  StringsProc:TPSCStringsOperateProc;UserData:Cardinal=0);

  procedure UpperSortedStrings(const S:IPSCStrings);
  begin
    S.Sorted:=False;
    StringsProc(S,UserData);
    S.Sorted:=True;
  end;

begin
  S.BeginUpdate;
  try
    If S.Sorted then
      UpperSortedStrings(S)
    else
      StringsProc(S,UserData);
  finally
    S.EndUpdate;
  end;
end;

{------------------------------------------------------------------}

function MyAnsiUpperCaseStrings(const S:IPSCStrings;UserData:Cardinal):boolean;
var
  i : integer;
begin
  Result:=True;
  for i := 0 to S.Count - 1 do
    // S[i] := PSCUpperCase(S[i]);
    S[i] := system.SysUtils.AnsiUpperCase(S[i]);
end;

procedure PSCAnsiUpperCaseStrings(const S : IPSCStrings);
begin
  PSCOperateStrings(S,MyAnsiUpperCaseStrings,0);
end;

{--------------------------------------}

end.
