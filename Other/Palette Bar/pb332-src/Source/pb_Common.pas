//----------------------------------------------------------------------------
// Unit Name: pb_Common
// Author:    Helli
// Date:      16.07.2003
// Purpose:
// History:
//----------------------------------------------------------------------------
//  Copyright © 2003 by Hellinger Software.  All Rights Reserved.
//----------------------------------------------------------------------------

unit pb_Common;

interface

uses Classes, Controls, Menus, Registry;

const cCompile =    '3.21';
      cDate =       '16.09.2003';
      cProgName =   'PaletteBar ' + cCompile;
      cVersion =    cCompile + ' - ' + cDate;
      cWizardMenu = 'P&aletteBar';
      cWizardName = 'PaletteBarWizard';
      cDebugFile =  'c:\pb_debug.txt';


//------------------------------------------------------------------------------
// Set-Handling (from the unit Bitsets)
//------------------------------------------------------------------------------

type TBit = (Bit0,   Bit1,   Bit2,   Bit3,   Bit4,   Bit5,   Bit6,   Bit7,
             Bit8,   Bit9,   Bit10,  Bit11,  Bit12,  Bit13,  Bit14,  Bit15,
             Bit16,  Bit17,  Bit18,  Bit19,  Bit20,  Bit21,  Bit22,  Bit23,
             Bit24,  Bit25,  Bit26,  Bit27,  Bit28,  Bit29,  Bit30,  Bit31,
             Bit32,  Bit33,  Bit34,  Bit35,  Bit36,  Bit37,  Bit38,  Bit39,
             Bit40,  Bit41,  Bit42,  Bit43,  Bit44,  Bit45,  Bit46,  Bit47,
             Bit48,  Bit49,  Bit50,  Bit51,  Bit52,  Bit53,  Bit54,  Bit55,
             Bit56,  Bit57,  Bit58,  Bit59,  Bit60,  Bit61,  Bit62,  Bit63,
             Bit64,  Bit65,  Bit66,  Bit67,  Bit68,  Bit69,  Bit70,  Bit71,
             Bit72,  Bit73,  Bit74,  Bit75,  Bit76,  Bit77,  Bit78,  Bit79,
             Bit80,  Bit81,  Bit82,  Bit83,  Bit84,  Bit85,  Bit86,  Bit87,
             Bit88,  Bit89,  Bit90,  Bit91,  Bit92,  Bit93,  Bit94,  Bit95,
             Bit96,  Bit97,  Bit98,  Bit99,  Bit100, Bit101, Bit102, Bit103,
             Bit104, Bit105, Bit106, Bit107, Bit108, Bit109, Bit110, Bit111,
             Bit112, Bit113, Bit114, Bit115, Bit116, Bit117, Bit118, Bit119,
             Bit120, Bit121, Bit122, Bit123, Bit124, Bit125, Bit126, Bit127,
             Bit128, Bit129, Bit130, Bit131, Bit132, Bit133, Bit134, Bit135,
             Bit136, Bit137, Bit138, Bit139, Bit140, Bit141, Bit142, Bit143,
             Bit144, Bit145, Bit146, Bit147, Bit148, Bit149, Bit150, Bit151,
             Bit152, Bit153, Bit154, Bit155, Bit156, Bit157, Bit158, Bit159,
             Bit160, Bit161, Bit162, Bit163, Bit164, Bit165, Bit166, Bit167,
             Bit168, Bit169, Bit170, Bit171, Bit172, Bit173, Bit174, Bit175,
             Bit176, Bit177, Bit178, Bit179, Bit180, Bit181, Bit182, Bit183,
             Bit184, Bit185, Bit186, Bit187, Bit188, Bit189, Bit190, Bit191,
             Bit192, Bit193, Bit194, Bit195, Bit196, Bit197, Bit198, Bit199,
             Bit200, Bit201, Bit202, Bit203, Bit204, Bit205, Bit206, Bit207,
             Bit208, Bit209, Bit210, Bit211, Bit212, Bit213, Bit214, Bit215,
             Bit216, Bit217, Bit218, Bit219, Bit220, Bit221, Bit222, Bit223,
             Bit224, Bit225, Bit226, Bit227, Bit228, Bit229, Bit230, Bit231,
             Bit232, Bit233, Bit234, Bit235, Bit236, Bit237, Bit238, Bit239,
             Bit240, Bit241, Bit242, Bit243, Bit244, Bit245, Bit246, Bit247,
             Bit248, Bit249, Bit250, Bit251, Bit252, Bit253, Bit254, Bit255);

type  T8Bits =   set of Bit0..Bit7;    // Byte    1
      T16Bits =  set of Bit0..Bit15;   // Word    2
      T32Bits =  set of Bit0..Bit31;   // Long    4
      T64Bits =  set of Bit0..Bit63;   // Int64   8
      T128Bits = set of Bit0..Bit127;  // Int128  16
      T256Bits = set of Bit0..Bit255;  // Int256  32

type  TInt128 =  array [0..1] of Int64;
      TInt256 =  array [0..3] of Int64;

function ValTo8Bits   (const i8):  t8Bits;
function ValTo16Bits  (const i16): t16Bits;
function ValTo32Bits  (const i32): t32Bits;
function ValTo64Bits  (const i64): t64Bits;
function ValTo128Bits (const i128): t128Bits;
function ValTo256Bits (const i256): t256Bits;
// Wert in Bitset

function BitsToByte   (const t8:  t8Bits): Byte;
function BitsToWord   (const t16: t16Bits): Word;
function BitsToInt    (const t32: t32Bits): Integer;
function BitsToInt64  (const t64: t64Bits): Int64;
function BitsToInt128 (const t128: t128Bits): tInt128;
function BitsToInt256 (const t256: t256Bits): tInt256;
// Bitset in Wert

//------------------------------------------------------------------------------
// from the unit Basics
//------------------------------------------------------------------------------

function  Min(i1, i2: INTEGER): INTEGER;
function  Max(i1, i2: INTEGER): INTEGER;

function GetStrPart (var s: string; delimiter: CHAR): string;
// Liefert einen Teilstring beginnend von Anfang bis zum Zeichen
// delimiter. Führende Blanks werden abgeschnitten!

function StrIPos (const s1, s2: string; pos, max: Integer): Integer;
function StrIPosCI (const s1, s2: string; pos, max: Integer): Integer;
// Wie Pos, bloss besser... 8-)  ...CI = CaseInsensitive

function StrMatch (const str1, str2: string; wild, joker: CHAR): BOOLEAN;
function StrMatchCI (const str1, str2: string; wild, joker: CHAR): BOOLEAN;
// Vergleicht zwei String, obs sie gleichen Inhalts sind, unter der
// Berüvcksichtigung von Wildcards

procedure FreeList (var l: TList);
// Löscht eine TList und deren Inhalt
procedure ReleaseList (var l: TList);
// Löscht den Inhalt einer TList

function DelShortcutChar (const s: string): string;
// Löscht das Zeichen & aus dem String

function FindMenu (menu: TMainMenu; const s: string): TMenuItem; overload;
function FindMenu (menu: TMenuItem; const s: string): TMenuItem; overload;
function FindMenuInt (menu: TMainMenu; const s: string): Integer; overload;
function FindMenuInt (menu: TMenuItem; const s: string): Integer; overload;
// Findet ein Menü anhand des Namens ( nicht (!) der caption!)

function TextLong(s: string): LONGINT;

procedure Debug (const s: string);

//----------------------------------------------------------------------------
// Delphi Registry-Eintrage
//----------------------------------------------------------------------------

const cD7 =                'Delphi 7';
      cD6 =                'Delphi 6';
      cC6 =                'C++Builder 6';

const cRegHelpFile =       'WinHelpPath';

//----------------------------------------------------------------------------
// PaletteBar Registry-Eintrage
//----------------------------------------------------------------------------

const cRegPalOpen =        'Open';
      cRegMenu =           'Menu';
      cRegPalHotTrack =    'Hottrack';
      cRegPalShowIcons =   'Icons';
      cRegPalTextsize =    'Textsize';
      cRegPalMenu =        'Menu';
      cRegPalButtons =     'Buttons';
      cRegPalBtnPos =      'ButtonPos';
      cRegToolbarViz =     'ToolBarViz';
      cRegPalFloating =    'Floating';
      cRegPalLeft =        'Left';
      cRegPalTop =         'Top';
      cRegPalWidth =       'Width';
      cRegPalHeight =      'Height';
      cRegPalRootPath =    'RootPath';
      cRegLanguage =       'Language';
      cRegFillmode =       'Fillmode';
      cRegAZBarShow =      'AZBarShow';
      cRegAZBarPos =       'AZBarPos';
      cRegAZBarFilter =    'AZBarFilter';
      cRegSaveHistory =    'SaveHistory';
      cRegSearchBar =      'SeachBar';
      cRegSearchBarViz =   'SeachBarViz';
      cRegSearchText =     'SearchText';
      cRegCategory =       'Categories';
      cRegHistory =        'History';
      cRegFavorites =      'Favorites';
      cRegStatusbar =      'StausbarViz';

//----------------------------------------------------------------------------
// Namen von Delphi IDE-Komponenten
//----------------------------------------------------------------------------

const cConfigMenu =        'ComponentMenu';
      cConfigItem =        'ComponentPaletteItem';
      cToolsMenu =         'ToolsMenu';
      cPaletteBar =        'PaletteBar';
      cMenuBar =           'MenuBar';
      cTabCaption =        'Tab';
      cPBMenu =            'pb_mnu_show';

const cDataModuleForm =    'DataModuleForm';
      cDataModuleSurface = 'DataModuleSurface';
      cWinControlForm =    'WinControlForm';
      cWidgetControlForm = 'WidgetControlForm';

//-----------------------------------------------------------------------------
// Sprachkonstanten
//-----------------------------------------------------------------------------

const c3Dots =            '...';

type  TLanguageText =     (ltLangName,        // Kürzel der Sprachversion
                           ltPalettebar,      // 'PaletteBar';

                           // Hauptmenü
                           ltDefCategory,     // 'Sonstige'
                           ltActCategory,     // 'Aktuelle Kategorie'
                           ltAllComps,        // 'Alle Komponenten'
                           ltHistory,         // 'Zuletzt Benutzte'
                           ltActForm,         // 'Aktuelles Formular '
                           ltFavorites,       // 'Favoriten'
                           ltCfgCategory,     // 'Kategorien anpassen';
                           ltCfgPalette,      // 'Palette konfigurieren';
                           ltCfgPaletteBar,   // 'PaletteBar konfigurieren';
                           ltShowHelp,        // 'Hilfe zur Komponente anzeigen';
                           ltAbout,           // 'Über PaletteBar';

                           // Konfigurationsmenü
                           ltLanguage,        // 'Sprache';
                                              // Die Sprachstrings werden über
                                              // die Datei palettebar.lng geladen
                           ltCompList,        // 'Komponentenliste';
                           ltToolbar,         // 'Werkzeugleiste';
                           ltA2ZBar,          // 'A-Z Leiste';
                                              // History = ltHistory

                           // Submenü PaletteBar
                           ltPalHottrack,     // 'Hottracking';
                           ltPalView,         // 'Ansicht';
                           ltPalShowIcons,    // 'Symbole';
                           ltPalShowText,     // 'Text';
                           ltPalShowBoth,     // 'Symbole und Text'
                           ltPalText,         // 'Textgröße';

                           // Submenü History
                           ltHistSave,        // 'Speichern';
                           ltHistClear,       // 'Löschen';

                           // Favoriten Menü
                           ltFavAdd,          // 'Zu Favoriten hinzufügen';
                           ltFavRemove,       // 'Aus den Favoriten entfernen';

                           // Eigenschaftsworte
                           ltTop,             // 'Oben';
                           ltRight,           // 'Rechts';
                           ltLeft,            // 'Links';
                           ltBottom,          // 'Unten';
                           ltVisible,         // 'Sichtbar';
                           ltSmall,           // 'klein';
                           ltMedium,          // 'mittel';
                           ltBig,             // 'groß';
                           ltYes,             // 'Ja';
                           ltNo,              // 'Nein';

                           // Kategorien Dialog
                           ltTabCaption,      // 'Kategorie';
                           ltCatCaption,      // 'Meta Kategorie';
                           ltOKCaption,       // '&OK';
                           ltCancelCaption,   // '&Abbruch';
                           ltAddCaption,      // '&Neu';
                           ltDelCaption,      // '&Löschen';
                           ltInsCaption,      // '&Einfügen';
                           ltCopyCaption,     // '&Kopieren';

                           // Sonstiges
                           ltShowMenu,        // 'Menü anzeigen';
                           ltShow,            // 'anzeigen';
                           ltMenu,            // 'Menü';
                           ltForm,            // 'Formular';
                           ltFilterMode,      // 'Filtern';
                           ltToolbarPos,      // 'Position';
                           ltToolbarShow,     // 'Button anzeigen';
                           ltFilterd,         // 'Gefiltert';
                           ltDeleteReally,    // 'Wirklich löschen?';
                           ltNewEntry,        // 'Neue Kategorie';

                           // Fehlermeldungen
                           ltErrorInsert,     // 'Fehler beim Einfügen der Komponente.';
                           ltErrorNoForm,     // 'Kein aktuelles Formular';

                           ltSearchbar,       // 'Suchleiste';
                           ltSearchResult,    // 'Suchergebnis';
                           ltSearchText,      // 'Suchtexte';

                           ltStatusbarViz,    // 'Statuszeile anzeigen';
                           ltToolbarviz,      // 'Werkzeugleiste anzeigen';
                           ltStatusbar,       // 'Statuszeile';

                           // Ende der Strings
                           ltNone);


var   gLangList:    TstringList;
var   gLangText:    TStringList;
var   gRegIni:      TRegIniFile;

//-----------------------------------------------------------------------------
// Konfigurationsparamenter
//-----------------------------------------------------------------------------

type  TTextsize =     (tsSmall, tsMedium, tsBig);

type  TPalShowMode =  (smIcon,
                       smText,
                       smBoth,
                       smNone);

type TFillmode =      (fmActTab,
                       fmAllTabs,
                       fmFavorites,
                       fmHistory,
                       fmActForm,
                       fmFilter,
                       fmSearch,
                       fmNone);

var   cBaseKey:       string;
      cRegKey:        string;
      cRegPalette:    string;
      cRegHelp:       string;
      cStatusbarText: string;

var   IsD7: Boolean = False;
      IsD6: Boolean = False;
      IsC6: Boolean = False;

type  pb_OptionType = record
                       RootPath:     string;
                       Language:     string;
                       Open:         Boolean;
                       Left:         Integer;
                       Top:          Integer;
                       Width:        Integer;
                       Height:       Integer;
                       Floating:     Boolean;
                       Menu:         Boolean;
                       TextSize:     TTextSize;
                       ShowMode:     TPalShowMode;
                       Fillmode:     TFillMode;
                       Hottrack:     Boolean;
                       ButtonPos:    TAlign;
                       Buttons:      T32Bits;
                       A2ZPos:       TAlign;
                       A2ZFilter:    Boolean;
                       SaveHistory:  Boolean;
                       SearchBar:    TAlign;
                       A2ZViz:       Boolean;
                       SearchBarViz: Boolean;
                       StatusBarViz: Boolean;
                       ToolBarViz:   Boolean;
                      end;

var PB_Options: pb_OptionType;
    oldopt:     pb_OptionType;

function OptionsChanged: Boolean;

procedure LoadFromRegistry;
// Lädt die Optionen aus der Registry

procedure SaveToRegistry;
// Schreibt die Optionen in die Registry

procedure LoadLangTypes;
// Lädt das Sprachverzeichnis

procedure LoadLanguage (const lang: string);
// lädt eine sprachdatei

function GetLangStr (txt: TLanguageText): string;
// Liefert einen String in der eingestellten Sprache



implementation

uses Sysutils, Dialogs;


//-----------------------------------------------------------------------------
function OptionsChanged: Boolean;
begin
 Result:= (PB_Options.Language <> oldopt.Language) or
          (PB_Options.Language <> oldopt.Language) or
          (PB_Options.Open <> oldopt.Open) or
          (PB_Options.Left <> oldopt.Left) or
          (PB_Options.Top <> oldopt.Top) or
          (PB_Options.Width <> oldopt.Width) or
          (PB_Options.Height <> oldopt.Height) or
          (PB_Options.Menu <> oldopt.Menu) or
          (PB_Options.TextSize <> oldopt.TextSize) or
          (PB_Options.ShowMode <> oldopt.ShowMode) or
          (PB_Options.Fillmode <> oldopt.Fillmode) or
          (PB_Options.Hottrack <> oldopt.Hottrack) or
          (PB_Options.ButtonPos <> oldopt.ButtonPos) or
          (PB_Options.Buttons <> oldopt.Buttons) or
          (PB_Options.A2ZPos <> oldopt.A2ZPos) or
          (PB_Options.A2ZFilter <> oldopt.A2ZFilter) or
          (PB_Options.A2ZViz <> oldopt.A2ZViz) or
          (PB_Options.SaveHistory <> oldopt.SaveHistory) or
          (PB_Options.SearchBar <> oldopt.SearchBar) or
          (PB_Options.SearchBarViz <> oldopt.SearchBarViz) or
          (PB_Options.StatusBarViz <> oldopt.StatusBarViz) or
          (PB_Options.ToolBarViz <> oldopt.ToolBarViz);
end;

//-----------------------------------------------------------------------------
procedure SaveToRegistry;
var fm: TFillmode;
begin
 gRegIni.WriteString  ('', cRegLanguage, PB_Options.Language);
 gRegIni.WriteBool ('', cRegPalOpen, PB_Options.Open);
 gRegIni.WriteBool ('', cRegPalFloating, PB_Options.Floating);
 if PB_Options.Floating then begin // Nur wenn wir nicht gedockt sind
  gRegIni.WriteInteger ('', cRegPalLeft, Ord (PB_Options.Left));
  gRegIni.WriteInteger ('', cRegPalTop, Ord (PB_Options.Top));
  gRegIni.WriteInteger ('', cRegPalWidth, Ord (PB_Options.Width));
  gRegIni.WriteInteger ('', cRegPalHeight, Ord (PB_Options.Height));
 end;
 gRegIni.WriteBool ('', cRegPalMenu, PB_Options.Menu);
 gRegIni.WriteInteger ('', cRegPalTextsize, Ord (PB_Options.TextSize));
 gRegIni.WriteInteger ('', cRegPalShowIcons, Ord (PB_Options.ShowMode));

 case PB_Options.Fillmode of
  fmActForm,
  fmFilter,
  fmSearch,
  fmNone:    fm:= fmNone else fm:= PB_Options.Fillmode;
 end;

 gRegIni.WriteInteger ('', cRegFillmode, Ord (fm));
 gRegIni.WriteBool ('', cRegPalHotTrack, PB_Options.Hottrack);
 gRegIni.WriteInteger ('', cRegPalBtnPos, Ord (PB_Options.ButtonPos));
 gRegIni.WriteInteger ('', cRegPalButtons, BitsToInt (PB_Options.Buttons));
 gRegIni.WriteInteger ('', cRegAZBarPos, Ord (PB_Options.A2ZPos));
 gRegIni.WriteBool ('', cRegAZBarFilter, PB_Options.A2ZFilter);
 gRegIni.WriteBool ('', cRegAZBarShow, PB_Options.A2ZViz);
 gRegIni.WriteBool ('', cRegSaveHistory, PB_Options.SaveHistory);
 gRegIni.WriteInteger ('', cRegSearchBar, Ord(PB_Options.SearchBar));
 gRegIni.WriteBool ('', cRegSearchBarViz, PB_Options.SearchBarViz);
 gRegIni.WriteBool ('', cRegToolBarViz, PB_Options.ToolBarViz);
 gRegIni.WriteBool ('', cRegStatusBar, PB_Options.StatusBarViz);
end;

//-----------------------------------------------------------------------------
procedure LoadFromRegistry;
var i: Integer;
begin
 PB_Options.RootPath:= gRegIni.ReadString ('', cRegPalRootPath, '');
 PB_Options.Language:= gRegIni.ReadString ('', cRegLanguage, 'EN');
 PB_Options.Open:= gRegIni.ReadBool ('', cRegPalOpen, True);
 PB_Options.Floating:= gRegIni.ReadBool ('', cRegPalFloating, True);
 PB_options.Left:= gRegIni.ReadInteger ('', cRegPalLeft, 200);
 PB_Options.Top:=  gRegIni.ReadInteger ('', cRegPalTop, 200);
 PB_Options.Width:= gRegIni.ReadInteger ('', cRegPalWidth, 280);
 PB_Options.Height:= gRegIni.ReadInteger ('', cRegPalHeight, 640);
 PB_Options.Menu:= gRegIni.ReadBool ('', cRegPalMenu, True);
 PB_Options.TextSize:= TTextsize (gRegIni.ReadInteger ('', cRegPalTextsize, Ord(tsMedium)));
 PB_Options.ShowMode:= TPalShowmode (gRegIni.ReadInteger ('', cRegPalShowIcons, Ord(smBoth)));
 PB_Options.Fillmode:= TFillmode (gRegIni.ReadInteger('', cRegFillmode, Ord(fmActTab)));
 PB_Options.Hottrack:= gRegIni.ReadBool ('', cRegPalHotTrack, True);
 PB_options.ButtonPos:= TAlign (gRegIni.ReadInteger('', cRegPalBtnPos, Ord(alRight)));
 i:= gRegIni.ReadInteger ('', cRegPalButtons, BitsToInt([Bit0..Bit15]));
 PB_Options.Buttons:= ValTo32Bits (i);
 PB_options.A2ZPos:= TAlign (gRegIni.ReadInteger('', cRegAZBarPos, Ord(alRight)));
 PB_Options.A2ZFilter:= gRegIni.ReadBool ('', cRegAZBarFilter, True);
 PB_Options.A2ZViz:= gRegIni.ReadBool ('', cRegAZBarShow, True);
 PB_Options.SaveHistory:= gRegIni.ReadBool ('', cRegSaveHistory, True);
 PB_Options.SearchBar:= TAlign (gRegIni.ReadInteger('', cRegSearchBar, Ord(alBottom)));
 PB_Options.SearchBarViz:= gRegIni.ReadBool ('', cRegSearchBarViz, True);
 PB_Options.ToolBarViz:= gRegIni.ReadBool ('', cRegToolBarViz, True);
 PB_Options.StatusBarViz:= gRegIni.ReadBool ('', cRegStatusBar, True);
end;

//-----------------------------------------------------------------------------
procedure LoadLangTypes;
begin
 if not Assigned (gLangList) then  gLangList:= TStringList.Create;
 if Assigned (gLangList) then
  gLangList.LoadFromFile(PB_Options.RootPath + '\palettebar.lng');
end;

//-----------------------------------------------------------------------------
procedure LoadLanguage (const lang: string);
begin
 PB_Options.Language:= lang;
 if not Assigned (gLangText) then  gLangText:= TStringList.Create;
 if Assigned (gLangText) then
  gLangText.LoadFromFile(PB_Options.RootPath + '\palettebar.' + lang);
end;

//-----------------------------------------------------------------------------
function GetLangStr (txt: TLanguageText): string;
begin
 Result:= '--Error finding text --';
 if Assigned (gLangText) and (Ord(txt) < gLangText.Count) then Result:= gLangText[Ord(txt)];
end;

//------------------------------------------------------------------------------
// Set-Handling (from the unit Bitsets)
//------------------------------------------------------------------------------

function ValTo8Bits  (CONST i8): t8Bits;
var b: t8Bits absolute i8;
begin
 Result:= b;
end;

//-----------------------------------------------------------------------------
function ValTo16Bits (CONST i16): t16Bits;
var b: t16Bits absolute i16;
begin
 Result:= b;
end;

//-----------------------------------------------------------------------------
function ValTo32Bits (CONST i32): t32Bits;
var b: t32Bits absolute i32;
begin
 Result:= b;
end;

//-----------------------------------------------------------------------------
function ValTo64Bits (CONST i64): t64Bits;
var b: t64Bits absolute i64;
begin
 Result:= b;
end;

//-----------------------------------------------------------------------------
function ValTo128Bits (CONST i128): t128Bits;
var b: t128Bits absolute i128;
begin
 Result:= b;
end;

//-----------------------------------------------------------------------------
function ValTo256Bits (CONST i256): t256Bits;
var b: t256Bits absolute i256;
begin
 Result:= b;
end;

//-----------------------------------------------------------------------------
function BitsToByte (CONST t8: t8Bits): Byte;
var b: Byte absolute t8;
begin
 Result:= b;
end;

//-----------------------------------------------------------------------------
function BitsToWord (CONST t16: t16Bits): Word;
var b: Word absolute t16;
begin
 Result:= b;
end;

//-----------------------------------------------------------------------------
function BitsToInt (CONST t32: t32Bits): Integer;
var b: Integer absolute t32;
begin
 Result:= b;
end;

//-----------------------------------------------------------------------------
function BitsToInt64 (CONST t64: t64Bits): Int64;
var b: int64 absolute t64;
begin
 Result:= b;
end;

//-----------------------------------------------------------------------------
function BitsToInt128 (CONST t128: t128Bits): tInt128;
var b: tInt128 absolute t128;
begin
 Result:= b;
end;

//-----------------------------------------------------------------------------
function BitsToInt256 (CONST t256: t256Bits): tInt256;
var b: tInt256 absolute t256;
begin
 Result:= b;
end;

//-----------------------------------------------------------------------------
function GetStrPart (var s: string; delimiter: CHAR): string;
var i: INTEGER;
begin
 Result:= '';
 if s <> '' then begin
  i:= Pos (delimiter, s);
  if i > 1 then begin
   Result:= Copy (s, 1, i - 1);
   System.Delete (s, 1, i);
   s:= TrimLeft (s);
  end else begin
   if s[1] = delimiter then  System.Delete (s, 1, 1);
  end;
 end;
end;

//-----------------------------------------------------------------------------
function StrIPos (const s1, s2: string; pos, max: Integer): Integer;
var s, l1, l2, j: Integer;
begin
 Result:= -1;
 l1:= Length (s1);
 l2:= Length (s2);
 if max <= 0 then  max:= l2;
 if pos <= 0 then  pos:= 0;
 if (l1 <= 0) or (pos > max) then  Exit;
 for s:= pos to (max - l1) do begin
  for j:= 1 to l2 do begin
   if (s1[j] <> s2[s+j]) and
      (not (s1[j] = '?')) then  BREAK;
   if (j = l1) then begin  Result:= s+1;  EXIT;  end;
  end;
 end;
end;

//-----------------------------------------------------------------------------
function StrIPosCI (const s1, s2: string; pos, max: Integer): Integer;
begin
 Result:= StrIPos (AnsiUpperCase(s1), AnsiUpperCase(s2), pos, max);
end;

//-----------------------------------------------------------------------------
function StrMatch  (const str1, str2: string; wild, joker: char): BOOLEAN;
var i, j, l1, l2: INTEGER;
begin
 StrMatch:= TRUE;
 l1:= Length (str1);
 l2:= Length (str2);
 i:= 1;
 j:= 1;
 repeat
  if (str1[i] = wild) then begin
   Inc (i);
   repeat
    Inc(j);
   until (i > l1) or (j > l2) or (str1[i] = str2[j]);
   if (i > l1) or (j > l2) then begin
    StrMatch:= False;  Exit;
   end;
  end;
  if (str1[i] <> str2[j]) and (str1[i] <> joker) then begin
   StrMatch:= False;  Exit;
  end;
  Inc (i);
  Inc (j);
 until (i > l1) or (j > l2);
end (*StrMatch*);

//-----------------------------------------------------------------------------
function StrMatchCI (const str1, str2: string; wild, joker: char): BOOLEAN;
begin
 Result:= StrMatch (AnsiUpperCase(str1), AnsiUpperCase(str2), wild, joker);
end (*StrMatchCI*);

//-----------------------------------------------------------------------------
procedure ListFree (var l: TList; Free: Boolean);
var i: Integer;
begin
 for i:= Pred (l.Count) downto 0 do
  if Assigned (l[i]) then Dispose (l[i]);
 if Free then l.Free else l.Clear;
end;

//-----------------------------------------------------------------------------
procedure FreeList (var l: TList);
begin
 ListFree (l, True);
end;

//-----------------------------------------------------------------------------
procedure ReleaseList (var l: TList);
begin
 ListFree (l, False);
end;

//-----------------------------------------------------------------------------
function DelShortcutChar (const s: string): string;
var x: integer;
begin
 Result:= s;
 x:= Pos ('&', Result);
 if x > 0 then Delete (Result, x, 1);
end;

//-----------------------------------------------------------------------------
function FindMenu (menu: TMainMenu; const s: string): TMenuItem;
var i: Integer;
begin
 Result:= nil;
 for i:= 0 to Pred (menu.Items.Count) do begin
  if menu.Items[i].Name = s then begin
   Result:= menu.Items[i];
   Exit;
  end;
 end;
end;

//-----------------------------------------------------------------------------
function FindMenu (menu: TMenuItem; const s: string): TMenuItem;
var i: Integer;
begin
 Result:= nil;
 for i:= 0 to Pred (menu.Count) do begin
  if menu.Items[i].Name = s then begin
   Result:= menu.Items[i];
   Exit;
  end;
 end;
end;

//-----------------------------------------------------------------------------
function FindMenuInt (menu: TMainMenu; const s: string): Integer;
var i: Integer;
begin
 Result:= -1;
 for i:= 0 to Pred (menu.Items.Count) do begin
  if menu.Items[i].Name = s then begin
   Result:= i;
   Exit;
  end;
 end;
end;

//-----------------------------------------------------------------------------
function FindMenuInt (menu: TMenuItem; const s: string): Integer;
var i: Integer;
begin
 Result:= -1;
 for i:= 0 to Pred (menu.Count) do begin
  if menu.Items[i].Name = s then begin
   Result:= i;
   Exit;
  end;
 end;
end;

//-----------------------------------------------------------------------------
function Min (i1, i2: Integer): Integer;
begin
 if i1 < i2 then Min:= i1 else Min:= i2;
end (*Min*);

//-----------------------------------------------------------------------------
function Max (i1, i2: Integer): Integer;
begin
 if i1 < i2 then Max:= i2 else Max:= i1;
end (*Max*);

//-----------------------------------------------------------------------------
function TextLong(s: string): LONGINT;
var x: record
        case Integer of
         0: (li: Longint);
         1: (l, h: Word);
         2: (a, b, c, d: char);
       end;
begin
 x.a:= s[1]; x.b:= s[2]; x.c:= s[3]; x.d:= s[4];
 TextLong:= x.li;
end (*TextLong*);

//-----------------------------------------------------------------------------
procedure debug (const s: string);
var f: Textfile;
begin
 {$I-}
 AssignFile (f, cDebugFile);
 Append (f);
 if IOResult <> 0 then Rewrite (f);
 WriteLn (f, s);
 CloseFile (f);
 {$I+}
end;

end.
