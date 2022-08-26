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

uses Classes, Forms, Controls, Menus, Registry, ActnList, ComCtrls;

const cCompile =    '4.00-Beta';
      cDate =       '01.08.2004';
      cPaletteBar = 'PaletteBar';
      cProgName =   'PaletteBar ' + cCompile;
      cVersion =    cCompile + ' - ' + cDate;
      cWizardMenu = 'P&aletteBar';
      cWizardName = 'PaletteBarWizard';

const cDebugFile =  'D:\Borland\Projekte\PaletteBar\D7\pb_debug.txt';

var   DoTrace:      Boolean = True;
      TraceStr:     string;


//==============================================================================
// Set-Handling (from the unit Bitsets)
//==============================================================================

type TBit = (Bit0,   Bit1,   Bit2,   Bit3,   Bit4,   Bit5,   Bit6,   Bit7,
             Bit8,   Bit9,   Bit10,  Bit11,  Bit12,  Bit13,  Bit14,  Bit15,
             Bit16,  Bit17,  Bit18,  Bit19,  Bit20,  Bit21,  Bit22,  Bit23,
             Bit24,  Bit25,  Bit26,  Bit27,  Bit28,  Bit29,  Bit30,  Bit31);

type  T8Bits =   set of Bit0..Bit7;    // Byte    1
      T16Bits =  set of Bit0..Bit15;   // Word    2
      T32Bits =  set of Bit0..Bit31;   // Long    4

function ValTo8Bits   (const i8):  t8Bits;
function ValTo16Bits  (const i16): t16Bits;
function ValTo32Bits  (const i32): t32Bits;
// Wert in Bitset

function BitsToByte   (const t8:  t8Bits): Byte;
function BitsToWord   (const t16: t16Bits): Word;
function BitsToInt    (const t32: t32Bits): Integer;
// Bitset in Wert

//==============================================================================
// from the unit Basics
//==============================================================================

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

function FindMenu (menu: TMainMenu; const s: string): TMenuItem; overload;
function FindMenu (menu: TMenuItem; const s: string): TMenuItem; overload;
function FindMenuInt (menu: TMainMenu; const s: string): Integer; overload;
function FindMenuInt (menu: TMenuItem; const s: string): Integer; overload;
// Findet ein Menü anhand des Namens ( nicht (!) der caption!)

procedure RunApp (const what, para: string);
// Execute a Application

function TextLong (const s: string): LONGINT;

procedure Debug (const s: string); overload;
procedure Debug (const s: string; i: integer); overload;
procedure TraceIn (const s: string);
procedure TraceOut (const s: string);

//==============================================================================
// Delphi Registry-Eintrage
//==============================================================================

const cD7 =                'Delphi 7';
      cD6 =                'Delphi 6';
      cC6 =                'C++Builder 6';

//const cRegHelpFile =       'WinHelpPath';

//==============================================================================
// PaletteBar Registry-Eintrage
//==============================================================================

const cRegPalOpen =        'Open';
//      cRegMenu =           'Menu';
      cRegPalHotTrack =    'Hottrack';
      cRegPalShowIcons =   'Iconmode';
      cRegPalDisplay =     'Displaymode';
      cRegPalTextsize =    'Textsize';
      cRegPalMenu =        'Menu';
      cRegPalButtons =     'Buttons';
      cRegPalBtnPos =      'ButtonPos';
      cRegToolbarViz =     'ToolBarViz';
      cRegPalFloating =    'Floating';
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
      cRegEdExInfo =       'EditExInfo';
      cRegConfig =         'Config';
      cRegVCLCat =         'Categories\VCL';
      cRegCLXCat =         'Categories\CLX';
      cRegTimerVal =       'TimerVal';
      cRegActTab =         'ActTab';

//==============================================================================
// Namen von Delphi IDE-Komponenten
//==============================================================================

const cConfigMenu =        'ComponentMenu';
      cConfigItem =        'ComponentPaletteItem';
      cToolsMenu =         'ToolsMenu';
      cPalette =           'Palette';
//      cMenuBar =           'MenuBar';
//      cTabCaption =        'Tab';
      cPBMenu =            'pb_mnu_show';

const cDataModuleForm =    'DataModuleForm';
      cDataModuleSurface = 'DataModuleSurface';
      cWinControlForm =    'WinControlForm';
      cWidgetControlForm = 'WidgetControlForm';
      cSelectedIndex =     'SelectedIndex';
      cPalToolCount =      'PalToolCount';
      cSelToolName =       'SelectedToolName';


//==============================================================================
// Sprachkonstanten
//==============================================================================

const c3Dots =            '...';
      cTextError =        '--Error finding text --';

type  TLanguageText =     (ltLangName,        // Kürzel der Sprachversion
                           // Hauptmenü
                           ltActCategory,     // Aktuelle Kategorie
                           ltAllComps,        // Alle Komponenten
                           ltTreeview,        // Baumdarstellung
                           ltHistory,         // Zuletzt Benutzte
                           ltActForm,         // Aktuelles Formular
                           ltFavorites,       // Favoriten
                           ltShowExtInfo,     // Externe Information anzeigen
                           ltEnterExtInfo,    // Externe Information eingeben
                           ltShowHelp,        // Hilfe zur Komponente anzeigen
                           ltFavAdd,          // Zu Favoriten hinzufügen
                           ltFavRemove,       // Aus den Favoriten entfernen
                           ltCfgCategory,     // Kategorien anpassen
                           ltCfgPaletteBar,   // PaletteBar konfigurieren
                           ltAbout,           // Über PaletteBar
                           // About-Dialog
                           // ltAbout,           // Über PaletteBar
                           ltHomepage,        // Homepage im Inernet
                           ltUrl,             // http://delphi.helli.de/PaletteBar/palettebar.html
                           // Kategorien-Dialog
                           // ltCfgCategory,     // Kategorien anpassen
                           ltTabCaption,      // Bibliothek
                           ltNewEntry,        // Neue Kategorie
                           ltDefCategory,     // Sonstige
                           ltAddCaption,      // &Neu
                           ltInsCaption,      // &Einfügen
                           ltCopyCaption,     // &Kopieren
                           ltResetCaption,    // &Reset
                           ltUpCaption,       // Nach &oben
                           ltDownCaption,     // Nach &unten
                           ltRenameCaption,   // Um&benennen
                           ltDelCaption,      // &Löschen
                           ltOKCaption,       // &OK
                           ltCancelCaption,   // &Abbruch
                           // Konfig-Dialog-Dialog
                           // ltCfgPaletteBar,   // PaletteBar konfigurieren
                           ltDelete,          // Löschen
                           // ltHistory,         // Zuletzt Benutzte
                           ltSearchText,      // Suchtexte
                           // ltFavorites,       // Favoriten
                           // ltOKCaption,       // &OK
                           // ltCancelCaption,   // &Abbruch
                           ltCompList,        // Komponentenliste
                           ltPalView,         // Ansicht
                           ltPalViewKachel,   // Kacheln
                           ltPalViewSymbol,   // Symbole
                           ltPalViewList,     // Liste
                           ltPalViewText,     // Text
                           ltPalViewTree,     // Baum
                           ltPalText,         // Textgröße
                           ltSmall,           // klein
                           ltMedium,          // mittel
                           ltBig,             // groß
                           ltPalHottrack,     // Hottracking
                           ltToolbar,         // Werkzeugleiste
                           ltShowButtons,     // Buttons anzeigen
                           // ltActCategory,     // Aktuelle Kategorie
                           // ltAllComps,        // Alle Komponenten
                           // ltTreeview,        // Baumdarstellung
                           // ltHistory,         // Zuletzt Benutzte
                           // ltActForm,         // Aktuelles Formular
                           // ltFavorites,       // Favoriten
                           // ltShowExtInfo,     // Externe Information anzeigen
                           // ltEnterExtInfo,    // Externe Information eingeben
                           // ltShowHelp,        // Hilfe zur Komponente anzeigen
                           // ltFavAdd,          // Zu Favoriten hinzufügen
                           // ltFavRemove,       // Aus den Favoriten entfernen
                           // ltCfgCategory,     // Kategorien anpassen
                           // ltCfgPaletteBar,   // PaletteBar konfigurieren
                           // ltAbout,           // Über PaletteBar
                           ltToolbarPos,      // Position
                           ltTop,             // Oben
                           ltLeft,            // Links
                           ltRight,           // Rechts
                           ltVisible,         // sichtbar
                           ltShowToolbar,     // Toolbar anzeigen
                           ltA2ZBar,          // A-Z Leiste
                           ltFilterMode,      // Filtern
                           // ltLeft,            // Links
                           // ltRight,           // Rechts
                           // ltVisible,         // sichtbar
                           // ltHistory,         // Zuletzt Benutzte
                           ltSave,            // speichern
                           ltSearchbar,       // Suchleiste
                           // ltTop,             // Oben
                           ltBottom,          // Unten
                           // ltVisible,         // sichtbar
                           ltShowSeachbar,    // Suchleiste anzeigen
                           ltMenu,            // Menü
                           ltShow,            // anzeigen
                           ltStatusbar,       // Statuszeile
                           // ltShow,            // anzeigen
                           ltShowStatusbar,   // Statuszeile anzeigen
                           ltLanguage,        // Sprache
                           // ExtInfo-Dialog
                           // ltEnterExtInfo,    // Externe Information eingeben
                           // ltOKCaption,       // &OK
                           // ltCancelCaption,   // &Abbruch
                           // Sonstiges
                           ltSearchResult,    // Suchergebnis
                           ltYes,             // Ja
                           ltNo,              // Nein
                           ltForm,            // Formular
                           ltFilterd,         // Gefiltert
                           ltSaveHistory,     // History speichern
                           ltDeleteHistory,   // History löschen
                           ltAllfiles,        // Alle Dateien (*.*)|*.*
                           // Abfragen
                           ltDeleteReally,    // Wirklich löschen?
                           // Fehlermeldungen
                           ltErrorInsert,     // Fehler beim Einfügen der Komponente!
                           ltErrorNoForm,     // Kein aktuelles Formular
                           ltErrorCat,        // Kategorienname muss eindeutig sein!
                           // Ende der Strings
                           ltNone);


var   gLangList:    TstringList;
      gLangText:    TStringList;
      gRegIni:      TRegIniFile;

//==============================================================================
// Komponentenliste
//==============================================================================
const pcHistory =  Bit0;
      pcFavorite = Bit1;
      pcChecked =  Bit2;
      pcInList =   Bit3;

type PtrComponent =   ^TComponentItem;
     TComponentItem = record
                       typename:   string;
                       tabname:    string;
                       extinfo:    string;
                       imgindex:   Integer;
                       flags:      T32Bits;
                      end;

//==============================================================================
// Konfigurationsparamenter
//==============================================================================

const cRootCat =    0;
      cMetaCat =    1;
      cCategory =   2;
      cUnknown =    3;
      cFrame =      4;
      cChecked =    22;

type  TTextsize =     (tsSmall, tsMedium, tsBig);

type  TDisplayMode =  (dmList,
                       dmTree,
                       dmNone);

type  TPalShowMode =  (smKachel,
                       smSymbol,
                       smList,
                       smText,
                       smNone);

type TFillmode =      (fmActTab,
                       fmAllTabs,
                       fmFavorites,
                       fmHistory,
                       fmActForm,
                       fmFilter,
                       fmSearch,
                       fmNone);

type TSenderType =    (stCaption,
                       stName,
                       stTag,
                       stNone);

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
                       ActTab:       string;
                       LibName:      string;
                       Open:         Boolean;
                       Floating:     Boolean;
                       Menu:         Boolean;
                       TextSize:     TTextSize;
                       DisplayMode:  TDisplayMode;
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
                       TimerVal:     Integer;
                       BuildMainMnu: Boolean;
                       BuildTabMnu:  Boolean;
                       Init:         Boolean;
                       GenCompList:  Boolean;
                       FreshUp:      Boolean;
                      end;

var PB_Options: pb_OptionType;
    oldopt:     pb_OptionType;

//function OptionsChanged: Boolean;

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

procedure Delay (ms: Cardinal);
// Wartet etwas

procedure GetSender (Sender: TObject; kind: TSenderType; var s: string); overload;
procedure GetSender (Sender: TObject; kind: TSenderType; var i: Integer); overload;
//procedure GetCategories (cat: TStrings);
//procedure GetCategory (const TabName: string; cat: TStrings);

// Formpositionen setzen und speichern
procedure SetFormPos (Form: TForm; const reg: string);
procedure SaveFormPos (Form: TForm; const reg: string);

// Treeview Standardfunktionen
procedure ReadNodes  (tv: TTreeView; Parent: TTreenode; const section: string);
procedure WriteNodes (node: TTreeNode; const s: string; org: TStrings);
function  GetTreeNode (tv: TTreeView; split: Boolean; const s: string): TTreeNode;
procedure SetNodeImage (Node: TTreeNode; image: Integer);
procedure CopyNodes (tv: TTreeView; source, dest: TTreeNode);

//==============================================================================
implementation
//==============================================================================

uses Sysutils, ShellApi, Windows, Dialogs;


//-----------------------------------------------------------------------------
procedure SaveToRegistry;
var fm: TFillmode;
begin
 gRegIni.WriteBool ('', cRegPalOpen, PB_Options.Open);
 gRegIni.WriteBool ('', cRegPalFloating, PB_Options.Floating);

 gRegIni.WriteString  ('', cRegLanguage, PB_Options.Language);
 gRegIni.WriteString  ('', cRegActTab, PB_Options.ActTab);

 gRegIni.WriteBool    ('', cRegPalMenu, PB_Options.Menu);
 gRegIni.WriteInteger ('', cRegPalTextsize, Ord (PB_Options.TextSize));
 gRegIni.WriteInteger ('', cRegPalShowIcons, Ord (PB_Options.ShowMode));
 gRegIni.WriteInteger ('', cRegPalDisplay, Ord (PB_Options.DisplayMode));

 case PB_Options.Fillmode of
  fmActForm,
  fmFilter,
  fmSearch,
  fmNone:    fm:= fmNone else fm:= PB_Options.Fillmode;
 end;
 gRegIni.WriteInteger ('', cRegFillmode,     Ord (fm));

 gRegIni.WriteBool    ('', cRegPalHotTrack,  PB_Options.Hottrack);
 gRegIni.WriteInteger ('', cRegPalBtnPos,    Ord (PB_Options.ButtonPos));
 gRegIni.WriteInteger ('', cRegPalButtons,   BitsToInt (PB_Options.Buttons));

 gRegIni.WriteInteger ('', cRegAZBarPos,     Ord (PB_Options.A2ZPos));
 gRegIni.WriteBool    ('', cRegAZBarFilter,  PB_Options.A2ZFilter);
 gRegIni.WriteBool    ('', cRegAZBarShow,    PB_Options.A2ZViz);
 gRegIni.WriteBool    ('', cRegSaveHistory,  PB_Options.SaveHistory);
 gRegIni.WriteInteger ('', cRegSearchBar,    Ord(PB_Options.SearchBar));
 gRegIni.WriteBool    ('', cRegSearchBarViz, PB_Options.SearchBarViz);
 gRegIni.WriteBool    ('', cRegToolBarViz,   PB_Options.ToolBarViz);
 gRegIni.WriteBool    ('', cRegStatusBar,    PB_Options.StatusBarViz);
 gRegIni.WriteInteger ('', cRegTimerVal,     PB_Options.TimerVal);
end;

//-----------------------------------------------------------------------------
procedure LoadFromRegistry;
var i: Integer;
begin
 PB_Options.RootPath:= gRegIni.ReadString ('', cRegPalRootPath, '');
 PB_Options.Open:=     gRegIni.ReadBool   ('', cRegPalOpen, True);
 PB_Options.Floating:= gRegIni.ReadBool   ('', cRegPalFloating, True);

 PB_Options.Language:= gRegIni.ReadString ('', cRegLanguage, 'EN');
 PB_Options.ActTab:= gRegIni.ReadString ('', cRegActTab, 'Standard');
 PB_Options.Menu:=     gRegIni.ReadBool ('', cRegPalMenu, True);
 PB_Options.TextSize:= TTextsize (gRegIni.ReadInteger ('', cRegPalTextsize, Ord(tsMedium)));
 PB_Options.DisplayMode:= TDisplayMode (gRegIni.ReadInteger ('', cRegPalDisplay, Ord(dmList)));
 PB_Options.ShowMode:= TPalShowmode (gRegIni.ReadInteger ('', cRegPalShowIcons, Ord(smSymbol)));
 PB_Options.Fillmode:= TFillmode (gRegIni.ReadInteger('', cRegFillmode, Ord(fmActTab)));
 PB_Options.Hottrack:= gRegIni.ReadBool ('', cRegPalHotTrack, True);
 PB_options.ButtonPos:= TAlign (gRegIni.ReadInteger('', cRegPalBtnPos, Ord(alTop)));
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
 PB_Options.TimerVal:= gRegIni.ReadInteger ('', cRegTimerVal, 20);
 PB_Options.BuildMainMnu:= True;
 PB_Options.BuildTabMnu:= True;
 PB_Options.Init:= False;
 PB_Options.GenCompList:= True;
 PB_Options.FreshUp:= False;
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
 if Assigned (gLangText) and (Ord(txt) < gLangText.Count) then Result:= gLangText[Ord(txt)]
                                                          else Result:= cTextError;
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
 if (l1 <= 0) or (pos > max) then begin
  Exit;
 end;
 for s:= pos to (max - l1) do begin
  for j:= 1 to l2 do begin
   if (s1[j] <> s2[s+j]) and
      (not (s1[j] = '?')) then  BREAK;
   if (j = l1) then begin
    Result:= s+1;
    Exit;
   end;
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
 StrMatch:= True;
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
    StrMatch:= False;
    Exit;
   end;
  end;
  if (str1[i] <> str2[j]) and (str1[i] <> joker) then begin
   StrMatch:= False;
   Exit;
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

//------------------------------------------------------------------------------
function FindMenu (menu: TMenuItem; const s: string): TMenuItem;
var found: Boolean;

 function find (sub: TMenuItem): TMenuItem;
 var i: Integer;
 begin
  Result:= nil;
  for i:= 0 to Pred (sub.Count) do begin
   if s = StripHotKey (sub.Items[i].Caption) then begin
    Result:= sub.Items[i];
    found:= True;
    Exit;
   end;
   if sub.Items[i].Count > 0 then begin
    Result:= find (sub.Items[i]);
    if found then Exit;
   end;
  end;
 end;

begin
 found:= False;
 Result:= find (menu);
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
function TextLong (const s: string): LONGINT;
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
procedure Debug (const s: string);
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

//-----------------------------------------------------------------------------
procedure Debug (const s: string; i: Integer);
var f: Textfile;
begin
 {$I-}
 AssignFile (f, cDebugFile);
 Append (f);
 if IOResult <> 0 then Rewrite (f);
 WriteLn (f, s + IntToStr (i));
 CloseFile (f);
 {$I+}
end;

//-----------------------------------------------------------------------------
procedure TraceIn (const s: string);
begin
 if DoTrace then begin
  TraceStr:= TraceStr + ' ';
  Debug (TraceStr + '> ' + s);
 end;
end;

//-----------------------------------------------------------------------------
procedure TraceOut (const s: string);
begin
 if DoTrace then begin
  Debug (TraceStr + '< ' + s);
  Delete (TraceStr, Length (TraceStr), 1);
 end;
end;

//------------------------------------------------------------------------------
procedure GetSender(Sender: TObject; kind: TSenderType; var i: Integer);
begin
 i:= -1;
 if Sender is TMenuItem then begin
  case kind of
   stTag: i:= TMenuItem (Sender).Tag;
  end;
 end else if Sender is TCustomAction then begin
  case kind of
   stTag: i:= TCustomAction (Sender).Tag;
  end;
 end;
end;

//------------------------------------------------------------------------------
procedure GetSender(Sender: TObject; kind: TSenderType; var s: string);
begin
 s:= '';
 if Sender is TMenuItem then begin
  case kind of
   stCaption: s:= TMenuItem (Sender).Caption;
   stName:    s:= TMenuItem (Sender).Name;
   stTag:     s:= IntToStr (TMenuItem (Sender).Tag);
  end;
 end else if Sender is TCustomAction then begin
  case kind of
   stCaption: s:= TCustomAction (Sender).Caption;
   stName:    s:= TCustomAction (Sender).Name;
   stTag:     s:= IntToStr (TCustomAction (Sender).Tag);
  end;
 end;
end;

//------------------------------------------------------------------------------
procedure ReadNodes (tv: TTreeView; Parent: TTreenode; const section: string);
var i, j: Integer;
    s, n, x: string;
    s1, s2: TStringList;
    t1, t2: TTreeNode;
    r: TRegIniFile;
begin
 s1:= TStringList.Create;
 s2:= TStringList.Create;
 r:= TRegIniFile.Create(cRegKey + '\' + section);
 try
  s1.Clear;
  s1.Sorted:= False;
  r.ReadSections (s1);
  for i:= 0 to Pred (s1.Count) do begin
   s:= r.ReadString(s1[i], '', '');
   n:= s1[i];
   x:= GetStrPart (n, #254);
   if s <> '' then t1:= tv.Items.AddChild (Parent, n + #254 + s)
              else t1:= tv.Items.AddChild (Parent, n);
   SetNodeImage (t1, cCategory);
   s2.Clear;
   s1.Sorted:= False;
   r.ReadSection(s1[i], s2);
   for j:= 0 to Pred (s2.Count) do begin
    if s2[j] <> '' then begin
     s:= r.ReadString(s1[i], s2[j], '');
     t2:= tv.Items.AddChild (t1, s2[j] + #254 + s);
     SetNodeImage (t2, cUnKnown);
    end;
   end;
   if r.HasSubKeys then ReadNodes (tv, t1, section + '\' + s1[i]);
  end;
 finally
  s2.Free;
  s1.Free;
  r.Free;
 end;
end;

//------------------------------------------------------------------------------
procedure WriteNodes (node: TTreeNode; const s: string; org: TStrings);
// node = zu untersuchender Knoten
// s =    Pfad in die Registry
// org =  Liste der Delphi-Tabs
var n: TTreeNode;
    a, b: string;
    pc: PtrComponent;

 function Num (i: Integer): string;
 // Erzeugt vierstellige Nummer
 begin
  Result:= IntToStr(i);
  while Length (Result) < 4 do Insert ('0', Result, 1);
 end;

begin
 if Assigned (node) then begin

  // String erzeugen nach Muster \vcl\0001¦Standard für Hauptkategorie
  a:= s + '\' + Num (Node.Index+1) + #254 + Node.Text;

  // Im Node.StateIndex ist der aktuelle Index der Delphitabs gespeichert
  // Namen der Delphitab wird als Standard-Wert eingetragen
  if Node.StateIndex >= 0 then b:= org[Node.StateIndex] else b:= '';

  // Wir haben eine Kategorie oder Metakategorie
  // Kategorie = Standarad-Wert <> ''
  // Metakategoerie = Standard-Wert = ''
  if Node.ImageIndex < cUnknown then begin
   gRegIni.WriteString (a, '', b);
  end else begin
   // Daten einer Komponente
   pc:= Node.Data;
   gRegIni.WriteString (s, pc.typename, pc.extinfo);
  end;

  // Kinder untersuchen...
  n:= node.GetFirstChild;
  if Assigned (n) then  WriteNodes (n, a, org);

  // auf gleicher Ebene weitermachen
  n:= Node.getNextSibling;
  if Assigned (n) then  WriteNodes (n, s, org);
 end;
end;

//------------------------------------------------------------------------------
function GetTreeNode (tv: TTreeView; split: Boolean; const s: string): TTreeNode;
var i: Integer;
    x: string;
begin
 Result:= nil;
 if Assigned (tv) then begin
  for i:= 0 to Pred (tv.Items.Count) do begin
   x:= tv.Items[i].Text;
   if split then GetStrPart (x, #254);
   if AnsiSameText (x, s) then begin
    Result:= tv.Items[i];
    Exit;
   end;
  end;
 end;
end;

//------------------------------------------------------------------------------
procedure SetNodeImage (Node: TTreeNode; image: Integer);
begin
 if Assigned (Node) then begin
  Node.ImageIndex:= image;
  Node.SelectedIndex:= image;
 end;
end;

//------------------------------------------------------------------------------
procedure CopyNodes (tv: TTreeView; Source, dest: TTreeNode);
var st, qt: TTreeNode;
begin
 if Assigned (source) then begin
  st:= Source.getFirstChild;
  repeat
   qt:= tv.Items.AddChild (dest, st.Text);
   qt.Data:= st.Data;
   SetNodeImage (qt, st.ImageIndex);
   if st.HasChildren then CopyNodes (tv, st, qt);
   st:= st.getNextSibling;
  until st = nil;
 end;
end;

//------------------------------------------------------------------------------
procedure SetFormPos (Form: TForm; const reg: string);
var s: string;
    x: Integer;
begin
 s:= gRegIni.ReadString (reg, '', '-1,-1,-1,-1');
 x:= StrToInt (GetStrPart (s, ','));
 if x >= 0 then begin
  Form.Position:= poDesigned;
  Form.Left:= x;
  Form.Top:= StrToInt (GetStrPart (s, ','));
  Form.Width:= StrToInt (GetStrPart (s, ','));
  Form.Height:= StrToInt (s);
 end;
end;

//------------------------------------------------------------------------------
procedure SaveFormPos (Form: TForm; const reg: string);
begin
 gRegIni.WriteString (reg, '', IntToStr (Form.Left) + ',' +
                               IntToStr (Form.Top) + ',' +
                               IntToStr (Form.Width) + ',' +
                               IntToStr (Form.Height));
end;

//------------------------------------------------------------------------------
procedure Delay (ms: Cardinal);
begin
 while GetTickCount < (GetTickCount + ms) do Application.ProcessMessages;
end;

//------------------------------------------------------------------------------
procedure RunApp (const what, para: string);
var p: PCHAR;
begin
 if para <> '' then p:= PCHAR (para) else p:= nil;
 ShellExecute (Application.Handle, 'Open', PChar (what), p, nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------
end.
