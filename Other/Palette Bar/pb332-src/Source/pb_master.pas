//----------------------------------------------------------------------------
// Unit Name: pb_master
// Author:    Helli
// Date:      16.07.2003
// Purpose:
// History:
//----------------------------------------------------------------------------
//  Copyright © 2003 by Hellinger Software.  All Rights Reserved.
//----------------------------------------------------------------------------

unit pb_master;

interface

{$ifdef ClxVersion}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Buttons,
  QControls, QForms,
  Controls, Forms,
  DockForm, DeskForm, DeskUtil,
  Dialogs, ActnList, Menus, ComCtrls, ToolWin, StdCtrls, ExtCtrls,
  ImgList, ToolsAPI, DesignIntf, IniFiles,
  pb_common;
{$endif}
{$ifndef ClxVersion}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Buttons,
  Controls, Forms,
  DockForm, DeskForm, DeskUtil,
  Dialogs, ActnList, Menus, ComCtrls, ToolWin, StdCtrls, ExtCtrls,
  ImgList, ToolsAPI, DesignIntf, IniFiles,
  pb_common;
{$endif}

type TDragging =      (dmClick,
                       dmDrag,
                       dmNone);

type TRegMode =       (rmHistory,
                       rmFavorites,
                       rmSearch,
                       rmNone);

type TPalSorting =    (psAZ,
                       psZA,
                       psNone);

type TSenderType =    (stCaption,
                       stName,
                       stTag,
                       stNone);

type PtrComponent=    ^TComponentItem;
     TComponentItem = record
                       compname: string;
                       typename: string;
                       imgindex: Integer;
                       history:  Boolean;
                       favorite: Boolean;
                      end;


type
  TPBar = class(TDockableForm)
    ToolBar: TToolBar;
    MainImages: TImageList;
    ImageList: TImageList;
    ActionList: TActionList;
    Panel1: TPanel;
    SearchPanel: TPanel;
    BtnCategory: TToolButton;
    BtnActTab: TToolButton;
    BtnAllComps: TToolButton;
    BtnActForm: TToolButton;
    BtnHistory: TToolButton;
    BtnFavorites: TToolButton;
    BtnEditCat: TToolButton;
    BtnConfPalette: TToolButton;
    BtnConfPB: TToolButton;
    BtnHelp: TToolButton;
    BtnAbout: TToolButton;
    Header: THeaderControl;
    aShowMenu: TAction;
    aActualTab: TAction;
    aAllComponents: TAction;
    aActualForm: TAction;
    aHistory: TAction;
    aFavorites: TAction;
    aEditCategory: TAction;
    aConfigPalette: TAction;
    aConfPaletteBar: TAction;
    aHelp: TAction;
    aAbout: TAction;
    aFavAdd: TAction;
    aFavRemove: TAction;
    aShowPaletteBar: TAction;
    Liste1: TTreeView;
    Liste2: TTreeView;
    TabMenu: TPopupMenu;
    TabMenuTearLine: TMenuItem;
    aActualForm1: TMenuItem;
    aActualTab1: TMenuItem;
    aAllComponents1: TMenuItem;
    aHistory1: TMenuItem;
    aFavAdd1: TMenuItem;
    N1: TMenuItem;
    aEditCategory1: TMenuItem;
    aConfigPalette1: TMenuItem;
    aConfPaletteBar1: TMenuItem;
    N2: TMenuItem;
    aHelp1: TMenuItem;
    N3: TMenuItem;
    aAbout1: TMenuItem;
    FavMenu: TPopupMenu;
    aFavAdd2: TMenuItem;
    aFavRemove1: TMenuItem;
    N4: TMenuItem;
    aHelp2: TMenuItem;
    CfgBtn: TPopupMenu;
    CfgBtnShow: TMenuItem;
    CfgBtnPos: TMenuItem;
    CfgPal: TPopupMenu;
    CfgBtn1: TMenuItem;
    CfgBtn2: TMenuItem;
    CfgBtn3: TMenuItem;
    CfgBtn4: TMenuItem;
    CfgBtn5: TMenuItem;
    CfgBtn6: TMenuItem;
    CfgBtn7: TMenuItem;
    CfgBtn8: TMenuItem;
    CfgBtn9: TMenuItem;
    CfgBtn10: TMenuItem;
    CfgBtnTop: TMenuItem;
    CfgBtnLeft: TMenuItem;
    CfgBtnRight: TMenuItem;
    CfgPalShow: TMenuItem;
    CfgPalText: TMenuItem;
    CfgPalHottrack: TMenuItem;
    CfgPalShowIcon: TMenuItem;
    CfgPalShowText: TMenuItem;
    CfgPalShowBoth: TMenuItem;
    CfgPalTextBig: TMenuItem;
    CfgPalTextMiddle: TMenuItem;
    CfgPalTextSmall: TMenuItem;
    CfgA2Z: TPopupMenu;
    CfgHist: TPopupMenu;
    CfgSearch: TPopupMenu;
    CfgA2ZFilter: TMenuItem;
    CfgA2ZLeft: TMenuItem;
    CfgA2ZRight: TMenuItem;
    CfgA2ZShow: TMenuItem;
    CfgHistSave: TMenuItem;
    CfgHistClear: TMenuItem;
    CfgSearchTop: TMenuItem;
    CfgSearchBottom: TMenuItem;
    CfgSearchShow: TMenuItem;
    A2ZBar: TPanel;
    StatusBar: TStatusBar;
    Button1: TSpeedButton;
    Button2: TSpeedButton;
    Button3: TSpeedButton;
    Button4: TSpeedButton;
    Button5: TSpeedButton;
    Button6: TSpeedButton;
    Button7: TSpeedButton;
    Button8: TSpeedButton;
    Button9: TSpeedButton;
    Button10: TSpeedButton;
    Button11: TSpeedButton;
    Button12: TSpeedButton;
    Button13: TSpeedButton;
    Button14: TSpeedButton;
    Button15: TSpeedButton;
    Button16: TSpeedButton;
    Button17: TSpeedButton;
    Button18: TSpeedButton;
    Button19: TSpeedButton;
    Button20: TSpeedButton;
    Button21: TSpeedButton;
    Button22: TSpeedButton;
    Button23: TSpeedButton;
    Button24: TSpeedButton;
    Button25: TSpeedButton;
    Button26: TSpeedButton;
    Edit: TComboBox;
    BtnGo: TButton;
    CfgBtnViz: TMenuItem;
    CfgBtn11: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure HeaderSectionClick(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure A2ZClick(Sender: TObject);
    procedure Liste1Click(Sender: TObject);
    procedure Liste1Compare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure Liste1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure Liste1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Liste1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Liste1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnGoClick(Sender: TObject);
    procedure TabMenuPopup(Sender: TObject);
    procedure CfgBtn10Click(Sender: TObject);
    procedure CfgBtnRightClick(Sender: TObject);
    procedure CfgPalShowBothClick(Sender: TObject);
    procedure CfgPalTextSmallClick(Sender: TObject);
    procedure CfgPalHottrackClick(Sender: TObject);
    procedure CfgA2ZShowClick(Sender: TObject);
    procedure CfgHistSaveClick(Sender: TObject);
    procedure CfgHistClearClick(Sender: TObject);
    procedure CfgSearchBottomClick(Sender: TObject);
    procedure CfgSearchShowClick(Sender: TObject);
    procedure aShowMenuExecute(Sender: TObject);
    procedure aActualTabExecute(Sender: TObject);
    procedure aAllComponentsExecute(Sender: TObject);
    procedure aActualFormExecute(Sender: TObject);
    procedure aHistoryExecute(Sender: TObject);
    procedure aFavoritesExecute(Sender: TObject);
    procedure aEditCategoryExecute(Sender: TObject);
    procedure aConfigPaletteExecute(Sender: TObject);
    procedure aConfPaletteBarExecute(Sender: TObject);
    procedure aHelpExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
    procedure aFavAddExecute(Sender: TObject);
    procedure aFavRemoveExecute(Sender: TObject);
    procedure aShowPaletteBarExecute(Sender: TObject);
    procedure CfgBtnVizClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private { Private-Deklarationen }
    FDelphiMenu: TMainMenu;
    FTabControl: TTabControl;
    FComponentBar: TWinControl;
    FComponentMenu: TMenuItem;
    FConfigItem: TMenuItem;
    FTabChange: TNotifyEvent;
    FPackageCount: Integer;
    FFilter: char;
    FSortAsc: Boolean;
    FPalSorting: TPalSorting;
    FListe: TTreeView;
    FContext: TTreeNode;
    FPBMenu: TMenuItem;
    FComponentList: TList;
    FTabs: TStrings;
    FActTab: string;
    FDragging: TDragging;
    FFormDesigner: IDesigner;
    FOpen: Boolean;

    procedure OnTabChange(Sender: TObject);
    procedure ConfigPalette;
    function  GetActiveTab: string;
    procedure SwitchToTab (const TabName: string);
    procedure BuildCatMenu (menu: TMenuItem; mm: Boolean; p: Integer; click: TNotifyEvent);
    procedure MenuClick(Sender: TObject);
    function  GetNumbers: string;
    procedure GetSender (Sender: TObject; kind: TSenderType; var s: string); overload;
    procedure GetSender (Sender: TObject; kind: TSenderType; var i: Integer); overload;
    procedure GenComponentlist;
    function  GetComponent (const Name: string): PtrComponent;
    function  GetActComponent: PtrComponent;
    function  GetActiveFormEditor: IOTAFormEditor;
    function  GetActiveFormDesigner: IDesigner;
    procedure SetTabButtons;
    procedure LoadList (mode: TRegMode);
    procedure SetTextSize;
    procedure SetA2Z (i: Integer);
    procedure ResetA2Z;
    procedure SetListView;
    procedure SetSearchbar (i: Integer);
    procedure SetSearchbarViz (b: Boolean);
    function  IsTarget (const s: string): Boolean;
    procedure TabMenuClickHandler (Sender: TObject);
    procedure AddListItem (const s2, s3: string; check: boolean);
    procedure FillActTab;
    procedure FillAllTabs (search: Boolean);
    procedure FillHistFav (hist: Boolean);
    procedure FillActForm;
    procedure FillCompList (mode: TFillmode);
    procedure UpdateControls;
    procedure UpdateLanguage;
    procedure UpdateOptions;
    procedure UpdateMenu;
  public  { Public-Deklarationen }
    procedure ClearList(mode: TRegMode);
    procedure GetCategories (cat: TStrings);
    function  GetTabs: TStrings;
    procedure GetCategory (const TabName: string; cat: TStrings);
    procedure LoadWindowState (Desktop: TMemIniFile); override;
    procedure SaveWindowState (Desktop: TMemIniFile; isProject: Boolean); override;
  end;

var PBar: TPBar;

procedure Register;

implementation

{$R *.dfm}

uses Registry,
     pb_about, pb_config, pb_categories;


const cIconUnsort = 14;
      cIconAZ =     15;
      cIconZA =     16;

var   RegKeyChecked: Boolean;


//------------------------------------------------------------------------------
procedure Register;
var s: string;
begin
 PBar:= nil;
 RegKeyChecked:= False;
 if Application.Title = cD7 then begin
  s:= cD7;  cBaseKey:= 'Software\Borland\Delphi\7.0'; IsD7:= True;
 end;
 if Application.Title = cD6 then begin
  s:= cD6;  cBaseKey:= 'Software\Borland\Delphi\6.0'; IsD6:= True;
 end;
 if Application.Title = cC6 then begin
  s:= cC6;  cBaseKey:= 'Software\Borland\C++Builder\6.0'; IsC6:= True;
 end;
{$ifdef ClxVersion}
 cStatusBarText:= 'PaletteBar for ' + s + ' (c) by Helli';
{$endif}
{$ifndef ClxVersion}
 cStatusBarText:= 'PaletteBar for ' + s + 'PE (c) by Helli';
{$endif}

 cRegKey:=        cBaseKey + '\PaletteBarWizard';
 cRegPalette:=    cBaseKey + '\Palette';
 cRegHelp:=       cBaseKey + '\Help';

 // Optionen laden
 gRegIni:= TRegIniFile.Create (cRegKey);
 LoadFromRegistry;
 LoadLangTypes;
 LoadLanguage (PB_Options.Language);

 if Assigned (RegisterFieldAddress) then begin
  RegisterFieldAddress (cWizardName, Addr(PBar));
  RegisterDesktopFormClass (TPBar, cWizardName, cWizardName);
 end;

 if not Assigned (PBar) then PBar:= TPBar.Create (Application);

end;


//------------------------------------------------------------------------------
// Allgemeine Routinen
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
function TPBar.GetNumbers: string;
begin
 Result:= '  (' + IntToStr (FListe.Items.Count) + ' / ' + IntToStr (FComponentList.Count+1) + ')';
end;

//------------------------------------------------------------------------------
function TPBar.GetTabs: TStrings;
begin
 Result:= FTabControl.Tabs;
end;

//------------------------------------------------------------------------------
procedure TPBar.GetCategory (const TabName: string; cat: TStrings);
var s1, s2:string;
    r: TRegIniFile;

 procedure AddStr (const s: string);
 begin
  if cat.IndexOf(s) < 0 then cat.Add(s);
 end;

begin
 if Assigned (cat) then begin
  cat.Clear;
  r:= TRegIniFile.Create (cRegKey);
  try
   s1:= r.ReadString (cRegCategory, TabName, '');
  finally
   r.Free;
  end;
  if s1 <> '' then begin
   repeat
    s2:= GetStrPart (s1, ',');
    if s2 <> '' then AddStr (s2);
   until s2 = '';
   AddStr (s1);
  end;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.GetCategories (cat: TStrings);
var sl: TStringList;
    s1, s2: string;
    i: Integer;
    r: TRegIniFile;

 procedure AddStr (const s: string);
 begin
  if cat.IndexOf(s) < 0 then cat.Add(s);
 end;

begin
 if Assigned (cat) then begin
  sl:= TStringList.Create;
  try
   r:= TRegIniFile.Create (cRegKey);
   try
    r.ReadSectionValues(cRegCategory, sl);
   finally
    r.Free;
   end;
   for i:= 0 to Pred (sl.Count) do begin
    s1:= sl.Values[sl.Names[i]];
    repeat
     s2:= GetStrPart (s1, ',');
     if s2 <> '' then AddStr (s2);
    until s2 = '';
    AddStr (s1);
   end;
  finally
   sl.Free;
  end;
 end;
end;

//------------------------------------------------------------------------------
function TPBar.GetActiveTab: string;
begin
 Result:= FTabControl.Tabs[FTabControl.TabIndex];
end;

//------------------------------------------------------------------------------
procedure TPBar.SwitchToTab (const TabName: string);
begin
 if not Assigned (FTabChange) then begin
  FTabChange:= FTabControl.OnChange;
  FTabControl.OnChange:= OnTabChange;
 end;
 FTabControl.TabIndex:= FTabControl.Tabs.IndexOf (TabName);
 FTabControl.OnChange (FTabControl);
end;

//------------------------------------------------------------------------------
procedure TPBar.BuildCatMenu(menu: TMenuItem; mm: Boolean;
  p: Integer; click: TNotifyEvent);
var i, j: Integer;
    tc: TMenuItem;
    sl, tabcat: TStringList;
    actcat: string;

 function AddItem (const caption: string): TMenuItem;
 begin
  Result:= TMenuItem.Create (Application);
  Result.Caption:= caption;
  Result.OnClick:= click;
  Result.Checked:= actcat = caption;
 end;

begin
 // Aktives Tab feststellen
 actcat:= GetActiveTab;

 // Das Menü füllen
 tabcat:= TStringList.Create;
 sl:= TStringList.Create;
 sl.Sorted:= True;
 try
  // Erst die Hauptkategorien füllen
  GetCategories (sl);
  sl.Add(GetLangStr (ltDefCategory));
  for i:= 0 to Pred (sl.Count) do begin
   tc:= TMenuItem.Create (Application);
   tc.Caption:= sl[i];
   if mm then menu.Add(tc)
         else menu.Insert(p, tc);
   Inc(p); // Hochzählen, sonst fügen wir verkehrt herum ein!
  end;
  sl.Clear; // Liste löschen
  sl.Assign (GetTabs);
  sl.Sorted:= True;
  for i:= 0 to Pred (sl.Count) do begin
   GetCategory (sl[i], tabcat);
   if tabcat.Count = 0 then  tabcat.Add(GetLangStr (ltDefCategory));
   for j:= 0 to Pred (tabcat.Count) do begin
    tc:= menu.Find (tabcat[j]);
    if Assigned (tc) then  tc.Add(AddItem(sl[i]));
   end;

  end;
 finally
  sl.Free;
  tabcat.Free;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.ConfigPalette;
begin
 if Assigned (FConfigItem) then FConfigItem.Action.Execute;
end;

//------------------------------------------------------------------------------
procedure TPBar.AddListItem (const s2, s3: string; check: boolean);
var tn: TTreeNode;
    pc: PtrComponent;
    s: string;
    i: Integer;
begin
 s:= s2;
 if Upcase(s[1]) = 'T' then Delete (s, 1, 1);
 pc:= GetComponent ('T' + s);
 if Assigned (pc) then begin
  if s3 <> '' then s:= s + '  (' + s3 + ')';
  if check then begin
   for i:= 0 to Pred (FListe.Items.Count) do begin
    if s = FListe.Items[i].Text then Exit;
   end;
  end;
  if PB_Options.ShowMode = smIcon then tn:= FListe.Items.Add (nil, '')
                                  else tn:= FListe.Items.Add (nil, s);
  if Assigned (tn) then begin
   tn.ImageIndex:= pc.imgindex;
   tn.SelectedIndex:= pc.imgindex;
   tn.Data:= pc;
  end;
 end;
end; // AddListItem

//------------------------------------------------------------------------------
procedure TPBar.FillActTab;
var r: TRegistry;
    s1, s2, s3: string;
begin
 FActTab:= GetActiveTab;
 FListe.Items.Clear;
 aActualTab.Hint:= GetLangStr (ltActCategory) + ' (' + FActTab + ')';
 r:= TRegistry.Create;
 try
  r.Rootkey:=HKEY_CURRENT_USER;
  if r.OpenKey (cRegPalette, False) then begin
   s1:= r.ReadString (FActTab);
   while s1 <> '' do begin
    s2:= GetStrPart (s1, ';');
    if s2[1] <> 'Q' then begin
     s3:= GetStrPart (s2, '.');
     AddListItem (s2, s3, false);
    end;
   end;
   r.CloseKey;
  end;
 finally
  r.Free;
 end;
 Header.Sections[0].Text:= FActTab + GetNumbers;
 Header.Sections[0].ImageIndex:= cIconUnsort; // Nicht sortiert!
end;

//------------------------------------------------------------------------------
procedure TPBar.FillAllTabs (search: Boolean);
var r: TRegistry;
    sl: TStringList;
    i: Integer;
    s1, s2, s3: string;
begin
 FListe.Items.Clear;
 r:= TRegistry.Create;
 try
  r.Rootkey:=HKEY_CURRENT_USER;
  if r.OpenKey (cRegPalette, False) then begin
   sl:= TStringList.Create;
   r.GetValueNames (sl);
   for i:= 0 to Pred (sl.Count) do begin
    s1:= r.ReadString (sl[i]);
    while s1 <> '' do begin
     s2:= GetStrPart (s1, ';');
     if s2[1] <> 'Q' then begin
      s3:= GetStrPart (s2, '.');
      if search then begin
       if (StrIPosCI (Edit.Text, s2, 0, -1) > 0) then AddListItem (s2, s3, false);
      end else begin
       AddListItem (s2, s3, false);
      end;
     end;
    end; // while
   end; // for
   sl.Free;
   r.CloseKey;
  end;
 finally
  r.Free;
 end;
 Header.Sections[0].Text:= GetLangStr (ltAllComps) + GetNumbers;
end;

//------------------------------------------------------------------------------
procedure TPBar.FillHistFav (hist: Boolean);
var i: Integer;
    pc: PtrComponent;
begin
 FListe.Items.Clear;
 for i:= 0 to Pred (FComponentList.Count) do begin
  pc:= FComponentList[i];
  if Assigned (pc) then begin
   if hist and pc.history then AddListItem (pc.typename, '', false);
   if (not hist) and pc.favorite then AddListItem (pc.typename, '', false);
  end;
 end;
 Header.Sections[0].Text:= GetLangStr (ltHistory) + GetNumbers;
end;

//------------------------------------------------------------------------------
procedure TPBar.FillActForm;
var i: Integer;
    pc: PtrComponent;
begin
 FListe.Items.Clear;
 FFormDesigner:= GetActiveFormDesigner;
 if Assigned (FFormDesigner) then begin
  Header.Sections[0].Text:= GetLangStr (ltForm) + ' (' + FFormDesigner.Root.Name + ')';
  for i:= 0 to Pred (FFormDesigner.Root.ComponentCount) do begin
   pc:= GetComponent(FFormDesigner.Root.Components[i].ClassName);
   if Assigned (pc) then  AddListItem (pc.typename, '', True);
  end;
 end else begin
  Header.Sections[0].Text:= GetLangStr (ltErrorNoForm);
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.FillCompList(mode: TFillmode);
var i: Integer;
    r: TRegistry;
    sl: TStringList;
    s1: string;
    pc: PtrComponent;
    tmp: TList;
begin
 // Liste füllen
 GenComponentlist;

 // Checken, ob wir die Daten haben
 if not RegKeyChecked then begin
  r:= TRegistry.Create;
  try
   r.Rootkey:= HKEY_CURRENT_USER;
   FActTab:= GetActiveTab;
   if r.OpenKey (cRegPalette, False) then begin
    sl:= TStringList.Create;
    r.GetValueNames (sl);
    s1:= r.ReadString (FActTab);
    if (sl.Count < 1) or (s1 = '') then begin
     aConfigPalette.Execute;
     FillCompList (mode);
     sl.Free;
     r.CloseKey;
     r.Free;
     Screen.Cursor:= crDefault;
     Exit;
    end else begin
     RegKeyChecked:= True;
    end;
    sl.Free;
    r.CloseKey;
   end;
  finally
   r.Free;
  end;
 end;

 Screen.Cursor:= crAppStart;
 FListe.Items.BeginUpdate;

 if mode = fmFilter then begin
  case PB_Options.Fillmode of
   fmActTab:    FillActTab;
   fmAllTabs:   FillAllTabs (False);
   fmFavorites: FillHistFav (False);
   fmHistory:   FillHistFav (True);
   fmActForm:   FillActForm;
  end;
  tmp:= TList.Create;
  try
   for i:= 0 to pred (FListe.Items.Count) do begin
    pc:= Fliste.Items[i].Data;
    if Assigned (pc) then begin
     if UpCase (pc.compname[2]) = FFilter then tmp.Add(pc);
    end;
   end;
   FListe.Items.Clear;
   for i:= 0 to Pred (tmp.Count) do begin
    pc:= tmp[i];
    if Assigned (pc) then AddListItem (pc.typename, '', false);
   end;
   Header.Font.Style:= [fsItalic];
  finally
   tmp.Free;
  end;
  FPalsorting:= psAZ;
  HeaderSectionClick (Header, Header.Sections[0]);
  aFavAdd.Enabled:= True;
  aFavRemove.Enabled:= False;
 end else begin
  Header.Font.Style:= [];
  ResetA2Z;
  FPalsorting:= psAZ;
  aFavAdd.Enabled:= True;
  aFavRemove.Enabled:= False;

  case mode of
   fmActTab:    begin
                 FillActTab;
                 BtnActTab.Down:= True;
                end;
   fmAllTabs:   begin
                 FillAllTabs (False);
                 HeaderSectionClick (Header, Header.Sections[0]);
                 BtnAllComps.Down:= True;
                end;
   fmHistory,
   fmFavorites: begin
                 FillHistFav (mode = fmHistory);
                 HeaderSectionClick (Header, Header.Sections[0]);
                 BtnHistory.Down:= True;
                 aFavAdd.Enabled:= (mode = fmHistory);
                end;
   fmActForm:   begin
                 FillActForm;
                 HeaderSectionClick (Header, Header.Sections[0]);
                 BtnActForm.Down:= True;
                end;
   fmSearch:    begin
                 FillAllTabs (True);
                 FPalsorting:= psAZ;
                 Header.Sections[0].Text:= GetLangStr (ltSearchResult) + GetNumbers;
                 HeaderSectionClick (Header, Header.Sections[0]);
                end;
  end; // case
 end;
 
 if mode <> fmFilter then PB_Options.Fillmode:= mode;
 FListe.Items.EndUpdate;
 Screen.Cursor:= crDefault;
end;

//------------------------------------------------------------------------------
procedure TPBar.GenComponentlist;
var i, j, k, l, x, y: Integer;
    s: string;
    ps: IOTAPackageServices;
    PackageHandle: HMODULE;
    Bitmap: TBitmap;
    pc: PtrComponent;
    col: TColor;
begin
 // Komponentenliste erstellen
 ps:= BorlandIDEServices as IOTAPackageServices;

 if FPackageCount <> ps.PackageCount then begin // Liste komplett neu aufbauen

  FPackageCount:= ps.PackageCount;

  // Liste löschen
  ReleaseList (FComponentList);
  // images rausschmeissen, bis auf die defaults
  if ImageList.Count > 2 then begin
   for i:= Pred (ImageList.Count) downto 2 do  ImageList.Delete(i);
  end;

  // Sonderbehandlung für Frames
  New (pc);
  if Assigned (pc) then begin
   pc.compname:= 'TFrames';
   pc.typename:= 'Frames';
   pc.favorite:= False;
   pc.history:= False;
   pc.imgindex:= 1;
   FComponentList.Add (pc);
  end;

  Bitmap:= TBitmap.Create;

  for i:= 0 to Pred (ps.PackageCount) do begin

   Debug ('Package: ' + ps.PackageNames[i] + ' ' + IntToStr(ps.ComponentCount[i]));

   if ps.ComponentCount[i] > 0 then begin
    PackageHandle:= GetModuleHandle (PChar(Uppercase(ps.PackageNames[i] + '.bpl')));
    if PackageHandle <> 0 then begin
     for j:= 0 to Pred (ps.ComponentCount[i]) do begin
      if Assigned (Getclass (ps.ComponentNames[i, j])) then begin
       s:= ps.ComponentNames[i, j];
       if s[1] <> 'T' then Insert ('T', s, 1); // Für die die Ohne T am Anfang arbeiten
       if GetComponent (s) = nil then begin
        New (pc);
        if Assigned (pc) then begin
         pc.compname:= s;
         pc.typename:= ps.ComponentNames[i, j];
         pc.imgindex:= 0; // Default-Image für Compos ohne Symbol
         pc.history:= False;
         pc.favorite:= False;
         try
          Bitmap.LoadFromResourceName (PackageHandle, UpperCase(ps.ComponentNames[i, j]));
          if not Bitmap.Empty then begin
           col:= bitmap.Canvas.Pixels[0, bitmap.Height-1]; // Transparente Farbe holen
           x:= bitmap.Width; // Alte Breite und Höhe merken
           y:= bitmap.Height;
           bitmap.Width:= 28; // Neue Breite und Höhe setzen
           bitmap.Height:= 28;
           bitmap.Transparent:= true;
           for k:= 0 to 27 do begin // mit transparenter Farbe füllen
            for l:= x to 27 do bitmap.Canvas.Pixels[k, l]:= col;
            for l:= y to 27 do bitmap.Canvas.Pixels[l, k]:= col;
           end;
           // Und in die Imageliste eintragen
           pc.imgindex:= ImageList.AddMasked (bitmap, col);
          end;
         except
          // Defaultimage verwenden
         end;
         FComponentList.Add (pc); // Komponenteninfo merken
        end;
       end; // if GetComponent
      end; // if GetClass
     end; // for j
    end; // if packagehandle
   end; // if ps.ComponentCount[i]
  end; // for i
  Bitmap.Free;
 end;
end;

//------------------------------------------------------------------------------
function TPBar.GetActComponent: PtrComponent;
var tn: TTreeNode;
begin
 tn:= FListe.Selected;
 if Assigned (tn) then Result:= tn.Data
                  else Result:= nil;
end;

//------------------------------------------------------------------------------
function TPBar.GetActiveFormDesigner: IDesigner;
var FormEditor: IOTAFormEditor;
begin
 Result:= nil;
 FormEditor:= GetActiveFormEditor;
 if Assigned (FormEditor) then begin
  Result:= (FormEditor as INTAFormEditor).FormDesigner;
  // Nur wenn wir auch ein Form haben!!!
  if not Assigned (Result.Root) then Result:= nil;
 end;
end;

//------------------------------------------------------------------------------
function TPBar.GetActiveFormEditor: IOTAFormEditor;
var Module: IOTAModule;
    Editor: IOTAEditor;
    i: Integer;
begin
 Result:= nil;
 Module:= (BorlandIDEServices as IOTAModuleServices).CurrentModule;
 if Assigned (Module) then begin
  for i := 0 to Pred (Module.GetModuleFileCount) do begin
   Editor:= Module.GetModuleFileEditor(i);
   Editor.QueryInterface(IOTAFormEditor, Result);
   if Assigned (Result) then Break;
  end;
 end;
end;

//------------------------------------------------------------------------------
function TPBar.GetComponent(const Name: string): PtrComponent;
var i: Integer;
    pc: PtrComponent;
begin
 Result:= nil;
 for i:= 0 to Pred (FComponentList.Count) do begin
  pc:= FComponentList[i];
  if Assigned (pc) then begin
   if (pc.compname = Name) then begin
    Result:= pc;
    Exit;
   end;
  end;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.GetSender(Sender: TObject; kind: TSenderType;
  var i: Integer);
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
procedure TPBar.GetSender(Sender: TObject; kind: TSenderType;
  var s: string);
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
function TPBar.IsTarget(const s: string): Boolean;
// True wenn das Objekt zum Einfügen gültig ist
begin
 Result:= (s = FFormDesigner.Root.Name) or
          (s = cDataModuleForm) or
          (s = cDataModuleSurface) or
          (s = cWinControlForm) or
          (s = cWidgetControlForm);
end;

//------------------------------------------------------------------------------
procedure TPBar.LoadList(mode: TRegMode);
var i: Integer;
    p: PtrComponent;
    s: TStringList;
begin
 s:= TStringList.Create;
 try
  case mode of
   rmHistory:   gRegIni.ReadSection(cRegHistory, s);
   rmFavorites: gRegIni.ReadSection(cRegFavorites, s);
   rmSearch:    gRegIni.ReadSection(cRegSearchText, Edit.Items);
  end;
  if mode <> rmSearch then begin
   for i:= 0 to Pred (s.Count) do begin
    p:= GetComponent (s[i]);
    case mode of
     rmHistory:   if Assigned (p) then p.history:= True;
     rmFavorites: if Assigned (p) then p.favorite:= True;
    end;
   end;
  end;
 finally
  s.Free;
 end;
end;

//-----------------------------------------------------------------------------
procedure TPBar.ClearList(mode: TRegMode);
var p: PtrComponent;
    i: Integer;
begin
 case mode of
  rmHistory:   begin
                gRegIni.EraseSection (cRegHistory);
                for i:= 0 to Pred (FComponentList.Count) do begin
                 p:= FComponentList[i];
                 if Assigned (p) then  p.history:= False;
                end;
                if PB_Options.Fillmode = fmHistory then  FillCompList (fmHistory);
               end;
  rmFavorites: begin
                gRegIni.EraseSection (cRegFavorites);
                for i:= 0 to Pred (FComponentList.Count) do begin
                 p:= FComponentList[i];
                 if Assigned (p) then  p.favorite:= False;
                end;
                if PB_Options.Fillmode = fmFavorites then  FillCompList (fmFavorites);
               end;
  rmSearch:    begin
                gRegIni.EraseSection (cRegSearchText);
                Edit.Items.Clear;
               end;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.MenuClick(Sender: TObject);
var i: Integer;
    m: TMenuItem;
begin
 // Kategorien löschen
 m:= FPBMenu.Find('-');
// FindMenu (FPBMenu, cPBMenu);
 for i:= Pred (FPBMenu.IndexOf(m)) downto 0 do begin
  FPBMenu.Delete (i);
 end;
 BuildCatMenu (FPBMenu, False, 0, TabMenuClickHandler);
end;

//------------------------------------------------------------------------------
procedure TPBar.OnTabChange(Sender: TObject);
begin
 FTabChange (Sender);
 UpdateControls;
 FillCompList (fmActTab);
end;

//------------------------------------------------------------------------------
procedure TPBar.SetA2Z(i: Integer);
begin
 case i of
  1: begin
      A2ZBar.Align:= alLeft;
      PB_Options.A2ZPos:= alLeft;
      CfgA2ZLeft.Checked:= True;
     end;
  2: begin
      A2ZBar.Align:= alRight;
      PB_Options.A2ZPos:= alRight;
      CfgA2ZRight.Checked:= True;
     end;
  3: PB_Options.A2ZFilter:= CfgA2ZFilter.Checked;
  4: begin
      PB_Options.A2ZViz:= CfgA2ZShow.Checked;
      A2ZBar.Visible:= CfgA2ZShow.Checked;
      CfgA2ZLeft.Enabled:= CfgA2ZShow.Checked;
      CfgA2ZRight.Enabled:= CfgA2ZShow.Checked;
     end;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.ResetA2Z;
var i: integer;
begin
 for i:= 0 to Pred (A2ZBar.ControlCount) do begin
  (A2ZBar.Controls[i] as TSpeedButton).Down:= False;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.SetListView;
begin
 if PB_Options.ShowMode = smText then begin
  Liste1.Enabled:= False;
  Liste1.Visible:= False;
  FListe:= Liste2;
 end else begin
  Liste2.Enabled:= False;
  Liste2.Visible:= False;
  FListe:= Liste1;
 end;
 FListe.Enabled:= True;
 FListe.Visible:= True;
 FillCompList (PB_Options.Fillmode);
end;

//------------------------------------------------------------------------------
procedure TPBar.SetTabButtons;
begin
 case PB_Options.Fillmode of
  fmActTab:    BtnActTab.Down:= True;
  fmAllTabs:   BtnAllComps.Down:= True;
  fmFavorites: BtnFavorites.Down:= True;
  fmHistory:   BtnHistory.Down:= True;
  fmSearch:    ;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.SetTextSize;
var i: Integer;
begin
 i:= 10; // tsMedium
 case PB_Options.TextSize of
  tsBig:    i:= 12;
  tsSmall:  i:= 8;
 end;
 Liste1.Font.Size:= i;
 Liste2.Font.Size:= i;
end;

//------------------------------------------------------------------------------
procedure TPBar.SetSearchbar(i: Integer);
begin
 PB_Options.SearchBar:= TAlign (i);
 SearchPanel.Align:= TAlign (i);
end;

//------------------------------------------------------------------------------
procedure TPBar.SetSearchbarViz (b: Boolean);
begin
 PB_Options.SearchBarViz:= b;
 SearchPanel.Visible:= b;
end;

//------------------------------------------------------------------------------
procedure TPBar.TabMenuClickHandler(Sender: TObject);
var s: string;
begin
 GetSender (Sender, stCaption, s);
 FActTab:= DelShortcutChar(s);
 SwitchToTab (FActTab);
 UpdateControls;
// FillCompList (fmActTab, 'G');
end;

//------------------------------------------------------------------------------
procedure TPBar.UpdateControls;
var i: Integer;
begin
 // Aktives Tab feststellen
 FActTab:= GetActiveTab;
 // Kategorien löschen
 for i:= Pred (TabMenu.Items.IndexOf(TabMenuTearLine)) downto 0 do begin
  TabMenu.Items.Delete (i);
 end;
 BuildCatMenu (TabMenu.Items, False, TabMenu.Items.IndexOf(TabMenuTearLine), TabMenuClickHandler);
end;

//------------------------------------------------------------------------------
procedure TPBar.UpdateLanguage;

 procedure SetAction (Action: TAction; txt: TLanguageText; const y, x: string);
 var s: string;
 begin
  s:= GetLangStr (txt);
  Action.Caption:= s + y;
  Action.Hint:= s + x;
 end;

begin
 Caption:= cProgName;

 // Den Actions Caption und Hint verpassen
 SetAction (aShowMenu, ltShowMenu, '', '');
 SetAction (aActualTab, ltActCategory, '', ' (' + FActTab + ')');
 SetAction (aActualForm, ltActForm, '', '');
 SetAction (aAllComponents, ltAllComps, '', '');
 SetAction (aHistory, ltHistory, '', '');
 SetAction (aFavorites, ltFavorites, '', '');
 SetAction (aEditCategory, ltCfgCategory, c3Dots, '');
 SetAction (aConfigPalette, ltCfgPalette, c3Dots, '');
 SetAction (aConfPaletteBar, ltCfgPaletteBar, c3Dots, '');
 SetAction (aHelp, ltShowHelp, '', '');
 SetAction (aAbout, ltAbout, c3Dots, '');
 SetAction (aShowPaletteBar, ltPalettebar, c3Dots, '');

 //-- Toolbar Popup -----------------------------
 CfgBtnShow.Caption:= GetLangStr (ltToolbarShow);
 CfgBtnPos.Caption:= GetLangStr (ltToolbarPos);
 CfgBtnTop.Caption:= GetLangStr (ltTop);
 CfgBtnLeft.Caption:= GetLangStr (ltLeft);
 CfgBtnRight.Caption:= GetLangStr (ltRight);
 CfgBtnViz.Caption:= GetLangStr (ltToolbarViz);
 CfgBtn1.Caption:= GetLangStr (ltActCategory);
 CfgBtn2.Caption:= GetLangStr (ltAllComps);
 CfgBtn3.Caption:= GetLangStr (ltHistory);
 CfgBtn4.Caption:= GetLangStr (ltActForm);
 CfgBtn5.Caption:= GetLangStr (ltFavorites);
 CfgBtn6.Caption:= GetLangStr (ltCfgCategory);
 CfgBtn7.Caption:= GetLangStr (ltCfgPalette);
 CfgBtn8.Caption:= GetLangStr (ltCfgPaletteBar);
 CfgBtn9.Caption:= GetLangStr (ltShowHelp);
 CfgBtn10.Caption:= GetLangStr (ltAbout);
 CfgBtn11.Caption:= GetLangStr (ltShowMenu);

 //-- A2Z Popup --------------------------------
 CfgA2ZLeft.Caption:= GetLangStr (ltLeft);
 CfgA2ZRight.Caption:= GetLangStr (ltRight);
 CfgA2ZFilter.Caption:= GetLangStr (ltFilterMode);
 CfgA2ZShow.Caption:= GetLangStr (ltVisible);

 //-- History ----------------------------------
 CfgHistSave.Caption:= GetLangStr (ltHistSave);
 CfgHistClear.Caption:= GetLangStr (ltHistClear);

 //-- Konfig Popup -----------------------------
 CfgPalHotTrack.Caption:= GetLangStr (ltPalHottrack);
 CfgPalShow.Caption:= GetLangStr (ltPalView);
 CfgPalShowText.Caption:= GetLangStr (ltPalShowText);
 CfgPalShowIcon.Caption:= GetLangStr (ltPalShowIcons);
 CfgPalShowBoth.Caption:= GetLangStr (ltPalShowBoth);
 CfgPalText.Caption:= GetLangStr (ltPalText);
 CfgPalTextSmall.Caption:= GetLangStr (ltSmall);
 CfgPalTextMiddle.Caption:= GetLangStr (ltMedium);
 CfgPalTextBig.Caption:= GetLangStr (ltBig);

 //-- Suchfeld -----------------------------------------------------------------
 CfgSearchTop.Caption:= GetLangStr (ltTop);
 CfgSearchBottom.Caption:= GetLangStr (ltBottom);
 CfgSearchShow.Caption:= GetLangStr (ltVisible);

 TabMenu.Items[Pred (N3.MenuIndex)].Caption:= GetLangStr (ltCfgPaletteBar);
 TabMenu.Items[Pred (N3.MenuIndex)].ImageIndex:= 8;

 // Favoriten Menü
 aFavAdd.Caption:= GetLangStr (ltFavAdd);
 aFavRemove.Caption:= GetLangStr (ltFavRemove);
end;

//------------------------------------------------------------------------------
procedure TPBar.UpdateOptions;
var b: TBit;
    i: Integer;
begin
 // Sprache einstellen
 LoadLanguage (Pb_Options.Language);
 UpdateLanguage;

 // Komponentenliste
 case PB_Options.ShowMode of
  smIcon: CfgPalShowIcon.Checked:= True;
  smText: CfgPalShowText.Checked:= True;
  smBoth: CfgPalShowBoth.Checked:= True;
 end;
 SetListView;

 case PB_Options.TextSize of
  tsSmall:  CfgPalTextsmall.Checked:= True;
  tsMedium: CfgPalTextMiddle.Checked:= True;
  tsBig:    CfgPalTextBig.Checked:= True;
 end;
 SetTextsize;

 Liste1.HotTrack:= PB_options.Hottrack;
 Liste2.HotTrack:= PB_options.Hottrack;
 CfgPalHotTrack.Checked:= PB_options.Hottrack;

 // Buttonleiste
 case PB_Options.ButtonPos of
  alTop:   CfgBtnTop.Checked:= True;
  alLeft:  CfgBtnLeft.Checked:= True;
  alRight: CfgBtnRight.Checked:= True;
 end;
 ToolBar.Align:= PB_Options.ButtonPos;
 if PB_Options.Menu then ToolBar.Visible:= PB_Options.ToolBarViz
                    else ToolBar.Visible:= True;

 // Restore Buttons
 for b:= Bit0 to Bit10 do begin
  with CfgBtnShow.Items[Ord(b)] do begin
   if b in PB_Options.Buttons then begin
    Checked:= True;
    ImageIndex:= -1;
   end else begin
    Checked:= False;
    ImageIndex:= Ord(b) + 1;
   end;
   for i:= 0 to Pred (ToolBar.ButtonCount) do begin
    if ToolBar.Buttons[i].Tag = Ord(b) then
     ToolBar.Buttons[i].Visible:= CfgbtnShow.Items[Ord(b)].Checked;
   end;
  end;
 end;

 // A2Z-Bar
 A2ZBar.Align:= PB_Options.A2ZPos;
 if A2ZBar.Align = alLeft then SetA2Z (1);
 if A2ZBar.Align = alRight then SetA2Z (2);
 CfgA2ZFilter.Checked:= PB_Options.A2ZFilter;
 SetA2Z (3);
 CfgA2ZShow.Checked:= PB_Options.A2ZViz;
 SetA2Z (4);

 if PB_Options.StatusBarViz and
    (not oldopt.StatusBarViz) then begin
  SearchPanel.Visible:= False;
 end;
 StatusBar.Visible:= PB_Options.StatusBarViz;

 // Suchbalken
 SetSearchBar (Ord(PB_Options.Searchbar));
 SetSearchBarViz (PB_Options.SearchBarViz);
 CfgSearchTop.Checked:= SearchPanel.Align = alTop;
 CfgSearchBottom.Checked:= SearchPanel.Align = alBottom;
 CfgSearchShow.Checked:= PB_Options.SearchBarViz;

 // History
 CfgHistSave.Checked:= PB_Options.SaveHistory;
 Edit.Text:= '';
end;

procedure TPBar.UpdateMenu;
begin
 // aktivieren, wenn offen
 aActualTab.Visible:= PB_Options.Open;
 aAllComponents.Visible:= PB_Options.Open;
 aActualForm.Visible:= PB_Options.Open;
 aHistory.Visible:= PB_Options.Open;
 aFavorites.Visible:= PB_Options.Open;
 aEditCategory.Visible:= PB_Options.Open;
 aConfigPalette.Visible:= PB_Options.Open;
 aConfPaletteBar.Visible:= PB_Options.Open;
 aHelp.Visible:= PB_Options.Open;
end;


//------------------------------------------------------------------------------
// Form-Abhängig
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
procedure TPBar.FormCreate(Sender: TObject);

 function AddIcon (icon: integer): Integer;
 // Symbol für Menü erzeugen
 var il: TCustomImageList;
     bitmap: TBitmap;
 begin
  Result:= -1;
  if icon >= 0 then begin
   il:= (BorlandIDEServices as INTAServices).ImageList;
   bitmap:= TBitmap.Create;
   try
    MainImages.GetBitmap(icon, bitmap);
    Result:= il.AddMasked (bitmap, bitmap.Canvas.Pixels[0, 0]);
   finally
    Bitmap.Free;
   end;
  end;
 end;

 procedure AddMenu (act: TAction);
 var m: TMenuItem;
 begin
  m:= TMenuItem.Create (Application);
  m.Action:= act;
  if not IsD7 then  m.ImageIndex:= AddIcon (act.ImageIndex);
  FPBMenu.Add (m);
 end;

begin
 inherited;

 // Main Form von Delphi auslesen
 FComponentBar:= TWinControl (Application.MainForm.FindComponent (cPaletteBar));
 FTabControl:= TTabControl (FComponentBar.Controls [0]);
 FTabChange:= nil;

 // Das ist das Hauptmenü von Delphi
 FDelphiMenu:= (BorlandIDEServices as INTAServices).MainMenu;

 // Palettenkonfiguration suchen
 FComponentMenu:= FindMenu (FDelphiMenu, cConfigMenu);
 FConfigItem:= FindMenu (FComponentMenu, cConfigItem);

 // Menü erzeugen
 if PB_Options.Menu then begin
  FPBMenu:= TMenuItem.Create (Application);
  FPBMenu.Caption:= cWizardMenu;
  FPBMenu.OnClick:= MenuClick;
  FDelphiMenu.Items.Insert(FindMenuInt(FDelphiMenu, cToolsMenu)+1, FPBMenu);
  FPBMenu.NewTopLine;
  AddMenu (aShowPaletteBar);
  FPBMenu.NewBottomLine;
  AddMenu (aActualTab);
  AddMenu (aAllComponents);
  AddMenu (aActualForm);
  AddMenu (aHistory);
  AddMenu (aFavorites);
  AddMenu (aEditCategory);
  AddMenu (aConfigPalette);
  AddMenu (aConfPaletteBar);
  AddMenu (aHelp);
  AddMenu (aAbout);
 end else begin
  FPBMenu:= TMenuItem.Create (Application);
  FPBMenu.Caption:= GetLangStr (ltPalettebar);
  FPBMenu.ImageIndex:= AddIcon (13);
  FPBMenu.OnClick:= aShowPaletteBarExecute;
  FPBMenu.Name:= cPBMenu;
  FComponentMenu.Add (FPBMenu);
 end;

 // PaletteBar einstellen
 FOpen:= False;
 FTabs:= TStringList.Create;
 FActTab:= '';
 //PB_Options.Fillmode:= fmNone;
 FDragging:= dmNone;
 FSortAsc:= True;
 FPalSorting:= psAZ;
 FContext:= nil;
 FPackageCount:= 9999999;
 FTabChange:= nil;
 FComponentList:= TList.Create;
 // Listen ausrichten
 Liste1.Align:= alClient;
 Liste2.Align:= alClient;
 UpdateMenu;

 //  Komponentenliste erstellen
 GenComponentlist;
 // Gespeicherte Listen laden
 LoadList (rmHistory);
 LoadList (rmFavorites);
 LoadList (rmSearch);

 if PB_Options.Floating then begin
  Left:= PB_Options.Left;
  Top:= PB_Options.Top;
  Width:= PB_Options.Width;
  Height:= PB_Options.Height;
 end;

 UpdateLanguage;
 UpdateOptions;

 if PB_Options.Open and PB_Options.Floating then begin
  while not Application.Active do Application.ProcessMessages;
  Show;
  BringToFront;
 end;

 // Docking stuff
 DeskSection:= cWizardName;
 AutoSave:= True;

end;

//------------------------------------------------------------------------------
procedure TPBar.FormShow(Sender: TObject);
begin
 StatusBar.SimpleText:= cStatusBarText;
 if PB_Options.Fillmode = fmNone then FillCompList (fmActTab)
                                 else FillCompList (PB_Options.Fillmode);
 PB_Options.Open:= True;  // und wir sind offen...
 FOpen:= True;
end;

//------------------------------------------------------------------------------
procedure TPBar.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 PB_Options.Open:= False;
 PB_Options.Floating:= Floating;
 PB_Options.Left:= Left;
 PB_options.Top:= Top;
 PB_Options.Width:= Width;
 PB_Options.Height:= Height;
 SaveToRegistry;
 UpDateMenu;
 inherited;
end;

//------------------------------------------------------------------------------
procedure TPBar.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var vcl_form: TCustomForm;
    vcl_ctrl: TControl;
{$ifdef ClxVersion}
    clx_form: QForms.TCustomForm;
    clx_ctrl: QControls.TControl;
{$endif}
    p1: TPoint;
    s: string;
begin
 if (ssLeft in Shift) and (FDragging = dmClick) then begin
  FDragging:= dmDrag;
 end else begin
  if (FDragging = dmDrag) and Assigned (FFormDesigner) then begin
   p1:= ClientToScreen(Point(x, y));
   s:= '';
   // Feststellen ob ClX oder VCL Form...
   vcl_ctrl:= FindDragTarget (p1, True);
   if Assigned (vcl_ctrl) then begin
    vcl_form:= GetParentForm (vcl_ctrl); // ParentForm dazu ermitteln
    s:= vcl_form.Name;
{$ifdef ClxVersion}
   end else begin
    clx_ctrl:= QControls.FindDragTarget (p1, True);
    if Assigned (clx_ctrl) then begin
     clx_form:= QForms.GetParentForm (clx_ctrl); // ParentForm dazu ermitteln
     s:= clx_form.Name;
    end;
{$endif}
   end;
   // ist das unser Zielfenster?
   if IsTarget (s) then  Screen.Cursor:= crDrag
                   else  Screen.Cursor:= crNoDrop;
  end; // if FDragging
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var vcl_form: TCustomForm;
    vcl_ctrl: TControl;
{$ifdef ClxVersion}
    clx_form: QForms.TCustomForm;
    clx_ctrl: QControls.TControl;
{$endif}
    fe: IOTAFormEditor;
    cp: IOTAComponent;
    pc: PtrComponent;
    p1, p2: TPoint;
begin
 Windows.ReleaseCapture;
 Screen.Cursor:= crDefault;
 if (FDragging = dmDrag) and Assigned (FFormDesigner) then begin

  p1:= ClientToScreen(Point(x, y));

  // Feststellen ob ClX oder VCL Form...
  vcl_ctrl:= FindDragTarget (p1, True);
  if Assigned (vcl_ctrl) then begin
   vcl_form:= GetParentForm (vcl_ctrl); // ParentForm dazu ermitteln
   // FormDesinger  = ParentForm sind identisch?
   if IsTarget (vcl_form.Name) and (vcl_form.Handle <> Self.Handle) then begin
    FFormDesigner.SelectComponent(vcl_ctrl);
    fe:= GetActiveFormEditor;
    if Assigned (fe) then begin // FormEditor holen
     cp:= fe.GetCreateParent; // und das Elternelement suchen
     if Assigned (cp) then begin

      if (vcl_form.Name = cDataModuleForm) then begin
       p2:= vcl_form.ScreenToClient (p1);
      end else begin
       p2:= ((cp as INTAComponent).GetComponent as TControl).ScreenToClient (p1);
      end;

      p2.X:= Max (p2.X, 1);
      p2.Y:= Max (p2.Y, 1);
      pc:= GetActComponent;
      if Assigned (pc) then begin
       ActivateClassGroup (TControl);
       cp:= fe.CreateComponent(cp, pc.typename, p2.X, p2.Y, 8, 8);
       if Assigned (cp) then begin
        pc.history:= True;
        gRegIni.WriteString (cRegHistory, pc.typename, '1');
       end else begin
        MessageDlg(GetLangStr (ltErrorInsert), mtError, [mbOK], 0);
       end;
      end;
     end; // if Assigned (cp)
    end; // if Assigned (fe)
   end; // if fd.Root.Name

{$ifdef ClxVersion}
  end else begin
   clx_ctrl:= QControls.FindDragTarget (p1, True);
   if Assigned (clx_ctrl) then begin
    clx_form:= QForms.GetParentForm (clx_ctrl); // ParentForm dazu ermitteln
    // FormDesinger  = ParentForm sind identisch?
    if IsTarget (clx_form.Name) then begin
     FFormDesigner.SelectComponent(clx_ctrl);
     fe:= GetActiveFormEditor;
     if Assigned (fe) then begin // FormEditor holen
      cp:= fe.GetCreateParent; // und das Elternelement suchen
      if Assigned (cp) then begin

       if (clx_form.Name = cDataModuleForm) then begin
        p2:= clx_form.ScreenToClient (p1);
       end else begin
        p2:= ((cp as INTAComponent).GetComponent as QControls.TControl).ScreenToClient (p1);
       end;

       p2.X:= Max (p2.X, 1);
       p2.Y:= Max (p2.Y, 1);
       pc:= GetActComponent;
       if Assigned (pc) then begin
        ActivateClassGroup (QControls.TControl);
        cp:= fe.CreateComponent(cp, pc.typename, p2.X, p2.Y, 8, 8);
        if Assigned (cp) then  pc.history:= True
                         else  MessageDlg(GetLangStr (ltErrorInsert), mtError, [mbOK], 0);
       end;
      end; // if Assigned (cp)
     end; // if Assigned (fe)
    end; // if fd.Root.Name
   end;
{$endif}
  end;
  FFormDesigner:= nil;
  FDragging:= dmNone;
 end; // if FDragging
end;

//------------------------------------------------------------------------------
procedure TPBar.HeaderSectionClick(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin
 case FPalSorting of
  psAZ:   begin
           FSortAsc:= True;
           FListe.AlphaSort;
           FPalSorting:= psZA;
           Section.ImageIndex:= cIconAZ;
          end;
  psZA:   begin
           FSortAsc:= False;
           FListe.AlphaSort;
           if PB_Options.Fillmode = fmActTab then FPalSorting:= psNone
                                             else FPalSorting:= psAZ;
           Section.ImageIndex:= cIconZA;
          end;
  psNone: begin
           FillCompList (PB_Options.Fillmode);
           FPalSorting:= psAZ;
           Section.ImageIndex:= cIconUnsort;
          end;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.A2ZClick(Sender: TObject);
var ch: char;
    s: string;
    i: Integer;
begin
 ch:= (Sender as TSpeedButton).Caption[1];
 if PB_Options.A2ZFilter then begin
  FFilter:= ch;
  FillCompList (fmFilter);
 end else begin
  for i:= 0 to Pred (FListe.Items.Count) do begin
   s:= FListe.Items[i].Text;
   if UpCase(s[1]) = ch then begin
    FListe.Items[i].MakeVisible;
//    FListe.Scroll(0, (FListView.Height div 3) * 2);
    Exit;
   end;
  end;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.Liste1Click(Sender: TObject);
begin
 FDragging:= dmNone;
 Windows.ReleaseCapture;
 Screen.Cursor:= crDefault;
end;

//------------------------------------------------------------------------------
procedure TPBar.Liste1Compare(Sender: TObject; Node1, Node2: TTreeNode;
  Data: Integer; var Compare: Integer);
var p1, p2: PtrComponent;
begin
 p1:= Node1.Data;
 p2:= Node2.Data;
 if FSortAsc then Compare:= AnsiCompareStr (p1.compname, p2.compname)
             else Compare:= AnsiCompareStr (p2.compname, p1.compname);
end;

//------------------------------------------------------------------------------
procedure TPBar.Liste1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var p: TPoint;
begin
 p:= FListe.ClientToScreen (MousePos);
 FContext:= FListe.GetNodeAt (MousePos.X, MousePos.Y);
 if Assigned (FContext) then begin
  FavMenu.Popup (p.X, p.Y);
 end else begin
  if PB_Options.Fillmode = fmHistory then CfgHist.Popup (p.X, p.Y)
                                     else CfgPal.Popup (p.X, p.Y);
 end;
 Handled:= True;
end;

//------------------------------------------------------------------------------
procedure TPBar.Liste1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case key of
  vk_F1: aHelpExecute(Sender);
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.Liste1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var ht: THitTests;
begin
 if Button = mbLeft then begin
  ht:= FListe.GetHitTestInfoAt(X, Y);
  if htOnItem in ht then begin
   FDragging:= dmClick;
   Windows.SetCapture (Self.Handle);
   FFormDesigner:= GetActiveFormDesigner;
   Screen.Cursor:= crNoDrop;
  end;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.Liste1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FDragging:= dmNone;
 Windows.ReleaseCapture;
 Screen.Cursor:= crDefault;
end;

//------------------------------------------------------------------------------
procedure TPBar.BtnGoClick(Sender: TObject);
var i: Integer;
begin
 if Edit.Text <> '' then begin
  FillCompList (fmSearch);
  i:= Edit.Items.IndexOf(Edit.Text);
  if i < 0 then begin
   Edit.Items.Add(Edit.Text);
   gRegIni.WriteString (cRegSearchText, Edit.Text, '1');
  end;
 end;
end;

//-----------------------------------------------------------------------------
// Menüs
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
procedure TPBar.TabMenuPopup(Sender: TObject);
begin
 UpdateControls;
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgBtn10Click(Sender: TObject);
var btn, i: integer;
begin
 GetSender (Sender, stTag, btn);
 if btn >= 0 then begin
  for i:= 0 to Pred (ToolBar.ButtonCount) do begin
   if ToolBar.Buttons[i].Tag = btn then begin
    ToolBar.Buttons[i].Visible:= CfgbtnShow.Items[btn].Checked;
    if CfgbtnShow.Items[btn].Checked then begin
     CfgBtnShow.Items[btn].ImageIndex:= -1;
     Include (PB_Options.Buttons, TBit (btn));
    end else begin
     CfgBtnShow.Items[btn].ImageIndex:= btn;
     Exclude (PB_Options.Buttons, TBit (btn));
    end;
    Exit;
   end;
  end;
  ToolBar.Update;
 end;
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgBtnRightClick(Sender: TObject);
var i: Integer;
begin
 GetSender (Sender, stTag, i);
 case i of
  1: PB_Options.ButtonPos:= alTop;
  2: PB_Options.ButtonPos:= alLeft;
  3: PB_Options.ButtonPos:= alRight;
 end;
 ToolBar.Align:= PB_Options.ButtonPos;
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgPalShowBothClick(Sender: TObject);
var i: Integer;
begin
 GetSender (Sender, stTag, i);
 PB_Options.ShowMode:= TPalShowmode (Pred(i));
 SetListView;
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgBtnVizClick(Sender: TObject);
begin
 if PB_Options.Menu then begin
  ToolBar.Visible:= CfgBtnViz.Checked;
  PB_Options.ToolBarViz:= CfgBtnViz.Checked;
 end;
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgPalTextSmallClick(Sender: TObject);
var i: integer;
begin
 GetSender (Sender, stTag, i);
 case i of
  1: PB_Options.TextSize:= tsBig;
  2: PB_Options.TextSize:= tsMedium;
  3: PB_Options.TextSize:= tsSmall;
 end;
 SetTextSize;
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgPalHottrackClick(Sender: TObject);
begin
 PB_Options.Hottrack:= CfgPalHottrack.Checked;
 Liste1.HotTrack:= CfgPalHottrack.Checked;
 Liste2.HotTrack:= CfgPalHottrack.Checked;
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgA2ZShowClick(Sender: TObject);
var i: integer;
begin
 GetSender (Sender, stTag, i);
 SetA2Z (i);
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgHistSaveClick(Sender: TObject);
begin
 PB_Options.SaveHistory:= CfgHistSave.Checked;
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgHistClearClick(Sender: TObject);
var r: TRegInifile;
    p: PtrComponent;
    i: Integer;
begin
 r:= TRegIniFile.Create (cRegKey);
 try
  r.EraseSection (cRegHistory);
 finally
  r.Free;
 end;
 for i:= 0 to Pred (FComponentList.Count) do begin
  p:= FComponentList[i];
  if Assigned (p) then  p.history:= False;
 end;
 if PB_Options.Fillmode = fmHistory then  FillCompList (fmHistory);
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgSearchBottomClick(Sender: TObject);
var i: integer;
begin
 GetSender (Sender, stTag, i);
 SetSearchbar (i);
end;

procedure TPBar.CfgSearchShowClick(Sender: TObject);
begin
 SetSearchbarViz ((Sender as TMenuItem).Checked);
end;

//-----------------------------------------------------------------------------
// Actions
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
procedure TPBar.aShowMenuExecute(Sender: TObject);
begin
 // Must exist!
end;

//-----------------------------------------------------------------------------
procedure TPBar.aActualTabExecute(Sender: TObject);
begin
 FillCompList (fmActTab);
end;

//-----------------------------------------------------------------------------
procedure TPBar.aAllComponentsExecute(Sender: TObject);
begin
 FillCompList (fmAllTabs);
end;

//-----------------------------------------------------------------------------
procedure TPBar.aActualFormExecute(Sender: TObject);
begin
 FillCompList (fmActForm);
end;

//-----------------------------------------------------------------------------
procedure TPBar.aHistoryExecute(Sender: TObject);
begin
 FillCompList (fmHistory);
end;

//-----------------------------------------------------------------------------
procedure TPBar.aFavoritesExecute(Sender: TObject);
begin
 FillCompList (fmFavorites);
end;

//-----------------------------------------------------------------------------
procedure TPBar.aEditCategoryExecute(Sender: TObject);
var f: TPaletteCategories;
begin
 BtnCategory.Down:= True;
 f:= TPaletteCategories.Create (Application);
 try
  if f.ShowModal = mrOk then begin
   f.SaveCategories;
  end;
 finally
  f.Free;
 end;
 SetTabButtons;
end;

//-----------------------------------------------------------------------------
procedure TPBar.aConfigPaletteExecute(Sender: TObject);
begin
 BtnConfPalette.Down:= True;
 ConfigPalette;
 SetTabButtons;
end;

//-----------------------------------------------------------------------------
procedure TPBar.aConfPaletteBarExecute(Sender: TObject);
var f: TPaletteConfig;
begin
 BtnCategory.Down:= True;
 f:= TPaletteConfig.Create (Application);
 try
  if f.ShowModal = mrOk then begin
   f.SetOptions;
   UpdateOptions;
  end;
 finally
  f.Free;
 end;
 SetTabButtons;
end;

//-----------------------------------------------------------------------------
procedure TPBar.aHelpExecute(Sender: TObject);
var p: PtrComponent;
begin
 p:= nil;
 if Assigned (FContext) then begin
  p:= FContext.Data;
 end else if Assigned (FListe.Selected) then begin
  p:= FListe.Selected.Data;
 end;
 if Assigned (p) then  Application.HelpKeyword (p.typename);
 FContext:= nil;
 SetTabButtons;
end;

//-----------------------------------------------------------------------------
procedure TPBar.aAboutExecute(Sender: TObject);
var f: TpbAboutFrm;
begin
 f:= TpbAboutFrm.Create (Application);
 try
  BtnAbout.Down:= True;
  f.ShowModal;
  SetTabButtons;
 finally
  f.Free;
 end;
end;

//-----------------------------------------------------------------------------
procedure TPBar.aFavAddExecute(Sender: TObject);
var p: PtrComponent;
begin
 if Assigned (FListe.Selected) then begin
  p:= FListe.Selected.Data;
  if Assigned (p) then begin
   p.favorite:= True;
   gRegIni.WriteString (cRegFavorites, p.typename, '1');
  end;
 end;
end;

//-----------------------------------------------------------------------------
procedure TPBar.aFavRemoveExecute(Sender: TObject);
var p: PtrComponent;
begin
 if Assigned (FListe.Selected) then begin
  p:= FListe.Selected.Data;
  if Assigned (p) then begin
   p.favorite:= false;
   gRegIni.DeleteKey(cRegFavorites, p.typename);
   FillCompList (fmFavorites);
  end;
 end;
end;

//-----------------------------------------------------------------------------
procedure TPBar.aShowPaletteBarExecute(Sender: TObject);
begin
 Show;
end;

//-----------------------------------------------------------------------------
procedure TPBar.LoadWindowState(Desktop: TMemIniFile);
begin
 inherited LoadWindowState(Desktop);
end;

//-----------------------------------------------------------------------------
procedure TPBar.SaveWindowState(Desktop: TMemIniFile; isProject: Boolean);
begin
 inherited SaveWindowState (Desktop, IsProject);
end;

//-----------------------------------------------------------------------------
procedure TPBar.FormDestroy(Sender: TObject);
begin
 if Fopen then begin
  PB_Options.Floating:= Floating;
  if Pb_Options.Floating then begin
   PB_Options.Left:= Left;
   PB_options.Top:= Top;
   PB_Options.Width:= Width;
   PB_Options.Height:= Height;
  end;
 end;
 SaveToRegistry;
 if Assigned (FTabChange) then begin
  FTabControl.OnChange:= FTabChange;
  FTabChange:= nil;
 end;
 FreeAndNil (FPBMenu);
 FreeAndNil (FTabs);
 FreeAndNil (FComponentList);
 FreeAndNil (gLangList);
 FreeAndNil (gLangText);
 FreeAndNil (gRegIni);
 inherited;
end;

initialization
 Pbar:= nil;
finalization
 if Assigned (UnregisterFieldAddress) then UnregisterFieldAddress (Addr(PBar));
 Pbar:= nil;
end.
