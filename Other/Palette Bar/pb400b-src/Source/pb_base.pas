//----------------------------------------------------------------------------
// Unit Name: pb_about
// Author:    Helli
// Date:      12.07.2003
// Purpose:
// History:
//----------------------------------------------------------------------------
//  Copyright © 2003 by Hellinger Software.  All Rights Reserved.
//----------------------------------------------------------------------------

unit pb_base;

interface

{$ifdef ClxVersion}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Buttons,
  QControls, QForms,
  Controls, Forms,
  DockForm, DeskForm, DeskUtil,
  Dialogs, ActnList, Menus, ComCtrls, ToolWin, StdCtrls, ExtCtrls,
  ImgList, ToolsAPI, DesignIntf, IniFiles, TypInfo, ComponentDesigner,
  pb_common, jpeg;
{$endif}
{$ifndef ClxVersion}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Buttons,
  Controls, Forms,
  DockForm, DeskForm, DeskUtil,
  Dialogs, ActnList, Menus, ComCtrls, ToolWin, StdCtrls, ExtCtrls,
  ImgList, ToolsAPI, DesignIntf, IniFiles, TypInfo, ComponentDesigner,
  pb_common;
{$endif}



//----------------------------------------------------------------------------

type
  TBase = class(TForm)
    Image1: TImage;
    Version: TLabel;
    procedure Image1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aActTabExecute(Sender: TObject);
    procedure aAllCompsExecute(Sender: TObject);
    procedure aActFormExecute(Sender: TObject);
    procedure aHistoryExecute(Sender: TObject);
    procedure aFavoritesExecute(Sender: TObject);
    procedure aCategoriesExecute(Sender: TObject);
    procedure aPaletteExecute(Sender: TObject);
    procedure aConfigExecute(Sender: TObject);
    procedure aHelpExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
    procedure aAddFavExecute(Sender: TObject);
    procedure aDelFavExecute(Sender: TObject);
    procedure aShowExecute(Sender: TObject);
  private
    { Private-Deklarationen }
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    FPackageCount: Integer;
    FDelphiMenu: TMainMenu;
    FTabControl: TTabControl;
    FComponentBar: TWinControl;
    FComponentMenu: TMenuItem;
    FConfigItem: TMenuItem;
    FPBMenu: TMenuItem;
//    FTabChange: TNotifyEvent;
    FActTab: string;
    FPalette: TCustomControl;
    FComponentList: TList;
    procedure OnTabChange(Sender: TObject);
    procedure SwitchToTab (const TabName: string);
    procedure MenuClick(Sender: TObject);
    procedure BuildCatMenu (menu: TMenuItem; mm: Boolean; p: Integer; click: TNotifyEvent);
    procedure TabMenuClickHandler (Sender: TObject);
    function  GetDelphiTabs: TStrings;
    function  GetDelphiActiveTab: string;
    function  GetTabs: TStrings;
    function  GetActiveTab: string;
    procedure UpdateMenu;
    procedure UpdateLanguage;
    procedure GenComponentlist;
    function  GetComponent(const Name: string): PtrComponent;
    procedure CfgHistClear;
    function  AddImage(comp: string; list: TImageList): integer;
    procedure ReadCategories;
    procedure WriteCategories;
  end;

var Base: TBase;

implementation

{$R *.dfm}

uses Registry,
     pb_config, pb_categories, pb_master;

var  gInList: Boolean;



//------------------------------------------------------------------------------
procedure TBase.FormCreate(Sender: TObject);

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
 // Main Form von Delphi auslesen
 FComponentBar:= TWinControl (Application.MainForm.FindComponent (cPaletteBar));
 FTabControl:= TTabControl (FComponentBar.Controls [0]);
 FPalette:= TCustomControl (Application.MainForm.FindComponent(cPalette));
// FTabChange:= nil;

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
  AddMenu (aShow);
  FPBMenu.NewBottomLine;
  AddMenu (aActTab);
  AddMenu (aAllComps);
  AddMenu (aActForm);
  AddMenu (aHistory);
  AddMenu (aFavorites);
  FPBMenu.NewBottomLine;
  AddMenu (aCategories);
  AddMenu (aPalette);
  AddMenu (aConfig);
  FPBMenu.NewBottomLine;
  AddMenu (aHelp);
  AddMenu (aAbout);
 end else begin
  FPBMenu:= TMenuItem.Create (Application);
  FPBMenu.Caption:= GetLangStr (ltPalettebar);
  FPBMenu.ImageIndex:= AddIcon (13);
  FPBMenu.OnClick:= aShowExecute;
  FPBMenu.Name:= cPBMenu;
  FComponentMenu.Add (FPBMenu);
 end;

 UpdateLanguage;

 // Komponentenliste erzeugen
 FComponentList:= TList.Create;

// Create Mainform if we are in floating mode
// if PB_Options.Floating and not Assigned (PBar) then begin
//  PBar:= TPBar.Create (Application);
// end;
end;

//------------------------------------------------------------------------------
procedure TBase.FormDestroy(Sender: TObject);
begin
// if Assigned (FTabChange) then begin
//  FTabControl.OnChange:= FTabChange;
//  FTabChange:= nil;
// end;
 FreeAndNil (FPBMenu);
 FreeAndNil (FComponentList);
end;

//------------------------------------------------------------------------------
procedure TBase.FormShow(Sender: TObject);
begin
 Caption:= GetLangStr (ltAbout);
 Version.Caption:= cVersion;
end;

//------------------------------------------------------------------------------
procedure TBase.Image1Click(Sender: TObject);
begin
 Close;
end;


//------------------------------------------------------------------------------
// Actions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
procedure TBase.aActTabExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.FillCompList (fmActTab);
end;

//------------------------------------------------------------------------------
procedure TBase.aAllCompsExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.FillCompList (fmAllTabs);
end;

//------------------------------------------------------------------------------
procedure TBase.aActFormExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.FillCompList (fmActForm);
end;

//------------------------------------------------------------------------------
procedure TBase.aHistoryExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.FillCompList (fmHistory);
end;

//------------------------------------------------------------------------------
procedure TBase.aFavoritesExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.FillCompList (fmFavorites);
end;

//------------------------------------------------------------------------------
procedure TBase.aCategoriesExecute(Sender: TObject);
var f: TPaletteCategories;
begin
 if Assigned (PBar) then PBar.BtnCategories.Down:= True;
 f:= TPaletteCategories.Create (Application);
 try
  if f.ShowModal = mrOk then begin
   f.SaveCategories;
  end;
 finally
  f.Free;
 end;
 if Assigned (PBar) then PBar.BtnCategories.Down:= False;
end;

//------------------------------------------------------------------------------
procedure TBase.aPaletteExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.BtnPalette.Down:= True;
 if Assigned (FConfigItem) then FConfigItem.Action.Execute;
 if Assigned (PBar) then PBar.BtnPalette.Down:= False;
end;

//------------------------------------------------------------------------------
procedure TBase.aConfigExecute(Sender: TObject);
var f: TPaletteConfig;
begin
 if Assigned (PBar) then PBar.BtnConfig.Down:= True;
 f:= TPaletteConfig.Create (Application);
 try
  if f.ShowModal = mrOk then begin
   f.SetOptions;
   if Assigned (PBar) then PBar.UpdateOptions;
  end;
 finally
  f.Free;
 end;
 if Assigned (PBar) then PBar.BtnConfig.Down:= False;
end;

//------------------------------------------------------------------------------
procedure TBase.aAddFavExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.AddFavorite;
end;

//------------------------------------------------------------------------------
procedure TBase.aDelFavExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.RemoveFavorite;
end;

//------------------------------------------------------------------------------
procedure TBase.aShowExecute(Sender: TObject);
begin
 if Assigned (PBar) then begin
  Pbar.Show;
 end else begin
  PBar:= TPBar.Create (Application);
  Pbar.Show;
 end;
end;

//------------------------------------------------------------------------------
procedure TBase.aHelpExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.HelpComponent;
end;

//------------------------------------------------------------------------------
procedure TBase.aAboutExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.BtnAbout.Down:= True;
 ShowModal;
 if Assigned (PBar) then PBar.BtnAbout.Down:= False;
end;


//------------------------------------------------------------------------------
// Support
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
function TBase.AddImage(comp: string; list: TImageList): integer;
var bitmap: TBitmap;
    col: TColor;
//    old: TPersistentClass;
    item: IPaletteItem;
    itemx: IPalettePaint;
begin
 Result:= -1;

 {$ifdef ClxVersion}
 if (BorlandIDEServices as IOTAServices).GetActiveDesignerType = dVCL then begin
  old:= ActivateClassGroup (TControl)
 end else begin
  old:= ActivateClassGroup (QControls.TControl);
 end;
 {$endif}

 item:= ComponentDesigner.ActiveDesigner.Environment.GetPaletteItem (TComponentClass (FindClass (comp))) as IPaletteItem;
 if Assigned (item) then begin
  if item.QueryInterface (IPalettePaint, itemx) = 0 then begin
   bitmap:= TBitmap.Create;
   bitmap.Height:= 28;
   bitmap.Width:= 28;
   bitmap.Canvas.Brush.Color:= clFuchsia;
   bitmap.Canvas.FillRect(Rect(0,0,28,28));
   itemx.Paint (Bitmap.Canvas, 0, 0);
   col:= Bitmap.Canvas.Pixels[0, 0]; // Transparente Farbe holen
   Result:= List.AddMasked (bitmap, col);
   bitmap.Free;
  end;
 end;

// {$ifdef ClxVersion} ActivateClassGroup (old); {$endif}
end;

//------------------------------------------------------------------------------
procedure TBase.BuildCatMenu(menu: TMenuItem; mm: Boolean; p: Integer;
  click: TNotifyEvent);
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
procedure TBase.CfgHistClear;
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
  if Assigned (p) then Exclude (p.flags, pcHistory);
 end;
end;

//------------------------------------------------------------------------------
procedure TBase.GenComponentlist;
const cSelectedIndex = 'SelectedIndex';
      cPalToolCount =  'PalToolCount';
      cSelToolName =   'SelectedToolName';
var   PropInfo: PPropInfo;
      i, j: Integer;
      PalToolCount: Integer;
      SelectedToolName: string;
      OldTabIndex, OldToolIndex: integer;
      pc: PtrComponent;
      ps: IOTAPackageServices;
begin
 // Komponentenliste erstellen
 if gInList then Exit;
 gInList:= True;

 ps:= BorlandIDEServices as IOTAPackageServices;
 if FPackageCount = ps.PackageCount then Exit; // Liste nicht neu aufbauen

 FPackageCount:= ps.PackageCount;

 if Assigned (FPalette) then begin

  // Checked-Flag auf false, um zu checken wer da ist
  for i:= 0 to Pred (FComponentList.Count) do begin
   pc:= FComponentList[i];
   if Assigned (pc) then Exclude (pc.flags, pcChecked);
  end;

  FTabControl.Visible:= False;
  FTabControl.Tabs.BeginUpdate;
  OldTabIndex:= FTabControl.TabIndex;
  PropInfo:= GetPropInfo (FPalette.ClassInfo, cSelectedIndex);
  OldToolIndex:= GetOrdProp (FPalette, PropInfo);

  for i:= 0 to Pred(FTabControl.Tabs.Count) do begin
   FTabControl.TabIndex:= i;
   FTabControl.OnChange (FTabControl);
   PropInfo:= GetPropInfo (FPalette.ClassInfo, cPalToolCount);
   PalToolCount:= GetOrdProp (FPalette, PropInfo);

   for j:= 0 to Pred (PalToolCount) do begin
    PropInfo:= GetPropInfo (FPalette.ClassInfo, cSelectedIndex);
    SetOrdProp (FPalette, PropInfo, j);
    PropInfo:= GetPropInfo (FPalette.ClassInfo, cSelToolName);
    SelectedToolName:= GetStrProp (FPalette, PropInfo);

    if SelectedToolName = 'Frames' then SelectedToolName:= 'TFrames';

    pc:= GetComponent (SelectedToolName);
    if Assigned (pc) then begin
     Include (pc.flags, pcChecked);
    end else begin
     New (pc);
     if Assigned (pc) then begin
      pc.typename:= SelectedToolName;
      pc.tabname:= FTabControl.Tabs[i];
      pc.categories:= '';
      pc.tempcat:= '';
      pc.flags:= [pcChecked];
      if pc.typename = 'TFrames' then pc.imgindex:= 1
                                 else pc.imgindex:= 0;
      FComponentList.Add (pc);
     end;
    end;
   end;
  end;
  FTabControl.Tabs.EndUpdate;
  FTabControl.TabIndex:= OldTabIndex;
  PropInfo:= GetPropInfo (FPalette.ClassInfo, cSelectedIndex);
  SetOrdProp (FPalette, PropInfo, OldToolIndex);
  FTabControl.Visible:= True;

  for i:= Pred (FComponentList.Count) downto 0 do begin
   pc:= FComponentList[i];
   if Assigned (pc) then begin
    if not (pcChecked in pc.flags) then begin
     Dispose (pc);
     FComponentList.Delete(i);
    end;
   end;
  end;

 end;
 gInList:= False;
end;

//------------------------------------------------------------------------------
function TBase.GetComponent(const Name: string): PtrComponent;
var i: Integer;
    pc: PtrComponent;
begin
 Result:= nil;
 for i:= 0 to Pred (FComponentList.Count) do begin
  pc:= FComponentList[i];
  if Assigned (pc) then begin
   if (pc.typename = Name) then begin
    Result:= pc;
    Exit;
   end;
  end;
 end;
end;

//------------------------------------------------------------------------------
function TBase.GetActiveTab: string;
begin
//
 Result:= FTabControl.Tabs[FTabControl.TabIndex];
end;

//------------------------------------------------------------------------------
function TBase.GetTabs: TStrings;
begin
//
 Result:= FTabControl.Tabs;
end;

//------------------------------------------------------------------------------
function TBase.GetDelphiActiveTab: string;
begin
 Result:= FTabControl.Tabs[FTabControl.TabIndex];
end;

//------------------------------------------------------------------------------
function TBase.GetDelphiTabs: TStrings;
begin
 Result:= FTabControl.Tabs;
end;

//------------------------------------------------------------------------------
procedure TBase.MenuClick(Sender: TObject);
var i: Integer;
    m: TMenuItem;
begin
 // Kategorien löschen
 m:= FPBMenu.Find('-');
 for i:= Pred (FPBMenu.IndexOf(m)) downto 0 do  FPBMenu.Delete (i);
 BuildCatMenu (FPBMenu, False, 0, TabMenuClickHandler);
end;

//------------------------------------------------------------------------------
procedure TBase.OnTabChange(Sender: TObject);
begin
// FTabChange (Sender);
 PBar.UpdateControls;
 PBar.FillCompList (fmActTab);
end;

//------------------------------------------------------------------------------
procedure TBase.SwitchToTab(const TabName: string);
begin
// if not Assigned (FTabChange) then begin
//  FTabChange:= FTabControl.OnChange;
//  FTabControl.OnChange:= OnTabChange;
// end;
// FTabControl.TabIndex:= FTabControl.Tabs.IndexOf (TabName);
// FTabControl.OnChange (FTabControl);
end;

//------------------------------------------------------------------------------
procedure TBase.TabMenuClickHandler(Sender: TObject);
var s: string;
begin
 GetSender (Sender, stCaption, s);
 DelShortcutChar(s);
 SwitchToTab (DelShortcutChar(s));
 if Assigned (PBar) then PBar.UpdateControls;
end;

//------------------------------------------------------------------------------
procedure TBase.UpdateLanguage;
var s: string;
begin
 LoadLanguage (Pb_Options.Language);

 // Den Actions Caption und Hint verpassen
 UpdateMenu;

 aShow.Caption:= GetLangStr (ltPalettebar) + c3Dots;

 // Favoriten Menü
 aAddFav.Caption:= GetLangStr (ltFavAdd);
 aDelFav.Caption:= GetLangStr (ltFavRemove);

 if Assigned (PBar) then with PBar do begin
  Caption:= cProgName;

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

  CfgPalViewKachel.Caption:= GetLangStr (ltPalViewKachel);
  CfgPalViewSymbol.Caption:= GetLangStr (ltPalViewSymbol);
  CfgPalViewList.Caption:= GetLangStr (ltPalViewList);
  CfgPalViewText.Caption:= GetLangStr (ltPalViewText);

  CfgPalText.Caption:= GetLangStr (ltPalText);
  CfgPalTextSmall.Caption:= GetLangStr (ltSmall);
  CfgPalTextMiddle.Caption:= GetLangStr (ltMedium);
  CfgPalTextBig.Caption:= GetLangStr (ltBig);

  //-- Suchfeld -----------------------------------------------------------------
  CfgSearchTop.Caption:= GetLangStr (ltTop);
  CfgSearchBottom.Caption:= GetLangStr (ltBottom);
  CfgSearchShow.Caption:= GetLangStr (ltVisible);

 end;
end;

//------------------------------------------------------------------------------
procedure TBase.UpdateMenu;

 procedure SetItems (Action: TAction; Button: TToolButton; Menu: TMenuItem;
                     txt: TLanguageText; const x, y: string);
 var s: string;
 begin
  s:= GetLangStr (txt);
  Action.Caption:= s;
  if Assigned (Button) then Button.Hint:= s + x;
  if Assigned (Menu) then Menu.Caption:= s + y;
 end;

begin
 if Assigned (PBar) then begin
  with PBar do begin
   SetItems (aActTab,     BtnActTab,      MnuActTab,     ltActCategory,   ' (' + FActTab + ')', ' (' + FActTab + ')');
   SetItems (aAllComps,   BtnAllComps,    MnuAllComps,   ltAllComps,      '', '');
   SetItems (aActForm,    BtnActForm,     MnuActForm,    ltActForm,       '', '');
   SetItems (aHistory,    BtnHistory,     MnuHistory,    ltHistory,       '', '');
   SetItems (aFavorites,  BtnFavorites,   MnuFavorites,  ltFavorites,     '', '');
   SetItems (aCategories, BtnCategories,  MnuCategories, ltCfgCategory,   '', c3Dots);
   SetItems (aPalette,    BtnPalette,     MnuPalette,    ltCfgPalette,    '', c3Dots);
   SetItems (aConfig,     BtnConfig,      MnuConfig,     ltCfgPaletteBar, '', c3Dots);
   SetItems (aHelp,       BtnHelp,        MnuHelp,       ltShowHelp,      '', c3Dots);
   SetItems (aAbout,      BtnAbout,       MnuAbout,      ltAbout,         '', c3Dots);
  end;
 end else begin
   SetItems (aActTab,     nil, nil, ltActCategory,   ' (' + FActTab + ')', ' (' + FActTab + ')');
   SetItems (aAllComps,   nil, nil, ltAllComps,      '', '');
   SetItems (aActForm,    nil, nil, ltActForm,       '', '');
   SetItems (aHistory,    nil, nil, ltHistory,       '', '');
   SetItems (aFavorites,  nil, nil, ltFavorites,     '', '');
   SetItems (aCategories, nil, nil, ltCfgCategory,   '', c3Dots);
   SetItems (aPalette,    nil, nil, ltCfgPalette,    '', c3Dots);
   SetItems (aConfig,     nil, nil, ltCfgPaletteBar, '', c3Dots);
   SetItems (aHelp,       nil, nil, ltShowHelp,      '', c3Dots);
   SetItems (aAbout,      nil, nil, ltAbout,         '', c3Dots);
 end;

 // aktivieren, wenn offen
 aActTab.Enabled:= PB_Options.Open;
 aAllComps.Enabled:= PB_Options.Open;
 aActForm.Enabled:= PB_Options.Open;
 aHistory.Enabled:= PB_Options.Open;
 aFavorites.Enabled:= PB_Options.Open;
 aHelp.Enabled:= PB_Options.Open;
end;

//------------------------------------------------------------------------------
procedure TBase.ReadCategories;
var s, rk: string;
    sl: TStrings;
    t1, t2: TTreeNode;
    i, j: Integer;
    pc: PtrComponent;
begin
 Screen.Cursor:= crHourGlass;
 Categories.Items.BeginUpdate;

 rk:= cRegVCLCat;
 {$ifdef ClxVersion}
  if (BorlandIDEServices as IOTAServices).GetActiveDesignerType = dCLX then rk:= cRegCLXCat;
 {$endif}

 // Vorhandene Liste einlesen
 ReadNodes (Categories, nil, rk);

 // Abgleichen mit den tatsächlichen Kategorien
 sl:= GetTabs;
 for i:= 0 to Pred (sl.Count) do begin

  if not IsDuplicate (Categories, sl[i]) then begin
   t1:= Categories.Items.AddChild (nil, sl[i]);
   t1.ImageIndex:= cMetaCat;
   t1.SelectedIndex:= cMetaCatSel;

   // Komponenten eintragen
   for j:= 0 to Pred (FComponentList.Count) do begin
    pc:= FComponentList[j];
    if Assigned (pc) then begin
     if pc.tabname = sl[i] then begin
      t2:= Categories.Items.AddChild (t1, pc.typename);
      t2.ImageIndex:= pc.imgindex;
      t2.SelectedIndex:= pc.imgindex;
      t2.Data:= pc;
      pc.tempcat:= pc.tabname;
     end;
    end;
   end; // for

  end; // if not IsDuplicate
 end;

 // Speichern
 WriteCategories;

 Categories.Items.AlphaSort(False);
 Categories.Items.EndUpdate;
 Screen.Cursor:= crDefault;
end;

//------------------------------------------------------------------------------
procedure TBase.WriteCategories;
var s, rk: string;
begin
 Categories.Items.BeginUpdate;
 rk:= cRegVCLCat;
 {$ifdef ClxVersion}
  if (BorlandIDEServices as IOTAServices).GetActiveDesignerType = dCLX then rk:= cRegCLXCat;
 {$endif}
 gRegIni.EraseSection (rk);
 WriteNodes (Categories.Items.GetFirstNode, rk);
 Categories.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
initialization
 Pbar:= nil;
 gInList:= False;
finalization
 if Assigned (UnregisterFieldAddress) then UnregisterFieldAddress (Addr(PBar));
 Pbar:= nil;
end.
