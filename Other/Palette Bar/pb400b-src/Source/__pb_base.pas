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
  pb_common;
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

const pcHistory =  Bit0;
      pcFavorite = Bit1;
      pcChecked =  Bit2;


type PtrComponent=    ^TComponentItem;
     TComponentItem = record
                       typename:   string;
                       tabname:    string;
                       categories: string;
                       tempcat:    string;
                       imgindex:   Integer;
                       flags:      T32Bits;
                      end;

type
  TBase = class(TDataModule)
    MainImages: TImageList;
    ImageList: TImageList;
    ActionList: TActionList;
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
    procedure DataModuleCreate(Sender: TObject);
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
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure OnTabChange(Sender: TObject);
    procedure SwitchToTab (const TabName: string);
    procedure MenuClick(Sender: TObject);
  public
    { Public-Deklarationen }
    FDelphiMenu: TMainMenu;
    FTabControl: TTabControl;
    FComponentBar: TWinControl;
    FComponentMenu: TMenuItem;
    FConfigItem: TMenuItem;
    FPBMenu: TMenuItem;
    FTabChange: TNotifyEvent;
    FActTab: string;
    FPalette: TCustomControl;
    FComponentList: TList;
    procedure BuildCatMenu (menu: TMenuItem; mm: Boolean; p: Integer; click: TNotifyEvent);
    procedure TabMenuClickHandler (Sender: TObject);
    function  GetTabs: TStrings;
    function  GetActiveTab: string;
    procedure UpdateMenu;
    procedure UpdateLanguage;
    procedure GenComponentlist;
    function  GetComponent(const Name: string): PtrComponent;
    procedure CfgHistClear;
    function  AddImage (comp: string): integer;
  end;

var
  Base: TBase;

procedure Register;


implementation

{$R *.dfm}

uses Registry,
     pb_config, pb_categories, pb_about, pb_master;

var  gInList: Boolean;

//------------------------------------------------------------------------------
procedure Register;
var s: string;
begin

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

 while not Application.Active do Application.ProcessMessages;

 if Assigned (RegisterFieldAddress) then begin
  RegisterFieldAddress (cWizardName, Addr(PBar));
  RegisterDesktopFormClass (TPBar, cWizardName, cWizardName);
 end;

 Base:= TBase.Create (Application);

end;

//------------------------------------------------------------------------------
procedure TBase.DataModuleCreate(Sender: TObject);

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

 FComponentList:= TList.Create;

// while not Application.Active do Application.ProcessMessages;
// GenComponentList;

 UpdateLanguage;
 UpdateMenu;

// Create Mainform if we are in floating mode
// if PB_Options.Floating and not Assigned (PBar) then begin
//  Debug ('Base: Create Mainform');
//  PBar:= TPBar.Create (Application);
// end;
end;

//------------------------------------------------------------------------------
procedure TBase.DataModuleDestroy(Sender: TObject);
begin
 if Assigned (FTabChange) then begin
  FTabControl.OnChange:= FTabChange;
  FTabChange:= nil;
 end;
 FreeAndNil (FPBMenu);
 FreeAndNil (FComponentList);
end;

//-----------------------------------------------------------------------------
procedure TBase.aShowMenuExecute(Sender: TObject);
begin
// Must exist to enable the entry!
end;

//-----------------------------------------------------------------------------
procedure TBase.aActualTabExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.FillCompList (fmActTab);
end;

//-----------------------------------------------------------------------------
procedure TBase.aAllComponentsExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.FillCompList (fmAllTabs);
end;

//-----------------------------------------------------------------------------
procedure TBase.aActualFormExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.FillCompList (fmActForm);
end;

//-----------------------------------------------------------------------------
procedure TBase.aHistoryExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.FillCompList (fmHistory);
end;

//-----------------------------------------------------------------------------
procedure TBase.aFavoritesExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.FillCompList (fmFavorites);
end;

//-----------------------------------------------------------------------------
procedure TBase.aEditCategoryExecute(Sender: TObject);
var f: TPaletteCategories;
begin
 if Assigned (PBar) then PBar.BtnCategory.Down:= True;
 f:= TPaletteCategories.Create (Application);
 try
  if f.ShowModal = mrOk then begin
   f.SaveCategories;
  end;
 finally
  f.Free;
 end;
 if Assigned (PBar) then PBar.SetTabButtons;
end;

//-----------------------------------------------------------------------------
procedure TBase.aConfigPaletteExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.BtnConfPalette.Down:= True;
 if Assigned (FConfigItem) then FConfigItem.Action.Execute;
 if Assigned (PBar) then PBar.SetTabButtons;
end;

//-----------------------------------------------------------------------------
procedure TBase.aConfPaletteBarExecute(Sender: TObject);
var f: TPaletteConfig;
begin
 if Assigned (PBar) then PBar.BtnCategory.Down:= True;
 f:= TPaletteConfig.Create (Application);
 try
  if f.ShowModal = mrOk then begin
   f.SetOptions;
   if Assigned (PBar) then PBar.UpdateOptions;
  end;
 finally
  f.Free;
 end;
 if Assigned (PBar) then PBar.SetTabButtons;
end;

//-----------------------------------------------------------------------------
procedure TBase.aHelpExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.HelpComponent;
end;

//-----------------------------------------------------------------------------
procedure TBase.aAboutExecute(Sender: TObject);
var f: TpbAboutFrm;
begin
 f:= TpbAboutFrm.Create (Application);
 try
  if Assigned (PBar) then PBar.BtnAbout.Down:= True;
  f.ShowModal;
  if Assigned (PBar) then PBar.SetTabButtons;
 finally
  f.Free;
 end;
end;

//-----------------------------------------------------------------------------
procedure TBase.aFavAddExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.AddFavorite;
end;

//-----------------------------------------------------------------------------
procedure TBase.aFavRemoveExecute(Sender: TObject);
begin
 if Assigned (PBar) then PBar.RemoveFavorite;
end;

//-----------------------------------------------------------------------------
procedure TBase.aShowPaletteBarExecute(Sender: TObject);
begin
 if Assigned (PBar) then begin
  Pbar.Show;
 end else begin
  PBar:= TPBar.Create (Application);
  Pbar.Show;
 end;
end;

//------------------------------------------------------------------------------
procedure TBase.MenuClick(Sender: TObject);
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
procedure TBase.OnTabChange(Sender: TObject);
begin
 FTabChange (Sender);
 PBar.UpdateControls;
 if gInList then Exit;
 PBar.FillCompList (fmActTab);
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
procedure TBase.BuildCatMenu(menu: TMenuItem; mm: Boolean;
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
function TBase.GetTabs: TStrings;
begin
 Result:= FTabControl.Tabs;
end;

//------------------------------------------------------------------------------
function TBase.GetActiveTab: string;
begin
 Result:= FTabControl.Tabs[FTabControl.TabIndex];
end;

//------------------------------------------------------------------------------
procedure TBase.SwitchToTab (const TabName: string);
begin
 if not Assigned (FTabChange) then begin
  FTabChange:= FTabControl.OnChange;
  FTabControl.OnChange:= OnTabChange;
 end;
 FTabControl.TabIndex:= FTabControl.Tabs.IndexOf (TabName);
 FTabControl.OnChange (FTabControl);
end;

//------------------------------------------------------------------------------
procedure TBase.UpdateMenu;
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
procedure TBase.UpdateLanguage;

 procedure SetAction (Action: TAction; txt: TLanguageText; const y, x: string);
 var s: string;
 begin
  s:= GetLangStr (txt);
  Action.Caption:= s + y;
  Action.Hint:= s + x;
 end;

begin
 LoadLanguage (Pb_Options.Language);

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

 // Favoriten Menü
 aFavAdd.Caption:= GetLangStr (ltFavAdd);
 aFavRemove.Caption:= GetLangStr (ltFavRemove);

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

  TabMenu.Items[Pred (N3.MenuIndex)].Caption:= GetLangStr (ltCfgPaletteBar);
  TabMenu.Items[Pred (N3.MenuIndex)].ImageIndex:= 8;
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
//      ps: IOTAPackageServices;
begin
 // Komponentenliste erstellen
 if gInList then Exit;
 gInList:= True;

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

//-----------------------------------------------------------------------------
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
function TBase.AddImage (comp: string): integer;
var bitmap: TBitmap;
    col: TColor;
    old: TPersistentClass;
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
   itemx.Paint (Bitmap.Canvas, 0, 0); // Das Bitmap kopieren
   Result:= Base.ImageList.AddMasked (bitmap, col);
   bitmap.Free;
  end;
 end;

 {$ifdef ClxVersion} ActivateClassGroup (old); {$endif}
end;


//------------------------------------------------------------------------------
initialization
 Pbar:= nil;
 gInList:= False;
finalization
 if Assigned (UnregisterFieldAddress) then UnregisterFieldAddress (Addr(PBar));
 Pbar:= nil;
end.
