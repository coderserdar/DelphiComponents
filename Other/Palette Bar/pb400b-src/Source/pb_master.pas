//----------------------------------------------------------------------------
// Unit Name: pb_master
// Author:    Helli
// Date:      01.06.2004
// Purpose:
// History:
//----------------------------------------------------------------------------
//  Copyright © 2003/2004 by Hellinger Software.  All Rights Reserved.
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
  ImgList, ToolsAPI, DesignIntf, IniFiles, TypInfo, ComponentDesigner,
  pb_common, pb_Categories, pb_Config, pb_About, pb_Extinfo;
{$endif}
{$ifndef ClxVersion}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Buttons,
  Controls, Forms,
  DockForm, DeskForm, DeskUtil,
  Dialogs, ActnList, Menus, ComCtrls, ToolWin, StdCtrls, ExtCtrls,
  ImgList, ToolsAPI, DesignIntf, IniFiles, TypInfo, ComponentDesigner,
  pb_common, pb_Categories, pb_Config, pbAbout, pb_Extinfo;
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

type
  TPBar = class(TDockableForm)
    Panel1: TPanel;
    SearchPanel: TPanel;
    FavMenu: TPopupMenu;
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
    CfgBtnTop: TMenuItem;
    CfgBtnLeft: TMenuItem;
    CfgBtnRight: TMenuItem;
    CfgPalShow: TMenuItem;
    CfgPalText: TMenuItem;
    CfgPalHottrack: TMenuItem;
    CfgPalViewKachel: TMenuItem;
    CfgPalViewSymbol: TMenuItem;
    CfgPalViewList: TMenuItem;
    CfgPalViewText: TMenuItem;
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
    ListView1: TListView;
    ListView2: TListView;
    Header: THeaderControl;
    ToolBar: TToolBar;
    TabMenu: TPopupMenu;
    MnuTearLine: TMenuItem;
    TreeView1: TTreeView;
    CfgBtn10: TMenuItem;
    TreeView2: TTreeView;
    CfgBtn11: TMenuItem;
    CfgBtn12: TMenuItem;
    CfgBtn13: TMenuItem;
    ActionList: TActionList;
    aCatActual: TAction;
    aCatAll: TAction;
    aCatTree: TAction;
    aCatForm: TAction;
    aCatHistory: TAction;
    aCatFavorites: TAction;
    aCompInfo: TAction;
    aCompHelp: TAction;
    aCompAddFav: TAction;
    aCompDelFav: TAction;
    aConfCat: TAction;
    aConfPB: TAction;
    aAbout: TAction;
    aCatActual1: TMenuItem;
    aCatAll1: TMenuItem;
    aCatTree1: TMenuItem;
    aCatForm1: TMenuItem;
    aCatHistory1: TMenuItem;
    aCatFavorites1: TMenuItem;
    N1: TMenuItem;
    aCompInfo1: TMenuItem;
    aCompHelp1: TMenuItem;
    aCompAddFav1: TMenuItem;
    aCompDelFav1: TMenuItem;
    N2: TMenuItem;
    aConfCat1: TMenuItem;
    aConfPB1: TMenuItem;
    aAbout1: TMenuItem;
    aCompInfo2: TMenuItem;
    aCompHelp2: TMenuItem;
    aCompAddFav2: TMenuItem;
    aCompDelFav2: TMenuItem;
    aCatActualBtn: TToolButton;
    aCatAllBtn: TToolButton;
    aCatTreeBtn: TToolButton;
    aCatFormBtn: TToolButton;
    aCatHistoryBtn: TToolButton;
    aCatFavoritesBtn: TToolButton;
    ToolButton7: TToolButton;
    aCompInfoBtn: TToolButton;
    aCompHelpBtn: TToolButton;
    aCompAddFavBtn: TToolButton;
    aCompDelFavBtn: TToolButton;
    ToolButton12: TToolButton;
    aConfCatBtn: TToolButton;
    aConfPBBtn: TToolButton;
    aAboutBtn: TToolButton;
    aCompAddBtn: TToolButton;
    aCompAddInfo: TAction;
    aCompAddInfo1: TMenuItem;
    aCompAddInfo2: TMenuItem;
    CfgBtn14: TMenuItem;
    ImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure HeaderSectionClick(HeaderControl: THeaderControl; Section: THeaderSection);
    procedure A2ZClick(Sender: TObject);
    procedure Liste1Click(Sender: TObject);
    procedure Liste1Compare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure Liste1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure Liste1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Liste1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Liste1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Liste1GetImageIndex(Sender: TObject; Item: TListItem);
    procedure Liste1DblClick(Sender: TObject);
    procedure BtnGoClick(Sender: TObject);
    procedure CfgBtn10Click(Sender: TObject);
    procedure CfgBtnRightClick(Sender: TObject);
    procedure CfgPalViewListClick(Sender: TObject);
    procedure CfgPalTextSmallClick(Sender: TObject);
    procedure CfgPalHottrackClick(Sender: TObject);
    procedure CfgA2ZShowClick(Sender: TObject);
    procedure CfgHistSaveClick(Sender: TObject);
    procedure CfgSearchBottomClick(Sender: TObject);
    procedure CfgBtnVizClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure CfgHistClearClick(Sender: TObject);
    procedure TreeView1GetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Compare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure aCatAllExecute(Sender: TObject);
    procedure aCatActualExecute(Sender: TObject);
    procedure aCatTreeExecute(Sender: TObject);
    procedure aCatFormExecute(Sender: TObject);
    procedure aCatHistoryExecute(Sender: TObject);
    procedure aCatFavoritesExecute(Sender: TObject);
    procedure aCompInfoExecute(Sender: TObject);
    procedure aCompHelpExecute(Sender: TObject);
    procedure aCompAddFavExecute(Sender: TObject);
    procedure aCompDelFavExecute(Sender: TObject);
    procedure aConfCatExecute(Sender: TObject);
    procedure aConfPBExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure aCompAddInfoExecute(Sender: TObject);
  private { Private-Deklarationen }
    FFilter: char;
    FSortAsc: Boolean;
    FPalSorting: TPalSorting;
    FDragging: TDragging;
    FFormDesigner: IDesigner;
    FOpen: Boolean;
    FListe: TListView;
    FTree:  TTreeview;

    // Buttons
    procedure BtnUpdate;
    procedure BtnConfigure;

    // Header
    procedure GenMenu;
    procedure HeaderMenu (show: Boolean);
    procedure HeaderSort;
    procedure HeaderNext;
    procedure HeaderPrev;

    // A-Z-Bar
    procedure A2ZConfigure (i: Integer);
    procedure A2ZReset;

    // Listview / TreeView
    // procedure SetListView; // must be public
    // procedure SetTreeView; // must be public
    procedure ItemSelected (pc: PtrComponent; sel: Boolean);
    procedure SetTextSize;
    procedure Compareitems (p1, p2: PtrComponent; var Compare: Integer);

    // Componentdisplay
    procedure AddListItem (pc: PtrComponent; all: boolean);
    procedure FillActTab (all, search: Boolean);
    procedure FillHistFav (hist: Boolean);
    procedure FillActForm;

    // Sonstige
    function  GetImageIndex(pc: PtrComponent): Integer;
    function  GetContextData: PtrComponent; overload;
    function  GetContextData (x, y: Integer): PtrComponent; overload;
    function  GetNumbers: string;
    function  GetActComponent: PtrComponent;
    function  GetActiveFormEditor: IOTAFormEditor;
    function  GetActiveFormDesigner: IDesigner;
    procedure LoadList (mode: TRegMode);
    function  IsTarget (const s: string): Boolean;
    procedure FormSavePos;
  public  { Public-Deklarationen }
    procedure SetListView;
    procedure SetTreeView;
    procedure FillCompList (mode: TFillmode);
    procedure UpdateOptions;
    procedure ClearList(mode: TRegMode);
    procedure LoadWindowState (Desktop: TMemIniFile); override;
    procedure SaveWindowState (Desktop: TMemIniFile; isProject: Boolean); override;
  end;

var PBar: TPBar;

implementation

{$R *.dfm}

uses Registry;


const cIconUnsort = 16;
      cIconAZ =     17;
      cIconZA =     18;

const cHeadMenu =   0;
      cHeadText =   1;
      cHeadLeft =   2;
      cHeadRight =  3;


//==============================================================================
// Actions
//==============================================================================

procedure TPBar.aCatActualExecute(Sender: TObject);
begin
 SetListView;
 FillCompList (fmActTab);
 BtnUpdate;
end;

//------------------------------------------------------------------------------
procedure TPBar.aCatAllExecute(Sender: TObject);
begin
 SetListView;
 FillCompList (fmAllTabs);
 BtnUpdate;
end;

//------------------------------------------------------------------------------
procedure TPBar.aCatTreeExecute(Sender: TObject);
begin
 SetTreeView;
 BtnUpdate;
end;

//------------------------------------------------------------------------------
procedure TPBar.aCatFormExecute(Sender: TObject);
begin
 SetListView;
 FillCompList (fmActForm);
 BtnUpdate;
end;

//------------------------------------------------------------------------------
procedure TPBar.aCatHistoryExecute(Sender: TObject);
begin
 SetListView;
 FillCompList (fmHistory);
 BtnUpdate;
end;

//------------------------------------------------------------------------------
procedure TPBar.aCatFavoritesExecute(Sender: TObject);
begin
 SetListView;
 FillCompList (fmFavorites);
 BtnUpdate;
end;

//------------------------------------------------------------------------------
procedure TPBar.aCompAddInfoExecute(Sender: TObject);
var pc: PtrComponent;
begin
 aCompAddBtn.Down:= True;
 pc:= GetContextData;
 if Assigned (pc) then begin
  EdExinfo:= TEdExinfo.Create (Application);
  SetFormPos (EdExinfo, cRegEdExInfo);
  EdExinfo.Info.Text:= pc.extinfo;
  try
   if EdExinfo.ShowModal = mrOk then begin
    if pc.extinfo <> EdExinfo.Info.Text then begin
     pc.extinfo:= EdExinfo.Info.Text;
     Categories.WriteCategories;
    end;
    ItemSelected (pc, true);
    UpdateOptions;
   end;
  finally
   EdExinfo.Free;
  end;
 end;
 aCompAddBtn.Down:= False;
end;

//------------------------------------------------------------------------------
procedure TPBar.aCompInfoExecute(Sender: TObject);
var pc: PtrComponent;
begin
 pc:= GetContextData;
 if Assigned (pc) then begin
  if pc.extinfo <> '' then  RunApp (pc.extinfo, '');
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.aCompHelpExecute(Sender: TObject);
var s: string;
    pc: PtrComponent;
begin
 pc:= GetContextData;
 if Assigned (pc) then begin
  s:= pc.typename;
  if s = 'TFrames' then s:= 'TFrame';
  Application.HelpKeyword (s);
 end;
 BtnUpdate;
end;

//------------------------------------------------------------------------------
procedure TPBar.aCompAddFavExecute(Sender: TObject);
var p: PtrComponent;
begin
 if Assigned (FListe.Selected) then begin
  p:= FListe.Selected.Data;
  if Assigned (p) then begin
   Include (p.flags, pcFavorite);
   gRegIni.WriteString (cRegFavorites, p.typename, '1');
  end;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.aCompDelFavExecute(Sender: TObject);
var p: PtrComponent;
begin
 if Assigned (FListe.Selected) then begin
  p:= FListe.Selected.Data;
  if Assigned (p) then begin
   Exclude (p.flags, pcFavorite);
   gRegIni.DeleteKey(cRegFavorites, p.typename);
   FillCompList (fmFavorites);
  end;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.aConfCatExecute(Sender: TObject);
begin
 aConfCatBtn.Down:= True;
 Categories.RunCategories;
 aConfCatBtn.Down:= False;
end;

//------------------------------------------------------------------------------
procedure TPBar.aConfPBExecute(Sender: TObject);
begin
 aConfPbBtn.Down:= True;
 Config:= TPaletteConfig.Create (Application);
 SetFormPos (Config, cRegConfig);
 try
  if Config.ShowModal = mrOk then begin
   Config.SetOptions;
   UpdateOptions;
  end;
 finally
  Config.Free;
 end;
 aConfPbBtn.Down:= False;
end;

//------------------------------------------------------------------------------
procedure TPBar.aAboutExecute(Sender: TObject);
var f: TpbAboutFrm;
begin
 aAboutBtn.Down:= True;
 f:= TpbAboutFrm.Create (Application);
 try
  f.ShowModal;
 finally
  f.Free;
 end;
 aAboutBtn.Down:= False;
end;


//==============================================================================
// Buttonleiste, Buttons und Kontextmenü dazu
//==============================================================================

//------------------------------------------------------------------------------
procedure TPBar.BtnUpdate;
begin
 if PB_Options.DisplayMode = dmTree then begin
  aCatTreeBtn.Down:= True;
 end else begin
  case PB_Options.Fillmode of
   fmActTab:    aCatActualBtn.Down:= True;
   fmAllTabs:   aCatAllBtn.Down:= True;
   fmFavorites: aCatFavoritesBtn.Down:= True;
   fmHistory:   aCatHistoryBtn.Down:= True;
   fmActForm:   aCatFormBtn.Down:= True;
   fmFilter:    ;
   fmSearch:    ;
  end;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.BtnConfigure;
var b: TBit;
    i: Integer;
begin
 // Texte
 CfgBtnShow.Caption:=  GetLangStr (ltShowToolbar);
 CfgBtnPos.Caption:=   GetLangStr (ltToolbarPos);
 CfgBtnTop.Caption:=   GetLangStr (ltTop);
 CfgBtnLeft.Caption:=  GetLangStr (ltLeft);
 CfgBtnRight.Caption:= GetLangStr (ltRight);
 CfgBtnViz.Caption:=   GetLangStr (ltVisible);

 CfgBtn1.Caption:=     GetLangStr (ltActCategory);
 CfgBtn2.Caption:=     GetLangStr (ltAllComps);
 CfgBtn3.Caption:=     GetLangStr (ltTreeview);
 CfgBtn4.Caption:=     GetLangStr (ltActForm);
 CfgBtn5.Caption:=     GetLangStr (ltHistory);
 CfgBtn6.Caption:=     GetLangStr (ltFavorites);
 CfgBtn7.Caption:=     GetLangStr (ltEnterExtInfo);
 CfgBtn8.Caption:=     GetLangStr (ltShowExtInfo);
 CfgBtn9.Caption:=     GetLangStr (ltShowHelp);
 CfgBtn10.Caption:=    GetLangStr (ltFavAdd);
 CfgBtn11.Caption:=    GetLangStr (ltFavRemove);
 CfgBtn12.Caption:=    GetLangStr (ltCfgCategory);
 CfgBtn13.Caption:=    GetLangStr (ltCfgPaletteBar);
 CfgBtn14.Caption:=    GetLangStr (ltAbout);

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
 for b:= Bit0 to Bit13 do begin
  with CfgBtnShow.Items[Ord(b)] do begin
   if b in PB_Options.Buttons then ImageIndex:= cChecked
                              else ImageIndex:= Ord(b) + 1;
   for i:= 0 to Pred (ToolBar.ButtonCount) do begin
    if ToolBar.Buttons[i].Tag = Ord(b) then
     ToolBar.Buttons[i].Visible:= CfgbtnShow.Items[Ord(b)].ImageIndex = cChecked;
   end; // for i
  end; // with
 end; // for b
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgBtn10Click(Sender: TObject);
var btn: integer;

 procedure SwitchButton (b: TToolButton; img: Integer);
 begin
  b.Visible:= not b.Visible;
  if b.Visible then begin
   (Sender as TMenuItem).ImageIndex:= cChecked;
   Include (PB_Options.Buttons, TBit (img-1));
  end else begin
   (Sender as TMenuItem).ImageIndex:= img;
   Exclude (PB_Options.Buttons, TBit (img-1));
  end;
 end;

begin
 GetSender (Sender, stTag, btn);
 case btn of
   1: SwitchButton (aCatActualBtn,    btn);
   2: SwitchButton (aCatAllBtn,       btn);
   3: SwitchButton (aCatTreeBtn,      btn);
   4: SwitchButton (aCatFormBtn,      btn);
   5: SwitchButton (aCatHistoryBtn,   btn);
   6: SwitchButton (aCatFavoritesBtn, btn);
   7: SwitchButton (aCompAddBtn,      btn);
   8: SwitchButton (aCompInfoBtn,     btn);
   9: SwitchButton (aCompHelpBtn,     btn);
  10: SwitchButton (aCompAddFavBtn,   btn);
  11: SwitchButton (aCompDelFavBtn,   btn);
  12: SwitchButton (aConfCatBtn,      btn);
  13: SwitchButton (aConfPBBtn,       btn);
  14: SwitchButton (aAboutBtn,        btn);
 end;
 ToolBar.Update;
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
procedure TPBar.CfgBtnVizClick(Sender: TObject);
begin
 if PB_Options.Menu then begin
  ToolBar.Visible:= CfgBtnViz.Checked;
  PB_Options.ToolBarViz:= CfgBtnViz.Checked;
 end;
end;

//==============================================================================
// Headerzeile und Kontextmenü
//==============================================================================

//------------------------------------------------------------------------------
procedure TPBar.GenMenu;
var i: Integer;
begin
 if PB_Options.BuildTabMnu then begin
  // Kategorie-Menü löschen
  for i:= Pred (TabMenu.Items.IndexOf(MnuTearLine)) downto 0 do TabMenu.Items.Delete (i);
  // Menü erzeugen
  Categories.BuildCatMenu (TabMenu.Items, False, TabMenu.Items.IndexOf(MnuTearLine));
  PB_Options.BuildTabMnu:= False;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.HeaderMenu (show: Boolean);
var p: TPoint;
    i: Integer;
begin
 // TraceIn ('TPBar.HeaderMenu');
 GenMenu;
 if show then begin
  p:= Header.ClientToScreen (Point (Header.Top, Header.Left));
  TabMenu.Popup(p.X, p.Y);
 end;
 // TraceOut ('TPBar.HeaderMenu');
end;

//------------------------------------------------------------------------------
procedure TPBar.HeaderSort;
var n: TTreeNode;
begin
 // TraceIn ('TPBar.HeaderSort');
 case FPalSorting of
  psAZ:   begin
           FSortAsc:= True;
           FPalSorting:= psZA;
           Header.Sections[cHeadText].ImageIndex:= cIconAZ;
          end;
  psZA:   begin
           FSortAsc:= False;
           if PB_Options.Fillmode = fmActTab then FPalSorting:= psNone
                                             else FPalSorting:= psAZ;
           Header.Sections[cHeadText].ImageIndex:= cIconZA;
          end;
  psNone: begin
           FillCompList (PB_Options.Fillmode);
           FPalSorting:= psAZ;
           Header.Sections[cHeadText].ImageIndex:= cIconUnsort;
           // TraceOut ('TPBar.HeaderSort-Exit');
           Exit;
          end;
 end;
 if PB_Options.DisplayMode = dmTree then begin
  n:= FTree.Selected;
  if n.ImageIndex > cCategory then n:= FTree.Selected.Parent;
  n.AlphaSort (false);
 end else begin
  FListe.AlphaSort;
 end;
 // TraceOut ('TPBar.HeaderSort');
end;

//------------------------------------------------------------------------------
procedure TPBar.HeaderNext;
var m: TMenuItem;
    i: Integer;
    sl: TStringList;
begin
 // TraceIn ('TPBar.HeaderNext');
 GenMenu;
 sl:= TStringList.Create;
 try
  Categories.GetCatTabs (sl);
  i:= sl.IndexOf(PB_Options.ActTab);
  Inc (i);  if i >= sl.Count then i:= 1;
  m:= FindMenu (TabMenu.Items, sl[i]);
  if m.Count > 0 then m:= m.Items[0];
  Categories.SwitchToTab (m.Caption, m.Tag);
 finally
  sl.Free;
 end;
 // TraceOut ('TPBar.HeaderNext');
end;

//------------------------------------------------------------------------------
procedure TPBar.HeaderPrev;
var m: TMenuItem;
    i: Integer;
    sl: TStringList;
begin
 // TraceIn ('TPBar.HeaderPrev');
 GenMenu;
 sl:= TStringList.Create;
 try
  Categories.GetCatTabs (sl);
  i:= sl.IndexOf(PB_Options.ActTab);
  Dec (i);  if i <= 0 then i:= sl.Count - 1;
  m:= FindMenu (TabMenu.Items, sl[i]);
  if m.Count > 0 then m:= m.Items[0];
  Categories.SwitchToTab (m.Caption, m.Tag);
 finally
  sl.Free;
 end;
 // TraceOut ('TPBar.HeaderPrev');
end;

//------------------------------------------------------------------------------
procedure TPBar.HeaderSectionClick(HeaderControl: THeaderControl; Section: THeaderSection);
begin
 // TraceIn ('TPBar.HeaderSectionClick');
 case Section.Index of
  cHeadMenu:  HeaderMenu (True);
  cHeadText:  HeaderSort;
  cHeadLeft:  HeaderPrev;
  cHeadRight: HeaderNext;
 end;
 // TraceOut ('TPBar.HeaderSectionClick');
end;

//==============================================================================
// A-Z-Bar
//==============================================================================

//------------------------------------------------------------------------------
procedure TPBar.A2ZConfigure (i: Integer);
begin
 // TraceIn ('TPBar.A2ZConfigure');
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
 // TraceOut ('TPBar.A2ZConfigure');
end;

//------------------------------------------------------------------------------
procedure TPBar.A2ZReset;
var i: integer;
begin
 // TraceIn ('TPBar.A2ZReset');
 for i:= 0 to Pred (A2ZBar.ControlCount) do begin
  (A2ZBar.Controls[i] as TSpeedButton).Down:= False;
 end;
 // TraceOut ('TPBar.A2ZReset');
end;

//------------------------------------------------------------------------------
procedure TPBar.A2ZClick(Sender: TObject);
//#todo1 18.03.04 Tree berücksichtigen!
var ch: char;
    s: string;
    i: Integer;
begin
 // TraceIn ('TPBar.A2ZClick');
 ch:= (Sender as TSpeedButton).Caption[1];
 if PB_Options.A2ZFilter then begin
  FFilter:= ch;
  FillCompList (fmFilter);
 end else begin
  for i:= 0 to Pred (FListe.Items.Count) do begin
   s:= FListe.Items[i].Caption;
   if UpCase(s[1]) = ch then begin
    FListe.Items[i].MakeVisible(False);
    // TraceOut ('TPBar.aaaaaaaaaa-Exit');
    Exit;
   end;
  end;
 end;
 // TraceOut ('TPBar.A2ZClick');
end;

//==============================================================================
// Suchbalken
//==============================================================================

//-----------------------------------------------------------------------------
procedure TPBar.CfgSearchBottomClick(Sender: TObject);
var i: integer;
begin
 // TraceIn ('TPBar.CfgSearchBottomClick');
 GetSender (Sender, stTag, i);
 case i of
  1, 2: begin
         PB_Options.SearchBar:= TAlign (i);
         SearchPanel.Align:= TAlign (i);
        end;
  3:    begin
         PB_Options.SearchBarViz:= (Sender as TMenuItem).Checked;
         SearchPanel.Visible:= (Sender as TMenuItem).Checked;
        end;
 end;
 // TraceOut ('TPBar.CfgSearchBottomClick');
end;


//==============================================================================
// Listview und TreeView
//==============================================================================

//------------------------------------------------------------------------------
procedure TPBar.SetListView;
begin
 // TraceIn ('TPBar.SetListView');
 ListView1.Enabled:= False;
 ListView1.Visible:= False;
 ListView2.Enabled:= False;
 ListView2.Visible:= False;
 TreeView1.Enabled:= False;
 TreeView1.Visible:= False;
 TreeView2.Enabled:= False;
 TreeView2.Visible:= False;
 if PB_Options.ShowMode = smText then begin
  if FListe <> ListView2 then begin
   FListe:= ListView2;
   ListView1.Items.Clear;
   FillCompList (PB_Options.Fillmode)
  end;
 end else begin
  if FListe <> ListView1 then begin
   FListe:= ListView1;
   ListView2.Items.Clear;
   FillCompList (PB_Options.Fillmode)
  end;
 end;
 case PB_Options.ShowMode of
  smKachel: FListe.ViewStyle:= vsIcon;
  smSymbol: FListe.ViewStyle:= vsReport;
  smList:   FListe.ViewStyle:= vsList;
  smText:   FListe.ViewStyle:= vsReport;
 end;
 FListe.HotTrack:= PB_options.Hottrack;
 PB_Options.DisplayMode:= dmList;
 BtnUpdate;
 FListe.Enabled:= True;
 FListe.Visible:= True;
 // TraceOut ('TPBar.SetListView');
end;

//------------------------------------------------------------------------------
procedure TPBar.SetTreeView;
var i: Integer;
begin
 // TraceIn ('TPBar.SetTreeView');
 ListView1.Enabled:= False;
 ListView1.Visible:= False;
 ListView2.Enabled:= False;
 ListView2.Visible:= False;
 if PB_Options.ShowMode = smText then begin
  TreeView1.Enabled:= False;
  TreeView1.Visible:= False;
  FTree:= TreeView2;
 end else begin
  TreeView2.Enabled:= False;
  TreeView2.Visible:= False;
  FTree:= TreeView1;
 end;
 FTree.OnChange:= nil;
 FTree.Items.BeginUpdate;
 FTree.Items.Clear;
 FTree.Items.Assign(Categories.CatList.Items);
 // Alle um eine Ebene nach oben schieben
 for i:= 0 to Pred (FTree.Items[0].Count) do  FTree.Items[0].Item[0].MoveTo(nil, naAdd);
 FTree.Items.Delete(FTree.Items[0]);
 FTree.HotTrack:= PB_Options.Hottrack;
 PB_Options.DisplayMode:= dmTree;
 Ftree.Selected:= nil;
 FTree.Items.EndUpdate;
 FillActTab (False, False);
 BtnUpdate;
 FTree.Enabled:= True;
 FTree.Visible:= True;
 FTree.OnChange:= TreeView1Change;
 // TraceOut ('TPBar.SetTreeView');
end;

//------------------------------------------------------------------------------
procedure TPBar.SetTextSize;
var i: Integer;
begin
 // TraceIn ('TPBar.SetTextSize');
 case PB_Options.TextSize of
  tsBig:    i:= 12;
  tsSmall:  i:= 8;
  else      i:= 10; // tsMedium
 end;
 ListView1.Font.Size:= i;
 ListView2.Font.Size:= i;
 TreeView1.Font.Size:= i;
 TreeView2.Font.Size:= i;
 // TraceOut ('TPBar.SetTextSize');
end;


//------------------------------------------------------------------------------
// Allgemeine Routinen
//------------------------------------------------------------------------------

function TPBar.GetImageIndex(pc: PtrComponent): Integer;
begin
 // TraceIn ('TPBar.GetImageIndex');
 Result:= -1;
 if Assigned (pc) then begin
  if pc.imgindex = cUnknown then  pc.imgindex:= Categories.AddImage (pc.typename);
  Result:= pc.imgindex;
 end;
 // TraceOut ('TPBar.GetImageIndex');
end;

//------------------------------------------------------------------------------
function TPBar.GetNumbers: string;
var i: Integer;
begin
 // TraceIn ('TPBar.GetNumbers');
 if PB_Options.DisplayMode = dmTree then begin
  if Assigned (FTree.Selected) then i:= FTree.Selected.Count
                               else i:= FTree.Items.Count;
 end else begin
  i:= FListe.Items.Count;
 end;
 Result:= '  (' + IntToStr(i) + ' / ' + IntToStr (Categories.FComponentList.Count) + ')';
 // TraceOut ('TPBar.GetNumbers');
end;

//------------------------------------------------------------------------------
function TPBar.GetContextData (x, y: Integer): PtrComponent;
var tc: TTreeNode;
    lc: TlistItem;
begin
 Result:= nil;
 if PB_Options.DisplayMode = dmTree then begin
  tc:= FTree.GetNodeAt (x, y);
  if Assigned (tc) then Result:= tc.Data;
 end else begin
  lc:= FListe.GetItemAt (x, y);
  if Assigned (lc) then Result:= lc.Data;
 end;
end;

//------------------------------------------------------------------------------
function TPBar.GetActComponent: PtrComponent;
begin
 // TraceIn ('TPBar.GetActComponent');
 Result:= nil;
 if PB_Options.DisplayMode = dmTree then begin
  if Assigned (Ftree.Selected) then Result:= Ftree.Selected.Data
 end else begin
  if Assigned (FListe.Selected) then Result:= FListe.Selected.Data
 end;
 // TraceOut ('TPBar.GetActComponent');
end;

//------------------------------------------------------------------------------
function TPBar.GetActiveFormDesigner: IDesigner;
var FormEditor: IOTAFormEditor;
begin
 // TraceIn ('TPBar.GetActiveFormDesigner');
 Result:= nil;
 FormEditor:= GetActiveFormEditor;
 if Assigned (FormEditor) then begin
  Result:= (FormEditor as INTAFormEditor).FormDesigner;
  // Nur wenn wir auch ein Form haben!!!
  if not Assigned (Result.Root) then Result:= nil;
 end;
 // TraceOut ('TPBar.GetActiveFormDesigner');
end;

//------------------------------------------------------------------------------
function TPBar.GetActiveFormEditor: IOTAFormEditor;
var Module: IOTAModule;
    Editor: IOTAEditor;
    i: Integer;
begin
 // TraceIn ('TPBar.GetActiveFormEditor');
 Result:= nil;
 Module:= (BorlandIDEServices as IOTAModuleServices).CurrentModule;
 if Assigned (Module) then begin
  for i := 0 to Pred (Module.GetModuleFileCount) do begin
   Editor:= Module.GetModuleFileEditor(i);
   Editor.QueryInterface(IOTAFormEditor, Result);
   if Assigned (Result) then Break;
  end;
 end;
 // TraceOut ('TPBar.GetActiveFormEditor');
end;

//------------------------------------------------------------------------------
function TPBar.IsTarget(const s: string): Boolean;
// True wenn das Objekt zum Einfügen gültig ist
begin
 // TraceIn ('TPBar.IsTarget');
 Result:= (s = FFormDesigner.Root.Name) or
          (s = cDataModuleForm) or
          (s = cDataModuleSurface) or
          (s = cWinControlForm) or
          (s = cWidgetControlForm);
 // TraceOut ('TPBar.IsTarget');
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
    p:= Categories.GetComponent (s[i]);
    case mode of
     rmHistory:   if Assigned (p) then Include (p.flags, pcHistory);
     rmFavorites: if Assigned (p) then Include (p.flags, pcFavorite);
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
 // TraceIn ('TPBar.ClearList');
 case mode of
  rmHistory:   begin
                gRegIni.EraseSection (cRegHistory);
                for i:= 0 to Pred (Categories.FComponentList.Count) do begin
                 p:= Categories.FComponentList[i];
                 if Assigned (p) then Exclude (p.flags, pcHistory);
                end;
                if PB_Options.Fillmode = fmHistory then  FillCompList (fmHistory);
               end;
  rmFavorites: begin
                gRegIni.EraseSection (cRegFavorites);
                for i:= 0 to Pred (Categories.FComponentList.Count) do begin
                 p:= Categories.FComponentList[i];
                 if Assigned (p) then Exclude (p.flags, pcFavorite);
                end;
                if PB_Options.Fillmode = fmFavorites then  FillCompList (fmFavorites);
               end;
  rmSearch:    begin
                gRegIni.EraseSection (cRegSearchText);
                Edit.Items.Clear;
               end;
 end;
 // TraceOut ('TPBar.ClearList');
end;

//------------------------------------------------------------------------------
procedure TPBar.AddListItem (pc: PtrComponent; all: Boolean);
var s: string;
    li: TListItem;
begin
 // TraceIn ('TPBar.AddListItem');
 if Assigned (pc) then begin
  s:= pc.typename;
  Delete (s, 1, 1);
  if all then s:= s + '  (' + pc.tabname + ')'
         else s:= s;
  // Checken ob schon da
  li:= FListe.FindCaption(0, s, False, True, True);
  if not Assigned (li) then begin
   li:= FListe.Items.Add;
   if Assigned (li) then begin
//    if pc.imgindex <= cUnKnown then pc.imgindex:= Categories.AddImage (pc.typename);
    li.Caption:= s;
    li.ImageIndex:= pc.imgindex;
    li.data:= pc;
   end;
  end;
 end;
 // TraceOut ('TPBar.AddListItem');
end; // AddListItem

//------------------------------------------------------------------------------
procedure TPBar.FillActTab (all, search: Boolean);
var i: Integer;
    tn: TTreeNode;

 procedure AddComp (tn: TTreeNode);
 var pc: PtrComponent;
 begin
  if tn.ImageIndex >= cUnknown then begin
   pc:= tn.Data;
   if search then begin
    if (StrIPosCI (Edit.Text, pc.typename, 0, -1) > 0) then AddListItem (pc, all);
   end else begin
    AddListItem (pc, all);
   end;
  end;
 end;

begin
 // TraceIn ('TPBar.FillActTab');
 if PB_Options.DisplayMode = dmTree then begin
  FTree.Items.BeginUpdate;
  if Assigned (FTree.Selected) then FTree.Selected.Collapse(false);
  for i:= 0 to Pred (FTree.Items.Count) do begin
   if FTree.Items[i].Text = PB_Options.ActTab then begin
    FTree.Selected:= FTree.Items[i];
    FTree.Selected.Expand(false);
    FTree.Selected.MakeVisible;
    Break;
   end;
  end;
  FTree.Items.EndUpdate;
 end else begin
  FListe.Items.Clear;
  if all then begin
   for i:= 0 to Pred (Categories.CatList.Items.Count) do begin
    AddComp (Categories.CatList.Items[i]);
   end;
   Header.Sections[cHeadText].Text:= GetLangStr (ltAllComps) + GetNumbers;
  end else begin
   tn:= Categories.GetEntry(PB_Options.ActTab).getFirstChild;
   while Assigned (tn) do begin
    AddComp (tn);
    tn:= tn.getNextSibling;
   end;
  end;
 end;
 aCatActual.Hint:= GetLangStr (ltActCategory) + ' (' + PB_Options.ActTab + ')';
 Header.Sections[cHeadText].Text:= PB_Options.ActTab + GetNumbers;
 Header.Sections[cHeadText].ImageIndex:= cIconUnsort; // Nicht sortiert!
 // TraceOut ('TPBar.FillActTab');
end;

//------------------------------------------------------------------------------
procedure TPBar.FillHistFav (hist: Boolean);
var i: Integer;
    pc: PtrComponent;
    s: string;
begin
 FListe.Items.Clear;
 for i:= 0 to Pred (Categories.FComponentList.Count) do begin
  pc:= Categories.FComponentList[i];
  if Assigned (pc) then begin
   if hist and (pcHistory in pc.flags) then AddListItem (pc, True);
   if ((not hist) and (pcFavorite in pc.flags)) then AddListItem (pc, True);
  end;
 end;
 if hist then begin
  s:= GetLangStr (ltHistory);
 end else begin
  s:= GetLangStr (ltFavorites);
  aCompAddFav.Enabled:= False;
  aCompDelFav.Enabled:= True;
 end;
 Header.Sections[cHeadText].Text:= s + GetNumbers;
end;

//------------------------------------------------------------------------------
procedure TPBar.FillActForm;
var i: Integer;
    pc: PtrComponent;
begin
 FListe.Items.Clear;
 FFormDesigner:= GetActiveFormDesigner;
 if Assigned (FFormDesigner) then begin
  Header.Sections[1].Text:= GetLangStr (ltForm) + ' (' + FFormDesigner.Root.Name + ')';
  for i:= 0 to Pred (FFormDesigner.Root.ComponentCount) do begin
   pc:= Categories.GetComponent(FFormDesigner.Root.Components[i].ClassName);
   if Assigned (pc) then  AddListItem (pc, True);
  end;
 end else begin
  Header.Sections[cHeadText].Text:= GetLangStr (ltErrorNoForm);
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.FillCompList(mode: TFillmode);
var i: Integer;
    pc: PtrComponent;
    tmp: TList;
begin
 // TraceIn ('TPBar.FillCompList');
 // Liste füllen
 StatusBar.Panels[0].Text:= '---';
 Categories.GenComponentlist;

 Screen.Cursor:= crAppStart;
 FListe.Items.BeginUpdate;

 if mode = fmFilter then begin
  case PB_Options.Fillmode of
   fmActTab:    FillActTab (False, False);
   fmAllTabs:   FillActTab (True, False);
   fmFavorites: FillHistFav (False);
   fmHistory:   FillHistFav (True);
   fmActForm:   FillActForm;
  end;
  tmp:= TList.Create;
  try
   for i:= 0 to pred (FListe.Items.Count) do begin
    pc:= FListe.Items[i].Data;
    if Assigned (pc) then begin
     if UpCase (pc.typename[2]) = FFilter then tmp.Add(pc);
    end;
   end;
   FListe.Items.Clear;
   for i:= 0 to Pred (tmp.Count) do begin
    pc:= tmp[i];
    if Assigned (pc) then AddListItem (pc, PB_Options.Fillmode <> fmActTab);
   end;
   Header.Font.Style:= [fsItalic];
  finally
   tmp.Free;
  end;
  FPalsorting:= psAZ;
  HeaderSectionClick (Header, Header.Sections[cHeadText]);
  aCompAddFav.Enabled:= True;
  aCompDelFav.Enabled:= False;
 end else begin
  Header.Font.Style:= [];
  A2ZReset;
  FPalsorting:= psAZ;
  aCompAddFav.Enabled:= True;
  aCompDelFav.Enabled:= False;

  case mode of
   fmActTab:    FillActTab (False, False);
   fmAllTabs:   begin
                 FillActTab (True, False);
                 HeaderSectionClick (Header, Header.Sections[cHeadText]);
                end;
   fmHistory,
   fmFavorites: begin
                 if mode = fmHistory then LoadList (rmHistory)
                                     else LoadList (rmFavorites);
                 FillHistFav (mode = fmHistory);
                 HeaderSectionClick (Header, Header.Sections[cHeadText]);
                 aCompAddFav.Enabled:= (mode = fmHistory);
                end;
   fmActForm:   begin
                 FillActForm;
                 HeaderSectionClick (Header, Header.Sections[cHeadText]);
                end;
   fmSearch:    begin
                 FillActTab (True, True);
                 FPalsorting:= psAZ;
                 Header.Sections[1].Text:= GetLangStr (ltSearchResult) + GetNumbers;
                 HeaderSectionClick (Header, Header.Sections[cHeadText]);
                end;
  end; // case
 end;

 if mode <> fmFilter then PB_Options.Fillmode:= mode;
 BtnUpdate;
 StatusBar.Panels[0].Text:= PB_Options.LibName;
 FListe.Items.EndUpdate;
 Screen.Cursor:= crDefault;

 // TraceOut ('TPBar.FillCompList');
end;

//------------------------------------------------------------------------------
// Eventhandler
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
procedure TPBar.Liste1Click(Sender: TObject);
begin
 FDragging:= dmNone;
 Windows.ReleaseCapture;
 Screen.Cursor:= crDefault;
end;

//------------------------------------------------------------------------------
procedure TPBar.Liste1DblClick(Sender: TObject);
var fe: IOTAFormEditor;
    cp: IOTAComponent;
    pc: PtrComponent;
//    p1, p2: TPoint;
begin
 Windows.ReleaseCapture;
 Screen.Cursor:= crDefault;
 FFormDesigner:= GetActiveFormDesigner;
 if Assigned (FFormDesigner) then begin
  fe:= GetActiveFormEditor;
  if Assigned (fe) then begin // FormEditor holen
   cp:= fe.GetCreateParent; // und das Elternelement suchen
   if Assigned (cp) then begin
    pc:= GetActComponent;
    if Assigned (pc) then begin
     cp:= fe.CreateComponent(cp, pc.typename, 0, 0, -1, -1);
     if Assigned (cp) then begin
      Include (pc.flags, pcHistory);
      gRegIni.WriteString (cRegHistory, pc.typename, '1');
     end else begin
      MessageDlg(GetLangStr (ltErrorInsert), mtError, [mbOK], 0);
     end;
    end;
   end; // if Assigned (cp)
  end; // if Assigned (fe)
 end; // if Assigned (FFormDesigner)
 FFormDesigner:= nil;
 FDragging:= dmNone;
end;


//------------------------------------------------------------------------------
procedure TPBar.CompareItems (p1, p2: PtrComponent; var Compare: Integer);
begin
 if FSortAsc then Compare:= AnsiCompareStr (p1.typename, p2.typename)
             else Compare:= AnsiCompareStr (p2.typename, p1.typename);
end;

//------------------------------------------------------------------------------
procedure TPBar.Liste1Compare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
 CompareItems (Item1.Data, Item2.Data, Compare);
end;

//------------------------------------------------------------------------------
procedure TPBar.TreeView1Compare(Sender: TObject; Node1, Node2: TTreeNode;
  Data: Integer; var Compare: Integer);
begin
 CompareItems (Node1.Data, Node2.Data, Compare);
end;

//------------------------------------------------------------------------------
procedure TPBar.Liste1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var p: TPoint;
    pc: PtrComponent;
begin
 if PB_Options.DisplayMode = dmTree then p:= FTree.ClientToScreen (MousePos)
                                    else p:= FListe.ClientToScreen (MousePos);

 pc:= GetContextData (MousePos.X, MousePos.Y);
 if Assigned (pc) then begin
  FavMenu.Popup (p.X, p.Y);
 end else begin
  if (PB_Options.DisplayMode = dmTree) then begin
   CfgPalViewKachel.Visible:= false;
   CfgPalViewSymbol.Visible:= False;
   CfgPal.Popup (p.X, p.Y);
  end else begin
   if PB_Options.Fillmode = fmHistory then CfgHist.Popup (p.X, p.Y)
                                      else CfgPal.Popup (p.X, p.Y);
  end;
 end;
 Handled:= True;
end;

//------------------------------------------------------------------------------
procedure TPBar.Liste1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case key of
  vk_F1: aCompHelp.Execute;
 end;
end;

//------------------------------------------------------------------------------
procedure TPBar.Liste1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var pc: PtrComponent;
begin
 if Button = mbLeft then begin
  pc:= GetContextData (X, Y);
  if Assigned (pc) then begin
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
procedure TPBar.Liste1GetImageIndex(Sender: TObject; Item: TListItem);
begin
 if Item.ImageIndex = cUnknown then Item.ImageIndex:= GetImageIndex (Item.Data);
end;

//------------------------------------------------------------------------------
procedure TPBar.TreeView1GetImageIndex(Sender: TObject; Node: TTreeNode);
begin
 if Node.ImageIndex = cUnknown then Node.ImageIndex:= GetImageIndex (Node.Data);
 Node.SelectedIndex:= Node.ImageIndex;
end;

//------------------------------------------------------------------------------
procedure TPBar.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
 ItemSelected (Item.Data, Selected);
end;

//------------------------------------------------------------------------------
procedure TPBar.TreeView1Change(Sender: TObject; Node: TTreeNode);

 procedure SwitchTab (const s: string);
 begin
  Categories.CheckItem (False);
  PB_Options.ActTab:= StripHotKey (s);
  Categories.CheckItem (True);
  aCatActual.Hint:= GetLangStr (ltActCategory) + ' (' + PB_Options.ActTab + ')';
  Header.Sections[cHeadText].Text:= PB_Options.ActTab + GetNumbers;
  Header.Sections[cHeadText].ImageIndex:= cIconUnsort; // Nicht sortiert!
  Categories.Updatemenu;
 end;

begin
 // TraceIn ('TPBar.TreeView1Change');
 ItemSelected (nil, False);
 if Node.Selected then begin // Only when selected
  case Node.ImageIndex of
   cRootCat:  aCompAddInfo.Enabled:= False;
   cMetaCat:  aCompAddInfo.Enabled:= False;
   cCategory: SwitchTab (Node.Text);
   else       begin
               if (Node.Parent.Text <> PB_Options.ActTab) then SwitchTab (Node.Parent.Text);
               ItemSelected (Node.Data, True);
              end;
  end;
 end;
 // TraceOut ('TPBar.TreeView1Change');
end;

//------------------------------------------------------------------------------
procedure TPBar.ItemSelected (pc: PtrComponent; sel: Boolean);
begin
 // TraceIn ('TPBar.ItemSelected');
 aCompAddInfo.Enabled:= False;
 aCompInfo.Enabled:= False;
 aCompHelp.Enabled:= False;
 aCompAddFav.Enabled:= False;
 aCompDelFav.Enabled:= False;
 if Assigned (pc) then begin
  aCompAddInfo.Enabled:= True;
  aCompInfo.Enabled:= pc.extinfo <> '';
  aCompHelp.Enabled:= True;
  aCompAddFav.Enabled:= not (pcFavorite in pc.flags);
  aCompDelFav.Enabled:= pcFavorite in pc.flags;
 end;
 // TraceOut ('TPBar.ItemSelected');
end;


//==============================================================================
//
//==============================================================================

//------------------------------------------------------------------------------
procedure TPBar.UpdateOptions;
// Sprache einstellen
begin
 // TraceIn ('TPBar.UpdateOptions');
 Caption:= cProgName;

 BtnConfigure; // Toolbar Popup

 //-- A2Z Popup --------------------------------
 CfgA2ZLeft.Caption:= GetLangStr (ltLeft);
 CfgA2ZRight.Caption:= GetLangStr (ltRight);
 CfgA2ZFilter.Caption:= GetLangStr (ltFilterMode);
 CfgA2ZShow.Caption:= GetLangStr (ltVisible);

 //-- History ----------------------------------
 CfgHistSave.Caption:= GetLangStr (ltSaveHistory);
 CfgHistClear.Caption:= GetLangStr (ltDeleteHistory);

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

 // Komponentenliste
 case PB_Options.ShowMode of
  smKachel: CfgPalViewKachel.Checked:= True;
  smSymbol: CfgPalViewSymbol.Checked:= True;
  smList:   CfgPalViewList.Checked:= True;
  smText:   CfgPalViewText.Checked:= True;
 end;
 if PB_Options.DisplayMode = dmList then SetListView else SetTreeView;

 case PB_Options.TextSize of
  tsSmall:  CfgPalTextsmall.Checked:= True;
  tsMedium: CfgPalTextMiddle.Checked:= True;
  tsBig:    CfgPalTextBig.Checked:= True;
 end;
 SetTextsize;

 CfgPalHotTrack.Checked:= PB_options.Hottrack;

 // A2Z-Bar
 A2ZBar.Align:= PB_Options.A2ZPos;
 if A2ZBar.Align = alLeft then A2ZConfigure (1);
 if A2ZBar.Align = alRight then A2ZConfigure (2);
 CfgA2ZFilter.Checked:= PB_Options.A2ZFilter;
 A2ZConfigure (3);
 CfgA2ZShow.Checked:= PB_Options.A2ZViz;
 A2ZConfigure (4);

 if PB_Options.StatusBarViz and
    (not oldopt.StatusBarViz) then begin
  SearchPanel.Visible:= False;
 end;
 StatusBar.Visible:= PB_Options.StatusBarViz;

 // Suchbalken
 SearchPanel.Align:= PB_Options.Searchbar;
 SearchPanel.Visible:= PB_Options.SearchBarViz;
 CfgSearchTop.Checked:= SearchPanel.Align = alTop;
 CfgSearchBottom.Checked:= SearchPanel.Align = alBottom;
 CfgSearchShow.Checked:= PB_Options.SearchBarViz;

 // History
 CfgHistSave.Checked:= PB_Options.SaveHistory;
 Edit.Text:= '';
 // TraceOut ('TPBar.UpdateOptions');
end;

//------------------------------------------------------------------------------
// Form-Abhängig
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
procedure TPBar.FormCreate(Sender: TObject);
var x: Integer;
    s: string;
begin
 inherited;
 // TraceIn ('TPBar.FormCreate');

 // PaletteBar einstellen
 FOpen:= False;
 //PB_Options.Fillmode:= fmNone;
 FDragging:= dmNone;
 FSortAsc:= True;
 FPalSorting:= psAZ;

 // Listen ausrichten
 ListView1.Align:= alClient;
 ListView2.Align:= alClient;
 TreeView1.Align:= alClient;
 TreeView2.Align:= alClient;

 // Gespeicherte Listen laden
 LoadList (rmSearch);

 if PB_Options.Floating then begin
  s:= gRegIni.ReadString ('', '', '-1,-1,-1,-1');
  x:= StrToInt (GetStrPart (s, ','));
  if x >= 0 then begin
   Position:= poDesigned;
   Left:= x;
   Top:= StrToInt (GetStrPart (s, ','));
   Width:= StrToInt (GetStrPart (s, ','));
   Height:= StrToInt (s);
  end;
 end;

 // Docking stuff
 DeskSection:= cWizardName;
 AutoSave:= True;

// while not Application.Active do Application.ProcessMessages;
// if PB_Options.Open and PB_Options.Floating then begin
//  Show;
//  BringToFront;
// end;

 // TraceOut ('TPBar.FormCreate');
end;

//------------------------------------------------------------------------------
procedure TPBar.FormShow(Sender: TObject);
begin
 // TraceIn ('TPBar.FormShow');
 repeat Application.ProcessMessages until PB_Options.Init;
 ImageList.Assign(Categories.MainImages);
 ListView1.LargeImages:= Categories.ImageList;
 ListView1.SmallImages:= Categories.ImageList;
 TreeView1.Images:= Categories.ImageList;
 TreeView1.StateImages:= nil;
 StatusBar.Panels[0].Text:= PB_Options.LibName;
 StatusBar.Panels[1].Text:= cStatusBarText;
 if PB_Options.Fillmode = fmNone then PB_Options.Fillmode:= fmActTab;
 UpdateOptions;
 LoadList (rmFavorites);
 PB_Options.Open:= True;  // und wir sind offen...
 FOpen:= True;
 Categories.UpdateMenu;
 GenMenu;
 // TraceOut ('TPBar.FormShow');
end;

//------------------------------------------------------------------------------
procedure TPBar.FormSavePos;
begin
 // TraceIn ('TPBar.FormSavePos');
 PB_Options.Floating:= Floating;
 if Pb_Options.Floating then begin
  gRegIni.WriteString ('', '', IntToStr (Left) + ',' +
                               IntToStr (Top) + ',' +
                               IntToStr (Width) + ',' +
                               IntToStr (Height));
 end;
 // TraceOut ('TPBar.FormSavePos');
end;

//------------------------------------------------------------------------------
procedure TPBar.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 // TraceIn ('TPBar.FormClose');
 PB_Options.Open:= False;
 FormSavePos;
 SaveToRegistry;
 Categories.UpDateMenu;
 inherited;
 // TraceOut ('TPBar.FormClose');
end;

//-----------------------------------------------------------------------------
procedure TPBar.FormDestroy(Sender: TObject);
begin
 // TraceIn ('TPBar.FormDestroy');
 if Fopen then FormSavePos;
 SaveToRegistry;
 FreeAndNil (gLangList);
 FreeAndNil (gLangText);
 FreeAndNil (gRegIni);
 inherited;
 // TraceOut ('TPBar.FormDestroy');
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
 // TraceIn ('TPBar.FormMouseMove');
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
 // TraceOut ('TPBar.FormMouseMove');
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
 // TraceIn ('TPBar.FormMouseUp');
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
        Include (pc.flags, pcHistory);
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
        if Assigned (cp) then  Include (pc.flags, pcHistory)
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
 // TraceOut ('TPBar.FormMouseUp');
end;

//------------------------------------------------------------------------------
procedure TPBar.BtnGoClick(Sender: TObject);
begin
 // TraceIn ('TPBar.BtnGoClick');
 if Edit.Text <> '' then begin
  FillCompList (fmSearch);
  if Edit.Items.IndexOf(Edit.Text) < 0 then begin
   Edit.Items.Add(Edit.Text);
   gRegIni.WriteString (cRegSearchText, Edit.Text, '1');
  end;
 end;
 // TraceOut ('TPBar.BtnGoClick');
end;

procedure TPBar.EditChange(Sender: TObject);
//var i: Integer;
begin
 // TraceIn ('TPBar.EditChange');
 if Edit.Text <> '' then FillCompList (fmSearch);
 // TraceOut ('TPBar.EditChange');
end;

//-----------------------------------------------------------------------------
// Menüs
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
procedure TPBar.CfgPalViewListClick(Sender: TObject);
var i: Integer;
begin
 GetSender (Sender, stTag, i);
 PB_Options.ShowMode:= TPalShowmode (Pred(i));
 if PB_Options.DisplayMode = dmTree then SetTreeView
                                    else SetListView;
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
 FListe.HotTrack:= CfgPalHottrack.Checked;
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgA2ZShowClick(Sender: TObject);
var i: integer;
begin
 GetSender (Sender, stTag, i);
 A2ZConfigure (i);
end;

//-----------------------------------------------------------------------------
procedure TPBar.CfgHistSaveClick(Sender: TObject);
begin
 PB_Options.SaveHistory:= CfgHistSave.Checked;
end;

//------------------------------------------------------------------------------
procedure TPBar.CfgHistClearClick(Sender: TObject);
begin
 Categories.CfgHistClear;
 if PB_Options.Fillmode = fmHistory then  FillCompList (fmHistory);
end;

//------------------------------------------------------------------------------
function TPBar.GetContextData: PtrComponent;
begin
 Result:= nil;
 if PB_Options.DisplayMode = dmTree then begin
  if Assigned (FTree.Selected) then Result:= FTree.Selected.Data;
 end else begin
  if Assigned (FListe.Selected) then Result:= FListe.Selected.Data;
 end;
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


//------------------------------------------------------------------------------
end.

