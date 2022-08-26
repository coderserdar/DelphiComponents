//============================================================================
// Unit Name: pb_Categories
// Author:    Helli
// Date:      12.07.2004
// Purpose:
// History:
//============================================================================
//       Copyright © 2004by Hellinger Software.  All Rights Reserved.
//============================================================================

unit pb_Categories;

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


type
  TIdeNotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
   protected
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
                               const FileName: string; var Cancel: Boolean);
  end;


type
  TCategories = class(TForm)
    BtnAdd: TButton;
    BtnDel: TButton;
    BtnOk: TButton;
    BtnCancel: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    CatList: TTreeView;
    BtnReset: TButton;
    MainImages: TImageList;
    StartTimer: TTimer;
    ImageList: TImageList;
    Backup: TTreeView;
    BtnUp: TButton;
    BtnDown: TButton;
    EditTimer: TTimer;
    BtnRename: TButton;
    procedure FormShow(Sender: TObject);
    procedure CatListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure BtnAddClick(Sender: TObject);
    procedure CatListEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure BtnDelClick(Sender: TObject);
    procedure CatListChange(Sender: TObject; Node: TTreeNode);
    procedure CatListEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure CatListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CatListKeyUp(Sender: TObject; var Key: Word;  Shift: TShiftState);
    procedure CatListStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure CatListEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure Label1MouseEnter(Sender: TObject);
    procedure Label1MouseLeave(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure StartTimerTimer(Sender: TObject);
    procedure CatListGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure BtnUpClick(Sender: TObject);
    procedure BtnDownClick(Sender: TObject);
    procedure EditTimerTimer(Sender: TObject);
    procedure BtnRenameClick(Sender: TObject);
  private
    FBlackList: TStringList;
    FComponentBar: TWinControl;
    FComponentMenu: TMenuItem;
    FConfigItem: TMenuItem;
    FDelphiMenu: TMainMenu;
    FDelphiTabs: TStrings;
    FDragging: Boolean;
    FEdited: Boolean;
    FPalette: TCustomControl;
    FPBMenu: TMenuItem;
    FRootNode:  TTreeNode;
    FScrolling: Boolean;
    FTabControl: TTabControl;
    function  IsDuplicate (Node: TTreeNode; const s: string): Boolean;
    procedure ShowPBar(Sender: TObject);
  public
    FComponentList: TList;
    procedure SwitchToTab (const TabName: string; tag: Integer);
    procedure MenuClick(Sender: TObject);
    procedure BuildCatMenu (menu: TMenuItem; mm: Boolean; p: Integer);
    procedure TabMenuClickHandler (Sender: TObject);
    procedure CheckItem (check: Boolean);
    function  GetDelphiTabs: TStrings;
    procedure GetCatTabs (sl: TStrings);
    procedure UpdateMenu;
    procedure UpdateLanguage;
    procedure GenComponentlist;
    function  GetComponent(const Name: string): PtrComponent;
    procedure CfgHistClear;
    function  AddImage (const comp: string): integer;
    procedure ReadCategories (read: Boolean);
    procedure WriteCategories;
    function  HasSub (Node: TTreeNode): Boolean;
    function  HasComp (Node: TTreeNode): Boolean;
    procedure GenMenu (node: TTreeNode; mnu: TMenuItem; main, add: Boolean; var p: Integer);
    function  GetEntry (const s: string): TTreeNode;
    procedure RunCategories;
    function  GetLibType: string;
  end;

var Categories: TCategories;

procedure Register;


implementation

{$R *.dfm}

uses Registry,
     pb_master, pb_config, pb_about;

var  gInList: Boolean;
     gInRead: Boolean;
     gTimerCount: Integer;
     NotifierIndex: Integer;

//==============================================================================
// Registrierung des Wizards
//==============================================================================

procedure Register;
var s: string;
    Services: IOTAServices;
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
 PB_Options.LibName:= 'VCL';

 cRegKey:=        cBaseKey + '\PaletteBarWizard\4.0';
 cRegPalette:=    cBaseKey + '\Palette';
 cRegHelp:=       cBaseKey + '\Help';

 // Optionen laden
 gRegIni:= TRegIniFile.Create (cRegKey);
 LoadFromRegistry;
 LoadLangTypes;

 Services:= BorlandIDEServices as IOTAServices;
// Assert(Assigned(Services), 'IOTAServices not available');
 NotifierIndex:= Services.AddNotifier(TIdeNotifier.Create);

 Categories:= TCategories.Create (Application);

 while not Application.Active do Application.ProcessMessages;

 if Assigned (RegisterFieldAddress) then begin
  RegisterFieldAddress (cWizardName, Addr(PBar));
  RegisterDesktopFormClass (TPBar, cWizardName, cWizardName);
 end;

end;


//==============================================================================
// Functions for the IdeNotifier
//==============================================================================

procedure RemoveNotifier;
var Services: IOTAServices;
begin
 if NotifierIndex <> -1 then begin
  Services:= BorlandIDEServices as IOTAServices;
  Assert(Assigned(Services), 'IOTAServices not available');
  Services.RemoveNotifier(NotifierIndex);
 end;
end;

function MsgServices: IOTAMessageServices;
begin
 Result:= (BorlandIDEServices as IOTAMessageServices);
// Assert(Result <> nil, 'IOTAMessageServices not available');
end;

procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
 // MsgServices.AddTitleMessage('After Compile');
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
 // MsgServices.AddTitleMessage('Before Compile');
end;

procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
                                        const FileName: string; var Cancel: Boolean);
begin
// MsgServices.AddTitleMessage(Format('%s: %s',
//    [GetEnumName(TypeInfo(TOTAFIleNotification), Ord(NotifyCode)), FileName]));
 case Notifycode of
//  ofnFileOpening:          ; // No action
//  ofnFileOpened:           ; // No action
//  ofnFileClosing:          ; // No action
//  ofnDefaultDesktopLoad:   ; // No action
//  ofnDefaultDesktopSave:   ; // No action
//  ofnProjectDesktopLoad:   ; // No action
//  ofnProjectDesktopSave:   ; // No action
  ofnPackageInstalled,
  ofnPackageUninstalled,
  ofnActiveProjectChanged: if PB_OPtions.Init then begin
                            PB_Options.GenCompList:= True;
                            PB_Options.BuildMainMnu:= True;
                            PB_Options.BuildTabMnu:= True;
                            PB_Options.FreshUp:= True;
                           end;
 end;
end;


//==============================================================================
// Functions for the Dialog
//==============================================================================

//------------------------------------------------------------------------------
procedure TCategories.FormCreate(Sender: TObject);
begin
 // Main Form von Delphi auslesen
 FComponentBar:= TWinControl (Application.MainForm.FindComponent (cPaletteBar));
 FTabControl:= TTabControl (FComponentBar.Controls [0]);
 FPalette:= TCustomControl (Application.MainForm.FindComponent(cPalette));

 // Das ist das Hauptmenü von Delphi
 FDelphiMenu:= (BorlandIDEServices as INTAServices).MainMenu;

 // Palettenkonfiguration suchen
 FComponentMenu:= FindMenu (FDelphiMenu, cConfigMenu);
 FConfigItem:= FindMenu (FComponentMenu, cConfigItem);

 // Komponentenliste erzeugen
 FComponentList:= TList.Create;
 FBlacklist:= TStringList.Create;
 FBlacklist.CaseSensitive:= False;

 StartTimer.Enabled:= True;
end;

//------------------------------------------------------------------------------
procedure TCategories.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 SaveFormPos (categories, cRegCategory);
end;

//------------------------------------------------------------------------------
procedure TCategories.FormShow(Sender: TObject);
begin
 // Sprache anpassen
 Caption:= GetLangStr (ltCfgCategory);
 BtnOk.Caption:= GetLangStr (ltOKCaption);
 BtnCancel.Caption:= GetLangStr (ltCancelCaption);
 BtnAdd.Caption:= GetLangStr (ltAddCaption);
 BtnDel.Caption:= GetLangStr (ltDelCaption);
 BtnReset.Caption:= GetLangStr (ltResetCaption);
 BtnUp.Caption:= GetLangStr (ltUpCaption);
 BtnDown.Caption:= GetLangStr (ltDownCaption);
 BtnRename.Caption:= GetLangStr (ltRenameCaption);
 FScrolling:= False;
 FDragging:= False;
 // Kategorien merken
 Backup.Items.Assign (CatList.Items);
 FRootNode.Expand (True);
 BringToFront;
 BtnCancel.SetFocus;
end;


//------------------------------------------------------------------------------
function TCategories.IsDuplicate (Node: TTreeNode; const s: string): Boolean;
var tn, sn: TTreeNode;
    i: Integer;
begin
 // TraceIn ('TCategories.IsDuplicate');
 Result:= False;
 if Assigned (Node) then begin
  tn:= Node;
  if (tn.ImageIndex >= cUnknown) then tn:= Node.Parent;
  if tn.HasChildren then begin
   sn:= tn.getFirstChild;
   repeat
    if Assigned (sn) and AnsiSameText (sn.Text, s) then begin
     Result:= True;
     // TraceOut ('TCategories.IsDuplicate-Exit1');
     Exit;
    end;
    sn:= tn.GetNextChild (sn);
   until (sn = nil);
  end;
 end else begin
  for i:= 0 to Pred (CatList.Items.Count) do begin
   if AnsiSameText (CatList.Items[i].Text, s) then begin
    Result:= True;
    // TraceOut ('TCategories.IsDuplicate-Exit2');
    Exit;
   end;
  end;
 end;
 // TraceOut ('TCategories.IsDuplicate');
end;

//------------------------------------------------------------------------------
procedure TCategories.BtnResetClick(Sender: TObject);
begin
 FBlacklist.Clear; // Blacklist löschen
 ReadCategories (False);
end;

//------------------------------------------------------------------------------
procedure TCategories.BtnAddClick(Sender: TObject);
var tn, rt: TTreeNode;
begin
 // TraceIn ('TCategories.BtnAddClick');
 CatList.Items.BeginUpdate;
 rt:= CatList.Selected;
 if not Assigned (rt) then rt:= FRootNode;
 if rt.ImageIndex <> cRootCat then rt:= rt.Parent;
 rt.Expand (false);
 tn:= CatList.Items.AddChildFirst (rt, GetLangStr (ltNewEntry));
 SetNodeImage (tn, cMetaCat);
 CatList.Items.EndUpdate;
 CatList.SetFocus;
 CatList.Selected:= tn;
 tn.MakeVisible;
 FEdited:= False;
 tn.EditText;
 // TraceOut ('TCategories.BtnAddClick');
end;

//------------------------------------------------------------------------------
procedure TCategories.BtnDelClick(Sender: TObject);
var tn: TTreeNode;
begin
 // TraceIn ('TCategories.BtnDelClick');
 CatList.Items.BeginUpdate;
 tn:= CatList.Selected;
 if Assigned (tn) then begin
  if not tn.HasChildren then begin
   if tn.StateIndex >= 0 then FBlacklist.Add (FDelphiTabs[tn.StateIndex]);
   CatList.Items.Delete (tn);
  end;
 end;
 CatList.Items.EndUpdate;
 // TraceOut ('TCategories.BtnDelClick');
end;

//------------------------------------------------------------------------------
procedure TCategories.BtnUpClick(Sender: TObject);
var tn, sn: TTreeNode;
begin
 // TraceIn ('TCategories.BtnUpClick');
 sn:= CatList.Selected;
 if Assigned (sn) then begin
  tn:= sn.getPrevSibling;
  if Assigned (tn) then sn.MoveTo(tn, naInsert);
 end;
 CatList.Selected:= sn;
 // TraceOut ('TCategories.BtnUpClick');
end;

//------------------------------------------------------------------------------
procedure TCategories.BtnDownClick(Sender: TObject);
var tn, sn: TTreeNode;
begin
 // TraceIn ('TCategories.BtnDownClick');
 sn:= CatList.Selected;
 if Assigned (sn) then begin
  tn:= sn.getNextSibling;
  if Assigned (tn) then tn.MoveTo(sn, naInsert);
 end;
 CatList.Selected:= sn;
 // TraceOut ('TCategories.BtnDownClick');
end;

//------------------------------------------------------------------------------
procedure TCategories.BtnRenameClick(Sender: TObject);
begin
 // TraceIn ('TCategories.BtnRenameClick');
 if Assigned (CatList.Selected) then begin
  if (CatList.Selected.ImageIndex = cCategory) then  CatList.Selected.EditText
 end;
 // TraceOut ('TCategories.BtnRenameClick');
end;

//------------------------------------------------------------------------------
procedure TCategories.CatListGetImageIndex(Sender: TObject; Node: TTreeNode);
var pc: PtrComponent;
begin
 // TraceIn ('TCategories.CatListGetImageIndex');
 if Node.ImageIndex = cUnknown then begin
  pc:= Node.Data;
  if Assigned (pc) then begin
   if pc.imgindex = cUnknown then  pc.imgindex:= AddImage (pc.typename);
   SetNodeImage (Node, pc.imgindex);
  end;
 end;
 // TraceOut ('TCategories.CatListGetImageIndex');
end;

//------------------------------------------------------------------------------
procedure TCategories.CatListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var tn: TTreeNode;
begin
 // TraceIn ('TCategories.CatListDragOver');
 tn:= CatList.GetNodeAt(x, y);
 if Assigned (tn) then begin
  Accept:= ((tn = FRootNode) and (CatList.Selected.ImageIndex < cUnKnown)) or
           ((tn <> FRootNode) and (tn <> CatList.Selected));
 end else begin
  Accept:= False;
 end;
 FScrolling:= False;
 // TraceOut ('TCategories.CatListDragOver');
end;

//------------------------------------------------------------------------------
procedure TCategories.CatListEdited(Sender: TObject; Node: TTreeNode; var S: String);
var i: Integer;
begin
 // TraceIn ('TCategories.CatListEdited');
 if IsDuplicate (nil, s) then begin
  MessageDlg(GetLangStr (ltErrorCat), mtWarning, [mbOK], 0);
  s:= GetLangStr (ltNewEntry);
  EditTimer.Enabled:= True;
 end;
 i:= FBlackList.IndexOf(s);
 if i >= 0 then FBlackList.Delete(i);
 FEdited:= True;
 // TraceOut ('TCategories.CatListEdited');
end;

//------------------------------------------------------------------------------
procedure TCategories.EditTimerTimer(Sender: TObject);
begin
 // TraceIn ('TCategories.EditTimerTimer');
 EditTimer.Enabled:= False;
 CatList.Selected.EditText;
 // TraceOut ('TCategories.EditTimerTimer');
end;

//------------------------------------------------------------------------------
procedure TCategories.CatListChange(Sender: TObject; Node: TTreeNode);
begin
 // TraceIn ('TCategories.CatListChange');
 BtnDel.Enabled:= ((Node.ImageIndex > cRootCat) and (Node.ImageIndex < cUnknown))
                  and not Node.HasChildren;
 BtnUp.Enabled:= (Node.ImageIndex > cRootCat);
 BtnDown.Enabled:= (Node.ImageIndex > cRootCat);
 BtnRename.Enabled:= (Node.ImageIndex > cRootCat) and (Node.ImageIndex < cUnknown);
 // TraceOut ('TCategories.CatListChange');
end;

//------------------------------------------------------------------------------
procedure TCategories.CatListEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
begin
 // TraceIn ('TCategories.CatListEditing');
 AllowEdit:= (Node.ImageIndex > cRootCat) and (Node.ImageIndex < cUnknown);
 // TraceOut ('TCategories.CatListEditing');
end;

//------------------------------------------------------------------------------
procedure TCategories.CatListDragDrop(Sender, Source: TObject; X, Y: Integer);
type TKeyMode = (kmCopy, kmMove, kmNone);
var tn, sn, nn: TTreeNode;
    keymode: TKeyMode;
begin
 // TraceIn ('TCategories.CatListDragDrop');
 FDragging:= False;
 FScrolling:= False;
 tn:= CatList.GetNodeAt (X, Y);
 sn:= CatList.Selected;
 if (not Assigned (tn)) or
    (tn = sn) or
    (tn = sn.Parent) then begin
  CatList.EndDrag (False);
  // TraceOut ('TCategories.CatListDragDrop-Exit');
  Exit;
 end;

 CatList.Items.BeginUpdate;

 if not IsDuplicate (tn, sn.Text) then begin

  keymode:= kmNone;
  if (Windows.GetKeyState (VK_LSHIFT) >= 128) or
     (Windows.GetKeyState (VK_RSHIFT) >= 128) then keymode:= kmCopy;
  if (Windows.GetKeyState (VK_LCONTROL) >= 128) or
     (Windows.GetKeyState (VK_RCONTROL) >= 128) then keymode:= kmMove;

  case keymode of
   kmMove: begin // Position des Knotens verschieben
            if tn.Parent = sn.Parent then sn.MoveTo(tn, naInsert);
           end;
   kmCopy: begin // Knoten kopieren
            if (tn.ImageIndex < cUnknown) then nn:= CatList.Items.AddChild (tn, sn.Text)
                                          else nn:= CatList.Items.Add (tn, sn.Text);
            SetNodeImage (nn, sn.ImageIndex);
            nn.Data:= sn.Data;
            if sn.HasChildren then CopyNodes (CatList, sn, nn);
            nn.Expand (False);
           end;
   else    begin // Knoten verschieben
            if (tn.ImageIndex < cUnknown) then  sn.MoveTo(tn, naAddChild)
                                          else  sn.MoveTo(tn, naAdd);
           end;
  end;

 end else begin // doppelt git nicht!
  MessageBeep (MB_ICONHAND);
 end;

 CatList.Items.EndUpdate;
 // TraceOut ('TCategories.CatListDragDrop');
end;

//------------------------------------------------------------------------------
procedure TCategories.CatListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 // TraceIn ('TCategories.CatListKeyUp');
 case key of
  vk_F2:     if BtnRename.Enabled then BtnRenameClick (Sender);
  vk_Return: if FEdited then FEdited:= False;
  vk_Insert: if BtnAdd.Enabled then BtnAddClick(Sender);
  vk_Delete: if BtnDel.Enabled then BtnDelClick (Sender);
 end;
 // TraceOut ('TCategories.CatListKeyUp');
end;

//------------------------------------------------------------------------------
procedure TCategories.CatListStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
 // TraceIn ('TCategories.CatListStartDrag');
 FDragging:= True;
 FScrolling:= False;
 // TraceOut ('TCategories.CatListStartDrag');
end;

//------------------------------------------------------------------------------
procedure TCategories.CatListEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
 // TraceIn ('TCategories.CatListEndDrag');
 FDragging:= False;
 FScrolling:= False;
 // TraceOut ('TCategories.CatListEndDrag');
end;

//------------------------------------------------------------------------------
procedure TCategories.Label1MouseEnter(Sender: TObject);
var Msg: Integer;
begin
 // TraceIn ('TCategories.Label1MouseEnter');
 if FDragging then begin
  FScrolling:= True;
  Msg:= -1;
  Repeat
   if ((Sender as TLabel).Name = 'Label1') then  Msg:= SB_LINEUP;
   if ((Sender as TLabel).Name = 'Label2') then  Msg:= SB_LINEDOWN;
   CatList.Perform (WM_VSCROLL, Msg, 0);
   Delay (25);
  until not FScrolling;
 end;
 // TraceOut ('TCategories.Label1MouseEnter');
end;

//------------------------------------------------------------------------------
procedure TCategories.Label1MouseLeave(Sender: TObject);
begin
 // TraceIn ('TCategories.Label1MouseLeave');
 FScrolling:= False;
 // TraceOut ('TCategories.Label1MouseLeave');
end;

//==============================================================================
// Globale Funktionen für den Wizard selbst
//==============================================================================

//------------------------------------------------------------------------------
// Actions
//------------------------------------------------------------------------------
procedure TCategories.StartTimerTimer(Sender: TObject);
var m: TMenuItem;

 function AddIcon (icon: integer): Integer;
 // Symbol für Menü erzeugen
 var bitmap: TBitmap;
 begin
  Result:= -1;
  if icon >= 0 then begin
   bitmap:= TBitmap.Create;
   try
    MainImages.GetBitmap(icon, bitmap);
    Result:= (BorlandIDEServices as INTAServices).ImageList.AddMasked (bitmap, bitmap.Canvas.Pixels[0, 0]);
   finally
    bitmap.Free;
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
 // TraceIn ('TCategories.StartTimer');
 Inc (gTimerCount);
 if Assigned (PBar) or (gTimerCount > PB_Options.TimerVal) then begin
  StartTimer.Enabled:= False;
  LoadLanguage (Pb_Options.Language);

  // Kategorien erzeugen
  PB_Options.LibName:= '---';
  GenComponentList; // Kompnentenliste erzeugen
//  ReadCategories (True);

  PB_Options.Init:= True;
  if not Assigned (PBar) then  Pbar:= TPbar.Create(Application);

  // Menü erzeugen
  if PB_Options.Menu then begin
   FPBMenu:= TMenuItem.Create (Application);
   FPBMenu.Caption:= cWizardMenu;
   FPBMenu.OnClick:= MenuClick;
   FPBMenu.NewTopLine;

   m:= TMenuItem.Create (Application);
   m.Caption:= cPalettebar + c3Dots;
   m.OnClick:= ShowPBar;
   m.ImageIndex:= AddIcon (15);
   m.Enabled:= True;
   m.Visible:= True;
   FPBMenu.Add (m);

   FPBMenu.NewBottomLine;
   AddMenu (PBar.aCatActual);
   AddMenu (PBar.aCatAll);
   AddMenu (PBar.aCatTree);
   AddMenu (PBar.aCatForm);
   AddMenu (PBar.aCatHistory);
   AddMenu (PBar.aCatFavorites);
   FPBMenu.NewBottomLine;
   AddMenu (PBar.aCompInfo);
   AddMenu (PBar.aCompHelp);
   AddMenu (PBar.aCompAddFav);
   AddMenu (PBar.aCompDelFav);
   FPBMenu.NewBottomLine;
   AddMenu (PBar.aConfCat);
   AddMenu (PBar.aConfPB);
   AddMenu (PBar.aAbout);
   FDelphiMenu.Items.Insert(FindMenuInt(FDelphiMenu, cToolsMenu)+1, FPBMenu);
   MenuClick (nil);
  end else begin
   FPBMenu:= TMenuItem.Create (Application);
   FPBMenu.Caption:= cPalettebar + c3Dots;
   FPBMenu.ImageIndex:= AddIcon (14);
   FPBMenu.OnClick:= ShowPBar;
   FPBMenu.Name:= cPBMenu;
   FPBMenu.Visible:= True;
   FComponentMenu.Add (FPBMenu);
  end;
  UpdateMenu;

  PBar.aConfCat.Enabled:= True;
  PBar.aConfPB.Enabled:= True;
  if PB_Options.Floating and PB_Options.Open then PBar.Show;
 end;
 // TraceOut ('TCategories.StartTimer');
end;

//------------------------------------------------------------------------------
procedure TCategories.ShowPBar(Sender: TObject);
begin
 if Assigned (PBar) then begin
  Pbar.Show;
 end else begin
  PBar:= TPBar.Create (Application);
  Pbar.Show;
 end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
function TCategories.AddImage (const comp: string): integer;
var bitmap: TBitmap;
    old: TPersistentClass;
    item: IPaletteItem;
    itemx: IPalettePaint;
begin
 // TraceIn ('TCategories.AddImage');
 Result:= -1;
 {$ifdef ClxVersion}
 if (BorlandIDEServices as IOTAServices).GetActiveDesignerType = dVCL then begin
  old:= ActivateClassGroup (TControl)
 end else begin
  old:= ActivateClassGroup (QControls.TControl);
 end;
 {$endif}
 try
  item:= ComponentDesigner.ActiveDesigner.Environment.GetPaletteItem (TComponentClass (FindClass (comp))) as IPaletteItem;
 except
  Exit;
 end;
 if Assigned (item) then begin
  if item.QueryInterface (IPalettePaint, itemx) = 0 then begin
   bitmap:= TBitmap.Create;
   try
    bitmap.Height:= 28;
    bitmap.Width:= 28;
    itemx.Paint (bitmap.Canvas, 0, 0);
    Result:= ImageList.AddMasked (bitmap, bitmap.Canvas.Pixels[0, 0]);
   finally
    bitmap.Free;
   end;
  end;
 end;
 {$ifdef ClxVersion} ActivateClassGroup (old); {$endif}
 // TraceOut ('TCategories.AddImage');
end;

//------------------------------------------------------------------------------
function TCategories.HasSub (Node: TTreeNode): Boolean;
begin
 Result:= False;
 Node:= Node.getFirstChild;
 while Assigned (Node) do begin
  if Node.ImageIndex < cUnknown then begin
   Result:= True;
   Exit;
  end;
  Node:= Node.getNextSibling;
 end;
end;

function TCategories.HasComp (Node: TTreeNode): Boolean;
begin
 Result:= False;
 Node:= Node.getFirstChild;
 while Assigned (Node) do begin
  if Node.ImageIndex >= cUnknown then begin
   Result:= True;
   Exit;
  end;
  Node:= Node.getNextSibling;
 end;
end;

//------------------------------------------------------------------------------
// Menu-Funktionen für die Kategorien
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
procedure TCategories.GenMenu (node: TTreeNode; mnu: TMenuItem; main, add: Boolean; var p: Integer);
var n: TTreeNode;
    tc, tc2: TMenuItem;
begin
 // TraceIn ('TCategories.GenMenu');
 if Assigned (node) then begin
  tc:= nil;
  if Node.ImageIndex < cUnknown then begin
   // Menüpunkt eintragen
   tc:= TMenuItem.Create (Application);
   tc.Caption:= Node.Text;
   tc.OnClick:= TabMenuClickhandler;
   tc.Checked:= Node.Text = PB_Options.ActTab;
   tc.Tag:= Node.AbsoluteIndex;
   if Add then begin
    mnu.Add(tc)
   end else begin
    mnu.Insert (p, tc);
    Inc(p); // Hochzählen, sonst fügen wir verkehrt herum ein!
   end;
  end;
  if HasSub (Node) and (Assigned (tc)) then begin
   tc.OnClick:= nil;
   tc.Checked:= False;
   tc.Tag:= 0;
   if HasComp (Node) then begin
    // Menüpunkt nochmals eintragen
    tc2:= TMenuItem.Create (Application);
    tc2.Caption:= Node.Text;
    tc2.OnClick:= TabMenuClickhandler;
    tc2.Checked:= Node.Text = PB_Options.ActTab;
    tc2.Tag:= Node.AbsoluteIndex;
    tc.Add(tc2)
   end else begin
    SetNodeImage (Node, cMetaCat);
   end;
   n:= node.GetFirstChild; // Kinder untersuchen...
   if Assigned (n) then GenMenu (n, tc, main, True, p);
  end;
  n:= Node.getNextSibling; // auf gleicher Ebene weitermachen
  if Assigned (n) then GenMenu (n, mnu, main, add, p);
 end;
 // TraceOut ('TCategories.GenMenu');
end;

//------------------------------------------------------------------------------
procedure TCategories.BuildCatMenu(menu: TMenuItem; mm: Boolean; p: Integer);
begin
 GenMenu (CatList.Items.GetFirstNode.getFirstChild, menu, mm, False, p);
end;

//------------------------------------------------------------------------------
procedure TCategories.MenuClick(Sender: TObject);
var i: Integer;
begin
 if PB_Options.BuildMainMnu then begin
  // Kategorien löschen
  for i:= Pred (FPBMenu.IndexOf(FPBMenu.Find('-'))) downto 0 do  FPBMenu.Delete (i);
  BuildCatMenu (FPBMenu, True, 0);
  PB_Options.BuildMainMnu:= False;
 end;
end;

//------------------------------------------------------------------------------
procedure TCategories.CheckItem (check: Boolean);
var m: TMenuItem;
begin
 // TraceIn ('TCategories.CheckItem');
 if Assigned (FPBMenu) then begin
  m:= FindMenu (FPBMenu, PB_Options.ActTab);
  if Assigned (m) then m.Checked:= check;
 end;
 if Assigned (PBar) and Assigned (PBar.TabMenu.Items) then begin
  m:= FindMenu (PBar.TabMenu.Items, PB_Options.ActTab);
  if Assigned (m) then m.Checked:= check;
 end;
 // TraceOut ('TCategories.CheckItem');
end;

//------------------------------------------------------------------------------
procedure TCategories.SwitchToTab (const TabName: string; tag: Integer);
begin
 // TraceIn ('TCategories.SwitchToTab');
 if Tag > 0 then begin
  if HasComp (CatList.Items[Tag]) then begin
   CheckItem (false);
   PB_Options.ActTab:= StripHotKey (TabName);
   CheckItem (True);
   PBar.FillCompList (fmActTab);
   Updatemenu;
  end;
 end;
 // TraceOut ('TCategories.SwitchToTab');
end;

//------------------------------------------------------------------------------
procedure TCategories.TabMenuClickHandler(Sender: TObject);
var s: string;
    i: Integer;
begin
 // TraceIn ('TCategories.TabMenuClickHandler');
 GetSender (Sender, stCaption, s);
 GetSender (Sender, stTag, i);
// (Sender as TMenuItem).Checked:= True;
 SwitchToTab (s, i);
 // TraceOut ('TCategories.TabMenuClickHandler');
end;

//------------------------------------------------------------------------------
procedure TCategories.CfgHistClear;
var p: PtrComponent;
    i: Integer;
begin
 // TraceIn ('TCategories.CfgHistClear');
 gRegIni.EraseSection (cRegHistory);
 for i:= 0 to Pred (FComponentList.Count) do begin
  p:= FComponentList[i];
  if Assigned (p) then Exclude (p.flags, pcHistory);
 end;
 // TraceOut ('TCategories.CfgHistClear');
end;

//------------------------------------------------------------------------------
function TCategories.GetDelphiTabs: TStrings;
begin
 // TraceIn ('TCategories.GetDelphiTabs');
 Result:= FTabControl.Tabs;
 // TraceOut ('TCategories.GetDelphiTabs');
end;

//------------------------------------------------------------------------------
procedure TCategories.GetCatTabs (sl: TStrings);
var i: Integer;
begin
 // TraceIn ('TCategories.GetCatTabs');
 sl.Add ('##ph##');
 for i:= 0 to Pred (CatList.Items.Count) do begin
  if CatList.Items[i].ImageIndex = cCategory then begin
   sl.Add (CatList.Items[i].Text);
  end;
 end;
 // TraceOut ('TCategories.GetCatTabs');
end;

//------------------------------------------------------------------------------
procedure TCategories.UpdateLanguage;
begin
 // TraceIn ('TCategories.UpdateLanguage');
 LoadLanguage (Pb_Options.Language);
 UpdateMenu;
 // TraceOut ('TCategories.UpdateLanguage');
end;

//------------------------------------------------------------------------------
procedure TCategories.UpdateMenu;

 procedure SetItems (Action: TAction; Button: TToolButton; Menu: TMenuItem;
                     txt: TLanguageText; const x, y: string);
 var s: string;
 begin
  s:= GetLangStr (txt);
  if Assigned (Action) then begin
   Action.Caption:= s + x;
   Action.Hint:= s + x;
  end;
  if Assigned (Button) then Button.Hint:= s + x;
  if Assigned (Menu)   then Menu.Caption:= s + y;
 end;

begin
 // TraceIn ('TCategories.UpdateMenu');
 if Assigned (PBar) then begin
  with PBar do begin
   SetItems (aCatActual,    nil, nil,  ltActCategory,   ' (' + PB_Options.ActTab + ')', ' (' + PB_Options.ActTab + ')');
   SetItems (aCatAll,       nil, nil,  ltAllComps,      '', '');
   SetItems (aCatTree,      nil, nil,  ltTreeView,      '', '');
   SetItems (aCatForm,      nil, nil,  ltActForm,       '', '');
   SetItems (aCatHistory,   nil, nil,  ltHistory,       '', '');
   SetItems (aCatFavorites, nil, nil,  ltFavorites,     '', '');
   SetItems (aCompAddInfo,  nil, nil,  ltEnterExtInfo,  '', '');
   SetItems (aCompInfo,     nil, nil,  ltShowExtInfo,   '', '');
   SetItems (aCompHelp,     nil, nil,  ltShowHelp,      '', '');
   SetItems (aCompAddFav,   nil, nil,  ltFavAdd,        '', '');
   SetItems (aCompDelFav,   nil, nil,  ltFavRemove,     '', '');
   SetItems (aConfCat,      nil, nil,  ltCfgCategory,   '', c3Dots);
   SetItems (aConfPb,       nil, nil,  ltCfgPaletteBar, '', c3Dots);
   SetItems (aAbout,        nil, nil,  ltAbout,         '', c3Dots);

   // aktivieren, wenn offen
   aCatActual.Enabled:= PB_Options.Open;
   aCatAll.Enabled:= PB_Options.Open;
   aCatTree.Enabled:= PB_Options.Open;
   aCatForm.Enabled:= PB_Options.Open;
   aCatHistory.Enabled:= PB_Options.Open;
   aCatFavorites.Enabled:= PB_Options.Open;
   aCompAddInfo.Enabled:= False;
   aCompInfo.Enabled:= False;
   aCompHelp.Enabled:= False;
   aCompAddFav.Enabled:= False;
   aCompDelFav.Enabled:= False;
  end;
 end;

 // TraceOut ('TCategories.UpdateMenu');
end;

//------------------------------------------------------------------------------
function TCategories.GetComponent(const Name: string): PtrComponent;
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
function TCategories.GetLibType: string;
begin
 Result:= 'VCL';
 {$ifdef ClxVersion}
 if (BorlandIDEServices as IOTAServices).GetActiveDesignerType = dCLX then Result:= 'CLX';
 {$endif}
end;

//------------------------------------------------------------------------------
procedure TCategories.GenComponentlist;
var   PropInfo: PPropInfo;
      i, j: Integer;
      PalToolCount: Integer;
      SelectedToolName: string;
      OldTabIndex, OldToolIndex: integer;
      pc: PtrComponent;
      old: string;
      readcat: Boolean;
begin
 // TraceIn ('TCategories.GenComponentlist');
 // Komponentenliste erstellen
 if gInList then  Exit;
 gInList:= True;

 if PB_Options.GenCompList then begin

  PB_Options.BuildMainMnu:= True;
  PB_Options.BuildTabMnu:= True;

  if Assigned (FPalette) then begin

   old:= PB_Options.LibName;
   PB_Options.LibName:= GetLibType;

   if PB_Options.LibName <> old then begin
    // clear Componentlist and Images!
    for i:= Pred(FcomponentList.Count) downto 0 do begin
     Dispose (FComponentList[i]);
    end;
    FComponentList.Clear;
    for i:= Pred(ImageList.Count) downto 5 do ImageList.Delete(i);
    readcat:= True;
   end else begin
    // Checked-Flag auf false, um zu checken wer da ist
    for i:= 0 to Pred (FComponentList.Count) do begin
     pc:= FComponentList[i];
     if Assigned (pc) then Exclude (pc.flags, pcChecked);
    end;
    readcat:= False;
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
       pc.extinfo:= '';
       pc.flags:= [pcChecked];
       if pc.typename = 'TFrames' then pc.imgindex:= cFrame
                                  else pc.imgindex:= cUnknown;
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
  Pb_Options.GenCompList:= False;
 end;

 if readcat then ReadCategories (True);

 gInList:= False;
 // TraceOut ('TCategories.GenComponentlist');
end;

//------------------------------------------------------------------------------
procedure TCategories.ReadCategories (read: Boolean);
var s, s2, rk: string;
    t1, t2: TTreeNode;
    i: Integer;
    pc: PtrComponent;
begin
 // TraceIn ('TCategories.ReadCategories');
 if gInRead then begin
  // TraceOut ('TCategories.ReadCategories-Exit');
  Exit;
 end;
 ginRead:= True;

 Screen.Cursor:= crHourGlass;
 CatList.Items.BeginUpdate;
 CatList.Items.Clear;

 PB_Options.BuildMainMnu:= True;
 PB_Options.BuildTabMnu:= True;

 rk:= cRegVCLCat;
 {$ifdef ClxVersion}
 if (BorlandIDEServices as IOTAServices).GetActiveDesignerType = dCLX then begin
  rk:= cRegCLXCat;
 end;
 {$endif}

 FRootNode:= CatList.Items.Add (nil, GetLangStr (ltTabCaption) + GetLibType);
 SetNodeImage (FRootNode, cRootCat);

 // Flags zurücksetzen
 for i:= 0 to Pred (FComponentList.Count) do begin
  pc:= FComponentList[i];
  if Assigned (pc) then Exclude (pc.flags, pcInList);
 end; // for

 // Vorhandene Liste einlesen
 if read then begin
  ReadNodes (CatList, FRootNode, rk);
  gRegini.ReadSection(rk, FBlacklist);
 end;

 // Abgleichen mit den tatsächlichen Kategorien
 FDelphiTabs:= GetDelphiTabs;
 for i:= 0 to Pred (FDelphiTabs.Count) do begin
  if FBlacklist.IndexOf (FDelphiTabs[i]) < 0 then begin // nicht auf der Blacklist
   t1:= GetTreeNode (CatList, True, FDelphiTabs[i]);
   if not Assigned (t1) then begin
    t1:= CatList.Items.AddChild (FRootNode, FDelphiTabs[i]);
   end else begin
    s:= t1.Text;
    t1.Text:= GetStrPart (s, #254); // führende Zahl abscheinden '0001¦Standard'
   end;
   SetNodeImage (t1, cCategory);
   t1.StateIndex:= i; // Index des Delphi-Tabs
  end; // if sl.IndexOf
 end; // for i

 // vorhandene Komponenten eintragen
 for i:= 0 to Pred (CatList.Items.Count) do begin
  if CatList.Items[i].ImageIndex = cUnknown then begin
   s:= CatList.Items[i].Text;
   s2:= GetStrPart (s, #254);
   pc:= GetComponent(s2);
   if Assigned (pc) then begin
    pc.extinfo:= s;
    CatList.Items[i].Text:= s2;
    SetNodeImage (CatList.Items[i], pc.imgindex);
    pc.tabname:= CatList.Items[i].Parent.Text;
    CatList.Items[i].Data:= pc;
    Include (pc.flags, pcInList);
   end;
  end;
 end;

 // Neue Komponenten eintragen
 for i:= 0 to Pred (FComponentList.Count) do begin
  pc:= FComponentList[i];
  if Assigned (pc) then begin
   if not (pcInList in pc.flags) then begin
    if FBlacklist.IndexOf (pc.tabname) < 0 then begin // auf der Blacklist?
     t1:= GetTreeNode (CatList, False, pc.tabname);
     SetNodeImage (t1, cCategory);
     t2:= CatList.Items.AddChild (t1, pc.typename);
     pc.tabname:= t1.Text;
     SetNodeImage (t2, pc.imgindex);
     t2.Data:= pc;
     Include (pc.flags, pcInList);
    end; // if sl.IndexOf
   end;
  end;
 end; // for

 // Nicht mehr vorhandene Komponenten entfernen
 for i:= Pred (CatList.Items.Count) downto 0 do begin
  if (CatList.Items[i].ImageIndex = cUnKnown) and
     (not Assigned (CatList.Items[i].Data)) then  CatList.Items[i].Delete;
 end;

 if read then WriteCategories;

 CatList.Items.EndUpdate;
 Screen.Cursor:= crDefault;
 ginRead:= False;
 // TraceOut ('TCategories.ReadCategories');
end;

//------------------------------------------------------------------------------
procedure TCategories.WriteCategories;
var rk: string;
    i: Integer;
begin
 // TraceIn ('TCategories.WriteCategories');
 Screen.Cursor:= crHourGlass;
 CatList.Items.BeginUpdate;
 // Detect if Lib is VCL or CLX
 rk:= cRegVCLCat;
 {$ifdef ClxVersion}
  if (BorlandIDEServices as IOTAServices).GetActiveDesignerType = dCLX then rk:= cRegCLXCat;
 {$endif}
 // Write Blacklist to registry
 gRegIni.EraseSection (rk);
 for i:= 0 to Pred (FBlacklist.Count) do begin
  gRegIni.WriteString(rk, FBlacklist[i], '');
 end;
 // Write the whole tree to registry
 WriteNodes (CatList.Items.GetFirstNode.getFirstChild, rk, FDelphiTabs);
 CatList.Items.EndUpdate;
 Screen.Cursor:= crDefault;
 // TraceOut ('TCategories.WriteCategories');
end;

//------------------------------------------------------------------------------
function TCategories.GetEntry (const s: string): TTreeNode;
var i: Integer;
begin
 // TraceIn ('TCategories.GetEntry');
 for i:= 0 to Pred (Catlist.Items.Count) do begin
  if s = CatList.Items[i].Text then begin
   Result:= CatList.Items[i];
   // TraceOut ('TCategories.GetEntry-Exit');
   Exit;
  end;
 end;
 Result:= nil;
 // TraceOut ('TCategories.GetEntry');
end;

//------------------------------------------------------------------------------
procedure TCategories.RunCategories;
begin
 SetFormPos (Categories, cRegCategory);
 if Categories.ShowModal = mrOk then begin
  WriteCategories;
  PB_Options.BuildMainMnu:= True;
  PB_Options.BuildTabMnu:= True;
 end;
end;

//------------------------------------------------------------------------------
initialization
 // TraceIn ('TCategories.initialization');
 Pbar:= nil;
 gInList:= False;
 gInRead:= False;
 gTimerCount:= 0;
 NotifierIndex:= -1;
 // TraceOut ('TCategories.initialization');
finalization
 // TraceIn ('TCategories.finalization');
 RemoveNotifier;
 if Assigned (UnregisterFieldAddress) then UnregisterFieldAddress (Addr(PBar));
 Pbar:= nil;
 // TraceOut ('TCategories.finalization');
end.
