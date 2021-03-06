unit SpTBXCustomizer;

{==============================================================================
Version 2.4

The contents of this file are subject to the SpTBXLib License; you may
not use or distribute this file except in compliance with the
SpTBXLib License.
A copy of the SpTBXLib License may be found in SpTBXLib-LICENSE.txt or at:
  http://www.silverpointdevelopment.com/sptbxlib/SpTBXLib-LICENSE.htm

Alternatively, the contents of this file may be used under the terms of the
Mozilla Public License Version 1.1 (the "MPL v1.1"), in which case the provisions
of the MPL v1.1 are applicable instead of those in the SpTBXLib License.
A copy of the MPL v1.1 may be found in MPL-LICENSE.txt or at:
  http://www.mozilla.org/MPL/

If you wish to allow use of your version of this file only under the terms of
the MPL v1.1 and not to allow others to use your version of this file under the
SpTBXLib License, indicate your decision by deleting the provisions
above and replace them with the notice and other provisions required by the
MPL v1.1. If you do not delete the provisions above, a recipient may use your
version of this file under either the SpTBXLib License or the MPL v1.1.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The initial developer of this code is Robert Lee.

Requirements:
For Delphi/C++Builder 2009 or newer:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
For Delphi/C++Builder 7-2007:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
  - Troy Wolbrink's TNT Unicode Controls
    http://www.tntware.com/delphicontrols/unicode/

History:
17 January 2009 - version 2.4
  - No changes.

26 September 2008 - version 2.3
  - No changes.

29 July 2008 - version 2.2
  - No changes.

26 June 2008 - version 2.1
  - No changes.

3 May 2008 - version 2.0
  - No changes.

2 April 2008 - version 1.9.5
  - No changes.

3 February 2008 - version 1.9.4
  - No changes.

19 January 2008 - version 1.9.3
  - Added BlankSeparators property to TSpTBXCustomizer.
  - Fixed AV on TSpTBXCustomizer when ShorcutsList was nil
    when calling ApplyItemOptions, thanks to PyScripter for
    reporting this.

26 December 2007 - version 1.9.2
  - No changes.

1 December 2007 - version 1.9.1
  - No changes.

20 November 2007 - version 1.9
  - Removed TBX dependency.
  - Added Reset method to TSpTBXCustomizer, used to reinitialize
    the toolbars.

8 February 2007 - version 1.8.3
  - Added DeleteLayout method to TSpTBXCustomizer.
  - The customizer now closes when ESC is pressed, thanks to
    Jim Kueneman for reporting this.
  - The customizer now saves the Toolbar's DisplayMode, thanks to
    Jim Kueneman for reporting this.

17 December 2006 - version 1.8.2
  - No changes.

24 November 2006 - version 1.8.1
  - No changes.

27 August 2006 - version 1.8
  - Fixed TSpTBXCustomizer items saving when the MenuBar items are
    nested in more than 3 subitems levels, thanks to Jim Kueneman
    for reporting this.

15 June 2006 - version 1.7
  - Added SaveFormState property to TSpTBXCustomizer, when SaveFormState
    is true the main form position and WindowState are saved.
  - Added Load and Save methods to TSpTBXCustomizer that loads/saves
    the customizer options to a StringList, thanks to Philipp Hechter
    for reporting this.

4 May 2006 - version 1.6
  - No changes.

12 April 2006 - version 1.5
  - No changes.

27 February 2006 - version 1.4
  - No changes.

10 February 2006 - version 1.3
  - No changes.

28 December 2005 - version 1.2
  - Fixed incorrect ShortCut processing.

18 October 2005 - version 1.1
  - Fixed TSpTBXCustomizer ShortCut processing method when
    loading from file or registry.
  - Fixed TSpTBXCustomizer support for separators items.
  - Fixed TSpTBXCustomizer support for anchored items.
  - Added OnGetShortcutsList event to the TSpTBXCustomizer to
    allow the shortcuts list filtering.
  - Added separator cloning support to the TSpTBXCustomizer.

10 August 2005 - version 1.0
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ImgList, IniFiles,
  {$IFNDEF UNICODE}
  TntClasses,
  {$ENDIF}
  TB2Dock, TB2Toolbar, TB2Item,
  SpTBXSkins, SpTBXItem, SpTBXEditors;


type

{$IFDEF UNICODE}
  TTntStringList = TStringList;
{$ENDIF}

  TSpTBXCustomizer = class;

  TShortCutsProcessor = class
  private
    FActive: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function MainWindowHook(var Message: TMessage): Boolean;
    property Active: Boolean read FActive write FActive;
  end;

  TSpTBXMenuBarShortcuts = class(TTntStringList)
  private
    FMenuBarName: string;
  public
    property MenuBarName: string read FMenuBarName write FMenuBarName;
  end;

  TSpTBXCustomCustomizeForm = class(TForm)
  private
    FEmbedded: Boolean;
  protected
    FCustomizer: TSpTBXCustomizer;
    FToolbarList: TTntStringList;
    FItemList: TTntStringList;
    FShortcutList: TTntStringList;
    FSeparatorList: TTntStringList;
    FBlankSeparatorList: TTntStringList;
    procedure DoFillCommands(ToolbarList, ItemList, ShortcutsList: TTntStringList); virtual; abstract;
    procedure DoShow; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoIconOptionsChange(UseSmallImages: Boolean); virtual;
    procedure DoSkinChange; virtual;
    procedure FillCommands;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TSpTBXCustomizer; EmbeddedParent: TWinControl); reintroduce;
    destructor Destroy; override;
    property Customizer: TSpTBXCustomizer read FCustomizer;
    property Embedded: Boolean read FEmbedded;
  end;

  TSpTBXCustomizeFormClass = class of TSpTBXCustomCustomizeForm;

  TSpTBXGetCustomizeFormClassEvent = procedure(Sender: TObject; var CustomizeFormClass: TSpTBXCustomizeFormClass) of object;
  TSpTBXExtraOptionsEvent = procedure(Sender: TObject; ExtraOptions: TStringList) of object;
  TSpTBXLayoutExtraOptionsEvent = procedure(Sender: TObject; LayoutName: string; ExtraOptions: TStringList) of object;

  TSpTBXIconOptionsChangeEvent = procedure(Sender: TObject; Toolbar: TTBCustomToolbar; UseSmallImages: Boolean) of object;
  TSpTBXAcceptItemEvent = procedure(Sender: TObject; AItem: TTBCustomItem; var Accept: Boolean) of object;

  TSpTBXCustomizer = class(TComponent, ITBItems)
  private
    FLayouts: TStringList;
    FResetState: TStringList;
    FBlankSeparators: Boolean;
    FItems: TTBRootItem;
    FCustomizeForm: TSpTBXCustomCustomizeForm;
    FMenuBar: TTBCustomToolbar;
    FShowing: Boolean;
    FSaveFormState: Boolean;
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnGetCustomizeForm: TSpTBXGetCustomizeFormClassEvent;
    FOnLoad: TSpTBXExtraOptionsEvent;
    FOnSave: TSpTBXExtraOptionsEvent;
    FOnLayoutLoad: TSpTBXLayoutExtraOptionsEvent;
    FOnLayoutSave: TSpTBXLayoutExtraOptionsEvent;
    FOnGetShortcutsList: TSpTBXAcceptItemEvent;
    FOnIconOptionsChange: TSpTBXIconOptionsChangeEvent;
    FOnSkinChange: TNotifyEvent;
    function GetItems: TTBCustomItem;  // For ITBItems interface
    function GetImages: TCustomImageList;
    procedure SetImages(Value: TCustomImageList);
    procedure SetMenuBar(const Value: TTBCustomToolbar);
    procedure SaveResetState(ShortcutsList: TSpTBXMenuBarShortcuts);
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    function DoGetShortcutsList(AItem: TTBCustomItem): Boolean; virtual;
    procedure DoIconOptionsChange(Toolbar: TTBCustomToolbar; UseSmallImages: Boolean); virtual;
    procedure DoLoad(ExtraOptions: TStringList); virtual;
    procedure DoSave(ExtraOptions: TStringList); virtual;
    procedure DoLayoutLoad(LayoutName: string; ExtraOptions: TStringList); virtual;
    procedure DoLayoutSave(LayoutName: string; ExtraOptions: TStringList); virtual;
    procedure DoSkinChange; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;  // For ITBItems interface
    function GetCustomizeFormClass: TSpTBXCustomizeFormClass; virtual;
    procedure GetShortcutList(ShortcutsList: TTntStringList);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetupForm; virtual;
    procedure ValidateContainer(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show;
    procedure ShowEmbedded(AParent: TWinControl = nil);
    procedure Close;
    procedure Load(OptionsList: TStrings; LoadLastLayout: Boolean = True); overload;
    procedure Load(const Filename: string; LoadLastLayout: Boolean = True); overload;
    procedure Load(const RootKey: DWORD; BaseRegistryKey: string; LoadLastLayout: Boolean = True); overload;
    procedure Save(OptionsList: TStrings); overload;
    procedure Save(MemIni: TMemIniFile); overload;
    procedure Save(const Filename: string); overload;
    procedure Save(const RootKey: DWORD; BaseRegistryKey: string); overload;
    procedure Reset;
    procedure LoadLayout(OptionsList: TStrings; LayoutName: string); overload;
    procedure LoadLayout(const Filename, LayoutName: string); overload;
    procedure LoadLayout(const RootKey: DWORD; BaseRegistryKey, LayoutName: string); overload;
    procedure SaveLayout(MemIni: TMemIniFile; LayoutName: string); overload;
    procedure SaveLayout(OptionsList: TStrings; LayoutName: string); overload;
    procedure SaveLayout(const Filename, LayoutName: string); overload;
    procedure SaveLayout(const RootKey: DWORD; BaseRegistryKey, LayoutName: string); overload;
    function DeleteLayout(OptionsList: TStrings; LayoutName: string): Boolean; overload;
    function DeleteLayout(const Filename, LayoutName: string): Boolean; overload;
    function DeleteLayout(const RootKey: DWORD; BaseRegistryKey, LayoutName: string): Boolean; overload;
    property CustomizeForm: TSpTBXCustomCustomizeForm read FCustomizeForm;
    property Layouts: TStringList read FLayouts;
    property Showing: Boolean read FShowing;
  published
    property BlankSeparators: Boolean read FBlankSeparators write FBlankSeparators default False;
    property Images: TCustomImageList read GetImages write SetImages;
    property Items: TTBRootItem read FItems;
    property MenuBar: TTBCustomToolbar read FMenuBar write SetMenuBar;
    property SaveFormState: Boolean read FSaveFormState write FSaveFormState default True;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnGetCustomizeForm: TSpTBXGetCustomizeFormClassEvent read FOnGetCustomizeForm write FOnGetCustomizeForm;
    property OnGetShortcutsList: TSpTBXAcceptItemEvent read FOnGetShortcutsList write FOnGetShortcutsList;
    property OnLoad: TSpTBXExtraOptionsEvent read FOnLoad write FOnLoad;
    property OnSave: TSpTBXExtraOptionsEvent read FOnSave write FOnSave;
    property OnLayoutLoad: TSpTBXLayoutExtraOptionsEvent read FOnLayoutLoad write FOnLayoutLoad;
    property OnLayoutSave: TSpTBXLayoutExtraOptionsEvent read FOnLayoutSave write FOnLayoutSave;
    property OnIconOptionsChange: TSpTBXIconOptionsChangeEvent read FOnIconOptionsChange write FOnIconOptionsChange;
    property OnSkinChange: TNotifyEvent read FOnSkinChange write FOnSkinChange;
  end;

{ SpTBMemIniLoadPositions and SpTBMemIniSavePositions are the same as TBIniLoadPositions/TBIniSavePositions
  of the TB2Dock unit but uses TMemIniFile instead of TIniFile for better readability and to solve
  the #7363 bug report from QC: http://qc.borland.com/wc/qcmain.aspx?d=7363 }
procedure SpTBMemIniLoadPositions(const OwnerComponent: TComponent; MemIni: TMemIniFile; SectionNamePrefix: string);
procedure SpTBMemIniSavePositions(const OwnerComponent: TComponent; MemIni: TMemIniFile; SectionNamePrefix: string);

{ Ini/Reg }
procedure SpMemIniEraseSection(MemIni: TMemIniFile; Section: string; EraseKeysOnly: Boolean);
procedure SpMemIniSaveStringList(MemIni: TMemIniFile; Section: string; L: TStringList);
procedure SpMemIniLoadStringList(MemIni: TMemIniFile; Section: string; L: TStringList);
procedure SpRegSaveStringList(L: TStringList; const RootKey: DWORD; const BaseRegistryKey: string);
procedure SpRegLoadStringList(L: TStringList; const RootKey: DWORD; const BaseRegistryKey: string);

{ Layouts }
procedure SpLoadFormState(Form: TCustomForm; OptionsList: TStrings);
procedure SpSaveFormState(Form: TCustomForm; OptionsList: TStrings);
procedure SpLoadLayoutList(MemIni: TMemIniFile; L: TStringList); overload;
procedure SpLoadLayoutList(const RootKey: DWORD; BaseRegistryKey: string; L: TStringList); overload;
procedure SpLoadLayout(const OwnerComponent: TComponent; MemIni: TMemIniFile; LayoutName: string; ExtraOptions: TStringList = nil); overload;
procedure SpLoadLayout(const OwnerComponent: TComponent; const RootKey: DWORD; BaseRegistryKey, LayoutName: string; ExtraOptions: TStringList = nil); overload;
procedure SpSaveLayout(const OwnerComponent: TComponent; MemIni: TMemIniFile; LayoutName: string; ExtraOptions: TStringList = nil); overload;
procedure SpSaveLayout(const OwnerComponent: TComponent; const RootKey: DWORD; BaseRegistryKey, LayoutName: string; ExtraOptions: TStringList = nil); overload;

{ Items }
procedure SpLoadItems(const OwnerComponent: TComponent; MemIni: TMemIniFile; ShortcutsList: TSpTBXMenuBarShortcuts = nil; ExtraOptions: TStringList = nil); overload;
procedure SpLoadItems(const OwnerComponent: TComponent; const RootKey: DWORD; BaseRegistryKey: string; ShortcutsList: TSpTBXMenuBarShortcuts = nil; ExtraOptions: TStringList = nil); overload;
procedure SpSaveItems(const OwnerComponent: TComponent; MemIni: TMemIniFile; ShortcutsList: TSpTBXMenuBarShortcuts = nil; ExtraOptions: TStringList = nil); overload;
procedure SpSaveItems(const OwnerComponent: TComponent; const RootKey: DWORD; BaseRegistryKey: string; ShortcutsList: TSpTBXMenuBarShortcuts = nil; ExtraOptions: TStringList = nil); overload;

{ Misc }
function SpCustomizerGetWideCaption(Item: TTBCustomItem): WideString;
function SpCreateUniqueSeparator(Blank: Boolean): TSpTBXSeparatorItem;

implementation

uses
  TypInfo, Registry, Menus, ActnList, TB2ExtItems,
  SpTBXTabs, SpTBXDkPanels, SpTBXCustomizerForm;

type
  TTBRootItemAccess = class(TTBRootItem);
  TTBCustomItemAccess = class(TTBCustomItem);
  TTBDockAccess = class(TTBDock);
  TSpTBXTabToolbarAccess = class(TSpTBXTabToolbar);

{ Constants for SpTBXCustomizer-specific registry values. Do not localize! }
const
  SSpTBXCustomizerRepeatedInstance = 'There''s already another instance of TSpTBXCustomizer';
  SSpTBXCustomizerInvalidParent = 'TSpTBXCustomizer must be dropped only on a Form or on a Frame';
  rvLastLayout = 'LastLayout';
  rvExtraOptions = 'ExtraOptions';
  rvLayoutRegList = 'LayoutsList';
  rvLayoutList = 'Layouts';
  rvItemsList = 'Items';
  rvCount = 'Count';
  rvSkin = 'Skin';
  rvMainFormWindowState = 'MainForm.WindowState';
  rvMainFormBounds = 'MainForm.Bounds';
  rvMainFormRestoreBounds = 'MainForm.RestoreBounds';
  rvSeparator = '--[Separator]--';
  rvBlankSeparator = '--[Blank Separator]--';
  rvUniqueSeparatorPrefix = 'CustomizerUniqueSeparator';
  rvUniqueBlankSeparatorPrefix = 'CustomizerUniqueBlankSeparator';

var
  FShortcutsProcessor: TShortCutsProcessor;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

function IncludeTrailingRegKeyDelimiter(const S: string): string;
var
  C: integer;
begin
  Result := S;
  C := Length(Result);
  if (C > 0) and (Result[C] <> '\') then
    Result := Result + '\';
end;

function ParseItemEntry(RootItemsList, ItemsList: TStringList; const ItemEntry: string;
  out ParentItem: TTBCustomItem; out ItemName: string; out ShortCut: TShortCut): TTBCustomItem;
var
  L: TStringList;
  I: Integer;
  TBName: string;
begin
  Result := nil;
  ParentItem := nil;
  ShortCut := 0;

  L := TStringList.Create;
  try
    L.CommaText := ItemEntry;
    TBName := L[0];
    ItemName := L[1];
    if L.Count > 2 then
      if L[2] = '0' then
        ShortCut := 0
      else
        ShortCut := TextToShortCut(L[2]);
    I := RootItemsList.IndexOf(TBName);
    if I > -1 then begin
      ParentItem := RootItemsList.Objects[I] as TTBCustomItem;
      if Pos(rvUniqueSeparatorPrefix, ItemName) > 0 then begin    // If it's a cloned separator create it
        Result := TSpTBXSeparatorItem.Create(nil);
        Result.Name := ItemName;
      end
      else
        if Pos(rvUniqueBlankSeparatorPrefix, ItemName) > 0 then begin  // If it's a cloned blank separator create it
          Result := TSpTBXSeparatorItem.Create(nil);
          Result.Name := ItemName;
          TSpTBXSeparatorItem(Result).Blank := True;
        end
        else begin
          I := ItemsList.IndexOf(ItemName);
          if I > -1 then
            Result := ItemsList.Objects[I] as TTBCustomItem;
        end;
    end;
  finally
    L.Free;
  end;
end;

procedure GetRootItemsList(OwnerComponent: TComponent; RootItemsList: TStringList);
// Creates a list with all the toolbar's root items
// It excludes Frames toolbars (name collision problems)
var
  I: Integer;
  C: TComponent;
begin
  for I := 0 to OwnerComponent.ComponentCount - 1 do begin
    C := OwnerComponent.Components[I];
    if C is TFrame then
      Exit
    else
      if C is TSpTBXToolbar then begin
        if TSpTBXToolbar(C).Customizable then
          RootItemsList.AddObject(C.Name, TSpTBXToolbar(C).Items);
      end
      else
        if C is TSpTBXCustomizer then
          RootItemsList.AddObject(C.Name, TSpTBXCustomizer(C).Items)
        else
          if C is TWinControl then
            GetRootItemsList(C, RootItemsList);
  end;
end;

procedure SaveItemOptions(OwnerComponent: TComponent;
  ShortcutsList: TSpTBXMenuBarShortcuts; OptionsList: TStringList);

  procedure SaveOption(ComponentName: string; AItem: TTBCustomItem);
  var
    ShortCut: string;
  begin
    ShortCut := ShortCutToText(AItem.ShortCut);
    if ShortCut = '' then ShortCut := '0';
    OptionsList.Add(ComponentName + ', ' + AItem.Name + ', ' + ShortCut);
  end;

var
  I, J: Integer;
  ParentItem: TTBCustomItem;
  RootItemsList: TStringList;
begin
  OptionsList.Clear;

  RootItemsList := TStringList.Create;
  try
    GetRootItemsList(OwnerComponent, RootItemsList);
    for I := 0 to RootItemsList.Count - 1 do begin
      ParentItem := TTBCustomItem(RootItemsList.Objects[I]);
      for J := 0 to ParentItem.Count - 1 do
        SaveOption(RootItemsList.Strings[I], ParentItem[J]);
    end;
  finally
    RootItemsList.Free;
  end;

  // Save the MenuBar shortcuts
  if Assigned(ShortcutsList) then begin
    for J := 0 to ShortcutsList.Count - 1 do
      SaveOption(ShortcutsList.MenuBarName, ShortcutsList.Objects[J] as TTBCustomItem);
  end;
end;

procedure ApplyItemOptions(OwnerComponent: TComponent;
  ShortcutsList: TSpTBXMenuBarShortcuts; OptionsList: TStringList);
var
  RootItemsList, ItemsList: TStringList;
  I, J, OrigPos, InsertPoint: Integer;
  Item, ParentItem, AuxParentItem: TTBCustomItem;
  ParentToolbar: TSpTBXTabToolbarAccess;
  ItemName: string;
  ShortCut: TShortCut;
begin
  RootItemsList := TStringList.Create;
  ItemsList := TStringList.Create;
  try
    GetRootItemsList(OwnerComponent, RootItemsList);
    for I := 0 to RootItemsList.Count - 1 do begin
      ParentItem := TTBCustomItem(RootItemsList.Objects[I]);
      for J := 0 to ParentItem.Count - 1 do
        ItemsList.AddObject(ParentItem[J].Name, ParentItem[J]);
    end;

    InsertPoint := 0;
    AuxParentItem := nil;
    for I := 0 to OptionsList.Count - 1 do begin
      Item := ParseItemEntry(RootItemsList, ItemsList, OptionsList[I], ParentItem, ItemName, ShortCut);
      if Assigned(Item) then begin
        if AuxParentItem <> ParentItem then begin
          AuxParentItem := ParentItem;
          InsertPoint := 0;
        end;
        // Move the item if the parent or index are different
        if not Assigned(Item.Parent) then
          ParentItem.Insert(InsertPoint, Item)
        else
          if Item.Parent <> ParentItem then begin
            Item.Parent.Remove(Item);
            ParentItem.Insert(InsertPoint, Item);
          end
          else begin
            OrigPos := Item.Parent.IndexOf(Item);
            if OrigPos <> InsertPoint then begin
              if Item.Parent.ParentComponent is TSpTBXTabToolbar then begin
                ParentToolbar := TSpTBXTabToolbarAccess(Item.Parent.ParentComponent);
                ParentToolbar.BeginItemMove;
                ParentToolbar.View.BeginUpdate;
                try
                  // The item is the active tab, we need to update the ActiveTabIndex
                  // Just set the internal value because the page didn't change
                  if ParentToolbar.FActiveTabIndex = OrigPos then
                    ParentToolbar.FActiveTabIndex := InsertPoint;
                  Item.Parent.Move(OrigPos, InsertPoint);
                finally
                  ParentToolbar.View.EndUpdate;
                  ParentToolbar.EndItemMove;
                end;
              end
              else
                Item.Parent.Move(OrigPos, InsertPoint);
            end;
          end;

        if Assigned(Item.Action) and (Item.Action is TAction) then
          TAction(Item.Action).ShortCut := ShortCut
        else
          Item.ShortCut := ShortCut;
        Inc(InsertPoint);
      end
      else begin
        // Item not found try to change the Shortcut
        if Assigned(ShortcutsList) then
          for J := 0 to ShortcutsList.Count - 1 do begin
            Item := ShortcutsList.Objects[J] as TTBCustomItem;
            if Item.Name = ItemName then begin
              if Assigned(Item.Action) and (Item.Action is TAction) then
                TAction(Item.Action).ShortCut := ShortCut
              else
                Item.ShortCut := ShortCut;
              Break;
            end;
          end;
      end;
    end;

  finally
    RootItemsList.Free;
    ItemsList.Free;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ SpTBMemIniLoadPositions/SpTBMemIniSavePositions }

procedure SpTBMemIniLoadPositions(const OwnerComponent: TComponent;
  MemIni: TMemIniFile; SectionNamePrefix: string);
begin
  SpTBIniLoadPositions(OwnerComponent, MemIni, SectionNamePrefix);
end;

procedure SpTBMemIniSavePositions(const OwnerComponent: TComponent;
  MemIni: TMemIniFile; SectionNamePrefix: string);
begin
  SpTBIniSavePositions(OwnerComponent, MemIni, SectionNamePrefix);
  if FileExists(MemIni.FileName) then
    MemIni.UpdateFile;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Ini/Reg }

procedure SpMemIniEraseSection(MemIni: TMemIniFile; Section: string; EraseKeysOnly: Boolean);
var
  I: Integer;
  Keys: TStringList;
begin
  if EraseKeysOnly then begin
    Keys := TStringList.Create;
    try
      MemIni.ReadSection(Section, Keys);
      for I := 0 to Keys.Count - 1 do
        MemIni.DeleteKey(Section, Keys[I]);
    finally
      Keys.Free;
    end;
  end
  else
    MemIni.EraseSection(Section);
end;

procedure SpMemIniSaveStringList(MemIni: TMemIniFile; Section: string; L: TStringList);
var
  I: integer;
begin
  if not Assigned(L) then Exit;
  SpMemIniEraseSection(MemIni, Section, True);
  if L.Count > 0 then begin
    MemIni.WriteInteger(Section, rvCount, L.Count);
    for I := 0 to L.Count - 1 do
      MemIni.WriteString(Section, IntToStr(I), L[I]);
    if FileExists(MemIni.FileName) then
      MemIni.UpdateFile;
  end;
end;

procedure SpMemIniLoadStringList(MemIni: TMemIniFile; Section: string; L: TStringList);
var
  I, C: integer;
begin
  if not Assigned(L) then Exit;
  L.Clear;
  C := MemIni.ReadInteger(Section, rvCount, -1);
  for I := 0 to C - 1 do
    L.Add(MemIni.ReadString(Section, IntToStr(I), ''));
end;

procedure SpRegSaveStringList(L: TStringList; const RootKey: DWORD; const BaseRegistryKey: string);
var
  Reg: TRegistry;
  I: integer;
begin
  if not Assigned(L) then Exit;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    Reg.DeleteKey(BaseRegistryKey);
    if Reg.OpenKey(BaseRegistryKey, True) and (L.Count > 0) then begin
      Reg.WriteInteger(rvCount, L.Count);
      for I := 0 to L.Count - 1 do
        Reg.WriteString(IntToStr(I), L[I]);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure SpRegLoadStringList(L: TStringList; const RootKey: DWORD; const BaseRegistryKey: string);
var
  Reg: TRegistry;
  I, C: integer;
begin
  if not Assigned(L) then Exit;
  Reg := TRegistry.Create;
  try
    L.Clear;
    Reg.RootKey := RootKey;
    if Reg.OpenKey(BaseRegistryKey, True) then begin
      if Reg.ValueExists(rvCount) then begin
        C := Reg.ReadInteger(rvCount);
        for I := 0 to C - 1 do
          if Reg.ValueExists(inttostr(I)) then
            L.Add(Reg.ReadString(inttostr(I)));
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Layouts }

procedure SpLoadFormState(Form: TCustomForm; OptionsList: TStrings);
var
  WState: TWindowState;
  R: TRect;
begin
  if Assigned(Form) then begin
    WState := TWindowState(GetEnumValue(TypeInfo(TWindowState), OptionsList.Values[rvMainFormWindowState]));
    if (WState < Low(WState)) or (WState > High(WState)) then
      WState := Form.WindowState; // Failed reading from string, leave the default value

    if SpStringToRect(OptionsList.Values[rvMainFormBounds], R) then
      Form.SetBounds(R.Left, R.Top, R.Right, R.Bottom);

    if not SpStringToRect(OptionsList.Values[rvMainFormRestoreBounds], R) then
      R := Rect(Form.Left, Form.Top, Form.Width, Form.Height);  // Failed reading from string, leave the default value

    SpSetFormWindowState(Form, WState, R);
  end;
end;

procedure SpSaveFormState(Form: TCustomForm; OptionsList: TStrings);
var
  WState: TWindowState;
  RestoreBounds: TRect;
begin
  if Assigned(Form) then begin
    WState := SpGetFormWindowState(Form, RestoreBounds);
    OptionsList.Values[rvMainFormWindowState] := GetEnumName(TypeInfo(TWindowState), Ord(WState));
    OptionsList.Values[rvMainFormBounds] := SpRectToString(Rect(Form.Left, Form.Top, Form.Width, Form.Height));
    OptionsList.Values[rvMainFormRestoreBounds] := SpRectToString(RestoreBounds);
  end;
end;

procedure SpLoadLayoutList(MemIni: TMemIniFile; L: TStringList); overload;
begin
  SpMemIniLoadStringList(MemIni, rvLayoutList, L);
end;

procedure SpLoadLayoutList(const RootKey: DWORD; BaseRegistryKey: string;
  L: TStringList); overload;
begin
  BaseRegistryKey := IncludeTrailingRegKeyDelimiter(BaseRegistryKey) + rvLayoutList;
  SpRegLoadStringList(L, RootKey, BaseRegistryKey);
end;

procedure SpSaveLayout(const OwnerComponent: TComponent;
  MemIni: TMemIniFile; LayoutName: string; ExtraOptions: TStringList = nil); overload;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    // Save the LayoutList
    SpMemIniLoadStringList(MemIni, rvLayoutList, L);
    if L.IndexOf(LayoutName) = -1 then
      L.Add(LayoutName);
    SpMemIniSaveStringList(MemIni, rvLayoutList, L);

    // Save the Layout
    SpTBMemIniSavePositions(OwnerComponent, MemIni, LayoutName + ' @ ');

    // Save the Multidocks
    if Assigned(ExtraOptions) then
      SpMemIniSaveStringList(MemIni, LayoutName + ' @ ' + rvExtraOptions, ExtraOptions);
  finally
    L.Free;
  end;
end;

procedure SpSaveLayout(const OwnerComponent: TComponent; const RootKey: DWORD;
  BaseRegistryKey, LayoutName: string; ExtraOptions: TStringList = nil); overload;
var
  L: TStringList;
  S: string;
begin
  L := TStringList.Create;
  try
    // Save the LayoutList
    S := IncludeTrailingRegKeyDelimiter(BaseRegistryKey) + rvLayoutList;
    SpRegLoadStringList(L, RootKey, S);
    if L.IndexOf(LayoutName) = -1 then
      L.Add(LayoutName);
    SpRegSaveStringList(L, RootKey, S);

    // Save the Layout
    S := IncludeTrailingRegKeyDelimiter(BaseRegistryKey) + rvLayoutRegList + '\' + LayoutName;
    TBRegSavePositions(OwnerComponent, RootKey, S);

    // Save the MultiDocks
    if Assigned(ExtraOptions) then
      SpRegSaveStringList(ExtraOptions, RootKey, S + '\' + rvExtraOptions);
  finally
    L.Free;
  end;
end;

procedure SpLoadLayout(const OwnerComponent: TComponent;
   MemIni: TMemIniFile; LayoutName: string; ExtraOptions: TStringList = nil); overload;
var
  L: TStringList;
begin
  // Check if the layout exists on the layout list and load it
  L := TStringList.Create;
  try
    SpMemIniLoadStringList(MemIni, rvLayoutList, L);
    if L.IndexOf(LayoutName) > -1 then begin
      SpTBMemIniLoadPositions(OwnerComponent, MemIni, LayoutName + ' @ ');

      SpMemIniLoadStringList(MemIni, LayoutName + ' @ ' + rvExtraOptions, L);
      if Assigned(ExtraOptions) then
        ExtraOptions.Assign(L);
    end;
  finally
    L.Free;
  end;
end;

procedure SpLoadLayout(const OwnerComponent: TComponent; const RootKey: DWORD;
  BaseRegistryKey, LayoutName: string; ExtraOptions: TStringList = nil) overload;
var
  L: TStringList;
  S: string;
begin
  // Check if the layout exists on the layout list and load it
  L := TStringList.Create;
  try
    S := IncludeTrailingRegKeyDelimiter(BaseRegistryKey) + rvLayoutList;
    SpRegLoadStringList(L, RootKey, S);
    if L.IndexOf(LayoutName) > -1 then begin
      S := IncludeTrailingRegKeyDelimiter(BaseRegistryKey) + rvLayoutRegList + '\' + LayoutName;
      TBRegLoadPositions(OwnerComponent, RootKey, S);

      SpRegLoadStringList(ExtraOptions, RootKey, S + '\' + rvExtraOptions);
      if Assigned(ExtraOptions) then
        ExtraOptions.Assign(L);
    end;
  finally
    L.Free;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Items }

procedure SpSaveItems(const OwnerComponent: TComponent; MemIni: TMemIniFile;
  ShortcutsList: TSpTBXMenuBarShortcuts = nil; ExtraOptions: TStringList = nil); overload;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    SaveItemOptions(OwnerComponent, ShortcutsList, L);
    SpMemIniSaveStringList(MemIni, rvItemsList, L);
    SpMemIniSaveStringList(MemIni, rvExtraOptions, ExtraOptions);
  finally
    L.Free;
  end;
end;

procedure SpSaveItems(const OwnerComponent: TComponent; const RootKey: DWORD;
  BaseRegistryKey: string; ShortcutsList: TSpTBXMenuBarShortcuts = nil;
  ExtraOptions: TStringList = nil); overload;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    BaseRegistryKey := IncludeTrailingRegKeyDelimiter(BaseRegistryKey);
    SaveItemOptions(OwnerComponent, ShortcutsList, L);
    SpRegSaveStringList(L, RootKey, BaseRegistryKey + rvItemsList);
    SpRegSaveStringList(ExtraOptions, RootKey, BaseRegistryKey + rvExtraOptions);
  finally
    L.Free;
  end;
end;

procedure SpLoadItems(const OwnerComponent: TComponent; MemIni: TMemIniFile;
  ShortcutsList: TSpTBXMenuBarShortcuts = nil; ExtraOptions: TStringList = nil); overload;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    SpMemIniLoadStringList(MemIni, rvItemsList, L);
    ApplyItemOptions(OwnerComponent, ShortcutsList, L);
    SpMemIniLoadStringList(MemIni, rvExtraOptions, ExtraOptions);
  finally
    L.Free;
  end;
end;

procedure SpLoadItems(const OwnerComponent: TComponent; const RootKey: DWORD; BaseRegistryKey: string;
  ShortcutsList: TSpTBXMenuBarShortcuts = nil; ExtraOptions: TStringList = nil); overload;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    BaseRegistryKey := IncludeTrailingRegKeyDelimiter(BaseRegistryKey);
    SpRegLoadStringList(L, RootKey, BaseRegistryKey + rvItemsList);
    ApplyItemOptions(OwnerComponent, ShortcutsList, L);
    SpRegLoadStringList(ExtraOptions, RootKey, BaseRegistryKey + rvExtraOptions);
  finally
    L.Free;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Misc }

function SpCustomizerGetWideCaption(Item: TTBCustomItem): WideString;
begin
  if Item is TSpTBXCustomItem then Result := TSpTBXCustomItem(Item).Caption
  else if Item is TSpTBXEditItem then Result := TSpTBXEditItem(Item).Caption
  else if Item is TTBSeparatorItem then Result := Item.Name
  else if Item is TTBGroupItem then Result := Item.Name
  else Result := Item.Caption;
  Result := SpStripShortcut(Result);
  Result := SpStripAccelChars(Result);
end;

function SpCreateUniqueSeparator(Blank: Boolean): TSpTBXSeparatorItem;
var
  S: string;
begin
  S := IntToStr(DateTimeToFileDate(Now));
  Result := TSpTBXSeparatorItem.Create(nil);
  if Blank then begin
    Result.Name := rvUniqueBlankSeparatorPrefix + S;
    TSpTBXSeparatorItem(Result).Blank := True;
  end
  else
    Result.Name := rvUniqueSeparatorPrefix + S;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TShortCutsProcessor }

constructor TShortCutsProcessor.Create;
begin
  inherited;
  if Assigned(Application) then
    Application.HookMainWindow(MainWindowHook);
end;

destructor TShortCutsProcessor.Destroy;
begin
  if Assigned(Application) then
    Application.UnhookMainWindow(MainWindowHook);
  inherited;
end;

function TShortCutsProcessor.MainWindowHook(var Message: TMessage): Boolean;
begin
  // Hook to the Application's message loop to disable ShortCut messages processing.
  // If the Form is non-modal and a key is pressed it tries to find a shortcut
  // handler in the main form, we need to disable this.
  // We have to hook in the initialization section so the hook is prior the
  // TTBCustomToolbar hook in the Application's hooklist.

  // Shortcut messages:
  // Whenever a keystroke is pressed and is not handled by the active control
  // or a suitable popup menu item, it is passed to the underlying form's
  // IsShortCut method. The form tries to handle the keystroke through its
  // OnShortCut event or, failing that, through its main menu. If nothing wants
  // it, all action lists owned by the form are checked for a matching shortcut.
  // The action list checks each of its actions and if a match is found,
  // the action's Execute method is called.
  // If no suitable action is found on the current form a CM_APPKEYDOWN message
  // is sent to the Application object which calls its own IsShortCut method.
  // This tries to handle the keystroke in its own OnShortCut event and if
  // that fails it calls the main form's IsShortCut method.

  // Key press -> Active Control -> Popup Menu -> TCustomForm.IsShortCut ->
  // TCustomForm.MainMenu -> TActionList -> TApplication CM_APPKEYDOWN and CM_APPSYSCOMMAND ->
  // TApplication.IsShortCut

  Result := False;
  if FActive then
    if (Message.Msg = CM_APPKEYDOWN) or (Message.Msg = CM_APPSYSCOMMAND) then begin
      Message.Result := 0;
      Result := true;
    end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomizer }

constructor TSpTBXCustomizer.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TTBRootItem.Create(Self);
  FItems.ParentComponent := Self;
  FLayouts := TStringList.Create;
  FResetState := TStringList.Create;
  FSaveFormState := True;
  SkinManager.AddSkinNotification(Self);
end;

destructor TSpTBXCustomizer.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  FItems.Free;
  FLayouts.Free;
  FResetState.Free;  
  inherited;
end;

procedure TSpTBXCustomizer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    if AComponent = FCustomizeForm then FCustomizeForm := nil;
    if AComponent = FMenuBar then FMenuBar := nil;
  end;
end;

procedure TSpTBXCustomizer.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
begin
  TTBRootItemAccess(FItems).GetChildren(Proc, Root);
end;

function TSpTBXCustomizer.GetCustomizeFormClass: TSpTBXCustomizeFormClass;
begin
  Result := TSpTBXCustomizeForm;
  if Assigned(FOnGetCustomizeForm) then FOnGetCustomizeForm(Self, Result);
  if not Assigned(Result) then
    Result := TSpTBXCustomizeForm;
end;

procedure TSpTBXCustomizer.GetShortcutList(ShortcutsList: TTntStringList);
var
  I: Integer;
  Item: TTBCustomItem;
  ItemStyle: TTBItemStyle;
  L: TTntStringList;
begin
  ShortcutsList.Clear;

  if Assigned(MenuBar) then begin
    if ShortcutsList is TSpTBXMenuBarShortcuts then
      TSpTBXMenuBarShortcuts(ShortcutsList).MenuBarName := MenuBar.Name;

    L := TTntStringList.Create;
    try
      SpGetAllItems(MenuBar.Items, L);
      for I := 0 to L.Count - 1 do begin
        Item := L.Objects[I] as TTBCustomItem;
        ItemStyle := TTBCustomItemAccess(Item).ItemStyle;
        // Exclude the submenus, separators, labels, groups and edit items
        if (ItemStyle * [tbisSubMenu, tbisSeparator, tbisEmbeddedGroup, tbisClicksTransparent] = []) and
          not (Item is TTBEditItem) then
        begin
          if DoGetShortcutsList(Item) then
            ShortcutsList.AddObject(SpCustomizerGetWideCaption(Item), Item);
        end;
      end;
    finally
      L.Free;
    end;
  end;
end;

function TSpTBXCustomizer.GetImages: TCustomImageList;
begin
  Result := FItems.SubMenuImages;
end;

function TSpTBXCustomizer.GetItems: TTBCustomItem;
begin
  Result := FItems;
end;

procedure TSpTBXCustomizer.SetImages(Value: TCustomImageList);
begin
  FItems.SubMenuImages := Value;
end;

procedure TSpTBXCustomizer.SetMenuBar(const Value: TTBCustomToolbar);
begin
  if FMenuBar <> Value then begin
    FMenuBar := Value;
    if Assigned(Value) then
      Value.FreeNotification(Self);
  end;
end;

procedure TSpTBXCustomizer.SetupForm;
begin
  FCustomizeForm.FormStyle := fsStayOnTop;
  FCustomizeForm.BorderIcons := [biSystemMenu];
  FCustomizeForm.Position := poMainFormCenter;
  SpCustomizeAllToolbars(Owner as TWinControl, False);
  SpCustomizeAllToolbars(FCustomizeForm, False);
end;

procedure TSpTBXCustomizer.Close;
begin
  if FShowing and Assigned(FCustomizeForm) then begin
    FCustomizeForm.Close;
    SpCustomizeAllToolbars(Owner as TWinControl, True);
    SpCustomizeAllToolbars(FCustomizeForm, True);
    FShowing := False;
    if Assigned(FOnClose) then FOnClose(Self);
  end;
end;

procedure TSpTBXCustomizer.Show;
begin
  ShowEmbedded;
end;

procedure TSpTBXCustomizer.ShowEmbedded(AParent: TWinControl = nil);
begin
  if not Assigned(FCustomizeForm) then
    FCustomizeForm := GetCustomizeFormClass.Create(Self, AParent);

  if not FShowing and Assigned(FCustomizeForm) then begin
    FShowing := True;
    SetupForm;
    FCustomizeForm.Show;
    FCustomizeForm.DoSkinChange;
    if Assigned(FOnShow) then FOnShow(Self);
  end;
end;

procedure TSpTBXCustomizer.ValidateContainer(AComponent: TComponent);
var
  I: Integer;
begin
  if Assigned(AComponent) then begin
    if not (AComponent is TCustomForm) and not (AComponent is TCustomFrame) then
      raise EInvalidOperation.Create(SSpTBXCustomizerInvalidParent);
    for I := 0 to AComponent.ComponentCount - 1 do
      if AComponent.Components[I] is TSpTBXCustomizer then
        raise EInvalidOperation.Create(SSpTBXCustomizerRepeatedInstance);
  end
  else
    raise EInvalidOperation.Create(SSpTBXCustomizerInvalidParent);

  inherited;
end;

procedure TSpTBXCustomizer.Load(OptionsList: TStrings; LoadLastLayout: Boolean = True);
var
  MemIni: TMemIniFile;
  ExtraL: TStringList;
  ShortcutsL: TSpTBXMenuBarShortcuts;
begin
  MemIni := TMemIniFile.Create('');
  ExtraL := TStringList.Create;
  ShortcutsL := TSpTBXMenuBarShortcuts.Create;
  try
    MemIni.SetStrings(OptionsList);

    // Load Shortcuts
    if Assigned(MenuBar) then
      GetShortcutList(ShortcutsL);

    // Save the ResetState, save only the Items and Toolbar state
    SaveResetState(ShortcutsL);

    // Load Items
    SpLoadItems(Owner, MemIni, ShortcutsL, ExtraL);

    // Fill Extra Options
    DoLoad(ExtraL);
    if FSaveFormState then
      SpLoadFormState(Application.MainForm, ExtraL);
    SkinManager.SetSkin(ExtraL.Values[rvSkin]);

    // Load Layouts
    SpLoadLayoutList(MemIni, FLayouts);
    if LoadLastLayout then
      LoadLayout(OptionsList, rvLastLayout);
  finally
    MemIni.Free;
    ExtraL.Free;
    ShortcutsL.Free;
  end;
end;

procedure TSpTBXCustomizer.Load(const Filename: string; LoadLastLayout: Boolean = True);
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    if FileExists(Filename) then begin
      L.LoadFromFile(Filename);
      Load(L, LoadLastLayout);
    end;
  finally
    L.Free;
  end;
end;

procedure TSpTBXCustomizer.Load(const RootKey: DWORD; BaseRegistryKey: string; LoadLastLayout: Boolean = True);
var
  ExtraL: TStringList;
  ShortcutsL: TSpTBXMenuBarShortcuts;
begin
  ExtraL := TStringList.Create;
  ShortcutsL := TSpTBXMenuBarShortcuts.Create;
  try
    // Load Shortcuts
    if Assigned(MenuBar) then
      GetShortcutList(ShortcutsL);

    // Save the ResetState, save only the Items and Toolbar state
    SaveResetState(ShortcutsL);

    // Load Items
    SpLoadItems(Owner, RootKey, BaseRegistryKey, ShortcutsL, ExtraL);

    // Fill Extra Options
    DoLoad(ExtraL);
    if FSaveFormState then
      SpLoadFormState(Application.MainForm, ExtraL);
    SkinManager.SetSkin(ExtraL.Values[rvSkin]);

    // Load Layouts
    SpLoadLayoutList(RootKey, BaseRegistryKey, FLayouts);
    if LoadLastLayout then
      LoadLayout(RootKey, BaseRegistryKey, rvLastLayout);
  finally
    ExtraL.Free;
    ShortcutsL.Free;
  end;
end;

procedure TSpTBXCustomizer.Save(MemIni: TMemIniFile);
var
  ExtraL: TStringList;
  ShortcutsL: TSpTBXMenuBarShortcuts;
begin
  ExtraL := TStringList.Create;
  ShortcutsL := TSpTBXMenuBarShortcuts.Create;
  try
    // Fill Extra Options, SpSaveItems will save it
    ExtraL.Values[rvSkin] := SkinManager.CurrentSkinName;
    if FSaveFormState then
      SpSaveFormState(Application.MainForm, ExtraL);
    DoSave(ExtraL);

    // Save Items
    if Assigned(MenuBar) then
      GetShortcutList(ShortcutsL);
    SpSaveItems(Owner, MemIni, ShortcutsL, ExtraL);

    // Save Layouts
    SaveLayout(MemIni, rvLastLayout);
  finally
    ExtraL.Free;
    ShortcutsL.Free;
  end;
end;

procedure TSpTBXCustomizer.Save(OptionsList: TStrings);
var
  MemIni: TMemIniFile;
begin
  MemIni := TMemIniFile.Create('');
  try
    MemIni.SetStrings(OptionsList); // Transfer L contents to MemIni
    Save(MemIni);
    OptionsList.Clear;
    MemIni.GetStrings(OptionsList); // Transfer MemIni contents to L
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXCustomizer.Save(const Filename: string);
var
  MemIni: TMemIniFile;
begin
  MemIni := TMemIniFile.Create(Filename);
  try
    Save(MemIni);
    MemIni.UpdateFile;    
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXCustomizer.Save(const RootKey: DWORD;
  BaseRegistryKey: string);
var
  ExtraL: TStringList;
  ShortcutsL: TSpTBXMenuBarShortcuts;
begin
  ExtraL := TStringList.Create;
  ShortcutsL := TSpTBXMenuBarShortcuts.Create;
  try
    // Fill Extra Options, SpSaveItems will save it
    ExtraL.Values[rvSkin] := SkinManager.CurrentSkinName;
    if FSaveFormState then
      SpSaveFormState(Application.MainForm, ExtraL);
    DoSave(ExtraL);

    // Save Items
    if Assigned(MenuBar) then
      GetShortcutList(ShortcutsL);
    SpSaveItems(Owner, RootKey, BaseRegistryKey, ShortcutsL, ExtraL);

    // Save Layouts
    SaveLayout(RootKey, BaseRegistryKey, rvLastLayout);
  finally
    ExtraL.Free;
    ShortcutsL.Free;
  end;
end;

procedure TSpTBXCustomizer.Reset;
var
  MemIni: TMemIniFile;
  ShortcutsL: TSpTBXMenuBarShortcuts;
begin
  // Load ResetState
  if FResetState.Count > 0 then begin
    MemIni := TMemIniFile.Create('');
    ShortcutsL := TSpTBXMenuBarShortcuts.Create;
    try
      MemIni.SetStrings(FResetState);
      // Load Shortcuts
      if Assigned(MenuBar) then
        GetShortcutList(ShortcutsL);
      // Load Items
      SpLoadItems(Owner, MemIni, ShortcutsL, nil);
      // Load last layout
      SpLoadLayout(Owner, MemIni, rvLastLayout, nil);
    finally
      MemIni.Free;
      ShortcutsL.Free;
    end;
  end;
end;

procedure TSpTBXCustomizer.LoadLayout(OptionsList: TStrings; LayoutName: string);
var
  MemIni: TMemIniFile;
  ExtraL: TStringList;
begin
  MemIni := TMemIniFile.Create('');
  ExtraL := TStringList.Create;
  try
    MemIni.SetStrings(OptionsList);

    SpLoadLayout(Owner, MemIni, LayoutName, ExtraL);
    DoLayoutLoad(LayoutName, ExtraL);
  finally
    ExtraL.Free;
    MemIni.Free;
  end;
end;

procedure TSpTBXCustomizer.LoadLayout(const Filename, LayoutName: string);
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    if FileExists(Filename) then begin
      L.LoadFromFile(Filename);
      LoadLayout(L, LayoutName);
    end;
  finally
    L.Free;
  end;
end;

procedure TSpTBXCustomizer.LoadLayout(const RootKey: DWORD;
  BaseRegistryKey, LayoutName: string);
var
  ExtraL: TStringList;
begin
  ExtraL := TStringList.Create;
  try
    SpLoadLayout(Owner, RootKey, BaseRegistryKey, LayoutName, ExtraL);
    DoLayoutLoad(LayoutName, ExtraL);
  finally
    ExtraL.Free;
  end;
end;

procedure TSpTBXCustomizer.SaveLayout(MemIni: TMemIniFile; LayoutName: string);
var
  ExtraL: TStringList;
begin
  if LayoutName = '' then Exit;
  ExtraL := TStringList.Create;
  try
    DoLayoutSave(LayoutName, ExtraL);
    SpSaveLayout(Owner, MemIni, LayoutName, ExtraL);
    SpLoadLayoutList(MemIni, FLayouts);
  finally
    ExtraL.Free;
  end;
end;

procedure TSpTBXCustomizer.SaveLayout(OptionsList: TStrings; LayoutName: string);
var
  MemIni: TMemIniFile;
begin
  if LayoutName = '' then Exit;
  MemIni := TMemIniFile.Create('');
  try
    MemIni.SetStrings(OptionsList);

    SaveLayout(MemIni, LayoutName);

    // Reload OptionsList
    OptionsList.Clear;
    MemIni.GetStrings(OptionsList);
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXCustomizer.SaveLayout(const Filename, LayoutName: string);
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    if FileExists(Filename) then
      L.LoadFromFile(Filename);
    SaveLayout(L, LayoutName);
    L.SaveToFile(Filename);
  finally
    L.Free;
  end;
end;

procedure TSpTBXCustomizer.SaveLayout(const RootKey: DWORD;
  BaseRegistryKey, LayoutName: string);
var
  ExtraL: TStringList;
begin
  if LayoutName = '' then Exit;
  ExtraL := TStringList.Create;
  try
    DoLayoutSave(LayoutName, ExtraL);
    SpSaveLayout(Owner, RootKey, BaseRegistryKey, LayoutName, ExtraL);
    SpLoadLayoutList(RootKey, BaseRegistryKey, FLayouts);
  finally
    ExtraL.Free;
  end;
end;

procedure TSpTBXCustomizer.SaveResetState(ShortcutsList: TSpTBXMenuBarShortcuts);
var
  TempIni: TMemIniFile;
begin
  // Save the ResetState, save only the Items and Toolbar state
  if FResetState.Count = 0 then begin
    TempIni := TMemIniFile.Create('');
    try
      SaveItemOptions(Owner, ShortcutsList, FResetState);
      SpMemIniSaveStringList(TempIni, rvItemsList, FResetState);
      SpSaveLayout(Owner, TempIni, rvLastLayout, nil);
      FResetState.Clear;
      TempIni.GetStrings(FResetState);
    finally
      TempIni.Free;
    end;
  end;
end;

function TSpTBXCustomizer.DeleteLayout(OptionsList: TStrings;
  LayoutName: string): Boolean;
var
  MemIni: TMemIniFile;
  L: TStringList;
  I, P: Integer;
  S: string;
begin
  Result := False;
  MemIni := TMemIniFile.Create('');
  L := TStringList.Create;
  try
    MemIni.SetStrings(OptionsList);

    SpMemIniLoadStringList(MemIni, rvLayoutList, L);
    I := L.IndexOf(LayoutName);
    if I > -1 then begin
      Result := True;
      L.Delete(I);
      SpMemIniSaveStringList(MemIni, rvLayoutList, L);

      MemIni.ReadSections(L);
      for I := 0 to L.Count - 1 do begin
        P := Pos(' @ ', L[I]);
        if P > 0 then begin
          S := Copy(L[I], 1, P - 1);
          if SameText(LayoutName, S) then
            MemIni.EraseSection(L[I]);
        end;
      end;

      SpLoadLayoutList(MemIni, FLayouts);

      // Reload OptionsList
      OptionsList.Clear;
      MemIni.GetStrings(OptionsList);
    end;
  finally
    MemIni.Free;
    L.Free;
  end;
end;

function TSpTBXCustomizer.DeleteLayout(const Filename, LayoutName: string): Boolean;
var
  L: TStringList;
begin
  Result := False;
  L := TStringList.Create;
  try
    if FileExists(Filename) then begin
      L.LoadFromFile(Filename);
      Result := DeleteLayout(L, LayoutName);
      if Result then
        L.SaveToFile(Filename);
    end;
  finally
    L.Free;
  end;
end;

function TSpTBXCustomizer.DeleteLayout(const RootKey: DWORD; BaseRegistryKey,
  LayoutName: string): Boolean;
var
  Reg: TRegistry;
  L: TStringList;
  S: string;
  I: integer;
begin
  Result := False;
  L := TStringList.Create;
  try
    S := IncludeTrailingRegKeyDelimiter(BaseRegistryKey) + rvLayoutList;
    SpRegLoadStringList(L, RootKey, S);
    I := L.IndexOf(LayoutName);
    if I > -1 then begin
      Result := True;
      L.Delete(I);
      SpRegSaveStringList(L, RootKey, S);

      Reg := TRegistry.Create;
      try
        Reg.RootKey := RootKey;
        S := IncludeTrailingRegKeyDelimiter(BaseRegistryKey) + rvLayoutRegList + '\' + LayoutName;
        Reg.DeleteKey(S);
      finally
        Reg.Free;
      end;

      SpLoadLayoutList(RootKey, BaseRegistryKey, FLayouts);
    end;
  finally
    L.Free;
  end;
end;

function TSpTBXCustomizer.DoGetShortcutsList(AItem: TTBCustomItem): Boolean;
begin
  if AItem.Parent is TSpTBXSkinGroupItem then
    Result := False
  else
    Result := True;
  if Assigned(FOnGetShortcutsList) then FOnGetShortcutsList(Self, AItem, Result);
end;

procedure TSpTBXCustomizer.DoIconOptionsChange(Toolbar: TTBCustomToolbar;
  UseSmallImages: Boolean);
begin
  if Assigned(FOnIconOptionsChange) then FOnIconOptionsChange(Self, Toolbar, UseSmallImages);
end;

procedure TSpTBXCustomizer.DoLoad(ExtraOptions: TStringList);
begin
  if Assigned(FOnLoad) then FOnLoad(Self, ExtraOptions);
end;

procedure TSpTBXCustomizer.DoSave(ExtraOptions: TStringList);
begin
  if Assigned(FOnSave) then FOnSave(Self, ExtraOptions);
end;

procedure TSpTBXCustomizer.DoLayoutLoad(LayoutName: string;
  ExtraOptions: TStringList);
begin
  if Assigned(FOnLayoutLoad) then FOnLayoutLoad(Self, LayoutName, ExtraOptions);
end;

procedure TSpTBXCustomizer.DoLayoutSave(LayoutName: string;
  ExtraOptions: TStringList);
begin
  if Assigned(FOnLayoutSave) then FOnLayoutSave(Self, LayoutName, ExtraOptions);
end;

procedure TSpTBXCustomizer.DoSkinChange;
begin
  if FShowing and Assigned(FCustomizeForm) then
    FCustomizeForm.DoSkinChange;
  if Assigned(FOnSkinChange) then FOnSkinChange(Self);
end;

procedure TSpTBXCustomizer.WMSpSkinChange(var Message: TMessage);
begin
  DoSkinChange;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomCustomizeForm }

constructor TSpTBXCustomCustomizeForm.Create(AOwner: TSpTBXCustomizer; EmbeddedParent: TWinControl);
begin
  FCustomizer := AOwner;
  FEmbedded := Assigned(EmbeddedParent);

  inherited Create(AOwner);

  FToolbarList := TTntStringList.Create;
  FItemList := TTntStringList.Create;
  FShortcutList := TTntStringList.Create;
  FSeparatorList := TTntStringList.Create;
  FBlankSeparatorList := TTntStringList.Create;

  // Hook to the Application's message loop to disable ShortCut messages processing.
  // Otherwise the TApplication.MainForm Actions are executed.
  if Assigned(FShortcutsProcessor) then
    FShortcutsProcessor.Active := True;

  if Assigned(EmbeddedParent) then begin
    Parent := EmbeddedParent;
    BorderStyle := bsNone;
    Align := alClient;
  end;
end;

destructor TSpTBXCustomCustomizeForm.Destroy;
begin
  Customizer.Close;
  if Assigned(FShortcutsProcessor) then
    FShortcutsProcessor.Active := False;
  FreeAndNil(FToolbarList);
  FreeAndNil(FItemList);
  FreeAndNil(FShortcutList);
  FreeAndNil(FSeparatorList);
  FreeAndNil(FBlankSeparatorList);
  inherited;
end;

procedure TSpTBXCustomCustomizeForm.FillCommands;
var
  I: Integer;
  W: TWinControl;
  TB: TSpTBXToolbar;
  Item: TTBCustomItem;
  WS: WideString;
  UseBlankSeparators: Boolean;
begin
  // Get the main form
  W := Customizer.Owner as TWinControl;

  FToolbarList.Clear;
  FSeparatorList.Clear;
  FBlankSeparatorList.Clear;
  FItemList.Clear;

  // Fill the Toolbars
  for I := 0 to W.ComponentCount - 1 do
    if W.Components[I] is TSpTBXToolbar then begin
      TB := W.Components[I] as TSpTBXToolbar;
      if TB.Customizable then
        FToolbarList.AddObject(TB.Caption, TB);
    end;

  // Add the Separator item
  FItemList.Add(rvSeparator);
  UseBlankSeparators := Customizer.BlankSeparators;
  if UseBlankSeparators then
    FItemList.Add(rvBlankSeparator);

  // Add the Customizer items
  for I := 0 to Customizer.Items.Count - 1 do begin
    Item := Customizer.Items[I];
    WS := SpCustomizerGetWideCaption(Item);
    if Item is TTBSeparatorItem then begin
      if UseBlankSeparators and TTBSeparatorItem(Item).Blank then
        FBlankSeparatorList.AddObject(WS, Item)
      else
        FSeparatorList.AddObject(WS, Item);
    end
    else
      FItemList.AddObject(WS, Item);
  end;

  // Add the Shortcuts items
  Customizer.GetShortcutList(FShortcutList);

  DoFillCommands(FToolbarList, FItemList, FShortcutList);
end;

procedure TSpTBXCustomCustomizeForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Key = VK_ESCAPE then Close;
end;

procedure TSpTBXCustomCustomizeForm.DoClose(var Action: TCloseAction);
begin
  Action := caFree;
  inherited DoClose(Action);
end;

procedure TSpTBXCustomCustomizeForm.DoShow;
begin
  if Assigned(Customizer) then
    FillCommands;
  inherited;
end;

procedure TSpTBXCustomCustomizeForm.DoIconOptionsChange(UseSmallImages: Boolean);
var
  I: integer;
  TB: TTBCustomToolbar;
begin
  if Assigned(Customizer) then
    for I := 0 to FToolbarList.Count - 1 do
      if FToolbarList.Objects[I] is TTBCustomToolbar then begin
        TB := FToolbarList.Objects[I] as TTBCustomToolbar;
        Customizer.DoIconOptionsChange(TB, UseSmallImages);
      end;
end;

procedure TSpTBXCustomCustomizeForm.DoSkinChange;
begin
  // The skin has changed
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM

initialization
  FShortcutsProcessor := TShortCutsProcessor.Create;
finalization
  FreeAndNil(FShortcutsProcessor);

end.
