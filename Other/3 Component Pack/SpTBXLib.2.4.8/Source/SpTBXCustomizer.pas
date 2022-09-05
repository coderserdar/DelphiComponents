unit SpTBXCustomizer;

{==============================================================================
Version 2.4.8

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
15 April 2013 - version 2.4.8
  - No changes.

7 February 2012 - version 2.4.7
  - Minor bug fixes.
  - Added support for Delphi XE2.
  - Added support for 64 bit Delphi compiler.

25 June 2011 - version 2.4.6
  - No changes.

12 March 2010 - version 2.4.5
  - No changes.

2 December 2009 - version 2.4.4
  - No changes.

13 September 2009 - version 2.4.3
  - Fixed incorrect Customizer shortcut processing when the
    shortcut text contains a space, thanks to Jim Kueneman
    for reporting this.

8 May 2009 - version 2.4.2
  - No changes.

15 March 2009 - version 2.4.1
  - Fixed incorrect Customizer loading and saving when Frames
    were used, thanks to Eduardo Mauro for reporting this.

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
    FOnCreateCustomizeForm: TNotifyEvent;
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
    procedure Load(IniFile: TCustomIniFile; LoadLastLayout: Boolean = True); overload;
    procedure Load(OptionsList: TStrings; LoadLastLayout: Boolean = True); overload;
    procedure Load(const Filename: string; LoadLastLayout: Boolean = True); overload;
    procedure Load(const RootKey: DWORD; BaseRegistryKey: string; LoadLastLayout: Boolean = True); overload;
    procedure Save(IniFile: TCustomIniFile; SaveLastLayout: Boolean = True); overload;
    procedure Save(OptionsList: TStrings; SaveLastLayout: Boolean = True); overload;
    procedure Save(const Filename: string; SaveLastLayout: Boolean = True); overload;
    procedure Save(const RootKey: DWORD; BaseRegistryKey: string; SaveLastLayout: Boolean = True); overload;
    procedure Reset;
    procedure LoadLayout(IniFile: TCustomIniFile; LayoutName: string); overload;
    procedure LoadLayout(OptionsList: TStrings; LayoutName: string); overload;
    procedure LoadLayout(const Filename, LayoutName: string); overload;
    procedure LoadLayout(const RootKey: DWORD; BaseRegistryKey, LayoutName: string); overload;
    procedure SaveLayout(IniFile: TCustomIniFile; LayoutName: string); overload;
    procedure SaveLayout(OptionsList: TStrings; LayoutName: string); overload;
    procedure SaveLayout(const Filename, LayoutName: string); overload;
    procedure SaveLayout(const RootKey: DWORD; BaseRegistryKey, LayoutName: string); overload;
    function DeleteLayout(IniFile: TCustomIniFile; LayoutName: string): Boolean; overload;
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
    property OnCreateCustomizeForm: TNotifyEvent read FOnCreateCustomizeForm write FOnCreateCustomizeForm;
    property OnGetCustomizeForm: TSpTBXGetCustomizeFormClassEvent read FOnGetCustomizeForm write FOnGetCustomizeForm;
    property OnGetShortcutsList: TSpTBXAcceptItemEvent read FOnGetShortcutsList write FOnGetShortcutsList;
    property OnLoad: TSpTBXExtraOptionsEvent read FOnLoad write FOnLoad;
    property OnSave: TSpTBXExtraOptionsEvent read FOnSave write FOnSave;
    property OnLayoutLoad: TSpTBXLayoutExtraOptionsEvent read FOnLayoutLoad write FOnLayoutLoad;
    property OnLayoutSave: TSpTBXLayoutExtraOptionsEvent read FOnLayoutSave write FOnLayoutSave;
    property OnIconOptionsChange: TSpTBXIconOptionsChangeEvent read FOnIconOptionsChange write FOnIconOptionsChange;
    property OnSkinChange: TNotifyEvent read FOnSkinChange write FOnSkinChange;
  end;

{ Ini/Reg }
procedure SpIniEraseSection(IniFile: TCustomIniFile; Section: string; EraseKeysOnly: Boolean);
procedure SpIniSaveStringList(IniFile: TCustomIniFile; Section: string; L: TStringList);
procedure SpIniLoadStringList(IniFile: TCustomIniFile; Section: string; L: TStringList);
procedure SpRegSaveStringList(L: TStringList; const RootKey: DWORD; const BaseRegistryKey: string);
procedure SpRegLoadStringList(L: TStringList; const RootKey: DWORD; const BaseRegistryKey: string);

{ Layouts }
procedure SpLoadFormState(Form: TCustomForm; OptionsList: TStrings);
procedure SpSaveFormState(Form: TCustomForm; OptionsList: TStrings);
procedure SpLoadLayoutList(IniFile: TCustomIniFile; L: TStringList); overload;
procedure SpLoadLayout(const OwnerComponent: TComponent; IniFile: TCustomIniFile; LayoutName: string; ExtraOptions: TStringList = nil);
procedure SpSaveLayout(const OwnerComponent: TComponent; IniFile: TCustomIniFile; LayoutName: string; ExtraOptions: TStringList = nil);

{ Items }
procedure SpLoadItems(const OwnerComponent: TComponent; IniFile: TCustomIniFile; ShortcutsList: TSpTBXMenuBarShortcuts = nil; ExtraOptions: TStringList = nil);
procedure SpSaveItems(const OwnerComponent: TComponent; IniFile: TCustomIniFile; ShortcutsList: TSpTBXMenuBarShortcuts = nil; ExtraOptions: TStringList = nil);

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
    if L.Count <= 1 then
      Exit;
    TBName := L[0];
    ItemName := L[1];
    if (L.Count > 2) and (L[2] <> '0') then
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
      Continue
    else
      if C is TSpTBXToolbar then begin
        if TSpTBXToolbar(C).Customizable then
          RootItemsList.AddObject(C.Name, TSpTBXToolbar(C).Items);
      end
      else
        if C is TSpTBXCustomizer then
          RootItemsList.AddObject(C.Name, TSpTBXCustomizer(C).Items)
        else
          if (C is TWinControl) and not (C is TFrame) and not (C is TForm) then
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
    if ShortCut = '' then ShortCut := '0'
    else if Pos(' ', ShortCut) > 0 then ShortCut := '"' + ShortCut + '"';
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
{ Ini/Reg }

procedure SpIniEraseSection(IniFile: TCustomIniFile; Section: string; EraseKeysOnly: Boolean);
var
  I: Integer;
  Keys: TStringList;
begin
  if EraseKeysOnly then begin
    Keys := TStringList.Create;
    try
      IniFile.ReadSection(Section, Keys);
      for I := 0 to Keys.Count - 1 do
        IniFile.DeleteKey(Section, Keys[I]);
    finally
      Keys.Free;
    end;
  end
  else
    IniFile.EraseSection(Section);
end;

procedure SpIniSaveStringList(IniFile: TCustomIniFile; Section: string; L: TStringList);
var
  I: Integer;
begin
  if not Assigned(L) then Exit;
  SpIniEraseSection(IniFile, Section, True);
  if L.Count > 0 then begin
    IniFile.WriteInteger(Section, rvCount, L.Count);
    for I := 0 to L.Count - 1 do
      IniFile.WriteString(Section, IntToStr(I), L[I]);
  end;
end;

procedure SpIniLoadStringList(IniFile: TCustomIniFile; Section: string; L: TStringList);
var
  I, C: integer;
begin
  if not Assigned(L) then Exit;
  L.Clear;
  C := IniFile.ReadInteger(Section, rvCount, -1);
  for I := 0 to C - 1 do
    L.Add(IniFile.ReadString(Section, IntToStr(I), ''));
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

procedure SpLoadLayoutList(IniFile: TCustomIniFile; L: TStringList); overload;
begin
  SpIniLoadStringList(IniFile, rvLayoutList, L);
end;

procedure SpSaveLayout(const OwnerComponent: TComponent; IniFile: TCustomIniFile;
  LayoutName: string; ExtraOptions: TStringList = nil);
var
  L: TStringList;
  Prefix, RegPrevPath: string;
begin
  // Add the Layout to the LayoutList
  L := TStringList.Create;
  try
    SpIniLoadStringList(IniFile, rvLayoutList, L);
    if L.IndexOf(LayoutName) = -1 then
      L.Add(LayoutName);
    SpIniSaveStringList(IniFile, rvLayoutList, L);
  finally
    L.Free;
  end;

  Prefix := LayoutName + ' @ ';
  // Open the new key and save the previous key if it's a TRegistryIniFile
  if IniFile is TRegistryIniFile then begin
    RegPrevPath := TRegistryIniFile(IniFile).RegIniFile.CurrentPath;
    Prefix := '';
    if not TRegistryIniFile(IniFile).RegIniFile.OpenKey(RegPrevPath + '\' + rvLayoutRegList + '\' + LayoutName, True) then
      Exit;
  end;
  // Save the Layout and ExtraOptions
  SpTBIniSavePositions(OwnerComponent, IniFile, Prefix);
  if Assigned(ExtraOptions) then
    SpIniSaveStringList(IniFile, Prefix + rvExtraOptions, ExtraOptions);
  // Open the previous key if it's a TRegistryIniFile
  if IniFile is TRegistryIniFile then
    TRegistryIniFile(IniFile).RegIniFile.OpenKey(RegPrevPath, False);
end;

procedure SpLoadLayout(const OwnerComponent: TComponent; IniFile: TCustomIniFile;
  LayoutName: string; ExtraOptions: TStringList = nil);
var
  L: TStringList;
  Prefix, RegPrevPath: string;
begin
  // Check if the layout exists on the layout list
  L := TStringList.Create;
  try
    SpIniLoadStringList(IniFile, rvLayoutList, L);
    if L.IndexOf(LayoutName) < 0 then
      Exit;
  finally
    L.Free;
  end;

  Prefix := LayoutName + ' @ ';
  // Open the new key and save the previous key if it's a TRegistryIniFile
  if IniFile is TRegistryIniFile then begin
    RegPrevPath := TRegistryIniFile(IniFile).RegIniFile.CurrentPath;
    Prefix := '';
    if not TRegistryIniFile(IniFile).RegIniFile.OpenKey(RegPrevPath + '\' + rvLayoutRegList + '\' + LayoutName, False) then
      Exit;
  end;
  // Load the Layout and ExtraOptions
  SpTBIniLoadPositions(OwnerComponent, IniFile, Prefix);
  if Assigned(ExtraOptions) then
    SpIniLoadStringList(IniFile, Prefix + rvExtraOptions, ExtraOptions);
  // Open the previous key if it's a TRegistryIniFile
  if IniFile is TRegistryIniFile then
    TRegistryIniFile(IniFile).RegIniFile.OpenKey(RegPrevPath, False);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Items }

procedure SpSaveItems(const OwnerComponent: TComponent; IniFile: TCustomIniFile;
  ShortcutsList: TSpTBXMenuBarShortcuts = nil; ExtraOptions: TStringList = nil); overload;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    SaveItemOptions(OwnerComponent, ShortcutsList, L);
    SpIniSaveStringList(IniFile, rvItemsList, L);
    SpIniSaveStringList(IniFile, rvExtraOptions, ExtraOptions);
  finally
    L.Free;
  end;
end;

procedure SpLoadItems(const OwnerComponent: TComponent; IniFile: TCustomIniFile;
  ShortcutsList: TSpTBXMenuBarShortcuts = nil; ExtraOptions: TStringList = nil); overload;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    SpIniLoadStringList(IniFile, rvItemsList, L);
    ApplyItemOptions(OwnerComponent, ShortcutsList, L);
    SpIniLoadStringList(IniFile, rvExtraOptions, ExtraOptions);
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
        // Exclude the submenus, separators, groups, labels, edit items, palettes and control items
        if (ItemStyle * [tbisSubMenu, tbisSeparator, tbisEmbeddedGroup, tbisClicksTransparent] = []) and
          not (Item is TTBEditItem) and not (Item is TSpTBXEditItem) and not (Item is TSpTBXCustomToolPalette) and
          not (Item is TTBControlItem) then
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
  if FShowing then Exit;

  if not Assigned(FCustomizeForm) then
    FCustomizeForm := GetCustomizeFormClass.Create(Self, AParent);

  if Assigned(FCustomizeForm) then begin
    if Assigned(FOnCreateCustomizeForm) then FOnCreateCustomizeForm(FCustomizeForm);
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

procedure TSpTBXCustomizer.Load(IniFile: TCustomIniFile; LoadLastLayout: Boolean = True);
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
    SpLoadItems(Owner, IniFile, ShortcutsL, ExtraL);

    // Fill Extra Options
    DoLoad(ExtraL);
    if FSaveFormState then
      SpLoadFormState(Application.MainForm, ExtraL);
    SkinManager.SetSkin(ExtraL.Values[rvSkin]);

    // Load Layouts
    SpLoadLayoutList(IniFile, FLayouts);
    if LoadLastLayout then
      LoadLayout(IniFile, rvLastLayout);
  finally
    ExtraL.Free;
    ShortcutsL.Free;
  end;
end;

procedure TSpTBXCustomizer.Load(OptionsList: TStrings; LoadLastLayout: Boolean = True);
var
  MemIni: TMemIniFile;
begin
  MemIni := TMemIniFile.Create('');
  try
    MemIni.SetStrings(OptionsList); // Transfer OptionsList contents to MemIni
    Load(MemIni, LoadLastLayout);
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXCustomizer.Load(const Filename: string; LoadLastLayout: Boolean = True);
var
  MemIni: TMemIniFile;
begin
  MemIni := TMemIniFile.Create(Filename);
  try
    Load(MemIni, LoadLastLayout);
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXCustomizer.Load(const RootKey: DWORD; BaseRegistryKey: string; LoadLastLayout: Boolean = True);
var
  Reg: TRegistryIniFile;
begin
  // Use TRegistryIniFile to call Load
  Reg := TRegistryIniFile.Create('', KEY_QUERY_VALUE);
  try
    Reg.RegIniFile.RootKey := RootKey;
    if Reg.RegIniFile.OpenKey(BaseRegistryKey, False) then
      Load(Reg, LoadLastLayout);
  finally
    Reg.Free;
  end;
end;

procedure TSpTBXCustomizer.Save(IniFile: TCustomIniFile; SaveLastLayout: Boolean = True);
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
    SpSaveItems(Owner, IniFile, ShortcutsL, ExtraL);

    // Save LastLayout
    if SaveLastLayout then
      SaveLayout(IniFile, rvLastLayout)
    else
      DeleteLayout(IniFile, rvLastLayout);
  finally
    ExtraL.Free;
    ShortcutsL.Free;
  end;
end;

procedure TSpTBXCustomizer.Save(OptionsList: TStrings; SaveLastLayout: Boolean = True);
var
  MemIni: TMemIniFile;
begin
  MemIni := TMemIniFile.Create('');
  try
    MemIni.SetStrings(OptionsList); // Transfer OptionsList contents to MemIni
    Save(MemIni, SaveLastLayout);
    OptionsList.Clear;
    MemIni.GetStrings(OptionsList); // Transfer MemIni contents to OptionsList
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXCustomizer.Save(const Filename: string; SaveLastLayout: Boolean = True);
var
  MemIni: TMemIniFile;
begin
  MemIni := TMemIniFile.Create(Filename);
  try
    Save(MemIni, SaveLastLayout);
    MemIni.UpdateFile;
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXCustomizer.Save(const RootKey: DWORD; BaseRegistryKey: string; SaveLastLayout: Boolean = True);
var
  Reg: TRegistryIniFile;
begin
  // Use TRegistryIniFile to call Save
  Reg := TRegistryIniFile.Create('');
  try
    Reg.RegIniFile.RootKey := RootKey;
    Reg.RegIniFile.CreateKey(BaseRegistryKey);
    if Reg.RegIniFile.OpenKey(BaseRegistryKey, True) then
      Save(Reg, SaveLastLayout);
  finally
    Reg.Free;
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
      MemIni.SetStrings(FResetState); // Transfer FResetState contents to MemIni
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

procedure TSpTBXCustomizer.LoadLayout(IniFile: TCustomIniFile; LayoutName: string);
var
  ExtraL: TStringList;
begin
  ExtraL := TStringList.Create;
  try
    SpLoadLayout(Owner, IniFile, LayoutName, ExtraL);
    DoLayoutLoad(LayoutName, ExtraL);
  finally
    ExtraL.Free;
  end;
end;

procedure TSpTBXCustomizer.LoadLayout(OptionsList: TStrings; LayoutName: string);
var
  MemIni: TMemIniFile;
begin
  MemIni := TMemIniFile.Create('');
  try
    MemIni.SetStrings(OptionsList); // Transfer OptionsList contents to MemIni
    LoadLayout(MemIni, LayoutName);
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXCustomizer.LoadLayout(const Filename, LayoutName: string);
var
  MemIni: TMemIniFile;
begin
  MemIni := TMemIniFile.Create(Filename);
  try
    LoadLayout(MemIni, LayoutName);
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXCustomizer.LoadLayout(const RootKey: DWORD;
  BaseRegistryKey, LayoutName: string);
var
  Reg: TRegistryIniFile;
begin
  // Use TRegistryIniFile to call LoadLayout
  Reg := TRegistryIniFile.Create('', KEY_QUERY_VALUE);
  try
    Reg.RegIniFile.RootKey := RootKey;
    if Reg.RegIniFile.OpenKey(BaseRegistryKey, False) then
      LoadLayout(Reg, LayoutName);
  finally
    Reg.Free;
  end;
end;

procedure TSpTBXCustomizer.SaveLayout(IniFile: TCustomIniFile; LayoutName: string);
var
  ExtraL: TStringList;
begin
  if LayoutName = '' then Exit;
  ExtraL := TStringList.Create;
  try
    DoLayoutSave(LayoutName, ExtraL);
    SpSaveLayout(Owner, IniFile, LayoutName, ExtraL);
    SpLoadLayoutList(IniFile, FLayouts);
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
    MemIni.SetStrings(OptionsList); // Transfer OptionsList contents to MemIni
    SaveLayout(MemIni, LayoutName);

    // Reload OptionsList
    OptionsList.Clear;
    MemIni.GetStrings(OptionsList); // Transfer MemIni contents to OptionsList
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXCustomizer.SaveLayout(const Filename, LayoutName: string);
var
  MemIni: TMemIniFile;
begin
  if LayoutName = '' then Exit;
  MemIni := TMemIniFile.Create(Filename);
  try
    SaveLayout(MemIni, LayoutName);
    MemIni.UpdateFile;
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXCustomizer.SaveLayout(const RootKey: DWORD;
  BaseRegistryKey, LayoutName: string);
var
  Reg: TRegistryIniFile;
begin
  if LayoutName = '' then Exit;
  // Use TRegistryIniFile to call SaveLayout
  Reg := TRegistryIniFile.Create('');
  try
    Reg.RegIniFile.RootKey := RootKey;
    Reg.RegIniFile.CreateKey(BaseRegistryKey);
    if Reg.RegIniFile.OpenKey(BaseRegistryKey, True) then
      SaveLayout(Reg, LayoutName);
  finally
    Reg.Free;
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
      SpIniSaveStringList(TempIni, rvItemsList, FResetState);
      SpSaveLayout(Owner, TempIni, rvLastLayout, nil);
      FResetState.Clear;
      TempIni.GetStrings(FResetState); // Transfer MemIni contents to OptionsList
    finally
      TempIni.Free;
    end;
  end;
end;

function TSpTBXCustomizer.DeleteLayout(IniFile: TCustomIniFile; LayoutName: string): Boolean;
var
  L: TStringList;
  I, P: Integer;
  S: string;
begin
  Result := False;
  L := TStringList.Create;
  try
    SpIniLoadStringList(IniFile, rvLayoutList, L);
    I := L.IndexOf(LayoutName);
    if I > -1 then begin
      Result := True;
      L.Delete(I);
      SpIniSaveStringList(IniFile, rvLayoutList, L);

      if IniFile is TRegistryIniFile then
        TRegistryIniFile(IniFile).EraseSection(rvLayoutRegList + '\' + LayoutName)
      else begin
        // Delete all the Layout sections
        IniFile.ReadSections(L);
        for I := 0 to L.Count - 1 do begin
          P := Pos(' @ ', L[I]);
          if P > 0 then begin
            S := Copy(L[I], 1, P - 1);
            if SameText(LayoutName, S) then
              IniFile.EraseSection(L[I]);
          end;
        end;
      end;

      SpLoadLayoutList(IniFile, FLayouts);
    end;
  finally
    L.Free;
  end;
end;

function TSpTBXCustomizer.DeleteLayout(OptionsList: TStrings; LayoutName: string): Boolean;
var
  MemIni: TMemIniFile;
begin
  Result := False;
  MemIni := TMemIniFile.Create('');
  try
    MemIni.SetStrings(OptionsList); // Transfer OptionsList contents to MemIni
    if DeleteLayout(MemIni, LayoutName) then begin
      Result := True;
      // Reload OptionsList
      OptionsList.Clear;
      MemIni.GetStrings(OptionsList); // Transfer MemIni contents to OptionsList
    end;
  finally
    MemIni.Free;
  end;
end;

function TSpTBXCustomizer.DeleteLayout(const Filename, LayoutName: string): Boolean;
var
  MemIni: TMemIniFile;
begin
  Result := False;
  MemIni := TMemIniFile.Create(Filename);
  try
    if DeleteLayout(MemIni, LayoutName) then begin
      Result := True;
      MemIni.UpdateFile;
    end;
  finally
    MemIni.Free;
  end;
end;

function TSpTBXCustomizer.DeleteLayout(const RootKey: DWORD; BaseRegistryKey,
  LayoutName: string): Boolean;
var
  Reg: TRegistryIniFile;
begin
  // Use TRegistryIniFile to call DeleteLayout
  Result := False;
  Reg := TRegistryIniFile.Create('');
  try
    Reg.RegIniFile.RootKey := RootKey;
    Reg.RegIniFile.CreateKey(BaseRegistryKey);
    if Reg.RegIniFile.OpenKey(BaseRegistryKey, True) then
      Result := DeleteLayout(Reg, LayoutName);
  finally
    Reg.Free;
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
