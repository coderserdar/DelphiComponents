{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: DataEditor.pas,v 1.161 2003/04/02 11:39:05 mvj Exp $}

{----------------------------------------------------------------------------
  DataEditor         Show and edit a RowStorage in a PolyGrid.

  What:              TDataEditor (with descendants)

  Company:           Polycon Ab
  Authors:           MVJ
----------------------------------------------------------------------------}

unit DataEditor;

interface

{$i common.inc}

uses
  CalcField, Interruptable, Classes,
  DBUGrid, DBUCellTypes,
  Storages, Criteria, DataElements, DataTypes, DataEditorDefinition, DataEditorLib, GridEditor,

  CommonLib, CommonCalcFields, StandardView, EditorInterfaces
{$ifndef LINUX}
  ,Controls, ComCtrls, ExtCtrls, StdCtrls, Menus
{$else}
  ,QControls, QComCtrls, QExtCtrls, QStdCtrls, QMenus
{$endif LINUX}
;

type
  TOnSavingEvent = procedure ( Sender: TObject; var Continue : Boolean ) of object;

  TDataEditorThread = class(TThread)
  private
    FThreadMethod : TThreadMethod;
  public
    constructor Create( ThreadMethod : TThreadMethod );
    procedure Execute; override;
  end;

  TAbstractDataEditorSettingsSaver = class;

  TDataEditor = class(TGarbageOwner, IMaster, ISubTotalHandlerInterface, IRowStorageHandlerInterface)
  private
    FParent : TWinControl;
    FPageControl : TPageControl;
    FPanel : TPanel;
    FPanelTotal : TPanel;
    FPanelTotalDescription : TPanel;
    FLabelTotalDescription : TLabel;

    FSettingsSaver : TAbstractDataEditorSettingsSaver;

    FDefinition : TDataEditorDefinition;
    FRowStorage : TCustomRowStorage;

    FStandardView : TStandardView;

    FGridEditorList : TList;
    FVisibleGridEditorList : TList;
    FActiveViewer : TGridEditor;
    FTotalViewer : TTotalViewer;

    FHiddenKeys : TFieldList;
    
    FDecimalCount : Integer;
    FEditorDecimalCount : Integer;
    FCurrencyDecimalCount : Integer;
    FDivisor : Double;
    FShowStripes : Boolean;
    FCreateTotals : Boolean;
    FShowTotals : Boolean;
    FShowSubTotals : Boolean;
    FShowKeys : Boolean;
    FSortByFieldOnExec : Boolean;
    FInterruptable : TInterruptable;

    FDataEditorThread : TDataEditorThread;
    FLoadAndSaveSettings : Boolean;

    FKill : Boolean;
    FKeyFormatting : TDisplayValues;
    FOnSave : TNotifyEvent;
    FOnSaving : TOnSavingEvent;
    FOnLoadSettings  : TNotifyEvent;
    FOnSaveSettings : TNotifyEvent;

// plain puckoness of delphi, need to define this crap for interfaces
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function CanEditValue(ARow : TAbstractRow; ADataField : TDataField; var ReadOnlyReason : String) : Boolean;
    function GetSettingsSaver : TAbstractDataEditorSettingsSaver;
    procedure SetSettingsSaver(const Value : TAbstractDataEditorSettingsSaver);

    procedure SetHourGlass;

    procedure UpdateVisibility;
    procedure SetSetFocusOnExec( Value : Boolean );


    property DataEditorThread : TDataEditorThread read FDataEditorThread;
    procedure _DoExecute;
    function GetMasterEditor: IMaster;
  protected
    FOwnedObjects : TList;

    procedure DoExecute; virtual;

    function CreateKeyOrderStorage(DataTable : TDataTable; OpenCriteria : TCondition;
                                   SortOrder : TRowSortOrder; ExistsInDb : Boolean) : TRowStorage; virtual;
    function CreateDefaultStorage(DataTable : TDataTable; OpenCriteria : TCondition; ExistsInDb : Boolean) : TRowStorage; virtual;
    function CreateStorage : TRowStorage;
    function DoCreateStorage : TRowStorage; virtual;
    function GetGridEditorStorage( index : Integer; APageView : TPageView ) : TCustomRowStorage; virtual;
    function CreateGridEditor(aParent : TWinControl; index : Integer) : TGridEditor; virtual;
    function CreateInterruptable : TInterruptable;
    function DoCreateGridEditor(aParent : TWinControl; index : Integer) : TGridEditor; virtual;

    function DoCreateStandardView(ADefaultStandardView : TSingletonStandardView; KeyCriteria : TCondition;
                aKeyList, HideKeyList : TFieldList; ViewCommonKeys : Boolean) : TStandardView; virtual;
    function CreateStandardView(ViewCommonKeys : Boolean) : TStandardView; virtual;
    function CreateDefinition : TDataEditorDefinition; virtual;
    function CreateEditorList(aParent : TWinControl; EditorCount : Integer; OpenCriteria : TCondition) : TList;
    function CreateSettingsSaver : TAbstractDataEditorSettingsSaver; virtual;
    function AcceptUnacceptedRows : Boolean; virtual;
    function GetDefinition : TDataEditorDefinition;
    function GetGridEditorCount : Integer;
    function GetGridEditor(idx : Integer) : TGridEditor;

    function ContainsChanges : Boolean; virtual;
    function CheckStoragesDiffer(BaseStorage, CompareStorage : TCustomRowStorage) : Boolean;
    function CheckDataRowsDiffer(BaseRow, CompareRow : TAbstractRow) : Boolean;

    function GetStandardView : TStandardView;
    procedure SetStandardView(aStandardView : TStandardView);
    function GetDefaultStandardView : TSingletonStandardView;
    function GetActiveGridEditor : TGridEditor;
    procedure SetActiveGridEditor(Editor : TGridEditor); virtual;
    function GetReadOnly : Boolean;
    procedure SetReadOnly(Value : Boolean);
    function GetActiveDataRow : TAbstractRow; virtual;
    procedure GetInvisibleFields(var AList : TFieldList);
    function GetViewCommonKeys : Boolean;
    procedure SetViewCommonKeys(Value : Boolean);
    function GetRowStorage : TCustomRowStorage;
    procedure SetRowStorage(const Value  : TCustomRowStorage);

    function GetOnDefineCell : TOnDefineCellEvent;
    procedure SetOnDefineCell(Event : TOnDefineCellEvent);
    procedure SetOnDeleteRow(Event : TOnDeleteRowEvent);
    function GetOnCellComboStringsFilling : TOnCellComboStringsFillingEvent;
    procedure SetOnCellComboStringsFilling(Event : TOnCellComboStringsFillingEvent);
    procedure SetOnDataRowChange(Event : TNotifyEvent);
    procedure SetOnChangeValue(Event : TOnChangeValueEvent);
    procedure SetOnChangingValue(Event : TOnChangingValueEvent);
    procedure SetOnMarkRowColClickEvent(Event : TOnMarkRowColClickEvent);
    procedure SetOnHeaderClickEvent(Event : TOnHeaderClickEvent);
    procedure SetOnClick(Event : TNotifyEvent);
    procedure SetOnDblClick(Event : TNotifyEvent);
    procedure SetOnGetKeyFormatting(Event : TOnGetKeyFormatting);
    procedure SetOnGridKeyDown(Event : TOnGridKeyDownEvent);

    procedure Initialize;
    procedure AfterInitialize; virtual;
    procedure InitializeTotal;
    procedure InitEditors;
    procedure GetCaptionForKeyField(var CaptionString : String; KeyField : TDataField); virtual;
    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;
    procedure LoadShownSubtotalLevels; virtual;
    procedure SaveShownSubtotalLevels; virtual;
    procedure FillSortOrder(SortOrder : TRowSortOrder);
    procedure UpdateSortOrder;
    procedure RemoveHideFields(AStandardView : TStandardView; HideFields : TFieldList);
    procedure InterruptableInterrupt(Sender : TInterruptable; Operation : TOperationType);

    procedure SetDivisor(Value : Double);
    procedure SetDecimalCount(Value : Integer);
    procedure SetEditorDecimalCount(Value : Integer);
    procedure SetCurrencyDecimalCount(Value : Integer);
    procedure SetKeyFormatting(Value : TDisplayValues);
    procedure SetShowTotals(Value : Boolean); virtual;
    procedure SetShowSubTotals(Value : Boolean); virtual;
    procedure SetShowStripes(Value : Boolean); virtual;
    procedure SetUpdateType(Value : TUpdateType);
    procedure SetFocusOnFirstUnacceptedRow;
    procedure SetMarkRowCalcField(AField : TCalcField);
    procedure SetShowMarkRowCalcField(Value : Boolean);
    procedure SetEnableSorting(Value : Boolean);
    procedure SetConstantBufferEdit(Value : Boolean);
    procedure SetConstantBufferKey(AKey : TDataField);
    procedure SetEditSubtotals(Value : Boolean);
    procedure SetAutoCreateRows(Value : Boolean);
    procedure SetAutoDeleteRows(Value : Boolean);
    procedure SetAllowMultipleAutoCreatedRows(Value : Boolean);
    procedure SetShowAllBooleansAsCheckBoxes(Value : Boolean);
    procedure SetRestoreForIllegalValues(Value : Boolean);
    procedure SetAbortOnIllegalValues(const Value: Boolean);
    procedure SetOnAutoCreateRow(Event : TOnAutoCreateRowEvent);
//    procedure SetOnCellContentsChanges(Event : TOnCellContentsChangesEvent);
    procedure SetOnGridHScroll(Event : TOnScrollEvent);
    procedure SetOnGridVScroll(Event : TOnScrollEvent);
    procedure SetOnGridCheckColWidths(Event : TNotifyEvent);
    procedure SetOnGridCheckScroll(Event : TNotifyEvent);
    procedure SetOnCreateCombo(Event : TComboCellTypeCreateCombo);

    {/** Set variables right after changing tab */}
    procedure ChangeTab;
    procedure PageControlChanging(Sender: TObject;
              var AllowChange: Boolean);
    procedure PageControlChange(Sender : TObject);

    property Panel : TPanel read FPanel;
    property PanelTotal : TPanel read FPanelTotal;
    property StandardView : TStandardView read GetStandardView;
    property DefaultStandardView : TSingletonStandardView read GetDefaultStandardView;
    property Interruptable : TInterruptable read FInterruptable;
    property HiddenKeys : TFieldList read FHiddenKeys;
    property MasterEditor : IMaster read GetMasterEditor implements IMaster;
  public
    {/** Create a DataEditor from a DataEditorDefinition */}
    constructor Create(aParent : TWinControl; aDefinition : TDataEditorDefinition);
    constructor CreateWithDefinitionAndStorage(aParent : TWinControl; aDefinition : TDataEditorDefinition; aRowStorage : TCustomRowStorage);
    {/** Create a DataEditor showing RowStorage. The CriteriaList items  defines each tab, the FieldList defines which fields to show */}
    constructor CreateWithStandardView(aParent : TWinControl; aRowStorage : TCustomRowStorage;
                IsReadOnly : Boolean; aStandardView : TSingletonStandardView;
                OpenCriteria: TCondition; InternalCriteria : TCriteria);
    {/** Create a DataEditor showing RowStorage. The FieldList the fields to be shown */}
    constructor CreateDefault(aParent : TWinControl; aRowStorage : TCustomRowStorage;
                IsReadOnly : Boolean; OpenCriteria : TCondition; InternalCriteria : TCriteria);
    {/** Destructor */}
    destructor Destroy; override;
    {/** Create the StandardView with the additional parameters */}
    function Execute : Boolean;

    function TryClose : Boolean; virtual;
    {/** Get the caption for this DataEditor */}
    procedure GetCaptions(AllCaptions : TStrings);
    function GetCaption : String;


    procedure SortRows; virtual;
    procedure SetFocus;
    procedure DefaultColWidths(Value : Integer);
    procedure OptimizeColWidths;
    procedure MinimizeColWidths;
    function UseTreeKey( AField : TDataField ) : Boolean;
    procedure FillMenuSubTotalLevel(MenuItem : TMenuItem);
    procedure FillMenuHideSubTotalLevel(MenuItem : TMenuItem);
    procedure ShowSubTotalLevel(AField : TDataField);
    procedure HideSubTotalLevel(AField : TDataField);
    function SubTotalLevelVisible(AField : TDataField) : Boolean;
    procedure ShowSubTotalLevelOnly(AField : TDataField);
    procedure ShowHighestLevelOnly;
    procedure ShowDetailOnly;
    procedure SetDialogBoxFields(Fields : TDataFieldSet; OnEditButtonClick : TOnEditButtonClickEvent);
    procedure HideGridEditor( AEditor : TGridEditor );
    procedure ShowGridEditor( AEditor : TGridEditor );
    procedure ShowAllGridEditors;

    function CanHideRow : Boolean;
    procedure HideRow;

    function CanShowTotals : Boolean;
    property ShowTotals : Boolean read FShowTotals write SetShowTotals;
    property ShowSubTotals : Boolean read FShowSubTotals write SetShowSubTotals;
    property ShowStripes : Boolean read FShowStripes write SetShowStripes;
    property UpdateType : TUpdateType write SetUpdateType;
    property ViewCommonKeys : Boolean read GetViewCommonKeys write SetViewCommonKeys;
    property ShowKeys : Boolean read FShowKeys write FShowKeys;

    property Parent : TWinControl read FParent;
    property Definition : TDataEditorDefinition read GetDefinition;
    property RowStorage : TCustomRowStorage read GetRowStorage write SetRowStorage;

    property GridEditorCount : Integer read GetGridEditorCount;
    property GridEditors[idx : Integer] : TGridEditor read GetGridEditor;
    property ActiveGridEditor : TGridEditor read GetActiveGridEditor write SetActiveGridEditor;
    property ActiveDataRow : TAbstractRow read GetActiveDataRow;
    property TotalViewer : TTotalViewer read FTotalViewer;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly;
    property DecimalCount : Integer read FDecimalCount write SetDecimalCount;
    property EditorDecimalCount : Integer read FEditorDecimalCount write SetEditorDecimalCount;
    property CurrencyDecimalCount : Integer read FCurrencyDecimalCount write SetCurrencyDecimalCount;
    property Divisor : Double read FDivisor write SetDivisor;
    property KeyFormatting : TDisplayValues read FKeyFormatting write SetKeyFormatting;
//    property HideKeysValidKeys : Boolean read GetHideKeysValid write SetHideKeysValid;
    property MarkRowCalcField : TCalcField write SetMarkRowCalcField;
    property ShowMarkRowCalcField : Boolean write SetShowMarkRowCalcField;
    property EnableSorting : Boolean write SetEnableSorting;
    property SortByFieldOnExec : Boolean read FSortByFieldOnExec write FSortByFieldOnExec;
    property SetFocusOnExec : Boolean write SetSetFocusOnExec;
    property ConstantBufferEdit : Boolean write SetConstantBufferEdit;
    property ConstantBufferKey : TDataField write SetConstantBufferKey;
    property EditSubtotals : Boolean write SetEditSubtotals;
    property AutoCreateRows : Boolean write SetAutoCreateRows;
    property AutoDeleteRows : Boolean write SetAutoDeleteRows;
    property AllowMultipleAutoCreatedRows : Boolean write SetAllowMultipleAutoCreatedRows;

    property CreateTotals : Boolean read FCreateTotals write FCreateTotals;
    property SettingsSaver : TAbstractDataEditorSettingsSaver read GetSettingsSaver write SetSettingsSaver;
    property LoadAndSaveSettings : Boolean read FLoadAndSaveSettings write FLoadAndSaveSettings;
    property ShowAllBooleansAsCheckBoxes : Boolean write SetShowAllBooleansAsCheckBoxes;
    property RestoreForIllegalValues : Boolean write SetRestoreForIllegalValues;
    property AbortOnIllegalValues : Boolean write SetAbortOnIllegalValues;

    property OnDefineCell : TOnDefineCellEvent read GetOnDefineCell write SetOnDefineCell;
    property OnDeleteRow : TOnDeleteRowEvent write SetOnDeleteRow;
    property OnCellComboStringsFilling : TOnCellComboStringsFillingEvent read GetOnCellComboStringsFilling write SetOnCellComboStringsFilling;
    property OnDataRowChange : TNotifyEvent  write SetOnDataRowChange;
    property OnChangeValue : TOnChangeValueEvent write SetOnChangeValue;
    property OnChangingValue : TOnChangingValueEvent write SetOnChangingValue;
    property OnMarkRowColClickEvent : TOnMarkRowColClickEvent write SetOnMarkRowColClickEvent;
    property OnHeaderClickEvent : TOnHeaderClickEvent write SetOnHeaderClickEvent;
    property OnSave : TNotifyEvent read FOnSave write FOnSave;
    property OnSaving : TOnSavingEvent read FOnSaving write FOnSaving;
    property OnClick : TNotifyEvent write SetOnClick;
    property OnDblClick : TNotifyEvent write SetOnDblClick;
    property OnGetKeyFormatting : TOnGetKeyFormatting write SetOnGetKeyFormatting;
    property OnLoadSettings : TNotifyEvent read FOnLoadSettings write FOnLoadSettings;
    property OnSaveSettings : TNotifyEvent read FOnSaveSettings write FOnSaveSettings;
    property OnGridKeyDown : TOnGridKeyDownEvent write SetOnGridKeyDown;
    property OnAutoCreateRow : TOnAutoCreateRowEvent write SetOnAutoCreateRow;
//    property OnCellContentsChanges : TOnCellContentsChangesEvent write SetOnCellContentsChanges;
    property OnGridHScroll : TOnScrollEvent write SetOnGridHScroll;
    property OnGridVScroll : TOnScrollEvent write SetOnGridVScroll;
    property OnGridCheckColWidths : TNotifyEvent write SetOnGridCheckColWidths;
    property OnGridCheckScroll : TNotifyEvent write SetOnGridCheckScroll;
    property OnCreateCombo : TComboCellTypeCreateCombo write SetOnCreateCombo;

    class function FlushActiveControl(AbortOnFailiure : Boolean = True) : Boolean;
  end;

  TEditorMenuItem = class(TMenuItem)
  private
    FDataField : TDataField;
    FDataRow : TDataRow;
  public
    constructor CreateWithDataField(AOwner: TComponent; ADataField : TDataField);
    constructor CreateWithDataRow(AOwner: TComponent; ADataRow : TDataRow);

    property DataField : TDataField read FDataField write FDataField;
    property DataRow : TDataRow read FDataRow write FDataRow;
  end;

  TAbstractDataEditorSettingsSaver = class
  private
    FDataEditor : TDataEditor;
    FLoadProfile : String;
    FSaveProfile : String;
    FUseDefault : Boolean;
    FSubTotalLevels : TFieldList;

    procedure LoadGridEditorColWidths(AGridEditor: TGridEditor; AGridIndex : Integer);
    procedure SaveGridEditorColWidths(aGridEditor: TGridEditor; AGridIndex : Integer);
  protected
    procedure DoLoad; virtual;
    procedure DoLoadDefault; virtual;
    procedure DoSave; virtual;
    procedure DoSaveAsDefault; virtual;
    procedure DoSaveColWidths; virtual;
    procedure DoLoadColWidths; virtual;
    procedure ShowSubTotalLevelsInEditor( AList : TFieldList ); virtual; abstract;
    procedure FillVisibleSubTotalLevelsFromEditor( AList : TFieldList ); virtual; abstract;

    function GetSubTotals : Boolean; virtual; abstract;
    function GetTotals : Boolean; virtual; abstract;
    function GetColumnWidths(AField : TDataField; AGridIndex : Integer ) : Integer; virtual; abstract;
    function GetDecimalCount : Integer; virtual; abstract;
    function GetEditorDecimalCount : Integer; virtual; abstract;
    function GetCurrencyDecimalCount : Integer; virtual; abstract;
    function GetDivisor : Extended; virtual; abstract;
    function GetKeyFormatting : TDisplayValues; virtual; abstract;
    function GetStripes : Boolean; virtual; abstract;
    procedure GetSubtotalLevels( AList : TFieldList ); virtual; abstract;

    procedure SetSubTotals( Value : Boolean); virtual; abstract;
    procedure SetTotals( Value : Boolean); virtual; abstract;
    procedure SetColumnWidths( AField : TDataField; AGridIndex, Value : Integer); virtual; abstract;
    procedure SetDecimalCount( Value : Integer); virtual; abstract;
    procedure SetEditorDecimalCount( Value : Integer ); virtual; abstract;
    procedure SetCurrencyDecimalCount( Value : Integer); virtual; abstract;
    procedure SetDivisor( Value : Extended); virtual; abstract;
    procedure SetKeyFormatting( Value : TDisplayValues); virtual; abstract;
    procedure SetStripes( Value : Boolean); virtual; abstract;
    procedure SetSubtotalLevels( AList : TFieldList ); virtual; abstract;

    property SaveProfile : String read FSaveProfile write FSaveProfile;
    property UseDefault : Boolean read FUseDefault write FUseDefault;
    property SubTotals : Boolean read GetSubTotals write SetSubTotals;
    property Totals : Boolean read GetTotals write SetTotals;
    property DecimalCount : Integer read GetDecimalCount write SetDecimalCount;
    property EditorDecimalCount : Integer read GetEditorDecimalCount write SetEditorDecimalCount;
    property CurrencyDecimalCount : Integer read GetCurrencyDecimalCount write SetCurrencyDecimalCount;
    property Divisor : Extended read GetDivisor write SetDivisor;
    property KeyFormatting : TDisplayValues read GetKeyFormatting write SetKeyFormatting;
    property Stripes : Boolean read GetStripes write SetStripes;
    property DataEditor : TDataEditor read FDataEditor;
    property ColumnWidths[ AField : TDataField; AGridIndex : Integer ] : Integer read GetColumnWidths write SetColumnWidths;
  public
    constructor Create(ADataEditor : TDataEditor);
    destructor Destroy; override;

    procedure Load;
    procedure LoadDefault;
    procedure Save;
    procedure SaveAsDefault;
    procedure SaveColWidths;
    procedure LoadColWidths;
    property LoadProfile : String read FLoadProfile write FLoadProfile;
  end;

  TDataEditorSettingsSaver = class( TAbstractDataEditorSettingsSaver )
  protected
    procedure ShowSubTotalLevelsInEditor( AList : TFieldList ); override;
    procedure FillVisibleSubTotalLevelsFromEditor( AList : TFieldList ); override;

    function GetSubTotals : Boolean; override;
    function GetTotals : Boolean; override;
    function GetColumnWidths(AField : TDataField; AGridIndex : Integer ) : Integer; override;
    function GetDecimalCount : Integer; override;
    function GetEditorDecimalCount : Integer; override;
    function GetCurrencyDecimalCount : Integer; override;
    function GetDivisor : Extended; override;
    function GetKeyFormatting : TDisplayValues; override;
    function GetStripes : Boolean; override;
    procedure GetSubtotalLevels( AList : TFieldList ); override;

    procedure SetSubTotals( Value : Boolean); override;
    procedure SetTotals( Value : Boolean); override;
    procedure SetColumnWidths( AField : TDataField; AGridIndex, Value : Integer); override;
    procedure SetDecimalCount( Value : Integer); override;
    procedure SetEditorDecimalCount( Value : Integer ); override;
    procedure SetCurrencyDecimalCount( Value : Integer); override;
    procedure SetDivisor( Value : Extended); override;
    procedure SetKeyFormatting( Value : TDisplayValues); override;
    procedure SetStripes( Value : Boolean); override;
    procedure SetSubtotalLevels( AList : TFieldList ); override;

    property SubTotals : Boolean read GetSubTotals write SetSubTotals;
    property Totals : Boolean read GetTotals write SetTotals;
//    property SubtotalLevels : TFieldList read GetSubtotalLevels write SetSubtotalLevels;
    property DecimalCount : Integer read GetDecimalCount write SetDecimalCount;
    property EditorDecimalCount : Integer read GetEditorDecimalCount write SetEditorDecimalCount;
    property CurrencyDecimalCount : Integer read GetCurrencyDecimalCount write SetCurrencyDecimalCount;
    property Divisor : Extended read GetDivisor write SetDivisor;
    property KeyFormatting : TDisplayValues read GetKeyFormatting write SetKeyFormatting;
    property Stripes : Boolean read GetStripes write SetStripes;
    property DataEditor : TDataEditor read FDataEditor;
    property ColumnWidths; //[ AField : TDataField ] : Integer read GetColumnWidths write SetColumnWidths;
  end;

var
  FieldRowModified : TRowModifiedField;

implementation

uses
{$ifndef LINUX}
  Windows, Dialogs, Forms,
{$else}
  QDialogs, QForms,
{$endif LINUX}

{$ifdef CONDITIONDEBUG}
  DebugCondition, DebugRowStorage,
{$endif CONDITIONDEBUG}



  SysUtils, DataType, DataTranslations, DataEditorConstants;

const
  COUNT_EDITOR_DECIMALS = 5;

constructor TDataEditor.Create(aParent : TWinControl; aDefinition : TDataEditorDefinition);// ViewCommonKeys : Boolean);
var
  AForm : TCustomForm;
begin
  inherited Create;

  FLoadAndSaveSettings := False;

  FCreateTotals := False;
  FVisibleGridEditorList := TList.Create;

{$ifdef CONDITIONDEBUG}
  DisplayCondition( aDefinition.OpenCriteria );
{$endif CONDITIONDEBUG}



  if FOwnedObjects = nil then
    FOwnedObjects := TList.Create;

  FHiddenKeys := TFieldList.Create;

  FDefinition := aDefinition;
  FParent := aParent;

  try
    if (aParent is TCustomForm) or (aParent.Owner is TCustomForm) then
    begin
      if (aParent is TCustomForm) then
        AForm := TCustomForm(aParent)
      else
        AForm := TCustomForm(aParent.Owner);

      if (csDesigning in AForm.ComponentState)
       {$ifdef D4_OR_HIGHER}  or not (fsShowing in AForm.FormState) {$endif D4_OR_HIGHER} then
        aParent.Visible := False;
    end;
  except
  end;

  Initialize;
  FGridEditorList := CreateEditorList(Panel, DefaultStandardView.PageViewCount, Definition.OpenCriteria);
  UpdateVisibility;

  KeyFormatting := dvDefault;
  ShowTotals := False;
  ShowSubTotals := False;
  Divisor := 1.0;
  DecimalCount := DoubleType.DefaultDecimalCount;
  EditorDecimalCount := COUNT_EDITOR_DECIMALS;
  CurrencyDecimalCount := CurrencyType.DefaultDecimalCount;

  ShowKeys := True;
  ShowStripes := True;
  EnableSorting := True;
end;

constructor TDataEditor.CreateWithDefinitionAndStorage(aParent : TWinControl; aDefinition : TDataEditorDefinition; aRowStorage : TCustomRowStorage);
begin
  FRowStorage := aRowStorage;
  FRowStorage.AutoArrange := False;
  Create(aParent, aDefinition);
end;

constructor TDataEditor.CreateWithStandardView(aParent : TWinControl; aRowStorage : TCustomRowStorage;
                IsReadOnly : Boolean; aStandardView : TSingletonStandardView;
                OpenCriteria: TCondition; InternalCriteria : TCriteria);
var
  iKey : Integer;
  TempList : TList;
  Table : TDataTable;
  aDefinition : TDataEditorDefinition;
  ASet : TFieldList;
begin
  TempList := TList.Create;

  aDefinition := CreateDefinition;
  with aDefinition do
  begin
    ReadOnly := IsReadOnly;
    Table := aRowStorage.DataTable;
    DefaultStandardView := aStandardView;
    Table.KeysToList(TempList);
    for iKey := 0 to TempList.Count -1 do
    begin
      if (TObject(TempList[iKey]) is TDataField) and
         TDataField(TempList[iKey]).IsRunningNumber then
        Keys.AddField(TempList[iKey], ltRunning, True)
      else
        Keys.AddField(TempList[iKey], ltLast, False);
    end;

    ASet := TFieldList.Create;
    ASet.Duplicates := dupIgnore;
    DefaultStandardView.GetTabKeys(ASet);
    for iKey := 0 to ASet.Count -1 do
      Keys.AddField(ASet.Field[iKey], ltTab, True);
    ASet.Free;
//      KeyList.Add(TempList[iKey]);
//    if AHideKeyList <> nil then
//      HideKeyList.Assign(AHideKeyList);
  end;

  if OpenCriteria = nil then
    aDefinition.OpenCriteria := aRowStorage.CommonCriteria //.CreateCopy
  else
    aDefinition.OpenCriteria := OpenCriteria;

  if InternalCriteria <> nil then
    aDefinition.InternalCriteria := InternalCriteria;
  TempList.Free;

  FRowStorage := aRowStorage;
  FRowStorage.AutoArrange := False;

  Create(aParent, aDefinition);
end;

constructor TDataEditor.CreateDefault(aParent : TWinControl; aRowStorage : TCustomRowStorage;
            IsReadOnly: Boolean;OpenCriteria: TCondition; InternalCriteria : TCriteria);
var
  TableName, ViewName, ViewCaption : String;
  aDefaultStandardView : TSingletonStandardView;
begin
  TableName := aRowStorage.DataTable.TableName;
  ViewCaption := TableName;
  ViewName := 'IndependentStandardView' + TableName;
  aDefaultStandardView := TSingletonStandardView.CreateDefault(aRowStorage.DataTable, ViewName, ViewCaption);

  CreateWithStandardView( aParent, aRowStorage, IsReadOnly,
                          aDefaultStandardView, OpenCriteria, InternalCriteria );

  FOwnedObjects.Add(aDefaultStandardView);
end;

function TDataEditor.Execute : Boolean;
var

  AForm : TCustomForm;
begin

  SetHourGlass;

  try
    if not Assigned(RowStorage) then
    begin

        FInterruptable := nil;

      FStandardView := CreateStandardView(ViewCommonKeys);
      FDataEditorThread := TDataEditorThread.Create( _DoExecute );
//      FDataEditorThread.Priority := tpLower;



//      DataEditorThread.Synchronize(
      SetHourGlass;// );
      DataEditorThread.WaitFor;

      if Assigned( Interruptable ) then
        Result := not Interruptable.Interrupted
      else
        Result := True;
    end
    else
    begin
      FStandardView := CreateStandardView(ViewCommonKeys);
      _DoExecute;
      Result := True;
    end;
  finally
    DataEditorThread.Free;


    try
      if (Parent is TCustomForm) or (Parent.Owner is TCustomForm) then
      begin
        if (Parent is TCustomForm) then
          AForm := TCustomForm(Parent)
        else
          AForm := TCustomForm(Parent.Owner);

        if (csDesigning in AForm.ComponentState)
       {$ifdef D4_OR_HIGHER}  or not (fsShowing in AForm.FormState) {$endif D4_OR_HIGHER} then
          Parent.Visible := True;
      end;
    except
    end;

    Screen.Cursor := crDefault;
  end;
end;



procedure TDataEditor.SetHourGlass;
begin
  Screen.Cursor := crHourGlass;
end;

procedure TDataEditor._DoExecute;
begin
  DoExecute;
end;

procedure TDataEditor.DoExecute;
begin
  RemoveHideFields(FStandardView, Definition.HideFieldList);

  if not Assigned(FRowStorage) then
  begin
    FRowStorage := CreateStorage;
    FOwnedObjects.Add(FRowStorage);

    if Interruptable.Interrupted then
      Exit;

  end;

{$ifdef CONDITIONDEBUG}
 DisplayRowStorage( FRowStorage );
{$endif CONDITIONDEBUG}

  if Assigned( DataEditorThread ) then
    DataEditorThread.Synchronize( InitEditors )
  else
    InitEditors;


end;

procedure TDataEditor.InitEditors;
var
  Storage : TCustomRowStorage;
  iPage : Integer;
  HasStripes : Boolean;
  AEditor : TGridEditor;
begin
  UpdateSortOrder;

  HasStripes := ShowStripes;

  for iPage := 0 to GridEditorCount - 1 do
  begin
    if Interruptable.Interrupted then
      Exit;
    // LAA-added so that different tabs can do magic when opening a forecast.
    // In busy "Accrual from Invoicing"-tab is always re-calculated
    try
      FStandardView.PageView[iPage].BeforeGridEditorInitialize(RowStorage, Self.ReadOnly);
    except
      on E:Exception do
        TranslateMessageDlg('TDataEditor.Execute: Exception during initialization "' + E.Message + '"', mtError, [mbOK], 0);
    end;

    Storage := GetGridEditorStorage( iPage, FStandardView.PageView[iPage] );

    AEditor := GridEditors[iPage];
    AEditor.Initialize(Storage, FStandardView, FStandardView.PageView[iPage], ShowTotals, ReadOnly, HasStripes);
    if SortByFieldOnExec and (AEditor.PageView.DisplayKeyCount > 0) then
      AEditor.SortByField(AEditor.PageView.DisplayKey[0]);
    AEditor.Execute;
    AEditor.DisabledFieldList := Definition.DisabledFieldList;
    AEditor.SetSaveToAuxTableFields(Definition.SaveToAuxTableFields);
  end;

  if Interruptable.Interrupted then
    Exit;

  if CreateTotals then
    InitializeTotal;



  if Interruptable.Interrupted then
    Exit;

  LoadSettings;

  if Interruptable.Interrupted then
    Exit;

  ActiveGridEditor := GridEditors[0];
end;

destructor TDataEditor.Destroy;
var
  iGridEditor, iItem : Integer;
begin
  try
    try
      FKill := True;
      if ActiveGridEditor <> nil then
        ActiveGridEditor.Killing := True;

      for iGridEditor := GridEditorCount -1 downto 0 do
        GridEditors[iGridEditor].Free;

      FGridEditorList.Free;
      FTotalViewer.Free;
      FStandardView.Free;
      FDefinition.Free;

      FVisibleGridEditorList.Free;
      FHiddenKeys.Free;

      for iItem := FOwnedObjects.Count -1 downto 0 do
        TObject(FOwnedObjects[iItem]).Free;
      FOwnedObjects.Free;

//      FStorageToSave.Free;
      FInterruptable.Free;
      FSettingsSaver.Free;
    except
      on E: Exception do
        TranslateMessageDlg(Self.ClassName + '.Destroy: ' + TranslateMessage(E_ErrorOccuredWhileFreeing) + #13#10 + E.Message, mtError, [mbOK], 0);
    end;
    FPanelTotal.Parent := nil;
{$ifdef D4_OR_HIGHER}
    FreeAndNil(FPanelTotal);
    FPanel.Parent := nil;
    FreeAndNil(FPanel);
{$else}
    FPanelTotal.Free;
    FPanelTotal := nil;
    FPanel.Parent := nil;
    FPanel.Free;
    FPanel := nil;
{$endif D4_OR_HIGHER}

    inherited Destroy;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TDataEditor.Initialize;

  procedure InitComponent(aComp : TControl; CompParent : TWinControl; aCaption : String;
            aAlign : TAlign; iLeft, iTop, iWidth, iHeight : Integer);
  begin
    with aComp do
    begin
      Parent := CompParent;
      Align := aAlign;
      Left := iLeft;
      Top := iTop;
      Width := iWidth;
      Height := iHeight;
    end;
    if aComp is TPanel then
      TPanel(aComp).Caption := aCaption
    else if aComp is TLabel then
      TLabel(aComp).Caption := aCaption;
  end;

begin
//  FParent := aPaent;

  FPanelTotal := TPanel.Create(Parent);
  FPanelTotal.Visible := False;
  InitComponent(FPanelTotal, Parent, '', alBottom, 0, 0, 0, PanTotDefHeight);
  FPanelTotal.BevelInner := bvNone;
  FPanelTotal.BevelOuter := bvNone;
  FPanelTotal.BorderWidth := PanTotBorWidth;
{$ifndef LINUX}
  FPanelTotal.Ctl3D := False;
{$endif LINUX}

  FPanel := TPanel.Create(Parent);
  InitComponent(FPanel, Parent, '', alClient, 0, 0, 0, 0);
  FPanel.BevelInner := bvNone;
  FPanel.BevelOuter := bvNone;

  if Definition.DefaultStandardView.PageViewCount = 1 then
    FPanel.BorderWidth := PanBorWidth;

  FPanelTotalDescription := TPanel.Create(Parent);
  InitComponent(FPanelTotalDescription, FPanelTotal, '', alTop, PanTotDescLeft,
                PanTotDescTop, 0, PanTotDescHeight);
  FPanelTotalDescription.BevelInner := bvNone;
  FPanelTotalDescription.BevelOuter := bvNone;

  FLabelTotalDescription := TLabel.Create(Parent);
  InitComponent(FLabelTotalDescription, FPanelTotalDescription, 'Total:', alNone, LabTotDescLeft,
                LabTotDescTop, LabTotDescWidth, LabTotDescHeight);

  AfterInitialize;
end;

procedure TDataEditor.InitializeTotal;
var
  iGridEditor : Integer;
begin
  FTotalViewer := TTotalViewer.Create(PanelTotal);
  TotalViewer.Execute;

  for iGridEditor := 0 to GridEditorCount -1 do
    GridEditors[iGridEditor].TotalViewer := TotalViewer;
end;

procedure TDataEditor.AfterInitialize;
begin
  // Nothing
end;

function TDataEditor.CreateStorage : TRowStorage;
begin
  Result := DoCreateStorage;
end;

procedure TDataEditor.InterruptableInterrupt(Sender : TInterruptable; Operation : TOperationType);
begin

end;

function TDataEditor.DoCreateStorage : TRowStorage;
var
  SortOrder : TRowSortOrder;
  AStorage : TRowStorage;
  DataTable : TDataTable;
  OpenCriteria : TCondition;
  ExistsInDb : Boolean;
begin
  DataTable := DefaultStandardView.DataTable;
  OpenCriteria := Definition.OpenCriteria.CreateCopy;
  ExistsInDb := True;

  SortOrder := TRowSortOrder.Create;
  FillSortOrder(SortOrder);

  if SortOrder.RuleCount > 0 then
    AStorage := CreateKeyOrderStorage(DataTable, OpenCriteria, SortOrder, ExistsInDb)
  else
  begin
    AStorage := CreateDefaultStorage(DataTable, OpenCriteria, ExistsInDb);
    SortOrder.Free;
  end;

  Result := AStorage;
  Result.AutoArrange := False;

  if Result.CanHaveTotals and CreateTotals then
    Result.SubTotalsUnder := True;
end;

function TDataEditor.CreateDefaultStorage(DataTable : TDataTable; OpenCriteria : TCondition; ExistsInDb : Boolean) : TRowStorage;
begin
  Result := TRowStorage.CreateAndLoad( DataTable, OpenCriteria, Interruptable, nil);
end;

function TDataEditor.CreateKeyOrderStorage(DataTable : TDataTable; OpenCriteria : TCondition;
                                           SortOrder : TRowSortOrder; ExistsInDb : Boolean) : TRowStorage;
begin
//  Result := TRowStorage.CreateWithKeySortOrder(DataTable, OpenCriteria, Interruptable, SortOrder, true);
  Result := TRowStorage.Create(DataTable);
  Result.TreeKeys := SortOrder;
  SortOrder.Free;

end;

function TDataEditor.GetGridEditorStorage( index : Integer; APageView : TPageView ) : TCustomRowStorage;
begin
  if GridEditorCount = 1 then
    Result := RowStorage
  else

    Result := nil;

end;

function TDataEditor.CreateEditorList(aParent : TWinControl; EditorCount : Integer; OpenCriteria : TCondition) : TList;
var
  iGridEditor : Integer;
  aPage : TTabSheet;
begin
  Result := TList.Create;

  if EditorCount > 1 then
  begin
    FPageControl := TPageControl.Create(aParent);
    FPageControl.Parent := aParent;
    FPageControl.Align := alClient;
    FPageControl.OnChange := PageControlChange;
    FPageControl.OnChanging := PageControlChanging;
{$ifndef LINUX}
    FPageControl.TabPosition := tpBottom;
{$endif LINUX}

    for iGridEditor := 0 to EditorCount -1 do
    begin
      aPage := TTabSheet.Create(FPageControl);
      aPage.Parent := FPageControl;
      aPage.PageControl := FPageControl;
      Result.Add(CreateGridEditor(aPage, iGridEditor));
    end;
  end
  else
    Result.Add(CreateGridEditor(aParent, 0));
end;

function TDataEditor.CreateGridEditor(aParent : TWinControl; index : Integer) : TGridEditor;
begin
  Result := DoCreateGridEditor(aParent, index);

  Result.ConstantBufferEdit := Definition.ConstantBufferEdit;
  Result.ConstantBufferKey := Definition.ConstantBufferKey;
  Result.EditSubtotals := Definition.EditSubtotals;
end;

function TDataEditor.DoCreateGridEditor(aParent : TWinControl; index : Integer) : TGridEditor;
begin
  Result := TGridEditor.Create(aParent);
end;

function TDataEditor.DoCreateStandardView(ADefaultStandardView : TSingletonStandardView; KeyCriteria : TCondition;
  aKeyList, HideKeyList : TFieldList; ViewCommonKeys : Boolean) : TStandardView;
begin
  Result := TStandardView.CreateDynamic(DefaultStandardView, KeyCriteria,
            aKeyList, HideKeyList, ViewCommonKeys);
end;

function TDataEditor.CreateStandardView(ViewCommonKeys : Boolean) : TStandardView;
var
  aKeyList, aHideKeyList : TFieldList;
begin
  with Definition do
  begin
    if ShowKeys then
      aKeyList := Keys.GetFieldList
    else
      aKeyList := nil;

    aHideKeyList := TFieldList.Create;
    GetInvisibleFields(aHideKeyList);

    Result := DoCreateStandardView(DefaultStandardView, InternalCriteria,
              aKeyList, aHideKeyList, ViewCommonKeys);

    aHideKeyList.Free;
  end;
end;



function TDataEditor.CreateInterruptable : TInterruptable;
begin
  Result := TInterruptable.Create;
end;

function TDataEditor.CreateSettingsSaver : TAbstractDataEditorSettingsSaver;
begin
  Result := nil;
end;

procedure TDataEditor.GetInvisibleFields(var AList : TFieldList);
begin
  aList.AddFrom(Definition.HideKeyList);
  aList.AddFrom(Definition.DisabledFieldList);
end;

procedure TDataEditor.RemoveHideFields(AStandardView : TStandardView; HideFields : TFieldList);
var
  iCol, iPageView, iField : Integer;
  AList : TValueList;
begin
  AList := TValueList.Create(IntegerType);
  AList.Sorted := True;
  for iPageView := 0 to StandardView.PageViewCount -1 do
  begin
    AList.Clear;
    for iField := 0 to HideFields.Count -1 do
    begin
      iCol := StandardView.PageView[iPageView].ColOfField(HideFields[iField]);
      if iCol >= 0 then
        AList.AddVal(ValueFromInteger(iCol));
    end;
    for iField := 0 to AList.Count -1 do
      StandardView.PageView[iPageView].HideCol(AsInteger(AList.Values[iField]));
  end;

  AList.Free;
end;

function TDataEditor.CreateDefinition : TDataEditorDefinition;
begin
  Result := TDataEditorDefinition.Create;
end;

procedure TDataEditor.FillSortOrder(SortOrder : TRowSortOrder);
var
  iKey, iField, idx : Integer;
  AField : TDataField;
  AList : TList;
begin
  AList := TList.Create;
  CopyListContent(Definition.Keys.GetList, AList);

  for iKey := 0 to StandardView.HierarchyKeys.Count -1 do
  begin
    if not (StandardView.HierarchyKeys.Field[iKey] is TKeyField) then
      Continue;

    AField := StandardView.HierarchyKeys.Field[iKey];

    // LGE-tillsatt rad 30.3.2000; Fixa MVJ
    if StandardView.DataTable.IndexOfField(AField) >= StandardView.DataTable.KeyCount then
      Continue;

    if not StandardView.DataTable.TableHasField(AField) then
      Continue;

    idx := AList.IndexOf(AField);
    if idx = -1 then
    begin
      AList.Add(AField);
      idx := AList.IndexOf(AField);
    end;

    for iField := AField.AncestorCount -1 downto 0 do
      if AList.IndexOf(AField.Ancestor[iField]) = -1 then
        AList.Insert(idx, AField.Ancestor[iField]);
  end;

  for iKey := AList.Count -1 downto 0 do
    if Definition.DisabledFieldList.ContainsField(TDataField(AList[iKey])) then
      AList.Delete(iKey);
//      AList.Remove(TDataField(AList[iKey]));

  for iKey := 0 to StandardView.DataTable.KeyCount -1 do
  begin
    AField := StandardView.DataTable.Field[iKey];
    if AList.IndexOf(AField) < 0 then
    begin
      HiddenKeys.Add(AField);
      AList.Add(AField);
    end;
  end;

  SortOrder.Clear;
  for iField := 0 to AList.Count - 1 do
    SortOrder.AddRule(AList[iField], TDataField(AList[iField]).SortOrder);
  AList.Free;  
end;

procedure TDataEditor.UpdateSortOrder;
var
  TempSortOrder : TRowSortOrder;
  idx, iRule : Integer;
  AField : TDataField;
begin
  // Fixa MVJ Varför gör du detta undrar LGR 110602

  TempSortOrder := TRowSortOrder.CreateCopy(RowStorage.CustomSortOrder);
  RowStorage.CustomSortOrder.Clear;
  for idx := 0 to StandardView.DisplayKeyCount -1 do
  begin
    AField := StandardView.DisplayKey[idx];
    iRule := TempSortOrder.IndexOfField[AField];
    if iRule >= 0 then
      with TempSortOrder.Rule[iRule] do
        RowStorage.CustomSortOrder.AddRule(DataField, SortOrder);
  end;
  TempSortOrder.Free;
end;

function TDataEditor.AcceptUnacceptedRows : Boolean;
var
  iEditor : Integer;
begin
  Result := True;
  for iEditor := GridEditorCount -1 downto 0 do
    Result := Result and GridEditors[iEditor].AcceptUnacceptedRows;
  Result := Result and (RowStorage.UnacceptedRowCount = 0);
end;

function TDataEditor.GetDefinition : TDataEditorDefinition;
begin
  Result := FDefinition;
end;

function TDataEditor.GetRowStorage : TCustomRowStorage;
begin
  Result := FRowStorage;
end;

procedure TDataEditor.SetRowStorage(const Value  : TCustomRowStorage);
begin
  Assert(GridEditorCount = 1);
  FRowStorage := Value;
  GridEditors[0].Disable;
  GridEditors[0].RowStorage := Value;
  GridEditors[0].Enable;
end;

function TDataEditor.GetStandardView : TStandardView;
begin
  Result := FStandardView;
end;

procedure TDataEditor.SetStandardView(aStandardView : TStandardView);
begin
  FStandardView := aStandardView;
end;

function TDataEditor.GetDefaultStandardView : TSingletonStandardView;
begin
  Result := nil;
  if Definition <> nil then
    Result := Definition.DefaultStandardView;
end;

function TDataEditor.GetViewCommonKeys : Boolean;
begin
  Result := Definition.ViewCommonKeys;
end;

procedure TDataEditor.SetViewCommonKeys(Value : Boolean);
begin
  Definition.ViewCommonKeys := Value;
end;

function TDataEditor.GetGridEditorCount : Integer;
begin
  Result := FVisibleGridEditorList.Count;
end;

function TDataEditor.GetGridEditor(idx : Integer) : TGridEditor;
begin
  Assert(idx < GridEditorCount, Self.ClassName + '.GetGridEditor: '
       + TranslateMessage(E_IndexOutOfBounds) + ' ' + IntToStr(idx) + ' >= ' + IntToStr(GridEditorCount));
  Result := TGridEditor(FVisibleGridEditorList[idx]);
end;

procedure TDataEditor.GetCaptions(AllCaptions : TStrings);
var
  CaptionString : String;
  iCommonKey : Integer;
  aKeyField : TDataField;
begin
  with Definition do
  begin
   if StandardView.Caption<>'' then
     CaptionString := StandardView.Caption
   else
     CaptionString := Self.RowStorage.DataTable.TableName + ' ' + Self.RowStorage.DataTable.Description;

    for iCommonKey := 0 to StandardView.CommonKeyCount -1 do
    begin
      aKeyField := StandardView.CommonKey[iCommonKey];
      GetCaptionForKeyField(CaptionString, aKeyField);
    end;
  end;
  AllCaptions.Add( CaptionString );
end;

function TDataEditor.GetCaption : String;
var
  Tmp : TStrings;
  i : Integer;
begin
  Tmp := TStringList.Create;
  GetCaptions( tmp );
  if Tmp.Count > 0 then
  begin
    Result := Tmp[0];
    for i := 1 to Tmp.Count -1 do
      Result := Result + ', ' + Tmp[i];
  end
  else
    Result := '';
  Tmp.Free;
end;

procedure TDataEditor.GetCaptionForKeyField(var CaptionString : String; KeyField : TDataField);
begin
  if Definition.OpenCriteria is TCriteria then
    CaptionString := CaptionString + ' / ' + KeyField.ShortDescription + ': '
       +  TCriteria(Definition.OpenCriteria).Caption[KeyField]
  else
    CaptionString := CaptionString + ' / ' + KeyField.ShortDescription;
end;

procedure TDataEditor.LoadSettings;
begin
  if LoadAndSaveSettings and
     Assigned(SettingsSaver) then
    SettingsSaver.Load;

  if Assigned(OnLoadSettings) then
    OnLoadSettings(Self);
end;

procedure TDataEditor.SaveSettings;
begin
  if LoadAndSaveSettings and
     Assigned(SettingsSaver) then
    SettingsSaver.Save;

  if Assigned(OnSaveSettings) then
    OnSaveSettings(Self);
end;

procedure TDataEditor.LoadShownSubtotalLevels;
begin
  StandardView.ShowDefaultSubtotalLevels(Self);
end;

procedure TDataEditor.SaveShownSubtotalLevels;
begin
end;

function TDataEditor.GetActiveGridEditor : TGridEditor;
begin
  Result := FActiveViewer;
end;

procedure TDataEditor.SetActiveGridEditor(Editor : TGridEditor);
var
  iGridEditor : Integer;
begin
  if ActiveGridEditor <> nil then
    ActiveGridEditor.Disable;
  for iGridEditor := 0 to GridEditorCount -1 do
    if GridEditors[iGridEditor] <> Editor then
      GridEditors[iGridEditor].IsActive := False;
  FActiveViewer := Editor;
  ActiveGridEditor.IsActive := True;
  if CreateTotals and (TotalViewer <> nil) then
  begin
    TotalViewer.ActiveEditor := ActiveGridEditor;
    if ActiveGridEditor.RowStorage.CanHaveTotals then
      PanelTotal.Visible := ShowTotals
    else
      PanelTotal.Visible := False;
  end;
  ActiveGridEditor.Enable;
end;

function TDataEditor.GetActiveDataRow : TAbstractRow;
begin
  if ActiveGridEditor = nil then
    Result := nil
  else
    Result := ActiveGridEditor.ActiveDataRow;
end;

function TDataEditor.CanShowTotals : Boolean;
begin
  Result := (CreateTotals and ActiveGridEditor.RowStorage.CanHaveTotals);
end;

procedure TDataEditor.SetShowTotals(Value : Boolean);
var
  AEditor : TGridEditor;
begin
  FShowTotals := Value;

  AEditor := ActiveGridEditor;
  if AEditor = nil then
    AEditor := GridEditors[0];

  if CreateTotals and AEditor.RowStorage.CanHaveTotals and (TotalViewer <> nil) then
  begin
    PanelTotal.Visible := ShowTotals;
    AEditor.CheckTotalGridCorrelation;
  end
  else
    PanelTotal.Visible := False;
end;

procedure TDataEditor.SetShowSubTotals(Value : Boolean);
var
  iGridEditor : Integer;
begin
  FShowSubTotals := Value;
  if ActiveGridEditor <> nil then
    ActiveGridEditor.Disable;

  for iGridEditor := 0 to GridEditorCount -1 do
    GridEditors[iGridEditor].ShowSubTotals := Value;
  if Assigned(RowStorage) and RowStorage.CanHaveTotals then
    RowStorage.ShowSubTotals := Value;

  if ActiveGridEditor <> nil then
    ActiveGridEditor.Enable;
end;

procedure TDataEditor.SetShowStripes(Value : Boolean);
var
  iGridEditor : Integer;
begin
  FShowStripes := Value;
  for iGridEditor := 0 to GridEditorCount -1 do
    GridEditors[iGridEditor].ShowStripes := Value;
end;

procedure TDataEditor.SetUpdateType(Value : TUpdateType);
var
  iGridEditor : Integer;
begin
  for iGridEditor := 0 to GridEditorCount -1 do
    GridEditors[iGridEditor].UpdateType := Value;
end;

function TDataEditor.GetReadOnly : Boolean;
begin
  Result := Definition.ReadOnly or DefaultStandardView.ReadOnly;
end;

procedure TDataEditor.SetReadOnly(Value : Boolean);
var
  iEditor : Integer;
begin
  Definition.ReadOnly := Value;
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].ReadOnly := Value;
end;

{function TDataEditor.GetHideKeysValid : Boolean;
begin
  Result := ActiveGridEditor.HideKeysValid;
end;
}
{
procedure TDataEditor.SetHideKeysValid(Value : Boolean);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].HideKeysValid := Value;
end;
}
{procedure TDataEditor.AddHideKeys(Fields : array of TDataField);
begin
  Definition.HideKeyList.AddFromArray(Fields);
end;

procedure TDataEditor.RemoveHideKey(AField : TDataField);
begin
end;
}
function TDataEditor.GetOnDefineCell : TOnDefineCellEvent;
begin
  Result := ActiveGridEditor.OnDefineCell;
end;

procedure TDataEditor.SetOnDefineCell(Event : TOnDefineCellEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnDefineCell := Event;
end;

procedure TDataEditor.SetOnDeleteRow(Event : TOnDeleteRowEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnDeleteRow := Event;
end;

function TDataEditor.GetOnCellComboStringsFilling : TOnCellComboStringsFillingEvent;
begin
  Result := ActiveGridEditor.OnCellComboStringsFilling;
end;

procedure TDataEditor.SetOnCellComboStringsFilling(Event : TOnCellComboStringsFillingEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnCellComboStringsFilling := Event;
end;

procedure TDataEditor.SetOnDataRowChange(Event : TNotifyEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnDataRowChange := Event;
end;

procedure TDataEditor.SetOnChangeValue(Event : TOnChangeValueEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnChangeValue := Event;
end;

procedure TDataEditor.SetOnChangingValue(Event : TOnChangingValueEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnChangingValue := Event;
end;

procedure TDataEditor.SetOnMarkRowColClickEvent(Event : TOnMarkRowColClickEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnMarkRowColClickEvent := Event;
end;

procedure TDataEditor.SetOnHeaderClickEvent(Event : TOnHeaderClickEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnHeaderClickEvent := Event;
end;

procedure TDataEditor.SetOnClick(Event : TNotifyEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnClick := Event;
end;

procedure TDataEditor.SetOnDblClick(Event : TNotifyEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnDblClick := Event;
end;

procedure TDataEditor.SetOnGetKeyFormatting(Event : TOnGetKeyFormatting);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnGetKeyFormatting := Event;
end;

procedure TDataEditor.SetOnGridKeyDown(Event : TOnGridKeyDownEvent);

var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnGridKeyDown := Event;
end;

procedure TDataEditor.SetDivisor(Value : Double);
var
  iPage : Integer;
begin
  FDivisor := Value;
  for iPage := 0 to GridEditorCount -1 do
    GridEditors[iPage].CurrencyDivisor := Value;
end;

procedure TDataEditor.SetDecimalCount(Value : Integer);
var
  iPage : Integer;
begin
  FDecimalCount := Value;
  for iPage := 0 to GridEditorCount -1 do
    GridEditors[iPage].DecimalCount := Value;
end;

procedure TDataEditor.SetEditorDecimalCount(Value : Integer);
var
  iPage : Integer;
begin
  FEditorDecimalCount := Value;
  for iPage := 0 to GridEditorCount -1 do
    GridEditors[iPage].EditorDecimalCount := Value;
end;

procedure TDataEditor.SetCurrencyDecimalCount(Value : Integer);
var
  iPage : Integer;
begin
  FCurrencyDecimalCount := Value;
  for iPage := 0 to GridEditorCount -1 do
    GridEditors[iPage].CurrencyDecimalCount := Value;
end;

procedure TDataEditor.SetKeyFormatting(Value : TDisplayValues);
var
  iPage : Integer;
begin
  FKeyFormatting := Value;
  for iPage := 0 to GridEditorCount -1 do
    GridEditors[iPage].KeyFormatting := Value;
end;













procedure TDataEditor.SortRows;
begin
  with ActiveGridEditor do
  begin
    Disable;
    SortRows;
    while not Enabled do
      EnableWithCoord(0, ActiveGridEditor.PageView.HeaderRowViewCount);
  end;
end;

procedure TDataEditor.SetFocus;
begin
  if not ActiveGridEditor.Grid.HasFocus then
    ActiveGridEditor.Grid.SetFocus;
end;

procedure TDataEditor.DefaultColWidths(Value : Integer);
var
  iPage : Integer;
begin
  ActiveGridEditor.Disable;
  KeyFormatting := dvDefault;
  for iPage := 0 to GridEditorCount -1 do
    GridEditors[iPage].DefaultColWidths(Value);
  ActiveGridEditor.Enable;
end;

procedure TDataEditor.OptimizeColWidths;
var
  iPage : Integer;
begin
  ActiveGridEditor.Disable;
  KeyFormatting := dvDefault;
  for iPage := 0 to GridEditorCount -1 do
    GridEditors[iPage].OptimizeColWidths;
  ActiveGridEditor.Enable;
end;

procedure TDataEditor.MinimizeColWidths;
var
  iPage : Integer;
begin
  ActiveGridEditor.Disable;
  KeyFormatting := dvKeyOnly;
  for iPage := 0 to GridEditorCount -1 do
    GridEditors[iPage].MinimizeColWidths;
  ActiveGridEditor.Enable;
end;

function TDataEditor.UseTreeKey( AField : TDataField ) : Boolean;
begin
  Result := not (AField.IsRunningNumber or
            StandardView.PageView[0].CommonKeyList.ContainsField(AField) or
            HiddenKeys.ContainsField(AField));
end;

procedure TDataEditor.FillMenuSubTotalLevel(MenuItem : TMenuItem);

  function CreateMenuItem(ACaption : String; AField : TDataField ) : TMenuItem;
  begin
    Result := TEditorMenuItem.CreateWithDataField(MenuItem, AField);
    Result.Caption := ACaption;
  end;

var
  idx : Integer;
  AField : TDataField;
  Name : String;
begin
  MenuItem.Add(CreateMenuItem('Total', nil));;

  for idx := 0 to RowStorage.TreeKeyCount -2 do
  begin
    AField := RowStorage.TreeKey[idx].TreeKey;
    if not UseTreeKey(AField) then
      Continue;
    Name := AField.LongDescription;
    MenuItem.Add(CreateMenuItem(Name, AField));
  end;

  MenuItem.Add(CreateMenuItem('Detail', RowStorage.TreeKey[RowStorage.TreeKeyCount -1].TreeKey));
end;

procedure TDataEditor.ShowSubTotalLevel(AField : TDataField);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].ShowSubTotalLevel(AField );
end;

procedure TDataEditor.ShowSubTotalLevelOnly(AField : TDataField);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].ShowSubTotalLevelOnly( AField )
end;

procedure TDataEditor.FillMenuHideSubTotalLevel(MenuItem : TMenuItem);
var
  idx : Integer;
  aItem : TMenuItem;
begin
  for idx := 0 to StandardView.DisplayKeyList.Count -1 do
  begin
    aItem := TMenuItem.Create(MenuItem);
    aItem.Caption := StandardView.DisplayKeyList[idx].FieldName;
    aItem.Tag := idx;
    MenuItem.Add(aItem);
  end;
end;

procedure TDataEditor.HideSubTotalLevel(AField : TDataField);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].HideSubTotalLevel(AField);
end;

function TDataEditor.SubTotalLevelVisible(AField : TDataField) : Boolean;
var
  iEditor : Integer;
begin
  Result := True;
  for iEditor := 0 to GridEditorCount -1 do
    Result := Result and GridEditors[iEditor].SubTotalLevelVisible(AField);
end;

procedure TDataEditor.ShowHighestLevelOnly;
var
  i : Integer;
begin
  for i := 0 to GridEditorCount -1 do
    GridEditors[i].ShowHighestLevelOnly;
end;

procedure TDataEditor.ShowDetailOnly;
var
  i : Integer;
begin
  for i := 0 to GridEditorCount -1 do
    GridEditors[i].ShowDetailOnly;
end;

function TDataEditor.CanHideRow : Boolean;
begin
  Result := ActiveGridEditor.CanHideRow;
end;

procedure TDataEditor.HideRow;
begin
  ActiveGridEditor.HideRow;
end;

function TDataEditor.TryClose : Boolean;
var
  iGridEditor : Integer;
  GridEditorWithUnaccepted : TGridEditor;
begin
  try
    ActiveGridEditor.Killing := True;
//    ActiveGridEditor.CommitChanges;
    Result := AcceptUnacceptedRows;

    if not Result then
    begin
      TranslateShowMessage(TranslateMessage(I_StillHaveInvalidKeys));
      GridEditorWithUnaccepted := nil;
      if ActiveGridEditor.RowStorage.UnacceptedRowCount <> 0 then
        GridEditorWithUnaccepted := ActiveGridEditor
      else
        for iGridEditor := 0 to GridEditorCount -1 do
          if GridEditors[iGridEditor].RowStorage.UnacceptedRowCount <> 0 then
            GridEditorWithUnaccepted := GridEditors[iGridEditor];

      if GridEditorWithUnaccepted <> nil then
      begin
        ActiveGridEditor := GridEditorWithUnaccepted;
        SetFocusOnFirstUnacceptedRow;
      end
      else
        raise Exception.Create(Self.ClassName + '.TryClose: Could not find any grid ' +
              'containing the row with illegal key values! Report this bug!');
    end;

    SaveSettings;
  except
    Result := False;
  end;
  ActiveGridEditor.Killing := False;
end;



function TDataEditor.ContainsChanges : Boolean;
var
  iGrid : Integer;
begin
  if (RowStorage = nil) or ReadOnly then
    Result := False
  else
    Result := RowStorage.ContainsChanges;

  if not Result then
    for iGrid := 0 to GridEditorCount -1 do
    begin
      Result := not GridEditors[iGrid].ReadOnly and
               (GridEditors[iGrid].RowStorage <> nil) and
               GridEditors[iGrid].RowStorage.ContainsChanges;
      if Result then
        Break;
    end;
end;

function TDataEditor.CheckStoragesDiffer(BaseStorage, CompareStorage : TCustomRowStorage) : Boolean;
var
  iRow : Integer;
  ARow, MatchRow : TAbstractRow;
  ACriteria : TCriteria;
begin
  Result := False;

  for iRow := BaseStorage.RowCount -1 downto 0 do
  begin
    ARow := BaseStorage.Rows[iRow];
    ACriteria := TCriteria.CreateFromRowKeys(ARow);

{$ifdef D4_OR_HIGHER}
    MatchRow := RowStorage.LocateRowByCriteria(ACriteria);
{$else}
    MatchRow := RowStorage.LocateRowByCriteria(ACriteria, False);
{$endif D4_OR_HIGHER}
    ACriteria.Free;
    if CheckDataRowsDiffer(ARow, MatchRow) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TDataEditor.CheckDataRowsDiffer(BaseRow, CompareRow : TAbstractRow) : Boolean;
var
  iField : Integer;
  AField : TDataField;
begin
  Result := False;
  if CompareRow = nil then
  begin
    Result := (BaseRow = nil);
    Exit;
  end
  else
  begin
    for iField := 0 to BaseRow.DataTable.FieldCount -1 do
    begin
      AField := BaseRow.DataTable.Field[iField];
      if not AField.DataType.Equals(BaseRow[AField], CompareRow[AField]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure TDataEditor.SetFocusOnFirstUnacceptedRow;
begin
  ActiveGridEditor.ActiveDataRow := ActiveGridEditor.RowStorage.UnAcceptedRows[0];
  ActiveGridEditor.Grid.Col := ActiveGridEditor.AlwaysFixedColCount;
  if ActiveGridEditor.Grid.Showing and ActiveGridEditor.Grid.CanFocus then
    ActiveGridEditor.Grid.SetFocus;
end;

procedure TDataEditor.SetMarkRowCalcField(AField : TCalcField);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].MarkRowCalcField := AField;
end;

procedure TDataEditor.SetShowMarkRowCalcField(Value : Boolean);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].ShowMarkRowCalcField := Value;
end;

procedure TDataEditor.SetEnableSorting(Value : Boolean);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].EnableSorting := Value;
end;

procedure TDataEditor.SetConstantBufferEdit(Value : Boolean);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].ConstantBufferEdit := Value;
end;

procedure TDataEditor.SetConstantBufferKey(AKey : TDataField);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].ConstantBufferKey := AKey;
end;

procedure TDataEditor.SetEditSubtotals(Value : Boolean);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].EditSubtotals := Value;
end;

procedure TDataEditor.SetAutoCreateRows(Value : Boolean);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].AutoCreateRows := Value;
end;

procedure TDataEditor.SetAutoDeleteRows(Value : Boolean);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].AutoDeleteRows := Value;
end;

procedure TDataEditor.SetAllowMultipleAutoCreatedRows(Value : Boolean);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].AllowMultipleAutoCreatedRows := Value;
end;

procedure TDataEditor.SetShowAllBooleansAsCheckBoxes(Value : Boolean);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].ShowAllBooleansAsCheckBoxes := Value;
end;

procedure TDataEditor.SetRestoreForIllegalValues(Value : Boolean);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].RestoreForIllegalValues := Value;
end;

procedure TDataEditor.SetAbortOnIllegalValues(const Value: Boolean);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].AbortOnIllegalValues := Value;
end;

procedure TDataEditor.SetOnAutoCreateRow(Event : TOnAutoCreateRowEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnAutoCreateRowEvent:= Event;
end;

(*
procedure TDataEditor.SetOnCellContentsChanges(Event : TOnCellContentsChangesEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnCellContentsChangesEvent := Event;
end;
*)

procedure TDataEditor.SetOnGridHScroll(Event : TOnScrollEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnGridHScroll := Event;
end;

procedure TDataEditor.SetOnGridVScroll(Event : TOnScrollEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnGridVScroll:= Event;
end;

procedure TDataEditor.SetOnGridCheckColWidths(Event : TNotifyEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnGridCheckColWidths:= Event;
end;

procedure TDataEditor.SetOnGridCheckScroll(Event : TNotifyEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnGridCheckScroll:= Event;
end;

procedure TDataEditor.SetOnCreateCombo(Event : TComboCellTypeCreateCombo);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].OnCreateCombo := Event;
end;

procedure TDataEditor.SetDialogBoxFields(Fields : TDataFieldSet; OnEditButtonClick : TOnEditButtonClickEvent);
var
  iEditor : Integer;
begin
  for iEditor := 0 to GridEditorCount -1 do
    GridEditors[iEditor].SetDialogBoxFields(Fields, OnEditButtonClick);
end;

procedure TDataEditor.ChangeTab;
var
  iActive : Integer;
begin
  iActive := FPageControl.ActivePage.TabIndex;
  ActiveGridEditor := GridEditors[iActive];
  SetFocus;
end;

procedure TDataEditor.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange := ActiveGridEditor.AcceptUnacceptedRows;
  if not AllowChange then
  begin
    TranslateShowMessage(TranslateMessage(I_StillHaveInvalidKeys));
    SetFocusOnFirstUnacceptedRow;
  end;
end;

procedure TDataEditor.PageControlChange(Sender : TObject);
begin
  ChangeTab;
end;

function TDataEditor.CanEditValue(ARow : TAbstractRow; ADataField : TDataField; var ReadOnlyReason : String) : Boolean;
begin
  Result := ActiveGridEditor.CanEditValue(ARow, ADataField, ReadOnlyReason);
end;

procedure TDataEditor.HideGridEditor( AEditor : TGridEditor );

  function GetNextActive : TGridEditor;
  var
    idx : Integer;
  begin
    Result := nil;
    for idx := 0 to GridEditorCount -1 do
      if GridEditors[idx].Visible and (GridEditors[idx] <> AEditor) then
      begin
        Result := GridEditors[idx];
        Break;
      end;
    if Result = nil then
      raise Exception.Create( Self.ClassName + '.HideGridEditor: Can not hide all tabs! At least one has to be visible!' );  
  end;

begin
  if AEditor.IsActive then
    ActiveGridEditor := GetNextActive;
  AEditor.Hide;
  UpdateVisibility;
end;

procedure TDataEditor.ShowGridEditor( AEditor : TGridEditor );
begin
  AEditor.Show;
  UpdateVisibility;
end;

procedure TDataEditor.ShowAllGridEditors;
var
  i : Integer;
begin
  for i := 0 to fGridEditorList.Count -1 do
    TGridEditor(fGridEditorList[i]).Show;
  UpdateVisibility;
end;

procedure TDataEditor.UpdateVisibility;
var
  i : Integer;
begin
  FVisibleGridEditorList.Clear;
  for i := 0 to fGridEditorList.Count -1 do
    if TGridEditor(fGridEditorList[i]).Visible then
      FVisibleGridEditorList.Add( fGridEditorList[i] );
end;

procedure TDataEditor.SetSetFocusOnExec( Value : Boolean );
var
  i : Integer;
begin
  for i := 0 to GridEditorCount -1 do
    GridEditors[i].SetFocusOnExec := Value;
end;

function TDataEditor.GetSettingsSaver : TAbstractDataEditorSettingsSaver;
begin
  if FSettingsSaver = nil then
    FSettingsSaver := CreateSettingsSaver;

  Result := FSettingsSaver;
end;

procedure TDataEditor.SetSettingsSaver(const Value : TAbstractDataEditorSettingsSaver);
begin
  if FSettingsSaver <> nil then
    FSettingsSaver.Free;

  FSettingsSaver := Value;
  FLoadAndSaveSettings := Value <> nil;
end;

// plain puckoness of delphi, need to define this crap for interfaces
function TDataEditor.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TDataEditor._AddRef: Integer; stdcall;
begin
  result := 1;
end;

function TDataEditor._Release: Integer; stdcall;
begin
  result := 1;
end;

function TDataEditor.GetMasterEditor: IMaster;
begin
  if Self = nil then
    Result := nil
  else
    Result := GridEditors[0];
end;

{ TDataEditorThread }

constructor TDataEditorThread.Create( ThreadMethod : TThreadMethod );
begin
  FThreadMethod := ThreadMethod;
  inherited Create( False );
end;

procedure TDataEditorThread.Execute;
begin
  if Assigned( FThreadMethod ) then
    FThreadMethod
  else
    raise Exception.Create( Self.ClassName + ': No function supplied!' );
end;

{ TEditorMenuItem }

constructor TEditorMenuItem.CreateWithDataField(AOwner: TComponent; ADataField : TDataField);
begin
  Inherited Create(AOwner);

  FDataField := ADataField;
  FDataRow := nil;
end;

constructor TEditorMenuItem.CreateWithDataRow(AOwner: TComponent;
  ADataRow: TDataRow);
begin
  Inherited Create(AOwner);

  FDataField := nil;
  FDataRow := ADataRow;
end;

{ TAbstractDataEditorSettingsSaver }

constructor TAbstractDataEditorSettingsSaver.Create(
  ADataEditor: TDataEditor);
begin
  inherited Create;

  FUseDefault := False;
  FDataEditor := ADataEditor;
  FSubTotalLevels := TFieldList.Create;
end;

destructor TAbstractDataEditorSettingsSaver.Destroy;
begin
  FSubTotalLevels.Free;

  inherited Destroy;
end;

procedure TAbstractDataEditorSettingsSaver.DoLoad;
var
  i : Integer;
begin
  DataEditor.ShowSubTotals := SubTotals;
  DataEditor.ShowTotals := Totals;
  GetSubtotalLevels( FSubTotalLevels );
  ShowSubTotalLevelsInEditor( FSubTotalLevels );
  DataEditor.DecimalCount := DecimalCount;
  DataEditor.CurrencyDecimalCount := CurrencyDecimalCount;
  DataEditor.Divisor := Divisor;
  DataEditor.KeyFormatting := KeyFormatting;
  DataEditor.ShowStripes := Stripes;
  LoadColWidths;
  for i := 0 to DataEditor.GridEditorCount -1 do
    DataEditor.GridEditors[i].ActiveRow := 0;
end;

procedure TAbstractDataEditorSettingsSaver.DoLoadColWidths;
var
  iEditor : Integer;
begin
  for iEditor := 0 to DataEditor.FGridEditorList.Count -1 do
    LoadGridEditorColWidths( TGridEditor( DataEditor.FGridEditorList[iEditor] ), iEditor );
end;

procedure TAbstractDataEditorSettingsSaver.LoadGridEditorColWidths( AGridEditor : TGridEditor; AGridIndex : Integer );
var
  aField : TDataField;
  iCol : Integer;
begin
  for iCol := aGridEditor.AlwaysFixedColCount to aGridEditor.TotalColCount -1 do
  begin
    aField := aGridEditor.RowViewDataField[aGridEditor.PageView.RowView[0], iCol];
    aGridEditor.Grid.ColWidths[iCol] := ColumnWidths[ aField, AGridIndex ];
 end;
end;

procedure TAbstractDataEditorSettingsSaver.DoLoadDefault;
begin
  UseDefault := True;
  try
    Load;
  finally
    UseDefault := False;
  end;
end;

procedure TAbstractDataEditorSettingsSaver.DoSave;
begin
  SubTotals := DataEditor.ShowSubTotals;
  Totals := DataEditor.ShowTotals;
  FillVisibleSubTotalLevelsFromEditor( FSubTotalLevels );
  SetSubtotalLevels( FSubTotalLevels );
  DecimalCount := DataEditor.DecimalCount;
  CurrencyDecimalCount := DataEditor.CurrencyDecimalCount;
  Divisor := DataEditor.Divisor;
  KeyFormatting := DataEditor.KeyFormatting;
  Stripes := DataEditor.ShowStripes;
  SaveColWidths;
end;

procedure TAbstractDataEditorSettingsSaver.DoSaveAsDefault;
begin
  UseDefault := True;
  try
    Save;
  finally
    UseDefault := False;
  end;
end;

procedure TAbstractDataEditorSettingsSaver.DoSaveColWidths;
var
  iEditor : Integer;
begin
  for iEditor := DataEditor.FGridEditorList.Count -1 downto 0 do
    SaveGridEditorColWidths( TGridEditor( DataEditor.FGridEditorList[iEditor] ), iEditor );
end;

procedure TAbstractDataEditorSettingsSaver.SaveGridEditorColWidths(aGridEditor : TGridEditor; AGridIndex : Integer);
var
  aField : TDataField;
  iCol : Integer;
begin
  for iCol := aGridEditor.AlwaysFixedColCount to aGridEditor.TotalColCount -1 do
  begin
    aField := aGridEditor.RowViewDataField[aGridEditor.PageView.RowView[0], iCol];
    ColumnWidths[ aField, AGridIndex ] := aGridEditor.Grid.ColWidths[iCol];
 end;
end;

procedure TAbstractDataEditorSettingsSaver.Load;
begin
  DoLoad;
end;

procedure TAbstractDataEditorSettingsSaver.LoadColWidths;
begin
  DoLoadColWidths;
end;

procedure TAbstractDataEditorSettingsSaver.LoadDefault;
begin
  DoLoadDefault;
end;

procedure TAbstractDataEditorSettingsSaver.Save;
begin
  DoSave;
end;

procedure TAbstractDataEditorSettingsSaver.SaveAsDefault;
begin
  DoSaveAsDefault;
end;

procedure TAbstractDataEditorSettingsSaver.SaveColWidths;
begin
  DoSaveColWidths;
end;

{ TDataEditorSettingsSaver }

function TDataEditorSettingsSaver.GetTotals: Boolean;
begin
  Result := DataEditor.ShowTotals;
//  Result := False;
end;

function TDataEditorSettingsSaver.GetColumnWidths(
  AField: TDataField; AGridIndex : Integer ): Integer;
var
  AEditor : TGridEditor;
begin
  AEditor := TGridEditor(DataEditor.FGridEditorList[AGridIndex]);
  Result := AEditor.Grid.ColWidths[AEditor.GridIndexOfField[AField]];

//  Result := 80;
end;

function TDataEditorSettingsSaver.GetCurrencyDecimalCount: Integer;
begin
  Result := DataEditor.CurrencyDecimalCount
//  Result := 2;
end;

function TDataEditorSettingsSaver.GetDecimalCount: Integer;
begin
  Result := DataEditor.DecimalCount;
//  Result := DoubleType.DefaultDecimalCount;
end;

function TDataEditorSettingsSaver.GetEditorDecimalCount: Integer;
begin
  Result := DataEditor.EditorDecimalCount;
//  Result := DecimalCount;
end;

function TDataEditorSettingsSaver.GetDivisor: Extended;
begin
  Result := DataEditor.Divisor;
//  Result := 1.0;
end;

function TDataEditorSettingsSaver.GetKeyFormatting: TDisplayValues;
begin
  Result := DataEditor.KeyFormatting;
//  Result := dvDefault;
end;

function TDataEditorSettingsSaver.GetStripes: Boolean;
begin
  Result := DataEditor.ShowStripes
//  Result := True;
end;

function TDataEditorSettingsSaver.GetSubTotals: Boolean;
begin
  Result := DataEditor.ShowSubTotals;
//  Result := False;
end;

procedure TDataEditorSettingsSaver.SetTotals(Value: Boolean);
begin
  // nothing
end;

procedure TDataEditorSettingsSaver.SetColumnWidths(AField: TDataField;
  AGridIndex, Value: Integer);
begin
  // nothing
end;

procedure TDataEditorSettingsSaver.SetCurrencyDecimalCount(Value: Integer);
begin
  // nothing
end;

procedure TDataEditorSettingsSaver.SetDecimalCount(Value: Integer);
begin
  // nothing
end;

procedure TDataEditorSettingsSaver.SetDivisor(Value: Extended);
begin
  // nothing
end;

procedure TDataEditorSettingsSaver.SetKeyFormatting(Value: TDisplayValues);
begin
  // nothing
end;

procedure TDataEditorSettingsSaver.SetStripes(Value: Boolean);
begin
  // nothing
end;

procedure TDataEditorSettingsSaver.SetSubTotals(Value: Boolean);
begin
  // nothing
end;

procedure TDataEditorSettingsSaver.SetEditorDecimalCount(Value: Integer);
begin
  // nothing
end;

procedure TDataEditorSettingsSaver.GetSubtotalLevels(AList: TFieldList);
begin
  DataEditor.StandardView.ShowDefaultSubtotalLevels(DataEditor);
end;

procedure TDataEditorSettingsSaver.SetSubtotalLevels(AList: TFieldList);
begin
  // nothing
end;

procedure TDataEditorSettingsSaver.FillVisibleSubTotalLevelsFromEditor(
  AList: TFieldList);
var
  idx : Integer;
  AField : TDataField;
begin
  AList.Clear;
  if DataEditor.SubTotalLevelVisible( nil ) then
    AList.Add( nil );

  for idx := 0 to DataEditor.RowStorage.TreeKeyCount -1 do
  begin
    AField := DataEditor.RowStorage.TreeKey[idx].TreeKey;
    if not DataEditor.UseTreeKey(AField) then
      Continue;
    if DataEditor.SubTotalLevelVisible( AField ) then
      AList.Add( AField );
  end;
end;

procedure TDataEditorSettingsSaver.ShowSubTotalLevelsInEditor(
  AList: TFieldList);
var
  idx : Integer;
begin
  if AList.Count > 0 then
    for idx := 0 to AList.Count -1 do
    begin
      if idx = 0 then
        DataEditor.ShowSubTotalLevelOnly( AList.Field[idx] )
      else
        DataEditor.ShowSubTotalLevel( AList.Field[idx] );
    end
  else
    DataEditor.ShowDetailOnly;
end;

class function TDataEditor.FlushActiveControl(
  AbortOnFailiure: Boolean = True): Boolean;
begin
  Result := TDBUGrid.FlushActiveControl(AbortOnFailiure);
end;

initialization

  FieldRowModified := TRowModifiedField.CreateOld('');

finalization

  FieldRowModified.Free;

end.

