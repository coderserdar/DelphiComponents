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

{ $Id: StandardView.pas,v 1.79 2003/03/24 12:57:40 mvj Exp $ }

{-------------------------------------------------------------------------
  StandardView     Metadata defining the stucture of a forecast

  What             TAbstractStandardView
                   TAbstractPageView
                   TAbstractRowView

                   TCustomRowView

                   TSingletonStandardView
                   TSingletonPageView
                   TSingletonRowView

                   TStandardView
                   TPageView
                   TRowView

                   THeaderRowView

  Company          Polycon
  Authors          MVJ
-------------------------------------------------------------------------}

unit StandardView;

interface

{$i common.inc}

uses
  Classes,
  DataElements, DataType, Storages, Criteria, DataEditorLib, CommonLib,
  CalcField, CommonCalcFields, GridEditorProperties, EditorInterfaces,
  DBUTypes, IndexContainer;

type
  TAbstractStandardView = class;
  TAbstractPageView = class;
  TAbstractRowView = class;

  TCustomRowView = class;

  TSingletonPageView = class;
  TSingletonRowView = class;

  TStandardView = class;
  TPageView = class;
  TRowView = class;
  THeaderRowView = class;

  {/** Describes how a TRowStorage is to be displayed for editing on screen */}
  TAbstractStandardView = class
  private
    FPageViewList : TList;
    FDataTable : TDataTable;
//    FTableForLoad : TDataTable;
    FCaption : String;
    FViewName : String;
    FRowIsValidField : TClosedField;
    FOwnedObjects : TList;

    function GetPageViewCount: Integer;
  protected
    function GetAbstractPageView(iPageView: integer) : TAbstractPageView;
    function GetRowIsValidField : TClosedField; virtual;
    procedure SetRowIsValidField(AField : TClosedField); virtual;
//    function GetLookupBaseFieldSet : TFieldList; virtual; abstract;
    function GetSelectionFields : TFieldList; virtual; abstract;
    function GetHierarchyKeys : TFieldList; virtual; abstract;
    function GetEditState : TEditState; virtual; abstract;
    function GetMarmalade : Boolean; virtual; abstract;
    function GetMarmaladeKeys : TFieldList; virtual; abstract;
    function GetDefaultValueList : TValueList; virtual; abstract;
    function GetReadOnlyList : TValueList; virtual; abstract;
    function GetComboValuesList : TValueList; virtual; abstract;
    function GetDataRowReadOnlyList : TValueList; virtual; abstract;
    function GetDefaultSortFields : TFieldList; virtual; abstract;
    function GetReadOnly: Boolean; virtual; abstract;
    function GetAllowOnlyLegalComboValues: Boolean; virtual; abstract;
  public
    {/** Create singleton read-only standard view used as copy base for dynamic view */ }
    constructor Create(DataTable : TDataTable; ViewName, Caption : string);
    {/** Destructor */}
    destructor Destroy; override;

    {/** Create a independent editable PageView without singelton */}
//    function CreateIndependentPageView(PageViewDisplayKeyList, PageViewCommonKeyList : TFieldList) : TPageView; virtual;
    {/** Table to be viewed */}
    property DataTable : TDataTable read FDataTable;
    {/** Table to build RowStorageFrom viewed */}
//    property TableForLoad : TDataTable read FTableForLoad write FTableForLoad;
    {/** Description used at least in combos */}
    property Caption : string read FCaption write FCaption;
    {/** This StandardViews unique name */}
    property ViewName : string read FViewName;
    {/** Number of active page views */}
    property PageViewCount : integer read GetPageViewCount;
    {/** Pointer to list of TPageView objects */}
    property AbstractPageView[iPageView: integer] : TAbstractPageView read GetAbstractPageView;
    {/** Add a PageView to the StandardView */}
    procedure AddPageView(APageView : TAbstractPageView);
    {/** Add a RowView directly if the StandardView contains only one PageView*/}
    procedure AddRowView(ARowView : TAbstractRowView);
    {/** Pointer to the List containing default values for a specific DataField */}
    property DefaultValueList : TValueList read GetDefaultValueList;
    {/** Pointer to the List containing criteria for deciding if a DataField is editable */}
    property ReadOnlyList : TValueList read GetReadOnlyList;
    {/** Pointer to the List containing ComboBox values for a DataField */}
    property ComboValuesList : TValueList read GetComboValuesList;
    {/** Put the keys defining tabs in a set */}
    procedure GetTabKeys(ASet : TFieldList);
    {/** Pointer to the List containing criteria for deciding if a DataRow is read only */}
    property DataRowReadOnlyList : TValueList read GetDataRowReadOnlyList;
    {/** Add readonly-properties for the given fields */}
    procedure SetReadOnly(ROFields : array of TDataField; IsAbsolute : Boolean);
    {/** Add a object to the list with criterias decideing if DataField is editable */}
    procedure AddReadOnlyProperty(ReadOnlyProperty : TGridEditorReadOnly);
    {/** Add a object to the list with default values for DataField */}
    procedure AddDefaultValueProperty(DefaultValueProperty : TGridEditorDefaultValue);
    {/** Add a object to the list containing ComboBox items for DataField */}
    procedure AddComboStringsProperty(ComboStringsProperty : TGridEditorComboValues);
    {/** Add a object to the list with criterias decideing if DataRow is read only */}
    procedure AddDataRowReadOnlyProperty(ReadOnlyProperty : TGridEditorReadOnly);
    {/** Field for evaluating if a DataRow has legal values /*}
    property RowIsValidField : TClosedField read GetRowIsValidField write SetRowIsValidField;
    {/** List of fields needed for lookup of description and so forth */}
//    property LookupBaseFieldSet : TFieldList read GetLookupBaseFieldSet;
    {/** Set of KeyFields not keys in this table the user can make selections on */}
    property SelectionFields : TFieldList read GetSelectionFields;
    {/** Set of fields that make up additional levels in the RowStorage */}
    property HierarchyKeys : TFieldList read GetHierarchyKeys;
    {/** What is the user allowed to do to this StandardView */}
    property EditState : TEditState read GetEditState;
    {/** Should we enable Marmalading */}
    property Marmalade : Boolean read GetMarmalade;
    {/** List of allowed marmalade key combinations */}
    property MarmaladeKeys : TFieldList read GetMarmaladeKeys;
    {/** Should the Editor using this StandardView be ReadOnly? */}
    property ReadOnly : Boolean read GetReadOnly;
    {/** Should we only allow legal values in a combo) */}
    property AllowOnlyLegalComboValues : Boolean read GetAllowOnlyLegalComboValues;

    property DefaultSortFields : TFieldList read GetDefaultSortFields;
  end;

  {/** Describes how a TRowStorage is to be displayed for editing on screen */}
  TSingletonStandardView = class(TAbstractStandardView)
  private
    FDefaultSubtotalLevels : TFieldList;
    FDefaultValueList : TValueList;
    FReadOnlyList : TValueList;
    FComboValuesList : TValueList;
    FDataRowReadOnlyList : TValueList;
    FSelectionFields : TFieldList;
    FHierarchyKeys : TFieldList;
    FEditState : TEditState;
    FMarmaladeKeys : TFieldList;
    FMarmalade : Boolean;
    FDefaultSortFields : TFieldList;
    FOpenViewKeys : TFieldList;
    FReadOnly : Boolean;
    FAllowOnlyLegalComboValues : Boolean;
  protected
    // Let the standard view define which keys is to be shown in OpenView
    // - and in which default order
    function GetOpenViewKeyCount : integer; virtual;
    function GetOpenViewKey(index:integer) : TDataField; virtual;
//    function GetLookupBaseFieldSet : TFieldList;override;
    function GetSelectionFields : TFieldList;override;
    function GetHierarchyKeys : TFieldList;override;
    function GetEditState : TEditState;override;
    function GetMarmalade : Boolean;override;
    function GetMarmaladeKeys : TFieldList;override;
    function GetDefaultValueList : TValueList;override;
    function GetReadOnlyList : TValueList;override;
    function GetComboValuesList : TValueList;override;
    function GetDataRowReadOnlyList : TValueList;override;
    function GetDefaultSortFields : TFieldList; override;
    function GetSingletonPageView(iPageView: integer) : TSingletonPageView;
    procedure AddOpenViewKeys; virtual;
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(Value: Boolean);
    function GetAllowOnlyLegalComboValues: Boolean; override;
    procedure SetAllowOnlyLegalComboValues(Value: Boolean);
  public
    constructor Create(DataTable : TDataTable; ViewName, Caption : string);
    {/** Create a "default" standardView for this table */}
    constructor CreateDefault(DataTable : TDataTable; ViewName, Caption : string);
    destructor Destroy; override;

    {/** Which Marmalades should be permitted */}
    procedure SetMarmaladeKeys( Keys : array of TDataField );
    // ILAA moved away some kludge code from OpenView to be overridden from here
    procedure AddToInitialCriteria(ACriteria : TCriteria); virtual;

    {/** Pointer to list of TPageView objects */}
    property SingletonPageView[iPageView: integer] : TSingletonPageView read GetSingletonPageView;
    // ILAA moved away some kludge code from OpenView to be overridden from here
    property DefaultSubtotalLevels : TFieldList read FDefaultSubtotalLevels;
    {/** Should we enable Marmalading */}
    property Marmalade write FMarmalade;
    property OpenViewKeyCount : integer read GetOpenViewkeyCount;
    property OpenViewKey[index:integer] : TDataField read GetOpenViewKey;
    property OpenViewKeys : TFieldList read FOpenViewKeys;
    property EditState write FEditState;
    property ReadOnly write SetReadOnly;
    property AllowOnlyLegalComboValues : Boolean read GetAllowOnlyLegalComboValues write SetAllowOnlyLegalComboValues;
  end;

  {/** Describes how a TRowStorage is to be displayed for editing on screen */}
  TStandardView = class(TAbstractStandardView)
  private
    FDefaultStandardView : TSingletonStandardView;
    FCommonCriteria : TCondition;
    FCommonKeyList : TFieldList;
    FDisplayKeyList : TFieldList;
    FHideKeyList : TFieldList;
    FComboValuesList : TValueList;

    function GetCommonKeyCount : Integer;
    function GetCommonKey(iKey : Integer) : TDataField {TKeyField};
    function GetDisplayKeyCount : Integer;
    function GetDisplayKey(iKey : Integer) : TDataField {TKeyField};
    function CreateCommonCriteria(KeyCriteria : TCondition; PageViewCriteriaList : TList) : TCondition;
    procedure SeparateKeys(aKeyList : TFieldList; var aCommonKeyList, aDisplayKeyList, aHideKeyList : TFieldList;
              aCriteria : TCondition; aDataTable : TDataTable; ViewCommonKeys : Boolean);
  protected
    function GetPageView(iPageView: integer) : TPageView;
    function GetRowIsValidField : TClosedField; override;
    function CreatePageViewInstance(DefaultPageView : TSingletonPageView; DisplayKeyList,
       CommonKeyList : TFieldList) : TPageView; virtual;
//    function GetLookupBaseFieldSet : TFieldList; override;
    function GetSelectionFields : TFieldList; override;
    function GetHierarchyKeys : TFieldList; override;
    function GetEditState : TEditState; override;
    function GetMarmalade : Boolean; override;
    function GetMarmaladeKeys : TFieldList; override;
    function GetDefaultValueList : TValueList; override;
    function GetReadOnlyList : TValueList; override;
    function GetComboValuesList : TValueList; override;
    function GetDataRowReadOnlyList : TValueList; override;
    function GetDefaultSortFields : TFieldList; override;
    function GetReadOnly: Boolean; override;
    function GetAllowOnlyLegalComboValues: Boolean; override;
  public
    {/** Create dynamic standard view editable from TGridEditor */ }
    constructor CreateDynamic(ADefaultStandardView : TSingletonStandardView; KeyCriteria : TCondition;
                aKeyList, HideKeyList : TFieldList; ViewCommonKeys : boolean);
    {/** Create a independent editable StandardView that does not need any singelton */ }
//    constructor CreateIndependent(KeyCriteria : TCriteria; aFieldList : TFieldList;
//                ViewCommonKeys : boolean);
    {/** Destructor /*}
    destructor Destroy; override;

    {/** Show the levels that should be visible and hide all others */}
    procedure ShowDefaultSubtotalLevels(AStorageHandler : IRowStorageHandlerInterface); virtual;
    procedure ShowSubtotalLevels(Levels : TFieldList; AStorageHandler : IRowStorageHandlerInterface); virtual;

    {/** Pointer to list of TPageView objects */}
    property PageView[iPageView: integer] : TPageView read GetPageView;
    {/** Get the CommonKeyList for this StandardView */}
    property CommonKeyList : TFieldList read FCommonKeyList;
    {/** Get the CommonKeyCount for this StandardView */}
    property CommonKeyCount : Integer read GetCommonKeyCount;
    {/** Get a CommonKey for this StandardView */}
    property CommonKey[iKey : Integer] : TDataField {TKeyField} read GetCommonKey;
    {/** Get the DisplayKeyList for this StandardView */}
    property DisplayKeyList : TFieldList read FDisplayKeyList;
    {/** Get the CommonKeyCount for this StandardView */}
    property DisplayKeyCount : Integer read GetDisplayKeyCount;
    {/** Get a DisplayKey for this StandardView */}
    property DisplayKey[iKey : Integer] : TDataField {TKeyField} read GetDisplayKey;
    {/** Pointer to the list of fields never to be shown */}
    property HideKeyList : TFieldList read FHideKeyList;
    property DefaultStandardView : TSingletonStandardView read FDefaultStandardView;
  end;

  {/** Describes an individual page of a view */}
  TAbstractPageView = class
  private
    FRowViewList : TList;
    FCaption : String;
    FCriteria : TCriteria;
  protected
    function GetCustomRowView(iRowView : integer) : TCustomRowView;
    function GetColCount : Integer; virtual;
    function GetDisabledFieldList : TDefaultValueFieldList; virtual; abstract;
    function GetReadOnly : Boolean; virtual; abstract;
    function GetEmptyItem(AField: TDataField): TEmptyItem; virtual; abstract;
  public
    {/** Create singleton read-only page view used as copy base for dynamic view */ }
    constructor Create(TabCriteria : TCriteria; Caption : String);
    {/** Destructor */}
    destructor Destroy; override;

    {/** Number of active row views */}
    function RowViewCount: integer; virtual;
    {/** Pointer to list of TRowView objects */}
    property CustomRowView[iRowView : integer] : TCustomRowView read GetCustomRowView;
    {/** Add a RowView to the PageView */}
    procedure AddRowView(ARowView : TAbstractRowView); virtual;
    {/** Number of columns in all rows */}
    property ColCount : Integer read GetColCount;
    { /**  Procedure that closes four columns to a quarter and four quarters to a year */}
//    function CloseCol(iCol : Integer) : Integer; virtual;
    { /**  Procedure that opens a quarter to four months and a year to four quarters */}
//    procedure OpenCol(iCol : Integer); virtual;
    {/** Get the col for this field */}
    function ColOfField(AField : TDataField) : integer;
    function RowViewIndexOfField(AField : TDataField) : Integer;

    // "Event handler" that lets page views do magic when initializing an editor
    procedure BeforeGridEditorInitialize(ARowStorage:TCustomRowStorage; IsReadOnly:Boolean); virtual;

    {/** Conditions that separate DataRows of this PageView from other DataRows */}
    property Criteria: TCriteria read FCriteria;
    {/** Name of the Tab displayed on screen */}
    property Caption: string read FCaption;
    property DisabledFieldList : TDefaultValueFieldList read GetDisabledFieldList;
    property ReadOnly : Boolean read GetReadOnly;
    property EmptyItem[AField : TDataField] : TEmptyItem read GetEmptyItem;
  end;

  {/** Describes an individual page of a view */}
  TSingletonPageView = class(TAbstractPageView)
  private
    FDisabledFieldList : TDefaultValueFieldList;
    FReadOnly : Boolean;
    FHasHeaders: Boolean;
    FEmptyItems : TIndexContainer;
    function GetSingletonRowView(iRowView: integer): TSingletonRowView;
    procedure SetHasHeaders(const Value: Boolean);
    procedure SetEmptyItem(AField: TDataField; const Value: TEmptyItem);
  protected
    function GetDisabledFieldList : TDefaultValueFieldList; override;
//    function CreatePageViewInstance(DisplayKeyList, CommonKeyList : TFieldList) : TPageView; virtual;
    function GetReadOnly : Boolean; override;
    procedure SetReadOnly( const Value : Boolean );
    function GetEmptyItem(AField: TDataField): TEmptyItem; override;
  public
    {/** Create singleton read-only page view used as copy base for dynamic view */ }
    constructor Create(TabCriteria : TCriteria; Caption : String);
    {/** Create such PageViews of which there is only one per StandardView */ }
    constructor CreateSingle; virtual;
    {/** Destructor */}
    destructor Destroy; override;

    {/** Pointer to list of TSingletonRowView objects */}
    property SingletonRowView[iRowView: integer] : TSingletonRowView read GetSingletonRowView;
    property ReadOnly write SetReadOnly;
    property HasHeaders : Boolean read FHasHeaders write SetHasHeaders;
    property EmptyItem[AField : TDataField] : TEmptyItem read GetEmptyItem write SetEmptyItem;
  end;

  {/** Describes an individual page of a view */}
  TPageView = class(TAbstractPageView)
  private
    FDefaultPageView : TSingletonPageView;
    FHeaderRowViewList : TList;
    FCommonKeyList : TFieldList;
    FDisplayKeyList : TFieldList;

    function GetCommonKey(iKey : Integer) : TDataField {TKeyField};
    function GetHeaderRowView(iHeaderRowView : Integer) : THeaderRowView; virtual;
    function GetCommonKeyCount : Integer; //virtual;
    function GetRowView(iRowView : integer) : TRowView;
    function GetDisplayKey(iKey: Integer): TDataField {TKeyField};
    function GetDisplayKeyCount: Integer;
  protected
    {/** Create dynamic PageView editable from TGridEditor */ }
//    constructor DoCreateDynamic(DefaultPageView : TSingletonPageView; DisplayKeyList, CommonKeyList : TFieldList);
    function GetDisabledFieldList : TDefaultValueFieldList; override;
    function GetEmptyItem(AField: TDataField): TEmptyItem; override;

    {/** Do Create HeaderRowViews */}
    procedure DoCreateHeaders; virtual;
    {/** Create HeaderRowViews */}
    procedure CreateHeaders;
    function CreateRowViewInstance(DefaultRowView : TSingletonRowView;
      DisplayKeyList : TFieldList) : TRowView; virtual;
    function GetReadOnly : Boolean; override;
    {/** Get the singelton PageView for this PageView */}
    property DefaultPageView : TSingletonPageView read FDefaultPageView;
  public
    {/** Create dynamic PageView editable from TGridEditor */ }
    constructor CreateDynamic(DefaultPageView : TSingletonPageView; DisplayKeyList, CommonKeyList : TFieldList);// virtual;
    {/** Create a independent editable PageView that does not need any singelton */ }
//    constructor CreateIndependent(PageViewDisplayKeyList, PageViewCommonKeyList : TFieldList);
    {/** Destructor */}
    destructor Destroy; override;

    {/** Pointer to list of TRowView objects */}
    property RowView[iRowView : integer] : TRowView read GetRowView;
    {/** Add a HeaderRowView to the PageView */}
    procedure AddHeaderRowView(AHeaderRowView : THeaderRowView); virtual;
    {/** Number of active header row views */}
    function HeaderRowViewCount: integer; virtual;
    {/** Pointer to list of THeaderRowView objects */}
    property HeaderRowView[iRowView : integer] : THeaderRowView read GetHeaderRowView;
    {/** Refresh the headers when the number of columns has changed */}
    procedure RefreshHeaders; virtual;

    {/** Hide column ACol */}
    procedure HideCol(ACol : Integer);
    {/** Show column ACol */}
    procedure ShowCol(ACol : Integer);
    {/** Hide AField */}
    procedure HideField(AField : TDataField);
    {/** Show AField */}
    procedure ShowField(AField : TDataField);
    {/** Get the CommonKeyList for this StandardView */}
    property CommonKeyList : TFieldList read FCommonKeyList;
    {/** Get the CommonKeyCount for this StandardView */}
    property CommonKeyCount : Integer read GetCommonKeyCount;
    {/** Get a CommonKey for this StandardView */}
    property CommonKey[iKey : Integer] : TDataField {TKeyField} read GetCommonKey;
    {/** Get the DisplayKeyList for this StandardView */}
    property DisplayKeyList : TFieldList read FDisplayKeyList;
    {/** Get the CommonKeyCount for this StandardView */}
    property DisplayKeyCount : Integer read GetDisplayKeyCount;
    {/** Get a DisplayKey for this StandardView */}
    property DisplayKey[iKey : Integer] : TDataField {TKeyField} read GetDisplayKey;

    // "Event handler" that lets page views do magic when initializing an editor
    procedure BeforeGridEditorInitialize(ARowStorage:TCustomRowStorage; IsReadOnly:Boolean); override;
    procedure AddExtraFields( FieldSet : TFieldList ); virtual;
    procedure RemoveExtraField( Field : TDataField ); virtual;
  end;

  {/** Describes the individual columns to be shown within a screen row */}
  TAbstractRowView = class
  private
    FFieldList : TFieldList;
    function GetField(iColumn: integer) : TDataField;
  protected
    function GetReadOnly: Boolean; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    {/** Get the col for this field */}
    function ColOfField(AField : TDataField) : integer;

    {/** Number of data fields to be shown */}
    function FieldCount: Integer;
    {/** Pointer to the list of all fields in this RowView */}
    property Field[iColumn: Integer]: TDataField read GetField;
    {/** Pointer to the list of all fields in this RowView */}
    property FieldList : TFieldList read FFieldList;
    property ReadOnly : Boolean read GetReadOnly;
  end;

  TCustomRowView = class(TAbstractRowView)
  private
    FDataFieldList : TDataFieldList;
  protected
    {/** Create a description field for this RowViews DataFieldList */}
    function CreateListDescription(ADataFieldList : TDataFieldList) : TCalcField; virtual;
  public
    {/** Pointer to the list of MonthlyFields */}
    property DataFieldList : TDataFieldList read FDataFieldList;
    {/** Is the field mandatory to enter for the user? */}
    function IsMandatory(aKeyField: TDataField {TKeyField}) : Boolean;
  end;

  {/** Describes the individual columns to be shown within a screen row */}
  TSingletonRowView = class(TCustomRowView)
  private
    FReadOnly : Boolean;
  protected
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(Value: Boolean);
  public
    {/** Create individual screen rows; one database row can have several RowViews */}
    constructor Create(aFieldList : array of TDataField; aDataFieldList : TDataFieldList);
    {/** Create individual screen rows; one database row can have several RowViews */}
    constructor CreateWithFieldList(aFieldList : TFieldList);
    {/** Default RowView for this table */}
    constructor CreateDefaultForTable(DataTable : TDataTable);

    property ReadOnly write SetReadOnly;
  end;

  {/** Describes the individual columns to be shown within a screen row */}
  TRowView = class(TCustomRowView)
  private
    FDefaultRowView : TSingletonRowView;
    FHiddenCols : TValueList;
    FHiddenFields : TFieldList;
    FShowDescription : Boolean;
    FDescription : TCalcField;
    FDisplayKeyList : TFieldList;
    FExtraFields : TFieldList;

    procedure SetShowDescription(Value : Boolean);
    procedure SetDescription(ADescription : TCalcField);
  protected
    procedure FillFieldList; virtual;
    procedure RemoveHidden;
    procedure AddExtraField( Field : TDataField ); virtual;
    procedure RemoveExtraField( iField : Integer ); virtual;
    function IndexOfExtraField( Field : TDataField ) : Integer;
    property ShowDescription : Boolean read FShowDescription write SetShowDescription;
    property Description : TCalcField read FDescription write SetDescription;
    {/** Pointer to the List of cols not to be shown */}
    function GetReadOnly: Boolean; override;
  public
    {/** Create dynamic RowView editable from TGridEditor */ }
    constructor CreateDynamic(DefaultRowView : TSingletonRowView; DisplayKeyList : TFieldList);
    {/** Destructor */}
    destructor Destroy; override;

    {/** Hide column ACol */}
    procedure HideCol(ACol : Integer);
    {/** Show column ACol */}
    procedure ShowCol(ACol : Integer);
    {/** Hide AField */}
    procedure HideField(AField : TDataField);
    {/** Show AField */}
    procedure ShowField(AField : TDataField);

    {/** Pointer to the list with fields to be put first in the row */}
    property DisplayKeyList : TFieldList read FDisplayKeyList;
    {/** Pointer to the singleton RowView used as model for this RowView */}
    property DefaultRowView : TSingletonRowView read FDefaultRowView;
    {/** Pointer to the DescriptionField for this RowViews DataFieldList, if it has one */}
    property ListDescription : TCalcField read FDescription;
    property ExtraFields : TFieldList read FExtraFields;
    property HiddenCols : TValueList read FHiddenCols;
    property HiddenFields : TFieldList read FHiddenFields;
  end;

  THeaderRowView = class(TAbstractRowView)
  protected
    FDefaultRowView : TRowView;
    FOwnedObjects : TList;
    FMinimize : Boolean;
    function GetHeaderField(iColumn: integer) : THeaderField;
    procedure SetMinimize(Value : Boolean);
    function GetReadOnly: Boolean; override;
  public
    {/** Create individual screen rows; one database row can have several RowViews */}
    constructor CreateHeader(ADefaultRowView : TRowView);
    {/** Destructor */}
    destructor Destroy; override;

    {/** Refresh the header when the number of columns has changed */}
    procedure RefreshHeader; virtual;

    {/** Pointer to the list of HeaderFields */}
    property HeaderField[iColumn: Integer]: THeaderField read GetHeaderField;
    {/** Does the user want the columns to be as narrow as possible */}
    property Minimize : Boolean read FMinimize write SetMinimize;
    {/** Pointer to the singleton RowView used as model for this RowView */}
    property DefaultRowView : TRowView read FDefaultRowView;
  end;

  TStandardViewList = class(TStringList)
  private
//    function GetNumberOfStandardViews : Integer;
    function GetStandardView(idx : Integer) : TSingletonStandardView;
    function GetStandardViewByName(StandardView : String) : TSingletonStandardView;
    function GetStandardViewByTable(ATable : TDataTable) : TSingletonStandardView;
//    function GetIndexOfView(StandardView : String) : Integer;
  public
    {/** Create a list of singelton StandardViews from an array singelton StandardViews */}
    constructor Create(StandardViewArray : array of TSingletonStandardView);
    {/** Free the StandardViewList */}
    destructor Destroy; override;

    procedure FreeContents;
    {/** Number of standard views to be chosen from in view list */}
//    property ViewCount : Integer read GetNumberOfStandardViews;
    {/** Pointer to the individual standard view singletons */}
    property View[idx : Integer] : TSingletonStandardView read GetStandardView;
    {/** Pointer to the individual standard view singletons */}
    property ViewByName[StandardView : String] : TSingletonStandardView read GetStandardViewByName;
    {/** Get the first singleton standard view for this table */}
    property ViewByTable[ATable : TDataTable] : TSingletonStandardView read GetStandardViewByTable;
    {/** Get the index of StandardView in the StandardViewList */}
//   property IndexOf[StandardView : String] : Integer read GetIndexOfView;
 end;

var
  SVEmptyField : THeaderField;

implementation

uses
  SysUtils, DataEditorConstants, DataTranslations;

{ TAbstractStandardView }

constructor TAbstractStandardView.Create(DataTable : TDataTable; ViewName, Caption : string);
begin
  FDataTable := DataTable;
  FCaption := Caption;
  FViewName := ViewName;
//  FTableForLoad := nil;
  FPageViewList := TList.Create;
  FOwnedObjects := TList.Create;

  inherited Create;
end;

destructor TAbstractStandardView.Destroy;
var
   i : Integer;
begin
  for i := PageViewCount -1 downto 0 do
    AbstractPageView[i].Free;
  FPageViewList.Free;
  FreeListWithObjects(FOwnedObjects);

  inherited Destroy;
end;

function TAbstractStandardView.GetPageViewCount : Integer;
begin
   result := FPageViewList.Count;
end;

function TAbstractStandardView.GetAbstractPageView(iPageView: integer) : TAbstractPageView;
begin
  Assert (iPageView < PageViewCount, Self.ClassName + '.GetPageView: iPageView('+
               IntToStr(iPageView)+ ') >= PageViewCount(' +IntToStr(PageViewCount)+')');
  Assert (iPageView >= 0, Self.ClassName + '.GetPageView: iPageView ('+
               IntToStr(iPageView)+ ') <0');
  result := FPageViewList[iPageView];
end;

procedure TAbstractStandardView.AddPageView(APageView : TAbstractPageView);
begin
  FPageViewList.Add(APageView);
end;

procedure TAbstractStandardView.AddRowView(ARowView : TAbstractRowView);
begin
  Assert (PageViewCount=1, Self.ClassName + '.AddRowView: PageViewCount('+
        IntToStr(PageViewCount)+') <> 1');
  AbstractPageView[0].AddRowView(ARowView);
end;

procedure TAbstractStandardView.SetReadOnly(ROFields : array of TDataField; IsAbsolute : Boolean);
var
  iField : Integer;
  AROProperty : TGridEditorReadOnly;
begin
  for iField := Low(ROFields) to High(ROFields) do
  begin
    AROProperty := TAlwaysReadOnly.Create(ROFields[iField], IsAbsolute);
    AddReadOnlyProperty(AROProperty);
    FOwnedObjects.Add(AROProperty);
  end;
end;

procedure TAbstractStandardView.AddReadOnlyProperty(ReadOnlyProperty : TGridEditorReadOnly);
begin
  TGridEditorProperties.AddToList(ReadOnlyList, ReadOnlyProperty);
end;

procedure TAbstractStandardView.AddDefaultValueProperty(DefaultValueProperty : TGridEditorDefaultValue);
begin
  TGridEditorProperties.AddToList(DefaultValueList, DefaultValueProperty);
end;

procedure TAbstractStandardView.AddComboStringsProperty(ComboStringsProperty : TGridEditorComboValues);
begin
  TGridEditorProperties.AddToList(ComboValuesList, ComboStringsProperty);
end;

procedure TAbstractStandardView.AddDataRowReadOnlyProperty(ReadOnlyProperty : TGridEditorReadOnly);
begin
  TGridEditorProperties.AddToList(DataRowReadOnlyList, ReadOnlyProperty);
end;

function TAbstractStandardView.GetRowIsValidField : TClosedField;
begin
  if FRowIsValidField <> nil then
    Result := FRowIsValidField
  else
    Result := nil;
end;

procedure TAbstractStandardView.SetRowIsValidField(AField : TClosedField);
begin
  FRowIsValidField := AField;
end;

procedure TAbstractStandardView.GetTabKeys(ASet : TFieldList);
var
  ACrit : TCriteria;
  Iterator : TCriteriaFieldIterator;
begin
  ACrit := AbstractPageView[0].Criteria;
  if ACrit <> nil then
  begin
    Iterator := TCriteriaFieldIterator.Create(ACrit);
    while not Iterator.EOF do
    begin
      if Iterator.CriteriaField.HasExactlyOneValue then
        ASet.Add(Iterator.DataField);
      Iterator.Next;
    end;
    Iterator.Free;
  end;
end;

{ TSingletonStandardView }

constructor TSingletonStandardView.Create(DataTable : TDataTable; ViewName, Caption : string);
begin
  inherited Create(DataTable, ViewName, Caption);

  FReadOnly := False;
  FAllowOnlyLegalComboValues := True;
  FDefaultValueList := TValueList.Create(ObjectType);
  FReadOnlyList := TValueList.Create(ObjectType);
  FComboValuesList := TValueList.Create(ObjectType);
  FDataRowReadOnlyList := TValueList.Create(ObjectType);
  FDefaultSubtotalLevels := TFieldList.Create;
  FDefaultSubtotalLevels.Duplicates := dupIgnore;
//  FLookupBaseFieldSet := TFieldList.Create;
//  FLookupBaseFieldSet.Duplicates := dupIgnore;
  FSelectionFields := TFieldList.Create;
  FSelectionFields.Duplicates := dupIgnore;
  FHierarchyKeys := TFieldList.Create;
  FHierarchyKeys.Duplicates := dupIgnore;
  FMarmaladeKeys := TFieldList.Create;
  FDefaultSortFields := TFieldList.Create;
  FDefaultSortFields.Duplicates := dupIgnore;

  FOpenViewKeys := TFieldList.Create;
  FOpenViewKeys.Duplicates := dupIgnore;
  AddOpenViewKeys;

  FMarmalade := False;
  FEditState := [etAddRow, etDuplicateRow, etDeleteRow];
end;

constructor TSingletonStandardView.CreateDefault(DataTable : TDataTable; ViewName, Caption : string);
begin
  Create(DataTable, ViewName, Caption);

  Self.AddPageView(TSingletonPageView.CreateSingle);
  Self.AddRowView(TSingletonRowView.CreateDefaultForTable(DataTable));
end;

destructor TSingletonStandardView.Destroy;
begin
  inherited Destroy;

  FDefaultValueList.Free;
  FReadOnlyList.Free;
  FComboValuesList.Free;
  FDataRowReadOnlyList.Free;
  FSelectionFields.Free;
  FHierarchyKeys.Free;
  FOpenViewKeys.Free;
  FMarmaladeKeys.Free;
  FDefaultSubtotalLevels.Free;
  FDefaultSortFields.Free;
end;

procedure TSingletonStandardView.AddOpenViewKeys;
var
  AList : TList;
  i : Integer;
begin
  AList := TList.Create;
  DataTable.KeysToList( AList );

  for i := 0 to AList.Count -1 do
    OpenViewKeys.Add( AList[i] );
  AList.Free;
end;

procedure TSingletonStandardView.SetMarmaladeKeys( Keys : array of TDataField );
var
  iKey : Integer;
begin
  for iKey := Low(Keys) to High(Keys) -1 do
    DefaultSubtotalLevels.Add(Keys[iKey]);
  FMarmaladeKeys.AddFromArray(Keys);
end;

function TSingletonStandardView.GetSingletonPageView(iPageView: integer) : TSingletonPageView;
begin
  Assert( AbstractPageView[iPageView] is TSingletonPageView );
  Result := TSingletonPageView( AbstractPageView[iPAgeView] );
end;

function TSingletonStandardView.GetEditState: TEditState;
begin
  Result := FEditState;
end;

function TSingletonStandardView.GetHierarchyKeys: TFieldList;
begin
  Result := FHierarchyKeys;
end;

function TSingletonStandardView.GetMarmalade: Boolean;
begin
  Result := FMarmalade;
end;

function TSingletonStandardView.GetMarmaladeKeys: TFieldList;
begin
  Result := FMarmaladeKeys;
end;

function TSingletonStandardView.GetSelectionFields: TFieldList;
begin
  Result := FSelectionFields;
end;

function TSingletonStandardView.GetDefaultValueList : TValueList;
begin
  Result := FDefaultValueList;
end;

function TSingletonStandardView.GetReadOnlyList : TValueList;
begin
  Result := FReadOnlyList;
end;

function TSingletonStandardView.GetComboValuesList : TValueList;
begin
  Result := FComboValuesList;
end;

function TSingletonStandardView.GetDataRowReadOnlyList : TValueList;
begin
  Result := FDataRowReadOnlyList;
end;

function TSingletonStandardView.GetDefaultSortFields : TFieldList;
begin
  result := FDefaultSortFields;
end;

function TSingletonStandardView.GetOpenViewKeyCount : integer;
begin
  result := OpenViewKeys.Count;
end;

function TSingletonStandardView.GetOpenViewKey(index:integer) : TDataField;
begin
  result := OpenViewKeys.Field[index];
end;

procedure TSingletonStandardView.AddToInitialCriteria(ACriteria : TCriteria);
begin
   // the default is to do no restrictions
end;

procedure TSingletonStandardView.SetReadOnly( Value : Boolean );
begin
  FReadOnly := Value;
end;

function TSingletonStandardView.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TSingletonStandardView.GetAllowOnlyLegalComboValues: Boolean;
begin
  Result := FAllowOnlyLegalComboValues;
end;

procedure TSingletonStandardView.SetAllowOnlyLegalComboValues(
  Value: Boolean);
begin
  FAllowOnlyLegalComboValues := Value;
end;

{ TStandardView }

constructor TStandardView.CreateDynamic(ADefaultStandardView : TSingletonStandardView; KeyCriteria : TCondition;
            aKeyList, HideKeyList : TFieldList; ViewCommonKeys : boolean);

  function CreateCopyOfComboProperties : TValueList;
  var
    i : Integer;
    AProperty : TGridEditorComboValues;
  begin
    Result := TValueList.Create( ObjectType );
    for i := 0 to DefaultStandardView.ComboValuesList.Count -1 do
    begin
      Assert( DefaultStandardView.ComboValuesList.Objects[i] is TGridEditorComboValues );
      AProperty := TGridEditorComboValues(DefaultStandardView.ComboValuesList.Objects[i]);
      TGridEditorProperties.AddToList(Result, AProperty.CreateCopy );
    end;
  end;

var
  iPageView, iField : Integer;
  PageViewCriteriaList : TList;
  DefaultPageView : TSingletonPageView;
  PageViewCommonCriteria : TCondition;
  PageViewCommonKeyList, PageViewDisplayKeyList, PageViewHideKeysList : TFieldList;
begin
  FDefaultStandardView := ADefaultStandardView;
  FDisplayKeyList := TFieldList.Create;
  FCommonKeyList := TFieldList.Create;
  FHideKeyList := TFieldList.Create;
  FHideKeyList.CopyFrom(HideKeyList);
  FComboValuesList := CreateCopyOfComboProperties;

  inherited Create(DefaultStandardView.DataTable, DefaultStandardView.ViewName, DefaultStandardView.Caption);

  PageViewCriteriaList := TList.Create;
  for iPageView := 0 to DefaultStandardView.PageViewCount -1 do
    PageViewCriteriaList.Add(DefaultStandardView.AbstractPageView[iPageView].Criteria);

  FCommonCriteria := CreateCommonCriteria(KeyCriteria, PageViewCriteriaList);
  SeparateKeys(aKeyList, FCommonKeyList, FDisplayKeyList, HideKeyList, FCommonCriteria, DataTable, ViewCommonKeys);

  PageViewCommonKeyList := TFieldList.Create;
  PageViewDisplayKeyList := TFieldList.Create;
  PageViewHideKeysList := TFieldList.Create;
  for iPageView := 0 to DefaultStandardView.PageViewCount -1 do
  begin
      PageViewHideKeysList.Clear;
      PageViewHideKeysList.AddFrom( HideKeyList );
      PageViewHideKeysList.AddFrom( DefaultStandardView.AbstractPageView[iPageView].DisabledFieldList );


        PageViewCommonCriteria := KeyCriteria.CreateCopy;

      SeparateKeys(FDisplayKeyList, PageViewCommonKeyList, PageViewDisplayKeyList, PageViewHideKeysList,
        PageViewCommonCriteria, DataTable, ViewCommonKeys);
      PageViewCommonCriteria.Free;
{    end
    else
    begin
      PageViewDisplayKeyList.CopyFrom(FDisplayKeyList);
      PageViewCommonKeyList.CopyFrom(FCommonKeyList);
    end;}
    // Create the a dynamic PageView
    DefaultPageView := DefaultStandardView.SingletonPageView[iPageView];
    FPageViewList.Add( CreatePageViewInstance( DefaultPageView, PageViewDisplayKeyList, PageViewCommonKeyList ) );
    PageViewCommonKeyList.Clear;
    PageViewDisplayKeyList.Clear;
  end;
  PageViewCommonKeyList.Free;
  PageViewDisplayKeyList.Free;
  PageViewCriteriaList.Free;
  PageViewHideKeysList.Free;

  for iField := 0 to HideKeyList.Count -1 do
    for iPageView := 0 to PageViewCount -1 do
     if not PageView[iPageView].CommonKeyList.ContainsField( HideKeyList[iField] ) and
        not PageView[iPageView].RowView[0].DisplayKeyList.ContainsField( HideKeyList[iField] ) then
       PageView[iPageView].HideField( HideKeyList[iField] );
end;

destructor TStandardView.Destroy;
var
  i : Integer;
begin
  inherited Destroy;

  FCommonKeyList.Free;
  FDisplayKeyList.Free;
  FCommonCriteria.Free;
  FHideKeyList.Free;
  for i := FComboValuesList.Count -1 downto 0 do
    FComboValuesList.Objects[i].Free;
  FComboValuesList.Free;
end;

function TStandardView.CreatePageViewInstance(DefaultPageView : TSingletonPageView; DisplayKeyList, CommonKeyList : TFieldList) : TPageView;
begin
  Result := TPageView.CreateDynamic(DefaultPageView, DisplayKeyList, CommonKeyList);
end;

function TStandardView.CreateCommonCriteria(KeyCriteria : TCondition; PageViewCriteriaList : TList) : TCondition;

begin
  if (PageViewCriteriaList.Count = 1) and
     (PageViewCriteriaList[0] = nil) then
    Result := KeyCriteria.CreateCopy
  else

    Result := nil;

end;

procedure TStandardView.SeparateKeys(aKeyList : TFieldList; var aCommonKeyList, aDisplayKeyList, aHideKeyList : TFieldList;
              aCriteria : TCondition; aDataTable : TDataTable; ViewCommonKeys : Boolean);


  function TableHasField(aField : TDataField) : Boolean;
  begin
    result := aDataTable.TableHasField(aField);
    if not result then
    begin
      aField := aField.LookupField;
      result := ( not HierarchyKeys.ContainsField(aField) ) and
                  aDataTable.TableHasField(aField);
    end;
  end;

var
  iField : Integer;
  ActiveField : TDataField;
  HasJustOneValue : Boolean;
  AValue : TValue;
begin
  if aKeyList = nil then
    Exit;

  for iField := 0 to aKeyList.Count -1 do
  begin
    ActiveField := aKeyList.Field[iField];
    if TableHasField( ActiveField ) and
       ( (aHideKeyList = nil) or not aHideKeyList.ContainsField(ActiveField) ) then
    begin
      HasJustOneValue := aCriteria.AcceptsExactlyOneValue( ActiveField, AValue );
      if HasJustOneValue and not ViewCommonKeys then
        aCommonKeyList.Add(ActiveField)
      else
        aDisplayKeyList.Add(ActiveField);
    end;
  end;
end;

function TStandardView.GetPageView( iPageView : Integer ) : TPageView;
begin
  Assert( AbstractPageView[iPageView] is TPageView);
  Result := TPageView( AbstractPageView[iPageView]);
end;

function TStandardView.GetCommonKeyCount : Integer;
begin
  result := FCommonKeyList.Count;
end;

function TStandardView.GetCommonKey(iKey : Integer) : TDataField {TKeyField};
begin
  Assert (iKey < CommonKeyCount, Self.ClassName + '.GetCommonKey: iKey('+
               IntToStr(iKey)+ ') >= PageViewCount(' +IntToStr(CommonKeyCount)+')');
  Assert (iKey >= 0, Self.ClassName + '.GetCommonKey: iKey ('+
               IntToStr(iKey)+ ') <0');
//  result := TKeyField(FCommonKeyList.Field[iKey];
  result := FCommonKeyList.Field[iKey];
end;

function TStandardView.GetDisplayKeyCount : Integer;
begin
  result := FDisplayKeyList.Count;
end;

function TStandardView.GetDisplayKey(iKey : Integer) : TDataField {TKeyField};
begin
  Assert (iKey < DisplayKeyCount, Self.ClassName + '.GetDisplayKey: iKey('+
               IntToStr(iKey)+ ') >= PageViewCount(' +IntToStr(DisplayKeyCount)+')');
  Assert (iKey >= 0, Self.ClassName + '.GetDisplayKey: iKey ('+
               IntToStr(iKey)+ ') <0');
//  result := TKeyField(FDisplayKeyList.Field[iKey]);
  result := FDisplayKeyList.Field[iKey];
end;

function TStandardView.GetRowIsValidField : TClosedField;
begin
  Result := Inherited GetRowIsValidField;

  if (Result = nil) and
     (DefaultStandardView <> nil) then
    Result := DefaultStandardView.RowIsValidField;
end;

procedure TStandardView.ShowDefaultSubtotalLevels(AStorageHandler : IRowStorageHandlerInterface);
begin
  ShowSubtotalLevels(DefaultStandardView.DefaultSubtotalLevels, AStorageHandler);
end;

procedure TStandardView.ShowSubtotalLevels(Levels : TFieldList; AStorageHandler : IRowStorageHandlerInterface);
var
  iField : Integer;
begin
  for iField := 0 to Levels.Count -1 do
    if iField = 0 then
      AStorageHandler.ShowSubTotalLevelOnly(Levels.Field[iField])
    else
      AStorageHandler.ShowSubTotalLevel(Levels.Field[iField]);
end;

function TStandardView.GetEditState: TEditState;
begin
  Result := DefaultStandardView.EditState;
end;

function TStandardView.GetHierarchyKeys: TFieldList;
begin
  Result := DefaultStandardView.HierarchyKeys;
end;

{function TStandardView.GetLookupBaseFieldSet: TFieldList;
begin
  Result := DefaultStandardView.LookupBaseFieldSet;
end;
}
function TStandardView.GetMarmalade: Boolean;
begin
  Result := DefaultStandardView.Marmalade;
end;

function TStandardView.GetMarmaladeKeys: TFieldList;
begin
  Result := DefaultStandardView.MarmaladeKeys;
end;

function TStandardView.GetSelectionFields: TFieldList;
begin
  Result := DefaultStandardView.SelectionFields;
end;

function TStandardView.GetDefaultValueList : TValueList;
begin
  Result := DefaultStandardView.DefaultValueList;
end;

function TStandardView.GetReadOnlyList : TValueList;
begin
  Result := DefaultStandardView.ReadOnlyList;
end;

function TStandardView.GetComboValuesList : TValueList;
begin
  Result := fComboValuesList;
end;

function TStandardView.GetDataRowReadOnlyList : TValueList;
begin
  Result := DefaultStandardView.DataRowReadOnlyList;
end;

function TStandardView.GetDefaultSortFields : TFieldList;
begin
  result := DefaultStandardView.DefaultSortFields;
end;

function TStandardView.GetReadOnly: Boolean;
begin
  Result := DefaultStandardView.ReadOnly;
end;

function TStandardView.GetAllowOnlyLegalComboValues: Boolean;
begin
  Result := DefaultStandardView.AllowOnlyLegalComboValues;
end;

{ TAbstractPageView }

constructor TAbstractPageView.Create(TabCriteria : TCriteria; Caption : String);
begin
  inherited Create;
  FRowViewList := TList.Create;
  FCaption := Caption;
  FCriteria := TabCriteria;
end;

destructor TAbstractPageView.Destroy;
var
   iRowView: Integer;
begin
  for iRowView := RowViewCount -1 downto 0 do
    CustomRowView[iRowView].Free;
  FRowViewList.Free;

  inherited Destroy;
end;

function TAbstractPageView.GetCustomRowView(iRowView : Integer) : TCustomRowView;
begin
  Assert(iRowView < RowViewCount, Self.ClassName + '.GetCustomRowView ' + TranslateMessage(E_IndexOutOfBounds) + ' (' +
      IntToStr(iRowView) + ' >= ' + IntToStr(RowViewCount) + ')');
  result := FRowViewList[iRowView];
end;

function TAbstractPageView.RowViewCount: Integer;
begin
  result := FRowViewList.Count;
end;

function TAbstractPageView.GetColCount : Integer;
begin
  result := CustomRowView[0].FieldCount;
end;

procedure TAbstractPageView.AddRowView(ARowView : TAbstractRowView);
begin
  if RowViewCount > 0 then
    Assert(ARowView.FieldCount = CustomRowView[0].FieldCount,
       Self.ClassName + '.AddRowView: Check that subsequent RowViews have the same number of fields as the previous ones!' +
       IntToStr(ARowView.FieldCount) +  ' <> '  + IntToStr(CustomRowView[0].FieldCount));
  FRowViewList.Add(ARowView);
end;

function TAbstractPageView.ColOfField(AField : TDataField) : integer;
var
  iRowView : Integer;
begin
  Result := -1;
  for iRowView := 0 to RowViewCount -1 do
  begin
    Result := CustomRowView[iRowView].ColOfField(AField);
    if Result >= 0 then
      Break;
  end;
end;

function TAbstractPageView.RowViewIndexOfField(AField : TDataField) : Integer;
var
  iRowView, iField : Integer;
begin
  Result := -1;
  for iRowView := 0 to RowViewCount -1 do
  begin
    iField := CustomRowView[iRowView].ColOfField(AField);
    if iField >= 0 then
    begin
      Result := iRowView;
      Break;
    end;
  end;
end;

// "Event handler" that lets page views do magic when initializing an editor
procedure TAbstractPageView.BeforeGridEditorInitialize(ARowStorage:TCustomRowStorage; IsReadOnly:Boolean);
begin
  // the defualt is to do nothing
end;

{ TSingletonPageView }

constructor TSingletonPageView.Create(TabCriteria : TCriteria; Caption : String);
begin
  inherited Create(TabCriteria.CreateCopyOfCriteria, Caption);

  FReadOnly := False;
  FHasHeaders := True;
  FDisabledFieldList := TDefaultValueFieldList.Create;
  FEmptyItems := TIndexContainer.Create(17, True);
end;

constructor TSingletonPageView.CreateSingle;
begin
  inherited Create(nil, '');

  FHasHeaders := True;
  FDisabledFieldList := TDefaultValueFieldList.Create;
  FEmptyItems := TIndexContainer.Create(17, True);
end;

destructor TSingletonPageView.Destroy;
begin
  inherited Destroy;

  FCriteria.Free;
  FDisabledFieldList.Free;
  with TIndexContainerIterator.Create(FEmptyItems) do
  begin
    while not Eof do
    begin
      Dispose(Pointer(Data));
      Next;
    end;
    Free;
  end;
  FEmptyItems.Free;
end;

function TSingletonPageView.GetDisabledFieldList: TDefaultValueFieldList;
begin
  Result := FDisabledFieldList;
end;

function TSingletonPageView.GetEmptyItem(AField: TDataField): TEmptyItem;
var
  PItem : PEmptyItem;
  AObj : TObject;
begin
  if FEmptyItems.Contains(AField, AObj) then
  begin
    PItem := PEmptyItem(AObj);
    Result := PItem^;
  end
  else
    Result := eiNoEmpty;
end;

function TSingletonPageView.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TSingletonPageView.GetSingletonRowView(
  iRowView: integer): TSingletonRowView;
begin
  Assert( CustomRowView[iRowView] is TSingletonRowView );
  Result := TSingletonRowView( CustomRowView[iRowView] );
end;

procedure TSingletonPageView.SetEmptyItem(AField: TDataField;
  const Value: TEmptyItem);
var
  AObj : TObject;
  PItem : PEmptyItem;
begin
  if FEmptyItems.Contains(AField, AObj) then
  begin
    PItem := PEmptyItem(AObj);
    PItem^ := Value;
  end
  else
  begin
    New(PItem);
    PItem^ := Value;
    AObj := TObject(PItem);
    FEmptyItems.Add(AField, AObj);
  end;
end;

procedure TSingletonPageView.SetHasHeaders(const Value: Boolean);
begin
  FHasHeaders := Value;
end;

procedure TSingletonPageView.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

{ TPageView }

constructor TPageView.CreateDynamic(DefaultPageView : TSingletonPageView; DisplayKeyList, CommonKeyList : TFieldList);
var
   iField, iRow : Integer;
   FieldList : TFieldList;
begin
  inherited Create(DefaultPageView.Criteria, DefaultPageView.Caption);

  FDefaultPageView := DefaultPageView;
  FCommonKeyList := TFieldList.Create;
  FCommonKeyList.CopyFrom(CommonKeyList);
  FDisplayKeyList := TFieldList.Create;
  FDisplayKeyList.CopyFrom(DisplayKeyList);

  FieldList := TFieldList.Create;
  // Create a list of empty fields to be used as DisplayKeys on
  // other RowView rows than the first RowView row
  for iField := 0 to DisplayKeyList.Count -1 do
    FieldList.Add(SVEmptyField);


  // Fill out all the RowViews
  for iRow := 0 to DefaultPageView.RowViewCount -1 do
  begin
    if iRow = 0 then // Only first row contains DisplayKeyList
      FRowViewList.Add( CreateRowViewInstance( TSingletonRowView(DefaultPageView.CustomRowView[iRow]), DisplayKeyList ) )
    else  // subsequent rows contain empty fields instead of DisplayKeyList
      FRowViewList.Add( CreateRowViewInstance( TSingletonRowView(DefaultPageView.CustomRowView[iRow]), FieldList ) );
  end;
  FieldList.Free;

  // Create list of RowViews (usually 1) for displaying column headers
  FHeaderRowViewList := TList.Create();
  if DefaultPageView.HasHeaders then
    CreateHeaders;
end;

function TPageView.CreateRowViewInstance(DefaultRowView : TSingletonRowView;
  DisplayKeyList : TFieldList) : TRowView;
begin
  Result := TRowView.CreateDynamic(DefaultRowView, DisplayKeyList);
end;

destructor TPageView.Destroy;
var
  iRowView: Integer;
begin
  Inherited Destroy;

  for iRowView := HeaderRowViewCount -1 downto 0 do
    HeaderRowView[iRowView].Destroy;
  FHeaderRowViewList.Free;
  FCommonKeyList.Free;
  FDisplayKeyList.Destroy;
end;

function TPageView.GetReadOnly: Boolean;
begin
  Result := DefaultPageView.ReadOnly;
end;

function TPageView.GetDisabledFieldList: TDefaultValueFieldList;
begin
  Result := DefaultPageView.DisabledFieldList;
end;

function TPageView.GetRowView(iRowView : Integer) : TRowView;
begin
  Assert( iRowView < RowViewCount, Self.ClassName + '.GetRowView ' + TranslateMessage(E_IndexOutOfBounds) + ' (' +
      IntToStr(iRowView) + ' >= ' + IntToStr(RowViewCount) + ')' );
  Assert( TObject(FRowViewList[iRowView]) is TRowView );
  result := FRowViewList[iRowView];
end;

function TPageView.GetCommonKeyCount : Integer;
begin
  result := FCommonKeyList.Count;
end;

function TPageView.GetCommonKey(iKey : Integer) : TDataField {TKeyField};
begin
  Assert (iKey < CommonKeyCount, Self.ClassName + '.GetCommonKey: iKey('+
               IntToStr(iKey)+ ') >= PageViewCount(' +IntToStr(CommonKeyCount)+')');
  Assert (iKey >= 0, Self.ClassName + '.GetCommonKey: iKey ('+
               IntToStr(iKey)+ ') <0');
//  result := TKeyField(FCommonKeyList.Field[iKey]);
  result := FCommonKeyList.Field[iKey];
end;

procedure TPageView.CreateHeaders;
begin
  DoCreateHeaders;
end;

procedure TPageView.RefreshHeaders;
var
  iRow : Integer;
begin
  for iRow := 0 to HeaderRowViewCount -1 do
    HeaderRowView[iRow].RefreshHeader;
end;

procedure TPageView.DoCreateHeaders;
begin
  AddHeaderRowView(THeaderRowView.CreateHeader(RowView[0]));
//  RefreshHeaders;
end;

function TPageView.GetHeaderRowView(iHeaderRowView : Integer) : THeaderRowView;
begin
  Assert(iHeaderRowView < HeaderRowViewCount, Self.ClassName + '.GetHeaderRowView: ' + TranslateMessage(E_IndexOutOfBounds) + ' (' +
                                             IntToStr(iHeaderRowView) + ' >= ' + IntToStr(RowViewCount) + ')');
  result := FHeaderRowViewList[iHeaderRowView];
end;

function TPageView.HeaderRowViewCount: Integer;
begin
  result := FHeaderRowViewList.Count;
end;

procedure TPageView.AddHeaderRowView(AHeaderRowView : THeaderRowView);
begin
  Assert(AHeaderRowView.FieldCount = RowView[0].FieldCount,
     Self.ClassName + '.AddHeaderRowView: Check that HeaderRowViewList has the same number of fields as the RowViews themselves!');
  FHeaderRowViewList.Add(AHeaderRowView);
end;

procedure TPageView.HideCol(ACol : Integer);
var
  iRowView : Integer;
begin
  for iRowView := 0 to RowViewCount -1 do
    RowView[iRowView].HideCol(ACol);
  RefreshHeaders;
end;

procedure TPageView.ShowCol(ACol : Integer);
var
  iRowView : Integer;
begin
  for iRowView := 0 to RowViewCount -1 do
    RowView[iRowView].ShowCol(ACol);
  RefreshHeaders;
end;

procedure TPageView.HideField(AField : TDataField);
var
  iRowView, iCol : Integer;
begin
  iRowView := RowViewIndexOfField(AField);
  if iRowView <> -1 then
  begin
    iCol := RowView[iRowView].ColOfField(AField);
    if iCol <> -1 then
    begin
      for iRowView := 0 to RowViewCount -1 do
        RowView[iRowView].HideField(RowView[iRowView].Field[iCol]);
    end;
  end;
  RefreshHeaders;
end;

procedure TPageView.ShowField(AField : TDataField);
var
  iRowView, idx : Integer;
begin
  idx := -1;
  for iRowView := 0 to RowViewCount -1 do
  begin
    idx := RowView[iRowView].HiddenFields.IndexOf(AField);
    if idx >= 0 then
      Break;
  end;

  if idx >= 0 then
    for iRowView := 0 to RowViewCount -1 do
      RowView[iRowView].ShowField( RowView[iRowView].HiddenFields.Field[idx] );
  RefreshHeaders;
end;

// "Event handler" that lets page views do magic when initializing an editor
procedure TPageView.BeforeGridEditorInitialize(ARowStorage:TCustomRowStorage; IsReadOnly:Boolean);
begin
  if FDefaultPageView<>nil then
    FDefaultPageView.BeforeGridEditorInitialize(ARowStorage, IsReadOnly);
end;

procedure TPageView.AddExtraFields( FieldSet : TFieldList );
var
  iRowView : Integer;
begin
  Assert( FieldSet.Count = RowViewCount, Self.ClassName + '.AddExtraFields: The' +
           'number of fields have to match the number of RowViews!' );
  for iRowView := 0 to RowViewCount -1 do
    RowView[iRowView].AddExtraField( FieldSet.Field[iRowView] );
  RefreshHeaders;
end;

procedure TPageView.RemoveExtraField( Field : TDataField );
var
  iField, iRowView : Integer;
begin
  iField := -1;
  for iRowView := 0 to RowViewCount -1 do
  begin
    iField := RowView[iRowView].IndexOfExtraField( Field );
    if iField >= 0 then
      Break;
  end;

  if iField >= 0 then
    for iRowView := 0 to RowViewCount -1 do
      RowView[iRowView].RemoveExtraField( iField );
end;

function TPageView.GetDisplayKey(iKey: Integer): TDataField {TKeyField};
begin
  Result := DisplayKeyList[iKey];
end;

function TPageView.GetDisplayKeyCount: Integer;
begin
  Result := DisplayKeyList.Count;
end;

function TPageView.GetEmptyItem(AField: TDataField): TEmptyItem;
begin
  Result := DefaultPageView.EmptyItem[AField];
end;

{ TAbstractRowView }

constructor TAbstractRowView.Create;
begin
  inherited Create;
  FFieldList := TFieldList.Create;
end;

destructor TAbstractRowView.Destroy;
begin
  FFieldList.Free;
  inherited Destroy;
end;

function TAbstractRowView.FieldCount : Integer;
begin
  Result := FFieldList.Count;
end;

function TAbstractRowView.GetField(iColumn: Integer) : TDataField;
begin
  Assert( iColumn < FieldCount, Self.ClassName + '.GetField: ' + TranslateMessage(E_IndexOutOfBounds)
         + ' (' + IntToStr(iColumn) + ' >= ' + IntToStr(FieldCount) + ')' );
  Assert( iColumn <> -1, Self.ClassName + '.GetField: ' + TranslateMessage(E_IndexOutOfBounds)
          + ' (' + IntToStr(iColumn) + ')' );
  Result := FFieldList.Field[iColumn];
end;

function TAbstractRowView.ColOfField(AField : TDataField) : integer;
begin
  Result := FieldList.IndexOf(AField);
end;

{ TCustomRowView }

function TCustomRowView.CreateListDescription(ADataFieldList : TDataFieldList) : TCalcField;
begin
  if ADataFieldList <> nil then
    Result := TConstantField.CreateNoName(ValueFromString(ADataFieldList.LongDescription))
  else
    Result := nil;
end;

function TCustomRowView.IsMandatory(aKeyField: TDataField {TKeyField}) : Boolean;
begin
// Fixa MVJ!!
  result := False;
end;

{ TSingletonRowView }

constructor TSingletonRowView.Create(aFieldList : array of TDataField; aDataFieldList : TDataFieldList);
var
  iField : Integer;
begin
  inherited Create;
  FReadOnly := False;
  FDataFieldList := aDataFieldList;
  if aFieldList[Low(aFieldList)] <> nil then
    for iField := Low(aFieldList) to High(aFieldList) do
      FFieldList.Add(aFieldList[iField]);
end;

constructor TSingletonRowView.CreateWithFieldList(aFieldList : TFieldList);
begin
  Assert(aFieldList <> nil, Self.ClassName + '.CreateWithFieldList: aFieldList = nil!');
  inherited Create;
  FDataFieldList := nil;
  if aFieldList <> nil then
    FFieldList.Assign(aFieldList);
end;

constructor TSingletonRowView.CreateDefaultForTable(DataTable : TDataTable);
var
  iField : Integer;
  AField : TDataField;
begin
  inherited Create;
  FDataFieldList := nil;

  // Values in fieldlists are not added if FieldListCount <> 1 for the moment
  if DataTable.FieldListCount <> 1 then
    for iField := DataTable.KeyCount to DataTable.FieldCount - 1 do
      FFieldList.Add(DataTable.Field[iField])
  else
  begin
    FDataFieldList := DataTable.FieldList[0];
    for iField := DataTable.KeyCount to DataTable.FieldCount - 1 do
    begin
      AField := DataTable.Field[iField];
      if not FDataFieldList.ContainsField(AField) then
        FFieldList.Add(DataTable.Field[iField]);
    end;
  end;
end;

function TSingletonRowView.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TSingletonRowView.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

{ TRowView }

constructor TRowView.CreateDynamic(DefaultRowView : TSingletonRowView; DisplayKeyList : TFieldList);
begin
  inherited Create;
  FDefaultRowView := DefaultRowView;

  FDisplayKeyList := TFieldList.Create;
  FHiddenCols := TValueList.Create(IntegerType);
  FHiddenCols.Sorted := True;
  FHiddenCols.Duplicates := dupIgnore;
  FHiddenFields := TFieldList.Create;
  FHiddenFields.Duplicates := dupAccept;
  FExtraFields := TFieldList.Create;
  FExtraFields.Duplicates := dupAccept;

  FDisplayKeyList.CopyFrom(DisplayKeyList);
  FDataFieldList := DefaultRowView.DataFieldList;

  FDescription := CreateListDescription(FDataFieldList);
  FillFieldList;
end;

destructor TRowView.Destroy;
begin
  Inherited Destroy;
  FHiddenCols.Free;
  FHiddenFields.Free;
  FDescription.Free;
  FDisplayKeyList.Free;
  FExtraFields.Free;
end;

procedure TRowView.FillFieldList;
var
  iField : Integer;
  TempList : TList;
begin
  FFieldList.CopyFrom(FDisplayKeyList);
  if ShowDescription then
    FFieldList.Add(Description);

  for iField := 0 to DefaultRowView.FieldList.Count -1 do
    FFieldList.Add(DefaultRowView.FieldList.Field[iField]);

  if DataFieldList <> nil then
  begin
    TempList := TList.Create;
    DataFieldList.AddToList(TempList, True);
    for iField := 0 to TempList.Count -1 do
      FFieldList.Add(TempList[iField]);
    TempList.Free;
  end;

  for iField := 0 to ExtraFields.Count -1 do
    FFieldList.Add(ExtraFields.Field[iField]);

  RemoveHidden;
end;

procedure TRowView.RemoveHidden;
var
  idx, iCol : Integer;
begin
  for idx := HiddenCols.Count -1 downto 0 do
  begin
    iCol := AsInteger(FHiddenCols.Values[idx]);
    if iCol < FieldList.Count then
      FieldList.Delete(iCol);
  end;

  for idx := HiddenFields.Count -1 downto 0 do
    FieldList.Remove(HiddenFields.Field[idx]);
end;

procedure TRowView.AddExtraField( Field : TDataField );
begin
  ExtraFields.Add( Field );
  FillFieldList;
end;

procedure TRowView.RemoveExtraField( iField : Integer );
begin
  Assert( iField < ExtraFields.Count );
  ExtraFields.Delete( iField );
  FillFieldList;
end;

function TRowView.IndexOfExtraField( Field : TDataField ) : Integer;
begin
  Result := ExtraFields.IndexOf( Field );
end;

procedure TRowView.SetShowDescription(Value : Boolean);
begin
  FShowDescription := Value;
end;

procedure TRowView.SetDescription(ADescription : TCalcField);
begin
  FDescription.Free;
  FDescription := ADescription;
end;

procedure TRowView.HideCol(ACol : Integer);
begin
  HiddenCols.AddVal(ValueFromInteger(ACol));
  FillFieldList;
end;

procedure TRowView.ShowCol(ACol : Integer);
var
  idx : Integer;
begin
  idx := HiddenCols.IndexOfValue(ValueFromInteger(ACol));
  if idx >= 0 then
    HiddenCols.Delete(idx);
  FillFieldList;
end;

procedure TRowView.HideField(AField : TDataField);
begin
  HiddenFields.Add(AField);
  FillFieldList;
end;

procedure TRowView.ShowField(AField : TDataField);
begin
  HiddenFields.Remove(AField);
  FillFieldList;
end;

function TRowView.GetReadOnly: Boolean;
begin
  Result := DefaultRowView.ReadOnly;
end;

{ THeaderRowView }

constructor THeaderRowView.CreateHeader(ADefaultRowView : TRowView);
begin
  inherited Create;

  FOwnedObjects := TList.Create;
  FDefaultRowView := ADefaultRowView;
  Minimize := False;
  RefreshHeader;
end;

destructor THeaderRowView.Destroy;
begin
  FreeListWithObjects(FOwnedObjects);
  inherited Destroy;
end;

procedure THeaderRowView.RefreshHeader;
var
  iField : Integer;
  AField : THeaderField;
begin
  EmptyListWithObjects(FOwnedObjects);
  FieldList.Clear;
  for iField := 0 to DefaultRowView.FieldCount -1 do
  begin
    AField := TDerivedHeaderField.CreateOld('', DefaultRowView.Field[iField]);
    AField.UseShortDescription := Minimize;
    FieldList.Add(AField);
    FOwnedObjects.Add(AField);
  end;
end;

function THeaderRowView.GetHeaderField(iColumn: integer) : THeaderField;
var
  AField : TDataField;
begin
  AField := Inherited GetField(iColumn);
  Assert(AField is THeaderField);
  Result := THeaderField(AField);
end;

procedure THeaderRowView.SetMinimize(Value : Boolean);
var
  iField : Integer;
begin
  if Value <> FMinimize then
  begin
    FMinimize := Value;
    for iField := 0 to FieldCount -1 do
      HeaderField[iField].UseShortDescription := FMinimize;
  end;
end;

{ TAbstractStandardViewList }


constructor TStandardViewList.Create(StandardViewArray : array of TSingletonStandardView);
var
  iView : Integer;
begin
  inherited Create;
  for iView := Low(StandardViewArray) to High(StandardViewArray) do
    AddObject(StandardViewArray[iView].ViewName, StandardViewArray[iView]);
end;

function TStandardViewList.GetStandardView(idx : Integer) : TSingletonStandardView;
begin
  Assert(idx < Count, Self.ClassName + '.GetStandardView: '+ TranslateMessage(E_IndexOutOfBounds) +
  ' ' + IntToStr(idx) + ' >= ' + IntToStr(Count));
  Result := TSingletonStandardView(Objects[idx]);
end;

function TStandardViewList.GetStandardViewByName(StandardView : String) : TSingletonStandardView;
var
  idxView : Integer;
begin
  Result := nil;
  idxView := IndexOf(StandardView);
  if idxView = -1 then
    Exit;
  Result := TSingletonStandardView(Objects[idxView]);
end;

destructor TStandardViewList.Destroy;
begin
  inherited Destroy;
end;

function TStandardViewList.GetStandardViewByTable(
  ATable: TDataTable): TSingletonStandardView;
var
  idxView : Integer;
begin
  Result := nil;
  for idxView := 0 to Count -1 do
    if View[idxView].DataTable = ATable then
    begin
      Result := View[idxView];
      Exit;
    end;
end;

procedure TStandardViewList.FreeContents;
begin
  EmptyStringListWithObjects( Self );
end;

function THeaderRowView.GetReadOnly: Boolean;
begin
  Result := True;
end;

initialization

  SVEmptyField := THeaderField.CreateOld('', nil, ValueFromString(''));

finalization

  SVEmptyField.Free;
  
end.

