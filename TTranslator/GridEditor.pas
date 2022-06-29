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

{ $Id: GridEditor.pas,v 1.300 2003/04/02 11:39:05 mvj Exp $ }

{---------------------------------------------------------------------------
  What:              Interface for RowStorage to PolyGrid (and/or DBUGrid)

  Company:           Polycon Ab
  Authors:           MVJ
---------------------------------------------------------------------------}

unit GridEditor;

interface
{$i common.inc}


uses
{$ifndef LINUX}
  Windows, Controls, Graphics,
{$else LINUX}
  Qt, Types, QControls, QGraphics,
{$endif LINUX}
  Classes,
  DataTypes, DataType, DataElements, RowList, Storages, CommonLib, IndexContainer,
  CommonCalcFields, DataClipboard,
  DBUGrid, DBUInterfaces, DBUTypes, DBUFormatter, DBUDataEditorCell, DBUCell, DBUCellTypes,
  DataEditorLib, EditorInterfaces, StandardView;

type
  TOnCellComboStringsFillingEvent = procedure (Sender : TObject; Strings : TDataRowList;
    DataField : TDataField; DataRow : TAbstractRow; var DefaultFill : Boolean) of object;
  TOnMouseDownEvent = procedure (Sender: TObject; Button: TMouseButton; Shift: TShiftState; ACol, ARow: Integer; Cell : TDataEditorCell;
                                 var RunInherited : boolean) of object;
  TOnChangingValueEvent = procedure (Sender: TObject; Cell : TDataInplaceEditorCell; var Value : TValue;
    var Reject, RunInherited : Boolean; var RejectParams : TRejectParams) of object;
  TOnChangeValueEvent = procedure (Sender: TObject; Cell : TDataInplaceEditorCell) of object;
  TOnDefineCellEvent = procedure (Sender: TObject; Cell : IDataEditorCellInterface) of object;

  TOnMarkRowColClickEvent = procedure (Sender: TObject; Button: TMouseButton; Shift: TShiftState;
                                      GridCol, GridRow: Integer; Cell : TDataEditorCell) of object;
  TOnHeaderClickEvent = procedure (Sender: TObject; Button: TMouseButton; Shift: TShiftState;
                                  GridCol, GridRow: Integer; Cell : TDataEditorCell) of object;
  TOnDeleteRowEvent = procedure(Sender : TObject; ARow : TDataRow) of object;
  TOnGetKeyFormatting = procedure(Sender : TObject; AField : TDataField; var Result : TDisplayValues) of object;
  TOnEditButtonClickEvent = procedure (Field : TDataField; DataRow : TAbstractRow; var Value : TValue) of object;
  TOnGridKeyDownEvent = procedure (Sender: TObject; var Key: Word; Shift: TShiftState) of object;
  TOnGetTextEvent = procedure (Sender: TObject; AField : TDataField; ARow : TDataRow; var Text: String) of object;
  TOnAutoCreateRowEvent = procedure (Sender: TObject; NewRow : TDataRow) of object;
//  TOnCellContentsChangesEvent = procedure (Sender: TObject; ACol, ARow: Longint; const Value: String) of object;

  TCheckBoxFieldObject = class;
  TGridEditor = class;

  TRowFieldRecord = record
    Row : TAbstractRow;
    Field : TDataField;
  end;

  TAbstractViewer = class
  private
    FParent : TWinControl;
    FGrid : TDBUGrid;
    FAntFrameDrawer : TAntFrameDrawer;
    FRowStorage : TCustomRowStorage;
    FReadOnly : Boolean;
    FShowBooleansAsCheckBoxes : Boolean;
    FKilling : Boolean;
//    FEnabled : Boolean;

    FDisableCount : Integer;
    FArrangeOnEnable : Boolean;
    FFocused : Boolean;
    FIsActive : Boolean;
    FFixedCols : Integer;
    FUseNegativeColor : Boolean;

    FOnDefineCell : TOnDefineCellEvent;

    procedure SetSetFocusOnExec( Value : Boolean );
    procedure SetShowAllBooleansAsCheckBoxes(Value : Boolean);
    function GetShowAllBooleansAsCheckBoxes : Boolean;
    function GetGridCol : Integer;
    procedure SetGridCol(Value : Integer);
    function GetGridRow : Integer;
    procedure SetGridRow(Value : Integer);
    function GetRowViewByGridIndex(idxGridRow : Integer) : TRowView;
    function GetRowViewIndexByGridIndex(idxGridRow : Integer) : Integer;
  protected
    FFreeCell, FFreeEditorCell : Boolean;
    FFreeCellFormatter, FFreeEditorCellFormatter : Boolean;

    FCell : TDataEditorCell;
    FFormatter : TDataEditorCellFormatter;
    FSetValueFormatter : TDataEditorCellFormatter;
    FSetValueCell : TDataInplaceEditorCell;
    FGridIterator : IGridIterator;
    FEditorIterator : IEditorIterator;

    procedure CreateGrid; virtual;
    procedure CreateCells; virtual;
    procedure CreateFormatter; virtual;

    function DefineCellAlignment(DataField : TDataField) : TAlignment;

    procedure GridGetCell( Sender : IDataEditorCellInterface ); virtual;
    procedure GridSetCell( Sender : IDataEditorCellInterface ); virtual;
    procedure DefineCell( Cell : IDataEditorCellInterface ); virtual;
    procedure DefineDecimalCountAndDivisor( ACell: IDataEditorCellInterface ); virtual;
    procedure DefineCellLook( Cell : IDataEditorCellInterface ); virtual;

    function GridCreateCell( Sender: TObject ) : TDBUCustomGridCell;
    function GridCreateFormatter( Sender: TObject ) : TDBUFormatter;
    function GridCreateEditorCell( Sender: TObject ) : TDBUCustomEditorCell;
    function GridCreateEditorFormatter( Sender: TObject ) : TDBUFormatter;

    function GetGridIterator : IGridIterator;
    function GetEditorIterator : IEditorIterator;

    property Grid : TDBUGrid read FGrid;
    property SetValueCell : TDataInplaceEditorCell read FSetValueCell;
    property EditorIterator : IEditorIterator read GetEditorIterator;

    function GetDataField(AGridRow, AGridCol : Integer) : TDataField; virtual; abstract;
    function GetRowViewDataField(ARowView : TAbstractRowView; AGridCol : Integer) : TDataField; virtual; abstract;
    function GetTotalColCount : Integer; virtual; abstract;
    function GetTotalRowCount : Integer; virtual; abstract;
    function GetHeaderRowViewCount : Integer; virtual;
    function GetRowViewCount : Integer; virtual; abstract;
    function GetAlwaysFixedColCount : Integer; virtual; abstract;
    function GetMarkRowColCount : Integer; virtual; abstract;
    function CanFixCols(Value : Integer) : Boolean; virtual;
    function GetRowStorage: TCustomRowStorage; virtual; abstract;
    procedure SetRowStorage(const Value  : TCustomRowStorage); virtual; abstract;
    function GetPageView : TPageView; virtual; abstract;

    function FieldBelongsToTable(AField : TDataField; ARow : TAbstractRow) : Boolean; virtual; abstract;
    function CellIsSelected(ACol, ARow : Integer) : Boolean; virtual;

    function GetFixedCols : Integer; virtual;
    procedure SetFixedCols(Value : Integer); virtual;
    function GetEnabled : Boolean;
    procedure SetEnabled(Value : Boolean);
    function GetDivisor(AField: TDataField; ARow: TAbstractRow) : Double; virtual; abstract;
    function GetCurrencyDivisor : Double; virtual; abstract;
    function GetCurrencyDecimalCount : Integer; virtual; abstract;
    function GetDecimalCount : Integer; virtual; abstract;
    function GetEditorDecimalCount : Integer; virtual; abstract;
    procedure FlushCells; virtual;

    procedure ArrangeRows; virtual;
    procedure GridClick(Sender : TObject); virtual; abstract;
    procedure GridDblClick(Sender : TObject); virtual;
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure SetGridSize; virtual; abstract;
    procedure SetIsActive(Value : Boolean); virtual;
    function GetReadOnly : Boolean; virtual;
    procedure SetReadOnly(Value : Boolean); virtual;
    function GetShowStripes : Boolean; virtual; abstract;

    procedure HideAnts;
    procedure ShowAnts;
    procedure RedrawAntArea;
    procedure SetKilling( Value : Boolean ); virtual;
    procedure GetTotalSelection( SelList : TList ); virtual;
    procedure GetGridSelection( var ARect : TRect );

    property EditorCell : TDataEditorCell read FCell;
    property CellFormatter : TDataEditorCellFormatter read FFormatter;
    property SetValueFormatter : TDataEditorCellFormatter read FSetValueFormatter;

    property UseNegativeColor : Boolean read FUseNegativeColor write FUseNegativeColor;
    property GridCol : Integer read GetGridCol write SetGridCol;
    property GridRow : Integer read GetGridRow write SetGridRow;
    property RowStorage : TCustomRowStorage read GetRowStorage write SetRowStorage;
    property Parent : TWinControl read FParent;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly;
    property RowViewDataField[ARowView : TAbstractRowView; AGridCol : Integer] : TDataField read GetRowViewDataField;
    property TotalColCount : Integer read GetTotalColCount;
    property TotalRowCount : Integer read GetTotalRowCount;
    property FixedCols : Integer read GetFixedCols write SetFixedCols;
    property IsActive : Boolean read FIsActive write SetIsActive;
    property OnDefineCell : TOnDefineCellEvent read FOnDefineCell write FOnDefineCell;
    property AlwaysFixedColCount : Integer read GetAlwaysFixedColCount;
    property HeaderRowViewCount : Integer read GetHeaderRowViewCount;
    property RowViewCount : Integer read GetRowViewCount;
    property MarkRowColCount : Integer read GetMarkRowColCount;
    property DisableCount : Integer read FDisableCount;
    property Divisor[AField: TDataField; ARow: TAbstractRow] : Double read GetDivisor;
    property CurrencyDivisor : Double read GetCurrencyDivisor;
    property CurrencyDecimalCount : Integer read GetCurrencyDecimalCount;
    property DecimalCount : Integer read GetDecimalCount;
    property EditorDecimalCount : Integer read GetEditorDecimalCount;
    property ShowStripes : Boolean read GetShowStripes;

    property RowViewIndexByGridIndex[idxGridRow : Integer] : Integer read GetRowViewIndexByGridIndex;
    property RowViewByGridIndex[idxGridRow : Integer] : TRowView read GetRowViewByGridIndex;
  public
    constructor Create(AParent : TWinControl);
    destructor Destroy; override;

    procedure Execute; virtual;
    property GridIterator : IGridIterator read GetGridIterator;
    procedure Refresh; virtual;
    procedure InvalidateGrid; virtual;
    procedure Disable; virtual;
    procedure Enable; virtual;
    procedure EnableWithCoord(iCol, iRow : Integer); virtual;

    function DefineDecimalCount(AField: TDataField; ARow: TAbstractRow;
      ForEditor: Boolean; var AlwaysShowCount : Integer ): Integer; virtual;
    function DefineDivisor(AField: TDataField; ARow: TAbstractRow;
      ForEditor : Boolean): Double; virtual;

    function Find(AValue: TValue; var APos: TPoint; MatchCase : Boolean = True; Down : Boolean = True) : Boolean; virtual;

    property PageView : TPageView read GetPageView;// write SetPageView;
    property Enabled : Boolean read GetEnabled write SetEnabled;
    property DataField[AGridRow, AGridCol : Integer] : TDataField read GetDataField;
    property ShowAllBooleansAsCheckBoxes : Boolean read GetShowAllBooleansAsCheckBoxes write SetShowAllBooleansAsCheckBoxes;
    property SetFocusOnExec : Boolean write SetSetFocusOnExec;
    property Killing : Boolean read FKilling write SetKilling;
  end;

  TTotalViewer = class(TAbstractViewer)
  private
    FActiveEditor : TGridEditor;
  protected
    procedure CreateGrid; override;
    function ColWidthsChanged : Boolean;
    function GetDataField(AGridRow, AGridCol : Integer) : TDataField; override;
    function GetRowViewDataField(ARowView : TAbstractRowView; AGridCol : Integer) : TDataField; override;
    function GetTotalColCount : Integer; override;
    function GetTotalRowCount : Integer; override;
    function GetAlwaysFixedColCount : Integer; override;
    function GetMarkRowColCount : Integer; override;
    function GetDivisor(AField: TDataField; ARow: TAbstractRow) : Double; override;
    function GetCurrencyDivisor : Double; override;
    function GetCurrencyDecimalCount : Integer; override;
    function GetDecimalCount : Integer; override;
    function GetEditorDecimalCount : Integer; override;
    function GetRowViewCount : Integer; override;
    function GetShowStripes : Boolean; override;
    function GetRowStorage: TCustomRowStorage; override;
    procedure SetRowStorage(const Value  : TCustomRowStorage); override;
    function GetPageView : TPageView; override;

    function FieldBelongsToTable(AField : TDataField; ARow : TAbstractRow) : Boolean; override;

    procedure GridGetCell( Sender : IDataEditorCellInterface ); override;
    procedure DefineCell( Cell : IDataEditorCellInterface ); override;

    procedure SetActiveEditor(Editor : TGridEditor);
    procedure GridClick(Sender : TObject); override;
    procedure SetGridSize; override;
    procedure CheckColWidths;
    procedure AdjustColWidths;
    procedure ScrollGrid;
    property ReadOnly;
  public
    constructor Create(AParent : TWinControl);
    destructor Destroy; override;

    procedure Execute; override;
    property ActiveEditor : TGridEditor read FActiveEditor write SetActiveEditor;
  end;

  TFieldWidthOrganizer = class(TObject)
  private
    FWidths : TIndexContainer;
    FDefaultWidth: Integer;
    function GetWidth(AIdx: TObject): Integer;
    procedure SetDefaultWidth(const Value: Integer);
    procedure SetWidth(AIdx: TObject; const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function HasWidth(AIdx : TObject; var AWidth : Integer) : Boolean;  overload;
    function HasWidth(AIdx : TObject) : Boolean;  overload;
    function RemoveWidth(AIdx : TObject; var AWidth : Integer) : Boolean;
    property Width[AIdx : TObject] : Integer read GetWidth write SetWidth;
    property DefaultWidth : Integer read FDefaultWidth write SetDefaultWidth;
  end;

  TGridEditor = class(TAbstractViewer, IIconSupplier, ICheckInfoSupplier, IMaster)
  private
    FTotalViewer : TTotalViewer;
    FShowSubTotals : Boolean;
    FShowStripes : Boolean;
    FExecuting : Boolean;
    FStandardView : TStandardView;
    FPageView : TPageView;
    FPreviousRow : Integer;
    FPreviousRowViewCol : Integer;
    FKeyFormatting : TDisplayValues;
    FSelectedDataRows : TDataRowList;
    FSelectedCols : TValueList;
    FAllSelected : Boolean;
    FUpdateType : TUpdateType;
    FRemovingActive : Boolean;
    FHideSubtotalOnOpen : Boolean;
    FHideSubtotalForSingle : Boolean;
    FRecursiveOnSingleSubtotal : Boolean;
    FPervoOpenRow : Boolean;
    FEditSubtotals : Boolean;
    FConstantBufferEdit : Boolean;
    FConstantBufferKey : TDataField;
    FBalanceRow : TAbstractRow;
    FEditorDecimalCount : Integer;

    FMarkRowPicture : TPicture;
    FDefaultMarkRowCalcField : TDataField;
    FMarkRowCalcField : TDataField;
    FShowMarkRowCalcField : Boolean;
    FEnableSorting : Boolean;
    FDialogBoxFields : TDataFieldSet;
    FDisabledFieldList : TDefaultValueFieldList;
    FOwnedDisabledFieldList : TDefaultValueFieldList;
    FCheckBoxFieldsObjects : TList;
    FSaveToAuxtableFields : TDataFieldSet;
    FAutoCreatedRows : TDataRowList;
    FAutoCreateRows : Boolean;
    FAutoCreatingRow : Boolean;
    FAutoDeleteRows : Boolean;
    FAllowMultipleAutoCreatedRows : Boolean;
    FSortField : TDataField;
    FDisableDataRow : TAbstractRow;

    FOnCellComboStringsFilling : TOnCellComboStringsFillingEvent;
    FOnDataRowChange : TNotifyEvent;
    FOnChangeValue : TOnChangeValueEvent;
    FOnChangingValue : TOnChangingValueEvent;
    FOnMarkRowColClickEvent : TOnMarkRowColClickEvent;
    FOnHeaderClickEvent : TOnHeaderClickEvent;
    FOnDeleteRowEvent : TOnDeleteRowEvent;
    FOnClick : TNotifyEvent;
    FOnDblClick : TNotifyEvent;
    FOnGetKeyFormatting : TOnGetKeyFormatting;
    FOnEditButtonClick : TOnEditButtonClickEvent;
    FOnGridKeyDown : TOnGridKeyDownEvent;
    FOnAutoCreateRow : TOnAutoCreateRowEvent;
//    FOnCellContentsChanges : TOnCellContentsChangesEvent;
    FOnGridHScroll : TOnScrollEvent;
    FOnGridVScroll : TOnScrollEvent;
    FOnGridCheckColWidths : TNotifyEvent;
    FOnGridCheckScroll : TNotifyEvent;
    FOnMouseDownEvent : TOnMouseDownEvent;
    FOnCreateCombo : TComboCellTypeCreateCombo;

    FCurrencyDivisor : Double;
    FCurrencyDecimalCount : Integer;
    FDecimalCount : Integer;

    FSlaveList : TInterfaceList;

    FHiddenColWidthOrganizer : TFieldWidthOrganizer;
    FHiddenKeys : TFieldList;

    procedure OnHEvent( Sender : TObject );
    procedure OnSEvent( Sender : TObject );

    function SubTotalLevelEditable(ARow: TSubTotalRow): Boolean;
    function CellCreateEditCustomizer( Sender : TDBUEditorCell; CellType : TDBUCellType ) : TDBUCustomizer;
    procedure ComboCellTypeCreateCombo( Cell : IEditorCell; Identifier : TObject;
      var ACombo : TDBUComboBox );
    function CellCreateDrawCustomizer( Sender : TDBUCell; CellType : TDBUCellType ) : TDBUCustomizer;
    procedure ComboCellTypeGetStrings( Cell : IEditorCell; Combos : TPickListOrganizer;
      var ACombo : TDBUComboBox );
    procedure ComboSelectionChange( Cell : IEditorCell; ACombo : TDBUComboBox; var SetValue : TValue );
    procedure CellHeaderClick( Sender : TObject; ACell : IInfoCell; AMouseState : TMouseState );

    // interface IIconSupplier
    procedure GetIcon(ACell : IInfoCell; var APicture : TPicture;
      var Align : TDBUAlign; var Margin : TDBUMargin );
    function HasIcon( ACell : IInfoCell ) : Boolean;
    // interface ICheckInfoSupplier
    procedure GetDescription( ACell : IInfoCell; var Descr : TValue );

    // interface IMaster
    procedure RegisterSlave(ASlave : ISlave);
    procedure UnregisterSlave(ASlave : ISlave);
    procedure ValueChanged(AField : TDataField);

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetRestoreForIllegalValues: Boolean;
    procedure SetRestoreForIllegalValues(const Value: Boolean);
    function GetAbortOnIllegalValues: Boolean;
    procedure SetAbortOnIllegalValues(const Value: Boolean);
  protected
    procedure CreateGrid; override;
    procedure CreateFormatter; override;
    procedure CreateCells; override;
    function CreateMarkRowCalcField : TDataField;
    function GetDataField(AGridRow, AGridCol : Integer) : TDataField; override;
    function GetRowViewDataField(ARowView : TAbstractRowView; AGridCol : Integer) : TDataField; override;
    function GetFieldIndexByGridIndex(idxGridCol : Integer) : Integer;
    function GetGridIndexByFieldIndex(idxField : Integer) : Integer;
    function GetGridIndexOfField(AField : TDataField) : Integer;
    procedure GetGridIndex(ARow : TAbstractRow; AField : TDataField; var iCol, iRow : Integer);
    function GetTotalColCount : Integer; override;
    function GetTotalRowCount : Integer; override;
    function GetDataRowCount : Integer;
    function GetAlwaysFixedColCount : Integer; override;
    function GetMarkRowColCount : Integer; override;
    function GetDataRowByGridIndex(idxGridRow : Integer) : TAbstractRow;
    function GetDataRowIndexByGridIndex(idxGridRow : Integer) : Integer;
    function GridIndexOfDataRow(DataRow : TAbstractRow) : Integer;
    function GetRowViewCount : Integer; override;
    function GetHeaderRowViewCount : Integer; override;
    function CellIsSelected(ACol, ARow : Integer) : Boolean; override;
    function ColumnIsReadOnly(AGridCol : Integer) : Boolean; virtual;
    {/** Insert a new row in the DataRow and in the grid at idxGridRow /*}
    function InsertRow(idxGridRow : Integer; InsertUnder, DuplicateRow : Boolean): TDataRow; virtual;
    function InsertSubtotalRow(idxGridRow : Integer; InsertUnder : Boolean; SubtotalKey : TDataField): TSubtotalRow; virtual;
    function GetCriteria : TCondition;
    function GetPasteConvertCriteria : TCondition;
    function ShowPasteDialog(DataFieldLists : TStringList; var OtherNonKeys : Boolean;
        PasteRules : TPasteRulesSet; var PasteRule : TPasteRules; {PastingRows, PastingSingle, ForecastEmpty : Boolean;} ARowCount : Integer ) : Boolean;
    function ShouldFieldBePasted(AField : TDataField; AvailableLists : TStringList; OtherNonKeys : Boolean) : Boolean; virtual;
    function IsOnlyOneColCopied(AEntry : TClipboardEntry) : Boolean; virtual;
    function MapPasteFields( SourceFields, FromFields, ToFields : TList; NewFieldIdx : Integer ) : Boolean; virtual;
    function MapField( AField : TDataField; FromFields, ToFields : TList; NewFieldIdx : Integer; var ErrorMessage : String ) : Boolean; virtual;
    function DoShowMarkRowCalcField : Boolean;
    function GetFieldKeyFormatting(AField : TDataField) : TDisplayValues;
    function GetKeyInThisTable(AField : TDataField; ATable : TDataTable) : Boolean;
    function GetShowIcon(ACol : Integer) : Boolean;
    function CheckRowHasLegalValues(DataRow : TDataRow; var ErrorStrings : TStringList) : Boolean;
    function IsRowLegal(DataRow : TAbstractRow) : Boolean;
    function DoCheckRowHasLegalValues(DataRow : TDataRow; var ErrorStrings : TStringList; BreakOnFalse : Boolean) : Boolean;
    function AcceptRowWithMessage(DataRow : TAbstractRow) : TPutResult; virtual;
    function UnacceptRow(DataRow : TDataRow) : TDataRow; virtual;
    function HasEditableKeys : Boolean;
    function GetRowStorage: TCustomRowStorage; override;
    procedure SetRowStorage(const Value  : TCustomRowStorage); override;

    function FieldIsReadOnlyByProperty(ADataField : TDataField; ADataRow : TAbstractRow; var IsAbsolute : Boolean) : Boolean; virtual;
    function KeyIsReadOnly(ADataField : TDataField; ADataRow : TAbstractRow) : Boolean; virtual;
    function NonKeyIsReadOnly(ADataField : TDataField; ADataRow : TAbstractRow) : Boolean; virtual;
    function OtherFieldIsReadOnly(ADataField : TDataField; ADataRow : TAbstractRow) : Boolean; virtual;
    function SubTotalKeyIsReadOnly(ADataField : TDataField; ADataRow : TSubTotalRow) : Boolean; virtual;
    function SubTotalNonKeyIsReadOnly(ADataField : TDataField; ADataRow : TSubTotalRow) : Boolean; virtual;
    function SubTotalOtherFieldIsReadOnly(ADataField : TDataField; ADataRow : TSubTotalRow) : Boolean; virtual;
    function DataRowKeyIsReadOnly(ADataField : TDataField; ADataRow : TDataRow) : Boolean; virtual;
    function DataRowNonKeyIsReadOnly(ADataField : TDataField; ADataRow : TDataRow) : Boolean; virtual;
    function DataRowOtherFieldIsReadOnly(ADataField : TDataField; ADataRow : TDataRow) : Boolean; virtual;

    function GetActiveRowViewCol : Integer;
    function GetIndexOfSubTotalLevel(AField : TDataField) : Integer;
    function GetParentSubtotalsVisible(ADataRow : TAbstractRow) : Boolean;
    function GetContainsSelection : Boolean;
    function GetContainsGridSelection : Boolean;
    procedure GetTotalSelection( SelList : TList ); override;
    function GetBufferRow(ARow : TAbstractRow) : TDataRow;
    function GetLastDetail(ASubRow : TSubTotalRow) : TDataRow;
    function DoAcceptRow(DataRow : TAbstractRow) : TPutResult;
    function GetDisabledFieldList : TDefaultValueFieldList;
    procedure SetDisabledFieldList(AList : TDefaultValueFieldList);
    function FieldBelongsToTable(AField : TDataField; ARow : TAbstractRow) : Boolean; override;
    procedure FillHiddenKeys(AKeyList: TFieldList);

    function GetPageView : TPageView; override;
    procedure SetPageView(PageView : TPageView);
    function GetActiveDataRow : TAbstractRow; virtual;
    procedure SetActiveDataRow(DataRow : TAbstractRow); virtual;
    function GetActiveRow : Integer; virtual;
    procedure SetActiveRow(Value : Integer); virtual;
    function GetActiveField : TDataField;
    procedure SetActiveField( AField : TDataField );
    function GetDataRowSelected : Boolean;
    procedure SetDataRowSelected(Value : Boolean); virtual;
    function GetColSelected : Boolean;
    procedure SetColSelected(Value : Boolean); virtual;
    function GetPreviousDataRow : TAbstractRow;
    procedure SetPreviousDataRow(ARow : TAbstractRow);
    function GetEditorDecimalCount : Integer; override;
    procedure SetEditorDecimalCount(Value : Integer); virtual;
    function GetCurrencyDecimalCount : Integer; override;
    procedure SetCurrencyDecimalCount(Value : Integer); virtual;
    function GetDivisor(AField: TDataField; ARow: TAbstractRow): Double; override;
    function GetCurrencyDivisor: Double; override;
    procedure SetCurrencyDivisor(Value : Double); virtual;
    function GetDecimalCount : Integer; override;
    procedure SetDecimalCount(Value : Integer); virtual;
    function GetUsesDefaultSort : Boolean;
    function GetReadOnly : Boolean; override;
    function GetShowStripes : Boolean; override;
    procedure SetShowStripes(Value : Boolean); virtual;

    procedure SetInitialValuesToRow(ADataRow : TAbstractRow); virtual;
//    procedure SetInitialValuesToDataRow(ADataRow : TDataRow); virtual;
    procedure SetDefaultValuesToDataRow(ADataRow : TDataRow); virtual;
    procedure SetAllSelected(Value : Boolean);
    procedure UnselectAll;
    procedure SelectAllColsButOne(idx : Integer);
    procedure SelectAllDataRowsButOne(ARow : TAbstractRow);
    procedure SetTotalViewer(Viewer : TTotalViewer);
    procedure SetShowSubTotals(Value : Boolean);
    procedure SetIsActive(Value : Boolean); override;
    procedure SetMarkRowCalcField(AField : TDataField); virtual;
    procedure SetShowMarkRowCalcField(Value : Boolean); virtual;
    procedure SetEnableSorting(Value : Boolean);
    procedure AdjustFixedCols;
    procedure DoSortByFieldAndOrder(AField : TDataField; ASortOrder : TSortOrder); virtual;
    procedure DoShowDetailOnly;
    procedure DoDeleteRow(ARow : TAbstractRow); virtual;
    procedure ResetRowForBuffer(ARow : TDataRow);
    function BalanceBuffer(ARow : TAbstractRow; AField : TDataField) : Boolean;
    procedure MoveDiffToBuffer(OldRow, NewRow : TAbstractRow; Buffer : TDataRow);
    procedure SetBalanceRow( ARow : TAbstractRow);
    procedure GetDataRowsForRect(ASelRect : TRect; Rows : TStrings; ReverseRowOrder : Boolean );
    procedure CopyDataToRow( SrcRow, DestRow : TDataRow ); virtual;
    procedure SetKilling( Value : Boolean ); override;

    {/** Repaint ACol **/}
    procedure RepaintCol(ACol : Integer);
    {/** Repaint ARow **/}
    procedure RepaintRow(ARow : Integer);
    {/** Repaint the ACol and ARow  **/}
    procedure RepaintCross(ACol, ARow : Integer);
    procedure RepaintCell(ACol, ARow : Integer);
    procedure SetKeyFormatting(Value : TDisplayValues);
    procedure UpdateTotals;
    procedure UpdateTotalsSize;
    procedure CheckTotalViewerScroll;
    procedure CheckTotalViewerColWidths;
    procedure InsertDataRow(ARow : TDataRow; idxNewRow : Integer);
    procedure FillCopyListWithFields(ATable : TDataTable; AList : TList; UIList : TList); virtual;
    procedure GetInfoOfCopyEntry(AEntry : TClipboardEntry; var WithinForecast, ContainsRows, OnlyOne : Boolean);
    procedure CopyLegalValuesBetweenDataRows(AEntry : TClipboardEntry; DestRow, SourceRow : TAbstractRow;
              AvailableLists : TStringList; OtherNonKeys : Boolean; PasteRule : TPasteRules);
    procedure CopyLegalValuesBetweenCols(PasteRows : TStringList; AvailableLists : TStringList; ToFields : TList;
              PasteRule : TPasteRules; OtherNonKeys : Boolean);

    procedure CopyGridSelectionToClipboard;
    procedure CopyGridFormattedSelectionToClipboard(AFormat: UINT; AKeyFormat: TDisplayValues);

    procedure CopySelectionToClipboard;
    procedure CopyFormattedSelectionToClipboard(AFormat: UINT; AKeyFormat: TDisplayValues);

    procedure PutStringToClipboard(AString : String);
    procedure PutFormattedStringToClipboard(AFormat: UINT; AString: String);

    function GetSelectionAsString(ARect : TRect; NewLine, Tab : String) : String;
    function GetSelectionAsFormattedString(ARect : TRect; AKeyFormatting : TDisplayValues; NewLine, Tab : String) : String;
    procedure PasteSingleRow(AEntry : TClipboardEntry);
    procedure PasteManyRows(AEntry : TClipboardEntry);
    procedure PasteCols(AEntry : TClipboardEntry);
    procedure PasteColWithinForecast(AEntry : TClipboardEntry);
    procedure PasteLegalValues(AEntry : TClipboardEntry; PasteRows, AvailableLists : TStringList;
              PasteRule : TPasteRules; OtherNonKeys : Boolean);

    procedure PasteText( AFormat : UINT );
    procedure PasteFromEntry(AEntry : TClipboardEntry);

    procedure GetAvailableLists(AvailableLists : TStringList); virtual;
    procedure ResetValuesNotToBePasted(AEntry : TClipboardEntry; ARow : TDataRow;
              AvailableLists : TStringList; OtherNonKeys : Boolean); virtual;
    procedure ArrangeRows; override;

    procedure GridClick(Sender : TObject); override;
    procedure GridDblClick(Sender : TObject); override;
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;


    procedure GridGetCell( Sender : IDataEditorCellInterface ); override;
    procedure GridGetTotalCell( Sender : IDataEditorCellInterface );
    procedure GridSetCell( Sender : IDataEditorCellInterface ); override;
    procedure DefineCell( Cell : IDataEditorCellInterface ); override;

    procedure GridHScroll(Sender: TObject; ScrollCode,
              Pos: Smallint);
    procedure GridVScroll(Sender: TObject; ScrollCode,
              Pos: Smallint);
    procedure CellGetStrings(Sender: TObject; ACombo : TDBUComboBox; DataField : TDataField; DataRow : TAbstractRow);
    procedure CellButtonClick( Sender : TObject; ACell : IInfoCell; AMouseState : TMouseState; var Value : TValue );
    procedure CellSetItemIndex(Sender : TObject; SetResult : TSetResult);
//    procedure CellContentsChanges(Sender: TObject; ACol, ARow: Longint; const Value: String);
    procedure CellSetValue(Sender : TObject; SetResult : TSetResult);
    procedure CellSettingValue(Sender: TObject; var Value : TValue; var Reject, RunInherited : Boolean;
      var RejectParams : TRejectParams);
//    procedure CellSettingValue(Sender: TObject; var Value : TValue; var Reject : Boolean; var RejectReason : String; var RunInherited : Boolean);
    procedure CellGetIcon(Sender: TObject; APicture : TPicture; var ShowIcon : Boolean);
    procedure CellOverWritingRow(Sender: TObject; ADataRow : TAbstractRow; var OverWritingRow : Boolean);
    procedure CellOverWritedRow(Sender : TObject);
    procedure SetValueByCell(ARow : TAbstractRow; AField : TDataField; AValue : TValue);
    function GetValueByCell(ARow : TAbstractRow; AField : TDataField) : TValue;
    procedure SetValue(ARow : TAbstractRow; AField : TDataField; AValue : TValue);
    function GetValue(ARow : TAbstractRow; AField : TDataField) : TValue;

    function RowIsSubtotal(ARow : TAbstractRow) : Boolean; virtual;
    function RowSubRowCount(ARow : TAbstractRow) : Integer; virtual;
    function RowSubRows( ARow : TAbstractRow; Index : Integer ) : TAbstractRow; virtual;
    function RowSubTotalRow( ARow : TAbstractRow ) : TAbstractRow; virtual;

    function StorageCloseRow( ARow : TAbstractRow ) : TAbstractRow; virtual;
    procedure StorageOpenRow( ARow : TAbstractRow ); virtual;

    procedure SetString(ARow : TAbstractRow; AField : TDataField; AString : String);

    procedure ChangeSelection;
    procedure ChangeDataRow;
    procedure ChangeDataRowEvents;
    procedure ExitKeys;
    procedure ChangeValue(Cell : TDataInplaceEditorCell; SetResult : TSetResult; ChangeActiveDataRow : Boolean); virtual;
    procedure ChangingValue(Cell : TDataInplaceEditorCell; var Value : TValue;
      var Reject, RunInherited : Boolean; var RejectParams : TRejectParams); virtual;
    procedure SetFixedCols(Value : Integer); override;
    procedure ResizeCols(ResultList : TStringList); virtual;

    property Criteria : TCondition read GetCriteria;
    property PasteConvertCriteria : TCondition read GetPasteConvertCriteria;
    property FieldIndexByGridIndex[idxGridCol : Integer] : Integer read GetFieldIndexByGridIndex;
    property GridIndexByFieldIndex[idxField : Integer] : Integer read GetGridIndexByFieldIndex;

    property DataRowIndexByGridIndex[idxGridRow : Integer] : Integer read GetDataRowIndexByGridIndex;
    property PreviousDataRow : TAbstractRow read GetPreviousDataRow write SetPreviousDataRow;
    property PreviousRow : Integer read FPreviousRow write FPreviousRow;
    property ActiveRowViewCol : Integer read GetActiveRowViewCol;
    property PreviousRowViewCol : Integer read FPreviousRowViewCol write FPreviousRowViewCol;
    property Executing : Boolean read FExecuting write FExecuting;
    property SelectedDataRows : TDataRowList read FSelectedDataRows;
    property SelectedCols : TValueList read FSelectedCols;
    property ContainsSelection : Boolean read GetContainsSelection;
    property ContainsGridSelection : Boolean read GetContainsGridSelection;
    property RemovingActive : Boolean read FRemovingActive write FRemovingActive;
    property KeyInThisTable[AField : TDataField; ATable : TDataTable] : Boolean read GetKeyInThisTable;
    property IndexOfSubTotalLevel[AField : TDataField] : Integer read GetIndexOfSubTotalLevel;
    property ParentSubtotalsVisible[ADataRow : TAbstractRow] : Boolean read GetParentSubtotalsVisible;
    property DialogBoxFields : TDataFieldSet read FDialogBoxFields;
    property SaveToAuxtableFields : TDataFieldSet read FSaveToAuxtableFields;
    property BalanceRow : TAbstractRow read FBalanceRow write SetBalanceRow;
    property AutoCreatedRows : TDataRowList read FAutoCreatedRows;
    property SortField : TDataField read FSortField;
    procedure AutoCreateRow;
    procedure DeleteAutoCreatedRows;
    function GetVisible : Boolean;
    procedure SetVisible( AValue : Boolean );
    procedure SetGridColWidth(ACol, AValue : Integer);

    // Does the InplaceEditor have focus in the grid, so that it should hanlde the copy paste?
{ifndef TRANSLATOR}
    function InplaceCopyPaste : Boolean;
{endif TRANSLATOR}
  public
    constructor Create(AParent : TWinControl);
    destructor Destroy; override;

    procedure Disable; override;
    procedure Enable; override;
    procedure Initialize(RowStorage : TCustomRowStorage; StandardView : TStandardView; PageView : TPageView;
              ShowSubTotals, ReadOnly, ShowStripes : Boolean); virtual;
    procedure Execute; override;

    function NewRow : TDataRow; virtual;
    function CanAddNewRow : Boolean; virtual;
    function NewSubTotal : TSubTotalRow; virtual;
    function CanAddNewSubTotal : Boolean; virtual;
    function DuplicateRow : TDataRow; virtual;
    function CanDuplicateRow : Boolean; virtual;
    procedure DeleteRows ; virtual;
    function CanDeleteRow(ADataRow : TAbstractRow) : Boolean; virtual;
    function CanDeleteRows : Boolean; virtual;
    function AcceptUnacceptedRows : Boolean;
    function AcceptRow(DataRow : TAbstractRow) : TPutResult; virtual;

    function CanEditValue(ARow : TAbstractRow; ADataField : TDataField; var ReadOnlyReason : String) : Boolean;

    // LAA: moved from protected
    function FieldIsReadOnly(ADataField : TDataField; ADataRow : TAbstractRow) : Boolean; virtual;
    function DataRowIsReadOnly(ADataRow : TAbstractRow) : Boolean; virtual;

    {/** Lock the active column and all before it */}
    procedure LockToColumn;
    {/** Check if it is possible to lock the active column and all before it */}
    function CanLockToCol : Boolean;
    {/** Unlock all columns */}
    procedure UnlockColumns;
    {/** Check if it is possible to unlock columns */}
    function CanUnlockCols : Boolean;
    {/** Check if it is possible to fix 'Value' number of columns */}
    function CanFixCols(Value : Integer) : Boolean; override;

    {/** Sort the DataRows in the RowStorage */}
    procedure SortRows;
    {/** Sort the DataRows in the RowStorage in the default order*/}
    procedure DefaultSortRows;
    {/** Sort the DataRows in the RowStorage according to AField*/}
    procedure SortByField(AField : TDataField);
    {/** Sort the DataRows in the RowStorage according to AField in the given order*/}
    procedure SortByFieldAndOrder(AField : TDataField; ASortOrder : TSortOrder);

    {/** Close the active DataRow */}
    function CloseRow : TAbstractRow; virtual;//overload;
    procedure CloseSelectedRows; virtual;
    function CloseRowOL(ARow : TAbstractRow) : TAbstractRow; virtual;//overload;
    {/** Can the active DataRow be closed */}
    function CanCloseRow : Boolean; virtual;//overload;
    function CanCloseRowOL(ARow : TAbstractRow) : Boolean; virtual;//overload;
    {/** Open the active DataRow */}
    procedure OpenRow; virtual;//overload;
    procedure OpenSelectedRows; virtual;
    procedure OpenRowOL(ARow : TAbstractRow); virtual;//overload;
    procedure OpenAllRows; virtual;
    function CanOpenAllRows : Boolean; virtual;
    {/** Can the active DataRow be opened */}
    function CanOpenRow : Boolean; virtual;//overload;
    function CanOpenRowOL(ARow : TAbstractRow) : Boolean; virtual;//overload;
    {/** Hide the active DataRow */}
    procedure HideRow; virtual;
    {/** Can the active DataRow be hidden */}
    function CanHideRow : Boolean; virtual;
    {/** Hide a subtotal when it is opened? */}
    property HideSubtotalOnOpen : Boolean read FHideSubtotalOnOpen write FHideSubtotalOnOpen;
    {/** Hide a subtotal when it is opened if it contains only one row? */}
    property HideSubtotalForSingle : Boolean read FHideSubtotalForSingle write FHideSubtotalForSingle;
    {/** Open/close subtotals recursively if there is only one single subrow */}
    property RecursiveOnSingleSubtotal : Boolean read FRecursiveOnSingleSubtotal write FRecursiveOnSingleSubtotal;

    procedure ShowSubTotalLevel(AField : TDataField);
    procedure HideSubTotalLevel(AField : TDataField);
    function SubTotalLevelVisible(AField : TDataField) : Boolean;
    procedure ShowHighestLevelOnly;
    procedure ShowSubTotalLevelOnly(AField : TDataField);
    procedure ShowDetailOnly;
    procedure GetColumnHierarchyFields( GridCol : Integer; ASet : TFieldList );
    function ShowingTotals : Boolean;

    function CanHideField(AField : TDataField) : Boolean;
    procedure HideField(AField : TDataField);
    function CanShowField(AField : TDataField) : Boolean;
    procedure ShowField(AField : TDataField);

    procedure CommitChanges;
    {/** Repaint all rows in the grid belonging to ADataRow **/}
    procedure RepaintDataRow(ADataRow : TAbstractRow);
    procedure RepaintActiveMark;
    procedure InvalidateGrid; override;
    procedure DefaultColWidths(Value : Integer); virtual;
    procedure OptimizeColWidths; virtual;
    procedure MinimizeColWidths; virtual;
    procedure Cut(AClipboard : TDataClipBoard); virtual;
    procedure Copy(AClipboard : TDataClipBoard); virtual;
    procedure Paste(AClipboard : TDataClipBoard); virtual;
    procedure Delete; virtual;
    procedure CheckTotalGridCorrelation;
    {/** Unaccept all DataRows with illegal key combinations*/}
    procedure UnacceptIllegalRows;
    procedure SetDialogBoxFields(Fields : TDataFieldSet; OnEditButtonClick : TOnEditButtonClickEvent);
    procedure SetSaveToAuxTableFields(Fields : TDataFieldSet);
    procedure GetSelectedDataRows( Rows : TStrings; ReverseRowOrder : Boolean );
    procedure GetDataRowsInSelections(Rows: TStrings; Selections: TList; ReverseRowOrder : Boolean);
    procedure Hide;
    procedure Show;
    procedure SetActiveRowField( RowField : TRowFieldRecord );
    procedure SetGridSize; override;

    //ivt
    property GridIndexOfField[AField : TDataField] : Integer read GetGridIndexOfField;

    property StandardView : TStandardView read FStandardView write FStandardView;
    property RowViewIndexByGridIndex;
    property Grid;
    property TotalViewer : TTotalViewer read FTotalViewer write SetTotalViewer;
    property RowStorage;
    property ActiveDataRow : TAbstractRow read GetActiveDataRow write SetActiveDataRow;
    property ActiveRow : Integer read GetActiveRow write SetActiveRow;
    property ActiveField : TDataField read GetActiveField write SetActiveField;
    property KeyFormatting : TDisplayValues read FKeyFormatting write SetKeyFormatting;
    property ShowSubTotals : Boolean read FShowSubTotals write SetShowSubTotals;
    property ShowStripes write SetShowStripes;
    property UsesDefaultSort : Boolean read GetUsesDefaultSort;
    property UseNegativeColor;
    property ReadOnly;
    property IsActive;
    property DisabledFieldList : TDefaultValueFieldList read GetDisabledFieldList write SetDisabledFieldList;
    property ActiveDataRowSelected : Boolean read GetDataRowSelected write SetDataRowSelected;
    {/** Is it allowed to select this DataRow */}
    function CanSelectDataRow : Boolean;
    property ActiveColSelected : Boolean read GetColSelected write SetColSelected;
    {/** Is it allowed to select this column */}
    function CanSelectCol : Boolean;
    property AllSelected : Boolean read FAllSelected write SetAllSelected;
    property UpdateType : TUpdateType read FUpdateType write FUpdateType;
    property RowViewDataField;
    property MarkRowCalcField : TDataField read FMarkRowCalcField write SetMarkRowCalcField;
    property ShowMarkRowCalcField : Boolean read FShowMarkRowCalcField write SetShowMarkRowCalcField;
    property TotalColCount;
    property TotalRowCount;
    property DataRowCount : Integer read GetDataRowCount;
    property AlwaysFixedColCount;
    property FixedCols;
    property HeaderRowViewCount;
    property RowViewCount;
    property DataRowByGridIndex[idxGridRow : Integer] : TAbstractRow read GetDataRowByGridIndex;
    property EditorCell;
    property EnableSorting : Boolean read FEnableSorting write SetEnableSorting;
    property ConstantBufferEdit : Boolean read FConstantBufferEdit write FConstantBufferEdit;
    property ConstantBufferKey : TDataField read FConstantBufferKey write FConstantBufferKey;
    property EditSubtotals : Boolean read FEditSubtotals write FEditSubtotals;
    property AutoCreateRows : Boolean read FAutoCreateRows write FAutoCreateRows;
    property AutoDeleteRows : Boolean read FAutoDeleteRows write FAutoDeleteRows;
    property AllowMultipleAutoCreatedRows : Boolean read FAllowMultipleAutoCreatedRows write FAllowMultipleAutoCreatedRows;
    property EditorDecimalCount write SetEditorDecimalCount;
    property Visible : Boolean read GetVisible write SetVisible;

    property CurrencyDivisor : Double read FCurrencyDivisor write SetCurrencyDivisor;
    property CurrencyDecimalCount write SetCurrencyDecimalCount;
    property DecimalCount write SetDecimalCount;
    property RestoreForIllegalValues : Boolean read GetRestoreForIllegalValues write SetRestoreForIllegalValues;
    property AbortOnIllegalValues : Boolean read GetAbortOnIllegalValues write SetAbortOnIllegalValues;

    property OnCellComboStringsFilling : TOnCellComboStringsFillingEvent read FOnCellComboStringsFilling write FOnCellComboStringsFilling;
    property OnDefineCell;
    property OnDataRowChange : TNotifyEvent read FOnDataRowChange write FOnDataRowChange;
    property OnChangeValue : TOnChangeValueEvent read FOnChangeValue write FOnChangeValue;
    property OnChangingValue : TOnChangingValueEvent read FOnChangingValue write FOnChangingValue;
    property OnMarkRowColClickEvent : TOnMarkRowColClickEvent read FOnMarkRowColClickEvent write FOnMarkRowColClickEvent;
    property OnHeaderClickEvent : TOnHeaderClickEvent read FOnHeaderClickEvent write FOnHeaderClickEvent;
    property OnDeleteRow : TOnDeleteRowEvent read FOnDeleteRowEvent write FOnDeleteRowEvent;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick : TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnGetKeyFormatting : TOnGetKeyFormatting read FOnGetKeyFormatting write FOnGetKeyFormatting;
    property OnEditButtonClick : TOnEditButtonClickEvent read FOnEditButtonClick write FOnEditButtonClick;
    property OnGridKeyDown : TOnGridKeyDownEvent read FOnGridKeyDown write FOnGridKeyDown;
    property OnAutoCreateRowEvent : TOnAutoCreateRowEvent read FOnAutoCreateRow write FOnAutoCreateRow;
//    property OnCellContentsChangesEvent : TOnCellContentsChangesEvent read FOnCellContentsChanges write FOnCellContentsChanges;
    property OnGridHScroll : TOnScrollEvent read FOnGridHScroll write FOnGridHScroll;
    property OnGridVScroll : TOnScrollEvent read FOnGridVScroll write FOnGridVScroll;
    property OnGridCheckColWidths : TNotifyEvent read FOnGridCheckColWidths write FOnGridCheckColWidths;
    property OnGridCheckScroll : TNotifyEvent read FOnGridCheckScroll write FOnGridCheckScroll;
    property OnMouseDownEvent : TOnMouseDownEvent read FOnMouseDownEvent write FOnMouseDownEvent;
    property OnCreateCombo : TComboCellTypeCreateCombo read FOnCreateCombo write FOnCreateCombo;
  end;

  TEditorPictureField = class(TPictureField)
  private
    FEditor : TGridEditor;
  public
    constructor CreateOld(FieldName : String; AEditor : TGridEditor; APicture : TPicture);

    function CalcValue(ARow : TAbstractRow) : TValue; override;
  end;

  TCheckBoxFieldObject = class
  private
    FOnGetText : TOnGetTextEvent;
    FDataField : TDataField;
  public
    constructor Create(AField : TDataField; AOnGetText : TOnGetTextEvent);

    property OnGetText : TOnGetTextEvent read FOnGetText write FOnGetText;
    property DataField : TDataField read FDataField;
  end;

  TPasteRectObj = class
  private
    FPasteRows : TDataRowList;
    FToRect : TRect;
    FValues : TList;
    FGridEditor : TGridEditor;
  public
    constructor Create( AGridEditor : TGridEditor; APasteRowsList : TDataRowList;
      AToRect : TRect; AValuesList : TList );

    procedure PasteToRect( AUnacceptList : TDataRowList );
  end;

  TDBUDataCombo = class( TDBUComboBox )
  private
    FFillCriteria: TCondition;
    FFillTimeStamp: TDateTime;
    procedure SetFillCriteria(const Value: TCondition);
    procedure SetFillTimeStamp(const Value: TDateTime);
  public
    constructor Create;

    property FillTimeStamp : TDateTime read FFillTimeStamp write SetFillTimeStamp;
    property FillCriteria : TCondition read FFillCriteria write SetFillCriteria;
    destructor Destroy; override;
  end;


implementation


uses
{$ifndef LINUX}
  Dialogs, Forms, ComCtrls, StdCtrls, Grids,
{$else LINUX}
  QDialogs, QForms, QComCtrls, QStdCtrls, QGrids,
{$endif LINUX}



  SysUtils, Math, ImageNames,
  Criteria, CalcField, ClipboardInterface, DataTranslations,
  PasteDialog, DerivedDataType, DataEditorConstants, GridEditorProperties;

{$ifndef LINUX}
  {$R GridIcons.RES}
  {$R SavePrint.RES}
  {$R CutCopyPaste.RES}
  {$R RowCol.RES}
{$endif LINUX}

const
  MINWIDTH_NumCol = 40;
  COLWIDTH_MarkCol = 18;

function TableHasKeyOrLookup( ATable : TDataTable; AField : TDataField ) : Boolean;
begin
  Result := Assigned( AField ) and ATable.TableHasKey(AField);
  if not Result then
    Result := Assigned( AField.LookupField ) and ATable.TableHasKey(AField.LookupField);
end;

{ TAbstractViewer }

constructor TAbstractViewer.Create(AParent : TWinControl);
begin
  inherited Create;

  FUseNegativeColor := True;
  FFreeCell := False;
  FFreeEditorCell := False;

  FIsActive := False;
  CreateFormatter;
  FKilling := False;
  FParent := AParent;
  FDisableCount := 0;
  FArrangeOnEnable := False;
  FFocused := True;
  FShowBooleansAsCheckBoxes := False;
  FGridIterator := nil;
  FAntFrameDrawer := nil;
end;

procedure TAbstractViewer.Execute;
begin
  CreateCells;
  CreateGrid;
end;

destructor TAbstractViewer.Destroy;
begin
  HideAnts;

  if not Killing then
    Killing := True;

  inherited Destroy;

{$ifdef D4_OR_HIGHER}
  FreeAndNil( FGrid );

  if FFreeCell then
    FreeAndNil( FCell );

  if FFreeEditorCell then
    FreeAndNil( FSetValueCell );

  if FFreeCellFormatter then
    FreeAndNil( FFormatter );
  if FFreeEditorCellFormatter then
    FreeAndNil( FSetValueFormatter );
{$else}
  fGrid.Free;
  if FFreeCell then
    FCell.Free;
  if FFreeEditorCell then
    FSetValueCell.Free;
  if FFreeCellFormatter then
    FFormatter.Free;
  if FFreeEditorCellFormatter then
    FSetValueFormatter.Free;
{$endif D4_OR_HIGHER}
end;

procedure TAbstractViewer.SetKilling( Value : Boolean );
begin
  try
    FKilling := Value;
    Grid.Killing := Value;
    if Value then
      Disable
    else
      Enable;
  except
  end;
end;

function TAbstractViewer.GetGridCol : Integer;
begin
  Result := Grid.Col;
end;

procedure TAbstractViewer.SetGridCol(Value : Integer);
begin
  Grid.Col := Min(Max(Value, AlwaysFixedColCount), Grid.ColCount-1);
end;

function TAbstractViewer.GetGridRow : Integer;
begin
  Result := Grid.Row;
end;

procedure TAbstractViewer.SetGridRow(Value : Integer);
var
  NewRow : Integer;
begin
  NewRow := Min(Max(Value, HeaderRowViewCount), Grid.RowCount-1);

  if Grid.Showing then
  begin
    if (Grid.TopRow > NewRow) or (Grid.TopRow + Grid.VisibleRowCount <= NewRow) then
      Grid.TopRow := Max(HeaderRowViewCount, NewRow - Grid.VisibleRowCount div 2);
  end;

  Grid.Row := NewRow;
end;

procedure TAbstractViewer.CreateGrid;
begin
  FGrid := TDBUGrid.Create(Parent);
  FGrid.OnCreateCell := GridCreateCell;
  FGrid.OnCreateEditorCell := GridCreateEditorCell;
  FGrid.OnCreateFormatter := GridCreateFormatter;
  FGrid.OnCreateEditorFormatter := GridCreateEditorFormatter;
  FCell.Grid := Grid;
  FSetValueCell.Grid := Grid;
  FGrid.Parent := Parent;
end;

procedure TAbstractViewer.CreateCells;
begin
  FFreeCell := True;
  FFreeEditorCell := True;
  FCell := TDataEditorCell.Create; // dessa ser lite ut att läcka enligt slöten
  FCell.OnDefineCell := GridGetCell;
  FSetValueCell :=  TDataInplaceEditorCell.Create;
  FSetValueCell.OnDefineCell := GridSetCell;
end;

procedure TAbstractViewer.CreateFormatter;
begin
  FFreeCellFormatter := True;
  FFreeEditorCellFormatter := True;
  FFormatter := TDataEditorCellFormatter.Create;
  FSetValueFormatter := TDataEditorCellFormatter.Create;
end;

function TAbstractViewer.GridCreateCell( Sender: TObject ) : TDBUCustomGridCell;
begin
  FFreeCell := False;
  Result := EditorCell;
end;

function TAbstractViewer.GridCreateFormatter( Sender: TObject ) : TDBUFormatter;
begin
  FFreeCellFormatter := False;
  Result := CellFormatter;
end;

function TAbstractViewer.GridCreateEditorCell( Sender: TObject ) : TDBUCustomEditorCell;
begin
  FFreeEditorCell := False;
  Result := SetValueCell;
end;

function TAbstractViewer.GridCreateEditorFormatter( Sender: TObject ) : TDBUFormatter;
begin
  FFreeEditorCellFormatter := False;
  Result := SetValueFormatter;
end;

function TAbstractViewer.GetGridIterator : IGridIterator;
begin
  if not Assigned(FGridIterator) then
    FGridIterator := EditorCell.CreateIterator;
  Result := FGridIterator;
end;

function TAbstractViewer.GetEditorIterator : IEditorIterator;
begin
  if not Assigned(FEditorIterator) then
    FEditorIterator :=  SetValueCell.CreateIterator;
  Result := FEditorIterator;
end;

procedure TAbstractViewer.Refresh;
begin
  if Grid.Showing then
    Grid.Refresh;
end;

procedure TAbstractViewer.InvalidateGrid;
begin
  if Grid.Showing then
    Grid.Invalidate;
end;

function TAbstractViewer.GetEnabled : Boolean;
begin
  Result := DisableCount = 0;// FEnabled;
end;

procedure TAbstractViewer.SetEnabled(Value : Boolean);
begin
  if Value then
    Enable
  else
    Disable;
end;

procedure TAbstractViewer.FlushCells;
begin
  if Grid.DBUInplaceEdit <> nil then
    Grid.DBUInplaceEdit.Flush;
  EditorCell.Reset;
end;

procedure TAbstractViewer.Disable;
begin
  try
    Inc(FDisableCount);

    if DisableCount > 1 then
      Exit;

    FArrangeOnEnable := False;
    Screen.Cursor := crHourglass;
    if Grid <> nil then
    begin
      FFocused := Grid.Focused;
      if FFocused and
         Parent.CanFocus and
         Parent.Showing then
        Parent.SetFocus;
        Grid.Enabled := False;
        FlushCells;
    end;
  except
    Enable;
    Abort;
  end;
end;

procedure TAbstractViewer.Enable;

  function ComponentShowing( AComp : TWinControl ) : Boolean;
  begin
    repeat
      Result := AComp.Showing;
      if AComp is TCustomForm then
        Break;
      AComp := AComp.Parent;
    until ( (AComp = nil) or not Result );
  end;

begin
  Dec(FDisableCount);

  if DisableCount > 0 then
    Exit
  else
    FDisableCount := 0;

  if Grid <> nil then
  begin
    if FArrangeOnEnable then
    begin
      FArrangeOnEnable := False;
      ArrangeRows;
    end;

    SetGridSize;
    FlushCells;
    Grid.Enabled := True;

    if IsActive and
       ComponentShowing( Grid ) then
    begin
      InvalidateGrid;
      if Grid.CanFocus and
         FFocused then
        Grid.SetFocus ;
    end;
  end;

  Screen.Cursor := crDefault;
end;

procedure TAbstractViewer.EnableWithCoord(iCol, iRow : Integer);
begin
  if DisableCount <= 1 then
  begin
    SetGridSize;
    if iCol <> -1 then
      GridCol := iCol;
    if iRow <> -1 then
      GridRow := iRow;
  end;

  Enable;
end;

function TAbstractViewer.CellIsSelected(ACol, ARow : Integer) : Boolean;
begin
  Result := False;
end;

function TAbstractViewer.GetFixedCols : Integer;
begin
  Result := FFixedCols;
end;

procedure TAbstractViewer.SetFixedCols(Value : Integer);
var
  ActiveCol, ActiveRow : Integer;
begin
  if CanFixCols(Value) then
  begin
    Disable;
    try
      ActiveCol := Grid.Col;
      ActiveRow := Grid.Row;

      Grid.FixedCols := MinIntValue([MaxIntValue([Value, AlwaysFixedColCount]), Grid.ColCount -1]);
      FFixedCols := Grid.FixedCols;
      GridRow := ActiveRow;
      EnableWithCoord(MinIntValue([MaxIntValue([FFixedCols, ActiveCol]), Grid.ColCount -1]), -1);
    except
      Enable;
    end;
  end;
end;

function TAbstractViewer.CanFixCols(Value : Integer) : Boolean;
var
  FixedColsWidth, iCol : Integer;
begin
  Result := ( (Value <= Grid.ColCount) and
              (Value >= AlwaysFixedColCount) ) or
            ( (Value = 0) and (FixedCols <> AlwaysFixedColCount) );
  if Result then
  begin
    FixedColsWidth := 0;
    for iCol := 0 to MinIntValue([Value, Grid.ColCount -1]) do
      FixedColsWidth := FixedColsWidth + Grid.ColWidths[iCol];
    Result := FixedColsWidth < Grid.Width;
  end;
end;

function TAbstractViewer.DefineCellAlignment(DataField : TDataField) : TAlignment;
begin
  if Assigned(DataField) and
     ( (DataField.DataType is TPictureType) or
       (DataField.DisplayValues = dvKeyOnly)) then
      Result := DataField.DataType.Alignment
  else
    Result := taLeftJustify;
end;

procedure TAbstractViewer.GridGetCell( Sender : IDataEditorCellInterface );
begin
  DefineCell( Sender );
  DefineDecimalCountAndDivisor( Sender );
  DefineCellLook(Sender);
end;

procedure TAbstractViewer.GridSetCell( Sender : IDataEditorCellInterface );
begin
  DefineCell( Sender );

  DefineDecimalCountAndDivisor( Sender );
end;

function TAbstractViewer.GetShowAllBooleansAsCheckBoxes: Boolean;
begin
  Result := FShowBooleansAsCheckBoxes;
end;

procedure TAbstractViewer.SetShowAllBooleansAsCheckBoxes(Value: Boolean);
begin
  FShowBooleansAsCheckBoxes := Value;
  InvalidateGrid;
end;

function TAbstractViewer.DefineDecimalCount( AField : TDataField; ARow : TAbstractRow;
  ForEditor : Boolean; var AlwaysShowCount : Integer  ) : Integer;
var
  ADataType : TDataType;
begin
  Result := -1;
  AlwaysShowCount := 0;

  if (AField <> nil) then
  begin
    ADataType := AField.DataType;
    if ADataType is TCurrencyType then
    begin
      if ForEditor then
        AlwaysShowCount := CurrencyDecimalCount
      else
      begin
        Result := CurrencyDecimalCount;
        AlwaysShowCount := Result;
      end;
    end
    else if ADataType.IsNumeric and
            not (ADataType is TIntegerType) then
    begin
      AlwaysShowCount := DecimalCount;
      if ForEditor then
        Result := EditorDecimalCount
      else
        Result := DecimalCount;
    end
    else
      Result := ADataType.DefaultDecimalCount;
  end;
end;

function TAbstractViewer.DefineDivisor( AField : TDataField; ARow : TAbstractRow;
  ForEditor : Boolean) : Double;
begin
  if (AField <> nil) and
     (AField.DataType is TCurrencyType) then
  begin
    Result := Divisor[AField, ARow];
  end
  else
    Result := 1;
end;

procedure TAbstractViewer.DefineDecimalCountAndDivisor( ACell: IDataEditorCellInterface );
var
  AFormatter : TDBUFormatter;
  aDivisor : Double;
  aDecimalCount, AlwaysShowCount : Integer;
  AField : TDataField;
  ARow : TAbstractRow;
  ForEditor : Boolean;
begin
  AFormatter := ACell.GetDBUFormatter;
  AField := ACell.DataField;
  ARow := ACell.DataRow;

  ForEditor := ACell.ForEditor;

  AlwaysShowCount := -1;
  aDivisor := DefineDivisor( AField, ARow, ForEditor );
  aDecimalCount := DefineDecimalCount( AField, ARow, ForEditor, AlwaysShowCount );

  with AFormatter do
  begin
    Divisor := aDivisor;
    SetDecimalCount( aDecimalCount, AlwaysShowCount );
  end;
end;

procedure TAbstractViewer.DefineCell( Cell : IDataEditorCellInterface );
var
  ACol, ARow : Integer;
begin
  ACol := Cell.Col;
  ARow := Cell.Row;

  if (ACol >= TotalColCount) then
    Exit;

  with Cell do
  begin
    DataField := Self.DataField[ARow, ACol];
    ReadOnly := True;
  end;

  if (Cell.Row < HeaderRowViewCount) or (Cell.Col < AlwaysFixedColCount) then
    Cell.CellType := HeaderCellType
  else
    Cell.CellType := NormalCellType;
end;

procedure TAbstractViewer.DefineCellLook( Cell : IDataEditorCellInterface );
var
  ARow : TAbstractRow;
  IsSelected, IsFocused: Boolean;
  IsNegative, IsStripe, IsFixed, IsValid : Boolean;
  AValue : TValue;
  ASelection : TGridRect;
  FGColor, ABGColor : TColor;
  HorizAlign : TAlignment;
begin
  ARow := Cell.DataRow;

  ASelection := Grid.Selection;
  IsFocused := (Cell.Col = Grid.Col) and
               (Cell.Row = Grid.Row);
  IsSelected := CellIsSelected( Cell.Col, Cell.Row ) or
                (IsFocused and (dgDrawFocusSelected in Grid.DBUOptions) );
  IsStripe := (( (Cell.Row - HeaderRowViewCount) div RowViewCount ) mod 2 = 0) and
              Assigned(RowStorage) and (RowStorage.RowCount > 0);
  IsFixed := (Cell.Row < HeaderRowViewCount) or
             (Cell.Col < AlwaysFixedColCount);

  if Assigned(Cell.DataField) then
  begin
    try
      AValue := Cell.Value;
      IsNegative := UseNegativeColor and
                    AValue.DataType.IsNumeric and //LAA-tillsatt pga pang
                    Cell.DataField.DataType.IsNegative[ AValue ];
    except
      //LAA: Vad skall man göra, vid vissa dumma pastningar så pangar Cell.Value
      // och då hämtar sig Explorern aldrig. Detta sker tex när man pastar
      // förbi höger kant.
      IsNegative := False;
    end;
  end
  else
    IsNegative := False;

  if Assigned( ARow ) then
    IsValid := not RowStorage.RowIsUnaccepted(EditorCell.DataRow)
  else
    IsValid := True;

  if (Cell.Row < HeaderRowViewCount) and (RowViewCount > 0) then
    HorizAlign := DefineCellAlignment(RowViewDataField[RowViewByGridIndex[HeaderRowViewCount], Cell.Col])
  else
    HorizAlign := DefineCellAlignment(Cell.DataField);

  // Foreground Color
  if IsSelected and not IsFixed then
    FGColor := clHighlightText
  else if IsNegative then
    FGColor := clRed
  else
    FGColor := clBlack;

  // Background Color
  if IsFixed then                            // Header
    ABGColor := clBtnFace
  else if IsSelected then            // Selected
    ABGColor := clHighlight
  else if IsStripe and ShowStripes then           // Stripe
  begin
    if IsValid then
      ABGColor := StripeBGColor                    // Usual stripe
    else
      ABGColor := InvalidStripeBGColor;            // Invalid stripe
  end
  else
  begin
    if IsValid then
      ABGColor := clWhite                          // Ordinary row
    else
      ABGColor := InvalidBGColor                   // Invalid row
  end;

  with Cell.GetDBUFormatter do
  begin
    with Font do
    begin
      // Font style
      if (ARow is TSubTotalRow) then
        Style := Style + [fsBold]
      else
        Style := Style - [fsBold];

     Color := FGColor;
    end;
    HAlignment := HorizAlign;
    BGColor := ABGColor;
    if Assigned(Cell.DataField) and (Cell.DataField.DataType is TPictureType) then
    begin
      HMargin := 0;
      VMargin := 0;
    end;
  end;
end;

procedure TAbstractViewer.SetIsActive(Value : Boolean);
begin
  FIsActive := Value;
end;

function TAbstractViewer.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TAbstractViewer.SetReadOnly(Value : Boolean);
begin
  FReadOnly := Value;
  if IsActive then
  begin
    Disable;
    Enable;
  end;
end;

function TAbstractViewer.GetHeaderRowViewCount : Integer;
begin
  Result := 0;
end;

procedure TAbstractViewer.SetSetFocusOnExec(Value: Boolean);
begin
  FFocused := Value;
end;

procedure TAbstractViewer.GridDblClick(Sender: TObject);
begin
  HideAnts;
end;

procedure TAbstractViewer.GridKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  if Key = Key_Escape then
    HideAnts;
end;

procedure TAbstractViewer.ArrangeRows;
begin
  HideAnts;
  RowStorage.ArrangeRows;
end;

procedure TAbstractViewer.HideAnts;
begin
  FreeAndNil(FAntFrameDrawer);
end;

procedure TAbstractViewer.ShowAnts;

  procedure CreateAntsForRect(ARect : TRect);
  var
    AGridRect : TGridRect;
  begin
    with ARect do
      AGridRect := GridRect( GridCoord(Max(Left, AlwaysFixedColCount), Max(Top, HeaderRowViewCount)),
                             GridCoord(Right, Bottom) );

    FAntFrameDrawer.AddFrame(AGridRect);
  end;

var
  AList : TList;
  i : Integer;
  ARect : TRect;
begin
  HideAnts;

  AList := TList.Create;
  try
    GetTotalSelection( AList );
    FAntFrameDrawer := TAntFrameDrawer.Create(FGrid, []);
    if AList.Count = 0 then
    begin
      GetGridSelection(ARect);
      CreateAntsForRect(ARect);
    end
    else
      for i := 0 to AList.Count -1 do
      begin
        ARect := PRect(AList[i])^;
        CreateAntsForRect(ARect);
      end;
  finally
    FreeListWithPointers( AList );
  end;
end;

procedure TAbstractViewer.RedrawAntArea;
begin
  if Assigned(FAntFrameDrawer) then
    FAntFrameDrawer.Redraw;
end;

function TAbstractViewer.GetRowViewByGridIndex(idxGridRow : Integer) : TRowView;
begin
  try
    result := PageView.RowView[RowViewIndexByGridIndex[idxGridRow]];
  except
    on RE: Exception do
      raise ERangeError.Create(Self.ClassName + '.GetRowView: ' + TranslateMessage(E_IndexOutOfBounds) + ' idxGridRow = ' + IntToStr(idxGridRow) +
      ' RowViewCount = ' + IntToStr(RowViewCount) + ')' + NEW_LINE + RE.Message);
  end;
end;

function TAbstractViewer.GetRowViewIndexByGridIndex(idxGridRow : Integer) : Integer;
begin
  Result := (idxGridRow - HeaderRowViewCount) mod RowViewCount;
end;

procedure TAbstractViewer.GetTotalSelection(SelList: TList);
begin
  // nothing
end;

procedure TAbstractViewer.GetGridSelection(var ARect: TRect);
var
  GridRect : TGridRect;
begin
  GridRect := Grid.Selection;
  with GridRect do
    ARect := Rect( Left, Top, Right, Bottom );
end;

function TAbstractViewer.Find(AValue: TValue; var APos: TPoint;
  MatchCase : Boolean = True; Down : Boolean = True) : Boolean;
var
  Iterator : IGridIterator;

  function DoesMatch(AVal : TValue; x, y : Integer; MatchCase : Boolean) : Boolean;
  var
    DataType : TDataType;
    MatchStr, GridStr : String;
    AGridVal : TValue;
  begin
    Result := False;
    try
      Iterator.ChangeCell(x, y);
      AGridVal := Iterator.GetGridValue;

      DataType := AVal.DataType;
      if (DataType <> nil) then
      begin
        if not (DataType is TStringType) then
          Result := DataType.Equals(AVal, AGridVal);

        if not Result then
        begin
          MatchStr := AsString(AVal);
          GridStr := AsString(AGridVal);
          if MatchCase then
            Result := Pos(MatchStr, GridStr) > 0
          else
            Result := Pos(AnsiUpperCase(MatchStr), AnsiUpperCase(GridStr)) > 0;
        end;
      end;
    except
    end;
  end;

var
  x, y, startCol, startRow, stopCol, stopRow : Integer;
begin
  if Down then
  begin
    startRow := Max(APos.y, HeaderRowViewCount);
    startCol := Max(APos.x, AlwaysFixedColCount);
    stopRow := TotalRowCount -1;
    stopCol := TotalColCount -1;
  end
  else
  begin
    startRow := Min(APos.y, TotalRowCount -1);
    startCol := Min(APos.x, TotalColCount -1);
    stopRow := HeaderRowViewCount;
    stopCol := AlwaysFixedColCount;
  end;

  Iterator := GridIterator;

  Result := False;
  if Down then
  begin
    for y := startRow to stopRow do
    begin
      for x := startCol to stopCol do
        if DoesMatch(AValue, x, y, MatchCase) then
        begin
          APos := Point(x, y);
          Result := True;
          Break;
        end;

      if Result then
        Break;

      startCol := AlwaysFixedColCount;
    end;
  end
  else
  begin
    for y := startRow downto stopRow do
    begin
      for x := startCol downto stopCol do
        if DoesMatch(AValue, x, y, MatchCase) then
        begin
          APos := Point(x, y);
          Result := True;
          Break;
        end;

      if Result then
        Break;

      startCol := TotalColCount -1;
    end;
  end;
end;

{  TTotalViewer }

constructor  TTotalViewer.Create(AParent : TWinControl);
begin
  inherited Create(AParent);
  FIsActive := True;
end;

destructor TTotalViewer.Destroy;
begin
  inherited Destroy;
end;

procedure TTotalViewer.Execute;
begin
  Inherited Execute;
end;

function TTotalViewer.GetRowStorage: TCustomRowStorage;
begin
  if Assigned(ActiveEditor) then
    Result := ActiveEditor.RowStorage
  else
    Result := nil;
end;

procedure TTotalViewer.SetRowStorage(const Value  : TCustomRowStorage);
begin
  raise Exception.Create(Self.ClassName + '.SetRowStorage: Action not supported!');
end;

procedure TTotalViewer.CreateGrid;
begin
  Inherited CreateGrid;

  with Grid do
  begin
    Align := alClient;
    DefaultRowHeight := 16;
    FixedRows := 0;
    ScrollBars := ssNone;
    OnClick := GridClick;
    OnDblClick := GridDblClick;
    OnKeyDown := GridKeyDown;
  end;
end;

procedure TTotalViewer.CheckColWidths;
begin
  if (ActiveEditor <> nil) and
     ColWidthsChanged then
    AdjustColWidths;
end;

procedure TTotalViewer.ScrollGrid;
begin
  if (ActiveEditor <> nil) then
    Grid.LeftCol := ActiveEditor.Grid.LeftCol;
end;

procedure TTotalViewer.AdjustColWidths;
var
  iCol : Integer;
begin
  with Grid do
  begin
    SetGridSize;
    for iCol := 0 to ColCount - 1 do
      ColWidths[iCol] := ActiveEditor.Grid.ColWidths[iCol];
    InvalidateGrid;
  end;
end;

function TTotalViewer.ColWidthsChanged : Boolean;
var
  idx : Integer;
  EditorGrid : TDBUGrid;
begin
  Result := True;
  with Grid do
  begin
    ColCount := TotalColCount;
    EditorGrid := ActiveEditor.Grid;
    for idx := 0 to EditorGrid.FixedCols -1 do
      if EditorGrid.ColWidths[idx] <> ColWidths[idx] then
        Exit;

    for idx := EditorGrid.LeftCol to (EditorGrid.LeftCol + EditorGrid.VisibleColCount) do
      if EditorGrid.ColWidths[idx] <> ColWidths[idx] then
        Exit;
  end;
  Result := False;
end;

procedure TTotalViewer.SetGridSize;
begin
  if (ActiveEditor <> nil) and ActiveEditor.RowStorage.CanHaveTotals then
  begin
    Grid.ColCount := MaxIntValue([TotalColCount, 1]);
    Grid.RowCount := MaxIntValue([TotalRowCount, 1]);
    Grid.FixedCols := ActiveEditor.Grid.FixedCols;
    Grid.TopRow := 0;
    Grid.Left := ActiveEditor.Grid.Left;
    Grid.TabStop := False;
    Parent.Height := PanTotDefHeight + Grid.RowCount * (Grid.DefaultRowHeight +1);
  end
  else
  begin
    Grid.ColCount := 1;
    Grid.RowCount := 1;
    Grid.FixedCols := 0;
  end;
end;

function TTotalViewer.GetDataField(AGridRow, AGridCol : Integer) : TDataField;
begin
  Assert(AGridRow < ActiveEditor.PageView.RowViewCount, Self.ClassName
         + '.GetDataField: ' + TranslateMessage(E_IndexOutOfBounds) + ' ' + IntToStr(AGridRow) + ' >= '
         + IntToStr(ActiveEditor.PageView.RowViewCount));

  Result := RowViewDataField[ActiveEditor.PageView.RowView[AGridRow], AGridCol];
end;

function TTotalViewer.GetRowViewDataField(ARowView : TAbstractRowView; AGridCol : Integer) : TDataField;
var
  aDataField : TDataField;
begin
  aDataField := ActiveEditor.RowViewDataField[ARowView, AGridCol];
  if (aDataField is TConstantField) then
    Result := aDataField
  else if ( not aDataField.IsAggregable ) or
          ( AGridCol < ActiveEditor.AlwaysFixedColCount ) then
    Result := SVEmptyField
  else
    Result := aDataField;
end;

function TTotalViewer.GetTotalColCount : Integer;
begin
  Result := ActiveEditor.TotalColCount;
end;

function TTotalViewer.GetTotalRowCount : Integer;
begin
  Result := ActiveEditor.PageView.RowViewCount;
end;

function TTotalViewer.GetAlwaysFixedColCount : Integer;
begin
  Result := ActiveEditor.AlwaysFixedColCount;
end;

function TTotalViewer.GetMarkRowColCount : Integer;
begin
  Result := ActiveEditor.MarkRowColCount;
end;

procedure TTotalViewer.SetActiveEditor(Editor : TGridEditor);
begin
  FActiveEditor := Editor;
end;

procedure TTotalViewer.GridClick(Sender : TObject);
begin
  ScrollGrid;
end;

procedure TTotalViewer.GridGetCell( Sender : IDataEditorCellInterface );
begin
  if Killing then
    Exit;

  if (ActiveEditor = nil) or not ActiveEditor.RowStorage.CanHaveTotals then
    Exit;
  inherited GridGetCell(Sender);
end;

procedure TTotalViewer.DefineCell(Cell: IDataEditorCellInterface);
var
  ARowStorage : TCustomRowStorage;
begin
  inherited;

  ARowStorage := ActiveEditor.RowStorage;
  if ARowStorage.CanHaveTotals then
    Cell.DataRow := ARowStorage.Total
  else
    Cell.DataRow := nil;
end;

function TTotalViewer.GetDivisor(AField: TDataField; ARow: TAbstractRow) : Double;
begin
  Result := ActiveEditor.Divisor[AField, ARow];
end;

function TTotalViewer.GetCurrencyDivisor : Double;
begin
  Result := ActiveEditor.CurrencyDivisor;
end;

function TTotalViewer.GetDecimalCount : Integer;
begin
  Result := ActiveEditor.DecimalCount;
end;

function TTotalViewer.GetCurrencyDecimalCount : Integer;
begin
  Result := ActiveEditor.CurrencyDecimalCount;
end;

function TTotalViewer.GetEditorDecimalCount : Integer;
begin
  Result := ActiveEditor.EditorDecimalCount;
end;

function TTotalViewer.FieldBelongsToTable(AField: TDataField;
  ARow: TAbstractRow): Boolean;
begin
  Result := ActiveEditor.FieldBelongsToTable( AField, ARow );
end;

function TTotalViewer.GetRowViewCount: Integer;
begin
  Result := ActiveEditor.RowViewCount;
end;

function TTotalViewer.GetShowStripes: Boolean;
begin
  Result := False;
end;

function TTotalViewer.GetPageView: TPageView;
begin
  Result := ActiveEditor.PageView;
end;

{ TGridEditor }

constructor TGridEditor.Create(AParent : TWinControl);
begin
  Executing := True;
  inherited Create(AParent);

  FCurrencyDivisor := 1.0;
  FCurrencyDecimalCount := CurrencyType.DefaultDecimalCount; // DECIMALCOUNT_Currency;
  FDecimalCount := DoubleType.DefaultDecimalCount; //DECIMALCOUNT_Double;

  FCheckBoxFieldsObjects := TList.Create;
  FOwnedDisabledFieldList := TDefaultValueFieldList.Create;
  FAutoCreatedRows := TDataRowList.Create;
  FRemovingActive := False;
  FTotalViewer := nil;
  FShowSubTotals := False;
  FFixedCols := 0;
  FUpdateType := utRow;
  FConstantBufferEdit := False;
  FConstantBufferKey := nil;
  AllowMultipleAutoCreatedRows := False;
  AutoCreateRows := False;
  AutoDeleteRows := False;
  FAutoCreatingRow := False;
  FEditorDecimalCount := DecimalCount;
  FSortField := nil;
  FHiddenKeys := nil;

  FSelectedDataRows := TDataRowList.Create;
  FSelectedCols := TValueList.Create( IntegerType );
  FSelectedCols.Duplicates := dupIgnore;
  FSelectedCols.Sorted := True;

  FDefaultMarkRowCalcField := CreateMarkRowCalcField;
  FMarkRowCalcField := FDefaultMarkRowCalcField;
  FEnableSorting := False;
  FHideSubtotalForSingle := True;
  FHideSubtotalOnOpen := False;
  FRecursiveOnSingleSubtotal := True;
  FPervoOpenRow := False;

  FHiddenColWidthOrganizer := TFieldWidthOrganizer.Create;
end;

destructor TGridEditor.Destroy;
var
  i : Integer;
begin
  if Assigned(FSlaveList) then
  begin
    for i := FSlaveList.Count -1 downto 0 do
      ISlave(FSlaveList[i]).MasterDestroying;
    FreeAndNil(FSlaveList);
  end;

  inherited Destroy;

  FSelectedDataRows.Free;
  FSelectedCols.Free;
  FDefaultMarkRowCalcField.Free;
  FMarkRowPicture.Free;
  FDialogBoxFields.Free;
  FCheckBoxFieldsObjects.Free;
  FSaveToAuxtableFields.Free;
{$ifdef D4_OR_HIGHER}
  FreeAndNil( FBalanceRow );
{$else}
  FBalanceRow.Free;
{$endif D4_OR_HIGHER}
  FAutoCreatedRows.Free;
  FOwnedDisabledFieldList.Free;
  FHiddenColWidthOrganizer.Free;
  FHiddenKeys.Free;
end;

procedure TGridEditor.Initialize(RowStorage : TCustomRowStorage; StandardView : TStandardView; PageView : TPageView;
          ShowSubTotals, ReadOnly, ShowStripes : Boolean);
begin
  FRowStorage := RowStorage;
  FStandardView := StandardView;
  FPageView := PageView;
  DisabledFieldList := nil;
  KeyFormatting := dvDefault;
  FShowSubTotals := ShowSubTotals;
  FReadOnly := ReadOnly;
  FShowStripes := ShowStripes;
end;

procedure TGridEditor.Execute;

  procedure SetAlwaysFixedColWidths;
  var
    iCol : Integer;
  begin
    for iCol := 0 to AlwaysFixedColCount -1 do
      Grid.ColWidths[iCol] := COLWIDTH_MarkCol;
  end;

begin
  Inherited Execute;

  SetValueCell.OnSetValue := CellSetValue;
  SetValueCell.OnSettingValue := CellSettingValue;

  if (StandardView.PageViewCount > 1) and (Parent is TTabSheet) then
  begin
    TTabSheet(Parent).Caption := PageView.Caption;
{$ifdef D4_OR_HIGHER}
    TTabSheet(Parent).OnHide := OnHEvent;
    TTabSheet(Parent).OnShow := OnSEvent;
{$endif D4_OR_HIGHER}
  end;

  with FGrid do
  begin
    Align := alClient;
    DefaultRowHeight := 16;

    OnClick := GridClick;
    OnDblClick := GridDblClick;
    OnKeyDown := GridKeyDown;
    OnMouseDown := GridMouseDown;
    OnHScroll := GridHScroll;
    OnVScroll := GridVScroll;
  end;

  Executing := False;
  SetGridSize;
  FixedCols := MaxIntValue([FixedCols, AlwaysFixedColCount]);
  SetAlwaysFixedColWidths;
end;

procedure TGridEditor.SetKilling( Value : Boolean );
begin
  try
    FKilling := Value;
    Grid.Killing := Value;
    CommitChanges;
  except
  end;
end;

function TGridEditor.GetRowStorage: TCustomRowStorage;
begin
  Result := FRowStorage;
end;

procedure TGridEditor.SetRowStorage(const Value  : TCustomRowStorage);
var
  i : Integer;
begin
  Disable;
  RemovingActive := True;
  try
    FRowStorage := Value;
    if Assigned(FSlaveList) then
      for i := 0 to FSlaveList.Count -1 do
        ISlave(FSlaveList[i]).StorageChange(Value);
  finally
    Enable;
    RemovingActive := False;
  end;

  ActiveRow := 0;
end;

function TGridEditor.GetReadOnly: Boolean;
begin
  Result := PageView.ReadOnly or inherited GetReadOnly;
end;

function TGridEditor.InsertRow(idxGridRow : Integer; InsertUnder, DuplicateRow : Boolean): TDataRow;
var
  idxDataRow, i : Integer;
  newDataRow : TDataRow;
  oldDataRow : TAbstractRow;
  Unaccepted : Boolean;
begin
  Result := nil;
  oldDataRow := DataRowByGridIndex[idxGridRow];
  if DuplicateRow and ((oldDataRow = nil) or (oldDataRow is TSubTotalRow)) then
    Exit;

  if not AllowMultipleAutoCreatedRows then
    AutoCreatedRows.Clear;
  UnselectAll;
  HideAnts;

  newDataRow := TDataRow.Create(RowStorage.DataTable);
  idxDataRow := RowStorage.IndexOfRow(oldDataRow);
  if DuplicateRow then
    CopyDataToRow( TDataRow(oldDataRow), newDataRow )
  else
    SetInitialValuesToRow(newDataRow);

  Unaccepted := False;

  for i := 0 to PageView.RowView[0].DisplayKeyList.Count -1 do
    if TableHasKeyOrLookup( RowStorage.DataTable, PageView.RowView[0].DisplayKeyList[i]) then
    begin
      Unaccepted := True;
      Break;
    end;

  if InsertUnder then
    InsertDataRow(newDataRow, idxDataRow +1)
  else
    InsertDataRow(newDataRow, MaxIntValue( [idxDataRow, 0] ));

  SetDefaultValuesToDataRow(newDataRow);
  SetGridSize;

  if not Unaccepted then
    AcceptRow(newDataRow);

  Result := newDataRow;
  GridCol := AlwaysFixedColCount;
  ActiveDataRow := newDataRow;
  ChangeDataRowEvents;
end;

procedure TGridEditor.CopyDataToRow(SrcRow, DestRow : TDataRow);
var
  i : Integer;
  ATable : TDataTable;
begin
  ATable := SrcRow.DataTable;
  for i := 0 to ATable.KeyCount -1 do
    try
      DestRow[ATable.Field[i]] := SrcRow[ATable.Field[i]];
    except
    end;

  for i := ATable.KeyCount to ATable.FieldCount -1 do
    try
      if not ATable.Field[i].ReadOnly[DestRow] then
        DestRow[ATable.Field[i]] := SrcRow[ATable.Field[i]];
    except
    end;
end;

function TGridEditor.InsertSubtotalRow(idxGridRow : Integer; InsertUnder : Boolean;
    SubtotalKey : TDataField): TSubtotalRow;
var
  idxNew : Integer;
begin
  UnselectAll;
  HideAnts;

  idxNew := RowStorage.IndexOfRow(ActiveDataRow);
  if InsertUnder then
    Result := RowStorage.NewUnacceptedSubTotal( idxNew + 1, RowStorage.TreeKeyByField[ SubtotalKey ] )
  else
    Result := RowStorage.NewUnacceptedSubTotal( MaxIntValue([idxNew, 0]), RowStorage.TreeKeyByField[ SubtotalKey ] );

  SetInitialValuesToRow( Result );
  Result.Visible := True;

  SetGridSize;
  GridCol := AlwaysFixedColCount;
  ActiveDataRow := Result;
end;

procedure TGridEditor.AutoCreateRow;
var
  NewRow : TDataRow;
begin
  if ReadOnly or not CanAddNewRow then
    Exit;
    
  FAutoCreatingRow := True;
  Disable;
  try
    NewRow := InsertRow( Grid.RowCount -1, True, False );
    AutoCreatedRows.AddObject( '', NewRow );
  finally
    Enable;
  end;
  FAutoCreatingRow := False;
  if Assigned( OnAutoCreateRowEvent ) then
    OnAutoCreateRowEvent( Self, NewRow );
end;

procedure TGridEditor.SetInitialValuesToRow(ADataRow : TAbstractRow);
var
  iField : Integer;
  ATable : TDataTable;
  AField : TDataField;
  Value : TValue;
begin
  ATable := ADataRow.DataTable;
  for iField := 0 to ATable.KeyCount -1 do
  begin
    AField := ATable.Field[iField];
    if ( RowHasField( ADataRow, AField) ) and
       Criteria.AcceptsExactlyOneValue(AField, Value) then
        ADataRow[AField] := Value
  end;
end;

procedure TGridEditor.SetDefaultValuesToDataRow(ADataRow : TDataRow);
var
  iField, iProperty : Integer;
  AField : TDataField;
  DefaultValueProperty : TGridEditorDefaultValue;
  Cont : Boolean;
  TmpValue : TValue;
  TmpSet : TDataFieldSet;
begin
  if DisabledFieldList <> nil then
  begin
    TmpSet := TDataFieldSet.Create;
    for iField := 0 to DisabledFieldList.Count -1 do
    begin
      AField := DisabledFieldList.Field[iField];
      try
        if (AField <> nil) and
           (ADataRow.DataTable.TableHasField(AField)) then
        begin
          TmpValue := ADataRow[AField];
          ADataRow[AField] := DisabledFieldList.FieldValue[AField];

          TmpSet.Clear;
          TmpSet.AddField(AField);
          // PAGECRITERIA kan vara nil!
          if not PageView.Criteria.AcceptsRowDependent(ADataRow, TmpSet) then
            ADataRow[AField] := TmpValue;
        end;
      except
      end;
    end;

    TmpSet.Free;
  end;

  for iProperty := 0 to StandardView.DefaultValueList.Count -1 do
  begin
    DefaultValueProperty := TGridEditorDefaultValue(StandardView.DefaultValueList.Objects[iProperty]);

    Cont := False;
    for iField := 0 to DefaultValueProperty.DependFields.Count -1 do
    begin
      AField := DefaultValueProperty.DependFields.Field[iField];
      if DisabledFieldList.ContainsField( AField ) or
         not RowHasField( ADataRow, AField ) then
        Cont := True;
    end;

    if Cont then
      Continue;

    AField := DefaultValueProperty.DataField;
    if ADataRow.IndexOfField(aField) >= 0 then
      ADataRow[AField] := DefaultValueProperty.GetDefaultValue(ADataRow);
  end;
end;

function TGridEditor.NewRow : TDataRow;
begin
  Result := nil;
  if not CanAddNewRow then
    Exit;
  Disable;
  try
    Result := InsertRow(Grid.Row, True, False);
  finally
    Enable;
  end;
end;

function TGridEditor.NewSubTotal : TSubTotalRow;
var
  SubKey : TDataField;
begin
  Result := nil;
  if not CanAddNewSubTotal then
    Exit;

  Disable;
  try
    if StandardView.MarmaladeKeys.Count <= 1 then
      raise Exception.Create( 'Can''t determine subtotal level!' );
    SubKey := StandardView.MarmaladeKeys.Field[1];
    Result := InsertSubtotalRow(Grid.Row, True, SubKey);
  finally
    Enable;
  end;
end;

procedure TGridEditor.InsertDataRow(ARow : TDataRow; idxNewRow : Integer);
begin
  if idxNewRow > RowStorage.RowCount then
    idxNewRow := RowStorage.RowCount;

  RowStorage.NewUnacceptedRow(idxNewRow, ARow);
end;

function TGridEditor.CanAddNewRow : Boolean;
begin
  if not (etAddRow in StandardView.EditState) then
    Result := False
  else if PageView.RowViewCount <= 0 then
    Result := False
  else if PageView.RowView[0].FieldCount <= 0 then
    Result := False
  else if ReadOnly then
    Result := False
  else if (RowStorage.RowCount > 0) then
    Result := HasEditableKeys
  else
    Result := True;
end;

function TGridEditor.CanAddNewSubTotal : Boolean;
begin
  Result := CanAddNewRow and
            StandardView.Marmalade;
end;

procedure TGridEditor.FillHiddenKeys(AKeyList : TFieldList);

  procedure AddTableKeysToList(DestList : TFieldList; SrcList : TAbstractFieldList);
  var
    iKey : Integer;
  begin
    for iKey := 0 to SrcList.Count -1 do
      if RowStorage.DataTable.TableHasKey(SrcList[iKey]) then
        DestList.Add(SrcList[iKey]);
  end;

begin
  AKeyList.Clear;
  AKeyList.Duplicates := dupIgnore;
  AddTableKeysToList(AKeyList, StandardView.CommonKeyList);
  AddTableKeysToList(AKeyList, DisabledFieldList);
end;

function TGridEditor.HasEditableKeys : Boolean;
begin
  if not Assigned(FHiddenKeys) then
  begin
    FHiddenKeys := TFieldList.Create;
    FillHiddenKeys(FHiddenKeys);
  end;

  Result := (RowStorage.DataTable.RunningNumberField <> nil) or
            (FHiddenKeys.Count < RowStorage.DataTable.KeyCount);
end;

function TGridEditor.DuplicateRow : TDataRow;
begin
  Result := nil;
  if not CanDuplicateRow then
    Exit;
  Disable;
  try
    Result := InsertRow(Grid.Row, True, True);
  finally
    Enable;
  end;
end;

function TGridEditor.CanDuplicateRow : Boolean;
var
  aDataRow : TAbstractRow;
begin
  aDataRow := DataRowByGridIndex[Grid.Row];
  if not (etDuplicateRow in StandardView.EditState) then
    Result := False
  else if PageView.RowViewCount <= 0 then
    Result := False
  else if PageView.RowView[0].FieldCount <= 0 then
    Result := False
  else if ReadOnly then
    Result := False
  else if RowStorage.RowCount = 0 then
    Result := False
  else if (aDataRow is TSubTotalRow) or (aDataRow = nil) then
    Result := False
  else
    Result := HasEditableKeys;
end;

procedure TGridEditor.DeleteRows;
var
  iRow : Integer;
  AActiveDataRow : TAbstractRow;
  Msg : String;
  Rows : TDataRowList;
begin
  if not CanDeleteRows then
{$ifndef LINUX}
    MessageBeep(MB_OK)
{$else LINUX}
    Beep
{$endif LINUX}
  else if RowStorage.RowCount = 0 then
    Exit
  else
  begin
    Rows := TDataRowList.Create;
    GetSelectedDataRows( Rows, True );
    if (Rows.Count = 0) and
       (ActiveDataRow <> nil) then
      Rows.AddObject( '', ActiveDataRow );

    if Rows.Count > 1 then
      Msg := TranslateMessage(Q_DeleteSelectedRows)
    else
      Msg := TranslateMessage(Q_DeleteSelectedRow);

    if TranslateMessageDlg(Msg, mtConfirmation, mbOKCancel, 0) = mrOk then
    begin
      Disable;
      try
        CommitChanges;
        EditorCell.DataRow := nil;
        SetValueCell.DataRow := nil;
        AActiveDataRow := ActiveDataRow;

        for iRow := 0 to Rows.Count -1 do
          DoDeleteRow(Rows.AbstractRows[iRow]);
        Rows.Free;

        UnselectAll;
        HideAnts;
        if RowStorage.IndexOfRow(AActiveDataRow) >= 0 then
          ActiveDataRow := AActiveDataRow;
      finally
        Enable;
      end;
      ChangeDataRow;
      UpdateTotals;
    end;
  end;
end;

procedure TGridEditor.GetSelectedDataRows( Rows : TStrings; ReverseRowOrder : Boolean );
var
  AList : TList;
begin
  AList := TList.Create;
  try
    GetTotalSelection( AList );
    GetDataRowsInSelections( Rows, AList, ReverseRowOrder);
  finally
    FreeListWithPointers( AList );
  end;  

  if (Rows.Count = 0) and
     (ActiveDataRow <> nil) then
  Rows.AddObject( '', ActiveDataRow );
end;

procedure TGridEditor.GetDataRowsInSelections( Rows : TStrings; Selections : TList;
    ReverseRowOrder : Boolean );
var
  i : Integer;
  ARect : TRect;
begin
  for i := 0 to Selections.Count -1 do
  begin
    ARect := PRect( Selections[i] )^;
    GetDataRowsForRect(ARect, Rows, ReverseRowOrder);
  end;
end;

function TGridEditor.CanDeleteRows : Boolean;
var
  iRow : Integer;
  ASelRect : TRect;
  Rows : TDataRowList;
begin
  Result := True;
  if AllSelected or (SelectedCols.Count > 0) then
    for iRow := 0 to RowStorage.RowCount -1 do
    begin
      Result := Result and CanDeleteRow(RowStorage.Rows[iRow]);
      if not Result then
        break;
    end
  else if SelectedDataRows.Count > 0 then
    for iRow := 0 to SelectedDataRows.Count -1 do
    begin
      Result := Result and CanDeleteRow(SelectedDataRows.AbstractRows[iRow]);
      if not Result then
        Break;
    end
  else
  begin
    GetGridSelection(ASelRect);
    Rows := TDataRowList.Create;
    GetDataRowsForRect(ASelRect, Rows, True);

    if Rows.Count = 0 then
      Result := False
    else
      for iRow := 0 to Rows.Count -1 do
      begin
        Result := Result and CanDeleteRow(Rows.AbstractRows[iRow]);
        if not Result then
          Break;
      end;

    Rows.Free;
  end;
end;

procedure TGridEditor.GetDataRowsForRect(ASelRect : TRect; Rows : TStrings; ReverseRowOrder : Boolean );

  procedure AddDataRowToRowList(iRow : Integer );
  var
    ARow : TAbstractRow;
  begin
    if iRow < RowStorage.RowCount then
    begin
      ARow := RowStorage.Rows[iRow];
      if Rows.IndexOfObject(ARow) = -1 then
        Rows.AddObject('', ARow);
    end;
  end;

var
  Last, First, iRow : Integer;
begin
  ASelRect := ArrangeRect(ASelRect);
  First := DataRowIndexByGridIndex[ASelRect.Top];
  Last := DataRowIndexByGridIndex[ASelRect.Bottom];

  if (First = -1) or (Last = -1) then
    Exit;

  if ReverseRowOrder then
    for iRow := Last downto First do // Fixa MVJ denna tycks panga ifall det ej finns data i gridden!
      AddDataRowToRowList(iRow)
  else
    for iRow := First to Last do
      AddDataRowToRowList(iRow);
end;

function TGridEditor.CanDeleteRow(ADataRow : TAbstractRow) : Boolean;
begin
  if not (etDeleteRow in StandardView.EditState) then
    Result := False
  else if ReadOnly then
    Result := False
  else if RowStorage.RowCount = 0 then
    Result := False
  else if DataRowIsReadOnly(aDataRow) or (aDataRow = nil) then
    Result := False
  else if not (ADataRow is TDataRow) then
    Result := False
  else
    Result := True;
end;

procedure TGridEditor.DoDeleteRow(ARow : TAbstractRow);
var
  idxRow : Integer;
begin
  if ARow = nil then
    Exit
  else if ARow is TSubTotalRow then
  begin
    TranslateShowMessage(TranslateMessage(I_CanNotDeleteSubtotal));
    Exit;
  end;

  idxRow := AutoCreatedRows.IndexOfObject( ARow );
  if idxRow >= 0 then
    AutoCreatedRows.Delete( idxRow );

  if ConstantBufferEdit then
    ResetRowForBuffer(TDataRow(ARow));

  RemovingActive := (ARow = ActiveDataRow);
  if RemovingActive then
    ChangeDataRow;

  if Assigned(OnDeleteRow) then
    OnDeleteRow(Self, TDataRow(ARow));

  TDataRow(ARow).Delete;

  FlushCells;

  RemovingActive := False;
end;

procedure TGridEditor.ResetRowForBuffer(ARow : TDataRow);
var
  DestRow : TDataRow;
begin
  if RowStorage.RowIsUnaccepted(ARow) then
    Exit;

  DestRow := GetBufferRow(ARow);
  if (DestRow = nil) or
    (DestRow = ARow) then
    Abort;

  MoveDiffToBuffer(ARow, nil, DestRow);
end;

function TGridEditor.AcceptUnacceptedRows : Boolean;
var
  iUnaccepted : Integer;
begin
  CommitChanges;
  DeleteAutoCreatedRows;
  with RowStorage do
  begin
    iUnaccepted := UnacceptedRowCount -1;
    while (iUnaccepted >= 0) do
    begin
      Self.AcceptRow(UnacceptedRows[iUnaccepted]);
      Dec( iUnaccepted );
      if iUnaccepted > UnacceptedRowCount -1 then
        iUnaccepted := UnacceptedRowCount -1;                    
    end;

    Result := (UnAcceptedRowCount = 0);
  end;
end;

function TGridEditor.AcceptRow(DataRow : TAbstractRow) : TPutResult;
var
  ARow : TAbstractRow;
begin
  if not IsRowLegal(DataRow) then
    Result := prIllegalKeyValue
  else if RowStorage.RowIsUnaccepted( DataRow ) then
  begin
    Result := DoAcceptRow(DataRow);
    if ParentSubtotalsVisible[ DataRow ] or
       (DataRow is TSubTotalRow) then
    begin
      ARow := ActiveDataRow;
      try
        ArrangeRows;
        SetGridSize;
        ActiveDataRow := ARow;
        InvalidateGrid;
      except
      end;
    end
    else if not (Result in IllegalPutResults) then
      RepaintDataRow( DataRow );

    UpdateTotals;
  end
  else
    Result := prOk;
end;

function TGridEditor.DoAcceptRow(DataRow : TAbstractRow) : TPutResult;
var
  TmpRow, Buffer : TDataRow;
begin
  if DataRow is TDataRow then
  begin
    TmpRow := TDataRow(DataRow).CreateCopy;
    try
      Result := RowStorage.AcceptRow(DataRow);
      if ConstantBufferEdit and
         ( Result in [prOk, prKeyOverwrited, prRowsAdded] ) then
      begin
        Buffer := GetBufferRow(DataRow);
        MoveDiffToBuffer(nil, TmpRow, Buffer);
      end;
    finally
      TmpRow.Free;
    end;
  end
  else
    Result := RowStorage.AcceptRow( DataRow );
end;

procedure TGridEditor.ArrangeRows;
begin
  UnselectAll;
  if not Executing then
    inherited;
end;

procedure TGridEditor.SortRows;
begin
  Disable;
  try
    AcceptUnacceptedRows;
    ArrangeRows;
    PreviousRow := -1;
  finally
    EnableWithCoord(-1, HeaderRowViewCount);
  end;
end;

function TGridEditor.RowIsSubtotal(ARow : TAbstractRow) : Boolean;
begin
  Result := ARow is TSubTotalRow;
end;

function TGridEditor.RowSubTotalRow( ARow : TAbstractRow ) : TAbstractRow;
begin
  Result := ARow.SubTotalRow;
end;

function TGridEditor.RowSubRowCount(ARow : TAbstractRow) : Integer;
begin
  Assert( ARow is TSubTotalRow );
  Result := TSubTotalRow(ARow).SubRowCount;
end;

function TGridEditor.RowSubRows( ARow : TAbstractRow; Index : Integer ) : TAbstractRow;
begin
  Assert( ARow is TSubTotalRow );
  Result := TSubTotalRow(ARow).SubRows[Index];
end;

function TGridEditor.StorageCloseRow(ARow : TAbstractRow) : TAbstractRow;
begin
  Result := RowStorage.CloseRow(ARow);
end;

procedure TGridEditor.StorageOpenRow( ARow : TAbstractRow );
begin
  Assert( ARow is TSubTotalRow );
  RowStorage.OpenRow(TSubTotalRow(ARow));
end;

function TGridEditor.CloseRow : TAbstractRow;
begin
  Result := CloseRowOL(ActiveDataRow);
end;

function TGridEditor.CloseRowOL(ARow : TAbstractRow) : TAbstractRow;
var
  RowToClose : TAbstractRow;
  SubTotalRow : TAbstractRow;
begin
  Result := nil;
  RowToClose := ARow;
  DefaultSortRows;
  if not CanCloseRowOL(RowToClose) then
    Exit;

  Disable;
  try
    Result := StorageCloseRow(RowToClose);
    if Result <> nil then
      SubTotalRow := RowSubTotalRow( Result )
    else
      SubTotalRow := nil;

    if RecursiveOnSingleSubtotal and
       (SubTotalRow <> nil) and
       (RowSubRowCount( SubTotalRow ) = 1) and
       CanCloseRowOL(Result) then
      Result := CloseRowOL(Result)
    else
    begin
      ArrangeRows;
      SetGridSize;
    end;

    if Result <> nil then
      EnableWithCoord(-1, GridIndexOfDataRow(Result))
    else
      Enable;
  except
    Enable;
  end;
end;

function TGridEditor.CanCloseRow : Boolean;
begin
  Result := CanCloseRowOL(ActiveDataRow);
end;

function TGridEditor.CanCloseRowOL(ARow : TAbstractRow) : Boolean;
begin
  if ARow = nil then
    Result := False
  else
    Result := (RowStorage.Total <> ARow) and
              RowStorage.CanCloseRow(ARow);
end;

function TGridEditor.CanOpenAllRows : Boolean;
var
  iRow : integer;
begin
  Result := false;
  for iRow := 0 to RowStorage.RowCount-1 do
    Result := Result or CanOpenRowOL(RowStorage.Rows[iRow]);
end;

procedure TGridEditor.OpenAllRows;
var
  iRow : integer;
  OldRowCount : integer;
begin
  Disable;
  try
    // repeatedly open all rows, until rowcount does not increase
    repeat
      OldRowCount := RowStorage.RowCount;
      for iRow := 0 to OldRowCount-1 do
        if CanOpenRowOL( RowStorage.Rows[iRow] ) then
          OpenRowOL( RowStorage.Rows[iRow] );
    until RowStorage.RowCount = OldRowCount;
  finally
    Enable;
  end;
end;

procedure TGridEditor.OpenSelectedRows;
var
  iRow : integer;
  Rows : TStrings;
begin
  Rows := TStringList.Create;
  GetSelectedDataRows(Rows, false);
  for iRow := 0 to Rows.Count-1 do
    OpenRowOL( TAbstractRow(Rows.Objects[iRow]) );
  Rows.Free;
end;

procedure TGridEditor.CloseSelectedRows;
var
  iRow : integer;
  Rows : TStrings;
begin
  Rows := TStringList.Create;
  GetSelectedDataRows(Rows, false);
  for iRow := 0 to Rows.Count-1 do
    CloseRowOL( TAbstractRow(Rows.Objects[iRow]) );
  Rows.Free;
end;

procedure TGridEditor.OpenRow;
begin
  OpenRowOL(ActiveDataRow);
end;

procedure TGridEditor.OpenRowOL(ARow : TAbstractRow);

  function OpenRowWhenSingle(ThisRow : TAbstractRow) : TAbstractRow;
  var
    SubRow : TAbstractRow;
  begin
    if RowIsSubtotal( ThisRow ) then
    begin
      if RowSubRowCount( ThisRow ) = 1 then
      begin
        SubRow := RowSubRows( ThisRow, 0 );
        StorageOpenRow( ThisRow );
        if not HideSubtotalOnOpen and HideSubtotalForSingle then
          ThisRow.Visible := False;
        Result := OpenRowWhenSingle(SubRow);
      end
      else
        Result := ThisRow;
    end
    else
      Result := ThisRow;
  end;

var
  SubRow : TAbstractRow;
  SubCount, iSub : Integer;
begin
  if not RowIsSubtotal(ARow) then
    Exit;

  DefaultSortRows;

  Assert(CanOpenRowOL(ARow), Self.ClassName + '.OpenRow: Can not open row!');

  Disable;
  try
    SubCount := RowSubRowCount( ARow );
    if SubCount > 0 then
      SubRow := RowSubRows( ARow, 0 )
    else
      SubRow := nil;

    StorageOpenRow(ARow);

    if not HideSubtotalOnOpen then
    begin
      if HideSubtotalForSingle and
         (SubCount = 1) then
        ARow.Visible := False
      else
        ARow.Visible := True;
    end
    else
      ARow.Visible := False;

    if RecursiveOnSingleSubtotal and
       CanOpenRowOL(SubRow) and
       (SubCount = 1) then
      OpenRowOL(SubRow)
    else
    begin
      if FPervoOpenRow and RecursiveOnSingleSubtotal then
        for iSub := RowSubRowCount( ARow ) -1 downto 0 do
          SubRow := OpenRowWhenSingle( RowSubRows( ARow, iSub) );

      ArrangeRows;
      SetGridSize;
    end;

    if SubRow <> nil then
      EnableWithCoord(-1, GridIndexOfDataRow(SubRow))
    else
      Enable;
  except
    Enable;
  end;
end;

function TGridEditor.CanOpenRowOL(ARow : TAbstractRow) : Boolean;
begin
  if ARow = nil then
    Result := False
  else
    Result := RowStorage.CanOpenRow(ARow) and (ARow is TSubTotalRow);
end;

function TGridEditor.CanOpenRow : Boolean;
begin
  Result := CanOpenRowOL(ActiveDataRow);
end;

function TGridEditor.GetIndexOfSubTotalLevel(AField : TDataField) : Integer;
var
  iLevel : Integer;
begin
  Result := -1;
  if AField = nil then
    Result := 0
  else
    for iLevel := 0 to RowStorage.TreeKeyCount -1 do
      if RowStorage.TreeKey[iLevel].TreeKey = AField then
        Result := iLevel +1;
end;

function TGridEditor.GetParentSubtotalsVisible(ADataRow : TAbstractRow) : Boolean;
var
  ASubTotal : TSubTotalRow;
begin
  Result := False;

  if RowStorage.UsesCustomSortOrder then
    Exit;

  if ADataRow = nil then
    Exit;

  ASubTotal := ADataRow.SubTotalRow;

  try
    while ASubTotal <> nil do
    begin
      Result := ASubTotal.Visible;
      if Result then
        Break;
      ASubTotal := ASubTotal.SubTotalRow;
    end;
  except
    Result := False;
  end;
end;

procedure TGridEditor.HideSubTotalLevel(AField : TDataField);
var
  ThisRow : TAbstractRow;
  iLevel : Integer;
begin
  iLevel := IndexOfSubTotalLevel[AField];
  if iLevel < 0 then
    Exit;

  ThisRow := ActiveDataRow;
  Disable;
  try
    DefaultSortRows;
    if iLevel = 0 then
      RowStorage.Total.Visible  := False
    else
      RowStorage.TreeKey[iLevel].Visible := False;

    ArrangeRows;
    ActiveDataRow := ThisRow;
  finally
    Enable;
  end;
end;

function TGridEditor.SubTotalLevelVisible(AField : TDataField) : Boolean;
var
  iLevel : Integer;
begin
  Result := False;

  iLevel := IndexOfSubTotalLevel[AField];
  if iLevel < 0 then
    Exit;

  if iLevel = 0 then
    Result := RowStorage.Total.Visible
  else
    Result := RowStorage.TreeKey[iLevel].Visible;
end;

procedure TGridEditor.ShowSubTotalLevel(AField : TDataField);
var
  ThisRow : TAbstractRow;
  iLevel : Integer;
begin
  iLevel := IndexOfSubTotalLevel[AField];
  if iLevel < 0 then
    Exit;

  ThisRow := ActiveDataRow;
  Disable;
  try
    DefaultSortRows;
    if iLevel = 0 then
      RowStorage.Total.Visible := True
    else
      RowStorage.TreeKey[iLevel].Visible := True;

    ArrangeRows;
    SetGridSize;
    ActiveDataRow := ThisRow;
  finally
    Enable;
  end;
end;

procedure TGridEditor.ShowSubTotalLevelOnly(AField : TDataField);
var
  ThisRow : TAbstractRow;
  iLevel, iHideLevel : Integer;
begin
  iLevel := IndexOfSubTotalLevel[AField];
  if iLevel < 0 then
    Exit;

  ThisRow := ActiveDataRow;
  Disable;
  try
    DefaultSortRows;

    for iHideLevel := 0 to RowStorage.TreeKeyCount do
      RowStorage.TreeKey[iHideLevel].Visible := False;

    if iLevel = 0 then
      RowStorage.Total.Visible := True
    else
      RowStorage.TreeKey[iLevel].Visible := True;

    ArrangeRows;
    SetGridSize;
    ActiveDataRow := ThisRow;
  finally
    Enable;
  end;
end;

procedure TGridEditor.ShowHighestLevelOnly;
var
  iLevel : Integer;
  ThisRow : TAbstractRow;
begin
  ThisRow := ActiveDataRow;
  Disable;
  try
    DefaultSortRows;
    for iLevel := 0 to RowStorage.TreeKeyCount do
      RowStorage.TreeKey[iLevel].Visible := False;
    RowStorage.TreeKey[1].Visible := True;

    ArrangeRows;
    SetGridSize;
    ActiveDataRow := ThisRow;
  finally
    Enable;
  end;
end;

procedure TGridEditor.ShowDetailOnly;
var
  ThisRow : TAbstractRow;
begin
  ThisRow := ActiveDataRow;
  Disable;
  try
    DoShowDetailOnly;
    ArrangeRows;
    ActiveDataRow := ThisRow;
  finally
    Enable;
  end;
end;

procedure TGridEditor.DoShowDetailOnly;
var
  iLevel : Integer;
begin
  for iLevel := 0 to RowStorage.TreeKeyCount -1 do
    RowStorage.TreeKey[iLevel].Visible := False;
  RowStorage.DetailTreeKey.Visible := True;
end;

function TGridEditor.CanHideRow : Boolean;
begin
  if (ActiveDataRow = nil) or
     (ActiveDataRow is TDataRow) then
    Result := False
  else
    Result := ActiveDataRow.BuildingBlocksVisible;
end;

procedure TGridEditor.HideRow;
begin
  if CanHideRow then
  begin
    Disable;
    try
      ActiveDataRow.Visible := False;
      ArrangeRows;
    finally
      Enable;
    end;
  end;
end;

procedure TGridEditor.GetColumnHierarchyFields( GridCol : Integer; ASet : TFieldList );
var
  AField : TDataField;
  iField : Integer;
begin
  ASet.Clear;
  AField := RowViewDataField[PageView.RowView[0], GridCol];

  // Fixa MVJ: Vad händer om StandardView.DataTable <> RowStorage.DataTable
  if StandardView.DataTable.IndexOfField(AField) >= StandardView.DataTable.KeyCount then
    Exit;
  if not StandardView.DataTable.TableHasField(AField) then
    Exit;

  ASet.Add( AField );
  if StandardView.HierarchyKeys.ContainsField(AField) then
    for iField := AField.AncestorCount -1 downto 0 do
      if RowStorage.TreeKeyByField[ AField.Ancestor[iField] ] <> nil then
        ASet.Add( AField.Ancestor[iField] );
end;

procedure TGridEditor.CreateGrid;
begin
  Inherited CreateGrid;

  Grid.DisableKeyCombiantion( Key_Return, [] );
end;

procedure TGridEditor.CreateFormatter;
begin
  Inherited CreateFormatter;
end;

procedure TGridEditor.CreateCells;
begin
  inherited;

  FCell.OnCreateDrawCustomizer := CellCreateDrawCustomizer;
  FSetValueCell.OnCreateEditCustomizer := CellCreateEditCustomizer;
end;

function TGridEditor.CreateMarkRowCalcField : TDataField;
var
  ABitmap : TBitmap;
{$ifdef LINUX}
  Style: QStyleH;
  cg: QColorGroupH;
  aPalette : TWidgetPalette;
{$endif LINUX}
begin
  ABitmap := TBitmap.Create;
  FMarkRowPicture := TPicture.Create;
{$ifndef LINUX}
  ABitmap.Handle := LoadBitmap(0, PChar(OBM_RGARROW));

  with FMarkRowPicture.Bitmap do
  begin
    Height := 10;
    Width := 10;
    Canvas.CopyRect(Rect(0,0,10,10), ABitmap.Canvas, Rect(2,2,12,12));
  end;

{$else LINUX}
{  Style := QApplication_style;
  aPalette := TWidgetPalette.Create;
  aPalette.ColorRole := crButton;
  cg := aPalette.ColorGroup(cgActive);

  with ABitmap do
  begin
    Height := 14;
    Width := 14;
    QPainter_setRasterOp(Canvas.Handle, RasterOp_AndROP);
    QStyle_drawComboButton(Style, Canvas.Handle, 0, 0, Width, Height,
       cg, False, False, Enabled, nil);
  end;

  aPalette.Free;}
{$endif LINUX}

  ABitmap.Free;

  Result := TEditorPictureField.CreateOld('', Self, FMarkRowPicture);
end;

function TGridEditor.GetCriteria : TCondition;
begin
(*
  if RowStorage is TRowStorage then
    Result := TRowStorage(RowStorage).OpenCriteria
    *)
  if RowStorage is TCustomRowStorage then
    Result := TRowStorage(RowStorage).CommonCriteria
  else
    Result := nil;
end;

function TGridEditor.GetDataField(AGridRow, AGridCol : Integer) : TDataField;
var
  ARow : TAbstractRow;
  iField : Integer;
  TreeKey : TDataField;
begin
  if (AGridRow < 0) or (AGridCol < 0) then
    Result := SVEmptyField
  else if AGridRow < HeaderRowViewCount then
    Result := RowViewDataField[PageView.HeaderRowView[AGridRow], AGridCol]
  else if (AGridRow >= HeaderRowViewCount) and (DataRowIndexByGridIndex[AGridRow] < RowStorage.RowCount) then
  begin
    Result := RowViewDataField[RowViewByGridIndex[AGridRow], AGridCol];
    ARow := DataRowByGridIndex[AGridRow];
    if (ARow is TSubTotalRow) then
    begin
      if StandardView.HierarchyKeys.ContainsField(Result) and
         (TSubTotalRow(ARow).SubTotalKey.TreeKeyIndex > 0) then
      begin
        TreeKey := ARow.Storage.TreeKey[ TSubTotalRow(ARow).SubTotalKey.TreeKeyIndex -1 ].TreeKey;
        if TreeKey <> nil then
        begin
          for iField := 0 to Result.AncestorCount -1 do
            if Result.Ancestor[iField] = TreeKey then
            begin
              Result := Result.Ancestor[iField];
              Break;
            end;
        end;
      end;

      if not RowHasField(ARow, Result) then
        Result := SVEmptyField
      else if not Result.IsAggregable and
              not RowHasKey(ARow, Result) then
        Result := SVEmptyField;
    end;
  end
  else
    Result := SVEmptyField;
end;

function TGridEditor.GetRowViewDataField(ARowView : TAbstractRowView; AGridCol : Integer) : TDataField;
var
  RowViewCol : Integer;
begin
  if AGridCol < 0 then
  begin
    Result := SVEmptyField;
    Exit;
  end
  else if (AGridCol < 1) and  DoShowMarkRowCalcField then
  begin
    Result := FMarkRowCalcField;
    Exit;
  end;

  RowViewCol := FieldIndexByGridIndex[AGridCol];

  if RowViewCol >= PageView.ColCount then
    Result := SVEmptyField
  else
    Result := ARowView.Field[RowViewCol];
end;

function TGridEditor.GetFieldIndexByGridIndex(idxGridCol : Integer) : Integer;
begin
  Result := idxGridCol - AlwaysFixedColCount;
end;

function TGridEditor.GetGridIndexByFieldIndex(idxField : Integer) : Integer;
begin
  Result := idxField + AlwaysFixedColCount;
end;

function TGridEditor.GetGridIndexOfField(AField : TDataField) : Integer;
begin
  Result := AlwaysFixedColCount + PageView.ColOfField(AField);
end;

procedure TGridEditor.GetGridIndex(ARow : TAbstractRow; AField : TDataField; var iCol, iRow : Integer);
begin
  iCol := GridIndexOfField[AField];
  iRow := GridIndexOfDataRow(ARow) + MaxIntValue([PageView.RowViewIndexOfField(AField),0]);
end;

function TGridEditor.GetTotalColCount : Integer;
begin
  if PageView = nil then
    Result := 0
  else
    Result := PageView.ColCount + AlwaysFixedColCount;
end;

function TGridEditor.GetTotalRowCount : Integer;
var
  DataRowCount : Integer;
begin
  if RowStorage = nil then
    Result := 0
  else
  begin
    DataRowCount := RowStorage.RowCount * RowViewCount;
    Result := MaxIntValue([DataRowCount + HeaderRowViewCount, HeaderRowViewCount +1]);
  end;
end;

function TGridEditor.GetDataRowCount : Integer;
begin
  if RowStorage = nil then
    Result := 0
  else
    Result := RowStorage.RowCount;
end;

function TGridEditor.GetAlwaysFixedColCount : Integer;
begin
  Result := 0;
  if DoShowMarkRowCalcField then
    Result := MarkRowColCount;
end;

function TGridEditor.GetMarkRowColCount : Integer;
begin
  Result := 1;
end;

function TGridEditor.GetDataRowByGridIndex(idxGridRow : Integer) : TAbstractRow;
var
   idxDataRow : Integer;
begin
  idxDataRow := DataRowIndexByGridIndex[idxGridRow];
  Result := nil;

  if idxGridRow < HeaderRowViewCount then Exit
  else if (idxGridRow >= HeaderRowViewCount) and (idxDataRow < RowStorage.RowCount) then
    Result := TAbstractRow(RowStorage.Rows[idxDataRow]);
end;

function TGridEditor.GetDataRowIndexByGridIndex(idxGridRow : Integer) : Integer;
begin
  Result := ((idxGridRow - HeaderRowViewCount) div RowViewCount);
end;

function TGridEditor.GridIndexOfDataRow(DataRow : TAbstractRow) : Integer;
begin
  if (DataRow = nil) or (RowStorage.IndexOfRow(DataRow) = -1) then
  begin
    Result := -1;
    Exit;
  end;

  Result := HeaderRowViewCount + (RowStorage.IndexOfRow(DataRow) * RowViewCount);
  if Result >= Grid.RowCount then
  begin
    ArrangeRows;
    SetGridSize;
  end;
  Assert((Result < Grid.RowCount), Self.ClassName + '.GridIndexOfDataRow: '
     + TranslateMessage(E_IndexOutOfBounds) + ' ' + IntToStr(Result) + ' >= ' + IntToStr(Grid.RowCount));
end;

function TGridEditor.GetRowViewCount : Integer;
begin
  Result := PageView.RowViewCount;
end;

function TGridEditor.GetHeaderRowViewCount : Integer;
begin
  Result := PageView.HeaderRowViewCount;
end;

function TGridEditor.GetPageView : TPageView;
begin
  Result := FPageView;
end;

procedure TGridEditor.SetPageView(PageView : TPageView);
begin
  FPageView := PageView;
end;

function TGridEditor.GetActiveDataRow : TAbstractRow;
begin
  if not Assigned( RowStorage ) or RemovingActive then
    Result := nil
  else if not Assigned(Grid) then
    Result := RowStorage.FirstRow
  else
    Result := DataRowByGridIndex[Grid.Row];
end;

procedure TGridEditor.SetActiveDataRow(DataRow : TAbstractRow);
var
  iRow : Integer;
begin
  if (DataRow = ActiveDataRow) then
    Exit;

  PreviousRow := ActiveRow;
  iRow := GridIndexOfDataRow(DataRow);
  if iRow <> -1 then
    GridRow := iRow;

  if PreviousDataRow <> DataRow then
    ChangeDataRow;

  PreviousRow := iRow;
end;

procedure TGridEditor.SetActiveRowField( RowField : TRowFieldRecord );
var
  iCol, iRow : Integer;
begin
  if RowField.Row = nil then
    Exit;

  GetGridIndex( RowField.Row, RowField.Field, iCol, iRow );
  if (iRow >= 0) and
     (iCol >= 0) then
  begin
    Disable;
    EnableWithCoord( iCol, iRow );
  end;
end;

function TGridEditor.GetActiveRow : Integer;
begin
  if RemovingActive or
     not Assigned( Grid ) then
    Result := -1
  else
    Result := Grid.Row;
end;

procedure TGridEditor.SetActiveRow(Value : Integer);
begin
  PreviousRow := ActiveRow;

  if Value <> -1 then
    GridRow := Value;

  ChangeDataRow;
  PreviousRow := Value;
end;

function TGridEditor.GetActiveField : TDataField;
begin
  Result := DataField[Grid.Row, Grid.Col];
end;

procedure TGridEditor.SetActiveField( AField : TDataField );
var
  iRow, iCol : Integer;
begin
  if ActiveDataRow = nil then
    Exit;
    
  GetGridIndex(ActiveDataRow, AField, iCol, iRow );

  if iCol >= 0 then
  begin
    Disable;
    EnableWithCoord( iCol, iRow );
  end;
end;

function TGridEditor.GetActiveRowViewCol : Integer;
begin
  Result := FieldIndexByGridIndex[Grid.Col];
end;

function TGridEditor.GetPreviousDataRow : TAbstractRow;
begin
  Result := DataRowByGridIndex[PreviousRow];
end;

procedure TGridEditor.SetPreviousDataRow(ARow : TAbstractRow);
begin
  PreviousRow := GridIndexOfDataRow(ARow);
end;

procedure TGridEditor.ChangeSelection;
begin
  if Grid <> nil then
    Grid.Selection := GridRect( GridCoord(GridCol, GridRow), GridCoord(GridCol, GridRow) );
end;

function TGridEditor.GetDataRowSelected : Boolean;
begin
  Result := AllSelected or (SelectedDataRows.IndexOfObject(ActiveDataRow) >= 0);
end;

procedure TGridEditor.SetDataRowSelected(Value : Boolean);
var
  idxRow : Integer;
begin
  if CanSelectDataRow then
  begin
    idxRow := SelectedDataRows.IndexOfObject(ActiveDataRow);
    if not Value and AllSelected then
    begin
      AllSelected := False;
      SelectAllDataRowsButOne(ActiveDataRow);
      InvalidateGrid;
      Exit;
    end
    else if Value and (idxRow < 0)  then
      SelectedDataRows.AddObject('', ActiveDataRow)
    else if not Value and (idxRow >= 0) then
      SelectedDataRows.Delete(idxRow)
    else
      Exit;

    if SelectedDataRows.Count = RowStorage.RowCount then
    begin
      AllSelected := True;
      Exit;
    end;
    ChangeSelection;
    RepaintDataRow(ActiveDataRow);
  end;
end;

function TGridEditor.GetColSelected : Boolean;
begin
  Result := AllSelected or (SelectedCols.IndexOf(IntToStr(Grid.Col)) >= 0);
end;

function TGridEditor.CanSelectCol : Boolean;
begin
  Result := AllSelected or ( SelectedDataRows.Count = 0 );
end;

function TGridEditor.CanSelectDataRow : Boolean;
begin
  Result := AllSelected or ( SelectedCols.Count = 0 );
end;

procedure TGridEditor.SetColSelected(Value : Boolean);
begin
  if CanSelectCol then
  begin
    if not Value and AllSelected then
    begin
      AllSelected := False;
      SelectAllColsButOne(Grid.Col);
      InvalidateGrid;
      Exit;
    end
    else if Value then
      SelectedCols.Add(IntToStr(Grid.Col))
    else if not Value and ( SelectedCols.IndexOf(IntToStr(Grid.Col)) >= 0 ) then
      SelectedCols.Delete( SelectedCols.IndexOf(IntToStr(Grid.Col)) )
    else
      Exit;
    if SelectedCols.Count = TotalColCount - AlwaysFixedColCount then
    begin
      AllSelected := True;
      Exit;
    end;
    ChangeSelection;
    RepaintCol(Grid.Col);
  end;
end;

procedure TGridEditor.SetAllSelected(Value : Boolean);
begin
  FAllSelected := Value;
  SelectedCols.Clear;
  SelectedDataRows.Clear;
  ChangeSelection;
  InvalidateGrid;
end;

procedure TGridEditor.UnselectAll;
begin
  FAllSelected := False;
  SelectedCols.Clear;
  SelectedDataRows.Clear;
  ChangeSelection;
end;

procedure TGridEditor.SelectAllColsButOne(idx : Integer);
var
  iCol : Integer;
begin
  for iCol := AlwaysFixedColCount to Grid.ColCount -1 do
    if iCol <> idx then
      SelectedCols.Add(IntToStr(iCol));
  ChangeSelection;
end;

procedure TGridEditor.SelectAllDataRowsButOne(ARow : TAbstractRow);
var
  iRow : Integer;
  ThisRow : TAbstractRow;
begin
  for iRow := 0 to RowStorage.RowCount - 1 do
  begin
    ThisRow := RowStorage.Rows[iRow];
    if ThisRow <> ARow then
      SelectedDataRows.AddObject('', ThisRow);
  end;
  ChangeSelection;
end;

function TGridEditor.GetContainsSelection : Boolean;
begin
  Result := AllSelected or
            (SelectedCols.Count > 0) or
            (SelectedDataRows.Count > 0);
end;

function TGridEditor.GetContainsGridSelection : Boolean;
var
  ARect : TRect;
begin
  if not (dgAlwaysShowEditor in Grid.DBUOptions) and
     Assigned(Grid.DBUInplaceEdit) and
     not Grid.DBUInplaceEdit.EditorMode then
    Result := True
  else
  begin
    GetGridSelection( ARect );
    ARect := ArrangeRect( ARect );
    Result := ( ARect.Top < ARect.Bottom ) or
              ( ARect.Left < ARect.Right );
  end;
end;

procedure TGridEditor.GetTotalSelection( SelList : TList );

  procedure AddRect( ARect : TRect; var APRect : PRect );
  begin
    New( APRect );
    APRect^ := ARect;
    SelList.Add( APRect );
  end;

  procedure InsertRect( idx : Integer; ARect : TRect );
  var
    APRect : PRect;
  begin
    New( APRect );
    APRect^ := ARect;
    if idx >= 0 then
      SelList.Insert( idx, APRect )
    else
      SelList.Add( APRect );
  end;

  procedure MergeWithNext( ThisPRect : PRect; iNextRect : Integer );
  var
    NextPRect : PRect;
  begin
    if iNextRect >= SelList.Count then
      Exit;

    NextPRect := PRect(SelList[iNextRect]);

    if NextPRect.Top <= ThisPRect^.Bottom + 1 then
    begin
      if NextPRect.Bottom < ThisPRect^.Bottom then
        MergeWithNext( ThisPRect, iNextRect + 1 )
      else
        ThisPRect.Bottom := NextPRect.Bottom;

      SelList.Delete( iNextRect );
    end;
  end;

  function TryMergeWithRect( APRect : PRect; ARect : TRect; iRect : Integer ) : Boolean;
  begin
    Result := False;
    if (APRect^.Top <= ARect.Bottom +1 ) and
       (APRect^.Top > ARect.Top) then
    begin
      APRect^.Top := ARect.Top;
      Result := True;
    end;

    if APRect^.Bottom < ARect.Bottom then
    begin
      APRect^.Bottom := ARect.Bottom;
      MergeWithNext( APRect, iRect + 1 );
      Result := True;
    end;
  end;

  procedure AddRowRect( ARect : TRect );
  var
    APRect : PRect;
    iRect : Integer;
    Inserted : Boolean;
  begin
    Inserted := False;
    for iRect := 0 to SelList.Count -1 do
    begin
      APRect := PRect(SelList[iRect]);

      // Don't try to mix row selections of different colcounts
      Assert( ( APRect^.Left = ARect.Left ) and ( APRect^.Right = ARect.Right ) );

      if ( APRect^.Bottom +1 ) >= ARect.Top then
      begin
        Inserted := TryMergeWithRect( APRect, ARect, iRect );

        if (APRect^.Top > ARect.Bottom) and
           not Inserted then
        begin
          InsertRect( iRect, ARect );
          Inserted := True;
        end;

        Break;
      end
    end;

    if not Inserted then
      InsertRect( -1, ARect );
  end;

var
  i, iCol, iRow : Integer;
  ARect : TRect;
  APRect : PRect;
begin
  APRect := nil;
  if ContainsSelection then
  begin
    if AllSelected then
    begin
      ARect.Top := HeaderRowViewCount;
      ARect.Left := GridIndexByFieldIndex[ PageView.RowView[0].DisplayKeyList.Count -1];
      ARect.Bottom := Grid.RowCount -1;
      ARect.Right := Grid.ColCount -1;
      AddRect( ARect, APRect );
    end
    else if SelectedCols.Count > 0 then
    begin
      ARect.Top := HeaderRowViewCount;
      ARect.Bottom := Grid.RowCount -1;
      for i := 0 to SelectedCols.Count -1 do
      begin
        iCol := AsInteger( SelectedCols.Values[i] );
        if ( i > 0 ) and
           ( APRect^.Right = ( iCol - 1 ) ) then
          APRect^.Right := iCol
        else
        begin
          ARect.Left := iCol;
          ARect.Right := iCol;
          AddRect( ARect, APRect );
        end;
      end;
    end
    else if SelectedDataRows.Count > 0 then
    begin
      ARect.Left := GridIndexByFieldIndex[ PageView.RowView[0].DisplayKeyList.Count -1];
      ARect.Right := Grid.ColCount -1;
      for i := 0 to SelectedDataRows.Count -1 do
      begin
        iRow := GridIndexOfDataRow( SelectedDataRows.AbstractRows[i] );
        Assert( iRow >= 0 );
        ARect.Top := iRow;
        ARect.Bottom := iRow + RowViewCount -1;
        AddRowRect( ARect );
      end;
    end;
  end
  else if ContainsGridSelection then
  begin
    GetGridSelection(ARect);
    AddRect( ARect, APRect );
  end;
end;

procedure TGridEditor.SetTotalViewer(Viewer : TTotalViewer);
begin
  FTotalViewer := Viewer;
end;

function TGridEditor.GetKeyInThisTable(AField : TDataField; ATable : TDataTable) : Boolean;
begin
  Result := (AField <> nil) and 
            (AField.AuxTable <> nil) and
            (AField.AuxTable.AuxTableKey = AField) and
            (ATable = AField.AuxTable);
end;

function TGridEditor.GetFieldKeyFormatting(AField : TDataField) : TDisplayValues;
begin
  if not KeyInThisTable[AField, RowStorage.DataTable] or ReadOnly then
    Result := Self.KeyFormatting
  else
    Result := dvKeyOnly;
  if Assigned(OnGetKeyFormatting) then
    OnGetKeyFormatting(Self, AField, Result);
end;

procedure TGridEditor.SetKeyFormatting(Value : TDisplayValues);
var
  idx : Integer;
begin
  FKeyFormatting := Value;
  if PageView <> nil then
    for idx := 0 to PageView.HeaderRowViewCount -1 do
      PageView.HeaderRowView[idx].Minimize := (Value = dvKeyOnly);
end;

procedure TGridEditor.SetDialogBoxFields(Fields: TDataFieldSet;
  OnEditButtonClick: TOnEditButtonClickEvent);
begin
  FOnEditButtonClick := OnEditButtonClick;
  if FDialogBoxFields = nil then
    FDialogBoxFields := TDataFieldSet.Create
  else
    FDialogBoxFields.Clear;

  FDialogBoxFields.CopyFrom(Fields);
end;

procedure TGridEditor.SetShowSubTotals(Value : Boolean);
begin
  FShowSubTotals := Value;
  if Assigned(RowStorage) and RowStorage.CanHaveTotals then
    RowStorage.ShowSubTotals := Value;
  if IsActive then
  begin
    SetGridSize;
    InvalidateGrid;
  end;
end;

function TGridEditor.GetShowStripes : Boolean;
begin
  Result := FShowStripes;
end;

procedure TGridEditor.SetShowStripes(Value : Boolean);
begin
  FShowStripes := Value;

  if IsActive then
    InvalidateGrid;
end;

procedure TGridEditor.CommitChanges;
begin
  if Grid <> nil then
  begin
    Grid.CommitChanges;
//    Grid.Enabled := False;
//    Grid.Enabled := True;
  end;
end;

function TGridEditor.CellIsSelected(ACol, ARow : Integer) : Boolean;
var
  ARect : TRect;
begin
  with Grid.Selection do
    ARect := Rect( Left, Top, Right, Bottom );

  Result := AllSelected or
             (SelectedDataRows.IndexOfObject(DataRowByGridIndex[ARow]) >= 0) or
             (SelectedCols.IndexOf(IntToStr(ACol)) >= 0) or
             Grid.IsCellSelected(ACol, ARow) and
             not EqualRect( ARect, Rect( ACol, ARow, ACol, ARow ) );
end;

function TGridEditor.CanEditValue(ARow : TAbstractRow; ADataField : TDataField; var ReadOnlyReason : String) : Boolean;
begin
  Result := not FieldIsReadOnly(ADataField, ARow);
  if not Result then
    ReadOnlyReason := 'Field ''' + ADataField.FieldName + ''' is read only for this row!';
end;

function TGridEditor.FieldIsReadOnly(ADataField : TDataField; ADataRow : TAbstractRow) : Boolean;

  function RowIsValidDependsOnField : Boolean;
  var
    RowIsValidField : TRowIsValidField;
  begin
    Result := False;
    if StandardView.RowIsValidField is TRowIsValidField then
    begin
      RowIsValidField := TRowIsValidField(StandardView.RowIsValidField);
      if RowIsValidField.ConditionFieldForSource[ ADataField ] <> nil then
        Result := True;
    end;
  end;

  function RowViewReadOnly : Boolean;
  var
    iRowView : Integer;
  begin
    iRowView := PageView.RowViewIndexOfField( ADataField );
    if iRowView >= 0 then
      Result := PageView.RowView[iRowView].ReadOnly
    else
      Result := False;
  end;

var
  IsAbsolute : Boolean;
begin
  IsAbsolute := False;

  if ReadOnly or
     RowViewReadOnly or
     (ADataField = nil) or
     (ADataRow = nil) or
     ADataField.ReadOnly[ADataRow] or
     DataRowIsReadOnly( ADataRow ) then
    Result := True
  else if RowStorage.RowIsUnaccepted(ADataRow) and
       RowIsValidDependsOnField then
    Result := False    
  else if FieldIsReadOnlyByProperty(ADataField, ADataRow, IsAbsolute) then
    Result := True
  else if IsAbsolute then
    Result := False
  else if RowHasKey( ADataRow, ADataField ) then
    Result := KeyIsReadOnly(ADataField, ADataRow)
  else if not RowStorage.DataTable.TableHasField( ADataField ) then
    Result := OtherFieldIsReadOnly(ADataField, ADataRow)
  else
    Result := NonKeyIsReadOnly(ADataField, ADataRow);
end;

function TGridEditor.KeyIsReadOnly(ADataField : TDataField; ADataRow : TAbstractRow) : Boolean;

  function FieldIsRunningKeyInTable : Boolean;
  begin
    Result := ( ADataRow.DataTable.RunningNumberField = ADataField );
  end;

  function FieldIsTableKeyNotInRow : Boolean;
  begin
    Result := ADataRow.DataTable.TableHasKey( ADataField ) and not RowHasKey( ADataRow, ADataField );
  end;

begin
  if FieldIsRunningKeyInTable or FieldIsTableKeyNotInRow then
    Result := True
  else if ADataRow is TDataRow then
    Result := DataRowKeyIsReadOnly(ADataField, TDataRow(ADataRow))
  else if ADataRow is TSubTotalRow then
    Result := SubTotalKeyIsReadOnly(ADataField, TSubTotalRow(ADataRow))
  else
    raise Exception.Create( Self.ClassName + '.KeyIsReadOnly: Can''t recognize ' + ADataRow.ClassName );
end;

function TGridEditor.NonKeyIsReadOnly(ADataField: TDataField;
  ADataRow: TAbstractRow): Boolean;
begin
  if RowStorage.RowIsUnaccepted( ADataRow ) then
    Result := ADataRow is TSubTotalRow                                                                 // This row is unaccepted
  else if ADataRow is TDataRow then
    Result := DataRowNonKeyIsReadOnly(ADataField, TDataRow(ADataRow))
  else if ADataRow is TSubTotalRow then
    Result := SubTotalNonKeyIsReadOnly(ADataField, TSubTotalRow(ADataRow))
  else
    raise Exception.Create( Self.ClassName + '.NonKeyIsReadOnly: Can''t recognize ' + ADataRow.ClassName );
end;

function TGridEditor.OtherFieldIsReadOnly(ADataField : TDataField; ADataRow : TAbstractRow) : Boolean;
begin
  if RowStorage.RowIsUnaccepted( ADataRow ) then
    Result := ADataRow is TSubTotalRow                                                              // This row is unaccepted
  else if ADataRow is TDataRow then
    Result := DataRowOtherFieldIsReadOnly(ADataField, TDataRow(ADataRow))
  else if ADataRow is TSubTotalRow then
    Result := SubTotalOtherFieldIsReadOnly(ADataField, TSubTotalRow(ADataRow))
  else
    raise Exception.Create( Self.ClassName + '.OtherFieldIsReadOnly: Can''t recognize ' + ADataRow.ClassName );
end;

function TGridEditor.DataRowKeyIsReadOnly(ADataField: TDataField;
  ADataRow: TDataRow): Boolean;
begin
  Result := False;
end;

function TGridEditor.DataRowNonKeyIsReadOnly(ADataField: TDataField;
  ADataRow: TDataRow): Boolean;
begin
  Result := False;
end;

function TGridEditor.DataRowOtherFieldIsReadOnly(ADataField: TDataField;
  ADataRow: TDataRow): Boolean;
begin
  Result := False;
end;

function TGridEditor.SubTotalKeyIsReadOnly(ADataField: TDataField;
  ADataRow: TSubTotalRow): Boolean;
begin
  Result := not RowStorage.RowIsUnaccepted( ADataRow );
end;

function TGridEditor.SubTotalLevelEditable(ARow : TSubTotalRow) : Boolean;



begin

  Result := False;

end;

function TGridEditor.SubTotalNonKeyIsReadOnly(ADataField: TDataField;
  ADataRow: TSubTotalRow): Boolean;
begin
  if ADataRow.DataTable.TableHasKey( ADataField ) then
    Result := True
  else if ADataField.IsAggregable then
  begin
    if SubTotalLevelEditable(ADataRow) then
      Result := False                                                           // This level is editable
    else
      Result := True;
  end
  else
    Result := True;
end;

function TGridEditor.SubTotalOtherFieldIsReadOnly(ADataField: TDataField;
  ADataRow: TSubTotalRow): Boolean;
begin
  if ADataField.IsAggregable then
  begin
    if SubTotalLevelEditable(ADataRow) then
      Result := False                                                           // This level is editable
    else
      Result := True;
  end
  else
    Result := True;
end;

function TGridEditor.FieldIsReadOnlyByProperty(ADataField : TDataField; ADataRow : TAbstractRow; var IsAbsolute : Boolean) : Boolean;
var
  ReadOnlyProperty : TGridEditorReadOnly;
  RowUnaccepted : Boolean;
  iField : Integer;
  AField : TDataField;
begin
  Result := False;
  RowUnaccepted := RowStorage.RowIsUnaccepted(ADataRow);
  ReadOnlyProperty := TGridEditorProperties.GetReadOnlyObject(StandardView.ReadOnlyList, ADataField);

  if ReadOnlyProperty <> nil then
  begin
  // Does this row contain all fields needed for checking the value of the property?
    for iField := 0 to ReadOnlyProperty.DependFields.Count -1 do
    begin
      AField := ReadOnlyProperty.DependFields.Field[iField];
      if DisabledFieldList.ContainsField( AField ) or
         not RowHasField( ADataRow, AField ) then
        Exit;
    end;

    IsAbsolute := ReadOnlyProperty.IsAbsolute;
    // Only accepted rows or properties with IsAbslute=true can make the row readonly
    if IsAbsolute or not RowUnaccepted then
    begin
      Result := ReadOnlyProperty.GetIsReadOnly(ADataRow);
      IsAbsolute := IsAbsolute and Result;
    end;
  end;
end;

function TGridEditor.DataRowIsReadOnly(ADataRow : TAbstractRow) : Boolean;
var
  iItem : Integer;
  AList : TValueList;
begin
  if ReadOnly then
    Result := True
  else if not Assigned(ADataRow) then
    Result := False
  else if ConstantBufferEdit and
      (ADataRow = GetBufferRow(ADataRow)) then
    Result := True
  else if RowStorage.RowIsUnaccepted(ADataRow) then
    Result := False
  else
  begin
    Result := False;
    AList := StandardView.DataRowReadOnlyList;
    for iItem := 0 to AList.Count -1 do
    begin
      Result := Result or TGridEditorReadOnly(AList.Objects[iItem]).GetIsReadOnly(ADataRow);
      if Result then
        Break;
    end;
  end;
end;

function TGridEditor.ColumnIsReadOnly(AGridCol : Integer) : Boolean;
var
  iRow : Integer;
begin
  Result := True;

  if AGridCol >= TotalColCount then
    Exit;

  for iRow := 0 to PageView.RowViewCount -1 do
  begin
    Result := Result and FieldIsReadOnly(RowViewDataField[PageView.RowView[iRow], AGridCol], ActiveDataRow);
    if not Result then
      Break;
  end;
end;

procedure TGridEditor.SetGridSize;
begin
  with PageView do
  begin
    if (TotalColCount <= 0) and
       (TotalRowCount <= 0) then
    begin
      Grid.FixedCols := 0;
      Grid.ColCount := 1;
      Grid.RowCount := 1;
    end
    else
    begin
      Grid.ColCount := MaxIntValue([TotalColCount, 1]);
      Grid.RowCount := MaxIntValue([TotalRowCount, 1]);

      Grid.FixedCols := FFixedCols;
      Grid.FixedRows := HeaderRowViewCount;
    end;
  end;
  if TotalViewer <> nil then
    UpdateTotalsSize;
end;

procedure TGridEditor.SetIsActive(Value : Boolean);
begin
  if not Visible and Value then
    Show;
  UnselectAll;
  Inherited SetIsActive(Value);
  ActiveRow := ActiveRow;
end;

function TGridEditor.DoShowMarkRowCalcField : Boolean;
begin
  Result := ShowMarkRowCalcField and (MarkRowCalcField <> nil);
end;

procedure TGridEditor.SetMarkRowCalcField(AField : TDataField);
var
  OldValue, ValueChanged : Boolean;
  ActiveCol : Integer;
begin
  OldValue := DoShowMarkRowCalcField;
  FMarkRowCalcField := AField;

  ValueChanged := (OldValue <> DoShowMarkRowCalcField);

  if ValueChanged and not Executing then
  begin
    ActiveCol := Grid.Col;

    Disable;

    AdjustFixedCols;

    if DoShowMarkRowCalcField  then
      EnableWithCoord(ActiveCol + MarkRowColCount, -1)
    else
      EnableWithCoord(ActiveCol - MarkRowColCount, -1);
  end;
end;

procedure TGridEditor.SetShowMarkRowCalcField(Value : Boolean);
var
  OldValue, ValueChanged : Boolean;
  ActiveCol : Integer;
begin
  Disable;
  try
    ActiveCol := -1;
    if Grid <> nil then
      ActiveCol := Grid.Col;

    OldValue := DoShowMarkRowCalcField;
    FShowMarkRowCalcField := Value;
    ValueChanged := (OldValue <> DoShowMarkRowCalcField);

    if ValueChanged and
       (Grid <> nil) then
    begin
      AdjustFixedCols;

      if DoShowMarkRowCalcField then
        EnableWithCoord(ActiveCol + MarkRowColCount, -1)
      else
        EnableWithCoord(ActiveCol - MarkRowColCount, -1);
    end
    else
      Enable;
  except
    Enable;
  end;
end;

procedure TGridEditor.AdjustFixedCols;
var
  iCol : Integer;
  TempList : TStringList;
begin
  if (RowStorage = nil) or
     (PageView = nil) then
    Exit;

  TempList := TStringList.Create;
  for iCol := 0 to Grid.ColCount -1 do
    TempList.Add(IntToStr(Grid.ColWidths[iCol]));

  SetGridSize;

  if DoShowMarkRowCalcField then
  begin
    for iCol := TotalColCount - 1 downto 0 do
      if iCol - MarkRowColCount >= 0 then
        SetGridColWidth(iCol, StrToInt(TempList[iCol - MarkRowColCount]))
      else
        Grid.ColWidths[iCol] := COLWIDTH_MarkCol;

    for iCol := 0 to SelectedCols.Count - 1 do
      SelectedCols.Values[iCol] := SelectedCols.DataType.Sum( SelectedCols.Values[iCol], ValueFromInteger( MarkRowColCount ) );
  end
  else
  begin
    for iCol := AlwaysFixedColCount to TotalColCount - 1 do
      SetGridColWidth(iCol, StrToInt(TempList[iCol + MarkRowColCount]));

    for iCol := 0 to SelectedCols.Count - 1 do
      SelectedCols.Values[iCol] := SelectedCols.DataType.Difference( SelectedCols.Values[iCol], ValueFromInteger( MarkRowColCount ) );
  end;

  if DoShowMarkRowCalcField then
    FixedCols := FixedCols + MarkRowColCount
  else
    FixedCols := FixedCols - MarkRowColCount;

  TempList.Free;

  CheckTotalViewerColWidths;
end;

procedure TGridEditor.SetEnableSorting(Value : Boolean);
begin
  FEnableSorting := Value;
  if not Value and
     not Executing then
    DefaultSortRows;
end;

procedure TGridEditor.GridGetCell( Sender : IDataEditorCellInterface );
begin
  inherited GridGetCell(Sender);

  if (Sender.Row = Grid.TopRow) and ((Sender.Col = Grid.LeftCol) or (Sender.Col = Grid.LeftCol + Grid.VisibleColCount -1)) then
    CheckTotalGridCorrelation;
end;

procedure TGridEditor.GridSetCell( Sender : IDataEditorCellInterface );
begin
  inherited GridSetCell(Sender);
end;

procedure TGridEditor.DefineCell( Cell : IDataEditorCellInterface );

  function HasCombo(AField : TDataField) : Boolean;
  begin
    Result := (not ReadOnly) and
              AField.HasCombo and
              not KeyInThisTable[AField, RowStorage.DataTable];
  end;

  function HasButton(AField : TDataField) : Boolean;
  begin
    Result := (DialogBoxFields <> nil) and
              DialogBoxFields.ContainsField(AField);
  end;

begin
  inherited DefineCell(Cell);
  with Cell do
  begin
    DataRow := Self.DataRowByGridIndex[Cell.Row];

    if (Cell.Row < HeaderRowViewCount) and (RowViewCount > 0) then
      GetDBUFormatter.HAlignment := DefineCellAlignment(RowViewDataField[RowViewByGridIndex[HeaderRowViewCount], Cell.Col]);
//      GetDBUFormatter.HAlignment := DefineCellAlignment(Self.DataField[PageView.HeaderRowViewCount, Cell.Col]);

    ReadOnly := FieldIsReadOnly(DataField, DataRow);
    KeyFormatting := GetFieldKeyFormatting(DataField);

    if DataField <> nil then
    begin
      if HasButton(DataField) then
        CellType := ButtonCellType
      else if (DataField.DataType is TBooleanType) and ShowAllBooleansAsCheckBoxes then
      begin
        CellType := CheckCellType;
        KeyFormatting := dvKeyOnly;
      end
      else if HasCombo(DataField) then
        CellType := ComboCellType
      else if (Cell.Row < HeaderRowViewCount) and not(RowViewDataField[PageView.RowView[0], Cell.Col] is TConstantField) then
        CellType := ClickableHeaderCellType;
    end;

    if Assigned(FOnDefineCell) then
      OnDefineCell( Self, Cell );
  end;
end;

function TGridEditor.CellCreateEditCustomizer( Sender : TDBUEditorCell;
  CellType : TDBUCellType ) : TDBUCustomizer;
var
  AField : TDataField;
  AMax : Integer;
begin
  AField := SetValueCell.DataField;
  if (AField <> nil) and
     (AField.DataType is TStringType) and
     ((AField.DisplayValues = dvKeyOnly) or
      (AField.TextField <> nil)) and
     not AField.HasCombo then
  begin
    AMax := TStringType(AField.DataType).Size
  end
  else
    AMax := -1;

  Result := CellType.CreateEditCustomizer( Sender );
  if Result is TComboCellTypeCustomizer then
  begin
    TComboCellTypeCustomizer(Result).OnGetStrings := ComboCellTypeGetStrings;
    TComboCellTypeCustomizer(Result).OnCreateCombo := ComboCellTypeCreateCombo;
    TComboCellTypeCustomizer(Result).OnComboSelectionChange := ComboSelectionChange;
  end
  else if Result is TButtonCellTypeCustomizer then
    TButtonCellTypeCustomizer(Result).OnClick := CellButtonClick
  else if Result is TCheckCellTypeCustomizer then
    TCheckCellTypeCustomizer(Result).CheckInfoSupplier := Self;

  if (AMax > 0) and (Result is TDefaultCustomizer) then
    TDefaultCustomizer(Result).MaxLength := AMax;
end;

function TGridEditor.CellCreateDrawCustomizer( Sender : TDBUCell; CellType : TDBUCellType ) : TDBUCustomizer;
begin
  Result := CellType.CreateDrawCustomizer( Sender );
  if Result is THeaderDrawCustomizer then
    THeaderDrawCustomizer(Result).IconSupplier := Self;
  if Result is TClickableHeaderDrawCustomizer then
    TClickableHeaderDrawCustomizer(Result).OnClick := CellHeaderClick;
  if Result is TCheckCellTypeCustomizer then
    TCheckCellTypeCustomizer(Result).CheckInfoSupplier := Self;
end;

procedure TGridEditor.CellHeaderClick( Sender : TObject; ACell : IInfoCell; AMouseState : TMouseState );
var
  AField : TDataField;
begin
  with ACell, AMouseState do
  begin
    if (Col >= 0) and (Col < AlwaysFixedColCount) and
       (Row >= 0) and (Row < PageView.HeaderRowViewCount) then
    begin
      DefaultSortRows;
    end
    else if (Col >= 0) and (Col < AlwaysFixedColCount) then
    begin
      if Assigned(OnMarkRowColClickEvent) then
        OnMarkRowColClickEvent(Self, Button, Shift, Row, Col, EditorCell);
    end
    else if (Row >= 0) and (Row < PageView.HeaderRowViewCount) then
    begin
      AField := DataField[Row, Col];
      if ( AField is THeaderField ) and
         Assigned(THeaderField(AField).DataField) then
        SortByField(THeaderField(AField).DataField);
      if Assigned(OnHeaderClickEvent) then
        OnHeaderClickEvent(Self, Button, Shift, Row, Col, EditorCell);
    end;
  end;
end;

procedure TGridEditor.GetIcon(ACell : IInfoCell; var APicture : TPicture;
      var Align : TDBUAlign; var Margin : TDBUMargin );
var
  Dummy : Boolean;
begin
  CellGetIcon( EditorCell, APicture, Dummy );
  if ACell.Col < AlwaysFixedColCount then
    Align.H := taCenter
  else
    Margin.H := 3;
end;

function TGridEditor.HasIcon( ACell : IInfoCell ) : Boolean;
begin
  Result := (ACell.Row < HeaderRowViewCount) and  GetShowIcon(ACell.Col);
end;

procedure TGridEditor.GetDescription( ACell : IInfoCell; var Descr : TValue );
var
  AField : TDataField;
  ARow : TAbstractRow;
begin
  if ACell.SelfObject is TDataEditorCell then
  begin
    AField := TDataEditorCell(ACell.SelfObject).DataField;
    ARow := TDataEditorCell(ACell.SelfObject).DataRow;
  end
  else if ACell.SelfObject is TDataInplaceEditorCell then
  begin
    AField := TDataInplaceEditorCell(ACell.SelfObject).DataField;
    ARow := TDataInplaceEditorCell(ACell.SelfObject).DataRow;
  end
  else
  begin
    AField := nil;
    ARow := nil;
  end;

  if (AField <> nil) and
     (ARow <> nil) then
    Descr := ValueFromString( ARow.DisplayText[ AField ] );
end;

function TGridEditor.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TGridEditor._AddRef: Integer;
begin
  result := 1;
end;

function TGridEditor._Release: Integer;
begin
  result := 1;
end;

procedure TGridEditor.ComboCellTypeCreateCombo( Cell : IEditorCell;
    Identifier : TObject; var ACombo : TDBUComboBox );
begin
  if Assigned( OnCreateCombo ) then
    OnCreateCombo( Cell, Identifier, ACombo );

  if not Assigned( ACombo ) then
    ACombo := TDBUDataCombo.Create;

  ACombo.OnlyLegalValues := StandardView.AllowOnlyLegalComboValues;
  ACombo.AutoComplete := True;
  ACombo.CacheValues := False;
  ACombo.EmptyItem := PageView.EmptyItem[SetValueCell.DataField];
end;

procedure TGridEditor.ComboCellTypeGetStrings( Cell : IEditorCell; Combos : TPickListOrganizer;
  var ACombo : TDBUComboBox );
var
  AField : TDataField;
begin
  AField := SetValueCell.DataField;
  ACombo := Combos.Combos[ AField ];
  CellGetStrings( Self, ACombo, AField, SetValueCell.DataRow );
end;

procedure TGridEditor.ComboSelectionChange( Cell : IEditorCell; ACombo : TDBUComboBox; var SetValue : TValue );
var
  AObj : TObject;
  ARow : TAbstractRow;
begin
  if ACombo.ItemIndex >= 0 then
  begin
    AObj := ACombo.Items.Objects[ ACombo.ItemIndex ];
    if AObj is TAbstractRow then
    begin
      ARow := TAbstractRow(AObj);
      SetValue := ARow[SetValueCell.DataField];
    end;
  end;
end;

procedure TGridEditor.CellGetStrings(Sender: TObject; ACombo : TDBUComboBox;
  DataField : TDataField; DataRow : TAbstractRow);

  procedure ClearAncestors( ACrit : TCriteria; AField : TDataField );
  var
    i : Integer;
  begin
    ACrit[AField].Reset;
    for i := 0 to AField.AncestorCount -1 do
      ACrit[AField.Ancestor[i]].Reset;
  end;

  function CreateRowCriteria( ARow : TAbstractRow; AField : TDataField ) : TCondition;
  var
    tmpCrit : TCriteria;
  begin
    tmpCrit := TCriteria.CreateFromRowKeys( ARow );
    repeat
      ClearAncestors( tmpCrit, AField );
      if AField = AField.AuxTableField then
        Break
      else
        AField := AField.AuxTableField;
    until (AField = nil);

    Result := TAndCondition.CreateFromArray( [tmpCrit, Criteria.CreateCopy] );
  end;

var
  CurrentCrit, ComboCrit : TCondition;
  FillStamp : TDateTime;
  ComboValueProperty : TGridEditorComboValues;
  DefaultFill, UseDefault : Boolean;
  aStrings : TDataRowList;
  iField : Integer;
begin
  aStrings := TDataRowList(ACombo.Items);
  aStrings.BeginUpdate;

  DefaultFill := True;
  if Assigned(FOnCellComboStringsFilling) then
    OnCellComboStringsFilling(Sender, aStrings, DataField, DataRow, DefaultFill);

  if DefaultFill then
  begin
    CurrentCrit := CreateRowCriteria( DataRow, DataField );

    if ACombo is TDBUDataCombo then
    begin
      ComboCrit := TDBUDataCombo(ACombo).FillCriteria;
      FillStamp := TDBUDataCombo(ACombo).FillTimeStamp;
    end
    else
    begin
      ComboCrit := nil;
      FillStamp := 0;
    end;

    ComboValueProperty := TGridEditorProperties.GetComboValuesObject(StandardView.ComboValuesList, DataField);

    UseDefault := False;
    if Assigned( ComboValueProperty ) then
    begin
      ACombo.CacheValues := False;
      for iField := 0 to ComboValueProperty.DependFields.Count -1 do
        if DisabledFieldList.ContainsField( ComboValueProperty.DependFields.Field[iField]) then
          UseDefault := True;
    end
    else
      UseDefault := True;

    if Assigned( ComboValueProperty ) and
       not UseDefault then
      ComboValueProperty.GetStrings(aStrings, Criteria, DataRow)
    else if DataField.DoRefill( CurrentCrit, ComboCrit, FillStamp) then
    begin
      aStrings.Clear;
      DataField.GetValues( aStrings, DataRow.DataTable, DataRow, CurrentCrit );
      aStrings.FillStringsOptional(DataField, GetFieldKeyFormatting(DataField));
    end;

    if ACombo is TDBUDataCombo then
    begin
      TDBUDataCombo(ACombo).FillCriteria := CurrentCrit;
      TDBUDataCombo(ACombo).FillTimeStamp := Now;
    end;

    CurrentCrit.Free;
  end;

  aStrings.EndUpdate;
end;

function TGridEditor.FieldBelongsToTable(AField : TDataField; ARow : TAbstractRow) : Boolean;
var
  AList : TFieldList;
  RowView : TRowView;
  idxRowView : Integer;
begin
  if AField.FieldName = '' then
    Result := False
  else
  begin
    Result := RowStorage.DataTable.TableHasNonKey( AField );
    if not Result then
    begin
      idxRowView := PageView.RowViewIndexOfField( AField );
      if idxRowView<>-1 then
      begin
        RowView := PageView.RowView[idxRowView];
        AList := RowView.FieldList;
        if AList <> nil then
          Result := AList.ContainsField(AField);
      end
      else
      begin
        // LAA 28032001: Tillsatt med MVJ på telefon pga CACCTGROUPT-aktiga fält
        if AField.LookupField<>nil then
          result := FieldBelongsToTable(AField.LookupField, ARow);
      end;
    end;
  end;
end;

procedure TGridEditor.GridHScroll(Sender: TObject; ScrollCode,
  Pos: Smallint);
begin
  RedrawAntArea;

  if (TotalViewer <> nil) and not Executing then
    TotalViewer.ScrollGrid;

  if Assigned( OnGridHScroll ) then
    OnGridHScroll( Sender, ScrollCode, Pos );
  if Assigned( OnGridCheckScroll ) then
    OnGridCheckScroll( Grid );
end;

procedure TGridEditor.GridVScroll(Sender: TObject; ScrollCode,
  Pos: Smallint);
begin
  RedrawAntArea;

  if Assigned( OnGridVScroll ) then
    OnGridVScroll( Sender, ScrollCode, Pos );
  if Assigned( OnGridCheckScroll ) then
    OnGridCheckScroll( Grid );
end;
(*
procedure TGridEditor.CellContentsChanges(Sender: TObject; ACol, ARow: Longint;
        const Value: String);
begin
  if Assigned( OnCellContentsChangesEvent ) then
    OnCellContentsChangesEvent( Sender, ACol, ARow, Value );
end;
*)
procedure TGridEditor.GridClick(Sender : TObject);

  procedure RepaintMarked;
  begin
    RepaintCell(0, Grid.Row);
    RepaintCell(0, PreviousRow);
  end;

  function ExitedKeys : Boolean;
  var
    rView : TRowView;
    pField, aField : TDataField;
    aTable : TDataTable;
  begin
    rView := PageView.RowView[0];
    pField := rView.Field[PreviousRowViewCol];
    aField := rView.Field[ActiveRowViewCol];
    aTable := RowStorage.DataTable;

    Result := TableHasKeyOrLookup( aTable, pField ) and
              not TableHasKeyOrLookup( aTable, aField );
  end;

begin
  if PreviousDataRow <> ActiveDataRow then
    ChangeDataRow
  else if ExitedKeys then
    ExitKeys;

  if DoShowMarkRowCalcField then
    RepaintMarked;

  PreviousRow := Grid.Row;

  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TGridEditor.ChangeDataRow;
var
  PutResult : TPutResult;
  ADataRow : TAbstractRow;
begin
  ADataRow := PreviousDataRow;

  if (ADataRow <> nil) then
  begin
    if (ADataRow is TDataRow) and
       AutoDeleteRows and
       ( not FAutoCreatingRow ) and
       ( AutoCreatedRows.LastRow = ADataRow ) and
       ( CanDeleteRow( ADataRow ) ) then
    begin
      Disable;
      try
        DoDeleteRow( ADataRow );
      finally
        Enable;
      end;
    end
    else if RowStorage.RowIsUnaccepted(ADataRow) then
    begin
      begin
        PutResult := AcceptRow(ADataRow);
        case PutResult of
        prOk, prKeyOverWrited, prRowsAdded :
          RepaintDataRow(ADataRow);
        end;
      end;
    end;
  end;

  ChangeDataRowEvents;
end;

procedure TGridEditor.ChangeDataRowEvents;
var
  i : Integer;
begin
  if Assigned(FSlaveList) then
    for i := 0 to FSlaveList.Count -1 do
      ISlave(FSlaveList[i]).RowChange(ActiveDataRow);

  if Assigned(OnDataRowChange) then
    OnDataRowChange(Self);
end;

procedure TGridEditor.ExitKeys;
var
  PutResult : TPutResult;
  ADataRow : TAbstractRow;
begin
  ADataRow := ActiveDataRow;
  if (ADataRow <> nil) and
     ( AutoCreatedRows.IndexOfObject( ADataRow ) = -1 ) and
     RowStorage.RowIsUnaccepted(ADataRow) then
  begin
    PutResult := AcceptRow(ADataRow);
    case PutResult of
    prOk, prKeyOverWrited, prRowsAdded :
      RepaintDataRow(ADataRow);
    end;
  end;
end;

procedure TGridEditor.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  function SpecialDataRowIsReadOnly(ADataRow : TAbstractRow) : Boolean;
  var
    iField, iRowView : Integer;
  begin
    if DataRowIsReadOnly(ADataRow) then
      Result := True
    else
    begin
      Result := True;
      for iRowView := 0 to PageView.RowViewCount -1 do
        for iField := PageView.RowView[iRowView].FieldCount -1 downto 0 do
        begin
          Result := Result and FieldIsReadOnly(PageView.RowView[iRowView].Field[iField], ADataRow);
          if not Result then
            Exit;
        end;
    end;
  end;

  function AcceptNewPosition(ARow, ACol : Integer): Boolean;
  var
    Field : TDataField;
    Row : TAbstractRow;
  begin
    Field := DataField[ARow, ACol];
    Row := DataRowByGridIndex[ARow];
    Result := not FieldIsReadOnly(Field, Row) or SpecialDataRowIsReadOnly(Row);
  end;

var
  idxOldRow, idxNewRow, idxOldRowView, idxNewRowView, idxOldCol, idxNewFieldIdx : Integer;
begin
  inherited;

  if Assigned( OnGridKeyDown ) then
     OnGridKeyDown( Self, Key, Shift );

  if AutoCreateRows and
     (FixedCols <= AlwaysFixedColCount ) and
     ( AllowMultipleAutoCreatedRows or
       ( AutoCreatedRows.Count <= 0 ) ) and
     ( Grid.Row = Grid.RowCount -1 ) and
     ( ( Key = Key_Down ) or
       ( ( (Key = Key_Return) or (Key = Key_Tab) ) and
         ( Grid.Col = Grid.ColCount -1 ) ) ) then
  begin
    AutoCreateRow;
    Key := 0;
  end
  else if Key = Key_Return then
  begin
    idxOldRow := Grid.Row;
    idxOldCol := Grid.Col;
    repeat
      idxOldRowView := RowViewIndexByGridIndex[idxOldRow];
      idxNewRowView := RowViewIndexByGridIndex[idxOldRow + 1];
      idxNewRow := GridIndexOfDataRow(ActiveDataRow) + idxNewRowView;
      idxNewFieldIdx := idxOldCol;

      if idxNewRowView <> idxOldRowView + 1 then
      begin
        idxNewFieldIdx := idxOldCol + 1;

        if not SpecialDataRowIsReadOnly(ActiveDataRow) and
           (idxNewFieldIdx < Grid.ColCount) then
          while ColumnIsReadOnly(idxNewFieldIdx) do
          begin
            Inc(idxNewFieldIdx);
            if idxNewFieldIdx >= Grid.ColCount then
              Break;
          end;

        if idxNewFieldIdx >= Grid.ColCount then
        begin
          idxNewFieldIdx := Grid.FixedCols;
          idxNewRow := MaxIntValue( [(GridIndexOfDataRow(ActiveDataRow) + PageView.RowViewCount) mod Grid.RowCount,
                                    Grid.FixedRows] );
        end
        else
          idxNewFieldIdx := idxNewFieldIdx;
      end;

      if AcceptNewPosition(idxNewRow, idxNewFieldIdx) then
        Break
      else
      begin
       idxOldRow := idxNewRow;
       idxOldCol := idxNewFieldIdx;
      end;
    until False;

    if ( Grid.Row <> idxNewRow ) and
       ( idxNewRow >= 0 ) then
      GridRow := idxNewRow;

    if ( Grid.Col <> idxNewFieldIdx ) and
       ( idxNewFieldIdx >= 0 ) then
      GridCol := idxNewFieldIdx;
  end;
end;

procedure TGridEditor.GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Coords : TGridCoord;
  ACol, ARow : Integer;
  ACell : TDataEditorCell;
  RunInherited : boolean;
begin
  Coords := Grid.MouseCoord(X, Y);
  ACol := Coords.X;
  ARow := Coords.Y;

  ACell := EditorCell;

  RunInherited := True;
  if Assigned (OnMouseDownEvent) then
    OnMouseDownEvent(self, Button, Shift, ACol, ARow, ACell, RunInherited);
end;

function TGridEditor.GetUsesDefaultSort : Boolean;
begin
  Result := not RowStorage.UsesCustomSortOrder;
end;

procedure TGridEditor.DefaultSortRows;
begin
  Disable;
  try
    RowStorage.UsesCustomSortOrder := False;
    FSortField := nil;

    SortRows;
  finally
    Enable;
  end;
end;

procedure TGridEditor.SortByField(AField : TDataField);
var
  idxField : Integer;
  AOrder : TRowSortOrder;
  ASort : TSortOrder;
  ASortField : TDataField;
begin
  if AField is TConstantField then
    Exit;

  AOrder := RowStorage.CustomSortOrder;

  if Assigned(AField.SortField) then
  begin
    if (PageView.RowViewIndexOfField(AField.SortField) >= 0) then
//       (PageView.RowViewIndexOfField(AField) = 0) then
      ASortField := AField
    else 
      ASortField := AField.SortField
  end
  else
    ASortField := AField;

  idxField := AOrder.IndexOfField[ASortField];

  ASort := soAscending;
  if (idxField = 0) and RowStorage.UsesCustomSortOrder then
    case AOrder.Rule[0].SortOrder of
      soAscending : ASort := soDescending;
      soDescending : ASort := soAscending;
    end
  else
    ASort := ASortField.SortOrder;

  SortByFieldAndOrder(ASortField, ASort);
end;

procedure TGridEditor.SortByFieldAndOrder(AField : TDataField; ASortOrder : TSortOrder);
var
  iRow : Integer;
begin
  if AField is TConstantField then
    Exit
  else if not EnableSorting then
    Exit;

  Disable;
  try
    iRow := ActiveRow;

    DoSortByFieldAndOrder(AField, ASortOrder);

    SortRows;
    ActiveRow := iRow;
  finally
    Enable;
  end;
end;

procedure TGridEditor.DoSortByFieldAndOrder(AField : TDataField; ASortOrder : TSortOrder);
var
  idxField : Integer;
  AOrder : TRowSortOrder;
  Cursor : TCursor;
begin
  FSortField := AField;
  if AField = nil then
    Exit;
(*
  else if (PageView.RowViewIndexOfField(AField) <> 0) and {StandardView.DefaultSortFields.ContainsField(AField) and }
     Assigned(AField.SortField) then
    AField := AField.SortField;
*)
  Cursor := crDefault;
  if Grid <> nil then
  begin
    Cursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
  end;

  RowStorage.UsesCustomSortOrder := True;
  AOrder := RowStorage.CustomSortOrder;
  idxField := AOrder.IndexOfField[AField];

  if idxField = -1 then
    AOrder.AddRule(AField, ASortOrder)
  else
    AOrder.Rule[idxField].SortOrder := ASortOrder;

  AOrder.MoveRule(AField, 0);

  if Grid <> nil then
    Screen.Cursor := Cursor;
end;

function TGridEditor.GetShowIcon(ACol : Integer) : Boolean;
var
  AField, ADataField : TDataField;
begin
  Result := False;
  if not EnableSorting then
    Result := False
  else if ACol < AlwaysFixedColCount then
    Result := True
  else
  begin
    ADataField := DataField[0, ACol];
    if (ADataField is THeaderField) and
       (RowStorage <> nil) then
    begin
      AField := THeaderField(ADataField).DataField;
      if RowStorage.UsesCustomSortOrder then
        Result := (SortField = AField) or
                  (Assigned(AField.SortField) and (SortField = AField.SortField));
    end;
  end;
end;

procedure TGridEditor.CellGetIcon(Sender: TObject; APicture : TPicture; var ShowIcon : Boolean);
var
  ACell : TDataEditorCell;
begin
  ShowIcon := False;
  if not (Sender is TDataEditorCell) then
    Exit;

  ACell := TDataEditorCell(Sender);
  if not GetShowIcon(ACell.Col) then
    Exit;

  ShowIcon := True;

  if ACell.Col < AlwaysFixedColCount then
  begin
    if RowStorage.UsesCustomSortOrder then
      LoadImageFromRes( APicture, BMP_UNSORTED )
    else
      LoadImageFromRes( APicture, BMP_SORTED );
  end
  else
  begin
    case RowStorage.CustomSortOrder.Rule[0].SortOrder of
      soDescending : LoadImageFromRes( APicture, BMP_UPARROW );
    else
      LoadImageFromRes( APicture, BMP_DOWNARROW );
    end;
  end;
  APicture.Bitmap.Transparent := True;
  APicture.Bitmap.TransparentMode := tmAuto;
end;

procedure TGridEditor.CellOverWritingRow(Sender: TObject; ADataRow : TAbstractRow; var OverWritingRow : Boolean);
begin
  OverWritingRow := CanDeleteRow(ADataRow);
end;

procedure TGridEditor.CellOverWritedRow(Sender : TObject);
begin
//  InvalidateGrid;
end;

procedure TGridEditor.CellSetItemIndex(Sender : TObject; SetResult : TSetResult);
begin
  ChangeValue(SetValueCell, SetResult, True);
end;

procedure TGridEditor.CellSetValue(Sender : TObject; SetResult : TSetResult);
begin
  ChangeValue(SetValueCell, SetResult, True);
end;

//procedure TGridEditor.CellSettingValue(Sender: TObject; var Value : TValue;
//  var Reject : Boolean; var RejectReason : String; var RunInherited : Boolean);
procedure TGridEditor.CellSettingValue(Sender: TObject; var Value : TValue; var Reject, RunInherited : Boolean;
  var RejectParams : TRejectParams);
begin
//  ChangingValue(SetValueCell, Value, Reject, RejectReason, RunInherited);
  ChangingValue(SetValueCell, Value, Reject, RunInherited, RejectParams);
end;

procedure TGridEditor.CellButtonClick( Sender : TObject; ACell : IInfoCell;
  AMouseState : TMouseState; var Value : TValue );
var
  AField : TDataField;
  ADataRow : TAbstractRow;
begin
  if (ACell.SelfObject is TDataInplaceEditorCell) and
     Assigned(OnEditButtonClick) then
  begin
    AField := TDataInplaceEditorCell(ACell.SelfObject).DataField;
    ADataRow := TDataInplaceEditorCell(ACell.SelfObject).DataRow;
    OnEditButtonClick(AField, ADataRow, Value);
  end;
end;

procedure TGridEditor.ChangeValue(Cell : TDataInplaceEditorCell; SetResult : TSetResult;
  ChangeActiveDataRow : Boolean);
var
  AField : TDataField;
  ACol, ARow : Integer;
  AActiveRow, ADataRow : TAbstractRow;

  procedure CheckDefaultValues;
  var
    ARow : TDataRow;
    iField, iProperty : Integer;
    TmpField : TDataField;
    DefaultValueProperty : TGridEditorDefaultValue;
    Cont : Boolean;
  begin
    if not Assigned(ADataRow) or
       not (ADataRow is TDataRow) or
       not ADataRow.DataTable.TableHasKey(AField) or
       (AField.DataType.Equals(ADataRow[AField], AField.DataType.DefaultValue)) then
      Exit
    else
      ARow := TDataRow(ADataRow);

    for iProperty := 0 to StandardView.DefaultValueList.Count -1 do
    begin
      Cont := False;
      DefaultValueProperty := TGridEditorDefaultValue(StandardView.DefaultValueList.Objects[iProperty]);
      TmpField := DefaultValueProperty.DataField;
      if (TmpField <> AField) and
         (ARow.IndexOfField(TmpField) >= 0) and
         DefaultValueProperty.ChangeValueOnKeyChange and
         not DefaultValueProperty.ValueLegal( ARow ) then
      begin
        for iField := 0 to DefaultValueProperty.DependFields.Count -1 do
          if DisabledFieldList.ContainsField( DefaultValueProperty.DependFields.Field[iField]) then
          begin
            Cont := True;
            Break;
          end;

        if Cont then
          Continue;
        ARow[TmpField] := DefaultValueProperty.GetDefaultValue(ARow);
      end;
    end;
  end;

  procedure CheckAutoCreated;
  var
    idx, idxRow : Integer;
  begin
    idxRow := AutoCreatedRows.IndexOfObject( ADataRow );
    for idx := idxRow downto 0 do
      AutoCreatedRows.Delete( idx );
  end;

var
  i : Integer;
begin
  AActiveRow := ActiveDataRow;
  ACol := Cell.Col;
  ARow := Cell.Row;
  AField := Cell.DataField;
  ADataRow := Cell.DataRow;

  if Assigned(FSlaveList) then
    for i := 0 to FSlaveList.Count -1 do
      ISlave(FSlaveList[i]).ValueChanged(AField);

  CheckAutoCreated;
  CheckDefaultValues;

  if ChangeActiveDataRow and
     (ADataRow <> ActiveDataRow) then
    ChangeDataRow;

  if SetResult = srKeyOverwrited then
  begin
    SetGridSize;
    InvalidateGrid;

    AActiveRow := ActiveDataRow;
    ACol := Cell.Col;
    ARow := Cell.Row;
    AField := Cell.DataField;
    ADataRow := Cell.DataRow;
  end;

  // Fixa MVJ: When keys are overwritten the buffer edit will not work correctly
  if ConstantBufferEdit then
    BalanceBuffer(ADataRow, AField);

  // Fixa MVJ: Here we get a nil-assert when replasing data
  if Assigned(AField) and
     (ADataRow is TDataRow) and
     RowHasKey( ADataRow, AField ) then
  begin
    if Enabled and
       not IsRowLegal(ADataRow) and
       not RowStorage.RowIsUnaccepted(ADataRow) then
    begin
      Disable;
      ADataRow := UnacceptRow(TDataRow(ADataRow));
      Enable;
    end;
  end;

  if Assigned(OnChangeValue) then
    OnChangeValue(Self, Cell);

  if SaveToAuxtableFields.ContainsField(AField) or
    ConstantBufferEdit then
    RepaintCol(ACol);

  if not RowStorage.RowIsUnaccepted(ADataRow) and
     ( ParentSubtotalsVisible[ADataRow] or (ADataRow is TSubtotalRow) ) then
  begin
    if RowHasKey( ADataRow, AField ) or (ADataRow is TSubtotalRow) then
    begin
      if Enabled then
      begin
        ArrangeRows;
        SetGridSize;
        ActiveDataRow := AActiveRow;
      end
      else
        FArrangeOnEnable := True;
    end;

    if Enabled then
      InvalidateGrid;
  end
  else
  begin
    case UpdateType of
      utAll : InvalidateGrid;
      utCell:;
      utCol: RepaintCol(ACol);
      utRow: RepaintDataRow(ADataRow);
      utRowCol: RepaintCross(ACol, ARow);
    end;
  end;
end;

procedure TGridEditor.ChangingValue(Cell : TDataInplaceEditorCell; var Value : TValue;
      var Reject, RunInherited : Boolean; var RejectParams : TRejectParams);
begin
  if Assigned(OnChangingValue) then
    OnChangingValue(Self, Cell, Value, Reject, RunInherited, RejectParams);

  BalanceRow := Cell.DataRow;
end;

procedure TGridEditor.SetBalanceRow( ARow : TAbstractRow );

  function CreateSubTotalDataRow( ASubRow : TSubTotalRow ) : TDataRow;
  var
    iField : Integer;
    AField : TDataField;
    ATable : TDataTable;
  begin
    ATable := ASubRow.DataTable;
    Result := TDataRow.Create(ATable);
    for iField := ATable.KeyCount to ATable.FieldCount -1 do
    begin
      AField := ATable.Field[iField];
      if AField.IsAggregable then
        Result[AField] := ASubRow[AField];
    end;
  end;

begin
  FBalanceRow.Free;
  if ARow is TDataRow then
    FBalanceRow := TDataRow(ARow).CreateCopy
  else if ARow is TSubTotalRow then
    FBalanceRow := CreateSubTotalDataRow( TSubTotalRow(ARow) )
  else
    FBalanceRow := nil;
end;

function TGridEditor.BalanceBuffer(ARow : TAbstractRow; AField : TDataField) : Boolean;
var
  DestRow : TDataRow;
begin
  Result := False;
  DestRow := GetBufferRow(ARow);

  if DestRow = nil then
    Exit
  else
    Result := True;

  MoveDiffToBuffer(BalanceRow, ARow, DestRow);
end;

procedure TGridEditor.MoveDiffToBuffer(OldRow, NewRow : TAbstractRow; Buffer : TDataRow);
var
  iField : Integer;
  AField : TDataField;
  ATable : TDataTable;
  OldValue, NewValue, BufferOldValue, BufferNewValue, DiffValue: TValue;
begin
  ATable := RowStorage.DataTable;
  for iField := ATable.KeyCount to ATable.FieldCount -1 do
  begin
    AField := ATable.Field[iField];
    if AField.IsAggregable then
    begin
      if OldRow = nil then
        OldValue := AField.DataType.DefaultValue
      else
        OldValue := OldRow.GetFieldValue(AField);

      if NewRow = nil then
        NewValue := AField.DataType.DefaultValue
      else
        NewValue := NewRow.GetFieldValue(AField);

      if AField.DataType.Compare(OldValue, NewValue) <> 0 then
      begin
        DiffValue := AField.DataType.Difference(OldValue, NewValue);
        BufferOldValue := Buffer.GetFieldValue(AField);
        BufferNewValue := AField.DataType.Sum(BufferOldValue, DiffValue);
        Buffer.SetFieldValue(AField, BufferNewValue, saDontOverwriteKeys);
      end;
    end;
  end;
end;

function TGridEditor.GetBufferRow(ARow : TAbstractRow) : TDataRow;
var
  ASubRow, AHighSub : TSubTotalRow;
begin
  Result := nil;
  if ARow = nil then
    Exit;

  ASubRow := ARow.SubTotalRow;
  AHighSub := nil;

  if ConstantBufferKey <> nil then
  begin
    while (ASubRow <> nil) do
    begin
      if (ASubRow.SubTotalRow <> nil) and
        (ASubRow.SubTotalRow.SubTotalKey.TreeKey = ConstantBufferKey) then
      begin
        AHighSub := ASubRow;
        Break;
      end;
      ASubRow := ASubRow.SubTotalRow;
    end;
  end
  else
    AHighSub := ASubRow;

  Result := GetLastDetail(AHighSub);
end;

function TGridEditor.GetLastDetail(ASubRow : TSubTotalRow) : TDataRow;
var
  ASub : TAbstractRow;
begin
  Result := nil;
  if ASubRow = nil then
    Exit;

  ASub := ASubRow;

  while not (ASub is TDataRow) and
      ( TSubTotalRow(ASub).SubRowCount >= 0 ) do
    ASub := TSubTotalRow(ASub).SubRows[TSubTotalRow(ASub).SubRowCount -1];

  if ASub is TDataRow then
    Result := TDataRow(ASub)
  else
    Result := nil;
end;

procedure TGridEditor.SetFixedCols(Value : Integer);
begin
  Inherited SetFixedCols(Value);
  UpdateTotalsSize;
end;

procedure TGridEditor.RepaintCol(ACol : Integer);
begin
  Grid.RedrawCol(ACol);
  UpdateTotals;
end;

procedure TGridEditor.RepaintRow(ARow : Integer);
begin
  if ARow > 0 then
    Grid.RedrawRow(ARow);
end;

procedure TGridEditor.RepaintCross(ACol, ARow : Integer);
begin
  Grid.RedrawRow(ARow);
  Grid.RedrawCol(ACol);
  UpdateTotals;
end;

procedure TGridEditor.RepaintCell(ACol, ARow : Integer);
begin
  Grid.RedrawCell(ACol, ARow);
end;

procedure TGridEditor.RepaintDataRow(ADataRow : TAbstractRow);
var
  iDataRow, iRow : Integer;
begin
  iDataRow := GridIndexOfDataRow(ADataRow);
  if iDataRow >= 0 then
    for iRow := iDataRow to MinIntValue([iDataRow + PageView.RowViewCount, Grid.RowCount -1]) do
      RepaintRow(iRow);
  UpdateTotals;
end;

procedure TGridEditor.RepaintActiveMark;
var
  iRow, idxRow : Integer;
begin
  idxRow := GridIndexOfDataRow(ActiveDataRow);

  if DoShowMarkRowCalcField and (idxRow >= 0) then
  begin
    for iRow := idxRow to idxRow + RowViewCount do
      RepaintCell(0, iRow);
  end;
end;

procedure TGridEditor.InvalidateGrid;
begin
  if not Executing then
    Inherited InvalidateGrid;
  UpdateTotals;
end;

procedure TGridEditor.ResizeCols(ResultList : TStringList);
var
  MaxDataRow : TDataRow;
  AField : TDataField;
  FontWidth, iCol, iRowView, LargestValue, AValue : Integer;
  AString : String;
begin
  MaxDataRow := TDataRow.Create(RowStorage.DataTable);
  RowStorage.GetLargestValuesRestrictCount(MaxDataRow, 50);
  for iCol := 0 to TotalColCount -1 do
  begin
    LargestValue := 0;
    AField := nil;
    for iRowView := 0 to PageView.RowViewCount -1 do
    begin
      AField := RowViewDataField[PageView.RowView[iRowView], iCol];
      FontWidth := 6;
      if AField = MarkRowCalcField then
      begin
        AString := '';
        Break;
      end
      else if AField.DataType = IntegerType then
        AString := IntToStr(MaxDataRow.IntValue[AField])
      else if AField.DataType = DoubleType then
        AString := FormatFloat( ',0.0', MaxDataRow.DoubleValue[AField] )
      else if AField.DataType = CurrencyType then
        AString := FormatFloat( ',0.00', MaxDataRow.DoubleValue[AField]  / Divisor[AField, MaxDataRow])
      else
      begin
        AString := MaxDataRow.DisplayString[AField, GetFieldKeyFormatting(AField)];
        if Length(AString) < 4 then
          FontWidth := 8
        else if Length(AString) < 13 then
          FontWidth := 7
        else
          FontWidth := 6;
      end;
      AValue := MaxIntValue([Length(Trim(AString)) * FontWidth, FontWidth]);
      AValue := AValue + (9 * FontWidth) div AValue; // Avoid too narrow columns
      LargestValue := MaxIntValue([LargestValue, AValue]);
    end;
    if AField <> nil then
      ResultList.AddObject(IntToStr(LargestValue), AField);
  end;
  MaxDataRow.Free;
end;

procedure TGridEditor.DefaultColWidths(Value : Integer);
var
  iCol : Integer;
begin
  for iCol := AlwaysFixedColCount to TotalColCount -1 do
    Grid.ColWidths[iCol] := Value;

  if not CanFixCols(FixedCols) then
    FixedCols := 0;
end;

procedure TGridEditor.OptimizeColWidths;
var
  ValueList : TStringList;
  iNumeric, iField, TotWidth, AvgWidth: Integer;
begin
  ValueList := TStringList.Create;
  ResizeCols(ValueList);

  TotWidth := 0;
  iNumeric := 0;
  for iField := AlwaysFixedColCount to ValueList.Count -1 do
    if TDataField(ValueList.Objects[iField]).DataType.IsNumeric then
    begin
      TotWidth := TotWidth + StrToInt(ValueList[iField]);
      Inc(iNumeric);
    end;

  if iNumeric > 0 then
    AvgWidth := TotWidth div iNumeric
  else
    AvgWidth := 0;
    
  for iField := AlwaysFixedColCount to ValueList.Count -1 do
    if TDataField(ValueList.Objects[iField]).DataType.IsNumeric then
      SetGridColWidth(iField, MaxIntValue([MINWIDTH_NumCol, AvgWidth, StrToInt(ValueList[iField])]))
    else
      SetGridColWidth(iField, StrToInt(ValueList[iField]));
  ValueList.Free;

  if not CanFixCols(FixedCols) then
    FixedCols := 0;
end;

procedure TGridEditor.MinimizeColWidths;
var
  ValueList : TStringList;
  iField : Integer;
begin
  ValueList := TStringList.Create;
  ResizeCols(ValueList);
  for iField := AlwaysFixedColCount to ValueList.Count -1 do
    if TDataField(ValueList.Objects[iField]).DataType.IsNumeric then
      SetGridColWidth(iField, MaxIntValue([MINWIDTH_NumCol, StrToInt(ValueList[iField])]))
    else
      SetGridColWidth(iField, StrToInt(ValueList[iField]));
  ValueList.Free;

  if not CanFixCols(FixedCols) then
    FixedCols := 0;
end;

procedure TGridEditor.FillCopyListWithFields(ATable : TDataTable; AList : TList; UIList : TList);
var
 iCol, iRow : Integer;
 AField : TDataField;
begin
  for iCol := 0 to SelectedCols.Count -1 do
    for iRow := 0 to RowViewCount -1 do
    begin
      AField := RowViewDataField[ PageView.RowView[iRow], AsInteger( SelectedCols.Values[iCol] ) ];
      if ATable.IndexOfField(AField) >= 0 then
      begin
        AList.Add(AField);
        if UIList <> nil then
          UIList.Add(AField);
      end;
    end;
end;

procedure TGridEditor.CopyLegalValuesBetweenDataRows(AEntry : TClipboardEntry; DestRow, SourceRow : TAbstractRow;
          AvailableLists : TStringList; OtherNonKeys : Boolean; PasteRule : TPasteRules);
var
  iField : Integer;
  ValueOfField, ValueOfOldField : TValue;
  AField : TDataField;
begin
  try
    Grid.BeginUpdate;
    for iField := 0 to AEntry.ActualSelectedFields.Count -1 do
      try
        AField := TDataField(AEntry.ActualSelectedFields[iField]);
        if not ShouldFieldBePasted(AField, AvailableLists, OtherNonKeys) then
          Continue;
        if not AEntry.DataTable.TableHasKey(AField) and not FieldIsReadOnly(AField, DestRow) then
        begin
          ValueOfField := GetValue( SourceRow, AField );
          if (PasteRule = sprAdd) and AField.DataType.IsNumeric then
          begin
            ValueOfOldField := GetValue(DestRow, AField);
            ValueOfField := AField.DataType.Sum(ValueOfField, ValueOfOldField);
          end;

          SetValue(DestRow, AField, ValueOfField);
        end;
      except
      end;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TGridEditor.SetString(ARow: TAbstractRow; AField: TDataField;
  AString: String);
var
  iCol, iRow : Integer;
begin
  GetGridIndex( ARow, AField, iCol, iRow );

  EditorIterator.ChangeCell( iCol, iRow );
  EditorIterator.SetGridValue( ValueFromString( AString ) );
end;

procedure TGridEditor.SetValue(ARow : TAbstractRow; AField : TDataField; AValue : TValue);
begin
  try
    if not FieldIsReadOnly(AField, ARow ) then
      ARow[AField] := AValue;
  except
  end;
end;

procedure TGridEditor.SetValueByCell(ARow : TAbstractRow; AField : TDataField; AValue : TValue);
var
  iCol, iRow : Integer;
begin
  GetGridIndex( ARow, AField, iCol, iRow );

  EditorIterator.ChangeCell( iCol, iRow );
  EditorIterator.SetGridValue( AValue );
end;

function TGridEditor.GetValue(ARow : TAbstractRow; AField : TDataField) : TValue;
begin
  try
    Result := ARow[AField];
  except
  end;
end;

function TGridEditor.GetValueByCell(ARow : TAbstractRow; AField : TDataField) : TValue;
var
  iCol, iRow : Integer;
begin
  GetGridIndex( ARow, AField, iCol, iRow );
  GridIterator.ChangeCell( iCol, iRow );
  Result := GridIterator.GetGridValue;
end;

procedure TGridEditor.CopyLegalValuesBetweenCols(PasteRows : TStringList; AvailableLists : TStringList; ToFields : TList;
          PasteRule : TPasteRules; OtherNonKeys : Boolean);
var
  iField, iRow : Integer;
  ValueOfField, ValueOfOldField : TValue;
  ToField : TDataField;
  SrcRow, DestRow : TDataRow;
begin
  try
    Grid.BeginUpdate;
    for iRow := 0 to PasteRows.Count -1 do
    begin

      if not (PasteRows.Objects[iRow] is TDataRow) then
        Continue;

      SrcRow := TDataRow(PasteRows.Objects[iRow]);

      DestRow := nil;
      try
        DestRow := RowStorage.LocateByRowValues(SrcRow, [nil]);
      except
        TranslateShowMessage('Paste aborted!');
        Abort;
      end;

      for iField := 0 to ToFields.Count -1 do
        try
          ToField := TDataField(ToFields[iField]);
          if ShouldFieldBePasted(ToField, AvailableLists, OtherNonKeys) and
             not (ToField is TKeyField) and
             not FieldIsReadOnly(ToField, DestRow) then
          begin
            ValueOfField := GetValue(SrcRow, ToField);
            if (PasteRule = sprAdd) and (ToField.DataType.IsNumeric) then
            begin
              ValueOfOldField := GetValue(DestRow, ToField);
              ValueOfField := ToField.DataType.Sum(ValueOfField, ValueOfOldField);
            end;
            SetValue(DestRow, ToField, ValueOfField);
          end
        except
        end;
    end;
  finally
    Grid.EndUpdate;
  end;
end;

{ifndef TRANSLATOR}
procedure TGridEditor.CopyGridSelectionToClipboard;
begin
  CopyGridFormattedSelectionToClipboard( CF_TEXT, dvDefault );
  CopyGridFormattedSelectionToClipboard( KeyValueFormat, dvKeyOnly );
end;

procedure TGridEditor.CopyGridFormattedSelectionToClipboard( AFormat : UINT; AKeyFormat : TDisplayValues);
var
  AString : String;
  ARect : TRect;
begin
  GetGridSelection( ARect );
  AString := GetSelectionAsFormattedString(ARect, AKeyFormat, NEW_LINE, KEY_TABCHAR);

//  ShowAnts;
  PutFormattedStringToClipboard( AFormat, AString );
end;
{endif TRANSLATOR}

procedure TGridEditor.PutStringToClipboard(AString : String);
begin
  ClipbrdInterface.AsText := AString;
end;

procedure TGridEditor.PutFormattedStringToClipboard( AFormat : UINT; AString : String );
var
  AStream : TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  if AString <> '' then
    AStream.Write( AString[1], Length(AString) );
  AStream.Write( KEY_NULL, 1 );
  TStreamFormatObject.Create(AFormat, AStream);
end;

procedure TGridEditor.CopySelectionToClipboard;
begin
  CopyFormattedSelectionToClipboard( CF_TEXT, dvDefault );
  CopyFormattedSelectionToClipboard( KeyValueFormat, dvKeyOnly );
end;

procedure TGridEditor.CopyFormattedSelectionToClipboard( AFormat : UINT; AKeyFormat : TDisplayValues );
var
  iRow, StartRow, StopRow : Integer;
  iCol, StartCol, StopCol : Integer;
  AString : String;
  ARect : TRect;
begin
  AString := '';
  if not ContainsSelection then
  begin
  end
  else if AllSelected then
  begin
    ARect.Top := 0;
    ARect.Left := AlwaysFixedColCount;
    ARect.Bottom := Grid.RowCount -1;
    ARect.Right := Grid.ColCount -1;
    AString := GetSelectionAsFormattedString(ARect, AKeyFormat, NEW_LINE, KEY_TABCHAR);
  end
  else
  begin
    if SelectedCols.Count = 0 then
    begin
      ARect.Left := AlwaysFixedColCount;
      ARect.Right := Grid.ColCount -1;
      ARect.Top := 0;
      ARect.Bottom := HeaderRowViewCount -1;
      AString := GetSelectionAsFormattedString(ARect, AKeyFormat, NEW_LINE, KEY_TABCHAR) + NEW_LINE;
      for iRow := 0 to SelectedDataRows.Count -1 do
      begin
        StartRow := GridIndexOfDataRow( SelectedDataRows.AbstractRows[iRow] );
        StopRow := StartRow + RowViewCount -1;
        ARect.Top := StartRow;
        ARect.Bottom := StopRow;
        AString := AString  + GetSelectionAsFormattedString(ARect, AKeyFormat, NEW_LINE, KEY_TABCHAR);
        if iRow <> SelectedDataRows.Count -1 then
          AString := AString + NEW_LINE;
      end;
    end
    else if SelectedDataRows.Count = 0 then
    begin
      for iRow := 0 to Grid.RowCount -1 do
      begin
        ARect.Top := iRow;
        ARect.Bottom := iRow;
        for iCol := 0 to SelectedCols.Count -1 do
        begin
          StartCol := AsInteger( SelectedCols.Values[iCol] );
          StopCol := StartCol;
          ARect.Left := StartCol;
          ARect.Right := StopCol;
          AString := AString + GetSelectionAsFormattedString(ARect, AKeyFormat, NEW_LINE, KEY_TABCHAR);
          if iCol <> SelectedCols.Count -1 then
            AString := AString + KEY_TABCHAR;
        end;
        if iRow <> Grid.RowCount -1 then
          AString := AString + NEW_LINE;
      end;
    end;
  end;

  PutFormattedStringToClipboard( AFormat, AString );
end;

function TGridEditor.GetSelectionAsString(ARect : TRect; NewLine, Tab : String) : String;
begin
  GetSelectionAsFormattedString( ARect, dvDefault, NewLine, Tab );
end;

function TGridEditor.GetSelectionAsFormattedString(ARect: TRect;
  AKeyFormatting: TDisplayValues; NewLine, Tab: String): String;

  function SelectActive : Boolean;
  var
    TempRect : TRect;
    GridRect : TGridRect;
  begin
    Result := not ContainsSelection;
    GridRect := Grid.Selection;
    with GridRect do
      TempRect := Rect( Left, Top, Right, Bottom );

    Result := Result and
              (TempRect.Left = ARect.Left) and
              (TempRect.Top = ARect.Top) and
              (TempRect.Right = ARect.Right) and
              (TempRect.Bottom = ARect.Bottom);
  end;

var
  iRow, iCol : Integer;
  TempString : String;
  AField : TDataField;
  ADataEditorCellInterface : IDataEditorCellInterface;
begin
  ARect := ArrangeRect(ARect);
  Result := '';
  for iRow := ARect.Top to ARect.Bottom do
  begin
    for iCol := ARect.Left to ARect.Right do
    begin
      GridIterator.ChangeCell( iCol, iRow );
      if GridIterator.SelfObject.GetInterface( IDataEditorCellInterface, ADataEditorCellInterface ) then
      begin
        if AKeyFormatting <> dvDefault then
          ADataEditorCellInterface.KeyFormatting := AKeyFormatting;
        ADataEditorCellInterface.GetDBUFormatter.SetDecimalCount(-1, ADataEditorCellInterface.GetDBUFormatter.DecimalCount); 
      end;
      TempString := AsString(GridIterator.GetGridValue);

      AField := DataField[iRow, iCol];
      if AField.DataType.IsNumeric then
        TempString := TDataFormatter.RemoveThousandSeparator(TempString);

      if iCol <> ARect.Right then
        TempString := TempString + Tab;
      Result := Result + TempString;
    end;

    if iRow <> ARect.Bottom then
      Result := Result + NewLine;
  end;
end;

procedure TGridEditor.Cut(AClipboard : TDataClipBoard);

  procedure DeleteSelectedRows;
  var
    iRow : Integer;
    AActiveDataRow, ARow : TAbstractRow;
  begin
    Disable;
    try
      CommitChanges;
      EditorCell.DataRow := nil;
      SetValueCell.DataRow := nil;
      AActiveDataRow := ActiveDataRow;

      if AllSelected then
        for iRow := RowStorage.RowCount -1 downto 0 do
        begin
          ARow := RowStorage.Rows[iRow];
          if CanDeleteRow(ARow) then
            DoDeleteRow(ARow);
        end
      else if SelectedDataRows.Count > 0 then
        for iRow := SelectedDataRows.Count -1 downto 0 do
        begin
          ARow := SelectedDataRows.AbstractRows[iRow];
          if CanDeleteRow(ARow) then
            DoDeleteRow(ARow);
        end;

      UnselectAll;
      HideAnts;

      if RowStorage.IndexOfRow(AActiveDataRow) >= 0 then
        ActiveDataRow := AActiveDataRow;
    finally
      Enable;
    end;
    UpdateTotals;
  end;

begin
  Copy(AClipboard);
  if AllSelected or
     (SelectedDataRows.Count > 0) then
    DeleteSelectedRows
  else
    Delete;
end;

procedure TGridEditor.Copy(AClipboard : TDataClipBoard);

  procedure CopyDataRowsToNewList(NewList, OldList : TDataRowList);
  var
   iRow : Integer;
  begin
    if OldList.Count = 0 then
    begin
      for iRow := 0 to RowStorage.RowCount -1 do
        NewList.AddObject( '', RowStorage.Rows[iRow]);
    end
    else
      for iRow := 0 to OldList.Count -1 do
        NewList.AddObject( '', OldList.AbstractRows[iRow] );
  end;

var
  AddAllFields : Boolean;
  AEntry : TClipboardEntry;
  ATable : TDataTable;
begin
  if InplaceCopyPaste then
    Grid.DBUInplaceEdit.CopyToClipboard
  else
  begin
    Disable;
    try
      ClipbrdInterface.Open;
      ClipbrdInterface.Clear;

      HideAnts;

      if not ContainsSelection then
        CopyGridSelectionToClipboard
      else
      begin
        if AClipboard <> nil then
        begin
          ATable := RowStorage.DataTable;
          AddAllFields := AllSelected or ( SelectedCols.Count = 0 );
          AEntry := TClipboardEntry.Create( Self, ATable, AddAllFields );
          CopyDataRowsToNewList( AEntry.DataRows, SelectedDataRows );
          FillCopyListWithFields( ATable, AEntry.ActualSelectedFields, AEntry.UISelectedFields ) ;
          AClipboard.AddToClipboard(AEntry);
          AEntry.AddToClipbrdInterface;
        end;
        CopySelectionToClipboard;
      end;
      ShowAnts;
    finally
      ClipbrdInterface.Close;
      ClipbrdInterface.RenderFormat( CF_TEXT );
      ClipbrdInterface.RenderFormat( KeyValueFormat );
      Enable;
    end;
  end;
end;

// If we always show the editor , we only paste to it if
// less than the whole string in it is selected, otherwise
// we always paste when the editor has focus.
function TGridEditor.InplaceCopyPaste : Boolean;
var
  InplaceEdit : TDBUInplaceEdit;
begin
  InplaceEdit := Grid.DBUInplaceEdit;

  if Assigned(InplaceEdit) and
     InplaceEdit.Focused and
     ( not ( dgAlwaysShowEditor in Grid.DBUOptions ) or
       (InplaceEdit.SelLength = Length(InplaceEdit.Text)) ) then
    Result := True
  else
    Result := False;
end;

procedure TGridEditor.Paste(AClipboard : TDataClipBoard);
var
  AEntry : TClipboardEntry;
begin
  if ReadOnly then
  begin
    TranslateShowMessage('The table is read only!');
    Exit;
  end;

  if InplaceCopyPaste then
    Grid.DBUInplaceEdit.PasteFromClipBoard
  else
    try
      Disable;

      if (AClipboard <> nil) and
          IsClipboardFormatAvailable(TEntryFormatObject.GetFormat) then
      begin
        AEntry := TEntryFormatObject.CreateEntryFromClipboard(TEntryFormatObject.GetFormat);
        if AEntry <> nil then
          PasteFromEntry(AEntry);
        AEntry.Free;
      end
      else if IsClipboardFormatAvailable(KeyValueFormat) then
        PasteText( KeyValueFormat )
      else if IsClipboardFormatAvailable(CF_TEXT) then
        PasteText( CF_TEXT )
      else
{$ifndef LINUX}
        MessageBeep(MB_OK);
{$else LINUX}
        Beep;
{$endif LINUX}
    finally
      Enable;
    end;
end;

procedure TGridEditor.Delete;

  procedure DeleteRect(ARect : TRect);
  var
    iRow, iCol : Integer;
//    ARow : TAbstractRow;
    AField : TDataField;
    Iter : IEditorIterator;
  begin
    Iter := EditorIterator;
    for iRow := ARect.Top to ARect.Bottom do
      for iCol := ARect.Left to ARect.Right do
        try
          AField := DataField[iRow, iCol];
          Iter.ChangeCell(iCol, iRow);
          Iter.SetGridValue(AField.DataType.DefaultValue);
          
(*        if not (ARow is TDataRow) then
          Continue;
        try
          AField := DataField[iRow, iCol];
          if not FieldIsReadOnly(AField, ARow) then
            SetValue(ARow, AField, AField.DataType.DefaultValue);*)
        except
        end;
//      end;
  end;

  procedure GetAffectedFields(AFieldList : TFieldList);
  var
    iRowView, iCol : Integer;
    ARowView : TRowView;
    AField : TDataField;
  begin
    for iRowView := 0 to PageView.RowViewCount -1 do
    begin
      ARowView := PageView.RowView[iRowView];
      for iCol := 0 to SelectedCols.Count -1 do
      begin
        AField := GetRowViewDataField(ARowView, AsInteger( SelectedCols.Values[iCol] ));
        if not AField.ReadOnly[nil] then // Fixa MVJ hur göra med DataRow undrar LGE???
          AFieldList.Add(AField);
      end;
    end;
  end;

  procedure DeleteSelection;
  var
    iRow, iField : Integer;
    ARect : TRect;
    ARow : TDataRow;
    AField : TDataField;
    Rows : TDataRowList;
    AList : TFieldList;
  begin
    if AllSelected then
    begin
      ARect.Top := HeaderRowViewCount;
      ARect.Bottom := Grid.RowCount -1;
      ARect.Left := GridIndexByFieldIndex[PageView.RowView[0].DisplayKeyList.Count -1];
      ARect.Right := Grid.ColCount -1;
      DeleteRect(ARect);
    end
    else if SelectedDataRows.Count <> 0 then
    begin
      for iRow := 0 to SelectedDataRows.Count -1 do
      begin
        if not (SelectedDataRows.DataRows[iRow] is TDataRow) then
          Continue;
        ARow := SelectedDataRows.DataRows[iRow];
        for iField := ARow.DataTable.KeyCount to ARow.DataTable.FieldCount -1 do
          try
            AField := ARow.DataTable.Field[iField];
            if not FieldIsReadOnly(AField, ARow) then
              SetValue(ARow, AField, AField.DataType.DefaultValue);
          except
          end;
      end;
    end
    else if SelectedCols.Count <> 0 then
    begin
      AList := TFieldList.Create;
      GetAffectedFields(AList);
      Rows := TDataRowList.Create;

      try
        RowStorage.GetRows( Rows, nil, gaReference );

        for iRow := Rows.Count -1 downto 0 do
        begin
          ARow := Rows.DataRows[iRow];
          for iField := 0 to AList.Count -1 do
            try
              AField := AList.Field[iField];
              if not FieldIsReadOnly(AField, ARow) then
                SetValue(ARow, AField, AField.DataType.DefaultValue);
            except
            end;
        end;
      except
      end;

      AList.Free;
      Rows.Free;
    end;
  end;

var
  SelRect : TRect;
  GridRect : TGridRect;
begin
  if Grid = nil then
    Exit;

  if ContainsSelection then
  begin
    Disable;
    try
      DeleteSelection;
      InvalidateGrid;
    finally
      Enable;
    end;
  end
  else if ContainsGridSelection then
  begin
    Disable;
    try
      GridRect := Grid.Selection;
      with GridRect do
        SelRect := Rect( Left, Top, Right, Bottom );

      SelRect := ArrangeRect( SelRect );
      DeleteRect(SelRect);
    finally
      Enable;
    end;
  end
  else
    Grid.DeletePress;
end;

procedure TGridEditor.PasteText( AFormat : UINT );

   procedure FillListToSquare(AList : TList);
   var
     iRow, countMax : Integer;
   begin
     countMax := -1;
     for iRow := 0 to AList.Count -1 do
       if TStringList(AList[iRow]).Count > countMax then
         countMax := TStringList(AList[iRow]).Count;

     for iRow := 0 to AList.Count -1 do
       while TStringList(AList[iRow]).Count < countMax do
          TStringList(AList[iRow]).Add('');
   end;

  function CreateTwoDimensionalData(AString : String) : TList;
  var
    Len, iNewLine, iTab : Integer;
    ActiveList : TStringList;
  begin
    Len := Length(AString);
    if Len >= 2 then
      if System.Copy(AString, Len-1, 2) = #13#10 then
        System.Delete(AString, Len-1, 2);

    Len := Length(AString);
    if Len >= 1 then
      if AString[Len] = #9 then
        System.Delete(AString, Len, 1);

    Result := TList.Create;
    ActiveList := TStringList.Create;
    Result.Add(ActiveList);
    iNewLine := Pos(#13#10, AString);
    iTab := Pos(#9, AString);
    while (iNewLine > 0) or (iTab > 0) do
    begin
      if ( (iNewLine < iTab) and
           (iNewLine > 0) ) or
         ( (iTab = 0) and
           (iNewLine > 0)) then
      begin
        ActiveList.Add(System.Copy(AString, 1, iNewLine -1));
        System.Delete(AString, 1, iNewLine + Length(#13#10) -1);
        ActiveList := TStringList.Create;
        Result.Add(ActiveList);
      end
      else
      begin
        ActiveList.Add(System.Copy(AString, 1,iTab -1));
        System.Delete(AString, 1, iTab +Length(#9) -1);
      end;
      iNewLine := Pos(#13#10, AString);
      iTab := Pos(#9, AString);
    end;
    ActiveList.Add(AString);
    FillListToSquare(Result);
   end;

var
  APasteObj : TPasteRectObj;
  SelList, PasteMatrix, PasteObjects, PasteRowsLists : TList;
  UnacceptList, PasteRows : TDataRowList;
  NoLimit, PasteToSubTotals, ParentSubsVisible : Boolean;
  iRect, iPasteRow, iPasteCol, iPasteRowList, iDataRow, iPasteObj : Integer;
  NewSelRect, ToRect : TRect;
  AGridRect : TGridRect;
  ADataRow : TAbstractRow;
  AField : TDataField;
  PasteString : String;
begin
  PasteToSubTotals := True;
  ParentSubsVisible := False;
  NoLimit := False;
  NewSelRect := Rect( -1, -1, -1, -1);

  try
    PasteString := ClipbrdInterface.GetAsFormatText( AFormat );

    PasteMatrix := CreateTwoDimensionalData(PasteString);
    if PasteMatrix.Count > 0 then
    begin
      // Since we can have selected many separate columns or rows, we
      // have to generate a set of destination areas
      SelList := TList.Create;
      GetTotalSelection(SelList);

      UnacceptList := TDataRowList.Create;
      PasteObjects := TList.Create;
      PasteRowsLists := TList.Create;

      for iRect := 0 to MaxIntValue( [SelList.Count -1, 0] ) do
      begin
        if SelList.Count = 0 then
          GetGridSelection( ToRect )
        else
          ToRect := PRect( SelList[iRect] )^;

        ToRect := ArrangeRect(ToRect);
        NoLimit := (ToRect.Left = ToRect.Right) and
                   (ToRect.Top = ToRect.Bottom) and
                   (SelList.Count <= 1);

        if NoLimit then
        begin
          ToRect.Bottom := ToRect.Top + PasteMatrix.Count -1;
          ToRect.Right := ToRect.Left + TStringList(PasteMatrix[0]).Count -1;
          NewSelRect := ToRect;
        end;

        PasteRows := TDataRowList.Create;
        PasteRowsLists.Add( PasteRows );
        GetDataRowsForRect(ToRect, PasteRows, False);

        Assert( PasteRows.Count > 0 );
        iDataRow := GridIndexOfDataRow( PasteRows[0] );

        // Offset the destination area from Grid coordinates to coordinates relative to the
        // data rows and the row views fields 
        OffsetRect( ToRect, -AlwaysFixedColCount, -iDataRow );

        if PasteToSubTotals or not ParentSubsVisible then
          for iPasteRow := 0 to PasteRows.Count -1 do
          begin
            ADataRow := PasteRows.AbstractRows[iPasteRow];

            if not ParentSubsVisible then
              ParentSubsVisible := ParentSubtotalsVisible[ ADataRow ];

            // Don't paste to sub total rows, if we try to paste over detail keys,
            // since the subtotal can disappear.
            // *** We assume that all keys are in the first row view ***
            if PasteToSubTotals and (ADataRow is TDataRow) then
            begin
              for iPasteCol := ToRect.Left to ToRect.Right do
              begin
                if iPasteCol >= PageView.RowView[0].FieldCount then
                  Break;

                AField := PageView.RowView[0].Field[ iPasteCol ];
                if PasteToSubTotals and RowHasKey( ADataRow, AField ) then
                begin
                  PasteToSubTotals := False;
                  Break;
                end;
              end;
            end;

            if not PasteToSubTotals and ParentSubsVisible then
              Break;
          end;

        APasteObj := TPasteRectObj.Create( Self, PasteRows, ToRect, PasteMatrix );
        PasteObjects.Add( APasteObj );
      end;

      // Remove sub total rows from the paste lists (set pointer to nil) if we shouldn't paste to sub totals
      if not PasteToSubTotals then
        for iPasteRowList := 0 to PasteRowsLists.Count -1 do
        begin
          PasteRows := TDataRowList( PasteRowsLists[iPasteRowList] );

          for iPasteRow := 0 to PasteRows.Count -1 do
          begin
            if not (PasteRows.AbstractRows[iPasteRow] is TDataRow) then
              PasteRows.Objects[iPasteRow] := nil;
          end;
        end;

      // Do the actual paste
      for iPasteObj := 0 to PasteObjects.Count -1 do
        TPasteRectObj( PasteObjects[iPasteObj] ).PasteToRect( UnacceptList );

      // Unaccept rows that no longer are legal
      for iDataRow := 0 to UnacceptList.Count -1 do
        UnacceptRow( TDataRow(UnacceptList.Objects[ iDataRow ]) );

      UnacceptList.Free;
{      for iRect := 0 to SelList.Count -1 do
        Dispose( SelList[iRect] );
      SelList.Free; }
      FreeListWithPointers( SelList );

      FreeListWithObjects( PasteMatrix );
      FreeListWithObjects( PasteObjects );
      FreeListWithObjects( PasteRowsLists );
    end;
  except
    SortRows;
  end;

  if ParentSubsVisible and not PasteToSubTotals then
    FArrangeOnEnable := True
  else if NoLimit then
  begin
    with NewSelRect do
      AGridRect := GridRect( GridCoord( Left, Top ), GridCoord( Right, Bottom ) );
    Grid.Selection := AGridRect;
  end;
end;



procedure TGridEditor.PasteFromEntry(AEntry : TClipboardEntry);
var
  OnlyOne, ContainsRows, WithinForecast : Boolean;
begin
  if AEntry.DataTable <> RowStorage.DataTable then
  begin
    TranslateMessageDlg('Source and destination table must be the same', mtInformation, [mbOk], 0);
    Exit;
  end;


  GetInfoOfCopyEntry(AEntry, WithinForecast, ContainsRows, OnlyOne);

  if WithinForecast and not OnlyOne then
  begin
    if ContainsRows then
      TranslateShowMessage('You can only copy single DataRows within a table!')
    else
      TranslateShowMessage('You can only paste one column at a time within a table!');
    Screen.Cursor := crDefault;
    Exit;
  end;

  if ContainsRows then
  begin
    if OnlyOne then
      PasteSingleRow(AEntry)
    else
      PasteManyRows(AEntry);
  end
  else
  begin
    if not WithinForecast then
      PasteCols(AEntry)
    else if OnlyOne then
      PasteColWithinForecast(AEntry)
    else
      TranslateShowMessage( 'You can only paste one column at a time within a table!'
                   + ' Both columns have to be equal time periods' );
  end;
end;

procedure TGridEditor.GetInfoOfCopyEntry(AEntry : TClipboardEntry; var WithinForecast, ContainsRows, OnlyOne : Boolean);
begin
  ContainsRows := (AEntry.ActualSelectedFields.Count = AEntry.DataTable.FieldCount);
  if (ContainsRows and AEntry.OneRowSelected) or
     (not ContainsRows and IsOnlyOneColCopied(AEntry)) then
    OnlyOne := True
  else
    OnlyOne := False;
  WithinForecast := (AEntry.Source = Self);
end;

function TGridEditor.GetPasteConvertCriteria : TCondition;
begin
  Result := Criteria;
end;

procedure TGridEditor.PasteSingleRow(AEntry : TClipboardEntry);
var
  AvailableLists : TStringList;
  PasteRule : TPasteRules;
  ARow : TDataRow;
  OtherNonKeys, ForecastEmpty : Boolean;
begin
  Assert( AEntry.DataRows.Count = 1 , Self.ClassName + '.PasteSingleRow: The list of DataRows contains '
         + IntToStr(AEntry.DataRows.Count) + ' rows. Should be only 1!');

  AvailableLists := TStringList.Create;

  ForecastEmpty := (RowStorage.RowCount = 0);

  if ShowPasteDialog( AvailableLists, OtherNonKeys,
    [sprAdd, sprReplace, sprEnterAsNewRow], PasteRule, 1 ) then
  begin
    Disable;
    if ForecastEmpty then
      PasteRule := sprEnterAsNewRow;

    try
      ARow := AEntry.PasteOneRow(PasteConvertCriteria);

      ResetValuesNotToBePasted(AEntry, ARow, AvailableLists, OtherNonKeys);

      if PasteRule = sprEnterAsNewRow then
      begin
        InsertDataRow( ARow, RowStorage.IndexOfRow(ActiveDataRow) +1);
        EnableWithCoord(0, GridIndexOfDataRow(ARow));
      end
      else
      begin
        CopyLegalValuesBetweenDataRows(AEntry, ActiveDataRow, ARow, AvailableLists, OtherNonKeys, PasteRule);
        ARow.Free;
        Enable;
      end;
    except
      Enable;
    end;
  end;

  AvailableLists.Free;
end;

procedure TGridEditor.PasteManyRows(AEntry : TClipboardEntry);
var
  PasteRule : TPasteRules;
  AvailableLists, PasteRows : TStringList;
  OtherNonKeys : Boolean;
begin
  PasteRows := TStringList.Create;

  Disable;
  AvailableLists := TStringList.Create;
  try
    AEntry.PasteKeyConvert(PasteRows, PasteConvertCriteria);

    if ShowPasteDialog(AvailableLists, OtherNonKeys,
      [sprAdd, sprReplace, sprSkipIfExist] , PasteRule, PasteRows.Count) then
      PasteLegalValues(AEntry, PasteRows, AvailableLists, PasteRule, OtherNonKeys);
  finally
    AvailableLists.Free;
    PasteRows.Free;

    SetGridSize;
    Enable;
  end;
end;

procedure TGridEditor.PasteCols(AEntry : TClipboardEntry);
var
  PasteRule : TPasteRules;
  AvailableLists, PasteRows : TStringList;
  OtherNonKeys : Boolean;
begin
  PasteRows := TStringList.Create;

  Disable;
  AvailableLists := TStringList.Create;
  try
    AEntry.PasteKeyConvert(PasteRows, PasteConvertCriteria);

    if ShowPasteDialog(AvailableLists, OtherNonKeys,
      [ sprAdd, sprReplace, sprSkipIfExist ], PasteRule, -1) then
      PasteLegalValues(AEntry, PasteRows, AvailableLists, PasteRule, OtherNonKeys);

  finally
    AvailableLists.Free;
    PasteRows.Free;
    SetGridSize;
    Enable;
  end;
end;

procedure TGridEditor.PasteColWithinForecast(AEntry : TClipboardEntry);
var
  FromFields, ToFields : TList;
  PasteRule : TPasteRules;
  AvailableLists, PasteRows : TStringList;
  OtherNonKeys : Boolean;
  ARowView : TRowView;
begin
  FromFields := TList.Create;
  ToFields := TList.Create;

  ARowView := PageView.RowView[0];
  if (RowViewDataField[ARowView, Grid.Col] is TKeyField) {or
     (ARowView.DataFieldList = nil) }then
    Exit;

  if not MapPasteFields( AEntry.UISelectedFields, FromFields, ToFields, FieldIndexByGridIndex[Grid.Col]) then
  begin
    FromFields.Free;
    ToFields.Free;
    Exit;
  end;

  Disable;

  PasteRows := TStringList.Create;
  AvailableLists := TStringList.Create;
  try
    AEntry.PasteKeyConvertFieldMove(PasteRows, PasteConvertCriteria, FromFields, ToFields);
    if ShowPasteDialog(AvailableLists, OtherNonKeys,
      [ sprAdd, sprReplace ], PasteRule, -1) then

    CopyLegalValuesBetweenCols(PasteRows, AvailableLists, ToFields, PasteRule, OtherNonKeys);
  finally
    FromFields.Free;
    ToFields.Free;
    AvailableLists.Free;
    PasteRows.Free;

    SetGridSize;
    Enable;
  end;
end;

procedure TGridEditor.PasteLegalValues(AEntry : TClipboardEntry; PasteRows : TStringList; AvailableLists : TStringList;
          PasteRule : TPasteRules; OtherNonKeys : Boolean);
var
  ARow : TDataRow;
  RowExists : Boolean;
  OldRow : TDataRow;
  iDataRow : Integer;
begin
  for iDataRow := 0 to PasteRows.Count -1 do
  begin
    if not (PasteRows.Objects[iDataRow] is TDataRow) then
      Continue;

    ARow := TDataRow(PasteRows.Objects[iDataRow]);
    if ARow = nil then
      Continue;

    OldRow := nil;
    try
      OldRow := RowStorage.LocateByRowValues(ARow, [nil]);
    except
      TranslateShowMessage('Paste aborted!');
      Abort;
    end;
    RowExists := OldRow <> nil;

    ResetValuesNotToBePasted(AEntry, ARow, AvailableLists, OtherNonKeys);

    if RowExists then
    begin
      case PasteRule of
        sprAdd, sprReplace :
        begin
          CopyLegalValuesBetweenDataRows(AEntry, OldRow, ARow, AvailableLists, OtherNonKeys, PasteRule);
          ARow.Free;
        end;
        sprSkipIfExist :
        begin
          Continue;
          ARow.Free;
        end;
      end;
    end
    else
      InsertDataRow(ARow, -1);
  end;
  SortRows;
end;

function TGridEditor.ShowPasteDialog(DataFieldLists : TStringList; var OtherNonKeys : Boolean;
        PasteRules : TPasteRulesSet; var PasteRule : TPasteRules; ARowCount : Integer ) : Boolean;
var
  PasteDialog : TfrmPasteDialog;
  AvailableLists : TStringList;
  ARule : TPasteRules;
  FoundRule : Boolean;
begin
  if ARowCount = 0 then
  begin
    TranslateMessageDlg( 'No rows to paste after row filtering!' + #13#10 +
                'Check your selections and the active tab' + #13#10 +
                'sheet for an unambiguous paste to work!', mtInformation, [mbOK], 0 );
    Result := False;
    Exit;
  end;

  AvailableLists := TStringList.Create;
  GetAvailableLists(AvailableLists);
  OtherNonKeys := (PageView.RowView[0].DefaultRowView.FieldList.Count > 0);

  PasteDialog := TfrmPasteDialog.Create(nil);
  Result := PasteDialog.Run( AvailableLists, DataFieldLists, OtherNonKeys,
      PasteRules, ARowCount );

  FoundRule := False;
  for ARule := Low(TPasteRules) to High(TPasteRules) do
    if ARule in PasteRules then
    begin
      if FoundRule then
        raise Exception.Create( Self.ClassName + '.ShowPasteDialog: More than one paste rule given!' );
      FoundRule := True;
      PasteRule := ARule;
    end;

  if not FoundRule then
    raise Exception.Create( Self.ClassName + '.ShowPasteDialog: No paste rule given!' );

  PasteDialog.Free;
end;

procedure TGridEditor.GetAvailableLists(AvailableLists : TStringList);
var
  iRowView : Integer;
  RowView : TRowView;
begin
  for iRowView := 0 to PageView.RowViewCount -1 do
  begin
    RowView := PageView.RowView[iRowView];
    if (RowView.DataFieldList <> nil) and
       (RowView.ListDescription <> nil) then
      AvailableLists.AddObject(AsString(RowView.ListDescription.CalcValue(nil)), RowView.DataFieldList);
  end;
end;

function TGridEditor.ShouldFieldBePasted(AField : TDataField; AvailableLists : TStringList; OtherNonKeys : Boolean) : Boolean;
var
  iList : Integer;
  RowView : TRowView;
begin
  Result := False;

  RowView := PageView.RowView[0];
  if RowView.DefaultRowView.FieldList.IndexOf(AField) >= 0 then
  begin
    Result := OtherNonKeys;
    Exit;
  end;

  for iList := 0 to AvailableLists.Count -1 do
  begin
    if TDataFieldList(AvailableLists.Objects[iList]).ContainsField(AField) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TGridEditor.ResetValuesNotToBePasted(AEntry : TClipboardEntry; ARow : TDataRow;
          AvailableLists : TStringList; OtherNonKeys : Boolean);
var
  iField, iList : Integer;
  AField : TDataField;
  RowView : TRowView;
  ATable : TDataTable;
  Reset : Boolean;
begin
  ATable := AEntry.DataTable;
  for iField := ATable.KeyCount to ATable.FieldCount -1 do
  begin
    AField := ATable.Field[iField];

    RowView := PageView.RowView[0];
    if not OtherNonKeys and
       RowView.DefaultRowView.FieldList.ContainsField(AField) then
      SetValue(ARow, AField, AField.DataType.DefaultValue)
    else if not AEntry.DataTable.TableHasKey(AField) and
         (FieldIsReadOnly(AField, ARow) or
         (AEntry.ActualSelectedFields.IndexOf(AField) < 0)) then
      SetValue(ARow, AField, AField.DataType.DefaultValue)
    else if OtherNonKeys and
            not AEntry.DataTable.TableHasKey(AField) and
            not FieldIsReadOnly(AField, ARow) and
            (AEntry.ActualSelectedFields.IndexOf(AField) >= 0) and
            RowView.DefaultRowView.FieldList.ContainsField(AField) then
      Continue
    else
    begin
      Reset := True;
      for iList := 0 to AvailableLists.Count -1 do
        if TDataFieldList(AvailableLists.Objects[iList]).ContainsField(AField) then
        begin
          Reset := False;
          Break;
        end;
      if Reset then
        SetValue(ARow, AField, AField.DataType.DefaultValue);
    end
  end;
end;

function TGridEditor.IsOnlyOneColCopied(AEntry : TClipboardEntry) : Boolean;
var
  iField, idxCol, idxOldCol, iRowView : Integer;
  Fields : TList;
  AField : TDataField;
begin
  Result := True;
  Fields := AEntry.UISelectedFields;
  idxOldCol := -1;

  for iField := 0 to Fields.Count -1 do
  begin
    for iRowView := 0 to PageView.RowViewCount -1 do
    begin
      AField := TDataField(Fields[iField]);
      idxCol := PageView.RowView[iRowView].FieldList.IndexOf(AField);
      if idxCol = -1 then
        Continue
      else if (idxOldCol <> -1) and (idxOldCol <> idxCol) then
      begin
        Result := False;
        Exit;
      end
      else
        idxOldCol := idxCol;
    end;
    if idxOldCol = -1 then
    begin
      Result := False;
      Exit;
    end
  end;
end;

function TGridEditor.MapPasteFields( SourceFields, FromFields, ToFields : TList; NewFieldIdx : Integer ) : Boolean;
var
  iField : Integer;
  AField : TDataField;
  ErrorMessage : String;
begin
  Result := False;
  ErrorMessage := '';

  for iField := 0 to SourceFields.Count -1 do
  begin
    AField := TDataField(SourceFields[iField]);
    if not MapField(AField, FromFields, ToFields, NewFieldIdx, ErrorMessage) then
    begin
      if ErrorMessage = '' then
        ErrorMessage := 'Could not map copied column to the new one!';
      ErrorMessage := ErrorMessage + ' Paste aborted.';
      TranslateShowMessage(ErrorMessage);
      Exit;
    end;
  end;
  Result := True;
end;

function TGridEditor.MapField( AField : TDataField; FromFields, ToFields : TList;
         NewFieldIdx : Integer; var ErrorMessage : String ) : Boolean;
var
  iRowView, idxField : Integer;
  RowView : TRowView;
  NewField : TDataField;
begin
  if AField is TKeyField then
  begin
    FromFields.Add(AField);
    ToFields.Add(AField);
    Result := True;
    Exit;
  end;

  Result := False;
  for iRowView := 0 to PageView.RowViewCount -1 do
  begin
    RowView := PageView.RowView[iRowView];
    NewField := RowViewDataField[RowView, NewFieldIdx];
    idxField := RowView.FieldList.IndexOf(AField);
    if (idxField <> -1) then
    begin
      if NewField.DataType.ClassType <> AField.DataType.ClassType then
        ErrorMessage := 'The copied field ''' + AField.LongDescription +
                        ''' and ''' + NewField.LongDescription +
                        ''' don''t match!'
      else
      begin
        FromFields.Add(AField);
        ToFields.Add(NewField);
        Result := True;
      end;
      Exit;
    end;
  end;
  if not Result then
    ErrorMessage := 'Could not map ''' + AField.LongDescription + '''';
end;

procedure TGridEditor.Disable;
begin
  PreviousRow := -1;
  if Enabled then
    FDisableDataRow := ActiveDataRow;
  Inherited Disable;
end;

procedure TGridEditor.Enable;
var
  ARow : TAbstractRow;
begin
  if DisableCount <= 1 then
  begin
    if Grid <> nil then
      PreviousRow := Grid.Row;
  end;

  ARow := ActiveDataRow;
  Inherited Enable;

  if Enabled then
  begin
    if ARow <> ActiveDataRow then
      try
        ActiveDataRow := ARow;
      except
      end;

    if FDisableDataRow <> ActiveDataRow then
      ChangeDataRowEvents;
    FDisableDataRow := nil;
  end;
end;

procedure TGridEditor.UpdateTotals;
begin
  if (TotalViewer <> nil) and not Executing then
    TotalViewer.InvalidateGrid;
end;

procedure TGridEditor.UpdateTotalsSize;
begin
  if TotalViewer <> nil then
  begin
    TotalViewer.SetGridSize;
    UpdateTotals;
  end;
end;

procedure TGridEditor.CheckTotalGridCorrelation;
begin
  if Executing then
    Exit;
  CheckTotalViewerColWidths;
  CheckTotalViewerScroll;
end;

procedure TGridEditor.CheckTotalViewerColWidths;
begin
  if TotalViewer <> nil then
    TotalViewer.CheckColWidths;

  if Assigned( OnGridCheckColWidths ) then
    OnGridCheckColWidths( Grid );
end;

procedure TGridEditor.CheckTotalViewerScroll;
begin
  if (TotalViewer <> nil) and (TotalViewer.Grid.Left <> Grid.Left) then
    TotalViewer.ScrollGrid;

  if Assigned( OnGridCheckScroll ) then
    OnGridCheckScroll( Grid );
end;

function TGridEditor.AcceptRowWithMessage(DataRow : TAbstractRow) : TPutResult;
var
  ErrorStrings : TStringList;
  iErr : Integer;
  ErrorString : String;
begin
  if DataRow is TDataRow then
  begin
    ErrorStrings := TStringList.Create;

    if CheckRowHasLegalValues(TDataRow(DataRow), ErrorStrings) then
    begin
      Result := AcceptRow(DataRow);
      case Result of
        prValuesMissing : ErrorStrings.Add('Values Missing!');
        prKeyConflict : ErrorStrings.Add('Key conflict!');
        prIllegalKeyValue : ErrorStrings.Add('Illegal key value!');
        prCannotAdd : ErrorStrings.Add('Cannot add!');
      end;
    end
    else
      Result := prIllegalKeyValue;

    if Result in [prValuesMissing, prKeyConflict, prIllegalKeyValue, prCannotAdd] then
    begin
      for iErr := 0 to ErrorStrings.Count -1 do
        ErrorString := ErrorString + #13#10 + ErrorStrings[iErr];
      TranslateShowMessage(ErrorString);
    end;

    ErrorStrings.Free;
  end
  else
    Result := AcceptRow(DataRow);
end;

function TGridEditor.UnacceptRow(DataRow : TDataRow) : TDataRow;
var
  Idx : Integer;
  RowIsActive : Boolean;
begin
  Disable;

  RowIsActive := (ActiveDataRow = DataRow);
  try
    if RowIsActive then
    begin
      RemovingActive := True;
      ChangeDataRow;
    end;

    Idx := RowStorage.IndexOfRow(DataRow);
    Result := DataRow.CreateCopy;
    DataRow.Delete;
    RowStorage.NewUnacceptedRow(Idx, Result);

  finally
    if RowIsActive then
      RemovingActive := False;

    Enable;
  end;
end;

function TGridEditor.CheckRowHasLegalValues(DataRow : TDataRow; var ErrorStrings : TStringList) : Boolean;
begin
  Result := DoCheckRowHasLegalValues(DataRow, ErrorStrings, False);
end;

function TGridEditor.IsRowLegal(DataRow : TAbstractRow) : Boolean;
var
  Dummy : TStringList;
begin
  if DataRow is TDataRow then
  begin
    Dummy := nil;
    Result := DoCheckRowHasLegalValues(TDataRow(DataRow), Dummy, True);
  end
  else
    Result := True;
end;

function TGridEditor.DoCheckRowHasLegalValues(DataRow : TDataRow; var ErrorStrings : TStringList; BreakOnFalse : Boolean) : Boolean;
var
  AField : TCalcField;
begin
  Result := True;
  AField := StandardView.RowIsValidField;
  if AField = nil then
    Exit
  else if (AField is TRowIsValidField) then
    Result := TRowIsValidField(AField).IsRowValid(ErrorStrings, DataRow, DisabledFieldList, BreakOnFalse);
end;

procedure TGridEditor.UnacceptIllegalRows;
var
  iRow : Integer;
  ARow : TAbstractRow;
  Cursor : TCursor;
  Rows : TDataRowList;
begin
  Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Disable;

  Rows := TDataRowList.Create;
  try
    RowStorage.GetRows( Rows, nil, gaReference );
    for iRow := 0 to Rows.Count -1 do
    begin
      ARow := Rows.AbstractRows[iRow];
      if (ARow is TDataRow) and
         not IsRowLegal(ARow) and
         not DataRowIsReadOnly(ARow) then
        UnacceptRow(TDataRow(ARow));
    end;

    if RowStorage.UnacceptedRowCount > 0 then
    begin
      ARow := RowStorage.UnAcceptedRows[0];
      ActiveDataRow := ARow;
      EnableWithCoord(0, -1);
    end
    else
      Enable;
    Screen.Cursor := Cursor;
  except
    RowStorage.ArrangeRows;
    SetGridSize;
    Enable;
    Screen.Cursor := Cursor;
    Rows.Free;
    raise;
  end;
  Rows.Free;
end;

procedure TGridEditor.SetSaveToAuxTableFields(Fields : TDataFieldSet);
begin
  if FSaveToAuxtableFields = nil then
    FSaveToAuxtableFields := TDataFieldSet.Create
  else
    FSaveToAuxtableFields.Clear;

  FSaveToAuxtableFields.CopyFrom(Fields);
end;

procedure TGridEditor.SetCurrencyDecimalCount(Value: Integer);
begin
  FCurrencyDecimalCount := Value;
end;

procedure TGridEditor.SetDecimalCount(Value: Integer);
begin
  FDecimalCount := Value;
end;

procedure TGridEditor.SetCurrencyDivisor(Value: Double);
begin
  Disable;
  try
    CommitChanges;
    FCurrencyDivisor := Value;
  finally
    Enable;
  end;
end;

function TGridEditor.GetDecimalCount : Integer;
begin
  Result := FDecimalCount;
end;

function TGridEditor.GetCurrencyDecimalCount : Integer;
begin
  Result := FCurrencyDecimalCount;
end;

function TGridEditor.GetDivisor(AField: TDataField; ARow: TAbstractRow) : Double;
begin
  Result := CurrencyDivisor;
end;

function TGridEditor.GetCurrencyDivisor: Double;
begin
  Result := FCurrencyDivisor;
end;

function TGridEditor.GetEditorDecimalCount : Integer;
begin
  Result := FEditorDecimalCount;
end;

procedure TGridEditor.SetEditorDecimalCount(Value : Integer);
begin
  FEditorDecimalCount := Value;
end;

function TGridEditor.GetDisabledFieldList : TDefaultValueFieldList;
begin
  Result := FOwnedDisabledFieldList;
end;

procedure TGridEditor.SetDisabledFieldList(AList : TDefaultValueFieldList);
begin
  FDisabledFieldList := AList;

  FOwnedDisabledFieldList.Clear;
  FOwnedDisabledFieldList.CopyFrom( PageView.DisabledFieldList );
  FOwnedDisabledFieldList.CopyFrom( FDisabledFieldList );
end;

procedure TGridEditor.GridGetTotalCell( Sender : IDataEditorCellInterface );
var
  OldEditor : TGridEditor;
begin
  if not RowStorage.CanHaveTotals and
     Assigned(TotalViewer) then
    Exit;

  OldEditor := TotalViewer.ActiveEditor;
  TotalViewer.ActiveEditor := Self;
  // Fixa MJV!
  TotalViewer.GridGetCell(Sender);
  TotalViewer.ActiveEditor := OldEditor;
end;

function TGridEditor.ShowingTotals : Boolean;
begin
  Result := (TotalViewer <> nil) and TotalViewer.Parent.Visible;
end;

procedure TGridEditor.Hide;
begin
  if Visible then
  begin
    Assert( not IsActive, Self.ClassName + '.Hide: Can''t hide the active editor!' );
    Disable;
    if Parent is TTabSheet then
      TTabSheet(Parent).TabVisible := False
    else
      Parent.Visible := False;
  end;
end;

procedure TGridEditor.Show;
begin
  if not Visible then
  begin
    if Parent is TTabSheet then
      TTabSheet(Parent).TabVisible := True
    else
      Parent.Visible := True;
    Enable;
  end;
end;

function TGridEditor.GetVisible : Boolean;
begin
  if Parent is TTabSheet then
    Result := TTabSheet(Parent).TabVisible
  else if Parent <> nil then
    Result := Parent.Visible
  else
    Result := False;
end;

procedure TGridEditor.SetVisible( AValue : Boolean );
begin
  if AValue then
    Show
  else
    Hide;
end;

procedure TGridEditor.GridDblClick(Sender: TObject);
begin
  inherited;

  if Assigned( OnDblClick ) then
    OnDblClick( Self );
end;

procedure TGridEditor.DeleteAutoCreatedRows;
var
  i : integer;
begin
  Disable;
  try
    for i := AutoCreatedRows.Count -1 downto 0 do
      AutoCreatedRows.DataRows[i].Delete;
    AutoCreatedRows.Clear;
  finally
    Enable;
  end;
end;

procedure TGridEditor.OnHEvent( Sender : TObject );
begin
end;

procedure TGridEditor.OnSEvent( Sender : TObject );
begin
end;

procedure TGridEditor.LockToColumn;
begin
  if CanLockToCol then
    SetFixedCols(Grid.Col +1)
end;

function TGridEditor.CanLockToCol : Boolean;
begin
  Result := CanFixCols(Grid.Col +1);
end;

procedure TGridEditor.UnlockColumns;
begin
  SetFixedCols(0);
end;

function TGridEditor.CanUnlockCols : Boolean;
begin
  Result := CanFixCols(0);
end;

function TGridEditor.CanFixCols(Value: Integer): Boolean;
begin
  Result := inherited CanFixCols( Value );
end;

procedure TGridEditor.SetGridColWidth(ACol, AValue: Integer);
begin
  Grid.ColWidths[ACol] := Min(AValue, (Grid.Width * 9) div 10);
end;

procedure TGridEditor.RegisterSlave(ASlave : ISlave);
begin
  if not Assigned(FSlaveList) then
    FSlaveList := TInterfaceList.Create;

  FSlaveList.Add(ASlave);
  ASlave.StorageChange(RowStorage);
  ASlave.RowChange(ActiveDataRow);
end;

procedure TGridEditor.UnregisterSlave(ASlave : ISlave);
begin
  if Assigned(FSlaveList) then
    FSlaveList.Remove(ASlave);
end;

procedure TGridEditor.ValueChanged(AField : TDataField);
var
  iCol, iRow : Integer;
begin
  iCol := GridIndexOfField[AField];
  iRow := ActiveRow;

  if AField is TCalcField then
    InvalidateGrid
  else if (iCol >= 0) and
          (iRow >= 0) then
  begin
    case UpdateType of
      utAll : InvalidateGrid;
      utCell: RepaintCell(iCol, iRow);
      utCol: RepaintCol(iCol);
      utRow: RepaintDataRow(ActiveDataRow);
      utRowCol: RepaintCross(iCol, iRow);
    end;
  end;
end;

procedure TGridEditor.HideField(AField: TDataField);
var
  iCol, iField : Integer;
begin
  Assert(AField <> nil);

  if FHiddenColWidthOrganizer.HasWidth(AField) then
//    raise Exception.Create(Self.ClassName + '.HideField: The field ''' + AField.FieldName + ''' is already hidden!')
  else
  begin
    iCol := GridIndexOfField[AField];
    iField := PageView.ColOfField(AField);
    if iCol < 0 then
//      raise Exception.Create(Self.ClassName + '.HideField: The field ''' + AField.FieldName + ''' is not present this grid!')
    else if iCol < FixedCols then
//      raise Exception.Create(Self.ClassName + '.HideField: The field ''' + AField.FieldName + ''' is locked and can not be hidden!')
    else if iField < 0 then
//      raise Exception.Create(Self.ClassName + '.HideField: The field ''' + AField.FieldName + ''' can''t be hidden!')
    else
    begin
      FHiddenColWidthOrganizer.Width[AField] := Grid.ColWidths[iCol];
      PageView.HideField(AField);
      Grid.DeleteColumn(iCol);
    end;
  end;
end;

procedure TGridEditor.ShowField(AField: TDataField);
var
  iCol, AWidth : Integer;
begin
  Assert(AField <> nil);

  if not FHiddenColWidthOrganizer.RemoveWidth(AField, AWidth) then
//    raise Exception.Create(Self.ClassName + '.ShowField: The field ''' + AField.FieldName + ''' is not hidden or present in this grid!')
  else
  begin
    PageView.ShowField(AField);
    iCol := GridIndexOfField[AField];
    if iCol < 0 then
 //     raise Exception.Create(Self.ClassName + '.ShowField: The field ''' + AField.FieldName + ''' is not present in this grid!')
    else
      Grid.InsertColumn(iCol, AWidth);
  end;
end;

function TGridEditor.CanHideField(AField: TDataField): Boolean;
begin
  Result := Assigned(AField) and
            not FHiddenColWidthOrganizer.HasWidth(AField) and
            (PageView.ColOfField(AField) >= 0) and
            (GridIndexOfField[AField] >= FixedCols);
end;

function TGridEditor.CanShowField(AField: TDataField): Boolean;
begin
  Result := Assigned(AField) and
            FHiddenColWidthOrganizer.HasWidth(AField);
end;

function TGridEditor.GetRestoreForIllegalValues: Boolean;
begin
  Result := SetValueCell.RestoreForIllegalValues;
end;

procedure TGridEditor.SetRestoreForIllegalValues(const Value: Boolean);
begin
  SetValueCell.RestoreForIllegalValues := Value;
end;

function TGridEditor.GetAbortOnIllegalValues: Boolean;
begin
  Result := SetValueCell.AbortOnIllegalValues;
end;

procedure TGridEditor.SetAbortOnIllegalValues(const Value: Boolean);
begin
  SetValueCell.AbortOnIllegalValues := Value;
end;

{ TEditorPictureField }

constructor TEditorPictureField.CreateOld(FieldName : String; AEditor : TGridEditor; APicture : TPicture);
begin
  Inherited CreateOld(FieldName, APicture);

  FEditor := AEditor;
end;

function TEditorPictureField.CalcValue(ARow : TAbstractRow) : TValue;
var
  Active : Boolean;
begin
  Active := (FEditor.EditorCell.Row = FEditor.ActiveRow);

  if Active then
    Result := Inherited CalcValue(ARow)
  else
    Result := ValueFromPicture(nil);
end;

{ TCheckBoxFieldObject }

constructor TCheckBoxFieldObject.Create(AField : TDataField; AOnGetText : TOnGetTextEvent);
begin
  inherited Create;

  FOnGetText := AOnGetText;
  FDataField := AField;
end;

{ TPasteRectObj }

constructor TPasteRectObj.Create( AGridEditor : TGridEditor; APasteRowsList : TDataRowList;
      AToRect : TRect; AValuesList : TList );
begin
  inherited Create;

  FGridEditor := AGridEditor;
  FPasteRows := APasteRowsList;
  FValues := AValuesList;
  FToRect := AToRect;
end;

procedure TPasteRectObj.PasteToRect( AUnacceptList : TDataRowList );
var
  iRectRow, iRectCol, iDataRow, iField, iRowView, RowViewCount : Integer;
  RowValues : TStringList;
  RowView : TRowView;
  ADataRow : TAbstractRow;
  AField : TDataField;
  AValue : String;
begin
  RowViewCount := FGridEditor.RowViewCount;
  for iRectRow := FToRect.Top to FToRect.Bottom do
    try
      iDataRow := iRectRow div RowViewCount;
      if iDataRow >= FPasteRows.Count then
        break;

      RowValues := TStringList( FValues[ ( iRectRow - FToRect.Top ) mod FValues.Count] );
      ADataRow := FPasteRows.AbstractRows[iDataRow];
      if ADataRow = nil then
        Continue;

      iRowView := iRectRow mod RowViewCount;
      RowView := FGridEditor.PageView.RowView[iRowView];

      for iRectCol := FToRect.Left to FToRect.Right do
      begin
        iField := iRectCol;
        if ( iField >= RowView.FieldCount ) then
          break;

        AField := RowView.Field[iField];
        if not FGridEditor.FieldIsReadOnly( AField, ADataRow ) then
        begin
          AValue := RowValues[ ( iRectCol - FToRect.Left ) mod RowValues.Count];
          FGridEditor.SetString( ADataRow, AField, AValue );
          if RowHasKey(ADataRow, AField) and
             not FGridEditor.IsRowLegal( ADataRow ) and
             (AUnacceptList.IndexOfObject( ADataRow ) = -1) then
          begin
            AUnacceptList.AddObject( '', ADataRow );
          end;
        end;
      end;
    except
    end;
end;

{ TDBUDataCombo }

constructor TDBUDataCombo.Create;
begin
  inherited Create;
  FFillTimeStamp := 1;
  FFillCriteria := TCriteria.Create;
end;

destructor TDBUDataCombo.Destroy;
begin
  inherited;
  FFillCriteria.Free;
end;

procedure TDBUDataCombo.SetFillCriteria(const Value: TCondition);
begin
  FFillCriteria.Free;
  FFillCriteria := Value.CreateCopy;
end;

procedure TDBUDataCombo.SetFillTimeStamp(const Value: TDateTime);
begin
  FFillTimeStamp := Value;
end;


{ TFieldWidthOrganizer }

constructor TFieldWidthOrganizer.Create;
begin
  FWidths := TIndexContainer.Create(53, True);
  DefaultWidth := 80;
end;

destructor TFieldWidthOrganizer.Destroy;
begin
  inherited;
  FWidths.Free;
end;

function TFieldWidthOrganizer.GetWidth(AIdx: TObject): Integer;
var
  Data : TObject;
begin
  if FWidths.Contains(AIdx, Data) then
    Result := Integer(Data)
  else
    Result := DefaultWidth;
end;

function TFieldWidthOrganizer.HasWidth(AIdx: TObject): Boolean;
begin
  Result := FWidths.Contains(AIdx);
end;

function TFieldWidthOrganizer.HasWidth(AIdx: TObject;
  var AWidth: Integer): Boolean;
var
  Data : TObject;
begin
  Result := FWidths.Contains(AIdx, Data);
  if Result then
    AWidth := Integer(Data)
  else
    AWidth := DefaultWidth;
end;

function TFieldWidthOrganizer.RemoveWidth(AIdx : TObject; var AWidth : Integer) : Boolean;
var
  Data : TObject;
begin
  Result := FWidths.Remove(AIdx, Data);
  if Result then
    AWidth := Integer(Data)
  else
    AWidth := DefaultWidth;
end;

procedure TFieldWidthOrganizer.SetDefaultWidth(const Value: Integer);
begin
  FDefaultWidth := Value;
end;

procedure TFieldWidthOrganizer.SetWidth(AIdx: TObject;
  const Value: Integer);
begin
  FWidths.Add(AIdx, TObject(Value));
end;

end.






