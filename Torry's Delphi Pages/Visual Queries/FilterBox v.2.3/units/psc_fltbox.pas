{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
Unit psc_fltbox;
Interface
{$I psc_defines.inc}

Uses
{$IFDEF D6}
  variants,
{$ENDIF}

  Types,
  typinfo,
  sysutils,
  winapi.Windows,
  Forms,
  Controls,
  winapi.Messages,
  Classes,
  DB,
  DBCommon,
  dialogs,
  graphics,
  grids,
  dbgrids,

  myla_system,
  myla_interfaces,
  myla_parser,

  psc_listbox,
  psc_expreval,
  psc_edit,
  psc_edit_date,
  psc_edit_parts,
  psc_calculator,
  psc_parser_date,
  psc_wrapper,
  psc_procs,
  psc_const;

{----------------------------}

type
  TPSCWhereInc = (wiAuto,wiYes,wiNo);

  TPSCFltTemplateFilterKind=(
    tfkString,
    tfkProc
  );

  TPSCFltTemplateOption=(
    ftoCanIgnoreCase,
    ftoValueList
  );

  TPSCFltTemplateOptions=set of TPSCFltTemplateOption;

{----------------------------}

const
  ACT_FLTBLD_SETFLDORDER=1;

  cPSCFltBoxDefaultOptions =
    [fboSortFields,fboSortConditions,
    fboCanAdd,fboCanDelete,fboCanIndent,fboCanOpen,fboCanSave,
    fboCanSaveToMem,fboCanLoadFromMem,fboAutoSizePopups,
    fboLookupDSOpen];

//BeginSkipConst
  CPSCDefDateTimeParts=[Low(TPSCDateTimePart)..High(TPSCDateTimePart)];
  cPSCDefFiltered = True;
  cPSCDefAllowedUsageIDs: String = '0';
  cPSCDefUpdateOnLoaded = False;

  SPSCFieldParam = ':Field';  {never change}
  SPSCOpParam = ':Operation'; {never change}
  SPSCTemplateParam = ':Template'; {never change}

  SPSCTempl0 = ':Field :Template';
  SPSCTempl1 = ':Field :Template :Value';

  SPSCFltVar_ValuePlusOne = 'SYSVALUEPLUSONE';
  SPSCFltVar_Yesterday = 'SYSYESTERDAY';
  SPSCFltVar_Tomorrow = 'SYSTOMORROW';
  SPSCFltVar_TodayM7 = 'SYSTODM7';
  SPSCFltVar_Today = 'SYSTODAY';
  SPSCFltVar_TodayP7 = 'SYSTODP7';
  SPSCFltVar_TodayP2 = 'SYSTODP2';
  SPSCFltVar_TodayP8 = 'SYSTODP8';
  SPSCFltVar_TodayM8 = 'SYSTODM8';
  SPSCFltVar_LastWeek1 = 'SYSLastWeek1';
  SPSCFltVar_LastWeek7 = 'SYSLastWeek7';
  SPSCFltVar_NextWeek8 = 'SYSNextWeek8';
  SPSCFltVar_ThisWeek1 = 'SYSThisWeek1';
  SPSCFltVar_ThisWeek7 = 'SYSThisWeek7';
  SPSCFltVar_NextWeek1 = 'SYSNextWeek1';
  SPSCFltVar_NextWeek7 = 'SYSNextWeek7';
  SPSCFltVar_LastMon1 = 'SYSLastMon1';
  SPSCFltVar_LastMon31 = 'SYSLastMon31';
  SPSCFltVar_ThisMon1 = 'SYSThisMon1';
  SPSCFltVar_ThisMon31 = 'SYSThisMon31';
  SPSCFltVar_NextMon1 = 'SYSNextMon1';
  SPSCFltVar_NextMon31 = 'SYSNextMon31';
  SPSCFltVar_NextMon32 = 'SYSNextMon32';

  SPSCFltUpper_UPPER='UPPER';
  SPSCFltUpper_UCASE='UCASE';

  SPSCFlt_FieldLikeUpper = '%S(:Field) LIKE %S(:Value)';

  SPSCFlt_StringFieldEUpper = '%S(:Field) = %S(:Value)';

  SPSCFlt_FieldLike = ':Field LIKE :Value';
  SPSCFlt_FieldE = ':Field = :Value';
  SPSCFlt_FieldNE = ':Field <> :Value';
  SPSCIsNotNull = 'IS NOT NULL';
  SPSCIsNull = 'IS NULL';
  SPSCFlt_FieldIsNull = ':Field ' + SPSCIsNull;
  SPSCFlt_FieldIsNotNull =':Field '+SPSCIsNotNull;
  SPSCFlt_FieldGE = ':Field >= :Value';
  SPSCFlt_FieldLE = ':Field <= :Value';
  SPSCFlt_FieldBet = '((:Field >= :Value1) and (:Field <= :Value2))';
  SPSCFlt_FieldG = ':Field > :Value';
  SPSCFlt_FieldL = ':Field < :Value';

  SPSCFlt_FieldDateE = '((:Field >= :Value) and (:Field < :SYSVALUEPLUSONE))';

  SPSCFlt_FieldToday = '((:Field >= :' + SPSCFltVar_Today + ') and (:Field < :'
    + SPSCFltVar_Tomorrow + '))';
  SPSCFlt_FieldYest = '((:Field >= :' + SPSCFltVar_Yesterday +
    ') and (:Field < :' + SPSCFltVar_Today + '))';
  SPSCFlt_FieldTomor = '((:Field >= :' + SPSCFltVar_Tomorrow +
    ') and (:Field < :' + SPSCFltVar_TodayP2 + '))';

  SPSCFlt_FieldTodayM7 = '((:Field <= :' + SPSCFltVar_Today + ') and (:Field > :'
    + SPSCFltVar_TodayM7 + '))';
  SPSCFlt_FieldTodayP7 = '((:Field >= :' + SPSCFltVar_Today +
    ') and (:Field < :' + SPSCFltVar_TodayP7 + '))';

  SPSCFlt_FieldLastWeek = '((:Field <= :' + SPSCFltVar_LastWeek7 +
    ') and (:Field >= :' + SPSCFltVar_LastWeek1 + '))';
  SPSCFlt_FieldThisWeek = '((:Field < :' + SPSCFltVar_NextWeek1 +
    ') and (:Field >= :' + SPSCFltVar_ThisWeek1 + '))';
  SPSCFlt_FieldNextWeek = '((:Field < :' + SPSCFltVar_NextWeek8 +
    ') and (:Field >= :' + SPSCFltVar_NextWeek1 + '))';

  SPSCFlt_FieldLastMon = '((:Field < :' + SPSCFltVar_ThisMon1 +
    ') and (:Field >= :' + SPSCFltVar_LastMon1 + '))';

  SPSCFlt_FieldThisMon = '((:Field < :' + SPSCFltVar_NextMon1 +
    ') and (:Field >= :' + SPSCFltVar_ThisMon1 + '))';
  SPSCFlt_FieldNextMon = '((:Field < :' + SPSCFltVar_NextMon32 +
    ') and (:Field >= :' + SPSCFltVar_NextMon1 + '))';

  cDefaultWhereInc = wiAuto;
  cDefaultExcludeBlobs = True;
  cDefaultUsageId = 0;

  SPSCFlt_FieldLike_Proc          = 'PROC_FIELDLIKE';
  SPSCFlt_FieldNotLike_Proc       = 'PROC_FIELDNOTLIKE';
  SPSCFlt_BlankOrEmpty_Proc       = 'PROC_BLANKOREMPTY';
  SPSCFlt_StringFieldE_Proc       = 'PROC_STRINGFIELDE';
  SPSCFlt_StringFieldNE_Proc      = 'PROC_STRINGFIELDNE';
  SPSCFlt_BooleanFieldE_Proc      = 'PROC_BOOLEANFIELDE';
  SPSCFlt_FieldIn_Proc            = 'PROC_FIELDIN';
  SPSCFlt_FieldNotIn_Proc         = 'PROC_FIELDNOTIN';
  SPSCFlt_AdditionalFilterIs_Proc = 'PROC_ADDITIONALFILTERIS';
  SPSCFlt_FieldIsNull_Proc        = 'PROC_FIELDISNULL';
  SPSCFlt_FieldIsNotNull_Proc     = 'PROC_FIELDISNOTNULL';

  SPSCFltSaveFileExt                         :String = '.flt';

  SPSCTemplCatAdditionalFilter               = 'AdditionalFilter';
  SPSCTemplCatUnknown                        = 'Unknown';
  SPSCTemplCatText                           = 'Text';
  SPSCTemplCatNumber                         = 'Number';
  SPSCTemplCatLookup                         = 'Lookup';
  SPSCTemplCatBoolean                        = 'Boolean';
  SPSCTemplCatDateTime                       = 'DateTime';
  SPSCTemplCatDate                           = 'Date';
  SPSCTemplCatTime                           = 'Time';
  SPSCTemplCatMemo                           = 'Memo';

  SPSCAdditionalFilterField='{6C7AFB76-3834-4547-8798-DF4A5145EB26}';

  cPSCFltBoxSortAscKey: TPSCKeyDef = (KeyCode: Word('A'); ShiftState: [ssCtrl]);
  cPSCFltBoxSortDescKey: TPSCKeyDef = (KeyCode: Word('D'); ShiftState: [ssCtrl]);
  cPSCFltBoxSortClearKey: TPSCKeyDef = (KeyCode: Word('U'); ShiftState: [ssCtrl]);
  cPSCFltBoxFindNextKey: TPSCKeyDef = (KeyCode: VK_F3; ShiftState:[]);
  cPSCFltBoxFindPrevKey: TPSCKeyDef = (KeyCode: VK_F3; ShiftState:[ssShift]);
  cPSCFltBoxFindLastKey: TPSCKeyDef = (KeyCode: Word('F'); ShiftState:[ssCtrl,ssShift]);
  cPSCFltBoxFindFirstKey: TPSCKeyDef = (KeyCode: Word('F'); ShiftState:[ssCtrl]);
  cPSCFltBoxToggleFilteredKey: TPSCKeyDef = (KeyCode: Word('T'); ShiftState:[ssCtrl]);

//EndSkipConst

Procedure PSCSetDataSetSQL(DataSet: TDataSet; SQL: TStrings;
  Activate: boolean;const ASQLPropName:String='SQL'); //don't resource

const
// Don't make conditions without parameters first in the templates list
// Otherwise condition without parameters will be used when any condition
// is added (non needed filter change will occur)

//=============================== _TEMPLATES_ ================================

// !!!!!!!!!!! Please don't change CapID strings !!!!!!!!!!!!!!!!
  SPSCCapID_In                                 = '#in';
  SPSCCapID_NotIn                              = '#not in';
  SPSCCapID_Cont                               = '#contains';
  SPSCCapID_Is                                 = '#exactly';
  SPSCCapID_NotCont                            = '#not contain';
  SPSCCapID_IsEmpty                            = '#empty';
  SPSCCapID_IsNEmpty                           = '#not empty';
  SPSCCapID_Equals                             = '#equals';
  SPSCCapID_NEquals                            = '#not equal';
  SPSCCapID_AtMost                             = '#at most';
  SPSCCapID_AtLeast                            = '#at least';
  SPSCCapID_More                               = '#more than';
  SPSCCapID_Less                               = '#less than';
  SPSCCapID_Between                            = '#between';
  SPSCCapID_Yesterday                          = '#yesterday';
  SPSCCapID_Today                              = '#today';
  SPSCCapID_Tomorrow                           = '#tomorrow';
  SPSCCapID_Last7                              = '#last 7 days';
  SPSCCapID_Next7                              = '#next 7 days';
  SPSCCapID_LastWeek                           = '#last week';
  SPSCCapID_ThisWeek                           = '#this week';
  SPSCCapID_NextWeek                           = '#next week';
  SPSCCapID_LastMon                            = '#last month';
  SPSCCapID_ThisMon                            = '#this month';
  SPSCCapID_NextMon                            = '#next month';
  SPSCCapID_On                                 = '#on';
  SPSCCapID_OnAfter                            = '#on or after';
  SPSCCapID_Onbefore                           = '#on or before';
  SPSCCapID_After                              = '#after';
  SPSCCapID_Before                             = '#before';
  SPSCCapID_BlankEmpty                         = '#blank or empty';
  SPSCCapID_Begins                             = '#begins with';
  SPSCCapID_Ends                               = '#ends with';
  SPSCCapID_OnDate                             = '#on date';

  cDefaultDescending = False;
  cDefaultActivateDataSet = True;
  cDefaultEncloseInBrackets = True;

Type
  TPSCCustomFltBld = Class;

  TPSCOrderByItem = Class(TPSCNamedItem)
  private
    FDataField: String;
    FDescending: boolean;
    Procedure SetDataField(Const V: String);
    Procedure SetDescending(V: boolean);
  protected
  public
    Function GetFltBld: TPSCCustomFltBld;
  published
    Property DataField: String read FDataField write SetDataField;
    Property Descending: boolean read FDescending write SetDescending default
      cDefaultDescending;
  End;

  TPSCOrderByItems = Class(TPSCNamedItems)
  private
    Function GetItem(Index: Integer): TPSCOrderByItem;
    Procedure SetItem(Index: Integer; V: TPSCOrderByItem);
  public
    Function GetFltBld: TPSCCustomFltBld; virtual;
    Property Items[Index: Integer]: TPSCOrderByItem read GetItem write SetItem;
      default;
  End;

  TPSCFltBoxTemplate = Class(TPSCNamedItem)
  private
    FOptions:TPSCFltTemplateOptions;
    FFilterKind: TPSCFltTemplateFilterKind;
    FCategory: String;
    FCaption: String;
    FFilter: String;
    FCaptionID: String;
    FCaptionForUser: String;
    FUsageID: Integer;
    FValueSuffix: String;
    FValuePrefix: String;
    FParamsType: TPSCFieldType;
    Procedure SetCaptionForUser(Const V: String);
    Procedure SetCategory(Const V: String);
    Procedure SetValuePrefix(Const V: String);
    Procedure SetValueSuffix(Const V: String);
    Procedure SetCaptionID(Const V: String);
    Procedure SetCaption(Const V: String);
    Procedure SetFilter(Const V: String);
    Procedure SetParamsType(V:TPSCFieldType);
  protected
    Function GetDisplayName: String; override;
  public
    destructor Destroy;override;
  published
    property Options:TPSCFltTemplateOptions Read FOptions Write FOPtions Default [];
    property FilterKind: TPSCFltTemplateFilterKind Read FFilterKind Write FFilterKind Default tfkString;
    Property Category: String read FCategory write SetCategory;
    Property Caption: String read FCaption write SetCaption;
    Property CaptionID: String read FCaptionID write SetCaptionID;
    Property Filter: String read FFilter write SetFilter;
    Property UsageID: Integer read FUsageID write FUsageID default
      cDefaultUsageId;
    Property ValuePrefix: String read FValuePrefix write SetValuePrefix;
    Property ValueSuffix: String read FValueSuffix write SetValueSuffix;
    Property ParamsType: TPSCFieldType Read FParamsType Write SetParamsType Default FT_UNK;
    Property CaptionForUser: String Read FCaptionForUser Write SetCaptionForUser;
  End;

  TPSCFltBldOption=(
    fboBooleanAsBit,
    fboNoLookupCat,
    fboHideAdditionalFilter,
    fboAutoSearch,
    fboAddConditionBrackets,
    fboIncludeCalculated,
    fboSimpleBoolConditions,
    fboHideTemplate_LIKE,
    fboHideTemplate_IN,
    fboTemplatesStored,
    fboDoubleQuoteInValue,
    fboUseUCASEioUPPER,
    fboUseRTRIM,
    fboUseDollarioIN,
    fboUseEMPTYioNULL,
    fboUseCommandTextioSQL,
    fboUseADOFieldNameMask
  );

  TPSCFltBldOptions=set of TPSCFltBldOption;

  IPSCStrings=myla_interfaces.IPSCStrings;

  TPSCFltBoxTemplates = Class(TPSCNamedItems)
  private
    Function IndexOfCaptionID(Const ACategory,ACaptionID: String): Integer;
    Function ItemWithCaptionID(Const ACategory,ACaptionID: String):
      TPSCFltBoxTemplate;
    Procedure GetTemplatesWithCat(Const ACategory: String;
      const ATemplates: IPSCStrings;
      Const AllowedUsageIDs: String;AForDisplay:Boolean;
      AOptions:TPSCFltBldOptions);
    Function GetItem(Index: Integer): TPSCFltBoxTemplate;
    Procedure SetItem(Index: Integer; V: TPSCFltBoxTemplate);
  protected
    procedure AddTemplates_AdditionalFilter(const ACategory:String);virtual;
    procedure AddTemplates_Time(const ACategory:String);virtual;
    procedure AddTemplates_Date(const ACategory:String);virtual;
    procedure AddTemplates_DateTime(const ACategory:String);virtual;
    procedure AddTemplates_Memo(const ACategory:String);virtual;
    procedure AddTemplates_Text(const ACategory:String);virtual;
    procedure AddTemplates_Boolean(const ACategory:String);virtual;
    procedure AddTemplates_Unknown(const ACategory:String);virtual;
    procedure AddTemplates_Lookup(const ACategory:String);virtual;
    procedure AddTemplates_Number(const ACategory:String);virtual;
  public
    Procedure InitWithDefault;
    Procedure EnumTemplCategories(const TemplCategories: IPSCStrings);
    Property Items[Index: Integer]: TPSCFltBoxTemplate read GetItem write SetItem;
      default;
  End;

  TPSCFltBoxField = Class(TPSCLinkedItem)
  private
    FPickType: TPSCPickType;
    FDisplayLabel: String;
    FDataField: String;
    FTemplateCat: String;
    FDataType: TPSCFieldType;
    FPickList: TStrings;
    FLookupKeyField: String;
    FLookupDisplayField: String;
    FLookupGridFields: String;
    FLookupDataSet: TDataSet;
    FLookupFilterSource: TPSCCustomFltBld;
    FTableName: String;
    FSQLFieldName: String;
    FFieldNumber: Integer;
    FVisible:Boolean;

    procedure SetLookupFilterSource(V: TPSCCustomFltBld);
    Procedure AssignDataSetField(Field: TField);
    Procedure SetLookupDataSet(V: TDataSet);
    Procedure SetDataField(Const V: String);
    Procedure SetDisplayLabel(Const V: String);
    Procedure SetTemplateCat(Const V: String);
    Procedure SetDataType(V: TPSCFieldType);
    Procedure SetPickList(V: TStrings);
    Procedure SetFieldNumber(V:Integer);
  protected
    Function GetDisplayName: String; override;
    Procedure Notification(Instance: TComponent); override;
  public
    Function GetFltBld: TPSCCustomFltBld;
    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
  published
    Property FieldNumber:Integer Read FFieldNumber Write SetFieldNumber Default -1;
    Property DataField: String read FDataField write SetDataField;
    Property DisplayLabel: String read FDisplayLabel write SetDisplayLabel;
    Property TemplCat: String read FTemplateCat write SetTemplateCat;
    Property DataType: TPSCFieldType read FDataType write SetDataType;
    Property PickList: TStrings read FPickList write SetPickList;
    Property LookupKeyField: String read FLookupKeyField write FLookupKeyField;
    Property LookupDisplayField: String read FLookupDisplayField write
      FLookupDisplayField;
    Property LookupGridFields: String read FLookupGridFields write
      FLookupGridFields;
    Property LookupDataSet: TDataSet read FLookupDataSet write SetLookupDataSet;
    Property LookupFilterSource: TPSCCustomFltBld Read FLookupFilterSource Write SetLookupFilterSource;
    Property TableName: String read FTableName write FTableName;
    Property SQLFieldName: String read FSQLFieldName write FSQLFieldName;
    Property PickType: TPSCPickType read FPickType write FPickType Default ptAuto;
    Property Visible:Boolean Read FVisible Write FVisible Default True;
  End;

  TPSCFltBoxFields = Class(TPSCNamedItems)
  private
    function GetItem(Index: Integer): TPSCFltBoxField;
    procedure SetItem(Index: Integer; Value: TPSCFltBoxField);
  protected
    function GetHiddenProps:TArray<string>;override;
  public
    Procedure AppendFieldsList(const AFieldsList: IPSCStrings; AFieldType: TPSCFieldType);
    Procedure GetFieldsList(const FieldsList: IPSCStrings; AFieldType: TPSCFieldType);
    Function GetTemplCategory(Const FieldName: String): String;
    Function FieldByName(Const FieldName: String): TPSCFltBoxField;
    Function GetFltBld: TPSCCustomFltBld; virtual;
    property Items[Index:Integer]:TPSCFltBoxField Read GetItem Write SetItem;
  End;

  TPSCFltParam = class(TPSCListParam)
  private
  protected
  public
  published
  end;

  TPSCFltItemOption=(
    FltItem_ReadOnly_Field,
    FltItem_ReadOnly_Template,
    FltItem_ReadOnly_Operation,
    FltItem_ReadOnly_Value,
    FltItem_ReadOnly_Value1,
    FltItem_ReadOnly_Value2
  );

  TPSCFltItemOptions=set of TPSCFltItemOption;

  TPSCFltItem = Class(TPSCListItem)
  private
    FUserData: Cardinal;
    FTemplCat: String;
    FDateTimeParts: TPSCDateTimeParts;

    function IsItemOptionsStored:Boolean;
    function GetItemOptions:TPSCFltItemOptions;
    procedure SetItemOptions(v:TPSCFltItemOptions);

    procedure InternalSetValue(const AName:String;const AValue:Variant);
    Procedure InternalWriteValue(Writer: TWriter);
    Procedure InternalWriteValue1(Writer: TWriter);
    Procedure InternalWriteValue2(Writer: TWriter);
    Procedure InternalReadValue(Reader: TReader);
    Procedure InternalReadValue1(Reader: TReader);
    Procedure InternalReadValue2(Reader: TReader);

    Function IsDateTimePartsStored:Boolean;
    Function GetDataField: String;
    Function GetOperation: String;
    Function GetTemplate: String;
    Function GetValue: Variant;
    Function GetValue1: Variant;
    Function GetValue2: Variant;
    Function IsOperationStored:Boolean;

    Procedure SetDateTimeParts(AValue: TPSCDateTimeParts);
    Procedure SetOperation(Const V: String);
    Procedure SetTemplate(Const V: String);
    Procedure SetDataField(Const V: String);
    Procedure SetValue(V: Variant);
    Procedure SetValue1(V: Variant);
    Procedure SetValue2(V: Variant);

  protected
    Procedure CaptionChanged; override;
    Function GetListParamClass: TPSCListParamClass; override;
    Procedure DefineProperties(Filer: TFiler);override;
  public
    Function GetFltBld: TPSCCustomFltBld;

    Procedure Assign(Source: TPersistent); override;

    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;

    Property UserData: Cardinal read FUserData write FUserData;
  published
    Property DateTimeParts: TPSCDateTimeParts Read FDateTimeParts Write
      SetDateTimeParts Stored IsDateTimePartsStored;
    Property DataField: String read GetDataField write SetDataField;
    Property TemplCat: String read FTemplCat write FTemplCat;
    Property Template: String read GetTemplate write SetTemplate;
    Property Operation: String read GetOperation write SetOperation Stored IsOperationStored;
    Property IndentLev;
    Property Checked;
    Property Value: Variant read GetValue write SetValue Stored False;
    Property Value1: Variant read GetValue1 write SetValue1 Stored False;
    Property Value2: Variant read GetValue2 write SetValue2 Stored False;
    Property ReadOnly;
    Property ItemOptions:TPSCFltItemOptions Read GetItemOptions Write SetItemOptions Stored IsItemOptionsStored;
    Property Options;
    Property UserFields;
  End;

  TPSCFltItems = Class(TPSCListItems)
  private
    FDummySQLDateTimeFormat:TPSCDateTimeFormat;
    FDummyFilterDateTimeFormat:TPSCDateTimeFormat;
    Function GetItem(AIndex:Integer):TPSCFltItem;
    procedure SetItem(AIndex:Integer;V:TPSCFltItem);
  protected
    function GetHiddenProps:TArray<string>;override;
    function GetRealSQLDateTimeFormat:TPSCDateTimeFormat;override;
    function GetRealFilterDateTimeFormat:TPSCDateTimeFormat;override;
  public
    Procedure DoQuoteStr(Sender: TObject; Field: TPSCField; Var
      QuotedStr: String);override;
    Procedure GetAsString(Sender: TObject; Field: TPSCField;
      Var ResultStr: String; ForDisplay: boolean); override;
    Function GetFltBld: TPSCCustomFltBld; virtual;

    Constructor Create(AOwner: TPersistent; ItemClass: TPSCNamedItemClass);
      override;
    Destructor Destroy; override;

    Function IsPrmVisible(Item: TPSCListItem;
      Param: TPSCListParam): boolean; override;
    Property Items[Index: Integer]: TPSCFltItem read GetItem write SetItem;
      default;
  End;

  TPSCFltItemsClass=class of TPSCFltItems;

  TPSCFltBoxMemorySlot = Class(TPSCListMemorySlot)
  protected
    Function GetListItemsClass: TPSCListItemsClass; override;
  End;

  TPSCPrepareFieldNameEvent = Procedure(Sender: TObject; Const FieldName:
    String;Var PreparedFieldName: String) Of Object;

  TPSCPrepareOrderByTextEvent = Procedure(Sender: TObject;
    var APreparedOrderBy:String) Of Object;

  TPSCFieldSortType=(
    SortType_Unsorted,
    SortType_Ascending,
    SortType_Descending
  );

  TPSCFltOrderByStyle=(obsFieldNames,obsFieldNumbers);

  TPSCListItemToStrProc=function(Item: TPSCListItem): String of object;

  TPSCGetItemFilterStrEvent=procedure(Sender:TObject; AItem:TPSCFltItem;
    var AFilterStr:String) of object;

  IPSCDataFields=myla_interfaces.IPSCDataFields;

  TPSCOnPermitAction=procedure(Sender:TObject;
    AID:Integer;const AParams:IPSCDataFields; var APermit:LongBool) of object;

  TPSCCustomFltBld = Class(TPSCComponent)
  private
    FOnPermitAction:TPSCOnPermitAction;
    FOnGetItemFilterStr:TPSCGetItemFilterStrEvent;
    FSQLPropName:String;
    FEvents:IPSCEvents;
    FOnPrepareOrderByText:TPSCPrepareOrderByTextEvent;
    FAdditionalFields: TPSCFltBoxFields;
    FOnPrepareFieldName: TPSCPrepareFieldNameEvent;
    FUpdateOnLoaded: boolean;
    FOnChange: TPSCNotifyEvent;
    FOldOrderByText: String;
    FOrderByChanged: Boolean;
    FFieldNameMask: String;
    FDataSet: TDataSet;
    FMemorySlots: TPSCListMemorySlots;
    FAddTableNames: boolean;
    FTableNames: TStrings;
    FEncloseInBrackets: boolean;
    FOnGetLookupDef: TPSCOnGetLookupDef;
    FTargetIsSQL: boolean;
    FSQL: TStrings;
    FOldFilterStr,FOldSQLStr: String;
    FItems: TPSCFltItems;
    FOnModifyPickList: TPSCOnModifyPickList;
    FFilterStrChanged: boolean;
    FSQLOBTail: TStrings;
    FWhereInc: TPSCWhereInc;
    FHiddenFields: TStrings;
    FSQLAlreadyTaken: boolean;
    FActivateDataSet: boolean;
    FSQLHead: TStrings;
    FSQLHeadOper: String;
    FSQLTail: TStrings;
    FOrderByItems: TPSCOrderByItems;
    FAllowedUsageIDs: String;
    FFields: TPSCFltBoxFields;
    FFieldParams: TPSCFltBoxFields;
    FExcludeBlobs: boolean;
    FTemplates: TPSCFltBoxTemplates;
    FFiltered: boolean;
    FAdditionalFilter: String;
    FFilterOptions: TPSCFltBldOptions;
    FVersion: String;
    FUseLocalSettings: Boolean;
    FOrderByStyle:TPSCFltOrderByStyle;
    FOnGetAsString: TPSCOnGetAsString;
    FOnQuoteStr: TPSCOnQuoteStr;

    Function IsTemplatesStored:Boolean;
    Function IsSQLHeadOperStored:Boolean;
    Function ConditionBracketsNeeded:Boolean;
    function IsFilterOptionsStored:Boolean;
    function GetRealFieldNameMask:String;
    Function IsAdditionalFieldsStored:Boolean;
    Function IsItemsStored: boolean;
    Function IsMemorySlotsStored: boolean;
    Function IsFieldsStored: boolean;
    Function IsFieldParamsStored: boolean;
    Function IsOrderByItemsStored: boolean;
    Function IsAllowedUsageIDsStored: boolean;
    Function GetSQLFilterStrEx(UseExpressionProp: boolean): String;
    Function GetFilterStrChanged: boolean;
    Function GetOrderByChanged: boolean;
    Function GetDataSet: TDataSet;
    Function GetFilterOptions: TPSCFltBldOptions;
    Function GetMemorySlots: TPSCListMemorySlots;
    Function ReplaceWithSysPrms(Item: TPSCFltItem;Const S: String;
      AParams:TPSCFields): String;
    Function GetItems: TPSCFltItems;
    Function GetSQL: TStrings;
    Function GetFilterStr: String;
    Function GetForceSQLDateTime: boolean;
    Function GetFilterDateTimeFormat: TPSCDateTimeFormat;
    Function GetSQLDateTimeFormat: TPSCDateTimeFormat;
    Function GetDisplayDateTimeFormat: TPSCDateTimeFormat;
    Function GetSQLHeadText: String;
    function GetRealSQLPropName: String;

    procedure FieldsChanged(Sender:TObject);
    procedure SetVersion(const V:String);
    Procedure SetAdditionalFields(V: TPSCFltBoxFields);
    Procedure SetFilterOptions(V: TPSCFltBldOptions);
    Procedure SetMemorySlots(V: TPSCListMemorySlots);
    Procedure SetTableNames(V: TStrings);
    Procedure SetSQL(V: TStrings);
    Procedure SetItems(V: TPSCFltItems);
    Procedure SetFiltered(V: boolean);
    Procedure SetHiddenFields(V: TStrings);
    Procedure SetAdditionalFilter(Const V: String);
    Procedure SetSQLHead(V: TStrings);
    Procedure SetSQLTail(V: TStrings);    
    Procedure SetOrderByItems(V: TPSCOrderByItems);
    Procedure SetSQLOBTail(V: TStrings);
    Procedure SetDataSet(V: TDataSet);
    Procedure SetFields(V: TPSCFltBoxFields);
    Procedure SetFieldParams(V: TPSCFltBoxFields);
    Procedure SetTemplates(V: TPSCFltBoxTemplates);
    Procedure SetEncloseInBrackets(V: boolean);
    Procedure SetForceSQLDateTime(V: boolean);
    Procedure SetFilterDateTimeFormat(V: TPSCDateTimeFormat);
    Procedure SetSQLDateTimeFormat(V: TPSCDateTimeFormat);
    Procedure SetDisplayDateTimeFormat(V: TPSCDateTimeFormat);
    Procedure SetAddTableNames(V: boolean);
    Procedure SetUseLocalSettings(AValue: Boolean);
    Procedure OnUpdate(Sender: TObject; Item: TPSCNamedItem);
    Procedure GetDisplayFieldsList(const AStrings: IPSCStrings);
    function GetFieldLookupFilterSource(const AFieldName:String):TPSCCustomFltBld;
    function CanSetFieldSortOrder(const AFieldName:String;
      ASortType:TPSCFieldSortType;AClearExisting:Boolean):Boolean;
  protected
    function ActionPermitted(AID:Integer;
      const AParams:IPSCDataFields):LongBool;virtual;
    Procedure UpdateParamDisplayValue(AItem:TPSCFltItem;AParam:TPSCListParam);
    procedure UpdateDisplayValues(AItem:TPSCFltItem);overload;
    Procedure GetFieldPickList(Const FieldName: String; const PickList: IPSCStrings);virtual;
    Procedure DoModifyPickList(Const FieldName: String; const PickList: IPSCStrings);virtual;
    Procedure GetDisplayFieldsListEx(const AStrings: IPSCStrings;
      AFieldType:TPSCFieldType);virtual;
    procedure UpdateDateTimeFormat(AFormat:TPSCDateTimeFormat;AForSQL:Boolean);virtual;
    Procedure RemoveUsedFields(const FieldsList: IPSCStrings); virtual;
    Procedure Notification(AComponent: TComponent; AOperation: TOperation);
      override;
    Procedure Loaded; override;
    Procedure GetParamPickList(Item: TPSCFltItem; Param: TPSCListParam;
      const PickList: IPSCStrings);virtual;
    Procedure DoQuoteStr(AField:TPSCField;var AQuotedStr:String);virtual;
    Procedure DoGetAsString(AField:TPSCField;var AResultStr:String;
      AForDisplay:Boolean);virtual;

    Function GetFieldPickType(Const FieldName: String): TPSCPickType;virtual;
    Function ParamHasPickList(Item: TPSCFltItem; Param: TPSCListParam): boolean;virtual;
    Function GetFieldLookupDef(Const FieldName: String): TPSCLookupDef;virtual;
    Function GetFieldDisplayValue(Const FieldName: String): String;virtual;
    Function GetFirstField: String;virtual;
    Function GetTemplCategory(Const FieldName: String): String;virtual;
    Function CanChanged:Boolean;virtual;
    function FltToDispOperation(Const Operation: String): String;virtual;
    Function GetExpression:String;virtual;
    function GetExpressionEx(AItems:TPSCFltItems;
      AForDisplay:Boolean;AProc:TPSCListItemToStrProc):String;virtual;
    Function IncludeItemInFilter(Item: TPSCFltItem): boolean;virtual;
    Function GetParamLookupDef(Item: TPSCFltItem; Param: TPSCListParam):
      TPSCLookupDef;virtual;
    Function GetParamPickType(Item: TPSCListItem; Param: TPSCListParam):
      TPSCPickType;virtual;
    Function GetPreparedFilterStr(Item: TPSCFltItem;
      Const FilterStr,FieldName: String;
      ReplaceSysPrms: boolean; Params: TPSCFields): String;virtual;
    Function GetFieldType(Const FieldName: String): TPSCFieldType;virtual;

    Property Events:IPSCEvents Read FEvents;
  public
    Function GetRealFilterOptions:TPSCFltBldOptions;virtual;
    procedure UpdateDisplayValues(AItems:TPSCFltItems);overload;
    procedure LoadFromFile(const AFileName:String);
    procedure SaveToFile(const AFileName:String);
    procedure LoadFromStream(AStream:TStream);
    procedure SaveToStream(AStream:TStream);

    procedure LoadFromFileDlg;
    procedure SaveToFileDlg;

    Function GetItemFilterStr(Item: TPSCFltItem): String;virtual;
    Function FindRecord(ADirection: TPSCSearchType): Boolean;virtual;
    function GetFieldNumber(Const FieldName: String): Integer;virtual;
    Function GetFieldSortOrder(const AFieldName:String;
      var ASortOrder:Integer):TPSCFieldSortType;virtual;
    Function IsFieldLookup(Const FieldName: String): Boolean;virtual;
    Function GetFieldIDValue(Const FieldName,DisplayValue: String): Variant;virtual;
    Function GetPreparedFieldName(Const AFieldName: String; AForSQL: boolean):
      String; virtual;
    Function GetSQLFieldName(Const FieldName: String): String; virtual;
    Function GetFieldTableName(Const FieldName: String): String; virtual;
    Function ReallyAddTableNames: boolean;virtual;
    Function GetSQLExpression: String;virtual;
    Function GetSQLFilterStr: String;virtual;
    Function GetOrderByText: String;virtual;
    Function GetFirstUnusedField: String; virtual;
    Function GetFirstTemplate(Const FieldName: String;
      AForDisplay:Boolean): String; virtual;

    Procedure GetFieldsListEx(const AFieldsList: IPSCStrings; AFieldType: TPSCFieldType;
      ASpecialFields:Boolean=True);virtual;
    procedure EnumUsedFields(const AFields:IPSCStrings;AMustBeInFilter:Boolean=False);
    Procedure SetFieldSortOrder(const AFieldName:String;
      ASortType:TPSCFieldSortType;AClearExisting:Boolean);virtual;
    Procedure UpdateDataSets;virtual;
    Procedure Changed; virtual;
    Procedure GetOperations(const Strings: IPSCStrings); virtual;
    Procedure GetOperationsForDisplay(const Strings: IPSCStrings);
    Procedure GetFieldTemplates(Const FieldName: String;
      const TemplList: IPSCStrings;AForDisplay:Boolean);virtual;
    Procedure GetFieldsList(const AFieldsList: IPSCStrings); virtual;
    Procedure Assign(Source: TPersistent); override;
    Procedure GetFieldsFromDataSet(ADataSet: TDataSet); virtual;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Property FilterStrChanged: boolean read GetFilterStrChanged write
      FFilterStrChanged;
    Property OrderByChanged: boolean read GetOrderByChanged write
      FOrderByChanged;
    Property TargetIsSQL: boolean read FTargetIsSQL;
  published
    Property Templates: TPSCFltBoxTemplates read FTemplates write SetTemplates Stored IsTemplatesStored;
    property SQLPropName:String Read FSQLPropName Write FSQLPropName;
    Property OnPrepareOrderByText:TPSCPrepareOrderByTextEvent Read
      FOnPrepareOrderByText Write FOnPrepareOrderByText;
    Property AdditionalFields: TPSCFltBoxFields read FAdditionalFields write
      SetAdditionalFields Stored IsAdditionalFieldsStored;
    Property OnPrepareFieldName: TPSCPrepareFieldNameEvent read
      FOnPrepareFieldName write FOnPrepareFieldName;
    Property UpdateOnLoaded: boolean read FUpdateOnLoaded write FUpdateOnLoaded
      default cPSCDefUpdateOnLoaded;
    Property SQLHeadOper: String read FSQLHeadOper write FSQLHeadOper Stored IsSQLHeadOperStored;
    Property OnGetAsString: TPSCOnGetAsString read FOnGetAsString write
      FOnGetAsString;
    Property FieldNameMask: String read FFieldNameMask write FFieldNameMask;
    Property FilterOptions: TPSCFltBldOptions read GetFilterOptions write
      SetFilterOptions Stored IsFilterOptionsStored;
    Property AddTableNames: boolean read FAddtableNames write SetAddTableNames
      default False;
    Property TableNames: TStrings read FTableNames write SetTableNames;
    Property OnGetLookupDef: TPSCOnGetLookupDef read FOnGetLookupDef write
      FOnGetLookupDef;
    Property FilterStr: String read GetFilterStr stored False;
    Property Expression: String read GetExpression stored False;
    Property HiddenFields: TStrings read FHiddenFields write SetHiddenFields;
    Property AllowedUsageIDs: String read FAllowedUsageIDs write FAllowedUsageIDs
      stored IsAllowedUsageIDsStored;
    Property EncloseInBrackets: boolean read FEncloseInBrackets write
      SetEncloseInBrackets default cDefaultEncloseInBrackets;
    Property ExcludeBlobs: boolean read FExcludeBlobs write FExcludeBlobs default
      cDefaultExcludeBlobs;
    Property DataSet: TDataSet read GetDataSet write SetDataSet;
    Property Fields: TPSCFltBoxFields read FFields write SetFields stored
      IsFieldsStored;
    Property FieldParams: TPSCFltBoxFields read FFieldParams write SetFieldParams
      stored IsFieldParamsStored;
    Property OnModifyPickList: TPSCOnModifyPickList read FOnModifyPickList write
      FOnModifyPickList;
    Property OrderByStyle:TPSCFltOrderByStyle Read FOrderByStyle Write FOrderByStyle Default obsFieldNames;
    Property ActivateDataSet: boolean read FActivateDataSet write
      FActivateDataSet default cDefaultActivateDataSet;
    Property SQLHead: TStrings read FSQLHead write SetSQLHead;
    Property SQLTail: TStrings read FSQLTail write SetSQLTail;
    Property OrderByItems: TPSCOrderByItems read FOrderByItems write
      SetOrderByItems stored IsOrderByItemsStored;
    Property SQLOBTail: TStrings read FSQLOBTail write SetSQLOBTail;
    Property WhereInc: TPSCWhereInc read FWhereInc write FWhereInc default
      cDefaultWhereInc;
    Property Filtered: boolean read FFiltered write SetFiltered default
      cPSCDefFiltered;
    Property AdditionalFilter: String read FAdditionalFilter write
      SetAdditionalFilter;
    Property SQL: TStrings read GetSQL write SetSQL stored False;
    Property Items: TPSCFltItems read GetItems write SetItems stored
      IsItemsStored;
    Property OnQuoteStr: TPSCOnQuoteStr read FOnQuoteStr write FOnQuoteStr;
    Property ForceSQLDateTime: boolean read GetForceSQLDateTime write
      SetForceSQLDateTime default False;
    Property FilterDateTimeFormat: TPSCDateTimeFormat read
      GetFilterDateTimeFormat write SetFilterDateTimeFormat;
    Property SQLDateTimeFormat: TPSCDateTimeFormat read GetSQLDateTimeFormat
      write SetSQLDateTimeFormat;
    Property DisplayDateTimeFormat: TPSCDateTimeFormat read
      GetDisplayDateTimeFormat write SetDisplayDateTimeFormat;
    Property MemorySlots: TPSCListMemorySlots read GetMemorySlots write
      SetMemorySlots stored IsMemorySlotsStored;
    Property UseLocalSettings: Boolean read FUseLocalSettings write
      SetUseLocalSettings default False;
    Property Version: String read FVersion write SetVersion stored false;
    Property OnChange: TPSCNotifyEvent read FOnChange write FOnChange;
    Property OnGetItemFilterStr:TPSCGetItemFilterStrEvent
      Read FOnGetItemFilterStr Write FOnGetItemFilterStr;
    Property OnPermitAction:TPSCOnPermitAction
      Read FOnPermitAction Write FOnPermitAction;
  End;

  TPSCFltBld = Class(TPSCCustomFltBld)
  published
  end;

  TPSCFltBldClass = class of TPSCFltBld;

  TPSCCustomFltBox = Class(TPSCCustomListBox)
  private
    FNormalPaint:Boolean;
    FFilterSource:TPSCCustomFltBld;
    FEventsHandler:IPSCEventHandler;

    procedure SetFilterSource(V:TPSCCustomFltBld);
    procedure ShowDebugDialog;
    procedure SetItems2(V:TPSCFltItems);

    function GetItems2:TPSCFltItems;
  protected
    Procedure HandleEvent(const AParams:TPSCEventParams);override;

    procedure PrepareLookupDataSet(ADataSet:TDataSet);override;
    procedure DefaultPickLookupParam(LookupDef: TPSCLookupDef);override;

    Function GetRealOptions:TPSCListBoxOptions;override;
    function GetItemForDisplay(AItem:TPSCListItem):String;virtual;
    function GetRightFixedWidth:Integer;override;
    function IsIntemplateOptions(AElem:TPSCFltTemplateOption):Boolean;
    function IsItemOptionEnabled(AOption:TPSCListItemOption):Boolean;override;
    Function GetItems: TPSCListItems;override;
    Function GetMemorySlots: TPSCListMemorySlots;override;
    Function CanAddItem:Boolean;override;
    function AddActionsToPopup:Integer;override;
    Function IsOverSortOrder(const P:TPoint):Boolean;
    Function GetHintStr(Var HintStr: String; CursorPos: TPoint;
      Var CursorRect: TRect): Integer;virtual;
    Function IsURLPos(P: TPoint): boolean;override;
    Function AddItemTextStored: boolean; override;
    Function GetParamPickType(Item: TPSCListItem; Param: TPSCListParam):
      TPSCPickType; override;
    Procedure GetParamPickList(Item: TPSCListItem; Param: TPSCListParam;
      const PickList: IPSCStrings); override;
    Function GetParamLookupDef(Item: TPSCListItem; Param: TPSCListParam):
      TPSCLookupDef; override;

    procedure ParamValueOnChange(Sender:TObject);override;
    Procedure ShowValueListParameter;
    Procedure CreateActions;override;
    Procedure CMHintShow(Var Message: TCMHintShow);message CM_HintShow;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer);override;
    Procedure PaintItem(AItem: Integer; ARect: TRect; AItemState:
      TPSCOwnerDrawState); override;
    Procedure OnPopupFieldClosed(Sender: TObject; Canceled: boolean); virtual;
    Procedure OnPopupValueListClosed(Sender: TObject; Canceled: boolean); virtual;
    Procedure OnPopupTemplateClosed(Sender: TObject; Canceled: boolean);
      virtual;
    Procedure OnPopupOpClosed(Sender: TObject; Canceled: boolean); virtual;
    Procedure ShowPopupParameter; override;
    procedure InitDefaultKeyMapping;override;
  public
    function GetCurrentFieldSortOrder:TPSCFieldSortType;
    function GetFilterForDisplay:String;
    function AddItem(ACheckCanAdd:Boolean):TPSCListItem; override;

    procedure PickTextParam;override;
    Procedure DeleteAllItems; override;
    procedure Paint;override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCurrentFieldSortOrder(ASortType:TPSCFieldSortType;
      AClearExisting:Boolean);
    Procedure PickOpParam; virtual;
    Procedure PickFieldParam; virtual;
    Procedure PickTemplateParam; virtual;
    Procedure PickParameter; override;
    Procedure Assign(Source: TPersistent); override;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Property FilterSource:TPSCCustomFltBld Read FFilterSource Write SetFilterSource;
    Property ShowHint Default True;
    Property ParentShowHint Default False;
    Property ShowAddItem default True;
    Property EditWithMouse default True;
    Property Items:TPSCFltItems Read GetItems2 Write SetItems2 Stored False;
    Property CheckBoxes Default True;
    Property ShowDefaultPopup Default True;
    Property Options Default cPSCFltBoxDefaultOptions;
  End;

  TPSCFltBox = Class(TPSCCustomFltBox)
  published
    Property BevelInner;
    Property BevelOuter;
    Property BevelKind;
    Property BevelWidth;
    Property BevelEdges;
    Property BorderWidth;

    Property OnLoaded;
    Property OnUpdatePopup;
    Property OnAdditem;

    Property PopupParams;
    Property WantReturns;
    Property Options;
    Property NewItemChecked;
    Property ShowDefaultPopup;
    Property MergePopupMenus;
    Property ReadOnly;
    Property HoverLinkColor;
    Property LinkColor;
    Property HideHoverLink;
    Property ClickHereColor;
    Property HideSelected;
    Property SelColor;
    Property SelFontColor;
    Property ShowAddItem;
    Property AddItemText;
    Property EditWithMouse;
    Property HideFocus;
    Property CheckBoxes;
    Property OnPickParameter;
    Property OnPickLookupParam;
    Property Version;
    Property Align;
    Property Anchors;
    Property BiDiMode;
    Property DragCursor;
    Property DragKind;
    Property ImeMode;
    Property ImeName;
    Property ParentBiDiMode;
    Property Color;
    Property Constraints;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property IntegralHeight;
    Property ParentColor;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property Visible;
    Property OnClick;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDock;
    Property OnStartDock;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnStartDrag;

    Property FilterSource;
    Property OnChange;
  end;

  TPSCFltBoxClass = class of TPSCFltBox;

  TPSCModifyDefaultTemplProc = Procedure(ATemplates: TPSCFltBoxTemplates);

  TPSCFltSysParamProcData=class
  public
    Fields:TPSCFields;
    DataType: TPSCFieldType;
    Result: Variant;
  end;

  TPSCFltSysParamProc=procedure(AParams:TPSCFltSysParamProcData);

  TPSCFltTemplateProcData=class
  public
    FilterBuilder:TPSCCustomFltBld;
    Item: TPSCFltItem;
    Result: String;
  end;

  TPSCFltTemplateProc=procedure(AParams:TPSCFltTemplateProcData);

  TPSCChangeIsNullKind = (ctIsToEq,ctEqToIs);

  IPSCFltBoxRegister=interface
    ['{8F2A338B-1F96-4BA1-9266-6101C136763A}']
    Procedure RegisterTemplateProc(Const AName: String;AProc:TPSCFltTemplateProc);
    procedure RegisterSysParam(Const AName: String;AProc:TPSCFltSysParamProc);
    function GetSysParam(const AName:String):TPSCFltSysParamProc;
    Function GetTemplateProc(const AName:String):TPSCFltTemplateProc;
  end;

  TPSCFltDlgForm = Class(TForm)
  private
    FLeftApply: integer;
    FLeftHelp: integer;
    FLeftCancel: integer;
    FOnApplyBtnClick: TPSCNotifyEvent;
    FRealBld: TPSCCustomFltBld;
    FFltBox: TPSCFltBox;
    FHeaderLabel: TPSCLabel;
    FOKButton: TPSCButton;
    FCancelButton: TPSCButton;
    FApplyButton: TPSCButton;
    FHelpButton: TPSCButton;
    FFltBld: TPSCFltBld;
    Procedure ApplyButtonClick(Sender: TObject);
    Procedure FilterBoxChange(Sender: TObject);
    Procedure UpdateBtnsLeft;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;

    Property FltBox: TPSCFltBox read FFltBox write FFltBox;

    Property HeaderLabel: TPSCLabel Read FHeaderLabel;
    Property OKButton: TPSCButton Read FOKButton;
    Property CancelButton: TPSCButton Read FCancelButton;
    Property ApplyButton: TPSCButton Read FApplyButton;
    Property HelpButton: TPSCButton Read FHelpButton;

    Property OnApplyBtnClick: TPSCNotifyEvent read FOnApplyBtnClick write
      FOnApplyBtnClick;
  End;

  TPSCFltDlgFormClass = Class Of TPSCFltDlgForm;

  TPSCFltDlg = Class(TComponent)
  private
    FHideApplyButton:Boolean;
    FCaption: String;
    FDialog: TPSCFltDlgForm;
    FFilterLabel: String;
    FOnBeforeExecute,FOnAfterExecute: TPSCNotifyEvent;
    FHelpContext: THelpContext;
    FOwnerFormCenter: boolean;
    FFilterSource:TPSCCustomFltBld;
    Procedure ApplyClick(Sender: TObject);
    Procedure ApplyFilter(AFilterBox: TPSCCustomFltBox; ARealBld: TPSCCustomFltBld);
    procedure SetFilterSource(V:TPSCCustomFltBld);
  protected
    Function GetDlgFormClass: TPSCFltDlgFormClass; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  public
    Function Execute: boolean; virtual;
    Property Dialog: TPSCFltDlgForm read FDialog;
    Constructor Create(AOwner: TComponent); override;
  published
    Property HideApplyButton:Boolean read FHideApplyButton write FHideApplyButton;
    Property Caption: String read FCaption write FCaption;
    Property FilterLabel: String read FFilterLabel write FFilterLabel;
    Property OnBeforeExecute: TPSCNotifyEvent read FOnBeforeExecute write
      FOnBeforeExecute;
    Property OnAfterExecute: TPSCNotifyEvent read FOnAfterExecute write
      FOnAfterExecute;
    Property HelpContext: THelpContext read FHelpContext write FHelpContext
      default 0;
    Property OwnerFormCenter: boolean read FOwnerFormCenter write
      FOwnerFormCenter default False;
    Property FilterSource:TPSCCustomFltBld Read FFilterSource Write SetFilterSource;
  End;

  TPSCDBGridPopupEx = class(TPSCDBGridPopup)
  private
    FSplitter:TPSCSplitter;
    FFilterBox:TPSCFltBox;
    procedure SetFilterSource(V:TPSCCustomFltBld);
    function GetFilterSource:TPSCCustomFltBld;
  protected
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;
    property FilterBox:TPSCFltBox Read FFilterBox;
    property Splitter:TPSCSplitter Read FSplitter;
    property FilterSource:TPSCCustomFltBld Read GetFilterSource Write SetFilterSource;
  end;

  TPSCFltBoxTemplatesClass=class of TPSCFltBoxTemplates;
  TPSCFltBoxTemplateClass=class of TPSCFltBoxTemplate;


  TPSCRecordSetStyleOption = (
    rssoSetColor,
    rssoSetFontColor,
    rssoSetFontBold,
    rssoSetFontItalic,
    rssoSetFontUnderline,
    rssoSetFontStrikeOut
  );

  TPSCRecordSetStyleOptions = set of TPSCRecordSetStyleOption;

  TPSCRecordSetStyle = Class(TPSCLinkedItem)
  private
    FOptions:TPSCRecordSetStyleOptions;
    FFontStyles: TPSCFontStyles;
    FColor: TPSCColor;
    FActive: Boolean;
    FDataFields: IPSCStrings;
    FFilter: String;
    FExcludeFilter: String;
    FParser: TExprParser;
    FReadOnly: Boolean;
    FParams: TPSCFields;
    FFontColor:TPSCColor;
    FFilterSource: TPSCCustomFltBld;
    FEventsHandler: IPSCEventHandler;

    procedure SetFilterSource(V:TPSCCustomFltBld);
    function IsFilterStored:Boolean;

    Function GetDataColumn: String;
    Function IsParamsStored: Boolean;

    procedure SetFontColor(V:TPSCColor);
    Procedure SetFontStyles(Value: TPSCFontStyles);
    Procedure SetColor(Value: TPSCColor);
    Procedure SetDataColumn(Value: String);
    Procedure SetFilter(Const Value: String);
    Procedure SetExcludeFilter(Const Value: String);
    Procedure SetActive(Value: Boolean);
    Procedure SetParams(Value: TPSCFields);
    Procedure SetOptions(V:TPSCRecordSetStyleOptions);
  protected
    Procedure HandleEvent(const AParams:TPSCEventParams);override;

    Procedure Notification(Instance: TComponent);override;
    Property DataFields: IPSCStrings read FDataFields;
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;

    Procedure Assign(Source: TPersistent); override;

    Property Parser: TExprParser read FParser write FParser;
  published
    Property FilterSource: TPSCCustomFltBld Read FFilterSource Write SetFilterSource;
    Property FontStyle: TPSCFontStyles read FFontStyles write SetFontStyles Default [];
    Property FontColor: TPSCColor Read FFontColor Write SetFontColor Default clPSCWindowText;
    Property Color: TPSCColor read FColor write SetColor Default clPSCWindow;
    Property DataField: String read GetDataColumn write SetDataColumn;
    Property Filter: String read FFilter write SetFilter Stored IsFilterStored;
    Property ExcludeFilter: String read FExcludeFilter write SetExcludeFilter;
    Property Params: TPSCFields read FParams write SetParams Stored IsParamsStored;
    Property ReadOnly: Boolean read FReadOnly write FReadOnly Default False;
    Property Active: Boolean read FActive write SetActive Default False;
    Property Name;
    Property UserFields;
    property Options:TPSCRecordSetStyleOptions Read FOptions Write SetOptions Default [];
  End;

  TPSCOnGetRecordSetParamValue = Procedure(Sender: TObject; AItem:
    TPSCRecordSetStyle;
    Const AName: String; Var AValue: Variant) Of Object;

  TPSCRecordSetStyles = Class(TPSCNamedItems)
  private
    Function GetItem(Index: Integer): TPSCRecordSetStyle;
    Procedure SetItem(Index: Integer; V: TPSCRecordSetStyle);
  protected
    Property Items[Index: Integer]: TPSCRecordSetStyle read GetItem write SetItem;
      default;
  End;

  TPSCGridColors = Class(TPSCCustomExprEval)
  private
    FFormatOptions:TPSCRecordSetStyleOptions;
    FFormatColor:TPSCColor;
    FFormatFontColor:TPSCColor;
    FFormatFontStyles:TPSCFontStyles;

    FRecordsInfo: TPSCRecordSetStyles;
    FOnGetItemParamValue: TPSCOnGetRecordSetParamValue;

    Function IsRecordsInfoStored: boolean;
    Function GetParser(AItem: TPSCRecordSetStyle): TExprParser;

    Procedure SetRecordsInfo(Value: TPSCRecordSetStyles);
    Procedure UpdCollectEvent(Sender: TObject; Item: TPSCNamedItem);
    Procedure UpdateCanvasFormatting(const ACanvas:TCanvas);
  protected
    Procedure DoGetItemParamValue(AItem: TObject;
      Const AName: String; Var Value: Variant); virtual;
    Procedure DoOnChange(Sender: TObject); override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Procedure UpdateFormatting(const ACanvas:TCanvas=nil;const AFieldName:String='');
    procedure DoGridDrawColumnCell(Sender: TObject;
      const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);


    property FormatFontStyle:TPSCFontStyles Read FFormatFontStyles;
    property FormatColor:TPSCColor Read FFormatColor;
    property FormatFontColor:TPSCColor Read FFormatFontColor;
    property FormatOptions:TPSCRecordSetStyleOptions Read FFormatOptions;
  published
    Property DataSet;
    Property RecordsInfo: TPSCRecordSetStyles read FRecordsInfo write
      SetRecordsInfo stored IsRecordsInfoStored;
    Property Fields;
    Property OnChange;
    Property OnGetItemParamValue: TPSCOnGetRecordSetParamValue read
      FOnGetItemParamValue write FOnGetItemParamValue;
  End;
{------------------------------------------------------------------------------}

Function PSCEditFltBld(FltBld: TPSCCustomFltBld): boolean;
Function PSCEditFltDlg(FltDlg: TPSCFltDlg): boolean;

Function PSCEqNullToIsNull(Const SQL: String): String;
Function PSCIsNullToEqNull(Const SQL: String): String;
Function PSCChangeIsNull(Const SQL: String; ChangeKind: TPSCChangeIsNullKind):
  String;
Function PSCIsSystemParam(ParamName: String): boolean;
function PSCChangeColorWithDlg(Color:TPSCColor):TPSCColor;
Function PSCGetFieldTemplCat(DataSet: TDataSet; Const FieldName: String;
  AUseLookupCategory:Boolean):String;
function PSCGetDefaultTemplates:TPSCFltBoxTemplates;
function PSCDBGridColumnsToCommaText(AGrid:TPSCDBGrid):String;
function PSCGetFltBoxRegister:IPSCFltBoxRegister;

Procedure PSCChangeIsNullInTemplates(Templates: TPSCFltBoxTemplates; ChangeKind:
  TPSCChangeIsNullKind);
Procedure PSCParamsAssignSysPrms(Params: TPSCFields; DestroyNulls: boolean;
  AFields:TPSCFields=nil);
procedure PSCChangeFontWithDlg(Font:TPSCFont);
procedure PSCEnumDBGridColumns(AGrid:TPSCDBGrid;const AStrings:IPSCStrings);
Procedure PSCAssignFields(ASource: TPSCNamedItems; ADest: TPSCFltBoxFields);
Procedure PSCFltTemplate_AdditionalFilter(AParams:TPSCFltTemplateProcData);
Procedure PSCFltTemplate_Like(AParams:TPSCFltTemplateProcData);
Procedure PSCFltTemplate_NotLike(AParams:TPSCFltTemplateProcData);
Procedure PSCFltTemplate_StringFieldE(AParams:TPSCFltTemplateProcData);
Procedure PSCFltTemplate_BooleanFieldE(AParams:TPSCFltTemplateProcData);
Procedure PSCFltTemplate_FieldIn(AParams:TPSCFltTemplateProcData);
Procedure PSCFltTemplate_FieldNotIn(AParams:TPSCFltTemplateProcData);

Procedure PSCRemoveTemplatesByID(ATemplates: TPSCFltBoxTemplates;
  const AID:Array of String);

{------------------------------------------------------------------------------}

const
  cPSCDefaultFltBldOptions:TPSCFltBldOptions=[fboDoubleQuoteInValue];

Var
  PSCModifyDefaultTemplProc: TPSCModifyDefaultTemplProc = Nil;

  CPSCUsedFltBldClass:TPSCFltBldClass=TPSCFltBld;
  CPSCUsedFltBoxClass:TPSCFltBoxClass=TPSCFltBox;
  CPSCUsedDlgFormClass:TPSCFltDlgFormClass=TPSCFltDlgForm;
  CPSCUsedTemplatesClass:TPSCFltBoxTemplatesClass=TPSCFltBoxTemplates;
  CPSCUsedTemplateClass:TPSCFltBoxTemplateClass=TPSCFltBoxTemplate;

  SPSCFltSaveFileName:String;

function PSCShowFilterDialog:boolean;

{------------------------------------------------------------------------------}

Implementation

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBox.PrepareLookupDataSet(ADataSet:TDataSet);
begin
  inherited;
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFieldLookupFilterSource(const AFieldName:String):TPSCCustomFltBld;
var
  MyField:TPSCFltBoxField;
begin
  MyField:=FieldParams.FieldByName(AFieldName);
  If MyField=nil then
    MyField:=Fields.FieldByName(AFieldName);
  If MyField<>nil then
    Result:=MyField.LookupFilterSource
  else
    Result:=nil;


end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBox.DefaultPickLookupParam(LookupDef: TPSCLookupDef);
begin
  inherited;
  If (PopupEdit is TPSCDBGridPopupEx) and (PopupItem is TPSCFltItem) and
    (FilterSource<>nil)
  then
    begin
      TPSCDBGridPopupEx(PopupEdit).FilterSource:=
        FilterSource.GetFieldLookupFilterSource(TPSCFltItem(PopupItem).DataField);

      If TPSCDBGridPopupEx(PopupEdit).FilterBox.Items.Count>0 then
        TPSCDBGridPopupEx(PopupEdit).FilterBox.ItemIndex:=0;
    end;
end;

{------------------------------------------------------------------------------}

function TPSCDBGridPopupEx.GetFilterSource:TPSCCustomFltBld;
begin
  Result:=FilterBox.FilterSource;
end;

{------------------------------------------------------------------------------}

procedure TPSCDBGridPopupEx.SetFilterSource(V:TPSCCustomFltBld);
begin
  FilterBox.FilterSource:=V;

  If FilterSource=nil then
    begin
      FilterBox.Visible:=False;
      Splitter.Visible:=False;
    end
  else
    begin
      FilterBox.Visible:=True;
      Splitter.Visible:=True;
    end;
end;

{------------------------------------------------------------------------------}

constructor TPSCDBGridPopupEx.CreateNew(AOwner: TComponent; Dummy: Integer=0);
begin
  inherited;
  FFilterBox := CPSCUsedFltBoxClass.Create(Self);
  With FilterBox do
  begin
    Align:=alBottom;
    Visible:=False;
    BorderStyle:=bsNone;
    Parent:=SubPanel;
  end;

  FSplitter:=TPSCSplitter.Create(Self);
  With Splitter do
  begin
    Align:=alBottom;
    Visible:=False;
    Parent:=SubPanel;
    Beveled:=True;
  end;

  Height:=Height*2;
end;

{------------------------------------------------------------------------------}

Procedure PSCRemoveTemplateByID(ATemplates: TPSCFltBoxTemplates;
  const AID:String);
var
  i:Integer;
begin
  for i:=ATemplates.Count-1 downto 0 do
    If PSCCompareText(ATemplates.Items[i].CaptionID,AID)=0 then
      ATemplates.Items[i].Free;
end;

{------------------------------------------------------------------------------}

Procedure PSCRemoveTemplatesByID(ATemplates: TPSCFltBoxTemplates;
  const AID:Array of String);
var
  i:Integer;
begin
  for i:=Low(AID) to High(AID) do
    PSCRemoveTemplateByID(ATemplates,AID[i]);
end;

{------------------------------------------------------------------------------}

//BeginSkipConst
Const
  cAssignPropNamesBoth: Array[0..3] Of String = (
    'MemorySlots', 'Items', 'OrderByItems','Filtered'
    );
//EndSkipConst

{------------------------------------------------------------------}

procedure TPSCFltDlg.SetFilterSource(V:TPSCCustomFltBld);
begin
  If FFilterSource<>V then
  begin
    If FFilterSource<>nil then
      FFilterSource.RemoveFreeNotification(Self);
    FFilterSource:=V;
    If FFilterSource<>nil then
      FFilterSource.FreeNotification(Self);
  end;
end;

{------------------------------------------------------------------}

procedure TPSCFltDlg.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  If (Operation=OpRemove) and (AComponent=FFilterSource) then
    FilterSource:=nil;
end;

{-------------------------------------------------------------------------}

Function PSCEditFltDlg(FltDlg: TPSCFltDlg): boolean;
Begin
  If Assigned(FltDlg) Then
    Result := FltDlg.Execute
  Else
    Result := False;
End;

{-------------------------------------------------------------------------}

Function PSCEditFltBld(FltBld: TPSCCustomFltBld): boolean;
Var
  FltDlg: TPSCFltDlg;
Begin
  FltDlg := TPSCFltDlg.Create(Nil);
  Try
    FltDlg.FilterSource := FltBld;
    Result := FltDlg.Execute;
  Finally
    FltDlg.Free;
  End;
End;

{-------------------------------------------------------------------------}

Constructor TPSCFltDlg.Create(AOwner: TComponent);
Begin
  Inherited;
End;

{-------------------------------------------------------------------------}

Function TPSCFltDlg.GetDlgFormClass: TPSCFltDlgFormClass;
Begin
  Result := CPSCUsedDlgFormClass;
End;

{-------------------------------------------------------------------------}

Procedure TPSCFltDlg.ApplyFilter(AFilterBox: TPSCCustomFltBox; ARealBld: TPSCCustomFltBld);
Begin
  PSCAssignProps(AFilterBox.FilterSource,ARealBld,cAssignPropNamesBoth);
End;

{-------------------------------------------------------------------------}

Procedure TPSCFltDlg.ApplyClick(Sender: TObject);
Begin
  With TPSCFltDlgForm(Sender) Do
    ApplyFilter(FltBox,FRealBld);
End;

{-------------------------------------------------------------------------}

Function TPSCFltDlg.Execute: boolean;
Var
  ParentForm: TCustomForm;
  R: TRect;
Begin
  Result:=FFilterSource<>nil;
  If not Result then
  begin
    PSCShowMessage(PSCConsts.ErrAssignFilterSource);
    exit;
  end;

  FDialog := GetDlgFormClass.CreateNew(nil);
  With FDialog Do
  Try
    FRealBld := Self.FFilterSource;

    If FCaption <> '' Then
      Caption := FCaption;

    If FFilterLabel <> '' Then
      FHeaderLabel.Caption := FFilterLabel;

    PSCAssignAllPropsExcludeStr(FRealBld,FFltBox.FilterSource,'OnChange');

    If FFltBox.Items.Count > 0 Then
      FFltBox.ItemIndex := 0;

    HelpContext := Self.HelpContext;

    If Self.HelpContext = 0 Then
    begin
      FHelpButton.Free;
      FHelpButton:=nil;
    end;

    if FHideApplyButton then
    begin
      FApplyButton.Free;
      FApplyButton:=nil;
    end;

    If Self.Owner Is TCustomForm Then
      ParentForm := TCustomForm(Self.Owner)
    Else
      ParentForm := Nil;

    If OwnerFormCenter And (ParentForm <> Nil) Then
      Begin
        FDialog.Position := poDesigned;
        R := PSCCenterRect(FDialog.BoundsRect,ParentForm.BoundsRect);
        FDialog.BoundsRect := PSCEnsureRectInWorkArea(R);
      End;

    OnApplyBtnClick := ApplyClick;
    if FApplyButton<>nil  then
      FApplyButton.Enabled := False;

    FDialog.UpdateBtnsLeft;

    With FDialog do
    begin
      FltBox.Anchors:=FltBox.Anchors+[akRight,akBottom];
      If OkButton<>nil then
        OkButton.Anchors:=[akRight,akBottom];
      If CancelButton<>nil then
        CancelButton.Anchors:=[akRight,akBottom];
      If HelpButton<>nil then
        HelpButton.Anchors:=[akRight,akBottom];
      If ApplyButton<>nil then
        ApplyButton.Anchors:=[akRight,akBottom];
      BorderStyle:=bsSizeable;
    end;

    If Assigned(FOnBeforeExecute) Then
      FOnBeforeExecute(Self);

    Result := FDialog.ShowModal = mrOk;
    If Result Then
      ApplyFilter(FDialog.FltBox,FDialog.FRealBld);

    If Assigned(FOnAfterExecute) Then
      FOnAfterExecute(Self);

  Finally
    FDialog.Free;
    FDialog:=nil;
  End;
End;

{-------------------------------------------------------------------------}

Type
  TPSCFltDlgFormButtons = (fbOkCancel,fbOkCancelApply,fbOkCancelHelp,
    fbOkCancelApplyHelp);

Procedure TPSCFltDlgForm.UpdateBtnsLeft;
Var
  MyButtons: TPSCFltDlgFormButtons;
Begin
  If (FApplyButton <> Nil) And (FHelpButton <> Nil) Then
    MyButtons := fbOkCancelApplyHelp
  Else
    If (FApplyButton <> Nil) Then
      MyButtons := fbOkCancelApply
    Else
      If (FHelpButton <> Nil) Then
        MyButtons := fbOkCancelHelp
      Else
        MyButtons := fbOkCancel;
  Case MyButtons Of
    fbOkCancel:
      Begin
        FOkButton.Left := FLeftApply;
        FCancelButton.Left := FLeftHelp;
      End;
    fbOkCancelApply:
      Begin
        FOkButton.Left := FLeftCancel;
        FCancelButton.Left := FLeftApply;
        FApplyButton.Left := FLeftHelp;
      End;
    fbOkCancelHelp:
      Begin
        FOkButton.Left := FLeftCancel;
        FCancelButton.Left := FLeftApply;
      End;
  End;
End;

{-------------------------------------------------------------------------}

function PSCShowFilterDialog:boolean;
var
  flt:TPSCFltDlg;
  fltBuilder:TPSCFltBld;
  fltField:TPSCFltBoxField;

begin
  flt:=TPSCFltDlg.Create(nil);
  flt.HideApplyButton:=true;
  fltBuilder:=TPSCFltBld.Create(flt);

   fltField:=TPSCFltBoxField(fltBuilder.Fields.Add);
  fltField.DataField:='WideStr Field';
  fltField.DataType:=FT_WIDESTR;

  fltField:=TPSCFltBoxField(fltBuilder.Fields.Add);
  fltField.DataField:='DateTime Field';
  fltField.DataType:=FT_DATETIME;

  fltField:=TPSCFltBoxField(fltBuilder.Fields.Add);
  fltField.DataField:='Str Field';
  fltField.DataType:=FT_STRING;

  flt.FilterSource:=fltBuilder;

  result:=flt.Execute;

  flt.Free;
end;

{-------------------------------------------------------------------------}

constructor TPSCFltDlgForm.CreateNew(AOwner: TComponent; Dummy: Integer=0);
var
  MySmallDistX:Integer;
  MySmallDistY:Integer;
  MyBigDistX:Integer;
  MyBigDistY:Integer;
Begin
  Inherited;

  MySmallDistX:=PSCDialogUnitsToPixelsX(4);
  MySmallDistY:=PSCDialogUnitsToPixelsY(4);
  MyBigDistX:=PSCDialogUnitsToPixelsX(7);
  MyBigDistY:=PSCDialogUnitsToPixelsX(7);

  ClientHeight := 250;
  ClientWidth := 400;

  Position := poScreenCenter;
  BorderIcons := [biSystemMenu];

  FHeaderLabel:= TPSCLabel.Create(Self);
  With FHeaderLabel do
  begin
    Left := MyBigDistX;
    Top := MyBigDistY;
    AutoSize:=True;
    Caption := PSCConsts.FilterLabel;
    Parent:=Self;
  end;

  FFltBld:= CPSCUsedFltBldClass.Create(Self);
  FFltBox:= CPSCUsedFltBoxClass.Create(Self);
  With FFltBox do
  begin
    Left := FHeaderLabel.Left;
    Width := Self.ClientWidth-Left*2;
    Top := FHeaderLabel.BoundsRect.Bottom+MySmallDistY;
    TabOrder := 0;
    FilterSource := FFltBld;
    OnChange := FilterBoxChange;
    Parent:=Self;
    BorderStyle:=bsNone;
    BevelInner:=bvNone;
    BevelOuter:=bvRaised;
    BevelKind:=bkFlat;
    Options:=Options-[fboCanOpen,fboCanSave];
    Options:=Options+[fboHideFindRecord,fboHideSortOrder];


  end;
  {
    fboSortFields,
    fboSortConditions,

    fboCanAdd,
    fboCanDelete,
    fboCanIndent,
    fboCanSaveToMem,
    fboCanLoadFromMem,

    fboLookupDSOpen,
    fboLookupDSClose,
    fboLookupDSRefresh,

    lboAlwaysShowSelection,

    ,
    fboHideDragLine,
    fboHideFindRecord,
    fboHideToggleFilter,
    fboHideUnderline,
    fboHideAutoUpdate,
    fboHideCalendar,
    fboHideIgnoreCase,

    fboDeleteSubItems,
    fboClearWithSortOrder,
    fboChecksInDateEdit,
    fboCheckReadOnly,
    fboAutoSizePopups

  }



  FHelpButton:= TPSCButton.Create(Self);
  With FHelpButton do
  begin
    Left := FFltBox.BoundsRect.Right-Width;
    Top := Self.ClientHeight-Height-MyBigDistY;
    Caption := PSCConsts.HelpButton;
    TabOrder := 3;
    Parent:=Self;
  end;

  FApplyButton:= TPSCButton.Create(Self);
  With FApplyButton do
  begin
    Left := FHelpButton.BoundsRect.Left-MySmallDistX-Width;
    Top := FHelpButton.Top;
    Caption := PSCConsts.ApplyButton;
    TabOrder := 2;
    OnClick := ApplyButtonClick;
    Parent:=Self;
  end;

  FCancelButton:= TPSCButton.Create(Self);
  With FCancelButton do
  begin
    Left := FApplyButton.BoundsRect.Left-MySmallDistX-Width;
    Top := FHelpButton.Top;
    Cancel := True;
    Caption := PSCConsts.CancelButton;
    ModalResult := 2;
    TabOrder := 1;
    Parent:=Self;
  end;

  FOKButton:= TPSCButton.Create(Self);
  With FOKButton do
  begin
    Left := FCancelButton.BoundsRect.Left-MySmallDistX-Width;
    Top := FHelpButton.Top;
    Caption := PSCConsts.OKButton;
    Default := True;
    ModalResult := 1;
    TabOrder := 0;
    Parent:=Self;
  end;

  With FFltBox do
    Height := OkButton.Top-MySmallDistY-Top;

  ActiveControl := FFltBox;
  Caption := PSCConsts.SpecifyFilter;
  PSCSetFormFont(Self);

  FLeftApply := FApplyButton.Left;
  FLeftHelp := FHelpButton.Left;
  FLeftCancel := FCancelButton.Left;

  Constraints.MinHeight:=200;
  Constraints.MinWidth:=OkButton.Width*5;

End;

{-------------------------------------------------------------------------}

Procedure TPSCFltDlgForm.ApplyButtonClick(Sender: TObject);
Begin
  FApplyButton.Enabled := False;
  If Assigned(FOnApplyBtnClick) Then
    FOnApplyBtnClick(Self);
End;

{-------------------------------------------------------------------------}

Procedure TPSCFltDlgForm.FilterBoxChange(Sender: TObject);
Begin
  If Assigned(FApplyButton) Then
    FApplyButton.Enabled := True;
End;

{------------------------------------------------------------------------------}

var
  FDefaultTemplates:TPSCFltBoxTemplates;

function PSCGetDefaultTemplates:TPSCFltBoxTemplates;
begin
  If FDefaultTemplates=nil then
  begin
    FDefaultTemplates:=CPSCUsedTemplatesClass.Create(nil,CPSCUsedTemplateClass);
    FDefaultTemplates.InitWithDefault;
  end;
  Result:=FDefaultTemplates;
end;

{------------------------------------------------------------------------------}

type
  TPSCFltBoxAction = Class(TPSCComponentAction)
  private
    FFilterBox: TPSCCustomFltBox;
    Procedure SetFilterBox(V: TPSCCustomFltBox);
    Function GetFilterBox(Target: TObject): TPSCCustomFltBox; 
  protected
    Procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetComponent(V:TComponent);override;
  public
    Function HandlesTarget(Target: TObject): Boolean; override;
  published
    Property FilterBox: TPSCCustomFltBox read FFilterBox write SetFilterBox;
  End;

  TPSCFltBoxActionSortAsc = Class(TPSCFltBoxAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

  TPSCFltBoxActionSortDesc = Class(TPSCFltBoxAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

  TPSCFltBoxActionToggleFiltered = Class(TPSCFltBoxAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

  TPSCFltBoxActionFindRecord = Class(TPSCFltBoxAction)
  protected
    function GetDirection:TPSCSearchType;virtual;
  public
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

  TPSCFltBoxActionFindNextRecord = Class(TPSCFltBoxActionFindRecord)
  protected
    function GetDirection:TPSCSearchType;override;
  public
    Constructor Create(AOwner: TComponent); override;
  End;

  TPSCFltBoxActionFindPriorRecord = Class(TPSCFltBoxActionFindRecord)
  protected
    function GetDirection:TPSCSearchType;override;
  public
    Constructor Create(AOwner: TComponent); override;
  End;

  TPSCFltBoxActionFindLastRecord = Class(TPSCFltBoxActionFindRecord)
  protected
    function GetDirection:TPSCSearchType;override;
  public
    Constructor Create(AOwner: TComponent); override;
  End;

  TPSCFltBoxActionFindFirstRecord = Class(TPSCFltBoxActionFindRecord)
  protected
    function GetDirection:TPSCSearchType;override;
  public
    Constructor Create(AOwner: TComponent); override;
  End;

  TPSCFltBoxActionSortClear = Class(TPSCFltBoxAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

  TPSCFltBoxActionSave = Class(TPSCFltBoxAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

  TPSCFltBoxActionOpen = Class(TPSCFltBoxAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

{---------------------------------------------------------------}

Constructor TPSCFltBoxActionSave.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := Integer(Image_SaveFilter);
  Caption := PSCConsts.ListPopupSave;
  ShortCut := PSCShortCutFromKeyDef(cPSCListBoxSaveKey);
end;

{---------------------------------------------------------------}

procedure TPSCCustomFltBld.LoadFromStream(AStream:TStream);
begin
  Items.BeginUpdate;
  try
    PSCReadObjFromTextStream(AStream,Items);
    UpdateDisplayValues(Items);
  finally
    Items.EndUpdate;
  end;
end;

{---------------------------------------------------------------}

procedure TPSCCustomFltBld.SaveToStream(AStream:TStream);
begin
  PSCWriteObjToTextStream(AStream,Items);
end;

{---------------------------------------------------------------}

procedure TPSCCustomFltBld.LoadFromFile(const AFileName:String);
Var
  FileStream: TFileStream;
Begin
  FileStream := TFileStream.Create(AFileName,fmOpenRead);
  Try
    LoadFromStream(FileStream);
  Finally
    FileStream.Free;
  End;
End;

{---------------------------------------------------------------}

procedure TPSCCustomFltBld.SaveToFile(const AFileName:String);
Var
  FileStream: TFileStream;
Begin
  FileStream := TFileStream.Create(AFileName,fmCreate);
  Try
    SaveToStream(FileStream);
  Finally
    FileStream.Free;
  End;
End;

{---------------------------------------------------------------}

procedure TPSCCustomFltBld.LoadFromFileDlg;
Var
  R: boolean;
Begin
  If SPSCFltSaveFileName='' then
    SPSCFltSaveFileName:=PSCConsts.Untitled;

  With TPSCOpenDialog.Create(Nil) Do
  Try
    Filter := PSCGetOpenDlgFilter(PSCConsts.FltSaveFileCat,SPSCFltSaveFileExt);
    Options := [ofHideReadOnly];
    FileName := SPSCFltSaveFileName;
    If (FileName = '') Or (Not FileExists(FileName)) Then
      FileName := '';
    R := Execute;
    If R Then
      Begin
        LoadFromFile(FileName);
        SPSCFltSaveFileName := FileName;
      End;
  Finally
    Free;
  End;
End;

{---------------------------------------------------------------}

procedure TPSCCustomFltBld.SaveToFileDlg;
Var
  S: String;
  R: boolean;
Begin
  If SPSCFltSaveFileName='' then
    SPSCFltSaveFileName:=PSCConsts.Untitled;

  With TPSCSaveDialog.Create(Nil) Do
  Try
    Filter := PSCGetOpenDlgFilter(PSCConsts.FltSaveFileCat,SPSCFltSaveFileExt);
    Options := [ofHideReadOnly,ofOverwritePrompt];
    FileName := SPSCFltSaveFileName;
    If FileName = '' Then
      Begin
        S := ExtractFilePath(FileName);
        If (S<>'') and PSCFolderExists(S) Then
          InitialDir := S
        Else
          InitialDir := GetCurrentDir;
        FileName := PSCAddSlash(InitialDir) + PSCConsts.Untitled + SPSCFltSaveFileExt;
      End;
    R := Execute;
    If R Then
      Begin
        S := ChangeFileExt(FileName,SPSCFltSaveFileExt);
        SaveToFile(S);
        SPSCFltSaveFileName := S;
      End;
  Finally
    Free;
  End;
End;

{---------------------------------------------------------------}

Procedure TPSCFltBoxActionSave.ExecuteTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
    if FilterSource<>nil then
      FilterSource.SaveToFileDlg;
end;

{---------------------------------------------------------------}

Procedure TPSCFltBoxActionSave.UpdateTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
  begin
    Self.Enabled := True;
    Self.Visible := (fboCanSave In GetRealOptions) and (FilterSource<>nil);
  end;
end;

{---------------------------------------------------------------}

Constructor TPSCFltBoxActionOpen.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := Integer(Image_OpenFilter);
  Caption := PSCConsts.ListPopupLoad;
  ShortCut := PSCShortCutFromKeyDef(cPSCListBoxLoadKey);
end;

{---------------------------------------------------------------}

Procedure TPSCFltBoxActionOpen.ExecuteTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
    if FilterSource<>nil then
      FilterSource.LoadFromFileDlg;
end;

{---------------------------------------------------------------}

Procedure TPSCFltBoxActionOpen.UpdateTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
  begin
    Self.Enabled := True;
    Self.Visible := (fboCanOpen In GetRealOptions) and (FilterSource<>nil);
  end;
end;

{---------------------------------------------------------------}

Procedure PSCAssignFields(ASource: TPSCNamedItems; ADest: TPSCFltBoxFields);
Var
  I: Integer;
Begin
  If ASource <> Nil Then
    Begin
      ADest.Clear;
      If ASource Is TPSCFltBoxFields Then
        ADest.Assign(ASource)
      Else
        With TPSCFields(ASource) Do
          For I := 0 To Count - 1 Do
            With TPSCFltBoxField(ADest.Add) Do
              Begin
                DataField := Items[I].Name;
                DataType := Items[I].DataType;
              End;
    End;
End;

{------------------------------------------------------------------}

function PSCChangeColorWithDlg(Color:TPSCColor):TPSCColor;
var
  ColorDialog:TPSCColorDialog;
begin
  Result:=Color;
  ColorDialog:=TPSCColorDialog.Create(nil);
  try
    ColorDialog.Color:=Color;
    If ColorDialog.Execute then
      Result:=ColorDialog.Color;
  finally
    ColorDialog.Free;
  end;
end;

{------------------------------------------------------------------}

procedure PSCChangeFontWithDlg(Font:TPSCFont);
var
  FontDialog:TPSCFontDialog;
begin
  FontDialog:=TPSCFontDialog.Create(nil);
  try
    FontDialog.Font:=Font;
    If FontDialog.Execute then
      Font.Assign(FontDialog.Font);
  finally
    FontDialog.Free;
  end;
end;

{-------------------------------------------------------------}

procedure TPSCCustomFltBox.ShowDebugDialog;
begin
  If FilterSource=nil then
    exit;
  With FilterSource do
  PSCShowMessage(
    'SQL:'#13#10+       //don't resource
    SQL.Text+#13#10#13#10+
    'FilterStr:'#13#10+ //don't resource
    FilterStr+#13#10#13#10
  );
end;

{-------------------------------------------------------------}

procedure TPSCCustomFltBox.InitDefaultKeyMapping;
begin
  inherited;
  Keys.AddSimpleKey(Word('X'), [ssCtrl,ssShift,ssAlt],ShowDebugDialog);
end;

{------------------------------------------------------------------}

Function TPSCCustomFltBox.GetItems: TPSCListItems;
begin
  If FilterSource=nil then
    Result:=inherited GetItems
  else
    Result:=FilterSource.Items;
end;

{------------------------------------------------------------------}

Function TPSCCustomFltBox.GetMemorySlots: TPSCListMemorySlots;
begin
  If FilterSource=nil then
    Result:=inherited GetMemorySlots
  else
    Result:=FilterSource.MemorySlots;
end;

{------------------------------------------------------------------}

const
  clInfoColor = $00CCFFFF;

procedure TPSCCustomFltBox.Paint;
var
  MySize:TSize;
  MyHelpRect:TRect;
  S:String;
  NewNormalPaint:Boolean;
Begin
  If Not HandleAllocated Then
    exit;

  If not (csDesigning in ComponentState) then
  begin
    inherited;
    exit;
  end;

  NewNormalPaint:=(FilterSource<>nil) and (FilterSource.GetFirstField<>'');

  If NewNormalPaint<>FNormalPaint then
  begin
    FNormalPaint:=NewNormalPaint;
    Invalidate;
    exit;
  end;

  If FNormalPaint then
    inherited
  else
    begin
      Canvas.Brush.Color := Color;
      Canvas.Font := Font;
      Canvas.Font.Size:=8;
      Canvas.Font.Style:=[FontStyle_Bold];
      MyHelpRect:=ClientRect;
      InflateRect(MyHelpRect,-10,-10);
      PSCFillRectExclude(Canvas,ClientRect,MyHelpRect);
      InflateRect(MyHelpRect,1,1);

      Canvas.Brush.Color := clInfoColor;
      Canvas.Pen.Color := clPSCBlack;
      Canvas.Pen.Style:=PenStyle_Solid;
      Canvas.Pen.Width:=1;
      Canvas.Rectangle(MyHelpRect);

      If FilterSource=nil then
        S:=PSCConsts.ErrAssignFilterSource
      else
        S:=PSCConsts.ErrConnectDataSet;
      MySize:=Canvas.TextExtent(S);

      PSCUpdateCanvasState(Canvas);

      InflateRect(MyHelpRect,-5,-5);
      PSCDrawText(Canvas,s,Length(s),MyHelpRect,
        DT_CENTER or DT_VCENTER or DT_WORDBREAK);
    end;
end;

{------------------------------------------------------------------}

function TPSCCustomFltBox.GetRightFixedWidth:Integer;
begin
  If fboHideSortOrder in GetRealOptions then
    Result:=0
  else
    Result:=16;
end;

{-------------------------------------------}

Function TPSCCustomFltBox.GetRealOptions:TPSCListBoxOptions;
begin
  Result:=inherited GetRealOptions {+ [fboChecksInDateEdit]};
end;

{-------------------------------------------}

Procedure TPSCCustomFltBox.DeleteAllItems;
begin
  inherited;
  If (FilterSource<>nil) and (fboClearWithSortOrder in GetRealOptions) then
    FilterSource.OrderByItems.Clear;
end;

{-------------------------------------------}

function TPSCCustomFltBox.IsItemOptionEnabled(
  AOption:TPSCListItemOption):Boolean;
begin
  Result:=False;

  If FilterSource=nil then
    exit;

  Case AOption of
    ListItem_IgnoreCase:
      begin
        Result:=IsIntemplateOptions(ftoCanIgnoreCase) and
          (not (fboHideIgnoreCase in GetRealOptions));
      end;
    ListItem_AutoUpdate:
      With TPSCFltItem(PopupItem) do
        Result:=(DataField<>SPSCAdditionalFilterField) and
          (not (fboHideAutoUpdate in GetRealOptions));
  end;
end;

{-------------------------------------------}

function TPSCCustomFltBox.IsIntemplateOptions(AElem:TPSCFltTemplateOption):Boolean;
var
  MyTemplItem: TPSCFltBoxTemplate;
begin
  Result:=False;
  If FilterSource=nil then
    exit;
  With TPSCFltItem(PopupItem) do
    MyTemplItem := FilterSource.Templates.ItemWithCaptionID(TemplCat,Template);
  If MyTemplItem = Nil Then
    exit;
  Result:=AElem in MyTemplItem.Options;
end;

{-------------------------------------------}

procedure TPSCCustomFltBox.ParamValueOnChange(Sender:TObject);
begin
  If FPopupEdit is TPSCPopupEdit then
    With TPSCPopupEdit(FPopupEdit) do
    begin
      If CheckBoxAutoUpdate.Checked then
        OnPopupEditClosed(nil,False);
    end;
end;

{-------------------------------------------}

procedure TPSCCustomFltBox.PickTextParam;
begin
  inherited;
  With TPSCPopupEdit(FPopupEdit) Do
  Begin
    ListBox:=Self;
    If not (fboHideFindRecord in GetRealOptions) then
    begin
      AddButton(TPSCFltBoxActionFindFirstRecord);
      AddButton(TPSCFltBoxActionFindPriorRecord);
      AddButton(TPSCFltBoxActionFindNextRecord);
      AddButton(TPSCFltBoxActionFindLastRecord);
    end;
  end;
end;

{-------------------------------------------}

Procedure TPSCFltBoxAction.SetFilterBox(V: TPSCCustomFltBox);
begin
  If V <> FFilterBox Then
    Begin
      FFilterBox := V;
      If V <> Nil Then
        V.FreeNotification(Self);
    End;
end;

{-------------------------------------------}

Function TPSCFltBoxAction.GetFilterBox(Target: TObject): TPSCCustomFltBox;
begin
  Result := (Target As TPSCCustomFltBox);
end;

{-------------------------------------------}

procedure TPSCFltBoxAction.SetComponent(V:TComponent);
begin
  FilterBox:=TPSCCustomFltBox(V);
end;

{-------------------------------------------}

Procedure TPSCFltBoxAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  Inherited Notification(AComponent,Operation);
  If (Operation = opRemove) And (AComponent = FilterBox) Then
    FilterBox := Nil;
end;

{-------------------------------------------}

Function TPSCFltBoxAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((FilterBox <> Nil) And (Target = FilterBox))
    Or (Target Is TPSCCustomFltBox);
end;

{------------------------------------------------------------------}

Type
  TMWinControl = Class(TWinControl)
  End;

{------------------------------------------------------------------------------}

Function TPSCCustomFltBld.IsAllowedUsageIDsStored: boolean;
Begin
  Result := AllowedUsageIDs <> cPSCDefAllowedUsageIDs;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomFltBld.GetDataSet: TDataSet;
Begin
  Result := FDataSet;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomFltBld.GetFilterOptions: TPSCFltBldOptions;
Begin
  Result := FFilterOptions;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetFilterOptions(V: TPSCFltBldOptions);
Begin
  If FFilterOptions <> V Then
    Begin
      FFilterOptions := V;
      UpdateDataSets;
    End;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetMemorySlots: TPSCListMemorySlots;
Begin
  Result := FMemorySlots;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetMemorySlots(V: TPSCListMemorySlots);
Begin
  MemorySlots.Assign(V);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.Assign(Source: TPersistent);
Begin
  If Source Is TPSCCustomFltBld Then
    PSCAssignAllProps(Source,Self)
  else
    Inherited;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBox.Assign(Source: TPersistent);
Begin
  If Source Is TPSCCustomFltBox Then
    PSCAssignAllProps(Source,Self)
  Else
    Inherited;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.UpdateDisplayValues(AItems:TPSCFltItems);
var
  i:Integer;
begin
  AItems.BeginUpdate;
  try
    for i:=0 to AItems.Count-1 do
      UpdateDisplayValues(AItems[i]);
  finally
    AItems.EndUpdate;
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.UpdateDisplayValues(AItem:TPSCFltItem);

  procedure MyUpdate(const AParamName:String);
  var
    MyField:TPSCListParam;
  begin
    MyField:=TPSCListParam(AItem.Params.ItemByName(AParamName));
    If MyField<>nil then
      UpdateParamDisplayValue(AItem,MyField);
  end;

begin
  MyUpdate(SPSCValue);
  MyUpdate(SPSCValue1);
  MyUpdate(SPSCValue2);
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.UpdateParamDisplayValue(AItem:TPSCFltItem;
  AParam:TPSCListParam);
var
  MyLookupDef:TPSCLookupDef;
  MyPickList:IPSCStrings;
begin
  MyPickList:=PSCCreateStringList;
  GetParamPickList(AItem,AParam,MyPickList);
  MyLookupDef := GetParamLookupDef(AItem,AParam);
  AParam.UpdateDisplayValue(MyLookupDef,MyPickList);
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.OnUpdate(Sender: TObject; Item: TPSCNamedItem);
Begin
  Inherited;
  Changed;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBox.OnPopupTemplateClosed(Sender: TObject; Canceled: boolean);
Var
  Idx: Integer;
  Temp: IPSCStrings;
  ListBox: TPSCListBox;
Begin
  If (Not Canceled) and (FilterSource<>nil) Then
    With TPSCFltItem(PopupItem) Do
      Begin
        ListBox := TPSCPopupListBox(PopupEdit).ListBox;
        idx := TPSCValuesContainer(ListBox.Items[ListBox.ItemIndex].DataObject).IntValue;

        Temp := PSCCreateStringList;
        FilterSource.GetFieldTemplates(DataField,Temp,False);
        Template := Temp[idx];
      End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomFltBox.OnPopupFieldClosed(Sender: TObject; Canceled: boolean);
Var
  Temp: IPSCStrings;
  idx: Integer;
  ListBox: TPSCListBox;
  P: TPSCField;
Begin
  If (Not Canceled) and (FilterSource<>nil) Then
    Begin
      Temp := PSCCreateStringList;

      FilterSource.GetFieldsList(Temp);
      ListBox := TPSCPopupListBox(PopupEdit).ListBox;
      idx := TPSCValuesContainer(ListBox.Items[ListBox.ItemIndex].DataObject).IntValue;

      With TPSCFltItem(PopupItem) Do
        Begin
          Collection.BeginUpdate;
          Try
            P := TPSCField(Params.ItemByName(SPSCValue));
            If P <> Nil Then
              P.DataType := P.DataType;
            P := TPSCField(Params.ItemByName(SPSCValue1));
            If P <> Nil Then
              P.DataType := P.DataType;
            DataField := Temp[idx];
          Finally
            Collection.EndUpdate;
          End;
        End;
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomFltBox.PickFieldParam;
Begin
  With TPSCFltItem(PopupItem) Do
    If FilterSource<>nil then
    Begin
      PickStrings(Nil,FilterSource.GetDisplayFieldsList,
        FilterSource.GetFieldDisplayValue(DataField),
        OnPopupFieldClosed,fboSortFields In Self.Options,Point(0,0));
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomFltBox.PickTemplateParam;
Var
  Temp: IPSCStrings;
  TemplItem: TPSCFltBoxTemplate;
  ATemplate: String;
Begin
  With TPSCFltItem(PopupItem) Do
    If FilterSource<>nil then
    Begin
      Temp := PSCCreateStringList;
      ATemplate := Template;

      With GetFltBld Do
        TemplItem :=
          Templates.ItemWithCaptionID(GetTemplCategory(DataField),ATemplate);

      If TemplItem <> Nil Then
        ATemplate := TemplItem.CaptionForUser;

      FilterSource.GetFieldTemplates(DataField,Temp,True);

      If Temp.Count<=1 then
        exit;

      PickStrings(Temp,Nil,ATemplate,OnPopupTemplateClosed,
        fboSortConditions In Self.Options,Point(0,0));
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomFltBox.PickParameter;
Var
  Processed: Boolean;
Begin
  If PopupItem.IsParamReadOnly(PopupParam) Then
    exit;

  If Assigned(OnPickParameter) Then
    Begin
      Processed := False;
      OnPickParameter(Self,PopupItem,PopupParam,PopupParamRect,Processed);
      If Processed Then
        exit;
    End;

  If PSCISPrmName(PopupParam,SPSCOpParam) Then
    PickOpParam
  Else
    If PSCISPrmName(PopupParam,SPSCFieldParam) Then
      PickFieldParam
    Else
      If PSCISPrmName(PopupParam,SPSCTemplateParam) Then
        PickTemplateParam
      Else
        begin
          ShowPopupParameter;
        end;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetAddTableNames(V: boolean);
Begin
  If V <> FAddtableNames Then
    Begin
      FAddTableNames := V;
      FilterStrChanged := True;
      Changed;
    End;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetTableNames(V: TStrings);
Begin
  FTableNames.Assign(V);
End;

{------------------------------------------------------------------}

procedure TPSCCustomFltBox.SetFilterSource(V:TPSCCustomFltBld);
begin
  If FFilterSource<>V then
  begin
    If FFilterSource<>nil then
    begin
      FFilterSource.RemoveFreeNotification(Self);
      FFilterSource.Events.UnregisterHandler(FEventsHandler);
    end;
    FFilterSource:=V;
    If FFilterSource<>nil then
    begin
      FFilterSource.FreeNotification(Self);
      FFilterSource.Events.RegisterHandler(FEventsHandler);
    end;
    Invalidate;
  end;
end;

{------------------------------------------------------------------}

procedure TPSCCustomFltBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  If (Operation=OpRemove) and (AComponent=FFilterSource) then
    FilterSource:=nil;
end;

{------------------------------------------------------------------------------}

Procedure TPSCCustomFltBox.HandleEvent(const AParams:TPSCEventParams);
begin
  inherited;
  OnUpdate(Self,nil);
end;

{------------------------------------------------------------------------------}

Constructor TPSCCustomFltBox.Create(AOwner: TComponent);
Begin
  Inherited;

  If CPSCUsedPopupLookupClass=TPSCDBGridPopup then
    CPSCUsedPopupLookupClass:=TPSCDBGridPopupEx;

  AddItemText := PSCConsts.ClickHereToAddCond;
  ShowAddItem := True;
  EditWithMouse := True;
  ShowHint:=True;
  CheckBoxes:=True;
  FEventsHandler:=PSCCreateEventHandler(HandleEvent);
  ShowDefaultPopup:=True;
  AutoComplete:=False;
  Options:=cPSCFltBoxDefaultOptions;
End;

{------------------------------------------------------------------------------}

Destructor TPSCCustomFltBox.Destroy;
Begin
  FilterSource:=nil;
  Inherited;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBox.AddItem(ACheckCanAdd:Boolean):TPSCListItem;
Var
  ATemplate,ADataField: String;
Begin
  Result:=nil;
  If ((not CanAddItem) and ACheckCanAdd) or (FilterSource=nil) Then
    exit;
  ADataField := FilterSource.GetFirstUnusedField;
  If ADataField = '' Then
    exit;
  ATemplate := FilterSource.GetFirstTemplate(ADataField,False);
  If ATemplate = '' Then
    exit;

  Items.BeginUpdate;
  Try
    Result := TPSCFltItem(Items.Add);
    With TPSCFltItem(Result) Do
      Begin
        Checked := NewItemChecked;
        DataField := ADataField;
        Template := ATemplate;
        Self.ItemIndex := Index;
      End;
    ItemAdded(Result);
  Finally
    Items.EndUpdate;
  End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomFltBox.ShowValueListParameter;
begin
  FPopupEdit.Free;
  FPopupEdit:=nil;

  If FilterSource=nil then
    exit;

  FPopupEdit := CPSCUsedPopupValueListClass.CreateNew(Nil,0);
  With TPSCValueListPopup(FPopupEdit) Do
    Begin
      With ListBox do
      begin
        PopupParams            := Self.PopupParams          ;
        DisplayDateTimeFormat  := Self.DisplayDateTimeFormat;
        HoverLinkColor         := Self.HoverLinkColor       ;
        LinkColor              := Self.LinkColor            ;
        HideHoverLink          := Self.HideHoverLink        ;
        ClickHereColor         := Self.ClickHereColor       ;
        HideSelected           := Self.HideSelected         ;
        SelColor               := Self.SelColor             ;
        SelFontColor           := Self.SelFontColor         ;
        //OnPickParameter        := Self.OnPickParameter      ;
        //OnPickLookupParam      := Self.OnPickLookupParam    ;

        OnEditKeyPress:=Self.OnEditKeyPress;
        OnUpdatePopup:=Self.OnUpdatePopup;
        CheckBoxes:=False;
        If fboLookupDSOpen in Self.Options then
          Options:=Options+[fboLookupDSOpen];
        If fboLookupDSClose in Self.Options then
          Options:=Options+[fboLookupDSClose];
        If fboLookupDSRefresh in Self.Options then
          Options:=Options+[fboLookupDSRefresh];

        HideHoverLink:=True;
      end;

      DataType:=Self.PopupParam.DataType;
      GetParamPickList(PopupItem,TPSCListParam(PopupParam),PickList);
      LookupDef:=GetParamLookupDef(PopupItem,TPSCListParam(PopupParam));
      PickType:=GetParamPickType(PopupItem,PopupParam);

      SetValueList(PopupParam.Value);
      Self.UpdatePopupParams(FPopupEdit);
      Popup(Self,PopupParamRect,OnPopupValueListClosed);
    End;
end;

{------------------------------------------------------------------------------}

Procedure TPSCCustomFltBox.OnPopupValueListClosed(Sender: TObject; Canceled: boolean);
var
  k,i:Integer;
  MyCount:Integer;
  MyField:TPSCField;
  MyVariant:Variant;
begin
  If Canceled then
    exit;
  With TPSCValueListPopup(FPopupEdit),ListBox Do
    Begin
      Try
        If Items.Count=0 then
          Self.PopupParam.Clear
        else
          begin
            MyCount:=0;
            for i:=0 to Items.Count-1 do
              With Items[i].Params do
              begin
                MyField:=TPSCField(ItemByName(SPSCValue));
                If not MyField.IsNull then
                  inc(MyCount);
              end;
            If MyCount=0 then
              Self.PopupParam.Clear
            else
              begin
                MyVariant:=VarArrayCreate([0,MyCount-1],PSCFieldTypeToVarType(DataType));
                k:=0;
                for i:=0 to Items.Count-1 do
                  With Items[i].Params do
                  begin
                    MyField:=TPSCField(ItemByName(SPSCValue));
                    If not MyField.IsNull then
                    begin
                      MyVariant[k]:=MyField.Value;
                      inc(k);
                    end;
                  end;
                TPSCFltItem(Self.PopupItem).Params.BeginUpdate;
                try
                  Self.PopupParam.Value:=MyVariant;
                  Self.PopupParam.SafeSetDataType(DataType);
                  FilterSource.UpdateParamDisplayValue(TPSCFltItem(Self.PopupItem),Self.PopupParam);
                finally
                  TPSCFltItem(Self.PopupItem).Params.EndUpdate;
                end;
              end;
          end;
      Except
          Self.PopupParam.Clear;
      end;
    end;
end;

{------------------------------------------------------------------------------}

Procedure TPSCCustomFltBox.ShowPopupParameter;
begin
  If IsIntemplateOptions(ftoValueList) then
    ShowValueListParameter
  else
    inherited;
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBox.OnPopupOpClosed(Sender: TObject; Canceled: boolean);
var
  S:String;
  MyStrings:IPSCStrings;
  i:Integer;
Begin
  If (Not Canceled) and (FilterSource<>nil) Then
    With FilterSource do
    Begin
      With TPSCPopupListBox(PopupEdit).ListBox do
        S:=Items[ItemIndex].Caption;
      MyStrings:=PSCCreateStringList;
      GetOperationsForDisplay(MyStrings);
      i:=MyStrings.IndexOf(S);
      GetOperations(MyStrings);
      S:=MyStrings[i];
      TPSCFltItem(PopupItem).Operation := S;
    End;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBox.PickOpParam;
Begin
  If FilterSource<>nil then
    PickStrings(Nil,FilterSource.GetOperationsForDisplay,
      FilterSource.FltToDispOperation(PopupParam.AsString),
      OnPopupOpClosed,True,Point(0,0));
End;

{------------------------------------------------------------------------------}

Function PSCGetTodayP2: TDateTime;
Begin
  Result := PSCDateOf(PSCIncDay(PSCDateOf(PSCNow),2));
End;

{------------------------------------------------------------------------------}

Function PSCGetNextWeek8: TDateTime;
Begin
  Result := PSCGetWeekStart(PSCIncWeek(PSCDateOf(PSCNow),2));
End;

{------------------------------------------------------------------------------}

Function PSCGetNextMon32: TDateTime;
Begin
  Result := PSCGetMonthStart(PSCIncMonth(PSCDateOf(PSCNow),2));
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetValuePlusOne(AParams:TPSCFltSysParamProcData);
var
  MyField:TPSCField;
begin
  With AParams do
  begin
    MyField:=TPSCField(Fields.ItemByName(SPSCValue));
    If MyField=nil then
      MyField:=TPSCField(Fields.ItemByName(SPSCValue1));
    If MyField=nil then
      PSCError(PSCConsts.ErrBadFilter)
    else
      begin
        DataType:=MyField.DataType;
        Result:=MyField.Value+1;
      end;
  end;
end;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetYesterday(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.result := PSCGetYesterday;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetTomorrow(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetTomorrow;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetTodayM7(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetTodayM7;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetToday(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetToday;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetTodayP7(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetTodayP7;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetTodayP8(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetTodayP8;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetTodayM8(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetTodayM8;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetTodayP2(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetTodayP2;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetNextWeek8(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetNextWeek8;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetLastWeek1(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetLastWeek1;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetLastWeek7(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetLastWeek7;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetThisWeek1(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetThisWeek1;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetThisWeek7(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetThisWeek7;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetNextWeek1(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetNextWeek1;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetNextWeek7(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetNextWeek7;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetLastMon1(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetLastMon1;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetLastMon31(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetLastMon31;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetThisMon1(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetThisMon1;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetThisMon31(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetThisMon31;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetNextMon1(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetNextMon1;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetNextMon31(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetNextMon31;
End;

{------------------------------------------------------------------------------}

procedure PSCFltSysParam_GetNextMon32(AParams:TPSCFltSysParamProcData);
Begin
  AParams.DataType := FT_DATE;
  AParams.Result := PSCGetNextMon32;
End;

{------------------------------------------------------------------------------}

Function PSCChangeToUpperFuncName(const S:String;
  AFltBld:TPSCCustomFltBld):String;
var
  MyUpper:String;
begin
  If fboUseUCASEioUPPER in AFltBld.GetRealFilterOptions then
    MyUpper:=SPSCFltUpper_UCASE
  else
    MyUpper:=SPSCFltUpper_UPPER;
  Result:=PSCFormat(S,[MyUpper,MyUpper]);
end;

{------------------------------------------------------------------------------}

const
  SPSCFlt_FieldLikeUpper_Oracle='%S(RTRIM(:Field)) LIKE %S(:Value)'; //don't resource
  SPSCFlt_FieldLike_Oracle='RTRIM(:Field) LIKE :Value';//don't resource

procedure PSCFltTemplate_Like(AParams:TPSCFltTemplateProcData);
//var
//  MyField:TPSCField;
begin
  With AParams do
  begin
//    MyField:=TPSCField(Item.Params.ItemByName(SPSCValue));
    If fboUseRTRIM in FilterBuilder.GetRealFilterOptions then
      begin
        If ListItem_IgnoreCase in Item.Options then
          Result:=PSCChangeToUpperFuncName(SPSCFlt_FieldLikeUpper_Oracle,FilterBuilder)
        else
          Result:=SPSCFlt_FieldLike_Oracle;
      end
    else
      If ListItem_IgnoreCase in Item.Options then
        Result:=PSCChangeToUpperFuncName(SPSCFlt_FieldLikeUpper,FilterBuilder)
      else
        Result:=SPSCFlt_FieldLike;
  end;
end;

{------------------------------------------------------------------------------}

procedure PSCFltTemplate_NotLike(AParams:TPSCFltTemplateProcData);
begin
  PSCFltTemplate_Like(AParams);
  AParams.Result:='NOT ('+AParams.Result+')';//don't resource
end;

{------------------------------------------------------------------------------}

procedure PSCFltTemplate_StringFieldE(AParams:TPSCFltTemplateProcData);
//var
//  MyField:TPSCField;
begin
  With AParams do
  begin
//    MyField:=TPSCField(Item.Params.ItemByName(SPSCValue));
    If ListItem_IgnoreCase in Item.Options then
      Result:=PSCChangeToUpperFuncName(SPSCFlt_StringFieldEUpper,FilterBuilder)
    else
      Result:=SPSCFlt_FieldE;
  end;
end;

{------------------------------------------------------------------------------}

procedure PSCFltTemplate_StringFieldNE(AParams:TPSCFltTemplateProcData);
begin
  PSCFltTemplate_StringFieldE(AParams);
  AParams.Result:='NOT ('+AParams.Result+')';//don't resource
end;

{------------------------------------------------------------------------------}

procedure PSCFltTemplate_FieldIn(AParams:TPSCFltTemplateProcData);
var
  MyField:TPSCField;
begin
  With AParams do
  begin
    MyField:=TPSCField(Item.Params.ItemByName(SPSCValue));
    If MyField.IsArray then
      Result:=':Field %S :Value' //don't resource
    else
      Result:=':Field %S (:Value)'; //don't resource

    If fboUseDollarioIN in FilterBuilder.GetRealFilterOptions then
      Result:=PSCFormat(Result,['$'])
    else
      Result:=PSCFormat(Result,['IN']);//don't resource
  end;
end;

{------------------------------------------------------------------------------}

procedure PSCFltTemplate_FieldIsNull(AParams:TPSCFltTemplateProcData);
begin
  With AParams do
  begin
    If fboUseEMPTYioNULL in FilterBuilder.GetRealFilterOptions then
      Result:='EMPTY(:Field)'
    else
      Result:=SPSCFlt_FieldIsNull;
  end;
end;

{------------------------------------------------------------------------------}

procedure PSCFltTemplate_BlankOrEmpty(AParams:TPSCFltTemplateProcData);
begin
  PSCFltTemplate_FieldIsNull(AParams);
  With AParams do
  begin
    Result:='((:Field = '''') or ('+Result+'))';
  end;
end;

{------------------------------------------------------------------------------}

procedure PSCFltTemplate_FieldIsNotNull(AParams:TPSCFltTemplateProcData);
begin
  With AParams do
  begin
    If fboUseEMPTYioNULL in FilterBuilder.GetRealFilterOptions then
      Result:='NOT EMPTY(:Field)'
    else
      Result:=SPSCFlt_FieldIsNotNull;
  end;
end;

{------------------------------------------------------------------------------}

procedure PSCFltTemplate_FieldNotIn(AParams:TPSCFltTemplateProcData);
begin
  PSCFltTemplate_FieldIn(AParams);
  With AParams do
    Result:='NOT ('+Result+')';//don't resource
end;

{------------------------------------------------------------------------------}

procedure PSCFltTemplate_AdditionalFilter(AParams:TPSCFltTemplateProcData);
var
  MyField:TPSCField;
begin
  With AParams do
  begin
    MyField:=TPSCField(Item.Params.ItemByName(SPSCValue));
    If MyField<>nil then
      Result:=MyField.AsString
    else
      Result:='';
  end;
end;

{------------------------------------------------------------------------------}

procedure PSCFltTemplate_BooleanFieldE(AParams:TPSCFltTemplateProcData);
var
  MyValue:TPSCField;
begin
  With AParams do
    If fboSimpleBoolConditions in FilterBuilder.GetRealFilterOptions then
      begin
        MyValue:=TPSCField(Item.Params.ItemByName(SPSCValue));
        If (MyValue<>nil) then
        begin
          If MyValue.IsNull then
            Result:=SPSCFlt_FieldE
          else
          If MyValue.AsBoolean then
            Result:=SPSCFieldParam
          else
            Result:='NOT '+SPSCFieldParam;
        end;
      end
    else
      Result:=SPSCFlt_FieldE;
end;


{------------------------------------------------------------------------------}

Var
  RegisterDefSysParamsWasCalled: boolean;

Procedure RegisterDefSysParams;
Begin
  If RegisterDefSysParamsWasCalled Then
    exit;

  RegisterDefSysParamsWasCalled := True;

  With PSCGetFltBoxRegister do
  begin
    RegisterTemplateProc(SPSCFlt_AdditionalFilterIs_Proc,PSCFltTemplate_AdditionalFilter);
    RegisterTemplateProc(SPSCFlt_FieldLike_Proc,PSCFltTemplate_Like);
    RegisterTemplateProc(SPSCFlt_FieldNotLike_Proc,PSCFltTemplate_NotLike);
    RegisterTemplateProc(SPSCFlt_BlankOrEmpty_Proc,PSCFltTemplate_BlankOrEmpty);
    RegisterTemplateProc(SPSCFlt_StringFieldE_Proc,PSCFltTemplate_StringFieldE);
    RegisterTemplateProc(SPSCFlt_StringFieldNE_Proc,PSCFltTemplate_StringFieldNE);
    RegisterTemplateProc(SPSCFlt_BooleanFieldE_Proc,PSCFltTemplate_BooleanFieldE);
    RegisterTemplateProc(SPSCFlt_FieldIn_Proc,PSCFltTemplate_FieldIn);
    RegisterTemplateProc(SPSCFlt_FieldNotIn_Proc,PSCFltTemplate_FieldNotIn);

    RegisterTemplateProc(SPSCFlt_FieldIsNull_Proc,PSCFltTemplate_FieldIsNull);
    RegisterTemplateProc(SPSCFlt_FieldIsNotNull_Proc,PSCFltTemplate_FieldIsNotNull);

    RegisterSysParam(SPSCFltVar_ValuePlusOne,PSCFltSysParam_GetValuePlusOne);

    RegisterSysParam(SPSCFltVar_Yesterday,PSCFltSysParam_GetYesterday);
    RegisterSysParam(SPSCFltVar_Tomorrow,PSCFltSysParam_GetTomorrow);
    RegisterSysParam(SPSCFltVar_TodayM7,PSCFltSysParam_GetTodayM7);
    RegisterSysParam(SPSCFltVar_Today,PSCFltSysParam_GetToday);
    RegisterSysParam(SPSCFltVar_TodayP7,PSCFltSysParam_GetTodayP7);

    RegisterSysParam(SPSCFltVar_TodayP8,PSCFltSysParam_GetTodayP8);
    RegisterSysParam(SPSCFltVar_TodayM8,PSCFltSysParam_GetTodayM8);
    RegisterSysParam(SPSCFltVar_TodayP2,PSCFltSysParam_GetTodayP2);

    RegisterSysParam(SPSCFltVar_NextWeek8,PSCFltSysParam_GetNextWeek8);

    RegisterSysParam(SPSCFltVar_LastWeek1,PSCFltSysParam_GetLastWeek1);
    RegisterSysParam(SPSCFltVar_LastWeek7,PSCFltSysParam_GetLastWeek7);
    RegisterSysParam(SPSCFltVar_ThisWeek1,PSCFltSysParam_GetThisWeek1);
    RegisterSysParam(SPSCFltVar_ThisWeek7,PSCFltSysParam_GetThisWeek7);
    RegisterSysParam(SPSCFltVar_NextWeek1,PSCFltSysParam_GetNextWeek1);
    RegisterSysParam(SPSCFltVar_NextWeek7,PSCFltSysParam_GetNextWeek7);
    RegisterSysParam(SPSCFltVar_LastMon1,PSCFltSysParam_GetLastMon1);
    RegisterSysParam(SPSCFltVar_LastMon31,PSCFltSysParam_GetLastMon31);
    RegisterSysParam(SPSCFltVar_ThisMon1,PSCFltSysParam_GetThisMon1);
    RegisterSysParam(SPSCFltVar_ThisMon31,PSCFltSysParam_GetThisMon31);
    RegisterSysParam(SPSCFltVar_NextMon1,PSCFltSysParam_GetNextMon1);
    RegisterSysParam(SPSCFltVar_NextMon31,PSCFltSysParam_GetNextMon31);
    RegisterSysParam(SPSCFltVar_NextMon32,PSCFltSysParam_GetNextMon32);
  end;
End;

{------------------------------------------------------------------------------}

type
  TPSCSysParamData=class
  public
    Proc:TPSCFltSysParamProc;
  end;

  TPSCTemplateProcData=class
  public
    Proc:TPSCFltTemplateProc;
  end;

  TPSCFltBoxRegister=class(TInterfacedObject,IPSCFltBoxRegister)
  private
    FSysParams:IPSCStrings;
    FTemplateProcs:IPSCStrings;
  public
    procedure RegisterSysParam(Const AName: String;AProc:TPSCFltSysParamProc);
    Procedure RegisterTemplateProc(Const AName: String;AProc:TPSCFltTemplateProc);
    function GetSysParam(const AName:String):TPSCFltSysParamProc;
    Function GetTemplateProc(const AName:String):TPSCFltTemplateProc;
    constructor Create;            
  end;

{------------------------------------------------------------------------------}

constructor TPSCFltBoxRegister.Create;
begin
  inherited;
  FSysParams:=PSCCreateSortedStringList(ioOwned);
  FTemplateProcs:=PSCCreateSortedStringList(ioOwned);
end;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxRegister.RegisterSysParam(Const AName: String;AProc:TPSCFltSysParamProc);
var
  MyIndex:Integer;
  MyItem:TPSCSysParamData;
begin
  If FSysParams.Find(AName,MyIndex) then
    MyItem:=TPSCSysParamData(FSysParams.Objects[MyIndex])
  else
    begin
      MyItem:=TPSCSysParamData.Create;
      FSysParams.AddObject(AName,MyItem);
    end;
  MyItem.Proc:=AProc;  
end;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxRegister.RegisterTemplateProc(Const AName: String;AProc:TPSCFltTemplateProc);
var
  MyIndex:Integer;
  MyItem:TPSCTemplateProcData;
begin
  If FTemplateProcs.Find(AName,MyIndex) then
    MyItem:=TPSCTemplateProcData(FTemplateProcs.Objects[MyIndex])
  else
    begin
      MyItem:=TPSCTemplateProcData.Create;
      FTemplateProcs.AddObject(AName,MyItem);
    end;
  MyItem.Proc:=AProc;
end;

{------------------------------------------------------------------------------}

function TPSCFltBoxRegister.GetSysParam(const AName:String):TPSCFltSysParamProc;
var
  MyIndex:Integer;
begin
  If FSysParams.Find(AName,MyIndex) then
    Result:=TPSCSysParamData(FSysParams.Objects[MyIndex]).Proc
  else
    Result:=nil;
end;

{------------------------------------------------------------------------------}

Function TPSCFltBoxRegister.GetTemplateProc(const AName:String):TPSCFltTemplateProc;
var
  MyIndex:Integer;
begin
  If FTemplateProcs.Find(AName,MyIndex) then
    Result:=TPSCTemplateProcData(FTemplateProcs.Objects[MyIndex]).Proc
  else
    Result:=nil;
end;

{------------------------------------------------------------------------------}

var
  FFltBoxRegister:IPSCFltBoxRegister;

function PSCGetFltBoxRegister:IPSCFltBoxRegister;
begin
  If FFltBoxRegister=nil then
  begin
    FFltBoxRegister:=TPSCFltBoxRegister.Create;
    RegisterDefSysParams;
  end;
  Result:=FFltBoxRegister;
end;

{------------------------------------------------------------------------------}

Function PSCGetDefTemplCatByDataType(DataType: TPSCFieldType): String;
Begin
  Result := SPSCTemplCatUnknown;
  Case DataType Of
    FT_STRING:
      Result := SPSCTemplCatText;
    FT_FLOAT,FT_CURRENCY,FT_INT:
      Result := SPSCTemplCatNumber;
    FT_BOOL:
      Result := SPSCTemplCatBoolean;
    FT_DATE:
      Result := SPSCTemplCatDate;
    FT_DATETIME:
      Result := SPSCTemplCatDateTime;
    FT_TIME:
      Result := SPSCTemplCatTime;
    FT_MEMO:
      Result := SPSCTemplCatMemo;
  End;
End;

{------------------------------------------------------------------------------}

Function PSCGetFieldTemplCat(DataSet: TDataSet; Const FieldName: String;
  AUseLookupCategory:Boolean):String;
Var
  Field: TField;
Begin
  Result := SPSCTemplCatUnknown;
  If DataSet = Nil Then
    exit;
  Field := DataSet.FindField(FieldName);
  If Field = Nil Then
    exit;
  If Field.FieldKind = fkLookup Then
    begin
      If AUseLookupCategory then
        Result := SPSCTemplCatLookup
      else
        Result := PSCGetFieldTemplCat(Field.LookupDataSet,Field.LookupKeyFields,False)
    end
  Else
    Result := PSCGetDefTemplCatByDataType(PSCDBFieldTypeToPSC(Field.DataType));
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetOrderByText: String;
Var
  i: Integer;
  MyFieldNumber:Integer;
Begin
  Try

    If OrderByItems.Count = 0 Then
      Begin
        Result := '';
        exit;
      End;
    Result := 'ORDER BY'#13#10; //don't resource
    With OrderByItems Do
      For i := 0 To Count - 1 Do
        With TPSCOrderByItem(Items[i]) Do
          Begin
            If i > 0 Then
              Result := Result + ', ';

            MyFieldNumber:=GetFieldNumber(DataField);

            If (FOrderByStyle=obsFieldNames) or (MyFieldNumber<0)then
              Result := Result + GetPreparedFieldName(DataField,True)
            else
              Result := Result + PSCIntToStr(MyFieldNumber);

            If Descending Then
              Result := Result + ' DESC'; //don't resource
          End;

    If Assigned(FOnPrepareOrderByText) then
      FOnPrepareOrderByText(Self,Result);
  Finally
    If FOldOrderByText <> Result Then
      Begin
        FOrderByChanged := True;
        FOldOrderByText := Result;
      End;
  End;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.ReallyAddTableNames: boolean;
Begin
  Result := (AddTableNames) Or (TableNames.Count > 1);
End;

{------------------------------------------------------------------------------}

Procedure TPSCOrderByItem.SetDataField(Const V: String);
Begin
  If FDataField <> V Then
    Begin
      FDataField := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCOrderByItem.SetDescending(V: boolean);
Begin
  If FDescending <> V Then
    Begin
      FDescending := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------------------}

Procedure PSCParamsAssignSysPrms(Params:TPSCFields;DestroyNulls: boolean;
  AFields:TPSCFields=nil);
Var
  i: Integer;
  MyProc: TPSCFltSysParamProc;
  MyData: TPSCFltSysParamProcData;
Begin
  i:=0;
  While i<Params.Count Do
    Begin
      MyProc:=PSCGetFltBoxRegister.GetSysParam(Params[i].Name);
      If Assigned(MyProc) then
        begin
          MyData := TPSCFltSysParamProcData.Create;
          Try
            MyData.Fields:=AFields;
            MyProc(MyData);
            Params[i].Value := MyData.Result;
            Params[i].ChangeDataTypeTo(MyData.DataType);
            inc(i);
          Finally
            MyData.Free;
          End;
        end
      Else
      If DestroyNulls Then
        Params[i].Free
      else
        inc(i);
    End;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.ReplaceWithSysPrms(Item: TPSCFltItem;
  Const S: String;AParams:TPSCFields): String;
Var
  Params: TPSCListParams;
Begin
  Params := TPSCListParams.Create(Item,TPSCListParam);
  Params.OnGetAsString := GetItems.GetAsString;
  Params.OnQuoteStr := GetItems.DoQuoteStr;
  Try
    Params.ParseParams(S,False);
    PSCParamsAssignSysPrms(Params,True,AParams);
    Result := PSCReplaceWithParams(S,Params);
  Finally
    Params.Free;
  End;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetRealFieldNameMask:String;
begin
  If fboUseADOFieldNameMask in GetRealFilterOptions then
    Result:=SPSCAdoFieldNameMask
  else
    Result:=FieldNameMask;
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetPreparedFieldName(Const AFieldName: String; AForSQL:
  boolean): String;
Var
  ASQLFieldName: String;
Begin
  ASQLFieldName := GetSQLFieldName(AFieldName);

  If AForSQL Then
    Result := PSCSQLPrepareFieldNameEx(GetFieldTableName(AFieldName),
      ASQLFieldName,
      ReallyAddTableNames,GetRealFieldNameMask)
  Else
    If EncloseInBrackets Then
      Result := PSCPrepareFieldName(ASQLFieldName,fptEncloseInBrackets)
    Else
      Result := ASQLFieldName;

  If Assigned(FOnPrepareFieldName) Then
    FOnPrepareFieldName(Self,AFieldName,Result);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetPreparedFilterStr(Item: TPSCFltItem;
  Const FilterStr,FieldName: String;
  ReplaceSysPrms: boolean; Params: TPSCFields): String;
Begin
  Result := FilterStr;

  PSCReplaceAllOccur(Result,SPSCFieldParam,
    GetPreparedFieldName(FieldName,FTargetIsSQL));

  If Params <> Nil Then
    Result := PSCReplaceWithParams(Result,Params);
  If ReplaceSysPrms Then
    Result := ReplaceWithSysPrms(Item,Result,Params);
End;

{------------------------------------------------------------------------------}

Function TPSCFltBoxTemplates.GetItem(Index: Integer): TPSCFltBoxTemplate;
begin
  Result:=TPSCFltBoxTemplate(inherited Items[Index]);
end;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxTemplates.SetItem(Index: Integer; V: TPSCFltBoxTemplate);
begin
  inherited Items[Index]:=V;
end;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxTemplates.EnumTemplCategories(const TemplCategories: IPSCStrings);
Var
  i: Integer;
  Temp: IPSCStrings;
  Category: String;
Begin
  Temp := PSCCreateSortedStringList;
  For i := 0 To Count - 1 Do
    Begin
      Category := PSCTrim(TPSCFltBoxTemplate(Items[i]).Category);
      If Category <> '' Then
        Temp.Add(Category);
    End;
  TemplCategories.Assign(Temp);
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxTemplate.SetParamsType(V:TPSCFieldType);
begin
  If FParamsType<>V then
  begin
    FParamsType:=V;
    Changed(False);
  end;
end;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxTemplate.SetValuePrefix(Const V: String);
Begin
  If FValuePrefix <> V Then
    Begin
      FValuePrefix := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxTemplate.SetValueSuffix(Const V: String);
Begin
  If FValueSuffix <> V Then
    Begin
      FValueSuffix := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxTemplate.SetCategory(Const V: String);
Begin
  If FCategory <> V Then
    Begin
      FCategory := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxTemplate.SetCaption(Const V: String);
Begin
  If FCaption <> V Then
    Begin
      FCaption := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxTemplate.SetFilter(Const V: String);
Begin
  If FFilter <> V Then
    Begin
      FFilter := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxTemplates.GetTemplatesWithCat(Const ACategory: String;
  const ATemplates: IPSCStrings; Const AllowedUsageIDs: String;
  AForDisplay:Boolean;AOptions:TPSCFltBldOptions);
Var
  i: Integer;
  S: String;

  Function UsageIsAllowed(UsageID: Integer): boolean;
    Function _UsageIsAllowedChar(D: Char): boolean;
    Begin
      Result := (Pos(D + PSCIntToStr(UsageID) + D,D + AllowedUsageIDs + D) > 0)
    End;
  Begin
    Result := (AllowedUsageIDs = '') Or _UsageIsAllowedChar(',') Or
      _UsageIsAllowedChar(';');
  End;

  function MyTemplateIsOk(ATemplate:TPSCFltBoxTemplate):Boolean;
  const
    CLIKE:Array[0..3] of String=(SPSCCapID_Cont,SPSCCapID_NotCont,SPSCCapID_Begins,
      SPSCCapID_Ends);
    CIN:Array[0..1] of String=(SPSCCapID_In,SPSCCapID_NotIn);
  begin
    Result:=True;

    If fboHideTemplate_LIKE in AOptions then
      Result:=not PSCStrIsInArray(ATemplate.CaptionID,CLIKE);

    If fboHideTemplate_IN in AOptions then
      Result:=not PSCStrIsInArray(ATemplate.CaptionID,CIN);
  end;

Begin
  ATemplates.Clear;
  For i := 0 To Count - 1 Do
    With Items[i] Do
      If (PSCCompareText(ACategory,Category) = 0)
        And UsageIsAllowed(UsageID) And MyTemplateIsOk(Items[i])
      Then
        begin
          If AForDisplay then
            S:=CaptionForUser
          else
            S:=CaptionID;
          ATemplates.Add(S); // never add Items[i] as Object here
        end;
End;

{------------------------------------------------------------------------------}

Function TPSCFltBoxTemplates.IndexOfCaptionID(Const ACategory,ACaptionID:
  String): Integer;
Var
  RealCategory: String;
Begin
  If ACategory = '' Then
    RealCategory := SPSCTemplCatUnknown
  Else
    RealCategory := ACategory;

  For Result := 0 To Count - 1 Do
    With TPSCFltBoxTemplate(Items[Result]) Do
      If (PSCCompareText(PSCRemoveSpChars(ACaptionID),PSCRemoveSpChars(CaptionID))
        = 0) And
        (PSCCompareText(RealCategory,Category) = 0) Then
        exit;
  Result := -1;
End;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxTemplates.AddTemplates_AdditionalFilter(const ACategory:String);
begin
  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Is;
    CaptionForUser:= PSCConsts.Cap_Is;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_AdditionalFilterIs_Proc;
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxTemplates.AddTemplates_Date(const ACategory:String);
begin
  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_On;
    CaptionForUser:= PSCConsts.Cap_On;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Yesterday;
    CaptionForUser:= PSCConsts.Cap_Yesterday;
    Caption:= SPSCTempl0;
    Filter:= SPSCFlt_FieldYest;
    ParamsType:= FT_DATE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Today;
    CaptionForUser:= PSCConsts.Cap_Today;
    Caption:= SPSCTempl0;
    Filter:= SPSCFlt_FieldToday;
    ParamsType:= FT_DATE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Tomorrow;
    CaptionForUser:= PSCConsts.Cap_Tomorrow;
    Caption:= SPSCTempl0;
    Filter:= SPSCFlt_FieldTomor;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Last7;
    CaptionForUser:= PSCConsts.Cap_Last7;
    Caption:= SPSCTempl0;
    Filter:= SPSCFlt_FieldTodayM7;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Next7;
    CaptionForUser:= PSCConsts.Cap_Next7;
    Caption:= SPSCTempl0;
    Filter:= SPSCFlt_FieldTodayP7;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_LastWeek;
    CaptionForUser:= PSCConsts.Cap_LastWeek;
    Caption:= SPSCTempl0;
    Filter:= SPSCFlt_FieldLastWeek;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_ThisWeek;
    CaptionForUser:= PSCConsts.Cap_ThisWeek;
    Caption:= SPSCTempl0;
    Filter:= SPSCFlt_FieldThisWeek;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_NextWeek;
    CaptionForUser:= PSCConsts.Cap_NextWeek;
    Caption:= SPSCTempl0;
    Filter:= SPSCFlt_FieldNextWeek;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_LastMon;
    CaptionForUser:= PSCConsts.Cap_LastMon;
    Caption:= SPSCTempl0;
    Filter:= SPSCFlt_FieldLastMon;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_ThisMon;
    CaptionForUser:= PSCConsts.Cap_ThisMon;
    Caption:= SPSCTempl0;
    Filter:= SPSCFlt_FieldThisMon;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_NextMon;
    CaptionForUser:= PSCConsts.Cap_NextMon;
    Caption:= SPSCTempl0;
    Filter:= SPSCFlt_FieldNextMon;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_OnAfter;
    CaptionForUser:= PSCConsts.Cap_OnAfter;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldGE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Onbefore;
    CaptionForUser:= PSCConsts.Cap_Onbefore;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldLE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Between;
    CaptionForUser:= PSCConsts.Cap_Between;
    Caption:= PSCConsts.Templ2;
    Filter:= SPSCFlt_FieldBet;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_In;
    CaptionForUser:= PSCConsts.Cap_In;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldIn_Proc;
    Options:=[ftoValueList];
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_NotIn;
    CaptionForUser:= PSCConsts.Cap_NotIn;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldNotIn_Proc;
    Options:=[ftoValueList];
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxTemplates.AddTemplates_Time(const ACategory:String);
begin
  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Is;
    CaptionForUser:= PSCConsts.Cap_Is;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_After;
    CaptionForUser:= PSCConsts.Cap_After;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldGE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Before;
    CaptionForUser:= PSCConsts.Cap_Before;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldLE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Between;
    CaptionForUser:= PSCConsts.Cap_Between;
    Caption:= PSCConsts.Templ2;
    Filter:= SPSCFlt_FieldBet;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_In;
    CaptionForUser:= PSCConsts.Cap_In;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldIn_Proc;
    Options:=[ftoValueList];
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_NotIn;
    CaptionForUser:= PSCConsts.Cap_NotIn;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldNotIn_Proc;
    Options:=[ftoValueList];
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxTemplates.AddTemplates_DateTime(const ACategory:String);
begin
  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_OnDate;
    CaptionForUser:= PSCConsts.Cap_OnDate;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldDateE;
    ParamsType:= FT_DATE;
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxTemplates.AddTemplates_Memo(const ACategory:String);
begin
  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Cont;
    CaptionForUser:= PSCConsts.Cap_Cont;
    Caption:= SPSCTempl1;
    UsageID:= 1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldLike_Proc;
    Options:=[ftoCanIgnoreCase];
    ValuePrefix:= '%';
    ValueSuffix:= '%';
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Begins;
    CaptionForUser:= PSCConsts.Cap_Begins;
    Caption:= SPSCTempl1;
    UsageID:= 1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldLike_Proc;
    Options:=[ftoCanIgnoreCase];
    ValueSuffix:= '%';
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Ends;
    CaptionForUser:= PSCConsts.Cap_Ends;
    Caption:= SPSCTempl1;
    UsageID:= 1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldLike_Proc;
    Options:=[ftoCanIgnoreCase];
    ValuePrefix:= '%';
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Is;
    CaptionForUser:= PSCConsts.Cap_Is;
    Caption:= SPSCTempl1;
    UsageID:= 1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_StringFieldE_Proc;
    Options:=[ftoCanIgnoreCase];
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_NotCont;
    CaptionForUser:= PSCConsts.Cap_NotCont;
    Caption:= SPSCTempl1;
    UsageID:= 1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldNotLike_Proc;
    Options:=[ftoCanIgnoreCase];
    ValuePrefix:= '%';
    ValueSuffix:= '%';
  end;

end;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxTemplates.AddTemplates_Text(const ACategory:String);
begin
  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Cont;
    CaptionForUser:= PSCConsts.Cap_Cont;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldLike_Proc;
    Options:=[ftoCanIgnoreCase];
    ValuePrefix:= '%';
    ValueSuffix:= '%';
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Begins;
    CaptionForUser:= PSCConsts.Cap_Begins;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldLike_Proc;
    Options:=[ftoCanIgnoreCase];
    ValueSuffix:= '%';
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Ends;
    CaptionForUser:= PSCConsts.Cap_Ends;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldLike_Proc;
    Options:=[ftoCanIgnoreCase];
    ValuePrefix:= '%';
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Is;
    CaptionForUser:= PSCConsts.Cap_Is;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_StringFieldE_Proc;
    Options:=[ftoCanIgnoreCase];
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_NEquals;
    CaptionForUser:= PSCConsts.Cap_NEquals;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_StringFieldNE_Proc;
    Options:=[ftoCanIgnoreCase];
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_NotCont;
    CaptionForUser:= PSCConsts.Cap_NotCont;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldNotLike_Proc;
    Options:=[ftoCanIgnoreCase];
    ValuePrefix:= '%';
    ValueSuffix:= '%';
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Between;
    CaptionForUser:= PSCConsts.Cap_Between;
    Caption:= PSCConsts.Templ2;
    Filter:= SPSCFlt_FieldBet;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_More;
    CaptionForUser:= PSCConsts.Cap_More;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldG;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Less;
    CaptionForUser:= PSCConsts.Cap_Less;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldL;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_AtLeast;
    CaptionForUser:= PSCConsts.Cap_AtLeast;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldGE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_AtMost;
    CaptionForUser:= PSCConsts.Cap_AtMost;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldLE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_BlankEmpty;
    CaptionForUser:= PSCConsts.Cap_BlankEmpty;
    Caption:= SPSCTempl0;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_BlankOrEmpty_Proc;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_In;
    CaptionForUser:= PSCConsts.Cap_In;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldIn_Proc;
    Options:=[ftoValueList];
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_NotIn;
    CaptionForUser:= PSCConsts.Cap_NotIn;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldNotIn_Proc;
    Options:=[ftoValueList];
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxTemplates.AddTemplates_Number(const ACategory:String);
begin
  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Equals;
    CaptionForUser:= PSCConsts.Cap_Equals;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_NEquals;
    CaptionForUser:= PSCConsts.Cap_NEquals;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldNE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_AtMost;
    CaptionForUser:= PSCConsts.Cap_AtMost;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldLE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_AtLeast;
    CaptionForUser:= PSCConsts.Cap_AtLeast;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldGE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_More;
    CaptionForUser:= PSCConsts.Cap_More;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldG;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Less;
    CaptionForUser:= PSCConsts.Cap_Less;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldL;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Between;
    CaptionForUser:= PSCConsts.Cap_Between;
    Caption:= PSCConsts.Templ2;
    Filter:= SPSCFlt_FieldBet;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_In;
    CaptionForUser:= PSCConsts.Cap_In;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldIn_Proc;
    Options:=[ftoValueList];
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_NotIn;
    CaptionForUser:= PSCConsts.Cap_NotIn;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldNotIn_Proc;
    Options:=[ftoValueList];
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxTemplates.AddTemplates_Unknown(const ACategory:String);
begin
  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_IsEmpty;
    CaptionForUser:= PSCConsts.Cap_IsEmpty;
    Caption:= SPSCTempl0;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldIsNull_Proc;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_IsNEmpty;
    CaptionForUser:= PSCConsts.Cap_IsNEmpty;
    Caption:= SPSCTempl0;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldIsNotNull_Proc;
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxTemplates.AddTemplates_Boolean(const ACategory:String);
begin
  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Equals;
    CaptionForUser:= PSCConsts.Cap_Equals;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_BooleanFieldE_Proc;
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxTemplates.AddTemplates_Lookup(const ACategory:String);
begin
  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_Equals;
    CaptionForUser:= PSCConsts.Cap_Equals;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_NEquals;
    CaptionForUser:= PSCConsts.Cap_NEquals;
    Caption:= SPSCTempl1;
    Filter:= SPSCFlt_FieldNE;
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_In;
    CaptionForUser:= PSCConsts.Cap_In;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldIn_Proc;
    Options:=[ftoValueList];
  end;

  With TPSCFltBoxTemplate(Add) Do
  Begin
    Category:=ACategory;
    CaptionID:= SPSCCapID_NotIn;
    CaptionForUser:= PSCConsts.Cap_NotIn;
    Caption:= SPSCTempl1;
    FilterKind:=tfkProc;
    Filter:= SPSCFlt_FieldNotIn_Proc;
    Options:=[ftoValueList];
  end;
end;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxTemplates.InitWithDefault;
Begin
  BeginUpdate;
  try
    Clear;

    AddTemplates_AdditionalFilter(SPSCTemplCatAdditionalFilter);
    AddTemplates_Memo(SPSCTemplCatMemo);
    AddTemplates_Text(SPSCTemplCatText);
    AddTemplates_Time(SPSCTemplCatTime);
    AddTemplates_Date(SPSCTemplCatDate);
    AddTemplates_DateTime(SPSCTemplCatDateTime);
    AddTemplates_Date(SPSCTemplCatDateTime);
    AddTemplates_Boolean(SPSCTemplCatBoolean);
    AddTemplates_Unknown(SPSCTemplCatUnknown);
    AddTemplates_Number(SPSCTemplCatNumber);
    AddTemplates_Lookup(SPSCTemplCatLookup);

    AddTemplates_Unknown(SPSCTemplCatMemo);
    AddTemplates_Unknown(SPSCTemplCatText);
    AddTemplates_Unknown(SPSCTemplCatTime);
    AddTemplates_Unknown(SPSCTemplCatDate);
    AddTemplates_Unknown(SPSCTemplCatDateTime);
    AddTemplates_Unknown(SPSCTemplCatBoolean);
    AddTemplates_Unknown(SPSCTemplCatNumber);
    AddTemplates_Unknown(SPSCTemplCatLookup);

    If Assigned(PSCModifyDefaultTemplProc) Then
      PSCModifyDefaultTemplProc(Self);
  finally
    EndUpdate;
  end;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxTemplate.SetCaptionForUser(Const V: String);
begin
  If FCaptionForUser<>V then
  begin
    FCaptionForUser:=V;
    Changed(False);
  end;
end;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxTemplate.SetCaptionID(Const V: String);
Begin
  If FCaptionID <> V Then
    Begin
      FCaptionID := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------------------}

destructor TPSCFltBoxTemplate.Destroy;
begin
  inherited;
end;

{------------------------------------------------------------------------------}

Function TPSCFltBoxTemplate.GetDisplayName: String;
Begin
  If (Category <> '') And (CaptionID <> '') Then
    Result := Category + '.[' + CaptionID + ']'
  Else
    Result := Inherited GetDisplayName;
End;

{------------------------------------------------------------------------------}

const
  SPSCNENull = '<> NULL';//don't resource
  SPSCEqNull = '= NULL';//don't resource

Function PSCEqNullToIsNull(Const SQL: String): String;
Begin
  Result := SQL;
  PSCReplaceAllOccurEx(Result,SPSCNENull,SPSCIsNotNull,True);
  PSCReplaceAllOccurEx(Result,SPSCEqNull,SPSCIsNull,True);
End;

{------------------------------------------------------------------------------}

Function PSCIsNullToEqNull(Const SQL: String): String;
Begin
  Result := SQL;
  PSCReplaceAllOccurEx(Result,SPSCIsNotNull,SPSCNENull,True);
  PSCReplaceAllOccurEx(Result,SPSCIsNull,SPSCEqNull,True);
End;

{------------------------------------------------------------------------------}

Function PSCChangeIsNull(Const SQL: String; ChangeKind: TPSCChangeIsNullKind):
  String;
Begin
  Case ChangeKind Of
    ctIsToEq:
      Result := PSCIsNullToEqNull(SQL);
    ctEqToIs:
      Result := PSCEqNullToIsNull(SQL);
  Else
    Result := SQL;
  End;
End;

{------------------------------------------------------------------------------}

Procedure PSCChangeIsNullInTemplates(Templates: TPSCFltBoxTemplates;
  ChangeKind: TPSCChangeIsNullKind);
Var
  i: Integer;
Begin
  With Templates Do
    For i := 0 To Count - 1 Do
      With TPSCFltBoxTemplate(Templates.Items[i]) Do
        Filter := PSCChangeIsNull(Filter,ChangeKind);
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxField.AssignDataSetField(Field: TField);
Var
  OriginalField: TField;
Begin
  If Not (Field.FieldKind In [fkData,fkLookup]) Then
    PSCError(PSCConsts.ErrInvalidFieldKind);

  DataField := Field.FieldName;
  If Field.DisplayLabel <> '' Then
    DisplayLabel := Field.DisplayLabel;
  If Field.FieldKind = fkLookup Then
    Begin
      If Pos(';',Field.KeyFields) > 0 Then
        PSCError('Multiple field names in the KeyFields are not supported')
      Else
        Begin
          OriginalField := Field.DataSet.FieldByName(Field.KeyFields);
          LookupKeyField := Field.LookupKeyFields;
          LookupDisplayField := Field.LookupResultField;
          LookupDataSet := Field.LookupDataSet;
          DataType := PSCDBFieldTypeToPSC(OriginalField.DataType);
          SQLFieldName := OriginalField.FieldName;
        End;
    End
  Else
    Begin
      DataType := PSCDBFieldTypeToPSC(Field.DataType);
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxField.Assign(Source: TPersistent);
Begin
  If Source Is TField Then
    AssignDataSetField(TField(Source))
  Else
    Inherited;
End;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxField.SetLookupFilterSource(V: TPSCCustomFltBld);
begin
  If FLookupFilterSource <> V Then
    Begin
      FLookupFilterSource := V;
      RegisterNotifier(V);
    End;
end;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxField.SetLookupDataSet(V: TDataSet);
Begin
  If FLookupDataSet <> V Then
    Begin
      FLookupDataSet := V;
      RegisterNotifier(V);
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxField.Notification(Instance: TComponent);
Begin
  inherited;
  If Instance = FLookupDataSet Then
    FLookupDataSet := Nil
  else
  If Instance = FLookupFilterSource then
    FLookupFilterSource := Nil;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxField.SetPickList(V: TStrings);
Begin
  FPickList.Assign(V);
End;

{------------------------------------------------------------------------------}

Constructor TPSCFltBoxField.Create(ACollection: TCollection);
Begin
  Inherited;
  FPickList := TStringList.Create;
  FFieldNumber:=-1;
  FVisible:=True;
End;

{------------------------------------------------------------------------------}

Destructor TPSCFltBoxField.Destroy;
Begin
  FPickList.Free;
  Inherited;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxField.SetFieldNumber(V:Integer);
begin
  If V<>FFieldNumber then
  begin
    FFieldNumber:=V;
    Changed(False);
  end;
end;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxField.SetDataType(V: TPSCFieldType);
Begin
  If V <> FDataType Then
    Begin
      FDataType := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxField.SetTemplateCat(Const V: String);
Begin
  If FTemplateCat <> V Then
    Begin
      FTemplateCat := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxField.SetDataField(Const V: String);
Begin
  If FDataField <> V Then
    Begin
      FDataField := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------------------}

Function TPSCFltBoxFields.FieldByName(Const FieldName: String): TPSCFltBoxField;
Var
  i: Integer;
Begin
  For i := 0 To Count - 1 Do
    If PSCCompareText(FieldName,TPSCFltBoxField(Items[i]).DataField) = 0 Then
      Begin
        Result := TPSCFltBoxField(Items[i]);
        exit;
      End;
  Result := Nil;
End;

{------------------------------------------------------------------------------}

Function TPSCFltBoxFields.GetTemplCategory(Const FieldName: String): String;
Var
  Item: TPSCFltBoxField;
Begin
  Item := FieldByName(FieldName);
  If Item <> Nil Then
    Result := Item.TemplCat
  Else
    Result := SPSCTemplCatUnknown;
  If Result = '' Then
    Result := SPSCTemplCatUnknown;
End;

{------------------------------------------------------------------------------}

function TPSCFltBoxFields.GetItem(Index: Integer): TPSCFltBoxField;
begin
  Result:=TPSCFltBoxField(inherited Items[Index]);
end;

{------------------------------------------------------------------------------}

procedure TPSCFltBoxFields.SetItem(Index: Integer; Value: TPSCFltBoxField);
begin
  TPSCFltBoxField(inherited Items[Index]).Assign(Value);
end;

{------------------------------------------------------------------------------}

function TPSCFltBoxFields.GetHiddenProps:TArray<string>;
const
  MyArray: TArray<String> = [
    'LookupDataSet',
    'LookupFilterSource',
//    'PickList',
    'Collection',
    'DataObject',
    'Tag',
    'Index',
    'DisplayName',
    'UserFields'
  ];
begin
  result:=MyArray;
end;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxFields.AppendFieldsList(const AFieldsList: IPSCStrings;
  AFieldType:TPSCFieldType);
Var
  I: Integer;
  S: String;
Begin
  For I := 0 To Count - 1 Do
    With TPSCFltBoxField(Items[I]) Do
      Begin
        If (AFieldType <> FT_UNK) And
          (AFieldType <> DataType) And (DataType <> FT_UNK) Then
          continue;
        If not Visible then
          continue;
        S := PSCTrim(TPSCFltBoxField(Items[i]).DataField);
        If S <> '' Then
          AFieldsList.Add(S);
      End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxFields.GetFieldsList(const FieldsList: IPSCStrings;
  AFieldType:TPSCFieldType);
Begin
  FieldsList.Clear;
  AppendFieldsList(FieldsList,AFieldType);
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltBoxField.SetDisplayLabel(Const V: String);
Begin
  If FDisplayLabel <> V Then
    Begin
      FDisplayLabel := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------------------}

Function TPSCFltBoxField.GetDisplayName: String;
Begin
  Result := DataField;
  If Result = '' Then
    Result := Inherited GetDisplayName;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetSQL(V: TStrings);
Begin
  // do nothing as SQL is autogenerated property
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetForceSQLDateTime: boolean;
Begin
  Result := GetItems.ForceSQLDateTime;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetForceSQLDateTime(V: boolean);
Begin
  GetItems.ForceSQLDateTime := V;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFilterDateTimeFormat: TPSCDateTimeFormat;
Begin
  Result := GetItems.FilterDateTimeFormat;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetFilterDateTimeFormat(V: TPSCDateTimeFormat);
Begin
  GetItems.FilterDateTimeFormat := V;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetSQLDateTimeFormat: TPSCDateTimeFormat;
Begin
  Result := GetItems.SQLDateTimeFormat;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetSQLDateTimeFormat(V: TPSCDateTimeFormat);
Begin
  GetItems.SQLDateTimeFormat := V;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetDisplayDateTimeFormat: TPSCDateTimeFormat;
Begin
  Result := GetItems.DisplayDateTimeFormat;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetDisplayDateTimeFormat(V: TPSCDateTimeFormat);
Begin
  GetItems.DisplayDateTimeFormat := V;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetItems: TPSCFltItems;
Begin
  Result := FItems;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetFiltered(V: boolean);
Begin
  If FFiltered <> V Then
    Begin
      FFiltered := V;
      UpdateDataSets;
    End;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.GetFieldsList(const AFieldsList: IPSCStrings);
Begin
  GetFieldsListEx(AFieldsList,FT_UNK);
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomFltBld.DoGetAsString(AField:TPSCField;var AResultStr:String;
  AForDisplay:Boolean);

  function IsAdditionalFilterField(AField:TPSCField):Boolean;
  var
    MyItem:TPSCFltItem;
  begin
    Result:=False;
    If TPSCCustomNamedItems(AField.Collection).Owner is TPSCFltItem then
    begin
      MyItem:=TPSCFltItem(TPSCCustomNamedItems(AField.Collection).Owner);
      Result:=MyItem.DataField=SPSCAdditionalFilterField;
    end;
  end;

begin
  If (fboBooleanAsBit in GetRealFilterOptions) and TargetIsSQL and
    (AField.DataType=FT_BOOL)
  then
    begin
      If AField.AsBoolean then
        AResultStr:='1'
      else
        AResultStr:='0';
    end;

  If (fboDoubleQuoteInValue in GetRealFilterOptions) and (not AForDisplay)
    and (AField.DataType in [FT_STRING]) and (Not IsAdditionalFilterField(AField)) then
  begin
    PSCReplaceAllOccur(AResultStr,#39,#39#39);
  end;

  If Assigned(FOnGetAsString) then
    FOnGetAsString(Self,AField,AResultStr,AForDisplay);
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.GetFieldsListEx(const AFieldsList: IPSCStrings;
  AFieldType:TPSCFieldType;ASpecialFields:Boolean=True);

  Procedure GetFromFields;
  Begin
    Fields.GetFieldsList(AFieldsList,AFieldType);
  End;

  Procedure GetFromFieldParams;
  Begin
    FFieldParams.GetFieldsList(AFieldsList,AFieldType);
  End;

  Procedure GetFromDataSet;
  Var
    I: Integer;
  Begin
    With DataSet Do
      For I := 0 To FieldCount - 1 Do
        With Fields[I] Do
        begin
          If ExcludeBlobs and IsBlob then
            continue;

          If Not Visible then
            continue;

          If (FieldKind = fkLookup) And (FindField(KeyFields)=Nil) then
            continue;

          If (FieldKind=fkCalculated) and
             ([fboIncludeCalculated]*GetRealFilterOptions<>[fboIncludeCalculated])
          then
            continue;

          If not (FieldKind In [fkData,fkLookup,fkCalculated,fkInternalCalc]) then
            continue;

          If (AFieldType = FT_UNK) Or (DataType = ftUnknown) Or
            (AFieldType = PSCDBFieldTypeToPSC(DataType))
          Then
            AFieldsList.Add(FieldName);// never add anything as object here
        end;
  End;

  Procedure GetSpecialFields;
  begin
    If (AFieldsList.Count=0) or not ASpecialFields then
      exit;
    If not (fboHideAdditionalFilter in GetRealFilterOptions) then
      AFieldsList.Add(SPSCAdditionalFilterField);
  end;

Begin
  AFieldsList.Clear;
  If Fields.Count > 0 Then
    GetFromFields
  Else
    If DataSet <> Nil Then
      GetFromDataSet
    Else
      GetFromFieldParams;

  AdditionalFields.AppendFieldsList(AFieldsList,AFieldType);

  PSCRemoveStrings(AFieldsList,PSCCreateStringsAdapter(FHiddenFields));

  GetSpecialFields;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.IsAdditionalFieldsStored:Boolean;
begin
  Result:=FAdditionalFields.Count>0;
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetAdditionalFields(V: TPSCFltBoxFields);
Begin
  FAdditionalFields.Assign(V);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetFieldParams(V: TPSCFltBoxFields);
Begin
  FFieldParams.Assign(V);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetFields(V: TPSCFltBoxFields);
Begin
  FFields.Assign(V);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetHiddenFields(V: TStrings);
Begin
  FHiddenFields.Assign(V);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetAdditionalFilter(Const V: String);
Begin
  If FAdditionalFilter <> V Then
    Begin
      FAdditionalFilter := V;
      UpdateDataSets;
    End;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.ActionPermitted(AID:Integer;
  const AParams:IPSCDataFields):LongBool;
begin
  Result:=True;
  If Assigned(FOnPermitAction) then
    FOnPermitAction(Self,AID,AParams,Result);
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.CanSetFieldSortOrder(const AFieldName:String;
  ASortType:TPSCFieldSortType;AClearExisting:Boolean):Boolean;
const
  SFieldName='FieldName';
  SSortType='SortType';
  SClearExisting='ClearExisting';
var
  MyFields:IPSCDataFields;
begin
  MyFields:=PSCCreateMemDataFields(
    [SFieldName,SSortType,SClearExisting],
    [FT_STRING,FT_INT,FT_BOOL]);

  With MyFields do
  begin
    FindField(SFieldName).AsString:=AFieldName;
    FindField(SSortType).AsInteger:=Ord(ASortType);
    FindField(SClearExisting).AsBoolean:=AClearExisting;
  end;

  Result:=ActionPermitted(ACT_FLTBLD_SETFLDORDER,MyFields);
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetFieldSortOrder(const AFieldName:String;
  ASortType:TPSCFieldSortType;AClearExisting:Boolean);
var
  MySortType:TPSCFieldSortType;
  MySortOrder:Integer;
  MyItem:TPSCOrderByItem;
begin
  If not CanSetFieldSortOrder(AFieldName,ASortType,AClearExisting) then
    exit;

  FOrderByItems.BeginUpdate;
  try
    If AClearExisting then
      FOrderByItems.Clear;
    MySortType:=GetFieldSortOrder(AFieldName,MySortOrder);
    If MySortType=ASortType then
      exit;
    If MySortType=SortType_Unsorted then
      MySortOrder:=FOrderByItems.Add.Index;
    MyItem:=FOrderByItems.Items[MySortOrder];
    If ASortType=SortType_Unsorted then
      MyItem.Free
    else
      With MyItem do
      begin
        DataField:=AFieldName;
        Descending:=ASortType=SortType_Descending;
      end;
  finally
    FOrderByItems.EndUpdate;
  end;
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFieldSortOrder(const AFieldName:String;
  var ASortOrder:Integer):TPSCFieldSortType;
var
  i:Integer;
begin
  for i:=0 to FOrderByItems.Count-1 do
    With FOrderByItems.Items[i] do
      if AFieldName=DataField then
      begin
        If Descending then
          Result:=SortType_Descending
        else
          Result:=SortType_Ascending;
        ASortOrder:=i;
        exit;
      end;
  Result:=SortType_Unsorted;
  ASortOrder:=0;
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.UpdateDataSets;
Begin
  FilterStrChanged := True;
  Changed;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetSQLHead(V: TStrings);
Begin
  FSQLHead.Assign(V);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetSQLTail(V: TStrings);
Begin
  FSQLTail.Assign(V);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetOrderByItems(V: TPSCOrderByItems);
Begin
  FOrderByItems.Assign(V);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetRealSQLPropName: String;
begin
  Result:=FSQLPropName;

  If fboUseCommandTextioSQL in GetRealFilterOptions then
      Result:='CommandText';//don't resource

  If Result='' then
    Result:='SQL';//don't resource
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetSQLHeadText: String;
Begin
  Result := PSCTrim(FSQLHead.Text);
  If (PSCRemoveCharSet([#0..#31],Result) = '') And (Not FSQLAlreadyTaken) And
    (DataSet <> Nil) Then
    Begin
      If PSCPropertyExists(DataSet,GetRealSQLPropName) then
      begin
        If PSCIsPropStrings(GetPropInfo(DataSet.ClassInfo,GetRealSQLPropName)) then
          Result := PSCGetStringsPropAsText(DataSet, GetRealSQLPropName)
        else
          Result := PSCGetPropValue(DataSet,GetRealSQLPropName);
      end;
      If Not (csDesigning In ComponentState) Then
        Begin
          FSQLHead.Text := Result;
          FSQLAlreadyTaken := True;
        End;
    End;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetSQLOBTail(V: TStrings);
Begin
  FSQLOBTail.Assign(V);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.Notification(AComponent: TComponent; AOperation:
  TOperation);
Begin
  Inherited;
  If (AOperation = opRemove) And (AComponent = DataSet) Then
    DataSet := Nil;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetDataSet(V: TDataSet);
Begin
  If V <> FDataSet Then
    Begin
      FDataSet := v;
      If Assigned(V) Then
        V.FreeNotification(Self);
      Changed;
    End;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetTemplates(V: TPSCFltBoxTemplates);
Begin
  FTemplates.Assign(V);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFieldType(Const FieldName: String): TPSCFieldType;

  Procedure GetFromDataSet(ADataSet: TDataSet; Const AFieldName: String);
  Var
    Field: TField;
  Begin
    Field := ADataSet.FindField(AFieldName);
    If Field <> Nil Then
      If Field.FieldKind = fkLookup Then
        GetFromDataSet(Field.LookupDataSet,Field.LookupKeyFields)
      Else
        Result := PSCDBFieldTypeToPSC(Field.DataType);
  End;

  Function GetFromFields(Flds: TPSCFltBoxFields; Var DataType: TPSCFieldType):
    boolean;
  Var
    FilterField: TPSCFltBoxField;
  Begin
    FilterField := Flds.FieldByName(FieldName);
    Result := FilterField <> Nil;
    If Result Then
      DataType := FilterField.DataType;
  End;

Begin
  Result := FT_UNK;
  GetFromFields(FFieldParams,Result);
  If Result = FT_UNK Then
    Begin
      If Not GetFromFields(Fields,Result) Then
        If DataSet <> Nil Then
          GetFromDataSet(DataSet,FieldName);
    End;

  If Result = FT_UNK Then
    GetFromFields(AdditionalFields,Result);

  If (Result = FT_UNK) and (FieldName=SPSCAdditionalFilterField) Then
    Result:=FT_STRING;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.IsFieldLookup(Const FieldName: String): Boolean;
Var
  Field: TField;
Begin
  Result := False;
  If Fields.Count > 0 Then
    Exit;
  Field := DataSet.FindField(FieldName);
  If Field <> Nil Then
    Result := Field.FieldKind = fkLookup;
End;

{------------------------------------------------------------------------------}

Function PSCIsSystemParam(ParamName: String): boolean;
Begin
  ParamName := ':' + ParamName;
  Result := (PSCCompareText(ParamName,SPSCFieldParam) = 0) Or
    (PSCCompareText(ParamName,SPSCTemplateParam) = 0) Or
    (PSCCompareText(ParamName,SPSCOpParam) = 0);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetEncloseInBrackets(V: boolean);
Begin
  If V <> FEncloseInBrackets Then
    Begin
      FEncloseInBrackets := V;
      Changed;
    End;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.GetFieldPickList(Const FieldName: String;
  const PickList:IPSCStrings);

  Procedure GetFromFields(Flds: TPSCFltBoxFields);
  Var
    FilterField: TPSCFltBoxField;
  Begin
    FilterField := Flds.FieldByName(FieldName);
    If FilterField <> Nil Then
      PickList.Assign(PSCCreateStringsAdapter(FilterField.PickList));
  End;

Begin
  PickList.Clear;
  If FieldName <> '' Then
    Begin
      GetFromFields(FFieldParams);

      If PickList.Count = 0 Then
        GetFromFields(Fields);

      If PickList.Count = 0 Then
        GetFromFields(AdditionalFields);

      DoModifyPickList(FieldName,PickList);
    End;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.GetFieldTemplates(Const FieldName: String;
  const TemplList:IPSCStrings;AForDisplay:Boolean);
Begin
  FTemplates.GetTemplatesWithCat(
    GetTemplCategory(FieldName),TemplList,FAllowedUsageIDs,AForDisplay,
    GetRealFilterOptions);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetSQLFieldName(Const FieldName: String): String;

  Function GetFromFields(Flds: TPSCFltBoxFields): String;
  Var
    FilterField: TPSCFltBoxField;
  Begin
    FilterField := Flds.FieldByName(FieldName);
    If FilterField <> Nil Then
      Result := FilterField.SQLFieldName;
  End;
Var
  FieldDesc: TField;
Begin
  Result := GetFromFields(Fields);
  If Result = '' Then
    Result := GetFromFields(FieldParams);
  If Result = '' Then
    Result := GetFromFields(AdditionalFields);

  If Result = '' Then
    Begin
      If DataSet <> Nil Then
        Begin
          FieldDesc := DataSet.FindField(FieldName);
          If (FieldDesc <> Nil) And (FieldDesc.FieldKind = fkLookup) Then
            Result := FieldDesc.KeyFields
          Else
            Result := FieldName;
        End
      Else
        Result := FieldName;
    End;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFieldTableName(Const FieldName: String): String;

  Function GetFromFields(Flds: TPSCFltBoxFields): String;
  Var
    FilterField: TPSCFltBoxField;
  Begin
    FilterField := Flds.FieldByName(FieldName);
    If FilterField <> Nil Then
      Result := FilterField.TableName;
  End;

Begin
  Result := GetFromFields(Fields);

  If Result = '' Then
    Result := GetFromFields(FieldParams);

  If Result = '' Then
    Result := GetFromFields(AdditionalFields);

  If (Result = '') And (TableNames.Count = 1) Then
    Result := TableNames[0];
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFieldLookupDef(Const FieldName: String): TPSCLookupDef;

  Procedure GetFromDataSet;
  Var
    Field: TField;
  Begin
    Field := DataSet.FindField(FieldName);
    If Field <> Nil Then
      With Result Do
        Begin
          KeyField := Field.LookupKeyFields;
          DisplayField := Field.LookupResultField;
          DataSet := Field.LookupDataSet;
          If Pos(';',KeyField) > 0 Then
            DataSet := Nil;
        End;
  End;

  Function GetFromFields(Flds: TPSCFltBoxFields; Var LookupDef: TPSCLookupDef):
    boolean;
  Var
    FilterField: TPSCFltBoxField;
  Begin
    FilterField := Flds.FieldByName(FieldName);
    Result := FilterField <> Nil;
    If Result Then
      With LookupDef Do
        Begin
          KeyField := FilterField.LookupKeyField;
          DisplayField := FilterField.LookupDisplayField;
          DataSet := FilterField.LookupDataSet;
          GridFields:= FilterField.LookupGridFields;
        End;
  End;

Begin
  Result := CPSCEmptyLookupDef;
  If FieldName <> '' Then
    Begin
      If Not GetFromFields(AdditionalFields,Result) Then
        If Not GetFromFields(FieldParams,Result) Then
          If Not GetFromFields(Fields,Result) Then
            If DataSet <> Nil Then
              GetFromDataSet;
    End;
End;

{------------------------------------------------------------------}

function TPSCCustomFltBld.GetFieldDisplayValue(Const FieldName: String): String;

  Procedure GetFromDataSet;
  Var
    Field: TField;
  Begin
    Field := DataSet.FindField(FieldName);
    If Field <> Nil Then
      Result := Field.DisplayName;
  End;

  Function GetFromFields(Flds: TPSCFltBoxFields; Var DispLabel: String):
    boolean;
  Var
    FilterField: TPSCFltBoxField;
  Begin
    FilterField := Flds.FieldByName(FieldName);
    Result := FilterField <> Nil;
    If Result Then
      DispLabel := FilterField.DisplayLabel;
  End;

Begin
  Result := '';
  If Not GetFromFields(AdditionalFields,Result) Then
    If Not GetFromFields(FieldParams,Result) Then
      If Not GetFromFields(Fields,Result) Then
        If DataSet <> Nil Then
          GetFromDataSet;
  If Result = '' Then
  begin
    If FieldName=SPSCAdditionalFilterField then
      Result:=PSCConsts.AdditionalFilter
    else
      Result := FieldName;
  end;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.GetDisplayFieldsList(const AStrings: IPSCStrings);
Begin
  GetDisplayFieldsListEx(AStrings,FT_UNK);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.GetDisplayFieldsListEx(const AStrings: IPSCStrings;
  AFieldType:TPSCFieldType);
Var
  I: Integer;
Begin
  GetFieldsListEx(AStrings,AFieldType);
  For I := 0 To AStrings.Count - 1 Do
    AStrings[I] := GetFieldDisplayValue(AStrings[I]);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.DoModifyPickList(Const FieldName: String;
  const PickList:IPSCStrings);
Begin
  If Assigned(FOnModifyPickList) Then
    FOnModifyPickList(Self,FieldName,PickList);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetOrderByChanged: boolean;
Begin
  GetOrderByText;
  Result := FOrderByChanged;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFilterStrChanged: boolean;
Begin
  GetFilterStr;
  Result := FFilterStrChanged;
  GetSQL;
  Result := Result or FFilterStrChanged;
  FFilterStrChanged:=Result;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomFltBld.GetRealFilterOptions:TPSCFltBldOptions;
begin
  Result:=FilterOptions;
  If ConditionBracketsNeeded then
    Result:=Result+[fboAddConditionBrackets];
end;

{------------------------------------------------------------------------------}

Function TPSCCustomFltBld.CanChanged:Boolean;
const
  CState=[csloading,csReading,csDesigning,csDestroying];
begin
  Result:=
      (CState * ComponentState = []) And
      ((Owner = Nil) Or (CState * Owner.ComponentState = [])) And
      ((DataSet = Nil) Or (CState * DataSet.ComponentState = []));
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.Changed;
var
  MyFilterStrChanged:Boolean;
Begin
  FEvents.HandleSimpleEvent(EVENT_AFTER_CHANGE);
  If CanChanged Then
    Begin
      MyFilterStrChanged:=FilterStrChanged Or OrderByChanged Or FUpdateOnLoaded;

      If MyFilterStrChanged Then
      Begin
          If Assigned(OnChange) Then
            OnChange(Self);

          FilterStrChanged := False;
          FOrderByChanged := False;
          FUpdateOnLoaded := False;
      End;

      If (DataSet<>nil) and (fboAutoSearch in FilterOptions) then
        FindRecord(stFirst);
    End;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.GetFieldsFromDataSet(ADataSet: TDataSet);
Var
  i: Integer;
Begin
  Fields.Clear;
  For i := 0 To ADataSet.FieldCount - 1 Do
    If ADataSet.Fields[i].FieldKind In [fkData,fkLookup] Then
      Fields.Add.Assign(ADataSet.Fields[i]);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFirstTemplate(Const FieldName: String;
  AForDisplay:Boolean): String;
Var
  Temp: IPSCStrings;
Begin
  Result := '';
  Temp := PSCCreateStringList;
  GetFieldTemplates(FieldName,Temp,AForDisplay);
  If Temp.Count = 0 Then
    exit;
  Result := Temp[0];
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetTemplCategory(Const FieldName: String): String;
var
  MyLookupDef:TPSCLookupDef;
Begin
  If Fields.Count > 0 Then
    Result := Fields.GetTemplCategory(FieldName)
  Else
    Result := SPSCTemplCatUnknown;

  If (AdditionalFields.Count > 0) And (Result = SPSCTemplCatUnknown) Then
    Result := AdditionalFields.GetTemplCategory(FieldName);

  If Result = SPSCTemplCatUnknown Then
    Begin
      If FFieldParams.Count > 0 Then
        Result := FFieldParams.GetTemplCategory(FieldName);

      If not (fboNoLookupCat in FilterOptions) and (Result = SPSCTemplCatUnknown) then
      begin
        MyLookupDef:=GetFieldLookupDef(FieldName);
        If PSCIsValidLookupDef(MyLookupDef) then
          Result:=SPSCTemplCatLookup
        else
          Result := SPSCTemplCatUnknown;
      end;

      If Result = SPSCTemplCatUnknown Then
        Result := PSCGetFieldTemplCat(DataSet,FieldName,
          not (fboNoLookupCat in FFilterOptions));

      If (Result = SPSCTemplCatUnknown) and (FieldName=SPSCAdditionalFilterField) then
        Result:=SPSCTemplCatAdditionalFilter;

      If Result = SPSCTemplCatUnknown Then
        Result := PSCGetDefTemplCatByDataType(GetFieldType(FieldName));
    End;

End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetVersion(const V:String);
begin
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.IsFilterOptionsStored:Boolean;
begin
  Result:=FilterOptions<>cPSCDefaultFltBldOptions;
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.FieldsChanged(Sender:TObject);
begin
  FEvents.HandleSimpleEvent(EVENT_AFTER_CHANGE);
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBox.GetCurrentFieldSortOrder:TPSCFieldSortType;
var
  MyFieldName:String;
  MySortOrder:Integer;
begin
  If (ItemIndex>=0) and (ItemIndex<Items.Count) and (FilterSource<>nil) then
    begin
      MyFieldName:=TPSCFltItem(Items[ItemIndex]).DataField;
      Result:=FilterSource.GetFieldSortOrder(MyFieldName,MySortOrder);
    end
  else
    Result:=SortType_Unsorted;
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBox.SetCurrentFieldSortOrder(ASortType:TPSCFieldSortType;
  AClearExisting:Boolean);
var
  MyFieldName:String;
begin
  If (ItemIndex>=0) and (ItemIndex<Items.Count) and (FilterSource<>nil) then
  begin
    MyFieldName:=TPSCFltItem(Items[ItemIndex]).DataField;
    FilterSource.SetFieldSortOrder(MyFieldName,ASortType,AClearExisting);
    Invalidate;
  end;
end;

{-------------------------------------------}

Constructor TPSCFltBoxActionSortAsc.Create(AOwner: TComponent);
begin
  Inherited;
  ImageIndex := Integer(Image_SortAsc);
  Caption := PSCConsts.SortAscending;
  ShortCut := PSCShortCutFromKeyDef(cPSCFltBoxSortAscKey);
end;

{-------------------------------------------}

Procedure TPSCFltBoxActionFindRecord.ExecuteTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
    if FilterSource<>nil then
      FilterSource.FindRecord(GetDirection);
end;

{-------------------------------------------}

function TPSCFltBoxActionFindRecord.GetDirection:TPSCSearchType;
begin
  Result:=stFirst;
end;

{-------------------------------------------}

Procedure TPSCFltBoxActionFindRecord.UpdateTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
  begin
    Self.Enabled := (FilterSource<>nil) and (FilterSource.DataSet<>nil);
    Self.Visible := not (fboHideFindRecord In GetRealOptions);
  end;
end;

{-------------------------------------------}

Constructor TPSCFltBoxActionFindNextRecord.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := Integer(Image_SearchNext);
  Caption := PSCConsts.FindNext;
  ShortCut := PSCShortCutFromKeyDef(cPSCFltBoxFindNextKey);
end;

{-------------------------------------------}

Constructor TPSCFltBoxActionFindPriorRecord.Create(AOwner: TComponent);
Begin
  inherited;
  ImageIndex := Integer(Image_SearchPrev);
  Caption := PSCConsts.FindPrev;
  ShortCut := PSCShortCutFromKeyDef(cPSCFltBoxFindPrevKey);
End;

{-------------------------------------------}

Constructor TPSCFltBoxActionFindLastRecord.Create(AOwner: TComponent);
Begin
  inherited;
  ImageIndex := Integer(Image_SearchLast);
  Caption := PSCConsts.FindLast;
  ShortCut := PSCShortCutFromKeyDef(cPSCFltBoxFindLastKey);
End;

{-------------------------------------------}

Constructor TPSCFltBoxActionFindFirstRecord.Create(AOwner: TComponent);
Begin
  inherited;
  ImageIndex := Integer(Image_SearchFirst);
  Caption := PSCConsts.FindFirst;
  ShortCut := PSCShortCutFromKeyDef(cPSCFltBoxFindFirstKey);
End;

{-------------------------------------------}

function TPSCFltBoxActionFindNextRecord.GetDirection:TPSCSearchType;
Begin
  Result:=stNext;
End;

{-------------------------------------------}

function TPSCFltBoxActionFindPriorRecord.GetDirection:TPSCSearchType;
Begin
  Result:=stPrior;
End;

{-------------------------------------------}

function TPSCFltBoxActionFindLastRecord.GetDirection:TPSCSearchType;
Begin
  Result:=stLast;
End;

{-------------------------------------------}

function TPSCFltBoxActionFindFirstRecord.GetDirection:TPSCSearchType;
Begin
  Result:=stFirst;
End;

{-------------------------------------------}

Constructor TPSCFltBoxActionToggleFiltered.Create(AOwner: TComponent);
begin
  inherited;
  ImageIndex := Integer(Image_Filter);
  Caption := PSCConsts.Filtered;
  ShortCut := PSCShortCutFromKeyDef(cPSCFltBoxToggleFilteredKey);
end;

{-------------------------------------------}

Procedure TPSCFltBoxActionToggleFiltered.ExecuteTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
    If FilterSource<>nil then
      FilterSource.Filtered:=not FilterSource.Filtered;
end;

{-------------------------------------------}

Procedure TPSCFltBoxActionToggleFiltered.UpdateTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
  begin
    Self.Enabled := (FilterSource<>nil) and (Items.Count>0);
    Self.Visible := not (fboHideToggleFilter In GetRealOptions);
    Self.Checked := Self.Enabled and FilterSource.Filtered;
  end;
end;

{-------------------------------------------}

Constructor TPSCFltBoxActionSortDesc.Create(AOwner: TComponent);
begin
  Inherited;
  ImageIndex := Integer(Image_SortDesc);
  Caption := PSCConsts.SortDescending;
  ShortCut := PSCShortCutFromKeyDef(cPSCFltBoxSortDescKey);
end;

{-------------------------------------------}

Constructor TPSCFltBoxActionSortClear.Create(AOwner: TComponent);
begin
  Inherited;
  ImageIndex := -1;
  Caption := PSCConsts.ClearSortOrder;
  ShortCut := PSCShortCutFromKeyDef(cPSCFltBoxSortClearKey);
end;

{-------------------------------------------}

Procedure TPSCFltBoxActionSortDesc.UpdateTarget(Target: TObject);
Begin
  With GetFilterBox(Target) do
  begin
    Self.Enabled := (Items.Count>0) and (ItemIndex>=0);
    Self.Visible := not (fboHideSortOrder In GetRealOptions);
  end;
End;

{-------------------------------------------}

Procedure TPSCFltBoxActionSortAsc.UpdateTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
  begin
    Self.Enabled := (Items.Count>0) and (ItemIndex>=0);
    Self.Visible := not (fboHideSortOrder In GetRealOptions);
  end;
end;

{-------------------------------------------}

Procedure TPSCFltBoxActionSortClear.UpdateTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
  begin
    Self.Enabled := (FilterSource<>nil) and (FilterSource.OrderByItems.Count>0);
    Self.Visible := not (fboHideSortOrder In GetRealOptions);
  end;
end;

{-------------------------------------------}

Procedure TPSCFltBoxActionSortDesc.ExecuteTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
    SetCurrentFieldSortOrder(SortType_Descending,False);
end;

{-------------------------------------------}

Procedure TPSCFltBoxActionSortAsc.ExecuteTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
    SetCurrentFieldSortOrder(SortType_Ascending,False);
end;

{-------------------------------------------}

Procedure TPSCFltBoxActionSortClear.ExecuteTarget(Target: TObject);
begin
  With GetFilterBox(Target) do
    begin
      If FilterSource<>nil then
        FilterSource.OrderByItems.Clear;
      Invalidate;
    end
end;

{-------------------------------------------}

Procedure TPSCCustomFltBox.CreateActions;
begin
  inherited;
  CreateAction(TPSCFltBoxActionSortAsc);
  CreateAction(TPSCFltBoxActionSortDesc);
  CreateAction(TPSCFltBoxActionSortClear);
  CreateAction(TPSCFltBoxActionFindFirstRecord);
  CreateAction(TPSCFltBoxActionFindPriorRecord);
  CreateAction(TPSCFltBoxActionFindNextRecord);
  CreateAction(TPSCFltBoxActionFindLastRecord);
  CreateAction(TPSCFltBoxActionToggleFiltered);

  CreateAction(TPSCFltBoxActionSave);
  CreateAction(TPSCFltBoxActionOpen);
end;

{-------------------------------------------}

function TPSCCustomFltBox.AddActionsToPopup:Integer;
begin
  AddActionToPopup(TPSCFltBoxActionToggleFiltered,Result);
  AddActionToPopup(Nil,Result);
  Result:=inherited AddActionsToPopup;
  AddActionToPopup(Nil,Result);
  AddActionToPopup(TPSCFltBoxActionOpen,Result);
  AddActionToPopup(TPSCFltBoxActionSave,Result);
  
  AddActionToPopup(Nil,Result);
  AddActionToPopup(TPSCFltBoxActionSortAsc,Result);
  AddActionToPopup(TPSCFltBoxActionSortDesc,Result);
  AddActionToPopup(TPSCFltBoxActionSortClear,Result);
  AddActionToPopup(Nil,Result);
  AddActionToPopup(TPSCFltBoxActionFindFirstRecord,Result);
  AddActionToPopup(TPSCFltBoxActionFindPriorRecord,Result);
  AddActionToPopup(TPSCFltBoxActionFindNextRecord,Result);
  AddActionToPopup(TPSCFltBoxActionFindLastRecord,Result);


end;

{-------------------------------------------}

Function TPSCCustomFltBox.CanAddItem:Boolean;
begin
  Result:=(FilterSource<>nil) and (inherited CanAddItem);
end;

{-------------------------------------------}

function TPSCCustomFltBox.IsOverSortOrder(const P:TPoint):Boolean;
var
  MyItemPos:TPoint;
begin
  Result:=not (fboHideSortOrder in GetRealOptions);
  If Result then
  begin
    MyItemPos := MousePosToStrPos(P);
    Result:=(MyItemPos.Y>=0) and (P.X<ClientWidth) and (P.X>ClientWidth-16);
  end;
end;

{-------------------------------------------}

function TPSCCustomFltBox.IsURLPos(P: TPoint): boolean;
begin
  Result:=(inherited IsURLPos(P)) or IsOverSortOrder(P);
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBox.GetHintStr(
  Var HintStr: String; CursorPos: TPoint; Var CursorRect: TRect): Integer;
begin
  Result:=1;
  If IsOverSortOrder(CursorPos) then
  begin
    Result:=0;
    HintStr:=PSCConsts.ClickHeretoChangeSortOrder;
    CursorRect:=ItemRect(ItemIndex);
    CursorRect.Left:=CursorRect.Right-16;
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBox.CMHintShow(Var Message: TCMHintShow);
Begin
  With Message,HintInfo^ Do
    If HintControl <> Self Then
      Inherited
    Else
      Result := GetHintStr(HintStr,CursorPos,CursorRect);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
const
  CNextFieldSortType:Array[TPSCFieldSortType] of TPSCFieldSortType=
    (SortType_Ascending,SortType_Descending,SortType_Unsorted);
var
  MyIsOverSortOrder:Boolean;
begin
  MyIsOverSortOrder:=(ssLeft in Shift) and IsOverSortOrder(Point(X,Y));

  inherited;
  If MyIsOverSortOrder then
    SetCurrentFieldSortOrder(CNextFieldSortType[GetCurrentFieldSortOrder],
      not (ssCtrl in Shift));
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBox.SetItems2(V:TPSCFltItems);
begin
  inherited Items:=V;
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBox.GetItems2:TPSCFltItems;
begin
  Result:=TPSCFltItems(inherited Items);
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBox.PaintItem(AItem: Integer; ARect: TRect; AItemState:
  TOwnerDrawState);
var
  MyRect:TRect;
  MyFieldName:String;
  MySortOrder:Integer;
  MySortType:TPSCFieldSortType;
  MyImageIndex:Integer;
  MyDelta:Integer;
begin
  If FilterSource=nil then
  begin
    inherited;
    exit;
  end;

  If not (FilterSource.Filtered and Enabled and
    FilterSource.IncludeItemInFilter(FilterSource.Items[AItem]))
  then
    AItemState:=AItemState+[odDisabled];

  inherited;

  If not (fboHideSortOrder in GetRealOptions) then
  begin
    MyRect:=Rect(ARect.Right,ARect.Top,ARect.Right+GetRightFixedWidth,ARect.Bottom);

    Canvas.FillRect(MyRect);

    If (AItem>=0) and (AItem<Items.Count) then
    begin
      MyFieldName:=Items[AItem].DataField;
      MySortType:=FilterSource.GetFieldSortOrder(MyFieldName,MySortOrder);
      If MySortType<>SortType_Unsorted then
      begin
        If MySortType=SortType_Ascending then
          MyImageIndex:=FImageIndexes[Image_SortAscSmall]
        else
          MyImageIndex:=FImageIndexes[Image_SortDescSmall];
        MyDelta:=PSCMax(((MyRect.Bottom-MyRect.Top)-GetRightFixedWidth) div 2,0);
        InternalImages.Draw(Canvas,MyRect.Left,MyRect.Top+MyDelta,MyimageIndex);
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------}

Constructor TPSCCustomFltBld.Create(AOwner: TComponent);
Begin
  Inherited;
  FSQLHeadOper:=SPSCOperationFltAnd;
  FEvents:=TPSCEvents.Create;
  FVersion:=SPSCProductVerNo;
  FUpdateOnLoaded := cPSCDefUpdateOnLoaded;
  FTableNames := TStringList.Create;
  FSQL := TStringList.Create;
  FItems := TPSCFltItems.Create(Self,TPSCFltItem);
  FItems.OnUpdate := OnUpdate;
  FFiltered := cPSCDefFiltered;
  FHiddenFields := TStringList.Create;
  FFields := TPSCFltBoxFields.Create(Self,TPSCFltBoxField);
  FFields.OnChange:=FieldsChanged;
  FFieldParams := TPSCFltBoxFields.Create(Self,TPSCFltBoxField);
  FAdditionalFields := TPSCFltBoxFields.Create(Self,TPSCFltBoxField);
  FTemplates := CPSCUsedTemplatesClass.Create(Self,CPSCUsedTemplateClass);
  FTemplates.Assign(PSCGetDefaultTemplates);
  FEncloseInBrackets := cDefaultEncloseInBrackets;
  FAllowedUsageIDs := cPSCDefAllowedUsageIDs;
  FSQLHead := TStringList.Create;
  FSQLTail := TStringList.Create;
  FOrderByItems := TPSCOrderByItems.Create(Self,TPSCOrderByItem);
  FOrderByItems.OnUpdate := OnUpdate;
  FSQLOBTail := TStringList.Create;
  FWhereInc := cDefaultWhereInc;
  FExcludeBlobs := cDefaultExcludeBlobs;
  FActivateDataSet := cDefaultActivateDataSet;
  FMemorySlots := TPSCListMemorySlots.Create(Self,TPSCFltBoxMemorySlot);
  FMemorySlots.ListItemClass := TPSCFltItem;
  FFilterOptions:=cPSCDefaultFltBldOptions;
End;

{------------------------------------------------------------------------------}

Destructor TPSCCustomFltBld.Destroy;
Begin
  try
    FMemorySlots.Free;
    FTableNames.Free;
    FSQL.Free;
    FItems.Free;
    FHiddenFields.Free;
    FSQLHead.Free;
    FSQLTail.Free;
    FOrderByItems.Free;
    FSQLOBTail.Free;
    FAdditionalFields.Free;
    FFields.Free;
    FTemplates.Free;
    FFieldParams.Free;
    Inherited;
  except
    PSCErrorFmt(PSCConsts.ExceptionIn,['TPSCCustomFltBld.Destroy']);
  end;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFirstField: String;
Var
  Temp: IPSCStrings;
Begin
  Result := '';
  Temp := PSCCreateStringList;
  GetFieldsList(Temp);
  If Temp.Count = 0 Then
    exit;
  Result := Temp[0];
End;

{------------------------------------------------------------------------------}

Function TPSCFltBoxTemplates.ItemWithCaptionID(Const ACategory,
  ACaptionID: String): TPSCFltBoxTemplate;
Var
  i: Integer;
Begin
  I := IndexOfCaptionID(ACategory,ACaptionID);
  If I >= 0 Then
    Result := TPSCFltBoxTemplate(Items[I])
  Else
    Result := Nil;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltItem.Assign(Source: TPersistent);
Begin
  If Source is TPSCFltItem then
  begin
    Collection.BeginUpdate;
    try
      DateTimeParts  := TPSCFltItem(Source).DateTimeParts;
      DataField      := TPSCFltItem(Source).DataField    ;
      TemplCat       := TPSCFltItem(Source).TemplCat     ;
      Template       := TPSCFltItem(Source).Template     ;
      Operation      := TPSCFltItem(Source).Operation    ;
      IndentLev      := TPSCFltItem(Source).IndentLev    ;
      Checked        := TPSCFltItem(Source).Checked      ;
      Value          := TPSCFltItem(Source).Value        ;
      Value1         := TPSCFltItem(Source).Value1       ;
      Value2         := TPSCFltItem(Source).Value2       ;

      ItemOptions     := TPSCFltItem(Source).ItemOptions       ;
      Options     := TPSCFltItem(Source).Options       ;
    finally
      Collection.EndUpdate;
    end;
  end;  
End;

{------------------------------------------------------------------------------}

Function TPSCFltItem.GetValue: Variant;
Begin
  Result := Params.FieldValues[SPSCValue];
End;

{------------------------------------------------------------------------------}

procedure TPSCFltItem.InternalSetValue(const AName:String;const AValue:Variant);
var
  MyField: TPSCListParam;
begin
  MyField:=TPSCListParam(Params.ItemByName(AName));
  If MyField<>nil then
  begin
    MyField.Value := AValue;
  end;  
end;

{------------------------------------------------------------------------------}

Procedure TPSCFltItem.SetValue(V: Variant);
Begin
  InternalSetValue(SPSCValue,V);
End;

{------------------------------------------------------------------------------}

Function TPSCFltItem.GetValue1: Variant;
Begin
  Result := Params.FieldValues[SPSCValue1];
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltItem.SetValue1(V: Variant);
Begin
  InternalSetValue(SPSCValue1,V);
End;

{------------------------------------------------------------------------------}

Function TPSCFltItem.GetValue2: Variant;
Begin
  Result := Params.FieldValues[SPSCValue2];
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltItem.SetValue2(V: Variant);
Begin
  InternalSetValue(SPSCValue2,V);
End;

{------------------------------------------------------------------------------}

Function TPSCFltItem.GetDataField: String;
Begin
  Result := Params.FieldValues[PSCRemoveColons(SPSCFieldParam)];
End;

{------------------------------------------------------------------------------}

Function TPSCFltItem.GetOperation: String;
Begin
  Result := Params.FieldValues[PSCRemoveColons(SPSCOpParam)];
End;

{------------------------------------------------------------------------------}

Function TPSCFltItem.GetTemplate: String;
Begin
  Result := Params.FieldValues[PSCRemoveColons(SPSCTemplateParam)];
End;

{------------------------------------------------------------------------------}

Function TPSCFltItem.GetListParamClass: TPSCListParamClass;
begin
  Result:=TPSCFltParam;
end;

{------------------------------------------------------------------------------}

Procedure TPSCFltItem.CaptionChanged;
Var
  I: Integer;
  NewDataType: TPSCFieldType;
  TemplItem: TPSCFltBoxTemplate;
  ADataField: String;
  MyParam:TPSCListParam;
Begin
  ADataField := DataField;

  With GetFltBld Do
  begin
    TemplItem :=
      Templates.ItemWithCaptionID(TemplCat,Template);
  end;

  If TemplItem <> Nil Then
    begin
      Caption := TemplItem.Caption + ' ' + SPSCOpParam;
      MyParam :=
        TPSCListParam(Params.ItemByName(PSCRemoveCharSet([':'],
          SPSCTemplateParam)));
      If MyParam<>nil then
        Myparam.DisplayValue:=PSCRemoveSpChars(TemplItem.CaptionForUser);
    end
  Else
    Caption := '';

  Params.BeginUpdate;

  Try
    Inherited;

    If (Params.Count > 0) And (TemplItem <> Nil) Then
      Begin
        NewDataType:=TemplItem.ParamsType;
        If NewDataType=FT_UNK then
          NewDataType := GetFltBld.GetFieldType(ADataField);
        For I := 0 To Params.Count - 1 Do
          With TPSCListParam(Params[I]) Do
            If (Not PSCIsSystemParam(Name)) Then
              Begin
                ValuePrefix := TemplItem.ValuePrefix;
                ValueSuffix := TemplItem.ValueSuffix;
                If DataType <> NewDataType Then
                  SafeSetDataType(NewDataType);

                If not (ftoValueList in TemplItem.Options) and IsArray then
                  Clear;
              End;
      End;
  Finally
    Params.EndUpdate;
  End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltItem.SetDateTimeParts(AValue: TPSCDateTimeParts);
begin
  If FDateTimeParts<>AValue then
  begin
    FDateTimeParts:=AValue;
    Changed(False);
  end;
end;

{------------------------------------------------------------------------------}

Procedure TPSCFltItem.SetTemplate(Const V: String);
Begin
  Collection.BeginUpdate;
  Try
    SetParamValueAndStyle(SPSCTemplateParam,PSCRemoveSpChars(V),False, '');
    CaptionChanged;
  Finally
    Collection.EndUpdate;
  End;
End;

{------------------------------------------------}

Function TPSCFltItem.GetFltBld: TPSCCustomFltBld;
Begin
  Result := (Collection As TPSCFltItems).GetFltBld;
End;

{------------------------------------------------}

Function TPSCFltBoxField.GetFltBld: TPSCCustomFltBld;
Begin
  Result := (Collection As TPSCFltBoxFields).GetFltBld;
End;

{------------------------------------------------}

Function TPSCFltBoxFields.GetFltBld: TPSCCustomFltBld;
Begin
  Result := TPSCCustomFltBld(PSCFindOwnerWithClass(Self,TPSCCustomFltBld));
End;

{------------------------------------------------}

Function TPSCOrderByItem.GetFltBld: TPSCCustomFltBld;
Begin
  Result := (Collection As TPSCOrderByItems).GetFltBld;
End;

{------------------------------------------------}

Function TPSCOrderByItems.GetItem(Index: Integer): TPSCOrderByItem;
Begin
  Result := TPSCOrderByItem(Inherited Items[Index]);
End;

{------------------------------------------------}

Procedure TPSCOrderByItems.SetItem(Index: Integer; V: TPSCOrderByItem);
Begin
  Inherited Items[Index] := V;
End;

{------------------------------------------------}

Function TPSCOrderByItems.GetFltBld: TPSCCustomFltBld;
Begin
  Result := TPSCCustomFltBld(PSCFindOwnerWithClass(Self,TPSCCustomFltBld));
End;

{--------------------------------------------}

function TPSCFltItems.GetHiddenProps:TArray<string>;
const
  MyArray: TArray<String> = [
    'Caption',
    'UserData',
    'Collection',
    'DataObject',
    'Tag',
    'Index',
    'DisplayName',
    'UserFields'
  ];
begin
  result:=MyArray;
end;

{------------------------------------------------}

Procedure TPSCFltItems.DoQuoteStr(Sender: TObject; Field: TPSCField; Var
  QuotedStr: String);
begin
  inherited;
  If GetFltBld<>nil then
    GetFltBld.DoQuoteStr(Field,QuotedStr);
end;

{------------------------------------------------}

Procedure TPSCFltItems.GetAsString(Sender: TObject; Field: TPSCField;
  Var ResultStr: String; ForDisplay: boolean);
begin
  inherited;
  If GetFltBld<>nil then
    GetFltBld.DoGetAsString(Field,ResultStr,ForDisplay);
end;

{------------------------------------------------}

function TPSCFltItems.GetRealSQLDateTimeFormat:TPSCDateTimeFormat;
begin
  If GetFltBld=nil then
    Result:=inherited GetRealSQLDateTimeFormat
  else
    begin
      FDummySQLDateTimeFormat.Assign(SQLDateTimeFormat);
      GetFltBld.UpdateDateTimeFormat(FDummySQLDateTimeFormat,True);
      Result:=FDummySQLDateTimeFormat;
    end;
end;

{------------------------------------------------}

procedure TPSCCustomFltBld.UpdateDateTimeFormat(AFormat:TPSCDateTimeFormat;
  AForSQL:Boolean);
begin
end;

{------------------------------------------------}

Constructor TPSCFltItems.Create(AOwner: TPersistent; ItemClass: TPSCNamedItemClass);
begin
  inherited;
  FDummyFilterDateTimeFormat:=TPSCDateTimeFormat.Create(Self,PSCGetDefaultDateTimeFormat);
  FDummySQLDateTimeFormat:=TPSCDateTimeFormat.Create(Self,PSCGetDefaultDateTimeFormat);
end;

{------------------------------------------------}

Destructor TPSCFltItems.Destroy;
begin
  inherited;
  FDummyFilterDateTimeFormat.Free;
  FDummySQLDateTimeFormat.Free;
end;

{------------------------------------------------}

function TPSCFltItems.GetRealFilterDateTimeFormat:TPSCDateTimeFormat;
begin
  If GetFltBld=nil then
    Result:=inherited GetRealFilterDateTimeFormat
  else
    begin
      FDummyFilterDateTimeFormat.Assign(FilterDateTimeFormat);
      GetFltBld.UpdateDateTimeFormat(FDummyFilterDateTimeFormat,False);
      Result:=FDummyFilterDateTimeFormat;
    end;
end;

{------------------------------------------------}

Function TPSCFltItems.GetFltBld: TPSCCustomFltBld;
Begin
  Result := TPSCCustomFltBld(PSCFindOwnerWithClass(Self,TPSCCustomFltBld));
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltItem.SetDataField(Const V: String);
Begin
  Collection.BeginUpdate;
  Try
    SetParamValueAndStyle(SPSCFieldParam,V,True,GetFltBld.GetFieldDisplayValue(V));
    TemplCat := GetFltBld.GetTemplCategory(V);
    Template := GetFltBld.GetFirstTemplate(V,False);
  Finally
    Collection.EndUpdate;
  End;
End;

{------------------------------------------------------------------------------}

Function TPSCFltItem.IsDateTimePartsStored:Boolean;
begin
  Result:=FDateTimeParts<>CPSCDefDateTimeParts;
end;

{------------------------------------------------------------------------------}

Function TPSCFltItem.IsOperationStored:Boolean;
begin
  Result:=Operation<>SPSCOperationFltAnd;
end;

{------------------------------------------------------------------------------}

Constructor TPSCFltItem.Create(ACollection: TCollection);
Begin
  Inherited;
  Operation := SPSCOperationFltAnd;
  FDateTimeParts := CPSCDefDateTimeParts;
End;

{------------------------------------------------------------------------------}

Procedure TPSCFltItem.SetOperation(Const V: String);
var
  MyDisplayValue:String;
Begin
  If GetFltBld<>nil then
    MyDisplayValue:=GetFltBld.FltToDispOperation(V)
  else
    MyDisplayValue:='';
    
  SetParamValueAndStyle(SPSCOpParam,V,False, MyDisplayValue);
End;

{------------------------------------------------------------------------------}

Destructor TPSCFltItem.Destroy;
Begin
  Inherited;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetItemFilterStr(Item: TPSCFltItem): String;
Var
  TemplItem: TPSCFltBoxTemplate;
  MyFilter:String;
  MyProc:TPSCFltTemplateProc;
  MyProcData:TPSCFltTemplateProcData;
Begin
  try
    Result := '';
    With Item Do
      Begin
        TemplItem := Templates.ItemWithCaptionID(TemplCat,Template);
        If TemplItem = Nil Then
          exit;
        If TemplItem.FilterKind=tfkString then
          MyFilter:=TemplItem.Filter
        else
          begin
            MyProc:=PSCGetFltBoxRegister.GetTemplateProc(TemplItem.Filter);
            If not Assigned(MyProc) then
              PSCError(PSCConsts.ErrBadFilter);
            MyProcData:=TPSCFltTemplateProcData.Create;
            try
              MyProcData.Item:=Item;
              MyProcData.FilterBuilder:=Self;
              MyProc(MyProcData);
              MyFilter:=MyProcData.Result;
            finally
              MyProcData.Free;
            end;
          end;

        Result := GetPreparedFilterStr(Item,MyFilter,DataField,True,Params);

        If fboAddConditionBrackets in GetRealFilterOptions then
        begin
          Result:='('+Result+')';
        end;

        If FTargetIsSQL Then
          Result := PSCEqNullToIsNull(Result);
      End;
  finally
    If Assigned(FOnGetItemFilterStr) then
      FOnGetItemFilterStr(Self,Item,Result);
  end;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetSQLExpression: String;
Begin
  Result := GetSQLFilterStrEx(True);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetSQLFilterStr: String;
Begin
  Result := GetSQLFilterStrEx(False);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetSQLFilterStrEx(UseExpressionProp: boolean): String;
Var
  SavedForceBools,SavedForceDateTime: boolean;
Begin
  SavedForceBools := GetItems.FForceSQLBooleans;
  SavedForceDateTime := GetItems.ForceSQLDateTime;
  GetItems.ForceSQLDateTime := True;
  GetItems.FForceSQLBooleans := True;
  FTargetIsSQL := True;
  Try
    If UseExpressionProp Then
      Result := PSCTrim(Expression)
    Else
      Result := PSCTrim(FilterStr);
  Finally
    GetItems.ForceSQLDateTime := SavedForceDateTime;
    GetItems.FForceSQLBooleans := SavedForceBools;
    FTargetIsSQL := False;
  End;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetSQL: TStrings;
Var
  Temp,TempFilterStr: String;

  Procedure AddToTemp(Const S: String);
  Begin
    If S <> '' Then
      Temp := PSCAddCRLF(Temp) + S;
  End;

Begin
  Temp := GetSQLHeadText;

  TempFilterStr := GetSQLFilterStr;

  If TempFilterStr <> '' Then
    Begin
      If (WhereInc = wiYes) Or ((WhereInc = wiAuto)
        And (Not PSCSQLContainsWhere(Temp))) Then
        Begin
          AddToTemp('WHERE'); //don't resource
          AddToTemp(TempFilterStr);
        End
      Else
        Begin
          If PSCTrim(FSQLHeadOper) <> '' Then
            Begin
              AddToTemp(PSCTrim(FSQLHeadOper));
              AddToTemp('(' + TempFilterStr + ')');
            End
          Else
            AddToTemp(TempFilterStr);
        End;
    End;
  //----------------

  AddToTemp(SQLTail.Text);
  AddToTemp(GetOrderByText);
  AddToTemp(SQLOBTail.Text);
  FSQL.Text := Temp;
  Result := FSQL;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFilterStr: String;
Begin
  If Filtered Then
    Begin
      Result := GetExpression;

      If AdditionalFilter <> '' Then
        Begin
          If Result = '' Then
            Result := AdditionalFilter
          Else
            Result := AdditionalFilter + ' ' + SPSCOperationFltAnd + ' ' +
              Result;
        End;
    End
  Else
    Result := AdditionalFilter;

  If ((Result <> FOldFilterStr) And (Not FTargetIsSQL))
    Or ((Result <> FOldSQLStr) And FTargetIsSQL) Then
    Begin
      FilterStrChanged := True;
      If FTargetIsSQL Then
        FOldSQLStr := Result
      Else
        FOldFilterStr := Result;
    End;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetExpression: String;
begin
  Result:=GetExpressionEx(Items,False,nil);
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBox.GetItemForDisplay(AItem:TPSCListItem):String;
var
  MySaveVisible:Boolean;
  MyParam:TPSCListParam;
begin
  With AItem do
  begin
    MySaveVisible:=True;
    MyParam:=TPSCListParam(Params.ItemByName(PSCRemoveCharSet([':'],SPSCOpParam)));
    If MyParam<>nil then
    begin
      MySaveVisible:=MyParam.Visible;
      MyParam.FVisible:=False;
    end;
    try
      Result:=GetWithRemovedTags(AItem);
    finally
      If MyParam<>nil then
        MyParam.FVisible:=MySaveVisible;
    end;
  end;
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBox.GetFilterForDisplay:String;
begin
  If FilterSource=nil then
    Result:=''
  else
    Result:=FilterSource.GetExpressionEx(FilterSource.Items,True,GetItemForDisplay);
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetExpressionEx(AItems:TPSCFltItems;
  AForDisplay:Boolean;AProc:TPSCListItemToStrProc): String;
type
  TPSCExpressionItemKind=(eikCondition,eikOpenBracket,
    eikCloseBracket,eikOperation,eikEmpty,eikUnknown);
Var
  OpenedBrackets: Integer;
  IDelta: integer;
  SavedDecimalSeparator: Char;
  i:Integer;
  MyResult:IPSCStrings;
  MyOperation:String;

  procedure AddToResult(const S:String;AKind:TPSCExpressionItemKind);
  begin
    MyResult.AddObject(S,TPSCValuesContainer.Create(Integer(AKind)));
  end;

  Procedure UpdateIndentLev(Num: Integer);
  Var
    C: Char;
    i:Integer;
    AKind:TPSCExpressionItemKind;
  Begin
    If Num = 0 Then
      exit;
    If Num > 0 Then
      begin
        C := '(';
        AKind:=eikOpenBracket;
      end
    Else
      begin
        C := ')';
        AKind:=eikCloseBracket;
      end;
    inc(OpenedBrackets,Num);
    for i:=1 to Abs(Num) do
      AddToResult(C,AKind);
  End;

  Function RemoveEmptyCondition:Boolean;
  var
    i:Integer;
    MyThisKind:TPSCExpressionItemKind;
    MyNextKind:TPSCExpressionItemKind;
    MyPrevKind:TPSCExpressionItemKind;
  begin
    With MyResult do
      for i:=0 to Count do
      begin
        If i+1<Count then
          MyNextKind:=TPSCExpressionItemKind(TPSCValuesContainer(Objects[i+1]).IntValue)
        else
          MyNextKind:=eikUnknown;

        If i>0 then
          MyPrevKind:=TPSCExpressionItemKind(TPSCValuesContainer(Objects[i-1]).IntValue)
        else
          MyPrevKind:=eikUnknown;

        If i<Count then
          MyThisKind:=TPSCExpressionItemKind(TPSCValuesContainer(Objects[i]).IntValue)
        else
          MyThisKind:=eikUnknown;

        If (MyThisKind=eikEmpty) and (MyNextKind=eikOperation) then
        begin
          Delete(i);
          Delete(i);
          Result:=True;
          exit;
        end;

        If (MyPrevKind=eikOpenBracket) and (MyNextKind=eikCloseBracket) and
           (MyThisKind=eikEmpty)
        then
          begin
            Delete(i+1);
            Delete(i-1);
            Result:=True;
            exit;
          end;

        If (MyPrevKind=eikOperation) and (MyNextKind=eikCloseBracket) and
           (MyThisKind=eikEmpty)
        then
          begin
            Delete(i);
            Delete(i-1);
            Result:=True;
            exit;
          end;

        If (MyPrevKind=eikOpenBracket) and (MyNextKind=eikCloseBracket) and
           (MyThisKind=eikCondition)
        then
          begin
            Delete(i+1);
            Delete(i-1);
            Result:=True;
            exit;
          end;

        If (MyPrevKind=eikOperation) and (MyNextKind=eikUnknown) and
           (MyThisKind=eikEmpty)
        then
          begin
            Delete(i);
            Delete(i-1);
            Result:=True;
            exit;
          end;

      end;
    Result:=False;
  end;

  Procedure RemoveEmptyConditions;
  begin
    While RemoveEmptyCondition do;
  end;

  Function MyGetItemOperation(AItem:TPSCFltItem):String;
  begin
    If AForDisplay then
      Result:=FltToDispOperation(AItem.Operation)
    else
      Result:=AItem.Operation;
  end;

  Function MyGetItemFilterStr(AItem:TPSCFltItem):String;
  begin
    If Assigned(AProc) then
      Result:=AProc(AItem)
    else
      Result:=GetItemFilterStr(AItem);
  end;

begin
  Result := '';
  SavedDecimalSeparator := PSCGetDecimalSeparator;
  If Not UseLocalSettings Then
    PSCSetDecimalSeparator('.');
  MyResult:=PSCCreateStringList(ioOwned);
  Try
    OpenedBrackets := 0;
    IDelta := 0;
    MyOperation := '';
    For i := 0 To AItems.Count - 1 Do
      With AItems[i] Do
      begin
        If i>0 then
          begin
            IDelta := IndentLev - AItems[i-1].IndentLev;
            If IDelta < 0 Then
              UpdateIndentLev(IDelta);
          end;

        AddToResult(MyOperation,eikOperation);

        If IDelta > 0 Then
          UpdateIndentLev(IDelta);

        If IncludeItemInFilter(AItems[i]) then
          AddToResult(MyGetItemFilterStr(AItems[i]),eikCondition)
        else
          AddToResult('_',eikEmpty);

        MyOperation:=' ' + MyGetItemOperation(AItems[i]) + ' ';
      end;

    If OpenedBrackets > 0 Then
      UpdateIndentLev(-OpenedBrackets);

    RemoveEmptyConditions;

    for i:=0 to MyResult.Count-1 do
      Result:=Result+Myresult[i];
  Finally
    PSCSetDecimalSeparator(SavedDecimalSeparator);
  End;
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetUseLocalSettings(AValue: Boolean);
Begin
  If FUseLocalSettings = AValue Then
    Exit;
  FUseLocalSettings := AValue;
  Items.UseLocalSettings := AValue;
  Changed;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.SetItems(V: TPSCFltItems);
Begin
  Items.Assign(V);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBox.GetParamLookupDef(Item: TPSCListItem;
  Param: TPSCListParam): TPSCLookupDef;
Begin
  If FilterSource=nil then
    Result:=inherited GetParamLookupDef(Item,Param)
  else
    Result := FilterSource.GetParamLookupDef(TPSCFltItem(Item),Param);
  If Assigned(OnGetLookupDef) Then
    OnGetLookupDef(Self,Item,Param,Result);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetParamLookupDef(Item: TPSCFltItem;
  Param: TPSCListParam): TPSCLookupDef;
Var
  FieldLookupDef: TPSCLookupDef;
Begin
  Result := Param.GetLookupDef;
  FieldLookupDef := GetFieldLookupDef(TPSCFltItem(Item).DataField);
  With Result Do
    Begin
      If DataSet = Nil Then
        DataSet := FieldLookupDef.DataSet;
      If KeyField = '' Then
        KeyField := FieldLookupDef.KeyField;
      If DisplayField = '' Then
        DisplayField := FieldLookupDef.DisplayField;
      If DisplayField = '' Then
        DisplayField := KeyField;
      If GridFields='' then
        GridFields:=FieldLookupDef.GridFields;
    End;
  If Assigned(OnGetLookupDef) Then
    OnGetLookupDef(Self,Item,Param,Result);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBox.GetParamPickType(Item: TPSCListItem; Param: TPSCListParam):
  TPSCPickType;
Begin
  If FilterSource=nil then
    Result := inherited GetParamPickType(Item,Param)
  else
    Result := FilterSource.GetParamPickType(TPSCFltItem(Item),Param);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFieldPickType(Const FieldName: String): TPSCPickType;

  Function GetFromFields(Flds: TPSCFltBoxFields): TPSCPickType;
  Var
    FilterField: TPSCFltBoxField;
  Begin
    FilterField := Flds.FieldByName(FieldName);
    If FilterField <> Nil Then
      Result := FilterField.PickType
    Else
      Result := ptAuto;
  End;

Begin
  Result := ptAuto;
  If PSCTrim(FieldName) = '' Then
    exit;

  Result := GetFromFields(AdditionalFields);
  If Result <> ptAuto Then
    Exit;

  Result := GetFromFields(FFieldParams);
  If Result <> ptAuto Then
    exit;

  Result := GetFromFields(FFields);
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFieldNumber(Const FieldName: String): Integer;

  Function GetFromFields(Flds: TPSCFltBoxFields; ATakeIndex:Boolean): Integer;
  Var
    FilterField: TPSCFltBoxField;
  Begin
    FilterField := Flds.FieldByName(FieldName);
    If FilterField <> Nil Then
      begin
        Result := FilterField.FieldNumber;
        If ATakeIndex and (Result=-1) then
          Result:=FilterField.Index;
      end
    Else
      Result := -1;
  End;

  Procedure GetFromAllFields(ATakeIndex:Boolean);
  Begin
    Result := GetFromFields(AdditionalFields,ATakeIndex);
    If Result <> -1 Then
      Exit;

    Result := GetFromFields(FFieldParams,ATakeIndex);
    If Result <> -1 Then
      exit;

    Result := GetFromFields(FFields,ATakeIndex);
  End;

Var
  Field: TField;
Begin
  Result := -1;
  If PSCTrim(FieldName) = '' Then
    exit;

  GetFromAllFields(False);

  If Result = -1 then
    Result := GetFromFields(FFields,True);

  If (Result = -1) and (DataSet<>nil) Then
  begin
    Field := DataSet.FindField(FieldName);
    If Field<>nil then
      Result:=Field.Index;
  end;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetParamPickType(Item: TPSCListItem; Param: TPSCListParam):
  TPSCPickType;
Begin
  Result := Param.PickType;

  If Result = ptAuto Then
    Result := GetFieldPickType(TPSCFltItem(Item).DataField);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBox.GetParamPickList(Item: TPSCListItem; Param: TPSCListParam;
  const PickList: IPSCStrings);
Begin
  If FilterSource=nil then
    inherited
  else
    FilterSource.GetParamPickList(TPSCFltItem(Item),Param,PickList);
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.GetParamPickList(Item: TPSCFltItem; Param: TPSCListParam;
  const PickList: IPSCStrings);
Begin
  If Param.PickList.Count > 0 Then
    PickList.Assign(PSCCreateStringsAdapter(Param.PickList))
  Else
    PickList.Clear;

  If PickList.Count = 0 Then
    GetFieldPickList(TPSCFltItem(Item).DataField,PickList);
End;

{------------------------------------------------------------------}

function TPSCCustomFltBld.ParamHasPickList(Item: TPSCFltItem; Param: TPSCListParam):
  boolean;
Var
  PickList: IPSCStrings;
Begin
  PickList := PSCCreateStringList;
  GetParamPickList(Item,Param,PickList);
  Result := PickList.Count > 0;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFieldIDValue(Const FieldName,DisplayValue: String):
  Variant;
Var
  LookupDef: TPSCLookupDef;
Begin
  Result := Null;
  LookupDef := GetFieldLookupDef(FieldName);
  If Not PSCIsValidLookupDef(LookupDef) Then
    exit;
  With LookupDef Do
    Begin
      If (DisplayField = '') Or (DisplayField = KeyField) Then
        exit;
      If DataSet.Locate(DisplayField,DisplayValue, []) Then
        Result := DataSet.FieldByName(KeyField).Value;
    End;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.IncludeItemInFilter(Item: TPSCFltItem): boolean;
Var
  i: Integer;
  TemplItem: TPSCFltBoxTemplate;
Begin
  Result := False;
  With Item Do
    Begin
      If Not Checked Then
        exit;
      If (DataField = '') Or (Operation = '') Then
        exit;

      TemplItem := Templates.ItemWithCaptionID(TemplCat,Template);

      If (TemplItem = Nil) Or (TemplItem.Filter = '') Then
        exit;

      For i := 0 To Params.Count - 1 Do
        If Params[i].IsNull Then
          exit;
    End;
  Result := True;
End;

{------------------------------------------------------------------------------}

Function TPSCCustomFltBld.FindRecord(ADirection: TPSCSearchType): Boolean;
var
  MyExprEval:TPSCExprEval;
begin
  If DataSet=nil then
  begin
    Result:=False;
    exit;
  end;
  MyExprEval:=TPSCExprEval.Create(nil);
  try
    With MyExprEval do
    begin
      DataSet:=Self.DataSet;
      Expression:=Self.Expression;
      SearchInLoop:=True;
      Result:=FindRecordEx(ADirection);
    end;
  finally
    MyExprEval.Free;
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.EnumUsedFields(const AFields:IPSCStrings;
  AMustBeInFilter:Boolean=False);
Var
  i: Integer;
  MyStrings:IPSCStrings;
begin
  MyStrings:=PSCCreateSortedStringList;
  MyStrings.Duplicates:=dup_Ignore;
  For i := 0 To Items.Count - 1 Do
    Begin
      If (not AMustBeInFilter) or IncludeItemInFilter(Items[i]) then
        MyStrings.Add(Items[i].DataField);
    End;
  AFields.AddStrings(MyStrings);
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.RemoveUsedFields(const FieldsList: IPSCStrings);
Var
  i: Integer;
  Index: Integer;
Begin
  For i := 0 To Items.Count - 1 Do
    Begin
      Index := FieldsList.IndexOf(TPSCFltItem(Items[i]).DataField);
      If Index >= 0 Then
        FieldsList.Delete(Index);
    End;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.GetFirstUnusedField: String;
Var
  Temp: IPSCStrings;
Begin
  Result := GetFirstField;
  Temp := PSCCreateStringList;
  GetFieldsList(Temp);
  RemoveUsedFields(Temp);
  If Temp.Count = 0 Then
    exit;
  Result := Temp[0];
End;

{------------------------------------------------------------------------------}

Function TPSCCustomFltBld.IsTemplatesStored:Boolean;
begin
  Result:=fboTemplatesStored in GetRealFilterOptions;
end;

{------------------------------------------------------------------------------}

Function TPSCCustomFltBld.IsSQLHeadOperStored:Boolean;
begin
  Result:=FSQLHeadOper<>SPSCOperationFltAnd;
end;

{------------------------------------------------------------------------------}

Function TPSCCustomFltBld.ConditionBracketsNeeded:Boolean;
var
  i:Integer;
begin
  Result:=True;
  for i:=0 to Items.Count-1 do
    With Items[i] do
    begin
      If PSCCompareText(Operation,SPSCOperationFltOrNot) = 0 Then
        exit;
      If PSCCompareText(Operation,SPSCOperationFltAndNot) = 0 Then
        exit;
    end;
  Result:=False;
end;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.FltToDispOperation(Const Operation: String): String;
Begin
  If PSCCompareText(Operation,SPSCOperationFltAnd) = 0 Then
    Result := PSCConsts.OperationDispAnd
  Else
  If PSCCompareText(Operation,SPSCOperationFltOr) = 0 Then
    Result := PSCConsts.OperationDispOr
  Else
  If PSCCompareText(Operation,SPSCOperationFltOrNot) = 0 Then
    Result := PSCConsts.OperationDispOrNot
  Else
  If PSCCompareText(Operation,SPSCOperationFltAndNot) = 0 Then
    Result := PSCConsts.OperationDispAndNot
  Else
    Result := Operation;
End;

{------------------------------------------------------------------------------}

Function TPSCFltItems.GetItem(AIndex:Integer):TPSCFltItem;
begin
  Result:=TPSCFltItem(inherited Items[AIndex]);
end;

{------------------------------------------------------------------------------}

procedure TPSCFltItems.SetItem(AIndex:Integer;V:TPSCFltItem);
begin
  inherited Items[AIndex]:=V;
end;

{------------------------------------------------------------------------------}

Function TPSCFltItems.IsPrmVisible(Item: TPSCListItem;
  Param: TPSCListParam): boolean;
Begin
  Result := Inherited IsPrmVisible(Item,Param);
  If Result Then
    Result := Not (PSCIsPrmName(Param,SPSCOpParam) And
      (Item.Index = Count - 1));
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomFltBld.GetOperationsForDisplay(const Strings: IPSCStrings);
var
  i:Integer;
Begin
  GetOperations(Strings);
  With Strings Do
    for i:=0 to Count-1 do
      Strings[i]:=FltToDispOperation(Strings[i]);
end;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.GetOperations(const Strings: IPSCStrings);
Begin
  With Strings Do
    Begin
      Clear;
      Add(SPSCOperationFltAnd);
      Add(SPSCOperationFltOr);

      Add(SPSCOperationFltAndNot);
      Add(SPSCOperationFltOrNot);
    End;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomFltBld.Loaded;
Begin
  Inherited;
  try
    Changed;
  except
  end;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBox.AddItemTextStored: boolean;
Begin
  Result := AddItemText <> PSCConsts.ClickHereToAddCond;
End;

{------------------------------------------------------------------------------}

Function TPSCFltBoxMemorySlot.GetListItemsClass: TPSCListItemsClass;
Begin
  Result := TPSCFltItems;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.IsFieldsStored: boolean;
Begin
  Result := Fields.Count > 0;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.IsFieldParamsStored: boolean;
Begin
  Result := FieldParams.Count > 0;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.IsOrderByItemsStored: boolean;
Begin
  Result := OrderByItems.Count > 0;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.IsItemsStored: boolean;
Begin
  Result := Items.Count > 0;
End;

{------------------------------------------------------------------------------}

function TPSCCustomFltBld.IsMemorySlotsStored: boolean;
Begin
  Result := MemorySlots.Count > 0;
End;

{------------------------------------------------------------------------------}

procedure PSCEnumDBGridColumns(AGrid:TPSCDBGrid;const AStrings:IPSCStrings);
var
  i:Integer;
begin
  for i:=0 to AGrid.Columns.Count-1 do
    AStrings.Add(AGrid.Columns[i].FieldName);
end;

{------------------------------------------------------------------------------}

function PSCDBGridColumnsToCommaText(AGrid:TPSCDBGrid):String;
var
  AStrings:IPSCStrings;
begin
  AStrings:=PSCCreateStringList;
  PSCEnumDBGridColumns(AGrid,AStrings);
  Result:=PSCTrimSeparators(AStrings.GetCommaText,[',',#13,#10]);
end;

{-------------------------------------------}

Procedure PSCSetDataSetSQL(DataSet: TDataSet; SQL: TStrings; Activate: boolean;
 const ASQLPropName:String='SQL');//don't resource
Var
  OldActive: boolean;
Begin
  If DataSet = Nil Then
    exit;
  If Not PSCPropertyExists(DataSet, ASQLPropName) Then
    exit;

  If DataSet.Active And
    (PSCGetStringsPropAsText(DataSet, ASQLPropName) = SQL.Text) Then
    exit;

  DataSet.DisableControls;
  Try
    OldActive := DataSet.Active;
    DataSet.Active := False;
    PSCAssignStringsProp(DataSet, ASQLPropName, SQL);
    If Activate Then
      OldActive := True;
    DataSet.Active := OldActive;
  Finally
    DataSet.EnableControls;
  End;
End;

{------------------------------------------------------------------------------}

Procedure TPSCCustomFltBld.DoQuoteStr(AField:TPSCField;var AQuotedStr:String);
begin
  If Assigned(FOnQuoteStr) then
    FOnQuoteStr(Self,AField,AQuotedStr);
end;

{------------------------------------------------------------------}

Procedure TPSCFltItem.InternalReadValue(Reader: TReader);
Begin
  Value := PSCReadVariant(Reader);
End;

{------------------------------------------------------------------}

Procedure TPSCFltItem.InternalReadValue1(Reader: TReader);
Begin
  Value1 := PSCReadVariant(Reader);
End;

{------------------------------------------------------------------}

Procedure TPSCFltItem.InternalReadValue2(Reader: TReader);
Begin
  Value2 := PSCReadVariant(Reader);
End;

{------------------------------------------------------------------}

function TPSCFltItem.GetItemOptions:TPSCFltItemOptions;

  procedure MyIncludeReadOnly(const AName:String;AOption:TPSCFltItemOption);
  var
    MyField:TPSCListParam;
  begin
    MyField:=TPSCListParam(Params.ItemByName(AName));
    If (MyField<>nil) and MyField.ReadOnly then
      Result:=Result+[AOption];
  end;

begin
  Result:=[];
  MyIncludeReadOnly('Field',FltItem_ReadOnly_Field);
  MyIncludeReadOnly('Template',FltItem_ReadOnly_Template);
  MyIncludeReadOnly('Operation',FltItem_ReadOnly_Operation);
  MyIncludeReadOnly('Value',FltItem_ReadOnly_Value);
  MyIncludeReadOnly('Value1',FltItem_ReadOnly_Value1);
  MyIncludeReadOnly('Value2',FltItem_ReadOnly_Value2);
end;

{------------------------------------------------------------------}

function TPSCFltItem.IsItemOptionsStored:Boolean;
begin
  Result:=ItemOptions<>[];
end;

{------------------------------------------------------------------}

procedure TPSCFltItem.SetItemOptions(v:TPSCFltItemOptions);

  procedure MyIncludeReadOnly(const AName:String;AOption:TPSCFltItemOption);
  var
    MyField:TPSCListParam;
  begin
    MyField:=TPSCListParam(Params.ItemByName(AName));
    If (MyField<>nil) then
      MyField.ReadOnly:=AOption in V;
  end;

begin
  MyIncludeReadOnly('Field',FltItem_ReadOnly_Field);
  MyIncludeReadOnly('Template',FltItem_ReadOnly_Template);
  MyIncludeReadOnly('Operation',FltItem_ReadOnly_Operation);
  MyIncludeReadOnly('Value',FltItem_ReadOnly_Value);
  MyIncludeReadOnly('Value1',FltItem_ReadOnly_Value1);
  MyIncludeReadOnly('Value2',FltItem_ReadOnly_Value2);
end;

{------------------------------------------------------------------}

Procedure TPSCFltItem.InternalWriteValue(Writer: TWriter);
var
  MyField:TPSCField;
Begin
  MyField:=TPSCField(Params.ItemByName('Value'));
  If MyField<>nil then
    PSCWriteVariant(Writer,Value,MyField.DataType);
End;

{------------------------------------------------------------------}

Procedure TPSCFltItem.InternalWriteValue1(Writer: TWriter);
var
  MyField:TPSCField;
Begin
  MyField:=TPSCField(Params.ItemByName('Value1'));
  If MyField<>nil then
    PSCWriteVariant(Writer,Value1,MyField.DataType);
End;

{------------------------------------------------------------------}

Procedure TPSCFltItem.InternalWriteValue2(Writer: TWriter);
var
  MyField:TPSCField;
Begin
  MyField:=TPSCField(Params.ItemByName('Value2'));
  If MyField<>nil then
    PSCWriteVariant(Writer,Value2,MyField.DataType);
End;

{------------------------------------------------------------------}

Procedure TPSCFltItem.DefineProperties(Filer: TFiler);

  Function CanWriteValue: Boolean;
  var
    MyField:TPSCField;
  Begin
    If Filer.Ancestor <> Nil Then
      Result := Value <> TPSCFltItem(Filer.Ancestor).Value
    Else
      Result := True;

    If Result then
    begin
      MyField:=TPSCField(Params.ItemByName('Value'));
      result:=(MyField<>nil) and (not MyField.IsNull);
    end;
  End;

  Function CanWriteValue1: Boolean;
  var
    MyField:TPSCField;
  Begin
    If Filer.Ancestor <> Nil Then
      Result := Value1 <> TPSCFltItem(Filer.Ancestor).Value1
    Else
      Result := True;

    If Result then
    begin
      MyField:=TPSCField(Params.ItemByName('Value1'));
      result:=(MyField<>nil) and (not MyField.IsNull);
    end;
  End;

  Function CanWriteValue2: Boolean;
  var
    MyField:TPSCField;
  Begin
    If Filer.Ancestor <> Nil Then
      Result := Value2 <> TPSCFltItem(Filer.Ancestor).Value2
    Else
      Result := True;

    If Result then
    begin
      MyField:=TPSCField(Params.ItemByName('Value2'));
      result:=(MyField<>nil) and (not MyField.IsNull);
    end;
  End;

Begin
  Inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamValue',InternalReadValue,InternalWriteValue,CanWriteValue);
  Filer.DefineProperty('ParamValue1',InternalReadValue1,InternalWriteValue1,CanWriteValue1);
  Filer.DefineProperty('ParamValue2',InternalReadValue2,InternalWriteValue2,CanWriteValue2);
End;

{-------------------------------------------------------------------------}

Procedure TPSCRecordSetStyle.HandleEvent(const AParams:TPSCEventParams);
begin
  If FilterSource<>nil then
    Filter:=FilterSource.FilterStr
  else
    Filter:='';
end;

{-------------------------------------------------------------------------}

Constructor TPSCRecordSetStyle.Create(Collection: TCollection);
Begin
  Inherited Create(Collection);
  FParams := TPSCFields.Create(Self,TPSCField);
  FDataFields := PSCCreateStringList;
  FColor := clPSCWindow;
  FFontColor := clPSCWindowText;
  FActive := False;
  FReadOnly := False;
  FEventsHandler:=PSCCreateEventHandler(HandleEvent);
End;

{-------------------------------------------------------------------------}

Destructor TPSCRecordSetStyle.Destroy;
Begin
  FilterSource:=nil;
  FParams.Free;
  FParser.Free;
  Inherited;
End;

{-------------------------------------------------------------------------}

Function TPSCRecordSetStyle.IsParamsStored: Boolean;
begin
  Result:=Params.Count>0;
end;

{-------------------------------------------------------------------------}

Function TPSCRecordSetStyle.GetDataColumn: String;
Begin
  Result := PSCFieldStringsToStr(FDataFields);
End;

{-------------------------------------------------------------------------}

procedure TPSCRecordSetStyle.SetFilterSource(V:TPSCCustomFltBld);
begin
  If FFilterSource <> V Then
    Begin
      If FFilterSource<>nil then
        FFilterSource.Events.UnregisterHandler(FEventsHandler);

      FFilterSource := V;

      If FFilterSource<>nil then
        begin
          FFilterSource.Events.RegisterHandler(FEventsHandler);
          Filter:=FilterSource.FilterStr;
        end
      else
        Filter:='';

      RegisterNotifier(V);

    End;
end;

{-------------------------------------------------------------------------}

Procedure TPSCRecordSetStyle.Notification(Instance: TComponent);
Begin
  inherited;
  If Instance = FFilterSource then
    FFilterSource := Nil;
End;

{-------------------------------------------------------------------------}

function TPSCRecordSetStyle.IsFilterStored:Boolean;
begin
  Result:=FFilterSource=nil;
end;

{-------------------------------------------------------------------------}

procedure TPSCRecordSetStyle.SetFontColor(V:TPSCColor);
begin
  If FFontColor<>V then
  begin
    FFontColor:=V;
    Changed(False);
  end;
end;

{-------------------------------------------------------------------------}

Procedure TPSCRecordSetStyle.SetFontStyles(Value: TPSCFontStyles);
Begin
  If FFontStyles<>Value then
  begin
    FFontStyles:=Value;
    Changed(False);
  end;
End;

{-------------------------------------------------------------------------}

Procedure TPSCRecordSetStyle.SetColor(Value: TPSCColor);
Begin
  If FColor <> Value Then
    Begin
      FColor := Value;
      Changed(False);
    End;
End;

{-------------------------------------------------------------------------}

Procedure TPSCRecordSetStyle.SetDataColumn(Value: String);
Begin
  FDataFields.Clear;
  PSCStrToFieldStrings(Value,FDataFields);
  Changed(False);
End;

{-------------------------------------------------------------------------}

Procedure TPSCRecordSetStyle.SetFilter(Const Value: String);
Begin
  If FFilter = PSCTrim(Value) Then
    Exit;
  FFilter := PSCTrim(Value);
  FParser.Free;
  FParser:=nil;
  Changed(False);
End;

{-------------------------------------------------------------------------}

Procedure TPSCRecordSetStyle.SetOptions(V:TPSCRecordSetStyleOptions);
begin
  If FOptions<>V then
  begin
    FOptions:=V;
    Changed(False);
  end;
end;

{-------------------------------------------------------------------------}

Procedure TPSCRecordSetStyle.SetExcludeFilter(Const Value: String);
Begin
  If FExcludeFilter = PSCTrim(Value) Then
    Exit;
  FExcludeFilter := PSCTrim(Value);
  FParser.Free;
  FParser:=nil;
  Changed(False);
End;

{-------------------------------------------------------------------------}

Procedure TPSCRecordSetStyle.SetActive(Value: Boolean);
Begin
  If FActive <> Value Then
    Begin
      FActive := Value;
      Changed(False);
    End;
End;

{-------------------------------------------------------------------------}

Procedure TPSCRecordSetStyle.SetParams(Value: TPSCFields);
Begin
  FParams.Assign(Value);
End;

{-------------------------------------------------------------------------}

Procedure TPSCRecordSetStyle.Assign(Source: TPersistent);
Var
  Src: TPSCRecordSetStyle;
Begin
  Inherited Assign(Source);
  If Source Is TPSCRecordSetStyle Then
    Begin
      Src := TPSCRecordSetStyle(Source);
      FFontStyles:=Src.FFontStyles;
      FColor := Src.FColor;
      FFontColor := Src.FFontColor;
      FFilter := Src.FFilter;
      FReadOnly := Src.FReadOnly;
      FDataFields.Assign(Src.FDataFields);
      FExcludeFilter := Src.FExcludeFilter;
      FParams.Assign(Src.FParams);
      FParser.Free;
      FParser:=nil;
      Active := Src.FActive;
    End;
End;

{-------------------------------------------------------------------------}

Function TPSCGridColors.IsRecordsInfoStored: boolean;
Begin
  Result := RecordsInfo.Count > 0;
End;

{-------------------------------------------------------------------------}

Function TPSCRecordSetStyles.GetItem(Index: Integer): TPSCRecordSetStyle;
begin
  Result:=TPSCRecordSetStyle(inherited Items[Index]);
end;

{-------------------------------------------------------------------------}

Procedure TPSCRecordSetStyles.SetItem(Index: Integer; V: TPSCRecordSetStyle);
begin
  inherited Items[Index]:=V;
end;

{-------------------------------------------------------------------------}

Procedure TPSCGridColors.UpdateCanvasFormatting(const ACanvas:TCanvas);

  procedure MySetFontStyle(AOption:TPSCRecordSetStyleOption;
    AFontStyle:TPSCFontStyle);
  begin
    If AOption in FormatOptions then
      begin
        If AFontStyle in FormatFontStyle then
          ACanvas.Font.Style:=ACanvas.Font.Style+[TFontStyle(AFontStyle)]
        else
          ACanvas.Font.Style:=ACanvas.Font.Style-[TFontStyle(AFontStyle)];
      end;
  end;

begin
  If ACanvas=nil then
    exit;

  If rssoSetColor in FormatOptions then
    ACanvas.Brush.Color:=FormatColor;
  If rssoSetFontColor in FormatOptions then
    ACanvas.Font.Color:=FormatFontColor;

  MySetFontStyle(rssoSetFontBold,FS_Bold);
  MySetFontStyle(rssoSetFontItalic,FS_Italic);
  MySetFontStyle(rssoSetFontUnderline,FS_Underline);
  MySetFontStyle(rssoSetFontStrikeOut,FS_StrikeOut);
end;

{-------------------------------------------------------------------------}

procedure TPSCGridColors.DoGridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
var
  aGrid:TDBGrid;
begin
  aGrid:=Sender as TDBGrid;

  if not (gdSelected in State) then
    UpdateFormatting(aGrid.Canvas,Column.FieldName);
  aGrid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

{-------------------------------------------------------------------------}

Procedure TPSCGridColors.UpdateFormatting(const ACanvas:TCanvas=nil;
  const AFieldName:String='');
var
  i:Integer;
  Act:Boolean;

  procedure MySetFontStyle(AFontStyle:TPSCFontStyle);
  begin
    If AFontStyle in RecordsInfo.Items[I].FontStyle then
      FFormatFontStyles:=FFormatFontStyles+[AFontStyle]
    else
      FFormatFontStyles:=FFormatFontStyles-[AFontStyle];
  end;

begin
  FFormatFontStyles:=[];
  FFormatOptions:=[];

  For I := 0 To RecordsInfo.Count - 1 Do
    With RecordsInfo.Items[I] Do
      Begin
        If Active And (Options<>[]) And (Filter<>'') and ((DataFields.IndexOf(AFieldName) <> -1) Or
          (DataFields.Count = 0)) Then
          Begin
            ExtExprParser := GetParser(RecordsInfo.Items[I]);
            Act := False;
            If Assigned(ExtExprParser) Then
            Try
              Act := EvalResult;
            Except
              Active := False;
              ExtExprParser := Nil;
              Raise;
              Exit;
            End;
            ExtExprParser := Nil;
            If Act Then
              Begin
                FFormatOptions:=FFormatOptions+Options;
                If rssoSetColor in Options then
                  FFormatColor:=Color;
                If rssoSetFontColor in Options then
                  FFormatFontColor:=FontColor;
                If rssoSetFontBold in Options then
                  MySetFontStyle(FS_Bold);
                If rssoSetFontItalic in Options then
                  MySetFontStyle(FS_Italic);
                If rssoSetFontUnderline in Options then
                  MySetFontStyle(FS_Underline);
                If rssoSetFontStrikeOut in Options then
                  MySetFontStyle(FS_StrikeOut);
              End;
          End;
      End;

  UpdateCanvasFormatting(ACanvas);
end;
{-------------------------------------------------------------------------}

Constructor TPSCGridColors.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FRecordsInfo := TPSCRecordSetStyles.Create(Self,TPSCRecordSetStyle);
  FRecordsInfo.OnUpdate := UpdCollectEvent;
End;

{-------------------------------------------------------------------------}

Destructor TPSCGridColors.Destroy;
Begin
  FRecordsInfo.Free;
  Inherited;
End;

{-------------------------------------------------------------------------}

Procedure TPSCGridColors.DoGetItemParamValue(AItem: TObject;
  Const AName: String; Var Value: Variant);
Begin
  If Assigned(OnGetItemParamValue) Then
    OnGetItemParamValue(Self,TPSCRecordSetStyle(AItem),AName,Value);
End;

{-------------------------------------------------------------------------}

Procedure TPSCGridColors.UpdCollectEvent(Sender: TObject; Item:
  TPSCNamedItem);
Begin
  DoOnChange(Sender);
End;

{-------------------------------------------------------------------------}

Procedure TPSCGridColors.DoOnChange(Sender: TObject);
Var
  I: Integer;
Begin
  For I := 0 To RecordsInfo.Count - 1 Do
    With TPSCRecordSetStyle(RecordsInfo.Items[I]) do
    begin
      FParser.Free;
      FParser:=nil;
    end;
  Inherited DoOnChange(Sender);
End;

{-------------------------------------------------------------------------}

Function TPSCGridColors.GetParser(AItem: TPSCRecordSetStyle): TExprParser;
Var
  FalseDataSet: TPSCEmulDataSet;
  Simplified: String;
Begin
  FalseDataSet := Nil;
  If Assigned(AItem.Parser) Then
    Result := AItem.Parser
  Else
    Begin
      Simplified := AItem.Filter;
      If AItem.ExcludeFilter <> '' Then
        Begin
          If Simplified <> '' Then
            Simplified := '(' + Simplified + ') And ';//don't resource
          Simplified := Simplified + '( Not (' + AItem.ExcludeFilter + '))'; //don't resource
        End;

      If Simplified <> '' Then
        AItem.Params.SafeParseParams(Simplified);
      Simplified := PSCReplaceWithParams(Simplified,AItem.Params);
      Try
        FalseDataSet := TPSCEmulDataSet.Create(Nil);
        If Assigned(DataSet) Then
          FalseDataSet.AssignDataSet(DataSet);
        FieldsFromColl(FalseDataSet);
        Result := PSCCreateFilterParserEx(FalseDataSet,Simplified,[poExtSyntax]);
        AItem.Parser := Result;
      Except
        FalseDataSet.Free;
        AItem.Active := False;
        Raise;
        Exit;
      End;
      FalseDataSet.Free;
    End;
End;

{-------------------------------------------------------------------------}

Procedure TPSCGridColors.SetRecordsInfo(Value: TPSCRecordSetStyles);
Begin
  FRecordsInfo.Assign(Value);
End;

{------------------------------------------------------------------------------}
Initialization
{$IFDEF PSCPATCHVARMGR}
  PSCPatchVariantManager;
{$ENDIF}
Finalization
  FDefaultTemplates.Free;
  FDefaultTemplates:=nil;
{$IFDEF PSCPATCHVARMGR}
  PSCUnPatchVariantManager;
{$ENDIF}
End.

