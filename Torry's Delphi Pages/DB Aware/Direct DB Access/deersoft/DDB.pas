unit DDB;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, ComObj, ActiveX, ADOApi, ADOBase, ComCtrls,
  {$IFDEF MSWINDOWS} // Delphi 6 and up
  Variants,
  {$ELSE}
  FileCtrl,
  {$ENDIF}
  DFilters, DUtils, DMaster, DBGrids, DEditors;

const
  cnsSortASC            = 'ASC';
  cnsSortDESC           = 'DESC';
  cniSortNONE           = 0;
  cniSortASC            = 1;
  cniSortDESC           = 2;
  cnsEmptyBlob          = 'NONE';
  cnsFieldSpace         = '     ';
  cnsMarkerSpace        = ' + ';

  cnsResName            = 'Default'; // Name of the Resource file(s)
  cnsResExt             = '.drf'; // (D)ataSet (R)esource (F)ile
  cnsCircularDataLink   = 'Circular datalinks are not allowed';
  cnsUpdateFailed       = 'Update Failed!';

  // Added variant types for VarType function
  varArrayVariant       = 8204;
  varArrayByte          = 8209;

{
Internal buffer layout:
+------------------------+------------------------+---------------------------+
|     RECORD DATA        |     My Information     |     Calculated Fields     |
| Size: FRecSize bytes   |  SizeOf(TBookmarkbytes |    CalcFieldSize bytes    |
+------------------------+------------------------+---------------------------+
                         ^                        ^
                    StartBookmark           StartCalculated
}

type
  TDOption        = (opDialog,
                     opFilter,
                     opSubList,
                     opConnection);
  TDOptions       = set of TDOption;
  TSortMode       = (smNONE, smASC, smDESC);
  TVerboseMode    = (vmCommand, vmExecute, vmFilter, vmSearch);
  TVerboseEvent   = procedure (const Text: String; Mode: TVerboseMode) of object;

  PDInfo = ^TDInfo;
  TDInfo = record
              BookData : Integer;
              BookFlag : TBookmarkFlag;
             end;

  TDUpdateObject = class;
  TColumnData    = class;
  TDSubList      = class;

  TDADODataSet = class(TDataSet)
  private
    { Private declarations }

    // Internal objects
    FConnection     : TDConnection;
    FRecords        : TDRecords;
    FConnect        : String;
    FCommand        : String;
    FResync         : TResyncMode;
    FMaster         : TDMaster;

    // Internal variables
    FActive         : Boolean;
    FNames          : TStrings;
    FBlobData       : TStrings;
    FRecSize        : Integer;
    FBufferSize     : Integer;
    FInfoStart      : Integer;
    FCalcStart      : Integer;
    FReadOnly       : Boolean;
    FSort           : String;
    FFilter         : String;
    FSQLUpdate      : Boolean;
    FUpdateMode     : TUpdateMode;
    FUpdateObject   : TDUpdateObject;
    FPrimaryKey     : String;
    FMarkerField    : String;
    FFilters        : TDFilters;
    FColors         : TDFilters;
    FColumns        : TColumnData;
    FSubList        : TDSubList;
    FFileName       : String;
    FOptions        : TDOptions;
    FDefTable       : String;
    FStoreDefs      : Boolean;
    FInfo           : TStrings;
    FTrans          : Boolean;

    // ADO Parameters
    FCommandOption  : TCommandOption;
    FCursorType     : TCursorType;
    FCursorLocation : TCursorLocation;
    FLockType       : TLockType;

    // New Events
    evOnMarkStart : TDataSetNotifyEvent;
    evOnMarkEnd   : TDataSetNotifyEvent;
    evOnFillStart : TDataSetNotifyEvent;
    evOnFillEnd   : TDataSetNotifyEvent;
    evOnDialog    : TDataSetNotifyEvent;
    evOnScroll    : TDataSetNotifyEvent;
    evOnVerbose   : TVerboseEvent;

    // Handle properties
    function  GetData: TDConnection;
    procedure SetMaster(Value: TDMaster);
    procedure SetConnection(const Value: String);
    procedure SetSortStr(const Value: String);
    procedure SetReadOnly(Value: Boolean);
    procedure SetCommand(const Value: String);
    procedure SetUpdateObject(Value: TDUpdateObject);
    procedure SetMarkerField(const Value: String);
    procedure SetPrimaryKey(const Value: String);
    procedure SetColors(Value: TDFilters);
    procedure SetFilters(Value: TDFilters);
    procedure SetColumns(Value: TColumnData);
    function  GetPosition: String;
    function  GetMaxRecords: Integer;
    procedure SetMaxRecords(Value: Integer);
    function  GetVersion: String;
    function  GetConnInfo: TStrings;
    procedure SetOptions(Value: TDOptions);

    // Utilities
    procedure InitOneField(const Name: String; ADOType: DataTypeEnum; Size: Integer; Required: Boolean; Index: Integer);
    procedure LoadRecordBuffer(Buffer: PChar; EmptyRec: Boolean = False);
    procedure InternalBookmark(Flag: TBookmarkFlag);
    function  GetActiveRecordBuffer:  PChar;

    // Conversions
    function IntegerToBuffer(Buffer: Pointer; S: String = ''): Boolean;
    function FloatToBuffer(Buffer: Pointer; S: String = ''): Boolean;
    function DateToBuffer(Buffer: Pointer; S: String = ''): Boolean;
    function TimeToBuffer(Buffer: Pointer; S: String = ''): Boolean;
    function DateTimeToBuffer(Buffer: Pointer; S: String = ''): Boolean;
    function BooleanToBuffer(Buffer: Pointer; S: String = ''): Boolean;
    function BufferToDate(Buffer: Pointer): String;
    function BufferToTime(Buffer: Pointer): String;
    function BufferToDateTime(Buffer: Pointer): String;
    function BlobToVariant(Field: TField): OleVariant;

    // Create Criteria string for search
    function CreateCriteria(const FieldName, SearchValue: String; PartialSearch: Boolean): String;

    // Handle Internal Connection
    function  ConnectionOpen: Boolean;
    procedure ConnectionClose;

    // Execute Fill command
    procedure FillExec(const SQLText: String);

  protected
    { Protected declarations }
    procedure SetFilterText(const Value: String); override;
    function GetConnection: String; virtual;

    // Events methods
    procedure DoBeforeEdit; override;
    procedure DoBeforeInsert; override;
    procedure DoAfterScroll; override;
    procedure DoAfterOpen; override;
    procedure DoAfterClose; override;
    procedure DoDialog; virtual;

    // Record buffer methods:
    function  AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    function  GetCanModify: Boolean; override;
    function  GetRecordSize: Word; override;
    function  GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    function  GetStateFieldValue(State: TDataSetState; Field: TField): Variant; override;
    procedure InternalInitRecord(Buffer: PChar); override;

    // Bookmark methods:
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function  GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    function  GetBookmarkStr: TBookmarkStr; override;
    procedure SetBookmarkStr(const Value: TBookmarkStr); override;

    // Navigation methode
    procedure InternalFirst; override;
    procedure InternalLast; override;

    // Editing methods:
    procedure InternalCancel; override;
    procedure InternalEdit; override;
    procedure InternalInsert; override;
    procedure InternalRefresh; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;

    // Misc methods
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function  IsCursorOpen: Boolean; override;

    // Optional overrides
    function  GetRecordCount: Integer; override;
    function  GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;

    // Other methods
    procedure ClearCalcFields(Buffer: PChar); override;
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;

    // Generator routines
    function GenerateUpdate: String; virtual;
    function GenerateWhere(SQLText: String = ''; JoinList: TStrings = nil): String; virtual;
    function GenerateField(const TableName, FieldName: String): String; overload;
    function GenerateField(const LongName: String): String; overload;
    function InsertSQL: String; virtual;
    function ModifySQL: String; virtual;
    function DeleteSQL: String; virtual;

    // Convert and assign datatypes ADO -> Borland Data Type
    function AdoToBdt(ADOType: DataTypeEnum): TFieldType; virtual;
    function BdtToAdo(DataType: TFieldType): DataTypeEnum; virtual;

    // Stream handler routines
    procedure InternalLoad(const FileName: String); virtual;
    procedure InternalSave(const FileName: String); virtual;

    function CleanSQL(const SQLText: String): String;

    // Handle Blob data
    function  GetBlobData(Field: TField): TBlobData; virtual;
    procedure SetBlobData(Field: TField; Value: TBlobData); virtual;

    // Marker and Primery Key
    function  GetMarkerField: String; virtual;
    function  GetPrimaryKey: String; virtual;

    property FieldNames        : TStrings              read FNames;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function  CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function  Locate(const KeyFields: string; const KeyValues: Variant;
                     Options: TLocateOptions): Boolean; override;
    function  ConnectionEditor: String;

    procedure ReOpen; virtual;
    procedure ExecFilter; virtual;

    // Internal methods for the next generations
    procedure InternalFields; virtual;
    function  InternalSQL(const SQLText: String): Integer; virtual;
    function  InternalTable(const SQLText: String): String; virtual;

    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;

    // Sort handler methods
    procedure SortAdd(Field: TField; AddField: Boolean = False); overload;
    procedure SortAdd(const FieldName: String; SortMode: TSortMode; AddField: Boolean = False); overload;
    procedure SortDelete(Field: TField); overload;
    procedure SortDelete(const FieldName: String); overload;
    function  IsSorted(Field: TField): Boolean; overload;
    function  IsSorted(const FieldName: String): Boolean; overload;
    function  SortMode(Field: TField): TSortMode; overload;
    function  SortMode(const FieldName: String): TSortMode; overload;
    function  SortField(Field: TField): Integer;

    // Handle DataSet collection
    procedure SubListOpen;
    procedure SubListClose;

    // ADO Service functions
    procedure GetOdbcDriverList(List: TStrings);
    function  GetOdbcDriverExt(const Driver: String): String;
    function  GetOdbcDriverFilter(const Driver: String): String;
    procedure GetOdbcDSNList(List: TStrings);
    procedure GetADOProviderList(List: TStrings); overload;
    procedure GetADOProviderList(Name, Code: TStrings); overload;
    procedure GetADOTableNames(List: TStrings);
    procedure GetADOFieldNames(const TableName: String; List: TStrings); overload;
    procedure GetADOFieldNames(List: TStrings); overload;

    // Finder function
    function Find(const SearchStr: String): Boolean;

    // Extra features
    function Search(const FieldName, SearchStr: String; PartialSearch: Boolean = False; LeadZero: Byte = 0): Boolean; overload;
    function Search(Field: TField; const SearchStr: String; PartialSearch: Boolean = False; LeadZero: Byte = 0): Boolean; overload;
    function Search(const SearchExpr: String): Boolean; overload;

    // Load UDL file
    function LoadUDLFile(const FileName: String; UserName: String = ''; Password: String = ''): String;

    // Editors
    function  FilterEditor(Field: TField): Boolean; overload;
    function  FilterEditor(const FieldName: String): Boolean; overload;
    function  FilterEditor: Boolean; overload;
    function  ColorEditor: Boolean;
    procedure ExportEditor;
    procedure FillUpEditor(Field: TField); overload;
    procedure FillUpEditor(const FieldName: String); overload;
    procedure FieldColors(const FieldName: String; var PaperColor, InkColor: TColor);
    procedure UDLEditor(const FileName: String);

    // Fill Functions
    procedure FillUpper(const FieldName: String);
    procedure FillLower(const FieldName: String);
    procedure FillConst(const FieldName, FieldValue: String; StrType: Boolean = True);
    procedure FillExchange(const FieldName, FromValue, ToValue: String);
    procedure FillSQL(const FieldName: String; SQLItems: TStrings);
    procedure Markers(const Value: String); virtual;

    // ADO Settings
    property ADOCommandOption  : TCommandOption        read FCommandOption  write FCommandOption  default coText;
    property ADOCursorType     : TCursorType           read FCursorType     write FCursorType     default ctOpenDynamic;
    property ADOCursorLocation : TCursorLocation       read FCursorLocation write FCursorLocation default clClient;
    property ADOLockType       : TLockType             read FLockType       write FLockType       default ltLockOptimistic;

    // Public Properties
    property CentralData   : TDConnection          read GetData;
    property Records       : TDRecords             read FRecords;
    property Command       : String                read FCommand        write SetCommand;
    property PositionResync: TResyncMode           read FResync         write FResync;
    property UpdateSQL     : Boolean               read FSQLUpdate      write FSQLUpdate default False;
    property Position      : String                read GetPosition;
    property Filters       : TDFilters             read FFilters        write SetFilters;
    property Colors        : TDFilters             read FColors         write SetColors;
    property Columns       : TColumnData           read FColumns        write SetColumns;
    property DefaultTable  : String                read FDefTable;
    property Version       : String                read GetVersion;
    property Information   : TStrings              read GetConnInfo;

  published
    { Published declarations }

    // Standard Properties
    property Active;

    // Standard Events
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnDeleteError;
    property OnCalcFields;
    property OnEditError;
    property FieldDefs;
    property FilterOptions;

    // New properties
    property Connection    : String                read GetConnection   write SetConnection;
    property Sort          : String                read FSort           write SetSortStr;
    property Filter        : String                read FFilter         write SetFilterText;
    property Master        : TDMaster              read FMaster         write SetMaster;
    property MaxRecords    : Integer               read GetMaxRecords   write SetMaxRecords;
    property SubList       : TDSubList             read FSubList        write FSubList;
    property ReadOnly      : Boolean               read FReadOnly       write SetReadOnly default False;
    property UpdateMode    : TUpdateMode           read FUpdateMode     write FUpdateMode;
    property UpdateObject  : TDUpdateObject        read FUpdateObject   write SetUpdateObject;
    property MarkerField   : String                read GetMarkerField  write SetMarkerField;
    property PrimaryKey    : String                read GetPrimaryKey   write SetPrimaryKey;
    property FileName      : String                read FFileName       write FFileName;
    property StoreDefs     : Boolean               read FStoreDefs      write FStoreDefs  default False;
    property Options       : TDOptions             read FOptions        write SetOptions;
    property Transaction   : Boolean               read FTrans          write FTrans default False;

    // New Events
    property OnMarkStart   : TDataSetNotifyEvent   read evOnMarkStart   write evOnMarkStart;
    property OnMarkEnd     : TDataSetNotifyEvent   read evOnMarkEnd     write evOnMarkEnd;
    property OnFillStart   : TDataSetNotifyEvent   read evOnFillStart   write evOnFillStart;
    property OnFillEnd     : TDataSetNotifyEvent   read evOnFillEnd     write evOnFillEnd;
    property OnDialog      : TDataSetNotifyEvent   read evOnDialog      write evOnDialog;
    property OnScroll      : TDataSetNotifyEvent   read evOnScroll      write evOnScroll;
    property OnVerbose     : TVerboseEvent         read evOnVerbose     write evOnVerbose;

  end;

{******************************************************************************}

  // Update object for Query and Table
  TDUpdateObject = class(TComponent)
  protected
    function GetDataSet: TDADODataSet; virtual; abstract;
    procedure SetDataSet(ADataSet: TDADODataSet); virtual; abstract;
    procedure Apply(UpdateKind: TUpdateKind); virtual; abstract;
    property DataSet: TDADODataSet read GetDataSet write SetDataSet;

  end;

{******************************************************************************}

  // Handle Memo fields
  TDBlobStream = class(TStream)
  private
    FField: TBlobField;
    FDataSet: TDADODataSet;
    FBuffer: PChar;
    FMode: TBlobStreamMode;
    FOpened: Boolean;
    FModified: Boolean;
    FPosition: Longint;
    FBlobData: TBlobData;

    function GetBlobSize(): Longint;

  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function CopyFrom(Source: TStream; Count: Longint): Longint;
    procedure Truncate;

  end;

{******************************************************************************}

  // Wrapper to Save and Load
  TDADOWrapper = class(TComponent)
  private
    { Private declarations }
    FColors      : TDFilters;
    FFilters     : TDFilters;
    FSort        : String;
    FFilter      : String;
    FColumns     : TColumnData;

    procedure SetFilters(Value: TDFilters);
    procedure SetColors(Value: TDFilters);
    procedure SetColumns(Value: TColumnData);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    property Colors      : TDFilters      read FColors   write SetColors;
    property Filters     : TDFilters      read FFilters  write SetFilters;
    property Columns     : TColumnData    read FColumns  write SetColumns;
    property Sort        : String         read FSort     write FSort;
    property Filter      : String         read FFilter   write FFilter;

  end;

{******************************************************************************}

  TColumnData = class(TCollection)
  protected
    FOwner: TComponent;
    function  GetOwner: TPersistent; override;
    function  GetItem(Index: Integer): TColumn;
    procedure SetItem(Index: Integer; Value: TColumn);

  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
    function Add: TColumn;

    property Items[Index: Integer]: TColumn read GetItem  write SetItem; default;

  end;

{******************************************************************************}

  // DataCollection to Open in Background
  TDSubItem = class(TCollectionItem)
  private
    FDataLink : TDataLink;
    FBack     : Boolean;

    function  GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function  GetDataSet: TDataSet;
    function  GetItemName: String;

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function  GetDisplayName: String; override;
    property Name : String read GetItemName;

    property DataSet    : TDataSet    read GetDataSet;

 published
    property DataSource : TDataSource read GetDataSource   write SetDataSource;
    property Background : Boolean     read FBack           write FBack default True;

  end;

  TDSubList = class(TCollection)
  protected
    FOwner: TDataSet;
    function  GetOwner: TPersistent; override;
    function  GetItem(Index: Integer): TDSubItem;
    procedure SetItem(Index: Integer; Value: TDSubItem);

  public
    constructor Create(DataSet: TDataSet);
    function Add: TDSubItem; overload;
    function Add(Source: TDataSource; RunBack: Boolean = True): TDSubItem; overload;
    function FindItem(const ItemName: String): TDSubItem;
    property Items[Index: Integer]: TDSubItem read GetItem   write SetItem; default;

  end;

{******************************************************************************}

  // Thread for ADO dataset
  TDADOThread = class(TThread)
  private
    { Private declarations }
    FDataSet    : TDataSet;
    FDatasource : TDataSource;
    FQueryException: Exception;

    procedure ConnectDataSource;
    procedure ShowQryError;

  protected
    { Protected declarations }
    procedure Execute; override;

  public
    { Public declarations }
    constructor Create(DataSource: TDataSource); virtual;

  published
    { Published declarations }
  end;

{******************************************************************************}

implementation

{$R *.dcr}

{******************************************************************************}

{******************************************************************************}
{***                           ADO DataSet                                  ***}
{******************************************************************************}


constructor TDADODataSet.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FNames          := TStringList.Create;
     FBlobData       := TStringList.Create;
     FInfo           := TStringList.Create;
     FFilters        := TDFilters.Create(Self);
     FColors         := TDFilters.Create(Self);
     FColumns        := TColumnData.Create(Self);
     FSubList        := TDSubList.Create(Self);
     FSort           := '';
     FResync         := [rmExact, rmCenter];
     FReadOnly       := False;
     FSQLUpdate      := False;
     FUpdateMode     := upWhereAll;
     FUpdateObject   := nil;
     FOptions        := [opDialog, opFilter, opConnection];
     FFileName       := '';
     FilterOptions   := [];
     FStoreDefs      := False;
     FConnection     := nil;
     FRecords        := nil;
     FTrans          := False;

     // ADO Settings
     FCommandOption  := coText;
     FCursorType     := ctOpenDynamic;
     FCursorLocation := clClient;
     FLockType       := ltLockOptimistic;
end;


destructor TDADODataSet.Destroy;
begin
     if FActive then InternalClose;
     ConnectionClose;
     FRecords.Free;
     FRecords := nil;
     FFilters.Free;
     FColors.Free;
     FColumns.Free;
     FSubList.Free;
     FNames.Free;
     FNames := nil;
     FBlobData.Free;
     FBlobData := nil;
     FInfo.Free;
     FInfo := nil;
     inherited Destroy;
end;


procedure TDADODataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
     inherited Notification(AComponent, Operation);
     if (Operation = opRemove) and (FMaster <> nil) and
        (AComponent = Master) then Master := nil;
end;


function TDADODataSet.ConnectionEditor: String;
var
   oEditor : TDConnectionEditor;
begin
     oEditor := TDConnectionEditor.Create(Self);
     try
     begin
          oEditor.Connection := FConnect;
          if oEditor.Execute then
          begin
               FConnect  := oEditor.Connection;
               FDefTable := oEditor.DefaultTable;
          end
          else FDefTable := '';
     end;
     finally
          oEditor.Free;
     end;
     Result := FConnect;
end;


{******************************************************************************}


function TDADODataSet.GetData: TDConnection;
begin
     Result := nil;
     if Assigned(FConnection) then Result := FConnection;
end;


procedure TDADODataSet.SetMaster(Value: TDMaster);
begin
     if FMaster <> Value then
     begin
          if Value = nil then
          begin
               FMaster.RemoveDataSet(Self);
               if (opConnection in FOptions) and
                  Assigned(FConnection) and
                  not FConnection.IsLocale then FConnection := nil;
          end;
          FMaster := Value;
          if Assigned(FMaster) then
          begin
               Connection := FMaster.Connection;
               FMaster.InsertDataSet(Self);
               if opConnection in FOptions then
               begin
                    if Assigned(FConnection) and FConnection.IsLocale then
                    begin
                         FConnection.Close;
                         FConnection.Free;
                         FConnection := nil;
                         FConnection := FMaster.CentralData;
                    end;
               end;
          end;
     end;
end;


function TDADODataSet.GetConnection: String;
begin
     Result := FConnect;
end;


procedure TDADODataSet.SetConnection(const Value: String);
begin
     if FConnect <> Value then
     begin
          FConnect := Value;
          if Assigned(FConnection) and FConnection.IsLocale then
          begin
               FConnection.Close;
               FConnection.Connection := FConnect;
          end;
     end;
end;


procedure TDADODataSet.SetSortStr(const Value: String);
var
   i : Integer;
begin
     if FSort <> Value then
     begin
          FSort := Value;
          if FActive then
          begin
               if Assigned(FRecords) then
               begin
                    FRecords.Sort := FSort;
                    for i := 0 to Fields.Count-1 do
                    begin
                         if Fields[i].FieldName = MarkerField
                         then
                             Fields[i].DisplayLabel := cnsMarkerSpace
                         else
                             Fields[i].DisplayLabel := Trim(Fields[i].DisplayLabel) + cnsFieldSpace;
                         Fields[i].Tag := SortField(Fields[i]);
                    end;
                    First;
               end;
          end;
     end;
end;


procedure TDADODataSet.SetColors(Value: TDFilters);
begin
     FColors.Assign(Value);
end;


procedure TDADODataSet.SetFilters(Value: TDFilters);
begin
     FFilters.Assign(Value);
end;


procedure TDADODataSet.SetColumns(Value: TColumnData);
begin
     FColumns.Assign(Value);
end;


procedure TDADODataSet.SetFilterText(const Value: String);
begin
     inherited;
     if (FFilter <> Value) then
     begin
          FFilter := Value;
          ExecFilter;
     end;
end;


procedure TDADODataSet.SetReadOnly(Value: Boolean);
begin
     if FReadOnly <> Value then
     begin
          FReadOnly := Value;
          if ReadOnly
          then
              FLockType := ltLockReadOnly
          else
              FLockType   := ltLockBatchOptimistic;
     end;
end;


procedure TDADODataSet.SetCommand(const Value: String);
var
   sTmp : String;
begin
     sTmp := CleanSQL(Value);
     if FCommand <> sTmp then FCommand := sTmp;
end;


function TDADODataSet.GetMaxRecords: Integer;
begin
     Result := 0;
     if Assigned(FRecords) then Result := FRecords.MaxRecords;
end;

procedure TDADODataSet.SetMaxRecords(Value: Integer);
begin
     if Assigned(FRecords) then FRecords.MaxRecords := Value;
end;


function TDADODataSet.GetVersion: String;
begin
     Result := 'Unknown';
     if ConnectionOpen then Result := FConnection.Version;
end;


function TDADODataSet.GetConnInfo: TStrings;
begin
     Result := nil;
     if ConnectionOpen then Result := FConnection.Information;
     if Result = nil then Result := FInfo;
end;



procedure TDADODataSet.SetOptions(Value: TDOptions);
begin
     if FOptions <> Value then
     begin
          FOptions := Value;
          if opConnection in FOptions then
          begin
               if Assigned(FConnection) and Assigned(FMaster) and FConnection.IsLocale then
               begin
                    FConnection.Close;
                    FConnection.Free;
                    FConnection := nil;
                    FConnection := FMaster.CentralData;
               end;
          end;
     end;
end;


{******************************************************************************}
{***                   Open/Close and Buffer handling                       ***}
{******************************************************************************}


procedure TDADODataSet.InternalOpen;
var
   i: Integer;
begin
     if (FConnect = '') or (FCommand = '') then Exit;
     try
     begin
          if not ConnectionOpen then Exit;
          if not FConnection.Active then FConnection.Open;
          InternalLoad(FFileName);
          if Assigned(FRecords) then
          begin
               FRecords.Free;
               FRecords := nil;
          end;
          FRecords := TDRecords.Create(FConnection);
          FRecords.Sort := FSort;
          FRecords.CursorType    := FCursorType;
          FRecords.LockType      := FLockType;
          FRecords.CommandOption := FCommandOption;
          FRecords.Command       := FCommand;
          FRecords.Transaction   := FTrans;
          ExecFilter;
          if Assigned(evOnVerbose) then OnVerbose(FCommand, vmCommand);
          FRecords.Open;
          FActive      := FRecords.Active;
          BookmarkSize := 10;
          InternalInitFieldDefs;
          if DefaultFields then CreateFields;
          BindFields(True);
          InternalFields;
          FRecSize     := FRecords.CalcRecSize(Fields);
          FInfoStart   := FRecSize;
          FCalcStart   := FRecSize + SizeOf(TDInfo);
          FBufferSize  := FRecSize + SizeOf(TDInfo) + CalcFieldsSize;
          FBlobData.Clear;
          for i := 0 to Fields.Count-1 do
          begin
               if Fields[i] is TBlobField then
               begin
                    FBlobData.Add(Fields[i].FieldName + '=' + cnsEmptyBlob);
               end;
          end;
          if opSubList in FOptions then SubListOpen;
          FRecords.First;
     end;
     except
          DatabaseError('Open error!');
     end;
end;


procedure TDADODataSet.InternalClose;
begin
     if FActive then
     begin
          if opSubList in FOptions then SubListClose;
          InternalSave(FFileName);
          if FRecords.Active then FRecords.Close;
          if FConnection.IsLocale then FConnection.Close;
     end;
     FActive := False;
     BindFields(False);
     if DefaultFields then
     begin
          DestroyFields;
          if not FStoreDefs then FieldDefs.Clear;
     end;
end;


procedure TDADODataSet.InternalInitFieldDefs;
var
   i    : Integer;
   oRec : TDRecords;
begin
     if FActive then
     begin
          if not FStoreDefs or (FieldDefs.Count = 0) then
          begin
               FieldDefs.Clear;
               for i := 0 to FRecords.ADO.Fields.Count-1 do
               begin
                    InitOneField(FRecords.ADO.Fields.Item[i].Name,
                                 FRecords.ADO.Fields.Item[i].Type_,
                                 FRecords.ADO.Fields.Item[i].DefinedSize, False, i);
               end;
          end;
     end
     else
     begin
          if not FStoreDefs or (FieldDefs.Count = 0) then
          begin
               if ConnectionOpen then
               begin
                    oRec := TDRecords.Create(FConnection);
                    try
                    begin
                         oRec.MaxRecords    := 1;
                         oRec.CursorType    := FCursorType;
                         oRec.LockType      := FLockType;
                         oRec.CommandOption := FCommandOption;
                         oRec.Command       := FCommand;
                         oRec.Open;
                         FieldDefs.Clear;
                         for i := 0 to oRec.ADO.Fields.Count-1 do
                         begin
                              InitOneField(oRec.ADO.Fields.Item[i].Name,
                                           oRec.ADO.Fields.Item[i].Type_,
                                           oRec.ADO.Fields.Item[i].DefinedSize, False, i);
                         end;
                         oRec.Close;
                    end;
                    finally
                         oRec.Free;
                    end;
               end;
          end;
     end;
end;


function TDADODataSet.GetActiveRecordBuffer: PChar;
begin
     case State of
          dsBrowse        : if IsEmpty then Result := nil else Result := ActiveBuffer;
          dsCalcFields    : Result := CalcBuffer;
          dsEdit,dsInsert : Result := ActiveBuffer;
     else
          Result := nil;
     end;
end;


function TDADODataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
   pRec : PChar;
   oTmp : TStringList;
   sTmp : String;
   iPos : Integer;
begin
     Result := False;
     pRec := GetActiveRecordBuffer;
     if (pRec = nil) or not FActive or (pRec[0] = #0) then Exit;
     if Field.FieldKind in [fkCalculated, fkLookup] then
     begin
          Inc(pRec, FCalcStart + Field.Offset);
          Result := Boolean(pRec[0]);
          if Result and (Buffer <> nil) then Move(pRec[1], Buffer^, Field.DataSize);
     end
     else
     begin
          oTmp := TStringList.Create;
          try
          begin
               oTmp.CommaText := pRec;
               iPos := FRecords.FieldPosition(Field.FieldName);
               while iPos >= oTmp.Count do oTmp.Add('');
               if iPos > -1 then sTmp := oTmp.Strings[iPos] else sTmp := '';
               if Buffer = nil then Result := (sTmp <> '') else
               begin
                    case Field.DataType of
                         ftFixedChar,
                         ftWideString,
                         ftString   :
                         begin
                              StrCopy(PChar(Buffer), PChar(sTmp));
                              Result := (sTmp <> '');
                         end;
                         ftLargeint,
                         ftSmallint,
                         ftWord,
                         ftAutoInc,
                         ftInteger  : Result := IntegerToBuffer(Buffer, sTmp);
                         ftBCD,
                         ftCurrency,
                         ftFloat    : Result := FloatToBuffer(Buffer, sTmp);
                         ftDate     : Result := DateToBuffer(Buffer, sTmp);
                         ftTime     : Result := TimeToBuffer(Buffer, sTmp);
                         ftDateTime : Result := DateTimeToBuffer(Buffer, sTmp);
                         ftBoolean  : Result := BooleanToBuffer(Buffer, sTmp);
                    else StrCopy(PChar(Buffer), PChar('Unknown'));
                    end;
               end;
          end;
          finally
               oTmp.Free;
          end;
     end;
end;


procedure TDADODataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
   pRec : PChar;
   oTmp : TStringList;
   sTmp : String;
   iPos : Integer;
begin
     pRec := GetActiveRecordBuffer;
     if Field.FieldKind in [fkCalculated, fkLookup] then
     begin
          Inc(pRec, FCalcStart + Field.Offset);
          Boolean(pRec[0]) := LongBool(Buffer);
          if Boolean(pRec[0]) then Move(Buffer^, pRec[1], Field.DataSize);
     end
     else
     begin
          oTmp := TStringList.Create;
          try
          begin
               oTmp.CommaText := pRec;
               while Field.Index >= oTmp.Count do oTmp.Add('');
               sTmp := '';
               if Buffer <> nil then
               begin
                    case Field.DataType of
                         ftFixedChar,
                         ftWideString,
                         ftString   : sTmp := PChar(Buffer);
                         ftLargeint,
                         ftSmallint,
                         ftWord,
                         ftAutoInc,
                         ftInteger  : sTmp := IntToStr(Integer(Buffer^));
                         ftBCD,
                         ftCurrency,
                         ftFloat    : sTmp := FloatToStr(Double(Buffer^));
                         ftDate     : sTmp := BufferToDate(Buffer);
                         ftTime     : sTmp := BufferToTime(Buffer);
                         ftDateTime : sTmp := BufferToDateTime(Buffer);
                         ftBoolean  : sTmp := IIF(WordBool(Buffer^), cnsWordBoolTrue, cnsWordBoolFalse);
                    else sTmp := 'Unknown';
                    end;
               end;
               iPos := FRecords.FieldPosition(Field.FieldName);
               if iPos > -1 then oTmp.Strings[iPos] := sTmp;
               StrLCopy(ActiveBuffer, PChar(oTmp.CommaText), FRecSize);
               DataEvent(deFieldChange, Longint(Field));
          end;
          finally
               oTmp.Free;
          end;
     end;
end;


function TDADODataSet.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
begin
     if Field.FieldKind in [fkData, fkInternalCalc] then
     begin
          case State of
               dsNewValue : Result := Field.Value;
               dsOldValue : Result := FRecords.FieldOldValue(Field.FieldName);
               dsCurValue : Result := Field.Value;
          else Result := NULL;
          end;
     end
     else Result := NULL;
end;


function TDADODataSet.IsCursorOpen: Boolean;
begin
     Result := FActive;
end;


function TDADODataSet.GetCanModify: Boolean;
begin
     Result := not FReadOnly;
end;


function TDADODataSet.GetRecordSize: Word;
begin
     Result := FBufferSize;
end;


function TDADODataSet.AllocRecordBuffer: PChar;
begin
     Result := AllocMem(FBufferSize);
end;


procedure TDADODataSet.FreeRecordBuffer(var Buffer: PChar);
begin
     FreeMem(Buffer, FBufferSize);
end;


function TDADODataSet.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
     Result := grOk;
     case GetMode of
          gmPrior   : if not FRecords.BOF then FRecords.Prior() else Result := grBOF;
          gmNext    : if not FRecords.EOF then FRecords.Next()  else Result := grEOF;
          gmCurrent :
          begin
               if not FRecords.HaveRecords then Result := grEOF else
               begin
                    if FRecords.State = dsInsert then FRecords.Last();
               end;
          end;
     end;
     if Result = grOk then LoadRecordBuffer(Buffer) else
     begin
          if (Result = grError) then DatabaseError('No records');
     end;
end;


procedure TDADODataSet.LoadRecordBuffer(Buffer: PChar; EmptyRec: Boolean = False);
begin
     FRecords.LoadRecord(Buffer, EmptyRec);
     with PDInfo(Buffer + FInfoStart)^ do
     begin
          BookData := FRecords.Bookmark;
          if EmptyRec then BookFlag := bfInserted else BookFlag := bfCurrent;
     end;
     // Handle Calculated Fields
     ClearCalcFields(Buffer);
     GetCalcFields(Buffer);
end;


procedure TDADODataSet.InternalInitRecord(Buffer: PChar);
begin
     FillChar(Buffer^, FBufferSize, 0);
end;


{******************************************************************************}
{***                           Bookmark handling                            ***}
{******************************************************************************}


procedure TDADODataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
     PInteger(Data)^ := PDInfo(Buffer + FRecSize)^.BookData;
end;


procedure TDADODataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
     if Buffer^ <> #0 then PDInfo(Buffer + FInfoStart)^.BookData := PInteger(Data)^;
end;


function TDADODataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
     Result := PDInfo(Buffer + FInfoStart).BookFlag;
end;


procedure TDADODataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
     PDInfo(Buffer + FInfoStart)^.BookFlag := Value;
end;


procedure TDADODataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
     if FActive then FRecords.Bookmark := Integer(Bookmark);
end;


procedure TDADODataSet.InternalSetToRecord(Buffer: PChar);
begin
     if Assigned(FRecords) and (Buffer^ <> #0) then FRecords.Bookmark := PDInfo(Buffer + FInfoStart)^.BookData;
end;


function TDADODataSet.GetBookmarkStr: TBookmarkStr;
begin
     Result := '';
     if FActive then Result := TBookmarkStr(IntToStr(FRecords.Bookmark));
end;


procedure TDADODataSet.SetBookmarkStr(const Value: TBookmarkStr);
var
   iBook : Integer;
begin
     if FActive then
     begin
          if Value = '' then iBook := FRecords.Bookmark else iBook := StrToInt(Value);
          if iBook <> FRecords.Bookmark then
          begin
               FRecords.Bookmark := iBook;
               InternalRefresh;
          end;
     end;
end;


{******************************************************************************}
{***                             Navigation                                 ***}
{******************************************************************************}


function TDADODataSet.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
   sTmp : String;
   sFld : String;
   oFld : TStringList;
   sVal : String;
   oVal : TStringList;
   i    : Integer;
begin
     Result := False;
     if FActive then
     begin
          oFld := TStringList.Create;
          try
          begin
               oVal := TStringList.Create;
               try
               begin
                    sTmp := '';
                    oFld.CommaText := StrTran(KeyFields, ';', ',');
                    if VarType(KeyValues) = varArrayVariant then
                    begin
                         for i := 0 to VarArrayHighBound(KeyValues, 1) do
                         begin
                              sTmp := sTmp + VarToStr(KeyValues[i]) + ','
                         end;
                         sTmp := Copy(sTmp, 1, Length(sTmp)-1);
                         oVal.CommaText := sTmp;
                    end
                    else oVal.CommaText := StrTran(VarToStr(KeyValues), ';', ',');
                    while oVal.Count < oFld.Count do oVal.Add('');
                    sTmp := '';
                    for i := 0 to oFld.Count-1 do
                    begin
                         sFld := oFld.Strings[i];
                         sVal := oVal.Strings[i];
                         if (sFld <> '') and (sVal <> '')
                         then
                             sTmp := sTmp + CreateCriteria(sFld, sVal, (loPartialKey in Options)) + ' AND ';
                    end;
                    sTmp := Copy(sTmp, 1, Length(sTmp)-5);
                    if sTmp <> '' then Result := Find(sTmp);
               end;
               finally
                    oVal.Free;
               end;
          end;
          finally
               oFld.Free;
          end;
     end;
end;


function TDADODataSet.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
var
   oTmp : TStringList;
   oFld : TField;
   vTmp : Variant;
   i    : Integer;
begin
     Result := Null;
     if Locate(KeyFields, KeyValues, []) then
     begin
          if Pos(';', ResultFields) > 0 then
          begin
               oTmp := TStringList.Create;
               try
               begin
                    oTmp.CommaText := StrTran(ResultFields, ';', ',');
                    vTmp := VarArrayCreate([0, oTmp.Count-1], varVariant);
                    for i := 0 to oTmp.Count-1 do
                    begin
                         oFld := FindField(oTmp.Strings[i]);
                         if Assigned(oFld) then vTmp[i] := oFld.AsString;
                    end;
               end;
               finally
                    oTmp.Free;
               end;
          end
          else
          begin
               oFld := FindField(ResultFields);
               if Assigned(oFld) then vTmp := oFld.AsVariant;
          end;
          Result := vTmp;
     end;
end;


procedure TDADODataSet.InternalFirst;
begin
     if FActive then FRecords.First;
end;


procedure TDADODataSet.InternalLast;
begin
     if FActive then FRecords.Last;
end;


{******************************************************************************}
{***                               Editing                                  ***}
{******************************************************************************}


procedure TDADODataSet.InternalCancel;
var
   bLast : Boolean;
begin
     if FActive then
     begin
          bLast := (FRecords.State = dsInsert);
          FRecords.Cancel;
          if bLast then Last;
     end;
end;


procedure TDADODataSet.InternalEdit;
begin
     inherited InternalEdit;
     FRecords.State := dsEdit;
end;


procedure TDADODataSet.InternalInsert;
begin
     inherited InternalInsert;
     FRecords.State := dsInsert;
     LoadRecordBuffer(ActiveBuffer, True);
end;


procedure TDADODataSet.InternalRefresh;
begin
     Resync(FResync);
end;


procedure TDADODataSet.InternalDelete;
var
   bEof : Boolean;
   sSql : String;
begin
     if FActive then
     begin
          bEof := FRecords.EOF;
          if FSQLUpdate and not Assigned(FUpdateObject) then sSql := DeleteSQL;
          FRecords.Delete; // Delete record
          if FSQLUpdate and not Assigned(FUpdateObject) then InternalSQL(sSql);
          if Assigned(FUpdateObject)
          then
              FUpdateObject.Apply(ukDelete)
          else if not FSQLUpdate then FRecords.Update;

          // Move the record pointer
          if not bEof then
          begin
               if not FRecords.EOF then
               begin
                    FRecords.Next;
                    InternalBookmark(bfCurrent);
                    CursorPosChanged;
               end;
          end
          else
          begin
               if not FRecords.BOF then
               begin
                    FRecords.Prior;
                    InternalBookmark(bfCurrent);
                    CursorPosChanged;
               end;
          end;
     end;
end;


procedure TDADODataSet.InternalPost;
var
   i     : Integer;
   oFld  : TField;
   sSql  : String;
   sOld  : String;
   sNew  : String;
   xState: TDataSetState;
begin
     xState := FRecords.State;
     if FActive and (xState in [dsEdit, dsInsert]) then
     begin
          if (xState = dsInsert) then FRecords.Insert;
          if FSQLUpdate and not Assigned(FUpdateObject) then
          begin
               case xState of
                    dsEdit   : sSql := ModifySQL;
                    dsInsert : sSql := InsertSQL;
               end;
          end;
          for i := 0 to FRecords.ADO.Fields.Count-1 do
          begin
               oFld := FindField(FRecords.ADO.Fields.Item[i].Name);
               if Assigned(oFld) then
               begin
                    if oFld is TBlobField then
                    begin
                         if TBlobField(oFld).Modified
                         then
                             FRecords.ADO.Fields.Item[i].Value := BlobToVariant(oFld);
                    end
                    else
                    begin
                         if (oFld.DataType <> ftAutoInc) then
                         begin
                              sNew := oFld.AsString;
                              sOld := VarToStr(FRecords.ADO.Fields.Item[i].Value);
                              if oFld.DataType = ftBoolean then sNew := IIF(oFld.AsBoolean, cnsWordBoolTrue, cnsWordBoolFalse);
                              if sNew <> sOld then
                              begin
                                   if oFld.DataType = ftBoolean
                                   then
                                       FRecords.ADO.Fields.Item[i].Value := IIF(oFld.AsBoolean, cnsWordBoolTrue, cnsWordBoolFalse)
                                   else
                                       FRecords.ADO.Fields.Item[i].Value := oFld.Value;
                              end;
                         end;
                    end;
               end;
          end;
          // Store data
          if FSQLUpdate and not Assigned(FUpdateObject) then InternalSQL(sSql) else
          begin
               if Assigned(FUpdateObject) then
               begin
                    case xState of
                         dsEdit   : FUpdateObject.Apply(ukModify);
                         dsInsert : FUpdateObject.Apply(ukInsert);
                    end;
               end;
          end;
          if not FSQLUpdate and not Assigned(FUpdateObject) then FRecords.Update;
          // Set the Position
          if xState = dsInsert then
          begin
               if FRecords.RecordCount = 1 then ReOpen else
               begin
                    InternalBookmark(bfCurrent);
                    CursorPosChanged;
               end;
               FRecords.State := dsBrowse;
               DoAfterScroll;
          end
          else FRecords.State := dsBrowse;
     end;
end;


procedure TDADODataSet.InternalHandleException;
begin
     Application.HandleException(Self);
end;


procedure TDADODataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
   i    : Integer;
   oTmp : TStringList;
begin
     oTmp := TStringList.Create;
     try
     begin
          oTmp.CommaText := PChar(Buffer);
          while FRecords.ADO.Fields.Count >= oTmp.Count do oTmp.Add('');
          FRecords.Insert;
          for i := 0 to FRecords.ADO.Fields.Count-1 do
          begin
               FRecords.ADO.Fields.Item[i].Value := oTmp.Strings[i];
          end;
     end;
     finally
          oTmp.Free;
     end;
     if Append then FRecords.Last;
end;


function TDADODataSet.GetRecordCount: Integer;
begin
     Result := -1;
     if Assigned(FRecords) then Result := FRecords.RecordCount;
end;


function TDADODataSet.GetRecNo: Integer;
begin
     Result := -1;
     if Assigned(FRecords) then
     begin
          Result := FRecords.RecNo;
     end;
end;


procedure TDADODataSet.SetRecNo(Value: Integer);
begin
     //*** Do Nothing
end;


procedure TDADODataSet.DataEvent(Event: TDataEvent; Info: Longint);
begin
     inherited DataEvent(Event, Info);
     if (Event in [deDataSetScroll,
                   deUpdateState,
                   deDataSetChange]) and Active then InternalSetToRecord(ActiveBuffer);
end;


{******************************************************************************}
{***                              Utilities                                 ***}
{******************************************************************************}


procedure TDADODataSet.InternalBookmark(Flag: TBookmarkFlag);
var
   pBuf : PChar;
begin
     pBuf := ActiveBuffer;
     PDInfo(pBuf + FInfoStart)^.BookData := FRecords.Bookmark;
     PDInfo(pBuf + FInfoStart)^.BookFlag := Flag;
end;


procedure TDADODataSet.InitOneField(const Name: String; ADOType: DataTypeEnum; Size: Integer; Required: Boolean; Index: Integer);
var
   xType : TFieldType;
   iLen  : Integer;
   sName : String;
begin
     sName := Trim(Name);
     xType := AdoToBdt(ADOType);
     if (Name = FPrimaryKey) and (xType = ftInteger) then xType := ftAutoInc;
     if xType <> ftUnknown then
     begin
          case xType of
               ftString :
               begin
                    iLen := Size;
                    if iLen = 0 then iLen := 1;
                    if iLen > 65536 then
                    begin
                         iLen := 0;
                         xType := ftMemo;
                    end;
               end;
               ftBytes :
               begin
                    iLen := Size;
               end;
               else iLen := 0;
          end;
          TFieldDef.Create(FieldDefs, sName, xType, iLen, Required, Index);
     end;
end;


procedure TDADODataSet.ClearCalcFields(Buffer: PChar);
begin
     FillChar(Buffer[FCalcStart], CalcFieldsSize, 0);
end;


// Convert and assign datatypes
function TDADODataSet.AdoToBdt(ADOType: DataTypeEnum): TFieldType;
begin
     Result := ftUnknown;
     if (FConnection.ADO.Provider = 'MSIDXS.1') then
     begin
          // Index server mapping
          case ADOType of
               // Standard datatypes
               adTinyInt,
               adSmallInt,
               adUnsignedTinyInt,
               adUnsignedSmallInt,
               adBigInt,
               adUnsignedBigInt,
               adInteger,
               adUnsignedInt         : Result := ftInteger;
               adCurrency            : Result := ftCurrency;
               adSingle,
               adDouble,
               adDecimal,
               adNumeric,
               adVarNumeric          : Result := ftFloat;
               adBoolean             : Result := ftBoolean;
               adVariant             : Result := ftString;
               adFileTime,
               adDBFileTime,
               adDate,
               adDBDate,
               adDBTime,
               adDBTimeStamp         : Result := ftDateTime;
               adBSTR,
               adChar,
               adWChar,
               adVarWChar,
               adVarChar             : Result := ftString;
               adLongVarWChar,
               adLongVarChar         : Result := ftMemo;
               adGUID                : Result := ftString;
               // Special datatypes
               adBinary,
               adVarBinary,
               adLongVarBinary       : Result := ftBlob; // ftBytes;
               adIUnknown            : Result := ftADT;
               adIDispatch           : Result := ftADT;
               // Unknown dataypes
               adEmpty,
               adError,
               adChapter,
               adPropVariant,
               adUserDefined         : Result := ftUnknown;
          end;
     end
     else
     begin
          // Standard data mapping
          case ADOType of
               // Standard datatypes
               adTinyInt,
               adSmallInt            : Result := ftSmallInt;
               adUnsignedTinyInt,
               adUnsignedSmallInt    : Result := ftWord;
               adBigInt,
               adUnsignedBigInt      : Result := ftLargeInt;
               adInteger,
               adUnsignedInt         : Result := ftInteger;
               adCurrency            : Result := ftCurrency;
               adSingle,
               adDouble,
               adDecimal,
               adNumeric,
               adVarNumeric          : Result := ftFloat;
               adBoolean             : Result := ftBoolean;
               adVariant             :
               begin
                    {$IFDEF VER130}    // Delphi 5
                    Result := ftVariant;
                    {$ENDIF}

                    {$IFDEF MSWINDOWS} // Delphi 6 and up
                    Result := ftVariant;
                    {$ELSE}
                    Result := ftADT;
                    {$ENDIF}
               end;
               adFileTime,
               adDBFileTime,
               adDate,
               adDBDate,
               adDBTime,
               adDBTimeStamp         : Result := ftDateTime;
               adBSTR,
               adChar,
               adWChar,
               adVarWChar,
               adVarChar             : Result := ftString;
               adLongVarWChar,
               adLongVarChar         : Result := ftMemo;
               adGUID                : Result := ftString;
               // Special datatypes
               adBinary,
               adVarBinary,
               adLongVarBinary       : Result := ftBlob; // ftBytes;
               adIUnknown            : Result := ftADT;
               adIDispatch           : Result := ftADT;
               // Unknown dataypes
               adEmpty,
               adError,
               adChapter,
               adPropVariant,
               adUserDefined         : Result := ftUnknown;
          end;
     end;
end;


// Convert and assign datatypes
function TDADODataSet.BdtToAdo(DataType: TFieldType): DataTypeEnum;
begin
     Result := adError;
     case DataType of
          ftSmallInt    : Result := adSmallInt;
          ftWord        : Result := adUnsignedSmallInt;
          ftLargeInt    : Result := adBigInt;
          ftInteger     : Result := adInteger;
          ftCurrency    : Result := adCurrency;
          ftFLoat       : Result := adDouble;
          ftBoolean     : Result := adBoolean;
          ftADT         : Result := adVariant;
          ftDate        : Result := adDBTimeStamp;
          ftTime        : Result := adDBTime;
          ftString      : Result := adChar;
          ftMemo        : Result := adLongVarChar;
          ftBlob        : Result := adLongVarBinary;
          ftGraphic     : Result := adLongVarBinary;
          ftBytes       : Result := adLongVarBinary;
          ftTypedBinary	: Result := adLongVarBinary;
          ftAutoInc     : Result := adGUID;
     end;
end;


function TDADODataSet.IntegerToBuffer(Buffer: Pointer; S: String = ''): Boolean;
begin
     Result := (S <> '');
     if S = '' then S := '0';
     Integer(Buffer^) := StrToInt(S);
end;


function TDADODataSet.FloatToBuffer(Buffer: Pointer; S: String = ''): Boolean;
begin
     Result := (S <> '');
     if S = '' then S := '0';
     Double(Buffer^) := StrToFloat(S);
end;


function TDADODataSet.DateToBuffer(Buffer: Pointer; S: String = ''): Boolean;
var
   xTmp : TTimeStamp;
   dTmp : ^TDateTimeRec;
begin
     Result := (S <> '');
     xTmp.Date := 0;
     xTmp.Time := 0;
     if S <> '' then xTmp := DateTimeToTimeStamp(StdToDate(S));
     dTmp := Buffer;
     dTmp.Date := xTmp.Date;
end;


function TDADODataSet.TimeToBuffer(Buffer: Pointer; S: String = ''): Boolean;
var
   xTmp : TTimeStamp;
   dTmp : ^TDateTimeRec;
begin
     Result := (S <> '');
     xTmp.Date := 0;
     xTmp.Time := 0;
     if S <> '' then
     begin
          if Length(S) > 10
          then
              xTmp := DateTimeToTimeStamp(StrToDateTime(S))
          else
              xTmp := DateTimeToTimeStamp(StrToDate(S));
     end;
     dTmp := Buffer;
     dTmp.Time := xTmp.Time;
end;


function TDADODataSet.DateTimeToBuffer(Buffer: Pointer; S: String = ''): Boolean;
var
   xTmp : TTimeStamp;
   dTmp : ^TDateTimeRec;
begin
     if S = '0.0' then S := '';
     Result := (S <> '');
     xTmp.Date := 0;
     xTmp.Time := 0;
     if S <> '' then xTmp := DateTimeToTimeStamp(StdToDate(S));
     dTmp := Buffer;
     if S <> ''
     then
         dTmp^.DateTime := TimeStampToMSecs(xTmp)
     else
         dTmp^.DateTime := 0;
end;


function TDADODataSet.BooleanToBuffer(Buffer: Pointer; S: String = ''): Boolean;
begin
     Result := (S <> '');
     if S = '' then S := 'False';
     if (S = 'True') or
        (S = 'true') or
        (S = 'T')    or
        (S = 't')    or
        (S = '-1')   or
        (S = '1')
     then
         S := cnsWordBoolTrue
     else
         S := cnsWordBoolFalse;
     WordBool(Buffer^) := WordBool(StrToInt(S));
end;


function TDADODataSet.BufferToDate(Buffer: Pointer): String;
var
   xTmp : TTimeStamp;
   dTmp : ^TDateTimeRec;
begin
     dTmp := Buffer;
     xTmp.Date := dTmp^.Date;
     Result := DateToStd(TimeStampToDateTime(xTmp));
end;


function TDADODataSet.BufferToTime(Buffer: Pointer): String;
var
   xTmp : TTimeStamp;
   dTmp : ^TDateTimeRec;
begin
     dTmp := Buffer;
     xTmp.Time := dTmp^.Time;
     Result := FormatDateTime('hh:nn:ss AM/PM', TimeStampToDateTime(xTmp));
end;


function TDADODataSet.BufferToDateTime(Buffer: Pointer): String;
var
   xTmp : TTimeStamp;
   dTmp : ^TDateTimeRec;
begin
     dTmp   := Buffer;
     xTmp   := MSecsToTimeStamp(dTmp.DateTime);
     Result := DateToStd(TimeStampToDateTime(xTmp));
end;


function TDADODataSet.CreateCriteria(const FieldName, SearchValue: String; PartialSearch: Boolean): String;
var
   oFld : TField;
begin
     Result := '';
     oFld := FindField(FieldName);
     if Assigned(oFld) then
     begin
          // Add Field
          if Pos(' ', FieldName) > 0
          then
              Result := '[' + FieldName + ']'
          else
              Result := FieldName;
          if PartialSearch then
          begin
               case oFld.DataType of
                    ftFmtMemo,
                    ftFixedChar,
                    ftWideString,
                    ftMemo,
                    ftString   : Result := Result + ' LIKE ';
                    ftBoolean  : Result := Result + ' = ';
               else Result := Result + ' >= ';
               end;
          end
          else
          begin
               Result := Result + ' = ';
          end;

          // Add Value(s)
          case oFld.DataType of
               ftFmtMemo,
               ftFixedChar,
               ftWideString,
               ftMemo,
               ftString :
               begin
                    if PartialSearch
                    then
                        Result := Result + '''' + SearchValue + '%'''
                    else
                        Result := Result + '''' + SearchValue + '''';
               end;
               ftTime,
               ftDate,
               ftDateTime :
               begin
                    Result := Result + '#' + SearchValue + '#';  // Date & Time
               end;
          else
               Result := Result + SearchValue;      // Numeric or Logical data
          end;
     end;
end;


function TDADODataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
     Result := TDBlobStream.Create(Field as TBlobField, Mode);
end;


function TDADODataSet.GetBlobData(Field: TField): TBlobData;
begin
     Result := '';
     if FBlobData.IndexOfName(Field.FieldName) > -1 then Result := FBlobData.Values[Field.FieldName];
     if Result = cnsEmptyBlob then Result := '';
end;


procedure TDADODataSet.SetBlobData(Field: TField; Value: TBlobData);
begin
     if FBlobData.IndexOfName(Field.FieldName) > -1 then
     begin
          if Value = ''
          then
              FBlobData.Values[Field.FieldName] := cnsEmptyBlob
          else
              FBlobData.Values[Field.FieldName] := Value;
     end;
end;


function TDADODataSet.BlobToVariant(Field: TField): OleVariant;
var
   sTmp : String;
   pTmp : Pointer;
begin
     sTmp := GetBlobData(Field);
     case Field.DataType of
          ftMemo : 
          begin
               if sTmp=''
               then
                   Result := Null
               else
                   Result := sTmp;
          end;
          ftBlob, ftGraphic:
          begin
               Result := VarArrayCreate([0,0], varByte);
               VarArrayRedim(Result, VarArrayHighBound(Result, 1) + Length(sTmp));
               pTmp := VarArrayLock(Result);
               CopyMemory(pTmp, PChar(sTmp), Length(sTmp));
               VarArrayUnlock(Result);
          end;
     end;
end;


function TDADODataSet.GenerateField(const TableName, FieldName: String): String;
var
   sTab : String;
   sFld : String;
begin
     sTab := TableName;
     sFld := FieldName;
     if IsSub(' ', sTab) then sTab := '"' + sTab + '"';
     if IsSub(' ', sFld) then sFld := '"' + sFld + '"';
     Result := sTab + '.' + sFld;
end;


function TDADODataSet.GenerateField(const LongName: String): String;
begin
     Result := GenerateField(ExtractTable(LongName), ExtractField(LongName));
end;


function TDADODataSet.CleanSQL(const SQLText: String): String;
begin
     Result := Trim(Equalize(StrTran(SQLText, #$D#$A, ' ')));
     if Pos(#0, Result) > 0 then Result := Copy(Result, 1, Pos(#0, Result)-1);
end;


{******************************************************************************}
{***                            Virtual methods                             ***}
{******************************************************************************}


procedure TDADODataSet.InternalFields;
var
   i    : Integer;
   sTab : String;
begin
     sTab := InternalTable(FCommand);
     for i := 0 to Fields.Count-1 do
     begin
          if sTab = ''
          then
              Fields[i].Origin := Fields[i].FieldName
          else
              Fields[i].Origin := sTab + '.' + Fields[i].FieldName;

          if Fields[i].FieldName = MarkerField
          then
              Fields[i].DisplayLabel := cnsMarkerSpace
          else
              Fields[i].DisplayLabel := Trim(Fields[i].DisplayLabel);
          Fields[i].Tag := SortField(Fields[i]);
     end;
end;


function TDADODataSet.InternalTable(const SQLText: String): String;
begin
     if FCommandOption in [coTable, coTableDirect]
     then
         Result := SQLText
     else
         Result := ExtractTableName(SQLText, True);
end;


procedure TDADODataSet.InternalLoad(const FileName: String);
var
  oStream : TFileStream;
  Wrapper : TDADOWrapper;
begin
     if FileExists(FileName) then
     begin
          oStream := TFileStream.Create(FileName, fmOpenRead);
          try
          begin
               Wrapper := TDADOWrapper.Create(nil);
               try
               begin
                    oStream.ReadComponent(Wrapper);
                    Colors  := Wrapper.Colors;
                    Filters := Wrapper.Filters;
                    Columns := Wrapper.Columns;
                    Sort    := Wrapper.Sort;
                    if Wrapper.Filter <> '' then Filter := Wrapper.Filter;
               end;
               finally
                    Wrapper.Free;
               end;
          end;
          finally
               oStream.Free;
          end;
     end;
end;


procedure TDADODataSet.InternalSave(const FileName: String);
var
  oStream : TFileStream;
  Wrapper : TDADOWrapper;
begin
     if FileName = '' then Exit;
     ForceDirectories(ExtractFileDir(FileName));
     oStream := TFileStream.Create(FileName, fmCreate);
     try
     begin
          Wrapper := TDADOWrapper.Create(nil);
          try
          begin
               Wrapper.Colors  := Colors;
               Wrapper.Filters := Filters;
               Wrapper.Columns := Columns;
               Wrapper.Sort    := Sort;
               Wrapper.Filter  := Filter;
               oStream.WriteComponent(Wrapper);
          end;
          finally
               Wrapper.Free;
          end;
     end;
     finally
          oStream.Free;
     end;
end;


{******************************************************************************}
{***                          SQL Edit routines                             ***}
{******************************************************************************}


function TDADODataSet.InsertSQL: String;
var
   i    : Integer;
   oFld : TField;
   sFld : String;
   sVal : String;
   sTab : String;
begin
     Result := '';
     sFld := '';
     sVal := '';
     sTab := InternalTable(FCommand);
     if sTab = '' then Exit;
     for i := 0 to FRecords.ADO.Fields.Count-1 do
     begin
          oFld := FindField(FRecords.ADO.Fields.Item[i].Name);
          if Assigned(oFld) and (ExtractTable(Fields[i].Origin) = sTab) and (Fields[i].DataType <> ftAutoInc) then
          begin
               sFld := sFld + oFld.FieldName + ', ';
               case oFld.DataType of
                    ftDate, ftDateTime : sVal := sVal + '#' + DateToStd(oFld.AsDateTime, '-') + '#, ';
                    ftString           : sVal := sVal + '''' + oFld.AsString + ''', ';
                    ftBoolean          : sVal := sVal + IIF(oFld.AsBoolean, cnsWordBoolTrue, cnsWordBoolFalse) + ', ';
               else sVal := sVal + oFld.AsString + ', ';
               end;
          end;
     end;
     if RightStr(sFld, 2) = ', ' then sFld := LeftStr(sFld, Length(sFld)-2);
     if RightStr(sVal, 2) = ', ' then sVal := LeftStr(sVal, Length(sVal)-2);
     Result := 'INSERT INTO ' + sTab + ' ( ' + sFld + ' ) VALUES ( ' + sVal + ' ) ';
end;


function TDADODataSet.ModifySQL: String;
var
   i    : Integer;
   oFld : TField;
   sTab : String;
begin
     Result := '';
     sTab := InternalTable(FCommand);
     if sTab = '' then Exit;
     Result := 'UPDATE ' + sTab + ' SET ';
     for i := 0 to FRecords.ADO.Fields.Count-1 do
     begin
          oFld := FindField(FRecords.ADO.Fields.Item[i].Name);
          if Assigned(oFld) and (oFld.Value <> FRecords.ADO.Fields.Item[i].Value) then
          begin
               case oFld.DataType of
                    ftDate,
                    ftDateTime  : Result := Result + oFld.Origin + ' = #' + DateToStd(oFld.AsDateTime, '-') + '#, ';
                    ftMemo      : Result := Result + oFld.Origin + ' = ''' + GetBlobData(oFld) + ''', ';
                    ftString    : Result := Result + oFld.Origin + ' = ''' + oFld.AsString + ''', ';
                    ftBoolean   : Result := Result + oFld.Origin + ' = ' + IIF(oFld.AsBoolean, cnsWordBoolTrue, cnsWordBoolFalse) + ', ';
                    else Result := Result + oFld.Origin + ' = ' + oFld.AsString + ', ';
               end;
          end;
     end;
     if RightStr(Result, 2) = ', ' then Result := LeftStr(Result, Length(Result)-2);
     Result := Result + GenerateUpdate;
end;


function TDADODataSet.DeleteSQL: String;
var
   sTab : String;
begin
     Result := '';
     sTab := InternalTable(FCommand);
     if sTab = '' then Exit;
     Result := 'DELETE FROM ' + sTab + GenerateUpdate;
end;


function TDADODataSet.GenerateUpdate: String;
var
   i    : Integer;
   oFld : TField;
   sTmp : String;
   sTab : String;
begin
     Result := '';
     sTab := InternalTable(FCommand);
     if sTab = '' then Exit;
     Result := ' WHERE ';
     for i := 0 to FRecords.ADO.Fields.Count-1 do
     begin
          oFld := FieldByName(FRecords.ADO.Fields.Item[i].Name);
          if Assigned(oFld) and (ExtractTable(oFld.Origin) = sTab) then
          begin
               sTmp := VarToStr(FRecords.ADO.Fields.Item[i].Value);
               case oFld.DataType of
                    ftDate,
                    ftDateTime : Result := Result + oFld.Origin + ' = #' + DateToStd(StrToDateTime(sTmp), '-') + '# AND ';
                    ftString   : if sTmp <> '' then Result := Result + oFld.Origin + ' = ''' + sTmp + ''' AND ';
                    ftMemo     : ; //*** Do Nothing
                    ftBoolean  : Result := Result + oFld.Origin + ' = ' + sTmp + ' AND ';
               else Result := Result + oFld.Origin + ' = ' + sTmp + ' AND ';
               end;
          end;
     end;
     Result := TrimRight(Result);
     if RightStr(Result, 3) = 'AND' then Result := LeftStr(Result, Length(Result)-3);
end;


function TDADODataSet.GenerateWhere(SQLText: String = ''; JoinList: TStrings = nil): String;
begin
     Result := '';
     if Filters.Count > 0 then
     begin
          Result := ' WHERE ' + Filters.SQLText(fmADO, (foCaseInsensitive in FilterOptions));
     end;
     Result := TrimRight(Result);
     if RightStr(Result, 3) = 'AND' then Result := LeftStr(Result, Length(Result)-3);
end;


{******************************************************************************}
{***                          Event Methods                                 ***}
{******************************************************************************}


procedure TDADODataSet.DoBeforeEdit;
begin
     inherited DoBeforeEdit;
     DoDialog;
end;


procedure TDADODataSet.DoBeforeInsert;
begin
     inherited DoBeforeInsert;
     DoDialog;
end;


procedure TDADODataSet.DoAfterScroll;
begin
     inherited DoAfterScroll;
     if Assigned(evOnScroll) then OnScroll(Self);
end;


procedure TDADODataSet.DoAfterOpen;
begin
     inherited DoAfterOpen;
     SetCurrentRecord(0);
     if Assigned(evOnScroll) then OnScroll(Self);
end;


procedure TDADODataSet.DoAfterClose;
begin
     inherited DoAfterClose;
     if Assigned(evOnScroll) then OnScroll(Self);
end;


procedure TDADODataSet.DoDialog;
begin
     if (opDialog in FOptions) and Assigned(evOnDialog) then OnDialog(Self);
end;

{******************************************************************************}
{***                            Extra Features                              ***}
{******************************************************************************}


function TDADODataSet.Search(const FieldName, SearchStr: String; PartialSearch: Boolean = False; LeadZero: Byte = 0): Boolean;
var
   sValue : String;
begin
     Result := False;
     if not FActive then Exit;
     if (SearchStr <> '') and (FieldName <> '') then
     begin
          if LeadZero > 0 then sValue := FillZero(SearchStr, LeadZero) else sValue := SearchStr;
          // Searching
          Result := Find(CreateCriteria(FieldName, sValue, PartialSearch));
     end;
end;


function TDADODataSet.Search(Field: TField; const SearchStr: String; PartialSearch: Boolean = False; LeadZero: Byte = 0): Boolean;
begin
     Result := False;
     if Assigned(Field) then Result := Search(Field.FieldName, SearchStr, PartialSearch, LeadZero);
end;


function TDADODataSet.Search(const SearchExpr: String): Boolean;
begin
     Result := Find(SearchExpr);
end;

function TDADODataSet.Find(const SearchStr: String): Boolean;
var
   iBook : Integer;
begin
     Result := False;
     if FActive then
     begin
          // Searching
          if SearchStr <> '' then
          begin
               if Assigned(evOnVerbose) then OnVerbose(SearchStr, vmSearch);
               if FRecords.HaveRecords then
               begin
                    DoBeforeScroll;
                    if (Pos('AND', AnsiUpperCase(SearchStr)) > 0) or
                       (Pos('OR' , AnsiUpperCase(SearchStr)) > 0) then
                    begin
                         DisableControls;
                         try
                              iBook := FRecords.FilterFind(SearchStr);
                         finally
                              EnableControls;
                         end;
                         Result := (iBook > 0);
                         FRecords.Bookmark := iBook;
                    end
                    else Result := FRecords.Find(SearchStr);
                    if Result then Resync(FResync);
                    DoAfterScroll;
               end;
          end;
     end;
end;


function TDADODataSet.InternalSQL(const SQLText: String): Integer;
var
   sSql : String;
begin
     Result := -1;
     if SQLText <> '' then
     begin
          if ConnectionOpen then
          begin
               sSql := CleanSQL(SQLText);
               if Assigned(evOnVerbose) then OnVerbose(sSql, vmExecute);
               FConnection.StartTransaction;
               try
               begin
                    Result := FConnection.ExecSQL(sSql);
                    FConnection.Commit;
               end;
               except
                    FConnection.Rollback;
                    raise;
               end;
          end;
     end;
end;


procedure TDADODataSet.ReOpen;
begin
     DisableControls;
     try
     begin
          Close;
          Open;
     end;
     finally
          EnableControls;
     end;
end;


procedure TDADODataSet.ExecFilter;
var
   sTmp : String;
   sOld : String;
begin
     sOld := '';
     sTmp := '';
     if opFilter in FOptions then
     begin
          if FFilter <> '' then sTmp := FFilter + IIF(FFilters.Count > 0, ' AND ', '');
          sTmp := sTmp + FFilters.SQLText(fmADO, (foCaseInsensitive in FilterOptions));
     end
     else sTmp := FFilter;
     if Assigned(evOnVerbose) then OnVerbose(sTmp, vmFilter);
     if Assigned(FRecords) then
     begin
          sOld := FRecords.Filter;
          FRecords.Filter := sTmp;
     end;
     if FActive and (sOld <> sTmp) then First;
end;


procedure TDADODataSet.SortAdd(const FieldName: String; SortMode: TSortMode; AddField: Boolean = False);
var
   sTmp : String;
   oTmp : TStringList;
   iPos : Integer;
begin
     oTmp := TStringList.Create;
     try
     begin
          oTmp.CommaText := StrTran(FSort, ' ', '=');
          sTmp := 'ASC';
          case SortMode of
               smASC  : sTmp := cnsSortASC;
               smDESC : sTmp := cnsSortDESC;
          end;
          if not AddField then
          begin
               oTmp.Clear;
               oTmp.Add(FieldName + '=' + sTmp);
          end
          else
          begin
               iPos := oTmp.IndexOfName(FieldName);
               if iPos > -1
               then
                   oTmp.Strings[iPos] := FieldName + '=' + sTmp
               else
                   oTmp.Add(FieldName + '=' + sTmp);
          end;
          Sort := StrTran(oTmp.CommaText, '=', ' ');
     end;
     finally
          oTmp.Free;
     end;
end;


procedure TDADODataSet.SortAdd(Field: TField; AddField: Boolean = False);
var
   xMode : TSortMode;
begin
     xMode := smASC;
     if not Assigned(Field) then Exit;
     case Field.Tag of
          cniSortNONE : xMode := smASC;
          cniSortASC  : xMode := smDESC;
          cniSortDESC : xMode := smASC;
     end;
     SortAdd(Field.FieldName, xMode, AddField);
end;


procedure TDADODataSet.SortDelete(const FieldName: String);
var
   oTmp : TStringList;
   iPos : Integer;
begin
     oTmp := TStringList.Create;
     try
     begin
          oTmp.CommaText := StrTran(FSort, ' ', '=');
          iPos := oTmp.IndexOfName(FieldName);
          if iPos > -1 then oTmp.Delete(iPos);
          Sort := StrTran(oTmp.CommaText, '=', ' ');
     end;
     finally
          oTmp.Free;
     end;
end;


procedure TDADODataSet.SortDelete(Field : TField);
begin
     SortDelete(Field.FieldName);
end;


function TDADODataSet.IsSorted(const FieldName: String): Boolean;
begin
     Result := IsSub(FieldName, FSort);
end;


function TDADODataSet.IsSorted(Field: TField): Boolean;
begin
     Result := IsSorted(Field.FieldName);
end;


function TDADODataSet.SortMode(const FieldName: String): TSortMode;
var
   oTmp : TStringList;
begin
     Result := smNONE;
     oTmp := TStringList.Create;
     try
     begin
          oTmp.CommaText := StrTran(FSort, ' ', '=');
          if oTmp.IndexOfName(FieldName) > -1 then
          begin
               if oTmp.Values[FieldName] = cnsSortASC  then Result := smASC;
               if oTmp.Values[FieldName] = cnsSortDESC then Result := smDESC;
          end;
     end;
     finally
          oTmp.Free;
     end;
end;


function TDADODataSet.SortMode(Field: TField): TSortMode;
begin
     Result := SortMode(Field.FieldName);
end;


function TDADODataSet.SortField(Field: TField): Integer;
begin
     Result := cniSortNONE;
     if IsSorted(Field.FieldName) then
     begin
          case SortMode(Field.FieldName) of
               smASC  : Result := cniSortASC;
               smDESC : Result := cniSortDESC;
          end;
     end;
end;


function TDADODataSet.GetPrimaryKey: String;
begin
     Result := FPrimaryKey;
end;


procedure TDADODataSet.SetPrimaryKey(const Value: String);
begin
     if FPrimaryKey <> Value then FPrimaryKey := Value;
end;


function TDADODataSet.GetMarkerField: String;
begin
     Result := FMarkerField;
end;


procedure TDADODataSet.SetMarkerField(const Value: String);
begin
     if FMarkerField <> Value then FMarkerField := Value;
end;


function TDADODataSet.FilterEditor(Field: TField): Boolean;
var
   oEditor : TDFilterEditor;
begin
     oEditor := TDFilterEditor.Create(Self);
     try
     begin
          oEditor.Field     := Field;
          oEditor.Marker    := MarkerField;
          oEditor.DataSet   := Self;
          Result := oEditor.Execute;
          if Result then
          begin
               FFilters.Add(oEditor.SubFilter);
               ExecFilter;
          end;
     end;
     finally
          oEditor.Free;
     end;
end;


function TDADODataSet.FilterEditor(const FieldName: String): Boolean;
begin
     Result := FilterEditor(FindField(FieldName));
end;


function TDADODataSet.FilterEditor: Boolean;
begin
     Result := FilterEditor(nil);
end;


function TDADODataSet.ColorEditor: Boolean;
var
  oEditor : TDColorEditor;
begin
     oEditor := TDColorEditor.Create(Self);
     try
     begin
          oEditor.DataSet := Self;
          oEditor.Colors  := FColors;
          oEditor.Marker  := MarkerField;
          Result := oEditor.Execute;
          if Result then First;
     end;
     finally
          oEditor.Free;
     end;
end;


procedure TDADODataSet.ExportEditor;
var
   oEditor : TDExportEditor;
begin
     oEditor := TDExportEditor.Create(Self);
     try
     begin
          oEditor.DataSet := Self;
          oEditor.Execute;
     end;
     finally
          oEditor.Free;
     end;
end;


procedure TDADODataSet.FillUpEditor(const FieldName: String);
begin
     FillUpEditor(FindField(FieldName));
end;


procedure TDADODataSet.FillUpEditor(Field: TField);
var
   oEditor: TDFillUpEditor;
   sFld   : String;
begin
     oEditor := TDFillUpEditor.Create(Self);
     try
     begin
          oEditor.Field   := Field;
          oEditor.DataSet := Self;
          if oEditor.Execute then
          begin
               sFld := Field.FieldName;
               case oEditor.Mode of
                    fmUpper : FillUpper(sFld);
                    fmLower : FillLower(sFld);
                    fmConst : FillConst(sFld, oEditor.Text, Field.DataType = ftString);
                    fmSQL   : FillSQL(sFld, oEditor.Lines);
               end;
          end;
     end;
     finally
          oEditor.Free;
     end;
end;


procedure TDADODataSet.FillUpper(const FieldName: String);
var
   sTmp: String;
   sTab: String;
begin
     sTab := InternalTable(FCommand);
     if sTab = '' then Exit;
     sTmp := 'UPDATE ' + sTab + ' SET ' + FieldName + ' = UCASE(' + FieldName + ')' + GenerateWhere;
     FillExec(sTmp);
end;


procedure TDADODataSet.FillLower(const FieldName: String);
var
   sTmp : String;
   sTab : String;
begin
     sTab := InternalTable(FCommand);
     if sTab = '' then Exit;
     sTmp := 'UPDATE ' + sTab + ' SET ' + FieldName + ' = LCASE(' + FieldName + ')' + GenerateWhere;
     FillExec(sTmp);
end;


procedure TDADODataSet.FillConst(const FieldName, FieldValue: String; StrType: Boolean = True);
var
   sTmp : String;
   sTab : String;
begin
     sTab := InternalTable(FCommand);
     if sTab = '' then Exit;
     if StrType
     then
         sTmp := 'UPDATE ' + sTab + ' SET ' +
                             FieldName + ' = ''' + FieldValue + '''' +
                             GenerateWhere
     else
         sTmp := 'UPDATE ' + sTab + ' SET ' +
                             FieldName + ' = ' + FieldValue + GenerateWhere;
     FillExec(sTmp);
end;


procedure TDADODataSet.FillExchange(const FieldName, FromValue, ToValue: String);
var
   sTmp : String;
   sTab : String;
begin
     sTab := InternalTable(FCommand);
     if sTab = '' then Exit;
     sTmp := 'UPDATE ' + sTab + ' SET '   +
                         FieldName + ' = {fn REPLACE(' +
                         FieldName + ', ''' + FromValue + ''', ''' + ToValue + ''')}' +
                         GenerateWhere;
     FillExec(sTmp);
end;


procedure TDADODataSet.FillSQL(const FieldName: String; SQLItems: TStrings);
var
   sTmp : String;
   sTab : String;
begin
     sTab := InternalTable(FCommand);
     if (sTab = '') or (SQLItems.Count = 0) then Exit;
     sTmp := 'UPDATE ' + sTab + ' SET ' +
                         FieldName + ' = ' +
                         CleanSQL(SQLItems.Text) + ' ' +
                         GenerateWhere;
     FillExec(sTmp);
end;


// Execute Fill command
procedure TDADODataSet.FillExec(const SQLText: String);
begin
     DisableControls;
     try
     begin
          if Assigned(evOnFillStart) then OnFillStart(Self);
          Close;
          InternalSQL(SQLText);
          Open;
          if Assigned(evOnFillEnd) then OnFillEnd(Self);
     end;
     finally
          EnableControls;
     end;
end;


procedure TDADODataSet.Markers(const Value: String);
var
   sTmp : String;
   sTab : String;
begin
     sTab := InternalTable(FCommand);
     if sTab = '' then Exit;
     if Active and (MarkerField <> '') then
     begin
          sTmp := 'UPDATE ' + sTab + ' SET ' +
                              MarkerField + ' = ''' + Value + '''' + GenerateWhere;
          DisableControls;
          try
          begin
               if Assigned(evOnMarkStart) then OnMarkStart(Self);
               Close;
               InternalSQL(sTmp);
               Open;
               if Assigned(evOnMarkEnd) then OnMarkEnd(Self);
          end;
          finally
               EnableControls;
          end;
     end;
end;


procedure TDADODataSet.FieldColors(const FieldName: String; var PaperColor, InkColor: TColor);
var
   i : Integer;
begin
     for i := 0 to Colors.Count-1 do
     begin
          if (Colors.Items[i].Selected = cnsAllItem) or
          IsSub(FieldName, Colors.Items[i].Selected) then
          begin
               if Colors.Evaluate(i, Self) then
               begin
                    PaperColor := Colors.Items[i].BackGround;
                    InkColor   := Colors.Items[i].ForeGround;
               end;
          end;
     end;
end;


function TDADODataSet.GetPosition: String;
begin
     Result := '';
     if FActive then
     begin
          if Filters.Count = 0
          then
              Result := IntToStr(RecNo) + '/' + IntToStr(RecordCount)
          else
              Result := IntToStr(RecNo) + '/' + IntToStr(RecordCount) + ' (' + IntToStr(Filters.Count) + ')';
     end;
end;


procedure TDADODataSet.SubListOpen;
var
   i : Integer;
begin
     for i := 0 to FSubList.Count-1 do
     begin
          with FSubList.Items[i] do
          begin
               if Assigned(DataSet) and (not DataSet.Active) then
               begin
                    if Background
                    then
                        TDADOThread.Create(DataSource)
                    else
                    begin
                         if DataSet is TDADODataSet
                         then
                             TDADODataSet(DataSet).Open
                         else
                             DataSet.Open;
                    end;
               end;
          end;
     end;
end;


procedure TDADODataSet.SubListClose;
var
   i : Integer;
begin
     for i := FSubList.Count-1 downto 0 do
     begin
          with FSubList.Items[i] do
          begin
               if Assigned(DataSet) and DataSet.Active then
               begin
                    if DataSet is TDADODataSet
                    then
                        TDADODataSet(DataSet).Close
                    else
                        DataSet.Close;
               end;
          end;
     end;
end;


{******************************************************************************}
{***                          ADO Service functions                         ***}
{******************************************************************************}


function TDADODataSet.ConnectionOpen: Boolean;
begin
     if FConnect <> '' then
     begin
          if not Assigned(FConnection) then
          begin
               if (opConnection in FOptions) and Assigned(FMaster)
               then
                   FConnection := FMaster.CentralData
               else
               begin
                    FConnection := TDConnection.Create(True);
                    FConnection.CursorLocation := FCursorLocation;
                    FConnection.Connection     := FConnect;
               end;
          end;
          if FConnection.Connection <> FConnect then FConnection.Connection := FConnect;
          if not FConnection.Active then FConnection.Open;
     end;
     Result := (Assigned(FConnection) and FConnection.Active);
end;


procedure TDADODataSet.ConnectionClose;
begin
     if not Assigned(FConnection) then Exit;
     if FConnection.IsLocale then
     begin
          if FConnection.Active then FConnection.Close;
          FConnection.Free;
          FConnection := nil;
     end;
end;


procedure TDADODataSet.GetOdbcDriverList(List: TStrings);
begin
     if ConnectionOpen then FConnection.GetOdbcDriverList(List);
end;


function TDADODataSet.GetOdbcDriverExt(const Driver: String): String;
begin
     if ConnectionOpen then FConnection.GetOdbcDriverExt(Driver);
end;


function TDADODataSet.GetOdbcDriverFilter(const Driver: String): String;
begin
     Result := '';
     if ConnectionOpen then Result := FConnection.GetOdbcDriverFilter(Driver);
end;


procedure TDADODataSet.GetOdbcDSNList(List: TStrings);
begin
     if ConnectionOpen then FConnection.GetOdbcDSNList(List);
end;


procedure TDADODataSet.GetADOProviderList(List: TStrings);
begin
     if ConnectionOpen then FConnection.GetADOProviderList(List);
end;


procedure TDADODataSet.GetADOProviderList(Name, Code: TStrings);
begin
     if ConnectionOpen then FConnection.GetADOProviderList(Name, Code);
end;


procedure TDADODataSet.GetADOTableNames(List: TStrings);
begin
     if ConnectionOpen then FConnection.GetADOTableNames(List);
end;


procedure TDADODataSet.GetADOFieldNames(const TableName: String; List: TStrings);
begin
     if ConnectionOpen then FConnection.GetADOFieldNames(TableName, List);
end;


procedure TDADODataSet.GetADOFieldNames(List: TStrings);
var
   sTab : String;
begin
     sTab := InternalTable(FCommand);
     if sTab <> '' then GetADOFieldNames(sTab, List);
end;


procedure TDADODataSet.SetUpdateObject(Value: TDUpdateObject);
begin
     if Value <> FUpdateObject then
     begin
          if Assigned(FUpdateObject) and
          (FUpdateObject.DataSet = Self) then FUpdateObject.DataSet := nil;
          FUpdateObject := Value;
          if Assigned(FUpdateObject) then
          begin
               if Assigned(FUpdateObject.DataSet) and
                  (FUpdateObject.DataSet <> Self) then FUpdateObject.DataSet.UpdateObject := nil;
               FUpdateObject.DataSet := Self;
          end;
     end;
end;


function TDADODataSet.LoadUDLFile(const FileName: String; UserName: String = ''; Password: String = ''): String;
begin
     Result := '';
     if ConnectionOpen then Result := FConnection.LoadUDLFile(FileName, UserName, Password);
end;


procedure TDADODataSet.UDLEditor(const FileName: String);
begin
     if ConnectionOpen then FConnection.UDLEditor(FileName);
end;


{******************************************************************************}
{***                             Wrapper Properties                         ***}
{******************************************************************************}


constructor TDADOWrapper.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FColors   := TDFilters.Create(Self);
     FFilters  := TDFilters.Create(Self);
     FColumns  := TColumnData.Create(Self);
     FSort     := '';
     FFilter   := '';
end;


destructor TDADOWrapper.Destroy;
begin
     FFilters.Free;
     FColors.Free;
     FColumns.Free;
     inherited Destroy;
end;


procedure TDADOWrapper.SetColors(Value: TDFilters);
begin
     FColors.Assign(Value);
end;


procedure TDADOWrapper.SetFilters(Value: TDFilters);
begin
     FFilters.Assign(Value);
end;


procedure TDADOWrapper.SetColumns(Value: TColumnData);
begin
     FColumns.Assign(Value);
end;


{******************************************************************************}
{***                             Blob Stream                                ***}
{******************************************************************************}


constructor TDBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
var
   vTmp : OleVariant;
   pTmp : Pointer;
   iLen : Cardinal;
begin
     FBlobData := '';
     FMode     := Mode;
     FField    := Field;
     FDataSet  := FField.DataSet as TDADODataSet;
     FBuffer   := FDataSet.GetActiveRecordBuffer;
     if not ((FBuffer = nil) or (FBuffer^ = #0)) then FDataSet.InternalSetToRecord(FBuffer);
     FOpened := True;
     if not FField.Modified then
     begin
          if not FDataSet.Records.OkRecord or (FDataSet.Records.State = dsInsert)
          then
              FBlobData := ''
          else
          begin
               case FField.DataType of
                    ftMemo : FBlobData := VarToStr(FDataSet.Records.ADO.Fields[FField.FieldName].Value);
                    ftBlob, ftGraphic :
                    begin
                         vTmp := FDataSet.Records.ADO.Fields[FField.FieldName].Value;
                         if vTmp = NULL then FBlobData := '' else
                         begin
                              iLen := FDataSet.Records.ADO.Fields[FField.FieldName].ActualSize;
                              pTmp := VarArrayLock(vTmp);
                              SetLength(FBlobData, iLen + 1);
                              CopyMemory(PChar(FBlobData), PChar(pTmp), iLen);
                              VarArrayUnlock(vTmp);
                         end;
                    end;
               end;
          end;
          FDataSet.SetBlobData(FField, FBlobData);
     end;
     if Mode = bmWrite then Truncate;
end;


destructor TDBlobStream.Destroy;
begin
     if FOpened then
     begin
          if FModified then FField.Modified := True;
     end;
     if FModified then
     try
          FDataSet.DataEvent(deFieldChange, Longint(FField));
     except
          Application.HandleException(Self);
     end;
end;


function TDBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
     Result := 0;
     if FOpened then
     begin
          if Count > Size - FPosition then
             Result := Size - FPosition else
             Result := Count;
          if Result > 0 then
          begin
               Move(PChar(FDataSet.GetBlobData(FField))[FPosition], Buffer, Result);
               Inc(FPosition, Result);
          end;
     end;
end;


function TDBlobStream.Write(const Buffer; Count: Longint): Longint;
var
   pTemp : Pointer;
begin
     Result := 0;
     if FOpened then
     begin
          GetMem(pTemp, Count);
          try
          begin


               CopyMemory(pTemp, @Buffer, Count);
               SetLength(FBlobData, FPosition+Count );
// gld add
               CopyMemory(PChar(FBlobData)+FPosition, PChar(pTemp), count);
// -              FBlobData := PChar(pTemp);
// -              FBlobData := LeftStr(FBlobData, Count);
               FDataSet.SetBlobData(FField, FBlobData);
               FModified := True;
          end;
          finally
               FreeMem(pTemp, Count);
          end;
          Inc(FPosition, Count);
          Result := Count;
          FModified := True;
     end;
end;


function TDBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
     case Origin of
          0: FPosition := Offset;
          1: Inc(FPosition, Offset);
          2: FPosition := GetBlobSize + Offset;
     end;
     Result := FPosition;
end;

// from the original
function TDBlobStream.CopyFrom(Source: TStream; Count: Longint): Longint;
const
  MaxBufSize = $F000; // this was the limitation
var
  BufSize, N: Integer;
  Buffer: PChar;
begin
     if Count = 0 then
     begin
          Source.Position := 0;
          Count := Source.Size;
     end;
     Result := Count;
     // Buf size always equal with the count
     //if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
     BufSize := Count;
     GetMem(Buffer, BufSize);
     try
        while Count <> 0 do
        begin
             if Count > BufSize then N := BufSize else N := Count;
             Source.ReadBuffer(Buffer^, N);
             WriteBuffer(Buffer^, N);
             Dec(Count, N);
        end;
     finally
        FreeMem(Buffer, BufSize);
     end;
end;


procedure TDBlobStream.Truncate;
begin
     if FOpened then FModified := True;
end;


function TDBlobStream.GetBlobSize: Longint;
begin
     Result := 0;
     if FOpened then Result := Length(FDataSet.GetBlobData(FField));
end;


{******************************************************************************}
{***                      Grid Column store collection                      ***}
{******************************************************************************}


constructor TColumnData.Create(AOwner: TComponent);
begin
     inherited Create(TColumn);
     FOwner := AOwner;
end;


procedure TColumnData.Assign(Source: TPersistent);
var
   i     : Integer;
   oItem : TColumn;
begin
     if not Assigned(Source) then Exit;
     if Source is TColumnData then
     begin
          Clear;
          for i := 0 to TColumnData(Source).Count-1 do
          begin
               oItem := Add;
               oItem.Assign(TColumnData(Source).Items[i]);
          end;
     end
     else inherited Assign(Source);
end;


function TColumnData.Add: TColumn;
begin
     Result := TColumn(inherited Add);
end;


function TColumnData.GetOwner:TPersistent;
begin
     Result := FOwner;
end;


function TColumnData.GetItem(Index: Integer): TColumn;
begin
     Result := TColumn(inherited GetItem(Index));
end;


procedure TColumnData.SetItem(Index: Integer; Value: TColumn);
begin
     inherited SetItem(Index, Value);
end;


{******************************************************************************}
{***                         DataSet List Collection                        ***}
{******************************************************************************}


constructor TDSubItem.Create(Collection: TCollection);
begin
     inherited Create(Collection);
     FDataLink := TDataLink.Create;
     FBack     := True;
end;


destructor TDSubItem.Destroy;
begin
     FDataLink.Free;
     FDataLink := nil;
     inherited Destroy;
end;


procedure TDSubItem.Assign(Source: TPersistent);
begin
     if Source is TDSubItem then
     begin
          FDataLink := TDSubItem(Source).FDataLink;
          FBack     := TDSubItem(Source).FBack;
     end
     else inherited Assign(Source);
end;


function TDSubItem.GetDisplayName: String;
begin
     Result := '';
     if Assigned(FDataLink.DataSource) then Result := FDataLink.DataSource.Name;
     if Result = '' then Result := inherited GetDisplayName;
end;


function TDSubItem.GetDataSource: TDataSource;
begin
     Result := FDataLink.DataSource;
end;


procedure TDSubItem.SetDataSource(Value: TDataSource);
begin
     FDataLink.DataSource := Value;
end;


function TDSubItem.GetDataSet: TDataSet;
begin
     Result := FDataLink.DataSet;
end;


function TDSubItem.GetItemName: String;
begin
     Result := GetDisplayName;
end;


{******************************************************************************}


constructor TDSubList.Create(DataSet: TDataSet);
begin
     inherited Create(TDSubItem);
     FOwner := DataSet;
end;


function TDSubList.Add: TDSubItem;
begin
     Result := TDSubItem(inherited Add);
end;


function TDSubList.Add(Source: TDataSource; RunBack: Boolean = True): TDSubItem;
begin
     Result := Add;
     Result.DataSource := Source;
     Result.Background := RunBack;
end;


function TDSubList.GetOwner:TPersistent;
begin
     Result := FOwner;
end;


function TDSubList.GetItem(Index: Integer): TDSubItem;
begin
     Result := TDSubItem(inherited GetItem(Index));
end;


procedure TDSubList.SetItem(Index: Integer; Value: TDSubItem);
begin
     inherited SetItem(Index, Value);
end;


function TDSubList.FindItem(const ItemName: String): TDSubItem;
var
   i : Integer;
begin
     Result := nil;
     for i := 0 to Count-1 do
     begin
          if Items[i].Name = ItemName then
          begin
               Result := Items[i];
               Break;
          end;
     end;
end;


{******************************************************************************}
{***                              DataThread                                ***}
{******************************************************************************}


constructor TDADOThread.Create(DataSource: TDataSource);
begin
     inherited Create(True);
     FDataSet        := DataSource.DataSet;
     FDataSource     := DataSource;
     FDataSource.DataSet := nil;
     FreeOnTerminate := True;
     Resume;
end;


procedure TDADOThread.Execute;
begin
     try
     begin
          if FDataSet is TDADODataSet
          then
              TDADODataSet(FDataSet).Open
          else
              FDataSet.Open;
          Synchronize(ConnectDataSource);
     end;
     except
          FQueryException := ExceptObject as Exception;
          Synchronize(ShowQryError);
     end;
end;


procedure TDADOThread.ConnectDataSource;
begin
     FDataSource.DataSet := FDataSet;
end;


procedure TDADOThread.ShowQryError;
begin
     Application.ShowException(FQueryException);
end;


end.
