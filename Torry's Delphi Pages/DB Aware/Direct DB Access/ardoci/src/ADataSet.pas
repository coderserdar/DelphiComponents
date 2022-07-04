unit ADataSet;

{$INCLUDE dOCI.inc}

interface

uses
  Windows, SysUtils, Classes, DynamicArrays, Db, Math
  {$IFDEF D7} ,Variants {$ENDIF}
  ;

type
 TADataSet=class;

 TAFieldType = (ftoString, ftoSmallint, ftoInt64, ftoInteger, ftoWord, ftoBoolean,
                ftoDouble, ftoCurrency, ftoDate, ftoTime, ftoDateTime, ftoBlob, ftoClob, ftoUnknown);

 TAParamType = (ptoInput, ptoOutput, ptoInputOutput);

 TAFieldTypeNames = array [TAFieldType] of string;
 TAParamTypeNames = array [TAParamType] of string;

 TADataSetNotifyEvent = procedure(DataSet: TADataSet; Bookm : integer) of object;

 TSortType = (stASC,stDESC); // order of sorting (Ascending/Descending)

 TADatabase=class(TComponent)
  private
  protected
   FDataSets:THArrayPointer; // the list of DataSets which use this ADatabase
   procedure CloseLinkedDataSets;virtual; // if ADatabase is closing then all linked DataSets must be closed too
   procedure SetActive(Value:boolean);virtual;abstract;
   function GetActive:boolean;virtual;abstract;
  public
   constructor Create(AOwner:TComponent);override;
   destructor Destroy; override;
   procedure AddDataSet(DataSet:TDataSet);
   procedure RemoveDataSet(DataSet:TDataSet);
   procedure Open;virtual;abstract;
   procedure Close;virtual;abstract;
  published
   property Active:boolean read GetActive write SetActive default False;
 end;

 TAParam = class
  private
   FName:string;
   FFieldType:TAFieldType;
   FParamType:TAParamType;
  protected
   procedure TestType(t:TAFieldType);

   function  GetIsNull:boolean; virtual; abstract;
   procedure SetIsNull(Value:boolean); virtual; abstract;

   function  GetValue:variant; virtual; abstract;
   procedure SetValue(Value:variant); virtual; abstract;
   function  GetAsInteger:integer; virtual; abstract;
   procedure SetAsInteger(Value:integer); virtual; abstract;
   function  GetAsWord:Word; virtual; abstract;
   procedure SetAsWord(Value:Word); virtual; abstract;
   function  GetAsSmallInt:SmallInt; virtual; abstract;
   procedure SetAsSmallInt(Value:SmallInt); virtual; abstract;
   function  GetAsDate:integer; virtual; abstract;
   procedure SetAsDate(Value:integer); virtual; abstract;
   function  GetAsTime:integer; virtual; abstract;
   procedure SetAsTime(Value:integer); virtual; abstract;
   function  GetAsDateTime:int64; virtual; abstract;
   procedure SetAsDateTime(Value:int64); virtual; abstract;
   function  GetAsString:string; virtual; abstract;
   procedure SetAsString(Value:string); virtual; abstract;
   function  GetAsDouble:double; virtual; abstract;
   procedure SetAsDouble(Value:double); virtual; abstract;
   function  GetAsCurrency:currency; virtual; abstract;
   procedure SetAsCurrency(Value:currency); virtual; abstract;
   function  GetAsBoolean:Boolean; virtual; abstract;
   procedure SetAsBoolean(Value:Boolean); virtual; abstract;
   function  GetAsInt64:int64; virtual; abstract;
   procedure SetAsInt64(Value:int64); virtual; abstract;
  public
   constructor Create(ParamName:string;ParamFieldType:TAFieldType;ParamParamType:TAParamType); virtual;
   procedure Clear; virtual; abstract;
   property Name:string read FName;
   property IsNull:boolean read GetIsNull;
   property AsInteger:integer read GetAsInteger write SetAsInteger;
   property AsWord:Word read GetAsWord write SetAsWord;
   property AsSmallInt:SmallInt read GetAsSmallInt write SetAsSmallInt;
   property AsDate:integer read GetAsDate write SetAsDate;
   property AsTime:integer read GetAsTime write SetAsTime;
   property AsDateTime:int64 read GetAsDateTime write SetAsDateTime;
   property AsString:string read GetAsString write SetAsString;
   property AsDouble:double read GetAsDouble write SetAsDouble;
   property AsCurrency:currency read GetAsCurrency write SetAsCurrency;
   property AsBoolean:Boolean read GetAsBoolean write SetAsBoolean;
   property AsInt64:int64 read GetAsInt64 write SetAsInt64;
   property FieldType:TAFieldType read FFieldType;
   property ParamType:TAParamType read FParamType;
   property Value:variant read GetValue write SetValue;
  end;

  TAField = class
  private
   FRequired:boolean;
   FFieldType:TAFieldType;
   FFieldSize:word;
   FName:string;

  protected
   FParent:TADataSet;
   Values:THArray;            // the Values of field
   ValuesSize:THArrayInteger; // sizes in bytes of each field value for datatypes BLOB and CLOB
   ValuesNull:THArrayBoolean; // stores True - if field has value, False - if field has NULL value
   procedure Clear; virtual;
   procedure Allocate; overload; virtual;

   function RecordToInternal(RecordNum:integer):integer;

   procedure DeleteRecord(RecordNum:integer);
   procedure InsertRecord(RecordNum:integer);
   procedure TestType(t:TAFieldType);
   function  GetIsNull(RecordNum:integer):boolean;
   procedure SetIsNull(RecordNum:integer;Value:boolean);
   function  GetAsString(RecordNum:integer):string;
   procedure SetAsString(RecordNum:integer;Value:string);
   function  GetAsInteger(RecordNum:integer):Integer;
   procedure SetAsInteger(RecordNum:integer;Value:Integer);
   function  GetAsBoolean(RecordNum:integer):Boolean;
   procedure SetAsBoolean(RecordNum:integer;Value:Boolean);
   function  GetAsDate(RecordNum:integer):integer;
   procedure SetAsDate(RecordNum:integer;Value:integer);
   function  GetAsTime(RecordNum:integer):integer;
   procedure SetAsTime(RecordNum:integer;Value:integer);
   function  GetAsDateTime(RecordNum:integer):int64;
   procedure SetAsDateTime(RecordNum:integer;Value:int64);
   function  GetAsDouble(RecordNum:integer):Double;
   procedure SetAsDouble(RecordNum:integer;Value:Double);
   function  GetAsCurrency(RecordNum:integer):Currency;
   procedure SetAsCurrency(RecordNum:integer;Value:Currency);
   function  GetAsSmallInt(RecordNum:integer):SmallInt;
   procedure SetAsSmallInt(RecordNum:integer;Value:SmallInt);
   function  GetAsWord(RecordNum:integer):Word;
   procedure SetAsWord(RecordNum:integer;Value:Word);
   function  GetAsInt64(RecordNum:integer):int64;
   procedure SetAsInt64(RecordNum:integer;Value:int64);
   function  GetValue(RecordNum:integer):variant;
   procedure SetValue(RecordNum:integer;Value:variant);
  public
   Visible:boolean;
   ReadOnly:boolean;

   constructor Create(Parent:TADataSet;FieldName:string;RFieldType:TAFieldType;FieldSize:word;Required:boolean);virtual;
   destructor Destroy; override;
//   procedure Allocate(HArray:THArray;HArrayNull:THArrayBoolean;HArraySize:THArrayInteger=nil); overload;// virtual;

   procedure ClearBlob(RecordNum:integer); virtual;
   function WriteBlob(RecordNum:integer;Offset:integer;Buffer:pointer;Size:integer):cardinal; virtual;
   function ReadBlob(RecordNum:integer;Offset:integer;Buffer:pointer;Size:integer):cardinal; virtual;
   function ReadBlobToStream(RecordNum:integer;Stream:TStream):cardinal; virtual;
   function WriteBlobFromStream(RecordNum:integer;Stream:TStream):cardinal; virtual;
   function GetLobLength(RecordNum:integer):integer;virtual;

   property Name:string read FName;
   property FieldType:TAFieldType read FFieldType;
   property FieldSize:word read FFieldSize;
   property Required:boolean read FRequired;
   property IsNull[RecordNum:integer]:boolean read GetIsNull write SetIsNull;
   property AsString[RecordNum:integer]:string read GetAsString write SetAsString;
   property AsInteger[RecordNum:integer]:integer read GetAsInteger write SetAsInteger;
   property AsDate[RecordNum:integer]:integer read GetAsDate write SetAsDate;
   property AsTime[RecordNum:integer]:integer read GetAsTime write SetAsTime;
   property AsDateTime[RecordNum:integer]:int64 read GetAsDateTime write SetAsDateTime;
   property AsDouble[RecordNum:integer]:double read GetAsDouble write SetAsDouble;
   property AsCurrency[RecordNum:integer]:currency read GetAsCurrency write SetAsCurrency;
   property AsBoolean[RecordNum:integer]:Boolean read GetAsBoolean write SetAsBoolean;
   property AsWord[RecordNum:integer]:Word read GetAsWord write SetAsWord;
   property AsSmallInt[RecordNum:integer]:SmallInt read GetAsSmallInt write SetAsSmallInt;
   property Value[RecordNum:integer]:variant read GetValue write SetValue;
   property HArrayValues:THArray read Values;
   property HArrayValuesNull:THArrayBoolean read ValuesNull;
   property HArrayValuesSize:THArrayInteger read ValuesSize;
  end;

 TADataSet = class(TComponent)
  private
   FActive:boolean;
   FStreamedActive:boolean;
   FCurrentRec:integer;
   FAfterInsert  : TADataSetNotifyEvent;
   FBeforeDelete : TADataSetNotifyEvent;

//   FUni:boolean; moved to the AOraSQL.pas
   FFields:THArrayPointer;
//   FSortIndex:THArrayInteger; // index array for sort without moving records

   procedure SetActive(Value:boolean);

   procedure SetAfterInsert(proc : TADataSetNotifyEvent);
   procedure SetBeforeDelete(proc : TADataSetNotifyEvent);

   function GetFieldByIndex(Index:integer):TAField;
   function GetFieldByName(FieldName:string):TAField;
   function GetFieldID(FieldName:string):integer;
   function GetParamByIndex(Index:integer):TAParam;
   function GetParamCount:integer;
   function GetFieldCount: integer;
//   function GetSorted:boolean;

  protected
   FFetched:boolean;
   FBeginRecord:integer;
   FParams:THArrayPointer; //им здесь не место надо в private
   FCount:integer;
   procedure CheckActive;
   procedure Loaded; override;
   procedure EmptyFields;
   procedure AllocateFields;
   function  GetParamID(ParamName:string):integer;
   function  GetParamByName(ParamName:string):TAParam;
   procedure ForgetValues;
//   procedure Sort(FieldIndex:integer;SortType:TSortType);overload;
//   procedure Sort(FieldName:string;SortType:TSortType);overload;
   function CreateAField(FieldName:string;FieldType:TAFieldType;FieldSize:word;Required:boolean):TAField;virtual;

  public
     // abstract methods
   procedure Fetch;virtual;abstract;
   procedure Prepare; virtual;abstract;
   procedure UnPrepare; virtual;abstract;
   procedure Open; overload; virtual;
   procedure Open(Fields:THArrayPointer); overload; virtual;
   procedure OpenAll;virtual;
   procedure Close; virtual;
   function ReadRecord(RecordNum:integer):boolean; virtual;
   procedure DeleteRecord(RecordNum:integer); virtual;
   procedure InsertRecord(RecordNum:integer); virtual;
   procedure AppendRecord; virtual;

   procedure ClearFields; virtual;
   procedure ClearParams;virtual;
   procedure AddParam(ParamName:string;FieldType:TAFieldType;ParamType:TAParamType);virtual;abstract;
   procedure AddField(FieldName:string;FieldType:TAFieldType;FieldSize:word;Required:boolean); virtual;
   procedure CopyStructure(DataSet:TDataSet);overload;
   procedure CopyStructure(ADataSet:TADataSet);overload;

   procedure ReOpen;
   procedure ReadAll;
   function EOF:boolean;
   procedure Next;
   procedure First;


   procedure SaveToDBF(FileName:string);

   function WriteBlob(FieldNum,RecordNum:integer;Offset:integer;Buffer:pointer;Size:integer):cardinal;overload;
   function ReadBlob(FieldNum,RecordNum:integer;Offset:integer;Buffer:pointer;Size:integer):cardinal;overload;
   function ReadBlobToStream(FieldNum,RecordNum:integer;Stream:TStream):cardinal;overload;
   function WriteBlobFromStream(FieldNum,RecordNum:integer;Stream:TStream):cardinal;overload;

   function WriteBlob(FieldName:string;RecordNum:integer;Offset:integer;Buffer:pointer;Size:integer):cardinal; overload;
   function ReadBlob(FieldName:string;RecordNum:integer;Offset:integer;Buffer:pointer;Size:integer):cardinal; overload;
   function ReadBlobToStream(FieldName:string;RecordNum:integer;Stream:TStream):cardinal; overload;
   function WriteBlobFromStream(FieldName:string;RecordNum:integer;Stream:TStream):cardinal;overload;

   function ParamExists(ParamName: string): boolean;
   function FieldExists(FieldName: string): boolean;

   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;

   property ParamCount:integer read GetParamCount;
   property ParamByName[Name:string]:TAParam read GetParamByName;
   property ParamByIndex[Index:integer]:TAParam read GetParamByIndex;
   property FieldCount:integer read GetFieldCount;
   property FieldByName[Name:string]:TAField read GetFieldByName;
   property FieldByIndex[Index:integer]:TAField read GetFieldByIndex;


   property RecordCount:integer read FCount;
   property FieldID[FieldName:string]:integer read GetFieldID;
//   property Sorted:boolean read GetSorted;

   property CurrentRecord:integer read FCurrentRec;
   property aaAfterInsert : TADataSetNotifyEvent read FAfterInsert write SetAfterInsert;
   property aaBeforeDelete : TADataSetNotifyEvent read FBeforeDelete write SetBeforeDelete;
  published
   property Active:boolean read FActive write SetActive;
   property Fetched:boolean read FFetched;
//   property UniDirectional:boolean read FUni write FUni; moved to the AOraSQL.pas
  end;

var
 AParamTypeNames : TAParamTypeNames=('ptoInput','ptoOutput','ptoInputOutput');
 AFieldTypeNames : TAFieldTypeNames=('ftoString','ftoSmallint','ftoInt64','ftoInteger','ftoWord','ftoBoolean','ftoDouble',
                                     'ftoCurrency','ftoDate','ftoTime','ftoDateTime','ftoBlob','ftoClob','ftoUnknown');


function TypeAToDelphi(fta:TAFieldType):TFieldType;
function TypeDelphiToA(ft:TFieldType):TAFieldType;
function ParamTypeAToDelphi(pto:TAParamType):TParamType;
function ParamTypeDelphiToA(pt:TParamType):TAParamType;

implementation

uses GoodDate, DataSetQuery, OraUtils, OraError, dOCIMessages,
     DBConsts {$IFDEF D6} ,Variants {$ENDIF};


function TypeAToDelphi(fta:TAFieldType):TFieldType;
begin
  case fta of
   ftoString:   Result:=ftString;
   ftoBoolean:  Result:=ftBoolean;
   ftoDouble:   Result:=ftFloat;
   ftoCurrency: Result:=ftCurrency;
   ftoDate:     Result:=ftDate;
   ftoTime:     Result:=ftTime;
   ftoDateTime: Result:=ftDateTime;
   ftoInteger:  Result:=ftInteger;
   ftoSmallInt: Result:=ftSmallInt;
   ftoWord:     Result:=ftWord;
   ftoBlob:     Result:=ftBlob;
   ftoClob:     Result:=ftMemo;
  else
   raise Exception.Create(SUnknownFieldType);
  end;
end;

function TypeDelphiToA(ft:TFieldType):TAFieldType;
begin
  case ft of
   ftString:   Result:=ftoString;
   ftBoolean:  Result:=ftoBoolean;
   ftFloat:    Result:=ftoDouble;
   ftCurrency: Result:=ftoCurrency;
   ftDate:     Result:=ftoDate;
   ftTime:     Result:=ftoTime;
   ftDateTime: Result:=ftodateTime;
   ftInteger:  Result:=ftoInteger;
   ftSmallInt: Result:=ftoSmallInt;
   ftWord:     Result:=ftoWord;
   ftBlob:     Result:=ftoBlob;
   ftMemo:     Result:=ftoClob;
  else
   raise Exception.Create(SErrUnknownFieldType);
  end;
end;

function ParamTypeAToDelphi(pto:TAParamType):TParamType;
begin
 case pto of
  ptoInput:      Result:=ptInput;
  ptoOutput:     Result:=ptOutput;
  ptoInputOutput:Result:=ptInputOutput;
 else
  raise Exception.Create(SErrUnknownParameterDataType);
 end;
end;

function ParamTypeDelphiToA(pt:TParamType):TAParamType;
begin
 case pt of
  ptInput:       Result:=ptoInput;
  ptOutput:      Result:=ptoOutput;
  ptInputOutput,
  ptUnknown,
  ptResult:      Result:=ptoInputOutput;
 else
  raise Exception.Create(SUnknownFieldType);
 end;
end;

{ TAParam }

constructor TAParam.Create(ParamName: string; ParamFieldType: TAFieldType;
  ParamParamType: TAParamType);
begin
 inherited Create;
 FName:=ParamName;
 FFieldType:=ParamFieldType;
 FParamType:=ParamParamType;
end;

procedure TAParam.TestType(t: TAFieldType);
begin
 if t<>FieldType then raise Exception.Create(Format(SErrParamDataTypeMismatch,[FName,AFieldTypeNames[FFieldType],AFieldTypeNames[t]]));
end;

{ TAField }

procedure TAField.Allocate;
begin
 case FieldType of
  ftoString:   Values:=THArrayStringFix.CreateSize(FieldSize);
  ftoBoolean:  Values:=THArrayBoolean.Create;
  ftoDouble:   Values:=THArrayDouble.Create;
  ftoCurrency: Values:=THArrayCurrency.Create;
  ftoDate:     Values:=THArrayInteger.Create;
  ftoTime:     Values:=THArrayInteger.Create;
  ftoDateTime: Values:=THArrayInt64.Create;
  ftoInt64:    Values:=THArrayInt64.Create;
  ftoInteger:  Values:=THArrayInteger.Create;
  ftoSmallInt: Values:=THArraySmallInt.Create;
  ftoWord:     Values:=THArrayWord.Create;
  ftoBlob,
  ftoClob:     begin Values:=THArrayPointer.Create;ValuesSize:=THArrayInteger.Create; end;
 else
  raise Exception.Create(SUnknownFieldType);
 end;
 if (not FRequired)
  then ValuesNull:=THArrayBoolean.Create
  else ValuesNull:=nil;
end;

{procedure TAField.Allocate(HArray:THArray;HArrayNull:THArrayBoolean;HArraySize:THArrayInteger=nil);
begin
 Values:=HArray;
 ValuesNull:=HArrayNull;
 ValuesSize:=HArraySize;
 FRequired:=(HArrayNull=nil);
end;
 }
procedure TAField.Clear;
begin
 if Assigned(Values) then begin Values.Free; Values:=nil; end;
 if Assigned(ValuesSize) then begin ValuesSize.Free; ValuesSize:=nil; end;
 if Assigned(ValuesNull) then begin ValuesNull.Free; ValuesNull:=nil; end;
end;

constructor TAField.Create(Parent: TADataSet; FieldName: string;
  RFieldType: TAFieldType; FieldSize: word; Required: boolean);
begin
 inherited Create;
 FParent:=Parent;
 FName:=FieldName;
 FFieldType:=RFieldType;
 FFieldSize:=FieldSize;
 FRequired:=Required;
 if RFieldType in [ftoBlob,ftoClob] then FFieldSize:=0; // для BLOBов размер хранится в массиве ValuesSize

 Values:=nil;
 ValuesNull:=nil;
 ValuesSize:=nil;

 Visible:=True;
 ReadOnly:=False;
end;

procedure TAField.DeleteRecord(RecordNum: integer);
begin
 Values.Delete(RecordToInternal(RecordNum));
 if Assigned(ValuesSize) then ValuesSize.Delete(RecordToInternal(RecordNum));
 if Assigned(ValuesNull) then ValuesNull.Delete(RecordToInternal(RecordNum));
end;

destructor TAField.Destroy;
begin
{$ifdef ADEBUG}LogMessage('TAField.Destroy BEGIN');{$endif}

 Clear;
 inherited Destroy;
 
 {$ifdef ADEBUG}LogMessage('TAField.Destroy END');{$endif}
end;

function TAField.GetAsBoolean(RecordNum: integer): Boolean;
begin
 TestType(ftoBoolean);
 if IsNull[RecordNum]
  then Result:=False
  else Result:=THArrayBoolean(Values)[RecordToInternal(RecordNum)];
end;

function TAField.GetAsCurrency(RecordNum: integer): Currency;
begin
 TestType(ftoCurrency);
 if IsNull[RecordNum]
  then Result:=0
  else Result:=THArrayCurrency(Values)[RecordToInternal(RecordNum)];
end;

function TAField.GetAsDate(RecordNum: integer): integer;
begin
 TestType(ftoDate);
 if IsNull[RecordNum]
  then Result:=0
  else Result:=THArrayInteger(Values)[RecordToInternal(RecordNum)];
end;

function TAField.GetAsDateTime(RecordNum: integer): int64;
begin
 TestType(ftoDateTime);
 if IsNull[RecordNum]
  then Result:=0
  else Result:=THArrayInt64(Values)[RecordToInternal(RecordNum)];
end;

function TAField.GetAsDouble(RecordNum: integer): Double;
begin
 TestType(ftoDouble);
 if IsNull[RecordNum]
  then Result:=0
  else Result:=THArrayDouble(Values)[RecordToInternal(RecordNum)];
end;

function TAField.GetAsInteger(RecordNum: integer): Integer;
begin
 TestType(ftoInteger);
 if IsNull[RecordNum]
  then Result:=0
  else Result:=THArrayInteger(Values)[RecordToInternal(RecordNum)];
end;

function TAField.GetAsSmallInt(RecordNum: integer): SmallInt;
begin
 TestType(ftoSmallInt);
 if IsNull[RecordNum]
  then Result:=0
  else Result:=THArraySmallInt(Values)[RecordToInternal(RecordNum)];
end;

function TAField.GetAsString(RecordNum: integer): string;
begin
 TestType(ftoString);
 if IsNull[RecordNum]
  then Result:=''
  else Result:=THArrayStringFix(Values)[RecordToInternal(RecordNum)];
end;

function TAField.GetAsTime(RecordNum: integer): integer;
begin
 TestType(ftoTime);
 if IsNull[RecordNum]
  then Result:=0
  else Result:=THArrayInteger(Values)[RecordToInternal(RecordNum)];
end;

function TAField.GetValue(RecordNum: integer): variant;
begin
 Result:=Null;
 if IsNull[RecordNum] then begin
  exit;
 end;
 case FieldType of
  ftoString: Result:=AsString[RecordNum];
  ftoSmallint: Result:=AsSmallInt[RecordNum];
  ftoInteger: Result:=AsInteger[RecordNum];
  ftoWord: Result:=AsWord[RecordNum];
  ftoBoolean: Result:=AsBoolean[RecordNum];
  ftoDouble: Result:=AsDouble[RecordNum];
  ftoCurrency: Result:=AsCurrency[RecordNum];
  ftoDate: Result:=GoodDateToDateTime(AsDate[RecordNum]);
  ftoTime: Result:=GoodTimeToDateTime(AsTime[RecordNum]);
  ftoDateTime: Result:=GoodDateTimeToDateTime(AsDateTime[RecordNum]);
  else raise Exception.Create(SUnknownFieldType);
 end;
end;

function TAField.GetAsWord(RecordNum: integer): Word;
begin
 TestType(ftoWord);
 if IsNull[RecordNum]
  then Result:=0
  else Result:=THArrayWord(Values)[RecordToInternal(RecordNum)];
end;

function TAField.GetAsInt64(RecordNum: integer): int64;
begin
 TestType(ftoInt64);
 if IsNull[RecordNum]
  then Result:=0
  else Result:=THArrayInt64(Values)[RecordToInternal(RecordNum)];
end;

function TAField.GetIsNull(RecordNum: integer): boolean;
begin
 if ValuesNull=nil
  then Result:=False
  else Result:=not ValuesNull[RecordToInternal(RecordNum)];
end;

procedure TAField.InsertRecord(RecordNum: integer);
begin
 if Assigned(Values) then Values.Insert(RecordNum,nil);
 if Assigned(ValuesSize) then begin ValuesSize.Insert(RecordNum,nil); ValuesSize[RecordNum]:=0; end;
 if Assigned(ValuesNull) then begin ValuesNull.Insert(RecordNum,nil); ValuesNull[RecordNum]:=False; end;
end;

function TAField.RecordToInternal(RecordNum: integer): integer;
begin
 if (RecordNum<FParent.FBeginRecord) or (RecordNum>=FParent.RecordCount)
  then raise Exception.Create(Format(SErrRecordNotLoaded,[RecordNum]));
 Result:=RecordNum-FParent.FBeginRecord;
end;

procedure TAField.SetAsBoolean(RecordNum: integer; Value: Boolean);
begin
 TestType(ftoBoolean);
 IsNull[RecordNum]:=False;
 THArrayBoolean(Values)[RecordToInternal(RecordNum)]:=Value;
end;

procedure TAField.SetAsCurrency(RecordNum: integer; Value: Currency);
begin
 TestType(ftoCurrency);
 IsNull[RecordNum]:=False;
 THArrayCurrency(Values)[RecordToInternal(RecordNum)]:=Value;
end;

procedure TAField.SetAsDate(RecordNum, Value: integer);
begin
 TestType(ftoDate);
 IsNull[RecordNum]:=False;
 THArrayInteger(Values)[RecordToInternal(RecordNum)]:=Value;
end;

procedure TAField.SetAsDateTime(RecordNum: integer; Value: int64);
begin
 TestType(ftoDateTime);
 IsNull[RecordNum]:=False;
 THArrayInt64(Values)[RecordToInternal(RecordNum)]:=Value;
end;

procedure TAField.SetAsDouble(RecordNum: integer; Value: Double);
begin
 TestType(ftoDouble);
 IsNull[RecordNum]:=False;
 THArrayDouble(Values)[RecordToInternal(RecordNum)]:=Value;
end;

procedure TAField.SetAsInteger(RecordNum, Value: Integer);
begin
 TestType(ftoInteger);
 IsNull[RecordNum]:=False;
 THArrayInteger(Values)[RecordToInternal(RecordNum)]:=Value;
end;

procedure TAField.SetAsSmallInt(RecordNum: integer; Value: SmallInt);
begin
 TestType(ftoSmallInt);
 IsNull[RecordNum]:=False;
 THArraySmallInt(Values)[RecordToInternal(RecordNum)]:=Value;
end;

procedure TAField.SetAsString(RecordNum: integer; Value: string);
begin
 TestType(ftoString);
 IsNull[RecordNum]:=False;
 THArrayStringFix(Values)[RecordToInternal(RecordNum)]:=Value;
end;

procedure TAField.SetAsTime(RecordNum, Value: integer);
begin
 TestType(ftoTime);
 IsNull[RecordNum]:=False;
 THArrayInteger(Values)[RecordToInternal(RecordNum)]:=Value;
end;

procedure TAField.SetAsWord(RecordNum: integer; Value: Word);
begin
 TestType(ftoWord);
 IsNull[RecordNum]:=False;
 THArrayWord(Values)[RecordToInternal(RecordNum)]:=Value;
end;

procedure TAField.SetAsInt64(RecordNum: integer; Value: int64);
begin
 TestType(ftoInt64);
 IsNull[RecordNum]:=False;
 THArrayInt64(Values)[RecordToInternal(RecordNum)]:=Value;
end;

procedure TAField.SetIsNull(RecordNum: integer; Value: boolean);
begin
 if ValuesNull=nil then exit;
 ValuesNull[RecordToInternal(RecordNum)]:=not Value;
end;

procedure TAField.SetValue(RecordNum: integer; Value: variant);
begin
 if Value=Null then begin
  IsNull[RecordNum]:=True;
  exit;
 end;
 case FieldType of
  ftoString: AsString[RecordNum]:=Value;
  ftoSmallint: AsSmallInt[RecordNum]:=Value;
  ftoInteger: AsInteger[RecordNum]:=Value;
  ftoWord: AsWord[RecordNum]:=Value;
  ftoBoolean: AsBoolean[RecordNum]:=Value;
  ftoDouble: AsDouble[RecordNum]:=Value;
  ftoCurrency: AsCurrency[RecordNum]:=Value;
  ftoDate: AsDate[RecordNum]:=DateTimeToGoodDate(Value);
  ftoTime: AsTime[RecordNum]:=DateTimeToGoodTime(Value);
  ftoDateTime: AsDateTime[RecordNum]:=DateTimeToGoodDateTime(Value);
  ftoBlob,ftoClob : raise Exception.Create('Для записи в поля BLOB используйте ф-цию WriteBlob !!! ');
  else raise Exception.Create(SUnknownFieldType);
 end;
end;

procedure TAField.TestType(t: TAFieldType);
begin
 if t<>FFieldType then raise Exception.Create(Format(SFieldTypeMismatch,[FName,AFieldTypeNames[FFieldType],AFieldTypeNames[t]]));
end;

procedure TAField.ClearBlob(RecordNum: integer);
begin
 TestType(ftoBlob);
 FreeMem(THArrayPointer(Values)[RecordNum]);
 THArrayPointer(Values)[RecordNum]:=nil;
 ValuesNull[RecordNum]:=False; // flag that Bloba is NULL
 ValuesSize[RecordNum]:=0      // the length set to 0
end;

function TAField.ReadBlob(RecordNum, Offset: integer; Buffer: pointer; Size: integer): cardinal;
var pc:PChar;
    RealSize,pi,c:integer;
begin
 TestType(ftoBlob);
 Result:=0;
 if IsNull[RecordNum] then exit; // BLOB is empty
 pi:=cardinal(THArrayPointer(Values)[RecordNum]); // pointer to memory where BLOB field data stores
 if pi=0 then exit; // BLOB is empty
 RealSize:=ValuesSize[RecordNum];
 if Offset>=RealSize then exit;  // the requested offset larger than length of BLOB field

 pc:=PChar(pi+offset);

 c:=min(RealSize-offset,Size);
 memcpy(pc,Buffer,c);
 Result:=c;
end;

function TAField.WriteBlob(RecordNum, Offset: integer; Buffer: pointer; Size: integer): cardinal;
// if offset>0 then alloc memory offset+Size bytes and
// with offset "offset" writing data from buffer
// если offset>0 тогда захватываем памяти (offset+Size) байт и
// со смещения offset переписываем данные из буфера в эту память
var pc:PChar;
    pi:integer;
begin
 TestType(ftoBlob);
 Result:=Size;

// if(Offset=0)or(Size=0) then ClearBlob(RecordNum);

 if Size>0 then begin // have we any info?
  pc:=THArrayPointer(Values)[RecordNum];
  ReallocMem(pc,Offset+Size);
  THArrayPointer(Values)[RecordNum]:=pc;
  pi:=integer(pc);
  pc:=PChar(pi+offset);
  memcpy(Buffer,pc,Size);
  IsNull[RecordNum]:=False; // flag that BLOB field is not empty
 end;
 ValuesSize[RecordNum]:=Offset+Size;
end;

function TAField.ReadBlobToStream(RecordNum: integer;Stream: TStream): cardinal;
var buf:array[0..16383] of byte;
    sz:cardinal;
    Offset:integer;
begin
 Result:=0; Offset:=0;
 repeat
  sz:=ReadBlob(RecordNum,Offset,@buf,sizeof(buf));
  Result:=Result+sz;
  inc(Offset,sz);
  Stream.Write(buf,sz);
 until sz<>sizeof(buf);
end;

function TAField.WriteBlobFromStream(RecordNum: integer;Stream: TStream): cardinal;
var buf:array[0..16383] of byte;
    sz:integer;
    Offset:integer;
begin
 ClearBlob(RecordNum);
 Result:=0; Offset:=0;
 Stream.Seek(0,soFromBeginning);
 if Stream.Size>0 then begin
  repeat
   sz:=Stream.Read(buf,sizeof(buf));
   Result:=Result+WriteBlob(RecordNum,Offset,@buf,sz);
   inc(Offset,sz);
  until Offset=Stream.Size;
 end;
end;

function TAField.GetLobLength(RecordNum: integer): integer;
begin
// if (RecordNum<0)or(RecordNum>=ValuesSize.Count) then raise Exception.Create('The Record number '+IntToStr(RecordNum)+' is more than RecordCount='+IntToStr(ValuesSize.Count)+'!');
 Result:=ValuesSize[RecordToInternal(RecordNum)];
end;


{ TADataSet }

procedure TADataSet.AllocateFields;
var i:integer;
begin
 for i:=0 to FFields.Count-1 do TAField(FFields[i]).Allocate;
end;

procedure TADataSet.AppendRecord;
begin
 InsertRecord(RecordCount);
end;

procedure TADataSet.ClearFields;
var i:integer;
begin
 for i:=0 to FFields.Count-1 do TAField(FFields[i]).Free;
 FFields.Clear;
end;
                    
procedure TADataSet.ClearParams;
var i:integer;
begin
 for i:=0 to FParams.Count-1 do TAParam(FParams[i]).Free;
 FParams.Clear;
end;

procedure TADataSet.Close;
begin
 EmptyFields;
 FActive:=False;
 FCount:=0;
end;

constructor TADataSet.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FFields:=THArrayPointer.Create;
 FParams:=THArrayPointer.Create;
// FSortIndex:=nil;
 FBeginRecord:=0;
 FCurrentRec:=0;
 FCount:=0;
 FActive:=False;
end;

destructor TADataSet.Destroy;
begin
{$ifdef ADEBUG}LogMessage('TADataSet.Destroy BEGIN');{$endif}

 if FActive then Close;
 ClearFields;
 FFields.Free;
 ClearParams;
 FParams.Free;
 inherited Destroy;

 {$ifdef ADEBUG}LogMessage('TADataSet.Destroy END');{$endif}
end;

procedure TADataSet.AddField(FieldName: string; FieldType: TAFieldType;FieldSize: word; Required: boolean);
var F:TAField;
    i:integer;
begin
 if FieldName = '' then ADatabaseError(SFieldNameMissing, self);
 for i:=0 to FFields.Count-1 do
  if AnsiCompareText(TAField(FFields[i]).Name,FieldName)=0 then ADatabaseError(Format(SDuplicateFieldName,[FieldName,self.name]));

 F:=CreateAField(FieldName,FieldType,FieldSize,Required);
 FFields.AddValue(F);
end;

function TADataSet.CreateAField(FieldName:string;FieldType:TAFieldType;FieldSize:word;Required:boolean): TAField;
begin
 Result:=TAField.Create(self,FieldName,FieldType,FieldSize,Required);
end;


{procedure TADataSet.AddParam(ParamName:string;FieldType:TAFieldType;ParamType:TAParamType);
var i:integer;
begin
 if ParamName = '' then ADatabaseError('Paramter name missing!', self);
 for i:=0 to FParams.Count-1 do
  if AnsiCompareText(TAParam(FParams[i]).Name,ParamName)=0 then ADatabaseError(Format(SDuplicateName,[ParamName,self.name]));

 FParams.AddValue(TAParam.Create(ParamName,FieldType,ParamType));
end;}

procedure TADataSet.DeleteRecord(RecordNum: integer);
var i:integer;
begin
 CheckActive;
 if (RecordNum<0)or(RecordNum>=FCount) then raise Exception.Create('Record with number '+IntToStr(RecordNum)+' not found!');
 for i:=0 to FFields.Count-1 do begin
  TAField(FFields[i]).DeleteRecord(RecordNum);
 end;
 FCount:=FCount-1;
 if Assigned(FBeforeDelete) then FBeforeDelete(Self, RecordNum);
end;

procedure TADataSet.InsertRecord(RecordNum: integer);
var i:integer;
begin
 CheckActive;
 if (RecordNum<0)or(RecordNum>FCount) then raise Exception.Create('Record number '+IntToStr(RecordNum)+' too large! Cannot insert.');

 for i:=0 to FFields.Count-1 do begin
  TAField(FFields[i]).InsertRecord(RecordNum);
 end;
 FCount:=FCount+1;
 if Assigned(FAfterInsert) then FAfterInsert(Self, RecordNum);
end;

procedure TADataSet.EmptyFields;
var i:integer;
begin
 for i:=0 to FFields.Count-1 do TAField(FFields[i]).Clear;
end;

procedure TADataSet.ForgetValues;
var i:integer;
begin
 for i:=0 to FFields.Count-1 do begin
  TAField(FFields.Value[i]).Values.Clear;
  if Assigned(TAField(FFields.Value[i]).ValuesNull)
   then TAField(FFields.Value[i]).ValuesNull.Clear;
  if Assigned(TAField(FFields.Value[i]).ValuesSize)
   then TAField(FFields.Value[i]).ValuesSize.Clear;
 end;
end;

function TADataSet.GetFieldByName(FieldName: string): TAField;
var n:integer;
begin
 n:=GetFieldID(FieldName);
 if n=-1 then raise Exception.Create('Field '''+FieldName+''' not found!');
 Result:=FFields[n];
end;

function TADataSet.GetFieldByIndex(Index: integer): TAField;
begin
 if(Index<0)or(Index>=FFields.Count)then raise Exception.Create(SFieldIndexError);
 Result:=FFields[Index];
end;

function TADataSet.GetFieldCount: integer;
begin
 Result:=FFields.Count;
end;

function TADataSet.GetFieldID(FieldName: string): integer;
begin
  for Result:=0 to FFields.Count-1 do
  if AnsiCompareText(TAField(FFields[Result]).Name,FieldName)=0 then exit;
  Result:=-1;
end;

function TADataSet.FieldExists(FieldName: string): boolean;
begin
 Result:=GetFieldID(FieldName)>=0;
end;

function TADataSet.GetParamByIndex(Index: integer): TAParam;
begin
 if(Index<0)or(Index>=FParams.Count)then raise Exception.Create('Param with index '+IntToStr(Index)+' does not exists!');
 Result:=FParams[Index];
end;

function TADataSet.ParamExists(ParamName: string): boolean;
begin
 Result:=GetParamID(ParamName)>=0;
end;

function TADataSet.GetParamByName(ParamName: string): TAParam;
var i:integer;
begin
 i:=GetParamID(ParamName);
 if i=-1 then raise Exception.Create('Param '''+ParamName+''' not found !') //Result:=nil
         else Result:=FParams[i];
end;

function TADataSet.GetParamID(ParamName: string): integer;
begin
 for Result:=0 to FParams.Count-1 do
  if AnsiCompareText(TAParam(FParams[Result]).Name,ParamName)=0 then exit;
 Result:=-1;
end;

procedure TADataSet.Loaded;
begin
 inherited Loaded;
 Active:=FStreamedActive;
end;

procedure TADataSet.Open;
begin
 if FActive then exit;
 AllocateFields;
 FActive:=True;
 FCount:=0;
end;

procedure TADataSet.Open(Fields: THArrayPointer);
var i:integer;
begin
// if FActive then exit;
 if Fields.Count>0 then begin
  for i:=0 to Fields.Count-1 do TAField(Fields[i]).FParent:=self;
  FFields.Clear;
  FFields.AddMany(Fields.Memory,Fields.Count);
  FActive:=True;
  FCount:=TAField(FFields[0]).Values.Count;
 end else Open;
end;

procedure TADataSet.OpenAll;
begin
 Open;
 ReadAll;
end;

function TADataSet.ReadRecord(RecordNum: integer):boolean;
begin
 Result:=FCount>RecordNum;
end;

procedure TADataSet.ReOpen;
begin
 Close;
 Open;
end;

procedure TADataSet.SetActive(Value: boolean);
begin
 if (csReading in ComponentState) then  begin
   if Value then FStreamedActive := True;
   exit;
 end;
// if (csDestroying in ComponentState) then exit;
 if Value=FActive then exit;
 if Value then Open else Close;
end;

procedure TADataSet.SaveToDBF(FileName:string);
{ ============================================================
 ¦         Запись заголовка в файле с данными               ¦
 ¦----------------------------------------------------------¦
 ¦ Байты :              Описание                            ¦
 ¦==========================================================¦
 ¦ 00    :Типы файлов с данными:                            ¦
 ¦       : FoxBASE+/dBASE III +, без memo - 0х03            ¦
 ¦       : FoxBASE+/dBASE III +, с memo - 0х83              ¦
 ¦       : FoxPro/dBASE IV, без memo - 0х03                 ¦
 ¦       : FoxPro с memo - 0хF5                             ¦
 ¦       : dBASE IV с memo - 0x8B                           ¦
 ¦----------------------------------------------------------¦
 ¦ 01-03 :Последнее изменение (ГГММДД)                      ¦
 ¦----------------------------------------------------------¦
 ¦ 04-07 :Число записей в файле                             ¦
 ¦----------------------------------------------------------¦
 ¦ 08-09 :Положение первой записи с данными                 ¦
 ¦----------------------------------------------------------¦
 ¦ 10-11 :Длина одной записи с данными (включая признак     ¦
 ¦       :удаления)                                         ¦
 ¦----------------------------------------------------------¦
 ¦ 12-27 :Зарезервированы                                   ¦
 ¦----------------------------------------------------------¦
 ¦ 28    :1-есть структ.составной инд.файл (типа .CDX),0-нет¦
 ¦----------------------------------------------------------¦
 ¦ 29-31 :Зарезервированы                                   ¦
 ¦----------------------------------------------------------¦
 ¦ 32-n  :Подзаписи полей**                                 ¦
 ¦----------------------------------------------------------¦
 ¦  n+1  :Признак завершения записи заголовка (0х01)        ¦
 ============================================================

 ============================================================
 ¦                    Подзаписи полей                       ¦
 ¦----------------------------------------------------------¦
 ¦ Байты :                Описание                          ¦
 ¦==========================================================¦
 ¦ 00-10 :Название поля (максимально - 10 символов, если    ¦
 ¦       :меньше 10, то дополняется пустым символом (0х00)) ¦
 ¦----------------------------------------------------------¦
 ¦ 11    :Тип данных:                                       ¦
 ¦       : C - символьное;                                  ¦
 ¦       : N - числовое;                                    ¦
 ¦       : L - логическое;                                  ¦
 ¦       : M - типа memo;                                   ¦
 ¦       : D - дата;                                        ¦
 ¦       : F - с плавающей точкой;                          ¦
 ¦       : P - шаблон.                                      ¦
 ¦----------------------------------------------------------¦
 ¦ 12-15 :Расположение поля внутри записи                   ¦
 ¦----------------------------------------------------------¦
 ¦ 16    :Длина поля (в байтах)                             ¦
 ¦----------------------------------------------------------¦
 ¦ 18-32 :Зарезервированы                                   ¦
 ============================================================}
 type
    TFieldHeader=record
     Name:array[0..10] of char;  // field name
     DataType:char;              // data type of field
     Offset:integer;             // field offset in record
     Length:byte;                // field size in bytes
     dummy:array[18..32] of byte;// reserved
    end;

    THeader=record
     ftype:byte;
     LastChange:array[1..3] of byte;
     RecordCount:integer;
     FirstOffset:word;
     RecordLength:word;
     dummy1:array[12..27] of byte;
     HaveIndex:byte;
     dummy2:array[29..31] of byte;
    end;

   procedure ConvertToOut(RecordNum:integer;fi:TAField;Buffer:pointer;Len:integer);
   // переводит значение поля в нужный для записи .DBF формат
   //(чаще перегоняет просто в строку определенной кодировки)
   var Data:string;
       y,m,d:word;
   begin
    if fi.IsNull[RecordNum] then Data:=MakeStr(' ',Len) else
     case fi.FieldType of
      ftoString: Data:=fi.AsString[RecordNum];
      ftoDouble: Data:=RightStr(FloatToStr(fi.AsDouble[RecordNum]),Len);
      ftoCurrency: Data:=RightStr(CurrToStr(fi.AsCurrency[RecordNum]),Len);
      ftoSmallInt: Data:=RightStr(IntToStr(fi.AsSmallInt[RecordNum]),Len);
      ftoInteger: Data:=RightStr(IntToStr(fi.AsInteger[RecordNum]),Len);
      ftoWord: Data:=RightStr(IntToStr(fi.AsWord[RecordNum]),Len);
      ftoBoolean: if fi.AsBoolean[RecordNum] then Data:='T' else Data:='F';
      ftoDate: begin
                UnMakeGoodDate(fi.AsDate[RecordNum],y,m,d);
                Data:=inttostr(y);
                if m<10 then Data:=Data+'0'+inttostr(m) else Data:=Data+inttostr(m);
                if d<10 then Data:=Data+'0'+inttostr(d) else Data:=Data+inttostr(d);
               end;
     end;
    Data:=StrToOem(copy(Data,0,Len));
    memcpy(pchar(Data),Buffer,Length(Data));
   end;

var y,m,d:word;
    f,i:integer;
    Header:THeader;
    fi:TAField;
    Offset:integer;

    DBFFields:THArray;
    fh:^TFieldHeader;
    fd:pointer;
    r:integer;
begin

 f:=FileCreate(FileName);
 if f=-1 then raise Exception.Create('Error create file :'+FileName);

 memclr(@Header,sizeof(Header));

 Header.ftype:=3;
 Header.RecordCount:=RecordCount;
 Header.FirstOffset:=sizeof(Header)+sizeof(TFieldHeader)*FFields.Count+1;

 DecodeDate(SysUtils.Date,y,m,d);
 Header.LastChange[1]:=y;
 Header.LastChange[2]:=m;
 Header.LastChange[3]:=d;

 DBFFields:=THArray.Create;
 DBFFields.ItemSize:=sizeof(TFieldHeader);
 DBFFields.AddFillValues(FFields.Count);

 Offset:=1;
 for i:=0 to FFields.Count-1 do begin
  fi:=TAField(FFields[i]);
  fh:=DBFFields.GetAddr(i);
  memclr(fh,sizeof(TFieldHeader));
  strplcopy(fh.Name,uppercase(fi.Name),10);

  case TAField(FFields[i]).FieldType of
   ftoString: fh.DataType:='C';
   ftoDouble,ftoCurrency,ftoSmallInt,ftoInteger,ftoWord : fh.DataType:='N';
   ftoBoolean: fh.DataType:='L';
   ftoDate: fh.DataType:='D';
   ftoBlob,ftoClob:begin
                    fh.DataType:='M';
                    Header.ftype:=$83;
                   end;
  else raise Exception.Create('Unknown fields type !');
  end;

  case fi.FieldType of
   ftoString:begin
              if fi.FieldSize<1 then raise Exception.Create('Размер поля в DBF не может быть меньше 1 !');
              if fi.FieldSize>254 then raise Exception.Create('Размер поля в DBF не может превышать 254 символа !');
              fh.Length:=fi.FieldSize;
             end;
   ftoDouble:   fh.Length:=17;
   ftoCurrency: fh.Length:=22;
   ftoSmallInt: fh.Length:=6;
   ftoInteger:  fh.Length:=11;
   ftoWord:     fh.Length:=5;
   ftoBoolean:  fh.Length:=1;
   ftoDate:     fh.Length:=8;//10;
   ftoBlob,ftoClob:fh.Length:=25;
  else raise Exception.Create('Unknown fields type !');
  end;
  fh.Offset:=Offset;
  inc(Offset,fh.Length);
 end;

 Header.RecordLength:=Offset;

 FileWrite(f,Header,sizeof(Header));
 FileWrite(f,(DBFFields.Memory)^,DBFFields.ItemSize*DBFFields.Count);
 i:=$0D;
 FileWrite(f,i,1);

 // вывод записей
 fd:=AllocMem(Header.RecordLength);
 try
  for r:=0 to RecordCount-1 do begin
   memclr(fd,Header.RecordLength);
   for i:=0 to FFields.Count-1 do begin
    fi:=TAField(FFields[i]);
    fh:=DBFFields.GetAddr(i);
    ConvertToOut(r,fi,pointer(longword(fd)+longword(fh.Offset)),fh.Length);
   end;
   FileWrite(f,fd^,Header.RecordLength);
  end;
 finally
  FreeMem(fd);
 end;
 DBFFields.Free;
 FileClose(f);
end;

function TADataSet.EOF: boolean;
begin
 if not FFetched
  then Result:=False
  else Result:=RecordCount<=FCurrentRec;
end;

procedure TADataSet.Next;
begin
 Inc(FCurrentRec);
 ReadRecord(FCurrentRec);
end;

procedure TADataSet.First;
begin
 FCurrentRec:=0;
 ReadRecord(FCurrentRec);
end;

function TADataSet.GetParamCount: integer;
begin
 Result:=FParams.Count;
end;

procedure TADataSet.SetAfterInsert(proc : TADataSetNotifyEvent);
begin
  FAfterInsert := proc;
end;

procedure TADataSet.SetBeforeDelete(proc : TADataSetNotifyEvent);
begin
  FBeforeDelete := proc;
end;

procedure TADataSet.ReadAll;
begin
 while not FFetched do Fetch;
end;

function TADataSet.ReadBlob(FieldName: string; RecordNum, Offset: integer;
  Buffer: pointer; Size: integer): cardinal;
begin
 Result:=GetFieldByName(FieldName).ReadBlob(RecordNum,Offset,Buffer,Size);
end;

function TADataSet.ReadBlob(FieldNum, RecordNum, Offset: integer;
  Buffer: pointer; Size: integer): cardinal;
begin
  Result:=GetFieldByIndex(FieldNum).ReadBlob(RecordNum,Offset,Buffer,Size);
end;

function TADataSet.ReadBlobToStream(FieldName: string; RecordNum: integer;
  Stream: TStream): cardinal;
begin
  Result:=GetFieldByName(FieldName).ReadBlobToStream(RecordNum,Stream);
end;

function TADataSet.ReadBlobToStream(FieldNum, RecordNum: integer;
  Stream: TStream): cardinal;
begin
 Result:=GetFieldByIndex(FieldNum).ReadBlobToStream(RecordNum,Stream);
end;

function TADataSet.WriteBlob(FieldNum, RecordNum, Offset: integer;
  Buffer: pointer; Size: integer): cardinal;
begin
 Result:=GetFieldByIndex(FieldNum).WriteBlob(RecordNum,Offset,Buffer,Size);
end;

function TADataSet.WriteBlob(FieldName: string; RecordNum, Offset: integer;
  Buffer: pointer; Size: integer): cardinal;
begin
 Result:=GetFieldByName(FieldName).WriteBlob(RecordNum,Offset,Buffer,Size);
end;

function TADataSet.WriteBlobFromStream(FieldNum, RecordNum: integer;
  Stream: TStream): cardinal;
begin
  Result:=GetFieldByIndex(FieldNum).WriteBlobFromStream(RecordNum,Stream);
end;

function TADataSet.WriteBlobFromStream(FieldName: string;
  RecordNum: integer; Stream: TStream): cardinal;
begin
  Result:=GetFieldByName(FieldName).WriteBlobFromStream(RecordNum,Stream);
end;

procedure TADataSet.CopyStructure(DataSet: TDataSet);
var i:integer;
begin
 Close;
 FFields.Clear;
 for i:=0 to DataSet.FieldDefs.Count-1 do begin
   AddField(DataSet.FieldDefs[i].Name,TypeDelphiToA(DataSet.FieldDefs[i].DataType),DataSet.FieldDefs[i].Size,DataSet.FieldDefs[i].Required);
 end;
end;

procedure TADataSet.CopyStructure(ADataSet: TADataSet);
var i:integer;
begin
 Close;
 FFields.Clear;
 for i:=0 to ADataSet.FieldCount-1 do begin
  AddField(ADataSet.FieldByIndex[i].Name,ADataSet.FieldByIndex[i].FieldType,ADataSet.FieldByIndex[i].FieldSize,ADataSet.FieldByIndex[i].Required);
 end;
end;

//procedure TADataSet.Sort(FieldIndex: integer;SortType:TSortType);
//var i,j:integer;
//    a:THArray;
//begin
{ if Assigned(FSortIndex)
  then FSortIndex.Clear
  else FSortIndex:=THArrayInteger.Create;

 FSortIndex.SetCapacity(FCount); // so many items as many records in ADataSet
 for i:=0 to FCount do
  FSortIndex[i]:=i;

 a:=TAField(FFields[FieldIndex]).Values;
 if TAField(FFields[FieldIndex]).FFieldType in [ftoBlob, ftoClob]
  then raise Exception.Create('Can''t sort by BLOB field!');
 }
{ for i:=0 to FCount-1 do
  for j:=i to FCount-1 do begin
   case TAField(FFields[FieldIndex]).FFieldType of
//    ftoString:    THArrayStringFix(a).
    ftoSmallint,
    ftoInteger,
    ftoWord,
    ftoBoolean,
    ftoDouble,
    ftoCurrency,
    ftoDate,
    ftoTime,
    ftoDateTime,
  ftoString:   Values:=THArrayStringFix.CreateSize(FieldSize);
  ftoBoolean:  Values:=THArrayBoolean.Create;
  ftoDouble:   Values:=THArrayDouble.Create;
  ftoCurrency: Values:=THArrayCurrency.Create;
  ftoDate:     Values:=THArrayInteger.Create;
  ftoTime:     Values:=THArrayInteger.Create;
  ftoDateTime: Values:=THArrayInt64.Create;
  ftoInteger:  Values:=THArrayInteger.Create;
  ftoSmallInt: Values:=THArraySmallInt.Create;
  ftoWord:     Values:=THArrayWord.Create;


   end;
  end;}
//end;

{procedure TADataSet.Sort(FieldName: string;SortType:TSortType);
begin
 Sort(GetFieldID(FieldName),SortType);
end;
}

{function TADataSet.GetSorted: boolean;
begin
 Result:=Assigned(FSortIndex);
end;
}

procedure TADataSet.CheckActive;
begin
 if not FActive then raise Exception.Create(Format(SErrADataSetNotActive,[Name]));
end;


{ TADatabase }

constructor TADatabase.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FDataSets:=THArrayPointer.Create;
end;

destructor TADatabase.Destroy;
begin
{$ifdef ADEBUG}LogMessage('TADatabase.Destroy BEGIN');{$endif}

 while FDataSets.Count>0 do
  TDataSetQuery(FDataSets[0]).SetDatabase(nil);
 FDataSets.Free;
 inherited Destroy;

{$ifdef ADEBUG}LogMessage('TADatabase.Destroy END');{$endif}
end;

procedure TADatabase.CloseLinkedDataSets;
// if we close TADatabase - all linked DataSets must be closed too.
var i:integer;
begin
 for i:=0 to FDataSets.Count-1 do
   TDataSet(FDataSets[i]).Close;
end;

procedure TADatabase.AddDataSet(DataSet: TDataSet);
begin
 FDataSets.AddValue(DataSet);
end;

procedure TADatabase.RemoveDataSet(DataSet: TDataSet);
var n:integer;
begin
 if DataSet=nil then exit;
 n:=FDataSets.IndexOf(DataSet);
 if n<>-1 then FDataSets.Delete(n);
end;

end.
