unit DataSetQuery;

{$INCLUDE dOCI.inc}

{$define myora}
{$define mymem}

{
 Descendant of  TVirtualDataSet

  Wrapper for all tables based on TADataSet.
  allows to use them as TDataSet, i.e. in Delphi controls
}

interface

uses
  VirtualDataSet, DynamicArrays, Db, Classes,
  DBConsts, ADataSet, dOCIMEssages
  {$IFDEF D7} ,Variants {$ENDIF}
  {$ifdef myora},OraDB,AOraSQL{$endif}
  {$ifdef mymem},AMemoryDataSet{$endif}
  ;

{
  В ValuesNull contains True - if data exists, and False - if data absent

  indicators comes as : -1 - NUll, >=0 - NOT NULL
  For params indicators stores as described above
}

type
  TQueryType=(qtUnknown,qtOracle,qtMemory{,qtInterBase});

  PBlobRec=^TBlobRec;
  TBlobRec=record
   size:integer;
   data:pointer;
  end;

 TDataSetQuery = class;

{
  The stream for reading and writing BLOB fields
  used internal for BlobField.LoadFromStream and BlobField.SaveToStream calls
}

 TABlobStream = class(TStream)
  private
    FField:TAField;
    FBlobField:TBlobField;
    FDataSet: TDataSetQuery;
    FMode: TBlobStreamMode;
    FFieldNo: Integer;
    FModified: Boolean;
    FPosition: integer;
    FBlobData: PChar;
    FCacheSize: integer;
    procedure UpdateActiveBuffer;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: integer; Origin: Word): Longint; override;
    function GetBlobSize: integer;
    procedure Truncate;
  end;


 { TDataSetQuery }

  TDataSetQuery = class(TVirtualDataSet)
  private
   FQueryType:TQueryType;

   FDatabase:TADatabase;
   FSQL:TStrings;
   FUpdateRecord:TUpdateRecordEvent;
   FDParams:TParams;

   function GetField(Index:integer):TAField;
   procedure SetSQL(Value:TStrings);
   function GetParams:TParams;
   procedure QueryChanged(Sender:TObject);
   function GetFetched:boolean;
   procedure SetParams(const Value: TParams);
   procedure BeforeDel(DataSet : TADataSet; num : integer);
   procedure AfterIns(DataSet : TADataSet; num : integer);
  protected
   Query:TADataSet;
//   updRecNum:integer;
   updType:TUpdateKind;
   function VOpen:boolean; override;
   function VClose:boolean; override;
   function VPrepare:boolean; override;

   procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
   procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
   function  GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
   procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;

   function  VGetFieldValue(RecordNum:integer;FieldID:integer;Buffer:pointer):boolean; override;
   procedure VPutFieldValue(RecordNum:integer;FieldID:integer;Buffer:pointer;mode:TPutMode;IfNotNull:Boolean); override;
   procedure VInitFieldDefs(Opened:boolean); override;

   function VDeleteRecord (RecordNum:integer):TUpdateAction; override;
   function VPost         (RecordNum:integer):TUpdateAction; override;
   function VInsert       (RecordNum:integer):TUpdateAction; override;
   procedure BindParameters;
   procedure MapParams;

   procedure GetDataFromParameters;
   procedure SetFetchCount(Value:word);
   function GetFetchCount:word;
   procedure ApplyUpdates;virtual;
   function  GetRecordCount:integer;override;

   property FetchCount:word read GetFetchCount write SetFetchCount;
   property SQL:TStrings read FSQL write SetSQL;
   property Params:TParams read GetParams write SetParams stored True;
   property OnUpdateRecord:TUpdateRecordEvent read FUpdateRecord write FUpdateRecord;

  public
   constructor CreateSet(AOwner:TComponent;RQueryType:TQueryType);
   destructor Destroy; override;
   procedure SetDatabase(Value:TADatabase);
   procedure Prepare;
   procedure UnPrepare;
   procedure ExecSQL;

   function ParamByName(Name:string):TParam;
   function GetFieldNullHArray(FieldID : integer) : THArrayBoolean;
   function GetFieldHArray(FieldID : integer) : THArray;
   procedure VGoto(RecordNum:integer); override;
   procedure ReadAll;
   function FetchNextBlock:boolean; override;

   procedure SetFieldValue(RecordNum:integer; FieldID : integer; Value:variant); override;
   function  GetFieldValue(RecordNum:integer; FieldID : integer):variant; override;
   function  CreateBlobStream(Field:TField;Mode: TBlobStreamMode): TStream;override;
   procedure MoveRecord(FromPos,Count,Offset : integer);
   procedure DefineProperties(Filer:TFiler); override;
   property Fetched:boolean read GetFetched;
   property AField[Index:integer]:TAField read GetField;

   procedure SyncBookm;

  published
   property Active;
  protected
   property Database:TADatabase read FDatabase write SetDatabase;
  end;

implementation

uses  SysUtils, Windows, GoodDate, OraUtils, OraError,
      Math {$IFDEF D6} ,Variants {$ENDIF};

procedure TDataSetQuery.MoveRecord(FromPos,Count,Offset : integer);
var
  i : integer;
begin
  if Offset=0 then exit;
  for i:=0 to Query.FieldCount-1 do
  begin
   if Assigned(Query.FieldByIndex[i].HArrayValues) then Query.FieldByIndex[i].HArrayValues.MoveData(FromPos,Count,Offset);
   if Assigned(Query.FieldByIndex[i].HArrayValuesNull) then Query.FieldByIndex[i].HArrayValuesNull.MoveData(FromPos,Count,Offset);
  end;
  FBookm.MoveData(FromPos,Count,Offset);
end;

procedure TDataSetQuery.SetDatabase(Value:TADatabase);
begin
 if Assigned(FDatabase) then FDatabase.RemoveDataSet(self);
 FDatabase:=Value;
 if Assigned(FDatabase) then FDatabase.AddDataSet(self);

{$ifdef myora}
 if FQueryType=qtOracle then TAOraSQL(Query).Database:=TOraDB(Value);
{$endif}

{$ifdef mymem}
 if FQueryType=qtMemory then exit;
{$endif}
end;

constructor TDataSetQuery.CreateSet(AOwner:TComponent;RQueryType:TQueryType);
begin
 FQueryType:=RQueryType;
 inherited Create(AOwner);

 case FQueryType of
{$ifdef myora}
  qtOracle: Query:=TAOraSQL.Create(self);
{$endif}

{$ifdef mymem}
  qtMemory: Query:=TAMemoryDataSet.Create(self);
{$endif}
 end;

 FDParams:=TParams.Create(self);
 FSQL:=TStringList.Create;

 TStringList(FSQL).OnChange:=QueryChanged;
 Query.aaAfterInsert := AfterIns;
 Query.aaBeforeDelete := BeforeDel;
 Query.Name:='Query';
end;

destructor TDataSetQuery.Destroy;
begin
{$ifdef ADEBUG}LogMessage('TDataSetQuery.Destroy BEGIN');{$endif}

 UnPrepare;
 Close;
 FSQL.Free;
 FDParams.Free;
 Query.Free;

 if Assigned(FDatabase) then FDatabase.RemoveDataSet(self);  // remove itself from list in TADatabase
 inherited Destroy;

{$ifdef ADEBUG}LogMessage('TDataSetQuery.Destroy END');{$endif}
end;

procedure TDataSetQuery.QueryChanged(Sender:TObject);
var List:TParams;
    p:TParam;
    i:integer;
begin
{$ifdef myora}
 if FQueryType=qtOracle then TAOraSQL(Query).SetQuery(FSQL.Text);
{$endif}

{$ifdef mymem}
 if FQueryType=qtMemory then exit;
{$endif}

 FieldDefs.Clear; {roma 23.11.2002}
 Fields.Clear;    {roma 23.11.2002}
 List:=TParams.Create(self);
 try
  List.ParseSQL(FSQL.Text,True);
  List.AssignValues(FDParams);
  FDParams.Clear;
  for i:=0 to List.Count-1 do begin
   p:=List[i];
   if p.Name='=' then continue;
   if FDParams.FindParam(p.Name)=nil
     then FDParams.CreateParam(p.DataType,p.Name,p.ParamType);
  end;
 finally
  List.Free;
 end;
end;

procedure TDataSetQuery.SetSQL(Value:TStrings);
begin
 FSQL.Assign(Value);

{$ifdef myora}
 if FQueryType=qtOracle then TAOraSQL(Query).SetQuery(FSQL.Text);
{$endif}

{$ifdef mymem}
 if FQueryType=qtMemory then exit;
{$endif}

end;

function TDataSetQuery.ParamByName(Name:string):TParam;
begin
 Result:=FDParams.ParamByName(Name);
end;

procedure TDataSetQuery.Prepare;
begin
 Query.Prepare;
end;

procedure TDataSetQuery.BindParameters;
var i:integer;
begin
 for i:=0 to Params.Count-1 do begin
  if Params[i].IsNull then Query.ParamByName[Params[i].Name].Clear else
  case Params[i].DataType of
   ftString:   Query.ParamByName[Params[i].Name].AsString:=Params[i].Value;
   ftBoolean:  Query.ParamByName[Params[i].Name].AsBoolean:=Params[i].Value;
   ftFloat:    Query.ParamByName[Params[i].Name].AsDouble:=Params[i].Value;
   ftCurrency: Query.ParamByName[Params[i].Name].AsCurrency:=Params[i].Value;
   ftDate:     Query.ParamByName[Params[i].Name].AsDate:=DateTimeToGoodDate(Params[i].Value);
   ftTime:     Query.ParamByName[Params[i].Name].AsTime:=DateTimeToGoodTime(Params[i].Value);
   ftDateTime: Query.ParamByName[Params[i].Name].AsDateTime:=DateTimeToGoodDateTime(Params[i].Value);
   ftInteger:  Query.ParamByName[Params[i].Name].AsInteger:=Params[i].Value;
   ftSmallInt: Query.ParamByName[Params[i].Name].AsSmallInt:=Params[i].Value;
   ftWord:     Query.ParamByName[Params[i].Name].AsWord:=Params[i].Value;
  else
   raise Exception.Create(sErrUnknownParameterDataType);
  end;
 end;
end;

procedure TDataSetQuery.GetDataFromParameters;
var i:integer;
begin
 for i:=0 to Params.Count-1 do begin
  if Query.ParamByName[Params[i].Name].IsNull
   then Params[i].Clear
  else
  case Params[i].DataType of
   ftString:   Params[i].Value:=Query.ParamByName[Params[i].Name].AsString;
   ftBoolean:  Params[i].Value:=Query.ParamByName[Params[i].Name].AsBoolean;
   ftFloat:    Params[i].Value:=Query.ParamByName[Params[i].Name].AsDouble;
   ftCurrency: Params[i].Value:=Query.ParamByName[Params[i].Name].AsCurrency;
   ftDate:     Params[i].Value:=GoodDateToDateTime(Query.ParamByName[Params[i].Name].AsDate);
   ftTime:     Params[i].Value:=GoodTimeToDateTime(Query.ParamByName[Params[i].Name].AsTime);
   ftDateTime: Params[i].Value:=GoodDateTimeToDateTime(Query.ParamByName[Params[i].Name].AsDateTime);
   ftInteger:  Params[i].Value:=Query.ParamByName[Params[i].Name].AsInteger;
   ftSmallInt: Params[i].Value:=Query.ParamByName[Params[i].Name].AsSmallInt;
   ftWord:     Params[i].Value:=Query.ParamByName[Params[i].Name].AsWord;
  else
   raise Exception.Create(sErrUnknownParameterDataType);
  end;
 end;
end;

procedure TDataSetQuery.ExecSQL;
begin
 Query.ClearFields;
 MapParams;
 BindParameters;
{$ifdef myora}
 if FQueryType=qtOracle then TAOraSQL(Query).ExecSQL;
{$endif}
{$ifdef mymem}
 if FQueryType=qtMemory then exit;
{$endif}
 GetDataFromParameters;
end;

procedure TDataSetQuery.MapParams;
var i:integer;
//    pt:TAParamType;
begin
 Query.ClearParams;
 for i:=0 to Params.Count-1 do begin
{  case Params[i].ParamType of
   ptInput:  pt:=ptoInput;
   ptOutput: pt:=ptoOutput;
  else
   pt:=ptoInputOutput;
  end;}
//  if not Assigned(Query.ParamByName[Params[i].Name]) then
  Query.AddParam(Params[i].Name, TypeDelphiToA(Params[i].DataType),ParamTypeDelphiToA(Params[i].ParamType));
 end;
end;

function TDataSetQuery.VOpen:boolean;
var i:integer;
begin
 if DefaultFields then begin
   VInitFieldDefs(True);
   CreateFields;
 end else begin
 { fills field defines only if fields was entered in fields editor (double
   click on component and then press New Field menu item)
   in this case fields are stored in array Fields and Fields exist before opening DataSet}
  Query.ClearFields;
  FieldDefs.Clear;
  for i:=0 to Fields.Count-1 do
   FieldDefs.Add(Fields[i].FieldName,Fields[i].DataType,Fields[i].Size,Fields[i].Required);

  for i:=0 to FieldDefs.Count-1 do
   Query.AddField(FieldDefs[i].Name,TypeDelphiToA(FieldDefs[i].DataType),FieldDefs[i].Size,FieldDefs[i].Required);
 end;

 MapParams;
 BindParameters;
 Query.Open;
 Result:=Query.Active;
end;

procedure TDataSetQuery.VInitFieldDefs(Opened:boolean);
{ calls only if user does not explicity define fields (double
   click on component and then press New Field menu item)
  If fields in application are defined explicity through FieldDefs then
  copy this fields in aQuery
}
var i:integer;
begin
 MapParams;
 BindParameters;
{$ifdef myora}
 if FQueryType=qtOracle then begin // if works with Oracle then fetch fields from server and add them to FieldDefs
  if FieldDefs.Count=0 then begin
    // if FieldDefs are empty then loading fields from database
    TAOraSQL(Query).LoadFields;
    FieldDefs.Clear;
    for i:=0 to Query.FieldCount-1 do
      FieldDefs.Add(Query.FieldByIndex[i].Name,TypeAToDelphi(Query.FieldByIndex[i].FieldType),Query.FieldByIndex[i].FieldSize,Query.FieldByIndex[i].Required);
  end else begin
    // if FieldDefs are not empty then move them to the Ouery
    Query.ClearFields;
    for i:=0 to FieldDefs.Count-1 do
      Query.AddField(FieldDefs[i].Name,TypeDelphiToA(FieldDefs[i].DataType),FieldDefs[i].Size,FieldDefs[i].Required);
  end;
 end;
{$endif}

{$ifdef mymem}
 if FQueryType=qtMemory then begin // if work with MemoryDataSet then in FieldDefs fields already exists and we copy this fields into Query
  Query.ClearFields;
  for i:=0 to FieldDefs.Count-1 do
   Query.AddField(FieldDefs[i].Name,TypeDelphiToA(FieldDefs[i].DataType),FieldDefs[i].Size,FieldDefs[i].Required);
 end;
{$endif}
end;

function TDataSetQuery.VClose:boolean;
begin
 Query.Close;
 Result:=not Active;
end;

procedure TDataSetQuery.UnPrepare;
begin
 if Active then Close;
 Query.UnPrepare;
end;

procedure TDataSetQuery.VGoto(RecordNum:integer);
//var oldnumrow,i:integer;
begin
// oldnumrow:=Query.RecordCount;
 Query.ReadRecord(RecordNum);
 SyncBookm;
end;

function  TDataSetQuery.VGetFieldValue(RecordNum:integer;FieldID:integer;Buffer:pointer):boolean;
var d:TDateTimeRec;
    F:TAField;
    s:string;
    pp:pointer;
//    c:integer;
    st:TMemoryStream;
begin
 F:=Query.FieldByIndex[FieldID];
 Result:=not F.IsNull[RecordNum];
 if not Result then exit;
 case F.FieldType of
  ftoString:   begin
                s:=F.AsString[RecordNum];
                memclr(Buffer,F.FieldSize);
                memcpy(pchar(s),Buffer,Length(s));
               end;
  ftoBoolean:  pboolean(Buffer)^:=F.AsBoolean[RecordNum];
  ftoDouble:   pdouble(Buffer)^:=F.AsDouble[RecordNum];
  ftoCurrency: pdouble(Buffer)^:=F.AsCurrency[RecordNum];
  ftoDate:     begin
                d.Date:=F.AsDate[RecordNum];
                memcpy(@d,Buffer,sizeof(d));
               end;
  ftoTime:     begin
                d.Time:=F.AsTime[RecordNum];
                memcpy(@d,Buffer,sizeof(d));
               end;
  ftoDateTime: begin
                d.DateTime:=F.AsDateTime[RecordNum];
                memcpy(@d,Buffer,sizeof(d));
               end;
  ftoInteger:  pinteger(Buffer)^:=F.AsInteger[RecordNum];
  ftoSmallInt: psmallint(Buffer)^:=F.AsSmallInt[RecordNum];
  ftoWord:     pword(Buffer)^:=F.AsWord[RecordNum];
  ftoBlob,
  ftoClob:     begin
                st:=TMemoryStream.Create;
                F.ReadBlobToStream(RecordNum,st);
                pp:=AllocMem(st.Size); // copy BLOB data into temporary memory for edit time (on Post this memory will be freed)
                memcpy(st.Memory,pp,st.Size);
                PBlobRec(Buffer)^.size:=st.Size; // size of BLOB (buffer)
                PBlobRec(Buffer)^.Data:=pp; // pointer to BLOD data (buffer+4 )
                st.Free;
               end;
 else
  raise Exception.Create(sErrUnknownFieldType);
 end;
end;

procedure TDataSetQuery.VPutFieldValue(RecordNum, FieldID: integer;
  Buffer: pointer; mode: TPutMode; IfNotNull: Boolean);
var F:TAField;
begin
 F:=Query.FieldByIndex[FieldID];

 if not IfNotNull then begin F.IsNull[RecordNum]:=True; exit; end;
 case F.FieldType of
  ftoString:   F.AsString[RecordNum]:=strpas(Buffer);
  ftoBoolean:  F.AsBoolean[RecordNum]:=pboolean(Buffer)^;
  ftoDouble:   F.AsDouble[RecordNum]:=pdouble(Buffer)^;
  ftoCurrency: F.AsCurrency[RecordNum]:=pdouble(Buffer)^;
  ftoDate:     F.AsDate[RecordNum]:=TDateTimeRec(Buffer^).Date;
  ftoTime:     F.AsTime[RecordNum]:=TDateTimeRec(Buffer^).Time;
  ftoDateTime: F.AsDateTime[RecordNum]:=int64(Buffer^);
  ftoInteger:  F.AsInteger[RecordNum]:=pinteger(Buffer)^;
  ftoSmallInt: F.AsSmallInt[RecordNum]:=psmallint(Buffer)^;
  ftoWord:     F.AsWord[RecordNum]:=pword(Buffer)^;
  ftoBlob:     begin
                F.WriteBlob(RecordNum,0,PBlobRec(Buffer)^.Data,PBlobRec(Buffer)^.Size);
                FreeMem(PBlobRec(Buffer)^.Data);
                PBlobRec(Buffer)^.Data:=nil;
                PBlobRec(Buffer)^.Size:=0;
               end;
 else
  raise Exception.Create(sErrUnknownFieldType);
 end;
end;

function TDataSetQuery.GetParams:TParams;
begin
 Result:=FDParams;
end;

function TDataSetQuery.GetFieldNullHArray(FieldID : integer) : THArrayBoolean;
begin
  Result:=Query.FieldByIndex[FieldID].HArrayValuesNull;
end;

function TDataSetQuery.GetFieldHArray(FieldID : integer) : THArray;
begin
  Result:=Query.FieldByIndex[FieldID].HArrayValues;
end;

procedure TDataSetQuery.ApplyUpdates;
begin
 raise Exception.Create('Error ApplyUpdates !');
end;

function TDataSetQuery.VDeleteRecord(RecordNum:integer):TUpdateAction;
begin
{ if not Assigned(FUpdateRecord) then begin
  Result:=uaFail;
  exit;
 end;}
 Result:=uaFail;
 updType:=ukDelete;
// updRecNum:=RecordNum;
 if Assigned(FUpdateRecord) then FUpdateRecord(self,ukDelete,Result);
 if Result=uaApplied then Query.DeleteRecord(RecordNum);
end;

function TDataSetQuery.VPost(RecordNum:integer):TUpdateAction;
var UpdtKind:TUpdateKind;
begin
{ if not Assigned(FUpdateRecord) then begin
  Result:=uaAbort;
  exit;
 end;}
 case State of
  dsEdit   : UpdtKind:=ukModify;
  dsInsert : UpdtKind:=ukInsert;
  else  raise EDatabaseError.Create('Unknown State of DataSetQuery !');
 end;
 Result:=uaFail;
 updType:=UpdtKind;
// updRecNum:=RecordNum;
 if Assigned(FUpdateRecord) then FUpdateRecord(self,UpdtKind,Result);
end;

function TDataSetQuery.VInsert(RecordNum:integer):TUpdateAction;
begin
{ if not Assigned(FUpdateRecord) then begin
  Result:=uaAbort;
  exit;
 end;}
 Result:=uaApplied;
// FUpdateRecord(self,ukModify,Result);
// if Result=uaApplied then
 Query.InsertRecord(RecordNum);
end;

procedure TDataSetQuery.DefineProperties(Filer:TFiler);
begin
 inherited DefineProperties(Filer);
end;

function TDataSetQuery.GetFetched:boolean;
begin
 Result:=Query.Fetched;
end;

function TDataSetQuery.FetchNextBlock;
//var oldnumrow,i:integer;
begin
 Result:=not Query.Fetched;
 if not Result then exit;

// oldnumrow:=Query.RecordCount;
 Query.Fetch;
 SyncBookm;
end;

function TDataSetQuery.GetFieldValue(RecordNum, FieldID: integer): variant;
begin
 if Query.FieldByIndex[FieldID].IsNull[RecordNum] then begin
  Result:=Null;
  exit;
 end;
  case Query.FieldByIndex[FieldID].FieldType of
   ftoInteger : Result:=Query.FieldByIndex[FieldID].AsInteger[RecordNum];
   ftoSmallInt: Result:=Query.FieldByIndex[FieldID].AsSmallInt[RecordNum];
   ftoBoolean : Result:=Query.FieldByIndex[FieldID].AsBoolean[RecordNum];
   ftoDouble:   Result:=Query.FieldByIndex[FieldID].AsDouble[RecordNum];
   ftoDate:     Result:=Query.FieldByIndex[FieldID].AsDate[RecordNum];
   ftoTime :    Result:=Query.FieldByIndex[FieldID].AsTime[RecordNum];
   ftoDateTime: Result:=GoodDateTimeToDateTime(Query.FieldByIndex[FieldID].AsDateTime[RecordNum]);
   ftoCurrency: Result:=Query.FieldByIndex[FieldID].AsCurrency[RecordNum];
   ftoString  : Result:=Query.FieldByIndex[FieldID].AsString[RecordNum];
  else
   raise EDatabaseError.Create(sErrUnknownFieldType);
  end;
end;

procedure TDataSetQuery.SetFieldValue(RecordNum, FieldID: integer;
  Value: variant);
begin
 if Value=Null then begin
  Query.FieldByIndex[FieldID].IsNull[RecordNum]:=True;
  exit;
 end;
  case Query.FieldByIndex[FieldID].FieldType of
   ftoInteger : Query.FieldByIndex[FieldID].AsInteger[RecordNum]:=Value;
   ftoSmallInt: Query.FieldByIndex[FieldID].AsSmallInt[RecordNum]:=Value;
   ftoBoolean : Query.FieldByIndex[FieldID].AsBoolean[RecordNum]:=Value;
   ftoDouble:   Query.FieldByIndex[FieldID].AsDouble[RecordNum]:=Value;
   ftoDate:     Query.FieldByIndex[FieldID].AsDate[RecordNum]:=Value+DateDelta;
   ftoTime :    Query.FieldByIndex[FieldID].AsTime[RecordNum]:=Value;
   ftoDateTime: Query.FieldByIndex[FieldID].AsDateTime[RecordNum]:=DateTimeToGoodDateTime(Value);
   ftoCurrency: Query.FieldByIndex[FieldID].AsCurrency[RecordNum]:=Value;
   ftoString  : Query.FieldByIndex[FieldID].AsString[RecordNum]:=Value;
  else
   raise EDatabaseError.Create(sErrUnknownFieldType);
  end;
end;

function TDataSetQuery.GetField(Index: integer): TAField;
begin
 Result:=Query.FieldByIndex[Index];
end;

procedure TDataSetQuery.SetFetchCount(Value: word);
begin
{$ifdef myora}
 if FQueryType=qtOracle then TAOraSQL(Query).FetchCount:=Value;
{$endif}
end;

procedure TDataSetQuery.SetParams(const Value: TParams);
begin
 raise EDatabaseError.Create('TDataSetQuery.SetParams called!!');
end;

function TDataSetQuery.GetFetchCount: word;
begin
 case FQueryType of
  qtUnknown: Result:=0;
{$ifdef myora}
  qtOracle: Result:=TAOraSQL(Query).FetchCount;
{$endif}
 else Result:=0;
 end;
end;

function TDataSetQuery.GetRecordCount: integer;
begin
// result:= inherited GetRecordCount;
 Result:=Query.RecordCount;
end;

procedure TDataSetQuery.BeforeDel(DataSet : TADataSet; num : integer);
begin
  FBookm.Delete(num);
end;

procedure TDataSetQuery.AfterIns(DataSet : TADataSet; num : integer);
begin
  FBookm.Insert(num, @UniqBookmark);
  Inc(UniqBookmark);
end;

procedure TDataSetQuery.SyncBookm;
var i:integer;
begin
// это не очень классно! лучше как ниже закоментарено.
 FBookm.Clear;
 for i:=0 to Query.RecordCount-1 do
  FBookm.AddValue(i);
 UniqBookmark:=Query.RecordCount;

 { for i:=FBookm.Count to Query.RecordCount-1 do begin
  FBookm.AddValue(UniqBookmark);
  Inc(UniqBookmark);
 end;}
end;

function TDataSetQuery.VPrepare: boolean;
begin
 Result:=True;
end;

procedure TDataSetQuery.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^:=PBookmInfo(Buffer).Bookmark;
end;

function TDataSetQuery.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result:=PBookmInfo(Buffer).BookmarkFlag;
end;

procedure TDataSetQuery.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PBookmInfo(Buffer).Bookmark:=PInteger(Data)^;
end;

procedure TDataSetQuery.SetBookmarkFlag(Buffer: PChar;
  Value: TBookmarkFlag);
begin
  PBookmInfo(Buffer).BookmarkFlag:=Value;
end;

procedure TDataSetQuery.ReadAll;
begin
 Query.ReadAll;
 SyncBookm;
end;

function TDataSetQuery.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
// if (currentrecord <0)or(currentrecord >recordcount) then raise Exception.Create('in CreateBlobStream currentrecord !!!!');
 Result := TABlobStream.Create(Field as TBlobField, Mode);
end;

{ TABlobStream }

constructor TABlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
var br:TBlobRec;
begin
 FFieldNo:=Field.FieldNo;
 FBlobField:=Field;
 FField:=(Field.DataSet as TDataSetQuery).Query.FieldByIndex[FFieldNo-1];
 FMode:=Mode;
 FDataSet:=Field.DataSet as TDataSetQuery;
 FPosition:=0;
 FCacheSize:=0;
 FModified:=False;
 FBlobData:=nil;

 if Mode<>bmRead then begin
  if Field.ReadOnly
   then ADatabaseErrorFmt(SFieldReadOnly,[Field.DisplayName], FDataSet);
  if not (FDataSet.State in [dsEdit, dsInsert])
   then ADatabaseError(SNotEditing, FDataSet);
 end;

 if FDataSet.GetFieldData(FBlobField,@br) then begin
  FCacheSize:=br.Size;
  FBlobData:=br.Data;
 end;
end;

destructor TABlobStream.Destroy;
begin
{$ifdef ADEBUG}LogMessage('TABlobStream.Destroy BEGIN');{$endif}

 FBlobData:=nil;
 inherited Destroy;

{$ifdef ADEBUG}LogMessage('TABlobStream.Destroy END');{$endif}
end;

function TABlobStream.GetBlobSize: Longint;
begin
  Result:=FCacheSize;
end;

function TABlobStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
   soFromBeginning:begin
                    if(Offset<0)or(Offset>FCacheSize) then raise EStreamError.Create(sErrOffsetOutOfBounds);
                    FPosition:=Offset;
                   end;
   soFromCurrent:  begin
                    if(FPosition+Offset<0)or(FPosition+Offset>FCacheSize) then raise EStreamError.Create(sErrOffsetOutOfBounds);
                    inc(FPosition,Offset);
                   end;
   soFromEnd:      begin
                    if(FCacheSize-Offset<0)or(FCacheSize-Offset>FCacheSize) then raise EStreamError.Create(sErrOffsetOutOfBounds);
                    FPosition:=FCacheSize-Offset;
                   end;
   else raise EStreamError.Create('Unknown parameter "Origin" in function Seek !');
  end;
 Result:=FPosition;
end;

procedure TABlobStream.Truncate;
begin
 FBlobData:=nil;
 FPosition:=0;
 FCacheSize:=0;
 FModified:=True;
 UpdateActiveBuffer;
end;

function TABlobStream.Read(var Buffer; Count: Integer): Longint;
begin
 Result:=min(FCacheSize-FPosition,Count);
 memcpy(FBlobData+FPosition ,@Buffer,Result);
 inc(FPosition,Result);
end;

function TABlobStream.Write(const Buffer; Count: Integer): Longint;
begin
 if FPosition+Count>FCacheSize then begin
  FCacheSize:=FPosition+Count;
  ReallocMem(FBlobData,FCacheSize);
 end;

 memcpy(@Buffer,FBlobData+FPosition ,Count);
 UpdateActiveBuffer;  // refresh data in active buffer

// FPosition:=FCacheSize;
 inc(FPosition,Count);
 Result:=Count;
 FModified:=True;
end;

procedure TABlobStream.UpdateActiveBuffer;
var buf:TBlobRec;
begin
 buf.size:=FCacheSize;
 buf.data:=FBlobData;
 FDataSet.SetFieldData(FBlobField,@buf);
end;

end.

