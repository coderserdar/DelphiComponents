unit AOraSQL;

{$INCLUDE dOCI.inc}

interface

uses 
	ADataSet, OraDefines, DynamicArrays, Classes, OraDB
    {$IFDEF D6} ,Variants {$ENDIF}
    {$IFDEF D7} ,Variants {$ENDIF}
    ;

type
  TAOraSQL=class;

  TAOraParam = class (TAParam)
  private
   pData:pointer;
   pDataNull:sb2;
   LocalType:ub2;
   LocalSize:integer;

  protected
   function  GetIsNull:boolean;override;
   procedure SetIsNull(Value:boolean);override;

   function  GetValue:variant;override;
   procedure SetValue(Value:variant);override;
   function  GetAsInteger:integer;override;
   procedure SetAsInteger(Value:integer);override;
   function  GetAsWord:Word;override;
   procedure SetAsWord(Value:Word);override;
   function  GetAsSmallInt:SmallInt;override;
   procedure SetAsSmallInt(Value:SmallInt);override;
   function  GetAsDate:integer;override;
   procedure SetAsDate(Value:integer);override;
   function  GetAsTime:integer;override;
   procedure SetAsTime(Value:integer);override;
   function  GetAsDateTime:int64;override;
   procedure SetAsDateTime(Value:int64);override;
   function  GetAsString:string;override;
   procedure SetAsString(Value:string);override;
   function  GetAsDouble:double;override;
   procedure SetAsDouble(Value:double);override;
   function  GetAsCurrency:currency;override;
   procedure SetAsCurrency(Value:currency);override;
   function  GetAsBoolean:Boolean;override;
   procedure SetAsBoolean(Value:Boolean);override;
   function  GetAsInt64:int64;override;
   procedure SetAsInt64(Value:int64);override;

  public
   constructor Create(ParamName:string;ParamFieldType:TAFieldType;ParamParamType:TAParamType);override;
   destructor Destroy; override;
   procedure Clear;override;
  end;

  TAOraField = class(TAField)
  private
  // temporary arrays for storing data in oracle format (after data moves into persistent arrays)
   pData:THArray;
   pDataNull:THArray;
   pDataLen:THArraySmallInt;
   FLocalType:ub2;
   FLocalSize:integer;
   FMapped:boolean; // if this field isa exists in oracle
   defhp:pOCIDefine;

   procedure ZeroBuffer;
   procedure ClearTemp;
   procedure Add(CountF:integer); // move (and converts) data from temporary arrays(pData,...) into persistent ones in Delphi format

  protected
   procedure Clear; override;
   procedure Allocate; override;
   procedure DeleteRecord(RecordNum:integer);

  public
   constructor Create(Parent:TADataSet;FieldName:string;RFieldType:TAFieldType;FieldSize:word;Required:boolean);override;

   function WriteBlob(RecordNum:integer;Offset:integer;Buffer:pointer;Size:integer):ub4;override;
   function ReadBlob(RecordNum:integer;Offset:integer;Buffer:pointer;Size:integer):ub4; override;
   function GetLobLength(RecordNum:integer):integer;override;
   procedure ClearBlob(RecordNum:integer);override;

  end;

  TAOraSQL = class(TADataSet)
  private
   FDatabase:TOraDB;
   FPrepared,FSelfPrepared:boolean;
   FSQL:TStrings;
   FFetchCount: integer;
   FUni:boolean;

   procedure SetSQL(Value:TStrings);

  protected
   mystmthp:pOCIStmt;
   myerrhp:pOCIError;
   stmt_type:ub2;

   function TestError(where:string;ex:sword):sword;
   procedure MapParam;
   procedure MapFields;
   procedure SetFetchCount(Value:integer);
   procedure OpenDatabase;
  public
   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
   procedure Open; override;
   procedure Close; override;
   procedure Prepare;override;
   procedure UnPrepare;override;
   procedure ExecSQL;
   function ReadRecord(RecordNum:integer):boolean; override;

   procedure ClearParams;override;
   procedure SetQuery(Query:string);
   function CreateAField(FieldName:string;FieldType:TAFieldType;FieldSize:word;Required:boolean):TAField;override;
//   procedure AddField(FieldName:string;FieldType:TAFieldType;FieldSize:word;Required:boolean); override;
   procedure AddParam(ParamName:string;FieldType:TAFieldType;ParamType:TAParamType);override;
   procedure LoadFields;

   procedure Fetch;override;
   function GetNextSequenceNumber(SequenceName: String): Integer;

  published
   property Database:TOraDB read FDatabase write FDatabase;
   property FetchCount:integer read FFetchCount write SetFetchCount;
   property SQL:TStrings read FSQL write SetSQL;
   property UniDirectional:boolean read FUni write FUni;
  end;

procedure goodOra2Delphi(FT:TAFieldType;pin,pout:pointer);
procedure goodDelphi2Ora(FT:TAFieldType;pin,pout:pointer);


procedure Register;

implementation

uses SysUtils, Windows, DBConsts, GoodDate, OraError, OraUtils, dOCIMessages;

procedure Register;
begin
  RegisterComponents('Data Access', [TAOraSQL]);
end;

procedure goodOra2Delphi(FT:TAFieldType;pin,pout:pointer);
var od:^oradate;
begin
 case FT of
//  ftLargeInt: OraNumToInt64(pin,pout,2);
  ftoCurrency: pcurrency(pout)^:=pdouble(pin)^; //***
  ftoDate:    begin
               od:=pin;
               if od.Century<>0 then
                pinteger(pout)^:=MakeGoodDate((od.Century-100)*100+(od.Year-100),od.Month,od.Day);
             end;
  ftoTime:    begin
               od:=pin;
               if od.Century<>0 then
                pinteger(pout)^:=MakeGoodTime(od.Hour-1,od.Minute-1,od.Second-1,0);
             end;
  ftoDateTime:begin
               od:=pin;
               if od.Century<>0 then begin
                pInt64(pout)^:=MakeGoodDateTime((od.Century-100)*100+(od.Year-100),od.Month,od.Day,od.Hour-1,od.Minute-1,od.Second-1,0);
               end;
             end;
 end;
end;

procedure goodDelphi2Ora(FT:TAFieldType;pin,pout:pointer);
var od:^oradate;
    dd,dm,dy:word;
    th,tm,ts,tms:word;
begin
 case FT of
  ftoCurrency:pdouble(pout)^:=pcurrency(pin)^; //***
  ftoDate: begin
           od:=pout;
           UnMakeGoodDate(pinteger(pin)^,dy,dm,dd);
           od.century:=100+dy div 100;
           od.year:=dy mod 100 + 100;
           od.month:=dm;
           od.day:=dd;
           od.hour:=1;
           od.minute:=1;
           od.second:=1;
          end;
  ftoTime: begin
           od:=pout;
           UnMakeGoodTime(pinteger(pin)^,th,tm,ts,tms);
           od.century:=0;
           od.year:=0;
           od.month:=0;
           od.day:=0;
           od.hour:=th+1;
           od.minute:=tm+1;
           od.second:=ts+1;
          end;
  ftoDateTime: begin
               od:=pout;
               UnMakeGoodDateTime(pinteger(pin)^,dy,dm,dd,th,tm,ts,tms);
               od.century:=100+dy div 100;
               od.year:=dy mod 100 + 100;
               od.month:=dm;
               od.day:=dd;
               od.hour:=th+1;
               od.minute:=tm+1;
               od.second:=ts+1;
              end;
 end;
end;


 { AOraParam }

constructor TAOraParam.Create(ParamName:string;ParamFieldType:TAFieldType;ParamParamType:TAParamType);
begin
 inherited Create(ParamName,ParamFieldType,ParamParamType);
 case FieldType of
  ftoString:   LocalSize:=4001;
  ftoBoolean:  LocalSize:=sizeof(boolean);
  ftoDouble:   LocalSize:=sizeof(double);
  ftoCurrency: LocalSize:=sizeof(double); //***21;
  ftoDate:     LocalSize:=sizeof(oradate);
  ftoTime:     LocalSize:=sizeof(oradate);
  ftoDateTime: LocalSize:=sizeof(oradate);
  ftoInt64:    LocalSize:=sizeof(int64);
  ftoInteger:  LocalSize:=sizeof(integer);
  ftoSmallInt: LocalSize:=sizeof(smallint);
  ftoWord:     LocalSize:=sizeof(word);
 else
  raise Exception.Create('Unknown data type !');
 end;

 pData:=AllocMem(LocalSize);

 case FieldType of
  ftoString:  LocalType:=SQLT_STR;
  ftoBoolean: LocalType:=SQLT_INT;
  ftoDouble:  LocalType:=SQLT_FLT;
  ftoCurrency:LocalType:=SQLT_FLT; //***SQLT_NUM;
  ftoDate:    LocalType:=SQLT_DAT;
  ftoTime:    LocalType:=SQLT_DAT;
  ftoDateTime:LocalType:=SQLT_DAT;
  ftoInt64:   LocalType:=SQLT_FLT;//???
  ftoInteger: LocalType:=SQLT_INT;
  ftoSmallInt:LocalType:=SQLT_INT;
  ftoWord:    LocalType:=SQLT_INT;
 else
  LocalType:=65535;
 end;
end;

destructor TAOraParam.Destroy;
begin
{$ifdef ADEBUG}LogMessage('TAOraParam.Destroy BEGIN');{$endif}

 FreeMem(pData);
 inherited Destroy;

{$ifdef ADEBUG}LogMessage('TAOraParam.Destroy END');{$endif}
end;

function TAOraParam.GetIsNull:boolean;
begin
 Result:=pDataNull=-1;
end;

procedure TAOraParam.SetIsNull(Value:boolean);
begin
 if Value then pDataNull:=-1 else pDataNull:=0;
end;

function  TAOraParam.GetAsInteger:integer;
begin
 TestType(ftoInteger);
 if IsNull then Result:=0 else Result:=pInteger(pData)^;
end;

procedure TAOraParam.SetAsInteger(Value:integer);
begin
 TestType(ftoInteger);
 SetIsNull(False);
 pInteger(pData)^:=Value;
end;

function  TAOraParam.GetAsDate:integer;
begin
 TestType(ftoDate);
 if IsNull then Result:=0 else goodOra2Delphi(ftoDate,pData,@Result);
end;

procedure TAOraParam.SetAsDate(Value:integer);
begin
 TestType(ftoDate);
 SetIsNull(False);
 goodDelphi2Ora(ftoDate,@Value,pData);
end;

function  TAOraParam.GetAsString:string;
begin
 TestType(ftoString);
 if IsNull then Result:='' else Result:=pchar(pData);
end;

procedure TAOraParam.SetAsString(Value:string);
begin
 TestType(ftoString);
 SetIsNull(False);
 strpcopy(pData,Value);
end;

function  TAOraParam.GetAsDouble:double;
begin
 TestType(ftoDouble);
 if IsNull then Result:=0 else Result:=pdouble(pData)^;
end;

procedure TAOraParam.SetAsDouble(Value:double);
begin
 TestType(ftoDouble);
 SetIsNull(False);
 pdouble(pData)^:=Value;
end;

function  TAOraParam.GetAsCurrency:currency;
begin
 TestType(ftoCurrency);
 if IsNull then Result:=0 else Result:=pdouble(pData)^;
end;

procedure TAOraParam.SetAsCurrency(Value:currency);
begin
 TestType(ftoCurrency);
 SetIsNull(False);
 pdouble(pData)^:=Value;
end;

function  TAOraParam.GetAsBoolean:Boolean;
begin
 TestType(ftoBoolean);
 if IsNull then Result:=False else Result:=pbyte(pData)^<>0;
end;

procedure TAOraParam.SetAsBoolean(Value:Boolean);
begin
 TestType(ftoBoolean);
 SetIsNull(False);
 if Value then pbyte(pData)^:=1 else pbyte(pData)^:=0;
end;

function TAOraParam.GetAsDateTime: int64;
begin
 TestType(ftoDateTime);
 if IsNull then Result:=0 else goodOra2Delphi(ftoDateTime,pData,@Result);
end;

function TAOraParam.GetAsTime: integer;
begin
 TestType(ftoTime);
 if IsNull then Result:=0 else goodOra2Delphi(ftoTime,pData,@Result);
end;

procedure TAOraParam.SetAsDateTime(Value: int64);
begin
 TestType(ftoDateTime);
 SetIsNull(False);
 goodDelphi2Ora(ftoDateTime,@Value,pData);
end;

procedure TAOraParam.SetAsTime(Value: integer);
begin
 TestType(ftoTime);
 SetIsNull(False);
 goodDelphi2Ora(ftoTime,@Value,pData);
end;

function TAOraParam.GetAsSmallInt: SmallInt;
begin
 TestType(ftoSmallInt);
 if IsNull then Result:=0 else Result:=psmallint(pData)^;
end;

function TAOraParam.GetAsWord: Word;
begin
 TestType(ftoWord);
 if IsNull then Result:=0 else Result:=pword(pData)^;
end;

procedure TAOraParam.SetAsSmallInt(Value: SmallInt);
begin
 TestType(ftoSmallInt);
 SetIsNull(False);
 psmallint(pData)^:=Value;
end;

procedure TAOraParam.SetAsWord(Value: Word);
begin
 TestType(ftoWord);
 SetIsNull(False);
 pword(pData)^:=Value;
end;

function TAOraParam.GetAsInt64: int64;
begin
 TestType(ftoInt64);
 if IsNull then Result:=0 else Result:=pInt64(pData)^;
end;

procedure TAOraParam.SetAsInt64(Value: int64);
begin
 TestType(ftoInt64);
 SetIsNull(False);
 pInt64(pData)^:=Value;
end;

procedure TAOraParam.Clear;
begin
 SetIsNull(True);
end;

function TAOraParam.GetValue: variant;
begin
 if IsNull then begin
  Result:=Null;
  exit;
 end;
 case FieldType of
  ftoString: Value:=AsString;
  ftoSmallint: Value:=AsSmallInt;
  ftoInteger: Value:=AsInteger;
  ftoWord: Value:=AsWord;
  ftoBoolean: Value:=AsBoolean;
  ftoDouble: Value:=AsDouble;
  ftoCurrency: Value:=AsCurrency;
  ftoDate: Value:=AsDate;
  ftoTime: Value:=AsTime;
//  ftoDateTime: Value:=AsDateTime;
 end;
end;

procedure TAOraParam.SetValue(Value: variant);
begin
 if Value=Null then begin
  Clear;
  exit;
 end;
 case FieldType of
  ftoString: AsString:=Value;
  ftoSmallint: AsSmallInt:=Value;
  ftoInteger: AsInteger:=Value;
  ftoWord: AsWord:=Value;
  ftoBoolean: AsBoolean:=Value;
  ftoDouble: AsDouble:=Value;
  ftoCurrency: AsCurrency:=Value;
  ftoDate: AsDate:=Value;
  ftoTime: AsTime:=Value;
//  ftoDateTime: AsDateTime:=Value;
 end;
end;

{ TAOraField }

constructor TAOraField.Create(Parent:TADataSet;FieldName:string;RFieldType:TAFieldType;FieldSize:word;Required:boolean);
begin
 inherited Create(Parent,FieldName,RFieldType,FieldSize,Required);

 pData:=nil;
 pDataNull:=nil;
 pDataLen:=nil;
end;

procedure TAOraField.Add(CountF:integer);
var  resi,redi:pointer;
     i,j:integer;
     vcurrency:currency;
     vdate,vtime:integer;
     vdatetime:int64;
label next,nexti;
begin
 if not FMapped then begin
   if Assigned(ValuesNull) then ValuesNull.AddFillValues(CountF);
   Values.AddFillValues(CountF);
 end else begin
  if Assigned(ValuesNull) then begin
   ValuesNull.AddFillValues(CountF);
   resi:=pDataNull.Memory;
   redi:=ValuesNull.GetAddr(ValuesNull.Count-CountF);
   asm // move Null-indicators from sb2(Oracle format) to Boolean (Delphi format)
    pushad
    pushfd
    mov   esi,resi
    mov   edi,redi
    mov   ecx,CountF
  nexti:
    xor   bl,bl
    LODSW
    test  ax,ax
    jnz   next
    mov   bl,1            // it is True
  next:
    mov   [edi],bl
    inc   edi
    dec   ecx
    jnz   nexti
    popfd
    popad
   end;
  end;

 // moving field values
  case FieldType of
   ftoString:  Values.AddMany(pData.Memory,CountF);
   ftoBoolean: Values.AddMany(pData.Memory,CountF);
   ftoDouble:  Values.AddMany(pData.Memory,CountF);
   ftoCurrency:for i:=0 to CountF-1 do begin
                goodOra2Delphi(ftoCurrency,THArrayDouble(pData).GetAddr(i),@vcurrency);
                THArrayCurrency(Values).AddValue(vcurrency);
               end;
   ftoDate:    for i:=0 to CountF-1 do begin
                goodOra2Delphi(ftoDate,pData.GetAddr(i),@vdate);
                THArrayInteger(Values).AddValue(vdate);
               end;
   ftoTime:    for i:=0 to CountF-1 do begin
                goodOra2Delphi(ftoTime,pData.GetAddr(i),@vtime);
                THArrayInteger(Values).AddValue(vdate);
               end;
   ftoDateTime:for i:=0 to CountF-1 do begin
                goodOra2Delphi(ftoDateTime,pData.GetAddr(i),@vdatetime);
                THArrayInt64(Values).AddValue(vdatetime);
               end;
   ftoInteger: Values.AddMany(pData.Memory,CountF);
   ftoSmallInt:Values.AddMany(pData.Memory,CountF);
   ftoWord:    Values.AddMany(pData.Memory,CountF);
   ftoBlob,ftoClob:begin
                Values.AddMany(pData.Memory,CountF);// copying BLOB locators into persistent array
                // скопировали столько дескрипторов сколько строк достали
                // остальные дескрипторы освобождаем
                for j:=CountF to pData.Count-1 do
                 TAOraSQL(FParent).TestError('Add - OCIDescriptorFree - ',TAOraSQL(FParent).Database.OCIDescriptorFree(ppointer(pData.GetAddr(j))^,OCI_DTYPE_LOB));

{                for j:=0 to CountF-1 do begin
                 ppointer(pData.GetAddr(j))^:=nil;
                 // аллокайтим новые дескрипторы для сдедуюшего вызова fetch
                 TAOraSQL(FParent).TestError('Prepare - DescriptorAlloc - ',TAOraSQL(FParent).Database.OCIDescriptorAlloc(TAOraSQL(FParent).Database.myenvhp,pData.GetAddr(j),OCI_DTYPE_LOB,0,nil));
                end;}
               end;
  else
   raise Exception.Create('Unknown data type !');
  end;
 end; 
end;

procedure TAOraField.ZeroBuffer;
begin
 if Assigned(pData) and not (FieldType in [ftoBlob,ftoClob]) then pData.Zero;
 if Assigned(pDataNull) then pDataNull.Zero;
 if Assigned(pDataLen) then pDataLen.Zero;
end;

function TAOraField.GetLobLength(RecordNum: integer):integer;
var len:ub4;
begin
 TestType(ftoBlob);
 TAOraSQL(FParent).TestError('GetLobLength - OCILobGetLength - ',TAOraSQL(FParent).Database.OCILobGetLength(TAOraSQL(FParent).Database.mysvchp,TAOraSQL(FParent).myerrhp,THArrayPointer(Values)[RecordToInternal(RecordNum)],len));
 Result:=len;
end;

procedure TAOraField.ClearBlob(RecordNum: integer);
begin
 TestType(ftoBlob);
 TAOraSQL(FParent).TestError('ClearBlob - OCILobTrim - ',TAOraSQL(FParent).Database.OCILobTrim(TAOraSQL(FParent).Database.mysvchp,TAOraSQL(FParent).myerrhp,THArrayPointer(Values)[RecordToInternal(RecordNum)],0));
end;

function TAOraField.WriteBlob(RecordNum:integer;Offset:integer;Buffer:pointer;Size:integer):ub4;
begin
 TestType(ftoBlob);
 Result:=Size;
 TAOraSQL(FParent).TestError('WriteBlob - OCILobWrite - ',TAOraSQL(FParent).Database.OCILobWrite(TAOraSQL(FParent).Database.mysvchp,TAOraSQL(FParent).myerrhp,THArrayPointer(Values)[RecordToInternal(RecordNum)],Result,Offset+1,Buffer,Result,OCI_ONE_PIECE,nil,nil,0,0));
end;

function TAOraField.ReadBlob(RecordNum:integer;Offset:integer;Buffer:pointer;Size:integer):ub4;
begin
 TestType(ftoBlob);
 Result:=Size;
 TAOraSQL(FParent).TestError('ReadBlob - OCILobRead - ',TAOraSQL(FParent).Database.OCILobRead(TAOraSQL(FParent).Database.mysvchp,TAOraSQL(FParent).myerrhp,THArrayPointer(Values)[RecordToInternal(RecordNum)],Result,Offset+1,Buffer,Result,nil,nil,0,0));
end;

{function TAOraField.ReadBlobToStream(RecordNum:integer; Stream: TStream): ub4;
var buf:array[0..16383] of byte;
    sz:ub4;
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

function TAOraField.WriteBlobFromStream(RecordNum: integer; Stream: TStream):ub4;
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
   buf[sz]:=0; buf[sz+1]:=0; buf[sz+2]:=0;
   Result:=Result+WriteBlob(RecordNum,Offset,@buf,sz);
   inc(Offset,sz);
  until Offset=Stream.Size;
 end;
end;}

procedure TAOraField.DeleteRecord(RecordNum:integer);
begin
 if FieldType in [ftoBlob,ftoClob] then
  if THArrayPointer(Values)[RecordNum]<>nil
   then TAOraSQL(FParent).TestError('DeleteRecord - OCIDescriptorFree - ',TAOraSQL(FParent).Database.OCIDescriptorFree(THArrayPointer(Values)[RecordNum],OCI_DTYPE_LOB));
 inherited DeleteRecord(RecordNum);
end;


procedure TAOraField.Clear;
var j:integer;
begin
 ClearTemp;
// if(pData<>nil)or(pDataNull<>nil)or(pDataLen<>nil)then raise Exception.Create('TAOraField.Clear pData<>nil!!');

 if Assigned(Values) then
  if FieldType in [ftoBlob,ftoClob] then
   for j:=0 to Values.Count-1 do
    if THArrayPointer(Values)[j]<>nil
      then TAOraSQL(FParent).TestError('Clear - DescriptorFree - ',TAOraSQL(FParent).Database.OCIDescriptorFree(THArrayPointer(Values)[j],OCI_DTYPE_LOB));

 if Assigned(defhp) then begin
//  TAOraSQL(FParent).TestError('def handle free - ',TAOraSQL(FParent).Database.OCIHandleFree(defhp,OCI_HTYPE_DEFINE));
  defhp:=nil;
 end;
 inherited Clear;
end;

procedure TAOraField.ClearTemp;
{var j:integer;
    p:pointer;}
begin
{ if FieldType in [ftoBlob,ftoClob] then
  if pData<>nil then
   for j:=0 to pData.Count-1 do begin
    p:=ppointer(pData.GetAddr(j))^;
    if p<>nil then begin
     TAOraSQL(FParent).TestError('DescriptorFree - ',TAOraSQL(FParent).Database.OCIDescriptorFree(p,OCI_DTYPE_LOB));
     ppointer(pData.GetAddr(j))^:=nil;
    end;
   end;
 }
  if pData<>nil then begin pData.Free; pData:=nil; end;
  if pDataNull<>nil then begin pDataNull.Free; pDataNull:=nil; end;
  if pDataLen<>nil then begin pDataLen.Free; pDataLen:=nil; end;
  if Values<>nil then Values.Hold;
  if ValuesNull<>nil then ValuesNull.Hold;
end;

procedure TAOraField.Allocate;
begin
 inherited Allocate;

 pData:=THArray.Create;
 case FieldType of
  ftoString:   pData.ItemSize:=FieldSize;
  ftoBoolean:  pData.ItemSize:=sizeof(boolean);
  ftoDouble:   pData.ItemSize:=sizeof(double);
  ftoCurrency: pData.ItemSize:=sizeof(double); //***21;
  ftoDate:     pData.ItemSize:=sizeof(oradate);
  ftoTime:     pData.ItemSize:=sizeof(oradate);
  ftoDateTime: pData.ItemSize:=sizeof(oradate);
  ftoInteger:  pData.ItemSize:=sizeof(integer);
  ftoSmallInt: pData.ItemSize:=sizeof(smallint);
  ftoWord:     pData.ItemSize:=sizeof(word);
  ftoBlob,ftoClob:pData.ItemSize:=sizeof(pointer);
 else
  raise Exception.Create('Unknown data type !');
 end;

 pDataNull:=THArray.Create;
 pDataNull.ItemSize:=sizeof(sb2);
 pDataLen:=THArraySmallInt.Create;

 pData.SetCapacity(TAOraSQL(FParent).FetchCount);
 pData.AddFillValues(TAOraSQL(FParent).FetchCount);
 pDataNull.SetCapacity(TAOraSQL(FParent).FetchCount);
 pDataNull.AddFillValues(TAOraSQL(FParent).FetchCount);
 pDataLen.SetCapacity(TAOraSQL(FParent).FetchCount);
 pDataLen.AddFillValues(TAOraSQL(FParent).FetchCount);

 case FieldType of
  ftoString:   FLocalType:=SQLT_CHR;
  ftoBoolean:  FLocalType:=SQLT_CHR;
  ftoDouble:   FLocalType:=SQLT_FLT;
  ftoCurrency: FLocalType:=SQLT_FLT;
  ftoDate:     FLocalType:=SQLT_DAT;
  ftoTime:     FLocalType:=SQLT_DAT;
  ftoDateTime: FLocalType:=SQLT_DAT;
  ftoInteger:  FLocalType:=SQLT_INT;
  ftoSmallInt: FLocalType:=SQLT_INT;
  ftoWord:     FLocalType:=SQLT_INT;
  ftoBlob:     FLocalType:=SQLT_BLOB;
  ftoClob:     FLocalType:=SQLT_CLOB;
 else
  raise Exception.Create('Unknown data type !');
 end;

 case FieldType of
  ftoString:   FLocalSize:=FieldSize;
  ftoBoolean:  FLocalSize:=sizeof(boolean);
  ftoDouble:   FLocalSize:=sizeof(double);
  ftoCurrency: FLocalSize:=sizeof(double);
  ftoDate:     FLocalSize:=sizeof(oradate);
  ftoTime:     FLocalSize:=sizeof(oradate);
  ftoDateTime: FLocalSize:=sizeof(oradate);
  ftoInteger:  FLocalSize:=sizeof(integer);
  ftoSmallInt: FLocalSize:=sizeof(smallint);
  ftoWord:     FLocalSize:=sizeof(word);
  ftoBlob:     FLocalSize:=sizeof(pointer);
  ftoClob:     FLocalSize:=sizeof(pointer);
 else
  raise Exception.Create('Unknown data type !');
 end;

 FMapped:=False;
 defhp:=nil;
end;

 { TAOraSQL }

constructor TAOraSQL.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FFetchCount:=100;
 FSQL:=TStringList.Create;
 myerrhp:=nil;
end;

destructor TAOraSQL.Destroy;
begin
{$ifdef ADEBUG}LogMessage('TAOraSQL.Destroy BEGIN');{$endif}

 Close;
// ClearParams;
// FParams.Free;
 FSQL.Free;
 inherited Destroy;

{$ifdef ADEBUG}LogMessage('TAOraSQL.Destroy END');{$endif}
end;

procedure TAOraSQL.OpenDatabase;
begin
 if not Assigned(FDatabase) then begin
  raise Exception.Create('Database is not assigned.');
 end;

 if not Database.Active then begin
  Database.Open;
  if not Database.Active then raise Exception.Create('Database is not active.');
 end;
end;

procedure TAOraSQL.Open;
var e:sword;
begin
 if Active then exit;
 FSelfPrepared:=not FPrepared;
 if FSelfPrepared then Prepare;

 FFetched:=False;
 if stmt_type<>OCI_STMT_SELECT then
  raise Exception.Create('Do not SELECT - statement !');

 MapParam;

 e:=Database.OCIStmtExecute(Database.mysvchp,mystmthp,myerrhp,0,0,nil,nil,OCI_DEFAULT);
 if e<>OCI_NO_DATA
  then TestError('Open - OCIStmtExecute - ',e)
  else FFetched:=True;

 inherited Open;
 if e<>OCI_NO_DATA then MapFields;
end;

procedure TAOraSQL.Close;
begin
 if not Active then exit;
 inherited Close;
 if FSelfPrepared then UnPrepare;
end;

procedure TAOraSQL.Prepare;
var texta:pchar;
    stmt_type_len:ub4;
begin
 if FPrepared then exit;

 if Trim(FSQL.Text)='' then ADatabaseError(sEmptySQLStatement, Self); {roma 27.05.2002} //raise Exception.Create('Error: SQL statement is empty.');

 OpenDatabase;

 TestError('Prepare - OCIHandleAlloc - ',Database.OCIHandleAlloc(Database.myenvhp,myerrhp,OCI_HTYPE_ERROR,0,nil));
 TestError('Prepare - OCIHandleAlloc - ',Database.OCIHandleAlloc(Database.myenvhp,mystmthp,OCI_HTYPE_STMT,0,nil));

 texta:=AllocMem(Length(FSQL.Text)+1);
 strpcopy(texta,FSQL.Text);

 if Database.Preferences.ConvertCRLF then replaceDA(texta);

 TestError('Prepare - OCIStmtPrepare - ',Database.OCIStmtPrepare(mystmthp,myerrhp,texta,strlen(texta),OCI_NTV_SYNTAX,OCI_DEFAULT));
 stmt_type_len:=sizeof(stmt_type);
 TestError('Prepare - OCIAttrGet(STMT_TYPE) - ',Database.OCIAttrGet(mystmthp,OCI_HTYPE_STMT,@stmt_type,@stmt_type_len,OCI_ATTR_STMT_TYPE,myerrhp));

 FreeMem(texta);

 FPrepared:=True;
end;

procedure TAOraSQL.UnPrepare;
begin
 if not FPrepared then exit;
 if Active then ADatabaseError(SDataSetOpen, Self); {roma 27.05.2002} //if Active then Close;

 if Assigned(mystmthp) then TestError('UnPrepare - OCIHandleFree - ',Database.OCIHandleFree(mystmthp,OCI_HTYPE_STMT));
 mystmthp:=nil;

 if Assigned(myerrhp) then TestError('UnPrepare - OCIHandleFree - ',Database.OCIHandleFree(myerrhp,OCI_HTYPE_ERROR));
 myerrhp:=nil;

 FPrepared:=False;
end;

procedure TAOraSQL.ExecSQL;
begin
 if Active then exit;
 FSelfPrepared:=not FPrepared;
 if FSelfPrepared then Prepare;

 MapParam;
 TestError('ExecSQL - OCIStmtExecute ',Database.OCIStmtExecute(Database.mysvchp,mystmthp,myerrhp,1,0,nil,nil,OCI_DEFAULT));

 if FSelfPrepared then UnPrepare;
end;

procedure TAOraSQL.MapParam;
var Param:TAOraParam;
    texta:array[0..4000] of char;
    mybindhp:pOCIBind;
    i:integer;
begin
 mybindhp:=nil;
 for i:=0 to ParamCount-1 do begin
  Param:=TAOraParam(ParamByIndex[i]);
  strpcopy(texta,':'+Param.Name);
  TestError('MapParam - OCIBindByName - ',Database.OCIBindByName(mystmthp,mybindhp,myerrhp,texta,strlen(texta),
            Param.pData,Param.LocalSize,Param.LocalType,@(Param.pDataNull),nil,nil,0,nil,OCI_DEFAULT));
//  if mybindhp<>nil then raise Exception.Create('MapParam : mybindhp<>nil!!!');
 end;
end;

procedure TAOraSQL.MapFields;
var i:integer;
    paramcnt:ub4;
    mypard:pOCIParam;
    colname:pchar;
    colnamelen:ub4;
    fName:string;
//    ID:integer;
    CF:TAOraField;
begin
 TestError('MapFields - OCIAttrGet(Param Count) - ',Database.OCIAttrGet(mystmthp,OCI_HTYPE_STMT,@paramcnt,nil,OCI_ATTR_PARAM_COUNT,myerrhp));
 for i:=0 to paramcnt-1 do begin
  mypard:=nil;
  TestError('MapFields - OCIParamGet',Database.OCIParamGet(mystmthp,OCI_HTYPE_STMT,myerrhp,mypard,i+1));
  TestError('MapFields - OCIAttrGet(Column Name) - ',Database.OCIAttrGet(mypard,OCI_DTYPE_PARAM,@colname,@colnamelen,OCI_ATTR_NAME,myerrhp));
  fName:=strpas(colname);
  SetLength(fName,colnamelen);
   if FieldExists(fName) then begin
    CF:=TAOraField(FieldByName[fName]);
    CF.FMapped:=True;
    TestError('MapFields - OCIDefineByPos - ',Database.OCIDefineByPos(mystmthp,CF.defhp,myerrhp,i+1,CF.pData.Memory,CF.FLocalSize,CF.FLocalType,CF.pDataNull.Memory,CF.pDataLen.Memory,nil,OCI_DEFAULT));
    TestError('MapFields - OCIDefineArrayOfStruct - ',Database.OCIDefineArrayOfStruct(CF.defhp,myerrhp,CF.FLocalSize,sizeof(sb2),sizeof(smallint),0));
   end;
 end;
end;

procedure TAOraSQL.SetSQL(Value:TStrings);
begin
 FSQL.Assign(Value);
end;

function TAOraSQL.TestError(where:string;ex:sword):sword;
var errcode:sb4;
    errbuf:array[0..511] of char;
begin
 Result:=ex;
 case ex of
  OCI_SUCCESS: exit;
  OCI_SUCCESS_WITH_INFO: raise EOraError.Create(ex,0,'Oracle Error: OCI success with info');
  OCI_NEED_DATA: raise EOraError.Create(ex,0,'Oracle Error: OCI need data');
  OCI_NO_DATA: raise EOraError.Create(ex,0,'Oracle Error: OCI no data');
  OCI_ERROR: begin
              Database.OCIErrorGet(myerrhp,1,nil,errcode,errbuf,sizeof(errbuf),OCI_HTYPE_ERROR);
              raise EOraError.Create(ex,errcode,'Oracle Error '{#'+inttostr(errcode)+': '}+strpas(errbuf));
             end;
  OCI_INVALID_HANDLE: raise EOraError.Create(ex,0,'Oracle Error: OCI invalid handle');
  OCI_STILL_EXECUTING: raise EOraError.Create(ex,0,'Oracle Error: OCI still execute');
  else raise EOraError.Create(-20001,-20001,'UNKNOWN ORACLE ERROR!');
 end;
end;
                    
{procedure TAOraSQL.AddField(FieldName:string;FieldType:TAFieldType;FieldSize:word;Required:boolean);
var F:TAOraField;
var i:integer;
begin
 if FieldName = '' then ADatabaseError(SFieldNameMissing, self);
 for i:=0 to FieldCount-1 do
  if AnsiCompareText(FieldByIndex[i].Name,FieldName)=0 then ADatabaseError(Format(SDuplicateFieldName,[FieldName,self.name]));

 F:=TAOraField.Create(TADataSet(self),FieldName,FieldType,FieldSize,Required);
 FFields.AddValue(F);
end;}

function TAOraSQL.CreateAField(FieldName:string;FieldType:TAFieldType;FieldSize:word;Required:boolean): TAField;
begin
 Result:=TAOraField.Create(self,FieldName,FieldType,FieldSize,Required);
end;

procedure TAOraSQL.SetFetchCount(Value:integer);
begin
 if (Value<1) or (Value>32767) then raise Exception.Create('The FetchCount value ('+IntToStr(Value)+') too large!');
 if FPrepared then raise Exception.Create('Cannot set the FetchCount property because the TAOraSQL is prepared!');
 FFetchCount:=Value;
end;

function TAOraSQL.ReadRecord(RecordNum:integer):boolean;
begin
 while (not FFetched) and (RecordNum>=RecordCount) do Fetch;
 Result:=inherited ReadRecord(RecordNum);
end;

procedure TAOraSQL.Fetch;
var i,j:integer;
    numrow,oldnumrow:integer;
    Res:sword;
    CF:TAOraField;
begin
 if UniDirectional then begin ForgetValues; FBeginRecord:=RecordCount; end;

 oldnumrow:=RecordCount;

 for i:=0 to FieldCount-1 do begin
  CF:=TAOraField(FieldByIndex[i]);
  CF.ZeroBuffer;
  if CF.FieldType in [ftoBlob,ftoClob] then
    for j:=0 to FFetchCount-1 do
       TestError('Fetch - DescriptorAlloc - ',Database.OCIDescriptorAlloc(Database.myenvhp,CF.pData.GetAddr(j),OCI_DTYPE_LOB,0,nil));
 end;

 // do not need to call TestError here (see below)
 Res:=Database.OCIStmtFetch(mystmthp,myerrhp,FFetchCount,OCI_FETCH_NEXT,OCI_DEFAULT);

 if (Res=OCI_SUCCESS) or (Res=OCI_NO_DATA) or (Res=OCI_NEED_DATA) then begin
  TestError('Fetch - OCIAttrGet (fetched numrows)- ',Database.OCIAttrGet(mystmthp,OCI_HTYPE_STMT,@numrow,nil,OCI_ATTR_ROW_COUNT,myerrhp));
  FCount:=numrow;

  if numrow<>oldnumrow then
   for i:=0 to FieldCount-1 do
    TAOraField(FieldByIndex[i]).Add(numrow-oldnumrow); // moving from temporary arrays to persistent ones

  if (Res=OCI_NO_DATA) then begin // fetching finished
   FFetched:=True;
   for i:=0 to FieldCount-1 do begin
    CF:=TAOraField(FieldByIndex[i]);
    CF.ClearTemp;
   end;
  end;
 end else begin
  FFetched:=True;
  TestError('Fetch - OCIStmtFetch - ',Res);
 end;
end;

function TAOraSQL.GetNextSequenceNumber(SequenceName: String): Integer;
begin
  SetQuery('BEGIN SELECT ' + SequenceName + '.NEXTVAL INTO :Value FROM DUAL; END;');
  AddParam('Value', ftoInteger, ptoOutput);
  ExecSQL;
  Result := ParamByName['Value'].AsInteger;
end;

procedure TAOraSQL.LoadFields;
var i:integer;
    mypard:pOCIParam;
    colname:pchar;
    colnamelen:ub4;
    Name:string;
    isnull:ub1;
    OraType:ub2;
    OraSize:ub2;
    OraPrec:ub2;
    OraScale:sb1;
    parSize:ub4;
    data:ub4;
    DType:TAFieldType;
    DSize:integer;
    selfprepared:boolean;
    RealIntPrec, RealSmallIntPrec:integer;
begin
 if Active then exit;
 selfprepared:=not FPrepared;
 if selfprepared then Prepare;
 ClearFields;
try
 MapParam;
 // OCI_DESCRIBE_ONLY says do not execute query, only fetch field descriptions (this is params)
 TestError('LoadFields - OCIStmtExecute describe - ',Database.OCIStmtExecute(Database.mysvchp,mystmthp,myerrhp,0,0,nil,nil,OCI_DESCRIBE_ONLY));
 mypard:=nil;
 i:=0;
 while True do begin
  // get habdle if i-param (field) in query - mypard.
  // mypard helps us to get attributes of field such as data type, size, prescision, isnull and other.
  if Database.OCIParamGet(mystmthp,OCI_HTYPE_STMT,myerrhp,mypard,i+1)<>OCI_SUCCESS then break;
  TestError('LoadFields - OCIAttrGet(Column Name) - ',Database.OCIAttrGet(mypard,OCI_DTYPE_PARAM,@colname,@colnamelen,OCI_ATTR_NAME,myerrhp));
  Name:=strpas(colname);
  SetLength(Name,colnamelen);
  // immediately fetch attributes of current param (field)
  parSize:=sizeof(OraType);  TestError('LoadFields - OCIAttrGet(Column Type) - ',Database.OCIAttrGet(mypard,OCI_DTYPE_PARAM,@data,@parSize,OCI_ATTR_DATA_TYPE,myerrhp)); OraType:=ub2(data);
  parSize:=sizeof(OraSize);  TestError('LoadFields - OCIAttrGet(Column Size) - ',Database.OCIAttrGet(mypard,OCI_DTYPE_PARAM,@data,@parSize,OCI_ATTR_DATA_SIZE,myerrhp)); OraSize:=ub2(data);
  parSize:=sizeof(OraPrec);  TestError('LoadFields - OCIAttrGet(Column Prec) - ',Database.OCIAttrGet(mypard,OCI_DTYPE_PARAM,@data,@parSize,OCI_ATTR_PRECISION,myerrhp)); OraPrec:=ub1(data);
  parSize:=sizeof(OraScale); TestError('LoadFields - OCIAttrGet(Column Scale) - ',Database.OCIAttrGet(mypard,OCI_DTYPE_PARAM,@data,@parSize,OCI_ATTR_SCALE,myerrhp));    OraScale:=sb1(data);
  parSize:=sizeof(isnull);   TestError('LoadFields - OCIAttrGet(Column IsNull) - ',Database.OCIAttrGet(mypard,OCI_DTYPE_PARAM,@data,@parSize,OCI_ATTR_IS_NULL,myerrhp));  isnull:=ub1(data);
  case OraType of
   1,96:  begin DType:=ftoString; DSize:=OraSize; end;
   2:     begin
            if Database.Preferences.IntegerPrecision=0
              then RealIntPrec:=9 // by default all float values with precision <=9 treated as integers
              else RealIntPrec:=Database.Preferences.IntegerPrecision;

            if Database.Preferences.SmallIntPrecision=0
              then RealSmallIntPrec:=4 // by default all float values with precision <=4 treated as SmallInt
              else RealSmallIntPrec:=Database.Preferences.SmallIntPrecision;

            DSize:=0;
            if (OraScale=0) and (OraPrec>0) and (OraPrec<=RealIntPrec)
              then begin
                if OraPrec<=RealSmallIntPrec
                  then DType:=ftoSmallint
                  else DType:=ftoInteger;
              end
              else
                if (Database.Preferences.FloatPrecision>0) and (OraPrec>Database.Preferences.FloatPrecision)
                  then begin
                    DType:=ftoString;
                    DSize:=OraSize; //??? may be OraPrec+something ???
                  end
                  else DType:=ftoDouble;
          end;

   12:    begin DType:=ftoDate; DSize:=0; end;
   113:   begin DType:=ftoBlob; DSize:=0; end;
  else    begin DType:=ftoUnknown; DSize:=-1; end;
  end;
  AddField(Name,DType,DSize,isnull=0);
  i:=i+1;
 end;
finally
 if selfprepared then UnPrepare;
end;
end;

procedure TAOraSQL.AddParam(ParamName:string;FieldType:TAFieldType;ParamType:TAParamType);
var i:integer;
begin
 if ParamName = '' then ADatabaseError('Paramter name missing!', self);
 for i:=0 to ParamCount-1 do
  if AnsiCompareText(TAParam(ParamByIndex[i]).Name,ParamName)=0 then ADatabaseError(Format(SDuplicateName,[ParamName,self.name]));

 FParams.AddValue(TAOraParam.Create(ParamName,FieldType,ParamType));
end;

procedure TAOraSQL.SetQuery(Query:string);
begin
 if FPrepared then UnPrepare;
 Close;
 ClearFields;
 ClearParams;
 SQL.Text:=Query;
end;

procedure TAOraSQL.ClearParams;
var i:integer;
begin
 for i:=0 to FParams.Count-1 do TAOraParam(FParams[i]).Free;
 FParams.Clear;
end;




end.
