unit DynamicArrays;

{
 Dynamic arrays and hashes for storing various types of data.

 Arrays:
  THArray - Common array. Parent of all dynamic arrays.
  THArrayObjects,  THArrayByte,    THArraySmallInt, THArrayWord,  THArrayInt64,
  THArrayLongWord, THArrayInteger, THArrayPointer,  THArrayBoolean, THArrayDouble,
  THArrayCurrency, THArrayExtended,THArrayString,   THArrayStringFix.

 Hashes:
  THash - Common hash. Parent of all hashes.
  THashExists, THashBoolean, THashInteger, THashPointer,
  THashCurrency, THashDouble, THashString.

 Double Hashes:
  Like a table. Each value has two keys. Keys are always integer values.
  See DynamicArrays.txt for detail.
  THash2 - Common double hash. Parent of all double hashes.
  THash2Exists, THash2Currency, THash2Integer, THash2String.

}

interface

uses Classes, Windows;

resourcestring
 SItemNotFound = 'Element with index %d not found !';
 SKeyNotFound  = 'Element with index%d not found in Read-only hash !';

type
  pboolean  = ^boolean;
  ppointer  = ^pointer;

  THarray = class;

  {Compare callback function. Return values must be:
   0 - elements are equal
   1 - arr[i] > arr[j]
  -1 - arr[i] < arr[j] }
  TCompareProc = function(arr : THArray;i,j : integer) : integer of object;

  {Find callback function.
   FindData - pointer to the seaching data. Seaching data can be int, float, string and any other type.
   Return values must be.
   0 - arr[i] = FindData as <needed type>
   1 - arr[i] > FindData as <needed type>
  -1 - arr[i] < FindData as <needed type>
   See example application how to use TFindProc.
  }
  TFindProc = function(arr : THArray;i : integer; FindData:pointer):integer of object;
  TSwapProc = procedure(arr : THArray;i,j : integer) of object;


(***********************************************************)
(*  Arrays                                                 *)
(***********************************************************)

  THArray = class //common class of all dynamic arrays, does not depend on a type of stored data
  private
   FCount:integer;            // number of elements
   FCapacity:integer;         // number of elements on which memory is allocated
   FItemSize:integer;         // size of one element in bytes
   procedure SetItemSize(Size:integer);
  protected
   FValues:pointer;
   procedure Error(Value,min,max:integer);
   function CalcAddr(num:integer):pointer; virtual;
   procedure InternalQuickSort(CompareProc:TCompareProc;SwapProc:TSwapProc;L,R:integer);
  public
   constructor Create; virtual;
   destructor Destroy; override;
   procedure Clear;virtual;
   procedure ClearMem; virtual;
   function Add(pValue:pointer):integer; virtual;
   procedure AddMany(pValue:pointer;Count:integer);
   procedure AddFillValues(ACount:integer);
   procedure Delete(num:integer);virtual;
   procedure Hold;
   procedure Get(num:integer;pValue:pointer); virtual;
   function GetAddr(num:integer):pointer;
   procedure Grow;
   procedure GrowTo(Count:integer);
   function Insert(num:integer;pValue:pointer):integer; virtual;
   procedure InsertMany(num:integer;pValue:pointer;Count:integer);
   function IndexOf(Value:pointer):integer;
   function IndexOfFrom(Value:pointer;Start:integer):integer;
   procedure MoveData(FromPos,Count,Offset:integer);virtual;
   procedure SetCapacity(Value:integer);
   procedure Update(num:integer;pValue:pointer);virtual;
   procedure UpdateMany(num:integer;pValue:pointer;Count:integer);
   procedure Zero;
   procedure LoadFromStream(s:TStream);virtual; // readed values will be added to existing
   procedure SaveToStream(s:TStream);virtual;
   procedure Swap(Index1,Index2:integer);virtual;
   procedure Sort(CompareProc : TCompareProc);
   procedure QuickSort(CompareProc:TCompareProc;SwapProc:TSwapProc=nil);
   function QuickFind(FindProc:TFindProc;FindData:pointer):integer; // Find value in SORTED array!!
   property Capacity:integer read FCapacity;
   property Count:integer read FCount;
   property ItemSize:integer read FItemSize write SetItemSize;
   property Memory:pointer read FValues;
  end;

  THArrayObjects = class(THArray)
  protected
   function GetValue(Index:integer):TObject;
   procedure SetValue(Index:integer;const Value:TObject);
  public
   constructor Create; override;
   procedure ClearMem; override;              // (!) destroyes all saved objects! and deletes all references on them.
   procedure SafeClearMem;                    // deletes only references on all stored objects. Objects are leave safe
   procedure Delete(Index:integer); override; // (!) destroyes object with index Index and deletes reference on it.
   procedure SafeDelete(Index:integer);       // deletes only reference on object with index Index. Object is leaves safe.
   function AddValue(Value:TObject):integer;
   function IndexOf(Value:TObject):integer;
   function IndexOfFrom(Value:TObject;Start:integer):integer;
   property Value[Index:integer]:TObject read GetValue write SetValue; default;
  end;

  THArrayByte = class(THArray)
  protected
   function GetValue(Index:integer):byte;
   procedure SetValue(Index:integer;Value:byte);
  public
   constructor Create; override;
   function AddValue(Value:byte):integer;
   function IndexOf(Value:byte):integer;
   function IndexOfFrom(Value:byte;Start:integer):integer;
   property Value[Index:integer]:byte read GetValue write SetValue; default;
  end;

  THArraySmallInt = class(THArray)
  protected
   function GetValue(Index:integer):smallint;
   procedure SetValue(Index:integer;Value:smallint);
  public
   constructor Create; override;
   function AddValue(Value:smallint):integer;
   function IndexOf(Value:smallint):integer;
   function IndexOfFrom(Value:smallint;Start:integer):integer;
   property Value[Index:integer]:smallint read GetValue write SetValue; default;
  end;

  THArrayWord = class(THArray)
  protected
   function GetValue(Index:integer):word;
   procedure SetValue(Index:integer;Value:word);
  public
   constructor Create; override;
   function AddValue(Value:word):integer;
   function IndexOf(Value:word):integer;
   function IndexOfFrom(Value:word;Start:integer):integer;
   property Value[Index:integer]:word read GetValue write SetValue; default;
  end;

  THArrayInt64 = class(THArray)
  protected
   function GetValue(Index:integer):int64;
   procedure SetValue(Index:integer;Value:int64);
  public
   constructor Create; override;
   function AddValue(Value:int64):integer;
   function IndexOf(Value:int64):integer;
   function IndexOfFrom(Value:int64;Start:integer):integer;
   property Value[Index:integer]:int64 read GetValue write SetValue; default;
  end;

  THArrayLongWord = class(THArray)
  protected
   function GetValue(Index:integer):LongWord;
   procedure SetValue(Index:integer;Value:LongWord);
  public
   constructor Create; override;
   function AddValue(Value:LongWord):integer;
   function IndexOf(Value:LongWord):integer;
   function IndexOfFrom(Value:LongWord;Start:integer):integer;
   property Value[Index:integer]:LongWord read GetValue write SetValue; default;
  end;

  THArrayInteger = class(THArray)
  protected
   function GetValue(Index:integer):integer;
   procedure SetValue(Index:integer;Value:Integer);
  public
   constructor Create; override;
   function IndexOf(Value:integer):integer;
   function IndexOfFrom(Value:integer;Start:integer):integer;
   function AddValue(Value:integer):integer;
   function Pop:integer;
   procedure Push(Value:integer);
   property Value[Index:integer]:integer read GetValue write SetValue; default;
   function GetAsString:string;
   procedure AddFromString(InputString,Delimiters:string);
   function CalcMax:integer;
//   procedure QuickSort(l,r:integer);overload;
  end;

  THArrayPointer = class(THArray)
  protected
   function GetValue(Index:integer):Pointer;
   procedure SetValue(Index:integer;Value:Pointer);
  public
   constructor Create; override;
   function IndexOf(Value:pointer):integer;
   function IndexOfFrom(Value:pointer;Start:integer):integer;
   function AddValue(Value:pointer):integer;
   property Value[Index:integer]:pointer read GetValue write SetValue; default;
  end;

  THArrayBoolean = class(THArray)
  protected
   function GetValue(Index:integer):Boolean;
   procedure SetValue(Index:integer;Value:Boolean);
  public
   constructor Create; override;
   function AddValue(Value:Boolean):integer;
   function IndexOf(Value:Boolean):integer;
   function IndexOfFrom(Value:Boolean;Start:integer):integer;
   property Value[Index:integer]:Boolean read GetValue write SetValue; default;
  end;

  THArrayDouble = class(THArray)
  protected
   function GetValue(Index:integer):Double;
   procedure SetValue(Index:integer;Value:Double);
  public
   constructor Create; override;
   function AddValue(Value:double):integer;
   function IndexOf(Value:double):integer;
   function IndexOfFrom(Value:double;Start:integer):integer;
   property Value[Index:integer]:double read GetValue write SetValue; default;
  end;

  THArrayCurrency = class(THArray)
  protected
   function GetValue(Index:integer):Currency;
   procedure SetValue(Index:integer;Value:Currency);
  public
   constructor Create; override;
   function AddValue(Value:currency):integer;
   function IndexOf(Value:currency):integer;
   function IndexOfFrom(Value:currency;Start:integer):integer;
   property Value[Index:integer]:currency read GetValue write SetValue; default;
  end;

  THArrayExtended = class(THArray)
  protected
   function GetValue(Index:integer):Extended;
   procedure SetValue(Index:integer;Value:Extended);
  public
   constructor Create; override;
   function AddValue(Value:Extended):integer;
   function IndexOf(Value:Extended):integer;
   function IndexOfFrom(Value:Extended;Start:integer):integer;
   property Value[Index:integer]:Extended read GetValue write SetValue; default;
  end;

  TWideString = class
   Str:WideString;
  public
   constructor Create(Value:WideString);
  end;

  THArrayWideStrings = class(THArrayObjects)
  protected
   function GetValue(Index:integer):WideString;
   procedure SetValue(Index:integer;Value:WideString);
  public
   function AddValue(Value:WideString):integer;
   function IndexOf(Value:WideString):integer;
   function IndexOfFrom(Value:WideString;Start:integer):integer;
   property Value[Index:integer]:WideString read GetValue write SetValue; default;
  end;

  THArrayString = class(THArray)
  private
   str_ptr:THArrayPointer;
  protected
   function GetValue(Index:integer):string;
   procedure SetValue(Index:integer;Value:string);
   function CalcAddr(num:integer):pointer; override;
  public
   constructor Create; override;
   destructor Destroy; override;
   function AddValue(Value:string):integer;
   function Add(pValue:pointer):integer; override;
   procedure Clear;override;
   procedure ClearMem;override;
   procedure Delete(num:integer);override;
   procedure Get(num:integer;pValue:pointer); override;
   function Insert(num:integer;pValue:pointer):integer; override;
   function IndexOf(Value:string):integer;
   function IndexOfFrom(Value:string;Start:integer):integer;
   procedure MoveData(FromPos,Count,Offset:integer); override;
   procedure Swap(Index1,Index2:integer);override;
   procedure Update(num:integer;pValue:pointer);override;
   property Value[Index:integer]:string read GetValue write SetValue; default;
  end;

{  THArrayString_ = class(THArrayPointer)
  private
   procedure ClearStrings;
   function DublicateStr(pValue:pointer):PChar;
  protected
   function GetValue(Index:integer):string;
   procedure SetValue(Index:integer;Value:string);
  public
   destructor Destroy; override;
   procedure Clear;override;
   procedure ClearMem;override;
   function Add(pValue:pointer):integer;override;
   function AddValue(Value:string):integer;
   procedure Delete(num:integer);override;
   function Insert(num:integer;Value:string):integer;overload;
   function Insert(num:integer;pValue:pointer):integer;overload;override;
   procedure Update(num:integer;pValue:pointer);override;
   function IndexOf(Value:string):integer;
   function IndexOfFrom(Value:string;Start:integer):integer;
   procedure LoadFromStream(s:TStream);virtual; // readed values will be added to existing
   procedure SaveToStream(s:TStream);virtual;

   property Value[Index:integer]:string read GetValue write SetValue; default;
  end;}

  THArrayStringFix = class(THArray)
  protected
   function GetValue(Index:integer):string;
   procedure SetValue(Index:integer;Value:string);
  public
   constructor Create; override;
   constructor CreateSize(Size:integer);
   function AddValue(Value:string):integer;
   function IndexOf(Value:string):integer;
   function IndexOfFrom(Value:string;Start:integer):integer;
   property Value[Index:integer]:string read GetValue write SetValue; default;
  end;

(***********************************************************)
(*  Hashes                                                 *)
(***********************************************************)

  THash = class
  private
   FReadOnly:boolean;
   FAIndex:THArrayInteger;
   function GetKey(Index:integer):integer;
   function GetCount:integer;
  public
   constructor Create; virtual;
   destructor Destroy; override;
   procedure Clear; virtual;
   procedure ClearMem; virtual;
   function IfExist(Key:integer):boolean;  // check if values with key Key is exists in hash
   procedure Delete(Key:integer); virtual; abstract;// deletes value with key=Key
   property Count:integer read GetCount;   property Keys[Index:integer]:integer read GetKey;
   property AIndexes:THArrayInteger read FAIndex;
  end;

  THashExists = class (THash)
  private
   procedure SetValue(Index:integer;Value:boolean);
   function GetValue(Index:integer):boolean;
  public
   constructor Create; override;
   destructor Destroy; override;
   procedure Delete(Key:integer); override;
   property Value[Index:integer]:boolean read GetValue write SetValue; default;
  end;

  THashBoolean = class (THash)
  private
   FAValues:THArrayBoolean;
   procedure SetValue(Key:integer;Value:boolean);
   function GetValue(Key:integer):boolean;
  public
   constructor Create; override;
   constructor CreateFromHArrays(IndexHArray:THArrayInteger;ValueHArray:THArrayBoolean);
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(Key:integer); override;
   property Value[Index:integer]:boolean read GetValue write SetValue; default;
  end;

  THashInteger = class (THash)
  private
   FAValues:THArrayInteger;
   procedure SetValue(Key:integer;Value:integer);
   function GetValue(Key:integer):integer;
  public
   constructor Create; override;
   constructor CreateFromHArrays(IndexHArray:THArrayInteger;ValueHArray:THArrayInteger);
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(Key:integer); override;
   property AValues:THArrayInteger read FAValues;
   property Value[Index:integer]:integer read GetValue write SetValue; default;
  end;

  THashPointer = class (THash)
  private
   FAValues:THArrayPointer;
   procedure SetValue(Key:integer;Value:pointer);
   function GetValue(Key:integer):pointer;
  public
   constructor Create; override;
   constructor CreateFromHArrays(IndexHArray:THArrayInteger;ValueHArray:THArrayPointer);
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(Key:integer); override;
   property AValues:THArrayPointer read FAValues;
   property Value[Index:integer]:pointer read GetValue write SetValue; default;
  end;

  THashCurrency = class (THash)
  private
   FAValues:THArrayCurrency;
   procedure SetValue(Key:integer;Value:currency);
   function GetValue(Key:integer):currency;
  public
   constructor Create; override;
   constructor CreateFromHArrays(IndexHArray:THArrayInteger;ValueHArray:THArrayCurrency);
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(Key:integer); override;
   procedure Inc(Key:integer;Value:currency); // increases stored value with key=Key on a Value. If value with key=Key does not exists then it will be created with value=Value.
   property Value[Index:integer]:currency read GetValue write SetValue; default;
  end;

  THashDouble = class (THash)
  private
   FAValues:THArrayDouble;
   procedure SetValue(Key:integer;Value:Double);
   function GetValue(Key:integer):Double;
  public
   constructor Create; override;
   constructor CreateFromHArrays(IndexHArray:THArrayInteger;ValueHArray:THArrayDouble);
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(Key:integer); override;
   procedure Inc(Key:integer;Value:Double); // increases stored value with key=Key on a Value. If value with key=Key does not exists then it will be created with value=Value.
   property Value[Index:integer]:Double read GetValue write SetValue; default;
  end;

  THashString = class (THash)
  private
   FAllowEmptyStr:boolean;
   FAValues:TStrings;
   procedure SetValue(Key:integer;Value:string);
   function GetValue(Key:integer):string;
  public
   constructor Create; override;
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(Key:integer); override;
   property AllowEmptyStr:boolean read FAllowEmptyStr write FAllowEmptyStr;
   property Value[Index:integer]:string read GetValue write SetValue; default;
  end;

  THash2 = class
  private
   MainListIndex:THArrayInteger;
   MainListValue:THArrayPointer;
//   function GetKey(Index:integer):integer;
   function GetChildHash(Key:integer):THash;
  public
   constructor Create; virtual;
   destructor Destroy; override;
//   function Count:integer;
   procedure Clear; virtual; abstract;  // Creares hash. Allocated memory do not frees.
   procedure ClearMem;                  // Cleares hash. Allocated memory frees too.
   procedure Delete(MainIndex,Index:integer);
//   function ExistMainHash(MainIndex:integer):boolean;
//   function ExistIndex(Index:integer):boolean;
//   property Keys[Index:integer]:integer read GetKey;
   property MainIndexes:THArrayInteger read MainListIndex;
  end;

  THash2Exists = class (THash2)
  public
   procedure SetValue(MainIndex,Index:integer;Value:boolean); // creates new record with keys MainIndex, Index
   procedure Clear; override;
   function GetValue(MainIndex,Index:integer):boolean;        // Gets Value by keys MainIndex, Index
   function CreateMainHash(MainIndex:integer):THashExists;
   function CreateHash(Index:integer):THashExists;
//   procedure ExportChildHash(Hash:THashBoolean);
//   procedure DeleteMainIndex(MainIndex:integer);
//   procedure DeleteIndex(Index:integer);
  end;

  THash2Currency = class (THash2)
  public
   procedure SetValue(MainIndex,Index:integer;Value:currency);// creates new record with keys MainIndex, Index
   procedure Inc(MainIndex,Index:integer;Value:currency);     // increases exists/create new record with keys MainIndex, Index
   procedure Clear; override;
   function GetValue(MainIndex,Index:integer):currency;       // Gets Value by keys MainIndex, Index
   function CreateMainHash(MainIndex:integer):THashCurrency;
   function CreateHash(Index:integer):THashCurrency;
//   procedure ExportChildHash(Hash:THashCurrency);
  end;

  THash2Integer = class (THash2)
  public
   procedure SetValue(MainIndex,Index:integer;Value:Integer); // creates new record with keys MainIndex, Index
   procedure Clear; override;
   function GetValue(MainIndex,Index:integer):Integer;        // Gets Value by keys MainIndex, Index
   function CreateMainHash(MainIndex:integer):THashInteger;
   function CreateHash(Index:integer):THashInteger;
//   procedure ExportChildHash(Hash:THashInteger);
  end;

  THash2String = class (THash2)
  protected
   procedure SetValue(MainIndex,Index:integer;Value:String); // creates new record with keys MainIndex, Index
   function GetValue(MainIndex,Index:integer):String;        // Gets Value by keys MainIndex, Index
  public
   procedure Clear; override;
   function CreateMainHash(MainIndex:integer):THashString;
   function CreateHash(Index:integer):THashString;
//   procedure ExportChildHash(Hash:THashCurrency);
   property Value[MainIndex,Index:integer]:string read GetValue write SetValue; default;
  end;

procedure memcpy(pi,po:pointer;Count:integer); stdcall;
procedure memclr(po:pointer;Count:integer); stdcall;
procedure memset(po:pointer;Value:byte;Count:integer); stdcall;

function memfinddword(pi:pointer;Value:dword;Count:integer):integer; stdcall;
function memfindbyte(pi:pointer;Value:byte;Count:integer):integer; stdcall;
function memfindword(pi:pointer;Value:word;Count:integer):integer; stdcall;
function memfindint64(pi:pointer;Value:int64;Count:integer):integer; stdcall;
function memfindgeneral(pi,pValue:pointer;ValueSize:integer;Count:integer):integer; stdcall;

implementation

uses SysUtils;

const
 BLOCK=1024;

function HGetToken(InputString:string; Delimiters:string; OnlyOneDelimiter:boolean; Index:integer):string;
var i,p:integer;
begin
 Result:='';
 p:=1;
 while (p<=length(InputString)) and (pos(InputString[p],Delimiters)<>0) do
  inc(p);
 for i:=1 to index do begin
  while (p<=length(InputString)) and (pos(InputString[p],Delimiters)=0)
   do inc(p);

  if OnlyOneDelimiter
   then  inc(p)
   else  while (p<=length(InputString)) and (pos(InputString[p],Delimiters)<>0) do inc(p);
 end;
 while (p<=length(InputString)) and (pos(InputString[p],Delimiters)=0)
  do begin Result:=Result+InputString[p]; inc(p); end;
end;

function HGetTokenCount(InputString:string; Delimiters:string; OnlyOneDelimiter:boolean):integer;
var p:integer;
begin
 Result:=0;
 if InputString='' then exit;
 p:=1;
 while (p<=length(InputString)) and (pos(InputString[p],Delimiters)<>0) do
  inc(p);
 while (p<=length(InputString)) do begin
  while (p<=length(InputString)) and (pos(InputString[p],Delimiters)=0)
    do inc(p);

  if OnlyOneDelimiter
   then  inc(p)
   else  while (p<=length(InputString)) and (pos(InputString[p],Delimiters)<>0) do inc(p);
  Result:=Result+1;
 end;
 Result:=Result;
end;

procedure memcpy(pi,po:pointer;Count:integer); stdcall;
begin
 if ((dword(pi)+dword(Count))>dword(po)) and (dword(pi)<dword(po)) then // copy from end
 asm
  pushad
  pushfd
  mov ECX,Count
  mov EDI,po
  mov ESI,pi
  add ESI,ECX
  add EDI,ECX
  dec ESI
  dec EDI
  std
  repne MOVSB
  popfd
  popad
 end else // copying from begin
 asm
  pushad
  pushfd
  mov ECX,Count
  mov EDI,po
  mov ESI,pi
  cld
  repne MOVSB
  popfd
  popad
 end;
end;

procedure memclr(po:pointer;Count:integer); stdcall;
//begin
 asm
  pushad
  pushfd
  mov ECX,Count
  mov EDI,po
  xor AL,AL
  cld
  repne STOSB
  popfd
  popad
 end;
//end;

procedure memset(po:pointer;Value:byte;Count:integer); stdcall;
//begin
 asm
  pushad
  pushfd
  mov ECX,Count
  mov EDI,po
  mov AL,Value
  cld
  repne STOSB
  popfd
  popad
 end;
//end;

function memfinddword(pi:pointer;Value:dword;Count:integer):integer; stdcall;
//label ex;
//begin
 asm
  pushad
  pushfd
  mov Result,0
  mov ECX,Count
  cmp ECX,0
  jz @ex
  mov EAX,Value
  mov EDI,pi
  cld
  repne SCASD
  jne @ex
  mov EAX,Count
  sub EAX,ECX
  mov Result,EAX
@ex:
  dec Result
  popfd
  popad
 end;
//end;

function memfindbyte(pi:pointer;Value:byte;Count:integer):integer; stdcall;
//label ex;
//begin
 asm
  pushad
  pushfd
  mov @Result,0
  mov ECX,Count
  cmp ECX,0
  jz @ex
  mov AL,Value
  mov EDI,pi
  cld
  repne SCASB
  jne @ex
  mov EAX,Count
  sub EAX,ECX
  mov @Result,EAX
@ex:
  dec @Result
  popfd
  popad
 end;
//end;

function memfindword(pi:pointer;Value:word;Count:integer):integer; stdcall;
//label ex;
//begin
 asm
  pushad
  pushfd
  mov @Result,0
  mov ECX,Count
  cmp ECX,0
  jz @ex
  mov AX,Value
  mov EDI,pi
  cld
  repne SCASW
  jne @ex
  mov EAX,Count
  sub EAX,ECX
  mov @Result,EAX
@ex:
  dec @Result
  popfd
  popad
 end;
//end;

function memfindint64(pi:pointer;Value:int64;Count:integer):integer; stdcall;
asm
  pushad
  pushfd
  mov @Result,0
  mov ECX,Count
  cmp ECX,0
  jz @ex
  mov EAX,dword ptr Value
  mov EBX,dword ptr (Value+4)
  mov EDI,pi
@loop:
  cmp EAX,[EDI]
  je @found1
  dec ECX
  jz @ex
  add EDI,8     // go to next int 64 value
  jmp @loop
@found1:
  add EDI,4     // go to next half of current int64 value
  cmp EBX,[EDI]
  je @found2
  dec ECX
  jz @ex
  add EDI,4
  jmp @loop
@found2:
  mov EAX,Count
  sub EAX,ECX
  mov @Result,EAX
@ex:
  dec @Result
  popfd
  popad
end;

function memfindgeneral(
  pi:pointer;        // start address for finding
  pValue:pointer;    // pointer to the finding value
  ValueSize:integer; // the size of finding value in bytes
  Count:integer      // number of values in array
  ):integer; stdcall;
 asm
  pushad
  pushfd
  mov @Result,0
  mov EBX,Count
  cmp EBX,0
  jz @ex
  mov EDI,pi
@loop:
  mov ESI,pValue
  mov ECX,ValueSize;
  cld
  repe CMPSB
  jz @ex1
  add EDI,ECX       
  dec EBX
  jnz @loop
  jmp @ex
@ex1:
  dec EBX
  mov EAX,Count
  sub EAX,EBX
  mov @Result,EAX
@ex:
  dec @Result
  popfd
  popad
 end;

 { THArray }

constructor THArray.Create;
begin
 inherited Create;

 FCount:=0;
 FCapacity:=0;
 FItemSize:=1;
 FValues:=nil;
end;

destructor THArray.Destroy;
begin
 ClearMem;
 FItemSize:=0;
 inherited Destroy;
end;

procedure THArray.Delete(num:integer);
begin
 if num>=FCount then raise ERangeError.Create(Format(SItemNotFound,[num]));
 if num<(FCount-1) then memcpy(GetAddr(num+1),GetAddr(num),(FCount-num-1)*FItemSize);
 Dec(FCount);
end;

procedure THArray.Clear;
begin
 FCount:=0;
end;

procedure THArray.ClearMem;
begin
 FCount:=0;
 FCapacity:=0;
 FreeMem(FValues);
 FValues:=nil;
end;

function THArray.Add(pValue:pointer):integer;
begin
 Result:=Insert(FCount,pValue);
end;

procedure THArray.AddMany(pValue:pointer;Count:integer);
begin
 if Count<=0 then exit;
 InsertMany(FCount,pValue,Count);
end;

procedure THarray.Hold;
// frees unused memory
begin
 SetCapacity(FCount);
end;

procedure THArray.SetCapacity(Value:integer);
begin
  ReAllocMem(FValues,Value*FItemSize);
  FCapacity:=Value;
  if FCount>FCapacity then FCount:=FCapacity;
end;

procedure THArray.AddFillValues(ACount:integer);
begin
 if Count+ACount>Capacity then GrowTo(Count+ACount);
 memclr(CalcAddr(FCount),ACount*ItemSize);
 FCount:=FCount+ACount;
end;

procedure THArray.Zero;
begin
 if FCount=0 then exit;
 memclr(Memory,FCount*ItemSize);
end;

procedure THArray.Grow;
// allocates memory for more number of elements by the next rules
//     the size of allocated memory increases on 25% if array has more than 64 elements
//     the size of allocated memory increases on 16 elements if array has from 8 to 64 elements
//     the size of allocated memory increases on 4 elements if array has less than 8 elements
var Delta:integer;
begin
 if FCapacity > 64 then Delta := FCapacity div 4 else
   if FCapacity > 8 then Delta := 16 else Delta := 4;
 SetCapacity(FCapacity + Delta);
end;

procedure THArray.GrowTo(Count:integer);
// increases size of allocated memory till Count elements (if count enough large) or
// to a number as described in Grow procedure
var Delta:integer;
begin
 if Count<=FCapacity then exit;

 if FCapacity > 64 then Delta := FCapacity div 4 else
   if FCapacity > 8 then Delta := 16 else Delta := 4;
 if (FCapacity+Delta)<Count then Delta:=Count-FCapacity;
 SetCapacity(FCapacity + Delta);
end;

function THArray.Insert(num:integer;pValue:pointer):integer;
begin
 Error(num,0,FCount);
 inc(FCount);
 if FCount>=FCapacity then Grow;

 memcpy(CalcAddr(num),CalcAddr(num+1),(FCount-num-1)*FItemSize); // make place to insert
 Update(num,pValue);
 Result:=num;
end;

procedure THArray.InsertMany(num:integer;pValue:pointer;Count:integer);
begin
 Error(num,0,FCount);
 if FCount+Count>FCapacity then GrowTo(FCount+Count);

 FCount:=FCount+Count;
 memcpy(CalcAddr(num),CalcAddr(num+Count),(FCount-num-Count)*FItemSize);
 UpdateMany(num,pValue,Count);
end;

procedure THArray.Update(num:integer;pValue:pointer);
begin
 if pValue=nil
  then memclr(GetAddr(num),FItemSize)
  else memcpy(pValue,GetAddr(num),FItemSize);
end;

procedure THArray.UpdateMany(num:integer;pValue:pointer;Count:integer);
begin
 Error(num+Count,0,FCount);
 memcpy(pValue,GetAddr(num),FItemSize*Count);
end;

procedure THArray.Get(num:integer;pValue:pointer);
begin
 memcpy(GetAddr(num),pValue,FItemSize);
end;

function THArray.GetAddr(num:integer):pointer;
begin
 Error(num,0,FCount-1);
 Result:=CalcAddr(num);
end;

function THArray.CalcAddr(num:integer):pointer;
begin
 Result:=pointer(dword(FValues)+dword(num)*dword(FItemSize));
end;

procedure THArray.Error(Value,min,max:integer);
begin
  if (Value<min) or (Value>max) then raise ERangeError.Create(Format(SItemNotFound,[Value]));
end;

procedure THArray.SetItemSize(Size:integer);
begin
 ClearMem;
 if (FCount=0) and (Size>0) then FItemSize:=Size;
end;

procedure THArray.MoveData(FromPos,Count,Offset:integer);
var mem:pointer;
begin
 Error(FromPos,0,FCount-1);
 Error(FromPos+Count,0,FCount);
 Error(FromPos+Offset,0,FCount-1);
 Error(FromPos+Offset+Count,0,FCount);
 mem:=AllocMem(Count*FItemSize);
 try
  memcpy(CalcAddr(FromPos),mem,Count*FItemSize);
  if Offset<0 then memcpy(CalcAddr(FromPos+Offset),CalcAddr(FromPos+Offset+Count),(-Offset)*FItemSize);
  if Offset>0 then memcpy(CalcAddr(FromPos+Count),CalcAddr(FromPos),Offset*FItemSize);
  memcpy(mem,CalcAddr(FromPos+Offset),Count*FItemSize);
 finally
  FreeMem(mem);
 end; 
end;

procedure THArray.Sort(CompareProc : TCompareProc);
var
  maxEl : integer;
  i,j   : integer;
begin
  if Count<2 then exit;
  if @CompareProc=nil then exit;

  for i:=0 to Count-2 do
   begin
    maxEl:=i;
    for j:=i+1 to Count-1 do
      if CompareProc(self,maxEl,j)<0 then maxEl:=j;
    if maxEl<>i then
     begin
      Swap(i,maxEl);
//      MoveData(i,1,maxEl-i);
//      MoveData(maxEl-1,1,i-maxEl+1);
    end;
  end;
end;

procedure THArray.Swap(Index1, Index2: integer);
var p:pointer;
begin
 p:=AllocMem(FItemSize);
 try
  memcpy(GetAddr(Index1),p,FItemSize);
  memcpy(GetAddr(Index2),GetAddr(Index1),FItemSize);
  memcpy(p,GetAddr(Index2),FItemSize);
 finally
  FreeMem(p);
 end; 
end;

procedure THArray.QuickSort(CompareProc: TCompareProc; SwapProc: TSwapProc);
begin
 InternalQuickSort(CompareProc,SwapProc,0,Count-1);
end;

procedure THArray.InternalQuickSort(CompareProc: TCompareProc;
  SwapProc: TSwapProc; L, R: integer);
var
  I,J: Integer;
  P: Integer;
begin
  if @CompareProc=nil then exit;
{  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while CompareProc(self,I,P) < 0 do Inc(I);
      while CompareProc(self,J,P) > 0 do Dec(J);
      if I <= J then
      begin
        SwapProc(self,I,J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
     InternalQuickSort(CompareProc,SwapProc, L, J);
    L := I;
  until I >= R;}

    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while ((CompareProc(self,I,P) < 0){and(I<=J)}) do Inc(I);
      while ((CompareProc(self,J,P) > 0){and(I<=J)}) do Dec(J);
      if I <= J then
      begin
        if I=P then P:=J
         else if J=P then P:=I;
        if @SwapProc=nil
         then Swap(I,J)
         else SwapProc(self,I,J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then InternalQuickSort(CompareProc,SwapProc, L, J);
    if I < R then InternalQuickSort(CompareProc,SwapProc, I, R);
end;

function THArray.QuickFind(FindProc:TFindProc;FindData:pointer):integer;
label fin;
var L,R,res:integer;
    was1:boolean;
begin
 Result:=-1;
 if Count=0 then exit;
 if @FindProc=nil then exit;

 L:=0; R:=Count-1;
 if FindProc(self,R,FindData)<0 then begin
  Result:=-1;//R;
  exit;
 end;
 while True do begin
  was1:=abs(R-L)=1;
  Result:=(L+R) shr 1;
  if L=Result then goto fin;//exit;
  res:=FindProc(self,Result,FindData);
  if res<0 then L:=Result
  else if res>0 then R:=result
  else goto fin;//exit;
  if was1 then goto fin;//exit;
 end;
fin:

end;

procedure THArray.LoadFromStream(s: TStream);
var i,oc:integer;
begin
 s.Read(i,sizeof(i));
 oc:=FCount;
 AddFillValues(i);
 s.Read(CalcAddr(oc)^,i*FItemSize);
end;

procedure THArray.SaveToStream(s: TStream);
begin
 s.Write(Count,sizeof(integer));
 s.Write(PChar(FValues)^,Count*FItemSize);
end;

function THArray.IndexOf(Value: pointer): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArray.IndexOfFrom(Value: pointer; Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if FValues<>nil then begin
   Result:=memfindgeneral(GetAddr(Start),Value,FItemSize,Count-Start);
   if Result<>-1 then Result:=Result+Start;
 end;
end;

{ THArrayObjects }

function THArrayObjects.AddValue(Value: TObject): integer;
begin
 Result:=inherited Add(@Value);
end;

procedure THArrayObjects.ClearMem;
var i:integer;
begin
 for i:=0 to Count-1 do GetValue(i).Free;
 inherited;
end;

procedure THArrayObjects.SafeClearMem;
begin
 inherited ClearMem;
end;

constructor THArrayObjects.Create;
begin
 inherited;
 FItemSize:=sizeof(TObject);
end;

procedure THArrayObjects.Delete(Index: integer);
var o:TObject;
begin
 o:=GetValue(Index);
 inherited;
 if Assigned(o) then o.Free;
end;

procedure THArrayObjects.SafeDelete(Index: integer);
begin
 inherited Delete(Index);
end;

function THArrayObjects.GetValue(Index: integer): TObject;
begin
 Result:=TObject(GetAddr(Index)^);
end;

procedure THArrayObjects.SetValue(Index: integer;const Value: TObject);
begin
 Update(Index,@Value);
end;

function THArrayObjects.IndexOf(Value: TObject): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayObjects.IndexOfFrom(Value: TObject;
  Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if FValues<>nil then begin
   Result:=memfinddword(GetAddr(Start),dword(Value),Count-Start);
   if Result<>-1 then Result:=Result+Start;
 end;
end;

{ THArrayByte }

function THArrayByte.AddValue(Value: byte): integer;
begin
 Result:=inherited Add(@Value);
end;

constructor THArrayByte.Create;
begin
 inherited Create;
 FItemSize:=sizeof(byte);
end;

function THArrayByte.GetValue(Index: integer): byte;
begin
 Result:=pbyte(GetAddr(Index))^;
end;

function THArrayByte.IndexOf(Value: byte): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayByte.IndexOfFrom(Value: byte; Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if FValues<>nil then begin
   Result:=memfindbyte(GetAddr(Start),Value,Count-Start);
   if Result<>-1 then Result:=Result+Start;
 end;
end;

procedure THArrayByte.SetValue(Index: integer; Value: byte);
begin
 Update(Index,@Value);
end;

{ THArraySmallInt }

constructor THArraySmallInt.Create;
begin
 inherited Create;
 FItemSize:=sizeof(smallint);
end;

function THArraySmallInt.AddValue(Value:smallint):integer;
begin
 Result:=inherited Add(@Value);
end;

function THArraySmallInt.GetValue(Index:integer):smallint;
begin
 Result:=psmallint(GetAddr(Index))^;
end;

procedure THArraySmallInt.SetValue(Index:integer;Value:smallint);
begin
 Update(Index,@Value);
end;

 function THArraySmallInt.IndexOf(Value: smallint): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArraySmallInt.IndexOfFrom(Value: smallint;
  Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if FValues<>nil then begin
   Result:=memfindword(GetAddr(Start),word(Value),Count-Start);
   if Result<>-1 then Result:=Result+Start;
 end;
end;

{ THArrayWord }

constructor THArrayWord.Create;
begin
 inherited Create;
 FItemSize:=sizeof(Word);
end;

function THArrayWord.AddValue(Value:Word):integer;
begin
 Result:=inherited Add(@Value);
end;

function THArrayWord.GetValue(Index:integer):Word;
begin
 Result:=pword(GetAddr(Index))^;
end;

procedure THArrayWord.SetValue(Index:integer;Value:Word);
begin
 Update(Index,@Value);
end;

 function THArrayWord.IndexOf(Value: word): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayWord.IndexOfFrom(Value: word; Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if FValues<>nil then begin
   Result:=memfindword(GetAddr(Start),Value,Count-Start);
   if Result<>-1 then Result:=Result+Start;
 end;
end;

{ THArrayLongWord }

constructor THArrayLongWord.Create;
begin
 inherited Create;
 FItemSize:=sizeof(LongWord);
end;

function THArrayLongWord.AddValue(Value:LongWord):integer;
begin
 Result:=inherited Add(@Value);
end;

function THArrayLongWord.GetValue(Index:integer):LongWord;
begin
 Result:=pLongWord(GetAddr(Index))^;
end;

procedure THArrayLongWord.SetValue(Index:integer;Value:LongWord);
begin
 Update(Index,@Value);
end;

function THArrayLongWord.IndexOf(Value: LongWord): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayLongWord.IndexOfFrom(Value: LongWord; Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if FValues<>nil then begin
   Result:=memfinddword(GetAddr(Start),dword(Value),Count-Start);
   if Result<>-1 then Result:=Result+Start;
 end;
end;

 { THArrayInt64 }

constructor THArrayInt64.Create;
begin
 inherited Create;
 FItemSize:=sizeof(Int64);
end;

function THArrayInt64.AddValue(Value:Int64):integer;
begin
 Result:=inherited Add(@Value);
end;

function THArrayInt64.GetValue(Index:integer):Int64;
begin
 Result:=pint64(GetAddr(Index))^;
end;

procedure THArrayInt64.SetValue(Index:integer;Value:Int64);
begin
 Update(Index,@Value);
end;

 function THArrayInt64.IndexOf(Value: int64): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayInt64.IndexOfFrom(Value: int64; Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if FValues<>nil then begin
   Result:=memfindint64(GetAddr(Start),Value,Count-Start);
   if Result<>-1 then Result:=Result+Start;
 end;
end;

{ THArrayInteger }

constructor THArrayInteger.Create;
begin
 inherited Create;
 FItemSize:=sizeof(integer);
end;

function THArrayInteger.AddValue(Value:integer):integer;
begin
 Result:=inherited Add(@Value);
end;

function THArrayInteger.IndexOf(Value:integer):integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayInteger.IndexOfFrom(Value:integer;Start:integer):integer;
begin
 if Start=Count then begin
  Result:=-1;
  exit;
 end;
 Error(Start,0,Count-1);
 if FValues=nil
  then Result:=-1
  else begin
   Result:=memfinddword(GetAddr(Start),dword(Value),Count-Start);
   if Result<>-1 then Result:=Result+Start;
  end;
end;

function THArrayInteger.GetValue(Index:integer):integer;
begin
 Result:=pinteger(GetAddr(Index))^;
end;

procedure THArrayInteger.SetValue(Index:integer;Value:Integer);
begin
 Update(Index,@Value);
end;

procedure THArrayInteger.Push(Value:Integer);
begin
 AddValue(Value);
end;

function THArrayInteger.Pop:integer;
begin
 Result:=Value[Count-1];
 Delete(Count-1);
end;

procedure THArrayInteger.AddFromString(InputString,Delimiters:string);
var i,c:integer;
begin
 c:=HGetTokenCount(InputString,Delimiters,False);
 for i:=0 to c-1 do
  AddValue(StrToInt(HGetToken(InputString,Delimiters,False,i)));
end;

function THArrayInteger.GetAsString:string;
var i:integer;
begin
 Result:=' ';
 for i:=0 to Count-1 do
  Result:=Result+IntToStr(Value[i])+' ';
end;

function THArrayInteger.CalcMax: integer;
var i:integer;
begin
 if Count=0 then begin Result:=-1; exit; end;
 Result:=Value[0];
 for i:=1 to Count-1 do
  if Value[i]>Result then Result:=Value[i];
end;

{procedure THArrayInteger.QuickSort(L,R:integer);
 var
  I,J,P,temp: integer;
begin
  I:=L;
  J:=R;
  p:=(L+R) shr 1;
  repeat
    while Value[I]<Value[P] do Inc(I);
    while Value[J]>Value[P] do Dec(J);
    if I <= J then
    begin
      temp:=Value[I];
      Value[I]:=Value[J];
      Value[I]:=temp;
      Inc(I);
      Dec(J);
    end;
  until I > J;
  if L<J then QuickSort(L,J);
  if I<R then QuickSort(I,R);
end;}

{ THArrayPointer }

constructor THArrayPointer.Create;
begin
 inherited Create;
 FItemSize:=sizeof(pointer);
end;

function THArrayPointer.AddValue(Value:pointer):integer;
begin
 Result:=inherited Add(@Value);
end;

function THArrayPointer.IndexOf(Value:pointer):integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayPointer.IndexOfFrom(Value:pointer;Start:integer):integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if FValues<>nil then begin
   Result:=memfinddword(GetAddr(Start),dword(Value),Count-Start);
   if Result<>-1 then Result:=Result+Start;
 end;
end;

function THArrayPointer.GetValue(Index:integer):Pointer;
begin
 Result:=ppointer(GetAddr(Index))^;
end;

procedure THArrayPointer.SetValue(Index:integer;Value:Pointer);
begin
 Update(Index,@Value);
end;

 { THArrayBoolean }

constructor THArrayBoolean.Create;
begin
 inherited Create;
 FItemSize:=sizeof(boolean);
end;

function THArrayBoolean.AddValue(Value:boolean):integer;
begin
 Result:=inherited Add(@Value);
end;

function THArrayBoolean.GetValue(Index:integer):Boolean;
begin
 Result:=pboolean(GetAddr(Index))^;
end;

procedure THArrayBoolean.SetValue(Index:integer;Value:Boolean);
begin
 Update(Index,@Value);
end;

 function THArrayBoolean.IndexOf(Value: Boolean): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayBoolean.IndexOfFrom(Value: Boolean;
  Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if Assigned(FValues) then begin
   Result:=memfindbyte(GetAddr(Start),byte(Value),Count-Start);
   if Result<>-1 then Result:=Result+Start;
 end;
end;

{ THArrayDouble }

constructor THArrayDouble.Create;
begin
 inherited Create;
 FItemSize:=sizeof(Double);
end;

function THArrayDouble.AddValue(Value:Double):integer;
begin
 Result:=inherited Add(@Value);
end;

function THArrayDouble.GetValue(Index:integer):Double;
begin
 Result:=pdouble(GetAddr(Index))^;
end;

procedure THArrayDouble.SetValue(Index:integer;Value:Double);
begin
 Update(Index,@Value);
end;

 function THArrayDouble.IndexOf(Value: double): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayDouble.IndexOfFrom(Value: double; Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if Assigned(FValues) then begin
  Result:=memfindgeneral(FValues,@Value,ItemSize,Count-Start);
  if Result<>-1 then Result:=Result+Start;
 end;
end;

{ THArrayCurrency }

constructor THArrayCurrency.Create;
begin
 inherited Create;
 FItemSize:=sizeof(currency);
end;

function THArrayCurrency.AddValue(Value:Currency):integer;
begin
 Result:=inherited Add(@Value);
end;

function THArrayCurrency.GetValue(Index:integer):Currency;
begin
 Result:=pcurrency(GetAddr(Index))^;
end;

procedure THArrayCurrency.SetValue(Index:integer;Value:Currency);
begin
 Update(Index,@Value);
end;

function THArrayCurrency.IndexOf(Value: currency): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayCurrency.IndexOfFrom(Value: currency;Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if Assigned(FValues) then begin
  Result:=memfindgeneral(FValues,@Value,ItemSize,Count-Start);
  if Result<>-1 then Result:=Result+Start;
 end;
end;

{ THArrayExtended }

constructor THArrayExtended.Create;
begin
 inherited Create;
 FItemSize:=sizeof(Extended);
end;

function THArrayExtended.GetValue(Index: integer): Extended;
begin
 Result:=pextended(GetAddr(Index))^;
end;

function THArrayExtended.AddValue(Value: Extended): integer;
begin
 Result:=inherited Add(@Value);
end;

procedure THArrayExtended.SetValue(Index: integer; Value: Extended);
begin
 Update(Index,@Value);
end;

function THArrayExtended.IndexOf(Value: Extended): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayExtended.IndexOfFrom(Value: Extended;
  Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if Assigned(FValues) then begin
  Result:=memfindgeneral(FValues,@Value,ItemSize,Count-Start);
  if Result<>-1 then Result:=Result+Start;
 end;
end;

{ TWideString }

constructor TWideString.Create(Value: WideString);
begin
 Str:=Value;
end;

{ THArrayWideStrings }

function THArrayWideStrings.AddValue(Value: WideString): integer;
begin
 Result:=inherited AddValue(TWideString.Create(Value));
end;

function THArrayWideStrings.GetValue(Index: integer): WideString;
begin
 Result:=TWideString(inherited GetValue(Index)).Str;
end;

function THArrayWideStrings.IndexOf(Value: WideString): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayWideStrings.IndexOfFrom(Value: WideString;
  Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if Assigned(FValues) then
  for Result:=Start to Count-1 do
   if self.Value[Result]=Value then exit;
 Result:=-1;
end;

procedure THArrayWideStrings.SetValue(Index: integer; Value: WideString);
begin
 TWideString(inherited GetValue(Index)).Str:=Value;
end;

{ THArrayString }

constructor THArrayString.Create;
begin
  str_ptr:=THArrayPointer.Create;
  FCount:=0;
  FCapacity:=0;
  FItemSize:=0;
  FValues:=nil;
end;

destructor THArrayString.Destroy;
var
  i    : integer;
  pStr : PChar;
begin
  for i:=0 to str_ptr.Count-1 do
  begin
    pStr:=PChar(str_ptr.Value[i]);
    StrDispose(pStr);
  end;
  str_ptr.Free;
end;

function THArrayString.CalcAddr(num:integer):pointer;
begin
  Result:=pointer(dword(str_ptr.FValues)+dword(num)*dword(FItemSize));
end;

function THArrayString.AddValue(Value:String):integer;
begin
  result:=self.Add(PChar(Value));
end;

function THArrayString.Add(pValue:pointer):integer;
begin
  Result:=Insert(FCount,pValue);
end;

function THArrayString.Insert(num:integer;pValue:pointer):integer;
var
  pStr : PChar;
  l    : integer;
begin
  l:=StrLen(PChar(pValue));
  pStr:=StrAlloc(l+1);
  memcpy(pValue,pStr,l+1);
  str_ptr.Insert(num,@pStr);
  FCount:=str_ptr.Count;
  FCapacity:=str_ptr.Capacity;
  Result:=FCount;
end;

procedure THArrayString.Update(num:integer;pValue:pointer);
var
  pStr : PChar;
  l    : integer;
begin
  pStr:=PChar(str_ptr.Value[num]);
  if pStr<>nil then StrDispose(pStr);

  if pValue<>nil then begin
   l:=StrLen(PChar(pValue));
   pStr:=StrAlloc(l+1);
   memcpy(pValue,pStr,l+1);
   str_ptr.Value[num]:=pStr;
  end else
   str_ptr.Value[num]:=nil;
end;

procedure THArrayString.MoveData(FromPos,Count,Offset:integer);
begin
  str_ptr.MoveData(FromPos, Count, Offset);
end;

procedure THArrayString.Delete(num:integer);
var pStr:PChar;
begin
  pStr:=PChar(str_ptr.Value[num]);
  StrDispose(pStr);
  str_ptr.Delete(num);
  FCount:=str_ptr.Count;
end;

procedure THArrayString.Get(num:integer;pValue:pointer);
var
  pStr : PChar;
  l    : integer;
begin
  pStr:=PChar(str_ptr.Value[num]);
  l:=StrLen(pStr);
  memcpy(pointer(pStr),pValue,l+1);
end;

function THArrayString.GetValue(Index:integer):String;
var
  pStr : PChar;
begin
  pStr:=PChar(str_ptr.Value[Index]);
  Result:=pStr;
end;

procedure THArrayString.SetValue(Index:integer;Value:String);
begin
  Self.Update(Index,pointer(Value));
end;

procedure THArrayString.Clear;
var i:integer;
    pStr:PChar;
begin
  for i:=0 to str_ptr.Count-1 do
  begin
    pStr:=PChar(str_ptr.Value[i]);
    StrDispose(pStr);
  end;
  str_ptr.Clear;
  FCount:=0;
  FCapacity:=0;
end;

procedure THArrayString.ClearMem;
var
  i    : integer;
  pStr : PChar;
begin
  for i:=0 to str_ptr.Count-1 do
  begin
    pStr:=PChar(str_ptr.Value[i]);
    StrDispose(pStr);
  end;
 str_ptr.ClearMem;
 inherited ClearMem;
end;

function THArrayString.IndexOf(Value:string):integer;
//var i : integer;
//    PVal : PChar;
begin
{PVal := PChar(Value);
  for i := 0 to Count-1 do
  begin
    if (StrComp(PVal,PChar(str_ptr.Value[i])) = 0) then
    begin
      Result:=i;
      exit;
    end;
  end;
  Result := -1;}

 Result:=IndexOfFrom(Value,0);
end;

function THArrayString.IndexOfFrom(Value: string; Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if Assigned(FValues) then
  for Result:=Start to Count-1 do
   if self.Value[Result]=Value then exit;
 Result:=-1;
end;

procedure THArrayString.Swap(Index1, Index2: integer);
begin
 str_ptr.Swap(Index1,Index2);
end;

{ THArrayStringFix }

function THArrayStringFix.AddValue(Value: string): integer;
var buf:pointer;
begin
 buf:=AllocMem(FItemSize+1);
 memclr(buf,FItemSize+1);
 try
  strplcopy(buf,Value,FItemSize);
  Result:=inherited Add(buf);
 finally
  FreeMem(buf);
 end;
end;

constructor THArrayStringFix.Create;
begin
 raise Exception.Create('Use CreateSize !');
end;

constructor THArrayStringFix.CreateSize(Size: integer);
begin
 inherited Create;
 FItemSize:=Size;
end;

function THArrayStringFix.GetValue(Index: integer): string;
var buf:pointer;
begin
 buf:=AllocMem(FItemSize+1);
 memclr(buf,FItemSize+1);
 try
  memcpy(GetAddr(Index),buf,FItemSize);
  Result:=strpas(buf);
 finally
  FreeMem(buf);
 end;
end;

function THArrayStringFix.IndexOf(Value: string): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayStringFix.IndexOfFrom(Value: string;
  Start: integer): integer;
begin
 Result:=-1;
 if Start=Count then exit;
 Error(Start,0,Count-1);
 if Assigned(FValues) then
  for Result:=Start to Count-1 do
   if self.Value[Result]=Value then exit;
 Result:=-1;
end;

procedure THArrayStringFix.SetValue(Index: integer; Value: string);
var buf:pointer;
begin
 buf:=AllocMem(FItemSize+1);
 memclr(buf,FItemSize+1);
 try
  strplcopy(buf,Value,FItemSize);
  inherited Update(Index,buf);
 finally
  FreeMem(buf);
 end;
end;

{ THash }

constructor THash.Create;
begin
 FReadOnly:=False;
 FAIndex:=THArrayInteger.Create;
end;

destructor THash.Destroy;
begin
 if not FReadOnly then FAIndex.Free;
 inherited Destroy;
end;

procedure THash.Clear;
begin
 FAIndex.Clear;
end;

procedure THash.ClearMem;
begin
 FAIndex.ClearMem;
end;

function THash.GetCount:integer;
begin
 Result:=FAIndex.Count;
end;

function THash.GetKey(Index:integer):integer;
begin
 Result:=FAIndex[Index];
end;

function THash.IfExist(Key:integer):boolean;
begin
 Result:=FAIndex.IndexOf(Key)<>-1;
end;

 { THashExists }

constructor THashExists.Create;
begin
 inherited Create;
end;

destructor THashExists.Destroy;
begin
 inherited Destroy;
end;

procedure THashExists.SetValue(Index:integer;Value:boolean);
var r:integer;
begin
 r:=FAIndex.IndexOf(Index);
 if (r=-1) and Value then FAIndex.AddValue(Index);
 if (r<>-1) and (not Value) then FAIndex.Delete(r);
end;

procedure THashExists.Delete(Key:integer);
var r:integer;
begin
 r:=FAIndex.IndexOf(Key);
 if (r<>-1) then FAIndex.Delete(r);
end;

function THashExists.GetValue(Index:integer):boolean;
var r:integer;
begin
 r:=FAIndex.IndexOf(Index);
 Result:=(r<>-1);
end;

 { THashBoolean }

constructor THashBoolean.Create;
begin
 inherited Create;
 FAValues:=THArrayBoolean.Create;
end;

constructor THashBoolean.CreateFromHArrays(IndexHArray:THArrayInteger;ValueHArray:THArrayBoolean);
begin
 FAIndex:=IndexHArray;
 FAValues:=ValueHArray;
 FReadOnly:=True;
end;

destructor THashBoolean.Destroy;
begin
 if not FReadOnly then  FAValues.Free;
 inherited Destroy;
end;

procedure THashBoolean.SetValue(Key:integer;Value:boolean);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAValues[n]:=Value;
  exit;
 end;
 if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound,[Key]));
 FAIndex.AddValue(Key);
 FAValues.AddValue(Value);
end;

function THashBoolean.GetValue(Key:integer):boolean;
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  Result:=FAValues[n];
 end else begin
  Result:=False;
 end;
end;

procedure THashBoolean.Clear;
begin
 inherited Clear;
 FAValues.Clear;
end;

procedure THashBoolean.ClearMem;
begin
 inherited ClearMem;
 FAValues.ClearMem;
end;

procedure THashBoolean.Delete(Key:integer);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAIndex.Delete(n);
  FAValues.Delete(n);
 end;
end;

 { THashInteger }

constructor THashInteger.Create;
begin
 inherited Create;
 FAValues:=THArrayInteger.Create;
end;

constructor THashInteger.CreateFromHArrays(IndexHArray:THArrayInteger;ValueHArray:THArrayInteger);
begin
 FAIndex:=IndexHArray;
 FAValues:=ValueHArray;
 FReadOnly:=True;
end;

destructor THashInteger.Destroy;
begin
 if not FReadOnly then  FAValues.Free;
 inherited Destroy;
end;

procedure THashInteger.SetValue(Key:integer;Value:integer);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAValues[n]:=Value;
  exit;
 end;
 if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound,[Key]));
 FAIndex.AddValue(Key);
 FAValues.AddValue(Value);
end;

function THashInteger.GetValue(Key:integer):integer;
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  Result:=FAValues[n];
 end else begin
  Result:=0;
 end;
end;

procedure THashInteger.Clear;
begin
 inherited Clear;
 FAValues.Clear;
end;

procedure THashInteger.ClearMem;
begin
 inherited ClearMem;
 FAValues.ClearMem;
end;

procedure THashInteger.Delete(Key:integer);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAIndex.Delete(n);
  FAValues.Delete(n);
 end;
end;

 { THashPointer }

constructor THashPointer.Create;
begin
 inherited Create;
 FAValues:=THArrayPointer.Create;
end;

constructor THashPointer.CreateFromHArrays(IndexHArray:THArrayInteger;ValueHArray:THArrayPointer);
begin
 FAIndex:=IndexHArray;
 FAValues:=ValueHArray;
 FReadOnly:=True;
end;

destructor THashPointer.Destroy;
begin
 if not FReadOnly then  FAValues.Free;
 inherited Destroy;
end;

procedure THashPointer.SetValue(Key:integer;Value:Pointer);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAValues[n]:=Value;
  exit;
 end;
 if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound,[Key]));
 FAIndex.AddValue(Key);
 FAValues.AddValue(Value);
end;

function THashPointer.GetValue(Key:integer):Pointer;
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  Result:=FAValues[n];
 end else begin
  Result:=nil;
 end;
end;

procedure THashPointer.Clear;
begin
 inherited Clear;
 FAValues.Clear;
end;

procedure THashPointer.ClearMem;
begin
 inherited ClearMem;
 FAValues.ClearMem;
end;

procedure THashPointer.Delete(Key:integer);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAIndex.Delete(n);
  FAValues.Delete(n);
 end;
end;

 { THashCurrency }

constructor THashCurrency.Create;
begin
 inherited Create;
 FAValues:=THArrayCurrency.Create;
end;

constructor THashCurrency.CreateFromHArrays(IndexHArray:THArrayInteger;ValueHArray:THArrayCurrency);
begin
 FAIndex:=IndexHArray;
 FAValues:=ValueHArray;
 FReadOnly:=True;
end;

destructor THashCurrency.Destroy;
begin
 if not FReadOnly then  FAValues.Free;
 inherited Destroy;
end;

procedure THashCurrency.SetValue(Key:integer;Value:currency);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAValues[n]:=Value;
  exit;
 end;
 if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound,[Key]));
 FAIndex.AddValue(Key);
 FAValues.AddValue(Value);
end;

procedure THashCurrency.Inc(Key:integer;Value:currency);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAValues[n]:=FAValues[n]+Value;
 end else begin
  if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound,[Key]));
  SetValue(Key,Value);
 end;
end;

function THashCurrency.GetValue(Key:integer):currency;
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  Result:=FAValues[n];
 end else begin
  Result:=0;
 end;
end;

procedure THashCurrency.Clear;
begin
 inherited Clear;
 FAValues.Clear;
end;

procedure THashCurrency.ClearMem;
begin
 inherited ClearMem;
 FAValues.ClearMem;
end;

procedure THashCurrency.Delete(Key:integer);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAIndex.Delete(n);
  FAValues.Delete(n);
 end;
end;

 { THashDouble }

constructor THashDouble.Create;
begin
 inherited Create;
 FAValues:=THArrayDouble.Create;
end;

constructor THashDouble.CreateFromHArrays(IndexHArray:THArrayInteger;ValueHArray:THArrayDouble);
begin
 FAIndex:=IndexHArray;
 FAValues:=ValueHArray;
 FReadOnly:=True;
end;

destructor THashDouble.Destroy;
begin
 if not FReadOnly then  FAValues.Free;
 inherited Destroy;
end;

procedure THashDouble.SetValue(Key:integer;Value:Double);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAValues[n]:=Value;
  exit;
 end;
 if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound,[Key]));
 FAIndex.AddValue(Key);
 FAValues.AddValue(Value);
end;

procedure THashDouble.Inc(Key:integer;Value:Double);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAValues[n]:=FAValues[n]+Value;
 end else begin
  if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound,[Key]));
  SetValue(Key,Value);
 end;
end;

function THashDouble.GetValue(Key:integer):Double;
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  Result:=FAValues[n];
 end else begin
  Result:=0;
 end;
end;

procedure THashDouble.Clear;
begin
 inherited Clear;
 FAValues.Clear;
end;

procedure THashDouble.ClearMem;
begin
 inherited ClearMem;
 FAValues.ClearMem;
end;

procedure THashDouble.Delete(Key:integer);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAIndex.Delete(n);
  FAValues.Delete(n);
 end;
end;

 { THashString }

constructor THashString.Create;
begin
 inherited Create;
 FAValues:=TStringList.Create;
 FAllowEmptyStr:=True;
end;

destructor THashString.Destroy;
begin
 FAValues.Free;
 inherited Destroy;
end;

procedure THashString.SetValue(Key:integer;Value:String);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  if not FAllowEmptyStr and (Value='')
   then begin FAValues.Delete(n); FAIndex.Delete(n); end
   else FAValues[n]:=Value;
 end else
  if FAllowEmptyStr or (Value<>'') then begin
   FAIndex.AddValue(Key);
   FAValues.Add(Value);
  end;
end;

function THashString.GetValue(Key:integer):String;
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  Result:=FAValues[n];
 end else begin
  Result:='';
 end;
end;

procedure THashString.Clear;
begin
 inherited Clear;
 FAValues.Clear;
end;

procedure THashString.ClearMem;
begin
 inherited ClearMem;
 FAValues.Clear;
end;

procedure THashString.Delete(Key:integer);
var n:integer;
begin
 n:=FAIndex.IndexOf(Key);
 if n>=0 then begin
  FAIndex.Delete(n);
  FAValues.Delete(n);
 end;
end;

 { THash2 }

constructor THash2.Create;
begin
 MainListIndex:=THArrayInteger.Create;
 MainListValue:=THArrayPointer.Create;
end;

destructor THash2.Destroy;
begin
 Clear;
 MainListValue.Free;
 MainListIndex.Free;
 inherited Destroy;
end;

{function THash2.GetKey(Index:integer):integer;
begin
 Result:=MainListIndex[Index];
end;}

procedure THash2.ClearMem;
begin
 Clear;
 MainListValue.ClearMem;
 MainListIndex.ClearMem;
end;

function THash2.GetChildHash(Key:integer):THash;
var n:integer;
begin
 n:=MainListIndex.IndexOf(Key);
 if n=-1
  then Result:=nil
  else Result:=MainListValue[n];
end;

procedure THash2.Delete(MainIndex,Index:integer);
var n:integer;
    arr:THashBoolean;
begin
 n:=MainListIndex.IndexOf(MainIndex);
 if n=-1 then exit;
 arr:=MainListValue[n];
 THash(arr).Delete(Index);
 if arr.Count=0 then begin
  arr.Free;
  MainListValue.Delete(n);
  MainListIndex.Delete(n);
 end;
end;

{function THash2.ExistMainHash(MainIndex:integer):boolean;
var n:integer;
begin
 n:=MainListIndex.IndexOf(MainIndex);
 Result:=n<>-1;
end;}

 { THash2Exists }

procedure THash2Exists.Clear;
var i:integer;
begin
 for i:=0 to MainListValue.Count-1 do begin
  THashExists(MainListValue[i]).Free;
 end;
 MainListValue.Clear;
 MainListIndex.Clear;
end;

procedure THash2Exists.SetValue(MainIndex,Index:integer;Value:boolean);
var arr:THashExists;
begin
 arr:=THashExists(GetChildHash(MainIndex));
 if arr=nil then begin
  arr:=THashExists.Create;
  MainListIndex.AddValue(MainIndex);
  MainListValue.AddValue(arr);
 end;
 arr[Index]:=Value;
end;

function THash2Exists.GetValue(MainIndex,Index:integer):boolean;
var arr:THashExists;
begin
 Result:=False;
 arr:=THashExists(GetChildHash(MainIndex));
 if arr=nil then exit;
 Result:=arr[Index];
end;

function THash2Exists.CreateMainHash(MainIndex:integer):THashExists;
var Co:integer;
    n:integer;
    arr:THashExists;
begin
 Result:=nil;
 n:=MainListIndex.IndexOf(MainIndex);
 if n=-1 then exit;
 Result:=THashExists.Create;
 arr:=MainListValue[n];
 Co:=arr.Count;
 if Co>0 then begin
  Result.FAIndex.SetCapacity(Co);
  Result.FAIndex.FCount:=Co;
  memcpy(arr.FAIndex.FValues,Result.FAIndex.FValues,Co*Result.FAIndex.FItemSize);
 end else begin
  Result.Free;
  Result:=nil;
 end;
end;

function THash2Exists.CreateHash(Index:integer):THashExists;
var i:integer;
begin
 Result:=THashExists.Create;
 for i:=0 to MainListIndex.Count-1 do begin
  if THashExists(MainListValue[i])[Index] then Result.FAIndex.AddValue(MainListIndex[i]);
 end;
 if Result.Count=0 then begin
  Result.Free;
  Result:=nil;
 end;
end;

 { THash2Currency }

procedure THash2Currency.Clear;
var i:integer;
begin
 for i:=0 to MainListValue.Count-1 do begin
  THashCurrency(MainListValue[i]).Free;
 end;
 MainListValue.Clear;
 MainListIndex.Clear;
end;

procedure THash2Currency.SetValue(MainIndex,Index:integer;Value:Currency);
var arr:THashCurrency;
begin
 arr:=THashCurrency(GetChildHash(MainIndex));
 if arr=nil then begin
  arr:=THashCurrency.Create;
  MainListIndex.AddValue(MainIndex);
  MainListValue.AddValue(arr);
 end;
 arr[Index]:=Value;
end;

procedure THash2Currency.Inc(MainIndex,Index:integer;Value:Currency);
var c: currency;
begin
 c:=GetValue(MainIndex,Index);
 SetValue(MainIndex,Index,Value+c);
end;

function THash2Currency.GetValue(MainIndex,Index:integer):Currency;
var arr:THashCurrency;
begin
 Result:=0;
 arr:=THashCurrency(GetChildHash(MainIndex));
 if arr=nil then exit;
 Result:=arr[Index];
end;

function THash2Currency.CreateMainHash(MainIndex:integer):THashCurrency;
var arr:THashCurrency;
    Co:integer;
    n:integer;
begin
 Result:=nil;
 n:=MainListIndex.IndexOf(MainIndex);
 if n=-1 then exit;
 Result:=THashCurrency.Create;
 arr:=MainListValue[n];
 Co:=arr.Count;
 if Co>0 then begin
  Result.FAIndex.SetCapacity(Co);
  Result.FAIndex.FCount:=Co;
  Result.FAValues.SetCapacity(Co);
  Result.FAValues.FCount:=Co;
  memcpy(arr.FAIndex.FValues,Result.FAIndex.FValues,Co*Result.FAIndex.FItemSize);
  memcpy(arr.FAValues.FValues,Result.FAValues.FValues,Co*Result.FAValues.FItemSize);
 end else begin
  Result.Free;
  Result:=nil;
 end;
end;

function THash2Currency.CreateHash(Index:integer):THashCurrency;
var i:integer;
begin
 Result:=THashCurrency.Create;
 for i:=0 to MainListIndex.Count-1 do begin
  if THashCurrency(MainListValue[i]).FAIndex.IndexOf(Index)<>-1 then begin
   Result.FAIndex.AddValue(i);
   Result.FAValues.AddValue(THashCurrency(MainListValue[i])[Index]);
  end;
 end;
 if Result.Count=0 then begin
  Result.Free;
  Result:=nil;
 end;
end;

 { THash2Integer }

procedure THash2Integer.Clear;
var i:integer;
begin
 for i:=0 to MainListValue.Count-1 do begin
  THashInteger(MainListValue[i]).Free;
 end;
 MainListValue.Clear;
 MainListIndex.Clear;
end;

procedure THash2Integer.SetValue(MainIndex,Index:integer;Value:Integer);
var arr:THashInteger;
begin
 arr:=THashInteger(GetChildHash(MainIndex));
 if arr=nil then begin
  arr:=THashInteger.Create;
  MainListIndex.AddValue(MainIndex);
  MainListValue.AddValue(arr);
 end;
 arr[Index]:=Value;
end;

function THash2Integer.GetValue(MainIndex,Index:integer):Integer;
var arr:THashInteger;
begin
 Result:=0;
 arr:=THashInteger(GetChildHash(MainIndex));
 if arr=nil then exit;
 Result:=arr[Index];
end;

function THash2Integer.CreateMainHash(MainIndex:integer):THashInteger;
var arr:THashInteger;
    Co:integer;
    n:integer;
begin
 Result:=nil;
 n:=MainListIndex.IndexOf(MainIndex);
 if n=-1 then exit;
 Result:=THashInteger.Create;
 arr:=MainListValue[n];
 Co:=arr.Count;
 if Co>0 then begin
  Result.FAIndex.SetCapacity(Co);
  Result.FAIndex.FCount:=Co;
  Result.FAValues.SetCapacity(Co);
  Result.FAValues.FCount:=Co;
  memcpy(arr.FAIndex.FValues,Result.FAIndex.FValues,Co*Result.FAIndex.FItemSize);
  memcpy(arr.FAValues.FValues,Result.FAValues.FValues,Co*Result.FAValues.FItemSize);
 end else begin
  Result.Free;
  Result:=nil;
 end;
end;

function THash2Integer.CreateHash(Index:integer):THashInteger;
var i:integer;
begin
 Result:=THashInteger.Create;
 for i:=0 to MainListIndex.Count-1 do begin
  if THashInteger(MainListValue[i]).FAIndex.IndexOf(Index)<>-1 then begin
   Result.FAIndex.AddValue(i);
   Result.FAValues.AddValue(THashInteger(MainListValue[i])[Index]);
  end;
 end;
 if Result.Count=0 then begin
  Result.Free;
  Result:=nil;
 end;
end;

 { THash2String }

procedure THash2String.Clear;
var i:integer;
begin
 for i:=0 to MainListValue.Count-1 do begin
  THashString(MainListValue[i]).Free;
 end;
 MainListValue.Clear;
 MainListIndex.Clear;
end;

procedure THash2String.SetValue(MainIndex,Index:integer;Value:String);
var arr:THashString;
begin
 arr:=THashString(GetChildHash(MainIndex));
 if arr=nil then begin
  arr:=THashString.Create;
  MainListIndex.AddValue(MainIndex);
  MainListValue.AddValue(arr);
 end;
 arr[Index]:=Value;
end;

function THash2String.GetValue(MainIndex,Index:integer):String;
var arr:THashString;
begin
 Result:='';
 arr:=THashString(GetChildHash(MainIndex));
 if arr=nil then exit;
 Result:=arr[Index];
end;

function THash2String.CreateMainHash(MainIndex:integer):THashString;
var arr:THashString;
    Co:integer;
    n,i:integer;
begin
 Result:=nil;
 n:=MainListIndex.IndexOf(MainIndex);
 if n=-1 then exit;
 Result:=THashString.Create;
 arr:=MainListValue[n];
 Co:=arr.Count;
 if Co>0 then begin
  Result.FAIndex.SetCapacity(Co);
  for i:=0 to arr.Count-1 do begin
   Result[arr.Keys[i]]:=arr[arr.Keys[i]];
  end;
 end else begin
  Result.Free;
  Result:=nil;
 end;
end;

function THash2String.CreateHash(Index:integer):THashString;
var i:integer;
begin
 Result:=THashString.Create;
 for i:=0 to MainListIndex.Count-1 do begin
  if THashString(MainListValue[i]).FAIndex.IndexOf(Index)<>-1 then begin
   Result.FAIndex.AddValue(i);
   Result.FAValues.Add(THashString(MainListValue[i])[Index]);
  end;
 end;
 if Result.Count=0 then begin
  Result.Free;
  Result:=nil;
 end;
end;


{ THArrayString_ }

{function THArrayString_.Add(pValue: pointer): integer;
var pStr:PChar;
begin
 pStr:=DublicateStr(pValue);
 inherited Add(pStr);
end;

function THArrayString_.AddValue(Value: string): integer;
var pStr:PChar;
begin
  pStr:=DublicateStr(PChar(Value));
  Add(pStr);
end;

procedure THArrayString_.Clear;
begin
  ClearStrings;
  inherited Clear;
end;

procedure THArrayString_.ClearMem;
begin
  ClearStrings;
  inherited ClearMem;
end;

procedure THArrayString_.ClearStrings;
var i:integer;
    pStr:PChar;
begin
  for i:=0 to Count-1 do begin
   Get(i,pStr);
   StrDispose(pStr);
  end;
end;

procedure THArrayString_.Delete(num: integer);
var pStr:PChar;
begin
  Get(num,pStr);
  StrDispose(pStr);
  inherited Delete(num);
end;

destructor THArrayString_.Destroy;
begin
  ClearStrings;
  inherited Destroy;
end;

function THArrayString_.DublicateStr(pValue: pointer): PChar;
var len:integer;
begin
  if pValue<>nil then begin
   len:=StrLen(PChar(pValue))+1;
   Result:=StrAlloc(len);
   memcpy(pValue,Result,len);
  end else
   Result:=nil;
end;

function THArrayString_.GetValue(Index: integer): string;
begin
 Result:=PChar(ppointer(GetAddr(Index))^);
end;

function THArrayString_.IndexOf(Value: string): integer;
begin
 Result:=IndexOfFrom(Value,0);
end;

function THArrayString_.IndexOfFrom(Value: string;
  Start: integer): integer;
begin

end;

function THArrayString_.Insert(num: integer; Value: string): integer;
begin
 Insert(num,pointer(PChar(Value)));
end;

function THArrayString_.Insert(num: integer; pValue: pointer): integer;
var pStr:PChar;
begin
 pStr:=DublicateStr(pValue);
 inherited Insert(num,pStr);
end;

procedure THArrayString_.LoadFromStream(s: TStream);
 function LoadString(Stream:TStream):PChar;
 var sz:integer;
 begin
  Stream.Read(sz,sizeof(integer));
  Result:=StrAlloc(sz+1);
  Stream.Read(Result^,sz);
 end;
var c,i:integer;
begin
 s.Read(c,sizeof(integer));
 for i:=1 to c do
  inherited Add(LoadString(s));
end;

procedure THArrayString_.SaveToStream(s: TStream);
 procedure SaveString(Stream:TStream;pValue:PChar);
 var len:integer;
 begin
  len:=StrLen(pValue);
  Stream.Write(len,sizeof(integer));
  Stream.Write(pValue^,len);
 end;
var i:integer;
    pStr:PChar;
begin
 i:=Count;
 s.Write(i,sizeof(integer)); // number of elements
 for i:=0 to Count-1 do begin
  Get(i,pStr);
  SaveString(s,pStr);
 end;
end;

procedure THArrayString_.SetValue(Index: integer; Value: string);
begin
 Update(Index,PChar(Value));
end;

procedure THArrayString_.Update(num: integer; pValue: pointer);
var
  pStr : PChar;
  l    : integer;
begin
  Get(num,pStr);
  StrDispose(pStr);
  pStr:=DublicateStr(pValue);
  inherited Update(num,pStr);
end;
 }

end.
