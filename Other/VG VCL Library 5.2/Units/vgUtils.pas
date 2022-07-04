{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         General-purpose utility routines              }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgUtils;

interface
uses Windows, {$IFNDEF _D3_}Ole2,{$ENDIF} SysUtils, TypInfo, Classes, vgSystem;

type
  EMessage            = class(Exception);
  EWarningMessage     = class(EMessage);
  EInformationMessage = class(EMessage);
  EFileOperation      = class(Exception);

  TMessageProc = procedure(const Msg: string);
  TWriteLogProc = procedure(const FileName, Msg: string);

  TComponentCallback = procedure(Instance: TComponent; Data: Pointer);

  TCompareItems = function (Data: Pointer; Item1, Item2: Pointer): Integer;
  TExchangeItems = procedure (Data: Pointer; Index1, Index2: Integer);

{ --- Exceptions }
procedure CheckCondition(Condition: Boolean; EClass: ExceptClass; const EMessage: string);
{ Raises exception if not Condition }

procedure InformationMessage(const Msg: string);
{ Raises EInformationMessage exception with message }

procedure WarningMessage(const Msg: string);
{ Raises EWarningMessage exception with message }

{ --- Math }
function Max(A, B: Integer): Integer;
function Min(A, B: Integer): Integer;
{ Returns min or max value accordinally }

function RangeCheck(Value, Min, Max: Integer): Integer;
{ Controls that Value betweeen Min and Max and increase or decrease Result accordinaly }

function RoundFloat(Value: Extended; Digits: Integer): Extended;
{ Rounds Value to Digits digits after decimal point }

function CompareInteger(Value1, Value2: Integer): Integer;
function CompareDWord(Value1, Value2: DWord): Integer;
{$IFDEF _D4_}
function CompareInt64(const Value1, Value2: Int64): Integer;
function CompareUInt64(const Value1, Value2: Int64): Integer;
{$ENDIF}
{ Compares two given Integer values }

function CompareFloat(Value1, Value2: Extended; Digits: Integer): Integer;
{ Compares two given values and returns result of comparision }
{ Return values are the same as in CompareText function       }

function IsEqualFloat(Value1, Value2: Extended; Digits: Integer): Boolean;
function IsAboveFloat(Value1, Value2: Extended; Digits: Integer): Boolean;
function IsBehindFloat(Value1, Value2: Extended; Digits: Integer): Boolean;
function IsAboveEqualFloat(Value1, Value2: Extended; Digits: Integer): Boolean;
function IsBehindEqualFloat(Value1, Value2: Extended; Digits: Integer): Boolean;
{ Compares two values up to Digits digits after decimal point }

function StrToFloatDef(const Value: string; Default: Extended): Extended;
{ Converts Value into extended. }

{ --- Memory blocks }
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;
{ Compares to blocks of memory }

function FindInteger(Value: Integer; const Buff; Count: Integer): Integer;
{ Searches for Value in the Buff array. Returns index of Value or -1 if not found }

function CompareChars(const Buffer1, Buffer2; Count: Integer): Integer;
{ Compares two block of chars }
{ Return values are the same as in CompareText function       }

procedure ZeroMem(pBuff: Pointer; Count: Integer);
{ Initializes block of memory with zeros }

{ --- String routines }
function Like(const Source, Template: String): Boolean;

function Bin2Hex(Bytes: PChar; Count: Integer): string;
procedure Hex2Bin(Hex: string; Bytes: PChar; Count: Integer);
{ Converts binary buffer into string and backward }

procedure AddDelimeted(var S: string; const SubStr, Delimeter: string);
function GetListString(Fmt: string; Strings: TStrings): string;
function ExtractDelimeted(const S, Delimeter: string; var Pos: Integer): string;
function ExtractDelimetedWord(const S, Delimeter: string; Number: Integer; var Pos: Integer): string;
procedure GetDelimetedStrings(const S, Delimeter: string; List: TStrings);

function PosText(const SubStr, Source: string): Integer;
{ Searches postionion of SubStr without case sensitivity }

function ReplaceStr(const S, Srch, Replace: string): string;
{ Repleaces Srch substrings with Replace }

function WordCount(const S: string; const WordDelims: TCharSet): Integer;
{ Returns a number of words delimeted with WordDelims }

function WordPosition(const N: Integer; const S: string; const WordDelims: TCharSet): Integer;
{ Returns a position of word number N in the string S }

function ExtractWord(N: Integer; const S: string; const WordDelims: TCharSet): string;
{ Returns a word number N in the string S }

procedure WideCharToNames(Names: PChar; NameCount: Byte; var WideNames: TNames; var Size: Word);
{ Converts pointer to wide char to names }

{ --- Macro }
function ForEachString(Strings: TStrings; const Separator, StringMacro, Macro: string): string;
{ Expands macro Macro for each string from Strings.           }
{ Expanded macroses are concatenated with Separator separator }
{ Each occurence of StringMacro in Macro string is replaced   }
{ by string from Strings.                                     }

{ --- Nill-able TLists }
function ListAdd(var List: TList; Item: Pointer): Pointer;
{ Adds Item to list List. If List is nil than calls TList constructor }

procedure ListClear(var List: TList);
{ Macro for ListDestroy }

function ListCount(List: TList): Integer;
{ Returns count of items in List or zero if List = nil }

function ListDelete(var List: TList; Index: Integer): Pointer;
{ Deletes from List. If List is empty than destroys List }

procedure ListDestroy(var List: TList);
{ Destroys List and set it to nil }

procedure ListDestroyAll(var List: TList);
{ Destroys all items from TList as they are TObject descendents and List object }

procedure ListDestroyObjects(List: TList);
{ Destroys all items from TList as they are TObject descendents }
{ and clears the List }

procedure ListDestroyObjectsAll(var List: TList);
{ ListDestroyAll macro }

procedure ListFreeMem(List: TList);
{ Destroys all items from TList as they pointers allocated with GetMem }
{ and clears the List }

procedure ListFreeMemAll(var List: TList);
{ Destroys all items from TList as they pointers allocated with GetMem }
{ and frees the List }

procedure ListSort(List: TList; Compare: TListSortCompare);
{ Sorts given list }

procedure ListError(Index: Integer);
{ raises EListError exception }

function ListIndexOf(List: TList; Item: Pointer): Integer;
{ Returns index of Item in List }

procedure ListInsert(var List: TList; Index: Integer; Item: Pointer);
{ Inserts Item in List }

function ListItem(List: TList; Index: Integer): Pointer;
{ Returns item of List raises exception if List = nil }

function ListRemove(var List: TList; Item: Pointer): Pointer;
{ Removes Item from List. If List is empty than destroys List }

function ListRemoveLast(var List: TList): Pointer;
{ Same as ListRemove, but removes last item from List }

procedure QuickSortList(AList: TList; DataCompare, DataExchange: Pointer;
  Compare: TCompareItems; AExchange: TExchangeItems);
{ Sorts given List }

procedure ListAssign(var Dest: TList; Source: TList);
{ Copies contents of Source into Dest }

function ListCheck(var List: TList): TList;
{ Checks that List exists and creates it if not }

{ --- TStrings }
procedure StringsAssignTo(List: TStrings; const Strings: Array of string);
procedure ArrayAssignTo(List: TStrings; var Strings: Array of string);
{ Assings a strings from List to Strings elements and backward }

function StringsHistoryInsertObject(List: TStrings; Index: Integer;
  const Value: string; AObject: TObject; MaxCount: Integer): Integer;

function StringsHistoryInsert(List: TStrings; Index: Integer;
  const Value: string; MaxCount: Integer): Integer;

function StringsHistoryAddObject(List: TStrings; const Value: string;
  AObject: TObject; MaxCount: Integer): Integer;

function StringsHistoryAdd(List: TStrings; const Value: string; MaxCount: Integer): Integer;

{ --- Components and streams }
function IsClass(AClass: TClass; ParentClass: TClass): Boolean;
{ Returns True if AClass is ParentClass descendent }

procedure RegisterComponent(Instance: TComponent);
{ Register component Instance and all their recursive childrens             }
{ through RegisterClass procedure to make sure it can be readed from stream }

procedure CopyProps(Src: TComponent; Dst: TComponent);
{ Copies Src component and all their recursive childrens to Dst component   }
{ Note that Src.Owner cannot be nil                                         }

function CreateCloneOwner(Src: TComponent; AOwner: TComponent): TComponent;
{ Creates a clone of Src component with the Owner AOwner }

function CreateClone(Src: TComponent): TComponent;
{ Creates a clone of Src component with the same Owner }

function CreateComponentOwnerNeeded(var Instance; ComponentClass: TComponentClass;
  AOwner: TComponent): TComponent;
{ Creates a component with Owner if Instance is not assigned yet }

function CreateComponentNeeded(var Instance; ComponentClass: TComponentClass): TComponent;
{ Creates a component without Owner if Instance is not assigned yet }

function CreateCloneOwnerNeeded(var Instance; Src: TComponent; AOwner: TComponent): TComponent;
{ Creates a clone of Src component with the Owner AOwner if Instance is not Assigned and returns Instance }

function CreateCloneNeeded(var Instance; Src: TComponent): TComponent;
{ Creates a clone of Src component with the Owner AOwner if Instance is not Assigned and returns Instance }

procedure FreeObject(var Obj);
{ Frees Obj and initialize it to zero }

procedure CopyMethodProps(Src, Dst: TObject);
{ Copies method properties from Src to Dst objects }

function UniqueName(Instance: TComponent; const Name: string; Owner: TComponent): string;
{ Returns unique name to component Instance to make sure it valid for Owner }

procedure WriteAndRead(Src: TComponent; Dst: TComponent); { obsolete }
{ Macro for CopyProps procedure }

{ --- For Each component }
procedure ForComponents(AComponents: array of TComponent;
  Callback: TComponentCallback; Data: Pointer);
{ ForEach Callback }

procedure ForEachComponent(Instance: TComponent;
  ComponentClass: TComponentClass; Callback: TComponentCallback; Data: Pointer; Children: Boolean);
{ Recurrent Callback }

{ --- IniFiles and logs }
function AppPathFileName(FileName: TFileName): TFileName;
{ Returns the file name concated from application path and FileName }

procedure WriteBoolean(const IniFile, IniSection, Ident: string; const Value: Boolean; UseRegistry: Boolean);
procedure WriteFloat(const IniFile, IniSection, Ident: string; const Value: Double; UseRegistry: Boolean);
procedure WriteInteger(const IniFile, IniSection, Ident: string; const Value: Integer; UseRegistry: Boolean);
procedure WriteString(const IniFile, IniSection, Ident, Value: string; UseRegistry: Boolean);

function ReadBoolean(const IniFile, IniSection, Ident: string; const DefValue: Boolean; UseRegistry: Boolean): Boolean;
function ReadFloat(const IniFile, IniSection, Ident: string; const DefValue: Double; UseRegistry: Boolean): Double;
function ReadInteger(const IniFile, IniSection, Ident: string; const DefValue: Integer; UseRegistry: Boolean): Integer;
function ReadString(const IniFile, IniSection, Ident, DefValue: string; UseRegistry: Boolean): string;
{ Writes/reads Value to or/from ini files or registry }

procedure AppWriteLog(const Msg: string);
{ Writes a Msg to file with file name AppFileName and .log extension }

procedure WriteLog(const FileName: TFileName; const Msg: string);
{ Writes a log message in the critical section      }
{ through WriteLogProc or DefaultWriteLog procedure }

procedure DefaultWriteLog(const FileName: TFileName; const Msg: string);
{ Writes a log message in file }

{ --- File operations support }
function GetTempFileName(const Path: TFileName): TFileName;
{ Returns the generic name of the temporary file in the TEMP dir }

function BackupFile(const FileName: TFileName): Boolean;
{ Renames file FileName to *.bak deleting old backup if exists }

procedure CheckBackupFile(const FileName: TFileName);
{ Backups file and raises an exception if an error occurs }

procedure CheckDeleteFile(const FileName: TFileName);
{ Deletes file and raises an exception if an error occurs }

procedure CheckRenameFile(const OldName, NewName: TFileName);
{ Renames file and raises an exception if an error occurs }

procedure LoadComponent(const FileName: string; Instance: TComponent);
{ Loads component properties from file }

procedure SaveStream(const FileName: string; Source: TStream);
{ Writes stream to file and backups old if needed }

procedure SaveComponent(const FileName: string; Instance: TComponent);
{ Saves component properties to file and backups old if needed }

procedure GetFileNames(const Directory, FileMask: string; Attr: Integer; FileNames: TStrings);
{ Adds all found files to FileNames }

{ --- Variant }
function NvlInteger(const Value: Variant): Integer;
function NvlFloat(const Value: Variant): Double;
function NvlDateTime(const Value: Variant): TDateTime;
function NvlString(const Value: Variant): string;
{ Functions return "zero" values when Value is Null or   }
{ when Value value is compatible with the function result }

function VarRecToVariant(VarRec: TVarRec): Variant;
{ Converts VarRec record into Variant }

function VarArrayFromConst(const Args: array of const): Variant;
{ Converts array of const into Variant array with bounds 0, High(Args) }

function VarArrayFromConstCast(const Args: array of const): Variant;
{ Converts array of pairs [Variant, Integer] into Variant array with bounds 0, High(Args) }
{ and casts each of elements to Integer varXXX }

function VarArrayCast(const Values: Variant; Args: array of Integer): Variant;
{ Args is array of pairs [Index, varType]                       }
{ Function casts each Values[Index] to varType type             }

function VarArrayOfPairs(const Args: array of const): Variant;
{ Args is array of pairs. Each pair converted into variant array. }
{ Result is variant array of variant arrays                       }

function VarArrayOfPairsCast(const Values: Variant; const Args: array of Integer): Variant;
{ Function casts second element of pair accordinally to Args }

function VarComparable(const V1, V2: Variant): Boolean;
{ Returns true if V1 can be compared with V2 }

function VarIsEqual(const V1, V2: Variant): Boolean;
{ Reruns true if V1 is comparable with V2 and equal }

procedure StringsFromVarArray(const List: Variant; Strings: TStrings);
{ Converts array of Variant into list of strings }

function VarArrayFromStrings(Strings: TStrings): Variant;
{ Converts list of strings into Variant array }

procedure EnumStrings(List: TStrings; EnumProc: TGetStrProc);
{ Enums each elemnt of List }

procedure EnumVarArray(const List: Variant; EnumProc: TGetStrProc);
{ Enums each elemnt of List }

function VarToDispatch(Instance: Variant): IDispatch;
{ Extractts IDispatch interface from interface }

procedure WriteBufferAt(Stream: TStream; const Buff; Count: Integer; Position: Integer);
{ Writes Count bytes to the Stream and restores old stream position }

{ --- Swap }
procedure SwapStrings(var Str1, Str2: string);
procedure SwapInteger(var Value1, Value2: Integer);

{ --- DateTime }
function IsLeapYear(Year: Word): Boolean;
function GetDayTable(Year: Word): PDayTable;

{ --- DLLs }
procedure PreloadLibraries(const DLLs: array of PChar; Handles: PInstance);
{ Loads DLL libraries in DLLs array for faster execution after startup }

procedure UnloadLibraries(Handles: PInstance; Count: Integer);
{ Frees DLL libraries loaded in PreloadLibraries procedure }

function RegisterServer(const DLLName: string): Boolean;
{ Registers OLE server in filename DLLName and returns True if successfull }

{ --- VMT low-level }
function GetVirtualMethodAddress(AClass: TClass; AIndex: Integer): Pointer;
{ Returns address of virtual method of AClass with index AIndex }

function SetVirtualMethodAddress(AClass: TClass; AIndex: Integer;
  NewAddress: Pointer): Pointer;
{ Updates VMT of AClass and sets new method address of method with index AIndex }

function FindVirtualMethodIndex(AClass: TClass; MethodAddr: Pointer): Integer;
{ Iterates through VMT of AClass and seeks for method MethodAddr }

{ Misc }
{$IFDEF _D3_}
function ResStr(const Ident: string): string;
{$ELSE}
function ResStr(Ident: Integer): string;
{$ENDIF}

{ Macro for loading strings from resources }

{$IFDEF _D3_}
{ --- Resourcestring }
procedure StoreResString(P: PResStringRec);
{ Saves resourcestring information }

procedure RestoreResString(P: PResStringRec);
{ Restores resourcestring information }

procedure CopyResString(Source, Dest: PResStringRec; Store: Boolean);
{ Owerwrites resourcestring information to make Dest resourcesting   }
{ the same as Source                                                 }
{ Note that if Source and Dest are in different packages you should  }
{ always restore resourcestring information before unloading package }
{ that contains Source                                               }
{$ENDIF}

function Win32Description: string;
{ Returns string like 'Windows NT 4.00 (Service Pack 4)' }

procedure GetEnvironment(Strings: TStrings);
{ Extracts environment strings and sets Strings as Values array }

function GetEnvironmentVariable(const Variable: string): string;
{ Extracts value of variable }

const
  SEMAPHORE_MODIFY_STATE    =  $0002;
  SEMAPHORE_ALL_ACCESS      =  STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $0003;

{$IFNDEF _D3_}

  PROCESS_TERMINATE         =  $0001;
  PROCESS_CREATE_THREAD     =  $0002;
  PROCESS_VM_OPERATION      =  $0008;
  PROCESS_VM_READ           =  $0010;
  PROCESS_VM_WRITE          =  $0020;
  PROCESS_DUP_HANDLE        =  $0040;
  PROCESS_CREATE_PROCESS    =  $0080;
  PROCESS_SET_QUOTA         =  $0100;
  PROCESS_SET_INFORMATION   =  $0200;
  PROCESS_QUERY_INFORMATION =  $0400;
  PROCESS_ALL_ACCESS        =  STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $0FFF;

{$ENDIF _D3_}

{ --- System }
function IsMainThread: Boolean;
{ Returns True if called from the main thread }

function GetProcessHandle(ProcessID: DWORD): THandle;
{ Tryies to extract process handle from ProcessID         }

{ --- TypInfo utilites }
function GetPropType(PropInfo: PPropInfo): PTypeInfo;
{ Returns type information for the given property }

procedure GetPropInfoList(List: TList; Instance: TObject; Filter: TTypeKinds);
{ Fills List with PPropInfo pointers }

var
  { Variable used in Log procedures. Default is ParamStr(0) }
  AppFileName      : string = '';

  InformationProc  : TMessageProc  = nil;
  WarningProc      : TMessageProc  = nil;

  { When assigned, called by WriteLog procedure             }
  WriteLogProc     : TWriteLogProc = nil;
 
  { Timeout for waiting for logfile to be opened }
  AppLogTimeout    : Integer       = 0;

const
  EasyArrayTypes = [varSmallInt, varInteger, varSingle, varDouble, varCurrency,
                    varDate, varBoolean, varByte];

  VariantSize: array[0..varByte] of Word  = (0, 0, SizeOf(SmallInt), SizeOf(Integer),
    SizeOf(Single), SizeOf(Double), SizeOf(Currency), SizeOf(TDateTime), 0, 0,
    SizeOf(Integer), SizeOf(WordBool), 0, 0, 0, 0, 0, SizeOf(Byte));

const
  IniFileExt       = '.ini';

implementation
uses Consts, vgVCLRes, IniFiles, Registry, Math;

var
  FLogLock: TRTLCriticalSection;

procedure CheckCondition(Condition: Boolean; EClass: ExceptClass; const EMessage: string);
begin
  if not Condition then raise EClass.Create(EMessage);
end;

procedure InformationMessage(const Msg: string);
begin
  if Assigned(InformationProc) then
    InformationProc(Msg) else raise EInformationMessage.Create(Msg);
end;

procedure WarningMessage(const Msg: string);
begin
  if Assigned(WarningProc) then
    WarningProc(Msg) else raise EWarningMessage.Create(Msg);
end;

function Max(A, B: Integer): Integer; assembler;
asm
        CMP     EAX,EDX
        JG      @@Exit
        MOV     EAX,EDX
@@Exit:
end;

function Min(A, B: Integer): Integer; assembler;
asm
        CMP     EAX,EDX
        JL      @@Exit
        MOV     EAX,EDX
@@Exit:
end;

function RangeCheck(Value, Min, Max: Integer): Integer;
begin
  if Value < Min then Result := Min
  else if Value > Max then Result := Max
  else Result := Value;
end;

function RoundFloat(Value: Extended; Digits: Integer): Extended;
var
  StrFmt: string;
begin
  StrFmt := '%.' + IntToStr(Digits) + 'f';
  Result := StrToFloat(Format(StrFmt, [Value]));
end;

function CompareInteger(Value1, Value2: Integer): Integer;
asm
      MOV       ECX,EAX
      XOR       EAX,EAX
      CMP       ECX,EDX
      JG        @@G
      JE        @@Q
@@G:  INC       EAX
      JMP       @@Q
@@L:  DEC       EAX
@@Q:
end;

function CompareDWord(Value1, Value2: DWord): Integer;
asm
      MOV       ECX,EAX
      XOR       EAX,EAX
      CMP       ECX,EDX
      JA        @@G
      JE        @@Q
@@G:  INC       EAX
      JMP       @@Q
@@L:  DEC       EAX
@@Q:
end;

{$IFDEF _D4_}
function CompareInt64(const Value1, Value2: Int64): Integer;
asm
      MOV       ECX,DWORD PTR [Value1]
      MOV       EDX,DWORD PTR [Value1 + 4]
      XOR       EAX,EAX
@C1:  CMP       EDX,DWORD PTR [Value2]
      JG        @@G
      JL        @@L
@C2:  CMP       ECX,DWORD PTR [Value2 + 4]
      JB        @@L
      JE        @@Q
@@G:  INC       EAX
      JMP       @@Q
@@L:  DEC       EAX
@@Q:
end;

function CompareUInt64(const Value1, Value2: Int64): Integer;
asm
      MOV       ECX,DWORD PTR [Value1]
      MOV       EDX,DWORD PTR [Value1 + 4]
      XOR       EAX,EAX
@C1:  CMP       EDX,DWORD PTR [Value2]
      JA        @@G
      JB        @@L
@C2:  CMP       ECX,DWORD PTR [Value2 + 4]
      JB        @@L
      JE        @@Q
@@G:  INC       EAX
      JMP       @@Q
@@L:  DEC       EAX
@@Q:
end;
{$ENDIF}

function CompareFloat(Value1, Value2: Extended; Digits: Integer): Integer;
var
  Delta: Extended;
begin
  Value1 := Value1 - Value2;
  Delta := IntPower(1E1, - Digits);
  if Abs(Value1) <= Delta then Result := 0
  else if (Value1 > 0) then Result := 1
  else Result := -1;
end;

function IsEqualFloat(Value1, Value2: Extended; Digits: Integer): Boolean;
begin
  Result := CompareFloat(Value1, Value2, Digits) = 0;
end;

function IsAboveFloat(Value1, Value2: Extended; Digits: Integer): Boolean;
begin
  Result := CompareFloat(Value1, Value2, Digits) = 1;
end;

function IsBehindFloat(Value1, Value2: Extended; Digits: Integer): Boolean;
begin
  Result := CompareFloat(Value1, Value2, Digits) = -1;
end;

function IsAboveEqualFloat(Value1, Value2: Extended; Digits: Integer): Boolean;
begin
  Result := CompareFloat(Value1, Value2, Digits) >= 0;
end;

function IsBehindEqualFloat(Value1, Value2: Extended; Digits: Integer): Boolean;
begin
  Result := CompareFloat(Value1, Value2, Digits) <= 0;
end;

function StrToFloatDef(const Value: string; Default: Extended): Extended;
begin
  try
    Result := StrToFloat(Value);
  except
    on EConvertError do
      Result := Default
    else
      raise;
  end;
end;

function Bin2Hex(Bytes: PChar; Count: Integer): string;
var
  I: Integer;
begin
  Result := '';
  if Assigned(Bytes) then
  for I := 0 to Count - 1 do
    Result := Result + IntToHex(Byte((Bytes + I)^), 2);
end;

procedure Hex2Bin(Hex: string; Bytes: PChar; Count: Integer);
var
  I: Integer;
  C: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    C := StrToInt('$' + Copy(Hex, 1 + I * 2, 2));
    (PChar(Bytes) + I)^ := Chr(C);
  end;
end;

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
{$IFNDEF _D3_}
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;
{$ELSE}
asm
        JMP     SysUtils.CompareMem
end;
{$ENDIF}

function FindInteger(Value: Integer; const Buff; Count: Integer): Integer; assembler;
asm
        XCHG    EDI,EDX
        PUSH    ECX
        REPNE   SCASD
        MOV     EDI,EDX
        POP     EAX
        JE      @@1
        XOR     EAX,EAX
@@1:    SUB     EAX,ECX
        DEC     EAX
        MOV     EDI,EDX
end;

function CompareChars(const Buffer1, Buffer2; Count: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI, EAX
        MOV     EDI, EDX
        XOR     EAX, EAX
        REPE    CMPSB
        JB      @@1
        NEG     ECX
@@1:    SUB     EAX,ECX
        POP     EDI
        POP     ESI
end;

procedure ZeroMem(pBuff: Pointer; Count: Integer);
asm
        MOV     ECX,EDX
        SAR     ECX,2
        JS      @@exit
        PUSH    EDI
        MOV     EDI,EAX { Point EDI to destination      }
        XOR     EAX,EAX
        REP     STOSD   { Fill count DIV 4 dwords       }
        MOV     ECX,EDX
        AND     ECX,3
        REP     STOSB   { Fill count MOD 4 bytes        }
        POP     EDI
@@exit:
end;

{ 'Like' code is written by Wladimir Perepletchick }
{ Fido: 2:5037/10                                  }
function Like(const Source, Template: String): Boolean;
const
  SpecialChars: TCharSet = ['%', '*', '?', '_'];
var
 I, J, K, LTemplate, LSource: Integer;
begin
  Result := False;
  LTemplate := Length(Template);
  LSource := Length(Source);
  I := 1; J := 1;
  while (I <= LTemplate) and (J <= LSource) do
  begin
    case Template[I] of
      '?', '_':
        ;
      '*', '%':
        begin
          while (Template[I] in SpecialChars) and (I <= LTemplate) do Inc(I);
          if I > LTemplate then
            Result := True
          else
            while J <= LSource do
            begin
              while (Source[J] <> Template[I]) and (J <= LSource) do Inc(J);
              if J > LSource then Break;
              K := 0;
              while (Source[J + K] = Template[I + K]) and
                    (J + K <= LSource) and (I + K <= LTemplate) and
                    (not (Template[I + K] in SpecialChars)) do Inc(K);
              if (Template[I + K] in SpecialChars) or (I + K > LTemplate) then
              begin
                Inc(I, K - 1);
                Inc(J, K - 1);
                Break;
              end;
              Inc(J, K);
            end;
            if J > LSource then Break;
        end;
      else
        if (Source[J] <> Template[I]) then Break;
    end;
    Inc(I); Inc(J);
    if (J > LSource) then
    begin
      K := 0;
      while (Template[I + K] in ['%', '*']) and (I + K <= LTemplate) do Inc(K);
      if (I + K > LTemplate) then Result := True;
    end;
  end;
end;

procedure AddDelimeted(var S: string; const SubStr, Delimeter: string);
begin
  if S <> '' then S := S + Delimeter;
  S := S + SubStr;
end;

function GetListString(Fmt: string; Strings: TStrings): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Strings.Count - 1 do
    Result := Result + Format(Fmt, [Strings[I]]);
end;

function ExtractDelimeted(const S, Delimeter: string; var Pos: Integer): string;
var
  Tmp: string;
  I: Integer;
begin
  Tmp := Copy(S, Pos, MaxInt);
  I := System.Pos(Delimeter, Tmp) - 1;
  if I >= 0 then
  begin
    Result := Trim(Copy(S, Pos, I));
    Pos := I + Length(Delimeter) + Pos;
  end else begin
    Result := Trim(Tmp);
    Pos := Length(S) + 1;
  end;
end;

function ExtractDelimetedWord(const S, Delimeter: string; Number: Integer; var Pos: Integer): string;
var
  Tmp: string;
begin
  while (Number > 0) and (Pos <= Length(S)) do
  begin
    Tmp := ExtractDelimeted(S, Delimeter, Pos);
    Dec(Number);
  end;
  if Number = 0 then Result := Tmp else Result := '';
end;

procedure GetDelimetedStrings(const S, Delimeter: string; List: TStrings);
var
  Pos: Integer;
begin
  Pos := 1;
  List.BeginUpdate;
  try
    while (Pos <= Length(S)) do
      List.Add(ExtractDelimeted(S, Delimeter, Pos));
  finally
    List.EndUpdate;
  end;
end;

function PosText(const SubStr, Source: string): Integer;
begin
  Result := Pos(AnsiUpperCase(SubStr), AnsiUpperCase(Source));
end;

function ReplaceStr(const S, Srch, Replace: string): string;
var
  I: Integer;
  Source: string;
begin
  Source := S;
  Result := '';
  repeat
    I := Pos(Srch, Source);
    if I > 0 then
    begin
      Result := Result + Copy(Source, 1, I - 1) + Replace;
      Source := Copy(Source, I + Length(Srch), MaxInt);
    end else
      Result := Result + Source;
  until I <= 0;
end;

function WordCount(const S: string; const WordDelims: TCharSet): Integer;
var
  SLen, I: Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);
  while I <= SLen do
  begin
    while (I <= SLen) and (S[I] in WordDelims) do Inc(I);
    if I <= SLen then Inc(Result);
    while (I <= SLen) and not(S[I] in WordDelims) do Inc(I);
  end;
end;

function WordPosition(const N: Integer; const S: string;
  const WordDelims: TCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do
  begin
    { skip over delimiters }
    while (I <= Length(S)) and (S[I] in WordDelims) do Inc(I);
    { if we're not beyond end of S, we're at the start of a word }
    if I <= Length(S) then Inc(Count);
    { if not finished, find the end of the current word }
    if Count <> N then
      while (I <= Length(S)) and not (S[I] in WordDelims) do Inc(I)
    else Result := I;
  end;
end;

function ExtractWord(N: Integer; const S: string;
  const WordDelims: TCharSet): string;
var
  I: Integer;
  Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(S)) and not(S[I] in WordDelims) do
    begin
      { add the I'th character to result }
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;

procedure WideCharToNames(Names: PChar; NameCount: Byte; var WideNames: TNames; var Size: Word);
var
  I, N: Integer;
  Ch: WideChar;
begin
  I := 0;
  N := 0;
  Size := 0;
  repeat
    repeat
      Ch := WideChar(Names[I]);
      WideNames[I] := Char(Ch);
      Inc(Size);
      Inc(I);
    until Char(Ch) = #0;
    Inc(N);
  until N = NameCount;
end;

function ForEachString(Strings: TStrings; const Separator, StringMacro, Macro: string): string;
var
  I, Pos: Integer;
  S, Tmp: string;
begin
  Result := '';
  for I := 0 to Strings.Count - 1 do
  begin
    if I > 0 then Result := Result + Separator;
    S := Strings[I];
    Tmp := Macro;
    Pos := System.Pos(StringMacro, Tmp);
    while Pos > 0 do
    begin
      Delete(Tmp, Pos, Length(StringMacro));
      Insert(S, Tmp, Pos);
      Pos := System.Pos(StringMacro, Tmp);
    end;
    Result := Result + Tmp;
  end;
end;

function ListAdd(var List: TList; Item: Pointer): Pointer;
begin
  if List = nil then List := TList.Create;
  List.Add(Item);
  Result := Item;
end;

procedure ListClear(var List: TList);
asm
        JMP     FreeObject
end;

function ListCount(List: TList): Integer;
begin
  if Assigned(List) then Result := List.Count else Result := 0;
end;

function ListDelete(var List: TList; Index: Integer): Pointer;
begin
  Result := ListItem(List, Index);
  List.Delete(Index);
  if List.Count = 0 then ListDestroy(List);
end;

procedure ListDestroy(var List: TList);
asm
        JMP     FreeObject
end;

procedure ListDestroyAll(var List: TList);
begin
  if Assigned(List) then
  begin
    ListDestroyObjects(List);
    FreeObject(List);
  end;
end;

procedure ListDestroyObjects(List: TList);
var
  I: Integer;
begin
  if Assigned(List) then
    with List do
      for I := Count - 1 downto 0 do
        TObject(List^[I]).Free;
end;

procedure ListDestroyObjectsAll(var List: TList);
asm
        JMP     ListDestroyAll
end;

procedure ListFreeMem(List: TList);
var
  I: Integer;
  P: Pointer;
begin
  if Assigned(List) then
    with List do
    begin
      for I := 0 to Count - 1 do
      begin
        P := List^[I];
        FreeMem(P);
      end;
      Clear;
    end;
end;

procedure ListFreeMemAll(var List: TList);
begin
  ListFreeMem(List);
  FreeObject(List);
end;

procedure ListSort(List: TList; Compare: TListSortCompare);
begin
  if Assigned(List) then List.Sort(Compare);
end;

procedure ListError(Index: Integer);
begin
{$IFDEF _D3_}
  TList.Error(SListIndexError, Index);
{$ELSE}
  raise EListError.CreateRes(SListIndexError);
{$ENDIF}
end;

function ListIndexOf(List: TList; Item: Pointer): Integer;
begin
  if Assigned(List) then
    with List do Result := FindInteger(Integer(Item), List^, Count) else Result := -1;
end;

procedure ListInsert(var List: TList; Index: Integer; Item: Pointer);
begin
  if not Assigned(List) then List := TList.Create;
  List.Insert(Index, Item);
end;

function ListItem(List: TList; Index: Integer): Pointer;
begin
  if Assigned(List) then
    Result := List[Index]
  else begin
    Result := nil;
    ListError(Index);
  end;
end;

function ListRemove(var List: TList; Item: Pointer): Pointer;
var
  I: Integer;
begin
  I := ListIndexOf(List, Item);
  if I >= 0 then
    Result := ListDelete(List, I) else
    Result := nil;
end;

function ListRemoveLast(var List: TList): Pointer;
begin
  if Assigned(List) and (List.Count > 0) then
    Result := ListRemove(List, List.Last) else
    Result := nil;
end;

procedure QuickSortList(AList: TList; DataCompare, DataExchange: Pointer;
  Compare: TCompareItems; AExchange: TExchangeItems);

  procedure DoQuickSortList(L, R: Integer);
  var
    I, J: Integer;
    P: Pointer;
  begin
    with AList do
    repeat
      I := L;
      J := R;
      P := List^[(L + R) shr 1];
      repeat
        if (L <> R) then
        begin
          while Compare(DataCompare, List^[I], P) < 0 do Inc(I);
          while Compare(DataCompare, List^[J], P) > 0 do Dec(J);
        end;
        if I <= J then
        begin
          if I < J then
          begin
            if Assigned(AExchange) then
              AExchange(DataExchange, I, J)
            else begin
              P := List^[I];
              List^[I] := List^[J];
              List^[J] := P;
            end;
          end;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then DoQuickSortList(L, J);
      L := I;
    until I >= R;
  end;

begin
  if Assigned(AList) then
    DoQuickSortList(0, AList.Count - 1);
end;

procedure ListAssign(var Dest: TList; Source: TList);
var
  Count: Integer;
begin
  Count := ListCount(Source);
  if Count > 0 then
  begin
    if not Assigned(Dest) then
      Dest := TList.Create;
    Dest.Count := Count;
    Move(Source.List^, Dest.List^, Count * SizeOf(Pointer));
  end else
    ListClear(Dest);
end;

function ListCheck(var List: TList): TList;
begin
  if not Assigned(List) then List := TList.Create;
  Result := List;
end;

procedure StringsAssignTo(List: TStrings; const Strings: Array of string);
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := Low(Strings) to High(Strings) do List.Add(Strings[I]);
  finally
    List.EndUpdate;
  end;
end;

procedure ArrayAssignTo(List: TStrings; var Strings: Array of string);
var
  I: Integer;
begin
  for I := Low(Strings) to High(Strings) do
    if I < List.Count then Strings[I] := List[I] else Strings[I] := '';
end;

function StringsHistoryInsertObject(List: TStrings; Index: Integer;
  const Value: string; AObject: TObject; MaxCount: Integer): Integer;
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    Result := List.IndexOf(Value);
    if Result >= 0 then
    begin
      List.Move(Result, Index);
      List.Objects[Index] := AObject;
    end else begin
      List.InsertObject(Index, Value, AObject);
      Result := Index;
    end;
    I := List.Count - 1;
    while I >= MaxCount do
    begin
      List.Delete(I);
      Dec(I);
    end;
    Result := Max(Result, I);
  finally
    List.EndUpdate;
  end;
end;

function StringsHistoryInsert(List: TStrings; Index: Integer;
  const Value: string; MaxCount: Integer): Integer;
begin
  Result := StringsHistoryInsertObject(List, Index, Value, nil, MaxCount);
end;

function StringsHistoryAddObject(List: TStrings; const Value: string;
  AObject: TObject; MaxCount: Integer): Integer;
begin
  Result := StringsHistoryInsertObject(List, List.Count, Value, AObject, MaxCount);
end;

function StringsHistoryAdd(List: TStrings; const Value: string; MaxCount: Integer): Integer;
begin
  Result := StringsHistoryInsertObject(List, List.Count, Value, nil, MaxCount);
end;

function IsClass(AClass: TClass; ParentClass: TClass): Boolean;
begin
  Result := (ParentClass = nil);
  if not Result then
    while (AClass <> nil) do
    begin
      if AClass = ParentClass then
      begin
        Result := True;
        Break;
      end;
      AClass := AClass.ClassParent;
    end;
end;

type
  TClassRegistrator = class(TObject)
    class procedure RegisterClass(Instance: TComponent);
  end;

  TComponentHack = class(TComponent)
  end;

  TSetNameHelper = class
    OldName: string;
    procedure SetName(Reader: TReader; Component: TComponent; var Name: string);
  end;

class procedure TClassRegistrator.RegisterClass(Instance: TComponent);
begin
  Classes.RegisterClass(TComponentClass(Instance.ClassType));
  TComponentHack(Instance).GetChildren(TClassRegistrator.RegisterClass {$IFDEF _D3_}, nil {$ENDIF});
end;

procedure TSetNameHelper.SetName(Reader: TReader; Component: TComponent; var Name: string);
begin
  Name := UniqueName(Component, OldName, Component.Owner);
end;

procedure RegisterComponent(Instance: TComponent);
begin
  TClassRegistrator.RegisterClass(Instance);
end;

procedure CopyProps(Src: TComponent; Dst: TComponent);
var
  F: TStream;
  Reader: TReader;
  Writer: TWriter;
  NameHelper: TSetNameHelper;
  FOwner: TComponent;
begin
  F := TMemoryStream.Create;
  try
    Writer := TWriter.Create(F, 1024);
    try
      if Assigned(Src.Owner) then
        FOwner := Src.Owner else
        FOwner := Dst.Owner;
      Writer.Root := FOwner;
      Writer.WriteComponent(Src);
    finally
      Writer.Free;
    end;
    RegisterComponent(Src);
    F.Position := 0;
    Reader := TReader.Create(F, 1024);
    try
      NameHelper := TSetNameHelper.Create;
      try
        NameHelper.OldName := Dst.Name;
        Reader.Root := FOwner;
        Reader.OnSetName := NameHelper.SetName;
        Reader.BeginReferences;
        try
          Reader.ReadComponent(Dst);
          Reader.FixupReferences;
        finally
          Reader.EndReferences;
        end;
      finally
        NameHelper.Free;
      end;
    finally
      Reader.Free;
    end;
  finally
    F.Free;
  end;
end;

function CreateCloneOwner(Src: TComponent; AOwner: TComponent): TComponent;
begin
  Result := TComponentClass(Src.ClassType).Create(AOwner);
  try
    CopyProps(Src, Result);
  except
    Result.Free;
    raise;
  end;
end;

function CreateClone(Src: TComponent): TComponent;
begin
  Result := CreateCloneOwner(Src, Src.Owner)
end;

function CreateComponentOwnerNeeded(var Instance; ComponentClass: TComponentClass;
  AOwner: TComponent): TComponent;
begin
  if not Assigned(Pointer(Instance)) then
    TComponent(Instance) := ComponentClass.Create(AOwner);

  Result := TComponent(Instance);
end;

function CreateComponentNeeded(var Instance; ComponentClass: TComponentClass): TComponent;
begin
  Result := CreateComponentOwnerNeeded(Instance, ComponentClass, nil);
end;

function CreateCloneOwnerNeeded(var Instance; Src: TComponent; AOwner: TComponent): TComponent;
begin
  if not Assigned(Pointer(Instance)) then
    TComponent(Instance) := CreateCloneOwner(Src, AOwner);

  Result := TComponent(Instance)
end;

function CreateCloneNeeded(var Instance; Src: TComponent): TComponent;
begin
  Result := CreateCloneOwnerNeeded(Instance, Src, Src.Owner);
end;

procedure FreeObject(var Obj); assembler;
asm
  MOV     ECX, [EAX]
  TEST    ECX, ECX
  JE      @@exit
  PUSH    EAX
  MOV     EAX, ECX
  MOV     ECX, [EAX]
  MOV     DL,1
  CALL    dword ptr [ECX - 4] { vtDestroy }
  POP     EAX
  XOR     ECX, ECX
  MOV     [EAX], ECX
@@exit:
end;

procedure CopyMethodProps(Src, Dst: TObject);
var
  I, Count: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  Method: TMethod;
begin
  if not Dst.InheritsFrom(Src.ClassType) then Exit;
  PropList := nil;
  Count := GetTypeData(Src.ClassInfo)^.PropCount;
  try
    ReAllocMem(PropList, Count * SizeOf(Pointer));
    GetPropInfos(Src.ClassInfo, PropList);
    for I := 0 to Count - 1 do
    begin
      PropInfo := PropList^[I];
      if PropInfo^.PropType^.Kind = tkMethod then
      begin
        Method := GetMethodProp(Src, PropInfo);
        SetMethodProp(Dst, PropInfo, Method);
      end;
    end;
  finally
    ReAllocMem(PropList, 0);
  end;
end;

function UniqueName(Instance: TComponent; const Name: string; Owner: TComponent): string;
var
  I: Integer;
  Tmp: TComponent;
begin
  I := 0;
  Result := Name;
  if Assigned(Owner) then
  begin
    Tmp := Owner.FindComponent(Result);
    if Assigned(Tmp) and (Tmp <> Instance) then
    while (Tmp <> nil) do
    begin
      Result := Format('%s_%d', [Name, I]);
      Inc(I);
      Tmp := Owner.FindComponent(Result);
    end;
  end else begin
    Result := '';
    if Assigned(FindGlobalComponent) then
    begin
      Result := Name;
      while FindGlobalComponent(Result) <> nil do
      begin
        Result := Format('%s_%d', [Name, I]);
        Inc(I);
      end;
    end;
  end;
end;

procedure WriteAndRead(Src: TComponent; Dst: TComponent);
begin
  CopyProps(Src, Dst);
end;

procedure ForComponents(AComponents: array of TComponent;
  Callback: TComponentCallback; Data: Pointer);
var
  I: Integer;
begin
  for I := Low(AComponents) to High(AComponents) do
    Callback(AComponents[I], Data);
end;

procedure ForEachComponent(Instance: TComponent;
  ComponentClass: TComponentClass; Callback: TComponentCallback; Data: Pointer; Children: Boolean);
var
  I: Integer;
  C: TComponent;
begin
  for I := 0 to Instance.ComponentCount - 1 do
  begin
    C := Instance.Components[I];
    if C is ComponentClass then Callback(C, Data);
    if Children then ForEachComponent(C, ComponentClass, Callback, Data, Children);
  end;
end;

function AppPathFileName(FileName: TFileName): TFileName;
begin
  if ExtractFilePath(FileName) = '' then
    Result := ExtractFilePath(AppFileName) + FileName else
    Result := FileName;
end;

procedure WriteBoolean(const IniFile, IniSection, Ident: string; const Value: Boolean; UseRegistry: Boolean);
begin
  WriteInteger(IniFile, IniSection, Ident, Integer(Value), UseRegistry);
end;

procedure WriteFloat(const IniFile, IniSection, Ident: string; const Value: Double; UseRegistry: Boolean);
begin
  WriteString(IniFile, IniSection, Ident, FloatToStr(Value), UseRegistry);
end;

procedure WriteInteger(const IniFile, IniSection, Ident: string; const Value: Integer; UseRegistry: Boolean);
begin
  WriteString(IniFile, IniSection, Ident, IntToStr(Value), UseRegistry);
end;

procedure WriteString(const IniFile, IniSection, Ident, Value: string; UseRegistry: Boolean);
  function WriteIni: TObject;
  begin
    Result := TIniFile.Create(AppPathFileName(IniFile) + IniFileExt);
    with TIniFile(Result) do
      WriteString(IniSection, Ident, Value);
  end;

  function WriteReg: TObject;
  begin
    Result := TRegIniFile.Create(IniFile);
    with TRegIniFile(Result) do
      WriteString(IniSection, Ident, Value);
  end;
var
  Obj: TObject;
begin
  Obj := nil;
  try
    if UseRegistry then
      Obj := WriteReg else
      Obj := WriteIni;
  finally
    Obj.Free;
  end;
end;

function ReadBoolean(const IniFile, IniSection, Ident: string; const DefValue: Boolean; UseRegistry: Boolean): Boolean;
begin
  Result := Boolean(ReadInteger(IniFile, IniSection, Ident, Integer(DefValue), UseRegistry));
end;

function ReadFloat(const IniFile, IniSection, Ident: string; const DefValue: Double; UseRegistry: Boolean): Double;
var
  S: string;
begin
  S := ReadString(IniFile, IniSection, Ident, FloatToStr(DefValue), UseRegistry);
  try
    Result := StrToFloat(S);
  except
    Result := DefValue;
  end;
end;

function ReadInteger(const IniFile, IniSection, Ident: string; const DefValue: Integer; UseRegistry: Boolean): Integer;
begin
  try
    Result := StrToInt(ReadString(IniFile, IniSection, Ident, IntToStr(DefValue), UseRegistry));
  except
    Result := DefValue;
  end;
end;

function ReadString(const IniFile, IniSection, Ident, DefValue: string; UseRegistry: Boolean): string;
var
  S: string;
  function ReadIni: TObject;
  begin
    Result := TIniFile.Create(AppPathFileName(IniFile) + IniFileExt);
    with TIniFile(Result) do
      S := ReadString(IniSection, Ident, DefValue);
  end;

  function ReadReg: TObject;
  begin
    Result := TRegIniFile.Create(IniFile);
    with TRegIniFile(Result) do
      S := ReadString(IniSection, Ident, DefValue);
  end;
var
  Obj: TObject;
begin
  Obj := nil;
  try
    if UseRegistry then
      Obj := ReadReg else
      Obj := ReadIni;
    Result := S;
  finally
    Obj.Free;
  end;
end;

procedure AppWriteLog(const Msg: string);
begin
  WriteLog(Copy(AppFileName, 1, Length(AppFileName) - 3) + 'log', Msg);
end;

procedure WriteLog(const FileName: TFileName; const Msg: string);
var
  Tmp: string;
begin
  Tmp := DateTimeToStr(Now) + ' ' + Msg;
  EnterCriticalSection(FLogLock);
  try
    if Assigned(WriteLogProc) then
      WriteLogProc(FileName, Tmp)
    else
      DefaultWriteLog(FileName, Tmp);
  finally
    LeaveCriticalSection(FLogLock);
  end
end;

procedure DefaultWriteLog(const FileName: TFileName; const Msg: string);
begin
  try
    with TWinFileStream.Create(FileName, [famWrite], [fsmRead], fcmOpenAlways,
      FILE_ATTRIBUTE_NORMAL or FILE_FLAG_WRITE_THROUGH, nil, 0) do
    try
      Position := Size;
      WriteBuffer(PChar(Msg + #13#10)^, Length(Msg) + 2);
    finally
      Free;
    end;
  except end;
end;

function GetTempFileName(const Path: TFileName): TFileName;
var
  TmpFileName: TMaxPath;
begin
  Windows.GetTempFileName(PChar(Path), PChar('tmp'), 0, TmpFileName);
  Result := StrPas(TmpFileName);
end;

function BackupFile(const FileName: TFileName): Boolean;
var
  BakFileName: string;
begin
  Result := True;
  if FileExists(FileName) then
  begin
    BakFileName := ChangeFileExt(FileName, '.bak');
    if FileExists(BakFileName) then
      Result := DeleteFile(BakFileName);
    if Result then
      Result := RenameFile(FileName, BakFileName);
  end;
end;

procedure CheckBackupFile(const FileName: TFileName);
var
  BakFileName: string;
begin
  if FileExists(FileName) then
  begin
    BakFileName := ChangeFileExt(FileName, '.bak');
    if FileExists(BakFileName) then
      CheckDeleteFile(BakFileName);
    CheckRenameFile(FileName, BakFileName);
  end;
end;

procedure CheckDeleteFile(const FileName: TFileName);
begin
  CheckCondition(DeleteFile(FileName), EFileOperation, FmtLoadStr(SCannotDeleteFile, [FileName]));
end;

procedure CheckRenameFile(const OldName, NewName: TFileName);
begin
  CheckCondition(RenameFile(OldName, NewName), EFileOperation, FmtLoadStr(SCannotRenameFile, [OldName, NewName]));
end;

procedure LoadComponent(const FileName: string; Instance: TComponent);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Stream.ReadComponent(Instance);
  finally
    Stream.Free;
  end;
end;

procedure SaveStream(const FileName: string; Source: TStream);
var
  TmpFileName: string;
  Stream: TStream;
begin
  TmpFileName := GetTempFileName(ExtractFilePath(AppFileName));
  try
    Stream := TFileStream.Create(TmpFileName, fmCreate);
    try
      Stream.CopyFrom(Source, Source.Size - Source.Position);
    finally
      Stream.Free;
    end;
    CheckBackupFile(FileName);
    CheckRenameFile(TmpFileName, FileName);
  except
    DeleteFile(TmpFileName);
    raise;
  end;
end;

procedure SaveComponent(const FileName: string; Instance: TComponent);
var
  Stream: TStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.WriteComponent(Instance);
    Stream.Position := 0;
    SaveStream(FileName, Stream);
  finally
    Stream.Free;
  end;
end;

procedure GetFileNames(const Directory, FileMask: string; Attr: Integer; FileNames: TStrings);
var
  Status: Integer;
  SearchRec: TSearchRec;
  Mask: string;
begin
  with FileNames do
  begin
    BeginUpdate;
    try
      Clear;
      if Directory = '' then
        Mask := GetCurrentDir + '\' + FileMask else
        Mask := Directory + '\' + FileMask;

      Status := FindFirst(Mask, Attr, SearchRec);
      while Status = 0 do
      begin
        if (SearchRec.Attr and faDirectory) = 0 then
          Add(ExtractFilePath(Mask) + SearchRec.Name);
        Status := FindNext(SearchRec);
      end;
      FindClose(SearchRec);
    finally
      EndUpdate;
    end;
  end;
end;

function NvlInteger(const Value: Variant): Integer;
begin
  Result := 0;
  if not VarIsNull(Value) then
  try
    Result := VarAsType(Value, varInteger);
  except end;
end;

function NvlFloat(const Value: Variant): Double;
begin
  Result := 0;
  if not VarIsNull(Value) then
  try
    Result := VarAsType(Value, varDouble);
  except end;
end;

function NvlDateTime(const Value: Variant): TDateTime;
begin
  Result := 0;
  if not VarIsNull(Value) then
  try
    Result := VarAsType(Value, varDate);
  except end;
  if Result = 0 then Result := EncodeDate(1, 1, 1);
end;

function NvlString(const Value: Variant): string;
begin
  case TVarData(Value).VType of
    varOleStr, varString: Result := Value
  else
    Result := '';
  end;
end;

const
  VarTypesNumeric: array[0..5] of Integer = (varSmallInt, varInteger, varSingle, varDouble, varCurrency, varByte);
  VarTypesString:  array[0..1] of Integer = (varString, varOleStr);
  VarTypesNull:    array[0..0] of Integer = (varNull);
  VarTypesDate:    array[0..0] of Integer = (varDate);
  VarTypesBoolean: array[0..0] of Integer = (varBoolean);
  VarTypesEmpty:   array[0..0] of Integer = (varEmpty);

function VarComparable(const V1, V2: Variant): Boolean;
  function IsType(VarType: Integer; VarTypes: array of Integer): Boolean;
  var
    I: Integer;
  begin
    for I := Low(VarTypes) to High(VarTypes) do
    begin
      Result := VarType = VarTypes[I];
      if Result then Exit;
    end;
    Result := False;
  end;
var
  Type1, Type2: Integer;
  function IsBothType(VarTypes: array of Integer): Boolean;
  begin
    Result := IsType(Type1, VarTypes) and IsType(Type2, VarTypes);
  end;
begin
  Type1 := TVarData(V1).VType; Type2 := TVarData(V2).VType;
  Result := (
    IsBothType(VarTypesNumeric) or IsBothType(VarTypesString) or
    IsBothType(VarTypesNull) or IsBothType(VarTypesDate) or
    IsBothType(VarTypesBoolean) or IsBothType(VarTypesEmpty));
end;

function VarIsEqual(const V1, V2: Variant): Boolean;
begin
  Result := VarComparable(V1, V2) and (V1 = V2);
end;

function VarRecToVariant(VarRec: TVarRec): Variant;
begin
  with VarRec do case VType of
    vtInteger:    Result := VInteger;
    vtBoolean:    Result := VBoolean;
    vtChar:       Result := VChar;
    vtExtended:   Result := VExtended^;
    vtString:     Result := VString^;
    vtPChar:      Result := StrPas(VPChar);
    vtObject:     Result := VObject.ClassName;
    vtClass:      Result := VClass.ClassName;
    vtWideChar:   Result := VWideChar;
    vtPWideChar:  Result := VPWideChar^;
    vtAnsiString: Result := string(VAnsiString);
    vtCurrency:   Result := VCurrency^;
    vtVariant:    Result := VVariant^;
{$IFDEF _D3_}
    vtInterface:  Result := IUnknown(VInterface);
    vtWideString: Result := WideString(VWideString);
{$ENDIF}
  end;
end;

function VarArrayFromConst(const Args: array of const): Variant;
var
  I: Integer;
begin
  if High(Args) = 0 then
    Result := VarRecToVariant(Args[0])
  else begin
    Result := VarArrayCreate([0, High(Args)], varVariant);
    for I := 0 to High(Args) do
      Result[I] := VarRecToVariant(Args[I]);
  end;
end;

function VarArrayFromConstCast(const Args: array of const): Variant;
var
  I, Count: Integer;
begin
  Count := (High(Args) + 1) div 2;
  if Count <= 0 then
  begin
    Result := Null;
    Exit;
  end;
  if Count = 1 then
    Result := VarAsType(VarRecToVariant(Args[0]), Args[1].VInteger)
  else begin
    Result := VarArrayCreate([0, Count - 1], varVariant);
    for I := 0 to Count - 1 do
      Result[I] := VarAsType(VarRecToVariant(Args[I * 2]), Args[I * 2 + 1].VInteger)
  end;
end;

function VarArrayCast(const Values: Variant; Args: array of Integer): Variant;
var
  I, Count: Integer;
begin
  Result := Values;
  Count := (High(Args) - Low(Args) + 1) div 2;
  for I := 0 to Count - 1 do
    Result[Args[I * 2]] := VarAsType(Result[Args[I * 2]], Args[I * 2 + 1])
end;

function VarArrayOfPairs(const Args: array of const): Variant;
var
  Value: Variant;
  I, Count: Integer;
begin
  Count := (High(Args) - Low(Args) + 1) div 2;
  Result := VarArrayCreate([0, Count - 1], varVariant);
  for I := 0 to Count - 1 do
  begin
    Value := VarArrayCreate([0, 1], varVariant);
    Value[0] := VarRecToVariant(Args[I * 2]);
    Value[1] := VarRecToVariant(Args[I * 2 + 1]);
    Result[I] := Value;
  end;
end;

function VarArrayOfPairsCast(const Values: Variant; const Args: array of Integer): Variant;
var
  Value: Variant;
  I, Count: Integer;
begin
  Result := Values;
  Count := (High(Args) - Low(Args) + 1) div 2;
  for I := 0 to Count - 1 do
  begin
    Value := Result[Args[I * 2]];
    Value[1] := VarAsType(Value[1], Args[I * 2 + 1]);
    Result[Args[I * 2]] := Value;
  end;
end;

procedure StringsFromVarArray(const List: Variant; Strings: TStrings);
var
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    if VarIsArray(List) and (VarArrayDimCount(List) = 1) then
      for I := VarArrayLowBound(List, 1) to VarArrayHighBound(List, 1) do
        Strings.Add(List[I]);
  finally
    Strings.EndUpdate;
  end;
end;

function VarArrayFromStrings(Strings: TStrings): Variant;
var
  I: Integer;
begin
  Result := Null;
  if Strings.Count > 0 then
  begin
    Result := VarArrayCreate([0, Strings.Count - 1], varOleStr);
    for I := 0 to Strings.Count - 1 do Result[I] := VarAsType(Strings[I], varOleStr);
  end;
end;

procedure EnumStrings(List: TStrings; EnumProc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do EnumProc(List[I]);
end;

procedure EnumVarArray(const List: Variant; EnumProc: TGetStrProc);
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    StringsFromVarArray(List, S);
    EnumStrings(S, EnumProc);
  finally
    S.Free;
  end;
end;

function VarToDispatch(Instance: Variant): IDispatch;
begin
  if TVarData(Instance).VType = varDispatch then
    Result := IDispatch(TVarData(Instance).VDispatch)
  else if TVarData(Instance).VType = (varDispatch or varByRef) then
    Result := IDispatch(TVarData(Instance).VPointer^)
  else
    Result := nil;
end;

procedure WriteBufferAt(Stream: TStream; const Buff; Count: Integer; Position: Integer);
var
  OldPos: Integer;
begin
  OldPos := Stream.Position;
  Stream.Position := Position;
  Stream.WriteBuffer(Buff, Count);
  Stream.Position := OldPos;
end;

procedure SwapStrings(var Str1, Str2: string);
var
  Tmp: string;
begin
  Tmp := Str1; Str1 := Str2; Str2 := Tmp;
end;

procedure SwapInteger(var Value1, Value2: Integer);
var
  Tmp: Integer;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

function IsLeapYear(Year: Word): Boolean;
begin
  Result := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

function GetDayTable(Year: Word): PDayTable;
const
  DayTable1: TDayTable = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  DayTable2: TDayTable = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  DayTables: array[Boolean] of PDayTable = (@DayTable1, @DayTable2);
begin
  Result := DayTables[IsLeapYear(Year)];
end;

procedure PreloadLibraries(const DLLs: array of PChar; Handles: PInstance);
var
  I: Integer;
  Handle: HINST;
begin
  for I := Low(DLLs) to High(DLLs) do
  begin
    Handle := LoadLibrary(DLLs[I]);
    Handles^ := Handle;
    Inc(LongInt(Handles), SizeOf(HINST));
  end;
end;

procedure UnloadLibraries(Handles: PInstance; Count: Integer);
var
  I: Integer;
  Handle: HINST;
begin
  for I := 0 to Count - 1 do
  begin
    Handle := Handles^;
    if Handle <> 0 then
      FreeLibrary(Handle);
    Inc(LongInt(Handles), SizeOf(HINST));
  end;
end;

function RegisterServer(const DLLName: string): Boolean;
var
  Handle: THandle;
  DllRegServ: procedure;
begin
  Result := False;
  try
    Handle := LoadLibrary(PChar(DLLName));
    if Handle >= HINSTANCE_ERROR then
    try
      DllRegServ := GetProcAddress(Handle, 'DllRegisterServer');
      if Assigned(DllRegServ) then
      begin
        DllRegServ;
        Result := True;
      end;
    finally
      FreeLibrary(Handle);
    end;
  except
    Result := False;
  end;
end;

{ Copied from RxLib }
{ SetVirtualMethodAddress procedure. Destroy destructor has index 0,
  first user defined virtual method has index 1. }

type
  PPointer = ^Pointer;

function GetVirtualMethodAddress(AClass: TClass; AIndex: Integer): Pointer;
var
  Table: PPointer;
begin
  Table := PPointer(AClass);
  Inc(Table, AIndex - 1);
  Result := Table^;
end;

function SetVirtualMethodAddress(AClass: TClass; AIndex: Integer;
  NewAddress: Pointer): Pointer;
const
  PageSize = SizeOf(Pointer);
var
  Table: PPointer;
  SaveFlag: DWORD;
begin
  Table := PPointer(AClass);
  Inc(Table, AIndex - 1);
  Result := Table^;
  if VirtualProtect(Table, PageSize, PAGE_EXECUTE_READWRITE, @SaveFlag) then
  try
    Table^ := NewAddress;
  finally
    VirtualProtect(Table, PageSize, SaveFlag, @SaveFlag);
  end;
end;

function FindVirtualMethodIndex(AClass: TClass; MethodAddr: Pointer): Integer;
begin
  Result := 0;
  repeat
    Inc(Result);
  until (GetVirtualMethodAddress(AClass, Result) = MethodAddr);
end;

{$IFDEF _D3_}
function ResStr(const Ident: string): string;
begin
  Result := Ident;
end;
{$ELSE}
function ResStr(Ident: Integer): string;
begin
  Result := LoadStr(Ident);
end;
{$ENDIF}

{$IFDEF _D3_}
type
  PStoredResStringData = ^TStoredResStringData;
  TStoredResStringData = record
    Addr: Pointer;
    Data: TResStringRec;
  end;

var
  StoredResStrings: TList = nil;

function FindStoredResStringData(P: PResStringRec): PStoredResStringData;
var
  I: Integer;
begin
  if Assigned(StoredResStrings) then
  begin
    for I := 0 to StoredResStrings.Count - 1 do
    begin
      Result := StoredResStrings[I];
      if Result^.Addr = P then Exit;
    end;
  end;
  Result := nil;
end;

procedure StoreResString(P: PResStringRec);
var
  Tmp: PStoredResStringData;
begin
  if not Assigned(FindStoredResStringData(P)) then
  begin
    GetMem(Tmp, SizeOf(TStoredResStringData));
    try
      Tmp^.Addr := P;
      Move(P^, Tmp^.Data, SizeOf(TResStringRec));
    except
      FreeMem(Tmp);
      raise;
    end;
    ListAdd(StoredResStrings, Tmp);
  end;
end;

procedure RestoreResString(P: PResStringRec);
var
  Tmp: PStoredResStringData;
begin
  Tmp := FindStoredResStringData(P);
  if Assigned(Tmp) then
  begin
    CopyResString(@Tmp^.Data, P, False);
    ListRemove(StoredResStrings, Tmp);
    FreeMem(Tmp);
  end;
end;

procedure FreeStoredResStrings;
var
  I: Integer;
  Tmp: PStoredResStringData;
begin
  if Assigned(StoredResStrings) then
  begin
    for I := StoredResStrings.Count - 1 downto 0 do
    begin
      Tmp := StoredResStrings[I];
      RestoreResString(Tmp^.Addr);
    end;
  end;
end;

procedure CopyResString(Source, Dest: PResStringRec; Store: Boolean);
var
  SaveFlag: Integer;
begin
  if VirtualProtect(Dest, SizeOf(TResStringRec), PAGE_READWRITE, @SaveFlag) then
  try
    if Store then StoreResString(Dest);
    Move(Source^, Dest^, SizeOf(TResStringRec));
  finally
    VirtualProtect(Dest, SizeOf(TResStringRec), SaveFlag, @SaveFlag);
  end;
end;
{$ENDIF}

function Win32Description: string;
var
  Ver: TOsVersionInfo;
  S: string;
begin
  Ver.dwOSVersionInfoSize := SizeOf(Ver);
  GetVersionEx(Ver);
  with Ver do
  begin
    case dwPlatformId of
      VER_PLATFORM_WIN32_WINDOWS:
        S := 'Windows';
      VER_PLATFORM_WIN32_NT:
        S := 'Windows NT'
    else
        S := 'Win32s';
    end;
    Result := Format('%s %d.%.2d (%s)', [S, dwMajorVersion, dwMinorVersion, szCSDVersion]);
  end;
end;

procedure GetEnvironment(Strings: TStrings);
var
  P: PChar;
  I: Integer;
begin
  P := GetEnvironmentStrings;
  try
    Strings.BeginUpdate;
    try
      Strings.Clear;
      while P^ <> #0 do
      begin
        I := Pos('=', P);
        Strings.Values[Copy(P, 1, I - 1)] := Copy(P, I + 1, 255);
        P := P + StrLen(P) + 1;
      end;
    finally
      Strings.EndUpdate;
    end;
  finally
    FreeEnvironmentStrings(P);
  end;
end;

function GetEnvironmentVariable(const Variable: string): string;
var
  Buff: array[0..1023] of Char;
begin
  Buff[0] := #0;
  Windows.GetEnvironmentVariable(PChar(Variable), Buff, SizeOf(Buff));
  Result := StrPas(Buff);
end;

function IsMainThread: Boolean;
begin
 Result := GetCurrentThreadID = MainThreadID;
end;

function GetProcessHandle(ProcessID: DWORD): THandle;
begin
  Result := OpenProcess(PROCESS_ALL_ACCESS, True, ProcessID);
end;

function GetPropType(PropInfo: PPropInfo): PTypeInfo;
begin
{$IFDEF _D3_}
  Result := PropInfo^.PropType^;
{$ELSE}
  Result := PropInfo^.PropType;
{$ENDIF}
end;

procedure GetPropInfoList(List: TList; Instance: TObject; Filter: TTypeKinds);
var
  Count: Integer;
begin
  List.Clear;
  try
    Count := GetPropList(Instance.ClassInfo, Filter, nil);
    List.Count := Count;
    GetPropList(Instance.ClassInfo, Filter, PPropList(List.List));
  except
    List.Clear;
    raise;
  end;
end;

initialization
  InitializeCriticalSection(FLogLock);
  AppFileName := ParamStr(0);

finalization
  DeleteCriticalSection(FLogLock);
{$IFDEF _D3_}
  FreeStoredResStrings;
{$ENDIF}

end.
