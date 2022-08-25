unit magclasses ;

{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}

{ various classes
Updated by Angus Robertson, Magenta Systems Ltd, England, 6th June 2017
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd    }

{
25 July 2005 - Angus - added  AddSorted and Sorted
1 Aug 2005 - added CompareGTMem
8 Aug 2008 - made compatible with Delphi 2009
6 May 2010 - added TStringPairs, basic stringlist with key and value
11 June 2013 - added TMagStringBuild for D2007 and earlier
29 April 2014 - added Get, Put, Delete, GetText, SaveToFile, more Find methods to TStringPairs
29 July 2015 - added Capacity TStringPairs
31 Dec 2015  - corrected TStringPairs wrote LFCR instead of CRLF
4 Feb 2016   - messing with TStringPairs
6 June 2017  - added TExRegistry from Colin Wilson, updated to add signature
                 for unicode W2K/XP registry export file and other fixes.
             - TExRegistry extends TRegistry with ReadStrings and WriteStrings
               methods to handle REG_MULTI_SZ value with multiple null delimited
               strings, and methods to export and import REG files.
              - added MagRegReadStrings and MagRegWriteStrings



Pednding - support UTF16 REG files on Delphi 2007 and earlier

}

interface

uses
    Windows, Sysutils, Classes, Registry ;

type

// descendent of TList added a Find function using binary search identical to sorting

    TFindList = class(TList)
    private
      { Private declarations }
    protected
      { Protected declarations }
    public
      { Public declarations }
      Sorted: boolean ;
      function AddSorted(const Item2: Pointer; Compare: TListSortCompare): Integer; virtual;
      function Find(const Item2: Pointer; Compare: TListSortCompare;
                                    var index: longint): Boolean; virtual;
  end;

// StringList with keys and values (stored as objects)

    TStringPairs = class(TObject)
    protected
        FList : TStringList;
        function GetCapacity: Integer;
        procedure SetCapacity (Value: integer);
    public
        constructor Create;
        destructor Destroy; override;
        procedure Add(const Key, Value: String); Overload;
        procedure Delete(Index: Integer);
        function  Find(const Key: String; var Value: String) : Boolean; Overload;
        function  Find(const Key: String; var Index: Integer): Boolean; Overload;
        function  Find(const Key: String; var Index: Integer; var Value: String): Boolean; Overload;
        function  Get(Index: Integer; var Key: String): string;
        function  Put(Index: Integer; const Value: String): boolean;
        function  Count: Integer;
        function  GetText: string;
        procedure SaveToFile(const FileName: string);
        procedure Clear;
        property Capacity: Integer read GetCapacity write SetCapacity;
    end;

    TStringPairsObject = class(TObject)
    public
        Value : String;
        constructor Create(const Data: String);
    end;

// TMagStringBuild Class

     TMagStringBuild = class(TObject)
     private
       FBuffMax: integer ;
       FBuffSize: integer ;
       FIndex: integer ;
       FBuffer: array of Byte;
       FCharSize: integer ;
       procedure ExpandBuffer;
     public
       constructor Create (ABufferSize: integer = 4096) ;
       destructor Destroy; override ;
       procedure Append (const AString: string) ;
       procedure AppendW (const AString: widestring) ;
       procedure AppendLine (const AString: string) ;
       procedure AppendLineW (const AString: widestring) ;
       procedure Clear ;
       function ToString: string ; {$IFDEF UNICODE} override ;{$ENDIF}
       function ToStringW: widestring ;
       procedure Capacity (ABufferSize: integer) ;
       property Len: integer read FIndex ;
     end;


resourcestring
    errUnableToConnect = 'Unable to connect to the registry on %s (%d)';

type
  TRegRootRec = record
    key : HKEY;
    name : string
  end;

const
  NO_ROOT_KEYS = 7;
  RootKeys : array [0..NO_ROOT_KEYS - 1] of TRegRootRec = (
    (key : HKEY_CLASSES_ROOT;     name : 'HKEY_CLASSES_ROOT'),
    (key : HKEY_CURRENT_USER;     name : 'HKEY_CURRENT_USER'),
    (key : HKEY_LOCAL_MACHINE;    name : 'HKEY_LOCAL_MACHINE'),
    (key : HKEY_USERS;            name : 'HKEY_USERS'),
    (key : HKEY_PERFORMANCE_DATA; name : 'HKEY_PERFORMANCE_DATA'),
    (key : HKEY_CURRENT_CONFIG;   name : 'HKEY_CURRENT_CONFIG'),
    (key : HKEY_DYN_DATA;         name : 'HKEY_DYN_DATA'));

  RegSig4 = 'REGEDIT4' ;  // W9x and NT4
  RegSig5 = 'Windows Registry Editor Version 5.00' ;   // W2K and WXP signature - UNICODE !!

type

    TRegWalkProc = procedure (const keyName, valueName : string; dataType : DWORD; data : pointer; DataLen : Integer) of object;

    TRegSrchParam = (rsKeys, rsValues, rsData);
    TRegSrchParams = set of TRegSrchParam;

    TRegSrchNode = class
      fValueNames : TStringList;
      fKeyNames : TStringList;
      fCurrentKey : string;
      fPath: string;
      fValueIDX, fKeyIDX : Integer;
      fRegRoot : HKEY;
      constructor Create (ARegRoot : HKEY; const APath : string);
      destructor Destroy; override;

      procedure LoadKeyNames;
      procedure LoadValueNames;
    end;

    TExRegistry = class (TRegistry)
    private
      fSaveServer : string;
      fExportStrings : TStrings;
      fLastExportKey : string;
      fSearchParams : TRegSrchParams;
      fSearchString : string;
      fSearchStack : TList;
      fMatchWholeString : boolean;
      fCancelSearch : boolean;
      fLocalRoot : HKEY;
      fValuesSize : Integer;
      procedure ExportProc (const keyName, valueName : string; dataType : DWORD; data : pointer; DataLen : Integer);
      procedure ValuesSizeProc (const keyName, valueName : string; dataType : DWORD; data : pointer; DataLen : Integer);
      procedure ClearSearchStack;
    public
      destructor Destroy; override;
      procedure SetRoot (root : HKey; const server : string);
      procedure CopyValueFromReg (const valueName : string; otherReg : TExRegistry; deleteSource : boolean);
      procedure CopyKeyFromReg (const keyName : string; otherReg : TExRegistry; deleteSource : boolean);
      function GetValueType (const valueName : string) : DWORD;
      procedure ReadStrings (const valueName : string; strings : TStrings);
      procedure WriteStrings (const valueName : string; strings : TStrings);
      procedure ExportKey (const fileName : string);
      procedure ImportRegFile (const fileName : string);
      procedure WriteTypedBinaryData (const valueName : string; tp : Integer; var data; size : Integer);
      procedure Walk (walkProc : TRegWalkProc; valuesRequired : boolean);
      function FindFirst (const data : string; params : TRegSrchParams; MatchWholeString : boolean; var retPath, retValue : string) : boolean;
      function FindNext (var retPath, retValue : string) : boolean;
      procedure CancelSearch;
      property SearchString : string read fSearchString;
      procedure GetValuesSize (var size : Integer);
    end;

    EExRegistryException = class (ERegistryException)
    private
        fCode: Integer;
        function GetError : string;
    public
      constructor CreateLastError (const st : string);
      constructor Create (code : DWORD; const st : string);
      property Code : Integer read fCode;
    end;

    function MagRegReadStrings(aKey: HKEY; const Path, Value: String; Strings: TStrings): integer ;
    function MagRegWriteStrings(aKey: HKEY; const Path, Value: String; Strings: TStrings): boolean ;
    function CompareGTMem (P1, P2: Pointer; Length: Integer): Integer ;

implementation

// compare two memory buffers, used for sorting
// ideally ASM SysUtils.CompareMem should be modified to return less or greater

function CompareGTMem (P1, P2: Pointer; Length: Integer): Integer ;
var
    I: Integer;
    PC1, PC2: PAnsiChar; // 8 Aug 2008
begin
    result := 0 ;   // equals
    if Length <= 0 then exit ;
    PC1 := P1 ;
    PC2 := P2 ;
    for I := 1 to Length do
    begin
        if (PC1^ <> PC2^) then
        begin
            if (PC1^ < PC2^) then
                result := -1   // less than
            else
                result := 1 ;  // greater than
            exit ;
        end ;
        Inc (PC1) ;
        Inc (PC2) ;
    end;
end;


// descendent of TList, adding sorted, works on sorted list
function TFindList.AddSorted(const Item2: Pointer; Compare: TListSortCompare): Integer;
begin
    if not Sorted then
        Result := Count
    else
    begin
       if Find (Item2, Compare, Result) then exit ;
    end ;
    Insert (Result, Item2) ;
end;

// adding binary FIND works on sorted list

function TFindList.Find(const Item2: Pointer; Compare: TListSortCompare;
                                            var index: longint): Boolean;
var
    l, h, i, c: longint;
begin
    Result := False;
    index := 0 ;
    if (List = nil) or (Count = 0) then exit ;
    l := 0;
    h := Count - 1;
    while l <= h do
    begin
        i := (l + h) shr 1;  // binary shifting
        c := Compare (List[i], Item2) ;
        if c < 0 then
            l := i + 1
        else
        begin
            h := i - 1;
            if c = 0 then
            begin
                Result := True;
                l := i;
            end;
        end;
    end;
    index := l;
end;

// TStringPairs

procedure TStringPairs.Add(const Key, Value: String);
begin
    if NOT Assigned(Flist) then exit;
    FList.AddObject(Key, TStringPairsObject.Create(Value));
end;

procedure TStringPairs.Clear;
var
    I : Integer;
begin
    if NOT Assigned(Flist) then exit;
    for I := FList.Count - 1 downto 0 do FList.Objects[I].Free;
    FList.Clear;
end;

procedure TStringPairs.Delete(Index: Integer);
begin
    if NOT Assigned(Flist) then exit;
    if (Index < 0) or (Index >= FList.Count) then exit;
    FList.Objects[Index].Free;
    Flist.Delete (Index);
end;

function TStringPairs.Find(const Key: String; var Value: String): Boolean;
var
    Index: Integer;
begin
//    Index := FList.IndexOf(Key);
//    Result := (Index >= 0);
    Result := Find(Key, Index);
    if Result then Value := TStringPairsObject(FList.Objects[Index]).Value;
end;

function TStringPairs.Find(const Key: String; var Index: Integer; var Value: String): Boolean;
begin
    Result := Find(Key, Index);
    if Result then Value := TStringPairsObject(FList.Objects[Index]).Value;
end;

function TStringPairs.Find(const Key: String; var Index: Integer): Boolean;
begin
    Index := -1 ; // Feb 2016
    if not Assigned(Flist) then
    begin
        Result := false;
        exit;
    end;
    Result := FList.Find(Key, Index);
end;

function TStringPairs.Get(Index: Integer; var Key: String): string;
begin
    Result := '';
    Key := '';
    if NOT Assigned(Flist) then exit;
    if (Index < 0) or (Index >= FList.Count) then exit;
    Key := Flist [Index];
    Result := TStringPairsObject(FList.Objects[Index]).Value;
end;

function TStringPairs.GetText: string;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S, LB, Key: string;
begin
    Result := '';
    if NOT Assigned(Flist) then exit;
    Count := FList.Count;
    Size := 0;
    LB := #13#10; // 31 Dec 2015 was LFCR
    for I := 0 to Count - 1 do Inc(Size, Length(Get(I, Key)) + Length(LB));
    SetString(Result, nil, Size);
    P := Pointer(Result);
    for I := 0 to Count - 1 do
    begin
        S := Get(I, Key);
        L := Length(S);
        if L <> 0 then
        begin
            System.Move(Pointer(S)^, P^, L);
            Inc(P, L);
        end;
        L := Length(LB);
        if L <> 0 then
        begin
            System.Move(Pointer(LB)^, P^, L);
            Inc(P, L);
        end;
    end;
end;

function TStringPairs.Put(Index: Integer; const Value: String): boolean;
begin
    Result := False ;
    if not Assigned(Flist) then exit;
    if Index >= FList.Count then exit;
    TStringPairsObject(FList.Objects[Index]).Value := Value ;
    Result := True;
end;

procedure TStringPairs.SaveToFile(const FileName: string);
var
    S: string;
    Stream: TStream;
begin
    if NOT Assigned(Flist) then exit;
    S := GetText;
    Stream := TFileStream.Create(FileName, fmCreate);
    try
        Stream.WriteBuffer(Pointer(S)^, Length(S));
    finally
        Stream.Free;
    end;
end;

function TStringPairs.GetCapacity: integer;
begin
    Result := 0 ;
    if not Assigned(Flist) then exit;
    Result := FList.Capacity ;
end;

procedure TStringPairs.SetCapacity (Value: integer);
begin
    if not Assigned(Flist) then exit;
    FList.Capacity := Value ;
end;

constructor TStringPairs.Create;
begin
    inherited Create;
    FList := TStringList.Create;
    FList.CaseSensitive := false;
    FList.Sorted := true;
    Flist.Duplicates := dupIgnore ;
end;

destructor TStringPairs.Destroy;
begin
    Clear;
    if Assigned(FList) then
    begin
        FList.Free;
        FList := nil;
    end;
    inherited Destroy;
end;

constructor TStringPairsObject.Create(const Data: String);
begin
    Value := Data;
end;

function TStringPairs.Count: Integer;
begin
    if NOT Assigned(Flist) then
        Result := 0
    else
        Result := FList.Count;
end;

// TMagStringBuild Class

constructor TMagStringBuild.Create (ABufferSize: integer = 4096) ;
begin
    inherited Create;
    Capacity (ABufferSize) ;
    FIndex := 0 ;
    FCharSize := SizeOf (Char) ;
end;

procedure TMagStringBuild.Capacity (ABufferSize: integer) ;
begin
    if ABufferSize <= 0 then ABufferSize := 1024 ;
    if ABufferSize < FBuffSize then exit ;  // not smaller
    if ABufferSize <= FIndex then exit ;    // sanity check
    FBuffSize := ABufferSize ;
    FBuffMax := FBuffSize - 1 ;
    SetLength (FBuffer, FBuffSize) ;
end;

procedure TMagStringBuild.ExpandBuffer ;
begin
    FBuffSize := FBuffSize shl 1 ;
    Capacity (FBuffSize) ;
end;

destructor TMagStringBuild.Destroy;
begin
    SetLength (FBuffer, 0) ;
    inherited Destroy;
end;

procedure TMagStringBuild.Append (const AString: string);
var
    Len : integer;
begin
    Len := length (AString) * FCharSize ;
    if Len + FIndex >= FBuffMax then ExpandBuffer ;
    Move (AString [1], FBuffer [FIndex], Len) ;
    inc (FIndex, Len) ;
end;

procedure TMagStringBuild.AppendW (const AString: widestring);
var
    Len : integer;
begin
    FCharSize := 2 ;
    Len := length (AString) * FCharSize ;
    if Len + FIndex >= FBuffMax then ExpandBuffer ;
    Move (AString [1], FBuffer [FIndex], Len) ;
    inc (FIndex, Len) ;
end;

procedure TMagStringBuild.AppendLine (const AString: string);
begin
    Append (AString) ;
    Append (#13#10) ;
end;

procedure TMagStringBuild.AppendLineW (const AString: widestring);
begin
    AppendW (AString) ;
    AppendW (#13#10) ;
end;

function TMagStringBuild.ToString: string;
begin
    if FCharSize <> SizeOf (Char) then
    begin
        result := 'Need WideString Result' ;
        exit ;
    end;
    SetLength (result, FIndex div FCharSize) ;
    Move (FBuffer [0], result [1], FIndex) ;
end;

function TMagStringBuild.ToStringW: widestring;
begin
    if FCharSize <> 2 then
    begin
        result := 'Need AnsiString Result' ;
        exit ;
    end;
    SetLength (result, FIndex div FCharSize) ;
    Move (FBuffer [0], result [1], FIndex) ;
end;

procedure TMagStringBuild.Clear;
begin
    FIndex := 0;
end;

{ TExRegistry }

function MyRootKeyName (key : HKEY) : string;
var
  i : Integer;
begin
  result := '';
  for i := 0 to NO_ROOT_KEYS - 1 do
    if RootKeys [i].key = key then
    begin
      result := RootKeys [i].name;
      break
    end
end;

function MyRootKeyVal (const st : string) : HKEY;
var
  i : Integer;
begin
  result := $ffffffff;
  for i := 0 to NO_ROOT_KEYS - 1 do
    if RootKeys [i].name = st then
    begin
      result := RootKeys [i].key;
      break
    end
end;


procedure TExRegistry.CancelSearch;
begin
  fCancelSearch := True;
end;

procedure TExRegistry.ClearSearchStack;
var
  i : Integer;
begin
  if Assigned (fSearchStack) then
  begin
    for i := 0 to fSearchStack.Count - 1 do
      TRegSrchNode (fSearchStack [i]).Free;
    fSearchStack.Free;
    fSearchStack := Nil
  end
end;

procedure TExRegistry.CopyKeyFromReg(const keyName: string;
  otherReg: TExRegistry; deleteSource : boolean);
var
  i : Integer;
  values : TStringList;
  sourceReg : TExRegistry;
  destReg : TExRegistry;
begin
  sourceReg := TExRegistry.Create;
  destReg := TExRegistry.Create;
  values := TStringList.Create;
  try
    sourceReg.RootKey := otherReg.CurrentKey;
    if deleteSource then
      sourceReg.OpenKey (keyName, False)
    else
      sourceReg.OpenKeyReadOnly (keyName);
    sourceReg.GetValueNames (values);

    destReg.RootKey := CurrentKey;
    if destReg.OpenKey (keyName, True) then
    begin
      for i := 0 to values.Count - 1 do
        destReg.CopyValueFromReg (values [i], sourceReg, deleteSource);

      sourceReg.GetKeyNames (values);
      for i := 0 to values.Count - 1 do
        destReg.CopyKeyFromReg (values [i], sourceReg, deleteSource);

      if DeleteSource then
        if not otherReg.DeleteKey (keyName) then
          Raise ERegistryException.Create ('Unable to delete moved key')
    end
    else
      raise ERegistryException.Create ('Unable to open destination');
  finally
    values.Free;
    destReg.Free;
    sourceReg.Free
  end
end;

procedure TExRegistry.CopyValueFromReg(const valueName: string;
  otherReg: TExRegistry; deleteSource : boolean);
var
  buffer : PByte;
  BufSize : DWORD;
  DataType : DWORD;
begin
  BufSize := 65536;
  GetMem (buffer, BufSize);
  try
    DataType := REG_NONE;

    SetLastError (RegQueryValueEx(otherReg.CurrentKey, PChar(valueName), nil, @DataType, Buffer,
      @BufSize));

     if GetLastError <> ERROR_SUCCESS then
      raise EExRegistryException.CreateLastError ('Unable to copy value');

    SetLastError (RegSetValueEx (CurrentKey, PChar (valueName), 0, DataType, buffer, BufSize));
    if GetLastError <> ERROR_SUCCESS then
      raise EExRegistryException.CreateLastError ('Unable to copy value');

    if deleteSource then
      if not otherReg.DeleteValue (valueName) then
        raise ERegistryException.Create ('Unable to delete moved value')
  finally
    FreeMem (buffer)
  end
end;


destructor TExRegistry.Destroy;
begin
  ClearSearchStack;
  inherited Destroy
end;

procedure TExRegistry.ExportKey(const fileName: string);
begin
  fExportStrings := TStringList.Create;
  if Win32MajorVersion = 4 then
      fExportStrings.Add (RegSig4)
  else
      fExportStrings.Add (RegSig5) ;
  try
    fLastExportKey := '';
    Walk (ExportProc, True);
    fExportStrings.Add ('');
  finally
{$IFNDEF UNICODE}
    fExportStrings.SaveToFile (fileName);
    // pending - need to convert to widestring and write it using file I/O
{$ELSE}
    fExportStrings.SaveToFile (fileName, TEncoding.Unicode);
{$ENDIF}
    fExportStrings.Free;
  end
end;

procedure TExRegistry.ExportProc(const keyName, valueName: string;
  dataType: DWORD; data: pointer; DataLen: Integer);
var
  st : string;
  st1 : string;
  j : Integer;
  localRoot : HKey;

  function MakeCStringConst (s : string) : string;
  var
    i : Integer;
  begin
    result := '';
    for i := 1 to Length (s) do
    begin
      if (s [i] = '\') or (s [i] = '"') then
        result := result + '\';
      result := result + s [i]
    end
  end;

begin
  localRoot := fLocalRoot;
  if localRoot = 0 then
    localRoot := RootKey;

  if fLastExportKey <> keyName then
  begin
    fExportStrings.Add ('');
    fExportStrings.Add (Format ('[%s\%s]', [MyRootKeyName (localRoot), keyName]));

    fLastExportKey := keyName;
  end;

  if dataLen <> 0 then
  begin
    if valueName = '' then
      st := '@='
    else
      st := Format ('"%s"=', [MakeCStringConst (valueName)]);

    case dataType of
      REG_DWORD :
      begin
        st1 := LowerCase (Format ('%8.8x', [PDWORD (data)^]));
        st := st + format ('dword:%s', [st1])
      end;

      REG_SZ    :
        begin
          PChar (data) [dataLen] := #0;
          st := st + format ('"%s"', [MakeCStringConst (PChar (data))]);
        end;

      else
      begin
        if dataType = REG_BINARY then
          st := st + 'hex:'
        else
          st := st + format ('hex(%d):', [dataType]);
        for j := 0 to dataLen - 1 do
        begin
          st1 := LowerCase (format ('%02.2x', [Byte (PChar (data) [j])]));
          if j < dataLen - 1 then
            st1 := st1 + ',';

          if Length (st) + Length (st1) >= 77 then
          begin
            fExportStrings.Add (st + st1 + '\');
            st := '  ';
          end
          else
            st := st + st1;
        end
      end
    end;
    fExportStrings.Add (st);
  end
end;

function TExRegistry.FindFirst(const data: string; params: TRegSrchParams; MatchWholeString : boolean;
  var retPath, retValue: string): boolean;
var
  path, nPath, keyName : string;
  p : Integer;
  n : TRegSrchNode;
begin
  ClearSearchStack;

  fSearchStack := TList.Create;
  path := currentPath;


  nPath := '';
  repeat
    p := Pos ('\', path);
    if p > 0 then
    begin
      nPath := nPath + '\' + Copy (path, 1, p - 1);
      path := Copy (path, p + 1, MaxInt);
      n := TRegSrchNode.Create (RootKey, nPath);
      n.LoadKeyNames;
      p := Pos ('\', path);
      if p > 0 then
        keyName := Copy (path, 1, p - 1)
      else
        keyName := path;

      n.fKeyIDX := n.fKeyNames.IndexOf (keyName);

      fSearchStack.Add (n);
    end
  until p = 0;

  n := TRegSrchNode.Create (RootKey, nPath + '\' + path);
  fSearchStack.Add (n);

  fSearchString := UpperCase (data);
  fSearchParams := params;
  fMatchWholeString := MatchWholeString;
  result := FindNext (retPath, retValue);
end;

function TExRegistry.FindNext(var retPath, retValue: string): boolean;
var
  n : TRegSrchNode;
  found : boolean;
  k : string;
  msg : TMsg;
begin
  found := False;
  fCancelSearch := False;
  while (not found) and (not fCancelSearch) and (fSearchStack.Count > 0) do
  begin
    while PeekMessage (msg, 0, 0, 0, PM_REMOVE) do
    begin
      TranslateMessage (msg);
      DispatchMessage (msg)
    end;

    n := TRegSrchNode (fSearchStack [fSearchStack.Count - 1]);
    if rsValues in fSearchParams then
    begin
      n.LoadValueNames;
      with n do
        if fValueIdx < fValueNames.Count then
        repeat
          Inc (fValueIdx);
          if fValueIdx < fValueNames.Count then
          begin
            if fMatchWholeString then
              found := fSearchString = fValueNames [fValueIdx]
            else
              found := Pos (fSearchString, fValueNames [fValueIdx]) > 0
          end
        until fCancelSearch or found or (fValueIdx = fValueNames.Count)
    end;

    if not fCancelSearch and not found then
    begin
      n.LoadKeyNames;
      with n do
        if fKeyIdx < fKeyNames.Count then
        begin
          Inc (fKeyIdx);
          if fKeyIdx < fKeyNames.Count then
          begin

            if rsKeys in fSearchParams then
              if fMatchWholeString then
                found := fSearchString = fKeyNames [fKeyIdx]
              else
                found := Pos (fSearchString, fKeyNames [fKeyIdx]) > 0;

            if not found then
            begin
              if n.fPath = '\' then
                k := '\' + fKeyNames [fKeyIdx]
              else
                k := n.fPath + '\' + fKeyNames [fKeyIdx];

              fSearchStack.Add (TRegSrchNode.Create (RootKey, k));

              continue
            end
          end
      end
    end;

    if fCancelSearch then
      Break;

    if not found then
    begin
      n.Free;
      fSearchStack.Delete (fSearchStack.Count - 1)
    end
    else
    begin
      retPath := n.fPath;
      if n.fKeyIdx > -1 then
        retPath := retPath + '\' + n.fKeyNames [n.fKeyIdx];

      if rsValues in fSearchParams then
        if (n.fValueIdx > -1) and (n.fValueIdx < n.fValueNames.Count) then
          retValue := n.fValueNames [n.fValueIdx]
        else
          retValue := '';
    end
  end;
  result := found
end;

procedure TExRegistry.GetValuesSize(var size: Integer);
begin
  fValuesSize := 0;
  Walk (ValuesSizeProc, False);
  if fValuesSize = 0 then
    fValuesSize := -1;
  size := fValuesSize
end;

function TExRegistry.GetValueType(const valueName: string): DWORD;
var
  valueType : DWORD;
begin
  SetLastError (RegQueryValueEx (CurrentKey, PChar (valueName), Nil, @valueType, Nil, Nil));
  if GetLastError = ERROR_SUCCESS then
    result := valueType
  else
    raise EExRegistryException.CreateLastError ('Unable to get value type');
end;

procedure TExRegistry.ImportRegFile(const fileName: string);
var
  strings : TStrings;
  st : string;
  i : Integer;

  procedure SyntaxError;
  begin
    raise Exception.CreateFmt ('Syntax error in reg file %s at line %d', [fileName, i])
  end;

  procedure CreateNewKey;
  var
    s : string;
    p : Integer;
    r : HKEY;
  begin
    Delete (st, 1, 1);
    if st [Length (st)] <> ']' then
      SyntaxError;

    Delete (st, Length (st), 1);

    p := pos ('\', st);
    if p = 0 then
      SyntaxError;
    s := Copy (st, 1, p - 1);
    st := Copy (st, p + 1, MaxInt);

    if st = '' then
      SyntaxError;

    r := MyRootKeyVal (s);
    if r = $ffffffff then
      SyntaxError;

    SetRoot (r, fSaveServer);
    OpenKey ('\' + st, True)
  end;

  function GetCString (const st : string) : string;
  var
    i : Integer;
  begin
    result := '';
    i := 2;
    while i <= Length (st) - 1 do
    begin
      if st [i] = '\' then
        Inc (i);

      if i <= Length (st) - 1 then
        result := result + st [i];

      Inc (i)
    end
  end;

  function GetBinaryBuffer (const st : string) : string;
  var
    i : Integer;
    val : string;
  begin
    i := 1;
    result := '';
    while i <= Length (st) do
    begin
      if st [i] in ['0'..'9', 'a'..'f', 'A'..'F'] then
        val := val + st [i]
      else
      begin
        if val <> '' then
        begin
          result := result + chr (StrToInt ('$' + val));
          val := ''
        end
      end;

      Inc (i)
    end
  end;

  procedure CreateNewValue;
  var
    s : string;
    fn : string;
    p : Integer;
    tp : Integer;
    buf : string;
  begin
    if st [1] = '"' then
    begin
      Delete (st, 1, 1);
      p := Pos ('"', st);
      if p = 0 then
        SyntaxError;

      s := Copy (st, 1, p - 1);
      st := Copy (st, p + 1, MaxInt)
    end
    else
    begin
      Delete (st, 1, 1);
      s := ''
    end;

    st := TrimLeft (st);

    if st = '' then
      SyntaxError;

    if st [1] <> '=' then
      SyntaxError;

    Delete (st, 1, 1);

    st := TrimLeft (st);

    if st [1] = '"' then
      WriteString (s, GetCString (st))
    else
    begin
      p := 1;
      while (p <= Length (st)) and not (st [p] in [':', '(', ' ']) do
        Inc (p);

      fn := Copy (st, 1, p - 1);

      st := TrimLeft (Copy (st, p, MaxInt));

      if CompareText (fn, 'hex') = 0 then
      begin
        tp := 3;
        if st [1] = '(' then
        begin
          Delete (st, 1, 1);
          fn := '';
          p := 1;
          while (p <= Length (st)) and (st [p] <> ')') do
          begin
            fn := fn + st [p];
            Inc (p)
          end;

          tp := StrToInt (fn);
          st := Trim (Copy (st, p + 1, MaxInt));
        end;

        if st [1] <> ':' then
          SyntaxError;

        Delete (st, 1, 1);

        buf := GetBinaryBuffer (st);

        WriteTypedBinaryData (s, tp, PChar (buf)^, Length (buf));
      end
      else
        if CompareText (fn, 'dword') = 0 then
        begin
          if st [1] <> ':' then
            SyntaxError;

          Delete (st, 1, 1);
          WriteInteger (s, StrToInt ('$' + TrimLeft (st)))
        end
        else
          SyntaxError
    end
  end;

begin
  strings := TStringList.Create;
  try
{$IFNDEF UNICODE}
    strings.LoadFromFile (fileName);
    // pending - need to read it using file I/O and convert from widestring
{$ELSE}
    strings.LoadfromFile (fileName, TEncoding.Unicode);
{$ENDIF}

    while (strings.Count > 0) do
    begin
      st := Trim (strings [0]);
      if (st = '') or (st [1] = ';') then
        strings.Delete (0)
      else
        break
    end;

    if (strings [0] <> RegSig4) and (strings [0] <> RegSig5) then
      raise Exception.Create ('Bad file format.  Missing REGEDIT4 in first line.');

    i := 1;
    while i < strings.Count do
    begin
      st := Trim (strings [i]);

      if st <> '' then
        while st [Length (st)] = '\' do
        begin
          Inc (i);
          Delete (st, Length (st), 1);
          if i < strings.Count then
            st := st + strings [i]
          else
            break
        end;

      if (Length (st) > 0) and (st [1] <> ';') then
      begin
        case st [1] of
          '[' : CreateNewKey;
          '"' : CreateNewValue;
          '@' : CreateNewValue;
          else
            SyntaxError
        end
      end;

      Inc (i)
    end
  finally
    strings.Free
  end
end;

procedure TExRegistry.ReadStrings(const valueName: string;
  strings: TStrings);
var
  valueType : DWORD;
  valueLen : DWORD;
  p, buffer : PChar;
begin
  strings.Clear;
  SetLastError (RegQueryValueEx (CurrentKey, PChar (valueName), Nil, @valueType, Nil, @valueLen));
  if GetLastError = ERROR_SUCCESS then
    if valueType = REG_MULTI_SZ then
    begin
      GetMem (buffer, valueLen);
      try
        RegQueryValueEx (CurrentKey, PChar (valueName), Nil, Nil, PBYTE (buffer), @valueLen);
        p := buffer;
        while p^ <> #0 do
        begin
          strings.Add (p);
          Inc (p, lstrlen (p) + 1)
        end
      finally
        FreeMem (buffer)
      end
    end
    else
      raise ERegistryException.Create ('String list expected')
  else
    raise EExRegistryException.CreateLastError ('Unable read MULTI_SZ value')
end;

procedure TExRegistry.SetRoot(root: HKey; const server: string);
begin
  fSaveServer := server;
  RootKey := root;
  fLocalRoot := root;
  if server <> '' then
    if not RegistryConnect ('\\' + server) then
      Raise Exception.CreateFmt (errUnableToConnect, [server, GetLastError])
end;

procedure TExRegistry.ValuesSizeProc(const keyName, valueName: string;
  dataType: DWORD; data: pointer; DataLen: Integer);
begin
  Inc (fValuesSize, DataLen);
end;

procedure TExRegistry.Walk(walkProc: TRegWalkProc; valuesRequired : boolean);

var
  defaultValue : array [0..256] of char;
  defaultValueLen : DWORD;

  valueName : array [0..256] of char;
  valueNameLen : DWORD;

  keyName : array [0..256] of char;

  cValues : DWORD;
  tp : DWORD;

  buffer : PChar;
  bufSize : DWORD;

  valueLen, maxValueLen : DWORD;
  keyLen : DWORD;

  procedure DoWalk (const pathName : string);
  var
    k : HKEY;
    err : Integer;
    i : Integer;
    cSubKeys : DWORD;
  begin
    err := RegOpenKeyEx (RootKey, PChar (pathName), 0, KEY_READ, k);
    if err = ERROR_SUCCESS then
    try
      defaultValueLen := sizeof (defaultValue);

      err := RegQueryInfoKey (k, defaultValue, @defaultValueLen, Nil, @cSubkeys, Nil, Nil, @cValues, nil, @maxValueLen, nil, nil);
      if (err <> ERROR_SUCCESS) and (err <> ERROR_ACCESS_DENIED) then
        raise EExRegistryException.Create (err, 'Unable to query key info');

      if err = ERROR_SUCCESS then
      begin
        if cValues > 0 then
        begin
          if maxValueLen > bufSize then
          begin
            bufSize := 65536 * ((maxValueLen + 65536) div 65536);
            ReallocMem (buffer, bufSize)
          end;

          for i := 0 to cValues - 1 do
          begin
            valueNameLen := sizeof (valueName);
            valueLen := maxValueLen;
            if valuesRequired then
              err := RegEnumValue (k, i, valueName, valueNameLen, Nil, @tp, PByte (buffer), @valueLen)
            else
              err := RegEnumValue (k, i, valueName, valueNameLen, Nil, @tp, Nil, @valueLen);
            if err <> ERROR_SUCCESS then
              raise EExRegistryException.Create (err, 'Unable to get value info');

            walkProc (pathName, valueName, tp, buffer, valueLen);
          end
        end
        else
          walkProc (pathName, '', 0, Nil, 0);

        for i := 0 to cSubkeys - 1 do
        begin
          keyLen := sizeof (keyName);
          RegEnumKey (k, i, keyName, keyLen);
          if pathName = '' then
            DoWalk (keyName)
          else
            DoWalk (pathName + '\' + keyName)
        end
      end
    finally
      RegCloseKey (k);
    end
//    else
//      if err <> 161 then
//        raise EExRegistryException.Create (err, 'Unable to open key')
  end;

begin
  bufSize := 65536;
  GetMem (buffer, bufSize);

  try
    if Assigned (walkProc) then
      DoWalk (CurrentPath);
  finally
    FreeMem (buffer)
  end
end;

procedure TExRegistry.WriteStrings(const valueName: string;
  strings: TStrings);
var
  p, buffer : PChar;
  i : Integer;
  size : DWORD;
begin
  size := 0;
  for i := 0 to strings.Count - 1 do
    Inc (size, Length (strings [i]) + 1);
  Inc (size);
  GetMem (buffer, size);
  try
    p := buffer;
    for i := 0 to strings.count - 1 do
    begin
      lstrcpy (p, PChar (strings [i]));
      Inc (p, lstrlen (p) + 1)
    end;
    p^ := #0;
    SetLastError (RegSetValueEx (CurrentKey, PChar (valueName), 0, REG_MULTI_SZ, buffer, size));
    if GetLastError <> ERROR_SUCCESS then
      raise EExRegistryException.CreateLastError ('Unable to write MULTI_SZ value');
  finally
    FreeMem (buffer)
  end
end;

procedure TExRegistry.WriteTypedBinaryData(const valueName: string;
  tp: Integer; var data; size: Integer);
begin
  if RegSetValueEx (CurrentKey, PChar(valueName), 0, tp, @data, size) <> ERROR_SUCCESS then
    raise ERegistryException.CreateFmt('Unable to set registry data for %s', [valueName]);
end;

{ EExRegistryException }

constructor EExRegistryException.Create(code: DWORD; const st: string);
begin
  fCode := code;
  inherited Create (GetError + ':' + st);
end;

constructor EExRegistryException.CreateLastError(const st: string);
begin
  fCode := GetLastError;
  inherited Create (GetError + ':' + st);
end;

function EExRegistryException.GetError: string;
var
  msg : string;

  function GetErrorMessage (code : Integer) : string;
  var
    hErrLib : THandle;
    msg : PChar;
    flags : Integer;

    function MAKELANGID (p, s : word) : Integer;
    begin
      result := (s shl 10) or p
    end;

  begin
    hErrLib := LoadLibraryEx ('netmsg.dll', 0, LOAD_LIBRARY_AS_DATAFILE);

    try

      flags := FORMAT_MESSAGE_ALLOCATE_BUFFER or
               FORMAT_MESSAGE_IGNORE_INSERTS or
               FORMAT_MESSAGE_FROM_SYSTEM;

      if hErrLib <> 0 then
        flags := flags or FORMAT_MESSAGE_FROM_HMODULE;

      if FormatMessage (flags, pointer (hErrLib), code,
                        MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
                        PChar (@msg), 0, Nil) <> 0 then
        try
          result := msg;

        finally
          LocalFree (Integer (msg));
        end

    finally
      if hErrLib <> 0 then
        FreeLibrary (hErrLib)
    end
  end;

begin
  msg := GetErrorMessage (fCode);
  if msg = '' then
    result := Format ('Error %d', [fCode])
  else
    result := Format ('Error %d : %s', [fCode, msg]) ;
  result := trim (result) ; // angus   
end;

{ TRegSrchNode }

constructor TRegSrchNode.Create (ARegRoot : HKEY; const APath : string);
begin
  fRegRoot := ARegRoot;
  fValueIDX := -1;
  fKeyIdx := -1;
  fPath := APath
end;

destructor TRegSrchNode.Destroy;
begin
  fValueNames.Free;
  fKeyNames.Free;
  inherited Destroy
end;

procedure TRegSrchNode.LoadKeyNames;
var
  r : TExRegistry;
  i : Integer;
begin
  if not Assigned (fKeyNames) then
  begin
    fKeyNames := TStringList.Create;
    r := TExRegistry.Create;
    try
      r.RootKey := fRegRoot;
      r.OpenKey (fPath, False);
      r.GetKeyNames (fKeyNames);
    finally
      r.Free
    end;
    
    for i := 0 to fKeyNames.Count - 1 do
      fKeyNames [i] := UpperCase (fKeyNames [i]);
  end
end;

procedure TRegSrchNode.LoadValueNames;
var
  r : TExRegistry;
  i : Integer;
begin
  if not Assigned (fValueNames) then
  begin
    fValueNames := TStringList.Create;
    r := TExRegistry.Create;
    try
      r.RootKey := fRegRoot;
      r.OpenKey (fPath, False);
      r.GetValueNames (fValueNames);
    finally
      r.Free
    end;

    for i := 0 to fValueNames.Count - 1 do
      fValueNames [i] := UpperCase (fValueNames [i]);
  end
end;

function MagRegReadStrings(aKey: HKEY; const Path, Value: String; Strings: TStrings): integer ;
var
    aRegistry : TExRegistry;
begin
    aRegistry := TExRegistry.Create;
    if NOT Assigned (Strings) then Strings := TStringList.Create ;
    try
        try
            Strings.Clear ;
            with aRegistry do
            begin
                RootKey := aKey ;
                Access := KEY_QUERY_VALUE ;   // allows HLM for non-administrators
                OpenKey (Path, False) ;
                if ValueExists (Value) then ReadStrings (Value, Strings) ;
                CloseKey ;
            end;
        except
        end ;
    finally
        aRegistry.Free;
        result := Strings.Count ;
    end;
end;


function MagRegWriteStrings(aKey: HKEY; const Path, Value: String; Strings: TStrings): boolean ;
var
    aRegistry : TExRegistry;
begin
    Result := False;
    aRegistry := TExRegistry.Create;
    if NOT Assigned (Strings) then Strings := TStringList.Create ;
    try
        try
            with aRegistry do
            begin
                RootKey := aKey ;
                Access := KEY_WRITE ;   //requires administrator rights
                OpenKey (Path, True) ;
                WriteStrings (Value, Strings) ;
                CloseKey ;
                Result := True;
            end;
        except
        end ;
    finally
        aRegistry.Free;
    end;
end;


end.
