unit magclasses ;

{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}

{ various classes
Updated by Angus Robertson, Magenta Systems Ltd, England, 4th Fe 2016
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd    }

// 25 July 2005 - Angus - added  AddSorted and Sorted
// 1 Aug 2005 - added CompareGTMem
// 8 Aug 2008 - made compatible with Delphi 2009
// 6 May 2010 - added TStringPairs, basic stringlist with key and value
// 11 June 2013 - added TMagStringBuild for D2007 and earlier
// 29 April 2014 - added Get, Put, Delete, GetText, SaveToFile, more Find methods to TStringPairs
// 29 July 2015 - added Capacity TStringPairs
// 31 Dec 2015  - corrected TStringPairs wrote LFCR instead of CRLF
// 4 Feb 2016   - messing with TStringPairs 

interface

uses
  Classes ;

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

end.
