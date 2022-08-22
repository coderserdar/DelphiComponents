unit EncryptBase;

{$I TinyDB.INC}
{$WRITEABLECONST ON}

interface

uses Classes, Windows, SysUtils, TinyDB, HashBase, Hash_SHA;

const {ErrorCode's for EEncryptException}
  EncErrGeneric        = 0;  {generic Error}
  EncErrInvalidKey     = 1;  {Decode Key is not correct}
  EncErrInvalidKeySize = 2;  {Size of the Key is too large}
  EncErrNotInitialized = 3;  {Methods Init() or InitKey() were not called}

type
  EEncryptException = class(Exception)
  public
    ErrorCode: Integer;
  end;

  TEncryptClass = class of TEncrypt;

  PEncryptRec = ^TEncryptRec;
  TEncryptRec = packed record
    case Integer of
      0: (X: array[0..7] of Byte);
      1: (A, B: LongWord);
  end;

  TEncProgressEvent = procedure(Percent: Integer) of object;

  TEncrypt = class(TPersistent)
  private
    FMode: TEncryptMode;
    FHash: THash;
    FHashClass: THashClass;
    FKeySize: Integer;
    FBufSize: Integer;
    FUserSize: Integer;
    FBuffer: Pointer;
    FVector: Pointer;
    FFeedback: Pointer;
    FUser: Pointer;
    FFlags: Integer;
    function GetHash: THash;
    procedure SetHashClass(Value: THashClass);
    procedure InternalCodeStream(Source, Dest: TStream; DataSize: Integer; Encode: Boolean);
    procedure InternalCodeFile(const Source, Dest: string; Encode: Boolean);
  protected
    function GetFlag(Index: Integer): Boolean;
    procedure SetFlag(Index: Integer; Value: Boolean); virtual;
    {used in method Init()}
    procedure InitBegin(var Size: Integer);
    procedure InitEnd(IVector: Pointer);
    {must override}
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); virtual;
    class function TestVector: Pointer; virtual;

    {the encode function, must override}
    procedure Encode(Data: Pointer); virtual;
    {the decode function, must override}
    procedure Decode(Data: Pointer); virtual;
    {the individual Userdata}
    property User: Pointer read FUser;
    property UserSize: Integer read FUserSize;
    {the Key is set from InitKey() and the Hash.DigestKey^ include the encrypted Hash-Key}
    property HasHashKey: Boolean index 0 read GetFlag write SetFlag;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function MaxKeySize: Integer;
    {performs a Test of correct work}
    class function SelfTest: Boolean;
    {initialization form the Encrypt}
    procedure Init(const Key; Size: Integer; IVector: Pointer); virtual;
    procedure InitKey(const Key: AnsiString; IVector: Pointer);
    {reset the Feedbackregister with the actual IVector}
    procedure Done; virtual;
    {protect the security Data's, Feedback, Buffer, Vector etc.}
    procedure Protect; virtual;
    {en/decode a TStream, Source and Dest can be the same or Dest = nil
     when DataSize < 0 the Source.Size is used}
    procedure EncodeStream(const Source, Dest: TStream; DataSize: Integer);
    procedure DecodeStream(const Source, Dest: TStream; DataSize: Integer);
    {en/decode a File, Source and Dest can be the same File}
    procedure EncodeFile(const Source, Dest: string);
    procedure DecodeFile(const Source, Dest: string);
    {en/decode a Memory, Source and Dest can be the same}
    procedure EncodeBuffer(const Source; var Dest; DataSize: Integer);
    procedure DecodeBuffer(const Source; var Dest; DataSize: Integer);
    {en/decode a string}
    function EncodeString(const Source: AnsiString): AnsiString;
    function DecodeString(const Source: AnsiString): AnsiString;
    {the Encrypt Mode = cmXXX}
    property Mode: TEncryptMode read FMode write FMode;
    {the Current Hash-Object, to build a Digest from InitKey()}
    property Hash: THash read GetHash;
    {the Class of the Hash-Object}
    property HashClass: THashClass read FHashClass write SetHashClass;
    {the maximal KeySize and BufSize (Size of Feedback, Buffer and Vector}
    property KeySize: Integer read FKeySize;
    property BufSize: Integer read FBufSize;
    {when the Key is set with InitKey() = HasHashKey and IncludeHashKey = True then
     En/Decodefile & En/Decodestream read, write at first the encrypted Key}
    property IncludeHashKey: Boolean index 8 read GetFlag write SetFlag;
    {Init() was called}
    property Initialized: Boolean index 9 read GetFlag write SetFlag;
    {the actual IVector, BufSize Bytes long}
    property Vector: Pointer read FVector;
    {the Feedback register, BufSize Bytes long}
    property Feedback: Pointer read FFeedback;
  end;

{ TEncAlgo_Base }

  TEncAlgo_Base = class(TEncryptAlgo)
  private
    FEncObject: TEncrypt;
  protected
    procedure SetMode(Value: TEncryptMode); override;
    function GetMode: TEncryptMode; override;

    function GetEncryptObjectClass: TEncryptClass; virtual; abstract;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    procedure InitKey(const Key: string); override;
    procedure Done; override;

    procedure EncodeBuffer(const Source; var Dest; DataSize: Integer); override;
    procedure DecodeBuffer(const Source; var Dest; DataSize: Integer); override;
  end;

procedure RaiseEncryptException(const ErrorCode: Integer; const Msg: string);
function DefaultHashClass(NewHashClass: THashClass): THashClass;
procedure XORBuffers(I1, I2: Pointer; Size: Integer; Dest: Pointer); assembler;
procedure SHIFTBuffers(P, N: PByteArray; Size, Shift: Integer);
procedure INCBuffer(P: PByteArray; Size: Integer);
procedure DoProgress(Current, Maximal: Integer);

const
  CheckEncryptKeySize: Boolean = False;
  {set to True raises Exception when Size of the Key is too large, (Method Init())
   otherwise will truncate the Key, default mode is False}
  FDefaultHashClass : THashClass = THash_SHA;
  FProgress: TEncProgressEvent = nil;

implementation

resourcestring
  SInvalidKey       = 'Encryptionkey is invalid';
  SInvalidKeySize   = 'Length from Encryptionkey is invalid.'#13#10+
                      'Keysize for %s must be to %d-%d bytes';
  SNotInitialized   = '%s is not initialized call Init() or InitKey() before.';

const
  EncMaxBufSize     = 1024 * 4;  {Buffersize for File, Stream-Access}

type
  TCodeProc = procedure(const Source; var Dest; DataSize: Integer) of object;

procedure RaiseEncryptException(const ErrorCode: Integer; const Msg: string);
var
  E: EEncryptException;
begin
  E := EEncryptException.Create(Msg);
  E.ErrorCode := ErrorCode;
  raise E;
end;

function DefaultHashClass(NewHashClass: THashClass): THashClass;
begin
  Result := FDefaultHashClass;
  if NewHashClass <> nil then FDefaultHashClass := NewHashClass;
end;

{$IFDEF WIN64}
procedure XORBuffers(I1, I2: Pointer; Size: Integer; Dest: Pointer);
var
  i:integer;
  pI1, pI2, pDest: PByte;
begin
  pI1 := I1;
  pI2 := I2;
  pDest := Dest;
  for i := 0 to Size-1 do
  begin
    pDest^ := pI1^ xor pI2^;
    Inc(pI1);
    Inc(pI2);
    Inc(pDest);
  end;
end;
{$ELSE}
procedure XORBuffers(I1, I2: Pointer; Size: Integer; Dest: Pointer); assembler;
asm
       AND   ECX,ECX
       JZ    @@3
       PUSH  ESI
       PUSH  EDI
       MOV   ESI,EAX
       MOV   EDI,Dest
       TEST  ECX,1
       JZ    @@0
       DEC   ECX
       MOV   AL,[ESI + ECX]
       XOR   AL,[EDX + ECX]
       MOV   [EDI + ECX],AL
       AND   ECX,ECX
       JZ    @@2
@@0:   SHR   ECX,1
       TEST  ECX,1
       JZ    @@00
       DEC   ECX
       MOV   AX,[ESI + ECX * 2]
       XOR   AX,[EDX + ECX * 2]
       MOV   [EDI + ECX * 2],AX
@@00:  SHR   ECX,1
@@1:   DEC   ECX
       JL    @@2
       MOV   EAX,[ESI + ECX * 4]
       XOR   EAX,[EDX + ECX * 4]
       MOV   [EDI + ECX * 4],EAX
       JMP   @@1
@@2:   POP   EDI
       POP   ESI
@@3:
end;
{$ENDIF}

procedure SHIFTBuffers(P, N: PByteArray; Size, Shift: Integer);
var
  I,S: Integer;
begin
  if Shift >= 8 then
  begin
    S := Shift div 8;
    Move(P[S], P[0], Size - S);
    Move(N[0], P[Size-S], S);
  end else
    if Shift <= -8 then
    begin
      S := -Shift div 8;
      Move(P[0], P[S], Size - S);
      Move(N[0], P[0], S);
    end;
  Shift := Shift mod 8;
  if Shift > 0 then
  begin
    S := 8 - Shift;
    for I := Size-1 downto 1 do
      P[I] := (P[I] shl Shift) or (P[I-1] shr S);
    P[0] := (P[0] shl Shift) or (N[Size-1] shr S);
  end else
    if Shift < 0 then
    begin
      Shift := -Shift;
      S := 8 - Shift;
      for I := Size-1 downto 1 do
        P[I] := (P[I] shr Shift) or (P[I-1] shl S);
      P[0] := (P[0] shr Shift) or (N[Size-1] shl S);
    end;
end;

procedure INCBuffer(P: PByteArray; Size: Integer);
begin
  repeat
    Dec(Size);
    Inc(P[Size]);
  until (P[Size] <> 0) or (Size <= 1);
end;

procedure DoProgress(Current, Maximal: Integer);
begin
  if Assigned(FProgress) then
  begin
    if Maximal = 0 then
      FProgress(0)
    else
      FProgress(Round(Current / Maximal * 100));
  end;
end;

function TEncrypt.GetFlag(Index: Integer): Boolean;
begin
  Result := FFlags and (1 shl Index) <> 0;
end;

procedure TEncrypt.SetFlag(Index: Integer; Value: Boolean);
begin
  Index := 1 shl Index;
  if Value then FFlags := FFlags or Index
    else FFlags := FFlags and not Index;
end;

procedure TEncrypt.InitBegin(var Size: Integer);
begin
  Initialized := False;
  Protect;
  if Size < 0 then Size := 0;
  if Size > KeySize then
    if not CheckEncryptKeySize then Size := KeySize
      else RaiseEncryptException(EncErrInvalidKeySize, Format(SInvalidKeySize, [ClassName, 0, KeySize]));
end;

procedure TEncrypt.InitEnd(IVector: Pointer);
begin
  if IVector = nil then Encode(Vector)
    else Move(IVector^, Vector^, BufSize);
  Move(Vector^, Feedback^, BufSize);
  Initialized := True;
end;

class procedure TEncrypt.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 0;
  AKeySize := 0;
  AUserSize := 0;
end;

class function TEncrypt.TestVector: Pointer;
begin
  Result := GetTestVector;
end;

procedure TEncrypt.Encode(Data: Pointer);
begin
end;

procedure TEncrypt.Decode(Data: Pointer);
begin
end;

constructor TEncrypt.Create;
begin
  inherited Create;
  FHashClass := FDefaultHashClass;
  GetContext(FBufSize, FKeySize, FUserSize);
  GetMem(FVector, FBufSize);
  GetMem(FFeedback, FBufSize);
  GetMem(FBuffer, FBufSize);
  GetMem(FUser, FUserSize);
  Protect;
end;

destructor TEncrypt.Destroy;
begin
  Protect;
  ReallocMem(FVector, 0);
  ReallocMem(FFeedback, 0);
  ReallocMem(FBuffer, 0);
  ReallocMem(FUser, 0);
  FHash.Free;
  FHash := nil;
  inherited Destroy;
end;

class function TEncrypt.MaxKeySize: Integer;
var
  Dummy: Integer;
begin
  GetContext(Dummy, Result, Dummy);
end;

class function TEncrypt.SelfTest: Boolean;
var
  Data: array[0..63] of AnsiChar;
  Key: string;
  SaveKeyCheck: Boolean;
begin
  Result       := InitTestIsOk; {have anonyme modified the testvectors ?}
{we will use the ClassName as Key :-)}
  Key          := ClassName;
  SaveKeyCheck := CheckEncryptKeySize;
  with Self.Create do
  try
    CheckEncryptKeySize := False;
    Mode := emCTS;
    Init(PChar(Key)^, Length(Key), nil);
    EncodeBuffer(GetTestVector^, Data, 32);
    Result := Result and CompareMem(TestVector, @Data, 32);
    Done;
    DecodeBuffer(Data, Data, 32);
    Result := Result and CompareMem(GetTestVector, @Data, 32);
  finally
    CheckEncryptKeySize := SaveKeyCheck;
    Free;
  end;
  FillChar(Data, SizeOf(Data), 0);
end;

procedure TEncrypt.Init(const Key; Size: Integer; IVector: Pointer);
begin
end;

procedure TEncrypt.InitKey(const Key: AnsiString; IVector: Pointer);
var
  I: Integer;
begin
  Hash.Init;
  Hash.Calc(PAnsiChar(Key)^, Length(Key));
  Hash.Done;
  I := Hash.DigestKeySize;
  if I > FKeySize then I := FKeySize; //generaly will truncate to large Keys
  Init(Hash.DigestKey^, I, IVector);
  EncodeBuffer(Hash.DigestKey^, Hash.DigestKey^, Hash.DigestKeySize);
  Done;
  HasHashKey := True;
end;

procedure TEncrypt.Done;
begin
  Move(FVector^, FFeedback^, FBufSize);
end;

procedure TEncrypt.Protect;
begin
  HasHashKey := False;
  Initialized := False;
  FillChar(FVector^, FBufSize, $FF);
  FillChar(FFeedback^, FBufSize, $FF);
  FillChar(FBuffer^, FBufSize, 0);
  FillChar(FUser^, FUserSize, 0);
end;

function TEncrypt.GetHash: THash;
begin
  if FHash = nil then
  begin
    if FHashClass = nil then FHashClass := FDefaultHashClass;
    FHash := FHashClass.Create;
  end;
  Result := FHash;
end;

procedure TEncrypt.SetHashClass(Value: THashClass);
begin
  if Value <> FHashClass then
  begin
    FHash.Free;
    FHash := nil;
    FHashClass := Value;
    if FHashClass = nil then FHashClass := FDefaultHashClass; 
  end;
end;

procedure TEncrypt.InternalCodeStream(Source, Dest: TStream; DataSize: Integer; Encode: Boolean);
var
  Buf: PAnsiChar;
  Over: PAnsiChar;
  OverSize: Integer;
  SPos: Integer;
  DPos: Integer;
  Len: Integer;
  Proc: TCodeProc;
  Size: Integer;
begin
  if Source = nil then Exit;
  if Encode then Proc := EncodeBuffer else Proc := DecodeBuffer;
  if Dest = nil then Dest := Source;
  if DataSize < 0 then
  begin
    DataSize := Source.Size;
    Source.Position := 0;
  end;
  Buf := nil;
  Over := nil;
  OverSize := 0;
  Size := DataSize;
  DoProgress(0, Size);
  try
    Buf    := AllocMem(EncMaxBufSize);
    DPos   := Dest.Position;
    SPos   := Source.Position;
    if IncludeHashKey and HasHashKey then
      if Encode then
      begin
        if Source = Dest then
        begin
          OverSize := Hash.DigestKeySize;
          Over := AllocMem(OverSize);
          OverSize := Source.Read(Over^, OverSize);
          SPos := Source.Position;
        end;
        Dest.Position := DPos;
        Dest.Write(Hash.DigestKey^, Hash.DigestKeySize);
        DPos := Dest.Position;
      end else
      begin
        OverSize := Hash.DigestKeySize;
        OverSize := Source.Read(Buf^, OverSize);
        if not CompareMem(Buf, Hash.DigestKey, Hash.DigestKeySize) then
          RaiseEncryptException(EncErrInvalidKey, SInvalidKey);
        SPos := Source.Position;
//        Dec(DataSize, OverSize);
      end;
    while DataSize > 0 do
    begin
      Source.Position := SPos;
      Len := DataSize;
      if Len > EncMaxBufSize then Len := EncMaxBufSize;
      if Over <> nil then
      begin
        if Len < OverSize then
        begin
          Move(Over^, Buf^, Len);
          Move(PByteArray(Over)[Len], Over^, OverSize - Len);
          Dec(OverSize, Len);
          OverSize := Source.Read(PByteArray(Over)[OverSize], Len) + OverSize;
        end else
        begin
          Move(Over^, Buf^, OverSize);
          Dec(Len, OverSize);
          Len := Source.Read(PAnsiChar(Buf + OverSize)^, Len) + OverSize;
          OverSize := Source.Read(Over^, OverSize);
        end;
      end else Len := Source.Read(Buf^, Len);
      SPos := Source.Position;
      if Len <= 0 then Break;
      Proc(Buf^, Buf^, Len);
      Dest.Position := DPos;
      Dest.Write(Buf^, Len);
      DPos := Dest.Position;
      Dec(DataSize, Len);
      DoProgress(Size - DataSize, Size);
    end;
    if IncludeHashKey and HasHashKey and (Dest = Source) then
      if Encode and (Over <> nil) then
      begin
        while OverSize > 0 do
        begin
          Len := EncMaxBufSize;
          Move(Over^, Buf^, OverSize);
          Dec(Len, OverSize);
          Source.Position := SPos;
          Len := Source.Read(PAnsiChar(Buf + OverSize)^, Len) + OverSize;
          OverSize := Source.Read(Over^, OverSize);
          SPos := Source.Position;
          Source.Position := DPos;
          Source.Write(Buf^, Len);
          DPos := Source.Position;
        end;
      end else
        if not Encode then
        begin
          repeat
            Source.Position := SPos;
            Len := Source.Read(Buf^, EncMaxBufSize);
            SPos := Source.Position;
            Source.Position := DPos;
            Source.Write(Buf^, Len);
            DPos := Source.Position;
          until Len <= 0;
          Source.Size := Source.Position;
        end;
  finally
    DoProgress(0, 0);
    ReallocMem(Buf, 0);
    ReallocMem(Over, 0);
  end;
end;

procedure TEncrypt.InternalCodeFile(const Source, Dest: string; Encode: Boolean);
var
  S,D: TFileStream;
begin
  S := nil;
  D := nil;
  try
    if (AnsiCompareText(Source, Dest) <> 0) and (Trim(Dest) <> '') then
    begin
      S := TFileStream.Create(Source, fmOpenRead or fmShareDenyNone);
      if not FileExists(Dest) then D := TFileStream.Create(Dest, fmCreate)
        else D := TFileStream.Create(Dest, fmOpenReadWrite);
    end else
    begin
      S := TFileStream.Create(Source, fmOpenReadWrite);
      D := S;
    end;
    InternalCodeStream(S, D, -1, Encode);
  finally
    S.Free;
    if S <> D then
    begin
      D.Size := D.Position;
      D.Free;
    end;
  end;
end;

procedure TEncrypt.EncodeStream(const Source, Dest: TStream; DataSize: Integer);
begin
  InternalCodeStream(Source, Dest, DataSize, True);
end;

procedure TEncrypt.DecodeStream(const Source, Dest: TStream; DataSize: Integer);
begin
  InternalCodeStream(Source, Dest, DataSize, False);
end;

procedure TEncrypt.EncodeFile(const Source, Dest: string);
begin
  InternalCodeFile(Source, Dest, True);
end;

procedure TEncrypt.DecodeFile(const Source, Dest: string);
begin
  InternalCodeFile(Source, Dest, False);
end;

function TEncrypt.EncodeString(const Source: AnsiString): AnsiString;
begin
  SetLength(Result, Length(Source));
  EncodeBuffer(PAnsiChar(Source)^, PAnsiChar(Result)^, Length(Source));
end;

function TEncrypt.DecodeString(const Source: AnsiString): AnsiString;
begin
  SetLength(Result, Length(Source));
  DecodeBuffer(PAnsiChar(Source)^, PAnsiChar(Result)^, Length(Source));
end;

procedure TEncrypt.EncodeBuffer(const Source; var Dest; DataSize: Integer);
var
  S,D,F: PByte;
begin
  if not Initialized then
    RaiseEncryptException(EncErrNotInitialized, Format(SNotInitialized, [ClassName]));
  S := @Source;
  D := @Dest;
  case FMode of
    emECB:
      begin
        if S <> D then Move(S^, D^, DataSize);
        while DataSize >= FBufSize do
        begin
          Encode(D);
          Inc(D, FBufSize);
          Dec(DataSize, FBufSize);
        end;
        if DataSize > 0 then
        begin
          Move(D^, FBuffer^, DataSize);
          //Encode(FBuffer);
          Move(FBuffer^, D^, DataSize);
        end;
      end;
    emCTS:
      begin
        while DataSize >= FBufSize do
        begin
          XORBuffers(S, FFeedback, FBufSize, D);
          Encode(D);
          XORBuffers(D, FFeedback, FBufSize, FFeedback);
          Inc(S, FBufSize);
          Inc(D, FBufSize);
          Dec(DataSize, FBufSize);
        end;
        if DataSize > 0 then
        begin
          Move(FFeedback^, FBuffer^, FBufSize);
          Encode(FBuffer);
          XORBuffers(S, FBuffer, DataSize, D);
          XORBuffers(FBuffer, FFeedback, FBufSize, FFeedback);
        end;
      end;
    emCBC:
      begin
        F := FFeedback;
        while DataSize >= FBufSize do
        begin
          XORBuffers(S, F, FBufSize, D);
          Encode(D);
          F := D;
          Inc(S, FBufSize);
          Inc(D, FBufSize);
          Dec(DataSize, FBufSize);
        end;
        Move(F^, FFeedback^, FBufSize);
        if DataSize > 0 then
        begin
          Move(FFeedback^, FBuffer^, FBufSize);
          Encode(FBuffer);
          XORBuffers(S, FBuffer, DataSize, D);
        end;
      end;
    emCFB:
      while DataSize > 0 do
      begin
        Move(FFeedback^, FBuffer^, FBufSize);
        Encode(FBuffer);
        D^ := S^ xor PByte(FBuffer)^;
        Move(PByteArray(FFeedback)[1], FFeedback^, FBufSize-1);
        PByteArray(FFeedback)[FBufSize-1] := D^;
//        SHIFTBuffers(FFeedback, PByteArray(D), FBufSize, 8);
        Inc(D);
        Inc(S);
        Dec(DataSize);
      end;
    emOFB:
      while DataSize > 0 do
      begin
        Move(FFeedback^, FBuffer^, FBufSize);
        Encode(FBuffer);
        D^ := S^ xor PByte(FBuffer)^;
        Move(PByteArray(FFeedback)[1], FFeedback^, FBufSize-1);
        PByteArray(FFeedback)[FBufSize-1] := PByte(FBuffer)^;
//        SHIFTBuffers(FFeedback, PByteArray(D), FBufSize, 8);
        Inc(D);
        Inc(S);
        Dec(DataSize);
      end;
  end;
  FillChar(FBuffer^, FBufSize, 0);
end;

procedure TEncrypt.DecodeBuffer(const Source; var Dest; DataSize: Integer);
var
  S,D,F,B: PByte;
begin
  if not Initialized then
    RaiseEncryptException(EncErrNotInitialized, Format(SNotInitialized, [ClassName]));
  S := @Source;
  D := @Dest;
  case FMode of
    emECB:
      begin
        if S <> D then Move(S^, D^, DataSize);
        while DataSize >= FBufSize do
        begin
          Decode(D);
          Inc(D, FBufSize);
          Dec(DataSize, FBufSize);
        end;
        if DataSize > 0 then
        begin
          Move(D^, FBuffer^, DataSize);
          //Encode(FBuffer);
          Move(FBuffer^, D^, DataSize);
        end;
      end;
    emCTS:
      begin
        if S <> D then Move(S^, D^, DataSize);
        F := FFeedback;
        B := FBuffer;
        while DataSize >= FBufSize do
        begin
          XORBuffers(D, F, FBufSize, B);
          Decode(D);
          XORBuffers(D, F, FBufSize, D);
          S := B;
          B := F;
          F := S;
          Inc(D, FBufSize);
          Dec(DataSize, FBufSize);
        end;
        if F <> FFeedback then Move(F^, FFeedback^, FBufSize);
        if DataSize > 0 then
        begin
          Move(FFeedback^, FBuffer^, FBufSize);
          Encode(FBuffer);
          XORBuffers(FBuffer, D, DataSize, D);
          XORBuffers(FBuffer, FFeedback, FBufSize, FFeedback);
        end;
      end;
    emCBC:
      begin
        if S <> D then Move(S^, D^, DataSize);
        F := FFeedback;
        B := FBuffer;
        while DataSize >= FBufSize do
        begin
          Move(D^, B^, FBufSize);
          Decode(D);
          XORBuffers(F, D, FBufSize, D);
          S := B;
          B := F;
          F := S;
          Inc(D, FBufSize);
          Dec(DataSize, FBufSize);
        end;
        if F <> FFeedback then Move(F^, FFeedback^, FBufSize);
        if DataSize > 0 then
        begin
          Move(FFeedback^, FBuffer^, FBufSize);
          Encode(FBuffer);
          XORBuffers(D, FBuffer, DataSize, D);
        end;
      end;
    emCFB:
      while DataSize > 0 do
      begin
        Move(FFeedback^, FBuffer^, FBufSize);
        Encode(FBuffer);
        Move(PByteArray(FFeedback)[1], FFeedback^, FBufSize-1);
        PByteArray(FFeedback)[FBufSize-1] := S^;
//        SHIFTBuffers(FFeedback, PByteArray(S), FBufSize, 8);
        D^ := S^ xor PByte(FBuffer)^;
        Inc(D);
        Inc(S);
        Dec(DataSize);
      end;
    emOFB:
      while DataSize > 0 do
      begin
        Move(FFeedback^, FBuffer^, FBufSize);
        Encode(FBuffer);
        D^ := S^ xor PByte(FBuffer)^;
        Move(PByteArray(FFeedback)[1], FFeedback^, FBufSize-1);
        PByteArray(FFeedback)[FBufSize-1] := PByte(FBuffer)^;
//        SHIFTBuffers(FFeedback, PByteArray(D), FBufSize, 8);
        Inc(D);
        Inc(S);
        Dec(DataSize);
      end;
  end;
  FillChar(FBuffer^, FBufSize, 0);
end;

{ TEncAlgo_Base }

constructor TEncAlgo_Base.Create(AOwner: TObject);
begin
  inherited;
  FEncObject := GetEncryptObjectClass.Create;
end;

destructor TEncAlgo_Base.Destroy;
begin
  FEncObject.Free;
  inherited;
end;

function TEncAlgo_Base.GetMode: TEncryptMode;
begin
  Result := FEncObject.Mode;
end;

procedure TEncAlgo_Base.SetMode(Value: TEncryptMode);
begin
  FEncObject.Mode := Value;
end;

procedure TEncAlgo_Base.InitKey(const Key: string);
begin
  FEncObject.InitKey(AnsiString(Key), nil);
end;

procedure TEncAlgo_Base.Done;
begin
  FEncObject.Done;
end;

procedure TEncAlgo_Base.EncodeBuffer(const Source; var Dest;
  DataSize: Integer);
begin
  FProgress := DoEncodeProgress;
  try
    FEncObject.EncodeBuffer(Source, Dest, DataSize);
  finally
    FProgress := nil;
  end;
end;

procedure TEncAlgo_Base.DecodeBuffer(const Source; var Dest;
  DataSize: Integer);
begin
  FProgress := DoDecodeProgress;
  try
    FEncObject.DecodeBuffer(Source, Dest, DataSize);
  finally
    FProgress := nil;
  end;
end;

{$WRITEABLECONST OFF}
end.



