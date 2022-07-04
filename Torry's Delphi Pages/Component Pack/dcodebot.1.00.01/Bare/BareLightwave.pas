
(********************************************************)
(*                                                      *)
(*  Bare Object Library @ www.codebot.org/delphi        *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BareLightwave;

interface

{$I BARE.INC}

interface

uses
  Windows, BareUtils;

type
  PInterchangeNode = ^TInterchangeNode;
  PInterchangeNodes = array of PInterchangeNode;

  TInterchangeNode = record
    Tag: string;
    Offset: Longint;
    Size: Longint;
    Children: PInterchangeNodes;
  end;

  TInterchangeFile = class(TObject)
  private
    FOwnsStream: Boolean;
    FRoot: PInterchangeNode;
    FStream: TStream;
    procedure InternalEnum(Node: PInterchangeNode; IdentSize: Integer);
  protected
    procedure RemoveChildren(Node: PInterchangeNode);
  public
    constructor Create(const FileName: string);
    constructor CreateFromStream(Stream: TStream; OwnsStream: Boolean = False);
    destructor Destroy; override;
    procedure EnumChildren(Node: PInterchangeNode);
    procedure EnumSubChildren(Node: PInterchangeNode);
    property Root: PInterchangeNode read FRoot;
    property Stream: TStream read FStream;
  end;

procedure InterchangeCheck(Result: Boolean);

procedure SwapBufferLong(Bytes: Pointer; Count: Integer);
procedure SwapBufferSmall(Bytes: Pointer; Count: Integer);

function ReadFloat(Stream: TStream): Single;
function ReadUint4(Stream: TStream): Longword;
function ReadUint2(Stream: TStream): Word;
function ReadUint1(Stream: TStream): Byte;
function ReadInt4(Stream: TStream): Longint;
function ReadInt2(Stream: TStream): Smallint;
function ReadInt1(Stream: TStream): Shortint;
function ReadTag(Stream: TStream; const Tag: string): Boolean;
function ReadText(Stream: TStream; Size: Integer): string;
function ReadCharacters(Stream: TStream): string;

implementation

const
  TagLength = 4;

procedure Error;
const
  SInvalidInterchangeFormat = 'Invalid interchange file format';
begin
  raise Exception.Create(SInvalidInterchangeFormat);
end;

procedure InterchangeCheck(Result: Boolean);
begin
  if not Result then Error;
end;

{ TInterchangeFile }

constructor TInterchangeFile.Create(const FileName: string);
begin
  CreateFromStream(TFileStream.Create(FileName, fmOpenRead));
end;

constructor TInterchangeFile.CreateFromStream(Stream: TStream;
  OwnsStream: Boolean = False);
begin
  inherited Create;
  FStream := Stream;
  FOwnsStream := OwnsStream;
  New(FRoot);
  InterchangeCheck(ReadTag(FStream, 'FORM'));
  FRoot.Size := ReadUint4(FStream);
  FRoot.Tag := ReadText(FStream, TagLength);
  FRoot.Offset := FStream.Position;
end;

destructor TInterchangeFile.Destroy;
begin
  if FRoot <> nil then
  begin
    RemoveChildren(FRoot);
    Dispose(FRoot);
  end;
  if FOwnsStream then FStream.Free;
  inherited Destroy;
end;

procedure TInterchangeFile.RemoveChildren(Node: PInterchangeNode);
var
  Child: PInterchangeNode;
  I: Integer;
begin
  for I := 0 to Length(Node.Children) do
  begin
    Child := Node.Children[I];
    RemoveChildren(Child);
    Dispose(Child);
  end;
  Node.Children := nil;
end;

procedure TInterchangeFile.InternalEnum(Node: PInterchangeNode; IdentSize: Integer);
var
  Position, NextSection: Integer;
  Child: PInterchangeNode;
  I: Integer;
begin
  RemoveChildren(Node);
  try
    InterchangeCheck(FStream.Seek(Node.Offset, soFromBeginning) = Node.Offset);
    Position := Node.Offset;
    NextSection := Position + Node.Size;
    I := 0;
    while Position < NextSection do
    begin
      New(Child);
      SetLength(Node.Children, I);
      Node.Children[I] := Child;
      Inc(I);
      Child.Tag := ReadText(FStream, TagLength);
      if IdentSize = 4 then
      begin
        Child.Size := ReadUint4(FStream);
        Inc(Position, 8);
      end
      else
      begin
        Child.Size := ReadUint2(FStream);
        Inc(Position, 6);
      end;
      Child.Offset := Position;
      InterchangeCheck(FStream.Seek(Child.Size, soFromCurrent) = Position + Child.Offset);
      Inc(Position, Child.Offset);
    end;
  except
    RemoveChildren(Node);
    raise;
  end;
end;

procedure TInterchangeFile.EnumChildren(Node: PInterchangeNode);
begin
  InternalEnum(Node, 4);
end;

procedure TInterchangeFile.EnumSubChildren(Node: PInterchangeNode);
begin
  InternalEnum(Node, 2);
end;

procedure SwapBytesLong(Bytes: Pointer);
asm
     MOV  EDX, [EAX];
     MOV  [EAX + 3], DL;
     SHR  EDX, 8;
     MOV  [EAX + 2], DL;
     SHR  EDX, 8;
     MOV  [EAX + 1], DL;
     SHR  EDX, 8;
     MOV  [EAX], DL;
end;

procedure SwapBytesSmall(Bytes: Pointer);
asm
     MOV  DX, [EAX];
     MOV  [EAX + 1], DL;
     SHR  DX, 8;
     MOV  [EAX ], DL;
end;

procedure SwapBufferLong(Bytes: Pointer; Count: Integer);
asm
     MOV  ECX, EDX;
     TEST ECX, ECX;
     JNZ  @Swap;
     RET;
@Swap:
     MOV  EDX, [EAX];
     MOV  [EAX + 3], DL;
     SHR  EDX, 8;
     MOV  [EAX + 2], DL;
     SHR  EDX, 8;
     MOV  [EAX + 1], DL;
     SHR  EDX, 8;
     MOV  [EAX], DL;
     ADD  EAX, 4;
     DEC  ECX;
     JNZ  @Swap;
end;

procedure SwapBufferSmall(Bytes: Pointer; Count: Integer);
asm
     MOV  ECX, EDX;
     TEST ECX, ECX;
     JNZ  @Swap;
     RET;
@Swap:
     MOV  DX, [EAX];
     MOV  [EAX + 1], DL;
     SHR  DX, 8;
     MOV  [EAX], DL;
     ADD  EAX, 2;
     DEC  ECX;
     JNZ  @Swap;
end;

function ReadFloat(Stream: TStream): Single;
begin
  InterchangeCheck(Stream.Read(Result, SizeOf(Result)) = SizeOf(Result));
  SwapBytesLong(@Result);
end;

function ReadUint4(Stream: TStream): Longword;
begin
  InterchangeCheck(Stream.Read(Result, SizeOf(Result)) = SizeOf(Result));
  SwapBytesLong(@Result);
end;

function ReadUint2(Stream: TStream): Word;
begin
  InterchangeCheck(Stream.Read(Result, SizeOf(Result)) = SizeOf(Result));
  SwapBytesSmall(@Result);
end;

function ReadUint1(Stream: TStream): Byte;
begin
  InterchangeCheck(Stream.Read(Result, SizeOf(Result)) = SizeOf(Result));
end;

function ReadInt4(Stream: TStream): Longint;
begin
  InterchangeCheck(Stream.Read(Result, SizeOf(Result)) = SizeOf(Result));
  SwapBytesLong(@Result);
end;

function ReadInt2(Stream: TStream): Smallint;
begin
  InterchangeCheck(Stream.Read(Result, SizeOf(Result)) = SizeOf(Result));
  SwapBytesSmall(@Result);
end;

function ReadInt1(Stream: TStream): Shortint;
begin
  InterchangeCheck(Stream.Read(Result, SizeOf(Result)) = SizeOf(Result));
end;

function ReadTag(Stream: TStream; const Tag: string): Boolean;
begin
  Result := UpperCase(ReadText(Stream, TagLength)) = UpperCase(Tag);
end;

function ReadText(Stream: TStream; Size: Integer): string;
begin
  SetLength(Result, Size);
  InterchangeCheck(Stream.Read(PChar(Result)^, Size) = Size);
  SetLength(Result, StrLen(PChar(Result)));
end;

function ReadCharacters(Stream: TStream): string;
var
  Buffer: array[0..255] of Char;
  I: Integer;
begin
  I := 0;
  repeat
    InterchangeCheck(Stream.Read(Buffer[I], 1) = 1);
    Inc(I);
  until Buffer[I] = #0;
  if Odd(I) then Stream.Read(Buffer[I], 1);
  Result := Buffer;
end;

function FieldCount(const Source: string; Terminator: Char): Integer;
var
  P: PChar;
begin
  Result := 0;
  if Source <> '' then
  begin
    P := PChar(Source);
    repeat
      if P^ = Terminator then
        Inc(Result);
      Inc(P);
    until P^ = #0;
    if (Result > 0) and (P[-1] <> Terminator) then
      Inc(Result);
  end;
end;

function FieldValue(const Source: string; Terminator: Char; Index: Integer): string;
var
  Start, P: PChar;
  I: Integer;
begin
  Result := '';
  if Source <> '' then
  begin
    Start := PChar(Source);
    P := Start;
    I := 0;
    while P^ > #0 do
    begin
      if P^ = Terminator then
      begin
        if I = Index then
        begin
          SetString(Result, Start, Integer(P - Start));
          Exit;
        end;
        Start := P;
        Inc(Start);
        Inc(I);
      end;
      Inc(P);
    end;
    if I = Index then
      SetString(Result, Start, Integer(P - Start));
  end;
end;

// FORM/LWOB/PNTS
// FORM/LWOB/SRFS
// FORM/LWOB/POLS

{function FindSection(Stream: TStream; const Path: string): Longword;
var
  Field, Section: string;
  I: Integer;
begin
  Result := 0;
  Stream.Seek(0, soFromBeginning);
  for I := 0 to FieldCount(Path, '/') - 1 do
    case I of
      0:
        begin
          InterchangeCheck(ReadTag(Stream, FieldValue(Path, '/', I)));
          Result := ReadUint4(Stream);
        end;
      1: InterchangeCheck(ReadTag(Stream, FieldValue(Path, '/', I)));
    else
      Field := UpperCase(FieldValue(Path, '/', I));
      repeat
        Section := UpperCase(ReadText(Stream, TagLength));
        if Section <> Field then
        begin
          if I = 2 then
            Stream.Seek(ReadUint4(Stream), soFromCurrent)
          else
            Stream.Seek(ReadUint2(Stream), soFromCurrent);
        end;
      until Section = Field;
      if I = 2 then
        Result := ReadUint4(Stream)
      else
        Result := ReadUint2(Stream);
    end;
end;}

end.
