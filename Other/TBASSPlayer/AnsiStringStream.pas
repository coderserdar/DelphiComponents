unit AnsiStringStream;

interface

uses classes;

type

  TAnsiStringStream = class(TStream)
  private
    FDataString: ansistring;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const AString: ansistring);
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadString(Count: Longint): ansistring;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteString(const AString: ansistring);
    property DataString: ansistring read FDataString;
  end;

implementation

constructor TAnsiStringStream.Create(const AString: ansistring);
begin
  inherited Create;
  FDataString := AString;
end;

function TAnsiStringStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Length(FDataString) - FPosition;
  if Result > Count then Result := Count;
  Move(PAnsiChar(@FDataString[FPosition + 1])^, Buffer, Result);
  Inc(FPosition, Result);
end;

function TAnsiStringStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  SetLength(FDataString, (FPosition + Result));
  Move(Buffer, PAnsiChar(@FDataString[FPosition + 1])^, Result);
  Inc(FPosition, Result);
end;

function TAnsiStringStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := Length(FDataString) - Offset;
  end;
  if FPosition > Length(FDataString) then
    FPosition := Length(FDataString)
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

function TAnsiStringStream.ReadString(Count: Longint): Ansistring;
var
  Len: Integer;
begin
  Len := Length(FDataString) - FPosition;
  if Len > Count then Len := Count;
  SetString(Result, PAnsiChar(@FDataString[FPosition + 1]), Len);
  Inc(FPosition, Len);
end;

procedure TAnsiStringStream.WriteString(const AString: Ansistring);
begin
  Write(PAnsiChar(AString)^, Length(AString));
end;

procedure TAnsiStringStream.SetSize(NewSize: Longint);
begin
  SetLength(FDataString, NewSize);
  if FPosition > NewSize then FPosition := NewSize;
end;

end.
 