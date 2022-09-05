unit ZRStream;

interface

{$I ZRDefine.inc}

uses
  Windows,                                         // WinAPI
  SysUtils,                                        // Delphi RTL
  Classes;                                         // Delphi VCL

type
  { TZStreamWrapper }
  TZStreamWrapper = class(TStream)
  private
    fStream : TStream;
  protected
    property Stream: TStream read fStream;
  public
    constructor Create(aStream: TStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  { TZOEMStream }
  TZOEMStream = class(TZStreamWrapper)
  private
    fOEMConvert: Boolean;
    procedure UkraineUpdate(const Buffer; Count: Longint);
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property OEMConvert: Boolean read fOEMConvert write fOEMConvert default False;
  end;

  { TZStringStream }
  TZStringStream = class(TZOEMStream)
  public
    function  ReadChar(var Value: Char): Boolean;
    function  ReadString(var Value: String): Boolean; virtual;
    procedure WriteChar(Value: Char);
    procedure WriteString(const Value: String); virtual;
  end;

implementation

{ TZStreamWrapper }

constructor TZStreamWrapper.Create(aStream: TStream);
begin
  inherited Create;
  fStream := aStream;
end;

function TZStreamWrapper.Read(var Buffer; Count: Integer): Longint;
begin
  if Assigned(fStream) then
    Result := fStream.Read(Buffer, Count)
  else
    Result := 0;
end;

function TZStreamWrapper.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if Assigned(fStream) then
    Result := fStream.Seek(Offset, Origin)
  else
    Result := 0;
end;

function TZStreamWrapper.Write(const Buffer; Count: Integer): Longint;
begin
  if Assigned(fStream) then
    Result := fStream.Write(Buffer, Count)
  else
    Result := 0;
end;

{ TZOEMStream }

procedure TZOEMStream.UkraineUpdate(const Buffer; Count: Longint);
var
  Index : Integer;
  P     : PChar;
begin
  P:=@Buffer;
  for Index := 0 to Count -1 do begin
    if (P+Index)^ = Chr(178) then (P+Index)^ := Chr(73);      //  i
    if (P+Index)^ = Chr(179) then (P+Index)^ := Chr(105);     //  I
  end;
end;

function TZOEMStream.Write(const Buffer; Count: Longint): Longint;
var
  B: PChar;
begin
  if OEMConvert then begin
    GetMem(B, Count);
    UkraineUpdate(Buffer,Count);
    CharToOEMBuff(@Buffer, B, Count);
  end else
    B:= @Buffer;
  Result := inherited Write(B^, Count);
  if fOEMConvert then FreeMem(B);
end;

function TZOEMStream.Read(var Buffer; Count: Longint): Longint;
var
  B: PChar;
begin
  if OEMConvert then GetMem(B, Count) else B:= @Buffer;
  Result := inherited Read(B^, Count);
  if fOEMConvert then begin
    CharToOEMBuff(B, @Buffer, Count);
    FreeMem(B);
  end;
end;

{ TZStringStream }

function TZStringStream.ReadChar(var Value: Char): Boolean;
begin
  Result:= Read(Value, SizeOf(Value)) = SizeOf(Value);
end;

procedure TZStringStream.WriteChar(Value: Char);
begin
  Write(Value, SizeOf(Value));
end;

procedure TZStringStream.WriteString(const Value: String);
var
  S: String;
begin
  S:= {TrimRight(}Value{)};
  Write(PChar(S + #13#10)^, length(S)+2);
end;

function TZStringStream.ReadString(var Value: String): Boolean;
const
  Delimiters: set of Char = [#13, #10];
var
  Buffer: array[0..1023] of Char;
  BufPtr: PChar;
  C     : Char;
begin
  BufPtr:= Buffer;
  Result:= Position < Size;
  if Result then
    while ReadChar(C) do
      begin
        if (C = #13) and ReadChar(C) and (C <> #10) then begin
          Seek(-SizeOf(C), soFromCurrent);
          C:= #13;
        end;
        if (C in Delimiters) then Break;
        BufPtr^:= C;
        Inc(BufPtr);
      end;
  BufPtr^:= #0;
  {$ifdef D4Above}
  Value:= Buffer;
  {$else}
  Value:= StrPas(Buffer);
  {$endif}
end;

end.

