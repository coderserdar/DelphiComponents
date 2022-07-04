
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit StreamTools;

interface

{$I STD.INC}

uses
  Classes;

{ The stream worker routines simplify access to read and write operations on
  streams }

procedure SetStreamWorker(Stream: TStream);
function GetStreamWorker: TStream;

function ReadInteger: Integer;
procedure WriteInteger(I: Integer);
function ReadString: string;
procedure WriteString(const S: string);
function ReadBoolean: Boolean;
procedure WriteBoolean(Flag: Boolean);
function ReadFloat: Double;
procedure WriteFloat(const Value: Double);
procedure ReadInstance(Instance: TObject);
procedure WriteInstance(Instance: TObject);

implementation

var
  StreamWorker: TStream = nil;

procedure SetStreamWorker(Stream: TStream);
begin
  StreamWorker := Stream;
end;

function GetStreamWorker: TStream;
begin
  Result := StreamWorker;
end;

function ReadInteger: Integer;
begin
  Result := 0;
  if StreamWorker <> nil then
    StreamWorker.Read(Result, SizeOf(Result));
end;

procedure WriteInteger(I: Integer);
begin
  if StreamWorker <> nil then
    StreamWorker.Write(I, SizeOf(I));
end;

function ReadString: string;
var
  Size: Integer;
begin
  Result := '';
  if (StreamWorker <> nil) and (StreamWorker.Read(Size, SizeOf(Size)) > 0) then
  begin
    SetLength(Result, Size);
    StreamWorker.Read(PChar(Result)^, Size);
  end;
end;

procedure WriteString(const S: string);
var
  Size: Integer;
begin
  if StreamWorker <> nil then
  begin
    Size := Length(S);
    StreamWorker.Write(Size, SizeOf(Size));
    if Size > 0 then
      StreamWorker.Write(PChar(S)^, Size);
  end;
end;

function ReadBoolean: Boolean;
begin
  Result := False;
  if StreamWorker <> nil then
    StreamWorker.Read(Result, SizeOf(Result));
end;

procedure WriteBoolean(Flag: Boolean);
begin
  if StreamWorker <> nil then
    StreamWorker.Write(Flag, SizeOf(Flag));
end;

function ReadFloat: Double;
begin
  Result := 0.0;
  if StreamWorker <> nil then
    StreamWorker.Read(Result, SizeOf(Result));
end;

procedure WriteFloat(const Value: Double);
begin
  if StreamWorker <> nil then
    StreamWorker.Write(Value, SizeOf(Value));
end;

procedure ReadInstance(Instance: TObject);
begin
  { if StreamWorker <> nil then }
end;

procedure WriteInstance(Instance: TObject);
begin
  { if StreamWorker <> nil then }
end;

end.
