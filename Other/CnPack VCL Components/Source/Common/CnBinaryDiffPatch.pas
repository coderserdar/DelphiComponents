{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

{******************************************************************************}
{      �õ�Ԫ�󲿷����ݻ��� Stefan Reuther �� BDiff / BPatch C ���뷭�������  }
{      ������ BDiff / BPatch ������:                                           }
{ -----------------------------------------------------------------------------}
{(c) copyright 1999 by Stefan Reuther <Streu@gmx.de>. Copying this program is  }
{allowed, as long as you include source code and document changes you made in a}
{user-visible way so people know they're using your version, not mine.         }
{This program is distributed in the hope that it will be useful, but without   }
{warranties of any kind, be they explicit or implicit.                         }
{ -----------------------------------------------------------------------------}

unit CnBinaryDiffPatch;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ����׵Ķ����Ʋ���Լ������㷨��Ԫ
* ��Ԫ���ߣ���Х��liuxiao@cnpack.org��
* ��    ע���õ�Ԫ�Ǽ��׵Ķ����Ʋ���Լ������㷨ʵ�֡�
*           �󲿷ֻ��� Stefan Reuther �� BDiff / BPatch C ���뷭�������
* ����ƽ̨��PWin7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2016.08.16 V1.2
*               ʵ��Ŀ¼������ Diff/Patch ����
*           2016.08.08 V1.1
*               ʵ�� Patch ����
*           2016.08.05 V1.0
*               ������Ԫ��ʵ�� Diff ����
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, FileCtrl, CnCommon;

const
  CN_BINARY_DIFF_NAME_VER: AnsiString = 'CnBDiff1';

type
  ECnPatchFormatError = class(Exception);

function BinaryDiffStream(OldStream, NewStream, PatchStream: TMemoryStream): Boolean;
{* �����Ʋ�ֱȽ��¾��ڴ�飨��������ֽ������ PatchStream �У�Patch ��������������ı��ȸ�ʽ}

function BinaryPatchStream(OldStream, PatchStream, NewStream: TMemoryStream): Boolean;
{* �����Ʋ�ֲ������ڴ�飨�������ϳɽ������ NewStream �У�ֻ֧�ֶ����Ƹ�ʽ�� Patch}

function BinaryDiffFile(const OldFile, NewFile, PatchFile: string): Boolean;
{* �����Ʋ�ֱȽ��¾��ļ�����ֽ������ PatchFile ��}

function BinaryPatchFile(const OldFile, PatchFile, NewFile: string): Boolean;
{* �����Ʋ�ֲ������ļ����ϳɽ������ NewFile ��}

function BinaryDiffDirectory(const OldDir, NewDir, PatchDir: string): Boolean;
{* �����Ʋ�ֱȽ��¾�Ŀ¼���� PatchDir ��������硰���ļ���.patch���Ķ����Ʋ�����
   ���޾��ļ����������ļ�.patch Ϊ����������֧����Ŀ¼}

function BinaryPatchDirectory(const OldDir, PatchDir, NewDir: string): Boolean;
{* �����Ʋ�ֲ�����Ŀ¼�������������������Ŀ¼}

implementation

const
  CN_MIN_LENGTH = 24;
  ADD_CHAR: AnsiChar = '+';
  COPY_CHAR: AnsiChar = '@';
  DOT_CHAR: AnsiChar = '.';
  CRLF: AnsiString = #13#10;
  PATCH_SUFFIX = '.patch';

type
  TCardinalArray = array[0..65535] of Cardinal;
  PCardinalArray = ^TCardinalArray;

  TCnMatchRec = packed record
    OldPos: Cardinal;
    NewPos: Cardinal;
    Len: Cardinal;
  end;
  PCnMatchRec = ^TCnMatchRec;

  TCnDiffOutputType = (dotBinary, dotFiltered, dotQuoted);

var
  CnDiffOutputType: TCnDiffOutputType = dotBinary; // Ĭ�����ɶ����Ʒ�ʽ�� Patch

function BlockSortCompare(BytePosA, BytePosB: Cardinal; Data: PByte; DataLen: Cardinal): Integer;
var
  Pa, Pb: PShortInt;
  Len: Cardinal;
begin
  Pa := PShortInt(Cardinal(Data) + BytePosA);
  Pb := PShortInt(Cardinal(Data) + BytePosB);
  Len := DataLen - BytePosA;
  if DataLen - BytePosB < Len then
    Len := DataLen - BytePosB;

  while (Len <> 0) and (Pa^ = Pb^) do
  begin
    Inc(Pa);
    Inc(Pb);
    Dec(Len);
  end;

  if Len = 0 then
    Result := BytePosA - BytePosB
  else
    Result := Pa^ - Pb^;
end;

procedure BlockSortSink(LeftPos, RightPos: Cardinal; Block: PInteger;
  Data: PByte; DataLen: Cardinal);
var
  I, J, X: Cardinal;
  BlockIntArray: PCardinalArray;
begin
  I := LeftPos;
  BlockIntArray := PCardinalArray(Block);
  X := BlockIntArray^[I];
  while True do
  begin
    J := 2 * I + 1;
    if J >= RightPos then
      Break;
    if J < RightPos - 1 then
      if BlockSortCompare(BlockIntArray^[J], BlockIntArray^[J + 1], Data, DataLen) < 0 then
        Inc(J);
    if BlockSortCompare(X, BlockIntArray^[J], Data, DataLen) > 0 then
      Break;

    BlockIntArray^[I] := BlockIntArray^[J];
    I := J;
  end;
  BlockIntArray^[I] := X;
end;

function BlockSort(Data: PByte; DataLen: Cardinal): PInteger;
var
  Block: PInteger;
  I, X, LeftPos, RightPos: Cardinal;
  BlockIntArray: PCardinalArray;
begin
  Result := nil;
  if DataLen <= 0 then
    Exit;

  Block := PInteger(GetMemory(SizeOf(Cardinal) * DataLen));
  if Block = nil then
    Exit;

  BlockIntArray := PCardinalArray(Block);
  for I := 0 to DataLen - 1 do
    BlockIntArray^[I] := I;

  LeftPos := DataLen div 2;
  RightPos := DataLen;

  while LeftPos > 0 do
  begin
    Dec(LeftPos);
    BlockSortSink(LeftPos, RightPos, Block, Data, DataLen);
  end;

  while RightPos > 0 do
  begin
    X := BlockIntArray^[LeftPos];
    BlockIntArray^[LeftPos] := BlockIntArray^[RightPos - 1];
    BlockIntArray^[RightPos - 1] := X;
    Dec(RightPos);
    BlockSortSink(LeftPos, RightPos, Block, Data, DataLen);
  end;
  Result := Block;
end;

function FindString(Data: PByte; Block: PInteger; DataLen: Cardinal; Sub: PByte;
  MaxLen: Cardinal; var Index: Cardinal): Cardinal;
var
  First, Last, Mid, FoundSize, L: Cardinal;
  Pm, Sm: PShortInt;
  BlockIntArray: PCardinalArray;
begin
  Result := 0;
  Index := 0;
  if DataLen = 0 then
    Exit;

  First := 0;
  Last := DataLen - 1;

  BlockIntArray := PCardinalArray(Block);
  while First <= Last do
  begin
    Mid := (First + Last) div 2;
    Pm := PShortInt(Cardinal(Data) + BlockIntArray^[Mid]);
    Sm := PShortInt(Sub);

    L := DataLen - BlockIntArray^[Mid];
    if L > MaxLen then
      L := MaxLen;

    FoundSize := 0;
    while (FoundSize < L) and (Pm^ = Sm^) do
    begin
      Inc(FoundSize);
      Inc(Pm);
      Inc(Sm);
    end;

    if FoundSize > Result then
    begin
      Result := FoundSize;
      Index := BlockIntArray^[Mid];
    end;

    if (FoundSize = L) or (Pm^ < Sm^) then
      First := Mid + 1
    else
    begin
      Last := Mid;
      if Last <> 0 then
        Dec(Last)
      else
        Break;
    end;
  end;
end;

procedure PackLong(P: PByte; L: DWORD);
begin
  P^ := L and $FF;
  Inc(P);
  P^ := (L shr 8) and $FF;
  Inc(P);
  P^ := (L shr 16) and $FF;
  Inc(P);
  P^ := (L shr 24) and $FF;
end;

function GetLong(P: PByte): DWORD;
begin
  Result := P^;
  Inc(P);
  Result := Result + 256 * P^;
  Inc(P);
  Result := Result + 65536 * P^;
  Inc(P);
  Result := Result + 16777216 * P^;
end;

function CheckSum(Data: PByte; DataLen: Cardinal; InitialSum: Cardinal = 0): Cardinal;
begin
  Result := InitialSum;
  while DataLen > 0 do
  begin
    Result := ((Result shr 30) and 3) or (Result shl 2);
    Result := Result xor Data^;

    Dec(DataLen);
    Inc(Data);
  end;
end;

procedure BsFindMaxMatch(Ret: PCnMatchRec; Data: PByte; Sort: PInteger; Len: Cardinal;
  Text: PByte; TextLen: Cardinal);
var
  FoundPos, FoundLen: Cardinal;
begin
  Ret^.Len := 0;
  Ret^.NewPos := 0;
  while TextLen <> 0 do
  begin
    FoundLen := FindString(Data, Sort, Len, Text, TextLen, FoundPos);
    if FoundLen >= CN_MIN_LENGTH then
    begin
      Ret^.OldPos := FoundPos;
      Ret^.Len := FoundLen;
      Exit;
    end;
    Inc(Text);
    Dec(TextLen);
    Inc(Ret^.NewPos);
  end;
end;

procedure CopyStreamData(OldStream, NewStream: TStream; ASize: Cardinal;
  ACheckSum: Cardinal; FromPatch: Boolean);
const
  BUF_SIZE = 4096;
var
  ChkRes: Cardinal;
  Buf: array[0..BUF_SIZE - 1] of AnsiChar;
  ToReadSize: Cardinal;
begin
  if (OldStream = nil) or (NewStream = nil) or (ASize = 0) then
    Exit;

  ChkRes := 0;
  while ASize > 0 do
  begin
    if ASize > BUF_SIZE then
      ToReadSize := BUF_SIZE
    else
      ToReadSize := ASize;

    if Cardinal(OldStream.Read(Buf[0], ToReadSize)) <> ToReadSize then
      raise ECnPatchFormatError.Create('Copy Content Read Fail.');

    if Cardinal(NewStream.Write(Buf[0], ToReadSize)) <> ToReadSize then
      raise ECnPatchFormatError.Create('Copy Content Write Fail.');

    ChkRes := CheckSum(@Buf[0], ToReadSize, ChkRes);
    Dec(ASize, ToReadSize);
  end;

  if not FromPatch and (ChkRes <> ACheckSum) then
    raise ECnPatchFormatError.Create('Patch Checksum Fail.');
end;

procedure WriteHeader(OutStream: TStream; OldSize, NewSize: Cardinal);
var
  Buf: array[0..7] of Byte;
  S: AnsiString;
begin
  if OutStream <> nil then
  begin
    case CnDiffOutputType of
      dotBinary:
        begin
          OutStream.Write(CN_BINARY_DIFF_NAME_VER[1], Length(CN_BINARY_DIFF_NAME_VER));
          PackLong(@Buf[0], OldSize);
          PackLong(@Buf[4], NewSize);
          OutStream.Write(Buf[0], SizeOf(Buf));
        end;
      dotFiltered, dotQuoted:
        begin
          S := AnsiString(Format('%% --- Old (%d bytes)' + #13#10 + '%% +++ New (%d bytes)' + #13#10, [OldSize, NewSize]));
          OutStream.Write(S[1], Length(S));
        end;
    end;
  end;
end;

function IsPrintableChar(AChar: Byte): Boolean;
begin
  Result := AChar in [32..127];
end;

procedure WriteFilteredOrQuotedData(OutStream: TStream; Data: PByte;
  DataLen: Cardinal; IsFiltered: Boolean);
var
  S: AnsiString;
begin
  if IsFiltered then
  begin
    while DataLen > 0 do
    begin
      if IsPrintableChar(Data^) and (Chr(Data^) <> '\') then
        OutStream.Write(Data^, 1)
      else
      begin
        S := AnsiString(Format('#$%2.2x', [Data^]));
        OutStream.Write(S[1], Length(S));
      end;

      Inc(Data);
      Dec(DataLen);
    end;
  end
  else
  begin
    while DataLen > 0 do
    begin
      if IsPrintableChar(Data^) then
        OutStream.Write(Data^, 1)
      else
        OutStream.Write(DOT_CHAR, 1);
      Inc(Data);
      Dec(DataLen);
    end;
  end;
end;

procedure WriteAddContent(OutStream: TStream; Data: PByte; DataLen: Cardinal);
var
  Buf: array[0..3] of Byte;
begin
  if OutStream <> nil then
  begin
    if CnDiffOutputType = dotBinary then
    begin
      OutStream.Write(ADD_CHAR, 1);
      PackLong(@Buf[0], DataLen);
      OutStream.Write(Buf[0], SizeOf(Buf));
      OutStream.Write(Data^, DataLen);
    end
    else
    begin
      OutStream.Write(ADD_CHAR, 1);
      WriteFilteredOrQuotedData(OutStream, Data, DataLen, CnDiffOutputType = dotFiltered);
      OutStream.Write(CRLF[1], Length(CRLF));
    end;
  end;
end;

procedure WriteCopyContent(OutStream: TStream; NewBase: PByte; NewPos: Cardinal;
  OldBase: PByte; OldPos: Cardinal; DataLen: Cardinal);
var
  Buf: array[0..11] of Byte;
  S: AnsiString;
begin
  if OutStream <> nil then
  begin
    if CnDiffOutputType = dotBinary then
    begin
      OutStream.Write(COPY_CHAR, 1);
      PackLong(@Buf[0], OldPos);
      PackLong(@Buf[4], DataLen);
      PackLong(@Buf[8], CheckSum(PByte(Cardinal(NewBase) + NewPos), DataLen));
      OutStream.Write(Buf[0], SizeOf(Buf));
    end
    else
    begin
      S := AnsiString(Format('@ -[%d] => +[%d] %d bytes' + #13#10, [OldPos, NewPos, DataLen]));
      OutStream.Write(S[1], Length(S));
      WriteFilteredOrQuotedData(OutStream, PByte(Cardinal(NewBase) + NewPos), DataLen,
        CnDiffOutputType = dotFiltered);
      OutStream.Write(CRLF[1], Length(CRLF));
    end;
  end;
end;

// �����Ʋ�ֱȽ��¾��ڴ�飨��������ֽ������ PatchStream ��
function BinaryDiffStream(OldStream, NewStream, PatchStream: TMemoryStream): Boolean;
var
  Sort: PInteger;
  Todo, Nofs: Cardinal;
  Match: TCnMatchRec;
begin
  Result := False;
  if (OldStream = nil) or (NewStream = nil) or (PatchStream = nil) then
    Exit;

  Sort := BlockSort(OldStream.Memory, OldStream.Size);
  if (Sort = nil) and (OldStream.Size > 0) then
    Exit;

  WriteHeader(PatchStream, OldStream.Size, NewStream.Size);

  Todo := NewStream.Size;
  Nofs := 0;
  while Todo > 0 do
  begin
    BsFindMaxMatch(@Match, OldStream.Memory, Sort, OldStream.Size,
      PByte(Cardinal(NewStream.Memory) + Nofs), Todo);

    if Match.Len <> 0 then
    begin
      WriteAddContent(PatchStream, PByte(Cardinal(NewStream.Memory) + Nofs), Match.NewPos);

      Inc(Nofs, Match.NewPos);
      Dec(Todo, Match.NewPos);

      WriteCopyContent(PatchStream, NewStream.Memory, Nofs, OldStream.Memory, Match.OldPos, Match.Len);

      Inc(Nofs, Match.Len);
      Dec(Todo, Match.Len);
    end
    else
    begin
      WriteAddContent(PatchStream, PByte(Cardinal(NewStream.Memory) + Nofs), Todo);
      Break;
    end;
  end;
  FreeMemory(Sort);
  Result := True;
end;

// �����Ʋ�ֲ������ڴ�飨�������ϳɽ������ NewStream ��
function BinaryPatchStream(OldStream, PatchStream, NewStream: TMemoryStream): Boolean;
var
  Buf: array[0..15] of Byte;
  SrcLen, DstLen, ASize, AnOffset: Cardinal;
  AnOperator: AnsiChar;
begin
  Result := False;
  if (OldStream = nil) or (PatchStream = nil) or (NewStream = nil) then
    Exit;

  if PatchStream.Read(Buf[0], 16) <> 16 then
    raise ECnPatchFormatError.Create('Patch Header Missing.');

  if not CompareMem(@CN_BINARY_DIFF_NAME_VER[1], @Buf[0], Length(CN_BINARY_DIFF_NAME_VER)) then
    raise ECnPatchFormatError.Create('Patch Header Name/Version Mismatch.');

  SrcLen := GetLong(@Buf[8]);
  DstLen := GetLong(@Buf[12]);

  AnOperator := #0;
  while True do
  begin
    if PatchStream.Read(AnOperator, 1) <> 1 then // ��ĩβ
      Break;

    case AnOperator of
    '@':
      begin
        if PatchStream.Read(Buf[0], 12) <> 12 then
          raise ECnPatchFormatError.Create('Patch Copy Area Mismatch.');

        ASize := GetLong(@Buf[4]);
        AnOffset := GetLong(@Buf[0]);

        if (AnOffset > SrcLen) or (ASize > SrcLen) or (ASize + AnOffset > SrcLen) then
          raise ECnPatchFormatError.Create('Patch Copy Size Mismatch.');

        OldStream.Seek(AnOffset, soFromBeginning);
        CopyStreamData(OldStream, NewStream, ASize, GetLong(@Buf[8]), False);
        Dec(DstLen, ASize);
      end;
    '+':
      begin
        if PatchStream.Read(Buf[0], 4) <> 4 then
          raise ECnPatchFormatError.Create('Patch Add Area Mismatch.');
        ASize := GetLong(@Buf[0]);

        CopyStreamData(PatchStream, NewStream, ASize, 0, True);
        Dec(DstLen, ASize);
      end;
    else
      raise ECnPatchFormatError.CreateFmt('Patch Operator Unknown %c.', [AnOperator]);
    end;
  end;

  if DstLen <> 0 then
    raise ECnPatchFormatError.Create('Patch Length Mismatch.');

  Result := True;
end;

// �����Ʋ�ֱȽ��¾��ļ�����ֽ������ PatchFile ��
function BinaryDiffFile(const OldFile, NewFile, PatchFile: string): Boolean;
var
  OldStream, NewStream, PatchStream: TMemoryStream;
begin
  OldStream := nil;
  NewStream := nil;
  PatchStream := nil;

  try
    OldStream := TMemoryStream.Create;
    if OldFile <> '' then // ���ļ�������Ҳ��
      OldStream.LoadFromFile(OldFile);
    NewStream := TMemoryStream.Create;
    if NewFile <> '' then // ���ļ�������Ҳ�У���ʾɾ��
      NewStream.LoadFromFile(NewFile);

    PatchStream := TMemoryStream.Create;
    Result := BinaryDiffStream(OldStream, NewStream, PatchStream);
    PatchStream.SaveToFile(PatchFile);
  finally
    PatchStream.Free;
    NewStream.Free;
    OldStream.Free;
  end;
end;

// �����Ʋ�ֲ������ļ����ϳɽ������ NewFile ��
function BinaryPatchFile(const OldFile, PatchFile, NewFile: string): Boolean;
var
  OldStream, NewStream, PatchStream: TMemoryStream;
begin
  OldStream := nil;
  PatchStream := nil;
  NewStream := nil;

  try
    OldStream := TMemoryStream.Create;
    if OldFile <> '' then // ���ļ�������Ҳ��
      OldStream.LoadFromFile(OldFile);
    PatchStream := TMemoryStream.Create;
    PatchStream.LoadFromFile(PatchFile);

    NewStream := TMemoryStream.Create;
    Result := BinaryPatchStream(OldStream, PatchStream, NewStream);
    if NewStream.Size > 0 then // ���ļ�����������򲻴�
      NewStream.SaveToFile(NewFile)
    else
      DeleteFile(PChar(NewFile));
  finally
    NewStream.Free;
    PatchStream.Free;
    OldStream.Free;
  end;
end;

// �����Ʋ�ֱȽ��¾�Ŀ¼���� PatchDir ��������硰���ļ���.patch���Ķ����Ʋ�����
// ���޾��ļ����������ļ�.patch Ϊ����������֧����Ŀ¼
function BinaryDiffDirectory(const OldDir, NewDir, PatchDir: string): Boolean;
var
  OldFiles, NewFiles: TStrings;
  OldFile: string;
  I, Idx: Integer;
begin
  Result := False;
  if not DirectoryExists(OldDir) or not DirectoryExists(NewDir) then
    Exit;

  ForceDirectories(PatchDir);
  OldFiles := nil;
  NewFiles := nil;
  try
    OldFiles := TStringList.Create;
    NewFiles := TStringList.Create;

    GetDirFiles(OldDir, OldFiles);
    GetDirFiles(NewDir, NewFiles);

    for I := 0 to OldFiles.Count - 1 do
    begin
      OldFile := OldFiles[I];
      Idx := NewFiles.IndexOf(OldFile);
      if Idx >= 0 then // �¾��ļ�������
      begin
        if not BinaryDiffFile(AddDirSuffix(OldDir) + OldFile, AddDirSuffix(NewDir) + OldFile,
          AddDirSuffix(PatchDir) + OldFile + PATCH_SUFFIX) then
          Exit;
        NewFiles.Delete(Idx);
      end
      else // ���ļ�������
      begin
        if not BinaryDiffFile(AddDirSuffix(OldDir) + OldFile, '',
          AddDirSuffix(PatchDir) + OldFile + PATCH_SUFFIX) then
          Exit;
      end;
    end;

    for I := 0 to NewFiles.Count - 1 do
    begin
      // ���ļ�������
      if not BinaryDiffFile('', AddDirSuffix(NewDir) + NewFiles[I],
        AddDirSuffix(PatchDir) + NewFiles[I] + PATCH_SUFFIX) then
        Exit;
    end;
    Result := True;
  finally
    OldFiles.Free;
    NewFiles.Free;
  end;
end;

// �����Ʋ�ֲ�����Ŀ¼�������������������Ŀ¼
function BinaryPatchDirectory(const OldDir, PatchDir, NewDir: string): Boolean;
var
  OldFiles, PatchFiles: TStrings;
  PatchFile, FileName, OldFile: string;
  I: Integer;
begin
  Result := False;
  if not DirectoryExists(OldDir) or not DirectoryExists(PatchDir) then
    Exit;

  ForceDirectories(NewDir);
  OldFiles := nil;
  PatchFiles := nil;
  try
    OldFiles := TStringList.Create;
    PatchFiles := TStringList.Create;

    GetDirFiles(OldDir, OldFiles);
    GetDirFiles(PatchDir, PatchFiles);

    for I := 0 to PatchFiles.Count - 1 do
    begin
      PatchFile := PatchFiles[I];
      if StrRight(PatchFile, Length(PATCH_SUFFIX)) <> PATCH_SUFFIX then
        Continue;  // .patch ��β�Ĳ���Ϊ�ǲ���

      FileName := PatchFile;
      Delete(FileName, Pos(PATCH_SUFFIX, FileName), MaxInt);

      OldFile := AddDirSuffix(OldDir) + FileName;
      if not FileExists(OldFile) then // ����Դ�ļ�������
        OldFile := '';

      if not BinaryPatchFile(OldFile, AddDirSuffix(PatchDir) + PatchFile,
        AddDirSuffix(NewDir) + FileName) then
        Exit;
    end;
    Result := True;
  finally
    OldFiles.Free;
    PatchFiles.Free;
  end;

end;

end.
