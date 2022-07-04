{***********************************************}
{                                               }
{  Diff Maker                                   }
{  Copyright (c) 1997 S.Kurinny & S.Kostinsky   }
{                                               }
{***********************************************}

unit aDiff;

interface
Uses SysUtils, Classes, aCRC32;

{-------------------------------------------------}

Const
  BufSize=2048*1024;

type
  PInteger=^Integer;
  TMyProcedure=procedure;
  TByteArray=Array[1..MaxInt] of byte;
  PByteArray=^TByteArray;

     TDiffCompData=record
       InBuf      : PByteArray;      {pointer to input buffer}
       UseBuf     : PByteArray;      {pointer to use buffer}
       InBufSize  : integer;         {input buffer size}
       UseBufSize : integer;         {use buffer size}
       MaxLevel   : integer;         {max search level}
       MaxLength  : integer;         {max string length}
       MinLength  : integer;         {min string length}
       OutBuf     : PByteArray;
       OutSpBuf   : PByteArray;
       OutBufSize : PInteger;
       OutSpBufSize:PInteger;
       EnoughLen   :integer;
     end;

{-------------------------------------------------}

{ Calculates hash value
  3 bytes to 0..32767 }
Function CalcHash(a,b,c:Integer):Integer;

{ Compares buffers and returns number of equal bytes
  Len - max length }
function comp(var a,b;len:integer):integer;
Function Min(a,b:integer):integer;
Function Max(a,b:integer):integer;
procedure DiffStreamCompress(InStream,UseStream,OutStream:TStream;Notify:TmyProcedure;MaxLev:Integer);
procedure DiffCompress(D:TDiffCompData);
procedure DiffStreamExtract(InStream,UseStream,OutStream:TStream;Notify:TMyProcedure);

{-------------------------------------------------}

Const
  SSmallBuffer='Buffer size should be at least 4 bytes';

{-------------------------------------------------}
implementation
{-------------------------------------------------}

Const
  MaxHashValue=131071;
  cMaxFastData=6;

type
  t3word=record
    len:word;
    useofs:integer;
    bufofs:integer;
  end;
  p3word=^t3word;

  arrayt3word=array[0..Maxint div 20] of t3word;
  parrayt3word=^arrayt3word;

  THashTable=Array[0..MaxHashValue] of Integer;
  PHashTable=^THashTable;
  THashList=Array[1..MaxInt div 4] of integer;
  PHashList=^THashList;

{-----------------------------------------}

Function Max(a,b:integer):integer;
begin
  If a>b then result:=a else result:=b;
end;

{-----------------------------------------}

Function Min(a,b:integer):integer;
begin
  If a<b then result:=a else result:=b;
end;

{----------------------------------------------------------------}

function comp(var a,b;len:integer):integer; assembler;
asm
    PUSH ESI
    PUSH EDI
    mov esi,a
    mov edi,b
    cld
    mov eax,len
    mov ecx,eax
    rep cmpsb
    inc ecx
    sub eax,ecx
    POP EDI
    POP ESI
end;

{--------------------------------------------------------}

procedure DiffCompress(D:TDiffCompData);
Var
  HTab:PHashTable;
  HList:PHashList;
  i:integer;
  a,b,c:integer;
  h:integer;
  curofs:integer;
  curlen:integer;
  curlevel:integer;
  templen:integer;
  tempOFs:Integer;
  oldh:integer;
  x:integer;
  t3:t3word;
  curpos,cursppos:integer;

Label
  l3,l2,l1;
begin
  With D do
  begin

  If (InBufSize<4) or (UseBufSize<4) then
    raise Exception.Create(SSmallBuffer);
  CurPos:=1;
  CurSpPos:=1;
  CurOfs:=0;
  New(HTab);
  GetMem(HList,UseBufSize*4);
  try
    FillChar(HTab^,Sizeof(THashTable),0);
    a:=UseBuf[1];
    b:=UseBuf[2];
    c:=UseBuf[3];
    oldh:=maxint;
    For i:=1 to UseBufSize-4 do
    begin
      h:=(a shl 9) xor (b shl 5) xor c;
       If h<>oldh then
       begin
        HList^[i]:=HTab^[h];
        HTab^[h]:=i;
        oldh:=h;
       end;
      a:=b;b:=c;c:=UseBuf[i+3];
    end;
    {-------}
    i:=1;
    While i<=InBufSize do
    begin
      a:=InBuf[i];
      if i>inbufsize-3 then goto l1;
      h:=(a shl 9) xor (InBuf[i+1] shl 5) xor InBuf[i+2];
      curlen:=minlength-1;
      TempOFs:=HTab^[h];
      curlevel:=0;
      While (tempofs<>0) and (CurLevel<MaxLevel) do
      begin
        x:=Min(InBufSize-i,UseBufSize-TempOFs);
        templen:=Comp(InBuf[i],UseBuf[TempOfs],x) and $0000ffff;
        If TempLen>CurLen then
        begin
          CurLen:=TempLen;
          CurOfs:=TempOfs;
        end;
     l3:
        TempOfs:=HList^[TempOfs];
        inc(CurLevel);
      end;
      If CurLen<MinLength then
      begin
      l1:
        OutBuf[CurPos]:=a;
        inc(CurPos);
        Inc(i);
      end else
      begin
    l2:
       t3.len:=CurLen;
       t3.useofs:=CurOfs;
       t3.bufofs:=i;
       Move(t3,OutSpBuf[CurSpPos],SizeOf(t3));
       inc(CurSpPos,SizeOf(t3));
       Inc(i,CurLen);
      end;
    end;
    {-------}
    t3.len:=0;
    t3.useofs:=0;
    t3.bufofs:=InBufSize+1;
    Move(t3,OutSpBuf[CurSpPos],SizeOf(t3));
    inc(CurSpPos,SizeOf(t3));
    OutBufSize^:=CurPos-1;
    OutSpBufSize^:=CurSpPos-1;
  finally
    Dispose(HTab);
    FreeMem(HList,UseBufSize*4);
  end;
  end;
end;

{-------------------------------------------------}

procedure DiffStreamCompress(InStream,UseStream,OutStream:TStream;Notify:TMyProcedure;MaxLev:Integer);
Var
   Buf,Temp,OutBuf,OutSpBuf:PByteArray;
   BufRead,TempRead:Integer;
{----}

procedure WriteByte(A:Byte);
begin
  OutStream.Write(A,1);
end;

{----}

procedure WriteInt(A:Integer);
begin
  OutStream.Write(A,SizeOF(Integer));
end;

{----}

procedure CompressBuf(Var aInBuf,aUseBuf;BufSize,UseSize,MaxLev:Integer);
Var
  dat:TDiffCompData;
  obufsize,ospbufsize:integer;
begin
    Dat.OutBuf     :=OutBuf;
    Dat.OutSpBuf   :=OutSpBuf;
    With dat do
    begin
       InBuf      :=@TByteArray(aInBuf);
       UseBuf     :=@TByteArray(aUseBuf);
       InBufSize  :=BufSize;
       UseBufSize :=UseSize;
       MaxLevel   :=MaxLev;
       MaxLength  :=65535;
       MinLength  :=20;
       OutBufSize :=@obufsize;
       OutSpBufSize:=@ospbufsize;
       EnoughLen:=1024;
    end;
    obufsize:=0;
    ospbufsize:=0;
    DiffCompress(Dat);

    WriteInt(OBufSize);
    WriteInt(OSpBufSize);
    OutStream.Write(OutBuf^,OBufSize);

    OutStream.Write(OutSpBuf^,OSpBufSize);
end;

label l1;

{----}

begin
  GetMem(Buf,BufSize);
  GetMem(Temp,BufSize);
  GetMem(OutBuf,BufSize);
  GetMem(OutSpBuf,BufSize);
  try
    BufRead:=1;
    While BufRead<>0 do
    begin
      BufRead:=InStream.Read(Buf^,BufSize);
      TempRead:=UseStream.Read(Temp^,BufSize);
      {--}
      WriteInt(TempRead);
      If (BufRead<4) or (TempRead<4) or (BufRead div 4>TempRead) then
      begin
        WriteByte(0); {block copied flag}
        WriteInt(BufRead);
        if bufread=0 then goto l1;
        WriteInt(CalculateCRC32(Buf^,Bufread));
        OutStream.Write(Buf^,BufRead);
      end else
      begin
        WriteByte(1);{block compressed flag}
        WriteInt(CalculateCRC32(Buf^,Bufread));
        WriteInt(CalculateCRC32(Temp^,Tempread));
        CompressBuf(Buf^,Temp^,BufRead,TempRead,MaxLev);
      end;
      l1:
      If Assigned(Notify) then Notify;
    end;
  finally
    FreeMem(OutBuf,BufSize);
    FreeMem(OutSpBuf,BufSize);
    FreeMem(Buf,BufSize);
    FreeMem(Temp,BufSize);
  end;
end;

{-------------------------------------------------}

procedure DiffStreamExtract(InStream,UseStream,OutStream:TStream;Notify:TMyProcedure);
Var
   Buf,Temp,OutBuf,OutSpBuf:PByteArray;
   curoutpos,BufRead,TempRead:Integer;

{----}

Function ReadByte:Byte;
begin
  InStream.Read(Result,1);
end;

{----}

Function ReadInt:Integer;
begin
  InStream.Read(Result,SizeOF(Integer));
end;

{----}

procedure ExtractBuf;
Var
  obufsize,ospbufsize:integer;
  p:parrayt3word;
  d,er,len,useofs,bufofs,i,psize:integer;
begin
    OBufSize:=ReadInt;
    OSpBufSize:=ReadInt;

    InStream.Read(Buf^,OBufSize);
    InStream.Read(OutSpBuf^,OSpBufSize);
    p:=pointer(OutSpBuf);
    psize:=OSpBufSize div sizeof(t3word);
    er:=1;
    curoutpos:=1;
    For i:=0 to PSize-1 do
    begin
      len:=p^[i].len;
      useofs:=p^[i].useofs;
      bufofs:=p^[i].bufofs;

      d:=BufOfs-CurOutPos;
      If d<>0 then
      begin
        Move(Buf[er],OutBuf[CurOutPos],d);
        inc(er,d);
        inc(CurOutPos,d);
      end;
      Move(Temp[UseOFs],OutBuf[CurOutPos],Len);
      inc(CurOutPos,len);
    end;
end;
{----}

Var CRC,BufCRC:Integer;
label l1;
begin
  try
    GetMem(Buf,BufSize);
    GetMem(Temp,BufSize);
    GetMem(OutBuf,BufSize);
    GetMem(OutSpBuf,BufSize);
    BufRead:=1;
    While BufRead<>0 do
    begin
       TempRead:=ReadInt;
       UseStream.Read(Temp^,Tempread);
       Case ReadByte of
         0: begin //copy
              BufRead:=ReadInt;
              If bufread=0 then goto l1;
              CRC:=ReadInt;
              InStream.Read(Buf^,Bufread);
              If CRC<>CalculateCRC32(Buf^,Bufread) then
                raise Exception.Create('CRC Error');
              OutStream.Write(Buf^,BufRead);
            end;
         1: begin //extract
              BufCRC:=ReadInt;
              CRC:=ReadInt;{tempcrc}
              if CRC<>CalculateCRC32(Temp^,Tempread) then
                raise Exception.Create('CRC Error');
              ExtractBuf;
              If BufCRC<>CalculateCRC32(OutBuf^,CurOutPos-1) then
                raise Exception.Create('CRC Error');
              OutStream.Write(OutBuf^,CurOutPos-1);
            end;
         else raise Exception.Create('CRC Error');
       end;
    l1:
    If Assigned(Notify) then Notify;
    end;
  finally
    FreeMem(OutBuf,BufSize);
    FreeMem(OutSpBuf,BufSize);
    FreeMem(Buf,BufSize);
    FreeMem(Temp,BufSize);
  end;
end;

{-------------------------------------------------}

Function CalcHash(a,b,c:Integer):Integer;
begin
  Result:=(a shl 7) xor (b shl 4) xor c;
end;

{-------------------------------------------------}
end.
