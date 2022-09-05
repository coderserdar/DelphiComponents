// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //
//                                                                            //  
// For further information / comments, visit our WEB site at                  //
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  18983: mcmFileStream.pas 
//
//    Rev 1.9    2014-02-02 21:09:54  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.8    11-08-2009 10:02:48  mcm    Version: IMG 3.3
// Modified use of PChar to PAnsiChar
//
//   Rev 1.7    07-03-2004 14:34:02  mcm    Version: IMG 2.4
// Internal.

//
//   Rev 1.6    22-12-2003 16:03:00  mcm
// Modification to Position property (streaming multiple TIFF's).

//
//   Rev 1.5    29-09-2003 18:44:32  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.4    25-07-2003 00:04:16  mcm
// Code clean-up.

//
//   Rev 1.3    25-03-2003 21:57:04  mcm
// Modified SetSource to cope with Nil.

//
//   Rev 1.2    05-02-03 19:59:02  mcm    Version: IMG 1.3

//
//   Rev 1.1    29-01-2003 15:38:56  mcm
// Design 2.

//
//   Rev 1.0    27-01-2003 13:31:44  mcm
// Initial version, Buffers read and write methods

unit mcmFileStream;

{$Include 'mcmDefines.pas'}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

interface

uses {$IFNDEF GE_DXE2}
      Windows, Classes,
     {$ELSE}
      WinApi.Windows, System.Classes,
     {$ENDIF}
     mcmImageTypeDef;

{ $DEFINE STANDARDACCESS}

type
  TmcmReadWriteMode = (RWM_READ, RWM_WRITE);

  TmcmBufferStream = class(TPersistent)
  private
    FMode         : TmcmReadWriteMode;
    FRBitIndex    : word;
    FWBitIndex    : word;
    FWJPGBitIndex : word;
    FLastByte     : longword;
    FBigEndian    : boolean;
    FSource       : TStream;
    FMemStream    : TMemoryStream;
    {$IFNDEF DCB3_5}
      FPos          : int64; // Current position in file.
      FHead         : int64; // File position of first byte in FMemStream.
      FTail         : int64; // File position of last byte in FMemStream.
      FBufSize      : int64; // Size of FMemStream.
    {$ELSE}
      FPos          : longint;
      FHead         : longint;
      FTail         : longint;
      FBufSize      : longint;
    {$ENDIF}
  protected
    function    GetBuffer : Pointer;
    {$IFDEF DCB3_5}
      function    GetBufferSize : longint;
      procedure   SetBufferSize(const Value : longint);
      function    GetPosition : longint;
      function    GetSize : longint;
      procedure   SetPosition(Pos : longint);
      procedure   SetSize(NewSize : longint);
    {$ELSE}
      function    GetBufferSize : int64;
      function    GetPosition : int64;
      function    GetSize : int64;
      procedure   SetBufferSize(const Value : int64);
      procedure   SetPosition(const Pos : int64);
      procedure   SetSize(NewSize : int64);
    {$ENDIF}
    procedure   SetMode(NewMode : TmcmReadWriteMode);
    procedure   SetSource(Stream : TStream);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Clear;
    procedure   ByteAlign;
    procedure   FilePosAlign;
    procedure   Flush;
    procedure   FlushBits;
    function    Read(var Buffer; Count : longint) : longint;
    //function    ReadNBits(Count : word) : cardinal;
    function    ReadJPEGBits(Count : word) : cardinal;
    function    ReadLong : longint;
    function    ReadWord : word;
    procedure   ResetBitIndex;
    function    Seek(Offset : longint; Origin : word) : longint; {$IFDEF DCB3_5} {$ELSE} overload;
    function    Seek(const Offset : int64; Origin : TSeekOrigin) : int64; overload;  {$ENDIF}
    function    Write(const Buffer; Count : longint) : longint;
    procedure   WriteJPEGBits(Bits : cardinal; Count : word);
    procedure   WriteLong(Value : longint);
    procedure   WriteWord(Value : word);

    property    BigEndian : boolean
      read      FBigEndian
      write     FBigEndian;
    {$IFDEF DCB3_5}
      property    MemorySize : longint
        read      GetBufferSize
        write     SetBufferSize;
    {$ELSE}
      property    MemorySize : int64
        read      GetBufferSize
        write     SetBufferSize;
    {$ENDIF}
    property    Memory : Pointer
      read      GetBuffer;
    property    Mode : TmcmReadWriteMode
      read      FMode
      write     SetMode default RWM_READ;
    {$IFDEF DCB3_5}
       property    Position : longint
         read      GetPosition
         write     SetPosition;
    {$ELSE}
       property    Position : int64
         read      GetPosition
         write     SetPosition;
    {$ENDIF}
    {$IFDEF DCB3_5}
      property     Size : longint
        read       GetSize
        write      SetSize;
    {$ELSE}
      property     Size : int64
        read       GetSize
        write      SetSize;
    {$ENDIF}
    property     Source : TStream
      read       FSource
      write      SetSource;
  published
  end;


implementation

Uses {$IFNDEF GE_DXE2}
     SysUtils,
     {$ELSE}
     System.SysUtils,
     {$ENDIF}
     DefJPEG, mcmImageResStr;

constructor TmcmBufferStream.Create;
begin
  Inherited Create;
  FSource := Nil;
  FMemStream := TMemoryStream.Create;
  FBufSize := $8FFF;
  FMemStream.SetSize(FBufSize);
  Mode  := RWM_READ;
  FPos  := 0;
  FHead := 0;
  FTail := 0;
  FRBitIndex := 0;
  FWBitIndex := 32;
  FWJPGBitIndex := 8;
  FBigEndian := False;
  FLastByte := 0;
end; // TmcmBufferStream.Create.


destructor TmcmBufferStream.Destroy;
begin
  FMemStream.Free;
  FMemStream := Nil;
  Inherited Destroy;
end; // TmcmBufferStream.Destroy.


procedure TmcmBufferStream.Clear;
begin
  //FMemStream.Clear;
  FBufSize  := 0;
  FLastByte := 0;
  FRBitIndex := 0;
  FWBitIndex := 32;
  FWJPGBitIndex := 8;
  FHead := FPos;
  FTail := FPos;
end; // TmcmBufferStream.Clear.


procedure TmcmBufferStream.SetMode(NewMode : TmcmReadWriteMode);
begin
  if (FMode <> NewMode)
  then begin
       FMode := NewMode;
       if (FMode = RWM_READ)
       then FMemStream.Position := FMemStream.Size
       else FMemStream.Position := 0;
  end;
end; // TmcmBufferStream.SetMode.


procedure TmcmBufferStream.ResetBitIndex;
begin
  FRBitIndex := 0;
  FWBitIndex := 32;
  FWJPGBitIndex := 8;
end; // TmcmBufferStream.ResetBitIndex.


procedure TmcmBufferStream.FlushBits;
var BytsToFlush : integer;
begin
  if (FWJPGBitIndex <> 8)
  then Write(FLastByte, 1)
  else if (FWBitIndex <> 32)
       then begin
            FWBitIndex := FWBitIndex div 8;
            BytsToFlush := 4 - FWBitIndex;
            if (BytsToFlush > 0)
            then begin
                 FLastByte := FLastByte shr (FWBitIndex * 8);
                 Write(FLastByte, BytsToFlush);
            end;
       end;

  FLastByte := 0;
  ResetBitIndex;
end; // TmcmBufferStream.FlushBits.


procedure TmcmBufferStream.Flush;
{$IFNDEF STANDARDACCESS}
var BytesWrite : Longint;
{$ENDIF}
begin
  FlushBits;
  {$IFNDEF STANDARDACCESS}
    if (FHead <> FPos)
    then begin
         BytesWrite := FSource.Write(FMemStream.Memory^, (FPos - FHead));
         if (BytesWrite <> (FPos - FHead))
         then Raise EWriteError.Create(resEC_WRITETOFILE);
         FMemStream.Position := 0;
         FHead := FPos;
         FTail := FPos;
    end;
  {$ENDIF}
end; // TmcmBufferStream.Flush.


procedure TmcmBufferStream.ByteAlign;
// Used during "Read" process to align the .
var StepBack : integer;
begin
  StepBack := FRBitIndex div 8;
  ResetBitIndex;
  if (StepBack > 0)
  then begin
       {$IFDEF STANDARDACCESS}
         Position := Position - StepBack;
       {$ELSE}
         dec(FPos, StepBack);
         FMemStream.Position := FMemStream.Position - StepBack;
       {$ENDIF}
  end;
end; // TmcmBufferStream.ByteAlign.


procedure TmcmBufferStream.FilePosAlign;
begin
  ByteAlign;
  {$IFNDEF STANDARDACCESS}
    SetPosition(FSource.Position - (FTail - FPos));
  {$ENDIF}
end; // TmcmBufferStream.FilePosAlign


function TmcmBufferStream.GetBuffer : Pointer;
begin
  Result := FMemStream.Memory;
end; // TmcmBufferStream.GetBuffer.


{$IFDEF DCB3_5}

function TmcmBufferStream.GetSize : longint;
begin
  Result := FSource.Size;
end; // TmcmBufferStream.GetSize.


procedure TmcmBufferStream.SetSize(NewSize : longint);
begin
  // Do nothing.
end; // TmcmBufferStream.SetSize.


function TmcmBufferStream.GetBufferSize : longint;
begin
  Result := FBufSize;
end; // TmcmBufferStream.GetBufferSize.


procedure TmcmBufferStream.SetBufferSize(const Value : longint);
begin
  if (FBufSize <> Value)
  then begin
       FBufSize := Value;
       FMemStream.SetSize(FBufSize);
  end;
  if (FMode = RWM_READ)
  then FMemStream.Position := FMemStream.Size
  else FMemStream.Position := 0;
end; // TmcmBufferStream.SetBufferSize.


function TmcmBufferStream.GetPosition : longint;
begin
  {$IFNDEF STANDARDACCESS}
    Result := FPos;
  {$ELSE}
    Result := FSource.Position;
  {$ENDIF}
end; // TmcmBufferStream.GetPosition.


procedure TmcmBufferStream.SetPosition(Pos : longint);
begin
  {$IFNDEF STANDARDACCESS}
    if (FPos <> Pos)
    then begin
         if (FHead > Pos) or (Pos > FTail)
         then begin
              if (FMode = RWM_READ)
              then FMemStream.Position := FMemStream.Size
              else begin
                   Flush;
                   FHead := Pos;
                   FTail := Pos;
              end;
              if Assigned(FSource)
              then FSource.Position := Pos;
         end
         else FMemStream.Position := Pos - FHead;
         FPos := Pos;
    end;
  {$ELSE}
    FSource.Position := Pos;
  {$ENDIF}
end; // TmcmBufferStream.SetPosition.

{$ELSE}

function TmcmBufferStream.GetSize : int64;
begin
  Result := FSource.Size;
end; // TmcmBufferStream.GetSize.


procedure TmcmBufferStream.SetSize(NewSize : int64);
begin
  // Do nothing.
end; // TmcmBufferStream.SetSize.


function TmcmBufferStream.GetBufferSize : int64;
begin
  Result := FBufSize;
end; // TmcmBufferStream.GetBufferSize.


procedure TmcmBufferStream.SetBufferSize(const Value : int64);
begin
  if (FBufSize <> Value)
  then begin
       FBufSize := Value;
       FMemStream.SetSize(FBufSize);
  end;
  if (FMode = RWM_READ)
  then FMemStream.Position := FMemStream.Size
  else FMemStream.Position := 0;
end; // TmcmBufferStream.SetBufferSize.


function TmcmBufferStream.GetPosition : int64;
begin
  {$IFNDEF STANDARDACCESS}
    Result := FPos;
  {$ELSE}
    Result := FSource.Position;
  {$ENDIF}
end; // TmcmBufferStream.GetPosition.


procedure TmcmBufferStream.SetPosition(const Pos : int64);
begin
  {$IFNDEF STANDARDACCESS}
    if (FPos <> Pos)
    then begin
         if (FHead > Pos) or (Pos > FTail)
         then begin
              if (FMode = RWM_READ)
              then FMemStream.Position := FMemStream.Size
              else begin
                   Flush;
                   FHead := Pos;
                   FTail := Pos;
              end;
              if Assigned(FSource)
              then FSource.Position := Pos
              else FPos := Pos;
         end
         else FMemStream.Position := Pos - FHead;
         FPos := Pos;
    end;
  {$ELSE}
    if Assigned(FSource)
    then FSource.Position := Pos;
  {$ENDIF}
end; // TmcmBufferStream.SetPosition.

{$ENDIF}

procedure TmcmBufferStream.SetSource(Stream : TStream);
begin
  FSource := Stream;
  if (FSource <> Nil)
  then Position := FSource.Position
  else Position := 0;
  FHead := FPos;
  FTail := FPos;
end; // TmcmBufferStream.SetSource.


function TmcmBufferStream.Read(var Buffer; Count : longint) : longint;
{$IFNDEF STANDARDACCESS}
var pTemp     : PAnsiChar;
    BytesRead : Longint;
    BytesFile : Longint;
{$ENDIF}
begin
  {$IFNDEF STANDARDACCESS}
    if (Count > FBufSize)
    then begin
         if (FSource.Position <> FPos)
         then FSource.Position := FPos;
         BytesRead := FSource.Read(Buffer, Count);
         FPos  := FPos + BytesRead;
         FHead := FPos;
         FTail := FPos;
         FMemStream.Position := FMemStream.Size;
    end
    else begin
         BytesRead := FMemStream.Read(Buffer, Count);
         if (BytesRead < Count)
         then begin
              Count := Count - BytesRead;
              pTemp := PAnsiChar(@Buffer) + BytesRead;
              FHead := FPos + BytesRead;
              BytesFile := FSource.Read(FMemStream.Memory^, FMemStream.Size);
              FTail := FHead + BytesFile;
              FMemStream.Position := 0;
              if (Count > BytesFile)
              then Count := BytesFile;
              BytesRead := BytesRead + FMemStream.Read(pTemp^, Count);
         end;
         FPos := FPos + BytesRead;
    end;
    Result := BytesRead;
  {$ELSE}
    Result := FSource.Read(Buffer, Count);
  {$ENDIF}
end; // TmcmBufferStream.Read.


function TmcmBufferStream.ReadLong : longint;
begin
  Read(Result, 4);
  if FBigEndian
  then begin
       asm
         MOV   EAX,Result
         BSWAP EAX
         MOV   Result,EAX
       end;
  end;
end; // TmcmBufferStream.ReadLong.


function TmcmBufferStream.ReadWord : word;
begin
  Read(Result, 2);
  if FBigEndian
  then begin
       asm
         MOV   AX,Result
         XCHG  AL,AH
         MOV   Result,AX
       end;
  end;
end; // TmcmBufferStream.ReadWord.


function TmcmBufferStream.Seek(Offset : longint; Origin : word) : longint;
begin
  {$IFNDEF STANDARDACCESS}
    Result := FSource.Seek(Offset, Origin);
  {$ELSE}
    Result := FSource.Seek(Offset, Origin);
  {$ENDIF}
end; // TmcmBufferStream.Seek.


{$IFNDEF DCB3_5}
function TmcmBufferStream.Seek(const Offset : int64; Origin : TSeekOrigin) : int64;
begin
  {$IFNDEF STANDARDACCESS}
    Result := FSource.Seek(Offset, Origin);
  {$ELSE}
    Result := FSource.Seek(Offset, Origin);
  {$ENDIF}
end; // TmcmBufferStream.Seek.
{$ENDIF}

function TmcmBufferStream.Write(const Buffer; Count : longint) : longint;
{$IFNDEF STANDARDACCESS}
var BytesWrite : Longint;
    BytesFile  : Longint;
{$ENDIF}
begin
  {$IFNDEF STANDARDACCESS}
    if (Count > FBufSize)
    then begin
         if (FHead <> FPos)
         then begin
              BytesFile := FSource.Write(FMemStream.Memory^, (FPos - FHead));
              FMemStream.Position := 0;
              if (BytesFile <> (FPos - FHead))
              then Raise EWriteError.Create(resEC_WRITETOFILE);
         end;
         BytesWrite := FSource.Write(Buffer, Count);
         FPos  := FPos + BytesWrite;
         FHead := FPos;
         FTail := FPos; //FHead + FBufSize;
    end
    else begin
         if (Count > FBufSize - (FPos - FHead))
         then begin
              BytesFile := FSource.Write(FMemStream.Memory^, (FPos - FHead));
              if (BytesFile <> (FPos - FHead))
              then Raise EWriteError.Create(resEC_WRITETOFILE);
              FMemStream.Position := 0;
              FHead := FPos;
         end;
         BytesWrite := FMemStream.Write(Buffer, Count);
         FPos := FPos + BytesWrite;
         if (FTail < FPos)
         then FTail := FPos;
    end;
    Result := BytesWrite;
  {$ELSE}
    Result := FSource.Write(Buffer, Count);
  {$ENDIF}
end; // TmcmBufferStream.Write.


procedure TmcmBufferStream.WriteLong(Value : longint);
begin
  if FBigEndian
  then begin
       asm
         MOV   EAX,Value
         BSWAP EAX
         MOV   Value,EAX
       end;
  end;
  Write(Value, 4);
end; // TmcmBufferStream.WriteLong.


procedure TmcmBufferStream.WriteWord(Value : word);
begin
  if FBigEndian
  then begin
       asm
         MOV   AX,Value
         XCHG  AL,AH
         MOV   Value,AX
       end;
  end;
  Write(Value, 2);
end; // TmcmBufferStream.WriteWord.


(*
function TmcmBufferStream.ReadNBits(Count : word) : cardinal;
var Value   : cardinal;
    InByte  : byte;
    SubCode : byte;
    i       : integer;
begin
  if (Count = 0)
  then Result := 0
  else begin
       if (Count <= FRBitIndex)
       then begin
            Result := (FLastByte shr (FRBitIndex - Count)) and ((1 shl Count) - 1);
            dec(FRBitIndex, Count);
       end
       else begin
            if (FRBitIndex = 0)
            then Value := 0
            else Value := (FLastByte and ((1 shl FRBitIndex) - 1)) shl (Count - FRBitIndex);

            FLastByte := 0;
            //Read(FLastByte, 4);
            for i := 3 downto 0
            do begin
               {$IFNDEF STANDARDACCESS}
                 if (FMemStream.Read(InByte, 1) = 0)
                 then Read(InByte, 1)
                 else inc(FPos);
               {$ELSE}
                 Read(InByte, 1);
               {$ENDIF}

               FLastByte := FLastByte or InByte shl (8 * i);
            end;

            FRBitIndex := 32 + FRBitIndex - Count;
            Result := Value or (FLastByte shr FRBitIndex);
       end;
  end;
end; // TmcmBufferStream.ReadNBits.
*)

function TmcmBufferStream.ReadJPEGBits(Count : word) : cardinal;
var Value   : cardinal;
    InByte  : byte;
    SubCode : byte;
    i       : integer;
begin
  if (Count = 0)
  then Result := 0
  else begin
       if (Count <= FRBitIndex)
       then begin
            Result := (FLastByte shr (FRBitIndex - Count)) and ((1 shl Count) - 1);
            dec(FRBitIndex, Count);
       end
       else begin
            if (FRBitIndex = 0)
            then Value := 0
            else Value := (FLastByte and ((1 shl FRBitIndex) - 1)) shl (Count - FRBitIndex);

            FLastByte := 0;
            //Read(FLastByte, 4);
            for i := 3 downto 0
            do begin
               {$IFNDEF STANDARDACCESS}
                 if (FMemStream.Read(InByte, 1) = 0)
                 then Read(InByte, 1)
                 else inc(FPos);
               {$ELSE}
                 Read(InByte, 1);
               {$ENDIF}

               if (InByte = $FF)
               then begin
                    Read(SubCode, 1);
                    if (SubCode <> 0)
                    then begin
                         if (SubCode = ($FF and JPG_DNL))
                         then Raise Exception.Create('JPEG, Unexpected DNL marker.')
                         //else Raise Exception.Create('JPEG, Unexpected marker.');
                         else begin
                              // Tricky - decrements the Position back to start
                              // of JPEG marker.
                              Position := Position - 2;
                              Break;
                         end;
                    end;
               end;
               FLastByte := FLastByte or InByte shl (8 * i);
            end;

            FRBitIndex := 32 + FRBitIndex - Count;
            Result := Value or (FLastByte shr FRBitIndex);
       end;
  end;
end; // TmcmBufferStream.ReadJPEGBits.


procedure TmcmBufferStream.WriteJPEGBits(Bits : cardinal; Count : word);
var i : integer;
begin
  if (Count > 0)
  then begin
       for i := (Count - 1) downto 0
       do begin
          if (FWJPGBitIndex <= 0)
          then begin
               Write(FLastByte, 1);
               if (FLastByte = $FF)
               then begin
                    FLastByte := 0;
                    Write(FLastByte, 1);
               end
               else FLastByte := 0;
               FWJPGBitIndex := 8;
          end;
          dec(FWJPGBitIndex);
          if ((Bits and BitMask[i]) <> 0)
          then FLastByte := FLastByte or BitMask[FWJPGBitIndex];
       end;
  end;
end; // TmcmBufferStream.WriteJPEGBits.



{$IFDEF STANDARDACCESS}
  {$UNDEF STANDARDACCESS}
{$ENDIF}


end.
