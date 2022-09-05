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
// $Log:  25960: mcmHandleStream.pas 
//
//    Rev 1.4    2014-02-02 21:09:56  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.3    26-08-2009 23:01:14  mcm    Version: IMG 3.2
// Delphi 2009 support
//
//    Rev 1.2    20-08-2007 20:28:22  mcm
// Added support for Delphi 2007
//
//   Rev 1.1    30/07/2005 12:21:06  mcm    Version: IMG 2.9
// Added compiler options.

//
//   Rev 1.0    24-07-2005 18:51:40  mcm    Version: IMG 2.9
// Specialised "Handle" stream control.

unit mcmHandleStream;

interface

{$Include 'mcmDefines.pas'}

Uses {$IFNDEF GE_DXE2}
      Windows, Classes;
     {$ELSE}
      WinApi.Windows, System.Classes;
     {$ENDIF}

type
  //----------------------------------------------------------------------------
  // TmcmHandleStream
  // is an alternative memory stream. This stream has two functions:
  // 1. if Handle passed in Create is not "0" it provides means to read data
  //    from the memory Handle.
  // 2. When the Handle passed in Create is "0" this stream can be used to write
  //    data to the "memory stream" and when done obtain a THandle using the
  //    method ReleaseHandle.

  TmcmHandleStream = class(TStream)
  private
    FHandle   : THandle;
    FMemory   : Pointer;
    FSize     : longint;
    FPosition : longint;
    FCapacity : cardinal;

    procedure SetCapacity(NewCapacity : cardinal);
  protected
    function Realloc(NewCapacity : cardinal) : Pointer; virtual;
    procedure SetPointer(Ptr: Pointer; Size: cardinal);
  public
    constructor Create(Handle : THandle);
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const FileName : string);
    procedure LoadFromStream(Stream : TStream);
    function  Read(var Buffer; Count: Longint): Longint; override;
    function  ReleaseHandle : THandle;
    function  Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SetSize(NewSize : longint); override;
    function  Write(const Buffer; Count : longint) : longint; override;

    property  Capacity : cardinal
      read    FCapacity
      write   SetCapacity;
    property  Memory : Pointer
      read    FMemory;
  end;

implementation

Uses {$IFNDEF GE_DXE2}
      Consts, SysUtils,
     {$ELSE}
      Vcl.Consts, System.SysUtils,
     {$ENDIF}
     mcmImageResStr;

const
  MemoryDelta = $2000; // Must be a power of 2.

constructor TmcmHandleStream.Create(Handle : THandle);
begin
  FMemory   := Nil;
  FSize     := 0;
  FPosition := 0;
  FCapacity := 0;
  Inherited Create;

  FHandle := Handle;
  if (FHandle <> 0)
  then begin
       FSize := GlobalSize(FHandle);
       FMemory := GlobalLock(FHandle);
  end;
end; // TmcmHandleStream.Create.


destructor TmcmHandleStream.Destroy;
begin
  Clear;
  inherited Destroy;
end; // TmcmHandleStream.Destroy.


procedure TmcmHandleStream.Clear;
begin
  SetCapacity(0);
  Size := 0;
  Position := 0;
end; // TmcmHandleStream.Clear.


function TmcmHandleStream.Seek(Offset : longint; Origin : word) : longint;
begin
  case Origin of
  0 : FPosition := Offset;
  1 : Inc(FPosition, Offset);
  2 : FPosition := FSize + Offset;
  end;
  Result := FPosition;
end; // TmcmHandleStream.Seek.


function TmcmHandleStream.Read(var Buffer; Count : longint) : longint;
begin
  if (FPosition >= 0) and (Count >= 0)
  then begin
       Result := FSize - FPosition;
       if (Result > 0)
       then begin
            if Result > Count
            then Result := Count;
            Move(Pointer(Longint(FMemory) + FPosition)^, Buffer, Result);
            Inc(FPosition, Result);
            Exit;
       end;
  end;
  Result := 0;
end; // TmcmHandleStream.Read.


function TmcmHandleStream.Write(const Buffer; Count : longint): Longint;
var Pos : cardinal;
begin
  if (Position >= 0) and (Count >= 0)
  then begin
       Pos := Position + Count;
       if (Pos > 0)
       then begin
            if (Pos > cardinal(Size))
            then begin
                 if (Pos > FCapacity)
                 then SetCapacity(Pos);
                 Size := Pos;
            end;
            System.Move(Buffer, Pointer(Longint(FMemory) + FPosition)^, Count);
            Position := Pos;
            Result := Count;
            Exit;
       end;
  end;
  Result := 0;
end; // TmcmHandleStream.Write.


procedure TmcmHandleStream.LoadFromStream(Stream : TStream);
var Count : longint;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetSize(Count);
  if (Count <> 0)
  then Stream.ReadBuffer(FMemory^, Count);
end; // TmcmHandleStream.LoadFromStream.


procedure TmcmHandleStream.SaveToStream(Stream : TStream);
begin
  if (FSize <> 0)
  then Stream.WriteBuffer(FMemory^, FSize);
end; // TmcmHandleStream.SaveToStream.


procedure TmcmHandleStream.LoadFromFile(const FileName : string);
var Stream : TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end; // TmcmHandleStream.LoadFromFile.


procedure TmcmHandleStream.SaveToFile(const FileName : string);
var Stream : TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end; // TmcmHandleStream.SaveToFile.


procedure TmcmHandleStream.SetPointer(Ptr : Pointer; Size : cardinal);
begin
  FMemory := Ptr;
  FSize := Size;
end; // TmcmHandleStream.SetPointer.


procedure TmcmHandleStream.SetCapacity(NewCapacity : cardinal);
begin
  SetPointer(Realloc(NewCapacity), Size);
end; // TmcmHandleStream.SetCapacity.


procedure TmcmHandleStream.SetSize(NewSize : longint);
var OldPosition : longint;
begin
  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if (OldPosition > NewSize)
  then Seek(0, soFromEnd);
end; // TmcmHandleStream.SetSize.


function TmcmHandleStream.Realloc(NewCapacity : cardinal) : Pointer;
(* var ErrorCode  : DWord;
    DebugStr   : Widestring; *)
begin
  if (NewCapacity > 0)
  then NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  Result := Memory;
  if (NewCapacity <> FCapacity)
  then begin
       if (NewCapacity = 0)
       then begin
            if (FHandle <> 0)
            then GlobalFree(FHandle)
            else GlobalFreePtr(Memory);
            Result := Nil;
       end
       else begin
            if (FHandle <> 0)
            then begin
                 // What to do.....
            end;
       
            if (Capacity = 0)
            then begin
                 Result := GlobalAllocPtr(GMEM_MOVEABLE, NewCapacity);
                 if (Result = Nil) 
                 then begin
                 (*
                      ErrorCode := GetLastError();
                      FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
                                    Nil, ErrorCode, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), @DebugStr, 0, Nil);
                      OutputDebugString(PChar(DebugStr));
                      *)
                 end
                 else FCapacity := NewCapacity;
            end
            else begin
                 Result := GlobalReallocPtr(Memory, NewCapacity, GMEM_MOVEABLE);
                 FCapacity := NewCapacity;
            end;
            if (Result = Nil)
            then begin
                 FCapacity := 0;
                 raise EStreamError.Create(resEC_HANDLESTREAMERROR);
            end;
       end;
  end;
  if (Result = Nil) 
  then FCapacity := 0;
end; // TmcmHandleStream.Realloc.


function TmcmHandleStream.ReleaseHandle : THandle;
begin
  // Returns a THandle to the memory stream. The memory is released, i.e. not
  // owned by TmcmHandleStream.
  // The returned THandle must be deleted after use.
  Result := GlobalHandle(FMemory);
  if (Result <> 0)
  then SetPointer(Nil, 0);
end; // TmcmHandleStream.ReleaseHandle.

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
