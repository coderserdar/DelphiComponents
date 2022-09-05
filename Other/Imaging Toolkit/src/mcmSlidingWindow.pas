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
// $Log:  17589: mcmSlidingWindow.pas 
//
//    Rev 1.4    2014-02-02 21:10:06  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.3    15-01-2009 00:20:32  mcm
// Added support for Delphi 2009
//
//    Rev 1.2    02-01-2009 20:10:28  mcm    Version: IMG 3.3
// Delphi 2009 Support
//
//   Rev 1.1    27-01-2003 13:46:20  mcm

//
//   Rev 1.0    27-05-2002 16:22:28  mcm

unit mcmSlidingWindow;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses {$IFNDEF GE_DXE2}
      Windows, Classes,
     {$ELSE}
      WinApi.Windows, System.Classes,
     {$ENDIF}
     mcmImageTypeDef, mcmHashTable;

type TOnNeedData = procedure (Sender : TObject; pData : Pointer; var NoBytes : cardinal) of object;
     TOnHasData = procedure (Sender : TObject; pData : Pointer; NoBytes : cardinal) of object;
type
  TmcmSlidingWindow = class(TPersistent)
  private
    FDoCompress     : boolean;
    FWindowSize     : longint;
    FLookAheadSize  : longint;
    FStart          : PAnsiChar;
    FCurrent        : PAnsiChar;
    FLookAheadEnd   : PAnsiChar;
    FMidPoint       : PAnsiChar;
    FBuffer         : PAnsiChar;
    FBufferEnd      : PAnsiChar;
    FStartOffset    : longint;
    FMaxMatchLength : longint;

    FOnHasData      : TOnHasData;
    FOnNeedData     : TOnNeedData;
  protected
    procedure   AdvanceAfterAdd(Count : integer);
    procedure   GetInputData;
    procedure   SetCapacity(Value : longint);
    procedure   WriteToStream(FinalBlock : boolean);
    procedure   SetOnNeedData(AOnNeedData : TOnNeedData);
  public
    constructor Create(WindowSize    : word;
                       LookAheadSize : word;
                       DoCompress    : boolean);
    destructor  Destroy; override;
    procedure   Clear;
    procedure   Advance(Count : cardinal);
    function    Compare(Offset : longint; var Distance : longint) : longint;
    procedure   GetNextKey(var Key    : THashKey;
                           var Offset : longint);
    procedure   AddByte(NewByte : byte);
    procedure   AddDistLen(Distance : integer; Length : integer);

    property    OnHasData : TOnHasData
      read      FOnHasData
      write     FOnHasData;
    property    OnNeedData : TOnNeedData
      read      FOnNeedData
      write     SetOnNeedData;
  published
  end;

implementation

//------------------------------------------------------------------------------
//  Meaning of the internal pointers:
//
//  |----------+===================+==+--------------------------|
//  |          |                   |  |                          |
//  FBuffer    FStart      FCurrent   FLookAheadEnd    FBufferEnd
//
//  Valid data is between FStart and FLookAheadEnd when compressing, and between
//  FStart and FCurrent when decompressing (FLookAheadEnd not being used
//  in this latter case). Usually the difference between FStart and FCurrent
//  is 4096, the size of the sliding window.
//------------------------------------------------------------------------------

constructor TmcmSlidingWindow.Create(    WindowSize    : word;
                                         LookAheadSize : word;
                                         DoCompress    : boolean);
begin
  Inherited Create;
  FOnHasData  := Nil;
  FOnNeedData := Nil;

  FWindowSize := WindowSize;
  FLookAheadSize := LookAheadSize;
  FMaxMatchLength := FLookAheadSize;

  FDoCompress := DoCompress;

  // Set capacity of sliding window: by definition this is 4096 bytes of
  // sliding window and 18 bytes of lookahead
  SetCapacity(FWindowSize + FLookAheadSize);

  // Reset the buffer and, if we're compressing, read some data from the
  // stream to be compressed.
  Clear;
end; // TmcmSlidingWindow.Create.


destructor TmcmSlidingWindow.Destroy;
begin
  if Assigned(FBuffer)
  then begin
       // Finish writing to the output stream if we're decompressing}
       if Not(FDoCompress)
       then WriteToStream(True);
       // Free the buffer.
       FreeMem(FBuffer);
       FBuffer := Nil;
  end;
  Inherited Destroy;
end; // TmcmSlidingWindow.Destroy.


procedure TmcmSlidingWindow.Clear;
begin
  FStart := FBuffer;
  FCurrent := FBuffer;
  FLookAheadEnd := FBuffer;
  FStartOffset := 0;
end; // TmcmSlidingWindow.Clear.


procedure TmcmSlidingWindow.SetOnNeedData(AOnNeedData : TOnNeedData);
begin
  FOnNeedData := AOnNeedData;
  if FDoCompress and Assigned(FOnNeedData)
  then GetInputData;
end; // TmcmSlidingWindow.SetOnNeedData.


procedure TmcmSlidingWindow.SetCapacity(Value : longint);
var NewQueue : PAnsiChar;
begin
  // Round the requested capacity to nearest 64 bytes.
  Value := (Value + 63) and $7FFFFFC0;

  // Destroy the old buffer.
  if (FBuffer <> Nil)
  then FreeMem(FBuffer);

  // Get a new buffer.
  GetMem(NewQueue, Value * 2);

  // Set the head/tail and other pointers.
  FBuffer := NewQueue;
  FStart := NewQueue;
  FCurrent := NewQueue;
  FLookAheadEnd := NewQueue;
  FBufferEnd := NewQueue + (Value * 2);
  FMidPoint := NewQueue + Value;
end; // TmcmSlidingWindow.SetCapacity.


procedure TmcmSlidingWindow.WriteToStream(FinalBlock : boolean);
var BytesToWrite : longint;
begin
  // Write the data before the current sliding window.
  if FinalBlock
  then BytesToWrite := FCurrent - FBuffer
  else BytesToWrite := FStart - FBuffer;

  if Assigned(FOnHasData)
  then FOnHasData(Self, FBuffer, BytesToWrite);
end; // TmcmSlidingWindow.WriteToStream.


procedure TmcmSlidingWindow.GetInputData;
var NoBytes : cardinal;
begin
  // Read some more data into the look ahead zone.
  NoBytes := FBufferEnd - FLookAheadEnd;
  if Assigned(FOnNeedData)
  then FOnNeedData(Self, FLookAheadEnd, NoBytes)
  else NoBytes := 0;
  inc(FLookAheadEnd, NoBytes);
end; // TmcmSlidingWindow.GetInputData.


procedure TmcmSlidingWindow.AdvanceAfterAdd(Count : integer);
begin
  // Advance the start of the sliding window, if required
  if ((FCurrent - FStart) >= FWindowSize)
  then begin
       inc(FStart, Count);
       inc(FStartOffset, Count);
  end;

  // Advance the current pointer.
  inc(FCurrent, Count);

  // Check to see if we have advanced into the overflow zone.
  if (FStart >= FMidPoint)
  then begin
       // Write some more data to the stream (from FBuffer to FStart).
       WriteToStream(False);

       // Move current data back to the start of the buffer.
       Move(FStart^, FBuffer^, FCurrent - FStart);

       // Reset the various pointers.
       dec(FCurrent, FStart - FBuffer);
       FStart := FBuffer;
  end;
end; // TmcmSlidingWindow.AdvanceAfterAdd.


procedure TmcmSlidingWindow.Advance(Count : cardinal);
var ByteCount : longint;
begin
  // Advance the start of the sliding window, if required.
  if ((FCurrent - FStart) >= FWindowSize)
  then begin
       inc(FStart, Count);
       inc(FStartOffset, Count);
  end;

  // Advance the current pointer.
  inc(FCurrent, Count);

  // Check to see if we have advanced into the overflow zone.
  if (FStart >= FMidPoint)
  then begin
       // Move current data back to the start of the buffer.
       ByteCount := FLookAheadEnd - FStart;
       Move(FStart^, FBuffer^, ByteCount);

       // Reset the various pointers.
       ByteCount := FStart - FBuffer;
       FStart := FBuffer;
       dec(FCurrent, ByteCount);
       dec(FLookAheadEnd, ByteCount);

       // Read some more data from the stream.
       GetInputData;
  end;
end; // TmcmSlidingWindow.Advance.


function TmcmSlidingWindow.Compare(Offset : longint; var Distance : longint) : longint;
var MatchStr  : PAnsiChar;
    CurrentCh : PAnsiChar;
begin
  // Note: When this routine is called it is assumed that at least three
  //       characters will match between the passed position and the
  //       current position}
  // Calculate the position in the sliding window for the passed offset
  // and its distance from the current position.
  MatchStr := FStart + (Offset - FStartOffset);
  Distance := FCurrent - MatchStr;
  inc(MatchStr, 3);

  // Calculate the length of the matching characters between this and
  // the current position. Don't go above the maximum length. Have a
  // special case for the end of the input stream.
  Result := 3;
  CurrentCh := FCurrent + 3;
  if (CurrentCh <> FLookAheadEnd)
  then begin
       while (Result < FMaxMatchLength) and
             (MatchStr^ = CurrentCh^)
       do begin
          inc(Result);
          inc(MatchStr);
          inc(CurrentCh);
          if (CurrentCh = FLookAheadEnd)
          then Break;
       end;
  end;
end; // TmcmSlidingWindow.Compare.


procedure TmcmSlidingWindow.GetNextKey(var Key    : THashKey;
                                       var Offset : longint);
var P : PAnsiChar;
    i : integer;
begin
  // Calculate the length of the match string; usually it's 3, but at
  // the end of the input stream it could be 2 or less.
  if ((FLookAheadEnd - FCurrent) < 3)
  then Key.AsString[0] := AnsiChar(FLookAheadEnd - FCurrent)
  else Key.AsString[0] := #3;
  P := FCurrent;
  for i := 1 to Length(Key.AsString)
  do begin
     Key.AsString[i] := P^;
     inc(P);
  end;
  Offset := FStartOffset + (FCurrent - FStart);
end; // TmcmSlidingWindow.GetNextKey.


procedure TmcmSlidingWindow.AddByte(NewByte : byte);
begin
  // Decompression ONLY.

  // Add the byte to the buffer.
  FCurrent^ := PAnsiChar(@NewByte)^;

  // Advance the start of the sliding window.
  AdvanceAfterAdd(1);
end; // TmcmSlidingWindow.AddByte.


procedure TmcmSlidingWindow.AddDistLen(Distance : integer; Length : integer);
var FromByte : PAnsiChar;
    ToByte   : PAnsiChar;
    i        : integer;
begin
  // Decompression ONLY.

  // Set up the pointers to do the data copy. Note we cannot use Move
  // since part of the data we are copying may be set up by the actual
  // copying of the data.
  FromByte := FCurrent - Distance;
  ToByte := FCurrent;
  for i := 1 to Length
  do begin
     ToByte^ := FromByte^;
     inc(FromByte);
     inc(ToByte);
  end;

  // Advance the start of the sliding window.
  AdvanceAfterAdd(Length);
end; // TmcmSlidingWindow.CopyCode.


end.
