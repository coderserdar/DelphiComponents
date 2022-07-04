{ RLECOMP.PAS for TCompress v2.5 -- no change from 2.0

 TCompress Event Handler examples & RLE compression source example

 This file contains (near the bottom) examples of valid handlers for
 OnCompress, OnRecognize and OnExpand events. The rest of the file
 is routines to perform RLE compression, designed to be called by our
 event handlers. The RLE compression is the same as that built-in to TCompress,
 but uses the 'RLX' compression ID -- if it used 'RLE', TCompress would never
 call it for expansion!

 You are free to use this source code as you wish.

 (To use, transfer it ALL into a working form, make proper form object
 declarations for the event handlers at the bottom, AND point the
 Compress object's OnCompress/OnExpand and OnRecognize events at them)

}

const CustomCode = 'RLX' ; { this one is a *custom* RLE handler }
   ChunkSize = 8192;       { work with 8K chunks }


{ Variables for ProcessStreams, getchar, putchar etc. }
var AtStart, InputEOF, inRepeat: Boolean;
    inBuffer, outBuffer: PChar;
    inBmax, inBptr, outBmax, outBptr: Pchar;
    source, dest: TStream;
    lastch: Char;
    DupCount : Integer;
    InChecksum, Outchecksum, Readsize, lChunk, BytesOut: Longint;

const RLEescapechar:char=#148; { thus 65 148 37 is 37 A's, and 148 00 is ONE 148 }

{ Generic routines to get and put characters, buffered. Should not
  change from one compression approach to another... }

function GetChar: Char;
var BytesRead: LongInt;
begin
  if inBptr = inBMax then { done buffer }
  begin
     Application.ProcessMessages;
     Result := #0;
     InputEOF := True; { precautionary }
     if readsize=0 then
        exit; { no more boss }
     if lChunk > readsize then
        lChunk := readsize;
     BytesRead := source.Read(inBuffer^, lChunk); { read chunk }
     readsize:=readsize-BytesRead;
     if BytesRead = 0 then { EOF }
        exit
     else
     begin
       InputEOF := False; { keep on in there... }
       inBmax := inBuffer+BytesRead;
       inBptr := inBuffer;
     end;
  end;
  Result := inBptr^;
  InCheckSum:= InChecksum+Ord(Result);
  Inc(inBptr);
end;

procedure PutChar(ch: Char);
begin
  if outBptr = outBmax then { filled buffer }
  begin
    Application.ProcessMessages;
    dest.writebuffer(OutBuffer^,ChunkSize);
    outBptr := outBuffer;
  end;
  outBptr^ := ch;
  OutCheckSum:= OutChecksum+Ord(ch);
  Inc(outBptr);
  Inc(BytesOut);
end;


{ Start of RLE-specific code }

procedure emit(count: Integer; ch: char);
begin
  if (count > 2) or (count=0) then {only emit if worth it }
  begin
    putChar(RLEescapechar);
    putChar(chr(count));
  end else
  begin
     Dec(count);
     while count > 0 do begin putChar(ch); Dec(count) end;
  end;
end;


procedure CompressRLE;
var ch: Char;
begin
 while True do
 begin
   ch:= GetChar;
   if InputEOF then
   begin
     if inRepeat then
        emit(Dupcount,lastch); { flag the repeat }
     break;
   end;
   if inRepeat then
   begin
      if (lastch = ch) and (DupCount<255) then
        Inc(DupCount) { and stay in inRepeat }
      else
      begin
        emit(DupCount,lastch); { however many }
        lastch := ch;
        if ch=RLEescapechar then
        begin
           emit(0,RLEEscapechar); { flag it }
        end else
           Putchar(ch);
        inRepeat := False;
      end;
   end else
   begin
     if (ch=RLEescapechar) then
        emit(0,ch)
     else if (ch=lastch) and not AtStart then
     begin
        DupCount := 2;
        inRepeat := True;
     end else Putchar(ch);
     lastch := ch;
   end;
   AtStart := False;
 end; { While not InputEOF }
end;

procedure ExpandRLE;
var ch: Char;
begin
 while True do
 begin
   ch:= GetChar;
   if InputEOF then
     break; { done, at last... }
   if ch<> RLEescapechar then
     Putchar(ch)
   else { ok, get a count... MUST be there, really! }
   begin
     DupCount := Ord(GetChar); { 0 if EOF, but not legal, however... }
     if DupCount=0 then Putchar(RLEEscapechar) { special flag }
     else
     begin
        Dec(DupCount);  { because one was already IN the bytestream }
        while Dupcount>0 do begin Putchar(lastch); Dec(DupCount) end;
     end;
   end;
   lastch := ch;
 end; { while }
end;

{ END of RLE }


{ The main handler -- this shouldn't change from compression method to
  compression method -- it just calls what it should... }

function ProcessStreams(outstream, instream: TStream; size: longint;
         var checksum: Longint; mode: TCProcessMode): longint;
begin
  source := inStream; { messy, but allows modular routines w/o zillions of parameters }
  dest := outStream;
  GetMem(inBuffer, ChunkSize); { allocate the buffers }
  inBMax := inBuffer; { initially, until first read... }
  inBptr := inBuffer;
  GetMem(outBuffer, ChunkSize);
  outBMax := outBuffer+ChunkSize;
  outBptr := outBuffer; { not same as inBptr! }
  InputEOF := False;
  AtStart := True;
  lastch := #0;
  inRepeat := False;
  dupCount := 0;
  ReadSize := size;
  lChunk := Chunksize;
  inChecksum:= 0;
  outChecksum := 0;
  try
   if mode = cmCompress then
   begin
    BytesOut:=0;
    CompressRLE;
    checksum := InChecksum;
   end else { expand }
   begin
    BytesOut:=1;
    ExpandRLE;
    checksum := OutChecksum;
   end;
   if outBptr<>OutBuffer then { must flush }
     dest.WriteBuffer(OutBuffer^,outBptr-OutBuffer);
  finally
   FreeMem(inBuffer, ChunkSize); { free the buffer }
   FreeMem(outBuffer, ChunkSize);
  end;
  Result := BytesOut;
end;

{ Now the custom event handlers which provide hooks from TCompress into
  the above code... }

{ NOTE: Make CompressID below an OpenString if using this in Delphi 1.0
  -- don't forget the Form-level declaration too... }
procedure TForm1.Compress1Compress(dest, source: TStream;
  var CompressID: String; var Outputsize, checksum: Longint);
begin
  OutputSize := ProcessStreams(dest,source,source.size,checksum, cmCompress);
  CompressID := CustomCode;
end;

procedure TForm1.Compress1Recognize(CompressID: String;
var recognized: Boolean);
begin
  if CompressID = CustomCode then recognized := True; { easy, yes? }
end;

procedure TForm1.Compress1Expand(dest, source: TStream;
  Sourcesize, DestSize: Longint; CompressID: String; var checksum: Longint);
begin { could check CompressID for more detail, but no need... Destsize not needed either}
  ProcessStreams(dest,source,Sourcesize,checksum, cmExpand);
end;

