unit API_SRecordFile;

//------------------------------------------------------------------------------
// API_SRecordFile: for parsing Motorola S-Records files
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
//
// 02092009, ari pikivirta
//  * seems to parse all MOT files okay
//  * checks internally also each line checksum
//  * lines can be parsed separately
//
//------------------------------------------------------------------------------

interface

uses
  windows, sysutils, classes;

const
  MAXMOTDATASIZE = $1f;                           // might have to be increased.. ?

type
  TMotRecord = record
    RecordType: Integer;                          // Sn
    Length: Integer;                              // 00
    Address: Integer;                             // 0000
    Data: array[0..MAXMOTDATASIZE-1] of byte;     // only filled in case data exists on parse
    DataLength: Integer;                          // only length of data content
    Checksum: Integer;                            // 00
    ChecksumOk: Boolean;                          // checked on parsing line
  end;

function Mot2Bin(
  Const ASourceFile: String; var AStream: TStream;
  var AHeaderAsString: String;
  Const AOffset: Integer = $000000;
  Const AFillChar: Byte = $ff)
  : Integer; // returns file size as result
  overload;

function Mot2Bin(
  Const ASourceFile, ATargetFile: String;
  var AHeaderAsString: String;
  Const AOffset: Integer = $000000;
  Const AFillChar: Byte = $ff)
  : Integer; // returns file size as result
  overload;

function ParseMotLine(Const Line: String): TMotRecord;

implementation

uses
  api_strings, dialogs;

//------------------------------------------------------------------------------
function ParseMotLine(Const Line: String): TMotRecord;
var
  p, AddressLength: integer;
  IntChecksum: Byte;
begin
  // S0 = header (sometimes file name is found here..)
  // S1, S2, S3 = data sequence (address size differs 8, 16, 24 bits)
  // S5 = record count (hmm...)
  // S7, S8, S9 = end of block; Note: The address may contain a starting address for the program
  //
  // init variables
  result.RecordType:= -1;
  result.Address:= 0;
  result.length:= 0;
  result.DataLength:= 0;
  result.Checksum:= 0;
  result.ChecksumOk:= FALSE;
  //
  if length(line)<11 then exit; // exit if line is empty or not enough data
  if line[1]<>'S' then exit; // not valid line
  //
  // Line type
  result.RecordType:= strtoint(copy(line, 2, 1)); // Sn, n = integer
  result.Length:= strtoint('$'+copy(line,3,2)); // length of data
  if (result.length>2) then // check if address and checksum exists
  begin
    result.Checksum:= strtoint('$'+copy(line, length(line)-1, 2)); // checksum
    //
    // check address length from recordtype
    if (result.recordtype=0) or (result.RecordType=1) or (result.recordtype=7) then AddressLength:= 2 else
    if (result.RecordType=2) or (result.recordtype=8) then AddressLength:= 3 else
    if (result.RecordType=3) or (result.recordtype=9) then AddressLength:= 4 else
      AddressLength:= 1;
    //
    // parse message contents
    result.datalength:= result.length-(Addresslength+1); // totallength - (addresslength + checksum)
    result.Address:= strtoint('$'+copy(line,5,(2*AddressLength))); // address
    for p:=0 to (result.datalength-1) do // read data only
      result.data[p]:= strtoint('$'+copy(line,((5+(AddressLength*2))+2*p), 2));
    //
    // DEBUGGIN..
    (*
    s:= '';
    for p:=0 to result.dataLength-1 do
      if (char(result.data[p]) in ['0'..'9', 'a'..'z', 'A'..'Z']) then
        s:= s + char(result.Data[p]) else
        s:= s + '.';
    showmessage(
      'Recordtype = '+inttostr(result.RecordType)+#13+
      'Length = '+inttostr(result.Length)+#13+
      'Address = '+inttohex(result.Address,6)+#13+
      'Data Length = '+inttostr(result.DataLength)+#13+
      'Data = '+s+#13+
      'Checksum = '+inttohex(result.Checksum,2)
      );
    *)
    //
    // Checksum, two hex digits - the least significant byte of one's complement
    // of the sum of the values represented by the two hex digit pairs for the
    // byte count, address and data fields.
    IntChecksum:= $ff;
    p:= 0;
    while ((4+p)<length(line)) do
    begin
      IntChecksum:= IntChecksum-strtoint('$'+copy(line,(4+p),2));
      p:= p + 2;
    end;
    result.ChecksumOk:= (IntChecksum=result.Checksum);
  end;

end;

//------------------------------------------------------------------------------
function Mot2Bin(Const ASourceFile: String; var AStream: TStream;
  var AHeaderAsString: String;
  Const AOffset: Integer = $000000; Const AFillChar: Byte = $ff): Integer;
var
  motlist: tstringlist;
  i: integer;
  APos: Integer;
  MRec: TMotRecord;
  Buf: array[0..MAXMOTDATASIZE-1] of byte;
begin
  AHeaderAsString:= '';
  if not assigned(AStream) then
    raise exception.create('Input stream is not assigned.');
  //
  motlist:= tstringlist.create;
  try
    AStream.Seek(0, SoFromBeginning);
    //
    if not api_Strings.OpenFileToStrings(ASourceFile, motlist, TRUE) then
      raise exception.create('Failed to open MOT file for reading.');
    //
    for i:=0 to motlist.count-1 do
    if motlist[i]<>'' then
    begin
      MRec:= ParseMotLine(motlist[i]);
      if (MRec.RecordType=0) then // read header
      begin
        for Apos:=0 to MRec.dataLength-1 do
          AHeaderAsString:= AHeaderAsString + char(Mrec.Data[Apos]);
      end else
      if (MRec.RecordType>0) then // fill in the data
      begin
        // fill/seek to correct position, will be ignored
        // in case the stream position is already at correct place.
        buf[0]:= AFillChar;
        while (Astream.Position<MRec.Address+AOffSet) do AStream.Write(Buf, 1);
        // write record data that was found on the mot line
        AStream.WriteBuffer(Mrec.Data, MRec.dataLength);
      end;
    end;
  finally
    motlist.free;
  end;
  //
  result:= AStream.Size;
end;

//------------------------------------------------------------------------------
function Mot2Bin(Const ASourceFile, ATargetFile: String;
  var AHeaderAsString: String;
  Const AOffset: Integer = $000000; Const AFillChar: Byte = $ff): Integer;
var
  fs: TFileStream;
begin
  Fs:= tfilestream.create(ATargetfile, fmopenwrite or fmcreate);
  try
    result:= Mot2Bin(ASourceFile, tstream(Fs), AheaderasString, AOffset, AFillchar);
  finally
    fs.free;
  end;
end;

end.
