unit SciZipFile;
// Copyright 2005 Patrik Spanel
// scilib@sendme.cz
// Written from scratch using InfoZip PKZip file specification application note
// ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
// uses the Borland out of the box zlib

// 2011 FPC compatibility and new function IndexOfFile by Precision software & consulting
// 2009 Delphi 2009 compatibility by Precision software & consulting
// 2005 Added support for streams (LoadFromStream(const ZipFileStream: TStream),SaveToStream(...))
// Nick Naimo <nick@naimo.com> added support for folders on 6/29/2004
// Marcin Wojda <Marcin@sacer.com.pl> added exceptions and try finally blocks
// Jarek Stok³osa <jarek.stoklosa@gmail.com> 11/04/2008 added support for additional file descriptor(LoadFromStream and SaveToStream), add const section;

interface

uses SysUtils, Classes, {$IFDEF FPC} zstream {$ELSE} zlib {$ENDIF}, Windows;

type
  TFileDescriptor = packed record
    Crc32: DWORD; //                          4 bytes
    CompressedSize: DWORD; //                 4 bytes
    UncompressedSize: DWORD; //               4 bytes
  end;

  TCommonFileHeader = packed record
    VersionNeededToExtract: WORD; //       2 bytes
    GeneralPurposeBitFlag: WORD; //        2 bytes
    CompressionMethod: WORD; //              2 bytes
    LastModFileTimeDate: DWORD; //             4 bytes
    FileDescriptor:TFileDescriptor;
    FilenameLength: WORD; //                 2 bytes
    ExtraFieldLength: WORD; //              2 bytes
  end;

  TLocalFile = packed record
    LocalFileHeaderSignature: DWORD; //     4 bytes  (0x04034b50)
    CommonFileHeader: TCommonFileHeader; //
    filename: AnsiString; //variable size
    extrafield: AnsiString; //variable size
    CompressedData: AnsiString; //variable size
  end;

  TFileHeader = packed record
    CentralFileHeaderSignature: DWORD; //   4 bytes  (0x02014b50)
    VersionMadeBy: WORD; //                 2 bytes
    CommonFileHeader: TCommonFileHeader; //
    FileCommentLength: WORD; //             2 bytes
    DiskNumberStart: WORD; //               2 bytes
    InternalFileAttributes: WORD; //        2 bytes
    ExternalFileAttributes: DWORD; //        4 bytes
    RelativeOffsetOfLocalHeader: DWORD; // 4 bytes
    filename: AnsiString; //variable size
    extrafield: AnsiString; //variable size
    fileComment: AnsiString; //variable size
  end;

  TEndOfCentralDir = packed record
    EndOfCentralDirSignature: DWORD; //    4 bytes  (0x06054b50)
    NumberOfThisDisk: WORD; //             2 bytes
    NumberOfTheDiskWithTheStart: WORD; //  2 bytes
    TotalNumberOfEntriesOnThisDisk: WORD; //    2 bytes
    TotalNumberOfEntries: WORD; //            2 bytes
    SizeOfTheCentralDirectory: DWORD; //   4 bytes
    OffsetOfStartOfCentralDirectory: DWORD; // 4 bytes
    ZipfileCommentLength: WORD; //          2 bytes
  end;

  TZipFile = class(TObject)
    Files: array of TLocalFile;
    CentralDirectory: array of TFileHeader;
    EndOfCentralDirectory: TEndOfCentralDir;
    ZipFileComment: AnsiString;
  private
    FLevel:TCompressionLevel;
    FCMethod:Word;
    function GetUncompressed(i: integer): AnsiString;
    procedure SetUncompressed(i: integer; const Value: AnsiString);
    function GetDateTime(i: integer): TDateTime;
    procedure SetDateTime(i: integer; const Value: TDateTime);
    function GetCount: integer;
    function GetName(i: integer): AnsiString;
    procedure SetName(i: integer; const Value: AnsiString);
    procedure SetLevel(i: TCompressionLevel);
  public
    constructor Create;
    property Count: integer read GetCount;
    procedure AddFile(const name: AnsiString; FAttribute: DWord = 0);
    procedure SaveToFile(const filename: String);
    procedure SaveToStream(ZipFileStream: TStream);
    procedure LoadFromFile(const filename: String);
    procedure LoadFromStream(const ZipFileStream: TStream);
    procedure SetDataEx(i: integer; const Value: AnsiString; CompressionMethod:word);
    function IndexOfFile(FileName:AnsiString):Integer;
    property Uncompressed[i: integer]: AnsiString read GetUncompressed write SetUncompressed;
    property Data[i: integer]: AnsiString read GetUncompressed write SetUncompressed;
    property DateTime[i: integer]: TDateTime read GetDateTime write SetDateTime;
    property Name[i: integer]: AnsiString read GetName write SetName;
    property Level:TCompressionLevel read FLevel write SetLevel;
  end;

  EZipFileCRCError = class(Exception);

  const
   dwLocalFileHeaderSignature = $04034B50;
   dwLocalFileDescriptorSignature = $08074B50;
   dwCentralFileHeaderSignature = $02014B50;
   dwEndOfCentralDirSignature = $06054b50;

function ZipCRC32(const Data: AnsiString): longword;

implementation

{ TZipFile }

constructor TZipFile.Create;
begin
  inherited;
  FLevel:=clDefault;
  FCMethod:=8; // ZLib deflated
end;

procedure TZipFile.SaveToFile(const filename: String);
var
  ZipFileStream: TFileStream;
begin
  ZipFileStream := TFileStream.Create(filename, fmCreate);
  try
  SaveToStream(ZipFileStream);
  finally
    ZipFileStream.Free;
  end;
end;

procedure TZipFile.SaveToStream(ZipFileStream: TStream);
var
  i: integer;
  dw:DWORD;
  cfh:TCommonFileHeader;
  additionalDescriptor:Boolean;
begin
    for i := 0 to High(Files) do
      with Files[i] do
      begin
        CentralDirectory[i].RelativeOffsetOfLocalHeader := ZipFileStream.Position;
        ZipFileStream.Write(LocalFileHeaderSignature, 4);
        if (LocalFileHeaderSignature = (dwLocalFileHeaderSignature)) then
        begin
          cfh := CommonFileHeader;
          additionalDescriptor :=((cfh.GeneralPurposeBitFlag AND 8) = 8);
          if additionalDescriptor then
          begin
             cfh.FileDescriptor.Crc32 := 0;
             cfh.FileDescriptor.CompressedSize := 0;
             cfh.FileDescriptor.UncompressedSize := 0;
             ZipFileStream.Write(cfh, SizeOf(cfh));
          end
          else
            ZipFileStream.Write(CommonFileHeader, SizeOf(CommonFileHeader));
          ZipFileStream.Write(PAnsiChar(filename)^, CommonFileHeader.FilenameLength);
          ZipFileStream.Write(PAnsiChar(extrafield)^, CommonFileHeader.ExtraFieldLength);
          ZipFileStream.Write(PAnsiChar(CompressedData)^, CommonFileHeader.FileDescriptor.CompressedSize);
          if additionalDescriptor then
          begin
            dw := dwLocalFileDescriptorSignature;
            ZipFileStream.Write(dw, 4);
            ZipFileStream.Write(CommonFileHeader.FileDescriptor, SizeOf(CommonFileHeader.FileDescriptor));
          end
        end;
      end;

    EndOfCentralDirectory.OffsetOfStartOfCentralDirectory := ZipFileStream.Position;

    for i := 0 to High(CentralDirectory) do
      with CentralDirectory[i] do
      begin
        ZipFileStream.Write(CentralFileHeaderSignature, 4);
        ZipFileStream.Write(VersionMadeBy, 2);
        ZipFileStream.Write(CommonFileHeader, SizeOf(CommonFileHeader));
        ZipFileStream.Write(FileCommentLength, 2);
        ZipFileStream.Write(DiskNumberStart, 2);
        ZipFileStream.Write(InternalFileAttributes, 2);
        ZipFileStream.Write(ExternalFileAttributes, 4);
        ZipFileStream.Write(RelativeOffsetOfLocalHeader, 4);
        ZipFileStream.Write(PAnsiChar(filename)^, length(filename));
        ZipFileStream.Write(PAnsiChar(extrafield)^, length(extrafield));
        ZipFileStream.Write(PAnsiChar(fileComment)^, length(fileComment));
      end;
    with EndOfCentralDirectory do
    begin
      EndOfCentralDirSignature := dwEndOfCentralDirSignature;
      NumberOfThisDisk := 0;
      NumberOfTheDiskWithTheStart := 0;
      TotalNumberOfEntriesOnThisDisk := High(Files) + 1;
      TotalNumberOfEntries := High(Files) + 1;
      SizeOfTheCentralDirectory :=
        ZipFileStream.Position - OffsetOfStartOfCentralDirectory;
      ZipfileCommentLength := length(ZipFileComment);
    end;
    ZipFileStream.Write(EndOfCentralDirectory, SizeOf(EndOfCentralDirectory));
    ZipFileStream.Write(PAnsiChar(ZipFileComment)^, length(ZipFileComment));
end;


procedure TZipFile.LoadFromStream(const ZipFileStream: TStream);
var
  n: integer;
  signature: DWORD;
  rawData:AnsiString;
  searchSignature:DWORD;
  c:AnsiChar;
begin
  n := 0;
  repeat
    signature := 0;
    ZipFileStream.Read(signature, 4);
    if   (ZipFileStream.Position =  ZipFileStream.Size) then exit;
  until signature = dwLocalFileHeaderSignature;
  repeat
    begin
      if (signature = dwLocalFileHeaderSignature) then
      begin
        inc(n);
        SetLength(Files, n);
        SetLength(CentralDirectory, n);
        with Files[n - 1] do
        begin
          LocalFileHeaderSignature := signature;
          ZipFileStream.Read(CommonFileHeader, SizeOf(CommonFileHeader));
          SetLength(filename, CommonFileHeader.FilenameLength);
          ZipFileStream.Read(PAnsiChar(filename)^, CommonFileHeader.FilenameLength);
          SetLength(extrafield, CommonFileHeader.ExtraFieldLength);
          ZipFileStream.Read(PAnsiChar(extrafield)^, CommonFileHeader.ExtraFieldLength);
          if ((CommonFileHeader.GeneralPurposeBitFlag and 8) = 8) then
          begin
            searchSignature := 0;
            rawData := '';
            repeat
              ZipFileStream.Read(searchSignature, 4);
              if searchSignature <> dwLocalFileDescriptorSignature then
              begin
                ZipFileStream.Seek(-4, soFromCurrent);
                ZipFileStream.Read(c, SizeOf(c));
                rawData := rawData + c;
              end;
            until  searchSignature = dwLocalFileDescriptorSignature;
            CompressedData := rawData;
            ZipFileStream.Read(CommonFileHeader.FileDescriptor, SizeOf(CommonFileHeader.FileDescriptor));
          end
          else
          begin
            SetLength(CompressedData, CommonFileHeader.FileDescriptor.CompressedSize);
            ZipFileStream.Read(PAnsiChar(CompressedData)^, CommonFileHeader.FileDescriptor.CompressedSize);
          end;
        end;
      end;
    end;
    signature := 0;
    ZipFileStream.Read(signature, 4);
  until signature <> (dwLocalFileHeaderSignature);

  n := 0;
  repeat
    begin
      if (signature = dwCentralFileHeaderSignature) then
      begin
        inc(n);
        with CentralDirectory[n - 1] do
        begin
          CentralFileHeaderSignature := signature;
          ZipFileStream.Read(VersionMadeBy, 2);
          ZipFileStream.Read(CommonFileHeader, SizeOf(CommonFileHeader));
          ZipFileStream.Read(FileCommentLength, 2);
          ZipFileStream.Read(DiskNumberStart, 2);
          ZipFileStream.Read(InternalFileAttributes, 2);
          ZipFileStream.Read(ExternalFileAttributes, 4);
          ZipFileStream.Read(RelativeOffsetOfLocalHeader, 4);
          SetLength(filename, CommonFileHeader.FilenameLength);
          ZipFileStream.Read(PAnsiChar(filename)^, CommonFileHeader.FilenameLength);
          SetLength(extrafield, CommonFileHeader.ExtraFieldLength);
          ZipFileStream.Read(PAnsiChar(extrafield)^, CommonFileHeader.ExtraFieldLength);
          SetLength(fileComment, FileCommentLength);
          ZipFileStream.Read(PAnsiChar(fileComment)^, FileCommentLength);
        end;
      end;
    end;
    signature := 0;
    ZipFileStream.Read(signature, 4);
  until signature <> (dwCentralFileHeaderSignature);
  if signature = dwEndOfCentralDirSignature then
  begin
    EndOfCentralDirectory.EndOfCentralDirSignature := Signature;
    ZipFileStream.Read(EndOfCentralDirectory.NumberOfThisDisk,
      SizeOf(EndOfCentralDirectory) - 4);
    SetLength(ZipFileComment, EndOfCentralDirectory.ZipfileCommentLength);
    ZipFileStream.Read(PAnsiChar(ZipFileComment)^, EndOfCentralDirectory.ZipfileCommentLength);
  end;
end;

procedure TZipFile.LoadFromFile(const filename: String);
var
  ZipFileStream: TFileStream;
begin
  ZipFileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(ZipFileStream);
  finally
    ZipFileStream.Free;
  end;
end;

function TZipFile.GetUncompressed(i: integer): AnsiString;
var
  Decompressor: TDecompressionStream;
  UncompressedStream: TStringStream;
  Aheader: AnsiString;
  ReadBytes: integer;
  LoadedCrc32: DWORD;
begin
  if (i < 0) or (i > High(Files)) then
    //  begin
    //    result := '';
    //    exit;
    //  end;
    raise Exception.Create('Index out of range.');
  Aheader := #120#156;// AnsiChar(chr(120)) + AnsiChar(chr(156));
  //manufacture a 2 byte header for zlib; 4 byte footer is not required.
  UncompressedStream := TStringStream.Create(Aheader + Files[i].CompressedData);
  try {+}
    Decompressor := TDecompressionStream.Create(UncompressedStream);
    try {+}
      SetLength(Result, Files[i].CommonFileHeader.FileDescriptor.UncompressedSize);
      ReadBytes := Decompressor.Read(PAnsiChar(Result)^,
        Files[i].CommonFileHeader.FileDescriptor.UncompressedSize);
      if ReadBytes <> integer(Files[i].CommonFileHeader.FileDescriptor.UncompressedSize) then
        Result := '';
    finally
      Decompressor.Free;
    end;
  finally
    UncompressedStream.Free;
  end;

  LoadedCRC32 := ZipCRC32(Result);
  if LoadedCRC32 <> Files[i].CommonFileHeader.FileDescriptor.Crc32 then
    // - Result := '';
    raise EZipFileCRCError.CreateFmt('CRC Error in "%s".', [Files[i].filename]);
end;

procedure TZipFile.SetUncompressed(i: integer; const Value: AnsiString);
var
  Compressor: TCompressionStream;
  CompressedStream: TStringStream;
begin
  if i > High(Files) then // exit;
    raise Exception.Create('Index out of range.');
  compressedStream := TStringStream.Create('');
  try {+}
    if FCMethod=0 then
      Files[i].CompressedData := Value
    else
    begin
      compressor := TcompressionStream.Create(FLevel, CompressedStream);
      try {+}
        compressor.Write(PAnsiChar(Value)^, length(Value));
      finally
        compressor.Free;
      end;
      Files[i].CompressedData := AnsiString(Copy(compressedStream.DataString, 3,
        length(compressedStream.DataString) - 6));
    end;
    //strip the 2 byte headers and 4 byte footers
    Files[i].LocalFileHeaderSignature := (dwLocalFileHeaderSignature);
    with Files[i].CommonFileHeader do
    begin
      VersionNeededToExtract := 20;
      GeneralPurposeBitFlag := 0;
      CompressionMethod := FCMethod; // =8; // Zlib
      if LastModFileTimeDate = 0 then
        LastModFileTimeDate := DateTimeToFileDate(Now);
      FileDescriptor.Crc32 := ZipCRC32(Value);
      FileDescriptor.CompressedSize := length(Files[i].CompressedData);
      FileDescriptor.UncompressedSize := length(Value);
      FilenameLength := length(Files[i].filename);
      ExtraFieldLength := length(Files[i].extrafield);
    end;

    with CentralDirectory[i] do
    begin
      CentralFileHeaderSignature := dwCentralFileHeaderSignature;
      VersionMadeBy := 20;
      CommonFileHeader := Files[i].CommonFileHeader;
      FileCommentLength := 0;
      DiskNumberStart := 0;
      InternalFileAttributes := 0;
      //      ExternalFileAttributes := 0;
      RelativeOffsetOfLocalHeader := 0;
      filename := Files[i].filename;
      extrafield := Files[i].extrafield;
      fileComment := '';
    end;
  finally
    compressedStream.Free;
  end;
end;

procedure TZipFile.SetDataEx(i: integer; const Value: AnsiString; CompressionMethod:word);
var
  ol:WORD;
begin
  ol:=FCMethod;
  FCMethod:=CompressionMethod;
  SetUncompressed(i,Value);
  FCMethod:=ol;
end;

function TZipFile.IndexOfFile(FileName:AnsiString):Integer;
var
  k:Integer;
begin
  Result:=-1;
  for k:=0 To Count-1 do
    if Name[k]=FileName then
    begin
      Result:=k;
      Break;
    end;
end;

procedure TZipFile.AddFile(const name: AnsiString; FAttribute: DWord = 0);
begin
  SetLength(Files, High(Files) + 2);
  SetLength(CentralDirectory, length(Files));
  Files[High(Files)].filename := name;
  Files[High(Files)].CompressedData := ''; //start with an empty file
  Files[High(Files)].extrafield := '';
  Files[High(Files)].LocalFileHeaderSignature := dwLocalFileHeaderSignature;
  with Files[High(Files)].CommonFileHeader do
  begin
    VersionNeededToExtract := 20;
    GeneralPurposeBitFlag := 0;
    CompressionMethod := FCMethod; // =8; // Zlib
    LastModFileTimeDate := 0;
    FileDescriptor.Crc32 := 0;
    FileDescriptor.CompressedSize := 0;
    FileDescriptor.UncompressedSize := 0;
    FilenameLength := length(Files[High(Files)].filename);
    ExtraFieldLength := length(Files[High(Files)].extrafield);
  end;

  with CentralDirectory[High(Files)] do
  begin
    CentralFileHeaderSignature := dwCentralFileHeaderSignature;
    VersionMadeBy := 20;
    CommonFileHeader := Files[High(Files)].CommonFileHeader;
    FileCommentLength := 0;
    DiskNumberStart := 0;
    InternalFileAttributes := 0;
    ExternalFileAttributes := FAttribute;
    RelativeOffsetOfLocalHeader := 0;
    filename := Files[High(Files)].filename;
    extrafield := Files[High(Files)].extrafield;
    fileComment := '';
  end;
end;

function TZipFile.GetDateTime(i: integer): TDateTime;
begin
  if i > High(Files) then // begin Result:=0; exit; end;
    raise Exception.Create('Index out of range.');
  result := FileDateToDateTime(Files[i].CommonFileHeader.LastModFileTimeDate);
end;

procedure TZipFile.SetDateTime(i: integer; const Value: TDateTime);
begin
  if i > High(Files) then //exit;
    raise Exception.Create('Index out of range.');
  Files[i].CommonFileHeader.LastModFileTimeDate := DateTimeToFileDate(Value);
end;

function TZipFile.GetCount: integer;
begin
  Result := High(Files) + 1;
end;

function TZipFile.GetName(i: integer): AnsiString;
begin
  Result := Files[i].filename;
end;

procedure TZipFile.SetName(i: integer; const Value: AnsiString);
begin
  Files[i].filename := Value;
end;

procedure TZipFile.SetLevel(i: TCompressionLevel);
begin
  if (FLevel<>i) and (Count=0) then
    FLevel:=i;
end;

{ ZipCRC32 }

//calculates the zipfile CRC32 value from a AnsiString

function ZipCRC32(const Data: AnsiString): longword;
const
  CRCtable: array[0..255] of DWORD = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535,
    $9E6495A3, $0EDB8832, $79DCB8A4,
    $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, $1DB71064,
    $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A,
    $8A65C9EC, $14015C4F, $63066CD9,
    $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1,
    $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF,
    $ABD13D59, $26D930AC, $51DE003A,
    $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F, $2802B89E,
    $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190, $01DB7106, $98D220BC,
    $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB,
    $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1,
    $F50FC457, $65B0D9C6, $12B7E950,
    $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158,
    $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846,
    $DA60B8D0, $44042D73, $33031DE5,
    $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525,
    $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B,
    $C0BA6CAD, $EDB88320, $9ABFB3B6,
    $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683, $E3630B12,
    $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268,
    $6906C2FE, $F762575D, $806567CB,
    $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F,
    $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD,
    $48B2364B, $D80D2BDA, $AF0A1B4C,
    $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79, $CB61B38C,
    $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92,
    $5CB36A04, $C2D7FFA7, $B5D0CF31,
    $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9,
    $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7,
    $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6,
    $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45, $A00AE278, $D70DD2EE, $4E048354,
    $3903B3C2, $A7672661, $D06016F7,
    $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53,
    $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729,
    $23D967BF, $B3667A2E, $C4614AB8,
    $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);
var
  i: integer;
begin
  result := $FFFFFFFF;
  for i := 0 to length(Data) - 1 do
    result := (result shr 8) xor (CRCtable[byte(result) xor Ord(Data[i + 1])]);
  result := result xor $FFFFFFFF;
end;

end.

