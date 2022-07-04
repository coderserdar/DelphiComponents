unit SXZipUtils;

////////////////////////////////////////////////////////////////////////////////
// SXSkinComponents: Skinnable Visual Controls for Delphi and C++Builder      //
//----------------------------------------------------------------------------//
// Version: 1.2.1                                                             //
// Author: Alexey Sadovnikov                                                  //
// Web Site: http://www.saarixx.info/sxskincomponents/                        //
// E-Mail: sxskincomponents@saarixx.info                                      //
//----------------------------------------------------------------------------//
// LICENSE:                                                                   //
// 1. You may freely distribute this file.                                    //
// 2. You may not make any changes to this file.                              //
// 3. The only person who may change this file is Alexey Sadovnikov.          //
// 4. You may use this file in your freeware projects.                        //
// 5. If you want to use this file in your shareware or commercial project,   //
//    you should purchase a project license or a personal license of          //
//    SXSkinComponents: http://saarixx.info/sxskincomponents/en/purchase.htm  //
// 6. You may freely use, distribute and modify skins for SXSkinComponents.   //
// 7. You may create skins for SXSkinComponents.                              //
//----------------------------------------------------------------------------//
// Copyright (C) 2006-2007, Alexey Sadovnikov. All Rights Reserved.           //
////////////////////////////////////////////////////////////////////////////////

interface

{$I Compilers.inc}

uses SysUtils, Classes, Types, Windows;

type

  TCommonFileHeader=packed record
   VersionNeededToExtract:Word;
   GeneralPurposeBitFlag:Word;
   CompressionMethod:Word;
   LastModFileTimeDate:DWORD;
   Crc32:DWORD;
   CompressedSize:DWORD;
   UncompressedSize:DWORD;
   FilenameLength:Word;
   ExtraFieldLength:Word;
  end;

  TLocalFile=packed record
   LocalFileHeaderSignature:DWORD;
   CommonFileHeader:TCommonFileHeader;
   FileName:String;
   ExtraField:String;
   CompressedData:String;
  end;

  TFileHeader=packed record
   CentralFileHeaderSignature:DWORD;
   VersionMadeBy:Word;
   CommonFileHeader:TCommonFileHeader;
   FileCommentLength:Word;
   DiskNumberStart:Word;
   InternalFileAttributes:Word;
   ExternalFileAttributes:DWORD;
   RelativeOffsetOfLocalHeader:DWORD;
   FileName:String;
   ExtraField:String;
   FileComment:String;
  end;

  TEndOfCentralDir=packed record
   EndOfCentralDirSignature:DWORD;
   NumberOfThisDisk:Word;
   NumberOfTheDiskWithTheStart:Word;
   TotalNumberOfEntriesOnThisDisk:Word;
   TotalNumberOfEntries:Word;
   SizeOfTheCentralDirectory:DWORD;
   OffsetOfStartOfCentralDirectory:DWORD;
   ZipfileCommentLength:Word;
  end;

  TZipFile=class(TObject)
    Files:array of TLocalFile;
    CentralDirectory:array of TFileHeader;
    EndOfCentralDirectory:TEndOfCentralDir;
    ZipFileComment:String;
   private
    function GetUncompressed(I:Integer):String;
    procedure SetUncompressed(I:Integer;const Value:String);
    function GetDateTime(I:Integer):TDateTime;
    procedure SetDateTime(I:Integer;const Value:TDateTime);
    function GetCount:Integer;
    function GetName(I:Integer):String;
    procedure SetName(I:Integer;const Value:String);
   public
    procedure WriteFileToStream(Stream:TStream;FileName:String);
    property Count:Integer read GetCount;
    procedure AddFile(const Name:String;FAttribute:DWORD=0);
    procedure SaveToFile(const FileName:String);
    procedure SaveToStream(ZipFileStream:TStream);
    procedure LoadFromFile(const FileName:String);
    procedure LoadFromStream(const ZipFileStream:TStream);
    property Data[I:Integer]:String read GetUncompressed write SetUncompressed;
    property DateTime[I:Integer]:TDateTime read GetDateTime write SetDateTime;
    property Name[I:Integer]:String read GetName write SetName;
  end;

  TSXPreloadedZipFile=class
   public
    FullPath:String;
    FileName:String;
    ZipFile:TZipFile;
    SkinLibrary:Pointer;
    constructor Create;
    destructor Destroy; override;
  end;

  TSXPreloadedZipFileList=class
   protected
    FItem:TList;
    function Get(const Index:Integer):TSXPreloadedZipFile;
    procedure Put(const Index:Integer;const Item:TSXPreloadedZipFile);
    function GetCount:Integer;
   public
    procedure Add(ZipFile:TSXPreloadedZipFile);
    function GetZipFileIndex(const FileName:String):Integer;
    procedure Delete(const Index:Integer);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    property Item[const Index:Integer]:TSXPreloadedZipFile read Get write Put; default;
    property Count:Integer read GetCount;
  end;

  EZipFileCRCError=class(Exception);

function ZipCRC32(const Data:String):Cardinal;

function PreloadZipFile(Stream:TStream;const FilePath:String;
           SkinLibrary:Pointer):TZipFile;
function GetPreloadedZipFile(SkinLibrary:Pointer;const FilePath:String):TZipFile;
procedure DeletePreloadedZipFiles(SkinLibrary:Pointer);
function ExtractZipPath(var FilePath,ZipPath:String):Boolean;

var PreloadedZipFiles:TSXPreloadedZipFileList;

implementation

uses SXSkinUtils, SXSkinLibrary, SXZLibCompress;

{ TZipFile }

procedure TZipFile.SaveToFile(const FileName:String);
var ZipFileStream:TFileStream;
begin
 ZipFileStream:=TFileStream.Create(FileName,fmCreate);
 try
  SaveToStream(ZipFileStream);
 finally
  ZipFileStream.Free;
 end;
end;

procedure TZipFile.SaveToStream(ZipFileStream:TStream);
var I:Integer;
begin
 for I:=0 to High(Files) do
  with Files[i] do
   begin
    CentralDirectory[I].RelativeOffsetOfLocalHeader:=ZipFileStream.Position;
    ZipFileStream.Write(LocalFileHeaderSignature,4);
    if LocalFileHeaderSignature=$04034B50 then
     begin
      ZipFileStream.Write(CommonFileHeader,sizeof(CommonFileHeader));
      ZipFileStream.Write(PChar(FileName)^,CommonFileHeader.FilenameLength);
      ZipFileStream.Write(PChar(extrafield)^,CommonFileHeader.ExtraFieldLength);
      ZipFileStream.Write(PChar(CompressedData)^,CommonFileHeader.CompressedSize);
     end;
   end;
 EndOfCentralDirectory.OffsetOfStartOfCentralDirectory:=ZipFileStream.Position;
 for I:=0 to High(CentralDirectory) do
  with CentralDirectory[i] do
   begin
    ZipFileStream.Write(CentralFileHeaderSignature,4);
    ZipFileStream.Write(VersionMadeBy,2);
    ZipFileStream.Write(CommonFileHeader,sizeof(CommonFileHeader));
    ZipFileStream.Write(FileCommentLength,2);
    ZipFileStream.Write(DiskNumberStart,2);
    ZipFileStream.Write(InternalFileAttributes,2);
    ZipFileStream.Write(ExternalFileAttributes,4);
    ZipFileStream.Write(RelativeOffsetOfLocalHeader,4);
    ZipFileStream.Write(PChar(FileName)^,length(FileName));
    ZipFileStream.Write(PChar(ExtraField)^,length(ExtraField));
    ZipFileStream.Write(PChar(FileComment)^,length(FileComment));
   end;
 with EndOfCentralDirectory do
  begin
   EndOfCentralDirSignature:=$06054B50;
   NumberOfThisDisk:=0;
   NumberOfTheDiskWithTheStart:=0;
   TotalNumberOfEntriesOnThisDisk:=High(Files)+1;
   TotalNumberOfEntries:=High(Files)+1;
   SizeOfTheCentralDirectory:=ZipFileStream.Position-OffsetOfStartOfCentralDirectory;
   ZipfileCommentLength:=length(ZipFileComment);
  end;
 ZipFileStream.Write(EndOfCentralDirectory,sizeof(EndOfCentralDirectory));
 ZipFileStream.Write(PChar(ZipFileComment)^,length(ZipFileComment));
end;

procedure TZipFile.LoadFromStream(const ZipFileStream:TStream);
var Signature:DWORD;
            N:Integer;
begin
 N:=0;
 repeat
  Signature:=0;
  ZipFileStream.Read(Signature,4);
  if ZipFileStream.Position=ZipFileStream.Size then exit;
 until Signature=$04034B50;
 repeat
  if Signature=$04034B50 then
   begin
    Inc(N);
    SetLength(Files,N);
    SetLength(CentralDirectory,N);
    with Files[N-1] do
     begin
      LocalFileHeaderSignature:=Signature;
      ZipFileStream.Read(CommonFileHeader,sizeof(CommonFileHeader));
      SetLength(FileName,CommonFileHeader.FilenameLength);
      ZipFileStream.Read(PChar(FileName)^,CommonFileHeader.FilenameLength);
      FileName:=StringReplace(FileName,'\','/',[rfReplaceAll]);
      
      SetLength(ExtraField,CommonFileHeader.ExtraFieldLength);
      ZipFileStream.Read(PChar(ExtraField)^,CommonFileHeader.ExtraFieldLength);
      SetLength(CompressedData,CommonFileHeader.CompressedSize);
      ZipFileStream.Read(PChar(CompressedData)^,CommonFileHeader.CompressedSize);
     end;
   end;
  Signature:=0;
  ZipFileStream.Read(Signature,4);
 until Signature<>$04034B50;
 N:=0;
 repeat
  if Signature=$02014B50 then
   begin
    Inc(N);
    with CentralDirectory[N-1] do
     begin
      CentralFileHeaderSignature:=Signature;
      ZipFileStream.Read(VersionMadeBy,2);
      ZipFileStream.Read(CommonFileHeader,sizeof(CommonFileHeader));
      ZipFileStream.Read(FileCommentLength,2);
      ZipFileStream.Read(DiskNumberStart,2);
      ZipFileStream.Read(InternalFileAttributes,2);
      ZipFileStream.Read(ExternalFileAttributes,4);
      ZipFileStream.Read(RelativeOffsetOfLocalHeader,4);
      SetLength(FileName,CommonFileHeader.FilenameLength);
      ZipFileStream.Read(PChar(FileName)^,CommonFileHeader.FileNameLength);
      SetLength(ExtraField,CommonFileHeader.ExtraFieldLength);
      ZipFileStream.Read(PChar(ExtraField)^,CommonFileHeader.ExtraFieldLength);
      SetLength(FileComment,FileCommentLength);
      ZipFileStream.Read(PChar(FileComment)^,FileCommentLength);
      end;
   end;
  Signature:=0;
  ZipFileStream.Read(Signature,4);
 until Signature<>$02014B50;
 if Signature=$06054B50 then
  begin
   EndOfCentralDirectory.EndOfCentralDirSignature:=Signature;
   ZipFileStream.Read(EndOfCentralDirectory.NumberOfThisDisk,sizeof(EndOfCentralDirectory)-4);
   SetLength(ZipFileComment,EndOfCentralDirectory.ZipfileCommentLength);
   ZipFileStream.Read(PChar(ZipFileComment)^,EndOfCentralDirectory.ZipfileCommentLength);
  end;
end;

procedure TZipFile.LoadFromFile(const FileName:String);
var ZipFileStream:TFileStream;
begin
 ZipFileStream:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
 try
  LoadFromStream(ZipFileStream);
 finally
  ZipFileStream.Free;
 end;
end;

procedure TZipFile.WriteFileToStream(Stream:TStream;FileName:String);
var A,B:Integer;
      S:String;
begin
 while (FileName<>'') and ((FileName[1]='/') or (FileName[1]='\')) do Delete(FileName,1,1);
 FileName:=StringReplace(FileName,'\','/',[rfReplaceAll]);
 B:=-1;
 for A:=0 to High(Files) do
  if SameText(Files[A].FileName,FileName) then
   begin
    B:=A;
    break;
   end;
 if B<0 then exit;
 S:=GetUncompressed(B);
 Stream.Write(S[1],length(S));
end;

function TZipFile.GetUncompressed(I:Integer):String;
var    Decompressor:TDecompressionStream;
 UncompressedStream:TStringStream;
            AHeader:String;
          ReadBytes:Integer;
        LoadedCrc32:DWORD;
begin
 if (I<0) or (I>High(Files)) then
  raise Exception.Create('Index out of range.');
 AHeader:=Chr(120)+Chr(156);
 if Files[I].CommonFileHeader.CompressionMethod=0 then //not compressed
  begin
   Result:=Files[I].CompressedData;
  end else
   begin
    UncompressedStream:=TStringStream.Create(AHeader+Files[I].CompressedData);
    try
     Decompressor:=TDecompressionStream.Create(UncompressedStream);
     try
      SetLength(Result,Files[I].CommonFileHeader.UncompressedSize);
      ReadBytes:=Decompressor.Read(PChar(Result)^,Files[I].CommonFileHeader.UncompressedSize);
      if ReadBytes<>Integer(Files[I].CommonFileHeader.UncompressedSize) then Result:='';
     finally
      Decompressor.Free;
     end;
    finally
     UncompressedStream.Free;
    end;
    LoadedCRC32:=ZipCRC32(Result);
    if LoadedCRC32<>Files[I].CommonFileHeader.Crc32 then
     raise EZipFileCRCError.CreateFmt('CRC Error in "%s".',[Files[I].FileName]);
   end;  
end;

procedure TZipFile.SetUncompressed(I:Integer;const Value:String);
var    Compressor:TCompressionStream;
 CompressedStream:TStringStream;
  UseUncompressed:Boolean;
begin
 if I>High(Files) then
  raise Exception.Create('Index out of range.');
 CompressedStream:=TStringStream.Create('');
 try
  Compressor:=TCompressionStream.Create(clDefault{clMax},CompressedStream);
  try
   Compressor.Write(PChar(Value)^,length(Value));
  finally
   Compressor.Free;
  end;
  UseUncompressed:=length(Value)<=length(CompressedStream.DataString)-6;
  if UseUncompressed then
   Files[I].CompressedData:=Value else
    Files[I].CompressedData:=Copy(CompressedStream.DataString,3,length(CompressedStream.DataString)-6);
  Files[I].LocalFileHeaderSignature:=$04034B50;
  with Files[I].CommonFileHeader do
   begin
    VersionNeededToExtract:=20;
    GeneralPurposeBitFlag:=0;
    if UseUncompressed then
     CompressionMethod:=0 else
      CompressionMethod:=8;
    LastModFileTimeDate:=DateTimeToFileDate(Now);
    Crc32:=ZipCRC32(Value);
    CompressedSize:=length(Files[I].CompressedData);
    UncompressedSize:=length(Value);
    FilenameLength:=length(Files[I].FileName);
    ExtraFieldLength:=length(Files[I].ExtraField);
   end;
  with CentralDirectory[I] do
   begin
    CentralFileHeaderSignature:=$02014B50;
    VersionMadeBy:=20;
    CommonFileHeader:=Files[i].CommonFileHeader;
    FileCommentLength:=0;
    DiskNumberStart:=0;
    InternalFileAttributes := 0;
    RelativeOffsetOfLocalHeader := 0;
    FileName:=Files[i].FileName;
    ExtraField:=Files[i].ExtraField;
    FileComment:='';
   end;
 finally
  CompressedStream.Free;
 end;
end;

procedure TZipFile.AddFile(const Name:String;FAttribute:DWORD=0);
begin
 SetLength(Files,High(Files)+2);
 SetLength(CentralDirectory,length(Files));
 Files[High(Files)].FileName:=Name;
 Files[High(Files)].CompressedData:='';
 Files[High(Files)].ExtraField:='';
 Files[High(Files)].LocalFileHeaderSignature:=$04034B50;
 with Files[High(Files)].CommonFileHeader do
  begin
   VersionNeededToExtract:=20;
   GeneralPurposeBitFlag:=0;
   CompressionMethod:=8;
   LastModFileTimeDate:=DateTimeToFileDate(Now);
   Crc32:=0;
   CompressedSize:=0;
   UncompressedSize:=0;
   FilenameLength:=length(Files[High(Files)].FileName);
   ExtraFieldLength:=length(Files[High(Files)].ExtraField);
  end;
 with CentralDirectory[High(Files)] do
  begin
   CentralFileHeaderSignature:=$02014B50;
   VersionMadeBy:=20;
   CommonFileHeader:=Files[High(Files)].CommonFileHeader;
   FileCommentLength:=0;
   DiskNumberStart:=0;
   InternalFileAttributes:=0;
   ExternalFileAttributes:=FAttribute;
   RelativeOffsetOfLocalHeader:=0;
   FileName:=Files[High(Files)].FileName;
   ExtraField:=Files[High(Files)].ExtraField;
   FileComment:='';
  end;
end;

function TZipFile.GetDateTime(I:Integer):TDateTime;
begin
 if I>High(Files) then
  raise Exception.Create('Index out of range.');
 Result:=FileDateToDateTime(Files[i].CommonFileHeader.LastModFileTimeDate);
end;

procedure TZipFile.SetDateTime(I:Integer;const Value:TDateTime);
begin
 if I>High(Files) then
  raise Exception.Create('Index out of range.');
 Files[i].CommonFileHeader.LastModFileTimeDate:=DateTimeToFileDate(Value);
end;

function TZipFile.GetCount:Integer;
begin
 Result:=High(Files)+1;
end;

function TZipFile.GetName(I:Integer):String;
begin
 Result:=Files[I].FileName;
end;

procedure TZipFile.SetName(I:Integer;const Value:String);
begin
 Files[I].FileName:=Value;
end;

{ ZipCRC32 }

function ZipCRC32(const Data:String):Cardinal;
const CRCTable:array[0..255]of DWORD=(
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535,
    $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD,
    $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D,
    $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4,
    $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C,
    $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC,
    $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB,
    $B6662D3D, $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB,
    $086D3D2D, $91646C97, $E6635C01, $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA,
    $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE,
    $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A,
    $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409,
    $CE61E49F, $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81,
    $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739,
    $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268,
    $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7, $FED41B76, $89D32BE0,
    $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8,
    $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF,
    $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703,
    $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7,
    $B5D0CF31, $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE,
    $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6,
    $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D,
    $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5,
    $47B2CF7F, $30B5FFE9, $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605,
    $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);
var I:Integer;
begin
 Result:=$FFFFFFFF;
 for I:=0 to length(Data)-1 do
  Result:=(Result shr 8) xor (CRCTable[Byte(Result) xor Ord(Data[I+1])]);
 Result:=Result xor $FFFFFFFF;
end;

{ TSXPreloadedZipFile }

constructor TSXPreloadedZipFile.Create;
begin
 inherited;
end;

destructor TSXPreloadedZipFile.Destroy;
begin
 ZipFile.Free;
 inherited;
end;

{ TSXPreloadedZipFileList }

function TSXPreloadedZipFileList.Get(const Index:Integer):TSXPreloadedZipFile;
begin
 Result:=TSXPreloadedZipFile(FItem[Index]);
end;

procedure TSXPreloadedZipFileList.Put(const Index:Integer;const Item:TSXPreloadedZipFile);
begin
 FItem[Index]:=Item;
end;

function TSXPreloadedZipFileList.GetCount:Integer;
begin
 Result:=FItem.Count;
end;

procedure TSXPreloadedZipFileList.Add(ZipFile:TSXPreloadedZipFile);
begin
 FItem.Add(ZipFile);
end;

function TSXPreloadedZipFileList.GetZipFileIndex(const FileName:String):Integer;
var A:Integer;
begin
 for A:=0 to Count-1 do
  if SameText(Item[A].FileName,FileName) then
   begin
    Result:=A;
    exit;
   end;
 Result:=-1;
end;

procedure TSXPreloadedZipFileList.Delete(const Index:Integer);
begin
 Item[Index].Free;
 FItem.Delete(Index);
end;

procedure TSXPreloadedZipFileList.Clear;
var A:Integer;
begin
 for A:=0 to Count-1 do
  Item[A].Free;
 FItem.Clear;
end;

constructor TSXPreloadedZipFileList.Create;
begin
 inherited Create;
 FItem:=TList.Create;
end;

destructor TSXPreloadedZipFileList.Destroy;
begin
 Clear;
 FItem.Free;
 inherited Destroy;
end;

///////////////////////////////////////////////////////////////////////////////

function PreloadZipFile(Stream:TStream;const FilePath:String;
           SkinLibrary:Pointer):TZipFile;
var S:TStream;
    T:TSXPreloadedZipFile;
    A:Integer;
begin
 Result:=nil;
 if Stream=nil then
  begin
   if not FileExists(FilePath) then exit;
   S:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
  end else S:=Stream;
 try
  T:=TSXPreloadedZipFile.Create;
  T.FileName:=ExtractFileName(FilePath);
  T.FullPath:=FilePath;
  T.ZipFile:=TZipFile.Create;
  T.ZipFile.LoadFromStream(S);
  Result:=T.ZipFile;
  T.SkinLibrary:=SkinLibrary;
  A:=PreloadedZipFiles.GetZipFileIndex(T.FileName);
  if A>=0 then
   begin
    PreloadedZipFiles[A].Free;
    PreloadedZipFiles[A]:=T;
   end else PreloadedZipFiles.Add(T);
 finally
  if Stream=nil then S.Free;
 end;
end;

function GetPreloadedZipFile(SkinLibrary:Pointer;const FilePath:String):TZipFile;
var FileName,S:String;
             A:Integer;
          Skin:TSXStoredSkin;
begin
 FileName:=ExtractFileName(FilePath);
 A:=PreloadedZipFiles.GetZipFileIndex(FileName);
 if A>=0 then
  begin
   Result:=PreloadedZipFiles[A].ZipFile;
   exit;
  end;
 Skin:=GetStoredSkinByZIPName(FileName);
 if Skin<>nil then
  begin
   Result:=PreloadZipFile(Skin.Stream,FilePath,SkinLibrary);
   exit;
  end;
 if FileExists(FilePath) then
  Result:=PreloadZipFile(nil,FilePath,SkinLibrary) else
   begin
    S:=GetFullPath(FilePath,WithLastSlash(TSXSkinLibrary(SkinLibrary).SkinDir));
    if FileExists(S) then
     Result:=PreloadZipFile(nil,S,SkinLibrary) else
      Result:=nil;
   end;
end;       

procedure DeletePreloadedZipFiles(SkinLibrary:Pointer);
var A:Integer;
begin
 for A:=PreloadedZipFiles.Count-1 downto 0 do
  if PreloadedZipFiles[A].SkinLibrary=SkinLibrary then
   PreloadedZipFiles.Delete(A);
end;

function ExtractZipPath(var FilePath,ZipPath:String):Boolean;
var A,B:Integer;
begin
 B:=MaxInt;
 for A:=1 to length(FilePath) do
  begin
   if FilePath[A]=':' then
    begin
     Result:=False;
     exit;
    end;
   if (FilePath[A]='/') or (FilePath[A]='\') then
    begin
     B:=A-1;
     break;
    end;
  end;
 if SameText(ExtractFileExt(Copy(FilePath,1,B)),'.zip') then
  begin
   ZipPath:=Copy(FilePath,1,B);
   FilePath:=Copy(FilePath,B+2,MaxInt);
   Result:=True;
   exit;
  end;
 Result:=False; 
end;

initialization

 PreloadedZipFiles:=TSXPreloadedZipFileList.Create;

finalization

 PreloadedZipFiles.Free;

end.

