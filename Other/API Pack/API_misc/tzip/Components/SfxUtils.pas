//------------------------------------------------------------------------------
// Module            SfxUtils                                                  .
// Version:          1.4                                                       .
// Date:             6 April 2003                                              .
// Compilers:        Delphi 3 - Delphi 7, C++ Builder 3 - C++ Builder 5        .
// Author:           Angus Johnson - angusj-AT-myrealbox-DOT-com               .
// Copyright:        © 2001-2003 Angus Johnson                                 .
//                                                                             .
// Description:      Support functions to assist using SFX stubs with Zip      .
//                   files.                                                    .
// -----------------------------------------------------------------------------

//Updated 5 Sept 2001:
//  ReplaceExeIcon() improved:
//    1. Setting the IconNum to 0, will now replace the first icon found in the exe.
//    2. The icon source file can now be either an ico or exe (PE format) file.

unit SfxUtils;

{$IFDEF VER120} //delphi 4
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER125} //bcb 4
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER130} //delphi 5
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER135} //bcb 5
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER140} //delphi 6
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER150} //delphi 7
  {$DEFINE VER120_PLUS}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Zip, ZipDlls;

function JoinFiles(FileList: TStrings; const TargetFile: string): boolean;
function StripSfxStub(const SfxExeFile, NewZipFile: string): boolean;
procedure ReplaceExeIcon(const ExeFile, SaveToFile, SrcFile: string; IconNum: cardinal);

implementation

//Concatenate files in the order supplied by FileList...
function JoinFiles(FileList: TStrings; const TargetFile: string): boolean;
var
  i: integer;
  src, tgt: TFileStream;
begin
  result := false;
  if (FileList = nil) or (FileList.Count < 2) or (TargetFile = '') then exit;
  tgt := TFileStream.create(TargetFile, fmCreate);
  try
    for i := 0 to FileList.Count-1 do
    begin
      src := TFileStream.create(FileList[i], fmOpenRead);
      try
        if tgt.copyfrom(src,0) < src.Size then exit; //error
      finally
        src.free;
      end;
    end;
  finally
    tgt.free;
  end;
  result := true;
end;
//--------------------------------------------------------------------------

function StripSfxStub(const SfxExeFile, NewZipFile: string): boolean;
var
  i, BytesRead, TrueSfxSize, EocOffset: integer;
  SrcStream,TrgtStream: TFileStream;
  Eoc: TEndOfCentralHeader;
  cfh: TCentralFileHeader;
  SavedCursor: TCursor;
begin
  result := false;
  if (not FileExists(SfxExeFile)) or (NewZipFile = '') then exit;

  SrcStream := nil;
  TrgtStream := nil;
  SavedCursor := screen.cursor;
  screen.cursor := crHourglass;
  try
    SrcStream := TFileStream.create(SfxExeFile,fmOpenRead or fmShareDenyWrite);
    TrgtStream := TFileStream.create(NewZipFile, fmCreate); //nb - no check for overwrite

    EocOffset := FindEOCHeaderOffset(SrcStream);
    if (EocOffset = ERROR_VALUE) then
      raise ZipException.Createfmt(s_not_an_sfx_file, [SfxExeFile]);
    if SrcStream.Read(Eoc, sizeof(TEndOfCentralHeader)) <> sizeof(TEndOfCentralHeader) then
      raise ZipException.Create(s_sfx_read_error);

    if (Eoc.ThisDiskNo > 0) then exit; //Multi disk archives cannot be SfxExes!

    //get the relative offset of the first file in the archive ...
    SrcStream.seek(EocOffset - Eoc.CentralSize, soFromBeginning);
    SrcStream.read(cfh,sizeof(TCentralFileHeader));
    if cfh.HeaderSig <> CENTRAL_HEADERSIG then
      raise ZipException.Create(s_sfx_read_error);

    {TODO - nb: the central directory order does not have to reflect local order}  
    TrueSfxSize := cfh.RelOffLocalHdr +
      (EocOffset - (Eoc.CentralOffset + Eoc.CentralSize));
    if TrueSfxSize < 1 then exit; //not an Exe.

    //skip over the Sfx stub...
    SrcStream.Seek(TrueSfxSize,soFromBeginning);
    //copy the rest of the file...
    BytesRead := TrgtStream.copyfrom(SrcStream, SrcStream.size-TrueSfxSize);
    if BytesRead <> SrcStream.size-TrueSfxSize then exit; //error

    //now fix up the central directory offsets if necessary...
    if cfh.RelOffLocalHdr <> 0 then
    begin
      Eoc.CentralOffset := Eoc.CentralOffset - TrueSfxSize;
      TrgtStream.Seek(EocOffset-TrueSfxSize,soFromBeginning);
      TrgtStream.Write(Eoc,sizeof(TEndOfCentralHeader));
      TrgtStream.Seek(Eoc.CentralOffset,soFromBeginning);
      for i := 0 to Eoc.TotalEntries-1 do
      begin
        TrgtStream.read(cfh,sizeof(TCentralFileHeader));
        if cfh.HeaderSig <> CENTRAL_HEADERSIG then exit; //error
        cfh.RelOffLocalHdr := cfh.RelOffLocalHdr - TrueSfxSize;
        TrgtStream.Seek(-sizeof(TCentralFileHeader),soFromCurrent);
        TrgtStream.write(cfh,sizeof(TCentralFileHeader));
        //go to next central file header...
        TrgtStream.Seek(cfh.FileNameLength+
          cfh.ExtraFieldLength+cfh.FileCommentLen,soFromCurrent);
      end;
    end;
    result := true; //all ok if we get this far.
  finally
    SrcStream.free;
    TrgtStream.free;
    screen.cursor := SavedCursor;
  end;
end;
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
// Everything from here down is just to replace an icon image in an exe.   .
//--------------------------------------------------------------------------

const
  RES_TYPE_ICON = 3;

resourcestring
  s_no_res_section = 'No resource section exists in'#10'%s';
  s_no_matching_icon = 'No matching icon exists in'#10'%s';
  s_copy_error = 'Error while copying the icon.';
  s_res_not_at_start = 'Resources not at start of Section!';

type

{$IFNDEF VER120_PLUS}
  PImageDosHeader = ^TImageDosHeader;
  TImageDosHeader = packed record
    e_magic         : WORD;
    e_ignore        : packed array [0..28] of WORD;
    _lfanew        : Longint;
  end;
{$ENDIF}

  IMAGE_RESOURCE_DIRECTORY = packed record
    Characteristics : DWORD;
    TimeDateStamp   : DWORD;
    MajorVersion    : WORD;
    MinorVersion    : WORD;
    NumberOfNamedEntries : WORD;
    NumberOfIdEntries : WORD;
  end;

  IMAGE_RESOURCE_DIRECTORY_ENTRY = packed record
    Name: DWORD;        // Or ID: Word (Union)
    OffsetToData: DWORD;
  end;

  IMAGE_RESOURCE_DATA_ENTRY = packed record
    OffsetToData    : DWORD;
    Size            : DWORD;
    CodePage        : DWORD;
    Reserved        : DWORD;
  end;

//------------------------------------------------------------------------------
//  EXE RESOURCE TREE LAYOUT:                                                  .
//                                                                             .
//       DIR               DIRECTORY OF ALL RESOURCE TYPES                 (1) .
//        |                                                                    .
//   -----------                                                               .
//   |    |    |                                                               .
//  ENT  ENT  ENT                                                          (2) .
//   .    .    |                                                               .
//   .        DIR          DIRECTORY OF NAMES FOR A SPECIFIC TYPE          (3) .
//   .         |                                                               .
//        -----------                                                          .
//        |    |    |                                                          .
//       ENT  ENT  ENT                                                     (4) .
//        |    .    .                                                          .
//       DIR        .      DIRECTORY OF LANGUAGES FOR A SPECIFIC NAME      (5) .
//        |                                                                    .
//      ------                                                                 .
//      |    |                                                                 .
//     ENT  ENT                                                            (6) .
//      |    .                                                                 .
//    DATA                                                                 (7) .
//------------------------------------------------------------------------------

  TRes= class;      //forward declaration

  TResTree = class  //container for TRes tree structure
  private
    fStream: TStream;
    fSectionOffset: cardinal;
    fTreeRoot: TRes;
    fDataList: TList;
    fCurrentName: integer;
    fCurrentLang: integer;
  public
    constructor Create(stream: TStream);
    destructor Destroy; override;
  end;

  TRes = class      //base class for TResDir, TResEnt & TResData
  private
    fTree: TResTree;
    fChilds: TList;
    function Stream: TStream;
    constructor Create(aTree: TResTree; Level: integer); virtual;
    destructor Destroy; override;
  end;

  TResDir = class(TRes)
    Dir: IMAGE_RESOURCE_DIRECTORY;
    constructor Create(aTree: TResTree; Level: integer); override;
  end;

  TResEnt = class(TRes)
    Ent: IMAGE_RESOURCE_DIRECTORY_ENTRY;
    constructor Create(aTree: TResTree; Level: integer); override;
  end;

  TResData = class(TRes)
    Data: IMAGE_RESOURCE_DATA_ENTRY;
    ResName: cardinal;  //nb: RT_ICON names are ALWAYS ordinal values
    ResLang: cardinal;
    constructor Create(aTree: TResTree; Level: integer); override;
  end;

//--------------------------------------------------------------------------
// TResTree class
//--------------------------------------------------------------------------

constructor TResTree.Create(stream: TStream);
begin
  fStream := stream;
  fSectionOffset := stream.Position;
  fDataList := TList.Create;
  fTreeRoot := TResDir.Create(self, 1); //recursively fills fDataList
end;
//--------------------------------------------------------------------------

destructor TResTree.Destroy;
begin
  fDataList.free;
  fTreeRoot.free;
  inherited;
end;

//--------------------------------------------------------------------------
// TRes class
//--------------------------------------------------------------------------

constructor TRes.Create(aTree: TResTree; Level: integer);
begin
  fChilds := TList.Create;
  fTree := aTree;
end;
//--------------------------------------------------------------------------

destructor TRes.Destroy;
var
  i: integer;
begin
  for i := 0 to fChilds.count-1 do TRes(fChilds[i]).free;
  fChilds.free;
  inherited;
end;
//--------------------------------------------------------------------------

function TRes.Stream: TStream;
begin
  result := fTree.fStream;
end;

//--------------------------------------------------------------------------
// TResDir class
//--------------------------------------------------------------------------

constructor TResDir.Create(aTree: TResTree; Level: integer);
var
  i: integer;
  Child: TRes;
begin
  inherited;
  stream.Read(Dir, sizeof(IMAGE_RESOURCE_DIRECTORY));
  with Dir do
    for i := 1 to NumberOfNamedEntries+NumberOfIdEntries do
    begin
      Child := TResEnt.Create(aTree,Level+1);
      if (Child.fChilds.Count > 0) then fChilds.Add(Child)
      else Child.free;
    end;
end;

//--------------------------------------------------------------------------
// TResEnt class
//--------------------------------------------------------------------------

constructor TResEnt.Create(aTree: TResTree; Level: integer);
var
  Child: TRes;
  SavedPos: {$IFDEF VER120_PLUS} int64{$ELSE} integer{$ENDIF};

  //-------------------------------------------
  function HighBitSet(Val: dword): boolean;
  begin
    result := (Val and $80000000) = $80000000;
  end;
  //-------------------------------------------

  function StripHighBit(Val: dword): dword;
  begin
    result := Val and not $80000000;
  end;
  //-------------------------------------------

begin
  inherited;
  stream.Read(Ent, sizeof(IMAGE_RESOURCE_DIRECTORY_ENTRY));
  if (Level = 2) and (Ent.name <> RES_TYPE_ICON) then
    exit //no point creating childs when not part of the ICON resource tree
  else if (Level = 4) then
    aTree.fCurrentName := Ent.name
  else if (Level = 6) then
    aTree.fCurrentLang := Ent.name;

  SavedPos := stream.Position;
  with Ent do
    if HighBitSet(OffsetToData) then //child is a Directory
    begin
      stream.Seek(aTree.fSectionOffset + StripHighBit(OffsetToData),
        soFromBeginning);
      Child := TResDir.Create(aTree, Level+1);
      fChilds.Add(Child);
    end else                         //child is Data
    begin
      stream.Seek(aTree.fSectionOffset + OffsetToData, soFromBeginning);
      Child := TResData.Create(aTree, Level+1);
      fChilds.Add(Child);
    end;
  stream.Position := SavedPos;       //position for next entry
end;

//--------------------------------------------------------------------------
// TResData class
//--------------------------------------------------------------------------

constructor TResData.Create(aTree: TResTree; Level: integer);
begin
  inherited;
  stream.Read(Data, sizeof(IMAGE_RESOURCE_DATA_ENTRY));
  aTree.fDataList.Add(self);
  ResName := aTree.fCurrentName;
  ResLang := aTree.fCurrentLang;
end;
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------


const
  IMAGE_DOS_SIGNATURE = $5A4D;     //'MZ'
  IMAGE_PE_SIGNATURE  = $00004550; //'PE'#0#0

//positions stream at the beginning of the Resource SectionHeader if found...
function FindResSectionHeader(stream: TStream): boolean;
var
  i: integer;
  idh: TImageDosHeader;
  pe_sig: dword;
  fh: TImageFileHeader;
  oh: TImageOptionalHeader;
  sh: TImageSectionHeader;
  res_virt_addr: cardinal;
begin
  result := false;
  if (stream = nil) or
     (stream.Read(idh,sizeof(TImageDosHeader)) <> sizeof(TImageDosHeader)) or
     (idh.e_magic <> IMAGE_DOS_SIGNATURE) then exit;
  stream.Seek(idh._lfanew,soFromBeginning);
  if (stream.Read(pe_sig,sizeof(dword)) <> sizeof(dword)) or
    (pe_sig <> IMAGE_PE_SIGNATURE) or
    (stream.Read(fh,sizeof(TImageFileHeader)) <> sizeof(TImageFileHeader)) or
    (stream.Read(oh,sizeof(TImageOptionalHeader)) <>
      sizeof(TImageOptionalHeader)) then exit;
  with oh.DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE] do
    if (VirtualAddress = 0) or (Size = 0) then exit;
  res_virt_addr := oh.DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress;
  for i := 0 to fh.NumberOfSections do
  begin
    if stream.Read(sh,sizeof(TImageSectionHeader)) <>
      sizeof(TImageSectionHeader) then exit;
    if (sh.VirtualAddress <= res_virt_addr) and
      (res_virt_addr < sh.VirtualAddress + sh.SizeOfRawData) then
    begin
      // assume resources start at beginning of a section
      // which is the case for all modern compilers
      if (res_virt_addr <> sh.VirtualAddress) then
        raise Exception.create(s_res_not_at_start);
      stream.Seek(-sizeof(TImageSectionHeader),soFromCurrent);
      result := true;
      break;
    end;
  end;
end;
//--------------------------------------------------------------------------

type

  TIconDir = packed record
    idReserved: word;      //must be 0
    idType: word;          //1 for icons, 2 for cursors
    idCount: word;         //images in Ico file
  end;

  TIconDirEntry = packed record
    bWidth         : byte;
    bHeight        : byte;
    bColorCount    : byte;
    bReserved      : byte;
    wPlanes        : word;
    wBitCount      : word;
    dwBytesInRes   : dword;
    dwImageOffset  : dword;
  end;

//--------------------------------------------------------------------------

function FindIcoImage(stream: TStream; ImageSize: cardinal): boolean;
var
  IconDir: TIconDir;
  IconDirEntry: TIconDirEntry;
  i: integer;
begin
  result := false;
  //assumes already at start of stream...
  stream.Read(IconDir,sizeof(IconDir));
  if (IconDir.idReserved <> 0) and (IconDir.idType <> 1) then exit;
  for i := 1 to IconDir.idCount do
  begin
    stream.Read(IconDirEntry, sizeof(TIconDirEntry));
    if (IconDirEntry.dwBytesInRes = ImageSize) then
    begin
      stream.Seek(IconDirEntry.dwImageOffset,soFromBeginning);
      result := true;
      break;
    end;
  end;
end;
//--------------------------------------------------------------------------

procedure ReplaceExeIcon(const ExeFile, SaveToFile, SrcFile: string; IconNum: cardinal);
var
  exe: TMemoryStream;
  src: TFileStream;
  sh: TImageSectionHeader;
  resTree,resTree2: TResTree;
  resData,resData2: TResData;

  //-----------------------------------------------------------
  function FindIconNum(Tree: TResTree; Num: cardinal): TResData;
  var
    i: integer;
  begin
    result := nil;
    for i := 0 to Tree.fDataList.Count - 1 do
      with TResData(Tree.fDataList[i]) do
        if ((Num = 0) or (ResName = Num)) and
          (Data.Size <> 0) and (Data.OffsetToData <> 0) then
        begin
          result := TResData(Tree.fDataList[i]);
          break;
        end;
  end;
  //-----------------------------------------------------------

  function FindIconSize(Tree: TResTree; Size: cardinal): TResData;
  var
    i: integer;
  begin
    result := nil;
    for i := 0 to Tree.fDataList.Count - 1 do
      with TResData(Tree.fDataList[i]) do
        if (Data.Size = Size) and (Data.OffsetToData <> 0) then
        begin
          result := TResData(Tree.fDataList[i]);
          break;
        end;
  end;
  //-----------------------------------------------------------

begin
  exe := nil;
  src := nil;
  try
    exe := TMemoryStream.Create;
    exe.LoadFromFile(ExeFile);
    src := TFileStream.create(SrcFile, fmOpenRead or fmShareDenyNone);

    if not FindResSectionHeader(exe) then
      raise Exception.createfmt(s_no_res_section,[ExeFile]);
    if exe.Read(sh,sizeof(TImageSectionHeader)) <> sizeof(TImageSectionHeader) then
      Exception.createfmt(s_no_res_section,[ExeFile]);
    exe.Seek(sh.PointerToRawData,soFromBeginning);

    resTree := TResTree.Create(exe);
    try
      //fortunately, RT_ICON 'names' can only be ordinal values...
      resData := FindIconNum(resTree,IconNum);
      if not assigned(resData) then
        raise Exception.createfmt(s_no_matching_icon,[ExeFile]);
      exe.Seek(sh.PointerToRawData +
        resData.Data.OffsetToData - sh.VirtualAddress, soFromBeginning);

      if (lowercase(ExtractFileExt(SrcFile)) = '.ico') then
      begin
        //Find the matching sized icon in the ico file ...
        if not FindIcoImage(src, resData.Data.Size) then
          raise Exception.createfmt(s_no_matching_icon,[SrcFile]);
      end else
      begin
        //assume an exe file which contains a matching sized icon...
        if not FindResSectionHeader(src) then
          raise Exception.createfmt(s_no_res_section,[SrcFile]);
        if src.Read(sh,sizeof(TImageSectionHeader)) <>
          sizeof(TImageSectionHeader) then
            raise Exception.createfmt(s_no_res_section,[SrcFile]);
        src.Seek(sh.PointerToRawData,soFromBeginning);
        resTree2 := TResTree.Create(src);
        try
          resData2 := FindIconSize(resTree2,resData.Data.Size);
          if not assigned(resData2) then
            raise Exception.createfmt(s_no_matching_icon,[SrcFile]);
          src.Seek(sh.PointerToRawData +
            resData2.Data.OffsetToData - sh.VirtualAddress, soFromBeginning);
        finally
          resTree2.free;
        end;
      end;

      //copy and save the modified file...
      if exe.CopyFrom(src,resData.Data.Size) <> resData.Data.Size then
        raise Exception.create(s_copy_error);
      exe.SaveToFile(SaveToFile);
    finally
      resTree.Free;
    end;
  finally
    exe.free;
    src.free;
  end;
end;
//--------------------------------------------------------------------------

end.
