{

  Audio Tools Library                                                         
  Class TID3v2 - for manipulating with ID3v2 tags                             
                                                                            
  http://mac.sourceforge.net/atl/                                             
  e-mail: macteam@users.sourceforge.net
  
  Copyright (c) 2000-2002 by Jurgen Faul                                      
  Copyright (c) 2003-2005 by The MAC Team
  Copyright (c) 2006-2007 by Vitaly Kravchenko

  Version 2.1 (28 May 2007) by Vitaly Kravchenko
    - made ReadFromTagStream a non-object method (renamed to ReadID3v2TagFromStream)
                                                                            
  Version 2.0 (6 April 2007) by Vitaly Kravchenko                             
    - Support for saving the track number in the 'xx/xx' format               
                                                                            
  Version 1.9 (August 2006) by Vitaly Kravchenko                              
    - Unicode fields support                                                  
    - minor code changes and improvements                                     
                                                                            
  Version 1.8 (April 2005) by Gambit                                          
    - updated to unicode file access                                          
                                                                            
  Version 1.7 (2 October 2002)                                                
    - Added property TrackString                                              
                                                                            
  Version 1.6 (29 July 2002)                                                  
    - Reading support for Unicode                                             
    - Removed limitation for the track number                                 
                                                                            
  Version 1.5 (23 May 2002)                                                   
    - Support for padding                                                     
                                                                            
  Version 1.4 (24 March 2002)                                                 
    - Reading support for ID3v2.2.x & ID3v2.4.x tags                          
                                                                            
  Version 1.3 (16 February 2002)
    - Fixed bug with property Comment
    - Added info: composer, encoder, copyright, language, link                

  Version 1.2 (17 October 2001)                                               
    - Writing support for ID3v2.3.x tags                                      
    - Fixed bug with track number detection
    - Fixed bug with tag reading                                              
                                                                            
  Version 1.1 (31 August 2001)                                                
    - Added public procedure ResetData                                        
                                                                            
  Version 1.0 (14 August 2001)                                                
    - Reading support for ID3v2.3.x tags                                      
    - Tag info: title, artist, album, track, year, genre, comment             
                                                                            
  This library is free software; you can redistribute it and/or               
  modify it under the terms of the GNU Lesser General Public                  
  License as published by the Free Software Foundation; either                
  version 2.1 of the License, or (at your option) any later version.          
                                                                            
  This library is distributed in the hope that it will be useful,             
  but WITHOUT ANY WARRANTY; without even the implied warranty of              
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           
  Lesser General Public License for more details.                             
                                                                            
  You should have received a copy of the GNU Lesser General Public            
  License along with this library; if not, write to the Free Software         
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}   

// -- Modified for TBASSPlayer  by Silhwan Hyun --
// Replaced the refernced unit TntClasses, TntSysUtils of "Tnt Delphi UNICODE Controls"
//  package with UniCodeUtils and TntCollection.
// Added a property OrigArtist for TBASSPlayer.

// Modified for Delphi 2009   (21 Apr 2009)

unit ID3v2;

interface

{$INCLUDE Delphi_Ver.inc}

uses
  Windows, Classes, SysUtils, UniCodeUtils, TntCollection,
  {$IFNDEF DELPHI_2007_BELOW}AnsiStringStream,{$ENDIF} ID3v1;

const
  TAG_VERSION_2_2 = 2;                               { Code for ID3v2.2.x tag }
  TAG_VERSION_2_3 = 3;                               { Code for ID3v2.3.x tag }
  TAG_VERSION_2_4 = 4;                               { Code for ID3v2.4.x tag }

type
  { Class TID3v2 }
  TID3v2 = class(TObject)
    private
      { Private declarations }
      FExists: Boolean;
      FVersionID: Byte;
      FSize: Integer;
      FTitle: WideString;
      FArtist: WideString;
      FAlbum: WideString;
      FTrack: Word;
      FTrackString: WideString;
      FYear: WideString;
      FGenre: WideString;
      FComment: WideString;
      FComposer: WideString;
      FEncoder: WideString;
      FCopyright: WideString;
      FLanguage: WideString;
      FLink: WideString;
      FOrigArtist: WideString;
      FLyrics: WideString;
      FTSIZ: WideString;
      procedure FSetTitle(const NewTitle: WideString);
      procedure FSetArtist(const NewArtist: WideString);
      procedure FSetAlbum(const NewAlbum: WideString);
      procedure FSetTrack(const NewTrack: Word);
      procedure FSetYear(const NewYear: WideString);
      procedure FSetGenre(const NewGenre: WideString);
      procedure FSetComment(const NewComment: WideString);
      procedure FSetComposer(const NewComposer: WideString);
      procedure FSetEncoder(const NewEncoder: WideString);
      procedure FSetCopyright(const NewCopyright: WideString);
      procedure FSetLanguage(const NewLanguage: WideString);
      procedure FSetLink(const NewLink: WideString);
      procedure FSetTrackString(const NewTrack: WideString);
      procedure FSetLyrics(const NewLyrics: WideString);
      procedure FSetOrigArtist(const NewOrigArtist: WideString);
    public
      { Public declarations }
      constructor Create;                                     { Create object }
      procedure ResetData;                                   { Reset all data }
      function ReadFromFile(const FileName: WideString): Boolean;  { Load tag }
      function SaveToFile(const FileName: WideString): Boolean;    { Save tag }
      function RemoveFromFile(const FileName: WideString): Boolean;{ Delete tag }
      property Exists: Boolean read FExists;              { True if tag found }
      property VersionID: Byte read FVersionID;                { Version code }
      property Size: Integer read FSize;                     { Total tag size }
      property Title: WideString read FTitle write FSetTitle;        { Song title }
      property Artist: WideString read FArtist write FSetArtist;    { Artist name }
      property Album: WideString read FAlbum write FSetAlbum;       { Album title }
      property Track: Word read FTrack write FSetTrack;             { Track number }
      property TrackString: WideString read FTrackString write FSetTrackString; { Track number (string) }
      property Year: WideString read FYear write FSetYear;         { Release year }
      property Genre: WideString read FGenre write FSetGenre;        { Genre name }
      property Comment: WideString read FComment write FSetComment;     { Comment }
      property Composer: WideString read FComposer write FSetComposer; { Composer }
      property Encoder: WideString read FEncoder write FSetEncoder;     { Encoder }
      property Copyright: WideString read FCopyright write FSetCopyright;   { (c) }
      property Language: WideString read FLanguage write FSetLanguage; { Language }
      property Link: WideString read FLink write FSetLink;             { URL link }
    // property OrigArtist is added for TBASSPlayer, by Silhwan Hyun
      property OrigArtist: WideString read FOrigArtist write FSetOrigArtist; { Original Artist name }
      property Lyrics: WideString read FLyrics write FSetLyrics;
      property TSIZ: WideString read FTSIZ;
  end;

// Load ID3v2 tag from stream: added for TBASSPlayer by Silhwan Hyun
function ReadID3v2TagFromStream(TagStream: PAnsiChar; StreamSize : Longword;
  var MP3TagInfo: TMP3TagInfo): Boolean;

implementation

const
  { ID3v2 tag ID }
  ID3V2_ID = 'ID3';

  { Max. number of supported tag frames }
  ID3V2_FRAME_COUNT = 18;

  { Names of supported tag frames (ID3v2.3.x & ID3v2.4.x) }
  ID3V2_FRAME_NEW: array [1..ID3V2_FRAME_COUNT] of ansistring =
    ('TIT2', 'TPE1', 'TALB', 'TRCK', 'TYER', 'TCON', 'COMM', 'TCOM', 'TENC',
     'TCOP', 'TLAN', 'WXXX', 'TDRC', 'TOPE', 'TIT1', 'TOAL', 'TSIZ', 'USLT');

  { Names of supported tag frames (ID3v2.2.x) }
  ID3V2_FRAME_OLD: array [1..ID3V2_FRAME_COUNT] of ansistring =
    ('TT2', 'TP1', 'TAL', 'TRK', 'TYE', 'TCO', 'COM', 'TCM', 'TEN',
     'TCR', 'TLA', 'WXX', 'TOR', 'TOA', 'TT1', 'TOT', 'TSI', 'ULT');

  { Max. tag size for saving }
  ID3V2_MAX_SIZE = 4096;

  { Unicode ID }
  UNICODE_ID = #1;

type
  { Frame header (ID3v2.3.x & ID3v2.4.x) }
  FrameHeaderNew = record
    ID: array [1..4] of AnsiChar;                     { Frame ID }
    Size: Integer;                                    { Size excluding header }
    Flags: Word;                                      { Flags }
  end;

  { Frame header (ID3v2.2.x) }
  FrameHeaderOld = record
    ID: array [1..3] of AnsiChar;                     { Frame ID }
    Size: array [1..3] of Byte;                       { Size excluding header }
  end;

  { ID3v2 header data - for internal use }
  TagInfo = record
    { Real structure of ID3v2 header }
    ID: array [1..3] of AnsiChar;                          { Always "ID3" }
    Version: Byte;                                         { Version number }
    Revision: Byte;                                        { Revision number }
    Flags: Byte;                                               { Flags of tag }
    Size: array [1..4] of Byte;                   { Tag size excluding header }
    { Extended data }
    FileSize: Integer;                                    { File size (bytes) }
    Frame: array [1..ID3V2_FRAME_COUNT] of ansistring;  { Information from frames }
    FrameEncode: array [1..ID3V2_FRAME_COUNT] of byte;  { encode byte }  // ** Added **
    NeedRewrite: Boolean;                           { Tag should be rewritten }
    PaddingSize: Integer;                              { Padding size (bytes) }
  end;

{ ********************* Auxiliary functions & procedures ******************** }

function ReadHeader(const FileName: WideString; var Tag: TagInfo): Boolean;
var
  SourceFile: TTntFileStream;
  Transferred: Integer;
begin
  try
    Result := true;
    { Set read-access and open file }
    SourceFile := TTntFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);

    { Read header and get file size }
    Transferred := SourceFile.Read(Tag, 10);
    Tag.FileSize := SourceFile.Size;
    SourceFile.Free;
    { if transfer is not complete }
    if Transferred < 10 then Result := false;
  except
    { Error }
    Result := false;
  end;
end;

{ --------------------------------------------------------------------------- }

function GetTagSize(const Tag: TagInfo): Integer;
begin
  { Get total tag size }
  Result :=
    Tag.Size[1] * $200000 +
    Tag.Size[2] * $4000 +
    Tag.Size[3] * $80 +
    Tag.Size[4] + 10;
  if Tag.Flags and $10 = $10 then Inc(Result, 10);
  if Result > Tag.FileSize then Result := 0;
end;

{ --------------------------------------------------------------------------- }

procedure SetTagItem(const ID, Data: ansistring; var Tag: TagInfo; EncodeByte : Byte);
var
  Iterator: Byte;
  FrameID: ansistring;
begin
  { Set tag item if supported frame found }
  for Iterator := 1 to ID3V2_FRAME_COUNT do
  begin
    if Tag.Version > TAG_VERSION_2_2 then
      FrameID := ID3V2_FRAME_NEW[Iterator]
    else
      FrameID := ID3V2_FRAME_OLD[Iterator];
    if (FrameID = ID) {and (Data[1] <= UNICODE_ID)} then
    begin
      Tag.Frame[Iterator] := Data;

      Tag.FrameEncode[Iterator] := EncodeByte;
      Break;     // ** Added to avoid redudant operation
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

function Swap32(const Figure: Integer): Integer;
var
  ByteArray: array [1..4] of Byte absolute Figure;
begin
  { Swap 4 bytes }
  Result :=
    ByteArray[1] * $1000000 +
    ByteArray[2] * $10000 +
    ByteArray[3] * $100 +
    ByteArray[4];
end;

{ --------------------------------------------------------------------------- }

procedure ReadFramesNew(const FileName: WideString; var Tag: TagInfo);
var
  SourceFile: TTntFileStream;
  Frame: FrameHeaderNew;
  Data: array [1..5000] of AnsiChar;
  Data2 : ansistring;
  i : integer;
  DataPosition, DataSize: Integer;
begin
  { Get information from frames (ID3v2.3.x & ID3v2.4.x) }
  try
    { Set read-access, open file }
    SourceFile := TTntFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    SourceFile.Seek(10, soFromBeginning);
    while (SourceFile.Position < GetTagSize(Tag)) and (SourceFile.Position < SourceFile.Size) do
    begin
      FillChar(Data, SizeOf(Data), 0);
      { Read frame header and check frame ID }
      SourceFile.Read(Frame, 10);
      if not (Frame.ID[1] in ['A'..'Z']) then
        Break;
      { Note data position and determine significant data size }
      DataPosition := SourceFile.Position;
      if Swap32(Frame.Size) > SizeOf(Data) then
        DataSize := SizeOf(Data)
      else
        DataSize := Swap32(Frame.Size);
      { Read frame data and set tag item if frame supported }
      SourceFile.Read(Data, DataSize);
      if Frame.Flags and $8000 <> $8000 then
      begin
        if Data[1] = #0 then
        begin   // for non-unicode
           Data2 := '';
           i := 2;
           repeat
              if Data[i] <> #0 then
                 Data2 := Data2 + ansichar(Data[i]);
              inc(i);
           until (Data[i] = #0) or (i = 5000);
           SetTagItem(Frame.ID, Data2, Tag, ord(Data[1]));
        end else
           SetTagItem(Frame.ID, Data, Tag, ord(Data[1]));
      end;
      SourceFile.Seek(DataPosition + Swap32(Frame.Size), soFromBeginning);
    end;
    SourceFile.Free;
  except
  end;
end;

{ --------------------------------------------------------------------------- }

procedure ReadFramesOld(const FileName: WideString; var Tag: TagInfo);
var
  SourceFile: TTntFileStream;
  Frame: FrameHeaderOld;
  Data: array [1..5000] of AnsiChar;
  DataPosition, FrameSize, DataSize: Integer;
begin
  { Get information from frames (ID3v2.2.x) }
  try
    { Set read-access, open file }
    SourceFile := TTntFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    SourceFile.Seek(10, soFromBeginning);
    while (SourceFile.Position < GetTagSize(Tag)) and (SourceFile.Position < SourceFile.Size) do
    begin
      FillChar(Data, SizeOf(Data), 0);
      { Read frame header and check frame ID }
      SourceFile.Read(Frame, 6);
      if not (Frame.ID[1] in ['A'..'Z']) then break;
      { Note data position and determine significant data size }
      DataPosition := SourceFile.Position;
      FrameSize := Frame.Size[1] shl 16 + Frame.Size[2] shl 8 + Frame.Size[3];
      if FrameSize > SizeOf(Data) then DataSize := SizeOf(Data)
      else DataSize := FrameSize;
      { Read frame data and set tag item if frame supported }
      SourceFile.Read(Data, DataSize);
      SetTagItem(Frame.ID, Data, Tag, ord(Data[1]));
      SourceFile.Seek(DataPosition + FrameSize, soFromBeginning);
    end;
    SourceFile.Free;
  except
  end;
end;

{ --------------------------------------------------------------------------- }

function GetUnicode(const Source: ansistring): WideString;
var
  Index: Integer;
  FirstByte, SecondByte: Byte;
  UnicodeChar: WideChar;
begin
  if (Length(Source) > 0) and (Source[1] = UNICODE_ID) then
  begin
    Result := '';
    for Index := 2 to ((Length(Source) - 1) div 2) do
    begin
      FirstByte := Ord(Source[Index * 2]);
      SecondByte := Ord(Source[Index * 2 + 1]);
      UnicodeChar := WideChar(FirstByte or (SecondByte shl 8));
      if UnicodeChar = #0 then break;
      Result := Result + UnicodeChar;
    end;
    Result := Trim(Result);
  end
  else
   {$IFDEF DELPHI_2007_BELOW}
    Result := Trim(UTF8Decode(Source));
   {$ELSE}
    Result := Trim(UTF8ToWideString(Source));
   {$ENDIF}
end;

function GetUnicode2(const Source: ansistring; EncodeByte : byte): WideString;
begin
   if EncodeByte = 0 then
   begin
   {$IFDEF DELPHI_2007_BELOW}
     Result := Trim(UTF8Decode(Source));
   {$ELSE}
     Result := Trim(UTF8ToWideString(Source));
   {$ENDIF}
   end else
     Result := GetUnicode(Source);

end;

{ --------------------------------------------------------------------------- }

function GetANSIUnicode(const WS: WideString): ansistring;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to length(WS) do
    Result := Result + AnsiChar(Lo(Word(WS[i]))) + AnsiChar(Hi(Word(WS[i])));
  if Length(result) > 0 then
    Result := #255#254 + result;
end;

{ --------------------------------------------------------------------------- }

function GetContent(const Content1, Content2: ansistring): WideString;
begin
  { Get content preferring the first content }
  Result := GetUnicode(Content1);
  if Result = '' then
    Result := GetUnicode(Content2);
end;

function GetContent2(const Content1, Content2: ansistring; EncodeByte1, EncodeByte2 : byte): WideString;
begin
   if (Content1 = '') and (Content2 = '') then
      result := ''
   else if (EncodeByte1 = 0) and (EncodeByte2 = 0) then
   begin
    //  result := Content1;         // Use automatic code conversion
      result := ToWideString(Content1);
      if result = '' then
       //  result := Content2;      // Use automatic code conversion
         result := ToWideString(Content2);
   end
   else if (EncodeByte1 = 0) and (Content2 = '') then
    //  result := Content1          // Use automatic code conversion
      result := ToWideString(Content1)
   else if (Content1 = '') and (EncodeByte2 = 0) then
    //  result := Content2          // Use automatic code conversion
      result := ToWideString(Content2)
   else
   begin
      result := GetContent(Content1, Content2);
   end;

end;

{ --------------------------------------------------------------------------- }

function ExtractTrack(const TrackString: ansistring): Word;
var
  Uni_Track : wideString;
  Index, Value, Code : integer;
begin
  { Extract track from string }
 // Result := ID3v1.ExtractTrack(GetUnicode(TrackString));

 // Track := GetANSI(TrackString);
  Uni_Track := GetUnicode(TrackString);
  Index := Pos('/', Uni_Track);
  if Index = 0 then Val(Uni_Track, Value, Code)
  else Val(Copy(Uni_Track, 1, Index - 1), Value, Code);
  if Code = 0 then Result := Value
  else Result := 0;
end;

{ --------------------------------------------------------------------------- }

function ExtractYear(const YearString, DateString: ansistring): WideString;
begin
  { Extract year from strings }
  Result := GetUnicode(YearString);
  if Result = '' then
    Result := Copy(GetUnicode(DateString), 1, 4);
end;

{ --------------------------------------------------------------------------- }

function ExtractGenre(const GenreString: ansistring): WideString;
begin
  { Extract genre from string }
  Result := GetUnicode(GenreString);
  if Pos(')', Result) > 0 then
    Delete(Result, 1, LastDelimiter(')', Result));
end;

{ --------------------------------------------------------------------------- }

function ExtractText(const SourceString: ansistring; LanguageID: Boolean; EncodeByte : byte): WideString;
var
  Source, Separator: ansistring;
  EncodingID: AnsiChar;
begin
  { Extract significant text data from a complex field }
  Source := SourceString;
  Result := '';
  if Length(Source) > 0 then
  begin
    if EncodeByte = 0 then
       EncodingID := #0
    else
       EncodingID := Source[1];

    if EncodingID = UNICODE_ID then
      Separator := #0#0
    else
      Separator := #0;

    if LanguageID then
      if EncodeByte = 0 then
         Delete(Source, 1, 3)
      else
         Delete(Source, 1, 4)
    else
      Delete(Source, 1, 1);
    
    Delete(Source, 1, Pos(Separator, Source) + Length(Separator) - 1);
    if EncodeByte = 0 then
     // Result := Source   // Use automatic code conversion
       result := ToWideString(Source)
    else
       Result := GetUnicode(EncodingID + Source);
  end;
end;

{ --------------------------------------------------------------------------- }

procedure BuildHeader(var Tag: TagInfo);
var
  Iterator, TagSize: Integer;
begin
  { Calculate new tag size (without padding) }
  TagSize := 10;
  for Iterator := 1 to ID3V2_FRAME_COUNT do
    if Tag.Frame[Iterator] <> '' then
      Inc(TagSize, Length(Tag.Frame[Iterator]) + 11);
  { Check for ability to change existing tag }
  Tag.NeedRewrite :=
    (Tag.ID <> ID3V2_ID) or
    (GetTagSize(Tag) < TagSize) or
    (GetTagSize(Tag) > ID3V2_MAX_SIZE);
  { Calculate padding size and set padded tag size }
  if Tag.NeedRewrite then
    Tag.PaddingSize := ID3V2_MAX_SIZE - TagSize
  else
    Tag.PaddingSize := GetTagSize(Tag) - TagSize;
  if Tag.PaddingSize > 0 then
    Inc(TagSize, Tag.PaddingSize);
  { Build tag header }
  Tag.ID := ID3V2_ID;
  Tag.Version := TAG_VERSION_2_3;
  Tag.Revision := 0;
  Tag.Flags := 0;
  { Convert tag size }
  for Iterator := 1 to 4 do
    Tag.Size[Iterator] := ((TagSize - 10) shr ((4 - Iterator) * 7)) and $7F;
end;

{ --------------------------------------------------------------------------- }

function ReplaceTag(const FileName: WideString; TagData: TStream): Boolean;
var
  Destination: TTntFileStream;
begin
  { Replace old tag with new tag data }
  Result := false;
  if (not WideFileExists(FileName)) or (WideFileSetAttr(FileName, 0) <> True) then exit;
  try
    TagData.Position := 0;
    Destination := TTntFileStream.Create(FileName, fmOpenReadWrite);
    Destination.CopyFrom(TagData, TagData.Size);
    Destination.Free;
    Result := true;
  except
    { Access error }
  end;
end;

{ --------------------------------------------------------------------------- }

function RebuildFile(const FileName: WideString; TagData: TStream): Boolean;
var
  Tag: TagInfo;
  Source, Destination: TTntFileStream;
  BufferName: string;
begin
  { Rebuild file with old file data and new tag data (optional) }
  Result := false;
  if (not WideFileExists(FileName)) or (WideFileSetAttr(FileName, 0) <> True) then exit;
  if not ReadHeader(FileName, Tag) then exit;
  if (TagData = nil) and (Tag.ID <> ID3V2_ID) then exit;
  try
    { Create file streams }
    BufferName := FileName + '~';
    Source := TTntFileStream.Create(FileName, fmOpenRead);
    Destination := TTntFileStream.Create(BufferName, fmCreate);
    { Copy data blocks }
    if Tag.ID = ID3V2_ID then Source.Seek(GetTagSize(Tag), soFromBeginning);
    if TagData <> nil then Destination.CopyFrom(TagData, 0);
    Destination.CopyFrom(Source, Source.Size - Source.Position);
    { Free resources }
    Source.Free;
    Destination.Free;
    { Replace old file and delete temporary file }
    if (WideDeleteFile(FileName)) and (WideRenameFile(BufferName, FileName)) then
      Result := true
    else
      raise Exception.Create('');
  except
    { Access error }
    if WideFileExists(BufferName) then WideDeleteFile(BufferName);
  end;
end;

{ --------------------------------------------------------------------------- }

function SaveTag(const FileName: WideString; Tag: TagInfo): Boolean;
var
 {$IFDEF DELPHI_2007_BELOW}
  TagData: TStringStream;
 {$ELSE}
 // TStringStream of Delphi 2009 shows erroneous behaviour at handling ansistring.
 // So, use TAnsiStringStream instead of TStringStream for ansistring.
  TagData: TAnsiStringStream;
 {$ENDIF}
  Iterator, FrameSize: Integer;
  Padding: array [1..ID3V2_MAX_SIZE] of Byte;
begin
  { Build and write tag header and frames to stream }
  {$IFDEF DELPHI_2007_BELOW}
   TagData := TStringStream.Create('');
  {$ELSE}
   TagData := TAnsiStringStream.Create('');
  {$ENDIF}
  BuildHeader(Tag);
  TagData.Write(Tag, 10);
  for Iterator := 1 to ID3V2_FRAME_COUNT do
    if Tag.Frame[Iterator] <> '' then
    begin
      TagData.WriteString(ID3V2_FRAME_NEW[Iterator]);
      FrameSize := Swap32(Length(Tag.Frame[Iterator]) + 1);
      TagData.Write(FrameSize, SizeOf(FrameSize));
      if Iterator = 12 then
        TagData.WriteString(#0#0#0 + Tag.Frame[Iterator])
      else
        TagData.WriteString(#0#0#1 + Tag.Frame[Iterator]);
      // in #0#0#1 last #1 indicates that this is a Unicode string
    end;
  { Add padding }
  FillChar(Padding, SizeOf(Padding), 0);
  if Tag.PaddingSize > 0 then
    TagData.Write(Padding, Tag.PaddingSize);
  { Rebuild file or replace tag with new tag data }
  if Tag.NeedRewrite then
    Result := RebuildFile(FileName, TagData)
  else
    Result := ReplaceTag(FileName, TagData);
  TagData.Free;
end;

{ ********************** Private functions & procedures ********************* }

procedure TID3v2.FSetTitle(const NewTitle: WideString);
begin
  { Set song title }
  FTitle := Trim(NewTitle);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetArtist(const NewArtist: WideString);
begin
  { Set artist name }
  FArtist := Trim(NewArtist);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetAlbum(const NewAlbum: WideString);
begin
  { Set album title }
  FAlbum := Trim(NewAlbum);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetTrack(const NewTrack: Word);
begin
  { Set track number }
  FTrack := NewTrack;
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetTrackString(const NewTrack: WideString);
begin
  FTrackString := Trim(NewTrack);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetYear(const NewYear: WideString);
begin
  { Set release year }
  FYear := Trim(NewYear);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetGenre(const NewGenre: WideString);
begin
  { Set genre name }
  FGenre := Trim(NewGenre);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetComment(const NewComment: WideString);
begin
  { Set comment }
  FComment := Trim(NewComment);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetComposer(const NewComposer: WideString);
begin
  { Set composer name }
  FComposer := Trim(NewComposer);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetEncoder(const NewEncoder: WideString);
begin
  { Set encoder name }
  FEncoder := Trim(NewEncoder);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetCopyright(const NewCopyright: WideString);
begin
  { Set copyright information }
  FCopyright := Trim(NewCopyright);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetOrigArtist(const NewOrigArtist: WideString);
begin
  { Set original artist name }
  FOrigArtist := Trim(NewOrigArtist);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetLanguage(const NewLanguage: WideString);
begin
  { Set language }
  FLanguage := Trim(NewLanguage);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetLink(const NewLink: WideString);
begin
  { Set URL link }
  FLink := Trim(NewLink);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetLyrics(const NewLyrics: WideString);
begin
  { Set lyrics }
  FLyrics := Trim(NewLyrics);
end;

{ ********************** Public functions & procedures ********************** }

constructor TID3v2.Create;
begin
  { Create object }
  inherited;
  ResetData;
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.ResetData;
begin
  { Reset all variables }
  FExists := False;
  FVersionID := 0;
  FSize := 0;
  FTitle := '';
  FArtist := '';
  FAlbum := '';
  FTrack := 0;
  FTrackString := '';
  FYear := '';
  FGenre := '';
  FComment := '';
  FComposer := '';
  FEncoder := '';
  FCopyright := '';
  FLanguage := '';
  FLink := '';
  FOrigArtist := '';
  FLyrics := '';
  FTSIZ := '';
end;

{ --------------------------------------------------------------------------- }

function TID3v2.ReadFromFile(const FileName: WideString): Boolean;
var
  Tag: TagInfo;
begin
  { Reset data and load header from file to variable }
  ResetData;
  Result := ReadHeader(FileName, Tag);
  { Process data if loaded and header valid }
  if (Result) and (Tag.ID = ID3V2_ID) then
  begin
    FExists := true;
    { Fill properties with header data }
    FVersionID := Tag.Version;
    FSize := GetTagSize(Tag);
    { Get information from frames if version supported }
    if (FVersionID in [TAG_VERSION_2_2..TAG_VERSION_2_4]) and (FSize > 0) then
    begin
      if FVersionID > TAG_VERSION_2_2 then
        ReadFramesNew(FileName, Tag)
      else
        ReadFramesOld(FileName, Tag);
      FTitle := GetContent2(Tag.Frame[1], Tag.Frame[15], Tag.FrameEncode[1], Tag.FrameEncode[15]);
      FArtist := GetContent2(Tag.Frame[2], Tag.Frame[14], Tag.FrameEncode[2], Tag.FrameEncode[14]);
      FAlbum := GetContent2(Tag.Frame[3], Tag.Frame[16], Tag.FrameEncode[3], Tag.FrameEncode[16]);
      FTrack := ExtractTrack(Tag.Frame[4]);
      FTrackString := GetUnicode2(Tag.Frame[4], Tag.FrameEncode[4]);
      FYear := ExtractYear(Tag.Frame[5], Tag.Frame[13]);
      FGenre := ExtractGenre(Tag.Frame[6]);
      FComment := ExtractText(Tag.Frame[7], True, Tag.FrameEncode[7]);
      FComposer := GetUnicode2(Tag.Frame[8], Tag.FrameEncode[8]);
      FEncoder := GetUnicode2(Tag.Frame[9], Tag.FrameEncode[9]);
      FCopyright := GetUnicode2(Tag.Frame[10], Tag.FrameEncode[10]);
      FLanguage := GetUnicode2(Tag.Frame[11], Tag.FrameEncode[11]);
      FLink := ExtractText(Tag.Frame[12], False, Tag.FrameEncode[12]);
      FOrigArtist := GetUnicode2(Tag.Frame[14], Tag.FrameEncode[14]);
      FLyrics := ExtractText(Tag.Frame[18], True, Tag.FrameEncode[18]);
      FTSIZ := GetUnicode2(Tag.Frame[17], Tag.FrameEncode[17]);
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

function TID3v2.SaveToFile(const FileName: WideString): Boolean;
var
  Tag: TagInfo;
begin
  { Check for existing tag }
  FillChar(Tag, SizeOf(Tag), 0);
  ReadHeader(FileName, Tag);
  { Prepare tag data and save to file }
  Tag.Frame[1] := GetANSIUnicode(FTitle);
  Tag.Frame[2] := GetANSIUnicode(FArtist);
  Tag.Frame[3] := GetANSIUnicode(FAlbum);
  Tag.Frame[4] := GetANSIUnicode(FTrackString);
  Tag.Frame[5] := GetANSIUnicode(FYear);
  Tag.Frame[6] := GetANSIUnicode(FGenre);
  if FComment <> '' then
    Tag.Frame[7] := 'eng' + #0#0 + GetANSIUnicode(FComment);
  Tag.Frame[8] := GetANSIUnicode(FComposer);
  Tag.Frame[9] := GetANSIUnicode(FEncoder);
  Tag.Frame[10] := GetANSIUnicode(FCopyright);
  Tag.Frame[11] := GetANSIUnicode(FLanguage);
  if FLyrics <> '' then
    Tag.Frame[18] := 'eng' + #0#0 + GetANSIUnicode(FLyrics);
  if FLink <> '' then Tag.Frame[12] := #0 + FLink;
  Tag.Frame[14] := GetANSIUnicode(FOrigArtist);
  Result := SaveTag(FileName, Tag);
end;

{ --------------------------------------------------------------------------- }

function TID3v2.RemoveFromFile(const FileName: WideString): Boolean;
begin
  { Remove tag from file }
  Result := RebuildFile(FileName, nil);
end;

{ --------------------------------------------------------------------------- }

// This procedure is added for TBASSPlayer  by Silhwan Hyun
procedure ReadFramesNew2(TagStream: PAnsiChar; var Tag: TagInfo);
var
  Frame: FrameHeaderNew;
  Data: array [1..500] of AnsiChar;
  DataOffset, DataSize: Integer;
  p : ^byte;
begin
  { Get information from frames (ID3v2.3.x & ID3v2.4.x) }
  try
    p := pointer(TagStream);
    inc(p, 10);
    DataOffset := 10;

    while ((DataOffset < GetTagSize(Tag)) and (DataOffset < Tag.FileSize)) do
    begin
      FillChar(Data, SizeOf(Data), 0);
      Move(pointer(p)^, Frame, 10);
      inc(p, 10);

      if not (Frame.ID[1] in ['A'..'Z']) then
         break;

      if Swap32(Frame.Size) > SizeOf(Data) then
         DataSize := SizeOf(Data)
      else
         DataSize := Swap32(Frame.Size);

      Move(pointer(p)^, Data, DataSize);
      if Frame.Flags and $8000 <> $8000 then
         SetTagItem(Frame.ID, Data, Tag, ord(Data[1]));

      inc(p, Swap32(Frame.Size));
      DataOffset := Longword(TagStream) - Longword(p);
    end;
  except

  end;
end;

{ --------------------------------------------------------------------------- }

// This procedure is added for TBASSPlayer  by Silhwan Hyun
procedure ReadFramesOld2(TagStream: PAnsiChar; var Tag: TagInfo);
var
  Frame: FrameHeaderOld;
  Data: array [1..500] of AnsiChar;
  DataOffset, FrameSize, DataSize: Integer;
  p : ^byte;
begin
  { Get information from frames (ID3v2.2.x }
  try
    p := pointer(TagStream);
    inc(p, 10);
    DataOffset := 10;

    while ((DataOffset < GetTagSize(Tag)) and (DataOffset < Tag.FileSize)) do
    begin
      FillChar(Data, SizeOf(Data), 0);
      Move(pointer(p)^, Frame, 6);
      inc(p, 6);

      if not (Frame.ID[1] in ['A'..'Z']) then
         break;

      FrameSize := Frame.Size[1] shl 16 + Frame.Size[2] shl 8 + Frame.Size[3];
      if FrameSize > SizeOf(Data) then
         DataSize := SizeOf(Data)
      else
         DataSize := FrameSize;

      Move(pointer(p)^, Data, DataSize);
      SetTagItem(Frame.ID, Data, Tag, ord(Data[1]));

      inc(p, FrameSize);
      DataOffset := Longword(TagStream) - Longword(p);
    end;
  except

  end;
end;

{ --------------------------------------------------------------------------- }

function ReadID3v2TagFromStream(TagStream: PAnsiChar; StreamSize : Longword;
  var MP3TagInfo: TMP3TagInfo): Boolean;
var
  Tag: TagInfo;
begin
   Result := false;
   
   Move(Pointer(TagStream)^, Tag, 10);
   if Tag.ID = ID3V2_ID then
   begin
      Result := true;
      Tag.FileSize := StreamSize;

      if (Tag.Version in [TAG_VERSION_2_2..TAG_VERSION_2_4]) and (GetTagSize(Tag) > 0) then
      begin
         if Tag.Version > TAG_VERSION_2_2
         then ReadFramesNew2(TagStream, Tag)
         else ReadFramesOld2(TagStream, Tag);

         MP3TagInfo.Title := GetContent(Tag.Frame[1], Tag.Frame[15]);
         MP3TagInfo.Artist := GetContent(Tag.Frame[2], Tag.Frame[14]);
         MP3TagInfo.Album := GetContent(Tag.Frame[3], Tag.Frame[16]);
         MP3TagInfo.Track := ExtractTrack(Tag.Frame[4]);
         MP3TagInfo.Year := ExtractYear(Tag.Frame[5], Tag.Frame[13]);
         MP3TagInfo.Genre := ExtractGenre(Tag.Frame[6]);
         MP3TagInfo.Comment := ExtractText(Tag.Frame[7], true, Tag.FrameEncode[7]);
      end;
   end;
end;

end.
