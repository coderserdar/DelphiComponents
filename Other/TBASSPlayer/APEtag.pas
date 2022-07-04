{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Class TAPEtag - for manipulating with APE tags                              }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2000-2002 by Jurgen Faul                                      }
{ Copyright (c) 2003-2005 by The MAC Team                                     }
{                                                                             }
{ Version 2.1 (April 2005) by Gambit                                          }
{   - updated to unicode file access                                          }
{                                                                             }
{ Version 2.0 (30 May 2003) by Jean-Marie Prat                                }
{   - Writing support for APE 2.0 tags                                        }
{   - Removed UTF8 decoding since calling application is supposed to provide  }
{     or handle UTF8 strings.                                                 }
{   - Removed direct tag infos. All fields are now stored into an array. A    }
{     specific field can be requested using SeekField function.               }
{   - Introduced procedures to add/remove/order fields.                       }
{                                                                             }
{ Version 1.0 (21 April 2002)                                                 }
{   - Reading & writing support for APE 1.0 tags                              }
{   - Reading support for APE 2.0 tags (UTF-8 decoding)                       }
{   - Tag info: title, artist, album, track, year, genre, comment, copyright  }
{                                                                             }
{ This library is free software; you can redistribute it and/or               }
{ modify it under the terms of the GNU Lesser General Public                  }
{ License as published by the Free Software Foundation; either                }
{ version 2.1 of the License, or (at your option) any later version.          }
{                                                                             }
{ This library is distributed in the hope that it will be useful,             }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of              }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           }
{ Lesser General Public License for more details.                             }
{                                                                             }
{ You should have received a copy of the GNU Lesser General Public            }
{ License along with this library; if not, write to the Free Software         }
{ Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   }
{                                                                             }
{ *************************************************************************** }


// -- Modified for TBASSPlayer  by Silhwan Hyun --
// Replaced the refernced unit TntClasses of "Tnt Delphi UNICODE Controls" package
//  with UniCodeUtils and TntCollection.

//  - Modified for Delphi 2009  (30 Apr 2009)

unit APETag;

interface

{$INCLUDE Delphi_Ver.inc}

uses
  Dialogs, Classes, SysUtils, UniCodeUtils, TntCollection
 {$IFNDEF DELPHI_2007_BELOW}, AnsiStringStream, AnsiStrings{$ENDIF};

const
  { Tag ID }
  ID3V1_ID = 'TAG';                                                   { ID3v1 }
  APE_ID = 'APETAGEX';                                                  { APE }

  { Size constants }
  ID3V1_TAG_SIZE = 128;                                           { ID3v1 tag }
  APE_TAG_FOOTER_SIZE = 32;                                  { APE tag footer }
  APE_TAG_HEADER_SIZE = 32;                                  { APE tag header }

  { Version of APE tag }
  APE_VERSION_1_0 = 1000;
  APE_VERSION_2_0 = 1000;

type
  { APE tag header/footer - for internal use }
  RTagHeader = record
    { Real structure of APE footer }
    ID: array [0..7] of AnsiChar;                             { Always "APETAGEX" }
    Version: Integer;                                           { Tag version }
    Size: Integer;                                { Tag size including footer }
    Fields: Integer;                                       { Number of fields }
    Flags: Integer;                                               { Tag flags }
    Reserved: array [0..7] of AnsiChar;                  { Reserved for later use }
    { Extended data }
    DataShift: Byte;                                { Used if ID3v1 tag found }
    FileSize: Integer;                                    { File size (bytes) }
  end;

  UTF8string_ = AnsiString;
  RField = record
    Name: AnsiString;
    Value: UTF8string_;
  end;
  AField = array of RField;

  TAPETag = class
    private
      pField: Afield;
      pExists: Boolean;
      pVersion: Integer;
      pSize: Integer;
      function  ReadFooter(sFile: WideString; var footer: RTagHeader): boolean;
      procedure ReadFields(sFile: WideString; footer: RTagHeader);
    public
      property Exists: Boolean read pExists;              { True if tag found }
      property Version: Integer read pVersion;                  { Tag version }
      property Fields: AField read pField;
      property Size: Integer read pSize;
      constructor Create();
      function    ReadFromFile(sFile: WideString): Boolean;
      function    RemoveTagFromFile(sFile: WideString): Boolean;
      function    WriteTagInFile(sFile: WideString): Boolean;
      procedure   InsertField(pos: integer ; name: AnsiString ; value: UTF8string_);
      { Insert field so that it has position pos}
      procedure   RemoveField(pos: integer);
      procedure   AppendField(name: Ansistring ; value: UTF8string_);
      procedure   SwapFields(pos1, pos2: integer);
      function    SeekField(Field: Ansistring): UTF8string_;
      procedure ResetData;      
  end;

implementation

//----------------------------------------------------------------------------//
//                             Private stuff                                  //
//----------------------------------------------------------------------------//

procedure TAPETag.ResetData();
begin
  SetLength(pField,0);
  pExists := False;
  pVersion := 0;
  pSize := 0;
end;

// ----------------------------------------------------------------------------

function TAPETag.ReadFooter(sFile: WideString; var footer: RTagHeader): boolean;
var
  SourceFile: TTntFileStream;
  TagID: array [1..3] of AnsiChar;
  Transferred: Integer;
begin
  FillChar(Footer, SizeOf(Footer), 0);
  try
    Result := true;
    { Set read-access and open file }
    SourceFile := TTntFileStream.Create(sFile, fmOpenRead or fmShareDenyWrite);
    Footer.FileSize := SourceFile.Size;
    if (IOResult <> 0) then
    begin
      SourceFile.Free;
      Result := False;
      Exit;
    end;

    { Check for existing ID3v1 tag }
    if (Footer.FileSize - ID3V1_TAG_SIZE > 0) then
    begin
      SourceFile.Seek(Footer.FileSize - ID3V1_TAG_SIZE, soFromBeginning);
      SourceFile.Read(TagID, SizeOf(TagID));

      if TagID = ID3V1_ID then
        Footer.DataShift := ID3V1_TAG_SIZE
      else
        Footer.DataShift := 0;
    end;

    { Read footer data }
    Transferred := 0;
    if (Footer.FileSize - Footer.DataShift - APE_TAG_FOOTER_SIZE) > 0 then
    begin
      SourceFile.Seek(Footer.FileSize - Footer.DataShift - APE_TAG_FOOTER_SIZE, soFromBeginning);
      //BlockRead(SourceFile, Footer, APE_TAG_FOOTER_SIZE, Transferred);
      Transferred := SourceFile.Read(Footer, APE_TAG_FOOTER_SIZE);
    end;
    SourceFile.Free;

    { if transfer is not complete }
    if Transferred < APE_TAG_FOOTER_SIZE then
      Result := false;
  except
    { Error }
    Result := false;
  end;
end;

// ----------------------------------------------------------------------------

procedure TAPETag.ReadFields(sFile: WideString; footer: RTagHeader);
var
  SourceFile: TTntFileStream;
  FieldName: AnsiString;
  FieldValue: array [1..250] of AnsiChar;
  NextChar: AnsiChar;
  Iterator, ValueSize, ValuePosition, FieldFlags: Integer;
begin
  try
  { Set read-access, open file }
    SourceFile := TTntFileStream.Create(sFile, fmOpenRead or fmShareDenyWrite);
    SourceFile.Seek(footer.FileSize - footer.DataShift - footer.Size, soFromBeginning);

  { Read all stored fields }
    SetLength(pField,footer.Fields);
    for Iterator := 0 to footer.Fields-1 do
    begin
      FillChar(FieldValue, SizeOf(FieldValue), 0);
      SourceFile.Read(ValueSize, SizeOf(ValueSize));
      SourceFile.Read(FieldFlags, SizeOf(FieldFlags));
      FieldName := '';

      repeat
        SourceFile.Read(NextChar, SizeOf(NextChar));
        FieldName := FieldName + NextChar;
      until Ord(NextChar) = 0;

      ValuePosition := SourceFile.Position;
      SourceFile.Read(FieldValue, ValueSize mod SizeOf(FieldValue));
     {$IFNDEF DELPHI_2007_BELOW}
      pField[Iterator].Name := Ansistrings.Trim(FieldName);
      pField[Iterator].Value := Ansistrings.Trim(FieldValue);
     {$ELSE}
      pField[Iterator].Name := Trim(FieldName);
      pField[Iterator].Value := Trim(FieldValue);
     {$ENDIF}
      SourceFile.Seek(ValuePosition + ValueSize, soFromBeginning);
    end;

    SourceFile.Free;
  except

  end;

end;


//----------------------------------------------------------------------------//
//                             Public stuff                                   //
//----------------------------------------------------------------------------//

constructor TAPETag.Create();
begin
  inherited;

  ResetData;
end;

// ----------------------------------------------------------------------------

function TAPETag.ReadFromFile(sFile: WideString): Boolean;
var
  Footer: RTagHeader;
begin
  ResetData;
  Result := ReadFooter(sFile, Footer);

{ Process data if loaded and footer valid }
  if (Result) and (Footer.ID = APE_ID) then
  begin
    pExists := True;
    pVersion := Footer.Version;
    pSize := Footer.Size;
    ReadFields(sFile, Footer);
  end;
end;

// ----------------------------------------------------------------------------

function TAPETag.RemoveTagFromFile(sFile: WideString): Boolean;
var
  SourceFile: TTntFileStream;
  Footer: RTagHeader;
  ID3: pointer;
begin
  Result := ReadFooter(sFile, Footer);

  { Process data if loaded and footer valid }
  if (Result) and (Footer.ID = APE_ID) then
  begin
    SourceFile := TTntFileStream.Create(sFile, fmOpenReadWrite or fmShareDenyWrite);
  { If there is an ID3v1 tag roaming around behind the APE tag, we have to buffer it }
    if Footer.DataShift = ID3V1_TAG_SIZE then
    begin
      GetMem(ID3,ID3V1_TAG_SIZE);
      SourceFile.Seek(footer.FileSize - footer.DataShift, soFromBeginning);
      SourceFile.Read(ID3^, ID3V1_TAG_SIZE);
    end;

  { If this is an APEv2, header size must be added }
    if (Footer.Flags shr 31) > 0 then
      Inc(Footer.Size, APE_TAG_HEADER_SIZE);
    SourceFile.Seek(Footer.FileSize - footer.Size-Footer.DataShift, soFromBeginning);

  { If there is an ID3v1 tag roaming around, we copy it }
    if Footer.DataShift = ID3V1_TAG_SIZE then
    begin
      SourceFile.Write(ID3^, ID3V1_TAG_SIZE);
      FreeMem(ID3,128);
    end;

    SourceFile.Seek(Footer.FileSize-Footer.Size, soFromBeginning);
  //truncate
    SourceFile.Size := SourceFile.Position;
    SourceFile.Free;
  end;
end;

// ----------------------------------------------------------------------------

function TAPETag.WriteTagInFile(sFile: WideString): Boolean;
const
  APEPreample: array [0..7] of Ansichar = ('A','P','E','T','A','G','E','X');
var
  SourceFile: TTntFileStream;
  Header, Footer, RefFooter: RTagHeader;
  ID3: PAnsiChar;
  i, len, TagSize, Flags: integer;
 {$IFDEF DELPHI_2007_BELOW}
  TagData: TStringStream;
 {$ELSE}
  TagData: TAnsiStringStream;
 {$ENDIF}
begin
  ID3 := nil;
 // method : first, save any eventual ID3v1 tag lying around
 //          then we truncate the file after the audio data
 //          then write the APE tag (and possibly the ID3)
  Result := ReadFooter(sFile, RefFooter);

{ Process data if loaded and footer valid }
  if (Result) and (RefFooter.ID = APE_ID) then
  begin
    SourceFile := TTntFileStream.Create(sFile, fmOpenReadWrite or fmShareDenyWrite);
  { If there is an ID3v1 tag roaming around behind the APE tag, we have to buffer it }
    if RefFooter.DataShift = ID3V1_TAG_SIZE then
    begin
      GetMem(ID3,ID3V1_TAG_SIZE);
      SourceFile.Seek(Reffooter.FileSize - Reffooter.DataShift, soFromBeginning);
      SourceFile.Read(ID3^, ID3V1_TAG_SIZE);
    end;

  { If this is an APEv2, header size must be added }
  //if (RefFooter.Flags shr 31) > 0 then
    Inc(RefFooter.Size, APE_TAG_HEADER_SIZE);
    SourceFile.Seek(RefFooter.FileSize - RefFooter.Size-RefFooter.DataShift, soFromBeginning);
   //truncate
    SourceFile.Size := SourceFile.Position;
    SourceFile.Free;
  end;

 {$IFDEF DELPHI_2007_BELOW}
  TagData := TStringStream.Create('');
 {$ELSE}
  TagData := TAnsiStringStream.Create('');
 {$ENDIF}
  TagSize := APE_TAG_FOOTER_SIZE;

  for i:=0 to high(pField) do
  begin
    TagSize := TagSize + 9 + Length(pField[i].Name) + Length(pField[i].Value);
  end;

  Header.ID[0] := 'A';
  Header.ID[1] := 'P';
  Header.ID[2] := 'E';
  Header.ID[3] := 'T';
  Header.ID[4] := 'A';
  Header.ID[5] := 'G';
  Header.ID[6] := 'E';
  Header.ID[7] := 'X';
  Header.Version := 2000;
  Header.Size := TagSize;
  Header.Fields := Length(pField);
  Header.Flags := 0 or (1 shl 29) or (1 shl 31);       // tag contains a header and this is the header
 //ShowMessage(IntToSTr(Header.Flags));
  TagData.Write(Header, APE_TAG_HEADER_SIZE);

  for i:=0 to high(pField) do
  begin
    len := Length(pField[i].Value);
    Flags := 0;
    TagData.Write(len, SizeOf(len));
    TagData.Write(Flags, SizeOf(Flags));
    TagData.WriteString(pField[i].Name + #0);
    TagData.WriteString(pField[i].Value);
  end;

  Footer.ID[0] := 'A';
  Footer.ID[1] := 'P';
  Footer.ID[2] := 'E';
  Footer.ID[3] := 'T';
  Footer.ID[4] := 'A';
  Footer.ID[5] := 'G';
  Footer.ID[6] := 'E';
  Footer.ID[7] := 'X';
  Footer.Version := 2000;
  Footer.Size := TagSize;
  Footer.Fields := Length(pField);
  Footer.Flags := 0 or (1 shl 31);                     // tag contains a header and this is the footer
  TagData.Write(Footer,APE_TAG_FOOTER_SIZE) ;

  if (RefFooter.DataShift = ID3V1_TAG_SIZE) and Assigned(ID3)then
  begin
    TagData.Write(ID3^,ID3V1_TAG_SIZE);
    FreeMem(ID3);
  end;

  SourceFile := TTntFileStream.Create(sFile, fmOpenReadWrite or fmShareDenyWrite);
  SourceFile.Seek(0, soFromEnd);
  TagData.Seek(0, soFromBeginning);
  SourceFile.CopyFrom(TagData, TagData.Size);
  SourceFile.Free;
  TagData.Free;
end;

// ----------------------------------------------------------------------------

procedure TAPETag.InsertField (pos: integer ; name: Ansistring ; value: UTF8string_);
var
  dummy: AField;
  i: integer;
begin
  if pos>=Length(pField) then exit;

  SetLength(dummy,Length(pField)-pos);
  dummy := copy(pField,pos,Length(dummy));
  pField[pos].Name := name;
  pField[pos].Value := value;
  SetLength(pField,Length(pField)+1);

  for i:= pos+1 to high(pField) do
    pField[i] := dummy[i-pos-1];
end;

// ----------------------------------------------------------------------------

procedure TAPETag.RemoveField (pos: integer);
var
  i: integer;
begin
  if pos>Length(pField) then exit;

  for i:=pos+1 to high(pField) do
    pField[i-1]:=pField[i];

  SetLength(pField,Length(pField)-1);
end;

// ----------------------------------------------------------------------------

procedure TAPETag.AppendField(name: Ansistring ; value: UTF8string_);
begin
  SetLength(pField,Length(pField)+1);
  pField[high(pField)].Name := name;
  pField[high(pField)].Value := value;
end;

// ----------------------------------------------------------------------------

procedure TAPETag.SwapFields (pos1, pos2: integer);
var
  dummy: RField;
begin
  dummy.Name         := pField[pos1].Name;
  dummy.Value        := pField[pos1].Value;
  pField[pos1].Name  := pField[pos2].Name;
  pField[pos1].Value := pField[pos2].Value;
  pField[pos2].Name  := dummy.Name;
  pField[pos2].Value := dummy.Value;
end;

// ----------------------------------------------------------------------------

function TAPETag.SeekField(Field: Ansistring): UTF8string_;
var
  i: integer;
begin
  Result := '';

  for i:=0 to high(pField) do
  begin
  {$IFNDEF DELPHI_2007_BELOW}
   if Ansistrings.UpperCase(Field)=Ansistrings.UpperCase(pField[i].Name) then
  {$ELSE}
   if UpperCase(Field)=UpperCase(pField[i].Name) then
  {$ENDIF}
   begin
     Result := pField[i].Value;
     Break;
   end;
  end;
end;

// ----------------------------------------------------------------------------

end.
