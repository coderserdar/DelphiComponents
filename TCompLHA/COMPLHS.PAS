unit complhs;

(* TCompLHAStream V1.0
   Written for single-file compression to/from a stream with LZH5 only.

   *Requires* TCompLHA V2.5 or later: http://www.spis.co.nz/compLHA.htm.

   Copyright (c) 1998 P J Hyde, South Pacific Information Services Ltd
   All Rights Reserved

   This code may be freely used and adapted so long as it is used
   in conjunction with a registered version of TCompLHA.

   Key class: TStream descendant TCompLHAStream -- call with its
     own special Create, then use regular stream methods to access it
     (read, write, seek/position, free). Compression/expansion using LZH5 is
     automatic. Note that it makes use of a TMemoryStream buffer and hence
     is NOT recommended for use with very large archives (e.g. multi-megabyte).

   Creation:
     constructor Create(stream: TStream; Mode: TCompLHAStreamMode;
                        const RegName: string; RegNumber: Integer);
     where: "stream" is any stream based class to which you want to read/write
               compressed data, e.g. a TFileStream you've just opened.
            "Mode" is clhmRead or clhmWrite
            RegName and RegNumber are your TCompLHA registration codes -- these
               can be empty strings if you are linking with the full CompLHA source
               which is optionally available with TCompLHA.
*)


interface

uses Classes,
     CompLHA; { ideally, you should link with a special version of this
                which has all compression-method $DEFINES at the top DISABLED
                except LZH5. Just saves code/data space, basically -- purchase the
                TCompLHA source if you want to do this. }

type
  TCompLHAStreamCore = class(TCompLHA)
  public
     constructor Create(AOwner: TComponent); override;

     procedure CompressStreamToStream(compressedStream, uncompressedStream: TStream); { always uses coLZH5 mode... }

     procedure ExpandStreamFromStream(uncompressedStream, compressedStream: TStream); { new, just for this component }
  end;

  TCompLHAStreamMode = (clhmRead, clhmWrite);

  TCompLHAStream = Class(TStream)
  private
     FOpened: Boolean;
     FModified: Boolean;
     FMemoryStream: TMemoryStream;
     FStream: TStream;
     FCompLHAStreamCore: TCompLHAStreamCore; { a TCompLHA in 'drag }'
  public
     constructor Create(stream: TStream; Mode: TCompLHAStreamMode; const RegName: string; RegNumber: Integer);
     destructor Destroy; Override;
     function Read(var Buffer; Count: Longint): Longint; Override;
     function Write(const Buffer; Count: Longint): Longint; Override;
     function Seek(Offset: Longint; Origin: Word): Longint; Override;
     procedure Truncate;
  end;


implementation

constructor TCompLHAStreamCore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ArchiveType := caLHA;   { default it to use regular LHA archive mode }
end;

{ always uses LZH5, always assumes it is the only archive (to be) in the stream,
  i.e. we are not putting multiple files or multiple archives in there }
procedure TCompLHAStreamCore.CompressStreamToStream(compressedStream, uncompressedStream: TStream);
var fHdr: TCompLHAFileHeader;
begin
  fHdr.FileDate := 0;
  Fhdr.Filetime := 0;
  fHdr.attributes :=0; { all else is set & written in the next call... }
  CompressStreamToArchiveStream(compressedStream,uncompressedStream,
                                                            'CompLHA',fHdr,colh5);
end;

{ always assumes it is about to fetch the only archive in the stream, therefore
  doesn't care about name }
procedure TCompLHAStreamCore.ExpandStreamFromStream(uncompressedStream, compressedStream: TStream); { new just for this component }
var thisfile: string;
    fHdr: TCompLHAFileHeader;
    crc: Word;
begin
  if GetFileHeader(compressedStream,Fhdr,thisfile,crc) then
     DoExpand(unCompressedStream,compressedStream,fhdr.CompressedSize,fhdr.Fullsize,crc,
              Recognize(fhdr.ComMethodID),False);
end;

constructor TCompLHAStream.Create(stream: TStream; Mode: TCompLHAStreamMode; const RegName: string; RegNumber: Integer);
begin
  FStream:= stream;
  FMemoryStream := TMemoryStream.create;

  FCompLHAStreamCore := TCompLHAStreamCore.Create(nil);
  FCompLHAStreamCore.RegName := RegName; { will get nag messages unless valid,
                                          or unless you are building the project
                                          with the TCompLHA source handy }
  FCompLHAStreamCore.RegNumber := RegNumber;

  if Mode=clhmRead then { fill the buffer initially }
     FCompLHAStreamCore.ExpandStreamFromStream(FMemoryStream,FStream)
  else
    FMemoryStream.SetSize(FStream.size); { preset wot they will write to us... }

  FOpened := True;
  FMemoryStream.Position:=0;
  FStream.Position := 0;
  { At this moment we have either a full (expanded) memory stream, or an empty
   (ready to be written) Memorystream allegedly the same size as the original
   object }
end;

destructor TCompLHAStream.Destroy;
begin
  if FModified then { we must Compress the result! }
  begin
    FMemoryStream.Position := 0;
    FStream.Position := 0;
    FCompLHAStreamCore.CompressStreamToStream(FStream,FMemoryStream)
  end;
  FMemoryStream.destroy;
end;

function TCompLHAStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Fopened then
    Result := FMemoryStream.Read(Buffer,Count)
  else
    Result := 0;
end;

function TCompLHAStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Fopened then
  begin
    Result := FMemoryStream.Write(Buffer,Count);
    FModified := True
  end else
    Result := 0;
end;

function TCompLHAStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if Fopened then
    Result := FMemoryStream.Seek(Offset,Origin)
  else
    Result := 0;
end;

procedure TCompLHAStream.Truncate;
begin
  if FOpened then
  begin
    FMemorystream.setSize(Fmemorystream.Position);
    FModified := True;
  end;
end;

end.
