(*  ARC2BLOB.PAS for TCompress V3.0 (no change from V2.5 except para 4 in comments)

This code is designed to be dropped into the COMPDEMO application, adding
one new routine (LoadArchivedFileToBlobStream) and replacing one existing one
(CDBImage1DragDrop).

It shows what is probably the most efficient way to load compressed data from
a file archive to a blob field, without using an intermediate file as
COMPDEMO currently does.

Keep in mind that both this and the ARC2MEM code require manipulation of some
of the TCompress data structures... Unlike ARC2MEM, this approach doesn't
require a large memory buffer (because no expansion step), but does require
more knowledge of the TCompress data structures.

*** In TCompress 3.0, an alternative (and far easier) approach would be to use
the new ExpandStreamFromArchive routine. Because this would be writing
*expanded* data to the blob (which hence must be recompressed for storage),
two important points would apply if you used it:
1. It would not be as efficient as this routine, which copies the data in
   compressed form
2. You must pass a TCBlobstream to the above method, NOT the TBlobstream which
   this routine uses.
ExpandStreamFromArchive would be ideal, however, if you were loading a regular
field or Delphi stream-based object from an archive (e.g. a TDBImage)
*)

{ Example of Expanding a file DIRECTLY from an archive to a COMPRESSED field's blobstream.
  Important note: we are NOT expanding the data at all, thus we are actually bypassing
  all the expansion/compression stuff and writing directly to the underlying database }
procedure TForm1.LoadArchivedFileToBlobStream(bs:TBlobstream;filepath:String);
var fs: TFilestream;
    cfinfo: TCompressedFileInfo;
    fHeader: TCompressedFileHeader; { file header so we can get compression mode }
    aheader: TCompressHeader;       { archive header required for Blobstream     }
    cmode: string;
begin
  cfinfo := TCompressedFileInfo(FileList.objects[FileList.indexof(filepath)]);
  fs:=TFileStream.Create(archivefile.text,fmOpenRead or fmShareExclusive); { just want to READ it... }
  try
     fs.read(aheader,sizeof(aheader));  { quick way to initialize archive header }
     fs.seek(cfinfo.Position,0);        { start of FILE header within archive }
     fs.read(fheader, sizeof(fheader)); { let's have it }
     fs.seek(fheader.FilenameLength,1); { skip the filename which is stored next }

     { Now the tricky part -- we need to store a valid 3-char compression ID
       in our archive header -- here's the best approach: }

     if fheader.compressedmode<>coNone then { coNone won't GET a header... }
     begin
       case fheader.compressedmode of
          coLZH: cmode := 'LZH';
          coRLE: cmode := 'RLE';
          coCustom: cmode := 'CUS'; { CHANGE this if you use a different ID! }
       end;
       with aheader do  { ComID is already set by the read we did }
       begin
          Fullsize:=fheader.FullSize;
          ArchiveType:=caSingle;
          CheckSum := fheader.checksum;
          Move(cmode[1],ComMethodID,sizeof(ComMethodID)); { get precisely the bytes we want... }
       end;
       bs.write(aheader,sizeof(aheader)); { set up the header }
     end;
     bs.copyFrom(fs,Fheader.compressedsize); { now get the raw (compressed) data! }
     bs.truncate; { in case it started out larger... }
  finally
     fs.free
  end;
end;

{ Examples of setting/loading/shifting image blobs using the above routine }
procedure TForm1.CDBImage1DragDrop(Sender, Source: TObject; X, Y: Integer);
var filepath: String;
     cbs: TCBlobStream; { for loading image from an archived file }
begin
   if Source=Sender then exit; { nowt to do }
   if (Sender is TCDBImage) and (not Table1.active) then
   begin
     showmessage('Can''t do this unless table has been opened...');
     exit;
   end;

  Screen.Cursor := crHourGlass;
  if (Source is TImage) and (Sender is TCDBImage) then
     CDBImage1.picture.bitmap.Assign(Image1.Picture.bitmap)
  else if (Source is TCDBImage) and (Sender is TImage) then
     Image1.picture.bitmap.Assign(CDBImage1.Picture.Bitmap)
  else
  begin   { Have we got an image? }
     filepath := '';
     if (Source is TListBox) and (Listbox1.selcount = 1) then
      filepath:=ListBox1.Items[Listbox1.ItemIndex] { archive list }
     else if (Source is TFileListBox) and (FL.selcount=1) then
        filepath:=FL.Items[FL.ItemIndex]; { file list }
     if ExtractFileExt(filepath)<>'.bmp' then
        showmessage('Must be a .BMP file...')
     else                                     { ok, here we go... }
     if Source is TFileListBox then { just load from file... }
       if Sender is TImage then
          Image1.Picture.Bitmap.LoadFromfile(filepath)
       else
          CDBImage1.Picture.Bitmap.LoadFromFile(filepath)
     else { source must be our archive file... }
     begin
       if Sender is TImage then
           { Fastest way is using LoadArchivedFileToMemory approach per ARC2MEM.PAS }
       else
       begin { here is where we use our direct access routine... }
         cbs:=TCBlobStream.Create(CDBImage1.CField,bmWrite);      { we ARE going to update but... }
         try
           LoadArchivedFileToBlobstream(cbs.Blobstream,filepath); { NOT via expansion/compression }
         finally  { See the HELP notes on the Blobstream property for caveats }
           cbs.free
         end;
       end
     end;
  end;
  if Table1.active and Table1.Modified then Table1.post; { save immediately if updated }
  if not Image1.Picture.Bitmap.Empty then Memo1.visible := False; { got a piccy showing... }
  Screen.Cursor := crDefault;
end;

