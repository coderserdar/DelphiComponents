(* ARC2MEM.PAS for TCompress V3.0 (no change from V2.5 except para 5 in comments)

This code is designed to be dropped into the COMPDEMO application, adding
one new routine (LoadArchivedFileToMemory) and replacing two existing ones
(CDBImage1DragDrop and CDBMemo1DragDrop).

It gives an idea of a more efficient way to load compressed data from
a file archive to a blob field, without using an intermediate file as
COMPDEMO currently does.

However, it still decompresses the data on the way, so the most efficient
approach is that shown ARC2BLOB.PAS which transfers the UNcompressed data
as-is to a target compressed blob.

Keep in mind that both this and the ARC2BLOB code require manipulation of some
of the TCompress data structures...

** See the comment in ARC2BLOB.PAS about the new TCompress 3.0
ExpandStreamFromArchive routine, which makes this kind of task simpler. This
code is effectively rendered obsolete by the new routine, but we've kept it
as an example of direct archive manipulation.
*)


{ Example of Expanding a file DIRECTLY from an archive to another stream }
procedure TForm1.LoadArchivedFileToMemory(var mem:Tmemorystream;filepath:String);
var fs: TFilestream; { here we go... }
    cfinfo: TCompressedFileInfo;
    fheader: TCompressedFileHeader;
begin
  cfinfo := TCompressedFileInfo(FileList.objects[FileList.indexof(filepath)]);
  fs:=TFileStream.Create(archivefile.text,fmOpenRead or fmShareExclusive); { just want to READ it... }
  try
     fs.seek(cfinfo.Position,0);        { find the file info start }
     fs.read(fheader,sizeof(fheader));  { read the header }
     fs.seek(fheader.filenameLength,1); { and skip the filename -- now at compressed data start }
     mem.SetSize(cfinfo.FullSize);      { pre-set the size for fastest/cleanest results }
     Compress1.DoExpand(mem,fs,cfinfo.CompressedSize,cfinfo.Fullsize, cfinfo.Checksum,
                 cfinfo.CompressedMode,'',cfInfo.Locked); { Empty CompressID added v3.5 }
  finally
     fs.free
  end;
end;

{ Examples of setting/loading/shifting image blobs using the above routine }
procedure TForm1.CDBImage1DragDrop(Sender, Source: TObject; X, Y: Integer);
var filepath: String;
     mem: TMemoryStream; { for loading image from an archived file }
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
           mem:= TMemoryStream.create;
           try
             LoadArchivedFileToMemory(mem,filepath);
             mem.seek(0,0);
             if Sender is TImage then
               Image1.Picture.Bitmap.LoadFromStream(mem)
             else
               CDBImage1.Picture.Bitmap.LoadFromStream(mem);
           finally
              mem.free
           end;
        end;
  end;
  if Table1.active and (Table1.State in [dsEdit]) then Table1.post; { save immediately if updated }
  if not Image1.Picture.Bitmap.Empty then Memo1.visible := False; { got a piccy showing... }
  Screen.Cursor := crDefault;
end;

{ Examples of setting/loading/shifting Memo blobs using LoadArchivedFileToMemory }
procedure TForm1.CDBMemo1DragDrop(Sender, Source: TObject; X, Y: Integer);
var filepath: String;
     mem: TMemoryStream; { for loading text from an archived file }
begin
  filepath := ''; { in case fails }
  if (Source is TListBox) and (Listbox1.selcount = 1) then
   filepath:=ListBox1.Items[Listbox1.ItemIndex] { archive list }
  else if (Source is TFileListBox) and (FL.selcount=1) then
     filepath:=FL.Items[FL.ItemIndex]; { file list }
  if ExtractFileExt(filepath)<>'.txt' then
     showmessage('Must be a .TXT file...')
  else begin                     { ok, here we go... }
    Screen.Cursor := crHourGlass;
   if Source is TFileListBox then
     CDBMemo1.Lines.LoadfromFile(filepath)
   else
   begin
     mem:= TMemoryStream.create;
     try
       LoadArchivedFileToMemory(mem,filepath);
       mem.seek(0,0);
       CDBMemo1.Lines.LoadfromStream(mem)
     finally
        mem.free
     end;
   end;
  end;
  if Table1.active and (Table1.State in [dsEdit]) then Table1.post; { save immediately if updated }
  Screen.Cursor := crDefault;
end;
