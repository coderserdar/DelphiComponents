(*
  CompDemo for TCompress Component Set

  Changes from previous version:
    Delphi 6 ready

  See Thread-safe code example in CompressFilesViaThread

  Note: This demo is currently saved in 32-bit (Delphi 2 or later) format
  -- see the notes below for minor adaptations required to compile it
  with Delphi 1.0.

  You are free to amend, adjust, improve, update, borrow, alter and play
  with this demonstration program at will.

  However, if you redistribute the unregistered TCompress components, please be
  sure to include ALL the files that came with it (incl. Compress.hlp, Readme.txt
  and the ORIGINAL COMPDEMO source).  Thanks.

  Hint: To find the code which makes use of the TCompress components, search
  for Compress1, CDBImage1 and CDBMemo1 references...  At some point, you may
  also want to modify this demo to play with the Key, TargetPath and
  MakeDirectories properties of the TCompress component (all new in V2.5), or
  to experiment with the CompressStreamToArchive method (new in V3.0) of which
  a sample is given in SaveDirectToArchive.

  USING THIS DEMO with Delphi V1.0:
  1. Copy COMPDEMO.DPR, COMPMAIN.PAS and COMPMAIN.NFM to a new directory
  2. Load Delphi 1.0, install Compress/Compctrl and load the new project
  3. Ignore errors about duplicated components and Blobtype properties (not in Delphi 1.0)
  4. In the CheckFile event handler, change the filepath type from string
     (Delphi 2.0+) to OpenString (Delphi 1.0). Don't forget to do this in
    the method declaration as well as its implementation.
  5. Compile and go.  Be aware that you may need to add special filename
     handling in Checkfile to deal with any archives already compressed with
     looong (Win95+) filenames in them. Basically, just truncate to
     a suitable 8.3 format name.

  Enjoy.
*)

unit Compmain;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, DB, DBTables, DBCtrls, ExtCtrls, Buttons, FileCtrl, Mask,
  Compress, CompCtrl
{$IFDEF CLR}
  , System.ComponentModel, System.Threading
{$ENDIF}
  ;


type
  TForm1 = class(TForm)
    Panel2: TPanel;
    Shape1: TShape;
    DBText1: TDBText;
    Image1: TImage;
    Memo1: TMemo;
    Memo2: TMemo;
    DBNavigator1: TDBNavigator;
    CMethod: TRadioGroup;
    GroupBox1: TGroupBox;
    FL: TFileListBox;
    DL: TDirectoryListBox;
    DCB: TDriveComboBox;
    Memo3: TMemo;
    ArchiveGroup: TGroupBox;
    ArchiveLabel: TLabel;
    Label2: TLabel;
    archivefile: TEdit;
    ListBox1: TListBox;
    Memo4: TMemo;
    Fishname: TDBEdit;
    Memo5: TMemo;
    Memo6: TMemo;
    Button1: TButton;
    Panel1: TPanel;
    Bevel1: TBevel;
    Time: TLabel;
    Percentage: TLabel;
    TimeLabel: TLabel;
    Label7: TLabel;
    Trashcan: TImage;
    Button2: TButton;
    CDBImage1: TCDBImage;
    CDBMemo1: TCDBMemo;
    Button3: TButton;
    Table1: TTable;
    Table1SpeciesNo: TFloatField;
    Table1Category: TStringField;
    Table1Common_Name: TStringField;
    Table1SpeciesName: TStringField;
    Table1Lengthcm: TFloatField;
    Table1Length_In: TFloatField;
    CDBImage1Graphic: TCGraphicField;
    CDBMemo1Notes: TCMemoField;
    DataSource1: TDataSource;
    Compress1: TCompress;
    procedure CompressOneFile(var fname: String);
    procedure ResetFileInfo;
    function GetDir: string;
    function GetDummyFilename(generatefrom: string; ext: string): string;
    procedure handleDropField(Source: TObject; archivetoo: Boolean);
    procedure SaveDirectToArchive(Source: TField; filename: string);
    procedure CompressFiles;
    function getCompressionMethod: TCompressionMethod;
    procedure showInfo(comp: TCompress);
    procedure FormCreate(Sender: TObject);
    procedure showfiles;
    procedure ExpandDelete(Operation: TCProcessMode; All: Boolean);
    procedure archivefileChange(Sender: TObject);
    procedure CMethodClick(Sender: TObject);
    procedure DLDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure CDBImage1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure CDBImage1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CDBMemo1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure CDBMemo1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CDBImage1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CDBMemo1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure archivefileDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure archivefileDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DLDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TrashcanDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Table1AfterPost(DataSet: TDataset);
    procedure Button1Click(Sender: TObject);
    procedure FLClick(Sender: TObject);
    procedure Compress1CheckFile(var filepath: String;
      mode: TCProcessMode);
    procedure Panel1Click(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
    procedure TrashcanDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Compress1ShowProgress(var PercentageDone: Longint);
    procedure Button3Click(Sender: TObject);
    procedure disabledragMode;
    procedure enabledragMode;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.NFM}

var FileList: TStringList; { holds information about our archive files }
    saveCompressionMethod: Integer; { see ListBox1.click }

const ShowFileInfoColor :Tcolor = clGray; { see Listbox1.click }

{ Example of accessing the TCompress performance properties }
procedure Tform1.showinfo(comp: TCompress);
begin
   ResetFileInfo;
   Time.caption:=Format('%-5.1fsecs',[Comp.CompressionTime/1000.0]{[f]});
   Percentage.caption:=IntToStr(Comp.CompressedPercentage)+'%';
end;

{ Example of a progress event (new in TCompress 2.0) }
procedure TForm1.Compress1ShowProgress(var PercentageDone: Longint);
begin
   Percentage.caption:=IntToStr(PercentageDone)+'%';
   Application.ProcessMessages;
  { you may have *other* uses for this every-8K-read event...  In fact, in V2.5
    if you set PercentageDone to -1, it will cause compression to end at the
    point reached. If so, delete from the archive the compressed file
    which was created before the abort  }
end;

{ Example of getting a list of files in a multi-file archive }
procedure TForm1.showfiles;
begin
  listbox1.clear;
  Compress1.FreeFileList(FileList); { clear list and free any file information objects in it }
  if not FileExists(archivefile.Text) then exit;
  Compress1.ScanCompressedFile(ArchiveFile.Text,Filelist);
  ListBox1.Items.addStrings(FileList); { and File info objects are
                            there too -- see ListBox1Click and FormDestroy }
end;

{ Example of expanding/deleting one or more files from a multi-file archive }
procedure TForm1.ExpandDelete(Operation: TCProcessMode; All: Boolean);
var s: Tstringlist;
  count: Integer;
begin
  if (All and (Listbox1.Items.count > 0)) or (Listbox1.selcount>0) then { something is... }
  begin
     s:=Tstringlist.create;
     try
        if All then
           s.addStrings(ListBox1.Items)
        else
           for count :=0 to Listbox1.ITems.count-1 do
            if Listbox1.selected[count] then
              s.add(Listbox1.items[count]);
        if Operation=cmExpand then { expand }
          compress1.expandfiles(ArchiveFile.Text,s)
        else
          compress1.deletefiles(ArchiveFile.Text,s);
        showinfo(Compress1);
        showfiles; { also clears selections... }
     finally
        s.free;
        Screen.Cursor := crDefault;
     end;
  end;
end;

{ Example of compressing a SINGLE file into an archive }
procedure TForm1.CompressOneFile(var fname: String);
begin
  disableDragMode;
  try
    Compress1.CompressFile(ArchiveFile.Text,fname,getCompressionMethod);
    showInfo(Compress1);
    showfiles;
  finally
    Screen.Cursor := crDefault;
    enableDragMode;
  end;
  SysUtils.DeleteFile(fname); { because for this example we're creating TEMP files only... }
end;


{ Example of compressing MULTIPLE files into an archive }
{ V4.0: For CompressFiles(only) we're also showing how
  to create a thread (and a new instance of the component) to do the
  addition.
}

procedure CompressFilesViaThread;
var s: Tstringlist;
    Count: Integer;
    tc: TCompress;
begin
  with Form1 do
  if FL.selcount>0 then { something is... }
  begin
    s:=TStringlist.Create;
    try
      disableDragMode;
      for count :=0 to FL.Items.count-1 do
        if FL.selected[count] then
          s.add(FL.items[count]);
       tc := TCompress.create(nil);
       with tc do
       begin
         RegName := Compress1.RegName; { in case you've set these }
         RegNumber := Compress1.RegNumber;
         Key := Compress1.Key;
         OnShowProgress := Compress1.OnShowProgress;
         CompressFiles(ArchiveFile.Text,s,getCompressionMethod);
         showInfo(tc);
         free;
       end;
       showfiles;
    finally;
       s.free;
       Screen.Cursor := crDefault;
       enableDragMode;
    end;
  end;
end;

procedure TForm1.CompressFiles;
var
{$IFDEF CLR}
  aThread: System.Threading.Thread;
{$ELSE}
  threadid: Cardinal;
{$ENDIF}
begin
{$IFDEF CLR}
  aThread := System.Threading.Thread.Create(CompressFilesViaThread);
  aThread.Start;
{$ELSE}
  IsMultiThread := true;
  CreateThread(nil, 8192, @CompressFilesViaThread, nil, 0, threadID);
{$ENDIF}
end;

{ Examples of setting/loading/shifting image blobs }
procedure TForm1.CDBImage1DragDrop(Sender, Source: TObject; X, Y: Integer);
var filepath: String;
    isCenterImage: Boolean;
begin
   if Source=Sender then exit; { nowt to do }
   isCenterImage := (Sender=Image1) or (Sender=Memo1);
   if (Sender is TCDBImage) and (not Table1.active) then
   begin
     showmessage('Can''t do this unless table has been opened...');
     exit;
   end;

  Screen.Cursor:= crHourGlass;
  if (Source = Image1) and (Sender is TCDBImage) then
  begin
     Table1.edit;
     CDBImage1.picture.bitmap.Assign(Image1.Picture.bitmap)
  end
  else if (Source is TCDBImage) and isCenterImage then
     Image1.picture.bitmap.Assign(CDBImage1.Picture.Bitmap)
  else
  begin   { Have we got an image? }
     filepath := '';
     if (Source is TListBox) and (Listbox1.selcount = 1) then
      filepath:=ListBox1.Items[Listbox1.ItemIndex] { archive list }
     else if (Source is TFileListBox) and (FL.selcount=1) then
        filepath:=FL.Items[FL.ItemIndex]; { file list }
     if LowerCase(ExtractFileExt(filepath))<>'.bmp' then
     begin
        MessageBeep(1);
        showmessage('Must be a .BMP file...')
     end else begin                             { ok, here we go... }
        if Source is TListBox then { must first extract file... }
        begin { Note: see ARC2BLOB.PAS and ARC2MEM.PAS for three FASTER ways
                        of going about this (no expanded file needed) }
          try
            Compress1.ExpandFile(filepath,ArchiveFile.Text);
          finally
            Screen.cursor := crDefault; { as our OnCheckFile sets it on }
          end;
          if filepath='' then exit; { was skipped on confirmation }
        end;
        Screen.Cursor:= crHourGlass;
        if isCenterImage then
           Image1.Picture.Bitmap.LoadFromfile(filepath)
        else begin
           Table1.edit;
           CDBImage1.Picture.Bitmap.LoadFromFile(filepath);
        end
     end; { else }
  end;
  if not Image1.Picture.Bitmap.Empty then
  begin
   Memo1.visible := False; { got a piccy showing... }
   image1.visible := True;
  end;
  Screen.Cursor:= crDefault;
end;

{ Examples of setting/loading/shifting CDBMemo blobs }
procedure TForm1.CDBMemo1DragDrop(Sender, Source: TObject; X, Y: Integer);
var filepath: String;
begin
  if Source=Sender then exit; { nowt to do }
  filepath := ''; { in case fails }
  if (Source is TListBox) and (Listbox1.selcount = 1) then
   filepath:=ListBox1.Items[Listbox1.ItemIndex] { archive list }
  else if (Source is TFileListBox) and (FL.selcount=1) then
     filepath:=FL.Items[FL.ItemIndex]; { file list }
  if LowerCase(ExtractFileExt(filepath))<>'.txt' then
  begin
    MessageBeep(1);
    showmessage('Must be a .TXT file...')
  end else begin                             { ok, here we go... }
    if Source is TListBox then { must first extract file... }
    begin { Note: see ARC2BLOB.PAS and ARC2MEM.PAS for three FASTER ways
                       of going about this (no expanded file needed) }
      try
        Compress1.ExpandFile(filepath,ArchiveFile.Text);
      finally
        Screen.cursor := crDefault; { as our OnCheckFile sets it on }
      end;
      if filepath='' then exit; { was skipped on confirmation }
    end;
    Screen.Cursor:= crHourGlass;
    Table1.edit;
    CDBMemo1.Lines.LoadfromFile(filepath)
  end;
  Screen.Cursor:= crDefault;
end;

procedure TForm1.CDBMemo1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  accept := (Source is TFileListBox) or (Source is TListBox) or (Source is TCDBMemo);
end;

procedure TForm1.CDBImage1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  accept := (Source=Image1) or (Source is TCDBImage) or
     (Source is TFileListBox) or (Source is TListBox);
end;

{ Refreshing a CDBImage so it will be compressed (assuming previously uncompressed) }
procedure TForm1.CDBImage1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then { ok, refresh our field }
  begin
     CDBImage1.CopyToClipBoard;
     CDBImage1.PasteFromClipBoard;
     Table1.post;
  end;
end;

procedure TForm1.CDBMemo1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then { ok, refresh our field }
  begin
     CDBMemo1.Lines[0]:=CDBMemo1.Lines[0]; { setting .Modified doesn't do it... }
     Table1.post;
  end;

end;

procedure TForm1.CMethodClick(Sender: TObject);
begin
  CDBIMage1.CompressionMethod := getCompressionMethod;
  CDBMemo1.CompressionMethod := getCompressionMethod;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 Application.HelpFile:='COMPRESS.HLP';
 fileList := TStringList.create; { keeps track of our archive files for display etc. }
 SendMessage(ListBox1.handle,LB_SetHorizontalExtent,300,LongInt(0));
 saveCompressionMethod := -1; { see Listbox1.click }
 showfiles; { show files in archive (if any)... }
{$IFNDEF CLR}
 try
 {$IFDEF VER140}
   DL.Directory := '\Program Files\Borland\Delphi6\IMAGES\BACKGRND';
 {$ENDIF}
 {$IFDEF VER150}
   DL.Directory := '\Program Files\Borland\Delphi7\IMAGES\BACKGRND';
 {$ENDIF}
 except on EInOutError do ; { nowt, let it default }
 end;
{$ENDIF}

 try Table1.Active := True;
     DataSource1.Edit;
 except
  on EDBEngineError do
     showmessage('The BLOB compression portion of this demonstration'+#13+
                 'requires that the DBDEMOS alias be set up and pointing'+#13+
                 'to the BIOLIFE.DB table in \DELPHI\DEMOS\DATA.'+#13+#13+
                 '-- as this is not currently the case, the BLOB demonstration'+#13+
                 'is disabled.');
  on EUnrecognizedCompressionMethod do
     showmessage('Your BIOLIFE database appears to have been compressed with'+#13+
                 'a custom compression method which cannot be recognised.'+#13+
                 'Please revert to an uncompressed backup of BIOLIFE.*');
 end; {try }

 if not Table1.Active then { something went wrong... }
 begin
     CDBImage1.visible:=False;
     CDBMemo1.visible:=False;
     DBNavigator1.visible:=False;
     Memo1.visible:=False;
     Memo2.visible := True;
 end;
 CMethodClick(self);  { get default compression for our database controls }

end;

function TForm1.GetDir: string; { called below and in GetDummyFileName }
begin
  Result := DL.Directory;
  if Copy(Result,Length(Result),1)<>'\' then { not already \'d? }
    Result := Result+'\';
end;

procedure TForm1.archivefileChange(Sender: TObject);
begin
  showfiles;
end;

function TForm1.getCompressionMethod: TCompressionMethod;
begin
   result := coNone; { default }
   case CMethod.ItemIndex of
     1: result := coRLE;
     2: result := coLZH;
     3: result := coLZH5;
   end;
end;

procedure TForm1.DLDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  accept := True;
  if ((Sender is TDirectoryListBox) and (Source is TFileListBox)) or
     (Source=Trashcan) then
        accept := False; { fair enough? }
end;

procedure TForm1.archivefileDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  accept := True; { but... }
  if ((Source is TGroupBox) and not (Sender is TGroupBox)) or
         (((Sender is TEdit)or (Sender is TGroupBox)) and (Source is TListBox)) or { not from our OWN list }
           (Source=Trashcan) then
     accept := False;
end;

{ Used to create 'work' filenames for saving images and memos
  from the database into our archive or to disk... }
function TForm1.GetDummyFilename(generatefrom: string; ext: string): string;
begin
  if (generatefrom='Image') or (generateFrom='') then
     generatefrom:='image';
  result := Getdir+generatefrom+'.'+ext;
end;

function Confirmfilename(filename: String; archiving: Boolean): Boolean;
begin
  Result := True; { default for archiving }
  if (not Archiving) and
     (MessageDlg('Save to '+filename+'?', mtConfirmation,[mbYes, mbNo], 0)<>id_Yes) then
     Result := False;
end;

{ The handler for dropping things on the file list or archive list }
procedure TForm1.handleDropField(Source: TObject; archivetoo: Boolean);
var filename: String;
begin
  filename := ''; { in case it is NOT one of those below... }
  if Source is TCDBMemo then
  begin
     filename := GetDummyFilename(Fishname.Text,'TXT');
     if not confirmFilename(filename,archivetoo) then exit;
     if ArchiveToo then { V3.0 -- save directly into archive -- no temp file }
     begin
       SaveDirectToArchive((Source as TCDBMemo).Field,filename);
       exit;
     end else
       CDBMemo1.Lines.SaveToFile(filename); { save to directory }
  end else if Source is TCDBImage then
  begin
     filename := GetDummyFilename(Fishname.Text,'BMP');
     if not confirmFilename(filename,Archivetoo) then exit;
     if ArchiveToo then { V3.0 -- save directly into archive -- no temp file }
     begin
       SaveDirectToArchive((Source as TCDBImage).Field,filename);
       exit;
     end else
        CDBImage1.Picture.Bitmap.SaveToFile(filename); { save to directory }
  end
  else
   if Source = Image1 then
  begin
     filename := GetDummyFilename('Image','BMP');
     if not confirmFilename(filename,Archivetoo) then exit;
     Image1.Picture.Bitmap.SaveToFile(filename);
  end;
  if (filename<>'') and (ArchiveToo) then
      CompressOneFile(filename);
end;

{ new in V3.0, this routine APPENDS a blob to the archive, after first making
  sure something of the same name is not already there. While this is fast,
  in a working situation it would be tidier with a DeleteFiles call to remove
  any prior copy of the blob first...
}
procedure TForm1.SaveDirectToArchive(Source: TField; filename: string);
var bs: TCBlobstream; { for compressing into the archive: may need to auto-EXPAND first, hence TCBlobstream... }
begin
  filename :=ExtractFileName(filename);
  if FileList.Indexof(filename) >=0 then
  begin
    showmessage(filename+' is already in the archive -- please delete it first');
    exit; { to automate the deletion, we could just use the Compress1.DeleteFiles method }
  end;
  bs := TCBlobstream.Create(Source as TCBlobField,bmRead); { we're going to read the (expanded) field contents) }
  try
     if Source is TCGraphicField then { sorry about this, but we have to skip a graphic header which Delphi stores }
        bs.seek(8,soFromBeginning);  { in blob bitmaps, but which DON'T belong in BMP files -- this very hardwired
                                     code assumes it is there, and skips it }

     Screen.cursor := crHourGlass;
     disableDragMode;
     Compress1.CompressStreamToArchive(ArchiveFile.Text,bs, { and append/compress them to the archive... }
                                      filename,getCompressionMethod);
  finally
     enableDragMode;
     Screen.cursor := crDefault;
     bs.free;
  end;
  showinfo(Compress1);
  showfiles;
end;

procedure TForm1.archivefileDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if Source is TFileListBox then
     CompressFiles
  else
    HandleDropField(Source, True); { save to temp file AND archive... }
end;

procedure TForm1.DLDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source=Sender then exit; { seems reasonable, and IS necessary }
  if Source is TListBox then
    ExpandDelete(cmExpand,False) { selected archive files }
  else if Source=ArchiveGroup then
     ExpandDelete(cmExpand,True) { all archived files }
  else
    HandleDropField(Source, False); { save field to a file }
  FL.Update; { get up to date... }
end;
procedure TForm1.TrashcanDragDrop(Sender, Source: TObject; X, Y: Integer);
var count: Integer;
    tempBitmap: TBitMap; { just to get an empty one }
begin
  if Source is TListBox then
    ExpandDelete(cmDelete,False)
  else if Source=ArchiveGroup then
     ExpandDelete(cmDelete,True) { all files }
     { and strictly speaking, should now delete the archive if it is
       empty, but I'll leave that as an exercise... }
  else if Source is TFileListBox then { delete some or all... }
  begin
     for count:=0 to FL.Items.count-1 do
        if FL.selected[count] and
           (MessageDlg('Delete '+GetDir+FL.Items[count],mtConfirmation,[mbYes,mbNo],0)=id_Yes) then
           SysUtils.DeleteFile(GetDir+FL.Items[count]);
     FL.Update;
  end
  else if (Source is TCDBMemo) and
              (MessageDlg('Cut to clipboard?',mtConfirmation,[mbYes,mbNo],0)=id_Yes) then
  begin
     CDBMemo1.SelectAll;
     CDBMemo1.cutToClipboard { safer than .clear, for demo purposes }
  end
  else if (Source is TCDBImage) and
            (MessageDlg('Cut to clipboard?',mtConfirmation,[mbYes,mbNo],0)=id_Yes) then
      CDBImage1.cutToClipboard { not quite a delete, but just for example... }
  else if Source=Image1 then
  begin
     tempBitMap := TBitMap.Create;
     try
        Image1.Picture.Bitmap.Assign(tempBitMap);
        Image1.visible := False;
        Memo1.visible := True
     finally
        tempBitMap.free;
     end;
  end;


end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Compress1.FreeFileList(FileList); { free list and any file information objects in it }
  FileList.free;
end;


procedure TForm1.ListBox1Click(Sender: TObject);
var cfinfo: TCompressedFileInfo;
    compmethod, percentageval: Integer;
begin
  if listBox1.ItemIndex >=0 then
  begin
     CMethod.Color := ShowFileInfoColor; { make it clear we are showing off a bit... }
     Percentage.Color := ShowFileInfoColor;
     Time.Color := ShowFileInfoColor;
     TimeLabel.Caption := 'Full Size:';

     cfinfo:=TCompressedFileinfo(FileList.objects[listBox1.ItemIndex]); { how to get at the other stuff... }
     if cfinfo.Fullsize>0 then
     begin
       if cfinfo.Fullsize>100000 then { makes safe for files >20Mb actually }
         Percentageval := cfinfo.CompressedSize div (cfinfo.Fullsize div 100)
       else
         Percentageval := 100*cfinfo.CompressedSize div cfinfo.Fullsize;
       Percentage.caption:=IntToStr(100-percentageval)+'%'
     end else
       Percentage.caption:='(empty)';
     if cfinfo.locked then
        Percentage.caption := Percentage.caption + ' (locked)';
     Time.caption:= IntToStr((512+cfinfo.Fullsize) div 1024)+' Kb';
     if saveCompressionMethod <0 then
        savecompressionMethod :=cMethod.ItemIndex;
     compMethod :=Integer(cfinfo.CompressedMode);
     if compMethod = 4 then
         compMethod := 3; { force LZH5 to show up as the third box }
     cMethod.ItemIndex :=compMethod;
  end;
end;

procedure TForm1.ResetFileInfo;
begin
  if saveCompressionMethod <0 then exit;
  cMethod.ItemIndex:=savecompressionMethod;
  saveCompressionMethod := -1;
  CMethod.Color := clBtnFace;
  Percentage.Color := clWindow;
  Time.Color := clWindow;
  TimeLabel.Caption := 'Time:';
  showInfo(Compress1); { get the right stuff too... }
  Time.Caption:=''; { but this is meaningless at this point... }
end;


procedure TForm1.Table1AfterPost(DataSet: TDataset);
begin
  Showinfo(Compress1);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Drag && Drop at will: compression/expansion'+#13+
  'is automatic.'+#13+#13+
  'Uses TCompress, TCDBMemo and TCDBImage.'+#13+#13+
  'Component Registration and License: $NZ149(about $US59)'+#13+
  'See registration form in Help or:'+#13+
  'Fax +64-3-384-5138   Email: software@spis.co.nz');
end;

procedure TForm1.FLClick(Sender: TObject);
begin
  ResetFileInfo;
end;

{ Example of OnCheckFile user interface handling routine
  Note that the V2.5 TargetPath property frequently obviates the need
  for any Expand handler, but we've kept it anyway for your
  info. Also, you could Set the MakeDirectories property if
  the target path's should be created if required.
}
procedure TForm1.Compress1CheckFile(var filepath: String;
  mode: TCProcessMode);
var modestr: String;
  dlg: Integer;
begin
  case mode of
     cmExpand: begin
                 modestr := 'Expand';
                 filepath:=Getdir+extractfilename(filepath); { go where we should }
               end;
     cmCompress: begin
                    modestr := 'Compress';
                    filepath:={Getdir+}extractfilename(filepath); { use GetDir if you want full path... }
                 end;
     cmDelete: modestr := 'Delete';
  end;
  showInfo(Compress1);
  Screen.cursor := crDefault; { in case this is second call in a sequence }
  dlg := MessageDlg(modestr+' '+filepath+'?', mtConfirmation,[mbYes, mbNo, mbCancel], 0);
  case dlg of
     id_No: filepath :=CompressSkipFlag; { flag 'not this one'}
     id_Cancel: filepath :=CompressNoMoreFlag; { flag 'no more!' }
     id_Yes: Screen.Cursor := crHourGlass; { for operation itself }
  end;
end;


procedure TForm1.Panel1Click(Sender: TObject);
begin
ResetFileInfo;
end;

procedure TForm1.FormClick(Sender: TObject);
begin
ResetFileInfo;
end;

procedure TForm1.GroupBox1Click(Sender: TObject);
begin
ResetFileInfo;
end;

procedure TForm1.TrashcanDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  accept := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
Application.HelpJump('1050');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
Application.HelpJump('1030');
end;

{ V3.03 -- disable dragging temporarily while compression
  is in progress, because otherwise it is *possible* (tho unlikely)
  to request a second compression before the first has finished,
  i.e. code is made non-re-entrant via the user interface.
  Not a problem with threads, just the fact that trying
  to add two files to the same archive at the same time is bad karma!
}
procedure TForm1.disableDragMode;
begin
  Fl.dragMode := dmManual;
  CDBMemo1.dragMode := dmManual;
  CDBImage1.dragMode := dmManual;
  ArchiveGroup.dragMode := dmManual;
  ListBox1.dragMode := dmManual;
end;

procedure TForm1.enableDragMode;
begin
  Fl.dragMode := dmAutomatic;
  CDBMemo1.dragMode := dmAutomatic;
  CDBImage1.dragMode := dmAutomatic;
  ArchiveGroup.dragMode := dmAutomatic;
  ListBox1.dragMode := dmAutomatic;
end;

end.
