unit LHAmain;
{ LHADemo for TCompLHA
  
  Changes from previous version:
    D6 requirements such as counter instead of count in most loops

  Note: Before compiling this with Delphi 1.0,
  change the following line in the TForm1 declaration:
    procedure CompLHA1CheckFile(const originalFilePath: string; var newFilePath: String;
  to:
    procedure CompLHA1CheckFile(const originalFilePath: string; var newFilePath: OPENstring;

Notes:
   1. We've set CompLHA1's TempArchivePath to c:\temp\ -- it will use it if there
      is one, and do without it (silently) if not. See help for more information.
   2. If you want to test wild card expand/verify/compress, search for 'WILD' and
      follow the instructions there.
   3. See CompressFileViaThread for an example of how to create a new thread
      and safely have it use an independent instance of the component.
}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, CompLHA, StdCtrls,
  ExtCtrls, Buttons, FileCtrl, Mask;

type
  TForm1 = class(TForm)
    ArchiveGroup: TGroupBox;
    ListBox1: TListBox;
    Memo4: TMemo;
    Status: TPanel;
    GroupBox2: TGroupBox;
    DL: TDirectoryListBox;
    FL: TFileListBox;
    Panel3: TPanel;
    DCB: TDriveComboBox;
    Memo3: TMemo;
    Panel2: TPanel;
    ArchiveLabel: TLabel;
    archivefile: TEdit;
    Label2: TLabel;
    CompLHA1: TCompLHA;
    Panel1: TPanel;
    Panel4: TPanel;
    Bevel1: TBevel;
    Time: TLabel;
    Percentage: TLabel;
    TimeLabel: TLabel;
    Label7: TLabel;
    Trashcan: TImage;
    CMethod: TRadioGroup;
    SaveFullPath: TCheckBox;
    ConfirmOperations: TCheckBox;
    ArchiveHeader: TRadioGroup;
    AllowDuplicates: TCheckBox;
    VerifyExpands: TCheckBox;
    HelpButton: TButton;
    AboutButton: TButton;
    RegistrationButton: TButton;
    Mask: TComboBox;
    AbortButton: TSpeedButton;
    Edit1: TEdit;
    Key: TImage;
    procedure ResetFileInfo;
    procedure SetDir(const path: string);
    function GetDir: string;
    procedure CompressFiles;
    function getCompressionMethod: TCompLHAMethod;
    procedure showInfo(tc: TCompLHA);
    procedure FormCreate(Sender: TObject);
    procedure showfiles;
    procedure ExpandDelete(Operation: TCompLHAProcessMode; All: Boolean);
    procedure archivefileChange(Sender: TObject);
    procedure DLDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure archivefileDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure archivefileDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DLDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TrashcanDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListBox1Click(Sender: TObject);
    procedure FLClick(Sender: TObject);
{ before compiling with Delphi 1.0, change newFilePath: string;
  to newFilePath: OPENString; in the following line: }
    procedure CompLHA1CheckFile(const originalFilePath: string; var newFilePath: string;
              DateTime: Longint; mode: TCompLHAProcessMode);
    procedure TrashcanDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure CompLHA1ShowProgress(var PercentageDone: Longint);
    procedure Panel2Resize(Sender: TObject);
    procedure SaveFullPathClick(Sender: TObject);
    procedure CMethodClick(Sender: TObject);
    procedure ConfirmOperationsClick(Sender: TObject);
    procedure ArchiveHeaderClick(Sender: TObject);
    procedure CMethodEnter(Sender: TObject);
    procedure AllowDuplicatesClick(Sender: TObject);
    procedure CompLHA1AfterProcessed(const originalFilePath, newFilePath: string;
      dateTime: Longint; mode: TCompLHAProcessMode);
    procedure AboutButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure RegistrationButtonClick(Sender: TObject);
    procedure disableDragMode;
    procedure enableDragMode;
    procedure ExpandButtonClick(Sender: TObject);
    procedure MaskChange(Sender: TObject);
    procedure FillFileList;
    procedure AbortButtonClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{$J+} { allow typed constants -- directive required in D6 }
const ShowFileInfoColor :Tcolor = clGray; { see Listbox1.click }
      saveCompressionMethod: Integer = -1; { see ListBox1.click }
      saveArchiveHeader: Integer = -1;
      SingleFileBeingExpanded : Integer = -1; { just to demonstrate datetime checking
                                                 in an "AllowDuplicates" archive... }

{ Example of getting a list of files in an archive }
procedure TForm1.showfiles;
begin
  CompLHA1.Scan; { ArchiveName property is assumed set }
  FillFileList; { get from CompLHA1.FileList, but apply display mask as well }
{  ListBox1.Items := CompLHA1.FileList; }
  { and FileInformation objects are there too -- see ListBox1Click }
end;

{ get from CompLHA1.FileList, but apply display mask as well }
procedure TForm1.FillFileList;
var counter: Integer;
begin
  with ListBox1, CompLHA1 do
  begin
    clear;
    for counter := 0 to pred(FileList.Count) do
       if MatchFileName(FileList[Counter],Mask.Text) then
        ListBox1.Items.add(FileList[Counter]);
  end;
end;

{ Example of expanding/deleting one or more files from a multi-file archive }
procedure TForm1.ExpandDelete(Operation: TCompLHAProcessMode; All: Boolean);
var count: Integer;
begin
  if (All and (Listbox1.Items.count > 0)) or (Listbox1.selcount>0) then { something is... }
  begin
     CompLHA1.FilesToProcess.clear;
     try
        if All then
           CompLHA1.FilesToProcess := ListBox1.Items
        else
           for count :=0 to Listbox1.ITems.count-1 do
            if Listbox1.selected[count] then
            begin
                CompLHA1.FilesToProcess.add(Listbox1.items[count]);
                SingleFileBeingExpanded := count; { see ONCheckFile for how this is used for DateTime checks }
            end;
        if (CompLHA1.FilesToProcess.count <> 1) or (mask.text<>'*.*') then
          SingleFileBeingExpanded := -1; { overruled if > 1 in this simple example }

        Screen.Cursor := crHourGlass;
        if Operation=cmLHAExpand then { expand }
        begin { check for verify }
          if VerifyExpands.checked then
             CompLHA1.Verify
          else
             CompLHA1.Expand
        end else
          CompLHA1.Delete;
        showinfo(CompLHA1);
        showfiles; { also clears selections... }
        SingleFileBeingExpanded := -1; { so doesn't affect other calls in any way }
     finally
        Screen.Cursor := crDefault;
     end;
  end;
end;

{ Example of compressing MULTIPLE files into an archive. }
{ V4.0: For CompressFiles(only) we're also showing how
  to create a thread (and a new instance of the component) to do the
  addition.
}

procedure CompressFilesViaThread(dummy: pchar);
var Count: Integer;
    fileToAdd: string;
    tc: TCompLHA;
begin
  with Form1 do
  if FL.selcount>0 then { something is selected... }
  begin
   try
     disableDragMode;
     tc := TCompLHA.create(nil);
     with tc do
     begin { first, make sure we match all the UI-derived settings... }
       RegName := CompLHA1.RegName; { in case you've set these }
       RegNumber := CompLHA1.RegNumber;
       Password := CompLHA1.Password;
       OnShowProgress :=CompLHA1.OnShowProgress;
       ArchiveName := CompLHA1.ArchiveName;
       ArchiveType := CompLHA1.ArchiveType;
       StorePath :=CompLHA1.StorePath;
       CompressionMethod := CompLHA1.CompressionMethod;
       Confirm := CompLHA1.Confirm;
       AllowDuplicates := CompLHA1.AllowDuplicates;

       FilesToProcess.Clear;
       for count :=0 to FL.Items.count-1 do
         if FL.selected[count] then
         begin
          fileToadd :=FL.items[count];
          if SaveFullPath.Checked then { must provide a path then... }
          begin
             if copy(DL.directory,Length(DL.Directory),1)<>'\' then
                fileToadd := '\'+FileToadd; { need a preceding \ }
            fileToadd := DL.Directory+FileToadd;
          end;
          FilesToProcess.add(filetoadd);
         end;
       Screen.cursor := crHourGlass;
       disableDragMode;
       Compress;
       showInfo(tc);
       free;
     end; { with tc }
     showfiles;
   finally;
     Screen.Cursor := crDefault;
     enableDragMode;
   end; { try }
  end; { with Form1 }
end;

procedure TForm1.CompressFiles;
var param: pchar;
{ Get correct parameter for CreateThread according to Delphi version, sigh }
{$IFDEF VER90} { Delphi 2}
   {$DEFINE INTEGERTHREADID}
{$ENDIF}
{$IFDEF VER95} { CB1 }
   {$DEFINE INTEGERTHREADID}
{$ENDIF}
{$IFDEF VER100} { Delphi 3}
   {$DEFINE INTEGERTHREADID}
{$ENDIF}
{$IFDEF VER105} { CB2 }
   {$DEFINE INTEGERTHREADID}
{$ENDIF}

{$IFDEF INTEGERTHREADID}  { older compilers }
   threadid: Integer;
{$ELSE}
   threadid: Longword;
{$ENDIF}

begin
{$IFDEF WINDOWS} { Delphi 1.0 -- no threads, just do it }
  CompressFilesViaThread(nil);
{$ELSE}  { 32-bit code }
   IsMultiThread := true;
   param:=nil;
   CreateThread(nil,
      8192,
      @CompressFilesViaThread,
      param,
       0,
       threadID);
{$ENDIF}
end;

{ Example of accessing the TCompLHA performance properties }
procedure Tform1.showinfo(tc: TCompLHA);
begin
   ResetFileInfo;
   Time.caption:=Format('%-5.1fsecs',[tc.CompressionTime/1000.0]{[f]});
   Percentage.caption:=IntToStr(tc.CompressedPercentage)+'%';
end;

{ Example of OnCheckFile event (simple user interface supplement) }
{ before compiling with Delphi 1.0, change newFilePath: string;
  to newFilePath: OPENString; in the following line: }
procedure TForm1.CompLHA1CheckFile(const originalFilePath: string; var newFilePath: string;
           DateTime: Longint; mode: TCompLHAProcessMode);
begin
  case mode of
      cmLHACompress : Status.caption := 'Compressing '+originalFilePath+' as '+newFilePath;
      cmLHADelete: Status.caption := 'Deleting '+originalFilePath;
      cmLHAExpand : begin { let's not expand a single file unless the FileInformationIndex matches
                        the original selection -- useful if archive was made with AllowDuplicates True }
                      if SingleFileBeingExpanded >=0 then { expanding a single file -- check for matching date }
                         if SingleFileBeingExpanded <> CompLHA1.FileInformationIndex then
                         begin { sorry, not THIS copy (must be another copy in an AllowDuplicates archive... }
                           newFilePath := CompLHASkipFlag; { please skip this one }
                           exit;
                         end;
                      if CompLHA1.VerifyMode then { faking it, NOT overwriting original... }
                       Status.caption := 'Verifying expansion of '+originalFilePath+' to '+newFilePath
                      else
                       Status.caption := 'Expanding '+originalFilePath+' to '+newFilePath;
                    end;
  end;
  Status.Update;
end;

{ Example of an OnAfterProcessed event handler }
procedure TForm1.CompLHA1AfterProcessed(const originalFilePath,
  newFilePath: string; dateTime: Longint; mode: TCompLHAProcessMode);
begin
   case mode of
    cmLHACompress : Status.caption := 'Compressed '+originalFilePath+' as '+newFilePath;
    cmLHADelete: Status.caption := 'Deleted '+originalFilePath;
    cmLHAExpand : Status.caption := 'Expanded '+originalFilePath+' as '+newfilepath;
   end;
   status.update; { force display }
end;

{ Example of an OnShowProgress event handler }
procedure TForm1.CompLHA1ShowProgress(var PercentageDone: Longint);
begin
   Percentage.caption:=IntToStr(PercentageDone)+'%';
   Application.ProcessMessages;
  { you may have *other* uses for this every-8K-read event...  In fact, in V2.5
    if you set PercentageDone to -1, it will cause compression to end at the
    point reached. If so, delete from the archive the compressed file
    which was created before the abort  }
end;

{ Example of using the FileInformation property to display file specs from the archive }
procedure TForm1.ListBox1Click(Sender: TObject);
var percentageval: Longint;
    attribstr: string;
    counter, whichFile: Integer;
    WhichFileName: string;
begin
  if listBox1.ItemIndex >=0 then
  begin
     CMethod.Color := ShowFileInfoColor; { make it clear we are showing off a bit... }
     Percentage.Color := ShowFileInfoColor;
     ArchiveHeader.Color := ShowFileInfoColor;
     Time.Color := ShowFileInfoColor;
     TimeLabel.Caption := 'Full Size:';

     { this next bit is ONLY needed because we are applying the display
       mask to our ListBox. If we were not, we could simply set
       whichFile to ListBox1.itemIndex and proceed directly (i.e. 1:1 map) }
     with ListBox1 do { find out *which* occurrence of this filename in ListBox we have, }
     begin            { (as duplicates ARE possible thanks to the AllowDuplicates property }
       whichFileName := Items[ItemIndex];
       whichFile := 1;  { can assume at LEAST one! }
       for counter := 0 to pred(ItemIndex) do
       begin
         if Items[counter]=WhichFilename then { a duplicate }
            inc(WhichFile);
       end; { ok, WhichFile is the occurrence # }
     end; { with }
     with CompLHA1 do { now hunt CompLHA1.FileList for the nth occurrence too }
     begin
       for counter := 0 to pred(FileList.count) do
       begin
         if FileList[counter] = WhichFileName then
         begin
           dec(WhichFile);
           if whichFile = 0 then { got it! }
           begin
              whichFile := counter; { THIS FileList entry corresponds to our Listbox one }
              break;
           end;
         end;
         if counter = pred(FileList.count) then { should never happen! }
            raise Exception.Create('No match between archive list and display!');
        end;  { for }
     end; { with }

     with TCompLHAFileinfo(CompLHA1.FileInformation[whichFile]) do { how to get at the other stuff... }
     begin
       if Fullsize>0 then
       begin
         if Fullsize>100000 then { makes safe for files >20Mb actually }
           Percentageval := CompressedSize div (Fullsize div 100)
         else
           Percentageval := 100*CompressedSize div Fullsize;
         Percentage.caption:=IntToStr(100-percentageval)+'%'
       end else
         Percentage.caption:='(empty)';
       Time.caption:= IntToStr((512+Fullsize) div 1024)+' Kb';
       Key.visible := Encrypted;
       if saveCompressionMethod <0 then
          savecompressionMethod :=cMethod.ItemIndex;
       cMethod.ItemIndex :=ord(CompressedMode); { will show a TCompress RLE as LZH5 }
       if saveArchiveHeader < 0 then
          saveArchiveHeader := ArchiveHeader.ItemIndex;
       ArchiveHeader.ItemIndex := Integer(HeaderLevel);
       attribstr := 'a--'; { simple example of attribute display... }
       if (attributes and faReadOnly) > 0 then
          attribstr := attribstr +'o'
       else
          attribstr := attribstr +'w';
       try { will give exception if DateTime of 0 is in the file -- possible if CompressStreamToArchive is used... }
         Status.caption := 'Datestamp: '+DateTimeToStr(FileDateToDateTime(DateTime))+
             '  '+attribstr;
       except on EConvertError do status.caption := 'No Datestamp'; { suppress message }
       end;
     end; { with }
  end;
end;

procedure TForm1.ResetFileInfo;
begin
  status.caption := '';
  if saveCompressionMethod <0 then exit;
  cMethod.ItemIndex:=savecompressionMethod;
  saveCompressionMethod := -1;
  archiveHeader.ItemIndex := savearchiveHeader;
  SaveArchiveHeader := -1;
  CMethod.Color := clBtnFace;
  ArchiveHeader.Color := clBtnFace;
  Percentage.Color := clWindow;
  Time.Color := clWindow;
  TimeLabel.Caption := 'Time:';
  Key.visible := CompLHA1.Password<>'';
  showInfo(CompLHA1); { get the right stuff too... }
  Time.Caption:=''; { but this is meaningless at this point... }
end;

procedure TForm1.SetDir(const path: string);
begin
 try
   DL.Directory := path;
 except on EInOutError do ; { nowt, let it default }
 end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 Application.HelpFile:='COMPLHA.HLP';
 SetDir(ExtractFilePath(paramstr(0)));
 CompLHA1.archiveName := Archivefile.text;
 SendMessage(ListBox1.handle,LB_SetHorizontalExtent,300,LongInt(0));
end;

function TForm1.GetDir: string; { called below and in GetDummyFileName }
begin
  Result := DL.Directory;
  if Copy(Result,Length(Result),1)<>'\' then { not already \'d? }
    Result := Result+'\';
end;

procedure TForm1.archivefileChange(Sender: TObject);
begin
  CompLHA1.ArchiveName := archiveFile.text;
  showfiles;
end;

function TForm1.getCompressionMethod: TCompLHAMethod;
begin
   result := TCompLHAMethod(CMethod.ItemIndex);
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

{ The handler for dropping things on the file list or archive list }
procedure TForm1.archivefileDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
   if Source = FL then
      CompressFiles;
end;

procedure TForm1.DLDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source=Sender then exit; { seems reasonable, and IS necessary }
  if Source is TListBox then
    ExpandDelete(cmLHAExpand,False) { selected archive files }
  else if Source=ArchiveGroup then
     ExpandDelete(cmLHAExpand,True); { all archived files }
  FL.Update; { get up to date... }
end;

procedure TForm1.TrashcanDragDrop(Sender, Source: TObject; X, Y: Integer);
var counter: Integer;
begin
  if Source is TListBox then
    ExpandDelete(cmLHADelete,False)
  else if Source=ArchiveGroup then
     ExpandDelete(cmLHADelete,True) { all files }
     { and strictly speaking, should now delete the archive if it is
       empty, but I'll leave that as an exercise... }
  else if Source is TFileListBox then { delete some or all... }
  begin
     for counter:=0 to FL.Items.count-1 do
        if FL.selected[counter] and
           (MessageDlg('Delete '+GetDir+FL.Items[counter],mtConfirmation,[mbYes,mbNo],0)=id_Yes) then
           SysUtils.DeleteFile(GetDir+FL.Items[counter]);
     FL.Update;
  end
end;

procedure TForm1.FLClick(Sender: TObject);
begin
  ResetFileInfo;
end;

procedure TForm1.TrashcanDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  accept := True;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
 showfiles; { show files in archive (if any)... }
end;

procedure TForm1.Panel2Resize(Sender: TObject);
begin
  archiveFile.width := Panel2.Width -3;
  Mask.Left := Panel2.Width-Mask.Width;
end;

procedure TForm1.SaveFullPathClick(Sender: TObject);
begin
  ResetFileInfo; { cosmetic stuff }
  CompLHA1.StorePath := SaveFullpath.checked;
end;

procedure TForm1.CMethodClick(Sender: TObject);
begin
  CompLHA1.CompressionMethod := TCompLHAMethod(CMethod.ItemIndex);
end;

procedure TForm1.ConfirmOperationsClick(Sender: TObject);
begin
   ResetFileInfo; { cosmetic stuff }
   CompLHA1.Confirm := ConfirmOperations.checked;
end;

procedure TForm1.ArchiveHeaderClick(Sender: TObject);
begin
   CompLHA1.ArchiveType := TCompLHAArchiveType(ArchiveHeader.ItemIndex);
end;

procedure TForm1.CMethodEnter(Sender: TObject);
begin
     ResetFileInfo; { just a cosmetic thing... }
end;

procedure TForm1.AllowDuplicatesClick(Sender: TObject);
begin
   ResetFileInfo; { cosmetic stuff }
   CompLHA1.AllowDuplicates := AllowDuplicates.checked;
end;

{ Disable dragging temporarily while compression
  is in progress, because otherwise it is *possible* (tho unlikely)
  to request a second compression before the first has finished,
  i.e. code hsould not be re-entrant via the user interface.
}
procedure TForm1.disableDragMode;
begin
  Fl.dragMode := dmManual;
  ArchiveGroup.dragMode := dmManual;
  ListBox1.dragMode := dmManual;
end;

procedure TForm1.enableDragMode;
begin
  Fl.dragMode := dmAutomatic;
  ArchiveGroup.dragMode := dmAutomatic;
  ListBox1.dragMode := dmAutomatic;
end;

procedure TForm1.AboutButtonClick(Sender: TObject);
begin
  ShowMessage('Drag && Drop at will: compression/expansion'+#13+
  'is automatic.'+#13+#13+
  'Component Registration and License: $NZ149 (about $US59)'+#13+
  'See Registration Form in Help or:'+#13+
  'Fax +64-3-384-5138   Email: admin@spis.co.nz');
end;

procedure TForm1.HelpButtonClick(Sender: TObject);
begin
  Application.HelpJump('1050');
end;

procedure TForm1.RegistrationButtonClick(Sender: TObject);
begin
  Application.HelpJump('1030');
end;

{ WILD: If you want to test wild card compression/expansion,
  make this dummy routine "live" by:
  a) Dropping on a new button and hooking it to this event
  b) Creating an edit box (TEdit) called "WildTest"
  c) Enabling either the compression or expansion code -- whichever you prefer
     by removing the "-" from in front of one of the $DEFINES below
}
{-$DEFINE EXPANDWILD} { enable one or the other of these by removing the "-" }
{-$DEFINE COMPRESSWILD}
procedure TForm1.ExpandButtonClick(Sender: TObject);
begin
  with CompLHA1 do
  begin
    FilesToProcess.clear;

{$IFDEF EXPANDWILD}
    FilesToProcess.add(WildTest.text); { a blank string means NOTHING -- NO string
                                        (i.e. empty list) would mean ALL if expanding }
    if VerifyExpands.checked then
       Verify
    else
       Expand;
    FL.Update; { get up to date... }
{$ENDIF}

{$IFDEF COMPRESSWILD}
    FilesToProcess.add(WildTest.text); { a blank string means NOTHING -- NO string
                                        (i.e. empty list) would mean ALL if expanding }
    Compress;  { works also }
    showfiles;
{$ENDIF}

    showinfo(CompLHA1);
  end;
end;

procedure TForm1.MaskChange(Sender: TObject);
begin
  FillFileList;
end;

{ Note: the abort button will be *ignored* unless
  there is an Application.Processmessages in our OnShowProgress
  event -- otherwise the button never "sees" the mouse click
}
procedure TForm1.AbortButtonClick(Sender: TObject);
begin
  CompLHA1.AbortOperation := True;
end;


procedure TForm1.Edit1Change(Sender: TObject);
begin
  with CompLHA1 do
  begin
    Password := (Sender as TEdit).Text;
    Key.visible := Password<>'';
  end;
end;

end.
