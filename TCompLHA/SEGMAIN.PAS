unit SegMain;

interface

uses
{$IFDEF WIN32}
  Windows,
{$ELSE}
  WinTypes, WinProcs,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FileCtrl, StdCtrls, ExtCtrls, Menus, Buttons, CompLHA, SegLHA;

type
  TSegForm = class(TForm)
    MenuBar: TMainMenu;
    MFile: TMenuItem;
    MExit: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    MConfigure: TMenuItem;
    N1: TMenuItem;
    Label1: TLabel;
    ETemplate: TEdit;
    DCB: TDriveComboBox;
    DLB: TDirectoryListBox;
    FLB: TFileListBox;
    Panel3: TPanel;
    GLB: TGroupBox;
    LB: TListBox;
    MCommands: TMenuItem;
    MCompress: TMenuItem;
    MExpand: TMenuItem;
    MScan: TMenuItem;
    MVerify: TMenuItem;
    N2: TMenuItem;
    MReset: TMenuItem;
    MsgBox: TGroupBox;
    MsgLB: TListBox;
    ShowProg: TPanel;
    FileCnt: TLabel;
    Percent: TLabel;
    MFlush: TMenuItem;
    OptionGroup: TRadioGroup;
    AbortBtn: TBitBtn;
    ExecuteBtn: TBitBtn;
    ConfigBtn: TBitBtn;
    MsgLabel: TLabel;
    SegLHA: TSegLHA;
    procedure LBDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ETemplateChange(Sender: TObject);
    procedure ConfigBtnClick(Sender: TObject);
    procedure FLBDblClick(Sender: TObject);
    procedure LBKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MExitClick(Sender: TObject);
    procedure MFlushClick(Sender: TObject);
    procedure SegLHASegChange(const SegPath: String);
    procedure SegLHAShowProgress(var PercentageDone: Longint);
    {Use the following for Delphi 1.0}
    {procedure SegLHACheckFile(const originalFilePath: String;
      var newFilePath: OpenString; dateTime: Longint;
      mode: TCompLHAProcessMode);
    procedure SegLHASegExpand(const CurTargetPath: String;
      var NewTargetPath: OpenString; var ListOfFiles: TStrings);}
    procedure SegLHASegExpand(const CurTargetPath: String;
      var NewTargetPath: String; var ListOfFiles: TStrings);
    procedure SegLHACheckFile(const originalFilePath: string;
      var newFilePath: string; dateTime: Longint;
      mode: TCompLHAProcessMode);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure AbortBtnClick(Sender: TObject);
    procedure OptionGroupClick(Sender: TObject);
    procedure DLBDblClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure MCompressClick(Sender: TObject);
    procedure MExpandClick(Sender: TObject);
    procedure MScanClick(Sender: TObject);
    procedure MVerifyClick(Sender: TObject);
  private
    { Private declarations }
    procedure ResetApp;
    procedure DemoCompress;
    procedure DemoExpand;
    procedure DemoScan;
    procedure DemoVerify;
    procedure InitFileSelect( State: Boolean );
    procedure AddToLB( path: String; UniqueFlag: Boolean );
    procedure AddToMsgLB( Msg: String );
    procedure BuildFileList( DirPath: String; Template: String );
    procedure UpdateCount;
  public
    { Public declarations }
  end;

var
  SegForm: TSegForm;

implementation

uses   SegCfg, SegSel;

{$R *.DFM}
var
  BuildListAbort:    Boolean;

procedure TSegForm.InitFileSelect( State: Boolean );
begin
     ETemplate.Enabled := State;
     DCB.Enabled := State;
     DLB.Enabled := State;
     FLB.Enabled := State;
end;

procedure TSegForm.FormCreate(Sender: TObject);
begin
    ResetApp;
    MsgLB.Clear;
end;

procedure TSegForm.ResetApp;
begin
    BuildListAbort := False;
    ETemplate.Text := '*.*';
    FLB.Mask := ETemplate.Text;
    GLB.Caption := 'File List:';
    LB.OnKeyDown := LBKeyDown;
    LB.Clear;
    UpdateCount;
    Percent.Caption := '';
    InitFileSelect(True);
    MReset.Enabled := False;
    AbortBtn.Enabled := True;
    case OptionGroup.ItemIndex of
    0:  MsgLabel.Caption := 'Drag/Drop Directories or Files to File List';
    1:  MsgLabel.Caption := 'Set Destination directory before expanding';
    2:  MsgLabel.Caption := 'Scan operation only needs the first segment';
    3:  MsgLabel.Caption := 'Set Destination directory before verifying';
    end;
end;

procedure TSegForm.LBDragDrop(Sender, Source: TObject; X, Y: Integer);
var
   i:      Integer;
   PName:  String;
begin
     if (Source is TDirectoryListBox) then
       begin
       LB.Clear;
       GLB.Caption := 'Target: ' + AnsiLowerCase(DLB.Directory);
       GLB.Update;
       BuildListAbort := False;
       BuildFileList(DLB.Directory,FLB.Mask);
       if (BuildListAbort = True) then
         AddToMsgLB('Operation aborted');
       LB.ItemIndex := -1;
       end
     else if (Source is TFileListBox) then
       begin
       with (Source as TFileListBox) do
         begin
         GLB.Caption := 'Target: ' + AnsiLowerCase(DLB.Directory);
         GLB.Update;
         PName := DLB.Directory;
         if (PName[Length(PName)] <> '\') then
           PName := PName + '\';
         if (SelCount > 0) then
           begin
           for i := 0 to Items.Count-1 do
             if (Selected[i] = True) then
               begin
               AddToLB(PName+Items[i],True);
               LB.ItemIndex := -1;
               Application.ProcessMessages;
               end;
           end;
         end
       end;

end;

procedure TSegForm.LBDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
     if (Sender = Source) then
       Accept := False;
end;

procedure TSegForm.FormResize(Sender: TObject);
begin
     if (Height < 390) then
       Height := 390;
     if (Width < 452) then
       Width := 452;
end;

procedure TSegForm.ETemplateChange(Sender: TObject);
begin
     if (ETemplate.Text = '') then
       ETemplate.Text := '*.*';
     FLB.Mask := ETemplate.Text;
end;

procedure TSegForm.AddToLB( path: String; UniqueFlag: Boolean );
var
  i:     Integer;
  ok:    Boolean;
begin
     ok := True;
     if (UniqueFlag) then
       begin
       for i := 0 to LB.Items.Count-1 do
         begin
         if (CompareText(path, LB.Items[i]) = 0) then
           ok := False;
         if (ok = False) then
           break;
         end;
       end;

     if (ok) then
       begin
       LB.Items.Add(AnsiLowerCase(Path));
       LB.ItemIndex := LB.Items.Count-1;
       LB.Update;
       UpdateCount;
       end;
end;

procedure TSegForm.AddToMsgLB( Msg: String );
begin
     MsgLB.Items.Add(Msg);
     MsgLB.ItemIndex := MsgLB.Items.Count-1;
     MsgLB.Update;
end;

procedure TSegForm.BuildFileList( DirPath: String; Template: String );
var
   FInfo:         TSearchRec;
   DirSL:         TStringList;
   FileResult:    Integer;
   PName:         String;
   DName:         String;
   i:             Integer;
begin
     DirSL := TStringList.Create;
     if (DirPath[Length(DirPath)] = '\') then
       DName := DirPath
     else
       DName := DirPath + '\';
     FileResult := FindFirst( DName+Template, faAnyFile, FInfo );
     while (FileResult = 0) do
       begin
       PName := DirPath;
       if (DirPath[Length(DirPath)] = '\') then
         PName := Copy(PName,1,Length(DirPath)-1);
       PName := PName + '\' + FInfo.Name;
       if ((FInfo.Attr and faDirectory) = faDirectory) then
         begin
         if (FInfo.Name[1] <> '.') then
           DirSL.Add(PName)
         end
       else
         begin
         AddToLB(PName,True);
         Application.ProcessMessages;
         if (BuildListAbort = True) then
           exit;
         end;

       FileResult := FindNext(FInfo);
       end;

     FindClose(FInfo);

     for i := 0 to DirSL.Count-1 do
       begin
       BuildFileList(DirSL[i],Template);
       if (BuildListAbort = True) then
         break;
       end;

     DirSL.Free;
end;

procedure TSegForm.UpdateCount;
begin
     FileCnt.Caption := 'Files: ' + IntToStr(LB.Items.Count);
     FileCnt.Update;
end;

procedure TSegForm.ConfigBtnClick(Sender: TObject);
begin
     CfgForm.ShowModal;
end;

procedure TSegForm.FLBDblClick(Sender: TObject);
var
   PName:     String;
begin
     PName := FLB.Directory;
     if (PName[Length(PName)] <> '\') then
       PName := PName + '\';
     PName := PName + FLB.Items[FLB.ItemIndex];
     AddToLB(PName,True);
end;

procedure TSegForm.OptionGroupClick(Sender: TObject);
begin
     ResetApp;
end;

procedure TSegForm.ResetBtnClick(Sender: TObject);
begin
     ResetApp;
end;

procedure TSegForm.AbortBtnClick(Sender: TObject);
begin
     MReset.Enabled := True;
     AbortBtn.Enabled := False;
     BuildListAbort := True;
     SegLHA.AbortOperation := True;
     Application.ProcessMessages;
end;

procedure TSegForm.ExecuteBtnClick(Sender: TObject);
begin
     AbortBtn.Enabled := True;
     case OptionGroup.ItemIndex of
     0:   DemoCompress;
     1:   DemoExpand;
     2:   DemoScan;
     3:   DemoVerify;
     end;
end;

procedure TSegForm.DemoCompress;
var
   i:     Integer;
begin
     if (LB.Items.Count = 0) then
       Exit;
     InitFileSelect(False);
     with SegLHA do
       begin
       FilesToProcess.Clear;
       for i := 0 to Pred(LB.Items.Count) do
         FilesToProcess.Add(LB.Items[i]);
       LB.Clear;
       TargetPath := AnsiLowerCase(DLB.Directory);
       StorePath := True;
       try
         AddToMsgLB('[Compress operation]');
         Compress;
       except on E: Exception do
         begin
         AddToMsgLB('>> ' + E.Message);
         AbortBtn.Enabled := False;
         MReset.Enabled := True;
         exit;
         end;
       end; {try}

       DLB.Refresh;
       FLB.Refresh;
       AbortBtn.Enabled := False;
       AddToMsgLB('Compress operation complete!');
       end;
end;

procedure TSegForm.DemoExpand;
begin
     InitFileSelect(True);
     with SegLHA do
       begin
       try
         TargetPath := DLB.Directory;
         AddToMsgLB('[Expand operation]');
         MakeDirectories := True;
         Expand;
       except on E: Exception do
         begin
         AddToMsgLB('>> ' + E.Message);
         AbortBtn.Enabled := False;
         MReset.Enabled := True;
         exit;
         end;
       end; {try}

       DLB.Update;
       FLB.Update;
       AbortBtn.Enabled := False;
       AddToMsgLB('Expand operation complete!');
       end;
end;

procedure TSegForm.DemoScan;
var
   i:     Integer;
begin
     LB.Clear;
     UpdateCount;
     InitFileSelect(False);
     with SegLHA do
       begin
       try
         if (TargetPath = '') then
           TargetPath := TempArchivePath;
         AddToMsgLB('[Scan operation]');
         Scan;
       except on E: Exception do
         begin
         AddToMsgLB('>> ' + E.Message);
         AbortBtn.Enabled := False;
         MReset.Enabled := True;
         exit;
         end;
       end; {try}

       AbortBtn.Enabled := False;
       for i := 0 to pred(FileList.Count) do
         AddToLB(AnsiLowerCase(FileList[i]),False);
       LB.ItemIndex := 0;
       LB.OnKeyDown := nil;
       AddToMsgLB('Scan operation complete!');
       end;

end;

procedure TSegForm.DemoVerify;
begin
     InitFileSelect(True);
     with SegLHA do
       begin
       try
         if (TargetPath = '') then
           TargetPath := TempArchivePath;
         AddToMsgLB('[Verify operation]');
         MakeDirectories := True;
         Verify;
       except on E: Exception do
         begin
         AddToMsgLB('>> ' + E.Message);
         AbortBtn.Enabled := False;
         MReset.Enabled := True;
         exit;
         end;
       end; {try}

       AbortBtn.Enabled := False;
       AddToMsgLB('Verify operation complete!');
       end;
end;

{When compiling under Delphi 1.0 use the following defination}
{procedure TSegForm.SegLHACheckFile(const originalFilePath: String;
  var newFilePath: OpenString; dateTime: Longint;
  mode: TCompLHAProcessMode);}

procedure TSegForm.SegLHACheckFile(const originalFilePath: string;
  var newFilePath: string; dateTime: Longint; mode: TCompLHAProcessMode);
begin
     AddToLB(originalFilePath,False);
     Application.ProcessMessages;
end;

procedure TSegForm.SegLHASegChange(const SegPath: String);
begin
     AddToMsgLB('Segment #' + IntToStr(SegLHA.SegNumber) + ': ' + SegPath);
end;

procedure TSegForm.LBKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
   i:     Integer;
begin
     if (Key = VK_DELETE) then
       begin
       while (LB.SelCount > 0) do
         begin
         for i := 0 to pred(LB.Items.Count) do
           begin
           if (LB.Selected[i]) then
             begin
             LB.Items.Delete(i);
             break;
             end;
           end;
         end;
       UpdateCount;
       LB.Update;
       end;

       if (LB.Items.Count > 0) then
         GLB.Caption := 'File List:';
end;

procedure TSegForm.SegLHAShowProgress(var PercentageDone: Longint);
begin
     Percent.Caption := 'Percent Done: ' + IntToStr(PercentageDone) + '%';
     Application.ProcessMessages;
end;

procedure TSegForm.MExitClick(Sender: TObject);
begin
     Close;
end;

procedure TSegForm.MFlushClick(Sender: TObject);
begin
     MsgLB.Clear;
end;

{When compiling under Delphi 1.0 use the following defination}
{procedure TSegForm.SegLHASegExpand(const CurTargetPath: String;
  var NewTargetPath: OpenString; var ListOfFiles: TStrings);}

procedure TSegForm.SegLHASegExpand(const CurTargetPath: String;
  var NewTargetPath: String; var ListOfFiles: TStrings);
var
   i:      Integer;
   SL:     TStringList;
   Target: String;
begin
     if (ListOfFiles.Count > 0) then
       begin
       SL := TStringList.Create;
       Target := CurTargetPath;
       for i := 0 to pred(ListOfFiles.Count) do
         SL.Add(ListOfFiles[i]);
       SelForm.EntryPoint(Target,SL);
       if (SegLHA.AbortOperation = True) then
         begin
         SL.Free;
         end
       else
         begin
         if ((Target <> '') and (Target <> CurTargetPath)) then
           NewTargetPath := Target;
         AddToMsgLB('Target: ' + NewTargetPath);
         ListOfFiles.Clear;
         for i := 0 to pred(SL.Count) do
           ListOfFiles.Add(SL[i]);
         SL.Free;
         end;
       end;
end;

procedure TSegForm.DLBDblClick(Sender: TObject);
begin
     SegLHA.TargetPath := DLB.Directory;
end;


procedure TSegForm.MCompressClick(Sender: TObject);
begin
     OptionGroup.ItemIndex := 0;
     OptionGroupClick(Sender);
end;

procedure TSegForm.MExpandClick(Sender: TObject);
begin
     OptionGroup.ItemIndex := 1;
     OptionGroupClick(Sender);
end;

procedure TSegForm.MScanClick(Sender: TObject);
begin
     OptionGroup.ItemIndex := 2;
     OptionGroupClick(Sender);
end;

procedure TSegForm.MVerifyClick(Sender: TObject);
begin
     OptionGroup.ItemIndex := 3;
     OptionGroupClick(Sender);
end;

end.
