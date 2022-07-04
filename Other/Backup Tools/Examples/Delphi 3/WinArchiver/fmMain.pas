unit fmMain;

interface
{$I mmm.inc}

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Grids,
  ExtCtrls,
  Menus,
  ComCtrls,
  Archiver,
  StdCtrls,
  {$IFDEF D3}
  ToolWin,
  {$ENDIF}
  Registry,
  ArchiverRoot,
  CustExtractor,
  CustArchiver,
  Dialogs,
  ArchiverMisc,
  CustSFXGenerator,
  SFXGenerator,
  ShellAPI,
  {$IFDEF D3}
  dropsource,
  {$ENDIF}
  {$IFDEF D2}
  uDragFilesSrc,
  {$ENDIF}
  unTranslation;

type
  TListSortCompare = function ( Sender : TObject; item1, item2 : TObject ) : Integer of Object;

  TMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Quit1: TMenuItem;
    Action1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Add1: TMenuItem;
    Extract1: TMenuItem;
    Delete1: TMenuItem;
    Openarchive1: TMenuItem;
    Newarchive1: TMenuItem;
    N1: TMenuItem;
    StatusPanel: TPanel;
    PanelProgress: TPanel;
    OpenDialog1: TOpenDialog;
    Archiver1: TArchiver;
    N2: TMenuItem;
    Resetarchive1: TMenuItem;
    Deletearchive1: TMenuItem;
    Closearchive1: TMenuItem;
    N3: TMenuItem;
    Information1: TMenuItem;
    PanelStatusBar: TPanel;
    StatusBar1: TStatusBar;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    N4: TMenuItem;
    Selectall1: TMenuItem;
    Options1: TMenuItem;
    Configuration1: TMenuItem;
    Panel1: TPanel;
    btnAbort: TButton;
    Opensegment1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    tbAdd: TToolButton;
    tbExtract: TToolButton;
    tbDelete: TToolButton;
    ImageList1: TImageList;
    Timer1: TTimer;
    Rename1: TMenuItem;
    MakeEXEfile1: TMenuItem;
    N5: TMenuItem;
    SFXConfiguration1: TMenuItem;
    SFXGenerator1: TSFXGenerator;
    SetArchiveComment1: TMenuItem;
    View1: TMenuItem;
    Selectnone1: TMenuItem;
    Invertselection1: TMenuItem;
    tvDirectory: TTreeView;
    Slider1: TPanel;
    ImageList2: TImageList;
    PopupMenu1: TPopupMenu;
    Treeview1: TMenuItem;
    N6: TMenuItem;
    PopupMenu2: TPopupMenu;
    Expandall1: TMenuItem;
    Collapseall1: TMenuItem;
    paFiles: TPanel;
    lbFiles: TListBox;
    paScrollBar: TPanel;
    paHeader: TPanel;
    hcFiles: THeaderControl;
    Selectall2: TMenuItem;
    Selectnone2: TMenuItem;
    InvertSelection2: TMenuItem;
    tbView: TToolButton;
    Checkintegrity1: TMenuItem;
    N7: TMenuItem;
    ViewlastOutput1: TMenuItem;
    tbInstall: TToolButton;
    Font1: TMenuItem;
    Sort1: TMenuItem;
    Originalorder1: TMenuItem;
    Name1: TMenuItem;
    Date1: TMenuItem;
    Time1: TMenuItem;
    Size1: TMenuItem;
    Ratio1: TMenuItem;
    Packed1: TMenuItem;
    Segment1: TMenuItem;
    Path1: TMenuItem;
    N8: TMenuItem;
    FontDialog1: TFontDialog;
    Filters1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Quit1Click(Sender: TObject);
    procedure Archiver1StartOperation(Sender: TObject);
    procedure Archiver1FinishOperation(Sender: TObject);
    procedure Archiver1FileProgress(Sender: TObject; Percent: Integer);
    procedure Archiver1ExtractFile(Sender: TObject;
      const FileEntry: TFileEntry; var DestPath: String;
      var Accept: Boolean);
    procedure Openarchive1Click(Sender: TObject);
    procedure Archiver1Enumeration(Sender: TObject;
      const FileEntry: TFileEntry);
    procedure Newarchive1Click(Sender: TObject);
    procedure Resetarchive1Click(Sender: TObject);
    procedure Deletearchive1Click(Sender: TObject);
    procedure Closearchive1Click(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure StatusBar1Resize(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Extract1Click(Sender: TObject);
    procedure Archiver1DeleteFile(Sender: TObject;
      const FileEntry: TFileEntry; var Accept: Boolean);
    procedure Selectall1Click(Sender: TObject);
    procedure Information1Click(Sender: TObject);
    procedure Configuration1Click(Sender: TObject);
    procedure Archiver1FileAdded(Sender: TObject;
      const FileEntry: TFileEntry);
    procedure btnAbortClick(Sender: TObject);
    procedure Opensegment1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Archiver1AfterOpen(Sender: TObject);
    procedure Archiver1AfterClose(Sender: TObject);
    procedure Archiver1EnterCryptKey(Sender: TObject; var Key: String);
    procedure Archiver1RequestCryptKey(Sender: TObject; var Key: String);
    procedure Rename1Click(Sender: TObject);
    procedure SFXConfiguration1Click(Sender: TObject);
    procedure MakeEXEfile1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SetArchiveComment1Click(Sender: TObject);
    procedure Archiver1BeforeClose(Sender: TObject);
    procedure Archiver1ShowComment(Sender: TObject; const Comment: String);
    procedure Archiver1ShowTiming(Sender: TObject; ElapsedTime,
      RemainingTime: TDateTime);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure Archiver1DisplayMessage(Sender: TObject; const msg: String);
    procedure Archiver1ClearFileList(Sender: TObject);
    procedure Selectnone1Click(Sender: TObject);
    procedure Invertselection1Click(Sender: TObject);
    procedure Action1Click(Sender: TObject);
    procedure Slider1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lvFilesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvDirectoryClick(Sender: TObject);
    procedure tvDirectoryKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvDirectoryCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure Treeview1Click(Sender: TObject);
    procedure Expandall1Click(Sender: TObject);
    procedure Collapseall1Click(Sender: TObject);
    procedure lbFilesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure paFilesResize(Sender: TObject);
    procedure hcFilesSectionResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure hcFilesSectionTrack(HeaderControl: THeaderControl;
      Section: THeaderSection; Width: Integer; State: TSectionTrackState);
    procedure lbFilesDblClick(Sender: TObject);
    procedure lbFilesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbFilesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure paHeaderResize(Sender: TObject);
    procedure paScrollBarResize(Sender: TObject);
    procedure lbFilesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure hcFilesSectionClick(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure Archiver1AddFile(Sender: TObject; var FileEntry: TFileEntry;
      var Accept: Boolean);
    procedure FormResize(Sender: TObject);
    procedure ViewlastOutput1Click(Sender: TObject);
    procedure Archiver1Error(Sender: TObject; E: Exception;
      const FileEntry: TFileEntry; var ErrorAction: TErrorAction);
    procedure Archiver1FileExtracted(Sender: TObject;
      const FileEntry: TFileEntry; const DestPath: String);
    procedure Checkintegrity1Click(Sender: TObject);
    procedure tbInstallClick(Sender: TObject);
    procedure lbFilesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbFilesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Font1Click(Sender: TObject);
    procedure Originalorder1Click(Sender: TObject);
    procedure hcFilesDrawSection(HeaderControl: THeaderControl;
      Section: THeaderSection; const Rect: TRect; Pressed: Boolean);
    procedure Archiver1AddToLog(Sender: TObject; const msg: String);
    procedure Archiver1DeleteFileByIndex(Sender: TObject; Index: Integer;
      var Accept: Boolean);
    procedure Archiver1ExtractFileByIndex(Sender: TObject; Index: Integer;
      var DestPath: String; var Accept: Boolean);
    procedure Filters1Click(Sender: TObject);
  private
    { Déclarations privées }
    FTitle : String;
    FCaption : String;
    {$IFDEF D3}
    FDropSource : TDropFileSource;
    {$ENDIF}
    {$IFDEF D2}
    FDragFilesSrc : TDragFilesSrc;
    {$ENDIF}
    FShowTreeView : Boolean;
    FNodeID : Integer;
    FCurrentDirID : Integer;
    FLastFileName : String;
    FFileSep : TMenuItem;
    FLastWidth : Integer;
    FScrollBar : TScrollBar;
    FListSortCompare : TListSortCompare;
    FAscending : Boolean;
    FStart : DWord;
    FAddToCurrentFolder : Boolean;
    FExtIcons : TStringList;
    FDroping : Boolean;
    FFilesDroped : TStringList;
    FInstallIdx : Integer;
    FDragPoint : TPoint;
    FButtonClicked : Boolean;
    FSortedColumn : Integer;
    FFilterExtension : Boolean;
    FKindOfFilter : Integer;
    FFilters : TStringList;

    procedure SetProgressBar( val : Boolean );
    procedure DisplayHint(Sender: TObject);
    procedure UpdateCaption;
    procedure AddFileToGrid( const FileEntry : TFileEntry; listIdx : Integer );
    procedure ShowArchiveSize;
    procedure UpdateActionMenu;
    function  IsArchiveEmpty : Boolean;
    function  GetFullFileName( const FileName : String ) : String;
    procedure WMDROPFILES(var Message: TWMDROPFILES); message WM_DROPFILES;
    function  GetRegistryKey : String;
    procedure SaveToRegistry;
    procedure LoadFromRegistry;
    procedure ExecuteFile( ask : Boolean );
    procedure WMTranslate( var Message : TMessage ); message WM_TRANSLATE;
    function  IsFileSelected( idx : Integer ) : Boolean;
    procedure CalcSelSize;
    function  lvFilesCompareCol0(Sender: TObject; Item1, Item2: TObject ) : Integer;
    function  lvFilesCompareCol1(Sender: TObject; Item1, Item2: TObject ) : Integer;
    function  lvFilesCompareCol2(Sender: TObject; Item1, Item2: TObject ) : Integer;
    function  lvFilesCompareCol3(Sender: TObject; Item1, Item2: TObject ) : Integer;
    function  lvFilesCompareCol4(Sender: TObject; Item1, Item2: TObject ) : Integer;
    function  lvFilesCompareCol5(Sender: TObject; Item1, Item2: TObject ) : Integer;
    function  lvFilesCompareCol6(Sender: TObject; Item1, Item2: TObject ) : Integer;
    function  lvFilesCompareCol7(Sender: TObject; Item1, Item2: TObject ) : Integer;
    function  GetDirectoryIdx( const FileName : String ) : Integer;
    procedure ClearDirectory;
    procedure SetShowTreeView( val : Boolean );
    procedure DefDirectory( dirIdx : Integer );
    procedure CreateSep;
    procedure RemoveSep;
    procedure RecordFile( const FileName : String );
    procedure RemoveLastFileMenu( menuItem : TMenuItem );
    procedure RemoveLastFile( const FileName : String );
    procedure LastFilesClick( Sender : TObject );
    procedure OpenArchive( const FileName : String );
    procedure DirSelChanged;
    procedure FileSelChanged;
    procedure WriteLastFilesToRegistry( reg : TRegistry );
    procedure ReadLastFilesFromRegistry( reg : TRegistry );
    function  CalcColWidth : Integer;
    procedure ScrollBarChanged( Sender : TObject );
    function  GetDispayableFiles : Integer;
    procedure UpdateScrollBar( w : Integer );
    procedure QuickSort( SortList: TStrings; iLo, iHi : Integer;
                         SCompare: TListSortCompare);
    procedure Sort(Compare: TListSortCompare);
    procedure DoSort;
    function  GetIconFrom( const FileName : String; var IconIndex : Integer ) : Boolean;
    {$IFDEF D3}
    procedure DropSource1Drop( Sender: TObject; DragType: TDragType; var ContinueDrop: Boolean);
    {$ENDIF}
    {$IFDEF D2}
    procedure DragFilesSrcDropping( Sender : TObject );
    {$ENDIF}
    procedure DeleteTempDropedFiles;
    procedure CheckIfInstall( const FileEntry : TFileEntry; idx : Integer );
    procedure ClearInstallRef;
    procedure StartFileDrag;
    procedure SelectSortCol( col : Integer );
    function  CheckFilesToDrop( const FileName : String ) : Boolean;

  public
    { Déclarations publiques }
    procedure DisplayMessage( const msg : String );
    procedure ClearGrid;
    procedure RefreshContent;
    function  SizeAsString( size : Comp ) : String;
    function  SelectionAsString : String;
    function  FilterIsTrue( const filter, searchValue : String ) : Boolean;
    procedure DisplayComment( const comment : String );
    function  CheckBusy : Boolean;

    property ShowTreeView : Boolean read FShowTreeView write SetShowTreeView;
  end;

  procedure RegisterFileType( ft, key, desc, icon, prg : String );
  function  AddQuotes (const S: String) : String;
  function  fileExec(const aCmdLine: String; aHide, aWait: Boolean): Boolean;
  function  ExecAndWait (const Filename, Parms: String) : Boolean;
  function  FindExec( const FileName : String; var Exe : String ) : Boolean;
  function  GetShIcon( FileName : String; var IconIndex : Integer ) : Boolean; // get the shell's small icon for a given file/folder
  function  ImageList_Draw( ImageList : THandle; IconIndex: Integer; // to draw a imagelist-image on a hdc
                            Dest : HDC; X, Y : Integer; Style : UINT): Bool; stdcall; external 'comctl32.dll' name 'ImageList_Draw';


var
  Main: TMain;
  gExecuting : Boolean;
  shlv : THandle;

implementation
uses FileCtrl, fmAboutBox, fmAdd, fmDelete, fmExtract, fmInformation,
     fmConfiguration, fmEnterCryptKey, fmSFXConfig, fmArchComment,
     fmTiming, fmAddDropedFiles, fmSFXComments, fmView,
     fmTextViewer, fmLastOutput, fmFilters;
{$R *.DFM}
{$R version.res}

procedure TMain.FormCreate(Sender: TObject);
begin
  FInstallIdx := -1;
  FAddToCurrentFolder := True;
  FAscending := True;
  PanelProgress.Visible := False;
  FTitle := Application.Title;
  FCaption := Caption;
  Application.OnHint := DisplayHint;
  Archiver1AfterClose( Archiver1 );
  RegisterFileType( '.mmm', '', Format(GetStr(sFileAssoc),[Application.Title]), Application.ExeName, Application.ExeName );
  Timing := TTiming.Create( Self );
  Timing.Parent := Self;
  FExtIcons := TStringList.Create;
  FFilesDroped := TStringList.Create;
  FSortedColumn := -1;
  {Let Windows know we accept dropped files}
  DragAcceptFiles( Handle, True );
  {$IFDEF D3}
    // Create a TDropSource component
    FDropSource := TDropFileSource.Create( Self );
    with FDropSource do
      begin
        DragTypes := [dtCopy];
        OnDrop := DropSource1Drop;
      end;
  {$ENDIF}
  {$IFDEF D2}
  FDragFilesSrc := TDragFilesSrc.Create(Self);
  FDragFilesSrc.DropEffect := deCopy;
  //FDragFilesSrc.VerifyFiles := True;
  FDragFilesSrc.OnDropping := DragFilesSrcDropping;
  {$ENDIF}
  FShowTreeView := True;
  FScrollBar := TScrollBar.Create(Self);
  with FScrollBar do
    begin
      Parent := paScrollBar;
      Kind := sbHorizontal;
      TabStop := False;
      OnChange := ScrollBarChanged;
    end;
  FFilterExtension := True;
  FKindOfFilter := 1; // Exclude
  FFilters := TStringList.Create;
  {ShowScrollBar( lbFiles.Handle, SB_HORZ, True );
  EnableScrollBar( lbFiles.Handle, SB_HORZ, ESB_ENABLE_BOTH );
  SetScrollRange( lbFiles.Handle, SB_HORZ, 0, 100, True );
  SetScrollPos( lbFiles.Handle, SB_HORZ, 10, True );}
end;

procedure TMain.DisplayHint(Sender: TObject);
var
  s : String;
begin
  s := GetLongHint(Application.Hint);
  if s = '' then
    if StatusBar1.SimplePanel then
      s := GetStr( sDefaultMsg )
    else
      s := SelectionAsString;
  DisplayMessage( s );
end;

procedure TMain.DisplayMessage( const msg : String );
begin
  //if StatusBar1.SimplePanel then
    StatusBar1.SimpleText := msg;
  //else
    StatusBar1.Panels.Items[0].Text := msg;
  StatusBar1.Update;
end;


procedure TMain.About1Click(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TMain.Quit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMain.SetProgressBar( val : Boolean );
begin
  if not val and Archiver1.IsBusy then
    Exit;
  ProgressBar1.Position := 0;
  ProgressBar1.Max := 100;
  PanelProgress.Visible := val;
  StatusBar1.SimplePanel := val;
  if val then
    DisplayMessage( '' )
  else
    if Archiver1.IsOpen then
      DisplayMessage( SelectionAsString )
    else
      DisplayMessage( '' );
end;

procedure TMain.Archiver1StartOperation(Sender: TObject);
const
  kMinSize = 300*1024;
begin
  SetProgressBar( True );
  ClearLog;
  Timing.Left := (Width - Timing.Width) div 2;
  Timing.Top := (Height - Timing.Height) div 2;
  if Archiver1.Operation <> opExtract then
    begin
      lbFiles.Enabled := False;
      lbFiles.Items.BeginUpdate;
      tvDirectory.Items.BeginUpdate;
    end;
  if Archiver1.Operation in [opEnumerate, opDelete] then
    ClearInstallRef;
end;

procedure TMain.Archiver1FinishOperation(Sender: TObject);
begin
  AddToLog( Format('### %s    %s', [GetStr(600), TimeToStr(Archiver1.ElapsedTime)]) );
  Timing.Hide;
  SetProgressBar( False );
  ShowArchiveSize;
  UpdateActionMenu;
  if Archiver1.Operation <> opExtract then
    begin
      lbFiles.Items.EndUpdate;
      lbFiles.Enabled := True;
      tvDirectory.Items.EndUpdate;
      paScrollBarResize(paScrollBar);
      DoSort;
    end;
end;

var
  gCounter : DWORD;
procedure TMain.Archiver1FileProgress(Sender: TObject;
  Percent: Integer);
begin
  ProgressBar1.Position := Percent;
  // Process messages only every 1/3 second
  if Abs(Integer(GetTickCount) - Integer(gCounter)) >= 333 then
    begin
      gCounter := GetTickCount;
      Application.ProcessMessages;
    end;
  btnAbort.Enabled := Archiver1.CanAbort;
end;

function TMain.IsFileSelected( idx : Integer ) : Boolean;
var
  i : Integer;
begin
  Result := False;
  with lbFiles do
    for i := 0 to Items.Count - 1 do
      if Selected[i] and  (idx = Integer(Items.Objects[i])) then
        begin
          Result := True;
          Break;
        end;
end;

function TMain.CheckFilesToDrop( const FileName : String ) : Boolean;
begin
  {$IFDEF D3}
    Result := Assigned(FDropSource) and (FDropSource.Files.IndexOf( GetTempDir+ExtractFileName(FileName) ) >= 0);
  {$ENDIF}
  {$IFDEF D2}
    Result := Assigned(FDragFilesSrc) and (FDragFilesSrc.Files.IndexOf( GetTempDir+ExtractFileName(FileName) ) >= 0);
  {$ENDIF}
end;

procedure TMain.Archiver1ExtractFile(Sender: TObject;
  const FileEntry: TFileEntry; var DestPath: String; var Accept: Boolean);
begin
  if FDroping then
    begin
      Accept := CheckFilesToDrop(FileEntry.Name);
      Exit;
    end;

  case Extract.rgFiles.ItemIndex of
  0: // Selected files
    begin
    end;
  2: // pattern
    begin
      Accept := FilterIsTrue( Extract.edFiles.Text, ExtractFileName(FileEntry.Name) );
    end;
  end;
end;

procedure TMain.Openarchive1Click(Sender: TObject);
begin
  if CheckBusy then
    Exit;
  with OpenDialog1 do
    begin
      InitialDir := ExtractFilePath( SaveDialog1.FileName );
      Title := GetStr( sOpenExistingArchive );
      if Execute then
        begin
          if (FileName = Archiver1.FileName) and Archiver1.IsOpen then
            Exit;
          OpenArchive( FileName );
        end;
    end;
end;

procedure TMain.OpenArchive( const FileName : String );
begin
  if CheckBusy then
    Exit;
  if not FileExists( FileName ) then
    begin
      MessageDlg( Format(GetStr(sArchiveDoesNotExist), [FileName]), mtWarning, [mbOk], 0 );
      Exit;
    end;
  Archiver1.Close;
  Archiver1.Options := Archiver1.Options - [oOpenSingleSegment];
  Archiver1.FileName := FileName;
  Archiver1.Open;
end;

procedure TMain.ClearGrid;
begin
  lbFiles.Items.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    lbFiles.Clear;
  finally
    lbFiles.Items.EndUpdate;
    Screen.Cursor := crDefault;
    paScrollBarResize(paScrollBar);
  end;
end;

procedure TMain.ShowArchiveSize;
var
  tmp : TDataInfo;
begin
  with Archiver1 do
    begin
      if oOpenSingleSegment in Options then
        tmp := Header.SegmentInfo
      else
        tmp := Header.ArchiveInfo;
      StatusBar1.Panels[1].Text := Format( GetStr(sFiles), [tmp.FileCount, SizeAsString(tmp.Size)]);
    end;
  DisplayMessage( SelectionAsString );
end;

procedure TMain.RefreshContent;
begin
  Archiver1.EnumerateFiles;
end;

procedure TMain.AddFileToGrid( const FileEntry : TFileEntry; listIdx : Integer );

  function GetAssociatedIcon( const FileName : String ) : Integer;
  var
    name, ext : String;
  begin
    name := UpperCase(ExtractFileName(FileName));
    ext := UpperCase(ExtractFileExt(FileName));
    if ( name = 'INSTALL.EXE') or
            ( name = 'SETUP.EXE' ) then
      Result := -8
    else if CompareText( ext, '.EXE' ) = 0 then
      Result := -6
    else if UpperCase(Copy(name, 1, 6)) = 'README' then
      Result := -7
    else if not GetIconFrom( FileName, Result ) then
      Result := -1;

  end;

begin
  with FileEntry, Archiver1.Files[listIdx] do
    begin
      if DirectoryIndex = 0 then
        begin
          DirectoryIndex := GetDirectoryIdx(Name);
        end;
      if ffCrypted in FileEntry.FileFlag then
        StateIndex := 5
      else
        StateIndex := -1;
      ImageIndex := GetAssociatedIcon( FileEntry.Name );
      if FShowTreeView and
         ((tvDirectory.Selected = nil) or (Integer(tvDirectory.Selected.Data) <> DirectoryIndex)) then
        Exit;
      if not(oShowEmptyFolders in Archiver1.Options) and (ffEmptyFolder in FileEntry.FileFlag) then
        Exit;
      lbFiles.Items.AddObject( '', TObject(listIdx) );
    end;
end;

procedure TMain.CheckIfInstall( const FileEntry : TFileEntry; idx : Integer );
var
  s : String;
begin
  s := UpperCase(ExtractFileName(FileEntry.Name));
  if (FInstallIdx < 0) and
     ( (s = 'INSTALL.EXE') or (s = 'SETUP.EXE') ) then
    begin
      FInstallIdx := idx;
      tbInstall.Enabled := True;
    end;
end;

procedure TMain.Archiver1Enumeration(Sender: TObject;
  const FileEntry: TFileEntry);
begin
  AddFileToGrid( FileEntry, Archiver1.FileCount - 1 );
  CheckIfInstall( FileEntry, Archiver1.FileCount - 1 );
end;

procedure TMain.Newarchive1Click(Sender: TObject);
var
  tmp : String;
begin
  if CheckBusy then
    Exit;
  with SaveDialog1 do
    begin
      InitialDir := ExtractFilePath( OpenDialog1.FileName );
      Title := GetStr( sCreateNewArchive );;
      if Execute then
        begin
          if oWriteSFXCode in Archiver1.Options then
            FileName := ChangeFileExt( FileName, '.exe' );
          if FileExists( FileName ) then
            raise Exception.CreateFmt( GetStr(sFileAlreadyExists), [FileName]);
          Archiver1.Close;
          Archiver1.FileName := FileName;
          Archiver1.Open;
          // Set the archive comment for a SFX archive
          with Archiver1, SFXGenerator1.TagInfo do
            begin
              if IsEmpty and IsExeFile(SaveDialog1.FileName) and (Comment <> none) then
                begin
                  tmp := '<SFXStartComment>'+SFXComments.mmBefore.Text+
                         '<SFXEndComment>'+SFXComments.mmAfter.Text+
                         '<SFXCommentEnd>';
                  SetArchiveComment( tmp );
                end;
            end;
        end;
    end;
end;

procedure TMain.UpdateCaption;
begin
  Caption := FCaption + ' - ' + Archiver1.ArchiveName+Archiver1.ArchiveExt;
  Application.Title := FTitle+ ' - ' + Archiver1.ArchiveName+Archiver1.ArchiveExt;
  Resetarchive1.Enabled := True;
  Deletearchive1.Enabled := True;
  Closearchive1.Enabled := True;
  Information1.Enabled := True;
  Rename1.Enabled := True;
  StatusBar1.SimplePanel := False;
  UpdateActionMenu;
end;

procedure TMain.Resetarchive1Click(Sender: TObject);
begin
  if CheckBusy then
    Exit;
  if MessageDlg( GetStr(sResetArchive), mtConfirmation, [mbYes, mbNo], 0 ) = mrYes then
    begin
      Archiver1.Reset;
    end;
end;

procedure TMain.Deletearchive1Click(Sender: TObject);
begin
  if CheckBusy then
    Exit;
  if MessageDlg( GetStr(sDeleteArchive), mtConfirmation, [mbYes, mbNo], 0 ) = mrYes then
    begin
      Archiver1.Delete;
    end;
end;

procedure TMain.Closearchive1Click(Sender: TObject);
begin
  if CheckBusy then
    Exit;
  Archiver1.Close;
end;

procedure TMain.Add1Click(Sender: TObject);
var
  id : Integer;
begin
  if CheckBusy then
    Exit;
  Add.cbRecurse.Checked := oRecurseFolders in Archiver1.Options;
  Add.cbIncludeCurrent.Checked := oIncludeStartingDirectory in Archiver1.Options;
  Add.cbEmptyFolders.Checked := oStoreEmptyFolders in Archiver1.Options;
  Add.cbCryptFiles.Checked := oEncryptFiles in Archiver1.Options;
  Add.cbCryptFiles.Enabled := not(afCrypted in Archiver1.Header.ArchiveFlag);
  Add.cbAddToCurrentFolder.Visible := tvDirectory.Visible;
  Add.cbAddToCurrentFolder.Checked := FAddToCurrentFolder;
  case Archiver1.PathStorage of
    psNone:     Add.cbPathStorage.ItemIndex := 0;
    psWhole:    Add.cbPathStorage.ItemIndex := 1;
    psRelative: Add.cbPathStorage.ItemIndex := 2;
  end;
  case Archiver1.CompressionLevel of
    clMaximum:   Add.cbComprLevel.ItemIndex := 0;
    clNormal:    Add.cbComprLevel.ItemIndex := 1;
    clFast:      Add.cbComprLevel.ItemIndex := 2;
    clSuperFast: Add.cbComprLevel.ItemIndex := 3;
    clNone:      Add.cbComprLevel.ItemIndex := 4;
  end;
  Add.cbComprLevel.Enabled := afCompressed in Archiver1.Header.ArchiveFlag;
  Add.cbFilterExtension.Checked := FFilterExtension;
  id := Add.ShowModal;
  if (id = mrOk) or (id = mrYes) then
    begin
      FAddToCurrentFolder := Add.cbAddToCurrentFolder.Checked;
      if Add.cbRecurse.Checked then
        Archiver1.Options := Archiver1.Options + [oRecurseFolders]
      else
        Archiver1.Options := Archiver1.Options - [oRecurseFolders];
      if Add.cbIncludeCurrent.Checked then
        Archiver1.Options := Archiver1.Options + [oIncludeStartingDirectory]
      else
        Archiver1.Options := Archiver1.Options - [oIncludeStartingDirectory];
      if Add.cbEmptyFolders.Checked then
        Archiver1.Options := Archiver1.Options + [oStoreEmptyFolders]
      else
        Archiver1.Options := Archiver1.Options - [oStoreEmptyFolders];
      if Add.cbCryptFiles.Checked then
        Archiver1.Options := Archiver1.Options + [oEncryptFiles]
      else
        Archiver1.Options := Archiver1.Options - [oEncryptFiles];
      case Add.cbPathStorage.ItemIndex of
        0: Archiver1.PathStorage := psNone;
        1: Archiver1.PathStorage := psWhole;
        2: Archiver1.PathStorage := psRelative;
      end;
      case Add.cbComprLevel.ItemIndex of
      0: Archiver1.CompressionLevel := clMaximum;
      1: Archiver1.CompressionLevel := clNormal;
      2: Archiver1.CompressionLevel := clFast;
      3: Archiver1.CompressionLevel := clSuperFast;
      4: Archiver1.CompressionLevel := clNone;
      end;
      FFilterExtension := Add.cbFilterExtension.Checked;
      if Pos( '*', Add.edName.Text ) > 0 then
        Archiver1.Filter := Add.edName.Text
      else
        Archiver1.Filter := '*.*';
      AddToLog( LoadStr(781) );
      if Pos( '*', Add.edName.Text ) > 0 then
        Archiver1.AddDirectory( Add.DirectoryListBox1.Directory )
      else
        Archiver1.AddFile( AppendSlash(Add.DirectoryListBox1.Directory) + Add.edName.Text );
    end;
end;

procedure TMain.StatusBar1Resize(Sender: TObject);
begin
  with Sender as TStatusBar do
    Panels.Items[0].Width := Width div 2;
end;

function TMain.SizeAsString( size : Comp ) : String;
begin
  if size < 1024 then
    begin
      if size > 1 then
        Result := Format( GetStr(sBytes), [size*1.0])
      else
        Result := Format( GetStr(sByte), [size*1.0])
    end
  else if size < 1024*1024 then
    Result := Format( GetStr(sKb), [size/1024])
  else
    Result := Format( GetStr(sMb), [size/(1024*1024)]);
end;

function TMain.SelectionAsString : String;

  function CalcSelSize : Integer;
  var
    i : Integer;
  begin
    Result := 0;
    with lbFiles do
      begin
        for i := 0 to Items.Count-1 do
          if Selected[i] then
            if oOpenSingleSegment in Archiver1.Options then
              Inc( Result, Archiver1.Files[ Integer(Items.Objects[i]) ].FileEntry.SegmentInfo.Size )
            else
              Inc( Result, Archiver1.Files[ Integer(Items.Objects[i]) ].FileEntry.ArchiveInfo.Size );
      end;
  end;

  function CalcDirSize : Integer;
  var
    i : Integer;
  begin
    Result := 0;
    with lbFiles do
      begin
        for i := 0 to Items.Count-1 do
          with Archiver1.Files[ Integer(Items.Objects[i]) ].FileEntry do
            if oOpenSingleSegment in Archiver1.Options then
              Inc( Result, SegmentInfo.Size )
            else
              Inc( Result, ArchiveInfo.Size );
      end;
  end;

begin
  with lbFiles do
    begin
      if SelCount > 0 then
        Result := Format( GetStr(sSelectedFiles),
                  [lbFiles.SelCount, SizeAsString(CalcSelSize)])
      else
        Result := Format( GetStr(sFiles),
                  [Items.Count, SizeAsString(CalcDirSize)]);
    end;
end;

procedure TMain.Delete1Click(Sender: TObject);
begin
  if CheckBusy then
    Exit;
  if IsArchiveEmpty then
    Exit;
  if lbFiles.SelCount > 0 then
    Delete.rgFiles.ItemIndex := 1
  else
    Delete.rgFiles.ItemIndex := 0;
  if Delete.ShowModal = mrOk then
    begin
      if Delete.rgFiles.ItemIndex = 0 then
        begin
          AddToLog( LoadStr(776) );
          Archiver1.Reset;
        end
      else
        begin
        AddToLog( LoadStr(783) );
        Archiver1.DeleteFiles;
        end;
    end;
end;

procedure TMain.Extract1Click(Sender: TObject);
begin
  if CheckBusy then
    Exit;
  if IsArchiveEmpty then
    Exit;
  case Archiver1.RestoreAction of
    raAsk:            Extract.cbRestore.ItemIndex := 3;
    raOverwrite:      Extract.cbRestore.ItemIndex := 0;
    raSkip:           Extract.cbRestore.ItemIndex := 1;
    raUpdate:         Extract.cbRestore.ItemIndex := 2;
    raExistingOnly:   Extract.cbRestore.ItemIndex := 4;
    raUpdateExisting: Extract.cbRestore.ItemIndex := 5;
  end;
  Extract.cbRestorePath.Checked := oRestorePath in Archiver1.Options;
  if lbFiles.SelCount > 0 then
    Extract.rgFiles.ItemIndex := 0
  else
    Extract.rgFiles.ItemIndex := 1;
  if Extract.ShowModal = mrOk then
    begin
      case Extract.cbRestore.ItemIndex of
        0: Archiver1.RestoreAction := raOverwrite;
        1: Archiver1.RestoreAction := raSkip;
        2: Archiver1.RestoreAction := raUpdate;
        3: Archiver1.RestoreAction := raAsk;
        4: Archiver1.RestoreAction := raExistingOnly;
        5: Archiver1.RestoreAction := raUpdateExisting;
      end;
      if Extract.cbRestorePath.Checked then
        Archiver1.Options := Archiver1.Options + [oRestorePath]
      else
        Archiver1.Options := Archiver1.Options - [oRestorePath];
      ForceDirectories( Extract.edExtractPath.Text );
      Archiver1.ExtractPath := Extract.edExtractPath.Text;
      AddToLog( LoadStr(782) );
      if Extract.rgFiles.ItemIndex = 0 then
        with lbFiles do
        begin
          if (SelCount = 1) and (ItemIndex >= 0) then
            with Archiver1.Files[ Integer(Items.Objects[ItemIndex]) ].FileEntry do
              Archiver1.ExtractFile( Segment, Offset, ArchiveInfo.CompressedSize )
          else
            Archiver1.ExtractFiles;
        end
      else
        Archiver1.ExtractFiles;
    end;
end;

procedure TMain.Archiver1DeleteFile(Sender: TObject;
  const FileEntry: TFileEntry; var Accept: Boolean);
begin
  case Delete.rgFiles.ItemIndex of
  1: // Selected files
    begin
    end;
  2: // pattern
    begin
      Accept := FilterIsTrue( Delete.edFiles.Text, ExtractFileName(FileEntry.Name) );
    end;
  end;
end;

procedure TMain.Selectall1Click(Sender: TObject);
var
  i : Integer;
begin
  with lbFiles do
    begin
      Items.BeginUpdate;
      for i := 0 to Items.Count - 1 do
        Selected[i] := True;
      Items.EndUpdate;
    end;
  CalcSelSize;
end;

function TMain.FilterIsTrue( const filter, searchValue : String ) : Boolean;

  procedure ParseValue(Text: String; List: TStringList);
  var
    Temp: String;
    i: Integer;
  begin
    i := 1;
    while i <= Length(Text) do
    begin
      if (Text[i] = '*') {and (i < Length(Text)) and (Text[i+1] = '.')} then
      begin
        if Length(Temp) > 0 then
          List.Add(Temp);
        Temp := '';
        List.Add('*');
        //Inc(i);
      end else
      if (Text[i] = '?') then
      begin
        if Length(Temp) > 0 then
          List.Add(Temp);
        Temp := '';
        List.Add('?')
      end else
        Temp := Temp + Text[i];
      Inc(i);
    end;
    if Length(Temp) > 0 then
      List.Add(Temp);
  end;

const
  ExactMatch = False;
var
  P, Tmp: PChar;
  Text, Value: String;
  i: Integer;
  FindAnywhere: Boolean;
  List: TStringList;
begin
  if ((Pos('*',Filter) > 0) or (Pos('?',Filter) > 0)) then
  begin
    List := TStringList.Create;
    try
      Result := False;
      Value := AnsiUpperCase(Filter);
      ParseValue(Value, List);
      Text := AnsiUpperCase(SearchValue);
          if ExactMatch or (List.Count = 1) then
          begin
            if ExactMatch then
              Result := AnsiCompareStr(Value,Text) = 0 else
              Result := Pos(Value,Text) = 1;
          end else
          begin
            P := PChar(Text);
            FindAnywhere := False;
            for i := 0 to List.Count - 1 do
            begin
              if List[i] = '*' then
                FindAnywhere := True else
              if List[i] = '?' then
              begin
                Inc(P);
                FindAnywhere := False;
              end else
              begin
                Tmp := StrPos(P,PChar(List[i]));
                if Tmp = nil then
                  break else
                if not FindAnywhere and (Tmp <> P) then
                  break else
                begin
                  P := Tmp;
                  Inc(P,StrLen(PChar(List[i])));
                end;
                FindAnywhere := False;
              end;
              if i = List.Count - 1 then Result := True;
            end;
          end;
    finally
      List.Free;
    end;
  end
  else
    Result := AnsiCompareStr( Filter, SearchValue ) = 0;
end;

procedure TMain.Information1Click(Sender: TObject);
var
  dt : TDateTime;
  tmp : TDataInfo;
  ratio : Integer;
begin
  if CheckBusy then
    Exit;
  with Information, Archiver1 do
    begin
      if oOpenSingleSegment in Options then
        tmp := Header.SegmentInfo
      else
        tmp := Header.ArchiveInfo;
      lPath.Caption := AdjustPath( ExtractFilePath( FileName ), 50);
      lName.Caption := ExtractFileName( FileName );
      lFiles.Caption := IntToStr( tmp.FileCount );
      cbEncrypted.Checked := afCrypted in Archiver1.Header.ArchiveFlag;
      cbCompressed.Checked := afCompressed in Archiver1.Header.ArchiveFlag;
      cbSolid.Checked := afSolid in Archiver1.Header.ArchiveFlag;
      cbReadOnly.Checked := afReadOnly in Archiver1.Header.ArchiveFlag;
      cbFinalSegment.Checked := afFinalSegment in Archiver1.Header.ArchiveFlag;
      if IsSolidArchive then
        begin
          lSize.Caption := GetStr( sNotAvailable );
          lCompression.Caption := GetStr( sNotAvailable );
          lDate.Caption := GetStr( sNotAvailable );
          lSegNum.Visible := False;
          Label8.Visible := False;
          cbSolid.Checked := True;
          lPath.Caption := ArchiveDrive+ArchiveDir;
          lName.Caption := ArchiveName+ArchiveExt;
        end
      else
        begin
          with Stream as THandleStream do
            lSize.Caption := Format('%.0n',[Size*1.0]);
          if tmp.Size > 0 then
            begin
              ratio := 100-Round(tmp.CompressedSize / tmp.Size * 100);
              if ratio < 0 then
                ratio := 0;
            end
          else
            ratio := 0;
          lCompression.Caption := IntToStr(ratio)+'%';
          with Stream as THandleStream do
            dt := FileDateToDateTime( FileGetDate(Handle) );
          lDate.Caption := DateTimeToStr( dt );
          lSegNum.Caption := Format( '#%d', [Header.Segment]);
          lSegNum.Visible := IsSegmented;
          Label8.Visible := lSegNum.Visible;
        end;
      ShowModal;
    end;
end;

procedure TMain.Configuration1Click(Sender: TObject);
begin
  if CheckBusy then
    Exit;
  Configuration.cbCrypt.Checked := oCrypt in Archiver1.Options;
  Configuration.cbCompress.Checked := oCompress in Archiver1.Options;
  Configuration.cbSolid.Checked := oCreateSolidArchives in Archiver1.Options;
  Configuration.cbShowEmpty.Checked := oShowEmptyFolders in Archiver1.Options;
  Configuration.cbReadOnly.Checked := oCreateReadOnly in Archiver1.Options;
  Configuration.edBlockSize.Text := IntToStr(Archiver1.BlockSize div 1024);
  Configuration.edReserveSpace.Text := IntToStr(Archiver1.ReserveSpace div 1024);
  Configuration.cbWriteSFXCode.Checked := oWriteSFXCode in Archiver1.Options;
  Configuration.cbLanguage.ItemIndex := Integer(Archiver1.Language);
  Configuration.cbShowTreeView.Checked := FShowTreeView;
  Configuration.cbSplit.Enabled := not (Archiver1.IsOpen and Archiver1.IsSolidArchive);
  Configuration.rgMaxSize.Enabled := not (Archiver1.IsOpen and Archiver1.IsSolidArchive);
  Configuration.edMaxSize.Enabled := not (Archiver1.IsOpen and Archiver1.IsSolidArchive);
  case Archiver1.CompressionLevel of
    clMaximum:   Configuration.cbComprLevel.ItemIndex := 0;
    clNormal:    Configuration.cbComprLevel.ItemIndex := 1;
    clFast:      Configuration.cbComprLevel.ItemIndex := 2;
    clSuperFast: Configuration.cbComprLevel.ItemIndex := 3;
    clNone:      Configuration.cbComprLevel.ItemIndex := 4;
  end;
  if Configuration.ShowModal = mrOk then
    begin
      if StrToIntDef( Configuration.edBlockSize.Text, 0 ) >= 1 then
        Archiver1.BlockSize := StrToIntDef( Configuration.edBlockSize.Text, 0 ) * 1024;
      if StrToIntDef( Configuration.edReserveSpace.Text, 0 ) >= 1 then
        Archiver1.ReserveSpace := StrToIntDef( Configuration.edReserveSpace.Text, 0 ) * 1024;
      if Configuration.cbSplit.Checked then
        case Configuration.rgMaxSize.ItemIndex of
        0: Archiver1.MaxSegmentSize := 710 * 1024;
        1: Archiver1.MaxSegmentSize := 1400 * 1024;
        else
          Archiver1.MaxSegmentSize := StrToIntDef(Configuration.edMaxSize.Text, 0)*1024;
        end
      else
        Archiver1.MaxSegmentSize := 0;
      if Configuration.cbCrypt.Checked then
        Archiver1.Options := Archiver1.Options + [oCrypt]
      else
        Archiver1.Options := Archiver1.Options - [oCrypt];
      if Configuration.cbCompress.Checked then
        Archiver1.Options := Archiver1.Options + [oCompress]
      else
        Archiver1.Options := Archiver1.Options - [oCompress];
      if Configuration.cbSolid.Checked then
        Archiver1.Options := Archiver1.Options + [oCreateSolidArchives]
      else
        Archiver1.Options := Archiver1.Options - [oCreateSolidArchives];
      if Configuration.cbShowEmpty.Checked then
        Archiver1.Options := Archiver1.Options + [oShowEmptyFolders]
      else
        Archiver1.Options := Archiver1.Options - [oShowEmptyFolders];
      if Configuration.cbReadOnly.Checked then
        Archiver1.Options := Archiver1.Options + [oCreateReadOnly]
      else
        Archiver1.Options := Archiver1.Options - [oCreateReadOnly];
      if Configuration.cbWriteSFXCode.Checked then
        Archiver1.Options := Archiver1.Options + [oWriteSFXCode]
      else
        Archiver1.Options := Archiver1.Options - [oWriteSFXCode];
      case Configuration.cbComprLevel.ItemIndex of
      0: Archiver1.CompressionLevel := clMaximum;
      1: Archiver1.CompressionLevel := clNormal;
      2: Archiver1.CompressionLevel := clFast;
      3: Archiver1.CompressionLevel := clSuperFast;
      4: Archiver1.CompressionLevel := clNone;
      end;
      if TLanguage(Configuration.cbLanguage.ItemIndex) <> Archiver1.Language then
        begin
          Archiver1.Language := TLanguage(Configuration.cbLanguage.ItemIndex);
          TranslateTo( Archiver1.Language );
          ShowArchiveSize;
          DisplayHint(Self);
        end;
      if Configuration.cbShowTreeView.Checked <> FShowTreeView then
        begin
          ShowTreeView := Configuration.cbShowTreeView.Checked;
        end;
    end;
end;


procedure TMain.Archiver1FileAdded(Sender: TObject;
  const FileEntry: TFileEntry);
begin
  AddFileToGrid( FileEntry, Archiver1.FileCount - 1 );
  CheckIfInstall( FileEntry, Archiver1.FileCount - 1 );
end;

procedure TMain.btnAbortClick(Sender: TObject);
begin
  Archiver1.RequestAbort;
end;

procedure TMain.Opensegment1Click(Sender: TObject);
begin
  if CheckBusy then
    Exit;
  with OpenDialog1 do
    begin
      InitialDir := ExtractFilePath( SaveDialog1.FileName );
      Title := GetStr( sOpenSegment );
      if Execute then
        begin
          if (FileName = Archiver1.FileName) and Archiver1.IsOpen then
            Exit;
          Archiver1.Close;
          Archiver1.Options := Archiver1.Options + [oOpenSingleSegment];
          Archiver1.FileName := FileName;
          Archiver1.Open;
        end;
    end;
end;

procedure TMain.UpdateActionMenu;
begin
  tbAdd.Enabled := Archiver1.IsOpen;
  tbExtract.Enabled := Archiver1.IsOpen and not IsArchiveEmpty;
  tbDelete.Enabled := tbExtract.Enabled;
  tbView.Enabled := tbExtract.Enabled;
  Add1.Enabled := tbAdd.Enabled;
  Extract1.Enabled := tbExtract.Enabled;
  Delete1.Enabled := tbDelete.Enabled;
  View1.Enabled := tbExtract.Enabled;
  Selectall1.Enabled := tbExtract.Enabled;
  SelectNone1.Enabled := tbExtract.Enabled;
  InvertSelection1.Enabled := tbExtract.Enabled;
  Selectall2.Enabled := tbExtract.Enabled;
  SelectNone2.Enabled := tbExtract.Enabled;
  InvertSelection2.Enabled := tbExtract.Enabled;
  MakeEXEfile1.Enabled := tbAdd.Enabled and (not Archiver1.IsSegmented) and
                          (not IsExeFile(Archiver1.FileName));
  SetArchiveComment1.Enabled := tbAdd.Enabled and not tbExtract.Enabled;
  Checkintegrity1.Enabled := tbExtract.Enabled;
end;

function TMain.IsArchiveEmpty : Boolean;
begin
  with Archiver1.Header do
    Result := (ArchiveInfo.FileCount + SegmentInfo.FileCount) = 0;
end;

procedure RegisterFileType( ft, key, desc, icon, prg : String );
var
  myreg : TRegIniFile;
  ct : Integer;
begin
  // make a correct file-extension
  ct := pos('.',ft);
  while ct > 0 do begin
        System.Delete(ft,ct,1);
        ct := pos('.',ft);
  end;
  if (ft = '') or (prg = '') then exit; //not a valid file-ext or ass. app
  ft := '.'+ft;
  myreg := TRegIniFile.Create('');
  try
     myreg.rootkey := hkey_classes_root; // where all file-types are described
     if key = '' then key := copy(ft,2,maxint)+'_auto_file'; // if no key-name is given,
                                                          // create one
     myreg.writestring(ft,'',key); // set a pointer to the description-key
     myreg.writestring(key,'',desc); // write the description
     if icon <> '' then
        myreg.writestring(key+'\DefaultIcon','',icon); // write the def-icon if given
     myreg.writestring(key+'\shell\open\command','',prg+' "%1"'); //association
  finally
     myreg.Free;
  end;
end;

procedure TMain.Timer1Timer(Sender: TObject);
begin
  with Sender as TTimer do
    Enabled := False;
  if ParamCount > 0 then
    begin
      Archiver1.FileName := GetFullFileName( ParamStr(1) );
      Archiver1.Open;
    end;
end;

procedure TMain.Archiver1AfterOpen(Sender: TObject);
begin
  UpdateCaption;
  with Archiver1 do
    RemoveLastFile( ArchiveDrive + ArchiveDir + ArchiveName + ArchiveExt );
end;

procedure TMain.Archiver1AfterClose(Sender: TObject);
begin
  ClearInstallRef;   
  Resetarchive1.Enabled := False;
  Deletearchive1.Enabled := False;
  Closearchive1.Enabled := False;
  Information1.Enabled := False;
  Rename1.Enabled := False;
  Caption := FCaption;
  Application.Title := FTitle;
  StatusBar1.SimplePanel := True;
  DisplayMessage( GetStr( sDefaultMsg ) );
  Archiver1.FileName := '';
  UpdateActionMenu;
  RecordFile( FLastFileName );
  DeleteTempDropedFiles;
end;

function TMain.GetFullFileName( const FileName : String ) : String;
var
  rec : TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, rec) = 0 then
    begin
      Result := ExtractFilePath(FileName)+rec.Name;
      FindClose(rec);
    end
  else
    Result := FileName;
end;

procedure TMain.Archiver1EnterCryptKey(Sender: TObject; var Key: String);
var
  tmp, tmp2 : String;
begin
  with EnterCryptKey, Archiver1 do
    begin
      repeat
        tmp := '';
        edKey.Text := '';
        lPrompt.Caption := Messages.EnterCryptKey;
        if ShowModal = mrOk then
          tmp := edKey.Text
        else
          Abort;
        edKey.Text := '';
        lPrompt.Caption := Messages.ConfirmCryptKey;
        if ShowModal = mrOk then
          tmp2 := edKey.Text
        else
          Abort;
        if tmp <> tmp2 then
          begin
            MessageDlg( Messages.KeyNotConfirmed, mtError, [mbOk], 0 );
          end;
      until (tmp = tmp2);
      Key := tmp;
    end;
end;

procedure TMain.Archiver1RequestCryptKey(Sender: TObject; var Key: String);
begin
  with EnterCryptKey, Archiver1 do
    begin
      edKey.Text := Key;
      lPrompt.Caption := Messages.EnterDecryptKey;
      if ShowModal = mrOk then
        Key := edKey.Text
      else
        Abort;
    end;
end;

procedure TMain.Rename1Click(Sender: TObject);
var
  NewName : String;
begin
  if CheckBusy then
    Exit;
  NewName := Archiver1.FileName;
  if InputQuery( Archiver1.Messages.SystemMessage, GetStr(sRenameTo), NewName ) then
    begin
      if Archiver1.Rename( NewName ) then
        UpdateCaption
      else
        MessageDlg( Format( GetStr(sCouldNotRename), [NewName]), mtError, [mbOk], 0 );
    end;
end;

procedure TMain.SFXConfiguration1Click(Sender: TObject);
begin
  if CheckBusy then
    Exit;
  SFXConfig.Caption := GetStr(sSFXConfig);
  SFXConfig.btnOk.Caption := GetStr(sOk);
  SFXConfig.ShowModal;
end;

procedure TMain.MakeEXEfile1Click(Sender: TObject);
begin
  if CheckBusy then
    Exit;
  SFXConfig.Caption := GetStr(sMakeSFX);
  SFXConfig.btnOk.Caption := GetStr(sMake);
  if SFXConfig.ShowModal = mrOk then
    begin
      if not Archiver1.MakeSFX then
        MessageDlg( GetStr(sCouldNotMakeSFX), mtError, [mbOk], 0 );
    end;
end;

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not CheckBusy;
  if not CanClose then
    Exit;
  Archiver1.Close;
  SaveToRegistry;
end;

procedure TMain.SetArchiveComment1Click(Sender: TObject);
const
  kTag = '<SFXCommentEnd>';
var
  tmp : String;
  idx : Integer;
begin
  if CheckBusy then
    Exit;
  tmp := Archiver1.Header.Comment;
  idx := Pos( kTag, tmp );
  if idx > 0 then
    begin
      System.Delete( tmp, 1, idx + Length(kTag) );
      ArchComment.Comment.Text := tmp;
    end
  else
    ArchComment.Comment.Text := fmArchComment.Comment;
  ArchComment.Caption := GetStr(sSetArchiveComment);
  ArchComment.btnCancel.Visible := True;
  if ArchComment.ShowModal = mrOk then
    begin
      fmArchComment.Comment := ArchComment.Comment.Text;
      if idx > 0 then
        begin
          tmp := Archiver1.Header.Comment;
          System.Delete( tmp, idx+Length(kTag), Length(tmp) );
        end
      else
        tmp := '';
      Archiver1.SetArchiveComment( tmp + ArchComment.Comment.Text );
    end;
end;

procedure TMain.DisplayComment( const comment : String );
begin
  if Comment = '' then
    Exit;
  ArchComment.Caption := GetStr(sArchiveComment);
  ArchComment.Comment.Text := comment;
  ArchComment.btnCancel.Visible := False;
  ArchComment.ShowModal;
end;

function TMain.CheckBusy : Boolean;
begin
  if Archiver1.IsBusy then
    begin
      Result := True;
      MessageDlg( GetStr(sBusy), mtInformation, [mbOk], 0 );
    end
  else if gExecuting then
    begin
      Result := True;
      MessageDlg( GetStr(sFileExecuted), mtInformation, [mbOk], 0 );
    end
  else
    Result := False;
end;

procedure TMain.Archiver1BeforeClose(Sender: TObject);
begin
  with Sender as TArchiver do
    begin
      FLastFileName := ArchiveDrive + ArchiveDir + ArchiveName + ArchiveExt;
    end;
  DisplayMessage( GetStr(sClosingArchive) );
end;

procedure TMain.Archiver1ShowComment(Sender: TObject;
  const Comment: String);
begin
  DisplayComment( Comment );
end;

procedure TMain.Archiver1ShowTiming(Sender: TObject; ElapsedTime,
  RemainingTime: TDateTime);
begin
  if not Timing.Visible and (TimeAsMSecs(Archiver1.ElapsedTime ) > 2*1000) and
     (Archiver1.Percent <= 40) then
    Timing.Show;
  Timing.lElapsed.Caption := TimeToStr( ElapsedTime );
  Timing.lRemaining.Caption := TimeToStr( RemainingTime );
end;

procedure TMain.WMDROPFILES(var Message: TWMDROPFILES);
var
  NumFiles : longint;
  i : longint;
  buffer : array[0..2048] of char;
  id : Integer;
begin
  if CheckBusy then
    begin
      DragFinish( Message.Drop );
      Exit;
    end;
  {How many files are being dropped}
  {$IFDEF D4}
  NumFiles := DragQueryFile(Message.Drop, $FFFFFFFF, nil, 0);
  {$ELSE}
  NumFiles := DragQueryFile(Message.Drop, -1, nil, 0);
  {$ENDIF}
  AddDropedFiles.lbFiles.Clear;
  {Accept the dropped files}
  for i := 0 to (NumFiles - 1) do
    begin
      DragQueryFile( Message.Drop,
                     i,
                     @buffer,
                     sizeof(buffer));
      AddDropedFiles.lbFiles.Items.Add( buffer );
    end;
  DragFinish( Message.Drop );
  Application.BringToFront;
  // If there's no opened archive, then try to find an archive
  // in the droped files and open it
  if not Archiver1.IsOpen then
    begin
      with AddDropedFiles.lbFiles.Items do
        for i := 0 to Count - 1 do
          if CompareText( ExtractFileExt(Strings[i]), kDefaultExt ) = 0 then
            begin
              Archiver1.Options := Archiver1.Options - [oOpenSingleSegment];
              Archiver1.FileName := Copy( Strings[i], 1, length(Strings[i]) );
              Archiver1.Open;
              Break;
            end;
      if not Archiver1.IsOpen then
        MessageDlg( GetStr(sOpenOrCreate), mtInformation, [mbOk], 0 );
      DragFinish( Message.Drop );
      Exit;
    end;
  // Prepare dialog box
  AddDropedFiles.cbFilterExtension.Checked := FFilterExtension;
  AddDropedFiles.cbRecurse.Checked := oRecurseFolders in Archiver1.Options;
  AddDropedFiles.cbIncludeCurrent.Checked := oIncludeStartingDirectory in Archiver1.Options;
  AddDropedFiles.cbEmptyFolders.Checked := oStoreEmptyFolders in Archiver1.Options;
  AddDropedFiles.cbCryptFiles.Checked := oEncryptFiles in Archiver1.Options;
  AddDropedFiles.cbCryptFiles.Enabled := not(afCrypted in Archiver1.Header.ArchiveFlag);
  case Archiver1.PathStorage of
    psNone:     AddDropedFiles.cbPathStorage.ItemIndex := 0;
    psWhole:    AddDropedFiles.cbPathStorage.ItemIndex := 1;
    psRelative: AddDropedFiles.cbPathStorage.ItemIndex := 2;
  end;
  case Archiver1.CompressionLevel of
    clMaximum:   AddDropedFiles.cbComprLevel.ItemIndex := 0;
    clNormal:    AddDropedFiles.cbComprLevel.ItemIndex := 1;
    clFast:      AddDropedFiles.cbComprLevel.ItemIndex := 2;
    clSuperFast: AddDropedFiles.cbComprLevel.ItemIndex := 3;
    clNone:      AddDropedFiles.cbComprLevel.ItemIndex := 4;
  end;
  AddDropedFiles.cbComprLevel.Enabled := afCompressed in Archiver1.Header.ArchiveFlag;
  AddDropedFiles.cbAddToCurrentFolder.Visible := tvDirectory.Visible;
  AddDropedFiles.cbAddToCurrentFolder.Checked := FAddToCurrentFolder;
  id := AddDropedFiles.ShowModal;
  if (id = mrOk) or (id = mrYes) then
    begin
      FFilterExtension := AddDropedFiles.cbFilterExtension.Checked;
      if AddDropedFiles.cbRecurse.Checked then
        Archiver1.Options := Archiver1.Options + [oRecurseFolders]
      else
        Archiver1.Options := Archiver1.Options - [oRecurseFolders];
      if AddDropedFiles.cbIncludeCurrent.Checked then
        Archiver1.Options := Archiver1.Options + [oIncludeStartingDirectory]
      else
        Archiver1.Options := Archiver1.Options - [oIncludeStartingDirectory];
      if AddDropedFiles.cbEmptyFolders.Checked then
        Archiver1.Options := Archiver1.Options + [oStoreEmptyFolders]
      else
        Archiver1.Options := Archiver1.Options - [oStoreEmptyFolders];
      if AddDropedFiles.cbCryptFiles.Checked then
        Archiver1.Options := Archiver1.Options + [oEncryptFiles]
      else
        Archiver1.Options := Archiver1.Options - [oEncryptFiles];
      case AddDropedFiles.cbPathStorage.ItemIndex of
        0: Archiver1.PathStorage := psNone;
        1: Archiver1.PathStorage := psWhole;
        2: Archiver1.PathStorage := psRelative;
      end;
      case AddDropedFiles.cbComprLevel.ItemIndex of
      0: Archiver1.CompressionLevel := clMaximum;
      1: Archiver1.CompressionLevel := clNormal;
      2: Archiver1.CompressionLevel := clFast;
      3: Archiver1.CompressionLevel := clSuperFast;
      4: Archiver1.CompressionLevel := clNone;
      end;
      if Pos( '*', AddDropedFiles.edName.Text ) > 0 then
        Archiver1.Filter := AddDropedFiles.edName.Text
      else
        Archiver1.Filter := '*.*';
      FAddToCurrentFolder := AddDropedFiles.cbAddToCurrentFolder.Checked;
      Archiver1.AddFiles( AddDropedFiles.lbFiles.Items );
    end;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  FExtIcons.Free;
  FFilesDroped.Free;
  FFilters.Free;
  DragAcceptFiles( Handle, False);
end;

function TMain.GetRegistryKey : String;
begin
  Result := '\Software\'+Application.Title;
end;

procedure TMain.SaveToRegistry;

  procedure SaveFormPos( Registry : TRegistry; const name : String; form : TForm );
  begin
    with Registry do
      if OpenKey( GetRegistryKey+'\'+name, True) then
        begin
          WriteBool( 'Maximized', (form.WindowState = wsMaximized) );
          WriteFloat( 'Left', form.Left / Screen.Width );
          WriteFloat( 'Top', form.Top / Screen.Height );
          WriteFloat( 'Width', form.Width / Screen.Width );
          WriteFloat( 'Height', form.Height / Screen.Height );
          CloseKey;
        end;
  end;

  procedure SaveFont( Registry : TRegistry; const name : String; font : TFont );
  begin
    with Registry do
      if OpenKey( GetRegistryKey+'\'+name, True) then
        begin
          WriteString( 'Name', font.Name );
          WriteInteger( 'Size', font.Size );
          WriteBool( 'Bold', fsBold in font.Style );
          WriteBool( 'Italic', fsItalic in font.Style );
          WriteBool( 'Underline', fsUnderline in font.Style );
          WriteBool( 'StrikeOut', fsStrikeOut in font.Style );
          CloseKey;
        end;
  end;

var
  Registry  : TRegistry;
  i : Integer;
begin
  Registry:=TRegistry.Create;
  with Registry do
    begin
      try
        RootKey:=HKEY_CURRENT_USER;
        SaveFormPos( Registry, 'Main', Self );
        SaveFormPos( Registry, 'TextViewer', TextViewer );
        SaveFont( Registry, 'TextViewerFont', TextViewer.RichEdit1.Font );
        SaveFont( Registry, 'MainFont', lbFiles.Font );
        if OpenKey( GetRegistryKey, True) then
          begin
            WriteBool( 'ShowEmptyFolders', oShowEmptyFolders in Archiver1.Options );
            WriteInteger( 'ViewMode', View.rgUsing.ItemIndex );
            WriteInteger( 'Language', Integer(Archiver1.Language) );
            for i := 0 to hcFiles.Sections.Count - 1 do
              WriteInteger( Format('ColumnWidth%d', [i]), hcFiles.Sections[i].Width );
            WriteInteger( 'DirectoryWidth', tvDirectory.Width );
            WriteBool( 'ShowTreeView', ShowTreeView );
            WriteBool( 'Ascending', FAscending );
            WriteInteger( 'SortedColumn', FSortedColumn );
            WriteBool( 'FilterExtensions', FFilterExtension );
            WriteInteger( 'KindOfFilter', FKindOfFilter );
            WriteString( 'Filters', FFilters.Text );
          end;
        CloseKey;
        WriteLastFilesToRegistry( Registry );
      finally
        Free;
      end;
    end;
end;

procedure TMain.LoadFromRegistry;

  procedure CheckFormPos( form : TForm );
  begin
    if form.Left < 10 then
      form.Left := 10;
    if form.Top < 10 then
      form.Top := 10;
    if form.Left > Screen.Width-200 then
      form.Left := Screen.Width-200;
    if form.Top > Screen.Height-150 then
      form.Top := Screen.Height-150;
  end;

  procedure CenterForm( form : TForm );
  begin
    form.Left := (Screen.Width-form.Width) div 2;
    form.Top  := (Screen.Height-form.Height) div 2;
  end;

  procedure LoadFormPos( Registry : TRegistry; const name : String; form : TForm );
  var
    x, y, w, h : Integer;
  begin
    with Registry do
      if OpenKey(GetRegistryKey+'\'+name, True) then
        begin
          if ValueExists( 'Maximized' ) and
             ValueExists( 'Left' ) and
             ValueExists( 'Top' ) and
             ValueExists( 'Width' ) and
             ValueExists( 'Height' )
          then
            begin
              if ReadBool( 'Maximized' ) then
                WindowState := wsMaximized
              else
                begin
                  WindowState := wsNormal;
                  x := Round(ReadFloat( 'Left' ) * Screen.Width);
                  y := Round(ReadFloat( 'Top' ) * Screen.Height);
                  w := Round(ReadFloat( 'Width' ) * Screen.Width);
                  h := Round(ReadFloat( 'Height' ) * Screen.Height);
                  form.SetBounds( x, y, w, h );
                end;
            end
          else
            CenterForm( form );
          CloseKey;
        end;
    CheckFormPos( form );
  end;

  procedure LoadFont( Registry : TRegistry; const name : String; font : TFont );
  begin
    with Registry do
      if OpenKey( GetRegistryKey+'\'+name, True) then
        begin
          if ValueExists('Name') then
            font.Name := ReadString( 'Name' );
          if ValueExists('Size') then
            font.Size := ReadInteger( 'Size' );
          font.Style := [];
          if ValueExists('Bold') and ReadBool( 'Bold' ) then
            font.Style := font.Style + [fsBold];
          if ValueExists('Italic') and ReadBool( 'Italic' ) then
            font.Style := font.Style + [fsItalic];
          if ValueExists('Underline') and ReadBool( 'Underline' ) then
            font.Style := font.Style + [fsUnderline];
          if ValueExists('StrikeOut') and ReadBool( 'StrikeOut' ) then
            font.Style := font.Style + [fsStrikeOut];
          CloseKey;
        end;
  end;

var
  Registry  : TRegistry;
  i : Integer;
begin
  Registry:=TRegistry.Create;
  with Registry do
    begin
      try
        RootKey:=HKEY_CURRENT_USER;
        LoadFormPos( Registry, 'Main', Self );
        LoadFormPos( Registry, 'TextViewer', TextViewer );
        LoadFormPos( Registry, 'TextViewer', LastOutput );
        LoadFont( Registry, 'MainFont', lbFiles.Font );
        tvDirectory.Font.Assign( lbFiles.Font );
        LoadFont( Registry, 'TextViewerFont', TextViewer.RichEdit1.Font );
        LastOutput.RichEdit1.Font.Assign( TextViewer.RichEdit1.Font );
        if OpenKey( GetRegistryKey, True) then
          begin
            if ValueExists( 'ShowEmptyFolders' ) then
              if ReadBool( 'ShowEmptyFolders' ) then
                Archiver1.Options := Archiver1.Options + [oShowEmptyFolders]
              else
                Archiver1.Options := Archiver1.Options - [oShowEmptyFolders];
            if ValueExists( 'ViewMode' ) then
              View.rgUsing.ItemIndex := ReadInteger( 'ViewMode' );
            if ValueExists( 'Language' ) then
              Archiver1.Language := TLanguage(ReadInteger( 'Language' ));
            for i := 0 to hcFiles.Sections.Count - 1 do
              if ValueExists( Format('ColumnWidth%d', [i]) ) then
                hcFiles.Sections[i].Width := ReadInteger( Format('ColumnWidth%d', [i]) );
            if ValueExists( 'DirectoryWidth' ) then
              tvDirectory.Width := ReadInteger( 'DirectoryWidth' );
            if ValueExists( 'ShowTreeView' ) then
              ShowTreeView := ReadBool( 'ShowTreeView' );
            if ValueExists( 'SortedColumn' ) then
              SelectSortCol( ReadInteger( 'SortedColumn' ) );
            if ValueExists( 'Ascending' ) and not ReadBool( 'Ascending' ) then
              SelectSortCol( ReadInteger( 'SortedColumn' ) );
            if ValueExists( 'FilterExtensions' ) then
              FFilterExtension := ReadBool( 'FilterExtensions' );
            if ValueExists( 'KindOfFilter' ) then
              FKindOfFilter := ReadInteger( 'KindOfFilter' );
            if ValueExists( 'Filters' ) then
              FFilters.Text := ReadString( 'Filters' );
          end;
        CloseKey;
        ReadLastFilesFromRegistry( Registry );
      finally
        Free;
      end;
    end;
end;

procedure TMain.FormShow(Sender: TObject);
begin
  LoadFromRegistry;
  InitXXForms;
  TranslateTo( Archiver1.Language );
end;

procedure TMain.View1Click(Sender: TObject);
begin
  ExecuteFile( True );
end;

function AddQuotes (const S: String): String;
{ Adds a quote (") character to the left and right sides of the string if
  the string contains a space and it didn't have quotes already. This is
  primarily used when spawning another process with a long filename as one of
  the parameters. }
begin
  Result := Trim(S);
  if (Pos(' ', Result) <> 0) and
     ((Result[1] <> '"') or (Result[Length(Result)] <> '"')) then
    Result := '"' + Result + '"';
end;

function fileExec(const aCmdLine: String; aHide, aWait: Boolean): Boolean;
var
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
begin
  {setup the startup information for the application }
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
  begin
    cb:= SizeOf(TStartupInfo);
    dwFlags:= STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    if aHide then wShowWindow:= SW_HIDE
             else wShowWindow:= SW_SHOWNORMAL;
  end;

  Result := CreateProcess(nil,PChar(aCmdLine), nil, nil, False,
               NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo);
  if aWait then
     if Result then
     begin
       WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
       WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
     end;
end;

function ExecAndWait (const Filename, Parms: String) : Boolean;
var
  CmdLine: String;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  Msg: TMsg;
begin
  gExecuting := False;
  CmdLine := AddQuotes(Filename) + ' ' + AddQuotes(Parms);

  FillChar (StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  if not CreateProcess(nil, PChar(CmdLine), nil, nil, False, 0, nil, nil,
     StartupInfo, ProcessInfo) then
    begin
      Result := False;
      Exit;
    end;
  with ProcessInfo do begin
    { Don't need the thread handle, so close it now }
    CloseHandle (hThread);
    { Wait until the process returns. Uses MsgWaitForMultipleObjects
      because it has to check the message queue so the "feedback"
      cursor doesn't stay on. }
    gExecuting := True;
    repeat
      while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do begin
        TranslateMessage (Msg);
        DispatchMessage (Msg);
      end;
    until MsgWaitForMultipleObjects(1, hProcess, False, INFINITE,
      QS_ALLINPUT) <> WAIT_OBJECT_0+1;
    { Then close the process handle }
    CloseHandle (hProcess);
    gExecuting := False;
  end;
  Result := True;
end;

function FindExec( const FileName : String; var Exe : String ) : Boolean;
begin
  SetLength( Exe, 512 );
  Exe[1] := #0;
  if FindExecutable( PChar(FileName), nil, PChar(Exe) ) > 32 then
    begin
      SetLength( exe, strlen(PChar(exe)) );
      Result := True;
    end
  else
    Result := False;
end;

procedure TMain.ExecuteFile( ask : Boolean );
var
  opt : TOptions;
  tmp, exe : String;
  exec : Boolean;
  idx : Integer;
begin
  if CheckBusy then
    Exit;
  if IsArchiveEmpty then
    Exit;
  if lbFiles.SelCount = 0 then
    Exit;
  // Try to extract the file to the temp directory
  opt := Archiver1.Options;
  try
    Archiver1.Options := Archiver1.Options - [oRestorePath];
    with lbFiles do
    begin
      if {(SelCount = 1) and} (ItemIndex >= 0) then
        begin
          idx := Integer(Items.Objects[ItemIndex]);
          tmp := GetTempDir + ExtractFileName( Archiver1.Files[idx].FileEntry.Name );
          try
            with Archiver1.Files[ Integer(Items.Objects[ItemIndex]) ].FileEntry do
              Archiver1.ExtractFileTo( Segment, Offset, ArchiveInfo.CompressedSize, tmp );
            // Execute it
            if IsExeFile( tmp ) then
              begin
                ExecAndWait( tmp, '' );
              end
            else
              begin
                if ask then
                  begin
                    View.FileName := ExtractFileName(tmp);
                    if not FindExec( tmp, exe ) then
                      exe := '';
                    View.Exe := ExtractFileName(exe);
                    if View.ShowModal = mrOk then
                      exec := View.rgUsing.ItemIndex = 0
                    else
                      Exit;
                  end
                else
                  exec := True;
                if exec then
                  begin
                    if FindExec( tmp, exe ) then
                      ExecAndWait( exe, tmp )
                    else
                      MessageDlg( Format( GetStr(sNoAssociation), [tmp]), mtError, [mbOk], 0 );
                  end
                else
                  begin
                    TextViewer.RichEdit1.Lines.LoadFromFile( tmp );
                    TextViewer.FileName := ExtractFileName(tmp);
                    TextViewer.ShowModal;
                  end;
              end;
            Application.BringToFront;
          finally
            // Remove ReadOnly, Hidden, System attributes
            // Otherwise we can't delete the file
            FileSetAttr( tmp, FileGetAttr(tmp) and (faVolumeID or faDirectory) );
            DeleteFile( tmp );
          end;
        end;
    end
  finally
    Archiver1.Options := opt;
  end;
end;

procedure TMain.WMTranslate( var Message : TMessage );
begin
  with hcFiles do
    begin
      Sections[0].Text := GetStr(sName);
      Sections[1].Text := GetStr(sDate);
      Sections[2].Text := GetStr(sTime);
      Sections[3].Text := GetStr(sSize);
      Sections[4].Text := GetStr(sRatio);
      Sections[5].Text := GetStr(sPacked);
      Sections[6].Text := GetStr(sSeg);
      Sections[7].Text := GetStr(sPath);
    end;
end;

procedure TMain.Archiver1DisplayMessage(Sender: TObject;
  const msg: String);
begin
  DisplayMessage( msg );
end;

procedure TMain.Archiver1ClearFileList(Sender: TObject);
begin
  ClearGrid;
  ClearDirectory;
end;

procedure TMain.Selectnone1Click(Sender: TObject);
var
  i : Integer;
begin
  with lbFiles do
    begin
      Items.BeginUpdate;
      for i := 0 to Items.Count - 1 do
        Selected[i] := False;
      Items.EndUpdate;
      ItemIndex := -1;
    end;
  CalcSelSize;
end;

procedure TMain.Invertselection1Click(Sender: TObject);
var
  i : Integer;
begin
  with lbFiles do
    begin
      Items.BeginUpdate;
      for i := 0 to Items.Count - 1 do
        Selected[i] := not Selected[i];
      Items.EndUpdate;
    end;
  CalcSelSize;
end;

procedure TMain.CalcSelSize;
begin
  if Archiver1.IsOpen and (Archiver1.Operation = opNone) then
    DisplayMessage( SelectionAsString );
end;

function TMain.lvFilesCompareCol0(Sender: TObject; Item1, Item2: TObject ) : Integer;
var
  it1, it2 : TFileObject;
begin
  it1 := Archiver1.Files[ Integer(Item1) ];
  it2 := Archiver1.Files[ Integer(Item2) ];
  Result := CompareText( ExtractFileName(it1.FileEntry.Name), ExtractFileName(it2.FileEntry.Name) );
end;

function TMain.lvFilesCompareCol1(Sender: TObject; Item1, Item2: TObject ) : Integer;
var
  ts1, ts2 : TTimeStamp;
  it1, it2 : TFileObject;
begin
  it1 := Archiver1.Files[ Integer(Item1) ];
  it2 := Archiver1.Files[ Integer(Item2) ];
  ts1 := DateTimeToTimeStamp( it1.FileEntry.Date );
  ts2 := DateTimeToTimeStamp( it2.FileEntry.Date );
  if ts1.Date < ts2.Date then
    Result := -1
  else if ts1.Date = ts2.Date then
    Result := 0
  else
    Result := 1;
end;

function TMain.lvFilesCompareCol2(Sender: TObject; Item1, Item2: TObject ) : Integer;
var
  ts1, ts2 : TTimeStamp;
  it1, it2 : TFileObject;
begin
  it1 := Archiver1.Files[ Integer(Item1) ];
  it2 := Archiver1.Files[ Integer(Item2) ];
  ts1 := DateTimeToTimeStamp( it1.FileEntry.Date );
  ts2 := DateTimeToTimeStamp( it2.FileEntry.Date );
  if ts1.Time < ts2.Time then
    Result := -1
  else if ts1.Time = ts2.Time then
    Result := 0
  else
    Result := 1;
end;

function TMain.lvFilesCompareCol3(Sender: TObject; Item1, Item2: TObject ) : Integer;
var
  s1, s2 : Integer;
  it1, it2 : TFileObject;
begin
  it1 := Archiver1.Files[ Integer(Item1) ];
  it2 := Archiver1.Files[ Integer(Item2) ];
  s1 := it1.FileEntry.ArchiveInfo.Size;
  s2 := it2.FileEntry.ArchiveInfo.Size;
  if s1 < s2 then
    Result := -1
  else if s1 = s2 then
    Result := 0
  else
    Result := 1;
end;

function TMain.lvFilesCompareCol4(Sender: TObject; Item1, Item2: TObject ) : Integer;
var
  f1, f2 : TFileInfo;
  s1, s2 : Integer;
  it1, it2 : TFileObject;
begin
  it1 := Archiver1.Files[ Integer(Item1) ];
  it2 := Archiver1.Files[ Integer(Item2) ];
  with it1.FileEntry do
    if oOpenSingleSegment in Archiver1.Options then
      f1 := SegmentInfo
    else
      f1 := ArchiveInfo;
  with it2.FileEntry do
    if oOpenSingleSegment in Archiver1.Options then
      f2 := SegmentInfo
    else
      f2 := ArchiveInfo;
  s1 := CalcRatio( f1.Size, f1.CompressedSize);
  s2 := CalcRatio( f2.Size, f2.CompressedSize);
  if s1 < s2 then
    Result := -1
  else if s1 = s2 then
    Result := 0
  else
    Result := 1;
end;

function TMain.lvFilesCompareCol5(Sender: TObject; Item1, Item2: TObject ) : Integer;
var
  s1, s2 : TFileInfo;
  it1, it2 : TFileObject;
begin
  it1 := Archiver1.Files[ Integer(Item1) ];
  it2 := Archiver1.Files[ Integer(Item2) ];
  with it1.FileEntry do
    if oOpenSingleSegment in Archiver1.Options then
      s1 := SegmentInfo
    else
      s1 := ArchiveInfo;
  with it2.FileEntry do
    if oOpenSingleSegment in Archiver1.Options then
      s2 := SegmentInfo
    else
      s2 := ArchiveInfo;
  if s1.CompressedSize < s2.CompressedSize then
    Result := -1
  else if s1.CompressedSize = s2.CompressedSize then
    Result := 0
  else
    Result := 1;
end;

function TMain.lvFilesCompareCol6(Sender: TObject; Item1, Item2: TObject ) : Integer;
var
  s1, s2 : Integer;
  it1, it2 : TFileObject;
begin
  it1 := Archiver1.Files[ Integer(Item1) ];
  it2 := Archiver1.Files[ Integer(Item2) ];
  s1 := it1.FileEntry.Segment;
  s2 := it2.FileEntry.Segment;
  if s1 < s2 then
    Result := -1
  else if s1 = s2 then
    Result := 0
  else
    Result := 1;
end;

function TMain.lvFilesCompareCol7(Sender: TObject; Item1, Item2: TObject ) : Integer;
var
  it1, it2 : TFileObject;
begin
  it1 := Archiver1.Files[ Integer(Item1) ];
  it2 := Archiver1.Files[ Integer(Item2) ];
  Result := CompareText( it1.FileEntry.Name, it2.FileEntry.Name );
end;

procedure TMain.Action1Click(Sender: TObject);
begin
  View1.Enabled := lbFiles.SelCount = 1;
end;

procedure TMain.Slider1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  w : Integer;
begin
  with Sender as TPanel do
    begin
      if x > Width then
        w := x - Width
      else if x < 0 then
        w := x
      else
        w := 0;
      if tvDirectory.Width + w < Self.Width - 64 then
        tvDirectory.Width := tvDirectory.Width + w;
    end;
end;

function TMain.GetDirectoryIdx( const FileName : String ) : Integer;

  function FindDir( const dir : String; first : TTreeNode ) : TTreeNode;
  begin
    Result := first;
    while Assigned(Result) and (CompareText(Result.Text, dir) <> 0) do
      Result := Result.GetNextSibling;
  end;

  function DefPath( parent : TTreeNode; const path : String ) : Integer;
  var
    dir, rest : String;
    idx : Integer;
    node : TTreeNode;
  begin
    if not Assigned(parent) then
      begin
        Result := 1;
        Exit;
      end;
    idx := Pos( '\', path );
    if idx > 0 then
      begin
        dir := Copy( path, 1, idx-1 );
        rest := Trim( Copy( path, idx+1, Length(path) ) );
      end
    else
      begin
        dir := path;
        rest := '';
      end;
    node := FindDir( dir, parent.GetFirstChild );
    if not Assigned(node) then
      begin
        node := tvDirectory.Items.AddChild( parent, dir );
        node.ImageIndex := 1;
        node.SelectedIndex := 2;
        node.Data := Pointer( FNodeID );
        Inc( FNodeID );
      end;
    if Length(rest) > 0 then
      Result := DefPath( node, rest )
    else
      Result := Integer( node.Data );
  end;

var
  dir, drive, machine, resource : String;
  nodeMachine, parent : TTreeNode;
  idx : Integer;
begin
  dir := ExtractFilePath( FileName );
  if dir = '' then
    begin
      Result := 1;
      Exit;
    end;
  drive := ExtractFileDrive(dir);
  if Length(drive) > 0 then
    begin
      dir := Copy( dir, Length(drive)+1, Length(dir) );
      // check for drive
      if Pos( ':', drive ) > 0 then
        begin
          parent := FindDir( drive, tvDirectory.Items.GetFirstNode );
          if not Assigned(parent) then
            begin
              parent := tvDirectory.Items.Add( nil, drive );
              parent.ImageIndex := 3;
              parent.SelectedIndex := 3;
              parent.Data := Pointer( FNodeID );
              Inc(FNodeID);
            end;
        end
      // check for network resource
      else if Copy(drive, 1, 2 ) = '\\' then
        begin
          System.Delete( drive, 1, 2 );
          idx := Pos( '\', drive );
          if idx > 0 then
            begin
              machine := Copy( drive, 1, idx - 1 );
              resource := RemoveSlash( Copy( drive, idx + 1, length(drive) ) );
            end
          else
            begin
              machine := drive;
              resource := '';
            end;
          parent := FindDir( machine, tvDirectory.Items.GetFirstNode );
          if not Assigned(parent) then
            begin
              parent := tvDirectory.Items.Add( nil, machine );
              parent.ImageIndex := 4;
              parent.SelectedIndex := 4;
              parent.Data := Pointer( FNodeID );
              Inc(FNodeID);
            end;
          if resource <> '' then
            begin
              nodeMachine := parent;
              parent := FindDir( resource, nodeMachine.GetFirstChild );
              if not Assigned(parent) then
                begin
                  parent := tvDirectory.Items.AddChild( nodeMachine, resource );
                  parent.ImageIndex := 3;
                  parent.SelectedIndex := 3;
                  parent.Data := Pointer( FNodeID );
                  Inc(FNodeID);
                end;
            end;
        end  // end of check network
      else // Unknown drive like default drive
        begin
          parent := FindDir( drive, tvDirectory.Items.GetFirstNode );
          if not Assigned(parent) then
            begin
              parent := tvDirectory.Items.Add( nil, drive );
              parent.ImageIndex := 3;
              parent.SelectedIndex := 3;
              parent.Data := Pointer( FNodeID );
              Inc(FNodeID);
            end;
        end;
      if (Length(dir) > 0) and (dir[1] = '\') then
        System.Delete( dir, 1, 1 );
      if Length(dir) = 0 then
        begin
          Result := Integer(parent.Data);
          Exit;
        end;
    end
  else
    parent := tvDirectory.Items.GetFirstNode;
  Result := DefPath( parent, dir );
  tvDirectory.Items.GetFirstNode.Expanded := True;
end;

procedure TMain.ClearDirectory;
var
  node : TTreeNode;
begin
  FNodeID := 1;
  FCurrentDirID := FNodeID;
  tvDirectory.Items.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    tvDirectory.Items.Clear;
    if Archiver1.IsOpen then
      begin
        node := tvDirectory.Items.AddFirst( nil, GetStr(sRoot) );
        node.ImageIndex := 0;
        node.SelectedIndex := 0;
        node.Expanded := True;
        node.Data := Pointer( FNodeID );
        tvDirectory.Selected := node;
        Inc( FNodeID );
      end;
  finally
    tvDirectory.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMain.lvFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  CalcSelSize;
end;

procedure TMain.SetShowTreeView( val : Boolean );
begin
  if FShowTreeView <> val then
    begin
      FShowTreeView := val;
      Treeview1.Checked := val;
      Slider1.Visible := FShowTreeView;
      tvDirectory.Visible := FShowTreeView;
      if FShowTreeView then
        begin
          FCurrentDirID := -1;
          tvDirectory.Selected := tvDirectory.Items.GetFirstNode;
          tvDirectoryClick(tvDirectory);
        end
      else
        DefDirectory( -1 );
    end;
end;

procedure TMain.DefDirectory( dirIdx : Integer );
var
  i : Integer;
  start : DWord;
  iPercent : Integer;
begin
  if dirIdx = FCurrentDirID then
    Exit;
  ClearGrid;
  Screen.Cursor := crHourGlass;
  lbFiles.Items.BeginUpdate;
  start := GetTickCount;
  DisplayMessage( GetStr(sMakingFileList) );
  Enabled := False;
  try
    with Archiver1 do
      for i := 0 to FileCount - 1 do
        begin
          if FileCount > 1 then
            iPercent := Round( (i / (FileCount-1)) * 100)
          else
            iPercent := 0;
          if (PanelProgress.Visible = False) and (Abs(Integer(GetTickCount)-Integer(start)) > 1000) and
             (percent < 50) then
            begin
              SetProgressBar( True );
              btnAbort.Enabled := False;
              btnAbort.Update;
              DisplayMessage( GetStr(sMakingFileList) );
            end;
          if PanelProgress.Visible then
            ProgressBar1.Position := iPercent;
          //if (i mod 100) = 0 then
            //Application.ProcessMessages;
          with Files[i] do
            if (dirIdx = DirectoryIndex) or (dirIdx = -1) then
              AddFileToGrid( FileEntry, i );
        end;
    Sort( FListSortCompare );
  finally
    btnAbort.Enabled := True;
    Enabled := True;
    Screen.Cursor := crDefault;
    lbFiles.Items.EndUpdate;
    SetProgressBar( False );
    paScrollBarResize(paScrollBar);
  end;
  FCurrentDirID := dirIdx;
  CalcSelSize;
end;

procedure TMain.tvDirectoryClick(Sender: TObject);
begin
  DirSelChanged;
end;

procedure TMain.tvDirectoryKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  DirSelChanged;
end;

procedure TMain.tvDirectoryCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
  if Node1.ImageIndex < Node2.ImageIndex then
    Compare := -1
  else if Node1.ImageIndex > Node2.ImageIndex then
    Compare := 1
  else
    Compare := CompareText( Node1.Text, Node2.Text );
end;

procedure TMain.Treeview1Click(Sender: TObject);
begin
  with Sender as TMenuItem do
    begin
      ShowTreeView := not ShowTreeView;
    end;
end;

procedure TMain.CreateSep;
begin
  if not Assigned( FFileSep ) then
    begin
      FFileSep := TMenuItem.Create( Self );
      FFileSep.Caption := '-';
      File1.Add( FFileSep );
    end;
end;

procedure TMain.RemoveSep;
begin
  if Assigned(FFileSep) and
     (File1.Count = FFileSep.MenuIndex+1) then
    begin
      FFileSep.Free;
      FFileSep := nil;
    end;
end;

procedure TMain.RecordFile( const FileName : String );
var
  tmp : TMenuItem;
begin
  if FileName = '' then
    Exit;
  CreateSep;
  tmp := TMenuItem.Create( Self );
  tmp.Caption := FileName;
  tmp.OnClick := LastFilesClick;
  File1.Insert( FFileSep.MenuIndex+1, tmp );
  if File1.Count - FFileSep.MenuIndex > 6 then
    File1.Items[ File1.Count-1 ].Free;
end;

procedure TMain.RemoveLastFileMenu( menuItem : TMenuItem );
begin
  menuItem.Free;
  RemoveSep;
end;

procedure TMain.RemoveLastFile( const FileName : String );
var
  i : Integer;
begin
  if not Assigned(FFileSep) then
    Exit;
  for i := FFileSep.MenuIndex to File1.Count - 1 do
    if CompareText( File1.Items[i].Caption, FileName ) = 0 then
      begin
        File1.Items[i].Free;
        Break;
      end;
  RemoveSep;
end;

procedure TMain.LastFilesClick( Sender : TObject );

  function Clean( const str : String ) : String;
  var
    i : Integer;
  begin
    Result := str;
    for i := Length(Result) downto 1 do
      if Result[i] = '&' then
        System.Delete( Result, i, 1 );
  end;

var
  fName : String;
begin
  with Sender as TMenuItem do
    begin
      fName := Clean(Caption);
      RemoveLastFileMenu( Sender as TMenuItem );
      OpenArchive( fName );
    end;
end;

procedure TMain.WriteLastFilesToRegistry( reg : TRegistry );
var
  i : Integer;
begin
  if not Assigned(FFileSep) then
    Exit;
  with reg do
    begin
      if OpenKey( GetRegistryKey + '\LastFiles', True) then
        begin
            WriteInteger( 'FileCount', File1.Count - FFileSep.MenuIndex - 1 );
            for i := FFileSep.MenuIndex+1 to File1.Count - 1 do
              WriteString( Format('File%d', [i-(FFileSep.MenuIndex+1)]), File1.Items[i].Caption );
          end;
      CloseKey;
    end;
end;

procedure TMain.ReadLastFilesFromRegistry( reg : TRegistry );
var
  i, count : Integer;
  menu : TMenuItem;
begin
  with reg do
    begin
      if OpenKey( GetRegistryKey + '\LastFiles', True) then
        begin
          if ValueExists( 'FileCount' ) then
            begin
              CreateSep;
              count := ReadInteger( 'FileCount' );
              for i := 0 to count - 1 do
                begin
                  menu := TMenuItem.Create( Self );
                  menu.Caption := ReadString( Format('File%d', [i]) );
                  menu.OnClick := LastFilesClick;
                  File1.Add( menu );
                end;
            end;
          end;
      CloseKey;
    end;
end;

procedure TMain.Expandall1Click(Sender: TObject);
begin
  tvDirectory.FullExpand;
end;

procedure TMain.Collapseall1Click(Sender: TObject);
begin
  tvDirectory.FullCollapse;
end;

procedure TMain.DirSelChanged;
begin
  if tvDirectory.Selected <> nil then
    DefDirectory( Integer(tvDirectory.Selected.Data) );
end;

procedure TMain.FileSelChanged;
begin
  CalcSelSize;
end;

procedure TMain.lbFilesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);

  procedure DrawCell( const text : String; var Rect : TRect; al : TAlignment );
  begin
    with Control as TListBox do
      case al of
      taCenter:
          DrawText( Canvas.Handle, PChar(text), Length(text),
                    Rect, DT_END_ELLIPSIS or DT_CENTER or DT_SINGLELINE or DT_VCENTER );
      taRightJustify:
          DrawText( Canvas.Handle, PChar(text), Length(text),
                    Rect, DT_END_ELLIPSIS or DT_RIGHT or DT_SINGLELINE or DT_VCENTER );
      taLeftJustify:
          DrawText( Canvas.Handle, PChar(text), Length(text),
                    Rect, DT_END_ELLIPSIS or DT_LEFT or DT_SINGLELINE or DT_VCENTER );
      end;
  end;

const
  LeftMargin = 3;
  RightMargin = 3;
  TopMargin = 2;
  BottomMargin = 2;
var
  i, w, offset : Integer;
  al : TAlignment;
  r : TRect;
  f : Extended;
  tmp : TFileInfo;
begin
  with Control as TListBox do
    begin
      Canvas.FillRect(Rect);
      offset := Rect.Left + hcFiles.Left;
      with Archiver1.Files[ Integer(Items.Objects[Index]) ] do
        begin
          if (StateIndex >= 0) and (offset+ImageList2.Width>0) then
            ImageList2.Draw( Canvas, offset, Rect.Top, StateIndex );
          if (ImageIndex < -1) and (offset+ImageList2.Width*2>0) then
            ImageList2.Draw( Canvas, offset+ImageList2.Width, Rect.Top, -ImageIndex )
          else if (ImageIndex >= 0) and (offset+ImageList2.Width*2>0) then
            Imagelist_Draw( shlv, ImageIndex, Canvas.Handle, offset+ImageList2.Width, Rect.Top, 1);
        end;
      for i := 0 to hcFiles.Sections.Count - 1 do
        begin
          al := hcFiles.Sections[i].Alignment;
          w := hcFiles.Sections[i].Width;
          R := Rect;
          R.Left := Offset;
          R.Right := Offset + w;
          if R.Left > Width then
            Break;
          InflateRect( R, -3, -2 );
          if R.Right > 0 then
            with Archiver1.Files[ Integer(Items.Objects[Index]) ].FileEntry do
              begin
                if oOpenSingleSegment in Archiver1.Options then
                  tmp := SegmentInfo
                else
                  tmp := ArchiveInfo;
                case i of
                0: // Name
                  begin
                    Inc( R.Left, ImageList2.Width*2 );
                    DrawCell( ExtractFileName(Name), R, al );
                  end;
                1: // Date
                  begin
                    DrawCell( DateToStr(Date), R, al );
                  end;
                2: // Time
                  begin
                    DrawCell( TimeToStr(Date), R, al );
                  end;
                3: // Size
                  begin
                    f := tmp.Size;
                    DrawCell( Format('%.0n', [f]), R, al );
                  end;
                4: // Ratio
                  begin
                    if Archiver1.IsSolidArchive then
                      DrawCell( '-', R, al )
                    else
                      begin
                        if tmp.Size <> 0 then
                          DrawCell( IntToStr(CalcRatio( tmp.Size, tmp.CompressedSize))+'%', R, al )
                        else
                          DrawCell( '-', R, al );
                      end;
                  end;
                5: // Packed
                  begin
                    f := tmp.CompressedSize;
                    if Archiver1.IsSolidArchive then
                      DrawCell( '-', R, al )
                    else
                      DrawCell( Format('%.0n', [f]), R, al );
                  end;
                6: // Segment
                  begin
                    DrawCell( IntToStr(Segment), R, al );
                  end;
                7: // Path
                  begin
                    DrawCell( ExtractFilePath(Name), R, al );
                  end;
                end; // end of case
              end; // end of with
          Inc( Offset, w );
        end; // end of for
    end; // end of with
end;

procedure TMain.paFilesResize(Sender: TObject);
begin
  lbFiles.Invalidate;
end;

procedure TMain.hcFilesSectionResize(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin
  lbFiles.Invalidate;
  UpdateScrollBar( paHeader.Width );
end;

procedure TMain.hcFilesSectionTrack(HeaderControl: THeaderControl;
  Section: THeaderSection; Width: Integer; State: TSectionTrackState);

  function CalcWidth : Integer;
  var
    i : Integer;
  begin
    Result := 0;
    for i := 0 to Section.Index - 1 do
      Inc( Result, HeaderControl.Sections[i].Width );
  end;

var
  offset : Integer;
begin
  offset := CalcWidth + hcFiles.Left;
  case State of
  tsTrackBegin:
    begin
      lbFiles.Canvas.Pen.Mode := pmNotXor;
      lbFiles.Canvas.MoveTo( offset + Width, 0 );
      lbFiles.Canvas.LineTo( offset + Width, lbFiles.Height );
      FLastWidth := Width;
      Section.MaxWidth := HeaderControl.Width - offset - 8;
    end;
  tsTrackMove:
    begin
      lbFiles.Canvas.MoveTo( offset + FLastWidth, 0 );
      lbFiles.Canvas.LineTo( offset + FLastWidth, lbFiles.Height );
      lbFiles.Canvas.MoveTo( offset + Width, 0 );
      lbFiles.Canvas.LineTo( offset + Width, lbFiles.Height );
      FLastWidth := Width;
    end;
  tsTrackEnd:
    begin
      lbFiles.Canvas.MoveTo( offset + Width, 0 );
      lbFiles.Canvas.LineTo( offset + Width, lbFiles.Height );
      lbFiles.Canvas.Pen.Mode := pmBlack;
    end;
  end;
end;

procedure TMain.lbFilesDblClick(Sender: TObject);
begin
  ExecuteFile( False );
end;

procedure TMain.lbFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FileSelChanged;
end;

function TMain.CalcColWidth : Integer;
var
  i : Integer;
begin
  Result := 0;
  with hcFiles do
    for i := 0 to Sections.Count - 1 do
      Inc( Result, Sections[i].Width );
end;

procedure TMain.ScrollBarChanged( Sender : TObject );
begin
  with Sender as TScrollBar do
    begin
      hcFiles.Left := - Position;
      hcFiles.Width := paHeader.Width - hcFiles.Left;
      lbFiles.Invalidate;
    end;
end;

procedure TMain.UpdateScrollBar( w : Integer );
var
  v : Integer;
begin
  v := Max( CalcColWidth-w, 0 );
  FScrollBar.Max := v;
  FScrollBar.LargeChange := v div 4;
  FScrollBar.SmallChange := FScrollBar.LargeChange;
  paScrollBar.Visible := v > 0;
end;

procedure TMain.paHeaderResize(Sender: TObject);
begin
  with Sender as TPanel do
    begin
      hcFiles.Width := Width - hcFiles.Left;
      UpdateScrollBar( Width );
    end;
end;

procedure TMain.paScrollBarResize(Sender: TObject);
var
  offset : Integer;
begin
  if GetDispayableFiles < lbFiles.Items.Count then
    offset := 16
  else
    offset := 0;
  with Sender as TPanel do
    FScrollBar.Width := Width - offset;
end;

function TMain.GetDispayableFiles : Integer;
begin
  Result := lbFiles.Height div lbFiles.ItemHeight;
end;

procedure TMain.lbFilesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ExecuteFile( False );
end;

// Quicksort was copied from the SortThds.pas unit of Threads demo
procedure TMain.QuickSort(SortList: TStrings; iLo, iHi: Integer;
  SCompare: TListSortCompare);

  function AdjustSortOrder( val : Integer ) : Integer;
  begin
    if FAscending then
      Result := val
    else
      Result := -val;
  end;

var
  Lo, Hi, percent : Integer;
  Mid, T : TObject;
begin
  Lo := iLo;
  Hi := iHi;
  Mid := SortList.Objects[(Lo + Hi) div 2];
  repeat
    while AdjustSortOrder( SCompare( Self, SortList.Objects[Lo], Mid) ) < 0 do Inc(Lo);
    while AdjustSortOrder( SCompare( Self, SortList.Objects[Hi], Mid) ) > 0 do Dec(Hi);
    if Lo <= Hi then
    begin
      T := SortList.Objects[Lo];
      SortList.Objects[Lo] := SortList.Objects[Hi];
      SortList.Objects[Hi] := T;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if Hi > iLo then QuickSort( SortList, iLo, Hi, SCompare );
  if Lo < iHi then QuickSort( SortList, Lo, iHi, SCompare );
  if lbFiles.Items.Count > 1 then
    percent := Round(iHi / (lbFiles.Items.Count-1) * 100)
  else
    percent := 0;
  if (PanelProgress.Visible = False) and (Abs(Integer(GetTickCount)-Integer(FStart)) > 1000) and
     (percent < 50) then
    begin
      SetProgressBar( True );
      btnAbort.Enabled := False;
      btnAbort.Update;
      DisplayMessage( GetStr(sSortingFileList) );
    end;
  if PanelProgress.Visible then
    ProgressBar1.Position := percent;
end;

procedure TMain.Sort(Compare: TListSortCompare);
begin
  if Assigned(Compare) and (lbFiles.Items.Count > 0) then
    begin
      Screen.Cursor := crHourGlass;
      FStart := GetTickCount;
      try
        DisplayMessage( GetStr(sSortingFileList) );
        QuickSort(lbFiles.Items, 0, lbFiles.Items.Count - 1, Compare);
      finally
        Screen.Cursor := crDefault;
        SetProgressBar( False );
        btnAbort.Enabled := True;
      end;
    end;
end;

procedure TMain.DoSort;
begin
  if Assigned(FListSortCompare) then
    begin
      Sort( FListSortCompare );
      lbFiles.Invalidate;
    end;
end;

procedure TMain.SelectSortCol( col : Integer );
var
  tmp : TListSortCompare;
begin
  FSortedColumn := col;
  hcFiles.Invalidate;
  OriginalOrder1.Checked := False;
  Name1.Checked := False;
  Date1.Checked := False;
  Time1.Checked := False;
  Size1.Checked := False;
  Ratio1.Checked := False;
  Packed1.Checked := False;
  Segment1.Checked := False;
  Path1.Checked := False;
  case col of
  -1: OriginalOrder1.Checked := True;
   0: Name1.Checked := True;
   1: Date1.Checked := True;
   2: Time1.Checked := True;
   3: Size1.Checked := True;
   4: Ratio1.Checked := True;
   5: Packed1.Checked := True;
   6: Segment1.Checked := True;
   7: Path1.Checked := True;
  end;
  case col of
  0: tmp := lvFilesCompareCol0;
  1: tmp := lvFilesCompareCol1;
  2: tmp := lvFilesCompareCol2;
  3: tmp := lvFilesCompareCol3;
  4: tmp := lvFilesCompareCol4;
  5: tmp := lvFilesCompareCol5;
  6: tmp := lvFilesCompareCol6;
  7: tmp := lvFilesCompareCol7;
  else
    tmp := nil;
  end;
  if not Assigned(tmp) then
    begin
      if FShowTreeView then
        begin
          FCurrentDirID := -1;
          DirSelChanged;
        end
      else
        DefDirectory( -1 );
      Exit;
    end;
  if @FListSortCompare <> @tmp then
    begin
      FListSortCompare := tmp;
      FAscending := True;
    end
  else
    FAscending := not FAscending;
  DoSort;
end;

procedure TMain.hcFilesSectionClick(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin
  SelectSortCol( Section.Index );
end;

procedure TMain.Archiver1AddFile(Sender: TObject;
  var FileEntry: TFileEntry; var Accept: Boolean);

  function GetPath : String;
  var
    node : TTreeNode;
  begin
    Result := '';
    node := tvDirectory.Selected;
    while Assigned(node) do
      begin
        if node <> tvDirectory.Items.GetFirstNode then
          Result := node.Text + '\' + Result;
        if (node.Parent = nil) and (node.ImageIndex = 4) then
          Result := '\\'+Result;
        node := node.Parent;
      end;
  end;

  function ApplyFilter( const FileName : String ) : Boolean;
  var
    fname : String;
    i : Integer;
    found : Boolean;
  begin
    Result := True;
    if FFilters.Count = 0 then
      Exit;
    fname := UpperCase(ExtractFileName(FileName));
    found := False;
    for i := 0 to FFilters.Count - 1 do
      begin
        if FilterIsTrue( FFilters.Strings[i], fname ) then
           begin
             found := True;
             Break;
           end;
      end;
    if FKindOfFilter = 0 then // require
      Result := found
    else                      // exclude
      Result := not found;
  end;

var
  s : String;
begin
  if FFilterExtension then
    Accept := ApplyFilter(FileEntry.Name);
  if not Accept then
    Exit;
  if FAddToCurrentFolder and tvDirectory.Visible then
    begin
      if ExtractFileDrive(FileEntry.Name) = '' then
        begin
          s := GetPath;
          FileEntry.Name := s + FileEntry.Name;
        end;
    end;
end;

procedure TMain.FormResize(Sender: TObject);
begin
  Timing.Left := (Width - Timing.Width) div 2;
  Timing.Top := (Height - Timing.Height) div 2;
end;

// get the shell's small icon for a given file/folder
function GetShIcon( FileName : String; var IconIndex : Integer ) : Boolean;
var
  sfi : PShFileInfo;
begin
  GetMem( sfi, sizeof(TShFileInfo) );
  try
    shlv:= shGetFileInfo( PChar(FileName), 0, sfi^, sizeof(TShFileInfo), shgfi_sysiconindex or shgfi_icon or shgfi_smallicon
          or shgfi_displayname );
    Result := sfi.szDisplayName <> '';
    IconIndex := sfi.iIcon;
  finally
    FreeMem(sfi);
  end;
end;

function TMain.GetIconFrom( const FileName : String; var IconIndex : Integer ) : Boolean;
var
  ext, tmp : String;
  idx : Integer;
  S : TFileStream;
begin
  ext := UpperCase( ExtractFileExt( FileName ) );
  // First, check in our list of extensions if we already know this one
  idx := FExtIcons.IndexOf( ext );
  if idx >= 0 then
    begin
      IconIndex := Integer(FExtIcons.Objects[idx]);
      Result := True;
      Exit;
    end;
  // Then, we must create a temporary file with this extension, otherwise
  // the function GetShIcon won't work.
  tmp := Format( '%s~WinArchiver%x%s', [GetTempDir, GetTickCount, ext] );
  S := TFileStream.Create( tmp, fmCreate );
  try
    Result := GetShIcon( tmp, IconIndex );
    FExtIcons.AddObject( ext, TObject(IconIndex) );
  finally
    S.Free;
    DeleteFile( tmp );
  end;
end;

{$IFDEF D3}
procedure TMain.DropSource1Drop( Sender: TObject; DragType: TDragType; var ContinueDrop: Boolean);
var
  opt : TOptions;
  ra : TRestoreAction;
begin
  with lbFiles do
    begin
      opt := Archiver1.Options;
      ra := Archiver1.RestoreAction;
      Archiver1.RestoreAction := raOverwrite;
      opt := opt - [oRestorePath];
      Archiver1.ExtractPath := GetTempDir;
      try
          if FDropSource.Files.Count = 1 then
            with Archiver1.Files[ Integer(FDropSource.Files.Objects[0]) ].FileEntry do
              Archiver1.ExtractFile( Segment, Offset, ArchiveInfo.CompressedSize )
          else
            Archiver1.ExtractFiles;
      except
        ContinueDrop := False;
      end;
      Archiver1.Options := opt;
      Archiver1.RestoreAction := ra;
      FDroping := False;
    end;
end;

{$ENDIF}

procedure TMain.ViewlastOutput1Click(Sender: TObject);
begin
  with LastOutput do
    begin
      ShowModal;
    end;
end;

procedure TMain.Archiver1Error(Sender: TObject; E: Exception;
  const FileEntry: TFileEntry; var ErrorAction: TErrorAction);
begin
  AddToLog( Format('---> %s: %s', [ Archiver1.Messages.Error,
                               E.Message]) );
  AddToLog( Format('     %s: %s', [ Archiver1.Messages.AFile,
                               AdjustPath( FileEntry.Name, 80 )]) );
end;

procedure TMain.Archiver1FileExtracted(Sender: TObject;
  const FileEntry: TFileEntry; const DestPath: String);
begin
  if FDroping then
    FFilesDroped.Add( DestPath + ExtractFileName(FileEntry.Name) );
end;

procedure TMain.DeleteTempDropedFiles;
var
  i : Integer;
begin
  if not Assigned(FFilesDroped) then
    Exit;
  for i := 0 to FFilesDroped.Count - 1 do
    if DirectoryExists( FFilesDroped.Strings[i] ) then
      Archiver1.DeleteDirectory( FFilesDroped.Strings[i] )
    else
      DeleteFile( FFilesDroped.Strings[i] );
  FFilesDroped.Clear;
end;

procedure TMain.Checkintegrity1Click(Sender: TObject);
begin
  Archiver1.CheckIntegrity;
end;

procedure TMain.tbInstallClick(Sender: TObject);
var
  opt : TOptions;
  ra : TRestoreAction;
  dest, install : String;
begin
  // Create a temp folder for the extraction
  dest := Format( '%s~WinArchiver-Install-%x', [GetTempDir, GetTickCount] );
  FFilesDroped.Add( dest );
  opt := Archiver1.Options;
  ra := Archiver1.RestoreAction;
  Archiver1.RestoreAction := raOverwrite;
  opt := opt + [oRestorePath];
  Archiver1.ExtractPath := dest;
  try
    // Extract content
    Archiver1.ExtractFiles;
    // Execute install program
    with Archiver1.Files[ FInstallIdx ] do
      install := FileEntry.Name;
    if ExtractFileDrive( install ) = '' then
      install := dest + '\' + install;
    if FileExists( install ) then
      begin
        Application.Minimize;
        fileExec( install, False, False );
      end;
  finally
    Archiver1.Options := opt;
    Archiver1.RestoreAction := ra;
    // Delete temp folder
    // the folder will be deleted when we'll close the archive
  end;
end;

procedure TMain.ClearInstallRef;
begin
  tbInstall.Enabled := False;
  FInstallIdx := -1;
end;

procedure TMain.lbFilesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragPoint := Point(X,Y);
  FButtonClicked := True;
end;

procedure TMain.lbFilesMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  with Sender as TListBox do
    begin
      if (SelCount = 0) or not FButtonClicked then exit;

      //Make sure mouse has moved at least 10 pixels first..
      if (abs(FDragPoint.X - X) >10) or
           (abs(FDragPoint.Y - Y) >10) then StartFileDrag;
    end;
end;

procedure TMain.lbFilesMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FButtonClicked := False;
  FileSelChanged;
end;

procedure TMain.StartFileDrag;
var
  i: integer;
begin
  FButtonClicked := False;
  {$IFDEF D3}
    //Fill FDropSource.Files with selected files
    with FDropSource do
      begin
        Files.Clear;
        for i := 0 to lbFiles.Items.Count - 1 do
          if lbFiles.Selected[i] then
            with Archiver1.Files[ Integer(lbFiles.Items.Objects[i]) ] do
              Files.AddObject( GetTempDir + ExtractFileName(FileEntry.Name), lbFiles.Items.Objects[i] );
      end;

    // Temporarily turn off Drop target so you can't
    // drop straight back onto the listbox...
    DragAcceptFiles( Handle, False );

    //DO IT HERE!!!!!
    FDroping := True;
    try
      FDropSource.Execute;
    finally
      DragAcceptFiles( Handle, True ); // Reenable file drop operations.
    end;

    FDropSource.Files.clear; //added for safety
  {$ENDIF}
  {$IFDEF D2}
    //Fill FDropSource.Files with selected files
    with FDragFilesSrc do
      begin
        ClearFiles;
        for i := 0 to lbFiles.Items.Count - 1 do
          if lbFiles.Selected[i] then
            with Archiver1.Files[ Integer(lbFiles.Items.Objects[i]) ] do
              Files.AddObject( GetTempDir + ExtractFileName(FileEntry.Name), lbFiles.Items.Objects[i] );
      end;

    // Temporarily turn off Drop target so you can't
    // drop straight back onto the listbox...
    DragAcceptFiles( Handle, False );

    //DO IT HERE!!!!!
    FDroping := True;
    try
      FDragFilesSrc.Execute;
    finally
      DragAcceptFiles( Handle, True ); // Reenable file drop operations.
    end;

    FDragFilesSrc.ClearFiles; //added for safety

  {$ENDIF}
end;

{$IFDEF D2}
procedure TMain.DragFilesSrcDropping( Sender : TObject );
var
  opt : TOptions;
  ra : TRestoreAction;
begin
  with lbFiles do
    begin
      opt := Archiver1.Options;
      ra := Archiver1.RestoreAction;
      Archiver1.RestoreAction := raOverwrite;
      opt := opt - [oRestorePath];
      Archiver1.ExtractPath := GetTempDir;
      try
          if FDragFilesSrc.Files.Count = 1 then
            with Archiver1.Files[ Integer(FDragFilesSrc.Files.Objects[0]) ].FileEntry do
              Archiver1.ExtractFile( Segment, Offset, ArchiveInfo.CompressedSize )
          else
            Archiver1.ExtractFiles;
        Archiver1.Options := opt;
        Archiver1.RestoreAction := ra;
      except
        Archiver1.Options := opt;
        Archiver1.RestoreAction := ra;
      end;
    end;
end;
{$ENDIF}

procedure TMain.Font1Click(Sender: TObject);
begin
  with FontDialog1 do
    begin
      Font.Assign( lbFiles.Font );
      if Execute then
        begin
          lbFiles.Font.Assign( Font );
          lbFiles.ItemHeight := Max( abs(Font.Height), 18 );
          tvDirectory.Font.Assign( Font );
        end;
    end;
end;

procedure TMain.Originalorder1Click(Sender: TObject);
begin
  with Sender as TMenuItem do
    SelectSortCol( Tag );
end;

procedure TMain.hcFilesDrawSection(HeaderControl: THeaderControl;
  Section: THeaderSection; const Rect: TRect; Pressed: Boolean);

  procedure DrawTriangle1( left, top : Integer );
  begin
    with HeaderControl.Canvas do
      begin
        Pen.Color := clBtnShadow;
        MoveTo( left, top );
        LineTo( left + 8, top );
        Pen.Color := clBtnHighlight;
        LineTo( left + 5, top + 7 );
        Pen.Color := clBtnShadow;
        MoveTo( left + 4, top + 7 );
        LineTo( left, top );
      end;
  end;

  procedure DrawTriangle2( left, top : Integer );
  begin
    with HeaderControl.Canvas do
      begin
        Pen.Color := clBtnHighlight;
        MoveTo( left, top );
        LineTo( left + 8, top );
        Pen.Color := clBtnHighlight;
        LineTo( left + 5, top - 7 );
        Pen.Color := clBtnShadow;
        MoveTo( left + 4, top - 7 );
        LineTo( left, top );
        Pixels[ left, top ] := clBtnShadow;
      end;
  end;

const
  kTriWidth = 25;
var
  R : TRect;
  W : Integer;
begin
  with Rect do
    IntersectClipRect( HeaderControl.Canvas.Handle, left, top, right, bottom );
  w := HeaderControl.Canvas.TextWidth(Section.Text);
  R := Rect;
  R.Left := R.Left + 8;
  if FSortedColumn = Section.Index then
    begin
      R.Right := R.Right - kTriWidth;
      if w < R.Right-R.Left then
        R.Right := R.Left + w;
    end
  else
    R.Right := R.Right - 4;
  DrawText( HeaderControl.Canvas.Handle, PChar(Section.Text), Length(Section.Text),
            R, DT_END_ELLIPSIS or DT_LEFT or DT_SINGLELINE or DT_VCENTER );

  if FSortedColumn = Section.Index then
    if not FAscending then
      DrawTriangle1( R.Right+10, Rect.Top+4 )
    else
      DrawTriangle2( R.Right+10, Rect.Bottom-6 );
end;

procedure TMain.Archiver1AddToLog(Sender: TObject; const msg: String);
begin
  AddToLog( msg );
end;

procedure TMain.Archiver1DeleteFileByIndex(Sender: TObject; Index: Integer;
  var Accept: Boolean);
begin
  case Delete.rgFiles.ItemIndex of
  1: // Selected files
    begin
      Accept := IsFileSelected(Index);
    end;
  2: // pattern
    begin
    end;
  end;
end;

procedure TMain.Archiver1ExtractFileByIndex(Sender: TObject;
  Index: Integer; var DestPath: String; var Accept: Boolean);
begin
  with Sender as TArchiver do
    begin
      if FDroping then
        begin
          Accept := CheckFilesToDrop(Files[Index].FileEntry.Name);
          Exit;
        end;

      case Extract.rgFiles.ItemIndex of
      0: // Selected files
        begin
          Accept := IsFileSelected(Index);
        end;
      2: // pattern
        begin
        end;
      end;
    end;
end;

procedure TMain.Filters1Click(Sender: TObject);
begin
  if CheckBusy then
    Exit;
  Filters.rgKindOfFilter.ItemIndex := FKindOfFilter;
  Filters.lbExtensions.Items.Assign( FFilters );
  if Filters.ShowModal = mrOk then
    begin
      FKindOfFilter := Filters.rgKindOfFilter.ItemIndex;
      FFilters.Assign( Filters.lbExtensions.Items );
    end;
end;

end.

