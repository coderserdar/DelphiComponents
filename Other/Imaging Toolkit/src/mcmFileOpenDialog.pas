// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  23264: mcmFileOpenDialog.pas 
//
//    Rev 1.22    2014-02-02 21:09:54  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.21    15-08-2009 13:39:02  mcm    Version: IMG 3.3
// Delphi 2009 support
//
//    Rev 1.20    30-11-2008 13:33:08  mcm
// Delphi 2009
//
//    Rev 1.19    31-10-2007 20:21:02  mcm    Version: IMG 3.2
//
//    Rev 1.18    20-08-2007 20:28:22  mcm
// Added support for Delphi 2007
//
//    Rev 1.17    01-07-2006 11:36:24  mcm    Version: IMG 3.0
// Enabled displaying "fax" images in correct dimension by using the horizontal
// and vertical resolution to scale the vertical direction additionally.
//
//    Rev 1.16    19-03-2006 10:19:26  mcm    Version: IMG 2.16
// Fixed typing sub-directory and full path in filename box.
//
//    Rev 1.15    18-03-2006 18:30:50  mcm    Version: IMG 2.16
// Enabled typing just a sub-directory in the FileOpenDialog.
// Corrected a potential problem using the Copy() method.
//
//    Rev 1.14    29-01-2006 12:21:02  mcm    Version: IMG 2.14
// Initial Unicode support.
//
//    Rev 1.13    11-12-2005 20:55:22  mcm    Version: IMG 2.11
// Added support for JPEG compression in TIFF files.
//
//   Rev 1.12    28-09-2005 17:09:36  mcm    Version: IMG 2.9
// Fixed incorrect disable of the Save button.

//
//   Rev 1.11    22-09-2005 20:58:10  mcm    Version: IMG 2.9
// Modified how images files/directories are opened using the Open buton.

//
//   Rev 1.10    23-08-2005 23:28:10  mcm    Version: IMG 2.9
// Added two new events to support external file formats. The events
// OnSaveHasOption and OnSaveShowOption. 
// Added option of showing all file formates and compression methods, ie. no
// color resolution checking.
// Included XP button icons and displaying a "No Access" bitmap when the preview
// image cannont be displayed.

//
//   Rev 1.9    15-05-2005 19:39:14  mcm    Version: IMG 2.9
// Corrected problems in the Save dialogue.

//
//   Rev 1.8    20-12-2004 22:58:58  mcm
// Fixed missing anchor

//
//   Rev 1.7    26-09-2004 11:09:16  mcm    Version: IMG 2.6
// Renamed compression const.

//
//   Rev 1.6    20-07-2004 23:30:16  mcm    Version: IMG 2.5
// Increased "Icon" size to 32 pixels for vsSmallIcon and vsList views.

//
//   Rev 1.5    19-07-2004 20:27:42  mcm    Version: IMG 2.5
// Anchored the Option button in the Save dialogue.

//
//   Rev 1.4    15-07-2004 19:05:14  mcm    Version: IMG 2.5
// Corrected presentation in Windows XP.

//
//   Rev 1.3    08-07-2004 21:44:00  mcm    Version: IMG 2.5
// Clear TmcmImageCtrl before Open & Cancel.

//
//   Rev 1.2    25-06-2004 21:05:44  mcm    Version: IMG 2.5
// Added compression support in the save dialogue and Help for the Compression
// pop-up dialogue. 

//
//   Rev 1.1    31-05-2004 23:55:54  mcm    Version: IMG 2.5
// Implemented file save dialogue.

//
//   Rev 1.0    19-05-2004 21:57:40  mcm    Version: IMG 2.5
// Initial revision

unit mcmFileOpenDialog;

interface

{$Include 'mcmDefines.pas'}            

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
      Dialogs, StdCtrls, ComCtrls,
      {$IFNDEF DCB3} ImgList, {$ENDIF}
      ToolWin,
      {$IFDEF DCB3_5} FileCtrl, {$ENDIF}
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, Vcl.ComCtrls,
      Vcl.ImgList, Vcl.ToolWin, Vcl.FileCtrl,
     {$ENDIF}
     mcmImageTypeDef,
     mcmImage,
     mcmControls,
     mcmImageFile;

type
  TmcmViewStyle = (vsIcon, vsSmallIcon, vsList, vsReport, vsPreview);

  // File save Option events.
  TOnSaveHasOption  = procedure(    Sender     : TObject;
                                    FileFormat : TmcmFileFormat;
                                var HasOption  : boolean) of object;

  TOnSaveShowOption = procedure(    Sender     : TObject;
                                    FileFormat : TmcmFileFormat) of object;

  TFormOpenFile = class(TForm)
    ilToolbuttons : TImageList;
    ilBadImage    : TImageList;
    ilToolbuttons2: TImageList;
    Toolbar       : TToolBar;
    tbBack        : TToolButton;
    tbLevelUp     : TToolButton;
    tbNewFolder   : TToolButton;
    tbView        : TToolButton;

    ViewMenu      : TPopupMenu;
    LargeIconItem : TMenuItem;
    SmallIconItem : TMenuItem;
    ListItem      : TMenuItem;
    DetailsItem   : TMenuItem;
    PreviewItem   : TMenuItem;

    lLookIn       : TLabel;
    ShellComboBox : TmcmShellComboBox;
    ShellListView : TmcmShellListView;
    mcmImageCtrl  : TmcmImageCtrl;
    btnOpen       : TButton;
    btnCancel     : TButton;
    btnHelp       : TButton;
    btnOption     : TButton;

    lFilename     : TLabel;
    cbFilename    : TComboBox;
    lFileOfType   : TLabel;
    cbFileOfType  : TComboBox;
    lCompression  : TLabel;
    cbCompression : TComboBox;

    procedure FormCreate(Sender : TObject); virtual;
    procedure FormDestroy(Sender : TObject); virtual;
    procedure FormResize(Sender : TObject);

    procedure btnOpenClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure btnHelpClick(Sender : TObject);
    procedure tbBackClick(Sender : TObject);
    procedure tbLevelUpClick(Sender : TObject);
    procedure tbNewFolderClick(Sender : TObject);
    procedure tbViewClick(Sender : TObject);
    procedure LargeIconItemClick(Sender : TObject);
    procedure SmallIconItemClick(Sender : TObject);
    procedure ListItemClick(Sender : TObject);
    procedure DetailsItemClick(Sender : TObject);
    procedure PreviewItemClick(Sender : TObject);
    procedure ShellListViewFileChange(Sender : TObject; AFileName : String);
    procedure cbFileOfTypeChange(Sender : TObject);
    procedure cbCompressionChange(Sender : TObject);
    procedure ShellListViewDblClick(Sender : TObject);
    procedure ShellListViewClick(Sender : TObject);
    procedure ShellComboBoxChange(Sender : TObject);
    procedure cbFilenameKeyUp(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure cbFilenameKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure cbFilenameClick(Sender : TObject);
    procedure cbFilenameDropDown(Sender : TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : Boolean);
    procedure FormClose(Sender : TObject; var Action : TCloseAction);
    procedure FormShow(Sender : TObject);
    procedure btnOptionClick(Sender : TObject);
  private
    { Private declarations }
  protected
    { Protected declarations }
    FCanClose          : boolean;
    FOptionHelp        : THelpContext;
    FFileManager       : TmcmImageFileMgr;
    FImage             : TmcmImage;
    FFileFormat        : TmcmFileFormat;
    FCompression       : TmcmCompress;
    FInterlaced        : boolean;
    FQuality           : integer;
    FViewStyle         : TmcmViewStyle;
    FExtList           : TStringList;
    FFilter            : String;
    FHistory           : TStrings;
    FOldText           : String;
    FDefaultExt        : String;
    FOptions           : TOpenOptions;
    FCurDir            : String;
    FEnableAllFormats  : boolean;
    FOnClose           : TNotifyEvent;
    FOnFolderChange    : TNotifyEvent;
    FOnSelectionChange : TNotifyEvent;
    FOnShow            : TNotifyEvent;
    FOnTypeChange      : TNotifyEvent;
    FOnHasOption       : TOnSaveHasOption;
    FOnShowOption      : TOnSaveShowOption;

    procedure AddExtensions(Extension : String);
    function  AppendExtension(Name : String) : String;
    procedure CheckFileCreate;
    function  CheckFileName(Name : String) : boolean;
    procedure FillFileNameList(MatchName : String);
    function  GetFileName : TFileName;
    function  GetFilterIndex : integer;
    function  GetInitDir : String;
    procedure ResizePreviewColumns;
    procedure ShellListFolderChanged;
    procedure SetDefaultExt(Value : String);
    procedure SetFileManager(Value : TmcmImageFileMgr); virtual;
    procedure SetFileName(Value : TFileName);
    procedure SetFilter(Value : String);
    procedure SetFilterIndex(Value : integer);
    procedure SetInitDir(Value : String);
    procedure SetOptions(Value : TOpenOptions); virtual;
    procedure SetViewStyle(Value : TmcmViewStyle);
    property  OptionHelpContext : THelpContext
      read    FOptionHelp
      write   FOptionHelp;
  public
    { Public declarations }
    property    DefaultExt : String
      read      FDefaultExt
      write     SetDefaultExt;
    property    Filename : TFileName
      read      GetFileName
      write     SetFileName;
    property    Filter : String
      read      FFilter
      write     SetFilter;
    property    FilterIndex : Integer
      read      GetFilterIndex
      write     SetFilterIndex;
    property    InitDir : String
      read      GetInitDir
      write     SetInitDir;
    property    HelpContext;
    property    ImageFileManager : TmcmImageFileMgr
      read      FFileManager
      write     SetFileManager;
    property    ViewStyle : TmcmViewStyle
      read      FViewStyle
      write     SetViewStyle;
    property    Options : TOpenOptions
      read      FOptions
      write     SetOptions;
    property    OnClose : TNotifyEvent
      read      FOnClose
      write     FOnClose;
    property    OnFolderChange : TNotifyEvent
      read      FOnFolderChange
      write     FOnFolderChange;
    property    OnSelectionChange : TNotifyEvent
      read      FOnSelectionChange
      write     FOnSelectionChange;
    property    OnShow : TNotifyEvent
      read      FOnShow
      write     FOnShow;
    property    OnTypeChange : TNotifyEvent
      read      FOnTypeChange
      write     FOnTypeChange;
  end;


  TFormSaveFile = class(TFormOpenFile)
    procedure FormCreate(Sender : TObject); override;
    procedure FormDestroy(Sender : TObject); override;
    procedure btnSaveClick(Sender : TObject);
  protected
    FFormats : TmcmFileFormatsList;

    function  GetImage : TmcmImage;
    function  GetCompression : TmcmCompress;
    procedure SetCompression(Value : TmcmCompress);
    procedure SetEnableAllFormats(Value : boolean);
    procedure SetFileManager(Value : TmcmImageFileMgr); override;
    procedure SetImage(AImage : TmcmImage);
    procedure SetOptions(Value : TOpenOptions); override;
    function  GetInterlaced : boolean;
    procedure SetInterlaced(Value : boolean);
    function  GetQuality : integer;
    procedure SetQuality(Value : integer);
  public
    property  Compression : TmcmCompress
      read    GetCompression
      write   SetCompression;
    property  EnableAllFormats : boolean
      read    FEnableAllFormats
      write   SetEnableAllFormats default False;
    property  Image : TmcmImage
      read    GetImage
      write   SetImage;
    property  Interlaced : boolean
      read    GetInterlaced
      write   SetInterlaced;
    property  OptionHelpContext;
    property  Quality : integer
      read    GetQuality
      write   SetQuality;
    // Events
    property  OnHasOption : TOnSaveHasOption
      read    FOnHasOption
      write   FOnHasOption;
    property  OnShowOption : TOnSaveShowOption
      read    FOnShowOption
      write   FOnShowOption;
  end;

implementation

{$R mcmFileOpenDialog.DFM}

uses {$IFNDEF GE_DXE2}
      CommCtrl,
     {$ELSE}
      WinApi.CommCtrl, System.UITypes,
     {$ENDIF}
     mcmImageResStr, mcmCompressOptionDialog;


procedure TFormOpenFile.FormCreate(Sender : TObject);
begin
  FImage := Nil;
  FOnHasOption  := Nil;
  FOnShowOption := Nil;
  FCanClose := True;
  FOptionHelp  := 0;
  ViewMenu.AutoPopup := True;
  cbFilename.Clear;
  mcmImageCtrl.Visible := False;
  btnOption.Visible := False;
  btnOption.Enabled := False;
  FExtList := TStringList.Create;
  FHistory := TStringList.Create;
  FOldText := '';
  btnOpen.Enabled := True;
  ShellListView.FileMask := '';
  FCurDir := GetCurrentDir;
  FEnableAllFormats := False;
  FFileManager := Nil;
end; // TFormOpenFile.FormCreate.


procedure TFormOpenFile.FormDestroy(Sender : TObject);
begin
  if (ofNoChangeDir in FOptions)
  then SetCurrentDir(FCurDir);
  if Assigned(FExtList)
  then FExtList.Free;
  if Assigned(FHistory)
  then FHistory.Free;
end; // TFormOpenFile.FormDestroy.


procedure TFormOpenFile.ResizePreviewColumns;
var i        : integer;
    NewWidth : integer;
    w, s, t  : integer;
    OldMode  : TViewStyle;
begin
  try
    ShellListView.Columns.BeginUpdate;
    try
      if (ShellListView.Columns.Count > 0)
      then begin
           case FViewStyle of
           vsReport  :   begin
                           if (ShellListView.Columns.Count > 0)
                           then begin
                                w := ShellListView.ClientWidth;
                                s := w;
                                for i := 1 to (ShellListView.Columns.Count - 1)
                                do begin
                                   t := Round(14 * w div 100);
                                   s := s - t;
                                   ShellListView.Columns[i].Width := t;
                                end;
                                ShellListView.Columns[0].Width := s;
                           end;
                         end;
           vsPreview :   begin
                           ShellListView.ViewStyle := {$IFDEF GE_DXE2}Vcl.{$ENDIF}ComCtrls.vsList;
                           ShellListView.UpdateView;
                           SendMessage(ShellListView.Items.Handle, LVM_SETCOLUMNWIDTH, 0, MakeLong(ClientWidth, 0));
                           ShellListView.ViewStyle := {$IFDEF GE_DXE2}Vcl.{$ENDIF}ComCtrls.vsSmallIcon;
                         end;
           vsIcon      : begin
                         end;
           vsSmallIcon,
           vsList      : begin
                           OldMode := ShellListView.ViewStyle;
                           ShellListView.ViewStyle := {$IFDEF GE_DXE2}Vcl.{$ENDIF}ComCtrls.vsList;
                           ShellListView.UpdateView;
                           w := 0;
                           for i := 0 to ShellListView.Count - 1
                           do begin
                              NewWidth := Canvas.TextWidth(ShellListView.Items[i].Caption);
                              if (w < NewWidth)
                              then w := NewWidth;
                           end;
                           w := w + 32;
                           SendMessage(ShellListView.Items.Handle, LVM_SETCOLUMNWIDTH, 0, w);
                           ShellListView.ViewStyle := OldMode;
                         end;
           end;
      end;
    finally
      ShellListView.Columns.EndUpdate;
      ShellListView.UpdateView;
    end;
  except
  end;
end; // TFormOpenFile.ResizePreviewColumns.


procedure TFormOpenFile.FormResize(Sender : TObject);
begin
  {$IFNDEF DCB3}  // Delphi 3, dialogue cannot be resized.
    ShellListView.Height := mcmImageCtrl.Height;
  {$ENDIF}
  case FViewStyle of
  vsReport  : begin
                ResizePreviewColumns;
              end;
  vsPreview : begin
                ShellListView.Width := ((btnOpen.Left + btnOpen.Width) - 2 * ShellListView.Left) div 2;
                mcmImageCtrl.Left := 2 * ShellListView.Left + ShellListView.Width;
                mcmImageCtrl.Width := (btnOpen.Left + btnOpen.Width) - mcmImageCtrl.Left;
                ResizePreviewColumns;
              end;
  end;
end; // TFormOpenFile.FormResize.


procedure TFormOpenFile.SetOptions(Value : TOpenOptions);
begin
  FOptions := Value;
  btnHelp.Visible := (ofShowHelp in FOptions);
  if (ofHideReadOnly in FOptions)
  then ShellListView.FileType := ShellListView.FileType - [mftHidden]
  else ShellListView.FileType := ShellListView.FileType + [mftHidden];
  tbNewFolder.Enabled := Not(ofReadOnly in FOptions);
  ShellListView.ReadOnly := (ofReadOnly in FOptions);
  ShellListView.MultiSelect := (ofAllowMultiSelect in FOptions);

  if btnOption.Visible
  then begin
       if Not(ofShowHelp in FOptions)
       then begin
            btnOption.Left := btnHelp.Left;
            btnOption.Top  := btnHelp.Top;
       end;
  end;

  if btnOption.Visible
  then begin
       ClientHeight := btnOption.Top + btnOption.Height + 8
  end
  else begin
       if (ofShowHelp in FOptions)
       then ClientHeight := btnHelp.Top + btnHelp.Height + 8
       else begin
            if cbCompression.Visible
            then ClientHeight := cbCompression.Top + cbCompression.Height + 8
            else ClientHeight := btnCancel.Top + btnCancel.Height + 8;
       end;
  end;

  btnOpen.Enabled := Not(ofFileMustExist in FOptions);

  {$IFNDEF DCB3} // Delphi 3, dialogue cannot be resized.
    // Ensure that all controls position correctly when
    // dialogue is resized.
    if (ofShowHelp in FOptions)
    then Constraints.MinHeight := btnHelp.Top + btnHelp.Height + 8
    else begin
         if cbCompression.Visible
         then Constraints.MinHeight := cbCompression.Top + cbCompression.Height + 8
         else Constraints.MinHeight := btnCancel.Top + btnCancel.Height + 8;
    end;
    
    Constraints.MinHeight := ClientHeight;
    Constraints.MinWidth  := 481;

    lLookIn.Anchors       := [akTop, akLeft];
    ShellComboBox.Anchors := [akTop, akLeft, akRight];
    ShellListView.Anchors := [akTop, akLeft, akRight, akBottom];
    mcmImageCtrl.Anchors  := [akTop, akLeft, akRight, akBottom];
    Toolbar.Anchors       := [akTop, akRight];
    lFilename.Anchors     := [akLeft, akBottom];
    lFileOfType.Anchors   := [akLeft, akBottom];
    cbFilename.Anchors    := [akLeft, akRight, akBottom];
    cbFileOfType.Anchors  := [akLeft, akRight, akBottom];
    cbCompression.Anchors := [akLeft, akRight, akBottom];
    lCompression.Anchors  := [akLeft, akRight, akBottom];
    btnOpen.Anchors       := [akRight, akBottom];
    btnCancel.Anchors     := [akRight, akBottom];
    btnHelp.Anchors       := [akRight, akBottom];
    btnOption.Anchors     := [akRight, akBottom];

    BorderStyle := bsSizeable;
  {$ENDIF}
end; // TFormOpenFile.SetOptions.


procedure TFormOpenFile.AddExtensions(Extension : String);
var DispExt   : String;
    SubExt    : String;
    TmpExt    : String;
    i, j      : integer;
begin
  // Initially Extensions contain something like:
  //   "Joint Photographics Expert Group (*.jpg, *.jpeg)|*.jpg, *.jpeg|All files (*.*)|*.*|"
  // AddExtension seperates this into:
  //   DispExt = "Joint Photographics Expert Group (*.jpg, *.jpeg)"
  // and
  //  SubExt = "*.jpg, *.jpeg"
  // Reduces Extension to
  //   "All files (*.*)|*.*|"
  // and continues Extension is an empty string.
  while (Length(Extension) > 0)
  do begin
     // Get listbox text.
     i := Pos('|', Extension);
     if (i = 0)
     then Exit;
     DispExt := Copy(Extension, 1, i - 1);
     TmpExt := Copy(Extension, i + 1, Length(Extension));
     Extension := TmpExt;
     // Get extensions.
     i := Pos('|', Extension);
     if (i = 0)
     then i := Length(Extension) + 1;
     SubExt := Copy(Extension, 1, i - 1);
     TmpExt := Copy(Extension, i + 1, Length(Extension));
     Extension := TmpExt;
     j := FExtList.Add(SubExt);
     cbFileOfType.Items.AddObject(DispExt , pointer(j));
  end;
end; // TFormOpenFile.AddExtensions.


procedure TFormOpenFile.SetFileManager(Value : TmcmImageFileMgr);
var Extension : String;
begin
  // Set the TmcmImageFileMgr.
  // This causes each enabled file format to be added to the
  FFileManager := Value;
  if Assigned(FFileManager)
  then begin
       if (Self is TFormOpenFile)
       then Extension := FFileManager.GetReadFilter
       else Extension := FFileManager.GetWriteFilter;
       AddExtensions(Extension);
  end;
end; // TFormOpenFile.SetFileManager.


procedure TFormOpenFile.SetDefaultExt(Value : String);
begin
end; //


function TFormOpenFile.GetFileName : TFileName;
begin
  if (ExtractFilePath(ShellListView.Filename) <> '')
  then Result := ShellListView.Filename
  else Result := ShellListView.Path + ShellListView.Filename;
end; // TFormOpenFile.GetFileName.


procedure TFormOpenFile.SetFileName(Value : TFileName);
begin
  if (Value <> '')
  then ShellListView.Filename := Value;
end; // TFormOpenFile.SetFileName.


procedure TFormOpenFile.SetFilter(Value : String);
begin
  FFilter := Value;
  AddExtensions(FFilter);
end; // TFormOpenFile.SetFilter.


function TFormOpenFile.GetFilterIndex : integer;
begin
  Result := cbFileOfType.ItemIndex;
end; // TFormOpenFile.GetFilterIndex.


procedure TFormOpenFile.SetFilterIndex(Value : integer);
begin
  cbFileOfType.ItemIndex := Value;
  cbFileOfTypeChange(Self);
end; // TFormOpenFile.SetFilterIndex.


function TFormOpenFile.GetInitDir : String;
begin
  Result := ShellComboBox.Path;
  if Result[Length(Result)] <> '\'
  then Result := Result + '\';
end; // TFormOpenFile.GetInitDir.


procedure TFormOpenFile.SetInitDir(Value : String);
begin
  if (Value = '')
  then ShellComboBox.Path := GetCurrentDir
  else if SetCurrentDir(Value)
       then ShellComboBox.Path := Value
       else ShellComboBox.Path := GetCurrentDir;
end; // TFormOpenFile.SetInitDir.


procedure TFormOpenFile.SetViewStyle(Value : TmcmViewStyle);
begin
  // Specifies how files are viewed.
  FViewStyle := Value;
  case FViewStyle of
  vsIcon      : LargeIconItemClick(Self);
  vsSmallIcon : SmallIconItemClick(Self);
  vsList      : ListItemClick(Self);
  vsReport    : DetailsItemClick(Self);
  vsPreview   : PreviewItemClick(Self);
  else ShellListView.ViewStyle := TViewStyle(Value);
  end;
end; // TFormOpenFile.SetViewStyle.


function TFormOpenFile.AppendExtension(Name : String) : String;
var Ext : String;
begin
  // Used ONLY when opening an image file. 
  if Not(FileExists(Name))
  then if CheckFileName(Name)
       then begin
            // The most likely candidate is in "cbFilename.Items[0]", therefore
            // get it's extension and append this to our file name.
            Ext := ExtractFileExt(cbFilename.Items[0]);
            Name := ChangeFileExt(Name, Ext);
       end;
  Result := Name;
end; // TFormOpenFile.AppendExtension.


function TFormOpenFile.CheckFileName(Name : String) : boolean;
var TmpName : String;
    Ext     : String;
    i       : integer;
begin
  Result := False;
  if FileExists(Name)
  then begin
       if (ofExtensionDifferent in FOptions)
       then Result := True
       else Result := True;
  end
  else begin
       if (cbFilename.Items.Count > 0)
       then begin
            TmpName := ChangeFileExt(cbFilename.Items[0], '');
            if (CompareText(ExtractFileName(Name), TmpName) = 0)
            then begin
                 Ext := lowercase(ExtractFileExt(cbFilename.Items[0]));
                 if (cbFileOfType.ItemIndex >= 0)
                 then begin
                      i := integer(cbFileOfType.Items.Objects[cbFileOfType.ItemIndex]);
                      if (0 <= i) and (i < FExtList.Count)
                      then if (Pos(Ext, FExtList.Strings[i]) <> 0)
                           then Result := True;
                 end;

            end;
       end;
  end;
end; // TFormOpenFile.CheckFileName.


procedure TFormOpenFile.CheckFileCreate;
var MsgStr    : String;
    MsgResult : word;
begin
  if (ofCreatePrompt in FOptions)
  then begin
       if Not(FileExists(Filename))
       then begin
            FCanClose := True;
            MsgStr := Format(resFileCreatePrompt, [Filename]);
            MsgResult := MessageDlg(MsgStr, mtConfirmation, [mbYes, mbNo, mbCancel], HelpContext);
            case MsgResult of
            mrNo     : ModalResult := mrCancel;
            mrCancel : FCanClose := False;
            end;
       end;
  end;
end; // TFormOpenFile.CheckFileCreate.


procedure TFormOpenFile.btnOpenClick(Sender : TObject);
var TempFileName : String;
begin
  ModalResult := mrNone;
  TempFileName := AppendExtension(ShellListView.FileName);

  if Not(FileExists(TempFileName))
  then begin
       if FileExists(cbFilename.Text)
       then TempFileName := cbFilename.Text
       else begin
            if {$IFDEF GE_DXE2} System.SysUtils.{$ENDIF}DirectoryExists(cbFilename.Text) or
               {$IFDEF GE_DXE2} System.SysUtils.{$ENDIF}DirectoryExists(ShellComboBox.Path + cbFilename.Text)
            then begin
                 TempFileName := cbFilename.Text;
                 cbFilename.Text := '';
                 if {$IFDEF GE_DXE2} System.SysUtils.{$ENDIF}DirectoryExists(ShellComboBox.Path + TempFileName)
                 then InitDir := ShellComboBox.Path + TempFileName
                 else InitDir := TempFileName;
                 btnOpen.Enabled := False;
            end;
            TempFileName := '';
       end;
  end;

  if FileExists(TempFileName)
  then begin
       ShellListView.Filename := TempFileName;
       CheckFileCreate;
       mcmImageCtrl.Clear;
       ModalResult := mrOK;
  end;
end; // TFormOpenFile.btnOpenClick.


procedure TFormOpenFile.btnCancelClick(Sender : TObject);
begin
  mcmImageCtrl.Clear;
  FCanClose := True;
end; // TFormOpenFile.btnCancelClick.


procedure TFormOpenFile.btnHelpClick(Sender : TObject);
begin
  Application.HelpContext(HelpContext);
end; // TFormOpenFile.btnHelpClick.


procedure TFormOpenFile.ShellComboBoxChange(Sender : TObject);
var CurPath : String;
begin
  if Not(Assigned(FHistory))
  then Exit;
  // Add current path to history list.
  CurPath := ShellComboBox.Path;
  FHistory.Add(CurPath);
  // If history list contains more than one entry enable "go back" button.
  if (FHistory.Count > 1)
  then tbBack.Enabled := True
  else tbBack.Enabled := False;

  ShellListFolderChanged;
  if Assigned(FOnFolderChange)
  then FOnFolderChange(Self);
end; // TFormOpenFile.ShellComboBoxChange.


procedure TFormOpenFile.tbBackClick(Sender : TObject);
var PreviousPath : String;
begin
  // Go back to previously visited folder.
  if (FHistory.Count > 0)
  then begin
       PreviousPath := FHistory.Strings[FHistory.Count-1];
       // If PreviousPath equals current path, pick next entry in list.
       if (FHistory.Count > 1) and (ShellComboBox.Path = PreviousPath)
       then begin
            FHistory.Delete(FHistory.Count-1);
            PreviousPath := FHistory.Strings[FHistory.Count-1];
       end;
       ShellComboBox.Path := PreviousPath;
       FHistory.Delete(FHistory.Count-1);
       // If history list has one entry disable "go back" button.
       if (FHistory.Count <= 1)
       then tbBack.Enabled := False;
  end;
end; // TFormOpenFile.tbBackClick.


procedure TFormOpenFile.tbLevelUpClick(Sender : TObject);
begin
  ShellListView.OneLevelUp;
end; // TFormOpenFile.tbLevelUpClick.


procedure TFormOpenFile.tbNewFolderClick(Sender : TObject);
begin
  ShellListView.NewFolder;
end; // TFormOpenFile.tbNewFolderClick.


procedure TFormOpenFile.tbViewClick(Sender : TObject);
var MenuPos : TPoint;
    i       : integer;
begin
  for i := 0 to (ViewMenu.Items.Count - 1)
  do ViewMenu.Items[i].Checked := False;
  i := integer(FViewStyle);
  ViewMenu.Items[i].Checked := True;
  MenuPos.x := 0;
  MenuPos.y := tbView.Height;
  MenuPos := tbView.ClientToScreen(MenuPos);
  ViewMenu.Popup(MenuPos.x, MenuPos.y);
end; // TFormOpenFile.tbViewClick.


procedure TFormOpenFile.LargeIconItemClick(Sender : TObject);
begin
  FViewStyle := vsIcon;
  mcmImageCtrl.Visible := False;
  ShellListView.Width := (btnOpen.Left + btnOpen.Width) - ShellListView.Left;
  ShellListView.ViewStyle := {$IFDEF GE_DXE2}Vcl.{$ENDIF}ComCtrls.vsIcon;
  ShellListView.UpdateView;
end; // TFormOpenFile.LargeIconItemClick.


procedure TFormOpenFile.SmallIconItemClick(Sender : TObject);
begin
  FViewStyle := vsSmallIcon;
  mcmImageCtrl.Visible := False;
  ShellListView.Width := (btnOpen.Left + btnOpen.Width) - ShellListView.Left;
  ShellListView.ViewStyle := {$IFDEF GE_DXE2}Vcl.{$ENDIF}ComCtrls.vsSmallIcon;
  ResizePreviewColumns;
end; // TFormOpenFile.SmallIconItemClick.


procedure TFormOpenFile.ListItemClick(Sender : TObject);
begin
  FViewStyle := vsList;
  mcmImageCtrl.Visible := False;
  ShellListView.Width := (btnOpen.Left + btnOpen.Width) - ShellListView.Left;
  ShellListView.ViewStyle := {$IFDEF GE_DXE2}Vcl.{$ENDIF}ComCtrls.vsList;
  ResizePreviewColumns;
end; // TFormOpenFile.ListItemClick.


procedure TFormOpenFile.DetailsItemClick(Sender : TObject);
var i, s, w, t : integer;
begin
  FViewStyle := vsReport;
  mcmImageCtrl.Visible := False;
  ShellListView.ViewStyle := {$IFDEF GE_DXE2}Vcl.{$ENDIF}ComCtrls.vsReport;
  ShellListView.Width := (btnOpen.Left + btnOpen.Width) - ShellListView.Left;
  if (ShellListView.Columns.Count > 0)
  then begin
       w := ShellListView.ClientWidth;
       s := w;
       ShellListView.Columns.BeginUpdate;
       for i := 1 to (ShellListView.Columns.Count - 1)
       do begin
          t := Round(14 * w div 100);
          s := s - t;
          ShellListView.Columns[i].Width := t;
       end;
       ShellListView.Columns[0].Width := s;
       ShellListView.Columns.EndUpdate;
  end;
  ResizePreviewColumns;
end; // TFormOpenFile.DetailsItemClick.


procedure TFormOpenFile.PreviewItemClick(Sender : TObject);
begin
  FViewStyle := vsPreview;
  ShellListView.ViewStyle := {$IFDEF GE_DXE2}Vcl.{$ENDIF}ComCtrls.vsSmallIcon;
  FormResize(Self);
  //ResizePreviewColumns; not required as FormResize calls this method.
  mcmImageCtrl.Visible := True;

  // Need to show image which is donw by calling ShellListViewFileChange.
  if (ShellListView.Columns.Count > 0)
  then ShellListViewFileChange(Self, ShellListView.FileName);
end; // TFormOpenFile.PreviewItemClick.


procedure TFormOpenFile.ShellListViewFileChange(Sender : TObject; AFileName : String);
var SaveCursor : TCursor;
    w, h       : integer;
begin
  if CheckFileName(AFileName)
  then begin
       // A file was selected.
       btnOpen.Enabled := True;
       if (CompareText(cbFilename.Text, ExtractFileName(AFileName)) <> 0)
       then begin
            cbFilename.Text := ExtractFileName(AFileName);
            FillFileNameList(cbFilename.Text);
       end;
  end
  else begin
       // Changed to another folder.
       if (ofFileMustExist in FOptions)
       then btnOpen.Enabled := False;
       if (ExtractFileName(AFileName) = '') and (cbFilename.Text <> '')
       then begin
            FileName := AFileName + cbFilename.Text;
            Exit;
       end
       else cbFilename.Text := ExtractFileName(AFileName);
  end;

  if mcmImageCtrl.Visible and FileExists(AFileName)
  then begin
       // Show image in preview area.
       if (mcmImageCtrl.Image.ImageInfo.FileName = AFileName)
       then Exit;
       SaveCursor := Screen.Cursor;
       try
         Screen.Cursor := crHourglass;
         mcmImageCtrl.Image.FileOpen(AFileName);
         if (ImageFileManager.Error = EC_OK)
         then begin
              // Image was load.
              if (mcmImageCtrl.Image.ImageFormat <> IF_GREY8)
              then mcmImageCtrl.Image.SetStretchMode(HALFTONE)
              else mcmImageCtrl.Image.SetStretchMode(COLORONCOLOR);
              if (mcmImageCtrl.Width  < mcmImageCtrl.Image.DispWidth) or
                 (mcmImageCtrl.Height < mcmImageCtrl.Image.DispHeight)
              then mcmImageCtrl.ScaleToFit := True
              else begin
                   mcmImageCtrl.ScaleToFit := False;
                   mcmImageCtrl.Scale := 1.0;
              end;
         end
         else begin
              // Image/file could not be loaded. Show unknow file graphics.
              mcmImageCtrl.Clear;
              w := mcmImageCtrl.Width;
              h := mcmImageCtrl.Height;
              mcmImageCtrl.Image.Width := w;
              mcmImageCtrl.Image.Height := h;
              mcmImageCtrl.Image.ImageFormat := IF_RGB24;
              mcmImageCtrl.Image.FillAll(192);
              ilBadImage.Draw(mcmImageCtrl.Image.Canvas,
                              (w - ilBadImage.Width) div 2,
                              (h - ilBadImage.Height) div 2, 0);
              {
              w := w div 2;
              h := h div 2;
              l := 25;

              // Unable to read and display image/file.
              mcmImageCtrl.Image.Canvas.Pen.Color := clBlack;
              mcmImageCtrl.Image.Canvas.Pen.Width := 1;
              mcmImageCtrl.Image.Canvas.Brush.Color := clWhite;
              mcmImageCtrl.Image.Canvas.Rectangle(w - l, h - l, w + l, h + l);
              mcmImageCtrl.Image.Canvas.MoveTo(w - l, h - l);
              mcmImageCtrl.Image.Canvas.LineTo(w + l, h + l);
              mcmImageCtrl.Image.Canvas.MoveTo(w + l, h - l);
              mcmImageCtrl.Image.Canvas.LineTo(w - l, h + l);
              }
              {
              // No good image sign
              mcmImageCtrl.Image.Canvas.Pen.Color := clRed;
              mcmImageCtrl.Image.Canvas.Pen.Width := 6;
              mcmImageCtrl.Image.Canvas.Brush.Color := clWhite;
              l := Round(1.5 * 25);
              mcmImageCtrl.Image.Canvas.Ellipse(w - l, h - l, w + l, h + l);
              l := 25;
              mcmImageCtrl.Image.Canvas.MoveTo(w - l, h - l);
              mcmImageCtrl.Image.Canvas.LineTo(w + l, h + l);
              }
              mcmImageCtrl.ScaleToFit := False;
              mcmImageCtrl.Scale := 1.0;
              mcmImageCtrl.Invalidate;
         end;
       finally
         Screen.Cursor := SaveCursor;
       end;
  end
  else begin
       mcmImageCtrl.Clear;
       mcmImageCtrl.Invalidate;
       //ResizePreviewColumns;
  end;
  // Fire selection change event.
  if Assigned(FOnSelectionChange)
  then FOnSelectionChange(Self);
end; // TFormOpenFile.ShellListViewFileChange.


procedure TFormOpenFile.cbFileOfTypeChange(Sender : TObject);
var i, j         : integer;
    EnableOption : boolean;
    Compression  : array[0..127] of TmcmCompress;
    Count        : integer;
begin
  if (0 <= cbFileOfType.ItemIndex) and (cbFileOfType.ItemIndex < cbFileOfType.Items.Count)
  then begin
       ShellListView.FileMask := FExtList.Strings[cbFileOfType.ItemIndex];
       FillFileNameList(cbFilename.Text);
       if (ofFileMustExist in FOptions)
       then if CheckFileName(ShellListView.FileName)
            then btnOpen.Enabled := True
            else btnOpen.Enabled := False;

       ResizePreviewColumns;
       ShellListView.Update;
       if Assigned(FOnTypeChange)
       then FOnTypeChange(Self);

       // ONLY used when saving an image.
       if cbCompression.Visible
       then begin
            // A file format has been select.
            // Update the cbCompression list box with the supported compression
            // methods.
            if (cbFileOfType.ItemIndex < 0)
            then Exit;

            cbCompression.Clear;
            cbCompression.Enabled := False;
            lCompression.Enabled  := False;

            // Update compression formats.
            FFileFormat := TmcmFileFormat(cbFileOfType.Items.Objects[cbFileOfType.ItemIndex]);

            Count := 0;
            if FEnableAllFormats
            then Count := FFileManager.GetCompressionFromColor(FFileFormat, IF_NONE, Compression)
            else if Assigned(FImage)
                 then if Not(FImage.Empty)
                      then Count := FFileManager.GetCompressionFromColor(FFileFormat, FImage.ImageFormat, Compression);

            j := -1;
            for i := 0 to (Count - 1)
            do begin
               cbCompression.Items.AddObject(FFileManager.GetCompressionName(FFileFormat, Compression[i]), pointer(Compression[i]));
               if (FCompression = Compression[i])
               then j := cbCompression.Items.Count - 1;
            end;

            // Select/show the format used to store the image.
            if (j > -1)
            then cbCompression.ItemIndex := j
            else cbCompression.ItemIndex := 0;

            // Disable cbCompression if item count is one.
            if (cbCompression.Items.Count > 1)
            then cbCompression.Enabled := True;
            lCompression.Enabled  := cbCompression.Enabled;

            // Enable option button if file format is GIF, JPEG, PNG or TIFF.
            case FFileFormat of
            // FF_CGM,
            FF_CRI,
            // FF_DICOM,
            // FF_CUT,
            // FF_FPX,
            // FF_IMG,
            // FF_KDC,
            // FF_PCD,
            // FF_PICT,
            // FF_PSP,
            FF_PBM,
            FF_PGM,
            FF_PPM,
            FF_SGI,
            FF_TIFF : if cbCompression.Enabled
                      then begin
                           if (integer(cbCompression.Items.Objects[cbCompression.ItemIndex]) = CP_JPEG_STD)
                           then btnOption.Enabled := True
                           else btnOption.Enabled := False;
                      end;
            FF_TARGA,
            FF_ICO,
            FF_BMP,
            FF_DIB,
            FF_PCX : btnOption.Enabled := False;
            // FF_NEF,
            // FF_EMF,
            // FF_WMF,
            // FF_WPG,
            // FF_JPG2K,
            FF_GIF,
            FF_JPEG,
            FF_PNG  : btnOption.Enabled := True;
            else begin
                 if Assigned(FOnHasOption)
                 then begin
                      EnableOption := False;
                      FOnHasOption(Self, FFileFormat, EnableOption);
                      btnOption.Enabled := EnableOption;
                 end
                 else btnOption.Enabled := False;
            end;
            end;
       end;
  end;
end; // TFormOpenFile.cbFileOfTypeChange.


procedure TFormOpenFile.cbCompressionChange(Sender : TObject);
begin
  // ONLY used when saving an image.
  if cbCompression.Visible
  then begin
       if (0 <= cbCompression.ItemIndex) and (cbCompression.ItemIndex < cbCompression.Items.Count)
       then begin
            // Enable option button if file format is GIF, JPEG, PNG or TIFF.
            case FFileFormat of
            FF_TIFF : if cbCompression.Enabled
                      then begin
                           if (integer(cbCompression.Items.Objects[cbCompression.ItemIndex]) = CP_JPEG_STD)
                           then btnOption.Enabled := True
                           else btnOption.Enabled := False;
                      end;
            end;
       end;
  end;
end; // TFormOpenFile.cbCompressionChange.


procedure TFormOpenFile.ShellListViewClick(Sender : TObject);
begin
  // Disable Open button if selected item is a folder and file must exist.
  if (ShellListView.Index >= 0)
  then begin
       if (TmcmShellFolder(ShellListView.Selected.Data).IsFolder)
       then if CheckFileName(ShellListView.FileName)
            then btnOpen.Enabled := True
            else if (ofFileMustExist in FOptions)
                 then btnOpen.Enabled := False;
  end;
end; // TFormOpenFile.ShellListViewClick.


procedure TFormOpenFile.ShellListViewDblClick(Sender : TObject);
begin
  // Open file, if item double-clicked on is a file item.
  if (ShellListView.Index >= 0)
  then if Not(TmcmShellFolder(ShellListView.Selected.Data).IsFolder)
       then begin
            btnOpen.Click;
            Exit;
       end;
end; // TFormOpenFile.ShellListViewDblClick.


procedure TFormOpenFile.ShellListFolderChanged;
var i : integer;
begin
  // Folder has changed.
  ResizePreviewColumns;
  ShellListView.Update;

  // Scroll to top.
  i := 0;
  if (ShellListView.ViewStyle = {$IFDEF GE_DXE2}Vcl.{$ENDIF}ComCtrls.vsIcon) or
     (ShellListView.ViewStyle = {$IFDEF GE_DXE2}Vcl.{$ENDIF}ComCtrls.vsSmallIcon)
  then begin
       i := -ShellListView.ViewOrigin.y;
  end
  else begin
       if Assigned(ShellListView.TopItem)
       then i := -ShellListView.TopItem.Top;
  end;
  ShellListView.Scroll(0, i);
end; // TFormOpenFile.ShellListFolderChanged.


//------------------------------------------------------------------------------
// cbFilename drop down and editing methods.
//------------------------------------------------------------------------------

procedure TFormOpenFile.FillFileNameList(MatchName : String);
var i : integer;
    s : String;
begin
  MatchName := LowerCase(MatchName);
  cbFilename.Items.Clear;
  for i := 0 to (ShellListView.Count - 1)
  do begin
     if Not(TmcmShellFolder(ShellListView.Items[i].Data).IsFolder)
     then begin
          s := ShellListView.Items[i].Caption;
          if (Pos(MatchName, LowerCase(s)) = 1)
          then begin
               cbFilename.Items.Add(s);
               if (cbFilename.Items.Count = 1)
               then ;//SendMessage(cbFilename.Handle, WM_CANCELMODE, 0, 0);
          end;
     end;
  end;
end; // TFormOpenFile.FillFileNameList.


const NavKeys = [VK_PRIOR,
                 VK_NEXT,
                 VK_END,
                 VK_HOME,
                 VK_LEFT,
                 VK_UP,
                 VK_RIGHT,
                 VK_DOWN,
                 VK_SHIFT,
                 VK_CONTROL,
                 VK_MENU,
                 VK_F1..VK_F24, VK_NUMLOCK];


procedure TFormOpenFile.cbFilenameKeyUp(    Sender : TObject;
                                        var Key    : Word;
                                            Shift  : TShiftState);
var s1        : String;
    i         : integer;
begin
  if (Key = VK_RETURN)
  then Exit;
  if (Key = VK_ESCAPE) or (Key = VK_TAB)
  then begin
       if (Key = VK_ESCAPE)
       then cbFilename.Text := FOldText;
       SendMessage(cbFilename.Handle, CB_SHOWDROPDOWN, 0, 0);
  end;
  if (cbFilename.Text <> '')
  then begin
       if Not(Key in NavKeys) or (Key = VK_DELETE) or (Key = VK_BACK)
       then begin
            s1 := LowerCase(cbFilename.Text);
            if (CompareText(FOldText, cbFilename.Text) <> 0)
            then begin
                 i := cbFilename.SelStart;
                 FOldText := cbFilename.Text;
                 cbFilename.Items.Clear;
                 FillFileNameList(s1);
                 cbFilename.SelStart := i;
                 cbFilename.SelLength := 0;
            end;
            s1 := cbFilename.Text;

            if (cbFilename.Items.Count > 0)
            then begin
                 if Not((Key = VK_DELETE) or (Key = VK_BACK))
                 then begin
                      SendMessage(cbFilename.Handle, WM_CANCELMODE, 0, 0);
                      if Not(cbFilename.DroppedDown)
                      then begin
                           // SendMessage(cbFilename.Handle, CB_SHOWDROPDOWN, 1, 0);
                           cbFilename.Text := s1;
                      end;
                 end;
            end
            else SendMessage(cbFilename.Handle, CB_SHOWDROPDOWN, 0, 0);
       end;
  end
  else begin
       FOldText := '';
       btnOpen.Enabled := False;
       Exit;
       //SendMessage(cbFilename.Handle, CB_SHOWDROPDOWN, 0, 0);
  end;

  if (Self is TFormSaveFile)
  then begin
       if (ExtractFileName(cbFilename.Text) = cbFilename.Text)
       then begin
            if (cbFilename.Text <> '')
            then btnOpen.Enabled := True
            else btnOpen.Enabled := False;
       end
       else begin
            if {$IFDEF GE_DXE2} System.SysUtils.{$ENDIF}DirectoryExists(ExtractFilePath(cbFilename.Text))
            then btnOpen.Enabled := True
            else btnOpen.Enabled := False; 
       end;
       if btnOpen.Enabled
       then ShellListView.FileName := ShellComboBox.Path + cbFilename.Text;
  end
  else begin
       if FileExists(ShellComboBox.Path + cbFilename.Text)
       then ShellListView.FileName := ShellComboBox.Path + cbFilename.Text
       else begin
            if FileExists(cbFilename.Text)
            then btnOpen.Enabled := True
            else begin
                 if ({$IFDEF GE_DXE2} System.SysUtils.{$ENDIF}DirectoryExists(cbFilename.Text) or
                     {$IFDEF GE_DXE2} System.SysUtils.{$ENDIF}DirectoryExists(ShellComboBox.Path + cbFilename.Text)) and
                    (cbFilename.Text[1] <> '.')
                 then btnOpen.Enabled := True
                 else btnOpen.Enabled := False;
            end;
       end;
  end;
end; // TFormOpenFile.cbFilenameKeyUp.


procedure TFormOpenFile.cbFilenameKeyDown(    Sender : TObject;
                                          var Key    : Word;
                                              Shift  : TShiftState);
var i : integer;
begin
  if (Key = VK_ESCAPE)
  then begin
       i := cbFilename.SelStart;
       SendMessage(cbFilename.Handle, WM_CANCELMODE, 0, 0);
       SendMessage(cbFilename.Handle, CB_SHOWDROPDOWN, 0, 0);
       cbFilename.Text := FOldText;
       cbFilename.SelStart := i;
       cbFilename.SelLength := 0;
  end
  else FOldText := cbFilename.Text;
end; // TFormOpenFile.cbFilenameKeyDown.


procedure TFormOpenFile.cbFilenameClick(Sender : TObject);
begin
  ShellListView.FileName := ShellListView.Path + cbFilename.Text;
  FOldText := '';
end; // TFormOpenFile.cbFilenameClick.


procedure TFormOpenFile.cbFilenameDropDown(Sender : TObject);
begin
  cbFilename.Text := FOldText;
end; // TFormOpenFile.cbFilenameDropDown.


procedure TFormOpenFile.FormCloseQuery(Sender : TObject; var CanClose : Boolean);
begin
  if ShellListView.IsEditing
  then CanClose := False
  else CanClose := FCanClose;
end; // TFormOpenFile.FormCloseQuery.


procedure TFormOpenFile.FormClose(Sender : TObject; var Action : TCloseAction);
begin
  if Assigned(FOnClose)
  then FOnClose(Sender);
end; // TFormOpenFile.FormClose.


procedure TFormOpenFile.FormShow(Sender : TObject);
begin
  if Assigned(FOnShow)
  then FOnShow(Sender);
end; // TFormOpenFile.FormShow.


//------------------------------------------------------------------------------
// TFormSaveFile
//------------------------------------------------------------------------------

procedure TFormSaveFile.FormCreate(Sender : TObject);
begin
  Inherited FormCreate(Sender);
  FCompression := CP_NOCOMP;
  cbCompression.Visible := True;
  lCompression.Visible := True;
  btnOption.Visible := True;
  PreviewItem.Visible := False;
  Caption := resSave;
  btnOpen.Caption := resSave;
  btnOpen.OnClick := btnSaveClick;
  FFormats := TmcmFileFormatsList.Create;
end; // TFormSaveFile.FormCreate.


procedure TFormSaveFile.FormDestroy(Sender : TObject);
begin
  FFormats.Free;
  Inherited FormDestroy(Sender);
end; // TFormSaveFile.FormDestroy.


procedure TFormSaveFile.btnSaveClick(Sender : TObject);
var FileFormat : TmcmFileFormat;
    MessageStr : String;
    Ext        : String;
begin
  ModalResult := mrOK;
  if Assigned(FFileManager) and Assigned(FImage)
  then begin
       if (cbFileOfType.ItemIndex >= 0)
       then begin
            FileFormat  := TmcmFileFormat(cbFileOfType.Items.Objects[cbFileOfType.ItemIndex]);
            if (FileFormat < FF_NONE)
            then FDefaultExt := FFileManager.GetDefaultExtension(FileFormat, FImage.ImageFormat);
       end;
  end;

  // Is the extension legal.
  if (cbFileOfType.ItemIndex >= 0)
  then begin
       if (cbFileOfType.ItemIndex < FExtList.Count)
       then begin
            Ext := ExtractFileExt(FileName);
            if (Pos(Ext, FExtList.Strings[cbFileOfType.ItemIndex]) = 0)
            then FileName := ChangeFileExt(FileName, '.' + FDefaultExt);
       end;
  end;

  // Does file name include an extension.
  Ext := ExtractFileExt(FileName);
  if (Ext = '')
  then FileName := ChangeFileExt(FileName, '.' + FDefaultExt);

  if (ofOverwritePrompt in FOptions)
  then begin
       if (FileExists(FileName))
       then begin
            MessageStr := Format(resOverwriteFile1, [FileName]) + chr($0D) + resOverwriteFile2;
            if (MessageDlg(MessageStr, mtConfirmation, [mbYes, mbNo], HelpContext) = mrNo)
            then begin
                 ModalResult := mrNone;
                 Exit;
            end;
       end;
  end;

  CheckFileCreate;

  if (0 <= cbCompression.ItemIndex)
  then FCompression := TmcmCompress(cbCompression.Items.Objects[cbCompression.ItemIndex])
  else FCompression := CP_NOCOMP;
end; // TFormSaveFile.btnSaveClick.


procedure TFormSaveFile.SetEnableAllFormats(Value : boolean);
begin
  if (FEnableAllFormats <> Value)
  then begin
       FEnableAllFormats := Value;
  end;
end; // TFormSaveFile.SetEnableAllFormats.


procedure TFormSaveFile.SetFileManager(Value : TmcmImageFileMgr);
var Extension : String;
    DispExt   : String;
    j, k      : integer;
begin
  // Image must be assigned before assigning the ImageFileManager, for proper
  // initialization.
  FFileManager := Value;
  if Assigned(FFileManager) and Assigned(FImage)
  then begin
       // Update the cbFileFormat list box to show the file formats that supports the
       // current image format (color resolution/bit depth).
       if Not(FImage.Empty)
       then begin
            k := -1;

            if FEnableAllFormats
            then FFileManager.GetFormatsFromColor(IF_NONE, FFormats)
            else if Assigned(FImage)
                 then FFileManager.GetFormatsFromColor(FImage.ImageFormat, FFormats);

            for j := 0 to (FFormats.Count - 1)
            do begin
               if TmcmFileFormatItem(FFormats.Items[j]).WriteEnabled
               then begin
                    DispExt := TmcmFileFormatItem(FFormats.Items[j]).Description;
                    Extension := TmcmFileFormatItem(FFormats.Items[j]).GetFilter;
                    FExtList.Add(Extension);

                    // Add display file mask and file format definition = FF_xxx.
                    DispExt := DispExt + ' (' + Extension + ')';
                    cbFileOfType.Items.AddObject(DispExt, pointer(TmcmFileFormatItem(FFormats.Items[j]).FileClassID));

                    // Get image's current file format.
                    if Assigned(FImage)
                    then if Assigned(FImage.ImageInfo)
                         then if (FImage.ImageInfo.FileFormat = TmcmFileFormatItem(FFormats.Items[j]).FileClassID)
                              then k := cbFileOfType.Items.Count - 1;
               end;
            end;

            // Select/show the format used to store the image.
            cbFileOfType.ItemIndex := k;

            // Update compression list
            cbFileOfTypeChange(Self);
       end
       else begin
            Extension := FFileManager.GetWriteFilter;
            AddExtensions(Extension);
            cbCompression.Enabled := False;
            lCompression.Enabled  := False;
       end;
  end
  else begin
       if Assigned(FFileManager)
       then begin
            Extension := FFileManager.GetWriteFilter;
            AddExtensions(Extension);
            cbCompression.Enabled := False;
            lCompression.Enabled  := False;
       end;
  end;
end; // TFormSaveFile.SetFileManager.


function TFormSaveFile.GetImage : TmcmImage;
begin
  Result := FImage;
end; // TFormSaveFile.GetImage.


procedure TFormSaveFile.SetImage(AImage : TmcmImage);
begin
  // Set/FImage is only used when saving an image, to control
  // which file formats and compressions can be used.
  FImage := AImage;
end; // TFormSaveFile.SetImage.


procedure TFormSaveFile.SetOptions(Value : TOpenOptions);
begin
  //Value := Value - [ofFileMustExist];
  Inherited SetOptions(Value);
end; // TFormSaveFile.SetOptions.


function TFormSaveFile.GetCompression : TmcmCompress;
begin
  Result := FCompression;
end; // TFormSaveFile.GetCompression.


procedure TFormSaveFile.SetCompression(Value : TmcmCompress);
begin
  FCompression := Value;
end; // TFormSaveFile.SetCompression.


function TFormSaveFile.GetInterlaced : boolean;
begin
  Result := FInterlaced;
end; // TFormSaveFile.GetInterlaced.


procedure TFormSaveFile.SetInterlaced(Value : boolean);
begin
  FInterlaced := Value;
end; // TFormSaveFile.SetInterlaced.


function TFormSaveFile.GetQuality : integer;
begin
  Result := FQuality;
end; // TFormSaveFile.GetQuality.


procedure TFormSaveFile.SetQuality(Value : integer);
begin
  FQuality := Value;
  if (FQuality < 1)
  then FQuality := 1;
  if (FQuality > 100)
  then FQuality := 100;
end; // TFormSaveFile.SetQuality.


procedure TFormOpenFile.btnOptionClick(Sender : TObject);
var AFormat : TmcmFileFormat;
begin
  // Only accessed when in "save" mode.
  if (cbFileOfType.ItemIndex >= 0)
  then AFormat := TmcmFileFormat(cbFileOfType.Items.Objects[cbFileOfType.ItemIndex])
  else Exit;

  if (AFormat = FF_GIF) or
     (AFormat = FF_PNG) or
     (AFormat = FF_JPEG) or
     (AFormat = FF_TIFF)
  then begin
       FormCompressOption := TFormCompressOption.Create(Self);
       FormCompressOption.Left := Self.Left + (Self.Width - FormCompressOption.Width) div 2;
       FormCompressOption.Top  := Self.Top + (Self.Height - FormCompressOption.Height) div 2;
       FormCompressOption.ShowHelp(ofShowHelp in FOptions);
       FormCompressOption.HelpContext := Self.FOptionHelp;

       FormCompressOption.FileFormat := AFormat;
       FormCompressOption.Interlaced := FInterlaced;
       FormCompressOption.Quality    := FQuality;

       if (FormCompressOption.ShowModal = mrOK)
       then begin
            FInterlaced := FormCompressOption.Interlaced;
            FQuality    := FormCompressOption.Quality;
       end;
       FormCompressOption.Free;
  end
  else if (AFormat > FF_USERDEFINED) and Assigned(FOnShowOption)
       then FOnShowOption(Self, AFormat);
end; // TFormOpenFile.btnOptionClick.

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
