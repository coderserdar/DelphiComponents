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
// $Log:  17537: mcmFileDialogues.pas 
//
//    Rev 1.7    2014-02-02 21:09:54  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.6    29-01-2006 12:21:02  mcm    Version: IMG 2.14
// Initial Unicode support.
//
//   Rev 1.5    23-08-2005 23:18:04  mcm    Version: IMG 2.9
// Added two new events to support external file formats. The events
// OnSaveHasOption and OnSaveShowOption. 
// Added option of showing all file formates and compression methods, ie. no
// color resolution checking.

//
//   Rev 1.4    25-06-2004 21:03:46  mcm    Version: IMG 2.5
// Added support for compressions, interlace mode, etc.

//
//   Rev 1.3    31-05-2004 23:55:08  mcm    Version: IMG 2.5
// Implemented Save dialogue.

//
//   Rev 1.2    29-09-2003 18:44:32  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.1    27-01-2003 13:52:58  mcm

//
//   Rev 1.0    27-05-2002 16:21:58  mcm

unit mcmFileDialogues;

interface

{$R-}
{$Include 'mcmDefines.pas'}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Controls, Graphics,
      Dialogs, Buttons, StdCtrls, ComCtrls, ExtCtrls,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Dialogs, Vcl.Buttons, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
     {$ENDIF}
     mcmFileOpenDialog,
     mcmImage,
     mcmImageFile,
     mcmImageTypeDef;

//------------------------------------------------------------------------------
// TmcmOpenDialog
//------------------------------------------------------------------------------

type
  TmcmOpenDialog = class(TComponent)
  private
    FFileDialog  : TFormOpenFile;
    FFileManager : TmcmImageFileMgr;
    FViewStyle   : TmcmViewStyle;

    FHelpContext : THelpContext;
    FCtl3D       : boolean;
    FDefaultExt  : WideString;
    FFileName    : TFileName;
    FFiles       : TStrings;
    FFilter      : WideString;
    FFilterIndex : Integer;
    FInitialDir  : WideString;
    FOptions     : TOpenOptions;
    FTitle       : WideString;

    FOnClose           : TNotifyEvent;
    FOnFolderChange    : TNotifyEvent;
    FOnSelectionChange : TNotifyEvent;
    FOnShow            : TNotifyEvent;
    FOnTypeChange      : TNotifyEvent;
  protected
    procedure   DoSelectionChange(Sender : TObject);
    procedure   DoFolderChange(Sender : TObject);
    procedure   DoTypeChange(Sender : TObject);
    function    GetFileName : TFileName;
    function    GetFilterIndex : integer;
    procedure   SetFileManager(Value : TmcmImageFileMgr);
    procedure   SetFileName(Value : TFileName);
    procedure   SetFilter(Value : WideString);
    procedure   SetFilterIndex(Value : integer);
    procedure   SetInitialDir(Value : WideString);
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    function    Execute : boolean; virtual;
    property    Files : TStrings
      read      FFiles;
    property    ImageFileManager : TmcmImageFileMgr
      read      FFileManager
      write     SetFileManager;
  published
    property    Ctl3D : boolean
      read      FCtl3D
      write     FCtl3D default True;
    property    DefaultExt : WideString
      read      FDefaultExt
      write     FDefaultExt;
    property    FileName : TFileName
      read      GetFileName
      write     SetFileName;
    property    Filter : WideString
      read      FFilter
      write     SetFilter;
    property    FilterIndex : Integer
      read      GetFilterIndex
      write     SetFilterIndex default 0;
    property    HelpContext : THelpContext
      read      FHelpContext
      write     FHelpContext;
    property    InitialDir : WideString
      read      FInitialDir
      write     SetInitialDir;
    property    Options : TOpenOptions
      read      FOptions
      write     FOptions default [ofHideReadOnly];
    property    Title : WideString
      read      FTitle
      write     FTitle;
    property    ViewStyle : TmcmViewStyle
      read      FViewStyle
      write     FViewStyle default vsList;

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

//------------------------------------------------------------------------------
// TmcmSaveDialog
//------------------------------------------------------------------------------

  TmcmSaveDialog = class(TmcmOpenDialog)
  private
    FOptionHelpContext : THelpContext;
    FImage        : TmcmImage;
    FCompression  : TmcmCompress;
    FInterlaced   : boolean;
    FQuality      : integer;
    FOnHasOption  : TOnSaveHasOption;
    FOnShowOption : TOnSaveShowOption;
    FEnableAllFmt : boolean;
  protected
    procedure   DoClose;
    function    GetInterlaced : boolean;
    function    GetQuality : integer;
    procedure   SetEnableAllFormats(Value : boolean);
    procedure   SetImage(AImage : TmcmImage);
    procedure   SetCompression(Value : TmcmCompress);
    procedure   SetInterlaced(Value : boolean);
    procedure   SetQuality(Value : integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    Execute: Boolean; override;
    property    Compression : TmcmCompress
      read      FCompression
      write     SetCompression;
    property    Image : TmcmImage
      read      FImage
      write     SetImage;
    property    Interlaced : boolean
      read      GetInterlaced
      write     SetInterlaced;
    property    Quality : integer
      read      GetQuality
      write     SetQuality;
  published
    property    EnableAllFormats : boolean
      read      FEnableAllFmt
      write     SetEnableAllFormats default False;
    property    OptionHelpContext : THelpContext
      read      FOptionHelpContext
      write     FOptionHelpContext;
    // Events
    property    OnClose;
    property    OnFolderChange;
    property    OnHasOption : TOnSaveHasOption
      read      FOnHasOption
      write     FOnHasOption;
    property    OnShowOption : TOnSaveShowOption
      read      FOnShowOption
      write     FOnShowOption;
    property    OnSelectionChange;
    property    OnShow;
    property    OnTypeChange;
  end;


implementation


Uses {$IFNDEF GE_DXE2}
      Forms,
     {$ELSE}
      Vcl.Forms,
     {$ENDIF}
     mcmImageResStr;

{$R mcmDialogs.res}

//------------------------------------------------------------------------------
// TmcmOpenDialog
//------------------------------------------------------------------------------

constructor TmcmOpenDialog.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FFileDialog  := Nil;
  FDefaultExt  := '';
  FFileManager := Nil;
  FFileName    := '';
  FFiles       := TStringList.Create;
  FFilter      := '';
  FFilterIndex := 0;
  FOptions     := [ofHideReadOnly];
  FTitle       := resOpen;
  FViewStyle   := vsList;
  FCtl3D       := True;

  FOnClose           := Nil;
  FOnFolderChange    := Nil;
  FOnSelectionChange := Nil;
  FOnShow            := Nil;
  FOnTypeChange      := Nil;
end; // TmcmOpenDialog.Create.


destructor TmcmOpenDialog.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end; // TmcmOpenDialog.Destroy.


procedure TmcmOpenDialog.SetFileManager(Value : TmcmImageFileMgr);
begin
  FFileManager := Value;
  if (FFileDialog <> Nil)
  then FFileDialog.ImageFileManager := FFileManager;
end; // TmcmOpenDialog.SetFileManager.


function TmcmOpenDialog.GetFileName : TFileName;
begin
  if (FFileDialog <> Nil)
  then FFileName := FFileDialog.Filename;
  Result := FFileName;
end; // TmcmOpenDialog.GetFileName.


procedure TmcmOpenDialog.SetFileName(Value : TFileName);
begin
  FFileName := Value;
  if (FFileDialog <> Nil)
  then FFileDialog.Filename := FFileName;
end; // TmcmOpenDialog.SetFileName.


function TmcmOpenDialog.GetFilterIndex : integer;
begin
  if (FFileDialog <> Nil)
  then FFilterIndex := FFileDialog.FilterIndex;
  Result := FFilterIndex;
end; // TmcmOpenDialog.GetFilterIndex.


procedure TmcmOpenDialog.SetFilter(Value : WideString);
begin
  FFilter := Value;
  if (FFileDialog <> Nil)
  then FFileDialog.Filter := FFilter;
end; // TmcmOpenDialog.SetFilter.


procedure TmcmOpenDialog.SetFilterIndex(Value : integer);
begin
  FFilterIndex := Value;
  if (FFileDialog <> Nil)
  then FFileDialog.FilterIndex := FFilterIndex;
end; // TmcmOpenDialog.SetFilterIndex.


procedure TmcmOpenDialog.SetInitialDir(Value : WideString);
begin
  FInitialDir := Value;
  if (FFileDialog <> Nil)
  then FFileDialog.InitDir := FInitialDir;
end; // TmcmOpenDialog.SetInitialDir.


procedure TmcmOpenDialog.DoSelectionChange(Sender : TObject);
begin
  if Assigned(FOnSelectionChange)
  then FOnSelectionChange(Self);
end; // TmcmOpenDialog.DoSelectionChange.


procedure TmcmOpenDialog.DoFolderChange(Sender : TObject);
begin
  if Assigned(FOnFolderChange)
  then FOnFolderChange(Self);
end; // TmcmOpenDialog.DoFolderChange.


procedure TmcmOpenDialog.DoTypeChange(Sender : TObject);
begin
  if Assigned(FOnTypeChange)
  then FOnTypeChange(Self);
end; // TmcmOpenDialog.DoTypeChange.


function TmcmOpenDialog.Execute : boolean;
var i       : integer;
    SelItem : TListItem;
begin
  FFileDialog := TFormOpenFile.Create(Self);
  try
    if (Owner is TWinControl)
    then begin
         FFileDialog.Top  := TWinControl(Owner).Top + 100;
         FFileDialog.Left := TWinControl(Owner).Left + 60;
    end;
    FFileDialog.Ctl3D := FCtl3D;
    FFileDialog.HelpContext := FHelpContext;
    FFileDialog.OnClose := FOnClose;
    FFileDialog.OnFolderChange := FOnFolderChange;
    FFileDialog.OnSelectionChange := DoSelectionChange;
    FFileDialog.OnShow := FOnShow;
    FFileDialog.OnTypeChange := FOnTypeChange;

    FFileDialog.Caption := FTitle;
    FFileDialog.Options := FOptions;
    FFileDialog.ImageFileManager := ImageFileManager;
    FFileDialog.Filter := FFilter;

    FFileDialog.InitDir := FInitialDir;
    FFileDialog.DefaultExt := FDefaultExt;
    FFileDialog.Filename := FFileName;
    FFileDialog.FilterIndex := FFilterIndex;
    FFileDialog.ViewStyle := FViewStyle;

    if (FFileDialog.ShowModal = mrOK)
    then begin
         FInitialDir  := FFileDialog.InitDir;
         FFileName    := FFileDialog.Filename;
         FFilterIndex := FFileDialog.FilterIndex;
         FViewStyle   := FFileDialog.ViewStyle;
         FOptions     := FFileDialog.Options;

         // When multiple files are selected add these to FFiles.
         FFiles.Clear;
         if (FFileDialog.ShellListView.SelCount > 1)
         then begin
              SelItem := FFileDialog.ShellListView.Selected;
              if (SelItem <> Nil)
              then FFiles.Add(FInitialDir + SelItem.Caption);
              for i := 0 to (FFileDialog.ShellListView.SelCount - 2)
              do begin
                 SelItem := FFileDialog.ShellListView.GetNextItem(SelItem, sdAll, [isSelected]);
                 if (SelItem <> Nil)
                 then FFiles.Add(FInitialDir + SelItem.Caption);
              end;
         end;

         Result := True;
    end
    else Result := False;
  finally
    if Assigned(FFileDialog)
    then FFileDialog.Free;
    FFileDialog := Nil;
  end;
end; // TmcmOpenDialog.Execute.


//------------------------------------------------------------------------------
// TmcmSaveDialog
//------------------------------------------------------------------------------

constructor TmcmSaveDialog.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FTitle := resSave;
  FImage := Nil;
  FEnableAllFmt := False;
  FOnHasOption  := Nil;
  FOnShowOption := Nil;
end; // TmcmSaveDialog.Create.


destructor TmcmSaveDialog.Destroy;
begin
  inherited Destroy;
end; // TmcmSaveDialog.Destroy.


procedure TmcmSaveDialog.DoClose;
begin
  // Hide any hint windows left behind.
  Application.HideHint;
end; // TmcmSaveDialog.DoClose.


procedure TmcmSaveDialog.SetEnableAllFormats(Value : boolean);
begin
  FEnableAllFmt := Value;
end; // TmcmSaveDialog.SetEnableAllFormats.


procedure TmcmSaveDialog.SetImage(AImage : TmcmImage);
begin
  FImage := AImage;
end; // TmcmSaveDialog.SetImage.


procedure TmcmSaveDialog.SetCompression(Value : TmcmCompress);
begin
  FCompression := Value;
end; // TmcmSaveDialog.SetCompression.


function TmcmSaveDialog.GetInterlaced : boolean;
begin
  if Assigned(FFileDialog)
  then FInterlaced := TFormSaveFile(FFileDialog).Interlaced;
  Result := FInterlaced;
end; // TmcmSaveDialog.GetInterlaced.


procedure TmcmSaveDialog.SetInterlaced(Value : boolean);
begin
  FInterlaced := Value;
  if Assigned(FFileDialog)
  then TFormSaveFile(FFileDialog).Interlaced := FInterlaced;
end; // TmcmSaveDialog.SetInterlaced.


function TmcmSaveDialog.GetQuality : integer;
begin
  if Assigned(FFileDialog)
  then FQuality := TFormSaveFile(FFileDialog).Quality;
  Result := FQuality;
end; // TmcmSaveDialog.GetQuality.


procedure TmcmSaveDialog.SetQuality(Value : integer);
begin
  FQuality := Value;
  if (FQuality < 1)
  then FQuality := 1;
  if (FQuality > 100)
  then FQuality := 100;
  if Assigned(FFileDialog)
  then TFormSaveFile(FFileDialog).Quality := FQuality;
end; // TmcmSaveDialog.SetQuality.


function TmcmSaveDialog.Execute : Boolean;
begin
  FFileDialog := TFormSaveFile.Create(Self);
  try
    if (Owner is TWinControl)
    then begin
         FFileDialog.Top  := TWinControl(Owner).Top + 100;
         FFileDialog.Left := TWinControl(Owner).Left + 60;
    end;
    FFileDialog.Ctl3D := FCtl3D;
    FFileDialog.HelpContext := FHelpContext;
    TFormSaveFile(FFileDialog).OptionHelpContext := FOptionHelpContext;
    FFileDialog.OnClose := FOnClose;
    FFileDialog.OnFolderChange := FOnFolderChange;
    FFileDialog.OnSelectionChange := DoSelectionChange;
    FFileDialog.OnShow := FOnShow;
    FFileDialog.OnTypeChange := FOnTypeChange;
    TFormSaveFile(FFileDialog).OnHasOption := FOnHasOption;
    TFormSaveFile(FFileDialog).OnShowOption := FOnShowOption;

    FFileDialog.Caption := FTitle;
    FOptions := FOptions - [ofAllowMultiSelect];
    FFileDialog.Options := FOptions;
    TFormSaveFile(FFileDialog).EnableAllFormats := FEnableAllFmt;
    TFormSaveFile(FFileDialog).Image := FImage;
    TFormSaveFile(FFileDialog).Compression := FCompression;
    TFormSaveFile(FFileDialog).Interlaced := FInterlaced;
    TFormSaveFile(FFileDialog).Quality := FQuality;

    FFileDialog.ImageFileManager := ImageFileManager;
    FFileDialog.Filter := FFilter;

    if (FInitialDir = '')
    then FFileDialog.InitDir := ExtractFileDir(FFileName)
    else FFileDialog.InitDir := FInitialDir;
    FFileDialog.DefaultExt := FDefaultExt;
    FFileDialog.Filename := FFileName;
    if (FFilterIndex > -1)
    then FFileDialog.FilterIndex := FFilterIndex;
    FFileDialog.ViewStyle := FViewStyle;

    if (FFileDialog.ShowModal = mrOK)
    then begin
         FDefaultExt  := FFileDialog.DefaultExt;
         FInitialDir  := FFileDialog.InitDir;
         FFileName    := FFileDialog.Filename;
         FFilterIndex := FFileDialog.FilterIndex;
         FViewStyle   := FFileDialog.ViewStyle;
         FCompression := TFormSaveFile(FFileDialog).Compression;
         FInterlaced  := TFormSaveFile(FFileDialog).Interlaced;
         FQuality     := TFormSaveFile(FFileDialog).Quality;
         Result := True;
    end
    else Result := False;
  finally
    FImage := Nil;
    if Assigned(FFileDialog)
    then FFileDialog.Free;
    FFileDialog := Nil;
  end;
end; // TmcmSaveDialog.Execute.

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
