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
// $Log:  25392: uFormLoad2TmcmImage.pas 
//
//    Rev 1.2    18-03-2006 18:34:04  mcm    Version: IMG 2.16
// Corrected sample to use UnregisterFileFormat
//
//   Rev 1.1    24-07-2005 18:56:32  mcm    Version: IMG 2.9
// Added Copy and Paste methods.

//
//   Rev 1.0    13-02-2005 19:48:06  mcm    Version: IMG 2.8

unit uFormLoad2TmcmImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  mcmImage, Menus, mcmFileDialogues;

type
  TForm1 = class(TForm)
    mcmImageCtrl : TmcmImageCtrl;
    OpenDialog   : TmcmOpenDialog;
    SaveDialog   : TmcmSaveDialog;
    MainMenu1    : TMainMenu;
    FileMenu     : TMenuItem;
    OpenItem     : TMenuItem;
    SaveItem     : TMenuItem;
    N1           : TMenuItem;
    ExitItem     : TMenuItem;
    EditMenu     : TMenuItem;
    CopyItem     : TMenuItem;
    PasteItem    : TMenuItem;
    procedure FormCreate(Sender : TObject);
    procedure OpenItemClick(Sender : TObject);
    procedure SaveItemClick(Sender : TObject);
    procedure ExitItemClick(Sender : TObject);
    procedure EditMenuClick(Sender : TObject);
    procedure CopyItemClick(Sender : TObject);
    procedure PasteItemClick(Sender : TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var Form1 : TForm1;

implementation

uses ClipBrd, mcmImageFile, mcmImageTypeDef;

{$R *.DFM}

procedure TForm1.FormCreate(Sender : TObject);
begin
  //----------------------------------------------------------------------------
  // Specify which formats to support!
  ImageFileManager.UnregisterFileFormat(TmcmICONImage);

  ImageFileManager.YCbCrMode := JYCC_AUTO;
  OpenDialog.FilterIndex := 1;

  //----------------------------------------------------------------------------
  // Assign the global ImageFileManager to both the mcmOpenDialog and
  // mcmSaveDialog.
  // This enables both dialogues to automatically obtain the supported and
  // enabled file formats & compressions.
  OpenDialog.ImageFileManager := ImageFileManager;
  SaveDialog.ImageFileManager := ImageFileManager;
end; // TForm1.FormCreate.


procedure TForm1.OpenItemClick(Sender : TObject);
begin
  if (OpenDialog.Execute)
  then mcmImageCtrl.Image.FileOpen(OpenDialog.FileName);
end; // TForm1.OpenItemClick.


procedure TForm1.SaveItemClick(Sender : TObject);
begin
  SaveDialog.Image       := mcmImageCtrl.Image;
  SaveDialog.FileName    := mcmImageCtrl.Image.ImageInfo.FileName;
  SaveDialog.Compression := mcmImageCtrl.Image.Compression;
  SaveDialog.Quality     := mcmImageCtrl.Image.Quality;
  SaveDialog.Interlaced  := mcmImageCtrl.Image.Interlaced;
  if (SaveDialog.Execute)
  then begin
       // Set compression matching the file extension CP_xxx ref. TmcmCompress.
       mcmImageCtrl.Image.Compression := SaveDialog.Compression;
       // Set Quality, ref. TmcmImage/TmcmImageFileMgr.Quality
       mcmImageCtrl.Image.Quality     := SaveDialog.Quality;
       // Set Interlaced mode, ref. TmcmImage/TmcmImageFileMgr.Interlaced
       mcmImageCtrl.Image.Interlaced  := SaveDialog.Interlaced;
       // Save image to disk
       mcmImageCtrl.Image.FileSave(SaveDialog.FileName);
  end;
end; // TForm1.SaveItemClick.


procedure TForm1.ExitItemClick(Sender : TObject);
begin
  Close;
end; // TForm1.ExitItemClick.


procedure TForm1.EditMenuClick(Sender : TObject);
begin
  CopyItem.Enabled := Not(mcmImageCtrl.Image.Empty);
  PasteItem.Enabled := Clipboard.HasFormat(CF_DIB);
end; // TForm1.EditMenuClick.


procedure TForm1.CopyItemClick(Sender : TObject);
var AFormat : word;
    AData   : THandle;
begin
  AFormat := CF_DIB;
  mcmImageCtrl.Image.SaveToClipboardFormat(AFormat, AData);
  if (AData <> 0)
  then begin
       Clipboard.Open;
       Clipboard.SetAsHandle(AFormat, AData);
       Clipboard.Close;
  end;
end; // TForm1.CopyItemClick.


procedure TForm1.PasteItemClick(Sender : TObject);
var AFormat : word;
    AData   : THandle;
begin
  Clipboard.Open;
  try
    AFormat :=  CF_DIB; // NOTE: CF_BITMAP is not supported yet.
    AData := Clipboard.GetAsHandle(AFormat);
    if (AData <> 0)
    then mcmImageCtrl.Image.LoadFromClipboardFormat(AFormat, AData);
  finally
    Clipboard.Close;
  end;
end; // TForm1.PasteItemClick.

end.
