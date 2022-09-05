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
// $Log:  26069: uFormRegExternalFile.pas 
//
//    Rev 1.1    31-10-2007 20:22:50  mcm    Version: IMG 3.2
// Moved the WMP file class to mcmWMFFile.pas.
//
//   Rev 1.0    23-08-2005 18:59:50  mcm    Version: IMG 2.9

unit uFormRegExternalFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus,
  mcmImage,
  mcmImageFile,
  mcmWMFFile, // The unit containing the added WMF file format.
  mcmFileDialogues,
  mcmImageTypeDef,
  mcmImageResStr;

type
  TFormMain = class(TForm)
    MainMenu      : TMainMenu;
    FileMenu      : TMenuItem;
    OpenItem      : TMenuItem;
    SaveItem      : TMenuItem;
    ExitItem      : TMenuItem;
    mcmOpenDialog : TmcmOpenDialog;
    mcmSaveDialog : TmcmSaveDialog;
    mcmImageCtrl  : TmcmImageCtrl;
    BrowseItem: TMenuItem;
    procedure OpenItemClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure SaveItemClick(Sender : TObject);
    procedure ExitItemClick(Sender : TObject);
    procedure BrowseItemClick(Sender : TObject);
    procedure mcmSaveDialogHasOption(Sender : TObject; FileFormat : Integer; var HasOption : Boolean);
    procedure mcmSaveDialogShowOption(Sender : TObject; FileFormat : Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var FormMain : TFormMain;

implementation

{$R *.DFM}

uses uFormBrowse;

//------------------------------------------------------------------------------
// Main form.
//------------------------------------------------------------------------------

procedure TFormMain.FormCreate(Sender : TObject);
begin
  //----------------------------------------------------------------------------
  // Specify which formats to support - if not all!
  // We'll exclude the FF_ICO Icon format.
  ImageFileManager.UnregisterFileFormat(TmcmICONImage);

  ImageFileManager.RegisterFileFormat(TImageWMF, FF_MyWMF, 'wmf', resMyWMF, True, True);
  //----------------------------------------------------------------------------
  // Assign the global ImageFileManager to both the mcmOpenDialog and
  // mcmSaveDialog.
  // This enables both dialogues to automatically obtain the supported and
  // enabled file formats & compressions.
  mcmOpenDialog.ImageFileManager := ImageFileManager;
  mcmSaveDialog.ImageFileManager := ImageFileManager;
end; // TFormMain.FormCreate.


procedure TFormMain.OpenItemClick(Sender : TObject);
begin
  if mcmOpenDialog.Execute
  then begin
       mcmImageCtrl.Image.FileOpen(mcmOpenDialog.FileName);
  end;
end; // TFormMain.OpenItemClick.


procedure TFormMain.SaveItemClick(Sender : TObject);
begin
  mcmSaveDialog.FilterIndex := -1; // Image will specify the file format.
                                   // A zero or positive value will instead
                                   // select the indexed file mask.
  mcmSaveDialog.Image       := mcmImageCtrl.Image;
  mcmSaveDialog.FileName    := mcmImageCtrl.Image.ImageInfo.FileName;
  mcmSaveDialog.Compression := mcmImageCtrl.Image.Compression;
  mcmSaveDialog.Quality     := mcmImageCtrl.Image.Quality;
  mcmSaveDialog.Interlaced  := mcmImageCtrl.Image.Interlaced;
  if mcmSaveDialog.Execute
  then begin
       mcmSaveDialog.InitialDir := ExtractFileDir(mcmSaveDialog.FileName);
       mcmOpenDialog.InitialDir := mcmSaveDialog.InitialDir;

       // Get selected compression settings.
       mcmImageCtrl.Image.Compression := mcmSaveDialog.Compression;
       mcmImageCtrl.Image.Interlaced  := mcmSaveDialog.Interlaced;
       mcmImageCtrl.Image.Quality     := mcmSaveDialog.Quality;

       // Store image to disk

       if (mcmSaveDialog.DefaultExt = 'wmf')
       then // Call your own save method
       else mcmImageCtrl.Image.FileSave(mcmSaveDialog.FileName);

       if (ImageFileManager.Error <> EC_OK)
       then ShowMessage('Error writing image: ' + CErrorStrings[word(ImageFileManager.Error)]);
  end;
end; // TFormMain.SaveItemClick.


procedure TFormMain.BrowseItemClick(Sender : TObject);
begin
  if (FormBrowse = Nil)
  then begin
       FormBrowse := TFormBrowse.Create(Self);
//       FormBrowse.mcmThumbView.OnLoadImage := OpenImageFile;
  end;
  FormBrowse.Show;
end; // TFormMain.BrowseItemClick.


procedure TFormMain.ExitItemClick(Sender : TObject);
begin
  Close;
end; // TFormMain.ExitItemClick.


procedure TFormMain.mcmSaveDialogHasOption(    Sender     : TObject;
                                               FileFormat : Integer;
                                           var HasOption  : Boolean);
begin
  // This will enable the Option button in the SaveDialog when the WMF format
  // is selected.
  if (FileFormat = FF_WMF)
  then HasOption := True;
end; // TFormMain.mcmSaveDialogHasOption.


procedure TFormMain.mcmSaveDialogShowOption(Sender     : TObject;
                                            FileFormat : Integer);
begin
  // Create and show your Option dialog now. This Option dialog MUST be modal!
  // The owner of your Option dialog should be "Sender".
  if (FileFormat = FF_WMF)
  then ShowMessage('Show your option dialog');
end; // TFormMain.mcmSaveDialogShowOption.

end.
