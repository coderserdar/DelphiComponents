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
// $Log:  19199: uFormLoad2TImage.pas 
//
//   Rev 1.5    21/07/2005 23:05:18  mcm    Version: IMG 2.9
// Added Copy/Paste methods.

//
//   Rev 1.4    05-01-2005 18:58:34  mcm

//
//   Rev 1.3    06-10-2004 19:20:28  mcm    Version: IMG 2.6

//
//   Rev 1.2    26-09-2004 10:57:38  mcm    Version: IMG 2.6
// Modified compression const name.

//
//   Rev 1.1    08-07-2004 23:28:58  mcm    Version: IMG 2.5

//
//   Rev 1.0    10-02-2003 18:46:54  mcm    Version: IMG 1.3

unit uFormLoad2TImage;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls,
     mcmImage,
     mcmImageFile,
     mcmImageTypeDef,
     mcmFileDialogues;

type
  TForm1 = class(TForm)
    Image1    : TImage;
    MainMenu1 : TMainMenu;
    FileMenu  : TMenuItem;
    OpenItem  : TMenuItem;
    ExitItem  : TMenuItem;
    OpenDialog: TOpenDialog;
    SaveAsItem: TMenuItem;
    SaveDialog: TSaveDialog;
    EditMenu: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    procedure OpenItemClick(Sender : TObject);
    procedure ExitItemClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure SaveAsItemClick(Sender : TObject);
    procedure CopyItemClick(Sender : TObject);
    procedure PasteItemClick(Sender : TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var Form1 : TForm1;

implementation

{$R *.DFM}

uses Clipbrd;

procedure TForm1.FormCreate(Sender : TObject);
var FilterStr : string;
begin
  // Specify which formats to support - if not all!
  // ImageFileManager.SetSupportedFormats([IF_TIFF, IF_BMP, IF_DIB]);
  FilterStr := ImageFileManager.GetReadFilter;
  OpenDialog.Filter := FilterStr;
  OpenDialog.FilterIndex := 15; // Max number of file formats in filter list!

  FilterStr := ImageFileManager.GetWriteFilter;
  SaveDialog.Filter := FilterStr;
  SaveDialog.FilterIndex := 9; // Let's set TIFF as default.
end; // TForm1.FormCreate.


procedure TForm1.OpenItemClick(Sender : TObject);
var FSaveCursor : TCursor;
begin
  if (OpenDialog.Execute)
  then begin
       FSaveCursor := Screen.Cursor;
       Screen.Cursor := crHourGlass;

       if (Image1.Picture.Bitmap = Nil)
       then Image1.Picture.Bitmap := TBitmap.Create;

       // Read image from disk

       // Use ImageFileManager to open file. This allows you to return the pointer to
       // a TBitmap.Handle.
       ImageFileManager.Quality := 100;
       ImageFileManager.Compression := CP_NOCOMP;
       Image1.Picture.Bitmap.Handle := ImageFileManager.LoadImage(OpenDialog.FileName, FF_DETECT);

       Screen.Cursor := FSaveCursor;

       if (ImageFileManager.Error <> EC_OK)
       then ShowMessage('Error reading image: ' + CErrorStrings[word(ImageFileManager.Error)]);
  end;
end; // TForm1.OpenItemClick.


procedure TForm1.SaveAsItemClick(Sender : TObject);
const
    ConvertFormat : array[1..14] of TmcmFileFormat =
                    (FF_CRI, FF_GIF, FF_JPEG, FF_PBM, FF_PGM, FF_PNG, FF_PPM,
                     FF_SGI, FF_TIFF, FF_TARGA, FF_BMP, FF_DIB, FF_ICO, FF_PCX);
var FSaveCursor : TCursor;
    ExtName     : string;
begin
  if Not(Image1.Picture.Bitmap.Empty)
  then begin
       if (SaveDialog.Execute)
       then begin
            SaveDialog.InitialDir := ExtractFileDir(SaveDialog.FileName);

            case ConvertFormat[SaveDialog.FilterIndex] of
            FF_CRI   : ExtName := '.cri';
            FF_GIF   : ExtName := '.gif';
            FF_JPEG  : ExtName := '.jpg';
            FF_PBM   : ExtName := '.pbm';
            FF_PGM   : ExtName := '.pgm';
            FF_PNG   : ExtName := '.png';
            FF_PPM   : ExtName := '.ppm';
            FF_SGI   : ExtName := '.rgb';
            FF_TIFF  : ExtName := '.tif';
            FF_TARGA : ExtName := '.tga';
            FF_BMP   : ExtName := '.bmp';
            FF_DIB   : ExtName := '.dib';
            FF_ICO   : ExtName := '.ico';
            FF_PCX   : ExtName := '.pcx';
            else ExtName := '.bmp';
            end;

            // Store last directory and file extension.
            SaveDialog.FileName := ChangeFileExt(SaveDialog.FileName, ExtName);

            FSaveCursor := Screen.Cursor;
            Screen.Cursor := crHourGlass;

            // Store image to disk
            ImageFileManager.Quality := 100;
            // If the format is TIFF, Compression could assigned CP_LZW below.
            ImageFileManager.Compression := CP_NOCOMP;
            ImageFileManager.SaveImage(SaveDialog.FileName,
                                       FF_DETECT,
                                       Image1.Picture.Bitmap.Handle);

            Screen.Cursor := FSaveCursor;

            if (ImageFileManager.Error <> EC_OK)
            then ShowMessage('Error writing image: ' + CErrorStrings[word(ImageFileManager.Error)]);
       end;

  end
  else ShowMessage('There is nothing to save !');
end;


procedure TForm1.ExitItemClick(Sender : TObject);
begin
  Close;
end; // TForm1.ExitItemClick.


procedure TForm1.CopyItemClick(Sender : TObject);
var AFormat  : word;
    AData    : THandle;
    APalette : HPALETTE;
begin
  if Assigned(Image1.Picture)
  then if Not(Image1.Picture.Bitmap.Empty)
       then begin
            Image1.Picture.SaveToClipboardFormat(AFormat, AData, APalette);
            if (AData <> 0)
            then begin
                 Clipboard.Open;
                 Clipboard.SetAsHandle(AFormat, AData);
                 if (APalette <> 0)
                 then Clipboard.SetAsHandle(CF_PALETTE, APalette);
                 Clipboard.Close;
            end;
       end;
end; // TForm1.CopyItemClick.


procedure TForm1.PasteItemClick(Sender : TObject);
var AFormat  : word;
    AData    : THandle;
    APalette : HPALETTE;
begin
  Clipboard.Open;
  try
    AFormat :=  CF_PALETTE;
    APalette := Clipboard.GetAsHandle(AFormat);
    AFormat :=  CF_BITMAP;
    AData := Clipboard.GetAsHandle(AFormat);
    if (AData <> 0)
    then begin
         Image1.Picture.LoadFromClipboardFormat(AFormat, AData, APalette);
    end;
  finally
    Clipboard.Close;
  end;
end; // TForm1.PasteItemClick.

end.
