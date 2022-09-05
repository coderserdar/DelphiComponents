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
// $Log:  22735: UFormFishImage2DB.pas 
//
//   Rev 1.3    23-08-2005 21:59:46  mcm    Version: IMG 2.9
// Replaced use of TmcmFileFormat and TmcmCompressions with
// TmcmFileFormatList/TmcmFileFormatItem and array of TmcmCompress.

//
//   Rev 1.2    06-10-2004 19:18:58  mcm    Version: IMG 2.6
// Updated to use CP_NOCOMP.

//
//   Rev 1.1    24-01-2004 13:05:28  mcm    Version: IMG 2.3

//
//   Rev 1.0    24-01-2004 13:00:00  mcm    Version: IMG 2.3
// Initial edition

unit UFormFishImage2DB;

interface

//------------------------------------------------------------------------------
// This sample show how to use the TmcmImageDB component.
//
// The Paradox database used is the well known BIOLIFE.
// The images is in this example stored in the database using the Window Bitmap
// file format.
//
// To stay compatible with Borland's TDBImage control, the graphic fields
// BlobType was set to ftGraphic. This disables the TmcmImageDB control from
// storing images in any other format but the bitmap format.
// To enable TmcmImageDB to save in the other supported formats, set the graphic
// fields BlobType to ftBlob.
//
// It should be noted that if images are stored in other formats than the bitmap
// format, then the Borland TDBImage will not be able to read these.
// It should also be noted that to read images compatible with those store by
// TDBImage, the properties AddGraphicHeader and AutoFormat must both be set to
// True on the TmcmImageDB control.
//------------------------------------------------------------------------------

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, StdCtrls, Mask, DBCtrls, ExtCtrls, DBTables,
  mcmImage,
  mcmImageDB;

type
  TFormImageDB = class(TForm)
    mcmImageDB1       : TmcmImageDB;
    DataSource1       : TDataSource;
    Table1            : TTable;
    DBNavigator1      : TDBNavigator;
    btnReadImage      : TButton;
    OpenDialog        : TOpenDialog;
    lName             : TLabel;
    dbeName           : TDBEdit;
    lFileFormat       : TLabel;
    lCompression      : TLabel;
    cbFilename        : TComboBox;
    cbCompression     : TComboBox;
    lAboutFish        : TLabel;
    DBMemoAbout       : TDBMemo;

    Table1SpeciesNo   : TFloatField;
    Table1Category    : TStringField;
    Table1Common_Name : TStringField;
    Table1SpeciesName : TStringField;
    Table1Lengthcm    : TFloatField;
    Table1Length_In   : TFloatField;
    Table1Notes       : TMemoField;
    Table1Graphic     : TGraphicField;

    procedure FormCreate(Sender: TObject);
    procedure btnReadImageClick(Sender : TObject);
    procedure mcmImageDB1Change(Sender : TObject);
    procedure cbFilenameChange(Sender : TObject);
    procedure cbCompressionChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var FormImageDB : TFormImageDB;

implementation

uses mcmImageFile, mcmImageTypeDef, mcmImageResStr;

{$R *.DFM}

procedure TFormImageDB.FormCreate(Sender : TObject);
begin
  //----------------------------------------------------------------------------
  // Specify which formats to support!
  ImageFileManager.UnregisterFileFormat(TmcmICONImage);

  ImageFileManager.YCbCrMode := JYCC_AUTO;
  OpenDialog.Filter := ImageFileManager.GetReadFilter;
  OpenDialog.FilterIndex := 13;
  Table1.Active := True;

  if (mcmImageDB1.Field.DataType = ftGraphic)
  then begin
       // ftGraphic fields are bitmaps (FF_BMP).
       cbFilename.Enabled := False;
       cbCompression.Enabled := False;
  end;
end;


procedure TFormImageDB.btnReadImageClick(Sender : TObject);
var FSaveCursor : TCursor;
begin
  // Read an image from disk into the mcmImageDB component.
  if (OpenDialog.Execute)
  then begin
       FSaveCursor := Screen.Cursor;
       Screen.Cursor := crHourGlass;

       mcmImageDB1.Image.FileOpen(OpenDialog.FileName);

       OpenDialog.InitialDir := ExtractFileDir(OpenDialog.FileName);
       Screen.Cursor := FSaveCursor;
  end;
end;

//------------------------------------------------------------------------------
// The following methods determines which file format and compression that can
// be used when storing the image to the database.
//------------------------------------------------------------------------------

procedure TFormImageDB.mcmImageDB1Change(Sender : TObject);
var Formats     : TmcmFileFormatsList;
    i, j        : integer;
    ColorStr    : string;
    FileSizeStr : string;
begin
  // Update Hint info to reflect the image data.
  with mcmImageDB1.Image
  do begin
     // Set hint information.
     case BitCount of
     1..14 : ColorStr := IntToStr(1 shl BitCount);
     15 : ColorStr := resColor32K;
     16 : ColorStr := resColor64K;
     24,
     32 : ColorStr := resColor16M;
     48 :
     // else ColorStr := IntToStr(1 shl NewThumb.Image.BitCount);
     end;

     if (ImageInfo.FileSize > 1048576)
     then FileSizeStr := FloatToStrF(ImageInfo.FileSize / 1048576, ffFixed, 7, 3) + ' ' + resMegaBytes
     else if (ImageInfo.FileSize > 1024)
          then FileSizeStr := FloatToStrF(ImageInfo.FileSize / 1024, ffFixed, 7, 3) + ' ' + resKiloBytes
          else FileSizeStr := IntToStr(ImageInfo.FileSize) + ' ' + resBytes;

     mcmImageDB1.Hint := resDimension + ': ' + IntToStr(Width) + ' x ' +
                                               IntToStr(Height) + ' x ' +
                                               ColorStr + chr($0D) +
                         resFileSize + ': ' + FileSizeStr + chr($0D) +
                         resFileFormat + ': ' + ImageFileManager.FileFormatToStr(ImageInfo.FileFormat) + chr($0D) +
                         resCompression + ': ' + CCompressStrings[integer(Compression)];
  end;

  // Update the cbFileFormat list box to show the file formats that supports the
  // current image format (color resolution/bit depth).
  cbFilename.Clear;
  if Not(mcmImageDB1.Image.Empty)
  then begin
       j := -1;
       Formats := TmcmFileFormatsList.Create;
       ImageFileManager.GetFormatsFromColor(mcmImageDB1.Image.ImageFormat, Formats);
       for i := 0 to (Formats.Count - 1)
       do begin
          cbFilename.Items.AddObject(Formats.Items[i].Description, pointer(Formats.Items[i].FileClassID));
          if (mcmImageDB1.FileFormat = Formats.Items[i].FileClassID)
          then j := cbFilename.Items.Count - 1;
       end;
       Formats.Free;
       
       // Select/show the format used to store the image.
       cbFilename.ItemIndex := j;
       // Update compression list
       cbFilenameChange(Sender);
  end;
end; // TFormImageDB.mcmImageDB1Change.


procedure TFormImageDB.cbFilenameChange(Sender : TObject);
var Formats : array[0..255] of TmcmCompress;
    Count   : integer;
    i, j    : integer;
begin
  // A file format has been select.
  //Update the cbCompression list box with the supported compression methods.
  cbCompression.Clear;
  if Not(mcmImageDB1.Image.Empty)
  then begin
       // Update if file format is different.
       if (mcmImageDB1.FileFormat <> TmcmFileFormat(cbFilename.Items.Objects[cbFilename.ItemIndex]))
       then mcmImageDB1.FileFormat := TmcmFileFormat(cbFilename.Items.Objects[cbFilename.ItemIndex]);

       j := -1;
       Count := ImageFileManager.GetCompressionFromColor(mcmImageDB1.FileFormat, mcmImageDB1.Image.ImageFormat, Formats);
       for i := 0 to (Count - 1)
       do begin
          cbCompression.Items.AddObject(ImageFileManager.GetCompressionName(mcmImageDB1.FileFormat, Formats[i]), pointer(Formats[i]));
          if (mcmImageDB1.Compression = Formats[i])
          then j := cbCompression.Items.Count - 1;
       end;

       // Select/show the format used to store the image.
       cbCompression.ItemIndex := j;
  end;
end; // TFormImageDB.cbFilenameChange.


procedure TFormImageDB.cbCompressionChange(Sender : TObject);
begin
  if Not(mcmImageDB1.Image.Empty)
  then begin
       // Update file format is different.
       if (mcmImageDB1.Compression <> TmcmCompress(cbCompression.Items.Objects[cbCompression.ItemIndex]))
       then mcmImageDB1.Compression := TmcmCompress(cbCompression.Items.Objects[cbCompression.ItemIndex]);
  end;
end; // TFormImageDB.cbCompressionChange.

end.
