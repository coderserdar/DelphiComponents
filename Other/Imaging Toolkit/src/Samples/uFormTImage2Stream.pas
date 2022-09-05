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
// $Log:  19520: uFormTImage2Stream.pas 
//
//   Rev 1.3    23-08-2005 21:59:48  mcm    Version: IMG 2.9
// Replaced use of TmcmFileFormat and TmcmCompressions with
// TmcmFileFormatList/TmcmFileFormatItem and array of TmcmCompress.

//
//   Rev 1.2    05-01-2005 15:16:10  mcm
// Updated example.

//
//   Rev 1.1    26-09-2004 10:57:38  mcm    Version: IMG 2.6
// Modified compression const name.

//
//   Rev 1.0    18-03-2003 16:34:48  mcm    Version: IMG 1.3.3

unit uFormTImage2Stream;

interface

//------------------------------------------------------------------------------
// TImage2Stream
//
// This example show you how to store images mixed with other data to a stream.
// The example uses TImage and TmcmImageFileMgr (ImageFileManager automatically
// created).
// The example use Borlands TImage component to further show how to mix between
// the image components from Borland and MCM DESIGN.
//
// The images can be stored to stream in the different file formats (except
// Targa - this file format can only store an image in individual files) with or
// without compression.
//
// Make sure that the "Search Path" in "Project, Options, Directories/
// Conditionals" points to the directory that the Imaging Toolkit for Delphi was
// installed to: d:\Program files\Borland\DelphiX\MCM DESIGN.
// Where "d" is the drive, and "X" is the Delphi version.
// Also make sure that "Output directory" and "Unit output directory" points to
// valid directories.
//------------------------------------------------------------------------------

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, mcmImage, mcmImageFile, mcmImageTypeDef, ComCtrls,
  Spin;

type
  TFormStreaming = class(TForm)
    Label1         : TLabel;
    Label2         : TLabel;
    Image1         : TImage;
    Image3         : TImage;
    Edit1          : TEdit;
    Edit2          : TEdit;
    Edit3          : TEdit;
    Edit4          : TEdit;
    Bevel1         : TBevel;
    Bevel2         : TBevel;
    eFilename      : TEdit;
    Label3         : TLabel;
    cbFileFormat   : TComboBox;
    Label4         : TLabel;
    cbCompression  : TComboBox;
    Label5         : TLabel;
    btnSaveAndLoad : TButton;
    StatusBar      : TStatusBar;
    Label6         : TLabel;
    cbQuality      : TComboBox;
    procedure FormCreate(Sender : TObject);
    procedure btnSaveAndLoadClick(Sender : TObject);
    procedure mcmImageChange(Sender : TObject);
    procedure cbFileFormatChange(Sender : TObject);
    procedure cbCompressionChange(Sender : TObject);
    procedure cbQualityChange(Sender : TObject);
  private
    { Private declarations }
    FDefaultFileFormat : TmcmFileFormat;
    FImageFormat       : TmcmImageFormat;
    FFileFormat        : TmcmFileFormat;
    FCompression       : TmcmCompress;
    procedure LoadData2Stream(AName : string);
    procedure SaveData2Stream(AName : string);
    function IsGreyScale : boolean;
  public
    { Public declarations }
  end;

var FormStreaming : TFormStreaming;

implementation

{$R *.DFM}

procedure TFormStreaming.FormCreate(Sender : TObject);
var FormatName : string;
    i          : integer;
begin
  //----------------------------------------------------------------------------
  // Specify which formats to support - if not all!
  // ImageFileManager.SetSupportedFormats([FF_TIFF, FF_BMP, FF_DIB]);
  ImageFileManager.UnregisterFileFormat(TmcmICONImage);

  ImageFileManager.YCbCrMode := JYCC_AUTO;

  FDefaultFileFormat := FF_TIFF; // We're setting TIFF as our default file format.
  // Change the FF_xxx assignment above to use another default file format.
  FFileFormat := FDefaultFileFormat;

  cbFileFormat.ItemIndex := 0;
  cbCompression.ItemIndex := 0;
  cbQuality.ItemIndex := 3;
  mcmImageChange(Self);
end; // TFormStreaming.FormCreate.


procedure TFormStreaming.btnSaveAndLoadClick(Sender : TObject);
var Name : string;
begin
  Edit3.Text := '';
  Edit4.Text := '';
  Image3.Picture.Bitmap.Handle := 0;
  Invalidate;
  Update;

  Name := eFilename.Text;
  SaveData2Stream(Name);
  if (ImageFileManager.Error = EC_OK)
  then LoadData2Stream(Name);

  if (ImageFileManager.Error <> EC_OK)
  then ShowMessage(CErrorStrings[word(ImageFileManager.Error)]);
end; // TFormStreaming.btnSaveAndLoadClick.


procedure TFormStreaming.SaveData2Stream(AName : string);
var Stream  : TFileStream;
    Count   : integer;
    StrData : string;
    ChData  : array[0..255] of char;
begin
  Stream := TFileStream.Create(AName, fmCreate);
  try
    // Write first string
    StrData := Edit1.Text;
    Count := Length(StrData);
    Stream.Write(Count, 4);
    StrPCopy(ChData, StrData);
    Stream.Write(ChData, Count);

    // Write first image
    ImageFileManager.Compression := FCompression;
    ImageFileManager.SaveToStream(Stream, FFileFormat, Image1.Picture.Bitmap.Handle);
    if (ImageFileManager.Error <> EC_OK)
    then Exit;

    // Write second string
    StrData := Edit2.Text;
    Count := Length(StrData);
    Stream.Write(Count, 4);
    StrPCopy(ChData, StrData);
    Stream.Write(ChData, Count);

    Count := Stream.Size;
    StatusBar.SimpleText := 'File size: ' + IntToStr(Count);
  finally
    Stream.Free;
  end;
end; // TFormStreaming.SaveData2Stream.


procedure TFormStreaming.LoadData2Stream(AName : string);
var Stream : TFileStream;
    Count   : integer;
    StrData : string;
    ChData  : array[0..255] of char;
begin
  Stream := TFileStream.Create(AName, fmOpenRead);
  try
    // Read first string
    StrData := '';
    Stream.Read(Count, 4);
    Stream.Read(ChData, Count);
    ChData[Count] := #0;
    StrData := StrPas(ChData);
    Edit3.Text := StrData;

    // Read first image
    Image3.Picture.Bitmap.Handle := ImageFileManager.LoadFromStream(Stream, FFileFormat);
    if (ImageFileManager.Error <> EC_OK)
    then Exit;

    // Read second string
    Stream.Read(Count, 4);
    Stream.Read(ChData, Count);
    ChData[Count] := #0;
    StrData := StrPas(ChData);
    Edit4.Text := StrData;
  finally
    Stream.Free;
  end;
end; // TFormStreaming.LoadData2Stream.


function TFormStreaming.IsGreyScale : boolean;
var hPal    : HPALETTE;
    pPal    : PLogPalette;
    NoColor : integer;
    i       : integer;
begin
  // This method checks if the image contains a grey scale palette.
  Result := False;
  hPal := Image1.Picture.Bitmap.Palette;
  pPal := Nil;
  NoColor := 0;
  Windows.GetObject(hPal, SizeOf(NoColor), @NoColor);
  if (NoColor = 16) or (NoColor = 256)
  then begin
       GetMem(pPal, NoColor * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
       if Assigned(pPal)
       then begin
            Windows.GetPaletteEntries(hPal, 0, NoColor, pPal^.palPalEntry[0]);
            Result := True;

            {$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}
            for i := 0 to (NoColor - 1)
            do begin
               with pPal^.palPalEntry[i]
               do begin
                  if (peRed <> peGreen) or (peRed <> peBlue)
                  then begin
                       Result := False;
                       Exit;
                  end;
               end;
            end;
            {$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

            FreeMem(pPal);
       end;
  end;
end; // TFormStreaming.IsGreyScale.


procedure TFormStreaming.mcmImageChange(Sender : TObject);
var Formats     : TmcmFileFormatsList;
    i, j        : integer;
    ColorStr    : string;
    FileSizeStr : string;

begin
  // Check which image format Image1 contains.
  // We need this to determin which file formats we can use.
  case Image1.Picture.Bitmap.PixelFormat of
  pf1bit  : FImageFormat := IF_BW;
  pf4bit  : if (IsGreyScale)
            then FImageFormat := IF_GREY4
            else FImageFormat := IF_PAL4;
  pf8bit  : if (IsGreyScale)
            then FImageFormat := IF_GREY8
            else FImageFormat := IF_PAL8;
  pf15bit : FImageFormat := IF_RGB15;
  pf16bit : FImageFormat := IF_RGB16;
  pf24bit : FImageFormat := IF_RGB24;
  pf32bit : FImageFormat := IF_RGBA32;
  end;

  // Update the cbFileFormat list box to show the file formats that supports the
  // current image format (color resolution/bit depth).
  cbFileFormat.Clear;
  if Not(Image1.Picture.Bitmap.Empty)
  then begin
       j := -1;

       Formats := TmcmFileFormatsList.Create;
       ImageFileManager.GetFormatsFromColor(FImageFormat, Formats);
       for i := 0 to (Formats.Count - 1)
       do begin
          if (Formats.Items[i].FileClassID <> FF_TARGA)
          then begin
               cbFileFormat.Items.AddObject(Formats.Items[i].Description, pointer(Formats.Items[i].FileClassID));
               if (FFileFormat = Formats.Items[i].FileClassID)
               then j := cbFileFormat.Items.Count - 1;
          end;
       end;
       Formats.Free;

       if (j < 0)
       then j := 0;

       // Select/show the format used to store the image.
       cbFileFormat.ItemIndex := j;
       
       // Update compression list
       cbFileFormatChange(Sender);
  end;
end; // TFormStreaming.mcmImageChange.


procedure TFormStreaming.cbFileFormatChange(Sender : TObject);
var Formats : array[0..255] of TmcmCompress;
    Count   : integer;
    i, j    : integer;
begin
  // A file format has been select.
  // Update the cbCompression list box with the supported compression methods.
  cbCompression.Clear;
  if Not(Image1.Picture.Bitmap.Empty)
  then begin
       // Update if file format is different.
       if (FFileFormat <> TmcmFileFormat(cbFileFormat.Items.Objects[cbFileFormat.ItemIndex]))
       then FFileFormat := TmcmFileFormat(cbFileFormat.Items.Objects[cbFileFormat.ItemIndex]);

       j := -1;
       Count := ImageFileManager.GetCompressionFromColor(FFileFormat, FImageFormat, Formats);
       for i := 0 to (Count - 1)
       do begin
          cbCompression.Items.AddObject(ImageFileManager.GetCompressionName(FFileFormat, Formats[i]), pointer(Formats[i]));
          if (FCompression = Formats[i])
          then j := cbCompression.Items.Count - 1;
       end;

       if (j < 0)
       then j := 0;

       // Select/show the format used to store the image.
       cbCompression.ItemIndex := j;
       cbCompressionChange(Sender);
  end;
end; // TFormStreaming.cbFileFormatChange.


procedure TFormStreaming.cbCompressionChange(Sender : TObject);
begin
  if Not(Image1.Picture.Bitmap.Empty)
  then begin
       // Update Compression if different.
       if (FCompression <> TmcmCompress(cbCompression.Items.Objects[cbCompression.ItemIndex]))
       then FCompression := TmcmCompress(cbCompression.Items.Objects[cbCompression.ItemIndex]);
       if (FFileFormat = FF_PNG) or (FFileFormat = FF_JPEG)
       then cbQuality.Enabled := True
       else cbQuality.Enabled := False;
  end;
end; // TFormStreaming.cbCompressionChange.


procedure TFormStreaming.cbQualityChange(Sender : TObject);
begin
  // For JPEG and PNG only. Here we specify the compression ratio = (1 to 100).
  case cbQuality.ItemIndex of
  0 : ImageFileManager.Quality := 24; // Lowest quality, highest compression
  1 : ImageFileManager.Quality := 49;
  2 : ImageFileManager.Quality := 74;
  3 : ImageFileManager.Quality := 100; // Best quality
  end;
end; // TFormStreaming.cbQualityChange.

end.
