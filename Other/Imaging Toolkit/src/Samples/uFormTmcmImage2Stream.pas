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
// $Log:  27790: uFormTmcmImage2Stream.pas 
//
//    Rev 1.0    01-06-2006 19:59:54  mcm
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

unit uFormTmcmImage2Stream;

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
  Spin, mcmFileDialogues;

type
  TFormStreaming = class(TForm)
    Label1         : TLabel;
    Label2         : TLabel;
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
    mcmImageCtrl1  : TmcmImageCtrl;
    mcmImageCtrl2  : TmcmImageCtrl;
    OpenDialog: TmcmOpenDialog;
    procedure FormCreate(Sender : TObject);
    procedure btnSaveAndLoadClick(Sender : TObject);
    procedure mcmImageCtrl1Change(Sender : TObject);
    procedure cbFileFormatChange(Sender : TObject);
    procedure cbCompressionChange(Sender : TObject);
    procedure cbQualityChange(Sender : TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FDefaultFileFormat : TmcmFileFormat;
    FFileFormat        : TmcmFileFormat;
    FCompression       : TmcmCompress;
    procedure LoadData2Stream(AName : string);
    procedure SaveData2Stream(AName : string);
  public
    { Public declarations }
  end;

var FormStreaming : TFormStreaming;

implementation

{$R *.DFM}

procedure TFormStreaming.FormCreate(Sender : TObject);
begin
  //----------------------------------------------------------------------------
  // Specify which formats to support - if not all!
  ImageFileManager.UnregisterFileFormat(TmcmICONImage);

  ImageFileManager.YCbCrMode := JYCC_AUTO;

  FDefaultFileFormat := FF_TIFF; // We're setting TIFF as our default file format.
  // Change the FF_xxx assignment above to use another default file format.
  FFileFormat := FDefaultFileFormat;

  cbFileFormat.ItemIndex := 0;
  cbCompression.ItemIndex := 0;
  cbQuality.ItemIndex := 3;
  mcmImageCtrl1.Image.SetStretchMode(HALFTONE);

  OpenDialog.FilterIndex := 1;
  //----------------------------------------------------------------------------
  // Assign the global ImageFileManager to both the mcmOpenDialog and
  // mcmSaveDialog.
  // This enables both dialogues to automatically obtain the supported and
  // enabled file formats & compressions.
  OpenDialog.ImageFileManager := ImageFileManager;
end; // TFormStreaming.FormCreate.


procedure TFormStreaming.btnSaveAndLoadClick(Sender : TObject);
var Name       : string;
    SaveCursor : TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  
  Edit3.Text := '';
  Edit4.Text := '';
  mcmImageCtrl2.Clear;
  Update;
  mcmImageCtrl2.Image.SetStretchMode(HALFTONE);

  Name := eFilename.Text;
  SaveData2Stream(Name);
  if (mcmImageCtrl1.Image.Error = EC_OK)
  then LoadData2Stream(Name);

  if (ImageFileManager.Error <> EC_OK)
  then ShowMessage(CErrorStrings[word(ImageFileManager.Error)]);
  Screen.Cursor := SaveCursor;
end; // TFormStreaming.btnSaveAndLoadClick.


procedure TFormStreaming.SaveData2Stream(AName : string);
var Stream  : TFileStream;
    Count   : integer;
    StrData : string;
    ChData  : array[0..255] of char;
begin
  StatusBar.SimpleText := 'Saving data';
  Stream := TFileStream.Create(AName, fmCreate);
  try
    // Write first string
    StrData := Edit1.Text;
    Count := Length(StrData);
    Stream.Write(Count, 4);
    StrPCopy(ChData, StrData);
    Stream.Write(ChData, Count);

    // Write first image
    // mcmImageCtrl1.Image.Quality     := See TFormStreaming.cbQualityChange
    mcmImageCtrl1.Image.Compression := FCompression;
    mcmImageCtrl1.Image.SaveToStreamEx(Stream, FFileFormat);

    if (mcmImageCtrl1.Image.Error <> EC_OK)
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
  StatusBar.SimpleText := 'Loading data';
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
    mcmImageCtrl2.Image.LoadFromStreamEx(Stream, FFileFormat);
    if (mcmImageCtrl2.Image.Error <> EC_OK)
    then Exit;

    // Read second string
    Stream.Read(Count, 4);
    Stream.Read(ChData, Count);
    ChData[Count] := #0;
    StrData := StrPas(ChData);
    Edit4.Text := StrData;

    Count := Stream.Size;
    StatusBar.SimpleText := 'File size: ' + IntToStr(Count);
  finally
    Stream.Free;
  end;
end; // TFormStreaming.LoadData2Stream.


procedure TFormStreaming.mcmImageCtrl1Change(Sender : TObject);
var Formats : TmcmFileFormatsList;
    i, j    : integer;
begin
  // Update the cbFileFormat list box to show the file formats that supports the
  // current image format (color resolution/bit depth).
  cbFileFormat.Clear;
  if Not(mcmImageCtrl1.Image.Empty)
  then begin
       j := -1;
       Formats := TmcmFileFormatsList.Create;
       ImageFileManager.GetFormatsFromColor(mcmImageCtrl1.Image.ImageFormat, Formats);
       for i := 0 to (Formats.Count - 1)
       do begin
          cbFileFormat.Items.AddObject(Formats.Items[i].Description, pointer(Formats.Items[i].FileClassID));
          if (FFileFormat = Formats.Items[i].FileClassID)
          then j := cbFileFormat.Items.Count - 1;
       end;
       Formats.Free;

       // Select/show the format used to store the image.
       cbFileFormat.ItemIndex := j;
       // Update compression list
       cbFileFormatChange(Sender);
  end;
end; // TFormStreaming.mcmImageCtrl1Change.


procedure TFormStreaming.cbFileFormatChange(Sender : TObject);
var Formats : array[0..255] of TmcmCompress;
    Count   : integer;
    i, j    : integer;
begin
  // A file format has been select.
  //Update the cbCompression list box with the supported compression methods.
  cbCompression.Clear;
  if Not(mcmImageCtrl1.Image.Empty)
  then begin
       // Update file format is different.
       if (FFileFormat <> TmcmFileFormat(cbFileFormat.Items.Objects[cbFileFormat.ItemIndex]))
       then FFileFormat := TmcmFileFormat(cbFileFormat.Items.Objects[cbFileFormat.ItemIndex]);

       j := 0;
       Count := ImageFileManager.GetCompressionFromColor(FFileFormat, mcmImageCtrl1.Image.ImageFormat, Formats);
       for i := 0 to (Count - 1)
       do begin
          cbCompression.Items.AddObject(ImageFileManager.GetCompressionName(FFileFormat, Formats[i]), pointer(Formats[i]));
          if (mcmImageCtrl1.Image.Compression = Formats[i])
          then j := cbCompression.Items.Count - 1;
       end;

       // Select/show the format used to store the image.
       cbCompression.ItemIndex := j;
       cbCompressionChange(Sender);
  end;
end; // TFormStreaming.cbFileFormatChange.


procedure TFormStreaming.cbCompressionChange(Sender : TObject);
begin
  if Not(mcmImageCtrl1.Image.Empty)
  then begin
       // Update Compression if different.
       if (FCompression <> TmcmCompress(cbCompression.Items.Objects[cbCompression.ItemIndex]))
       then FCompression := TmcmCompress(cbCompression.Items.Objects[cbCompression.ItemIndex]);
       if (FCompression in [CP_PNG, CP_JPEG_PROG, CP_JPEG_STD])
       then cbQuality.Enabled := True
       else cbQuality.Enabled := False;
  end;
end; // TFormStreaming.cbCompressionChange.


procedure TFormStreaming.cbQualityChange(Sender : TObject);
begin
  // For JPEG and PNG only. Here we specify the compression ratio = (1 to 100).
  case cbQuality.ItemIndex of
  0 : mcmImageCtrl1.Image.Quality := 24; // Lowest quality, highest compression
  1 : mcmImageCtrl1.Image.Quality := 49;
  2 : mcmImageCtrl1.Image.Quality := 74;
  3 : mcmImageCtrl1.Image.Quality := 100; // Best quality
  end;
end; // TFormStreaming.cbQualityChange.

procedure TFormStreaming.FormShow(Sender: TObject);
begin
  if (OpenDialog.Execute)
  then mcmImageCtrl1.Image.FileOpen(OpenDialog.FileName);
end;

end.
