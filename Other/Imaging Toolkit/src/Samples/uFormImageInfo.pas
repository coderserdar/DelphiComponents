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
// $Log:  17603: uFormImageInfo.pas 
//
//    Rev 1.4    2014-02-02 21:10:10  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.3    01-06-2004 00:07:42  mcm    Version: IMG 2.5
// Fixed groupbox text.

//
//   Rev 1.2    14-03-2004 09:01:32  mcm    Version: IMG 2.4
// Display Exif and GPS information.

//
//   Rev 1.1    24-11-2003 20:16:00  mcm

//
//   Rev 1.0    27-05-2002 16:22:34  mcm

unit uFormImageInfo;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      ComCtrls, StdCtrls, Grids,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Grids,
     {$ENDIF}
     mcmImageTypeDef,
     mcmImageResStr,
     mcmImage;

type
  TFormImageInfo = class(TForm)
    btnCancel       : TButton;
    btnOK           : TButton;
    pcImageInfo     : TPageControl;
    tsInformation   : TTabSheet;
    gbImageInfo     : TGroupBox;
    lDimension      : TLabel;
    lResolution     : TLabel;
    lPixelColor     : TLabel;
    lDimensionVal   : TLabel;
    lResolutionVal  : TLabel;
    lPixelColorVal  : TLabel;
    lDimensionDpi   : TLabel;
    lUsedColours    : TLabel;
    lUsedColoursVal : TLabel;
    lModified       : TLabel;
    lModifiedVal    : TLabel;
    lPixelHeight    : TLabel;
    lPixelWidth     : TLabel;
    lPixelHeightVal : TLabel;
    lPixelWidthVal  : TLabel;
    tsAdditional    : TTabSheet;
    gbExtraImageInfo: TGroupBox;
    lFileFormat     : TLabel;
    lArtist         : TLabel;
    lCopyright      : TLabel;
    lDateTime       : TLabel;
    lDescription    : TLabel;
    lFileSize       : TLabel;
    lFileSizeValue  : TLabel;
    cbFileFormat    : TComboBox;
    eDateTime       : TEdit;
    eArtist         : TEdit;
    eCopyright      : TEdit;
    mDescription    : TMemo;
    tsDocument      : TTabSheet;
    gbDocument      : TGroupBox;
    lDocumentName   : TLabel;
    lPageName       : TLabel;
    lPageNumber     : TLabel;
    eDocumentName   : TEdit;
    ePageName       : TEdit;
    ePageNumber     : TEdit;
    gbSystemInfo    : TGroupBox;
    lHostComputer   : TLabel;
    lMake           : TLabel;
    lModel          : TLabel;
    lSoftware       : TLabel;
    lSoftwareVersion: TLabel;
    eHostComputer   : TEdit;
    eMake           : TEdit;
    eModel          : TEdit;
    eSoftware       : TEdit;
    eVersion        : TEdit;
    tsExif          : TTabSheet;
    sgExif          : TStringGrid;
    tsGPS           : TTabSheet;
    sgGPS           : TStringGrid;
    procedure FormCreate(Sender : TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbFileFormatChange(Sender: TObject);
  private
    { Private declarations }
    FImage     : TmcmImage;
    FImageInfo : TmcmImageInfo;
  protected
    procedure SetImage(Value : TmcmImage);
    procedure SetImageInfo(Value : TmcmImageInfo);
    procedure InsertRow(Grid : TStringGrid; Name, Data : string; AppendRow : boolean);
    procedure ShowExifInfo;
    procedure ShowGPSInfo;
  public
    { Public declarations }
    property Image : TmcmImage
      write  SetImage;
    property ImageInfo : TmcmImageInfo
      write  SetImageInfo;
  end;

var FormImageInfo : TFormImageInfo;

implementation

{$R *.DFM}

procedure TFormImageInfo.FormCreate(Sender : TObject);
var i : integer;
begin
  FImageInfo := Nil;

  tsExif.TabVisible := False;
  tsGPS.TabVisible  := False;
  lDimensionVal.Caption   := '';
  lResolutionVal.Caption  := '';
  lPixelColorVal.Caption  := '';
  lUsedColoursVal.Caption := '';
  lModifiedVal.Caption    := '';
  lPixelHeightVal.Caption := '';
  lPixelWidthVal.Caption  := '';

  cbFileFormat.Clear;
  for i := 0 to integer(FF_NONE)
  do cbFileFormat.Items.Add(CFileFormatStrings[i]);

  pcImageInfo.ActivePage := tsInformation;

  sgExif.RowCount := 1;
  // Set header.
  sgExif.Cells[0,0] := 'Name';
  sgExif.Cells[1,0] := 'Data';
  // Clear first row.
  sgExif.Cells[0,1] := '';
  sgExif.Cells[1,1] := '';

  sgGPS.RowCount := 1;
  // Set header.
  sgGPS.Cells[0,0] := 'Name';
  sgGPS.Cells[1,0] := 'Data';
  // Clear first row.
  sgGPS.Cells[0,1] := '';
  sgGPS.Cells[1,1] := '';

end; // TFormImageInfo.FormCreate.


procedure TFormImageInfo.SetImage(Value : TmcmImage);
var NoColors   : longword;
    ColorExt   : string;
    dpiScale   : double;
    ResStr     : string;
begin
  if Assigned(Value)
  then begin
       FImage := Value;
       with FImage
       do begin
          if Modified
          then lModifiedVal.Caption := 'Yes'
          else lModifiedVal.Caption := 'No';
          ColorExt := '';

          if (FImage.ImageFormat in [IF_RGB24, IF_RGBA32])
          then NoColors := 1 shl 24
          else NoColors := 1 shl BitCount;

          if (NoColors > 65536)
          then begin
               ColorExt := ' K';
               NoColors := NoColors div 1024;
               if (NoColors > 1024)
               then begin
                    ColorExt := ' Million';
                    NoColors := NoColors div 1024;
               end;
          end;
          dpiScale := 2.54 / 100.0;
          lDimensionVal.Caption  := IntToStr(Width) +  ' x ' +
                                    IntToStr(Height) + ' Pixels';
          ResStr := '';
          if ((dpiScale * XResolution) > 0)
          then ResStr := FloatToStrF(Width / (dpiScale * XResolution), ffFixed, 8, 2) + ' x '
          else ResStr := 'Unknown x ';
          if ((dpiScale * YResolution) > 0)
          then ResStr := ResStr + FloatToStrF(Height / (dpiScale * YResolution), ffFixed, 8, 2)
          else ResStr := ResStr + 'Unknown';
          lDimensionDpi.Caption  := ResStr + ' inch';
          if (XResolution <> YResolution)
          then lResolutionVal.Caption := FloatToStrF(dpiScale * XResolution, ffFixed, 8, 1) +
                                         ', ' +
                                         FloatToStrF(dpiScale * YResolution, ffFixed, 8, 1) +
                                         ' dpi'
          else lResolutionVal.Caption := FloatToStrF(dpiScale * XResolution, ffFixed, 8, 1) +
                                         ' dpi';

          lPixelColorVal.Caption := IntToStr(BitCount) + ' / ' +
                                    IntToStr(NoColors) + ColorExt;

       end;
       ImageInfo := FImage.ImageInfo;

       NoColors := FImage.UniqueColors;
       lUsedColoursVal.Caption := IntToStr(NoColors);
  end;
end; // End TFormImageInfo.SetImage.


procedure TFormImageInfo.SetImageInfo(Value : TmcmImageInfo);
begin
  FImageInfo := Value;
  if Assigned(FImageInfo)
  then begin
       with FImageInfo
       do begin
          // Image information.
          {
          FImageInfo.Units
          FImageInfo.XPosition
          FImageInfo.XResolution
          FImageInfo.YPosition
          FImageInfo.YResolution
          }
          lPixelHeightVal.Caption := IntToStr(PixelHeight);
          lPixelWidthVal.Caption  := IntToStr(PixelWidth);

          // Additional information.
          cbFileFormat.ItemIndex := integer(FImageInfo.FileFormat);
          eArtist.Text    := FImageInfo.Artist;
          eCopyright.Text := FImageInfo.Copyright;
          eDateTime.Text  := DateToStr(FImageInfo.DateTime) + ', ' +
                             TimeToStr(FImageInfo.DateTime);
          mDescription.Lines.Text := FImageInfo.Description;

          if (FImageInfo.FileSize > 1048576)
          then lFileSizeValue.Caption := FloatToStrF(FImageInfo.FileSize / 1048576, ffFixed, 7, 3) + ' ' + resMegaBytes
          else if (FImageInfo.FileSize > 1024)
               then lFileSizeValue.Caption := FloatToStrF(FImageInfo.FileSize / 1024, ffFixed, 7, 3) + ' ' + resKiloBytes
               else lFileSizeValue.Caption := IntToStr(FImageInfo.FileSize) + ' ' + resBytes;


          // System information.
          eHostComputer.Text := FImageInfo.HostComputer;
          eMake.Text         := FImageInfo.Make;
          eModel.Text        := FImageInfo.Model;
          eSoftware.Text     := FImageInfo.Software;
          eVersion.Text      := FloatToStrF(FImageInfo.SoftwareVersion, ffFixed, 4, 2);

          if FImageInfo.Modified
          then lModifiedVal.Caption := resYes
          else lModifiedVal.Caption := resNo;

          eDocumentName.Text := DocumentName;
          ePageName.Text     := PageName;
          ePageNumber.Text   := IntToStr(PageNumber);


          ShowExifInfo;
          ShowGPSInfo;
       end;
  end;
end; // End TFormImageInfo.SetImageInfo.


procedure TFormImageInfo.InsertRow(Grid : TStringGrid; Name, Data : string; AppendRow : boolean);
var i : integer;
begin
  i := Grid.RowCount;
  Grid.RowCount := Grid.RowCount + 1;
  Grid.Cells[0,i] := Name;
  Grid.Cells[1,i] := Data;
  Grid.FixedRows := 1;
end; // TFormImageInfo.InsertRow.


procedure TFormImageInfo.ShowExifInfo;
var ExifUnit  : string;
    HeaderStr : string;
    DataStr   : string;
    AppendRow : boolean;
    i         : integer;
begin
  if Assigned(FImageInfo.Exif)
  then begin
       tsExif.TabVisible := True;
       with FImageInfo.Exif
       do begin
          AppendRow := True;

          // User information.
          if (MakerNote <> '')
          then InsertRow(sgExif, 'Manufacturer data', StrPas(PChar(MakerNote)), AppendRow);
          if (UserComment <> '')
          then InsertRow(sgExif, 'User comment', UserComment, AppendRow);

          // Date & Time
          if (DateTimeOriginal <> 0)
          then begin
               InsertRow(sgExif, 'Photo taken', ' ', AppendRow);
               InsertRow(sgExif, '  date & time', DateTimeToStr(DateTimeOriginal), AppendRow);
               if (SubSecTimeOriginal > 0)
               then InsertRow(sgExif, '  sub-seconds', IntToStr(SubSecTimeOriginal) + ' ms', AppendRow);
          end;

          if (DateTimeDigitized <> 0)
          then begin
               InsertRow(sgExif, 'Photo stored', ' ', AppendRow);
               InsertRow(sgExif, '  date & time', DateTimeToStr(DateTimeDigitized), AppendRow);
               if (SubSecTimeDigitized > 0)
               then InsertRow(sgExif, '  sub-seconds', IntToStr(SubSecTimeDigitized) + ' ms', AppendRow);
          end;

          if (SubSecTime > 0)
          then InsertRow(sgExif, 'TIFF, sub-seconds', IntToStr(SubSecTime) + ' ms', AppendRow);

          // Other information.
          if (UniqueID <> '')
          then InsertRow(sgExif, 'Unique Id', UniqueID, AppendRow);

          // Color space info.
          HeaderStr := 'Color space';
          case ColorSpace of
          1     : InsertRow(sgExif, HeaderStr, 'sRGB', AppendRow);
          $FFFF : InsertRow(sgExif, HeaderStr, 'Uncalibrated', AppendRow);
          end;

          if (ComponentConfig[0] <> 0)
          then begin
               DataStr := '';
               for i := 0 to 3
               do begin
                  case ComponentConfig[i] of
                  1 : DataStr := DataStr + 'Y ';
                  2 : DataStr := DataStr + 'Cb ';
                  3 : DataStr := DataStr + 'Cr ';
                  4 : DataStr := DataStr + 'R ';
                  5 : DataStr := DataStr + 'G ';
                  6 : DataStr := DataStr + 'B ';
                  end;
               end;
               InsertRow(sgExif, 'Components Configuration', DataStr, AppendRow);
          end;

          if (CompressdBitPerPixel > 0)
          then InsertRow(sgExif, 'Compression mode', FloatToStrF(CompressdBitPerPixel, ffFixed, 7, 2) + ' Bits/Pixel', AppendRow);

          // Image configuration.
          if (XDimension <> 0)
          then InsertRow(sgExif, 'Valid image width', IntToStr(XDimension), AppendRow);
          if (YDimension <> 0)
          then InsertRow(sgExif, 'Valid image height', IntToStr(YDimension), AppendRow);

          if (SceneType = 1)
          then InsertRow(sgExif, 'Scene type', 'Photograph', AppendRow);

          // Picture taking conditions
          HeaderStr := 'Scene captured';
          case SceneCaptureType of
          0 : DataStr := 'Standard';
          1 : DataStr := 'Landscape';
          2 : DataStr := 'Portrait';
          3 : DataStr := 'Night scene';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

          if (FNumber > 0)
          then InsertRow(sgExif, 'F number', 'f/' + FloatToStrF(FNumber, ffFixed, 7, 2), AppendRow);

          HeaderStr := 'Exposure mode';
          case ExposureMode of
          0 : DataStr := 'Auto exposure';
          1 : DataStr := 'Manual exposure';
          2 : DataStr := 'Auto bracket';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

          HeaderStr := 'Exposure program';
          case ExposureProgram of
          0 : DataStr := 'Not defined';
          1 : DataStr := 'Manual';
          2 : DataStr := 'Normal';
          3 : DataStr := 'Aperture';
          4 : DataStr := 'Shutter';
          5 : DataStr := 'Creative, biased toward depth of field';
          6 : DataStr := 'Action, biased toward fast shutter speed';
          7 : DataStr := 'Portrait (closeup photo)';
          8 : DataStr := 'Landscape (background in focus)';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

          if (ExposureIndex > 0.0)
          then InsertRow(sgExif, 'Exposure index', FloatToStrF(ExposureIndex, ffFixed, 7, 2), AppendRow);

          if (ExposureTime > 0)
          then InsertRow(sgExif, 'Exposure', '1/' + FloatToStrF(1.0 / ExposureTime, ffFixed, 7, 1) + ' sec', AppendRow);

          if (-100.0 < ExposureBias) and (ExposureBias < 100.0)
          then InsertRow(sgExif, 'Exposure bias [APEX]', FloatToStrF(ExposureBias, ffFixed, 7, 2), AppendRow);


          if (ISOSpeedRating <> 0)
          then InsertRow(sgExif, 'ISO Speed Rating', IntToStr(ISOSpeedRating), AppendRow);

      //    FOECF                     : string; // Use SetLength and copy as were it bytes in an array.

          if (ShutterSpeed > 0.0)
          then InsertRow(sgExif, 'Shutter speed [APEX]', FloatToStrF(ShutterSpeed, ffFixed, 7, 2), AppendRow);

          if (Aperture > 0.0)
          then InsertRow(sgExif, 'Aperature [APEX]', FloatToStrF(Aperture, ffFixed, 7, 2), AppendRow);

          if (MaxAperture > 0.0)
          then InsertRow(sgExif, 'Max. Aperture [APEX]', FloatToStrF(MaxAperture, ffFixed, 7, 2), AppendRow);

          if (-100.0 < Brightness) and (Brightness < 100.0)
          then InsertRow(sgExif, 'Brightness [APEX]', FloatToStrF(Brightness, ffFixed, 7, 2), AppendRow);

          if (SpectralSensitivity <> '')
          then InsertRow(sgExif, 'Spectral sensitivity', SpectralSensitivity, AppendRow);

          HeaderStr := 'Metering mode';
          case MeteringMode of
          0 : DataStr := 'Unknown';
          1 : DataStr := 'Average';
          2 : DataStr := 'Center weighted average';
          3 : DataStr := 'Spot';
          4 : DataStr := 'Multi-spot';
          5 : DataStr := 'Pattern';
          6 : DataStr := 'Partial';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

          HeaderStr := 'Light source';
          case LightSource of
          0 : DataStr := 'Unknown';
          1 : DataStr := 'Daylight';
          2 : DataStr := 'Fluorescent';
          3 : DataStr := 'Tungsten (incandescent light)';
          4 : DataStr := 'Flash';
          9 : DataStr := 'Fine weather';
          10 : DataStr := 'Cloudy weather';
          11 : DataStr := 'Shade';
          12 : DataStr := 'Daylight flourescent (D 5700-7100K)';
          13 : DataStr := 'Day white flourescent (N 4600-5400K)';
          14 : DataStr := 'Cool white flourescent (W 3900-4500K)';
          15 : DataStr := 'White flourescent (WW 3200-3700K)';
          17 : DataStr := 'Standard light A';
          18 : DataStr := 'Standard light B';
          19 : DataStr := 'Standard light C';
          20 : DataStr := 'D55';
          21 : DataStr := 'D65';
          22 : DataStr := 'D75';
          23 : DataStr := 'D50';
          24 : DataStr := 'ISO studio tungsten';
          255 : DataStr := 'Other light source';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

          HeaderStr := 'Flash';
          if (Flash <> $FFFF)
          then begin
               case Flash of
               $000 : DataStr := 'Flash did not fire';
               $001 : DataStr := 'Flash fired';
               $005 : DataStr := 'Strobe return light not detected';
               $007 : DataStr := 'Strobe return light detected';
               $009 : DataStr := 'Flash fired, compulsory flash mode';
               $00D : DataStr := 'Flash fired, compulsory flash mode, return light not detected';
               $00F : DataStr := 'Flash fired, compulsory flash mode, return light detected';
               $010 : DataStr := 'Flash did not fire, compulsory flash mode';
               $018 : DataStr := 'Flash did not fire, auto mode';
               $019 : DataStr := 'Flash fired, auto mode';
               $01D : DataStr := 'Flash fired, auto mode, return light not detected';
               $01F : DataStr := 'Flash fired, auto mode, return light detected';
               $020 : DataStr := 'No flash function';
               $041 : DataStr := 'Flash fired, red-eye reduction mode';
               $045 : DataStr := 'Flash fired, red-eye reduction mode, return light not detected';
               $047 : DataStr := 'Flash fired, red-eye reduction mode, return light detected';
               $049 : DataStr := 'Flash fired, compulsory flash mode, red-eye reduction mode';
               $04D : DataStr := 'Flash fired, compulsory flash mode, red-eye reduction mode, return light not detected';
               $04F : DataStr := 'Flash fired, compulsory flash mode, red-eye reduction mode, return light detected';
               $059 : DataStr := 'Flash fired, auto mode, red-eye reduction mode';
               $05D : DataStr := 'Flash fired, auto mode, return light not detected, red-eye reduction mode';
               $05F : DataStr := 'Flash fired, auto mode, return light detected, red-eye reduction mode';
               else DataStr := '';
               end;
               if (DataStr <> '')
               then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);
          end;
          if (FlashEnergy > 0.0)
          then InsertRow(sgExif, 'Flash energy', FloatToStrF(FlashEnergy, ffFixed, 7, 2) + ' BCPS', AppendRow);

      //    FFrequencyResponse        : string; // Use SetLength and copy as were it bytes in an array.

          if (FocalLength > 0.0)
          then InsertRow(sgExif, 'Focal length', FloatToStrF(FocalLength, ffFixed, 7, 2) + ' mm', AppendRow);

          if (FocalLengthIn35mmFilm > 0)
          then InsertRow(sgExif, 'Focal length (35mm film)', IntToStr(FocalLengthIn35mmFilm), AppendRow);

          case FocalPlaneResolutionUnit of
          2 : ExifUnit := ' pixel/inch';
          3 : ExifUnit := ' pixel/cm';
          else ExifUnit := '';
          end;

          if (FocalPlaneXResolution > 0.0)
          then InsertRow(sgExif, 'Focal plane horizontal', FloatToStrF(FocalPlaneXResolution, ffGeneral, 7, 2) + ExifUnit, AppendRow);
          if (FocalPlaneYResolution > 0.0)
          then InsertRow(sgExif, 'Focal plane vertical', FloatToStrF(FocalPlaneYResolution, ffGeneral, 7, 2) + ExifUnit, AppendRow);

          if (SubjectDistance > 0.0)
          then InsertRow(sgExif, 'Subject distance', FloatToStrF(SubjectDistance, ffFixed, 7, 2) + ' m', AppendRow)
          else if (SubjectDistance = -1.0)
               then InsertRow(sgExif, 'Subject distance', 'Infinit', AppendRow)
               else if (SubjectDistance = 0.0)
                    then InsertRow(sgExif, 'Subject distance', 'Unknown', AppendRow);

          HeaderStr := 'Distance to the subject';
          case SubjectDistanceRange of
          0 : DataStr := 'Unknown';
          1 : DataStr := 'Macro';
          2 : DataStr := 'Close view';
          3 : DataStr := 'Distant view';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

          if (SubjectLocation[0] <> 0) or (SubjectLocation[1] <> 0)
          then InsertRow(sgExif, 'Sub location (x,y)',
                                 '(' + IntToStr(SubjectLocation[0]) +
                                 ', ' + IntToStr(SubjectLocation[1]) + ')',
                                 AppendRow);

          if (SubjectAreaLength > 0)
          then begin
               DataStr := '(';
               case SubjectAreaLength of
               2 : begin
                     HeaderStr := 'Subject area (x,y)';
                     DataStr := DataStr + IntToStr(SubjectArea[0]) + ', ';
                     DataStr := DataStr + IntToStr(SubjectArea[1]) + ')';
                   end;
               3 : begin
                     HeaderStr := 'Subject area (x,y,d)';
                     DataStr := IntToStr(SubjectArea[0]) + ', ';
                     DataStr := DataStr + IntToStr(SubjectArea[1]) + ', ';
                     DataStr := DataStr + IntToStr(SubjectArea[1]) + ')';
                   end;
               4 : begin
                     HeaderStr := 'Subject area (x,y),(dx,dy)';
                     DataStr := DataStr + IntToStr(SubjectArea[0]) + ', ';
                     DataStr := DataStr + IntToStr(SubjectArea[1]) + ') (';
                     DataStr := DataStr + IntToStr(SubjectArea[2]) + ', ';
                     DataStr := DataStr + IntToStr(SubjectArea[3]) + ')';
                   end;
               end;

               InsertRow(sgExif, HeaderStr, DataStr, AppendRow);
          end;

          HeaderStr := 'White balance mode';
          case WhiteBalance of
          0 : DataStr := 'Auto';
          1 : DataStr := 'Manual';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

          HeaderStr := 'Gain control';
          case GainControl of
          0 : DataStr := 'None';
          1 : DataStr := 'Low gain up';
          2 : DataStr := 'High gain up';
          3 : DataStr := 'Low gain down';
          4 : DataStr := 'High gain down';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

          HeaderStr := 'Contrast';
          case Contrast of
          0 : DataStr := 'Normal';
          1 : DataStr := 'Soft';
          2 : DataStr := 'Hard';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

          HeaderStr := 'Saturation';
          case Saturation of
          0 : DataStr := 'Normal';
          1 : DataStr := 'Low saturation';
          2 : DataStr := 'High saturation';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

          HeaderStr := 'Sharpness';
          case Sharpness of
          0 : DataStr := 'Normal';
          1 : DataStr := 'Soft';
          2 : DataStr := 'Hard';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

          HeaderStr := 'Custom rendered';
          case CustomRendered of
          0 : DataStr := 'Normal process';
          1 : DataStr := 'Custom process';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

          if (DigitalZoomRatio > 0.0)
          then InsertRow(sgExif, 'Digital zoom ratio', FloatToStrF(DigitalZoomRatio, ffFixed, 7, 2), AppendRow);

          if (FileSource = 3)
          then InsertRow(sgExif, 'File source', 'DSC, Digital Camera', AppendRow);

          HeaderStr := 'Image sensor type';
          case SensingMethod of
          1 : DataStr := 'Not defined';
          2 : DataStr := 'One-chip color area sensor';
          3 : DataStr := 'Two-chip color area sensor';
          4 : DataStr := 'Three-chip color area sensor';
          5 : DataStr := 'Color sequential area sensor';
          7 : DataStr := 'Trilinear sensor';
          8 : DataStr := 'Color sequential linear sensor';
          else DataStr := '';
          end;
          if (DataStr <> '')
          then InsertRow(sgExif, HeaderStr, DataStr, AppendRow);

      //    FCFAPattern               :

      //    FDeviceSettingDescription :

          if (ExifVersion <> 0)
          then InsertRow(sgExif, 'Exif version', FloatToStrF(ExifVersion / 100.0, ffFixed, 7, 2), AppendRow);

          if (FlashPixVersion <> 0)
          then InsertRow(sgExif, 'FlashPix version', FloatToStrF(FlashPixVersion / 100.0, ffFixed, 7, 2), AppendRow);

          if (InteroperabilityIndex <> '')
          then begin
               InsertRow(sgExif, 'Interoperability index', InteroperabilityIndex, AppendRow);
               if (InteroperabilityVersion > 0)
               then InsertRow(sgExif, 'Interoperability version', FloatToStrF(InteroperabilityVersion / 100.0, ffFixed, 7, 2), AppendRow);
          end;
       end;
  end;
end; // TFormImageInfo.ShowExifInfo.


procedure TFormImageInfo.ShowGPSInfo;
var DataStr   : string;
    AppendRow : boolean;
    i         : integer;
begin
  if Assigned(FImageInfo.GPS)
  then begin
       tsGPS.TabVisible := True;
       with FImageInfo.GPS
       do begin
          AppendRow := True;

          if (VersionID <> 0)
          then InsertRow(sgGPS, 'GPS version', IntToStr(VersionID), AppendRow)
          else InsertRow(sgGPS, 'GPS version', 'Unknown', AppendRow);      
          
          if (Latitude[0] >= 0.0)
          then begin
               DataStr := '';
               for i := 0 to 2
               do begin
                  DataStr := DataStr + FloatToStrF(Latitude[i], ffFixed, 7, 2);
                  if (i < 2)
                  then DataStr := DataStr + '/'
                  else DataStr := DataStr + ' ';
               end;
               if (LatitudeRef <> '')
               then DataStr := DataStr + LatitudeRef;
               InsertRow(sgGPS, 'Latitude', DataStr, AppendRow);
          end;

          if (Longitude[0] >= 0.0)
          then begin
               DataStr := '';
               for i := 0 to 2
               do begin
                  DataStr := DataStr + FloatToStrF(Longitude[i], ffFixed, 7, 2);
                  if (i < 2)
                  then DataStr := DataStr + '/'
                  else DataStr := DataStr + ' ';
               end;
               if (LongitudeRef <> '')
               then DataStr := DataStr + LongitudeRef;
               InsertRow(sgGPS, 'Longitude', DataStr, AppendRow);
          end;

          if (Altitude >= 0.0)
          then begin
               DataStr :=  FloatToStrF(Altitude, ffFixed, 7, 2);
               if (AltitudeRef > -1)
               then begin
                    case AltitudeRef of
                    0 : DataStr := DataStr + ' Above sea level';
                    1 : DataStr := DataStr + ' Below sea level';
                    end;
               end;
               InsertRow(sgGPS, 'Altitude', DataStr, AppendRow);
          end;

          if (TimeStamp[0] >= 0.0)
          then begin
               DataStr := '';
               for i := 0 to 2
               do begin
                  DataStr := DataStr + FloatToStrF(TimeStamp[i], ffFixed, 7, 2);
                  if (i < 2)
                  then DataStr := DataStr + '/';
               end;
               InsertRow(sgGPS, 'GPS time (atomic clock)', DataStr, AppendRow);
          end;

          if (Satellites <> '')
          then InsertRow(sgGPS, 'GPS satellites used for measurement', Satellites, AppendRow);

          if (Status <> '')
          then begin
               case Status[1] of
               'A' : DataStr := 'Measurement in progress';
               'V' : DataStr := 'Measurement Interoperability';
               end;
               InsertRow(sgGPS, 'GPS receiver status', DataStr, AppendRow);
          end;

          if (MeasureMode <> '')
          then InsertRow(sgGPS, 'GPS measurement mode', MeasureMode + '-dimensional measurement', AppendRow);

          if (DOP <> 0)
          then InsertRow(sgGPS, 'Measurement precision', FloatToStrF(DOP, ffFixed, 7, 2), AppendRow);

          if (Speed <> 0)
          then begin
               DataStr := FloatToStrF(Speed, ffFixed, 7, 2);
               if (SpeedRef <> '')
               then begin
                    case SpeedRef[1] of
                    'K' : DataStr := DataStr + ' kilometers/hour';
                    'M' : DataStr := DataStr + ' Miles/hour';
                    'N' : DataStr := DataStr + ' Knots';
                    end;
               end;
               InsertRow(sgGPS, 'Speed of GPS receiver', DataStr, AppendRow);
          end;

          if (Track >= 0)
          then begin
               DataStr := FloatToStrF(Track, ffFixed, 7, 2);
               if (TrackRef <> '')
               then begin
                    case TrackRef[1] of
                    'T' : DataStr := DataStr + ' True direction';
                    'M' : DataStr := DataStr + ' Magnetic direction';
                    end;
               end;
               InsertRow(sgGPS, 'Direction of movement', DataStr, AppendRow);
          end;

          if (ImgDirection >= 0.0)
          then begin
               DataStr := FloatToStrF(ImgDirection, ffFixed, 7, 2);
               if (ImgDirectionRef <> '')
               then begin
                    case ImgDirectionRef[1] of
                    'T' : DataStr := DataStr + ' True direction';
                    'M' : DataStr := DataStr + ' Magnetic direction';
                    end;
               end;
               InsertRow(sgGPS, 'Direction of image', DataStr, AppendRow);
          end;

          if (MapDatum <> '')
          then InsertRow(sgGPS, 'Geodetic survey data used', MapDatum, AppendRow);

          if (DestLatitude[0] >= 0.0)
          then begin
               DataStr := '';
               for i := 0 to 2
               do begin
                  DataStr := DataStr + FloatToStrF(DestLatitude[i], ffFixed, 7, 2);
                  if (i < 2)
                  then DataStr := DataStr + '/'
                  else DataStr := DataStr + ' ';
               end;
               if (DestLatitudeRef <> '')
               then DataStr := DataStr + DestLatitudeRef;
               InsertRow(sgGPS, 'Latitude of destination', DataStr, AppendRow);
          end;

          if (DestLongitude[0] >= 0.0)
          then begin
               DataStr := '';
               for i := 0 to 2
               do begin
                  DataStr := DataStr + FloatToStrF(DestLongitude[i], ffFixed, 7, 2);
                  if (i < 2)
                  then DataStr := DataStr + '/'
                  else DataStr := DataStr + ' ';
               end;
               if (DestLongitudeRef <> '')
               then DataStr := DataStr + DestLongitudeRef;
               InsertRow(sgGPS, 'Longitude of destination', DataStr, AppendRow);
          end;

          if (DestBearing >= 0.0)
          then begin
               DataStr := FloatToStrF(DestBearing, ffFixed, 7, 2);
               if (DestBearingRef <> '')
               then begin
                    case DestBearingRef[1] of
                    'T' : DataStr := DataStr + ' True direction';
                    'M' : DataStr := DataStr + ' Magnetic direction';
                    end;
               end;
               InsertRow(sgGPS, 'Bearing of destination', DataStr, AppendRow);
          end;

          if (DestDistance >= 0.0)
          then begin
               DataStr := FloatToStrF(DestDistance, ffFixed, 7, 2);
               if (DestDistanceRef <> '')
               then begin
                    case DestDistanceRef[1] of
                    'K' : DataStr := DataStr + ' kilometers';
                    'M' : DataStr := DataStr + ' Miles';
                    'N' : DataStr := DataStr + ' Knots';
                    end;
               end;
               InsertRow(sgGPS, 'Distance to destination', DataStr, AppendRow);
          end;
(*
          if (ProcessingMethod <> Nil)
          then begin
          end;
          if (AreaInformation <> Nil)
          then begin
          end;
*)
          if (DateStamp <> 0)
          then InsertRow(sgGPS, 'GPS date', DateToStr(DateStamp), AppendRow);

          if (Differential <> 0)
          then InsertRow(sgGPS, 'GPS differential correction', IntToStr(Differential), AppendRow);
       end;
  end;
end; // TFormImageInfo.ShowGPSInfo.


procedure TFormImageInfo.btnOKClick(Sender : TObject);
begin
  // Assign changed data to ImageInfo.
  if Assigned(FImageInfo)
  then begin
       with FImageInfo
       do begin
          FileFormat   := TmcmFileFormat(cbFileFormat.ItemIndex);
          // eDateTime.Text;
          Artist       := eArtist.Text;
          Copyright    := eCopyright.Text;
          Description  := mDescription.Lines.Text;

          DocumentName := eDocumentName.Text;
          PageName     := ePageName.Text;
          PageNumber   := StrToInt(ePageNumber.Text);

          HostComputer := eHostComputer.Text;
          Make         := eMake.Text;
          Model        := eModel.Text;
          Software     := eSoftware.Text;
          // eVersion.Text;
       end;
  end;
end; // TFormImageInfo.btnOKClick.


procedure TFormImageInfo.cbFileFormatChange(Sender : TObject);
begin
  case TmcmFileFormat(cbFileFormat.ItemIndex) of
  // FF_GIF   : ;
  // FF_CGM   : ;
  FF_CRI      : ;
  // FF_DICOM : ;
  // FF_CUT   : ;
  // FF_FPX   : ;
  // FF_IMG   : ;
  FF_JPEG  : ;
  // FF_KDC   : ;
  // FF_PCD   : ;
  // FF_PICT  : ;
  // FF_PSP   : ;
  FF_PBM   : ;
  FF_PGM   : ;
  // FF_PNG   : ;
  FF_PPM   : ;
  // FF_SGI   : ;
  FF_TIFF  : ;
  FF_Targa : ;
  // FF_EMF   : ;
  // FF_WMF   : ;
  FF_ICO   : ;
  // FF_RLE   : ;
  FF_BMP   : ;
  FF_DIB   : ;
  // FF_WPG   : ;
  FF_PCX   : ;
  FF_NONE  : ;
  end;
end; // TFormImageInfo.cbFileFormatChange.

end.
