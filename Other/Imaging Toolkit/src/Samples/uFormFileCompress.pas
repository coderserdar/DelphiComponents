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
// $Log:  17599: uFormFileCompress.pas 
//
//   Rev 1.7    24-11-2003 20:15:40  mcm

//
//   Rev 1.6    25-07-2003 00:02:00  mcm
// Included LZW compression for TIFF files.

//
//   Rev 1.5    06-07-2003 10:56:58  mcm    Version: IMG 1.3.4
// Modified to work in BCB.
// Initial steps towards including LZW.

//
//   Rev 1.4    27-01-2003 14:19:08  mcm
// Modified to match new JPEG engine

//
//   Rev 1.3    27-09-2002 13:11:56  mcm    Version: IMG 1.2
// Included SGI support.

//
//   Rev 1.2    07-08-2002 14:10:28  mcm    Version: IMG 1.1
// Added GIF.

//
//   Rev 1.1    01-08-2002 11:38:18  mcm    Version: IMG 1.1
// Added CCITT Group 3 & 4.

//
//   Rev 1.0    27-05-2002 16:22:32  mcm

unit uFormFileCompress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ComCtrls,
  Registry,
  mcmImage, mcmImageTypeDef;

const FCRegKey : string = '\SOFTWARE\MCM DESIGN\mcmImaging\Compress\';

type
  TFormFileCompress = class(TForm)
    pcCompress         : TPageControl;
    tsCri              : TTabSheet;
    cbCRI              : TGroupBox;
    rbCRIUncompress    : TRadioButton;
    tsJpeg             : TTabSheet;
    gbJPEG             : TGroupBox;
    lJPEGQuality       : TLabel;
    seQuality          : TSpinEdit;
    rbJPEGStandard     : TRadioButton;
    rbJPEGProgressive  : TRadioButton;
    tsTiff             : TTabSheet;
    gbTIFF             : TGroupBox;
    rbTIFFFaxCcitt3    : TRadioButton;
    rbTIFFFaxCcitt3_2D : TRadioButton;
    rbTIFFFaxCcitt4    : TRadioButton;
    rbTIFFHuffmanRLE   : TRadioButton;
    rbTIFFLZW          : TRadioButton;
    rbTIFFPackBits     : TRadioButton;
    rbTIFFUncompress   : TRadioButton;
    tsTarga            : TTabSheet;
    gbTARGA            : TGroupBox;
    rbTGACompress      : TRadioButton;
    rbTGAUncompress    : TRadioButton;
    tsBmp              : TTabSheet;
    gbBMP              : TGroupBox;
    rbBMPRGB           : TRadioButton;
    rbBMP4RLE          : TRadioButton;
    rbBMP8RLE          : TRadioButton;
    tsGIF              : TTabSheet;
    gbGIF              : TGroupBox;
    rbGIFVersion87A    : TRadioButton;
    rbGIFVersion89A    : TRadioButton;
    gbGIFInterlacing   : TGroupBox;
    rbGIFInterlaced    : TRadioButton;
    rbGIFNonInterlaced : TRadioButton;
    tsICO              : TTabSheet;
    gbICO              : TGroupBox;
    tsPBM              : TTabSheet;
    gbPBM              : TGroupBox;
    rbPBMBinary        : TRadioButton;
    rbPBMASCII         : TRadioButton;
    tsPGM              : TTabSheet;
    gbPGM              : TGroupBox;
    rbPGMBinary        : TRadioButton;
    rbPGMASCII         : TRadioButton;
    tsPPM              : TTabSheet;
    gbPPM              : TGroupBox;
    rbPPMBinary        : TRadioButton;
    rbPPMASCII         : TRadioButton;
    tsPNG              : TTabSheet;
    gbPNG              : TGroupBox;
    rbPNGProgressive   : TRadioButton;
    rbPNGStandard      : TRadioButton;
    cbPNGQuality       : TComboBox;
    lPNGQuality        : TLabel;
    btnOK              : TButton;
    btnCancel          : TButton;
    tsSGI              : TTabSheet;
    gbSGI              : TGroupBox;
    rbSGIUncompressed  : TRadioButton;
    rbSGIRLE           : TRadioButton;
    procedure FormCreate(Sender : TObject);
    procedure seQualityChange(Sender : TObject);
    procedure rbSelectCompressClick(Sender : TObject);
    procedure btnOKClick(Sender : TObject);
    procedure rbSelectGIFCompressClick(Sender : TObject);
    procedure cbPNGQualityChange(Sender : TObject);
  private
    { Private declarations }
    FFileFormat     : TmcmFileFormat;
    FImage          : TmcmImage;
    FCompress       : TmcmCompress;
    FQuality        : word;
    FInterlaced     : boolean;
    FRegPath        : string;
    FRootKey        : HKEY;
  protected
    procedure SetCompress(Value : TmcmCompress);
    procedure SetInterlaced(Value : boolean);
    procedure SetQuality(Value : word);
    procedure SetRootKey(Value : HKEY);
    procedure SetRegPath(Value : string);
    procedure CheckCompress(Control : TWinControl);
  public
    { Public declarations }
    function SetImageAndFormat(Image : TmcmImage; FileFormat : TmcmFileFormat) : boolean;
    property Compress : TmcmCompress
      read   FCompress
      write  SetCompress;
    property Interlaced : boolean
      read   FInterlaced
      write  SetInterlaced;
    property Quality : word
      read   FQuality
      write  SetQuality;
    property RegPath : string
      read   FRegPath
      write  SetRegPath;
    property RootKey : HKEY
      read   FRootKey
      write  SetRootKey default HKEY_LOCAL_MACHINE;
  end;

var FormFileCompress : TFormFileCompress;

implementation

{$R *.DFM}

procedure TFormFileCompress.FormCreate(Sender : TObject);
var i : integer;
begin
  FImage := Nil;
  for i := 0 to (pcCompress.PageCount - 1)
  do pcCompress.Pages[i].TabVisible := False;

  FRootKey  := HKEY_LOCAL_MACHINE;
  FRegPath  := FCRegKey;
  FCompress := CP_NONE;
  FQuality  := 100;

  rbCRIUncompress.Tag    := integer(CP_NONE);

  rbBMPRGB.Tag           := integer(CP_NONE);
  rbBMP4RLE.Tag          := integer(CP_RLE4);
  rbBMP8RLE.Tag          := integer(CP_RLE8);

  rbTIFFFaxCcitt3.Tag    := integer(CP_CCITTGROUP3);
  rbTIFFFaxCcitt3_2D.Tag := integer(CP_CCITTGROUP3_2D);
  rbTIFFFaxCcitt4.Tag    := integer(CP_CCITTGROUP4);
  rbTIFFHuffmanRLE.Tag   := integer(CP_MODIFIED_HUFFMAN);
  rbTIFFLZW.Tag          := integer(CP_LZW);
  rbTIFFPackBits.Tag     := integer(CP_PACKBITS);
  rbTIFFUncompress.Tag   := integer(CP_NONE);

  rbTGACompress.Tag      := integer(CP_RLE_TARGA);
  rbTGAUncompress.Tag    := integer(CP_NONE);

  rbJPEGStandard.Tag     := integer(CP_JPEG_STD);
  rbJPEGProgressive.Tag  := integer(CP_JPEG_PROG);

  rbPNGStandard.Tag      := integer(CP_PNG);
  rbPNGProgressive.Tag   := integer(CP_PNG);

  rbGIFVersion87A.Tag    := integer(CP_GIF87A);
  rbGIFVersion89A.Tag    := integer(CP_GIF89A);

  rbPBMBinary.Tag        := integer(CP_BIN_PBM);
  rbPBMASCII.Tag         := integer(CP_ASCII_PBM);

  rbPGMBinary.Tag        := integer(CP_BIN_PGM);
  rbPGMASCII.Tag         := integer(CP_ASCII_PGM);

  rbPPMBinary.Tag        := integer(CP_BIN_PPM);
  rbPPMASCII.Tag         := integer(CP_ASCII_PPM);

  rbSGIUncompressed.Tag  := integer(CP_NONE);
  rbSGIRLE.Tag           := integer(CP_RLE_SGI);
end; // TFormFileCompress.FormCreate.


procedure TFormFileCompress.seQualityChange(Sender : TObject);
begin
  if (seQuality.Text <> '')
  then FQuality := seQuality.Value;
end; // TFormFileCompress.seQualityChange.


procedure TFormFileCompress.rbSelectCompressClick(Sender : TObject);
begin
  FCompress := TmcmCompress(TRadioButton(Sender).Tag);
  case FCompress of
  {
  CP_JPEG : if (Sender = rbJPEGProgressive)
            then FInterlaced := False
            else FInterlaced := True;
  }
  CP_PNG  : if (Sender = rbPNGProgressive) or False
            then FInterlaced := False
            else FInterlaced := True;
  end;
end; // TFormFileCompress.rbSelectCompressClick.


procedure TFormFileCompress.rbSelectGIFCompressClick(Sender : TObject);
begin
  FInterlaced := rbGIFInterlaced.Checked
end; // TFormFileCompress.rbSelectGIFCompressClick.


procedure TFormFileCompress.btnOKClick(Sender : TObject);
var Reg : TRegIniFile;
begin
  // Recall last directory and file extension.
  Reg := TRegIniFile.Create('');
  Reg.RootKey := FRootKey; // HKEY_LOCAL_MACHINE; // HKEY_CURRENT_USER;
  if Reg.OpenKey(FRegPath, True)
  then begin
       try
         case FFileFormat of
         FF_CRI   : begin
                      Reg.WriteInteger('CRI', 'c', integer(FCompress));
                      // Reg.WriteInteger('CRI', 'Q', FQuality);
                    end;
         FF_GIF   : begin
                      Reg.WriteInteger('GIF', 'C', integer(FCompress));
                      Reg.WriteBool('GIF', 'I', FInterlaced);
                    end;
         FF_TIFF  : begin
                      Reg.WriteInteger('TIFF', 'C', integer(FCompress));
                      // FQuality  := Reg.ReadInteger('TIFF', 'Q', FQuality);
                    end;
         FF_BMP   : begin
                      Reg.WriteInteger('BMP', 'C', integer(FCompress));
                      // Reg.WriteInteger('BMP', 'Q', FQuality);
                    end;
         FF_DIB   : begin
                      Reg.WriteInteger('BMP', 'C', integer(FCompress));
                      // Reg.WriteInteger('BMP', 'Q', FQuality);
                    end;
         // FF_DICOM : ;
         FF_ICO   : begin
                      // Reg.WriteInteger('ICO', 'C', integer(FCompress));
                      // Reg.WriteInteger('ICO', 'Q', FQuality);
                    end;
         FF_JPEG  : begin
                      Reg.WriteInteger('JPEG', 'C', integer(FCompress));
                      Reg.WriteInteger('JPEG', 'Q', FQuality);
                      Reg.WriteBool('JPEG', 'I', FInterlaced);
                    end;
         // FF_PCX   : ;
         FF_PBM   : begin
                      Reg.WriteInteger('PBM', 'C', integer(FCompress));
                      // Reg.WriteInteger('PBM', 'Q', FQuality);
                    end;
         FF_PGM   : begin
                      Reg.WriteInteger('PGM', 'C', integer(FCompress));
                      // Reg.WriteInteger('PMG', 'Q', FQuality);
                    end;
         FF_PNG   : begin
                      Reg.WriteInteger('PNG', 'C', integer(FCompress));
                      Reg.WriteInteger('PNG', 'Q', FQuality);
                    end;
         FF_PPM   : begin
                      Reg.WriteInteger('PPM', 'C', integer(FCompress));
                      // Reg.WriteInteger('PPM', 'Q', FQuality);
                    end;
         FF_SGI   : begin
                      Reg.WriteInteger('SGI', 'C', integer(FCompress));
                      // Reg.WriteInteger('SGI', 'Q', FQuality);
                    end;
         FF_TARGA : begin
                      Reg.WriteInteger('TARGA', 'C', integer(FCompress));
                      // Reg.WriteInteger('TARGA', 'Q', FQuality);
                    end;
         end;
       except
         on E:Exception
         do ShowMessage(E.Message);
       end;
       Reg.CloseKey;
  end;
  Reg.Free;
end; // TFormFileCompress.btnOKClick.


procedure TFormFileCompress.SetRootKey(Value : HKEY);
begin
  FRootKey := Value;
end; // TFormFileCompress.SetRootKey.


procedure TFormFileCompress.SetRegPath(Value : string);
begin
  FRegPath := Value;
end; // TFormFileCompress.SetRegPath.


procedure TFormFileCompress.SetCompress(Value : TmcmCompress);
begin
  FCompress := Value;
end; // TFormFileCompress.SetCompress.


procedure TFormFileCompress.SetInterlaced(Value : boolean);
begin
  FInterlaced := Value;
end; // TFormFileCompress.SetInterlaced.


procedure TFormFileCompress.SetQuality(Value : word);
begin
  FQuality := Value;
  seQuality.Value := Value;
end; // TFormFileCompress.SetQuality.


procedure TFormFileCompress.CheckCompress(Control : TWinControl);
var i : integer;
begin
  for i := 0 to (Control.ControlCount - 1)
  do begin
     if (Control.Controls[i] is TRadioButton)
     then begin
          if (TRadioButton(Control.Controls[i]).Tag = integer(FCompress))
          then if Not(TRadioButton(Control.Controls[i]).Enabled)
               then FCompress := CP_NONE
               else TRadioButton(Control.Controls[i]).Checked := True;
     end;
  end;
end; // TFormFileCompress.CheckCompress.


function TFormFileCompress.SetImageAndFormat(Image      : TmcmImage;
                                             FileFormat : TmcmFileFormat) : boolean;
var Reg : TRegIniFile;
begin
  FImage      := Image;
  FFileFormat := FileFormat;

  // Recall last directory and file extension.
  Reg := TRegIniFile.Create('');
  Reg.RootKey := FRootKey; // HKEY_LOCAL_MACHINE; // HKEY_CURRENT_USER;
  if Reg.OpenKey(FRegPath, True)
  then begin
       try
         case FFileFormat of
         FF_CRI   : begin
                      pcCompress.ActivePage := tsCRI;
                      FCompress := TmcmCompress(Reg.ReadInteger('CRI', 'C', integer(CP_NONE)));
                      FQuality  := Reg.ReadInteger('CRI', 'Q', 100);
                      if Assigned(FImage)
                      then begin
                      end;
                      FCompress := CP_NONE;
                    end;
         FF_GIF   : begin
                      pcCompress.ActivePage := tsGIF;
                      FCompress := TmcmCompress(Reg.ReadInteger('GIF', 'C', integer(CP_NONE)));
                      FInterlaced := Reg.ReadBool('GIF', 'I', True);
                      // FQuality  := Reg.ReadInteger('GIF', 'Q', 100);
                      if Assigned(FImage)
                      then begin
                           case FImage.ImageFormat of
                           IF_BW,
                           IF_GREY4,
                           IF_PAL4,
                           IF_GREY8,
                           IF_PAL8   : begin
                                         rbGIFVersion87A.Enabled := True;
                                         rbGIFVersion89A.Enabled := True;
                                         rbGIFInterlaced.Enabled := True;
                                         rbGIFNonInterlaced.Enabled := True;
                                       end;
                           end;
                           if FInterlaced
                           then rbGIFInterlaced.Checked := True
                           else rbGIFNonInterlaced.Checked := True;
                           CheckCompress(gbGIF);
                      end;
                    end;
         FF_SGI   : begin
                      pcCompress.ActivePage := tsSGI;
                      FCompress := TmcmCompress(Reg.ReadInteger('SGI', 'C', integer(CP_NONE)));
                      if Assigned(FImage)
                      then begin
                           case FImage.ImageFormat of
                           IF_GREY8  : begin
                                         rbSGIRLE.Enabled  := True;
                                         rbSGIUncompressed.Enabled  := True;
                                       end;
                           IF_RGB24..
                           IF_RGBA32 : begin
                                         rbSGIRLE.Enabled  := True;
                                         rbSGIUncompressed.Enabled  := True;
                                       end;
                           end;
                           CheckCompress(gbSGI);
                      end;
                    end;
         FF_TIFF  : begin
                      pcCompress.ActivePage := tsTIFF;
                      FCompress := TmcmCompress(Reg.ReadInteger('TIFF', 'C', integer(CP_NONE)));
                      FQuality  := Reg.ReadInteger('TIFF', 'Q', 100);
                      if Assigned(FImage)
                      then begin
                           case FImage.ImageFormat of
                           IF_BW     : begin
                                         rbTIFFUncompress.Enabled := True;
                                         rbTIFFFaxCcitt3.Enabled := True;
                                         rbTIFFFaxCcitt3_2D.Enabled := True;
                                         rbTIFFFaxCcitt4.Enabled := True;
                                         rbTIFFHuffmanRLE.Enabled := True;
                                         rbTIFFPackBits.Enabled := True;
                                         {$IFDEF mcmLZW}
                                           rbTIFFLZW.Enabled := True;
                                         {$ENDIF}
                                       end;
                           IF_GREY4,
                           IF_PAL4,
                           IF_GREY8,
                           IF_PAL8,
                           IF_RGB24  : begin
                                         rbTIFFUncompress.Enabled := True;
                                         rbTIFFPackBits.Enabled := True;
                                         {$IFDEF mcmLZW}
                                           rbTIFFLZW.Enabled := True;
                                         {$ENDIF}
                                       end;
                           IF_RGB15,
                           IF_RGB16,
                           IF_RGBA32 : begin
                                         rbTIFFUncompress.Enabled := True;
                                         {$IFDEF mcmLZW}
                                           rbTIFFLZW.Enabled := True;
                                         {$ENDIF}
                                       end;
                           else begin
                           end;
                           end;
                           CheckCompress(gbTIFF);
                      end;
                    end;
         FF_BMP   : begin
                      pcCompress.ActivePage := tsBMP;
                      FCompress := TmcmCompress(Reg.ReadInteger('BMP', 'C', integer(CP_NONE)));
                      // FQuality  := Reg.ReadInteger('BMP', 'Q', 100);
                      if Assigned(FImage)
                      then begin
                           case FImage.ImageFormat of
                           IF_GREY4,
                           IF_PAL4   : begin
                                         rbBMP4RLE.Enabled := True;
                                         rbBMPRGB.Enabled  := True;
                                       end;
                           IF_GREY8,
                           IF_PAL8   : begin
                                         rbBMP8RLE.Enabled := True;
                                         rbBMPRGB.Enabled  := True;
                                       end;
                           IF_BW,
                           IF_RGB15..
                           IF_RGBA32 : begin
                                         rbBMPRGB.Enabled  := True;
                                       end;
                           end;
                           CheckCompress(gbBMP);
                      end;
                    end;
         FF_DIB   : begin
                      pcCompress.ActivePage := tsBMP;
                      FCompress := TmcmCompress(Reg.ReadInteger('BMP', 'C', integer(CP_NONE)));
                      // FQuality  := Reg.ReadInteger('BMP', 'Q', 100);
                      if Assigned(FImage)
                      then begin
                           case FImage.ImageFormat of
                           IF_GREY4,
                           IF_PAL4   : begin
                                         rbBMP4RLE.Enabled := True;
                                         rbBMPRGB.Enabled  := True;
                                       end;
                           IF_GREY8,
                           IF_PAL8   : begin
                                         rbBMP8RLE.Enabled := True;
                                         rbBMPRGB.Enabled  := True;
                                       end;
                           IF_BW,
                           IF_RGB15..
                           IF_RGBA32 : begin
                                         rbBMPRGB.Enabled  := True;
                                       end;
                           end;
                           CheckCompress(gbBMP);
                      end;
                    end;
         // FF_DICOM : ;
         FF_ICO   : begin
                      pcCompress.ActivePage := tsICO;
                      FCompress := TmcmCompress(Reg.ReadInteger('ICO', 'C', integer(CP_NONE)));
                      FQuality  := Reg.ReadInteger('ICO', 'Q', 100);
                      if Assigned(FImage)
                      then begin
                      end;
                      FCompress := CP_NONE;
                    end;
         FF_JPEG  : begin
                      pcCompress.ActivePage := tsJPEG;
                      FCompress := TmcmCompress(Reg.ReadInteger('JPEG', 'C', integer(CP_NONE)));
                      Quality   := Reg.ReadInteger('JPEG', 'Q', 100);
                      FInterlaced := Reg.ReadBool('JPEG', 'I', True);
                      if Assigned(FImage)
                      then begin
                           case FImage.ImageFormat of
                           IF_GREY8,
                           IF_RGB24,
                           IF_RGBA32 : begin
                                         rbJPEGStandard.Enabled    := True;
                                         rbJPEGProgressive.Enabled := True;
                                       end;
                           end;
                           CheckCompress(gbJPEG);
                      end;
                    end;
         // FF_PCX   : ;
         FF_PBM   : begin
                      pcCompress.ActivePage := tsPBM;
                      FCompress := TmcmCompress(Reg.ReadInteger('PBM', 'C', integer(CP_BIN_PBM)));
                      if Assigned(FImage)
                      then begin
                           case FImage.ImageFormat of
                           IF_BW : begin
                                     rbPBMBinary.Enabled := True;
                                     rbPBMASCII.Enabled := True;
                                   end;
                           end;
                           CheckCompress(gbPBM);
                      end;
                    end;
         FF_PGM   : begin
                      pcCompress.ActivePage := tsPGM;
                      FCompress := TmcmCompress(Reg.ReadInteger('PGM', 'C', integer(CP_BIN_PGM)));
                      if Assigned(FImage)
                      then begin
                           case FImage.ImageFormat of
                           IF_GREY8 : begin
                                        rbPGMBinary.Enabled := True;
                                        rbPGMASCII.Enabled := True;
                                      end;
                           end;
                           CheckCompress(gbPGM);
                      end;
                    end;
         FF_PNG   : begin
                      pcCompress.ActivePage := tsPNG;
                      FCompress := TmcmCompress(Reg.ReadInteger('PNG', 'C', integer(CP_PNG)));
                      FQuality  := Reg.ReadInteger('PNG', 'Q', 100);
                      FInterlaced := Reg.ReadBool('PNG', 'I', True);

                      if (FInterlaced)
                      then rbPNGStandard.Checked := True
                      else rbPNGProgressive.Checked := True;

                      case FQuality of
                      0..25  : cbPNGQuality.ItemIndex := 0;
                      26..50 : cbPNGQuality.ItemIndex := 1;
                      51..75 : cbPNGQuality.ItemIndex := 2;
                      else     cbPNGQuality.ItemIndex := 3;
                      end;
                      if Assigned(FImage)
                      then begin
                           rbPNGProgressive.Enabled := False;
                           rbPNGStandard.Enabled := True;
                           CheckCompress(gbPNG);
                      end;
                    end;
         FF_PPM   : begin
                      pcCompress.ActivePage := tsPPM;
                      FCompress := TmcmCompress(Reg.ReadInteger('PPM', 'C', integer(CP_BIN_PPM)));
                      if Assigned(FImage)
                      then begin
                           case FImage.ImageFormat of
                           IF_RGB24 : begin
                                        rbPPMBinary.Enabled := True;
                                        rbPPMASCII.Enabled := True;
                                      end;
                           end;
                           CheckCompress(gbPPM);
                      end;
                    end;
         FF_TARGA : begin
                      pcCompress.ActivePage := tsTARGA;
                      FCompress := TmcmCompress(Reg.ReadInteger('TARGA', 'C', integer(CP_NONE)));
                      // FQuality  := Reg.ReadInteger('TARGA', 'Q', 100);
                      if Assigned(FImage)
                      then begin
                           case FImage.ImageFormat of
                           IF_GREY8,
                           IF_PAL8,
                           IF_RGB15,
                           IF_RGB16,
                           IF_RGB24,
                           IF_RGBA32 : begin
                                         rbTGACompress.Enabled   := True;
                                         rbTGAUncompress.Enabled := True;
                                       end;
                           end;
                           CheckCompress(gbTARGA);
                      end;
                    end;
         end;
       except
         on E:Exception
         do ShowMessage(E.Message);
       end;
       Reg.CloseKey;
  end;
  Reg.Free;

  Result := True;
end; // TFormFileCompress.SetImageAndFormat.


procedure TFormFileCompress.cbPNGQualityChange(Sender : TObject);
begin
  case cbPNGQuality.ItemIndex of
  0 : FQuality := 25;
  1 : FQuality := 50;
  2 : FQuality := 75;
  3 : FQuality := 100;
  end;
end; // TFormFileCompress.cbPNGQualityChange.

end.
