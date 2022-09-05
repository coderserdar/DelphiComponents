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
// $Log:  29811: mcmWMFFile.pas 
//
//    Rev 1.1    2014-02-02 21:10:08  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.0    31-10-2007 20:17:38  mcm
unit mcmWMFFile;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, SysUtils, Classes, Graphics,
     {$ELSE}
      WinApi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, 
     {$ENDIF}
     mcmImage,
     mcmImageFile,
     mcmImageTypeDef,
     mcmImageResStr;

type
  // Defining an External image file class.
  TImageWMF = class(TmcmImageFile)
  protected
    FPicture : TPicture;
  public
    class function GetColorFormats : TmcmImageFormats; override;
    class function GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer; override;
    class function GetCompressionName(Compression : TmcmCompress) : string; override;
    constructor Create; override;
    destructor  Destroy; override;
    function    IsFormatValid(Stream : TStream) : TmcmErrorCode; override;
    procedure   LoadFromStream(Stream : TStream); override;
    procedure   SaveToStream(Stream : TStream); override;
  end;

  const
  // File format identifier
  FF_MyWMF      = FF_USERDEFINED + 1;
  // Compression identifier
  CP_MyCompress = CP_USERDEFINED + 1;

ResourceString
  // File format description.
  resMyWMF         = 'My WMF implementation';
  // Compression methods description.
  resCP_MyCompress = 'My WMF Compression';


implementation

//------------------------------------------------------------------------------
// An external image file class.
//------------------------------------------------------------------------------

class function TImageWMF.GetColorFormats : TmcmImageFormats;
begin
  // As GetColorFormats is a "class method" an instance of TImageWMF is not
  // necessaryly created when this method is called. Therefore, non of its
  // member functions nor variables may be called/used.

  // Returns the color formats/resolutions that the file format supports.
  Result := [IF_BW,IF_GREY4,IF_PAL4,IF_GREY8,IF_PAL8,IF_RGB15,IF_RGB16,IF_RGB24,IF_RGBA32];
end; // TImageWMF.GetColorFormats.


class function TImageWMF.GetCompressionFromColor(ImageFormat : TmcmImageFormat; var Compress : array of TmcmCompress) : integer;
var Count : integer;
begin
  // This function builds an array of compression methods supported by the file
  // format.

  // If the file format offers compression methods, then implement this class
  // function. Otherwise simply omit adding this function in your file class.

  // Call inherited to add "No compression" - Omit calling the inherited method
  // if "no compresson" isn't an option in the file format, and set "Count := 0;".
  Count := Inherited GetCompressionFromColor(ImageFormat, Compress);

  // Now add the compressions that apply to the file format. If the compression
  // methods vary depending on the Color resolution of the image use ImageFormat
  // as below.
  // Note: The IF_NONE is used when the TmcmSaveDialog.EnableAllFormats is set
  // to True. In this case the TmcmSaveDialog doesn't check the color resolution
  // against what a file format supports. It is expected that the image is
  // modified to match a resolution which the file formats supports after
  // TmcmSaveDialog.Execute has returned True.

  if (ImageFormat in [IF_NONE,IF_RGB24])
  then begin
       // In this case "My Compression" is only supported for 24 bit color images.
       if (High(Compress) > Count)
       then Compress[Count] := CP_MyCompress;
       inc(Count);
  end;
  Result := Count;
end; // TImageWMF.GetCompressionFromColor.


class function TImageWMF.GetCompressionName(Compression : TmcmCompress) : string;
begin
  // This function returns the name of the Compression method.
  // If the file format offers compression methods, then implement this class
  // function. Otherwise simply omit implementing this function.

  case Compression of
  CP_NOCOMP     : Result := resCP_NOCOMP;
  CP_MyCompress : Result := resCP_MyCompress;
  end;
end; // function TImageWMF.GetCompressionName.


constructor TImageWMF.Create;
begin
  Inherited Create;
  // Perform necessary initialization.
  //..
  FPicture := TPicture.Create;
end; // TImageWMF.Create.


destructor TImageWMF.Destroy;
begin
  // Clear-up, free allocated memory.
  //..
  FPicture.Free;
  Inherited Destroy;
end; // TImageWMF.Destroy.


function TImageWMF.IsFormatValid(Stream : TStream) : TmcmErrorCode;
begin
  // Call inherited - checks if stream is valid and sets the Error poperty
  // to EC_BADFORMAT.
  FError := Inherited IsFormatValid(Stream);
  if Assigned(Stream)
  then begin
       try
         // Read "magic word" that identifies the correctness of the file format.

         FError := EC_OK;
       except
         On EReadError
         do FError := EC_READFROMFILE;
         On Exception
         do FError := EC_UNKNOWN;
       end;
  end
  else FError := EC_FILENOTOPEN;
  Result := FError;
end; // TImageWMF.IsFormatValid.


procedure TImageWMF.LoadFromStream(Stream : TStream);
var Bitmap : TBitmap;
begin
  try
    // Read meta file data.
    FPicture.Metafile.LoadFromStream(Stream);
    if (FPicture.Metafile.Handle <> 0)
    then begin
         // Convert the meta file to a bitmap. This allows the TmcmOpenDialog
         // and TmcmThumbView to display a thumb image of the WMF file.
         Bitmap := TBitmap.Create;
         try
           Bitmap.Width := FPicture.Width;
           Bitmap.Height := FPicture.Height;
           Bitmap.PixelFormat := pf24bit;
           Bitmap.Canvas.Draw(0, 0, FPicture.Metafile);

           // FDibHandle is the Image handle to receive the bitmap handle.
           // We call Bitmap.ReleaseHandle to release the bitmap handle from
           // Bitmap. The handle will be disposed of by the receiving control.
           FDibHandle := Bitmap.ReleaseHandle;
         finally
           Bitmap.Free;
         end;

         // Get additional information from the meta file.
         ImageInfo.Artist := FPicture.Metafile.CreatedBy;
         ImageInfo.Description := FPicture.Metafile.Description;
         ImageInfo.Units := UN_INCHS;
         ImageInfo.XResolution := FPicture.Metafile.Inch;
         ImageInfo.YResolution := FPicture.Metafile.Inch;
    end;
    FError := EC_OK;
  except
    On E:Exception
    do FError := EC_READFROMFILE;
  end;
end; // TImageWMF.LoadFromStream.


procedure TImageWMF.SaveToStream(Stream : TStream);
begin
  // Could be used to save the local member variable FDibHandle (device
  // independent bitmap handle) to the stream.
  FError := EC_OK;
end; // TImageWMF.SaveToStream.


end.
