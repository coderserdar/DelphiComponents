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
// $Log:  17517: DefTarga.pas 
//
//    Rev 1.1    26-08-2009 22:49:16  mcm
// Fixed unicode issues (PChar -> PAnsiChar)
//
//   Rev 1.0    27-05-2002 16:21:50  mcm

unit DefTarga;

interface

uses mcmImageTypeDef;

const
  // Image type.
  TARGA_EMPTY         = 0;  // No image data
  TARGA_INDEXED       = 1;  // Palette indexed
  TARGA_TRUECOLOR     = 2;  // True colour
  TARGA_BW            = 3;  // Black & white (Grey)
  TARGA_INDEXED_RLE   = 9;  // Palette indexed
  TARGA_TRUECOLOR_RLE = 10; //
  TARGA_BW_RLE        = 11; //

  // Colour map type
  TARGA_NOCOLORMAP    = 0;
  TARGA_COLORMAP      = 1;

  TARGA2_SIGNATURE    = 'TRUEVISION-XFILE.';

type
  //----------------------------------------------------------------------------
  // TGA File Header
  TTargaHeader = packed record
    IDLength          : byte; // Number of characters in ID Field.
    ColorMapType      : byte; // Colour map type.
    ImageType         : byte; // Image code type.
                              // 0  - No image data included.
                              // 1  - Uncompressed colour mapped image.
                              // 2  - Uncompressed true colour image.
                              // 3  - Uncompressed black & white image (greyscale).
                              // 9  - Run-length colour mapped image.
                              // 10 - Run-length true colour image.
                              // 11 - Run-length black & white image.

    ColorMapStart     : word; // Colour map first entry in palette.
    ColorMapLength    : word; // Colour map length.
    ColorMapEntrySize : byte; // Colour map entry size.
    XOrigin           : word; // X origin
    YOrigin           : word; // Y origin
    Width             : word; // Width of image
    Height            : word; // Height of image.
    PixelDepth        : byte; // Pixel depth!
    ImageDescriptor   : byte; // Image descriptor
                              // Bits
                              // 0..3 - Attribute bits per pixel
                              // 4..5 - Image orientation
                              //        0 : Bottom left
                              //        1 : Bottom right
                              //        2 : Top left
                              //        3 : Top right
                              // 6..7 - Interleaved flag (Must be ZERO)
                              //        0 : Two way (even-odd) interleave
                              //            (e.g. IBM Graphics Card Adapter), obsolete
                              //        1 : Four way interleave (e.g. AT&T 6300
                              //            High Resolution), obsolete

  end;

  //----------------------------------------------------------------------------
  // ID Field

  // A string, variable length, max 255 chars.

  //----------------------------------------------------------------------------
  // Image / Color Map Data

  // A palette, variable length

  //----------------------------------------------------------------------------
  // Developers Area
  // Not supported!
  TTargaDeveloper = packed record
    NoTags        : word;
  // array of Tags.
  end;

  //----------------------------------------------------------------------------
  // Extension Area
  TTargaSoftwareVer = packed record
    VersionNo       : word;
    VersionLetter   : AnsiChar;
  end;

  TTargaExtension   = packed record
    ExtensionSize   : word;
    AuthorName      : array[0..40] of AnsiChar;
    AuthorComments  : array[0..323] of AnsiChar;
    DateTimeStamp   : array[0..5] of word;
    JobNameID       : array[0..40] of AnsiChar;
    JobTime         : array[0..2] of word;
    SoftwareID      : array[0..40] of AnsiChar;
    SoftwareVersion : TTargaSoftwareVer;
    KeyColor        : cardinal;
    AspectRatioW    : word;
    AspectRatioH    : word;
    GammaNum        : word;
    GammaDeNom      : word;
    ColorCorrectOfs : cardinal;
    PostStampOffset : cardinal;
    ScanLineOffset  : cardinal ;
    AttributeType   : byte;
  end;

  //----------------------------------------------------------------------------
  // Scan Line Table

  //----------------------------------------------------------------------------
  // Post Stamp Image

  //----------------------------------------------------------------------------
  // Color Correction Table

  //----------------------------------------------------------------------------
  // TGA File Footer
  // In Targa version 2 a footer is included.
  // Laste 26 bytes of Targa file (Optional).
  TTargaFooter = packed record
  ExtensionOffset : cardinal; // Offset from start of file to Extension Area
  DeveloperOffset : cardinal; // Offset from start of file to Developers area.
  Signature       : array[0..17] of AnsiChar; // Must be "TRUEVISION-XFILE." + /0.
  end;




implementation

end.
