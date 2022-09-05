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
// $Log:  17507: DefIcon.pas 
//
//    Rev 1.1    2014-02-02 21:09:52  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.0    27-05-2002 16:21:46  mcm

unit DefIcon;

interface

{$Include 'mcmDefines.pas'}
                      
uses {$IFNDEF GE_DXE2}
      Windows;
     {$ELSE}
      WinApi.Windows;
     {$ENDIF}

type
  TIconDir =  packed record
    idReserved : word; // Reserved (must be 0)
    idType     : word; // Resource Type (1 for icons)
    idCount    : word; // How many images?
    // IconDir    : idEntries[1]; // An entry for each image (idCount of 'em)
  end;

  TIconDirEntry =  packed record
    bWidth        : byte;  // Width, in pixels, of the image
    bHeight       : byte;  // Height, in pixels, of the image
    bColorCount   : byte;  // Number of colors in image (0 if >=8bpp)
    bReserved     : byte;  // Reserved ( must be 0)
    wPlanes       : word;  // Color Planes
    wBitCount     : word;  // Bits per pixel
    dwBytesInRes  : dword; // How many bytes in this resource?
    dwImageOffset : dword; // Where in the file is this image?
  end;
  PIconDirEntry = ^TIconDirEntry;
{
  TIconImage =  packedrecord
    BITMAPINFOHEADER   icHeader;      // DIB header
    RGBQUAD         icColors[1];   // Color table
    BYTE            icXOR[1];      // DIB bits for XOR mask
    BYTE            icAND[1];      // DIB bits for AND mask
  end;
}

implementation

end.
