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
// $Log:  17822: DefGIF.pas 
//
//   Rev 1.0    07-08-2002 11:40:48  mcm    Version: IMG 1.1

unit DefGIF;

interface

type
 TGIFHeader = Packed Record
 ScreenWidth    : word; // Screen width.
 ScreenHeight   : word; // Screen height.
 BitFields      : byte; // bit 0..2 -> Color table size 2^(n+1).
                        // bit 3    -> Color table sort flag, set when colors
                        //             are sorted in order of importance.
                        // bit 4..6 -> Bits per pixel - 1.
                        // bit 7    -> Set when a global color table is present.
 BKColor        : byte; // Index into global color table.
 AspectRatio    : byte; //
 end;

 TGIFImageHeader = Packed Record
 LeftPos     : word; // Left offset into global image.
 TopPos      : word; // Top offset into global image.
 ImageWidth  : word; // Sub-image width.
 ImageHeight : word; // Sub-image height.
 BitField    : byte; // bit 0..2 -> 2^(n+1) number of colors in local table.
                     // bit 3..4 -> Not used.
                     // bit 5    -> Color table sort flag, set when colors
                     //             are sorted in order of importance.
                     // bit 6    -> Data are stored interlaced.
                     // bit 7    -> Image uses local palette.
 end;

 TGIFInterRow = Packed Record
                Row      : integer;
                Interval : integer;
                end;

const
  GIFInterlaced : array[1..5] of TGIFInterRow = ((Row : 0; Interval : 8),
                                                 (Row : 4; Interval : 8),
                                                 (Row : 2; Interval : 4),
                                                 (Row : 1; Interval : 2),
                                                 (Row : -1; Interval : 0));

  // Block codes.
  GIF_EXTENSION  = $21;
  GIF_IMAGEBLOCK = $2C;
  GIF_TERMINATOR = $3B; // GIF Trailer, end of GIF file.

  // Extension types
  GIF_EXT_TEXT     = $01;
  GIF_EXT_GRAPHICS = $F9;
  GIF_EXT_COMMENT  = $FE;
  GIF_EXT_APP      = $FF;

implementation

end.
