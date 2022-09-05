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
// $Log:  17969: DefSGI.pas 
//
//    Rev 1.1    19-01-2009 22:34:00  mcm    Version: IMG 3.3
// Added support for Delphi 2009
//
//   Rev 1.0    09-09-2002 13:05:22  mcm    Version: IMG 1.2
// Header definition for SGI image file.

unit DefSGI;

interface

const
  SGI_SIGNATURE    : word = $01DA;

type
  TSGIHeader = packed record
    // Magic Number : word.
    Compress       : byte;                  // 0 -> Uncompressed
                                            // 1 -> Compressed
    BytePerChannel : byte;                  // 1 or 2 (bytes per pixel channel)
                                            // is allowed.
    Dimension      : word;                  // 1 -> 1 color channel and 1 row,
                                            //      width ig provided in "Width".
                                            // 2 -> 1 color channel, where width
                                            //      and height is provided in
                                            //      "Width" and "Height".
                                            // 3 -> width
                                            //      and height is provided in
                                            //      "Width" and "Height" and
                                            //      number of channels are given
                                            //      by "Planes".
    Width          : word;                  // Pixels per row.
    Height         : word;                  // Number of rows.
    Planes         : word;                  // Number of color channels.
    Minimum        : longint;               // Lowest pixel value.
    Maximum        : longint;               // Highest pixel value.
    NotUsed1       : longint;               // Ignored - should be zero.
    ImageName      : array[0..79] of AnsiChar;  // Image name, nul terminating ASCII
                                            // string.
    ColorMap       : longint;               // 0 -> Normal.
                                            // 1 -> Dithered, 3 bits are used
                                            //      for red [0..2] and green [3..5]
                                            //      and 2 bits are used for blue
                                            //      [6..7]. Obsolete mode!
                                            // 2 -> Screen, Obsolete mode!
                                            // 3 -> Colormap,
    NotUsed2       : array[0..403] of byte; // Ignored - should be zero.
  end;

implementation

end.
