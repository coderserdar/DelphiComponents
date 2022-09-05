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
// $Log:  17511: DefPCX.pas 
//
//   Rev 1.0    27-05-2002 16:21:48  mcm

unit DefPCX;

interface

type
  TWordRect  = Packed Record
               Xmin, Ymin, Xmax, Ymax : word;
               end;

  TPCXColorMap = array[0..47] of byte;

  TPCXHeader = Packed Record
                 Manufacturer : byte;         // Constant Flag, 10 = ZSoft .pcx
                 Version      : byte;         // Version information
                                              // 0 = Version 2.5 of PC Paintbrush
                                              // 2 = Version 2.8 w/palette information
                                              // 3 = Version 2.8 w/o palette information
                                              // 4 = PC Paintbrush for Windows(Plus for
                                              //     Windows uses Ver 5)
                                              // 5 = Version 3.0 and > of PC Paintbrush
                                              //     and PC Paintbrush +, includes
                                              //     Publisher's Paintbrush . Includes
                                              //     24-bit .PCX files
                 Encoding     : byte;         // 1 = .PCX run length encoding
                 BitsPerPixel : byte;         // Number of bits to represent a pixel
                                              // (per Plane) - 1, 2, 4, or 8
                 Window       : TWordRect;    // Image Dimensions: Xmin,Ymin,Xmax,Ymax
                 HDpi         : smallint;     // Horizontal Resolution of image in DPI*
                 VDpi         : smallint;     // Vertical Resolution of image in DPI*
                 Colormap     : TPCXColorMap; //  48     Color palette setting, see text
                 Reserved     : byte;         // Should be set to 0.
                 NPlanes      : byte;         // Number of color planes
                 BytesPerLine : smallint;     // Number of bytes to allocate for a scanline
                                              // plane.  MUST be an EVEN number.  Do NOT
                                              // calculate from Xmax-Xmin.
                 PaletteInfo  : smallint;     // How to interpret palette-
                                              //   1 = Color/BW,
                                              //   2 = Grayscale (ignored in PB IV/ IV +)
                 HscreenSize  : smallint;     // Horizontal screen size in pixels. New field
                                              // found only in PB IV/IV Plus
                 VscreenSize  : smallint;     // Vertical screen size in pixels. New field
                                              // found only in PB IV/IV Plus
                 Filler       : array[0..53] of byte;
                                              // Blank to fill out 128 byte header.
                                              // Set all bytes to 0
               end;

implementation

end.
