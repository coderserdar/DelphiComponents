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
// $Log:  17515: DefPortablemap.pas 
//
//   Rev 1.0    27-05-2002 16:21:50  mcm

unit PortablemapDef;

interface

type
  TPortableType = (PPGM_NONE, PPM_ASCII, PPM_BIN, PGM_ASCII, PGM_BIN);

  TPortableGreyHeader = packed record
                          Width        : cardinal;
                          Height       : cardinal;
                          MaxGreyValue : word;
  end;

  TPortablePixelHeader = packed record
                          Width        : cardinal;
                          Height       : cardinal;
                          MaxGreyValue : word;
  end;


implementation

end.
