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
// $Log:  22851: DefExif.pas 
//
//   Rev 1.0    21-02-2004 01:06:36  mcm    Version: IMG 2.4
// Initial revision

unit DefExif;

interface

const
  //----------------------------------------------------------------------------
  // Private TAG's not registered with TIFF.
  // Registered Exif tag's are defined in deftiff.pas.

  EXIF_GPSVersionID        = 0;  // 0 BYTE 4
  EXIF_GPSLatitudeRef      = 1;  // 1 ASCII 2
  EXIF_GPSLatitude         = 2;  // 2 RATIONAL 3
  EXIF_GPSLongitudeRef     = 3;  // 3 ASCII 2
  EXIF_GPSLongitude        = 4;  // 4 RATIONAL 3
  EXIF_GPSAltitudeRef      = 5;  // 5 BYTE 1
  EXIF_GPSAltitude         = 6;  // 6 RATIONAL 1
  EXIF_GPSTimeStamp        = 7;  // 7 RATIONAL 3
  EXIF_GPSSatellites       = 8;  // 8 ASCII Any
  EXIF_GPSStatus           = 9;  // 9 ASCII 2
  EXIF_GPSMeasureMode      = 10; // A ASCII 2
  EXIF_GPSDOP              = 11; // B RATIONAL 1
  EXIF_GPSSpeedRef         = 12; // C ASCII 2
  EXIF_GPSSpeed            = 13; // D RATIONAL 1
  EXIF_GPSTrackRef         = 14; // E ASCII 2
  EXIF_GPSTrack            = 15; // F RATIONAL 1
  EXIF_GPSImgDirectionRef  = 16; // 10 ASCII 2
  EXIF_GPSImgDirection     = 17; // 11 RATIONAL 1
  EXIF_GPSMapDatum         = 18; // 12 ASCII Any
  EXIF_GPSDestLatitudeRef  = 19; // 13 ASCII 2
  EXIF_GPSDestLatitude     = 20; // 14 RATIONAL 3
  EXIF_GPSDestLongitudeRef = 21; // 15 ASCII 2
  EXIF_GPSDestLongitude    = 22; // 16 RATIONAL 3
  EXIF_GPSDestBearingRef   = 23; // 17 ASCII 2
  EXIF_GPSDestBearing      = 24; // 18 RATIONAL 1
  EXIF_GPSDestDistanceRef  = 25; // 19 ASCII 2
  EXIF_GPSDestDistance     = 26; // 1A RATIONAL 1
  EXIF_GPSProcessingMethod = 27; // 1B UNDEFINED Any
  EXIF_GPSAreaInformation  = 28; // 1C UNDEFINED Any
  EXIF_GPSDateStamp        = 29; // 1D ASCII 11
  EXIF_GPSDifferential     = 30; // 1E SHORT 1


implementation

end.
 
