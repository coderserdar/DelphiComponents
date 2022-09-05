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
// $Log:  17553: mcmImageResStr.pas 
//
//    Rev 1.24    01-01-2009 13:45:48  mcm
// Initial support for JBIG.
//
//    Rev 1.23    05-03-2006 10:51:02  mcm    Version: IMG 2.16
// Added OCR Color error.
//
//    Rev 1.22    09/10/2005 08:56:08  mcm
// Changed JPEG compression strings to include JPEG.
//
//   Rev 1.21    31-07-2005 21:02:20  mcm
// Added File move error message.

//
//   Rev 1.20    24-07-2005 18:49:02  mcm    Version: IMG 2.9
// Added error message for TmcmHandleStream.

//
//   Rev 1.19    23-05-2005 21:41:24  mcm    Version: IMG 2.9
// New error definition for Marr-Hilderth.

//
//   Rev 1.18    03-02-2005 21:16:10  mcm
// Added DeskewMinMax potential error definition.

//
//   Rev 1.17    20-01-2005 22:24:08  mcm
// Added new BADPARAMETER error code.

//
//   Rev 1.16    26-09-2004 11:13:28  mcm    Version: IMG 2.6
// Renamed compression const.

//
//   Rev 1.15    26-08-2004 23:52:56  mcm    Version: IMG 2.6
// Added display error used by TmcmImage.Draw.

//
//   Rev 1.14    25-06-2004 20:48:44  mcm    Version: IMG 2.5
// Added new resource strings for JPEG2000 and file messages.

//
//   Rev 1.13    01-06-2004 00:04:08  mcm    Version: IMG 2.5
// Added resource strings for GIF.

//
//   Rev 1.12    14-03-2004 09:06:00  mcm    Version: IMG 2.4
// Added definitions/string for NEF file format.

//
//   Rev 1.11    21-02-2004 01:12:32  mcm    Version: IMG 2.4
// Added metadata (Exif & GPS) strings.

//
//   Rev 1.10    30-01-2004 20:29:20  mcm    Version: IMG 2.3
// Added Stream and TmcmProfile error messages.

//
//   Rev 1.9    16-10-2003 11:31:30  mcm    Version: IMG 1.6
// Resource strings for OCR

//
//   Rev 1.8    29-09-2003 18:45:24  mcm    Version: IMG 1.6
// Changed JPEG resource string.

//
//   Rev 1.7    25-09-2003 23:34:50  mcm    Version: IMG 1.5
// Added interpolation error.

//
//   Rev 1.6    25-07-2003 00:51:50  mcm
// Added Difference Predictor error for un-supported bit depth.

//
//   Rev 1.5    12-05-2003 15:31:52  mcm    Version: IMG 1.3.4
// Added error message for missing result image.

//
//   Rev 1.4    25-03-2003 21:58:56  mcm    Version: IMG 1.3.3
// Added error string for Multiple Image/page files.
// Added bit resolution dependend strings for RLE.

//
//   Rev 1.3    29-01-2003 15:44:02  mcm
// New error messages for JPEG.

//
//   Rev 1.2    27-09-2002 13:05:40  mcm    Version: IMG 1.2
// New error messages and SGI support.

//
//   Rev 1.1    07-08-2002 14:13:12  mcm    Version: IMG 1.1
// Added new resource strings for error, file and compression messages.

//
//   Rev 1.0    27-05-2002 16:22:06  mcm

unit mcmImageResStr;

interface         

Resourcestring
  // Error messages.
  resEC_OK                = 'No error.';
  resEC_WINDOWS           = 'A Windows function didn''t return an expected value.';
  resEC_NOMEMORY          = 'Failed to allocate memory.';

  // TmcmImageFile errors.
  resEC_NOSTREAM          = 'File stream is not initialised.';
  resEC_OPENFILE          = 'Cannot open image file.';
  resEC_MOVEFILE          = 'Cannot move file %s.';
  resEC_CREATEFILE        = 'Cannot create file.';
  resEC_FILENOTOPEN       = 'File was not open for read or write.';
  resEC_READFROMFILE      = 'Error reading data from file.';
  resEC_WRITETOFILE       = 'Error writing data to file.';
  resEC_BADFORMAT         = 'Un-supported or incorrect file format.';
  resEC_UNKNOWNFORMAT     = 'Format not supported.';
  resEC_ENDOFFILE         = 'Unexpected end of file.';
  resEC_DISKSPACE         = 'Insufficient disk space.';
  resEC_COMPRESSION       = 'Un-supported compression scheme.';
  resEC_DECOMPRESSION     = 'Un-supported decompression scheme,' + chr($0D) +
                            'or incorrect compressed data.';
  resEC_BADCOMPRESS       = 'Compression error.';
  resEC_BADDECOMPRESS     = 'Decompression error.';
  resEC_ENDOFDATA         = 'Unexpected end of data.';
  resEC_COMPRESSCOLOR     = 'Color format is not supported by compression scheme.';
  resEC_DATAORDER         = 'Bit order is not supported.';
  resEC_BITDEPTH          = 'Un-supported bit depth.';
  resEC_TIFFTAG           = 'Un-supported or Unknown TIFF Tag.';
  resEC_BADIMAGEHANDLE    = 'Invalid image handle.';
  resEC_CRC               = 'CRC did not match, data may be incorrect.';
  resEC_NOHUFFMANCODES    = 'Huffman codes are not defined.';
  resEC_MAXHUFFMANCODES   = 'Number of huffman codes exceed allowed size.';
  resEC_SAMPLEFREQ        = 'JFIF, Horizontal or vertical sample frequency is zero.';
  resEC_HORIZFREQ         = 'JFIF, Fractional horizontal frequency.';
  resEC_VERTFREQ          = 'JFIF, Fractional vertical frequency.';
  resEC_SUCCAPPROX        = 'JFIF, Successive approximation extends range.';
  resEC_MAXHUFFMANTABLES  = 'JFIF, Too many huffman tables.';
  resEC_JPGNOPROGRESSIVE  = 'JPEG, Progressive JPEG is not supported.';
  resEC_NOLOSSLESSHUFFMAN = 'JFIF, Lossless Huffman coding is not supported.';
  resEC_NODIFFSEQHUFFMAN  = 'JFIF, Differential sequential Huffman coding is not supported.';
  resEC_NODIFFPRGHUFFNAM  = 'JFIF, Differential progressive Huffman coding is not supported.';
  resEC_NODIFFLOSSHUFFMAN = 'JFIF, Differential lossless Huffman coding is not supported.';
  resEC_JFIFBADCOMPCOUNT  = 'JFIF, Incorrect number of components.';
  resEC_JPEGDHPCODE       = 'JFIF, Hierarchical progression (DHP marker) is not supported.';
  resEC_JFIFBADCOMPID     = 'JFIF, Invalid component identifier or identifier is out of order.';
  resEC_JPEGMISSOF        = 'JFIF, Start Of Scan reached before Start Of Field.';
  resEC_JPEGMISTABLE      = 'JFIF, One or more tables are missing (AC, DC or Quantification).';
  resEC_JPEGPROGACDC      = 'JFIF, Progressive scan contains both DC and AC data.';
  resEC_DUPCOMPSCAN       = 'Duplicate component scan.';

  // JBIG errors.
  resEC_JBIGMAXSIZE       = 'JBIG, Reached specified maximum size';
  resEC_JBIGABORTMARK     = 'JBIG, ABORT marker found';
  resEC_JBIGMARKSEGMENT   = 'JBIG, Unknown marker segment encountered';
  resEC_JBIGBIENOFIT      = 'JBIG, Incremental BIE does not fit to previous one';
  resEC_JBIGINVALID       = 'JBIG, Invalid data encountered';
  resEC_UNIMPLFEATURE     = 'JBIG, Unimplemented features used';

  resEC_PATENTS           = 'Arithmetic coding is covered by patents, and is therefore not supported.';
  resEC_PREDICTORBIT      = 'Difference Predictor does not support image bit depth.';

  // - Multi image errors
  resEC_NOSUBIMAGE        = 'Additional sub-images are not available.';
  resEC_APPENDIMAGE       = 'Could not append image to file. File format does not support multiple images.';

  // General TmcmImagexxx errors.
  resEC_MISSOURCEIMAGE    = 'A source image is not assigned.';
  resEC_MISRESULTIMAGE    = 'A result image is not assigned or could not be created.';
  resEC_BADTARGETSIZE     = 'Target image does not have correct spatial dimensions.';
  resEC_NOMATCHFORMAT     = 'Source images does not have matching formats (Color, Height or Width).';

  resEC_UNKNOWNMETHOD     = 'Method parameter is unknown.';
  resEC_DISPLAYIMAGE      = 'Error copying image lines to device.';


  // TmcmImageColor errors.
  resEC_BADCOLORFORMAT    = 'Image color format (bit depth) is not supported.';
  resEC_RANGEINDEX        = 'Index = %d is not within the valid range of %d..%d';
  resEC_RANGEVALUE        = 'The specified value = %d is not within the valid range of %d..%d';

  // TmcmImageFilter errors.

  // TmcmImageTransform errors.
  resEC_NOINTERPOLATE     = 'Unknown interpolation method.';
  resEC_DESKEWMAXMIN      = 'The deskew angle may be incorrect, as the angle found is the maximum allowed.';

  // TmcmImageFilter
  resEC_MARR_DEVIATION    = 'Marr-Hildreth, the specified "standard deviation" resulted in a filter that is too large.';

  // TmcmImageOCR errors.
  resEC_OCRBADCOLORFORMAT = 'Color format or bit depht is not supported. TmcmOCR supports 8 bit Grey scale images!';

  // TmcmProfile errors.
  resEC_INVALIDSCANPARAM  = 'Invalid scan parameter (Scan length <= Average).';
  resEC_CHECKXY           = 'X or Y coordinate is not within the valid range.';

  // TmcmHandleStream errors.
  resEC_HANDLESTREAMERROR = 'Out of memory while expanding memory stream.';

  // General.
  resEC_DEMOMODE          = 'Facility is not available in this demo edition.';
  resEC_USERCANCEL        = 'User canceled operation';
  resEC_BADPARAMETER      = 'Bad parameters';

  resEC_UNKNOWN           = 'Error is unknown (Exception).';


  //
  resYes                  = 'Yes';
  resNo                   = 'No';
  resColor                = 'Color';
  resColor32K             = '32 Kilo';
  resColor64K             = '64 Kilo';
  resColor16M             = '16 Million';

  resResolution           = 'Resolution';
  resDimension            = 'Dimension';

  resFileName             = 'File name';
  resFileSize             = 'File size';
  resFileFormat           = 'File format';
  resDateTime             = 'Date/Time';
  resCompression          = 'Compression';
  resMetadata             = 'Metadata';

  // Files sizes
  resGigaBytes            = 'GB';
  resMegaBytes            = 'MB';
  resKiloBytes            = 'KB';
  resBytes                = 'bytes';

  // File resource strings.
  resGIF                  = 'CompuServe Graphics Interchange';
  resCGM                  = 'Computer Graphics Metafile';
  resCRI                  = 'CRI';
  resDICOM                = 'Digital Imaging and Communications in Medicine';
  resCUT                  = 'Dr. Halo';
  resFPX                  = 'FlashPix';
  resIMG                  = 'GEM Paint';
  resJPEG                 = 'Joint Photographics Expert Group';
  resJBIG                 = 'Joint Bi-level Image experts Group';
  resJPEG2K               = 'JPEG 2000';
  resKDC                  = 'Kodak Digital Camera File';
  resPCD                  = 'Kodak Photo CD';
  resPICT                 = 'Machintosh PICT';
  resPSP                  = 'Paint Shop Pro';
  resPBM                  = 'Portable Bitmap';
  resPGM                  = 'Portable Greymap';
  resPNG                  = 'Portable Network Graphics';
  resPPM                  = 'Portable Pixelmap';
  resSGI                  = 'Silicon Graphics Image';
  resTIFF                 = 'Tag Image File Format';
  resTarga                = 'Truevision Targa';
  resWIcon                = 'Windows Icon';
  resEMF                  = 'Windows Enhanced Meta File';
  resWMF                  = 'Windows Meta File';
  resCRLE                 = 'CompuServe RLE';
  resWRLE                 = 'Windows RLE';
  resWBMP                 = 'Windows or OS/2 Bitmap';
  resWOS2DIB              = 'Windows or OS/2 DIB';
  resWPG                  = 'WordPerfect Bitmap';
  resPCX                  = 'Zsoft Paintbrush';
  resNEF                  = 'Nikon, Raw data format';
  {$IFDEF MCMFILEFORMAT}
    resMCF                = 'MCM Bitmap'; // Test image file format.
  {$ENDIF}
  resUnknown              = 'Unknown';
  resDetect               = 'All image file formats';

  resExif                 = 'Exchangeable Image File Format';
  resExifShort            = 'Exif';
  resGPS                  = 'GPS';

  // Compression resource strings.
  resCP_NOCOMP             = 'None';
  resCP_RLE                = 'Run length encoded';
  resCP_RLE4               = 'Run length, 4 bit';
  resCP_RLE8               = 'Run length, 8 bit';
  resCP_RLE8_24            = 'Run length, 8 & 24 bit';
  resCP_RLE8_32            = 'Run length, 8, 24 & 32 bit';
  resCP_RLE1_24            = 'Run length, 1, 4, 8 & 24 bit';
  resCP_ASCII              = 'ASCII';
  resCP_BIN                = 'Binary';
  resCP_HUFFMAN            = 'Huffman';
  resCP_PACKBITS           = 'Pack bits';
  resCP_MODIFIED_HUFFMAN   = 'Modified Huffman';
  resCP_MODIFIED_HUFFMAN_W = 'Modified Huffman, word aligned';
  resCP_CCITTGROUP3        = 'CCITT Group 3, one dimensional';
  resCP_CCITTGROUP3_2D     = 'CCITT Group 3, two dimensional';
  resCP_CCITTGROUP4        = 'CCITT Group 4';
  resCP_LZW                = 'LZW';
  resCP_THUNDERSCAN        = 'Thunderscan';
  resCP_JPEG_STD           = 'JPEG, Standard';
  resCP_JPEG_PROG          = 'JPEG, Progressive';
  resCP_JPEG_2000          = 'JPEG 2000';
  resCP_GIF87A             = 'GIF 87a';
  resCP_GIF89A             = 'GIF 89a';
  resCP_PNG                = 'Deflate (LZ77)';
  {$IFDEF MCMFILEFORMAT}
  resCP_LZM                = 'Deflate (LZ78)';
  {$ENDIF}

  // Huffman error resource strings.
  resHuffmannTooManyCodes   = 'Too many Huffman codes are defined';
  resHuffmannInvalidCode    = 'An invalid Huffman code was generated';
  resHuffmannMissingSize    = 'Missing Huffman code size';
  resHuffmannDuplicateValue = 'Duplicated Huffman value';
  resHuffmannMissingValue   = 'Missing Huffman value';
  resHuffmannInvalidRange   = 'Invalid Huffman range';
  resHuffmannDuplicateCode  = 'Duplicated Huffman code';

  // OCR resource strings.
  resOCRDUPLICATE           = 'The glyph is identical to a template already in the OCR dictionary.';
  resOCRDUPLICATE2          = 'Do you wish to replace the template?';

  // Open & Save dialogues.
  resOpen                   = 'Open';
  resSave                   = 'Save';
  resFailedToOpen           = 'Failed to rename: ';
  resErrorSettingPath       = 'Error setting path: "%s"';
  resName                   = 'Name';
  resSize                   = 'Size';
  resType                   = 'Type';
  resModified               = 'Modified';
  resAttributes             = 'Attributes';
  resNewFolder              = 'New Folder';
  resEnterNewFolder         = 'Enter new floder name';
  resFolder                 = 'Folder';
  resAlreadyExist           = 'already exists';
  resOverwriteFile1         = '%s already exists.';
  resOverwriteFile2         = 'Do you want to replace it?';
  resFileDoesNotExist       = '"%s" file does not exist.';
  resFileAlreadyExist       = '"%s" file already exist.';
  resFileCreatePrompt       = 'The file "%s" does not exist. Do you want to create it?';

implementation

end.
