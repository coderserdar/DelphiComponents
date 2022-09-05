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
// $Log:  15886: mcmTWAIN.pas 
//
//    Rev 1.29    2014-03-28 17:52:54  mcm    Version: DT 4.1
// Added TWAIN 2.x support, and thereby support for Windows 7 & 8
//
//    Rev 1.28    2014-02-02 16:46:48  mcm
// Fixed warning in XE2
//
//    Rev 1.27    2014-01-15 13:41:56  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.26    2013-12-04 23:16:12  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.25    01-03-2011 20:42:06  mcm    Version: DT 3.11
//
//    Rev 1.24    25-10-2009 16:44:28  mcm    Version: DT 3.10
// Support for Delphi 2010
//
//    Rev 1.23    26-08-2009 22:39:50  mcm    Version: DT 3.9
// Fixed unicode issues (PChar -> PAnsiChar)
//
//    Rev 1.22    08-01-2009 21:09:20  mcm
// Added support for Delphi 2009
//
//    Rev 1.21    28-12-2008 20:03:32  mcm    Version: DT 3.8
// Delphi 2009 support
//
//    Rev 1.20    11-08-2007 10:59:12  mcm    Version: DT 3.7
// Added support for Delphi 2007
//
//    Rev 1.19    22-12-2005 18:08:32  mcm    Version: DT 3.6
// Added support for Delphi 2006
//
//   Rev 1.18    11/06/2005 09:53:08  mcm

//
//   Rev 1.18    11-06-2005 09:53:08  mcm    Version: DT 3.5

//
//   Rev 1.17    19-02-2005 00:28:44  mcm    Version: DT 3.4
// Added support for configuration mode of data sources.

//
//   Rev 1.16    03-01-2005 18:36:00  mcm    Version: DT 3.3
// Added support for Delphi 2005.

//
//   Rev 1.15    28-10-2004 20:07:18  mcm    Version: DT 3.2

//
//   Rev 1.14    05-09-2004 19:54:36  mcm    Version: DT 3.2
// Minor change ensuring that string members on FAppIdentity are zero-terminated
// correctly.

//
//   Rev 1.13    04-03-2004 20:58:16  mcm    Version: DT3.0
// Changed the (F)OnNegotiation event to include the DoCancel argument, allowing
// the application to cancel an acquisition while negotiating capabilities.

//
//   Rev 1.12    20-01-2004 21:09:50  mcm    Version: DT3.0
// Added on line of code to disable Floating Point exceptions. Should solve
// problems with some data sources. 

//
//   Rev 1.11    12-11-2003 13:57:24  mcm    Version: DT3.0
// Added a fix for win 95 & 98. A lock-up situation occurs when Hints are or
// have been shown. Therefore, Hints are canceled and disabled while opening the
// TWAIN connection.
// Moved code in SelectDS to TmcmTWAINThread.

//
//   Rev 1.10    06-11-2003 09:49:26  mcm    Version: DT3.0
// Modified to threaded model, see TmcmTWAINThread and TmcmTWAINQueue.

//
//   Rev 1.9    31-07-2003 11:22:20  mcm
// Corrected AutoBright
// Modified FeederEnabled

//
//   Rev 1.8.1.0    08-07-2003 02:25:40  mcm
// Release edition.

//
//   Rev 1.8    06-07-2003 11:10:58  mcm    Version: DT 2.5
// Added property LenientOnCaps.

//
//   Rev 1.7    14-06-2003 11:07:40  mcm    Version: DT 2.4
// Added the property DeviceOnline that checkes if the open data source's device
// is online.
// Modified OpenSource to check if the device is online by calling DeviceOnline,
// and if not fire the DeviceNotOnline event.

//
//   Rev 1.6    17-05-2003 12:55:44  mcm
// Added support for SPIFF and EXIF.

//
//   Rev 1.5    15-04-2003 10:42:46  mcm    Version: DT 2.3
// Added new properties: AutoBrightness, ClearBatchBuffers, MaxBatchBuffers,
// NativeX/YResolution, and X/YScaling. 
// Modified Get/SetCapabilityMsg to check if a capability a CAP_xxx and ICAP_xxx
// is supported by the data source. All properties and methods on TmcmTWAIN uses
// these two methods. Checking with IsCapSupported as a seperate call is
// therefore no longer necessary.

//
//   Rev 1.4    21-01-2002 11:56:40  mcm    Version: DT 2.0
// Release Edition

//
//   Rev 1.3.1.2    06-03-2003 11:05:28  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.
// Fixed missing implementation of ImageHeight/Width.

//
//   Rev 1.3.1.1    07-10-2002 15:45:50  mcm    Version: DT2.1
// Added ResetImageLayout and LogFileName

//
//   Rev 1.3.1.0    27-09-2002 13:22:38  mcm
// Replaced lstrcpy with StrPLCopy.

//
//   Rev 1.3    21-01-2002 11:45:34  mcm    Version: DT 2.0
// Fixed/set DisableAfterAcquire default property setting to True.

//
//   Rev 1.2    18-01-2002 15:11:54  mcm    Version: DT 2.0
// Access to all property members are through function/procedures.

//
//   Rev 1.1    11-01-2002 15:28:48  mcm    Version: DT 2.0
// Replaced property FileFormats read method from direct access to FAllFormats to access via GetFileFormats, to resolve problem in C++Builder 3 and 4.
// Changed casing on TtwnXX variables to TTwnXX.

//
//   Rev 1.0    04-12-2001 16:49:06  mcm    Version: DT 2.0

unit mcmTWAIN;

{$INCLUDE mcmDefines.pas}

{$DEFINE SENDDIALOGUE}
{$DEFINE PREFDIALOGUE}
{$IFNDEF DCB3_4}
{ $DEFINE ICONSOURCEDIALOGUE}
{$ENDIF}

{$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows, WinApi.Messages, System.Classes, Vcl.Graphics,
     Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.SysUtils,
     {$IFDEF GE_DXE4}
     System.AnsiStrings,
     {$ENDIF}
     {$ELSE}
     {$IFDEF GE_DXE}
      Types,
     {$ENDIF}
     Windows, Messages, Classes, SysUtils, Graphics, Controls, Forms, Dialogs,
     {$ENDIF}
     twain,
     mcmTWAINIntf,
     mcmTWAINContainer;

const NoCountry    = 239;
      CmcmCountry  : array[0..NoCountry-1] of TW_UINT16
                   = (TWCY_AFGHANISTAN,
                      TWCY_ALGERIA,
                      TWCY_AMERICANSAMOA,
                      TWCY_ANDORRA,
                      TWCY_ANGOLA,
                      TWCY_ANGUILLA,
                      TWCY_ANTIGUA,
                      TWCY_ARGENTINA,
                      TWCY_ARUBA,
                      TWCY_ASCENSIONI,
                      TWCY_AUSTRALIA,
                      TWCY_AUSTRIA,
                      TWCY_BAHAMAS,
                      TWCY_BAHRAIN,
                      TWCY_BANGLADESH,
                      TWCY_BARBADOS,
                      TWCY_BELGIUM,
                      TWCY_BELIZE,
                      TWCY_BENIN,
                      TWCY_BERMUDA,
                      TWCY_BHUTAN,
                      TWCY_BOLIVIA,
                      TWCY_BOTSWANA,
                      TWCY_BRITAIN,
                      TWCY_BRITVIRGINIS,
                      TWCY_BRAZIL,
                      TWCY_BRUNEI,
                      TWCY_BULGARIA,
                      TWCY_BURKINAFASO,
                      TWCY_BURMA,
                      TWCY_BURUNDI,
                      TWCY_CAMAROON,
                      TWCY_CANADA,
                      TWCY_CAPEVERDEIS,
                      TWCY_CAYMANIS,
                      TWCY_CENTRALAFREP,
                      TWCY_CHAD,
                      TWCY_CHILE,
                      TWCY_CHINA,
                      TWCY_CHRISTMASIS,
                      TWCY_COCOSIS,
                      TWCY_COLOMBIA,
                      TWCY_COMOROS,
                      TWCY_CONGO,
                      TWCY_COOKIS,
                      TWCY_COSTARICA,
                      TWCY_CUBA,
                      TWCY_CYPRUS,
                      TWCY_CZECHOSLOVAKIA,
                      TWCY_DENMARK,
                      TWCY_DJIBOUTI,
                      TWCY_DOMINICA,
                      TWCY_DOMINCANREP,
                      TWCY_EASTERIS,
                      TWCY_ECUADOR,
                      TWCY_EGYPT,
                      TWCY_ELSALVADOR,
                      TWCY_EQGUINEA,
                      TWCY_ETHIOPIA,
                      TWCY_FALKLANDIS,
                      TWCY_FAEROEIS,
                      TWCY_FIJIISLANDS,
                      TWCY_FINLAND,
                      TWCY_FRANCE,
                      TWCY_FRANTILLES,
                      TWCY_FRGUIANA,
                      TWCY_FRPOLYNEISA,
                      TWCY_FUTANAIS,
                      TWCY_GABON,
                      TWCY_GAMBIA,
                      TWCY_GERMANY,
                      TWCY_GHANA,
                      TWCY_GIBRALTER,
                      TWCY_GREECE,
                      TWCY_GREENLAND,
                      TWCY_GRENADA,
                      TWCY_GRENEDINES,
                      TWCY_GUADELOUPE,
                      TWCY_GUAM,
                      TWCY_GUANTANAMOBAY,
                      TWCY_GUATEMALA,
                      TWCY_GUINEA,
                      TWCY_GUINEABISSAU,
                      TWCY_GUYANA,
                      TWCY_HAITI,
                      TWCY_HONDURAS,
                      TWCY_HONGKONG,
                      TWCY_HUNGARY,
                      TWCY_ICELAND,
                      TWCY_INDIA,
                      TWCY_INDONESIA,
                      TWCY_IRAN,
                      TWCY_IRAQ,
                      TWCY_IRELAND,
                      TWCY_ISRAEL,
                      TWCY_ITALY,
                      TWCY_IVORY_COAST,
                      TWCY_JAMAICA,
                      TWCY_JAPAN,
                      TWCY_JORDAN,
                      TWCY_KENYA,
                      TWCY_KIRIBATI,
                      TWCY_KOREA,
                      TWCY_KUWAIT,
                      TWCY_LAOS,
                      TWCY_LEBANON,
                      TWCY_LIBERIA,
                      TWCY_LIBYA,
                      TWCY_LIECHTENSTEIN,
                      TWCY_LUXENBOURG,
                      TWCY_MACAO,
                      TWCY_MADAGASCAR,
                      TWCY_MALAWI,
                      TWCY_MALAYSIA,
                      TWCY_MALDIVES,
                      TWCY_MALI,
                      TWCY_MALTA,
                      TWCY_MARSHALLIS,
                      TWCY_MAURITANIA,
                      TWCY_MAURITIUS,
                      TWCY_MEXICO,
                      TWCY_MICRONESIA,
                      TWCY_MIQUELON,
                      TWCY_MONACO,
                      TWCY_MONGOLIA,
                      TWCY_MONTSERRAT,
                      TWCY_MOROCCO,
                      TWCY_MOZAMBIQUE,
                      TWCY_NAMIBIA,
                      TWCY_NAURU,
                      TWCY_NEPAL,
                      TWCY_NETHERLANDS,
                      TWCY_NETHANTILLES,
                      TWCY_NEVIS,
                      TWCY_NEWCALEDONIA,
                      TWCY_NEWZEALAND,
                      TWCY_NICARAGUA,
                      TWCY_NIGER,
                      TWCY_NIGERIA,
                      TWCY_NIUE,
                      TWCY_NORFOLKI,
                      TWCY_NORWAY,
                      TWCY_OMAN,
                      TWCY_PAKISTAN,
                      TWCY_PALAU,
                      TWCY_PANAMA,
                      TWCY_PARAGUAY,
                      TWCY_PERU,
                      TWCY_PHILLIPPINES,
                      TWCY_PITCAIRNIS,
                      TWCY_PNEWGUINEA,
                      TWCY_POLAND,
                      TWCY_PORTUGAL,
                      TWCY_QATAR,
                      TWCY_REUNIONI,
                      TWCY_ROMANIA,
                      TWCY_RWANDA,
                      TWCY_SAIPAN,
                      TWCY_SANMARINO,
                      TWCY_SAOTOME,
                      TWCY_SAUDIARABIA,
                      TWCY_SENEGAL,
                      TWCY_SEYCHELLESIS,
                      TWCY_SIERRALEONE,
                      TWCY_SINGAPORE,
                      TWCY_SOLOMONIS,
                      TWCY_SOMALI,
                      TWCY_SOUTH_AFRICA,
                      TWCY_SPAIN,
                      TWCY_SRILANKA,
                      TWCY_STHELENA,
                      TWCY_STKITTS,
                      TWCY_STLUCIA,
                      TWCY_STPIERRE,
                      TWCY_STVINCENT,
                      TWCY_SUDAN,
                      TWCY_SURINAME,
                      TWCY_SWAZILAND,
                      TWCY_SWEDEN,
                      TWCY_SWITZERLAND,
                      TWCY_SYRIA,
                      TWCY_TAIWAN,
                      TWCY_TANZANIA,
                      TWCY_THAILAND,
                      TWCY_TOBAGO,
                      TWCY_TOGO,
                      TWCY_TONGAIS,
                      TWCY_TRINIDAD,
                      TWCY_TUNISIA,
                      TWCY_TURKEY,
                      TWCY_TURKSCAICOS,
                      TWCY_TUVALU,
                      TWCY_UGANDA,
                      TWCY_USSR,
                      TWCY_UAEMIRATES,
                      TWCY_UNITEDKINGDOM,
                      TWCY_USA,
                      TWCY_URUGUAY,
                      TWCY_VANUATU,
                      TWCY_VATICANCITY,
                      TWCY_VENEZUELA,
                      TWCY_WAKE,
                      TWCY_WALLISIS,
                      TWCY_WESTERNSAHARA,
                      TWCY_WESTERNSAMOA,
                      TWCY_YEMEN,
                      TWCY_YUGOSLAVIA,
                      TWCY_ZAIRE,
                      TWCY_ZAMBIA,
                      TWCY_ZIMBABWE,
                      TWCY_ALBANIA, // This and below was added to TWAIN ver 1.8
                      TWCY_ARMENIA,
                      TWCY_AZERBAIJAN,
                      TWCY_BELARUS,
                      TWCY_BOSNIAHERZGO,
                      TWCY_CAMBODIA,
                      TWCY_CROATIA,
                      TWCY_CZECHREPUBLIC,
                      TWCY_DIEGOGARCIA,
                      TWCY_ERITREA,
                      TWCY_ESTONIA,
                      TWCY_GEORGIA,
                      TWCY_LATVIA,
                      TWCY_LESOTHO,
                      TWCY_LITHUANIA,
                      TWCY_MACEDONIA,
                      TWCY_MAYOTTEIS,
                      TWCY_MOLDOVA,
                      TWCY_MYANMAR,
                      TWCY_NORTHKOREA,
                      TWCY_PUERTORICO,
                      TWCY_RUSSIA,
                      TWCY_SERBIA,
                      TWCY_SLOVAKIA,
                      TWCY_SLOVENIA,
                      TWCY_SOUTHKOREA,
                      TWCY_UKRAINE,
                      TWCY_USVIRGINIS,
                      TWCY_VIETNAM);

type  TmcmLanguage = (USERLOCALE, // = -1, This was added to TWAIN ver 1.8
                      DANISH,
                      DUTCH,
                      ENGLISH_INTL,
                      FRENCH_CANADIAN,
                      FINNISH,
                      FRENCH,
                      GERMAN,
                      ICELANDIC,
                      ITALIAN,
                      NORWEGIAN,
                      PORTUGUESE,
                      SPANISH,
                      SWEDISH,
                      US_ENGLISH,
                      AFRIKAANS, // This and below was added to TWAIN ver 1.8
                      ALBANIA,
                      ARABIC,
                      ARABIC_ALGERIA,
                      ARABIC_BAHRAIN,
                      ARABIC_EGYPT,
                      ARABIC_IRAQ,
                      ARABIC_JORDAN,
                      ARABIC_KUWAIT,
                      ARABIC_LEBANON,
                      ARABIC_LIBYA,
                      ARABIC_MOROCCO,
                      ARABIC_OMAN,
                      ARABIC_QATAR,
                      ARABIC_SAUDIARABIA,
                      ARABIC_SYRIA,
                      ARABIC_TUNISIA,
                      ARABIC_UAE, // United Arabic Emirates
                      ARABIC_YEMEN,
                      BASQUE,
                      BYELORUSSIAN,
                      BULGARIAN,
                      CATALAN,
                      CHINESE,
                      CHINESE_HONGKONG,
                      CHINESE_PRC, // People's Republic of China
                      CHINESE_SINGAPORE,
                      CHINESE_SIMPLIFIED,
                      CHINESE_TAIWAN,
                      CHINESE_TRADITIONAL,
                      CROATIA,
                      CZECH,
                      // DANISH,            = TWLG_DAN;
                      // DUTCH,             = TWLG_DUT;
                      DUTCH_BELGIAN,
                      // ENGLISH,           = TWLG_ENG;
                      ENGLISH_AUSTRALIAN,
                      ENGLISH_CANADIAN,
                      ENGLISH_IRELAND,
                      ENGLISH_NEWZEALAND,
                      ENGLISH_SOUTHAFRICA,
                      ENGLISH_UK,
                      // ENGLISH_USA,       = TWLG_USA;
                      ESTONIAN,
                      FAEROESE,
                      FARSI,
                      // FINNISH,           = TWLG_FIN;
                      // FRENCH,            = TWLG_FRN;
                      FRENCH_BELGIAN,
                      // FRENCH_CANADIAN,   = TWLG_FCF;
                      FRENCH_LUXEMBOURG,
                      FRENCH_SWISS,
                      // GERMAN,            = TWLG_GER;
                      GERMAN_AUSTRIAN,
                      GERMAN_LUXEMBOURG,
                      GERMAN_LIECHTENSTEIN,
                      GERMAN_SWISS,
                      GREEK,
                      HEBREW,
                      HUNGARIAN,
                      // ICELANDIC,         = TWLG_ICE;
                      INDONESIAN,
                      // ITALIAN,           = TWLG_ITN;
                      ITALIAN_SWISS,
                      JAPANESE,
                      KOREAN,
                      KOREAN_JOHAB,
                      LATVIAN,
                      LITHUANIAN,
                      // NORWEGIAN,         = TWLG_NOR;
                      NORWEGIAN_BOKMAL,
                      NORWEGIAN_NYNORSK,
                      POLISH,
                      // PORTUGUESE,        = TWLG_POR;
                      PORTUGUESE_BRAZIL,
                      ROMANIAN,
                      RUSSIAN,
                      SERBIAN_LATIN,
                      SLOVAK,
                      SLOVENIAN,
                      // SPANISH,           = TWLG_SPA;
                      SPANISH_MEXICAN,
                      SPANISH_MODERN,
                      // SWEDISH,           = TWLG_SWE;
                      THAI,
                      TURKISH,
                      UKRANIAN,
                      ASSAMESE,
                      BENGALI,
                      BIHARI,
                      BODO,
                      DOGRI,
                      GUJARATI,
                      HARYANVI,
                      HINDI,
                      KANNADA,
                      KASHMIRI,
                      MALAYALAM,
                      MARATHI,
                      MARWARI,
                      MEGHALAYAN,
                      MIZO,
                      NAGA,
                      ORISSI,
                      PUNJABI,
                      PUSHTU,
                      SERBIAN_CYRILLIC,
                      SIKKIMI,
                      SWEDISH_FINLAND,
                      TAMIL,
                      TELUGU,
                      TRIPURI,
                      URDU,
                      VIETNAMESE);


type TmcmCountry   = (AFGHANISTAN,
                      ALGERIA,
                      AMERICANSAMOA,
                      ANDORRA,
                      ANGOLA,
                      ANGUILLA,
                      ANTIGUA,
                      ARGENTINA,
                      ARUBA,
                      ASCENSIONI,
                      AUSTRALIA,
                      AUSTRIA,
                      BAHAMAS,
                      BAHRAIN,
                      BANGLADESH,
                      BARBADOS,
                      BELGIUM,
                      BELIZE,
                      BENIN,
                      BERMUDA,
                      BHUTAN,
                      BOLIVIA,
                      BOTSWANA,
                      BRITAIN,
                      BRITVIRGINIS,
                      BRAZIL,
                      BRUNEI,
                      BULGARIA,
                      BURKINAFASO,
                      BURMA,
                      BURUNDI,
                      CAMAROON,
                      CANADA,
                      CAPEVERDEIS,
                      CAYMANIS,
                      CENTRALAFREP,
                      CHAD,
                      CHILE,
                      CHINA,
                      CHRISTMASIS,
                      COCOSIS,
                      COLOMBIA,
                      COMOROS,
                      CONGO,
                      COOKIS,
                      COSTA_RICA,
                      CUBA,
                      CYPRUS,
                      CZECHOSLOVAKIA,
                      DENMARK,
                      DJIBOUTI,
                      DOMINICA,
                      DOMINCANREP,
                      EASTERIS,
                      ECUADOR,
                      EGYPT,
                      ELSALVADOR,
                      EQGUINEA,
                      ETHIOPIA,
                      FALKLANDIS,
                      FAEROEIS,
                      FIJIISLANDS,
                      FINLAND,
                      FRANCE,
                      FRANTILLES,
                      FRGUIANA,
                      FRPOLYNEISA,
                      FUTANAIS,
                      GABON,
                      GAMBIA,
                      GERMANY,
                      GHANA,
                      GIBRALTER,
                      GREECE,
                      GREENLAND,
                      GRENADA,
                      GRENEDINES,
                      GUADELOUPE,
                      GUAM,
                      GUANTANAMOBAY,
                      GUATEMALA,
                      GUINEA,
                      GUINEABISSAU,
                      GUYANA,
                      HAITI,
                      HONDURAS,
                      HONG_KONG,
                      HUNGARY,
                      ICELAND,
                      INDIA,
                      INDONESIA,
                      IRAN,
                      IRAQ,
                      IRELAND,
                      ISRAEL,
                      ITALY,
                      IVORY_COAST,
                      JAMAICA,
                      JAPAN,
                      JORDAN,
                      KENYA,
                      KIRIBATI,
                      KOREA,
                      KUWAIT,
                      LAOS,
                      LEBANON,
                      LIBERIA,
                      LIBYA,
                      LIECHTENSTEIN,
                      LUXENBOURG,
                      MACAO,
                      MADAGASCAR,
                      MALAWI,
                      MALAYSIA,
                      MALDIVES,
                      MALI,
                      MALTA,
                      MARSHALLIS,
                      MAURITANIA,
                      MAURITIUS,
                      MEXICO,
                      MICRONESIA,
                      MIQUELON,
                      MONACO,
                      MONGOLIA,
                      MONTSERRAT,
                      MOROCCO,
                      MOZAMBIQUE,
                      NAMIBIA,
                      NAURU,
                      NEPAL,
                      NETHERLANDS,
                      NETHANTILLES,
                      NEVIS,
                      NEWCALEDONIA,
                      NEWZEALAND,
                      NICARAGUA,
                      NIGER,
                      NIGERIA,
                      NIUE,
                      NORFOLKI,
                      NORWAY,
                      OMAN,
                      PAKISTAN,
                      PALAU,
                      PANAMA,
                      PARAGUAY,
                      PERU,
                      PHILLIPPINES,
                      PITCAIRNIS,
                      PNEWGUINEA,
                      POLAND,
                      PORTUGAL,
                      QATAR,
                      REUNIONI,
                      ROMANIA,
                      RWANDA,
                      SAIPAN,
                      SANMARINO,
                      SAOTOME,
                      SAUDIARABIA,
                      SENEGAL,
                      SEYCHELLESIS,
                      SIERRALEONE,
                      SINGAPORE,
                      SOLOMONIS,
                      SOMALI,
                      SOUTH_AFRICA,
                      SPAIN,
                      SRILANKA,
                      STHELENA,
                      STKITTS,
                      STLUCIA,
                      STPIERRE,
                      STVINCENT,
                      SUDAN,
                      SURINAME,
                      SWAZILAND,
                      SWEDEN,
                      SWITZERLAND,
                      SYRIA,
                      TAIWAN,
                      TANZANIA,
                      THAILAND,
                      TOBAGO,
                      TOGO,
                      TONGAIS,
                      TRINIDAD,
                      TUNISIA,
                      TURKEY,
                      TURKSCAICOS,
                      TUVALU,
                      UGANDA,
                      USSR,
                      UAEMIRATES,
                      UNITEDKINGDOM,
                      USA,
                      URUGUAY,
                      VANUATU,
                      VATICANCITY,
                      VENEZUELA,
                      WAKE,
                      WALLISIS,
                      WESTERNSAHARA,
                      WESTERNSAMOA,
                      YEMEN,
                      YUGOSLAVIA,
                      ZAIRE,
                      ZAMBIA,
                      ZIMBABWE,
                      ALBANIA_, // This and below was added to TWAIN ver 1.8
                      ARMENIA,
                      AZERBAIJAN,
                      BELARUS,
                      BOSNIAHERZGO,
                      CAMBODIA,
                      CROATIA_,
                      CZECHREPUBLIC,
                      DIEGOGARCIA,
                      ERITREA,
                      ESTONIA,
                      GEORGIA,
                      LATVIA,
                      LESOTHO,
                      LITHUANIA,
                      MACEDONIA,
                      MAYOTTEIS,
                      MOLDOVA,
                      MYANMAR,
                      NORTHKOREA,
                      PUERTORICO,
                      RUSSIA,
                      SERBIA,
                      SLOVAKIA,
                      SLOVENIA,
                      SOUTHKOREA,
                      UKRAINE,
                      USVIRGINIS,
                      VIETNAM);


type TRealFrame     = record
                      Left   : double;
                      Top    : double;
                      Right  : double;
                      Bottom : double;
                      end;

type TImageLayout   = record
                      Frame          : TRealFrame;
                      DocumentNumber : integer;
                      PageNumber     : integer;
                      FrameNumber    : integer;
                      end;

type TImageInfo     = record
                      XResolution     : real;
                      YResolution     : real;
                      ImageWidth      : integer;
                      ImageLength     : integer;
                      SamplesPerPixel : smallint;
                      BitsPerSample   : array[0..7] of smallint;
                      BitsPerPixel    : smallint;
                      Planar          : Bool;
                      PixelType       : smallint;
                      Compression     : word;
                      end;

Resourcestring
  rsImageReadyError = 'Event "OnImageReady" or "OnMemXferBuffer" must be assigned, ' + chr($0D) +
                      'before an image acquisition can commence.';

type
  EImageReadyError = class(Exception);

//------------------------------------------------------------------------------
// TTwnSourceInfo.
//------------------------------------------------------------------------------

  TTwnSourceInfo = class(TComponent)
  private
    // Private declarations
    SrcId : TW_IDENTITY;
    function GetId : TW_UINT32;
    function GetVersion : string;
    function GetLanguage : TmcmLanguage;
    function GetCountry : TmcmCountry;
    function GetInfo : string;
    function GetManufacturer : string;
    function GetProductFamily : string;
    function GetProtocol : string;
    function GetProductName : string;
    function GetSupportedGroups  : TW_UINT32;
  protected
    // Protected declarations
    procedure SetSourceIdentity(pValue : pTW_IDENTITY);
    procedure Clear;
  public
    // public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    property Country : TmcmCountry
      read   GetCountry;
    property Id : TW_UINT32
      read   GetId;
    property Info : string
      read   GetInfo;
    property Language : TmcmLanguage
      read   GetLanguage;
    property Manufacturer : string
      read   GetManufacturer;
    property ProductFamily : string
      read   GetProductFamily;
    property ProductName : string
      read   GetProductName;
    property Protocol : string
      read   GetProtocol;
    property SupportedGroups  : TW_UINT32
      read   GetSupportedGroups;
    property Version : string
      read   GetVersion;
  published
    // published declarations
  end;

//------------------------------------------------------------------------------
// TmcmTWAIN.
//------------------------------------------------------------------------------

  TmcmTWAIN = class(TmcmTWAINIntf)
  private
    // Private declarations
    hTWNInternal      : TBitmap;           // Bitmap displayed in design mode.
    bDesignMode       : boolean;           // In Design mode ?
    FDisEndAcq        : bool;              // Disable after acquire.
    FSrcInfo          : TTwnSourceInfo;    // Source information object.
    hBmpImage         : HBitmap;           // Bitmap handle, to returned image.
    FEnableFeeder     : boolean;           // True if Feeder enabled.
    FOnNegotiation    : TOnNegotiate;      // Capability negotiations can begin.
    FOnDeviceNotReady : TOnDeviceNotReady; // Device not ready on source open.
    FOnEnableUIOnly   : TOnNegotiate;      // Capability negotiations can begin.

    function    GetFileFormats : TTwnFileFmts;
    procedure   SetFileFormats(Value : TTwnFileFmts);

  protected
    // Protected declarations
    procedure   Clear; override;
    procedure   Paint; override;
    function    GetAutoBrightness : boolean;
    function    GetBitDepth : integer;
    function    GetBitOrder : integer;
    function    GetBrightness : real;
    function    GetClearBatchBuffers : integer;
    function    GetContrast : real;
    function    GetCountry : TmcmCountry;
    function    GetDeviceOnline : bool;
    function    GetDisEndAcq : bool;
    function    GetDuplexEnabled : boolean;
    function    GetEnableUIOnly : boolean;
    function    GetLanguage : TmcmLanguage;
    function    GetManufacturer : AnsiString;
    function    GetMaxBatchBuffers : integer;
    function    GetMaxFrames : integer;
    function    GetMinimumHeight : double;
    function    GetMinimumWidth : double;
    function    GetNativeXResolution : real;
    function    GetNativeYResolution : real;
    function    GetOrientation : integer;
    function    GetPageType : integer;
    function    GetPixelType : integer;
    function    GetPlanarChunky : integer;
    function    GetPhysicalHeight : real;
    function    GetPhysicalWidth : real;
    function    GetProductFamily : AnsiString;
    function    GetProductName : AnsiString;
    function    GetRotation : real;
    function    GetSourceInfo : TTwnSourceInfo;
    function    GetXResolution : real;
    function    GetXScaling : real;
    function    GetYResolution : real;
    function    GetYScaling : real;
    procedure   SetAutoBrightness(Value : boolean);
    procedure   SetBitDepth(Value : integer);
    procedure   SetBitOrder(Value : integer);
    procedure   SetBrightness(Value : real);
    procedure   SetClearBatchBuffers(Value : integer);
    procedure   SetContrast(Value : real);
    procedure   SetCountry(Value : TmcmCountry);
    procedure   SetDisEndAcq(Value : bool);
    procedure   SetDuplexEnabled(Value : boolean);
    procedure   SetLanguage(Value : TmcmLanguage);
    procedure   SetManufacturer(Value : AnsiString);
    procedure   SetMaxBatchBuffers(Value : integer);
    procedure   SetMaxFrames(Value : integer);
    procedure   SetOrientation(Value : integer);
    procedure   SetPageType(Value : integer);
    procedure   SetPixelType(Value : integer);
    procedure   SetPlanarChunky(Value : integer);
    procedure   SetProductFamily(Value : AnsiString);
    procedure   SetProductName(Value : AnsiString);
    procedure   SetRotation(Value : real);
    procedure   SetXResolution(Value : real);
    procedure   SetXScaling(Value : real);
    procedure   SetYResolution(Value : real);
    procedure   SetYScaling(Value : real);
    procedure   WMSize(var Msg : TMessage); message WM_SIZE;
    procedure   WMGetMaxMinInfo(var Msg : TMessage); message WM_GETMINMAXINFO;
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   About;
    function    Acquire(SourceName : string) : boolean;
    function    AutoFeed(Value : boolean) : boolean;
    function    AutoScan(Value : boolean) : boolean;
    function    ClearPage : boolean;
    function    CloseSource : boolean;
    function    CloseSourceMgr : boolean;
    procedure   DisableSource;
    function    Duplex : integer;
    function    ConfigureSource(SourceName : string) : boolean;
    function    EnableSource(ShowUI : boolean) : boolean;
{$IFDEF SENDDIALOGUE}
    procedure   ExecuteDlgSend;
{$ENDIF}
    function    FeederEnabled(Value : boolean) : boolean;
    function    FeederLoaded : boolean;
    function    FeedPage : boolean;
    function    GetCapabilityMsg (    Cap           : word;
                                      Msg           : word;
                                  var Container     : TTwnContainer) : integer; override; // Overrides visibility of mcmTWAINIntf.
    function    GetFrames : TTwnContainer;
    function    GetImageInfo : TImageInfo;
    function    GetImageLayout : TImageLayout;
    function    GetImagePalette(var PalType : word; var pPalette : PLogPalette) : TW_UINT16;
    procedure   GetSourceList(Items : TStrings);
    function    IsCapSupported(Value : integer) : boolean; override;
    function    IsExCapSupported(Value : integer) : boolean; override;
    function    NumImagesToScan(Value : smallint) : boolean;
    function    OpenSource(SourceName : string) : boolean;
    function    OpenSourceMgr : boolean;
    function    PaperDetectable : boolean;
{$IFDEF PREFDIALOGUE}
    procedure   Preferrences;
{$ENDIF}
    function    ResetImageLayout : TImageLayout;
    function    RewindPage : boolean;
    procedure   SelectSource;
{$IFDEF ICONSOURCEDIALOGUE}
    procedure   SelectSourceIcon;
{$ENDIF}
    function    SetCapabilityMsg (    Msg           : word;
                                      AsOneValue    : bool;
                                      Container     : TTwnContainer) : integer; override;
    procedure   SetFrames(Value : TTwnContainer);
    function    SetImageLayout(ImageLayout : TImageLayout) : integer;
    function    SetImagePalette(PalType : word; pPalette : PLogPalette) : TW_UINT16;

  {$IFDEF ACTIVETWAIN}
    property    DemoVer : boolean
      read      FIsDemoVer
      write     FIsDemoVer;
    property    DesignMode : boolean
      read      bDesignMode
      write     bDesignMode;
  {$ENDIF}

    property    AutoBrightness : boolean
      read      GetAutoBrightness
      write     SetAutoBrightness;
    property    BitDepth : integer
      read      GetBitDepth
      write     SetBitDepth;
    property    BitOrder : integer
      read      GetBitOrder
      write     SetBitOrder;
    property    Brightness : real
      read      GetBrightness
      write     SetBrightness;
    property    ClearBatchBuffers : integer
      read      GetClearBatchBuffers
      write     SetClearBatchBuffers;
    property    Containers; // Overrides visibility of mcmTWAINIntf.
    property    Contrast : real
      read      GetContrast
      write     SetContrast;
    property    DeviceOnline : bool
      read      GetDeviceOnline;
    property    DSResult;
    property    DSStatus;
    property    DuplexEnabled : boolean
      read      GetDuplexEnabled
      write     SetDuplexEnabled;
    property    ImageHeight;      // Overrides visibility of mcmTWAINKernel.
    property    ImageWidth;       // Overrides visibility of mcmTWAINKernel.
    property    IsDSMOpen;        // Overrides visibility of mcmTWAINKernel.
    property    IsDSOpen;         // Overrides visibility of mcmTWAINKernel.
    property    MaxBatchBuffers : integer
      read      GetMaxBatchBuffers
      write     SetMaxBatchBuffers;
    property    MaxFrames : integer
      read      GetMaxFrames
      write     SetMaxFrames;
    property    MinimumHeight : double
      read      GetMinimumHeight;
    property    MinimumWidth : double
      read      GetMinimumWidth;
    property    NativeXResolution : real
      read      GetNativeXResolution;
    property    NativeYResolution : real
      read      GetNativeYResolution;
    property    NumCapabilities; // Overrides visibility of mcmTWAINIntf.
    property    Orientation : integer
      read      GetOrientation
      write     SetOrientation;
    property    PhysicalHeight : real
      read      GetPhysicalHeight;
    property    PhysicalWidth : real
      read      GetPhysicalWidth;
    property    PageType : integer
      read      GetPageType
      write     SetPageType;
    property    PixelFlavor : integer
      read      GetPixelFlavor
      write     SetPixelFlavor;
    property    PixelType : integer
      read      GetPixelType
      write     SetPixelType;
    property    PlanarChunky : integer
      read      GetPlanarChunky
      write     SetPlanarChunky;
    property    Rotation : real
      read      GetRotation
      write     SetRotation;
    property    SourceInfo : TTwnSourceInfo
      read      GetSourceInfo;
    property    State;            // Overrides visibility of mcmTWAINKernel.
    property    Units : integer
      read      GetUnits
      write     SetUnits;
    property    XResolution : real
      read      GetXResolution
      write     SetXResolution;
    property    XScaling : real
      read      GetXScaling
      write     SetXScaling;
    property    YResolution : real
      read      GetYResolution
      write     SetYResolution;
    property    YScaling : real
      read      GetYScaling
      write     SetYScaling;
  published
    // Published declarations
    property    Country : TmcmCountry
      read      GetCountry
      write     SetCountry default TmcmCountry(TWCY_UNITEDKINGDOM);
    property    DeviceEventTypes;
    property    DIBHandleType;    // Overrides visibility of mcmTWAINKernel.
    property    DisableAfterAcquire : bool
      read      GetDisEndAcq
      write     SetDisEndAcq default True;
    property    FileFormat;       // Overrides visibility of mcmTWAINKernel.
    property    FileFormats : TTwnFileFmts
      read      GetFileFormats
      write     SetFileFormats default [TWFF_BMP];
    property    Filename;         // Overrides visibility of mcmTWAINKernel.
    property    Language : TmcmLanguage
      read      GetLanguage
      write     SetLanguage default ENGLISH_INTL;
    property    LenientOnCaps;
    property    LogFilename;      // Overrides visibility of mcmTWAINKernel.
    property    LogToFile;        // Overrides visibility of mcmTWAINKernel.
    property    Manufacturer : AnsiString
      read      GetManufacturer
      write     SetManufacturer;
    property    MessageLevel;     // Overrides visibility of mcmTWAINKernel.
    property    ProductFamily : AnsiString
      read      GetProductFamily
      write     SetProductFamily;
    property    ProductName : AnsiString
      read      GetProductName
      write     SetProductName;
    property    ReturnHandle;     // Overrides visibility of mcmTWAINKernel.
    property    ShowIndicators;   // Overrides visibility of mcmTWAINIntf.
    property    ShowUI;           // Overrides visibility of mcmTWAINIntf.
    property    SwapMemRGB;       // Overrides visibility of mcmTWAINIntf.
    property    XferMech;         // Overrides visibility of mcmTWAINIntf.

    // Events.
    property    OnCloseSource;    // Overrides visibility of mcmTWAINKernel.
    property    OnDeviceEvent;    // Overrides visibility of mcmTWAINIntf.
    property    OnDeviceNotReady : TOnDeviceNotReady
      read      FOnDeviceNotReady
      write     FOnDeviceNotReady;
    property    OnDisableMenus;   // Overrides visibility of mcmTWAINKernel.
    property    OnEnableMenus;    // Overrides visibility of mcmTWAINKernel.
    property    OnEnableUIOnly : TOnNegotiate
      read      FOnEnableUIOnly
      write     FOnEnableUIOnly;
    property    OnFailure;        // Overrides visibility of mcmTWAINKernel.
    property    OnImageReady;     // Overrides visibility of mcmTWAINIntf.
    property    OnMemXferBuffer;  // Overrides visibility of mcmTWAINIntf.
    property    OnMemXferSize;    // Overrides visibility of mcmTWAINIntf.
    property    OnNegotiation : TOnNegotiate
      read      FOnNegotiation
      write     FOnNegotiation;
    property    OnXferNext;       // Overrides visibility of mcmTWAINIntf.
    property    OnXferReady;      // Overrides visibility of mcmTWAINKernel.
    property    OnXferProgress;   // Overrides visibility of mcmTWAINIntf.
  end;


implementation

uses
{$IFDEF GE_DXE2}  System.Types, {$ENDIF}
{$IFDEF ICONSOURCEDIALOGUE} mcmSelectSource, {$ENDIF}
{$IFDEF PREFDIALOGUE}       uTwnDlgPref,     {$ENDIF}
{$IFDEF SENDDIALOGUE}       uTwnDlgSend,     {$ENDIF}
                  mcmTWAINFix,
                  uTwnAbout;

{$IFOPT T+} {$DEFINE TYPED_ADDRESS_ON} {$T-} {$ENDIF}
{$IFOPT X-} {$DEFINE EXTENDED_SYNTAX} {$X+} {$ENDIF}


//------------------------------------------------------------------------------
// TTwnSourceInfo.
//------------------------------------------------------------------------------

constructor TTwnSourceInfo.Create(AOwner : TComponent);
begin
  Inherited;
  Clear;
end; // TTwnSourceInfo.Create.                                          


destructor TTwnSourceInfo.Destroy;
begin
  Inherited;
end; // TTwnSourceInfo.Destroy.                                         


procedure TTwnSourceInfo.SetSourceIdentity(pValue : pTW_IDENTITY);
begin
  CopyMemory(@SrcId, pValue, SizeOf(TW_IDENTITY));
end; // TTwnSourceInfo.SetSourceIdentity.                               


procedure TTwnSourceInfo.Clear;
begin
  FillChar(SrcId, SizeOf(TW_IDENTITY), 0);
end; // TTwnSourceInfo.Clear.                                           


function TTwnSourceInfo.GetId : TW_UINT32;
begin
  Result := SrcId.Id;
end; // TTwnSourceInfo.GetId.                                           


function TTwnSourceInfo.GetVersion : string;
begin
  Result := IntToStr(SrcId.Version.MajorNum) + '.' + IntToStr(SrcId.Version.MinorNum);
end; // TTwnSourceInfo.GetVersion.                                      


function TTwnSourceInfo.GetLanguage : TmcmLanguage;
begin
  Result := TmcmLanguage(SrcId.Version.Language);
end; // TTwnSourceInfo.GetLanguage.                                     


function TTwnSourceInfo.GetCountry : TmcmCountry;
begin
  Result := TmcmCountry(SrcId.Version.Country);
end; // TTwnSourceInfo.GetCountry.                                      


function TTwnSourceInfo.GetInfo : string;
begin
  Result := string(SrcId.Version.Info);
end; // TTwnSourceInfo.GetInfo.                                         


function TTwnSourceInfo.GetManufacturer : string;
begin
  Result := string(SrcId.Manufacturer);
end; // TTwnSourceInfo.GetManufacturer.                                 


function TTwnSourceInfo.GetProductFamily : string;
begin
  Result := string(SrcId.ProductFamily);
end; // TTwnSourceInfo.GetProductFamily.                                


function TTwnSourceInfo.GetProtocol : string;
begin
  Result := IntToStr(SrcId.ProtocolMajor) + '.' + IntToStr(SrcId.ProtocolMinor);
end; // TTwnSourceInfo.GetProtocol.                                     


function TTwnSourceInfo.GetProductName : string;
begin
  Result := string(SrcId.ProductName);
end; // TTwnSourceInfo.GetProductName.                                  


function TTwnSourceInfo.GetSupportedGroups : TW_UINT32;
begin
  Result := SrcId.SupportedGroups;
end; // TTwnSourceInfo.GetSupportedGroups.                              


//------------------------------------------------------------------------------
// TmcmTWAIN.
//------------------------------------------------------------------------------

//const MCW_EM = DWord($133f); // Constant to turn off floating point exceptions!

constructor TmcmTWAIN.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  ControlStyle := [csNoStdEvents, csFixedWidth, csFixedHeight];

  Set8087CW(DWord($133f)); // Turn off floating point exceptions!

  Width  := 28;
  Height := 28;

  FOnDeviceNotReady := Nil;
  FOnNegotiation    := Nil;
  FOnEnableUIOnly   := Nil;

  {$IFDEF MCMDEMO}
    FIsDemoVer := True;
  {$ELSE}
    FIsDemoVer := False;
  {$ENDIF}

  {$IFDEF ACTIVETWAIN}
    bDesignMode := False;
    hTWNInternal := Nil;
    hTWNInternal := TBitmap.Create;
    hTWNInternal.Handle := LoadBitmap(HInstance, PChar(1));
  {$ELSE}
  bDesignMode := (csDesigning in ComponentState);
  if Not(bDesignMode)
  then begin
       Visible := False;
       hTWNInternal := Nil;
  end
  else begin
       hTWNInternal := TBitmap.Create;
       hTWNInternal.Handle := LoadBitmap(HInstance, 'TmcmTWAIN');
  end;
  {$ENDIF}

  FDisEndAcq    := True;
  FEnableFeeder := False;
  hBmpImage := 0;
  FSrcInfo := Nil;
end; // TmcmTWAIN.Create.


destructor TmcmTWAIN.Destroy;
begin
  //FTWAINThread.ClearEventProcs;

  // Clean-up, free memory.
  if (hBmpImage <> 0)
  then DeleteObject(hBmpImage);
  hBmpImage := 0;
  if Assigned(FSrcInfo)
  then FSrcInfo.Free;
  if Assigned(hTWNInternal)
  then hTWNInternal.Free;

  Inherited Destroy;
end; // TmcmTWAIN.Destroy.


procedure TmcmTWAIN.Clear;
begin
  Inherited Clear;
end; // TmcmTWAIN.Clear.                                                


procedure TmcmTWAIN.About;
begin
  mcmAboutBox := TmcmAboutBox.Create(Self);
  {$IFDEF ACTIVETWAIN}
    mcmAboutBox.DemoVersion := FIsDemoVer;
  {$ENDIF}
  mcmAboutBox.ShowModal;
  mcmAboutBox.Free;
end; // TmcmTWAIN.About.                                                


function TmcmTWAIN.GetDisEndAcq : bool;
begin
  Result := FDisEndAcq;
end; // TmcmTWAIN.GetDisEndAcq.                                         


procedure TmcmTWAIN.SetDisEndAcq(Value : bool);
begin
  FDisEndAcq := Value;
end; // TmcmTWAIN.SetDisEndAcq.                                         


procedure TmcmTWAIN.SetManufacturer(Value : AnsiString);
begin
  {$IFDEF GE_DXE4}
  System.AnsiStrings.StrPLCopy(FAppIdentity.Manufacturer, Value, 32);
  {$ELSE}
  StrPLCopy(FAppIdentity.Manufacturer, Value, 32);
  {$ENDIF}
  FAppIdentity.Manufacturer[33] := #0;
end; // TmcmTWAIN.SetManufacturer.


function TmcmTWAIN.GetManufacturer : AnsiString;
begin
  Result := AnsiString(FAppIdentity.Manufacturer);
end; // TmcmTWAIN.GetManufacturer.


procedure TmcmTWAIN.SetProductFamily(Value : AnsiString);
begin
  {$IFDEF GE_DXE4}
  System.AnsiStrings.StrPLCopy(FAppIdentity.ProductFamily, Value, 32);
  {$ELSE}
  StrPLCopy(FAppIdentity.ProductFamily, Value, 32);
  {$ENDIF}
  FAppIdentity.ProductFamily[33] := #0;
end; // TmcmTWAIN.SetProductFamily.


function TmcmTWAIN.GetProductFamily : AnsiString;
begin
  Result := AnsiString(FAppIdentity.ProductFamily);
end; // TmcmTWAIN.GetProductFamily.


procedure TmcmTWAIN.SetProductName(Value : AnsiString);
begin
  {$IFDEF GE_DXE4}
  System.AnsiStrings.StrPLCopy(FAppIdentity.ProductName, Value, 32);
  {$ELSE}
  StrPLCopy(FAppIdentity.ProductName, Value, 32);
  {$ENDIF}
  FAppIdentity.ProductName[33] := #0;
end; // TmcmTWAIN.SetProductName.


function TmcmTWAIN.GetProductName : AnsiString;
begin
  Result := AnsiString(FAppIdentity.ProductName);
end; // TmcmTWAIN.GetProductName.                                       


procedure TmcmTWAIN.SetLanguage(Value : TmcmLanguage);
var iLanguage : integer;
begin
  with FAppIdentity
  do begin
     if (integer(Value) = 0)
     then iLanguage := -1
     else iLanguage := integer(Value) - 1;
     Version.Language := word(iLanguage);
  end;
end; // TmcmTWAIN.SetLanguage.


function TmcmTWAIN.GetLanguage : TmcmLanguage;
var iLanguage : integer;
begin
  iLanguage := FAppIdentity.Version.Language;
  if (iLanguage = -1)
  then iLanguage := 0
  else inc(iLanguage);
  Result := TmcmLanguage(iLanguage);
end; // TmcmTWAIN.GetLanguage.                                          


procedure TmcmTWAIN.SetCountry(Value : TmcmCountry);
begin
  FAppIdentity.Version.Country := CmcmCountry[integer(Value)];
end; // TmcmTWAIN.SetCountry.                                           


function TmcmTWAIN.GetCountry : TmcmCountry;
var i : integer;
begin
  Result := UNITEDKINGDOM;
  i := 0;
  while  (i < NoCountry)
  do begin
     if (FAppIdentity.Version.Country = CmcmCountry[i])
     then begin
          Result := TmcmCountry(i);
          exit;
     end;
     inc(i);
  end;
end; // TmcmTWAIN.GetCountry.                                           


function TmcmTWAIN.GetFileFormats : TTwnFileFmts;
begin
  Result := FAllFormats;
end; // TmcmTWAIN.GetFileFormats.                                       


procedure TmcmTWAIN.SetFileFormats(Value : TTwnFileFmts);
begin
  // Set applications supported file formats.
  // Disable Macintosh file formats.
  if (TWFF_PICT in Value)
  then Value := Value - [TWFF_PICT];
  if (TWFF_XBM in Value)
  then Value := Value - [TWFF_XBM];
  FAllFormats := Value;
end; // TmcmTWAIN.SetFileFormats.                                       


procedure TmcmTWAIN.Paint;
var x, y : integer;
begin
  Inherited Paint;
  if bDesignMode
  then begin
       if (Width <> 28)
       then Width := 28;
       if (Height <> 28)
       then Height := 28;
       if Assigned(hTWNInternal)
       then begin
            x := (Width - hTWNInternal.Width) div 2;
            y := (Height - hTWNInternal.Height) div 2;
            hTWNInternal.Transparent := True;
            Canvas.Draw(x, y, hTWNInternal);
            hTWNInternal.TransparentMode := tmAuto;
       end;
       Canvas.Pen.Color := clWhite;
       Canvas.MoveTo(0, Height - 1);
       Canvas.LineTo(0, 0);
       Canvas.LineTo(Width - 1, 0);
       Canvas.Pen.Color := clGray;
       Canvas.LineTo(Width - 1, Height - 1);
       Canvas.LineTo(0, Height - 1);
  end;
end; // TmcmTWAIN.Paint.                                                


procedure TmcmTWAIN.WMSize(var Msg : TMessage);
begin
  if bDesignMode
  then begin
       Msg.lParam := 28 + (28 shl 16);
       Msg.WParam := SIZE_MAXIMIZED;
  end;
  Msg.Result := 0;
end; // TmcmTWAIN.WMSize.                                               


procedure TmcmTWAIN.WMGetMaxMinInfo(var Msg : TMessage);
var pMaxMinInfo : PMINMAXINFO;
begin
  if bDesignMode
  then begin
       pMaxMinInfo := PMINMAXINFO(Msg.lParam);
       if Assigned(pMaxMinInfo)
       then begin
            with pMaxMinInfo^
            do begin
               ptMaxSize := Point(28, 28);
               ptMinTrackSize := Point(28, 28);
               ptMaxTrackSize := Point(28, 28);
            end;
       end;
  end;
  Msg.Result := 0;
end; // TmcmTWAIN.WMGetMaxMinInfo.


function TmcmTWAIN.OpenSourceMgr : boolean;
begin
  // Fix for windows 95, 98 and Me
  if Application.ShowHint
  then Application.ShowHint := False;
  Application.CancelHint;
  Application.ProcessMessages;
  // End - Fix for windows 95, 98 and Me

  if Not(IsDSMOpen)
  then OpenDSM;
  Result := IsDSMOpen;

  // Fix for windows 95, 98 and Me
  Application.ShowHint := True;
  // End - Fix for windows 95, 98 and Me
end; // TmcmTWAIN.OpenSourceMgr.


function TmcmTWAIN.CloseSourceMgr : boolean;
begin
  if IsDSMOpen
  then CloseDSM;
  Result := Not(IsDSMOpen); // Return true if DSM is closed.
end; // TmcmTWAIN.CloseSourceMgr.                                       


function TmcmTWAIN.OpenSource(SourceName : string) : boolean;
var DoOpenSource : boolean;
    DoCancel     : boolean;
begin
  DoCancel := False;
  // If TWAIN drivers "Product name" is supplied, use that TWAIN driver.
  if (Length(SourceName) > 0)
  then SetDefaultSource(SourceName);

  if Not(IsDSOpen)
  then begin
       Clear;
       AcqFlag := TWFG_NONE; // Not ready to transfer yet.
       if (OpenDS = TWRC_SUCCESS)
       then begin
            // Get supported capabilities
            // - for negotiation in state 4
            GetSupportedCaps;

            // Online status is only checked if FOnDeviceNotReady is assigned.
            if Assigned(FOnDeviceNotReady)
            then begin
                 // Check if Device is online!
                 DoOpenSource := True;
                 while DoOpenSource and Not(DeviceOnline)
                 do FOnDeviceNotReady(Self, DoOpenSource);
                 if Not(DoOpenSource)
                 then CloseSource;
            end;

            if IsDSOpen
            then begin
                 // - for negotiation after state 4
                 GetSupportedExCaps;

                 // Set device events supported by application.
                 SetDeviceEvents(DeviceEventTypes);
                 // Set transfer mechanisme.
                 if (SetupXferMech = TWRC_SUCCESS)
                 then begin
                      // Negotiate if Indicators should show.
                      if IsCapSupported(CAP_INDICATORS)
                      then SetIndicators;

                      // Call applications negotiate event procedure.
                      if Assigned(FOnNegotiation)
                      then begin
                           FOnNegotiation(Self, DoCancel);
                           if DoCancel
                           then CloseSource;
                      end;
                 end
                 else CloseSource;
            end;
       end;
  end;
  Result := IsDSOpen and Not(DoCancel);
end; // TmcmTWAIN.OpenSource.                                           


function TmcmTWAIN.CloseSource : boolean;
begin
  DisableSource;
  if IsDSOpen
  then begin
       Result := (CloseDS = TWRC_SUCCESS);
       Clear;
       AcqFlag := TWFG_NONE;
  end
  else Result := False;
end; // TmcmTWAIN.CloseSource.                                          


function TmcmTWAIN.EnableSource(ShowUI : boolean) : boolean;
begin
  if Not(Assigned(OnImageReady) or Assigned(OnMemXferBuffer))
  then begin
       CloseSource;
       CloseSourceMgr;
       Raise EImageReadyError.Create(RSImageReadyError);
  end;
  if Not(IsDSEnabled)
  then begin
       FShowUI := ShowUI;
       // FAcqFlag = TWFG_NONE     Do Not Accept MSG_XFERREADY
       //            TWFG_CLOSE    Disable/CloseDS/CloseDSM
       //            TWFG_DISABLE  Disable Only
       //            TWFG_KEEPOPEN Do Not Disable - only if ShowUI=True
       if FShowUI
       then begin
            if DisableAfterAcquire
            then AcqFlag := TWFG_CLOSE
            else AcqFlag := TWFG_KEEPOPEN;
       end
       else begin
            if DisableAfterAcquire
            then AcqFlag := TWFG_CLOSE
            else AcqFlag := TWFG_KEEPOPEN;
       end;
       EnableDS(FShowUI, FModalUI);
  end;
  Result := IsDSEnabled;
end; // TmcmTWAIN.EnableSource.


procedure TmcmTWAIN.DisableSource;
begin
  if IsDSEnabled
  then DisableDS;
end; // TmcmTWAIN.DisableSource.


procedure TmcmTWAIN.SelectSource;
begin
  SelectDS;
end; // TmcmTWAIN.SelectSource.


{$IFDEF ICONSOURCEDIALOGUE}
procedure TmcmTWAIN.SelectSourceIcon;
begin
  FormSelectSource := TFormSelectSource.Create(Self);
  FormSelectSource.ShowModal;
  FormSelectSource.Free;
end; // TmcmTWAIN.SelectSourceIcon.
{$ENDIF}


procedure TmcmTWAIN.GetSourceList(Items : TStrings);
var SourceStr  : string;
    DSMwasOpen : boolean;
    DSIdentity : TW_IDENTITY;
begin
  if Not(IsDSMOpen)
  then begin
       OpenSourceMgr;
       DSMwasOpen := False;
  end
  else DSMwasOpen := True;
  if IsDSMOpen
  then begin
       Items.Clear;
       DSIdentity := FSourceID;
       if (GetFirstSource(SourceStr) = TWRC_SUCCESS)
       then begin
            Items.Add(SourceStr);
            while (GetNextSource(SourceStr) = TWRC_SUCCESS) and (Length(SourceStr) > 0)
            do Items.Add(SourceStr);
       end;
       FSourceID := DSIdentity;
       if Not(DSMwasOpen)
       then CloseSourceMgr;
  end;
end; // TmcmTWAIN.GetSourceList.


function TmcmTWAIN.Acquire(SourceName : string) : boolean;
begin
  Result := False;
  if OpenSourceMgr
  then begin
       // Please note that another Source may change the system default while
       // your not looking and simply getting the default will not guarentee
       // you get what you want. Suggest saving the dsID structure privately
       // after you open it to assure subsequent connections are to your
       // intended Source -- ed
       // Also note the the DSM will maintain a static list of all Sources in
       // the system at the moment it is opened.  Changes in available Sources
       // while the DSM is open may cause unpredictable results. -- ed

       // Open data source.
       if OpenSource(SourceName)
       then begin
            if Not(IsDSEnabled)
            then Result := EnableSource(FShowUI);
       end;

       if Not(Result)
       then begin
            CloseSource;
            CloseSourceMgr;
       end;
  end;
end; // TmcmTWAIN.Acquire.


function TmcmTWAIN.ConfigureSource(SourceName : string) : boolean;
var SaveNegotiation : TOnNegotiate;
    DoCancel        : boolean;
begin
  DoCancel := False;
  // Disable applications OnNegotiate event.
  // We will be calling the event FOnEnableUIOnly instead.
  SaveNegotiation := FOnNegotiation;
  FOnNegotiation := Nil;

  Result := False;
  if OpenSourceMgr
  then begin
       // Please note that another Source may change the system default while
       // your not looking and simply getting the default will not guarentee
       // you get what you want. Suggest saving the dsID structure privately
       // after you open it to assure subsequent connections are to your
       // intended Source -- ed
       // Also note the the DSM will maintain a static list of all Sources in
       // the system at the moment it is opened.  Changes in available Sources
       // while the DSM is open may cause unpredictable results. -- ed

       // Open data source.
       if OpenSource(SourceName)
       then begin
            // Check that using MSG_ENABLEDSUIONLY is allowed.
            if GetEnableUIOnly
            then begin
                 // Call applications negotiate event procedure.
                 if Assigned(FOnEnableUIOnly)
                 then begin
                      FOnEnableUIOnly(Self, DoCancel);
                      if DoCancel
                      then CloseSource;
                 end;

                 if Not(DoCancel) and Not(IsDSEnabled)
                 then begin
                      // Check that CAP_ENABLEDSUIONLY

                      // FAcqFlag = TWFG_NONE     Do Not Accept MSG_XFERREADY
                      //            TWFG_CLOSE    Disable/CloseDS/CloseDSM
                      //            TWFG_DISABLE  Disable Only
                      //            TWFG_KEEPOPEN Do Not Disable - only if ShowUI=True
                      if FShowUI
                      then begin
                           if DisableAfterAcquire
                           then AcqFlag := TWFG_CLOSE
                           else AcqFlag := TWFG_KEEPOPEN;
                      end
                      else begin
                           if DisableAfterAcquire
                           then AcqFlag := TWFG_CLOSE
                           else AcqFlag := TWFG_KEEPOPEN;
                      end;
                      Result := (ConfigureDS(FShowUI, FModalUI) = TWRC_SUCCESS);
                 end;
            end;
       end;

       if Not(Result)
       then begin
            // If the DS failed to open in configuration mode close it.
            CloseSource;
            CloseSourceMgr;
       end;
  end;

  // Re-enable applications OnNegotiate event.
  FOnNegotiation := SaveNegotiation;
end; // TmcmTWAIN.ConfigureSource.


function TmcmTWAIN.IsCapSupported(Value : integer) : boolean;
begin
  Result := Inherited IsCapSupported(Value);
end; // TmcmTWAIN.IsCapSupported.


function TmcmTWAIN.IsExCapSupported(Value : integer) : boolean;
begin
  Result := Inherited IsExCapSupported(Value);
end; // TmcmTWAIN.IsExCapSupported.


function TmcmTWAIN.GetCapabilityMsg(    Cap       : word;
                                        Msg       : word;
                                    var Container : TTwnContainer) : integer;
begin
  if IsCapSupported(Cap)
  then Result := Inherited GetCapabilityMsg(Cap, Msg, Container)
  else Result := -1;
end; // TmcmTWAIN.GetCapabilityMsg.


function TmcmTWAIN.SetCapabilityMsg(Msg        : word;
                                    AsOneValue : bool;
                                    Container  : TTwnContainer) : integer;
begin
  if IsCapSupported(Container.Capability)
  then Result := Inherited SetCapabilityMsg(Msg, AsOneValue, Container)
  else Result := -1;
end; // TmcmTWAIN.SetCapabilityMsg.                                     


//------------------------------------------------------------------------------
// DG_IMAGE / DAT_XXXX / MSG_XXX
//------------------------------------------------------------------------------

function TmcmTWAIN.GetImageInfo : TImageInfo;
var ImageInfo    : TW_IMAGEINFO;
    TwnImageInfo : TImageInfo;
    i            : Integer;
begin
  FillChar(TwnImageInfo, SizeOf(TImageInfo), 0);
  if (ImageInfoA(@ImageInfo) = 0)
  then begin
       with TwnImageInfo
       do begin
          XResolution      := FIX32ToFloat(ImageInfo.XResolution);
          YResolution      := FIX32ToFloat(ImageInfo.YResolution);
          ImageWidth       := ImageInfo.ImageWidth;
          ImageLength      := ImageInfo.ImageLength;
          SamplesPerPixel  := ImageInfo.SamplesPerPixel;
          for i := 0 to 6
          do BitsPerSample[i] := ImageInfo.BitsPerSample[i];
          BitsPerPixel     := ImageInfo.BitsPerPixel;
          Planar           := Bool(ImageInfo.Planar);
          PixelType        := ImageInfo.PixelType;
          Compression      := ImageInfo.Compression;
       end;
  end;
  Result := TwnImageInfo;
end; // TmcmTWAIN.GetImageInfo.


function TmcmTWAIN.GetImageLayout : TImageLayout;
var ImageLayout : TW_IMAGELAYOUT;
begin
  if (ImageLayoutA(@ImageLayout, MSG_GET) = 0)
  then begin
       Result.Frame.Left     := FIX32ToFloat(ImageLayout.Frame.Left);
       Result.Frame.Top      := FIX32ToFloat(ImageLayout.Frame.Top);
       Result.Frame.Right    := FIX32ToFloat(ImageLayout.Frame.Right);
       Result.Frame.Bottom   := FIX32ToFloat(ImageLayout.Frame.Bottom);
       Result.DocumentNumber := ImageLayout.DocumentNumber;
       Result.PageNumber     := ImageLayout.PageNumber;
       Result.FrameNumber    := ImageLayout.FrameNumber;
  end
  else FillChar(Result, SizeOf(TImageLayout), 0);
end; // TmcmTWAIN.GetImageLayout.                                       


function TmcmTWAIN.ResetImageLayout : TImageLayout;
var ImageLayout : TW_IMAGELAYOUT;
begin
  if (ImageLayoutA(@ImageLayout, MSG_RESET) = 0)
  then begin
       Result.Frame.Left     := FIX32ToFloat(ImageLayout.Frame.Left);
       Result.Frame.Top      := FIX32ToFloat(ImageLayout.Frame.Top);
       Result.Frame.Right    := FIX32ToFloat(ImageLayout.Frame.Right);
       Result.Frame.Bottom   := FIX32ToFloat(ImageLayout.Frame.Bottom);
       Result.DocumentNumber := ImageLayout.DocumentNumber;
       Result.PageNumber     := ImageLayout.PageNumber;
       Result.FrameNumber    := ImageLayout.FrameNumber;
  end
  else FillChar(Result, SizeOf(TImageLayout), 0);
end; // TmcmTWAIN.ResetImageLayout.


function TmcmTWAIN.SetImageLayout(ImageLayout : TImageLayout) : integer;
var AImageLayout : TW_IMAGELAYOUT;
begin
  AImageLayout.Frame.Left     := FloatToFIX32(ImageLayout.Frame.Left);
  AImageLayout.Frame.Top      := FloatToFIX32(ImageLayout.Frame.Top);
  AImageLayout.Frame.Right    := FloatToFIX32(ImageLayout.Frame.Right);
  AImageLayout.Frame.Bottom   := FloatToFIX32(ImageLayout.Frame.Bottom);
  AImageLayout.DocumentNumber := ImageLayout.DocumentNumber;
  AImageLayout.PageNumber     := ImageLayout.PageNumber;
  AImageLayout.FrameNumber    := ImageLayout.FrameNumber;
  Result := ImageLayoutA(@AImageLayout, MSG_SET);
end; // TmcmTWAIN.SetImageLayout.                                       



function TmcmTWAIN.GetDeviceOnline : bool;
var Container : TTwnContainer;
begin
  {$IFOPT B+} {$DEFINE COMPLETE_BOOL_EVAL} {$B-} {$ENDIF}
  Container := Nil;
  if (GetCapabilityMsg(CAP_DEVICEONLINE, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := True; // Return "True" as data source doesn't support
                       // CAP_DEVICEONLINE and we don't know the true status.
  {$IFDEF COMPLETE_BOOL_EVAL} {$B+} {$UNDEF COMPLETE_BOOL_EVAL} {$ENDIF}
end; // TmcmTWAIN.GetDeviceOnline.


function TmcmTWAIN.GetEnableUIOnly : boolean;
//------------------------------------------------------------------------------
// YResolution
//   MSG_GET container (Enumeration, OneValue, Range)
//   ItemType = TW_FIX32
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(CAP_ENABLEDSUIONLY, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := False;
end; // TmcmTWAIN.GetEnableUIOnly.


function TmcmTWAIN.GetBitDepth : integer;
//------------------------------------------------------------------------------
// BitDepth
//   MSG_GET container (Enumeration, OneValue)
//   ItemType = TW_UINT16
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  {$IFOPT B+} {$DEFINE COMPLETE_BOOL_EVAL} {$B-} {$ENDIF}
  Container := Nil;
  if (GetCapabilityMsg(ICAP_BITDEPTH, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1;
  {$IFDEF COMPLETE_BOOL_EVAL} {$B+} {$UNDEF COMPLETE_BOOL_EVAL} {$ENDIF}
end; // TmcmTWAIN.GetBitDepth.                                          


procedure TmcmTWAIN.SetBitDepth(Value : integer);
//------------------------------------------------------------------------------
// BitDepth
//   MSG_SET container (Enumeration, OneValue)
//   ItemType = TW_UINT16
//------------------------------------------------------------------------------
begin
  if (Containers.Items[ICAP_BITDEPTH] = Nil)
  then GetBitDepth;
  if (Containers.Items[ICAP_BITDEPTH] <> Nil)
  then begin
       Containers.Items[ICAP_BITDEPTH].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_BITDEPTH]);
  end;
end; // TmcmTWAIN.SetBitDepth.                                          


function TmcmTWAIN.GetBitOrder : integer;
//------------------------------------------------------------------------------
// BitOrder
//   MSG_GET container (Enumeration, OneValue)
//   ItemType = TW_UINT16
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_BITORDER, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1;
end; // TmcmTWAIN.GetBitOrder.                                          


procedure TmcmTWAIN.SetBitOrder(Value : integer);
//------------------------------------------------------------------------------
// BitOrder
//   MSG_SET container (OneValue)
//   ItemType = TW_UINT16
//------------------------------------------------------------------------------
begin
  if (Containers.Items[ICAP_BITORDER] = Nil)
  then GetBitOrder;
  if (Containers.Items[ICAP_BITORDER] <> Nil)
  then begin
       Containers.Items[ICAP_BITORDER].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_BITORDER]);
  end;
end; // TmcmTWAIN.SetBitOrder.                                          


function TmcmTWAIN.GetMaxFrames : integer;
//------------------------------------------------------------------------------
// MaxFrames
//   MSG_GET container (Enumeration, OneValue)
//   ItemType = TW_UINT16
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_MAXFRAMES, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := 0;
end; // TmcmTWAIN.GetMaxFrames.                                         


procedure TmcmTWAIN.SetMaxFrames(Value : integer);
//------------------------------------------------------------------------------
// MaxFrames
//   MSG_SET container (Enumeration, OneValue)
//   ItemType = TW_UINT16
//------------------------------------------------------------------------------
begin
  if (Containers.Items[ICAP_MAXFRAMES] = Nil)
  then GetMaxFrames;
  if (Containers.Items[ICAP_MAXFRAMES] <> Nil)
  then begin
       Containers.Items[ICAP_MAXFRAMES].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_MAXFRAMES]);
  end;
end; // TmcmTWAIN.SetMaxFrames.                                         


function TmcmTWAIN.GetFrames : TTwnContainer;
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_FRAMES, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container
  else Result := Nil;
end; // TmcmTWAIN.GetFrames.                                            


procedure TmcmTWAIN.SetFrames(Value : TTwnContainer);
var Container : TTwnContainer;
begin
  Container := Value;
  if Not(Assigned(Container))
  then Container := Containers.Items[ICAP_FRAMES];
  if Assigned(Container)
  then SetCapabilityMsg(MSG_SET, False, Container);
end; // TmcmTWAIN.SetFrames.


function TmcmTWAIN.GetOrientation : integer;
//------------------------------------------------------------------------------
// Orientation
//   MSG_GET container (OneValue)
//   ItemType = TW_UINT16
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_ORIENTATION, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1;
end; // TmcmTWAIN.GetOrientation.                                       


procedure TmcmTWAIN.SetOrientation(Value : integer);
//------------------------------------------------------------------------------
// Orientation
//   MSG_SET container (Enumeration, OneValue)                              
//   ItemType = TW_UINT16                                                   
//------------------------------------------------------------------------------
begin
  if (Containers.Items[ICAP_ORIENTATION] = Nil)
  then GetOrientation;
  if (Containers.Items[ICAP_ORIENTATION] <> Nil)
  then begin
       Containers.Items[ICAP_ORIENTATION].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_ORIENTATION]);
  end;
end; // TmcmTWAIN.SetOrientation.                                       


function TmcmTWAIN.GetMinimumHeight : double;
//------------------------------------------------------------------------------
// MinimumHeight                                                            
//   MSG_GET container (Enumeration, OneValue)                              
//   MSG_SET container (NONE)
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_MINIMUMHEIGHT, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1.0;
end; // TmcmTWAIN.GetMinimumHeight.


function TmcmTWAIN.GetMinimumWidth : double;
//------------------------------------------------------------------------------
// MinimumWidth                                                             
//   MSG_GET container (Enumeration, OneValue)                              
//   MSG_SET container (NONE)                                               
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_MINIMUMWIDTH, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1.0;
end; // TmcmTWAIN.GetMinimumWidth.                                      


function TmcmTWAIN.GetPhysicalHeight : real;
//------------------------------------------------------------------------------
// PhysicalHeight                                                           
//   MSG_GET container (Enumeration, OneValue)                              
//   MSG_SET container (NONE)                                               
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_PHYSICALHEIGHT, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1.0;
end; // TmcmTWAIN.GetPhysicalHeight.                                    


function TmcmTWAIN.GetPhysicalWidth : real;
//------------------------------------------------------------------------------
// PhysicalWidth                                                            
//   MSG_GET container (Enumeration, OneValue)                              
//   MSG_SET container (NONE)                                               
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_PHYSICALWIDTH, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1.0;
end; // TmcmTWAIN.GetPhysicalWidth.                                     


function TmcmTWAIN.GetPageType : integer;
//------------------------------------------------------------------------------
// SupportedSize                                                            
//   MSG_GET container (Enumeration, OneValue)                              
//   ItemType = TW_UINT16                                                   
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_SUPPORTEDSIZES, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1;
end; // TmcmTWAIN.GetPageType.                                          


procedure TmcmTWAIN.SetPageType(Value : integer);
//------------------------------------------------------------------------------
// SupportedSize                                                            
//   MSG_SET container (Enumeration, OneValue)
//   ItemType = TW_UINT16                                                   
//------------------------------------------------------------------------------
begin
  if (Containers.Items[ICAP_SUPPORTEDSIZES] = Nil)
  then GetPageType;
  if (Containers.Items[ICAP_SUPPORTEDSIZES] <> Nil)
  then begin
       Containers.Items[ICAP_SUPPORTEDSIZES].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_SUPPORTEDSIZES]);
  end;
end; // TmcmTWAIN.SetPageType.                                          


function TmcmTWAIN.GetPixelType : integer;
//------------------------------------------------------------------------------
// PixelType                                                                
//   MSG_GET container (Enumeration, OneValue)                              
//   ItemType = TW_UINT16                                                   
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_PIXELTYPE, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1;
end; // TmcmTWAIN.GetPixelType.                                         


procedure TmcmTWAIN.SetPixelType(Value : integer);
//------------------------------------------------------------------------------
// PixelType                                                                
//   MSG_SET container (Enumeration, OneValue)                              
//   ItemType = TW_UINT16                                                   
//------------------------------------------------------------------------------
begin
  if (Containers.Items[ICAP_PIXELTYPE] = Nil)
  then GetPixelType;
  if (Containers.Items[ICAP_PIXELTYPE] <> Nil)
  then begin
       Containers.Items[ICAP_PIXELTYPE].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_PIXELTYPE]);
  end;
end; // TmcmTWAIN.SetPixelType.                                         


function TmcmTWAIN.GetPlanarChunky : integer;
//------------------------------------------------------------------------------
// PlanarChunky                                                             
//   MSG_GET container (Enumeration, OneValue)                              
//   ItemType = TW_UINT16                                                   
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_PLANARCHUNKY, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1;
end; // TmcmTWAIN.GetPlanarChunky.                                      


procedure TmcmTWAIN.SetPlanarChunky(Value : integer);
//------------------------------------------------------------------------------
// PlanarChunky                                                             
//   MSG_SET container (Enumeration, OneValue)                              
//   ItemType = TW_UINT16                                                   
//------------------------------------------------------------------------------
begin
  if (Containers.Items[ICAP_PLANARCHUNKY] = Nil)
  then GetPlanarChunky;
  if (Containers.Items[ICAP_PLANARCHUNKY] <> Nil)
  then begin
       Containers.Items[ICAP_PLANARCHUNKY].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_PLANARCHUNKY]);
  end;
end; // TmcmTWAIN.SetPlanarChunky.                                      


function TmcmTWAIN.GetAutoBrightness : boolean;
//------------------------------------------------------------------------------
// Auto Brightness                                                          
//   MSG_GET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_AUTOBRIGHT, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := False;
end; // TmcmTWAIN.GetAutoBrightness.                                    


procedure TmcmTWAIN.SetAutoBrightness(Value : boolean);
//------------------------------------------------------------------------------
// Auto Brightness
//   MSG_SET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
begin
  if (Containers.Items[ICAP_AUTOBRIGHT] = Nil)
  then GetAutoBrightness;
  if (Containers.Items[ICAP_AUTOBRIGHT] <> Nil)
  then begin
       Containers.Items[ICAP_AUTOBRIGHT].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_AUTOBRIGHT]);
  end;
end; // TmcmTWAIN.SetAutoBrightness.                                    


function TmcmTWAIN.GetBrightness : real;
//------------------------------------------------------------------------------
// Brightness                                                               
//   MSG_GET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_BRIGHTNESS, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1;
end; // TmcmTWAIN.GetBrightness.                                        


procedure TmcmTWAIN.SetBrightness(Value : real);
//------------------------------------------------------------------------------
// Brightness                                                               
//   MSG_SET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
begin
  if (Containers.Items[ICAP_BRIGHTNESS] = Nil)
  then GetBrightness;
  if (Containers.Items[ICAP_BRIGHTNESS] <> Nil)
  then begin
       Containers.Items[ICAP_BRIGHTNESS].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_BRIGHTNESS]);
  end;
end; // TmcmTWAIN.SetBrightness.                                        


function TmcmTWAIN.GetContrast : real;
//------------------------------------------------------------------------------
// Contrast                                                                 
//   MSG_GET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//   Range -1000..0..1000                                                   
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_CONTRAST, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1;
end; // TmcmTWAIN.GetContrast.                                          


procedure TmcmTWAIN.SetContrast(Value : real);
//------------------------------------------------------------------------------
// Contrast                                                                 
//   MSG_SET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//   Range -1000..0..1000                                                   
//------------------------------------------------------------------------------
begin
  if (Containers.Items[ICAP_CONTRAST] = Nil)
  then GetContrast;
  if (Containers.Items[ICAP_CONTRAST] <> Nil)
  then begin
       Containers.Items[ICAP_CONTRAST].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_CONTRAST]);
  end;
end; // TmcmTWAIN.SetContrast.                                          


function TmcmTWAIN.GetRotation : real;
//------------------------------------------------------------------------------
// Rotation                                                                 
//   MSG_GET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//   Range -1000..0..1000                                                   
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_ROTATION, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1.0;
end; // TmcmTWAIN.GetRotation.                                          


procedure TmcmTWAIN.SetRotation(Value : real);
//------------------------------------------------------------------------------
// Rotation                                                                 
//   MSG_SET container (OneValue)                                           
//   ItemType = TW_FIX32                                                    
//   Range -1000..0..1000                                                   
//------------------------------------------------------------------------------
begin
  if (Containers.Items[ICAP_ROTATION] = Nil)
  then GetRotation;
  if (Containers.Items[ICAP_ROTATION] <> Nil)
  then begin
       Containers.Items[ICAP_ROTATION].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_ROTATION]);
  end;
end; // TmcmTWAIN.SetRotation.                                          


function TmcmTWAIN.GetNativeXResolution : real;
//------------------------------------------------------------------------------
// X Native Resolution                                                              
//   MSG_GET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_XNATIVERESOLUTION, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1.0;
end; // TmcmTWAIN.GetNativeXResolution.                                       


function TmcmTWAIN.GetNativeYResolution : real;
//------------------------------------------------------------------------------
// Y Native Resolution                                                              
//   MSG_GET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_YNATIVERESOLUTION, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1.0;
end; // TmcmTWAIN.GetNativeYResolution.                                       


function TmcmTWAIN.GetXResolution : real;
//------------------------------------------------------------------------------
// XResolution                                                              
//   MSG_GET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_XRESOLUTION, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1.0;
end; // TmcmTWAIN.GetXResolution.                                       


procedure TmcmTWAIN.SetXResolution(Value : real);
//------------------------------------------------------------------------------
// XResolution                                                              
//   MSG_SET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32
//------------------------------------------------------------------------------
begin
  // NOTE: Generates an access violation in AGFA twain driver, if container is
  // different from TW_ONEVALUE.
  if (Containers.Items[ICAP_XRESOLUTION] = Nil)
  then GetXResolution;
  if (Containers.Items[ICAP_XRESOLUTION] <> Nil)
  then begin
       Containers.Items[ICAP_XRESOLUTION].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_XRESOLUTION]);
  end;
end; // TmcmTWAIN.SetXResolution.                                       


function TmcmTWAIN.GetYResolution : real;
//------------------------------------------------------------------------------
// YResolution                                                              
//   MSG_GET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_YRESOLUTION, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1.0;
end; // TmcmTWAIN.GetYResolution.                                       


procedure TmcmTWAIN.SetYResolution(Value : real);
//------------------------------------------------------------------------------
// YResolution                                                              
//   MSG_SET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
begin
  // Generates an access violation in AGFA twain driver, if container is
  // different from TW_ONEVALUE.
  if (Containers.Items[ICAP_YRESOLUTION] = Nil)
  then GetYResolution;
  if (Containers.Items[ICAP_YRESOLUTION] <> Nil)
  then begin
       Containers.Items[ICAP_YRESOLUTION].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_YRESOLUTION]);
  end;
end; // TmcmTWAIN.SetYResolution.                                       


function TmcmTWAIN.GetXScaling : real;
//------------------------------------------------------------------------------
// XScaling                                                                 
//   MSG_GET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_XSCALING, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1.0;
end; // TmcmTWAIN.GetXScaling.                                          


procedure TmcmTWAIN.SetXScaling(Value : real);
//------------------------------------------------------------------------------
// XResolution                                                              
//   MSG_SET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
begin
  // NOTE: Generates an access violation in AGFA twain driver, if container is
  // different from TW_ONEVALUE.
  if (Containers.Items[ICAP_XSCALING] = Nil)
  then GetXScaling;
  if (Containers.Items[ICAP_XSCALING] <> Nil)
  then begin
       Containers.Items[ICAP_XSCALING].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_XSCALING]);
  end;
end; // TmcmTWAIN.SetXScaling.                                       


function TmcmTWAIN.GetYScaling : real;
//------------------------------------------------------------------------------
// YScaling                                                                 
//   MSG_GET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(ICAP_YSCALING, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1.0;
end; // TmcmTWAIN.GetYScaling.                                          


procedure TmcmTWAIN.SetYScaling(Value : real);
//------------------------------------------------------------------------------
// YResolution                                                              
//   MSG_SET container (Enumeration, OneValue, Range)                       
//   ItemType = TW_FIX32                                                    
//------------------------------------------------------------------------------
begin
  // Generates an access violation in AGFA twain driver, if container is
  // different from TW_ONEVALUE.
  if (Containers.Items[ICAP_YSCALING] = Nil)
  then GetYScaling;
  if (Containers.Items[ICAP_YSCALING] <> Nil)
  then begin
       Containers.Items[ICAP_YSCALING].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[ICAP_YSCALING]);
  end;
end; // TmcmTWAIN.SetYScaling.                                          


function TmcmTWAIN.NumImagesToScan(Value : smallint) : boolean;
var Container : TTwnContainer;
begin
  Container := Containers.Items[CAP_XFERCOUNT];
  if Not(Assigned(Container))
  then Container := Containers.CreateItem(CAP_XFERCOUNT);
  if Assigned(Container)
  then begin
       Container.ItemType      := TWTY_UINT16;
       Container.CurrentValue  := Value;
       Result := (SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS);
  end
  else Result := False;
end; // TmcmTWAIN.NumImagesToScan.


function TmcmTWAIN.FeederEnabled(Value : boolean) : boolean;
var Container : TTwnContainer;
    TmpValue  : boolean;
begin
  // EnableFeeder must be called in State 4, i.e. after call to OpenSource, but
  // before EnableSource.
  Result := False;
  if IsCapSupported(CAP_FEEDERENABLED)
  then begin
       Container := Nil;
       if (GetCapabilityMsg(CAP_FEEDERENABLED, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            // Document feeder is available.
            FEnableFeeder := Container.CurrentValue;
            if (Container.CurrentValue <> Value)
            then begin
                 // CurrentValue is different from Value, so set it.
                 TmpValue := Container.CurrentValue;
                 Container.CurrentValue := Value;
                 if (SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                 then begin
                      // Verify that CAP_FEEDERENABLED was set to Value.
                      Container := Nil;
                      if (GetCapabilityMsg(CAP_FEEDERENABLED, MSG_GET, Container) = TWRC_SUCCESS)
                      then begin
                           FEnableFeeder := Container.CurrentValue;
                           if (Container.CurrentValue = Value)
                           then Result := True;
                      end;
                 end
                 else Container.CurrentValue := TmpValue;
            end
            else Result := True;
       end
       else FEnableFeeder := False; // Document feeder is not available.
  end
  else FEnableFeeder := False;
end; // TmcmTWAIN.FeederEnabled.                                        


function TmcmTWAIN.AutoFeed(Value : boolean) : boolean;
var Container : TTwnContainer;
begin
  Result := False;
  if FEnableFeeder and IsCapSupported(CAP_AUTOFEED)
  then begin
       Container := Nil;
       if (GetCapabilityMsg(CAP_AUTOFEED, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            // Auto feed is available.
            if (Container.CurrentValue <> Value)
            then begin
                 // CurrentValue is different from Value, so set it.
                 Container.CurrentValue := Value;
                 if (SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                 then begin
                      // Verify that CAP_AUTOFEED was set to Value.
                      Container := Nil;
                      if (GetCapabilityMsg(CAP_AUTOFEED, MSG_GET, Container) = TWRC_SUCCESS)
                      then if (Container.CurrentValue = Value)
                           then Result := True;
                 end;
            end
            else Result := True;
       end;
  end;
end; // TmcmTWAIN.AutoFeed.                                             


function TmcmTWAIN.AutoScan(Value : boolean) : boolean;
begin
  if IsCapSupported(CAP_AUTOSCAN)
  then Result := (SendBoolCap(CAP_AUTOSCAN, MSG_SET, Value) = TWRC_SUCCESS)
  else Result := False;
end; // TmcmTWAIN.AutoScan.                                             


function TmcmTWAIN.GetClearBatchBuffers : integer;
var Container  : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(CAP_CLEARBUFFERS, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1;
end; // TmcmTWAIN.GetClearBatchBuffers.


procedure TmcmTWAIN.SetClearBatchBuffers(Value : integer);
begin
  if (Containers.Items[CAP_CLEARBUFFERS] = Nil)
  then GetClearBatchBuffers;
  if (Containers.Items[CAP_CLEARBUFFERS] <> Nil)
  then begin
       Containers.Items[CAP_CLEARBUFFERS].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[CAP_CLEARBUFFERS]);
  end;
end; // TmcmTWAIN.SetClearBatchBuffers.


function TmcmTWAIN.GetMaxBatchBuffers : integer;
var Container  : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(CAP_MAXBATCHBUFFERS, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := -1;
end; // TmcmTWAIN.GetMaxBatchBuffers.


procedure TmcmTWAIN.SetMaxBatchBuffers(Value : integer);
begin
  if (Containers.Items[CAP_MAXBATCHBUFFERS] = Nil)
  then GetMaxBatchBuffers;
  if (Containers.Items[CAP_MAXBATCHBUFFERS] <> Nil)
  then begin
       Containers.Items[CAP_MAXBATCHBUFFERS].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[CAP_MAXBATCHBUFFERS]);
  end;
end; // TmcmTWAIN.SetMaxBatchBuffers.


function TmcmTWAIN.FeederLoaded : boolean;
var Container      : TTwnContainer;
    //bPaperDetected : boolean;
    bFeederLoaded  : boolean;
begin
  if FEnableFeeder //and IsCapSupported(CAP_PAPERDETECTABLE)
  then begin
       {
       Container := Nil;
       if (GetCapabilityMsg(CAP_PAPERDETECTABLE, MSG_GET, Container) = TWRC_SUCCESS)
       then bPaperDetected := Container.CurrentValue
       else bPaperDetected := False;
       }
       Container := Nil;
       if (GetCapabilityMsg(CAP_FEEDERLOADED, MSG_GET, Container) = TWRC_SUCCESS)
       then bFeederLoaded := Container.CurrentValue
       else bFeederLoaded := False;

       Result := {bPaperDetected or} bFeederLoaded;
  end
  else Result := False;
end; // TmcmTWAIN.GetFeederLoaded.                                      


function TmcmTWAIN.FeedPage : boolean;
begin
  // Note: FeedPage can only be used in state 5 & 6.
  if FEnableFeeder and IsCapSupported(CAP_FEEDPAGE)
  then Result := (SendBoolCap(CAP_FEEDPAGE, MSG_SET, True) = TWRC_SUCCESS)
  else Result := False;
end; // TmcmTWAIN.FeedPage.                                             


function TmcmTWAIN.RewindPage : boolean;
begin
  // Note: RewindPage can only be used in state 5 & 6.
  if FEnableFeeder
  then Result := (SendBoolCap(CAP_REWINDPAGE, MSG_SET, True) = TWRC_SUCCESS)
  else Result := False;
end; // TmcmTWAIN.RewindPage.                                           


function TmcmTWAIN.ClearPage : boolean;
begin
  // Note: ClearPage can only be used in state 5 & 6.
  if FEnableFeeder
  then Result := (SendBoolCap(CAP_CLEARPAGE, MSG_SET, True) = TWRC_SUCCESS)
  else Result := False;
end; // TmcmTWAIN.ClearPage.                                            


function TmcmTWAIN.PaperDetectable : boolean;
var Container  : TTwnContainer;
begin
  // Note: PaperDetectable can only be used in state 5 & 6.
  Container := Nil;
  if (GetCapabilityMsg(CAP_PAPERDETECTABLE, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := False;
end; // TmcmTWAIN.PaperDetectable.                                      


function TmcmTWAIN.Duplex : integer;
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(CAP_DUPLEX, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := TWDX_NONE;
end; // TmcmTWAIN.Duplex.                                               


function TmcmTWAIN.GetDuplexEnabled : boolean;
var Container : TTwnContainer;
begin
  Container := Nil;
  if (GetCapabilityMsg(CAP_DUPLEXENABLED, MSG_GET, Container) = TWRC_SUCCESS)
  then Result := Container.CurrentValue
  else Result := False;
end; // TmcmTWAIN.GetDuplexEnabled.                                     


procedure TmcmTWAIN.SetDuplexEnabled(Value : boolean);
begin
  if (Containers.Items[CAP_DUPLEXENABLED] = Nil)
  then GetDuplexEnabled;
  if (Containers.Items[CAP_DUPLEXENABLED] <> Nil)
  then begin
       Containers.Items[CAP_DUPLEXENABLED].CurrentValue := Value;
       SetCapabilityMsg(MSG_SET, True, Containers.Items[CAP_DUPLEXENABLED]);
  end;
end; // TmcmTWAIN.SetDuplexEnabled.                                     


function TmcmTWAIN.GetImagePalette(var PalType : word; var pPalette : PLogPalette) : TW_UINT16;
var twRC : TW_UINT16;
begin
  twRC := TWRC_FAILURE;
  GetMem(pPalette, SizeOf(TLogPalette) + 256 * SizeOf(TPaletteEntry));
  try
    twRC := Palette8(pPalette, PalType, MSG_GET);
  finally
    if (twRC <> TWRC_SUCCESS)
    then if (pPalette <> Nil)
         then begin
              FreeMem(pPalette);
              pPalette := Nil;
         end;
  end;
  Result := twRC;
end; // TmcmTWAIN.GetImagePalette.                                      


function TmcmTWAIN.SetImagePalette(PalType : word; pPalette : PLogPalette) : TW_UINT16;
var twRC : TW_UINT16;
begin
  twRC := TWRC_FAILURE;
  if (pPalette^.palNumEntries = 256)
  then twRC := Palette8(pPalette, PalType, MSG_SET);
  Result := twRC;
end; // TmcmTWAIN.SetPalette.                                           


{$IFDEF SENDDIALOGUE}
procedure TmcmTWAIN.ExecuteDlgSend;
begin
  DlgTwnSend := TDlgTwnSend.Create(Self);
  DlgTwnSend.ShowModal;
  DlgTwnSend.Free;
end; // TmcmTWAIN.ExecuteDlgSend.                                       
{$ENDIF}


{$IFDEF PREFDIALOGUE}
procedure TmcmTWAIN.Preferrences;
var SaveNegotiation : TOnNegotiate;
    PrefResult      : integer;
    PrefName        : string;
begin
  SaveNegotiation := FOnNegotiation;
  FOnNegotiation := Nil;
  DlgTwainPref := TDlgTwainPref.Create(Self);
//  FOnNegotiation := DlgTwainPref.TWAINNegotiation;
  PrefResult := DlgTwainPref.ShowModal;
  if (PrefResult = mrOK)
  then begin
       PrefName    := DlgTwainPref.SelectedDevice;
       FDisEndAcq  := DlgTwainPref.DisableAfterAcquire;
       FIndicators := DlgTwainPref.ShowIndicator;
       Acquire(PrefName);
  end;
  DlgTwainPref.Free;
  FOnNegotiation := SaveNegotiation;
end; // TmcmTWAIN.Preferrences.                                         
{$ENDIF}


function TmcmTWAIN.GetSourceInfo : TTwnSourceInfo;
var SaveNegotiation : TOnNegotiate;
begin
  // Disable applications OnNegotiate event.
  SaveNegotiation := FOnNegotiation;
  FOnNegotiation := Nil;

  // Create instance of source information object.
  if Not(Assigned(FSrcInfo))
  then FSrcInfo := TTwnSourceInfo.Create(Self);

  if Assigned(FSrcInfo)
  then begin
       // If a data source is opened, return information on this.
       FSrcInfo.SetSourceIdentity(@FSourceID);
  end;

  // Enable applications OnNegotiate event.
  FOnNegotiation := SaveNegotiation;
  Result := FSrcInfo;
end; // TmcmTWAIN.GetSourceInfo.


{$IFDEF TYPED_ADDRESS_ON} {$T+} {$UNDEF TYPED_ADDRESS_ON} {$ENDIF}
{$IFDEF EXTENDED_SYNTAX} {$X-} {$UNDEF EXTENDED_SYNTAX} {$ENDIF}
{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}


end.





