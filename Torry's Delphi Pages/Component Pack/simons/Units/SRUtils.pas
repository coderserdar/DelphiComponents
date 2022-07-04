unit SRUtils;

{----------------------------------------------------------------------}
{  Version  : 1.51                                                     }
{  Autor    : Simon Reinhardt                                          }
{  eMail    : reinhardt@picsoft.de                                     }
{  Internet : http://www.picsoft.de                                    }
{                                                                      }
{  Hilfreiche Prozeduren und Funktionen, die die Borland-Programmierer }
{  offensichtlich vergessen haben.                                     }
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{ Version 1.51:                                                        }
{ Neu: IsNumber                                                        }
{                                                                      }
{ Version 1.50:                                                        }
{ Geändert: ExtractItemsFromString, ConvertStrToDateTime,              }
{           CreateUniqueFileName,GetFirstDayOfWeekNr                   }
{                                                                      }
{ Version 1.49:                                                        }
{ Geändert: GetFirstDayOfWeekNr                                        }
{                                                                      }
{ Version 1.48:                                                        }
{ Neu: StringReplace                                                   }
{ Geändert: GetParentDir                                               }
{                                                                      }
{ Version 1.47:                                                        }
{ Neu: GetFolder, CreateUniqueFileName                                 }
{                                                                      }
{ Version 1.46:                                                        }
{ Neu: ExtractNumberFromText, CompareNumbers                           }
{      Konstante CSIDL_COMMON_APPDATA                                  }
{                                                                      }
{ Version 1.45:                                                        }
{ Neu: GetDiskSpace, GetTreeSize                                       }
{ Geändert: GetExeForProtocol, GetNumberFromText                       }
{                                                                      }
{ Version 1.44:                                                        }
{ Geändert: GetParentDir, RewrapText                                   }
{                                                                      }
{ Version 1.43:                                                        }
{ Neu: GetAppDataPeth,                                                 }
{      GetLocalComputerName, ReplaceEntities, ReplaceHighChars         }
{                                                                      }
{ Version 1.42:                                                        }
{ Neu: FindChildWindow, FindExecutableStr,                             }
{      GetWindowCaption, GetWindowClass, GetTextInWindow               }
{      Konstante CSIDL_LOCAL_APPDATA                                   }
{                                                                      }
{ Version 1.41:                                                        }
{ Neu: GetFileDateTime, SetFileDateTime, GetProgramFilesDir,           }
{      GetSystemFolderLocation, GetParentDir, GetTempDir               }
{ Geändert: GetExeForProtocol, FindAssociatedProgram                   }
{                                                                      }
{ Version 1.40:                                                        }
{ Neu: GetFullNodePath, GetNumberFromText, IsValidMailAdress           }
{                                                                      }
{ Version 1.39:                                                        }
{ Neu: GetFirstDayOfWeekNr                                             }
{ Geändert: IsSummertime , GetFirstDayOfWeek                           }
{                                                                      }
{ Version 1.38:                                                        }
{ Neu: ANSIUpCase, ANSILowCase                                         }
{ Geändert: CapitalizeString                                           }
{                                                                      }
{ Version 1.37:                                                        }
{ Neu: CapitalizeString, LowCase                                       }
{                                                                      }
{ Version 1.36:                                                        }
{ Neu: ExtractItemsFromString, FirstDelimiter, Like, RewrapText        }
{ Geändert: LastDelimiter                                              }
{                                                                      }
{ Version 1.35:                                                        }
{ Geändert: GetFirstPartOfString                                       }
{                                                                      }
{ Version 1.34:                                                        }
{ Neu: TrimChars                                                       }
{                                                                      }
{ Version 1.33:                                                        }
{ Neu: StrToTimeDef, TimeToStrDef                                      }
{                                                                      }
{ Version 1.32:                                                        }
{ Neu: GetDaysPerYear                                                  }
{ Geändert: StripForbiddenChars                                        }
{                                                                      }
{ Version 1.31:                                                        }
{ Neu: PosN                                                            }
{                                                                      }
{ Version 1.30:                                                        }
{ Neu: GetShiftState                                                   }
{                                                                      }
{ Version 1.29:                                                        }
{ Neu: ExtractFileDir, LastDelimiter                                   }
{ Geändert: GetExeForProtocol, FindAssociatedProgram                   }
{                                                                      }
{ Version 1.28:                                                        }
{ Geändert: DateTimeToStrDef, DateToStrDef                             }
{ Neu: Konstante PeriodNames                                           }
{                                                                      }
{ Version 1.27:                                                        }
{ Neu: GetWindowState, GetSystemWorkArea                               }
{                                                                      }
{ Version 1.26:                                                        }
{ Neu: GetFirstDayOfWeek                                               }
{ Geändert: IsSummerTime,                                              }
{ Initialisierung von FirstWeekDay und FirstWeekDate in Delphi 1       }
{                                                                      }
{ Version 1.25:                                                        }
{ Neu: GetHourFromTime, GetMinuteFromTime, GetSecondFromTime           }
{ Geändert: GetDayFromDate, GetMonthFromDate, GetYearFromDate          }
{                                                                      }
{ Version 1.24:                                                        }
{ Geändert: Konstanten ShortForbiddenChars und LongForbiddenChars      }
{                                                                      }
{ Version 1.23:                                                        }
{ Geändert: GetWeekOfYear, GetWeeksPerYear                             }
{                                                                      }
{ Version 1.22:                                                        }
{ Neu: DateToStrDef, StrToDateDef, GetWeeksPerYear                     }
{ Geändert: GetFirstPartOfString, AddBackSlash                         }
{                                                                      }
{----------------------------------------------------------------------}

interface

{$I SRDefine.inc}

{$IFDEF SR_Delphi1}
uses WinTypes, WinProcs, Graphics, Classes;
{$ELSE}
uses Windows, Graphics, Classes, ComCtrls;
{$ENDIF}


const
  { Standard Encarta & FlatStyle Color Constants     }
  { Diese konstanten hat maik Porkert am 31.10.2000  }
  { in de.comp.lang.delphi.non-tech gepostet.        }
  { Ich stelle Sie hier zur Verfügung:               }

  ecDarkBlue = TColor($00996633);
  ecBlue = TColor($00CF9030);
  ecLightBlue = TColor($00CFB78F);

  ecDarkRed = TColor($00302794);
  ecRed = TColor($005F58B0);
  ecLightRed = TColor($006963B6);

  ecDarkGreen = TColor($00385937);
  ecGreen = TColor($00518150);
  ecLightGreen = TColor($0093CAB1);

  ecDarkYellow = TColor($004EB6CF);
  ecYellow = TColor($0057D1FF);
  ecLightYellow = TColor($00B3F8FF);

  ecDarkBrown = TColor($00394D4D);
  ecBrown = TColor($00555E66);
  ecLightBrown = TColor($00829AA2);

  ecDarkKaki = TColor($00D3D3D3);
  ecKaki = TColor($00C8D7D7);
  ecLightKaki = TColor($00E0E9EF);

  { Konstanten für GetFileDateTime / SetFileDateTime: }
  ftCreation   = 0;
  ftLastAccess = 1;
  ftLastWrite  = 2;

  { Konstanten für GetSystemFolderLocation() }
  CSIDL_LOCAL_APPDATA  = $001C;
  CSIDL_COMMON_APPDATA = $0023;

  {$IFDEF SR_Delphi1}
  Max_Path = 255;
  {$ENDIF}
  { Ungültige Zeichen fuer 8.3-Dateinamen im DOS-Format: }
  ShortForbiddenChars :
    set of char = [':','?','*',';','=','+','<','>','|','"','[',']',' ','\',#39];
  { Ungültige Zeichen fuer lange Dateinamen im Win9x-Format: }
  LongForbiddenChars :
    set of char = ['\','/',':','*','?','"','<','>','|'];
  { Gültige Zeichen fuer E-Mail-Adressen: }
  AllowedMailChars :
    set of char = ['A'..'Z', 'a'..'z', '0'..'9', 'q', '.', '_', '-', '@'];

  { Bezeichner für relative Datumsangaben in DateTimeToStrDef und DateToStrDef: }
  PeriodNames :
    array [0..4] of string = ('Übermorgen', 'Morgen', 'Heute', 'Gestern', 'Vorgestern');

type
  TFileSizeFormat = (fsByte, fsKilobyte, fsMegabyte);
  { Rückgabe-Formate für die Funktion GetFileSize }
  {$IFDEF SR_Delphi4_Up}
  TDiskSpaceType = (dsTotal, dsFree, dsUsed);
  { Für die Funktion GetDiskSpace }
  {$ENDIF}
  {$IFNDEF SR_Delphi4_Up}
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);
  { Für die Funktion StringReplace }
  {$ENDIF}

{---------------------------------------}
{ Funktionen für alle Delphi-Versionen: }
{---------------------------------------}

function AddBackslash(FileName:string):string;
 { erweitert den Dateinamen mit einem abschließenden Backslash }
function ANSILowCase(const AChar:char):char;
 { Erweiterung der Lowcase-Funktion um Umlaute }
function ANSIUpCase(const AChar:char):char;
 { Erweiterung der Upcase-Funktion um Umlaute }
function CapitalizeString(const AText:string):string;
 { Wandelt den Text "AText" in einen Kleinbuchstaben um,
   alle Anfangsbuchstaben der Wörter werden aber groß geschrieben }
function CompareNumbers(const Number1,Number2:double):ShortInt;
 { Vergleicht die Zahlen "Number1" und "Number2" - wie CompareText }
function CutBackSlash(FileName:string):string;
 { entfernt den abschließenden Backslash aus dem Dateinamen }
function CutRootDirectory(FName:string):string;
 { entfernt das Stammverzeichnis aus einem Dateinamen }
function DateTimeToStrDef(ADate:TDateTime;Default:string;CompareToday:boolean):string;
 { Umwandlung DateTime->String mit Rückgabe eines Default-Wertes bei Fehlern }
function DateToStrDef(ADate:TDateTime;Default:string;CompareToday:boolean):string;
 { Umwandlung Date->String mit Rückgabe eines Default-Wertes bei Fehlern }
function ExecAndWait(const Filename,Params:string;WindowState:word):boolean;
 { Startet ein Programm und wartet auf dessen Ende }
function ExpandString(S:string;AChar:char;ALength:word):string;
 { Erweitert einen String mit dem Zeichen "AChar" auf die Länge ALength }
procedure ExtractItemsFromString(AFrom:string;ATo:TStrings;const Delimiter:string);
 { Extrahiert aus dem String "AFrom" die durch den String "Delimiter" getrennten Teile
   und gibt diese in der Stringliste "ATo" zurück }
function ExtractNumberFromText(const AText:string;const Digits:byte):extended;
 { Extrahiert eine Zahl aus dem String "AText". Dabei werden alle im String
   vorhandenen Ziffern, sowie ein Vorzeichen und Komma extrahiert }
function ExtractRawFileName(DName:string):string;
 { Gibt von einem vollen Dateinamen mit Pfad nur den Dateinamen ohne Erweiterung zurück }
function FindChildWindow(const OwnerWnd:HWnd;const AClass,AText:string;const ExactMatch:boolean):HWnd;
 { Ermittelt das Handle Fensters, dessen Parent oder Owner "OwnerWnd" ist.
   Wenn ExactMatch=false, dürfen AClass oder AText einen leeren String enthalten,
   um alle Fenster einer Klasse oder mit einem best. Titel zu finden }
function FindExecutableStr(const Filename:string):string;
 { Wrapper für die API-Funktion FindExecutable() }
function FirstDelimiter(AChars,AText:string):integer;
 { Ermittelt die erste Position eines Zeichens aus "AChars" im string "AText" }
function GetBuildInfo(const AFilename:String; var V1,V2,V3,V4:Word):Boolean;
 { Ermittelt die vier Versionsnummern einer Exe- oder Dll-Datei }
function GetDayFromDate(ADate:TDateTime):word;
 { Gibt den Tag im Monat aus einem Datums-Wert zurück }
function GetDayOfYear(ADate:TDateTime):word;
 { Gibt den Tag im Jahr aus einem Datums-Wert zurück }
function GetDaysPerMonth(AYear,AMonth:integer):integer;
 { Gibt die Anzahl Tage in einem Monat zurück }
function GetDaysPerYear(AYear:integer):integer;
 { Gibt die Anzahl Tage in einem Jahr zurück }
function GetFileSize(FileName:string;AFormat:TFileSizeFormat):integer;
 { Ermittelt die Größe der Datei "FileName" im Format "AFormat" }
function GetFirstDayOfWeek(ADate:TDateTime;DayIndex:integer):TDateTime;
 { Gibt den ersten Tag "DayOfWeek" der Woche zurück, in der das Datum "ADate" liegt.
   Ist DayOfWeek <= 0, wird der in FirstWeekday angegebene Tag als erster Wochentag gesetzt. }
function GetFirstDayOfWeekNr(AWeek,AYear:word;DayIndex:integer):TDateTime;
 { Gibt den ersten Tag "DayOfWeek" der Woche mit der Nummer "AWeek" im Jahr "AYear" zurück.
   Ist DayOfWeek <= 0, wird der in FirstWeekday angegebene Tag als erster Wochentag gesetzt. }
function GetFirstPartOfString(var AText:string;Delimiter:char;IncludeDelimiter:boolean):string;
 { Extrahiert aus einem String den ersten Teil bis zum Zeichen "Delimiter" und entfernt
   diesen Teil aus dem String "AText" }
function GetHourFromTime(ATime:TDateTime):byte;
 { Gibt die Stunde aus einem Zeit-Wert zurück }
function GetMinuteFromTime(ATime:TDateTime):byte;
 { Gibt die Minute aus einem Zeit-Wert zurück }
function GetMonthFromDate(ADate:TDateTime):word;
 { Gibt den Monat aus einem Datums-Wert zurück }
function GetNumberFromText(const AText:string):integer;
 { Gibt den Wert der ersten Ganzzahl einem String zurück }
function GetParentDir(const Directory:string):string;
 { Ermittelt das übergeordnete Verzeichnis von "Directory" }
function GetSecondFromTime(ATime:TDateTime):byte;
 { Gibt die Sekunde aus einem Zeit-Wert zurück }
function GetShiftState:TShiftState;
 { Ermittelt den Zustand der Shift-, Alt- und Ctrl-Tasten }
function GetSystemDir:string;
 { Ermittelt das Windows-System-Verzeichnis }
function GetTextInWindow(const AWnd:HWnd):string;
 { Gibt den Text des Fensters mit dem Handle AWnd zurück }
function GetVersionNr(ExeName:string;BuildNr:boolean):string;
 { Generiert einen Versionsnummern-string zu einer Exe- oder Dll-Datei }
function GetWeekOfYear(ADate:TDateTime):byte;
 { Gibt die Woche im Jahr aus einem Datums-Wert zurück }
function GetWeeksPerYear(AYear:word):byte;
 { Gibt die Wochenzahl der letzten Woche im Jahr "AYear" zurück }
function GetWindowCaption(const AWnd:HWnd):string;
 { Gibt den Titeltext des Fensters mit dem Handle AWnd zurück
   (kapselt die API-Funktion "GetWindowText") }
function GetWindowClass(const AWnd:HWnd):string;
 { Gibt den Klassennamen des Fensters mit dem Handle AWnd zurück
   (kapselt die API-Funktion "GetClassname") }
function GetWindowsDir:string;
 { Ermittelt das Windows-Verzeichnis }
function GetYearFromDate(ADate:TDateTime):word;
 { Gibt das Jahr aus einem Datums-Wert zurück }
function IntToStrFixed(IntValue:integer;OutDigits:byte):string;
 { Umwandlung Int->String mit fester Stellenzahl und führenden Nullen }
function IsNumber(const AText:string):boolean;
 { Überprüft, ob es sich bei "AText" um eine gültige Zahlenangabe handelt }
function IsSummertime(ADate:TDateTime):boolean;
 { Ermmittelt, ob ein Datum in der Sommerzeit liegt }
function IsValidMailAdress(const AText:string):boolean;
 { Prüft, ob "AText" eine gültige E-Mail-Adresse darstellt }
function LowCase(const AChar:char):char;
 { Wandelt den Buchstaben "AChar" in einen Kleinbuchstaben um }
function PosN(const AText,S:string;N:integer):integer;
 { Entspricht der Pos()-Funktion, sucht aber erst ab der Textposition "N" }
function ReplaceEntities(const AText:string):string;
 { Wandelt alle HTML-Entities in AText in Sonderzeichen um}
function ReplaceHighChars(const AText:string):string;
 { Wandelt alle Sonderzeichen in AText in HTML-Entities um}
function ReverseString(AText:string):string;
 { Spiegelt einen String, die Buchstabenfolge wird umgedreht }
procedure RewrapText(AFrom,ATo:TStrings;const MaxLineLength:integer;WrapInWord:boolean);
 { Der Text "AFrom" wird nach "MaxLineLength" Zeichen neu umbrochen und in "ATo"
   zurückgegeben. Wenn WrapInWord=true, wird auch mitten im Wort umbrochen }
function RGBToStr(RGBColor:integer):string;
 { Umwandlung Windows-RGB-Wert -> HTML-RGB-Wert }
function StripForbiddenChars(const AText:string):string;
 { Entfernt für Dateinamen nicht erlaubte Zeichen aus einem String }
function StrToDateDef(S:string;Def:TDateTime):TDateTime;
 { Umwandlung String->Date mit Rückgabe eines Default-Wertes bei Fehlern }
function StrToDateTimeDef(S:string;Def:TDateTime):TDateTime;
 { Umwandlung String->DateTime mit Rückgabe eines Default-Wertes bei Fehlern }
function StrToFloatDef(S:string;Def:extended):extended;
 { Umwandlung String->Extended mit Rückgabe eines Default-Wertes bei Fehlern }
function StrToTimeDef(S:string;Def:TDateTime):TDateTime;
 { Umwandlung String->Time mit Rückgabe eines Default-Wertes bei Fehlern }
function TimeToStrDef(ADate:TDateTime;Default:string):string;
 { Umwandlung Time->String mit Rückgabe eines Default-Wertes bei Fehlern }
function TrimChars(const AChar:char;const AText:string):string;
 { Entfernt alle Zeichen "AChar" von Anfang und Ende des Strings "AText" }
function ValidFileName(DName:string):boolean;
 { Ermittelt, ob es sich um einen gültigen Dateinamen handelt }


{---------------------------------------}
{ Funktionen nur für Delphi 1:          }
{---------------------------------------}

{$IFDEF SR_Delphi1}
procedure DrawEdge(ACanvas:TCanvas;ARect:TRect;Raised:boolean);
 { Zeichnet einen 3D-Rahmen auf der Zeichenfläche ACanvas }
procedure SetFileDate(FName:string;FDate:LongInt);
 { Setzt das Erstellungs-Datum einer Datei }
function Trim(const AText:string):string;
 { Entfernt führende und abschließende Leerzeichen aus einem String }
{$ENDIF}


{---------------------------------------}
{ Funktionen nur für alle 32Bit-        }
{ Delphi-Versionen                      }
{---------------------------------------}

{$IFDEF SR_Delphi2_Up}
function ConvertStrToDateTime(s:String):TDateTime;
 { Versucht, einen String in einen Datumswert zu wandeln
   (zuvor muß InitLocale aufgerufen werden) }
function CreateUniqueFileName(const FileName:string;const StartIndex:word):string;
 { Erstellt aus "FileName" per Inidzierung einen eindeutigen Dateinamen }
function GetAppDataPath(const Common,Local:boolean):string;
 { Ermittelt den Systemordner für Anwendungsdaten }
function FindAssociatedProgram(DateiName:String):String;
 { Ermittelt das mit einer Dateierweiterung verknüpfte Programm }
function GetExeForProtocol(URL:string):string;
 { Ermittelt das mit einem Übertragungs-Protokoll verknüpfte Programm }
function GetFileDateTime(const FName:string;const WhatTime:byte):TDateTime;
 { Ermittelt das Erstellungs-, Zugriffs- oder Änderungsdatum einer Datei (siehe ft*-Konstanten) }
function GetFolder(const Root:integer;const Caption,InitialDir:string):string;
 { Kapselt die API-Funktion SHBrowseForFolder }
function GetFocussedControl:HWnd;
 { Ermittelt das Fensterelement mit dem Eingabefokus }
function GetFullNodePath(Node:TTreeNode;const Divider:char;const IncludeRoot:boolean):string;
 { Gibt den vollen Pfad eines TreeNodes zurück }
function GetLocalComputerName:string;
 { Ermittelt den Netzwerknamen des Computers (Wrapper für die API-Funktion GetComputerName) }
function GetLongPathName(APath:String):String;
 { Wandelt einen verkürzten DOS-Dateinamen in einen langen Windows9x-Dateinamen }
function GetProgramFilesDir:string;
 { Gibt das Verzeichnis für Programme (Standard: "C:\Programme") zurück }
function GetSystemFileDescription(FileName:string):string;
 { Liefert die in Windows registrierte Dateibeschreibung zu einem Dateinamen zurück }
function GetSystemFolderLocation(const FolderID:integer):string;
 { Gibt den Pfad zum mit der ID "ID" Systemordner zurück }
function GetSystemWorkArea:TRect;
 { Gibt das Windows-Desktop-Rechteck ohne die Taskbar zurück }
function GetTempDir:string;
 { Gibt das Verzeichnis für temporäre Dateien zurück (Wrapper für die API-Funktion GetTempPath) }
function GetWindowState(WHandle:HWnd):integer;
 { Gibt den Anzeige-Zustand des Fenster mit dem Handle "WHandle" zurück }
function GetWinUsername:string;
 { Ermittelt den aktuell angemeldeten Windows-Benutzer (Wrapper für die API-Funktion GetUserName) }
procedure InitLocale;
 { Ermittelt die aktuellen Lokalisierungseinstellungen
   (muß vor  ConvertStrToDateTime aufgerufen werden) }
function IsWindowsNT:boolean;
 { Ermittelt ob es sich bei dem Betriebssystem um eine Windows-NT-Version handelt }
function Like(const AString, APattern: String): Boolean;
 { Prüft, ob der Dateiname "AString" der Maske "APattern" entspricht }
procedure SendKeys(AText:string);
 { Sendet einen String als Folge von Tastendrücken an ein Fensterelement }
function SetFileDate(FName:string;FDate:Integer):boolean;
 { Setzt das Erstellungs-Datum einer Datei, wird seit Version 1.41 ersetzt durch SetFileDateTime }
function SetFileDateTime(const FName:string;const ATime:TDateTime;const WhatTime:byte):boolean;
 { Setzt das Erstellungs-, Zugriffs- oder Änderungsdatum einer Datei (siehe ft*-Konstanten) }
procedure SimulateKeyDown(Key : byte);
 { Sendet eine KeyDown-Nachricht an ein Fensterelement }
procedure SimulateKeystroke(Key:byte; extra:DWORD);
 { Sendet einen vollständigen Tatendruck (KeyDown+KeyUp) an ein Fensterelement }
procedure SimulateKeyUp(Key : byte);
 { Sendet eine KeyUp-Nachricht an ein Fensterelement }
{$ENDIF}


{---------------------------------------}
{ Funktionen nur für bestimmte          }
{ Delphi-Versionen                      }
{---------------------------------------}

{$IFNDEF SR_Delphi4_Up}
procedure FreeAndNil(var Obj);
 { Gibt ein Objekt frei und setzt den Objektzeiger auf NIL (Delphi 1..3) }
function StringReplace(const S,OldPattern,NewPattern:string;const Flags:TReplaceFlags):string;
 { Ersetzt den Text "OldPattern" duch den Text "NewPattern" im String "S" }
{$ENDIF}
{$IFNDEF SR_Delphi3_Up}
function ExtractFileDir(APath:string):string;
 { Gibt wie ExtractFilePath den Pfad eines Dateinamens zurück,
  aber ohne abschließenden Backslash }
function IsLeapYear(AYear: Integer):boolean;
 { Ermittelt, ob ein Jahr ein Schaltjahr ist (Delphi 1..2) }
function LastDelimiter(AChars,AText:string):integer;
 { Ermittelt die letzte Position des Zeichens AChar im string AText (Delphi 1..2) }
{$ENDIF}
{$IFDEF SR_Delphi4_Up}
function GetDiskSpace(const RootDir:string;const AType:TDiskSpaceType;const Format:TFileSizeFormat):Int64;
 { Gibt den belegten, freien oder Gesamtplatz einer Partition zurück }
function GetTreeSize(const RootFolder:string;const Format:TFileSizeFormat):Int64;
 { Gibt die Gesamtgröße eines Verzeichnisbaums zurück }
{$ENDIF}

implementation

uses SysUtils, Messages, ShellAPI
     {$IFDEF SR_Delphi2_Up}, Registry, ShlObj
     {$ELSE}, Forms, Ver {$ENDIF};

const
  MaxChars = 89;
  Entities : array [0..MaxChars] of string[10] =
   ('&lt;', '&gt;', '&amp;', '&quot;', '&nbsp;', '&Ccdil;', '&ccdil;', '&Ntilde;',
   '&ntilde;', '&THORN;', '&thorn;', '&Yacute;', '&yacute;', '&yuml;', '&szlig;',
   '&AElig;', '&Aacute;', '&Acirc;', '&Agrave;', '&Aring;', '&Atilde;', '&Auml;',
   '&aelig;', '&aacute;', '&acirc;', '&agrave;', '&aring;', '&atilde;', '&auml;',
   '&ETH;', '&Eacute;', '&Ecirc;', '&Egrave;', '&Euml;', '&eth;', '&eacute;',
   '&ecirc;', '&egrave;', '&euml;', '&Iacute;', '&Icirc;', '&Igrave;', '&Iuml;',
   '&iacute;', '&icirc;', '&igrave;', '&iuml;', '&Oacute;', '&Ocirc;', '&Ograve;',
   '&Oslash;', '&Otilde;', '&Ouml;', '&oacute;', '&ocirc;', '&ograve;', '&oslash;',
   '&otilde;', '&ouml;', '&Uacute;', '&Ucirc;', '&Ugrave;', '&Uuml;', '&uacute;',
   '&ucirc;', '&ugrave;', '&uuml;', '&reg;', '&copy;', '&plusmn;', '&micro;',
   '&para;', '&middot;', '&cent;', '&pound;', '&yen;', '&frac14;', '&frac12;',
   '&frac34;', '&sup1;', '&sup2;', '&sup3;', '&iquest;', '&deg;', '&brvbar;',
   '&sect;', '&laquo;', '&raquo;', '&#132;', '&#147;');
  HighChars : array [0..MaxChars] of char =
   ('<', '>', '&', '"', '=', 'Ç', 'ç', 'Ñ', 'ñ', 'Þ', 'þ', 'Ý', 'ý', 'ÿ', 'ß', 'Æ', 'Á', 'Â',
   'À', 'Å', 'Ã', 'Ä', 'æ', 'á', 'â', 'à', 'å', 'ã', 'ä', 'Ð', 'É', 'Ê', 'È', 'Ë', 'ð', 'é',
   'ê', 'è', 'ë', 'Í', 'Î', 'Ì', 'Ï', 'í', 'î', 'ì', 'ï', 'Ó', 'Ô', 'Ò', 'Ø', 'Õ', 'Ö', 'ó',
   'ô', 'ò', 'ø', 'õ', 'ö', 'Ú', 'Û', 'Ù', 'Ü', 'ú', 'û', 'ù', 'ü', '®', '©', '±', 'µ', '¶',
   '·', '¢', '£', '¥', '¼', '½', '¾', '¹', '²', '³', '¿', '°', '¦', '§', '«', '»', '“', '”');

var
  {$IFDEF SR_Delphi2_Up}
  FirstWeekDay      : Integer = 2;  { Wochentag, mit dem die Woche beginnt
                                     (siehe Delphi-Wochentage)
                                      2 : Montag (nach DIN 1355) }
  FirstWeekDate     : Integer = 4;  { 1 : Beginnt am ersten Januar
                                      4 : Erste-4 Tage-Woche (nach DIN 1355)
                                      7 : Erste volle Woche }
  bffInitialFolder  : string;
  {$ELSE}
  FirstWeekDay      : Integer;
  FirstWeekDate     : Integer;
  {$ENDIF}
  LocaleIDate,
  LocaleILDate,
  CurrentYear2Digit,
  CurrentCentury    : Integer;

{$IFDEF SR_Delphi4_Up}
function GetDiskFreeSpaceEx(lpDirectoryName: PAnsiChar;
                            var lpFreeBytesAvailableToCaller : Int64;
                            var lpTotalNumberOfBytes: Int64;
                            var lpTotalNumberOfFreeBytes: Int64) : boolean;
                            stdcall; external kernel32 name 'GetDiskFreeSpaceExA';
{$ENDIF}

function AddBackslash(Filename:string):string;
begin
  if (length(Filename)>0) and (Filename[length(Filename)]<>'\') then
    Result:=Filename+'\'
  else
    Result:=Filename;
end; {AddBackslash}

{$IFDEF SR_Delphi2_Up}
function bffCallback(DlgHandle: HWND;Msg: Integer;lParam: Integer;lpData: Integer):Integer; stdcall;
var AFolder : string;
begin
  {Callback-Handler für SHBrowseForFolder}
  Result:=0;
  {vorgewähltes Verzeichnis übermitteln}
  AFolder:=bffInitialFolder;
  if Msg = BFFM_INITIALIZED then
    SendMessage(DlgHandle, BFFM_SETSELECTION, Integer(LongBool(True)),
                Integer(PChar(AFolder)));
end; {bffCallback}

function ConvertStrToDateTime(s:String):TDateTime;
var
  p,p2     : PChar;
  i1,i2,i3 : Integer;    { einzelne Datumsangaben }
  Mode     : Integer;    { Reihenfolge beim Datum }
  t1,t2    : String;     { Separator }
  t        : String;     { Zeit }

  function GetNumber:Integer;
  var s : String;
  begin
    p:=p2;
    while p2^ in ['0'..'9'] do
      Inc(p2);
    SetString(s,p,p2-p);
    Result:=StrToIntDef(s,-1);
  end; {GetNumber}

  function GetSeparator:String;
  begin
    p:=p2;
    while Not (p2^ in ['0'..'9',#0]) do
      Inc(p2);
    SetString(Result,p,p2-p);
    Result:=Trim(Result);
  end; {GetSeparator}

  procedure ConvertTo4Digit(var AYear:Integer);
  begin
    if AYear in [0..100] then begin
      if AYear>CurrentYear2Digit then
        Dec(AYear,100);
      Inc(AYear,CurrentCentury);
    end;
  end; {ConvertTo4Digit}

begin
  Result:=0;
  p:=Pointer(s);
  if p=Nil then
    Exit;
  p2:=p;
  i1:=GetNumber;
  t1:=GetSeparator;
  i2:=GetNumber;
  t2:=GetSeparator;
  i3:=GetNumber;
  SetString(t,p2,StrLen(p2));
  t:=Trim(t);
  Mode:=-1;
  if (i1<1) or (i1>31) then           { y/m/d }
    Mode:=2
  else begin
    if (i3<1) or (i3>31) then begin   { x/x/y }
      if Not (i1 in [1..31]) then     { m/d/y }
        Mode:=0
      else
        if (Not (i2 in [1..31])) or (i1<1) or (i1>12) then   { d/m/y }
          Mode:=1;
    end
    else
      if i1=i2 then                   { Tag=Monat, Format egal }
        Mode:=1;
  end;
  if Mode<0 then begin                { Format nicht auswertbar }
    if LocaleIDate in [0..1] then
      Mode:=LocaleIDate               { Reihenfolge kurzes Datum }
    else begin
      if LocaleILDate in [0..1] then
        Mode:=LocaleILDate            { Reihenfolge langes Datum }
      else                            // evtl. User befragen
        Mode:=1;
    end;
  end;
  // Jahr auf vierstellig bringen
  case Mode of
    0..1 : ConvertTo4Digit(i3);
    2    : ConvertTo4Digit(i1);
  end;
  // Datum konvertieren
  case Mode of
    0 : Result:=EncodeDate(i3,i1,i2);
    1 : Result:=EncodeDate(i3,i2,i1);
    2 : Result:=EncodeDate(i1,i2,i3);
  end;
  if Length(t)>0 then
    Result:=Result+StrToTime(t);
end; {ConvertStrToDateTime}

function CreateUniqueFileName(const FileName:string;const StartIndex:word):string;
var AIndex  : integer;
    AFolder,
    RawName,
    FileExt : string;
begin
  AFolder:=ExtractFilePath(FileName);
  FileExt:=ExtractFileExt(FileName);
  RawName:=ExtractRawFileName(FileName);
  AIndex:=StartIndex;
  repeat
    if AIndex=0 then
      Result:=AddBackSlash(AFolder)+RawName+FileExt
    else
      Result:=AddBackSlash(AFolder)+RawName+IntToStr(AIndex)+FileExt;
    inc(AIndex);
  until not FileExists(Result);
end; {CreateUniqueFileName}
{$ENDIF}

function ANSILowCase(const AChar:char):char;
var Temp : string;
begin
  Temp:=ANSILowerCase(AChar);
  if Temp<>'' then
    Result:=Temp[1]
  else
    Result:=AChar;
end; {ANSILowCase}

function ANSIUpCase(const AChar:char):char;
var Temp : string;
begin
  Temp:=ANSIUpperCase(AChar);
  if Temp<>'' then
    Result:=Temp[1]
  else
    Result:=AChar;
end; {ANSIUpCase}

function CapitalizeString(const AText:string):string;
const BMenge : set of char = ['a'..'z', 'A'..'Z', 'ä', 'ö', 'ü', 'Ä', 'Ö', 'Ü', 'ß'];
var i,L : integer;
    Buf : string;
begin
  Buf:='';
  if AText<>'' then begin
    L:=length(AText);
    for i:=1 to L do
      if (i=1) or ((i>1) and not (AText[i-1] in BMenge)) then
        Buf:=Buf+ANSIUpCase(AText[i])
      else
        Buf:=Buf+ANSILowCase(AText[i]);
  end;
  Result:=Buf;
end; {CapitalizeString}

function CompareNumbers(const Number1,Number2:double):ShortInt;
begin
  if Number1=Number2 then
    Result:=0
  else begin
    if Number1<Number2 then
      Result:=-1
    else
      Result:=1;
  end;
end; {CompareNumbers}

function CutBackSlash(FileName:string):string;
begin
  if (length(FileName)>0) and (FileName[length(FileName)]='\') then
    Result:=copy(FileName,1,length(FileName)-1)
  else
    Result:=FileName;
end; {CutBackSlash}

function CutRootDirectory(FName:string):string;
var P : integer;
begin
  P:=Pos(':',FName);
  if (P>0) and (P<length(FName)) then
    delete(FName,1,P+1);
  Result:=FName;
end; {CutRootDirectory}

function DateTimeToStrDef(ADate:TDateTime;Default:string;CompareToday:boolean):string;
var DayDiff : integer;
begin
  try
    Result:='';
    if CompareToday then begin
      DayDiff:=trunc(Date)-trunc(ADate);
      if (abs(DayDiff))<=2 then
        Result:=PeriodNames[DayDiff+2]+', '+TimeToStr(frac(ADate));
    end;
    if Result='' then
      Result:=DateTimeToStr(ADate);
  except
    Result:=Default;
  end;
end; {DateTimeToStrDef}

function DateToStrDef(ADate:TDateTime;Default:string;CompareToday:boolean):string;
var DayDiff : integer;
begin
  try
    Result:='';
    if CompareToday then begin
      DayDiff:=trunc(Date)-trunc(ADate);
      if (abs(DayDiff))<=2 then
        Result:=PeriodNames[DayDiff+2];
    end;
    if Result='' then
      Result:=DateToStr(ADate);
  except
    Result:=Default;
  end;
end; {DateToStrDef}

{$IFDEF SR_Delphi1}
procedure DrawEdge(ACanvas:TCanvas;ARect:TRect;Raised:boolean);
begin
  with ACanvas do begin
    if Raised then
      Pen.Color:=clBtnHighlight
    else
      Pen.Color:=clBtnShadow;
    MoveTo(ARect.Right-1,ARect.Top);
    LineTo(ARect.Left,ARect.Top);
    LineTo(ARect.Left,ARect.Bottom-2);
    if Raised then
      Pen.Color:=clBtnShadow
    else
      Pen.Color:=clBtnHighlight;
    MoveTo(ARect.Left,ARect.Bottom-2);
    LineTo(ARect.Right-1,ARect.Bottom-2);
    LineTo(ARect.Right-1,ARect.Top);
    Pen.Color:=clWindowFrame;
    MoveTo(ARect.Left,ARect.Bottom-1);
    LineTo(ARect.Right,ARect.Bottom-1);
    LineTo(ARect.Right,ARect.Top);
  end;
end; {DrawEdge}
{$ENDIF}

function ExecAndWait(const Filename, Params: string;
                     WindowState: word): boolean;
{$IFDEF SR_Delphi2_Up}
var
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: string;
begin
  { Enclose filename in quotes to take care of
    long filenames with spaces. }
  CmdLine := '"' + Filename + '" ' + Params;
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := WindowState;
  end;
  Result := CreateProcess(NIL, PChar(CmdLine), NIL, NIL, FALSE, 
                          CREATE_NEW_CONSOLE or 
                          NORMAL_PRIORITY_CLASS, NIL, 
                          PChar(ExtractFilePath(Filename)), 
                          SUInfo, ProcInfo);
  { Wait for it to finish. }
  if Result then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
 
{$ELSE}
var
  InstanceID : THandle;
  Buff: array[0..255] of char;
begin
  StrPCopy(Buff, Filename + ' ' + Params);
  InstanceID := WinExec(Buff, WindowState);
  if InstanceID < 32 then
  { a value less than 32 indicates an Exec error }
    Result := FALSE
  else begin
    Result := TRUE;
    repeat
      Application.ProcessMessages;
    until Application.Terminated or
          (GetModuleUsage(InstanceID) = 0);
  end;
{$ENDIF}
end;

function ExpandString(S:string;AChar:char;ALength:word):string;
begin
  while length(S)<ALength do
    S:=AChar+S;
  Result:=S;
end; {ExpandString}

{$IFNDEF SR_Delphi3_Up}
function ExtractFileDir(APath:string):string;
begin
  Result:=CutBackslash(ExtractFilePath(APath));
end; {ExtractFileDir}
{$ENDIF}

procedure ExtractItemsFromString(AFrom:string;ATo:TStrings;const Delimiter:string);
var i : integer;
begin
  repeat
    i:=Pos(Delimiter, AFrom);
    if i=0 then
      ATo.Add(TrimChars('"', Trim(AFrom)))
    else begin
      ATo.Add(TrimChars('"', Trim(copy(AFrom, 1, i-1))));
      delete(AFrom, 1, i+length(Delimiter)-1);
    end;
  until i=0;
end; {ExtractItemsFromString}

function ExtractNumberFromText(const AText:string;const Digits:byte):extended;
const ZMenge : set of char = ['0'..'9'];
var i,Len,APos,
    NumDigits  : integer;
    Buf        : string;
    CommaFlag  : boolean;
begin
  Len:=length(AText);
  Buf:='';
  CommaFlag:=false;
  NumDigits:=0;
  for i:=1 to Len do begin
    if (AText[i]='-') and (Buf='') then
      Buf:='-';
    if ((AText[i]=',') or (AText[i]='.')) and not CommaFlag then begin
      if Digits>0 then
        Buf:=Buf+'.';
      CommaFlag:=true;
    end;
    if (AText[i] in ZMenge) and (NumDigits<Digits) then begin
      Buf:=Buf+AText[i];
      if CommaFlag then
        inc(NumDigits);
    end;
  end;
  if Buf<>'' then begin
    try
      Val(Buf, Result, APos);
    except
      Result:=0;
    end;
  end
  else
    Result:=0;
end; {ExtractNumberFromText}

function ExtractRawFileName(DName:string):string;
begin
  Result:=ChangeFileExt(ExtractFileName(DName),'');
end; {ExtractRawFileName}

{$IFDEF SR_Delphi2_Up}
function FindAssociatedProgram(DateiName:String):String;
var Reg  : TRegistry;
    Res  : boolean;
    AKey : string;
    i    : integer;

  function ReplaceRegistryPathVars(const AText:string):string;
  var P,Q      : integer;
      PathVar,
      RealPath : string;

    function GetProgramFilesPathVar:string;
    var Reg : TRegistry;
        Res : boolean;
    begin
      Result:='%ProgramFiles%';
      Reg:=TRegistry.Create;
      try
        Reg.Rootkey:=HKEY_LOCAL_MACHINE;
        Res:=Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion',false);
        if Res then begin
          Result:=Reg.ReadString('ProgramFilesPath');
          Reg.CloseKey;
        end;
      finally
        Reg.Free;
      end;
    end; {GetProgramFilesPathVar}

  begin
    Result:=AText;
    P:=Pos('%', AText);
    if P>0 then begin
      Q:=PosN('%', AText, P+1);
      PathVar:=LowerCase(copy(AText, P, Q-P+1));
      RealPath:='???';
      if PathVar=LowerCase(GetProgramFilesPathVar) then
        RealPath:=GetProgramFilesDir;
      if PathVar='%systemroot%' then
        RealPath:=GetWindowsDir;
      Result:=copy(AText, 1, P-1)+CutBackSlash(RealPath)+copy(AText, Q+1, length(AText)-Q);
    end;
  end; {ReplaceRegistryPathVars}

begin
  Result:='';
  {$IFDEF SR_Delphi5_Up}
  Reg := TRegistry.Create(Key_Read);
  {$ELSE}
  Reg := TRegistry.Create;
  {$ENDIF}
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Res:=Reg.OpenKey(ExtractFileExt(DateiName), false);
    if Res then begin
      AKey:=Reg.ReadString('');
      Reg.CloseKey;
      if AKey<>'' then begin
        Res:=Reg.OpenKey(AKey+'\shell\open\command', false);
        if Res then begin
          Result:=Reg.ReadString('');
          for i:=length(Result) downto 1 do
            if Result[i]='"' then
              delete(Result, i, 1);
          i:=Pos(LowerCase('.exe'), LowerCase(Result));
          if i>0 then
            delete(Result, i+4, length(Result)-i-3);
          Result:=GetLongPathName(ReplaceRegistryPathVars(Result));
          Reg.CloseKey;
        end;
      end;
    end;
  finally
    Reg.Free;
  end;
end; {FindAssociatedProgram}
{$ENDIF}

function FindChildWindow(const OwnerWnd:HWnd;const AClass,AText:string;const ExactMatch:boolean):HWnd;
var AWnd,
    DeskHnd   : HWnd;
    WndClass,
    WndText   : string;
begin
  Result:=0;
  { Kinder von "OwnerWnd" abklappern }
  AWnd:=GetWindow(OwnerWnd, GW_Child);
  while (AWnd>0) and (Result=0) do begin
    WndClass:=GetWindowClass(AWnd);
    WndText:=GetWindowCaption(AWnd);
    if ((UpperCase(AClass)=UpperCase(WndClass)) or (AClass='')) and
     ((UpperCase(AText)=UpperCase(WndText)) or (AText='')) then
      Result:=AWnd;
    AWnd:=GetWindow(AWnd, GW_HWndNext);
  end;
  if Result=0 then begin
    { Kinder des Desktops mit Owner "OwnerWnd" abklappern }
    DeskHnd:=GetDesktopWindow;
    AWnd:=GetWindow(DeskHnd, GW_Child);
    while (AWnd>0) and (Result=0) do begin
      if GetWindow(AWnd, GW_Owner)=OwnerWnd then begin
        WndClass:=GetWindowClass(AWnd);
        WndText:=GetWindowCaption(AWnd);
        if ((UpperCase(AClass)=UpperCase(WndClass)) or ((AClass='') and not ExactMatch)) and
         ((UpperCase(AText)=UpperCase(WndText)) or ((AText='') and not ExactMatch)) then
          Result:=AWnd;
      end;
      AWnd:=GetWindow(AWnd, GW_HwndNext);
    end;
  end;
end; {FindChildWindow}

function FindExecutableStr(const Filename:string):string;
var Buf   : array [0..Max_Path] of char;
    {$IFNDEF SR_Delphi2_Up}
    FName : PChar;
    {$ENDIF}
begin
  {$IFDEF SR_Delphi2_Up}
  FindExecutable(PChar(FileName), nil, Buf);
  Result:=GetLongPathName(string(Buf));
  {$ELSE}
  FName:=StrAlloc(Max_Path);
  try
    StrPCopy(FName, FileName);
    FindExecutable(FName, nil, Buf);
  finally
    StrDispose(FName);
  end;
  Result:=string(Buf);
  {$ENDIF}
end; {FindExecutableStr}

function FirstDelimiter(AChars,AText:string):integer;
var i,j : integer;
begin
  Result:=0;
  if (length(AChars)=0) or (length(AText)=0) then
    Exit;
  for i:=1 to length(AText) do begin
    for j:=1 to length(AChars) do begin
      if AText[i]=AChars[j] then begin
        Result:=i;
        Exit;
      end;
    end;
  end;
end; {FirstDelimiter}

{$IFNDEF SR_Delphi4_Up}
procedure FreeAndNil(var Obj);
var P : TObject;
begin
  P:=TObject(Obj);
  TObject(Obj):=nil;
  P.Free;
end; {FreeAndNil}
{$ENDIF}

{$IFDEF SR_Delphi2_Up}
function GetAppDataPath(const Common,Local:boolean):string;
begin
  if Common then
    {Anwendungsdaten für alle Benutzer}
    Result:=GetSystemFolderLocation(CSIDL_COMMON_APPDATA)
  else begin
    {Benutzerabhängige Anwendungsdaten}
    if Local then
      {Nur auf dem lokalen Rechner}
      Result:=GetSystemFolderLocation(CSIDL_LOCAL_APPDATA)
    else
      {Auf allen Rechnern mit diesem Benutzerkonto im Netz}
      Result:=GetSystemFolderLocation(CSIDL_APPDATA);
  end;
end; {GetAppDataPath}
{$ENDIF}

function GetBuildInfo(const AFilename:String; var V1,V2,V3,V4:Word):Boolean;
var
  VerInfoSize  : Integer;
  {$IFDEF SR_Delphi2_Up}
  VerValueSize : DWord;
  Dummy        : DWord;
  VerValue     : PVSFixedFileInfo;
  {$ELSE}
  VerValueSize : Word;
  Dummy        : LongInt;
  VerValue     : ^TVS_FixedFileInfo;
  {$ENDIF}
  VerInfo      : Pointer;
  FName        : PChar;
begin
  FName:=StrAlloc(Max_Path);
  try
    StrPCopy(FName, AFileName);
    VerInfoSize:=GetFileVersionInfoSize(FName, Dummy);
    Result:=False;
    if VerInfoSize>0 then begin
      GetMem(VerInfo,VerInfoSize);
      try
        if GetFileVersionInfo(FName,0,VerInfoSize,VerInfo) then begin
          if VerQueryValue(VerInfo,'\',Pointer(VerValue),VerValueSize) then
           with VerValue^ do begin
            V1:=dwFileVersionMS shr 16;
            V2:=dwFileVersionMS and $FFFF;
            V3:=dwFileVersionLS shr 16;
            V4:=dwFileVersionLS and $FFFF;
          end;
          Result:=True;
        end;
      finally
        FreeMem(VerInfo,VerInfoSize);
      end;
    end;
  finally
    StrDispose(FName);
  end;
end; {GetBuildInfo}

function GetDayFromDate(ADate:TDateTime):word;
var Y,M,D : word;
begin
  try
    Decodedate(ADate, Y, M, D);
  except
    D:=0;
  end;
  Result:=D;
end; {GetDayFromDate}

function GetDayOfYear(ADate:TDateTime):word;
{ liefert den Tag im Jahr }
var T,M,J  : word;
    Erster : TDateTime;
begin
  try
    DecodeDate(ADate,J,M,T);
    Erster:=EncodeDate(J,1,1);
    Result:=trunc(ADate-Erster+1);
  except
    Result:=0;
  end;
end; {GetDayOfYear}

function GetDaysPerMonth(AYear,AMonth:integer):integer;
const
  DaysInMonth: array [1..12] of Integer =
   (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result:=DaysInMonth[AMonth];
  if (AMonth=2) and IsLeapYear(AYear) then
    Inc(Result);
end; {GetDaysPerMonth}

function GetDaysPerYear(AYear:integer):integer;
var ADate : TDateTime;
begin
  try
    ADate:=EncodeDate(AYear, 12, 31);
    Result:=GetDayOfYear(ADate);
  except
    Result:=0;
  end;
end; {GetDaysPerYear}

{$IFDEF SR_Delphi4_Up}
function GetDiskSpace(const RootDir:string;const AType:TDiskSpaceType;const Format:TFileSizeFormat):Int64;
var TotalBytes,
    TotalFree,
    TotalDir   : Int64;
begin
 Result:=0;
 if GetDiskFreeSpaceEx(PChar(RootDir), TotalDir, TotalBytes, TotalFree) then begin
   case AType of
     dsTotal : Result:=TotalBytes;
     dsFree  : Result:=TotalFree;
     dsUsed  : Result:=TotalBytes-TotalFree;
   end;
   if Format>=fsKilobyte then
     Result:=Result div 1024;
   if Format=fsMegabyte then
     Result:=Result div 1024;
 end;
end; {GetDiskSpace}

function GetTreeSize(const RootFolder:string;const Format:TFileSizeFormat):Int64;
var ASize : Int64;

  procedure GetSubtreeSize(const AFolder:string);
  var SR  : TSearchRec;
      Res : integer;
  begin
    Res:=FindFirst(AFolder+'*', faAnyFile, SR);
    if Res=0 then begin
      repeat
        if (SR.Attr and faDirectory)>0 then begin
          if  (SR.Name<>'.') and (SR.Name<>'..') then
            GetSubtreeSize(AddBackSlash(AFolder+SR.Name));
        end
        else
          ASize:=Asize+SR.Size;
      until FindNext(SR)<>0;
      FindClose(SR);
    end;
  end; {GetSubtreeSize}

begin
  ASize:=0;
  GetSubTreeSize(AddBackSlash(RootFolder));
  Result:=ASize;
 if Format>=fsKilobyte then
   Result:=Result div 1024;
 if Format=fsMegabyte then
   Result:=Result div 1024;
end; {GetTreeSize}
{$ENDIF}

{$IFDEF SR_Delphi2_Up}
function GetExeForProtocol(URL:string):string;
var Reg  : TRegistry;
    Res  : boolean;
    Temp : string;
    P    : integer;

  function ReplaceRegistryPathVars(const AText:string):string;
  var P,Q      : integer;
      PathVar,
      RealPath : string;

    function GetProgramFilesPathVar:string;
    var Reg : TRegistry;
        Res : boolean;
    begin
      Result:='%ProgramFiles%';
      Reg:=TRegistry.Create;
      try
        Reg.Rootkey:=HKEY_LOCAL_MACHINE;
        Res:=Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion',false);
        if Res then begin
          Result:=Reg.ReadString('ProgramFilesPath');
          Reg.CloseKey;
        end;
      finally
        Reg.Free;
      end;
    end; {GetProgramFilesPathVar}

  begin
    Result:=AText;
    P:=Pos('%', AText);
    if P>0 then begin
      Q:=PosN('%', AText, P+1);
      PathVar:=LowerCase(copy(AText, P, Q-P+1));
      RealPath:='???';
      if PathVar=LowerCase(GetProgramFilesPathVar) then
        RealPath:=GetProgramFilesDir;
      if PathVar='%systemroot%' then
        RealPath:=GetWindowsDir;
      Result:=copy(AText, 1, P-1)+CutBackSlash(RealPath)+copy(AText, Q+1, length(AText)-Q);
    end;
  end; {ReplaceRegistryPathVars}

begin
  Result:='';
  P:=Pos(':', URL);
  if P>1 then
    delete(URL, P, length(URL)-P+1);
  {$IFDEF SR_Delphi5_Up}
  Reg := TRegistry.Create(Key_Read);
  {$ELSE}
  Reg := TRegistry.Create;
  {$ENDIF}
  try
    Reg.Rootkey:=HKEY_CLASSES_ROOT;
    Res:=Reg.OpenKey(URL+'\shell\open\command', false);
    if Res then begin
      Temp:=Reg.ReadString('');
      while (length(Temp)>0) and ((Temp[1]='"') or (Temp[1]=' ')) do
        delete(Temp, 1, 1);
      P:=Pos('"', Temp);
      if P>0 then
        delete(Temp, P, length(Temp)-P+1);
      Result:=ReplaceRegistryPathVars(Temp);
      P:=Pos(' -', Result);
      if P>0 then
        Result:=Trim(copy(Result, 1, P-1)); 
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end; {GetExeForProtocol}
{$ENDIF}

function GetFileSize(FileName:string;AFormat:TFileSizeFormat):integer;
var SR : TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, SR)=0 then begin
    Result:=SR.Size;
    if AFormat=fsKilobyte then
      Result:=Result div 1024;
    if AFormat=fsMegabyte then
      Result:=Result div (1024*1024);
    FindClose(SR);
  end
  else
    Result:=-1;
end; {GetFileSize}

{$IFDEF SR_Delphi2_Up}
function GetFileDateTime(const FName:string;const WhatTime:byte):TDateTime;
var FileTime,
    LocalFileTime : TFileTime;
    DosDateTime,
    FHandle       : integer;
begin
  Result:=-1;
  FHandle:=FileOpen(FName, fmOpenRead);
  if (FHandle<>-1) then begin
    case WhatTime of
      ftCreation :
        if GetFileTime(THandle(FHandle), @FileTime, nil, nil) and
         FileTimeToLocalFileTime(FileTime, LocalFileTime) then
          FileTimeToDosDateTime(LocalFileTime, LongRec(DosDateTime).Hi, LongRec(DosDateTime).Lo);
      ftLastAccess :
        if GetFileTime(THandle(FHandle), nil, @FileTime, nil) and
         FileTimeToLocalFileTime(FileTime, LocalFileTime) then
          FileTimeToDosDateTime(LocalFileTime, LongRec(DosDateTime).Hi, LongRec(DosDateTime).Lo);
      else // ftLastWrite: wie FileAge()
        if GetFileTime(THandle(FHandle), nil, nil, @FileTime) and
         FileTimeToLocalFileTime(FileTime, LocalFileTime) then
          FileTimeToDosDateTime(LocalFileTime, LongRec(DosDateTime).Hi, LongRec(DosDateTime).Lo);
    end;
    FileClose(FHandle);
    Result:=FileDateToDateTime(DosDateTime);
  end;
end; {GetFileDateTime}
{$ENDIF}

function GetFirstDayOfWeek(ADate:TDateTime;DayIndex:integer):TDateTime;
begin
  if DayIndex<1 then
    DayIndex:=FirstWeekDay;
  while DayOfWeek(ADate)<>DayIndex do
    ADate:=ADate-1;
  Result:=ADate;
end; {GetFirstDayOfWeek}

function GetFirstDayOfWeekNr(AWeek,AYear:word;DayIndex:integer):TDateTime;
var ADate,
    FrstDate,
    Offset,
    FrstWeek : integer;
begin
  FrstDate:=trunc(EncodeDate(AYear, 1, 1));
  while GetWeekOfYear(FrstDate)<>1 do
    inc(FrstDate);
  FrstWeek:=GetWeekOfYear(FrstDate);
  Offset:=AWeek-FrstWeek+1;
  ADate:=FrstDate+(Offset*7)-1;
  if DayIndex<1 then
    DayIndex:=FirstWeekDay;
  while DayOfWeek(ADate)<>DayIndex do
    ADate:=ADate-1;
  Result:=ADate;
end; {GetFirstDayOfWeekNr}

function GetFirstPartOfString(var AText:string;Delimiter:char;IncludeDelimiter:boolean):string;
var P : integer;
begin
  P:=Pos(Delimiter,AText);
  if P>0 then begin
    if IncludeDelimiter then
      Result:=copy(AText,1,P)
    else
      Result:=copy(AText,1,P-1);
    delete(AText,1,P);
  end
  else begin
    Result:=AText;
    AText:='';
  end;
end; {GetFirstPartOfString}

function GetHourFromTime(ATime:TDateTime):byte;
var H,M,S,MS : word;
begin
  try
    DecodeTime(ATime, H, M, S, MS);
  except
    H:=0;
  end;
  Result:=H;
end; {GetHourFromTime}

function GetMinuteFromTime(ATime:TDateTime):byte;
var H,M,S,MS : word;
begin
  try
    DecodeTime(ATime, H, M, S, MS);
  except
    M:=0;
  end;
  Result:=M;
end; {GetMinuteFromTime}

{$IFDEF SR_Delphi2_Up}
function GetFolder(const Root:integer;const Caption,InitialDir:string):string;
var
  bi            : TBROWSEINFO;
  lpBuffer      : PChar;
  pidlPrograms,
  pidlBrowse    : PItemIDList;
begin
  if (not SUCCEEDED(SHGetSpecialFolderLocation(getactivewindow, Root, pidlPrograms))) then
    Exit;
  bffInitialFolder:=InitialDir;
  lpBuffer:=StrAlloc(max_path);
  bi.hwndOwner := getactivewindow;
  bi.pidlRoot := pidlPrograms;
  bi.pszDisplayName := lpBuffer;
  bi.lpszTitle := pChar(caption);
  bi.ulFlags := BIF_RETURNONLYFSDIRS;
  bi.lpfn := @bffCallback;
  bi.lParam := 0;
  pidlBrowse := SHBrowseForFolder(bi);
  if (pidlBrowse<>nil) then begin
    if (SHGetPathFromIDList(pidlBrowse, lpBuffer)) then
      Result:=lpBuffer;
  end;
  StrDispose(lpBuffer);
end; {GetFolder}

function GetFocussedControl:HWnd;
var OtherThreadID,
    Buffer        : DWord;
    ParentWnd     : HWnd;
begin
  Result:=0;
  ParentWnd:=GetForegroundWindow;
  if ParentWnd<>0 then begin
    OtherThreadID:=GetWindowThreadProcessID(ParentWnd, @Buffer);
    if AttachThreadInput(GetCurrentThreadID, OtherThreadID, true) then begin
      Result:=GetFocus;
      AttachThreadInput(GetCurrentThreadID, OtherThreadID, false);
    end;
  end;
end; {GetFocussedControl}

function GetFullNodePath(Node:TTreeNode;const Divider:char;const IncludeRoot:boolean):string;
begin
  if (Node=nil) or ((Node.Level=0) and not IncludeRoot) then
    Result:=''
  else begin
    Result:=Node.Text;
    while Node<>nil do begin
      Node:=Node.Parent;
      if (Node<>nil) and ((Node.Level>0) or IncludeRoot) then
        Result:=Node.Text+Divider+Result;
    end;
  end;
end; {GetFullNodePath}

function GetLocalComputerName:string;
var UName : PChar;
    USize : DWord;
begin
  USize:=Max_Path;
  UName:=StrAlloc(USize);
  try
    GetComputerName(UName,USize);
    Result:=string(UName);
  finally
    StrDispose(UName);
  end;
end; {GetLocalComputerName}

function GetLongPathName(APath:String):String;
var
  i : Integer;
  h : THandle;
  Data : TWin32FindData;
  IsBackSlash : Boolean;
begin
  APath:=ExpandFileName(APath);
  i:=Pos('\',APath);
  Result:=Copy(APath,1,i);
  Delete(APath,1,i);
  repeat
    i:=Pos('\',APath);
    IsBackSlash:=i>0;
    if Not IsBackSlash then
      i:=Length(APath)+1;
    h:=FindFirstFile(PChar(Result+Copy(APath,1,i-1)),Data);
    if h<>INVALID_HANDLE_VALUE then begin
      try
        Result:=Result+Data.cFileName;
        if IsBackSlash then
          Result:=Result+'\';
      finally
        Windows.FindClose(h);
      end;
    end
    else begin
      Result:=Result+APath;
      Exit;
    end;
    Delete(APath,1,i);
  until Length(APath)=0;
end; {GetLongPathName}
{$ENDIF}

function GetMonthFromDate(ADate:TDateTime):word;
var Y,M,D : word;
begin
  try
    Decodedate(ADate,Y,M,D);
  except
    M:=0;
  end;
  Result:=M;
end; {GetMonthFromDate}

function GetNumberFromText(const AText:string):integer;
const ZMenge : set of char = ['0'..'9'];
var i,Start  : integer;
    Buf      : string;
begin
  Result:=0;
  Start:=0;
  i:=1;
  while (i<=length(AText)) and (Result=0) do begin
    if (Start=0) and (AText[i] in ZMenge) then
      Start:=i;
    if (Start>0) and not (AText[i] in ZMenge) then begin
      Buf:=copy(AText, Start, i-Start);
      Result:=StrToIntDef(Buf, 0);
    end;
    if (Start>0) and (i=length(AText)) then begin
      Buf:=copy(AText, Start, i-Start+1);
      Result:=StrToIntDef(Buf, 0);
    end;
    inc(i);
  end;
end; {GetNumberFromText}

function GetParentDir(const Directory:string):string;
var P : integer;
begin
  Result:='';
  P:=LastDelimiter('\', CutBackSlash(Directory));
  if P>0 then
    Result:=copy(Directory, 1, P-1);
end; {GetParentDir}

{$IFDEF SR_Delphi2_Up}
function GetProgramFilesDir:string;
var Reg : TRegistry;
    Res : boolean;
begin
  Reg:=TRegistry.Create;
  try
    Reg.Rootkey:=HKEY_LOCAL_MACHINE;
    Res:=Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion',false);
    if Res then begin
      Result:=AddBackSlash(Reg.ReadString('ProgramFilesDir'));
      Reg.CloseKey;
    end
    else
      Result:='';
  finally
    Reg.Free;
  end;
end; {GetProgramFilesDir}
{$ENDIF}

function GetSecondFromTime(ATime:TDateTime):byte;
var H,M,S,MS : word;
begin
  try
    DecodeTime(ATime, H, M, S, MS);
  except
    S:=0;
  end;
  Result:=S;
end; {GetSecondFromTime}

function GetShiftState:TShiftState;
const Key_Pressed = 65535;
{$IFDEF SR_Delphi1}
var AState : integer;
{$ELSE}
var AState : short;
{$ENDIF}
begin
  Result:=[];
  AState:=GetAsyncKeyState(VK_Shift);
  if (AState and Key_Pressed)>0 then
    Include(Result, ssShift);
  AState:=GetAsyncKeyState(VK_Menu);
  if (AState and Key_Pressed)>0 then
    Include(Result, ssAlt);
  AState:=GetAsyncKeyState(VK_Control);
  if (AState and Key_Pressed)>0 then
    Include(Result, ssCtrl);
end; {GetShiftState}

function GetSystemDir:string;
var SysDir : array [0..Max_Path] of char;
begin
  GetSystemDirectory(SysDir,Max_Path);
  Result:=AddBackSlash(String(SysDir));
end; {GetSystemDir}

{$IFDEF SR_Delphi2_Up}
function GetSystemFileDescription(FileName:string):string;
var
 SysIL : UInt;
 Info  : TSHFileInfo;
begin
  SysIL:=SHGetFileInfo(PChar(FileName), 0, Info, SizeOf(TSHFileInfo), SHGFI_TYPENAME);
  if SysIL<>0 then
    Result:=Info.szTypeName
  else
    Result:='';
end; {GetSystemFileDescr}

function GetSystemFolderLocation(const FolderID:integer):string;
var pIDL : PItemIDList;
    Path : PChar;
begin
  if SUCCEEDED(SHGetSpecialFolderLocation(0, FolderID, pIDL)) then begin
    Path:=StrAlloc(Max_Path);
    SHGetPathFromIDList(pIDL, Path);
    Result:=AddBackSlash(String(Path));
  end;
end; {GetSystemFolderLocation}

function GetSystemWorkArea:TRect;
var PRect : ^TRect;
begin
  Result:=Rect(0, 0, 0, 0);
  GetMem(PRect, SizeOf(TRect));
  try
    if SystemParametersInfo(SPI_GetWorkArea, 0, PRect, 0) then begin
      Result:=PRect^;
    end;
  finally
    FreeMem(PRect);
  end;
end; {GetSystemWorkArea}
{$ENDIF}

function GetTextInWindow(const AWnd:HWnd):string;
var WndText : array [0..255] of Char;
begin
  try
    SendMessage(AWnd, WM_GetText, 255, integer(@WndText));
    Result:=string(WndText);
  except
    Result:='';
  end;
end; {GetTextInWindow}

function GetVersionNr(ExeName:string;BuildNr:boolean):string;
var V1,V2,V3,V4 : Word;
begin
  if GetBuildInfo(ExeName, V1, V2, V3, V4) then begin
    if BuildNr then
      Result:=IntToStr(V1)+'.'+IntToStr(V2)+IntToStr(V3)+' (Build '+IntToStr(V4)+')'
    else
      Result:=IntToStr(V1)+'.'+IntToStr(V2)+IntToStr(V3);
  end
  else
    Result:='';
end; {GetVersionNr}

function GetWeekOfYear(ADate:TDateTime):byte;
var Year,Month,Day : Word;
begin
  ADate:=ADate-((DayOfWeek(ADate)-FirstWeekDay+7) mod 7)+ 7-FirstWeekDate;
  DecodeDate(ADate, Year, Month, Day);
  Result:=(Trunc(ADate-EncodeDate(Year, 1, 1)) div 7)+1;
end; {GetWeekOfYear}

function GetWeeksPerYear(AYear:word):byte;
var AWeek : byte;
begin
  AWeek:=GetWeekOfYear(EncodeDate(AYear,12,31));
  if AWeek=1 then
    Result:=52
  else
    Result:=AWeek;
end; {GetWeeksPerYear}

function GetWindowCaption(const AWnd:HWnd):string;
var Buf : PChar;
begin
  Buf:=StrAlloc(255);
  try
    GetWindowText(AWnd, Buf, 255);
    {$IFDEF SR_Delphi1}
    Result:=StrPas(Buf);
    {$ELSE}
    Result:=string(Buf);
    {$ENDIF}
  finally
    StrDispose(Buf);
  end;
end; {GetWindowCaption}

function GetWindowClass(const AWnd:HWnd):string;
var Buf : PChar;
begin
  Buf:=StrAlloc(255);
  try
    GetClassName(AWnd, Buf, 255);
    {$IFDEF SR_Delphi1}
    Result:=StrPas(Buf);
    {$ELSE}
    Result:=string(Buf);
    {$ENDIF}
  finally
    StrDispose(Buf);
  end;
end; {GetWindowClass}

function GetWindowsDir:string;
var WinDir : array [0..Max_Path] of char;
begin
  GetWindowsDirectory(WinDir, Max_Path);
  Result:=AddBackSlash(String(WinDir));
end; {GetWindowsDir}

{$IFDEF SR_Delphi2_Up}
function GetTempDir:string;
var TempDir : array [0..Max_Path] of char;
begin
  GetTempPath(Max_Path, TempDir);
  Result:=AddBackSlash(String(TempDir));
end; {GetTemporaryDir}

function GetWindowState(WHandle:HWnd):integer;
{$IFNDEF SR_Delphi4_Up}
var PPlcmnt : TWindowPlacement;
{$ELSE}
var PPlcmnt : WindowPlacement;
{$ENDIF}

  { Die Rückgabewerte der Funktion entsprechen folgenden Konstanten
    aus der Unit Windows.pas:

    SW_HIDE = 0;
    SW_SHOWNORMAL = 1;
    SW_SHOWMINIMIZED = 2;
    SW_SHOWMAXIMIZED = 3;
    SW_SHOWNOACTIVATE = 4;
    SW_SHOW = 5;
    SW_MINIMIZE = 6;
    SW_SHOWMINNOACTIVE = 7;
    SW_SHOWNA = 8;
    SW_RESTORE = 9;
    SW_SHOWDEFAULT = 10; }

begin
  {$IFNDEF SR_Delphi4_Up}
  PPlcmnt.Length:=SizeOf(TWindowPlacement);
  {$ELSE}
  PPlcmnt.Length:=SizeOf(WindowPlacement);
  {$ENDIF}
  if GetWindowPlacement(WHandle, @PPlcmnt) then
    Result:=PPlcmnt.ShowCmd
  else
    Result:=-1;
end; {GetWindowState}

function GetWinUsername:string;
var UName : PChar;
    USize : DWord;
begin
  USize:=Max_Path;
  UName:=StrAlloc(USize);
  try
    GetUserName(UName,USize);
    Result:=string(UName);
  finally
    StrDispose(UName);
  end;
end; {GetWinUsername}
{$ENDIF}

function GetYearFromDate(ADate:TDateTime):word;
var Y,M,D : word;
begin
  try
    Decodedate(ADate, Y, M, D);
  except
    Y:=0;
  end;
  Result:=Y;
end; {GetYearFromDate}

{$IFDEF SR_Delphi2_Up}
procedure InitLocale;
var SystemTime: TSystemTime;

  function GetLocaleInt(AInfo:LCTYPE):Integer;
  var
    Buffer: array[0..1] of Char;
  begin
    if GetLocaleInfo(GetThreadLocale, AInfo, Buffer, 2) > 0 then
      Result:=Ord(Buffer[0])-Ord('0')
    else
      Result:=-1;
  end; {GetLocaleInt}

begin
  LocaleIDate :=GetLocaleInt(LOCALE_IDATE);
  LocaleILDate:=GetLocaleInt(LOCALE_ILDATE);
  GetLocalTime(SystemTime);
  CurrentYear2Digit:=SystemTime.wYear mod 100;
  CurrentCentury:=SystemTime.wYear-CurrentYear2Digit;
end; {InitLocale}
{$ENDIF}

function IntToStrFixed(IntValue:integer;OutDigits:byte):string;
begin
  try
    Result:=IntToStr(Abs(IntValue));
    while (length(Result)<OutDigits) do
      Result:='0'+Result;
    if (IntValue<0) then
      Result:='-'+Result;
  except
    Result:='';
  end;
end; {IntToStrFixed}

{$IFNDEF SR_Delphi3_Up}
function IsLeapYear(AYear:integer):boolean;
begin
  Result:=(AYear mod 4=0) and ((AYear mod 100<>0) or (AYear mod 400=0));
end; {IsLeapYear}
{$ENDIF}

function IsNumber(const AText:string):boolean;
const ZMenge : set of char = ['0'..'9'];
var i : integer;
begin
  if AText='' then
    Result:=false
  else begin
    Result:=true;
    i:=1;
    while (i<length(AText)) and Result do begin
      if not (AText[i] in ZMenge) and (AText[i]<>',') and (AText[i]<>'.') then
        Result:=false;
      inc(i);
    end;
  end;
end; {IsNumber}

function IsSummertime(ADate:TDateTime):boolean;
var AYear,
    AMonth,
    ADay   : word;
    Beginn,
    Ende   : TDateTime;
begin
  try
    ADate:=trunc(ADate);
    DecodeDate(ADate, AYear, AMonth, ADay);
    if AYear<1980 then
      { Keine Sommerzeit vor 1980 }
      Result:=false
    else begin
      { Beginn der Sommerzeit: }
      Beginn:=EncodeDate(AYear, 3, 31);
      while DayOfWeek(Beginn)<>1 do
        Beginn:=Beginn-1;
      { Ende der Sommerzeit: }
      if AYear<=1995 then
        { bis 1995: letzter So im September }
        Ende:=EncodeDate(AYear, 9, 30)
      else
        { ab 1996: letzter So im Oktober }
        Ende:=EncodeDate(AYear, 10, 31);
      while DayOfWeek(Ende)<>1 do
        Ende:=Ende-1;
      Result:=(ADate>=Beginn) and (ADate<Ende);
    end;
  except
    Result:=false;
  end;
end; {IsSummertime}

{$IFDEF SR_Delphi2_Up}
function IsWindowsNT:boolean;
var OsVinfo : TOSVERSIONINFO;
begin
  ZeroMemory(@OsVinfo,sizeOf(OsVinfo));
  OsVinfo.dwOSVersionInfoSize := sizeof(TOSVERSIONINFO);
  if GetVersionEx(OsVinfo) then
    Result:=OsVinfo.dwPlatformId = VER_PLATFORM_WIN32_NT
  else
    Result:=false;
end; {IsWindowsNT}
{$ENDIF}

{$IFNDEF SR_Delphi3_Up}
function LastDelimiter(AChars,AText:string):integer;
var i,j : integer;
begin
  Result:=0;
  if (length(AChars)=0) or (length(AText)=0) then
    Exit;
  for i:=length(AText) downto 1 do begin
    for j:=1 to length(AChars) do begin
      if AText[i]=AChars[j] then begin
        Result:=i;
        Exit;
      end;
    end;
  end;
end; {LastDelimiter}
{$ENDIF}

{$IFDEF SR_Delphi2_Up}
function Like(const AString, APattern: String): Boolean;
var
  StringPtr,
  PatternPtr,
  StringRes,
  PatternRes : PChar;
begin
  Result:=false;
  StringPtr:=PChar(AString);
  PatternPtr:=PChar(APattern);
  StringRes:=nil;
  PatternRes:=nil;
  repeat
    repeat
      case PatternPtr^ of
        #0: begin
          Result:=StringPtr^=#0;
          if Result or (StringRes=nil) or (PatternRes=nil) then
            Exit;
          StringPtr:=StringRes;
          PatternPtr:=PatternRes;
          Break;
        end;
        '*': begin
          inc(PatternPtr);
          PatternRes:=PatternPtr;
          Break;
        end;
        '?': begin
          if StringPtr^=#0 then
            Exit;
          inc(StringPtr);
          inc(PatternPtr);
        end;
        else begin
          if StringPtr^=#0 then
            Exit;
          if StringPtr^<>PatternPtr^ then begin
            if (StringRes=nil) or (PatternRes=nil) then
              Exit;
            StringPtr:=StringRes;
            PatternPtr:=PatternRes;
            Break;
          end
          else begin
            inc(StringPtr);
            inc(PatternPtr);
          end;
        end;
      end;
    until false;
    repeat
      case PatternPtr^ of
        #0: begin
          Result:=true;
          Exit;
        end;
        '*': begin
          inc(PatternPtr);
          PatternRes:=PatternPtr;
        end;
        '?': begin
          if StringPtr^=#0 then
            Exit;
          inc(StringPtr);
          inc(PatternPtr);
        end;
        else begin
          repeat
            if StringPtr^=#0 then
              Exit;
            if StringPtr^=PatternPtr^ then
              Break;
            inc(StringPtr);
          until false;
          inc(StringPtr);
          StringRes:=StringPtr;
          inc(PatternPtr);
          Break;
        end;
      end;
    until false;
  until false;
end; {Like}
{$ENDIF}

function IsValidMailAdress(const AText:string):boolean;
var P,Q : integer;
begin
  Result:=false;
  P:=Pos('@', AText);
  if P>=3 then begin
    Q:=LastDelimiter('.', AText);
    if (Q>(P+2)) and (Q<length(AText)) then begin
      Result:=true;
      for P:=1 to length(AText) do
        if not (AText[P] in AllowedMailChars) then
          Result:=false;
    end;
  end;
end; {IsValidMailAdress}

function LowCase(const AChar:char):char;
begin
  if (AChar>='A') and (AChar<='Z') then
    Result:=chr(ord(AChar)+32)
  else
    Result:=AChar;
end; {LowCase}

function PosN(const AText,S:string;N:integer):integer;
var Temp : string;
    P    : integer;
begin
  if N>1 then begin
    Temp:=copy(S, N, length(S)-N+1);
    dec(N);
  end
  else
    Temp:=S;
  P:=Pos(AText, Temp);
  if P>0 then
    Result:=P+N
  else
    Result:=0;
end; {PosN}

function ReplaceEntities(const AText:string):string;
var k,j,Len  : integer;
    IsEntity : boolean;

  function GetHighChar(const Entity:string):string;
  var i : integer;
  begin
    i:=0;
    Result:=Entity;
    while (Result=Entity) and (i<=MaxChars) do begin
      if Entities[i]=Entity then
        Result:=HighChars[i];
      inc(i);
    end;
  end; {GetHighChar}

begin
  Result:='';
  j:=1;
  Len:=length(AText);
  while j<=Len do begin
    IsEntity:=false;
    k:=1;
    if AText[j]='&' then begin
      while ((j+k) < len) and (AText[j+k] <> ';') and (k<7) do
        inc(k);
      if AText[j+k]=';' then
        IsEntity:=true;
    end;
    if IsEntity then begin
      Result:=Result+GetHighChar(copy(AText, j, k+1));
      inc(j, k+1);
    end
    else begin
      Result:=Result+AText[j];
      inc(j);
    end;
  end;
end; {ReplaceEntities}

function ReplaceHighChars(const AText:string):string;
var j,Len : integer;

  function GetEntity(const HighChar:string):string;
  var i : integer;
  begin
    i:=0;
    Result:=HighChar;
    while (Result=HighChar) and (i<MaxChars) do begin
      if HighChars[i]=HighChar then
        Result:=Entities[i];
      inc(i);
    end;
  end; {GetEntity}

begin
  Result:='';
  Len:=length(AText);
  for j:=1 to Len do begin
    if ord(AText[j])>127 then
      Result:=Result+GetEntity(AText[j])
    else
      Result:=Result+AText[j];
  end;
end; {ReplaceHighChars}

function ReverseString(AText:string):string;
var i : byte;
begin
  Result:='';
  for i:=length(AText) downto 1 do
    Result:=Result+AText[i];
end; {ReverseString}

procedure RewrapText(AFrom,ATo:TStrings;const MaxLineLength:integer;WrapInWord:boolean);
var AText,
    BText : string;
    i,P   : integer;
begin
  AText:='';
  ATo.Clear;
  for i:=0 to AFrom.Count-1 do begin
    if AText='' then
      AText:=AFrom[i]
    else
      AText:=AText+' '+AFrom[i];
    if length(AText)>=MaxLineLength then begin
      repeat
        BText:=copy(AText, 1, MaxLineLength);
        P:=LastDelimiter(' -,.!?:;/\+_=*', BText);
        if (P>0) or WrapInWord then begin
          if P<=0 then
            P:=MaxLineLength;
          ATo.Add(copy(AText, 1, P));
          Delete(AText, 1, P);
        end
        else begin
          P:=FirstDelimiter(' -,.!?:;/\+_=*', AText);
          if P>0 then begin
            ATo.Add(copy(AText, 1, P));
            Delete(AText, 1, P);
          end
          else begin
            ATo.Add(AText);
            BText:=AText;
          end;
        end;
      until (BText=AText);
    end
    else begin
      ATo.Add(AText);
      AText:='';
    end;
  end;
end; {RewrapText}

function RGBToStr(RGBColor:integer):string;
var ColText : string;
begin
  ColText:=IntToHex(RGBColor,6);
  Result:=copy(ColText,5,2)+copy(ColText,3,2)+copy(ColText,1,2);
end; {RGBToStr}

{$IFDEF SR_Delphi2_Up}
procedure SendKeys(AText:string);
var i : integer;
    w : word;
begin
  for i:=1 to Length(AText) do begin
    w:=VkKeyScan(AText[i]);
    if ((HiByte(w)<>$FF) and (LoByte(w)<>$FF)) then begin
      {If the key requires the shift key down - hold it down}
      if HiByte(w) and 1 = 1 then
        SimulateKeyDown(VK_SHIFT);
      {Send the VK_KEY}
      SimulateKeystroke(LoByte(w), 0);
      {If the key required the shift key down - release it}
      if HiByte(w) and 1 = 1 then
        SimulateKeyUp(VK_SHIFT);
    end;
  end;
end; {SendKeys}
{$ENDIF}

{$IFDEF SR_Delphi1}
procedure SetFileDate(FName:string;FDate:LongInt);
var F : TFileStream;
begin
  try
    F:=TFileStream.Create(FName,fmOpenWrite);
    FileSetDate(F.Handle,FDate);
  finally
    F.Free;
  end;
end;
{$ELSE}
function SetFileDate(FName:string;FDate:Integer):boolean;
var F : TFileStream;
begin
  try
    F:=TFileStream.Create(FName,fmOpenWrite);
    Result:=(FileSetDate(F.Handle,FDate)=0);
    F.Free;
  except
    Result:=false;
  end;
end; {SetFileDate}
{$ENDIF}

{$IFDEF SR_Delphi2_Up}
function SetFileDateTime(const FName:string;const ATime:TDateTime;const WhatTime:byte):boolean;
var
  FHandle       : integer;
  LocalFileTime : TFileTime;
  FileTime      : TFileTime;
  DosFileTime   : Integer;
begin
  Result:=False;
  DosFileTime:=DateTimeToFileDate(ATime);
  if DosDateTimeToFileTime(LongRec(DosFileTime).Hi,
                           LongRec(DosFileTime).Lo, LocalFileTime) and
     LocalFileTimeToFileTime(LocalFileTime, FileTime) then begin
    FHandle:=FileOpen(FName, fmOpenWrite);
    if (FHandle<>-1) then begin
      case WhatTime of
        ftCreation :
          Result:=SetFileTime(FHandle, @FileTime, Nil, Nil);
        ftLastAccess :
          Result:=SetFileTime(FHandle, Nil, @FileTime, Nil);
        else // ftLastWrite
          Result:=SetFileTime(FHandle, Nil, Nil, @FileTime);
      end;
      FileClose(FHandle);
    end;
  end;
end; {SetFileDateTime}

procedure SimulateKeyDown(Key : byte);
begin
  Keybd_Event(Key, 0, 0, 0);
end; {SimulateKeyDown}

procedure SimulateKeystroke(Key:byte; extra:DWORD);
begin
  Keybd_Event(Key, extra, 0, 0);
  Keybd_Event(Key, extra, KEYEVENTF_KEYUP, 0);
end; {SimulateKeystroke}

procedure SimulateKeyUp(Key : byte);
begin
  Keybd_Event(Key, 0, KEYEVENTF_KEYUP, 0);
end; {SimulateKeyUp}
{$ENDIF}

{$IFNDEF SR_Delphi4_Up}
function StringReplace(const S,OldPattern,NewPattern:string;const Flags:TReplaceFlags):string;
var i,Max,Len : integer;
    Found     : boolean;
    Patt,
    SearchStr : string;
begin
  Result:='';
  Max:=length(S)-length(OldPattern)+1;
  if Max>0 then begin
    if rfIgnoreCase in Flags then begin
      SearchStr:=AnsiUpperCase(S);
      Patt:=AnsiUpperCase(OldPattern);
    end
    else begin
      SearchStr:=S;
      Patt:=OldPattern;
    end;
    Len:=length(Patt);
    i:=1;
    Found:=false;
    while (i<=Max) and not Found do begin
      if copy(SearchStr, i, Len)=Patt then begin
        Result:=Result+NewPattern;
        inc(i, Len);
        if not (rfReplaceAll in Flags) then
          Found:=true;
      end
      else begin
        Result:=Result+SearchStr[i];
        inc(i);
      end;
    end;
  end;
end; {StringReplace}
{$ENDIF}

function StripForbiddenChars(const AText:string):string;
var i : integer;
begin
  Result:='';
  if length(AText)>0 then begin
    for i:=1 to length(AText) do begin
      {$IFDEF SR_Delphi1}
      if not (AText[i] in ShortForbiddenChars) then
        Result:=Result+AText[i];
      {$ELSE}
      if not (AText[i] in LongForbiddenChars) then
        Result:=Result+AText[i];
      {$ENDIF}
    end;
  end;
end; {StripForbiddenChars}

function StrToDateDef(S:string;Def:TDateTime):TDateTime;
begin
  try
    Result:=StrToDate(S);
  except
    Result:=Def;
  end;
end; {StrToDateDef}

function StrToDateTimeDef(S:string;Def:TDateTime):TDateTime;
begin
  try
    Result:=StrToDateTime(S);
  except
    Result:=Def;
  end;
end; {StrToDateTimeDef}

function StrToFloatDef(S:string;Def:extended):extended;
begin
  try
    Result:=StrToFloat(S);
  except
    Result:=Def;
  end;
end; {StrToFloatDef}

function StrToTimeDef(S:string;Def:TDateTime):TDateTime;
begin
  try
    Result:=StrToTime(S);
  except
    Result:=Def;
  end;
end; {StrToTimeDef}

function TimeToStrDef(ADate:TDateTime;Default:string):string;
begin
  try
    Result:=TimeToStr(ADate);
  except
    Result:=Default;
  end;
end; {TimeToStrDef}

{$IFDEF SR_Delphi1}
function Trim(const AText:string):string;
var i,L: Integer;
begin
  L:=length(AText);
  i:=1;
  while (i<=L) and (AText[i]<=' ') do
    inc(i);
  if i>L then
    Result:=''
  else begin
    while AText[L]<=' ' do
      dec(L);
    Result:=Copy(AText, i, L-i+1);
  end;
end; {Trim}
{$ENDIF}

function TrimChars(const AChar:char;const AText:string):string;
var i,L: Integer;
begin
  L:=length(AText);
  i:=1;
  while (i<=L) and (AText[i]=AChar) do
    inc(i);
  if i>L then
    Result:=''
  else begin
    while AText[L]=AChar do
      dec(L);
    Result:=Copy(AText, i, L-i+1);
  end;
end; {TrimChars}

function ValidFileName(DName:string):boolean;
var i : integer;
begin
  Result:=true;
  for i:=1 to length(DName) do
    {$IFDEF SR_Delphi1}
    if DName[i] in ShortForbiddenChars then
      Result:=false;
    {$ELSE}
    if DName[i] in LongForbiddenChars then
      Result:=false;
    {$ENDIF}
end; {ValidFileName}

{$IFDEF SR_Delphi1}
initialization

FirstWeekDay  := 2;  { Wochentag, mit dem die Woche beginnt
                       (siehe Delphi-Wochentage)
                       2 : Montag (nach DIN 1355) }
FirstWeekDate := 4;  { 1 : Beginnt am ersten Januar
                       4 : Erste-4 Tage-Woche (nach DIN 1355)
                       7 : Erste volle Woche }
{$ENDIF}

end.
