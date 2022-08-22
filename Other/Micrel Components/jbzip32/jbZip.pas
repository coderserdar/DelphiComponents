{.$D-,L-}
unit jbZip;
{$WARNINGS OFF}
{$HINTS OFF}
{$Define msgEnglish}
{$I jb.inc}
{Components for ZIPing a UNZIPing compatible with PkZip v1.1                }
{Now added features PKZIp 2.0 - Deflate compression method                  }
{Now is they compatible with WinZip and can you use longnames too           }

{Based on source (c) A.Byrne by old source                                  }
{Convert to component (c) Jaro Benes                                        }
{All right reserved                                                         }
{History and changes:                                                       }
{Modification to TP5.5 by Jaro Benes 1993                                   }
{Modification to TurboProfessional V.5.21 by Jaro Benes 1995                }
{Modification to Delphi 1 and changes by Jaro Benes 1996                    }
{Encapsulated as component for Delphi 1 by Jaro Benes 1997                  }
{Partially portation to Win32 (without assembler) Jaro Benes 1998           }
{Indication errors and connect to TGauge by Jaro Benes 1999                 }
{Note J.B. 16.7.1999}
{Components are free using under Delphi 1..7,9                              }
{Portation to Win32 finished, big thanks to Ivan Pavelka                    }
{1.12.1999                                                                  }

{last changes by J.B.                                                       }
{31.8.2001 fix getZipList                                                   }
{31.5.2002 Fix size of CentralDir for fully compatibility with Winzip       }
{25.2.2003 change param string to stringlist                                }
{13.8.2005 Added Deflate method through ZLib included on Delphi CD          }
{          Full compatibility with Winzip and PKZip                         }

{for use under Delphi 1 can you use assembler code for packing speed        }
{please do not use deflate method under Delphi 1                            }
{Please, send me any changes and improvement in code copy to my E-mail      }
{mailto:JBenes@micrel.cz                                                    }

{in code is used my library jbStr (c) Jaro Benes                            }
{last version can be download from www.micrel.cz/Delphi web page            }

{$IfNDef VER80}

{When you want use deflate method for better compression ratio              }
{.$Define useZLIB}

{$EndIf}

interface
uses
  SysUtils, FileCtrl, WinTypes, WinProcs, Classes, Dialogs{$IfDef useZLIB}, ZLib{$EndIf useZLIB};

Type
  TZipProgress = procedure (Sender: TObject; AProgress: Smallint) of object;
  TZipError = procedure (Sender: TObject; Const ErrorMsg: String) of object;
  TZip = class(TComponent)
    private
      FName: String;
      FBaseDir: String;
      FParams: TStringList;
      FWorkDir: String;
      FOverWrite: Boolean;
{$IfDef useZLIB}
      FForceDeflate: Boolean; //always true
{$EndIf useZLIB}
      FOnProgress: TZipProgress;
      FOnError: TZipError;
      { Private declarations }
    protected
      { Protected declarations }
{$IfDef useZLIB}
      CompressionStream: TCompressionStream;
      UncompressedSize: Integer;
      procedure ZLibProgress(Sender: TObject);
      Procedure ZLibCompress(UncompressData: String; Var CompressedData: String);
{$EndIf useZLIB}
      Procedure SetFName(Name:String);
    public
      { Public declarations }
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      Function Execute:Boolean;
      Procedure Crunch(Const FileNameForZip: String; FilesToZip:TStringList; Const WorkingDirForRelativePath:String);
    published
      { Published declarations }
      Property BasePath: String read FBaseDir write FBaseDir;
      Property ArcName:String Read FName Write SetFName;
      Property Files:TStringList Read FParams Write FParams;
      Property Overwrite:Boolean Read FOverWrite Write FOverWrite;
      Property OnProgress:TZipProgress Read FOnProgress Write FOnProgress;
      Property OnError:TZipError Read FOnError Write FOnError;
    end {Zip};

  TUnZip = class(TComponent)
    private
      FName:String; {name of input zip file in form pkzip 1.1}
      FExtrPath:String; {directory only}
      FParams:TStringList; {files to extract or empty for all}
      FOverWrite:Boolean;
      FOnProgress:TZipProgress;
      FOnError:TZipError;
      { Private declarations }
    protected
      { Protected declarations }
{$IfDef useZLIB}
      DecompressionStream: TDecompressionStream;
      UncompressedSize: Integer;
      procedure ZLibProgress(Sender: TObject);
      Procedure ZLibDecompress(CompressedData: String; UncompressedSize:integer;
        Var UncompressData: String);
{$EndIf useZLIB}
      Procedure SetFName(Name:String);
      Procedure SetExtrPath(Path:String);
      Procedure UnCrunch(Const ZipFileName, DirToExtract: String; FilesToExtract:TStringList);
    public
      { Public declarations }
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      Function Execute:Boolean;
      Procedure GetZipList(Const iFileName:String;Var A:TStringList);
    published
      { Published declarations }
      Property ArcName:String Read FName Write SetFName;
      Property ExtrPath:String Read FExtrPath Write SetExtrPath;
      Property Files:TStringList Read FParams Write FParams;
      Property Overwrite:Boolean Read FOverWrite Write FOverWrite;
      Property OnProgress:TZipProgress Read FOnProgress Write FOnProgress;
      Property OnError:TZipError Read FOnError Write FOnError;
    end {Zip};

procedure Register;

implementation

Uses jbStr;

{$IfDef VER4UP}
resourcestring
{$Else}
const
{$EndIf}
{$IfDef msgEnglish}
  StrCannotOpenGoNext = 'Cannot open %s, go just next...';
  StrNotEnoughMemoryToAllocStruct = 'Not enough memory to allocate LZW data structures!';
  StrNoFilesForCompress = 'No files for compress!';
  StrFileNotFound = 'File not found';
  StrIncorrectPathOrNotExists = 'Incorrect path or not found';
  StrBadNameOrPathOrSyntax = 'Bad name or path or syntax';
  StrDisketteIsFull = 'Diskette is full';
  StrInputOutputError = 'Input-output error %d';
  StrInvalidFileName = 'Invalid file name!';
  StrHeaderMissingOrDamaged = 'Header missing or damaged in %s';
  StrBadCodeBindingCont = 'Bad binding code! Continue...';
  StrCodeIsBad = 'Code is bad !';
  StrSorryStackOwerflow = 'Sorry stack owerflow (%d)!';
  StrBadShannonFanoDecodeTree = 'Bad Shannon-Fano decode tree !';
  StrNotEnoughMemoryForUnShrink = 'Not enough memory for UnShrink ! Continue...';
  StrNotEnoughMemoryForUnReduce = 'Not enough memory for UnReduce ! Continue...';
  StrNotEnoughMemoryForUnExplode = 'Not enough memory for UnExplode ! Continue...';
  StrUnknownCompressMethod = 'Unknown compress method (%d) used on %s ! Skip next...';
  StrFileHasBadCRC = 'File %s has bad CRC! Stored CRC is %x, computed CRC is %x.';
  StrNoOverwiteRights = 'No overwite rights for file %s!';
  StrNotEnoughMemoryToProcessAllFilenames = 'Not enough memory to process all filenames!';
  StrOpenOutputFileError = 'Open output file error.';
  StrBadCallOrBadParams = 'Bad call or bad parameters';
{$Else Others}
{$IfDef msgCzech}
{Czech messages}
  StrCannotOpenGoNext = 'Nemohu otevøít %s, jdu na další...';
  StrNotEnoughMemoryToAllocStruct = 'Není dost pamìti k vytvoøení LZW datové struktury!';
  StrNoFilesForCompress = 'Nejsou žádné soubory k smrsknutí!';
  StrFileNotFound = 'Soubor nebyl nalezen';
  StrIncorrectPathOrNotExists = 'Cesta není platná/nebyla nalezana';
  StrBadNameOrPathOrSyntax = 'Chyba souboru/cesty/syntaxe';
  StrDisketteIsFull = 'Disketa je plná';
  StrInputOutputError = 'Vstupnì/výstupní chyba %d';
  StrInvalidFileName = 'Chybné jméno souboru!';
  StrHeaderMissingOrDamaged = 'Záhlaví chybí nebo je poškozeno u %s';
  StrBadCodeBindingCont = 'Kód špatnì navazuje! Pokraèujeme...';
  StrCodeIsBad = 'Kód se jeví chybný!';
  StrSorryStackOwerflow = 'Promiòte, pøetekl mi zásobník (%d)!';
  StrBadShannonFanoDecodeTree = 'Chybný dekódovací strom!';
  StrNotEnoughMemoryForUnShrink = 'Není dost pamìti k rozmrsknutí!  Pøekraèuji...';
  StrNotEnoughMemoryForUnReduce = 'Není dost pamìti k rozvinutí! Pøekraèuji...';
  StrNotEnoughMemoryForUnExplode = 'Není dost pamìti k rozpnutí! Pøekraèuji...';
  StrUnknownCompressMethod = 'Neznámá svinovací metoda (%d) použitá na %s! Pøekraèuji...';
  StrFileHasBadCRC = 'Soubor %s má chybné CRC! Uložené CRC je %x, spoèítané CRC je %x.';
  StrNoOverwiteRights = 'Soubor %s není možné pøepsat!';
  StrNotEnoughMemoryToProcessAllFilenames = 'Není dost pamìti pro všechny soubory!';
  StrOpenOutputFileError = 'Chyba pøi otevírání výstupního souboru';
  StrBadCallOrBadParams = 'Chybné volání, chybné parametry';
{$EndIf}
{$IfDef msgFrench}
{$EndIf}
{$IfDef msgGerman}
{$EndIf}
   {others lang messages put here}
   {or include file like langXYZ.inc }
{$EndIf Others}


{$IfDef useZLIB}
{low-level routines}

procedure TZip.ZLibProgress(Sender: TObject);
begin
  if assigned(FOnProgress) then
    FOnProgress(Sender,Trunc(CompressionStream.Position / (UncompressedSize / 100)))
end;

Procedure TZip.ZLibCompress(UncompressData: String; Var CompressedData: String);
var
  CompressedStream: TStringStream;
begin
  CompressedStream := TStringStream.Create('');
  try
    CompressionStream := TCompressionStream.Create(clDefault, CompressedStream);
    try
      UncompressedSize := Length(UncompressData);
      CompressionStream.OnProgress := ZLibProgress;
      CompressionStream.Write(PChar(UncompressData)^, Length(UncompressData));
    finally
      CompressionStream.Free;
    end;
    {get out header (magical numbers) }
    CompressedData := Copy(compressedStream.DataString, 3,
      Length(compressedStream.DataString) - 6);
  finally
    CompressedStream.Free;
  end;
End;

procedure TUnZip.ZLibProgress(Sender: TObject);
begin
  if assigned(FOnProgress) then
    FOnProgress(Sender,Trunc(DecompressionStream.Position / (UncompressedSize / 100)))
end;

Procedure TUnZip.ZLibDecompress(CompressedData: String; UncompressedSize:integer;
  Var UncompressData: String);
var
  UncompressedStream: TStringStream;
  ZLibHeader: string;
  Readed: integer;
begin
  ZLibHeader := chr(120) + chr(156);
  {manufacture a 2 byte header for zlib; 4 byte footer is not required.}
  UncompressedStream := TStringStream.Create(ZLibHeader + CompressedData);
  try
    DecompressionStream := TDecompressionStream.Create(UncompressedStream);
    try
      DecompressionStream.OnProgress := ZLibProgress;
      Self.UncompressedSize := UncompressedSize;
      SetLength(UncompressData, UncompressedSize);
      Readed := DecompressionStream.Read(PChar(UncompressData)^,
        UncompressedSize);
      if Readed <> integer(UncompressedSize) then
        UncompressData := '';
    finally
      if assigned(FOnProgress) then
        FOnProgress(Self, 100);
      DecompressionStream.Free;
    end;
  finally
    UncompressedStream.Free;
  end;
end;
{$EndIf useZLIB}

Function SameName(Const N1, N2 : string) : Boolean;
{
  Function to compare filespecs.

  Wildcards allowed in either name.
  Filenames should be compared seperately from filename extensions by using
     seperate calls to this function
        e.g.  FName1.Ex1
              FName2.Ex2
              are they the same?
              they are if SameName(FName1, FName2) AND SameName(Ex1, Ex2)

  Wildcards work the way DOS should've let them work (eg. *XX.DAT doesn't
  match just any file...only those with 'XX' as the last two characters of
  the name portion and 'DAT' as the extension).

  This routine calls itself recursively to resolve wildcard matches.

}
Var
   P1, P2 : Smallint;
   Match  : Boolean;
Begin
  P1    := 1;
  P2    := 1;
  Match := True;
  If (Length(N1) = 0) And (Length(N2) = 0) Then
    Match := True
  Else
    If Length(N1) = 0 Then
      If N2[1] = '*' Then
        Match := True
      Else
        Match := False
    Else
      If Length(N2) = 0 Then
        If N1[1] = '*' Then
          Match := True
        Else
          Match := False;

  While (Match = True) And (P1 <= Length(N1)) And (P2 <= Length(N2)) Do
    If (N1[P1] = '?') Or (N2[P2] = '?') Then Begin
      Inc(P1);
      Inc(P2);
    End {then}
  Else
    If N1[P1] = '*' Then Begin
      Inc(P1);
      If P1 <= Length(N1) Then Begin
        While (P2 <= Length(N2)) And Not SameName(Copy(N1,P1,Length(N1)-P1+1), Copy(N2,P2,Length(N2)-P2+1)) Do
          Inc(P2);
        If P2 > Length(N2) Then
          Match := False
        Else Begin
          P1 := Succ(Length(N1));
          P2 := Succ(Length(N2));
        End {if};
      End {then}
      Else
        P2 := Succ(Length(N2));
    End {then}
  Else
    If N2[P2] = '*' Then Begin
      Inc(P2);
      If P2 <= Length(N2) Then Begin
        While (P1 <= Length(N1)) And Not SameName(Copy(N1,P1,Length(N1)-P1+1), Copy(N2,P2,Length(N2)-P2+1)) Do
          Inc(P1);
        If P1 > Length(N1) Then
          Match := False
        Else Begin
          P1 := Succ(Length(N1));
          P2 := Succ(Length(N2));
        End {if};
      End {then}
      Else
        P1 := Succ(Length(N1));
    End {then}
  Else
    If System.UpCase(N1[P1]) = System.UpCase(N2[P2]) Then Begin
      Inc(P1);
      Inc(P2);
    End {then}
  Else
    Match := False;

  If P1 > Length(N1) Then Begin
    While (P2 <= Length(N2)) And (N2[P2] = '*') Do
      Inc(P2);
    If P2 <= Length(N2) Then
      Match := False;
  End {if};
  
  If P2 > Length(N2) Then Begin
    While (P1 <= Length(N1)) And (N1[P1] = '*') Do
      Inc(P1);
    If P1 <= Length(N1) Then
      Match := False;
  End {if};
  
  SameName := Match;

End {SameName};

{ ---------------------------------------------------------------------------- }

Function SameFile(Const File1, File2 : String) : Boolean;
Var
  Path1, Path2 : String;
Begin
  Path1 := JustPathName(File1);
  Path2 := JustPathName(File2);
  SameFile := SameName(ExtractNameOfFileOnly(File1), ExtractNameOfFileOnly(File2)) And
    SameName(JustExtension(File1), JustExtension(File2)) And (Path1 = Path2);
End {SameFile};

Const
  Crc_32_Tab : Array[0..255] Of {$IfNDef VER5UP}LongInt{$Else}DWord{$EndIf} = (
$00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535, $9e6495a3,
$0edb8832, $79dcb8a4, $e0D5e91e, $97D2D988, $09b64c2b, $7eb17cbd, $e7b82D07, $90bf1D91,
$1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47D, $6ddde4eb, $f4D4b551, $83D385c7,
$136c9856, $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9, $fa0f3D63, $8D080df5,
$3b6e20c8, $4c69105e, $D56041e4, $a2677172, $3c03e4D1, $4b04D447, $D20D85fd, $a50ab56b,
$35b5a8fa, $42b2986c, $dbbbc9D6, $acbcf940, $32D86ce3, $45df5c75, $dcd60dcf, $abd13D59,
$26D930ac, $51de003a, $c8D75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
$2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab, $b6662D3D,
$76dc4190, $01db7106, $98D220bc, $efd5102a, $71b18589, $06b6b51f, $9fbfe4a5, $e8b8D433,
$7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb, $086D3D2D, $91646c97, $e6635c01,
$6b6b51f4, $1c6c6162, $856530D8, $f262004e, $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
$65b0D9c6, $12b7e950, $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2D49, $8cd37cf3, $fbd44c65,
$4db26158, $3ab551ce, $a3bc0074, $D4bb30e2, $4adfa541, $3dd895D7, $a4D1c46D, $D3D6f4fb,
$4369e96a, $346ed9fc, $ad678846, $da60b8D0, $44042D73, $33031de5, $aa0a4c5f, $dd0D7cc9,
$5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966D409, $ce61e49f,
$5edef90e, $29D9c998, $b0D09822, $c7D7a8b4, $59b33D17, $2eb40D81, $b7bd5c3b, $c0ba6cad,
$edb88320, $9abfb3b6, $03b6e20c, $74b1D29a, $ead54739, $9dd277af, $04db2615, $73dc1683,
$e3630b12, $94643b84, $0D6D6a3e, $7a6a5aa8, $e40ecf0b, $9309ff9D, $0a00ae27, $7D079eb1,
$f00f9344, $8708a3D2, $1e01f268, $6906c2fe, $f762575D, $806567cb, $196c3671, $6e6b06e7,
$fed41b76, $89D32be0, $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
$D6D6a3e8, $a1D1937e, $38D8c2c4, $4fdff252, $D1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
$D80D2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef, $4669be79,
$cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703, $220216b9, $5505262f,
$c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2D7ffa7, $b5D0cf31, $2cd99e8b, $5bdeae1D,
$9b64c2b0, $ec63f226, $756aa39c, $026D930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
$95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92D28e9b, $e5D5be0D, $7cdcefb7, $0bdbdf21,
$86D3D2D4, $f1D4e242, $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
$88085ae6, $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
$a00ae278, $D70dd2ee, $4e048354, $3903b3c2, $a7672661, $D06016f7, $4969474D, $3e6e77db,
$aed16a4a, $D9D65adc, $40df0b66, $37D83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
$bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605, $cdd70693, $54de5729, $23D967bf,
$b3667a2e, $c4614ab8, $5D681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b, $2D02ef8D
);

Procedure TZip.Crunch(Const FileNameForZip: String; FilesToZip:TStringList; Const WorkingDirForRelativePath:String);
Const
   MaxFilesSpesc =   21;   { Maximum souboru pouzito }
   BufSize     =  10240;   { Use 10K file buffers }
   MINBITS     =      9;   { Starting code size of 9 bits }
   MAXBITS     =     13;   { Maximum code size of 13 bits }
   TABLESIZE   =   8191;   { We'll need 4K entries in table }
   SPECIAL     =    256;   { Special function code }
   INCSIZE     =      1;   { Code indicating a jump in code size }
   CLEARCODE   =      2;   { Code indicating code table has been cleared }
   FIRSTENTRY  =    257;   { First available table entry }
   UNUSED      =     -1;   { Prefix indicating an unused code table entry }
   STDATTR     =    $23;   { Standard file attribute for DOS Find First/Next }
Const
   LOCAL_FILE_HEADER_SIGNATURE = $04034B50;
Type
   Local_File_Header_Type = packed Record
     Signature              :  LongInt;
     Extract_Version_Reqd   :  Word;
     Bit_Flag               :  Word;
     Compress_Method        :  Word;
     Last_Mod_Time          :  Word;
     Last_Mod_Date          :  Word;
     Crc32                  :  LongInt;
     Compressed_Size        :  LongInt;
     Uncompressed_Size      :  LongInt;
     Filename_Length        :  Word;
     Extra_Field_Length     :  Word;
   End;

{ Define the Central Directory record types }

Const
  CENTRAL_FILE_HEADER_SIGNATURE = $02014B50;
Type
  Central_File_Header_Type = packed Record 
    Signature            :  LongInt;
    MadeBy_Version       :  Word;
    Extract_Version_Reqd :  Word;
    Bit_Flag             :  Word;
    Compress_Method      :  Word;
    Last_Mod_Time        :  Word;
    Last_Mod_Date        :  Word;
    Crc32                :  LongInt;
    Compressed_Size      :  LongInt;
    Uncompressed_Size    :  LongInt;
    Filename_Length      :  Word;
    Extra_Field_Length   :  Word;
    File_Comment_Length  :  Word;
    Starting_Disk_Num    :  Word;
    Internal_Attributes  :  Word;
    External_Attributes  :  LongInt;
    Local_Header_Offset  :  LongInt;
  End;

Const
  END_OF_CENTRAL_DIR_SIGNATURE = $06054B50;

Type
  End_of_Central_Dir_Type =  packed Record
    Signature               :  LongInt;
    Disk_Number             :  Word;
    Central_Dir_Start_Disk  :  Word;
    Entries_This_Disk       :  Word;
    Total_Entries           :  Word;
    Central_Dir_Size        :  LongInt;
    Start_Disk_Offset       :  LongInt;
    ZipFile_Comment_Length  :  Word;
  End;

Type
  { Define data types needed to implement a code table for LZW compression   }
  CodeRec     =  Record                { Code Table record format...         }
    Child   : Smallint;                { Addr of 1st suffix for this prefix  }
    Sibling : Smallint;                { Addr of next suffix in chain        }
    Suffix  : Byte;                    { Suffix character                    }
  End {CodeRec};
  CodeArray   =  Array[0..TABLESIZE] Of CodeRec; { Define the code table     }
  TablePtr    =  ^CodeArray;                     { Allocate dynamically      }

  { Define data types needed to implement a free node list                   }
  FreeListPtr    =  ^FreeListArray;
  FreeListArray  =  Array[FIRSTENTRY..TABLESIZE] Of Word;

  { Define data types needed to implement input and output file buffers      }
  BufArray    =  Array[1..BufSize] Of Byte;
  BufPtr      =  ^BufArray;

  { Define the structure of a DOS Disk Transfer Area (DTA)                   }
  DTARec      =  Packed Record {J.B.}
    Filler   :  Array[1..21] Of Byte;
    Attr     :  Byte;
    Time     :  Word;
    Date     :  Word;
    Size     :  LongInt;
    Name     :  String[{$IfDef VER80}12{$Else}128{$EndIf}];
  End {DtaRec};

  { Define data types needed to implement a sorted singly linked list to     }
  { hold the names of all files to be compressed                             }
  NameStr      = String{$IFNDEF UNICODE}[80]{$ENDIF}; {12}
  PathStr      = String{$IFNDEF UNICODE}[127]{$ENDIF}; {64}
  NodePtr      = ^NameList;
  NameList     = Record                   { Linked list node structure...     }
    Path : PathStr;                       { Path of input file                }
    Name : NameStr;                       { Name of input file                }
    Size : LongInt;                       { Size in bytes of input file       }
    Date : Word;                          { Date stamp of input file          }
    Time : Word;                          { Time stamp of input file          }
    Next : NodePtr;                       { Next node in linked list          }
  End {NameList};
Type TNameFileSpec = Array[1..20] Of String;


Var
   OutFileName :  String;        { Name of resulting Zip file                 }
   InFile,                       { I/O file variables                         }
   OutFile     :  File;
   InBuf,                        { I/O buffers                                }
   OutBuf      :  BufPtr;
   InBufIdx,                     { Points to next char in buffer to be read   }
   OutBufIdx   :  Word;          { Points to next free space in output buffer }
   MaxInBufIdx :  integer;       { Count of valid chars in input buffer       }

   InputEof    :  Boolean;       { End of file indicator                      }
                                 { CRC calculation variable                   }
   Crc32Val    :  {$IFDEF VER5UP}DWORD{$ELSE}LongInt{$ENDIF};
   CodeTable   :  TablePtr;      { Points to code table for LZW compression   }

   FreeList    :  FreeListPtr;   { Table of free code table entries           }
   NextFree    :  Word;          { Index into free list table                 }

   ClearList   :  Array[0..1023] Of Byte;  { Bit mapped structure used in     }
                                           {    during adaptive resets        }
   CodeSize    :  Byte;     { Size of codes (in bits) currently being written }
   MaxCode     :  Word;   { Largest code that can be written in CodeSize bits }

   LocalHdr    :  Local_File_Header_Type;
   LocalHdrOfs :  LongInt;  { Offset within output file of the local header   }
   CentralHdr  :  Central_File_Header_Type;
   EndHdr      :  End_of_Central_Dir_Type;

   FirstCh     :  Boolean;  { Flag indicating the START of a shrink operation }
   TableFull   :  Boolean;  { Flag indicating a full symbol table             }

   SaveByte    :  Byte;     { Output code buffer                              }
   BitsUsed    :  Byte;     { Index into output code buffer                   }

   BytesIn     :  LongInt;  { Count of input file bytes processed             }
   BytesOut    :  LongInt;  { Count of output bytes                           }

   ListHead    :  NodePtr;  { Pointer to head of linked list                  }

   TenPercent  :  LongInt;

Procedure Fatal(Msg : String);
Begin
  If AsSigned(FOnError) Then FOnError(Self,Msg)
  Else
    MessageDlg(Msg, mtWarning, [mbOk], 0);
End;
{ --------------------------------------------------------------------------- }

Procedure AddToList(PathSpec : PathStr; DTA : DTARec);
{ Add an entry to a linked list of filenames to be crunched.  Maintain        }
{ sorted order (standard ASCII collating sequence) by filename                }
Var
  NewNode  : NodePtr;
  Done     : Boolean;
  ListNode : NodePtr;
Begin
  { Allocate a new node }
  try
    GetMem(NewNode, SizeOf(NewNode^))
  except
    on EOutOfMemory do
    Begin
      Fatal(StrNotEnoughMemoryToProcessAllFilenames);
      Exit
    End;
  end;
  { Populate the fields of the new node                                      }
  NewNode^.Path := PathSpec;
  NewNode^.Name := string(DTA.Name);
  NewNode^.Size := DTA.Size;
  NewNode^.Date := DTA.Date;
  NewNode^.Time := DTA.Time;
  NewNode^.Next := Nil;

  { Find the proper location in the list at which to insert the new node     }
  If ListHead = Nil
   Then ListHead := NewNode
   Else
    If string(DTA.Name) < ListHead^.Name
     Then
      Begin // zalozi se novy  zaznam
        NewNode^.Next := ListHead;
        ListHead      := NewNode;
      End {then}
     Else
      Begin
       Done     := False;
       ListNode := ListHead;
       While Not Done Do
         Begin
           If ListNode^.Name = string(DTA.Name)
            Then
              Begin
                ListNode^.Path := PathSpec;
                FreeMem(NewNode, SizeOf(NewNode^));
                Done := True;
              End {then}
            Else
             If ListNode^.Next = Nil
              Then
                Begin
                 ListNode^.Next := NewNode;
                 Done := True;
                End {then}
              Else
               If ListNode^.Next^.Name > string(DTA.Name) Then
               Begin
                  NewNode^.Next  := ListNode^.Next;
                  ListNode^.Next := NewNode;
                  Done := True;
                  End {then}
               Else
                  ListNode := ListNode^.Next;
         End {while};
      End {if};
End {AddToList};

{ --------------------------------------------------------------------------- }

procedure SRecToDTARec(SearchRec:TSearchRec;var DosDTA:DTARec);
Var vx:Array [1..2] of Word;
Begin
  Move(SearchRec.Time,vx,SizeOf(SearchRec.Time));
  DosDTA.Attr:= SearchRec.Attr AND $FF;
  //name falsing - date and time are complex and must be dissect
  DosDTA.Time:= vx[1]{SearchRec.Time};//datum a cas je slozeny, jmeno klame
  DosDTA.Date:= vx[2]{0};             //musi se rozlozit J.B.
  DosDTA.Size:= (SearchRec.FindData.nFileSizeHigh shl 8) +
                (SearchRec.FindData.nFileSizeLow);
  DosDTA.Name:= AnsiString(SearchRec.Name);
End;

Procedure GetNames(InFileSpecs: TStringList);
{ Expand input file specifications.  Store the name of each file to be        }
{ compressed in a sorted, singly linked list                                  }
Var
   DosDTA   : DTARec;
   ActRec   : TSearchRec;
   I        : Word;
   InPath   : String;
Begin
  ListHead := Nil;
  For I := 0 To InFileSpecs.Count-1 Do { Loop through all input file specs    }
  Begin
    InPath := ExtractFilePath(InFileSpecs[I]);
    If SysUtils.FindFirst(InFileSpecs[I], STDATTR, ActRec) = 0 Then
      Repeat
        If (Not SameFile(ExpandFileName(InPath + ActRec.Name), ExpandFileName(OutFileName))) Then
           Begin
             SRecToDTARec(ActRec,DosDTA);
             AddToList(InPath, DosDTA);
           end;
      Until SysUtils.FindNext(ActRec)<>0;
    Sysutils.FindClose(ActRec);
  End {for};
End {GetNames};

{ --------------------------------------------------------------------------- }

Function ParamCheck(OverWrite:Boolean) : Boolean;
  procedure GetAllFilesOnMask(mask:string; var ListOfFiles:TStringList);
  {search files recursive into dirs}
  var
    search:TSearchRec;
    verz:string;
    such:string;
  begin
    such:=ExtractFileName(mask);
    verz:=ExtractFilePath(mask);
    if verz[Length(verz)]<>'\' then verz:=verz+'\';

    {all files}
    if FindFirst(mask,$23,search)=0 then
    begin
      repeat
        ListOfFiles.AddObject(verz+search.Name,Pointer(search.Size));
      until FindNext(search)<>0;
    end;

    {Subdirectories}
    if FindFirst(verz+'*.*',fadirectory,search)=0 then
    begin
      repeat
        if ((search.Attr and fadirectory)=fadirectory) and (search.name <> '.') and (search.name <> '..') then
        begin
          GetAllFilesOnMask(verz+search.name+'\'+such,ListOfFiles);
        end;
      until FindNext(search) <>0;
      SysUtils.FindClose(search);
    end;
  end;

{Verify all command line parameters}
Var
   I           : Integer;
   InFileSpecs : TStringList; {Input file specifications}
   D,P:String;
Begin
  Result := False;

  InFileSpecs := TStringList.Create;
  Try

    If SysUtils.FileExists(FilenameForZip) Then
      If Not OverWrite Then
      Begin
        Fatal(Format(StrNoOverwiteRights,[FilenameForZip]));
        Exit;
      End;

    OutFileName := FilenameForZip;

    {working DIR}
    D := WorkingDirForRelativePath;
    If D <> '' Then
      If D[Length(D)] <> '\' Then D := D + '\';

    If (D <> '') And DirectoryExists(D) Then
    Begin
      If FilesToZip.Count = 0 Then
        getAllFilesOnMask(D+'*.*',InFileSpecs)
      Else
      Begin
        For I := 0 To FilesToZip.Count - 1 Do
        Begin
          P := ExtractFilePath(FilesToZip[I]);
          If Pos('*',FilesToZip[I]) <> 0  Then
          Begin
            If P = '' Then getAllFilesOnMask(D+FilesToZip[I],InFileSpecs)
          End
          Else
            If FileExists(FilesToZip[I]) Then
              InFileSpecs.Add(FilesToZip[I])
        End;
      End;
    End
    Else
      If FilesToZip.Count = 0 Then
      Begin
        Fatal(StrNoFilesForCompress);
        Exit
      End
      Else
        For I := 0 To FilesToZip.Count - 1 Do
          If FileExists(FilesToZip[I]) Then
              InFileSpecs.Add(FilesToZip[I]);

    GetNames(InFileSpecs);
  Finally
    InFileSpecs.Free;
  End;

  Result := True;

End {ParamCheck};

{ --------------------------------------------------------------------------- }
{ Running 32 Bit CRC update function                                          }
{ --------------------------------------------------------------------------- }

Function UpdC32(Octet: Byte; Crc: {$IFDEF VER5UP}DWORD{$ELSE}LongInt{$ENDIF}) : LongInt;
Begin
  Result := Crc_32_Tab[Byte(Crc XOr {$IFDEF VER5UP}DWORD{$ELSE}LongInt{$ENDIF}(Octet))] XOr ((Crc ShR 8) And $00FFFFFF);
End {UpdC32};

{ --------------------------------------------------------------------------- }
{ I/O Support routines                                                        }
{ --------------------------------------------------------------------------- }

Function GetBuffers:Boolean;
{ Allocate Input and Output buffers                                           }
Begin
  Result:=False;
  try
    GetMem(InBuf, SizeOf(InBuf^))
  except
    on EOutOfMemory do
     Exit;
  end;

  try
    GetMem(OutBuf, SizeOf(OutBuf^))
  except
    on EOutOfMemory do
    Begin
      FreeMem(InBuf, SizeOf(InBuf^));
      Exit
    End;
  end;
  Result := True;
End {GetBuffers};

{ --------------------------------------------------------------------------- }

Procedure DropBuffers;
{ Deallocate input and output buffers                                         }
Begin
   FreeMem(InBuf, SizeOf(InBuf^));
   FreeMem(OutBuf, SizeOf(OutBuf^))
end {DropBuffers};


Procedure OpenOutput;
Var
   RC : Smallint;
Begin
  AssignFile(OutFile, OutFileName);
  FileMode := 66; {fmShareDenyNone or fmOpenReadWrite}
  {$I-} Rewrite(OutFile, 1); {$I+}
  RC := IOResult;
  If RC <> 0 Then
    Fatal(StrOpenOutputFileError);
End {OpenOutput};

{ --------------------------------------------------------------------------- }

Function OpenInput(InFileName : String) : Boolean;
Begin
  AssignFile(InFile, InFileName);
  FileMode := 64;{fmShareDenyNone or fmOpenRead}
  {$I-} Reset(InFile, 1); {$I+}
  OpenInput := (IOResult = 0);
End {OpenInput};

{ --------------------------------------------------------------------------- }

Function CloseOutput: Integer;
Begin
  {$I-} CloseFile(OutFile) {$I+};
  Result := IOResult;
End {CloseOutput};

{ --------------------------------------------------------------------------- }

Function CloseInput: Integer;
Begin
  {$I-} CloseFile(InFile)  {$I+};
  Result := IOResult;
End {CloseInput};

{ --------------------------------------------------------------------------- }

Procedure Read_Block;
{ Read a "block" of data into our our input buffer                            }
Begin
  BlockRead(InFile, InBuf^[1], SizeOf(InBuf^), MaxInBufIdx);

  If MaxInBufIdx = 0 Then
    InputEof := True
  Else
    InputEOF := False;
  InBufIdx := 1;
End {Read_Block};

{ --------------------------------------------------------------------------- }

Procedure Write_Block;
{ Write a block of data from the output buffer to our output file             }
Begin
  BlockWrite(OutFile, OutBuf^[1], Pred(OutBufIdx));
  OutBufIdx := 1;
End {Write_Block};

{ --------------------------------------------------------------------------- }

Procedure PutChar(B : Byte);
{ Put one character into our output buffer                                    }
Begin
  OutBuf^[OutBufIdx] := B;
  Inc(OutBufIdx);
  If OutBufIdx > SizeOf(OutBuf^) Then
    Write_Block;
  Inc(BytesOut);
End {PutChar};

{ --------------------------------------------------------------------------- }

Procedure FlushOutput;
{ Write any data sitting in our output buffer to the output file              }
Begin
  If OutBufIdx > 1 Then
    Write_Block;
End {FlushOutput};

{--------------------------------------------------------------------------- }

Procedure PutCodePas(Code:Smallint);
{kod pro nahradu assembleru}
Var Mask:Word;
    Agent:Byte;
Var iSaveByte,
    iBitsUsed:Byte;
    iCodeSize:Byte;
Begin
  iSaveByte:=SaveByte;
  iBitsUsed:=BitsUsed;
  iCodeSize:=CodeSize;
  If Code=-1 Then Begin
    If iBitsUsed<>0 Then PutChar(SaveByte)
  End
  Else Begin
    Mask:=$0001;{%00000000000000001}
    Repeat
      Agent:=0;
      If (Code And Mask)<>0 Then Inc(Agent);
      Mask := Mask Shl 1;{%0000000000000010}
      Agent := Agent Shl iBitsUsed;
      Inc(iBitsUsed);
      iSaveByte:= iSaveByte Or Agent;
      If iBitsUsed=8 Then Begin
        PutChar(iSaveByte);
        iSaveByte:=0;
        Agent:=0;
        iBitsUsed:=0;
      End;
      Dec(iCodeSize);
    Until iCodeSize=0;
    SaveByte := iSaveByte;
    BitsUsed := iBitsUsed;
  End;
End;

Procedure PutCode(Code : Smallint);
{ Assemble coded bytes for output }
{$IFNDEF VER80}
Begin
  PutCodePas(Code);
{$ELSE}
Var
   PutCharAddr : Pointer;
Begin
  PutCharAddr := @PutChar;
  Inline(
  {;  Register useage:}
  {;}
  {;  AX - holds Code}
  {;  BX - BH is a work register, BL holds SaveByte}
  {;  CX - holds our loop counter CodeSize}
  {;  DX - holds BitsUsed}
  {;}
  $8B/$46/<Code/         {                mov         ax,[bp+<Code]}
  $31/$DB/               {                xor         bx,bx}
  $89/$D9/               {                mov         cx,bx}
  $89/$DA/               {                mov         dx,bx}
  $8A/$1E/>SaveByte/     {                mov         bl,[>SaveByte]}
  $8A/$0E/>CodeSize/     {                mov         cl,[>CodeSize]}
  $8A/$16/>BitsUsed/     {                mov         dl,[>BitsUsed]}
  $3D/$FF/$FF/           {                cmp         ax,-1               ;Any work to do?}
  $75/$0D/               {                jnz         Repeat              ;Yup, go do it}
  $80/$FA/$00/           {                cmp         dl,0                ;Any leftovers?}
  $74/$3A/               {                jz          AllDone             ;Nope, we're done}
  $53/                   {                push        bx                  ;Yup...push leftovers}
  $0E/                   {                push        cs}
  $FF/$96/>PutCharAddr/  {                call        [bp+>PutCharAddr]   ;   and send to output}
  $EB/$32/               {                jmp short   AllDone}
  {;}
  $30/$FF/               {Repeat:         xor         bh,bh               ;Zero out BH}
  $D1/$D8/               {                rcr         ax,1                ;Get low order bit into CY flag}
  $73/$02/               {                jnc         SkipBit             ;Was the bit set?}
  $FE/$C7/               {                inc         bh                  ;Yes, xfer to BH}
  $87/$D1/               {SkipBit:        xchg        cx,dx               ;Swap CX & DX}
  $D2/$E7/               {                shl         bh,cl               ;Shift bit over}
  $87/$D1/               {                xchg        cx,dx               ;Put CX & DX back where they were}
  $42/                   {                inc         dx                  ;Bump count of bit positions used}
  $08/$FB/               {                or          bl,bh               ;Transfer bit to output byte (SaveByte)}
  $83/$FA/$08/           {                cmp         dx,8                ;Full byte yet?}
  $72/$12/               {                jb          GetNext             ;Nope, go get more code bits}
  $50/                   {                push        ax                  ;Yup, save regs in preparation}
  $53/                   {                push        bx                  ;    for call to output routine}
  $51/                   {                push        cx}
  $52/                   {                push        dx}
  $53/                   {                push        bx                  ;Push byte to output onto stack}
  $0E/                   {                push        cs}
  $FF/$96/>PutCharAddr/  {                call        [bp+>PutCharAddr]   ;   and call the output routine}
  $5A/                   {                pop         dx}
  $59/                   {                pop         cx}
  $5B/                   {                pop         bx}
  $58/                   {                pop         ax}
  $31/$DB/               {                xor         bx,bx               ;Prepare SaveByte for next byte}
  $89/$DA/               {                mov         dx,bx               ;Set BitsUsed to zero}
  $E2/$D6/               {GetNext:        loop        Repeat              ;Repeat for all code bits}
  {;}
  $88/$1E/>SaveByte/     {                mov         [>SaveByte],bl      ;Put SaveByte and BitsUsed}
  $88/$16/>BitsUsed);    {                mov         [>BitsUsed],dl      ;   back in memory}
  {;}
  {AllDone:}
{$ENDIF}
End {Putcode};

{ --------------------------------------------------------------------------- }
{ The following routines are used to allocate, initialize, and de-allocate    }
{ various dynamic memory structures used by the LZW compression algorithm     }
{ --------------------------------------------------------------------------- }

Function Build_Data_Structures:Boolean;
Begin
  Result := False;
  try
    GetMem(CodeTable, SizeOf(CodeTable^))
  except
    on EOutOfMemory do
    begin
      Fatal(StrNotEnoughMemoryToAllocStruct);
      Exit;
    end;
  end;

  try
    GetMem(FreeList,  SizeOf(FreeList^ ))
  except
    on EOutOfMemory do
    begin
      Fatal(StrNotEnoughMemoryToAllocStruct);
      FreeMem(CodeTable, SizeOf(CodeTable^));
      Exit;
    end;
  end;
  Result := True;
End {Build_Data_Structures};

{ --------------------------------------------------------------------------- }

Procedure Destroy_Data_Structures;
Begin
   FreeMem(CodeTable, SizeOf(CodeTable^));
   FreeMem(FreeList,  SizeOf(FreeList^ ));
end {Destroy_Data_Structures};


Procedure Initialize_Data_Structures;
Var
   I  :  Word;
Begin
  For I := 0 To TableSize Do Begin
    With CodeTable^[I] Do Begin
      Child     := -1;
      Sibling   := -1;
      If I <= 255 Then
        Suffix := I;
    End {with};
    If I >= 257 Then
      FreeList^[I] := I;
  End {for};

  NextFree  := FIRSTENTRY;
  TableFull := False;

End {Initialize_Data_Structures};

{ --------------------------------------------------------------------------- }
{ The following routines handle manipulation of the LZW Code Table            }
{ --------------------------------------------------------------------------- }

Procedure Prune(Parent : Word);
{ Prune leaves from a subtree - Note: this is a recursive procedure }
Var
   CurrChild   : Smallint;
   NextSibling : Smallint;
Begin
  CurrChild := CodeTable^[Parent].Child;
  { Find first Child that has descendants .. clear any that don't }
  While (CurrChild <> -1) And (CodeTable^[CurrChild].Child = -1) Do Begin
    CodeTable^[Parent].Child := CodeTable^[CurrChild].Sibling;
    CodeTable^[CurrChild].Sibling := -1;
    { Turn on ClearList bit to indicate a cleared entry }
    ClearList[CurrChild Div 8] := (ClearList[CurrChild Div 8] Or (1 ShL (CurrChild Mod 8)));
    CurrChild := CodeTable^[Parent].Child;
  End {while};

  If CurrChild <> -1 Then Begin   { If there are any children left ...}
    Prune(CurrChild);
    NextSibling := CodeTable^[CurrChild].Sibling;
    While NextSibling <> -1 Do Begin
      If CodeTable^[NextSibling].Child = -1 Then Begin
        CodeTable^[CurrChild].Sibling := CodeTable^[NextSibling].Sibling;
        CodeTable^[NextSibling].Sibling := -1;
        { Turn on ClearList bit to indicate a cleared entry }
        ClearList[NextSibling Div 8] := (ClearList[NextSibling Div 8] Or (1 ShL (NextSibling Mod 8)));
        NextSibling := CodeTable^[CurrChild].Sibling;
      End {then}
      Else Begin
        CurrChild := NextSibling;
        Prune(CurrChild);
        NextSibling := CodeTable^[CurrChild].Sibling;
      End {if};
    End {while};
  End {if};

End {Prune};

{ --------------------------------------------------------------------------- }

Procedure Clear_Table;
Var
   Node : Word;
Begin
  FillChar(ClearList, SizeOf(ClearList), $00);
  { Remove all leaf nodes by recursively pruning subtrees}
  For Node := 0 To 255 Do
    Prune(Node);
  { Next, re-initialize our list of free table entries }
  NextFree := Succ(TABLESIZE);
  For Node := TABLESIZE Downto FIRSTENTRY Do Begin
    If (ClearList[Node Div 8] And (1 ShL (Node Mod 8))) <> 0 Then Begin
      Dec(NextFree);
      FreeList^[NextFree] := Node;
    End {if};
  End {for};
  If NextFree <= TABLESIZE Then
    TableFull := False;
End {Clear_Table};

{ --------------------------------------------------------------------------- }

Procedure Table_Add(Prefix : Word; Suffix : Byte);
Var
   FreeNode : Word;
Begin
  If NextFree <= TABLESIZE Then Begin
    FreeNode := FreeList^[NextFree];
    Inc(NextFree);
    CodeTable^[FreeNode].Child := -1;
    CodeTable^[FreeNode].Sibling := -1;
    CodeTable^[FreeNode].Suffix := Suffix;
    If CodeTable^[Prefix].Child  = -1 Then
      CodeTable^[Prefix].Child := FreeNode
    Else Begin
      Prefix := CodeTable^[Prefix].Child;
      While CodeTable^[Prefix].Sibling <> -1 Do
        Prefix := CodeTable^[Prefix].Sibling;
      CodeTable^[Prefix].Sibling := FreeNode;
    End {if};
  End {if};

  If NextFree > TABLESIZE Then
    TableFull := True;
End {Table_Add};

{ --------------------------------------------------------------------------- }

{$IFNDEF VER80}
Function Table_Lookup(TargetPrefix:Smallint;TargetSuffix:Byte;
                        Var FoundAt:Smallint):Boolean;
//this procedure is slower than code in assembler
//pascal code replacing assembler code only
Label Loop;
Var TempChild:Smallint;
Begin
  Table_Lookup := False;
  FoundAt:=-1;
  If CodeTable^[TargetPrefix].Child=-1 Then Exit;{not found}
  TempChild:=CodeTable^[TargetPrefix].Child;
 Loop:
  With CodeTable^[TempChild] Do Begin
    If Suffix=TargetSuffix Then Begin {found}
      FoundAt:=TempChild;
      Table_Lookup:=True;
      Exit
    End;
    If Sibling=-1 Then Exit;{not found}
    TempChild:=Sibling;
  End;
  GoTo Loop;
End;
{$ELSE}
Function Table_Lookup(    TargetPrefix : Smallint;
                          TargetSuffix : Byte;
                      Var FoundAt      : Smallint   ) : Boolean;
{ --------------------------------------------------------------------------- }
{ Search for a Prefix:Suffix pair in our Symbol table.  If found, return the  }
{ index value where found.  If not found, return FALSE and set the VAR parm   }
{ FoundAt to -1.                                                              }
{ --------------------------------------------------------------------------- }
Begin
  Inline(
  {;}
  {; Lookup an entry in the Hash Table.  If found, return TRUE and set the VAR}
  {; parameter FoundAt with the index of the entry at which the match was found.}
  {; If not found, return FALSE and plug a -1 into the FoundAt var.}
  {;}
  {;}
  {; Register usage:}
  {;   AX - varies                     BL - holds target suffix character}
  {;                                   BH - If search fails, determines how to}
  {;                                        add the new entry}
  {;   CX - not used                   DX - holds size of 1 table entry (5)}
  {;   DI - varies                     SI - holds offset of 1st table entry}
  {;   ES - seg addr of hash table     DS - program's data segment}
  {;}
  {;}
  $8A/$5E/<TargetSuffix/ {            mov byte    bl,[bp+<TargetSuffix]   ;Target Suffix character}
  $8B/$46/<TargetPrefix/ {            mov word    ax,[bp+<TargetPrefix]   ;Index into table}
  $BA/$05/$00/           {            mov         dx,5                    ;5 byte table entries}
  $F7/$E2/               {            mul         dx                      ;AX now an offset into table}
  $C4/$3E/>CodeTable/    {            les         di,[>CodeTable]         ;Hash table address}
  $89/$FE/               {            mov         si,di                   ;save offset in SI}
  $01/$C7/               {            add         di,ax                   ;es:di points to table entry}
  {;}
  $B7/$00/               {            mov         bh,0                    ;Chain empty flag (0=empty)}
  $26/$83/$3D/$FF/       {        es: cmp word    [di],-1                 ;Anything on the chain?}
  $74/$33/               {            jz          NotFound                ;Nope, search fails}
  $B7/$01/               {            mov         bh,1                    ;Chain empty flag (1=not empty)}
  {;}
  $26/$8B/$05/           {        es: mov word    ax,[di]                 ;Get index of 1st entry in chain}
  $89/$46/<TargetPrefix/ {Loop:       mov word    [bp+<TargetPrefix],ax   ;Save index for later}
  $BA/$05/$00/           {            mov         dx,5}
  $F7/$E2/               {            mul         dx                      ;convert index to offset}
  $89/$F7/               {            mov         di,si                   ;es:di points to start of table}
  $01/$C7/               {            add         di,ax                   ;es:di points to table entry}
  {;}
  $26/$3A/$5D/$04/       {        es: cmp byte    bl,[di+4]               ;match on suffix?}
  $74/$0D/               {            jz          Found                   ;Yup, search succeeds}
  {;}
  $26/$83/$7D/$02/$FF/   {        es: cmp word    [di+2],-1               ;any more entries in chain?}
  $74/$15/               {            jz          NotFound                ;nope, search fails}
  {;}
  $26/$8B/$45/$02/       {        es: mov word    ax,[di+2]               ;get index of next chain entry}
  $EB/$E1/               {            jmp short   Loop                    ;   and keep searching}
  {;}
  $C6/$46/$FF/$01/       {Found:      mov byte    [bp-1],1                ;return TRUE}
  $C4/$7E/<FoundAt/      {            les         di,[bp+<FoundAt]        ;get address of Var parameter}
  $8B/$46/<TargetPrefix/ {            mov word    ax,[bp+<TargetPrefix]   ;get index of entry where found}
  $26/$89/$05/           {        es: mov         [di],ax                 ;and store it}
  $EB/$0C/               {            jmp short   Done}
  {;}
  $C6/$46/$FF/$00/       {NotFound:   mov byte    [bp-1],0                ;return FALSE}
  $C4/$7E/<FoundAt/      {            les         di,[bp+<FoundAt]        ;get address of Var parameter}
  $26/$C7/$05/$FF/$FF);  {        es: mov word    [di],-1                 ;and store a -1 in it}
  {;}
  {Done:}
  {;}

End {Table_Lookup};
{$ENDIF}

{ --------------------------------------------------------------------------- }
{ These routines build the Header structures for the ZIP file                 }
{ --------------------------------------------------------------------------- }

Procedure Begin_ZIP(ListPtr : NodePtr);
{ Write a dummy header to the zip.  Include as much info as is currently      }
{ known (we'll come back and fill in the rest later...)                       }
Begin
  LocalHdrOfs := FilePos(OutFile);       { Save file position for later use  }
  With LocalHdr Do Begin
    Signature := LOCAL_FILE_HEADER_SIGNATURE;
    Extract_Version_Reqd := {$IfDef useZLIB}20{$Else}10{$EndIf useZLIB};
    Bit_Flag := 0;
    Compress_Method := {$IfDef useZLIB}8{$Else}1{$EndIf useZLIB};
    Last_Mod_Time := ListPtr^.Time;
    Last_Mod_Date := ListPtr^.Date;
    Crc32 := 0;
    Compressed_Size := 0;
    Uncompressed_Size := ListPtr^.Size;
    FileName_Length := Length(ListPtr^.Name);
    Extra_Field_Length := 0;
  End {with};
  Move(LocalHdr, OutBuf^, SizeOf(LocalHdr)); { Put header into output buffer }
  OutBufIdx := Succ(SizeOf(LocalHdr));   {...adjust buffer index accordingly }
  Move(ListPtr^.Name[1], OutBuf^[OutBufIdx], Length(ListPtr^.Name));
  Inc(OutBufIdx, Length(ListPtr^.Name));
  FlushOutput;                           { Write it now                      }
End {Begin_ZIP};

{ --------------------------------------------------------------------------- }

Procedure Update_ZIP_Header(ListPtr : NodePtr);
{ Update the zip's local header with information that we now possess.  Check  }
{ to make sure that our shrinker actually produced a smaller file.  If not,   }
{ scrap the shrunk data, modify the local header accordingly, and just copy   }
{ the input file to the output file (compress method 0 - Storing).            }
Var
   EndPos : LongInt;
   Redo   : Boolean;
Begin
  //Redo := False;                           { Set REDO flag to false         }
  EndPos := FilePos(OutFile);                { Save current file position     }

  Seek(OutFile, LocalHdrOfs);                { Rewind back to file header     }

  With LocalHdr Do Begin
    { Update compressed size field   }
    Compressed_Size := EndPos - LocalHdrOfs - SizeOf(LocalHdr) - Filename_Length;
    Crc32 := Crc32Val;                       { Update CRC value               }
    { Have we compressed the file?   }
    Redo := (Compressed_Size >= Uncompressed_Size);
{$IfDef useZLIB}
    Compress_Method := 8;
    If Redo Then
      If FForceDeflate Then
        Redo := False; {deflate file can be bigger than uncompressed file !}
{$EndIf}
    If Redo Then Begin                       { No...                          }
      Compress_Method := 0;                     { ...change stowage type      }
      Compressed_Size := Uncompressed_Size;     { ...update compressed size   }
    End {if};

  End {with};

  Move(LocalHdr, OutBuf^, SizeOf(LocalHdr));  { Put header into output buffer }
  OutBufIdx := Succ(SizeOf(LocalHdr));    {...adjust buffer index accordingly }
  Move(ListPtr^.Name[1], OutBuf^[OutBufIdx], Length(ListPtr^.Name));
  Inc(OutBufIdx, Length(ListPtr^.Name));
  FlushOutput;                            { Write it now                      }

  If Redo Then Begin
    { If compression didn't make a smaller file, then ...                     }
    Seek(InFile, 0);                         { Rewind the input file          }
    InputEof := False;                       { Reset EOF indicator            }
    Read_Block;                              { Prime the input buffer         }
    While Not InputEof Do Begin              { Copy input to output           }
      BlockWrite(OutFile, InBuf^, MaxInBufIdx);
      Read_Block;
    End {while};
    Truncate(Outfile);                       { Truncate output file           }
  End {then}
  Else Begin
    { Compression DID make a smaller file ...                                 }
    Seek(OutFile, FileSize(OutFile));     { Move output file pos back to eof  }
  End {if};
End {Update_ZIP_Header};

{ --------------------------------------------------------------------------- }

Procedure Build_Central_Dir;
{ Revisit each local file header to build the Central Directory.  When done,  }
{ build the End of Central Directory record.                                  }
Var
   BytesRead : Integer;
   SavePos   : LongInt;
   HdrPos    : LongInt;
   CenDirPos : LongInt;
   pom,
   Entries   : Word;
   FileName  : String;
   pomStr    : array[0..255] of char; {integer}
   tpmCDSize : LongInt;
Begin
  tpmCDSize := 0;
  Entries := 0;
  CenDirPos := FilePos(Outfile);
  Seek(OutFile, 0);             { Rewind output file }
  HdrPos := FilePos(OutFile);
  BlockRead(OutFile, LocalHdr, SizeOf(LocalHdr), BytesRead);
  Repeat
    BlockRead(OutFile, pomstr, LocalHdr.FileName_Length, BytesRead);
    pom:=LocalHdr.FileName_Length;
    if pom>255 then pom:=255;
    pomstr[pom]:=#0;
    FileName:=Strpas(pomstr);
    SavePos := FilePos(OutFile);
    With CentralHdr Do Begin
      Signature := CENTRAL_FILE_HEADER_SIGNATURE;
      MadeBy_Version := LocalHdr.Extract_Version_Reqd;
      Move(LocalHdr.Extract_Version_Reqd, Extract_Version_Reqd, 26);
      File_Comment_Length := 0;
      Starting_Disk_Num := 0;
      Internal_Attributes := 0;
      External_Attributes := faARCHIVE;
      Local_Header_Offset := HdrPos;
      Seek(OutFile, FileSize(OutFile));
      BlockWrite(Outfile, CentralHdr, SizeOf(CentralHdr));
      BlockWrite(OutFile, FileName[1], Length(FileName));
      Inc(tpmCDSize,SizeOf(CentralHdr)+Length(FileName));
      Inc(Entries);
    End {with};

    Seek(OutFile, SavePos + LocalHdr.Compressed_Size);
    HdrPos := FilePos(OutFile);
    BlockRead(OutFile, LocalHdr, SizeOf(LocalHdr), BytesRead);
  Until LocalHdr.Signature = CENTRAL_FILE_HEADER_SIGNATURE;

  Seek(OutFile, FileSize(OutFile));

  With EndHdr Do Begin
    Signature := END_OF_CENTRAL_DIR_SIGNATURE;
    Disk_Number := 0;
    Central_Dir_Start_Disk := 0;
    Entries_This_Disk := Entries;
    Total_Entries := Entries;
    Central_Dir_Size := {CenDirPos - FileSize(OutFile)}tpmCDSize;{fix 31.5.2002 by J.B.}
    Start_Disk_Offset := CenDirPos;
    ZipFile_Comment_Length := 0;
    BlockWrite(Outfile, EndHdr, SizeOf(EndHdr));
  End {with};

End {Build_Central_Dir};

{ --------------------------------------------------------------------------- }
{ The actual Crunching algorithm                                              }
{ --------------------------------------------------------------------------- }
{$J+}
Procedure Shrink(Suffix : Smallint);
Const
   LastCode    : Smallint = 0;   { Typed constant, so value retained across calls }
Var
   WhereFound   : Smallint;
Begin
  If FirstCh Then Begin          { If just getting started ...                }
    SaveByte := $00;             { Initialize our output code buffer          }
    BitsUsed := 0;
    CodeSize := MINBITS;         {     Initialize code size to minimum        }
    MaxCode  := (1 ShL CodeSize) - 1;
    LastCode := Suffix;          {     get first character from input,        }
    FirstCh  := False;           {     and reset the first char flag.         }
  End {then}
  Else Begin
    If Suffix <> -1 Then Begin   { If there's work to do ...                  }
      If TableFull Then Begin
        { Ok, lets clear the code table (adaptive reset)                      }
        Putcode(LastCode);
        PutCode(SPECIAL);
        Putcode(CLEARCODE);
        Clear_Table;
        Table_Add(LastCode, Suffix);
        LastCode := Suffix;
      End {then}
      Else Begin
        If Table_Lookup(LastCode, Suffix, WhereFound) Then Begin
          { If LastCode:Suffix pair is found in the code table, then ...     }
          { ... set LastCode to the entry where the pair is located          }
          LastCode  := WhereFound;
        End {then}
        Else Begin
          { Not in table                                                     }
          PutCode(LastCode);            { Write current LastCode code        }
          Table_Add(LastCode, Suffix);  { Attempt to add to code table       }
          LastCode := Suffix;           { Reset LastCode code for new char   }
          If (FreeList^[NextFree] > MaxCode) And (CodeSize < MaxBits) Then Begin
            { Time to increase the code size and change the max. code        }
            PutCode(SPECIAL);
            PutCode(INCSIZE);
            Inc(CodeSize);
            MaxCode := (1 ShL CodeSize) -1;
          End {if};
        End {if};
      End {if};
    End {then}
    Else Begin                   { Nothing to crunch...must be EOF on input   }
      PutCode(LastCode);         { Write last prefix code                     }
      PutCode(-1);               { Tell putcode to flush remaining bits       }
      FlushOutput;               { Flush our output buffer                    }
    End {if};
  End {if};
End {Crunch};
{$J-}
{ --------------------------------------------------------------------------- }

Procedure Process_Input(Source : String);
Var
   I       : Word;
   PctDone : Smallint;
Begin
  If Source = '' Then
    Shrink(-1)
  Else
    For I := 1 To Length(Source) Do Begin
      Inc(BytesIn);
      PctDone := Round( 100 * ( BytesIn / FileSize(InFile)));
      {nove hlaseni stavu}
      If AsSigned(FOnProgress) Then
        FOnProgress(Self,PctDone);
      CRC32Val := UpdC32(Ord(Source[I]), CRC32Val);
      Shrink(Ord(Source[I]));
    End {for};
End {Process_Input};

{ --------------------------------------------------------------------------- }
{ This routine handles processing for one input file                          }
{ --------------------------------------------------------------------------- }

Procedure Process_One_File;
Var
   OneString : string;
   Remaining : Word;
{$IfDef useZLIB}
   zlib_buff: AnsiString;
   I: Integer;
{$EndIf useZLIB}
Begin

  Read_Block;                 { Prime the input buffer                        }
  FirstCh   := True;          { 1st character flag for Crunch procedure       }
  Crc32Val  := $FFFFFFFF;

  TenPercent := FileSize(InFile) Div 10;

{$IfDef useZLIB}
  If FForceDeflate Then Begin
    Reset(InFile,1);
    SetLength(OneString, FileSize(InFile));
    BlockRead(inFile,OneString[1],FileSize(InFile));
    Crc32Val := $FFFFFFFF;
    For I := 1 To Length(OneString) Do Begin
      Crc32Val := UpdC32(Byte(OneString[I]), Crc32Val);
    End;
    Crc32Val := NOT Crc32Val;

    ZLibCompress(OneString,zlib_buff);

    If AsSigned(FOnProgress) Then
      FOnProgress(Self,100); //all done?

    {write to output directly}
    BlockWrite(OutFile, zlib_buff[1], Length(zlib_buff));
    {all bytes in input stream done}
    BytesIn := FileSize(InFile);
  End
  Else
{$EndIf useZLIB}
  Begin
  While Not InputEof Do Begin
    Remaining := Succ(MaxInBufIdx - InBufIdx);

    If Remaining > 255
     Then Remaining := 255;

    If Remaining = 0
     Then Read_Block
     Else
      Begin
        OneString:=Space(Remaining);
        Move(InBuf^[InBufIdx], OneString[1], Remaining);
        Inc(InBufIdx, Remaining);
        Process_Input(OneString);
      End {if};

  End {while};

  Crc32Val := Not Crc32Val;

  Process_Input('');                             { This forces EOF processing }
  End;
End {Process_One_File};

{ --------------------------------------------------------------------------- }

Procedure Process_All_Files;
Var
   ComprPct : Word;
   ListNode : NodePtr;
Begin
  If ListHead = Nil Then
  Begin
    //'No files for compress !'
    Fatal(StrNoFilesForCompress);
    Exit
  End {if};

  OpenOutput;
  ListNode := ListHead;
  While ListNode <> Nil Do
   Begin
    If OpenInput(Concat(ListNode^.Path, ListNode^.Name))
     Then
      Begin
       BytesIn := 1; BytesOut := 1;
       TenPercent := FileSize(InFile) Div 10;
       Initialize_Data_Structures;
       Begin_ZIP(ListNode);
       Process_One_File;
       Update_ZIP_Header(ListNode);
       CloseInput;
       If LocalHdr.Uncompressed_Size > 0
        Then
         ComprPct := Round((100.0 * (LocalHdr.Uncompressed_Size - LocalHdr.Compressed_Size)) / LocalHdr.Uncompressed_Size)
        Else
         ComprPct := 0;
      End {then}
     Else
       Fatal(Format(StrCannotOpenGoNext,[ListNode^.Name]));
    ListNode := ListNode^.Next;
   End {while};
  Build_Central_Dir;
  CloseOutput;
End {Process_All_Files};

{ Main Program (driver) }

Begin
  If ParamCheck(FOverWrite) Then
  Begin
    If Not GetBuffers Then Exit;       { Allocate input and output buffers ...}

    If Not Build_Data_Structures Then{ ... and other data structures required }
      Exit;
    Try
      Process_All_Files;       { Crunch the file }
    Finally
      DropBuffers;             { Be polite and de-allocate Buffer memory and }
      Destroy_Data_Structures; {    other allocated data structures }
    End;
  End {if};
End;

Procedure TUnZip.UnCrunch(Const ZipFileName, DirToExtract: String; FilesToExtract:TStringList);
Const
  MAXNAMES = 20;
  GloMem:Pointer=Nil;
Var
  InFileSpecif :  TStringList;  { Input file specifications }
  OutPath     :  String;       { Output path specification                    }
  TenPercent  :  LongInt;
  { Define ZIP file header types }
Const
  LOCAL_FILE_HEADER_SIGNATURE = $04034B50;
Type
  Local_File_Header_Type = packed Record
    { Signature              :  LongInt; }
    Extract_Version_Reqd   :  Word;
    Bit_Flag               :  Word;
    Compress_Method        :  Word;
    Last_Mod_Time          :  Word;
    Last_Mod_Date          :  Word;
    Crc32                  :  LongInt;
    Compressed_Size        :  LongInt;
    Uncompressed_Size      :  LongInt;
    Filename_Length        :  Word;
    Extra_Field_Length     :  Word;
  End;

Const
  CENTRAL_FILE_HEADER_SIGNATURE = $02014B50;

Type
  Central_File_Header_Type = packed Record
    { Signature            :  LongInt; }
    MadeBy_Version       :  Word;
    Extract_Version_Reqd :  Word;
    Bit_Flag             :  Word;
    Compress_Method      :  Word;
    Last_Mod_Time        :  Word;
    Last_Mod_Date        :  Word;
    Crc32                :  LongInt;
    Compressed_Size      :  LongInt;
    Uncompressed_Size    :  LongInt;
    Filename_Length      :  Word;
    Extra_Field_Length   :  Word;
    File_Comment_Length  :  Word;
    Starting_Disk_Num    :  Word;
    Internal_Attributes  :  Word;
    External_Attributes  :  LongInt;
    Local_Header_Offset  :  LongInt;
  End;

Const
  END_OF_CENTRAL_DIR_SIGNATURE = $06054B50;

Type
  End_of_Central_Dir_Type =  packed Record
    { Signature               :  LongInt; }
    Disk_Number             :  Word;
    Central_Dir_Start_Disk  :  Word;
    Entries_This_Disk       :  Word;
    Total_Entries           :  Word;
    Central_Dir_Size        :  LongInt;
    Start_Disk_Offset       :  LongInt;
    ZipFile_Comment_Length  :  Word;
  End;

Const
  BufSize = 8192;           { Size of buffers for I/O }

Type
   BufPtr = ^BufType;
   BufType = Array [1..BufSize] Of Byte;

Var
   ZipName       :  String;         { Name of Zip file to be processed }
   ZipFile       :  File;           { Zip file variable }
   EndFile       :  Boolean;        { End of file indicator for ZipFile }
   ZipBuf        :  BufPtr;         { Input buffer for ZipFile }
   ZipPtr        :  Word;           { Index for ZipFile input buffer }
   ZipCount      :  integer;        { Count of bytes in ZipFile input buffer }

   ExtFile       :  File;           { Output file variable }
   ExtBuf        :  BufPtr;         { Output buffer for ExtFile }
   ExtPtr        :  Word;           { Index for ExtFile output buffer }
   ExtCount      :  LongInt;        { Count of characters written to output }

   LocalHdr       : Local_File_Header_Type;  { Storage for a local file hdr }
   Hdr_FileName   : String;
   Hdr_ExtraField : String;
   //Hdr_Comment    : String;  is not used in this version

   Crc32Val      :  LongInt;        { Running CRC (32 bit) value }

   Bytes_To_Go   :  LongInt;        { Bytes left to process in compressed file }


{ Stuff needed for unSHRINKing }

Const
   MINCODESIZE    =    9;
   MAXCODESIZE    =   13;
   SPECIAL        =  256;
   FIRSTFREE      =  257;
   LZW_TABLE_SIZE =  (1 ShL MAXCODESIZE) - 1;      { 0..8191 }
   LZW_STACK_SIZE =  (1 ShL MAXCODESIZE) - 1;      { 0..8191 }

Type
   LZW_Table_Rec  =  Record
     Prefix      :  Smallint;
     Suffix      :  Byte;
     ChildCount  :  Word;  { If ChildCount = 0 then leaf node }
   End;
   LZW_Table_Ptr  =  ^LZW_Table_Type;
   LZW_Table_Type =  Array [0..LZW_TABLE_SIZE] Of LZW_Table_Rec;

   FreeListPtr    =  ^FreeListArray;
   FreeListArray  =  Array [FIRSTFREE..LZW_TABLE_SIZE] Of Word;

   StackPtr       =  ^StackType;
   StackType      =  Array [0..LZW_STACK_SIZE] Of Word;

Var
   LZW_Table   :  LZW_Table_Ptr; { Code table for LZW decoding                }
   FreeList    :  FreeListPtr;   { List of free table entries                 }
   NextFree    :  Word;          { Index for free list array                  }
                                 {   FreeList^[NextFree] always contains the  }
                                 {   index of the next available entry in     }
                                 {   the LZW Prefix:Suffix table (LZW_Table^) }
   LZW_Stack   :  StackPtr;      { A stack used to build decoded strings      }
   StackIdx    :  Word;          { Stack array index variable                 }
                                 {   StackIdx always points to the next       }
                                 {   available entry in the stack             }
   SaveByte    :  Byte;          { Our input code buffer - 1 byte long        }
   BitsLeft    :  Byte;          { Unprocessed bits in the input code buffer  }
   FirstCh     :  Boolean;       { Flag indicating first char being processed }


{ Stuff needed for unREDUCEing }

Const
   MAXDICTSIZE    =  8192;       { size will be 4096 for unreduce and either  }
                                 { 4096 or 8192 for exploding                 }
Type
   FollowerSet    =  Record
     SetSize  :  Word;
     FSet     :  Array [0..31] Of Byte;
   End;
   FollowerPtr    =  ^FollowerArray;
   FollowerArray  =  Array [0..255] Of FollowerSet;

   DictPtr        =  ^DictArray;
   DictArray      =  Array [0..MAXDICTSIZE - 1] Of Byte;

Var
   Followers   :  FollowerPtr;
   Dictionary  :  DictPtr;       { The sliding dictionary }
   DictIdx     :  Word;          { Always points to next pos. to be filled }
   DictSize    :  Word;          { size (in bytes) of sliding dictionary }
   State       :  Byte;
   Len         :  Word;
   V           :  Byte;

{ Stuff needed for unIMPLODEing }

Const
   MAX_SF_TREE_SIZE     =  511;
   LITERAL_TREE_ROOT    =  511;
   DISTANCE_TREE_ROOT   =  127;
   LENGTH_TREE_ROOT     =  127;
Type
   { The following structures are used to define the Shannon-Fano trees used  }
   { in decoding an imploded file                                             }
   SF_Node              =  Record
     LChild   :  Smallint;
     RChild   :  Smallint;
   End;
   SF_Literal_Ptr       =  ^SF_Literal_Array;
   SF_Distance_Ptr      =  ^SF_Distance_Array;
   SF_Length_Ptr        =  ^SF_Length_Array;
   SF_Literal_Array     =  Array [0..LITERAL_TREE_ROOT] Of SF_Node;
   SF_Distance_Array    =  Array [0..DISTANCE_TREE_ROOT] Of SF_Node;
   SF_Length_Array      =  Array [0..LENGTH_TREE_ROOT] Of SF_Node;
   { The Shannon-Fano data that is stored at the beginning of the compressed  }
   { file is itself compressed.  The following structures are used to decode  }
   { that data and build the required Shannon-Fano trees                      }
   SF_BuildRec          =  Record
     Len   :  Byte;
     Val   :  Byte;
     Code  :  Word;
   End;
   SF_BuildPtr          =  ^SF_BuildArray;
   SF_BuildArray        =  Array [0..255] Of SF_BuildRec;
Var
   SF_Literal           :  SF_Literal_Ptr;   { These are the 3 Shannon-Fano   }
   SF_Distance          :  SF_Distance_Ptr;  { trees that are used to implode }
   SF_Length            :  SF_Length_Ptr;    { a file.                        }
   NextFreeLiteral      :  Word;    { Free node pointers used while trees     }
   NextFreeLength       :  Word;    { are being constructed                   }
   NextFreeDistance     :  Word;
   SF_Build             :  SF_BuildPtr;      { Array used in building the     }
                                             { Shannon-Fano trees needed to   }
                                             { decode the imploded file       }
   SF_Build_Idx         :  Byte;    { Index var for SF_Build array            }
   NumOfTrees           :  Byte;    { the # of SF trees needed (2 or 3)       }
   MinMatchLen          :  Byte;    { minimum dictionary match length (2 or 3)}

{ --------------------------------------------------------------------------- }

Procedure Abort (Msg : String);
Begin
  If AsSigned(FOnError) Then FOnError(Self,Msg)
  Else
    MessageDlg(Msg, mtWarning, [mbOk], 0);
End {Abort} ;

{ --------------------------------------------------------------------------- }

Procedure Syntax;
Begin
  Abort(StrBadCallOrBadParams)
End;

{ --------------------------------------------------------------------------- }

//Function HexLInt (L : LongInt) : String;
//Type
//  HexType  = Array [0..15] Of Char;
//Const
//  HexChar : HexType =
//    ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
//Begin
//  Result := HexChar [ (L And $F0000000) ShR 28] +
//  HexChar [ (L And $0F000000) ShR 24] +
//  HexChar [ (L And $00F00000) ShR 20] +
//  HexChar [ (L And $000F0000) ShR 16] +
//  HexChar [ (L And $0000F000) ShR 12] +
//  HexChar [ (L And $00000F00) ShR  8] +
//  HexChar [ (L And $000000F0) ShR  4] +
//  HexChar [ (L And $0000000F)       ] +
//  'h';
//End {HexLInt} ;

{ --------------------------------------------------------------------------- }

Function IO_Test : Boolean;
Var
  ErrorCode : Word;
  Ok        : Boolean;
Begin
  Ok := True;
  ErrorCode := IOResult;
  If ErrorCode <> 0 Then Begin
    Ok := False;
    Case ErrorCode Of
      2 : Abort(StrFileNotFound); //File not found
      3 : Abort(StrIncorrectPathOrNotExists); //Incorrect path or not found
      5 : Abort(StrBadNameOrPathOrSyntax); //Bad file name, path name or file syntax
      101 : Abort(StrDisketteIsFull); //Diskette full
      Else   Abort(Format(StrInputOutputError,[ErrorCode])); //I/O error %d
    End {Case} ;
  End {if} ;
  Result := Ok;
End {IO_Test} ;

{ --------------------------------------------------------------------------- }

Function Initialize:Boolean;
Begin
  ZipBuf:=Nil;
  ExtBuf:=Nil;
  Result := False;
  try
    GetMem(ZipBuf,SizeOf (ZipBuf^))
  except
    on EOutOfMemory do
    begin
      Abort(StrNotEnoughMemoryToAllocStruct);
      Exit;
    end;
  end;
  try
    GetMem(ExtBuf,SizeOf (ExtBuf^))
  except
    on EOutOfMemory do
    begin
      Abort(StrNotEnoughMemoryToAllocStruct);
      FreeMem(ZipBuf,SizeOf (ZipBuf^));
      Exit;
    end;
  end;
  Result := True;
End {Initialize} ;

Procedure Finalize;
Begin
  If ZipBuf<>Nil Then FreeMem(ZipBuf,SizeOf (ZipBuf^));
  If ExtBuf<>Nil Then FreeMem(ExtBuf,SizeOf (ExtBuf^));
End;

{ --------------------------------------------------------------------------- }

{ Converted to Turbo Pascal (tm) V4.0 March, 1988 by J.R.Louvau               }
{ COPYRIGHT (C) 1986 Gary S. Brown.  You may use this program, or             }
{ code or tables extracted from it, as desired without restriction.           }
{                                                                             }
{ First, the polynomial itself and its table of feedback terms.  The          }
{ polynomial is                                                               }
{ X^32+X^26+X^23+X^22+X^16+X^12+X^11+X^10+X^8+X^7+X^5+X^4+X^2+X^1+X^0         }
{                                                                             }
{ Note that we take it "backwards" and put the highest-order term in          }
{ the lowest-order bit.  The X^32 term is "implied"; the LSB is the           }
{ X^31 term, etc.  The X^0 term (usually shown as "+1") results in            }
{ the MSB being 1.                                                            }
{                                                                             }
{ Note that the usual hardware shift register implementation, which           }
{ is what we're using (we're merely optimizing it by doing eight-bit          }
{ chunks at a time) shifts bits into the lowest-order term.  In our           }
{ implementation, that means shifting towards the right.  Why do we           }
{ do it this way?  Because the calculated CRC must be transmitted in          }
{ order from highest-order term to lowest-order term.  UARTs transmit         }
{ characters in order from LSB to MSB.  By storing the CRC this way,          }
{ we hand it to the UART in the order low-byte to high-byte; the UART         }
{ sends each low-bit to hight-bit; and the result is transmission bit         }
{ by bit from highest- to lowest-order term without requiring any bit         }
{ shuffling on our part.  Reception works similarly.                          }
{                                                                             }
{ The feedback terms table consists of 256, 32-bit entries.  Notes:           }
{                                                                             }
{     The table can be generated at runtime if desired; code to do so         }
{     is shown later.  It might not be obvious, but the feedback              }
{     terms simply represent the results of eight shift/xor opera-            }
{     tions for all combinations of data and CRC register values.             }
{                                                                             }
{     The values must be right-shifted by eight bits by the "updcrc"          }
{     logic; the shift must be unsigned (bring in zeroes).  On some           }
{     hardware you could probably optimize the shift in assembler by          }
{     using byte-swap instructions.                                           }
{     polynomial $edb88320                                                    }
{                                                                             }

Function UpdC32 (Octet: Byte; Crc: {$IFDEF VER5UP}DWORD{$ELSE}LongInt{$ENDIF}) : LongInt;
Begin
  Result := CRC_32_TAB [Byte (Crc XOr {$IFDEF VER5UP}DWORD{$ELSE}LongInt{$ENDIF} (Octet) ) ] XOr ( (Crc ShR 8) And $00FFFFFF);
End {UpdC32} ;

{ --------------------------------------------------------------------------- }

Procedure Read_Zip_Block;
Begin
  BlockRead (ZipFile, ZipBuf^, BufSize, ZipCount);
  If ZipCount = 0 Then EndFile := True;
  ZipPtr := 1;
End {Read_Zip_Block} ;

{ --------------------------------------------------------------------------- }

Procedure Write_Ext_Block;
Begin
  If ExtPtr > 1 Then Begin
    BlockWrite (ExtFile, ExtBuf^, Pred (ExtPtr) );
    If Not IO_Test Then {Halt};
    ExtPtr := 1;
  End {if} ;
End {Write_Ext_Block} ;

{ --------------------------------------------------------------------------- }

Procedure Open_Zip;
Begin
  AssignFile (ZipFile, ZipName);
  FileMode := 64;  {fmShareDenyNone or fmOpenRead}
  {$I-} Reset (ZipFile, 1) {$I+} ;
  If Not IO_Test Then {Halt};
  EndFile := False;
  Read_Zip_Block;
End {Open_Zip} ;

{ --------------------------------------------------------------------------- }

Function Open_Ext : Boolean;
Begin
  AssignFile (ExtFile, OutPath + Hdr_FileName);
  FileMode := 66;  {fmShareDenyNone or fmOpenReadWrite}
  {$I-} Rewrite (ExtFile, 1) {$I+} ;
  If Not IO_Test Then Result := False
  Else Begin
    ExtPtr := 1;
    Result := True;
  End {if} ;
End {Open_Ext} ;

{ --------------------------------------------------------------------------- }

Function Get_Zip : Smallint;
Begin
  If ZipPtr > ZipCount Then Read_Zip_Block;
  If EndFile Then Result := - 1
  Else Begin
    Result := ZipBuf^ [ZipPtr];
    Inc (ZipPtr);
  End {if} ;
End {Get_Zip} ;

{ --------------------------------------------------------------------------- }

Procedure Put_Ext (C : Byte);
Begin
  Crc32Val := UpdC32 (C, Crc32Val);
  ExtBuf^ [ExtPtr] := C;
  Inc (ExtPtr);
  Inc (ExtCount);
  If ExtPtr > BufSize Then Write_Ext_Block;
End {Put_Ext} ;

{ --------------------------------------------------------------------------- }

Procedure Close_Zip;
Begin
  {$I-} CloseFile (Zipfile) {$I+} ;
  If IO_Test Then ;
End {Close_Zip} ;

{ --------------------------------------------------------------------------- }

Procedure Close_Ext;
Type
  TimeDateRec = Record
    Time : Word;
    Date : Word;
  End {record} ;
Var
  TimeDate      : TimeDateRec;
  TimeDateStamp : LongInt Absolute TimeDate;
Begin
  Write_Ext_Block;
  TimeDate. Time := LocalHdr. Last_Mod_Time;
  TimeDate. Date := LocalHdr. Last_Mod_Date;
  FileSetDate(TFileRec(ExtFile).Handle, TimeDateStamp);
  {$I-} System.Close (ExtFile) {$I+} ;
  If IO_Test Then ;
End {Close_Ext} ;

{ --------------------------------------------------------------------------- }

Procedure FSkip (Offset : LongInt);
Var
  Rec : LongInt;
Begin
  If (Offset + ZipPtr) <= ZipCount Then Inc (ZipPtr, Offset)
  Else Begin
    Rec := FilePos (ZipFile) + (Offset - (ZipCount - ZipPtr) - 1);
    {$I-} Seek (ZipFile, Rec) {$I+} ;
    If Not IO_Test Then {Halt};
    Read_Zip_Block;
  End {if} ;
End {FSkip} ;

{ --------------------------------------------------------------------------- }

Procedure FReadOld (Var Buf; RecLen : Word);
Var
  I  :  Word;
  B  :  Array [1..MaxInt] Of Byte Absolute Buf;
Begin
  For I := 1 To RecLen Do B [I] := Get_Zip;
End {FRead} ;

Procedure FRead (Var Buf; RecLen : Word);
Type   TB  =  Array [1..MaxInt] Of Byte;
Var
  I  :  Word;
  pB :  ^TB;
Begin
  pB:=@Buf;
  For I := 1 To RecLen Do begin pB^[i]:= Get_Zip; end;
End {FRead} ;

Procedure FReadStr (Var Buf :string; RecLen : Word);
Var
  I  :  Word;
  ch :  char;
Begin
  Buf:='';
  For I := 1 To RecLen Do
  begin
    ch:=chr(Get_Zip);
    if i<=255 then Buf:=Buf + ch;
  end;
End {FRead} ;

{ --------------------------------------------------------------------------- }

Function Read_Local_Hdr : Boolean;
Var
  Sig: LongInt;
Begin
  If EndFile Then Result := False
  Else Begin
    FRead (Sig, SizeOf (Sig) );
    If Sig = CENTRAL_FILE_HEADER_SIGNATURE Then Begin
      Result := False;
      EndFile        := True;
    End {then}
    Else Begin
      If Sig <> LOCAL_FILE_HEADER_SIGNATURE Then
        Abort (Format(StrHeaderMissingOrDamaged, [ZipName]));
      FRead (LocalHdr, SizeOf (LocalHdr) );
      With LocalHdr Do
       Begin
        If LocalHdr.FileName_Length > 255 Then
          Abort (StrInvalidFileName);
        FReadStr (Hdr_FileName, LocalHdr.FileName_Length);
        If LocalHdr.Extra_Field_Length > 255 Then
          Abort (StrInvalidFileName);
        FReadStr (Hdr_ExtraField, LocalHdr.Extra_Field_Length);
       End {with} ;
      Result := True;
    End {if} ;
  End {if} ;
End {Read_Local_Hdr} ;

{ --------------------------------------------------------------------------- }

Function Get_Compressed : Smallint;
Var
  PctDone: Smallint;
Begin
  If Bytes_To_Go = 0 Then Result := - 1
  Else Begin
    Result := Get_Zip;
    PctDone := 100 - Round ( 100 * (Bytes_To_Go / LocalHdr. Compressed_Size) );
    {zde je volana udalost pro upravu nejakeho meridla}
    {call gauge event here}
    If AsSigned(FOnProgress) Then
      FOnProgress(Self,PctDone);
    Dec (Bytes_To_Go);
  End {if} ;
End {Get_Compressed} ;

{ --------------------------------------------------------------------------- }

Function LZW_Init : Boolean;
Var
  I: Word;
Begin
  { Initialize LZW Table }
  try
    GetMem(LZW_Table, SizeOf (LZW_Table^))
  except
    on EOutOfMemory do
    Begin
      Result := False;
      Exit;
    End {if} ;
  end;

  For I := 0 To LZW_TABLE_SIZE Do With LZW_Table^ [I] Do Begin
    Prefix     := - 1;
    If I < 256 Then Suffix  := I Else Suffix  := 0;
    ChildCount := 0;
  End {with-for} ;

  try
    GetMem(FreeList, SizeOf (FreeList^))
  except
    on EOutOfMemory do
    Begin
      Result := False;
      {pripadne alokovanou tabulku uvolni}
      FreeMem(LZW_Table, SizeOf (LZW_Table^));
      Exit;
    End;
  end;

  For I := FIRSTFREE To LZW_TABLE_SIZE Do FreeList^ [I] := I;
  NextFree := FIRSTFREE;
  { Initialize the LZW Character Stack }

  try
    GetMem(LZW_Stack,SizeOf(LZW_Stack^))
  except
    on EOutOfMemory do
    Begin
      {pripadne alokovanou tabulku uvolni}
      FreeMem(LZW_Table, SizeOf (LZW_Table^));
      FreeMem(FreeList, SizeOf (FreeList^));
      Exit;
    End;
  end;

  StackIdx := 0;
  Result := True;
End {LZW_Init} ;

{ --------------------------------------------------------------------------- }

Procedure LZW_Cleanup;
Begin
  FreeMem(LZW_Table,SizeOf (LZW_Table^));
  FreeMem(FreeList,SizeOf (FreeList^));
  FreeMem(LZW_Stack,SizeOf (LZW_Stack^));
End {LZW_Cleanup} ;

{ --------------------------------------------------------------------------- }

Procedure Clear_LZW_Table;
Var
   I      :  Word;
Begin
  StackIdx := 0;
  For I := FIRSTFREE To LZW_TABLE_SIZE Do Begin      { Find all leaf nodes }
    If LZW_Table^ [I].ChildCount = 0 Then Begin
      LZW_Stack^ [StackIdx] := I;                   { and put each on stack }
      Inc (StackIdx);
    End {if} ;
  End {for} ;
  NextFree := Succ (LZW_TABLE_SIZE);
  While StackIdx > 0 Do Begin                        { clear all leaf nodes }
    Dec (StackIdx);
    I := LZW_Stack^ [StackIdx];
    With LZW_Table^ [I] Do Begin
      If LZW_Table^ [I].Prefix <> - 1 Then
        Dec (LZW_Table^ [Prefix].ChildCount);
      Prefix     := - 1;
      Suffix     :=  0;
      ChildCount :=  0;
    End {with} ;
    Dec (NextFree);                         { add cleared nodes to freelist }
    FreeList^ [NextFree] := I;
  End {while} ;
End {Clear_LZW_Table} ;

{ --------------------------------------------------------------------------- }

Procedure Add_To_LZW_Table (Prefix : Smallint; Suffix : Byte);
Var
  I  :  Word;
Begin
  If NextFree <= LZW_TABLE_SIZE Then Begin
    I := FreeList^ [NextFree];
    Inc (NextFree);
    LZW_Table^ [I].Prefix     := Prefix;
    LZW_Table^ [I].Suffix     := Suffix;
    Inc (LZW_Table^ [Prefix].ChildCount);
  End {if} ;
End {Add_To_LZW_Table} ;

{ --------------------------------------------------------------------------- }
{$J+}
Function GetCode (CodeSize : Byte) : Smallint;
Const
  Mask : Array [1..8] Of Byte = ($01,$03,$07,$0F,$1F,$3F,$7F,$FF);
  TmpInt : Smallint = 0;
Var
  BitsNeeded : Byte;
  HowMany    : Byte;
  HoldCode   : Smallint;
Label
  Exit;
Begin
  If FirstCh Then Begin                  { If first time through ...         }
    TmpInt := Get_Compressed;            { ... then prime the code buffer    }
    If TmpInt = - 1 Then Begin           { If EOF on fill attempt ...        }
      GetCode := - 1;                    { ... then return EOF indicator ... }
      Goto Exit;                         { ... and return to caller.         }
    End {if} ;
    SaveByte := TmpInt;
    BitsLeft := 8;                       { there's now 8 bits in our buffer  }
    FirstCh  := False;
  End {if} ;
  BitsNeeded := CodeSize;
  HoldCode   := 0;
  While (BitsNeeded > 0) And (TmpInt <> - 1) Do Begin
    If BitsNeeded >= BitsLeft Then HowMany := BitsLeft{ HowMany <-- Min(BitsLeft, BitsNeeded) }
    Else HowMany := BitsNeeded;
    HoldCode := HoldCode Or ( (SaveByte And Mask [HowMany] ) ShL (CodeSize - BitsNeeded) );
    SaveByte := SaveByte ShR HowMany;
    Dec (BitsNeeded, HowMany);
    Dec (BitsLeft, HowMany);
    If BitsLeft <= 0 Then Begin          { If no bits left in buffer ...     }
      TmpInt := Get_Compressed;          { ... then attempt to get 8 more.   }
      If TmpInt = - 1 Then
        Goto Exit;
      SaveByte := TmpInt;
      BitsLeft := 8;
    End {if} ;
  End {while} ;
  Exit:
  If (BitsNeeded = 0) Then                  { If we got what we came for ... }
    GetCode := HoldCode                     { ... then return it             }
  Else
    GetCode := - 1;                         { ... Otherwise, return EOF      }
End {GetCode} ;
{$J-}
{ --------------------------------------------------------------------------- }

Procedure UnShrink;
Var
  CodeSize :  Byte;          { Current size (in bits) of codes coming in  }
  CurrCode :  Smallint;
  SaveCode :  Smallint;
  PrevCode :  Smallint;
  BaseChar :  Byte;
Begin
  CodeSize := MINCODESIZE;               { Start with the smallest code size }
  PrevCode := GetCode (CodeSize);        { Get first code from file          }
  If PrevCode = - 1 Then                 { If EOF already, then ...          }
    Exit;                                { ... just exit without further ado }
  BaseChar := PrevCode;
  Put_Ext (BaseChar);                    { Unpack the first character        }
  CurrCode := GetCode (CodeSize);        { Get next code to prime the while loop }
  While CurrCode <> - 1 Do Begin         { Repeat for all compressed bytes   }
    If CurrCode = SPECIAL Then Begin     { If we've got a "special" code ... }
      CurrCode := GetCode (CodeSize);
      Case CurrCode Of
        1: Begin                         { ... and if followed by a 1 ...    }
          Inc (CodeSize);                { ... then increase code size       }
        End {1} ;
        2: Begin                         { ... and if followed by a 2 ...    }
          Clear_LZW_Table;               { ... clear leaf nodes in the table }
        End {2} ;
        Else  Begin                      { ... if neither 1 or 2, discard    }
          Abort(StrBadCodeBindingCont);
        End {else} ;
      End {case} ;
    End {then}
    Else Begin                          { Not a "special" code              }
      SaveCode := CurrCode;            { Save this code someplace safe...  }
      If CurrCode > LZW_TABLE_SIZE Then
        Abort(StrCodeIsBad);
      If (CurrCode >= FIRSTFREE) And (LZW_Table^ [CurrCode].Prefix = - 1) Then Begin
        If StackIdx > LZW_STACK_SIZE Then Begin
          Write_Ext_Block;
          Abort(Format(StrSorryStackOwerflow,[StackIdx]));
          Exit;{musis vystoupit}
        End {if} ;
        LZW_Stack^ [StackIdx] := BaseChar;
        Inc (StackIdx);
        CurrCode := PrevCode;
      End {if} ;
      While CurrCode >= FIRSTFREE Do Begin
        If StackIdx > LZW_STACK_SIZE Then Begin
          Write_Ext_Block;
          Abort(Format(StrSorryStackOwerflow,[StackIdx]));
          Exit;{musis vystoupit}
        End {if} ;
        LZW_Stack^ [StackIdx] := LZW_Table^ [CurrCode].Suffix;
        Inc (StackIdx);
        CurrCode := LZW_Table^ [CurrCode].Prefix;
      End {while} ;
      BaseChar := LZW_Table^ [CurrCode].Suffix;   { Get last character ...   }
      Put_Ext (BaseChar);
      While (StackIdx > 0) Do Begin
        Dec (StackIdx);
        Put_Ext (LZW_Stack^ [StackIdx] );
      End {while} ;                      { ... until there are none left     }
      Add_to_LZW_Table (PrevCode, BaseChar);   { Add new entry to table      }

      PrevCode := SaveCode;

    End {if} ;

    CurrCode := GetCode (CodeSize);      { Get next code from input stream   }

  End {while} ;

End {UnShrink} ;

{ --------------------------------------------------------------------------- }

Function Init_UnReduce : Boolean;
Begin
  Result := False;
  try
    GetMem(Followers, SizeOf (Followers^))
  except
    on EOutOfMemory do Exit
  end;
  DictSize := 4096;
  try
    GetMem(Dictionary,DictSize)
  except
    on EOutOfMemory do
    Begin
      {uvolni jiz alokovane tabulky}
      FreeMem(Followers, SizeOf (Followers^));
      Exit;
    End;
  end;
  Result := True;
End {Init_UnReduce} ;

{ --------------------------------------------------------------------------- }

Procedure Cleanup_UnReduce;
Begin
  FreeMem(Followers,SizeOf (Followers^));
  FreeMem(Dictionary,DictSize);
End {Cleanup_UnReduce} ;

{ --------------------------------------------------------------------------- }

Function D (X, Y : Byte) : Word;
Var
  tmp : LongInt;
Begin
  X := X ShR (8 - Pred (LocalHdr. Compress_Method) );
  Tmp := X * 256;
  D := Tmp + Y + 1;
End {D} ;

{ --------------------------------------------------------------------------- }

Function F (X : Word) : Byte;
Const
  TestVal : Array [1..4] Of Byte = (127, 63, 31, 15);
Begin
  If X = TestVal [Pred (LocalHdr. Compress_Method) ] Then F := 2 Else F := 3;
End {F} ;

{ --------------------------------------------------------------------------- }

Function L (X : Byte) : Byte;
Const
   Mask : Array [1..4] Of Byte = ($7F, $3F, $1F, $0F);
Begin
  L := X And Mask [Pred (LocalHdr. Compress_Method) ];
End {L} ;

{ --------------------------------------------------------------------------- }

Procedure UpdateDictionary (C : Byte);
Begin
  Put_Ext (C);
  Dictionary^ [DictIdx] := C;
  DictIdx := Succ (DictIdx) Mod DictSize;
End {UpdateDictionary} ;

{ --------------------------------------------------------------------------- }

Procedure DictionaryInit;
Begin
  State := 0;
  FillChar (Dictionary^ [0], DictSize, $00);
  DictIdx := 0;
End {DictionaryInit} ;

{ --------------------------------------------------------------------------- }

Procedure UnScrnch (C : Byte);
Const
   DLE   =  $90;
Var
   S           :  Smallint;
   Count       :  Word;
   OneByte     :  Byte;
   Tmp1        :  LongInt;
Begin
  Case State Of
    0:If C = DLE Then State := 1 Else UpdateDictionary (C);
    1: Begin
      If C = 0 Then Begin
        UpdateDictionary (DLE);
        State := 0;
      End {then}
      Else Begin
        V     := C;
        Len   := L (V);
        State := F (Len);
      End {if} ;
    End {1} ;
    2: Begin
      Inc (Len, C);
      State := 3;
    End {2} ;
    3: Begin
      Tmp1 := D (V, C);
      S    := DictIdx - Tmp1;
      If S < 0 Then
        S := S + DictSize;
      Count := Len + 3;
      While Count > 0 Do Begin
        OneByte := Dictionary^ [S];
        UpdateDictionary (OneByte);
        S := Succ (S) Mod DictSize;
        Dec (Count);
      End {while} ;
      State := 0;
    End {3} ;
  End {case} ;
End {UnScrnch} ;

{ --------------------------------------------------------------------------- }

Function MinBits (Val : Byte) : Byte;
Begin
  Dec (Val);
  Case Val Of
    0..1  : MinBits := 1;
    2..3  : MinBits := 2;
    4..7  : MinBits := 3;
    8..15 : MinBits := 4;
    16..31: MinBits := 5;
  Else
    MinBits := 6;
  End {case} ;
End {MinBits} ;

{ --------------------------------------------------------------------------- }

Procedure UnReduce;
Var
  LastChar    :  Byte;
  N           :  Byte;
  I, J        :  Word;
  Code        :  Smallint;
Begin
  For I := 255 Downto 0 Do { Load follower sets }
  Begin
    N := GetCode (6);      { Get size of 1st set }
    Followers^ [I].SetSize := N;
    If N > 0 Then
      For J := 0 To Pred (N) Do
        Followers^ [I].FSet [J] := GetCode (8);
  End {for} ;
  DictionaryInit;
  LastChar := 0;
  Repeat
    If Followers^ [LastChar].SetSize = 0 Then
    Begin
      Code := GetCode (8);
      UnScrnch (Code);
      LastChar := Code;
    End {then}
    Else
    Begin
      Code := GetCode (1);
      If Code <> 0 Then
      Begin
        Code := GetCode (8);
        UnScrnch (Code);
        LastChar := Code;
      End {then}
      Else
      Begin
        I := MinBits (Followers^ [LastChar].SetSize);
        Code := GetCode (I);
        UnScrnch (Followers^ [LastChar].FSet [Code] );
        LastChar := Followers^ [LastChar].FSet [Code];
      End {if} ;
    End {if} ;
  Until (ExtCount = LocalHdr. Uncompressed_Size);
  FreeMem(Followers,SizeOf (Followers^))
End {UnReduce} ;

{ --------------------------------------------------------------------------- }

Function Init_Explode: Boolean;
{ Get ready to unimplode }
Begin
  Result := False;
  { Extract pertinent info from the general purpose bit flag                 }
  DictSize    := ( ( (LocalHdr. Bit_Flag ShR 1) And $01) * 4096) + 4096;
  NumOfTrees  := ( ( LocalHdr. Bit_Flag ShR 2) And $01) + 2;
  MinMatchLen := NumOfTrees;
  { Allocate memory for the Length & Distance Shannon-Fano trees             }
  try
    GetMem(SF_Length,SizeOf(SF_Length^))
  except
    on EOutOfMemory do Exit
  end;
  try
    GetMem(SF_Distance,SizeOf(SF_Distance^))
  except
    on EOutOfMemory do
    Begin
      FreeMem(SF_Length,SizeOf(SF_Length^)); {uvolni jiz alokovane tabulky}
      Exit;
    End;
  end;
  { Initialize Length & Distance nodes to all -1's and set the Next Free     }
  { Node pointers for each                                                   }
  FillChar (SF_Length^,   SizeOf (SF_Length^),   $FF);
  NextFreeLength   := Pred (LENGTH_TREE_ROOT);
  FillChar (SF_Distance^, SizeOf (SF_Distance^), $FF);
  NextFreeDistance := Pred (DISTANCE_TREE_ROOT);
  { If we need a literal tree, then allocate the memory , initialize the     }
  { nodes to all -1's, and set the Next Free Node pointer                    }
  SF_Literal:=Nil;{indikace nepouziti}
  If NumOfTrees = 3 Then
  Begin
    try
      GetMem(SF_Literal,SizeOf(SF_Literal^))
    except
      on EOutOfMemory do
      Begin
        {uvolni jiz alokovane tabulky}
        FreeMem(SF_Length,SizeOf(SF_Length^));
        FreeMem(SF_Distance,SizeOf(SF_Distance^));
        Exit;
      End;
    end;
    FillChar (SF_Literal^, SizeOf (SF_Literal^), $FF);
    NextFreeLiteral := Pred (LITERAL_TREE_ROOT);
  End {if} ;
  { Allocate memory for the sliding dictionary                               }
  try
    GetMem(Dictionary,DictSize)
  except
    on EOutOfMemory do
    Begin
      {uvolni jiz alokovane tabulky}
      FreeMem(SF_Length,SizeOf(SF_Length^));
      FreeMem(SF_Distance,SizeOf(SF_Distance^));
      If SF_Literal<>Nil Then FreeMem(SF_Literal,SizeOf(SF_Literal^));
      Exit;
    End;
  end;
  { Allocate memory for the array used in building the SF-Trees              }
  try
    GetMem(SF_Build,SizeOf(SF_Build^))
  except
    on EOutOfMemory do
    Begin
      {uvolni jiz alokovane tabulky}
      FreeMem(SF_Length,SizeOf(SF_Length^));
      FreeMem(SF_Distance,SizeOf(SF_Distance^));
      If SF_Literal<>Nil Then FreeMem(SF_Literal,SizeOf(SF_Literal^));
      FreeMem(Dictionary,DictSize);
      Exit;
    End;
  end;
  { If any memory allocations failed, deallocate any memory that may have    }
  { been successfully allocated.                                             }
  { Return either success or failure }
  Result := True;
End { Init_Explode } ;

{ --------------------------------------------------------------------------- }

Procedure Cleanup_Explode;
{ Clean things up after unimploding a file }
Begin
  FreeMem(SF_Length,SizeOf(SF_Length^));
  FreeMem(SF_Distance,SizeOf(SF_Distance^));
  If SF_Literal<>Nil Then FreeMem(SF_Literal,SizeOf(SF_Literal^));
  FreeMem(Dictionary,DictSize);
  FreeMem(SF_Build,SizeOf(SF_Build^))
End { Cleanup_Explode } ;

{ --------------------------------------------------------------------------- }

Procedure Bad_SF_Tree;
Begin
  {Chybný Shannon-Fano dekódovací strom !}
  Abort (StrBadShannonFanoDecodeTree);
End { Bad_SF_Tree } ;

{ --------------------------------------------------------------------------- }

Procedure Add_SF_SubTree ( Var SF_Tree;
                           Var SF_NextFree    : Word;
                              SF_Root         : Word;
                              SF_Code         : Word;
                              SF_Code_Length  : Byte;
                              SF_Value        : Byte );
{ Add the subtree defined by SF_Code to a Shannon-Fano tree                   }
Var
  SF_Array :  Array [0..MAX_SF_TREE_SIZE] Of SF_Node Absolute SF_Tree;
  CurrNode :  Word;
  LastLeaf :  Word;
  I        :  Byte;
Begin
  { The Shannon-Fano tree is implemented as an array of records. Each        }
  { record contains both left and right pointers (ie. this is a binary       }
  { tree).  The root of the tree is the last array element. The first N      }
  { elements (0..N-1) are defined to be the "leaves" of the tree (ie. they   }
  { represent the characters that the decode algorithm will generate).  N    }
  { may be 64 (for the length tree), 128 (for the distance tree), or 256     }
  { (for the Literal tree). The remaining elements of the array are used to  }
  { represent the non-leaf and non-root nodes of the tree.                   }
  CurrNode := SF_Root;
  LastLeaf := Pred (Succ (SF_Root) Div 2);
  { All bits in the code except the least significant define non-leaf nodes  }
  { in the tree.  Process these first.                                       }
  For I := Pred (SF_Code_Length) Downto 1 Do Begin
    If CurrNode <= LastLeaf Then Bad_SF_Tree;
    If Boolean ( (SF_Code ShR I) And $0001) Then Begin   { if the bit is a 1  }
      If SF_Array [CurrNode].RChild = - 1 Then Begin    { no RChild yet      }
        SF_Array [CurrNode].RChild := SF_NextFree;
        Dec (SF_NextFree);
      End {if} ;
      CurrNode := SF_Array [CurrNode].RChild;       { on 1 bits, follow the }
      { right subtree         }
    End { then }
    Else Begin                                         { the bit is a 0     }
      If SF_Array [CurrNode].LChild = - 1 Then Begin    { no LChild yet      }
        SF_Array [CurrNode].LChild := SF_NextFree;
        Dec (SF_NextFree);
      End {if} ;
      CurrNode := SF_Array [CurrNode].LChild;       { on 0 bits, follow the }
      { left subtree          }
    End { if } ;
  End { for } ;
  { All that's left now is to process the least significant bit of the code. }
  { This will define a leaf node.  The leaf node to be linked is defined by  }
  { the SF_Value that is passed to the procedure.                            }
  If Boolean (SF_Code And $0001) Then
    If SF_Array [CurrNode].RChild <> - 1 Then
      Bad_SF_Tree
    Else
      SF_Array [CurrNode].RChild := SF_Value
  Else
    If SF_Array [CurrNode].LChild <> - 1 Then
      Bad_SF_Tree
    Else
      SF_Array [CurrNode].LChild := SF_Value;
End { Add_SF_SubTree } ;

{ --------------------------------------------------------------------------- }

Procedure Sort_SF_Build_Array ( Count : Word );

   Procedure Exchange (Var Node1, Node2 : SF_BuildRec);
   Var
      Node3 : SF_BuildRec;
   Begin
     Node3.Len  := Node1.Len;
     Node3.Val  := Node1.Val;
     { Node3.Code := Node1.Code; }   { the Code field is irrelevant at this point }
     Node1.Len  := Node2.Len;
     Node1.Val  := Node2.Val;
     { Node1.Code := Node2.Code; }   { ditto }
     Node2.Len  := Node3.Len;
     Node2.Val  := Node3.Val;
     { Node2.Code := Node3.Code; }   { ditto again }
   End { Exchange } ;

   Function ShouldSwap ( P1, P2 : SF_BuildRec ) : Boolean;
   Begin
     ShouldSwap := (P1.Len>P2.Len) Or ((P1.Len=P2.Len) And (P1.Val>P2.Val))
   End { ShouldSwap } ;

   Procedure Sort (lb, ub : Smallint);

   (***** BUBBLE SORT **************************************************)

   (*  The list is scanned repeatedly, and adjacent items that are out of
       order are swapped.  When a pass occurs with no swaps, the list is
       sorted.  *)

   Var
     swapped : Boolean;
     cell    : Smallint;
   Begin
     Repeat
       swapped := False;
       For cell := lb To ub - 1 Do Begin
         If ShouldSwap (SF_Build^ [cell], SF_Build^ [cell + 1] ) Then Begin
           Exchange (SF_Build^ [cell], SF_Build^ [cell + 1] );
           swapped := True;
         End;
       End;
     Until (swapped = False);
   End;

Begin
  Sort (0, Count);
End { Sort_SF_Build_Array } ;

{ --------------------------------------------------------------------------- }

Procedure Build_SF_Trees;
{ Extract SF data from an imploded file and build the required SF trees }
Var
  OneByte              :  Byte;    { These "misc" variables are also used in }
  CodeLen              :  Byte;    { building the SF trees                   }
  CodeCount            :  Byte;
  SF_Table_Codes       :  Word;    { # of bytes representing SF tree data - 1}
  BuildCount           :  Word;    { total entries in SF_Build array         }
  Code                 :  Word;    { These three variables used in           }
  CodeIncrement        :  Word;    { constructing the Shannon-Fano codes     }
  LastBitLength        :  Word;    { that will be used to build the SF trees }
  WhichTree            :  Word;    { Counter indicating which SF tree is     }
                                   {   currently under construction          }
  SF_Tree              :  Pointer;
  SF_NextFree          :  Word;
  SF_Root              :  Word;
  I, J                 :  Word;    { Generic loop counter                    }
Begin
  For WhichTree := 1 To NumOfTrees Do Begin
    { Before we go any further, determine which subtree-add procedure       }
    { parameters will be needed on the call to Add_SF_SubTree               }
    Case NumOfTrees Of
      2:
        Case WhichTree Of
          1: Begin
            SF_Tree     := SF_Length;
            SF_NextFree := NextFreeLength;
            SF_Root     := LENGTH_TREE_ROOT;
          End { 1 } ;
          2: Begin
            SF_Tree     := SF_Distance;
            SF_NextFree := NextFreeDistance;
            SF_Root     := DISTANCE_TREE_ROOT;
          End { 2 } ;
        End { case whichtree } ;
      3:
        Case WhichTree Of
          1: Begin
            SF_Tree     := SF_Literal;
            SF_NextFree := NextFreeLiteral;
            SF_Root     := LITERAL_TREE_ROOT;
          End { 1 } ;
          2: Begin
            SF_Tree     := SF_Length;
            SF_NextFree := NextFreeLength;
            SF_Root     := LENGTH_TREE_ROOT;
          End { 2 } ;
          3: Begin
            SF_Tree     := SF_Distance;
            SF_NextFree := NextFreeDistance;
            SF_Root     := DISTANCE_TREE_ROOT;
          End { 3 } ;
        End { case whichtree } ;
    End { case numoftrees } ;
    { Build the Shannon-Fano tree                                           }
    SF_Build_Idx   := 0;
    BuildCount     := 0;
    SF_Table_Codes := GetCode (8);
    For I := 0 To SF_Table_Codes Do Begin
      { Load the SF_Build array with data from the compressed file         }
      OneByte     := GetCode (8);
      CodeLen     := (OneByte And $0F) + 1;
      CodeCount   := (OneByte ShR 4);
      For J := 0 To CodeCount Do Begin
        SF_Build^ [SF_Build_Idx].Len  := CodeLen;
        SF_Build^ [SF_Build_Idx].Val  := SF_Build_Idx;
        Inc (SF_Build_Idx);
      End { for J } ;
    End { for I } ;
    BuildCount := Pred (SF_Build_Idx);
    { Sort the SF_Build Array based on the Len field                        }
    Sort_SF_Build_Array (BuildCount);
    { Generate the SF codes that will be used to grow the SF tree using the }
    { algorithm outlined in the AppNote.Txt file (as distributed within the }
    { PKZip v1.0 self extracting ZIP archive).                              }
    Code           := 0;
    CodeIncrement  := 0;
    LastBitLength  := 0;
    For I := BuildCount Downto 0 Do Begin
      Inc (Code, CodeIncrement);
      If SF_Build^ [I].Len <> LastBitLength Then Begin
        LastBitLength := SF_Build^ [I].Len;
        CodeIncrement := 1 ShL (16 - LastBitLength);
      End {if} ;
      SF_Build^ [I].Code := Code ShR (16 - SF_Build^ [I].Len);
      { Ok, we've got a value and a code.  This represents a subtree in    }
      { the Shannon-Fano tree structure.  Add it to the appropriate tree.  }
      Add_SF_SubTree(SF_Tree^,SF_NextFree,SF_Root,SF_Build^[I].Code,
        SF_Build^[I].Len,SF_Build^[I].Val);
    End { for buildcount } ;
  End { for whichtree } ;
End { Build_SF_Trees } ;

{ --------------------------------------------------------------------------- }

Procedure Bad_SF_Data;
Begin
  {Chybný Shannon-Fano kód !}
  Abort (StrBadShannonFanoDecodeTree);
End { Bad_SF_Tree } ;

{ --------------------------------------------------------------------------- }

Function Decode_SF_Data(Var SF_Tree;SF_Root : Word ) : Byte;
{ Read bits from the input file and decode them using one of the 3 possible   }
{ Shannon-Fano trees.  The method is idential to that used in decoding files  }
{ encoded with the Huffman method (popularaly known as "squeezing") in that   }
{ the tree is traced from the root to either the right or left depending on   }
{ the last bit read until finally, one encounteres a leaf node.               }
Var
  SF_Array :  Array [0..MAX_SF_TREE_SIZE] Of SF_Node Absolute SF_Tree;
  OneBit   :  Byte;
  CurrNode :  Word;
  LastLeaf :  Word;
Begin
  CurrNode := SF_Root; { We start traversing the tree from it's root node    }
  LastLeaf := Pred (Succ (SF_Root) Div 2);
  While CurrNode > LastLeaf Do Begin
    { Walk the tree until you hit a leaf node                               }
    OneBit := GetCode (1);
    If Boolean (OneBit And $01) Then        { if the bit is a 1 ...          }
      If SF_Array [CurrNode].RChild = - 1 Then
        Bad_SF_Data
      Else
        CurrNode := SF_Array [CurrNode].RChild
    Else
      If SF_Array [CurrNode].LChild = - 1 Then
        Bad_SF_Data
      Else
        CurrNode := SF_Array [CurrNode].LChild
  End { while } ;
  Decode_SF_Data := CurrNode;
End { Decode_SF_Data } ;

{ --------------------------------------------------------------------------- }

Procedure Explode;
Var
  OneByte     :  Byte;
  Literal     :  Byte;
  Length      :  Word;
  DistVal     :  Word;
  Distance    :  Word;
  DictStart   :  Smallint;
Begin
  Build_SF_Trees;
  DictionaryInit;
  Repeat
    OneByte := GetCode (1);
    If OneByte <> 0 Then Begin
      { This is literal data ... no dictionary lookup involved          }
      If NumOfTrees = 3 Then
        Literal := Decode_SF_Data (SF_Literal^, LITERAL_TREE_ROOT)
      Else
        Literal := GetCode (8);
      UpdateDictionary (Literal);
    End { then }
    Else Begin
      { Data for output will come from the sliding dictionary           }
      If DictSize = 8192 Then Begin
        Distance := GetCode (7);
        DistVal  := Decode_SF_Data (SF_Distance^, DISTANCE_TREE_ROOT);
        Distance := (Distance Or (DistVal ShL 7) ) And $1FFF;
      End {then}
      Else Begin
        Distance := GetCode (6);
        DistVal  := Decode_SF_Data (SF_Distance^, DISTANCE_TREE_ROOT);
        Distance := (Distance Or (DistVal ShL 6) ) And $0FFF;
      End {if} ;
      Length   := Decode_SF_Data ( SF_Length^, LENGTH_TREE_ROOT );
      If Length = 63 Then
        Length := Length + GetCode (8);
      Length := Length + MinMatchLen;
      DictStart := DictIdx - (Distance + 1);
      If DictStart < 0 Then
        DictStart := DictStart + DictSize;
      While Length > 0 Do Begin
        UpdateDictionary (Dictionary^ [DictStart] );
        DictStart := Succ (DictStart) Mod DictSize;
        Dec (Length);
      End {while} ;
    End {if} ;
  Until (ExtCount >= LocalHdr. Uncompressed_Size);
End { Explode } ;

{ --------------------------------------------------------------------------- }

Procedure UnShrinkProc;
Begin
  If Not LZW_Init Then Begin
    Abort(StrNotEnoughMemoryForUnShrink);
    FSkip (LocalHdr. Compressed_Size);
    Crc32Val := Not LocalHdr. Crc32;
    Exit;
  End;
  Try
    UnShrink
  Finally
    LZW_Cleanup;
  End;
End;

Procedure UnReduceProc;
Begin
  If Not Init_UnReduce Then Begin
    Abort(StrNotEnoughMemoryForUnReduce);
    FSkip (LocalHdr. Compressed_Size);
    Crc32Val := Not LocalHdr. Crc32;
    Exit;
  End;
  Try
    UnReduce
  Finally
    Cleanup_UnReduce;
  End;
End;

Procedure UnExplodeProc;
Begin
  If Not Init_Explode Then Begin
    Abort(StrNotEnoughMemoryForUnExplode);
    FSkip (LocalHdr. Compressed_Size);
    Crc32Val := Not LocalHdr. Crc32;
    Exit;
  End;
  Try
    Explode
  Finally
    Cleanup_Explode;
  End;
End;

Procedure UnZipex;
{$IfDef useZLIB}
var
  zlib_buff, zlib_outputbuff: String;
  i: Integer; p: TZipProgress;
{$EndIf useZLIB}
Begin
  Crc32Val    := Integer($FFFFFFFF);
  Bytes_To_Go := LocalHdr. Compressed_Size;
  FirstCh     := True;
  ExtCount    := 0;
  TenPercent := LocalHdr. Compressed_Size;
  Case LocalHdr. Compress_Method Of
    0: Begin
      While Bytes_to_go > 0 Do
        Put_Ext (Get_Compressed);
    End {0 = Stored} ;
    1: Begin
      UnShrinkProc
    End {1 = shrunk} ;
    2..5  : Begin
      UnReduceProc
    End {2..5} ;
    6  : Begin
      UnExplodeProc
    End {6} ;
{$IfDef useZLIB}
    8  : Begin  {8 = un-Deflate}
        SetLength(zlib_buff,Bytes_To_Go);
        i := 1;
        p := FOnProgress;
        FOnProgress := NIL;
        While Bytes_to_go > 0 Do Begin
          zlib_buff[i] := Char(Get_Compressed);
          Inc(i);
        End;
        FOnProgress := p;
        ZLibDecompress(zlib_buff,LocalHdr.Uncompressed_Size,zlib_outputbuff);
        BlockWrite (ExtFile, zlib_outputbuff[1], LocalHdr.Uncompressed_Size);
        Crc32Val := $FFFFFFFF;
        For I := 1 To LocalHdr.Uncompressed_Size Do
          Crc32Val := UpdC32 (Byte(zlib_outputbuff[I]), Crc32Val);
    End;
{$EndIf useZLIB}
    Else
    Begin
      Abort(Format(StrUnknownCompressMethod,[LocalHdr.Compress_Method,Hdr_FileName]));
      FSkip (LocalHdr. Compressed_Size);
      Crc32Val := Not LocalHdr. Crc32;
    End {else} ;
  End {case} ;
  Crc32Val := Not Crc32Val;
  If Crc32Val <> LocalHdr. Crc32 Then Begin
    Abort(Format(StrFileHasBadCRC,[OutPath + Hdr_FileName,LocalHdr. Crc32,Crc32Val]));
  End {if} ;
End {UnZipex} ;

{ --------------------------------------------------------------------------- }

Procedure Extract_File(OverWrite:Boolean);
Var
  DosDTA : TSearchRec;
Begin
  If SysUtils.FindFirst (OutPath + Hdr_FileName, faAnyFile, DosDTA)= 0 Then Begin
    If Not OverWrite Then Begin
      FSkip (LocalHdr. Compressed_Size);
      Exit;
    End {if} ;
  End {if} ;
  If Open_Ext Then Begin
    UnZipex;
    Close_Ext;
  End {then}
  Else Begin
    FSkip (LocalHdr. Compressed_Size);
  End {If} ;
End {Extract_File} ;

{ --------------------------------------------------------------------------- }

Procedure Extract_Zip;
Var
  I     : Word;
Begin
  Open_Zip;
  While Read_Local_Hdr Do
  Begin
    For I := 0 To InFileSpecif.Count-1 Do
    Begin
      If SameFile (ExpandFileName(InFileSpecif [I]), ExpandFileName(Hdr_FileName)) Then
        Extract_File(True) {tj. prepis vsechny}
      Else
        FSkip (LocalHdr. Compressed_Size);
    End;
  End {while} ;
  Close_Zip;
End;

{ --------------------------------------------------------------------------- }

Begin
  InFileSpecif := TStringList.Create;
  Try
    If NOT SysUtils.FileExists(ZipFileName) Then Exit;
    ZipName := ZipFileName;

    If NOT DirectoryExists(DirToExtract) Then
      {$IfDef VER5UP}
      If NOT ForceDirectories(DirToExtract) Then Exit;
      {$Else}
      ForceDirectories(DirToExtract);
      {$EndIf}
    OutPath := DirToExtract;
    If OutPath [Length (OutPath) ] <> '\' Then OutPath := OutPath + '\';
    If FilesToExtract.Count = 0 Then
    Begin
      InFileSpecif.Clear;
      InFileSpecif.Add('*.*');
    End {if}
    Else
    Begin
      InFileSpecif.AsSign(FilesToExtract);
    End;

    If Not Initialize Then Exit;   { one-time initialization }
    Try
      Extract_Zip;  { de-arc the file }
    Finally
      Finalize
    End
  Finally
    InFileSpecif.Free;
  End;
End;

constructor TZip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IfDef useZLIB}
  FForceDeflate := True; {for future use, please don't change it}
{$EndIf useZLIB}
  FParams := TStringList.Create;
end;

destructor TZip.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

Procedure TZip.SetFName(Name:String);
Begin
  FName := Name;
End;

Function TZip.Execute:Boolean;
Begin
  Result := True;
  Try
    Crunch(FName,FParams,FWorkDir);
  Except
    Result := False;
  End;
End;

{-----------------------------------------------------------------}

constructor TUnZip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TStringList.Create;
end;

destructor TUnZip.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

Procedure TUnZip.SetFName(Name:String);
Begin
  FName := Name;
End;

Procedure TUnZip.SetExtrPath(Path:String);
Begin
  FExtrPath:=Path;
  If FExtrPath<>'' Then
    If FExtrPath[Length(FExtrPath)]<>'\' Then
      FExtrPath := FExtrPath +'\';
End;

Function TUnZip.Execute:Boolean;
Begin
  Result := True;
  Try
    UnCrunch(FName,FExtrPath,FParams);
  Except
    Result := False
  End;
End;

Procedure TUnZip.GetZipList(Const iFileName:String;Var A:TStringList);
{StringList return list of files in zip file}
{call it like Zip.GetZipList(Zip.FName,FList);}
{ve stringlistu je navracen seznam souboru ze zipu}
{volani Zip.GetZipList(Zip.FName,FList);}
Const
  lf_signature = $4034b50;
  f_signature  = $2014b50;
  e_signature  = $6054b50;
Type

  L_f_head = Packed Record                 {  A.  Local file header:}
    Signature : LongInt;  { local file header signature     4 bytes }
    { (0x04034b50) }
    Unp_ver,              { version needed to extract       2 bytes }
    Gen_pur,              { general purpose bit flag        2 bytes }
    Compr_met,            { compression method              2 bytes }
    File_time,            { last mod file time              2 bytes }
    File_date : Word;     { last mod file date              2 bytes }
    CRC,                  { crc-32                          4 bytes }
    Comp_size,            { compressed size                 4 bytes }
    full_size : LongInt;  { uncompressed size               4 bytes }
    Name_len,             { filename length                 2 bytes }
    Eks_len   : Word;     { extra field length              2 bytes }

    { filename (variable size)                }
    { extra field (variable size)             }
  End;


  {  B.  Central directory structure: }

  { [file header] . . .  end of central dir record }


  F_header = Packed Record                 { File header: }
    Signature : LongInt;  { central file header signature   4 bytes }
    { (0x02014b50) }
    Com_ver,              { version made by                 2 bytes }
    Unp_ver,              { version needed to extract       2 bytes }
    Gen_pur,              { general purpose bit flag        2 bytes }
    compr_met,            { compression method              2 bytes }
    File_time,            { last mod file time              2 bytes }
    File_date : Word;     { last mod file date              2 bytes }
    CRC,                  { crc-32                          4 bytes }
    Comp_size,            { compressed size                 4 bytes }
    full_size : LongInt;  { uncompressed size               4 bytes }
    Name_len,             { filename length                 2 bytes }
    Eks_len,              { extra field length              2 bytes }
    Com_len,              { file comment length             2 bytes }
    Disk_start,           { disk number start               2 bytes }
    Int_att   : Word;     { internal file attributes        2 bytes }
    ext_att,              { external file attributes        4 bytes }
    L_h_ofs   : LongInt;  { relative offset of local header 4 bytes }

    { filename (variable size)                }
    { extra field (variable size)             }
    { file comment (variable size)            }
  End;


  End_cd = Packed Record                   { End of central dir record: }
    Signature   : LongInt;  { end of central dir signature 4 bytes    }
                            { (0x06054b50) }
    Disk_nr,                { number of this disk             2 bytes }
                            { number of the disk with the             }
    Start_nr,               { start of the central directory  2 bytes }
                            { total number of entries in              }
    CD_entrys,              { the central dir on this disk    2 bytes }
                            { total number of entries in              }
    Tot_cd_ent  : Word;     { the central dir                 2 bytes }
    CD_size,                { size of the central directory   4 bytes }
                            { offset of start of central              }
                            { directory with respect to               }
    CD_start_ofs: LongInt;  { the starting disk number        4 bytes }
    Com_leng    : Word;     { zipfile comment length          2 bytes }
                            { zipfile comment (variable size)         }
  End;
  Streng    = String {$IFNDEF UNICODE}[255]{$ENDIF}; //special type only
  P_l_head  = Packed Record
    zip_head : L_f_head;
    Name,
    comment  : streng;
  End;

Const
  buffsize = 2*4095;{doubled 14.2.1996}
Var
  fil	: File;
  Buffer  : ^Word;
  laest	: Word;
  hoved	: p_l_head;


Function read_headder ( Var head : P_l_head) : Boolean;
Var
  ok: Boolean;
  l, len: Integer;
Begin
  ok:=True;
  With head Do Begin
    If laest<SizeOf(zip_head) Then Begin
      l:=SizeOf(zip_head);
      BlockRead(fil,Pointer(LongInt(Buffer)+laest)^,l,len);
      ok:= (len=(SizeOf(zip_head)-laest));
      Inc(laest,len);
    End;
    Move(Buffer^,zip_head,SizeOf(zip_head));
    ok:=ok And (zip_head.signature=lf_signature);
    If ok Then Begin
      {set length of name}
      SetLength(Name,zip_head.name_len);
      {read it into string}
      BlockRead(fil,Name[1],zip_head.name_len,len);
      {set length of comment}
      SetLength(comment,zip_head.eks_len);
      {read it into string}
      BlockRead(fil,comment[1],zip_head.eks_len,len);
      {ok when is extra file fine readed and signeture is well}
      ok:= (len=zip_head.eks_len) And (zip_head.signature=lf_signature);
    End;
  End;
  read_headder:=ok;
End;

Function skip ( leng : LongInt) : Boolean;
Var
  len 	: Integer;
  ok	: Boolean;
Begin
  ok:=True;
  Repeat
    If (leng>buffsize) Then Begin
      BlockRead(fil,Buffer^,buffsize,len);
      Dec(leng,buffsize);
      If len<buffsize Then ok:=False;
    End
    Else Begin
      len:=leng;
      BlockRead(fil,Buffer^,len,len);
      If len<leng Then ok:=False;
    End;
  Until Not ok Or (leng<buffsize);
  skip:=ok;
  laest:=0;
End;

Function timestring ( tiden : LongInt) : streng;
Var DT:TDateTime;
Begin
  DT := FileDateToDateTime(tiden);
  Result := FormatDateTime('dd'+{$IFDEF VER15UP}FormatSettings.{$ENDIF}DateSeparator
   +'mm'+{$IFDEF VER15UP}FormatSettings.{$ENDIF}DateSeparator
   +'yyyy"'+#9+'"hh'+{$IFDEF VER15UP}FormatSettings.{$ENDIF}TimeSeparator+'mm'
   +{$IFDEF VER15UP}FormatSettings.{$ENDIF}TimeSeparator+'ss',DT)
End;

Const maxpollin=100;{maxpollin}
Var
  ok: Boolean;
  t_len,t_size: LongInt;
  antal: Word;
  navn: streng;
  stamp,latest: LongInt;
  S: String;
  Procedure Init;
  Begin
    laest:=0; antal:=0;
    t_size:=0; t_len:=0;
    latest:=0;stamp:=0;
    ok:=True;
  End;
Var
  R:Real;
Begin { Zipview }
  GetMem(Buffer,buffsize);
  try
    navn:=iFileName;
    Init;
    Assignfile(fil,navn);
    Reset(fil,1);
    try
      //pocet := 0;
      While ok And read_headder(hoved) Do With hoved,zip_head Do
      Begin
        //Inc(pocet);
        ok:= skip(zip_head.comp_size);
        Inc(t_len,full_size);
        Inc(t_size,comp_size);
        If stamp>latest Then latest:=stamp;
      End;
      Init;
      Reset(fil,1);
      While ok And read_headder(hoved) Do With hoved,zip_head Do Begin
        Inc(antal);

        S := Name;
        Try
          R := -1*(100-(comp_size/full_size*100));
        Except
          R := 0;
        End;
        stamp:=LongInt(zip_head.file_date) ShL 16 Or zip_head.file_time;
        S := S+#9+Long2Str(full_size)+#9+Long2Str(comp_size)+#9+
          Trim(Real2Str(R,5,0))+'%'+#9+timestring(stamp);

        A.Add(S);{<----- vlozi retezec do seznamu}

        ok:= skip(zip_head.comp_size);
        Inc(t_len,full_size);
        Inc(t_size,comp_size);
        If stamp>latest Then latest:=stamp;
      End;
    finally
      Close(fil);
    end;
  finally
    FreeMem(Buffer,buffsize);
  end;
End;

//------------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('Library', [TZip,TUnZip]);
end;

end.