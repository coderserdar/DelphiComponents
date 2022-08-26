unit hhTTFinfo;

{ Unit to retrieve font details from a TTF file }
{ Author: H.J.Harvey }
{ Dated:  5/June/2005 }
{ Email:  hharvey@picknowl.com.au }
{ Version: 2.0 }

{----
Using this VCL:
  (1) Place at least 1 copy of hhTTFinfo on a form
  (2) Define a pointer to the TTF data (from hhTTFinfo)
          TTFptr : hhTTFinfo.TTFListPtr = (nil) ;
  (3) Call GetTTF with a defined pointer to the structure
      eg. New(TTFptr) ;
          hhTTFinfo1.GetTTFinfo( TTFilename, TTFptr, errcode) ;
          if errcode <> 0 then Dispose( TTFptr )
          else .... // process the details

The data may then be accessed directly from the pointer, or
can be stored in a tList array containing the details.
---- }

// {$DEFINE OS2}    { OS2 capability may be added later }

interface

uses
  Windows, Classes, SysUtils;

const
  Version_Number = '2.00.0' ;

type
  tTTFList = record
    Copyright  : string ;
    Family     : string ;
    SubFamily  : string ;
    Identifier : string ;
    FontName   : string ;
    Version    : string ;
    PSname     : string ;
    Trademark  : string ;
    Company    : string ;
    Designer   : string ;
    PlatformID : word ;
    EncodingID : word ;
    LanguageID : word ;
    FilePath   : string ;
    FileCalled : string ;
    FileSize   : longint ;
    FileDated  : tdatetime ;
    FileAttribs: Word ;
    Track      : integer ;      // Debug variable only
  end ;

  TTFListPtr = ^tTTFList ;

  ThhTTFinfo = class(TComponent)
  private
    { Private declarations }
    fVersion : string ;
    procedure Dummy(Value: String);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetTTFinfo( fname : string ; var TTFPtr : TTFListPtr ;
                          var ErrorNum : word);
  published
    { Published declarations }
    property Version : string  read fVersion write Dummy stored False;
  end;

procedure Register;

implementation

type
  UKind = (motorola,intel);

  USHORT = record               { 2-byte values unsigned }
  case Ukind of
    motorola : (b1,b2     : byte) ;
    intel    : (shortword : word) ;
  end;

  SHORT = record                { 2-byte values signed }
  case Ukind of
    motorola : (b1,b2     : byte) ;
    intel    : (shortword : integer) ;
  end;

  ULONG  = record               { 4-byte values unsigned }
  case Ukind of
    motorola : (b1,b2,b3,b4 : byte) ;
    intel    : (longword    : longint) ;
  end;

  PANOSE = array [1..10] of byte ;

  THeaderRec = record           { Header Record - 16 bytes }
    ident : array[1..4] of char ;
    csum  : ULONG ;
    pntr  : ULONG ;
    rsize : ULONG ;
  end;

  TNameHeader = record          { Name Header - 6 bytes }
    FormatSel  : USHORT ;
    NumRecords : USHORT ;
    StrOffset  : USHORT ;
  end;

  TNameRecord = record          { Name Record - 12 bytes }
    PlatformID : USHORT ;
    EncodingID : USHORT ;
    LanguageID : USHORT ;
    NameID     : USHORT ;
    Slength    : USHORT ;
    Soffset    : USHORT ;
  end;

{$IFDEF OS2}
  TOS2Record = record
    version             : USHORT ;
    xAvgCharWidth       : SHORT ;
    usWeightClass       : USHORT ;
    usWidthClass        : USHORT ;
    fsType              : SHORT ;
    ySubscriptXSize     : SHORT ;
    ySubscriptYSize     : SHORT ;
    ySubscriptXOffset   : SHORT ;
    ySubscriptYOffset   : SHORT ;
    ySuperscriptXSize   : SHORT ;
    ySuperscriptYSize   : SHORT ;
    ySuperscriptXOffset : SHORT ;
    ySuperscriptYOffset : SHORT ;
    yStrikeoutSize      : SHORT ;
    yStrikeoutPosition  : SHORT ;
    sFamilyClass        : SHORT ;
    panose              : PANOSE ;
    ulCharRange         : array [1..4] of ULONG;
    achVendID           : array [1..4] OF CHAR ;
    fsSelection         : USHORT ;
    usFirstCharIndex    : USHORT ;
    usLastCharIndex     : USHORT ;
    sTypoAscender       : USHORT ;
    sTypoDescender      : USHORT ;
    sTypoLineGap        : USHORT ;
    usWinAscent         : USHORT ;
    usWinDescent        : USHORT ;
  end ;
{$ENDIF}

var
  HeaderRec  : THeaderRec ;
  NameHeader : TNameHeader ;
  NameRecord : TNameRecord ;
{$IFDEF OS2}
  OS2Record  : TOS2Record ;
{$ENDIF}

{ -------------------------------------------------------------------- }

constructor ThhTTFinfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fVersion := Version_Number ;
end;

{ -------------------------------------------------------------------- }

destructor ThhTTFinfo.Destroy;
begin
  inherited Destroy;
end;

{ -------------------------------------------------------------------- }

procedure ThhTTFinfo.Dummy(Value: String);
begin
//  Read only !
end;

{ -------------------------------------------------------------------- }

procedure ThhTTFinfo.GetTTFinfo( fname : string ; var TTFPtr : TTFListPtr ;
                                 var ErrorNum : word) ;

{ Given:
    name of the file in "fname"
    pointer to a suitable store in "TTFPtr"
  Returns:
    if ErrorNum = 0 then details are in TTFptr^
    if ErrorNum <> 0 then FPtr^ is incomplete }

{ -------------------------------------------------------------------- }

procedure ConvertUSHORT( var value : USHORT ) ;

{ Two byte swap around }
{ Converts BigEndian (Motorola) to LittleEndian (Intel) }

var temp : byte ;

begin
  temp := value.b2 ;
  value.b2 := value.b1 ;
  value.b1 := temp ;
end;

{ -------------------------------------------------------------------- }

procedure ConvertULONG( var value : ULONG ) ;

{ Four byte swap around }
{ Converts BigEndian (Motorola) to LittleEndian (Intel) }

var temp : byte ;

begin

{ Outer bytes }

  temp := value.b4 ;
  value.b4 := value.b1 ;
  value.b1 := temp ;

{ Inner bytes }

  temp := value.b3 ;
  value.b3 := value.b2 ;
  value.b2 := temp ;
end;

{ -------------------------------------------------------------------- }

procedure FindNameRecord( var nrec : THeaderRec ; var TT_File : file ) ;

var index : longint ;

begin
  index := 12 ;

{ Look for the 'name' string field }

  repeat
    SEEK( TT_File , index ) ;
    BlockRead( TT_File , nrec , 16 );
    index := index + 16;
  until (nrec.ident[1] = 'n')
    and (nrec.ident[2] = 'a')
    and (nrec.ident[3] = 'm')
    and (nrec.ident[4] = 'e');

{ Convert the data in the fields to Intel format }

  ConvertULONG( nrec.csum );
  ConvertULONG( nrec.pntr );
  ConvertULONG( nrec.rsize );
end;

{ -------------------------------------------------------------------- }

{$IFDEF OS2}
procedure FindOS2Record( var nrec : THeaderRec ; var TT_File : file ) ;

var index : longint ;

begin
  index := 12 ;

{ Look for the 'name' string field }

  repeat
    SEEK( TT_File , index ) ;
    BlockRead( TT_File , nrec , 16 );
    index := index + 16;
  until (nrec.ident[1] = 'O')
    and (nrec.ident[2] = 'S')
    and (nrec.ident[3] = '/')
    and (nrec.ident[4] = '2');

{ Convert the data in the fields to Intel format }

  ConvertULONG( nrec.csum );
  ConvertULONG( nrec.pntr );
  ConvertULONG( nrec.rsize );
end;
{$ENDIF}

{ -------------------------------------------------------------------- }

function ValidLanguage( Code : word ) : boolean ;

{ Various English or French-Canadian only accepted }

begin
  Result := ((Code AND $00FF) = $0009) OR (Code = $0C0C) ;
end ;

var
  NameStart   : longint ;
  index       : longint ;
  uindex      : integer ;
  records     : integer ;
  TT_File     : file ;
  NameString  : string[255] ;
  StringStart : longint ;
  FileCalled  : string ;
  UCchar      : array[0..509] of char ;
  NameLength  : integer ;

{ -------------------------------------------------------------------- }

procedure PreclearInfo ;

{ Initialise returned results to defaults }

begin
  TTFPtr^.Copyright  := '' ;
  TTFPtr^.Family     := '' ;
  TTFPtr^.SubFamily  := '' ;
  TTFPtr^.Identifier := '' ;
  TTFPtr^.FontName   := '' ;
  TTFPtr^.Version    := '' ;
  TTFPtr^.PSname     := '' ;
  TTFPtr^.Trademark  := '' ;
  TTFPtr^.Company    := '' ;
  TTFPtr^.Designer   := '' ;
  TTFPtr^.PlatformID := 0 ;
  TTFPtr^.EncodingID := 0 ;
  TTFPtr^.LanguageID := 0 ;
  TTFPtr^.FilePath   := fname ;
  TTFPtr^.FileCalled := ExtractFileName( fname ) ;
  TTFPtr^.FileSize   := FileSize( TT_File ) ;
  TTFPtr^.FileDated  := FileDateToDateTime(FileAge(fname)) ;
  TTFPtr^.FileAttribs:= FileGetAttr(fname) ;
end ;

{ -------------------------------------------------------------------- }

procedure InsertEntry( var Destination : string ; value : string ) ;

{ Only take first non-empty string }

begin
  if (Destination = '') AND (value <> '')
  then Destination := value ;
end ;

{ ----------------------- ThhTTFinfo.GetTTFinfo ---------------------- }

begin
  ErrorNum := $FFFF ;
  TTFPtr^.Track := 0 ;
  if TTFptr = nil then exit ;
  FileCalled := ExtractFileName(fname) ;
  TTFPtr^.Track := 1 ;

{ Open the file using ReadOnly Mode and extract font information }

  FileMode := 0 ;
  ASSIGNFILE( TT_File , fname ) ;
  {$I-} RESET( TT_File , 1 ) {$I+};
  ErrorNum := IORESULT ;

  if ErrorNum = 0
  then begin
    TTFPtr^.Track := 2 ;
    PreclearInfo ;

{ Find the "name" header }

    FindNameRecord ( HeaderRec , TT_File ) ;
    NameStart := HeaderRec.pntr.longword ;

{ Go to start of data and read 6-byte "NameHeader" record }

    SEEK( TT_File , NameStart ) ;
    {$I-} BlockRead( TT_File , NameHeader , 6 ); {$I+}
    ErrorNum := IORESULT ;
    if ErrorNum = 0
    then begin
      TTFPtr^.Track := 3 ;

{ Number of records and start offset }

      ConvertUSHORT( NameHeader.NumRecords ) ;
      ConvertUSHORT( NameHeader.StrOffset ) ;

{ String starts at offset from name start }

      StringStart := NameStart + NameHeader.StrOffset.shortword;

{ Bump index pointer over the record }

      index := NameStart + 6;
      for records := 1 to NameHeader.NumRecords.shortword do
      begin
//          TTFPtr^.Track := 1000+records ;

{ Read a 12 byte "NameRecord" record }

        SEEK( TT_File , index ) ;
        {$I-} BlockRead( TT_File , NameRecord , 12 ) {$I+} ;
        ErrorNum := IORESULT ;
        if ErrorNum = 0
        then begin

{ Point past the record }

          index := index + 12;

{ Convert parameters }

          ConvertUSHORT( NameRecord.PlatformID ) ;
          ConvertUSHORT( NameRecord.EncodingID ) ;
          ConvertUSHORT( NameRecord.LanguageID ) ;
          ConvertUSHORT( NameRecord.NameID ) ;
          ConvertUSHORT( NameRecord.Slength ) ;
          ConvertUSHORT( NameRecord.Soffset ) ;

{ Filter valid entries }

          if
             ((NameRecord.PlatformID.shortword = 3)
          and (NameRecord.EncodingID.shortword <= 1)
          and ValidLanguage(NameRecord.LanguageID.shortword))
          or
             ((NameRecord.PlatformID.shortword = 1)
          and (NameRecord.EncodingID.shortword = 0)
          and (NameRecord.LanguageID.shortword = 0))
          then begin
            TTFPtr^.Track := NameRecord.PlatformID.shortword*100+records ;
            SEEK( TT_File , StringStart+NameRecord.Soffset.shortword );

{ Character strings are limited to 255 characters in this version. }
{ Some fields may exceed this length, so will be truncated to 255. }

            NameString := '' ;
            NameLength := NameRecord.Slength.shortword ;

{ Read up to 510 bytes }

            if NameLength > 0
            then begin
              if (NameLength > 510) then NameLength := 510 ;
              BlockRead( TT_File , UCchar[0] , NameLength ) ;
            end ;

{ Process, removing any non ASCII visible bytes }
{ This removes most UNICODE page bytes }

            for uindex := 0 to NameLength-1 do
            begin
              if ODD(uindex) OR (UCchar[uindex] IN [' '..'~'])
              then begin
                if (length(NameString) < 255)
                then NameString := NameString + UCchar[uindex]
                else
                if uindex+3 < NameLength
                then NameString := COPY(NameString,1,252)+'...' ;
              end ;
            end ;       // for uindex := 0 to NameLength-1 do

{ If the string is valid then store the string in the identified field }

            case NameRecord.NameID.shortword of
              0: InsertEntry(TTFPtr^.Copyright, NameString) ; { CopyRight msg }
              1: InsertEntry(TTFPtr^.Family,    NameString) ; { Family Name }
              2: InsertEntry(TTFPtr^.SubFamily, NameString) ; { Sub Family Name }
              3: InsertEntry(TTFPtr^.Identifier,NameString) ; { Identifier }
              4:
                begin
                  InsertEntry(TTFPtr^.FontName,  NameString) ; { FontName }

{ For the FontName we also store the Platform, Encoding and Language info }

                  TTFPtr^.PlatformID := NameRecord.PlatformID.shortword ;
                  TTFPtr^.EncodingID := NameRecord.EncodingID.shortword ;
                  TTFPtr^.LanguageID := NameRecord.LanguageID.shortword ;
                end ;
              5: InsertEntry(TTFPtr^.Version,   NameString) ; { Version }
              6: InsertEntry(TTFPtr^.PSname,    NameString) ; { Postscript Name }
              7: InsertEntry(TTFPtr^.Trademark, NameString) ; { Trademark }
              8: InsertEntry(TTFPtr^.Company,   NameString) ; { Manufacturer }
              9: InsertEntry(TTFPtr^.Designer,  NameString) ; { Designer }
              else ;    { other NameIDs (10+) not used }
            end;      // case NameRecord.NameID.shortword of
          end ;         // if (  (NameRecord.PlatformID.shortword = 3)...
        end             // if ErrorNum = 0
        else break ;
      end;              // for records := 1 to NameHeader.NumRecords...
    end ;               // if errornum = 0
    CLOSEFILE( TT_File ) ;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure Register;
begin
  RegisterComponents('Howie', [ThhTTFinfo]);
end;

end.

