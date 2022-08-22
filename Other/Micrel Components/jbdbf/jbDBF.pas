{*******************************************************}
{                                                       }
{       jbDBF advanced edition                          }
{                                                       }
{       Copyright (C) 2009-2013 Jaro Benes              }
{                                                       }
{       Compatible with unicode Delphi                  }
{                                                       }
{*******************************************************}

{$IFDEF VER80}
{$X+,H-}
{$ELSE}
{$X+}
{$ENDIF}
unit jbDbf;
{$I jb.inc}

interface
{$IFDEF LINUX} { /AR/ }
uses SysUtils, Classes, QDialogs;
{$ELSE}
uses
  Windows,
  {$IFDEF UNICODE}AnsiStrings,{$ENDIF}
  SysUtils, DateUtils,
  {$IFDEF VER19UP}System.UITypes,{$ENDIF}
  Classes, Dialogs, TypInfo;
{$ENDIF}

const
  FileVersion = '2.01';

{works with DBF table by direct file}
{for version Delphi 1..7, 2005, 2006/Turbo, 2007}
{
History
1/ First release based on jbDBF older version 1.00
}
{-------------------------------------------------------------------------------
Original file founded 1999 (c) Jaro Benes
Older history:
  10.11.2011 Compatibility with Unicode Delphi
  13.11.2006 Fix bug on PackDBF;
  06.09.2004 added DBF Packing function PackDBF and deleted the empty procedure PruneDBF;
             renamed PruneDBT to PackDBT; on TjbDBF.Close Actualization only if file is not readonly
             by Andrea Russo /AR/ [mailto:andrusso@yahoo.com]
  18.12.2003 changes and Kylix compatibility by Andrea Russo /AR/ [mailto:andrusso@yahoo.com]
  20.08.2002 changes and Italian messages by Andrea Russo /AR/ [mailto:andrusso@yahoo.com]
  18.04.2001 bugfix changes by Jarda Jirava [<mailto:listuj@centrum.cz>]
  30.10.2001 bugfix and extended by Vyacheslav Nebroev /VN/ [<mailto:vasu@amts.smolensk.ru>]
  15.01.2002 all comments convert from Czech to English
-------------------------------------------------------------------------------}


{$IFNDEF VER5UP}
const
{$ELSE}
resourcestring { Changed by me /VN/ }
{$ENDIF}

{$IFDEF MessagesItalianLang}
  msgCreateDBFError = 'Impossibile creare un nuovo file DBF';
  msgErrorOnOpen = 'Impossibile aprire il file DBF';
  msgEOFmarkMiss = 'Fine del file non trovata';
  msgNoPasswordMatch = 'Password non valida';
  msgNotEnoughtCreateIdx = 'Impossibile creare l''indice IDX';
  msgIdxTableNotFound = 'Impossibile aprire l''indice IDX "%s"';
  msgErrorOnIdxTable = 'Errore durante la lettura dell''indice IDX "%s"';
  msgIdxFieldNotFound = 'Campo indice "%s" non trovato';
  msgFieldNotFound = 'Il campo numero %d non è presente';
  msgFileIsTooLarge = 'File troppo grande';
  msgErrorOnWrite = 'Errore durante la scrittura della tabella';
  msgFileTooRemote = 'La data del file non è valida';
  msgCannotOpenTable = 'Impossibile aprire la tabella';
  msgCannotDeleteItem = 'Impossibile cancellare il campo';
  msgCannotAddItem = 'Impossibile aggiungere un nuovo campo';
  msgBadDefinitionOfField = 'La definizione del campo è errata';
  msgDuplicateInUnique = 'Valore duplicato in un indice univoco';
  msgErrorOnMemoOpen = 'Errore durante l''apertura del memo';
  prgMakeIndexSort = 'Ordinamento dell''indice';
  prgWriteIndexSort = 'Scrittura dell''indice';
  prgSearchByKey = 'Ricerca per chiave';

{$ELSE}

{$IFDEF MessagesCzechLang}
  {hlasky lze doplnit diakritikou}
  {messages (and all comments) in Czech haven't got diacritics}
  {but is possible insert it into messages}
  msgCreateDBFError = 'Nemohu vytvorit novou DBF tabulku';
  msgErrorOnOpen = 'Nemohu otevrit DBF tabulku';
  msgEOFmarkMiss = 'Chybi oznaceni konce souboru';
  msgNoPasswordMatch = 'Nesouhlasi overovaci heslo administratora';
  msgNotEnoughtCreateIdx = 'Nemohu vytvorit IDX soubor';
  msgIdxTableNotFound = 'Nemohu otevrit IDX soubor "%s"';
  msgErrorOnIdxTable = 'Nesouhlasi mohutnost IDX souboru "%s"';
  msgIdxFieldNotFound = 'Nebyla nalezena polozka "%s"';
  msgFileIsTooLarge = 'Soubor ma priliz mnoho polozek nez muze tato verze zpracovat';
  msgErrorOnWrite = 'Chyba pri pokusu o zapis do tabulky';
  msgFileTooRemote = 'Soubor je priliz vzdaleny az nedostupny';
  msgCannotOpenTable = 'Tabulka se nenechala otevrit';
  msgCannotDeleteItem = 'Zaznam z tabulky nejde odstranit';
  msgCannotAddItem = 'Novy zaznam nejde do tabulky pridat';
  msgBadDefinitionOfField = 'Chybna definice zaznamu pole tabulky';
  msgDuplicateInUnique = 'Pokus pridat do unikatniho klice duplicitu';
  msgErrorOnMemoOpen = 'Chyba pri otevirani memo souboru';
  prgMakeIndexSort = 'Vytvarim indexovou strukturu';
  prgWriteIndexSort = 'Zapisuji index na disk';
  prgSearchByKey = 'Prohledavam tabulku dle indexu';
{$ELSE}
  msgCreateDBFError = 'Cannot create new DBF file';
  msgErrorOnOpen = 'Cannot open DBF file';
  msgEOFmarkMiss = 'Missing EOF mark';
  msgNoPasswordMatch = 'No password match';
  msgNotEnoughtCreateIdx = 'Cannot create IDX';
  msgIdxTableNotFound = 'Cannot open IDX "%s"';
  msgErrorOnIdxTable = 'Error of reading IDX "%s"';
  msgIdxFieldNotFound = 'Index field "%s" not found';
  msgFieldNotFound = 'Field with number %d not present';
  msgFileIsTooLarge = 'File is too large';
  msgErrorOnWrite = 'Error writing table';
  msgFileTooRemote = 'File too remote';
  msgCannotOpenTable = 'Cannot open table';
  msgCannotDeleteItem = 'Cannot delete item';
  msgCannotAddItem = 'Cannot add new item';
  msgBadDefinitionOfField = 'Bad field definition';
  msgDuplicateInUnique = 'Duplicate in Unique';
  msgErrorOnMemoOpen = 'Error of memo opening';
  prgMakeIndexSort = 'Make index sort';
  prgWriteIndexSort = 'Write indes sort';
  prgSearchByKey = 'Search by key';
{$ENDIF}
{$ENDIF}
{----------------------------------------------------------------------------}
const
  MaxSize = $7FFF; { maximum for buffer }
  {$IFDEF VER80}
  MaxItems = 16384; { this is for 16bit Delphi, is possible to change}
  {$ELSE}
  MaxItems = MaxInt;
  {$ENDIF}
  MaxFields = 128; { maximum count for columns }
  DeleteFlag = '*';
  EOFFlag = #$1A;
  SpacerD = ' ';
type
  {  TCharrs11  }

  TCharrs11 = array[0..10] of AnsiChar; {filename defs.}

  {zapis se podaril, db je nedostupna-v transakci, storno operace, chyba pri zapisu}
  {write unsuccessful, db is inaccessible in transaction, cancel operation, error on write}
  TStatusWrite = (dbfOK, dbfBusy, dbfCancel, dbfError);
  {zaznam je ukladan jako novy, nebo je obcerstvovan}
  {record is stored as new or is refreshed}
  TPostAct = (dbfNew, dbfUpdate);
  {pole ma unikatni klic, duplicitni klic,autoinkcementalni klic}
  {field has unique key, duplicate key or autoincrement key}
  TFieldReq = (dbfUnique, dbfDuplicates);
  {pole razeno vzestupne, sestupne, podle alternativniho klice}
  {field sorted in ascending order, descending order or by alternative key}
  TSortByIndex = (dbfAscending, dbfDescending, dbfAlternative);
  {declaration events procedures}
  TDBFError = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TDBFMakeItems = procedure(Sender: TObject; var Posit: Integer;
    var INname: TCharrs11; var IWhat: AnsiChar; var ILen, IPlaces: Byte;
    var IDXName: TCharrs11; var Req: TFieldReq; var Sort: TSortByIndex) of object;
  TDBFProgress = procedure(Sender: TObject; const Operace: string; Progress: Integer) of object;
  TDBFPassword = procedure(Sender: TObject; var Pass: string) of object;
  TDBFAssigned = procedure(Sender: TObject; var FName: string) of object;
  TDBFBeforeConfirm = procedure(Sender: TObject; const FName: string; var Confirm: Boolean) of object;
  TDBFConfirm = procedure(Sender: TObject; var Confirm: Boolean) of object;
  TDBFNavigate = procedure(Sender: TObject; Position: LongInt) of object;
  TDBFOpened = procedure(Sender: TObject; IsOpened: Boolean) of object;
  TDBFChange = procedure(Sender: TObject; var Cancel: Boolean) of object;
  TDBFActualize = procedure(Sender: TObject; Status: TPostAct) of object;
  TDBFQuery = procedure(Sender: TObject; const IdxName, IdxField, Key: string;
    var Accept, Cancel: Boolean) of object;
  TDBFAltSort = procedure(Sender: TObject; TblIdx: TStringList) of object;

  TDBFHeader = packed record { legends are from manual, changed for me specific}
    version: Byte;            { Should be 3 or $83                           1 }
                              { $3  - FoxBase+/dBase III Plus bez souboru MEMO }
                              {     - FoxPro/dBase IV bez souboru memo
                              { $83 - FoxBase+/dBase III Plus se souborem MEMO }
                              { $F5 - FoxPRo se souborem memo                  }
                              { $8B - dBase IV se souborem memo                }
    year, month, day: Byte;   { Date of last update                          3 }
    numRecs: LongInt;         { Number of records in the file                4 }
    headLen: Word;            { Length of the header                         2 }
    recLen: Word;             { Length of one data record, incl.delete flag  2 }
    case Byte of
    $0:
    ( {modifications for me}
    nets: Word;               { not used                                       }
    transaction: Byte;        { begin-end transaction                          }
                              { 00 - no transaction protected                  }
                              { 01 - transaction protected                     }
    encrypted: Byte;          { coded fields                                   }
                              { 00 - uncrypted                                 }
                              { 01 - encrypted                                 }
    network: array[1..12] of Byte;
    mdxfile: Byte;            { exist .mdx file indicator                      }
                              { 00 - non exist                                 }
                              { 01 - exist and join                            }
                              { NOTE: other interpretation is here             }
                              {Table flags:
                                $01   file has a structural .cdx
                                $02   file has a Memo field
                                $04   file is a database (.dbc)
                                This Byte can contain the sum of any of the
                                above values. For example, the value $03
                                indicates the table has a structural .cdx
                                and a Memo field.}
    LangDrv: Byte;            { language driver /fox/                          }
                              { 001 - code page 437                            }
                              { 002 - code page 850                            }
                              { 100 - code page 852                            }
                              { 102 - code page 865                            }
                              { 101 - code page 866                            }
                              { 104 - code page 895                            }
                              { 200 - code page 1250                           }
                              { 201 - code page 1251                           }
                              { 003 - code page 1252                           }
    labeled: word;            { not used, contain binary zero only             }
    );
    $3:
    ( {standard structure part}
    unused: array [1..17] of Byte; { unused, filled by binary zero             }
    cdxfile: Byte;                 { Type associated file:                     }
                                   {  Bit 1: is there a structural CDX?        }
                                   {  Bit 2: is there an associated memo file? }
                                   {  Bit 3: is this file used as a DBC?       }
    langDrv2: Byte;                { Code page signature, the same as LangDrv  }
    labeled2: Byte                 { unused, binary zero filled                }
    )
  end;

const
  ccTrue = 'T';
  ccFalse = 'F';

  TdbTypes: set of AnsiChar = [
    'C' {characters, all ascii},
    'D' {date ddmmyyyy, fix size 8}, //default is yyyymmdd
    'T' {time hhmmss, fix size 6}, //it is possible as DBF format extension
    'F' {float point, -,.,0..9, right padded, left spaced},
    'L' {logical, fix size 1 Byte, initial with space ,T,t,F,f},
    'M' {memo, as numeric, fix size 10, right padded number as point to .DBT},
    'N' {numeric, -,.,0..9}
    {$IFDEF VERDB5UP} ,
    'B' {binary, as numeric, fix size 10, right padded number as point to .DBT},
    '@' {timestamp - 8 bytes - two longs, first for date, second for time.  The date is the number of days since  01/01/4713 BC. Time is hours * 3600000L + minutes * 60000L + Seconds * 1000L},
    'I' {long, fixed 4 bytes as longword with sign},
    '+' {autoincrement like as long},
    'O' {double, fixed 8 Byte, stored as double without any conversion}{$ENDIF} ];
  {struktura zazn. zapisniku, pripojene soubory predava pres disk}
  {structure of memo appended file on disk}
type
  TDBAsTypes = (dbtString, dbtDate, dbtTime, dbtFloat, dbtLogical, dbtMemo, dbtNumber
   {$IFDEF VERDB5UP},dbtBinary, dbtTimeStamp, dbtLongInt, dbtAutoInc, dbtDouble{$ENDIF});
  TDBAsTypesSet = set of TDBAsTypes;
const
  ccDBTypes: array [TDBAsTypes] of AnsiChar = ('C','D','T','F','L','M','N'{$IFDEF VERDB5UP},'B','@','I','+','O'{$ENDIF});

  {  TDBTTypes  }

type
  TDBTTypes = packed record
    NumberOf: LongInt; { record no. }
    AsFileType: array[1..3] of AnsiChar; { extension of saved type }
    Used: Boolean; { used/unused }
    SizeOfMemo: LongInt; { size of appended file }
    FileDateTime: Double; { original date and time of file }
    {MemoField:Array[1..SizeOfMemo] of Byte; // ulozeny soubor}
  {$IFDEF VER10UP}
  public
    procedure Clear;
  {$ENDIF}
  end;
{
  struktura zaznamu klice, je bez hlavicky
  structure record key, without head
  TIDXTypes = packed record
    ItemNo:LongInt;                   // refer to record in table
    Key:TDBField.Len;                 // key component
  end;
}

  TjbDBF = class;

  TDBFieldRec = packed record
    name: TCharrs11;                { Name of the field                     11 }
    what: AnsiChar;                 { Type of data in this field             1 }
    data: LongInt;            { Not used or Displacement of field in record  4 }
    len: Byte;                      { Length of the field                    1 }
    places: Byte;                   { Number of decimal places               1 }
    dfIdent: Byte;                  { Field-level flags:                     1 }
                                    {Bit 1: is this a "system" (hidden) field? }
                                    {Bit 2: is this field nullable?            }
                                    {Bit 3: is this field NOCPTRANS?           }
                                    {---}
                                    { NOTE: other values interpretation        }
                                    {  Field flags:
                                     $01   System Column (not visible to user)
                                     $02   Column can store null values
                                     $04   Binary column (for CHAR and MEMO only)
                                     $06   ($02+$04) When a field is NULL
                                           and binary (Integer, Currency,
                                           and Character/Memo fields)
                                     $0C   Column is autoincrementing          }
    case integer of
    $0: {my extension}
    (
      idxtyp: TFieldReq;            { typ klice unikatni/duplicitni...       1 }
      idxsrt: TSortByIndex;         { setridit vzestupne, sestupne, custom...1 }
      idx: TCharrs11                { here file name in index  XXXXXXXXXXX  11 }
    );
    $3: {standard}
     (unused: array [0..12] of Byte)
  end;

  {  TDBField  }

  TDBField = class;

  TDBFieldNotifyEvent = procedure(Sender: TDBField) of object;
  TDBFieldGetTextEvent = procedure(Sender: TDBField; var Text: string; DisplayText: Boolean) of object;
  TDBFieldSetTextEvent = procedure(Sender: TDBField; const Text: string) of object;
  TDisplayEvent = procedure(Sender: TDBField; var Text: string) of object;

  TDBField = class(TCollectionItem)
  {$IFDEF VER9UP}strict protected{$ELSE}protected{$ENDIF}
    FDBFieldRec: TDBFieldRec;
  private
    FOwner: TCollection;
    FDisplayName: string;
    FOnGetText: TDBFieldGetTextEvent;
    FOnChange: TDBFieldNotifyEvent;
    FOnSetText: TDBFieldSetTextEvent;
    FOnDisplay: TDisplayEvent;
    FOnValidate: TDBFieldNotifyEvent;
    function GetAsBoolean: Boolean;
    function GetAsCurrency: Currency;
    function GetAsDate: TDateTime;
    function GetAsFloat: Double;
    function GetAsInteger: Longint;
    function GetAsString: string;
    function GetAsTime: TDateTime;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(const Value: Longint);
    procedure SetAsString(const Value: string);
    procedure SetAsTime(const Value: TDateTime);
    function GetAsMemo: string;
    procedure SetAsMemo(const Value: string);
    function GetDataFieldIdent: Byte;
    function GetFieldLength: Byte;
    function GetFieldName: AnsiString;
    function GetFieldType: TDBAsTypes;
    function GetIndexName: AnsiString;
    function GetIndexSort: TSortByIndex;
    function GetIndexType: TFieldReq;
    function GetPlaces: Byte;
    function GetTag: LongInt;
    procedure SetDataFieldIdent(const Value: Byte);
    procedure SetFieldLength(const Value: Byte);
    procedure SetFieldName(const Value: AnsiString);
    procedure SetFieldType(const Value: TDBAsTypes);
    procedure SetIndexName(const Value: AnsiString);
    procedure SetIndexSort(const Value: TSortByIndex);
    procedure SetIndexType(const Value: TFieldReq);
    procedure SetPlaces(const Value: Byte);
    procedure SetTag(const Value: LongInt);
  protected

    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;

    function IsEqual(Value: TDBField): Boolean;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function IsNull: Boolean;
    function IsNotNull: Boolean;
    function BeginString(prm: string): Boolean;
    function EndingString(prm: string): Boolean;
    function ContainString(prm: string): Boolean;
    property Field: TDBFieldRec read fDBFieldRec write fDBFieldRec;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property AsMemo: string read GetAsMemo write SetAsMemo;
  published
    property FieldName: AnsiString read GetFieldName write SetFieldName; //name
    property FieldType: TDBAsTypes read GetFieldType write SetFieldType; //what
    property Tag: LongInt read GetTag write SetTag; //data
    property FieldLength: Byte read GetFieldLength write SetFieldLength;  //len
    property Places: Byte read GetPlaces write SetPlaces; //
    property IndexType: TFieldReq read GetIndexType write SetIndexType;  //idxtyp
    property IndexSort: TSortByIndex read GetIndexSort write SetIndexSort; //
    property DataFieldIdent: Byte read GetDataFieldIdent write SetDataFieldIdent; //
    property IndexName: AnsiString read GetIndexName write SetIndexName; //
    property OnGetText: TDBFieldGetTextEvent read FOnGetText write FOnGetText;
    property OnSetText: TDBFieldSetTextEvent read FOnSetText write FOnSetText;
    property OnValidate: TDBFieldNotifyEvent read FOnValidate write FOnValidate;
    property OnDisplay: TDisplayEvent read FOnDisplay write FOnDisplay;
    property OnChange: TDBFieldNotifyEvent read FOnChange write FOnChange;
  end;

  {  TDBFields  }

  TDBFields = class({$IFDEF VER4UP}TOwnedCollection{$ELSE}TCollection{$ENDIF})
  private
    FOwner: TPersistent;
    function GetFieldValue(const FieldName: AnsiString): TDBField;
    function GetItem(Index: Integer): TDBField;
    procedure SetFieldValue(const FieldName: AnsiString;
      const Value: TDBField);
    procedure SetItem(Index: Integer; const Value: TDBField);
  public
    constructor Create{$IFDEF VER4UP}(AOwner: TPersistent){$ENDIF};
    function Add: TDBField;
    procedure RemoveField(Value: TDBField);
    function CreateField(const FieldName: AnsiString): TDBField;
    function IndexOf(const AName: AnsiString): Integer;
    function IsEqual(Value: TDBFields): Boolean;
    function FindByName(const Value: AnsiString): TDBField;
    property Items[Index: Integer]: TDBField read GetItem
    write SetItem; default;
    property Values[const FieldName: AnsiString]: TDBField read GetFieldValue
    write SetFieldValue;
  end;

  {  TKey  }

  TKey = AnsiString;

  TLocateOption = (loCaseSensitive, loPartialKey, loNextLocate);
  TLocateOptionSet = set of TLocateOption;
{----------------------------------------------------------------------------}

  {  TjbDBF  }

  TjbDBF = class(TComponent)
  private
    FDBFName: string;                                        { Full name table }
    FDBFIsOpened: Boolean;                          { TRUE when is file opened }
    FDBFStoreByIndex: Boolean;                           { Store by list index }
    FDBFHandle: file;                                  { Handle of actual file }
    FDBFExist: Boolean;           { Indicate file exists when is name assigned }
    FDBFReadOnly: Boolean;                                         { Read only }
    FDBFSaveOnClose: Boolean;                                  { Save on close }
    FDBFHeader: TDBFHeader;                       { Header,  filled after open }
    FDBFIndex: string;                                 { Actual index for FIND }
    FDBFPassword: AnsiString;                         { Administrator password }
    //FDBFFilter: string;                                            { not used}
    FLangCodePage: LongInt; {currently used language driver corresponding with header}
    FDBFIndexList: TStringList;                    { List all indexes of table }
    FDBFBuff: AnsiString{PBigArray};                { Temp FDBFBuff for record }
    FDBFCurrRec: LongInt;                    { Cursor position point to record }
    FDBFCountItems: Integer;                        { Count of recors collumns }
    FOnError: TDBFError;                      { Event for error administration }
    FOnWarn: TDBFError;                    { Event for warnings administration }
    FOnMakeFields: TDBFMakeItems;                { For create fields in record }
    FOnErase: TDBFBeforeConfirm;               { For confirm with pack of .DBF }
    FOnOverwrite: TDBFBeforeConfirm;  { For confirm with overwrite of.DBF .IDX }
    FOnAdded: TNotifyEvent;                                  { If record added }
    FDBFOnAltSort: TDBFAltSort;               { Alternative sort on stringlist }
    FOnChange: TDBFChange;                               { If record in change }
    FOnChanged: TNotifyEvent;                           { If record is changed }
    FOnDelete: TDBFConfirm;
    FOnActualize: TDBFActualize;
    FOnDeleted: TNotifyEvent;                           { If record is deleted }
    FOnPassword: TDBFPassword;       { If administrator request password check }
    FOnOpened: TDBFOpened;                                { If table is opened }
    FOnAsSigned: TDBFAsSigned;                               { If table attach }
    FOnFound: TNotifyEvent;                         { If found record by index }
    FOnErased: TNotifyEvent;                              { If table is packed }
    FOnNavigate: TDBFNavigate;                       { If navigation is called }
    FOnProgress: TDBFProgress;   { If table is updating, show percent on gauge }
    FOnUpdate: TNotifyEvent;                         { If record is actualized }
    FOnClosed: TNotifyEvent;                              { If table is closed }
    FOnLoaded: TNotifyEvent;               { If record attach to buffer memory }
    FOnQuery: TDBFQuery;                       { For query with find statement }
    function GetField(Index: Integer): TDBField; { /VN/ }
    function GetFieldByName(const Key: TKey): TDBField; { /AR/ }
    function GetPassword: string;
    function GetRecordsCount: LongInt; { return the records count /VN/ }
    function IsCurrentRecDeleted: Boolean; { /VN/ }
    procedure SetFileName(name: string);
    procedure SetPassword(const thepassword: string);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  protected
    FDBFFields: TDBFields; { The field data }
    procedure Fatal(const Msg: string);
    procedure Warn(const Msg: string);
    procedure PutSysDate;
    function LangDriver2CodePage(LangDrv: Byte): Integer;
    function CodePage2LangDriver(CodePage: Integer): Byte;
  public
    constructor Create(AOWner: TComponent); override;
    destructor Destroy; override;
    function GetVersion: string;
    procedure Close; virtual;
    function Open: Boolean; virtual;
    function Write(r: longint): TStatusWrite; virtual;
    procedure Seek(r: longint); virtual;
    procedure InternalAppend; virtual;
    procedure GotoStart;
    procedure GotoEnd;
    procedure GotoNext;
    procedure GotoPrev;
    function Explore(Key: TKey): TDBField;
    function Delete(R: longint): TStatusWrite; virtual;
    function UpdateIndexes(R: LongInt): Boolean; virtual;
    procedure RemoveIndexes(R: LongInt); virtual;
    procedure MakeIndex(const IdxName: string; const Key: TKey;
     const IdxType: TFieldReq = dbfDuplicates; const idxsorting: TSortByIndex = dbfAscending); {make index} virtual;
    function Search(const Key: TKey; const Value: string; const From: Integer = 0): Integer; virtual;
    function Find(const Key: TKey; const  Value: string): Integer; {search value by key} virtual;
    procedure InternalSave(const Key: TKey; const Rec: string); {field of record} virtual;
    procedure InternalLoad(const Key: TKey; var Rec: string); {with conversion} virtual;
    function Load(const Key: TKey): string;
    procedure Update(R: LongInt); virtual;
    procedure CreateDB(const fname: string); virtual;
    procedure ClearFields;
    function MakeField(posit: Byte; const iname: AnsiString; iFldType: TDBAsTypes; ilen: Byte;
      iplaces: Byte; const idxnme: AnsiString;
      iUnique: TFieldReq = dbfDuplicates; iSorting: TSortByIndex = dbfAscending): Boolean; virtual;
    function Cover: Boolean; virtual;
    procedure UnCover; virtual;
    procedure RemoveIndex(const Name: string); virtual;
    function IsMarked: Boolean; {is in transaction?} virtual;
    function ReIndex: Boolean; virtual;
    procedure IncNumRec;
    function GetLastNoOfMemo: Integer;
    function SaveMemo(No: LongInt; const FName: string): Boolean; virtual;
    function LoadMemo(No: LongInt; var FName: string): Boolean; virtual;
    function EraseMemo(No: LongInt): Boolean; virtual;
    function PackDBF: Boolean; virtual; {added by /AR/ }
    { aliased functions for better compatibility }
    function EOF: Boolean;
    function BOF: Boolean;
    procedure Next;
    procedure Prior;
    procedure First;
    procedure Last;
    procedure Post;
    procedure Append;
    function Locate(const KeyFields: string; const KeyValues: string;
      Options: TLocateOptionSet): Boolean;
    procedure Cancel;
    procedure PackDBT; virtual;
    property CurrRec: LongInt read FDBFCurrRec;
    property RecordsCount: LongInt read GetRecordsCount; { /VN/ }
    property FieldsCount: Integer read FDBFCountItems; { /VN/ }
    property Fields[Index: Integer]: TDBField read GetField; { /VN/ }
    property CurrentRecDeleted: Boolean read IsCurrentRecDeleted; { changed /VN/ }
    property FieldByName[const Key: TKey]: TDBField read GetFieldByName; default;{ /AR/ }
  published
    property Active: Boolean read GetActive write SetActive;
    property ByIndex: string read FDBFIndex write FDBFIndex;
    property DBFields: TDBFields read FDBFFields write FDBFFields;
    property FileIsExist: Boolean read FDBFExist;
    property FileIsOpen: Boolean read FDBFIsOpened;
    property FileName: string read FDBFName write SetFileName;
    property LangCodePage: LongInt read FLangCodePage write FLangCodePage;
    property Password: string read GetPassword write SetPassword;
    property ReadOnly: Boolean read FDBFReadOnly write FDBFReadOnly;
    property SaveOnClose: Boolean read FDBFSaveOnClose write FDBFSaveOnClose;
    property StoreByIndex: Boolean read FDBFStoreByIndex write FDBFStoreByIndex;
    property OnActualize: TDBFActualize read FOnActualize write FOnActualize;
    property OnAdded: TNotifyEvent read FOnAdded write FOnAdded;
    property OnAltSort: TDBFAltSort read FDBFOnAltSort write FDBFOnAltSort;
    property OnAsSigned: TDBFAsSigned read FOnAsSigned write FOnAsSigned;
    property OnChange: TDBFChange read FOnChange write FOnChange;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnClosed: TNotifyEvent read FOnClosed write FOnClosed;
    property OnDelete: TDBFConfirm read FOnDelete write FOnDelete;
    property OnDeleted: TNotifyEvent read FOnDeleted write FOnDeleted;
    property OnErase: TDBFBeforeConfirm read FOnErase write FOnErase;
    property OnErased: TNotifyEvent read FOnErased write FOnErased;
    property OnError: TdbfError read FOnError write FOnError;
    property OnFound: TNotifyEvent read FOnFound write FOnFound;
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
    property OnMakeFields: TDBFMakeItems read FOnMakeFields write FOnMakeFields;
    property OnNavigate: TDBFNavigate read FOnNavigate write FOnNavigate;
    property OnOpened: TDBFOpened read FOnOpened write FOnOpened;
    property OnOverwrite: TDBFBeforeConfirm read FOnOverwrite write FOnOverwrite;
    property OnPassword: TDBFPassword read FOnPassword write FOnPassword;
    property OnProgress: TDBFProgress read FOnProgress write FOnProgress;
    property OnQuery: TDBFQuery read FOnQuery write FOnQuery;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
    property OnWarn: TdbfError read FOnWarn write FOnWarn;
  end;

procedure Register;

implementation

procedure Register;

begin
  RegisterComponents('Library', [TjbDBF]);
end;

{  Helper functions  }

{$IFDEF VER80}
function Trim(const S: string): string;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[1] <= ' ') do
    System.Delete(Result, 1, 1);
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do
    System.Delete(Result, Length(Result), 1);
end;
{$ENDIF}
{$Ifdef UNICODE}
function LeftPad(S: string; Len: Integer): string; overload;
begin
  Result := S;
  while Length(Result) < Len do
    Result := SpacerD + Result;
end;

function RightPad(S: string; Len: Integer): string; overload;
begin
  Result := S;
  while Length(Result) < Len do
    Result :=  Result + SpacerD;
end;
{$ENDIF}
function LeftPad(S: AnsiString; Len: Integer): AnsiString; overload;
begin
  Result := S;
  while Length(Result) < Len do
    Result := SpacerD + Result;
end;

function RightPad(S: AnsiString; Len: Integer): AnsiString; overload;
begin
  Result := S;
  while Length(Result) < Len do
    Result :=  Result + SpacerD;
end;

{  TjbDBF  }

constructor TjbDBF.Create(AOwner: TComponent);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Create
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: AOwner: TComponent
  Result:    None
-------------------------------------------------------------------------------}
begin
  inherited Create(AOwner);
  FDBFFields := TDBFields.Create{$IFDEF VER4UP}(Self){$ENDIF};
  FDBFIsOpened := False;
  FileName := '';
  FDBFBuff := '';
  FDBFIndexList := TStringList.Create;
  FDBFStoreByIndex := False;
  FDBFExist := False;
  FDBFReadOnly := False;
  FDBFSaveOnClose := False;
  FLangCodePage := 852; //directly sets to 852
end;

destructor TjbDBF.Destroy;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Destroy
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
-------------------------------------------------------------------------------}
begin
  {kdyby byla nahodou tabulka otevrena, tak ji explicitne uzavri}
  {when table was opened then explicit close it}
  if FDBFIsOpened then
    Close;
  {uvolni instanci seznamu}
  {free list}
  FDBFIndexList.Free;
  {$IFNDEF VER4UP}FDBFFields.Free;{$ENDIF}
  inherited Destroy;
end;

procedure TjbDBF.SetActive(const Value: Boolean);
begin

end;

procedure TjbDBF.SetFileName(name: string);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.SetFileName
  Author:    Jaro Benes
  DateTime:  2009.07.21
  Arguments: name: string
  Result:    None
  Note:      Linux changes by * Andrea Russo *
-------------------------------------------------------------------------------}
begin
  {kdyby byla nahodou tabulka otevrena, tak ji explicitne uzavri}
  {when table was opened then explicit close it}
  if FDBFIsOpened then
    Close;
  {a pak zmen jmeno}
  {and change name after them}
  FDBFName := name;
  {prirazeni jmena oznam, a nabidni k pripadne zmene zvenku}
  {give a message if name assigned for possible outside change}
  if AsSigned(FOnAsSigned) then FOnAsSigned(Self, FDBFName);
{$IFNDEF LINUX} { /AR/ }
  FDBFName := LowerCase(FDBFName);
{$ENDIF}
  {test, zda tabulka existuje, neexistuje-li, bude potreba ji zalozit}
  {test for table exists - when not exists, create it latter}
  FDBFExist := FileExists(FDBFName);
end;

function TjbDBF.GetPassword: string;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.GetPassword
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    string
  Purpose:   Get password
-------------------------------------------------------------------------------}
begin
  Result := string(FDBFPassword)
end;

procedure TjbDBF.SetPassword(const thepassword: string);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.SetPassword
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: const thepassword: string
  Result:    None
  Purpose:   Set password
-------------------------------------------------------------------------------}
begin
  FDBFPassword := AnsiString(thepassword)
end;

function TjbDBF.Open: Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Open
  Author:    Jaro Benes
  DateTime:  2009.07.21
  Arguments: None
  Result:    Boolean
  Purpose:   Open existing databese file
-------------------------------------------------------------------------------}
var
  Temp: TDBFieldRec;
  FF: TDBField;
  Done: Boolean;
  Readed: {$IFDEF VER80}Word{$ELSE}Integer{$ENDIF};
  Pass: string;
begin
  Result := False;
  {kdyby nahodou byla tabulka otevrena, tak ji zavri}
  {close table when is opened still}
  if FDBFIsOpened then Close;
  {neexistuje-li tabulka, nedelej nic}
  {when table not exists, do nothing}
  if not FDBFExist then Exit;

  done := False;

  if AsSigned(FOnPassword) then FOnPassword(Self, Pass);
  {prikryti heslem je mozne overit tady}
  {cover by password validate here}
  if FDBFPassword <> '' then
    if AnsiString(Pass) <> FDBFPassword then
    begin
      Fatal(msgNoPasswordMatch);
      Exit;
    end;
  {otevreni pres handle}
  {open through handle}
  AsSignFile(FDBFHandle, FDBFName);
  try
    Reset(FDBFHandle, 1);
    {kdyz se povedlo, zustane otevrena az do close}
    {if success, stay opened to close}
    FDBFIsOpened := True;
    {vyzvedni si header}
    {get header}
    BlockRead(FDBFHandle, FDBFHeader, SizeOf(TDBFHeader)); { Get the header }
    {synchronize LangDriver}
    FLangCodePage := LangDriver2CodePage(FDBFHeader.LangDrv);
    {tohle bude pracovni buffer, kam budes davat data}
    {working data buffer is here}
    SetLength(FDBFBuff, FDBFHeader.RecLen);
    {tady ctes polozky/sloupce a v tehle promenn vzdy budou}
    {reading field here}
    FDBFCountItems := 0;
    ClearFields;
    repeat
      {cti obezretne, co kdybys narazil na neocekacany konec}
      {read circumspection what about giv unexpected end of file }
      {$I-}
      BlockRead(FDBFHandle, temp, SizeOf(TDBFieldRec), Readed);
      {$I+}
      if Temp.name[0] <> #$0D then
      begin
        {ukazuj na prvni volny}
        {show first free}
        Inc(FDBFCountItems);
        FF := FDBFFields.Add;
        //FF.FDBFieldRec := temp;
        FF.Field := Temp;
        FillChar(temp, SizeOf(temp), 0);
      end
      else
      begin
        done := True;
        {jsou-li nacteny prave dva znaky, tabulka je prazdna, uprav pozici}
        {when two chars readed, table is empty, correct position}
        if readed = 2 then
          System.Seek(FDBFHandle, System.FilePos(FDBFHandle) - 1)
          {jinak se postav na prvni zaznam a nacti ho do bufferu}
          {other stay on first record and read it into buffer}
        else seek(0);
      end;
    until DONE;
    {seek(0);}
    if AsSigned(FOnOpened) then FOnOpened(Self, FDBFIsOpened);
    Result := True;
  except
    Fatal(msgErrorOnOpen)
  end;
end;

procedure TjbDBF.Append;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Append
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   Flag set up indicates new record only
-------------------------------------------------------------------------------}
begin
  {funkce se vola po vlozeni hodnot do prazdneho bufferu}
  {function is called after store valuse into new clean buffer}
  FDBFCurrRec := -1;
  {it dont't write buffer to disk, only set flag as indicator for POST procedure}
  {
  must be called sequentially like:
  Append;
  FieldByName['number'].AsInteger := 5;
  ...
  Post;
  }
end;

function TjbDBF.BOF: Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.BOF
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    Boolean
  Purpose:   Get BOF mark
-------------------------------------------------------------------------------}
begin
  {indikuje ze jsme na prvnim zaznamu nebo je tabulka prazdna}
  {first row indicated or empty table}
  Result := FDBFCurrRec = 0;
end;

procedure TjbDBF.Cancel;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Cancel
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   Buffer is cleared as cancel
-------------------------------------------------------------------------------}
begin
  SetLength(FDBFBuff, FDBFHeader.RecLen);
  FillChar(FDBFBuff[1], FDBFHeader.RecLen, spacerD);
end;

procedure TjbDBF.ClearFields;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.ClearFields
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   Remove all declared column from definition
-------------------------------------------------------------------------------}
begin
  FDBFFields.Clear;
end;

procedure TjbDBF.Close;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Close
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   Close DBF and release file handle
-------------------------------------------------------------------------------}
var
  B: Byte;
begin
  if not FDBFIsOpened then Exit;
  {nasleduje test EOF mark a oznaceni kdyz chybi}
  {tohle ale lze udelat v pripade, kdyz soubor neni read only}
  {je-li read only, zahlasi se chyba a nejde soubor opravit}
  {follow EOF mark test; if missing}
  {do it when isn't file read-only only}
  {if read only get error message and do not repair it}

  System.Seek(FDBFHandle, FileSize(FDBFHandle) - 1);
  Blockread(FDBFHandle, b, 1);
  if B <> $1A then
  begin
    if not FDBFReadOnly and not IsMarked then
    begin
      {je-li povoleno stouchni tam posledni zaznam}
      {when consented, poke last record there}
      if FDBFSaveOnClose then Write(CurrRec);
      B := $1A; {there is EOF mark }
      BlockWrite(FDBFHandle, B, 1);
    end
    else
      Fatal(msgEOFmarkMiss)
  end;
  CloseFile(FDBFHandle);
  {date of actualization}
  if FileMode <> 0 then {[AR]}
    PutSysDate;
  FDBFIsOpened := False; {message - file closed}
  if AsSigned(FOnClosed) then FOnClosed(Self);
  FDBFBuff := '';
end;

function TjbDBF.Write(R: LongInt): TStatusWrite;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Write
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: R: LongInt
  Result:    TStatusWrite
  Purpose:   Basic function for write data (InternalWrite)
             hlavni funkce zapisu, data jsou vzdy ukladana na pozadani
             main function for write, data store for demand
-------------------------------------------------------------------------------}
var
  Cancel: Boolean;
begin
  Result := dbfError;
  {zapis muze byt proveden pouze v pripade ze neni jen pro cteni, existuje a je otevren}
  {write can be do only when isn't read-only or exists or is opened}
  if FDBFReadOnly or not FDBFExist or not FDBFIsOpened then Exit;
  {nastav pro pripad, kdyby to pouzival jiny proces}
  {set for occur, if it use other process}
  Result := dbfBusy;
  {je nastaveno navesti transakce, tj. pouziva to nekdo jiny}
  {is set signal label of transaction -> use it another process}
  if not IsMarked then
  begin
    {ale ted ho chces pouzit ty}
    {but now it want use you}
    Cover;
    try
      {priznak storna}
      {cancel prompt}
      Result := dbfCancel;
      Cancel := False;
      if FDBFCurrRec <> R then FDBFCurrRec := R;
      {kdykoliv zapisujes, pak menis zaznam; zde ho lze odvolat}
      {write any time, you change record -> you can cancel here}
      if AsSigned(FOnChange) then FOnChange(Self, Cancel);
      {je-li zaznam odvolan, zapis nebude proveden}
      {when record canceled, no write}
      if Cancel then Exit;
      {priznak chyby}
      {error prompt}
      Result := dbfError;
      {pokousis se updatovat indexy}
      {you can update of indexes}
      try
        {jsou-li updatovany}
        {if updated now}
        if UpdateIndexes(R) then
        begin
          try
            {vyhledej fyzicky zaznam k prepisu}
            {search physical record for overwrite}
            System.Seek(FDBFHandle, R * FDBFHeader.recLen + FDBFHeader.headLen);
            {nastav signal pro platny zapis - zaznam je platny}
            {set prompt for true write -> record is OK}
            FDBFBuff[1] := ' '; { Record not deleted! } {uncomment /AR/ }
            {zapis ho na vyhledane misto}
            {write it to found place}
            BlockWrite(FDBFHandle, FDBFBuff[1], FDBFHeader.RecLen);

            PutSysDate;
            {teprve tady je vsechno OK}
            {only here is OK}
            Result := dbfOK;
          except
            on EInOutError do
            begin
              Fatal(msgErrorOnWrite);
              Result := dbfError;
            end;
          end;
          {zahlas, zes zaznam zmenil}
          {get message - record is changed}
          if AsSigned(FOnChanged) then FOnChanged(Self);
        end;
      except
        {v pripade vyskytu nejake chyby je ale musis odstranit}
        {but when error ocurred, have to remove all}
        RemoveIndexes(R)
      end
    finally
      {a tady ho zase mohou pouzivat jini}
      {and there can use it others}
      UnCover;
    end;
  end;
end;

function TjbDBF.Delete(R: longint): TStatusWrite;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Delete
  Author:    Jaro Benes
  DateTime:  2009.07.21
  Arguments: R: longint
  Result:    TStatusWrite
  Purpose:   Delete record by number
-------------------------------------------------------------------------------}
var
  Confirm: Boolean;
begin
  Result := dbfError;
  {zapis muze byt proveden pouze v pripade ze neni jen pro cteni, existuje a je otevren}
  {write can be do only when isn't read-only or exists or is opened}
  if FDBFReadOnly or not FDBFExist or not FDBFIsOpened then Exit;
  Result := dbfBusy;
  if not IsMarked then
  begin
    Cover;
    try
      {zadej o svoleni s vymazanim vety}
      {require consent record delete}
      if AsSigned(FOnDelete) then FOnDelete(Self, Confirm) else Confirm := True;
      if Confirm then
      begin
        try
          {nezmenil-li se zaznam od aktualniho}
          {when actual record is the same as required}
          if FDBFCurrRec <> R then FDBFCurrRec := R;
          {vyhledej ho v zaznamech}
          {seek new position}
          System.Seek(FDBFHandle, R * FDBFHeader.recLen + FDBFHeader.headLen);
          {nastav priznak vymazani}
          {set erase label }
          FDBFBuff[1] := DeleteFlag; { Record is deleted! }
          {zapis do souboru}
          {write it into file}
          BlockWrite(FDBFHandle, FDBFBuff[1], FDBFHeader.recLen);
          {aktualizuj indexy, tj. odstran z nich vymazany zaznam}
          {and do index actualizing }
          RemoveIndexes(R);

          PutSysDate;
          Result := dbfOk; { /AR/ }
        except
          on EInOutError do Fatal(msgCannotDeleteItem);
        end;
        {oznam zes vymazal}
        {and get message}
        if AsSigned(FOnDeleted) then FOnDeleted(Self);
      end;
    finally
      UnCover;
    end
  end;
end;

function TjbDBF.Search(const Key: TKey; const Value: string; const From: Integer = 0): Integer;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Search
  Author:    Jaro Benes
  DateTime:  2009.07.21
  Arguments: const Key: TKey; const Value: string; const From: Integer = 0
  Result:    Integer
  Purpose:   Search for value repeately
-------------------------------------------------------------------------------}
var
  F: file;
  A, B, Size: Integer;
  S: string;
  I, N, L: LongInt;
  OK, Cancel: Boolean;
  SS: string;
  idxnme: AnsiString;
begin
  Result := -1;
  {Musi existovat a byt otevrena neprazdna tabulka, lze pouzit i pro preindexovani}
  {opened and unempty table}
  if not (FDBFExist and FDBFIsOpened and (FDBFHeader.NumRecs > 0)) then Exit;
  I := FDBFFields.IndexOf(Key);
  if I = -1 then Exit;
  idxnme := FDBFFields[I].IndexName;
  {je definovan index a index existuje}
  if (idxnme <> '') and FileExists(ExtractFilePath(FDBFName) + string(idxnme)) then
  begin
    idxnme := AnsiString(ExtractFilePath(FDBFName) + string(idxnme));
    {Otevri ji na stejnem miste, pouzij idxname a defaultni priponu}
    {open index}
    AsSignFile(F, string(idxnme));
    try
      ReSet(F, 1);
      try
        B := FDBFFields[I].FieldLength;
        {v tomto pripade vydano varovani a odchod, klic musi byt nenulovy}
        {key have to non zero}
        if B = 0 then Exit;
        {sestaveni zaznamu}
        {build record}
        Size := B + SizeOf(LongInt);
        {overeni na velikost souboru}
        {size of file for align}
        I := FileSize(F);
        {v pripade ze neco zbyde (polozky nejsou align) tak chyba}
        {fatal error occurr when non align}
        if (I mod Size) <> 0 then
        begin
          Fatal(Format(msgErrorOnIdxTable, [idxnme]));
          Exit;
        end;
        {tohle je pocet polozek}
        {count of records}
        N := I div Size;
        {nastav prostor na S}
        {make place for it}
        SetLength(S, Size);
        FillChar(S[1], Size, ' ');
        Cancel := False;
        {prochazej periodicky klic}
        {go by key}
        A := -1;
        for I := 0 to N - 1 do
        begin
          BlockRead(F, S[1], Size);
          Move(S[1], L, SizeOf(L));
          SS := Trim(Copy(S, 5, 255));
          {zde je dotaz na tabulku, uzivatel filtruje dle pole}
          {query to table, user do filtering}
          {no accept}
          OK := False; {predpoklad, ze ho nechci}
          if Assigned(FOnQuery) then
            FOnQuery(Self, string(IdxNme), Value, Trim(string(SS)), OK, Cancel)
          else
            OK := AnsiCompareText(Value, S) = 0;

          if OK then
          begin
            {je-li pozadovany filtr akceptovan, vyzvedni zaznam}
            {when accept, get record from table}
            Seek(L);
            {zaznam se musi zpracovat, jinak jsou data ztracena}
            {zde se data ctou napr. do listboxu nebo stringgridu}
            {record must be worked but data throw off}
            {may be read to list ??}
            if Assigned(FOnFound) then
              FOnFound(Self) {periodically call when is found}
            else
            begin
              Result := L;
              Exit;
            end;
          end;
          {aktualizujes citac - vhodne je tez nastavovat kurzor}
          {for gauge}
          B := Round((I + 1) / (N / 100));
          if Cancel then B := 100;
          if A <> B then
          begin {tohle je proto, aby se progress volal jen 101x}
            A := B;
            if AsSigned(FOnProgress) then FOnProgress(Self, prgSearchByKey, B);
          end;
          if Cancel then Break;
        end;
        Result := -1;
      finally
        CloseFile(F)
      end;
    except
      Fatal(Format(msgIdxTableNotFound, [ExtractFilePath(FDBFName) + string(IdxNme)]));
    end;
  end
  else
  begin
    {pokud index neni zaveden}
    Seek(From);
    {pohyb shora dolu}
    while not Eof do
    begin
      if AnsiCompareText(FieldByName[Key].AsString, Value) = 0 then
      begin
        Result := FDBFCurrRec; {aktualni zaznam}
        Exit; {a ven}
      end;
      Next;
    end;
    Result := -1;
  end;
end;

procedure TjbDBF.Seek(R: LongInt);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Seek
  Author:    Jaro Benes
  DateTime:  2009.07.21
  Arguments: R: LongInt
  Result:    None
  Purpose:   Navigate to explicit position
-------------------------------------------------------------------------------}
var
  L: LongInt;
  Readed: {$IFDEF VER80}Word{$ELSE}Integer{$ENDIF};
begin
  if not FDBFExist or not FDBFIsOpened then Exit;
  {nezmenil-li se zaznam od aktualniho}
  {when actual record is the same as required}
  if FDBFCurrRec <> R then FDBFCurrRec := R;
  {fyzicka delka zacatku vet}
  {physical size of record begins}
  L := R * FDBFHeader.recLen + FDBFHeader.headLen;
  {kdyz je nahodou za}
  {when beyond}
  if L > (FileSize(FDBFHandle) - 1) then Exit;
  {postav se tam}
  {stay there}
  System.Seek(FDBFHandle, L);
  {precti vetu do bufferu}
  {read record into buffer}
  BlockRead(FDBFHandle, FDBFBuff[1], FDBFHeader.RecLen, Readed);
  {veta je uspesne nactena, jen kdyz je v bufferu cela}
  {when all readed}
  if FDBFHeader.RecLen = Readed then
    {a zahlas zes ji precetl}
    {get message}
    if AsSigned(FOnLoaded) then FOnLoaded(Self);
end;

procedure TjbDBF.GotoStart;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.GotoStart
  Author:    Jaro Benes
  DateTime:  2009.07.21
  Arguments: None
  Result:    None
  Purpose:   Navigate to top of the table
-------------------------------------------------------------------------------}
begin
  if not FDBFExist or not FDBFIsOpened then Exit;
  {nastav se na prvni zaznam}
  {seek to first}
  Seek(0);
  {zahlas pro navigaci, ze na nem stojis}
  {and get message for navigation your position}
  if AsSigned(FOnNavigate) then FOnNavigate(Self, 0);
end;

procedure TjbDBF.GotoEnd;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.GotoEnd
  Author:    Jaro Benes
  DateTime:  2009.07.21
  Arguments: None
  Result:    None
  Purpose:   Navigate to bottom of the table
-------------------------------------------------------------------------------}
begin
  if not FDBFExist or not FDBFIsOpened then Exit;
  {nastav se na posledni zaznam}
  {seek to last}
  Seek(FDBFHeader.numRecs - 1);
  {zahlas pro navigaci, ze na nem stojis}
  {and get message for navigation your position}
  if AsSigned(FOnNavigate) then FOnNavigate(Self, FDBFHeader.numRecs - 1);
end;

procedure TjbDBF.GotoNext;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.GotoNext
  Author:    Jaro Benes
  DateTime:  2009.07.21
  Arguments: None
  Result:    None
  Purpose:   Navigate to next record
-------------------------------------------------------------------------------}
begin
  if not FDBFExist or not FDBFIsOpened then Exit;
  {nastav se na nasledujici zaznam}
  {seek to next}
  Seek(FDBFCurrRec + 1);
  {zahlas pro navigaci, ze na nem stojis}
  {and get message for navigation your position}
  if AsSigned(FOnNavigate) then FOnNavigate(Self, FDBFCurrRec + 1);
end;

procedure TjbDBF.GotoPrev;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.GotoPrev
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   Move cursor to previous position
-------------------------------------------------------------------------------}
begin
  if not FDBFExist or not FDBFIsOpened then Exit;
  {nastav se na predchazejici zaznam}
  {seek to previous}
  Seek(FDBFCurrRec - 1);
  {zahlas pro navigaci, ze na nem stojis}
  {and get message for navigation your position}
  if AsSigned(FOnNavigate) then FOnNavigate(Self, FDBFCurrRec - 1);
end;

procedure TjbDBF.InternalAppend;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.InternalAppend
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   Store new item/record into DBF
-------------------------------------------------------------------------------}
begin
  {nemuzes nic pridavat, kdyz je jen ke cteni, nebo neexistuje, neni otevren}
  {cannot doing anything when is read-only or no exists or is closed}
  if FDBFReadonly or not FDBFExist or not FDBFIsOpened then Exit;
  if not IsMarked then
  begin
    Cover;
    try
      {zde je mozne udelat implicitni naplneni zaznamu, coz vrele doporucuji}
      {can do implicit fill of record (I recommend to)}
      if AsSigned(FOnActualize) then FOnActualize(Self, dbfNew);
      try
        {zvyz pocet zaznamu a uloz je do hlavicky}
        {increment counter of records and save it}
        IncNumRec;
        {jdi na fyzicky zacatek}
        {go to start}
        System.Seek(FDBFHandle, 0);
        {zapis hlavicku}
        {write header}
        BlockWrite(FDBFHandle, FDBFHeader, SizeOf(TDBFHeader));
        {zapis-vloz novy zaznam na konec}
        {write-insert new record to end}
        Write(FDBFHeader.numRecs - 1);
        {nastav se na ten zaznam a aktualizuj buffer}
        {set position to new record and do buffer actual}
        Seek(FDBFHeader.numRecs - 1);
      except
        on EInOutError do Fatal(msgCannotAddItem);
      end;
      {a pripadne zahlas, ze zaznam byl pridan}
      {and get message when added}
      if AsSigned(FOnAdded) then FOnAdded(Self);
    finally
      UnCover;
    end;
  end;
end;

procedure TjbDBF.Next;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Next
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   Alias function
-------------------------------------------------------------------------------}
begin
  GotoNext;
end;

procedure TjbDBF.CreateDB(const fname: string);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.CreateDB
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: const fname: string
  Result:    None
  Purpose:   Main function for create database
             Two ways affected
             1/ direcly from items created before by MakeField()
             2/ indirectly through event OnMakeField with specific parameters
-------------------------------------------------------------------------------}
var
  y, m, d: Word;
  c: AnsiChar;
  i: Integer;
  FF: TDBField;
  QQ: TDBFieldRec;
  rL {reclen}, numFields: word;
begin
  {kdyby byla nahodou tabulka otevrena, tak ji explicitne uzavri}
  {close table}
  if FDBFIsOpened then
    Close;
  if FDBFFields.Count = 0 then
  begin
    {jestlize je attachnuty event pro vyrobu pole on line tak ho zavolej}
    {jinak je predpokladano ze pred volanim teto metody byly
     vytvoreny sloupce pomoci MakeField()}
    {on line create field}
    {else before create was MakeField called}
    if AsSigned(FOnMakeFields) then
    I := 0;
    repeat
      {zavolej ho tolikrat, kolik je potreba vyrobit poli}
      {call by columns count}
      with QQ do
        FOnMakeFields(Self, I, Name, What, Len, Places, idx, idxtyp, idxsrt);
      if I <> -1 then
      begin
        FF := FDBFFields.Add;
        FF.Clear;
        {zavolej ho a vyrob zaznam}
        {make field here}
        QQ.What := UpCase(QQ.What);
        FF.Field := QQ;
        {uprav na velka pismena}
        {upper case only please}
        FF.FieldName := QQ.name;
        FF.IndexName := QQ.idx;
      end;
    until I = -1;  {end when event returns -1}
  end;
  numFields := FDBFFields.Count;
  rL := 0;
  for I := 0 to FDBFFields.Count - 1 do
    rL := rL + FDBFFields[I].FieldLength; {size all added column}
  {vytvaris novou tabulku, zde je hlavicka}
  {for new table refill header}
  FillChar(FDBFHeader, SizeOf(FDBFHeader), 0);
  with FDBFHeader do
  begin
    version := $3;
    DecodeDate(Date, y, m, d); {create date}
    year := y mod 100;
    month := Lo(m);
    day := Lo(d);
    numRecs := 0;
    headLen := SizeOf(FDBFHeader) + SizeOf(TDBFHeader) * numFields + 1;
    recLen := rl + 1; {begins delete flag}
  end;
  {tohle je nove jmeno tabulky}
  {new table name}
  FDBFName := fname;
  {priprav ji k fyzickemu zalozeni}
  {prepare it for physical create and store}
  AsSignFile(FDBFHandle, FDBFName);
  try
    ReWrite(FDBFHandle, 1);
    try
      {zalozeni se povedlo, tabulka je otevrena}
      {create is OK, table will be open}
      FDBFIsOpened := TRUE;
      {zapis hlavicku}
      {write the header}
      BlockWrite(FDBFHandle, FDBFHeader, sizeof(TDBFHeader));
      {pro stanoveny pocet sloupcu prochazej}
      {go through by columns}
      for i := 1 to numFields do
      begin
        {existing column is attached}
        QQ := FDBFFields[I - 1].Field;
        {zapis nove vyrobene pole}
        {write new made field}
        BlockWrite(FDBFHandle, QQ, SizeOf(QQ))
      end;
      {za hlavickou nasleduje vzdy CR}
      {sign over header poke CR mark}
      c := #$0D;
      BlockWrite(FDBFHandle, c, 1);
      {konec souboru je indikovan EOF mark}
      {sign end of file poke EOF mark}
      c := #26;
      BlockWrite(FDBFHandle, c, 1);
    finally
      {tady soubor fyzicky zavri}
      {and here file physicaly close}
      CloseFile(FDBFHandle);
    end;
  except
    {ejhle, chyba; tak ji zahlas}
    {ooh, error -> have to message}
    Fatal(msgCreateDBFError)
  end;
  {tabulka je stale uzavrena}
  {table still close}
  FDBFIsOpened := False;
end;

function TjbDBF.MakeField(posit: Byte;                                {position}
  const iname: AnsiString;                                         {name as key}
  iFldType: TDBAsTypes;                                             {field type}
  ilen: Byte;                               {length for non automatic typefield}
  iplaces: Byte;                                      {decimal placed for float}
  const idxnme: AnsiString;          {index name , should be different from key}
  iUnique: TFieldReq = dbfDuplicates;                              {set uniques}
  iSorting: TSortByIndex = dbfAscending): Boolean;               {set sort type}
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.MakeField
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: posit: Byte; const iname: string; iFldType: TDBAsTypes; ilen: Byte;
             iplaces: Byte; const idxnme: string;
             iUnique: TFieldReq = dbfDuplicates;
             iSorting: TSortByIndex = dbfAscending
  Result:    Boolean
  Purpose:   Main function for create column with properties
-------------------------------------------------------------------------------}
var
  FF: TDBField;
begin
  Result := False;
  if (Trim(IName) = '') or ((iFldType in [dbtString, dbtFloat, dbtNumber]) and (ILen = 0)) then
  begin
    Fatal(msgBadDefinitionOfField);
    Exit;
  end;
  Result := True;
  FF := FDBFFields.Add;
  FF.Clear;
  FF.FieldName := iname;
  FF.FieldType := iFldType;
  {tyhle polozky (cas, datum, memo) maji fixni tvar}
  {format is fixed (time, date, memo...)}
  case FF.FieldType of
    dbtString: FF.FieldLength := iLen;
    dbtDate: FF.FieldLength := 8;
    dbtTime: FF.FieldLength := 6;
    dbtFloat:
      begin
        FF.FieldLength := iLen; {bugfix by Jarda Jirava [<mailto:listuj@centrum.cz>]  18.4.2001}
        FF.Places := iplaces; {tohle je jenom pro float/float only}
      end;
    dbtLogical: FF.FieldLength := 1;
    dbtMemo: FF.FieldLength := 10;
    dbtNumber: FF.FieldLength := iLen;
  end;
  FF.IndexName := idxnme;
  FF.IndexSort := iSorting;
  FF.IndexType := iUnique;
end;

procedure TjbDBF.Fatal(const Msg: string);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Fatal
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: const Msg: string
  Result:    None
  Purpose:   Fatal error messenger
-------------------------------------------------------------------------------}
begin
  {kdyz je vnejsi zpracovani msg, tak ho zavolej, jinak ukaz vlastni}
  {outside messages showing}
  if AsSigned(FOnError) then FOnError(Self, msg)
  else
    {inside messages}
    MessageDlg(msg, mtError, [mbOk], 0);
end;

procedure TjbDBF.Warn(const Msg: string);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Warn
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: const Msg: string
  Result:    None
  Purpose:   Warnings messenger
-------------------------------------------------------------------------------}
begin
  {s varovanim je to stejne tak}
  {inside/outside warning message}
  if AsSigned(FOnWarn) then FOnWarn(Self, msg)
  else
    MessageDlg(msg, mtWarning, [mbOk], 0);
end;

function TjbDBF.UpdateIndexes(R: LongInt): Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.UpdateIndexes
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: R: LongInt
  Result:    Boolean
  Purpose:   Update index on position
-------------------------------------------------------------------------------}
var
  I, J, L, IdxRecSize: LongInt;
  TempFName: string;
  F: file;
  T: TStringList;
  IdxBuff, X, S: string;
  UpdateField: Boolean;
begin
  {indikace uspesneho  ukonceni}
  {all OK}
  Result := True;
  {indexy se aktualizuji zde, ale jen kdyz to chces}
  {can you actualise the index ?}
  if not FDBFStoreByIndex then Exit;
  {doslo-li k uspesnemu updatu, funkce vrati True jinak udela removeindexes}
  {if NOT OK data update then remove indexes}
  for I := 0 to FDBFFields.Count - 1 do
  begin
    {prochazis vsechny sloupce a hledas indexovy soubor}
    {go through collumns and search index file}
    if Trim(FDBFFields[I].IndexName) <> '' then
    begin
      {indexovy soubor byl nalezen}
      {jeho jmeno je ve tvaru xxxxxxxxIDX, vzdy zarovnan vpravo}
      {Note: size is depend by actual indexed column see FieldLength}
      IdxRecSize := FDBFFields[I].FieldLength + SizeOf(L);
      SetLength(IdxBuff, IdxRecSize);
      FillChar(IdxBuff[1], IdxRecSize, ' ');
      {vyrob temp jmeno indexoveho souboru}
      {found, format index filename}
      TempFName := ExtractFilePath(FDBFName) + Trim(string(FDBFFields[I].IndexName));
      if FileExists(TempFName) then
      begin
        AsSignFile(F, TempFName);
        try
          ReSet(F, 1);
          try
            UpdateField := False;
            {proc nepouzit k indexum stringlist?}
            {why don't use stringlist?}
            T := TStringList.Create;
            try
              {budes tridit radky}
              {will be assort lines}
              T.Sorted := True;
              {nastavujes vlastnost duplicit}
              {and property duplicit by type of index}
              case FDBFFields[I].IndexType of
                dbfUnique: T.Duplicates := dupError;
                dbfDuplicates: T.Duplicates := dupAccept;
              end;
              {indexes must be move}
              while not System.Eof(F) do
              begin
                {uprava indexu je zde}
                {adapt index here}
                BlockRead(F, IdxBuff[1], FDBFFields[I].FieldLength + SizeOf(L));
                Move(IdxBuff[1], L, SizeOf(L));
                {neni tam nahodou uz nektery k uprave?}
                {if adapted for change?}
                if R = L then
                begin
                  InternalLoad(FDBFFields[I].FieldName, X);
                  Move(X[1], IdxBuff[5], Length(X)); {zkus ho tam pridat}
                  UpdateField := True; {signalized, no new record}
                end;
                try
                  T.Add(' ' + Copy(IdxBuff, 5, 255) + #1 + IntToStr(L));
                except
                  on EListError do
                    if T.Duplicates = dupError then
                    begin
                      Warn(msgDuplicateInUnique);
                      Result := False;
                      Exit;
                    end;
                end;
              end {while};
              {vlozil jsi vsechny ze souboru, tak ted zkus primy}
              {all added, try direct}
              if not UpdateField then
              begin {get field from buffer}
                InternalLoad(FDBFFields[I].FieldName, X);
                try
                  T.Add(' ' + X + #1 + IntToStr(R));
                except
                  on EListError do
                    if T.Duplicates = dupError then
                    begin
                      Warn(msgDuplicateInUnique);
                      Result := False;
                      Exit;
                    end;
                end;
              end;
              {byl-li index uspesne vlozen, uloz indexovy soubor}
              {when index is OK, save as file}
              ReWrite(F, 1); {vymaz puvodni/recreate index file}
              case FDBFFields[I].IndexSort of
                dbfAscending:
                  begin
                    for J := 0 to T.Count - 1 do
                    begin
                      IdxBuff := T.Strings[J];
                      L := StrToInt(Copy(IdxBuff, Pos(#1, IdxBuff) + 1, 255));
                      S := Copy(IdxBuff, 1, Pos(#1, IdxBuff) - 1);
                      Move(S[1], IdxBuff[5], FDBFFields[I].FieldLength);
                      Move(L, IdxBuff[1], SizeOf(L));
                      BlockWrite(F, IdxBuff[1], IdxRecSize);
                    end;
                  end;
                dbfDescending:
                  begin
                    for J := T.Count - 1 downto 0 do
                    begin
                      IdxBuff := T.Strings[J];
                      L := StrToInt(Copy(IdxBuff, Pos(#1, IdxBuff) + 1, 255));
                      S := Copy(IdxBuff, 1, Pos(#1, IdxBuff) - 1);
                      Move(S[1], IdxBuff[5], FDBFFields[I].FieldLength);
                      Move(L, IdxBuff[1], SizeOf(L));
                      BlockWrite(F, IdxBuff[1], IdxRecSize);
                    end;
                  end;
                dbfAlternative:
                  begin
                    {Potrebujete-li to, tak jedine doprogramovat}
                    {if you want you have to coplete do it}
                    if AsSigned(FDBFOnAltSort) then FDBFOnAltSort(Self, T);
                    {a uloz to ...}
                    {and save it...}
                    for J := 0 to T.Count - 1 do
                    begin
                      IdxBuff := T.Strings[J];
                      L := StrToInt(Copy(IdxBuff, Pos(#1, IdxBuff) + 1, 255));
                      S := Copy(IdxBuff, 1, Pos(#1, IdxBuff) - 1);
                      Move(S[1], IdxBuff[5], FDBFFields[I].FieldLength);
                      Move(L, IdxBuff[1], SizeOf(L));
                      BlockWrite(F, IdxBuff[1], IdxRecSize);
                    end;
                  end;
              end;
            finally
              T.Free {zahod ho} {throw off}
            end;
          finally
            CloseFile(F); {close index file}
          end;
        except
          Result := False; {any error}
          Fatal(Format(msgIdxTableNotFound, [ExtractFileName(TempFName)]));
        end;
      end
      else
      begin
        AsSignFile(F, TempFName);
        {tabulka jeste neexistuje}
        {table doesn't exist}
        ReWrite(F, 1);
        InternalLoad(FDBFFields[I].FieldName, X);
        Move(R, IdxBuff[1], SizeOf(R));
        Move(X[1], IdxBuff[5], Length(X)); {try it add there}
        BlockWrite(F, IdxBuff[1], IdxRecSize);
        CloseFile(F);
      end;
    end;
  end;
end;

procedure TjbDBF.RemoveIndexes(R: LongInt);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.RemoveIndexes
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: R: LongInt
  Result:    None
  Purpose:   Remove index by position
-------------------------------------------------------------------------------}
var
  I, L: LongInt;
  TempFName, IdxBuff: AnsiString;
  F, Fnew: file;
begin
  {je-li nejaky zaznam vymazan, musi se tez odstranit ze vsech indexu}
  {when is some record deleted, have to delete from all indexes too}
  //for I := 1 to FDBFCountItems do
  for I := 0 to FDBFFields.Count - 1 do
  begin
    {prochazis vsechny sloupce a hledas indexovy soubor}
    {for through columns}
    if Trim(FDBFFields[I].IndexName) <> '' then
    begin
      {indexovy soubor byl nalezen}
      {jeho jmeno je obvykle stejne jako jmeno pole, zarovnano vpravo}
      {vyplne jsou SpacerD}
      SetLength(IdxBuff, FDBFFields[I].FieldLength + SizeOf(L));
      {index name for index file is the same as field name left padded}
      {found, format name}
      TempFName := AnsiString(ExtractFilePath(FDBFName)) + FDBFFields[I].IndexName;
      AsSignFile(F, string(TempFName));
      try
        ReSet(F, 1);
        try
          {create temporary file}
          AsSignFile(Fnew, ChangeFileExt(string(TempFName), '.$$$'));
          try
            ReWrite(Fnew, 1);
            try
              while not System.Eof(f) do
              begin
                BlockRead(F, IdxBuff[1], FDBFFields[I].FieldLength + SizeOf(L));
                Move(IdxBuff[1], L, SizeOf(L));
                if L <> R then
                begin
                  BlockWrite(Fnew, IdxBuff[1], FDBFFields[I].FieldLength + SizeOf(L))
                end;
              end;
            finally
              CloseFile(Fnew);
            end;
          except
            Warn(msgNotEnoughtCreateIdx)
          end;
        finally
          CloseFile(F);
          if FileExists(ChangeFileExt(string(TempFName), '.$$$')) then
          begin
            DeleteFile(string(TempFName));
            RenameFile(ChangeFileExt(string(TempFName), '.$$$'), string(TempFName))
          end;
        end;
      except
        Fatal(Format(msgIdxTableNotFound, [ExtractFileName(TempFName)]));
      end;
    end;
  end;
end;

procedure TjbDBF.MakeIndex(const IdxName: string; const Key: TKey;
  const IdxType: TFieldReq = dbfDuplicates; const idxsorting: TSortByIndex = dbfAscending);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.MakeIndex
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: const IdxName: string; const Key: TKey; const IdxType: TFieldReq = dbfDuplicates; const idxsorting: TSortByIndex = dbfAscending
  Result:    None
  Purpose:   Make index file for specific name of column
-------------------------------------------------------------------------------}
var
  F: file;
  I, L: LongInt;
  A, B, FLD: Integer;
  OK: Boolean;
  IdxBuff: AnsiString;
  s: string;
  tmpStrLst: TStringList;
begin
  {Musi existovat a byt otevrena neprazdna tabulka, lze pouzit i pro preindexovani}
  {Unempty table have to exists (and should be use for reindexing too)}
  if not (FDBFExist and FDBFIsOpened and (FDBFHeader.NumRecs > 0)) then Exit;
  {Otevri ji na stejnem miste, pouzij idxname = cele jmeno souboru}
  {Open it here, use idxname = full file name}
  AsSignFile(F, IdxName);
  try
    Rewrite(F, 1);
    try
      B := 0; A := -1;
      {do teto velikosti to lze setridit pres stringlist jinak per partes}
      {there is limit of stringlist for 16 bit Delphi}
      {$IFDEF VER80}
      if FDBFHeader.NumRecs < MaxItems then
      begin
      {$ENDIF}
        tmpStrLst := TStringList.Create;
        try
          {budes tridit radky}
          {lines sorting allways}
          tmpStrLst.Sorted := True;
          //for FLD := 1 to FDBFCountItems do
          OK := False;
          for FLD := 0 to FDBFFields.Count - 1 do {column search}
            if Trim(FDBFFields[FLD].Field.name) = Key then
            begin
              OK := True;
              Break;
            end;
          if not OK then Exit; {nothing do, column name does not exist}
          {index file is created as new}
          FDBFFields[FLD].IndexName := AnsiString(ExtractFileName(string(IdxName)));
          FDBFFields[FLD].IndexType := IdxType;
          FDBFFields[FLD].IndexSort := idxsorting;
          {nastavujes vlastnost duplicit}
          {property duplicates}
          case FDBFFields[FLD].IndexType of
            dbfUnique: tmpStrLst.Duplicates := dupError; {when is unique name expected}
            dbfDuplicates: tmpStrLst.Duplicates := dupAccept;
          end;
          {projdes celou tabulku a vytahnes z ni pozadovane pole}
          {go through table}
          for I := 0 to FDBFHeader.NumRecs - 1 do
          begin
            Seek(I); {seek to new position}

            InternalLoad(Key, s); {load the key}
            IdxBuff := AnsiString(s);
            {vlozis ho i s pozici do seznamu}
            {with position}
            try
              tmpStrLst.Add(' ' + string(IdxBuff) + #1 + IntToStr(I));
            except
              on EListError do
                if tmpStrLst.Duplicates = dupError then Fatal(msgDuplicateInUnique);
            end;
            {aktualizujes citac pro meridlo - vhodne je tez nastavovat kurzor}
            {counter actualisation for gauge}
            B := Round((I + 1) / (FDBFHeader.NumRecs / 100));
            if A <> B then
            begin {tohle je proto, aby se progress volal jen 101x}
              A := B;
              if AsSigned(FOnProgress) then FOnProgress(Self, prgMakeIndexSort, B);
            end;
          end;
          {znovu projdes seznam, upravis ho do tvaru <cislo><klic> a zapises}
          {go through list again, format items as <number><key> an write it}
          if AsSigned(FOnProgress) then FOnProgress(Self, prgWriteIndexSort, B);
          case FDBFFields[FLD].IndexSort of
            dbfDescending: {sestupne}
              for I := tmpStrLst.Count - 1 downto 0 do
              begin
                IdxBuff := AnsiString(tmpStrLst.Strings[I]);
                L := StrToInt(Copy(string(IdxBuff), Pos(#1, string(IdxBuff)) + 1, 255));
                IdxBuff := AnsiString('   ' + Copy(string(IdxBuff), 1, Pos(#1, string(IdxBuff)) - 1));
                Move(L, IdxBuff[1], SizeOf(L));
                BlockWrite(F, IdxBuff[1], Length(IdxBuff));
              end;
            dbfAscending: {vzestupne}
              for I := 0 to tmpStrLst.Count - 1 do
              begin
                IdxBuff := AnsiString(tmpStrLst.Strings[I]);
                L := StrToInt(Copy(string(IdxBuff), Pos(#1, string(IdxBuff)) + 1, 255));
                {dopredu tri mezery, jedna tam uz je}
                {fill three space front of it}
                IdxBuff := AnsiString('   ' + Copy(string(IdxBuff), 1, Pos(#1, string(IdxBuff)) - 1));
                Move(L, IdxBuff[1], SizeOf(L));
                BlockWrite(F, IdxBuff[1], Length(IdxBuff));
              end;
            dbfAlternative: {proste jinac}
              begin
                {Potrebujete-li to, tak jedine doprogramovat}
                {if you want you have to coplete do it}
                if AsSigned(FDBFOnAltSort) then FDBFOnAltSort(Self, tmpStrLst);
                for I := 0 to tmpStrLst.Count - 1 do
                begin
                  IdxBuff := AnsiString(tmpStrLst.Strings[I]);
                  L := StrToInt(Copy(string(IdxBuff), Pos(#1, string(IdxBuff)) + 1, 255));
                  IdxBuff := AnsiString('   ' + Copy(string(IdxBuff), 1, Pos(#1, string(IdxBuff)) - 1));
                  Move(L, IdxBuff[1], SizeOf(L));
                  BlockWrite(F, IdxBuff[1], Length(IdxBuff));
                end;
              end;
          end;
        finally
          tmpStrLst.Free
        end;
      {$IFDEF VER80}
      end
      else
        Warn(msgFileIsTooLarge);
      {$ENDIF}
    finally
      CloseFile(F)
    end;
  except
    Fatal(msgNotEnoughtCreateIdx)
  end;
end;

function TjbDBF.ReIndex;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.ReIndex
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   reindexing of the table
             provede reindexovani tabulky
-------------------------------------------------------------------------------}
var
  I: Integer;
  TempFName: string;
  S: AnsiString;
begin
  Result := False;
  {je-li tabulka zrovna otevrena, tak nedelej nic}
  {when table is opened do nothing}
  if IsMarked then Exit;
  {nemuzes-li si ji otevrit taky pro sebe, tak taky nic nedelej}
  {if you cannot open for this (transaction?) do nothing}
  if not Cover then Exit;
  try
    //for I := 1 to FDBFCountItems do
    for I := 0 to FDBFFields.Count - 1 do
    begin
      {prochazis vsechny sloupce a hledas indexovy soubor}
      S := Trim(FDBFFields[I].IndexName);
      {go through comumns}
      if S <> '' then
      begin
        {indexovy soubor byl nalezen}
        {index file name found}
        TempFName := ExtractFilePath(FDBFName) + string(S);
        MakeIndex(TempFName, Trim(FDBFFields[I].FieldName))
      end;
    end;
  finally
    UnCover;
    Result := True;
  end;
end;

procedure TjbDBF.Update(R: LongInt);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Update
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: R: LongInt
  Result:    None
  Purpose:   Update is write to default position with refresh by seeking
-------------------------------------------------------------------------------}
begin
  {bude-li zaznam aktualizovan tesne pred update, dej to vedet i s flagem}
  {if actualized before update get message with flag}
  if AsSigned(FOnActualize) then FOnActualize(Self, dbfUpdate);
  {zapis zaznam z bufferu}
  {write record}
  Write(CurrRec);
  {udelej jeste obnoveni/refresh, ale asi neni uz nutne}
  {and refresh, it is not needet immediatelly}
  Seek(CurrRec);
end;

procedure TjbDBF.InternalSave(const Key: TKey; const Rec: string);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.InternalSave
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: const Key: TKey; const Rec: string
  Result:    None
  Purpose:   save value to actual record buffer by field key
             vlozi hodnotu do bufferu aktualniho zaznamu dle klice pole
-------------------------------------------------------------------------------}
var
  I, Posic, Siz: Integer;
  tmprec: {$IFDEF UNICODE}RawByteString{$ELSE}AnsiString{$ENDIF};
begin
  {pozice zacina od jedne, i kdyz je buffer od 0 protoze na }
  {pozici 0 je indikacni Byte o vymazani vety}
  {cout from 1, position 0 is flag for deleting}
  Posic := 2;
  //for I := 1 to FDBFCountItems do
  for I := 0 to FDBFFields.Count - 1 do
  begin
    if Trim(FDBFFields[I].FieldName) = UpperCase(Key) then Break
    else Inc(Posic, FDBFFields[I].FieldLength);
  end;
  tmprec := AnsiString(Rec);
  Siz := FDBFFields[I].FieldLength;
  if Length(tmprec) < Siz then
    case FDBFFields[I].FieldType of
      dbtString, dbtLogical: tmprec := RightPad(tmprec, Siz);
      dbtFloat, dbtNumber, dbtMemo: tmprec := LeftPad(tmprec, Siz);
      dbtDate: ; {date is 8 chars only ddmmyyy or mmddyyyy}
      dbtTime: ; {time is 6 chars only hhmmss}
    end;
  {$IFDEF UNICODE}
  SetCodePage(tmprec, CodePage2LangDriver(FLangCodePage), True);
  {$ENDIF}
  Move(tmprec[1], FDBFBuff[Posic], Siz);
end;

procedure TjbDBF.InternalLoad(const Key: TKey; var Rec: string);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.InternalLoad
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: const Key: TKey; var Rec: string
  Result:    None
  Purpose:   read value from actual record buffer by field key
             Precte hodnotu z bufferu aktualniho zaznamu dle klice pole
-------------------------------------------------------------------------------}
var
  I, Posic: Integer;
  tmprec: {$IFDEF UNICODE}RawByteString{$ELSE}AnsiString{$ENDIF};
begin
  {pozice zacina od jedne, i kdyz je buffer od 0 protoze na }
  {pozici 0 je indikacni Byte o vymazani vety}
  {cout from 1, position 0 is flag for deleting}
  Posic := 2;
  {nejprve musi najit jmeno pole a nascitat pocatek}
  {search field name and recount start of}
  for I := 0 to FDBFFields.Count - 1 do
  begin
    if Trim(FDBFFields[I].FieldName) = UpperCase(Trim(Key)) then Break
    else Inc(Posic, FDBFFields[I].FieldLength);
  end;
  {predej zaznam neotrimovany}
  {add unformating record}
  {$IFNDEF VER80}
  SetLength(tmprec, FDBFFields[I].FieldLength);
  {$ELSE}
  tmprec[0] := Chr(FDBFFields[I].FieldLength);
  {$ENDIF}
  Move(FDBFBuff[Posic], tmprec[1], FDBFFields[I].FieldLength);
  {$IFDEF UNICODE}
  SetCodePage(tmprec, FLangCodePage, false);
  {$ENDIF}
  Rec := string(tmprec);
end;

function TjbDBF.EOF: Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.EOF
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    Boolean
  Purpose:   cursor position indikated on last row of the table
             indikovan stav, je-li kurzor na konci tabulky
-------------------------------------------------------------------------------}
begin
  Result := FDBFCurrRec = (FDBFHeader.numRecs - 1);
end;

function TjbDBF.LangDriver2CodePage(LangDrv: Byte): Integer;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.First
  Author:    Jaro Benes
  DateTime:  2012.08.03
  Arguments: LangDrv: Byte
  Result:    Integer
  Purpose:   transcode codepage to langdrv header byte
             prekodovani kodove stranky do vlajky langdrv v zahlavi
-------------------------------------------------------------------------------}
begin
  case LangDrv of
    001: Result := 437;
    002: Result := 850;
    100: Result := 852;
    102: Result := 865;
    101: Result := 866;
    104: Result := 895;
    200: Result := 1250;
    201: Result := 1251;
    003: Result := 1252;
  else
    Result := 852 {when is not LangDrv actually set}
    //DefaultSystemCodePage;
  end;
end;

procedure TjbDBF.Last;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Last
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   cursor set up to last row of table
             kurzor ukazuje na posledni radek tabulky
-------------------------------------------------------------------------------}
begin
  GotoEnd;
end;

function TjbDBF.Load(const Key: TKey): string;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Load
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: const Key: TKey
  Result:    string
  Purpose:   this is formated version of ELoad only
             tohle je uzivatelska modifikace funkce ELoad, ktera orizne mezery
-------------------------------------------------------------------------------}
begin
  {vola standarni funkci}
  {call standard function}
  InternalLoad(Trim(Key), Result);
  {a tady jeste orizne nadbytecne mezery}
  {and trim spaces}
  Result := Trim(Result);
end;

function TjbDBF.Find(const Key: TKey; const  Value: string): Integer;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Find
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: const Key: TKey; const Value: string
  Result:    Integer
  Purpose:   Alias for Search() only with default position to TOP
-------------------------------------------------------------------------------}
begin
  Result := Search(Key, Value, 0); //alias only
end;

procedure TjbDBF.First;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.First
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   cursor set up to first row of table
             kurzor ukazuje na prvni radek tabulky
-------------------------------------------------------------------------------}
begin
  GotoStart;
end;

function TjbDBF.CodePage2LangDriver(CodePage: Integer): Byte;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.First
  Author:    Jaro Benes
  DateTime:  2012.08.03
  Arguments: CodePage: Integer
  Result:    Byte
  Purpose:   transcode langdrv header byte to actual code page
             prekodovani vlajky langdrv do kodove stranky
-------------------------------------------------------------------------------}
begin
  case CodePage of
    437: Result := 001;
    850: Result := 002;
    852: Result := 100;
    865: Result := 102;
    866: Result := 101;
    895: Result := 104;
    1250: Result := 200;
    1251: Result := 201;
    1252: Result := 003;
  else
    Result := 000;
  end;
end;

function TjbDBF.Cover: Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Cover
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    Boolean
  Purpose:   set flag for transaction
             nastavuje bit transakce
-------------------------------------------------------------------------------}
var
  F: file;
  B: Byte;
begin
  Result := False;
  if IsMarked then Exit;
  AssignFile(F, FDBFName);
  try
    Reset(F, 1);
    try
      B := 1;
      System.Seek(F, 14);
      BlockWrite(F, B, 1);
      Result := True;
    finally
      CloseFile(F);
    end;
  except
    on EInOutError do Warn(msgFileTooRemote);
  end;
end;

procedure TjbDBF.UnCover;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.UnCover
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   reset flag for transaction
             shazuje bit transakce
-------------------------------------------------------------------------------}
var
  F: file;
  B: Byte;
begin
  if not IsMarked then Exit;
  AssignFile(F, FDBFName);
  try
    Reset(F, 1);
    try
      B := 0;
      System.Seek(F, 14);
      BlockWrite(F, B, 1);
    finally
      CloseFile(F);
    end;
  except
    on EInOutError do Warn(msgFileTooRemote);
  end;
end;

procedure TjbDBF.RemoveIndex(const Name: string);
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.RemoveIndex
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: const Name: string
  Result:    None
  Purpose:   Explicit delete of index and erase link
             explicitni zruseni indexu .mdx a zruseni propojeni
-------------------------------------------------------------------------------}
var
  F: file;
  B: Byte;
begin
  AssignFile(F, FDBFName);
  try
    Reset(F, 1);
    try
      System.Seek(F, 28);
      B := 0;
      BlockWrite(F, B, 1);
      DeleteFile(ChangeFileExt(Name, '.mdx'));
    finally
      CloseFile(F);
    end;
  except
    on EInOutError do Warn(msgCannotOpenTable)
  end;
end;

function TjbDBF.IsMarked: Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.IsMarked
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    Boolean
  Purpose:   Get flag that transaction is of/off
             vraci priznak, zda je nastavena transakce
-------------------------------------------------------------------------------}
var
  F: file;
  B: Byte;
begin
  Result := True;
  AssignFile(F, FDBFName);
  try
    Reset(F, 1);
    try
      System.Seek(F, 14);
      BlockRead(F, B, 1);
      Result := B = 1;
    finally
      CloseFile(F);
    end;
  except
    on EInOutError do Warn(msgFileTooRemote);
  end;
end;

procedure TjbDBF.IncNumRec;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.IncNumRec
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   increment count of records +1
             procedura zvysi pocet zaznamu o jeden
-------------------------------------------------------------------------------}
var
  F: file;
  L: LongInt;
begin
  AssignFile(F, FDBFName);
  try
    Reset(F, 1);
    try
      System.Seek(F, 4);
      BlockRead(F, L, SizeOf(L));
      Inc(L);
      FDBFHeader.numRecs := L;
      System.Seek(F, 4);
      BlockWrite(F, L, SizeOf(L));
    finally
      CloseFile(F);
    end;
  except
    on EInOutError do Warn(msgFileTooRemote);
  end;
end;

procedure TjbDBF.PutSysDate;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.PutSysDate
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   write actual date and time to header of dbf
             zapis do hlavicky aktualni datum
-------------------------------------------------------------------------------}
var
  F: file;
  S: string[3];
  Year, Month, Day: Word;
begin
  AssignFile(F, FDBFName);
  try
    Reset(F, 1);
    try
      System.Seek(F, 1);
      BlockRead(F, S[1], 3);
      DecodeDate(Date, Year, Month, Day);
      Byte(S[1]) := (Year mod 100);
      Byte(S[2]) := Month;
      Byte(S[3]) := Day;
      Move(S[1], FDBFHeader.Year, 3);
      System.Seek(F, 1);
      BlockWrite(F, S[1], 3);
    finally
      CloseFile(F);
    end;
  except
    on EInOutError do Warn(msgFileTooRemote);
  end;
end;

function TjbDBF.GetLastNoOfMemo: Integer;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.GetLastNoOfMemo
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    Integer
  Purpose:   get last highest number of record of memo
             funkce ziska z memo souboru posledni nejvyssi cislo zaznamu
-------------------------------------------------------------------------------}
var
  F: file;
  T: TDBTTypes;
  Readed: {$IFDEF VER80}Word{$ELSE}Integer{$ENDIF};
begin
  Result := 0; {file is empty or not exists}
  if not FileExists(ChangeFileExt(FDBFName, '.DBT')) then Exit;
  AsSignFile(F, ChangeFileExt(FDBFName, '.DBT'));
  ReSet(F, 1);
  try
    Result := -1;
    {hleda nejvyssi cislo }
    {search highest number of records}
    while not System.Eof(F) do
    begin
      BlockRead(F, T, SizeOf(T), Readed);
      if Readed = 0 then Exit;
      System.Seek(F, System.FilePos(F) + T.SizeOfMemo);
      if T.NumberOf > Result then Result := T.NumberOf;
    end;
  finally
    CloseFile(F);
  end;
  Inc(Result);
end;

function TjbDBF.SaveMemo(No: LongInt; const FName: string): Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.SaveMemo
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: No: LongInt; const FName: string
  Result:    Boolean
  Purpose:   Save memo-file at position
             ulozi soubor do memo
-------------------------------------------------------------------------------}
var
  F, FF: file;
  T: TDBTTypes;
  S: string;
  SR: TSearchRec;
  A: array[1..1024] of AnsiChar;
  NumRead, NumWritten: {$IFDEF VER80}Word{$ELSE}Integer{$ENDIF};
begin
  Result := False;
  if not FileExists(FName) then Exit;
  if FindFirst(FName, faAnyFile, SR) <> 0 then Exit;
  FindClose(SR);
  if No = -1 then No := GetLastNoOfMemo; {search last number}
  AsSignFile(F, ChangeFileExt(FDBFName, '.DBT'));
  try
    {$I-}
    ReSet(F, 1);
    {$I+}
    if IoResult <> 0 then ReWrite(F, 1);
    {zapis vety}
    try
      System.Seek(F, System.FileSize(F));
      with T do
      begin
        { cislo zaznamu}
        {record no}
        NumberOf := No;
        S := ExtractFileExt(FName);
        if Length(S) <= 3 then
          Move(S[1], AsFileType, Length(S)); { extension of stored type}
        Used := True; { is used}
        SizeOfMemo := SR.Size; { size of linked file}
        FileDateTime := FileDateToDateTime(SR.Time); { original date and time of file}
      end;
      BlockWrite(F, T, SizeOf(T));
      {prekopiruj soubor do memo}
      {re-copy file to memo}
      AsSignFile(FF, FName);
      ReSet(FF, 1);
      try
        repeat
          BlockRead(FF, A, SizeOf(A), NumRead);
          BlockWrite(F, A, NumRead, NumWritten);
        until (NumRead = 0) or (NumWritten <> NumRead);
      finally
        CloseFile(FF)
      end;
      Result := True;
    finally
      CloseFile(F);
    end;
  except
    on EInOutError do Fatal(msgErrorOnMemoOpen);
  end;
end;

function TjbDBF.LoadMemo(No: LongInt; var FName: string): Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.LoadMemo
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: No: LongInt; var FName: string
  Result:    Boolean
  Purpose:   get filename from memo
             preda soubor z memo na disk do FName - zmeni u nej pouze extenzi dle uloz. typu
  Note:      Linux changes by * Andrea Russo *
-------------------------------------------------------------------------------}
var F, FF: file;
  T: TDBTTypes;
  Readed: {$IFDEF VER80}Word{$ELSE}Integer{$ENDIF};
  C: AnsiChar;
  I: LongInt;
  {$IFNDEF LINUX} { /AR/ }
  Handle: Integer;
  {$ENDIF}
begin
  Result := False;
  if not FileExists(ChangeFileExt(FDBFName, '.DBT')) then Exit;
  AsSignFile(F, ChangeFileExt(FDBFName, '.DBT'));
  try
    ReSet(F, 1);
    try
      T.NumberOf := -1;
      {hleda hlavickovy zaznam}
      {search header}
      while T.NumberOf <> No do begin
        BlockRead(F, T, SizeOf(T), Readed);
        if Readed = 0 then Exit;
        System.Seek(F, System.FilePos(F) + T.SizeOfMemo);
      end;
      {nasels, vrat se}
      {found, go back}
      System.Seek(F, System.FilePos(F) - T.SizeOfMemo);
      {zapis ho na disk - je-li jineho typu, zmen extenzi!! }
      {write to disk}
      {$IFDEF VER12UP}
      FName := ChangeFileExt(FName, '.' + string(T.AsFileType));
      {$ELSE}
      FName := ChangeFileExt(FName, '.' + T.AsFileType);
      {$ENDIF}
      AsSignFile(FF, FName);
      try
        ReWrite(FF, 1);
        try
          {prekopiruj to do souboru}
          {re-copy to file}
          for I := 1 to T.SizeOfMemo do
          begin
            BlockRead(F, C, SizeOf(C));
            BlockWrite(FF, C, SizeOf(C));
          end;
        finally
          CloseFile(FF);
          {nastav puvodni datum souboru}
          {set original date and time of file}
          {$IFDEF LINUX} { /AR/ }
          FileSetDate(FName, DateTimeToFileDate(T.FileDateTime));
          {$ELSE}
          Handle := FileOpen(FName, fmOpenReadWrite);
          FileSetDate(Handle, DateTimeToFileDate(T.FileDateTime));
          FileClose(Handle);
          {$ENDIF}
        end;
      except
        on EInOutError do ;
      end;
      {a zapis}
      {and write}
      BlockWrite(F, T, SizeOf(T));
    finally
      CloseFile(F);
    end;
    Result := True;
  except
    on EInOutError do Fatal(msgErrorOnMemoOpen);
  end;

end;

function TjbDBF.EraseMemo(No: LongInt): Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.EraseMemo
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: No: LongInt
  Result:    Boolean
  Purpose:   set mark in memo file as unused
             oznaci soubor v memo za nepouzivany
-------------------------------------------------------------------------------}
var
  F: file;
  T: TDBTTypes;
  Readed: {$IFDEF VER80}Word{$ELSE}Integer{$ENDIF};
begin
  Result := False;
  AsSignFile(F, ChangeFileExt(FDBFName, '.DBT'));
  try
    ReSet(F, 1);
    try
      T.NumberOf := -1;
      while T.NumberOf <> No do
      begin
        BlockRead(F, T, SizeOf(T), Readed);
        if Readed = 0 then Exit;
        System.Seek(F, System.FilePos(F) + T.SizeOfMemo);
      end;
      {nasels, vrat se}
      {find, go back}
      System.Seek(F, System.FilePos(F) - T.SizeOfMemo - SizeOf(T));
      {Nastav priznak}
      {set flag}
      T.Used := False;
      {a zapis}
      {and write it}
      BlockWrite(F, T, SizeOf(T));
    finally
      CloseFile(F);
    end;
    Result := True;
  except
    on EInOutError do Fatal(msgErrorOnMemoOpen);
  end;
end;

function TjbDBF.Locate(const KeyFields: string;
  const KeyValues: string; Options: TLocateOptionSet): Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Locate
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: const KeyFields: string; const KeyValues: string; Options: TLocateOptionSet
  Result:    Boolean
  Purpose:   Locate record for specific value by column
             special version locate for quick search records
-------------------------------------------------------------------------------}

  procedure GiveFields(const KeyFields: string; out ListFields: TStringList);
  begin
    ListFields := TStringList.Create;
    ListFields.Delimiter := ';';
    ListFields.DelimitedText := KeyFields;
  end;
  function EvaluateFields(const KeyValue, Value: string; Options: TLocateOptionSet): Boolean;
  begin
    Result := False;
    if Options = [] then
    begin
      if AnsiCompareText(KeyValue, Value) = 0 then
      begin
        Result := True;
        Exit
      end;
    end
    else
      if Options = [loCaseSensitive, loPartialKey] then
      begin
        if Pos(KeyValue, Value) > 0 then
        begin
          Result := True;
          Exit
        end;
      end
      else
        if Options = [loCaseSensitive] then
        begin
          if AnsiCompareStr(KeyValue, Value) = 0 then
          begin
            Result := True;
            Exit
          end;
        end
        else
          if Options = [loPartialKey] then
          begin
            if Pos(AnsiUpperCase(KeyValue), AnsiUpperCase(Value)) > 0 then
            begin
              Result := True;
              Exit
            end;
          end;
  end;

  function GetAsDateTimeValue(value: string): string;
  begin

  end;

  function EvalExpression(FLD_CLMN: TDBField; ExpressionValue: string;
    Options: TLocateOptionSet): Boolean;
  type
    TOPE = (toEQ, toNE, toLE, toGE, toLS, toGR);
  const
    OPE: array [TOPE] of string = ('=', '<>', '<=', '>=', '<', '>');
  var
    C: TOPE;
    StrExpressionValue: string;
    KeyValue: string;
  begin
    Result := False;
    StrExpressionValue := ExpressionValue;
    KeyValue := FLD_CLMN.AsString;
    for C := toEQ to toGR do
    begin
      if Pos(OPE[C], StrExpressionValue) = 1 then
      begin
        System.Delete(StrExpressionValue,1,Length(OPE[C]));
        try
          case C of
            toEQ: Break;
            toNE:
              case FLD_CLMN.FieldType of
                dbtMemo, dbtString{$IFDEF VERDB5UP}, dbtBinary{$ENDIF}:
                  Result := AnsiCompareText(KeyValue, StrExpressionValue) <> 0;
                dbtNumber, dbtLogical:
                  Result := StrToInt(KeyValue) <> StrToInt(StrExpressionValue);
                dbtFloat:
                  Result := StrToFloat(KeyValue) <> StrToFloat(StrExpressionValue);
                dbtDate, dbtTime:
                  Result := GetAsDateTimeValue(KeyValue) <> GetAsDateTimeValue(StrExpressionValue);
              end;
            toLE:
              case FLD_CLMN.FieldType of
                dbtMemo, dbtString{$IFDEF VERDB5UP}, dbtBinary{$ENDIF}:
                  Result := AnsiCompareText(KeyValue, StrExpressionValue) <= 0;
                dbtNumber, dbtLogical:
                  Result := StrToInt(KeyValue) <= StrToInt(StrExpressionValue);
                dbtFloat:
                  Result := StrToFloat(KeyValue) <= StrToFloat(StrExpressionValue);
                dbtDate, dbtTime:
                  Result := GetAsDateTimeValue(KeyValue) <= GetAsDateTimeValue(StrExpressionValue);
              end;
            toGE:
              case FLD_CLMN.FieldType of
                dbtMemo, dbtString{$IFDEF VERDB5UP}, dbtBinary{$ENDIF}:
                  Result := AnsiCompareText(KeyValue, StrExpressionValue) >= 0;
                dbtNumber, dbtLogical:
                  Result := StrToInt(KeyValue) >= StrToInt(StrExpressionValue);
                dbtFloat:
                  Result := StrToFloat(KeyValue) >= StrToFloat(StrExpressionValue);
                dbtDate, dbtTime:
                  Result := GetAsDateTimeValue(KeyValue) >= GetAsDateTimeValue(StrExpressionValue);
              end;
            toLS:
              case FLD_CLMN.FieldType of
                dbtMemo, dbtString{$IFDEF VERDB5UP}, dbtBinary{$ENDIF}:
                  Result := AnsiCompareText(KeyValue, StrExpressionValue) < 0;
                dbtNumber, dbtLogical:
                  Result := StrToInt(KeyValue) < StrToInt(StrExpressionValue);
                dbtFloat:
                  Result := StrToFloat(KeyValue) < StrToFloat(StrExpressionValue);
                dbtDate, dbtTime:
                  Result := GetAsDateTimeValue(KeyValue) < GetAsDateTimeValue(StrExpressionValue);
              end;
            toGR:
              case FLD_CLMN.FieldType of
                dbtMemo, dbtString{$IFDEF VERDB5UP}, dbtBinary{$ENDIF}:
                  Result := AnsiCompareText(KeyValue, StrExpressionValue) > 0;
                dbtNumber, dbtLogical:
                  Result := StrToInt(KeyValue) > StrToInt(StrExpressionValue);
                dbtFloat:
                  Result := StrToFloat(KeyValue) > StrToFloat(StrExpressionValue);
                dbtDate, dbtTime:
                  Result := GetAsDateTimeValue(KeyValue) > GetAsDateTimeValue(StrExpressionValue);
              end;
          end;
        except
          Break;
        end;
        Exit;
      end;
    end;
    Result := EvaluateFields(KeyValue, StrExpressionValue, Options)
  end;
var
  I, J: Integer;
  OK: Boolean;
  ListFields: TStringList;
  ListParams: TStringList;
  OKARR: array of Boolean;
  //=, <>, <=, >=, <, >
  //&&, ||
  clmnName: AnsiString;
begin
  Result := False; {nothing found}
  GiveFields(KeyFields, ListFields); {get params}
  GiveFields(KeyValues, ListParams); {get check values}
  SetLength(OKARR, ListFields.Count); {checked array}
  try
    {if not added loNextLocate then set to top of table, else search next from cursor}
    if not (loNextLocate in Options) then First;
    while not EOF do
    begin {sequentially}
      for J := Low(OKARR) to High(OKARR) do OKARR[J] := False; {smazat! clear flags!}
      for I := 0 to ListFields.Count - 1 do
      begin {go trough collumns}
        clmnName := AnsiString(ListFields[I]); {get name of column}
        if FieldByName[clmnName].IsNull then {when is null it true allways}
          OKARR[I] := True
        else  {check by expression}
          OKARR[I] := EvalExpression(FieldByName[clmnName], ListParams[I], Options);
      end;
      {sloupce musi souhlasit}
      OK := True; {flag set up}
      for J := Low(OKARR) to High(OKARR) do {check all elements}
        if not OKARR[J] then
        begin {when is one from false reset flag}
          OK := False;
          Break; {waiting for new cycle}
        end;
      if OK then
      begin {if OK only}
        Result := True; {return true}
        Exit; {}
      end;
      Next {next row}
    end;
  finally
    ListFields.Free; {free memory}
    ListParams.Free;
  end;
end;

function TjbDBF.Explore(Key: TKey): TDBField;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Explore
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: Key: TKey
  Result:    TDBField
  Purpose:   Explore column by key (column name) - and return represented object
-------------------------------------------------------------------------------}
var
  FLD: Integer;
begin
  Result := nil;
  {table must exists and muste be opened and header must be declared}
  if not (FDBFExist and FDBFIsOpened and (FDBFHeader.NumRecs > 0)) then Exit;
  {go through header and check key name}
  FLD := FDBFFields.IndexOf(Key); {column search}
  if FLD <> -1 then
    if AnsiCompareText(Trim(FDBFFields[FLD].FieldName), Key) = 0 then
      Result := FDBFFields[FLD];
  {nothing do, column name does not exist}
end;

function TjbDBF.PackDBF: Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.PackDBF
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    Boolean
  Purpose:   odstrani z tabulky vymazane zaznamy
             remove deleted records from table
  Note:      create a temporary file named [filename]_temp_pack.dbf and when
             finish renamed it ** added by Andrea Russo **
-------------------------------------------------------------------------------}
var
  DBFNew: TjbDBF;
  iCol: integer;
  sFileNew: string;
  sField: AnsiString;
  bConfirm: Boolean;
  QQ: TDBFieldRec;
begin
  Result := False;
  bConfirm := True;
  DBFNew := TjbDBF.Create(Self);
  try
    if AsSigned(FOnErase) then FOnErase(Self, ExtractFileName(FDBFName), bConfirm);

    if not (bConfirm) then Exit;

    FileName := FDBFName;
    sFileNew := Copy(FDBFName, 1, Length(FDBFName) - Length(ExtractFileExt(FDBFName))) + '_temp_pack.dbf';

    SysUtils.DeleteFile(sFileNew);

    try
      if Open then
      begin
        GotoStart;

        for iCol := 0 to FDBFFields.Count - 1 do
        begin
          QQ := Fields[iCol].Field;
          with Fields[iCol] do
          begin
            DBFNew.MakeField(iCol, FieldName, FieldType, FieldLength, Places, IndexName, IndexType, IndexSort);
          end;
        end;
        DBFNew.CreateDB(sFileNew);

        DBFNew.FileName := sFileNew;
        if DBFNew.Open then
          while CurrRec < RecordsCount do
          begin
            if not (CurrentRecDeleted) then
            begin
              for iCol := 1 to FieldsCount do
              begin
                sField := Fields[iCol].FieldName;
                DBFNew.InternalSave(sField, Load(sField));
              end;
              DBFNew.InternalAppend;
            end;
            GotoNext;
          end;
        if FDBFIsOpened then
          Close;

        if DBFNew.FDBFIsOpened then
        begin
          DBFNew.Close;
          if SysUtils.FileExists(sFileNew) then
          begin
            SysUtils.DeleteFile(FDBFName);
            SysUtils.RenameFile(sFileNew, FDBFName);
            Result := True;
            if AsSigned(FOnErased) then FOnErased(Self);
          end;
        end;
      end;
    finally
      if FDBFIsOpened then
        Close;
      if DBFNew.FileIsOpen then
        DBFNew.Close;

      SysUtils.DeleteFile(sFileNew);
      ReIndex;
    end;
  finally
    DBFNew.Free;
  end;
end;

procedure TjbDBF.PackDBT;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.PackDBT
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   odstrani z memo nepouzite zaznamy
             remove deleted records from memo
-------------------------------------------------------------------------------}
begin
end;

procedure TjbDBF.Post;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Post
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   Alias procedure for better using
             Similar like as BDE.POST
-------------------------------------------------------------------------------}
begin
  {prave zmeneny zaznam je aktualizovan}
  {currently changed record is actualized, stored}
  if FDBFCurrRec = -1 then InternalAppend
  else
    Update(FDBFCurrRec);
end;

procedure TjbDBF.Prior;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.Prior
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
  Purpose:   Alias procedure for better using
-------------------------------------------------------------------------------}
begin
  {postup o radek k vrsku tabulky}
  {go to top table by one row}
  GotoPrev;
end;

function TjbDBF.GetRecordsCount: LongInt;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.GetRecordsCount
  Author:    Andrea Russo
  DateTime:  2009.07.22
  Arguments: None
  Result:    LongInt
  Purpose:   vraci pocet zaznamu
             get count of records
-------------------------------------------------------------------------------}
begin
  Result := FDBFHeader.numRecs;
end;

function TjbDBF.GetVersion: string;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.GetVersion
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    string
  Purpose:   Check flag placed in header for target DB engine version
-------------------------------------------------------------------------------}
begin
  if not FDBFIsOpened then Exit;
  case FDBFHeader.version of
    $02: Result := 'FoxBASE';
    $03: Result := 'FoxBASE+/Dbase III plus, no memo';
    $30: Result := 'Visual FoxPro';
    $31: Result := 'Visual FoxPro, autoincrement enabled';
    $32: Result := 'Visual FoxPro with field type Varchar or Varbinary';
    $43: Result := 'dBASE IV SQL table files, no memo';
    $63: Result := 'dBASE IV SQL system files, no memo';
    $83: Result := 'FoxBASE+/dBASE III PLUS, with memo';
    $8B: Result := 'dBASE IV with memo';
    $CB: Result := 'dBASE IV SQL table files, with memo';
    $F5: Result := 'FoxPro 2.x (or earlier) with memo';
    $E5: Result := 'HiPer-Six format with SMT memo file';
    $FB: Result := 'FoxBASE';
  else
    Result := ''
  end;
end;

function TjbDBF.GetActive: Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.GetActive
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    Boolean
  Purpose:   Check when si DBF opened or not only
-------------------------------------------------------------------------------}
begin
  Result := FDBFReadOnly or not FDBFExist or not FDBFIsOpened;
end;

function TjbDBF.GetField(Index: Integer): TDBField;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.GetField
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: Index: Integer
  Result:    TDBField
  Purpose:   Return field by index number of field definition
-------------------------------------------------------------------------------}
begin
  Result := nil;
  if (Index < 0) or (Index > FDBFCountItems) then
    Fatal(Format(msgFieldNotFound, [Index]))
  else
    Result := FDBFFields[Index];
end;

function TjbDBF.IsCurrentRecDeleted: Boolean;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.IsCurrentRecDeleted
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    Boolean
  Purpose:   Set up flag that records is deleted
             Rekne zda-li je zaznam vymazan
-------------------------------------------------------------------------------}
begin
  Result := FDBFBuff[1] = '*';
end;

function TjbDBF.GetFieldByName(const Key: TKey): TDBField;
{-------------------------------------------------------------------------------
  Procedure: TjbDBF.GetFieldByName
  Author:    Andrea Russo
  Modified:  Jaro Benes
  DateTime:  2009.07.22
  Arguments: const Key: TKey
  Result:    TDBField
  Purpose:   Proclamation through field item
-------------------------------------------------------------------------------}
var
  bFound: Boolean;
  i, c: integer;
begin
  Result := nil;
  bFound := False;
  i := 0;

  while not (bFound or (i > (FDBFCountItems))) do
  begin
    inc(i);
    c := FDBFFields.IndexOf(Key);
    if c <> -1 then
    begin
      bFound := True;
      Result := FDBFFields[c];
    end;
  end;
  if not (bFound) then
    Fatal(msgBadDefinitionOfField);
end;

{ datetime helper }

function DateTimeStrEval(const DateTimeFormat : string;
                         const DateTimeStr : string) : TDateTime;
// DATE-TIME convert helper
// ============================================================================= 
// Evaluate a date time string into a TDateTime obeying the 
// rules of the specified DateTimeFormat string
// eg. DateTimeStrEval('dd-MMM-yyyy hh:nn','23-May-2002 12:34) 
// 
// NOTE : One assumption I have to make that DAYS,MONTHS,HOURS and 
//        MINUTES have a leading ZERO or SPACE (ie. are 2 chars long)
//        and MILLISECONDS are 3 chars long (ZERO or SPACE padded) 
//
// Supports DateTimeFormat Specifiers 
// 
// dd    the day as a number with a leading zero or space (01-31). 
// ddd the day as an abbreviation (Sun-Sat)
// dddd the day as a full name (Sunday-Saturday)
// mm    the month as a number with a leading zero or space (01-12). 
// mmm the month as an abbreviation (Jan-Dec) 
// mmmm the month as a full name (January-December) 
// yy    the year as a two-digit number (00-99). 
// yyyy the year as a four-digit number (0000-9999).
// hh    the hour with a leading zero or space (00-23) 
// nn    the minute with a leading zero or space (00-59). 
// ss    the second with a leading zero or space (00-59). 
// zzz the millisecond with a leading zero (000-999). 
// ampm  Specifies am or pm flag hours (0..12) 
// ap    Specifies a or p flag hours (0..12)
// 
// 
// Delphi 6 Specific in DateUtils can be translated to .... 
//
// YearOf() 
//
// function YearOf(const AValue: TDateTime): Word; 
// var LMonth, LDay : word; 
// begin
//   DecodeDate(AValue,Result,LMonth,LDay); 
// end; 
//
// TryEncodeDateTime()
//
// function TryEncodeDateTime(const AYear,AMonth,ADay,AHour,AMinute,ASecond,
//                            AMilliSecond : word; 
//                            out AValue : TDateTime): Boolean;
// var LTime : TDateTime; 
// begin
//   Result := TryEncodeDate(AYear, AMonth, ADay, AValue); 
//   if Result then begin
//     Result := TryEncodeTime(AHour, AMinute, ASecond, AMilliSecond, LTime); 
//     if Result then
//       AValue := AValue + LTime; 
//   end;
// end;
//
// =============================================================================

var
  I, ii, iii: Integer;
  Retvar: TDateTime;
  Tmp, Fmt, data, Mask, Spec: string;
  year, month, day, Hour, Minute, Second, MSec: Word;
  AmPm: Integer;
begin
  year := 1;
  month := 1;
  day := 1;
  Hour := 0;
  Minute := 0;
  Second := 0;
  MSec := 0;
  Fmt := UpperCase(DateTimeFormat);
  data := UpperCase(DateTimeStr);
  I := 1;
  Mask := '';
  AmPm := 0;

  while I < Length(Fmt) do
  begin
{$IFDEF UNICODE}
    if CharInSet(Fmt[I], ['A', 'P', 'D', 'M', 'Y', 'H', 'N', 'S', 'Z']) then
{$ELSE}
    if Fmt[I] in ['A', 'P', 'D', 'M', 'Y', 'H', 'N', 'S', 'Z'] then
{$ENDIF}
    begin
      // Start of a date specifier
      Mask := Fmt[I];
      ii := I + 1;

      // Keep going till not valid specifier
      while True do
      begin
        if ii > Length(Fmt) then
          Break; // End of specifier string
        Spec := Mask + Fmt[ii];

        if (Spec = 'DD') or (Spec = 'DDD') or (Spec = 'DDDD') or (Spec = 'MM')
          or (Spec = 'MMM') or (Spec = 'MMMM') or (Spec = 'YY') or
          (Spec = 'YYY') or (Spec = 'YYYY') or (Spec = 'HH') or (Spec = 'NN') or
          (Spec = 'SS') or (Spec = 'ZZ') or (Spec = 'ZZZ') or (Spec = 'AP') or
          (Spec = 'AM') or (Spec = 'AMP') or (Spec = 'AMPM')
        then
        begin
          Mask := Spec;
          Inc(ii);
        end
        else
        begin
          // End of or Invalid specifier
          Break;
        end;
      end;

      // Got a valid specifier ? - evaluate it from data string
      if (Mask <> '') and (Length(data) > 0) then
      begin
        // Day 1..31
        if (Mask = 'DD') then
        begin
          day := StrToIntDef(Trim(Copy(data, 1, 2)), 0);
          Delete(data, 1, 2);
        end;

        // Day Sun..Sat (Just remove from data string)
        if Mask = 'DDD' then
          Delete(data, 1, 3);

        // Day Sunday..Saturday (Just remove from data string LEN)
        if Mask = 'DDDD' then
        begin
          Tmp := Copy(data, 1, 3);
          for iii := 1 to 7 do
          begin
            if Tmp = UpperCase(Copy({$IFDEF VER15UP}FormatSettings.{$ENDIF}LongDayNames[iii], 1, 3)) then
            begin
              Delete(data, 1, Length({$IFDEF VER15UP}FormatSettings.{$ENDIF}LongDayNames[iii]));
              Break;
            end;
          end;
        end;

        // Month 1..12
        if (Mask = 'MM') then
        begin
          month := StrToIntDef(Trim(Copy(data, 1, 2)), 0);
          Delete(data, 1, 2);
        end;

        // Month Jan..Dec
        if Mask = 'MMM' then
        begin
          Tmp := Copy(data, 1, 3);
          for iii := 1 to 12 do
          begin
            if Tmp = UpperCase(Copy({$IFDEF VER15UP}FormatSettings.{$ENDIF}LongMonthNames[iii], 1, 3)) then
            begin
              month := iii;
              Delete(data, 1, 3);
              Break;
            end;
          end;
        end;

        // Month January..December
        if Mask = 'MMMM' then
        begin
          Tmp := Copy(data, 1, 3);
          for iii := 1 to 12 do
          begin
            if Tmp = UpperCase(Copy({$IFDEF VER15UP}FormatSettings.{$ENDIF}LongMonthNames[iii], 1, 3)) then
            begin
              month := iii;
              Delete(data, 1, Length({$IFDEF VER15UP}FormatSettings.{$ENDIF}LongMonthNames[iii]));
              Break;
            end;
          end;
        end;

        // Year 2 Digit
        if Mask = 'YY' then
        begin
          year := StrToIntDef(Copy(data, 1, 2), 0);
          Delete(data, 1, 2);
          if year < {$IFDEF VER15UP}FormatSettings.{$ENDIF}TwoDigitYearCenturyWindow then
            year := (YearOf(Date) div 100) * 100 + year
          else
            year := (YearOf(Date) div 100 - 1) * 100 + year;
        end;

        // Year 4 Digit
        if Mask = 'YYYY' then
        begin
          year := StrToIntDef(Copy(data, 1, 4), 0);
          Delete(data, 1, 4);
        end;

        // Hours
        if Mask = 'HH' then
        begin
          Hour := StrToIntDef(Trim(Copy(data, 1, 2)), 0);
          Delete(data, 1, 2);
        end;

        // Minutes
        if Mask = 'NN' then
        begin
          Minute := StrToIntDef(Trim(Copy(data, 1, 2)), 0);
          Delete(data, 1, 2);
        end;

        // Seconds
        if Mask = 'SS' then
        begin
          Second := StrToIntDef(Trim(Copy(data, 1, 2)), 0);
          Delete(data, 1, 2);
        end;

        // Milliseconds
        if (Mask = 'ZZ') or (Mask = 'ZZZ') then
        begin
          MSec := StrToIntDef(Trim(Copy(data, 1, 3)), 0);
          Delete(data, 1, 3);
        end;

        // AmPm A or P flag
        if (Mask = 'AP') then
        begin
          if data[1] = 'A' then
            AmPm := -1
          else
            AmPm := 1;
          Delete(data, 1, 1);
        end;

        // AmPm AM or PM flag
        if (Mask = 'AM') or (Mask = 'AMP') or (Mask = 'AMPM') then
        begin
          if Copy(data, 1, 2) = 'AM' then
            AmPm := -1
          else
            AmPm := 1;
          Delete(data, 1, 2);
        end;

        Mask := '';
        I := ii;
      end;
    end
    else
    begin
      // Remove delimiter from data string
      if Length(data) > 1 then
        Delete(data, 1, 1);
      Inc(I);
    end;
  end;

  if AmPm = 1 then
    Hour := Hour + 12;
  if not TryEncodeDateTime(year, month, day, Hour, Minute, Second, MSec, Retvar) then
    Retvar := 0.0;
  Result := Retvar;
end;

{  TDBField  }

constructor TDBField.Create(Collection: TCollection);
{-------------------------------------------------------------------------------
  Procedure: TDBField.Create
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: Collection: TCollection
  Result:    None
-------------------------------------------------------------------------------}
begin
  inherited Create(Collection);
  FOwner := Collection;
  FillChar(FDBFieldRec, SizeOf(FDBFieldRec), 0);
  FDisplayName := '';
end;

destructor TDBField.Destroy;
{-------------------------------------------------------------------------------
  Procedure: TDBField.Destroy
  Author:    Jaro Benes
  DateTime:  2009.07.22
  Arguments: None
  Result:    None
-------------------------------------------------------------------------------}
begin
  inherited;
end;

function TDBField.EndingString(prm: string): Boolean;
var
  S: string;
begin
  Result := False;
  if prm = '' then Exit;
  S := AsString;
  if S = '' then Exit;
  if AnsiCompareText(prm, Copy(S, Length(S) - Length(prm),  Length(prm))) = 0 then
    Result := True
end;

function TDBField.GetAsBoolean: Boolean;
begin
  Result := AnsiUpperCase(TjbDBF(FOwner.Owner).Load(FDBFieldRec.name)) = ccTrue;
end;

function TDBField.GetAsCurrency: Currency;
begin
  Result := StrToFloat(TjbDBF(FOwner.Owner).Load(FDBFieldRec.name));
end;

function TDBField.GetAsDate: TDateTime;
begin
  Result := 0;
  if IsNotNull then
    if FDBFieldRec.what = 'D' then
      Result := DateTimeStrEval('ddmmyyyy',TjbDBF(FOwner.Owner).Load(FDBFieldRec.name));
end;

function TDBField.GetAsFloat: Double;
var
  S: string;
begin
  Result := 0;
  if IsNotNull then
    if FDBFieldRec.what = 'F' then
    begin
      S := TjbDBF(FOwner.Owner).Load(FDBFieldRec.name);
      if FDBFieldRec.places > 0 then
        Insert({$IFDEF VER15UP}FormatSettings.{$ENDIF}DecimalSeparator, S, Length(S) - FDBFieldRec.places);
      Result := StrToFloat(S);
    end;
end;

function TDBField.GetAsInteger: Longint;
begin
  Result := 0;
  if IsNotNull then
    try
      Result := StrToInt(TjbDBF(FOwner.Owner).Load(FDBFieldRec.name))
    except
      Result := 0;
    end;
end;

function TDBField.GetAsMemo: string;
begin
  Result := '';
  if IsNotNull then
    case FDBFieldRec.what of
      'M', 'B':
      begin
        Result := TjbDBF(FOwner.Owner).Load(FDBFieldRec.name); //get number of file
        TjbDBF(FOwner.Owner).LoadMemo(StrToInt(Result), Result); //give filename
      end;
    end;
end;

function TDBField.GetAsString: string;
begin
  Result := TjbDBF(FOwner.Owner).Load(FDBFieldRec.name);
end;

function TDBField.GetAsTime: TDateTime;
begin
  Result := 0;
  if IsNotNull then
    if FDBFieldRec.what = 'T' then
      Result := DateTimeStrEval('hhnnss',TjbDBF(FOwner.Owner).Load(FDBFieldRec.name));
end;

function TDBField.GetDataFieldIdent: Byte;
begin
  Result := FDBFieldRec.dfIdent;
end;

function TDBField.GetDisplayName: string;
begin
  if FDisplayName = '' then
    Result := inherited GetDisplayName
  else
    Result := FDisplayName;
end;

function TDBField.GetFieldLength: Byte;
begin
  Result := FDBFieldRec.len;
end;

function TDBField.GetFieldName: AnsiString;
begin
  Result := Trim(FDBFieldRec.name);
end;

function TDBField.GetFieldType: TDBAsTypes;
begin
  for Result := Low(TDBAsTypes) to High(TDBAsTypes) do
    if ccDBTypes[Result] = FDBFieldRec.what then
      Exit;
  Result := dbtString;
end;

function TDBField.GetIndexName: AnsiString;
begin
  Result := '';
  if Trim(FDBFieldRec.idx) <> '' then
    Result := Trim(FDBFieldRec.idx) + '.IDX';
end;

function TDBField.GetIndexSort: TSortByIndex;
begin
  Result := FDBFieldRec.idxsrt;
end;

function TDBField.GetIndexType: TFieldReq;
begin
  Result := FDBFieldRec.idxtyp;
end;

function TDBField.GetPlaces: Byte;
begin
  Result := FDBFieldRec.places;
end;

function TDBField.GetTag: LongInt;
begin
  Result := FDBFieldRec.data;
end;

function TDBField.IsEqual(Value: TDBField): Boolean;
begin
  Result := (AnsiCompareText(FieldName, Value.FieldName) = 0)
    and (AnsiCompareText(AsString, Value.AsString) = 0)
end;

function TDBField.IsNotNull: Boolean;
var
  S: string;
begin
  S := TjbDBF(FOwner.Owner).Load(FDBFieldRec.name);
  case FDBFieldRec.what of
    'D': Result := (S <> '') and (S <> '00000000');
  else
    Result := S <> '';
  end;
end;

procedure TDBField.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TDBField) then
    with Source as TDBField do
    begin
      Self.FDBFieldRec := FDBFieldRec;
      Self.FDisplayName := FDisplayName;
    end
  else
    inherited;
end;

function TDBField.BeginString(prm: string): Boolean;
var
  S: string;
begin
  Result := False;
  if prm = '' then Exit;
  S := AsString;
  if S = '' then Exit;

  if AnsiCompareText(prm, Copy(S, 1,  Length(prm))) = 0 then
    Result := True
end;

procedure TDBField.Clear;
begin
  FillChar(FDBFieldRec, SizeOf(FDBFieldRec), 0);
end;

function TDBField.ContainString(prm: string): Boolean;
var
  S: string;
begin
  Result := False;
  if prm = '' then Exit;
  S := AsString;
  if S = '' then Exit;
  if AnsiPos(prm, S) > 0 then
    Result := True
end;

function TDBField.IsNull: Boolean;
var
  S: string;
begin
  S := TjbDBF(FOwner.Owner).Load(FDBFieldRec.name);
  case FDBFieldRec.what of
    'D': Result := (S = '') or (S = '00000000');
  else
    Result := S = '';
  end;
end;

procedure TDBField.SetAsBoolean(const Value: Boolean);
begin
  if FDBFieldRec.what = 'L' then
    if Value then
      TjbDBF(FOwner.Owner).InternalSave(FDBFieldRec.name, ccTrue)
    else
      TjbDBF(FOwner.Owner).InternalSave(FDBFieldRec.name, ccFalse);
end;

procedure TDBField.SetAsCurrency(const Value: Currency);
begin
  if FDBFieldRec.what in ['F', 'N', 'C'] then
    TjbDBF(FOwner.Owner).InternalSave(FDBFieldRec.name, Format('%'+IntToStr(FDBFieldRec.len)+'.' + IntToStr(FDBFieldRec.places) + 'f', [Value]));
end;

procedure TDBField.SetAsDate(const Value: TDateTime);
begin
  if FDBFieldRec.what in ['D', 'C'] then
    TjbDBF(FOwner.Owner).InternalSave(FDBFieldRec.name, SysUtils.FormatDateTime('yyyymmdd',Value));
end;

procedure TDBField.SetAsFloat(const Value: Double);
begin
  if FDBFieldRec.what in ['F', 'C'] then
    TjbDBF(FOwner.Owner).InternalSave(FDBFieldRec.name, Format('%'+IntToStr(FDBFieldRec.len)+'.' + IntToStr(FDBFieldRec.places) + 'f', [Value]));
end;

procedure TDBField.SetAsInteger(const Value: Integer);
begin
  case FDBFieldRec.what of
    'F': TjbDBF(FOwner.Owner).InternalSave(FDBFieldRec.name, Format('%'+IntToStr(FDBFieldRec.len)+'.' + IntToStr(FDBFieldRec.places) + 'f', [Value]));
    'N', 'C': TjbDBF(FOwner.Owner).InternalSave(FDBFieldRec.name, Format('%'+IntToStr(FDBFieldRec.len)+'.d', [Value]));
  end;
end;

procedure TDBField.SetAsMemo(const Value: string);
var
  No: Integer;
begin
  if FDBFieldRec.what = 'M' then
  begin
    No := TjbDBF(FOwner.Owner).GetLastNoOfMemo;
    TjbDBF(FOwner.Owner).SaveMemo(No, Value);
    TjbDBF(FOwner.Owner).InternalSave(FDBFieldRec.name, IntToStr(No));
  end;
end;

procedure TDBField.SetAsString(const Value: string);
begin
  TjbDBF(FOwner.Owner).InternalSave(FDBFieldRec.name, Value);
end;

procedure TDBField.SetAsTime(const Value: TDateTime);
begin
  if FDBFieldRec.what in ['T', 'C'] then
    TjbDBF(FOwner.Owner).InternalSave(FDBFieldRec.name, SysUtils.FormatDateTime('hhnnss',Value));
end;

procedure TDBField.SetDataFieldIdent(const Value: Byte);
begin
  FDBFieldRec.dfIdent := Value;
end;

procedure TDBField.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TDBField.SetFieldLength(const Value: Byte);
begin
  FDBFieldRec.len := Value;
end;

procedure TDBField.SetFieldName(const Value: AnsiString);
var
  fname, fldname: AnsiString;
  I: Integer;
begin
  //FDBFieldRec.name := Value;
  FillChar(FDBFieldRec.name, SizeOf(FDBFieldRec.name), SpacerD);
  {set name of index without .name just file name only}
  fldname := Trim(AnsiString(Value));
  if (fldname <> '') then
  begin
    I := Pos('.', string(fldname)); {cut the extension}
    if I > 0 then
    begin
      fname := Trim(Copy(fldname, 1, I - 1)); {copy it}
    end
    else
    begin
      fname := Copy(Trim(fldname), 1, SizeOf(FDBFieldRec.name)); {all is name}
    end;
    I := SizeOf(FDBFieldRec.name);
    if Length(fname) < I then
      while Length(fname) < I do
        fname := SpacerD + fname;
    fname := UpperCase(fname);
    Move(fname[1], FDBFieldRec.name, SizeOf(FDBFieldRec.name));
  end;
end;

procedure TDBField.SetFieldType(const Value: TDBAsTypes);
begin
  FDBFieldRec.what := ccDBTypes[Value];
end;

procedure TDBField.SetIndexName(const Value: AnsiString);
var
  fname, idxname: AnsiString;
  I: Integer;
begin
  FillChar(FDBFieldRec.idx, SizeOf(FDBFieldRec.idx), SpacerD);
  {set name of index without .IDX just file name only}
  idxname := Trim(AnsiString(Value));
  if (idxname <> '') then
  begin
    I := Pos('.', string(idxname)); {cut the extension}
    if I > 0 then
    begin
      fname := Trim(Copy(idxname, 1, I - 1)); {copy it}
    end
    else
    begin
      fname := Copy(Trim(idxname), 1, SizeOf(FDBFieldRec.idx)); {all is name}
    end;
    I := SizeOf(FDBFieldRec.idx);
    if Length(fname) < I then
      while Length(fname) < I do
        fname := SpacerD + fname;
    fname := UpperCase(fname);
    Move(fname[1], FDBFieldRec.idx, 11);
  end;
end;

procedure TDBField.SetIndexSort(const Value: TSortByIndex);
begin
  FDBFieldRec.idxsrt := Value;
end;

procedure TDBField.SetIndexType(const Value: TFieldReq);
begin
  FDBFieldRec.idxtyp := Value;
end;

procedure TDBField.SetPlaces(const Value: Byte);
begin
  FDBFieldRec.places := Value;
end;

procedure TDBField.SetTag(const Value: LongInt);
begin
  FDBFieldRec.data := Value;
end;

{  TDBTTypes  }
{$IFDEF VER10UP}
procedure TDBTTypes.Clear;
begin
  FillChar(Self, SizeOf(Self), 0);
end;
{$ENDIF}

{  TDBFields  }

function TDBFields.Add: TDBField;
begin
  Result := TDBField(inherited Add);
end;

constructor TDBFields.Create{$IFDEF VER4UP}(AOwner: TPersistent){$ENDIF};
begin
  inherited Create({$IFDEF VER4UP}AOwner, {$ENDIF}TDBField);
  {$IFDEF VER4UP}FOwner := AOwner;{$ENDIF}
end;

function TDBFields.CreateField(const FieldName: AnsiString): TDBField;
begin
  Result := Add;
  Result.FieldName := FieldName;
end;

function TDBFields.FindByName(const Value: AnsiString): TDBField;
var I: Integer;
begin
  Result := nil;
  I := IndexOf(Value);
  if I <> -1 then
    Result := Items[I];
end;

function TDBFields.GetFieldValue(const FieldName: AnsiString): TDBField;
begin
  Result := FindByName(FieldName);
end;

function TDBFields.GetItem(Index: Integer): TDBField;
begin
  try
    Result := TDBField(inherited Items[Index]);
  except
    raise Exception.Create('No item found');
  end;
end;

function TDBFields.IndexOf(const AName: AnsiString): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(TDBField(Items[Result]).FieldName, AName) = 0 then Exit;
  Result := -1;
end;

function TDBFields.IsEqual(Value: TDBFields): Boolean;
var
  I: Integer;
begin
  Result := Count = Value.Count;
  if Result then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].IsEqual(Value.Items[I]);
      if not Result then Break;
    end;
end;

procedure TDBFields.RemoveField(Value: TDBField);
begin
  if Value.Collection = Self then
    Value.Collection := nil;
end;

procedure TDBFields.SetFieldValue(const FieldName: AnsiString;
  const Value: TDBField);
begin
  FindByName(FieldName).Field := Value.Field;
end;

procedure TDBFields.SetItem(Index: Integer; const Value: TDBField);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

{end of file; end of comments 20.7.2009 by JB.}
end. 