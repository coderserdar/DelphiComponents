{$IFDEF VER80}
{$X+}
{$ELSE}
{$X+,H-}
{$ENDIF}
unit jbDbf;
{$I jb.inc}

{founded 1999 (c) Jaro Benes}
{works with DBF table by direct file}
{for version Delphi 1..7, 2005, 2006/Turbo, 2007}
{history:}
{  13.11.2006 Fix bug on PackDBF;}
{  06.09.2004 added DBF Packing function PackDBF and deleted the empty procedure PruneDBF;}
{ renamed PruneDBT to PackDBT; on TjbDBF.Close Actualization only if file is not readonly}
{ by Andrea Russo /AR/ [mailto:andrusso@yahoo.com]}
{  18.12.2003 changes and Kylix compatibility by Andrea Russo /AR/ [mailto:andrusso@yahoo.com]}
{  20.8.2002 changes and Italian messages by Andrea Russo /AR/ [mailto:andrusso@yahoo.com]}
{  18.04.2001 bugfix changes by Jarda Jirava [<mailto:listuj@centrum.cz>]}
{  30.10.2001 bugfix and extended by Vyacheslav Nebroev /VN/ [<mailto:vasu@amts.smolensk.ru>]}
{  15.01.2002 all comments convert from Czech to English}
interface

{$IFDEF LINUX} { /AR/ }
uses SysUtils, Classes, QDialogs;
{$ELSE}
uses Windows, SysUtils, DateUtils, Classes, Dialogs;
{$ENDIF}

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
  TDBFMakeItems = procedure(Sender: TObject; Posit: Integer;
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

  TRecArray = array[0..MaxSize] of AnsiChar;
  PRecArray = ^TRecArray;
  PBigArray = PRecArray;

  TDBFHeader = record        { legends are from manual, changed for me specific}
    version: byte;            { Should be 3 or $83                           1 }
                              { $3  - FoxBase+/dBase III Plus bez souboru MEMO }
                              {     - FoxPro/dBase IV bez souboru memo
                              { $83 - FoxBase+/dBase III Plus se souborem MEMO }
                              { $F5 - FoxPRo se souborem memo                  }
                              { $8B - dBase IV se souborem memo                }
    year, month, day: byte;   { Date of last update                          3 }
    numRecs: longint;         { Number of records in the file                4 }
    headLen: word;            { Length of the header                         2 }
    recLen: word;             { Length of one data record, incl.delete flag  2 }
    nets: word;               { not used                                       }
    transaction: byte;        { begin-end transaction                          }
                              { 00 - no transaction protected                  }
                              { 01 - transaction protected                     }
    encrypted: byte;          { coded fields                                   }
                              { 00 - uncrypted                                 }
                              { 01 - encrypted                                 }
    network: array[1..12] of byte;
    mdxfile: byte;            { exist .mdx file indicator                      }
                              { 00 - non exist                                 }
                              { 01 - exist and join                            }
                              { NOTE: other interpretation is here             }
                              {Table flags:
                                $01   file has a structural .cdx
                                $02   file has a Memo field
                                $04   file is a database (.dbc)
                                This byte can contain the sum of any of the
                                above values. For example, the value $03
                                indicates the table has a structural .cdx
                                and a Memo field.}
    LangDrv: byte;            { language driver /fox/                          }
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
  end;

const
  ccTrue = 'T';
  ccFalse = 'F';

  TdbTypes: set of AnsiChar = [
    'C' {characters, all ascii},
    'D' {date ddmmyyyy, fix size 8}, //default is yyyymmdd
    'T' {time hhmmss, fix size 6}, //it is possible as DBF format extension
    'F' {float point, -,.,0..9, right padded, left spaced},
    'L' {logical, fix size 1 byte, initial with space ,T,t,F,f},
    'M' {memo, as numeric, fix size 10, right padded number as point to .DBT},
    'N' {numeric, -,.,0..9}
    {$IFDEF VERDB5UP} ,
    'B' {binary, as numeric, fix size 10, right padded number as point to .DBT},
    '@' {timestamp - 8 bytes - two longs, first for date, second for time.  The date is the number of days since  01/01/4713 BC. Time is hours * 3600000L + minutes * 60000L + Seconds * 1000L},
    'I' {long, fixed 4 bytes as longword with sign},
    '+' {autoincrement like as long},
    'O' {double, fixed 8 byte, stored as double without any conversion}{$ENDIF} ];
  {struktura zazn. zapisniku, pripojene soubory predava pres disk}
  {structure of memo appended file on disk}
type

  TDBRowsType = (rtString, rtDate, rtTime, rtFloat, rtLogical, rtMemo, rtNumeric);

  {  TDBTTypes  }

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
  TIDXTypes=Record
    ItemNo:LongInt;                   // refer to record in table
    Key:TDBField.Len;                 // key component
  End;
  }

  TjbDBF = class;

  {  TDBField  }

  TDBField = packed record
    name: TCharrs11;           { Name of the field                     11 }
    what: AnsiChar;            { Type of data in this field             1 }
    data: LongInt;       { Not used or Displacement of field in record  4 }
    len: byte;                 { Length of the field                    1 }
    places: byte;              { Number of decimal places               1 }
    idxtyp: TFieldReq;         { typ klice unikatni/duplicitni...       1 }
                               { NOTE: other values interpretation        }
                               {  Field flags:
                                  $01   System Column (not visible to user)
                                  $02   Column can store null values
                                  $04   Binary column (for CHAR and MEMO only)
                                  $06   ($02+$04) When a field is NULL and binary (Integer, Currency, and Character/Memo fields)
                                  $0C   Column is autoincrementing        }
    idxsrt: TSortByIndex;      { setridit vzestupne, sestupne, custom...1 }
    dfIdent: Byte;             { datafield identifier ??                1 }
    idx: TCharrs11;            { here file name in index  XXXXXXXXIDX  11 }
  {$IFDEF VER10UP}
  private
    FDBF: TjbDBF;
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
  public
    procedure Clear;
    function IsNull: Boolean;
    function IsNotNull: Boolean;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property AsMemo: string read GetAsMemo write SetAsMemo;
  {$ENDIF}
  end;

  {  TKey  }

  TKey = string;

  TLocateOption = (loCaseSensitive, loPartialKey, loNextLocate);
  TLocateOptionSet = set of TLocateOption;
{----------------------------------------------------------------------------}

  {  TjbDBF  }

  TjbDBF = class(TComponent)
  private
    FDBFName: string;                                      { Full name table }
    FDBFIsOpened: Boolean;                        { TRUE when is file opened }
    FDBFStoreByIndex: Boolean;                         { Store by list index }
    FDBFHandle: file;                                { Handle of actual file }
    FDBFExist: Boolean;         { Indicate file exists when is name assigned }
    FDBFReadOnly: Boolean;                                       { Read only }
    FDBFSaveOnClose: Boolean;                                { Save on close }
    FDBFHeader: TDBFHeader;                     { Header,  filled after open }
    FDBFIndex: string;                               { Actual index for FIND }
    FDBFPassword: string;                           { Administrator password }
    FDBFFilter: string;                                            { not used}
    FDBFIndexList: TStringList;                  { List all indexes of table }
    FDBFBuff: PBigArray;                          { Temp FDBFBuff for record }
    FDBFCurrRec: LongInt;                  { Cursor position point to record }
    FDBFCountItems: Integer;                      { Count of recors collumns }
    FOnError: TDBFError;                    { Event for error administration }
    FOnWarn: TDBFError;                  { Event for warnings administration }
    FOnMakeFields: TDBFMakeItems;              { For create fields in record }
    FOnErase: TDBFBeforeConfirm;             { For confirm with pack of .DBF }
    FOnOverwrite: TDBFBeforeConfirm;{ For confirm with overwrite of.DBF .IDX }
    FOnAdded: TNotifyEvent;                                { If record added }
    FDBFOnAltSort: TDBFAltSort;             { Alternative sort on stringlist }
    FOnChange: TDBFChange;                             { If record in change }
    FOnChanged: TNotifyEvent;                         { If record is changed }
    FOnDelete: TDBFConfirm;
    FOnActualize: TDBFActualize;
    FOnDeleted: TNotifyEvent;                         { If record is deleted }
    FOnPassword: TDBFPassword;     { If administrator request password check }
    FOnOpened: TDBFOpened;                              { If table is opened }
    FOnAsSigned: TDBFAsSigned;                             { If table attach }
    FOnFound: TNotifyEvent;                       { If found record by index }
    FOnErased: TNotifyEvent;                            { If table is packed }
    FOnNavigate: TDBFNavigate;                     { If navigation is called }
    FOnProgress: TDBFProgress;  { If table id updated, show percent on gauge }
    FOnUpdate: TNotifyEvent;                       { If record is actualized }
    FOnClosed: TNotifyEvent;                            { If table is closed }
    FOnLoaded: TNotifyEvent;             { If record attach to buffer memory }
    FOnQuery: TDBFQuery;                     { For query with find statement }

    function GetField(Index: Integer): TDBField; { /VN/ }
    function GetFieldByName(const Key: TKey): TDBField; { /AR/ }
    function GetPassword: string;
    function GetRecordsCount: LongInt; { return the records count /VN/ }
    function IsCurrentRecDeleted: Boolean; { /VN/ }
    procedure SetFileName(name: string);
    procedure SetPassword(const thepassword: string);
  protected
    FDBFFields: array[1..maxFields] of TDBField; { The field data }
    procedure Fatal(const Msg: string);
    procedure Warn(const Msg: string);
    procedure Actualization;
  public
    constructor Create(AOWner: TComponent); override;
    destructor Destroy; override;
    function GetVersion: string;
    procedure Close; virtual;
    function Open: Boolean; virtual;
    function Write(r: longint): TStatusWrite; virtual;
    procedure Seek(r: longint); virtual;
    procedure NewRecord; virtual;
    procedure GotoStart;
    procedure GotoEnd;
    procedure GotoNext;
    procedure GotoPrev;
    function Delete(R: longint): TStatusWrite; virtual;
    function UpdateIndexes(R: LongInt): Boolean; virtual;
    procedure RemoveIndexes(R: LongInt); virtual;
    procedure MakeIndex(const IdxName: string; const Key: TKey); {make index} virtual;
    procedure Find(const IdxName, Value: string); {search value by key} virtual;
    procedure Store(const Key: TKey; const Rec: string); {field of record} virtual;
    procedure ELoad(const Key: TKey; var Rec: string); {with conversion} virtual;
    function Load(const Key: TKey): string;
    procedure Update(R: LongInt); virtual;
    procedure CreateDB(const fname: string; rL {reclen}, numFields: word); virtual;
    function MakeField(posit: Byte; const iname: string; iwhat: AnsiChar; ilen: byte;
      iplaces: byte; const idxnme: string;
      Req: TFieldReq; Sort: TSortByIndex): Boolean; virtual;
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
    procedure PackDBT; virtual;
    property CurrRec: LongInt read FDBFCurrRec;
    property RecordsCount: LongInt read GetRecordsCount; { /VN/ }
    property FieldsCount: Integer read FDBFCountItems; { /VN/ }
    property Fields[Index: Integer]: TDBField read GetField; { /VN/ }
    property CurrentRecDeleted: Boolean read IsCurrentRecDeleted; { changed /VN/ }
    property FieldByName[const Key: TKey]: TDBField read GetFieldByName; default;{ /AR/ }
  published
    property FileIsOpen: Boolean read FDBFIsOpened;
    property StoreByIndex: Boolean read FDBFStoreByIndex write FDBFStoreByIndex;
    property FileIsExist: Boolean read FDBFExist;
    property ReadOnly: Boolean read FDBFReadOnly write FDBFReadOnly;
    property SaveOnClose: Boolean read FDBFSaveOnClose write FDBFSaveOnClose;
    property ByIndex: string read FDBFIndex write FDBFIndex;
    property Password: string read GetPassword write SetPassword;
    property FileName: string read FDBFName write SetFileName;
    property OnError: TdbfError read FOnError write FOnError;
    property OnWarn: TdbfError read FOnWarn write FOnWarn;
    property OnMakeFields: TDBFMakeItems read FOnMakeFields write FOnMakeFields;
    property OnErase: TDBFBeforeConfirm read FOnErase write FOnErase;
    property OnOverwrite: TDBFBeforeConfirm read FOnOverwrite write FOnOverwrite;
    property OnAdded: TNotifyEvent read FOnAdded write FOnAdded;
    property OnAltSort: TDBFAltSort read FDBFOnAltSort write FDBFOnAltSort;
    property OnChange: TDBFChange read FOnChange write FOnChange;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnDelete: TDBFConfirm read FOnDelete write FOnDelete;
    property OnDeleted: TNotifyEvent read FOnDeleted write FOnDeleted;
    property OnPassword: TDBFPassword read FOnPassword write FOnPassword;
    property OnOpened: TDBFOpened read FOnOpened write FOnOpened;
    property OnAsSigned: TDBFAsSigned read FOnAsSigned write FOnAsSigned;
    property OnFound: TNotifyEvent read FOnFound write FOnFound;
    property OnErased: TNotifyEvent read FOnErased write FOnErased;
    property OnNavigate: TDBFNavigate read FOnNavigate write FOnNavigate;
    property OnProgress: TDBFProgress read FOnProgress write FOnProgress;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
    property OnClosed: TNotifyEvent read FOnClosed write FOnClosed;
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
    property OnActualize: TDBFActualize read FOnActualize write FOnActualize;
    property OnQuery: TDBFQuery read FOnQuery write FOnQuery;
  end;

procedure Register;

implementation

{----------------------------------------------------------------------------}
procedure Register;
{----------------------------------------------------------------------------}
begin
  RegisterComponents('Library', [TjbDBF]);
end;

{$IFDEF VER80}
{----------------------------------------------------------------------------}
function Trim(const S: string): string;
{----------------------------------------------------------------------------}
begin
  Result := S;
  while (Length(Result) > 0) and (Result[1] <= ' ') do
    System.Delete(Result, 1, 1);
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do
    System.Delete(Result, Length(Result), 1);
end;
{$ENDIF}

{----------------------------------------------------------------------------}
constructor TjbDBF.Create(AOwner: TComponent);
{----------------------------------------------------------------------------}
var
  I:Integer;
begin
  inherited Create(AOwner);
  FillChar(FDBFFields, SizeOf(FDBFFields), 0);
  {$IFDEF VER10UP}
  for I := 1 to MaxFields do
    FDBFFields[I].FDBF := Self;
  {$ENDIF}
  FDBFIsOpened := False;
  FileName := '';
  FDBFBuff := nil;
  FDBFIndexList := TStringList.Create;
  FDBFStoreByIndex := False;
  FDBFExist := False;
  FDBFReadOnly := False;
  FDBFSaveOnClose := False;
end;

{----------------------------------------------------------------------------}
destructor TjbDBF.Destroy;
{----------------------------------------------------------------------------}
begin
  {kdyby byla nahodou tabulka otevrena, tak ji explicitne uzavri}
  {when table was opened then explicit close it}
  if FDBFIsOpened then
    close;
  {uvolni instanci seznamu}
  {free list}
  FDBFIndexList.Free;
  inherited destroy;
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.SetFileName(name: string);
{----------------------------------------------------------------------------}
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

{----------------------------------------------------------------------------}
function TjbDBF.GetPassword: string;
{----------------------------------------------------------------------------}
begin
  Result := LowerCase(FDBFPassword)
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.SetPassword(const thepassword: string);
{----------------------------------------------------------------------------}
begin
  FDBFPassword := thepassword
end;

{----------------------------------------------------------------------------}
function TjbDBF.Open: Boolean;
{----------------------------------------------------------------------------}
var
  Temp: TDBField;
  Done: Boolean;
  Readed: {$IFDEF VER80}Word{$ELSE}Integer{$ENDIF};
  Pass: string;
begin
  Result := False;
  {kdyby nahodou byla tabulka otevrena, tak ji zavri}
  {sloce table}
  if FDBFIsOpened then Close;
  {neexistuje-li tabulka, nedelej nic}
  {when table not exists, do nothing}
  if not FDBFExist then Exit;

  done := FALSE;

  if AsSigned(FOnPassword) then FOnPassword(Self, Pass);
  {prikryti heslem je mozne overit tady}
  {cover by password validate here}
  if FDBFPassword <> '' then
    if Pass <> FDBFPassword then begin
      Fatal(msgNoPasswordMatch);
      Exit;
    end;
  {otevreni pres handle}
  {open through handle}
  AsSignFile(FDBFHandle, FDBFName);
  try
    Reset(FDBFHandle, 1);
    {kdyz se povedlo, zustane otevrena az do close}
    {if success, stay open to close}
    FDBFIsOpened := True;
    {vyzvedni si header}
    {get header}
    BlockRead(FDBFHandle, FDBFHeader, sizeof(TDBFHeader)); { Get the header }
    {tohle bude pracovni buffer, kam budes davat data}
    {working data buffer is here}
    GetMem(FDBFBuff, FDBFHeader.RecLen);
    {tady ctes polozky/sloupce a v tehle promenn vzdy budou}
    {reading field here}
    FDBFCountItems := 0;
    repeat
      {cti obezretne, co kdybys narazil na neocekacany konec}
      {read circumspection what about giv unexpected end of file }
      {$I-}
      BlockRead(FDBFHandle, temp, SizeOf(TDBField), Readed);
      {$I+}
      if Temp.name[0] <> #$0D then
      begin
        {ukazuj na prvni volny}
        {show first free}
        Inc(FDBFCountItems);
        FDBFFields[FDBFCountItems] := temp;
        {$IFDEF VER10UP}
        FDBFFields[FDBFCountItems].FDBF := Self;
        {$ENDIF}
        FillChar(temp, SizeOf(temp), 0);
      end
      else
      begin
        done := TRUE;
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

{----------------------------------------------------------------------------}
procedure TjbDBF.Append;
{----------------------------------------------------------------------------}
begin
  {funkce se vola po vlozeni hodnot do prazdneho bufferu}
  {function is called after store valuse into new clean buffer}
  NewRecord;
end;

{----------------------------------------------------------------------------}
function TjbDBF.BOF: Boolean;
{----------------------------------------------------------------------------}
begin
  {indikuje ze jsme na prvnim zaznamu nebo je tabulka prazdna}
  {first row indicated or empty table}
  Result := FDBFCurrRec = 0;
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.Close;
{----------------------------------------------------------------------------}
var B: Byte;
begin
  {nasleduje test EOF mark a oznaceni kdyz chybi}
  {tohle ale lze udelat v pripade, kdyz soubor neni read only}
  {je-li read only, zahlasi se chyba a nejde soubor opravit}
  {follow EOF mark test; if missing}
  {do it when isn't file read-only only}
  {if read only get error message and do not repair it}

  System.Seek(FDBFHandle, FileSize(FDBFHandle) - 1);
  Blockread(FDBFHandle, b, 1);
  if B <> $1A then begin
    if not FDBFReadOnly and not IsMarked then
    begin
        {je-li povoleno stouchni tam posledni zaznam}
        {when consented, poke last record there}
      if FDBFSaveOnClose then Write(CurrRec);
      B := $1A;
      BlockWrite(FDBFHandle, B, 1);
    end
    else
      Fatal(msgEOFmarkMiss)
  end;
  CloseFile(FDBFHandle);
  {date of actualization}
  if FileMode <> 0 then {[AR]}
    Actualization;
  FDBFIsOpened := False; {message - file closed}
  if AsSigned(FOnClosed) then FOnClosed(Self);
  FreeMem(FDBFBuff, FDBFHeader.RecLen); { free allocated buffer}
end;

{----------------------------------------------------------------------------}
function TjbDBF.Write(R: LongInt): TStatusWrite;
{hlavni funkce zapisu, data jsou vzdy ukladana na pozadani}
{main function for write, data store for request}
{----------------------------------------------------------------------------}
var Cancel: Boolean;
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
  if not IsMarked then begin
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
        if UpdateIndexes(R) then begin
          try
            {vyhledej fyzicky zaznam k prepisu}
            {search physical record for overwrite}
            System.Seek(FDBFHandle, R * FDBFHeader.recLen + FDBFHeader.headLen);
            {nastav signal pro platny zapis - zaznam je platny}
            {set prompt for true write -> record is OK}
            FDBFBuff^[0] := ' '; { Record not deleted! } {uncomment /AR/ }
            {zapis ho na vyhledane misto}
            {write it to found place}
            BlockWrite(FDBFHandle, FDBFBuff^, FDBFHeader.RecLen);

            Actualization;
            {teprve tady je vsechno OK}
            {only here is OK}
            Result := dbfOK;
          except
            on EInOutError do begin
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

{----------------------------------------------------------------------------}
function TjbDBF.Delete(R: longint): TStatusWrite;
{----------------------------------------------------------------------------}
var Confirm: Boolean;
begin
  Result := dbfError;
  {zapis muze byt proveden pouze v pripade ze neni jen pro cteni, existuje a je otevren}
  {write can be do only when isn't read-only or exists or is opened}
  if FDBFReadOnly or not FDBFExist or not FDBFIsOpened then Exit;
  Result := dbfBusy;
  if not IsMarked then begin
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
          FDBFBuff^[0] := DeleteFlag; { Record is deleted! }
          {zapis do souboru}
          {write it into file}
          BlockWrite(FDBFHandle, FDBFBuff^, FDBFHeader.recLen);
          {aktualizuj indexy, tj. odstran z nich vymazany zaznam}
          {and do index actualizing }
          RemoveIndexes(R);

          Actualization;
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

{----------------------------------------------------------------------------}
procedure TjbDBF.Seek(R: LongInt);
{----------------------------------------------------------------------------}
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
  BlockRead(FDBFHandle, FDBFBuff^, FDBFHeader.RecLen, Readed);
  {veta je uspesne nactena, jen kdyz je v bufferu cela}
  {when all readed}
  if FDBFHeader.RecLen = Readed then
    {a zahlas zes ji precetl}
    {get message}
    if AsSigned(FOnLoaded) then FOnLoaded(Self);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.GotoStart;
{----------------------------------------------------------------------------}
begin
  if not FDBFExist or not FDBFIsOpened then Exit;
  {nastav se na prvni zaznam}
  {seek to first}
  Seek(0);
  {zahlas pro navigaci, ze na nem stojis}
  {and get message for navigation your position}
  if AsSigned(FOnNavigate) then FOnNavigate(Self, 0);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.GotoEnd;
{----------------------------------------------------------------------------}
begin
  if not FDBFExist or not FDBFIsOpened then Exit;
  {nastav se na posledni zaznam}
  {seek to last}
  Seek(FDBFHeader.numRecs - 1);
  {zahlas pro navigaci, ze na nem stojis}
  {and get message for navigation your position}
  if AsSigned(FOnNavigate) then FOnNavigate(Self, FDBFHeader.numRecs - 1);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.GotoNext;
{----------------------------------------------------------------------------}
begin
  if not FDBFExist or not FDBFIsOpened then Exit;
  {nastav se na nasledujici zaznam}
  {seek to next}
  Seek(FDBFCurrRec + 1);
  {zahlas pro navigaci, ze na nem stojis}
  {and get message for navigation your position}
  if AsSigned(FOnNavigate) then FOnNavigate(Self, FDBFCurrRec + 1);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.GotoPrev;
{----------------------------------------------------------------------------}
begin
  if not FDBFExist or not FDBFIsOpened then Exit;
  {nastav se na predchazejici zaznam}
  {seek to previous}
  Seek(FDBFCurrRec - 1);
  {zahlas pro navigaci, ze na nem stojis}
  {and get message for navigation your position}
  if AsSigned(FOnNavigate) then FOnNavigate(Self, FDBFCurrRec - 1);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.NewRecord;
{----------------------------------------------------------------------------}
begin
  {nemuzes nic pridavat, kdyz je jen ke cteni, nebo neexistuje, neni otevren}
  {cannot do when is read-only or no exists or is closed}
  if FDBFReadonly or not FDBFExist or not FDBFIsOpened then Exit;
  if not IsMarked then begin
    Cover;
    try
      {vycisti buffer}
      {clear buffer}
      {FillChar(FDBFBuff^,FDBFHeader.RecLen,' ');}
      {zde je mozne udelat implicitni naplneni zaznamu, coz vrele doporucuji}
      {can do implicit fill of record (I recommend to)}
      if AsSigned(FOnActualize) then FOnActualize(Self, dbfNew);
      try
        {zvyz pocet zaznamu a uloz je do hlavicky}
        {increment count of records and save it}
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
begin
  GotoNext;
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.CreateDB(const fname: string; rL {reclen}, numFields: word);
{----------------------------------------------------------------------------}
var
  y, m, d: Word;
  c: AnsiChar;
  i, j: Byte;
begin
  {kdyby byla nahodou tabulka otevrena, tak ji explicitne uzavri}
  {close table}
  if FDBFIsOpened then
    Close;

  {vytvatis novou tabulku, zde je hlavicka}
  {for new table refill header}
  FillChar(FDBFHeader, SizeOf(FDBFHeader), 0);
  with FDBFHeader do begin
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
      for i := 1 to numFields do begin
        {jestlize je attachnuty event pro vyrobu pole on line tak ho zavolej}
        {jinak je predpokladano ze pred volanim teto metody byly
         vytvoreny sloupce pomoci MakeField()}
        {on line create field}
        {else before create was MakeField called}
        if AsSigned(FOnMakeFields) then begin
          {zavolej ho tolikrat, kolik je potreba vyrobit poli}
          {call by columns count}
          FillChar(FDBFFields[i], SizeOf(FDBFFields[i]), 0);
          {$IFDEF VER10UP}
          FDBFFields[I].FDBF := Self;
          {$ENDIF}
          {zavolej ho a vyrob zaznam}
          {make field here}
          with FDBFFields[I] do
            FOnMakeFields(Self, I, Name, What, Len, Places, idx, idxtyp, idxsrt);
          {uprav na velka pismena}
          {upper case only please}
          for j := 0 to 10 do FDBFFields[i].Name[j] := UpCase(FDBFFields[i].Name[j]);
          FDBFFields[i].What := UpCase(FDBFFields[i].What);
        end;
        {zapis nove vyrobene pole}
        {write new made field}
        BlockWrite(FDBFHandle, FDBFFields[i], sizeof(TDBField))
      end;
      {za hlavickou nasleduje vzdy CR}
      {over header poke CR mark}
      c := #$0D;
      BlockWrite(FDBFHandle, c, 1);
      {konec souboru je indikovan EOF mark}
      {end of file poke EOF mark}
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

{----------------------------------------------------------------------------}
function TjbDBF.MakeField(posit: Byte;
  const iname: string;
  iwhat: AnsiChar;
  ilen: byte;
  iplaces: byte;
  const idxnme: string; {filename xxxxxxxxIDX}
  Req: TFieldReq;
  Sort: TSortByIndex
  ): Boolean;
{----------------------------------------------------------------------------}
var
  I: byte;
  S: string;
  X: string[8];
begin
  Result := False;
  if (Trim(IName) = '') or not (UpCase(IWhat) in TdbTypes) or (ILen = 0) then begin
    Fatal(msgBadDefinitionOfField);
    Exit;
  end;
  Result := True;
  FillChar(FDBFFields[posit], SizeOf(FDBFFields[posit]), 0);
  {$IFDEF VER10UP}
  FDBFFields[posit].FDBF := Self;
  {$ENDIF}
  with FDBFFields[posit] do begin
    {prvnich 11 znaku, velka pismena}
    {first 11 chars, uppercase please}
    S := Copy(UpperCase(Trim(IName)), 1, 11);
    Move(S[1], Name, Length(S));
    What := UpCase(IWhat);
    {tyhle polozky (cas, datum, memo) maji fixni tvar}
    {format is fixed (time, date, memo...)}
    case What of
      'L': len := 1;
      'T': Len := 6;
      'D': Len := 8;
      'M': Len := 10;
      'F':
        begin
          Len := iLen; {bugfix by Jarda Jirava [<mailto:listuj@centrum.cz>]  18.4.2001}
          Places := IPlaces; {tohle je jenom pro float/float only}
        end;

    else
      Len := ILen;
    end;

    if (IdxNme <> '') then begin
      I := Pos('.', IdxNme);
      if I > 0 then begin
        S := Trim(Copy(IdxNme, I + 1, 3));
        if Length(S) < 3 then while Length(S) < 3 do S := S + SpacerD;
        X := Trim(Copy(IdxNme, 1, I - 1));
        if Length(X) < 8 then while Length(X) < 8 do X := SpacerD + X;
        S := X + S;
      end
      else begin
        X := Trim(IdxNme);
        if Length(X) < 8 then while Length(X) < 8 do X := SpacerD + X;
        S := X + 'IDX';
      end;
      S := UpperCase(S);
      Move(S[1], idx, 11);
      idxtyp := Req; { /AR/ }
      idxsrt := Sort; { /AR/ }
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.Fatal(const Msg: string);
{----------------------------------------------------------------------------}
begin
  {kdyz je vnejsi zpracovani msg, tak ho zavolej, jinak ukaz vlastni}
  {outside messages showing}
  if AsSigned(FOnError) then FOnError(Self, msg)
  else
    {inside messages}
    MessageDlg(msg, mtError, [mbOk], 0);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.Warn(const Msg: string);
{----------------------------------------------------------------------------}
begin
  {s varovanim je to stejne tak}
  {inside/outside warning message}
  if AsSigned(FOnWarn) then FOnWarn(Self, msg)
  else
    MessageDlg(msg, mtWarning, [mbOk], 0);
end;

{----------------------------------------------------------------------------}
function TjbDBF.UpdateIndexes(R: LongInt): Boolean;
{----------------------------------------------------------------------------}
var
  I, J, L: LongInt;
  TempFName: string;
  F: file;
  T: TStringList;
  S, X: string;
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
  for I := 1 to FDBFCountItems do begin
    {prochazis vsechny sloupce a hledas indexovy soubor}
    {go through collumns and search index file}
    if Trim(FDBFFields[I].Idx) <> '' then begin
      {indexovy soubor byl nalezen}
      {jeho jmeno je ve tvaru xxxxxxxxIDX, vzdy zarovnan vpravo}
      {vyplne jsou SpacerD tj. "~~~JMENOIDX" nebo "~~ZAMESTID~"}
      {vyrob temp jmeno souboru}
      {found, format index filename}
      TempFName := ExtractFilePath(FDBFName)
        + Trim(Copy(FDBFFields[I].Idx, 1, 8) + '.' + Copy(FDBFFields[I].Idx, 9, 3));
      if FileExists(TempFName) then begin
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
              case FDBFFields[I].idxtyp of
                dbfUnique: T.Duplicates := dupError;
                dbfDuplicates: T.Duplicates := dupAccept;
              end;
              while not System.Eof(F) do begin
                {uprava indexu je zde}
                {adapt index here}
                BlockRead(F, S[1], FDBFFields[I].len + SizeOf(L));
                Move(S[1], L, SizeOf(L));
                {neni tam nahodou uz nektery k uprave?}
                {if adapted?}
                if R = L then begin
                  ELoad(FDBFFields[I].Name, X);
                  Move(X[1], S[5], Length(X)); {zkus ho tam pridat}
                  UpdateField := True;
                end;
                try
                  T.Add(' ' + Copy(S, 5, 255) + #1 + IntToStr(L));
                except
                  on EListError do
                    if T.Duplicates = dupError then begin
                      Warn(msgDuplicateInUnique);
                      Result := False;
                      Exit;
                    end;
                end;
              end {while};
              {vlozil jsi vsechny ze souboru, tak ted zkus primy}
              {all added, try direct}
              if not UpdateField then begin
                ELoad(FDBFFields[I].Name, X);
                try
                  T.Add(' ' + X + #1 + IntToStr(R));
                except
                  on EListError do
                    if T.Duplicates = dupError then begin
                      Warn(msgDuplicateInUnique);
                      Result := False;
                      Exit;
                    end;
                end;
              end;
              {byl-li index uspesne vlozen, uloz indexovy soubor}
              {when index is OK, save as file}
              ReWrite(F, 1); {vymaz puvodni}
              case FDBFFields[I].idxsrt of
                dbfAscending:
                  begin
                    for J := 0 to T.Count - 1 do begin
                      S := T.Strings[J];
                      L := StrToInt(Copy(S, Pos(#1, S) + 1, 255));
                      S := '   ' + Copy(S, 1, Pos(#1, S) - 1);
                      Move(L, S[1], SizeOf(L));
                      BlockWrite(F, S[1], Length(S));
                    end;
                  end;
                dbfDescending:
                  begin
                    for J := T.Count - 1 downto 0 do begin
                      S := T.Strings[J];
                      L := StrToInt(Copy(S, Pos(#1, S) + 1, 255));
                      S := '   ' + Copy(S, 1, Pos(#1, S) - 1);
                      Move(L, S[1], SizeOf(L));
                      BlockWrite(F, S[1], Length(S));
                    end;
                  end;
                dbfAlternative:
                  begin
                  {Potrebujete-li to, tak jedine doprogramovat}
                  {if you want you have to coplete do it}
                    if AsSigned(FDBFOnAltSort) then FDBFOnAltSort(Self, T);
                  {a uloz to ...}
                  {and save it...}
                    for J := 0 to T.Count - 1 do begin
                      S := T.Strings[J];
                      L := StrToInt(Copy(S, Pos(#1, S) + 1, 255));
                      S := '   ' + Copy(S, 1, Pos(#1, S) - 1);
                      Move(L, S[1], SizeOf(L));
                      BlockWrite(F, S[1], Length(S));
                    end;
                  end;
              end;
            finally
              T.Free {zahod ho} {throw off}
            end;
            {idxsrt:TSortByIndex = (dbfAscending, dbfDescending, dbfAlternative);}
          finally
            CloseFile(F);
          end;
        except
          Fatal(Format(msgIdxTableNotFound, [ExtractFileName(TempFName)]));
        end;
      end
      else begin
        AsSignFile(F, TempFName);
        {tabulka jeste neexistuje}
        {table doesn't exist}
        ReWrite(F, 1);
        ELoad(FDBFFields[I].Name, X);
        Move(R, S[1], SizeOf(R));
        Move(X[1], S[5], Length(X)); {try it add there}
        BlockWrite(F, S[1], Length(X) + SizeOf(R));
        CloseFile(F);
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.RemoveIndexes(R: LongInt);
{----------------------------------------------------------------------------}
var
  I: Integer;
  L: LongInt;
  TempFName, S: string;
  F, Fnew: file;
begin
  {je-li nejaky zaznam vymazan, musi se tez odstranit ze vsech indexu}
  {when is some record deleted, have to delete from all indexes too}
  for I := 1 to FDBFCountItems do begin
    {prochazis vsechny sloupce a hledas indexovy soubor}
    {for through columns}
    if Trim(FDBFFields[I].Idx) <> '' then begin
      {indexovy soubor byl nalezen}
      {jeho jmeno je ve tvaru xxxxxxxxIDX, vzdy zarovnan vpravo}
      {vyplne jsou SpacerD tj. "~~~JMENOIDX" nebo "~~ZAMESTID~"}
      {vyrob temp jmeno souboru}
      {found, format name}
      TempFName := ExtractFilePath(FDBFName)
        + Trim(Copy(FDBFFields[I].Idx, 1, 8) + '.' + Copy(FDBFFields[I].Idx, 9, 3));
      AsSignFile(F, TempFName);
      try
        ReSet(F, 1);
        try
          AsSignFile(Fnew, ChangeFileExt(TempFName, '.$$$'));
          try
            ReWrite(Fnew, 1);
            try
              while not System.Eof(f) do begin
                BlockRead(F, S[1], FDBFFields[I].len + SizeOf(L));
                Move(S[1], L, SizeOf(L));
                if L <> R then begin
                  BlockWrite(Fnew, S[1], FDBFFields[I].len + SizeOf(L))
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
          if FileExists(ChangeFileExt(TempFName, '.$$$')) then begin
            DeleteFile(TempFName);
            RenameFile(ChangeFileExt(TempFName, '.$$$'), TempFName)
          end;
        end;
      except
        Fatal(Format(msgIdxTableNotFound, [ExtractFileName(TempFName)]));
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.MakeIndex(const IdxName: string; const Key: TKey);
{vytvori index}
{Make the index}
{----------------------------------------------------------------------------}
var
  F: file;
  I, L: LongInt;
  A, B, FLD: Integer;
  S: string;
  T: TStringList;
begin
  {Musi existovat a byt otevrena neprazdna tabulka, lze pouzit i pro preindexovani}
  {Unempty table have to exist (and reindexing too)}
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
      if FDBFHeader.NumRecs < MaxItems then begin
        T := TStringList.Create;
        try
          {budes tridit radky}
          {lines sorting}
          T.Sorted := True;
          for FLD := 1 to FDBFCountItems do
            if Trim(FDBFFields[FLD].Name) = Key then Break;
          {nastavujes vlastnost duplicit}
          {property duplicates}
          case FDBFFields[FLD].idxtyp of
            dbfUnique: T.Duplicates := dupError;
            dbfDuplicates: T.Duplicates := dupAccept;
          end;
          {projdes celou tabulku a vytahnes z ni pozadovane pole}
          {go through table}
          for I := 0 to FDBFHeader.NumRecs - 1 do begin
            Seek(I);
            ELoad(Key, S);
            {vlozis ho i s pozici do seznamu}
            {with position}
            try
              T.Add(' ' + S + #1 + IntToStr(I));
            except
              on EListError do
                if T.Duplicates = dupError then Fatal(msgDuplicateInUnique);
            end;
            {aktualizujes citac pro meridlo - vhodne je tez nastavovat kurzor}
            {counter actualisation for gauge}
            B := Round((I + 1) / (FDBFHeader.NumRecs / 100));
            if A <> B then begin {tohle je proto, aby se progress volal jen 101x}
              A := B;
              if AsSigned(FOnProgress) then FOnProgress(Self, prgMakeIndexSort, B);
            end;
          end;
          {znovu projdes seznam, upravis ho do tvaru <cislo><klic> a zapises}
          {go through list, format items as <number><key> an write it}
          if AsSigned(FOnProgress) then FOnProgress(Self, prgWriteIndexSort, B);
          case FDBFFields[FLD].idxsrt of
            dbfDescending: {sestupne}
              for I := T.Count - 1 downto 0 do begin
                S := T.Strings[I];
                L := StrToInt(Copy(S, Pos(#1, S) + 1, 255));
                S := '   ' + Copy(S, 1, Pos(#1, S) - 1);
                Move(L, S[1], SizeOf(L));
                BlockWrite(F, S[1], Length(S));
              end;
            dbfAscending: {vzestupne}
              for I := 0 to T.Count - 1 do begin
                S := T.Strings[I];
                L := StrToInt(Copy(S, Pos(#1, S) + 1, 255));
                {dopredu tri mezery, jedna tam uz je}
                {fill three space before it}
                S := '   ' + Copy(S, 1, Pos(#1, S) - 1);
                Move(L, S[1], SizeOf(L));
                BlockWrite(F, S[1], Length(S));
              end;
            dbfAlternative: {proste jinac}
              begin
              {Potrebujete-li to, tak jedine doprogramovat}
              {if you want you have to coplete do it}
                if AsSigned(FDBFOnAltSort) then FDBFOnAltSort(Self, T);
                for I := 0 to T.Count - 1 do begin
                  S := T.Strings[I];
                  L := StrToInt(Copy(S, Pos(#1, S) + 1, 255));
                  S := '   ' + Copy(S, 1, Pos(#1, S) - 1);
                  Move(L, S[1], SizeOf(L));
                  BlockWrite(F, S[1], Length(S));
                end;
              end;
          end;
        finally
          T.Free
        end;
      end
      else
        Warn(msgFileIsTooLarge);
    finally
      CloseFile(F)
    end;
  except
    Fatal(msgNotEnoughtCreateIdx)
  end;
end;

{----------------------------------------------------------------------------}
function TjbDBF.ReIndex;
{provede reindexovani tabulky}
{reindexing of table}
{----------------------------------------------------------------------------}
var
  I: Integer;
  TempFName: string;
begin
  Result := False;
  {je-li tabulka zrovna otevrena, tak nedelej nic}
  {when table is opened do nothing}
  if IsMarked then Exit;
  {nemuzes-li si ji otevrit taky pro sebe, tak taky nic nedelej}
  {if you cannot open for this (transaction?) do nothing}
  if not Cover then Exit;
  try
    for I := 1 to FDBFCountItems do begin
      {prochazis vsechny sloupce a hledas indexovy soubor}
      {go through comumns}
      if Trim(FDBFFields[I].Idx) <> '' then begin
        {indexovy soubor byl nalezen}
        {jeho jmeno je ve tvaru xxxxxxxxIDX, vzdy zarovnan jako soubor 8-3}
        {vyplne jsou SpacerD tj. "~~~JMENOIDX" nebo "~~ZAMESTID~"}
        {vyrob temp jmeno souboru}
        {found, format name}
        TempFName := ExtractFilePath(FDBFName)
          + Trim(Copy(FDBFFields[I].Idx, 1, 8) + '.' + Copy(FDBFFields[I].Idx, 9, 3));

        MakeIndex(TempFName, Trim(FDBFFields[I].Name))
      end;
    end;
  finally
    UnCover;
    Result := True;
  end;
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.Update(R: LongInt);
{----------------------------------------------------------------------------}
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

{----------------------------------------------------------------------------}
procedure TjbDBF.Store(const Key: TKey; const Rec: string);
{vlozi hodnotu do bufferu aktualniho zaznamu dle klice pole}
{save value to actual record buffer by field key}
{----------------------------------------------------------------------------}
var
  I, Posic: Integer;
  S: string;
begin
  {pozice zacina od jedne, i kdyz je buffer od 0 protoze na }
  {pozici 0 je indikacni byte o vymazani vety}
  {cout from 1, position 0 is flag for deleting}
  Posic := 1;
  for I := 1 to FDBFCountItems do begin
    if Trim(FDBFFields[I].Name) = UpperCase(Key) then Break
    else Inc(Posic, FDBFFields[I].Len);
  end;
  S := Rec;
  if Length(S) < FDBFFields[I].Len then
    case FDBFFields[I].What of
      'C', 'L': while Length(S) < FDBFFields[I].Len do S := S + ' ';
      'F', 'N', 'M': while Length(S) < FDBFFields[I].Len do S := ' ' + S;
      'D': ; {date is 8 chars only ddmmyyy or mmddyyyy}
      'T': ; {time is 6 chars only hhmmss}
    end;
  Move(S[1], FDBFBuff^[Posic], FDBFFields[I].Len);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.ELoad(const Key: TKey; var Rec: string);
{Precte hodnotu z bufferu aktualniho zaznamu dle klice pole}
{read value from actualrecord buffer by field key}
{----------------------------------------------------------------------------}
var
  I, Posic: Integer;
begin
  {pozice zacina od jedne, i kdyz je buffer od 0 protoze na }
  {pozici 0 je indikacni byte o vymazani vety}
  {cout from 1, position 0 is flag for deleting}
  Posic := 1;
  {nejprve musi najit jmeno pole a nascitat pocatek}
  {search field name and recount start of}
  for I := 1 to FDBFCountItems do begin
    if Trim(FDBFFields[I].Name) = UpperCase(Key) then Break
    else Inc(Posic, FDBFFields[I].Len);
  end;
  {predej zaznam neotrimovany}
  {add unformating record}
  Move(FDBFBuff^[Posic], Rec[1], FDBFFields[I].Len);
{$IFDEF VER12UP}
  SetLength(Rec, FDBFFields[I].Len);
{$ELSE}
  Rec[0] := Chr(FDBFFields[I].Len);
{$ENDIF}
end;

{----------------------------------------------------------------------------}
function TjbDBF.EOF: Boolean;
{----------------------------------------------------------------------------}
begin
  {indikovan stav, je-li kurzor na konci tabulky}
  {cursor position indikated on last row of the table}
  Result := FDBFCurrRec = (FDBFHeader.numRecs - 1);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.Last;
{----------------------------------------------------------------------------}
begin
  {kurzor ukazuje na posledni radek tabulky}
  {cursor set up to last row of table}
  GotoEnd;
end;

{----------------------------------------------------------------------------}
function TjbDBF.Load(const Key: TKey): string;
{tohle je uzivatelska modifikace funkce ELoad, ktera orizne mezery}
{this formating version of ELoad}
{----------------------------------------------------------------------------}
begin
  {vola standarni funkci}
  {call standard function}
  ELoad(Key, Result);
  {a tady jeste orizne nadbytecne mezery}
  {and trim spaces}
  Result := Trim(Result);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.Find(const IdxName, Value {toto je vlastne klic}: string);
{hleda hodnotu podle klice idxname je jmeno sloupce value je hodnota
 funkcionalitu = <> > < >= <= dodava onquery}
{dearch value by key}
{more give = <> > < >= <= get onquery}
{----------------------------------------------------------------------------}
  function SizeOfKey(const Key: string): Integer;
  {vraci sirku klice/pole}
  var I: Integer;
  begin
    for I := 1 to FDBFCountItems do begin
      if Trim(FDBFFields[I].Name) = UpperCase(Key) then begin
        Result := FDBFFields[I].Len;
        Exit
      end;
    end;
    Result := 0;
    Warn(Format(msgIdxFieldNotFound, [Key]));
  end;
var
  F: file;
  A, B, Size: Integer;
  S: string;
  I, N, L: LongInt;
  OK, Cancel: Boolean;
begin
  {Musi existovat a byt otevrena neprazdna tabulka, lze pouzit i pro preindexovani}
  {opened and unempty table}
  if not (FDBFExist and FDBFIsOpened and (FDBFHeader.NumRecs > 0)) then Exit;
  {tato procedura musi byt attachnuta, aby mohl dotaz fungovat}
  {OnQuery must be attach for good work !!!}
  if not AsSigned(FOnQuery) then Exit;
  {tahle taky, aby se dala predavat data}
  {and OnFound must be attach for good work too!!!}
  if not AsSigned(FOnFound) then Exit;
  {Otevri ji na stejnem miste, pouzij idxname a defaultni priponu}
  {open index}
  AsSignFile(F, ExtractFilePath(FDBFName) + IdxName);
  try
    ReSet(F, 1);
    try
      B := SizeOfKey(Value);
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
      if (I mod Size) <> 0 then begin
        Fatal(Format(msgErrorOnIdxTable, [IdxName]));
        Exit;
      end;
      {tohle je pocet polozek}
      {count of records}
      N := I div Size;
      {nastav prostor na S}
      {make place for it}
      S := ''; for I := 1 to Size do S := S + ' ';
      Cancel := False;
      {prochazej periodicky klic}
      {go by key}
      A := -1;
      for I := 0 to N - 1 do begin
        {$IFDEF VER12UP}
        SetLength(S, Size);
        {$ELSE}
        S[0] := Chr(Size);
        {$ENDIF}
        BlockRead(F, S[1], Size);
        Move(S[1], L, SizeOf(L));
        S := Copy(S, 5, 255);
        {zde je dotaz na tabulku, uzivatel filtruje dle pole}
        {query to table, user do filtering}
        {no accept}
        OK := False; {predpoklad, ze ho nechci}
        FOnQuery(Self, IdxName, Value, Trim(S), OK, Cancel);
        if OK then begin
          {je-li pozadovany filtr akceptovan, vyzvedni zaznam}
          {when accept, get record from table}
          Seek(L);
          {zaznam se musi zpracovat, jinak jsou data ztracena}
          {zde se data ctou napr. do listboxu nebo stringgridu}
          {record must be worked but data throw off}
          {may be read to list ??}
          FOnFound(Self);
        end;
        {aktualizujes citac - vhodne je tez nastavovat kurzor}
        {for gauge}
        B := Round((I + 1) / (N / 100));
        if Cancel then B := 100;
        if A <> B then begin {tohle je proto, aby se progress volal jen 101x}
          A := B;
          if AsSigned(FOnProgress) then FOnProgress(Self, prgSearchByKey, B);
        end;
        if Cancel then Break;
      end;
    finally
      CloseFile(F)
    end;
  except
    Fatal(Format(msgIdxTableNotFound, [ExtractFilePath(FDBFName) + IdxName]));
  end;
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.First;
{----------------------------------------------------------------------------}
begin
  {kurzor ukazuje na prvni radek tabulky}
  {cursor set up to first row of table}
  GotoStart;
end;

{----------------------------------------------------------------------------}
function TjbDBF.Cover: Boolean;
{nastavuje bit transakce}
{set flag for transaction}
{----------------------------------------------------------------------------}
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

{----------------------------------------------------------------------------}
procedure TjbDBF.UnCover;
{shazuje bit transakce}
{reset flag for transaction}
{----------------------------------------------------------------------------}
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

{----------------------------------------------------------------------------}
procedure TjbDBF.RemoveIndex(const Name: string);
{explicitni zruseni indexu .mdx a zruseni propojeni}
{explicit delete of index and erase link}
{----------------------------------------------------------------------------}
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

{----------------------------------------------------------------------------}
function TjbDBF.IsMarked: Boolean;
{vraci priznak, zda je nastavena transakce}
{get flag that transaction is of/off}
{----------------------------------------------------------------------------}
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

{----------------------------------------------------------------------------}
procedure TjbDBF.IncNumRec;
{procedura zvysi pocet zaznamu o jeden}
{increment count of records +1}
{----------------------------------------------------------------------------}
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

{----------------------------------------------------------------------------}
procedure TjbDBF.Actualization;
{zapis do hlavicky aktualni datum}
{write to header of dbf actual date and time}
{----------------------------------------------------------------------------}
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

{----------------------------------------------------------------------------}
function TjbDBF.GetLastNoOfMemo: Integer;
{funkce ziska z memo souboru posledni nejvyssi cislo zaznamu}
{get last highest number of record of memo}
{----------------------------------------------------------------------------}
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
    while not System.Eof(F) do begin
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

{----------------------------------------------------------------------------}
function TjbDBF.SaveMemo(No: LongInt; const FName: string): Boolean;
{ulozi soubor do memo}
{store file to memo}
{----------------------------------------------------------------------------}

var F, FF: file;
  T: TDBTTypes;
  S: string[79];
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
      with T do begin
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

{----------------------------------------------------------------------------}
function TjbDBF.LoadMemo(No: LongInt; var FName: string): Boolean;
{preda soubor z memo na disk do FName - zmeni u nej pouze extenzi dle uloz. typu}
{get filename from memo}
{----------------------------------------------------------------------------}
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
          for I := 1 to T.SizeOfMemo do begin
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

function TjbDBF.Locate(const KeyFields: string;
  const KeyValues: string; Options: TLocateOptionSet): Boolean;
  procedure GiveFields(const KeyFields: string; out ListFields: TStringList);
  begin
    ListFields := TStringList.Create;
    ListFields.Delimiter := ';';
    ListFields.DelimitedText := KeyFields;
  end;
  function EvaluateFields(const KeyValue, Value: string; Options: TLocateOptionSet): Boolean;
  begin
    Result := False;
    if Options = [] then begin
      if AnsiCompareText(KeyValue, Value) = 0 then begin
        Result := True;
        Exit
      end;
    end
    else
      if Options = [loCaseSensitive, loPartialKey] then begin
        if Pos(KeyValue, Value) > 0 then begin
          Result := True;
          Exit
        end;
      end
      else
        if Options = [loCaseSensitive] then begin
          if AnsiCompareStr(KeyValue, Value) = 0 then begin
            Result := True;
            Exit
          end;
        end
        else
          if Options = [loPartialKey] then begin
            if Pos(AnsiUpperCase(KeyValue), AnsiUpperCase(Value)) > 0 then begin
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
    for C := toEQ to toGR do begin
      if Pos(OPE[C], StrExpressionValue) = 1 then begin
        System.Delete(StrExpressionValue,1,Length(OPE[C]));
        try
          case C of
            toEQ: Break;
            toNE:
              case FLD_CLMN.what of
                'M', 'C', 'B':
                  Result := AnsiCompareText(KeyValue, StrExpressionValue) <> 0;
                'N', 'L':
                  Result := StrToInt(KeyValue) <> StrToInt(StrExpressionValue);
                'F':
                  Result := StrToFloat(KeyValue) <> StrToFloat(StrExpressionValue);
                'D', 'T':
                  Result := GetAsDateTimeValue(KeyValue) <> GetAsDateTimeValue(StrExpressionValue);
              end;
            toLE:
              case FLD_CLMN.what of
                'M', 'C', 'B':
                  Result := AnsiCompareText(KeyValue, StrExpressionValue) <= 0;
                'N', 'L':
                  Result := StrToInt(KeyValue) <= StrToInt(StrExpressionValue);
                'F':
                  Result := StrToFloat(KeyValue) <= StrToFloat(StrExpressionValue);
                'D', 'T':
                  Result := GetAsDateTimeValue(KeyValue) <= GetAsDateTimeValue(StrExpressionValue);
              end;
            toGE:
              case FLD_CLMN.what of
                'M', 'C', 'B':
                  Result := AnsiCompareText(KeyValue, StrExpressionValue) >= 0;
                'N', 'L':
                  Result := StrToInt(KeyValue) >= StrToInt(StrExpressionValue);
                'F':
                  Result := StrToFloat(KeyValue) >= StrToFloat(StrExpressionValue);
                'D', 'T':
                  Result := GetAsDateTimeValue(KeyValue) >= GetAsDateTimeValue(StrExpressionValue);
              end;
            toLS:
              case FLD_CLMN.what of
                'M', 'C', 'B':
                  Result := AnsiCompareText(KeyValue, StrExpressionValue) < 0;
                'N', 'L':
                  Result := StrToInt(KeyValue) < StrToInt(StrExpressionValue);
                'F':
                  Result := StrToFloat(KeyValue) < StrToFloat(StrExpressionValue);
                'D', 'T':
                  Result := GetAsDateTimeValue(KeyValue) < GetAsDateTimeValue(StrExpressionValue);
              end;
            toGR:
              case FLD_CLMN.what of
                'M', 'C', 'B':
                  Result := AnsiCompareText(KeyValue, StrExpressionValue) > 0;
                'N', 'L':
                  Result := StrToInt(KeyValue) > StrToInt(StrExpressionValue);
                'F':
                  Result := StrToFloat(KeyValue) > StrToFloat(StrExpressionValue);
                'D', 'T':
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
  clmnName: string;
begin
  Result := False; {nothing found}
  GiveFields(KeyFields, ListFields); {get params}
  GiveFields(KeyValues, ListParams); {get check values}
  SetLength(OKARR, ListFields.Count); {checked array}
  try
    {if not added loNextLocate then set to top of table, else search next from cursor}
    if not (loNextLocate in Options) then First;
    while not EOF do begin {sequentially}
      for J := Low(OKARR) to High(OKARR) do OKARR[J] := False; {smazat! clear flags!}
      for I := 0 to ListFields.Count - 1 do begin {go trough collumns}
        clmnName := ListFields[I]; {get name}
        if FieldByName[clmnName].IsNull then {when is null it true allways}
          OKARR[I] := True
        else  {check by expression}
          OKARR[I] := EvalExpression(FieldByName[clmnName], ListParams[I], Options);
      end;
      {sloupce musi souhlasit}
      OK := True; {flag set up}
      for J := Low(OKARR) to High(OKARR) do {check all elements}
        if not OKARR[J] then begin {when is one from false reset flag}
          OK := False;
          Break; {waiting for new cycle}
        end;
      if OK then begin {if OK only}
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

{----------------------------------------------------------------------------}
function TjbDBF.EraseMemo(No: LongInt): Boolean;
{oznaci soubor v memo za nepouzivany}
{set mark in memo file as unused}
{----------------------------------------------------------------------------}
var F: file;
  T: TDBTTypes;
  Readed: {$IFDEF VER80}Word{$ELSE}Integer{$ENDIF};
begin
  Result := False;
  AsSignFile(F, ChangeFileExt(FDBFName, '.DBT'));
  try
    ReSet(F, 1);
    try
      T.NumberOf := -1;
      while T.NumberOf <> No do begin
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

{----------------------------------------------------------------------------}
function TjbDBF.PackDBF: boolean;
{odstrani z tabulky vymazane zaznamy}
{remove deleted records from table}
{create a temporary file named [filename]_temp_pack.dbf and when finish renamed it}
{added by /AR/ }
{----------------------------------------------------------------------------}
var
  DBFNew: TjbDBF;
  iCol: integer;
  sFileNew: string;
  iLenTot: integer;
  sField: string;
  bConfirm: boolean;
begin
  Result := false;
  bConfirm := true;
  DBFNew := TjbDBF.Create(Self);
  try
    if AsSigned(FOnErase) then FOnErase(Self, ExtractFileName(FDBFName), bConfirm);

    if not (bConfirm) then exit;

    iLenTot := 0;
    FileName := FDBFName;
    sFileNew := Copy(FDBFName, 1, Length(FDBFName) - Length(ExtractFileExt(FDBFName))) + '_temp_pack.dbf';

    SysUtils.DeleteFile(sFileNew);

    try
      if Open then
      begin
        GotoStart;

        for iCol := 1 to FieldsCount do
        begin
          with Fields[iCol] do
          begin
            iLenTot := iLenTot + len;
            DBFNew.MakeField(iCol, name, what, len, places, '', idxtyp, idxsrt);
            DBFNew.FDBFFields[iCol].Idx := idx;
          end;
        end;
        DBFNew.CreateDB(sFileNew, iLenTot, FieldsCount);

        DBFNew.FileName := sFileNew;
        if DBFNew.Open then
          while CurrRec < RecordsCount do
          begin
            if not (CurrentRecDeleted) then
            begin
              for iCol := 1 to FieldsCount do
              begin
                sField := Fields[iCol].Name;
                DBFNew.Store(sField, Load(sField));
              end;
              DBFNew.NewRecord;
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
            Result := true;
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

{----------------------------------------------------------------------------}
procedure TjbDBF.PackDBT;
{odstrani z memo nepouzite zaznamy}
{remove deleted records from memo}
{----------------------------------------------------------------------------}
begin
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.Post;
{----------------------------------------------------------------------------}
begin
  {prave zmeneny zaznam je aktualizovan}
  {currently changed record is actualized, stored}
  Update(FDBFCurrRec);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.Prior;
{----------------------------------------------------------------------------}
begin
  {postup o radek k vrsku tabulky}
  {go to top table by one row}
  GotoPrev;
end;

{ Changed by /VN/: }
{----------------------------------------------------------------------------}
function TjbDBF.GetRecordsCount: LongInt;
{vraci pocet zaznamu}
{get count of records}
{----------------------------------------------------------------------------}
begin
  Result := FDBFHeader.numRecs;
end;

function TjbDBF.GetVersion: string;
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

{----------------------------------------------------------------------------}
function TjbDBF.GetField(Index: Integer): TDBField;
{vrati polozku}
{get field}
{----------------------------------------------------------------------------}
begin
  if (Index < 0) or (Index > FDBFCountItems) then
    Fatal(Format(msgFieldNotFound, [Index]))
  else
    Result := FDBFFields[Index];
end;

{----------------------------------------------------------------------------}
function TjbDBF.IsCurrentRecDeleted: Boolean;
{rekne zda-li je zaznam vymazan}
{get flat that records is deleted}
{----------------------------------------------------------------------------}
begin
  Result := FDBFBuff^[0] = '*';
end;

{added by /AR/ }
{----------------------------------------------------------------------------}
function TjbDBF.GetFieldByName(const Key: TKey): TDBField;
{----------------------------------------------------------------------------}
var
  bFound: boolean;
  i: integer;
begin

  bFound := false;
  i := 0;

  while not (bFound or (i > (FDBFCountItems))) do
  begin
    inc(i);
    if Trim(FDBFFields[i].Name) = UpperCase(Key) then
    begin
      bFound := true;
      Result := FDBFFields[i];
    end;
  end;
  if not (bFound) then
    Fatal(msgBadDefinitionOfField);
end;

{$IFDEF VER10UP}

{ TDBField }

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

function DateTimeStrEval(const DateTimeFormat : string;
                         const DateTimeStr : string) : TDateTime;
var i,ii,iii : integer; 
    Retvar : TDateTime; 
    Tmp, 
    Fmt,Data,Mask,Spec : string; 
    Year,Month,Day,Hour, 
    Minute,Second,MSec : word; 
    AmPm : integer; 
begin 
  Year := 1; 
  Month := 1; 
  Day := 1; 
  Hour := 0; 
  Minute := 0; 
  Second := 0; 
  MSec := 0; 
  Fmt := UpperCase(DateTimeFormat); 
  Data := UpperCase(DateTimeStr); 
  i := 1; 
  Mask := ''; 
  AmPm := 0; 

  while i < length(Fmt) do begin 
    if Fmt[i] in ['A','P','D','M','Y','H','N','S','Z'] then begin 
      // Start of a date specifier 
      Mask  := Fmt[i]; 
      ii := i + 1; 

      // Keep going till not valid specifier 
      while true do begin 
        if ii > length(Fmt) then Break; // End of specifier string 
        Spec := Mask + Fmt[ii]; 

        if (Spec = 'DD') or (Spec = 'DDD') or (Spec = 'DDDD') or 
           (Spec = 'MM') or (Spec = 'MMM') or (Spec = 'MMMM') or 
           (Spec = 'YY') or (Spec = 'YYY') or (Spec = 'YYYY') or 
           (Spec = 'HH') or (Spec = 'NN') or (Spec = 'SS') or 
           (Spec = 'ZZ') or (Spec = 'ZZZ') or 
           (Spec = 'AP') or (Spec = 'AM') or (Spec = 'AMP') or 
           (Spec = 'AMPM') then begin 
          Mask := Spec; 
          inc(ii); 
        end 
        else begin 
          // End of or Invalid specifier 
          Break; 
        end; 
      end; 

      // Got a valid specifier ? - evaluate it from data string 
      if (Mask <> '') and (length(Data) > 0) then begin 
        // Day 1..31 
        if (Mask = 'DD') then begin 
           Day := StrToIntDef(trim(copy(Data,1,2)),0); 
           delete(Data,1,2); 
        end; 

        // Day Sun..Sat (Just remove from data string) 
        if Mask = 'DDD' then delete(Data,1,3); 

        // Day Sunday..Saturday (Just remove from data string LEN) 
        if Mask = 'DDDD' then begin 
          Tmp := copy(Data,1,3); 
          for iii := 1 to 7 do begin 
            if Tmp = Uppercase(copy(LongDayNames[iii],1,3)) then begin 
              delete(Data,1,length(LongDayNames[iii])); 
              Break; 
            end; 
          end; 
        end; 

        // Month 1..12 
        if (Mask = 'MM') then begin 
           Month := StrToIntDef(trim(copy(Data,1,2)),0); 
           delete(Data,1,2); 
        end; 

        // Month Jan..Dec 
        if Mask = 'MMM' then begin 
          Tmp := copy(Data,1,3); 
          for iii := 1 to 12 do begin 
            if Tmp = Uppercase(copy(LongMonthNames[iii],1,3)) then begin 
              Month := iii; 
              delete(Data,1,3); 
              Break; 
            end; 
          end; 
        end; 


        // Month January..December 
        if Mask = 'MMMM' then begin 
          Tmp := copy(Data,1,3); 
          for iii := 1 to 12 do begin 
            if Tmp = Uppercase(copy(LongMonthNames[iii],1,3)) then begin 
              Month := iii; 
              delete(Data,1,length(LongMonthNames[iii])); 
              Break; 
            end; 
          end; 
        end; 

        // Year 2 Digit 
        if Mask = 'YY' then begin 
          Year := StrToIntDef(copy(Data,1,2),0);
          delete(Data,1,2); 
          if Year < TwoDigitYearCenturyWindow then 
            Year := (YearOf(Date) div 100) * 100 + Year
          else 
            Year := (YearOf(Date) div 100 - 1) * 100 + Year; 
        end; 

        // Year 4 Digit 
        if Mask = 'YYYY' then begin 
          Year := StrToIntDef(copy(Data,1,4),0); 
          delete(Data,1,4); 
        end; 

        // Hours 
        if Mask = 'HH' then begin 
           Hour := StrToIntDef(trim(copy(Data,1,2)),0); 
           delete(Data,1,2); 
        end; 

        // Minutes 
        if Mask = 'NN' then begin 
           Minute := StrToIntDef(trim(copy(Data,1,2)),0); 
           delete(Data,1,2); 
        end; 

        // Seconds 
        if Mask = 'SS' then begin 
           Second := StrToIntDef(trim(copy(Data,1,2)),0); 
           delete(Data,1,2); 
        end; 

        // Milliseconds 
        if (Mask = 'ZZ') or (Mask = 'ZZZ') then begin 
           MSec := StrToIntDef(trim(copy(Data,1,3)),0); 
           delete(Data,1,3); 
        end; 


        // AmPm A or P flag 
        if (Mask = 'AP') then begin 
           if Data[1] = 'A' then 
             AmPm := -1 
           else 
             AmPm := 1; 
           delete(Data,1,1); 
        end; 

        // AmPm AM or PM flag 
        if (Mask = 'AM') or (Mask = 'AMP') or (Mask = 'AMPM') then begin 
           if copy(Data,1,2) = 'AM' then 
             AmPm := -1 
           else 
             AmPm := 1; 
           delete(Data,1,2); 
        end; 

        Mask := ''; 
        i := ii; 
      end; 
    end 
    else begin 
      // Remove delimiter from data string 
      if length(Data) > 1 then delete(Data,1,1); 
      inc(i); 
    end; 
  end; 

  if AmPm = 1 then Hour := Hour + 12; 
  if not TryEncodeDateTime(Year,Month,Day,Hour,Minute,Second,MSec,Retvar) then 
    Retvar := 0.0; 
  Result := Retvar; 
end;

function TDBField.GetAsBoolean: Boolean;
begin
  Result := AnsiUpperCase(FDBF.Load(name)) = ccTrue;
end;

function TDBField.GetAsCurrency: Currency;
begin
  Result := StrToFloat(FDBF.Load(name));
end;

function TDBField.GetAsDate: TDateTime;
begin
  Result := 0;
  if IsNotNull then
    if what = 'D' then
      Result := DateTimeStrEval('ddmmyyyy',FDBF.Load(name));
end;

function TDBField.GetAsFloat: Double;
var
  S: string;
begin
  Result := 0;
  if IsNotNull then
    if what = 'F' then begin
      S := FDBF.Load(name);
      if places > 0 then
        Insert(DecimalSeparator, S, Length(S) - places);
      Result := StrToFloat(S);
    end;
end;

function TDBField.GetAsInteger: Longint;
begin
  Result := 0;
  if IsNotNull then
    try
      Result := StrToInt(FDBF.Load(name))
    except
      Result := 0;
    end;
end;

function TDBField.GetAsMemo: string;
begin
  Result := '';
  if IsNotNull then
    case what of
      'M', 'B':
      begin
        Result := FDBF.Load(name); //get number of file
        FDBF.LoadMemo(StrToInt(Result), Result); //give filename
      end;
    end;
end;

function TDBField.GetAsString: string;
begin
  Result := FDBF.Load(name);
end;

function TDBField.GetAsTime: TDateTime;
begin
  Result := 0;
  if IsNotNull then
    if what = 'T' then
      Result := DateTimeStrEval('hhnnss',FDBF.Load(name));
end;

function TDBField.IsNotNull: Boolean;
var
  S: string;
begin
  S := FDBF.Load(name);
  case what of
    'D': Result := (S <> '') and (S <> '00000000');
  else
    Result := S <> '';
  end;
end;

procedure TDBField.Clear;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TDBField.IsNull: Boolean;
var
  S: string;
begin
  S := FDBF.Load(name);
  case what of
    'D': Result := (S = '') or (S = '00000000');
  else
    Result := S = '';
  end;
end;

procedure TDBField.SetAsBoolean(const Value: Boolean);
begin
  if what = 'L' then
    if Value then
      FDBF.Store(name, ccTrue)
    else
      FDBF.Store(name, ccFalse);
end;

procedure TDBField.SetAsCurrency(const Value: Currency);
begin
  if what in ['F', 'N', 'C'] then
    FDBF.Store(name, Format('%'+IntToStr(len)+'.' + IntToStr(places) + 'f', [Value]));
end;

procedure TDBField.SetAsDate(const Value: TDateTime);
begin
  if what in ['D', 'C'] then
    FDBF.Store(name, SysUtils.FormatDateTime('yyyymmdd',Value));
end;

procedure TDBField.SetAsFloat(const Value: Double);
begin
  if what in ['F', 'C'] then
    FDBF.Store(name, Format('%'+IntToStr(len)+'.' + IntToStr(places) + 'f', [Value]));
end;

procedure TDBField.SetAsInteger(const Value: Integer);
begin
  case what of
    'F': FDBF.Store(name, Format('%'+IntToStr(len)+'.' + IntToStr(places) + 'f', [Value]));
    'N', 'C': FDBF.Store(name, Format('%'+IntToStr(len)+'.d', [Value]));
  end;
end;

procedure TDBField.SetAsMemo(const Value: string);
var
  No: Integer;
begin
  if what = 'M' then begin
    FDBF.SaveMemo(No, Value);
    FDBF.Store(name, IntToStr(No));
  end;
end;

procedure TDBField.SetAsString(const Value: string);
begin
  FDBF.Store(name, Value);
end;

procedure TDBField.SetAsTime(const Value: TDateTime);
begin
  if what in ['T', 'C'] then
    FDBF.Store(name, SysUtils.FormatDateTime('hhnnss',Value));
end;

{  TDBTTypes  }

procedure TDBTTypes.Clear;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

{$ENDIF}

end. {end of file; end of comments 15.1.2002 by J.B. Sorry for my English}
