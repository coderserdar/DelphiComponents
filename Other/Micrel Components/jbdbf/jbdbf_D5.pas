{$IfDef VER80}
{$X+}
{$Else}
{$X+,H-}
{$EndIf}
unit jbDbf_D5;
{$I jb.inc}

{founded 1999 (c) Jaro Benes}
{works with DBF table by direct file}
{for version Delphi 1..6}
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

{$IfDef LINUX} { /AR/ }  
  uses SysUtils, Classes, QDialogs;
{$Else}
  uses SysUtils, Classes, Dialogs;
{$EndIf}

{$IfNDef VER5UP}
Const
{$Else}
ResourceString { Changed by me /VN/ }
{$EndIf}

{$IfDEF MessagesItalianLang}
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

{$Else}

{$IfDef MessagesCzechLang}
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
{$Else}
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
{$EndIf}
{$EndIf}
{----------------------------------------------------------------------------}
Const
  MaxSize = $7FFF;  { maximum for buffer }
  {$IfDef VER80}
  MaxItems = 16384; { this is for 16bit Delphi, is possible to change}
  {$Else}
  MaxItems = MaxInt;
  {$EndIf}
  MaxFields = 128;  { maximum count for columns }
  DeleteFlag = '*';
  EOFFlag = #$1A;
  SpacerD = ' ';
Type
  charay11=Array [0..10] Of Char; {filename defs.}

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
  TDBFError = procedure (Sender: TObject; Const ErrorMsg: String) of object;
  TDBFMakeItems = procedure (Sender: TObject; Posit: Integer;
   Var INname: ChAray11; Var IWhat: Char; Var ILen, IPlaces: Byte;
   Var IDXName:charay11;Var Req: TFieldReq; Var Sort: TSortByIndex) of object;
  TDBFProgress = procedure (Sender: TObject; Const Operace: OpenString; Progress: Integer) of object;
  TDBFPassword = procedure (Sender: TObject; Var Pass: OpenString) of object;
  TDBFAssigned = procedure (Sender: TObject; Var FName: OpenString) of object;
  TDBFBeforeConfirm = procedure (Sender: TObject; Const FName:String; Var Confirm: Boolean) of object;
  TDBFConfirm = procedure (Sender: TObject; Var Confirm: Boolean) of object;
  TDBFNavigate = procedure (Sender: TObject; Position: LongInt) of object;
  TDBFOpened = procedure (Sender: TObject; IsOpened: Boolean) of object;
  TDBFChange = procedure (Sender: TObject; Var Cancel: Boolean) of object;
  TDBFActualize = procedure (Sender: TObject; Status: TPostAct) of object;
  TDBFQuery = procedure (Sender: TObject; Const IdxName, IdxField, Key: OpenString;
    Var Accept, Cancel: Boolean) of object;
  TDBFAltSort = procedure (Sender: TObject; TblIdx: TStringList) of object;

  TRecArray = Array [0..MaxSize] of Char;
  PRecArray = ^TRecArray;
  PBigArray = PRecArray;

  TDBFHeader = record         { legends are from manual, changed for me specific}
    version        : byte;    { Should be 3 or $83                           1 }
                              { $3  - FoxBase+/dBase III Plus bez souboru MEMO }
                              {     - FoxPro/dBase IV bez souboru memo
                              { $83 - FoxBase+/dBase III Plus se souborem MEMO }
                              { $F5 - FoxPRo se souborem memo                  }
                              { $8B - dBase IV se souborem memo                }
    year,month,day : byte;    { Date of last update                          3 }
    numRecs        : longint; { Number of records in the file                4 }
    headLen        : word;    { Length of the header                         2 }
    recLen         : word;    { Length of individual records                 2 }
    nets           : word;    { not used                                       }
    transaction    : byte;    { begin-end transaction                          }
                              { 00 - no transaction protected                  }
                              { 01 - transaction protected                     }
    encrypted      : byte;    { coded fields                                   }
                              { 00 - uncrypted                                 }
                              { 01 - encrypted                                 }
    network        : array [1..12] of byte;
    mdxfile        : byte;    { exist .mdx file indicator                      }
                              { 00 - non exist                                 }
                              { 01 - exist and join                            }
    LangDrv        : byte;    { language driver /fox/                          }
                              { 001 - code page 437                            }
                              { 002 - code page 850                            }
                              { 100 - code page 852                            }
                              { 102 - code page 865                            }
                              { 101 - code page 866                            }
                              { 104 - code page 895                            }
                              { 200 - code page 1250                           }
                              { 201 - code page 1251                           }
                              { 003 - code page 1252                           }
    labeled        : word;
  end;

Const
  TdbTypes:Set of Char = ['C'{characters, all ascii},
                          'D'{date ddmmyyyy, fix size 8},
                          'T'{time hhmmss, fix size 6},
                          'F'{float point, -,.,0..9},
                          'L'{logical, ?,Y,y,N,n},
                          'M'{memo, as numeric, fix size 10},
                          'N'{numeric, -,.,0..9}];
  {struktura zazn. zapisniku, pripojene soubory predava pres disk}
  {structure of memo appended file on disk}
Type
  TDBTTypes=Record
    NumberOf:LongInt;                 { record no. }
    AsFileType:Array [1..3] of Char;  { extension of saved type }
    Used:Boolean;                     { used/unused }
    SizeOfMemo:LongInt;               { size of appended file }
    FileDateTime:Double;              { original date and time of file }
    {MemoField:Array[1..SizeOfMemo] of Byte; // ulozeny soubor}
  End;
  {
  struktura zaznamu klice, je bez hlavicky
  structure record key, without head
  TIDXTypes=Record
    ItemNo:LongInt;                   // refer to record in table
    Key:TDBField.Len;                 // key component
  End;
  }

Type
  TdbField = Record
    name   : charay11;           { Name of the field                     11 }
    what   : Char;               { Type of data in this field             1 }
    data   : array[0..1] of word;{ Not used                               2 }
    len    : byte;               { Length of the field                    1 }
    places : byte;               { Number of decimal places               1 }
    idxtyp : TFieldReq;          { typ klice unikatni/duplicitni...       1 }
    idxsrt : TSortByIndex;       { setridit vzestupne, sestupne, custom...1 }
    dfIdent: Byte;               { datafield identifier ??                1 }
    idx    : charay11;           { here file name in index  XXXXXXXXIDX  11 }
  End;

Type
  TKey=String;
{----------------------------------------------------------------------------}
  TjbDBF = class(TComponent)
  private
    FDBFName        : String;           { Full name table }
    FDBFIsOpened    : Boolean;          { TRUE when is file opened }
    FDBFStoreByIndex: Boolean;          { Store by list index }
    FDBFHandle      : File;             { Handle of actual file }
    FDBFExist       : Boolean;          { Indicate file exists when is name assigned }
    FDBFReadOnly    : Boolean;          { Read only }
    FDBFSaveOnClose : Boolean;          { Save on close }
    FDBFHeader      : TDBFHeader;       { Header,  filled after open }
    FDBFIndex       : String;           { Actual index for FIND }
    FDBFPassword    : String;           { Administrator password }
    FDBFFilter      : String;           { not used}
    FDBFIndexList   : TStringList;      { List all indexes of table }
    FDBFBuff        : PBigArray;        { Temp FDBFBuff for record }
    FDBFCurrRec     : LongInt;          { Cursor position point to record }
    FDBFCountItems  : Integer;          { Count of recors collumns }
    FOnError        : TDBFError;        { Event for error administration }
    FOnWarn         : TDBFError;        { Event for warnings administration }
    FOnMakeFields   : TDBFMakeItems;    { For create fields in record }
    FOnErase        : TDBFBeforeConfirm;{ For confirm with pack of .DBF }
    FOnOverwrite    : TDBFBeforeConfirm;{ For confirm with overwrite of .DBF .IDX }
    FOnAdded        : TNotifyEvent;     { If record added }
    FDBFOnAltSort   : TDBFAltSort;      { Alternative sort on stringlist }
    FOnChange       : TDBFChange;       { If record in change }
    FOnChanged      : TNotifyEvent;     { If record is changed }
    FOnDelete       : TDBFConfirm;
    FOnActualize    : TDBFActualize;
    FOnDeleted      : TNotifyEvent;     { If record is deleted }
    FOnPassword     : TDBFPassword;     { If administrator request password check }
    FOnOpened       : TDBFOpened;       { If table is opened }
    FOnAsSigned     : TDBFAsSigned;     { If table attach }
    FOnFound        : TNotifyEvent;     { If found record by index }
    FOnErased       : TNotifyEvent;     { If table is packed }
    FOnNavigate     : TDBFNavigate;     { If navigation is called }
    FOnProgress     : TDBFProgress;     { If table id updated, show percent on gauge}
    FOnUpdate       : TNotifyEvent;     { If record is actualized }
    FOnClosed       : TNotifyEvent;     { If table is closed}
    FOnLoaded       : TNotifyEvent;     { If record attach to buffer memory }
    FOnQuery        : TDBFQuery;        { For query with find statement }

    procedure SetFileName(name : string);
    Function GetPassword: String;
    procedure SetPassword(Const thepassword:String);
    function GetRecordsCount: LongInt; { return the records count /VN/ }
    function GetField(Index: Integer): TDBField; { /VN/ }
    function GetFieldByName(Const Key:TKey): TDBField; { /AR/ }
    function IsCurrentRecDeleted: Boolean; { /VN/ }
  protected
    FDBFFields    : array[1..maxFields] of TdbField;  { The field data }
    procedure Fatal(Const Msg:String);
    Procedure Warn(Const Msg:String);
    Procedure Actualization;
  public
    constructor Create(AOWner : TComponent); override;
    destructor Destroy; override;

    procedure Close; virtual;
    Function  Open:Boolean; virtual;
    Function  Write(r : longint):TStatusWrite; virtual;
    procedure Seek(r : longint); virtual;
    procedure NewRecord; virtual;
    procedure GotoStart;
    procedure GotoEnd;
    procedure GotoNext;
    Procedure GotoPrev;
    Function  Delete(R : longint):TStatusWrite; Virtual;
    Function  UpdateIndexes(R:LongInt):Boolean; Virtual;
    Procedure RemoveIndexes(R:LongInt); Virtual;
    procedure MakeIndex(Const IdxName:String; Const Key:TKey);{make index} Virtual;
    Procedure Find(Const IdxName, Value:String);{search value by key} Virtual;
    Procedure Store(Const Key:TKey; Const Rec:String);{field of record} Virtual;
    Procedure ELoad(Const Key:TKey; Var Rec:String);{with conversion} Virtual;
    Function  Load(Const Key:TKey):String;
    Procedure Update(R:LongInt); Virtual;
    procedure CreateDB(Const fname:String;rL{reclen},numFields: word); Virtual;
    Function  MakeField(posit:Byte;Const iname:String;iwhat:Char;ilen:byte;
                        iplaces:byte;Const idxnme:String;
                        Req:TFieldReq;Sort:TSortByIndex):Boolean; Virtual;
    Function  Cover:Boolean; Virtual;
    Procedure UnCover; Virtual;
    Procedure RemoveIndex(Const Name: String); Virtual;
    Function  IsMarked:Boolean;{is in transaction?} Virtual;
    Function  ReIndex:Boolean; Virtual;
    Procedure IncNumRec;
    Function  SaveMemo(No:LongInt;Const FName:String):Boolean; Virtual;
    Function  LoadMemo(No:LongInt;Var FName:String):Boolean; Virtual;
    Function  EraseMemo(No:LongInt):Boolean; Virtual;
    Function  PackDBF: boolean; Virtual; {added by /AR/ }
    Procedure PackDBT; Virtual;
    property  CurrRec: LongInt read FDBFCurrRec;
    property  RecordsCount: LongInt read GetRecordsCount; { /VN/ }
    property  FieldsCount: Integer read FDBFCountItems; { /VN/ }
    property  Fields[Index: Integer]: TDBField read GetField; { /VN/ }
    property  FieldByName[Const Key:TKey]: TDBField read GetFieldByName; { /AR/ }
  published
    property CurrentRecDeleted : Boolean read IsCurrentRecDeleted; { changed /VN/ }
    property FileIsOpen : Boolean read FDBFIsOpened;
    property StoreByIndex : Boolean read FDBFStoreByIndex write FDBFStoreByIndex;
    property FileIsExist : Boolean read FDBFExist;
    property ReadOnly : Boolean read FDBFReadOnly write FDBFReadOnly;
    property SaveOnClose : Boolean read FDBFSaveOnClose write FDBFSaveOnClose;
    property ByIndex : String read FDBFIndex write FDBFIndex;
    property Password : String read GetPassword write SetPassword;

    property FileName     : string read FDBFName write SetFileName;
    property OnError      : TdbfError read FOnError write FOnError;
    property OnWarn       : TdbfError read FOnWarn write FOnWarn;
    property OnMakeFields : TDBFMakeItems read FOnMakeFields write FOnMakeFields;
    property OnErase      : TDBFBeforeConfirm read FOnErase write FOnErase;
    property OnOverwrite  : TDBFBeforeConfirm read FOnOverwrite write FOnOverwrite;
    property OnAdded      : TNotifyEvent read FOnAdded write FOnAdded;
    property OnAltSort    : TDBFAltSort read FDBFOnAltSort write FDBFOnAltSort;
    property OnChange     : TDBFChange read FOnChange write FOnChange;
    property OnChanged    : TNotifyEvent read FOnChanged write FOnChanged;
    property OnDelete     : TDBFConfirm read FOnDelete  write FOnDelete;
    property OnDeleted    : TNotifyEvent read FOnDeleted write FOnDeleted;
    property OnPassword   : TDBFPassword read FOnPassword write FOnPassword;
    property OnOpened     : TDBFOpened read FOnOpened write FOnOpened;
    property OnAsSigned   : TDBFAsSigned read FOnAsSigned write FOnAsSigned;
    property OnFound      : TNotifyEvent read FOnFound write FOnFound;
    property OnErased     : TNotifyEvent read FOnErased write FOnErased;
    property OnNavigate   : TDBFNavigate read FOnNavigate write FOnNavigate;
    property OnProgress   : TDBFProgress read FOnProgress write FOnProgress;
    property OnUpdate     : TNotifyEvent read FOnUpdate write FOnUpdate;
    property OnClosed     : TNotifyEvent read FOnClosed write FOnClosed;
    property OnLoaded     : TNotifyEvent read FOnLoaded write FOnLoaded;
    property OnActualize  : TDBFActualize read FOnActualize write FOnActualize;
    property OnQuery      : TDBFQuery read FOnQuery write FOnQuery;
  end;

procedure Register;

implementation

{----------------------------------------------------------------------------}
procedure Register;
{----------------------------------------------------------------------------}
begin
  RegisterComponents('Lib', [TjbDBF]);
end;

{$IFDEF VER80}
{----------------------------------------------------------------------------}
Function Trim(Const S:String):String;
{----------------------------------------------------------------------------}
Begin
  Result:=S;
  While (Length(Result)>0) And (Result[1] <= ' ') Do
    System.Delete(Result,1,1);
  While (Length(Result)>0) And (Result[Length(Result)] <= ' ') Do
    System.Delete(Result,Length(Result),1);
End;
{$ENDIF}

{----------------------------------------------------------------------------}
Constructor TjbDBF.Create(AOwner : TComponent);
{----------------------------------------------------------------------------}
Begin
  Inherited Create(AOwner);
  FDBFIsOpened := False;
  FileName := '';
  FDBFBuff := Nil;
  FDBFIndexList := TStringList.Create;
  FDBFStoreByIndex := False;
  FDBFExist := False;
  FDBFReadOnly := False;
  FDBFSaveOnClose := False;
End;

{----------------------------------------------------------------------------}
Destructor TjbDBF.Destroy;
{----------------------------------------------------------------------------}
Begin
  {kdyby byla nahodou tabulka otevrena, tak ji explicitne uzavri}
  {when table was opened then explicit close it}
  If FDBFIsOpened Then
    close;
  {uvolni instanci seznamu}
  {free list}
  FDBFIndexList.Free;
  Inherited destroy;
End;

{----------------------------------------------------------------------------}
procedure TjbDBF.SetFileName(name : string);
{----------------------------------------------------------------------------}
Begin
  {kdyby byla nahodou tabulka otevrena, tak ji explicitne uzavri}
  {when table was opened then explicit close it}
  If FDBFIsOpened Then
    Close;
  {a pak zmen jmeno}
  {and change name after them}
  FDBFName:=name;
  {prirazeni jmena oznam, a nabidni k pripadne zmene zvenku}
  {give a message if name assigned for possible outside change}
  If AsSigned(FOnAsSigned) Then FOnAsSigned(Self, FDBFName);
  {$IFNDEF LINUX} { /AR/ }
    FDBFName:=LowerCase(FDBFName);
  {$ENDIF}
  {test, zda tabulka existuje, neexistuje-li, bude potreba ji zalozit}
  {test for table exists - when not exists, create it latter}
  FDBFExist:=FileExists(FDBFName);
End;

{----------------------------------------------------------------------------}
Function TjbDBF.GetPassword: String;
{----------------------------------------------------------------------------}
Begin
  Result := LowerCase(FDBFPassword)
End;

{----------------------------------------------------------------------------}
procedure TjbDBF.SetPassword(Const thepassword:String);
{----------------------------------------------------------------------------}
Begin
  FDBFPassword := thepassword
End;

{----------------------------------------------------------------------------}
Function TjbDBF.Open:Boolean;
{----------------------------------------------------------------------------}
var
  Temp : TdbField;
  Done : Boolean;
  Readed:{$IfDef VER80}Word{$Else}Integer{$EndIf};
  Pass : String;
begin
  Result := False;
  {kdyby nahodou byla tabulka otevrena, tak ji zavri}
  {sloce table}
  If FDBFIsOpened Then Close;
  {neexistuje-li tabulka, nedelej nic}
  {when table not exists, do nothing}
  If Not FDBFExist Then Exit;

  done:=FALSE;

  If AsSigned(FOnPassword) Then FOnPassword(Self,Pass);
  {prikryti heslem je mozne overit tady}
  {cover by password validate here}
  If FDBFPassword<>'' Then
    If Pass<>FDBFPassword Then Begin
      Fatal(msgNoPasswordMatch);
      Exit;
    End;
  {otevreni pres handle}
  {open through handle}
  AsSignFile(FDBFHandle,FDBFName);
  Try
    Reset(FDBFHandle,1);
    {kdyz se povedlo, zustane otevrena az do close}
    {if success, stay open to close}
    FDBFIsOpened:=True;
    {vyzvedni si header}
    {get header}
    BlockRead(FDBFHandle,FDBFHeader,sizeof(TDBFHeader)); { Get the header }
    {tohle bude pracovni buffer, kam budes davat data}
    {working data buffer is here}
    GetMem(FDBFBuff,FDBFHeader.RecLen);
    {tady ctes polozky/sloupce a v tehle promenn vzdy budou}
    {reading field here}
    FDBFCountItems:=0;
    Repeat
      {cti obezretne, co kdybys narazil na neocekacany konec}
      {read circumspection what about giv unexpected end of file }
      {$I-}
      BlockRead(FDBFHandle,temp,SizeOf(TdbField),Readed);
      {$I+}
      If Temp.name[0]<>#$0D then
        Begin
          {ukazuj na prvni volny}
          {show first free}
          inc(FDBFCountItems);
          FDBFFields[FDBFCountItems]:=temp;
          fillchar(temp,SizeOf(temp),0);
        End
      Else
        Begin
          done:=TRUE;
          {jsou-li nacteny prave dva znaky, tabulka je prazdna, uprav pozici}
          {when two chars readed, table is empty, correct position}
          If readed=2 Then
            System.Seek(FDBFHandle,System.FilePos(FDBFHandle)-1)
          {jinak se postav na prvni zaznam a nacti ho do bufferu}
          {other stay on first record and read it into buffer}
          Else seek(0);
        End;
    Until DONE;
    {seek(0);}
    If AsSigned(FOnOpened) Then FOnOpened(Self,FDBFIsOpened);
    Result := True;
  Except
    Fatal(msgErrorOnOpen)
  End;
End;

{----------------------------------------------------------------------------}
procedure TjbDBF.Close;
{----------------------------------------------------------------------------}
Var B:Byte;
Begin
  {nasleduje test EOF mark a oznaceni kdyz chybi}
  {tohle ale lze udelat v pripade, kdyz soubor neni read only}
  {je-li read only, zahlasi se chyba a nejde soubor opravit}
  {follow EOF mark test; if missing}
  {do it when isn't file read-only only}
  {if read only get error message and do not repair it}

  System.Seek(FDBFHandle,FileSize(FDBFHandle)-1);
  Blockread(FDBFHandle,b,1);
  If B<>$1A Then Begin
    If Not FDBFReadOnly And Not IsMarked Then
      Begin
        {je-li povoleno stouchni tam posledni zaznam}
        {when consented, poke last record there}
        If FDBFSaveOnClose Then Write(CurrRec);
        B:=$1A;
        BlockWrite(FDBFHandle,B,1);
      End
    Else
      Fatal(msgEOFmarkMiss)
  End;
  CloseFile(FDBFHandle);
  {date of actualization}
  if FileMode<>0 then {[AR]}
    Actualization;
  FDBFIsOpened:=False; {message - file closed}
  If AsSigned(FOnClosed) Then FOnClosed(Self);
  FreeMem(FDBFBuff,FDBFHeader.RecLen);{ free allocated buffer}
end;

{----------------------------------------------------------------------------}
Function TjbDBF.Write(R : LongInt):TStatusWrite;
{hlavni funkce zapisu, data jsou vzdy ukladana na pozadani}
{main function for write, data store for request}
{----------------------------------------------------------------------------}
Var Cancel:Boolean;
Begin
  Result := dbfError;
  {zapis muze byt proveden pouze v pripade ze neni jen pro cteni, existuje a je otevren}
  {write can be do only when isn't read-only or exists or is opened}
  If FDBFReadOnly Or Not FDBFExist Or Not FDBFIsOpened Then Exit;
  {nastav pro pripad, kdyby to pouzival jiny proces}
  {set for occur, if it use other process}
  Result := dbfBusy;
  {je nastaveno navesti transakce, tj. pouziva to nekdo jiny}
  {is set signal label of transaction -> use it another process}
  If Not IsMarked Then Begin
    {ale ted ho chces pouzit ty}
    {but now it want use you}
    Cover;
    Try
      {priznak storna}
      {cancel prompt}
      Result := dbfCancel;
      Cancel := False;
      If FDBFCurrRec <> R Then FDBFCurrRec := R;
      {kdykoliv zapisujes, pak menis zaznam; zde ho lze odvolat}
      {write any time, you change record -> you can cancel here}
      If AsSigned(FOnChange) Then FOnChange(Self, Cancel);
      {je-li zaznam odvolan, zapis nebude proveden}
      {when record canceled, no write}
      If Cancel Then Exit;
      {priznak chyby}
      {error prompt}
      Result := dbfError;
      {pokousis se updatovat indexy}
      {you can update of indexes}
      Try
        {jsou-li updatovany}
        {if updated now}
        If UpdateIndexes(R) Then Begin
          Try
            {vyhledej fyzicky zaznam k prepisu}
            {search physical record for overwrite}
            System.Seek(FDBFHandle,R * FDBFHeader.recLen + FDBFHeader.headLen);
            {nastav signal pro platny zapis - zaznam je platny}
            {set prompt for true write -> record is OK}
            FDBFBuff^[0]:=' '; { Record not deleted! }         {uncomment /AR/ }
            {zapis ho na vyhledane misto}
            {write it to found place}
            BlockWrite(FDBFHandle,FDBFBuff^,FDBFHeader.RecLen);

            Actualization;
            {teprve tady je vsechno OK}
            {only here is OK}
            Result := dbfOK;
          Except
            On EInOutError Do Begin
              Fatal(msgErrorOnWrite);
              Result := dbfError;
            End;
          End;
          {zahlas, zes zaznam zmenil}
          {get message - record is changed}
          If AsSigned(FOnChanged) Then FOnChanged(Self);
        End;
      Except
        {v pripade vyskytu nejake chyby je ale musis odstranit}
        {but when error ocurred, have to remove all}
        RemoveIndexes(R)
      End
    Finally
      {a tady ho zase mohou pouzivat jini}
      {and there can use it others}
      UnCover;
    End;
  End;
End;

{----------------------------------------------------------------------------}
Function TjbDBF.Delete(R : longint):TStatusWrite;
{----------------------------------------------------------------------------}
Var Confirm : Boolean;
Begin
  Result:=dbfError;
  {zapis muze byt proveden pouze v pripade ze neni jen pro cteni, existuje a je otevren}
  {write can be do only when isn't read-only or exists or is opened}
  If FDBFReadOnly Or Not FDBFExist Or Not FDBFIsOpened Then Exit;
  Result:=dbfBusy;
  If Not IsMarked Then Begin
    Cover;
    Try
      {zadej o svoleni s vymazanim vety}
      {require consent record delete}
      If AsSigned(FOnDelete) Then FOnDelete(Self, Confirm) Else Confirm := True;
      If Confirm Then
      Begin
        Try
          {nezmenil-li se zaznam od aktualniho}
          {when actual record is the same as required}
          If FDBFCurrRec <> R Then FDBFCurrRec := R;
          {vyhledej ho v zaznamech}
          {seek new position}
          System.Seek(FDBFHandle,R * FDBFHeader.recLen + FDBFHeader.headLen);
          {nastav priznak vymazani}
          {set erase label }
          FDBFBuff^[0]:=DeleteFlag; { Record is deleted! }
          {zapis do souboru}
          {write it into file}
          BlockWrite(FDBFHandle,FDBFBuff^,FDBFHeader.recLen);
          {aktualizuj indexy, tj. odstran z nich vymazany zaznam}
          {and do index actualizing }
          RemoveIndexes(R);

          Actualization;
          Result:=dbfOk;  { /AR/ }
        Except
          On EInOutError Do Fatal(msgCannotDeleteItem);
        End;
        {oznam zes vymazal}
        {and get message}
        If AsSigned(FOnDeleted) Then FOnDeleted(Self);
      End;
    Finally
      UnCover;
    End
  End;
End;

{----------------------------------------------------------------------------}
procedure TjbDBF.Seek(R : LongInt);
{----------------------------------------------------------------------------}
var
  L      : LongInt;
  Readed : {$IfDef VER80}Word{$Else}Integer{$EndIf};
begin
  If Not FDBFExist Or Not FDBFIsOpened Then Exit;
  {nezmenil-li se zaznam od aktualniho}
  {when actual record is the same as required}
  If FDBFCurrRec <> R Then FDBFCurrRec := R;
  {fyzicka delka zacatku vet}
  {physical size of record begins}
  L := R * FDBFHeader.recLen + FDBFHeader.headLen;
  {kdyz je nahodou za}
  {when beyond}
  If L > (FileSize(FDBFHandle)-1) Then Exit;
  {postav se tam}
  {stay here}
  System.Seek(FDBFHandle, L);
  {precti vetu do bufferu}
  {read record into buffer}
  BlockRead(FDBFHandle, FDBFBuff^, FDBFHeader.RecLen, Readed);
  {veta je uspesne nactena, jen kdyz je v bufferu cela}
  {when all readed}
  If FDBFHeader.RecLen = Readed Then
    {a zahlas zes ji precetl}
    {get message}
    If AsSigned(FOnLoaded) Then FOnLoaded(Self);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.GotoStart;
{----------------------------------------------------------------------------}
begin
  If Not FDBFExist Or Not FDBFIsOpened Then Exit;
  {nastav se na prvni zaznam}
  {seek to first}
  Seek(0);
  {zahlas pro navigaci, ze na nem stojis}
  {and get message for navigation your position}
  If AsSigned(FOnNavigate) Then FOnNavigate(Self, 0);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.GotoEnd;
{----------------------------------------------------------------------------}
begin
  If Not FDBFExist Or Not FDBFIsOpened Then Exit;
  {nastav se na posledni zaznam}
  {seek to last}
  Seek(FDBFHeader.numRecs-1);
  {zahlas pro navigaci, ze na nem stojis}
  {and get message for navigation your position}
  If AsSigned(FOnNavigate) Then FOnNavigate(Self, FDBFHeader.numRecs-1);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.GotoNext;
{----------------------------------------------------------------------------}
begin
  If Not FDBFExist Or Not FDBFIsOpened Then Exit;
  {nastav se na nasledujici zaznam}
  {seek to next}
  Seek(FDBFCurrRec+1);
  {zahlas pro navigaci, ze na nem stojis}
  {and get message for navigation your position}
  If AsSigned(FOnNavigate) Then FOnNavigate(Self, FDBFCurrRec+1);
end;

{----------------------------------------------------------------------------}
Procedure TjbDBF.GotoPrev;
{----------------------------------------------------------------------------}
begin
  If Not FDBFExist Or Not FDBFIsOpened Then Exit;
  {nastav se na predchazejici zaznam}
  {seek to previous}
  Seek(FDBFCurrRec-1);
  {zahlas pro navigaci, ze na nem stojis}
  {and get message for navigation your position}
  If AsSigned(FOnNavigate) Then FOnNavigate(Self, FDBFCurrRec-1);
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.NewRecord;
{----------------------------------------------------------------------------}
begin
  {nemuzes nic pridavat, kdyz je jen ke cteni, nebo neexistuje, neni otevren}
  {cannot do when is read-only or no exists or is closed}
  If FDBFReadonly Or Not FDBFExist Or Not FDBFIsOpened Then Exit;
  If Not IsMarked Then Begin
    Cover;
    Try
      {vycisti buffer}
      {clear buffer}
      {FillChar(FDBFBuff^,FDBFHeader.RecLen,' ');}
      {zde je mozne udelat implicitni naplneni zaznamu, coz vrele doporucuji}
      {can do implicit fill of record (I recommend to)}
      If AsSigned(FOnActualize) Then FOnActualize(Self,dbfNew);
      Try
        {zvyz pocet zaznamu a uloz je do hlavicky}
        {increment count of records and save it}
        IncNumRec;
        {jdi na fyzicky zacatek}
        {go to start}
        System.Seek(FDBFHandle,0);
        {zapis hlavicku}
        {write header}
        BlockWrite(FDBFHandle,FDBFHeader,SizeOf(TDBFHeader));
        {zapis-vloz novy zaznam na konec}
        {write-insert new record to end}
        Write(FDBFHeader.numRecs-1);
        {nastav se na ten zaznam a aktualizuj buffer}
        {set position to new record and do buffer actual}
        Seek(FDBFHeader.numRecs-1);
      Except
        On EInOutError Do Fatal(msgCannotAddItem);
      End;
      {a pripadne zahlas, ze zaznam byl pridan}
      {and get message when added}
      If AsSigned(FOnAdded) Then FOnAdded(Self);
    Finally
      UnCover;
    End;
  End;
end;

{----------------------------------------------------------------------------}
procedure TjbDBF.CreateDB(Const fname:String;rL{reclen},numFields: word);
{----------------------------------------------------------------------------}
Var
  y,m,d : Word;
  c:Char;
  i,j:Byte;
Begin
  {kdyby byla nahodou tabulka otevrena, tak ji explicitne uzavri}
  {close table}
  If FDBFIsOpened Then
    Close;

  {vytvatis novou tabulku, zde je hlavicka}
  {for new table refill header}
  FillChar(FDBFHeader,SizeOf(FDBFHeader),0);
  With FDBFHeader Do Begin
    version:=$3;
    DecodeDate(Date,y,m,d); {create date}
    year:=y Mod 100;
    month:=Lo(m);
    day:=Lo(d);
    numRecs:=0;
    headLen:=SizeOf(FDBFHeader)+SizeOf(TDBFHeader) * numFields + 1;
    recLen:=rl + 1; {begins delete flag}
  End;
  {tohle je nove jmeno tabulky}
  {new table name}
  FDBFName:=fname;
  {priprav ji k fyzickemu zalozeni}
  {prepare it for physical store}
  AsSignFile(FDBFHandle,FDBFName);
  Try
    ReWrite(FDBFHandle,1);
    Try
      {zalozeni se povedlo, tabulka je otevrena}
      {create id OK, table wil be open}
      FDBFIsOpened:=TRUE;
      {zapis hlavicku}
      {write header}
      BlockWrite(FDBFHandle,FDBFHeader,sizeof(TDBFHeader));
      {pro stanoveny pocet sloupcu prochazej}
      {go by columns}
      For i:=1 To numFields Do Begin
        {jestlize je attachnuty event pro vyrobu pole on line tak ho zavolej}
        {jinak je predpokladano ze pred volanim teto metody byly
         vytvoreny sloupce pomoci MakeField()}
        {on line create field}
        {else before create vas MakeField called}
        If AsSigned(FOnMakeFields) Then Begin
          {zavolej ho tolikrat, kolik je potreba vyrobit poli}
          {call by columns count}
          FillChar(FDBFFields[i],SizeOf(FDBFFields[i]),0);
          {zavolej ho a vyrob zaznam}
          {make field here}
          With FDBFFields[I] Do
            FOnMakeFields(Self,I,Name,What,Len,Places,idx,idxtyp,idxsrt);
          {uprav na velka pismena}
          {upper case only please}
          For j:=0 to 10 Do FDBFFields[i].Name[j]:=UpCase(FDBFFields[i].Name[j]);
          FDBFFields[i].What := UpCase(FDBFFields[i].What);
        End;
        {zapis nove vyrobene pole}
        {write new made field}
        BlockWrite(FDBFHandle,FDBFFields[i],sizeof(TdbField))
      End;
      {za hlavickou nasleduje vzdy CR}
      {over header poke CR mark}
      c:=#$0D;
      BlockWrite(FDBFHandle,c,1);
      {konec souboru je indikovan EOF mark}
      {end of file poke EOF mark}
      c:=#26;
      BlockWrite(FDBFHandle,c,1);
    Finally
      {tady soubor fyzicky zavri}
      {and here file physicaly close}
      CloseFile(FDBFHandle);
    End;
  Except
    {ejhle, chyba; tak ji zahlas}
    {ooh, error -> have to message}
    Fatal(msgCreateDBFError)
  End;
  {tabulka je stale uzavrena}
  {table still close}
  FDBFIsOpened:=False;
End;

{----------------------------------------------------------------------------}
Function  TjbDBF.MakeField( posit:Byte;
                          Const iname:String;
                          iwhat:Char;
                          ilen:byte;
                          iplaces:byte;
                          Const idxnme:String;{filename xxxxxxxxIDX}
                          Req:TFieldReq;
                          Sort:TSortByIndex
                        ):Boolean;
{----------------------------------------------------------------------------}
Var
  I:byte;
  S:String;
  X:String[8];
Begin
  Result := False;
  If (Trim(IName)='') Or Not(UpCase(IWhat) in TdbTypes) Or (ILen = 0) Then Begin
    Fatal(msgBadDefinitionOfField);
    Exit;
  End;
  Result := True;
  FillChar(FDBFFields[posit],SizeOf(FDBFFields[posit]),0);
  With FDBFFields[posit] Do Begin
    {prvnich 11 znaku, velka pismena}
    {first 11 chars, uppercase please}
    S:=Copy(UpperCase(Trim(IName)),1,11);
    Move(S[1],Name,Length(S));
    What   := UpCase(IWhat);
    {tyhle polozky (cas, datum, memo) maji fixni tvar}
    {format is fixed (time, date, memo...)}
    Case What of
      'T': Len := 6;
      'D': Len := 8;
      'M': Len := 10;
      'F':
      Begin
        Len := iLen; {bugfix by Jarda Jirava [<mailto:listuj@centrum.cz>]  18.4.2001}
        Places := IPlaces; {tohle je jenom pro float/float only}
      End;

    Else
      Len    := ILen;
    End;

    If (IdxNme<>'') Then Begin
      I:=Pos('.',IdxNme);
      If I>0 Then Begin
        S:=Trim(Copy(IdxNme,I+1,3));
        If Length(S)<3 Then While Length(S)<3 Do S:=S+SpacerD;
        X:= Trim(Copy(IdxNme,1,I-1));
        If Length(X)<8 Then While Length(X)<8 Do X:=SpacerD+X;
        S:=X+S;
      End
      Else Begin
        X:=Trim(IdxNme);
        If Length(X)<8 Then While Length(X)<8 Do X:=SpacerD+X;
        S:=X+'IDX';
      End;
      S:=UpperCase(S);
      Move(S[1],idx,11);
      idxtyp := Req; { /AR/ }
      idxsrt := Sort; { /AR/ }
    End;
  End;
End;

{----------------------------------------------------------------------------}
Procedure TjbDBF.Fatal(Const Msg:String);
{----------------------------------------------------------------------------}
Begin
  {kdyz je vnejsi zpracovani msg, tak ho zavolej, jinak ukaz vlastni}
  {outside messages}
  If AsSigned(FOnError) Then FOnError(Self,msg)
  Else
    {inside messages}
    MessageDlg(msg, mtError, [mbOk], 0);
End;

{----------------------------------------------------------------------------}
Procedure TjbDBF.Warn(Const Msg:String);
{----------------------------------------------------------------------------}
Begin
  {s varovanim je to stejne tak}
  {inside/outside}
  If AsSigned(FOnWarn) Then FOnWarn(Self,msg)
  Else
    MessageDlg(msg, mtWarning, [mbOk], 0);
End;

{----------------------------------------------------------------------------}
Function  TjbDBF.UpdateIndexes(R:LongInt):Boolean;
{----------------------------------------------------------------------------}
Var
  I,J,L:LongInt;
  TempFName:String;
  F:File;
  T:TStringList;
  S,X:String;
  UpdateField:Boolean;
Begin
  {indikace uspesneho  ukonceni}
  {all OK}
  Result := True;
  {indexy se aktualizuji zde, ale jen kdyz to chces}
  {can you actualise index ?}
  If Not FDBFStoreByIndex Then Exit;
  {doslo-li k uspesnemu updatu, funkce vrati True jinak udela removeindexes}
  {if NOT OK then remove indexes}
  For I:=1 To FDBFCountItems Do Begin
    {prochazis vsechny sloupce a hledas indexovy soubor}
    {go through columns and search index file}
    If Trim(FDBFFields[I].Idx)<>'' Then Begin
      {indexovy soubor byl nalezen}
      {jeho jmeno je ve tvaru xxxxxxxxIDX, vzdy zarovnan vpravo}
      {vyplne jsou SpacerD tj. "~~~JMENOIDX" nebo "~~ZAMESTID~"}
      {vyrob temp jmeno souboru}
      {found, format index filename}
      TempFName:=ExtractFilePath(FDBFName)
       +Trim(Copy(FDBFFields[I].Idx,1,8)+'.'+Copy(FDBFFields[I].Idx,9,3));
      If FileExists(TempFName) Then Begin
        AsSignFile(F,TempFName);
        Try
          ReSet(F,1);
          Try
            UpdateField:=False;
            {proc nepouzit k indexum stringlist?}
            {why don't use stringlist?}
            T:=TStringList.Create;
            Try
              {budes tridit radky}
              {will be assort lines}
              T.Sorted:=True;
              {nastavujes vlastnost duplicit}
              {and property duplicit by type of index}
              Case FDBFFields[I].idxtyp Of
                dbfUnique:T.Duplicates := dupError;
                dbfDuplicates:T.Duplicates := dupAccept;
              End;
              While Not Eof(F) Do Begin
                {uprava indexu je zde}
                {adapt index here}
                BlockRead(F,S[1],FDBFFields[I].len+SizeOf(L));
                Move(S[1],L,SizeOf(L));
                {neni tam nahodou uz nektery k uprave?}
                {if adapted?}
                If R=L Then Begin
                  ELoad(FDBFFields[I].Name,X);
                  Move(X[1],S[5],Length(X));{zkus ho tam pridat}
                  UpdateField:=True;
                End;
                Try
                  T.Add(' '+Copy(S,5,255)+#1+IntToStr(L));
                Except
                  On EListError Do
                    If T.Duplicates = dupError Then Begin
                      Warn(msgDuplicateInUnique);
                      Result:=False;
                      Exit;
                    End;
                End;
              End{while};
              {vlozil jsi vsechny ze souboru, tak ted zkus primy}
              {all added, try direct}
              If Not UpdateField Then Begin
                ELoad(FDBFFields[I].Name,X);
                Try
                  T.Add(' '+X+#1+IntToStr(R));
                Except
                  On EListError Do
                    If T.Duplicates = dupError Then Begin
                      Warn(msgDuplicateInUnique);
                      Result:=False;
                      Exit;
                    End;
                End;
              End;
              {byl-li index uspesne vlozen, uloz indexovy soubor}
              {when index is OK, save as file}
              ReWrite(F,1);{vymaz puvodni}
              Case FDBFFields[I].idxsrt Of
                dbfAscending:
                Begin
                  For J := 0 To T.Count-1 Do Begin
                    S:=T.Strings[J];
                    L:=StrToInt(Copy(S,Pos(#1,S)+1,255));
                    S:='   '+Copy(S,1,Pos(#1,S)-1);
                    Move(L,S[1],SizeOf(L));
                    BlockWrite(F,S[1],Length(S));
                  End;
                End;
                dbfDescending:
                Begin
                  For J := T.Count - 1 DownTo 0 Do Begin
                    S:=T.Strings[J];
                    L:=StrToInt(Copy(S,Pos(#1,S)+1,255));
                    S:='   '+Copy(S,1,Pos(#1,S)-1);
                    Move(L,S[1],SizeOf(L));
                    BlockWrite(F,S[1],Length(S));
                  End;
                End;
                dbfAlternative:
                Begin
                  {Potrebujete-li to, tak jedine doprogramovat}
                  {if you want you have to coplete do it}
                  If AsSigned(FDBFOnAltSort) Then FDBFOnAltSort(Self,T);
                  {a uloz to ...}
                  {and save it...}
                  For J:=0 To T.Count-1 Do Begin
                    S:=T.Strings[J];
                    L:=StrToInt(Copy(S,Pos(#1,S)+1,255));
                    S:='   '+Copy(S,1,Pos(#1,S)-1);
                    Move(L,S[1],SizeOf(L));
                    BlockWrite(F,S[1],Length(S));
                  End;
                End;
              End;
            Finally
              T.Free {zahod ho} {throw off}
            End;
            {idxsrt:TSortByIndex = (dbfAscending, dbfDescending, dbfAlternative);}
          Finally
            CloseFile(F);
          End;
        Except
          Fatal(Format(msgIdxTableNotFound,[ExtractFileName(TempFName)]));
        End;
      End
      Else Begin
        AsSignFile(F,TempFName);
        {tabulka jeste neexistuje}
        {table doesn't exist}
        ReWrite(F,1);
        ELoad(FDBFFields[I].Name,X);
        Move(R,S[1],SizeOf(R));
        Move(X[1],S[5],Length(X));{try it add there}
        BlockWrite(F,S[1],Length(X)+SizeOf(R));
        CloseFile(F);
      End;
    End;
  End;
End;

{----------------------------------------------------------------------------}
Procedure TjbDBF.RemoveIndexes(R:LongInt);
{----------------------------------------------------------------------------}
Var
  I:Integer;
  L:LongInt;
  TempFName,S:String;
  F,Fnew:File;
Begin
  {je-li nejaky zaznam vymazan, musi se tez odstranit ze vsech indexu}
  {when is some record deleted, have to delete from all indexes too}
  For I:=1 To FDBFCountItems Do Begin
    {prochazis vsechny sloupce a hledas indexovy soubor}
    {for through columns}
    If Trim(FDBFFields[I].Idx)<>'' Then Begin
      {indexovy soubor byl nalezen}
      {jeho jmeno je ve tvaru xxxxxxxxIDX, vzdy zarovnan vpravo}
      {vyplne jsou SpacerD tj. "~~~JMENOIDX" nebo "~~ZAMESTID~"}
      {vyrob temp jmeno souboru}
      {found, format name}
      TempFName:=ExtractFilePath(FDBFName)
       +Trim(Copy(FDBFFields[I].Idx,1,8)+'.'+Copy(FDBFFields[I].Idx,9,3));
      AsSignFile(F,TempFName);
      Try
        ReSet(F,1);
        Try
          AsSignFile(Fnew,ChangeFileExt(TempFName,'.$$$'));
          Try
            ReWrite(Fnew,1);
            Try
            While Not Eof(f) Do Begin
              BlockRead(F,S[1],FDBFFields[I].len+SizeOf(L));
              Move(S[1],L,SizeOf(L));
              If L<>R Then Begin
                BlockWrite(Fnew,S[1],FDBFFields[I].len+SizeOf(L))
              End;
            End;
            Finally
              CloseFile(Fnew);
            End;
          Except
            Warn(msgNotEnoughtCreateIdx)
          End;
        Finally
          CloseFile(F);
          If FileExists(ChangeFileExt(TempFName,'.$$$')) Then Begin
            DeleteFile(TempFName);
            RenameFile(ChangeFileExt(TempFName,'.$$$'),TempFName)
          End;
        End;
      Except
        Fatal(Format(msgIdxTableNotFound,[ExtractFileName(TempFName)]));
      End;
    End;
  End;
End;

{----------------------------------------------------------------------------}
procedure TjbDBF.MakeIndex(Const IdxName:String; Const Key:TKey);
{vytvori index}
{Make the index}
{----------------------------------------------------------------------------}
Var
  F:File;
  I,L:LongInt;
  A,B,FLD:Integer;
  S:String;
  T:TStringList;
Begin
  {Musi existovat a byt otevrena neprazdna tabulka, lze pouzit i pro preindexovani}
  {Unempty table have to exist (and reindexing too)}
  If Not (FDBFExist And  FDBFIsOpened And (FDBFHeader.NumRecs>0)) Then Exit;
  {Otevri ji na stejnem miste, pouzij idxname = cele jmeno souboru}
  {Open it here, use idxname = full file name}
  AsSignFile(F,IdxName);
  Try
    Rewrite(F,1);
    Try
      B := 0;A := -1;
      {do teto velikosti to lze setridit pres stringlist jinak per partes}
      {there is limit of stringlist for 16 bit Delphi}
      If FDBFHeader.NumRecs<MaxItems Then Begin
        T:=TStringList.Create;
        Try
          {budes tridit radky}
          {lines sorting}
          T.Sorted:=True;
          For FLD:=1 To FDBFCountItems Do
            If Trim(FDBFFields[FLD].Name)=Key Then Break;
          {nastavujes vlastnost duplicit}
          {property duplicates}
          Case FDBFFields[FLD].idxtyp Of
            dbfUnique:T.Duplicates := dupError;
            dbfDuplicates:T.Duplicates := dupAccept;
          End;
          {projdes celou tabulku a vytahnes z ni pozadovane pole}
          {go through table}
          For I := 0 To FDBFHeader.NumRecs-1 Do Begin
            Seek(I);
            ELoad(Key,S);
            {vlozis ho i s pozici do seznamu}
            {with position}
            Try
              T.Add(' '+S+#1+IntToStr(I));
            Except
              On EListError Do
                If T.Duplicates = dupError Then Fatal(msgDuplicateInUnique);
            End;
            {aktualizujes citac - vhodne je tez nastavovat kurzor}
            {counter actualisation for gauge}
            B:=Round((I+1)/(FDBFHeader.NumRecs/100));
            If A<>B Then Begin {tohle je proto, aby se progress volal jen 101x}
              A := B;
              If AsSigned(FOnProgress) Then FOnProgress(Self,prgMakeIndexSort,B);
            End;
          End;
          {znovu projdes seznam, upravis ho do tvaru <cislo><klic> a zapises}
          {go through list, format items as <number><key> an write it}
          If AsSigned(FOnProgress) Then FOnProgress(Self,prgWriteIndexSort,B);
          Case FDBFFields[FLD].idxsrt Of
            dbfDescending:
              For I:=T.Count-1 DownTo 0 Do Begin
                S:=T.Strings[I];
                L:=StrToInt(Copy(S,Pos(#1,S)+1,255));
                S:='   '+Copy(S,1,Pos(#1,S)-1);
                Move(L,S[1],SizeOf(L));
                BlockWrite(F,S[1],Length(S));
              End;
            dbfAscending:
              For I:=0 To T.Count-1 Do Begin
                S:=T.Strings[I];
                L:=StrToInt(Copy(S,Pos(#1,S)+1,255));
                {dopredu tri mezery, jedna tam uz je}
                {fill three space before it}
                S:='   '+Copy(S,1,Pos(#1,S)-1);
                Move(L,S[1],SizeOf(L));
                BlockWrite(F,S[1],Length(S));
              End;
            dbfAlternative:
            Begin
              {Potrebujete-li to, tak jedine doprogramovat}
              {if you want you have to coplete do it}
              If AsSigned(FDBFOnAltSort) Then FDBFOnAltSort(Self,T);
              For I:=0 To T.Count-1 Do Begin
                S:=T.Strings[I];
                L:=StrToInt(Copy(S,Pos(#1,S)+1,255));
                S:='   '+Copy(S,1,Pos(#1,S)-1);
                Move(L,S[1],SizeOf(L));
                BlockWrite(F,S[1],Length(S));
              End;
            End;
          End;
        Finally
          T.Free
        End;
      End
      Else
        Warn(msgFileIsTooLarge);
    Finally
      CloseFile(F)
    End;
  Except
    Fatal(msgNotEnoughtCreateIdx)
  End;
End;

{----------------------------------------------------------------------------}
Function TjbDBF.ReIndex;
{provede reindexovani tabulky}
{reindexing of table}
{----------------------------------------------------------------------------}
Var
  I:Integer;
  TempFName:String;
Begin
  Result := False;
  {je-li tabulka zrovna otevrena, tak nedelej nic}
  {when table is opened do nothing}
  If IsMarked Then Exit;
  {nemuzes-li si ji otevrit taky pro sebe, tak taky nic nedelej}
  {if you cannot open for this (transaction?) do nothing}
  If Not Cover Then Exit;
  Try
    For I:=1 To FDBFCountItems Do Begin
      {prochazis vsechny sloupce a hledas indexovy soubor}
      {go through comumns}
      If Trim(FDBFFields[I].Idx)<>'' Then Begin
        {indexovy soubor byl nalezen}
        {jeho jmeno je ve tvaru xxxxxxxxIDX, vzdy zarovnan jako soubor 8-3}
        {vyplne jsou SpacerD tj. "~~~JMENOIDX" nebo "~~ZAMESTID~"}
        {vyrob temp jmeno souboru}
        {found, format name}
        TempFName:=ExtractFilePath(FDBFName)
         +Trim(Copy(FDBFFields[I].Idx,1,8)+'.'+Copy(FDBFFields[I].Idx,9,3));

        MakeIndex(TempFName,Trim(FDBFFields[I].Name))
      End;
    End;
  Finally
    UnCover;
    Result := True;
  End;
End;

{----------------------------------------------------------------------------}
Procedure TjbDBF.Update(R:LongInt);
{----------------------------------------------------------------------------}
Begin
  {bude-li zaznam aktualizovan tesne pred update, dej to vedet i s flagem}
  {if actualized before update get message with flag}
  If AsSigned(FOnActualize) Then FOnActualize(Self,dbfUpdate);
  {zapis zaznam z bufferu}
  {write record}
  Write(CurrRec);
  {udelej jeste obnoveni/refresh, ale asi neni uz nutne}
  {and refresh}
  Seek(CurrRec);
End;

{----------------------------------------------------------------------------}
Procedure TjbDBF.Store(Const Key:TKey; Const Rec:String);
{vlozi hodnotu do bufferu aktualniho zaznamu dle klice pole}
{save value to actual record buffer by field key}
{----------------------------------------------------------------------------}
Var
  I,Posic:Integer;
  S:String;
Begin
  {pozice zacina od jedne, i kdyz je buffer od 0 protoze na }
  {pozici 0 je indikacni byte o vymazani vety}
  {cout from 1, position 0 is flag for deleting}
  Posic := 1;
  For I := 1 To FDBFCountItems Do Begin
    If Trim(FDBFFields[I].Name) = UpperCase(Key) Then Break
    Else Inc(Posic,FDBFFields[I].Len);
  End;
  S:=Rec;
  If Length(S)<FDBFFields[I].Len Then
    Case FDBFFields[I].What of
      'C','L':While Length(S)<FDBFFields[I].Len Do S := S + ' ';
      'F','N','M':While Length(S)<FDBFFields[I].Len Do S := ' ' + S;
      'D':;{date is 8 chars only ddmmyyy or mmddyyyy}
      'T':;{time is 6 chars only hhmmss}
    End;
  Move(S[1],FDBFBuff^[Posic],FDBFFields[I].Len);
End;

{----------------------------------------------------------------------------}
Procedure TjbDBF.ELoad(Const Key:TKey; Var Rec:String);
{Precte hodnotu z bufferu aktualniho zaznamu dle klice pole}
{read value from actualrecord buffer by field key}
{----------------------------------------------------------------------------}
Var
  I,Posic:Integer;
Begin
  {pozice zacina od jedne, i kdyz je buffer od 0 protoze na }
  {pozici 0 je indikacni byte o vymazani vety}
  {cout from 1, position 0 is flag for deleting}
  Posic := 1;
  {nejprve musi najit jmeno pole a nascitat pocatek}
  {search field name and recount start of}
  For I := 1 To FDBFCountItems Do Begin
    If Trim(FDBFFields[I].Name) = UpperCase(Key) Then Break
    Else Inc(Posic,FDBFFields[I].Len);
  End;
  {predej zaznam neotrimovany}
  {add unformating record}
  Move(FDBFBuff^[Posic],Rec[1],FDBFFields[I].Len);
  Rec[0]:=Chr(FDBFFields[I].Len);
End;

{----------------------------------------------------------------------------}
Function TjbDBF.Load(Const Key:TKey):String;
{tohle je uzivatelska modifikace funkce ELoad, ktera orizne mezery}
{this formating version of ELoad}
{----------------------------------------------------------------------------}
Begin
  {vola standarni funkci}
  {call standard function}
  ELoad(Key,Result);
  {a tady jeste orizne nadbytecne mezery}
  {and trim spaces}
  Result:=Trim(Result);
End;

{----------------------------------------------------------------------------}
Procedure TjbDBF.Find(Const IdxName, Value{toto je vlastne klic}:String);
{hleda hodnotu podle klice idxname je jmeno sloupce value je hodnota
 funkcionalitu = <> > < >= <= dodava onquery}
{dearch value by key}
{more give = <> > < >= <= get onquery}
{----------------------------------------------------------------------------}
  Function SizeOfKey(Const Key:String):Integer;
  {vraci sirku klice/pole}
  Var I:Integer;
  Begin
    For I := 1 To FDBFCountItems Do Begin
      If Trim(FDBFFields[I].Name) = UpperCase(Key) Then Begin
        Result := FDBFFields[I].Len;
        Exit
      End;
    End;
    Result:=0;
    Warn(Format(msgIdxFieldNotFound,[Key]));
  End;
Var
  F:File;
  A,B,Size:Integer;
  S:String;
  I,N,L:LongInt;
  OK, Cancel:Boolean;
Begin
  {Musi existovat a byt otevrena neprazdna tabulka, lze pouzit i pro preindexovani}
  {opened and unempty table}
  If Not (FDBFExist And  FDBFIsOpened And (FDBFHeader.NumRecs>0)) Then Exit;
  {tato procedura musi byt attachnuta, aby mohl dotaz fungovat}
  {OnQuery must be attach for good work !!!}
  If Not AsSigned(FOnQuery) Then Exit;
  {tahle taky, aby se dala predavat data}
  {and OnFound must be attach for good work too!!!}
  If Not AsSigned(FOnFound) Then Exit;
  {Otevri ji na stejnem miste, pouzij idxname a defaultni priponu}
  {open index}
  AsSignFile(F,ExtractFilePath(FDBFName)+IdxName);
  Try
    ReSet(F,1);
    Try
      B :=SizeOfKey(Value);
      {v tomto pripade vydano varovani a odchod, klic musi byt nenulovy}
      {key have to non zero}
      If B=0 Then Exit;
      {sestaveni zaznamu}
      {build record}
      Size:=B+SizeOf(LongInt);
      {overeni na velikost souboru}
      {size of file for align}
      I := FileSize(F);
      {v pripade ze neco zbyde (polozky nejsou align) tak chyba}
      {fatal error occurr when non align}
      If (I Mod Size) <> 0 Then Begin
        Fatal(Format(msgErrorOnIdxTable,[IdxName]));
        Exit;
      End;
      {tohle je pocet polozek}
      {count of records}
      N := I Div Size;
      {nastav prostor na S}
      {make place for it}
      S:='';For I := 1 To Size Do S:=S+' ';
      Cancel:=False;
      {prochazej periodicky klic}
      {go by key}
      B := 0; A := -1;
      For I := 0 To N-1 Do Begin
        BlockRead(F,S[1],Size);
        S[0]:=Chr(Size);
        Move(S[1],L,SizeOf(L));
        S:=Copy(S,5,255);
        {zde je dotaz na tabulku, uzivatel filtruje dle pole}
        {query to table, user do filtering}
        {no accept}
        OK:=False;{predpoklad, ze ho nechci}
        FOnQuery(Self,IdxName,Value,Trim(S),OK, Cancel);
        If OK Then Begin
          {je-li pozadovany filtr akceptovan, vyzvedni zaznam}
          {when accept, get record from table}
          Seek(L);
          {zaznam se musi zpracovat, jinak jsou data ztracena}
          {zde se data ctou napr. do listboxu nebo stringgridu}
          {record must be worked but data throw off}
          {may be read to list ??}
          FOnFound(Self);
        End;
        {aktualizujes citac - vhodne je tez nastavovat kurzor}
        {for gauge}
        B:=Round((I+1)/(N/100));
        If Cancel Then B:=100;
        If A<>B Then Begin {tohle je proto, aby se progress volal jen 101x}
          A := B;
          If AsSigned(FOnProgress) Then FOnProgress(Self,prgSearchByKey,B);
        End;
        If Cancel Then Break;
      End;
    Finally
      CloseFile(F)
    End;
  Except
    Fatal(Format(msgIdxTableNotFound,[ExtractFilePath(FDBFName)+IdxName]));
  End;
End;

{----------------------------------------------------------------------------}
Function TjbDBF.Cover:Boolean;
{nastavuje bit transakce}
{set flag for transaction}
{----------------------------------------------------------------------------}
Var
  F: File;
  B: Byte;
Begin
  Result := False;
  If IsMarked Then Exit;
  AssignFile(F, FDBFName);
  Try
    Reset(F, 1);
    Try
      B:=1;
      System.Seek(F, 14);
      BlockWrite(F, B, 1);
      Result := True;
    Finally
      CloseFile(F);
    End;
  Except
    On EInOutError Do Warn(msgFileTooRemote);
  End;
End;

{----------------------------------------------------------------------------}
Procedure TjbDBF.UnCover;
{shazuje bit transakce}
{reset flag for transaction}
{----------------------------------------------------------------------------}
Var
  F: File;
  B: Byte;
Begin
  If Not IsMarked Then Exit;
  AssignFile(F, FDBFName);
  Try
    Reset(F, 1);
    Try
      B:=0;
      System.Seek(F, 14);
      BlockWrite(F, B, 1);
    Finally
      CloseFile(F);
    End;
  Except
    On EInOutError Do Warn(msgFileTooRemote);
  End;
End;

{----------------------------------------------------------------------------}
procedure TjbDBF.RemoveIndex(Const Name: String);
{explicitni zruseni indexu .mdx a zruseni propojeni}
{explicit delete of index and erase link}
{----------------------------------------------------------------------------}
Var
  F: File;
  B: Byte;
Begin
  AssignFile(F, FDBFName);
  Try
    Reset(F, 1);
    Try
      System.Seek(F, 28);
      B := 0;
      BlockWrite(F, B, 1);
      DeleteFile(ChangeFileExt(Name, '.mdx'));
    Finally
      CloseFile(F);
    End;
  Except
    On EInOutError Do Warn(msgCannotOpenTable)
  End;
End;

{----------------------------------------------------------------------------}
Function TjbDBF.IsMarked:Boolean;
{vraci priznak, zda je nastavena transakce}
{get flag that transaction is of/off}
{----------------------------------------------------------------------------}
Var
  F: File;
  B: Byte;
Begin
  Result := True;
  AssignFile(F, FDBFName);
  Try
    Reset(F, 1);
    Try
      System.Seek(F, 14);
      BlockRead(F, B, 1);
      Result := B = 1;
    Finally
      CloseFile(F);
    End;
  Except
    On EInOutError Do Warn(msgFileTooRemote);
  End;
End;

{----------------------------------------------------------------------------}
Procedure TjbDBF.IncNumRec;
{procedura zvysi pocet zaznamu o jeden}
{increment count of records +1}
{----------------------------------------------------------------------------}
Var
  F: File;
  L: LongInt;
Begin
  AssignFile(F, FDBFName);
  Try
    Reset(F, 1);
    Try
      System.Seek(F, 4);
      BlockRead(F, L, SizeOf(L));
      Inc(L);
      FDBFHeader.numRecs:=L;
      System.Seek(F, 4);
      BlockWrite(F, L, SizeOf(L));
    Finally
      CloseFile(F);
    End;
  Except
    On EInOutError Do Warn(msgFileTooRemote);
  End;
End;

{----------------------------------------------------------------------------}
Procedure TjbDBF.Actualization;
{zapis do hlavicky aktualni datum}
{write to header of dbf actual date and time}
{----------------------------------------------------------------------------}
Var
  F: File;
  S: String[3];
  Year, Month, Day: Word;
Begin
  AssignFile(F, FDBFName);
  Try
    Reset(F, 1);
    Try
      System.Seek(F, 1);
      BlockRead(F, S[1], 3);
      DecodeDate(Date, Year, Month, Day);
      Byte(S[1]):=(Year Mod 100);
      Byte(S[2]):=Month;
      Byte(S[3]):=Day;
      Move(S[1],FDBFHeader.Year,3);
      System.Seek(F, 1);
      BlockWrite(F, S[1], 3);
    Finally
      CloseFile(F);
    End;
  Except
    On EInOutError Do Warn(msgFileTooRemote);
  End;
End;

{----------------------------------------------------------------------------}
Function TjbDBF.SaveMemo(No:LongInt;Const FName:String):Boolean;
{ulozi soubor do memo}
{store file to memo}
{----------------------------------------------------------------------------}
Var F,FF:File;
    T:TDBTTypes;
    S:String[79];
    SR:TSearchRec;
    A:Array[1..1024] of Char;
    NumRead, NumWritten:{$IfDef VER80}Word{$Else}Integer{$EndIf};
Begin
  Result:=False;
  If Not FileExists(FName) Then Exit;
  If FindFirst(FName,faAnyFile,SR)<>0 Then Exit;
  FindClose(SR);
  AsSignFile(F,ChangeFileExt(FDBFName,'.DBT'));
  Try
    {$I-}
    ReSet(F,1);
    {$I+}
    If IoResult<>0 Then ReWrite(F,1);
    {zapis vety}
    Try
      System.Seek(F,System.FileSize(F));
      With T Do Begin
        { cislo zaznamu}
        {record no}
        NumberOf:=No;
        S:=ExtractFileExt(FName);
        If Length(S)<=3 Then
          Move(S[1],AsFileType,Length(S)); { extension of dtored type}
        Used:=True;                     { is used}
        SizeOfMemo:=SR.Size; { size of linked file}
        FileDateTime:=FileDateToDateTime(SR.Time); { original date and time of file}
      End;
      BlockWrite(F,T,SizeOf(T));
      {prekopiruj soubor do memo}
      {re-copy file to memo}
      AsSignFile(FF,FName);
      ReSet(FF,1);
      Try
        Repeat
          BlockRead(FF, A, SizeOf(A), NumRead);
          BlockWrite(F, A, NumRead, NumWritten);
        Until (NumRead = 0) or (NumWritten <> NumRead);
      Finally
        CloseFile(FF)
      End;
      Result:=True;
    Finally
      CloseFile(F);
    End;
  Except
    On EInOutError Do Fatal(msgErrorOnMemoOpen);
  End;
End;

{----------------------------------------------------------------------------}
Function TjbDBF.LoadMemo(No:LongInt;Var FName:String):Boolean;
{preda soubor z memo na disk do FName - zmeni u nej pouze extenzi dle uloz. typu}
{get filename from memo}
{----------------------------------------------------------------------------}
Var F,FF:File;
    T:TDBTTypes;
    Readed:{$IfDef VER80}Word{$Else}Integer{$EndIf};
    C:Char;
    I:LongInt;
    {$IFNDEF LINUX} { /AR/ }
    Handle:Integer;
    {$ENDIF}
Begin
  Result:=False;
  AsSignFile(F,ChangeFileExt(FDBFName,'.DBT'));
  Try
    ReSet(F,1);
    Try
      T.NumberOf:=-1;
      {hleda hlavickovy zaznam}
      {search header}
      While T.NumberOf<>No Do Begin
        BlockRead(F,T,SizeOf(T),Readed);
        If Readed=0 Then Exit;
        System.Seek(F,System.FilePos(F)+T.SizeOfMemo);
      End;
      {nasels, vrat se}
      {found, go back}
      System.Seek(F,System.FilePos(F)-T.SizeOfMemo);
      {zapis ho na disk - je-li jineho typu, zmen extenzi!! }
      {write to disk}
      FName:=ChangeFileExt(FName,'.'+T.AsFileType);
      AsSignFile(FF,FName);
      Try
        ReWrite(FF,1);
        Try
          {prekopiruj to do souboru}
          {re-copy to file}
          For I:=1 To T.SizeOfMemo Do Begin
            BlockRead(F,C,SizeOf(C));
            BlockWrite(FF,C,SizeOf(C));
          End;
        Finally
          CloseFile(FF);
          {nastav puvodni datum souboru}
          {set original date and time of file}
          
          {$IFDEF LINUX} { /AR/ }
            FileSetDate(FName,DateTimeToFileDate(T.FileDateTime));
          {$ELSE}
            Handle:=FileOpen(FName,fmOpenReadWrite);
            FileSetDate(Handle,DateTimeToFileDate(T.FileDateTime));
            FileClose(Handle);
          {$ENDIF}
          
        End;
      Except
        On EInOutError Do ;
      End;
      {a zapis}
      {and write}
      BlockWrite(F,T,SizeOf(T));
    Finally
      CloseFile(F);
    End;
    Result:=True;
  Except
    On EInOutError Do Fatal(msgErrorOnMemoOpen);
  End;

End;

{----------------------------------------------------------------------------}
Function TjbDBF.EraseMemo(No:LongInt):Boolean;
{oznaci soubor v memo za nepouzivany}
{set mark in memo file as unused}
{----------------------------------------------------------------------------}
Var F:File;
    T:TDBTTypes;
    Readed:{$IfDef VER80}Word{$Else}Integer{$EndIf};
Begin
  Result:=False;
  AsSignFile(F,ChangeFileExt(FDBFName,'.DBT'));
  Try
    ReSet(F,1);
    Try
      T.NumberOf:=-1;
      While T.NumberOf<>No Do Begin
        BlockRead(F,T,SizeOf(T),Readed);
        If Readed=0 Then Exit;
        System.Seek(F,System.FilePos(F)+T.SizeOfMemo);
      End;
      {nasels, vrat se}
      {find, go back}
      System.Seek(F,System.FilePos(F)-T.SizeOfMemo-SizeOf(T));
      {Nastav priznak}
      {set flag}
      T.Used:=False;
      {a zapis}
      {and write it}
      BlockWrite(F,T,SizeOf(T));
    Finally
      CloseFile(F);
    End;
    Result:=True;
  Except
    On EInOutError Do Fatal(msgErrorOnMemoOpen);
  End;
End;

{----------------------------------------------------------------------------}
Function TjbDBF.PackDBF : boolean;
{odstrani z tabulky vymazane zaznamy}
{remove deleted records from table}
{create a temporary file named [filename]_temp_pack.dbf and when finish renamed it}
{added by /AR/ }
{----------------------------------------------------------------------------}
var
 DBFNew : TjbDBF;
 iCol : integer;
 sFileNew : String;
 iLenTot : integer;
 sField : String;
 bConfirm : boolean;
begin
Result:=false;
bConfirm:=true;
DBFNew := TjbDBF.Create(Self);
try
  If AsSigned(FOnErase) Then FOnErase(Self, ExtractFileName(FDBFName),bConfirm);

  if not(bConfirm) then exit;

  iLenTot:=0;
  FileName := FDBFName;
  sFileNew := Copy(FDBFName,1,Length(FDBFName)-Length(ExtractFileExt(FDBFName)))+'_temp_pack.dbf';

  SysUtils.DeleteFile(sFileNew);

  try
    if Open then
    begin
     GotoStart;

     for iCol := 1 to FieldsCount do
     begin
       with Fields[iCol] do
       begin
         iLenTot := iLenTot+len;
         DBFNew.MakeField(iCol,name,what,len,places,'',idxtyp,idxsrt);
         DBFNew.FDBFFields[iCol].Idx := idx;
       end;
     end;
     DBFNew.CreateDB(sFileNew,iLenTot,FieldsCount);

     DBFNew.FileName := sFileNew;
     if DBFNew.Open then
       while CurrRec<RecordsCount do
       begin
         if not(CurrentRecDeleted) then
         begin
           for iCol := 1 to FieldsCount do
           begin
             sField := Fields[iCol].Name;
             DBFNew.Store(sField,Load(sField));
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
           SysUtils.RenameFile(sFileNew,FDBFName);
           Result:=true;
           If AsSigned(FOnErased) Then FOnErased(Self);
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
End;
{----------------------------------------------------------------------------}
Procedure TjbDBF.PackDBT;
{odstrani z memo nepouzite zaznamy}
{remove deleted records from memo}
{----------------------------------------------------------------------------}
Begin
End;

{ Changed by /VN/: }
{----------------------------------------------------------------------------}
function TjbDBF.GetRecordsCount: LongInt;
{vraci pocet zaznamu}
{get count of records}
{----------------------------------------------------------------------------}
begin
  Result := FDBFHeader.numRecs;
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
function TjbDBF.GetFieldByName(Const Key:TKey): TDBField;
{----------------------------------------------------------------------------}
var
  bFound : boolean;
  i : integer;
begin

bFound := false;
i:=0;

while not(bFound or (i>(FDBFCountItems))) do
begin
  inc(i);
  if Trim(FDBFFields[i].Name)=UpperCase(Key) then
  begin
     bFound := true;
     Result := FDBFFields[i];
  end;
end;
if not(bFound) then
    Fatal(msgBadDefinitionOfField);
end;
 {end of file; end of comment 15.1.2002 by J.B. Sorry for my English}
End.