unit VirtualDataSet;

{$INCLUDE dOCI.inc}

{
 Descendant of TDataSet

 This is a wrapper of TDataSet, for implementing Delphi specific functions.

}


{
 Data in buffers are stores in BDE compatible format
  ftInteger - as integer(4 bytes)
  ftSmallInt - as smallint(2 bytes)
  ftFloat   - as double (8 bytes)
  ftCurrency - as double(8 bytes)
  ftDate - as TDateTimeRec
  ftTime - as TDateTimeRec
  ftDateTime - as TDateTimeRec
  ftWord - as Word(2 bytes)
  ftBoolean - as WordBool(2 bytes)
  ftBlob,ftMemo - as pointer (pointer to BLOB data) and integer (size of BLOB data) (8 bytes)
  ftString  - as pchar (Size bytes), probably without #0 at the end.
}

interface

uses Db, Classes, DynamicArrays
  {$IFDEF D6} ,Variants {$ENDIF}
  {$IFDEF D7} ,Variants {$ENDIF}
  {$ifdef ADEBUG} ,ADataSet {$endif}
;

type
{$IFDEF D4}  { Borland Delphi 4.0 }
  TUpdateAction = (uaFail, uaAbort, uaSkip, uaRetry, uaApplied);
  TUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction) of object;
{$ENDIF}

  TVirtualDataSet = class;

  TBookmInfo = record
   Bookmark     : integer;
   BookmarkFlag: TBookmarkFlag;
  end;
  PBookmInfo=^TBookmInfo;
  PInteger=^Integer;
  TPutMode = (pmAppend,pmInsert,pmUpdate);
  TVirtualFilterRecordEvent = procedure(DataSet: TVirtualDataSet; RecNum : integer;
                                          var Accept: Boolean) of object;
  TOnFastCalcFields = procedure(DataSet: TVirtualDataSet; RecNum : integer) of object;

  TVirtualDataSet = class(TDataSet)
  private
   FCalcBuf:pointer;
   FFieldsOffset :THArrayInteger;
   FFieldsSize   :THArrayInteger;
   FOpened       :boolean;
   FRecSize      :word;

   FFilterRecordEvent :TVirtualFilterRecordEvent;
   FOnFastCalcFields :TOnFastCalcFields;
   FAfterInternalOpen: TDataSetNotifyEvent;
   OldBuffer:pointer;
   procedure VReadAll;

  protected
   FCount        :integer;
   FCurrent      :integer;
   FBookm        :THArrayInteger;
   UniqBookmark  :integer;

   function GetRecNo : integer; override;

   //abstract methods
   function  VOpen            :boolean; virtual; abstract;
   function  VPrepare         :boolean; virtual; abstract;
   function  VClose           :boolean; virtual; abstract;
   procedure VGoto(RecordNum :integer); virtual; abstract;
   procedure VInitFieldDefs(Opened:boolean); virtual; abstract;

   function  VGetFieldValue( RecordNum:integer;
                             FieldID  :integer;
                             Buffer   :pointer):boolean; virtual; abstract;

   procedure VPutFieldValue( RecordNum:integer;
                             FieldID  :integer;
                             Buffer   :pointer;
                             mode     :TPutMode;
                             IfNotNull:Boolean);virtual; abstract;

   function VPost        (RecordNum:integer):TUpdateAction;  virtual; abstract;
   function VInsert      (RecordNum:integer):TUpdateAction;  virtual; abstract;
   function VDeleteRecord(RecordNum:integer):TUpdateAction;  virtual; abstract;

   function FetchNextBlock:boolean; virtual; abstract;

   function GetRealSize(FieldType:TFieldType;Size:integer):integer;

   //overrided methods from TDataSet
   procedure InternalOpen;                                  override;
   procedure InternalEdit;                                  override;
   procedure InternalInitFieldDefs;                         override;
   procedure InternalClose;                                 override;
   procedure InternalInsert;                                override;
   function  IsCursorOpen:boolean;                          override;
   procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
   procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
   function  GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
   procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
   procedure InternalSetToRecord(Buffer: PChar);            override;
   procedure InternalGotoBookmark(Bookmark: Pointer);       override;
   function  GetRecordCount:integer;        override;
   function  AllocRecordBuffer:PChar;       override;
   procedure FreeRecordBuffer(var Buffer: PChar);           override;
   function  GetRecordSize: Word;                           override;
   procedure InternalInitRecord(Buffer: PChar);             override;
   procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
   procedure InternalPost; override;
   procedure InternalDelete; override;
   function  GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
   procedure SetFieldData(Field: TField; Buffer: Pointer); override;
   procedure InternalFirst; override;
   procedure InternalLast; override;
   procedure InternalHandleException; override;
   procedure SetFiltered(Value:boolean); override;

   procedure SetRecNo(Value : integer); override;
   function  GetFieldID(FieldName : string) : integer;
   function  GetFieldCount: integer;

   procedure ClearDataSet; virtual;
  public
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy; override;
    procedure SetFieldValue(RecordNum:integer; FieldID : integer; Value:variant); virtual; abstract;
    function GetFieldValue(RecordNum:integer; FieldID : integer):variant; virtual; abstract;
    procedure GotoRecNum(RecNum:integer);
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure OpenAll;

    procedure ReOpen;

    property  RecNo; //for internal use
    procedure CopyStructure(DataSet:TDataSet);

    function Locate(const KeyFields: string; const KeyValues: Variant;
                    Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
                    const ResultFields: string): Variant;override;

    property FieldID[Index:string]:integer read GetFieldID;

    property ActiveRecord;
    property CurrentRecord;

   function CompareBookmarks(Bookmark1,Bookmark2:TBookmark):integer;override;

  published
   property Active;
   property BeforeOpen;
   property AfterOpen;
   property BeforeClose;
   property AfterClose;
   property BeforeInsert;
   property AfterInsert;
   property BeforeEdit;
   property AfterEdit;
   property BeforePost;
   property AfterPost;
   property BeforeCancel;
   property AfterCancel;
   property BeforeDelete;
   property AfterDelete;
   property BeforeScroll;
   property AfterScroll;
   property OnFastCalcFields:TOnFastCalcFields read FOnFastCalcFields write FOnFastCalcFields;
   property OnDeleteError;
   property OnEditError;
   property OnNewRecord;
   property OnPostError;
   property Filtered;
   property AfterInternalOpen: TDataSetNotifyEvent read FAfterInternalOpen write FAfterInternalOpen;
   property OnVFilterRecord : TVirtualFilterRecordEvent
                         read  FFilterRecordEvent
                         write FFilterRecordEvent;

  end;

implementation

uses SysUtils, DBConsts;

constructor TVirtualDataSet.Create(AOwner:TComponent);
begin
  inherited Create(Aowner);
  FFieldsOffset:=THArrayInteger.Create;
  FFieldsSize:=THArrayInteger.Create;
  FBookm:=THArrayInteger.Create;
  UniqBookmark:=0;
  FCount:=0;
  FCurrent:=-1;
  FOpened:=False;
  FCalcBuf:=nil;
end;

destructor TVirtualDataSet.Destroy;
begin
{$ifdef ADEBUG}LogMessage('TVirtualDataSet.Destroy BEGIN');{$endif}

  FFieldsOffset.Free;
  FFieldsSize.Free;
  FBookm.Free;
  inherited Destroy;

{$ifdef ADEBUG}LogMessage('TVirtualDataSet.Destroy END');{$endif}
end;

procedure TVirtualDataSet.InternalOpen;
var
  i         :integer;
  RealSize  :integer;
  off       :word;
begin
  ClearBuffers;
  BookmarkSize:=sizeof(TBookmInfo);
  FCount:=0;
  FCurrent:=-1;

    { moved to VOpen by roma 24.05.2002}
//  if DefaultFields then begin
//   VInitFieldDefs(True);
//   CreateFields;
//  end;

  FOpened:=VOpen;
  if not FOpened then exit;

  FFieldsOffset.ClearMem;
  FFieldsSize.ClearMem;
  FBookm.ClearMem;
  UniqBookmark:=0;

  off:=sizeof(TBookmInfo);
  for i:=0 to FieldDefs.Count-1 do begin
    FFieldsOffset.AddValue(off);
    RealSize:=GetRealSize(FieldDefs[i].DataType,FieldDefs[i].Size);
    FFieldsSize.AddValue(RealSize);
    off:=off+RealSize+1;  //one extra byte for isFieldNull function
  end;
  FRecSize:=off;

  BindFields(True);
  OldBuffer:=AllocMem(RecordSize);
  if Assigned (AfterInternalOpen) then AfterInternalOpen(self);
end;

function TVirtualDataSet.GetRealSize(FieldType:TFieldType;Size:integer):integer;
begin
  case FieldType of
       ftFloat           : Result:=SizeOf(Double);
       ftCurrency        : Result:=SizeOf(Currency);
       ftInteger         : Result:=SizeOf(Integer);
       ftSmallInt        : Result:=SizeOf(SmallInt);
       ftDate            : Result:=SizeOf(TDateTimeRec);
       ftTime            : Result:=SizeOf(TDateTimeRec);
       ftWord            : Result:=SizeOf(Word);
       ftBoolean         : Result:=SizeOf(WordBool);
       ftDateTime        : Result:=SizeOf(TDateTimeRec);
       ftString          : Result:=Size;
       ftBlob,ftMemo     : Result:=2*SizeOf(pointer); // BlobSize and pointer to memory where BLOB field stores data
    else
       Result:=Size;
  end;
end;

function  TVirtualDataSet.GetFieldID(FieldName : string) : integer;
begin
  Result:=FieldDefs.IndexOf(FieldName);
  if Result=-1 then raise EDatabaseError.Create('Field "'+FieldName+'" not found!');
end;

function  TVirtualDataSet.GetFieldCount : integer;
begin
  Result:=FieldDefs.Count;
end;

procedure TVirtualDataSet.InternalClose;
begin
  if not FOpened then exit;
  if DefaultFields then Fields.Clear;
  FOpened:=not VClose;
  FCount:=0;
  FBookm.ClearMem;
  FFieldsOffset.ClearMem;
  FFieldsSize.ClearMem;
  FreeMem(OldBuffer);
  OldBuffer:=nil;
end;

procedure TVirtualDataSet.InternalInitFieldDefs;
begin
 if Active then exit;
 VInitFieldDefs(False);
end;

function TVirtualDataSet.IsCursorOpen:boolean;
begin
  Result:=FOpened;
end;

procedure TVirtualDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^:=PBookmInfo(Buffer).Bookmark;
end;

procedure TVirtualDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PBookmInfo(Buffer).Bookmark:=PInteger(Data)^;
end;

function  TVirtualDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result:=PBookmInfo(Buffer).BookmarkFlag;
end;

procedure TVirtualDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PBookmInfo(Buffer).BookmarkFlag:=Value;
end;

procedure TVirtualDataSet.InternalSetToRecord(Buffer: PChar);
begin
  FCurrent:=FBookm.IndexOf(PBookmInfo(Buffer).Bookmark);
  //MoveBy(1);
  //PBookmInfo(Buffer).BookmarkFlag := bfCurrent;
{  if PBookmInfo(Buffer).BookmarkFlag=bfBOF then
    FCurrent:=FCurrent-1;}
end;

procedure TVirtualDataSet.GotoRecNum(RecNum:integer);
var u:integer;
begin
// b.Bookmark
  u:=FBookm[RecNum];
  GotoBookmark(@u);
end;

procedure TVirtualDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  InternalSetToRecord(PChar(bookmark));
end;

function  TVirtualDataSet.GetRecordCount:integer;
begin
 Result:=FCount;
end;

function  TVirtualDataSet.AllocRecordBuffer:PChar;
begin
  Result:=AllocMem(FRecSize);
end;

procedure TVirtualDataSet.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeMem(Buffer);
end;

function  TVirtualDataSet.GetRecordSize: Word;
begin
  Result:=FRecSize;
end;

function TVirtualDataSet.GetRecNo : integer;
begin
  UpdateCursorPos;
  Result:=FCurrent;
end;

procedure TVirtualDataSet.SetRecNo(Value : integer);
begin
 if (Value>-1) and (Value<RecordCount) then
 begin
   FCurrent:=Value;
   Resync([]);//Refresh; {roma 13.08.2000}
 end;
end;

procedure TVirtualDataSet.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer^,RecordSize,#0);
end;

procedure TVirtualDataSet.InternalInsert;
begin
 // nothing yet
end;

procedure TVirtualDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  i    : integer;
  mode : TPutMode;
  r:integer;
begin
  r:=FCurrent;
  if r=-1 then r:=0;
  if Append then r:=RecordCount;

  if Append
   then mode:=pmAppend
   else mode:=pmInsert;

  VInsert(r);
  for i:=0 to FieldDefs.Count-1 do
    VPutFieldValue(r,i,
                   pointer(cardinal(Buffer) +
                           cardinal(FFieldsOffset.Value[i])+1),
                   mode, Boolean(pointer(cardinal(Buffer)+cardinal(FFieldsOffset.Value[i]))^));
//  Inc(UniqBookmark); // VInsert automatically call FBookm.Insert
//  FBookm.AddValue(UniqBookmark);
  if mode=pmAppend then FCurrent:=RecordCount;
  Inc(FCount);
end;

procedure TVirtualDataSet.InternalPost;
var
  i    : integer;
  mode : TPutMode;
  ua : TUpdateAction;
begin
  case State of
    dsEdit   : begin
                 ua:=VPost(FCurrent);
                 if ua<>uaApplied then abort;
                 if ua=uaAbort then abort;
                 mode:=pmUpdate;
                 for i:=0 to FieldDefs.Count-1 do
                   VPutFieldValue(FCurrent,i,
                                  pointer(cardinal(ActiveBuffer) +
                                          cardinal(FFieldsOffset.Value[i])+1),
                                  mode,
                                  Boolean(pointer(cardinal(ActiveBuffer) +
                                          cardinal(FFieldsOffset.Value[i]))^));

               end;
    dsInsert : begin
                if FCurrent<>-1 then ua:=VPost(FCurrent) else ua:=VPost(0);
                if ua=uaAbort then abort;
                if ua<>uaApplied then abort;
                if PBookmInfo(ActiveBuffer).BookmarkFlag<>bfEOF then
                 begin
                   if FCurrent=-1 then
                   begin
                     Inc(FCurrent);
                     InternalAddRecord(ActiveBuffer,False);
                     Dec(FCurrent);
                   end else
                     InternalAddRecord(ActiveBuffer,False)
                 end else
                    InternalAddRecord(ActiveBuffer,True);
               end;
  end;
end;

procedure TVirtualDataSet.InternalDelete;
var ua:TUpdateAction;
begin
  ua:=VDeleteRecord(FCurrent);
  if ua=uaAbort then abort;
  if ua<>uaApplied then raise EDatabaseError.Create('Error delete');
  InternalInitRecord(ActiveBuffer);
  Dec(FCount);
end;

function  TVirtualDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  i       :integer;
  accept  :boolean;
  inv     :boolean;
//  SaveState:TDataSetState;
begin
//inv:=inherited Active;
  Result:=grOk;
  if GetMode=gmNext then
    repeat
      FCurrent:=FCurrent+1;
      VGoto(FCurrent);
      if FCurrent>=RecordCount then break;
      accept:=FCurrent<>-1;
        if Filtered and Assigned(FFilterRecordEvent) then
          FFilterRecordEvent(self,FCurrent,accept);
    until accept;

  if GetMode=gmPrior then
    repeat
      FCurrent:=FCurrent-1;
      VGoto(FCurrent);
      if FCurrent<0 then break; {roma}
      accept:=FCurrent<>-1;
        if Filtered and Assigned(FFilterRecordEvent) then
          FFilterRecordEvent(self,FCurrent,accept);

    until accept;           

  if GetMode=gmCurrent then
  begin
      VGoto(FCurrent);
      accept:=(FCurrent>=0) and (FCurrent<RecordCount);
        if Filtered and Assigned(FFilterRecordEvent) and (accept) then
          FFilterRecordEvent(self,FCurrent,accept);
      if not accept then Result:=grEOF;
  end;

  if FCurrent>=RecordCount then begin Result:=grEOF; FCurrent:=RecordCount; end;
  if FCurrent<0       then begin Result:=grBOF; FCurrent:=-1; end;

  if Result=grOk then
//  if FCurrent<RecordCount then
  begin
    if Assigned(OnFastCalcFields) then OnFastCalcFields(self,FCurrent);
    for i:=0 to FieldDefs.Count-1 do
    begin
      inv:=VGetFieldValue(FCurrent,i,
                     pointer(cardinal(Buffer) + cardinal(FFieldsOffset.Value[i])+1)
                    );
      Boolean(pointer(cardinal(Buffer) + cardinal(FFieldsOffset.Value[i]))^):=inv; // True - if data exists. False - if field=NULL
    end;
    FBookm.Get(FCurrent,@(PBookmInfo(Buffer)^.Bookmark));
    PBookmInfo(Buffer)^.BookmarkFlag:=bfCurrent;
    FCalcBuf:=Buffer;
    CalculateFields(Buffer);
    FCalcBuf:=nil;
  end;
end;

function  TVirtualDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  i   :integer;
  pi:pointer;
begin
//  if FDoCalc and Assigned(OnCalculateFields) then
  Result:=False;
  if IsEmpty and (FCurrent=-1) then exit;  // needed !!! Otherwise in empty table data will be shown in first row
  i:=GetFieldID(Field.FieldName);
  if i>=FFieldsOffset.Count then exit;

  if State=dsOldValue
   then pi:=OldBuffer
   else if Assigned(FCalcBuf)
         then pi:=FCalcBuf
         else pi:=ActiveBuffer;



  pi:=pointer(cardinal(pi)+cardinal(FFieldsOffset.Value[i]));
{  if Assigned(FCalcBuf)
   then pi:=pointer(cardinal(FCalcBuf)+ofs)
   else pi:=pointer(cardinal(CalcBuffer)+ofs);}


  Result:=Boolean(pi^);
  if not Result then exit;
  if Buffer=nil then exit;

  pi:=pointer(cardinal(pi)+1);
  memcpy(pi,Buffer,FFieldsSize.Value[i]);
  if Field.DataType=ftString
   then pchar(cardinal(Buffer)+cardinal(FFieldsSize.Value[i]))^:=#0;
end;

procedure TVirtualDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  i   :integer;
  ofs :cardinal;
  po:pointer;
begin
 // if not (State in dsWriteModes) then ADatabaseError(SNotEditing, Self);
  Field.Validate(Buffer);
  i:=GetFieldID(Field.FieldName);
  ofs:=cardinal(FFieldsOffset.Value[i]);

  if Assigned(FCalcBuf)
   then po:=pointer(cardinal(FCalcBuf)+ofs)
   else po:=pointer(cardinal(ActiveBuffer)+ofs);
{  if Assigned(FCalcBuf)
   then po:=pointer(cardinal(FCalcBuf)+ofs)
   else po:=pointer(cardinal(CalcBuffer)+ofs);}

  if Buffer=nil then
  begin
    FillChar(po^,Integer(FFieldsSize.Value[i])+1,0);
    if not (State in [dsBrowse, dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Longint(Field));
    exit;
  end;
  Boolean(po^):=True;
  po:=pointer(cardinal(po)+1);
  memcpy(Buffer,po,FFieldsSize.Value[i]);
  if not (State in [dsBrowse, dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));
end;

procedure TVirtualDataSet.InternalFirst;
var
  accept : boolean;
begin
  FCurrent:=-1;

  //exit;
/////////////////////////////////////////////////
  repeat
    FCurrent:=FCurrent+1;
    if FCurrent>=RecordCount then begin break;end;
    accept:=true;
    try
      if Filtered and Assigned(FFilterRecordEvent) then
        FFilterRecordEvent(self,FCurrent,accept);
    except
      on Exception do accept:=false;
    end;

  until accept;
//  if FCurrent=0 then FCurrent:=-1;
  dec(FCurrent);{roma}
end;

procedure TVirtualDataSet.InternalLast;
var
  accept : boolean;
begin
  VReadAll;
  FCurrent:=RecordCount;

// exit;
//////////////////////////
  repeat
    FCurrent:=FCurrent-1;
    if FCurrent<0 then break;
    accept:=true;
    try
      if Filtered and Assigned(FFilterRecordEvent) then
        FFilterRecordEvent(self,FCurrent,accept);
    except
      on Exception do accept:=false;
    end;
  until accept;
  //if FCurrent=(RecordCount-1) then FCurrent:=RecordCount;
  inc(FCurrent);
end;

procedure TVirtualDataSet.InternalHandleException;
begin
 raise Exception.Create('TVirtualDataSet.InternalHandleException'); {roma 14.12.2000}
end;

procedure TVirtualDataSet.ClearDataSet;
begin
end;

procedure TVirtualDataSet.SetFiltered(Value:boolean);
begin
 inherited SetFiltered(Value);
 if FOpened then Resync([]);// Refresh; {roma 13.08.2000}
end;

function TVirtualDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
                    Options: TLocateOptions): Boolean;
var
  FieldList:THArrayInteger;
  sl1,sl2:TStrings;
  p,i:integer;
  Accept:boolean;
  Bookm : integer;

  function AnsiCompareCS(const v,w: string; CaseSensitive: boolean; PartialKey: boolean): boolean;
  var s: string;
  begin
   if PartialKey
    then s := Copy(w, 1, length(v))
    else s := w;
   if CaseSensitive
    then Result := AnsiCompareStr(v,s)=0
    else Result := AnsiCompareText(v,s)=0
  end;

  function Compare: boolean;
  var i: integer;
  begin
   Result:=True;
   for i:=0 to sl1.Count-1 do
    if not AnsiCompareCS(sl1[i],sl2[i],not(loCaseInsensitive in Options),loPartialKey in Options) then begin
     Result:=False;
     exit;
    end;
  end;

  procedure FillCurKeyValues;
  var i:integer;
  begin
   sl2.Clear;
   for i:=0 to FieldList.Count-1 do sl2.Add(VarToStr(GetFieldValue(p,FieldList[i])))
  end;

  function VGetFieldList(FieldList:THArrayInteger;const FieldNames:string):boolean;
  var Pos,f:integer;
  begin
   Result:=True;
   Pos:=1;
   while Pos<=Length(FieldNames) do begin
    Result:=True;
    try
     f:=GetFieldID(ExtractFieldName(FieldNames,Pos));
     FieldList.AddValue(f);
    except
     Result:=False;
    end;
   end;
  end;

begin
 Result:=False;
 FieldList:=THArrayInteger.Create;
 sl1:=TStringList.Create;
 sl2:=TStringList.Create;
 try
  if not VGetFieldList(FieldList,KeyFields) then exit;
  if FieldList.Count=1
   then sl1.Add(VarToStr(KeyValues))
   else for i:=0 to FieldList.Count-1 do
          sl1.Add(VarToStr(KeyValues[i]));

  p:=0;

//  VReadAll;
 repeat
  while p<RecordCount do begin
   Accept:=True;
   if Filtered and Assigned(FFilterRecordEvent)
    then FFilterRecordEvent(self,p,Accept);
   if Accept then begin
    FillCurKeyValues;
    Result:=Compare;
    if Result then begin
     Bookm:=FBookm.Value[p];
     GotoBookmark(@Bookm);
     exit;
    end;
   end;
   inc(p);
  end;
 until not FetchNextBlock;
 finally
  sl2.Free;
  sl1.Free;
  FieldList.Free;
 end;
end;

function TVirtualDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
var
  FieldList:THArrayInteger;
  sl1,sl2:TStrings;
  p,i:integer;
  Accept:boolean;
  Bookm1 : TBookmark;
  Bookm2 : integer;

  function AnsiCompareCS(const v,w: string; CaseSensitive: boolean; PartialKey: boolean): boolean;
  var s: string;
  begin
   if PartialKey
    then s := Copy(w, 1, length(v))
    else s := w;
   if CaseSensitive
    then Result := AnsiCompareStr(v,s)=0
    else Result := AnsiCompareText(v,s)=0
  end;

  function Compare: boolean;
  var i: integer;
  begin
   Result:=True;
   for i:=0 to sl1.Count-1 do
    if not AnsiCompareCS(sl1[i],sl2[i],True,False) then begin
     Result:=False;
     exit;
    end;
  end;

  procedure FillCurKeyValues;
  var i:integer;
  begin
   sl2.Clear;
   for i:=0 to FieldList.Count-1 do sl2.Add(VarToStr(GetFieldValue(p,FieldList[i])))
  end;

  function VGetFieldList(FieldList:THArrayInteger;const FieldNames:string):boolean;
  var Pos,f:integer;
  begin
   Result:=True;
   Pos:=1;
   while Pos<=Length(FieldNames) do begin
    Result:=True;
    try
     f:=GetFieldID(ExtractFieldName(FieldNames,Pos));
     FieldList.AddValue(f);
    except
     Result:=False;
    end;
   end;
  end;

begin
 Result:=Null;
 FieldList:=THArrayInteger.Create;
 sl1:=TStringList.Create;
 sl2:=TStringList.Create;
 try
  if not VGetFieldList(FieldList,KeyFields) then exit;
  if FieldList.Count=1
   then sl1.Add(VarToStr(KeyValues))
   else for i:=0 to FieldList.Count-1 do
          sl1.Add(VarToStr(KeyValues[i]));

  p:=0;

//  VReadAll;
 repeat
  while p<RecordCount do begin
   Accept:=True;
   if Filtered and Assigned(FFilterRecordEvent)
    then FFilterRecordEvent(self,p,Accept);
   if Accept then begin
    FillCurKeyValues;
    if Compare then begin
     Bookm1:=GetBookmark; //remember current position
     Bookm2:=FBookm.Value[p];
     GotoBookmark(@Bookm2); // go to the found record
     Result:=FieldValues[ResultFields]; // get found data
     GotoBookmark(Bookm1); // return back to the remembered position
     exit;
    end;
   end;
   inc(p);
  end;
 until not FetchNextBlock;
 finally
  sl2.Free;
  sl1.Free;
  FieldList.Free;
 end;
end;

procedure TVirtualDataSet.ReOpen;
begin
 Close;
 Open;
end;

function TVirtualDataSet.CompareBookmarks(Bookmark1,
  Bookmark2: TBookmark): integer;
const
 RetCodes: array[Boolean, Boolean] of ShortInt = ((2,-1),(1,0));
begin
  { Check for uninitialized bookmarks }
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then begin
   if PBookmInfo(Bookmark1).Bookmark<PBookmInfo(Bookmark2).Bookmark
    then Result := -1
    else if PBookmInfo(Bookmark1).Bookmark>PBookmInfo(Bookmark2).Bookmark
          then Result := 1
          else Result := 0;
  end;
end;

procedure TVirtualDataSet.VReadAll;
begin
 while FetchNextBlock do;
end;

procedure TVirtualDataSet.OpenAll;
begin
 Open;
 VReadAll;
end;

procedure TVirtualDataSet.InternalEdit;
begin
 memcpy(ActiveBuffer,OldBuffer,RecordSize);
end;

procedure TVirtualDataSet.CopyStructure(DataSet: TDataSet);
var i:integer;
begin
 FieldDefs.Clear;
 for i:=0 to DataSet.FieldDefs.Count-1 do begin
   FieldDefs.Add(DataSet.FieldDefs[i].Name,DataSet.FieldDefs[i].DataType,DataSet.FieldDefs[i].Size,DataSet.FieldDefs[i].Required);
 end;
end;

end.
