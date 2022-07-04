{$I fb_define.inc}
unit fbmisc;
{$IFDEF FPC}
{.$mode objfpc}{$H+}
{$ENDIF}
{.$I jvuib.inc}

interface
uses Classes, SysUtils, DB, FBParams;

{$I fbmisc.inc}
const
  MaxSortField = 256;

type
  TCharSet = TSysCharSet;

  TFBDsOption =
   (poTrimCharFields,
    poRefreshAfterPost,
    poAutoParamsToFields,
    poFetchAll,
    poFillEmptyEPFromParams,
    poRefreshBeforeEdit
   );

  TFBDsOptions = set of TFBDsOption;
  TUpdateKinds = set of TUpdateKind;

  EFBError = class(EDatabaseError);

  TFBErrorID = (fbeCircularReference, fbeEmptySQLEdit, fbeDatabaseNotAssigned,
                fbeTransactionNotAssigned, fbeParameterNotFound,
                fbeNotCachedUpdates, fbeUserAbort, fbeErrorExecuteQ,
                fbeBlobCannotBeWritten, fbeCannotInsert);

  TWhenGetGenID = (wgNever, wgOnNewRecord, wgBeforePost);


  TTransactionKind = (tkDefault, tkReadTransaction, tkUpdateTransaction);
  TBFCurrentOperationState = (cosNone, cosInPost);
  
  { TDefaultFormats }

  TDefaultFormats = class(TPersistent)
  private
    FDisplayFormatDate: string;
    FDisplayFormatDateTime: string;
    FDisplayFormatTime: string;
    FDisplayFormatNumeric: string;
    FDisplayFormatInteger: string;
    FEditFormatInteger: string;
    FEditFormatNumeric: string;
    function IsStoreDT:boolean;
    function IsStoreD:boolean;
    function IsStoreT:boolean;
  protected
    procedure AssignTo(Dest: TPersistent);override;
  public
    constructor Create;
  published
    property DisplayFormatDateTime:string read FDisplayFormatDateTime
                                   write FDisplayFormatDateTime  stored IsStoreDT ;
    property DisplayFormatDate:string read FDisplayFormatDate
                               write FDisplayFormatDate  stored IsStoreD;
    property DisplayFormatTime:string read FDisplayFormatTime
                               write FDisplayFormatTime stored IsStoreT;
    property DisplayFormatNumeric:string read FDisplayFormatNumeric
                               write FDisplayFormatNumeric;
    property DisplayFormatInteger:string read FDisplayFormatInteger
                               write FDisplayFormatInteger;
    property EditFormatNumeric:string read FEditFormatNumeric
                               write FEditFormatNumeric;
    property EditFormatInteger:string read FEditFormatInteger
                               write FEditFormatInteger;
  end;

{$IFDEF FPC}
  TUpdateAction = (uaFail, uaAbort, uaSkip, uaRetry, uaApplied);
  TUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TUpdateKind;
    var UpdateAction: TUpdateAction) of object;
  TUpdateErrorEvent = procedure(DataSet: TDataSet; E: EDatabaseError;
    UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction) of object;
{$ENDIF}

type
  TFBInternalSortItem = record
    FieldNo:integer;
    Asc:boolean;
  end;

  TFBInternalSortArray = array [0..MaxSortField-1] of TFBInternalSortItem;

type
  TMasterUpdateStatus   = (muClose, muOpen, muFieldChange);
  TDetailCondition      = (dcForceOpen, dcIgnoreMasterClose, dcForceMasterRefresh);
  TDetailConditions     = set of TDetailCondition;
  TMasterScrollBehavior = (msbCancel, msbPost, msbNone);

const
  DefaultMacroChar = '@';
  DefaultTermChar  = '/';
  TrueExpr = '0=0';

procedure FBError(FBErrorID:TFBErrorID; Args:array of const);
function FBErrorStr(FBErrorID:TFBErrorID):string;
function QuoteIdentifier(Dialect: Integer; Value: String): String;
function NameDelimiter(C: Char; Delims: TCharSet): Boolean;
function IsLiteral(C: Char): Boolean;
procedure CreateQueryParams(List: TFBParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TCharSet);

{$IFDEF FPC}
function ExtractFieldName(const Fields: string; var Pos: Integer): string;
{$ENDIF}

function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
implementation

{$IFDEF FPC}
function ExtractFieldName(const Fields: string; var Pos: Integer): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(Fields)) and (Fields[I] <> ';') do Inc(I);
  Result := Trim(Copy(Fields, Pos, I - Pos));
  if (I <= Length(Fields)) and (Fields[I] = ';') then Inc(I);
  Pos := I;
end;
{$ENDIF}

procedure FBError(FBErrorID:TFBErrorID; Args:array of const);
begin
   raise EFBError.CreateFmt(FBErrorStr(FBErrorID), Args);
end;

function FBErrorStr(FBErrorID:TFBErrorID):string;
begin
  case FBErrorID of
    fbeCircularReference:Result:=sfbeCircularReference;
    fbeEmptySQLEdit:Result:=sfbeEmptySQLEdit;
    fbeDatabaseNotAssigned:Result:=sfbeDatabaseNotAssigned;
    fbeTransactionNotAssigned:Result:=sfbeTransactionNotAssigned;
    fbeParameterNotFound:Result:=sfbeParameterNotFound;
    fbeNotCachedUpdates:Result:=sfbeNotCachedUpdates;
    fbeUserAbort:Result:=sfbeUserAbort;
    fbeErrorExecuteQ:Result:=sfbeErrorExecuteQ;
    fbeBlobCannotBeWritten:Result:=sfbeBlobCannotBeWritten;
    fbeCannotInsert:Result:=sfbeCannotInsert;
  else
    Result:=sfbeOtherError;
  end;
end;

function QuoteIdentifier(Dialect: Integer; Value: String): String;
begin
  if Dialect = 1 then
    Value := AnsiUpperCase(Trim(Value))
  else
    Value := '"' + StringReplace (Value, '"', '""', [rfReplaceAll]) + '"';
  Result := Value;
end;

function NameDelimiter(C: Char; Delims: TCharSet): Boolean;
begin
  Result := (C in [' ', ',', ';', ')', #13, #10]) or (C in Delims);
end;

function IsLiteral(C: Char): Boolean;
begin
  Result := C in ['''', '"'];
end;

procedure CreateQueryParams(List: TFBParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TCharSet);
var
  CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

  function StripLiterals(Buffer: PChar): string;
  var
    Len: Word;
    TempBuf: PChar;

    procedure StripChar(Value: Char);
    begin
      if TempBuf^ = Value then
        StrMove(TempBuf, TempBuf + 1, Len - 1);
      if TempBuf[StrLen(TempBuf) - 1] = Value then
        TempBuf[StrLen(TempBuf) - 1] := #0;
    end;

  begin
    Len := StrLen(Buffer) + 1;
    TempBuf := AllocMem(Len);
    Result := '';
    try
      StrCopy(TempBuf, Buffer);
      StripChar('''');
      StripChar('"');
      Result := StrPas(TempBuf);
    finally
      FreeMem(TempBuf, Len);
    end;
  end;

begin
  if SpecialChar = #0 then Exit;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ <> SpecialChar) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter(CurChar, Delims)) do begin
        Inc(CurPos);
        CurChar := CurPos^;
        if IsLiteral(CurChar) then begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else Name := StrPas(StartPos + 1);
      if Assigned(List) then
      begin
        if List.FindParam(Name) = nil then
        begin
          if Macro then
            List.CreateParam(Name).Value := TrueExpr
          else List.CreateParam(Name);
        end;
      end;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ = SpecialChar) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral(CurChar) then Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

{ TDefaultFormats }

procedure TDefaultFormats.AssignTo(Dest: TPersistent);
begin
  if Dest is TDefaultFormats then
  with Dest as TDefaultFormats do
  begin
    FDisplayFormatDate:=Self.FDisplayFormatDate;
    FDisplayFormatDateTime:=Self.FDisplayFormatDateTime;
    FDisplayFormatTime:=Self.FDisplayFormatTime;
    FDisplayFormatNumeric:=Self.FDisplayFormatNumeric;
    FDisplayFormatInteger:=Self.FDisplayFormatInteger;
    FEditFormatNumeric:=Self.FEditFormatNumeric;
    FEditFormatInteger:=Self.FEditFormatInteger;
  end
  else
    inherited AssignTo(Dest)
end;

constructor TDefaultFormats.Create;
begin
  FDisplayFormatDateTime:=ShortDateFormat + ' '+ShortTimeFormat;
  FDisplayFormatDate:=ShortDateFormat;
  FDisplayFormatTime:=ShortTimeFormat;
  FDisplayFormatNumeric:='#,##0.0';
  FDisplayFormatInteger:='#,##0';
  FEditFormatNumeric:='#0.0';
  FEditFormatInteger:='#0';
end;

function TDefaultFormats.IsStoreD: boolean;
begin
  Result := FDisplayFormatDate <> ShortDateFormat;
end;

function TDefaultFormats.IsStoreDT: boolean;
begin
  Result := FDisplayFormatDateTime<>(ShortDateFormat + ' '+ShortTimeFormat);
end;

function TDefaultFormats.IsStoreT: boolean;
begin
  Result := FDisplayFormatTime <> ShortTimeFormat;
end;

function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  FieldCount: Integer;
  Fields: TList;
  Bookmark: TBookmarkStr;

  function CompareField(Field: TField; Value: Variant): Boolean;
  var
    S, S1: string;
  begin
    if Field.DataType = ftString then
    begin
      S := Field.AsString;
      S1:=Value;
      if (loPartialKey in Options) then
        Delete(S, Length(S1) + 1, MaxInt);
      if (loCaseInsensitive in Options) then
        Result := AnsiCompareText(S, S1) = 0
      else
        Result := AnsiCompareStr(S, S1) = 0;
    end
    else
      Result := (Field.Value = Value);
  end;

  function CompareRecord: Boolean;
  var
    I: Integer;
  begin
    if FieldCount = 1 then
      Result := CompareField(TField(Fields.First), KeyValues)
    else begin
      Result := True;
      for I := 0 to FieldCount - 1 do
        Result := Result and CompareField(TField(Fields[I]), KeyValues[I]);
    end;
  end;

begin
  Result := False;
  with DataSet do begin
    CheckBrowseMode;
    if BOF and EOF then Exit;
  end;
  Fields := TList.Create;
  try
    DataSet.GetFieldList(Fields, KeyFields);
    FieldCount := Fields.Count;
    Result := CompareRecord;
    if Result then Exit;
    DataSet.DisableControls;
    try
      Bookmark := DataSet.Bookmark;
      try
        with DataSet do begin
          First;
          while not EOF do begin
            Result := CompareRecord;
            if Result then Break;
            Next;
          end;
        end;
      finally
        if not Result {$IFDEF RX_D3} and
          DataSet.BookmarkValid(PChar(Bookmark)) {$ENDIF} then
          DataSet.Bookmark := Bookmark;
      end;
    finally
      DataSet.EnableControls;
    end;
  finally
    Fields.Free;
  end;
end;

end.




