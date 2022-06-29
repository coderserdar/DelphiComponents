{************************************************}
{                                                }
{  ATStreamSearch Component                      }
{  Copyright (C) 2007-2008 Alexey Torgashin      }
{  http://atorg.net.ru                           }
{  support@uvviewsoft.com                        }
{                                                }
{************************************************}

{$BOOLEVAL OFF} //Short boolean evaluation required.

{$I ATStreamSearchOptions.inc} //ATStreamSearch options.

unit ATStreamSearch;

interface

uses
  Windows,
  Classes,
  {$IFDEF REGEX}
  DIRegEx,
  {$ENDIF}
  {$IFDEF TNT}
  TntClasses,
  {$ENDIF}
  ATxCodepages;

type
  TATStreamSearchOption = (
    asoCaseSens,
    asoWholeWords,
    asoBackward,
    {$IFDEF REGEX} asoRegEx, {$ENDIF}
    {$IFDEF REGEX} asoRegExMLine, {$ENDIF}
    asoFromPage //For ATViewer only, ignored in ATStreamSearch
    );

  TATStreamSearchOptions = set of TATStreamSearchOption;

  TATStreamSearchProgress = procedure(
    const ACurrentPos, AMaximalPos: Int64;
    var AContinueSearching: Boolean) of object;

type
  TATStreamSearch = class(TComponent)
  private
    FStream: TStream;
    FStreamOwner: Boolean;
    FFileName: WideString;
    FStreamStart: Int64;
    FStreamSize: Int64;
    FFoundStart: Int64;
    FFoundLength: Int64;
    {$IFDEF REGEX}
    FRegEx: TDIRegExSearchStream_Enc;
    {$ENDIF}
    FOnProgress: TATStreamSearchProgress;
    FCharSize: Integer;

    FSavedText: WideString;
    FSavedEncoding: TATEncoding;
    FSavedOptions: TATStreamSearchOptions;
    //FSearchForValidUTF16: Boolean;

    procedure FreeStream;
    procedure InitSavedOptions;
    function InitProgressFields(
      const AStartPos: Int64;
      AEncoding: TATEncoding): Boolean;
    procedure DoProgress(
      const ACurrentPos, AMaximalPos: Int64;
      var AContinueSearching: Boolean);
    procedure SetFileName(const AFileName: WideString);
    procedure SetStream(AStream: TStream);

    {$IFDEF REGEX}
    procedure FreeRegex;
    procedure InitRegex;
    procedure RegexProgress(
      const ASender: TDICustomRegExSearch;
      const AProgress: Int64;
      var AAbort: Boolean);
    function RegexFindFirst(
      const AText: WideString;
      const AStartPos: Int64;
      AEncoding: TATEncoding;
      AOptions: TATStreamSearchOptions): Boolean;
    function RegexFindNext: Boolean;
    {$ENDIF}

    function TextFind(
      const AText: WideString;
      const AStartPos: Int64;
      AEncoding: TATEncoding;
      AOptions: TATStreamSearchOptions): Int64;
    function TextFindFirst(
      const AText: WideString;
      const AStartPos: Int64;
      AEncoding: TATEncoding;
      AOptions: TATStreamSearchOptions): Boolean;
    function TextFindNext(AFindPrevious: Boolean = False): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property FileName: WideString read FFileName write SetFileName;
    property Stream: TStream read FStream write SetStream;

    function FindFirst(
      const AText: WideString;
      const AStartPos: Int64;
      AEncoding: TATEncoding;
      AOptions: TATStreamSearchOptions): Boolean;
    function FindNext(AFindPrevious: Boolean = False): Boolean;

    property FoundStart: Int64 read FFoundStart;
    property FoundLength: Int64 read FFoundLength;
    property SearchString: WideString read FSavedText;

  published
    //property SearchForValidUTF16: Boolean read FSearchForValidUTF16 write FSearchForValidUTF16 default False;
    property OnProgress: TATStreamSearchProgress read FOnProgress write FOnProgress;
  end;

var
  MsgATStreamSearchRegExError: AnsiString = 'Regular expression pattern error:'#13#10#13#10'%s at offset %d';
  MsgATStreamSearchReadError: AnsiString = 'Read error at offset %d';

procedure Register;

implementation

uses
  {$IFDEF REGEX}
  DIRegEx_Api, DIRegEx_SearchStream, DIUtils,
  {$ENDIF}
  SysUtils, ATxSProc;

{ Constants }

const
  cBlockSize = 64 * 1024;

{ Helper functions }

function CharSize(AEncoding: TATEncoding): Integer;
begin
  if AEncoding in cATUnicodeEncodings then
    Result := 2
  else
    Result := 1;
end;

function BoolToSign(AValue: Boolean): Integer;
begin
  if AValue then
    Result := 1
  else
    Result := -1;
end;

procedure NormalizePos(var APos: Int64; ACharSize: Integer);
begin
  if ACharSize <> 1 then
    APos := APos div ACharSize * ACharSize;
end;

function LastPos(const AFileSize: Int64; ACharSize: Integer): Int64;
begin
  Result := AFileSize;
  NormalizePos(Result, ACharSize);
  Dec(Result, ACharSize);
  I64LimitMin(Result, 0);
end;

{ TATStreamSearch }

constructor TATStreamSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStream := nil;
  FStreamOwner := False;
  FFileName := '';
  FStreamStart := -1;
  FStreamSize := 0;
  FFoundStart := -1;
  FFoundLength := 0;
  //FSearchForValidUTF16 := False;

  {$IFDEF REGEX}
  FRegEx := nil;
  {$ENDIF}

  FOnProgress := nil;
  FCharSize := 1;
  InitSavedOptions;
end;

destructor TATStreamSearch.Destroy;
begin
  FreeStream;
  {$IFDEF REGEX}
  FreeRegex;
  {$ENDIF}
  inherited;
end;

procedure TATStreamSearch.FreeStream;
begin
  if FStreamOwner then
    if Assigned(FStream) then
      FreeAndNil(FStream);
end;

procedure TATStreamSearch.InitSavedOptions;
begin
  FSavedText := '';
  FSavedEncoding := vencANSI;
  FSavedOptions := [];
end;

procedure TATStreamSearch.SetFileName(const AFileName: WideString);
begin
  FreeStream;

  if AFileName <> '' then
    begin
      InitSavedOptions;
      FFileName := AFileName;
      FStreamOwner := True;
      FStream := {$IFDEF TNT}TTntFileStream{$ELSE}TFileStream{$ENDIF}.Create(
        AFileName, fmOpenRead or fmShareDenyNone);
    end;
end;

procedure TATStreamSearch.SetStream(AStream: TStream);
begin
  FreeStream;
  InitSavedOptions;
  FFileName := '';
  FStreamOwner := False;
  FStream := AStream;
end;

function TATStreamSearch.InitProgressFields(
  const AStartPos: Int64;
  AEncoding: TATEncoding): Boolean;
begin
  FStreamStart := AStartPos;
  FStreamSize := FStream.Size;
  FCharSize := CharSize(AEncoding);
  Result := FStreamSize >= FCharSize;
end;

procedure TATStreamSearch.DoProgress(
  const ACurrentPos, AMaximalPos: Int64;
  var AContinueSearching: Boolean);
begin
  AContinueSearching := True;

  if Assigned(FOnProgress) then
    FOnProgress(ACurrentPos, AMaximalPos, AContinueSearching);
end;

//-----------------------------------------------------------------
// RegEx-related code

{$IFDEF REGEX}

procedure TATStreamSearch.FreeRegex;
begin
  if Assigned(FRegEx) then
    FreeAndNil(FRegEx);
end;

procedure TATStreamSearch.InitRegex;
begin
  if not Assigned(FRegEx) then
    begin
      FRegEx := TDIRegExSearchStream_Enc.Create(Self);
      FRegEx.MatchOptions := FRegEx.MatchOptions - [moNotEmpty];
      FRegEx.OnProgress := RegexProgress;
    end;
end;

procedure TATStreamSearch.RegexProgress(
  const ASender: TDICustomRegExSearch;
  const AProgress: Int64;
  var AAbort: Boolean);
var
  ContinueSearching: Boolean;
begin
  ContinueSearching := True;
  DoProgress(
    FStreamStart + AProgress,
    FStreamSize,
    ContinueSearching);
  if not ContinueSearching then
    AAbort := True;
end;

function TATStreamSearch.RegexFindFirst(
  const AText: WideString;
  const AStartPos: Int64;
  AEncoding: TATEncoding;
  AOptions: TATStreamSearchOptions): Boolean;
var
  RealText: AnsiString;
begin
  Result := False;
  if AText = '' then Exit;

  //1. Prepare objects and fields

  InitRegex;

  Assert(Assigned(FRegEx), 'RegEx object not initialized');
  Assert(Assigned(FStream), 'Stream object not initialized');

  if not InitProgressFields(AStartPos, AEncoding) then Exit;

  FStream.Position := AStartPos;

  //2. Prepare RegEx object

  if asoCaseSens in AOptions then
    FRegEx.CompileOptions := FRegEx.CompileOptions - [coCaseLess]
  else
    FRegEx.CompileOptions := FRegEx.CompileOptions + [coCaseLess];

  if asoRegExMLine in AOptions then
    FRegEx.CompileOptions := FRegEx.CompileOptions + [coDotAll]
  else
    FRegEx.CompileOptions := FRegEx.CompileOptions - [coDotAll];

  RealText := StrEncodeUtf8(AText);

  if asoWholeWords in AOptions then
    begin
      //If "Whole Words" option is used, we first need to check
      //validity of original regex:
      if not FRegEx.CompileMatchPatternStr(RealText) then
        begin
          raise Exception.Create(Format(MsgATStreamSearchRegExError,
            [FRegEx.ErrorMessage, FRegEx.ErrorOffset]));
          Exit;
        end;
      //If it's OK we append '\b...\b' and compile regex again:
      RealText := '\b' + RealText + '\b';
    end;

  if not FRegEx.CompileMatchPatternStr(RealText) then
    begin
      raise Exception.Create(Format(MsgATStreamSearchRegExError,
        [FRegEx.ErrorMessage, FRegEx.ErrorOffset]));
      Exit;
    end;

  case AEncoding of
    vencANSI:
      FRegEx.SearchInitEnc(FStream, ansi_mbtowc);
    vencOEM:
      FRegEx.SearchInitEnc(FStream, oem_mbtowc);
    vencUnicodeLE:
      begin
        {if FSearchForValidUTF16 then
          FRegex.SearchInitEnc(FStream, utf16le_mbtowc)
        else}
        FRegEx.SearchInitEnc(FStream, binary16le_mbtowc);
      end;
    vencUnicodeBE:
      begin
        {if FSearchForValidUTF16 then
          FRegex.SearchInitEnc(FStream, utf16be_mbtowc)
        else}
        FRegEx.SearchInitEnc(FStream, binary16be_mbtowc);
      end;
    else
      Assert(False, 'Unknown encoding specified');
  end;

  //3. Search

  Result := RegexFindNext;
end;

function TATStreamSearch.RegexFindNext: Boolean;
var
  DummyStart, DummyLength,
  MatchStart, MatchLength: Int64;
begin
  Assert(Assigned(FRegEx), 'RegEx object not initialized');
  Assert(Assigned(FStream), 'Stream object not initialized');

  Result := FRegEx.SearchNext(
    DummyStart, DummyLength,
    MatchStart, MatchLength) >= 0;

  if Result then
    begin
      FFoundStart := FStreamStart + MatchStart * FCharSize;
      FFoundLength := MatchLength * FCharSize;
    end
  else
    begin
      FFoundStart := -1;
      FFoundLength := 0;
    end;
end;

{$ENDIF}

//-----------------------------------------------------------------
// Plain search code

function TATStreamSearch.TextFind(
  const AText: WideString;
  const AStartPos: Int64;
  AEncoding: TATEncoding;
  AOptions: TATStreamSearchOptions): Int64;
var
  Buffer: array[0 .. cBlockSize - 1] of AnsiChar;
  BufPosMax, BufPos, ReadPos: Int64;
  ReadSize, BytesRead: DWORD;
  SBufferA: AnsiString;
  SBufferW: WideString;
  StringPos: Integer;
  AForward, AWholeWords, ACaseSens, AContinue: Boolean;
begin
  Result := -1;

  if AText = '' then Exit;

  //1. Init objects and fields

  Assert(Assigned(FStream), 'Stream object not initialized');

  if not InitProgressFields(AStartPos, AEncoding) then Exit;

  //2. Init variables

  AForward := not (asoBackward in AOptions);
  AWholeWords := asoWholeWords in AOptions;
  ACaseSens := asoCaseSens in AOptions;

  BufPosMax := LastPos(FStreamSize, FCharSize);
  NormalizePos(BufPosMax, FCharSize);

  BufPos := AStartPos;
  NormalizePos(BufPos, FCharSize);

  if BufPos > BufPosMax then
    begin
      if AForward then
        Exit
      else
        BufPos := BufPosMax;
    end;

  if BufPos < 0 then
    begin
      if AForward then
        BufPos := 0
      else
        Exit;
    end;

  //3. Search

  DoProgress(BufPos, FStreamSize, AContinue);
  if not AContinue then Exit;

  repeat
    ReadPos := BufPos;
    ReadSize := cBlockSize;

    if not AForward then
      begin
        Dec(ReadPos, cBlockSize - FCharSize);
        I64LimitMin(ReadPos, 0);

        ReadSize := BufPos - ReadPos + FCharSize;
        if ReadSize > cBlockSize then
          ReadSize := cBlockSize;
      end;

    try
      FillChar(Buffer, SizeOf(Buffer), 0);
      FStream.Position := ReadPos;
      BytesRead := FStream.Read(Buffer, ReadSize);
    except
      raise Exception.Create(Format(MsgATStreamSearchReadError, [ReadPos]));
      Exit;
    end;

    if FCharSize = 2 then
      begin
        SBufferW := SetStringW(Buffer, BytesRead, AEncoding = vencUnicodeBE);
        StringPos := SFindTextW(AText, SBufferW, AForward, AWholeWords, ACaseSens, BytesRead < cBlockSize);
      end
    else
      begin
        SetString(SBufferA, Buffer, BytesRead);
        SBufferA := SCodepageToUnicode(SBufferA, AEncoding);
        StringPos := SFindText(AText, SBufferA, AForward, AWholeWords, ACaseSens, BytesRead < cBlockSize);
      end;

    if StringPos > 0 then
      begin
        Result := ReadPos + (StringPos - 1) * FCharSize;
        Exit
      end;

    DoProgress(BufPos, FStreamSize, AContinue);
    if not AContinue then Exit;

    Inc(BufPos, Int64(ReadSize) * BoolToSign(AForward));
    Dec(BufPos, Int64(Length(AText) + 1) * FCharSize * BoolToSign(AForward));
    NormalizePos(BufPos, FCharSize);

    if (BufPos < 0) or (BufPos > BufPosMax) then Exit;

  until BytesRead < cBlockSize;
end;

function TATStreamSearch.TextFindFirst(
  const AText: WideString;
  const AStartPos: Int64;
  AEncoding: TATEncoding;
  AOptions: TATStreamSearchOptions): Boolean;
var
  ARealStartPos: Int64;
begin
  ARealStartPos := AStartPos;
  if (asoBackward in AOptions) and (AStartPos = 0) then
    ARealStartPos := High(Int64);

  FFoundStart := TextFind(AText, ARealStartPos, AEncoding, AOptions);
  Result := FFoundStart >= 0;

  if Result then
    FFoundLength := Length(AText) * FCharSize
  else
    FFoundLength := 0;
end;

function TATStreamSearch.TextFindNext(AFindPrevious: Boolean = False): Boolean;
var
  ACharSize: Integer;
  AStartPos,
  AForwardPos, ABackwardPos: Int64;
  ANewOptions: TATStreamSearchOptions;
begin
  ACharSize := CharSize(FSavedEncoding);

  AForwardPos := FFoundStart + ACharSize;
  ABackwardPos := FFoundStart + (Length(FSavedText) - 2) * ACharSize;

  if (asoBackward in FSavedOptions) xor (AFindPrevious) then
    AStartPos := ABackwardPos
  else
    AStartPos := AForwardPos;

  ANewOptions := FSavedOptions;
  if AFindPrevious then
    if (asoBackward in ANewOptions) then
      Exclude(ANewOptions, asoBackward)
    else
      Include(ANewOptions, asoBackward);

  FFoundStart := TextFind(FSavedText, AStartPos, FSavedEncoding, ANewOptions);
  Result := FFoundStart >= 0;

  if Result then
    FFoundLength := Length(FSavedText) * ACharSize
  else
    FFoundLength := 0;
end;

//-----------------------------------------------------------------
// Combined search code

function TATStreamSearch.FindFirst(
  const AText: WideString;
  const AStartPos: Int64;
  AEncoding: TATEncoding;
  AOptions: TATStreamSearchOptions): Boolean;
begin
  InitSavedOptions;

  FSavedText := AText;
  FSavedEncoding := AEncoding;
  FSavedOptions := AOptions;

  {$IFDEF REGEX}
  if asoRegEx in AOptions then
  begin
    Assert(not (asoBackward in AOptions), 'Backward search not supported for Regex');
    Result := RegexFindFirst(AText, AStartPos, AEncoding, AOptions);
  end
  else
  {$ENDIF}
    Result := TextFindFirst(AText, AStartPos, AEncoding, AOptions);
end;

function TATStreamSearch.FindNext(AFindPrevious: Boolean = False): Boolean;
begin
  Assert(FSavedText <> '', 'Search text is empty');

  {$IFDEF REGEX}
  if asoRegEx in FSavedOptions then
  begin
    Assert(AFindPrevious = False, 'FindPrevious not supported for Regex');
    Result := RegexFindNext;
  end
  else
  {$ENDIF}
    Result := TextFindNext(AFindPrevious);
end;


{ Registration }

procedure Register;
begin
  RegisterComponents('Samples', [TATStreamSearch]);
end;

end.
