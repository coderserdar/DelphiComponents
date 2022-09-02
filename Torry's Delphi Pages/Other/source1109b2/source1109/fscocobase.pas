// FSSQL: 1.02x
// FSSQL: Base components for Coco/R for Delphi grammars for use with
//        version 1.1 of Coco/R

Unit fscocobase;

Interface

{$I fsdefine.inc}

Uses
  Classes,
  SysUtils;

Const
  SetSize = 16; { sets are stored in 16 bits }

  { Standard Error Types }
  etSyntax = 0;
  etSymantic = 1;

  chCR = #13;
  chLF = #10;
  chEOL = chCR + chLF; { End of line characters for Microsoft Windows }
  chLineSeparator = chCR;

Type
  ECocoBookmark = Class( Exception );
  TCocoStatusType = ( cstInvalid, cstBeginParse, cstEndParse, cstLineNum, cstString );
  TCocoError = Class( TObject )
  Private
    FErrorCode: Integer;
    FCol: Integer;
    FLine: Integer;
    FData: String;
    FErrorType: Integer;
  Public
    Property ErrorType: Integer Read FErrorType Write FErrorType;
    Property ErrorCode: Integer Read FErrorCode Write FErrorCode;
    Property Line: Integer Read FLine Write FLine;
    Property Col: Integer Read FCol Write FCol;
    Property Data: String Read FData Write FData;
  End; {TCocoError}

  TCommentItem = Class( TObject )
  Private
    fComment: String;
    fLine: Integer;
    fColumn: Integer;
  Public
    Property Comment: String Read fComment Write fComment;
    Property Line: Integer Read fLine Write fLine;
    Property Column: Integer Read fColumn Write fColumn;
  End; {TCommentItem}

  TCommentList = Class( TObject )
  Private
    FList: TList;

    Function FixComment( Const S: String ): String;
    Function GetComments( Idx: Integer ): String;
    Procedure SetComments( Idx: Integer; Const Value: String );
    Function GetCount: Integer;
    Function GetText: String;
    Function GetColumn( Idx: Integer ): Integer;
    Function GetLine( Idx: Integer ): Integer;
    Procedure SetColumn( Idx: Integer; Const Value: Integer );
    Procedure SetLine( Idx: Integer; Const Value: Integer );
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Clear;
    Procedure Add( Const S: String; Const aLine: Integer; Const aColumn: Integer );
    Property Comments[Idx: Integer]: String Read GetComments Write SetComments; Default;
    Property Line[Idx: Integer]: Integer Read GetLine Write SetLine;
    Property Column[Idx: Integer]: Integer Read GetColumn Write SetColumn;
    Property Count: Integer Read GetCount;
    Property Text: String Read GetText;
  End; {TCommentList}

  TSymbolPosition = Class( TObject )
  Private
    fLine: Integer;
    fCol: Integer;
    fLen: Integer;
    fPos: Integer;
  Public
    Procedure Clear;
    Procedure Assign( Source: TSymbolPosition );

    Property Line: Integer Read fLine Write fLine; {line of symbol}
    Property Col: Integer Read fCol Write fCol; {column of symbol}
    Property Len: Integer Read fLen Write fLen; {length of symbol}
    Property Pos: Integer Read fPos Write fPos; {file position of symbol}
  End; {TSymbolPosition}

  TGenListType = ( glNever, glAlways, glOnError );

  TBitSet = Set Of 0..15;
  PStartTable = ^TStartTable;
  TStartTable = Array[0..255] Of Integer;
  TCharSet = Set Of Char;

  TAfterGenListEvent = Procedure( Sender: TObject;
    Var PrintErrorCount: Boolean ) Of Object;
  TAfterGrammarGetEvent = Procedure( Sender: TObject;
    Var CurrentInputSymbol: Integer ) Of Object;
  TCommentEvent = Procedure( Sender: TObject; CommentList: TCommentList ) Of Object;
  TCustomErrorEvent = Function( Sender: TObject; Const ErrorCode: Integer;
    Const Data: String ): String Of Object;
  TErrorEvent = Procedure( Sender: TObject; Error: TCocoError ) Of Object;
  TErrorProc = Procedure( ErrorCode: Integer; Symbol: TSymbolPosition;
    Const Data: String; ErrorType: Integer ) Of Object;
  TFailureEvent = Procedure( Sender: TObject; NumErrors: Integer ) Of Object;
  TGetCH = Function( Pos: Integer ): Char Of Object;
  TStatusUpdateProc = Procedure( Sender: TObject;
    Const StatusType: TCocoStatusType;
    Const Status: String;
    Const LineNum: Integer ) Of Object;

  TCocoRScanner = Class( TObject )
  Private
    FbpCurrToken: Integer; {position of current token)}
    FBufferPosition: Integer; {current position in buf }
    FContextLen: Integer; {length of appendix (CONTEXT phrase)}
    FCurrentCh: TGetCH; {procedural variable to get current input character}
    FCurrentSymbol: TSymbolPosition; {position of the current symbol in the source stream}
    FCurrInputCh: Char; {current input character}
    FCurrLine: Integer; {current input line (may be higher than line)}
    FLastInputCh: Char; {the last input character that was read}
    FNextSymbol: TSymbolPosition; {position of the next symbol in the source stream}
    FNumEOLInComment: Integer; {number of _EOLs in a comment}
    FOnStatusUpdate: TStatusUpdateProc;
    FScannerError: TErrorProc;
    FSourceLen: Integer; {source file size}
    FSrcStream: TMemoryStream; {source memory stream}
    FStartOfLine: Integer;

    Function GetNStr( Symbol: TSymbolPosition; ChProc: TGetCh ): String;
    Function ExtractBookmarkChar( Var aBookmark: String ): Char;
  Protected
    FStartState: TStartTable; {start state for every character}

    Function Bookmark: String; Virtual;
    Procedure GotoBookmark( aBookmark: String ); Virtual;

    Function CapChAt( Pos: Integer ): Char;
    Procedure Get( Var sym: Integer; TableN: boolean = False ); Virtual; Abstract;
    Procedure NextCh; Virtual; Abstract;

    Function GetStartState: PStartTable;
    Procedure SetStartState( aStartTable: PStartTable );

    Property bpCurrToken: Integer Read fbpCurrToken Write fbpCurrToken;
    Property BufferPosition: Integer Read fBufferPosition Write fBufferPosition;
    Property ContextLen: Integer Read fContextLen Write fContextLen;
    Property CurrentCh: TGetCh Read fCurrentCh Write fCurrentCh;
    Property CurrentSymbol: TSymbolPosition Read fCurrentSymbol Write fCurrentSymbol;
    Property CurrInputCh: Char Read fCurrInputCh Write fCurrInputCh;
    Property CurrLine: Integer Read fCurrLine Write fCurrLine;
    Property LastInputCh: Char Read fLastInputCh Write fLastInputCh;
    Property NextSymbol: TSymbolPosition Read fNextSymbol Write fNextSymbol;
    Property NumEOLInComment: Integer Read fNumEOLInComment Write fNumEOLInComment;
    Property OnStatusUpdate: TStatusUpdateProc Read FOnStatusUpdate Write FOnStatusUpdate;
    Property ScannerError: TErrorProc Read FScannerError Write FScannerError;
    Property SourceLen: Integer Read fSourceLen Write fSourceLen;
    Property SrcStream: TMemoryStream Read fSrcStream Write fSrcStream;
    Property StartOfLine: Integer Read fStartOfLine Write fStartOfLine;
    Property StartState: PStartTable Read GetStartState Write SetStartState;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Function CharAt( Pos: Integer ): Char;
    Function GetName( Symbol: TSymbolPosition ): String; // Retrieves name of symbol of length len at position pos in source file
    Function GetString( Symbol: TSymbolPosition ): String; // Retrieves exact string of max length len from position pos in source file
    Procedure _Reset;
  End; {TCocoRScanner}

  TCocoRGrammar = Class( TComponent )
  Private
    fAfterGet: TAfterGrammarGetEvent;
    FAfterGenList: TAfterGenListEvent;
    FAfterParse: TNotifyEvent;
    FBeforeGenList: TNotifyEvent;
    FBeforeParse: TNotifyEvent;
    fClearSourceStream: Boolean;
    FErrDist: Integer; // number of symbols recognized since last error
    FErrorList: TList;
    fGenListWhen: TGenListType;
    FListStream: TMemoryStream;
    FOnCustomError: TCustomErrorEvent;
    FOnError: TErrorEvent;
    FOnFailure: TFailureEvent;
    FOnStatusUpdate: TStatusUpdateProc;
    FOnSuccess: TNotifyEvent;
    FScanner: TCocoRScanner;
    FSourceFileName: String;
    fExtra: Integer;

    Function GetSourceStream: TMemoryStream;
    Function GetSuccessful: Boolean;
    Procedure SetOnStatusUpdate( Const Value: TStatusUpdateProc );
    Procedure SetSourceStream( Const Value: TMemoryStream );
    Function GetLineCount: Integer;
    Function GetCharacterCount: Integer;
  Protected
    fCurrentInputSymbol: Integer; // current input symbol

    Function Bookmark: String; Virtual;
    Procedure GotoBookmark( aBookmark: String ); Virtual;

    Procedure ClearErrors;
    Function ErrorStr( Const ErrorCode: Integer; Const Data: String ): String; Virtual; Abstract;
    Procedure Expect( N: Integer );
    Procedure GenerateListing;
    Procedure Get( TableN: boolean = False ); Virtual; Abstract;
    Procedure PrintErr( Line: String; ErrorCode, lnr, Col: Integer;
      Data: String );
    Procedure StoreError( nr: Integer; Symbol: TSymbolPosition;
      Const Data: String; ErrorType: Integer );

    Procedure DoAfterParse; Virtual;
    Procedure DoBeforeParse; Virtual;

    Property ClearSourceStream: Boolean Read fClearSourceStream Write fClearSourceStream Default True;
    Property CurrentInputSymbol: Integer Read fCurrentInputSymbol Write fCurrentInputSymbol;
    Property ErrDist: Integer Read fErrDist Write fErrDist; // number of symbols recognized since last error
    Property ErrorList: TList Read FErrorList Write FErrorList;
    Property Extra: Integer Read fExtra Write fExtra;
    Property GenListWhen: TGenListType Read fGenListWhen Write fGenListWhen Default glOnError;
    Property ListStream: TMemoryStream Read FListStream Write FListStream;
    Property SourceFileName: String Read FSourceFileName Write FSourceFileName;
    Property SourceStream: TMemoryStream Read GetSourceStream Write SetSourceStream;
    Property Successful: Boolean Read GetSuccessful;

    {Events}
    Property AfterParse: TNotifyEvent Read fAfterParse Write fAfterParse;
    Property AfterGenList: TAfterGenListEvent Read fAfterGenList Write fAfterGenList;
    Property AfterGet: TAfterGrammarGetEvent Read fAfterGet Write fAfterGet;
    Property BeforeGenList: TNotifyEvent Read fBeforeGenList Write fBeforeGenList;
    Property BeforeParse: TNotifyEvent Read fBeforeParse Write fBeforeParse;
    Property OnCustomError: TCustomErrorEvent Read FOnCustomError Write FOnCustomError;
    Property OnError: TErrorEvent Read FOnError Write FOnError;
    Property OnFailure: TFailureEvent Read FOnFailure Write FOnFailure;
    Property OnStatusUpdate: TStatusUpdateProc Read FOnStatusUpdate Write SetOnStatusUpdate;
    Property OnSuccess: TNotifyEvent Read FOnSuccess Write FOnSuccess;
  Public
    Constructor Create( aOwner: TComponent ); Override;
    Destructor Destroy; Override;

    Procedure GetLine( Var Pos: Integer; Var Line: String;
      Var Eof: Boolean );
    Function LexName: String;
    Function LexString: String;
    Function LookAheadName: String;
    Function LookAheadString: String;
    Procedure _StreamLine( S: String );
    Procedure _StreamLn( Const S: String );
    Procedure SemError( Const errNo: Integer; Const Data: String );
    Procedure SynError( Const errNo: Integer );

    Property Scanner: TCocoRScanner Read fScanner Write fScanner;
    Property LineCount: Integer Read GetLineCount;
    Property CharacterCount: Integer Read GetCharacterCount;
  End; {TCocoRGrammar}

Const
  _EF = #0;
  _TAB = #09;
  _CR = #13;
  _LF = #10;
  _EL = _CR;
  _EOF = #26; {MS-DOS eof}
  LineEnds: TCharSet = [_CR, _LF, _EF];
  { not only for errors but also for not finished states of scanner analysis }
  minErrDist = 2; { minimal distance (good tokens) between two errors }

Function PadL( Const S: String; Ch: Char; L: Integer ): String;
Function strtok(
  Var Text: String;
  Const Ch: Char ): String;

Implementation

Const
  INVALID_CHAR = 'Invalid Coco/R for Delphi bookmark character';
  INVALID_INTEGER = 'Invalid Coco/R for Delphi bookmark integer';
  BOOKMARK_STR_SEPARATOR = ' ';

Function PadL( Const S: String; Ch: Char; L: Integer ): String;
Var
  i: Integer;
Begin
  Result := S;
  For i := 1 To L - ( Length( Result ) ) Do
    Result := Ch + Result;
End; {PadL}

Function strtok(
  Var Text: String;
  Const Ch: Char ): String;
Var
  aPos: Integer;
Begin
  aPos := Pos( Ch, Text );
  If ( aPos > 0 ) Then
    Begin
      Result := Copy( Text, 1, aPos - 1 );
      Delete( Text, 1, aPos );
    End
  Else
    Begin
      Result := Text;
      Text := '';
    End;
End; {StrTok}

{ TSymbolPosition }

Procedure TSymbolPosition.Assign( Source: TSymbolPosition );
Begin
  fLine := Source.fLine;
  fCol := Source.fCol;
  fLen := Source.fLen;
  fPos := Source.fPos;
End; {Assign}

Procedure TSymbolPosition.Clear;
Begin
  fLen := 0;
  fPos := 0;
  fLine := 0;
  fCol := 0;
End; { Clear }

{ TCocoRScanner }

Function TCocoRScanner.Bookmark: String;
Begin
  Result := IntToStr( bpCurrToken ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( BufferPosition ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( ContextLen ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( CurrLine ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( NumEOLInComment ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( StartOfLine ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( CurrentSymbol.Line ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( CurrentSymbol.Col ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( CurrentSymbol.Len ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( CurrentSymbol.Pos ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( NextSymbol.Line ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( NextSymbol.Col ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( NextSymbol.Len ) + BOOKMARK_STR_SEPARATOR
    + IntToStr( NextSymbol.Pos ) + BOOKMARK_STR_SEPARATOR
    + CurrInputCh
    + LastInputCh
End; {Bookmark}

Function TCocoRScanner.ExtractBookmarkChar( Var aBookmark: String ): Char;
Begin
  If Length( aBookmark ) > 0 Then
    Result := aBookmark[1]
  Else
    Raise ECocoBookmark.Create( INVALID_CHAR );
End; {ExtractBookmarkChar}

Procedure TCocoRScanner.GotoBookmark( aBookmark: String );
Var
  BookmarkToken: String;
Begin
  Try
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    bpCurrToken := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    BufferPosition := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    ContextLen := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    CurrLine := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    NumEOLInComment := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    StartOfLine := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    CurrentSymbol.Line := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    CurrentSymbol.Col := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    CurrentSymbol.Len := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    CurrentSymbol.Pos := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    NextSymbol.Line := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    NextSymbol.Col := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    NextSymbol.Len := StrToInt( BookmarkToken );
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    NextSymbol.Pos := StrToInt( BookmarkToken );
    CurrInputCh := ExtractBookmarkChar( aBookmark );
    LastInputCh := ExtractBookmarkChar( aBookmark );
  Except
    On EConvertError Do
      Raise ECocoBookmark.Create( INVALID_INTEGER );
    Else
      Raise;
  End;
End; {GotoBookmark}

Constructor TCocoRScanner.Create;
Begin
  Inherited;
  fSrcStream := TMemoryStream.Create;
  CurrentSymbol := TSymbolPosition.Create;
  NextSymbol := TSymbolPosition.Create;
End; {Create}

Destructor TCocoRScanner.Destroy;
Begin
  fSrcStream.Free;
  fSrcStream := Nil;
  CurrentSymbol.Free;
  CurrentSymbol := Nil;
  NextSymbol.Free;
  NextSymbol := Nil;
  Inherited;
End; {Destroy}

Function TCocoRScanner.CapChAt( Pos: Integer ): Char;
Begin
  Result := UpCase( CharAt( Pos ) );
End; {CapCharAt}

Function TCocoRScanner.CharAt( Pos: Integer ): Char;
Var
  Ch: Char;
Begin
  If Pos >= SourceLen Then
    Begin
      Result := _EF;
      Exit;
    End;

  {SrcStream.Seek(pos, soFromBeginning);
  SrcStream.ReadBuffer(Ch, 1);}
  Ch := PChar( FSrcStream.Memory )[Pos];

  If Ch <> _EOF Then
    Result := Ch
  Else
    Result := _EF
End; {CharAt}

Function TCocoRScanner.GetNStr( Symbol: TSymbolPosition; ChProc: TGetCh ): String;
Var
  i: Integer;
  P: Integer;
Begin
  SetLength( Result, Symbol.Len );
  P := Symbol.Pos;
  i := 1;
  While i <= Symbol.Len Do
    Begin
      Result[i] := ChProc( P );
      Inc( i );
      Inc( P )
    End;
End; {GetNStr}

Function TCocoRScanner.GetName( Symbol: TSymbolPosition ): String;
Begin
  Result := GetNStr( Symbol, CurrentCh );
End; {GetName}

Function TCocoRScanner.GetStartState: PStartTable;
Begin
  Result := @fStartState;
End; {GetStartState}

Procedure TCocoRScanner.SetStartState( aStartTable: PStartTable );
Begin
  fStartState := aStartTable^;
End; {SetStartState}

Function TCocoRScanner.GetString( Symbol: TSymbolPosition ): String;
Begin
  Result := GetNStr( Symbol, CharAt );
End; {GetString}

Procedure TCocoRScanner._Reset;
Var
  Len: Integer;
Begin
  { Make sure that the stream has the _EF character at the end. }
  CurrInputCh := _EF;
  SrcStream.Seek( 0, soFromEnd );
  SrcStream.WriteBuffer( CurrInputCh, 1 );
  SrcStream.Seek( 0, soFromBeginning );

  LastInputCh := _EF;
  Len := SrcStream.Size;
  SourceLen := Len;
  CurrLine := 1;
  StartOfLine := -2;
  BufferPosition := -1;
  CurrentSymbol.Clear;
  NextSymbol.Clear;
  NumEOLInComment := 0;
  ContextLen := 0;
  NextCh;
End; {_Reset}

{ TCocoRGrammar }

Procedure TCocoRGrammar.ClearErrors;
Var
  i: Integer;
Begin
  For i := 0 To fErrorList.Count - 1 Do
    TCocoError( fErrorList[i] ).Free;
  fErrorList.Clear;
End; {ClearErrors}

Constructor TCocoRGrammar.Create( aOwner: TComponent );
Begin
  Inherited;
  FGenListWhen := glOnError;
  fClearSourceStream := True;
  fListStream := TMemoryStream.Create;
  fErrorList := TList.Create;
End; {Create}

Destructor TCocoRGrammar.Destroy;
Begin
  fListStream.Clear;
  fListStream.Free;
  ClearErrors;
  fErrorList.Free;
  Inherited;
End; {Destroy}

Procedure TCocoRGrammar.Expect( N: Integer );
Begin
  If CurrentInputSymbol = N Then
    Get
  Else
    SynError( N );
End; {Expect}

Procedure TCocoRGrammar.GenerateListing;
{ Generate a source listing with error messages }
Var
  i: Integer;
  Eof: Boolean;
  lnr, errC: Integer;
  srcPos: Integer;
  Line: String;
  PrintErrorCount: Boolean;
  StartLine, StopLine: Integer;
  Done: Boolean;
Begin
  If Assigned( BeforeGenList ) Then
    BeforeGenList( Self );
  {find the line of the first error}
  StopLine := 0;

  srcPos := 0;
  lnr := 1;
  GetLine( srcPos, Line, Eof );
  While ( StopLine = 0 ) And Not Eof Do
    Begin
      For i := 0 To ErrorList.Count - 1 Do
        Begin
          If TCocoError( ErrorList[i] ).Line = lnr Then
            Begin
              StopLine := lnr;
              Break;
            End;
        End;
      GetLine( srcPos, Line, Eof );
      Inc( lnr );
    End;

  If StopLine = 0 Then
    StopLine := 1;
  If StopLine > 1 Then
    StartLine := StopLine - 1
  Else
    StartLine := StopLine;

  srcPos := 0;
  GetLine( srcPos, Line, Eof );
  lnr := 1;
  errC := 0;
  Done := False;
  While  Not Done And Not Eof Do
    Begin
      If lnr >= StartLine Then
        Begin
          _StreamLine( Line );
          For i := 0 To ErrorList.Count - 1 Do
            Begin
              If TCocoError( ErrorList[i] ).Line = lnr Then
                Begin
                  PrintErr( Line, TCocoError( ErrorList[i] ).ErrorCode,
                    TCocoError( ErrorList[i] ).Line,
                    TCocoError( ErrorList[i] ).Col,
                    TCocoError( ErrorList[i] ).Data );
                  Inc( errC );
                  Done := True;
                  Break;
                End;
            End;
        End;
      GetLine( srcPos, Line, Eof );
      Inc( lnr );
    End;

  If Not Done Then
    For i := 0 To ErrorList.Count - 1 Do
      Begin
        If TCocoError( ErrorList[i] ).Line = lnr Then
          Begin
            PrintErr( Line, TCocoError( ErrorList[i] ).ErrorCode,
              TCocoError( ErrorList[i] ).Line,
              TCocoError( ErrorList[i] ).Col,
              TCocoError( ErrorList[i] ).Data );
            Inc( errC );
          End;
      End;
  PrintErrorCount := True;
  If Assigned( AfterGenList ) Then
    AfterGenList( Self, PrintErrorCount );
  If PrintErrorCount Then
    Begin
      _StreamLine( '' );
      _StreamLn( PadL( IntToStr( errC ), ' ', 5 ) + ' error' );
      If errC <> 1 Then
        _StreamLine( 's' );
    End;
End; {GenerateListing}

Procedure TCocoRGrammar.GetLine( Var Pos: Integer;
  Var Line: String;
  Var Eof: Boolean );
{ Read a source line. Return empty line if eof }
Var
  Ch: Char;
  i: Integer;
Begin
  i := 1;
  Eof := False;
  Ch := Scanner.CharAt( Pos );
  Inc( Pos );
  While Not ( Ch In LineEnds ) Do
    Begin
      SetLength( Line, Length( Line ) + 1 );
      Line[i] := Ch;
      Inc( i );
      Ch := Scanner.CharAt( Pos );
      Inc( Pos );
    End;
  SetLength( Line, i - 1 );
  Eof := ( i = 1 ) And ( Ch = _EF );
  If Ch = _CR Then
    Begin { check for MsDos end of lines }
      Ch := Scanner.CharAt( Pos );
      If Ch = _LF Then
        Begin
          Inc( Pos );
          Extra := 0;
        End;
    End;
End; {GetLine}

Function TCocoRGrammar.GetSourceStream: TMemoryStream;
Begin
  Result := Scanner.SrcStream;
End; {GetSourceStream}

Function TCocoRGrammar.GetSuccessful: Boolean;
Begin
  Result := ErrorList.Count = 0;
End; {GetSuccessful}

Function TCocoRGrammar.LexName: String;
Begin
  Result := Scanner.GetName( Scanner.CurrentSymbol )
End; {LexName}

Function TCocoRGrammar.LexString: String;
Begin
  Result := Scanner.GetString( Scanner.CurrentSymbol )
End; {LexString}

Function TCocoRGrammar.LookAheadName: String;
Begin
  Result := Scanner.GetName( Scanner.NextSymbol )
End; {LookAheadName}

Function TCocoRGrammar.LookAheadString: String;
Begin
  Result := Scanner.GetString( Scanner.NextSymbol )
End; {LookAheadString}

Procedure TCocoRGrammar.PrintErr( Line: String; ErrorCode: Integer; lnr, Col: Integer; Data: String );
{ Print an error message }

  Procedure DrawErrorPointer;
  Var
    i: Integer;
  Begin
    _StreamLn( '*****  ' );
    i := 0;
    While i < Col + Extra - 2 Do
      Begin
        If ( ( Length( Line ) > 0 ) And ( Length( Line ) < i ) ) And ( Line[i] = _TAB ) Then
          _StreamLn( _TAB )
        Else
          _StreamLn( ' ' );
        Inc( i )
      End;
    _StreamLn( '^ ' )
  End; {DrawErrorPointer}

Var
  S: String;
Begin {PrintErr}
  //DrawErrorPointer;
  _StreamLine( Copy( Line, 1, Col - 2 ) + '--------------------------' );
  S := ErrorStr( ErrorCode, Data );
  _StreamLine( Format( 'Syntax error at line %d, position %d: %s',
    [lnr, Col - 1, S] ) );
  _StreamLine( '' )
End; {PrintErr}

Procedure TCocoRGrammar.SemError( Const errNo: Integer; Const Data: String );
Begin
  If errDist >= minErrDist Then
    Scanner.ScannerError( errNo, Scanner.CurrentSymbol, Data, etSymantic );
  errDist := 0;
End; {SemError}

Procedure TCocoRGrammar._StreamLn( Const S: String );
Begin
  If Length( S ) > 0 Then
    ListStream.WriteBuffer( S[1], Length( S ) );
End; {_StreamLn}

Procedure TCocoRGrammar._StreamLine( S: String );
Begin
  S := S + chEOL;
  _StreamLn( S );
End; {_StreamLine}

Procedure TCocoRGrammar.SynError( Const errNo: Integer );
Begin
  If errDist >= minErrDist Then
    Scanner.ScannerError( errNo, Scanner.NextSymbol, '', etSyntax );
  errDist := 0;
End; {SynError}

Procedure TCocoRGrammar.SetOnStatusUpdate( Const Value: TStatusUpdateProc );
Begin
  FOnStatusUpdate := Value;
  Scanner.OnStatusUpdate := Value;
End; {SetOnStatusUpdate}

Procedure TCocoRGrammar.SetSourceStream( Const Value: TMemoryStream );
Begin
  Scanner.SrcStream := Value;
End; {SetSourceStream}

Procedure TCocoRGrammar.StoreError( nr: Integer; Symbol: TSymbolPosition;
  Const Data: String; ErrorType: Integer );
{ Store an error message for later printing }
Var
  Error: TCocoError;
Begin
  Error := TCocoError.Create;
  Error.ErrorCode := nr;
  If Assigned( Symbol ) Then
    Begin
      Error.Line := Symbol.Line;
      Error.Col := Symbol.Col;
    End
  Else
    Begin
      Error.Line := 0;
      Error.Col := 0;
    End;
  Error.Data := Data;
  Error.ErrorType := ErrorType;
  ErrorList.Add( Error );
  If Assigned( OnError ) Then
    OnError( Self, Error );
End; {StoreError}

Function TCocoRGrammar.GetLineCount: Integer;
Begin
  Result := Scanner.CurrLine;
End; {GetLineCount}

Function TCocoRGrammar.GetCharacterCount: Integer;
Begin
  Result := Scanner.BufferPosition;
End; {GetCharacterCount}

Procedure TCocoRGrammar.DoBeforeParse;
Begin
  If Assigned( fBeforeParse ) Then
    fBeforeParse( Self );
  If Assigned( fOnStatusUpdate ) Then
    fOnStatusUpdate( Self, cstBeginParse, '', -1 );
End; {DoBeforeParse}

Procedure TCocoRGrammar.DoAfterParse;
Begin
  If Assigned( fOnStatusUpdate ) Then
    fOnStatusUpdate( Self, cstEndParse, '', -1 );
  If Assigned( fAfterParse ) Then
    fAfterParse( Self );
End; {DoAfterParse}

Function TCocoRGrammar.Bookmark: String;
Begin
  Result :=
    IntToStr( fCurrentInputSymbol ) + BOOKMARK_STR_SEPARATOR
    + Scanner.Bookmark;
End; {Bookmark}

Procedure TCocoRGrammar.GotoBookmark( aBookmark: String );
Var
  BookmarkToken: String;
Begin
  Try
    BookmarkToken := strtok( aBookmark, BOOKMARK_STR_SEPARATOR );
    fCurrentInputSymbol := StrToInt( BookmarkToken );
    Scanner.GotoBookmark( aBookmark );
  Except
    On EConvertError Do
      Raise ECocoBookmark.Create( INVALID_INTEGER );
    Else
      Raise;
  End;
End; {GotoBookmark}

{ TCommentList }

Procedure TCommentList.Add( Const S: String; Const aLine: Integer;
  Const aColumn: Integer );
Var
  CommentItem: TCommentItem;
Begin
  CommentItem := TCommentItem.Create;
  Try
    CommentItem.Comment := FixComment( S );
    CommentItem.Line := aLine;
    CommentItem.Column := aColumn;
    FList.Add( CommentItem );
  Except
    CommentItem.Free;
  End;
End; {Add}

Procedure TCommentList.Clear;
Var
  i: Integer;
Begin
  For i := 0 To FList.Count - 1 Do
    TCommentItem( FList[i] ).Free;
  FList.Clear;
End; {Clear}

Constructor TCommentList.Create;
Begin
  FList := TList.Create;
End; {Create}

Destructor TCommentList.Destroy;
Begin
  Clear;
  If Assigned( FList ) Then
    Begin
      FList.Free;
      FList := Nil;
    End;
  Inherited;
End; {Destroy}

Function TCommentList.FixComment( Const S: String ): String;
Begin
  Result := S;
  While ( Length( Result ) > 0 ) And ( Result[Length( Result )] < #32 ) Do
    Delete( Result, Length( Result ), 1 );
End; {FixComment}

Function TCommentList.GetColumn( Idx: Integer ): Integer;
Begin
  Result := TCommentItem( FList[Idx] ).Column;
End; {GetColumn}

Function TCommentList.GetComments( Idx: Integer ): String;
Begin
  Result := TCommentItem( FList[Idx] ).Comment;
End; {GetComments}

Function TCommentList.GetCount: Integer;
Begin
  Result := FList.Count;
End; {GetCount}

Function TCommentList.GetLine( Idx: Integer ): Integer;
Begin
  Result := TCommentItem( FList[Idx] ).Line;
End; {GetLine}

Function TCommentList.GetText: String;
Var
  i: Integer;
Begin
  Result := '';
  For i := 0 To Count - 1 Do
    Begin
      Result := Result + Comments[i];
      If i < Count - 1 Then
        Result := Result + chEOL;
    End;
End; {GetText}

Procedure TCommentList.SetColumn( Idx: Integer; Const Value: Integer );
Begin
  TCommentItem( FList[Idx] ).Column := Value;
End; {SetColumn}

Procedure TCommentList.SetComments( Idx: Integer; Const Value: String );
Begin
  TCommentItem( FList[Idx] ).Comment := Value;
End; {SetComments}

Procedure TCommentList.SetLine( Idx: Integer; Const Value: Integer );
Begin
  TCommentItem( FList[Idx] ).Line := Value;
End; {SetLine}

End.

