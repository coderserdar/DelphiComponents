unit SQLHilit;

{ © Electro-Concept Mauricie, 2003 }
{ Implements TSQLHighlighter object, used for SQL syntax highlighting
  in a TPlusMemo }

{ Note: to install this component on your palette, add file SQLHilitReg in your package }

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$DEFINE SQLHilit}
{UCONVERT}
  {$IFDEF SQLHilitClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}

interface

{$IFDEF pmClx}
uses SysUtils, Classes, QGraphics, QControls, PlusMemoClx, PMSupportClx, ExtHilitClx, NbHilitClx;
{$ELSE}
uses SysUtils, Classes, Graphics, Controls, PlusMemo, PMSupport, ExtHilit, NbHilit;
{$ENDIF}

const SQLDelimiters: TSysCharSet = ['"', ' ', '''', '(', ')', ',', '.', '/', ':', ';',
                                   '<', '>', '[', ']', '{', '}', #9, '=', '-'];
      SQLKeywordContext    = 1;
      SQLCommentContext    = 2;
      SQLStringContext     = 3;
      SQLOperatorContext   = 4;
      NumberContext        = 5;

type
     TSQLHighlighter = class(TCustomExtHighlighter)
       private
         fSQLKeywords,
         fPreviousSQLKeywords,
         fStringsHilit,
         fComments,
         fOperators,
         fNumbers         : THighlightInfo;
         fKeywordsOvr     : TExtKeywordList;  // provided only for the purpose of design time editing of individual keyword attributes
         fApplyHChange    : Boolean;
         fNumHilit        : TNumberHighlighter;
         fUseSharpAsComment: Boolean;

         procedure ReadNewFormats(Reader: TReader);
         procedure WriteFormats(Writer: TWriter);

         procedure HighlightChange(Sender: TObject);

         procedure setSQLKeywords(h: THighlightInfo);
         procedure setStringsHilit(h: THighlightInfo);
         procedure setComments(h: THighlightInfo);
         procedure setNumbers(h: THighlightInfo);
         procedure setOperators(h: THighlightInfo);
    procedure SetUseSharpAsComment(const Value: Boolean);

       protected
         procedure Loaded; override;
         procedure DefineProperties(Filer: TFiler); override;
         procedure ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);  override;

       public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

       published
         property SQLKeywords   : THighlightInfo  read fSQLKeywords   write setSQLKeywords;
         property Strings       : THighlightInfo  read fStringsHilit  write setStringsHilit;
         property Comments      : THighlightInfo  read fComments      write setComments;
         property Numbers       : THighlightInfo  read fNumbers       write setNumbers;
         property Operators     : THighlightInfo  read fOperators     write setOperators;
         property KeywordsOvr   : TExtKeywordList read fKeywordsOvr   write fKeywordsOvr; // provided only for the purpose of design time editing of keyword attributes
         property Scope;        // this property is not functional in this version
         property SubHighlighter;  { promoted from TCustomExtHighlighter to allow nested highlighting, ex:
                                     url highlighter within comment sections }
         property UseSharpAsComment: Boolean read FUseSharpAsComment write SetUseSharpAsComment;
       end;

  TConfirmFunc = function (Msg: AnsiString): Boolean;

var
  ConfirmFunc: TConfirmFunc = nil;

implementation

const
  SQLKeywordCount = 212;
  SQLKeywordsArray: array[0..SQLKeywordCount-1] of string =
       ('ACTIVE', 'ADD', 'ALL', 'AFTER',
        'ALTER', 'AND', 'ANY', 'AS',
        'ASCASCENDING', 'AT', 'AUTO',
        'AUTOINC', 'AVG', 'BASE_NAME', 'BEFORE',
        'BEGIN', 'BETWEEN', 'BLOB', 'BOOLEAN',
        'BOTH', 'BY', 'BYTES', 'CACHE',
        'CAST', 'CHAR', 'CHARACTER', 'CHECK',
        'CHECK_POINT_LENGTH', 'COLLATE', 'COLUMN', 'COMMIT',
        'COMMITTED', 'COMPUTED', 'CONDITIONAL', 'CONSTRAINT',
        'CONTAINING', 'COUNT', 'CREATE', 'CSTRING',
        'CURRENT', 'CURSOR', 'DATABASE', 'DATE',
        'DAY', 'DEBUG', 'DEC', 'DECIMAL',
        'DECLARE', 'DEFAULT', 'DELETE', 'DESC',
        'DESCENDING', 'DISTINCT DO', 'DOMAIN', 'DOUBLE',
        'DROP', 'ELSE', 'END', 'ENTRY_POINT',
        'ESCAPE', 'EXCEPTION', 'EXECUTE', 'EXISTS',
        'EXIT', 'EXTERNAL', 'EXTRACT', 'FILE',
        'FILTER', 'FLOAT', 'FOR', 'FOREIGN',
        'FROM', 'FULL', 'FUNCTION', 'GDSCODE',
        'GENERATOR', 'GEN_ID', 'GRANT', 'GROUP',
        'GROUP_COMMIT_WAIT_TIME', 'HAVING', 'HOUR', 'IF',
        'ININT', 'INACTIVE', 'INDEX', 'INNER',
        'INPUT_TYPE', 'INSERT', 'INTEGER', 'INTO',
        'IS', 'ISOLATION', 'JOIN', 'KEY',
        'LONG', 'LENGTH', 'LOGFILE', 'LOWER',
        'LEADING', 'LEFT', 'LEVEL', 'LIKE',
        'LOG_BUFFER_SIZE', 'MANUAL', 'MAX', 'MAXIMUM_SEGMENT',
        'MERGE', 'MESSAGE', 'MIN', 'MINUTE',
        'MODULE_NAME', 'MONEY', 'MONTH', 'NAMES',
        'NATIONAL', 'NATURAL', 'NCHAR', 'NO',
        'NOT', 'NULL', 'NUM_LOG_BUFFERS', 'NUMERIC',
        'OF', 'ON', 'ONLY', 'OPTION',
        'OR', 'ORDER', 'OUTER', 'OUTPUT_TYPE',
        'OVERFLOW', 'PAGE_SIZE', 'PAGE', 'PAGES',
        'PARAMETER', 'PASSWORD', 'PLAN', 'POSITION',
        'POST_EVENT', 'PRECISION', 'PROCEDURE', 'PROTECTED',
        'PRIMARY', 'PRIVILEGES', 'RAW_PARTITIONS', 'RDB$DB_KEY',
        'READ', 'REAL', 'RECORD_VERSION', 'REFERENCES',
        'RESERV', 'RESERVING', 'RETAIN', 'RETURNING_VALUES',
        'RETURNS', 'REVOKE', 'RIGHT ROLLBACK', 'SCHEMA',
        'SECOND', 'SEGMENT', 'SELECT', 'SET',
        'SHADOW',  'SHARED', 'SHOW', 'SINGULAR', 'SIZE',
        'SMALLINT', 'SNAPSHOT', 'SOME', 'SORT',
        'SQLCODE', 'STABILITY', 'STARTING', 'STARTS',
        'STATISTICS', 'SUB_TYPE', 'SUBSTRING', 'SUM',
        'SUSPEND', 'TABLE', 'THEN', 'TIME',
        'TIMESTAMP', 'TIMEZONE_HOUR', 'TIMEZONE_MINUTE', 'TO',
        'TRAILING', 'TRANSACTION', 'TRIGGER', 'TRIM',
        'UNCOMMITTED', 'UNION', 'UNIQUE', 'UPDATE',
        'UPPER', 'USER', 'VALUE', 'VALUES',
        'VARCHAR', 'VARIABLE', 'VARYING', 'VIEW',
        'WAIT', 'WHEN', 'WHERE', 'WHILE',
        'WITH', 'WORK', 'WRITE', 'YEAR');

  SQLOperatorCount = 8;
  SQLOperators : array[0..SQLOperatorCount-1] of string = ('(', ')', '[', ']', '/', '-', '^', '+');


function ConfirmKeywords(newHighlight, oldHighlight: THighlightInfo): Boolean;
var msg: AnsiString; savedOnChange: TNotifyEvent;
begin
  Result:= True;
  if not Assigned(ConfirmFunc) then Exit;
  msg:= 'Are you sure you want to change ';
  if (newHighlight.AltFont<>oldHighlight.AltFont) or (newHighlight.Style<>oldHighlight.Style) then
    msg:= msg + 'the style '
  else
    if (newHighlight.Background<>oldHighlight.Background) or
       (newHighlight.Foreground<>oldHighlight.Foreground) then
         msg:= msg + 'the color '
    else Exit;

  Result:= ConfirmFunc(msg+ 'of all your keywords?');
  if not Result then
    with newHighlight do
      begin
        savedOnChange:= OnChange;
        OnChange:= nil;
        Assign(oldHighlight);
        OnChange:= savedOnChange
      end
end;

constructor TSQLHighlighter.Create(AOwner: TComponent);
  procedure CreateHighlight(var h: THighlightInfo; style: TExtFontStyles; bk, fr: TColor);
    var newstyle: TPlusFontStyles;
    begin
      h:= THighlightInfo.Create;
      h.AltFont:=  TPlusFontStyle(fsAltFont) in TPlusFontStyles(style);
      newstyle:= TPlusFontStyles(style) - [TPlusFontStyle(fsAltFont)];
      h.Style:= TFontStyles(newstyle);
      h.Background:= bk;
      h.Foreground:= fr;
      h.OnChange:= HighlightChange
    end;

var i: Integer;
begin
  inherited Create(AOwner);

  Delimiters:= SQLDelimiters;

  { Install keywords }
  Keywords.BeginUpdate;
  for i:= 0 to SQLKeyWordCount-1 do
    Keywords.AddExtKeyword(SQLKeyWordsArray[i], [woWholeWordsOnly], [fsBold],  SQLKeywordContext, 0, 0,
                           crDefault, -1, -1);

  for i:= 0 to SQLOperatorCount-1 do
    Keywords.AddExtKeyword(SQLOperators[i], [], [fsBold], SQLOperatorContext, 0, 0, crDefault, -1, clRed);

  Keywords.EndUpdate;

  { Install start-stop keys }
  StartStopKeys.AddExStartStopKey('"', '"', [], [], SQLStringContext, 0, 0, crDefault, clSilver, clBlack, [ssoParStop]);
  StartStopKeys.AddExStartStopKey('''', '''', [], [], SQLStringContext, 0, 0, crDefault, clSilver, clBlack, [ssoParStop]);
  StartStopKeys.AddExStartStopKey('/*', '*/', [], [fsItalic], SQLCommentContext, 0, 0, crDefault, -1, clGreen, []);
      // This one is a place holder for # style comments
  StartStopKeys.AddExStartStopKey(#1, '', [], [fsItalic], SQLCommentContext, 0, 0, crDefault, -1, clGreen, [ssoParStop]);
  StartStopKeys.AddExStartStopKey('--', '', [], [fsItalic], SQLCommentContext, 0, 0, crDefault, -1, clGreen, [ssoParStop]);

  fKeywordsOvr:= TExtKeywordList.Create;

  { Create the THighlightInfo we use }
  CreateHighlight(fSQLKeywords, [fsBold], -1, -1);
  CreateHighlight(fPreviousSQLKeywords, [fsBold], -1, -1);
  fPreviousSQLKeywords.OnChange:= nil;

  CreateHighlight(fStringsHilit, [], clSilver, clBlack);
  CreateHighlight(fComments, [fsItalic], -1, clBlue);
  CreateHighlight(fNumbers, [], -1, clRed);
  CreateHighlight(fOperators, [fsBold], -1, clRed);

  { Create internal TNumberHighlighter }
  fNumHilit:= TNumberHighlighter.Create(Self);
  with fNumHilit do
    begin
      Delimiters:= SQLDelimiters;
      ForeGround:= clRed;
      ContextNum:= NumberContext;
    end;
end;


destructor TSQLHighlighter.Destroy;
begin
  fStringsHilit.Free;
  fSQLKeywords.Free;
  fComments.Free;
  fPreviousSQLKeywords.Free;
  fNumbers.Free;
  fKeywordsOvr.Free;
  fNumHilit.Free;
  fOperators.Free;
  inherited Destroy
end;

procedure TSQLHighlighter.Loaded;
var i: Integer; kinfo: TKeywordInfo;
begin
  fPreviousSQLKeywords.Assign(fSQLKeywords);

  { update internal keyword and start-stop key lists }

  { update SQL keywords, only those that were not streamed in
    for those having been read from stream, turn off the flag that says so. }
  kinfo.Options:= [woWholeWordsOnly];
  kinfo.Style:= fSQLKeywords.Style;
  if fSQLKeywords.AltFont then TPlusFontStyles(kinfo.Style):= TPlusFontStyles(kinfo.Style) + [TPlusFontStyle(fsAltFont)];
  kinfo.ContextNumber:= SQLKeywordContext;
  kinfo.Cursor:= crDefault;
  kinfo.Backgnd:= fSQLKeywords.Background;
  kinfo.Foregnd:= fSQLKeywords.Foreground;

  for i:= 0 to SQLKeywordCount-1 do
      with pKeyInfoLen(Keywords.KeyList[i])^, BasicPart do
          if Byte(Style) and $80 = 0 then BasicPart:= kinfo
                                     else Byte(Style):= Byte(Style) and $7f;


  fApplyHChange:= True;
  { update start-stop keys through the HighlightChange handler }
  HighlightChange(fStringsHilit);
  HighlightChange(fOperators);
  HighlightChange(fComments);
  HighlightChange(fNumbers);
  fApplyHChange:= False;
  inherited Loaded;    { note: this will also call ReApplyKeys }
end;

procedure TSQLHighlighter.setSQLKeywords(h: THighlightInfo);
begin
  fSQLKeywords.Assign(h)
end;

procedure TSQLHighlighter.setOperators(h: THighlightInfo);
begin
  fOperators.Assign(h)
end;

procedure TSQLHighlighter.setStringsHilit(h: THighlightInfo);
begin
  fStringsHilit.Assign(h)
end;

procedure TSQLHighlighter.setComments(h: THighlightInfo);
begin
  fComments.Assign(h)
end;

procedure TSQLHighlighter.setNumbers(h: THighlightInfo);
begin
  fNumbers.Assign(h)
end;

procedure TSQLHighlighter.setUseSharpAsComment(const Value: Boolean);
var c: Char;
begin
  if Value<>fUseSharpAsComment then
    begin
      fUseSharpAsComment := Value;
      if Value then c:= '#'
               else c:= #1;
      pStartStopInfo(StartStopKeys.Pointers[3]).StartKey^:= c;
      if (csDesigning in ComponentState) and (not (csLoading in ComponentState)) then ReApplyKeys
    end
end;



procedure TSQLHighlighter.HighlightChange(Sender: TObject);
  { event handler for internal THighlightInfo modifications }

  procedure setkeyword(var kw: TKeyInfoLen; h: THighlightInfo);
    begin    { set keyword formatting info according to values of h }
      kw.BasicPart.Style:= h.Style;
      kw.BasicPart.Backgnd:= h.Background;
      kw.BasicPart.Foregnd:= h.Foreground;
    end;

  procedure updatekeyword(var kw: TKeyInfoLen; oldHighlight, newHighlight: THighlightInfo);
    begin    { set keyword formatting info according to values of newHighlight, only
               for attributes that differ from oldHighlight }
      kw.BasicPart.Style:= kw.BasicPart.Style + (newHighlight.Style-oldHighlight.Style)
                                    - (oldHighlight.Style-newHighlight.Style);

      if oldHighlight.AltFont<>newHighlight.AltFont then
        if newHighlight.AltFont then Include(kw.BasicPart.Style, TFontStyle(fsAltFont))
                                else Exclude(kw.BasicPart.Style, TFontStyle(fsAltFont));

      if oldHighlight.Background<>newHighlight.Background then
                              kw.BasicPart.Backgnd:= newHighlight.Background;

      if oldHighlight.Foreground<>newHighlight.Foreground then
                              kw.BasicPart.Foregnd:= newHighlight.Foreground
    end;

  procedure setssinfo(var ss: StartStopInfo; aHighlight: THighlightInfo);
    begin   { set ss formatting info according to values of aHighlight }
      ss.Attributes.Style:= aHighlight.Style;
      if aHighlight.AltFont then Include(ss.Attributes.Style, TFontStyle(fsAltFont));
      ss.Attributes.Backgnd:= aHighlight.Background;
      ss.Attributes.Foregnd:= aHighlight.Foreground
    end;

var i, sstart, sstop: Integer;
begin    { HighlightChange }
  if (csLoading in ComponentState) and (not fApplyHChange) then Exit;

  if Sender=fSQLKeywords then
    begin
      if csDesigning in ComponentState then
        if not ConfirmKeywords(fSQLKeywords, fPreviousSQLKeywords) then Exit;

      { update the SQL keywords highlight attributes }
      for i:= 0 to SQLKeywordCount-1 do
          updatekeyword(pKeyInfoLen(Keywords.KeyList[i])^, fPreviousSQLKeywords, fSQLKeywords);
      fPreviousSQLKeywords.Assign(fSQLKeywords)
    end;

  if Sender=fOperators then
      { update the SQL operators highlight attributes }
      for i:= SQLKeywordCount to SQLKeywordCount+SQLOperatorCount-1 do
          setkeyword(pKeyInfoLen(Keywords.KeyList[i])^, fOperators);

  { determine which start-stop key needs to be updated }
  sstart:= 0;
  sstop:= -1;

  if Sender=fComments then
    begin
      sstart:= 2;
      sstop:= 4
    end
  else
    if Sender=fStringsHilit then
      begin
        sstart:= 0;
        sstop:= 1
      end;

  for i:= sstart to sstop do
      setssinfo(pStartStopInfo(StartStopKeys.Pointers[i])^, THighlightInfo(Sender));

  if Sender=fNumbers then
    with fNumHilit do
      begin
        Background:= fNumbers.Background;
        Foreground:= fNumbers.Foreground;
        Style:= fNumbers.Style;
        AltFont:= fNumbers.AltFont
      end;

  if (csDesigning in ComponentState) and (not (csLoading in ComponentState)) then ReApplyKeys
end;

procedure TSQLHighlighter.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ModFormats', ReadNewFormats, WriteFormats, True)
end;

procedure TSQLHighlighter.ReadNewFormats(Reader: TReader);
var kindex: Integer; k: TKeywordInfo;
begin
  Reader.ReadListBegin;
  while not Reader.EndOfList do
    begin
      kindex:= Reader.ReadInteger;
      with k do
        begin
          Byte(Options):= Reader.ReadInteger;
          Byte(Style):= Reader.ReadInteger or $80; { a flag to mark as loaded from stream }
          ContextNumber:= Reader.ReadInteger;
          Cursor:= Reader.ReadInteger;
          Backgnd:= Reader.ReadInteger;
          Foregnd:= Reader.ReadInteger
        end;
      if kindex<Keywords.Count then pKeyInfoLen(Keywords.KeyList[kindex])^.BasicPart:= k;
    end;
  Reader.ReadListEnd;
end;

procedure TSQLHighlighter.WriteFormats(Writer: TWriter);
  function samehinfo(const k: TKeywordInfo; hilit: THighlightInfo): Boolean;
    var st: TExtFontStyles;
    begin
      st:= hilit.Style;
      if hilit.AltFont then TPlusFontStyles(st):= TPlusFontStyles(st) + [TPlusFontStyle(fsAltFont)];
      Result:= (k.Backgnd=hilit.Background) and (k.Foregnd=hilit.Foreground) and (k.Style=st)
    end;

var i: Integer;

type pKeyInfo = ^TKeywordInfo;
begin
  Writer.WriteListBegin;
  for i:= 0 to Keywords.Count-1 do
    with pKeyInfoLen(Keywords.KeyList[i])^ do
      begin
        if not samehinfo(BasicPart, SQLKeywords) then
          begin
            Writer.WriteInteger(i);
            Writer.WriteInteger(Byte(BasicPart.Options));
            Writer.WriteInteger(Byte(BasicPart.Style));
            Writer.WriteInteger(BasicPart.ContextNumber);
            Writer.WriteInteger(BasicPart.Cursor);
            Writer.WriteInteger(BasicPart.Backgnd);
            Writer.WriteInteger(BasicPart.Foregnd)
          end
      end;
  Writer.WriteListEnd
end;

procedure TSQLHighlighter.ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);
begin
  inherited ApplyKeywordsList(Start, Stop, BaseIndex);
  TSQLHighlighter(fNumHilit).ApplyKeywordsList(Start, Stop, BaseIndex)
end;


end.
