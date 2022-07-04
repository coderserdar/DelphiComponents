unit OOPHilit;

{ © Electro-Concept Mauricie, 1999-2003 }
{ Implements TOOPHighlighter object, used for Delphi like syntax highlighting
  in a TPlusMemo }

{ Note: to install this component on your palette, add file OOPReg to your package }

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$DEFINE OOPHilit}
{UCONVERT}
  {$IFDEF OOPHilitClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}

interface

{$IFDEF pmClx}
uses SysUtils, Classes, QGraphics, QControls, PlusMemoClx, PMSupportClx, ExtHilitClx, NbHilitClx;
{$ELSE}
uses SysUtils, Classes, Graphics, Controls, PlusMemo, PMSupport, ExtHilit, NbHilit;
{$ENDIF}

const OOPDelimiters: TSysCharSet = ['"', ' ', '''', '(', ')', ',', '.', '/', ':', ';',
                                   '<', '>', '[', ']', '{', '}', #9, '=', '-', '+', '*', '^'];
      OOPKeywordContext    = 1;
      PascalStringContext  = 2;
      OOPCommentContext    = 3;
      DirectiveContext     = 4;
      NumberContext        = 5;

      ASMContext           = 6;
      ASMKeywordContext    = 7;
      ASMStringContext     = 8;
      ASMCommentContext    = 3;
      OOPSymbolContext     = 9;

type
     TOOPKeywordList = class(TPersistent) { a class provided for the purpose of editing individual keyword formats }
        protected
          ParentHighlighter: TCustomExtHighlighter;
          procedure DefineProperties(Filer: TFiler); override;
          procedure WriteData(Writer: TWriter);
        end;

     TOOPHighlighter = class(TCustomExtHighlighter)
       private
         fOOPKeywords,       fPreviousOOPKeywords,
         fASMKeywords,       fPreviousASMKeywords,
         fAsmHighlight,      fStringsHilit,
         fCommentSingleLine, fCommentBrace,
         fCommentParAst,     fDirectives,
         fNumbers,           fProperties,
         fSymbols           : THighlightInfo;

         fKeywordsOvr       : TOOPKeywordList;
         fApplyHChange      : Boolean;

         fNumHilit          : TNumberHighlighter;

         procedure ReadOldFormats(Reader: TReader);
         procedure ReadNewFormats(Reader: TReader);
         procedure WriteFormats(Writer: TWriter);

         procedure HighlightChange(Sender: TObject);

         procedure setOOPKeywords(h: THighlightInfo);
         procedure setASMKeywords(h: THighlightInfo);
         procedure setASMHighlight(h: THighlightInfo);
         procedure setStringsHilit(h: THighlightInfo);
         procedure setCommentSingleLine(h: THighlightInfo);
         procedure setCommentBrace(h: THighlightInfo);
         procedure setCommentParAst(h: THighlightInfo);
         procedure setDirectives(h: THighlightInfo);
         procedure setNumbers(h: THighlightInfo);
         procedure setProperties(h: THighlightInfo);

         procedure GetKeywordConfirmation(Sender: TObject; StartKey, StopKey: TPlusNavigator; KIndex: Integer;
                                          var Accept: Boolean);
         procedure GetNumberConfirmation(Sender: TObject; Scope: SmallInt; var DoHilit: Boolean);
         procedure setSymbols(const Value: THighlightInfo);

       protected
         procedure Loaded; override;
         procedure DefineProperties(Filer: TFiler); override;
         procedure ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);  override;
         function  FindStop(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean; override;
                   // process # type strings differently
       public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

       published
         property KeywordsOOP   : THighlightInfo  read fOOPKeywords   write setOOPKeywords;
         property KeywordsASM   : THighlightInfo  read fASMKeywords   write setASMKeywords;
         property ASMHighlight  : THighlightInfo  read fASMHighlight  write setASMHighlight;
         property Strings       : THighlightInfo  read fStringsHilit  write setStringsHilit;
         property CommentSingleLine: THighlightInfo read fCommentSingleLine write setCommentSingleLine;
         property CommentBrace  : THighlightInfo  read fCommentBrace  write setCommentBrace;
         property CommentParAst : THighlightInfo  read fCommentParAst write setCommentParAst;
         property Directives    : THighlightInfo  read fDirectives    write setDirectives;
         property Numbers       : THighlightInfo  read fNumbers       write setNumbers;
         property Properties    : THighlightInfo  read fProperties    write setProperties;
         property KeywordsOvr   : TOOPKeywordList read fKeywordsOvr   write fKeywordsOvr;
         property SubHighlighter;  { promoted from TCustomExtHighlighter to allow nested highlighting, ex:
                                     url highlighter within comment sections }
         property Symbols       : THighlightInfo  read fSymbols       write setSymbols;
       end;

  TConfirmFunc = function (Msg: AnsiString): Boolean;

var
  ConfirmFunc: TConfirmFunc = nil;    // for design time confirmation of global keyword modifications

implementation

const
      OOPKeywordCount = 85;
      OOPKeywordList: array[0..OOPKeywordCount-1] of string =
           ('and', 'array', 'as', 'automated',      { note: asm keyword must not be there... }
            'begin', 'case', 'class', 'const',
            'constructor', 'default', 'destructor', 'dispinterface', 
            'div', 'do', 'downto', 'dynamic', 
            'else', 'end', 'except', 'exports',
            'far', 'file', 'finalization', 'finally', 
            'for', 'function', 'goto', 'if', 
            'implementation', 'in', 'inherited', 'initialization',
            'inline', 'interface', 'is', 'label',
            'library', 'mod', 'message', 'nil', 
            'not', 'object',  'of', 'on',
            'or', 'out', 'overload', 'override',
            'packed', 'private', 'procedure', 'program',
            'property', 'protected', 'public', 'published', 
            'raise', 'record', 'register', 'repeat', 
            'resourcestring', 'set', 'shl', 'shr', 
            'string', 'then', 'threadvar', 'to', 
            'try', 'type', 'unit', 'until', 
            'uses', 'var', 'virtual', 'while', 
            'with', 'xor', 'absolute',
            'implements', 'index', 'nodefault', 'read',
            'stored', 'write');   { these last six highlighted only inside of property section }

      PropertyKeywordsIndex = 79;    { index of keywords highlighted only in property sections }

      OOPSymbolCount = 15;
      OOPSymbols: array[0..OOPSymbolCount-1] of string =
           ('(', ')', ',', '.', ';', ':', '=', '[', ']', '<', '>', '+', '-', '*', '^');

      ASMKeywordCount = 166;
      ASMKeywordList: array[0..ASMKeywordCount-1] of string =
           (  'ASM', 'END',
              'AAA', 'AAS', 'AAD', 'AAM',        'ADC', 'ADD', 'AND', 'BOUND',
              'BSF', 'BSR', 'BT', 'BTC',         'BTR', 'BTS', 'CALL', 'CMP',
              'CBW', 'CWD', 'CDQ',  'CWDE',      'CLC', 'CLD', 'CLI',  'CMC',
              'CMPSB', 'CMPSD', 'CMPSW', 'DAA',  'DAS', 'DEC', 'DIV', 'ENTER',
              'HLT', 'IDIV', 'IMUL', 'IN',       'INC', 'INSB', 'INSW', 'INSD',
              'INT', 'INTO', 'IRET', 'IRETD',    'JA',  'JAE', 'JBE',  'JC',
              'JE',  'JCXZ','JG',  'JGE',        'JMP', 'JO',  'JNA', 'JNO',
              'JB',  'JNAE', 'JNB', 'JNC',       'JZ',  'JNE',  'JNZ', 'JNBE',
              'JS', 'JNS',   'JP',  'JPE',       'JNP', 'JPO',  'JL',  'JNGE',
              'JNL', 'JLE', 'JNG', 'JNLE',       'JECXZ', 'LAHF', 'LEA', 'LEAVE',
              'LDS', 'LES', 'LFS', 'LGS',        'LODSB', 'LODSW', 'LODSD', 'LOOP',
              'LOCK',   'REP',   'REPE', 'REPZ', 'REPNE',  'REPNZ', 'SEGES',  'SEGCS',
              'SEGSS',  'SEGDS', 'SEGFS','SEGGS', 'LOOPE', 'LOOPZ', 'LOOPNE', 'LOOPNZ',
              'LOOPD', 'LOOPDE', 'LOOPDZ', 'LOOPDNE','LOOPDNZ', 'MOV', 'MOVSX', 'MOVZX',
              'MOVSB', 'MOVSW', 'MOVSD', 'MUL',   'NEG', 'NOP', 'NOT', 'OR',
              'OUT', 'OUTSB', 'OUTSW', 'OUTSD',   'POP', 'POPF', 'POPA', 'POPAD',
              'POPFD', 'PUSH', 'PUSHF', 'PUSHA',  'PUSHAD', 'PUSHFD', 'RET', 'RETN',
              'RETF', 'SUB', 'SBB', 'RCL',        'RCR', 'ROL', 'ROR', 'SAL',
              'SHL', 'SAR', 'SHR', 'SHLD',        'SHRD', 'SAHF', 'SCASB', 'SCASW',
              'SCASD', 'STC', 'STD', 'STI',       'STOSB', 'STOSW', 'STOSD', 'TEST',
              'WAIT', 'XCHG', 'XLAT', 'XOR');

      AsmKIndex = OOPKeywordCount;
      AsmSSIndex = 12;    { index of asm ... end in StartStopKeys list }

function ConfirmKeywords(newHighlight, oldHighlight: THighlightInfo; kkind: string): Boolean;
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

  Result:= ConfirmFunc(msg+'of all your keywords?');
  if not Result then
    with newHighlight do
      begin
        savedOnChange:= OnChange;
        OnChange:= nil;
        Assign(oldHighlight);
        OnChange:= savedOnChange
      end
end;

constructor TOOPHighlighter.Create(AOwner: TComponent);
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

  fKeywordsOvr:= TOOPKeywordList.Create;
  fKeywordsOvr.ParentHighlighter:= Self;

  OnKeyword:= GetKeywordConfirmation;  { special treatment for asm, index, read, write keywords }
  Delimiters:= OOPDelimiters;

  { Install keywords:
    Keywords contains first the Pascal keywords, followed by Assembler keywords, then OOPSymbols }

  Keywords.BeginUpdate;
  for i:= 0 to OOPKeyWordCount-1 do
    Keywords.AddExtKeyword(OOPKeyWordList[i], [woWholeWordsOnly], [fsBold],  OOPKeywordContext, 0, 0,
                                     crDefault, -1, -1);

  for i:= 0 to ASMKeywordCount-1 do
    Keywords.AddExtKeyword(ASMKeywordList[i], [woWholeWordsOnly], [fsBold], ASMKeywordContext, ASMContext, 0,
                                    crDefault, clYellow, -1);

  for i:= 0 to OOPSymbolCount-1 do
    inherited Keywords.AddExtKeyWord(OOPSymbols[i], [], [], OOPSymbolContext, 0, 0, crDefault, -1, -1);

  Keywords.EndUpdate;

  { Install start-stop keys }
  StartStopKeys.AddExStartStopKey('property', ';', [woWholeWordsOnly], [], 0, 0, 0, crDefault,
                                        -1, -1, []);

  StartStopKeys.AddExStartStopKey('''', '''', [], [fsItalic], PascalStringContext, 0, 0, crDefault,
                                        clSilver, clBlack, [ssoParStop]);
  StartStopKeys.AddExStartStopKey('#', '', [], [fsItalic], PascalStringContext, 0, 0, crDefault,
                                        clSilver, clBlack, [ssoParStop]);

  StartStopKeys.AddExStartStopKey('//', '', [], [fsItalic], OOPCommentContext, 0, 0, crDefault,
                                        -1, clBlue, [ssoParStop]);

  StartStopKeys.AddExStartStopKey('{$', '}', [], [fsItalic], DirectiveContext, 0, 0, crDefault,
                                        clTeal, clWhite, []);

  StartStopKeys.AddExStartStopKey('{', '}', [], [fsItalic], OOPCommentContext, 0,0, crDefault,
                                        -1, clNavy, []);

  StartStopKeys.AddExStartStopKey('(*', '*)', [], [fsItalic], OOPCommentContext, 0, 0, crDefault,
                                        -1, clGreen, []);

  StartStopKeys.AddExStartStopKey('''', '''', [], [fsItalic], ASMStringContext, ASMContext,0, crDefault,
                                        clSilver, clBlack, [ssoParStop]);

  StartStopKeys.AddExStartStopKey('//', '', [], [fsItalic], ASMCommentContext, ASMContext, 0, crDefault,
                                        -1, clBlue, [ssoParStop]);

  StartStopKeys.AddExStartStopKey('{$', '}', [], [fsItalic], DirectiveContext, ASMContext, 0, crDefault,
                                        clTeal, clWhite, []);

  StartStopKeys.AddExStartStopKey('{', '}', [], [fsItalic], ASMCommentContext, ASMContext, 0, crDefault,
                                        -1, clNavy, []);

  StartStopKeys.AddExStartStopKey('(*', '*)', [], [fsItalic], ASMCommentContext, ASMContext, 0, crDefault,
                                        -1, clGreen, []);

  StartStopKeys.AddExStartStopKey('asm', 'end', [woWholeWordsOnly], [fsUnderline], ASMContext, 0, 0, crDefault,
                                        -1, clPurple, []);

  { Create the THighlightInfo we use }
  CreateHighlight(fOOPKeywords, [fsBold], -1, -1);
  CreateHighlight(fPreviousOOPKeywords, [fsBold], -1, -1);
  fPreviousOOPKeywords.OnChange:= nil;

  CreateHighlight(fASMKeywords, [fsBold], clYellow, -1);
  CreateHighlight(fPreviousASMKeywords, [fsBold], clYellow, -1);
  fPreviousASMKeywords.OnChange:= nil;

  CreateHighlight(fASMHighlight, [], -1, clPurple);
  CreateHighlight(fStringsHilit, [fsItalic], clSilver, clBlack);
  CreateHighlight(fCommentSingleLine, [fsItalic], -1, clBlue);
  CreateHighlight(fDirectives, [fsItalic], clTeal, clWhite);
  CreateHighlight(fCommentBrace, [fsItalic], -1, clNavy);
  CreateHighlight(fCommentParAst, [fsItalic], -1, clGreen);
  CreateHighlight(fNumbers, [], -1, clRed);
  CreateHighlight(fProperties, [], -1, -1);
  CreateHighlight(fSymbols, [], -1, -1);

  { Create internal TNumberHighlighter }
  fNumHilit:= TNumberHighlighter.Create(Self);
  with fNumHilit do
    begin
      Delimiters:= OOPDelimiters;
      ForeGround:= clRed;
      ContextNum:= NumberContext;
      OnQueryHilit:= GetNumberConfirmation;  { highlight numbers in asm and base parts as well }
    end;
end;


destructor TOOPHighlighter.Destroy;
begin
  fAsmHighlight.Free;
  fStringsHilit.Free;
  fCommentBrace.Free;
  fCommentParAst.Free;
  fCommentSingleLine.Free;
  fDirectives.Free;
  fASMKeywords.Free;
  fOOPKeywords.Free;
  fPreviousASMKeywords.Free;
  fPreviousOOPKeywords.Free;
  fNumbers.Free;
  fKeywordsOvr.Free;
  fNumHilit.Free;
  fProperties.Free;
  fSymbols.Free;
  inherited Destroy
end;

procedure TOOPHighlighter.Loaded;
var i: Integer; kinfo: TKeywordInfo;
begin
  fPreviousOOPKeywords.Assign(fOOPKeywords);
  fPreviousASMKeywords.Assign(fASMKeywords);

  { update internal keyword and start-stop key lists }

  { update OOP keywords, only those that were not streamed in
    for those having been read from stream, turn off the flag that says so. }
  kinfo.Options:= [woWholeWordsOnly];
  kinfo.Style:= fOOPKeywords.Style;
  if fOOPKeywords.AltFont then TPlusFontStyles(kinfo.Style):= TPlusFontStyles(kinfo.Style) + [TPlusFontStyle(fsAltFont)];
  kinfo.ContextNumber:= OOPKeywordContext;
  kinfo.Cursor:= crDefault;
  kinfo.Backgnd:= fOOPKeywords.Background;
  kinfo.Foregnd:= fOOPKeywords.Foreground;

  for i:= 0 to OOPKeywordCount-1 do
      with pKeyInfoLen(Keywords.KeyList[i])^, BasicPart do
          if Byte(Style) and $80 = 0 then BasicPart:= kinfo
                                     else Byte(Style):= Byte(Style) and $7f;

  { update ASM keywords, only those that were not streamed in }
  kinfo.Style:= fASMKeywords.Style;
  if fASMKeywords.AltFont then TPlusFontStyles(kinfo.Style):= TPlusFontStyles(kinfo.Style) + [TPlusFontStyle(fsAltFont)];
  kinfo.ContextNumber:= ASMKeywordContext;
  kinfo.Backgnd:= fASMKeywords.Background;
  kinfo.Foregnd:= fASMKeywords.Foreground;

  for i:= OOPKeywordCount to OOPKeywordCount+ASMKeywordCount-1 do
      with pKeyInfoLen(Keywords.KeyList[i])^, BasicPart do
          if Byte(Style) and $80 = 0 then BasicPart:= kinfo
                                     else Byte(Style):= Byte(Style) and $7f;

  fApplyHChange:= True;
  { update start-stop keys through the HighlightChange handler }
  HighlightChange(fASMHighlight);
  HighlightChange(fStringsHilit);
  HighlightChange(fCommentSingleLine);
  HighlightChange(fCommentBrace);
  HighlightChange(fCommentParAst);
  HighlightChange(fNumbers);
  HighlightChange(fDirectives);
  HighlightChange(fProperties);

  fApplyHChange:= False;
  inherited Loaded;    { note: this will also call ReApplyKeys }
end;

procedure TOOPHighlighter.setOOPKeywords(h: THighlightInfo);
begin
  fOOPKeywords.Assign(h)
end;

procedure TOOPHighlighter.setASMKeywords(h: THighlightInfo);
begin
  fASMKeywords.Assign(h)
end;

procedure TOOPHighlighter.setASMHighlight(h: THighlightInfo);
begin
  fASMHighlight.Assign(h)
end;

procedure TOOPHighlighter.setStringsHilit(h: THighlightInfo);
begin
  fStringsHilit.Assign(h)
end;

procedure TOOPHighlighter.setCommentSingleLine(h: THighlightInfo);
begin
  fCommentSingleLine.Assign(h)
end;

procedure TOOPHighlighter.setCommentBrace(h: THighlightInfo);
begin
  fCommentBrace.Assign(h)
end;

procedure TOOPHighlighter.setCommentParAst(h: THighlightInfo);
begin
  fCommentParAst.Assign(h)
end;

procedure TOOPHighlighter.setDirectives(h: THighlightInfo);
begin
  fDirectives.Assign(h)
end;

procedure TOOPHighlighter.setNumbers(h: THighlightInfo);
begin
  fNumbers.Assign(h)
end;

procedure TOOPHighlighter.setProperties(h: THighlightInfo);
begin
  fProperties.Assign(h)
end;

procedure TOOPHighlighter.setSymbols(const Value: THighlightInfo);
begin
  fSymbols.Assign(Value);
end;


procedure TOOPKeywordList.DefineProperties(Filer: TFiler);
{ for compatibility with 4.1 and before }
begin
  Filer.DefineProperty('Formats', TOOPHighlighter(ParentHighlighter).ReadOldFormats, WriteData, False)
end;


procedure TOOPKeywordList.WriteData(Writer: TWriter);
begin
  { nothing to do }
end;


procedure TOOPHighlighter.HighlightChange(Sender: TObject);
  { event handler for internal THighlightInfo modifications }

  procedure setkeyword(var kw: TKeyInfoLen; oldHighlight, newHighlight: THighlightInfo);
    begin    { set keyword formatting info according to values of newHighlight, only
               for attributes that differ from oldHighlight, unless oldHighlight is nil }
      if oldhighlight=nil then
        begin
          kw.BasicPart.Style:= newHighlight.Style;
          if newHighlight.AltFont then Include(kw.BasicPart.Style, TFontStyle(fsAltFont))
                                  else Exclude(kw.BasicPart.Style, TFontStyle(fsAltFont));
          kw.BasicPart.Backgnd:= newHighlight.Background;
          kw.BasicPart.Foregnd:= newHighlight.Foreground
        end

      else
        begin
          kw.BasicPart.Style:= kw.BasicPart.Style + (newHighlight.Style-oldHighlight.Style)
                                    - (oldHighlight.Style-newHighlight.Style);

          if oldHighlight.AltFont<>newHighlight.AltFont then
            if newHighlight.AltFont then Include(kw.BasicPart.Style, TFontStyle(fsAltFont))
                                    else Exclude(kw.BasicPart.Style, TFontStyle(fsAltFont));

          if oldHighlight.Background<>newHighlight.Background then
                                  kw.BasicPart.Backgnd:= newHighlight.Background;

          if oldHighlight.Foreground<>newHighlight.Foreground then
                                  kw.BasicPart.Foregnd:= newHighlight.Foreground
        end
    end;

  procedure setssinfo(var ss: StartStopInfo; aHighlight: THighlightInfo);
    begin   { set ss formatting info according to values of aHighlight }
      ss.Attributes.Style:= aHighlight.Style;
      if aHighlight.AltFont then Include(ss.Attributes.Style, TFontStyle(fsAltFont));
      ss.Attributes.Backgnd:= aHighlight.Background;
      ss.Attributes.Foregnd:= aHighlight.Foreground
    end;

var i, ssupdate: Integer; klist: TExtKeywordList;
begin    { HighlightChange }
  if (csLoading in ComponentState) and (not fApplyHChange) then Exit;

  klist:= inherited Keywords;
  if Sender=fOOPKeywords then
    begin
      if csDesigning in ComponentState then
        if not ConfirmKeywords(fOOPKeywords, fPreviousOOPKeywords, 'OOP') then Exit;

      { update the OOP keywords highlight attributes }
      for i:= 0 to OOPKeywordCount-1 do
          setkeyword(pKeyInfoLen(klist.KeyList[i])^, fPreviousOOPKeywords, fOOPKeywords);
      fPreviousOOPKeywords.Assign(fOOPKeywords)
    end;

  if Sender=fASMKeywords then
    begin
      if csDesigning in ComponentState then
        if not ConfirmKeywords(fASMKeywords, fPreviousASMKeywords, 'ASM') then Exit;

      { update the ASM keywords highlight attributes }
      for i:= OOPKeywordCount to OOPKeywordCount+ASMKeywordCount-1 do
          setkeyword(pKeyInfoLen(klist.KeyList[i])^, fPreviousASMKeywords, fASMKeywords);
      fPreviousASMKeywords.Assign(fASMKeywords)
    end;

  if Sender= fSymbols then
      for i:= OOPKeywordCount+ASMKeywordCount to OOPKeywordCount+ASMKeywordCount+OOPSymbolCount-1 do
        setkeyword(pKeyInfoLen(klist.KeyList[i])^, nil, fSymbols);

  if Sender=fASMHighlight then
    setssinfo(pStartStopInfo(StartStopKeys.Pointers[AsmSSIndex])^, fASMHighlight);

  { determine which start-stop key needs to be updated }
  ssupdate:= -1;

  if Sender=fCommentBrace then ssupdate:= 5;
  if Sender=fCommentParAst then ssupdate:= 6;
  if Sender=fCommentSingleLine then ssupdate:= 3;
  if Sender=fDirectives then ssupdate:= 4;
  if Sender=fStringsHilit then ssupdate:= 1;
  if Sender=fProperties then ssupdate:= 0;
  
  if ssupdate>=0 then
    begin
      setssinfo(pStartStopInfo(StartStopKeys.Pointers[ssupdate])^, THighlightInfo(Sender));
      if ssupdate>=3 then setssinfo(pStartStopInfo(StartStopKeys.Pointers[ssupdate+5])^, THighlightInfo(Sender));
      if ssupdate=1 then setssinfo(pStartStopInfo(StartStopKeys.Pointers[2])^, THighlightInfo(Sender));
    end;

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

procedure TOOPHighlighter.GetKeywordConfirmation(Sender: TObject; StartKey,
  StopKey: TPlusNavigator; KIndex: Integer; var Accept: Boolean);
begin
  case KIndex of
    AsmKIndex:   Accept:= StartKey.ParOffset=StartKey.pDynAttr^.DynOffset;
                 { asm keyword, highlight only if found at start of asm section }

    PropertyKeywordsIndex..OOPKeywordCount-1:
                 Accept:= StartKey.pDynAttr^.DynStyle and $c0 = $c0
                 { read, write and default keywords highlighted only in property sections }
  end;
end;

procedure TOOPHighlighter.GetNumberConfirmation(Sender: TObject; Scope: SmallInt; var DoHilit: Boolean);
begin
  DoHilit:= (Scope=0) or (Scope=ASMContext)
end;


procedure TOOPHighlighter.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ModFormats', ReadNewFormats, WriteFormats, True)
end;

procedure TOOPHighlighter.ReadNewFormats(Reader: TReader);
var klist: TExtKeywordList; kindex: Integer; k: TKeywordInfo;
begin
  klist:= inherited Keywords;
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
      if kindex<klist.Count then pKeyInfoLen(klist.KeyList[kindex])^.BasicPart:= k;
    end;
  Reader.ReadListEnd;
end;

procedure TOOPHighlighter.ReadOldFormats(Reader: TReader);
{ reads pre-4.1 streams }
var k: TKeywordInfo; kindex: Integer; klist: TExtKeywordList;
begin
  klist:= inherited Keywords;
  Reader.ReadListBegin;
  kindex:= 0;
  while not Reader.EndOfList do
    begin
      with k do
        begin
          Byte(Options):= Reader.ReadInteger;
          Byte(Style):= Reader.ReadInteger or $80; { flag it as loaded from stream }
          ContextNumber:= Reader.ReadInteger;
          Cursor:= Reader.ReadInteger;
          Backgnd:= Reader.ReadInteger;
          Foregnd:= Reader.ReadInteger
        end;
      if kindex<klist.Count then pKeyInfoLen(klist.KeyList[kindex])^.BasicPart:= k;
      Inc(kindex)
    end;
  Reader.ReadListEnd;
end;

procedure TOOPHighlighter.WriteFormats(Writer: TWriter);
  function samehinfo(const k: TKeywordInfo; hilit: THighlightInfo): Boolean;
    var st: TExtFontStyles;
    begin
      st:= hilit.Style;
      if hilit.AltFont then TPlusFontStyles(st):= TPlusFontStyles(st) + [TPlusFontStyle(fsAltFont)];
      Result:= (k.Backgnd=hilit.Background) and (k.Foregnd=hilit.Foreground) and (k.Style=st)
    end;

var i: Integer; klist: TExtKeywordList; h: THighlightInfo;

type pKeyInfo = ^TKeywordInfo;
begin
  Writer.WriteListBegin;
  klist:= inherited Keywords;
  for i:= 0 to klist.Count-1 do
    with pKeyInfoLen(klist.KeyList[i])^ do
      begin
        if i<OOPKeywordCount then h:= fOOPKeywords
                             else h:= fASMKeywords;
        if not samehinfo(BasicPart, h) then
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

procedure TOOPHighlighter.ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);
begin
  inherited ApplyKeywordsList(Start, Stop, BaseIndex);
  TOOPHighlighter(fNumHilit).ApplyKeywordsList(Start, Stop, BaseIndex)
end;

function TOOPHighlighter.FindStop(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean;
var spstartdyn: pDynInfoRec; slevel, slim, i: Integer; dinfo: DynInfoRec;
begin
  spstartdyn:= Start.pDynAttr;
  slevel:= DynToLevel(spstartdyn^);
  if (slevel>=0) and ((spstartdyn^.KeyIndex[slevel] and $7fff) - BaseIndex = 2) then
    begin     // this is a #xx type section
      if Stop.ParNumber=Start.ParNumber then slim:= Stop.fOffset
                                        else slim:= GetParLength(Start.fPar^);
      Result:= False;
      i:= Start.fOffset;
      while (not Result) and (i<=slim) do
          if not (pmChar(Start.fPar.ParText[i]) in ['0'..'9']) then Result:= True
                                                               else Inc(i);
      if Result then
        begin
          fNav1.fPMemo:= Start.fPMemo;
          fNav1.Assign(Start);
          Dec(slevel);
          while DynToLevel(fNav1.DynAttr)>slevel do
            if not fNav1.BackToDyn(0) then Break;
          dinfo:= fNav1.DynAttr;
          Stop.ParOffset:= i;
          dinfo.StartKLen:= 0;
          Stop.AddDyn(dinfo);
          fNav1.fPMemo:= nil
        end
    end

  else
    Result:= inherited FindStop(Start, Stop, BaseIndex)
end;

end.
