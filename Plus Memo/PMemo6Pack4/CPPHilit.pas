unit CPPHilit;

{ © Electro-Concept Mauricie, 2003-2004 }
{ Implements TCPPHighlighter object, used for C++ syntax highlighting
  in a TPlusMemo }

{ Note: to install this component on your palette, add file CPPReg in your package }

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$DEFINE CPPHilit}
{UCONVERT}
  {$IFDEF CPPHilitClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}

interface

{$IFDEF pmClx}
uses SysUtils, Classes, QGraphics, QControls, PlusMemoClx, PMSupportClx, ExtHilitClx, NbHilitClx;
{$ELSE}
uses SysUtils, Classes, Graphics, Controls, PlusMemo, PMSupport, ExtHilit, NbHilit;
{$ENDIF}


const CPPDelimiters: TSysCharSet = ['"', ' ', '''', '(', ')', ',', '.', '/', ':', ';',
                                   '<', '>', '[', ']', '{', '}', #9, '=', '-'];
      CPPKeywordContext    = 1;
      CPPStringContext     = 2;
      CPPCommentContext    = 3;
      CPPDirectiveContext  = 4;
      NumberContext        = 5;

type
     TCPPKeywordList = class(TPersistent); { a class provided for design time editing individual keyword
                                             formats, independantly of global settings }

     TCPPHighlighter = class(TCustomExtHighlighter)
       private
         fCPPKeywords,
         fPreviousCPPKeywords,
         fStringsHilit,
         fDirectives,
         fCommentSingleLine,
         fCommentMultiline,
         fNumbers           : THighlightInfo;

         fKeywordsOvr       : TCPPKeywordList;
         fApplyHChange      : Boolean;

         fNumHilit          : TNumberHighlighter;

         procedure ReadNewFormats(Reader: TReader);
         procedure WriteFormats(Writer: TWriter);

         procedure HighlightChange(Sender: TObject);

         procedure setCPPKeywords(h: THighlightInfo);
         procedure setStringsHilit(h: THighlightInfo);
         procedure setCommentSingleLine(h: THighlightInfo);
         procedure setCommentMultiLine(h: THighlightInfo);
         procedure setDirectives(h: THighlightInfo);
         procedure setNumbers(h: THighlightInfo);

         procedure GetStartConfirmation(Sender: TObject; StartKey, StopKey: TPlusNavigator; KIndex: Integer; var Accept: Boolean);
         procedure GetStopConfirmation(Sender: TObject; StartKey, StopKey: TPlusNavigator; KIndex: Integer; var Accept: Boolean);
         procedure GetNumberConfirmation(Sender: TObject; Scope: SmallInt; var DoHilit: Boolean);

       protected
         procedure Loaded; override;
         procedure DefineProperties(Filer: TFiler); override;
         procedure ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);  override;
         function  FindStart(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean; override;

       public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

       published
         property CPPKeywords      : THighlightInfo  read fCPPKeywords       write setCPPKeywords;
         property Strings          : THighlightInfo  read fStringsHilit      write setStringsHilit;
         property CommentSingleLine: THighlightInfo  read fCommentSingleLine write setCommentSingleLine;
         property CommentMultiLine : THighlightInfo  read fCommentMultiline  write setCommentMultiLine;
         property Directives       : THighlightInfo  read fDirectives        write setDirectives;
         property Numbers          : THighlightInfo  read fNumbers           write setNumbers;
         property KeywordsOvr      : TCPPKeywordList read fKeywordsOvr       write fKeywordsOvr;
         property SubHighlighter;  { promoted from TCustomExtHighlighter to allow nested highlighting, ex:
                                     url highlighter within comment sections }
       end;

  TConfirmFunc = function (Msg: AnsiString): Boolean;

var
  ConfirmFunc: TConfirmFunc = nil;  // for design time confirmation of global keyword modifications

implementation

const
  CPPKeywordCount = 97;
  CPPKeywordsArray: array[0..CPPKeywordCount-1] of string =
       ('__rrti', '__automated', 'auto', 'bool',      { note: asm keyword must not be there... }
        'break', 'case', 'catch', '__cdecl',
        '_cdecl', 'cdecl', 'char', 'class',
        '__classid', '__closure', 'const', 'const_cast',
        'continue', '__declspec', 'default', 'delete',
        '__dispid', 'do', 'double', 'dynamic_cast',
        'else', 'enum', '__except', 'explicit',
        '__export', '_export', 'export', '__int8',
        '__int16', '__int32', '__int64', 'unsigned __int64',
        'extern', 'false', '__fastcall', '_fastcall',
        '__finally', 'float', 'for', 'friend',
        'goto', 'if', '__import', '_import',
        '__inline', 'inline', 'int', 'long',
        '__msfastcall', '__msreturn', 'mutable', 'namespace',
        'new', 'operator', '__pascal', '_pascal',
        'private', '__property', 'protected', 'public',
        '__published', 'register', 'reinterpret_cast', 'return',
        'short', 'signed', 'sizeof', 'static_cast',
        '__stdcall', '_stdcall', 'struct', 'switch',
        'template', 'this', '__trhead', 'throw',
        'true', '__try', 'try', 'typedef',
        'typeid', 'typename', 'union', 'unsigned',
        'using', 'virtual', 'void', 'volatile',
        'wchar_t', 'while', 'asm', '_asm',
        '__asm');


function IsSpaceOrTab(t: PChar; Len: Integer): Boolean;
  { Returns True if t contains only spaces and tabs over a given length;
    used by start key confirmation handler and FindStart range extender }
begin
  Result:= True;
  while Len>0 do
    if (t^<>' ') and (t^<>#9) then
      begin
        Result:= False;
        Break
      end
    else
      begin
        Dec(Len);
        Inc(t)
      end
end;

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

constructor TCPPHighlighter.Create(AOwner: TComponent);
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
  Delimiters:= CPPDelimiters;
  fKeywordsOvr:= TCPPKeywordList.Create;

  { Install keywords:
    Keywords contains first the CPP keywords, followed by Assembler keywords }
  Keywords.BeginUpdate;
  for i:= 0 to CPPKeyWordCount-1 do
    Keywords.AddExtKeyword(CPPKeyWordsArray[i], [woWholeWordsOnly, woMatchCase], [fsBold],  CPPKeywordContext, 0, 0,
                                     crDefault, -1, -1);
  Keywords.EndUpdate;

  { Install start-stop keys }
  StartStopKeys.AddExStartStopKey('"', '"', [], [fsItalic], CPPStringContext, 0, 0, crDefault,
                                        clSilver, clBlack, [ssoParStop]);

  StartStopKeys.AddExStartStopKey('//', '', [], [fsItalic], CPPCommentContext, 0, 0, crDefault,
                                        -1, clBlue, [ssoParStop]);

  StartStopKeys.AddExStartStopKey('#', '', [], [fsItalic], CPPDirectiveContext, 0, 0, crDefault,
                                        clTeal, clWhite, [ssoParStop]);

  StartStopKeys.AddExStartStopKey('/*', '*/', [], [fsItalic], CPPCommentContext, 0, 0, crDefault,
                                        -1, clGreen, []);

  OnStart:= GetStartConfirmation;   // handle directive highlighting only if at start of line
  OnStop:= GetStopConfirmation;     // handle \  string non-termination

  { Create the THighlightInfo we use }
  CreateHighlight(fCPPKeywords, [fsBold], -1, -1);
  CreateHighlight(fPreviousCPPKeywords, [fsBold], -1, -1);
  fPreviousCPPKeywords.OnChange:= nil;

  CreateHighlight(fStringsHilit, [fsItalic], clSilver, clBlack);
  CreateHighlight(fCommentSingleLine, [fsItalic], -1, clBlue);
  CreateHighlight(fDirectives, [fsItalic], clTeal, clWhite);
  CreateHighlight(fCommentMultiline, [fsItalic], -1, clNavy);
  CreateHighlight(fNumbers, [], -1, clRed);

  { Create internal TNumberHighlighter }
  fNumHilit:= TNumberHighlighter.Create(Self);
  with fNumHilit do
    begin
      CTypeHex:= True;
      Delimiters:= CPPDelimiters;
      ForeGround:= clRed;
      ContextNum:= NumberContext;
      OnQueryHilit:= GetNumberConfirmation;  { highlight numbers in asm and base parts as well }
    end;
end;


destructor TCPPHighlighter.Destroy;
begin
  fKeywordsOvr.Free;
  fStringsHilit.Free;
  fCommentMultiline.Free;
  fCommentSingleLine.Free;
  fDirectives.Free;
  fCPPKeywords.Free;
  fPreviousCPPKeywords.Free;
  fNumbers.Free;
  fNumHilit.Free;
  inherited Destroy
end;

procedure TCPPHighlighter.Loaded;
var i: Integer; kinfo: TKeywordInfo; pkey: pKeyInfoLen;
begin
  fPreviousCPPKeywords.Assign(fCPPKeywords);

  { update CPP keywords, only those that were not streamed in
    for those having been read from stream, turn off the flag that says so. }
  kinfo.Options:= [woMatchCase, woWholeWordsOnly];
  kinfo.Style:= fCPPKeywords.Style;
  if fCPPKeywords.AltFont then TPlusFontStyles(kinfo.Style):= TPlusFontStyles(kinfo.Style) + [TPlusFontStyle(fsAltFont)];
  kinfo.ContextNumber:= CPPKeywordContext;
  kinfo.Cursor:= crDefault;
  kinfo.Backgnd:= fCPPKeywords.Background;
  kinfo.Foregnd:= fCPPKeywords.Foreground;

  for i:= 0 to CPPKeywordCount-1 do
    begin
      pkey:= pKeyInfoLen(Keywords.KeyList[i]);
      if Byte(pkey.BasicPart.Style) and $80 = 0 then pkey.BasicPart:= kinfo
                                                else Byte(pkey.BasicPart.Style):= Byte(pkey.BasicPart.Style) and $7f
    end;

  fApplyHChange:= True;
  { update start-stop keys through the HighlightChange handler }
  HighlightChange(fStringsHilit);
  HighlightChange(fCommentSingleLine);
  HighlightChange(fCommentMultiline);
  HighlightChange(fNumbers);
  HighlightChange(fDirectives);

  fApplyHChange:= False;
  inherited Loaded;    { note: this will also call ReApplyKeys }
end;

procedure TCPPHighlighter.setCPPKeywords(h: THighlightInfo);
begin
  fCPPKeywords.Assign(h)
end;

procedure TCPPHighlighter.setStringsHilit(h: THighlightInfo);
begin
  fStringsHilit.Assign(h)
end;

procedure TCPPHighlighter.setCommentSingleLine(h: THighlightInfo);
begin
  fCommentSingleLine.Assign(h)
end;

procedure TCPPHighlighter.setCommentMultiLine(h: THighlightInfo);
begin
  fCommentMultiline.Assign(h)
end;

procedure TCPPHighlighter.setDirectives(h: THighlightInfo);
begin
  fDirectives.Assign(h)
end;

procedure TCPPHighlighter.setNumbers(h: THighlightInfo);
begin
  fNumbers.Assign(h)
end;

procedure TCPPHighlighter.HighlightChange(Sender: TObject);
  { event handler for internal THighlightInfo modifications }

  procedure setkeyword(var kw: TKeyInfoLen; oldHighlight, newHighlight: THighlightInfo);
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

var i, ssupdate: Integer; klist: TExtKeywordList;
begin    { HighlightChange }
  if (csLoading in ComponentState) and (not fApplyHChange) then Exit;

   if Sender=fCPPKeywords then
    begin
      if csDesigning in ComponentState then
        if not ConfirmKeywords(fCPPKeywords, fPreviousCPPKeywords) then Exit;

      { update the CPP keywords highlight attributes }
      klist:= inherited Keywords;
      for i:= 0 to CPPKeywordCount-1 do
          setkeyword(pKeyInfoLen(klist.KeyList[i])^, fPreviousCPPKeywords, fCPPKeywords);
      fPreviousCPPKeywords.Assign(fCPPKeywords)
    end;

  { determine which start-stop key needs to be updated }
  ssupdate:= -1;

  if Sender=fCommentMultiline then ssupdate:= 3;
  if Sender=fCommentSingleLine then ssupdate:= 1;
  if Sender=fDirectives then ssupdate:= 2;
  if Sender=fStringsHilit then ssupdate:= 0;

  if ssupdate>=0 then
      setssinfo(pStartStopInfo(StartStopKeys.Pointers[ssupdate])^, THighlightInfo(Sender));

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

procedure TCPPHighlighter.GetStartConfirmation(Sender: TObject; StartKey, StopKey: TPlusNavigator;
                                               KIndex: Integer; var Accept: Boolean);
begin
  if (KIndex=2 {DirectiveIndex}) and (not IsSpaceOrTab(StartKey.Par.ParText, StartKey.ParOffset)) then
     Accept:= False
end;

procedure TCPPHighlighter.GetStopConfirmation(Sender: TObject; StartKey, StopKey: TPlusNavigator;
                                              KIndex: Integer; var Accept: Boolean);
begin
  Accept:= (KIndex<>0 {String section index}) or (StopKey.ParOffset<GetParLength(StopKey.Par^)) or
           (StopKey.ParOffset=0) or (StopKey.Par.ParText[StopKey.ParOffset-1]<>'\')
end;

procedure TCPPHighlighter.GetNumberConfirmation(Sender: TObject; Scope: SmallInt; var DoHilit: Boolean);
begin
  DoHilit:= Scope=0
end;

procedure TCPPHighlighter.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ModFormats', ReadNewFormats, WriteFormats, True)
end;

procedure TCPPHighlighter.ReadNewFormats(Reader: TReader);
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

procedure TCPPHighlighter.WriteFormats(Writer: TWriter);
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
        if not samehinfo(BasicPart, CPPKeywords) then
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

procedure TCPPHighlighter.ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);
begin
  inherited ApplyKeywordsList(Start, Stop, BaseIndex);
  TCPPHighlighter(fNumHilit).ApplyKeywordsList(Start, Stop, BaseIndex)
end;



function TCPPHighlighter.FindStart(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean;
var i: Integer; d: TDynInfoArray; tscan: PChar;
begin
  d:= nil;     // to avoid a warning
  { Extend range to include directive character (#) if we're at the base context }
  if (Start.Context=0) and (Stop.ParNumber=Start.ParNumber) and IsSpaceOrTab(Start.fPar.ParText, Stop.fOffset) then
    begin
      tscan:= pmStrScan(Start.fPar.ParText+Stop.fOffset, '#');
      if tscan<>nil then Stop.ParOffset:= tscan-Start.fPar.ParText
    end;

  Result:= inherited FindStart(Start, Stop, BaseIndex);
  if Result then
    begin
      { Adjust directive context position to zero, as FindStart will make it at the position of the '#' }
      d:= Start.Par.ParExtra.DynCodes;
      for i:= 0 to High(d) do
        if d[i].Context=CPPDirectiveContext then
          begin
            d[i].StartKlen:= d[i].DynOffset+1;
            d[i].DynOffset:= 0;
            Break
          end
    end
end;

end.
