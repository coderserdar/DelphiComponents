unit HtmlHighlight;
{ © Electro-Concept Mauricie, 1998-2004

  Installation: Add file HtmlHilitReg.pas to a package of your choice

  A component to dynamically highlight html language elements in a TPlusMemo.

  Place a THtmlHighlighter on your form (it is located on the PlusMemo page as default), set your
  TPlusMemo.Highlighter property to point to it, et voilà, html tags are turned colored,
  underlined, bolded, as you want.  Various parts of html tags are highlighted independantly,
  for example the bracket pair, the attribute values and strings can have their own highlighting,
  different from the html keyword.  Html comments and script sections are also highlighted
  differently.

  Properties DefaultText and ScriptTag are there only for the purpose of compatibility with earlier version.

  Note: this component is based on TCustomExtHighlighter, a class defined in file ExtHilit.pas.

  You can control the highlighting attributes for different parts of html tags from within the Object
  Inspector. This effect is best appreciated if some html text is loaded in the TPlusMemo (via its
  Lines property), and watching how the highlighting changes when changing various properties in the
  Object Inspector }


{$A+}  { this unit requires word alignment of data }
{$B-}  { not complete boolean evaluation }

{$H+}  { long strings }
{$J+}  { writeable typed constants }

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$DEFINE HtmlHighlight}
{UCONVERT}
  {$IFDEF HtmlHighlightClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}

interface

{$IFDEF pmClx}
uses Classes, QGraphics, QControls, PlusMemoClx, PMSupportClx, ExtHilitClx;
{$ELSE}
uses Classes, Graphics, Controls, PlusMemo, PMSupport, ExtHilit;
{$ENDIF}

const      // Context values for the different parts of html highlighting
  HTMLTagScope = 1;
  HTMLScriptScope = 2;
  HTMLCommentScope= 3;
  HTMLStartScope  = 4;
  HtmlKeywordScope= 5;
  HtmlUnknownScope= 6;
  HtmlStopScope   = 7;
  HtmlAttributeScope = 8;
  HtmlAttributeValueScope = 9;
  SpecialCharScope = 10;
  PerlScriptScope = 11;
  AspScriptScope = 12;   //JH


type
  THtmlHighlighter = class(TCustomExtHighlighter)
    private
      fBracket,
      fDefaultText,
      fHtmlTag,
      fHtmlComment,
      fHtmlAttribute,
      fHtmlAttributeValue,
      fScriptTag,
      fScriptContent,
      fUnknownTag,
      fHtmlKeyword,
      fSpecialChars,
      fPerlScript,
      fWorkingHilit: THighlightInfo;

      fXMLSyntax: Boolean;

      procedure HighlightChange(Sender: TObject);
      procedure setBracket(const Value: THighlightInfo);
      procedure setHtmlTag(ht: THighlightInfo);
      procedure setHtmlComment(hc: THighlightInfo);
      procedure setHtmlAttribute(ha: THighlightInfo);
      procedure setHtmlAttributeValue(ha: THighlightInfo);
      procedure setScriptTag(st: THighlightInfo);
      procedure setScriptContent(sc: THighlightInfo);
      procedure setSpecialChars(sc: THighlightInfo);
      procedure setUnknownTag(ut: THighlightInfo);
      procedure setHtmlKeyword(const Value: THighlightInfo);
      procedure setPerlScript(ps: THighlightInfo);

    protected
      procedure Loaded; override;
      procedure ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);  override;
      function  FindStart(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean; override;
      function  FindStop(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean; override;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

    published
      property Bracket      : THighlightInfo read fBracket       write setBracket;
      property DefaultText  : THighlightInfo read fDefaultText   write fDefaultText;
                                      { only for compatibility with pre-4.1 version }
      property HtmlKeyword  : THighlightInfo read fHtmlKeyword   write setHtmlKeyword;
      property HtmlTag      : THighlightInfo read fHtmlTag       write setHtmlTag;
      property HtmlComment  : THighlightInfo read fHtmlComment   write setHtmlComment;
      property HtmlAttribute: THighlightInfo read fHtmlAttribute write setHtmlAttribute;
      property HtmlAttributeValue: THighlightInfo read fHtmlAttributeValue write setHtmlAttributeValue;
      property PerlScript   : THighlightInfo read fPerlScript    write setPerlScript;
      property ScriptTag    : THighlightInfo read fScriptTag     write setScriptTag;
                                      { only for compatibility with pre-4.1 version }
      property ScriptContent: THighlightInfo read fScriptContent write setScriptContent;
      property SpecialChars : THighlightInfo read fSpecialChars  write setSpecialChars;
      property UnknownTag   : THighlightInfo read fUnknownTag    write setUnknownTag;
      property SubHighlighter;  { promoted from TCustomExtHighlighter to allow nested highlighting, ex:
                                  url highlighter within comment sections }

      property XMLSyntax: Boolean read fXMLSyntax write fXMLSyntax;
      property Keywords;    // promoted from TCustomExtHighlighter to allow for individual html keywords coloring
    end;

implementation

uses SysUtils;  // StrPos function

const    // Default colors set at create time
  DefaultTextColor = clWindowText;
  DefaultBackColor = -1;

  HTMLTagColor      = clGreen;
  HTMLTagBackground = -1;
  HTMLCommentColor  = clOlive;
  HTMLKeywordColor  = clNavy;
  HTMLAttributeColor = clGreen;
  HTMLAttributeValueColor = clGray;

  ScriptTagBackColor   = clWindow;
  ScriptTagForeColor   = clPurple;
  ScriptTextColor      = clWindowText;
  UnknownTagColor      = clRed;


  PerlBackColor     = -1;
  PerlForeColor     = clTeal;
  ScriptBackColor   = -1;
  ScriptForeColor   = clNavy;
  BracketColor      = clBlue;
  SpecialCharColor  = clFuchsia;

  HtmlKeywordCount  = 106;

type HtmlElementRecord = record Element: string; Attributes, Complement: Boolean end;
     HtmlElementArrays = array[0..HtmlKeywordCount-1] of HtmlElementRecord;

const HtmlList: HtmlElementArrays =

    ((Element: 'SCRIPT';       Attributes: True;  Complement: False),    { this one put first because of special treatment }
     (Element: 'A';            Attributes: True;  Complement: True ),
     (Element: 'ADDRESS';      Attributes: True;  Complement: True ),
     (Element: 'ACRONYM';      Attributes: True;  Complement: True ),
     (Element: 'APPLET';       Attributes: True;  Complement: True ),
     (Element: 'AREA';         Attributes: True;  Complement: True ),
     (Element: 'B';            Attributes: False; Complement: True ),
     (Element: 'BASE';         Attributes: True;  Complement: False),
     (Element: 'BASEFONT';     Attributes: True;  Complement: False),
     (Element: 'BGSOUND';      Attributes: True;  Complement: True ),
     (Element: 'BIG';          Attributes: True;  Complement: True ),
     (Element: 'BLINK';        Attributes: True;  Complement: True ),
     (Element: 'BLOCKQUOTE';   Attributes: True;  Complement: True ),
     (Element: 'BODY';         Attributes: True;  Complement: True ),
     (Element: 'BR';           Attributes: False; Complement: False),
     (Element:  'BUTTON';      Attributes: True;  Complement: True ),
     (Element:  'CAPTION';     Attributes: True;  Complement: True ),
     (Element:  'CITE';        Attributes: True;  Complement: True ),
     (Element:  'CENTER';      Attributes: True;  Complement: True ),
     (Element:  'CODE';        Attributes: True;  Complement: True ),
     (Element:  'COL';         Attributes: True;  Complement: True ),
     (Element:  'COLGROUP';    Attributes: True;  Complement: True ),
     (Element:  'COMMENT';     Attributes: True;  Complement: True ),
     (Element:  'DD';          Attributes: True;  Complement: True ),
     (Element:  'DEL';         Attributes: True;  Complement: True ),
     (Element:  'DFN';         Attributes: True;  Complement: True ),
     (Element:  'DIR';         Attributes: True;  Complement: True ),
     (Element:  'DIV';         Attributes: True;  Complement: True ),
     (Element:  'DL';          Attributes: True;  Complement: True ),
     (Element:  'DT';          Attributes: True;  Complement: True ),
     (Element:  'EM';          Attributes: True;  Complement: True ),
     (Element:  'EMBED';       Attributes: True;  Complement: True ),
     (Element:  'FIELDSET';    Attributes: True;  Complement: True ),
     (Element:  'FORM';        Attributes: True;  Complement: True ),
     (Element:  'FONT';        Attributes: True;  Complement: True ),
     (Element: 'FRAME';        Attributes: True;  Complement: False),
     (Element:  'FRAMESET';    Attributes: True;  Complement: True ),
     (Element:  'H1';          Attributes: True;  Complement: True ),
     (Element:  'H2';          Attributes: True;  Complement: True ),
     (Element:  'H3';          Attributes: True;  Complement: True ),
     (Element:  'H4';          Attributes: True;  Complement: True ),
     (Element:  'H5';          Attributes: True;  Complement: True ),
     (Element:  'H6';          Attributes: True;  Complement: True ),
     (Element:  'HEAD';        Attributes: False; Complement: True ),
     (Element:  'HR';          Attributes: True;  Complement: True ),
     (Element:  'HTML';        Attributes: False; Complement: True ),
     (Element:  'I';           Attributes: False; Complement: True ),
     (Element:  'IFRAME';      Attributes: True;  Complement: True ),
     (Element:  'ILAYER';      Attributes: True;  Complement: True ),
     (Element:  'IMG';         Attributes: True;  Complement: False),
     (Element:  'INPUT';       Attributes: True;  Complement: False),
     (Element:  'INS';         Attributes: True;  Complement: True ),
     (Element:  'ISINDEX';     Attributes: True;  Complement: True ),
     (Element:  'KBD';         Attributes: True;  Complement: True ),
     (Element:  'KEYGEN';      Attributes: True;  Complement: True ),
     (Element:  'LABEL';       Attributes: True;  Complement: True ),
     (Element:  'LAYER';       Attributes: True;  Complement: True ),
     (Element:  'LEGEND';      Attributes: True;  Complement: True ),
     (Element:  'LI';          Attributes: True;  Complement: True ),
     (Element:  'LINK';        Attributes: True;  Complement: True ),
     (Element:  'LISTING';     Attributes: True;  Complement: True ),
     (Element:  'MAP';         Attributes: True;  Complement: True ),
     (Element:  'MARQUEE';     Attributes: True;  Complement: True ),
     (Element:  'MENU';        Attributes: True;  Complement: True ),
     (Element:  'META';        Attributes: True;  Complement: False),
     (Element:  'MULTICOL';    Attributes: True;  Complement: True ),
     (Element:  'NEXTID';      Attributes: True;  Complement: True ),
     (Element:  'NOBR';        Attributes: True;  Complement: True ),
     (Element:  'NOEMBED' ;    Attributes: True;  Complement: True ),
     (Element:  'NOFRAMES';    Attributes: True;  Complement: True ),
     (Element:  'NOLAYER';     Attributes: True;  Complement: True ),
     (Element:  'NOSCRIPT';    Attributes: True;  Complement: True ),
     (Element:  'OBJECT';      Attributes: True;  Complement: True ),
     (Element:  'OL';          Attributes: True;  Complement: True ),
     (Element:  'OPTION';      Attributes: True;  Complement: True ),
     (Element:  'P';           Attributes: True;  Complement: True ),
     (Element:  'PARAM';       Attributes: True;  Complement: True ),
     (Element:  'PLAINTEXT';   Attributes: True;  Complement: True ),
     (Element:  'Q';           Attributes: True;  Complement: True ),
     (Element:  'S';           Attributes: False; Complement: True ),
     (Element:  'SAMP';        Attributes: True;  Complement: True ),
     (Element:  'SELECT';      Attributes: True;  Complement: True ),
     (Element:  'SOUND';       Attributes: True;  Complement: True ),
     (Element:  'SMALL';       Attributes: True;  Complement: True ),
     (Element:  'SPACER';      Attributes: True;  Complement: True ),
     (Element:  'SPAN';        Attributes: True;  Complement: True ),
     (Element:  'STRIKE';      Attributes: True;  Complement: True ),
     (Element:  'STRONG';      Attributes: True;  Complement: True ),
     (Element:  'STYLE';       Attributes: True;  Complement: True ),
     (Element:  'SUB';         Attributes: True;  Complement: True ),
     (Element:  'SUP';         Attributes: True;  Complement: True ),
     (Element:  'TABLE';       Attributes: True;  Complement: True ),
     (Element:  'TBODY';       Attributes: True;  Complement: True ),
     (Element:  'TD';          Attributes: True;  Complement: True ),
     (Element:  'TEXTAREA';    Attributes: True;  Complement: True ),
     (Element:  'TFOOT';       Attributes: True;  Complement: True ),
     (Element:  'TH';          Attributes: True;  Complement: True ),
     (Element:  'THEAD';       Attributes: True;  Complement: True ),
     (Element:  'TITLE';       Attributes: False; Complement: True ),
     (Element:  'TR';          Attributes: True;  Complement: True ),
     (Element:  'TT';          Attributes: True;  Complement: True ),
     (Element:  'U';           Attributes: False; Complement: True ),
     (Element:  'UL';          Attributes: True;  Complement: True ),
     (Element:  'VAR';         Attributes: True;  Complement: True ),
     (Element:  'WBR';         Attributes: True;  Complement: True ),
     (Element:  '!DOCTYPE';    Attributes: True;  Complement: False ));

  HtmlCommentIndex = HtmlKeywordCount;
  HtmlUnknownIndex = HtmlCommentIndex + 1;
  PcScriptIndex    = HtmlUnknownIndex + 1;    { % script index }
  SpecialCharIndex = PcScriptIndex + 1;
  PerlScriptIndex  = SpecialCharIndex + 1;
  HtmlScriptIndex  = 0;
  HtmlIndexCount   = PerlScriptIndex + 1;


function HStrScan(t: PChar; c: Char): PChar;  { better than the one supplied in SysUtils for long PChars }
begin
  while (t^<>#0) and (t^<>c) do Inc(t);
  if t^=#0 then Result:= nil else Result:= t
end;

procedure FillDyn(h: THighlightInfo; nav: TPlusNavigator; Nested: Boolean; Ctx, KIndex: Integer; StartLen, StopLen: Integer);
var dinfo: DynInfoRec; oldpos: Longint; navpdyn: pDynInfoRec;
begin
  navpdyn:= nav.pDynAttr;
  with dinfo do
    begin
      Level:= DynToLevel(navpdyn^);
      if (Level>=0) and Nested then
        with nav do
          begin
            oldpos:= Pos;
            if BackToDyn(0) then
              begin
                Par^.ParExtra.DynCodes[DynNb].StopKLen:= 0;
                Pos:= oldpos;
                RightOfDyn;
                navpdyn:= nav.pDynAttr
              end
          end;

      if Nested or (Level<0) then Inc(Level);
      DynStyle:= Byte(h.Style) or $c0;
      DynOffset:= nav.ParOffset;
      CollpsState:= navpdyn.CollpsState;
      if navpdyn.DynStyle and $80<>0 then CollpsLevel:= navpdyn.CollpsLevel
                                     else CollpsLevel:= 0;
      if h.AltFont then DynStyle:= DynStyle or (1 shl Ord(fsAltFont));
      KeyIndex:= navpdyn.KeyIndex;
      KeyIndex[Level]:= -32768+ KIndex;
      Cursor:= crDefault;
      Backgnd:= h.Background;
      Foregnd:= h.Foreground;
      Context:= Ctx;
      if StartLen<256 then StartKlen:= StartLen
                      else StartKLen:= 255;
      if StopLen<256 then StopKLen:= StopLen
                     else StopKLen:= 255;
      with nav do
        if (fDynNb<GetDynCount(fPar^)) and (fPar^.ParExtra.DynCodes[fDynNb].DynOffset=fOffset) then
                       fPar^.ParExtra.DynCodes[fDynNb]:= dinfo
        else AddDyn(dinfo);
    end
end;

constructor THtmlHighlighter.Create(AOwner: TComponent);
  procedure CreateHighlight(var h: THighlightInfo; style: TFontStyles; altf: Boolean; bk, fr: TColor);
  begin
    h:= THighlightInfo.Create;
    h.AltFont:=  altf;
    h.Style:= style;
    h.Background:= bk;
    h.Foreground:= fr;
    h.OnChange:= HighlightChange
  end;

begin
  inherited Create(AOwner);
  CreateHighlight(fDefaultText,[], False, DefaultBackColor, DefaultTextColor);
  CreateHighlight(fHtmlKeyword,[fsBold], False, DefaultBackColor,HtmlKeywordColor);
  CreateHighlight(fHtmlTag, [fsItalic], False, DefaultBackColor, HTMLTagColor);
  CreateHighlight(fHtmlComment, [fsItalic], False, DefaultBackColor, HTMLCommentColor);
  CreateHighlight(fHtmlAttribute, [], False, DefaultBackColor, HTMLAttributeColor);
  CreateHighlight(fHtmlAttributeValue, [], False, DefaultBackColor, HTMLAttributeValueColor);
  CreateHighlight(fPerlScript, [], False, PerlBackColor, PerlForeColor);
  CreateHighlight(fScriptTag, [fsBold], False, ScriptTagBackColor, ScriptTagForeColor);
  CreateHighlight(fScriptContent, [], True, DefaultBackColor, ScriptTextColor);
  CreateHighlight(fUnknownTag, [], False, DefaultBackColor, UnknownTagColor);
  CreateHighlight(fBracket, [fsBold], False, DefaultBackColor, BracketColor);
  CreateHighlight(fSpecialChars, [], False, DefaultBackColor, SpecialCharColor);
  fWorkingHilit:= THighlightInfo.Create;
end;


destructor THtmlHighlighter.Destroy;
begin
  fHtmlKeyword.Destroy;
  fHtmlTag.Destroy;
  fHtmlComment.Destroy;
  fHtmlAttribute.Destroy;
  fHtmlAttributeValue.Destroy;
  fScriptTag.Destroy;
  fDefaultText.Destroy;
  fPerlScript.Destroy;
  fScriptContent.Destroy;
  fUnknownTag.Destroy;
  fBracket.Destroy;
  fSpecialChars.Destroy;
  fWorkingHilit.Destroy;
  inherited Destroy
end;

procedure THtmlHighlighter.HighlightChange(Sender: TObject);
    { event handler for internal THighlightInfo modifications }
begin
  if (csDesigning in ComponentState) and (not (csLoading in ComponentState)) then ReApplyKeys
end;

procedure THtmlHighlighter.Loaded;
begin
  inherited Loaded;
  ReApplyKeys
end;


procedure THtmlHighlighter.setBracket(const Value: THighlightInfo);
begin
  fBracket.Assign(Value);
end;

procedure THtmlHighlighter.setHtmlKeyword(const Value: THighlightInfo);
begin
  fHtmlKeyword.Assign(Value);
end;

procedure THtmlHighlighter.setHtmlTag(ht: THighlightInfo);
begin
  fHtmlTag.Assign(ht)
end;

procedure THtmlHighlighter.setHtmlComment(hc: THighlightInfo);
begin
  fHtmlComment.Assign(hc)
end;

procedure THtmlHighlighter.setHtmlAttribute(ha: THighlightInfo);
begin
  fHtmlAttribute.Assign(ha)
end;

procedure THtmlHighlighter.setHtmlAttributeValue(ha: THighlightInfo);
begin
  fHtmlAttributeValue.Assign(ha)
end;

procedure THtmlHighlighter.setPerlScript(ps: THighlightInfo);
begin
  fPerlScript.Assign(ps)
end;

procedure THtmlHighlighter.setScriptTag(st: THighlightInfo);
begin
  fScriptTag.Assign(st)
end;

procedure THtmlHighlighter.setScriptContent(sc: THighlightInfo);
begin
  fScriptContent.Assign(sc)
end;

procedure THtmlHighlighter.setSpecialChars(sc: THighlightInfo);
begin
  fSpecialChars.Assign(sc)
end;

procedure THtmlHighlighter.setUnknownTag(ut: THighlightInfo);
begin
  fUnknownTag.Assign(ut)
end;

function THtmlHighlighter.FindStop(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean; { they must be in the same par. }
var searchlen  : Integer;
    backstart, backend: Integer;
    dinfo      : DynInfoRec;
    t, keyfound: PChar;
    scar       : Char;
    curscope,
    curlevel, curkey   : Integer;
    stopendpos : Longint;

const EndComment: string = '-->'#0;
      EndScript: string  = '%>'#0;
      EndPerlScript: string = '?>'#0;
      EndHtmlScript: string = '</SCRIPT>'#0;

begin
  Result:= False;

  Start.RightOfDyn;
  curlevel:= DynToLevel(Start.pDynAttr^);
  with Start.pDynAttr^ do
    if DynStyle and $80 = 0 then
      begin
        curscope:= 0;
        curkey:= High(curkey)
      end
    else
      begin
        curscope:= Context;
        if curlevel>=0 then curkey:= KeyIndex[curlevel]
                       else curkey:= High(curkey);
        if curkey<0 then curkey:= curkey + 32768
                    else curkey:= High(curkey)
      end;

  if (curkey-BaseIndex>=HtmlIndexCount) and (SubHighlighter<>nil) then
    begin
      Result:= inherited FindStop(Start, Stop, HtmlIndexCount+BaseIndex);
      Exit
    end;

  case curscope of
    HtmlTagScope, HtmlUnknownScope:
      begin
        t:= Start.fPar^.ParText + Start.fOffset;
        if t=nil then Exit;
        searchlen:= Stop.ParOffset - Start.fOffset;
        scar:= t[searchlen];
        t[searchlen]:= #0;
        keyfound:= HStrScan(t, '>');
        if keyfound<>nil then
          begin
            Result:= True;
            Stop.Assign(Start);
            Stop.Pos:= Start.Pos + (keyfound - t);
            FillDyn(Bracket, Stop, False, HtmlStopScope, BaseIndex, 1, 2);
            Stop.Pos:= Stop.Pos + 1;
            if (curkey=HtmlScriptIndex+BaseIndex) then
              FillDyn(ScriptContent, Stop, True, HTMLScriptScope, BaseIndex + HtmlScriptIndex, 0, 10)
            else
              begin
                dinfo.DynStyle:= 0;
                Stop.AddDyn(dinfo)
              end
          end;
        t[searchlen]:= scar
      end;

    HtmlCommentScope:
      begin
        if Start.fOffset>3 then backstart:= 3
                           else backstart:= Start.fOffset;
        t:= Start.fPar^.ParText+ Start.fOffset-backstart;
        if t<>nil then
          begin
            if GetParLength(Stop.Par^) > Stop.ParOffset+3 then backend:= 3
                                                          else backend:= GetParLength(Stop.fPar^) - Stop.fOffset;
            searchlen:= Stop.fOffset+backend - (Start.fOffset-backstart);
            scar:= t[searchlen];
            t[searchlen]:= #0;
            keyfound:= StrPos(t, PChar(EndComment));
            if keyfound<>nil then
              begin
                Result:= True;
                stopendpos:= Start.Pos - backstart + (keyfound-t) + 3;
                Stop.Assign(Start);
                while Stop.ForwardToDyn(stopendpos) do Stop.RemoveDyn;
                Stop.Pos:= stopendpos;
                dinfo.DynStyle:= 0;
                Stop.AddDyn(dinfo)
              end;
            t[searchlen]:= scar
          end
      end;

    HTMLScriptScope, PerlScriptScope, AspScriptScope:
      begin
        if curkey-BaseIndex = HtmlScriptIndex then
          begin
            if Start.fOffset>Length(EndHtmlScript) then backstart:= Length(EndHtmlScript)
                                                   else backstart:= Start.fOffset;
            if GetParLength(Stop.Par^) > Stop.ParOffset+Length(EndHtmlScript) then backend:= Length(EndHtmlScript)
                                                                       else backend:= GetParLength(Stop.fPar^) - Stop.fOffset;
            searchlen:= Stop.fOffset+backend - (Start.fOffset-backstart);
            if searchlen>0 then
              begin
                t:= TPlusMemo(Start.fPMemo).GetUpText(Start.fPar, Start.fParNb, Start.fOffset-backstart, searchlen);
                scar:= t[searchlen];
                t[searchlen]:= #0;
                keyfound:= StrPos(t, PChar(EndHtmlScript));
                if keyfound<>nil then
                  begin
                    Result:= True;
                    Stop.Pos:= Start.Pos - backstart + (keyfound-t);
                    Stop.RightOfDyn;
                    FillDyn(Bracket, Stop, True, HtmlStopScope, BaseIndex, Length(EndHtmlScript)-1, Length(EndHtmlScript));
                    Stop.Pos:= Stop.Pos+1;
                    FillDyn(HtmlKeyword, Stop, True, HtmlStopScope, BaseIndex, Length(EndHtmlScript)-3, Length(EndHtmlScript)-3);
                    Stop.Pos:= Stop.Pos+Length(EndHtmlScript)-3;
                    FillDyn(Bracket, Stop, True, HtmlStopScope, BaseIndex, 1, 1);
                    Stop.Pos:= Stop.Pos + 1;
                    dinfo.DynStyle:= 0;
                    Stop.AddDyn(dinfo)
                  end;
                t[searchlen]:= scar
              end
          end

        else
          begin
            if Start.fOffset>2 then backstart:= 2
                               else backstart:= Start.fOffset;
            t:= Start.fPar^.ParText+ Start.fOffset-backstart;
            if GetParLength(Stop.Par^) > Stop.ParOffset+2 then backend:= 2
                                                   else backend:= GetParLength(Stop.fPar^) - Stop.fOffset;
            searchlen:= Stop.fOffset+backend - (Start.fOffset-backstart);
            if searchlen>0 then
              begin
                scar:= t[searchlen];
                t[searchlen]:= #0;
                if curkey-BaseIndex = PerlScriptIndex then keyfound:= StrPos(t, PChar(EndPerlScript))
                                                      else keyfound:= StrPos(t, PChar(EndScript));
                if keyfound<>nil then
                  begin
                    Result:= True;
                    Stop.Pos:= Start.Pos - backstart + (keyfound-t) + 1;
                    FillDyn(Bracket, Stop, False, HtmlStopScope, BaseIndex, 1, 2);
                    Stop.Pos:= Stop.Pos+1;
                    dinfo.DynStyle:= 0;
                    Stop.AddDyn(dinfo)
                  end;
                t[searchlen]:= scar
              end
          end
      end;

    SpecialCharScope:
      begin
        t:= Start.fPar^.ParText + Start.fOffset;
        searchlen:= Stop.ParOffset - Start.fOffset;
        keyfound:= t;
        if (Start.fOffset=0) or (Start.fPar^.ParText[Start.fOffset-1]<>';') then
          while (searchlen>0) and (pmChar(keyfound^) in ['a'..'z', 'A'..'Z', '0'..'9', '&', '_', '#']) do
            begin
              Inc(keyfound);
              Dec(searchlen)
            end;
        if (searchlen>0) or (Stop.fOffset=GetParLength(Start.fPar^)) then
          begin
            Result:= True;
            Stop.Assign(Start);
            Stop.Pos:= Start.Pos + (keyfound - t);
            if (keyfound^=';') then Stop.Pos:= Stop.Pos + 1;
            dinfo.DynStyle:= 0;
            Stop.AddDyn(dinfo)
          end
      end
    end;

end;     { FindStop }

function SpecStrScan(t: PChar): PChar;
  { special scan for either an opening bracket (<) or ampersand
    used for searching a start key in base context }
begin
  while (t^<>#0) and (t^<>'<') and (t^<>'&') do Inc(t);
  if t^=#0 then Result:= nil else Result:= t
end;

function THtmlHighlighter.FindStart(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean;

var slen    : Integer;
    t       : PChar;
    tcomp1, tcomp2: PChar;
    i       : Integer;
    curscope : Integer;
    scar     : Char;
    startword: Integer;
    endword  : Integer;
    wordlen  : Integer;
    searchlen: Integer;
    compelem : Boolean;
    kindex   : Integer;
    ahilit   : THighlightInfo;
    actx     : Integer;
    dinfo    : DynInfoRec;
    sklist   : TList;
    sscope   : Integer;
    skey     : pKeyInfoLen;
    saddkindex: Integer;

label SubH;

begin
  Start.RightOfDyn;
  Result:= False;
  if DynToLevel(Start.pDynAttr^)>=15 then Exit;

  with Start.pDynAttr^ do
    if DynStyle and $80 = 0 then curscope:= 0
                            else curscope:= Context;


  case curscope of
    0:  if Stop.ParOffset>Start.fOffset then
          begin
            searchlen:= Stop.fOffset - Start.fOffset;
            t:= Start.fPar^.ParText + Start.fOffset;
            scar:= t[searchlen];
            t[searchlen]:= #0;
            tcomp1:= SpecStrScan(t);
            if tcomp1<>nil then
              begin
                Result:= True;
                Stop.Pos:= Start.Pos + (tcomp1-t);
                Stop.RightOfDyn;
                t[searchlen]:= scar;
                if tcomp1^='<' then
                  if (tcomp1[1]='!') and (tcomp1[2]='-') and (tcomp1[3]='-') then
                    begin
                      FillDyn(HtmlComment, Stop, True, HtmlCommentScope, BaseIndex + HtmlCommentIndex, 3, 4);
                      Stop.Pos:= Stop.Pos + 4
                    end
                  else
                    begin
                      FillDyn(Bracket, Stop, True, HtmlStartScope, BaseIndex, 1, 1);
                      Stop.Pos:= Stop.Pos+1
                    end
                else      { Highlighting &nbsp or other like this }
                  begin
                    FillDyn(SpecialChars, Stop, True, SpecialCharScope, BaseIndex + SpecialCharIndex, 1, 1);
                    Stop.Pos:= Stop.Pos+1
                  end
              end { found start of tag }
            else t[searchlen]:= scar
          end;

    HtmlStartScope: { Start is normally positioned right after the opening bracket }
      if Start.fPar^.ParText<>nil then
        begin
          t:= Start.fPar^.ParText;
          startword:= Start.fOffset;

          if t[startword]='%' then
            begin
              Result:= True;
              FillDyn(ScriptContent, Start, False, AspScriptScope, BaseIndex + PcScriptIndex, 1, 2);
              Stop.Assign(Start);
              Stop.Pos:= Stop.Pos + 1;
              goto SubH
            end;

          if t[startword]='?' then
            begin
              Result:= True;
              FillDyn(PerlScript, Start, False, PerlScriptScope, BaseIndex + PerlScriptIndex, 1, 2);
              Stop.Assign(Start);
              Stop.Pos:= Stop.Pos + 1;
              goto SubH
            end;

          { find the end of word }
          endword:= startword;
          while not (pmChar(t[endword]) in [' ', #0, '>', #9]) do Inc(endword);
          wordlen:= endword-startword;
          t:= TPlusMemo(Start.fPMemo).GetUpText(Start.fPar, Start.fParNb, startword, wordlen);
          saddkindex:= -1;  // flag as not set
          kindex:= -1;
          if not fXMLSyntax then
            begin
              if t^='/' then
                begin
                  compelem:= True;
                  Dec(wordlen);
                  Inc(t)
                end
              else compelem:= False;

              for i:= 0 to HtmlKeywordCount-1 do
                begin
                  slen:= Length(HtmlList[i].Element);
                  if slen=wordlen then
                    begin
                      tcomp1:= PChar(HtmlList[i].Element);
                      tcomp2:= t;
                      while (slen>0) and (tcomp1^=tcomp2^) do
                        begin
                          Dec(slen);
                          Inc(tcomp1);
                          Inc(tcomp2)
                        end;

                      if (slen=0) and ((not compelem) or HtmlList[i].Complement) then
                        begin
                          kindex:= i;
                          Result:= True;
                          Break
                        end
                    end
                end;

              if not Result then
                begin
                  sklist:= Keywords.KeyList;
                  for i:= 0 to Keywords.Count-1 do
                    begin
                      tcomp1:= PChar(pKeyInfoLen(sklist[i]).KeywordTrans);
                      if tcomp1=nil then Continue;
                      tcomp2:= t;
                      slen:= wordlen;
                      while (slen>0) and (tcomp1^=tcomp2^) do
                        begin
                          Dec(slen);
                          Inc(tcomp1);
                          Inc(tcomp2)
                        end;
                      if (slen=0) and (tcomp1^=#0) then
                        begin
                          saddkindex:= i;
                          Result:= True;
                          Break
                        end
                    end
                end
            end

          else   // XMLSyntax
            begin
              Result:= True;
              compelem:= False;
              kindex:= 1
            end;

          if compelem then Inc(wordlen);
          Stop.Assign(Start);
          while Stop.ForwardToDyn(Start.Pos + wordlen) do Stop.RemoveDyn;

          if Result then
            begin
              ahilit:= HtmlKeyword;
              sscope:= HtmlKeywordScope;
              // check for being defined in Keywords
              sklist:= Keywords.KeyList;
              if (saddkindex<0) and (sklist<>nil) then
                  for i:= 0 to sklist.Count-1 do
                    begin
                      tcomp1:= PChar(pKeyInfoLen(sklist[i]).KeywordTrans);
                      if tcomp1=nil then Continue;
                      tcomp2:= t;
                      slen:= wordlen;
                      while (slen>0) and (tcomp1^=tcomp2^) do
                        begin
                          Dec(slen);
                          Inc(tcomp1);
                          Inc(tcomp2)
                        end;
                      if (slen=0) and (tcomp1^=#0) then
                        begin
                          saddkindex:= i;
                          Break
                        end
                    end;

              if saddkindex>=0 then
                begin
                  ahilit:= fWorkingHilit;
                  skey:= sklist[saddkindex];
                  ahilit.AltFont:= TFontStyle(fsAltFont) in skey.BasicPart.Style;
                  ahilit.Style:= skey.BasicPart.Style;
                  ahilit.Background:= skey.BasicPart.Backgnd;
                  ahilit.Foreground:= skey.BasicPart.Foregnd;
                  sscope:= skey.BasicPart.ContextNumber;
                  if kindex<0 then kindex:= 1  // apply Complement and Attributes to additional keywords
                end;

              FillDyn(ahilit, Start, False, sscope, BaseIndex + kindex, wordlen, wordlen);
              if compelem or (not (HtmlList[kindex].Attributes)) then
                begin
                  ahilit:= UnknownTag;
                  actx  := HtmlUnknownScope
                end
              else
                begin
                  ahilit:= HtmlTag;
                  actx  := HtmlTagScope
                end;

              Stop.Assign(Start);
              Stop.Pos:= Start.Pos + wordlen;
              FillDyn(ahilit, Stop, False, actx, BaseIndex+kindex, 0, 0)
            end

          else
            begin
              Result:= True;
              FillDyn(UnknownTag, Start, False, HtmlUnknownScope, BaseIndex+HtmlUnknownIndex, wordlen, 0);
              Stop.Assign(Start);
              Stop.Pos:= Start.Pos + wordlen;
            end
        end;   { HtmlStartContext }

    HtmlTagScope:
      if Start.fPar^.ParText<>nil then
        begin
          t:= Start.fPar^.ParText + Start.fOffset;
          searchlen:= Stop.fOffset-Start.fOffset;
          scar:= t[searchlen];
          t[searchlen]:= #0;
          tcomp1:= HStrScan(t, '=');
          t[searchlen]:= scar;
          if tcomp1<>nil then
            begin
              Result:= True;
              dinfo:= Start.DynAttr;
              startword:= Start.fOffset + (tcomp1-t);
              t:= Start.fPar^.ParText;
              endword:= startword+1;
              while (startword>0) and (t[startword-1]=' ') do Dec(startword);
              while (startword>0) and (t[startword-1]<>' ') do Dec(startword);
              Stop.Pos:= Start.fPar^.StartOffset + startword;
              FillDyn(HtmlAttribute, Stop, False, HtmlAttributeScope, BaseIndex, endword-startword, endword-startword);
              Stop.Pos:= Start.fPar^.StartOffset + endword;
              wordlen:= startword;
              startword:= endword;
              while t[endword]=' ' do Inc(endword);
              if t[endword]='"' then
                begin
                  Inc(endword);
                  while not (pmChar(t[endword]) in ['"', #0]) do Inc(endword);
                  if t[endword]='"' then Inc(endword)
                end
              else
                if t[endword]='''' then
                  begin
                    Inc(endword);
                    while not (pmChar(t[endword]) in ['''', #0]) do Inc(endword);
                    if t[endword] = '''' then Inc(endword)
                  end
                else
                  while not (pmChar(t[endword]) in [' ', '>', #0]) do Inc(endword);
              while Stop.ForwardToDyn(Start.fPar^.StartOffset+endword) do
                                                             Stop.RemoveDyn;
              if Start.fOffset>=wordlen then
                begin
                  Start.fDynNb:= -1;
                  Start.RightOfDyn
                end;
              Stop.Pos:= Start.fPar^.StartOffset + startword;
              FillDyn(HtmlAttributeValue, Stop, False, HtmlAttributeValueScope, BaseIndex, endword-startword, endword-startword);
              Stop.Pos:= Start.fPar^.StartOffset+endword;
              Stop.AddDyn(dinfo)
            end;
      end
    end;
SubH:
  if SubHighlighter<>nil then Result:= Result or (inherited FindStart(Start, Stop, BaseIndex+HtmlIndexCount))
end;    { FindStart }


procedure THtmlHighlighter.ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);
begin  // nothing to do except take care of SubHighlighter, and must not use inherited otherwise keywords will be highlighted
  if SubHighlighter<>nil then
      THtmlHighlighter(SubHighlighter).ApplyKeywordsList(Start, Stop, BaseIndex + HTMLKeywordCount + Keywords.Count);
end;

end.


