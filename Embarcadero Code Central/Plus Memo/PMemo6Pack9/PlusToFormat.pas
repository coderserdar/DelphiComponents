unit PlusToFormat;
{ Copyright (c) Electro-Concept Mauricie, 1999-2003 }

{ Implements TPlusToRTF and TPlusToHtml components, used to get the content of a TPlusMemo
  in RTF or HTML format.

  See PlusToFormat.txt for information on how to use them. }

  { Issue: in Clx version, html and rtf clipboard formats are not those used by convention by other applications }
  
{$IFDEF VER140} {$DEFINE D6New} {$ENDIF}    // Use GroupDescendantsWidth under D6 and up
{$IFDEF VER150} {$DEFINE D6New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D6New} {$ENDIF}

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$DEFINE PlusToFormat}
{UCONVERT}
  {$IFDEF PlusToFormatClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}

interface

{$IFDEF pmClx}
uses Classes, PlusMemoClx, PMSupportClx;
{$ELSE}
uses Classes, PlusMemo, PMSupport;
{$ENDIF}

type
  TPlusToFormattedText = class(TComponent)
    private
      fDynFormat   : Boolean;
      fPlusMemo    : TPlusMemo;
      fOnProgress  : TNotifyEvent;
      function getSelection: string;
      function getText: string;
    protected
      function ClipboardFormat: {$IFDEF pmClx} AnsiString {$ELSE} Integer {$ENDIF}; virtual;
      procedure FormatForClipboard(var CText: string); virtual; abstract;
      procedure getFormattedText(Nav1, Nav2: TPlusNavigator; Stream: TStream; sList: TStringList); virtual; abstract;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(AOwner: TComponent); override;
      procedure CopyToClipboard; virtual;
      procedure SaveToStream(Stream: TStream);
      procedure SaveToFile(FName: string);
      property Text: string read getText;
      property Selection: string read getSelection;
    published
      property DynFormat: Boolean read fDynFormat write fDynFormat;
      property PlusMemo: TPlusMemo read fPlusMemo write fPlusMemo;
      property OnProgress: TNotifyEvent read fOnProgress write fOnProgress;
    end;

  TPlusToRTF = class(TPlusToFormattedText)
    private
      fRTFLeading  : Single;
    protected
      function ClipboardFormat: {$IFDEF pmClx} AnsiString {$ELSE} Integer {$ENDIF}; override;
      procedure FormatForClipboard(var CText: string); override;
      procedure getFormattedText(Nav1, Nav2: TPlusNavigator; Stream: TStream; sList: TStringList); override;
    public
      constructor Create(AOwner: TComponent); override;
    published
      property RTFLeading: Single read fRTFLeading write fRTFLeading;
    end;

  TPlusToHTML = class(TPlusToFormattedText)
    private
      fBodyTag: Boolean;
      fHeadTag: Boolean;
      fUrlsAsATags: Boolean;
      fTitle: string;
      fBaseFont: Boolean;
      fDocType: string;
      fForceFont: Boolean;
    protected
      function ClipboardFormat: {$IFDEF pmClx} AnsiString {$ELSE} Integer {$ENDIF}; override;
      procedure FormatForClipboard(var CText: string); override;
      procedure getFormattedText(Nav1, Nav2: TPlusNavigator; Stream: TStream; sList: TStringList); override;
    public
      constructor Create(AOwner: TComponent); override;
    published
      property BaseFont: Boolean read fBaseFont write fBaseFont;
      property BodyTag: Boolean read fBodyTag write fBodyTag;
      property ForceFont: Boolean read fForceFont write fForceFont;
      property HeadTag: Boolean read fHeadTag write fHeadTag;
      property DocType: string read fDocType write fDocType;
      property Title: string read fTitle write fTitle;
      property UrlsAsATags: Boolean read fUrlsAsATags write fUrlsAsATags default True;
    end;

var UrlContextLow: Integer = 1024;
    UrlContextHigh: Integer = 1024+14;

procedure Register;

implementation

{$IFDEF pmClx}
uses SysUtils, QGraphics, QClipbrd, QControls;
{$ELSE}
uses SysUtils, Windows, Graphics, Clipbrd, Controls;
{$ENDIF}

{$IFNDEF pmClx}
var RTFClipboardFormat, HtmlClipboardFormat: Integer;   { set to zero at init time }
{$ENDIF}

procedure Register;
begin
  RegisterComponents({UCONVERT}'PlusMemo'{/UCONVERT}, [TPlusToRTF, TPlusToHtml]);
end;

{ TPlusToFormattedText }

function TPlusToFormattedText.ClipboardFormat: {$IFDEF pmClx} AnsiString {$ELSE} Integer {$ENDIF};
begin
  {$IFDEF pmClx}
  Result:= 'text/ansi'
  {$ELSE}
  Result:= CF_TEXT
  {$ENDIF}
end;

procedure TPlusToFormattedText.CopyToClipboard;
var
  copytext: string;
  {$IFDEF pmClx}
  cstream: TStream;
  cformat: AnsiString;
  {$ELSE}
  Data: THandle;
  DataPtr: Pointer;
  cformat: Integer;
  {$ENDIF}
begin
  if (PlusMemo=nil) or (PlusMemo.SelLength=0) then Exit;
  cformat:= ClipboardFormat;
  copytext:= Selection;
  FormatForClipboard(copytext);
  
  {$IFDEF pmClx}
  cstream:= TMemoryStream.Create;
  cstream.Write(copytext[1], Length(copytext));
  Clipboard.SetFormat(cformat, cstream);
  cstream.Free

  {$ELSE}
  Data := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, Length(copytext)+1);
  try
    DataPtr := GlobalLock(Data);
    try
      Move(copytext[1], DataPtr^, Length(copytext)+1);
      Clipboard.SetAsHandle(cformat, Data);
    finally
      GlobalUnlock(Data);
    end;
  except
    GlobalFree(Data);
    raise;
  end
  {$ENDIF}
end;

constructor TPlusToFormattedText.Create(AOwner: TComponent);
begin
  inherited;
  fDynFormat:= True
end;

function TPlusToFormattedText.getSelection: string;
var tmpstrings: TStringList;
begin
  if (PlusMemo=nil) or (PlusMemo.SelLength=0) then Result:= ''
  else
    begin
      tmpstrings:= TStringList.Create;
      getFormattedText(PlusMemo.SelStartNav, PlusMemo.SelStopNav, nil, tmpstrings);
      Result:= tmpstrings.Text;
      tmpstrings.Free
    end
end;

function TPlusToFormattedText.getText: string;
var tmpstrings: TStringList; start, stop: TPlusNavigator;
begin
  if (PlusMemo=nil) or (PlusMemo.CharCount=0) then Result:= ''
  else
    begin
      tmpstrings:= TStringList.Create;
      start:= TPlusNavigator.Create(PlusMemo);
      stop:= TPlusNavigator.Create(PlusMemo);
      stop.Pos:= PlusMemo.CharCount;
      getFormattedText(start, stop, nil, tmpstrings);
      Result:= tmpstrings.Text;
      tmpstrings.Free;
      start.Free;
      stop.Free
    end
end;

procedure TPlusToFormattedText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation=opRemove) and (AComponent=PlusMemo) then
      fPlusMemo:= nil;
  inherited;
end;

procedure TPlusToFormattedText.SaveToFile(FName: string);
var f: TFileStream;
begin
  if PlusMemo=nil then Exit;
  f:= TFileStream.Create(FName, fmCreate);
  try
    SaveToStream(f);
  finally
    f.Free
  end
end;

procedure TPlusToFormattedText.SaveToStream(Stream: TStream);
var start, stop: TPlusNavigator;
begin
  if PlusMemo=nil then Exit;
  start:= TPlusNavigator.Create(PlusMemo);
  stop:= TPlusNavigator.Create(PlusMemo);
  stop.Pos:= PlusMemo.CharCount;
  GetFormattedText(start, stop, Stream, nil)
end;


{ TPlusToRTF }

function TPlusToRTF.ClipboardFormat: {$IFDEF pmClx} AnsiString {$ELSE} Integer {$ENDIF};
begin
  {$IFDEF pmClx}
  Result:= 'mime/rtf'
  {$ELSE}
  if RTFClipboardFormat=0 then
      RTFClipboardFormat:= RegisterClipboardFormat('Rich Text Format');
  Result:= RTFClipboardFormat
  {$ENDIF}
end;

constructor TPlusToRTF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRTFLeading:= -1;
end;

procedure TPlusToRTF.FormatForClipboard(var CText: string);
begin
end;

procedure TPlusToRTF.getFormattedText(Nav1, Nav2: TPlusNavigator; Stream: TStream; sList: TStringList);
const SpecialChars = ['{', '}', '\'];
      crlf: AnsiString = #13#10;

var smemo: TPlusMemo;
    colortable: TList;

  procedure Output(const s: AnsiString);
    begin
      if Stream<>nil then
        begin
          if s<>'' then Stream.WriteBuffer(s[1], Length(s)*SizeOf(s[1]));
          Stream.WriteBuffer(crlf[1], 2*SizeOf(crlf[1]))
        end;
      if sList<>nil then sList.Add(s)
    end;

  function RTFColor(c: TColor): string;
    var crgb: Longint;
    begin
      crgb:= ColorToRGB(c);
      Result:= '\red' + IntToStr(crgb and $0FF) +
               '\green' + IntToStr((crgb and $0FF00) shr 8) +
               '\blue' + IntToStr((crgb and $FF0000) shr 16) +
               ';'
    end;


  procedure buildcolortable;  { add to color table the values needed for dyn parts }
    var snav: TPlusNavigator;
    begin
      snav:= TPlusNavigator.Create(nil);
      snav.fPMemo:= Nav1.fPMemo;
      snav.Assign(Nav1);
        repeat
          snav.RightOfDyn;
          with snav.DynAttr do
              if (DynStyle and $80 <>0) and ((Backgnd<>0) or (Foregnd<>0)) then
                  begin
                    if colortable.IndexOf(Pointer(Backgnd))<0 then colortable.Add(Pointer(Backgnd));
                    if colortable.IndexOf(Pointer(Foregnd))<0 then colortable.Add(Pointer(Foregnd))
                  end
        until not snav.ForwardToDyn(Nav2.Pos);
      snav.Free
    end;


  function RTFFormat(Style: TExtFontStyles; const dyn: DynInfoRec): string;
    var sb: Byte; dyncolor: Boolean;
    begin
      Result:= '\plain';
      dyncolor:= False;

      if (dyn.DynStyle and $80<>0) and DynFormat then
        begin
          sb:= dyn.DynStyle and $3f;
          Style:= TExtFontStyles(sb);
          dyncolor:= (dyn.Foregnd<>0) or (dyn.Backgnd<>0)
        end;
      if TPlusFontStyle(fsAltFont) in TPlusFontStyles(Style) then
          Result:= Result + '\f1\fs' + IntToStr(Abs(smemo.AltFont.Size)*2)
      else
          Result:= Result + '\f0\fs' + IntToStr(Abs(smemo.Font.Size)*2);
      { N.B: The font size in RTF for TrueType is the double as normal font }

      if fsBold in Style then Result:= Result + '\b';
      if fsItalic in Style then Result:= Result + '\i';
      if fsUnderline in Style then Result:= Result + '\ul';
      if not dyncolor then
          if TPlusFontStyle(fsHighlight) in TPlusFontStyles(Style) then Result:= Result + '\cf1\cb3'
                                                                   else Result:= Result + '\cf0\cb2'
      else
          Result:= Result + '\cf' + IntToStr(colortable.IndexOf(Pointer(dyn.Foregnd))) +
                            '\cb'+ IntToStr(colortable.IndexOf(Pointer(dyn.Backgnd)));

      Result:= Result + ' '
    end;

var i, j, jlim,
    lastwritten,
    lastchar, tlen: Integer;
    s, olds, st, s1: string;
    parp          : pParInfo;
    t             : PChar;
    CurrentStyle  : TExtFontStyles;
    currentdyn    : DynInfoRec;
    currdynnb     : Integer;
    formatmodified: Boolean;
    fs            : TFontStyles;
    dcount        : Integer;
    lastnotify    : Cardinal;

begin
  smemo:= TPlusMemo(nav1.fPMemo);
  if DynFormat then smemo.DoDynParse(Nav1.ParNumber, Nav2.ParNumber);
  lastnotify:= GetTickCount;
  S:= '{\rtf1\ansi\deff0\deftab720'+          { head of an rtf an open { }
      '{\fonttbl{\f0\fnil '+smemo.Font.Name+';}' +  { First font 0 Index }
      '{\f1\fnil '+smemo.AltFont.Name+';}}';        { uses another font }
  Output(s);

  colortable:= TList.Create;
  { Write the color table: 0 is for normal text, 1 for highlighted text,
                           2 normal background,  3 for highlighted backg. }
  colortable.Add(Pointer(smemo.Font.Color));
  colortable.Add(Pointer(smemo.HighlightColor));
  colortable.Add(Pointer(smemo.Color));
  colortable.Add(Pointer(smemo.HighlightBackgnd));
  Output('{\colortbl');
  if DynFormat then buildcolortable;

  s:= '';
  for i:= 0 to colortable.Count-1 do s:= s + RTFColor(TColor(colortable[i]));
  s:= s+ '}';

  { Alignment of the text, we make general because TPlusMemo
    does not support paragraph format }
  if smemo.Justified then s:= s + '\pard\qj'
  else case smemo.Alignment of
        taLeftJustify : s:= s+'\pard\ql';
        taRightJustify: s:= s+'\pard\qr'
       else s:= s+'\pard\qc'   { center }
       end;

  { leading, written in twips }
  if RTFLeading>0 then st:= s + '\sl'+IntToStr(Round(1440*RTFLeading))
  else
    if RTFLeading<0 then st:= s + '\sl'+IntToStr(Round(240*-RTFLeading)) + '\slmult1'
    else st:= s;

  { write initial formatting info }
  fs:= smemo.Font.Style;
  CurrentStyle:= Nav1.Style;
  currentdyn:= Nav1.DynAttr;
  s:= RTFFormat(CurrentStyle, currentdyn);
  Output(st+s);
  st:= '';

  { go with the text content }
  for i:= Nav1.ParNumber to Nav2.ParNumber do
    begin
      parp:= smemo.IParList.ParPointers[i];
      t:= parp^.ParText;
      dcount:= GetDynCount(parp^);

      if i=Nav1.fParNb then
        begin
          currdynnb:= Nav1.DynNb;
          j:= Nav1.ParOffset;
          st:= ''
        end
      else
        begin
          j:= 0;
          currdynnb:= 0;
          st:= '\par '
        end;

      if DynFormat and (dcount>currdynnb) then jlim:= parp^.ParExtra.DynCodes[currdynnb].DynOffset
                                          else jlim:= High(jlim);
      if i=Nav2.fParNb then tlen:= Nav2.ParOffset
      else
        begin
          tlen:= GetParLength(parp^);
          if (tlen>=1) and (t^=#12) then
            begin
              Inc(j);
              st:= '\page '
            end
        end;

      lastwritten:= j; lastchar:= j;

      while j<tlen do
        begin
          formatmodified:= False;
          if j<jlim then
            if pmChar(t[j]) in SpecialChars then
              begin
                SetLength(s1, j-lastwritten);
                if j>lastwritten then Move(t[lastwritten], s1[1], (j-lastwritten)*SizeOf(s1[1]));
                st:= st + s1 + '\' + t[j];
                Inc(j);
                lastwritten:= j;
                lastchar:= j
              end

            else
              if smemo.StaticFormat and (t[j]<#26) and (AnsiChar(t[j]) in CtrlCodesSet) then
                begin
                  formatmodified:= True;
                  repeat
                    XORStyleCode(CurrentStyle, t[j]);
                    Inc(j)
                  until not (pmChar(t[j]) in CtrlCodesSet)
                end
              else
                begin
                  Inc(j);
                  lastchar:= j
                end;

          if j>=jlim then
            begin
              Inc(currdynnb);
              while (currdynnb<dcount) and (parp^.ParExtra.DynCodes[currdynnb].DynOffset<=j) do Inc(currdynnb);
              currentdyn:= parp^.ParExtra.DynCodes[currdynnb-1];
              if currdynnb<dcount then jlim:= parp^.ParExtra.DynCodes[currdynnb].DynOffset
                                  else jlim:= High(jlim);
              formatmodified:= True
            end;

          if formatmodified then
            begin
              SetLength(s1, lastchar-lastwritten);
              if lastchar>lastwritten then Move(t[lastwritten], s1[1], (lastchar-lastwritten)*SizeOf(s1[1]));
              st:= st + s1;
              olds:= s;
              s:= RTFFormat(CurrentStyle, currentdyn);
              if s<>olds then st:= st + s;
              lastwritten:= j
            end
        end;  { while j<tlen }

      SetLength(s1, j-lastwritten);
      if j>lastwritten then Move(t[lastwritten], s1[1], (j-lastwritten)*SizeOf(s1[1]));
      st:= st + s1;
      Output(st);
      if Assigned(fOnProgress) then
        if GetTickCount-lastnotify>smemo.ProgressInterval then
          begin
            fOnProgress(Self);
            lastnotify:= GetTickCount
          end
    end;  { i loop over paragraphs }

  { write RTF end part }
  Output('\par }'#0);

  if Assigned(fOnProgress) then fOnProgress(Self);
  colortable.Free
end;


{ TPlusToHTML }

const BRTag = '<BR>';
      NbspTag = '&nbsp;';


type SpecCharRec = record c: AnsiChar; Tag: AnsiString end;

const SpecCharCount = 101;
      SpecChars: array[0..SpecCharCount-1] of SpecCharRec =
        ((c: '>'; Tag: '&gt;'), (c: '<'; Tag: '&lt;'), (c: '&'; Tag: '&amp;'), (c: '"'; Tag: '&quot;'),
         (c: #9;  Tag: '&#9;'), (c: '¡'; Tag: '&iexcl;'), (c: '¢'; Tag: '&cent;'), (c: '£'; Tag: '&pound;'),
         (c: '¤'; Tag: '&curren;'), (c: '¥'; Tag: '&yen;'), (c: '¦'; Tag: '&brvbar;'), (c: '§'; Tag: '&sect;'),
         (c: '¨'; Tag: '&uml;'), (c: '©'; Tag: '&copy;'), (c: 'ª'; Tag: '&ordf;'), (c: '«'; Tag: '&laquo;'),
         (c: '¬'; Tag: '&not;'), (c: '®'; Tag: '&reg;'), (c: '¯'; Tag: '&macr;'), (c: '°'; Tag: '&deg;'),
         (c: '±'; Tag: '&plusmn;'), (c: '²'; Tag: '&sup2;'), (c: '³'; Tag: '&sup3;'), (c: '´'; Tag: '&acute;'),
         (c: 'µ'; Tag: '&micro;'), (c: '¶'; Tag: '&para;'), (c: '·'; Tag: '&middot;'), (c: '¸'; Tag: '&cedil;'),
         (c: '¹'; Tag: '&sup1;'), (c: 'º'; Tag: '&ordm;'), (c: '»'; Tag: '&raquo;'), (c: '¼'; Tag: '&frac14;'),
         (c: '½'; Tag: '&frac12;'), (c: '¾'; Tag: '&frac34;'), (c: '¿'; Tag: '&iquest;'), (c: 'À'; Tag: '&Agrave;'),
         (c: 'Á'; Tag: '&Aacute;'), (c: 'Â'; Tag: '&Acirc;'), (c: 'Ã'; Tag: '&Atilde;'), (c: 'Ä'; Tag: '&Auml;'),
         (c: 'Å'; Tag: '&Aring;'), (c: 'Æ'; Tag: '&AElig;'), (c: 'Ç'; Tag: '&Ccedil;'), (c: 'È'; Tag: '&Egrave;'),
         (c: 'É'; Tag: '&Eacute;'), (c: 'Ê'; Tag: '&Ecirc;'), (c: 'Ë'; Tag: '&Euml;'), (c: 'Ì'; Tag: '&Igrave;'),
         (c: 'Í'; Tag: '&Iacute;'), (c: 'Î'; Tag: '&Icirc;'), (c: 'Ï'; Tag: '&Iuml;'), (c: 'Ð'; Tag: '&ETH;'),
         (c: 'Ñ'; Tag: '&Ntilde;'), (c: 'Ò'; Tag: '&Ograve;'), (c: 'Ó'; Tag: '&Oacute;'), (c: 'Ô'; Tag: '&Ocirc;'),
         (c: 'Õ'; Tag: '&Otilde;'), (c: 'Ö'; Tag: '&Ouml;'), (c: '×'; Tag: '&times;'), (c: 'Ø'; Tag: '&Oslash;'),
         (c: 'Ù'; Tag: '&Ugrave;'), (c: 'Ú'; Tag: '&Uacute;'), (c: 'Û'; Tag: '&Ucirc;'), (c: 'Ü'; Tag: '&Uuml;'),
         (c: 'Ý'; Tag: '&Yacute;'), (c: 'Þ'; Tag: '&THORN;'), (c: 'ß'; Tag: '&szlig;'), (c: 'à'; Tag: '&agrave;'),
         (c: 'á'; Tag: '&aacute;'), (c: 'â'; Tag: '&acirc;'), (c: 'ã'; Tag: '&atilde;'), (c: 'ä'; Tag: '&auml;'),
         (c: 'å'; Tag: '&aring;'), (c: 'æ'; Tag: '&aelig;'), (c: 'ç'; Tag: '&ccedil;'), (c: 'è'; Tag: '&egrave;'),
         (c: 'é'; Tag: '&eacute;'), (c: 'ê'; Tag: '&ecirc;'), (c: 'ë'; Tag: '&euml;'), (c: 'ì'; Tag: '&igrave;'),
         (c: 'í'; Tag: '&iacute;'), (c: 'î'; Tag: '&icirc;'), (c: 'ï'; Tag: '&iuml;'), (c: 'ð'; Tag: '&eth;'),
         (c: 'ñ'; Tag: '&ntilde;'), (c: 'ò'; Tag: '&ograve;'), (c: 'ó'; Tag: '&oacute;'), (c: 'ô'; Tag: '&ocirc;'),
         (c: 'õ'; Tag: '&otilde;'), (c: 'ö'; Tag: '&ouml;'), (c: '÷'; Tag: '&divide'), (c: 'ø'; Tag: '&oslash;'),
         (c: 'ù'; Tag: '&ugrave;'), (c: 'ú'; Tag: '&uacute;'), (c: 'û'; Tag: '&ucirc;'), (c: 'ü'; Tag: '&uuml;'),
         (c: 'ý'; Tag: '&yacute;'), (c: 'þ'; Tag: '&thorn;'), (c: 'ÿ'; Tag: '&yuml;'), (c: '™'; Tag: '&trade;'),
         (c: '€'; Tag: '&euro;'));


var SpecialChars: TSysCharSet;   { initialized at startup from elements found SpecChars }

function HtmlFontSizeToString(Size: Integer): string;
begin
  if Size>0 then Result:= 'font-size: ' + IntToStr(Size) + 'pt'
            else Result:= 'font-size: ' + IntToStr(-Size) + 'px'
end;

function HtmlColorToString(color: TColor): string;
begin
  if color=clNone then Result:= ''
  else
    begin
      Result:= IntToHex(ColorToRGB(color), 6);
      Result:= '#' + Copy(Result, 5,2) + Copy(Result, 3, 2) + Copy(Result, 1, 2)
    end
end;


function TagFromChar(car: Char): string;
var i: Integer;
begin
  Result:= '';
  for i:= 0 to SpecCharCount-1 do
    if Char(SpecChars[i].c)=car then
      begin
        Result:= SpecChars[i].Tag;
        Exit
      end
end;

function GetEndTag(element: TPlusFontStyle): string;
begin
  case element of
    Byte(fsBold): Result:= '</B>';
    Byte(fsItalic): Result:= '</I>';
    Byte(fsUnderline): Result:= '</U>';
    Byte(fsStrikeOut): Result:= '</S>';
    fsHighlight, fsAltFont: Result:= '</FONT>'
    else Result:= ''
  end
end;

function GetStartTag(element: TPlusFontStyle; fontcolor, fontbackgnd: TColor; fontsize: Integer; fontname: string): string;
var s: string;
begin
  case element of
    Byte(fsBold): Result:= '<B>';
    Byte(fsItalic): Result:= '<I>';
    Byte(fsUnderline): Result:= '<U>';
    Byte(fsStrikeOut): Result:= '<S>';
    Byte(fsAltFont), Byte(fsHighlight):
      begin
        Result:= '<FONT';
        if fontname<>'' then Result:= Result + ' FACE="' + fontname+'"';
        if (fontcolor<>clNone) or (fontbackgnd<>clNone) or (fontsize<>0) then
          begin
            if fontcolor<>clNone then s:= 'color: ' + HtmlColorToString(fontcolor);
            if fontbackgnd<>clNone then
              begin
                if s<>'' then s:= s + '; ';
                s:= s + 'background-color: ' + HtmlColorToString(fontbackgnd)
              end;
            if fontsize<>0 then
              begin
                if s<>'' then s:= s + '; ';
                s:= s + HtmlFontSizeToString(fontsize)
              end;
            Result:= Result + ' STYLE="' + s + '"'
          end;
        Result:= Result + '>'
      end
    else Result:= ''
  end
end;

function ExpandUrl(const url: string; Context: Integer): string;
begin   // expand urls based on context: see file UrlHighlightClx.pas for definition of individual constants below
  case Context-UrlContextLow of
    1: Result:= 'http://' + url;
    3: Result:= 'mailto:' + url;
    7: Result:= 'ftp://' + url;
    9: Result:= 'gopher://' + url
    else Result:= url
  end
end;

function TPlusToHTML.ClipboardFormat: {$IFDEF pmClx} AnsiString {$ELSE} Integer {$ENDIF};
begin
  {$IFDEF pmClx}
  Result:= 'html'
  {$ELSE}
  if HtmlClipboardFormat=0 then
    HtmlClipboardFormat:= RegisterClipboardFormat('HTML Format');
  Result:= HtmlClipboardFormat
  {$ENDIF}
end;

constructor TPlusToHTML.Create(AOwner: TComponent);
begin
  inherited;
  DocType:= '-//W3C//DTD HTML 3.2 Final//EN';
  HeadTag:= True;
  BodyTag:= True;
  BaseFont:= True;
  Title:= 'PlusToHtml view';
  fUrlsAsATags:= True
end;

procedure TPlusToHTML.FormatForClipboard(var CText: string);
const
  Version = 'Version:1.0'#13#10;
  StartHTML = 'StartHTML:';
  EndHTML = 'EndHTML:';
  StartFragment = 'StartFragment:';
  EndFragment = 'EndFragment:';
  DocType = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">';
  HTMLIntro = '<html><head><META http-equiv=Content-Type content="text/html; charset=utf-8">' +
              '</head><body><!--StartFragment-->';
  HTMLExtro = '<!--EndFragment--></body></html>';
  NumberLengthAndCR = 10;

  // Let the compiler determine the description length.
  DescriptionLength = Length(Version) + Length(StartHTML) + Length(EndHTML) + Length(StartFragment) +
                      Length(EndFragment) + 4 * NumberLengthAndCR;

var
  Description: string;
  StartHTMLIndex,
  EndHTMLIndex,
  StartFragmentIndex,
  EndFragmentIndex: Integer;

begin
    // The HTML clipboard format is defined by using byte positions in the entire block where HTML text and
    // fragments start and end. These positions are written in a description. Unfortunately the positions depend on the
    // length of the description but the description may change with varying positions.
    // To solve this dilemma the offsets are converted into fixed length strings which makes it possible to know
    // the description length in advance.
  StartHTMLIndex := DescriptionLength;              // position 0 after the description
  StartFragmentIndex := StartHTMLIndex + Length(DocType) + Length(HTMLIntro);
  EndFragmentIndex := StartFragmentIndex + Length(CText);
  EndHTMLIndex := EndFragmentIndex + Length(HTMLExtro);

  Description := Version + SysUtils.Format('%s%.8d', [StartHTML, StartHTMLIndex]) + #13#10 +
                 SysUtils.Format('%s%.8d', [EndHTML, EndHTMLIndex]) + #13#10 +
                 SysUtils.Format('%s%.8d', [StartFragment, StartFragmentIndex]) + #13#10 +
                 SysUtils.Format('%s%.8d', [EndFragment, EndFragmentIndex]) + #13#10;
  CText := Description + DocType + HTMLIntro + CText + HTMLExtro;
end;

procedure TPlusToHTML.getFormattedText(Nav1, Nav2: TPlusNavigator; Stream: TStream; sList: TStringList);
var stylestack : array[TPlusFontStyle] of TPlusFontStyles;
    stylelevel : Integer;
    accumulator: PChar;
    acclen     : Integer;
    smemo      : TPlusMemo;
    parp       : pParInfo;
    t          : PChar;

  procedure AccumulateBuf(const Buf; Len: Integer);
    var newacc: PChar;
    begin
      if accumulator=nil then
        begin
          accumulator:= StrAlloc(Len);
          acclen:= 0
        end;

      if Integer(StrBufSize(accumulator))<acclen+Len then
        begin
          newacc:= StrAlloc(acclen+Len);
          Move(accumulator^, newacc^, acclen*SizeOf(accumulator^));
          StrDispose(accumulator);
          accumulator:= newacc
        end;
      Move(Buf, accumulator[acclen], Len*SizeOf(accumulator^));
      Inc(acclen, Len)
    end;

  procedure AccumulateString(const s: string);
    begin
      AccumulateBuf(s[1], Length(s))
    end;

  procedure StreamOut(s: string);
    const crlf: string = #13#10;
    var oldlen: Integer;
    begin
      if Stream<>nil then
        begin
          if accumulator<>nil then Stream.WriteBuffer(accumulator^, acclen*SizeOf(accumulator^));
          if s<>'' then Stream.WriteBuffer(s[1], Length(s)*SizeOf(s[1]));
          Stream.WriteBuffer(crlf[1], 2*SizeOf(crlf[1]))
        end;
      if sList<>nil then
        begin
          if (accumulator<>nil) and (acclen>0) then
            begin
              oldlen:= Length(s);
              SetLength(s, Length(s)+acclen);
              Move(s[1], s[acclen+1], oldlen*SizeOf(s[1]));
              Move(accumulator^, s[1], acclen*SizeOf(accumulator^))
            end;
          sList.Add(s)
        end;
      acclen:= 0
    end;

  procedure HTMLFormat(Style: TFontStyles; const dyn: DynInfoRec; var currentcolor, currentbackcolor: TColor);
    var sb: Byte;
        dyncolor, dynbackcolor: Boolean;
        laststyle, turnoff, turnon: TPlusFontStyles;
        i: TPlusFontStyle;
        slevel, newsize: Integer;
        newfontname: string;
        newcolor, newbackcolor: TColor;
        sbuildstyle: TPlusFontStyles;
    begin
      dyncolor:= False;
      dynbackcolor:= False;
      sbuildstyle:= TPlusFontStyles(Style);
      if (dyn.DynStyle and $80<>0) and DynFormat then
        begin
          sb:= dyn.DynStyle and $3f;
          sbuildstyle:= TPlusFontStyles(sb);
          dyncolor:= (dyn.Foregnd<>-1) and (dyn.Foregnd<>clNone);
          dynbackcolor:= (dyn.Backgnd<>-1) and (dyn.Backgnd<>clNone)
        end;

      if dyncolor then
        begin
          newcolor:= dyn.Foregnd;
          Include(sbuildstyle, fsHighlight)
        end
      else
        if TFontStyle(fsHighlight) in Style then newcolor:= smemo.HighlightColor
                                            else newcolor:= clNone;

      if dynbackcolor then
        begin
          newbackcolor:= dyn.Backgnd;
          Include(sbuildstyle, fsHighlight)
        end
      else
        if TFontStyle(fsHighlight) in Style then newbackcolor:= smemo.HighlightBackgnd
                                            else newbackcolor:= clNone;

      { find elements we have to turn off }
      if stylelevel>=0 then laststyle:= stylestack[stylelevel]
                       else laststyle:= [];
      turnoff:= laststyle-TPlusFontStyles(Style);
      if ((currentcolor<>newcolor) or (currentbackcolor<>newbackcolor)) and
         (fsHighlight in laststyle) then
           Include(turnoff, fsHighlight);

      { find stack level to unwind }
      slevel:= stylelevel;
      while laststyle*turnoff<>[] do
        begin
          Dec(slevel);
          if slevel>=0 then laststyle:= stylestack[slevel]
                       else laststyle:= []
        end;

      { output end tags }
      if slevel<stylelevel then turnoff:= turnoff + (stylestack[stylelevel]-laststyle);
      for i:= High(TPlusFontStyle) downto Low(TPlusFontStyle) do
        if i in turnoff then
            AccumulateString(GetEndTag(i));

      { output attribute tags, put them on the stack }
      if fsAltFont in sbuildstyle then
        begin
          newfontname:= smemo.AltFont.Name;
          newsize:= smemo.AltFont.Size;
        end
      else
        begin
          newfontname:= '';
          newsize:= 0
        end;

      turnon:= sbuildstyle-laststyle;
      for i:= Low(TPlusFontStyle) to High(TPlusFontStyle) do
        if i in turnon then
          begin
            Include(laststyle, i);
            Inc(slevel);
            stylestack[slevel]:= laststyle;
            AccumulateString(GetStartTag(i, newcolor, newbackcolor, newsize, newfontname))
          end;

      stylelevel:= slevel;
      currentcolor:= newcolor
    end;

  function UrlDynToATag(var dynnb: Integer; var dyn: DynInfoRec; var FinalIndex: Integer): Boolean;
    var stopoff: Integer; url: string; ctx: Integer;
    begin
      Result:= UrlsAsATags and (dyn.Context>=UrlContextLow) and (dyn.Context<=UrlContextHigh);
      if Result then
        begin
          Inc(dynnb);
          ctx:= dyn.Context;
          dyn:= parp^.ParExtra.DynCodes[dynnb-1];
          stopoff:= dyn.DynOffset;
          SetLength(url, stopoff-FinalIndex);
          if stopoff>FinalIndex then Move(parp^.ParText[FinalIndex], url[1], (stopoff-FinalIndex)*SizeOf(url[1]));
          url:= ExpandUrl(url, ctx);
          AccumulateString('<A href="' + url);
          AccumulateString('">');
          AccumulateBuf(parp^.ParText[FinalIndex], stopoff-FinalIndex);
          AccumulateString('</A>');
          FinalIndex:= stopoff
        end
    end;

var i              : Longint;
    j, jlim, tindx,
    lastwritten,
    lastchar, tlen: Integer;
    lastnotify    : Cardinal;
    CurrentStyle  : TFontStyles;
    currentdyn    : DynInfoRec;
    currdynnb     : Integer;
    runningcolor  : TColor;
    runningbackground: TColor;
    formatmodified: Boolean;
    fs            : TFontStyles;
    full          : Boolean;
    s             : string;
    dynnblim      : Integer;

const AlignmentToStr: array[TAlignment] of string = ('LEFT', 'RIGHT', 'CENTER');

begin
  accumulator:= nil;
  smemo:= TPlusMemo(Nav1.fPMemo);
  if DynFormat then smemo.DoDynParse(Nav1.ParNumber, Nav2.ParNumber);
  lastnotify:= GetTickCount;
  stylelevel:= -1;
  full:= (Nav1.Pos=0) and (Nav2.Pos=smemo.CharCount);

  if full then
    begin
      if DocType<>'' then
        StreamOut('<!DOCTYPE HTML PUBLIC "' + DocType + '">');

      if HeadTag then
        begin
          StreamOut('<HTML>');
          StreamOut('<HEAD>')
        end;

      if Title<>'' then
        StreamOut('<TITLE>' + Title + '</TITLe>');

      if HeadTag then StreamOut('</HEAD>')
    end;

  if BodyTag then
      StreamOut('<BODY BGCOLOR=' + HtmlColorToString(smemo.Color) +
                            ' TEXT=' + HtmlColorToString(smemo.Font.Color) + '>');

  if BaseFont then
    begin
      s:= GetStartTag(fsHighlight, smemo.Font.Color, clNone, smemo.Font.Size, smemo.Font.Name);
      s:= Copy(s, 6, Length(s)-5);
      StreamOut('<BASEFONT '+s)
    end;

  StreamOut('<DIV ALIGN='+AlignmentToStr[smemo.Alignment]+'>');

  if not BaseFont and ForceFont then
      StreamOut(GetStartTag(fsHighlight, smemo.Font.Color, clNone, smemo.Font.Size, smemo.Font.Name));

  fs:= Nav1.Style;
  CurrentStyle:= TExtFontStyles(fs);
  if DynFormat then currentdyn:= Nav1.DynAttr else currentdyn.DynStyle:= 0;
  runningcolor:= clNone;
  runningbackground:= clNone;
  HTMLFormat(CurrentStyle, currentdyn, runningcolor, runningbackground);

  { go with the text content }
  for i:= Nav1.ParNumber to Nav2.ParNumber do
    begin
      parp:= smemo.IParList.Pointers[i];
      t:= parp^.ParText;

      if i=Nav1.fParNb then
        begin
          currdynnb:= Nav1.DynNb;
          j:= Nav1.fOffset
        end
      else
        begin
          currdynnb:= 0;
          j:= 0
        end;

      if i=Nav2.fParNb then
        begin
          tlen:= Nav2.fOffset;
          dynnblim:= Nav2.DynNb
        end
      else
        begin
          tlen:= GetParLength(parp^);
          dynnblim:= GetDynCount(parp^)
        end;

      if DynFormat and (currdynnb<dynnblim) then jlim:= parp^.ParExtra.DynCodes[currdynnb].DynOffset
                                            else jlim:= High(jlim);

      if jlim<=j then
        begin
          Inc(currdynnb);
          while (currdynnb<dynnblim) and (parp^.ParExtra.DynCodes[currdynnb].DynOffset=0) do Inc(currdynnb);
          currentdyn:= parp^.ParExtra.DynCodes[currdynnb-1];
          if currdynnb<dynnblim then jlim:= parp^.ParExtra.DynCodes[currdynnb].DynOffset
                                else jlim:= High(jlim);
          if UrlDynToATag(currdynnb, currentdyn, j) then
              if currdynnb<dynnblim then jlim:= parp^.ParExtra.DynCodes[currdynnb].DynOffset
                                    else jlim:= High(jlim)

          else
              HtmlFormat(CurrentStyle, currentdyn, runningcolor, runningbackground);
        end;

      lastwritten:= j;
      lastchar:= j;

      while j<tlen do
        begin
          formatmodified:= False;
          if (t[j]<#26) and (AnsiChar(t[j]) in CtrlCodesSet) and smemo.StaticFormat then
            begin
              formatmodified:= True;
              repeat
                XORStyleCode(CurrentStyle, t[j]);
                Inc(j)
              until not (pmChar(t[j]) in CtrlCodesSet)
            end
          else
           begin
             if ((t[j]<#64) or (t[j]>#160)) and
                (((t[j]=' ') and (t[j+1]=' ')) or (pmChar(t[j]) in SpecialChars)) then
               begin    { output a non breaking space }
                 AccumulateBuf(t[lastwritten], lastchar-lastwritten);
                 if t[j]=' ' then AccumulateString(NbspTag)
                 else
                   if t[j]=#9 then for tindx:= 1 to PlusMemo.TabStops do AccumulateString(NbspTag)
                              else AccumulateString(TagFromChar(t[j]));
                 lastwritten:= j+1
               end;
             Inc(j);
             lastchar:= j
           end;

          if j>=jlim then
            begin
              Inc(currdynnb);
              while (currdynnb<dynnblim) and (parp^.ParExtra.DynCodes[currdynnb].DynOffset<=j) do Inc(currdynnb);
              currentdyn:= parp^.ParExtra.DynCodes[currdynnb-1];
              if currdynnb<dynnblim then jlim:= parp^.ParExtra.DynCodes[currdynnb].DynOffset
                                    else jlim:= High(jlim);
              formatmodified:= True
            end;

          if formatmodified then
            begin
              AccumulateBuf(t[lastwritten], lastchar-lastwritten);
              if UrlDynToATag(currdynnb, currentdyn, j) then
                  if currdynnb<dynnblim then jlim:= parp^.ParExtra.DynCodes[currdynnb].DynOffset
                                        else jlim:= High(jlim)
              else
                  HtmlFormat(CurrentStyle, currentdyn, runningcolor, runningbackground);
              lastwritten:= j;
              lastchar:= j
            end;
        end;  { while j<tlen }

      AccumulateBuf(t[lastwritten], j-lastwritten);
      StreamOut(BRTag);
      if Assigned(fOnProgress) then
        if GetTickCount-lastnotify>smemo.ProgressInterval then
          begin
            fOnProgress(Self);
            lastnotify:= GetTickCount
          end
    end;  { i loop over paragraphs }

  currentdyn.DynStyle:= 0;
  runningcolor:= clNone;
  runningbackground:= clNone;
  HtmlFormat([], currentdyn, runningcolor, runningbackground);
  if not BaseFont and ForceFont then StreamOut('</FONT>');
  
  StreamOut('');
  StreamOut('</DIV>');
  if full then
    begin
      if BodyTag then StreamOut('</BODY>');
      if HeadTag then StreamOut('</HTML>')
    end;

  if accumulator<>nil then StrDispose(accumulator)
end;

var i: Integer;

initialization
  {$IFNDEF pmClx}
  RTFClipboardFormat:= 0;  { mark them as not registered }
  HtmlClipboardFormat:= 0;
  {$ENDIF}
  
  SpecialChars:= [];
  for i:= 0 to SpecCharCount-1 do
    Include(SpecialChars, SpecChars[i].c);

  {$IFDEF D6New}
  GroupDescendentsWith(TPlusToFormattedText, TControl);
  {$ENDIF}
end.

