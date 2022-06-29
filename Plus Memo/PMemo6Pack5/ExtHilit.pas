unit ExtHilit;

{ © Electro-Concept Mauricie, 1999-2004 }
{ Implements TExtHighlighter object, used for complex scope dependant and prioritized syntax highlighting
  in a TPlusMemo }

{ Note: to install this component on your palette, add file ExtHilitReg to your package }

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$DEFINE ExtHilit}

{UCONVERT}
  {$IFDEF ExtHilitClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}

interface

{$IFDEF pmClx}
uses SysUtils, Classes, QGraphics, QControls, PlusMemoClx, PMSupportClx;
{$ELSE}
uses SysUtils, Classes, Graphics, Controls, PlusMemo, PMSupport;
{$ENDIF}

{$B-}  { not complete boolean evaluation }
{$H+}  { long strings }
{$J+}  { writeable typed constants }

type
  TExtKeywordList = class(TKeywordList)
    private
      function getPriority(i: Integer): Integer;
      function getScope(i: Integer): Integer;
      procedure setPriority(i: Integer; const Value: Integer);
      procedure setScope(i: Integer; const Value: Integer);
    protected
    public
      function AddExtKeyWord(const KeyWord: string; Options: TWordOptions;
                             Style        : TFontStyles;
                             ContextNumber,
                             Scope,
                             Priority      : SmallInt;
                             Cursor        : TCursor;
                             Backgnd,
                             Foregnd       : TColor): Integer;

      procedure LoadFromIniStrings(IniStrings: TStrings); override;
      property Scope[i: Integer]: Integer read getScope write setScope;
      property Priority[i: Integer]: Integer read getPriority write setPriority;
    end;


  { This class defined to expose Scope and Priority properties, and LoadFrom/SaveTo Ini strings }
  TExtStartStopList = class(TStartStopKeyList)
    private
      function getPriority(i: Integer): Integer;
      function getScope(i: Integer): Integer;
      procedure setPriority(i: Integer; const Value: Integer);
      procedure setScope(i: Integer; const Value: Integer);
    protected
    public
      function AddExStartStopKey(const StartKey, StopKey: string;
                                 Options         : TWordOptions;
                                 Style           : TFontStyles;
                                 ContextNumber,
                                 Scope, Priority : SmallInt;
                                 Cursor          : TCursor;
                                 Backgnd, Foregnd: TColor;
                                 ssOptions       : TssOptions): Integer;

      procedure LoadFromIniStrings(IniStrings: TStrings); override;
      property Scope[i: Integer]: Integer read getScope write setScope;
      property Priority[i: Integer]: Integer read getPriority write setPriority;
    end;

  THighlightInfo = class(TPersistent)
    private
      fAltFont    : Boolean;
      fStyle      : TFontStyles;
      fBackground,
      fForeground : TColor;
      fCursor     : TCursor;
      procedure setAltFont   (af: Boolean);
      procedure setStyle     (s: TFontStyles);
      procedure setBackground(bg: TColor);
      procedure setForeground(fg: TColor);
    protected
      fOnChange: TNotifyEvent;
    public
      procedure Assign(Source: TPersistent); override;
      property OnChange  : TNotifyEvent read fOnChange write fOnChange;
    published
      property AltFont   : Boolean     read fAltFont    write setAltFont;
      property Style     : TFontStyles read fStyle      write setStyle;
      property Background: TColor      read fBackground write setBackground;
      property Foreground: TColor      read fForeground write setForeground;
      property Cursor    : TCursor     read fCursor     write fCursor;
    end;


  TKeywordEvent = procedure (Sender: TObject; StartKey, StopKey: TPlusNavigator; KIndex: Integer; var Accept: Boolean) of object;
  TQueryHilitEvent = procedure (Sender: TObject; Scope: SmallInt; var DoHilit: Boolean) of object;

  TCustomExtHighlighter = class(TPlusHighlighter)
    private
      fKeywords      : TExtKeywordList;
      fStartStopKeys : TExtStartStopList;
      fDelimiters    : TSysCharSet;
      fSubHighlighter: TPlusHighlighter;
      fActive        : Boolean;

      fOnKeyword,
      fOnStop, fOnStart :TKeywordEvent;
      fOnQueryHilit  : TQueryHilitEvent;

      fAltFont       : Boolean;
      fContext       : SmallInt;
      fBackgnd       : TColor;
      fForegnd       : TColor;
      fStyle         : TFontStyles;
      fPriority      : Integer;
      fScope         : Integer;

      procedure setActive(Value: Boolean);
      procedure SetSeparators(const sep: string);
      function  GetSeparators: string;
      procedure setAltFont(const Value: Boolean);
      procedure setBackgnd(const Value: TColor);
      procedure setForegnd(const Value: TColor);
      procedure setStyle(const Value: TFontStyles);
      procedure setPriority(const Value: Integer);
      procedure setScope(const Value: Integer);
      procedure setSubHighlighter(H: TPlusHighlighter);

    protected
      fNav1, fNav2, fNav3: TPlusNavigator;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);  override;
      function  FindStart(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean; override;
      function  FindStop(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean; override;
      function  GetPriority(const d: DynInfoRec; BaseIndex: Integer): SmallInt;
      property Delimiters: TSysCharSet read fDelimiters write fDelimiters;
      procedure Notify(Sender: TComponent; Events: TpmEvents); override;

      { support for global highlighting from descendants }
      property AltFont: Boolean read fAltFont write setAltFont default False;
      property Background: TColor read fBackgnd write setBackgnd default clNone;
      property ContextNum: SmallInt read fContext write fContext default 0;
      property Foreground: TColor read fForegnd write setForegnd default clNone;
      property Scope: Integer read fScope write setScope default 0;
      property Priority: Integer read fPriority write setPriority default 0;
      property Style: TFontStyles read fStyle write setStyle default [];
      property OnQueryHilit: TQueryHilitEvent read fOnQueryHilit write fOnQueryHilit;

      { support for extended keywords and start-stop key highlighting }
      property Keywords      : TExtKeywordList   read fKeywords      write fKeywords;
      property StartStopKeys : TExtStartStopList read fStartStopKeys write fStartStopKeys;
      property SubHighlighter: TPlusHighlighter  read fSubHighlighter write setSubHighlighter;
      property OnKeyword: TKeywordEvent read fOnKeyword write fOnKeyword;
      property OnStart  : TKeywordEvent read fOnStart   write fOnStart;
      property OnStop   : TKeywordEvent read fOnStop    write fOnStop;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function  FixRange(Start, Stop: TPlusNavigator; KeywordBase, SSBase: Integer): Boolean; override;
      procedure  ReApplyKeys;
      property Separators: string read GetSeparators write SetSeparators;
      property Active: Boolean read fActive write setActive default True;
    end;

  TExtHighlighter = class(TCustomExtHighlighter)
    published
      property Active;
      property Keywords;
      property StartStopKeys;
      property Separators;
      property SubHighlighter;
      property OnKeyword;
      property OnStart;
      property OnStop;
    end;

const
  QueryPriority = 32767;     { This value of Priority will make TExtHighlighter trigger an OnQueryHilit upon finding a start key
                               or a keyword }
implementation

{ ***** routine for dealing with IniFile storage/loading *********
        returns the comma delimited integer at the start of t;
        advances t past the delimiting comma }
{UCONVERT}
function ScanInt(var t: PChar): Integer;
var tstop: PChar; intstr: AnsiString;
begin
  tstop:= StrScan(t, ',');
  if tstop=nil then tstop:=StrEnd(t);
  SetLength(intstr, tstop-t);
  Move(t^, intstr[1], tstop-t);
  Result:= StrToInt(intstr);
  t:= tstop;
  if t^=',' then Inc(t)
end;
{/UCONVERT}


function TExtKeywordList.AddExtKeyWord(const KeyWord   : string;
                                       Options         : TWordOptions;
                                       Style           : TExtFontStyles;
                                       ContextNumber,
                                       Scope,
                                       Priority        : SmallInt;
                                       Cursor          : TCursor;
                                       Backgnd, Foregnd: TColor): Integer;
begin
  Result:= AddKeyWord(KeyWord, Options, Style, ContextNumber, Cursor, Backgnd, Foregnd);
  Self.Scope[Result]:= Scope;
  Self.Priority[Result]:= Priority
end;

procedure TExtKeywordList.LoadFromIniStrings(IniStrings: TStrings);
var ms: AnsiString;
    i: Integer;
    Opt: TWordOptions;
    St: TExtFontStyles;
    Cn: Integer;
    Cr: TCursor;
    Bg, Fg: TColor;
    SScope, SPriority: Integer;
    msp, tscan: PAnsiChar;
    eqpos: Integer;
begin
  BeginUpdate;
  Clear;

  try
    for i:= 0 to IniStrings.Count-1 do
      begin
        ms:= IniStrings[i];
        eqpos:= Pos('=', ms);
        if eqpos>0 then
          begin
            msp:= PAnsiChar(ms);
            tscan:= msp+eqpos;
            Byte(Opt):= ScanInt(tscan);
            Byte(St):= ScanInt(tscan);
            Cn:= ScanInt(tscan);
            Cr:= ScanInt(tscan);
            Bg:= ScanInt(tscan);
            Fg:= ScanInt(tscan);
            if tscan^<>#0 then
              begin
                SScope:= ScanInt(tscan);
                SPriority:= ScanInt(tscan);
              end
            else
              begin
                SScope:= 0;
                SPriority:= 0
              end;
            AddExtKeyword(Copy(ms, 1, eqpos-1), Opt, St, Cn, SScope, SPriority, Cr, Bg, Fg);
          end
      end

  finally
    EndUpdate;
  end
end;

function TExtKeywordList.getPriority(i: Integer): Integer;
begin
  Result:= pKeyInfoLen(KeyList[i]).Priority
end;

function TExtKeywordList.getScope(i: Integer): Integer;
begin
  Result:= pKeyInfoLen(KeyList[i]).Scope
end;

procedure TExtKeywordList.setPriority(i: Integer; const Value: Integer);
begin
  pKeyInfoLen(KeyList[i]).Priority:= Value
end;

procedure TExtKeywordList.setScope(i: Integer; const Value: Integer);
begin
  pKeyInfoLen(KeyList[i]).Scope:= Value
end;

function TExtStartStopList.AddExStartStopKey(const StartKey, StopKey: string;
                                             Options           : TWordOptions;
                                             Style             : TExtFontStyles;
                                             ContextNumber,
                                             Scope, Priority   : SmallInt;
                                             Cursor            : TCursor;
                                             Backgnd, Foregnd  : TColor;
                                             ssOptions         : TssOptions): Integer;
var pss : pStartStopInfo;
begin
  Result:= AddStartStopKey(StartKey, StopKey, Options, Style, ContextNumber, Cursor, Backgnd, Foregnd, ssoParStop in ssOptions);
  pss:= Pointers[Result];
  pss^.Scope:= Scope;
  pss^.Priority:= Priority;
  pss^.ssOptions:= ssOptions;
end;

function TExtStartStopList.getPriority(i: Integer): Integer;
begin
  Result:= pStartStopInfo(Pointers[i]).Priority
end;

function TExtStartStopList.getScope(i: Integer): Integer;
begin
  Result:= pStartStopInfo(Pointers[i]).Scope
end;

procedure TExtStartStopList.LoadFromIniStrings(IniStrings: TStrings);
var ms: AnsiString;
    ks: TStrings;
    i: Integer;
    Opt: TWordOptions;
    St: TExtFontStyles;
    Cn: Integer;
    Cr: TCursor;
    Bg, Fg: TColor;
    Ep: Integer;
    Sscope, SPriority: Integer;
    ssOpt: TssOptions;
    msp, tscan: PAnsiChar;
    eqpos: Integer;
begin
  if IniStrings<>nil then ks:= IniStrings
                     else ks:= TStringList.Create;

  Count:= 0;
  for i:= 0 to ks.Count-1 do
    begin
      ms:= ks[i];
      eqpos:= Pos('=', ms);
      if eqpos>0 then
        begin
          msp:= PAnsiChar(ms);
          tscan:= msp+eqpos;
          Byte(Opt):= ScanInt(tscan);
          Byte(St):= ScanInt(tscan);
          Cn:= ScanInt(tscan);
          Cr:= ScanInt(tscan);
          Bg:= ScanInt(tscan);
          Fg:= ScanInt(tscan);
          Ep:= ScanInt(tscan);
          if tscan^<>#0 then
            begin
              SScope:= ScanInt(tscan);
              SPriority:= ScanInt(tscan);
              Byte(ssOpt):= Ep
            end
          else
            begin
              SScope:= 0;
              SPriority:= 0;
              if Ep<>0 then ssOpt:= [ssoParStop]
                       else ssOpt:= []
            end;
          {if tscan^<>#0 then Byte(ssOpt):= ScanInt(tscan)
                        else ssOpt:= [];
          if Ep<>0 then Include(ssOpt, ssoParStop);}
          ms:= Copy(ms, 1, eqpos-1);
          eqpos:= Pos('|', ms);
          if eqpos<1 then eqpos:= Length(ms);
          AddExStartStopKey(Copy(ms, 1, eqpos-1), Copy(ms, eqpos+1, Length(ms)-eqpos),
                                 Opt, St, Cn, SScope, SPriority, Cr, Bg, Fg, ssOpt);
        end
    end
end;

procedure TExtStartStopList.setPriority(i: Integer; const Value: Integer);
begin
  pStartStopInfo(Pointers[i]).Priority:= Value
end;

procedure TExtStartStopList.setScope(i: Integer; const Value: Integer);
begin
  pStartStopInfo(Pointers[i]).Scope:= Value
end;

procedure THighlightInfo.Assign(Source: TPersistent);
var srch: THighlightInfo;
begin
  if Source is THighlightInfo then
    begin
      srch:= THighlightInfo(Source);
      fAltFont   := srch.fAltFont;
      fStyle     := srch.fStyle;
      fBackground:= srch.fBackground;
      fForeground:= srch.fForeground;
      if Assigned(fOnChange) then fOnChange(Self)
    end
  else inherited Assign(Source)
end;

procedure THighlightInfo.setAltFont(af: Boolean);
begin
  if af<>fAltFont then
    begin
      fAltFont:= af;
      if Assigned(fOnChange) then fOnChange(Self)
    end
end;

procedure THighlightInfo.setStyle(s: TFontStyles);
begin
  if s<>fStyle then
    begin
      fStyle:= s;
      if Assigned(fOnChange) then fOnChange(Self)
    end
end;

procedure THighlightInfo.setBackground(bg: TColor);
begin
  if bg<>fBackground then
    begin
      fBackground:= bg;
      if Assigned(fOnChange) then fOnChange(Self)
    end
end;

procedure THighlightInfo.setForeground(fg: TColor);
begin
  if fg<>fForeground then
    begin
      fForeground:= fg;
      if Assigned(fOnChange) then fOnChange(Self)
    end
end;

constructor TCustomExtHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fStartStopKeys:= TExtStartStopList.Create;
  fKeywords:= TExtKeywordList.Create;
  fDelimiters:= [#9, ' ', '.', ',', ';'];
  fBackgnd:= clNone; fForegnd:= clNone;
  fNav1:= TPlusNavigator.Create(nil);
  fNav2:= TPlusNavigator.Create(nil);
  fNav3:= TPlusNavigator.Create(nil);
  fActive:= True
end;

destructor TCustomExtHighlighter.Destroy;
begin
  fStartStopKeys.Free;
  fKeywords.Free;
  fNav1.Free;
  fNav2.Free;
  fNav3.Free;
  inherited Destroy
end;

procedure TCustomExtHighlighter.Notify(Sender: TComponent; Events: TpmEvents);
begin   { propagate context messages coming from Sender }
  if fSubHighlighter<>nil then IpmsNotify(fSubHighlighter).Notify(Sender, Events);
end;

procedure TCustomExtHighlighter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent=fSubHighlighter) and (Operation=opRemove) then fSubHighlighter:= nil;
  inherited Notification(AComponent, Operation)
end;

procedure TCustomExtHighlighter.ReApplyKeys;
var i: Integer;
begin
  for i:= 0 to MemoList.Count-1 do
      TPlusMemo(MemoList[i]).ReApplyKeywords
end;

procedure TCustomExtHighlighter.SetSeparators(const sep: string);
var i: Integer; del: TSysCharSet;
begin
  del:= [#0..#26];
  for i:= 1 to Length(sep) do del:= del + [pmChar(sep[i])];
  if del<>fDelimiters then
    begin
      fDelimiters:= del;
      if ComponentState*[csDesigning, csLoading]=[csDesigning] then ReApplyKeys
    end
end;

function TCustomExtHighlighter.GetSeparators: string;
var i: AnsiChar;
begin
  Result:= '';
  for i:= #27 to #255 do
      if i in fDelimiters then Result:= Result + i
end;

procedure TCustomExtHighlighter.setAltFont(const Value: Boolean);
begin
  if Value<>fAltFont then
    begin
      fAltFont := Value;
      if ComponentState*[csDesigning, csLoading]=[csDesigning] then ReApplyKeys
    end
end;

procedure TCustomExtHighlighter.setBackgnd(const Value: TColor);
begin
  if Value<>fBackgnd then
    begin
      fBackgnd := Value;
      if ComponentState*[csDesigning, csLoading]=[csDesigning] then ReApplyKeys
    end
end;

procedure TCustomExtHighlighter.setForegnd(const Value: TColor);
begin
  if Value<>fForegnd then
    begin
      fForegnd := Value;
      if ComponentState*[csDesigning, csLoading]=[csDesigning] then ReApplyKeys
    end
end;

procedure TCustomExtHighlighter.setStyle(const Value: TFontStyles);
begin
  if Value<>fStyle then
    begin
      fStyle := Value;
      if ComponentState*[csDesigning, csLoading]=[csDesigning] then ReApplyKeys
    end
end;

procedure TCustomExtHighlighter.setPriority(const Value: Integer);
begin
  if Value<>fPriority then
    begin
      fPriority := Value;
      if ComponentState*[csDesigning, csLoading]=[csDesigning] then ReApplyKeys
    end
end;

procedure TCustomExtHighlighter.setScope(const Value: Integer);
begin
  if Value<>fScope then
    begin
      fScope := Value;
      if ComponentState*[csDesigning, csLoading]=[csDesigning] then ReApplyKeys
    end
end;

procedure TCustomExtHighlighter.setSubHighlighter(H: TPlusHighlighter);
var sh: TPlusHighlighter;
begin
  if H<>fSubHighlighter then
    begin
      if H<>nil then
        begin   // Detect circular references
          sh:= H;
          repeat
            if sh=Self then raise Exception.Create('Circular highlighting references');
            if sh is TCustomExtHighlighter then sh:= TCustomExtHighlighter(sh).SubHighlighter
                                           else sh:= nil
          until sh = nil
        end;

      fSubHighlighter:= H;
      if H<>nil then H.FreeNotification(Self);
      if csDesigning in ComponentState then ReApplyKeys
    end
end;


function TCustomExtHighlighter.GetPriority(const d: DynInfoRec; BaseIndex: Integer): SmallInt;
var kindex: SmallInt;
begin
  Result:= 0;
  if (d.DynStyle and $80 <> 0) and (d.Level>=0) then
    begin
      kindex:= d.KeyIndex[d.Level];
      if kindex>=0 then Result:= High(Result)
      else
        begin
          kindex:= kindex and $7fff;
          if (kindex>=BaseIndex) and (kindex-BaseIndex<StartStopKeys.Count) then
                 Result:= pStartStopInfo(StartStopKeys.Pointers[kindex-BaseIndex])^.Priority
        end
    end
end;

procedure TCustomExtHighlighter.ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);
type keywfound = record keyw: Integer; Offset: Integer; keylen: Integer end;
var
  pp: pParInfo;
  pnb: Integer;
  nbkeywtodo: SmallInt;
  keywtodo: array[0..15] of keywfound;
  smemo: TPlusMemo;
  sposmod: Integer;   // min position modified with style

  procedure applykwfound;
    var ok, inkeyword: Boolean;
        drec: DynInfoRec;
        kinfo: pKeyInfoLen;
        k, m,
        currentlevel,
        currentscope,
        currentpriority: SmallInt;
    begin
      fNav1.fPMemo:= Start.fPMemo;
      fNav2.fPMemo:= Start.fPMemo;
      with fNav1 do
        begin
          fPar:= pp;
          fPos:= pp^.StartOffset;
          fDynNb:= 0; fOffset:= 0;
          fParLine:= 0;
          fParNb:= pnb;
        end;

      for k:= 0 to nbkeywtodo-1 do
        with keywtodo[k] do
          begin
            fNav1.Pos:= pp^.StartOffset+offset;
            fNav1.RightOfDyn;
            currentlevel:= DynToLevel(fNav1.pDynAttr^);
            currentscope:= DynToContext(fNav1.pDynAttr^);
            currentpriority:= GetPriority(fNav1.pDynAttr^, BaseIndex);
            kinfo:= Keywords.KeyList[keyw];
            ok:= False;

            // Determine scope based acceptance
            if (currentlevel<15) and
               ((kinfo^.Priority=QueryPriority) or
               (((kinfo^.Scope=0) and (kinfo^.Priority>currentpriority)) or (kinfo^.Scope=currentscope))) then
              begin
                fNav2.Assign(fNav1);
                fNav2.Pos:= fNav2.Pos+ keylen;
                ok:= True; inkeyword:= False;
                if fNav2.DynNb>fNav1.fDynNb then
                  begin
                    m:= fNav1.fDynNb+1;
                    while (m<=fNav2.fDynNb) and ok do
                      with fNav1.fPar^.ParExtra.DynCodes[m] do
                        begin
                          if DynStyle and $c0<>$c0 then begin ok:= inkeyword; inkeyword:= False end
                          else
                            if Level<currentlevel then ok:= False
                            else
                              if KeyIndex[Level]<0 then ok:= False
                                                   else inkeyword:= True;
                          Inc(m)
                        end;
                    if inkeyword then ok:= False
                  end
              end;

            // determine positional info acceptance
            if ok and (kinfo.BasicPart.Options*[woFirstParWord, woFirstNonBlank, woStartPar]<>[]) then
              if woStartPar in kinfo.BasicPart.Options then ok:= fNav1.fOffset=0
              else
                if woFirstNonBlank in kinfo.BasicPart.Options then
                    for m:= 0 to fNav1.fOffset-1 do
                      begin
                        if not (pmChar(fNav1.fPar.ParText[m]) in [' ', #9]) then
                          begin
                            ok:= False;
                            Break
                          end
                      end
                else
                    for m:= 0 to fNav1.fOffset-1 do
                        if not (pmChar(fNav1.fPar.ParText[m]) in Delimiters) then
                          begin
                            ok:= False;
                            Break
                          end;

            if ok and Assigned(fOnKeyword) then fOnKeyword(Self, fNav1, fNav2, keyw, ok);
            if ok then
              begin
                if sposmod<0 then sposmod:= fNav1.Pos
                             else
                                 if sposmod>fNav1.Pos then sposmod:= fNav1.Pos;
                with drec do
                  begin
                    KeyIndex:= fNav1.DynAttr.KeyIndex;
                    DynStyle:= Byte(kinfo.BasicPart.Style) or $c0;
                    Backgnd:= kinfo.BasicPart.Backgnd;
                    Foregnd:= kinfo.BasicPart.Foregnd;
                    Context:= kinfo.BasicPart.ContextNumber;
                    CollpsState:= [];
                    Cursor:= kinfo.BasicPart.Cursor;
                    StartKLen:= keylen;  //fNav2.Pos-fNav1.Pos;
                    StopKLen:= StartKLen;
                    Level:= CurrentLevel+1;
                    KeyIndex[Level]:= keyw + BaseIndex;
                  end;

                SetDynStyleP(smemo.IParList, fNav1, fNav2, drec, True, False);
              end
           end;   // k loop over keyword
           
      fNav1.fPMemo:= nil;
      fNav2.fPMemo:= nil;
      nbkeywtodo:= 0;
    end;  { local proc. applykwfound }

var j: Integer;
    tlow, tup, tfound, tsearch, s: PChar;
    { tlow, tup:  initial case text; tfound: working text; s: keyword we search }

    tlen, slen: Integer;
    dinfop: pKeyInfoLen;
    scar: Char;
    startoff, stopoff: Integer;
    tstartoffset: Integer;  { offset of tlow, tup in par.Text }

    startp, stopp: Integer;

    endword: Integer;
    slowcar, supcar: Char;
    kcount: Integer;


begin    { ApplyKeywordsList }
  if Keywords<>nil then kcount:= Keywords.Count
                   else kcount:= 0;

  if Active and (kcount>0) then
    begin
      smemo:= TPlusMemo(Start.fPMemo);
      pp:= Start.Par;
      tup:= nil;
      startp:= start.fParNb;
      stopp:= Stop.ParNumber;
      sposmod:= -1;  // flag as not set

      for pnb:= startp to stopp do
        begin
          if pp=nil then pp:= smemo.IParList.Pointers[pnb];

          if pnb=startp then
            begin
              startoff:= Start.fOffset;
              with fKeywords do
                if startoff>fLongestKeyword+1 then tstartoffset:= startoff-fLongestKeyword-1
                                              else tstartoffset:= 0
            end
          else begin startoff:= 0; tstartoffset:= 0 end;

          tlen:= GetParLength(pp^);
          if pnb=stopp then
            begin
              stopoff:= Stop.fOffset;
              with fKeywords do
                if stopoff+fLongestKeyword<tlen then tlen:= stopoff+fLongestKeyword-tstartoffset
                                                else tlen:= tlen-tstartoffset
            end
          else
            begin
              stopoff:= tlen;
              tlen:= tlen-tstartoffset
            end;

          if tlen=0 then begin pp:= nil; Continue end;

          { prepare tlow, tup }
          tlow:= pp^.ParText+tstartoffset;
          slowcar:= tlow[tlen];
          supcar:= #0;   // to avoid a warning
          scar:= #0;

          if not (woMatchCase in Keywords.GlobalWordOptions) then
            begin
              tup:= smemo.GetUptext(pp, pnb, tstartoffset, tlen);
              supcar:= tup[tlen]
            end;

          tlow[tlen]:= #0;
          if tup<>nil then tup[tlen]:= #0;

          { tlow, tup are ready, now iterate over keywords }
          nbkeywtodo:= 0;
          for j:= 0 to Keywords.Count-1 do
            begin
              dinfop:= pKeyInfoLen(Keywords.KeyList[j]);
              slen:= dinfop^.KeyLen;
              if dinfop^.BasicPart.ContextNumber<>0 then
                begin
                  if woMatchCase in dinfop^.BasicPart.Options then tsearch:= tlow
                                                              else tsearch:= tup;

                  if  (startoff>tstartoffset+slen) then tfound:= tsearch+(startoff-tstartoffset-slen)
                                                   else tfound:= tsearch;

                  endword:= stopoff+slen-tstartoffset;
                  if endword<tlen then
                    begin
                      scar:= tsearch[endword];
                      tsearch[endword]:= #0
                    end;
                  s:= PChar(dinfop.KeywordTrans);

                  while tfound<>nil do
                    begin
                    tfound:= StrPos(tfound, s);
                    if tfound<>nil then
                       begin
                       if not (woWholeWordsOnly in dinfop^.BasicPart.Options) or
                          (((tfound=tsearch) or (pmChar(tlow[tfound-tsearch-1]) in fDelimiters)) and
                           (((tfound-tsearch+slen=tlen)       or
                           (pmChar(tlow[tfound-tsearch+slen]) in fDelimiters))))
                           then
                           begin // add to keywtodo array
                             if nbkeywtodo=16 then applykwfound;
                             with keywtodo[nbkeywtodo] do
                               begin
                                 Offset:= tfound-tsearch+tstartoffset;
                                 keyw:= j;
                                 keylen:= slen
                               end;
                             Inc(nbkeywtodo);
                           end;  // boundaries are ok

                       tfound:= tfound + slen
                       end   // if tfound<>nil

                    end; { while tfound<>nil }
                  if endword<tlen then tsearch[endword]:= scar
                end;  // valid word options
            end;   // j loop over Keyword list

          if nbkeywtodo>0 then applykwfound;
          tlow[tlen]:= slowcar;
          if tup<>nil then tup[tlen]:= supcar;
          pp:= nil;
        end;     // i loop over paragraphs

      if sposmod>=0 then InvalidateNavs(smemo.INavigators, sposmod, stopp);
    end;   // keyword list not empty

  if fSubHighlighter<>nil then TCustomExtHighlighter(fSubHighlighter).ApplyKeywordsList(Start, Stop, BaseIndex+kcount)
end;  // method ApplyKeywordsList


function TCustomExtHighlighter.FindStop(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean; { they must be in the same par. }
var
  skeyindex  : SmallInt;
  sssindex   : SmallInt;
  slen       : Integer;
  t, keyfound: PChar;
  backstart,
  backend,
  eoff       : Longint;
  i          : Integer;
  tlen       : Integer;
  dinfo      : DynInfoRec;
  plen       : Integer;

  rightchar, endchar          : Char;
  leftdelcheck, rightdelcheck : Boolean;

  schecklen  : Integer;
  otherindex : Integer;
  otherssinfo,
  pssinfo    : pStartStopInfo;

  slevel     : SmallInt;
  scount     : Integer;
  spstartdyn : pDynInfoRec;
  smemo      : TPlusMemo;

begin
  Result:= False;
  spstartdyn:= Start.pDynAttr;
  slevel:= DynToLevel(spstartdyn^);
  if slevel<0 then Exit;
  skeyindex:= spstartdyn^.KeyIndex[slevel];
  sssindex:= (skeyindex and $7fff) - BaseIndex;
  if sssindex<0 then Exit;

  if (not Active) or (skeyindex>0) or (fStartStopKeys=nil) or (sssindex>= fStartStopKeys.Count) then
    begin
      if fStartStopKeys=nil then scount:= 0
                            else scount:= fStartStopKeys.Count;
      if fSubHighlighter<>nil then Result:= TCustomExtHighlighter(fSubHighlighter).FindStop(Start, Stop, scount+BaseIndex);
      Exit
    end;

  plen:= GetParLength(Start.fPar^);
  smemo:= TPlusMemo(Start.fPMemo);
  pSSInfo:= fStartStopKeys.Pointers[sssindex];

  slen:= pSSInfo^.StopLen;
  leftdelcheck:= pSSInfo^.StopLeftCheck;
  rightdelcheck:= pSSInfo^.StopRightCheck;
  schecklen:= slen;
  if leftdelcheck then Inc(schecklen);
  if Start.fOffset>=schecklen then backstart:= schecklen
                              else
                                begin
                                  backstart:= Start.fOffset;
                                  leftdelcheck:= False
                                end;

  { go past the start key }
  if Start.fDynNb>0 then
    with spstartdyn^ do
      if Start.fOffset-backstart<DynOffset+StartKLen then
        begin
          backstart:= Start.fOffset-(DynOffset+StartKLen);
          if leftdelcheck and (backstart<Start.fOffset) then Inc(backstart)
        end;

  schecklen:= slen;
  if rightdelcheck then Inc(schecklen);
  if Stop.ParOffset+schecklen>plen then
    begin
      eoff:= plen;
      backend:= 0;
      rightdelcheck:= False;
    end
  else
    begin
      eoff:= Stop.fOffset;
      backend:= schecklen
    end;

  tlen:= eoff-Start.ParOffset+backstart+backend;
  if not (woMatchCase in pssinfo^.Attributes.Options) then
      t:= smemo.GetUpText(Start.fPar, Start.fParNb, Start.fOffset-backstart, tlen)
  else
      t:= Start.fPar^.ParText+(Start.fOffset-backstart);

  endchar:= #0;

  if t<>nil then
    begin
      endchar:= t[tlen];
      t[tlen]:= #0
    end;
  rightchar:= #0;  // to avoid a warning

  { t is ready; look for a stop key in it }
  keyfound:= t;
  if leftdelcheck then Inc(keyfound);
  while (not Result) and (keyfound<>nil) do
    begin
      if rightdelcheck then
        begin
          rightchar:= t[tlen-1];
          t[tlen-1]:= #5
        end;

      if ssoDelStop in pSSInfo^.ssOptions then
        begin
          while (keyfound^<>#0) and (not (pmChar(keyfound^) in Delimiters)) do Inc(keyfound);
          Result:= keyfound^<>#0;
          if not Result then keyfound:= nil
        end

      else
        begin
          keyfound:= StrPos(keyfound, pSSInfo^.StopKey);
          if rightdelcheck then t[tlen-1]:= rightchar;

          if keyfound<>nil then
             if (pSSInfo^.StopLeftCheck and (keyfound-t>1) and (not (pmChar(t[keyfound-t-1]) in fDelimiters))) or
                (pSSInfo^.StopRightCheck and (keyfound-t<tlen-slen) and (not (pmChar(t[keyfound-t+slen]) in fDelimiters))) then
                   Inc(keyfound)
             else Result:= True
        end;

      if Result or ((ssoParStop in PSSInfo^.ssOptions) and (Stop.ParOffset=plen)) then
        begin
          Result:= True;
          fNav1.fPMemo:= smemo;
          if t<>nil then t[tlen]:= endchar;
          fNav1.Assign(Stop);
          if keyfound<>nil then
            begin
              eoff:= Start.Pos-backstart+(keyfound-t)+slen;
              Stop.Pos:= eoff-slen;
              Stop.RightOfDyn;
              while Stop.ForwardToDyn(eoff) do Stop.RemoveDyn;
              Stop.Pos:= eoff
            end;
          if Assigned(fOnStop) then fOnStop(Self, Start, Stop, sssIndex, Result);
          if not Result then
            begin
              if keyfound<>nil then Inc(keyfound);
              Stop.Assign(fNav1)
            end
        end;
    end;

  if t<>nil then t[tlen]:= endchar;

  if Result then
    begin
      fNav1.Assign(Start);
      fNav1.BackToDyn(0);
      if pSSInfo^.StopLeftCheck then Inc(slen);
      fNav1.Par^.ParExtra.DynCodes[fNav1.fDynNb].StopKLen:= slen;

      { check whether another identical start key exists }
      otherindex:= -1;
      with StartStopKeys do
        for i:= (skeyindex and $7fff)+1 to Count-1 do
          with pStartStopInfo(Pointers[i])^ do
            if (Previous=skeyindex and $7fff) and (Scope= PSSInfo^.Scope) and (Priority=PSSInfo^.Priority) then
             begin
               otherindex:= i;
               Break
             end;

      if otherindex=-1 then
        begin
          fNav1.Assign(Start);
          while DynToLevel(fNav1.pDynAttr^)>=spstartdyn.Level do fNav1.BackToDyn(0);
          dinfo:= fNav1.DynAttr
        end
      else
        with dinfo, StartStopKeys do
          begin
            otherssinfo:= Pointers[otherindex];
            KeyIndex:= spstartdyn.KeyIndex;
            CollpsState:= spstartdyn.CollpsState;
            CollpsLevel:= spstartdyn.CollpsLevel;
            Level:= spstartdyn.Level;
            DynStyle:= Byte(otherssinfo^.Attributes.Style) or $c0;
            KeyIndex[Level]:= -32768+otherindex;
            Cursor:= otherssinfo^.Attributes.Cursor;
            Backgnd:= otherssinfo^.Attributes.Backgnd;
            Foregnd:= otherssinfo^.Attributes.Foregnd
          end;

      dinfo.StartKLen:= 0;
      Stop.AddDyn(dinfo);
      if keyfound=nil then Stop.Pos:= Stop.Pos+1; // an end at par stop: advance this nav for parser to behave correctly
      fNav1.fPMemo:= nil;
    end;
end;     { FindStop }


function TCustomExtHighlighter.FindStart(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean;

  function CheckPosOk(Pt: PChar; Ofs: Integer; WO: TWordOptions): Boolean;
    var i: Integer;
    begin
      Result:= True;
      if woStartPar in WO then Result:= Ofs=0
      else
        if woFirstNonBlank in WO then
          begin
            for i:= 0 to Ofs-1 do
              if (Pt[i]<>' ') and (Pt[i]<>#9) then
                begin
                  Result:= False;
                  Break
                end
          end
        else
          if woFirstParWord in WO then
            begin
              for i:= 0 to Ofs-1 do
                if not (pmChar(Pt[i]) in Delimiters) then
                  begin
                    Result:= False;
                    Break
                  end
            end
    end;

var
  currentkey    : SmallInt;
  slen, tlen    : Integer;
  t, tup        : PChar;
  tcomp1, tcomp2: PChar;
  backstart,
  backend, eoff : Integer;
  i, ilim       : Integer;
  j, jlim, jadd : SmallInt;
  jssinfo       : pStartStopInfo;
  m, mlim       : Integer;
  dinfo         : DynInfoRec;
  checklen  : Integer;
  smemo     : TPlusMemo;
  scount    : Integer;
  sdyns     : TDynInfoArray;
  pdyn      : pDynInfoRec;
  currentdyn: DynInfoRec;
  currentpriority, currentscope: SmallInt;
  snavindex : Integer;
  plen      : Integer;

label jContinue, FindHStart;

begin
  Start.RightOfDyn;
  Result:= False;
  sdyns:= nil;  // to avoid a warning
  if DynToLevel(Start.pDynAttr^)>=15 then Exit;

  smemo:= TPlusMemo(Start.fPMemo);
  if fStartStopKeys=nil then scount:= 0
                        else scount:= fStartStopKeys.Count;

  if (not Active) or (scount=0) then goto FindHStart;

  with StartStopKeys do
    begin
      if not fDelChecked then
         for i:= 0 to Count-1 do
           with pStartStopInfo(Pointers[i])^ do
             begin
               StartLeftCheck:= (woWholeWordsOnly in Attributes.Options) and (not (pmChar(StartKey[0]) in fDelimiters));
               StartRightCheck:= (woWholeWordsOnly in Attributes.Options) and (not (pmChar(StartKey[StartLen-1]) in fDelimiters));
               StopLeftCheck:= (StopLen>0) and (woWholeWordsOnly in Attributes.Options) and (not (pmChar(StopKey[0]) in fDelimiters));
               StopRightCheck:= (StopLen>0) and (woWholeWordsOnly in Attributes.Options) and
                                (not (pmChar(StopKey[StopLen-1]) in fDelimiters))
             end;
      fDelChecked:= True
    end;

  fNav1.fPMemo:= smemo;
  fNav1.Assign(Start);

  if Start.fOffset>=fStartStopKeys.fLongestStartKey+1 then backstart:= fStartStopKeys.fLongestStartKey+1
                                                      else backstart:= Start.ParOffset;
  fNav1.Pos:= Start.Pos-backstart;
  eoff:= Stop.ParOffset;
  plen:= GetParLength(Stop.fPar^);
  if eoff+fStartStopKeys.fLongestStartKey+1>plen then backend:= plen-eoff
                                                 else backend:= fStartStopKeys.fLongestStartKey+1;
  tlen:= eoff-Start.fOffset+backstart+backend;
  if tlen<=0 then goto FindHStart;

  t:= Start.fPar^.ParText + Start.fOffset-backstart;
  tup:= nil;

  m:= fNav1.DynNb;
  mlim:= GetDynCount(Start.fPar^);

  i:= 0;
  jadd:= fStartStopKeys.fElementSpace;
  jlim:= fStartStopKeys.Count;
  sdyns:= GetDynArray(Start.fPar^);
  ilim:= tlen-backend;
  slen:= 0;

  currentdyn:= fNav1.DynAttr;
  currentpriority:= getpriority(currentdyn, BaseIndex);
  currentscope:= DynToContext(currentdyn);

  while i<=ilim do
    begin
      while (m<mlim) and (sdyns[m].DynOffset<=i+fNav1.fOffset) do
        begin
          currentdyn:= sdyns[m];
          Inc(m);
          currentpriority:= GetPriority(currentdyn, BaseIndex);
          currentscope:= DynToContext(currentdyn)
        end;

      jssinfo:= fStartStopKeys.Pointers[0];
      j:= 0;
      while j<jlim do
        begin
          if (jssinfo^.Priority=QueryPriority) or
             ((jssinfo^.Scope=0) and (jssinfo^.Priority>currentpriority)) or
             ((jssinfo^.Scope=currentscope) and ((jssinfo^.Scope<>0) or (currentdyn.DynStyle and $80=0))) then
            begin
              slen:= jssinfo^.StartLen;
              if jssinfo^.StartRightCheck then checklen:= slen+1 else checklen:= slen;
              if (i+checklen>backstart) and (i+slen<=tlen) then
                begin
                  tcomp1:= jssinfo^.StartKey;
                  if not (woMatchCase in jssinfo^.Attributes.Options) then
                    begin
                      if tup=nil then tup:= smemo.GetUpText(Start.fPar, Start.fParNb, fNav1.fOffset, tlen);
                      tcomp2:= tup+i
                    end
                  else tcomp2:= t+i;

                  while (slen>0) and (tcomp1^=tcomp2^) do
                    begin
                      Dec(slen);
                      Inc(tcomp1);
                      Inc(tcomp2)
                    end;

                  if slen=0 then
                    begin
                      slen:= jssinfo^.StartLen;
                      if not (jssinfo^.StartLeftCheck and (i>0) and (not (pmChar(t[i-1]) in fDelimiters))) and
                         not(jssinfo^.StartRightCheck and (i+slen<tlen) and (not (pmChar(t[i+slen]) in fDelimiters))) and
                         CheckPosOk(Start.fPar.ParText, i+Start.fOffset-backstart, jssinfo^.Attributes.Options) then
                        begin
                          Result:= True;
                          Break
                        end
                    end
                end
            end;    // scope, priority is ok
          jContinue:
          Inc(PAnsiChar(jSSInfo), jadd);
          Inc(j)
        end;  // j loop over start/stop keys

      if not Result then Inc(i)
      else
        begin
          fNav1.Pos:= fNav1.Pos+i;
          fNav1.RightOfDyn;
          if Assigned(fOnStart) then fOnStart(Self, fNav1, nil, j, Result);
          if not Result then
            begin
              fNav1.Pos:= fNav1.Pos - i;
              goto jContinue
            end;

          pdyn:= fNav1.pDynAttr;
          dinfo.Level:= DynToLevel(pdyn^)+1;
          dinfo.CollpsLevel:= DynToCollapseLevel(pdyn^);
          { check whether we are nesting within the same kind of start-stop key,
            which must be avoided if stopkey=startkey}
          if dinfo.Level-1>=0 then
            if (fNav1.DynAttr.KeyIndex[dinfo.Level-1]=j or SmallInt($8000)) and
                ((jssinfo^.StartLen=jssinfo^.StopLen) and
                 (StrComp(jssinfo^.StartKey, jssinfo^.StopKey)=0)) then
              begin
                Inc(i);
                Result:= False;
                Continue
              end;
          Stop.Assign(fNav1);
          if dinfo.Level>0 then
            with Stop do
              begin
                if BackToDyn(0) then Par^.ParExtra.DynCodes[DynNb].StopKLen:= 0;
                Assign(fNav1);
                RightOfDyn
              end;

          {remove stray dyn codes}
          while Stop.ForwardToDyn(fNav1.Pos+slen) do Stop.RemoveDyn;

          with dinfo do
            begin
              DynOffset:= fNav1.ParOffset;
              KeyIndex:= Stop.DynAttr.KeyIndex;
              DynStyle:= Byte(jssinfo^.Attributes.Style) or $c0;
              KeyIndex[Level]:= -32768+j + BaseIndex;
              if ssoCollapsible in jssinfo^.ssOptions then
                begin
                  Inc(CollpsLevel);
                  CollpsState:= [pmdCollapsible]
                end
              else
                  CollpsState:= [];
              Cursor:= jssinfo^.Attributes.Cursor;
              Backgnd:= jssinfo^.Attributes.Backgnd;
              Foregnd:= jssinfo^.Attributes.Foregnd;
              Context:= jssinfo^.Attributes.ContextNumber;
              StartKlen:= slen;
              StopKLen:= jssinfo^.StopLen;
              if jssinfo^.StartRightCheck then Inc(StartKLen)
            end;
          with fNav1 do
            if (fDynNb<Length(sdyns)) and (sdyns[fDynNb].DynOffset=fOffset) then sdyns[fDynNb]:= dinfo
                                                                            else AddDyn(dinfo);
          Stop.Pos:= fNav1.Pos+slen;
          Stop.RightOfDyn;
          Break
        end // found start key

      end; // iloop in text buffer 


  FindHStart:
  fNav1.fPMemo:= nil;
  if fSubHighlighter<>nil then Result:= Result or TCustomExtHighlighter(fSubHighlighter).FindStart(Start, Stop, scount+BaseIndex)
end;    // FindStart

function TCustomExtHighlighter.FixRange(Start, Stop: TPlusNavigator; KeywordBase, SSBase: Integer): Boolean;

  function GetWordOptions(const d: DynInfoRec): TWordOptions;
    var skey: Integer; pkl: pKeyInfoLen; ssp: pStartStopInfo;
    begin
      skey:= d.KeyIndex[d.Level];
      if skey>=0 then
          if (skey>=KeywordBase) and (skey<Keywords.KeyList.Count+KeywordBase) then
            begin
              pkl:= Keywords.KeyList[skey-KeywordBase];
              Result:= pkl.BasicPart.Options
            end
          else
              Result:= []
      else
        begin
          skey:= skey + 32768 -SSBase;
          if (skey>=0) and (skey<StartStopKeys.Count) then
            begin
              ssp:= StartStopKeys.Pointers[skey];
              Result:= ssp.Attributes.Options
            end
          else
              Result:= []
        end
    end;

var t: PChar; plen, i, b, ndyn: Integer; acceptdel: Boolean; c: AnsiChar; wo: TWordOptions;
begin
  Result:= False;
  if Active and ((Keywords.GlobalWordOptions+StartStopKeys.GlobalWordOptions) * [woFirstParWord, woFirstNonBlank] <> []) then
    begin
      plen:= GetParLength(Stop.Par^);
      if Stop.fOffset<plen then
        begin
          ndyn:= GetDynCount(Stop.fPar^);
          if (ndyn>0) and (Stop.DynNb=0) and (DynToLevel(Stop.fPar.ParExtra.DynCodes[0])>DynToLevel(Stop.pDynAttr^)) then
            begin
              wo:= GetWordOptions(Stop.fPar.ParExtra.DynCodes[0]);
              if wo*[woFirstParWord, woFirstNonBlank]<>[] then Stop.ParOffset:= Stop.fPar.ParExtra.DynCodes[0].DynOffset
            end;

          acceptdel:= woFirstParWord in (Keywords.GlobalWordOptions+StartStopKeys.GlobalWordOptions);
          t:= Stop.fPar.ParText;
          b:= -1;
          for i:= 0 to plen-1 do
            begin
              c:= pmChar(t[i]);
              if (acceptdel and not (c in Delimiters)) or (not acceptdel and not (c in [' ', #9])) then
                begin
                  b:= i;
                  Break
                end
            end;
          if b>Stop.fOffset then
            begin
              Result:= True;
              Stop.ParOffset:= b
            end
        end
    end
end;

procedure TCustomExtHighlighter.setActive(Value: Boolean);
begin
  if Value<>Active then
    begin
      fActive:= Value;
      if csDesigning in ComponentState then ReApplyKeys
    end
end;

end.
