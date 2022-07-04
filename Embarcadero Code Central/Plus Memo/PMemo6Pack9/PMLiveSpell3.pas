unit PMLiveSpell3;
{ Copyright (c) Electro-Concept Mauricie, 2001-2003 }

{ Implements TPMLiveSpell3 component, used for live spell checking of the text content
  in a TPlusMemo.

  Version of April 18, 2003

  Requires Addict3 to be installed on your system.  Install this source file in a package for
  design time access from your component palette: TPMLiveSpell3 will be shown on the PlusMemo page by default.

  Version 6.1a: works with PlusMemo v6.1 or later
  }

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$DEFINE PMLiveSpell3}

interface
uses Classes, PlusMemo, PMSupport, ExtHilit, Ad3SpellBase, ad3StringParser, ad3ParserBase;

type
  TSuggestPopupEvent = procedure(Sender: TObject; BadWord: string; var DoPopup: Boolean) of object;

  TPMLiveSpell3 = class(TCustomExtHighlighter)
    private
      fAddictSpell: TAddictSpell3Base;
      fStringParser: TStringParser;     // Internal string parser object used inside of ApplyKeywordsList
      fParsingEngine: TParsingEngine;   // Internal parsing engine used inside of ApplyKeywordsList
      fDeferredNavList: TList;          // list of navigators pointing to deferred word
      fDeferredBaseIndex: TList;        // base index of deferred spell checkings
      fInProcessDeferred: Boolean;      // whether we are inside deferred processing
      fLiveCorrectNav: TPlusNavigator;  // a navigator positioned where live correct must be done
      fLiveCorrectWord: AnsiString;     // replacement string when doing live correct
      fLiveCorrectLen: Integer;         // length of part to replace when doing live correct
      fMemoList: TList;                 // list of TPlusMemo we have notification attached to
      fOnSuggestPopup: TSuggestPopupEvent; // Triggered when showing the popup menu associated with a bad word
      procedure DeferredRemove(Sender: TObject);
      procedure setParsingEngine(const NewEngine: TParsingEngine);
      procedure setAddictSpell(Spell: TAddictSpell3Base);
      procedure AddNotificationTo(AMemo: TPlusMemo);
    protected
      procedure ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);  override;
      procedure Loaded; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure Notify(Sender: TComponent; Events: TpmEvents); override;  // IpmsNotify
      procedure ProcessDeferred(Memo: TPlusMemo);   // process deferred spell checking and live correct
      procedure SuggestionPopup(Memo: TPlusMemo);   // open the suggestion popup menu
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      property ParsingEngine: TParsingEngine read fParsingEngine write setParsingEngine;
    published
      property Active;
      property ContextNum;
      property Speller: TAddictSpell3Base read fAddictSpell write setAddictSpell;
      property Priority;
      property Scope;
      property Separators;
      property SubHighlighter;
      property OnSuggestPopup: TSuggestPopupEvent read fOnSuggestPopup write fOnSuggestPopup;
    end;

procedure Register;

implementation
uses SysUtils, Graphics, Windows, Controls, ad3PlusMemoParser, ad3ParseEngine, ad3Configuration;

procedure Register;
begin
  RegisterComponents({UCONVERT}'PlusMemo'{/UCONVERT}, [TPMLiveSpell3]);
end;

{$IFDEF PMLiveSpell3U}
function WPosToAPos(const WString: WideString; WPos: Integer): Integer;
var s: AnsiString;
begin
  s:= Copy(WString, 1, WPos-1);
  Result:= Length(s)+1;
  if WPos>Length(WString)+1 then
      Inc(Result, WPos-Length(WString)-1)
end;

function APosToWPos(const AString: AnsiString; APos: Integer): Integer;
var w: WideString;
begin
  w:= Copy(AString, 1, APos-1);
  Result:= Length(w)+1;
  if APos>Length(AString)+1 then
      Inc(Result, APos-Length(AString)-1)
end;
{$ENDIF}

{ TPMLiveSpell3 }

procedure TPMLiveSpell3.AddNotificationTo(AMemo: TPlusMemo);
var snotify: IpmsNotify;
begin
  if fMemoList.IndexOf(AMemo)<0 then   // only process if we did not notify already
    begin
      fMemoList.Add(AMemo);
      snotify:= Self;
      if AMemo.NotifyList.IndexOf(Pointer(snotify))<0 then 
        begin
          AMemo.NotifyList.Add(Pointer(snotify));
          AMemo.FreeNotification(Self)
        end
    end
end;

procedure TPMLiveSpell3.ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);
type badwfound = record Offset, WordLen: Integer end;  // used to record locations of bad words
var
  nbbadwtodo: SmallInt;
  badwtodo  : array[0..15] of badwfound; // up to 16 bad words are processed in batch
  bwsome    : Boolean;                   // whether any badword has been highlighted
  smemo     : TPlusMemo;

  procedure applybwfound;     // process the batched bad words in badwtodo
    var drec: DynInfoRec;
        k: SmallInt;
    begin
      for k:= 0 to nbbadwtodo-1 do
        with badwtodo[k] do
          begin
            { set up navigators for this bad word }
            fNav1.Pos:= fNav1.fPar^.StartOffset+offset;
            fNav1.RightOfDyn;
            fNav2.Assign(fNav1);
            fNav2.Pos:= fNav2.Pos+ WordLen;

            { set up dyn record }
            drec:= fNav1.DynAttr;
            with drec do
              begin
                if DynStyle and $80<>0 then DynStyle:= $d8 or DynStyle
                else
                  begin
                    DynStyle:= (Byte(fNav1.Style) xor Byte(smemo.Font.Style)) or $d8;
                    Level:= -1;
                    Backgnd:= -1;
                    Foregnd:= -1
                  end;
                Context:= ContextNum;
                Cursor:= 0;
                StartKLen:= Wordlen;
                StopKLen:= Wordlen;
                Inc(Level);
                KeyIndex[Level]:= BaseIndex;
              end;

            bwsome:= True;
            SetDynStyleP(smemo.IParList, fnav1, fNav2, drec, True, fInProcessDeferred);
          end;
      nbbadwtodo:= 0;
    end;  // local proc. applybwfound

var
  i        : Longint;         // paragraph index for looping
  twork    : PChar;           // working paragraph text
  tlen     : Integer;         // length of twork
  startoff,
  stopoff  : Integer;         // offsets in twork we have to parse
  endword  : Integer;
  cp       : Integer;         // current cursor position in paragraph, or High(Integer) if cursor is not in paragraph
  pdyn     : pDynInfoRec;     // pointer to dyn record of start of word being processed
  curword  : AnsiString;      // word being processed
  curpar   : AnsiString;      // same as twork, ansi version
  curparw  : string;          // same as twork, string or WideString version
  good     : Boolean;
  defnav   : TPlusNavigator;
  dummy    : Integer;
  sbackcars: TSysCharSet;

begin    { ApplyKeywordsList }
  if (not fInProcessDeferred) and (SubHighlighter<>nil) then
      TPMLiveSpell3(SubHighlighter).ApplyKeywordsList(Start, Stop, BaseIndex+1);
  if (fAddictSpell=nil) or (not Active) or (csDesigning in ComponentState) then Exit;

  smemo:= TPlusMemo(Start.fPMemo);
  fNav1.fPMemo:= smemo;
  fNav2.fPMemo:= smemo;
  bwsome:= False;
  fNav1.fPar:= Start.Par;


  for i:= Start.ParNumber to Stop.ParNumber do
    begin
      fNav1.ParNumber:= i;
      if smemo.CurrentPosNav.ParNumber=i then cp:= smemo.CurrentPosNav.fOffset
                                         else cp:= High(cp);

      twork:= fNav1.fPar^.ParText;
      tlen:= GetParLength(fNav1.fPar^);

      if tlen=0 then
        begin
          fNav1.fPar:= nil;
          Continue
        end;

      SetLength(curparw, tlen);
      Move(twork^, curparw[1], tlen*SizeOf(curparw[1]));
      curpar:= curparw;
      fStringParser.Initialize(@curpar);
      fParsingEngine.Initialize(fStringParser, CheckType_All);

      if i=Start.fParNb then
      begin
        startoff:= Start.fOffset;
        sbackcars:= smemo.Delimiters-['.'];
        while (startoff>0) and (not (pmChar(twork[startoff-1]) in sbackcars)) do Dec(startoff);
        fParsingEngine.AdjustToPosition({$IFDEF PMLiveSpell3U} WPosToAPos(curparw, startoff+1), 0)
                                        {$ELSE} startoff+1, 0)
                                        {$ENDIF}
      end;

      fStringParser.GetCursorPosition(startoff, dummy);
      {$IFDEF PMLiveSpell3U} startoff:= APosToWPos(curpar, startoff); {$ENDIF}
      Dec(startoff);

      if i=Stop.fParNb then stopoff:= Stop.fOffset
                       else stopoff:= tlen;

      nbbadwtodo:= 0;

      while startoff<=stopoff do
        begin
          { fetch current word and set boundaries (startoff, endword) }
          curword:= fParsingEngine.NextWord;
          fStringParser.GetCursorPosition(endword, dummy);
          startoff:= endword - Length(curword);
          {$IFDEF PMLiveSpell3U} endword:= APosToWPos(curpar, endword); {$ENDIF}
          Dec(endword);
          {$IFDEF PMLiveSpell3U} startoff:= APosToWPos(curpar, startoff); {$ENDIF}
          Dec(startoff);
          if (startoff>stopoff) or (startoff=tlen) then Break;

          { do we have to check this word? }
          good:= endword>startoff;
          if good then
            begin
              fNav1.ParOffset:= startoff;
              fNav1.RightOfDyn;
              pDyn:= fNav1.pDynAttr;
              with fNav1 do
                good:= (DynToLevel(pDyn^)<15) and ((Scope=DynToContext(pDyn^)) or (Priority>0)) and
                       ((fDynNb>=GetDynCount(fPar^)) or (fPar.ParExtra.DynCodes[fDynNb].DynOffset>=endword))
            end;

          if good then
            begin
              if (startoff<=cp) and (endword>=cp) then
                begin        // currently editing this word, arrange a deferred spell check for it
                  if fDeferredNavList=nil then
                    begin
                      fDeferredNavList:= TList.Create;
                      fDeferredBaseIndex:= TList.Create
                    end;

                  { check if there is already a deferred check for this memo }
                  defnav:= nil;
                  for dummy:= 0 to fDeferredNavList.Count-1 do
                    begin
                      defnav:= TPlusNavigator(fDeferredNavList[dummy]);
                      if defnav.fPMemo=smemo then Break;
                      defnav:= nil
                    end;

                  if defnav=nil then
                    begin   // add this deferred check
                      defnav:= TPlusNavigator.Create(smemo);
                      defnav.OnFree:= DeferredRemove;
                      defnav.Assign(fNav1);
                      fDeferredNavList.Add(defnav);
                      fDeferredBaseIndex.Add(Pointer(BaseIndex));
                      AddNotificationTo(smemo);  // to receive OnSelMove events
                    end
                  else
                      defnav.Assign(fNav1)
                end

              else
                begin   // ok, spell check it
                  good:= fAddictSpell.WordAcceptable(curword);

                  // v6.2c: if soHtml, then check if this word is followed or preceeded by an html entity
                  if not good and (soHtml in fAddictSpell.Configuration.SpellOptions) then
                    if ((startoff > 0) and (twork[startoff-1]=';')) or
                       ((endword<tlen) and (twork[endword]='&')) then
                        good:= True;

                  if not good then
                    begin
                      if Assigned(OnKeyword) then OnKeyword(Self, fNav1, fNav2, 0, good);
                      if not good then
                        if fInProcessDeferred and fAddictSpell.LiveCorrect and
                           fAddictSpell.WordHasCorrection(curword, fLiveCorrectWord) then
                          begin
                            fLiveCorrectNav:= TPlusNavigator.Create(smemo);
                            fLiveCorrectNav.Assign(fNav1);
                            fLiveCorrectLen:= endword-startoff
                          end
                        else
                          begin    // add it to the batch list
                            if nbbadwtodo=16 then applybwfound;
                            with badwtodo[nbbadwtodo] do
                              begin
                                Offset:= startoff;
                                WordLen:= endword-startoff
                              end;
                            Inc(nbbadwtodo);
                          end
                    end // not good
                end  // spell check current word
            end;  // current word needs spell checked

          startoff:= endword+1
        end;   // while loop into paragraph text

      if nbbadwtodo>0 then applybwfound;
      fNav1.fPar:= nil;
    end;     // for i loop over paragraphs

  fNav1.fPMemo:= nil;
  fNav2.fPMemo:= nil;
  if bwsome then
    begin
      AddNotificationTo(smemo); // add ourself to notification list, to receive context events
      InvalidateNavs(smemo.INavigators, Start.Pos, Stop.fParNb)
    end
end;  // method ApplyKeywordsList 

constructor TPMLiveSpell3.Create(AOwner: TComponent);
begin
  inherited;
  ContextNum:= 2000;
  fStringParser:= TStringParser.Create;
  fParsingEngine:= TMainParsingEngine.Create;
  fMemoList:= TList.Create;
  Delimiters:= []
end;

procedure TPMLiveSpell3.DeferredRemove(Sender: TObject);
  { A navigator recorded for deferred spell checking has been destroyed }
var i: Integer;
begin
  if fDeferredNavList<>nil then
    for i:= 0 to fDeferredNavList.Count-1 do
      if fDeferredNavList[i]=Sender then
        begin
          fDeferredNavList.Delete(i);
          fDeferredBaseIndex.Delete(i);
          Break
        end
end;

destructor TPMLiveSpell3.Destroy;
var i: Integer; snotify: IpmsNotify;
begin
  snotify:= Self;
  for i:= 0 to fMemoList.Count-1 do
      TPlusMemo(fMemoList[i]).NotifyList.Remove(Pointer(snotify));

  FreeAndNil(fMemoList);
      
  if fDeferredNavList<>nil then
    begin
      fDeferredNavList.Free;
      fDeferredBaseIndex.Free
    end;
  fStringParser.Free;
  fParsingEngine.Free;
  inherited;
end;

procedure TPMLiveSpell3.Loaded;
begin
  inherited;
  Delimiters:= []
end;

procedure TPMLiveSpell3.Notification(AComponent: TComponent; Operation: TOperation);
var i: Integer;
begin
  if Operation=opRemove then
    begin
      if AComponent=fAddictSpell then fAddictSpell:= nil;
      if fMemoList<>nil then
        begin
          for i:= 0 to fMemoList.Count-1 do
              if AComponent=fMemoList[i] then fMemoList[i]:= nil;
          fMemoList.Pack
        end
    end;

  inherited Notification(AComponent, Operation)
end;

procedure TPMLiveSpell3.Notify(Sender: TComponent; Events: TpmEvents);
begin
  if pmeSelMove in Events then
      if not fInProcessDeferred then ProcessDeferred(TPlusMemo(Sender));
  if pmeRightContext in Events then SuggestionPopup(TPlusMemo(Sender));
  inherited
end;

procedure TPMLiveSpell3.ProcessDeferred(Memo: TPlusMemo);
  { spell check the word which was deferred because it was being edited }
var i, bi: Integer; snav: TPlusNavigator;
begin
  fInProcessDeferred:= True;
  if fDeferredNavList<>nil then
    for i:= 0 to fDeferredNavList.Count-1 do
      begin
        snav:= TPlusNavigator(fDeferredNavList[i]);
        if snav.fPMemo=Memo then
          begin
            Memo.BeginUpdate;
            snav.OnFree:= nil;
            fDeferredNavList.Delete(i);
            bi:= Integer(fDeferredBaseIndex[i]);
            fDeferredBaseIndex.Delete(i);
            ApplyKeywordsList(snav, snav, bi);
            Memo.EndUpdate;
            snav.Free;
            
            if Assigned(fLiveCorrectNav) then
              begin // apply LiveCorrect change
                { remember current selection position }
                snav:= TPlusNavigator.Create(Memo);
                snav.Assign(Memo.CurrentPosNav);
                bi:= Memo.SelLength;
                { make the livecorrect replacement }
                Memo.SelStart:= fLiveCorrectNav.Pos;
                Memo.SelLength:= fLiveCorrectLen;
                Memo.SelText:= fLiveCorrectWord;
                { replace selection }
                Memo.SelStart:= snav.Pos;
                Memo.SelLength:= bi;
                { return things to rest }
                snav.Free;
                fLiveCorrectNav.Free;
                fLiveCorrectNav:= nil
              end;

            Break
          end
      end;
    fInProcessDeferred:= False
end;

function WordAtNav(anav: TPlusNavigator; const w: string): Boolean;
var t1, t2: PChar; i: Integer;
begin
  Result:= True;
  i:= Length(w);
  t1:= anav.Par^.ParText+anav.ParOffset;
  t2:= @w[1];
  while i>0 do
    if t1^<>t2^ then
      begin
        Result:= False;
        Exit
      end
    else
      begin
        Inc(t1); Inc(t2);
        Dec(i)
      end
end;

procedure TPMLiveSpell3.setAddictSpell(Spell: TAddictSpell3Base);
begin
  if Spell<>fAddictSpell then
    begin
      fAddictSpell:= Spell;
      if Spell<>nil then Spell.FreeNotification(Self)
    end;
end;

procedure TPMLiveSpell3.setParsingEngine(const NewEngine: TParsingEngine);
begin
  if Assigned(NewEngine) then
    begin
      fParsingEngine.Free;
      fParsingEngine := NewEngine;
    end;
end;

procedure StripAmpersand(var s: string);
var slen: Integer;
begin
  slen:= Pos('&', s);
  if slen>0 then Delete(s, slen, 1)
end;

procedure TPMLiveSpell3.SuggestionPopup(Memo: TPlusMemo);
  procedure ScanMemo(replace: Boolean; const oword, nword: string);
    var n1, savedstart: TPlusNavigator;
    begin
      Memo.BeginUpdate;
      n1:= TPlusNavigator.Create(Memo);
      savedstart:= TPlusNavigator.Create(Memo);
      savedstart.Assign(Memo.SelStartNav);
      while n1.ForwardToDyn(Memo.CharCount) do
        begin
          n1.RightOfDyn;
          if (DynToContext(n1.DynAttr)=ContextNum) and WordAtNav(n1, oword) then
            begin
              Memo.SelStart:= n1.Pos;
              Memo.SelLength:= Length(oword);
              if replace then Memo.SelText:= nword
                         else Memo.SelText:= oword
            end
        end;
      n1.Free;
      Memo.SelStart:= savedstart.Pos;
      savedstart.Free;
      Memo.EndUpdate
    end;

var cw, oldword: string; where: TPoint; worknav: TPlusNavigator; cwlen: Integer; sparser: TPlusMemoControlParser;
    cansi: AnsiString; dopopup: Boolean;
begin
  if (Memo.LastContext=ContextNum) {and (Memo.PopupMenu=nil)} then
    begin
      worknav:= TPlusNavigator.Create(Memo);
      worknav.Assign(Memo.MouseNav);
      cwlen:= GetParLength(worknav.Par^);
      if not worknav.ForwardToDyn(worknav.Par.StartOffset+cwlen) then worknav.ParOffset:= High(worknav.ParOffset);
      cwlen:= worknav.Pos;
      worknav.BackToDyn(worknav.fPar.StartOffset);
      cwlen:= cwlen - worknav.Pos;
      SetLength(cw, cwlen);
      if cwlen>0 then worknav.GetTextBuf(PChar(cw), cwlen);
      oldword:= cw;
      cansi:= cw;
      Memo.SelStart:= Memo.MouseNav.Pos;
      where:= Memo.ClientToScreen(Point(Memo.CaretX, Memo.CaretY));
      dopopup:= True;
      if Assigned(fOnSuggestPopup) then fOnSuggestPopup(Self, cw, dopopup);
      if dopopup then
        begin
          case fAddictSpell.ShowPopupMenu(Memo, [spCancel, spDialog, spChangeAll, spAdd, spIgnoreAll, spIgnore, spReplace],
                                          where.X, where.Y+Memo.LineHeight, cansi) of
            spReplace:
              begin
                cw:= cansi;
                Memo.SelStart:= worknav.Pos;
                Memo.SelLength:= cwlen;
                StripAmpersand(cw);
                Memo.SelText:= cw
              end;
            spDialog:
              begin
                Memo.SelStart:= worknav.Pos;    // go back to beginning of word
                sparser:= TPlusMemoControlParser.Create;
                sparser.Initialize(Memo);
                fAddictSpell.CheckParser(sParser, ctFromCursor)
              end;
            spIgnore: Memo.ClearStyle(Memo.SelStart);
            spAdd, spIgnoreAll: ScanMemo(False, oldword, cansi);
            spChangeAll:
              begin
                cw:= cansi;
                StripAmpersand(cw);
                ScanMemo(True, oldword, cw)
              end;
            end;
          Memo.LastContext:= 0
        end;
      worknav.Free
    end;
end;

end.
