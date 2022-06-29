unit NbHilit;

{ © Electro-Concept Mauricie, 1999-2004 }
{ Implements TNumberHighlighter object, used for dynamic highlighting of numbers
  in a TPlusMemo }

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$DEFINE NbHilit}
{UCONVERT}
  {$IFDEF NbHilitClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}


interface

{$IFDEF pmClx}
uses Classes, ExtHilitClx, PMSupportClx;
{$ELSE}
uses Classes, ExtHilit, PMSupport;
{$ENDIF}

type
  TNumberHighlighter = class(TCustomExtHighlighter)
    private
      fCTypeHex: Boolean;
    procedure setCTypeHex(const Value: Boolean);
    protected
      procedure ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);  override;
      function OkContext(const dyn: DynInfoRec): Boolean;
    public
      constructor Create(AOwner: TComponent); override;
      property Delimiters;
    published
      { promotes from TCustomExtHighlighter }
      property AltFont;
      property Background;
      property ContextNum default 3000;
		  property CTypeHex: Boolean read fCTypeHex write setCTypeHex default False;
      property Foreground default $0ff;
      property Separators;
      property Style;
      property Scope;
      property Priority;
      property OnQueryHilit;
    end;

procedure Register;

implementation

{$IFDEF pmClx}
uses QControls, PlusMemoClx;
{$ELSE}
uses Controls, PlusMemo;
{$ENDIF}


const DigitSet  = ['0'..'9'];
      HDigitSet = DigitSet + [ 'a'..'f', 'A'..'F'];

procedure Register;
begin
  RegisterComponents({UCONVERT}'PlusMemo'{/UCONVERT}, [TNumberHighlighter]);
end;

procedure TNumberHighlighter.setCTypeHex(const Value: Boolean);
begin
  if Value<>fCTypeHex then
    begin
      fCTypeHex := Value;
      if ComponentState*[csDesigning, csLoading]=[csDesigning] then ReApplyKeys
    end
end;


procedure TNumberHighlighter.ApplyKeywordsList(Start, Stop: TPlusNavigator; BaseIndex: Integer);
  type keyinfo = record start, stop: Integer end;
  var
    foundcount: Integer;
    foundsome : Boolean;
    nbfound   : array[0..15] of keyinfo;
    smemo     : TPlusMemo;

  procedure applynbfound;
    var drec: DynInfoRec;  k, currentlevel: SmallInt;
    begin
      foundsome:= True;
      fNav1.fPMemo:= smemo;
      fNav2.fPMemo:= smemo;
      fNav1.Assign(Start);

      for k:= 0 to foundcount-1 do
        begin
          fNav1.ParOffset:= nbfound[k].start;
          fNav1.RightOfDyn;
          currentlevel:= DynToLevel(fNav1.pDynAttr^);
          if currentlevel<15 then
            begin
              fNav2.Assign(fNav1);
              fNav2.ParOffset:= fNav2.ParOffset+ (nbfound[k].stop-nbfound[k].start);
              with drec do
                begin
                  KeyIndex:= fNav1.DynAttr.KeyIndex;
                  DynStyle:= Byte(Style) or $c0;
                  if AltFont then DynStyle:= DynStyle or (1 shl Ord(fsAltFont));
                  Backgnd:= Background;
                  Foregnd:= Foreground;
                  Context:= ContextNum;
                  Cursor:= crDefault;
                  StartKLen:= fNav2.Pos-fNav1.Pos;
                  StopKLen:= StartKLen;
                  Level:= CurrentLevel+1;
                  KeyIndex[Level]:= BaseIndex;
                end;

              SetDynStyleP(smemo.IParList, fNav1, fNav2, drec, True, False);
            end
        end;
      fNav1.fPMemo:= nil;
      fNav2.fPMemo:= nil;
      foundcount:= 0;
    end;  { local proc. applynbfound }

var
  runoff, stopoff,
  startword, rundynnb,
  runlim             : Integer;
  ok        : Boolean;
  i         : Integer;
  rundynp   : pDynInfoRec;
  t         : PChar;
  isnb      : Boolean;

begin
  runoff:= Start.ParOffset;
  if runoff>0 then Dec(runoff);
  rundynnb:= Start.DynNb;
  rundynp:= Start.pDynAttr;
  stopoff:= Stop.ParOffset;
  if stopoff<GetParLength(Start.fPar^) then Inc(stopoff);
  t:= Start.fPar^.ParText;
  foundcount:= 0;
  foundsome:= False;
  smemo:= TPlusMemo(Start.fPMemo);

  { reach the start of word }
  while (runoff>0) and (not (pmChar(t[runoff-1]) in Delimiters)) do Dec(runoff);

  if rundynnb>0 then
    while runoff<rundynp^.DynOffset do
      begin
        Dec(rundynnb);
        if rundynnb>0 then rundynp:= @Start.fPar^.ParExtra.DynCodes[rundynnb-1]
                      else
                        begin
                          rundynp:= GetStartDynAttrib(Start.fPar^);
                          Break
                        end
      end;

  if rundynnb<GetDynCount(Start.fPar^) then runlim:= Start.fPar^.ParExtra.DynCodes[rundynnb].DynOffset
                                       else runlim:= High(runlim);
  ok:= OkContext(rundynp^);

  repeat
    { reach start of word }
    while (runoff<stopoff) and (pmChar(t[runoff]) in Delimiters) do Inc(runoff);

    repeat
      while (runoff>=runlim) and (runoff<=stopoff) do
        begin
          rundynp:= @Start.fPar^.ParExtra.DynCodes[rundynnb];
          Inc(rundynnb);
          if rundynnb<GetDynCount(Start.fPar^) then runlim:= Start.fPar^.ParExtra.DynCodes[rundynnb].DynOffset
                                               else runlim:= High(runlim);
          ok:= OkContext(rundynp^)
        end;


      if not ok then
        begin
          while (runoff<stopoff) and (not ok) do
            begin
              runoff:= runlim;
              if runoff<High(runlim) then
                begin
                  rundynp:= @Start.fPar^.ParExtra.DynCodes[rundynnb];
                  Inc(rundynnb);
                  if rundynnb<GetDynCount(Start.fPar^) then runlim:= Start.fPar^.ParExtra.DynCodes[rundynnb].DynOffset
                                                       else runlim:= High(runlim);
                  ok:= OkContext(rundynp^)
                end
            end;

          { reach start of word }
          while (runoff<stopoff) and (not (pmChar(t[runoff]) in Delimiters)) do Inc(runoff);
          while (runoff<stopoff) and (pmChar(t[runoff]) in Delimiters) do Inc(runoff)
        end
    until (ok and (runoff<runlim)) or (runoff>=stopoff);

    if runoff<stopoff then
      begin
      startword:= runoff;
      { reach end of word }
      Inc(runoff);
      while (not (pmChar(t[runoff]) in Delimiters)) and (t[runoff]<>#0) do Inc(runoff);

      if (runoff>startword) and (runoff<=runlim) then
        begin
        isnb:= False;
        if t[startword]='$' then
            begin
            isnb:= True;
            for i:= startword+1 to runoff-1 do
              if not (pmChar(t[i]) in HDigitSet) then
                begin
                  isnb:= False;
                  Break
                end
            end
        else
          if fCTypeHex and (t[startword]='0') and (t[startword+1] ='x') then
            begin
              isnb:= True;
              for i:= startword+2 to runoff-1 do
                if not (pmChar(t[i]) in HDigitSet) then
                  begin
                    isnb:= False;
                    Break
                  end
            end
          else
            if pmChar(t[startword]) in DigitSet then
              begin
              isnb:= True;
              for i:= startword+1 to runoff-1 do
                if not (pmChar(t[i]) in DigitSet) then
                  begin
                    isnb:= False;
                    Break
                  end
              end;

       if isnb then
         begin
           nbfound[foundcount].start:= startword;
           nbfound[foundcount].stop:= runoff;
           Inc(foundcount);
           if foundcount=16 then applynbfound
         end
       end
      end
  until runoff>=stopoff;

  if foundcount>0 then applynbfound;
  if foundsome then InvalidateNavs(smemo.INavigators, Start.fPar^.StartOffset, Start.ParNumber)

end;

constructor TNumberHighlighter.Create(AOwner: TComponent);
begin
  inherited;
  ContextNum:= 3000;
  Foreground:= $0ff
end;

function TNumberHighlighter.OkContext(const dyn: DynInfoRec): Boolean;
var curscope: SmallInt;
begin
  if dyn.DynStyle and $80 = 0 then curscope:= 0
  else
    if dyn.DynStyle and $c0 = $c0 then curscope:= dyn.Context
                                  else curscope:= Low(curscope);
  Result:= (Scope=curscope) or (Priority>0);
  if Assigned(OnQueryHilit) then OnQueryHilit(Self, curscope, Result)
end;

end.
