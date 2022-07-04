unit EMailHilit;

{ © Electro-Concept Mauricie, 2004 }
{ Implements TEmailHighlighter object, used for highlighting quotes in emails loaded
  in a TPlusMemo }

{ Note: to install this component on your palette, add this file (EmailHilit.pas) to your package }

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

{$DEFINE EmailHilit}

{UCONVERT}
  {$IFDEF EmailHilitClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}

interface

{$IFDEF pmClx}
uses Classes, PlusMemoClx, PMSupportClx, ExtHilitClx;
{$ELSE}
uses Classes, PlusMemo, PMSupport, ExtHilit;
{$ENDIF}

const
  MaxQuoteLevels = 5;
  DefaultQuoteBaseContext = 4096;

type
  TGetQuoteLevelEvent = procedure(Sender: TObject; Text: PChar; var QuoteLevel, KeyLen: Integer) of object;

  TEmailHighlighter = class(TCustomExtHighlighter)
    private
      fHighlights: array[0..MaxQuoteLevels-1] of THighlightInfo;
      fQuoteChars: string;
      fUnwrapQuotedText: Boolean;
      fOnGetQuoteLevel: TGetQuoteLevelEvent;

      function getQuoteLevelHighlight(I: Integer): THighlightInfo;
      procedure setQuoteLevelHighlight(I: Integer; H: THighlightInfo);
      procedure HighlightChange(Sender: TObject);
      procedure setQuoteChars(const Value: string);
      procedure setUnwrapQuotedText(const Value: Boolean);

    protected
      function FindStart(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean; override;
      function FindStop(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean; override;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

    published
      // Promoted from TCustomExtHighlighter
      property Active;
      property Keywords;
      property Priority;
      property Scope;
      property StartStopKeys;
      property SubHighlighter;

      // Genuine TEmailHighlighter properties
      property ContextNum default DefaultQuoteBaseContext;
      property QuoteChars: string read fQuoteChars write setQuoteChars {default '>|'};
      property QuoteLevel1: THighlightInfo index 0 read getQuoteLevelHighlight write setQuoteLevelHighlight;
      property QuoteLevel2: THighlightInfo index 1 read getQuoteLevelHighlight write setQuoteLevelHighlight;
      property QuoteLevel3: THighlightInfo index 2 read getQuoteLevelHighlight write setQuoteLevelHighlight;
      property QuoteLevel4: THighlightInfo index 3 read getQuoteLevelHighlight write setQuoteLevelHighlight;
      property QuoteLevel5: THighlightInfo index 4 read getQuoteLevelHighlight write setQuoteLevelHighlight;
      property UnwrapQuotedText: Boolean read fUnwrapQuotedText write setUnwrapQuotedText default False;

      property OnGetQuoteLevel: TGetQuoteLevelEvent read fOnGetQuoteLevel write fOnGetQuoteLevel;
    end;

implementation

{$IFDEF pmClx}
uses QGraphics;
{$ELSE}
uses Graphics;
{$ENDIF}

{ TEmailHighlighter }

const DefaultQuoteColors : array[0..MaxQuoteLevels-1] of TColor = (clGreen, clRed, clBlue, clPurple, clNavy);

constructor TEmailHighlighter.Create(AOwner: TComponent);
var i: Integer;
begin
  inherited;
  ContextNum:= DefaultQuoteBaseContext;
  fQuoteChars:= '>|';
  for i:= 0 to MaxQuoteLevels-1 do
    begin
      fHighlights[i]:= THighlightInfo.Create;
      fHighlights[i].Foreground:= DefaultQuoteColors[i];
      fHighlights[i].Background:= clNone;
      fHighlights[i].OnChange:= HighlightChange
    end;

  (*// A dummy start-stop key used only for FindStop
  StartStopKeys.AddExStartStopKey(#255, { an arbitrary character not found in normal text }
                                  '', [woMatchCase {to avoid taking uppercase text content {woStartPar}],
                                  [], ContextNum, 0, 0, 0, clNone, clNone, [ssoParStop])*)
end;

destructor TEmailHighlighter.Destroy;
var i: Integer;
begin
  for i:= 0 to MaxQuoteLevels-1 do fHighlights[i].Free;
  inherited;
end;

function TEmailHighlighter.FindStart(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean;
var
  i, sqlevel: Integer;
  st: PChar;
  dinfo: DynInfoRec;
  pdyn: pDynInforec;
  shilit: THighlightInfo;
  sdyns : TDynInfoArray;

begin
  Result:= False;
  Start.RightOfDyn;
  if Active and (Start.ParOffset=0) and ((Scope=Start.Context) or (Priority>Start.Context)) then
    begin
      // Find quoting level (sqlevel)
      st:= Start.fPar.ParText;
      i:= 0;
      sqlevel:= -1;
      if st<>nil then
        begin
          while Pos(st^, fQuoteChars)>0 do
            begin
              Inc(sqlevel);
              Inc(i);
              Inc(st);
              if st^=' ' then
                begin
                  Inc(st);
                  Inc(i)
                end
            end
        end;

      if Assigned(fOnGetQuoteLevel) then fOnGetQuoteLevel(Self, Start.fPar.ParText, sqlevel, i);
      
      // Apply dyn style
      if sqlevel>=0 then
        begin
          Result:= True;
          if sqlevel>=MaxQuoteLevels then sqlevel:= MaxQuoteLevels-1;
          shilit:= fHighlights[sqlevel];
          pdyn:= Start.pDynAttr;
          dinfo.Level:= DynToLevel(pdyn^)+1;
          dinfo.CollpsLevel:= DynToCollapseLevel(pdyn^);
          Stop.Assign(Start);
          if dinfo.Level>0 then
            with Stop do
              begin
                if BackToDyn(0) then Par^.ParExtra.DynCodes[DynNb].StopKLen:= 0;
                Assign(Start)
              end;

          // Remove stray dyn codes
          while Stop.ForwardToDyn(Start.Pos+i) do Stop.RemoveDyn;

          with dinfo do
            begin
              DynOffset:= 0;
              KeyIndex:= Start.DynAttr.KeyIndex;
              DynStyle:= Byte(shilit.Style) or $c0;
              KeyIndex[Level]:= -32768 + BaseIndex + sqlevel;
              CollpsState:= [];
              Cursor:= shilit.Cursor;
              Backgnd:= shilit.Background;
              Foregnd:= shilit.Foreground;
              Context:= ContextNum + sqlevel;
              StartKlen:= i;
              StopKLen:= 0
            end;
          sdyns:= GetDynArray(Start.fPar^);
          with Start do
            if (fDynNb<Length(sdyns)) and (sdyns[fDynNb].DynOffset=0) then sdyns[fDynNb]:= dinfo
                                                                      else AddDyn(dinfo);
          Stop.Pos:= Start.Pos + i;
        end // found start key
    end;

  if UnwrapQuotedText then
    if Result then
      for i:= Start.fParNb to Stop.ParNumber do
        //TPlusMemo(Start.fPMemo).ParagraphsWrapable[i]:= False   can't do that inside of FindStart
        Include(pParInfo(TPlusMemo(Start.fPMemo).IParList.Pointers[i])^.ParState, pmpNoWrap)
    else
      if Start.ParOffset=0 then
          for i:= Start.fParNb to Stop.ParNumber do
            //TPlusMemo(Start.fPMemo).ParagraphsWrapable[i]:= True   can't do that inside of FindStart
            Exclude(pParInfo(TPlusMemo(Start.fPMemo).IParList.Pointers[i])^.ParState, pmpNoWrap);

  Result:= Result or inherited FindStart(Start, Stop, BaseIndex+MaxQuoteLevels);  // to have sub highlighter working
end;

function TEmailHighlighter.FindStop(Start, Stop: TPlusNavigator; BaseIndex: Integer): Boolean;
var
  spstartdyn: pDynInfoRec;
  dinfo: DynInfoRec;
  slevel, skeyindex, sssindex, sdnb: Integer;
begin
  Result:= False;
  spstartdyn:= Start.pDynAttr;
  slevel:= DynToLevel(spstartdyn^);
  if slevel<0 then Exit;
  skeyindex:= spstartdyn^.KeyIndex[slevel];
  sssindex:= (skeyindex and $7fff) - BaseIndex;

  if (not Active) or (skeyindex>0) or (sssindex<0) or (sssindex>=MaxQuoteLevels) then
    begin
      Result:= inherited FindStop(Start, Stop, BaseIndex+MaxQuoteLevels);
      Exit
    end;

  Result:= Stop.ParOffset=GetParLength(Start.fPar^);
  if Result then
    begin
      sdnb:= Start.fDynNb-1;
      if sdnb<1 then dinfo:= GetStartDynAttrib(Start.fPar^)^
                else dinfo:= Start.fPar.ParExtra.DynCodes[sdnb-1];
      dinfo.StartKLen:= 0;
      Stop.AddDyn(dinfo);
    end;
end;

function TEmailHighlighter.getQuoteLevelHighlight(I: Integer): THighlightInfo;
begin
  Result:= fHighlights[I]
end;

procedure TEmailHighlighter.HighlightChange(Sender: TObject);
begin
  if csDesigning in ComponentState then ReApplyKeys
end;

procedure TEmailHighlighter.setQuoteChars(const Value: string);
begin
  if Value<>fQuoteChars then
    begin
      fQuoteChars := Value;
      if csDesigning in ComponentState then ReApplyKeys
    end
end;

procedure TEmailHighlighter.setQuoteLevelHighlight(I: Integer; H: THighlightInfo);
begin
  fHighlights[I].Assign(H);
end;

procedure TEmailHighlighter.setUnwrapQuotedText(const Value: Boolean);
begin
  if Value<>fUnwrapQuotedText then
    begin
      fUnwrapQuotedText := Value;
      if csDesigning in ComponentState then ReApplyKeys
    end
end;

end.
