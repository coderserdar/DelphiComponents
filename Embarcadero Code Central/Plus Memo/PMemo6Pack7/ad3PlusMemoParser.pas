unit ad3PlusMemoParser;

{ Copyright (c) Electro-Concept Mauricie, 2001-2002
  Sept. 22, 2001 release

  A TPlusMemoControlParser object to enable Addict3 Spell checker to work with a TPlusMemo;

  Place a TAddictSpell3 on your form (or create one at run time), and when you
  want your TPlusMemo to be spell checked, simply do

    Parser := TPlusMemoControlParser.Create;
    Parser.Initialize(PlusMemo1);
    AddictSpell31.CheckParser(Parser, ctxxx);

  Please note that PlusMemo1.HideSelection will be set to False inside of Parser.Initialize.

  TPlusMemoControlParser can also be used with a TThesaurus3 component, in the following way:
    Parser:= TPlusMemoControlParser.Create;
    Parser.Initialize(PlusMemo1);
    Thesaurus31.LookupParser(Parser)

  This object works with Addict version 3.  You must have Addict3 installed on your system for this
  parser to work.  }

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

interface

uses
    Windows, ad3ParserBase, PlusMemo, PMSupport;

type

    TPlusMemoControlParser = class(TControlParser)
    protected
        fMemo : TPlusMemo;
        fNav  : TPlusNavigator;
        fNavEnd: TPlusNavigator;
    public
        {UCONVERT}
        procedure Initialize(EditControl:Pointer); override;
        function GetChar : Char; override;
        function GetLine : string; override;
        function MoveNext: Boolean; override;
        function MovePrevious: Boolean; override;
        procedure SetPosition(XPos: LongInt; YPos: LongInt; PosType: TPositionType); override;
        procedure GetPosition(var XPos: LongInt; var YPos: LongInt); override;
        procedure SelectWord(Length: LongInt); override;
        procedure ReplaceWord(Replacement: string; State: LongInt); override;
        procedure IgnoreWord(State: LongInt); override;
        procedure CenterSelection; override;
        procedure GetCursorPosition(var XPos: LongInt; var YPos: LongInt); override;
        procedure GetSelectionPosition(var XPosStart: LongInt; var YPosStart: LongInt;
                                       var XPosEnd: LongInt; var YPosEnd: LongInt); override;
        procedure GetControlScreenPosition(var ControlPosition: TRect); override;
        procedure GetSelectionScreenPosition(var SelectionPosition: TRect); override;
        procedure UndoLast(State: LongInt; UndoAction:LongInt; var UndoData: LongInt); override;
        {/UCONVERT}
    end;

implementation

uses Classes;

{ Note: we present the content of the PlusMemo to the Parsing engine as one long line
        rather than multiple lines for best efficiency working with TPlusNavigator }

procedure TPlusMemoControlParser.Initialize(EditControl:Pointer);
begin
  fMemo:= EditControl;
  fNav:= TPlusNavigator.Create(fMemo);
  fMemo.HideSelection:= False;
  fNavEnd:= nil;
end;

function TPlusMemoControlParser.GetChar: AnsiChar;
begin
  if fNav.Pos<fMemo.CharCount then Result:= fNav.AnsiText
                              else Result:= #0
end;

function TPlusMemoControlParser.GetLine: AnsiString;
begin
  Result:= fMemo.Lines[fNav.TrueLineNumber]
end;

function TPlusMemoControlParser.MoveNext:Boolean;
begin
  fNav.Pos:= fNav.Pos + 1;
  if Assigned(fNavEnd) then Result:= fNav.Pos<=fNavEnd.Pos
                       else Result:= fNav.Pos<=fMemo.CharCount;
end;

function TPlusMemoControlParser.MovePrevious:Boolean;
begin
  Result:= fNav.Pos>0;
  if Result then fNav.Pos:= fNav.Pos - 1
end;

procedure TPlusMemoControlParser.SetPosition(XPos:LongInt; YPos:LongInt; PosType:TPositionType);
begin
  case PosType of
    ptCurrent:
      begin
        fNav.Pos:= XPos;
        if YPos>0 then fNav.Pos:= fMemo.CharCount
      end;
    ptEnd:
      begin
        if fNavEnd=nil then fNavEnd:= TPlusNavigator.Create(fMemo);
        fNavEnd.Pos:= XPos;
        if YPos>0 then fNavEnd.Pos:= fMemo.CharCount
      end
    end
end;

procedure TPlusMemoControlParser.GetPosition(var XPos: LongInt; var YPos: LongInt);
begin
  XPos:= fnav.Pos;
  YPos:= 0;
end;

procedure TPlusMemoControlParser.SelectWord(Length:LongInt);
begin
  fNav.Pos:= fNav.Pos - Length;
  fMemo.SelStart:= fNav.Pos;
  fMemo.SelLength:= Length
end;

procedure TPlusMemoControlParser.ReplaceWord(Replacement: AnsiString; State:LongInt);
begin
  fMemo.SelText:= Replacement;
  fNav.Assign(fMemo.SelStartNav)
end;

procedure TPlusMemoControlParser.IgnoreWord(State:LongInt);
begin
  fNav.Assign(fMemo.SelStopNav)
end;

procedure TPlusMemoControlParser.CenterSelection;
var newtop, newleft, sstart, sstop: Integer;
begin
  newtop:= fMemo.SelLine*fMemo.LineHeight - fMemo.ClientHeight div 2;
  if newtop<0 then newtop:= 0;
  if fMemo.WordWrap then newleft:= 0
  else
    begin     // try to make sure the selection is visible horizontally
      newleft:= fMemo.LeftOrigin;
      sstop:= fMemo.SelStopNav.DisplayPos.X;
      if sstop>fMemo.ClientWidth then newleft:= newleft + (sstop-fMemo.ClientWidth);
      sstart:= fMemo.SelStartNav.DisplayPos.X;
      if sstart<0 then newleft:= fMemo.LeftOrigin + sstart;
    end;

  fMemo.SetTopLeft(newtop, newleft, fMemo.ScrollTime)
end;

procedure TPlusMemoControlParser.GetCursorPosition(var XPos: LongInt; var YPos: LongInt);
begin
  XPos:= fMemo.SelStart;
  YPos:= 0
end;

procedure TPlusMemoControlParser.GetSelectionPosition(var XPosStart:LongInt; var YPosStart:LongInt;
                                                      var XPosEnd:LongInt; var YPosEnd:LongInt);
begin
  XPosStart:= fMemo.SelStartNav.Pos;
  XPosEnd:= fMemo.SelStopNav.Pos;
  YPosStart:= 0;
  YPosEnd:= 0
end;

procedure TPlusMemoControlParser.GetControlScreenPosition(var ControlPosition:TRect);
var P: TPoint;
begin
  P := fMemo.ClientToScreen(Point(0, 0));
  ControlPosition := Rect( P.X, P.Y, P.X + fMemo.Width, P.Y + fMemo.Height );
end;

procedure TPlusMemoControlParser.GetSelectionScreenPosition( var SelectionPosition:TRect );
begin
  SelectionPosition.TopLeft:= fMemo.ClientToScreen(fMemo.SelStartNav.DisplayPos);
  Dec(SelectionPosition.Top, fMemo.LineBase);
  SelectionPosition.BottomRight:= fMemo.ClientToScreen(fMemo.SelStopNav.DisplayPos);
  Inc(SelectionPosition.Bottom, fMemo.LineHeightRT-fMemo.LineBase)
end;

procedure TPlusMemoControlParser.UndoLast( State:LongInt; UndoAction:LongInt; var UndoData:LongInt );
begin
end;

end.

