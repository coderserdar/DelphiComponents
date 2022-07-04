
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit Games;

{$I STD.INC}

interface

uses
  Classes, Graphics, SysUtils, Windows; //, Cards;

{ TOthello class }

type
  TPlayer = (plWhite, plBlack);
  TCellStatus = (csEmpty, csWhite, csBlack);

  TScores = array [TPlayer] of Integer;

  TCellRange = 0..7;
  TCells = array [TCellRange, TCellRange] of TCellStatus;

  TCellChangeEvent = procedure (Sender: TObject; ACol: Integer; ARow: Integer) of object;

  TOthello = class(TObject)
  private
    FAvailableMoves: Integer;
    FCells: TCells;
    FPlayer: TPlayer;
    FScores: TScores;
    FOnCellChange: TCellChangeEvent;
    FOnChange: TNotifyEvent;
    FOnGameOver: TNotifyEvent;
    function GetCells(ACol: Integer; ARow: Integer): TCellStatus;
    procedure SetCells(ACol: Integer; ARow: Integer; Value: TCellStatus);
    function GetScore(APlayer: TPlayer): Integer;
  public
    constructor Create(Source: TOthello); overload;
    constructor Create; overload;
    procedure Reset;
    function Move(ACol: Integer; ARow: Integer; Test: Boolean = False): Boolean;
    property Cells[ACol: Integer; ARow: Integer]: TCellStatus read GetCells write SetCells;
    property AvailableMoves: Integer read FAvailableMoves;
    property Player: TPlayer read FPlayer write FPlayer;
    property Scores[APlayer: TPlayer]: Integer read GetScore;
    property OnCellChange: TCellChangeEvent read FOnCellChange write FOnCellChange;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGameOver: TNotifyEvent read FOnGameOver write FOnGameOver;
  end;

{ TDeck class }

type
  TSuit = (suClub, suDiamond, suHeart, suSpade);
  TRank = (raAce, raTwo, raThree, raFour, raFive, raSix, raSeven, raEight,
    raNine, raTen, raJack, raQueen, raKing);

const
  SuitNames: array[TSuit] of string = ('Diamond', 'Club', 'Heart', 'Spade');
  RankNames: array[TRank] of string = ('Ace', 'Two', 'Three', 'Four', 'Five',
    'Six', 'Seven', 'Eight', 'Nine', 'Ten', 'Jack', 'Queen', 'King');

type
  TCard = record
    Rank: TRank;
    Suit: TSuit;
  end;

function Card(Rank: TRank; Suit: TSuit): TCard;

type
  TCardState = (csFaceDown, csFaceUp, csSelected);

  TCards = array of TCard;

  EDeckError = class(Exception);

  TDeck = class(TObject)
  private
    FCards: TCards;
    FHeight: Integer;
  protected
    function Generate: TCards; dynamic;
  public
    constructor Create;
    function Deal: TCard;
    procedure Shuffle;
    property Height: Integer read FHeight;
  end;

{ Card drawing routines }

procedure DrawCard(DC: HDC; const Card: TCard; X, Y: Integer;
  State: TCardState = csFaceUp; TableColor: TColor = clGreen);

function CardRect: TRect;

implementation

resourcestring
  SDeckEmpty = 'Deck is empty.';

function CheckStatusBounds(Value: Integer): Boolean;
begin
  Result := (Value > Low(TCellRange) - 1) and (Value <  High(TCellRange) + 1);
end;

constructor TOthello.Create(Source: TOthello);
begin
  inherited Create;
  FCells := Source.FCells;
  FPlayer := Source.FPlayer;
  FScores := Source.FScores;
end;

constructor TOthello.Create;
begin
  inherited Create;
  Reset;
end;

procedure TOthello.Reset;
var
  I: Integer;
begin
  FillChar(FCells, SizeOf(TCells), #0);
  FPlayer := plBlack;
  I := High(TCellRange) div 2;
  FCells[I, I] := csWhite;
  FCells[I, I + 1] := csBlack;
  FCells[I + 1, I] := csBlack;
  FCells[I + 1, I + 1] := csWhite;
  FScores[plWhite] := 2;
  FScores[plBlack] := 2;
  FAvailableMoves := 4;
end;

function TOthello.GetCells(ACol: Integer; ARow: Integer): TCellStatus;
begin
  Result := FCells[ACol, ARow];
end;

procedure TOthello.SetCells(ACol: Integer; ARow: Integer; Value: TCellStatus);
begin
  if FCells[ACol, ARow] <> Value then
  begin
    FCells[ACol, ARow] := Value;
    if Assigned(FOnCellChange) then
      FOnCellChange(Self, ACol, ARow);
  end;
end;

function TOthello.GetScore(APlayer: TPlayer): Integer;
begin
  Result := FScores[APlayer];
end;

function TOthello.Move(ACol: Integer; ARow: Integer; Test: Boolean = False): Boolean;
const
  Status: array [TPlayer] of TCellStatus = (csWhite, csBlack);
  ReversePlayer: array [TPlayer] of TPlayer = (plBlack, plWhite);
var
  Flipped: Boolean absolute Result;

  function Flip(ACol: Integer; DeltaCol: Integer; ARow: Integer; DeltaRow: Integer): TCellStatus;
  begin
    if CheckStatusBounds(ACol) and CheckStatusBounds(ARow) then
    begin
      Result := FCells[ACol, ARow];
      if Result = Status[ReversePlayer[FPlayer]] then
      begin
        Result := Flip(ACol + DeltaCol, DeltaCol, ARow + DeltaRow, DeltaRow);
        if Result = Status[FPlayer] then
        begin
          if not Test then
            Cells[ACol, ARow] := Status[FPlayer];
          Flipped := True;
        end;
      end;
    end
    else
      Result := csEmpty;
  end;

var
  ColSearch: Integer;
  RowSearch: Integer;
begin
  Result := False;
  if Cells[ACol, ARow] = csEmpty then
  begin
    for ColSearch := ACol - 1 to ACol + 1 do
      for RowSearch := ARow - 1 to ARow + 1 do
      if (ColSearch <> ACol) or (RowSearch <> ARow) then
        Flip(ColSearch, ColSearch - ACol, RowSearch, RowSearch - ARow);
    if Result and (not Test) then
    begin
      Cells[ACol, ARow] := Status[FPlayer];
      FillChar(FScores, SizeOf(TScores), #0);
      FAvailableMoves := 0;
      for ColSearch := Low(TCellRange) to High(TCellRange) do
        for RowSearch := Low(TCellRange) to High(TCellRange) do
        begin
          if Move(ColSearch, RowSearch, True) then
            Inc(FAvailableMoves);
          case FCells[ColSearch, RowSearch] of
            csWhite: Inc(FScores[plWhite]);
            csBlack: Inc(FScores[plBlack]);
          end;
        end;
      if FAvailableMoves <> 0 then
        FPlayer := ReversePlayer[FPlayer]
      else if Assigned(FOnGameOver) then
        FOnGameOver(Self);
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
  end;
end;

{ TDeck }

function Card(Rank: TRank; Suit: TSuit): TCard;
begin
  Result.Rank := Rank;
  Result.Suit := Suit;
end;

constructor TDeck.Create;
begin
  FCards := Generate;
  Shuffle;
end;

function TDeck.Deal: TCard;
begin
  if FHeight > 0 then
  begin
    Dec(FHeight);
    Result := FCards[FHeight];
  end
  else
    raise EDeckError.Create(SDeckEmpty);
end;

function TDeck.Generate: TCards;
var
  Suit: TSuit;
  Rank: TRank;
  I: Integer;
begin
  I := 0;
  for Suit := Low(TSuit) to High(TSuit) do
    for Rank := Low(Rank) to High(Rank) do
    begin
      SetLength(Result, I + 1);
      Result[I].Suit := Suit;
      Result[I].Rank := Rank;
      Inc(I);
    end;
end;

procedure TDeck.Shuffle;
var
  Card: TCard;
  Offset: Integer;
  I: Integer;
begin
  Randomize;
  for I := Low(FCards) to High(FCards) do
  begin
    Card := FCards[I];
    Offset := Random(High(FCards));
    FCards[I] := FCards[Offset];
    FCards[Offset] := Card;
  end;
  FHeight := Length(FCards);
end;

var
  W, H: Integer;

procedure DrawCard(DC: HDC; const Card: TCard; X, Y: Integer;
  State: TCardState = csFaceUp; TableColor: TColor = clGreen);
{const
  States: array[TCardState] of Integer = (mdFaceDown, mdFaceUp, mdHilite);}
begin
  {if W = 0 then
    cdtInit(W, H);
  cdtDraw(DC, X, Y, Cd(Ord(Card.Rank), Ord(Card.Suit)), States[State],
    ColorToRGB(TableColor));}
end;

function CardRect: TRect;
begin
  {if W = 0 then
    cdtInit(W, H);
  Result := Rect(0, 0, W, H);}
end;

initialization
  W := 0;
  H := 0;
finalization
  {if W <> 0 then cdtTerm;}
end.

