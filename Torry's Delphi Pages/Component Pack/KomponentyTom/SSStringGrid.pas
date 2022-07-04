unit SSStringGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, Hex;

type

  TGetColorSeriiEvent = procedure(Sender: TObject; NumerSerii: Byte; var ColorFont, ColorBack: TColor) of object;

  TSSStringGrid = class(TStringGrid)
  private
    FFixedColorFont: TColor;
    FSzybFixedColorFont: TColor;
    FPomFixedColorFont: TColor;
    FSzybFixedColor: TColor;
    FSzybColorFont: TColor;
    FPomColorFont: TColor;
    FSzybColor: TColor;
    FPomFixedColor: TColor;
    FPomColor: TColor;
    FSpecColor: TColor;
    FSpecColorFont: TColor;
    FSpecFixedColorFont: TColor;
    FSpecFixedColor: TColor;
    FOnGetColorSerii: TGetColorSeriiEvent;
    FOnGetFixColorSerii: TGetColorSeriiEvent;
    procedure SetFixedColorFont(const Value: TColor);
    procedure SetSzybFixedColorFont(const Value: TColor);
    procedure SetPomFixedColorFont(const Value: TColor);
    procedure SetSzybFixedColor(const Value: TColor);
    procedure SetPomColorFont(const Value: TColor);
    procedure SetSzybColor(const Value: TColor);
    procedure SetSzybColorFont(const Value: TColor);
    procedure SetPomFixedColor(const Value: TColor);
    procedure SetPomColor(const Value: TColor);
    procedure SetSpecColor(const Value: TColor);
    procedure SetSpecColorFont(const Value: TColor);
    procedure SetSpecFixedColor(const Value: TColor);
    procedure SetSpecFixedColorFont(const Value: TColor);
  public
    procedure InsertRowValue(const Index: Integer;
      const A: array of string);
    procedure DeleteRow(ARow: Integer); override;
    procedure AddRow(const A: array of string);
    procedure InsertRow(Index: Integer; const A: array of string);
    procedure AddEmptyRow;
    procedure InsertEmptyRow(Index: Integer);
    procedure ClearRow(Index: Integer);
    procedure ClearAll;
    procedure DeleteAllRow;
  published
    property FixedColorFont: TColor read FFixedColorFont write SetFixedColorFont;
    property SzybFixedColorFont: TColor read FSzybFixedColorFont write SetSzybFixedColorFont;
    property PomFixedColorFont: TColor read FPomFixedColorFont write SetPomFixedColorFont;

    property SzybFixedColor: TColor read FSzybFixedColor write SetSzybFixedColor;
    property PomFixedColor: TColor read FPomFixedColor write SetPomFixedColor;

    property SzybColorFont: TColor read FSzybColorFont write SetSzybColorFont;
    property PomColorFont: TColor read FPomColorFont write SetPomColorFont;

    property SzybColor: TColor read FSzybColor write SetSzybColor;
    property PomColor: TColor read FPomColor write SetPomColor;

    property SpecFixedColorFont: TColor read FSpecFixedColorFont write SetSpecFixedColorFont;
    property SpecFixedColor: TColor read FSpecFixedColor write SetSpecFixedColor;

    property SpecColorFont: TColor read FSpecColorFont write SetSpecColorFont;
    property SpecColor: TColor read FSpecColor write SetSpecColor;
    property OnGetColorSerii: TGetColorSeriiEvent read FOnGetColorSerii write FOnGetColorSerii;
    property OnGetFixColorSerii: TGetColorSeriiEvent read FOnGetFixColorSerii write FOnGetFixColorSerii;
  end;

  TBitZmiany = record
    Bit: TBit;
    Czas: TDateTime;
    Akcept: Boolean;
  end;

  PBit = ^TBitZmiany;

  TZmiana = class
  private
    FStringGrid: TStringGrid;
//    FLastZmiana: Integer;
    FZmiana: Boolean;
    FHistorja: TList;
    FNumer: Integer;
    BitZmianyTemp: PBit;
    procedure AddZmiane(ABit: TBit; ACzas: TDateTime);
    function GetBitZmiany(Index: Integer): TBitZmiany;
    function GetLastZmiana: Integer;
  protected
    property LastZmiana: Integer read GetLastZmiana;
    procedure FreeZmiany;
  public
    constructor Create(AStringGrid: TStringGrid; ANumer: Integer); virtual;
    destructor Destroy; override;
    procedure SetZmiana(ACol, ARow: Integer);
    procedure Reset;
    procedure ClearHis;
    property Zmiana: Boolean read FZmiana;
    property Historja: TList read FHistorja write FHistorja;
    property Zmiany[Index: Integer]: TBitZmiany read GetBitZmiany;
    property Numer: Integer read FNumer;
  end;

  TSSHexBinGrid = class(TSSStringGrid)
  protected
    procedure NewDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    function SelectCell(ACol, ARow: Longint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteRow(ARow: Integer); override;
    procedure InsertSS(Numer: Byte; NumerHex, SeriaHex: string;
       SeriaBin: TWordRek);
    procedure ModyfiSS(Index, NumerSerii: Integer);
    procedure Reset(ACol, ARow: Integer);
    procedure ResetRow(Index: Integer);
    procedure ResetAll;
    procedure ClearHis(ACol, ARow: Integer);
    procedure ClearHisRow(Index: Integer);
    procedure ClearHisAll(AFree: Boolean);
    procedure FreeRow(Index: Integer);
  end;

  TSSEditGrid = class(TSSStringGrid)
  protected
    procedure NewDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InsertSS(Numer: Byte; Seria: Word; Telegram: string);
  end;

implementation

const MaxZmian = 16;

{ TSSStringGrid }

procedure TSSStringGrid.AddEmptyRow;
begin
  AddRow([]);
end;

procedure TSSStringGrid.InsertEmptyRow(Index: Integer);
begin
  InsertRow(Index, []);
end;

procedure TSSStringGrid.AddRow(const A: array of string);
begin
  RowCount:= RowCount + 1;
  InsertRowValue(RowCount - 1, A);
end;

procedure TSSStringGrid.ClearAll;
var i: Integer;
begin
  for i:= 0 to RowCount - 1 do ClearRow(i);
end;

procedure TSSStringGrid.ClearRow(Index: Integer);
var i: Integer;
begin
  for i:= 0 to ColCount - 1 do Cells[i, Index]:= '';
end;

procedure TSSStringGrid.DeleteAllRow;
begin
  ClearAll;
  if RowCount > 0 then RowCount:= 0;
end;

procedure TSSStringGrid.DeleteRow(ARow: Integer);
begin
  ClearRow(ARow);
  inherited;
end;

procedure TSSStringGrid.InsertRow(Index: Integer;
  const A: array of string);
var i: Integer;
begin
  if Index > RowCount - 1 then Index:= RowCount;
  RowCount:= RowCount + 1;
  for i:=RowCount - 2 downto Index do
    MoveRow(i, i + 1);
  ClearRow(Index);
  InsertRowValue(Index, A);
end;

procedure TSSStringGrid.InsertRowValue(const Index: Integer;
  const A: array of string);
var i, C: Integer;
    S   : string;
begin
  C:= High(A);
  if C > -1 then
    for i:=0 to C do
      if i <= ColCount - 1 then
      begin
        S:= A[i];
        if S <> '' then
          if S = '_'
            then Cells[i, Index]:= ''     // zerowanie znakiem '_'
            else Cells[i, Index]:= A[i];
      end;
end;

procedure TSSStringGrid.SetFixedColorFont(const Value: TColor);
begin
  if FFixedColorFont <> Value then
  begin
    FFixedColorFont := Value;
    Invalidate;
  end;
end;

procedure TSSStringGrid.SetSzybFixedColorFont(const Value: TColor);
begin
  if FSzybFixedColorFont <> Value then
  begin
    FSzybFixedColorFont := Value;
    Invalidate;
  end;
end;

procedure TSSStringGrid.SetPomFixedColorFont(const Value: TColor);
begin
  if FPomFixedColorFont <> Value then
  begin
    FPomFixedColorFont := Value;
    Invalidate;
  end;
end;

procedure TSSStringGrid.SetSzybFixedColor(const Value: TColor);
begin
  if FSzybFixedColor <> Value then
  begin
    FSzybFixedColor := Value;
    Invalidate;
  end;
end;

procedure TSSStringGrid.SetPomColorFont(const Value: TColor);
begin
  if FPomColorFont <> Value then
  begin
    FPomColorFont := Value;
    Invalidate;
  end;
end;

procedure TSSStringGrid.SetSzybColor(const Value: TColor);
begin
  if FSzybColor <> Value then
  begin
    FSzybColor := Value;
    Invalidate;
  end;
end;

procedure TSSStringGrid.SetSzybColorFont(const Value: TColor);
begin
  if FSzybColorFont <> Value then
  begin
    FSzybColorFont := Value;
    Invalidate;
  end;
end;

procedure TSSStringGrid.SetPomFixedColor(const Value: TColor);
begin
  if FPomFixedColor <> Value then
  begin
    FPomFixedColor := Value;
    Invalidate;
  end;
end;

procedure TSSStringGrid.SetPomColor(const Value: TColor);
begin
  if FPomColor <> Value then
  begin
    FPomColor := Value;
    Invalidate;
  end;
end;

procedure TSSStringGrid.SetSpecColor(const Value: TColor);
begin
  if FSpecColor <> Value then
  begin
    FSpecColor := Value;
    Invalidate;
  end;
end;

procedure TSSStringGrid.SetSpecColorFont(const Value: TColor);
begin
  if FSpecColorFont <> Value then
  begin
    FSpecColorFont := Value;
    Invalidate;
  end;
end;

procedure TSSStringGrid.SetSpecFixedColor(const Value: TColor);
begin
  if FSpecFixedColor <> Value then
  begin
    FSpecFixedColor := Value;
    Invalidate;
  end;
end;

procedure TSSStringGrid.SetSpecFixedColorFont(const Value: TColor);
begin
  if FSpecFixedColorFont <> Value then
  begin
    FSpecFixedColorFont := Value;
    Invalidate;
  end;
end;

{ TZmiana }

procedure TZmiana.AddZmiane(ABit: TBit; ACzas: TDateTime);
begin
  with FHistorja do
  begin
    if Count >= MaxZmian then Delete(0);
    New(BitZmianyTemp);
    BitZmianyTemp.Bit:= ABit;
    BitZmianyTemp.Czas:= ACzas;
    Add(BitZmianyTemp);
  end;  
end;

procedure TZmiana.ClearHis;
begin
  FreeZmiany;
  FHistorja.Clear;
  FZmiana:= False;
end;

constructor TZmiana.Create(AStringGrid: TStringGrid; ANumer: Integer);
begin
  FNumer:= ANumer;
  FStringGrid:= AStringGrid;
//  FLastZmiana:= -1;
  FZmiana:= False;
  FHistorja:= TList.Create;
  FHistorja.Capacity:= MaxZmian;
end;

destructor TZmiana.Destroy;
begin
  FreeZmiany;
  inherited Destroy;
end;

procedure TZmiana.FreeZmiany;
var i: Integer;
begin
  for i:= 0 to FHistorja.Count -1 do
    Dispose(FHistorja.Items[i]);
end;

function TZmiana.GetBitZmiany(Index: Integer): TBitZmiany;
begin
  if (Index >= 0) and (Index <= FHistorja.Count - 1) then
  begin
    with FHistorja do
    begin
      Result.Bit:= PBit(Items[Index]).Bit;
      Result.Czas:= PBit(Items[Index]).Czas;
    end;
  end;
end;

function TZmiana.GetLastZmiana: Integer;
begin
  if FHistorja.Count > 0
    then Result:= PBit(FHistorja.List[FHistorja.Count - 1]).Bit
    else Result:= -1;
end;

procedure TZmiana.Reset;
begin
  FZmiana:= False;
end;

procedure TZmiana.SetZmiana(ACol, ARow: Integer);
var S: string;
    B, lz: Integer;
    R: TRect;

begin
  S:= FStringGrid.Cells[ACol, ARow];
  if S <> '' then
  begin
    B:= StrToInt(S);
    lz:= GetLastZmiana;
    if lz <> b then
    begin
      AddZmiane(B, Time);
      if lz <> -1 then
      begin
        FZmiana:= True;
        R:= FStringGrid.CellRect(ACol, ARow);
        InvalidateRect(FStringGrid.Canvas.Handle, @R, False);
      end;
    end;
  end;
end;


{ TSSHexBinGrid }

procedure TSSHexBinGrid.ClearHis(ACol, ARow: Integer);
begin
  TZmiana(Objects[ACol, ARow]).ClearHis;
end;

procedure TSSHexBinGrid.ClearHisAll(AFree: Boolean);
var i, ii: Integer;
begin
  for i:= 1 to RowCount - 1 do
    for ii:= 3 to 18 do
    begin
      if Objects[ii, i] <> nil then
      begin
        TZmiana(Objects[ii, i]).ClearHis;
        if AFree and Assigned(Objects[ii, i]) then
        begin
          Objects[ii, i].Free;
          Objects[ii, i]:= nil;
        end;
      end;
    end;
end;

procedure TSSHexBinGrid.ClearHisRow(Index: Integer);
var i: Integer;
begin
  for i:= 3 to 18 do TZmiana(Objects[i, Index]).ClearHis;
end;

constructor TSSHexBinGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFixedColorFont:= clBlue;
  FSzybFixedColorFont:= clBlue;
  FPomFixedColorFont:= clBlue;
  FSzybFixedColor:= clBackground;
  FPomFixedColor:= clBtnShadow;
  FSzybColor:= clWindow;
  FPomColor:= clWindow;
  FSzybColorFont:= clYellow;
  FPomColorFont:= clGreen;
  FSpecColor:= clScrollBar;
  FSpecColorFont:= clRed;
  FSpecFixedColorFont:= clBlue;
  FSpecFixedColor:= clHotLight;

  OnDrawCell:= NewDrawCell;
  Width:= 350;
  Height:= 200;
  RowCount:= 1;
  ColCount:= 19;
  FixedRows:= 0;
  FixedCols:= 0;
  DefaultColWidth:= 12;
  DefaultRowHeight:= 13;
  ColWidths[0]:= 25;
  ColWidths[1]:= 25;
  ColWidths[2]:= 50;
  Cells[0, 0]:= 'SD';
  Cells[1, 0]:= 'SH';
  Cells[2, 0]:= 'HEX';
  Cells[3, 0]:= '7';
  Cells[4, 0]:= '6';
  Cells[5, 0]:= '5';
  Cells[6, 0]:= '4';
  Cells[7, 0]:= '3';
  Cells[8, 0]:= '2';
  Cells[9, 0]:= '1';
  Cells[10, 0]:= '0';
  Cells[11, 0]:= '7';
  Cells[12, 0]:= '6';
  Cells[13, 0]:= '5';
  Cells[14, 0]:= '4';
  Cells[15, 0]:= '3';
  Cells[16, 0]:= '2';
  Cells[17, 0]:= '1';
  Cells[18, 0]:= '0';
end;

procedure TSSHexBinGrid.DeleteRow(ARow: Integer);
begin
  FreeRow(ARow);
  inherited;
end;

destructor TSSHexBinGrid.Destroy;
begin

  ClearHisAll(True);
  inherited Destroy;
end;

procedure TSSHexBinGrid.FreeRow(Index: Integer);
var i: Integer;
begin
  for i:= 0 to ColCount - 1 do
    if Assigned(Objects[i, Index]) then
    begin
      Objects[i, Index].Free;
      Objects[i, Index]:= nil;
    end;
end;

procedure TSSHexBinGrid.InsertSS(Numer: Byte; NumerHex, SeriaHex: string;
  SeriaBin: TWordRek);
var i,NN   : integer;
    Jest   : Boolean;

  function ZnajdzSerie(N: Byte): Integer;
  var ii: Integer;
  begin
    Result:= -1;
    for ii:=1 to RowCount - 1 do
      if StrToInt(Cells[0, ii]) = N then
      begin
        Result:= ii;
        Break;
      end;
  end;

begin
  NN:= ZnajdzSerie(Numer);
  if NN = -1 then
  begin
    Jest:= False;
    for i:= Numer - 1 downto 1 do
    begin
      NN:= ZnajdzSerie(i);
      if NN <> - 1 then
      begin
        NN:= NN + 1;
        Jest:= True;
        Break;
      end;
    end;
    if not Jest then
    begin
      for i:= Numer + 1 to 255 do
      begin
        NN:= ZnajdzSerie(i);
        if NN <> - 1 then
        begin
          Jest:= True;
          Break;
        end;
      end;
      if not Jest then NN:= 2; // niema jeszcze zadnej serrii
    end;
    InsertRow(NN, [ByteToDec(Numer), NumerHex, SeriaHex,
        IntToStr(SeriaBin.B15),
        IntToStr(SeriaBin.B14),
        IntToStr(SeriaBin.B13),
        IntToStr(SeriaBin.B12),
        IntToStr(SeriaBin.B11),
        IntToStr(SeriaBin.B10),
        IntToStr(SeriaBin.B9),
        IntToStr(SeriaBin.B8),
        IntToStr(SeriaBin.B7),
        IntToStr(SeriaBin.B6),
        IntToStr(SeriaBin.B5),
        IntToStr(SeriaBin.B4),
        IntToStr(SeriaBin.B3),
        IntToStr(SeriaBin.B2),
        IntToStr(SeriaBin.B1),
        IntToStr(SeriaBin.B0)]);
  end else
  begin
    InsertRowValue(NN, [ByteToDec(Numer), NumerHex, SeriaHex,
        IntToStr(SeriaBin.B15),
        IntToStr(SeriaBin.B14),
        IntToStr(SeriaBin.B13),
        IntToStr(SeriaBin.B12),
        IntToStr(SeriaBin.B11),
        IntToStr(SeriaBin.B10),
        IntToStr(SeriaBin.B9),
        IntToStr(SeriaBin.B8),
        IntToStr(SeriaBin.B7),
        IntToStr(SeriaBin.B6),
        IntToStr(SeriaBin.B5),
        IntToStr(SeriaBin.B4),
        IntToStr(SeriaBin.B3),
        IntToStr(SeriaBin.B2),
        IntToStr(SeriaBin.B1),
        IntToStr(SeriaBin.B0)]);
    ModyfiSS(NN, Numer);
  end;
end;

procedure TSSHexBinGrid.ModyfiSS(Index, NumerSerii: Integer);
var i: Integer;
    P: TObject;
begin
//if StrToInt(Cells[0, Index]) <> NumerSerii then Exit;


  for i:= 3 to 18 do
  begin
    P:= Objects[i, Index];
    if Assigned(P) then
    begin
      TZmiana(P).SetZmiana(i, Index);
    {  Invalidate;  } InvalidateEditor;
    end else
    begin
      P:= TZmiana.Create(Self, NumerSerii);
      Objects[i, Index]:= P;
      TZmiana(P).SetZmiana(i, Index);
    end;
  end;
end;

procedure TSSHexBinGrid.NewDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var P : Pointer;
    CB, CF: TColor;
    ns: Byte;
    temp_bool: Boolean;

    procedure SetColor(const ACB, ACF: TColor);
    begin
      CB:= ACB;
      CF:= ACF;
    end;

begin
  CB:= Color;
  CF:= Font.Color;
  if ARow = 0 then
  begin
    SetColor(FixedColor, FFixedColorFont);
  end else
  if (ACol = 0) or (ACol = 1) then
  begin
    ns:= StrToInt(Cells[0, ARow]);
    temp_bool:= False;
    if Assigned(FOnGetFixColorSerii) then
    begin
      FOnGetFixColorSerii(Self, ns, CF, CB);
      if (CF = 0) and (CB = 0) then
      begin
        SetColor(FixedColor, FFixedColorFont);
        temp_bool:= False;
      end else
        temp_bool:= True;
    end;
    if not temp_bool then
    case ns of
      $40: SetColor(FSpecFixedColor, FSpecFixedColorFont); // statusowa
      $41..$7F: SetColor(FSzybFixedColor, FSzybFixedColorFont); // szybkie
      $80.. $8F: SetColor(FSpecFixedColor, FSpecFixedColorFont); // specjalne
      $C0..$FF: SetColor(FPomFixedColor, FPomFixedColorFont); //pomiarowe
      else
      SetColor(FixedColor, FFixedColorFont);
    end;
  end else
  begin
    ns:= StrToInt(Cells[0, ARow]);

//    if ns = 1 then

    temp_bool:= False;
    if Assigned(FOnGetColorSerii) then
    begin
      FOnGetColorSerii(Self, ns, CF, CB);
      if (CF = 0) and (CB = 0) then
      begin
        CB:= Color;
        CF:= Font.Color;
        temp_bool:= False;
      end else
        temp_bool:= True;
    end;
    if not temp_bool then
    case ns of
      $40: SetColor(FSpecColor, FSpecColorFont); // statusowa
      $41..$7F: SetColor(FSzybColor, FSzybColorFont); // szybkie
      $80.. $8F: SetColor(FSpecColor, FSpecColorFont); // specjalne
      $C0..$FF:  SetColor(FPomColor, FPomColorFont); //pomiarowe
    end;
  end;
  if (ARow > 0) and (ACol > 1) then
  begin
    P:= Objects[ACol, ARow];
    if (P <> nil) and TZmiana(P).Zmiana then
    begin
      CB:= clBlue;
      CF:= clWindow;
    end
  end;
  Canvas.Brush.Color:= CB;
  Canvas.FillRect(Rect);
  Canvas.Font.Color:= CF;

  DrawText(Canvas.Handle, PChar(Cells[ACol, ARow]),
    -1, Rect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
end;


procedure TSSHexBinGrid.Reset(ACol, ARow: Integer);
begin
  TZmiana(Objects[ACol, ARow]).Reset;
  Invalidate;
end;

procedure TSSHexBinGrid.ResetAll;
var i, ii: Integer;
    P : Pointer;
begin
  for i:= 1 to RowCount - 1 do
    for ii:= 3 to 18 do
    begin
      P:= Objects[ii, i];
      if P <> nil then  TZmiana(P).Reset;
    end;
  Invalidate;
end;

procedure TSSHexBinGrid.ResetRow(Index: Integer);
var i: Integer;
begin
  for i:= 3 to 18 do TZmiana(Objects[i, Index]).Reset;
  Invalidate;
end;

function TSSHexBinGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  if (ARow = 0) or ((ACol >= 0) and (ACol <= 2))
    then Result:= False
    else Result:= inherited SelectCell(ACol, ARow);
end;

{ TSSEditGrid }

constructor TSSEditGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFixedColorFont:= clRed;
  OnDrawCell:= NewDrawCell;
  Width:= 400;
  Height:= 200;
  RowCount:= 1;
  ColCount:= 20;
  FixedRows:= 0;
  FixedCols:= 0;
  DefaultColWidth:= 12;
  DefaultRowHeight:= 13;
  ColWidths[0]:= 25;
  ColWidths[1]:= 25;
  ColWidths[2]:= 50;
  ColWidths[19]:= 100;
  Cells[0, 0]:= 'SD';
  Cells[1, 0]:= 'SH';
  Cells[2, 0]:= 'HEX';
  Cells[3, 0]:= '7';
  Cells[4, 0]:= '6';
  Cells[5, 0]:= '5';
  Cells[6, 0]:= '4';
  Cells[7, 0]:= '3';
  Cells[8, 0]:= '2';
  Cells[9, 0]:= '1';
  Cells[10, 0]:= '0';
  Cells[11, 0]:= '7';
  Cells[12, 0]:= '6';
  Cells[13, 0]:= '5';
  Cells[14, 0]:= '4';
  Cells[15, 0]:= '3';
  Cells[16, 0]:= '2';
  Cells[17, 0]:= '1';
  Cells[18, 0]:= '0';
  Cells[19, 0]:= 'Telegram';
  Options:= Options + [goEditing];
end;

procedure TSSEditGrid.InsertSS(Numer: Byte; Seria: Word;
  Telegram: string);
var i,NN   : integer;
    Jest   : Boolean;
    SeriaBin: TWordRek;
  function ZnajdzSerie(N: Byte): Integer;
  var ii: Integer;
  begin
    Result:= -1;
    for ii:=1 to RowCount - 1 do
      if StrToInt(Cells[0, ii]) = N then
      begin
        Result:= ii;
        Break;
      end;
  end;
begin
  NN:= ZnajdzSerie(Numer);
  SeriaBin:= WordToBinRek(Seria);
  if NN = -1 then
  begin
    Jest:= False;
    for i:= Numer - 1 downto 1 do
    begin
      NN:= ZnajdzSerie(i);
      if NN <> - 1 then
      begin
        NN:= NN + 1;
        Jest:= True;
        Break;
      end;
    end;
    if not Jest then
    begin
      for i:= Numer + 1 to 255 do
      begin
        NN:= ZnajdzSerie(i);
        if NN <> - 1 then
        begin
          Jest:= True;
          Break;
        end;
      end;
      if not Jest then NN:= 2; // niema jeszcze zadnej serrii
    end;
    InsertRow(NN, [ByteToDec(Numer), ByteToHex(Numer, ''), WordToHex(Seria, ''),
        IntToStr(SeriaBin.B15),
        IntToStr(SeriaBin.B14),
        IntToStr(SeriaBin.B13),
        IntToStr(SeriaBin.B12),
        IntToStr(SeriaBin.B11),
        IntToStr(SeriaBin.B10),
        IntToStr(SeriaBin.B9),
        IntToStr(SeriaBin.B8),
        IntToStr(SeriaBin.B7),
        IntToStr(SeriaBin.B6),
        IntToStr(SeriaBin.B5),
        IntToStr(SeriaBin.B4),
        IntToStr(SeriaBin.B3),
        IntToStr(SeriaBin.B2),
        IntToStr(SeriaBin.B1),
        IntToStr(SeriaBin.B0),
        Telegram]);
  end else
  begin
    InsertRowValue(NN, [ByteToDec(Numer), ByteToHex(Numer, ''),
      WordToHex(Seria, ''),
        IntToStr(SeriaBin.B15),
        IntToStr(SeriaBin.B14),
        IntToStr(SeriaBin.B13),
        IntToStr(SeriaBin.B12),
        IntToStr(SeriaBin.B11),
        IntToStr(SeriaBin.B10),
        IntToStr(SeriaBin.B9),
        IntToStr(SeriaBin.B8),
        IntToStr(SeriaBin.B7),
        IntToStr(SeriaBin.B6),
        IntToStr(SeriaBin.B5),
        IntToStr(SeriaBin.B4),
        IntToStr(SeriaBin.B3),
        IntToStr(SeriaBin.B2),
        IntToStr(SeriaBin.B1),
        IntToStr(SeriaBin.B0),
        Telegram]);
  end;
end;

procedure TSSEditGrid.NewDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var P : Pointer;
    CB, CF : TColor;
begin
  CB:= Color;
  CF:= Font.Color;
  if (ARow = 0) or (ACol = 0) or (ACol = 1) then
  begin
    CB:= FixedColor;
    CF:= FFixedColorFont;
  end else
  begin
    P:= Objects[ACol, ARow];
    if P <> nil then
      if TZmiana(P).Zmiana then
      begin
        CB:= clBlue;
        CF:= clWindow;
      end;
  end;
  Canvas.Brush.Color:= CB;
  Canvas.FillRect(Rect);
  Canvas.Font.Color:= CF;
  DrawText(Canvas.Handle, PChar(Cells[ACol, ARow]),
    -1, Rect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
end;

function TSSEditGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  if (ARow = 0) or ((ACol >= 0) and (ACol <= 1)) or (ACol = ColCount - 1)   
    then Result:= False
    else Result:= inherited SelectCell(ACol, ARow);
end;

procedure TSSEditGrid.SetEditText(ACol, ARow: Integer;
  const Value: string);
begin
  if Col = 2 then inherited;
end;

procedure TSSEditGrid.WMChar(var Msg: TWMChar);
begin
  if Col <> 2 then
  begin
    if (Chr(Msg.CharCode) = '1') or (Chr(Msg.CharCode) = '0')
    then begin
      Cells[Col, Row] := Chr(Msg.CharCode);
      if Col <= ColCount - 1
       then Col:= Col + 1
       else Col:= Col;
    end;
  end else
  begin
//    inherited;
    Cells[Col, Row] := Cells[Col, Row] + Chr(Msg.CharCode);
  end;
end;

end.
