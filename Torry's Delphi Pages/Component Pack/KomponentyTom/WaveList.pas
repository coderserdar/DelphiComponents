unit WaveList;

interface

uses
  Windows, Messages, SysUtils, Classes;

type

  TWaveFile = class
  private
    FDane: Pointer;
    FRozDanych: Integer;
    FNazwaPliku: string;
    FPriorytet: Integer;
    FCzasOdtwarzania: Integer;
    FKopia: Boolean;
  public
    destructor Destroy; override;
    function Empty: Boolean;
    procedure LoadFromFile(const FileName: String);
    procedure LoadFromStream(S: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(S: TStream);
    procedure Wczytaj(const ANazwaPliku: string; const APriorytet: Integer;
      const ACzasOdtwarzania: Integer);
    procedure WczytajKop(const ANazwaPliku: string; const APriorytet: Integer;
      const ACzasOdtwarzania: Integer; const AKop: Pointer);
    procedure WczytajPusty(const ANazwaPliku: string; const APriorytet: Integer;
      const ACzasOdtwarzania: Integer);
    property Priorytet: Integer read FPriorytet;
    property NazwaPliku: string read FNazwaPliku;
    property CzasOdtwarzania: Integer read FCzasOdtwarzania;
    property Kopia: Boolean read FKopia;
    property Dane: Pointer read FDane;
  end;

  TWaveList = class(TComponent)
  private
    { Private declarations }
    FLista: TList;
    FAktPriorytet: Integer;
    FKatalog: string;
    FLiczbaPustych: Integer;
    function GetDzwiek(Index: Integer): TWaveFile;
    function GetLiczba: Integer;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function JestDzwiek(const ANazwaPliku: string): TWaveFile;
    function Wstaw(const ANazwaPliku: string; const APriorytet: Integer;
      const ACzasOdtwarzania: Integer): Boolean;
    procedure UsunWszystkie;
    function Graj(const ANumer: Integer): TWaveFile;
    procedure Stop;
    function JestWolny: Boolean;
    property Dzwieki[Index: Integer]: TWaveFile read GetDzwiek;
    property Liczba: Integer read GetLiczba;
    property LiczbaPustych: Integer read FLiczbaPustych;
    property AktPriorytet: Integer read FAktPriorytet;
  published
    property Katalog: string read FKatalog write FKatalog;
  end;

implementation
uses
  MMSystem;

{ TWaveFile }

destructor TWaveFile.Destroy;
begin
  if not Empty then
    FreeMem(FDane, FRozDanych);
  inherited Destroy;
end;

function TWaveFile.Empty: Boolean;
begin
  Result:= FRozDanych = 0;
end;

procedure TWaveFile.LoadFromFile(const FileName: String);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TWaveFile.LoadFromStream(S: TStream);
begin
  if not Empty then
    FreeMem(FDane, FRozDanych);
  FRozDanych := 0;
  FDane := AllocMem(S.Size);
  FRozDanych := S.Size;
  S.Read(FDane^, FRozDanych);
end;

procedure TWaveFile.SaveToFile(const AFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TWaveFile.SaveToStream(S: TStream);
begin
  if not Empty then S.Write(FDane^, FRozDanych);
end;

procedure TWaveFile.Wczytaj(const ANazwaPliku: string; const APriorytet,
  ACzasOdtwarzania: Integer);
begin
  FNazwaPliku := ANazwaPliku;
  FPriorytet:= APriorytet;
  FCzasOdtwarzania:= ACzasOdtwarzania;
  LoadFromFile(ANazwaPliku);
end;

procedure TWaveFile.WczytajKop(const ANazwaPliku: string; const APriorytet,
  ACzasOdtwarzania: Integer; const AKop: Pointer);
begin
  FNazwaPliku := ANazwaPliku;
  FPriorytet:= APriorytet;
  FCzasOdtwarzania:= ACzasOdtwarzania;
  FDane:= AKop;
  FKopia:= True;
end;

procedure TWaveFile.WczytajPusty(const ANazwaPliku: string;
  const APriorytet, ACzasOdtwarzania: Integer);
begin
  FNazwaPliku := ANazwaPliku;
  FPriorytet:= APriorytet;
  FCzasOdtwarzania:= ACzasOdtwarzania;
  FDane:= nil;
  FRozDanych:= 0;
end;

{ TWaveList }

constructor TWaveList.Create(AOwner: TComponent);
begin
  inherited;
  FLista:= TList.Create;
  FAktPriorytet:= -1;
end;

destructor TWaveList.Destroy;
begin
  Stop;
  FLista.Free;
  inherited;
end;

function TWaveList.GetDzwiek(Index: Integer): TWaveFile;
begin
  Result:= TWaveFile(FLista[Index]);
end;

function TWaveList.GetLiczba: Integer;
begin
  Result:= FLista.Count;
end;

function TWaveList.Graj(const ANumer: Integer): TWaveFile;
const
  LoopArray: array[Boolean] of DWORD = (0, SND_LOOP);
begin
  if (ANumer > -1) and (ANumer < FLista.Count) then
  begin
    Result:= Dzwieki[ANumer];
    if JestWolny then       // jestem wolny
    begin
      with Result do
      if PlaySound(FDane, 0, SND_ASYNC or SND_MEMORY or
        LoopArray[Boolean(CzasOdtwarzania)]) then
        FAktPriorytet:= Priorytet;
    end
    else                        // jest zajety
      with Result do
      if Priorytet > FAktPriorytet then           // spr priorytet
      begin
        if PlaySound(FDane, 0, SND_ASYNC or SND_MEMORY or
          LoopArray[Boolean(CzasOdtwarzania)]) then
          FAktPriorytet:= Priorytet;
      end;
  end else
    Result:= nil;
end;

function TWaveList.JestDzwiek(const ANazwaPliku: string): TWaveFile;
var i: Integer;
begin
  Result:= nil;
  for i:= 0 to FLista.Count - 1 do
    if ANazwaPliku = TWaveFile(FLista[i]).NazwaPliku then
    begin
      Result:= TWaveFile(FLista[i]);
      Break;
    end;
end;

function TWaveList.JestWolny: Boolean;
begin
  Result:= PlaySound(nil, 0, SND_NOSTOP);
end;

procedure TWaveList.Stop;
begin
  PlaySound(nil, 0, SND_PURGE);
end;

procedure TWaveList.UsunWszystkie;
var i: Integer;
begin
  for i:= 0 to FLista.Count - 1 do TWaveFile(FLista[i]).Free;
  FLista.Clear;
  FLiczbaPustych:= 0;
end;

function TWaveList.Wstaw(const ANazwaPliku: string; const APriorytet,
  ACzasOdtwarzania: Integer): Boolean;
var
  WF: TWaveFile;
  D: Pointer;

begin
  if FileExists(FKatalog + ANazwaPliku) then
  begin
    WF:= JestDzwiek(FKatalog + ANazwaPliku);
    if WF <> nil then             // tzn, ¿e jest kopia
    begin
      D:= WF.Dane;
      WF:= TWaveFile.Create;
      WF.WczytajKop(FKatalog + ANazwaPliku, APriorytet, ACzasOdtwarzania, D);
    end
    else begin
      WF:= TWaveFile.Create;
      WF.Wczytaj(FKatalog + ANazwaPliku, APriorytet, ACzasOdtwarzania);
    end;
    FLista.Add(WF);
    Result:= True;
  end
  else begin
    WF:= TWaveFile.Create;
    WF.WczytajPusty(FKatalog + ANazwaPliku, APriorytet, ACzasOdtwarzania);
    FLista.Add(WF);    
    Inc(FLiczbaPustych);
    Result:= False;
  end;
end;

end.

