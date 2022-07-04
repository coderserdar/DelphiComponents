unit KytDBGrid;

interface

uses
  SysUtils, Classes, Controls, Grids, DBGrids, Types, Graphics;

type
  TCustomDBGridCracker = class(TCustomDBGrid);
  TKytDBGrid = class(TDBGrid)
  private
    FxThankTo: string;
    FxAuthor: string;
    FxVersion: string;
    FWarnaBarisPilih: TColor;
    FWarnaBarisGenap: TColor;
    FWarnaBarisGanjil: TColor;
    procedure SetxThankTo(const Value: string);
    procedure SetxAuthor(const Value: string);
    procedure SetxVersion(const Value: string);
    procedure SetWarnaBarisGanjil(const Value: TColor);
    procedure SetWarnaBarisGenap(const Value: TColor);
    procedure SetWarnaBarisPilih(const Value: TColor);
    { Private declarations }
  protected
    { Protected declarations }
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property WarnaBarisGanjil:TColor read FWarnaBarisGanjil write SetWarnaBarisGanjil;
    property WarnaBarisGenap:TColor read FWarnaBarisGenap write SetWarnaBarisGenap;
    property WarnaBarisPilih:TColor read FWarnaBarisPilih write SetWarnaBarisPilih;
    property xAuthor:string read FxAuthor write SetxAuthor;
    property xVersion:string read FxVersion write SetxVersion;
    property xThankTo : string read FxThankTo write SetxThankTo;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Kiyat', [TKytDBGrid]);
end;

{ TKytDBGrid }

constructor TKytDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FxVersion          := 'March.27.2005 1.2';
  FxAuthor           := 'Kiyat Yuni Saptoko';
  FxThankTo          :='AgusMade@Diskusiweb.com';
  FWarnaBarisGenap   := clSilver;
  FWarnaBarisGanjil  := clSkyBlue;
  FWarnaBarisPilih   := clMoneyGreen;
end;

destructor TKytDBGrid.Destroy;
begin
  inherited Destroy;
end;

procedure TKytDBGrid.SetxThankTo(const Value: string);
begin
  FxThankTo := Value;
  if FxThankTo <> 'AgusMade@Diskusiweb.com' then FxThankTo := 'AgusMade@Diskusiweb.com';
end;

procedure TKytDBGrid.SetxAuthor(const Value: string);
begin
  FxAuthor := Value;
  if FxAuthor <> 'Kiyat Yuni Saptoko' then FxAuthor := 'Kiyat Yuni Saptoko';
end;

procedure TKytDBGrid.SetxVersion(const Value: string);
begin
  FxVersion := Value;
  if FxVersion <> 'Apr.17.2004 1.2' then FxVersion := 'Apr.17.2004 1.2';
end;

procedure TKytDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
begin
  inherited;
  if DataSource.DataSet.Active = true then
  if DataLink.ActiveRecord = Row - 1 then
  Canvas.Brush.Color := FWarnaBarisPilih
  else if (DataSource.DataSet.RecNo mod 2)=0 then
  Canvas.Brush.Color := FWarnaBarisGenap
  else
  Canvas.Brush.Color := FWarnaBarisGanjil;
  DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TKytDBGrid.SetWarnaBarisGanjil(const Value: TColor);
begin
  FWarnaBarisGanjil := Value;
end;

procedure TKytDBGrid.SetWarnaBarisGenap(const Value: TColor);
begin
  FWarnaBarisGenap := Value;
end;

procedure TKytDBGrid.SetWarnaBarisPilih(const Value: TColor);
begin
  FWarnaBarisPilih := Value;
end;

end.
