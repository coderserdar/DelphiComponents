unit CustSFXGenerator;

interface
uses
  Windows,
  Messages,
  Classes,
  SysUtils,
  ArchiverMisc,
  ArchiverRoot;

type
  TCustomSFXGenerator = class(TComponent)
  protected
    FLanguage : TLanguage;
    FNewTagInfoSize : Integer;
    FNewSFXCodeSize : Integer;
    FCurrentTagInfoSize : Integer;
    FCurrentSFXCodeSize : Integer;

    procedure SetLanguage( val : TLanguage );
    procedure UpdateLanguage; virtual;

  public
    // Creators & Destructor
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure WriteSFXCodeToStream( S : TStream ); virtual;
    procedure UpdateTagInfos( S : TStream ); virtual;
    procedure ExtractSizeInfosFromFile( const FileName : String );
    procedure DefineSizeFromFile( const FileName : String );

    // Public properties
    property NewTagInfoSize : Integer read FNewTagInfoSize write FNewTagInfoSize;
    property NewSFXCodeSize : Integer read FNewSFXCodeSize write FNewSFXCodeSize;
    property CurrentTagInfoSize : Integer read FCurrentTagInfoSize write FCurrentTagInfoSize;
    property CurrentSFXCodeSize : Integer read FCurrentSFXCodeSize write FCurrentSFXCodeSize;

  published
    property Language : TLanguage read FLanguage write SetLanguage;
  end;

implementation

procedure TCustomSFXGenerator.SetLanguage( val : TLanguage );
begin
  FLanguage := val;
  UpdateLanguage;
end;

procedure TCustomSFXGenerator.UpdateLanguage;
begin
end;

constructor TCustomSFXGenerator.Create( AOwner : TComponent );
begin
  inherited;
  Language := lgAutomatic;
end;

destructor  TCustomSFXGenerator.Destroy;
begin
  inherited;
end;

procedure TCustomSFXGenerator.WriteSFXCodeToStream( S : TStream );
begin
end;

procedure TCustomSFXGenerator.UpdateTagInfos( S : TStream );
begin
end;

procedure TCustomSFXGenerator.ExtractSizeInfosFromFile( const FileName : String );
var
  L : TStringList;
begin
  L := TStringList.Create;
  try
    GetVersionInfo( FileName, L );
    CurrentSFXCodeSize := StrToIntDef( L.Values['SFXCodeSize'], 0 );
    CurrentTagInfoSize := StrToIntDef( L.Values['TagInfoSize'], 0 );
  finally
    L.Free;
  end;
end;

procedure TCustomSFXGenerator.DefineSizeFromFile( const FileName : String );
begin
  if FileExists( FileName ) then
    ExtractSizeInfosFromFile( FileName )
  else
    begin
      CurrentSFXCodeSize := NewSFXCodeSize;
      CurrentTagInfoSize := NewTagInfoSize;
    end;
end;

end.
