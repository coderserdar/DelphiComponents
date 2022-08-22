
{*******************************************************}
{                                                       }
{       Bidirectional Controls Library                  }
{                                                       }
{       TBdDBNavigator                                  }
{       ver. 1.0 (build 1) (February 9, 2003)           }
{                                                       }
{       Copyright (c) 2003 - Amir Rahimi F.             }
{       email: amir@farsicomponents.com                 }
{                                                       }
{*******************************************************}

unit BdDBNavigator;

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, Messages, DBCtrls, Graphics;

type
  TBdDBNavigator = class(TDBNavigator)
  private
    function GetVisibleButtons: TButtonSet;
    procedure SetVisible(const Value: TButtonSet);
    procedure ExchangeGlyphs;
    procedure RearrangeButtons;
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
  protected
    procedure Loaded; override;
    procedure SetBiDiMode(Value: TBiDiMode); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property VisibleButtons: TButtonSet read GetVisibleButtons write SetVisible
      default [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
        nbEdit, nbPost, nbCancel, nbRefresh];
    property BiDiMode;
    property ParentBiDiMode;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Bidirectional', [TBdDBNavigator]);
end;

{ TBdDBNavigator }

constructor TBdDBNavigator.Create(AOwner: TComponent);
begin
  inherited;
  inherited VisibleButtons:= [nbFirst, nbPrior, nbNext, nbLast, nbInsert,
    nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
end;

function TBdDBNavigator.GetVisibleButtons: TButtonSet;
begin
  Result:= inherited VisibleButtons;
end;

procedure TBdDBNavigator.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if UseRightToLeftAlignment then
    if Key = VK_RIGHT then
      Key:= VK_LEFT
    else if Key = VK_LEFT then
      Key:= VK_RIGHT;
  inherited;
end;

procedure TBdDBNavigator.Loaded;
begin
  inherited;
  if UseRightToLeftAlignment then
    RearrangeButtons;
end;

procedure TBdDBNavigator.RearrangeButtons;
var
  I: TNavigateBtn;
begin
  for I := Low(Buttons) to High(Buttons) do
    if Buttons[I].Visible then
      Buttons[I].Left:= Width-Buttons[I].Left-Buttons[I].Width;
end;

procedure TBdDBNavigator.SetBiDiMode(Value: TBiDiMode);
begin
  if Value<>BiDiMode then
  begin
    if UseRightToLeftAlignment then
      ExchangeGlyphs;
    inherited;
    if UseRightToLeftAlignment then
      ExchangeGlyphs;
  end;
end;

procedure TBdDBNavigator.SetVisible(const Value: TButtonSet);
begin
  inherited VisibleButtons:= Value;
  if not (csLoading in ComponentState) and UseRightToLeftAlignment then
    RearrangeButtons;
end;

procedure TBdDBNavigator.ExchangeGlyphs;
var
  AuxBitmap: TBitmap;
begin
  AuxBitmap:= TBitmap.Create;
  try
    AuxBitmap.Assign(Buttons[nbFirst].Glyph);
    Buttons[nbFirst].Glyph:= Buttons[nbLast].Glyph;
    Buttons[nbLast].Glyph:= AuxBitmap;
    AuxBitmap.Assign(Buttons[nbNext].Glyph);
    Buttons[nbNext].Glyph:= Buttons[nbPrior].Glyph;
    Buttons[nbPrior].Glyph:= AuxBitmap;
  finally
    AuxBitmap.Free;
  end;
end;

procedure TBdDBNavigator.WMSize(var Message: TWMSize);
begin
  inherited;
  if UseRightToLeftAlignment then
    RearrangeButtons;
end;

end.
