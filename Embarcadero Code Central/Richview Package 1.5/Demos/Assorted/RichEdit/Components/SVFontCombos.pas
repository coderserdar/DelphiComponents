{=============================}unit SVFontCombos;{=============================}
{ TFontSizeComboBox - combobox for selecting sizes of font                     }
{ TFontCharsetComboBox - combobox for selecting character sets of font         }
{ Assign FontName property of these combos and they will be filled with values }
{------------------------------------------------------------------------------}
{ TFontCharsetComboBox uses Items.Objects property itself, do not use it.      }
{ TFontCharsetComboBox has additional properties:                              }
{ AddDefaultCharset: Boolean - if True, DEFAULT_CHARSET item will be added     }
{ DefaultCharsetCaption: String - caption of list item above                   }
{ Charsets[Index: Integer]: TFontCharset - returns Charset of the Index-th item}
{ function IndexOfCharset(Charset: TFontCharset):Integer - returns index of    }
{ item with specified Charset in combo, or -1 if not found                     }
{==============================================================================}
{ ver 1.1, Apr 16, 2001 (C) Sergey Tkachenko www.trichview.com                 }
{==============================================================================}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFontSizeComboBox = class(TComboBox)
  private
    PixelsPerInch: Integer;
    FFontName: TFontName;
    procedure SetFontName(const Value: TFontName);
    procedure Build;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    property FontName: TFontName read FFontName write SetFontName;
  published
    { Published declarations }
  end;

  TFontCharsetComboBox = class(TComboBox)
  private
    FFontName: TFontName;
    FAddDefaultCharset: Boolean;
    FDefaultCharsetCaption: String;
    procedure SetFontName(const Value: TFontName);
    procedure Build;
    function GetCharsets(Index: Integer): TFontCharset;
    procedure SetAddDefaultCharset(const Value: Boolean);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);override;
    function IndexOfCharset(Charset: TFontCharset):Integer;
    property FontName: TFontName read FFontName write SetFontName;
    property Charsets[Index: Integer]: TFontCharset read GetCharsets;
  published
    { Published declarations }
    property AddDefaultCharset: Boolean read FAddDefaultCharset write SetAddDefaultCharset;
    property DefaultCharsetCaption: String read FDefaultCharsetCaption write FDefaultCharsetCaption;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RichView Misc', [TFontSizeComboBox, TFontCharsetComboBox]);
end;

{============================= TFontSizeComboBox ==============================}
function EnumFontSizes(var EnumLogFont: TEnumLogFont;
  PTextMetric: PNewTextMetric; FontType: Integer; Data: LPARAM): Integer;
  export; stdcall;
var s: String;
    i,v,v2: Integer;
begin
  if (FontType and TRUETYPE_FONTTYPE)<>0 then begin
    TFontSizeComboBox(Data).Items.Add('8');
    TFontSizeComboBox(Data).Items.Add('9');
    TFontSizeComboBox(Data).Items.Add('10');
    TFontSizeComboBox(Data).Items.Add('11');
    TFontSizeComboBox(Data).Items.Add('12');
    TFontSizeComboBox(Data).Items.Add('14');
    TFontSizeComboBox(Data).Items.Add('16');
    TFontSizeComboBox(Data).Items.Add('18');
    TFontSizeComboBox(Data).Items.Add('20');
    TFontSizeComboBox(Data).Items.Add('22');
    TFontSizeComboBox(Data).Items.Add('24');
    TFontSizeComboBox(Data).Items.Add('26');
    TFontSizeComboBox(Data).Items.Add('28');
    TFontSizeComboBox(Data).Items.Add('36');
    TFontSizeComboBox(Data).Items.Add('48');
    TFontSizeComboBox(Data).Items.Add('72');
    Result := 0;
    end
  else begin
    v := Round((EnumLogFont.elfLogFont.lfHeight-PTextMetric.tmInternalLeading)*72 /
      TFontSizeComboBox(Data).PixelsPerInch);
    s := IntToStr(v);
    Result := 1;
    for i := 0 to TFontSizeComboBox(Data).Items.Count-1 do begin
      v2 := StrToInt(TFontSizeComboBox(Data).Items[i]);
      if v2=v then
        exit;
      if v2>v then begin
        TFontSizeComboBox(Data).Items.Insert(i,s);
        exit;
      end;
    end;
    TFontSizeComboBox(Data).Items.Add(s);
  end;
end;
{------------------------------------------------------------------------------}
procedure TFontSizeComboBox.Build;
var
  DC: HDC;
  OC: TNotifyEvent;
begin
  DC := GetDC(0);
  Items.BeginUpdate;
  try
    Items.Clear;
    if FontName<>'' then begin
      PixelsPerInch := GetDeviceCaps(DC, LOGPIXELSY);
      EnumFontFamilies(DC, PChar(FontName), @EnumFontSizes, Longint(Self));
      OC := OnClick;
      OnClick := nil;
      ItemIndex := Items.IndexOf(Text);
      OnClick := OC;
      if Assigned(OnClick) then
        OnClick(Self);
    end;
  finally
    Items.EndUpdate;
    ReleaseDC(0, DC);
  end;
end;
{------------------------------------------------------------------------------}
procedure TFontSizeComboBox.SetFontName(const Value: TFontName);
begin
  FFontName := Value;
  Build;
end;
{============================= TFontCharsetComboBox ===========================}
function EnumFontCharsets(var EnumLogFont: TEnumLogFontEx;
  PTextMetric: PNewTextMetricEx; FontType: Integer; Data: LPARAM): Integer;
  export; stdcall;
var s: String;
    l,cs: Integer;
begin
  Result := 1;
  cs := EnumLogFont.elfLogFont.lfCharSet;
  if cs<>MAC_CHARSET then begin
    l := StrLen(EnumLogFont.elfScript);
    SetLength(s,l);
    Move(EnumLogFont.elfScript, PChar(s)^,  l);
    for l := 0 to TFontCharsetComboBox(Data).Items.Count-1 do begin
      if Integer(TFontCharsetComboBox(Data).Items.Objects[l])=cs then
        exit;
      if AnsiCompareText(TFontCharsetComboBox(Data).Items[l],s)>0 then begin
        TFontCharsetComboBox(Data).Items.InsertObject(l,s,TObject(cs));
        exit;
      end;
    end;
    TFontCharsetComboBox(Data).Items.AddObject(s, TObject(cs));
  end;
end;
{------------------------------------------------------------------------------}
constructor TFontCharsetComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCharsetCaption := '(Default)';
end;
{------------------------------------------------------------------------------}
procedure TFontCharsetComboBox.Build;
var
  DC: HDC;
  lf: TLogFont;
  CurrentCharset,idx: Integer;
  OC: TNotifyEvent;
begin
  DC := GetDC(0);
  Items.BeginUpdate;
  try
    if ItemIndex=-1 then
      CurrentCharset := -1
    else
      CurrentCharset := Integer(Charsets[ItemIndex]);
    Items.Clear;
    if FontName<>'' then begin
      FillChar(lf, sizeof(lf), 0);
      lf.lfCharset  := DEFAULT_CHARSET;
      Move(PChar(FontName)^, lf.lfFaceName, Length(FontName));
      EnumFontFamiliesEx(DC, lf, @EnumFontCharsets, Longint(Self),0);
      if AddDefaultCharset then
        Items.AddObject(DefaultCharsetCaption, TObject(DEFAULT_CHARSET));
      idx := Items.IndexOfObject(TObject(CurrentCharset));
      OC := OnClick;
      OnClick := nil;
      if (idx<>-1) then
        ItemIndex := idx
      else
        if Items.Count>0 then
          ItemIndex := 0;
      OnClick := OC;
      if Assigned(OnClick) then
        OnClick(Self);
    end;
  finally
    Items.EndUpdate;
    ReleaseDC(0, DC);
  end;
end;
{------------------------------------------------------------------------------}
function TFontCharsetComboBox.GetCharsets(Index: Integer): TFontCharset;
begin
  Result := TFontCharset(Items.Objects[Index]);
end;
{------------------------------------------------------------------------------}
function TFontCharsetComboBox.IndexOfCharset(
  Charset: TFontCharset): Integer;
begin
  Result := Items.IndexOfObject(TObject(Charset));
end;
{------------------------------------------------------------------------------}
procedure TFontCharsetComboBox.SetAddDefaultCharset(const Value: Boolean);
begin
  if FAddDefaultCharset<>Value then begin
    FAddDefaultCharset := Value;
    Build;
  end;
end;
{------------------------------------------------------------------------------}
procedure TFontCharsetComboBox.SetFontName(const Value: TFontName);
begin
  FFontName := Value;
  Build;
end;

end.
