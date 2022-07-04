unit o_GTMenus;

interface
uses
   Classes
  ,Menus
  ,Graphics
  ,Types
  ,Windows
  ,Controls
  ;
type
{------------------------------------------------------------------------------}
 TMenuStyle = (
                 msDefault
                ,msOffice2007Luna
                ,msOffice2007Obsidian
                ,msOffice2007Silver
                ,msVista
                );
{------------------------------------------------------------------------------}
 TGradientDirection = (
                        gdVertical
                       ,gdHorizontal
                       );
{------------------------------------------------------------------------------}
  TgtMenuAppearence = class(TPersistent)
  private
    FMarginY: Byte;
    FGutterWidth: Byte;
    FMarginX: Byte;
    FSeparatorLeading: Byte;
    FGradient1Start: TColor;
    FItemSelectedColor: TColor;
    FFontColor: TColor;
    FGradient2End: TColor;
    FSeparatorBackgroundColor: TColor;
    FItemBackgroundColor: TColor;
    FSeparatorLineColor: TColor;
    FGradient1End: TColor;
    FGutterColor: TColor;
    FGradient2Start: TColor;
    FFontDisabledColor: TColor;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure DrawDefault;
    procedure DrawLuna;
    procedure DrawObsidian;
    procedure DrawSilver;
    procedure DrawVista;
  public
    procedure Assign(Source : TPersistent);override;
  published
    { Published declarations}
    property Gradient1Start          : TColor read FGradient1Start write FGradient1Start;
    property Gradient1End            : TColor read FGradient1End   write FGradient1End;
    property Gradient2Start          : TColor read FGradient2Start write FGradient2Start;
    property Gradient2End            : TColor read FGradient2End   write FGradient2End;

    property MarginX                 : Byte read FMarginX          write FMarginX;
    property MarginY                 : Byte read FMarginY          write FMarginY;
    property SeparatorLeading        : Byte read FSeparatorLeading write FSeparatorLeading;
    property GutterWidth             : Byte read FGutterWidth      write FGutterWidth;

    property SeparatorBackgroundColor: TColor read FSeparatorBackgroundColor write FSeparatorBackgroundColor;
    property SeparatorLineColor      : TColor read FSeparatorLineColor       write FSeparatorLineColor;
    property GutterColor             : TColor read FGutterColor              write FGutterColor;
    property ItemBackgroundColor     : TColor read FItemBackgroundColor      write FItemBackgroundColor;
    property ItemSelectedColor       : TColor read FItemSelectedColor        write FItemSelectedColor;
    property FontColor               : TColor read FFontColor                write FFontColor;
    property FontDisabledColor       : TColor read FFontDisabledColor        write FFontDisabledColor;
  end;
{------------------------------------------------------------------------------}
  TgtCustomOfficeMenu = class(TComponent)
  private
    FMenuAppearence : TgtMenuAppearence;
    FMenuStyle: TMenuStyle;
    procedure SetMenuAppearence(const Value: TgtMenuAppearence);
    procedure SetMenuStyle(const Value: TMenuStyle);
    { Private declarations }
  protected
    { Protected declarations }
    function  FillGradient(DC: HDC; ARect: TRect; ColorCount: Integer;StartColor, EndColor: TColor; ADirection: TGradientDirection): Boolean;
    procedure InitializeItems;virtual;abstract;
    function  IsMenuItemDivider(AMenuItem : TMenuItem): Boolean;
  protected
    procedure MeasureSeparator   (Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure SeparatorDrawItem  (Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure MenuItemMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure MenuItemDrawItem   (Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner : TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property Appearance : TgtMenuAppearence read FMenuAppearence write SetMenuAppearence;
    property Style      : TMenuStyle        read FMenuStyle      write SetMenuStyle;
  end;
{------------------------------------------------------------------------------}
  TgtOfficeMainMenu = class(TgtCustomOfficeMenu)
  private
    FMainMenu: TMainMenu;
    procedure SetMainMenu(const Value: TMainMenu);
    { Private declarations }
  protected
    { Protected declarations }
    procedure InitializeItems;override;
  public
    { Public declarations }
  published
    { Published declarations}
    property MainMenu : TMainMenu read FMainMenu write SetMainMenu;
  end;
{------------------------------------------------------------------------------}
  TgtOfficePopUpMenu = class(TgtCustomOfficeMenu)
  private
    FPopUpMenu: TPopUpMenu;
    procedure SetPopUpMenu(const Value: TPopUpMenu);
    { Private declarations }
  protected
    { Protected declarations }
    procedure InitializeItems;override;
  public
    { Public declarations }
  published
    { Published declarations}
    property PopUpMenu : TPopUpMenu read FPopUpMenu write SetPopUpMenu;
  end;

{------------------------------------------------------------------------------}


implementation

uses
   Math
  ;










{ TgtMenuAppearence }
{------------------------------------------------------------------------------}
procedure TgtMenuAppearence.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if Source is TgtMenuAppearence then
    begin
      Self.Gradient1Start             := TgtMenuAppearence(Source).Gradient1Start;
      Self.Gradient1End               := TgtMenuAppearence(Source).Gradient1End;
      Self.Gradient2Start             := TgtMenuAppearence(Source).Gradient2Start;
      Self.Gradient2End               := TgtMenuAppearence(Source).Gradient2End;

      Self.MarginX                    := TgtMenuAppearence(Source).MarginX;
      Self.MarginY                    := TgtMenuAppearence(Source).MarginY;
      Self.SeparatorLeading           := TgtMenuAppearence(Source).SeparatorLeading;
      Self.GutterWidth                := TgtMenuAppearence(Source).GutterWidth;

      Self.SeparatorBackgroundColor   := TgtMenuAppearence(Source).SeparatorBackgroundColor;
      Self.SeparatorLineColor         := TgtMenuAppearence(Source).SeparatorLineColor;
      Self.GutterColor                := TgtMenuAppearence(Source).GutterColor;
      Self.ItemBackgroundColor        := TgtMenuAppearence(Source).ItemBackgroundColor;
      Self.ItemSelectedColor          := TgtMenuAppearence(Source).ItemSelectedColor;
      Self.FontColor                  := TgtMenuAppearence(Source).FontColor;
      Self.FontDisabledColor          := TgtMenuAppearence(Source).FontDisabledColor;
    end
    else
      inherited Assign(Source);
  end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
procedure TgtMenuAppearence.DrawDefault;
begin
  MarginX                  := 4;
  MarginY                  := 2;
  SeparatorLeading         := 6;
  GutterWidth              := 26;
  SeparatorBackgroundColor := $00eee7dd;
  SeparatorLineColor       := $00c5c5c5;
  GutterColor              := $00eeeee9;
  ItemBackgroundColor      := $00fafafa;
  ItemSelectedColor        := $00e6d5cb;
  FontColor                := $006e1500;
  FontDisabledColor        := $00dec5d8;
  Gradient1Start           := $00efe8e4;
  Gradient1End             := $00dec5b8;
  Gradient2Start           := $00d8baab;
  Gradient2End             := $00efe8e4;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuAppearence.DrawLuna;
begin
  MarginX                   := 4;
  MarginY                   := 2;
  SeparatorLeading          := 6;
  GutterWidth               := 36;
  SeparatorBackgroundColor  := clWhite;
  SeparatorLineColor        := $00C5C5C5;
  GutterColor               := $00F2DAC2;
  ItemBackgroundColor       := clWhite;
  ItemSelectedColor         := clNavy;
  FontColor                 := clNavy;
  FontDisabledColor         := clSilver;
  Gradient1Start            := $00EBFDFF;
  Gradient1End              := $00ABEBFF;
  Gradient2Start            := $0069D6FF;
  Gradient2End              := $0096E4FF;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuAppearence.DrawObsidian;
begin
  MarginX                   := 4;
  MarginY                   := 2;
  SeparatorLeading          := 6;
  GutterWidth               := 36;
  SeparatorBackgroundColor  := clWhite;
  SeparatorLineColor        := $00C5C5C5;
  GutterColor               := $00676767;
  ItemBackgroundColor       := clWhite;
  ItemSelectedColor         := $00464646;
  FontColor                 := $00464646;
  FontDisabledColor         := clSilver;
  Gradient1Start            := $00EBFDFF;
  Gradient1End              := $00ABEBFF;
  Gradient2Start            := $0069D6FF;
  Gradient2End              := $0096E4FF;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuAppearence.DrawSilver;
begin
  MarginX                   := 4;
  MarginY                   := 2;
  SeparatorLeading          := 6;
  GutterWidth               := 36;
  SeparatorBackgroundColor  := clWhite;
  SeparatorLineColor        := $00C5C5C5;
  GutterColor               := $00e0e4e7;
  ItemBackgroundColor       := clWhite;
  ItemSelectedColor         := $004c535c;
  FontColor                 := $004c535c;
  FontDisabledColor         := clSilver;
  Gradient1Start            := $00EBFDFF;
  Gradient1End              := $00ABEBFF;
  Gradient2Start            := $0069D6FF;
  Gradient2End              := $0096E4FF;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuAppearence.DrawVista;
begin
  MarginX                   := 2;
  MarginY                   := 2;
  SeparatorLeading          := 2;
  GutterWidth               := 28;
  SeparatorBackgroundColor  := $00E0E0E0;
  SeparatorLineColor        := clWhite;
  GutterColor               := $00f0f0f0;
  ItemBackgroundColor       := $00f0f0f0;
  ItemSelectedColor         := clNone;
  FontColor                 := clBlack;
  FontDisabledColor         := clSilver;
  Gradient1Start            := $00F4F1EB;
  Gradient1End              := $00F3EBDA;
  Gradient2Start            := $00F3EBDA;
  Gradient2End              := $00F4F1EB;
end;
{------------------------------------------------------------------------------}




{ TgtCustomOfficeMenu }
{------------------------------------------------------------------------------}
constructor TgtCustomOfficeMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMenuAppearence := TgtMenuAppearence.Create;
  Style           := msDefault;
end;
{------------------------------------------------------------------------------}
destructor TgtCustomOfficeMenu.Destroy;
begin
  FMenuAppearence.Free;
  inherited;
end;
{------------------------------------------------------------------------------}




{------------------------------------------------------------------------------}
function TgtCustomOfficeMenu.IsMenuItemDivider(AMenuItem : TMenuItem): Boolean;
begin
  Result := AMenuItem.Caption = '-';
end;
{------------------------------------------------------------------------------}
procedure TgtCustomOfficeMenu.MeasureSeparator(Sender: TObject;ACanvas: TCanvas; var Width, Height: Integer);
var
  SeparatorHint: string;
  Item         : TMenuItem;
  R            : TRect;
begin
  Item := TMenuItem(sender);
  SeparatorHint := Item.Hint;

  //Separator with text:
  if SeparatorHint <> '' then
  begin
    //Initialize
    R := Rect(0, 0, 0, 0);
    ACanvas.Font.Style := [fsBold];

    //Make windows calculate needed space
    Height := DrawText(ACanvas.Handle, PChar(SeparatorHint), Length(SeparatorHint), R, DT_CALCRECT or DT_LEFT or DT_EXTERNALLEADING);
    Width := R.Right - R.Left;

    //Give some extra room for padding:
    inc(Height, Appearance.MarginY*4);
    inc(Width,  Appearance.MarginX*2 + Appearance.SeparatorLeading);
  end
  else
  //Plain old separator:
  begin
    //Fixed height and width:
    Height := 4;
    Width  := 10;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomOfficeMenu.SeparatorDrawItem(Sender: TObject;ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var
  HintStr  : string;
  Item     : TMenuItem;
  R        : TRect;
  HasGutter: boolean;
begin
  Item      := TMenuItem(sender);
  HasGutter := Item.GetImageList <> nil;

  //Background:
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := Appearance.SeparatorBackgroundColor;
  ACanvas.FillRect(ARect);

  //Lines:
  ACanvas.Pen.Color := Appearance.SeparatorLineColor;
  ACanvas.Polyline([point(ARect.Left, ARect.Bottom-2), point(ARect.Right, ARect.Bottom-2)]);
  ACanvas.Pen.Color := Appearance.ItemBackgroundColor;
  ACanvas.Polyline([point(ARect.Left, ARect.Bottom-1), point(ARect.Right, ARect.Bottom-1)]);

  //Text
  HintStr := Item.Hint;
  if Length(HintStr) > 0  then
  begin
    //Text:
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Style := [fsBold];
    ACanvas.Font.Color := Appearance.FontColor;
    r.Left := ARect.Left + Appearance.MarginX;
    if hasGutter then
      inc(R.Left, Appearance.SeparatorLeading);
    R.Right  := ARect.Right - Appearance.MarginX;
    R.Top    := ARect.Top;
    R.Bottom := ARect.Bottom;
    DrawText(ACanvas.Handle, PChar(hintStr), Length(hintStr), R, DT_LEFT or DT_EXTERNALLEADING or DT_SINGLELINE	or DT_VCENTER);
  end
  else
  if HasGutter then
  begin
   //Gutter
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := Appearance.GutterColor;
    R                   := ARect;
    R.Right             := Appearance.GutterWidth;
    ACanvas.FillRect(r);

    ACanvas.Pen.Color := Appearance.SeparatorLineColor;
    ACanvas.Polyline([Point(R.Right, R.top), point(R.Right, R.Bottom)]);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomOfficeMenu.MenuItemDrawItem(Sender: TObject;ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var
  HintStr     : string;
  CaptionStr  : string;
  ShortCutStr : string;
  R           : TRect;
  Offset      : Integer;
  SelRgn      : HRGN;
  HasGutter   : boolean;
  Item        : TMenuItem;
begin
  Item       := TMenuItem(sender);
  HintStr    := Item.Hint;
  CaptionStr := Item.Caption;
  HasGutter  := Item.GetImageList <> nil;

  //Caption-hight:
  ACanvas.Font.Style := [fsBold];
  R      := Rect(0, 0, 0, 0);
  Offset := DrawText(ACanvas.Handle, PChar(captionStr), length(captionStr), r, DT_CALCRECT or DT_EXTERNALLEADING or DT_TOP);

  //Backgrount
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := Appearance.ItemBackgroundColor;
  ACanvas.FillRect(ARect);

  //Gutter
  if HasGutter then
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := Appearance.GutterColor;
    R := ARect;
    R.Right := Appearance.GutterWidth;
    ACanvas.FillRect(r);

    ACanvas.Pen.Color := Appearance.SeparatorLineColor;
    ACanvas.Polyline([Point(R.Right, R.top), Point(R.Right, R.Bottom)]);
  end;

  //Selection
  if selected then
  begin
    //Set a rounded rectangle as clip-region
    SelRgn := CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 3, 3);
    SelectClipRgn(ACanvas.Handle, SelRgn);

    if hintStr <> '' then
    begin
      //First gradient - caption
      R := ARect;
      R.Bottom := R.Top + Offset + Appearance.MarginY*2;
      FillGradient(ACanvas.Handle, R, 256, Appearance.Gradient1Start, Appearance.Gradient1End, gdVertical);

      //Second gradient - description
      R.Top    := R.Bottom;
      R.Bottom := ARect.Bottom;
      FillGradient(ACanvas.Handle, r, 256, Appearance.Gradient2Start, Appearance.Gradient2End, gdVertical);
    end
    else
    begin
      //Only one gradient under captoin
      R := ARect;
      FillGradient(ACanvas.Handle, R, 256, Appearance.Gradient1Start, Appearance.Gradient1End, gdVertical);
    end;

    //Release clipregion
    SelectClipRgn(ACanvas.Handle, 0);

    //Outline selection
    ACanvas.Pen.Color   := Appearance.SeparatorLineColor;
    ACanvas.Brush.Style := bsClear;
    ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 3, 3);
  end;

  //Caption
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Style  := [fsBold];
  if Item.Enabled then
    ACanvas.Font.Color := Appearance.FontColor
  else
    ACanvas.Font.Color := Appearance.FontDisabledColor;
  R.Left := ARect.Left + Appearance.MarginX;
  if hasGutter then
    inc(r.Left, Appearance.GutterWidth);
  R.Right := ARect.Right - Appearance.MarginX;
  R.Top := ARect.Top + Appearance.MarginY;
  R.Bottom := ARect.Bottom;
  DrawText(ACanvas.Handle, PChar(CaptionStr), Length(CaptionStr), R, DT_LEFT or DT_EXTERNALLEADING or DT_TOP);

  //Shortcut
  ShortCutStr := ShortCutToText(item.ShortCut);
  if Length(ShortCutStr) > 0 then
  begin
    DrawText(ACanvas.Handle, PChar(ShortCutStr), Length(shortCutStr), R, DT_RIGHT or DT_EXTERNALLEADING or DT_TOP);
  end;

  //Hint
  ACanvas.Font.Style := [];
  if Item.Enabled then
    ACanvas.Font.Color := Appearance.FontColor
  else
    ACanvas.Font.Color := Appearance.FontDisabledColor;
  R.Left := ARect.Left + Appearance.MarginX;
  if HasGutter then
    inc(R.Left, Appearance.GutterWidth);
  R.Right  := ARect.Right - Appearance.MarginX;
  R.Top    := ARect.Top + Offset + Appearance.MarginY*2;
  R.Bottom := ARect.Bottom;
  DrawText(ACanvas.Handle, PChar(HintStr), Length(HintStr), R, DT_LEFT or DT_EXTERNALLEADING or DT_TOP);

  //Icon
  if (Item.ImageIndex >= 0) and (Item.GetImageList <> nil) then
  begin
    Item.GetImageList.Draw(ACanvas, ARect.Left + Appearance.MarginX, ARect.Top + Appearance.MarginY, Item.ImageIndex);
  end;


end;
{------------------------------------------------------------------------------}
procedure TgtCustomOfficeMenu.MenuItemMeasureItem(Sender: TObject;ACanvas: TCanvas; var Width, Height: Integer);
var
  Item         : TMenuItem;

  CaptionStr   : string;
  CaptionRect  : TRect;
  CaptionHeight: integer;
  CaptionWidth : integer;

  HintStr      : string;
  HintRect     : TRect;
  HintHeight   : integer;
  HintWidth    : integer;

  ShortCutStr  : string;
  ShortCutRect : TRect;
  ShortCutWidth: integer;
begin
  Item := TMenuItem(sender);

  //Caption
  CaptionStr  := Item.Caption;
  CaptionRect := Rect(0, 0, 0, 0);
  ACanvas.Font.Style := [fsBold];

  CaptionHeight := DrawText(ACanvas.Handle, PChar(CaptionStr), Length(CaptionStr), CaptionRect, DT_CALCRECT or DT_LEFT or DT_EXTERNALLEADING);
  CaptionWidth  := CaptionRect.Right - CaptionRect.Left;

  //Shortcut:
  ShortCutStr := ShortCutToText(Item.ShortCut);
  if Length(ShortCutStr) > 0 then
  begin
    ShortCutRect  := Rect(0, 0, 0, 0);
//    shortCutHeight := DrawText(ACanvas.Handle, PChar(shortCutStr), length(shortCutStr), shortCutRect, DT_CALCRECT or DT_RIGHT or DT_EXTERNALLEADING);
    ShortCutWidth := ShortCutRect.Right - ShortCutRect.Left;
    inc(CaptionWidth, shortCutWidth + Appearance.MarginX*2);
  end;

  //Hint:
  HintRect := Rect(0, 0, 0, 0);
  HintStr  := Item.Hint;
  ACanvas.Font.Style := [];

  HintHeight := DrawText(ACanvas.Handle, PChar(HintStr), Length(HintStr), HintRect, DT_CALCRECT or DT_LEFT or DT_EXTERNALLEADING);
  HintWidth  := HintRect.Right - HintRect.Left;

  width := Max(CaptionWidth, HintWidth) + Appearance.MarginX*2;
  if Item.GetImageList <> nil then
    inc(Width, Appearance.GutterWidth);

  Height := CaptionHeight + HintHeight + Appearance.MarginY*4;
end;
{------------------------------------------------------------------------------}
function TgtCustomOfficeMenu.FillGradient(DC: HDC; ARect: TRect;ColorCount: Integer; StartColor, EndColor: TColor;ADirection: TGradientDirection): Boolean;
var
  StartRGB: array [0..2] of Byte;
  RGBKoef : array [0..2] of Double;
  Brush   : HBRUSH;
  AreaWidth, AreaHeight, I: Integer;
  ColorRect  : TRect;
  RectOffset : Double;
begin
  RectOffset := 0;
  Result     := False;
  if ColorCount < 1 then
    Exit;
  StartColor := ColorToRGB(StartColor);
  EndColor := ColorToRGB(EndColor);
  StartRGB[0] := GetRValue(StartColor);
  StartRGB[1] := GetGValue(StartColor);
  StartRGB[2] := GetBValue(StartColor);
  RGBKoef[0] := (GetRValue(EndColor) - StartRGB[0]) / ColorCount;
  RGBKoef[1] := (GetGValue(EndColor) - StartRGB[1]) / ColorCount;
  RGBKoef[2] := (GetBValue(EndColor) - StartRGB[2]) / ColorCount;
  AreaWidth := ARect.Right - ARect.Left;
  AreaHeight :=  ARect.Bottom - ARect.Top;
  case ADirection of
    gdHorizontal:
      RectOffset := AreaWidth / ColorCount;
    gdVertical:
      RectOffset := AreaHeight / ColorCount;
  end;
  for I := 0 to ColorCount - 1 do
  begin
    Brush := CreateSolidBrush(RGB(
      StartRGB[0] + Round((I + 1) * RGBKoef[0]),
      StartRGB[1] + Round((I + 1) * RGBKoef[1]),
      StartRGB[2] + Round((I + 1) * RGBKoef[2])));
    case ADirection of
      gdHorizontal:
        SetRect(ColorRect, Round(RectOffset * I), 0, Round(RectOffset * (I + 1)), AreaHeight);
      gdVertical:
        SetRect(ColorRect, 0, Round(RectOffset * I), AreaWidth, Round(RectOffset * (I + 1)));
    end;
    OffsetRect(ColorRect, ARect.Left, ARect.Top);
    FillRect(DC, ColorRect, Brush);
    DeleteObject(Brush);
  end;
  Result := True;

end;
{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
procedure TgtCustomOfficeMenu.SetMenuAppearence(const Value: TgtMenuAppearence);
begin
  FMenuAppearence.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TgtCustomOfficeMenu.SetMenuStyle(const Value: TMenuStyle);
begin
  FMenuStyle := Value;
  case FMenuStyle of
    msDefault            : FMenuAppearence.DrawDefault;
    msOffice2007Luna     : FMenuAppearence.DrawLuna;
    msOffice2007Obsidian : FMenuAppearence.DrawObsidian;
    msOffice2007Silver   : FMenuAppearence.DrawSilver;
    msVista              : FMenuAppearence.DrawVista;
  end;
end;
{------------------------------------------------------------------------------}






{ TgtOfficeMainMenu }

{------------------------------------------------------------------------------}
procedure TgtOfficeMainMenu.InitializeItems;
  procedure InternalInitializeItem(Item : TMenuItem);
  var
    j : Integer;
  begin
    for j:=0 to Pred(Item.Count) do
    begin
     if IsMenuItemDivider(Item.Items[j]) then
      begin
        Item.Items[j].OnDrawItem    := SeparatorDrawItem;
        Item.Items[j].OnMeasureItem := MeasureSeparator;
      end
      else
      begin
        Item.Items[j].OnDrawItem    := MenuItemDrawItem;
        Item.Items[j].OnMeasureItem := MenuItemMeasureItem;
      end;
      end;
  end;
var
 i : Integer;
begin
  for i:= 0 to Pred(FMainMenu.Items.Count) do
  begin
    if IsMenuItemDivider(FMainMenu.Items[i]) then
    begin
      FMainMenu.Items[i].OnDrawItem    := SeparatorDrawItem;
      FMainMenu.Items[i].OnMeasureItem := MeasureSeparator;
    end
    else
    begin
      FMainMenu.Items[i].OnDrawItem    := MenuItemDrawItem;
      FMainMenu.Items[i].OnMeasureItem := MenuItemMeasureItem;
    end;
    InternalInitializeItem(FMainMenu.Items[i]);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtOfficeMainMenu.SetMainMenu(const Value: TMainMenu);
begin
  if Assigned(FMainMenu) then
  begin
    FMainMenu.RemoveFreeNotification(Self);
    FMainMenu.OwnerDraw := False;
  end;

  FMainMenu := Value;

  if Assigned(FMainMenu) then
  begin
    FMainMenu.FreeNotification(Self);
    FMainMenu.OwnerDraw := True;
    InitializeItems;
  end;
end;
{------------------------------------------------------------------------------}


{ TgtOfficePopUpMenu }
{------------------------------------------------------------------------------}
procedure TgtOfficePopUpMenu.InitializeItems;
var
 i : Integer;
begin
  for i:= 0 to Pred(FPopUpMenu.Items.Count) do
  begin
    if IsMenuItemDivider(FPopUpMenu.Items[i]) then
    begin
      FPopUpMenu.Items[i].OnDrawItem    := SeparatorDrawItem;
      FPopUpMenu.Items[i].OnMeasureItem := MeasureSeparator;
    end
    else
    begin
      FPopUpMenu.Items[i].OnDrawItem    := MenuItemDrawItem;
      FPopUpMenu.Items[i].OnMeasureItem := MenuItemMeasureItem;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtOfficePopUpMenu.SetPopUpMenu(const Value: TPopUpMenu);
begin
  if Assigned(FPopUpMenu) then
  begin
    FPopUpMenu.RemoveFreeNotification(Self);
    FPopUpMenu.OwnerDraw := False;
  end;

  FPopUpMenu := Value;

  if Assigned(FPopUpMenu) then
  begin
    FPopUpMenu.FreeNotification(Self);
    FPopUpMenu.OwnerDraw := True;
    InitializeItems;
  end;
end;
{------------------------------------------------------------------------------}



end.
