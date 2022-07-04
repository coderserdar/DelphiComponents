{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtMenuStyler                                   }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTMenuStyler;

interface
uses
    Classes
   ,Graphics
   ,Menus
   ,Types
   ,Windows
  ;

type
{------------------------------------------------------------------------------}
  TgtMenuStyle = (
                      msDefault
                     ,msOfficeBlue
                     ,msObsidian
                     ,msLuna
                     ,msSilver
                     ,msVista
                    );
{------------------------------------------------------------------------------}
  TgtColorScheme = class(TPersistent)
  private
    FGradientStart1           : TColor;
    FGutterColor              : TColor;
    FItemSelectedColor        : TColor;
    FGradientEnd2             : TColor;
    FFontColor                : TColor;
    FGradientStart2           : TColor;
    FGradientEnd1             : TColor;
    FSeperatorBackgroundColor : TColor;
    FSeperatorLineColor       : TColor;
    FFontDisabledColor        : TColor;
    FItemBackgroundColor      : TColor;
    FCheckedColor             : TColor;
    { Private declarations }
  public
    { Public declarations }
    procedure Assign(Source : TPersistent);override;
  public
    procedure ColorSchemeObsidian;
    procedure ColorSchemeLuna;
    procedure ColorSchemeOfficeBlue;
    procedure ColorSchemeVista;
    procedure ColorSchemeDefault;
    procedure ColorSchemeSilver;
  published
    { Published declarations}
    property SeperatorBackgroundColor : TColor read FSeperatorBackgroundColor write FSeperatorBackgroundColor;
    property SeperatorLineColor       : TColor read FSeperatorLineColor       write FSeperatorLineColor;
    property GutterColor              : TColor read FGutterColor              write FGutterColor;
    property ItemBackgroundColor      : TColor read FItemBackgroundColor      write FItemBackgroundColor;
    property ItemSelectedColor        : TColor read FItemSelectedColor        write FItemSelectedColor;
    property FontColor                : TColor read FFontColor                write FFontColor;
    property FontDisabledColor        : TColor read FFontDisabledColor        write FFontDisabledColor;
    property GradientStart1           : TColor read FGradientStart1           write FGradientStart1;
    property GradientEnd1             : TColor read FGradientEnd1             write FGradientEnd1;
    property GradientStart2           : TColor read FGradientStart2           write FGradientStart2;
    property GradientEnd2             : TColor read FGradientEnd2             write FGradientEnd2;
    property CheckedColor             : TColor read FCheckedColor             write FCheckedColor;
  end;
{------------------------------------------------------------------------------}
  TGradientDirection = (gdVertical, gdHorizontal);
{------------------------------------------------------------------------------}
  TgtMenuStyler = class(TComponent)
  private
    FHookAllMenus: Boolean;
    FGutterWidth: Integer;
    FMarginY: Integer;
    FMarginX: Integer;
    FColorScheme: TgtColorScheme;
    FMenuStyle: TgtMenuStyle;
    FMenu: TMenu;
    FSeparatorLead: Integer;
    procedure SetMenu(const Value: TMenu);
    procedure SetHookAllMenus(const Value: Boolean);
    procedure SetMenuStyle(const Value: TgtMenuStyle);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Notification(AComponent : TComponent ; Operation : TOperation);override;
    procedure HookEvents;
    procedure UnHookEvents;
    procedure InvalidateMenu(AMenu : TMenu);
  protected
    //Drawing Routines
    procedure MeasureSeparator   (Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure MenuItemMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure SeparatorDrawItem  (Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure MenuItemDrawItem   (Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    function  FillGradient       (DC    : HDC    ; ARect  : TRect  ; ColorCount: Integer;
                                  StartColor, EndColor: TColor; ADirection: TGradientDirection): Boolean;{from JclGraphics Jedi Project}
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property MenuStyle     : TgtMenuStyle   read FMenuStyle      write SetMenuStyle;
    property MarginX       : Integer        read FMarginX        write FMarginX;
    property MarginY       : Integer        read FMarginY        write FMarginY;
    property GutterWidth   : Integer        read FGutterWidth    write FGutterWidth;
    property SeparatorLead : Integer        read FSeparatorLead  write FSeparatorLead;
    property ColorScheme   : TgtColorScheme read FColorScheme;
    property HookAllMenus  : Boolean        read FHookAllMenus   write SetHookAllMenus;
  published
    property Menu          : TMenu          read FMenu           write SetMenu;
  end;
{------------------------------------------------------------------------------}

implementation

uses
    Math
   ,Controls
  ;

{$R GTMenuStyler.res}


{Based on article found at http://delphi.about.com/od/vclusing/a/2007ownerdraw.htm}

{ TgtColorScheme }
{------------------------------------------------------------------------------}
procedure TgtColorScheme.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if Source is TgtColorScheme then
    begin
      Self.SeperatorBackgroundColor := TgtColorScheme(Source).SeperatorBackgroundColor;
      Self.SeperatorLineColor       := TgtColorScheme(Source).SeperatorLineColor;
      Self.GutterColor              := TgtColorScheme(Source).GutterColor;
      Self.ItemBackgroundColor      := TgtColorScheme(Source).ItemBackgroundColor;
      Self.ItemSelectedColor        := TgtColorScheme(Source).ItemSelectedColor;
      Self.FontColor                := TgtColorScheme(Source).FontColor;
      Self.FontDisabledColor        := TgtColorScheme(Source).FontDisabledColor;
      Self.GradientStart1           := TgtColorScheme(Source).GradientStart1;
      Self.GradientEnd1             := TgtColorScheme(Source).GradientEnd1;
      Self.GradientStart2           := TgtColorScheme(Source).GradientStart2;
      Self.GradientEnd2             := TgtColorScheme(Source).GradientEnd2;
      Self.CheckedColor             := TgtColorScheme(Source).CheckedColor;
    end
    else
      inherited Assign(Source);
  end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
procedure TgtColorScheme.ColorSchemeDefault;
begin
  FSeperatorBackgroundColor  := $00eee7dd;
  FSeperatorLineColor        := $00c5c5c5;
  FGutterColor               := $00eeeee9;
  FItemBackgroundColor       := $00fafafa;
  FItemSelectedColor         := $00e6d5cb;
  FFontColor                 := $006e1500;
  FFontDisabledColor         := $00dec5d8;
  FGradientStart1            := $00efe8e4;
  FGradientEnd1              := $00dec5b8;
  FGradientStart2            := $00d8baab;
  FGradientEnd2              := $00efe8e4;
  FCheckedColor              := $00e6d5cb;
end;
{------------------------------------------------------------------------------}
procedure TgtColorScheme.ColorSchemeSilver;
begin
  FSeperatorBackgroundColor  := clWhite;
  FSeperatorLineColor        := $00C5C5C5;
  FGutterColor               := $00e0e4e7;
  FItemBackgroundColor       := clWhite;
  FItemSelectedColor         := $004c535c;
  FFontColor                 := $004c535c;
  FFontDisabledColor         := clSilver;
  FGradientStart1            := $00EBFDFF;
  FGradientEnd1              := $00ABEBFF;
  FGradientStart2            := $0069D6FF;
  FGradientEnd2              := $0096E4FF;
  FCheckedColor              := $00e6d5cb;
end;
{------------------------------------------------------------------------------}
procedure TgtColorScheme.ColorSchemeLuna;
begin
  FSeperatorBackgroundColor  := clCream;
  FSeperatorLineColor        := $00C5C5C5;
  FGutterColor               := $00F2DAC2;
  FItemBackgroundColor       := clWhite;
  FItemSelectedColor         := clNavy;
  FFontColor                 := clNavy;
  FFontDisabledColor         := clSilver;
  FGradientStart1            := $00EBFDFF;
  FGradientEnd1              := $00ABEBFF;
  FGradientStart2            := $0069D6FF;
  FGradientEnd2              := $0096E4FF;
  FCheckedColor              := $00e6d5cb;
end;
{------------------------------------------------------------------------------}
procedure TgtColorScheme.ColorSchemeObsidian;
begin
  FSeperatorBackgroundColor  := clWhite;
  FSeperatorLineColor        := $00C5C5C5;
  FGutterColor               := $00676767;
  FItemBackgroundColor       := clWhite;
  FItemSelectedColor         := $00464646;
  FFontColor                 := $00464646;
  FFontDisabledColor         := clSilver;
  FGradientStart1            := $00EBFDFF;
  FGradientEnd1              := $00ABEBFF;
  FGradientStart2            := $0069D6FF;
  FGradientEnd2              := $0096E4FF;
  FCheckedColor              := $00e6d5cb;
end;
{------------------------------------------------------------------------------}
procedure TgtColorScheme.ColorSchemeOfficeBlue;
begin
  FSeperatorBackgroundColor  := $00eee7dd;
  FSeperatorLineColor        := $00c5c5c5;
  FGutterColor               := $00eeeee9;
  FItemBackgroundColor       := $00fafafa;
  FItemSelectedColor         := $00e6d5cb;
  FFontColor                 := $006e1500;
  FFontDisabledColor         := $00dec5d8;
  FGradientStart1            := $00efe8e4;
  FGradientEnd1              := $00dec5b8;
  FGradientStart2            := $00d8baab;
  FGradientEnd2              := $00efe8e4;
  FCheckedColor              := $00e6d5cb;
end;
{------------------------------------------------------------------------------}
procedure TgtColorScheme.ColorSchemeVista;
begin
  FSeperatorBackgroundColor := $00E0E0E0;
  FSeperatorLineColor       := clWhite;
  FGutterColor              := $00f0f0f0;
  FItemBackgroundColor      := $00f0f0f0;
  FItemSelectedColor        := clNone;
  FFontColor                := clBlack;
  FFontDisabledColor        := clSilver;
  FGradientStart1           := $00F4F1EB;
  FGradientEnd1             := $00F3EBDA;
  FGradientStart2           := $00F3EBDA;
  FGradientEnd2             := $00F4F1EB;
  FCheckedColor             := $00e6d5cb;
end;
{------------------------------------------------------------------------------}

{ TgtMenuStyler }
{------------------------------------------------------------------------------}
constructor TgtMenuStyler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMarginX       := 4;
  FMarginY       := 4;
  FSeparatorLead := 6;
  FGutterWidth   := 26;
  FHookAllMenus  :=  False;
  FColorScheme   := TgtColorScheme.Create;
  FColorScheme.ColorSchemeDefault;
end;
{------------------------------------------------------------------------------}
destructor TgtMenuStyler.Destroy;
begin
  FColorScheme.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuStyler.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = FMenu then
      Menu := nil;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtMenuStyler.HookEvents;
var
  i : Integer;
begin
  if FHookAllMenus then
  begin
    for i:= 0 to Pred(Self.Owner.ComponentCount) do
    begin
      if Owner.Components[i].InheritsFrom(TMenu) then
        TMenu(Owner.Components[i]).OwnerDraw := True;
      if Owner.Components[i].InheritsFrom(TMenuItem) then
      begin
        if TMenuItem(Owner.Components[i]).IsLine  then
        begin
          TMenuItem(Owner.Components[i]).OnDrawItem     := SeparatorDrawItem;
          TMenuItem(Owner.Components[i]).OnMeasureItem  := MeasureSeparator;
        end
        else
        begin
          TMenuItem(Owner.Components[i]).OnDrawItem    := MenuItemDrawItem;
          TMenuItem(Owner.Components[i]).OnMeasureItem := MenuItemMeasureItem;
        end;
      end;
    end;
  end
 else
  begin
    if Assigned(FMenu) then
    begin
      FMenu.OwnerDraw := True;
      for i:= 0 to Pred(FMenu.Items.Count) do
      begin
        if FMenu.Items[i].IsLine then
        begin
          FMenu.Items[i].OnDrawItem     := SeparatorDrawItem;
          FMenu.Items[i].OnMeasureItem  := MeasureSeparator;
        end
        else
        begin
          FMenu.Items[i].OnDrawItem    := MenuItemDrawItem;
          FMenu.Items[i].OnMeasureItem := MenuItemMeasureItem;
        end;
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuStyler.UnHookEvents;
var
  i : Integer;
begin
  if FHookAllMenus then
  begin
    for i:= 0 to Pred(Self.Owner.ComponentCount) do
    begin
      if Owner.Components[i].InheritsFrom(TMenu) then
        TMenu(Owner.Components[i]).OwnerDraw := False;
      if Owner.Components[i].InheritsFrom(TMenuItem) then
      begin
        TMenuItem(Owner.Components[i]).OnDrawItem    := nil;
        TMenuItem(Owner.Components[i]).OnMeasureItem := nil;
      end;
    end;
  end
  else
  begin
    if Assigned(FMenu) then
    begin
      FMenu.OwnerDraw := False;
      for i:= 0 to Pred(FMenu.Items.Count) do
      begin
        FMenu.Items[i].OnDrawItem    := nil;
        FMenu.Items[i].OnMeasureItem := nil;
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuStyler.MeasureSeparator(Sender: TObject; ACanvas: TCanvas;var Width, Height: Integer);
var
  SeparatorHint : string;
  Item          : TMenuItem;
  R             : TRect;
begin
  Item := TMenuItem(sender);
  SeparatorHint := item.Hint;

  //Separator with text:
  if SeparatorHint <> '' then
  begin
    //Initialize
    R := rect(0, 0, 0, 0);
    ACanvas.Font.Style := [fsBold];

    //Make windows calculate needed space
    Height := drawText(ACanvas.Handle, PChar(SeparatorHint), Length(SeparatorHint), R, DT_CALCRECT or DT_LEFT or DT_EXTERNALLEADING);
    Width := R.Right - R.Left;

    //Give some extra room for padding:
    inc(Height, MarginY*4);
    inc(Width,  MarginX*2 + SeparatorLead);
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
procedure TgtMenuStyler.MenuItemDrawItem(Sender: TObject; ACanvas: TCanvas;ARect: TRect; Selected: Boolean);
const
  Checks: array[Boolean] of DWORD = (MF_UNCHECKED, MF_CHECKED);
var
  HintStr    : string;
  CaptionStr : string;
  ShortCutStr: string;
  R          : TRect;
  Offset     : integer;
  SelRgn     : HRGN;
  HasGutter  : boolean;
  Item       : TMenuItem;
  Bmp        : Graphics.TBitMap;
begin
  Item       := TMenuItem(Sender);
  //If it is a root menu item or the menu is TPopUp which all
  if (Item.Parent.MenuIndex <> -1) or (Item.GetParentMenu.InheritsFrom(TPopUpMenu)) then
  begin
    HintStr    := Item.Hint;
    CaptionStr := Item.Caption;
    HasGutter  := Item.GetImageList <> nil;

    //Caption-hight:
    ACanvas.Font.Style := [fsBold];
    R := rect(0, 0, 0, 0);
    Offset := DrawText(ACanvas.Handle, PChar(CaptionStr), Length(CaptionStr), R, DT_CALCRECT or DT_EXTERNALLEADING or DT_TOP);

    //Backgrount

    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := ColorScheme.ItemBackgroundColor;
    ACanvas.FillRect(ARect);



    //Gutter
    if HasGutter then
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := ColorScheme.GutterColor;
      R       := ARect;
      R.Right := GutterWidth;
      ACanvas.FillRect(R);

      ACanvas.Pen.Color := ColorScheme.SeperatorLineColor;
      ACanvas.Polyline([point(R.Right, R.top), point(R.Right, R.Bottom)]);

      if (Item.Checked) and (Item.ImageIndex < 0) then
      begin
        Bmp               := Graphics.TBitMap.Create;
        try
          Bmp.Handle      := Windows.LoadBitMap(hInstance,'MENUITEMCHECKED');
          Bmp.Transparent := True;
          ACanvas.Draw(ARect.Left+3,ARect.Top+5,Bmp);
        finally
          Bmp.Free;
        end;
      end;
    end;

    //Selection
    if Selected then
    begin
      //Set a rounded rectangle as clip-region
      SelRgn := CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 3, 3);
      SelectClipRgn(ACanvas.Handle, SelRgn);

      if HintStr <> '' then
      begin
        //First gradient - caption
        R := ARect;
        R.Bottom := R.Top + Offset + MarginY*2;
        FillGradient(ACanvas.Handle, R, 256, ColorScheme.GradientStart1, ColorScheme.GradientEnd1, gdVertical);

        //Second gradient - description
        R.Top    := R.Bottom;
        R.Bottom := ARect.Bottom;
        FillGradient(ACanvas.Handle, R, 256, ColorScheme.GradientStart2, ColorScheme.GradientEnd2, gdVertical);
      end
      else
      begin
        //Only one gradient under captoin
        R := ARect;
        FillGradient(ACanvas.Handle, R, 256, ColorScheme.GradientStart1, ColorScheme.GradientEnd1, gdVertical);
      end;

      //Release clipregion
      SelectClipRgn(ACanvas.Handle, 0);

      //Outline selection
      ACanvas.Pen.Color   := ColorScheme.SeperatorLineColor;
      ACanvas.Brush.Style := bsClear;
      ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 3, 3);
      if (Item.Checked) and (Item.ImageIndex < 0) then
      begin
        Bmp               := Graphics.TBitMap.Create;
        try
          Bmp.Handle      := Windows.LoadBitMap(hInstance,'MENUITEMCHECKED');
          Bmp.Transparent := True;
          ACanvas.Draw(ARect.Left+3,ARect.Top+5,Bmp);
        finally
          Bmp.Free;
        end;
      end;
    end;

    //Caption
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Style := [fsBold];
    if Item.Enabled then
      ACanvas.Font.Color := ColorScheme.FontColor
    else
      ACanvas.Font.Color := ColorScheme.FontDisabledColor;
    R.Left := ARect.Left + MarginX;
    if HasGutter then
      inc(R.Left, GutterWidth);
    R.Right  := ARect.Right - MarginX;
    R.Top    := ARect.Top   + MarginY;
    R.Bottom := ARect.Bottom;
    DrawText(ACanvas.Handle, PChar(CaptionStr), Length(CaptionStr), R, DT_LEFT or DT_EXTERNALLEADING or DT_TOP);

    //Shortcut
    ShortCutStr := ShortCutToText(Item.ShortCut);
    if shortCutStr <> '' then
    begin
      DrawText(ACanvas.Handle, PChar(ShortCutStr), Length(shortCutStr), R, DT_RIGHT or DT_EXTERNALLEADING or DT_TOP);
    end;

    //Hint
    ACanvas.Font.Style := [];
    if Item.Enabled then
      ACanvas.Font.Color := ColorScheme.FontColor
    else
      ACanvas.Font.Color := ColorScheme.FontDisabledColor;
    R.Left := ARect.Left + MarginX;
    if HasGutter then
      inc(R.Left, GutterWidth);
    R.Right  := ARect.Right - MarginX;
    R.Top    := ARect.Top + Offset + MarginY*2;
    R.Bottom := ARect.Bottom;
    DrawText(ACanvas.Handle, PChar(HintStr), Length(HintStr), R, DT_LEFT or DT_EXTERNALLEADING or DT_TOP);

    //Icon
    if (Item.ImageIndex >= 0) and (Item.GetImageList <> nil) then
    begin
      Item.GetImageList.Draw(ACanvas, ARect.Left + MarginX, ARect.Top + MarginY, Item.ImageIndex);
    end;
  end
  else
  begin
    HintStr    := Item.Hint;
    CaptionStr := Item.Caption;
    //Caption-hight:
    ACanvas.Font.Style := [fsBold];
    R := rect(0, 0, 0, 0);
    Offset := DrawText(ACanvas.Handle, PChar(CaptionStr), Length(CaptionStr), R, DT_CALCRECT or DT_EXTERNALLEADING or DT_TOP);
    //Backgrount
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := ColorScheme.ItemBackgroundColor;
    ACanvas.FillRect(ARect);
    //Selection
    if Selected then
    begin
      //Set a rounded rectangle as clip-region
      SelRgn := CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 3, 3);
      SelectClipRgn(ACanvas.Handle, selRgn);

      if HintStr <> '' then
      begin
        //First gradient - caption
        R := ARect;
        R.Bottom := R.Top + Offset + MarginY*2;
        FillGradient(ACanvas.Handle, R, 256, ColorScheme.GradientStart1, ColorScheme.GradientEnd1, gdVertical);

        //Second gradient - description
        R.Top    := R.Bottom;
        R.Bottom := ARect.Bottom;
        FillGradient(ACanvas.Handle, R, 256, ColorScheme.GradientStart2, ColorScheme.GradientEnd2, gdVertical);
      end
      else
      begin
        //Only one gradient under captoin
        R := ARect;
        FillGradient(ACanvas.Handle, R, 256, ColorScheme.GradientStart1, ColorScheme.GradientEnd1, gdVertical);
      end;

      //Release clipregion
      SelectClipRgn(ACanvas.Handle, 0);

      //Outline selection
      ACanvas.Pen.Color   := ColorScheme.SeperatorLineColor;
      ACanvas.Brush.Style := bsClear;
      ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 3, 3);
    end;
    //Caption
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Style  := [fsBold];
    if Item.Enabled then
      ACanvas.Font.Color := ColorScheme.FontColor
    else
      ACanvas.Font.Color := ColorScheme.FontDisabledColor;
    R.Left := ARect.Left + MarginX;

    R.Right  := ARect.Right;
    R.Top    := ARect.Top;
    R.Bottom := ARect.Bottom;
    if Item.GetParentMenu.IsRightToLeft then
      DrawText(ACanvas.Handle, PChar(CaptionStr), Length(CaptionStr), R, DT_RIGHT+DT_RTLREADING)
    else
      DrawText(ACanvas.Handle, PChar(CaptionStr), Length(CaptionStr), R, DT_LEFT +DT_RTLREADING);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuStyler.MenuItemMeasureItem(Sender: TObject;ACanvas: TCanvas; var Width, Height: Integer);
var
  Item          : TMenuItem;

  CaptionStr    : string;
  CaptionRect   : TRect;
  CaptionHeight : integer;
  CaptionWidth  : integer;

  HintStr       : string;
  HintRect      : TRect;
  HintHeight    : integer;
  HintWidth     : integer;

  ShortCutStr   : string;
  ShortCutRect  : TRect;
  ShortCutWidth : integer;
begin
  item := TMenuItem(sender);

  //Caption
  captionStr := item.Caption;
  captionRect := rect(0, 0, 0, 0);
  ACanvas.Font.Style := [fsBold];

  captionHeight := DrawText(ACanvas.Handle, PChar(captionStr), length(captionStr), captionRect, DT_CALCRECT or DT_LEFT or DT_EXTERNALLEADING);
  captionWidth  := captionRect.Right - captionRect.Left;

  //Shortcut:
  shortCutStr := ShortCutToText(item.ShortCut);
  if shortCutStr <> '' then
  begin
    if Item.Count = 0 then
      shortCutRect    := rect(0, 0, 0, 0)
    else
      shortCutRect    := rect(0, 0, 0, 0);
    DrawText(ACanvas.Handle, PChar(shortCutStr), length(shortCutStr), shortCutRect, DT_CALCRECT or DT_RIGHT or DT_EXTERNALLEADING);
    shortCutWidth  := shortCutRect.Right - shortCutRect.Left;
    inc(captionWidth, shortCutWidth + MarginX*2);
  end;

  //Hint:
  hintRect := rect(0, 0, 0, 0);
  hintStr := item.Hint;
  ACanvas.Font.Style := [];

  hintHeight := DrawText(ACanvas.Handle, PChar(hintStr), length(hintStr), hintRect, DT_CALCRECT or DT_LEFT or DT_EXTERNALLEADING);
  hintWidth  := hintRect.Right - hintRect.Left;

  width := Max(captionWidth, hintWidth) + MarginX*2;
  if item.GetImageList <> nil then
    inc(width, GutterWidth);

  height := captionHeight + hintHeight + MarginY*4;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuStyler.SeparatorDrawItem(Sender: TObject;ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var
  hintStr: string;
  item: TMenuItem;
  r: TRect;
  hasGutter: boolean;
begin
  item := TMenuItem(sender);
  hasGutter := item.GetImageList <> nil;

  //Background:
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := ColorScheme.SeperatorBackgroundColor;
  ACanvas.FillRect(ARect);

  //Lines:
  ACanvas.Pen.Color := ColorScheme.SeperatorLineColor;
  ACanvas.Polyline([point(ARect.Left, ARect.Bottom-2), point(ARect.Right, ARect.Bottom-2)]);
  ACanvas.Pen.Color := ColorScheme.ItemBackgroundColor;
  ACanvas.Polyline([point(ARect.Left, ARect.Bottom-1), point(ARect.Right, ARect.Bottom-1)]);

  //Text
  hintStr := item.Hint;
  if hintStr <> '' then
  begin
    //Text:
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Style := [fsBold];
    ACanvas.Font.Color := ColorScheme.FontColor;
    r.Left := ARect.Left + MarginX;
    if hasGutter then
      inc(r.Left, SeparatorLead);
    r.Right := ARect.Right - MarginX;
    r.Top := ARect.Top;
    r.Bottom := ARect.Bottom;
    DrawText(ACanvas.Handle, PChar(hintStr), length(hintStr), r, DT_LEFT or DT_EXTERNALLEADING or DT_SINGLELINE	or DT_VCENTER);
  end
  else if hasGutter then
  begin
   //Gutter
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := ColorScheme.GutterColor;
    r := ARect;
    r.Right := GutterWidth;
    ACanvas.FillRect(r);

    ACanvas.Pen.Color := ColorScheme.SeperatorLineColor;
    ACanvas.Polyline([point(r.Right, r.top), point(r.Right, r.Bottom)]);
  end;
end;
{------------------------------------------------------------------------------}
{
  Taken from JclGraphics.pas JEDI project  http://www.delphi-jedi.org
}
function TgtMenuStyler.FillGradient(DC: HDC; ARect: TRect;ColorCount: Integer; StartColor, EndColor: TColor;
  ADirection: TGradientDirection): Boolean;
var
  StartRGB: array [0..2] of Byte;
  RGBKoef: array [0..2] of Double;
  Brush: HBRUSH;
  AreaWidth, AreaHeight, I: Integer;
  ColorRect: TRect;
  RectOffset: Double;
begin
  RectOffset := 0;
  Result := False;
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
procedure TgtMenuStyler.InvalidateMenu(AMenu: TMenu);
begin
  if Assigned(AMenu) then
  begin
    AMenu.OwnerDraw := False;
    if Owner.InheritsFrom(TControl) then
    begin
      TControl(Owner).Visible := False;
      TControl(Owner).Visible := True;
      TControl(Owner).BringToFront;
    end;
    AMenu.OwnerDraw := True;
  end;
end;
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
procedure TgtMenuStyler.SetMenu(const Value: TMenu);
begin
  if Assigned(FMenu) then
  begin
    FMenu.RemoveFreeNotification(Self);
    UnHookEvents;
  end;

  FMenu := Value;

  if Assigned(FMenu) then
  begin
    FMenu.FreeNotification(Self);
    HookEvents;
    InvalidateMenu(FMenu);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuStyler.SetHookAllMenus(const Value: Boolean);
var
  i : Integer;
begin
  FHookAllMenus := Value;
  if FHookAllMenus then
    HookEvents
  else
    UnHookEvents;
  if FHookAllMenus then
    begin
      for i:= 0 to Pred(Owner.ComponentCount) do
      begin
        if Owner.Components[i].InheritsFrom(TMenu) then
          InvalidateMenu(TMenu(Owner.Components[i]));
      end
    end
    else
      InvalidateMenu(FMenu);
end;
{------------------------------------------------------------------------------}
procedure TgtMenuStyler.SetMenuStyle(const Value: TgtMenuStyle);
var
  i : Integer;
begin
  if FMenuStyle <> Value then
  begin
    FMenuStyle := Value;
    case FMenuStyle of
      msDefault     : ColorScheme.ColorSchemeDefault;
      msOfficeBlue  : ColorScheme.ColorSchemeOfficeBlue;
      msObsidian    : ColorScheme.ColorSchemeObsidian;
      msLuna        : ColorScheme.ColorSchemeLuna;
      msSilver      : ColorScheme.ColorSchemeSilver;
      msVista       : ColorScheme.ColorSchemeVista;
    end;
    if FHookAllMenus then
    begin
      for i:= 0 to Pred(Owner.ComponentCount) do
      begin
        if Owner.Components[i].InheritsFrom(TMenu) then
          InvalidateMenu(TMenu(Owner.Components[i]));
      end
    end
    else
      InvalidateMenu(FMenu);
  end;
end;
{------------------------------------------------------------------------------}



end.

