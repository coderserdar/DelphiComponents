unit DXSpriteEdit;
//(c)2006-7 Jaro Benes
//All Rights Reserved

{
Complex application for users of unDelphiX as component editor:

Supported:
 a) create sprite and store it into rersource.
 b) allow do change and create complex sprite (background or particle).
 c) default values for complex type too.
 d) direct link to image from dximagelist.

}
interface                   
{$INCLUDE DelphiXcfg.inc}
{$WARNINGS OFF}
uses
  Windows, SysUtils, Classes, Forms, Dialogs, Controls, ExtCtrls, StdCtrls,
  Graphics, DXSprite, DXDraws;

type
  {  TEdit  }

  TEdit = class(StdCtrls.TEdit) {injected class}   
  private
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
    function GetAsFloat: Double;
    procedure SetAsFloat(const Value: Double);
  published
  public
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
  end;

  {  TDelphiXWaveEditForm  }

  TDelphiXSpriteEditForm = class(TForm)
    Bevel2: TBevel;
    OKButton: TButton;
    CancelButton: TButton;
    ClearButton: TButton;
    Panel1: TPanel;
    EAlpha: TEdit;
    EAngle: TEdit;
    EAnimCount: TEdit;
    EAnimPos: TEdit;
    EAnimSpeed: TEdit;
    EAnimStart: TEdit;
    EHeight: TEdit;
    EMapHeight: TEdit;
    EMapWidth: TEdit;
    EWidth: TEdit;
    EX: TEdit;
    EY: TEdit;
    EZ: TEdit;
    LAlpha: TLabel;
    LAngle: TLabel;
    LAnimCount: TLabel;
    LAnimPos: TLabel;
    LAnimSpeed: TLabel;
    LAnimStart: TLabel;
    LHeight: TLabel;
    LMapHeight: TLabel;
    LMapWidth: TLabel;
    LWidth: TLabel;
    LX: TLabel;
    LY: TLabel;
    LZ: TLabel;
    Label1: TLabel;
    EBlendMode: TComboBox;
    eImage: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ECenterX: TEdit;
    ECenterY: TEdit;
    chbBlurImage: TCheckBox;
    chbAnimLooped: TCheckBox;
    chbMoved: TCheckBox;
    chbCollisioned: TCheckBox;
    chbPixelCheck: TCheckBox;
    chbTile: TCheckBox;
    chbVisible: TCheckBox;
    btnMapEdit: TButton;
    Label5: TLabel;
    ETexFilter: TComboBox;
    procedure eImageChange(Sender: TObject);
    procedure eImageExit(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure MapEditButtonClick(Sender: TObject);
  private
    FChanged: Boolean;
    FvkType: TSpriteType;
    FSprite: TSprite;
    //
    FDXImageList: TCustomDXImageList;
    FDXImageName: string;
    procedure FieldEnabler(SpriteType: TSpriteType);
  public
    procedure LoadDataToForm(AData: TPersistent);
    function SaveDataFromForm: TPersistent;
    property Sprite: TSprite read FSprite write FSprite;
  end;

var
  DelphiXSpriteEditForm: TDelphiXSpriteEditForm;

implementation

uses DXConsts, DXMapEdit, DXClass;

{$R *.DFM}

{ TEdit }

function TEdit.GetAsFloat: Double;
begin
  try
    Result := StrToFloat(Self.Text);
  except
    Result := 0;
  end;
end;

function TEdit.GetAsInteger: Integer;
begin
  try
    Result := StrToInt(Self.Text);
  except
    Result := 0;
  end;
end;

procedure TEdit.SetAsFloat(const Value: Double);
begin
  Self.Text := FloatToStr(Value)
end;

procedure TEdit.SetAsInteger(const Value: Integer);
begin
  Self.Text := IntToStr(Value)
end;

{ TDelphiXSpriteEditForm }

procedure TDelphiXSpriteEditForm.LoadDataToForm(AData: TPersistent);
  procedure LoadAsSprite;
  begin
    with AData as TSprite do begin
      chbCollisioned.Checked := Collisioned;
      chbMoved.Checked := Moved;
      chbVisible.Checked := Visible;
      EHeight.AsInteger := Height;
      EWidth.AsInteger := Width;
      EX.AsFloat := X;
      EY.AsFloat := Y;
      EZ.AsInteger := Z;
    end;
    Caption := 'Sprite Init Editor (TSprite)';
  end;
  procedure LoadAsImageSprite;
  var
    ii: Integer;
    pci: TPictureCollectionItem;
  begin
    LoadAsSprite;
    with AData as TImageSprite do begin
      EAnimCount.AsInteger := AnimCount;
      chbAnimLooped.Checked := AnimLooped;
      EAnimPos.AsFloat := AnimPos;
      EAnimSpeed.AsFloat := AnimSpeed;
      EAnimStart.AsInteger := AnimStart;
      ECenterX.AsFloat := CenterX;
      ECenterY.AsFloat := CenterY;
      chbPixelCheck.Checked := PixelCheck;
      chbTile.Checked := Tile;
      EAlpha.AsInteger := Alpha;
      EAngle.AsFloat := Angle;
      EBlendMode.ItemIndex := Ord(BlendMode);
      ETexFilter.ItemIndex := Ord(TextureFilter);
      EImage.Clear;
      if Assigned(DXImageList) then
        for ii := 0 to DXImageList.Items.Count - 1 do begin
          if DXImageList.Items[ii].Name = '' then
            EImage.Items.Add('_unnamed_' + IntToStr(ii))
          else
            EImage.Items.Add(DXImageList.Items[ii].Name);
        end;
      {contain image}
      pci := Image;
      {is attached}
      if Assigned(pci) then begin
        {search in imagelist}
        for ii := 0 to DXImageList.Items.Count - 1 do
          if DXImageList.Items[ii] = pci then begin
            EImage.ItemIndex := ii;
            Break;
          end;
      end
      else begin
        if DXImageName <> '' then begin
          ii := EImage.Items.IndexOf(DXImageName);
          if ii <> -1 then
            EImage.ItemIndex := ii;
        end;
      end;
    end;
    Caption := 'Sprite Init Editor (TImageSprite)';
  end;
  procedure LoadAsImageSpriteEx;
  begin
    LoadAsImageSprite;
    Caption := 'Sprite Init Editor (TImageSpriteEx)';
  end;
  procedure LoadAsBackgroundSprite;
  begin
    LoadAsImageSprite;
    with AData as TBackgroundSprite do begin
      EMapHeight.AsInteger := MapHeight;
      EMapWidth.AsInteger := MapWidth;
      chbTile.Checked := Tile;
    end;
    btnMapEdit.Enabled := eImage.ItemIndex <> -1;
    Caption := 'Sprite Init Editor (TBackgroundSprite)';
  end;
const
  cktypearr: array[TSpriteType] of Integer = (0, 2, 3, 1);
begin
  FSprite := TSprite(AData);
  FDXImageList := TSprite(AData).DXImageList;
  if AData is TBackgroundSprite then Fvktype := stBackgroundSprite
  else if AData is TImageSpriteEx then Fvktype := stImageSpriteEx
  else if AData is TImageSprite then Fvktype := stImageSprite
  else Fvktype := stSprite;

  FieldEnabler(Fvktype);
  try
    case Fvktype of
      stSprite: LoadAsSprite;
      stImageSprite: LoadAsImageSprite;
      stImageSpriteEx: LoadAsImageSpriteEx;
      stBackgroundSprite: LoadAsBackgroundSprite;
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

function TDelphiXSpriteEditForm.SaveDataFromForm: TPersistent;
var AData: TSprite;
  procedure SaveAsSprite;
  begin
    with AData as TSprite do begin
      Collisioned := chbCollisioned.Checked;
      Moved := chbMoved.Checked;
      Visible := chbVisible.Checked;
      Height := EHeight.AsInteger;
      Width := EWidth.AsInteger;
      X := EX.AsFloat;
      Y := EY.AsFloat;
      Z := EZ.AsInteger;
    end;
  end;
  procedure SaveAsImageSprite;
  begin
    SaveAsSprite;
    with AData as TImageSprite do begin
      AnimCount := EAnimCount.AsInteger;
      AnimLooped := chbAnimLooped.Checked;
      AnimPos := EAnimPos.AsFloat;
      AnimSpeed := EAnimSpeed.AsFloat;
      AnimStart := EAnimStart.AsInteger;
      PixelCheck := chbPixelCheck.Checked;
      Tile := chbTile.Checked;
      Alpha := EAlpha.AsInteger;
      Angle := EAngle.AsFloat;
      CenterX := ECenterX.AsFloat;
      CenterY := ECenterY.AsFloat;
      BlendMode := TRenderType(EBlendMode.ItemIndex);
      TextureFilter := TD2DTextureFilter(ETexFilter.ItemIndex);
      BlurImage := chbBlurImage.Checked;
      if Assigned(DXImageList) then
        if DXImageName <> eImage.Text then
          if eImage.ItemIndex <> -1 then begin
            {DX}Image := DXImageList.Items[eImage.ItemIndex];
            DXImageName := DXImageList.Items[eImage.ItemIndex].Name;
          end;
    end;
  end;
  procedure SaveAsImageSpriteEx;
  begin
    SaveAsImageSprite;
  end;
  procedure SaveAsBackgroundSprite;
  begin
    SaveAsImageSprite;
    with AData as TBackgroundSprite do begin
      MapHeight := EMapHeight.AsInteger;
      Tile := chbTile.Checked;
      MapWidth := EMapWidth.AsInteger;
    end;
  end;
begin
  Result := nil;
  try
    AData := FSprite;
    case Fvktype of
      stSprite: SaveAsSprite;
      stImageSprite: SaveAsImageSprite;
      stImageSpriteEx: SaveAsImageSpriteEx;
      stBackgroundSprite: SaveAsBackgroundSprite;
    end;
    Result := AData;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TDelphiXSpriteEditForm.OKButtonClick(Sender: TObject);
begin
  FChanged := True;
  if FChanged then
  begin
    Tag := 1;
  end;

  Close;
end;

procedure TDelphiXSpriteEditForm.FieldEnabler(SpriteType: TSpriteType);
var I: Integer;
begin
  ECenterX.Enabled := (SpriteType in [stImageSprite..stBackgroundSprite]);
  ECenterY.Enabled := (SpriteType in [stImageSprite..stBackgroundSprite]);
  EAlpha.Enabled := (SpriteType in [stImageSprite..stBackgroundSprite]);
  EAngle.Enabled := (SpriteType in [stImageSprite..stBackgroundSprite]);
  EBlendMode.Enabled := (SpriteType in [stImageSprite..stBackgroundSprite]);
  EAnimCount.Enabled := (SpriteType in [stImageSprite, stImageSpriteEx]);
  chbAnimLooped.Enabled := (SpriteType in [stImageSprite, stImageSpriteEx]);
  EAnimPos.Enabled := (SpriteType in [stImageSprite, stImageSpriteEx]);
  EAnimSpeed.Enabled := (SpriteType in [stImageSprite, stImageSpriteEx]);
  EAnimStart.Enabled := (SpriteType in [stImageSprite, stImageSpriteEx]);
  chbCollisioned.Enabled := (SpriteType in [stSprite..stBackgroundSprite]);
  chbBlurImage.Enabled := (SpriteType in [stImageSprite, stImageSpriteEx]);
  EHeight.Enabled := (SpriteType in [stSprite..stBackgroundSprite]);
  EImage.Enabled := (SpriteType in [stImageSprite..stBackgroundSprite]);
  EMapHeight.Enabled := (SpriteType in [stBackgroundSprite]);
  EMapWidth.Enabled := (SpriteType in [stBackgroundSprite]);
  chbMoved.Enabled := (SpriteType in [stSprite..stBackgroundSprite]);
  chbPixelCheck.Enabled := (SpriteType in [stImageSprite, stImageSpriteEx]);
  chbTile.Enabled := (SpriteType in [stImageSprite..stBackgroundSprite]);
  chbVisible.Enabled := (SpriteType in [stSprite..stBackgroundSprite]);
  chbBlurImage.Enabled := (SpriteType in [stImageSprite..stBackgroundSprite]);
  EWidth.Enabled := (SpriteType in [stSprite..stBackgroundSprite]);
  EX.Enabled := (SpriteType in [stSprite..stBackgroundSprite]);
  EY.Enabled := (SpriteType in [stSprite..stBackgroundSprite]);
  EZ.Enabled := (SpriteType in [stSprite..stBackgroundSprite]);
  ETexFilter.Enabled := (SpriteType in [stImageSprite..stBackgroundSprite]);
  for I := 0 to ComponentCount - 1 do begin
    if (Components[I] is TEdit) then with (Components[I] as TEdit) do
        if Enabled then Color := clWindow else Color := clBtnFace;
    if (Components[I] is TComboBox) then with (Components[I] as TComboBox) do
        if Enabled then Color := clWindow else Color := clBtnFace;
  end
end;

procedure TDelphiXSpriteEditForm.MapEditButtonClick(Sender: TObject);
{only for editing maps for TBackgroudSprite}
var
  I{, J}: Integer;
  Q: TBackgroundSprite;
begin
  DelphiXMapEditForm := TDelphiXMapEditForm.Create(nil);
  try
    DelphiXMapEditForm.DXImageList := FDXImageList; {local instance copy}
    DelphiXMapEditForm.DXBackgroundSprite := TBackgroundSprite(FSprite);
    {vymaz combo a natahni jmena}
    DelphiXMapEditForm.ImageToSet.Items.Clear;
    DelphiXMapEditForm.ImageToSet.Items.Assign(EImage.Items);
    DelphiXMapEditForm.ImageToSet.ItemIndex := EImage.ItemIndex;
    DelphiXMapEditForm.PicturesToChip.Visible := DelphiXMapEditForm.LoadSplittedImage{$IFNDEF VER4UP}(False){$ENDIF};
    {nepovolit zmenu, pokud byla udelana uz na hlavnim formu}
    DelphiXMapEditForm.ImageToSet.Enabled := DelphiXMapEditForm.ImageToSet.ItemIndex = -1;
    if Assigned(FSprite) then begin
      Q := TBackgroundSprite(FSprite);
      if not Q.IsMapEmpty then begin
        DelphiXMapEditForm.eMapSizeX.Value := Q.MapWidth;
        DelphiXMapEditForm.eMapSizeY.Value := Q.MapHeight;
        {velikost podle dlazdice v listu}
        I := DelphiXMapEditForm.ImageToSet.ItemIndex;
        if I <> -1 then begin
          DelphiXMapEditForm.EWidth.AsInteger := DelphiXMapEditForm.DXImageList.Items[I].Width; ;
          DelphiXMapEditForm.EHeight.AsInteger := DelphiXMapEditForm.DXImageList.Items[I].Height;
        end;
        DelphiXMapEditForm.ResizeMapArea;
      end
      else
        DelphiXMapEditForm.ResizeMapArea;
    end;
    {pokud je regulerni jmeno obrazku na spritu, vyber ho}
    with DelphiXMapEditForm.ImageToSet do
      if FDXImageName <> '' then
        ItemIndex := Items.IndexOf(FDXImageName);
    {nastav tam jeste obrabeny sprite}
    DelphiXMapEditForm.DXBackGroundSprite := TBackgroundSprite(FSprite);
    DelphiXMapEditForm.MapArea.DefaultColWidth := DelphiXMapEditForm.EWidth.AsInteger;
    DelphiXMapEditForm.MapArea.DefaultRowHeight := DelphiXMapEditForm.EHeight.AsInteger;
    {a volej dialog}
    DelphiXMapEditForm.eMapSizeX.OnChange := DelphiXMapEditForm.BtnSetSizeClick;
    DelphiXMapEditForm.eMapSizeY.OnChange := DelphiXMapEditForm.BtnSetSizeClick;
    if DelphiXMapEditForm.ShowModal = mrOK then begin
      {pokud je vse OK, uloz mapu ke spritu}
      if Assigned(DelphiXMapEditForm.DXBackGroundSprite) then begin
        FDXImageName := '';
        if DelphiXMapEditForm.ImageToSet.ItemIndex <> -1 then
          FDXImageName := FDXImageList.Items[DelphiXMapEditForm.ImageToSet.ItemIndex].Name;
        EMapHeight.AsInteger := DelphiXMapEditForm.eMapSizeY.Value;
        EMapWidth.AsInteger := DelphiXMapEditForm.eMapSizeX.Value;
      end;
    end;
  finally
    DelphiXMapEditForm.Free;
    DelphiXMapEditForm := nil;
  end;
end;

procedure TDelphiXSpriteEditForm.eImageExit(Sender: TObject);
begin
  if eImage.ItemIndex <> -1 then begin
    EWidth.AsInteger := FDXImageList.Items[eImage.ItemIndex].Width;
    EHeight.AsInteger := FDXImageList.Items[eImage.ItemIndex].Height;
  end;
end;

procedure TDelphiXSpriteEditForm.eImageChange(Sender: TObject);
begin
  if FvkType = stBackgroundSprite then
    btnMapEdit.Enabled := eImage.ItemIndex <> -1
  else
    btnMapEdit.Enabled := False;
end;

end.
