unit DXSprite;

interface

{$INCLUDE DelphiXcfg.inc}

uses
  Windows, SysUtils, Classes, Graphics, DXClass, DXDraws,
  {$IFDEF VER9UP} Types,{$ENDIF}
{$IFDEF StandardDX}
  DirectDraw;
{$ELSE}
  DirectX;
{$ENDIF}

type

  {  ESpriteError  }

  ESpriteError = class(Exception);

  {  TSprite  }

  TSpriteEngine = class;

  TSprite = class;
  TCollisionEvent = procedure(Sender: TObject; var Done: Boolean) of object;
  TMoveEvent = procedure(Sender: TObject; var MoveCount: Integer) of object;
  TDrawEvent = procedure(Sender: TObject) of object;
  TGetImage = procedure(Sender: TObject; var Image: TPictureCollectionItem) of object;

  TSprite = class(TPersistent)
  private
    FEngine: TSpriteEngine;
    FParent: TSprite;
    FList: TList;
    FDeaded: Boolean;
    FDrawList: TList;
    FCollisioned: Boolean;
    FMoved: Boolean;
    FVisible: Boolean;
    FX: Double;
    FY: Double;
    FZ: Integer;
    FWidth: Integer;
    FHeight: Integer;
    {$IFDEF Ver4Up}
    FSelected: Boolean;
    FGroupNumber: Integer;
    {$ENDIF}
    FCaption: string;
    FTag: Integer;

    FDXImageList: TCustomDXImageList;
    FDXImage: TPictureCollectionItem;
    FDXImageName: string;

    FOnDraw: TDrawEvent;
    FOnMove: TMoveEvent;
    FOnCollision: TCollisionEvent;
    FOnGetImage: TGetImage;
    procedure Add(Sprite: TSprite);
    procedure Remove(Sprite: TSprite);
    procedure AddDrawList(Sprite: TSprite);
    procedure Collision2;
    procedure Draw; {$IFDEF VER9UP}inline;{$ENDIF}
    function GetClientRect: TRect;
    function GetCount: Integer;
    function GetItem(Index: Integer): TSprite;
    function GetWorldX: Double; {$IFDEF VER9UP}inline;{$ENDIF}
    function GetWorldY: Double; {$IFDEF VER9UP}inline;{$ENDIF}
    procedure SetZ(Value: Integer);
  protected
    procedure DoCollision(Sprite: TSprite; var Done: Boolean); virtual;
    procedure DoDraw; virtual;
    procedure DoMove(MoveCount: Integer); virtual;
    function GetBoundsRect: TRect; virtual;
    function TestCollision(Sprite: TSprite): Boolean; virtual;
    {$IFDEF Ver4Up}
    procedure SetGroupNumber(AGroupNumber: Integer); virtual;
    procedure SetSelected(ASelected: Boolean); virtual;
    {$ENDIF}
  public
    constructor Create(AParent: TSprite); virtual;
    destructor Destroy; override;
    procedure Clear;
    function Collision: Integer;
    procedure Dead;
    procedure Move(MoveCount: Integer);
    procedure ReAnimate(MoveCount: Integer); virtual;
    function GetSpriteAt(X, Y: Integer): TSprite; 
    property BoundsRect: TRect read GetBoundsRect;
    property ClientRect: TRect read GetClientRect;
    property Count: Integer read GetCount;
    property Engine: TSpriteEngine read FEngine;
    property Items[Index: Integer]: TSprite read GetItem; default;
    property Deaded: Boolean read FDeaded;
    property Parent: TSprite read FParent;
    property WorldX: Double read GetWorldX;
    property WorldY: Double read GetWorldY;
    // Group handling support
    {$IFDEF Ver4Up}  // if GroupNumber < 0 then no group is assigned
    property GroupNumber: Integer read FGroupNumber write SetGroupNumber;
    property Selected: Boolean read FSelected write SetSelected;
    {$ENDIF}
    procedure Assign(Source: TPersistent); override;
  published
    property Height: Integer read FHeight write FHeight;
    property Moved: Boolean read FMoved write FMoved;
    property Visible: Boolean read FVisible write FVisible;
    property Width: Integer read FWidth write FWidth;
    property X: Double read FX write FX;
    property Y: Double read FY write FY;
    property Z: Integer read FZ write SetZ;
    property Collisioned: Boolean read FCollisioned write FCollisioned;
    property Tag: Integer read FTag write FTag;
    property Caption: string read FCaption write FCaption;

    property DXImageList: TCustomDXImageList read FDXImageList write FDXImageList;
    property DXImageName: string read FDXImageName write FDXImageName;

    property OnDraw: TDrawEvent read FOnDraw write FOnDraw;
    property OnMove: TMoveEvent read FOnMove write FOnMove;
    property OnCollision: TCollisionEvent read FOnCollision write FOnCollision;
    property OnGetImage: TGetImage read FOnGetImage write FOnGetImage;
  end;

  TSpriteClass = class of TSprite;

  {  TImageSprite  }

  TImageSprite = class(TSprite)
  private
    FAnimCount: Integer;
    FAnimLooped: Boolean;
    FAnimPos: Double;
    FAnimSpeed: Double;
    FAnimStart: Integer;
    FPixelCheck: Boolean;
    FTile: Boolean;
    FTransparent: Boolean;
    FAngle: Single;
    FAlpha: Integer;
    FBlendMode: TRenderType;
    FCenterX: Double;
    FCenterY: Double;
    FBlurImageArr: TBlurImageArr;
    FBlurImage: Boolean;
    FMirrorFlip: TRenderMirrorFlipSet;
    FTextureFilter: TD2DTextureFilter;
    function GetDrawImageIndex: Integer;
    function GetDrawRect: TRect;
    function ImageCollisionTest(suf1, suf2: TDirectDrawSurface;
      const rect1, rect2: TRect; x1, y1, x2, y2: Integer;
      DoPixelCheck: Boolean): Boolean;
    function StoreCenterX: Boolean;
    function StoreCenterY: Boolean;
    function StoreAlpha: Boolean;
    procedure SetBlurImage(const Value: Boolean);
    procedure SetBlurImageArr(const Value: TBlurImageArr);
    function GetImage: TPictureCollectionItem;
    procedure SetMirrorFlip(const Value: TRenderMirrorFlipSet);
    procedure ReadMirrorFlip(Reader: TReader);
    procedure WriteMirrorFlip(Writer: TWriter);
  protected
    {accessed methods}
    procedure ReadAlpha(Reader: TReader);
    procedure ReadAngle(Reader: TReader);
    procedure ReadAnimCount(Reader: TReader);
    procedure ReadAnimLooped(Reader: TReader);
    procedure ReadAnimPos(Reader: TReader);
    procedure ReadAnimSpeed(Reader: TReader);
    procedure ReadAnimStart(Reader: TReader);
    procedure ReadBlendMode(Reader: TReader);
    procedure ReadCenterX(Reader: TReader);
    procedure ReadCenterY(Reader: TReader);
    procedure ReadPixelCheck(Reader: TReader);
    procedure ReadTile(Reader: TReader);
    procedure ReadBlurImage(Reader: TReader);
    procedure ReadTextureFilter(Reader: TReader);
    procedure WriteAlpha(Writer: TWriter);
    procedure WriteAngle(Writer: TWriter);
    procedure WriteAnimCount(Writer: TWriter);
    procedure WriteAnimLooped(Writer: TWriter);
    procedure WriteAnimPos(Writer: TWriter);
    procedure WriteAnimSpeed(Writer: TWriter);
    procedure WriteAnimStart(Writer: TWriter);
    procedure WriteBlendMode(Writer: TWriter);
    procedure WriteCenterX(Writer: TWriter);
    procedure WriteCenterY(Writer: TWriter);
    procedure WritePixelCheck(Writer: TWriter);
    procedure WriteTile(Writer: TWriter);
    procedure WriteBlurImage(Writer: TWriter);
    procedure WriteTextureFilter(Writer: TWriter);
    {own store of properties}
    procedure DefineProperties(Filer: TFiler); override;
    procedure LoadImage; virtual;
    procedure DoDraw; override;
    procedure DoMove(MoveCount: Integer); override;
    function GetBoundsRect: TRect; override;
    function TestCollision(Sprite: TSprite): Boolean; override;
    procedure SetImage(AImage: TPictureCollectionItem); virtual;
  public
    constructor Create(AParent: TSprite); override;
    procedure Assign(Source: TPersistent); override;
    procedure ReAnimate(MoveCount: Integer); override;
    property Image: TPictureCollectionItem read GetImage write SetImage;
    property BlurImageArr: TBlurImageArr read FBlurImageArr write SetBlurImageArr;
    {un-published property}
    property BlendMode: TRenderType read FBlendMode write FBlendMode default rtDraw;
    property Angle: Single read FAngle write FAngle stored StoreAlpha;
    property Alpha: Integer read FAlpha write FAlpha default $FF;
    property CenterX: Double read FCenterX write FCenterX stored StoreCenterX;
    property CenterY: Double read FCenterY write FCenterY stored StoreCenterY;
    property AnimCount: Integer read FAnimCount write FAnimCount default 0;
    property AnimLooped: Boolean read FAnimLooped write FAnimLooped default False;
    property AnimPos: Double read FAnimPos write FAnimPos;
    property AnimSpeed: Double read FAnimSpeed write FAnimSpeed;
    property AnimStart: Integer read FAnimStart write FAnimStart default 0;
    property PixelCheck: Boolean read FPixelCheck write FPixelCheck default False;
    property Tile: Boolean read FTile write FTile default False;
    property BlurImage: Boolean read FBlurImage write SetBlurImage default False;
    property MirrorFlip: TRenderMirrorFlipSet read FMirrorFlip write SetMirrorFlip default [];
    property TextureFilter: TD2DTextureFilter read FTextureFilter write FTextureFilter default D2D_POINT;
  published
    property DXImageList;
    property DXImageName;

    property OnDraw;
    property OnMove;
    property OnCollision;
    property OnGetImage;
  end;

  {  TImageSpriteEx  }

  TImageSpriteEx = class(TImageSprite)
  end{$IFDEF VER9UP}deprecated{$IFDEF VER14UP} 'Use for backward compatibility only or replace by TImageSprite instead...'{$ENDIF}{$ENDIF};

  {  TBackgroundSprite  }

  PMapType = ^TMapType;
  TMapType = packed record
    MapChip: Integer; {image chip as number}
    //ImageName: string[127];
    CollisionChip: Boolean; {is collision brick}
    CollisionRect: TRect; {dirty vollision area, can be smaller or bigger than silhouette}
    Overlap: Integer; {for pulse image, like zoom etc.}
    AnimLooped: Boolean; {chip can be live}
    AnimStart, AnimCount: Integer;
    AnimSpeed, AnimPos: Double; {phase of picture by one map chip}
    Rendered: TRenderType; {can be blended}
    Alpha: Byte; {and blend level}
    Angle: Single;
    CenterX, CenterY: Double;
    MirrorFlip: TRenderMirrorFlipSet;
    TextureFilter: TD2DTextureFilter;
    Tag: Integer; {for application use}
  end;

  TBackgroundSprite = class(TImageSprite)
  private
    FMap: Pointer;
    FMapWidth: Integer;
    FMapHeight: Integer;

    FChipsRect: TRect;
    FChipsPatternIndex: Integer;
    function GetCollisionMapItem(X, Y: Integer): Boolean;
    function GetChip(X, Y: Integer): Integer;
    procedure SetChip(X, Y: Integer; Value: Integer);
    procedure SetCollisionMapItem(X, Y: Integer; Value: Boolean);
    procedure SetMapHeight(Value: Integer);
    procedure SetMapWidth(Value: Integer);

    function GetCollisionRectItem(X, Y: Integer): TRect;
    function GetMap(X, Y: Integer): TMapType;
    function GetTagMap(X, Y: Integer): Integer;
    procedure SetCollisionRectItem(X, Y: Integer; Value: TRect);
    procedure SetMap(X, Y: Integer; Value: TMapType);
    procedure SetTagMap(X, Y, Value: Integer);
    function GetOverlap(X, Y: Integer): Integer;
    procedure SetOverlap(X, Y: Integer; const Value: Integer);
  protected
    procedure ReadMapData(Stream: TStream);
    procedure WriteMapData(Stream: TStream);
    procedure DoDraw; override;
    function GetBoundsRect: TRect; override;
    function TestCollision(Sprite: TSprite): Boolean; override;
    procedure SetImage(Img: TPictureCollectionItem); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AParent: TSprite); override;
    destructor Destroy; override;
    procedure ChipsDraw(Image: TPictureCollectionItem; X, Y, PatternIndex: Integer);
    procedure SetMapSize(AMapWidth, AMapHeight: Integer);
    function IsMapEmpty: Boolean;
    property Chips[X, Y: Integer]: Integer read GetChip write SetChip;
    property CollisionMap[X, Y: Integer]: Boolean read GetCollisionMapItem write SetCollisionMapItem;
    property CollisionRect[X, Y: Integer]: TRect read GetCollisionRectItem write SetCollisionRectItem;
    property Overlap[X, Y: Integer]: Integer read GetOverlap write SetOverlap;
    property TagMap[X, Y: Integer]: Integer read GetTagMap write SetTagMap;
    property Map[X, Y: Integer]: TMapType read GetMap write SetMap;
    procedure Assign(Source: TPersistent); override;
    property ChipsRect: TRect read FChipsRect write FChipsRect;
    property ChipsPatternIndex: Integer read FChipsPatternIndex write FChipsPatternIndex default 0;
    {un-published property}
    property MapHeight: Integer read FMapHeight write SetMapHeight;
    property MapWidth: Integer read FMapWidth write SetMapWidth;
  published
    property DXImageList;
    property DXImageName;

    property OnDraw;
    property OnMove;
    property OnCollision;
    property OnGetImage;
  end;

  {  forward class  }

  TCustomDXSpriteEngine = class;

  {  TSpriteEngine  }

  TSpriteEngine = class(TSprite)
  private
    FOwner: TCustomDXSpriteEngine;
    FAllCount: Integer;
    FCollisionCount: Integer;
    FCollisionDone: Boolean;
    FCollisionRect: TRect;
    FCollisionSprite: TSprite;
    FDeadList: TList;
    FDrawCount: Integer;
    FSurface: TDirectDrawSurface;
    FSurfaceRect: TRect;
{$IFDEF Ver4Up}
    FObjectsSelected: Boolean;
    FGroupCount: Integer;
    FGroups: array of Tlist;
    FCurrentSelected: Tlist;
{$ENDIF}
  protected
    procedure SetSurface(Value: TDirectDrawSurface); virtual;
{$IFDEF Ver4Up}
    procedure SetGroupCount(AGroupCount: Integer); virtual;
    function GetGroup(Index: Integer): Tlist; virtual;
{$ENDIF}
  public
    constructor Create(AParent: TSprite); override;
    destructor Destroy; override;
    procedure Dead;
    procedure Draw;
    property AllCount: Integer read FAllCount;
    property DrawCount: Integer read FDrawCount;
    property Surface: TDirectDrawSurface read FSurface write SetSurface;
    property SurfaceRect: TRect read FSurfaceRect;

    // Extended Sprite Engine
    procedure Collisions;

    // Group handling support
    {$IFDEF Ver4Up}
    procedure ClearCurrent;
    procedure ClearGroup(GroupNumber: Integer);
    procedure GroupToCurrent(GroupNumber: Integer; Add: Boolean = False);
    procedure CurrentToGroup(GroupNumber: Integer; Add: Boolean = False);
    procedure GroupSelect(const Area: TRect; Filter: array of TSpriteClass; Add: Boolean = False); overload;
    procedure GroupSelect(const Area: TRect; Add: Boolean = False); overload;
    function Select(Point: TPoint; Filter: array of TSpriteClass; Add: Boolean = False): Tsprite; overload;
    function Select(Point: TPoint; Add: Boolean = False): Tsprite; overload;

    property CurrentSelected: TList read fCurrentSelected;
    property ObjectsSelected: Boolean read fObjectsSelected;
    property Groups[Index: Integer]: Tlist read GetGroup;
    property GroupCount: Integer read fGroupCount write SetGroupCount;
    {$ENDIF}
  end;

  {  EDXSpriteEngineError  }

  EDXSpriteEngineError = class(Exception);

  TSpriteCollection = class;

  {  TSpriteType  }

  TSpriteType = (stSprite, stImageSprite, stImageSpriteEx, stBackgroundSprite);

  {  TSpriteCollectionItem  }

  TSpriteCollectionItem = class(THashCollectionItem)
  private
    FOwner: TPersistent;
    FOwnerItem: TSpriteEngine;
    FSpriteType: TSpriteType;
    FSprite: TSprite;
    procedure Finalize;
    procedure Initialize;
    function GetSpriteCollection: TSpriteCollection;
    procedure SetSprite(const Value: TSprite);
    procedure SetOnCollision(const Value: TCollisionEvent);
    procedure SetOnDraw(const Value: TDrawEvent);
    procedure SetOnMove(const Value: TMoveEvent);
    function GetSpriteType: TSpriteType;
    procedure SetSpriteType(const Value: TSpriteType);
    function GetOnCollision: TCollisionEvent;
    function GetOnDraw: TDrawEvent;
    function GetOnMove: TMoveEvent;
    function GetOnGetImage: TGetImage;
    procedure SetOnGetImage(const Value: TGetImage);
    function GetImageList: TCustomDXImageList;
    procedure SetImageList(const Value: TCustomDXImageList);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property SpriteCollection: TSpriteCollection read GetSpriteCollection;
    function Clone(NewName: string): TSprite;
  published
    {published property of sprite}
    property KindSprite: TSpriteType read GetSpriteType write SetSpriteType;
    property ImageList: TCustomDXImageList read GetImageList write SetImageList;
    property Sprite: TSprite read FSprite write SetSprite;
    {published events of sprite}
    property OnDraw: TDrawEvent read GetOnDraw write SetOnDraw;
    property OnMove: TMoveEvent read GetOnMove write SetOnMove;
    property OnCollision: TCollisionEvent read GetOnCollision write SetOnCollision;
    property OnGetImage: TGetImage read GetOnGetImage write SetOnGetImage;
  end;

  {  ESpriteCollectionError  }

  ESpriteCollectionError = class(Exception);

  {  TSpriteCollection  }

  TSCInitialize = procedure(Owner: TSpriteEngine) of object;
  TSCFinalize = procedure(Owner: TSpriteEngine) of object;

  TSpriteCollection = class(THashCollection)
  private
    FInitializeFlag: Boolean;
    FOwner: TPersistent;
    FOwnerItem: TSpriteEngine;
    FOnInitialize: TSCInitialize;
    FOnFinalize: TSCFinalize;
    function GetItem(Index: Integer): TSpriteCollectionItem;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    function Initialized: Boolean;
    function Find(const Name: string): TSpriteCollectionItem;
    function Add: TSpriteCollectionItem;
    procedure Finalize;
    function Initialize(DXSpriteEngine: TSpriteEngine): Boolean;
    property Items[Index: Integer]: TSpriteCollectionItem read GetItem; default;
  published
    property OnInitialize: TSCInitialize read FOnInitialize write FOnInitialize;
    property OnFinalize: TSCFinalize read FOnFinalize write FOnFinalize;
  end;

  {  TCustomDXSpriteEngine  }

  TCustomDXSpriteEngine = class(TComponent)
  private
    FDXDraw: TCustomDXDraw;
    FEngine: TSpriteEngine;
    FItems: TSpriteCollection;
    procedure DXDrawNotifyEvent(Sender: TCustomDXDraw; NotifyType: TDXDrawNotifyType);
    procedure SetDXDraw(Value: TCustomDXDraw);
    procedure SetItems(const Value: TSpriteCollection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Dead;
    procedure Draw;
    procedure Move(MoveCount: Integer);
    procedure Clone(const Amount: Word; const BaseNameOfSprite: string);
    function ForEach(PrefixNameOdSprite: string; var Names: TStringList): Boolean;
    property DXDraw: TCustomDXDraw read FDXDraw write SetDXDraw;
    property Engine: TSpriteEngine read FEngine;
    property Items: TSpriteCollection read FItems write SetItems;
  end;

  {  TDXSpriteEngine  }

  TDXSpriteEngine = class(TCustomDXSpriteEngine)
    property Items;
  published
    property DXDraw;
  end;

function Mod2(i, i2: Integer): Integer; {$IFDEF VER9UP}inline;{$ENDIF}
function Mod2f(i: Double; i2: Integer): Double; {$IFDEF VER9UP}inline;{$ENDIF}
function DefaultMapChip(iMapChip: Integer = -1; iCollisionChip: Boolean = False): TMapType; {$IFDEF VER9UP}inline;{$ENDIF}

implementation

uses DXConsts, TypInfo;

const
  SSpriteNotFound = 'Sprite not found';
  SSpriteDuplicateName = 'Item duplicate name "%s" error';

function DefaultMapChip(iMapChip: Integer = -1; iCollisionChip: Boolean = False): TMapType;
begin
  FillChar(Result, SizeOf(Result), 0);
  with Result do
  begin
    MapChip := iMapChip; {image chip as number}
    CollisionChip := iCollisionChip; {is collision brick}
//    CollisionRect: TRect; {dirty vollision area, can be smaller or bigger than silhouette}
//    Overlap: Integer; {for pulse image, like zoom etc.}
//    AnimLooped: Boolean; {chip can be live}
//    AnimStart, AnimCount: Integer;
//    AnimSpeed, AnimPos: Double; {phase of picture by one map chip}
    Rendered := rtDraw; {can be blended}
    Alpha := $FF; {and blend level}
    Angle := 0;
    CenterX := 0.5;
    CenterY := 0.5;
    TextureFilter := D2D_POINT;
//    Tag: Integer; {for application use}
  end;
end;

function Mod2(i, i2: Integer): Integer;
begin
  Result := i mod i2;
  if Result < 0 then
    Result := i2 + Result;
end;

function Mod2f(i: Double; i2: Integer): Double;
begin
  if i2 = 0 then
    Result := i
  else
  begin
    Result := i - Round(i / i2) * i2;
    if Result < 0 then
      Result := i2 + Result;
  end;
end;

{  TSprite  }

constructor TSprite.Create(AParent: TSprite);
begin
  inherited Create;
{$IFDEF Ver4Up}
  fGroupnumber := -1;
{$ENDIF}
  FParent := AParent;
  if FParent <> nil then
  begin
    FParent.Add(Self);
    if FParent is TSpriteEngine then
      FEngine := TSpriteEngine(FParent)
    else
      FEngine := FParent.Engine;
    Inc(FEngine.FAllCount);
  end;

  FCollisioned := True;
  FMoved := True;
  FVisible := True;
end;

destructor TSprite.Destroy;
begin
{$IFDEF Ver4Up}
  GroupNumber := -1;
  Selected := False;
{$ENDIF}
  Clear;
  if FParent <> nil then
  begin
    Dec(FEngine.FAllCount);
    FParent.Remove(Self);
    FEngine.FDeadList.Remove(Self);
  end;
  FList.Free;
  FDrawList.Free;
  inherited Destroy;
end;

{$IFDEF Ver4Up}

procedure TSprite.SetGroupNumber(AGroupNumber: Integer);
begin
  if (AGroupNumber <> GroupNumber) and (Engine <> nil) then
  begin
    if Groupnumber >= 0 then
      Engine.Groups[GroupNumber].Remove(self);
    if AGroupNumber >= 0 then
      Engine.Groups[AGroupNumber].Add(self);
  end;
end; {SetGroupNumber}

procedure TSprite.SetSelected(ASelected: Boolean);
begin
  if (ASelected <> fSelected) and (Engine <> nil) then
  begin
    fSelected := ASelected;
    if Selected then
      Engine.CurrentSelected.Add(self)
    else
      Engine.CurrentSelected.Remove(self);
    Engine.fObjectsSelected := Engine.CurrentSelected.count <> 0;
  end;
end;
{$ENDIF}

procedure TSprite.Add(Sprite: TSprite);
begin
  if FList = nil then
  begin
    FList := TList.Create;
    FDrawList := TList.Create;
  end;
  FList.Add(Sprite);
  AddDrawList(Sprite);
end;

procedure TSprite.Remove(Sprite: TSprite);
begin
  FList.Remove(Sprite);
  FDrawList.Remove(Sprite);
  if FList.Count = 0 then
  begin
    FList.Free;
    FList := nil;
    FDrawList.Free;
    FDrawList := nil;
  end;
end;

procedure TSprite.AddDrawList(Sprite: TSprite);
var
  L, H, I, C: Integer;
begin
  L := 0;
  H := FDrawList.Count - 1;
  while L <= H do
  begin
    I := (L + H) div 2;
    C := TSprite(FDrawList[I]).Z - Sprite.Z;
    if C < 0 then
      L := I + 1
    else
      H := I - 1;
  end;
  FDrawList.Insert(L, Sprite);
end;

procedure TSprite.Clear;
begin
  while Count > 0 do
    Items[Count - 1].Free;
end;

function TSprite.Collision: Integer;
var
  i: Integer;
begin
  Result := 0;
  if (FEngine <> nil) and (not FDeaded) and (Collisioned) then
  begin
    with FEngine do
    begin
      FCollisionCount := 0;
      FCollisionDone := False;
      FCollisionRect := Self.BoundsRect;
      FCollisionSprite := Self;

      for i := 0 to Count - 1 do
        Items[i].Collision2;

      Result := FCollisionCount;
    end;
  end;
end;

procedure TSprite.Collision2;
var
  i: Integer;
begin
  if Collisioned then
  begin
    if (Self <> FEngine.FCollisionSprite) and OverlapRect(BoundsRect,
      FEngine.FCollisionRect) and FEngine.FCollisionSprite.TestCollision(Self) and
      TestCollision(FEngine.FCollisionSprite) then
    begin
      Inc(FEngine.FCollisionCount);
      FEngine.FCollisionSprite.DoCollision(Self, FEngine.FCollisionDone);
      if (not FEngine.FCollisionSprite.Collisioned) or
        (FEngine.FCollisionSprite.FDeaded) then
      begin
        FEngine.FCollisionDone := True;
      end;
    end;
    if FEngine.FCollisionDone then
      Exit;
    for i := 0 to Count - 1 do
      Items[i].Collision2;
  end;
end;

procedure TSprite.Dead;
begin
  if (FEngine <> nil) and (not FDeaded) then
  begin
    FDeaded := True;
    FEngine.FDeadList.Add(Self);
  end;
end;

procedure TSprite.DoMove(MoveCount: Integer);
begin
  if AsSigned(FOnMove) then
    FOnMove(Self, MoveCount);
end;

procedure TSprite.DoDraw;
begin
  if AsSigned(FOnDraw) then
    FOnDraw(Self);
end;

procedure TSprite.DoCollision(Sprite: TSprite; var Done: Boolean);
begin
  if AsSigned(FOnCollision) then
    FOnCollision(Sprite, Done);
end;

function TSprite.TestCollision(Sprite: TSprite): Boolean;
begin
  Result := True;
end;

procedure TSprite.Move(MoveCount: Integer);
var
  i: Integer;
begin
  if FMoved then
  begin
    DoMove(MoveCount); ReAnimate(MoveCount);
    for i := 0 to Count - 1 do
      Items[i].Move(MoveCount);
  end;
end;

procedure TSprite.Draw;
var
  i: Integer;
begin
  if FVisible then
  begin
    if FEngine <> nil then
    begin
      if OverlapRect(FEngine.FSurfaceRect, BoundsRect) then
      begin
        DoDraw;
        Inc(FEngine.FDrawCount);
      end;
    end;

    if FDrawList <> nil then
    begin
      for i := 0 to FDrawList.Count - 1 do
      begin
        TSprite(FDrawList[i]).Draw; 
      end;
    end;
  end;
end;

function TSprite.GetSpriteAt(X, Y: Integer): TSprite;

  procedure Collision_GetSpriteAt(X, Y: Double; Sprite: TSprite);
  var
    i: Integer;
    X2, Y2: Double;
  begin
    if Sprite.Visible and PointInRect(Point(Round(X), Round(Y)),
      Bounds(Round(Sprite.X), Round(Sprite.Y), Sprite.Width, Sprite.Height)) then //corrected by Sergey
    begin
      if (Result = nil) or (Sprite.Z > Result.Z) then
        Result := Sprite;
    end;

    X2 := X - Sprite.X;
    Y2 := Y - Sprite.Y;
    for i := 0 to Sprite.Count - 1 do
      Collision_GetSpriteAt(X2, Y2, Sprite.Items[i]);
  end;

var
  i: Integer;
  X2, Y2: Double;
begin
  Result := nil;

  X2 := X - Self.X;
  Y2 := Y - Self.Y;
  for i := 0 to Count - 1 do
    Collision_GetSpriteAt(X2, Y2, Items[i]);
end;

function TSprite.GetBoundsRect: TRect;
begin
  Result := Bounds(Round(WorldX), Round(WorldY), Width, Height);
end;

function TSprite.GetClientRect: TRect;
begin
  Result := Bounds(0, 0, Width, Height);
end;

function TSprite.GetCount: Integer;
begin
  if FList <> nil then
    Result := FList.Count
  else
    Result := 0;
end;

function TSprite.GetItem(Index: Integer): TSprite;
begin
  if FList <> nil then
    Result := FList[Index]
  else
    raise ESpriteError.CreateFmt(SListIndexError, [Index]);
end;

function TSprite.GetWorldX: Double;
begin
  if Parent <> nil then
    Result := Parent.WorldX + FX
  else
    Result := FX;
end;

function TSprite.GetWorldY: Double;
begin
  if Parent <> nil then
    Result := Parent.WorldY + FY
  else
    Result := FY;
end;

procedure TSprite.SetZ(Value: Integer);
begin
  if FZ <> Value then
  begin
    FZ := Value;
    if Parent <> nil then
    begin
      Parent.FDrawList.Remove(Self);
      Parent.AddDrawList(Self);
    end;
  end;
end;

procedure TSprite.Assign(Source: TPersistent);
begin
  if Source is TSprite then
  begin
    FCollisioned := TSprite(Source).FCollisioned;
    FMoved := TSprite(Source).FMoved;
    FVisible := TSprite(Source).FVisible;
    FHeight := TSprite(Source).FHeight;
    FWidth := TSprite(Source).FWidth;
    FX := TSprite(Source).FX;
    FY := TSprite(Source).FY;
    FZ := TSprite(Source).FZ;
{$IFDEF Ver4Up}
    FSelected := TSprite(Source).FSelected;
    FGroupNumber := TSprite(Source).FGroupNumber;
{$ENDIF}
    {copy image base - when exists}
    FDXImage := TSprite(Source).FDXImage;
    FDXImageName := TSprite(Source).FDXImageName;
    FDXImageList := TSprite(Source).FDXImageList;
    {events}
    FOnDraw := TSprite(Source).FOnDraw;
    FOnMove := TSprite(Source).FOnMove;
    FOnCollision := TSprite(Source).FOnCollision;
    FOnGetImage := TSprite(Source).FOnGetImage;
  end
  else
    inherited;
end;

procedure TSprite.ReAnimate(MoveCount: Integer);
begin

end;

{  TImageSprite  }

constructor TImageSprite.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  FTransparent := True;
  FAlpha := 255;
  FAngle := 0;
  FBlendMode := rtDraw;
  FCenterX := 0.5;
  FCenterY := 0.5;
  FBlurImage := False;
  FillChar(FBlurImageArr, SizeOf(FBlurImageArr), 0);
  FTextureFilter := D2D_POINT;
end;

procedure TImageSprite.SetImage(AImage: TPictureCollectionItem);
begin
  FDXImage := AImage;
  FDXImageName := '';
  if AImage <> nil then
  begin
    Width := AImage.Width;
    Height := AImage.Height;
    FDXImageName := FDXImage.Name;
  end
  else
  begin
    Width := 0;
    Height := 0;
  end;
end; {SetImage}

function TImageSprite.GetBoundsRect: TRect;
var
  dx, dy: Integer;
begin
  dx := Round(WorldX);
  dy := Round(WorldY);
  if FTile then
  begin
    dx := Mod2(dx, FEngine.SurfaceRect.Right + Width);
    dy := Mod2(dy, FEngine.SurfaceRect.Bottom + Height);

    if dx > FEngine.SurfaceRect.Right then
      dx := (dx - FEngine.SurfaceRect.Right) - Width;

    if dy > FEngine.SurfaceRect.Bottom then
      dy := (dy - FEngine.SurfaceRect.Bottom) - Height;
  end;

  Result := Bounds(dx, dy, Width, Height);
end;

procedure TImageSprite.DoMove(MoveCount: Integer);
begin
  if AsSigned(FOnMove) then
    FOnMove(Self, MoveCount)
  else
  begin
    ReAnimate(MoveCount);
  end;
end;

function TImageSprite.GetDrawImageIndex: Integer;
begin
  Result := FAnimStart + Trunc(FAnimPos); //solve 1.07f to Round()
end;

function TImageSprite.GetDrawRect: TRect;
begin
  Result := BoundsRect;
  OffsetRect(Result, (Width - Image.Width) div 2, (Height - Image.Height) div 2);
end;

procedure TImageSprite.LoadImage;
var
 vImage: TPictureCollectionItem;
begin
  if Image = nil then
    if AsSigned(FOnGetImage) then
    begin
      vImage := nil;
      FOnGetImage(Self, vImage);
      if vImage <> Image then
        Image := vImage;
    end
    else
      if FDXImageName <> '' then
        if Assigned(FDXImageList) then
        begin
          Image := FDXImageList.Items.Find(FDXImageName);
        end;
end;

procedure TImageSprite.DoDraw;
var
  r: TRect;
begin
  LoadImage;
  if Image = nil then
    Exit;
  if AsSigned(FOnDraw) then {owner draw called here}
    FOnDraw(Self)
  else {when is not owner draw then go here}
  begin
    r := Bounds(Round(WorldX), Round(WorldY), Width, Height);
    {New function implemented}
    if Assigned(FEngine.FOwner) then
      DXDraws.DXDraw_Paint(FEngine.FOwner.FDXDraw, Image, r, GetDrawImageIndex,
        FBlurImageArr, FBlurImage, FTextureFilter, FMirrorFlip, FBlendMode, FAngle,
        FAlpha, FCenterX, FCenterY);
  end;
end;

{$WARNINGS OFF}
{$HINTS OFF}

function TImageSprite.ImageCollisionTest(suf1, suf2: TDirectDrawSurface;
  const rect1, rect2: TRect; x1, y1, x2, y2: Integer; DoPixelCheck: Boolean): Boolean;

  function ClipRect(var DestRect: TRect; const DestRect2: TRect): Boolean;
  begin
    with DestRect do
    begin
      Left := Max(Left, DestRect2.Left);
      Right := Min(Right, DestRect2.Right);
      Top := Max(Top, DestRect2.Top);
      Bottom := Min(Bottom, DestRect2.Bottom);

      Result := (Left < Right) and (Top < Bottom);
    end;
  end;

type
  PRGB = ^TRGB;

  TRGB = packed record
    R, G, B: byte;
  end;
var
  ddsd1, ddsd2: {$IFDEF D3D_deprecated}TDDSURFACEDESC{$ELSE}TDDSurfaceDesc2{$ENDIF};
  r1, r2, r1a, r2a: TRect;
  tc1, tc2: DWORD;
  x, y, w, h: Integer;
  P1, P2: Pointer;
begin
  with rect1 do
    r1 := Bounds(0, 0, Right - Left, Bottom - Top);
  r1a := r1;
  with rect2 do
    r2 := Bounds(0, 0, Right - Left, Bottom - Top);
  r2a := r2;

  with rect2 do
    r2 := Bounds(x2 - x1, y2 - y1, Right - Left, Bottom - Top);

  Result := OverlapRect(r1, r2);

  if (suf1 = nil) or (suf2 = nil) then
    Exit;

  if DoPixelCheck and Result then
  begin
    {  Get Overlapping rectangle  }
    with r1 do
      r1 := Bounds(Max(x2 - x1, 0), Max(y2 - y1, 0), Right - Left, Bottom - Top);
    with r2 do
      r2 := Bounds(Max(x1 - x2, 0), Max(y1 - y2, 0), Right - Left, Bottom - Top);

    ClipRect(r1, r1a);
    ClipRect(r2, r2a);

    w := Min(r1.Right - r1.Left, r2.Right - r2.Left);
    h := Min(r1.Bottom - r1.Top, r2.Bottom - r2.Top);

    ClipRect(r1, bounds(r1.Left, r1.Top, w, h));
    ClipRect(r2, bounds(r2.Left, r2.Top, w, h));

    {  Pixel check !!!  }
    ddsd1.dwSize := SizeOf(ddsd1);

    with rect1 do
      r1 := Bounds(r1.Left + left, r1.Top + top, w, h);
    with rect2 do
      r2 := Bounds(r2.Left + left, r2.Top + top, w, h);

    if suf1 = suf2 then
    begin
      suf2.Lock(r2, ddsd2);
      suf2.unlock;
    end;

    if suf1.Lock(r1, ddsd1) then
    begin
      try
        ddsd2.dwSize := SizeOf(ddsd2);
        if (suf1 = suf2) or suf2.Lock(r2, ddsd2) then
        begin
          try
            {this line out: don't test pixel but rect only, its wrong}
            {if suf1=suf2 then ddsd2 := ddsd1;}
            if ddsd1.ddpfPixelFormat.dwRGBBitCount <> ddsd2.ddpfPixelFormat.dwRGBBitCount then
              Exit;

            {  Get transparent color  }
            tc1 := ddsd1.ddckCKSrcBlt.dwColorSpaceLowValue;
            tc2 := ddsd2.ddckCKSrcBlt.dwColorSpaceLowValue;

            case ddsd1.ddpfPixelFormat.dwRGBBitCount of
              8:
                begin
                  for y := 0 to h - 1 do
                  begin
                    P1 := Pointer(Integer(ddsd1.lpSurface) + y * ddsd1.lPitch);
                    P2 := Pointer(Integer(ddsd2.lpSurface) + y * ddsd2.lPitch);
                    for x := 0 to w - 1 do
                    begin
                      if (PByte(P1)^ <> tc1) and (PByte(P2)^ <> tc2) then
                        Exit;
                      Inc(PByte(P1));
                      Inc(PByte(P2));
                    end;
                  end;
                end;
              16:
                begin
                  for y := 0 to h - 1 do
                  begin
                    P1 := Pointer(Integer(ddsd1.lpSurface) + y * ddsd1.lPitch);
                    P2 := Pointer(Integer(ddsd2.lpSurface) + y * ddsd2.lPitch);
                    for x := 0 to w - 1 do
                    begin
                      if (PWord(P1)^ <> tc1) and (PWord(P2)^ <> tc2) then
                        Exit;
                      Inc(PWord(P1));
                      Inc(PWord(P2));
                    end;
                  end;
                end;
              24:
                begin
                  for y := 0 to h - 1 do
                  begin
                    P1 := Pointer(Integer(ddsd1.lpSurface) + y * ddsd1.lPitch);
                    P2 := Pointer(Integer(ddsd2.lpSurface) + y * ddsd2.lPitch);
                    for x := 0 to w - 1 do
                    begin
                      with PRGB(P1)^ do
                        if (R shl 16) or (G shl 8) or B <> tc1 then
                          Exit;
                      with PRGB(P2)^ do
                        if (R shl 16) or (G shl 8) or B <> tc2 then
                          Exit;
                      Inc(PRGB(P1));
                      Inc(PRGB(P2));
                    end;
                  end;
                end;
              32:
                begin
                  for y := 0 to h - 1 do
                  begin
                    P1 := Pointer(Integer(ddsd1.lpSurface) + y * ddsd1.lPitch);
                    P2 := Pointer(Integer(ddsd2.lpSurface) + y * ddsd2.lPitch);
                    for x := 0 to w - 1 do
                    begin
                      if (PDWORD(P1)^ <> tc1) and (PDWORD(P2)^ <> tc2) then
                        Exit;
                      Inc(PDWORD(P1));
                      Inc(PDWORD(P2));
                    end;
                  end;
                end;
            end;
          finally
            if suf1 <> suf2 then
              suf2.UnLock;
          end;
        end;
      finally
        suf1.UnLock;
      end;
    end;

    Result := False;
  end;
end;

{$HINTS ON}
{$WARNINGS ON}

function TImageSprite.TestCollision(Sprite: TSprite): Boolean;
var
  img1, img2: Integer;
  box1, box2: TRect;
begin
  if (Sprite is TImageSprite) then
    if FPixelCheck then
    begin
      box1 := GetDrawRect;
      box2 := TImageSprite(Sprite).GetDrawRect;

      img1 := GetDrawImageIndex;
      img2 := TImageSprite(Sprite).GetDrawImageIndex;

      Result := ImageCollisionTest(Image.PatternSurfaces[img1],
        TImageSprite(Sprite).Image.PatternSurfaces[img2], Image.PatternRects[img1],
        TImageSprite(Sprite).Image.PatternRects[img2], box1.Left, box1.Top,
        box2.Left, box2.Top, True);
    end
    else
      Result := OverlapRect(Bounds(Round(Sprite.WorldX), Round(Sprite.WorldY),
        Sprite.Width, Sprite.Height), Bounds(Round(WorldX), Round(WorldY), Width, Height))
  else
    Result := inherited TestCollision(Sprite);
end;

procedure TImageSprite.Assign(Source: TPersistent);
begin
  if Source is TImageSprite then begin
    FCenterX := TImageSprite(Source).FCenterX;
    FCenterY := TImageSprite(Source).FCenterY;
    FAnimCount := TImageSprite(Source).FAnimCount;
    FAnimLooped := TImageSprite(Source).FAnimLooped;
    FAnimPos := TImageSprite(Source).FAnimPos;
    FAnimSpeed := TImageSprite(Source).FAnimSpeed;
    FAnimStart := TImageSprite(Source).FAnimStart;
    FDXImage := TImageSprite(Source).FDXImage;
    FPixelCheck := TImageSprite(Source).FPixelCheck;
    FTile := TImageSprite(Source).FTile;
    FTransparent := TImageSprite(Source).FTransparent;
    FAngle := TImageSprite(Source).FAngle;
    FAlpha := TImageSprite(Source).FAlpha;
    FBlendMode := TImageSprite(Source).FBlendMode;
    FBlurImage := TImageSprite(Source).FBlurImage;
  end;
  inherited;
end;

procedure TImageSprite.ReAnimate(MoveCount: Integer);
var
  I: Integer;
begin
  FAnimPos := FAnimPos + FAnimSpeed * MoveCount;

  if FAnimLooped then
  begin
    if FAnimCount > 0 then
      FAnimPos := Mod2f(FAnimPos, FAnimCount)
    else
      FAnimPos := 0;
  end
  else
  begin
    if Round(FAnimPos) >= FAnimCount then
    begin
      FAnimPos := FAnimCount - 1;
      FAnimSpeed := 0;
    end;
    if FAnimPos < 0 then
    begin
      FAnimPos := 0;
      FAnimSpeed := 0;
    end;
  end;
  if FBlurImage then
  begin
    {ale jen jsou-li jine souradnice}
    if (FBlurImageArr[High(FBlurImageArr)].eX <> Round(WorldX)) or
    (FBlurImageArr[High(FBlurImageArr)].eY <> Round(WorldY)) then
    begin
      for i := Low(FBlurImageArr) + 1 to High(FBlurImageArr) do
      begin
        FBlurImageArr[i - 1] := FBlurImageArr[i];
        {adjust the blur intensity}
        FBlurImageArr[i - 1].eIntensity := Round(FAlpha / (High(FBlurImageArr) + 1)) * (i - 1);
      end;
      with FBlurImageArr[High(FBlurImageArr)] do
      begin
        eX := Round(WorldX);
        eY := Round(WorldY);
        ePatternIndex := GetDrawImageIndex;
        eIntensity := Round(FAlpha / (High(FBlurImageArr) + 1)) * High(FBlurImageArr);
        eBlendMode := FBlendMode;
        eActive := True;
      end;
    end;
  end;
end;

function TImageSprite.StoreCenterX: Boolean;
begin
  Result := FCenterX <> 0.5;
end;

function TImageSprite.StoreCenterY: Boolean;
begin
  Result := FCenterY <> 0.5;
end;

function TImageSprite.StoreAlpha: Boolean;
begin
  Result := FAlpha <> 0.0;
end;

procedure TImageSprite.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('BlendMode', ReadBlendMode, WriteBlendMode, FBlendMode <> rtDraw);
  Filer.DefineProperty('Angle', ReadAngle, WriteAngle, FAngle <> 0);
  Filer.DefineProperty('CenterX', ReadCenterX, WriteCenterX, FCenterX <> 0.5);
  Filer.DefineProperty('CenterY', ReadCenterY, WriteCenterY, FCenterY <> 0.5);
  Filer.DefineProperty('Alpha', ReadAlpha, WriteAlpha, FAlpha <> $FF);
  Filer.DefineProperty('AnimCount', ReadAnimCount, WriteAnimCount, FAnimCount <> 0);
  Filer.DefineProperty('AnimLooped', ReadAnimLooped, WriteAnimLooped, FAnimLooped);
  Filer.DefineProperty('AnimPos', ReadAnimPos, WriteAnimPos, FAnimPos <> 0);
  Filer.DefineProperty('AnimSpeed', ReadAnimSpeed, WriteAnimSpeed, FAnimSpeed <> 0);
  Filer.DefineProperty('AnimStart', ReadAnimStart, WriteAnimStart, True);
  Filer.DefineProperty('PixelCheck', ReadPixelCheck, WritePixelCheck, FPixelCheck);
  Filer.DefineProperty('Tile', ReadTile, WriteTile, FTile);
  Filer.DefineProperty('BlurImage', ReadBlurImage, WriteBlurImage, FBlurImage);
  Filer.DefineProperty('MirrorFlip', ReadMirrorFlip, WriteMirrorFlip, FMirrorFlip <> []);
  Filer.DefineProperty('TextureFilter', ReadTextureFilter, WriteTextureFilter, FTextureFilter <> D2D_POINT);
end;

procedure TImageSprite.WriteMirrorFlip(Writer: TWriter);
var
  q: TRenderMirrorFlip;
  s, ss: string;
//  I: Integer;
  //PI: PPropInfo;
begin
//  PI := GetPropInfo(Self,'MirrorFlip');
//  I := Integer(FMirrorFlip);
  s := '[]'; ss := '';
  for q := Low(TRenderMirrorFlip) to High(TRenderMirrorFlip) do
    if q in FMirrorFlip then
      ss := ss + GetEnumName(TypeInfo(TRenderMirrorFlip), Ord(q)) + ', ';
  if ss <> '' then
    s := '[' + Copy(ss, 1, Length(ss) - 2) + ']';
  Writer.WriteString(s);
//---  Writer.WriteString(SetToString(PI, GetOrdProp(Self, PI), True));
end;

procedure TImageSprite.ReadMirrorFlip(Reader: TReader);
var
  q: TRenderMirrorFlip;
  qq: TRenderMirrorFlipSet;
  s {, ss}: string;
//  PI: PPropInfo;
begin
//  PI := GetPropInfo(Self,'MirrorFlip');
//  SetOrdProp(Self,PI,StringToSet(PI, Reader.ReadString));
  qq := [];
  s := Reader.ReadString;
  for q := Low(TRenderMirrorFlip) to High(TRenderMirrorFlip) do
    if Pos(GetEnumName(TypeInfo(TRenderMirrorFlip), Ord(q)), s) <> 0 then
      qq := qq + [q];
  FMirrorFlip := qq;
end;

procedure TImageSprite.ReadAnimLooped(Reader: TReader);
begin
  FAnimLooped := Reader.ReadBoolean;
end;

procedure TImageSprite.WriteAnimLooped(Writer: TWriter);
begin
  Writer.WriteBoolean(FAnimLooped);
end;

procedure TImageSprite.ReadAnimPos(Reader: TReader);
begin
  FAnimPos := Reader.ReadFloat;
end;

procedure TImageSprite.WriteAnimPos(Writer: TWriter);
begin
  Writer.WriteFloat(FAnimPos);
end;

procedure TImageSprite.ReadAnimSpeed(Reader: TReader);
begin
  FAnimSpeed := Reader.ReadFloat;
end;

procedure TImageSprite.WriteAnimSpeed(Writer: TWriter);
begin
  Writer.WriteFloat(FAnimSpeed);
end;

procedure TImageSprite.ReadAnimStart(Reader: TReader);
begin
  FAnimStart := Reader.ReadInteger;
end;

procedure TImageSprite.WriteAnimStart(Writer: TWriter);
begin
  Writer.WriteInteger(FAnimStart);
end;

procedure TImageSprite.ReadPixelCheck(Reader: TReader);
begin
  FPixelCheck := Reader.ReadBoolean;
end;

procedure TImageSprite.WritePixelCheck(Writer: TWriter);
begin
  Writer.WriteBoolean(FPixelCheck);
end;

procedure TImageSprite.ReadTile(Reader: TReader);
begin
  FTile := Reader.ReadBoolean;
end;

procedure TImageSprite.WriteTile(Writer: TWriter);
begin
  Writer.WriteBoolean(FTile);
end;

procedure TImageSprite.ReadAnimCount(Reader: TReader);
begin
  FAnimCount := Reader.ReadInteger;
end;

procedure TImageSprite.WriteAnimCount(Writer: TWriter);
begin
  Writer.WriteInteger(FAnimCount);
end;

procedure TImageSprite.ReadAlpha(Reader: TReader);
begin
  FAlpha := Reader.ReadInteger;
end;

procedure TImageSprite.WriteAlpha(Writer: TWriter);
begin
  Writer.WriteInteger(FAlpha);
end;

procedure TImageSprite.ReadCenterY(Reader: TReader);
begin
  FCenterY := Reader.ReadFloat;
end;

procedure TImageSprite.WriteCenterY(Writer: TWriter);
begin
  Writer.WriteFloat(FCenterY);
end;

procedure TImageSprite.ReadCenterX(Reader: TReader);
begin
  FCenterX := Reader.ReadFloat;
end;

procedure TImageSprite.WriteCenterX(Writer: TWriter);
begin
  Writer.WriteFloat(FCenterX);
end;

procedure TImageSprite.ReadAngle(Reader: TReader);
begin
  FAngle := Reader.{$IFDEF VER4UP}ReadSingle{$ELSE}ReadFloat{$ENDIF};
end;

procedure TImageSprite.WriteAngle(Writer: TWriter);
begin
  Writer.{$IFDEF VER4UP}WriteSingle{$ELSE}WriteFloat{$ENDIF}(FAngle);
end;

procedure TImageSprite.ReadBlendMode(Reader: TReader);
begin
  FBlendMode := TRenderType(GetEnumValue(TypeInfo(TRenderType), Reader.ReadString));
end;

procedure TImageSprite.WriteBlendMode(Writer: TWriter);
begin
  Writer.WriteString(GetEnumName(TypeInfo(TRenderType), Ord(FBlendMode)));
end;

procedure TImageSprite.ReadBlurImage(Reader: TReader);
begin
  FBlurImage := Reader.ReadBoolean;
end;

procedure TImageSprite.WriteBlurImage(Writer: TWriter);
begin
  Writer.WriteBoolean(FBlurImage);
end;

procedure TImageSprite.ReadTextureFilter(Reader: TReader);
begin
  FTextureFilter := TD2DTextureFilter(Reader.ReadInteger);
end;

procedure TImageSprite.WriteTextureFilter(Writer: TWriter);
begin
  Writer.WriteInteger(Ord(FTextureFilter));
end;

procedure TImageSprite.SetBlurImageArr(const Value: TBlurImageArr);
begin
  FBlurImageArr := Value;
end;

procedure TImageSprite.SetBlurImage(const Value: Boolean);
begin
  if (FBlurImage <> Value) and (Value) then
  begin
    FillChar(FBlurImageArr, SizeOf(FBlurImageArr), 0); //get out when set up
  end;
  FBlurImage := Value;
end;

function TImageSprite.GetImage: TPictureCollectionItem;
begin
  Result := FDXImage;
end;

procedure TImageSprite.SetMirrorFlip(const Value: TRenderMirrorFlipSet);
begin
  FMirrorFlip := Value;
end;

{  TBackgroundSprite  }

constructor TBackgroundSprite.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  FMap := nil;
  FMapWidth := 0;
  FMapHeight := 0;
  Collisioned := False;
end;

destructor TBackgroundSprite.Destroy;
begin
  SetMapSize(0, 0);
  inherited Destroy;
end;

procedure TBackgroundSprite.ChipsDraw(Image: TPictureCollectionItem; X, Y: Integer; PatternIndex: Integer);
begin
  if AsSigned(FOnDraw) then
    FOnDraw(Self)
  else
  begin
    //Image.Draw(FEngine.Surface, X, Y, PatternIndex);
    {New function implemented}
    if Assigned(FEngine.FOwner) then
      //Image.DrawAlpha(DXDraw1.Surface,ChipsRect,ChipsPatternIndex,Blend);
      DXDraws.DXDraw_Paint(FEngine.FOwner.FDXDraw, Image, ChipsRect, ChipsPatternIndex,
        FBlurImageArr, FBlurImage, FTextureFilter, FMirrorFlip, FBlendMode, FAngle,
        Map[X,Y].Alpha, FCenterX, FCenterY);
  end;
end;

procedure TBackgroundSprite.DoDraw;
var
  TmpX, TmpY, cx, cy, cx2, cy2, PatternIndex, ChipWidth, ChipHeight: Integer;
  StartX, StartY, EndX, EndY, StartX_, StartY_, OfsX, OfsY, dWidth, dHeight: Integer;
  r: TRect;
  Q: TMapType;
begin
  LoadImage;
  if Image = nil then
    Exit;

  if (FMapWidth <= 0) or (FMapHeight <= 0) then
    Exit;

  r := Image.PatternRects[0];
  ChipWidth := r.Right - r.Left;
  ChipHeight := r.Bottom - r.Top;

  dWidth := (FEngine.SurfaceRect.Right + ChipWidth) div ChipWidth + 1;
  dHeight := (FEngine.SurfaceRect.Bottom + ChipHeight) div ChipHeight + 1;

  TmpX := Round(WorldX);
  TmpY := Round(WorldY);

  OfsX := TmpX mod ChipWidth;
  OfsY := TmpY mod ChipHeight;

  StartX := TmpX div ChipWidth;
  StartX_ := 0;

  if StartX < 0 then
  begin
    StartX_ := -StartX;
    StartX := 0;
  end;

  StartY := TmpY div ChipHeight;
  StartY_ := 0;

  if StartY < 0 then
  begin
    StartY_ := -StartY;
    StartY := 0;
  end;

  EndX := Min(StartX + FMapWidth - StartX_, dWidth);
  EndY := Min(StartY + FMapHeight - StartY_, dHeight);

  if FTile then
  begin
    for cy := -1 to dHeight do
    begin
      cy2 := Mod2((cy - StartY + StartY_), FMapHeight);
      for cx := -1 to dWidth do
      begin
        cx2 := Mod2((cx - StartX + StartX_), FMapWidth);
        PatternIndex := Chips[cx2, cy2];
        ChipsPatternIndex := PatternIndex; //refresh only
        ChipsRect := Bounds(cx * ChipWidth + OfsX, cy * ChipHeight + OfsY, ChipWidth, ChipHeight);
        if PatternIndex >= 0 then
        begin
          if AsSigned(FOnDraw) then
            FOnDraw(Self)
          else
          begin
            {New function implemented}
            if Assigned(FEngine.FOwner) then
            begin
              Q := Map[cx2,cy2];
              DXDraws.DXDraw_Paint(FEngine.FOwner.FDXDraw, Image, ChipsRect, Q.MapChip,
                FBlurImageArr, FBlurImage, Q.TextureFilter, Q.MirrorFlip, Q.Rendered, Q.Angle,
                Q.Alpha, Q.CenterX, Q.CenterY);
            end;
          end;
        end;
      end;
    end;
  end
  else
  begin
    for cy := StartY to EndY - 1 do
      for cx := StartX to EndX - 1 do
      begin
        PatternIndex := Chips[cx - StartX + StartX_, cy - StartY + StartY_];
        ChipsPatternIndex := PatternIndex; //refresh only
        ChipsRect := Bounds(cx * ChipWidth + OfsX, cy * ChipHeight + OfsY, ChipWidth, ChipHeight);
        if PatternIndex >= 0 then
        begin
          if AsSigned(FOnDraw) then
            FOnDraw(Self)
          else
          begin
            {New function implemented}
            if Assigned(FEngine.FOwner) then
            begin
              Q := Map[cx,cy];
              DXDraws.DXDraw_Paint(FEngine.FOwner.FDXDraw, Image, ChipsRect, Q.MapChip,
                FBlurImageArr, FBlurImage, Q.TextureFilter, Q.MirrorFlip, Q.Rendered, Q.Angle,
                Q.Alpha, Q.CenterX, Q.CenterY);
            end;
          end;
        end
      end;
  end; 
end;

function TBackgroundSprite.TestCollision(Sprite: TSprite): Boolean;
var
  box0, box1, box2: TRect;
  cx, cy, ChipWidth, ChipHeight: Integer;
  r: TRect;
begin
  Result := True;
  if Image = nil then
    Exit;
  if (FMapWidth <= 0) or (FMapHeight <= 0) then
    Exit;

  r := Image.PatternRects[0];
  ChipWidth := r.Right - r.Left;
  ChipHeight := r.Bottom - r.Top;

  box1 := Sprite.BoundsRect;
  box2 := BoundsRect;

  IntersectRect(box0, box1, box2);

  OffsetRect(box0, -Round(WorldX), -Round(WorldY));
  OffsetRect(box1, -Round(WorldX), -Round(WorldY));

  for cy := (box0.Top - ChipHeight + 1) div ChipHeight to box0.Bottom div ChipHeight do
    for cx := (box0.Left - ChipWidth + 1) div ChipWidth to box0.Right div ChipWidth do
      if CollisionMap[Mod2(cx, MapWidth), Mod2(cy, MapHeight)] then
      begin
        if OverlapRect(Bounds(cx * ChipWidth, cy * ChipHeight, ChipWidth,
          ChipHeight), box1) then
          Exit;
      end;

  Result := False;
end;

function TBackgroundSprite.GetChip(X, Y: Integer): Integer;
begin
  if (X >= 0) and (X < FMapWidth) and (Y >= 0) and (Y < FMapHeight) then
    Result := PMapType(Integer(FMap) + (Y * FMapWidth + X) * SizeOf(TMapType))^.MapChip
  else
    Result := -1;
end;

function TBackgroundSprite.GetCollisionMapItem(X, Y: Integer): Boolean;
begin
  if (X >= 0) and (X < FMapWidth) and (Y >= 0) and (Y < FMapHeight) then
    Result := PMapType(Integer(FMap) + (Y * FMapWidth + X) * SizeOf(TMapType))^.CollisionChip
  else
    Result := False;
end;

function TBackgroundSprite.GetCollisionRectItem(X, Y: Integer): TRect;
begin
  if (X >= 0) and (X < FMapWidth) and (Y >= 0) and (Y < FMapHeight) then
    Result := PMapType(Integer(FMap) + (Y * FMapWidth + X) * SizeOf(TMapType))^.CollisionRect
  else
    Result := Rect(0, 0, 0, 0);
end;

function TBackgroundSprite.GetTagMap(X, Y: Integer): Integer;
begin
  if (X >= 0) and (X < FMapWidth) and (Y >= 0) and (Y < FMapHeight) then
    Result := PMapType(Integer(FMap) + (Y * FMapWidth + X) * SizeOf(TMapType))^.Tag
  else
    Result := 0;
end;

function TBackgroundSprite.GetMap(X, Y: Integer): TMapType;
begin
  if (X >= 0) and (X < FMapWidth) and (Y >= 0) and (Y < FMapHeight) then
    Result := PMapType(Integer(FMap) + (Y * FMapWidth + X) * SizeOf(TMapType))^
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TBackgroundSprite.GetBoundsRect: TRect;
begin
  if FTile then
    Result := FEngine.SurfaceRect
  else
  begin
    LoadImage;
    if Image <> nil then
      Result := Bounds(Round(WorldX), Round(WorldY), Image.Width * FMapWidth,
        Image.Height * FMapHeight)
    else
      Result := Rect(0, 0, 0, 0);
  end;
end;

procedure TBackgroundSprite.SetChip(X, Y: Integer; Value: Integer);
begin
  if (X >= 0) and (X < FMapWidth) and (Y >= 0) and (Y < FMapHeight) then
    PMapType(Integer(FMap) + (Y * FMapWidth + X) * SizeOf(TMapType))^.MapChip := Value;
end;

procedure TBackgroundSprite.SetCollisionMapItem(X, Y: Integer; Value: Boolean);
begin
  if (X >= 0) and (X < FMapWidth) and (Y >= 0) and (Y < FMapHeight) then
    PMapType(Integer(FMap) + (Y * FMapWidth + X) * SizeOf(TMapType))^.CollisionChip := Value;
end;

procedure TBackgroundSprite.SetCollisionRectItem(X, Y: Integer; Value: TRect);
begin
  if (X >= 0) and (X < FMapWidth) and (Y >= 0) and (Y < FMapHeight) then
    PMapType(Integer(FMap) + (Y * FMapWidth + X) * SizeOf(TMapType))^.CollisionRect := Value;
end;

procedure TBackgroundSprite.SetTagMap(X, Y: Integer; Value: Integer);
begin
  if (X >= 0) and (X < FMapWidth) and (Y >= 0) and (Y < FMapHeight) then
    PMapType(Integer(FMap) + (Y * FMapWidth + X) * SizeOf(TMapType))^.Tag := Value;
end;

procedure TBackgroundSprite.SetMap(X, Y: Integer; Value: TMapType);
begin
  if (X >= 0) and (X < FMapWidth) and (Y >= 0) and (Y < FMapHeight) then
    PMapType(Integer(FMap) + (Y * FMapWidth + X) * SizeOf(TMapType))^ := Value;
end;

procedure TBackgroundSprite.SetMapHeight(Value: Integer);
begin
  SetMapSize(FMapWidth, Value);
end;

procedure TBackgroundSprite.SetMapWidth(Value: Integer);
begin
  SetMapSize(Value, FMapHeight);
end;

procedure TBackgroundSprite.SetImage(Img: TPictureCollectionItem);
begin
  inherited SetImage(Img);
  if Assigned(Img) then
  begin
    FWidth := FMapWidth * Img.Width;
    FHeight := FMapHeight * Img.Height;
  end
  else
  begin
    FWidth := 0;
    FHeight := 0;
  end;
end;

procedure TBackgroundSprite.SetMapSize(AMapWidth, AMapHeight: Integer);
var I: Integer;
begin
  if (FMapWidth <> AMapWidth) or (FMapHeight <> AMapHeight) or (FMap = nil) then
  begin
    try
      if (AMapWidth <= 0) or (AMapHeight <= 0) then
      begin
        FreeMem(FMap, FMapWidth * FMapHeight * SizeOf(TMapType)); FMap := nil;
        AMapWidth := 0;
        AMapHeight := 0;
      end;
      FMapWidth := AMapWidth;
      FMapHeight := AMapHeight;
      System.ReallocMem(FMap, FMapWidth * FMapHeight * SizeOf(TMapType));
      if Assigned(FMap) then
      begin
        FillChar(FMap^, FMapWidth * FMapHeight * SizeOf(TMapType), 0);
        for I := 0 to FMapWidth * FMapHeight - 1 do
          PMapType(Integer(FMap) + (I) * SizeOf(TMapType))^.CollisionChip := True;
      end
    except
      FreeMem(FMap, FMapWidth * FMapHeight * SizeOf(TMapType));
      FMap := nil;
    end;
  end
end;

procedure TBackgroundSprite.Assign(Source: TPersistent);
begin
  if Source is TBackgroundSprite then
  begin
    FMapWidth := TBackgroundSprite(Source).FMapWidth;
    FMapHeight := TBackgroundSprite(Source).FMapHeight;
    FTile := TBackgroundSprite(Source).FTile;
  end;
  inherited;
end;

procedure TBackgroundSprite.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Map', ReadMapData, WriteMapData, FMap <> nil);
end;

type
  TMapDataHeader = packed record
    MapWidth: Integer;
    MapHeight: Integer;
  end;

procedure TBackgroundSprite.ReadMapData(Stream: TStream);
var
  Header: TMapDataHeader;
begin
  Stream.ReadBuffer(Header, SizeOf(Header));
  FMapWidth := Header.MapWidth;
  FMapHeight := Header.MapHeight;
  SetMapSize(Header.MapWidth, Header.MapHeight);
  if Assigned(FMap) and (Header.MapWidth > 0) and (Header.MapHeight > 0) then
  begin
    Stream.ReadBuffer(FMap^, FMapWidth * FMapHeight * SizeOf(TMapType));
  end;
end;

procedure TBackgroundSprite.WriteMapData(Stream: TStream);
var
  Header: TMapDataHeader;
begin
  Header.MapWidth := FMapWidth;
  Header.MapHeight := FMapHeight;
  Stream.WriteBuffer(Header, SizeOf(Header));
  if Assigned(FMap) then
    Stream.WriteBuffer(FMap^, FMapWidth * FMapHeight * SizeOf(TMapType));
end;

function TBackgroundSprite.GetOverlap(X, Y: Integer): Integer;
begin
  if (X >= 0) and (X < FMapWidth) and (Y >= 0) and (Y < FMapHeight) then
    Result := PMapType(Integer(FMap) + (Y * FMapWidth + X) * SizeOf(TMapType))^.Overlap
  else
    Result := 0;
end;

procedure TBackgroundSprite.SetOverlap(X, Y: Integer; const Value: Integer);
begin
  if (X >= 0) and (X < FMapWidth) and (Y >= 0) and (Y < FMapHeight) then
    PMapType(Integer(FMap) + (Y * FMapWidth + X) * SizeOf(TMapType))^.Overlap := Value;
end;

function TBackgroundSprite.IsMapEmpty: Boolean;
begin
  Result := (FMap = nil) or (FMapWidth <= 0) or (FMapHeight <= 0);
end;

{  TSpriteEngine  }

constructor TSpriteEngine.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  FDeadList := TList.Create;
  // group handling
{$IFDEF Ver4Up}
  fCurrentSelected := Tlist.create;
  GroupCount := 10;
{$ENDIF}
end;

destructor TSpriteEngine.Destroy;
begin
  // cleanup Group handling
{$IFDEF Ver4Up}
  ClearCurrent;
  GroupCount := 0;
{$ENDIF}
  FDeadList.Free;
  inherited Destroy;
{$IFDEF Ver4Up}
  fCurrentSelected.free;
{$ENDIF}
end;

procedure TSpriteEngine.Collisions;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
    Items[index].Collision;
end;
{Collisions}
{$IFDEF Ver4Up}

procedure TSpriteEngine.GroupSelect(const Area: TRect; Add: Boolean = False);
begin
  GroupSelect(Area, [Tsprite], Add);
end; {GroupSelect}

procedure TSpriteEngine.GroupSelect(const Area: TRect; Filter: array of TSpriteClass; Add: Boolean = False);
var
  index, index2: Integer;
  sprite: TSprite;
begin
  Assert(length(Filter) <> 0, 'Filter = []');
  if not Add then
    ClearCurrent;
  if length(Filter) = 1 then
  begin
    for Index := 0 to Count - 1 do
    begin
      sprite := Items[Index];
      if (sprite is Filter[0]) and OverlapRect(sprite.GetBoundsRect, Area) then
        sprite.Selected := true;
    end
  end
  else
  begin
    for Index := 0 to Count - 1 do
    begin
      sprite := Items[index];
      for index2 := 0 to high(Filter) do
        if (sprite is Filter[index2]) and OverlapRect(sprite.GetBoundsRect, Area) then
        begin
          sprite.Selected := true;
          break;
        end;
    end
  end;
  fObjectsSelected := CurrentSelected.count <> 0;
end; {GroupSelect}

function TSpriteEngine.Select(Point: TPoint; Filter: array of TSpriteClass; Add: Boolean = False): Tsprite;
var
  index, index2: Integer;
begin
  Assert(length(Filter) <> 0, 'Filter = []');
  if not Add then
    ClearCurrent;
  // By searching the Drawlist in reverse
  // we select the highest sprite if the sprit is under the point
  assert(FDrawList <> nil, 'FDrawList = nil');
  if length(Filter) = 1 then
  begin
    for Index := FDrawList.Count - 1 downto 0 do
    begin
      Result := FDrawList[Index];
      if (Result is Filter[0]) and PointInRect(Point, Result.GetBoundsRect) then
      begin
        Result.Selected := true;
        fObjectsSelected := CurrentSelected.count <> 0;
        exit;
      end;
    end
  end
  else
  begin
    for Index := FDrawList.Count - 1 downto 0 do
    begin
      Result := FDrawList[index];
      for index2 := 0 to high(Filter) do
        if (Result is Filter[index2]) and PointInRect(Point, Result.GetBoundsRect) then
        begin
          Result.Selected := true;
          fObjectsSelected := CurrentSelected.count <> 0;
          exit;
        end;
    end
  end;
  Result := nil;
end; {Select}

function TSpriteEngine.Select(Point: TPoint; Add: Boolean = False): TSprite;
begin
  Result := Select(Point, [Tsprite], Add);
end; {Select}

procedure TSpriteEngine.ClearCurrent;
begin
  while CurrentSelected.count <> 0 do
    TSprite(CurrentSelected[CurrentSelected.count - 1]).Selected := False;
  fObjectsSelected := False;
end; {ClearCurrent}

procedure TSpriteEngine.ClearGroup(GroupNumber: Integer);
var
  index: Integer;
  Group: Tlist;
begin
  Group := Groups[GroupNumber];
  if Group <> nil then
    for index := 0 to Group.count - 1 do
      TSprite(Group[index]).Selected := False;
end; {ClearGroup}

procedure TSpriteEngine.CurrentToGroup(GroupNumber: Integer; Add: Boolean = False);
var
  Group: Tlist;
  index: Integer;
begin
  Group := Groups[GroupNumber];
  if Group = nil then
    exit;
  if not Add then
    ClearGroup(GroupNumber);
  for index := 0 to Group.count - 1 do
    TSprite(Group[index]).GroupNumber := GroupNumber;
end; {CurrentToGroup}

procedure TSpriteEngine.GroupToCurrent(GroupNumber: Integer; Add: Boolean = False);
var
  Group: Tlist;
  index: Integer;
begin
  if not Add then
    ClearCurrent;
  Group := Groups[GroupNumber];
  if Group <> nil then
    for index := 0 to Group.count - 1 do
      TSprite(Group[index]).Selected := true;
end; {GroupToCurrent}

function TSpriteEngine.GetGroup(Index: Integer): Tlist;
begin
  if (index >= 0) or (index < fGroupCount) then
    Result := fGroups[index]
  else
    Result := nil;
end; {GetGroup}

procedure TSpriteEngine.SetGroupCount(AGroupCount: Integer);
var
  index: Integer;
begin
  if (AGroupCount <> FGroupCount) and (AGroupCount >= 0) then
  begin
    if FGroupCount > AGroupCount then
    begin // remove groups
      for index := AGroupCount to FGroupCount - 1 do
      begin
        ClearGroup(index);
        FGroups[index].Free;
      end;
      SetLength(FGroups, AGroupCount);
    end
    else
    begin // add groups
      SetLength(FGroups, AGroupCount);
      for index := FGroupCount to AGroupCount - 1 do
        FGroups[index] := Tlist.Create;
    end;
    FGroupCount := Length(FGroups);
  end;
end; {SetGroupCount}
{$ENDIF}

procedure TSpriteEngine.Dead;
begin
  while FDeadList.Count > 0 do
    TSprite(FDeadList[FDeadList.Count - 1]).Free;
end;

procedure TSpriteEngine.Draw;
begin
  FDrawCount := 0;
  inherited Draw;
end;

procedure TSpriteEngine.SetSurface(Value: TDirectDrawSurface);
begin
  FSurface := Value;
  if FSurface <> nil then
  begin
    FSurfaceRect := Surface.ClientRect;
    Width := FSurfaceRect.Right - FSurfaceRect.Left;
    Height := FSurfaceRect.Bottom - FSurfaceRect.Top;
  end;
end;

{  TCustomDXSpriteEngine  }

constructor TCustomDXSpriteEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEngine := TSpriteEngine.Create(nil);
  FEngine.FOwner := Self;
  FItems := TSpriteCollection.Create(Self);
  FItems.FOwner := Self;
  FItems.FOwnerItem := FEngine;
  FItems.Initialize(FEngine);
end;

destructor TCustomDXSpriteEngine.Destroy;
begin
  FEngine.Free;
  inherited Destroy;
end;

procedure TCustomDXSpriteEngine.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (DXDraw = AComponent) then
    DXDraw := nil;
end;

procedure TCustomDXSpriteEngine.Dead;
begin
  FEngine.Dead;
end;

procedure TCustomDXSpriteEngine.Draw;
begin
  if (FDXDraw <> nil) and (FDXDraw.Initialized) then
    FEngine.Draw; 
end;

procedure TCustomDXSpriteEngine.Move(MoveCount: Integer);
begin
  FEngine.Move(MoveCount);
end;

procedure TCustomDXSpriteEngine.DXDrawNotifyEvent(Sender: TCustomDXDraw;
  NotifyType: TDXDrawNotifyType);
begin
  case NotifyType of
    dxntDestroying: DXDraw := nil;
    dxntInitialize: FEngine.Surface := Sender.Surface;
    dxntFinalize: FEngine.Surface := nil;
  end;
end;

procedure TCustomDXSpriteEngine.SetDXDraw(Value: TCustomDXDraw);
begin
  if FDXDraw <> nil then
    FDXDraw.UnRegisterNotifyEvent(DXDrawNotifyEvent);

  FDXDraw := Value;

  if FDXDraw <> nil then
    FDXDraw.RegisterNotifyEvent(DXDrawNotifyEvent);
end;

procedure TCustomDXSpriteEngine.SetItems(const Value: TSpriteCollection);
begin
  FItems.Assign(Value);
end;

procedure TCustomDXSpriteEngine.Clone(const Amount: Word; const BaseNameOfSprite: string);
var
  i: Integer;
begin
  if Amount = 0 then Exit;
  for i := 1 to Amount do
  begin
    with FItems.Add do
    begin
      KindSprite := FItems.Find(BaseNameOfSprite).KindSprite;
      Sprite.AsSign(FItems.Find(BaseNameOfSprite).Sprite);
      {name has to be different}
      Name := Format(BaseNameOfSprite + '_%d', [I]); //simple name for sprite like Name_1 etc.
      Sprite.Tag := 0; //for sprite you can use Tag property in future as well
    end;
  end;
end;

function TCustomDXSpriteEngine.ForEach(PrefixNameOdSprite: string; var Names: TStringList): Boolean;
var
  I: Integer;
begin
  if Names = nil then
    Names := TStringList.Create;
  for I := 0 to Items.Count - 1 do
  begin
    if PrefixNameOdSprite = '' then
      Names.Add(Items[I].Name)
    else
      {is prefix, fo names like Player????}
      if Pos(PrefixNameOdSprite, Items[I].Name) = 1 then
        Names.Add(Items[I].Name);
  end;
  Result := Names.Count > 0;
  if not Result then {$IFDEF VER5UP}FreeAndNil(Names){$ELSE}begin Names.Free; names := nil end{$ENDIF};
end;

{ TSpriteCollectionItem }

function TSpriteCollectionItem.GetSpriteCollection: TSpriteCollection;
begin
  Result := Collection as TSpriteCollection;
end;

procedure TSpriteCollectionItem.SetSprite(const Value: TSprite);
begin
  FSprite.Assign(Value);
end;

constructor TSpriteCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FOwner := Collection;
  FOwnerItem := (Collection as TSpriteCollection).FOwnerItem;
  FSpriteType := stSprite;
  FSprite := TSprite.Create(FOwnerItem);
end;

procedure TSpriteCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TSpriteCollectionItem then
  begin
    Finalize;
    FSprite.Assign(TSpriteCollectionItem(Source).FSprite);
    inherited Assign(Source);
    Initialize;
  end
  else
    inherited;
end;

procedure TSpriteCollectionItem.Initialize;
begin

end;

destructor TSpriteCollectionItem.Destroy;
begin
  FSprite.Destroy;
  inherited;
end;

procedure TSpriteCollectionItem.Finalize;
begin

end;

procedure TSpriteCollectionItem.SetOnCollision(
  const Value: TCollisionEvent);
begin
  FSprite.FOnCollision := Value;
end;

procedure TSpriteCollectionItem.SetOnDraw(const Value: TDrawEvent);
begin
  FSprite.FOnDraw := Value;
end;

procedure TSpriteCollectionItem.SetOnMove(const Value: TMoveEvent);
begin
  FSprite.FOnMove := Value
end;

function TSpriteCollectionItem.GetDisplayName: string;
begin
  Result := inherited GetDisplayName
end;

procedure TSpriteCollectionItem.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, GetDisplayName) <> 0) and
    (Collection is TSpriteCollection) and (TSpriteCollection(Collection).IndexOf(Value) >= 0) then
    raise Exception.Create(Format(SSpriteDuplicateName, [Value]));
  inherited SetDisplayName(Value);
end;

function TSpriteCollectionItem.GetSpriteType: TSpriteType;
begin
  Result := FSpriteType;
end;

procedure TSpriteCollectionItem.SetSpriteType(const Value: TSpriteType);
var
  tmpSprite: TSprite;
begin
  if Value <> FSpriteType then
  begin
    case Value of
      stSprite: tmpSprite := TSprite.Create(TSpriteEngine(FOwnerItem));
      stImageSprite: TImageSprite(tmpSprite) := TImageSprite.Create(TSpriteEngine(FOwnerItem));
      {$WARNINGS OFF}
      stImageSpriteEx: TImageSpriteEx(tmpSprite) := TImageSpriteEx.Create(TSpriteEngine(FOwnerItem));
      {$WARNINGS ON}
      stBackgroundSprite: TBackgroundSprite(tmpSprite) := TBackgroundSprite.Create(TSpriteEngine(FOwnerItem));
    else
      tmpSprite := nil
    end;
    if Assigned(FSprite) then
    try
      tmpSprite.Assign(FSprite);
      tmpSprite.FOnDraw := FSprite.FOnDraw;
      tmpSprite.FOnMove := FSprite.FOnMove;
      tmpSprite.FOnCollision := FSprite.FOnCollision;
      tmpSprite.FOnGetImage := FSprite.FOnGetImage;
    finally
      FSprite.Free; FSprite := nil;
    end;
    FSprite := tmpSprite;
    FSpriteType := Value;
  end;
end;

function TSpriteCollectionItem.GetOnCollision: TCollisionEvent;
begin
  Result := FSprite.FOnCollision
end;

function TSpriteCollectionItem.GetOnDraw: TDrawEvent;
begin
  Result := FSprite.FOnDraw
end;

function TSpriteCollectionItem.GetOnMove: TMoveEvent;
begin
  Result := FSprite.FOnMove
end;

function TSpriteCollectionItem.GetOnGetImage: TGetImage;
begin
  Result := FSprite.FOnGetImage;
end;

procedure TSpriteCollectionItem.SetOnGetImage(const Value: TGetImage);
begin
  FSprite.FOnGetImage := Value;
end;

function TSpriteCollectionItem.GetImageList: TCustomDXImageList;
begin
  Result := FSprite.FDXImageList;
end;

procedure TSpriteCollectionItem.SetImageList(const Value: TCustomDXImageList);
begin
  FSprite.FDXImageList := Value;
end;

function TSpriteCollectionItem.Clone(NewName: string): TSprite;
var
  T: TSpriteCollectionItem;
begin
  T := GetSpriteCollection.Add;
  T.KindSprite := Self.FSpriteType;
  T.Assign(Self);
  T.Name := NewName;
  Result := T.FSprite;
end;

{ TSpriteCollection }

function TSpriteCollection.Initialized: Boolean;
begin
  Result := FInitializeFlag;
end;

constructor TSpriteCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TSpriteCollectionItem);
  FOwner := AOwner;
  FInitializeFlag := Initialize(TSpriteEngine(AOwner));
end;

function TSpriteCollection.GetItem(Index: Integer): TSpriteCollectionItem;
begin
  Result := TSpriteCollectionItem(inherited Items[Index]);
end;

function TSpriteCollection.Initialize(DXSpriteEngine: TSpriteEngine): Boolean;
begin
  Result := True;
  try
    if AsSigned(FOnInitialize) then
      FOnInitialize(DXSpriteEngine);
  except
    Result := False;
  end
end;

function TSpriteCollection.Find(const Name: string): TSpriteCollectionItem;
var
  i: Integer;
begin
  i := IndexOf(Name);
  if i = -1 then
    raise ESpriteCollectionError.CreateFmt(SSpriteNotFound, [Name]);
  Result := Items[i];
end;

procedure TSpriteCollection.Finalize;
begin
  if AsSigned(FOnFinalize) then
    FOnFinalize(FOwnerItem);
end;

function TSpriteCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TSpriteCollection.Add: TSpriteCollectionItem;
begin
  Result := TSpriteCollectionItem(inherited Add);
  Result.FOwner := FOwner;
  Result.FOwnerItem := FOwnerItem;
end;

destructor TSpriteCollection.Destroy;
begin
  Finalize;
  inherited;
end;

end.
