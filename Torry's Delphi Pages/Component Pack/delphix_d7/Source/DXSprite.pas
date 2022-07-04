unit DXSprite;

interface
                                    
{$INCLUDE DelphiXcfg.inc}

uses
  Windows, SysUtils, Classes, DXClass, DXDraws, DirectX;

type

  {  ESpriteError  }

  ESpriteError = class(Exception);

  {  TSprite  }

  TSpriteEngine = class;

  TSprite = class
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
    procedure Add(Sprite: TSprite);
    procedure Remove(Sprite: TSprite);
    procedure AddDrawList(Sprite: TSprite);
    procedure Collision2;
    procedure Draw;
    function GetClientRect: TRect;
    function GetCount: Integer;
    function GetItem(Index: Integer): TSprite;
    function GetWorldX: Double;
    function GetWorldY: Double;
    procedure SetZ(Value: Integer);
  protected
    procedure DoCollision(Sprite: TSprite; var Done: Boolean); virtual;
    procedure DoDraw; virtual;
    procedure DoMove(MoveCount: Integer); virtual;
    function GetBoundsRect: TRect; virtual;
    function TestCollision(Sprite: TSprite): Boolean; virtual;
  public
    constructor Create(AParent: TSprite); virtual;
    destructor Destroy; override;
    procedure Clear;
    function Collision: Integer;
    procedure Dead;
    procedure Move(MoveCount: Integer);
    function GetSpriteAt(X, Y: Integer): TSprite;
    property BoundsRect: TRect read GetBoundsRect;
    property ClientRect: TRect read GetClientRect;
    property Collisioned: Boolean read FCollisioned write FCollisioned;
    property Count: Integer read GetCount;
    property Engine: TSpriteEngine read FEngine;
    property Items[Index: Integer]: TSprite read GetItem; default;
    property Moved: Boolean read FMoved write FMoved;
    property Parent: TSprite read FParent;
    property Visible: Boolean read FVisible write FVisible;
    property Width: Integer read FWidth write FWidth;
    property WorldX: Double read GetWorldX;
    property WorldY: Double read GetWorldY;
    property Height: Integer read FHeight write FHeight;
    property X: Double read FX write FX;
    property Y: Double read FY write FY;
    property Z: Integer read FZ write SetZ;
  end;

  {  TImageSprite  }

  TImageSprite = class(TSprite)
  private
    FAnimCount: Integer;
    FAnimLooped: Boolean;
    FAnimPos: Double;
    FAnimSpeed: Double;
    FAnimStart: Integer;
    FImage: TPictureCollectionItem;
    FPixelCheck: Boolean;
    FTile: Boolean;
    FTransparent: Boolean;
    function GetDrawImageIndex: Integer;
    function GetDrawRect: TRect;
  protected
    procedure DoDraw; override;
    procedure DoMove(MoveCount: Integer); override;
    function GetBoundsRect: TRect; override;
    function TestCollision(Sprite: TSprite): Boolean; override;
  public
    constructor Create(AParent: TSprite); override;
    property AnimCount: Integer read FAnimCount write FAnimCount;
    property AnimLooped: Boolean read FAnimLooped write FAnimLooped;
    property AnimPos: Double read FAnimPos write FAnimPos;
    property AnimSpeed: Double read FAnimSpeed write FAnimSpeed;
    property AnimStart: Integer read FAnimStart write FAnimStart;
    property PixelCheck: Boolean read FPixelCheck write FPixelCheck;
    property Image: TPictureCollectionItem read FImage write FImage;
    property Tile: Boolean read FTile write FTile;
  end;

  {  TImageSpriteEx  }

  TImageSpriteEx = class(TImageSprite)
  private
    FAngle: Integer;
    FAlpha: Integer;
  protected
    procedure DoDraw; override;
    function GetBoundsRect: TRect; override;
    function TestCollision(Sprite: TSprite): Boolean; override;
  public
    constructor Create(AParent: TSprite); override;
    property Angle: Integer read FAngle write FAngle;
    property Alpha: Integer read FAlpha write FAlpha;
  end;
                      
  {  TBackgroundSprite  }

  TBackgroundSprite = class(TSprite)
  private
    FImage: TPictureCollectionItem;
    FCollisionMap: Pointer;
    FMap: Pointer;
    FMapWidth: Integer;
    FMapHeight: Integer;
    FTile: Boolean;
    function GetCollisionMapItem(X, Y: Integer): Boolean;
    function GetChip(X, Y: Integer): Integer;
    procedure SetChip(X, Y: Integer; Value: Integer);
    procedure SetCollisionMapItem(X, Y: Integer; Value: Boolean);
    procedure SetMapHeight(Value: Integer);
    procedure SetMapWidth(Value: Integer);
  protected
    procedure DoDraw; override;
    function GetBoundsRect: TRect; override;
    function TestCollision(Sprite: TSprite): Boolean; override;
  public
    constructor Create(AParent: TSprite); override;
    destructor Destroy; override;
    procedure SetMapSize(AMapWidth, AMapHeight: Integer);
    property Chips[X, Y: Integer]: Integer read GetChip write SetChip;
    property CollisionMap[X, Y: Integer]: Boolean read GetCollisionMapItem write SetCollisionMapItem;
    property Image: TPictureCollectionItem read FImage write FImage;
    property MapHeight: Integer read FMapHeight write SetMapHeight;
    property MapWidth: Integer read FMapWidth write SetMapWidth;
    property Tile: Boolean read FTile write FTile;
  end;

  {  TSpriteEngine  }

  TSpriteEngine = class(TSprite)
  private
    FAllCount: Integer;
    FCollisionCount: Integer;
    FCollisionDone: Boolean;
    FCollisionRect: TRect;
    FCollisionSprite: TSprite;
    FDeadList: TList;
    FDrawCount: Integer;
    FSurface: TDirectDrawSurface;
    FSurfaceRect: TRect;
    procedure SetSurface(Value: TDirectDrawSurface);
  public
    constructor Create(AParent: TSprite); override;
    destructor Destroy; override;
    procedure Dead;
    procedure Draw;
    property AllCount: Integer read FAllCount;
    property DrawCount: Integer read FDrawCount;
    property Surface: TDirectDrawSurface read FSurface write SetSurface;
    property SurfaceRect: TRect read FSurfaceRect;
  end;

  {  EDXSpriteEngineError  }

  EDXSpriteEngineError = class(Exception);

  {  TCustomDXSpriteEngine  }

  TCustomDXSpriteEngine = class(TComponent)
  private
    FDXDraw: TCustomDXDraw;
    FEngine: TSpriteEngine;
    procedure DXDrawNotifyEvent(Sender: TCustomDXDraw; NotifyType: TDXDrawNotifyType);
    procedure SetDXDraw(Value: TCustomDXDraw);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    procedure Dead;
    procedure Draw;
    procedure Move(MoveCount: Integer);
    property DXDraw: TCustomDXDraw read FDXDraw write SetDXDraw;
    property Engine: TSpriteEngine read FEngine;                
  end;

  {  TDXSpriteEngine  }

  TDXSpriteEngine = class(TCustomDXSpriteEngine)
  published
    property DXDraw;
  end;

implementation

uses DXConsts;

function Mod2(i, i2: Integer): Integer;
begin
  Result := i mod i2;
  if Result<0 then
    Result := i2+Result;
end;

function Mod2f(i: Double; i2: Integer): Double;
begin
  if i2=0 then
    Result := i
  else
  begin
    Result := i-Trunc(i/i2)*i2;
    if Result<0 then
      Result := i2+Result;
  end;
end;

{  TSprite  }

constructor TSprite.Create(AParent: TSprite);
begin
  inherited Create;
  FParent := AParent;
  if FParent<>nil then
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
  Clear;
  if FParent<>nil then
  begin
    Dec(FEngine.FAllCount);
    FParent.Remove(Self);
    FEngine.FDeadList.Remove(Self);
  end;
  FList.Free;
  FDrawList.Free;
  inherited Destroy;
end;

procedure TSprite.Add(Sprite: TSprite);
begin
  if FList=nil then
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
  if FList.Count=0 then
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
    C := TSprite(FDrawList[I]).Z-Sprite.Z;
    if C < 0 then L := I + 1 else
      H := I - 1;
  end;
  FDrawList.Insert(L, Sprite);
end;

procedure TSprite.Clear;
begin
  while Count>0 do
    Items[Count-1].Free;
end;

function TSprite.Collision: Integer;
var
  i: Integer;
begin
  Result := 0;
  if (FEngine<>nil) and (not FDeaded) and (Collisioned) then
  begin
    with FEngine do
    begin
      FCollisionCount := 0;
      FCollisionDone := False;
      FCollisionRect := Self.BoundsRect;
      FCollisionSprite := Self;

      for i:=0 to Count-1 do
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
    if (Self<>FEngine.FCollisionSprite) and OverlapRect(BoundsRect, FEngine.FCollisionRect) and
      FEngine.FCollisionSprite.TestCollision(Self) and TestCollision(FEngine.FCollisionSprite) then
    begin
      Inc(FEngine.FCollisionCount);
      FEngine.FCollisionSprite.DoCollision(Self, FEngine.FCollisionDone);
      if (not FEngine.FCollisionSprite.Collisioned) or (FEngine.FCollisionSprite.FDeaded) then
      begin
        FEngine.FCollisionDone := True;
      end;
    end;
    if FEngine.FCollisionDone then Exit;
    for i:=0 to Count-1 do
      Items[i].Collision2;
  end;
end;

procedure TSprite.Dead;
begin
  if (FEngine<>nil) and (not FDeaded) then
  begin
    FDeaded := True;
    FEngine.FDeadList.Add(Self);
  end;
end;

procedure TSprite.DoMove;
begin
end;

procedure TSprite.DoDraw;
begin
end;

procedure TSprite.DoCollision(Sprite: TSprite; var Done: Boolean);
begin
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
    DoMove(MoveCount);
    for i:=0 to Count-1 do
      Items[i].Move(MoveCount);
  end;
end;

procedure TSprite.Draw;
var
  i: Integer;
begin
  if FVisible then
  begin
    if FEngine<>nil then
    begin
      if OverlapRect(FEngine.FSurfaceRect, BoundsRect) then
      begin
        DoDraw;
        Inc(FEngine.FDrawCount);
      end;
    end;

    if FDrawList<>nil then
    begin
      for i:=0 to FDrawList.Count-1 do
        TSprite(FDrawList[i]).Draw;
    end;
  end;
end;

function TSprite.GetSpriteAt(X, Y: Integer): TSprite;

  procedure Collision_GetSpriteAt(X, Y: Double; Sprite: TSprite);
  var
    i: Integer;
    X2, Y2: Double;
  begin
    if Sprite.Visible and PointInRect(Point(Round(X), Round(Y)), Bounds(Round(Sprite.X), Round(Sprite.Y), Sprite.Width, Sprite.Width)) then
    begin
      if (Result=nil) or (Sprite.Z>Result.Z) then
        Result := Sprite;
    end;

    X2 := X-Sprite.X;
    Y2 := Y-Sprite.Y;
    for i:=0 to Sprite.Count-1 do
      Collision_GetSpriteAt(X2, Y2, Sprite.Items[i]);
  end;

var
  i: Integer;
  X2, Y2: Double;
begin
  Result := nil;

  X2 := X-Self.X;
  Y2 := Y-Self.Y;
  for i:=0 to Count-1 do
    Collision_GetSpriteAt(X2, Y2, Items[i]);
end;                                    

function TSprite.GetBoundsRect: TRect;
begin
  Result := Bounds(Trunc(WorldX), Trunc(WorldY), Width, Height);
end;

function TSprite.GetClientRect: TRect;
begin
  Result := Bounds(0, 0, Width, Height);
end;

function TSprite.GetCount: Integer;
begin
  if FList<>nil then
    Result := FList.Count
  else
    Result := 0;
end;

function TSprite.GetItem(Index: Integer): TSprite;
begin
  if FList<>nil then
    Result := FList[Index]
  else
    raise ESpriteError.CreateFmt(SListIndexError, [Index]);
end;           

function TSprite.GetWorldX: Double;
begin
  if Parent<>nil then
    Result := Parent.WorldX+FX
  else
    Result := FX;
end;

function TSprite.GetWorldY: Double;
begin
  if Parent<>nil then
    Result := Parent.WorldY+FY
  else
    Result := FY;
end;

procedure TSprite.SetZ(Value: Integer);
begin
  if FZ<>Value then
  begin
    FZ := Value;
    if Parent<>nil then
    begin
      Parent.FDrawList.Remove(Self);
      Parent.AddDrawList(Self);
    end;
  end;
end;

{  TImageSprite  }

constructor TImageSprite.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  FTransparent := True;
end;

function TImageSprite.GetBoundsRect: TRect;
var
  dx, dy: Integer;
begin
  dx := Trunc(WorldX);
  dy := Trunc(WorldY);
  if FTile then
  begin
    dx := Mod2(dx, FEngine.SurfaceRect.Right+Width);
    dy := Mod2(dy, FEngine.SurfaceRect.Bottom+Height);

    if dx>FEngine.SurfaceRect.Right then
      dx := (dx-FEngine.SurfaceRect.Right)-Width;

    if dy>FEngine.SurfaceRect.Bottom then
      dy := (dy-FEngine.SurfaceRect.Bottom)-Height;
  end;

  Result := Bounds(dx, dy, Width, Height);
end;

procedure TImageSprite.DoMove(MoveCount: Integer);
begin
  FAnimPos := FAnimPos + FAnimSpeed*MoveCount;

  if FAnimLooped then
  begin
    if FAnimCount>0 then
      FAnimPos := Mod2f(FAnimPos, FAnimCount)
    else
      FAnimPos := 0;
  end else
  begin
    if FAnimPos>=FAnimCount then
    begin
      FAnimPos := FAnimCount-1;
      FAnimSpeed := 0;
    end;
    if FAnimPos<0 then
    begin
      FAnimPos := 0;
      FAnimSpeed := 0;
    end;
  end;
end;

function TImageSprite.GetDrawImageIndex: Integer;
begin
  Result := FAnimStart+Trunc(FAnimPos);
end;

function TImageSprite.GetDrawRect: TRect;
begin
  Result := BoundsRect;
  OffsetRect(Result, (Width-Image.Width) div 2, (Height-Image.Height) div 2);
end;

procedure TImageSprite.DoDraw;
var
  ImageIndex: Integer;
  r: TRect;
begin
  ImageIndex := GetDrawImageIndex;
  r := GetDrawRect;
  Image.Draw(FEngine.Surface, r.Left, r.Top, ImageIndex);
end;

function ImageCollisionTest(suf1, suf2: TDirectDrawSurface; const rect1, rect2: TRect;
  x1,y1,x2,y2: Integer; DoPixelCheck: Boolean): Boolean;

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
    R, G, B: Byte;
  end;
var
  ddsd1, ddsd2: TDDSurfaceDesc;
  r1, r2: TRect;
  tc1, tc2: DWORD;
  x, y, w, h: Integer;
  P1, P2: Pointer;
begin
  r1 := rect1;
  with rect2 do r2 := Bounds(x2-x1, y2-y1, Right-Left, Bottom-Top);

  Result := OverlapRect(r1, r2);

  if (suf1=nil) or (suf2=nil) then Exit;

  if DoPixelCheck and Result then
  begin
    {  Get Overlapping rectangle  }
    with r1 do r1 := Bounds(Max(x2-x1, 0), Max(y2-y1, 0), Right-Left, Bottom-Top);
    with r2 do r2 := Bounds(Max(x1-x2, 0), Max(y1-y2, 0), Right-Left, Bottom-Top);

    ClipRect(r1, rect1);
    ClipRect(r2, rect2);

    w := Min(r1.Right-r1.Left, r2.Right-r2.Left);
    h := Min(r1.Bottom-r1.Top, r2.Bottom-r2.Top);

    ClipRect(r1, bounds(r1.Left, r1.Top, w, h));
    ClipRect(r2, bounds(r2.Left, r2.Top, w, h));
                               
    {  Pixel check !!!  }
    ddsd1.dwSize := SizeOf(ddsd1);
    if suf1.Lock(r1, ddsd1) then
    begin
      try
        ddsd2.dwSize := SizeOf(ddsd2);
        if (suf1=suf2) or suf2.Lock(r2, ddsd2) then
        begin
          try
            if suf1=suf2 then ddsd2 := ddsd1;
            if ddsd1.ddpfPixelFormat.dwRGBBitCount<>ddsd2.ddpfPixelFormat.dwRGBBitCount then Exit;
                                     
            {  Get transparent color  }
            tc1 := ddsd1.ddckCKSrcBlt.dwColorSpaceLowValue;
            tc2 := ddsd2.ddckCKSrcBlt.dwColorSpaceLowValue;

            case ddsd1.ddpfPixelFormat.dwRGBBitCount of
              8 : begin
                    for y:=0 to h-1 do
                    begin
                      P1 := Pointer(Integer(ddsd1.lpSurface)+y*ddsd1.lPitch);
                      P2 := Pointer(Integer(ddsd2.lpSurface)+y*ddsd2.lPitch);
                      for x:=0 to w-1 do
                      begin
                        if (PByte(P1)^<>tc1) and (PByte(P2)^<>tc2) then Exit;
                        Inc(PByte(P1));
                        Inc(PByte(P2));
                      end;
                    end;
                  end;
              16: begin
                    for y:=0 to h-1 do
                    begin
                      P1 := Pointer(Integer(ddsd1.lpSurface)+y*ddsd1.lPitch);
                      P2 := Pointer(Integer(ddsd2.lpSurface)+y*ddsd2.lPitch);
                      for x:=0 to w-1 do
                      begin
                        if (PWord(P1)^<>tc1) and (PWord(P2)^<>tc2) then Exit;
                        Inc(PWord(P1));
                        Inc(PWord(P2));
                      end;
                    end;
                  end;
              24: begin
                    for y:=0 to h-1 do
                    begin
                      P1 := Pointer(Integer(ddsd1.lpSurface)+y*ddsd1.lPitch);
                      P2 := Pointer(Integer(ddsd2.lpSurface)+y*ddsd2.lPitch);
                      for x:=0 to w-1 do
                      begin        
                        if ((PRGB(P1)^.R shl 16) or (PRGB(P1)^.G shl 8) or PRGB(P1)^.B<>tc1) and
                          ((PRGB(P2)^.R shl 16) or (PRGB(P2)^.G shl 8) or PRGB(P2)^.B<>tc2) then Exit;
                        Inc(PRGB(P1));
                        Inc(PRGB(P2));
                      end;
                    end;
                  end;
              32: begin
                    for y:=0 to h-1 do
                    begin
                      P1 := Pointer(Integer(ddsd1.lpSurface)+y*ddsd1.lPitch);
                      P2 := Pointer(Integer(ddsd2.lpSurface)+y*ddsd2.lPitch);
                      for x:=0 to w-1 do
                      begin
                        if (PDWORD(P1)^ and $FFFFFF<>tc1) and (PDWORD(P2)^ and $FFFFFF<>tc2) then Exit;
                        Inc(PDWORD(P1));
                        Inc(PDWORD(P2));
                      end;
                    end;
                  end;
            end;
          finally
            if suf1<>suf2 then suf2.UnLock;
          end;
        end;
      finally
        suf1.UnLock;
      end;
    end;

    Result := False;
  end;
end;

function TImageSprite.TestCollision(Sprite: TSprite): Boolean;
var
  img1, img2: Integer;
  b1, b2: TRect;
begin
  if (Sprite is TImageSprite) and FPixelCheck then
  begin
    b1 := GetDrawRect;
    b2 := TImageSprite(Sprite).GetDrawRect;

    img1 := GetDrawImageIndex;
    img2 := TImageSprite(Sprite).GetDrawImageIndex;

    Result := ImageCollisionTest(Image.PatternSurfaces[img1], TImageSprite(Sprite).Image.PatternSurfaces[img2],
      Image.PatternRects[img1], TImageSprite(Sprite).Image.PatternRects[img2],
      b1.Left, b1.Top, b2.Left, b2.Top, True);
  end else
    Result := inherited TestCollision(Sprite);
end;

{  TImageSpriteEx  }

constructor TImageSpriteEx.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  FAlpha := 255;
end;

procedure TImageSpriteEx.DoDraw;
var
  r: TRect;
begin
  r := Bounds(Trunc(WorldX), Trunc(WorldY), Width, Height);

  if FAngle and $FF=0 then
  begin
    if FAlpha<255 then
    begin
      Image.DrawAlpha(FEngine.FSurface, r, GetDrawImageIndex, FAlpha)
    end else
    begin
      Image.StretchDraw(FEngine.FSurface, r, GetDrawImageIndex);
    end;
  end else
  begin
    if FAlpha<255 then
    begin
      Image.DrawRotateAlpha(FEngine.FSurface, (r.Left+r.Right) div 2, (r.Top+r.Bottom) div 2,
        Width, Height, GetDrawImageIndex, 0.5, 0.5, FAngle, FAlpha)
    end else
    begin
      Image.DrawRotate(FEngine.FSurface, (r.Left+r.Right) div 2, (r.Top+r.Bottom) div 2,
        Width, Height, GetDrawImageIndex, 0.5, 0.5, FAngle)
    end;
  end;
end;

function TImageSpriteEx.GetBoundsRect: TRect;
begin
  Result := FEngine.SurfaceRect;
end;

function TImageSpriteEx.TestCollision(Sprite: TSprite): Boolean;
begin
  if Sprite is TImageSpriteEx then
  begin
    Result := OverlapRect(Bounds(Trunc(Sprite.WorldX), Trunc(Sprite.WorldY), Sprite.Width, Sprite.Height),
      Bounds(Trunc(WorldX), Trunc(WorldY), Width, Height));
  end else
  begin
    Result := OverlapRect(Sprite.BoundsRect, Bounds(Trunc(WorldX), Trunc(WorldY), Width, Height));
  end;
end;

{  TBackgroundSprite  }

constructor TBackgroundSprite.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  Collisioned := False;
end;

destructor TBackgroundSprite.Destroy;
begin
  SetMapSize(0, 0);
  inherited Destroy;
end;

procedure TBackgroundSprite.DoDraw;
var
  _x, _y, cx, cy, cx2, cy2, c, ChipWidth, ChipHeight: Integer;
  StartX, StartY, EndX, EndY, StartX_, StartY_, OfsX, OfsY, dWidth, dHeight: Integer;
  r: TRect;
begin
  if Image=nil then Exit;

  if (FMapWidth<=0) or (FMapHeight<=0) then Exit;

  r := Image.PatternRects[0];
  ChipWidth := r.Right-r.Left;
  ChipHeight := r.Bottom-r.Top;

  dWidth := (FEngine.SurfaceRect.Right+ChipWidth) div ChipWidth+1;
  dHeight := (FEngine.SurfaceRect.Bottom+ChipHeight) div ChipHeight+1;

  _x := Trunc(WorldX);
  _y := Trunc(WorldY);

  OfsX := _x mod ChipWidth;
  OfsY := _y mod ChipHeight;

  StartX := _x div ChipWidth;
  StartX_ := 0;

  if StartX<0 then
  begin
    StartX_ := -StartX;
    StartX := 0;
  end;

  StartY := _y div ChipHeight;
  StartY_ := 0;

  if StartY<0 then
  begin
    StartY_ := -StartY;
    StartY := 0;
  end;

  EndX := Min(StartX+FMapWidth-StartX_, dWidth);
  EndY := Min(StartY+FMapHeight-StartY_, dHeight);

  if FTile then
  begin
    for cy:=-1 to dHeight do
    begin
      cy2 := Mod2((cy-StartY+StartY_), FMapHeight);
      for cx:=-1 to dWidth do
      begin
        cx2 := Mod2((cx-StartX+StartX_), FMapWidth);
        c := Chips[cx2, cy2];
        if c>=0 then
          Image.Draw(FEngine.Surface, cx*ChipWidth+OfsX, cy*ChipHeight+OfsY, c);
      end;
    end;
  end else
  begin
    for cy:=StartY to EndY-1 do
      for cx:=StartX to EndX-1 do
      begin
        c := Chips[cx-StartX+StartX_, cy-StartY+StartY_];
        if c>=0 then
          Image.Draw(FEngine.Surface, cx*ChipWidth+OfsX, cy*ChipHeight+OfsY, c);
      end;
  end;
end;

function TBackgroundSprite.TestCollision(Sprite: TSprite): Boolean;
var
  b, b1, b2: TRect;
  cx, cy, ChipWidth, ChipHeight: Integer;
  r: TRect;
begin
  Result := True;
  if Image=nil then Exit;
  if (FMapWidth<=0) or (FMapHeight<=0) then Exit;

  r := Image.PatternRects[0];
  ChipWidth := r.Right-r.Left;
  ChipHeight := r.Bottom-r.Top;



  b1 := Sprite.BoundsRect;
  b2 := BoundsRect;

  IntersectRect(b, b1, b2);

  OffsetRect(b, -Trunc(WorldX), -Trunc(WorldY));
  OffsetRect(b1, -Trunc(WorldX), -Trunc(WorldY));

  for cy:=(b.Top-ChipHeight+1) div ChipHeight to b.Bottom div ChipHeight do
    for cx:=(b.Left-ChipWidth+1) div ChipWidth to b.Right div ChipWidth do
      if CollisionMap[Mod2(cx, MapWidth), Mod2(cy, MapHeight)] then
      begin
        if OverlapRect(Bounds(cx*ChipWidth, cy*ChipHeight, ChipWidth, ChipHeight), b1) then Exit;
      end;

  Result := False;
end;

function TBackgroundSprite.GetChip(X, Y: Integer): Integer;
begin
  if (X>=0) and (X<FMapWidth) and (Y>=0) and (Y<FMapHeight) then
    Result := PInteger(Integer(FMap)+(Y*FMapWidth+X)*SizeOf(Integer))^
  else
    Result := -1;
end;

type
  PBoolean = ^Boolean;

function TBackgroundSprite.GetCollisionMapItem(X, Y: Integer): Boolean;
begin
  if (X>=0) and (X<FMapWidth) and (Y>=0) and (Y<FMapHeight) then
    Result := PBoolean(Integer(FCollisionMap)+(Y*FMapWidth+X)*SizeOf(Boolean))^
  else
    Result := False;
end;

function TBackgroundSprite.GetBoundsRect: TRect;
begin
  if FTile then
    Result := FEngine.SurfaceRect
  else
  begin
    if Image<>nil then
      Result := Bounds(Trunc(WorldX), Trunc(WorldY),
        Image.Width*FMapWidth, Image.Height*FMapHeight)
    else
      Result := Rect(0, 0, 0, 0);
  end;
end;

procedure TBackgroundSprite.SetChip(X, Y: Integer; Value: Integer);
begin
  if (X>=0) and (X<FMapWidth) and (Y>=0) and (Y<FMapHeight) then
    PInteger(Integer(FMap)+(Y*FMapWidth+X)*SizeOf(Integer))^ := Value;
end;

procedure TBackgroundSprite.SetCollisionMapItem(X, Y: Integer; Value: Boolean);
begin
  if (X>=0) and (X<FMapWidth) and (Y>=0) and (Y<FMapHeight) then
    PBoolean(Integer(FCollisionMap)+(Y*FMapWidth+X)*SizeOf(Boolean))^ := Value;
end;

procedure TBackgroundSprite.SetMapHeight(Value: Integer);
begin
  SetMapSize(FMapWidth, Value);
end;

procedure TBackgroundSprite.SetMapWidth(Value: Integer);
begin
  SetMapSize(Value, FMapHeight);
end;

procedure TBackgroundSprite.SetMapSize(AMapWidth, AMapHeight: Integer);
begin
  if (FMapWidth<>AMapWidth) or (FMapHeight<>AMapHeight) then
  begin
    if (AMapWidth<=0) or (AMapHeight<=0) then
    begin
      AMapWidth := 0;
      AMapHeight := 0;
    end;
    FMapWidth := AMapWidth;
    FMapHeight := AMapHeight;
    ReAllocMem(FMap, FMapWidth*FMapHeight*SizeOf(Integer));
    FillChar(FMap^, FMapWidth*FMapHeight*SizeOf(Integer), 0);

    ReAllocMem(FCollisionMap, FMapWidth*FMapHeight*SizeOf(Boolean));
    FillChar(FCollisionMap^, FMapWidth*FMapHeight*SizeOf(Boolean), 1);
  end;
end;

{  TSpriteEngine  }

constructor TSpriteEngine.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  FDeadList := TList.Create;
end;

destructor TSpriteEngine.Destroy;
begin
  FDeadList.Free;
  inherited Destroy;
end;

procedure TSpriteEngine.Dead;
begin
  while FDeadList.Count>0 do
    TSprite(FDeadList[FDeadList.Count-1]).Free;
end;

procedure TSpriteEngine.Draw;
begin
  FDrawCount := 0;
  inherited Draw;
end;

procedure TSpriteEngine.SetSurface(Value: TDirectDrawSurface);
begin
  FSurface := Value;
  if FSurface<>nil then
  begin
    FSurfaceRect := Surface.ClientRect;
    Width := FSurfaceRect.Right-FSurfaceRect.Left;
    Height := FSurfaceRect.Bottom-FSurfaceRect.Top;
  end;
end;

{  TCustomDXSpriteEngine  }

constructor TCustomDXSpriteEngine.Create(AOnwer: TComponent);
begin
  inherited Create(AOnwer);
  FEngine := TSpriteEngine.Create(nil);
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
  if (Operation=opRemove) and (DXDraw=AComponent) then
    DXDraw := nil;
end;

procedure TCustomDXSpriteEngine.Dead;
begin
  FEngine.Dead;
end;

procedure TCustomDXSpriteEngine.Draw;
begin
  if (FDXDraw<>nil) and (FDXDraw.Initialized) then
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
    dxntFinalize  : FEngine.Surface := nil;
  end;
end;

procedure TCustomDXSpriteEngine.SetDXDraw(Value: TCustomDXDraw);
begin
  if FDXDraw<>nil then
    FDXDraw.UnRegisterNotifyEvent(DXDrawNotifyEvent);

  FDXDraw := Value;

  if FDXDraw<>nil then
    FDXDraw.RegisterNotifyEvent(DXDrawNotifyEvent);
end;

end.
