unit DXClass;

interface

{$INCLUDE DelphiXcfg.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, MMSystem, Graphics, {$IFDEF _DMO_}MultiMon,{$ENDIF}
{$IfDef StandardDX}
  {$IfDef DX9}
  Direct3D, DirectInput,
  {$EndIf}
  DirectDraw, DirectSound;
{$Else}
  DirectX;
{$EndIf}

type

  {  EDirectDrawError  }

  EDirectXError = class(Exception);

  {  TDirectX  }

  TDirectX = class(TPersistent)
  private
    procedure SetDXResult(Value: HRESULT);
  protected
    FDXResult: HRESULT;
    procedure Check; virtual;
  public
    property DXResult: HRESULT read FDXResult write SetDXResult;
  end;

  {  TDirectXDriver  }

  TDirectXDriver = class(TCollectionItem)
  private
    FGUID: PGUID;
    FGUID2: TGUID;
    FDescription: string;
    FDriverName: string;
    procedure SetGUID(Value: PGUID);
  public
    property GUID: PGUID read FGUID write SetGUID;
    property Description: string read FDescription write FDescription;
    property DriverName: string read FDriverName write FDriverName;
  end;

  {  TDirectXDrivers  }

  TDirectXDrivers = class(TCollection)
  private
    function GetDriver(Index: Integer): TDirectXDriver;
  public
    constructor Create;
    property Drivers[Index: Integer]: TDirectXDriver read GetDriver; default;
  end;

  {$IFDEF _DMO_}
  {  TDirectXDriverEx  }

  TDirectXDriverEx = class(TCollectionItem)
  private
    FGUID: PGUID;
    FGUID2: TGUID;
    FDescription: string;
    FDriverName: string;
    FMonitor: HMonitor;
    FMonitorInfo: TMonitorInfo;
    procedure SetGUID(Value: PGUID);
    function ConvertHMonitor(iMonitor: HMonitor): TMonitorInfo;
    function GetMonitorInfo: TMonitorInfo;
    function GetFlags: DWORD;
    function GetTempSpace: TRect;
    function GetWorkSpace: TRect;
  public
    property GUID: PGUID read FGUID write SetGUID;
    property Monitor: HMonitor read FMonitor write FMonitor;
    property MonitorInfo: TMonitorInfo read GetMonitorInfo;
  published
    property Description: string read FDescription write FDescription;
    property DriverName: string read FDriverName write FDriverName;
    property WorkSpace: TRect read GetWorkSpace;
    property TempSpace: TRect read GetTempSpace;
    property Flags: DWORD read GetFlags;
  end;

  {  TDirectXDriversEx  }

  TDirectXDriversEx = class(TCollection)
  private
    function GetDriver(Index: Integer): TDirectXDriverEx;
  public
    constructor Create;
    property Drivers[Index: Integer]: TDirectXDriverEx read GetDriver; default;
  end;
  {$ENDIF}
  
  {  TDXForm  }

  TDXForm = class(TForm)
  private
    FStoreWindow: Boolean;
    FWindowPlacement: TWindowPlacement;
    procedure WMSYSCommand(var Msg: TWMSYSCommand); message WM_SYSCOMMAND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    procedure RestoreWindow;
    procedure StoreWindow;
  end;

  {  TCustomDXTimer  }

  TDXTimerEvent = procedure(Sender: TObject; LagCount: Integer) of object;

  TCustomDXTimer = class(TComponent)
  private
    FActiveOnly: Boolean;
    FEnabled: Boolean;
    FFrameRate: Integer;
    FInitialized: Boolean;
    FInterval: Cardinal;
    FInterval2: Cardinal;
    FNowFrameRate: Integer;
    FOldTime: DWORD;
    FOldTime2: DWORD;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnTimer: TDXTimerEvent;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    function AppProc(var Message: TMessage): Boolean;
    procedure Finalize;
    procedure Initialize;
    procedure Resume;
    procedure SetActiveOnly(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure Suspend;
  protected
    procedure DoActivate; virtual;
    procedure DoDeactivate; virtual;
    procedure DoTimer(LagCount: Integer); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ActiveOnly: Boolean read FActiveOnly write SetActiveOnly;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property FrameRate: Integer read FFrameRate;
    property Interval: Cardinal read FInterval write SetInterval;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnTimer: TDXTimerEvent read FOnTimer write FOnTimer;
  end;

  {  TDXTimer  }

  TDXTimer = class(TCustomDXTimer)
  published
    property ActiveOnly;
    property Enabled;
    property Interval;
    property OnActivate;
    property OnDeactivate;
    property OnTimer;
  end;

  {  TControlSubClass  }

  TControlSubClassProc = procedure(var Message: TMessage; DefWindowProc: TWndMethod) of object;

  TControlSubClass = class
  private
    FControl: TControl;
    FDefWindowProc: TWndMethod;
    FWindowProc: TControlSubClassProc;
    procedure WndProc(var Message: TMessage);
  public
    constructor Create(Control: TControl; WindowProc: TControlSubClassProc);
    destructor Destroy; override;
  end;

  {  THashCollectionItem  }

  THashCollectionItem = class(TCollectionItem)
  private
    FHashCode: Integer;
    FIndex: Integer;
    FName: string;
    FLeft: THashCollectionItem;
    FRight: THashCollectionItem;
    procedure SetName(const Value: string);
    procedure AddHash;
    procedure DeleteHash;
  protected
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Index: Integer read FIndex write SetIndex;
  published
    property Name: string read FName write SetName;
  end;

  {  THashCollection  }

  THashCollection = class(TCollection)
  private
    FHash: array[0..255] of THashCollectionItem;
  public
    function IndexOf(const Name: string): Integer;
  end;

{Addapted from RXLib.PicClip}

  { TPicClip }
  TCellRange = 1..MaxInt;

  TDXPictureClip = class(TComponent)
  private
    FPicture: TPicture;
    FRows: TCellRange;
    FCols: TCellRange;
    FBitmap: TBitmap;
    FMasked: Boolean;
    FMaskColor: TColor;
    FOnChange: TNotifyEvent;
    procedure CheckIndex(Index: Integer);
    function GetCell(Col, Row: Cardinal): TBitmap;
    function GetGraphicCell(Index: Integer): TBitmap;
    function GetDefaultMaskColor: TColor;
    function GetIsEmpty: Boolean;
    function GetCount: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function IsMaskStored: Boolean;
    procedure PictureChanged(Sender: TObject);
    procedure SetHeight(Value: Integer);
    procedure SetPicture(Value: TPicture);
    procedure SetWidth(Value: Integer);
    procedure SetMaskColor(Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetIndex(Col, Row: Cardinal): Integer;
    procedure Draw(Canvas: TCanvas; X, Y, Index: Integer);
    procedure DrawCenter(Canvas: TCanvas; Rect: TRect; Index: Integer);
    property Cells[Col, Row: Cardinal]: TBitmap read GetCell;
    property GraphicCell[Index: Integer]: TBitmap read GetGraphicCell;
    property IsEmpty: Boolean read GetIsEmpty;
    property Count: Integer read GetCount;
  published
    property Cols: TCellRange read FCols write FCols default 1;
    property Height: Integer read GetHeight write SetHeight stored False;
    property Masked: Boolean read FMasked write FMasked default True;
    property Rows: TCellRange read FRows write FRows default 1;
    property Picture: TPicture read FPicture write SetPicture;
    property MaskColor: TColor read FMaskColor write SetMaskColor stored IsMaskStored;
    property Width: Integer read GetWidth write SetWidth stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function Max(Val1, Val2: Integer): Integer; {$IFDEF VER9UP}inline;{$ENDIF}
function Min(Val1, Val2: Integer): Integer; {$IFDEF VER9UP}inline;{$ENDIF}

function Cos256(i: Integer): Double;
function Sin256(i: Integer): Double;

function PointInRect(const Point: TPoint; const Rect: TRect): Boolean; {$IFDEF VER9UP}inline;{$ENDIF}
function RectInRect(const Rect1, Rect2: TRect): Boolean; {$IFDEF VER9UP}inline;{$ENDIF}
function OverlapRect(const Rect1, Rect2: TRect): Boolean; {$IFDEF VER9UP}inline;{$ENDIF}

function WideRect(ALeft, ATop, AWidth, AHeight: Integer): TRect; {$IFDEF VER9UP}inline;{$ENDIF}

{ Transformations routines}

const
  L_Curve = 0;//The left curve
  R_Curve = 1;//The right curve

  C_Add = 0;//Increase (BTC)
  C_Dec = 1;//Decrease (ETC)

Type
  TDblPoint = packed record
    X, Y: Double;
  end;
  TSngPoint = packed record //SinglePoint
    X, Y: Single;
  end;


  //Transformation matrix
  T2DRowCol = Array[1..3] of Array[1..3] of Double;
  T2DVector = Array[1..3] of Double;
  //Distance between 2 points
  function Get2PointRange(a,b: TDblPoint):Double;
  //From vector angular calculation
  function Get256(dX,dY: Double):Double;
  //The angular calculation of the A from B
  function GetARadFromB(A,B: TDblPoint):Double;

  //It calculates the TDblPoint
  function DblPoint(a,b:Double):TDblPoint;
  //It converts the TDboPoint to the TPoint
  function TruncDblPoint(DblPos: TDblPoint): TPoint;

  function GetPointFromRangeAndAngle(SP: TDblPoint; Range,Angle: Double): TDblPoint;

  function Ini2DRowCol: T2DRowCol;
  function Trans2DRowCol(x,y:double):T2DRowCol;
  function Scale2DRowCol(x,y:double):T2DRowCol;
  function Rotate2DRowCol(Theta:double):T2DRowCol;
  function RotateIntoX2DRowCol(x,y: double):T2DRowCol;
  function Multiply2DRowCol(A,B:T2DRowCol):T2DRowCol;
  function ScaleAt2DRowCol(x,y,Sx,Sy:double):T2DRowCol;
  function ReflectAcross2DRowCol(x,y,dx,dy:Double): T2DRowCol;
  function Apply2DVector(V:T2DVector; M:T2DRowCol): T2DVector;
  function RotateAround2DRowCol(x,y,Theta:Double): T2DRowCol;

  //Collision decision
  function PointInCircle(PPos,CPos: TPoint; R: integer): Boolean;
  function CircleInCircle(C1Pos,C2Pos: TPoint; R1,R2:Integer): Boolean;
  function SegmentInCircle(SPos,EPos,CPos: TPoint; R: Integer): Boolean;

  //If A is closer than B from starting point S, the True is  returned.
  function CheckNearAThanB(S,A,B: TDblPoint): Boolean;

  //The Angle of 256 period is returned
  function Angle256(Angle: Single): Single;

{ Support functions }

procedure ReleaseCom(out Com);
function DXLoadLibrary(const FileName, FuncName: string): TFarProc;

{  Simple helper  }

procedure Log(const Co: string; const FName: string{$IFDEF VER4UP} = 'c:\logerr.txt'{$ENDIF});

implementation

uses DXConsts;

function Max(Val1, Val2: Integer): Integer;
begin
  if Val1>=Val2 then Result := Val1 else Result := Val2;
end;

function Min(Val1, Val2: Integer): Integer;
begin
  if Val1<=Val2 then Result := Val1 else Result := Val2;
end;

function PointInRect(const Point: TPoint; const Rect: TRect): Boolean;
begin
  Result := (Point.X >= Rect.Left) and
            (Point.X <= Rect.Right) and
            (Point.Y >= Rect.Top) and
            (Point.Y <= Rect.Bottom);
end;

function RectInRect(const Rect1, Rect2: TRect): Boolean;
begin
  Result := (Rect1.Left >= Rect2.Left) and
            (Rect1.Right <= Rect2.Right) and
            (Rect1.Top >= Rect2.Top) and
            (Rect1.Bottom <= Rect2.Bottom);
end;

function OverlapRect(const Rect1, Rect2: TRect): Boolean;
begin
  Result := (Rect1.Left < Rect2.Right) and
            (Rect1.Right > Rect2.Left) and
            (Rect1.Top < Rect2.Bottom) and
            (Rect1.Bottom > Rect2.Top);
end;

function WideRect(ALeft, ATop, AWidth, AHeight: Integer): TRect;
begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ALeft+AWidth;
    Bottom := ATop+AHeight;
  end;
end;

var
  CosinTable: array[0..255] of Double;

procedure InitCosinTable;
var
  i: Integer;
begin
  for i:=0 to 255 do
    CosinTable[i] := Cos((i/256)*2*PI);
end;

function Cos256(i: Integer): Double;
begin
  Result := CosinTable[i and 255];
end;

function Sin256(i: Integer): Double;
begin
  Result := CosinTable[(i+192) and 255];
end;

procedure ReleaseCom(out Com);
begin
end;

var
  LibList: TStringList;

function DXLoadLibrary(const FileName, FuncName: string): Pointer;
var
  i: Integer;
  h: THandle;
begin
  if LibList=nil then
    LibList := TStringList.Create;

  i := LibList.IndexOf(AnsiLowerCase(FileName));
  if i=-1 then
  begin
    {  DLL is loaded.  }
    h := LoadLibrary(PChar(FileName));
    if h=0 then
      raise Exception.CreateFmt(SDLLNotLoaded, [FileName]);
    LibList.AddObject(AnsiLowerCase(FileName), Pointer(h));
  end else
  begin
    {  DLL has already been loaded.  }
    h := THandle(LibList.Objects[i]);
  end;

  Result := GetProcAddress(h, PChar(FuncName));
  if Result=nil then
    raise Exception.CreateFmt(SDLLNotLoaded, [FileName]);
end;

procedure FreeLibList;
var
  i: Integer;
begin
  if LibList<>nil then
  begin
    for i:=0 to LibList.Count-1 do
      FreeLibrary(THandle(LibList.Objects[i]));
    LibList.Free;
  end;
end;

{  TDirectX  }

procedure TDirectX.Check;
begin
end;

procedure TDirectX.SetDXResult(Value: HRESULT);
begin
  FDXResult := Value;
  if FDXResult<>0 then Check;
end;

{  TDirectXDriver  }

procedure TDirectXDriver.SetGUID(Value: PGUID);
begin
  if not IsBadHugeReadPtr(Value, SizeOf(TGUID)) then
  begin
    FGUID2 := Value^;
    FGUID := @FGUID2;
  end else
    FGUID := Value;
end;

{  TDirectXDrivers  }

constructor TDirectXDrivers.Create;
begin
  inherited Create(TDirectXDriver);
end;

function TDirectXDrivers.GetDriver(Index: Integer): TDirectXDriver;
begin
  Result := (inherited Items[Index]) as TDirectXDriver;
end;

{  TDXForm  }

var
  SetAppExStyleCount: Integer;

constructor TDXForm.Create(AOnwer: TComponent);
var
  ExStyle: Integer;
begin
  inherited Create(AOnwer);
  Inc(SetAppExStyleCount);
  ExStyle := GetWindowLong(Application.Handle, GWL_EXSTYLE);
  ExStyle := ExStyle or WS_EX_TOOLWINDOW;
  SetWindowLong(Application.Handle, GWL_EXSTYLE, ExStyle);
end;

destructor TDXForm.Destroy;
var
  ExStyle: Integer;
begin
  Dec(SetAppExStyleCount);
  if SetAppExStyleCount=0 then
  begin
    ExStyle := GetWindowLong(Application.Handle, GWL_EXSTYLE);
    ExStyle := ExStyle and (not WS_EX_TOOLWINDOW);
    SetWindowLong(Application.Handle, GWL_EXSTYLE, ExStyle);
  end;
  inherited Destroy;
end;

procedure TDXForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TDXForm.RestoreWindow;
begin
  if FStoreWindow then
  begin
    SetWindowPlacement(Handle, @FWindowPlacement);
    FStoreWindow := False;
  end;
end;

procedure TDXForm.StoreWindow;
begin
  FWindowPlacement.Length := SizeOf(FWindowPlacement);
  FStoreWindow := GetWindowPlacement(Handle, @FWindowPlacement);
end;

procedure TDXForm.WMSYSCommand(var Msg: TWMSYSCommand);
begin
  if Msg.CmdType = SC_MINIMIZE then
  begin
    DefaultHandler(Msg);
    WindowState := wsMinimized;
  end else
    inherited;
end;

{  TCustomDXTimer  }

constructor TCustomDXTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveOnly := True;
  FEnabled := True;
  Interval := 1000;
  Application.HookMainWindow(AppProc);
end;

destructor TCustomDXTimer.Destroy;
begin
  Finalize;
  Application.UnHookMainWindow(AppProc);
  inherited Destroy;
end;

procedure TCustomDXTimer.AppIdle(Sender: TObject; var Done: Boolean);
var
  t, t2: DWORD;
  LagCount, i: Integer;
begin
  Done := False;

  t := TimeGetTime;
  t2 := t-FOldTime;
  if t2>=FInterval then
  begin
    FOldTime := t;

    LagCount := t2 div FInterval2;
    if LagCount<1 then LagCount := 1;

    Inc(FNowFrameRate);

    i := Max(t-FOldTime2, 1);
    if i>=1000 then
    begin
      FFrameRate := Round(FNowFrameRate*1000/i);
      FNowFrameRate := 0;
      FOldTime2 := t;
    end;

    DoTimer(LagCount);
  end;
end;

function TCustomDXTimer.AppProc(var Message: TMessage): Boolean;
begin
  Result := False;
  case Message.Msg of
    CM_ACTIVATE:
        begin
          DoActivate;
          if FInitialized and FActiveOnly then Resume;
        end;
    CM_DEACTIVATE:
        begin
          DoDeactivate;
          if FInitialized and FActiveOnly then Suspend;
        end;
  end;
end;

procedure TCustomDXTimer.DoActivate;
begin
  if Assigned(FOnActivate) then FOnActivate(Self);
end;

procedure TCustomDXTimer.DoDeactivate;
begin
  if Assigned(FOnDeactivate) then FOnDeactivate(Self);
end;

procedure TCustomDXTimer.DoTimer(LagCount: Integer);
begin
  if Assigned(FOnTimer) then FOnTimer(Self, LagCount);
end;

procedure TCustomDXTimer.Finalize;
begin
  if FInitialized then
  begin
    Suspend;
    FInitialized := False;
  end;
end;

procedure TCustomDXTimer.Initialize;
begin
  Finalize;

  if ActiveOnly then
  begin
    if Application.Active then
      Resume;
  end else
    Resume;
  FInitialized := True;
end;

procedure TCustomDXTimer.Loaded;
begin
  inherited Loaded;
  if (not (csDesigning in ComponentState)) and FEnabled then
    Initialize;
end;

procedure TCustomDXTimer.Resume;
begin
  FOldTime := TimeGetTime;
  FOldTime2 := TimeGetTime;
  Application.OnIdle := AppIdle;
end;

procedure TCustomDXTimer.SetActiveOnly(Value: Boolean);
begin
  if FActiveOnly<>Value then
  begin
    FActiveOnly := Value;

    if Application.Active and FActiveOnly then
      if FInitialized and FActiveOnly then Suspend;
  end;
end;

procedure TCustomDXTimer.SetEnabled(Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled := Value;
    if ComponentState*[csReading, csLoading]=[] then
      if FEnabled then Initialize else Finalize;
  end;
end;

procedure TCustomDXTimer.SetInterval(Value: Cardinal);
begin
  if FInterval<>Value then
  begin
    FInterval := Max(Value, 0);
    FInterval2 := Max(Value, 1);
  end;
end;

procedure TCustomDXTimer.Suspend;
begin
  Application.OnIdle := nil;
end;

{  TControlSubClass  }

constructor TControlSubClass.Create(Control: TControl;
  WindowProc: TControlSubClassProc);
begin
  inherited Create;
  FControl := Control;
  FDefWindowProc := FControl.WindowProc;
  FControl.WindowProc := WndProc;
  FWindowProc := WindowProc;
end;

destructor TControlSubClass.Destroy;
begin
  FControl.WindowProc := FDefWindowProc;
  inherited Destroy;
end;

procedure TControlSubClass.WndProc(var Message: TMessage);
begin
  FWindowProc(Message, FDefWindowProc);
end;

{  THashCollectionItem  }

function MakeHashCode(const Str: string): Integer;
var
  s: string;
begin
  s := AnsiLowerCase(Str);
  Result := Length(s)*16;
  if Length(s)>=2 then
    Result := Result + (Ord(s[1]) + Ord(s[Length(s)-1]));
  Result := Result and 255;
end;
               
constructor THashCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FIndex := inherited Index;
  AddHash;
end;

destructor THashCollectionItem.Destroy;
var
  i: Integer;
begin
  for i:=FIndex+1 to Collection.Count-1 do
    Dec(THashCollectionItem(Collection.Items[i]).FIndex);
  DeleteHash;
  inherited Destroy;
end;

procedure THashCollectionItem.Assign(Source: TPersistent);
begin
  if Source is THashCollectionItem then
  begin
    Name := THashCollectionItem(Source).Name;
  end else
    inherited Assign(Source);
end;

procedure THashCollectionItem.AddHash;
var
  Item: THashCollectionItem;
begin
  FHashCode := MakeHashCode(FName);

  Item := THashCollection(Collection).FHash[FHashCode];
  if Item<>nil then
  begin
    Item.FLeft := Self;
    Self.FRight := Item;
  end;

  THashCollection(Collection).FHash[FHashCode] := Self;
end;

procedure THashCollectionItem.DeleteHash;
begin
  if FLeft<>nil then
  begin
    FLeft.FRight := FRight;
    if FRight<>nil then
      FRight.FLeft := FLeft;
  end else
  begin
    if FHashCode<>-1 then
    begin
      THashCollection(Collection).FHash[FHashCode] := FRight;
      if FRight<>nil then
        FRight.FLeft := nil;
    end;
  end;
  FLeft := nil;
  FRight := nil;
end;

function THashCollectionItem.GetDisplayName: string;
begin
  Result := Name;
  if Result='' then Result := inherited GetDisplayName;
end;

procedure THashCollectionItem.SetIndex(Value: Integer);
begin
  if FIndex<>Value then
  begin
    FIndex := Value;
    inherited SetIndex(Value);
  end;
end;

procedure THashCollectionItem.SetName(const Value: string);
begin
  if FName<>Value then
  begin
    FName := Value;
    DeleteHash;
    AddHash;
  end;
end;

{  THashCollection  }

function THashCollection.IndexOf(const Name: string): Integer;
var
  Item: THashCollectionItem;
begin
  Item := FHash[MakeHashCode(Name)];
  while Item<>nil do
  begin
    if AnsiCompareText(Item.Name, Name)=0 then
    begin
      Result := Item.FIndex;
      Exit;
    end;
    Item := Item.FRight;
  end;
  Result := -1;
end;

{  TDXPictureClip  }

constructor TDXPictureClip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FBitmap := TBitmap.Create;
  FRows := 1;
  FCols := 1;
  FMaskColor := GetDefaultMaskColor;
  FMasked := True;
end;

destructor TDXPictureClip.Destroy;
begin
  FOnChange := nil;
  FPicture.OnChange := nil;
  FBitmap.Free;
  FPicture.Free;
  inherited Destroy;
end;

procedure TDXPictureClip.Assign(Source: TPersistent);
begin
  if Source is TDXPictureClip then begin
    with TDXPictureClip(Source) do begin
      Self.FRows := Rows;
      Self.FCols := Cols;
      Self.FMasked := Masked;
      Self.FMaskColor := MaskColor;
      Self.FPicture.Assign(FPicture);
    end;
  end
  else if (Source is TPicture) or (Source is TGraphic) then
    FPicture.Assign(Source)
  else inherited Assign(Source);
end;

type
  THack = class(TImageList);

procedure TDXPictureClip.AssignTo(Dest: TPersistent);
var
  I: Integer;
  SaveChange: TNotifyEvent;
begin
  if (Dest is TPicture) then Dest.Assign(FPicture)
  else if (Dest is TImageList) and not IsEmpty then begin
    with TImageList(Dest) do begin
      SaveChange := OnChange;
      try
        OnChange := nil;
        Clear;
        Width := Self.Width;
        Height := Self.Height;
        for I := 0 to Self.Count - 1 do begin
          if Self.Masked and (MaskColor <> clNone) then
            TImageList(Dest).AddMasked(GraphicCell[I], MaskColor)
          else TImageList(Dest).Add(GraphicCell[I], nil);
        end;
        Masked := Self.Masked;
      finally
        OnChange := SaveChange;
      end;
      THack(Dest).Change;
    end;
  end
  else inherited AssignTo(Dest);
end;

procedure TDXPictureClip.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TDXPictureClip.GetIsEmpty: Boolean;
begin
  Result := not Assigned(Picture) or Picture.Graphic.Empty;
end;

function TDXPictureClip.GetCount: Integer;
begin
  if IsEmpty then Result := 0
  else Result := Cols * Rows;
end;
const
{ TBitmap.GetTransparentColor from GRAPHICS.PAS uses this value }
  PaletteMask = $02000000;

procedure TDXPictureClip.Draw(Canvas: TCanvas; X, Y, Index: Integer);

  function PaletteColor(Color: TColor): Longint;
  begin
    Result := ColorToRGB(Color) or PaletteMask;
  end;
  procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
    SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; Palette: HPalette;
    TransparentColor: TColorRef);
  var
    Color: TColorRef;
    bmAndBack, bmAndObject, bmAndMem, bmSave: HBitmap;
    bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: HBitmap;
    MemDC, BackDC, ObjectDC, SaveDC: HDC;
    palDst, palMem, palSave, palObj: HPalette;
  begin
    { Create some DCs to hold temporary data }
    BackDC := CreateCompatibleDC(DstDC);
    ObjectDC := CreateCompatibleDC(DstDC);
    MemDC := CreateCompatibleDC(DstDC);
    SaveDC := CreateCompatibleDC(DstDC);
    { Create a bitmap for each DC }
    bmAndObject := CreateBitmap(SrcW, SrcH, 1, 1, nil);
    bmAndBack := CreateBitmap(SrcW, SrcH, 1, 1, nil);
    bmAndMem := CreateCompatibleBitmap(DstDC, DstW, DstH);
    bmSave := CreateCompatibleBitmap(DstDC, SrcW, SrcH);
    { Each DC must select a bitmap object to store pixel data }
    bmBackOld := SelectObject(BackDC, bmAndBack);
    bmObjectOld := SelectObject(ObjectDC, bmAndObject);
    bmMemOld := SelectObject(MemDC, bmAndMem);
    bmSaveOld := SelectObject(SaveDC, bmSave);
    { Select palette }
    palDst := 0; palMem := 0; palSave := 0; palObj := 0;
    if Palette <> 0 then begin
      palDst := SelectPalette(DstDC, Palette, True);
      RealizePalette(DstDC);
      palSave := SelectPalette(SaveDC, Palette, False);
      RealizePalette(SaveDC);
      palObj := SelectPalette(ObjectDC, Palette, False);
      RealizePalette(ObjectDC);
      palMem := SelectPalette(MemDC, Palette, True);
      RealizePalette(MemDC);
    end;
    { Set proper mapping mode }
    SetMapMode(SrcDC, GetMapMode(DstDC));
    SetMapMode(SaveDC, GetMapMode(DstDC));
    { Save the bitmap sent here }
    BitBlt(SaveDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SRCCOPY);
    { Set the background color of the source DC to the color,         }
    { contained in the parts of the bitmap that should be transparent }
    Color := SetBkColor(SaveDC, PaletteColor(TransparentColor));
    { Create the object mask for the bitmap by performing a BitBlt()  }
    { from the source bitmap to a monochrome bitmap                   }
    BitBlt(ObjectDC, 0, 0, SrcW, SrcH, SaveDC, 0, 0, SRCCOPY);
    { Set the background color of the source DC back to the original  }
    SetBkColor(SaveDC, Color);
    { Create the inverse of the object mask }
    BitBlt(BackDC, 0, 0, SrcW, SrcH, ObjectDC, 0, 0, NOTSRCCOPY);
    { Copy the background of the main DC to the destination }
    BitBlt(MemDC, 0, 0, DstW, DstH, DstDC, DstX, DstY, SRCCOPY);
    { Mask out the places where the bitmap will be placed }
    StretchBlt(MemDC, 0, 0, DstW, DstH, ObjectDC, 0, 0, SrcW, SrcH, SRCAND);
    { Mask out the transparent colored pixels on the bitmap }
    BitBlt(SaveDC, 0, 0, SrcW, SrcH, BackDC, 0, 0, SRCAND);
    { XOR the bitmap with the background on the destination DC }
    StretchBlt(MemDC, 0, 0, DstW, DstH, SaveDC, 0, 0, SrcW, SrcH, SRCPAINT);
    { Copy the destination to the screen }
    BitBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0,
      SRCCOPY);
    { Restore palette }
    if Palette <> 0 then begin
      SelectPalette(MemDC, palMem, False);
      SelectPalette(ObjectDC, palObj, False);
      SelectPalette(SaveDC, palSave, False);
      SelectPalette(DstDC, palDst, True);
    end;
    { Delete the memory bitmaps }
    DeleteObject(SelectObject(BackDC, bmBackOld));
    DeleteObject(SelectObject(ObjectDC, bmObjectOld));
    DeleteObject(SelectObject(MemDC, bmMemOld));
    DeleteObject(SelectObject(SaveDC, bmSaveOld));
    { Delete the memory DCs }
    DeleteDC(MemDC);
    DeleteDC(BackDC);
    DeleteDC(ObjectDC);
    DeleteDC(SaveDC);
  end;
  procedure StretchBitmapTransparent(Dest: TCanvas; Bitmap: TBitmap;
    TransparentColor: TColor; DstX, DstY, DstW, DstH, SrcX, SrcY,
    SrcW, SrcH: Integer);
  var
    CanvasChanging: TNotifyEvent;
  begin
    if DstW <= 0 then DstW := Bitmap.Width;
    if DstH <= 0 then DstH := Bitmap.Height;
    if (SrcW <= 0) or (SrcH <= 0) then begin
      SrcX := 0; SrcY := 0;
      SrcW := Bitmap.Width;
      SrcH := Bitmap.Height;
    end;
    if not Bitmap.Monochrome then
      SetStretchBltMode(Dest.Handle, STRETCH_DELETESCANS);
    CanvasChanging := Bitmap.Canvas.OnChanging;
    Bitmap.Canvas.Lock;
    try
      Bitmap.Canvas.OnChanging := nil;
      if TransparentColor = clNone then begin
        StretchBlt(Dest.Handle, DstX, DstY, DstW, DstH, Bitmap.Canvas.Handle,
          SrcX, SrcY, SrcW, SrcH, Dest.CopyMode);
      end
      else begin
        if TransparentColor = clDefault then
          TransparentColor := Bitmap.Canvas.Pixels[0, Bitmap.Height - 1];
        if Bitmap.Monochrome then TransparentColor := clWhite
        else TransparentColor := ColorToRGB(TransparentColor);
        StretchBltTransparent(Dest.Handle, DstX, DstY, DstW, DstH,
          Bitmap.Canvas.Handle, SrcX, SrcY, SrcW, SrcH, Bitmap.Palette,
          TransparentColor);
      end;
    finally
      Bitmap.Canvas.OnChanging := CanvasChanging;
      Bitmap.Canvas.Unlock;
    end;
  end;
  procedure DrawBitmapTransparent(Dest: TCanvas; DstX, DstY: Integer;
    Bitmap: TBitmap; TransparentColor: TColor);
  begin
    StretchBitmapTransparent(Dest, Bitmap, TransparentColor, DstX, DstY,
      Bitmap.Width, Bitmap.Height, 0, 0, Bitmap.Width, Bitmap.Height);
  end;
var
  Image: TGraphic;
begin
  if Index < 0 then Image := Picture.Graphic
  else Image := GraphicCell[Index];
  if (Image <> nil) and not Image.Empty then begin
    if FMasked and (FMaskColor <> clNone) and
      (Picture.Graphic is TBitmap) then
      DrawBitmapTransparent(Canvas, X, Y, TBitmap(Image), FMaskColor)
    else Canvas.Draw(X, Y, Image);
  end;
end;

procedure TDXPictureClip.DrawCenter(Canvas: TCanvas; Rect: TRect; Index: Integer);
var
  X, Y: Integer;
begin
  X := (Rect.Left + Rect.Right - Width) div 2;
  Y := (Rect.Bottom + Rect.Top - Height) div 2;
  Draw(Canvas, X, Y, Index);
end;

procedure TDXPictureClip.CheckIndex(Index: Integer);
begin
  if (Index >= Cols * Rows) or (Index < 0) then
    raise EListError.CreateFmt('%s (%d)', ['Load list error', Index]);
end;

function TDXPictureClip.GetIndex(Col, Row: Cardinal): Integer;
begin
  Result := Col + (Row * Cols);
  if (Result >= Cols * Rows) or IsEmpty then Result := -1;
end;

function TDXPictureClip.GetCell(Col, Row: Cardinal): TBitmap;
begin
  Result := GetGraphicCell(GetIndex(Col, Row));
end;

function TDXPictureClip.GetGraphicCell(Index: Integer): TBitmap;
  procedure AssignBitmapCell(Source: TGraphic; Dest: TBitmap; Cols, Rows,
    Index: Integer);
  var
    CellWidth, CellHeight: Integer;
  begin
    if (Source <> nil) and (Dest <> nil) then begin
      if Cols <= 0 then Cols := 1;
      if Rows <= 0 then Rows := 1;
      if Index < 0 then Index := 0;
      CellWidth := Source.Width div Cols;
      CellHeight := Source.Height div Rows;
      with Dest do begin
        Width := CellWidth; Height := CellHeight;
      end;
      if Source is TBitmap then begin
        Dest.Canvas.CopyRect(Bounds(0, 0, CellWidth, CellHeight),
          TBitmap(Source).Canvas, Bounds((Index mod Cols) * CellWidth,
          (Index div Cols) * CellHeight, CellWidth, CellHeight));
        Dest.TransparentColor := TBitmap(Source).TransparentColor;
      end
      else begin
        Dest.Canvas.Brush.Color := clSilver;
        Dest.Canvas.FillRect(Bounds(0, 0, CellWidth, CellHeight));
        Dest.Canvas.Draw(-(Index mod Cols) * CellWidth,
          -(Index div Cols) * CellHeight, Source);
      end;
      Dest.Transparent := Source.Transparent;
    end;
  end;
begin
  CheckIndex(Index);
  AssignBitmapCell(Picture.Graphic, FBitmap, Cols, Rows, Index);
  if Picture.Graphic is TBitmap then
    if FBitmap.PixelFormat <> pfDevice then
      FBitmap.PixelFormat := TBitmap(Picture.Graphic).PixelFormat;
  FBitmap.TransparentColor := FMaskColor or PaletteMask;
  FBitmap.Transparent := (FMaskColor <> clNone) and Masked;
  Result := FBitmap;
end;

function TDXPictureClip.GetDefaultMaskColor: TColor;
begin
  Result := clOlive;
  if (Picture.Graphic <> nil) and (Picture.Graphic is TBitmap) then
    Result := TBitmap(Picture.Graphic).TransparentColor and
      not PaletteMask;
end;

function TDXPictureClip.GetHeight: Integer;
begin
  Result := Picture.Height div FRows;
end;

function TDXPictureClip.GetWidth: Integer;
begin
  Result := Picture.Width div FCols;
end;

function TDXPictureClip.IsMaskStored: Boolean;
begin
  Result := MaskColor <> GetDefaultMaskColor;
end;

procedure TDXPictureClip.SetMaskColor(Value: TColor);
begin
  if Value <> FMaskColor then begin
    FMaskColor := Value;
    Changed;
  end;
end;

procedure TDXPictureClip.PictureChanged(Sender: TObject);
begin
  FMaskColor := GetDefaultMaskColor;
  if not (csReading in ComponentState) then Changed;
end;

procedure TDXPictureClip.SetHeight(Value: Integer);
begin
  if (Value > 0) and (Picture.Height div Value > 0) then
    Rows := Picture.Height div Value;
end;

procedure TDXPictureClip.SetWidth(Value: Integer);
begin
  if (Value > 0) and (Picture.Width div Value > 0) then
    Cols := Picture.Width div Value;
end;

procedure TDXPictureClip.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

{ Transformations routines }
{ Authorisation: Mr. Takanori Kawasaki}

//Distance between 2 points is calculated
function Get2PointRange(a,b: TDblPoint):Double;
var
  x,y: Double;
begin
  x := a.X - b.X;
  y := a.Y - b.Y;
  Result := Sqrt(x*x+y*y);
end;

//Direction angle in the coordinate A which was seen from  coordinate B is calculated
function GetARadFromB(A,B: TDblPoint):Double;
var
  dX,dY: Double;
begin
  dX := A.X - B.X;
  dY := A.Y - B.Y;
  Result := Get256(dX,dY);
end;

//Direction angle is returned with 0 - 255.
function Get256(dX,dY:Double):Double;
begin
  Result := 0;
  if dX > 0 then
  begin//0-63
    if dY > 0 then Result := ArcTan(dY / dX)          // 0 < Res < 90
    else//0
    if dY = 0 then Result := 0                        // 0
    else//192-255
    if dY < 0 then Result := 2*Pi + ArcTan(dY / dX)   // 270 < Res < 360
  end else
  if dX = 0 then
  begin//64
    if dY > 0 then Result := 1 / 2 * Pi               // 90
    else//0
    if dY = 0 then Result := 0                        // 0
    else//192
    if dY < 0 then Result := 3 / 2 * Pi               // 270
  end else
  if dX < 0 then
  begin//64-127
    if dY > 0 then Result := Pi + ArcTan(dY / dX)     // 90 < Res < 180
    else//128
    if dY = 0 then Result := Pi                       // 180
    else//128-191
    if dY < 0 then Result := Pi + ArcTan(dY / dX)     // 180 < Res < 270
  end;
  Result := 256 * Result / (2*Pi);
end;

//From the coordinate SP the Range it calculates the point  which leaves with the angular Angle
function GetPointFromRangeAndAngle(SP: TDblPoint; Range,Angle: Double): TDblPoint;
begin
  Result.X := SP.X + Range * Cos(Angle);
  Result.Y := SP.Y + Range * Sin(Angle);
end;

//* As for coordinate transformation coordinate for mathematics is used
//Identity matrix for the 2d is returned.
function Ini2DRowCol: T2DRowCol;
var
  i,ii:integer;
begin
  for i := 1 to 3 do
    for ii := 1 to 3 do
      if i = ii then Result[i,ii] := 1 else Result[i,ii] := 0;
end;

//Transformation matrix of the portable quantity
//where the one  for 2d is appointed is returned.
function Trans2DRowCol(x,y:double):T2DRowCol;
begin
  Result := Ini2DRowCol;
  Result[3,1] := x;
  Result[3,2] := y;
end;

//Conversion coordinate of the expansion and contraction
//quantity where the one for 2d is appointed is returned.
function Scale2DRowCol(x,y:double):T2DRowCol;
begin
  Result := Ini2DRowCol;
  Result[1,1] := x;
  Result[2,2] := y;
end;

//Coordinate transformation of the rotary quantity
//where the  one for 2d is appointed is returned.
function Rotate2DRowCol(Theta:double):T2DRowCol;
begin
  Result := Ini2DRowCol;
  Result[1,1] := Cos256(Trunc(Theta));
  Result[1,2] := Sin256(Trunc(Theta));
  Result[2,1] := -1 * Result[1,2];
  Result[2,2] := Result[1,1];
end;

//You apply two conversion coordinates and adjust.
function Multiply2DRowCol(A,B:T2DRowCol):T2DRowCol;
begin
  Result[1,1] := A[1,1] * B[1,1] + A[1,2] * B[2,1];
  Result[1,2] := A[1,1] * B[1,2] + A[1,2] * B[2,2];
  Result[1,3] := 0;
  Result[2,1] := A[2,1] * B[1,1] + A[2,2] * B[2,1];
  Result[2,2] := A[2,1] * B[1,2] + A[2,2] * B[2,2];
  Result[2,3] := 0;
  Result[3,1] := A[3,1] * B[1,1] + A[3,2] * B[2,1] + B[3,1];
  Result[3,2] := A[3,1] * B[1,2] + A[3,2] * B[2,2] + B[3,2];
  Result[3,3] := 1;
end;

//Until coordinate (the X and the Y) comes on the X axis,
//the  conversion coordinate which turns the position
//of the point is  returned.
function RotateIntoX2DRowCol(x,y: double):T2DRowCol;
var
  d: double;
begin
  Result := Ini2DRowCol;
  d := sqrt(x*x+y*y);
  Result[1,1] := x / d;
  Result[1,2] := y / d;
  Result[2,1] := -1 * Result[1,2];
  Result[2,2] := Result[1,1];
end;

//Coordinate (the X and the Y) as a center, the conversion
//coordinate which does the scaling of the magnification ratio
//which is  appointed with the Sx and the Sy is returned.
function ScaleAt2DRowCol(x,y,Sx,Sy:double):T2DRowCol;
var
  T,S,TInv,M:T2DRowCol;
begin
  T := Trans2DRowCol(-x,-y);
  TInv := Trans2DRowCol(x,y);
  S := Scale2DRowCol(Sx,Sy);
  M := Multiply2DRowCol(T,S);
  Result := Multiply2DRowCol(M,T);
end;

//Coordinate (the X and the Y) it passes, comes hard and
//(DX and the dy) with the direction which is shown it
//returns the  transformation matrix which does the reflected
//image conversion which  centers the line which faces.
function ReflectAcross2DRowCol(x,y,dx,dy:Double): T2DRowCol;
var
  T,R,S,RInv,TInv,M1,M2,M3: T2DRowCol;
begin
  T := Trans2DRowCol(-x,-y);
  TInv := Trans2DRowCol(x,y);
  R := RotateIntoX2DRowCol(dx,dy);
  RInv := RotateIntoX2DRowCol(dx,-dy);
  S := Scale2DRowCol(1,-1);
  M1 := Multiply2DRowCol(T,R);
  M2 := Multiply2DRowCol(S,RInv);
  M3 := Multiply2DRowCol(M1,M2);
  Result := Multiply2DRowCol(M3,TInv);
end;

//Coordinate focusing on (the X and the Y) the transformation
//matrix which turns the position of the point with angle Theta is  returned.
function RotateAround2DRowCol(x,y,Theta:Double): T2DRowCol;
var
  T,R,TInv,M: T2DRowCol;
begin
  T := Trans2DRowCol(-x,-y);
  TInv := Trans2DRowCol(x,y);
  R := Rotate2DRowCol(Theta);
  M := Multiply2DRowCol(T,R);
  Result := Multiply2DRowCol(M,TInv);
end;

//Transformation matrix is applied to the point.
function Apply2DVector(V:T2DVector; M:T2DRowCol): T2DVector;
begin
  Result[1] := V[1] * M[1,1] + V[2] * M[2,1] + M[3,1];
  Result[2] := V[1] * M[1,2] + V[2] * M[2,2] + M[3,2];
  Result[3] := 1;
end;

//The TDblPoint is returned
function DblPoint(a,b:Double):TDblPoint;
begin
  Result.X := a;
  Result.Y := b;
end;

function TruncDblPoint(DblPos: TDblPoint): TPoint;
begin
  Result.X := Trunc(DblPos.X);
  Result.Y := Trunc(DblPos.Y);
end;
{
+-----------------------------------------------------------------------------+
|Collision decision                                                           |
+-----------------------------------------------------------------------------+}

//Point and circle
function PointInCircle(PPos,CPos: TPoint; R: integer): Boolean;
begin
  Result := (PPos.X - CPos.X)*(PPos.X - CPos.X)+(PPos.Y - CPos.Y)*(PPos.Y - CPos.Y)<= R*R;
end;

//Circle and circle
function CircleInCircle(C1Pos,C2Pos: TPoint; R1,R2:Integer): Boolean;
begin
  Result := (C1Pos.X - C2Pos.X)*(C1Pos.X - C2Pos.X)+(C1Pos.Y - C2Pos.Y)*(C1Pos.Y - C2Pos.Y) <= (R1+R2)*(R1+R2);
end;

//Circle and line segment
function SegmentInCircle(SPos,EPos,CPos: TPoint; R: Integer): Boolean;
var
  V,C: TPoint;
  VC,VV,CC:integer;
begin
  Result := False;
  V.X := EPos.X - SPos.X; V.Y := EPos.Y - SPos.Y;
  C.X := CPos.X - SPos.X; C.Y := CPos.Y - SPos.Y;
  VC := V.X * C.X + V.Y * C.Y;
  if VC < 0 then
  begin
    Result := (C.X * C.X + C.Y * C.Y) <= R*R;
  end
  else
  begin
    VV := V.X * V.X + V.Y * V.Y;
    if VC >= VV then
    begin
      Result := (EPos.X - CPos.X)*(EPos.X - CPos.X)+(EPos.Y - CPos.Y)*(EPos.Y - CPos.Y) <= R * R;
    end
    else
      if VC < VV then
      begin
        CC := C.X * C.X + C.Y * C.Y;
        Result := CC - (VC div VV)* VC <= R*R;
      end;
  end;
end;

//Angle recalc
function Angle256(Angle: Single): Single;
begin
  Result := Angle;
  While Result < 0 do Result := Result + 256;
  While Result >= 256 do Result := Result -256;
end;

//If A is closer than B from starting point S, the True is  returned.
function CheckNearAThanB(S,A,B: TDblPoint): Boolean;
begin
  Result := (S.X-A.X)*(S.X-A.X)+(S.Y-A.Y)*(S.Y-A.Y) <= (S.X-B.X)*(S.X-B.X)+(S.Y-B.Y)*(S.Y-B.Y);
end;

function CircumCenter3Pt(const x1, y1, x2, y2, x3, y3: Single; out Px, Py: Single): Boolean;
var
  A,B,C,D,E,F,G: Single;
begin
  A := x2 - x1;
  B := y2 - y1;
  C := x3 - x1;
  D := y3 - y1;
  E := A * (x1 + x2) + B * (y1 + y2);
  F := C * (x1 + x3) + D * (y1 + y3);
  G := 2.0 * (A * (y3 - y2) - B * (x3 - x2));
  Result := G <> 0.0;
  if Result then begin
    Px := (D * E - B * F) / G;
    Py := (A * F - C * E) / G;
  end;
end;

function Distance(const x1, y1, x2, y2: Double): Double;
begin
  Result := Sqrt(Sqr(y2 - y1) + Sqr(x2 - x1));
end;

procedure InCenter(const x1, y1, x2, y2, x3, y3: Double; out Px, Py: Double);
var
  Perim: Double;
  Side12: Double;
  Side23: Double;
  Side31: Double;
begin
  Side12 := Distance(x1, y1, x2, y2);
  Side23 := Distance(x2, y2, x3, y3);
  Side31 := Distance(x3, y3, x1, y1);
  { Using Heron's S=UR }
  Perim := 1.0 / (Side12 + Side23 + Side31);
  Px := (Side23 * x1 + Side31 * x2 + Side12 * x3) * Perim;
  Py := (Side23 * y1 + Side31 * y2 + Side12 * y3) * Perim;
end;

function PointInTriangle(const Px, Py, x1, y1, x2, y2, x3, y3: Double): Boolean;
  function Orientation(const x1, y1, x2, y2, Px, Py: Double): Integer;
  var
    Orin: Double;
  begin
    (* Linear determinant of the 3 points *)
    Orin := (x2 - x1) * (py - y1) - (px - x1) * (y2 - y1);

    if Orin > 0.0 then
      Result := +1             (* Orientaion is to the right-hand side *)
    else if Orin < 0.0 then
      Result := -1             (* Orientaion is to the left-hand side  *)
    else
      Result := 0;             (* Orientaion is neutral aka collinear  *)
  end;
var
  Or1, Or2, Or3: Integer;
begin
  Or1 := Orientation(x1, y1, x2, y2, Px, Py);
  Or2 := Orientation(x2, y2, x3, y3, Px, Py);
  Or3 := Orientation(x3, y3, x1, y1, Px, Py);

  if (Or1 = Or2) and (Or2 = Or3) then
    Result := True
  else if Or1 = 0 then
    Result := (Or2 = 0) or (Or3 = 0)
  else if Or2 = 0 then
    Result := (Or1 = 0) or (Or3 = 0)
  else if Or3 = 0 then
    Result := (Or2 = 0) or (Or1 = 0)
  else
    Result := False;
end;

procedure Log(const Co: string; const FName: string);
var F: Text; D: TDateTime;
  Hour, Minute, Second, MSec: Word;
begin
  AsSignFile(F, FName);
  if FileExists(FName) then Append(F)
  else ReWrite(F);
  try
    D := Now;
    DecodeTime(D, Hour, Minute, Second, MSec);
    WriteLn(F, DateToStr(D) + ' ' + IntToStr(Hour)+':'+IntToStr(Minute)+':'+IntToStr(Second)+ '.'+IntToStr(MSec) +' ' + Co);
  finally
    CloseFile(F);
  end;
end;

{$IFDEF _DMO_}

{ TDirectXDriverEx }

function TDirectXDriverEx.ConvertHMonitor(iMonitor: HMonitor): TMonitorInfo;
begin
  ZeroMemory(@Result, sizeof(Result));
  Result.cbSize := SizeOf(Result);
  MultiMon.GetMonitorInfo(iMonitor, @Result);
end;

function TDirectXDriverEx.GetFlags: DWORD;
begin
  Result := ConvertHMonitor(FMonitor).dwFlags;
end;

function TDirectXDriverEx.GetMonitorInfo: TMonitorInfo;
begin
  Result:= ConvertHMonitor(FMonitor);
end;

function TDirectXDriverEx.GetTempSpace: TRect;
begin
  Result := ConvertHMonitor(FMonitor).rcWork
end;

function TDirectXDriverEx.GetWorkSpace: TRect;
begin
  Result := ConvertHMonitor(FMonitor).rcMonitor
end;

procedure TDirectXDriverEx.SetGUID(Value: PGUID);
begin
  if not IsBadHugeReadPtr(Value, SizeOf(TGUID)) then
  begin
    FGUID2 := Value^;
    FGUID := @FGUID2;
  end else
    FGUID := Value;
end;

{ TDirectXDriversEx }

constructor TDirectXDriversEx.Create;
begin
  inherited Create(TDirectXDriverEx);
end;

function TDirectXDriversEx.GetDriver(Index: Integer): TDirectXDriverEx;
begin
  Result := (inherited Items[Index]) as TDirectXDriverEx;
end;

{$ENDIF}

initialization
  InitCosinTable;
finalization
  FreeLibList;
end.