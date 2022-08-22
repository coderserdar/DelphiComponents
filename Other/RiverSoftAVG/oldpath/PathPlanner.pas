//=== File Prolog ============================================================
//	This code was developed by RiverSoftAVG.
//
//--- Notes ------------------------------------------------------------------
//                    
//--- Development History  ---------------------------------------------------
//
//      10/19/2000 T. Grubb
//	           - Added TSimplePathPlanner component, which descends from
//                   from TCustomPathPlanner
//                 - Published some events and properties
//                 - General Code cleanup (a LOT more needed :) )
//
//	03/2000	   T. Grubb
//		   Initial version.
//
//      File Contents:
//              EPathException  - Base exception class for pathing exceptions
//              TAStarPathPlanner - Finds path using A* Algorithm
//              TBasePlanner - Base component for TAStarPathPlanner
//              TCustomPathPlanner - Base class for path planners (deprecated)
//              TCustomSearchState - Base Class for one node in A* Search
//              TCustomStateFactory - Base class for creating and maintaining TCustomSearchStates
//              TMapLocation - TCustomSearchState Descendant for 2D Map representation
//              TPath - A Path of steps from a start location to a goal location
//              TPathStep - one step in a TPath
//              TSearchableMap - 2D Map used by TAStarPathPlanner, descendant of
//                             TCustomStateFactory and uses TMapLocations
//              TStateFactory - Public version of TCustomStateFactory
//
//
//--- Warning ----------------------------------------------------------------
//	This software is property of RiverSoftAVG. Unauthorized use or
//      duplication of this software is strictly prohibited. Authorized users
//      are subject to the following restrictions:
//	*	RiverSoftAVG is not responsible for
//		any consequence of the use of this software.
//	*	The origin of this software must not be misrepresented either by
//		explicit claim or by omission.
//	*	Altered versions of this software must be plainly marked as such.
//	*	This notice may not be removed or altered.
//
//      This software is freeware.  You are authorized to duplicate and modify
//      this software subject to the restrictions above.
//
//=== End File Prolog ========================================================
unit PathPlanner;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Contnrs;

resourcestring
  SInvalidDirection = 'Invalid %s Direction: %d';
  SMissingStateFactory = 'Unable to start search, missing factory';
  SUnableCreateState = ' Unable to create search state, factory returned nil';
  SInvalidNeighborRequest = 'Invalid neighbor request, index out of range';

type
  TPathDirection = (pdNorth, pdNorthEast, pdEast, pdSouthEast, pdSouth,
                   pdSouthWest, pdWest, pdNorthwest, pdYouAreHere);
  TPathDirectionSet = set of TPathDirection;
  TPathCost = type Single;
  TTerrainCost = type Integer;
  TPathDirectionCost = Array[TPathDirection] of Integer;

const
  XPathDirection: TPathDirectionCost = (0,1,1,1,0,-1,-1,-1,0);
  YPathDirection: TPathDirectionCost = (-1,-1,0,1,1,1,0,-1,0);
  Sqrt2: Single = 1.414213562373;

type

  EPathException = class(Exception);
  TPointEvent = procedure ( Sender: TObject; X, Y: Integer ) of object;
  TPointBoolEvent = procedure ( Sender: TObject; X, Y: Integer; var result: Boolean ) of object;
  TPointFloatEvent = procedure ( Sender: TObject; X, Y: Integer; var result: Single ) of object;

  TPathStep = class(TPersistent)
  { Purpose: One step in a path }
  private
    { Private declarations }
    FY: Integer;
    FX: Integer;
    FTag: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Assign( Source: TPersistent ); override;
    constructor Create( X: Integer = 0; Y: Integer = 0 ); overload;
    constructor Create( Point: TPoint ); overload; virtual;
    procedure Clear; virtual;
    procedure SetXY( const X, Y: Integer ); overload;
    procedure SetXY( const Point: TPoint ); overload;
    function DistanceFrom( AStep: TPathStep ): Integer;
  published
    { Published declarations }
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property Tag: Integer read FTag write FTag;
  end; { TPathStep }

  TPath = class(TPersistent)
  { Purpose: The TPath object holds a list of path steps from one point to another }
  private
    { Private declarations }
    FSteps: TObjectList;
    FCurrentStep: Integer;
    FLimit: Integer;
    procedure SetSteps(const Value: TObjectList);
    function GetCount: Integer;
    function GetStep(Index: Integer): TPathStep;
    procedure SetCurrentStep(const Value: Integer);
    function GetDistance: Integer;
    procedure SetLimit(const Value: Integer);
  protected
    { Protected declarations }
    property Steps: TObjectList read FSteps write SetSteps;
  public
    { Public declarations }
    procedure Assign( Source: TPersistent ); override;
    constructor Create; virtual;
    destructor Destroy; override;
    function Add( AStep: TPathStep ): Integer; overload; virtual;
    function Add( X, Y: Integer ): Integer; overload;
    procedure Clear; virtual;      // Clear it out completely
    procedure Reset; virtual;      // Reset the path to nothing
    function IndexOf( AStep: TPathStep ): Integer;
    procedure RemoveLast;
    procedure Reverse;
    property Distance: Integer read GetDistance;
    property CurrentStep: Integer read FCurrentStep write SetCurrentStep default -1;
    property Count: Integer read GetCount;
    property Step[ Index: Integer ]: TPathStep read GetStep; default;
    property Limit: Integer read FLimit write SetLimit default -1;
  published
    { Published declarations }
  end; { TPath }

  TSearchFlag = (sfVisited, sfInOpenQueue, sfInClosedQueue, sfReserved1, sfReserved2, sfReserved3);
  TSearchFlags = set of TSearchFlag;
  TCustomSearchState = class(TPersistent)
  { Purpose: One state in the search space }
  private
    { Private declarations }
    FCost: TPathCost;
    FScore: TPathCost;
    FGoalCostEstimate: TPathCost;
    FParent: TCustomSearchState;
    FTerrain: Integer;
    FSearchFlags: TSearchFlags;
    procedure SetCost(const Value: TPathCost);
    procedure SetGoalCostEstimate(const Value: TPathCost);
  protected
    { Protected declarations }
    function GetNeighbor(Index: Integer): TCustomSearchState; virtual; abstract;
    function GetNeighborCount: Integer; virtual; abstract;
    procedure SetTerrain(const Value: Integer); virtual;
    property Cost: TPathCost read FCost write SetCost;
    property GoalCostEstimate: TPathCost read FGoalCostEstimate write SetGoalCostEstimate;
    property Score: TPathCost read FScore;
  public
    { Public declarations }
    function Equals( State: TCustomSearchState ): Boolean; virtual;
    procedure Clear; virtual;
    procedure Reset; virtual;
    function TerrainCost: TTerrainCost; virtual;
    function IsImpassable: Boolean; virtual;
    property NeighborCount: Integer read GetNeighborCount;
    property Neighbor[Index: Integer]: TCustomSearchState read GetNeighbor;
  published
    { Published declarations }
    property Parent: TCustomSearchState read FParent write FParent;
    property Terrain: Integer read FTerrain write SetTerrain;
    property SearchFlags: TSearchFlags read FSearchFlags write FSearchFlags;
  end; { TCustomSearchState }

  TStateXYEvent = procedure ( Sender: TObject; const State: TCustomSearchState; var X, Y: Integer ) of object;
  TNewStateEvent = procedure ( Sender, StateUser: TObject; const X, Y: Integer; var State: TCustomSearchState ) of object;
  TGetStateEvent = procedure ( Sender: TObject; const X, Y: Integer; var State: TCustomSearchState ) of object;
  TGetIntegerEvent = procedure ( Sender: TObject; var Result: Integer ) of object;
  TStateEvent = procedure ( Sender: TObject; State: TCustomSearchState ) of object;
  TCustomStateFactory = class(TComponent)
  { Purpose: This component is responsible for making search states and managing
    their memory }
  private
    { Private declarations }
    FOnCreateState: TNewStateEvent;
    FOnGetStateCount: TGetIntegerEvent;
    FOnClear: TNotifyEvent;
    FOnReset: TNotifyEvent;
    FOnGetState: TGetStateEvent;
    FOnGetXY: TStateXYEvent;
  protected
    { Protected declarations }
    function GetState(X, Y: Integer): TCustomSearchState; virtual;
    function GetStateCount: Integer; virtual;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
    property OnCreateState: TNewStateEvent read FOnCreateState write FOnCreateState;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
    property OnGetState: TGetStateEvent read FOnGetState write FOnGetState;
    property OnGetStateCount: TGetIntegerEvent read FOnGetStateCount write FOnGetStateCount;
    property OnGetXY: TStateXYEvent read FOnGetXY write FOnGetXY;
  public
    { Public declarations }
    procedure GetXY( State: TCustomSearchState; var X, Y: Integer ); virtual;
    procedure Clear; virtual;
    function EstimateGoalCost( FromState, ToState: TCustomSearchState ): TPathCost; virtual; abstract;
    function CalculateCost( FromState, ToState: TCustomSearchState ): TPathCost; virtual;
    procedure Reset; virtual;
    function CreateState( Sender: TObject; const X, Y: Integer ): TCustomSearchState; virtual;
    property State[X, Y: Integer]: TCustomSearchState read GetState;
    property StateCount: Integer read GetStateCount;
  published
    { Published declarations }
  end; { TCustomStateFactory }

  TStateFactory = class(TCustomStateFactory)
  { Purpose: This component is responsible for making search states and managing
    their memory.  This specific one just publishes events so that the user can do
    it at design time }
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property OnClear;
    property OnCreateState;
    property OnReset;
    property OnGetState;
    property OnGetStateCount;
    property OnGetXY;
  end; { TStateFactory }

  TSearchableMap = class;
  TMapLocation = class(TCustomSearchState)
  { Purpose: One state in the search space }
  private
    { Private declarations }
    FOwner: TSearchableMap;
    FY: Integer;
    FX: Integer;
    FMapEdge: TPathDirectionSet;
  protected
    { Protected declarations }
    function GetNeighbor(Index: Integer): TCustomSearchState; override;
    function GetNeighborCount: Integer; override;
  public
    { Public declarations }
    constructor Create( AMap: TSearchableMap; const X, Y: Integer ); virtual;
    function IsImpassable: Boolean; override;
    property Owner: TSearchableMap read FOwner;
    property X: Integer read FX;
    property Y: Integer read FY;
    property MapEdge: TPathDirectionSet read FMapEdge;
  published
    { Published declarations }
  end; { TMapLocation }

  TSearchableMap = class(TCustomStateFactory)
  { Purpose: This component is responsible for making search states and managing
    their memory.  This one is for a 2D Byte map }
  private
    { Private declarations }
    FHeight: Integer;
    FWidth: Integer;
    FMapBuf: TObjectList;
    FWeight: Single;
    FMaxPassableTerrainCost: Integer;
    function GetMapLocation(X, Y: Integer): TMapLocation;
    procedure SetWeight(const Value: Single);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    { Protected declarations }
    property MapBuf: TObjectList read FMapBuf;
    procedure ResizeMap; overload; virtual;
  public
    { Public declarations }
    constructor Create( AOwner: TComponent ); overload; override;
    constructor Create( AOwner: TComponent; Width, Height: Integer ); reintroduce; overload;
    destructor Destroy; override;
    function CalculateCost( FromState, ToState: TCustomSearchState ): TPathCost; override;
    function EstimateGoalCost( FromState, ToState: TCustomSearchState ): TPathCost; override;
    procedure GetXY( State: TCustomSearchState; var X, Y: Integer ); override;
    procedure Clear; override;
    procedure Reset; override;
    procedure ResizeMap( Width, Height: Integer ); overload;
    function CreateState( Sender: TObject; const X, Y: Integer ): TCustomSearchState; override;
    property Map[X,Y: Integer]: TMapLocation read GetMapLocation; default;
  published
    { Published declarations }
    property MaxPassableTerrainCost: Integer read FMaxPassableTerrainCost
                                             write FMaxPassableTerrainCost default 255;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Weight: Single read FWeight write SetWeight;
  end; { TMapStateFactory }

  TBasePlanner = class(TComponent)
  { Purpose: Base component for TAStarPathPlanner }
  private
    { Private declarations }
    FOnReset: TNotifyEvent;
    FOnClear: TNotifyEvent;
    FGoal: TPoint;
    FStart: TPoint;
    FPath: TPath;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Reset; virtual;
    function FindPath( Start, Goal: TPoint ): Integer; virtual;    // nil if no path
    property Goal: TPoint read FGoal;
    property Start: TPoint read FStart;
    property Path: TPath read FPath;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
  published
    { Published declarations }
  end; { TBasePlanner }

  TAStarPathPlanner = class(TBasePlanner)
  { Purpose: Finds path using A* Algorithm }
  private
    { Private declarations }
    FIterationLimit: Integer;
    FStateFactory: TCustomStateFactory;
    FOnSearchState: TStateEvent;
    procedure SetIterationLimit(const Value: Integer);
    procedure SetStateFactory(const Value: TCustomStateFactory);
  protected
    { Protected declarations }
    function Search( Start: TPoint; var ScoreLimit: TPathCost ): Boolean; overload;
    function Search: Integer; overload;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
    function FindPath( Start, Goal: TPoint ): Integer; override;    // nil if no path
    property IterationLimit: Integer read FIterationLimit write SetIterationLimit default 100;
  published
    { Published declarations }
    property OnClear;
    property OnReset;
    property OnSearchState: TStateEvent read FOnSearchState write FOnSearchState;
    property StateFactory: TCustomStateFactory read FStateFactory write SetStateFactory;
  end; { TAStarPathPlanner }

  TCustomPathPlanner = class(TComponent)
  { Purpose: The TCustomPathPlanner component is the base class for the path
    planning components **deprecated**.  This planner searches without taking
    terrain costs into account (just walls) }
  private
    { Private declarations }
    FPath: TPath;
    FOnPrepareMap: TNotifyEvent;              // User should make sure visited location map is created and cleared
    FOnValidLocationCheck: TPointBoolEvent;   // User returns if this is a valid location/not visited
    FIsFound: Boolean;
    FOnMapPassability: TPointFloatEvent;      // User returns Passability of location, 0 is impassable, 1 is passable
    FCurrY: Integer;
    FStartY: Integer;
    FEndY: Integer;
    FEndX: Integer;
    FStartX: Integer;
    FCurrX: Integer;
    FLastDirection: TPathDirection;
    FOnBlockLocation: TPointEvent;            // User marks location as visited
    procedure SetEndX(const Value: Integer);
    procedure SetEndY(const Value: Integer);
    procedure SetStartX(const Value: Integer);
    procedure SetStartY(const Value: Integer);
  protected
    { Protected declarations }
    procedure LocationChange; virtual;
    procedure PrepareMap; virtual;
    function IsValidLocation( const X, Y: Integer ): Boolean; virtual;
    function FindFirstDirection: TPathDirection; virtual;
    function FindNextLocation: Boolean; virtual;
    function BackupToPreviousLocation: Boolean; virtual;
    function MapPassability( const X, Y: Integer ): Single; virtual;
    procedure BlockLocation( const X, Y: Integer );
    function AddPathStep( const X, Y: Integer ): Integer;
  public
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Reset; virtual;
    function FindPath: Integer; overload;
    function FindPath( Iterations: Integer ): Integer; overload; virtual;
    function Initialize: Boolean; virtual;
    procedure SetStart( X, Y: Integer ); overload;
    procedure SetStart( Point: TPoint ); overload;
    procedure SetEnd( X, Y: Integer ); overload;
    procedure SetEnd( Point: TPoint ); overload;
    property Path: TPath read FPath;
    property IsFound: Boolean read FIsFound;
    property StartX: Integer read FStartX write SetStartX;
    property StartY: Integer read FStartY write SetStartY;
    property EndX: Integer read FEndX write SetEndX;
    property EndY: Integer read FEndY write SetEndY;
    property CurrX: Integer read FCurrX;
    property CurrY: Integer read FCurrY;
    property LastDirection: TPathDirection read FLastDirection;
    property OnMapPassability: TPointFloatEvent read FOnMapPassability write FOnMapPassability;
    property OnBlockLocation: TPointEvent read FOnBlockLocation write FOnBlockLocation;
    property OnPrepareMap: TNotifyEvent read FOnPrepareMap write FOnPrepareMap;
    property OnValidLocationCheck: TPointBoolEvent read FOnValidLocationCheck write FOnValidLocationCheck;
  published
    { Published declarations }
  end; { TCustomPathPlanner }

  TSimplePathPlanner = class(TCustomPathPlanner)
  { Purpose:  **deprecated**  This planner searches without taking
    terrain costs into account (just walls) }
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property OnMapPassability;
    property OnBlockLocation;
    property OnPrepareMap;
    property OnValidLocationCheck;
    property StartX;
    property StartY;
    property EndX;
    property EndY;
  end; { TSimplePathPlanner }

function AbsDistance( const X1, Y1, X2, Y2: Integer ): Integer;
function ManhattanDistance( const X1, Y1, X2, Y2: Integer ): Integer;
function DistanceSqr( const X1, Y1, X2, Y2: Integer ): Integer; overload;
function DistanceSqr( const Step1, Step2: TPathStep ): Integer; overload;
function Distance( const X1, Y1, X2, Y2: Integer ): Double; overload;
function Distance(Direction: TPathDirection; X1, Y1: Integer): Single; overload;
function Vector2DToDirection(const X, Y: Integer): TPathDirection;

implementation

function CompareNodes(Data1, Data2 : pointer) : TPathCost;
begin
     result := TCustomSearchState(Data2).Score - TCustomSearchState(Data1).Score;
end;

function ManhattanDistance( const X1, Y1, X2, Y2: Integer ): Integer;
begin
     result := abs(x2-x1) + abs(y2-y1);
end;

function AbsDistance( const X1, Y1, X2, Y2: Integer ): Integer;
var
   d1, d2: Integer;
begin
     d1 := abs(x2-x1);
     d2 := abs(y2-y1);
     if d1 < d2 then
        result := d2
     else
         result := d1;
end;

function Distance(const X1, Y1, X2, Y2: Integer): Double;
begin
     result := Sqrt( Sqr( X2-X1 ) + Sqr( Y2-Y1 ) );
end;

function DistanceSqr(const X1, Y1, X2, Y2: Integer): Integer;
begin
     result := Sqr( X2-X1 ) + Sqr( Y2-Y1 );
end;

function DistanceSqr(const Step1, Step2: TPathStep): Integer;
begin
     result := Sqr( Step2.X-Step1.X ) + Sqr( Step2.Y-Step1.Y );
end;

function Distance(Direction: TPathDirection; X1,
  Y1: Integer): Single;
begin
     // for our purposes, all cardinal directions
     // have the same value
     // this may be different for different tile systems.

     // diagonals are the square root of 2. Look at a right triangle
     // with the 2 sides that have a value of 1.
     //	1^2 + 1^2 = 2^2
     //	so the answer is square root of 2 or 1.414213562373;
     if Direction in [pdNorth, pdEast, pdSouth, pdWest] then
        result := 1
     else
         result := Sqrt2;
end;

function Vector2DToDirection(const X, Y: Integer): TPathDirection;
begin
     if X < 0 then
     begin
          if Y < 0 then result := pdNorthWest
          else if Y > 0 then result := pdSouthWest
          else
              result := pdWest;
     end
     else if X > 0 then
     begin
          if Y < 0 then result := pdNorthEast
          else if Y > 0 then result := pdSouthEast
          else
              result := pdEast;
     end
     else
     begin
          if Y < 0 then result := pdNorth
          else if Y > 0 then result := pdSouth
          else
              raise EPathException.CreateFmt(SInvalidDirection,['xy', 0]);
     end;
end;

{ TPathStep }

procedure TPathStep.Assign(Source: TPersistent);
begin
     if Source is TPathStep then
     begin
          FX := TPathStep(Source).X;
          FY := TPathStep(Source).Y;
          FTag := TPathStep(Source).Tag;
          Exit;
     end;
     inherited Assign( Source );
end;

procedure TPathStep.Clear;
begin
     FX := 0;
     FY := 0;
end;

constructor TPathStep.Create(X, Y: Integer);
begin
     FX := X;
     FY := Y;
end;

function TPathStep.DistanceFrom(AStep: TPathStep): Integer;
begin
     result := Sqr( AStep.X-X ) + Sqr( AStep.Y-Y );
end;

procedure TPathStep.SetXY(const X, Y: Integer);
begin
     FX := X;
     FY := Y;
end;

procedure TPathStep.SetXY(const Point: TPoint);
begin
     SetXY( Point.X, Point.Y );
end;

constructor TPathStep.Create(Point: TPoint);
begin
     Create( Point.X, Point.Y );
end;

{ TPath }

function TPath.Add(AStep: TPathStep): Integer;
// This function can return -1 if we want to artificially limit the number of steps
begin
     if (Limit = -1) or (FSteps.Count < (Limit-1)) then
        result := FSteps.Add( AStep )
     else
         result:= -1;
end;

function TPath.Add(X, Y: Integer): Integer;
// This function can return -1 if we want to artificially limit the number of steps
begin
     if (Limit = -1) or (FSteps.Count < (Limit-1)) then
        result := Add( TPathStep.Create(X,Y) )
     else
         result:= -1;
end;

procedure TPath.Assign(Source: TPersistent);
begin
     if Source is TPath then
     begin
          Steps := TPath(Source).Steps;
          FCurrentStep := TPath(Source).CurrentStep;
          FLimit := TPath(Source).Limit;
     end;
     inherited Assign( Source );
end;

procedure TPath.Clear;
begin
     FSteps.Clear;
     FCurrentStep := -1;
     FLimit := -1;
end;

constructor TPath.Create;
begin
     FSteps := TObjectList.Create;
     FLimit := -1;
     FCurrentStep := -1;
end;

destructor TPath.Destroy;
begin
     FSteps.Free;
     inherited Destroy;
end;

function TPath.GetCount: Integer;
begin
     result := FSteps.Count;
end;

function TPath.GetDistance: Integer;
begin
     if FSteps.Count = 0 then
        result := 0
     else
        result := Step[Count - 1].DistanceFrom( Step[0] );
end;

function TPath.GetStep(Index: Integer): TPathStep;
begin
     result := FSteps[Index] as TPathStep;
end;

function TPath.IndexOf(AStep: TPathStep): Integer;
begin
     result := FSteps.IndexOf( AStep );
end;

procedure TPath.RemoveLast;
begin
     if FSteps.Count > 0 then
        FSteps.Delete( FSteps.Count - 1 );
end;

procedure TPath.Reset;
begin
     FSteps.Clear;
     FCurrentStep := -1;
end;

procedure TPath.Reverse;
var
   i: Integer;
begin
     if FSteps.Count > 1 then
     begin
          for i := 0 to (FSteps.Count div 2)-1 do
              FSteps.Exchange(i, FSteps.Count - 1 - i);
     end;
end;

procedure TPath.SetCurrentStep(const Value: Integer);
begin
     if Value >= Count then
        FCurrentStep := Count - 1
     else if Value < 0 then
          FCurrentStep := 0
     else
         FCurrentStep := Value;
end;

procedure TPath.SetLimit(const Value: Integer);
begin
     FLimit := Value;
     if FSteps.Count >= Limit then
        FSteps.Count := Limit - 1;
end;

procedure TPath.SetSteps(const Value: TObjectList);
begin
     raise Exception.Create('need to assign');
     FSteps := Value;
end;

{ TCustomPathPlanner }

procedure TCustomPathPlanner.Clear;
begin
     FPath.Clear;
     FStartX := 0;
     FStartY := 0;
     FEndX := 0;
     FEndY := 0;
     FCurrX := 0;
     FCurrY := 0;
     FIsFound := False;
end;

procedure TCustomPathPlanner.LocationChange;
begin
     Reset;
end;

constructor TCustomPathPlanner.Create(AOwner: TComponent);
begin
     inherited Create( AOwner );
     FPath := TPath.Create;
end;

destructor TCustomPathPlanner.Destroy;
begin
     FPath.Free;
     inherited Destroy;
end;

function TCustomPathPlanner.FindFirstDirection: TPathDirection;
var
   i: TPathDirection;
   Dist1, Dist2: Integer;
   x, y: Integer;
begin
     result := pdYouAreHere;
     Dist1 := MaxInt;
     for i := pdNorth to pdNorthwest do
     begin
          // Add cost of moving in direction to the current X and Y
          X := CurrX + XPathDirection[i];
          Y := CurrY + YPathDirection[i];
          if MapPassability(X,Y) = 0 then Continue;
          Dist2 := DistanceSqr(X,Y,EndX,EndY);
          if (result = pdYouAreHere) or
             (Dist2 < Dist1) then
          begin
               result := i;
               Dist1 := Dist2;
          end;
     end;
     BlockLocation( CurrX, CurrY );
end;

function TCustomPathPlanner.FindNextLocation: Boolean;
var
   i: Integer;
   iMod: TPathDirection;
   Dist1, Dist2: Integer;
   x, y: Integer;
   NextDir: TPathDirection;
begin
     if (CurrX = EndX) and (CurrY = EndY) then
     begin
          result := True;
          Exit;
     end
     else
         result := False;

     if LastDirection = pdYouAreHere then
        FLastDirection := FindFirstDirection;
     Dist1 := MaxInt-1;
     NextDir := pdYouAreHere;
     // Look in three directions
     for i := -1 to 1 do
     begin
          iMod := TPathDirection((Ord(LastDirection)+i) and (Ord(High(TPathDirection))-1));
          // Add cost of moving in direction to the current X and Y
          X := CurrX + XPathDirection[iMod];
          Y := CurrY + YPathDirection[iMod];
          if MapPassability(X,Y) = 0 then Continue;
          Dist2 := DistanceSqr(X,Y,EndX,EndY);
          if Dist2 < Dist1 then
          begin
               NextDir := iMod;
               Dist1 := Dist2;
          end;
          result := True;
     end;
     if not result then Exit;
     FCurrX := FCurrX + XPathDirection[NextDir];
     FCurrY := FCurrY + YPathDirection[NextDir];

     if AddPathStep(CurrX, CurrY) = -1 then
        result := False
     else
     begin
          FLastDirection := NextDir;
          BlockLocation( CurrX, CurrY );
     end;
end;

function TCustomPathPlanner.FindPath(Iterations: Integer): Integer;
// returns the number of iterations, -1 means an error, 0 means already there
  {	PrepTestMap ();
	if (IsValidLocation (destx, desty) == false) return -1;
	if (IsValidLocation (startx, starty) == false) return -1;
	if (startx == destx && starty == desty) return -1;// we are already there
	StartX = startx, StartY = starty, CurrX = startx, CurrY = starty,
		DestX = destx, DestY = desty;

	return (FindPath ());}
begin
     // Initialize if this is the first call after Reset
     result := -1;
     if Path.Count = 0 then
        if (not Initialize) or IsFound then
        begin
             if IsFound then
                result := 0;
             Exit;
        end;

     if LastDirection = pdYouAreHere then Exit;
     result := 0;
     while (result < Iterations) and (not IsFound) do
     begin
          if not FindNextLocation then // Couldn't find anything, back up
             BackupToPreviousLocation;
          if (CurrX = EndX) and (CurrY = EndY) then FIsFound := True;
          Inc(result);
     end;
end;

function TCustomPathPlanner.FindPath: Integer;
// returns the number of iterations
begin
     result := FindPath(MaxInt-1);
end;

function TCustomPathPlanner.Initialize: Boolean;
// false means not valid start/stop points
begin
     PrepareMap;
     FIsFound := False;
     FCurrX := FStartX;
     FCurrY := FStartY;
     result := IsValidLocation( StartX, StartY ) and
               IsValidLocation( EndX, EndY );
     FIsFound := (StartX = EndX) and (StartY = EndY);
     if result and (not IsFound) then
     begin
          FPath.Add( FStartX, FStartY );
         // FPath.Add( FCoordinate[2], FCoordinate[3] );
          FLastDirection := FindFirstDirection;
     end;
end;

function TCustomPathPlanner.IsValidLocation(const X,
  Y: Integer): Boolean;
begin
     result := False;
     if Assigned( FOnValidLocationCheck ) then FOnValidLocationCheck( Self, X, Y, result );
end;

function TCustomPathPlanner.MapPassability(const X, Y: Integer): Single;
begin
     if Assigned( FOnMapPassability ) then FOnMapPassability( Self, X, Y, result );
end;

procedure TCustomPathPlanner.PrepareMap;
begin
     if Assigned( FOnPrepareMap ) then FOnPrepareMap( Self );
end;

procedure TCustomPathPlanner.Reset;
begin
     FPath.Reset;
     FCurrX := 0;
     FCurrY := 0;
     FIsFound := False;
end;

procedure TCustomPathPlanner.SetEndX(const Value: Integer);
begin
     if Value <> EndX then
     begin
          FEndX := Value;
          LocationChange;
     end;
end;

procedure TCustomPathPlanner.SetEndY(const Value: Integer);
begin
     if Value <> FEndY then
     begin
          FEndY := Value;
          LocationChange;
     end;
end;

procedure TCustomPathPlanner.BlockLocation(const X, Y: Integer);
begin
     if not IsValidLocation(X, Y) then Exit;
     if Assigned( FOnBlockLocation ) then FOnBlockLocation( Self, X, Y );
end;

procedure TCustomPathPlanner.SetStartX(const Value: Integer);
begin
     if Value <> FStartX then
     begin
          FStartX := Value;
          LocationChange;
     end;
end;

procedure TCustomPathPlanner.SetStartY(const Value: Integer);
begin
     if Value <> FStartY then
     begin
          FStartY := Value;
          LocationChange;
     end;
end;

function TCustomPathPlanner.AddPathStep(const X, Y: Integer): Integer;
begin
     if IsValidLocation( X, Y ) then
        result := Path.Add( X, Y )
     else
         result := -1;
end;

function TCustomPathPlanner.BackupToPreviousLocation: Boolean;
begin
     result := Path.Distance > 0;
     if not result then Exit;

     Path.RemoveLast;
     with Path.Step[Path.Count-1] do
     begin
          FLastDirection := Vector2DToDirection( CurrX-X, CurrY-Y );
          FCurrX := X;
          FCurrY := Y;
     end;
end;

procedure TCustomPathPlanner.SetEnd(X, Y: Integer);
begin
     FEndX := X;
     FEndY := Y;
     LocationChange;
end;

procedure TCustomPathPlanner.SetEnd(Point: TPoint);
begin
     FEndX := Point.X;
     FEndY := Point.Y;
     LocationChange;
end;

procedure TCustomPathPlanner.SetStart(X, Y: Integer);
begin
     FStartX := X;
     FStartY := Y;
     LocationChange;
end;

procedure TCustomPathPlanner.SetStart(Point: TPoint);
begin
     FStartX := Point.X;
     FStartY := Point.Y;
     LocationChange;
end;

{ TCustomSearchState }

procedure TCustomSearchState.Clear;
begin
    FCost := 0;
    FScore := 0;
    FGoalCostEstimate := 0;
    FParent := nil;
    FTerrain := 0;
    FSearchFlags := [];
end;

function TCustomSearchState.Equals(State: TCustomSearchState): Boolean;
begin
     // In this case, two search states are only equal if their pointers equal
     result := Self = State;
end;

function TCustomSearchState.IsImpassable: Boolean;
begin
     result := False;
end;

procedure TCustomSearchState.Reset;
begin
    FCost := 0;
    FScore := 0;
    FGoalCostEstimate := 0;
    FParent := nil;
    FSearchFlags := [];
end;

procedure TCustomSearchState.SetCost(const Value: TPathCost);
begin
  FCost := Value;
  FScore := FCost + FGoalCostEstimate;
end;

procedure TCustomSearchState.SetGoalCostEstimate(const Value: TPathCost);
begin
  FGoalCostEstimate := Value;
  FScore := FCost + FGoalCostEstimate;
end;

procedure TCustomSearchState.SetTerrain(const Value: Integer);
begin
     if Value < 0 then
        FTerrain := 0
     else
         FTerrain := Value;
end;

function TCustomSearchState.TerrainCost: TTerrainCost;
begin
     result := Terrain+1;
end;

{ TBasePlanner }

procedure TBasePlanner.Clear;
begin
     FPath.Clear;
end;

constructor TBasePlanner.Create(AOwner: TComponent);
begin
     inherited;
     FPath := TPath.Create;
end;

destructor TBasePlanner.Destroy;
begin
     FPath.Free;
     inherited;
end;

function TBasePlanner.FindPath(Start, Goal: TPoint): Integer;
begin
     FStart := Start;
     FGoal := Goal;
     Reset;
     if (Start.X = Goal.X) and (Start.Y = Goal.Y) then
        result := 0
     else
         result := -1;
end;

procedure TBasePlanner.Reset;
begin
     FPath.Reset;
end;

{ TAStarPathPlanner }

constructor TAStarPathPlanner.Create(AOwner: TComponent);
begin
     inherited;
     FIterationLimit := 100;
end;

function TAStarPathPlanner.FindPath(Start, Goal: TPoint): Integer;
// returns nodes examined
{var
   i: Integer;
   ScoreLimit: Integer;}
begin
     // Set start and stop and check path
     result := inherited FindPath(Start,Goal);
     if result = -1 then
        result := Search;
  {   if (not result) and (StateFactory <> nil) then
     begin
          ScoreLimit := StateFactory.EstimateGoalCost( StateFactory.CreateState(Self,Start.X, Start.Y),
                                                       StateFactory.CreateState(Self,Goal.X, Goal.Y) );
          for i := 0 to IterationLimit - 1 do
          begin
               result := Search( Start, ScoreLimit );
               if result then Break
               else if (ScoreLimit >= MaxInt - 1) then Break;
          end;
     end;       }
end;

procedure TAStarPathPlanner.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
     inherited;
     if (Operation = opRemove) and (AComponent = StateFactory) then
        FStateFactory := nil;
end;

function TAStarPathPlanner.Search(Start: TPoint;
  var ScoreLimit: TPathCost): Boolean;
var
   Node: TCustomSearchState;
   ChildNode: TCustomSearchState;
   GoalState: TCustomSearchState;
   StartState: TCustomSearchState;
   Stack: TObjectStack;
   x, y: Integer;
   i: Integer;
   NewLimit: TPathCost;
   Done: Boolean;
begin
     result := False;
     if StateFactory = nil then
        raise EPathException.Create( SMissingStateFactory );
     Stack := TObjectStack.Create;
     try
        GoalState := StateFactory.CreateState( Self, FGoal.X, FGoal.Y );
        if GoalState = nil then
           raise EPathException.Create( SUnableCreateState );
        // Create the start node
        StartState := StateFactory.CreateState( Self, Start.X, Start.Y );
        StartState.GoalCostEstimate := StateFactory.EstimateGoalCost( StartState, GoalState );
        StartState.Cost := 0;
        if StartState = nil then
           raise EPathException.Create( SUnableCreateState );
        Stack.Push( StartState );
        // Search the space, depth first search, iterative deepening
        NewLimit := MaxInt - 1;
        Done := False;
        while (Stack.Count > 0) and (Not Done) do
        begin
             // Get the next node
             Node := TCustomSearchState(Stack.Pop);
             if Node.Equals( GoalState ) then
             begin
                  // we found it, construct path and get out of here
                  while Node <> nil do
                  begin
                       StateFactory.GetXY( Node, X, Y );
                       Path.Add( X, Y );
                       Node := Node.Parent;
                  end;
                  Path.Reverse;
                  result := True;
                  Exit;
             end;
             // if the cost to the node is already cheaper, change parent
             if (Node.Parent <> nil) and
                (Node.Cost > (Node.Parent.Cost + Node.TerrainCost)) then
                Node.Cost := Node.Parent.Cost + Node.TerrainCost;
             // Ok, add all neighbors under the ScoreLimit to the stack
             for i := 0 to Node.NeighborCount - 1 do
             begin
                  ChildNode := Node.Neighbor[i];
                  // check that the neighbor can be visited
                  if ChildNode = nil then continue
                  else if not (sfVisited in ChildNode.SearchFlags) then
                  begin
                       ChildNode.GoalCostEstimate := StateFactory.EstimateGoalCost( ChildNode, GoalState );
                       ChildNode.Cost := StateFactory.CalculateCost( Node, ChildNode );
                       ChildNode.SearchFlags := ChildNode.SearchFlags + [sfVisited];
                      if ChildNode.Score <= ScoreLimit then
                      begin
                           ChildNode.Parent := Node;
                           Stack.Push( ChildNode );
                      end
                      else if ChildNode.Score < NewLimit then
                           NewLimit := ChildNode.Score;
                  end
                  else if ChildNode.Cost > StateFactory.CalculateCost( Node, ChildNode ) then
                  begin
                       ChildNode.Cost := StateFactory.CalculateCost( Node, ChildNode );
                      if ChildNode.Score <= ScoreLimit then
                      begin
                           ChildNode.Parent := Node;
                           Stack.Push( ChildNode );
                      end
                      else if ChildNode.Score < NewLimit then
                           NewLimit := ChildNode.Score;
                  end
                  else if (ChildNode.Score > ScoreLimit) and (ChildNode.Score < NewLimit) then
                       NewLimit := ChildNode.Score;
                  if (Node.Score > ScoreLimit) and (Node.Score < NewLimit) then
                       NewLimit := Node.Score;
             end;
             Node.SearchFlags := Node.SearchFlags + [sfVisited];
        end;
        ScoreLimit := NewLimit;
        // Factory manages search state memory
     finally
        Stack.Free;
     end;
end;

function TAStarPathPlanner.Search: Integer;
var
   Open: TStrings;
   GoalState: TCustomSearchState;
   Node: TCustomSearchState;
   ChildNode: TCustomSearchState;
   i: Integer;
   Index: Integer;
   NewCost: TPathCost;
   OldScore: TPathCost;
   X, Y: Integer;
begin
     if StateFactory = nil then
        raise EPathException.Create( SMissingStateFactory );
     Open := TStringList.Create;
     TStringList(Open).Sorted := True;
     try
        Node := StateFactory.CreateState( Self, Start.X, Start.Y );
        GoalState := StateFactory.CreateState( Self, Goal.X, Goal.Y );
        Node.GoalCostEstimate := StateFactory.EstimateGoalCost( Node, GoalState );
        Node.Cost := 0;
        Node.Parent := nil;
        // Do a trick to sort by priority
        Open.AddObject( Format( '%s%p', [FormatFloat('000000.00',Node.Score),
                                                       Pointer(Node)] ), Node );
        result := -1;
        while Open.Count > 0 do
        begin
             Node := TCustomSearchState(Open.Objects[0]);
             Open.Delete( 0 );
             Node.SearchFlags := Node.SearchFlags - [sfInOpenQueue];
             Inc(result);
             if Node = GoalState then
             begin
                  // we found it, construct path and get out of here
                  while Node <> nil do
                  begin
                       StateFactory.GetXY( Node, X, Y );
                       Path[Path.Add( X, Y )].Tag := round(Node.Cost);
                       Node := Node.Parent;
                  end;
                  Path.Reverse;
                  Exit;          // make sure it goes to open.free
             end;
             if Assigned(FOnSearchState) then FOnSearchState( Self, Node );
             for i := 0 to Node.NeighborCount - 1 do
             begin
                  ChildNode := Node.Neighbor[i];
                  if ChildNode = nil then Continue;
                  NewCost := Node.Cost + StateFactory.CalculateCost( Node, ChildNode );
                  if ((sfInOpenQueue in ChildNode.SearchFlags) or
                      (sfInClosedQueue in ChildNode.SearchFlags)) and
                     (ChildNode.Cost <= NewCost) then Continue;
                  ChildNode.Parent := Node;
                  OldScore := ChildNode.Score;
                  ChildNode.Cost := NewCost;
                  ChildNode.GoalCostEstimate := StateFactory.EstimateGoalCost( ChildNode, GoalState );
                  if sfInClosedQueue in ChildNode.SearchFlags then
                  begin
                       ChildNode.SearchFlags := ChildNode.SearchFlags - [sfInClosedQueue];
                  end;
                  if sfInOpenQueue in ChildNode.SearchFlags then
                  begin
                       Index := Open.Indexof(Format( '%s%p', [FormatFloat('000000.00',OldScore),
                                                       Pointer(ChildNode)] ) );
                       if Index = -1 then
                          raise Exception.Create('Should not happen');
                       Open.Delete(Index);
                  end;
                  // Do a trick to sort by priority
                  Open.AddObject( Format( '%s%p', [FormatFloat('000000.00',ChildNode.Score),
                                                       Pointer(ChildNode)] ), ChildNode );
                  ChildNode.SearchFlags := ChildNode.SearchFlags + [sfInOpenQueue];
             end;
             Node.SearchFlags := Node.SearchFlags + [sfInClosedQueue];
        end;
        result := -1;
     finally
        Open.Free;
     end;
end;

procedure TAStarPathPlanner.SetIterationLimit(
  const Value: Integer);
begin
     if Value < 1 then
        FIterationLimit := 1
     else
         FIterationLimit := Value;
end;

procedure TAStarPathPlanner.SetStateFactory(
  const Value: TCustomStateFactory);
begin
  FStateFactory := Value;
end;

{ TCustomStateFactory }

function TCustomStateFactory.CalculateCost(FromState,
  ToState: TCustomSearchState): TPathCost;
begin
     if FromState <> nil then
        result := FromState.Cost
     else
         result := MaxInt shr 1;
     if ToState <> nil then
        result := result + ToState.TerrainCost;
end;

procedure TCustomStateFactory.Clear;
begin
     if Assigned(FOnClear) then FOnClear(Self);
end;

function TCustomStateFactory.CreateState(Sender: TObject;
  const X, Y: Integer): TCustomSearchState;
begin
     result := nil;
     if Assigned(FOnGetState) then FOnGetState(Self, X, Y, result);
end;

function TCustomStateFactory.GetState(X, Y: Integer): TCustomSearchState;
begin
     result := nil;
     if Assigned(FOnGetState) then FOnGetState(Self, X, Y, result);
end;

function TCustomStateFactory.GetStateCount: Integer;
begin
     result := 0;
     if Assigned(FOnGetStateCount) then FOnGetStateCount(Self, result);
end;

procedure TCustomStateFactory.GetXY(State: TCustomSearchState; var X, Y: Integer);
begin
     X := 0;
     Y := 0;
     if Assigned(OnGetXY) then OnGetXY( Self, State, X, Y );
end;

procedure TCustomStateFactory.Reset;
begin
     if Assigned(FOnReset) then FOnReset(Self);
end;

{ TSearchableMap }

function TSearchableMap.CalculateCost(FromState,
  ToState: TCustomSearchState): TPathCost;
begin
     if (FromState = nil) or (ToState = nil) then
        result := MaxInt shr 1
     else
         result := Distance( TMapLocation(FromState).X, TMapLocation(FromState).Y,
                             TMapLocation(ToState).X, TMapLocation(ToState).Y) +
                   ToState.TerrainCost;
end;

procedure TSearchableMap.Clear;
var
   i: Integer;
begin
  inherited;
  for i := 0 to MapBuf.Count - 1 do
      TMapLocation(FMapBuf[i]).Clear;
end;

constructor TSearchableMap.Create(AOwner: TComponent; Width,
  Height: Integer);
begin
     FWidth := Width;
     FHeight := Height;
     Create( AOwner );
end;

constructor TSearchableMap.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FWeight := 1;
     FMaxPassableTerrainCost := 255;  // set max passable terrain cost, anything over
                                      // this will not even be considered for path
                                      // (like walls)
     FMapBuf := TObjectList.Create;
     ResizeMap;
end;

function TSearchableMap.CreateState(Sender: TObject; const X,
  Y: Integer): TCustomSearchState;
begin
     result := Map[X,Y];
     if result.Parent = nil then result.Cost := result.TerrainCost;
end;

destructor TSearchableMap.Destroy;
begin
     FMapBuf.Free;
     inherited;
end;

function TSearchableMap.EstimateGoalCost(FromState,
  ToState: TCustomSearchState): TPathCost;
begin
     if (FromState <> nil) and (ToState <> nil) then
        result := AbsDistance( TMapLocation(FromState).X, TMapLocation(FromState).Y,
                               TMapLocation(ToState).X, TMapLocation(ToState).Y )*Weight
     else
         result := 0;
end;

function TSearchableMap.GetMapLocation(X, Y: Integer): TMapLocation;
begin
     result := TMapLocation(FMapBuf[X+Y*Width]);
end;

procedure TSearchableMap.GetXY(State: TCustomSearchState; var X,
  Y: Integer);
begin
  X := (State as TMapLocation).X;
  Y := TMapLocation(State).Y;
end;

procedure TSearchableMap.Reset;
var
   i: Integer;
begin
  inherited;
  for i := 0 to MapBuf.Count - 1 do
      TMapLocation(FMapBuf[i]).Reset;
end;

procedure TSearchableMap.ResizeMap;
var
   x, y, i: Integer;
begin
     FMapBuf.Count := Width*Height;
     i := 0;
     for y := 0 to Height - 1 do
         for x := 0 to Width - 1 do
         begin
              FMapBuf[i] := TMapLocation.Create(Self, x, y);
              Inc(i);
         end;
end;

procedure TSearchableMap.ResizeMap(Width, Height: Integer);
begin
     FWidth := Width;
     FHeight := Height;
     ResizeMap;
end;

procedure TSearchableMap.SetHeight(const Value: Integer);
begin
     if Height <> Value then
     begin
          FHeight := Value;
          ResizeMap;
     end;
end;

procedure TSearchableMap.SetWeight(const Value: Single);
begin
     if Value <= 0 then
        FWeight := 0
     else
        FWeight := Value;
end;

procedure TSearchableMap.SetWidth(const Value: Integer);
begin
     if FWidth <> Value then
     begin
          FWidth := Value;
          ResizeMap;
     end;
end;

{ TMapLocation }

constructor TMapLocation.Create(AMap: TSearchableMap; const X, Y: Integer);
begin
     FOwner := AMap;
     FX := X;
     FY := Y;
     if X = 0 then
        FMapEdge := [pdNorthWest, pdWest, pdSouthWest]
     else if X = (Owner.Width-1) then
        FMapEdge := [pdNorthEast, pdEast, pdSouthEast]
     else
         FMapEdge := [];
     if Y = 0 then
        FMapEdge := FMapEdge + [pdNorthWest, pdNorth, pdNorthEast]
     else if Y = (Owner.Height-1) then
        FMapEdge := FMapEdge + [pdSouthEast, pdSouth, pdSouthWest];
end;

function TMapLocation.GetNeighbor(Index: Integer): TCustomSearchState;
begin
     // fix index for edges
     if TPathDirection(Index) in MapEdge then
        result := nil
     else if Index >= Ord(High(TPathDirection)) then
          raise EPathException.Create( SInvalidNeighborRequest )
     else
     begin
          result := Owner.Map[ X+XPathDirection[TPathDirection(Index)],
                              Y+YPathDirection[TPathDirection(Index)] ];
          if result.IsImpassable then
             result := nil;
     end;
end;

function TMapLocation.GetNeighborCount: Integer;
begin
     Result := 8;
end;

function TMapLocation.IsImpassable: Boolean;
begin
     result := TerrainCost >= Owner.MaxPassableTerrainCost;
end;

end.
