
(********************************************************)
(*                                                      *)
(*  Bare Object Library @ www.codebot.org/delphi        *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BarePhysics;

interface

{$I BARE.INC}

uses
  {$IFNDEF BARE}SysUtils, Classes,{$ENDIF} BareUtils,
	Windows, ODEImport, BareOpenGL, BareOpenGLExt, BareGraphics;

{ TSynchronizer }

type
	TSynchronizer = class
  private
    FMutex: THandle;
  public
  	constructor Create(const UniqueName: string);
    destructor Destroy; override;
  	procedure Wait;
    procedure Release;
  end;

	TPhysicsGeom = class;
 	TPhysicsGeomList = class;

{ TPhysicsBody }

  TPhysicsBody = class
  private
  	FBody: PdxBody;
    FGeoms: TPhysicsGeomList;
    function GetGeom(Index: Integer): TPhysicsGeom;
    function GetGeomCount: Integer;
	protected
    procedure AddRef(Geom: TPhysicsGeom);
    procedure Release(Geom: TPhysicsGeom);
  public
  	constructor Create;
    destructor Destroy; override;
    procedure Build;
		procedure SetPosition(X, Y, Z: Single);
    procedure SetRotation(H, P, R: Single);
  	property Handle: PdxBody read FBody;
    property GeomCount: Integer read GetGeomCount;
    property Geoms[Index: Integer]: TPhysicsGeom read GetGeom;
  end;

(*

void compoundGeom(
    dBodyID aBodyID,   // Body associated with the geoms
    dSpaceID aSpaceID, // Space to put the geoms in
    int aNumGeoms,     // Number of geoms to combine
    dGeomID* aGeoms,   // Array of geoms to be combined
    dMass* aMasses,    // Array of masses for the geoms
    dVector3* aPos,    // Array of positions for the geoms
    dMatrix3* aRot     // Arrao of rotations for the geoms
    ) {
    ASSERT(MAXCOMPOUNDGEOMS < aNumGeoms);

    int i; // Used in several for-loops (scope ambiguity issue)
    dGeomID lXgeoms[MAXCOMPOUNDGEOMS]; // @@@NOTE: Hardcoded max number of components!

    // To accumulate the total mass of the compound geom
    dMass lMass;
    dMassSetZero (&lMass);

    // This is the space where the geoms are inserted.
    // If you want to put each compound geom into a new
    // space, you could use the following line:
    // dSpaceID lSpaceID = dSimpleSpaceCreate(aSpaceID);
    dSpaceID lSpaceID = aSpaceID;


    // Time to combine the geoms
    for(i = 0; i < aNumGeoms; i++) {

        // Put the geom into a transform-geometry
        lXgeoms[i] = dCreateGeomTransform(lSimpleSpaceID);
        dGeomTransformSetCleanup(lXgeoms[i], 1);
        dGeomTransformSetGeom(lXgeoms[i], aGeoms[i]);

        // Set the transformation for the geom and its mass
        dGeomSetPosition(aGeoms[i], aPos[i][0],aPos[i][1],aPos[i][2]);
        dMassTranslate(&aMasses[i],aPos[i][0],aPos[i][1],aPos[i][2]);
        dGeomSetRotation(aGeoms[i],aRot[i]);
        dMassRotate(&aMasses[i], aRot[i]);

        // Add to the total mass
        dMassAdd (&lMass,&aMasses[i]);
    }

    // Move all encapsulated objects so that the center of mass is (0,0,0)
    for(i = 0; i < aNumGeoms; i++) {
        dGeomSetPosition (aGeoms[i],
            aPos[i][0] - lMass.c[0],
            aPos[i][1] - lMass.c[1],
            aPos[i][2] - lMass.c[2]);
    }
    dMassTranslate (&lMass,-lMass.c[0],-lMass.c[1],-lMass.c[2]);

    // Attach to the dBody
    for(i = 0; i < aNumGeoms; i++) {
        dGeomSetBody(lXgeoms[i], aBodyID);
    }
    dBodySetMass(aBodyID, &lMass);
}

*)

{ TPhysicsGeom }

	TPhysicsGeom = class
  private
  	FBody: TPhysicsBody;
    FBounce: Single;
    FColor: TFloatColor;
    FDensity: Single;
    FFriction: Single;
  	FGeom: PdxGeom;
    FPlacement: TPlacement;
  protected
    constructor Create(AGeom: PdxGeom);
    procedure Draw; virtual; abstract;
    function GetMass: TdMass; virtual;
  public
  	destructor Destroy; override;
		procedure Attach; overload;
		procedure Attach(Body: TPhysicsBody); overload;
		procedure Detatch;
  	procedure Render;
    procedure SetPosition(X, Y, Z: Single);
    procedure SetRotation(H, P, R: Single);
    property Density: Single read FDensity write FDensity;
    property Body: TPhysicsBody read FBody;
   	property Bounce: Single read FBounce write FBounce;
    property Color: TFloatColor read FColor write FColor;
    property Friction: Single read FFriction write FFriction;
    property Handle: PdxGeom read FGeom;
  end;

{ TPhysicsBox }

	TPhysicsBox = class(TPhysicsGeom)
  private
    FW, FH, FD: Single;
  protected
  	procedure Draw; override;
		function GetMass: TdMass; override;
  public
    constructor Create(W, H, D: Single);
    constructor CreateComposite(Body: TPhysicsBody; W, H, D: Single);
  end;

{ TPhysicsSphere }

	TPhysicsSphere = class(TPhysicsGeom)
  private
    FR: Single;
  protected
  	procedure Draw; override;
		function GetMass: TdMass; override;
  public
    constructor Create(R: Single);
    constructor CreateComposite(Body: TPhysicsBody; R: Single);
  end;

{ TPhysicsCylinder }

	TPhysicsCylinder = class(TPhysicsGeom)
  private
    FR, FH: Single;
  protected
  	procedure Draw; override;
		function GetMass: TdMass; override;
  public
    constructor Create(R, H: Single);
    constructor CreateComposite(Body: TPhysicsBody; R, H: Single);
  end;

 { TPhysicsCapsule }

	TPhysicsCapsule = class(TPhysicsGeom)
  private
    FR, FH: Single;
  protected
  	procedure Draw; override;
		function GetMass: TdMass; override;
  public
    constructor Create(R, H: Single);
    constructor CreateComposite(Body: TPhysicsBody; R, H: Single);
  end;

 { TPhysicsComposite }

	TPhysicsComposite = class(TPhysicsGeom)
  private
    FGeom: TPhysicsGeom;
  protected
  	procedure Draw; override;
		function GetMass: TdMass; override;
  public
    constructor Create(Body: TPhysicsBody; Geom: TPhysicsGeom);
  	destructor Destroy; override;
		property Geom: TPhysicsGeom read FGeom;
  end;

 { TPhysicsGeomList }

 	TPhysicsGeomList = class
  private
  	FList: TList;
    FOwner: Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): TPhysicsGeom;
	public
    constructor Create(Owner: Boolean = True);
    destructor Destroy; override;
  	procedure Add(Geom: TPhysicsGeom);
    procedure Remove(Geom: TPhysicsGeom);
    procedure Clear;
    property Count: Integer read GetCount;
  	property Item[Index: Integer]: TPhysicsGeom read GetItem; default;
  end;

{ TPhysicsWorld }

	TPhysicsWorld = class
  private
    FContacts: TdJointGroupID;
    FCamera: TPlacement;
    FGeoms: TPhysicsGeomList;
    FSpace: PdxSpace;
    FSynchronizer: TSynchronizer;
    FTime: Single;
    FTimer: ITimer;
    FWorld: PdxWorld;
  public
  	constructor Create(const WorldName: string; Timer: ITimer);
    destructor Destroy; override;
    procedure Draw;
    procedure Step;
   	function GetCamera: TPlacement;
    procedure SetCamera(const P: TPlacement);
    property Geoms: TPhysicsGeomList read FGeoms;
    property Contacts: TdJointGroupID read FContacts;
    property Handle: PdxWorld read FWorld;
    property Space: PdxSpace read FSpace;
    property Synchronizer: TSynchronizer read FSynchronizer;
  end;

var
	World: TPhysicsWorld;

implementation

constructor TSynchronizer.Create(const UniqueName: string);
begin
	FMutex := CreateMutex(nil, True, PChar(UniqueName));
end;

destructor TSynchronizer.Destroy;
begin
	Wait;
  CloseHandle(FMutex);
end;

procedure TSynchronizer.Wait;
begin
	WaitForSingleObject(FMutex, Infinite);
end;

procedure TSynchronizer.Release;
begin
	ReleaseMutex(FMutex);
end;

{ ODE translation routines }

function CreatePhysicsMatrix(const M: TdMatrix3_As3x4; const V: TdVector3): TMatrix; overload;
var
  F: Single;
begin
  Result[0, 0] := M[0, 0];
  Result[0, 1] := M[0, 1];
  Result[0, 2] := M[0, 2];
  Result[0, 3] := 0;
  Result[1, 0] := M[1, 0];
  Result[1, 1] := M[1, 1];
  Result[1, 2] := M[1, 2];
  Result[1, 3] := 0;
  Result[2, 0] := M[2, 0];
  Result[2, 1] := M[2, 1];
  Result[2, 2] := M[2, 2];
  Result[2, 3] := 0;
  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;
  Result[3, 3] := 1;
	F := Result[0, 1]; Result[0, 1] := Result[1, 0]; Result[1, 0] := F;
	F := Result[0, 2]; Result[0, 2] := Result[2, 0]; Result[2, 0] := F;
	F := Result[0, 3]; Result[0, 3] := Result[3, 0]; Result[3, 0] := F;
	F := Result[1, 2]; Result[1, 2] := Result[2, 1]; Result[2, 1] := F;
	F := Result[1, 3]; Result[1, 3] := Result[3, 1]; Result[3, 1] := F;
	F := Result[2, 3]; Result[2, 3] := Result[3, 2]; Result[3, 2] := F;
  Result[3, 0] := V[0];
  Result[3, 1] := V[1];
  Result[3, 2] := V[2];
  Result[3, 3] := 1;
end;

function CreatePhysicsMatrix(const Body: PdxBody): TMatrix; overload;
begin
	Result := CreatePhysicsMatrix(TdMatrix3_As3x4(dBodyGetRotation(Body)^),	dBodyGetPosition(Body)^);
end;

function CreatePhysicsMatrix(const Geom: PdxGeom): TMatrix; overload;
begin
	Result := CreatePhysicsMatrix(TdMatrix3_As3x4(dGeomGetRotation(Geom)^),	dGeomGetPosition(Geom)^);
end;

function CreatePhysicsRotation(H, P, R: Single): TdMatrix3;
var
	M: TMatrix;
begin
	M := CreateMatrix(H, P, R);
  Result[0] := M[0, 0];
  Result[1] := M[0, 1];
  Result[2] := M[0, 2];
  Result[3] := M[0, 3];
  Result[4] := M[1, 0];
  Result[5] := M[1, 1];
  Result[6] := M[1, 2];
  Result[7] := M[1, 3];
  Result[8] := M[2, 0];
  Result[9] := M[2, 1];
  Result[10] := M[2, 2];
  Result[11] := M[2, 3];
end;

{ TPhysicsBody }

constructor TPhysicsBody.Create;
begin
	FBody := dBodyCreate(World.Handle);
  dBodySetData(FBody, Self);
  FGeoms := TPhysicsGeomList.Create(False);
end;

destructor TPhysicsBody.Destroy;
begin
	FGeoms.Free;
	dBodyDestroy(FBody);
end;

procedure TPhysicsBody.Build;
var
	Mass, NodeMass: TdMass;
  Geom: TPhysicsGeom;
  R: TdMatrix3;
  I: Integer;
begin
	dMassSetZero(Mass);
  for I := 0 to FGeoms.Count - 1 do
	begin
  	Geom := FGeoms[I];
    if Geom is TPhysicsComposite then
    	Geom := TPhysicsComposite(Geom).Geom;
	  NodeMass := Geom.GetMass;
    with Geom.FPlacement.Position do
	    dMassTranslate(NodeMass, X, Y, Z);
    with Geom.FPlacement.Direction do
    	R := CreatePhysicsRotation(Heading, Pitch, Roll);
		dMassRotate(Mass, R);
		dMassAdd(Mass, NodeMass);
  end;
  for I := 0 to FGeoms.Count - 1 do
	begin
  	Geom := FGeoms[I];
    if Geom is TPhysicsComposite then
    	Geom := TPhysicsComposite(Geom).Geom;
    with Geom.FPlacement.Position do
			dGeomSetPosition(Geom.Handle, X - Mass.c[0], Y - Mass.c[1], Z - X - Mass.c[2]);
	end;
  dMassTranslate(Mass, -Mass.c[0], -Mass.c[1], -Mass.c[2]);
	dBodySetMass(FBody, @Mass);
end;

(*

void compoundGeom(
    dBodyID aBodyID,   // Body associated with the geoms
    dSpaceID aSpaceID, // Space to put the geoms in
    int aNumGeoms,     // Number of geoms to combine
    dGeomID* aGeoms,   // Array of geoms to be combined
    dMass* aMasses,    // Array of masses for the geoms
    dVector3* aPos,    // Array of positions for the geoms
    dMatrix3* aRot     // Arrao of rotations for the geoms
    ) {
    ASSERT(MAXCOMPOUNDGEOMS < aNumGeoms);

    int i; // Used in several for-loops (scope ambiguity issue)
    dGeomID lXgeoms[MAXCOMPOUNDGEOMS]; // @@@NOTE: Hardcoded max number of components!

    // To accumulate the total mass of the compound geom
    dMass lMass;

    // This is the space where the geoms are inserted.
    // If you want to put each compound geom into a new
    // space, you could use the following line:
    // dSpaceID lSpaceID = dSimpleSpaceCreate(aSpaceID);
    dSpaceID lSpaceID = aSpaceID;


    // Time to combine the geoms
    for(i = 0; i < aNumGeoms; i++) {

        // Put the geom into a transform-geometry
        lXgeoms[i] = dCreateGeomTransform(lSimpleSpaceID);
        dGeomTransformSetCleanup(lXgeoms[i], 1);
        dGeomTransformSetGeom(lXgeoms[i], aGeoms[i]);

        // Set the transformation for the geom and its mass
        dGeomSetPosition(aGeoms[i], aPos[i][0],aPos[i][1],aPos[i][2]);
        dMassTranslate(&aMasses[i],aPos[i][0],aPos[i][1],aPos[i][2]);
        dGeomSetRotation(aGeoms[i],aRot[i]);
        dMassRotate(&aMasses[i], aRot[i]);

        // Add to the total mass
        dMassAdd (&lMass,&aMasses[i]);
    }

    // Move all encapsulated objects so that the center of mass is (0,0,0)
    for(i = 0; i < aNumGeoms; i++) {
        dGeomSetPosition (aGeoms[i],
            aPos[i][0] - lMass.c[0],
            aPos[i][1] - lMass.c[1],
            aPos[i][2] - lMass.c[2]);
    }
    dMassTranslate (&lMass,-lMass.c[0],-lMass.c[1],-lMass.c[2]);

    // Attach to the dBody
    for(i = 0; i < aNumGeoms; i++) {
        dGeomSetBody(lXgeoms[i], aBodyID);
    }
    dBodySetMass(aBodyID, &lMass);
}

*)

procedure TPhysicsBody.AddRef(Geom: TPhysicsGeom);
begin
	FGeoms.Add(Geom);
end;

procedure TPhysicsBody.Release(Geom: TPhysicsGeom);
begin
	FGeoms.Remove(Geom);
  if FGeoms.Count =  0 then Free;
end;

function TPhysicsBody.GetGeom(Index: Integer): TPhysicsGeom;
begin
	Result := FGeoms[Index];
end;

function TPhysicsBody.GetGeomCount: Integer;
begin
	Result := FGeoms.Count;
end;

procedure TPhysicsBody.SetPosition(X, Y, Z: Single);
begin
	dBodySetPosition(FBody, X, Y, Z);
end;

procedure TPhysicsBody.SetRotation(H, P, R: Single);
var
  M: TdMatrix3;
begin
	M := CreatePhysicsRotation(H, P, R);
	dBodySetRotation(FBody, M);
end;

{ TPhysicsGeom }

constructor TPhysicsGeom.Create(AGeom: PdxGeom);
begin
	FGeom := AGeom;
  dGeomSetData(FGeom, Self);
  FDensity := 10;
end;

destructor TPhysicsGeom.Destroy;
begin
	Detatch;
	dGeomDestroy(FGeom);
end;

procedure TPhysicsGeom.Attach;
begin
  Attach(TPhysicsBody.Create);
end;

procedure TPhysicsGeom.Attach(Body: TPhysicsBody);
begin
  if FBody <> nil then
		dGeomSetBody(FGeom, nil);
  if FBody <> nil then
    FBody.Release(Self);
  FBody := Body;
  if FBody <> nil then
  begin
  	FBody.AddRef(Self);
		dGeomSetBody(FGeom, FBody.Handle);
  end;
end;

procedure TPhysicsGeom.Detatch;
begin
  Attach(nil);
end;

procedure TPhysicsGeom.Render;
begin
	glPushMatrix;
  glxMultMatrix(CreatePhysicsMatrix(FGeom));
  Draw;
	glPopMatrix;
end;

function TPhysicsGeom.GetMass: TdMass;
begin
	dMassSetZero(Result);
end;

procedure TPhysicsGeom.SetPosition(X, Y, Z: Single);
begin
	dGeomSetPosition(FGeom, X, Y, Z);
	with FPlacement do
  begin
		Position.X := X;
		Position.Y := Y;
		Position.Z := Z;
  end;
end;

procedure TPhysicsGeom.SetRotation(H, P, R: Single);
var
  M: TdMatrix3;
begin
	M := CreatePhysicsRotation(H, P, R);
	dGeomSetRotation(FGeom, M);
	with FPlacement.Direction do
  begin
		Heading := H;
		Pitch := P;
		Roll := R;
  end;
end;

{ TPhysicsBox }

constructor TPhysicsBox.Create(W, H, D: Single);
begin
  inherited Create(dCreateBox(World.Space, W, H, D));
	FW := W;
	FH := H;
	FD := D;
  World.Geoms.Add(Self);
end;

constructor TPhysicsBox.CreateComposite(Body: TPhysicsBody; W, H, D: Single);
begin
  inherited Create(dCreateBox(nil, W, H, D));
	FW := W;
	FH := H;
	FD := D;
  World.Geoms.Add(TPhysicsComposite.Create(Body, Self));
end;

procedure TPhysicsBox.Draw;
begin
  DrawBox(FW, FH, FD);
end;

function TPhysicsBox.GetMass: TdMass;
begin
	dMassSetBox(Result, Density, FW, FH, FD);
end;

{ TPhysicsSphere }

constructor TPhysicsSphere.Create(R: Single);
begin
  inherited Create(dCreateSphere(World.Space, R));
	FR := R;
  World.Geoms.Add(Self);
end;

constructor TPhysicsSphere.CreateComposite(Body: TPhysicsBody; R: Single);
begin
  inherited Create(dCreateSphere(nil, R));
	FR := R;
  World.Geoms.Add(TPhysicsComposite.Create(Body, Self));
end;

procedure DrawSphere(Radius: Single; Slices, Loops: Integer);
var
	Q: GLUquadricObj;
begin
	Q := gluNewQuadric;
  gluQuadricDrawStyle(Q, GLU_FILL);
  gluSphere(Q, Radius, Slices, Loops);
	gluDeleteQuadric(Q);
end;

procedure TPhysicsSphere.Draw;
begin
  DrawSphere(FR, 8, 8);
end;

function TPhysicsSphere.GetMass: TdMass;
begin
	dMassSetSphere(Result, Density, FR);
end;

{ TPhysicsCylinder }

constructor TPhysicsCylinder.Create(R, H: Single);
begin
  inherited Create(dCreateCylinder(World.Space, R, H));
	FR := R;
  FH := H;
  World.Geoms.Add(Self);
end;

constructor TPhysicsCylinder.CreateComposite(Body: TPhysicsBody; R, H: Single);
begin
  inherited Create(dCreateCylinder(nil, R, H));
	FR := R;
  FH := H;
  World.Geoms.Add(TPhysicsComposite.Create(Body, Self));
end;

procedure DrawCylinder(Radius, Height: Single; Slices: Integer);
var
	Q: GLUquadricObj;
  X: Single;
  I: Integer;
begin
	Q := gluNewQuadric;
  glRotate(90, 1, 0, 0);
  glTranslate(0, 0, -Height / 2);
  gluQuadricDrawStyle(Q, GLU_FILL); // GLU_LINE draws wireframe
	gluCylinder(Q, Radius, Radius, Height, Slices, 1);
	gluDeleteQuadric(Q);
  X := Slices / 2;
  glBegin(GL_POLYGON);
  glNormal3f(0, 0, -1);
  for I := 0 to Slices - 1 do
		glVertex3f(Sin(Pi * (I / X - 0.5)) * Radius, Sin(Pi * I / -X) * Radius, 0);
  glEnd;
  glBegin(GL_POLYGON);
  glNormal3f(0, 0, 1);
  for I := Slices - 1 downto 0 do
		glVertex3f(Sin(Pi * (I / X - 0.5)) * Radius, Sin(Pi * I / -X) * Radius, Height);
  glEnd;
end;

procedure TPhysicsCylinder.Draw;
begin
	DrawCylinder(FR, FH, 12);
end;

function TPhysicsCylinder.GetMass: TdMass;
begin
	dMassSetCylinder(Result, Density, 3, FR, FH);
end;

{ TPhysicsCapsule }

constructor TPhysicsCapsule.Create(R, H: Single);
begin
  inherited Create(dCreateCCylinder(World.Space, R, H));
	FR := R;
  FH := H;
  World.Geoms.Add(Self);
end;

constructor TPhysicsCapsule.CreateComposite(Body: TPhysicsBody; R, H: Single);
begin
  inherited Create(dCreateCCylinder(nil, R, H));
	FR := R;
  FH := H;
  World.Geoms.Add(TPhysicsComposite.Create(Body, Self));
end;

procedure DrawCapsule(Radius, Height: Single; Slices: Integer);
var
	Q: GLUquadricObj;
begin
  glTranslate(0, 0, -Height / 2);
	Q := gluNewQuadric;
  gluQuadricDrawStyle(Q, GLU_FILL);
	gluCylinder(Q, Radius, Radius, Height, Slices, 1);
	gluDeleteQuadric(Q);
	glTranslate(0, 0, Height);
	Q := gluNewQuadric;
  gluQuadricDrawStyle(Q, GLU_FILL);
  gluSphere(Q, Radius, Slices, 8);
	glTranslate(0, 0, -Height);
  gluSphere(Q, Radius, Slices, 8);
	gluDeleteQuadric(Q);
end;

procedure TPhysicsCapsule.Draw;
begin
	DrawCapsule(FR, FH, 12);
end;

function TPhysicsCapsule.GetMass: TdMass;
begin
	dMassSetCappedCylinder(Result, Density, 3, FR, FH);
end;

{ TPhysicsCompo }

constructor TPhysicsComposite.Create(Body: TPhysicsBody; Geom: TPhysicsGeom);
begin
	inherited Create(dCreateGeomTransform(World.Space));
  Attach(Body);
	dGeomTransformSetGeom(Handle, Geom.Handle);
	FGeom := Geom;
end;

destructor TPhysicsComposite.Destroy;
begin
	FGeom.Free;
  inherited Destroy;
end;

function TPhysicsComposite.GetMass: TdMass;
begin
	Result := FGeom.GetMass;
end;

procedure TPhysicsComposite.Draw;
begin
	FGeom.Render;
end;

{ TPhysicsGeomList }

constructor TPhysicsGeomList.Create(Owner: Boolean = True);
begin
	FList := TList.Create;
	FOwner := Owner;
end;

destructor TPhysicsGeomList.Destroy;
begin
	Clear;
	FList.Free;
end;

procedure TPhysicsGeomList.Add(Geom: TPhysicsGeom);
begin
	FList.Add(Geom);
end;

procedure TPhysicsGeomList.Remove(Geom: TPhysicsGeom);
begin
	if (FList.Remove(Geom) > -1) and FOwner then Geom.Free;
end;

procedure TPhysicsGeomList.Clear;
var
	I: Integer;
begin
	if FOwner then
	  for I := 0 to FList.Count - 1 do
  	  TObject(FList[I]).Free;
  FList.Clear;
end;

function TPhysicsGeomList.GetCount: Integer;
begin
	Result := FList.Count;
end;

function TPhysicsGeomList.GetItem(Index: Integer): TPhysicsGeom;
begin
	Result := TPhysicsGeom(FList[Index]);
end;

{ TPhysicsWorld }

constructor TPhysicsWorld.Create(const WorldName: string; Timer: ITimer);
begin
	FSynchronizer :=  TSynchronizer.Create(WorldName);
	FTimer := Timer;
  FTime := FTimer.Time;
  FWorld := dWorldCreate;
  dWorldSetGravity(FWorld, 0, -9.81, 0);
  FSpace := dHashSpaceCreate(nil);
  FContacts := dJointGroupCreate(0);
	FGeoms := TPhysicsGeomList.Create;
end;

destructor TPhysicsWorld.Destroy;
begin
  FGeoms.Free;
  dJointGroupDestroy(FContacts);
  dSpaceDestroy(FSpace);
  dWorldDestroy(FWorld);
	FSynchronizer.Free;
end;

procedure TPhysicsWorld.Draw;
begin
end;

procedure NearCallback(Data: Pointer; O1, O2: PdxGeom); cdecl;
var
	World: TPhysicsWorld absolute Data;
  B1, B2 : PdxBody;
  Contact: array[0..2] of TdContact;
  Joint: TdJointID;
  I: Integer;
begin
  B1 := dGeomGetBody(O1);
  B2 := dGeomGetBody(O2);
  if Assigned(B1) and Assigned(B2) and (dAreConnected(B1, B2) <> 0) then
    Exit;
  for I := Low(Contact) to High(Contact) do
  begin
    Contact[I].surface.mode := dContactBounce;
    Contact[I].surface.mu := 1000;
    Contact[I].surface.bounce := 0.3;
    Contact[I].surface.bounce_vel := 0.1;
  end;
	for I := 0 to dCollide(O1, O2, High(Contact) + 1, Contact[0].geom, SizeOf(TdContact)) - 1 do
	begin
  	Joint := dJointCreateContact(World.Handle, World.Contacts, @Contact[I]);
    dJointAttach(Joint, B1, B2);
	end;
end;

procedure TPhysicsWorld.Step;
const
  PHYSICS_STEP = 0.01;
var
	T: Single;
begin
  T := FTimer.Time;
  while FTime < T do
  begin
    dSpaceCollide(FSpace, Self, NearCallback);
    dWorldQuickStep(FWorld, PHYSICS_STEP);
    dJointGroupEmpty(FContacts);
    FTime := FTime + PHYSICS_STEP;
  end;
end;

function TPhysicsWorld.GetCamera: TPlacement;
begin
	Result := FCamera;
end;

procedure TPhysicsWorld.SetCamera(const P: TPlacement);
begin
	FCamera := P;
end;

end.
