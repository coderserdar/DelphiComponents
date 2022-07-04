unit API_glpanel;

//------------------------------------------------------------------------------
// opengl panel that contains most needed opengl functions, also i've added
// some 3d model loading routines to this.
//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// r1.07, ari pikivirta
//  * added support for internal synchronized drawing thread
//
// r1.06, ari pikivirta
//  * 3DS model reading added (thx sulaco)
//  * added some env.mapping stuff
//
// r1.05, ari pikivirta
//  * fixed SMPL model set model color in make function
//  * added easy interface to supported models
//
// r1.04, ari pikivirta
//  * added SMPL model loading function
//
// r1.03, ari pikivirta
//  * bindtexture function added
//  * GMD meshes loading and drawing added
//  * OBJ model changed to class (similar to GMD)
//  * added file format regognition to texture loading
//
// r1.02, ari pikivirta
//  * added sphere item
//  * added cylinder item
//  * lighting added (types #0 and #1)
//  * fog changed to work without "TAPI_glFog" type
//
// r1.01, ari pikivirta
//  * corrected perspective record
//  * modified items to work with textures
//  * fog effect added
//  * fixed the cube textures
//
// r1.00, ari pikivirta
//  * created
//
//------------------------------------------------------------------------------

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, OpenGl, Graphics, JPEG,
  u_3dsfile, API_base;

type
  TAPI_glpanelevent = procedure of object;

  TAPI_OBJModel = class;
  TAPI_GMDModel = class;
  TAPI_SMPLModel = class;
  TAPI_3DSModel = T3DModel;

  TAPI_glviewpoint = record
    X0,Y0,Z0,X1,Y1,Z1,X2,Y2,Z2: single;
  end;

  TAPI_glperspective = record
    fovy, aspect, znear, zfar: single;
  end;

  TAPI_gldrawthread = class(tthread)
  private
    factive: boolean;
    fdelay: integer;
    fevent: TAPI_glpanelevent;
  protected
    procedure Execute; override;
  end;

  TAPI_glpanel = class(TAPI_Custom_Panel)
  private
    fhdc: HDC;
    frc: HDC;

    fglinit: boolean;
    fwireframe: boolean;
    fbackground: tcolor;
    fondraw3d: tnotifyevent;
    fglstartrender: boolean;
    fautoaspect: boolean;

    flist: GLuint;
    flistactive: boolean;

    faxislength: integer;
    faxiscolorz: tcolor;
    faxiscolorx: tcolor;
    faxiscolory: tcolor;

    fQuadricObj: GluQuadricOBJ;

    fthread: TAPI_gldrawthread;
    fthreadactive: boolean;
    fthreadevent: TAPI_glpanelevent;
    fthreadpriority: tthreadpriority;
    fthreaddelay: integer;
    procedure setthreadactive(b:boolean);

    // internal functions and procedures

    function LoadTexture_BMP(Filename: String; var Texture : GLuint): boolean;
    function LoadTexture_JPG(Filename: String; var Texture: GLuint): boolean;

    procedure dummyb(b: boolean);

  protected
  public
    ViewPoint: TAPI_glviewpoint;                            // view point parameters
    Perspective: TAPI_glperspective;                        // perspective parameters

    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;

    // miscellous
    procedure SetColor(Color:TColor);

    // creation of textures
    function CreateTexture(Width, Height, Format : Word; pData : Pointer): Integer;
    function LoadTexture(filename: string; var texture: GLuint): boolean;

    // creation of items
    procedure Startlist(var item: GLuint);                  // start creating list of items
    function MakeLine(                                      // line object
      X0,Y0,Z0: double; color1: tcolor;
      X1,Y1,Z1: double; color2: tcolor)
      : boolean;
    function MakeTriangle(                                  // triangle
      X0,Y0,Z0: double; color1: tcolor;
      X1,Y1,Z1: double; color2: tcolor;
      X2,Y2,Z2: double; color3: tcolor): boolean;
    function MakePlane(                                     // plane
      X0,Y0,Z0: double; color1: tcolor;
      X1,Y1,Z1: double; color2: tcolor;
      X2,Y2,Z2: double; color3: tcolor;
      X3,Y3,Z3: double; color4: tcolor): boolean;
    function MakeCube(                                      // cube
      X0,Y0,Z0: double; color1: tcolor;
      X1,Y1,Z1: double; color2: tcolor): boolean;
    function MakeSphere(                                    // sphere
      radius: double; slices, loops: integer): boolean;
    function MakeCylinder(                                  // cylinder
      baseradius, topradius: double;
      height: single;
      slices, stacks: integer): boolean;
    function Endlist: boolean;                              // end of object list

    // drawing items
    procedure StartRender;                                  // start rendering
      procedure DrawAxis;                                   // draw axis (x, y, z)
      procedure StartItem;                                  // start drawing group
        procedure MoveItem(x,y,z: single);                  // move group
        procedure ScaleItem(x,y,z: single);                 // scale group
        procedure RotateItem(angle,x,y,z: single);          // rotate group
        procedure BindTexture(texture: GLuint);             // fill with texture
        procedure DrawItem(Item: GLuint);                   // draw new object
        procedure DrawTexturedItem(item, texture: GLuint);  // draw object with texture
      procedure EndItem;                                    // end group
    procedure EndRender;                                    // end drawing (update view)

    // Blend
    procedure StartBlend;
    procedure EndBlend;
    procedure StartSphericalEnvironmentMapping;
    procedure EndSphericalEnvironmentMapping;

    // Fog
    procedure StartFog(                                     // fog parameters
      mode: integer;
      color: tcolor;
      density: single;
      depth: single;
      enddepth: single);
    procedure EndFog;                                       // done with fog

    // Lights
    function SetLight(                                      // set light source
      num: integer;
      x0,y0,z0: single;
      amb0, amb1, amb2: single;
      dif0, dif1, dif2: single;
      state: boolean): boolean;
    procedure DisableLighting;                              // remove all lights

    // 3d models
    function LoadModel(filename, modelname: string; var model: GLuint): boolean;

  published
    property Init: boolean read fglinit write dummyb stored false;
    property Background: tcolor read fbackground write fbackground;
    property WireFrame: boolean read fwireframe write fwireframe;

    property AutoAspect: boolean read fautoaspect write fautoaspect;

    property AxisLength: integer read faxislength write faxislength;
    property AxisColorZ: tcolor read faxiscolorz write faxiscolorz;
    property AxisColorX: tcolor read faxiscolorx write faxiscolorx;
    property AxisColorY: tcolor read faxiscolory write faxiscolory;

    property OnDraw3d: tnotifyevent read fondraw3d write fondraw3d;

    property ThreadActive: boolean read fthreadactive write setthreadactive;
    property ThreadEvent: TAPI_glpanelevent read fthreadevent write fthreadevent;
    property ThreadPriority: tthreadpriority read fthreadpriority write fthreadpriority;
    property ThreadDelay: integer read fthreaddelay write fthreaddelay;

  end;

  // OBJ MODEL -------------------

  TOBJColor = Record
    R,G,B: glFLoat;
  end;

  TOBJCoord = Record
    X,Y,Z: glFLoat;
  end;

  TOBJTexCoord = Record
    U,V: glFloat;
  end;

  TOBJMaterial = Record
    Name: String;
    Ambient: TOBJColor;
    Diffuse: TOBJColor;
    Specular: TOBJColor;
    Shininess: glFloat;
    Texture: glUint;
  end;

  TOBJFace = Record
    Count: Integer;
    vIndex: Array of Integer;
    tIndex: Array of Integer;
    nIndex: Array of Integer;
  end;

  TOBJGroup = Record
    Name: String;
    Faces: Integer;
    Face: Array of TOBJFace;
    mIndex: Integer;
  end;

  TAPI_OBJModel = class

    // model data
    Name: String;
    MaterialFile: String;
    Vertices: Integer;
    Normals: Integer;
    TexCoords: Integer;
    Materials: Integer;
    Groups: Integer;
    Vertex: Array of TOBJCoord;
    Normal: Array of TOBJCoord;
    TexCoord: Array of TOBJTexCoord;
    Group: Array of TOBJGroup;
    Material: Array of TOBJMaterial;

    // internal functions
    procedure InitModel;
    function GetCoords(S : String) : TOBJCoord;
    function GetTexCoords(S : String) : TOBJTexCoord;
    procedure ReadVertexData(S : String);
    procedure ReadFaceData(S : String);
    procedure GetMaterialName(S : String);
    procedure CreateMaterial(S : String);
    procedure GetMaterial(S : String);
    procedure GetShininess(S : String);
    procedure GetTexture(S : String);
    procedure LoadMaterials(S : String);

  public
    constructor Create;
    destructor Destroy; override;

    function DrawGroup(i: integer; owntexture: boolean): boolean;
    function LoadFromFile(filename: string): boolean;
    function Make: boolean;
    function Draw: boolean;
    function DrawTextured(tex: GLuint): boolean;

    property GroupCount: integer read groups;

  end;

  // GMD MODEL -------------------

  PGLVertex = ^TGLVertex;
  TGLVertex =
  record
    x,y,z : glfloat;
  end;

  PGLVector = ^TGLVector;
  TGLVector = array[0..2] of glfloat;

  PGLFace = ^TGLFace;
  TGLFace = array[0..2] of word;

  PGLVertexArray = ^TGLVertexArray;
  TGLVertexArray = array[Word] of TGLVertex;

  PGLFacesArray = ^TGLFacesArray;
  TGLFacesArray = array[word] of TGLFace;

  TGMDMesh = class
    Vertices,
    FasetNormals,
    SmoothNormals: PGLVertexArray;
    Faces: PGLFacesArray;
    Parent: TAPI_GMDModel;
  private
    procedure Draw;
  public
    destructor Destroy; override;
  end;

  TAPI_GMDModel = class
    Meshes: TList;
    CurMesh: word;
    TexVertices: PGLVertexArray;
    TexFaces: PGLFacesArray;

    TimeIsSet: boolean;
    TimeEnd: dword;

    MeshCount: word;
    MeshVCount: word;
    MeshFCount: word;
    isFaset: byte;
    isSmooth: byte;
    TexVCount: word;
    TexFCount: word;
    IsOwnTexture: byte;
    OwnTexture: gluint;
  public
    function LoadFromFile(FileName, ModelName:String):boolean;
    procedure Draw(StartMesh,EndMesh:word; mps:byte);
    procedure DrawTextured(StartMesh,EndMesh:word; mps:byte; tex:GLuint);
    function Make(Mesh: word): boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  // SMPL model ----------------

  TSMPLtriangle = record
    x0: GLfloat;
    y0: GLfloat;
    z0: GLfloat;
    x1: GLfloat;
    y1: GLfloat;
    z1: GLfloat;
    X2: GLfloat;
    y2: GLfloat;
    z2: GLfloat;
  end;

  TSMPLcolor = record
    r: GLfloat;
    g: GLfloat;
    b: GLfloat;
  end;

  TSMPLobject = record
    color: TSMPLcolor;
    triangle: array of TSMPLtriangle;
    triangles: integer;
  end;

  TAPI_SMPLModel = class
    obj: array of TSMPLObject;
    objs: integer;
    triangles: integer; // total
    procedure Initmodel;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromFile(Filename: string): boolean;
    function Make: boolean;
    function Draw: boolean;
    function DrawTextured(tex: GLuint): boolean;
  end;

procedure Register;

implementation

{$include '..\API_source\inc\CompilerVersions.INC'}
{$r *.res}

uses
  Dialogs, StrUtils;

const
  versioninfostring: string = 'r1.07/ari.pikivirta@kolumbus.fi';

  // Definition of a complete font
  rasters: array[0..94] of array[0..12] of GLubyte = (
  ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),    // " "
  ($00, $00, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $18),
  ($00, $00, $00, $00, $00, $00, $00, $00, $00, $36, $36, $36, $36),
  ($00, $00, $00, $66, $66, $ff, $66, $66, $ff, $66, $66, $00, $00),
  ($00, $00, $18, $7e, $ff, $1b, $1f, $7e, $f8, $d8, $ff, $7e, $18),
  ($00, $00, $0e, $1b, $db, $6e, $30, $18, $0c, $76, $db, $d8, $70),
  ($00, $00, $7f, $c6, $cf, $d8, $70, $70, $d8, $cc, $cc, $6c, $38),
  ($00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $1c, $0c, $0e),
  ($00, $00, $0c, $18, $30, $30, $30, $30, $30, $30, $30, $18, $0c),
  ($00, $00, $30, $18, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $18, $30),
  ($00, $00, $00, $00, $99, $5a, $3c, $ff, $3c, $5a, $99, $00, $00),
  ($00, $00, $00, $18, $18, $18, $ff, $ff, $18, $18, $18, $00, $00),
  ($00, $00, $30, $18, $1c, $1c, $00, $00, $00, $00, $00, $00, $00),
  ($00, $00, $00, $00, $00, $00, $ff, $ff, $00, $00, $00, $00, $00),
  ($00, $00, $00, $38, $38, $00, $00, $00, $00, $00, $00, $00, $00),
  ($00, $60, $60, $30, $30, $18, $18, $0c, $0c, $06, $06, $03, $03),
  ($00, $00, $3c, $66, $c3, $e3, $f3, $db, $cf, $c7, $c3, $66, $3c),
  ($00, $00, $7e, $18, $18, $18, $18, $18, $18, $18, $78, $38, $18),
  ($00, $00, $ff, $c0, $c0, $60, $30, $18, $0c, $06, $03, $e7, $7e),
  ($00, $00, $7e, $e7, $03, $03, $07, $7e, $07, $03, $03, $e7, $7e),
  ($00, $00, $0c, $0c, $0c, $0c, $0c, $ff, $cc, $6c, $3c, $1c, $0c),
  ($00, $00, $7e, $e7, $03, $03, $07, $fe, $c0, $c0, $c0, $c0, $ff),
  ($00, $00, $7e, $e7, $c3, $c3, $c7, $fe, $c0, $c0, $c0, $e7, $7e),
  ($00, $00, $30, $30, $30, $30, $18, $0c, $06, $03, $03, $03, $ff),
  ($00, $00, $7e, $e7, $c3, $c3, $e7, $7e, $e7, $c3, $c3, $e7, $7e),
  ($00, $00, $7e, $e7, $03, $03, $03, $7f, $e7, $c3, $c3, $e7, $7e),
  ($00, $00, $00, $38, $38, $00, $00, $38, $38, $00, $00, $00, $00),
  ($00, $00, $30, $18, $1c, $1c, $00, $00, $1c, $1c, $00, $00, $00),
  ($00, $00, $06, $0c, $18, $30, $60, $c0, $60, $30, $18, $0c, $06),
  ($00, $00, $00, $00, $ff, $ff, $00, $ff, $ff, $00, $00, $00, $00),
  ($00, $00, $60, $30, $18, $0c, $06, $03, $06, $0c, $18, $30, $60),
  ($00, $00, $18, $00, $00, $18, $18, $0c, $06, $03, $c3, $c3, $7e),
  ($00, $00, $3f, $60, $cf, $db, $d3, $dd, $c3, $7e, $00, $00, $00),
  ($00, $00, $c3, $c3, $c3, $c3, $ff, $c3, $c3, $c3, $66, $3c, $18),
  ($00, $00, $fe, $c7, $c3, $c3, $c7, $fe, $c7, $c3, $c3, $c7, $fe),
  ($00, $00, $7e, $e7, $c0, $c0, $c0, $c0, $c0, $c0, $c0, $e7, $7e),
  ($00, $00, $fc, $ce, $c7, $c3, $c3, $c3, $c3, $c3, $c7, $ce, $fc),
  ($00, $00, $ff, $c0, $c0, $c0, $c0, $fc, $c0, $c0, $c0, $c0, $ff),
  ($00, $00, $c0, $c0, $c0, $c0, $c0, $c0, $fc, $c0, $c0, $c0, $ff),
  ($00, $00, $7e, $e7, $c3, $c3, $cf, $c0, $c0, $c0, $c0, $e7, $7e),
  ($00, $00, $c3, $c3, $c3, $c3, $c3, $ff, $c3, $c3, $c3, $c3, $c3),
  ($00, $00, $7e, $18, $18, $18, $18, $18, $18, $18, $18, $18, $7e),
  ($00, $00, $7c, $ee, $c6, $06, $06, $06, $06, $06, $06, $06, $06),
  ($00, $00, $c3, $c6, $cc, $d8, $f0, $e0, $f0, $d8, $cc, $c6, $c3),
  ($00, $00, $ff, $c0, $c0, $c0, $c0, $c0, $c0, $c0, $c0, $c0, $c0),
  ($00, $00, $c3, $c3, $c3, $c3, $c3, $c3, $db, $ff, $ff, $e7, $c3),
  ($00, $00, $c7, $c7, $cf, $cf, $df, $db, $fb, $f3, $f3, $e3, $e3),
  ($00, $00, $7e, $e7, $c3, $c3, $c3, $c3, $c3, $c3, $c3, $e7, $7e),
  ($00, $00, $c0, $c0, $c0, $c0, $c0, $fe, $c7, $c3, $c3, $c7, $fe),
  ($00, $00, $3f, $6e, $df, $db, $c3, $c3, $c3, $c3, $c3, $66, $3c),
  ($00, $00, $c3, $c6, $cc, $d8, $f0, $fe, $c7, $c3, $c3, $c7, $fe),
  ($00, $00, $7e, $e7, $03, $03, $07, $7e, $e0, $c0, $c0, $e7, $7e),
  ($00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $ff),
  ($00, $00, $7e, $e7, $c3, $c3, $c3, $c3, $c3, $c3, $c3, $c3, $c3),
  ($00, $00, $18, $3c, $3c, $66, $66, $c3, $c3, $c3, $c3, $c3, $c3),
  ($00, $00, $c3, $e7, $ff, $ff, $db, $db, $c3, $c3, $c3, $c3, $c3),
  ($00, $00, $c3, $66, $66, $3c, $3c, $18, $3c, $3c, $66, $66, $c3),
  ($00, $00, $18, $18, $18, $18, $18, $18, $3c, $3c, $66, $66, $c3),
  ($00, $00, $ff, $c0, $c0, $60, $30, $7e, $0c, $06, $03, $03, $ff),
  ($00, $00, $3c, $30, $30, $30, $30, $30, $30, $30, $30, $30, $3c),
  ($00, $03, $03, $06, $06, $0c, $0c, $18, $18, $30, $30, $60, $60),
  ($00, $00, $3c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $3c),
  ($00, $00, $00, $00, $00, $00, $00, $00, $00, $c3, $66, $3c, $18),
  ($ff, $ff, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
  ($00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $38, $30, $70),
  ($00, $00, $7f, $c3, $c3, $7f, $03, $c3, $7e, $00, $00, $00, $00),
  ($00, $00, $fe, $c3, $c3, $c3, $c3, $fe, $c0, $c0, $c0, $c0, $c0),
  ($00, $00, $7e, $c3, $c0, $c0, $c0, $c3, $7e, $00, $00, $00, $00),
  ($00, $00, $7f, $c3, $c3, $c3, $c3, $7f, $03, $03, $03, $03, $03),
  ($00, $00, $7f, $c0, $c0, $fe, $c3, $c3, $7e, $00, $00, $00, $00),
  ($00, $00, $30, $30, $30, $30, $30, $fc, $30, $30, $30, $33, $1e),
  ($7e, $c3, $03, $03, $7f, $c3, $c3, $c3, $7e, $00, $00, $00, $00),
  ($00, $00, $c3, $c3, $c3, $c3, $c3, $c3, $fe, $c0, $c0, $c0, $c0),
  ($00, $00, $18, $18, $18, $18, $18, $18, $18, $00, $00, $18, $00),
  ($38, $6c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $00, $00, $0c, $00),
  ($00, $00, $c6, $cc, $f8, $f0, $d8, $cc, $c6, $c0, $c0, $c0, $c0),
  ($00, $00, $7e, $18, $18, $18, $18, $18, $18, $18, $18, $18, $78),
  ($00, $00, $db, $db, $db, $db, $db, $db, $fe, $00, $00, $00, $00),
  ($00, $00, $c6, $c6, $c6, $c6, $c6, $c6, $fc, $00, $00, $00, $00),
  ($00, $00, $7c, $c6, $c6, $c6, $c6, $c6, $7c, $00, $00, $00, $00),
  ($c0, $c0, $c0, $fe, $c3, $c3, $c3, $c3, $fe, $00, $00, $00, $00),
  ($03, $03, $03, $7f, $c3, $c3, $c3, $c3, $7f, $00, $00, $00, $00),
  ($00, $00, $c0, $c0, $c0, $c0, $c0, $e0, $fe, $00, $00, $00, $00),
  ($00, $00, $fe, $03, $03, $7e, $c0, $c0, $7f, $00, $00, $00, $00),
  ($00, $00, $1c, $36, $30, $30, $30, $30, $fc, $30, $30, $30, $00),
  ($00, $00, $7e, $c6, $c6, $c6, $c6, $c6, $c6, $00, $00, $00, $00),
  ($00, $00, $18, $3c, $3c, $66, $66, $c3, $c3, $00, $00, $00, $00),
  ($00, $00, $c3, $e7, $ff, $db, $c3, $c3, $c3, $00, $00, $00, $00),
  ($00, $00, $c3, $66, $3c, $18, $3c, $66, $c3, $00, $00, $00, $00),
  ($c0, $60, $60, $30, $18, $3c, $66, $66, $c3, $00, $00, $00, $00),
  ($00, $00, $ff, $60, $30, $18, $0c, $06, $ff, $00, $00, $00, $00),
  ($00, $00, $0f, $18, $18, $18, $38, $f0, $38, $18, $18, $18, $0f),
  ($18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18),
  ($00, $00, $f0, $18, $18, $18, $1c, $0f, $1c, $18, $18, $18, $f0),
  ($00, $00, $00, $00, $00, $00, $06, $8f, $f1, $60, $00, $00, $00));

//------------------------------------------------------------------------------
function gluBuild2DMipmaps(Target: GLenum; Components, Width, Height: GLint; Format, atype: GLenum; Data: Pointer): GLint; stdcall; external glu32;
procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;
procedure glDeleteTextures(n:GLsizei; var textureNames:GLuint); stdcall; external opengl32; //'opengl32.dll';

procedure TAPI_glpanel.dummyb(b: boolean); begin end;

//------------------------------------------------------------------------------
constructor TAPI_glpanel.create(aowner: tcomponent);
var
  pfd: TPixelFormatDescriptor;
begin
  inherited create(aowner);
  caption:='API_GLPANEL';
  version:=versioninfostring;
  fglstartrender:=false;
  flistactive:=false;
  fautoaspect:=true;

  // set default perspective
  perspective.fovy:=45;
  perspective.aspect:=width/height;
  perspective.znear:=0.1;
  perspective.zfar:=100;

  // set default viewpoint
  viewpoint.X0:=5;
  viewpoint.Y0:=5;
  viewpoint.Z0:=5;
  viewpoint.X1:=0;
  viewpoint.Y1:=0;
  viewpoint.Z1:=0;
  viewpoint.X2:=0;
  viewpoint.Y2:=0;
  viewpoint.Z2:=1;

  // init opengl if not designing
  if not (csdesigning in componentstate) then
  begin
    with pfd do
    begin
      nSize:=sizeof(pfd);
      nVersion:=1;
      dwFlags:=PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER or 0;
      cColorBits:=16;
      iPixelType:=PFD_TYPE_RGBA;
    end;
    fhdc:=getdc(Self.Handle);
    setpixelformat(fhdc, ChoosePixelFormat(fhdc, @pfd), @pfd);
    frc:=wglCreateContext(fhdc);
    wglMakeCurrent(fhdc,frc);
    glDisable(GL_TEXTURE_2D);
    glShadeModel(GL_SMOOTH);
    glEnable(GL_DEPTH_TEST);
    glClearDepth(1.0);
    glDepthFunc(GL_LESS);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

    // Create A Pointer To The Quadric Objec
    fQuadricObj := gluNewQuadric();
    gluQuadricTexture(fQuadricObj, GL_True);

    fglinit:=true;
  end else
    fglinit:=false;
end;

//------------------------------------------------------------------------------
destructor TAPI_glpanel.destroy;
begin
  if fglinit then
  begin
    // release handles
    if (not wglMakeCurrent(fhdc, 0)) then // failed;
    if (not wglDeleteContext(frc)) then // failed;
    ReleaseDC(self.handle, fhdc);
  end;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_glPanel.StartBlend;
begin
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glEnable(GL_BLEND);
//  glDisable(GL_DEPTH_TEST);
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.EndBlend;
begin
  glDisable(GL_BLEND);
//  glEnable(GL_DEPTH_TEST);
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.CreateTexture(Width, Height, Format : Word; pData : Pointer): Integer;
var
  Texture : GLuint;
begin
  glGenTextures(1, Texture);
  glBindTexture(GL_TEXTURE_2D, Texture);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  //glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  if Format = GL_RGBA then
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData)
    else gluBuild2DMipmaps(GL_TEXTURE_2D, 3, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, pData);
  result :=Texture;
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.SetLight(
  num: integer;
  x0,y0,z0: single;
  amb0, amb1, amb2: single;
  dif0, dif1, dif2: single;
  state: boolean): boolean;
var
  position: array[0..3] of GLfloat;
  ambient: array[0..3] of GLfloat;
  diffuse: array[0..3] of GLfloat;
begin
  result:=false;

  // position
  position[0]:=x0;
  position[1]:=y0;
  position[2]:=z0;
  position[3]:=1;

  ambient[0]:=amb0;
  ambient[1]:=amb1;
  ambient[2]:=amb2;
  ambient[3]:=1;

  diffuse[0]:=dif0;
  diffuse[1]:=dif1;
  diffuse[2]:=dif2;
  diffuse[3]:=1;

  // enable light
  case num of
  0: begin
       glLightfv(GL_LIGHT0, GL_POSITION, @position);
       gllightfv(GL_LIGHT0, GL_AMBIENT, @ambient);
       gllightfv(GL_LIGHT0, GL_DIFFUSE, @diffuse);
       if state then glEnable(GL_LIGHT0)
         else glDisable(GL_LIGHT0);
       result:=true;
     end;
  1: begin
       glLightfv(GL_LIGHT1, GL_POSITION, @position);
       gllightfv(GL_LIGHT1, GL_AMBIENT, @ambient);
       gllightfv(GL_LIGHT1, GL_DIFFUSE, @diffuse);
       if state then glEnable(GL_LIGHT1)
         else glDisable(GL_LIGHT1);
       result:=true;
     end;
  2: begin
       glLightfv(GL_LIGHT2, GL_POSITION, @position);
       gllightfv(GL_LIGHT2, GL_AMBIENT, @ambient);
       gllightfv(GL_LIGHT2, GL_DIFFUSE, @diffuse);
       if state then glEnable(GL_LIGHT2)
         else glDisable(GL_LIGHT2);
       result:=true;
     end;
  end;

  glEnable(GL_LIGHTING);
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.DisableLighting;
begin
  glDisable(GL_LIGHTING);
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.ScaleItem(x,y,z: single);
begin
  glScalef(x,y,z);
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.StartSphericalEnvironmentMapping;
begin
  glEnable(GL_TEXTURE_GEN_S);
  glEnable(GL_TEXTURE_GEN_T);
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.EndSphericalEnvironmentMapping;
begin
  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.LoadTexture_bmp(Filename: String; var Texture : GLuint): Boolean;

  procedure SwapRGB(data : Pointer; Size : Integer);
  asm
    mov ebx, eax
    mov ecx, size
  @@loop :
    mov al,[ebx+0]
    mov ah,[ebx+2]
    mov [ebx+2],al
    mov [ebx+0],ah
    add ebx,3
    dec ecx
    jnz @@loop
  end;

var
  fheader: BITMAPFILEHEADER;
  iheader: BITMAPINFOHEADER;
  palette: array of RGBQUAD;
  bmpfile: thandle;
  bmplength: cardinal;
  pallength: cardinal;
  readbytes: cardinal;
  width, height: integer;
  pData: Pointer;

begin
  result:=false;

  if not fileexists(filename) then
    exit;

  bmpfile := CreateFile(PChar(Filename), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);

  if (bmpfile<>INVALID_HANDLE_VALUE) then
  try
    // read headers
    ReadFile(bmpfile, fheader, SizeOf(fheader), ReadBytes, nil);
    ReadFile(bmpfile, iheader, SizeOf(iheader), ReadBytes, nil);

    // create palette
    pallength:= iheader.biClrUsed;
    SetLength(Palette, pallength);
    ReadFile(bmpfile, Palette, pallength, ReadBytes, nil);
    if (ReadBytes <> pallength) then exit;

    // get picture dimensions
    width:= iheader.biWidth;
    height:= iheader.biHeight;
    bmplength := iheader.biSizeImage;

    // fix the length if it didn't exist
    if bmplength=0 then
      bmplength:= Width * Height * iheader.biBitCount Div 8;

    // read actual bitmap
    getmem(pData, bmplength);
    try
      ReadFile(bmpfile, pData^, bmplength, ReadBytes, nil);
      if (ReadBytes <> bmplength) then exit;
      SwapRGB(pData, Width*Height);
      texture:=CreateTexture(width, height, GL_RGB, pData);
      result:=true;
    finally
      freeMem(pData);
    end;

  finally
    closehandle(bmpfile);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.LoadTexture_jpg(Filename: String; var Texture: GLuint): Boolean;
var
  Data : Array of LongWord;
  W, Width : Integer;
  H, Height : Integer;
  BMP : TBitmap;
  JPG : TJPEGImage;
  C : LongWord;
  Line : ^LongWord;
begin
  result:= false;

  if not fileexists(filename) then
    exit;

  // create jpg image
  jpg:=TJPEGImage.Create;
  try

    try
      jpg.LoadFromFile(Filename);
    except
      exit;
    end;

    // convert to bitmap
    bmp:=TBitmap.Create;
    try
      bmp.pixelformat:=pf32bit;
      bmp.width:=jpg.width;
      bmp.height:=jpg.height;
      bmp.canvas.draw(0,0,jpg);

      // set dimensions
      width:= bmp.Width;
      height:= bmp.Height;
      setlength(data, width*height);

      for H:=0 to height-1 do
      begin
        line:= bmp.scanline[height-H-1];
        for w:=0 to Width-1 do
        begin
          c:=line^ and $ffffff;
          data[W+(H*Width)] :=(((c and $ff) shl 16)+(c shr 16)+(c and $ff00)) or $ff000000;
          inc(line);
        end;
      end;

    finally
      bmp.free;
    end;

  finally
    jpg.free;
  end;

  texture:=createTexture(width, height, GL_RGBA, addr(data[0]));
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.LoadTexture(filename: string; var texture: GLuint): boolean;
var
  s: string;
begin
  s:=extractfileext(filename);

  if uppercase(s)='.JPG' then
    result:=loadtexture_jpg(filename, texture)
    else
  if uppercase(s)='.BMP' then
    result:=loadtexture_bmp(filename, texture)
    else
    result:=false;
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.SetColor(Color:TColor);
begin
  glColor3ub((color and $0000FF),(color and $00ff00)shr 8,(Color and $ff0000)shr 16);
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.Startlist(var item: GLuint);
begin
  if flistactive then exit;
  flistactive:=true;
  flist:=glGenLists(1);
  glNewList(flist, GL_COMPILE);
  item:=flist;
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.Endlist: boolean;
begin
  result:=false;
  if flistactive then
  begin
    glEndList;
    flistactive:=false;
    result:=true;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.LoadModel(filename, modelname: string; var model: GLuint): boolean;
var
  ext: string;
  fOBJModel: TAPI_OBJModel;
  fGMDModel: TAPI_GMDModel;
  fSMPLModel: TAPI_SMPLModel;
  f3DSModel: TAPI_3DSModel;
begin
  result:=false;
  ext:=uppercase(Extractfileext(filename));

  // 3D max
  if ext='.3DS' then
  begin
    f3DSModel:=T3DModel.Create;
    try
      result:=f3DSModel.LoadFromFile(filename);
      if result then
      begin
        startlist(model);
        f3DSModel.Draw;
        endlist;
      end;
    finally
      f3DSModel.Free;
    end;
  end else

  // Lightwave
  if ext='.OBJ' then
  begin
    fobjmodel:=tAPI_objmodel.create;
    try
      result:=fOBJModel.LoadFromFile(filename);
      if result then
      begin
        startlist(model);
        fOBJModel.Draw;
        endlist;
      end;
    finally
      fobjmodel.Free;
    end;
  end else

  // ??
  if (ext='.GMD') or (ext='.GMP') then
  begin
    fgmdmodel:=tapi_gmdmodel.Create;
    try
      result:=fGMDmodel.LoadFromFile(filename, modelname);
      if result then
      begin
        startlist(model);
        fGMDModel.make(0);
        endlist;
      end;
    finally
      fgmdmodel.Free;
    end;
  end else

  // SMPL
  if (ext='.SMPL') then
  begin
    fsmplmodel:=tapi_smplmodel.create;
    try
      result:=fSMPLmodel.LoadFromFile(filename);
      if result then
      begin
        startlist(model);
        fSMPLmodel.Draw;
        endlist;
      end;
    finally
      fsmplmodel.free;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.StartItem;
begin
  glPushMatrix;
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.EndItem;
begin
  glPopMatrix;
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.MoveItem(x,y,z: single);
begin
  glTranslatef(x,y,z);
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.RotateItem(angle,x,y,z: single);
begin
  glRotatef(angle,x,y,z);
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.MakeLine(
  X0,Y0,Z0: double; color1: tcolor;
  X1,Y1,Z1: double; color2: tcolor): boolean;
begin
  result:=false;
  if fglinit then
  begin
    glBegin(GL_LINE);
      setcolor(color1);
      glVertex(X0,Y0,Z0);
      setcolor(color2);
      glVertex(X1,Y1,Z1);
    glEnd;
    result:=true;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.MakeTriangle(
  X0,Y0,Z0: double; color1: tcolor;
  X1,Y1,Z1: double; color2: tcolor;
  X2,Y2,Z2: double; color3: tcolor): boolean;
begin
  result:=false;
  if fglinit then
  begin
    glBegin(GL_TRIANGLES);
      setcolor(color1); glTexCoord2f(0,0); glVertex(x0,y0,z0);
      setcolor(color2); glTexCoord2f(1,0); glVertex(x1,y1,z1);
      setcolor(color3); glTexCoord2f(1,1); glTexCoord2f(0,1); glVertex(x2,y2,z2);
    glEnd;
    result:=true;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.MakeCube(
  X0,Y0,Z0: double; color1: tcolor;
  X1,Y1,Z1: double; color2: tcolor): boolean;
begin
  result:=false;
  if fglinit then
  begin
    glBegin(GL_QUADS);

      // Front Face
      SetColor(color1);
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0); glVertex3f(x0,y0,z1);
      glTexCoord2f(1.0, 0.0); glVertex3f(x1,y0,z1);
      glTexCoord2f(1.0, 1.0); glVertex3f(x1,y1,z1);
      glTexCoord2f(0.0, 1.0); glVertex3f(x0,y1,z1);

      // Back Face
      setcolor(color2);
      glNormal3f( 0.0, 0.0,-1.0);
      glTexCoord2f(1.0, 0.0); glVertex3f(x0,y0,z0);
      glTexCoord2f(1.0, 1.0); glVertex3f(x0,y1,z0);
      glTexCoord2f(0.0, 1.0); glVertex3f(x1,y1,z0);
      glTexCoord2f(0.0, 0.0); glVertex3f(x1,y0,z0);

      // Top Face
      setcolor(color1);
      glNormal3f( 0.0, 1.0, 0.0);
      glTexCoord2f(0.0, 1.0); glVertex3f(x0,y1,z0);
      glTexCoord2f(0.0, 0.0); glVertex3f(x0,y1,z1);
      glTexCoord2f(1.0, 0.0); glVertex3f(x1,y1,z1);
      glTexCoord2f(1.0, 1.0); glVertex3f(x1,y1,z0);

      // Bottom Face
      setcolor(color2);
      glNormal3f( 0.0,-1.0, 0.0);
      glTexCoord2f(1.0, 1.0); glVertex3f(x0,y0,z0);
      glTexCoord2f(0.0, 1.0); glVertex3f(x1,y0,z0);
      glTexCoord2f(0.0, 0.0); glVertex3f(x1,y0,z1);
      glTexCoord2f(1.0, 0.0); glVertex3f(x0,y0,z1);

      // Right face
      setcolor(color1);
      glNormal3f( 1.0, 0.0, 0.0);
      glTexCoord2f(1.0, 0.0); glVertex3f(x1,y0,z0);
      glTexCoord2f(1.0, 1.0); glVertex3f(x1,y1,z0);
      glTexCoord2f(0.0, 1.0); glVertex3f(X1,y1,z1);
      glTexCoord2f(0.0, 0.0); glVertex3f(x1,y0,z1);

      // Left Face
      setcolor(color2);
      glNormal3f(-1.0, 0.0, 0.0);
      glTexCoord2f(0.0, 0.0); glVertex3f(x0,y0,z0);
      glTexCoord2f(1.0, 0.0); glVertex3f(x0,y0,z1);
      glTexCoord2f(1.0, 1.0); glVertex3f(x0,y1,z1);
      glTexCoord2f(0.0, 1.0); glVertex3f(x0,y1,z0);

    glEnd;
    result:=true;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.MakePlane(
  X0,Y0,Z0: double; color1: tcolor;
  X1,Y1,Z1: double; color2: tcolor;
  X2,Y2,Z2: double; color3: tcolor;
  X3,Y3,Z3: double; color4: tcolor): boolean;
begin
  result:=false;
  if fglinit then
  begin
    glBegin(GL_QUADS);
      SetColor(color1); glTexCoord2f(0,0); glVertex3f(X0,Y0,Z0);
      setcolor(color2); glTexCoord2f(1,0); glVertex3f(X1,Y1,Z1);
      SetColor(color3); glTexCoord2f(1,1); glVertex3f(X2,Y2,Z2);
      setcolor(color4); glTexCoord2f(0,1); glVertex3f(X3,Y3,Z3);
    glEnd;
    result:=true;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.MakeSphere(radius: double; slices, loops: integer): boolean;
begin
  result:=false;
  if not fglinit then exit;
  gluSphere(fQuadricObj, radius, slices, loops);
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_glpanel.MakeCylinder(baseradius, topradius: double; height: single; slices, stacks: integer): boolean;
begin
  result:=false;
  if not fglinit then exit;
  gluCylinder(fQuadricObj, baseradius, topradius, height, slices, stacks);
  result:=true;
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.StartRender;
begin
  if fglinit then
  begin

    // autoaspect
    if fautoaspect then
      perspective.aspect:=width/height;

    // config panel size
    glViewport(0,0,width,height);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(perspective.fovy, perspective.aspect, perspective.znear, perspective.zfar);
    glMatrixMode(GL_MODELVIEW);
    glClearColor((fbackground AND $000000FF) / 255,((fbackground AND $0000FF00) DIV 256) / 255,((fbackground AND $00FF0000) DIV 65536) / 255, 1);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();

    gluLookAt(viewpoint.X0, viewpoint.Y0, viewpoint.Z0,
      viewpoint.X1, viewpoint.Y1, viewpoint.Z1,
      viewpoint.X2, viewpoint.Y2, viewpoint.Z2);

    // set wireframe model
    if fwireframe then
      glPolygonmode(GL_FRONT_AND_BACK, GL_LINE)else glPolygonmode(GL_FRONT_AND_BACK, GL_FILL);

    fglstartrender:=true;
    if assigned(OnDraw3D) then
      OnDraw3D(self);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.DrawItem(Item: GLuint);
begin
  glDisable(GL_TEXTURE_2D);
  glCallList(item);
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.BindTexture(texture: GLuint);
begin
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, texture);
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.DrawTexturedItem(item, texture: GLuint);
begin
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, texture);
  glCallList(item);
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.EndRender;
begin
  if (fglinit) and (fglstartrender) then
  begin
    // swap
    SwapBuffers(fhdc);
    fglstartrender:=false;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.DrawAxis;
begin
  if (fglinit) then
  begin
    glDisable(GL_TEXTURE_2D);
    glBegin(GL_LINES);
    SetColor(faxiscolorx);
    glVertex3f(-faxislength,0,0);
    SetColor(faxiscolorx);
    glVertex3f(AxisLength,0,0);
    SetColor(faxiscolory);
    glVertex3f(0,-faxislength,0);
    SetColor(faxiscolory);
    glVertex3f(0,faxislength,0);
    SetColor(faxiscolorz);
    glVertex3f(0,0,-faxislength);
    SetColor(faxiscolorz);
    glVertex3f(0,0,faxislength);
    glEnd();
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.StartFog(
  mode: integer;
  color: tcolor;
  density: single;
  depth: single;
  enddepth: single);
const
  TFOGMode: array [0..2] of GLuint= (GL_EXP,GL_EXP2,GL_LINEAR);
var
  col: array [0..3] of single;
begin
  if fglinit then
  begin
    col[0]:=(color AND $000000FF) / 255;
    col[1]:=((color AND $0000FF00) DIV 256) / 255;
    col[2]:=((color AND $00FF0000) DIV 65536) / 255;
    col[3]:=1;

    glFogi(GL_FOG_MODE, TFOGmode[Mode]);
    glFogfv(GL_FOG_COLOR, @col);
    glFogf(GL_FOG_DENSITY, density);
    glHint(GL_FOG_HINT, GL_DONT_CARE);
    glFogf(GL_FOG_START, depth);
    glFogf(GL_FOG_END, enddepth);
    glEnable(GL_FOG);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.EndFog;
begin
  if not fglinit then exit;
  glDisable(GL_FOG);					// Enables GL_FOG
end;

//------------------------------------------------------------------------------
procedure TGMDMesh.Draw;
var
  i: glint;
  Face,TexFace: TGLFace;
  TexVertex: TGLVertex;
begin
  //  if Parent.IsOwnTexture=1 then glSetTexture(Parent.OwnTexture); //if own texture was included

  for i:=0 to Parent.MeshFCount-1 do
  begin
    Face:= Faces[i];

    //if texture coord was included
    if Parent.TexFCount>0 then
      TexFace := Parent.TexFaces[i];
    glBegin(GL_TRIANGLES);

    {if Parent.isFaset=1 then   //if faset normals was included
    glNormal3fv(@FasetNormals[Face[0]]);}
    if Parent.isSmooth=1 then    //if smooth normals was included
    glNormal3fv(@SmoothNormals[Face[0]]);
    if Parent.TexFCount>0 then   //if texture coord was included
    begin
      TexVertex := Parent.TexVertices[TexFace[0]];
      glTexCoord2f(TexVertex.x,TexVertex.y);
    end;
    glVertex3fv(@Vertices[Face[0]]);

    {if Parent.isFaset=1 then   //if faset normals was included
    glNormal3fv(@FasetNormals[Face[1]]);}
    if Parent.isSmooth=1 then    //if smooth normals was included
    glNormal3fv(@SmoothNormals[Face[1]]);
    if Parent.TexFCount>0 then   //if texture coord was included
    begin
      TexVertex := Parent.TexVertices[TexFace[1]];
      glTexCoord2f(TexVertex.x,TexVertex.y);
    end;
    glVertex3fv(@Vertices[Face[1]]);

    {if Parent.isFaset=1 then   //if faset normals was included
    glNormal3fv(@FasetNormals[Face[2]]);}
    if Parent.isSmooth=1 then    //if smooth normals was included
    glNormal3fv(@SmoothNormals[Face[2]]);
    if Parent.TexFCount>0 then   //if texture coord was included
    begin
      TexVertex := Parent.TexVertices[TexFace[2]];
      glTexCoord2f(TexVertex.x,TexVertex.y);
    end;
    glVertex3fv(@Vertices[Face[2]]);

    glEnd;
  end;
end;

//------------------------------------------------------------------------------
destructor TGMDMesh.Destroy;
begin
  FreeMem(Vertices, parent.MeshVCount*SizeOf(TGLVertex));
  FreeMem(SmoothNormals,parent.MeshVCount*SizeOf(TGLVertex));
  FreeMem(Faces,        parent.MeshFCount*SizeOf(TGLFace  ));
  FreeMem(FasetNormals, parent.MeshFCount*SizeOf(TGLVector));
end;

//------------------------------------------------------------------------------
function TAPI_GMDModel.LoadFromFile(FileName,ModelName:String):boolean;
var
  a             : dword;
  ogmd_File     : TFileStream;
  ogmd_Ini      : array[0..7]of byte;
  ogmd_Normal,
  ogmd_Vertex   : TGLVertex;
  ogmd_Face     : TGLFace;
  ogmd_NextMesh : TGMDMesh;
  ogmd_FileOK,               //was loaded from file
  ogmd_PackOK   : boolean;   //was loaded from pack
  ogmd_PackPos  : dword;     //position of model in package
  ogmd_pfOK     : boolean;   //file is found in pack
  ogmd_corr     : string;
//  ogmd_texpos   : glint;

        procedure ReadNextMesh(AParent : TAPI_GMDModel);
        var
          i : word;
        begin
          ogmd_NextMesh := TGMDMesh.Create;
          //get memory for current mesh
          GetMem(ogmd_NextMesh.Vertices, MeshVCount*SizeOf(TGLVertex));
          GetMem(ogmd_NextMesh.Faces, MeshFCount*SizeOf(TGLFace  ));
          if isFaset=1 then
            GetMem(ogmd_NextMesh.FasetNormals, MeshFCount*SizeOf(TGLVector));
          if isSmooth=1 then
            GetMem(ogmd_NextMesh.SmoothNormals, MeshVCount*SizeOf(TGLVector));
          //read mesh vertices
          for i := 0 to MeshVCount - 1 do
          begin
            ogmd_File.ReadBuffer(ogmd_Vertex.x,sizeof(glfloat));
            ogmd_File.ReadBuffer(ogmd_Vertex.y,sizeof(glfloat));
            ogmd_File.ReadBuffer(ogmd_Vertex.z,sizeof(glfloat));
            ogmd_NextMesh.Vertices[i] := ogmd_Vertex;
          end;
          //read faces
          for i := 0 to MeshFCount - 1 do
          begin
            ogmd_File.ReadBuffer(ogmd_Face[0],sizeof(word));
            ogmd_File.ReadBuffer(ogmd_Face[1],sizeof(word));
            ogmd_File.ReadBuffer(ogmd_Face[2],sizeof(word));
            ogmd_Face[0] := ogmd_Face[0] - 1;
            ogmd_Face[1] := ogmd_Face[1] - 1;
            ogmd_Face[2] := ogmd_Face[2] - 1;
            ogmd_NextMesh.Faces[i] := ogmd_Face;
          end;
          //faset normals
          if isFaset=1 then
          for i := 0 to MeshFCount - 1 do
          begin
            ogmd_File.ReadBuffer(ogmd_Normal.x,sizeof(glfloat));
            ogmd_File.ReadBuffer(ogmd_Normal.y,sizeof(glfloat));
            ogmd_File.ReadBuffer(ogmd_Normal.z,sizeof(glfloat));
            ogmd_NextMesh.FasetNormals[i] := ogmd_Normal;
          end;
          // Smooth normals
          if isSmooth=1 then
          for i := 0 to MeshVCount - 1 do
          begin
            ogmd_File.ReadBuffer(ogmd_Normal.x,sizeof(glfloat));
            ogmd_File.ReadBuffer(ogmd_Normal.y,sizeof(glfloat));
            ogmd_File.ReadBuffer(ogmd_Normal.z,sizeof(glfloat));
            ogmd_NextMesh.SmoothNormals[i] := ogmd_Normal;
          end;
          ogmd_NextMesh.Parent := AParent;
          Meshes.Add(ogmd_NextMesh);
        end;

        Procedure ReadTextureBlock;
        var
          i : word;
        begin
          if Assigned(TexVertices) then FreeMem(TexVertices);
          if Assigned(TexFaces) then FreeMem(TexFaces);
          GetMem(TexVertices,TexVCount*SizeOf(TGLVertex));
          GetMem(TexFaces,TexFCount*SizeOf(TGLFace));
          //texture vertices
          for i := 0 to TexVCount - 1 do
          begin
            ogmd_File.ReadBuffer(ogmd_Vertex.x,sizeof(glfloat));
            ogmd_File.ReadBuffer(ogmd_Vertex.y,sizeof(glfloat));
            ogmd_File.ReadBuffer(ogmd_Vertex.z,sizeof(glfloat));
            TexVertices[i] := ogmd_Vertex;
          end;
          //texture faces
          for i := 0 to TexFCount - 1 do
          begin
            ogmd_File.ReadBuffer(ogmd_Face[0],sizeof(word));
            ogmd_File.ReadBuffer(ogmd_Face[1],sizeof(word));
            ogmd_File.ReadBuffer(ogmd_Face[2],sizeof(word));
            ogmd_Face[0] := ogmd_Face[0] - 1;
            ogmd_Face[1] := ogmd_Face[1] - 1;
            ogmd_Face[2] := ogmd_Face[2] - 1;
            TexFaces[i] := ogmd_Face;
          end;
        end;

begin
  result:=false;
  if not fileexists(filename)then
    exit;

  // open file
  try
    setfileattributes(pchar(filename), FILE_ATTRIBUTE_ARCHIVE);
    ogmd_File:=TFileStream.Create(FileName,fmOpenRead);
    ogmd_File.Position:=0;
  except
    exit;
  end;

  ogmd_FileOK:=false;
  ogmd_PackOK:=false;
  ogmd_pfOK:=false;

  // file format check
  ogmd_File.ReadBuffer(ogmd_ini,4);
  if char(ogmd_ini[0])+char(ogmd_ini[1])+char(ogmd_ini[2])+char(ogmd_ini[3])='FGMD'then //file initialize
    ogmd_FileOK:=true
  else
    if char(ogmd_ini[0])+char(ogmd_ini[1])+char(ogmd_ini[2])+char(ogmd_ini[3])='PGMD'then  //pack ini
      ogmd_PackOK:=true;
  if not(ogmd_PackOK)and not(ogmd_FileOK) then
  begin
    ogmd_File.Free;
    ogmd_File:=nil;
    exit;
  end;

  //check package for specified model
  if ogmd_PackOK then
  begin
    {correct ModelName for searching}
    ogmd_corr:='        ';
    if length(ModelName)<8 then
    begin
      delete(ogmd_corr,1,length(ModelName));
      ogmd_corr:=lowercase(ModelName+ogmd_corr);
    end else
      ogmd_corr:=lowercase(ModelName);

    {searching for ModelName}
    for a:=0 to ogmd_File.Size-9 do
    begin
      ogmd_File.Position:=a;
      ogmd_File.ReadBuffer(ogmd_ini,8);
      if lowercase(char(ogmd_ini[0])+char(ogmd_ini[1])+char(ogmd_ini[2])+
      char(ogmd_ini[3])+char(ogmd_ini[4])+char(ogmd_ini[5])+
      char(ogmd_ini[6])+char(ogmd_ini[7]))=ogmd_corr then
      begin
        ogmd_File.ReadBuffer(ogmd_PackPos,4);
        ogmd_File.Position:=ogmd_PackPos+4;
        ogmd_pfOK:=true;
        break;
      end;
    end;

    if not ogmd_pfOK then
    begin
      ogmd_File.Free;
      ogmd_File:=nil;
     exit;
    end;
  end;

  ogmd_File.ReadBuffer(MeshCount,2);          //read mesh count
  ogmd_File.ReadBuffer(MeshVCount,2);         //read mesh vertices count
  ogmd_File.ReadBuffer(MeshFCount,2);         //read mesh faces count
  ogmd_File.ReadBuffer(isFaset,1);            //faset normals included
  ogmd_File.ReadBuffer(isSmooth,1);           //smooth normals included
  ogmd_File.ReadBuffer(TexVCount,2);          //read tex vertices count
  ogmd_File.ReadBuffer(TexFCount,2);          //read tex faces count
  ogmd_File.ReadBuffer(IsOwnTexture,1);       //texture file included

  {load meshes crd}
  Meshes := TList.Create;
  for a:=0 to MeshCount-1 do ReadNextMesh(self); //mesh count

  {load texture crd}
  if (TexVCount>0) and (TexFCount>0) then ReadTextureBlock;

  {load texture file}
  //ogmd_texpos:=ogmd_File.position;
  ogmd_File.Free;
  ogmd_File:=nil;
  result:=true;

  if IsOwnTexture=1 then
  begin
    // can only load bmp or jpg textures ---  ?? how about from this file format?
    //result:=oglLoadTexFromGMD(FileName, ogmd_texpos, OwnTexture);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_GMDModel.Draw(StartMesh,EndMesh:word; mps:byte);
var
  Ticks : dword;
begin
  Ticks:=gettickcount;

  if (StartMesh>MeshCount-1) or (EndMesh>MeshCount-1) then
  begin
    EndMesh:=MeshCount-1;
    StartMesh:=EndMesh;
    CurMesh:=StartMesh;                          //no animate
  end;

  if (EndMesh>StartMesh) and (MeshCount>1) then      //do animate
  begin
    if not TimeIsSet then
    begin
      TimeEnd:=Ticks+(1000 div mps);
      TimeIsSet:=true;
    end else
    if Ticks>=TimeEnd then
    begin
      TimeIsSet:=false;
      inc(CurMesh);
      if CurMesh>EndMesh then CurMesh:=StartMesh;
    end;
  end else
    if(StartMesh>=EndMesh)then
      CurMesh:=StartMesh; //no animate

  TGMDMesh(Meshes.Items[CurMesh]).Draw;
end;

//------------------------------------------------------------------------------
procedure TAPI_GMDModel.DrawTextured(StartMesh,EndMesh:word; mps:byte; tex:GLuint);
var
  Ticks : dword;
begin
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, tex);

  Ticks:=gettickcount;
  if (StartMesh>MeshCount-1) or (EndMesh>MeshCount-1) then
  begin
    EndMesh:=MeshCount-1;
    StartMesh:=EndMesh;
    CurMesh:=StartMesh;                          //no animate
  end;

  if (EndMesh>StartMesh) and (MeshCount>1) then      //do animate
  begin
    if not TimeIsSet then
    begin
      TimeEnd:=Ticks+(1000 div mps);
      TimeIsSet:=true;
    end else
    if Ticks>=TimeEnd then
    begin
      TimeIsSet:=false;
      inc(CurMesh);
      if CurMesh>EndMesh then CurMesh:=StartMesh;
    end;
  end else
    if(StartMesh>=EndMesh)then
      CurMesh:=StartMesh; //no animate

  TGMDMesh(Meshes.Items[CurMesh]).Draw;
end;

//------------------------------------------------------------------------------
function TAPI_GMDModel.Make(Mesh: word): boolean;
begin
  result:=false;
  if (mesh>MeshCount-1) then
    exit;
  TGMDMesh(Meshes.Items[mesh]).Draw;
  result:=true;
end;

//------------------------------------------------------------------------------
constructor TAPI_GMDModel.Create;
begin
  CurMesh:= 0;
  MeshCount:=0;
  MeshVCount:=0;
  MeshFCount:=0;
  TexVCount:=0;
  TexFCount:=0;
  IsFaset:=0;
  IsSmooth:=0;
  IsOwnTexture:=0;
end;

//------------------------------------------------------------------------------
destructor TAPI_GMDModel.Destroy;
Var
  i : word;
begin
  for i:=0 to Meshes.Count-1 do
    TGMDMesh(Meshes.Items[i]).Destroy;
  Meshes.Free;
end;

//------------------------------------------------------------------------------
constructor TAPI_OBJModel.create;
begin
  inherited create;
  Initmodel;
end;

//------------------------------------------------------------------------------
destructor TAPI_OBJModel.destroy;
begin
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_OBJModel.InitModel;
begin
  Name :='';
  MaterialFile:='';
  Vertices:=0;
  Normals:=0;
  TexCoords:=0;
  Groups:=0;
  Materials :=0;
  SetLength(Vertex, 0);
  SetLength(Normal, 0);
  SetLength(TexCoord, 0);
  SetLength(Group, 0);
  SetLength(Material, 0);
end;

//------------------------------------------------------------------------------
function TAPI_OBJModel.GetCoords(S : String) : TOBJCoord;
var
  P, P2 : Integer;
  C : TOBJCoord;
begin
  S :=Trim(Copy(S, 3, Length(S)));
  P :=Pos(' ', S);
  P2 :=PosEx(' ', S, P+1);
  S := StringReplace(S, '.', DecimalSeparator, [rfReplaceAll]);

  C.X :=StrToFloat(Copy(S, 1, P-1));
  C.Y :=StrToFloat(Copy(S, P+1, P2-P-1));
  C.Z :=StrToFloat(Copy(S, P2+1, Length(S)));
  Result :=C;
end;

//------------------------------------------------------------------------------
function TAPI_OBJModel.GetTexCoords(S : String) : TOBJTexCoord;
var
  P, P2 : Integer;
  T : TOBJTexCoord;
begin
  P :=Pos(' ', S);
  P2 :=PosEx(' ', S, P+1);
  S := StringReplace(S, '.', DecimalSeparator, [rfReplaceAll]);

  T.U :=StrToFloat(Copy(S, P+1, P2-P-1));
  T.V :=StrToFloat(Copy(S, P2+1, Length(S)));
  Result :=T;
end;

//------------------------------------------------------------------------------
procedure TAPI_OBJModel.ReadVertexData(S : String);
var
  C : TOBJCoord;
  T : TOBJTexCoord;
begin
  case S[2] of
    ' ' : begin                      // Read the vertex coords
            C :=GetCoords(S);
            Inc(Vertices);
            SetLength(Vertex, Vertices+1);
            Vertex[Vertices] :=C;
          end;
    'N' : begin                      // Read the vertex normals
            C :=GetCoords(S);
            Inc(Normals);
            SetLength(Normal, Normals+1);
            Normal[Normals] :=C;
          end;
    'T' : begin                      // Read the vertex texture coords
            T :=GetTexCoords(S);
            Inc(TexCoords);
            SetLength(TexCoord, TexCoords+1);
            TexCoord[TexCoords] :=T;
          end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_OBJModel.ReadFaceData(S : String);
var
  P, P2, P3 : Integer;
  F : TOBJFace;
begin
  P :=Pos(' ', S);
  S :=Trim(Copy(S, P+1, length(S)));

  Inc(Group[Groups].Faces);
  SetLength(Group[Groups].Face, Group[Groups].Faces+1);

  F.Count :=0;
  While Length(S) > 0 do
  begin
    P :=Pos('/', S);      // check for position of first /
    P3 :=Pos(' ', S);
    if P3 = 0 then      // if we reach the end
      P3 :=Length(S)+1;

    if P > 0 then              // there are normals or texture coords
    begin
      Inc(F.Count);
      SetLength(F.vIndex, F.Count);
      F.vIndex[F.Count-1] :=StrToInt(Copy(S, 1, P-1));
      P2 :=PosEx('/', S, P+1);   // check for position of second /
      if P2 > P+1 then          // there are normals AND texture coords
      begin
        SetLength(F.tIndex, F.Count);
        SetLength(F.nIndex, F.Count);
        { Change Suggested By Megaes }
        F.tIndex[F.Count-1] :=StrToInt(Copy(S, P+1, P2-P-1));
        F.nIndex[F.Count-1] :=StrToInt(Copy(S, P2+1, P3-P2-1));
        //F.tIndex[F.Count-1] :=StrToInt(Copy(S, P+1, P2-1));
        //F.nIndex[F.Count-1] :=StrToInt(Copy(S, P2+1, P3-1));
      end
      else
      begin
        SetLength(F.nIndex, F.Count);
        F.nIndex[F.Count-1] :=StrToInt(Copy(S, P2+1, P3-1 - P2));
      end;
    end
    else
    begin
      Inc(F.Count);
      SetLength(F.vIndex, F.Count);
      F.vIndex[F.Count-1] :=StrToInt(Copy(S, 1, P3-1));
    end;
    S :=Copy(S, P3+1, length(S));
  end;

  Group[Groups].Face[Group[Groups].Faces] :=F;
end;

//------------------------------------------------------------------------------
procedure TAPI_OBJModel.GetMaterialName(S : String);
var I, P : Integer;
begin
//  if copy(S, 1, 6) <> 'USEMTL' then exit;  // false call
  P :=Pos(' ', S);
  S :=Copy(S, P+1, length(S));
  For I :=1 to Materials do
    if Material[I].Name = S then
      Group[Groups].mIndex :=I;
end;

//------------------------------------------------------------------------------
procedure TAPI_OBJModel.CreateMaterial(S : String);
begin
//  if trim(Copy(S, 1, 6) <> 'NEWMTL') then exit;
  Inc(Materials);
  SetLength(Material, Materials+1);
  S :=Trim(Copy(S, 7, length(S)));
  FillChar(Material[Materials].Ambient, 0, Sizeof(Material[Materials].Ambient));
  FillChar(Material[Materials].Diffuse, 0, Sizeof(Material[Materials].Diffuse));
  FillChar(Material[Materials].Specular, 0, Sizeof(Material[Materials].Specular));
  Material[Materials].Shininess :=60;
  Material[Materials].Texture :=0;
  Material[Materials].Name :=S;
end;

//------------------------------------------------------------------------------
procedure TAPI_OBJModel.GetMaterial(S : String);
var C : TOBJColor;
    P, P2 : Integer;
    Ch : Char;
begin
  Ch :=S[2];
  S :=Trim(Copy(S, 3, Length(S)));
  P :=Pos(' ', S);
  P2 :=PosEx(' ', S, P+1);
  S := StringReplace(S, '.', DecimalSeparator, [rfReplaceAll]);

  C.R :=StrToFloat(Copy(S, 1, P-1));
  C.G :=StrToFloat(Copy(S, P+1, P2-P-1));
  C.B :=StrToFloat(Copy(S, P2+1, Length(S)));

  case CH of
    'A' : Material[Materials].Ambient :=C;
    'D' : Material[Materials].Diffuse :=C;
    'S' : Material[Materials].Specular :=C;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_OBJModel.GetShininess(S : String);
begin
  S :=Trim(Copy(S, 3, Length(S)));
  S := StringReplace(S, '.', DecimalSeparator, [rfReplaceAll]);

  Material[Materials].Shininess :=StrToFloat(S);
end;

//------------------------------------------------------------------------------
procedure TAPI_OBJModel.GetTexture(S : String);
begin
  // texturename = get the name from "map_Kd textures/fabric1.rgb"
  // LoadTexture( texturename, M.Material[M.Materials].Texture);
end;

//------------------------------------------------------------------------------
procedure TAPI_OBJModel.LoadMaterials(S : String);
var P : Integer;
    filename : String;
    F : TextFile;
begin
//  if copy(S, 1, 6) <> 'MTLLIB' then exit;  // false call

  P :=Pos(' ', S);
  filename :=Copy(S, P+1, length(S));

  if FileExists(filename) then
  begin
    AssignFile(F, filename);
    Reset(F);
    while not(EOF(F)) do
    begin
      Readln(F, S);
      if (S <> '') AND (S[1] <> '#') then
      begin
        S :=Uppercase(S);
        Case S[1] of
          'N' : begin
                  if S[2] = 'S' then GetShininess(S);
                  if S[2] = 'E' then CreateMaterial(S);
                end;
          'K' : GetMaterial(S);
          'M' : GetTexture(S);
        end;
      end;
    end;
    closeFile(F);
  end;

end;

//------------------------------------------------------------------------------
function TAPI_OBJModel.LoadFromFile(filename : String): boolean;
var
  F : TextFile;
  S, S2 : String;
  P : Integer;
  objdir: string;
  curdir: string;
begin
  result:=false;

  curdir:=getcurrentdir;
  objdir:=extractfiledir(filename);

  if FileExists(filename) then
  begin
    chdir(objdir);
    Initmodel;

    P :=Pos('.', filename)-1;
    if P < 1 then P :=Length(filename);
    Name :=Copy(filename, 1, P);

    AssignFile(F, filename);
    Reset(F);

    while not(EOF(F)) do
    begin
      Readln(F, S);
      if (S <> '') AND (S[1] <> '#') then
      begin
        S :=Uppercase(S);
        case S[1] of
          'G' : begin
                  Inc(Groups);
                  SetLength(Group, Groups+1);
                  S2 :=Trim(Copy(S, 2, length(S)));
                  Group[Groups].Name :=S2;
                end;
          'V' : ReadVertexData(S);
          'F' : ReadFaceData(S);
          'U' : GetMaterialName(S);
          'M' : LoadMaterials(S);
        end;
      end;
    end;

    Closefile(F);
    chdir(curdir);
    result:=true;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_OBJModel.DrawGroup(i: integer; owntexture: boolean): boolean;
var
  J: integer;
  K: Integer;
begin
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @Material[Group[I].mIndex].Diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @Material[Group[I].mIndex].Specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @Material[Group[I].mIndex].Ambient);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, @Material[Group[I].mIndex].Shininess);

  if not owntexture then
  begin
    if Material[Group[I].mIndex].Texture <> 0 then
    begin
      glEnable(GL_TEXTURE_2D);
      glBindTexture(GL_TEXTURE_2D, Material[Group[I].mIndex].Texture);
    end else
      glDisable(GL_TEXTURE_2D);
  end;

  For J :=1 to Group[I].Faces do
  begin
    with Group[I].Face[J] do
    begin
      case Count of
        3 : glBegin(GL_TRIANGLES);
        4 : glBegin(GL_QUADS);
      else
        glBegin(GL_POLYGON);
      end;
      for K :=0 to Count-1 do
      begin
        if Normals > 0 then

          glNormal3fv( @Normal[nIndex[K]] );
        if TexCoords > 0 then
          glTexCoord2fv( @TexCoord[tIndex[K]] );
        glVertex3fv( @Vertex[vIndex[K]] );
      end;
      glEnd();
    end; // face
  end; // faces
  result:= true;
end;

//------------------------------------------------------------------------------
function TAPI_OBJModel.Draw: boolean;
var
  I: integer;
begin
  For I :=1 to Groups do
    drawgroup(i, false);
  result:= true;
end;

//------------------------------------------------------------------------------
function TAPI_OBJModel.DrawTextured(tex: GLuint): boolean;
var
  i: integer;
begin
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, tex);
  for i:=1 to groups do
    drawgroup(i,true);
  result:= true;
end;

//------------------------------------------------------------------------------
function TAPI_OBJModel.Make: boolean;
var
  I: integer;
begin
  For I :=1 to Groups do
    drawgroup(i, true);
  result:= true;
end;

//------------------------------------------------------------------------------
constructor TAPI_SMPLModel.create;
begin
  inherited create;
  initmodel;
end;

//------------------------------------------------------------------------------
procedure TAPI_SMPLmodel.Initmodel;
begin
  triangles:=0;
  objs:=0;
  setlength(obj,0);
end;

//------------------------------------------------------------------------------
destructor TAPI_SMPLModel.destroy;
begin
  initmodel;
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_SMPLModel.LoadFromFile(Filename: string): boolean;
var
  f: textfile;
  s,s1,s2: string;
  p: integer;

    procedure parsetonums (s: string; var sl: tstringlist);
    var
      i: integer;
      s1: string;
    begin
      sl.clear;
      s1:='';
      for i:=1 to length(s) do
      begin
        {$ifdef DELPHI2009UP}
        if (charinset(s[i], ['0'..'9',decimalseparator,'+','-'])) then
        {$else}
        if (s[i] in ['0'..'9',decimalseparator,'+','-']) then
        {$endif}
        begin
          s1:=s1+s[i];
        end else
        if s1<>'' then
        begin
          sl.Add(s1);
          s1:='';
        end;
      end;
    end;

    function getSMPLcolor (s: string): tSMPLcolor;
    var
      s1: string;
      list: tstringlist;
    begin
      s1:=s;
      s1:=stringreplace(s1,'.',decimalSeparator, [rfReplaceAll]);
      list:=tstringlist.create;
      parsetonums( s1, list );
      if list.count>2 then
      begin
        result.r:=strtofloat(list[0]);
        result.g:=strtofloat(list[1]);
        result.b:=strtofloat(list[2]);
      end;
      list.free;
    end;

    function getSMPLtriangle (s: string): tSMPLtriangle;
    var
      s1: string;
      list: tstringlist;
    begin
      s1:=s;
      s1:=stringreplace(s1,'.',decimalSeparator, [rfReplaceAll]);
      list:=tstringlist.create;
      parsetonums( s1, list );
      if list.count>8 then
      begin
        result.x0:=strtofloat(list[0]);
        result.y0:=strtofloat(list[1]);
        result.z0:=strtofloat(list[2]);
        result.x1:=strtofloat(list[3]);
        result.y1:=strtofloat(list[4]);
        result.z1:=strtofloat(list[5]);
        result.x2:=strtofloat(list[6]);
        result.y2:=strtofloat(list[7]);
        result.z2:=strtofloat(list[8]);
      end;
      list.free;
    end;

begin
  result:=false;
  if fileexists(filename) then
  begin
    {$i-}
    assignfile(f,filename);
    reset(f);
    {$i+}
    if ioresult<>0 then exit;

    while not eof(f) do
    begin
      readln(f,s);
      s:=uppercase(s);

      // get command
      p:=pos(' ',s);
      if p>0 then
      begin
        s1:=trim(copy(s,1,p));
        s2:=trim(copy(s,p+1,length(s)-(p+1)));
      end else
      begin
        s1:='';
        s2:='';
      end;

      if s1='OBJECT' then
      begin
        // start new object
        objs:=objs+1;
        setlength(obj,objs);
      end else

      if (objs>0) then
      begin
        // parse data line
        if (s1='COLOR') and (s2<>'') then
        begin
          // get color
          obj[objs-1].color:=getSMPLcolor(s2);
        end else

        if (s1='TRIANGLE') and (s2<>'') then
        begin
          // get triangle
          triangles:=triangles+1;
          obj[objs-1].triangles:=obj[objs-1].triangles+1;
          setlength(obj[objs-1].triangle, obj[objs-1].triangles);
          obj[objs-1].triangle[obj[objs-1].triangles-1]:=getSMPLtriangle(s2);
        end;
      end;

    end;

    closefile(f);
    result:=true;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_SMPLmodel.Make: boolean;
var
  i,j: integer;
begin
  for i:=0 to objs-1 do
  begin
    with obj[i].color do
      glColor3f(r,g,b);
    for j:=0 to obj[i].triangles-1 do
    begin
      with obj[i].triangle[j] do
      begin
        glBegin(GL_TRIANGLES);

          glNormal3f(x0,y0,z0);
          glTexCoord3f(x0,y0,z0);
          glvertex3f(x0,y0,z0);

          glNormal3f(x1,y1,z1);
          glTexCoord3f(x1,y1,z1);
          glvertex3f(x1,y1,z1);

          glNormal3f(x2,y2,z2);
          glTexCoord3f(x2,y2,z2);
          glvertex3f(x2,y2,z2);

        glEnd;
      end;
    end;
  end;
  result:= true;
end;

//------------------------------------------------------------------------------
function TAPI_SMPLModel.Draw: boolean;
begin
  result:=make;
end;

//------------------------------------------------------------------------------
function TAPI_SMPLModel.DrawTextured(tex: GLuint): boolean;
begin
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, tex);
  result:=make;
end;

//------------------------------------------------------------------------------
procedure TAPI_glpanel.setthreadactive(b:boolean);
begin
  if csdesigning in componentstate then exit;

  if (b) and (not fthreadactive) then
  begin
    fthread:=tapi_gldrawthread.Create(true);
    fthread.fdelay:=fthreaddelay;
    fthread.fevent:=fthreadevent;
    fthread.Priority:=fthreadpriority;
    fthread.Resume;
    fthreadactive:=true;
  end;
  if (not b) and (fthreadactive) then
  begin
    fthread.Terminate;
    fthreadactive:=false;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_gldrawthread.execute;
begin
  factive:=true;
  while not terminated do
  begin
    sleep(fdelay);
    if assigned(fevent) then
      synchronize(fevent);
  end;
  factive:=false;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_glpanel]);
end;

end.
