unit API_sync;

interface

//------------------------------------------------------------------------------
// API_sync
//------------------------------------------------------------------------------
// r1.00, 13082007, ari pikivirta
//  * changed unit to class for easier interface
//  * supports both critical object and multi read exclusive write synchronizer
//    methods
//------------------------------------------------------------------------------

uses
  SysUtils, Classes;

type
  TAPI_sync = class(TComponent)
  private
    { Private declarations }
    fversion: string;
    flock: tmultireadexclusivewritesynchronizer;
    procedure dummys(s: string);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: tcomponent); override;
    destructor  Destroy; override;
    procedure Enter;
    procedure Leave;
    procedure BeginRead;
    procedure EndRead;
    function  BeginWrite: boolean;
    procedure EndWrite;
  published
    { Published declarations }
    property Version: string read fversion write dummys stored false;
  end;

procedure Register;

implementation

{$R *.RES}

//------------------------------------------------------------------------------
constructor TAPI_sync.Create(AOwner: tcomponent);
begin
  inherited Create(AOwner);
  fversion:= 'r1.00/ari.pikivirta@kolumbus.fi';
  flock:= tmultireadexclusivewritesynchronizer.create;
end;

//------------------------------------------------------------------------------
destructor TAPI_sync.Destroy;
begin
  flock.free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_sync.dummys(s: string);
begin
  // does nothing..
end;

//------------------------------------------------------------------------------
procedure TAPI_sync.Enter;
begin
  BeginWrite;
end;

procedure TAPI_sync.Leave;
begin
  EndWrite;
end;

procedure TAPI_sync.BeginRead;
begin
  flock.BeginRead;
end;

procedure TAPI_sync.EndRead;
begin
  flock.EndRead;
end;

function TAPI_sync.BeginWrite: boolean;
begin
  result:= flock.BeginWrite;
end;

procedure TAPI_sync.EndWrite;
begin
  flock.EndWrite;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_sync]);
end;

end.
