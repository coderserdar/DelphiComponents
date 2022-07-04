unit API_processlimit;

//------------------------------------------------------------------------------
// checks if previous instances exists
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
// r1.02, 13062008, ari pikivirta
//  * added activatepreviousinstance function
//
// r1.01, ari pikivirta
//  * just updated the revision info email
//
//------------------------------------------------------------------------------
// just to inform, nowadays, when there is multiuser environments even
// in real use - creating mutex or using atom is not enough for making
// sure that there is no other instances running. in the case of multiuser
// environment, previous instance check should be performed via lock file
// that the application creates on start and destroys on close (this works
// on all operating systems by the way..).
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Classes, WinProcs, WinTypes;

type
  TAPI_processlimit = class(tcomponent)
  private
    fversion: string;
    fmutexname: string;
    fMutex: thandle;
    fexists: boolean;
    fonexists: tnotifyevent;
    procedure dummys(s: string);
    procedure dummyb(b: boolean);
    function  MutexExists(const MutexName: string): boolean;

  protected
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Version: string read fversion write dummys stored false;
    property Instance: string read fmutexname write fmutexname;
    property Exists: boolean read fexists write dummyb stored false;
    property OnExists: tnotifyevent read fonexists write fonexists;

  end;

procedure Register;

implementation

{$R *.RES}

const
  versioninfo = 'r1.02/ari.pikivirta@kolumbus.fi';

type
  PHWnd = ^HWnd;

procedure TAPI_processlimit.dummys(s: string); begin end;
procedure TAPI_processlimit.dummyb(b: boolean); begin end;

//-----------------------------------------------------------------
constructor tAPI_processlimit.create(AOwner: TComponent);
begin
  inherited create(aowner);
  fversion:= versioninfo;
  fmutexname:= 'SingleInstance';
end;

//-----------------------------------------------------------------
procedure TAPI_processlimit.Loaded;
begin
  // check if previous instance exists
  // if not designing
  if (not (csdesigning in componentstate)) then
  begin
    // create mutex handle if it doesn't exist
    fexists:= MutexExists(fmutexname);
    // check result for the event at startup
    if fexists then
      // if event assigned..
      if assigned(fonexists) then
        // ..we'll do the event
        fonexists(self);
  end;
end;

//-----------------------------------------------------------------
function TAPI_processlimit.MutexExists(const MutexName: string): boolean;
var
  Mutex : THandle;
begin
  // create mutex with the name defined at
  // the component properties
  Mutex := CreateMutex(nil, True, pchar(MutexName));
  if (Mutex = 0) OR (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    // same named mutex already existed
    result:= TRUE;
  end else
    // we just created the mutex succesfully,
    // so there wasn't any other instances
    result:= FALSE;
end;

//-----------------------------------------------------------------
destructor tAPI_processlimit.destroy;
begin
  // release and close mutex handle
  if (fmutex<>0) then
  begin
    ReleaseMutex(fMutex);
    CloseHandle(fMutex);
  end;
  // destroy and free stuff
  inherited destroy;
end;

//-----------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_processlimit]);
end;

end.
