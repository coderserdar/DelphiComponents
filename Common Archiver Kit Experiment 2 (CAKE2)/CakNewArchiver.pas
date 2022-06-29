unit CakNewArchiver;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver;

const CanExtract = FALSE;
      CanAdd = FALSE;
      CanList = FALSE;
      CanSFX = FALSE;
      Dllname = 'dummydll.dll';
type
  TCakNewArchiver = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils, Cakdir2;
var Cakdir : TCakdir2;
procedure TCakNewArchiver.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakNewArchiver.DoStop(Stopp: Boolean);
begin
end;

procedure TCakNewArchiver.Process(dowhat : WorkType);
begin
    Load_DLL;
    {Code}
end;

function TCakNewArchiver.DllExists : boolean;
begin
        Result := Fileexists(GrabProgrampath+Dllname);
end;

function TCakNewArchiver.Cando(aWork: WorkType): Boolean;
begin
        Case aWork of
        wtTest, wtExtract : Result := CanExtract;
        wtAdd, wtDelete : Result := CanAdd;
        wtSFX :  Result := CanSFX;
        wtLoadContents: Result := CanList;
        else Result := false;
        end;
        if Result then
          if not DLLExists then
            Result := false;
end;

function TCakNewArchiver.Def_TreatAs : string;
begin
  result := SupportType;
end;

procedure TCakNewArchiver.Load_DLL;
begin
end;

procedure TCakNewArchiver.UnLoad_DLL;
begin
end;

procedure Register;
begin
  //RegisterComponents('QZip', [TCakNewArchiver]);
end;

end.
 