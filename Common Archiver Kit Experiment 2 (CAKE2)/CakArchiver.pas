unit CakArchiver;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2;

const CanExtract = FALSE;
      CanAdd = FALSE;
      CanList = TRUE;
      CanSFX = FALSE;
      Dllname = 'dummydll.dll';
type
  TCakArchiver = class(TComponent)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure SetCakdir(pCakdir : TComponent); virtual;
    procedure Process(dowhat : WorkType); virtual;
    function Cando(aWork: WorkType): Boolean; virtual;
    function Def_TreatAs : string; virtual;
    procedure Load_DLL; virtual;
    procedure UnLoad_DLL; virtual;
    function DllExists : Boolean; virtual;
    procedure DoStop(Stopp: Boolean); virtual;
    function GetComment : string; dynamic;
    procedure SetComment(text : string); dynamic;
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils, Cakdir2;
var Cakdir : TCakdir2;
procedure TCakArchiver.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakArchiver.DoStop(Stopp: Boolean);
begin
end;

procedure TCakArchiver.Process(dowhat : WorkType);
begin
    Load_DLL;
    {Code}
end;

function TCakArchiver.DllExists : boolean;
begin
        Result := True;
end;

function TCakArchiver.Cando(aWork: WorkType): Boolean;
begin
        Case aWork of
        wtNone : Result := true;
        wtExtract : Result := CanExtract;
        wtAdd, wtDelete : Result := CanAdd;
        wtSFX :  Result := CanSFX;
        wtLoadContents: Result := CanList;
        else Result := false;
        end;
        if Result then
          if not DLLExists then
            Result := false;
end;

function TCakArchiver.Def_TreatAs : string;
begin
end;

procedure TCakArchiver.Load_DLL;
begin
end;

procedure TCakArchiver.UnLoad_DLL;
begin
end;

function TCakArchiver.GetComment : string;
begin
  Result := '';
end;

procedure TCakArchiver.SetComment(text : string);
begin
  
end;

procedure Register;
begin
  //RegisterComponents('QZip', [TCakArchiver]);
end;

end.
 