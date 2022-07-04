{******************************************************************************}
{                                                                              }
{   Komponen z oknem klawiatura                                                }
{                                                                              }
{                                                                              }
{   Autor: Tomasz Bojara                                                       }
{                                                                              }
{******************************************************************************}

unit Klawiatura;

interface

uses
  SysUtils, Classes, Forms, StdCtrls, Controls;

type
  TKlawiatura = class(TComponent)
  private
    { Private declarations }
    fKeyForm: TForm;
    fEdit: TCustomEdit;
    procedure SetEdit(const Value: TCustomEdit);
    function GetKeyForm: TForm;
  protected
    { Protected declarations }
    procedure DoFormClose(Sender: TObject; var Action: TCloseAction);
  public
    { Public declarations }
    destructor Destroy; override;
    procedure FormCreate;
    procedure FormDestroy;
    procedure FormShow;
    procedure FormClose;
    property KeyForm: TForm read GetKeyForm;
  published
    { Published declarations }
    property Edit: TCustomEdit read fEdit write SetEdit;
  end;

implementation
uses F_klaw;

{ TKlawiatura }

destructor TKlawiatura.Destroy;
begin
  if Assigned(fKeyForm) then FormDestroy;
  inherited;
end;

procedure TKlawiatura.DoFormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
  fKeyForm:= nil;
end;

function TKlawiatura.GetKeyForm: TForm;
begin
  if Assigned(fKeyForm) then
    Result:= fKeyForm
  else
    Result:= nil;
end;

procedure TKlawiatura.SetEdit(const Value: TCustomEdit);
begin
  fEdit := Value;
  if Assigned(fKeyForm) then TfmKlaw(fKeyForm).SetEdit(fEdit);
end;

procedure TKlawiatura.FormClose;
begin
  if Assigned(fKeyForm) then fKeyForm.Close;
end;

procedure TKlawiatura.FormCreate;
begin
  fKeyForm:= TfmKlaw.Create(Self);
  fKeyForm.OnClose:= DoFormClose;
end;

procedure TKlawiatura.FormDestroy;
begin
  FormClose;
  fKeyForm.Free;
  fKeyForm:= nil;
end;

procedure TKlawiatura.FormShow;
begin
  if not Assigned(fKeyForm) then FormCreate;
  TfmKlaw(fKeyForm).SetEdit(fEdit);
  fKeyForm.Show;
end;

end.
