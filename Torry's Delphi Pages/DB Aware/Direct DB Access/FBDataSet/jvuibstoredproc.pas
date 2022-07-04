unit jvuibstoredproc;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils
{$ifdef FPC}
, LResources
{$endif}
, Forms, Controls, Graphics, Dialogs, jvuib;

type

  { TJvUIBStoredProc }

  TJvUIBStoredProc = class(TJvUIBQuery)
  private
    FSelectProc: boolean;
    FStoredProc: string;
    procedure SetSelectProc(const AValue: boolean);
    procedure SetStoredProc(const AValue: string);
    { Private declarations }
  protected
    { Protected declarations }
  public
    procedure ExecProc;
  published
    property SelectProc: boolean read FSelectProc write SetSelectProc;
    property StoredProcName: string read FStoredProc write SetStoredProc;
  end;

implementation

{ TJvUIBStoredProc }

procedure TJvUIBStoredProc.SetSelectProc(const AValue: boolean);
begin
  if FSelectProc=AValue then exit;
  FSelectProc:=AValue;
end;

procedure TJvUIBStoredProc.SetStoredProc(const AValue: string);
begin
  if FStoredProc=AValue then exit;
  FStoredProc:=AValue;
end;

procedure TJvUIBStoredProc.ExecProc;
begin
  ExecSQL;
end;

end.
