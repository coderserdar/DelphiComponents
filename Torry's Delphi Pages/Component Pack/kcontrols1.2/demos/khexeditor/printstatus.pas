unit PrintStatus;

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TPrintStatusForm = class(TForm)
    BUAbort: TButton;
    LBPage: TLabel;
    procedure BUAbortClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Aborted: Boolean;
  end;

var
  PrintStatusForm: TPrintStatusForm;

implementation

procedure TPrintStatusForm.BUAbortClick(Sender: TObject);
begin
  Aborted := True;
end;

{$IFDEF FPC}
initialization
  {$i printstatus.lrs}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.
