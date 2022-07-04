unit Input;

{$include KControls.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls
{$IFDEF FPC}
  , LResources
{$ENDIF}
  ;

type
  TInputForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  InputForm: TInputForm;

implementation

uses Main;

procedure TInputForm.FormCreate(Sender: TObject);
begin
  Caption := Form1.Caption;
end;

{$IFDEF FPC}
initialization
  {$i Input.lrs}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.
