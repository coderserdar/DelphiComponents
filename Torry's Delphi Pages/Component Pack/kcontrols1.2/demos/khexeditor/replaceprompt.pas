unit ReplacePrompt;

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
  TReplacePromptForm = class(TForm)
    LBText: TLabel;
    BUYes: TButton;
    BuNo: TButton;
    BUAll: TButton;
    BUCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ReplacePromptForm: TReplacePromptForm;

implementation

{$IFDEF FPC}
initialization
  {$i replaceprompt.lrs}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.
