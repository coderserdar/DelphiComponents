unit About;

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutForm = class(TForm)
    BUOk: TButton;
    LBProductName: TLabel;
    LBCopyright: TLabel;
    LBEmail: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$IFDEF FPC}
initialization
  {$i about.lrs}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.
 
