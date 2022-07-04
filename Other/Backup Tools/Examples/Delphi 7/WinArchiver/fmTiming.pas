unit fmTiming;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TTiming = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    lElapsed: TLabel;
    lRemaining: TLabel;
    Bevel1: TBevel;
    procedure FormShow(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Timing: TTiming;

implementation

{$R *.DFM}

procedure TTiming.FormShow(Sender: TObject);
begin
  lElapsed.Caption := '00:00:00';
  lRemaining.Caption := '00:00:00';
end;

end.
