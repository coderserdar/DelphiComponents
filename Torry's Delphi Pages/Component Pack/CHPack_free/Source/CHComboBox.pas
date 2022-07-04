unit CHComboBox;

interface

uses
  SysUtils, Classes, Controls, StdCtrls;

type
  TCHComboBox = class(TCustomComboBox)
  private
    { Private-Deklarationen }
  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
  published
    { Published-Deklarationen }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHComboBox]);
end;

end.
