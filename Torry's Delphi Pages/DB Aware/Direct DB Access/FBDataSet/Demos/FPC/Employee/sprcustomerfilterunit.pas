unit sprCustomerFilterUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TsprCustomerFilterForm }

  TsprCustomerFilterForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  sprCustomerFilterForm: TsprCustomerFilterForm;

implementation

initialization
  {$I sprcustomerfilterunit.lrs}

end.

