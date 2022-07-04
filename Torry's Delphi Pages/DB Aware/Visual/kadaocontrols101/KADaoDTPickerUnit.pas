unit KADaoDTPickerUnit;
{$I KADaoControlsCommonDirectives.pas}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons;

type
  TDTPicker = class(TForm)
    DTP: TDateTimePicker;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
    Function Execute(Var DT : TDateTime; Kind:TDateTimeKind):Boolean;
  end;

var
  DTPicker: TDTPicker;

implementation

{$R *.DFM}

Function TDTPicker.Execute(Var DT : TDateTime; Kind:TDateTimeKind):Boolean;
Begin
  Result := False;
  BitBtn1.Caption:='';
  BitBtn2.Caption:='';
  DTP.Kind := Kind;
  ShowModal;
  if ModalResult=mrOK Then
     Begin
       Result := True;
       if Kind=dtkDate Then DT := DTP.Date Else DT := DTP.Time;
     End;
End;

end.
