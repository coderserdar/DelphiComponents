unit LeDsgnReg;

interface

uses
{$IFDEF VER140}  { Delphi 6 }
  DesignIntf,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  LblEffct, LeColrEd, LeCompEd, LeEfctEd, LeGradEd,
  LeReszEd, LeStylEd;

procedure Register;

implementation

{ Define the property editors and component editor to Delphi }
procedure Register;
begin
  { Only applies to properties of type TEffectStyle in TLabelEffect component }
  RegisterPropertyEditor(TypeInfo(TEffectStyle), TLabelEffect, '',
    TEffectStylePropertyEditor);
  { Only applies to properties of type TColourScheme in TLabelEffect component }
  RegisterPropertyEditor(TypeInfo(TColourScheme), TLabelEffect, '',
    TColourSchemePropertyEditor);
  { Only applies to properties of type TEffectOption in TLabelEffect component }
  RegisterPropertyEditor(TypeInfo(TEffectOption), TLabelEffect, '',
    TEffectOptionPropertyEditor);
  { Only applies to properties of type TGraduateOption in TLabelEffect component }
  RegisterPropertyEditor(TypeInfo(TGraduateOption), TLabelEffect, '',
    TGraduateOptionPropertyEditor);
  { Only applies to properties of type TResizeOption in TLabelEffect component }
  RegisterPropertyEditor(TypeInfo(TResizeOption), TLabelEffect, '',
    TResizeOptionPropertyEditor);
  { Whole of component editor }
  RegisterComponentEditor(TLabelEffect, TLabelEffectCompEditor);
end;

end.
