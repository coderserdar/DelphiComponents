unit ZRReg;

interface

{$I ZRDefine.inc}

procedure Register;

implementation

uses
{$IFDEF D6Above}
  DesignIntf,
  DesignEditors,
  VCLEditors,
  RTLConsts,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  Classes, Graphics,  Controls,
  ZReport, ZRCtrls, ZREscape, ZRPrprty, ZRCapton,
  agDialog;

procedure Register;
begin
  RegisterClasses([TZRGroup, TZRBand, TZRSubDetail]);
  RegisterNoIcon ([TZRGroup, TZRBand, TZRSubDetail]);

  RegisterClasses([TZRField, TZRExpression, TZRAggregator]);
  RegisterNoIcon ([TZRField, TZRExpression, TZRAggregator]);

  RegisterComponents('ZReport', [TZReport, TZRComposite]);
  RegisterComponents('ZReport', [TZRLabel, TZRSystemLabel, TZRTotalLabel, TZRFrameLine]);

  RegisterPropertyEditor(TypeInfo(TFont), TZReport    , 'Font', TZRFontProperty);
  RegisterPropertyEditor(TypeInfo(TFont), TZRComposite, 'Font', TZRFontProperty);
  RegisterPropertyEditor(TypeInfo(String), TZREscapes, '', TZREscapeProperty);

  RegisterPropertyEditor(TypeInfo(String), TZRField, 'DataField', TZRDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(TZRVariable), nil, 'Variable' , TZRVariableProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TZRTotalLabel, 'Level', TZRLevelProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TZRCustomLabel, 'Caption', TZRCaptionProperty);

  RegisterComponentEditor(TZRCustomBand, TZReportEditor);
end;

end.

