unit EmailHilitReg;

{$IFDEF VER140} {$DEFINE D6New} {$ENDIF}
{$IFDEF VER150} {$DEFINE D6New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D6New} {$ENDIF}
interface

procedure Register;

implementation

uses Classes, EmailHilit, ExtHilit, ExtHilitReg,
  {$IFDEF D6New} DesignEditors, DesignIntf; {$ELSE} DsgnIntf; {$ENDIF}

procedure Register;
begin
  RegisterComponents({UCONVERT}'PlusMemo'{/UCONVERT}, [TEmailHighlighter]);
  RegisterPropertyEditor(TypeInfo(TExtKeywordList), TEmailHighlighter, 'Keywords', TExtKeywordsProperty);
  RegisterPropertyEditor(TypeInfo(TExtStartStopList), TEmailHighlighter, 'StartStopKeys', TExtStartStopProperty);
end;

end.

