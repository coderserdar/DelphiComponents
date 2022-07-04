unit ExtHilitReg;

interface
{$IFDEF VER140} {$DEFINE D6New} {$ENDIF}
{$IFDEF VER150} {$DEFINE D6New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D6New} {$ENDIF}

uses {$IFDEF D6New} DesignEditors, DesignIntf; {$ELSE} DsgnIntf; {$ENDIF}

type TExtKeywordsProperty = class(TStringProperty)
                             public
                             function GetAttributes: TPropertyAttributes; override;
                             function GetValue: string; override;
                             procedure Edit; override;
                             end;

     TExtStartStopProperty = class(TStringProperty)
                             public
                             function GetAttributes: TPropertyAttributes; override;
                             function GetValue: string; override;
                             procedure Edit; override;
                             end;

procedure Register;

implementation
uses Classes, ExtHilit, PlusKeys;

procedure Register;
begin
  RegisterComponents({UCONVERT}'PlusMemo'{/UCONVERT}, [TExtHighlighter]);
  RegisterPropertyEditor(TypeInfo(TExtKeywordList), TExtHighlighter, 'Keywords', TExtKeywordsProperty);
  RegisterPropertyEditor(TypeInfo(TExtStartStopList), TExtHighlighter, 'StartStopKeys', TExtStartStopProperty);
end;

function TExtKeywordsProperty.GetAttributes;
begin
  Result:= [paDialog, paAutoUpdate]
end;

function TExtKeywordsProperty.GetValue: string;
begin
  Result:= '(TExtKeywordList)'
end;

procedure TExtKeywordsProperty.Edit;
var shilit: TCustomExtHighlighter;
begin
  shilit:= GetComponent(0) as TCustomExtHighlighter;
  if EditKeywordList(TExtHighlighter(shilit).Keywords, True, True) then
    begin
      shilit.ReApplyKeys;
      Modified
    end
end;

function TExtStartStopProperty.GetAttributes;
begin
  Result:= [paDialog, paAutoUpdate]
end;

function TExtStartStopProperty.GetValue: string;
begin
  Result:= '(TExtStartStopKeyList)'
end;

procedure TExtStartStopProperty.Edit;
var shilit: TCustomExtHighlighter;
begin
  shilit:= GetComponent(0) as TCustomExtHighlighter;
  if EditStartStopList(TExtHighlighter(shilit).StartStopKeys, True) then
    begin
      shilit.ReApplyKeys;
      Modified
    end
end;



end.
