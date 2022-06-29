unit PMemoReg;

{ PlusMemoClx version 6.1
{ © Electro-Concept Mauricie, 2003-2004

  Source file for registration of PlusMemo component on the palette at design time;
}
{$DEFINE PMemoReg}
{UCONVERT}
  {$IFDEF PMemoRegClx}
    {$DEFINE pmClx}
  {$ENDIF}
{/UCONVERT}

interface
{$IFDEF VER140} {$DEFINE D6New} {$ENDIF}
{$IFDEF VER150} {$DEFINE D6New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D6New} {$ENDIF}

{$IFDEF D6New} uses DesignEditors, DesignIntf;
{$ELSE}        uses DsgnIntf;
{$ENDIF}


type
  { TPlusLinesProperty is a property editor for design time editing of text content
    in a TPlusMemo }
  TPlusLinesProperty = class(TStringProperty)
    public
      function GetAttributes: TPropertyAttributes; override;
      {UCONVERT}
      function GetValue: string; override;
      {/UCONVERT}
      procedure Edit; override;
    end;

  { TKeywordsProperty is a property editor for design time entry of keywords
    in a TPlusMemo }
  TKeywordsProperty = class(TStringProperty)
    public
      function GetAttributes: TPropertyAttributes; override;
      {UCONVERT}
      function GetValue: string; override;
      {/UCONVERT}
      procedure Edit; override;
    end;

  { TStartStopKeysProperty is a property editor for design time entry
    of start-stop keys in a TPlusMemo. }
  TStartStopKeysProperty = class(TStringProperty)
    public
      function GetAttributes: TPropertyAttributes; override;
      {UCONVERT}
      function GetValue: string; override;
      {/UCONVERT}
      procedure Edit; override;
    end;

  {TPlusMemoEditor is a component editor such that double clicking on a TPlusMemo at design time
   brings up the Lines editor.  It also offer keywords and start-stop keys in the context menu }
  TPlusMemoEditor = class(TComponentEditor)
    public
      function GetVerbCount: Integer; override;
      {UCONVERT}
      function GetVerb(Index: Integer): string; override;
      {/UCONVERT}
      procedure ExecuteVerb(Index: Integer); override;
    end;



procedure Register;

implementation

{$IFDEF pmClx}
uses Classes, QControls, PlusMemoClx, PlusLnsClx, PlusKeysClx, PMSupportClx;
{$ELSE}
uses Classes, Controls, PlusMemo, PlusLns, PlusKeys, PMSupport;
{$ENDIF}


procedure Register;
begin
  RegisterComponents({UCONVERT}'PlusMemo'{/UCONVERT}, [TPlusMemo]);
  RegisterPropertyEditor(TypeInfo(TStrings), TPlusMemo, 'Lines', TPlusLinesProperty);
  RegisterPropertyEditor(TypeInfo(TKeywordList), TPlusMemo, 'Keywords', TKeywordsProperty);
  RegisterPropertyEditor(TypeInfo(TStartStopKeyList), TPlusMemo, 'StartStopKeys', TStartStopKeysProperty);
  RegisterComponentEditor(TPlusMemo, TPlusMemoEditor);
end;

function TPlusLinesProperty.GetAttributes;
begin
  Result:= [paDialog, paAutoUpdate]
end;

function TPlusLinesProperty.GetValue: AnsiString;
begin
  Result:= '(TStrings)'
end;

procedure TPlusLinesProperty.Edit;
begin
  with TFrmPlusLines.Create(nil) do
    begin
      MyMemo:= GetComponent(0) as TPlusMemo;
      if (ShowModal=mrOk) and PlusMemo1.Modified then Self.Modified;
      Free
    end
end;


function TKeywordsProperty.GetAttributes;
begin
  Result:= [paDialog, paAutoUpdate]
end;

function TKeywordsProperty.GetValue;
begin
  Result:= '(TKeywordList)'
end;

procedure TKeywordsProperty.Edit;
var MyMemo: TPlusMemo;
begin
  MyMemo:= GetComponent(0) as TPlusMemo;
  if EditKeywordList(MyMemo.Keywords, False, True) then
    begin
      MyMemo.ReApplyKeywords;
      Modified
    end
end;

function TStartStopKeysProperty.GetAttributes;
begin
  Result:= [paDialog, paAutoUpdate]
end;

function TStartStopKeysProperty.GetValue;
begin
  Result:= '(TStartStopKeyList)'
end;

procedure TStartStopKeysProperty.Edit;
var MyMemo: TPlusMemo;
begin
  MyMemo:= GetComponent(0) as TPlusMemo;
  if EditStartStopList(MyMemo.StartStopKeys, False) then
    begin
      MyMemo.ReApplyKeywords;
      Modified
    end
end;

{ TPlusMemoEditor }

procedure TPlusMemoEditor.ExecuteVerb(Index: Integer);
var smemo: TPlusMemo;
begin
  smemo:= Component as TPlusMemo;
  case Index of
      0:
        with TFrmPlusLines.Create(nil) do
          begin
            MyMemo:= smemo;
            if (ShowModal=mrOk) and PlusMemo1.Modified then Self.Designer.Modified;
            Free
          end;
      1:
        if EditKeywordList(smemo.Keywords, False, True) then
          begin
            smemo.ReApplyKeywords;
            Designer.Modified
          end;
      2:
        if EditStartStopList(smemo.StartStopKeys, False) then
          begin
            smemo.ReApplyKeywords;
            Designer.Modified
          end
    end
end;

{UCONVERT}
function TPlusMemoEditor.GetVerb(Index: Integer): string;
{/UCONVERT}
begin
  case Index of
      0: Result:= 'Lines...';
      1: Result:= 'Keywords...';
      2: Result:= 'Start-stop keys...'
      else Result:= ''
    end
end;

function TPlusMemoEditor.GetVerbCount: Integer;
begin
  Result:= 3;
end;

end.
