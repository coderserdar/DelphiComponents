unit ZRPrprty;

interface

{$I ZRDefine.inc}

uses
{$IFDEF D6Above}
  DesignIntf,
  DesignEditors,
  VCLEditors,
  RTLConsts,
{$ELSE}
  DsgnIntf, // Delphi VCL
{$ENDIF}
  Classes;                               // Delphi VCL

type
  { TZRFontProperty }
  TZRFontProperty = class(TFontProperty)
  public
    procedure Edit; override;
  end;

  { TZREscapeProperty }
  TZREscapeProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: String; override;
    procedure SetValue(const Value: String);  override;
  end;

  { TZRDataFieldProperty }
  TZRDataFieldProperty = class(TStringProperty)
  public
    function GetAttributes : TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TZRVariableProperty }
  TZRVariableProperty = class(TComponentProperty)
  public
    function  GetAttributes : TPropertyAttributes; override;
    function  GetValue: String; override;
    procedure SetValue(const Value: String); override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TZRLevelProperty }
  TZRLevelProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TZReportEditor }
  TZReportEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  SysUtils, TypInfo, Graphics, Dialogs, Forms,
  ZREscape, ZReport, ZRCtrls, ZRDsgn;

const
  hcDFontEditor       = 25000;

{ TZRFontProperty }

procedure TZRFontProperty.Edit;
begin
  with TFontDialog.Create(Application) do
  try
    Font        := TFont(GetOrdValue);
    HelpContext := hcDFontEditor;
    Options     := [fdFixedPitchOnly, fdForceFontExist, fdNoSimulations, fdShowHelp];
    if Execute then SetOrdValue(Longint(Font));
  finally
    Free;
  end;
end;

{ TZREscapeProperty }

function TZREscapeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect];
end;

function TZREscapeProperty.GetValue: String;
begin
  Result := Escape2String(GetStrValue);
end;

procedure TZREscapeProperty.SetValue(const Value: String);
var
  Result : String;
begin
  Result := String2Escape(Value);
  if GetStrValue <> Result then SetStrValue(Result);
end;

{ TZRDataFieldProperty }

function TZRDataFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList];
end;

procedure TZRDataFieldProperty.GetValues(Proc: TGetStrProc);
var
  Field : TZRField;
  List  : TStringList;
  i     : Integer;
begin
  List := TStringList.Create;
  try
    Field := TZRField(GetComponent(0));
    if Assigned(Field.DataSet) then Field.DataSet.GetFieldNames(List);
    for i := 0 to List.Count-1 do Proc(List[i]);
  finally
    List.Free;
  end;
end;

{ TZRVariableProperty }

function TZRVariableProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paMultiSelect];
end;

function TZRVariableProperty.GetValue: String;
var
  aComponent : TPersistent;
  aVariable  : TZRVariable;
  //aName      : String;
begin
  aComponent := GetComponent(0);
  if aComponent is TZRCustomLabel then
    aVariable := TZRCustomLabel(aComponent).Variable
  else if aComponent is TZRGroup then
    aVariable := TZRGroup(aComponent).Variable
  else if aComponent is TZRAggregator then
    aVariable := TZRAggregator(aComponent).Variable
  else
    aVariable := nil;
  if Assigned(aVariable) then
    Result := aVariable.Master.Name + '.' + aVariable.Name
  else
    Result := inherited GetValue;
end;

procedure TZRVariableProperty.SetValue(const Value: String);
var
  p: Integer;
begin
  p := pos('.', Value);
  if p > 0 then
    inherited SetValue(copy(Value, p+1, length(Value)))
  else
    inherited;
end;

procedure TZRVariableProperty.GetValues(Proc: TGetStrProc);
var
  aComponent : TPersistent;
  aStart,
  aMaster    : TZRCustomController;
  aVariable  : TZRVariable;
  aName      : String;
  i          : Integer;
begin
  aComponent := GetComponent(0);
  if aComponent is TZRCustomLabel then begin
    if TZRLabel(aComponent).Band is TZRCustomController then
      aMaster := TZRLabel(aComponent).Band as TZRCustomController
    else
      aMaster := TZRLabel(aComponent).Band.Master;
  end else
  if aComponent is TZRGroup    then aMaster := TZRGroup   (aComponent).Master else
  if aComponent is TZRVariable then aMaster := TZRVariable(aComponent).Master else begin
    inherited;
    Exit;
  end;
  aStart := aMaster;
  while Assigned(aMaster) do begin
    with aMaster.VariableList do
      for i := 0 to Count-1 do begin
        aVariable := TZRVariable(Items[i]);
        aName     := aMaster.Name + '.' + aVariable.Name;
        if (aComponent is TZRAggregator) then begin
          if not (aVariable is TZRAggregator) then Proc(aName);
        end else
        if (aComponent is TZRTotalLabel) then begin
          if (aVariable is TZRAggregator) and
             (aStart = aVariable.Master) then Proc(aVariable.Name);
        end else
          Proc(aName);
      end;
    aMaster := aMaster.Master;
  end;
end;

{ TZRLevelProperty }

procedure TZRLevelProperty.GetValues(Proc: TGetStrProc);
var
  aMaster : TZRCustomController;
  i       : Integer;
begin
  aMaster := TZRTotalLabel(GetComponent(0)).Band.Master;
  while Assigned(aMaster) do begin
    Proc(aMaster.Name);
    for i := 0 to aMaster.GroupList.Count-1 do
      Proc(TZRGroup(aMaster.GroupList[i]).Name);
    //aMaster := aMaster.Master;
    aMaster := nil;
  end;
end;

{ TZReportEditor }

procedure TZReportEditor.Edit;
var
  Report : TZReport;
  Form   : TZRDesignForm;
begin
  Report := (Self.Component as TZReportControl).Report;
  if Report.DesignForm = nil then begin
    Form := TZRDesignForm.Create(Application);
    Report.DesignForm := Form;
    Form.Designer := Self.Designer;
    Form.Report   := Report;
  end;
  Report.DesignForm.Show;
end;

procedure TZReportEditor.ExecuteVerb(Index: Integer);
var
  Report : TZReport;
begin
  Report := (Component as TZReportControl).Report;
  case Index of
    0: Edit;
    1: if Assigned(Report) then Report.Preview;
  end;
end;

function TZReportEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TZReportEditor.GetVerb(Index: Integer): String;
var
  Report : TZReport;
begin
  Report := (Component as TZReportControl).Report;
  case Index of
    0  : Result := Report.Name + '..';
    1  : Result := Report.Name + ': Preview';
    else Result := '';
  end;
end;

end.

