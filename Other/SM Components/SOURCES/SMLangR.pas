{ Copyright (C) 1998-2008, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  In this unit I described the design-time editor for TSMLanguage component
}
unit SMLangR;

interface

{$IFDEF VER100}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER185}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
{$ENDIF}

{$IFDEF VER190}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
    {$DEFINE SMForBCB2009}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
  {$DEFINE SMForDelphi2009}
{$ENDIF}

uses Classes,
     {$IFDEF SMForDelphi6} DesignIntf, DesignEditors {$ELSE} DsgnIntf {$ENDIF};

procedure Register;

implementation

{$R SMLANG.DCR}
uses Dialogs, TypInfo, SMLang;

type
  { TSMLanguageComponentEditor }
  TSMLanguageComponentEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TSMLanguageFile = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMLanguage]);
  RegisterComponentEditor(TSMLanguage, TSMLanguageComponentEditor);
  RegisterPropertyEditor(TypeInfo(string), TSMLanguage, 'ResourceFile', TSMLanguageFile);
end;


{ TSMLanguageComponentEditor }
procedure TSMLanguageComponentEditor.ExecuteVerb(Index: Integer);

  function ConvertStrings(const s: string): string;
  begin
    Result := s
  end;

  procedure ProcessComponent(AComponent: TComponent; Properties: TStrings; strParentName: string);
  var
    i, j, k: Integer;
    PropList: TPropList;
    PropInfo: PPropInfo;
  begin
    if not Assigned(AComponent) or (AComponent.Name = '') then exit;

    i := GetPropList(AComponent.ClassInfo, tkProperties, @PropList);
    for j := 0 to i-1 do
    begin
      if (PropList[j].Name = 'Name') then continue;

      if Properties.IndexOf(PropList[j].Name) < 0 then
      begin
        PropInfo := GetPropInfo(AComponent.ClassInfo, PropList[j].Name);
        if (PropInfo <> nil) then
          if (PropInfo^.PropType^^.Kind in [tkString, tkLString, tkWString{, tkInteger}]) then
          begin
            try
              Properties.Add(strParentName + AComponent.Name + '.' + PropList[j].Name + ' = ' + GetStrProp(AComponent, PropInfo))
            except
            end
          end
          else
            if (PropInfo^.PropType^^.Kind = tkClass) then
            begin
              k := GetOrdProp(AComponent, PropInfo);
              if (TObject(k) is TStrings) then
                Properties.Add(strParentName + AComponent.Name + '.' + PropList[j].Name + ' = ' + ConvertStrings(TStrings(k).Text))
            end;
      end;
    end;

    for i := 0 to AComponent.ComponentCount-1 do
      ProcessComponent(AComponent.Components[i], Properties, AComponent.Name + '.');
  end;

const
  strLangFilter = 'Language files (*.lng)|(*.lng)|All files (*.*)|(*.*)';
begin
  with (Component as TSMLanguage) do
    case Index of
      0: begin
           with TSaveDialog.Create(nil) do
             try
               FileName := ResourceFile;
               Filter := strLangFilter;
               Options := [];
               if Execute then
                 SaveResources(nil, FileName);
             finally
               Free
             end;
         end;
      1: begin
           with TOpenDialog.Create(nil) do
             try
               FileName := ResourceFile;
               Filter := strLangFilter;
               Options := [ofPathMustExist];
               if Execute then
                 LoadResources(FileName);
             finally
               Free
             end;
         end;
    end
end;

function TSMLanguageComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Generate language file...';
    1: Result := 'Load from language file...';
  end;
end;

function TSMLanguageComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;


{ TSMLanguageFile }
procedure TSMLanguageFile.Edit;
begin
  with TOpenDialog.Create(nil) do
    try
      FileName := GetValue;
      Filter := 'Language files (*.lng)|(*.lng)|All files (*.*)|(*.*)';
      Options := [ofPathMustExist];
      if Execute then
        SetValue(FileName);
    finally
      Free
    end;
end;

function TSMLanguageFile.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

end.