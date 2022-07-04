{ Copyright (C) 1998-2006, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  The TSMLanguage component allows to develop the multilanguage application
  without recompile. You can define the external files with translated strings
  and custom messages and in run-time to load it for modification of component properties.
}
unit SMLang;

interface

uses Classes, IniFiles, TypInfo, SysUtils;

const
  cTabChar = '~';

type
  TLanguageEvent = procedure(Sender: TObject; AComponent: TComponent;
                             PropertyName, OldValue: string; var NewValue: string) of object;
  TSMLanguage = class(TComponent)
  private
    FAutoTranslate: Boolean;
    FResourceFile: string;
    FSeparator: string;

    FOnBeforeTranslation: TNotifyEvent;
    FOnAfterTranslation: TNotifyEvent;
    FOnTranslate: TLanguageEvent;

    function GetFirstPart(var AString: string): string;
    function IsLastPart(AString: string): Boolean;
    function GetProperty(AComponent: TComponent; sProperty: string): string;
    procedure SetProperty(AComponent: TComponent; sProperty, sValue: string);
    procedure TranslateProperty(sProperty, sTranslation: string);
    procedure SetResourceFile(Value: string);
    procedure SetStringsProperty(AComponent: TComponent; PropInfo: PPropInfo; sValues: string);
  protected
    procedure SetSeparator(const Value: string);
  public
    procedure SaveResources(AComponent: TComponent; AFileName: string);
    procedure LoadResources(AFileName: string);
    function TranslateUserMessage(Ident, sMessage: string): string;
  published
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default False;
    property ResourceFile: string read FResourceFile write SetResourceFile;
    property StringsSeparator: string read FSeparator write SetSeparator;
    property OnBeforeTranslation: TNotifyEvent read FOnBeforeTranslation write FOnBeforeTranslation;
    property OnAfterTranslation: TNotifyEvent read FOnAfterTranslation write FOnAfterTranslation;
    property OnTranslate: TLanguageEvent read FOnTranslate write FOnTranslate;
  end;

implementation
uses Forms;

const
  strSectionName = 'Translations';

{ TSMLanguage }
function TSMLanguage.GetFirstPart(var AString: string): string;
var i: Integer;
begin
  i := Pos('.', AString);
  if (i > 0) then
  begin
    Result := Copy(AString, 1, i-1);
    Delete(AString, 1, i);
  end
  else
  begin
    Result := AString;
    AString := '';
  end;
  AString := Trim(AString);
  Result := Trim(Result);
end;

function TSMLanguage.IsLastPart(AString: string): Boolean;
begin
  Result := (Pos('.', AString) = 0);
end;

function TSMLanguage.GetProperty(AComponent: TComponent; sProperty: string): string;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(AComponent.ClassInfo, sProperty);
  if (PropInfo <> nil) then
    if (PropInfo^.PropType^^.Kind in [tkString, tkLString, tkWString{, tkInteger}]) then
      Result := GetStrProp(AComponent, PropInfo);
end;

procedure TSMLanguage.SetProperty(AComponent: TComponent; sProperty, sValue: string);
var
  PropInfo: PPropInfo;
begin
  if (AComponent <> nil) then
  begin
    PropInfo := GetPropInfo(AComponent.ClassInfo, sProperty);
    if (PropInfo <> nil) then
    begin
      case PropInfo^.PropType^^.Kind of
        tkString,
        tkLString,
        tkWString: SetStrProp(AComponent, PropInfo, sValue);
        tkInteger: SetOrdProp(AComponent, PropInfo, StrToInt(sValue))
      else
        SetStringsProperty(AComponent, PropInfo, sValue);
      end;
    end;
  end;
end;

function ConvertStrings(const s: string): string;
begin
  Result := s
end;

procedure TSMLanguage.SaveResources(AComponent: TComponent; AFileName: string);
var
  resFile: TIniFile;

  procedure ProcessComponent(AComp: TComponent; strParentName: string);
  var
    i, j, k: Integer;
    PropList: TPropList;
    PropInfo: PPropInfo;
  begin
    if not Assigned(AComp) or
       (AComp.Name = '') or
       (AComp = Self) then
      exit;

    i := GetPropList(AComp.ClassInfo, tkProperties, @PropList);
    for j := 0 to i-1 do
    begin
      if (PropList[j].Name = 'Name') then continue;

      PropInfo := GetPropInfo(AComp.ClassInfo, PropList[j].Name);
      if (PropInfo <> nil) then
        if (PropInfo^.PropType^^.Kind in [tkString, tkLString, tkWString{, tkInteger}]) then
        begin
          try
            resFile.WriteString(strSectionName, strParentName + AComp.Name + '.' + PropList[j].Name, GetStrProp(AComp, PropInfo))
          except
          end
        end
        else
          if (PropInfo^.PropType^^.Kind = tkClass) then
          begin
            k := GetOrdProp(AComp, PropInfo);
            if (TObject(k) is TStrings) then
              resFile.WriteString(strSectionName, strParentName + AComp.Name + '.' + PropList[j].Name, ConvertStrings(TStrings(k).Text))
          end;
    end;

    for i := 0 to AComp.ComponentCount-1 do
      ProcessComponent(AComp.Components[i], AComp.Name + '.');
  end;

begin
  if AFileName = '' then
    AFileName := ResourceFile;
  if FileExists(AFileName) then
    DeleteFile(AFileName);

  resFile := TIniFile.Create(AFileName);
  with resFile do
    try
      if Assigned(AComponent) then
        ProcessComponent(AComponent, '')
      else
        ProcessComponent(Self.Owner, '');
    finally
      Free;
    end;
end;

procedure TSMLanguage.LoadResources(AFileName: string);
var
  i: Integer;
  Properties: TStrings;
begin
  if (Assigned(FOnBeforeTranslation)) then
    FOnBeforeTranslation(Self);
  if AFileName = '' then
    AFileName := ResourceFile;

  Properties := TStringList.Create;
  with TIniFile.Create(AFileName) do
    try
      ReadSectionValues(strSectionName, Properties);
      for i := 0 to Properties.Count-1 do
        if (Trim(Properties.Names[i]) <> '') then
//          TranslateProperty(Properties[i], ReadString(strSectionName, Properties[i], ''));
          TranslateProperty(Properties.Names[i], Properties.Values[Properties.Names[i]]);
    finally
      Free;
    end;
  Properties.Free;
  if (Assigned(FOnAfterTranslation)) then
    FOnAfterTranslation(Self);
end;

procedure TSMLanguage.TranslateProperty(sProperty, sTranslation: string);
var
  AComponent: TComponent;
  i: Integer;
begin
  sProperty := Trim(sProperty);
  AComponent := Application;
  i := Pos(cTabChar, sTranslation);
  while i > 0 do
  begin
    sTranslation := Copy(sTranslation, 1, i-1) + #9 + Copy(sTranslation, i+1, Length(sTranslation)-i);
    i := Pos(cTabChar, sTranslation);
  end;
  if (sProperty <> '') then
    while (not IsLastPart(sProperty)) do
      if Assigned(AComponent) then
      AComponent := AComponent.FindComponent(GetFirstPart(sProperty))
  else
    exit;
  if ((AComponent <> nil) and (sTranslation <> '')) then
  begin
    if (Assigned(FOnTranslate)) then
      FOnTranslate(Self, AComponent, sProperty,
                   GetProperty(AComponent, sProperty), sTranslation);
    SetProperty(AComponent, GetFirstPart(sProperty), sTranslation);
  end;
end;

procedure TSMLanguage.SetResourceFile(Value: string);
begin
  if Value <> FResourceFile then
  begin
    FResourceFile := Value;
    if (not (csLoading in ComponentState)) and FAutoTranslate then
      LoadResources('');
  end;
end;

function TSMLanguage.TranslateUserMessage(Ident, sMessage: string): string;
begin
  with TIniFile.Create(ResourceFile) do
    try
      Result := ReadString('Custom', Ident, sMessage);
    finally
      Free;
    end;
end;

procedure TSMLanguage.SetStringsProperty(AComponent: TComponent;
                                         PropInfo: PPropInfo;
                                         sValues: string);
var
  AStrings: TStringList;
  sBuffer: string;
  i: Integer;
begin
  AStrings := TStringList.Create;
  i := Pos(FSeparator, sValues);
  while (i > 0) do
  begin
    sBuffer := Copy(sValues, 1, i-1);
    Delete(sValues, 1, i - 1 + Length(FSeparator));
    i := Pos(FSeparator, sValues);
    AStrings.Add(Trim(sBuffer));
  end;
  if (Length(Trim(sValues)) > 0) then
    AStrings.Add(Trim(sValues));
  SetOrdProp(AComponent, PropInfo, LongInt(Pointer(AStrings)));
  AStrings.Free;
end;

procedure TSMLanguage.SetSeparator(const Value: string);
begin
  if (Length(Trim(Value)) = 0) then
    FSeparator := #13#10
  else
    FSeparator := Value;
end;

end.
