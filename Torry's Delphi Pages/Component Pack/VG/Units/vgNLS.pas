{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         National Language Support                     }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgNLS;

interface
uses SysUtils, Classes, IniFiles, vgStndrt;

type
  TTranslateMsgEvent = procedure (Sender: TObject; const OldMsg: string;
    var NewMsg: string) of object;

  TTranslatePropEvent = procedure (Sender: TObject; const PropPath, OldPropValue: string;
    var NewPropValue: string) of object;

  TTranslatePropStringEvent = procedure (Sender: TObject; Instance: TObject; const PropName: string;
    const OldPropValue: string; var NewPropValue: string) of object;

  TTranslatePropInstanceEvent = procedure (Sender: TObject; Instance: TObject) of object;

{$IFNDEF _D4_}
  TCustomIniFile = TIniFile;
{$ENDIF}

{ TvgCustomTranslator }
  TvgCustomTranslator = class(TComponent)
  private
    FActive, FStreamedActive: Boolean;
    FProps: TStrings;
    FRoot: TObject;
    FTransList: TList;
    FTransStack: string;
    FOnTranslateMsg: TTranslateMsgEvent;
    FOnTranslateProp: TTranslatePropEvent;
    FOnTranslatePropString: TTranslatePropStringEvent;
    FOnTranslatePropInstance: TTranslatePropInstanceEvent;
    procedure SetActive(Value: Boolean);
    procedure SetProps(Value: TStrings);
  protected
    procedure DoActivate; virtual;
    procedure DoDeactivate; virtual;
    function DoTranslateMsg(const Msg: string): string; virtual;
    function DoTranslateProp(const PropPath, PropValue: string): string; virtual;
    procedure DoTranslatePropString(Instance: TObject; const PropName: string;
      const OldPropValue: string; var NewPropValue: string); virtual;
    procedure DoTranslatePropInstance(Instance: TObject); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function TranslatePath: string;
    function TranslatePathProp(const PropName: string): string;
    function TranslateProp(const PropPath, PropValue: string): string;
    procedure TranslatePropClass(Instance: TObject; const PropName: string);
    procedure TranslatePropString(Instance: TObject; const PropName: string);
    procedure TranslateProps(Instance: TObject);
    function TranslateMessage(const Msg: string): string;
    function TranslateUserMessage(const Msg: string): string; { For TLanguage compability }
    function TMsg(const Msg: string): string;                 { TranslateMessage macro }
    procedure Translate;
    property Root: TObject read FRoot;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Properties: TStrings read FProps write SetProps;
    property OnTranslateMsg: TTranslateMsgEvent read FOnTranslateMsg write FOnTranslateMsg;
    property OnTranslateProp: TTranslatePropEvent read FOnTranslateProp write FOnTranslateProp;
    property OnTranslatePropString: TTranslatePropStringEvent read FOnTranslatePropString write FOnTranslatePropString;
    property OnTranslatePropInstance: TTranslatePropInstanceEvent read FOnTranslatePropInstance write FOnTranslatePropInstance;
  end;

{ TvgTranslator }
  TvgTranslator = class(TvgCustomTranslator)
  private
    FInCreateFile: Boolean;
    FIniFileName: TFileName;
    FIniFile: TCustomIniFile;
{$IFDEF _D4_}
    FAppIniFile: Boolean;
    FIniFileLink: TIniFileLink;
{$ENDIF}
    procedure SetIniFileName(const Value: TFileName);
{$IFDEF _D4_}
    function GetAppIniFile: TAppIniFile;
    procedure SetAppIniFile(Value: TAppIniFile);
    procedure IniDestroy(Sender: TObject);
{$ENDIF}
  protected
    procedure DoActivate; override;
    procedure DoDeactivate; override;
    function DoTranslateMsg(const Msg: string): string; override;
    function DoTranslateProp(const PropPath, PropValue: string): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateLanguageFile(const FileName: TFileName; SaveOld: Boolean);
    property IniFile: TCustomIniFile read FIniFile;
  published
    property Active: Boolean read FActive write SetActive default False;
{$IFDEF _D4_}
    property AppIniFile: TAppIniFile read GetAppIniFile write SetAppIniFile;
{$ENDIF}
    property LanguageFile: TFileName read FIniFileName write SetIniFileName;
  end;

const
  SectionTranslate = 'Translations';
  SectionMessages  = 'Messages';

{ Utility routines }
function TranslateStringsToString(Strings: TStrings): string;
procedure TranslateStringToStrings(Msg: string; Strings: TStrings);

implementation
uses TypInfo, vgUtils, Forms;

const
  StringProps   = [tkString, tkLString];
  ClassProps    = [tkClass];
  DefDelimeter  = '.';


function TranslateStringsToString(Strings: TStrings): string;
var
  S: string;
  I, J: Integer;
begin
  Result := '';
  for I := 0 to Strings.Count - 1 do
  begin
    S := Strings[I];

    for J := 1 to Length(S) do
      if S[J] = ',' then
        Result := Result + ',,' else
        Result := Result + S[J];

    Result := Result + ',';
  end;
  System.Delete(Result, Length(Result), 1);
end;

procedure TranslateStringToStrings(Msg: string; Strings: TStrings);
var
  P: PChar;
  Tmp: string;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;

    Tmp := '';
    P := PChar(Msg);
    while P^ <> #0 do
    begin
      if P^ = ',' then
      begin
        Inc(P);
        if (P^ <> ',') then
        begin
          Strings.Add(Tmp);
          Tmp := '';
          Continue;
        end;
      end;
      Tmp := Tmp + P^;
      Inc(P);
      if P^ = #0 then Strings.Add(Tmp);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

{ TvgCustomTranslator }
constructor TvgCustomTranslator.Create(AOwner: TComponent);
begin
  inherited;
  FProps := TStringList.Create;
end;

destructor TvgCustomTranslator.Destroy;
begin
  SetActive(False);
  FProps.Free;
  inherited;
end;

procedure TvgCustomTranslator.Loaded;
begin
  inherited;
  try
    SetActive(FStreamedActive);
    if not (csDesigning in ComponentState) and Active then
      Translate;
  except
    if (csDesigning in ComponentState) then
      Application.HandleException(Self)
    else
      raise;
  end;
end;

procedure TvgCustomTranslator.SetActive(Value: Boolean);
begin
  if csLoading in ComponentState then
    FStreamedActive := Value
  else if FActive <> Value then
  begin
    if Value then
      DoActivate else DoDeactivate;
    FActive := Value;
  end;
end;

procedure TvgCustomTranslator.SetProps(Value: TStrings);
begin
  FProps.Assign(Value);
end;

procedure TvgCustomTranslator.DoActivate;
begin
end;

procedure TvgCustomTranslator.DoDeactivate;
begin
end;

function TvgCustomTranslator.DoTranslateMsg(const Msg: string): string;
begin
  Result := Msg;
  if Assigned(FOnTranslateMsg) then
    FOnTranslateMsg(Self, Msg, Result);
end;

function TvgCustomTranslator.DoTranslateProp(const PropPath, PropValue: string): string;
begin
  Result := PropValue;
  if Assigned(FOnTranslateProp) then
    FOnTranslateProp(Self, PropPath, PropValue, Result);
end;

procedure TvgCustomTranslator.DoTranslatePropString(Instance: TObject; const PropName: string;
  const OldPropValue: string; var NewPropValue: string);
var
  PropPath: string;
begin
  PropPath := TranslatePathProp(PropName);

  if (FProps.Count = 0) or (FProps.IndexOf(PropPath) >= 0) then
  begin
    if Assigned(FOnTranslatePropString) then
      FOnTranslatePropString(Self, Instance, PropName, OldPropValue, NewPropValue) else
      NewPropValue := TranslateProp(PropPath, OldPropValue);
  end;
end;

procedure TvgCustomTranslator.DoTranslatePropInstance(Instance: TObject);
var
  I: Integer;
  SaveStack: string;
  NewPropValue, OldPropValue: string;
begin
  if Instance is TStrings then
  begin
    OldPropValue := TranslateStringsToString(TStrings(Instance));
    NewPropValue := OldPropValue;
    DoTranslatePropString(Instance, '', OldPropValue, NewPropValue);
    if OldPropValue <> NewPropValue then
      TranslateStringToStrings(NewPropValue, TStrings(Instance));
  end else if Instance is TCollection then
  begin
    SaveStack := FTransStack;
    try
      for I := 0 to TCollection(Instance).Count - 1 do
      begin
        FTransStack := SaveStack + DefDelimeter + IntToStr(I);;
        TranslateProps(TCollection(Instance).Items[I]);
      end;
    finally
      FTransStack := SaveStack;
    end;
  end;

  if Assigned(FOnTranslatePropInstance) then
    FOnTranslatePropInstance(Self, Instance);
end;

function TvgCustomTranslator.TranslatePath: string;
begin
  Result := FTransStack;
end;

function TvgCustomTranslator.TranslatePathProp(const PropName: string): string;
begin
  if PropName <> '' then
    Result := TranslatePath + DefDelimeter + PropName else
    Result := TranslatePath;
end;

function TvgCustomTranslator.TranslateProp(const PropPath, PropValue: string): string;
begin
  SetActive(True);
  Result := DoTranslateProp(PropPath, PropValue);
end;

procedure TvgCustomTranslator.TranslatePropClass(Instance: TObject; const PropName: string);
var
  PropValue: TObject;
  SaveStack: string;
begin
  PropValue := TObject(GetOrdProp(Instance, GetPropInfo(Instance.ClassInfo, PropName)));
  if Assigned(PropValue) and not (PropValue is TComponent) then
  begin
    SaveStack := FTransStack;
    FTransStack := FTransStack + DefDelimeter + PropName;
    try
      TranslateProps(PropValue);
    finally
      FTransStack := SaveStack;
    end;
  end;
end;

procedure TvgCustomTranslator.TranslatePropString(Instance: TObject; const PropName: string);
var
  OldPropValue, NewPropValue: string;
begin
  OldPropValue := GetStrProp(Instance, GetPropInfo(Instance.ClassInfo, PropName));
  NewPropValue := OldPropValue;
  DoTranslatePropString(Instance, PropName, OldPropValue, NewPropValue);
  if OldPropValue <> NewPropValue then
    SetStrProp(Instance, GetPropInfo(Instance.ClassInfo, PropName), NewPropValue);
end;

procedure TvgCustomTranslator.TranslateProps(Instance: TObject);

  procedure DoTranslate(Instance: TComponent; Data: Pointer);
  begin
    if Instance.Name <> '' then
      TvgCustomTranslator(Data).TranslateProps(Instance);
  end;

  function GetStackDelta: string;
  begin
    if Instance is TComponent then
      Result := TComponent(Instance).Name else
      Result := '';
  end;

var
  I: Integer;
  StackDelta: string;
  List: TList;
  First: Boolean;
begin
  Active := True;

  { Check for possible cross-references }
  First := not Assigned(FTransList);
  if First then FRoot := Instance;

  if (ListIndexOf(FTransList, Instance) >= 0) or (Instance is TComponent)
    and (Instance <> FRoot) and (ListIndexOf(FTransList, TComponent(Instance).Owner) < 0) then Exit;

  ListAdd(FTransList, Instance);
  try
    StackDelta := GetStackDelta;
    if StackDelta <> '' then
      AddDelimeted(FTransStack, StackDelta, DefDelimeter);

    List := TList.Create;
    try
      DoTranslatePropInstance(Instance);

      { String properties }
      GetPropInfoList(List, Instance, StringProps);
      for I := 0 to List.Count - 1 do
        TranslatePropString(Instance, PPropInfo(List[I]).Name);

      { Class properties }
      GetPropInfoList(List, Instance, ClassProps);
      for I := 0 to List.Count - 1 do
        TranslatePropClass(Instance, PPropInfo(List[I]).Name);

      { Nested components }
      if Instance is TComponent then
        ForEachComponent(TComponent(Instance), TComponent, @DoTranslate, Self, False);
    finally
      List.Free;
    end;
  finally
    if First then
    begin
      ListClear(FTransList);
      FRoot := nil;
      FTransStack := '';
    end else
      if StackDelta <> '' then
        FTransStack := Copy(FTransStack, 1, Length(FTransStack) - Length(StackDelta) - 1);
  end;
end;

function TvgCustomTranslator.TranslateMessage(const Msg: string): string;
begin
  SetActive(True);
  Result := DoTranslateMsg(Msg)
end;

function TvgCustomTranslator.TranslateUserMessage(const Msg: string): string;
begin
  Result := TranslateMessage(Msg);
end;

function TvgCustomTranslator.TMsg(const Msg: string): string;
begin
  Result := TranslateMessage(Msg);
end;

procedure TvgCustomTranslator.Translate;
begin
  if Assigned(Owner) then TranslateProps(Owner);
end;

{ TvgTranslator }
constructor TvgTranslator.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF _D4_}
  FIniFileLink := TIniFileLink.Create;
  FIniFileLink.OnIniDestroy := IniDestroy;
{$ENDIF}
end;

destructor TvgTranslator.Destroy;
begin
{$IFDEF _D4_}
  FIniFileLink.Free;
{$ENDIF}
  inherited;
end;

{$IFDEF _D4_}
function TvgTranslator.GetAppIniFile: TAppIniFile;
begin
  Result := FIniFileLink.AppIniFile;
end;

procedure TvgTranslator.SetAppIniFile(Value: TAppIniFile);
var
  SaveActive: Boolean;
begin
  if AppIniFile <> Value then
  begin
    SaveActive := Active;
    Active := False;
    SetIniFileName('');
    FIniFileLink.AppIniFile := Value;
    if Assigned(AppIniFile) then
      Active := SaveActive;
  end;
end;

procedure TvgTranslator.IniDestroy(Sender: TObject);
begin
  Active := False;
end;
{$ENDIF}

procedure TvgTranslator.DoActivate;
begin
{$IFDEF _D4_}
  if Assigned(AppIniFile) then
  begin
    if FInCreateFile and AppIniFile.BeginRead
      or not FInCreateFile and AppIniFile.BeginWrite then
    begin
      FIniFile := AppIniFile.IniFile;
      FAppIniFile := True;
      Exit;
    end;
  end;
{$ENDIF}
  FIniFile := TIniFile.Create(FIniFileName);
end;

procedure TvgTranslator.DoDeactivate;
begin
{$IFDEF _D4_}
  if FAppIniFile then
  begin
    if Assigned(AppIniFile) then
      if FInCreateFile then
        AppIniFile.EndWrite else
        AppIniFile.EndRead;
    FAppIniFile := False;
    FIniFile := nil;
  end else
{$ENDIF}
  begin
    FIniFile.Free;
    FIniFile := nil;
  end;
end;

function TvgTranslator.DoTranslateMsg(const Msg: string): string;
begin
  Result := Msg;
  if Assigned(OnTranslateMsg) then
    OnTranslateMsg(Self, Msg, Result)
  else
    Result := FIniFile.ReadString(SectionMessages, Msg, Result);
end;

function TvgTranslator.DoTranslateProp(const PropPath, PropValue: string): string;
begin
  Result := PropValue;
  if FInCreateFile then
  begin
    if PropValue <> '' then
      FIniFile.WriteString(SectionTranslate, PropPath, PropValue)
  end else if Assigned(OnTranslateProp) then
    OnTranslateProp(Self, PropPath, PropValue, Result)
  else
    Result := FIniFile.ReadString(SectionTranslate, PropPath, Result);
end;

procedure TvgTranslator.SetIniFileName(const Value: TFileName);
var
  SaveActive: Boolean;
begin
  if csLoading in ComponentState then
    FIniFileName := Value
  else if FIniFileName <> Value then
  begin
    SaveActive := Active;
    Active := False;
    FIniFileName := Value;
{$IFDEF _D4_}
    SetAppIniFile(nil);
{$ENDIF}
    if FIniFileName <> '' then Active := SaveActive;
  end;
end;

procedure TvgTranslator.CreateLanguageFile(const FileName: TFileName; SaveOld: Boolean);
var
  SaveActive: Boolean;
  SaveFileName: TFileName;
begin
  if SaveOld then
    CheckBackupFile(FileName) else DeleteFile(FileName);

  SaveActive := Active;
  try
    Active := False;

    try
      SaveFileName := LanguageFile;
      LanguageFile := FileName;
      FInCreateFile := True;
      try
        Translate;
      finally
        FInCreateFile := False;
      end;
    finally
      LanguageFile := SaveFileName;
    end;
  finally
    Active := SaveActive;
  end;
end;

end.



