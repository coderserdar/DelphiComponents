{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       Setting Controls                                }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{                                                       }
{*******************************************************}
unit SettingCtrls;

interface
uses
   Classes
  ,Controls
  ,Windows
  ,Contnrs
  ,DB
  ,IniFiles
  ,Registry
  ,Graphics
  ,ComCtrls
  ,ExtCtrls
  ;

type
{------------------------------------------------------------------------------}
  TgtSettingsManager = class;
{------------------------------------------------------------------------------}
  TgtSettingType = (
                      stNone
                     ,stIni
                     ,stRegistry
                     ,stDataBase
                     );
{------------------------------------------------------------------------------}
  TgtSettingRegistryRootKeys = (
                                rrNone
                               ,rrCurrentUser
                               ,rrLocalMachine
                               ,rrSystem
                               );
{------------------------------------------------------------------------------}
  TgtCustomSettingControl = class(TWinControl)
  private
    { Private declarations }
    FControl         : TControl;
    FSettingsManager : TgtSettingsManager;
    FGroupID         : Integer;
    FIniSection      : string;
    FEnCoded         : Boolean;
    FCaption         : string;
    FOnValueChanged  : TNotifyEvent;
    FColor           : TColor;
    FFont            : TFont;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
  protected
    { Protected declarations }
    procedure SetSettingsManager(const Value:TgtSettingsManager);virtual;
    procedure SetSetttingValue(const Value : Variant);
    procedure DoSetSettingValue(const Value : Variant);virtual;
    function  GetSettingValue: Variant;virtual;
    procedure CreateControl(ControlClass : TControlClass);
    procedure Notification(AComponent: TComponent;Operation: TOperation);override;
  protected
    procedure SetCaption(const Value: string);virtual;
    property  Caption : string read FCaption write SetCaption;
    function GetItems: TStrings;virtual;abstract;
    procedure SetItems(const Value: TStrings);virtual;abstract;
    property Items : TStrings read GetItems write SetItems;
    function GetSelectedIndex: Integer;virtual;abstract;
    procedure SetSelectedIndex(const Value: Integer);virtual;abstract;
    property SelectedIndex : Integer read GetSelectedIndex write SetSelectedIndex;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property SettingsManager : TgtSettingsManager read FSettingsManager write SetSettingsManager;
    property GroupID         : Integer            read FGroupID         write FGroupID;
    property IniSection      : string             read FIniSection      write FIniSection;
    property SettingValue    : Variant            read GetSettingValue  write SetSetttingValue;
    property EnCoded         : Boolean            read FEnCoded         write FEnCoded default False;
  published
    property Color           : TColor             read FColor           write SetColor;
    property Font            : TFont              read FFont            write SetFont;
    property Align;
    property Visible;
  published
    property OnValueChanged : TNotifyEvent read FOnValueChanged write FOnValueChanged;
  end;
{------------------------------------------------------------------------------}
  TgtCSCEdit = class(TgtCustomSettingControl)
  private
    FText: string;
    procedure SetText(const Value: string);
    { Private declarations }
  protected
    { Protected declarations }
    procedure DoSetSettingValue(const Value : Variant);override;
    function  GetSettingValue:Variant;override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property Text : string read FText write SetText;
  end;
{------------------------------------------------------------------------------}
  TLabelPosition = (lpAbove, lpBelow, lpLeft, lpRight);
{------------------------------------------------------------------------------}
  TgtCSCLabeledEdit = class(TgtCustomSettingControl)
  private
    FText: string;
    FLabelPosition: TLabelPosition;
    procedure SetText(const Value: string);
    procedure SetLabelPosition(const Value: TLabelPosition);
    { Private declarations }
  protected
    { Protected declarations }
    procedure DoSetSettingValue(const Value : Variant);override;
    function  GetSettingValue:Variant;override;
    procedure SetCaption(const Value: string);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property Text          : string         read FText           write SetText;
    property LabelPosition : TLabelPosition read FLabelPosition  write SetLabelPosition;
    property Caption;
  end;
{------------------------------------------------------------------------------}
  TgtCSCCheckBox = class(TgtCustomSettingControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure DoSetSettingValue(const Value : Variant);override;
    function  GetSettingValue:Variant;override;
    procedure SetCaption(const Value: string);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property Caption;
  end;
{------------------------------------------------------------------------------}
  TgtCSCRadioButton = class(TgtCustomSettingControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure DoSetSettingValue(const Value : Variant);override;
    function  GetSettingValue:Variant;override;
    procedure SetCaption(const Value: string);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property Caption;
  end;
{------------------------------------------------------------------------------}
  TgtCSCComboBox = class(TgtCustomSettingControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure DoSetSettingValue(const Value : Variant);override;
    function  GetSettingValue:Variant;override;
    function GetItems: TStrings;override;
    procedure SetItems(const Value: TStrings);override;
    function GetSelectedIndex: Integer;override;
    procedure SetSelectedIndex(const Value: Integer);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property Items;
    property SelectedIndex;
  end;
{------------------------------------------------------------------------------}
  TgtCSCListBox = class(TgtCustomSettingControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure DoSetSettingValue(const Value : Variant);override;
    function  GetSettingValue:Variant;override;
    function  GetItems: TStrings;override;
    procedure SetItems(const Value: TStrings);override;
    function  GetSelectedIndex: Integer;override;
    procedure SetSelectedIndex(const Value: Integer);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property Items;
    property SelectedIndex;
  end;
{------------------------------------------------------------------------------}
  TgtCSCMemo = class(TgtCustomSettingControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure DoSetSettingValue(const Value : Variant);override;
    function  GetSettingValue:Variant;override;
    function GetItems: TStrings;override;
    procedure SetItems(const Value: TStrings);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property Items;
  end;
{------------------------------------------------------------------------------}
  TgtCSCDateTimePicker = class(TgtCustomSettingControl)
  private
    FShowCheckBox: Boolean;
    FChecked: Boolean;
    FDateTimeKind: TDateTimeKind;
    FDateFormat: TDTDateFormat;
    procedure SetChecked(const Value: Boolean);
    procedure SetDateFormat(const Value: TDTDateFormat);
    procedure SetDateTimeKind(const Value: TDateTimeKind);
    procedure SetShowCheckBox(const Value: Boolean);
  {Private Declarations}
  protected
  {Protected Declarations}
    procedure DoSetSettingValue(const Value : Variant);override;
    function  GetSettingValue:Variant;override;
  public
  {Public Declarations}
    constructor Create(AOwner : TComponent);override;
    destructor  Destroy;override;
  published
  {Published Declarations}
    property DateTimeKind : TDateTimeKind read FDateTimeKind write SetDateTimeKind;
    property DateFormat   : TDTDateFormat read FDateFormat   write SetDateFormat;
    property ShowCheckBox : Boolean       read FShowCheckBox write SetShowCheckBox;
    property Checked      : Boolean       read FChecked      write SetChecked;
  end;
{------------------------------------------------------------------------------}
  TgtCSCTrackBar = class(TgtCustomSettingControl)
  private
    FOrientation: TTrackBarOrientation;
    procedure SetOrientation(const Value: TTrackBarOrientation);
  {Private Declarations}
  protected
  {Protected Declarations}
    procedure DoSetSettingValue(const Value : Variant);override;
    function  GetSettingValue:Variant;override;
  public
  {Public Declarations}
    constructor Create(AOwner : TComponent);override;
    destructor  Destroy;override;
  published
  {Published Declarations}
    property Orientation : TTrackBarOrientation read FOrientation write SetOrientation;
  end;
{------------------------------------------------------------------------------}
  TgtCSCRadioGroup = class(TgtCustomSettingControl)
  private
  {Private Declarations}
  protected
  {Protected Declarations}
    procedure DoSetSettingValue(const Value : Variant);override;
    function  GetSettingValue:Variant;override;
    function GetItems: TStrings;override;
    procedure SetItems(const Value: TStrings);override;
    function  GetSelectedIndex: Integer;override;
    procedure SetSelectedIndex(const Value: Integer);override;
    procedure SetCaption(const Value: string);override;
  public
  {Public Declarations}
    constructor Create(AOwner : TComponent);override;
    destructor  Destroy;override;
  published
  {Published Declarations}
    property Items;
    property SelectedIndex;
    property Caption;
  end;
{------------------------------------------------------------------------------}
  TgtCSCColorBox = class(TgtCustomSettingControl)
  private
    FDefaultColorColor: TColor;
    FNoneColorColor: TColor;
    FStyle: TColorBoxStyle;
    function GetColor(Index: Integer): TColor;
    function GetColorName(Index: Integer): string;
    function GetSelected: TColor;
    procedure SetDefaultColorColor(const Value: TColor);
    procedure SetNoneColorColor(const Value: TColor);
    procedure SetSelected(const Value: TColor);
    procedure SetStyle(const Value: TColorBoxStyle);
  {Private Declarations}
  protected
  {Protected Declarations}
    procedure DoSetSettingValue(const Value : Variant);override;
    function  GetSettingValue:Variant;override;
    function  GetSelectedIndex: Integer;override;
    procedure SetSelectedIndex(const Value: Integer);override;
  public
  {Public Declarations}
    constructor Create(AOwner : TComponent);override;
    destructor  Destroy;override;
  public
    property Colors[Index: Integer]    : TColor read GetColor;
    property ColorNames[Index: Integer]: string read GetColorName;
  published
  {Published Declarations}
    property Style                     : TColorBoxStyle read FStyle write SetStyle default [cbStandardColors, cbExtendedColors, cbSystemColors];
    property Selected                  : TColor read GetSelected write SetSelected default clBlack;
    property DefaultColorColor         : TColor read FDefaultColorColor write SetDefaultColorColor default clBlack;
    property NoneColorColor            : TColor read FNoneColorColor write SetNoneColorColor default clBlack;
  end;
{------------------------------------------------------------------------------}
  TgtSettingsManagerStorageOptions = class(TPersistent)
  private
    FRegistryKey      : string;
    FIniFileName      : string;
    FSettingsDataSet  : TDataSet;
    FRegistryRootKey  : TgtSettingRegistryRootKeys;
    FSettingsManager  : TgtSettingsManager;
    FSettingNameField: string;
    FSettingValueField: string;
    FIniSections: TStrings;
    procedure SetSettingsDataSet(const Value: TDataSet);
    procedure SetIniSections(const Value: TStrings);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(SettingsManager : TgtSettingsManager);
    destructor  Destroy;override;
  published
    property IniFileName       : string                     read FIniFileName       write FIniFileName;
    property IniSections       : TStrings                   read FIniSections       write SetIniSections;
    property RegistryKey       : string                     read FRegistryKey       write FRegistryKey;
    property RegistryRootKey   : TgtSettingRegistryRootKeys read FRegistryRootKey   write FRegistryRootKey default rrNone ;
    property SettingsDataSet   : TDataSet                   read FSettingsDataSet   write SetSettingsDataSet;
    property SettingNameField  : string                     read FSettingNameField  write FSettingNameField;
    property SettingValueField : string                     read FSettingValueField write FSettingValueField;
  end;
{------------------------------------------------------------------------------}
  TgtSettingsManager = class(TComponent)
  private
    { Private declarations }
    FSettingComponents : TComponentList;
    FSettingsType      : TgtSettingType;
    FStorageOptions    : TgtSettingsManagerStorageOptions;
    FIniFile           : TIniFile;
    FRegistry          : TRegistry;
    FOnBeforeSaveData  : TNotifyEvent;
    FOnAfterLoadData   : TNotifyEvent;
    FOnAfterSaveData   : TNotifyEvent;
    FOnBeforeLoadData  : TNotifyEvent;
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent;Operation: TOperation);override;
    procedure SaveSetting(SettingName :string;SettingValue:Variant;EnCoded:Boolean =False;IniSection:string = '');
    function  LoadSetting(SettingName:string;DefaultValue:Variant; EnCoded:Boolean =False;IniSection:string  = ''):Variant;
    procedure Prepare;
    function  Encode(S:string):string;
    function  Decode(S:string):string;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    procedure AddSettingsComponent   (ASettingsComponent : TComponent);
    procedure RemoveSettingsComponent(ASettingsComponent : TComponent);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure ClearControlValues;
    procedure DeleteRegistryKey;
    function  GetValue(SettingName:string):Variant;
  published
    { Published declarations}
    property OnAfterLoadData     : TNotifyEvent                     read FOnAfterLoadData     write FOnAfterLoadData;
    property OnAfterSaveData     : TNotifyEvent                     read FOnAfterSaveData     write FOnAfterSaveData;
    property OnBeforeLoadData    : TNotifyEvent                     read FOnBeforeLoadData    write FOnBeforeLoadData;
    property OnBeforeSaveData    : TNotifyEvent                     read FOnBeforeSaveData    write FOnBeforeSaveData;
  published
    property SettingsType   : TgtSettingType                   read FSettingsType   write FSettingsType default stNone;
    property StorageOptions : TgtSettingsManagerStorageOptions read FStorageOptions write FStorageOptions;
  end;
{------------------------------------------------------------------------------}


function SettingsManager : TgtSettingsManager;

var
 _SettingsManager : TgtSettingsManager = nil;

implementation

uses
   Variants
  ,SysUtils
  ,TypInfo
  ,StdCtrls
  ;

function SettingsManager : TgtSettingsManager;
begin
  if not Assigned(_SettingsManager) then
    _SettingsManager := TgtSettingsManager.Create(nil);
  Result := _SettingsManager;
end;


const
  REG_KEY_COULD_NOT_BE_OPENED  = 'The key %s could not be opened';
  REG_KEY_COULD_NOT_BE_DELETED = 'The key %s could not be deleted';

  COMPONENT_IS_NOT_VALID       = 'The component you have tried to add to SettingsManager is not VALID! Component must have a name!';
  COMPONENT_HAS_NO_INI_SECTION = 'The inisection in component %s has not been set';

const
  Codes64 = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/ÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÓÔÕÖ×ØÙáâãäåæçèéêëìíïðñóôõö÷øù';


{ TgtSettingsManager }
{------------------------------------------------------------------------------}
constructor TgtSettingsManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSettingComponents := TComponentList.Create(False);
  FStorageOptions    := TgtSettingsManagerStorageOptions.Create(Self);
end;
{------------------------------------------------------------------------------}
destructor TgtSettingsManager.Destroy;
begin
  FreeAndNil(FSettingComponents);
  FStorageOptions.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtSettingsManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FStorageOptions.SettingsDataSet  then FStorageOptions.SettingsDataSet := nil
  end;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
function TgtSettingsManager.Decode(S: string): string;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Pos(s[i], codes64) - 1;
    if x >= 0 then
    begin
      b := b * 64 + x;
      a := a + 6;
      if a >= 8 then
      begin
        a := a - 8;
        x := b shr a;
        b := b mod (1 shl a);
        x := x mod 256;
        Result := Result + chr(x);
      end;
    end
    else
      Exit;
  end;
end;
{------------------------------------------------------------------------------}
function TgtSettingsManager.Encode(S: string): string;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Ord(s[i]);
    b := b * 256 + x;
    a := a + 8;
    while a >= 6 do
    begin
      a := a - 6;
      x := b div (1 shl a);
      b := b mod (1 shl a);
      Result := Result + Codes64[x + 1];
    end;
  end;
  if a > 0 then
  begin
    x := b shl (6 - a);
    Result := Result + Codes64[x + 1];
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtSettingsManager.Prepare;
begin
  case SettingsType of
  stNone     :
    begin
    end;
  stIni      :
    begin
      if Assigned(FIniFile) then
        FreeAndNil(FIniFile);
      FIniFile := TIniFile.Create(FStorageOptions.IniFileName);
    end;
  stRegistry :
    begin
      if Assigned(FRegistry) then
        FreeAndNil(FRegistry);
      FRegistry := TRegistry.Create;
      case FStorageOptions.RegistryRootKey of
        rrNone         :
          begin
          end;
        rrCurrentUser  :
          begin
           FRegistry.RootKey := HKEY_CURRENT_USER;
           if not FRegistry.OpenKey(FStorageOptions.RegistryKey,False) then
            raise Exception.CreateFmt(REG_KEY_COULD_NOT_BE_OPENED,[FStorageOptions.RegistryKey]);
          end;
        rrLocalMachine :
          begin
            FRegistry.RootKey := HKEY_LOCAL_MACHINE;
            if not FRegistry.OpenKey(FStorageOptions.RegistryKey,False) then
              raise Exception.CreateFmt(REG_KEY_COULD_NOT_BE_OPENED,[FStorageOptions.RegistryKey]);
          end;
        rrSystem       :
          begin
            FRegistry.RootKey := HKEY_CURRENT_CONFIG;
            if not FRegistry.OpenKey(FStorageOptions.RegistryKey,False) then
             raise Exception.CreateFmt(REG_KEY_COULD_NOT_BE_OPENED,[FStorageOptions.RegistryKey]);
          end;
      end;
    end;
  stDataBase :
    begin
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtSettingsManager.DeleteRegistryKey;
begin
  if SettingsType = stRegistry then
    if not FRegistry.DeleteKey(FStorageOptions.RegistryKey) then
      raise Exception.CreateFmt(FStorageOptions.RegistryKey,[REG_KEY_COULD_NOT_BE_DELETED]);
end;
{------------------------------------------------------------------------------}
procedure TgtSettingsManager.ClearControlValues;
var
  i : Integer;
begin
  for i:= 0 to Pred(FSettingComponents.Count) do
  begin
    TgtCustomSettingControl(FSettingComponents[i]).SettingValue := null;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtSettingsManager.SaveSetting(SettingName: string;SettingValue: Variant;EnCoded:Boolean;IniSection:string);
var
  Temp : string;
begin
  case SettingsType of
  stNone     :
    begin
    end;
  stIni      :
    begin
      Temp := VarToStr(SettingValue);
      if EnCoded then
        FIniFile.WriteString(IniSection,SettingName,EnCode(Temp))
      else
        FIniFile.WriteString(IniSection,SettingName,Temp)
    end;
  stRegistry :
    begin
      if EnCoded then
        FRegistry.WriteString(SettingName,EnCode(VarToStr(SettingValue)))
      else
        FRegistry.WriteString(SettingName,SettingValue);
    end;
  stDataBase :
    begin
      if  not FStorageOptions.SettingsDataSet.Active then
        try
          FStorageOptions.SettingsDataSet.Active := True;
        except
          raise;
        end;
      if FStorageOptions.SettingsDataSet.Locate(FStorageOptions.SettingNameField,SettingName,[]) then
      begin
        FStorageOptions.SettingsDataSet.Edit;
        FStorageOptions.SettingsDataSet.FieldByName(FStorageOptions.SettingNameField).Value  := SettingName;
        if EnCoded then
          FStorageOptions.SettingsDataSet.FieldByName(FStorageOptions.SettingValueField).Value := EnCode(VarToStr(SettingValue))
        else
          FStorageOptions.SettingsDataSet.FieldByName(FStorageOptions.SettingValueField).Value := SettingValue;
        FStorageOptions.SettingsDataSet.Post;
      end
      else
      begin
        FStorageOptions.SettingsDataSet.Insert;
        FStorageOptions.SettingsDataSet.FieldByName(FStorageOptions.SettingNameField).Value  := SettingName;
        if EnCoded then
          FStorageOptions.SettingsDataSet.FieldByName(FStorageOptions.SettingValueField).Value := EnCode(VarToStr(SettingValue))
        else
          FStorageOptions.SettingsDataSet.FieldByName(FStorageOptions.SettingValueField).Value := SettingValue;
        FStorageOptions.SettingsDataSet.Post;
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TgtSettingsManager.LoadSetting(SettingName: string;DefaultValue: Variant;EnCoded:Boolean;IniSection:string): Variant;
var
  Temp : string;
begin
  case SettingsType of
  stNone     :
    begin
      Result := null;
    end;
  stIni      :
    begin
      Temp := VarToStr(DefaultValue);
      if EnCoded then
        Result :=  Decode(FIniFile.ReadString(IniSection,SettingName,Temp))
      else
        Result :=  FIniFile.ReadString(IniSection,SettingName,Temp);
      if VarIsNull(Result) or VarIsEmpty(Result) or (Trim(VarToStr(Result))='') then
        Result := Temp;
    end;
  stRegistry :
    begin
      if EnCoded then
        Result := Decode(FRegistry.ReadString(SettingName))
      else
        Result := FRegistry.ReadString(SettingName);
      if VarIsNull(Result) or VarIsEmpty(Result) or (Trim(VarToStr(Result))='') then
        Result := DefaultValue;
    end;
  stDataBase :
    begin
      if  not FStorageOptions.SettingsDataSet.Active then
        try
          FStorageOptions.SettingsDataSet.Active := True;
        except
          raise;
        end;
      if FStorageOptions.SettingsDataSet.Locate(FStorageOptions.SettingNameField,SettingName,[]) then
      begin
        Result := FStorageOptions.SettingsDataSet.FieldByName(FStorageOptions.SettingValueField).Value;
      end
      else
        Result := null;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtSettingsManager.AddSettingsComponent(ASettingsComponent: TComponent);
begin
  if Assigned(ASettingsComponent) and (Trim(ASettingsComponent.Name)<>'') then
  begin
    if FSettingComponents.IndexOf(ASettingsComponent) = -1 then
      FSettingComponents.Add(ASettingsComponent);
  end
  else
    raise Exception.Create(COMPONENT_IS_NOT_VALID);
end;
{------------------------------------------------------------------------------}
procedure TgtSettingsManager.RemoveSettingsComponent(ASettingsComponent: TComponent);
begin
  FSettingComponents.Remove(ASettingsComponent);
end;
{------------------------------------------------------------------------------}
procedure TgtSettingsManager.LoadSettings;
var
  i :Integer;
begin
  Prepare;
  if Assigned(FOnBeforeLoadData) then
    FOnBeforeLoadData(Self);
  for i:= 0 to Pred(FSettingComponents.Count) do
  begin
    TgtCustomSettingControl(FSettingComponents[i]).SettingValue :=
      LoadSetting(FSettingComponents[i].Name,null
                 ,TgtCustomSettingControl(FSettingComponents[i]).EnCoded
                 ,TgtCustomSettingControl(FSettingComponents[i]).IniSection);
  end;
  if Assigned(FOnAfterLoadData) then
    FOnAfterLoadData(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtSettingsManager.SaveSettings;
var
  i : Integer;
begin
  Prepare;
  if Assigned(FOnBeforeSaveData) then
    FOnBeforeSaveData(Self);
  for i := 0 to Pred(FSettingComponents.Count) do
    SaveSetting(FSettingComponents[i].Name
               ,TgtCustomSettingControl(FSettingComponents[i]).SettingValue
               ,TgtCustomSettingControl(FSettingComponents[i]).EnCoded
               ,TgtCustomSettingControl(FSettingComponents[i]).IniSection);
  if Assigned(FOnAfterSaveData) then
    FOnAfterSaveData(Self)
end;
{------------------------------------------------------------------------------}
function TgtSettingsManager.GetValue(SettingName: string): Variant;
var
  i : Integer;
begin
  Result := null;
  for i := 0 to Pred(FSettingComponents.Count) do
  begin
    if SameText(SettingName,FSettingComponents[i].Name) then
    begin
      Result := TgtCustomSettingControl(FSettingComponents[i]).SettingValue;
      Break;
    end;
  end;

end;
{------------------------------------------------------------------------------}




{ TgtSettingsManagerStorageOptions }
{------------------------------------------------------------------------------}
constructor TgtSettingsManagerStorageOptions.Create(SettingsManager: TgtSettingsManager);
begin
  inherited Create;
  FSettingsManager := SettingsManager;
  FIniSections     := TStringList.Create;
end;
{------------------------------------------------------------------------------}
destructor TgtSettingsManagerStorageOptions.Destroy;
begin
  FreeAndNil(FIniSections);
  inherited;
end;
{------------------------------------------------------------------------------}



//Getters - Setters
{------------------------------------------------------------------------------}
procedure TgtSettingsManagerStorageOptions.SetIniSections(const Value: TStrings);
begin
  FIniSections.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TgtSettingsManagerStorageOptions.SetSettingsDataSet(const Value: TDataSet);
begin
  if Assigned(FSettingsDataSet) then
    FSettingsManager.RemoveFreeNotification(FSettingsDataSet);

  FSettingsDataSet := Value;

  if Assigned(FSettingsDataSet) then
    FSettingsManager.FreeNotification(FSettingsDataSet);
end;
{------------------------------------------------------------------------------}


type
  _TControl = class(TControl);

 { TgtCustomSettingControl }
{------------------------------------------------------------------------------}
constructor TgtCustomSettingControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont  := TFont.Create;
  Width  := 121;
  Height := 21;
end;
{------------------------------------------------------------------------------}
destructor TgtCustomSettingControl.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomSettingControl.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = FSettingsManager  then SettingsManager := nil;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtCustomSettingControl.CreateControl(ControlClass: TControlClass);
begin
  FControl            := ControlClass.Create(Self);
  FControl.Parent     := Self;
  FControl.Visible    := True;
  FControl.Align      := alClient;
  Color               := _TControl(FControl).Color;
end;
{------------------------------------------------------------------------------}
function TgtCustomSettingControl.GetSettingValue: Variant;
begin
end;
{------------------------------------------------------------------------------}
procedure TgtCustomSettingControl.SetSetttingValue(const Value: Variant);
begin
  DoSetSettingValue(Value);
  if Assigned(FOnValueChanged) then
    FOnValueChanged(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtCustomSettingControl.DoSetSettingValue(const Value: Variant);
begin
end;
{------------------------------------------------------------------------------}
procedure TgtCustomSettingControl.SetSettingsManager(const Value: TgtSettingsManager);
begin
  if Assigned(FSettingsManager) then
  begin
    FSettingsManager.RemoveFreeNotification(Self);
    FSettingsManager.RemoveSettingsComponent(Self);
  end;

  FSettingsManager := Value;

  if Assigned(FSettingsManager) then
  begin
    FSettingsManager.FreeNotification(Self);
    FSettingsManager.AddSettingsComponent(Self);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomSettingControl.SetCaption(const Value: string);
begin
  FCaption := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomSettingControl.SetColor(const Value: TColor);
begin
  FColor := Value;
  _TControl(FControl).Color := FColor;
end;
{------------------------------------------------------------------------------}
procedure TgtCustomSettingControl.SetFont(const Value: TFont);
begin
  FFont := Value;
  _TControl(FControl).Font.Assign(FFont);
end;
{------------------------------------------------------------------------------}


{ TgtCSCEdit }
{------------------------------------------------------------------------------}
constructor TgtCSCEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControl(TEdit);
end;
{------------------------------------------------------------------------------}
destructor TgtCSCEdit.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
function TgtCSCEdit.GetSettingValue: Variant;
begin
  Result := TEdit(FControl).Text;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCEdit.SetText(const Value: string);
begin
  FText := Value;
  DoSetSettingValue(FText);
end;
{------------------------------------------------------------------------------}
procedure TgtCSCEdit.DoSetSettingValue(const Value: Variant);
begin
  if not VarIsNull(Value) then
    TEdit(FControl).Text := Value
  else
    TEdit(FControl).Text := '';
end;
{------------------------------------------------------------------------------}

{ TgtCSCLabeledEdit }
{------------------------------------------------------------------------------}
constructor TgtCSCLabeledEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControl(TLabeledEdit);
end;
{------------------------------------------------------------------------------}
destructor TgtCSCLabeledEdit.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCLabeledEdit.DoSetSettingValue(const Value: Variant);
begin
  if not VarIsNull(Value) then
    TLabeledEdit(FControl).Text := VarToStr(Value)
  else
    TLabeledEdit(FControl).Text := '';
end;
{------------------------------------------------------------------------------}
function TgtCSCLabeledEdit.GetSettingValue: Variant;
begin
   Result := TLabeledEdit(FControl).Text;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCLabeledEdit.SetCaption(const Value: string);
begin
  FCaption := VarToStr(Value);
  TLabeledEdit(FControl).EditLabel.Caption := FCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCLabeledEdit.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  TLabeledEdit(FControl).LabelPosition := ExtCtrls.TLabelPosition(FLabelPosition);
end;
{------------------------------------------------------------------------------}
procedure TgtCSCLabeledEdit.SetText(const Value: string);
begin
  FText := Value;
  DoSetSettingValue(FText);
end;
{------------------------------------------------------------------------------}




{ TgtCSCCheckBox }
{------------------------------------------------------------------------------}
constructor TgtCSCCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControl(TCheckBox);
  Color := clBtnFace;
end;
{------------------------------------------------------------------------------}
destructor TgtCSCCheckBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
function TgtCSCCheckBox.GetSettingValue: Variant;
begin
  Result := TCheckBox(FControl).Checked;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCCheckBox.DoSetSettingValue(const Value: Variant);
begin
  if not VarIsNull(Value) then
  begin
    if Trim(VarToStr(Value)) <> '' then
      TCheckBox(FControl).Checked := Value
  end
  else
    TCheckBox(FControl).Checked := False;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCCheckBox.SetCaption(const Value: string);
begin
  inherited SetCaption(Value);
  TCheckBox(FControl).Caption := Value;
end;
{------------------------------------------------------------------------------}

{ TgtCSCRadioButton }
{------------------------------------------------------------------------------}
constructor TgtCSCRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControl(TRadioButton);
  Color := clBtnFace;
end;
{------------------------------------------------------------------------------}
destructor TgtCSCRadioButton.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCRadioButton.DoSetSettingValue(const Value: Variant);
begin
  if not VarIsNull(Value) then
  begin
    if Trim(VarToStr(Value)) <> '' then
      TRadioButton(FControl).Checked := Value
  end
  else
    TRadioButton(FControl).Checked := False;
end;
{------------------------------------------------------------------------------}
function TgtCSCRadioButton.GetSettingValue: Variant;
begin
  Result := TRadioButton(FControl).Checked;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCRadioButton.SetCaption(const Value: string);
begin
  inherited SetCaption(Value);
  TRadioButton(FControl).Caption := Value;
end;
{------------------------------------------------------------------------------}



{ TgtCSCComboBox }
{------------------------------------------------------------------------------}
constructor TgtCSCComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControl(TComboBox);
  Self.Constraints.MaxHeight := Height;
  Self.Constraints.MinHeight := Height;
end;
{------------------------------------------------------------------------------}
destructor TgtCSCComboBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
function TgtCSCComboBox.GetSettingValue: Variant;
begin
  Result := TComboBox(FControl).ItemIndex;
end;
{------------------------------------------------------------------------------}
function TgtCSCComboBox.GetItems: TStrings;
begin
  Result := TComboBox(FControl).Items;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCComboBox.SetItems(const Value: TStrings);
begin
  TComboBox(FControl).Items.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TgtCSCComboBox.DoSetSettingValue(const Value: Variant);
begin
  if not VarIsNull(Value) then
  begin
    if Trim(VarToStr(Value)) <> '' then
      if Value <= TComboBox(FControl).Items.Count then
        TComboBox(FControl).ItemIndex := Value;
  end
  else
  begin
    TComboBox(FControl).ItemIndex := -1;
  end;
end;
{------------------------------------------------------------------------------}
function TgtCSCComboBox.GetSelectedIndex: Integer;
begin
  Result := TComboBox(FControl).ItemIndex;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCComboBox.SetSelectedIndex(const Value: Integer);
begin
  if Value = -1 then
    TComboBox(FControl).ItemIndex := Value
  else
  begin
    if Value <= TComboBox(FControl).Items.Count then
      TComboBox(FControl).ItemIndex := Value;
  end;
end;
{------------------------------------------------------------------------------}


{ TgtCSCListBox }
{------------------------------------------------------------------------------}
constructor TgtCSCListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControl(TListBox);
  Height := 120;
  Width  := 100;
end;
{------------------------------------------------------------------------------}
destructor TgtCSCListBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCListBox.DoSetSettingValue(const Value: Variant);
begin
  if not VarIsNull(Value) then
  begin
    TListBox(FControl).Items.CommaText := VarToStr(Value);
  end
  else
    TListBox(FControl).Items.Clear;
end;
{------------------------------------------------------------------------------}
function TgtCSCListBox.GetSettingValue: Variant;
begin
  Result := TListBox(FControl).Items.CommaText;
end;
{------------------------------------------------------------------------------}
function TgtCSCListBox.GetItems: TStrings;
begin
  Result := TListBox(FControl).Items
end;
{------------------------------------------------------------------------------}
procedure TgtCSCListBox.SetItems(const Value: TStrings);
begin
  TListBox(FControl).Items.Assign(Value);
end;
{------------------------------------------------------------------------------}
function TgtCSCListBox.GetSelectedIndex: Integer;
begin
  Result := TListBox(FControl).ItemIndex;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCListBox.SetSelectedIndex(const Value: Integer);
begin
  if Value = -1 then
    TListBox(FControl).ItemIndex := Value
  else
  begin
    if Value <= TListBox(FControl).Items.Count then
      TListBox(FControl).ItemIndex := Value;
  end;
end;
{------------------------------------------------------------------------------}





{ TgtCSCMemo }
{------------------------------------------------------------------------------}
constructor TgtCSCMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControl(TMemo);
  Height := 120;
  Width  := 100;
end;
{------------------------------------------------------------------------------}
destructor TgtCSCMemo.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCMemo.DoSetSettingValue(const Value: Variant);
begin
  if not VarIsNull(Value) then
  begin
    TMemo(FControl).Lines.CommaText := VarToStr(Value);
  end
  else
    TMemo(FControl).Lines.Clear;
end;
{------------------------------------------------------------------------------}
function TgtCSCMemo.GetSettingValue: Variant;
begin
  Result := TMemo(FControl).Lines.CommaText;
end;
{------------------------------------------------------------------------------}
function TgtCSCMemo.GetItems: TStrings;
begin
  Result := TMemo(FControl).Lines
end;
{------------------------------------------------------------------------------}
procedure TgtCSCMemo.SetItems(const Value: TStrings);
begin
  TMemo(FControl).Lines.Assign(Value);
end;
{------------------------------------------------------------------------------}

{ TgtCSCDateTimePicker }
{------------------------------------------------------------------------------}
constructor TgtCSCDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControl(TDateTimePicker);
end;
{------------------------------------------------------------------------------}
destructor TgtCSCDateTimePicker.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCDateTimePicker.DoSetSettingValue(const Value: Variant);
begin
  if not VarIsNull(Value) then
    TDateTimePicker(FControl).DateTime := VarToDateTime(Value)
  else
    TDateTimePicker(FControl).DateTime := 0;
end;
{------------------------------------------------------------------------------}
function TgtCSCDateTimePicker.GetSettingValue: Variant;
begin
  Result := VarFromDateTime(TDateTimePicker(FControl).DateTime);
end;
{------------------------------------------------------------------------------}
procedure TgtCSCDateTimePicker.SetChecked(const Value: Boolean);
begin
  FChecked := Value;
  TDateTimePicker(FControl).Checked := FChecked;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCDateTimePicker.SetDateFormat(const Value: TDTDateFormat);
begin
  FDateFormat := Value;
  TDateTimePicker(FControl).DateFormat := FDateFormat;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCDateTimePicker.SetDateTimeKind(const Value: TDateTimeKind);
begin
  FDateTimeKind := Value;
  TDateTimePicker(FControl).Kind := FDateTimeKind;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCDateTimePicker.SetShowCheckBox(const Value: Boolean);
begin
  FShowCheckBox := Value;
  TDateTimePicker(FControl).ShowCheckbox := FShowCheckBox;
end;
{------------------------------------------------------------------------------}


{ TgtCSCTrackBar }
{------------------------------------------------------------------------------}
constructor TgtCSCTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControl(TTrackBar);
  Color := clBtnFace;
end;
{------------------------------------------------------------------------------}
destructor TgtCSCTrackBar.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCTrackBar.DoSetSettingValue(const Value: Variant);
begin
  if not VarIsNull(Value) then
    TTrackBar(FControl).Position := Value
  else
    TTrackBar(FControl).Position := 0;
end;
{------------------------------------------------------------------------------}
function TgtCSCTrackBar.GetSettingValue: Variant;
begin
  Result := TTrackBar(FControl).Position;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCTrackBar.SetOrientation(const Value: TTrackBarOrientation);
begin
  FOrientation := Value;
  TTrackBar(FControl).Orientation := FOrientation;
end;
{------------------------------------------------------------------------------}



{ TgtCSCRadioGroup }
{------------------------------------------------------------------------------}
constructor TgtCSCRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControl(TRadioGroup);
  Height := 120;
  Width  := 100;
  Color := clBtnFace;
end;
{------------------------------------------------------------------------------}
destructor TgtCSCRadioGroup.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCRadioGroup.DoSetSettingValue(const Value: Variant);
begin
  if not VarIsNull(Value) then
  begin
    if Trim(VarToStr(Value)) <> '' then
      if Value <= TRadioGroup(FControl).Items.Count then
        TRadioGroup(FControl).ItemIndex := Value;
  end
  else
  begin
    TRadioGroup(FControl).ItemIndex := -1;
  end;
end;
{------------------------------------------------------------------------------}
function TgtCSCRadioGroup.GetItems: TStrings;
begin
  Result := TRadioGroup(FControl).Items;
end;
{------------------------------------------------------------------------------}
function TgtCSCRadioGroup.GetSelectedIndex: Integer;
begin
  Result := TRadioGroup(FControl).ItemIndex;
end;
{------------------------------------------------------------------------------}
function TgtCSCRadioGroup.GetSettingValue: Variant;
begin
  Result := TRadioGroup(FControl).ItemIndex;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCRadioGroup.SetCaption(const Value: string);
begin
  TRadioGroup(FControl).Caption := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCRadioGroup.SetItems(const Value: TStrings);
begin
  TRadioGroup(FControl).Items.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TgtCSCRadioGroup.SetSelectedIndex(const Value: Integer);
begin
  if Value = -1 then
    TRadioGroup(FControl).ItemIndex := Value
  else
  begin
    if Value <= TRadioGroup(FControl).Items.Count then
      TRadioGroup(FControl).ItemIndex := Value;
  end;
end;
{------------------------------------------------------------------------------}





{ TgtCSCColorBox }
{------------------------------------------------------------------------------}
constructor TgtCSCColorBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControl(TColorBox);
end;
{------------------------------------------------------------------------------}
destructor TgtCSCColorBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCColorBox.DoSetSettingValue(const Value: Variant);
begin
  if not VarIsNull(Value) then
  begin
    if Trim(VarToStr(Value)) <> '' then
      if Value <= TColorBox(FControl).Items.Count then
        TColorBox(FControl).ItemIndex := Value;
  end
  else
  begin
    TColorBox(FControl).ItemIndex := -1;
  end;
end;
{------------------------------------------------------------------------------}
function TgtCSCColorBox.GetColor(Index: Integer): TColor;
begin
  Result := TColorBox(FControl).Colors[Index];
end;
{------------------------------------------------------------------------------}
function TgtCSCColorBox.GetColorName(Index: Integer): string;
begin
  Result := TColorBox(FControl).ColorNames[Index];
end;
{------------------------------------------------------------------------------}
function TgtCSCColorBox.GetSelected: TColor;
begin
  Result := TColorBox(FControl).Selected;
end;
{------------------------------------------------------------------------------}
function TgtCSCColorBox.GetSelectedIndex: Integer;
begin
  Result := TColorBox(FControl).ItemIndex;
end;
{------------------------------------------------------------------------------}
function TgtCSCColorBox.GetSettingValue: Variant;
begin
  Result := TColorBox(FControl).ItemIndex;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCColorBox.SetDefaultColorColor(const Value: TColor);
begin
  FDefaultColorColor := Value;
  TColorBox(FControl).DefaultColorColor := FDefaultColorColor;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCColorBox.SetNoneColorColor(const Value: TColor);
begin
  FNoneColorColor := Value;
  TColorBox(FControl).NoneColorColor := FNoneColorColor;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCColorBox.SetSelected(const Value: TColor);
begin
  TColorBox(FControl).Selected := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCColorBox.SetSelectedIndex(const Value: Integer);
begin
  if Value = -1 then
    TColorBox(FControl).ItemIndex := Value
  else
  begin
    if Value <= TColorBox(FControl).Items.Count then
      TColorBox(FControl).ItemIndex := Value;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtCSCColorBox.SetStyle(const Value: TColorBoxStyle);
begin
  FStyle := Value;
  TColorBox(FControl).Style := FStyle;
end;
{------------------------------------------------------------------------------}





initialization

finalization
  if Assigned(_SettingsManager) then
    FreeAndNil(_SettingsManager);















end.
