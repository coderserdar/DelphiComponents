{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         General non-visual components                 }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{         TStoredValue, TStoredValues                   }
{         TPropStorage (portion of TFormStorage)        }
{         Copyright (c) 1995, 1997 RX Library           }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgStndrt;

interface
uses Classes{$IFDEF _D4_}, IniFiles, vgTools{$ENDIF}, Forms;

type
{ TLocaleStorage }
  TLocaleStorage = class(TComponent)
  private
    FSysDefault: Boolean;                
    FText: string;
    procedure SetSysDefault(Value: Boolean);
  protected
    function IsStored: Boolean;
    procedure Loaded; override;
    procedure DefaultChanged;
    procedure SetText(Value: string);
    procedure UpdateText; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Read; virtual;
    procedure Write; virtual;
  published
    property SysDefault: Boolean read FSysDefault write SetSysDefault default True;
    property Text: string read FText write SetText stored False;
  end;

{ TDateTimeStorage }
  TDateTimeStorage = class(TLocaleStorage)
  private
    FDate: string;
    FTime: string;
    FShortDateFormat: string;
    FLongDateFormat: string;
    FDateSeparator: Char;
    FTimeSeparator: Char;
    FTimeAMString: string;
    FTimePMString: string;
    FShortTimeFormat: string;
    FLongTimeFormat: string;
    FShortMonthNames: TStrings;
    FLongMonthNames: TStrings;
    FShortDayNames: TStrings;
    FLongDayNames: TStrings;
    FDateTime: Single;
    procedure SetDateTime(Value: Single);
    procedure SetDate(Value: string);
    procedure SetTime(Value: string);
    procedure SetShortDateFormat(Value: string);
    procedure SetLongDateFormat(Value: string);
    procedure SetDateSeparator(Value: Char);
    procedure SetTimeSeparator(Value: Char);
    procedure SetTimeAMString(Value: string);
    procedure SetTimePMString(Value: string);
    procedure SetShortTimeFormat(Value: string);
    procedure SetLongTimeFormat(Value: string);
    procedure SetShortMonthNames(Value: TStrings);
    procedure SetLongMonthNames(Value: TStrings);
    procedure SetShortDayNames(Value: TStrings);
    procedure SetLongDayNames(Value: TStrings);
    procedure StringsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Read; override;
    procedure Write; override;
    procedure UpdateText; override;
  published
    property ShortDateFormat: string read FShortDateFormat write SetShortDateFormat stored IsStored;
    property LongDateFormat: string read FLongDateFormat write SetLongDateFormat stored IsStored;
    property DateSeparator: Char read FDateSeparator write SetDateSeparator stored IsStored;
    property TimeSeparator: Char read FTimeSeparator write SetTimeSeparator stored IsStored;
    property TimeAMString: string read FTimeAMString write SetTimeAMString stored IsStored;
    property TimePMString: string read FTimePMString write SetTimePMString stored IsStored;
    property ShortTimeFormat: string read FShortTimeFormat write SetShortTimeFormat stored IsStored;
    property LongTimeFormat: string read FLongTimeFormat write SetLongTimeFormat stored IsStored;

    property ShortMonthNames: TStrings read FShortMonthNames write SetShortMonthNames stored IsStored;
    property LongMonthNames: TStrings read FLongMonthNames write SetLongMonthNames stored IsStored;
    property ShortDayNames: TStrings read FShortDayNames write SetShortDayNames stored IsStored;
    property LongDayNames: TStrings read FLongDayNames write SetLongDayNames stored IsStored;

    property Value: Single read FDateTime write SetDateTime stored False;
    property TextDate: string read FDate write SetDate stored False;
    property TextTime: string read FTime write SetTime stored False;
  end;

{ TCurrencyStorage }
  TCurrencyStorage = class(TLocaleStorage)
  private
    { Private declarations }
    FCurrencyString: string;
    FCurrencyFormat: Byte;
    FNegCurrFormat: Byte;
    FThousandSeparator: Char;
    FDecimalSeparator: Char;
    FCurrencyDecimals: Byte;
    FValue: Single;
    procedure SetCurrencyString(Value: string);
    procedure SetCurrencyFormat(Value: Byte);
    procedure SetNegCurrFormat(Value: Byte);
    procedure SetThousandSeparator(Value: Char);
    procedure SetDecimalSeparator(Value: Char);
    procedure SetCurrencyDecimals(Value: Byte);
    procedure SetValue(Value: Single);
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure IncorrectValue;
    procedure UpdateText; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function GetText(
      ACurrencyString: string;
      ACurrencyFormat: Byte;
      ANegCurrFormat: Byte;
      AThousandSeparator: Char;
      ADecimalSeparator: Char;
      ACurrencyDecimals: Byte;
      AValue: Single): string;
    function ValueToStr(Value: Single): string;
    procedure Read; override;
    procedure Write; override;
  published
    { Published declarations }
    property CurrencyString: string read FCurrencyString write SetCurrencyString stored IsStored;
    property CurrencyFormat: Byte read FCurrencyFormat write SetCurrencyFormat stored IsStored;
    property NegCurrFormat: Byte read FNegCurrFormat write SetNegCurrFormat stored IsStored;
    property ThousandSeparator: Char read FThousandSeparator write SetThousandSeparator stored IsStored;
    property DecimalSeparator: Char read FDecimalSeparator write SetDecimalSeparator stored IsStored;
    property CurrencyDecimals: Byte read FCurrencyDecimals write SetCurrencyDecimals stored IsStored;
    property Value: Single read FValue write SetValue stored False;
  end;

{ TMoneyString }
  TStringResource = (srCurrency, srCurrencySub, srMaleOne, srFemaleOne,
    srTen, srFirstTen, srHundred, srThousand, srMillion, srBillion);

  TMoneyFormat  = (mfVerbal, mfDigit);


  TMoneyInteger = {$IFDEF _D4_}Int64{$ELSE}Integer{$ENDIF};

  TMoneyString = class(TComponent)
  private
    FCurrencyGender: array [0..1] of Boolean;
    FDefault: Boolean;
    FDelimeter: string;
    FLists: array [TStringResource] of TStrings;
    FFormats: array [0..1] of TMoneyFormat;
    FUpperStart: Boolean;
    FValueNumber: Currency;
    FValueString: string;
    FZero: string;
    FZeroEmpty: array [0..1] of Boolean;
    function GetString(Strings: TStrings; Index: Integer): string;
    procedure ListChanged(Sender: TObject);
    function ListStored(Index: Integer): Boolean;
    procedure SetCurrencyGender(Index: Integer; Value: Boolean);
    procedure SetDefault(Value: Boolean);
    procedure SetDelimeter(Value: string);
    procedure SetFormat(Index: Integer; Value: TMoneyFormat);
    procedure SetList(Index: Integer; Value: TStrings);
    procedure SetUpperStart(Value: Boolean);
    procedure SetValueNumber(Value: Currency);
    procedure SetValueString(Value: string);
    procedure SetZero(Value: string);
    procedure SetZeroEmpty(Index: Integer; Value: Boolean);
    function ZeroStored: Boolean;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    function IntegerCurrencyStr(Value: TMoneyInteger; Currency: TStrings): string;
    function IntegerGenderedToString(Value: TMoneyInteger; MaleGender: Boolean): string;
    function IntegerToString(Value: TMoneyInteger): string;
    function MoneyToString(Value: Currency): string;
    function Triada(Value: Integer; MaleGender: Boolean): string;
  published
    property Billion: TStrings index Integer(srBillion) read FLists[srBillion] write SetList stored ListStored;
    property CurrencySub: TStrings index Integer(srCurrencySub) read FLists[srCurrencySub] write SetList stored ListStored;
    property Currency: TStrings index Integer(srCurrency) read FLists[srCurrency] write SetList stored ListStored;
    property Default: Boolean read FDefault write SetDefault stored False default True;
    property Delimeter: string read FDelimeter write SetDelimeter;
    property FemaleOne: TStrings index Integer(srFemaleOne) read FLists[srFemaleOne] write SetList stored ListStored;
    property FirstTen: TStrings index Integer(srFirstTen) read FLists[srFirstTen] write SetList stored ListStored;
    property FormatCurrency: TMoneyFormat index 0 read FFormats[0] write SetFormat default mfVerbal;
    property FormatCurrencySub: TMoneyFormat index 1 read FFormats[1] write SetFormat default mfVerbal;
    property Hundred: TStrings index Integer(srHundred) read FLists[srHundred] write SetList stored ListStored;
    property MaleCurrency: Boolean index 0 read FCurrencyGender[0] write SetCurrencyGender default True;
    property MaleCurrencySub: Boolean index 1 read FCurrencyGender[1] write SetCurrencyGender;
    property MaleOne: TStrings index Integer(srMaleOne) read FLists[srMaleOne] write SetList stored ListStored;
    property Thousand: TStrings index Integer(srThousand) read FLists[srThousand] write SetList stored ListStored;
    property Million: TStrings index Integer(srMillion) read FLists[srMillion] write SetList stored ListStored;
    property Ten: TStrings index Integer(srTen) read FLists[srTen] write SetList stored ListStored;
    property UpperStart: Boolean read FUpperStart write SetUpperStart default True;
    property ValueNumber: Currency read FValueNumber write SetValueNumber stored False;
    property ValueString: string read FValueString write SetValueString stored False;
    property Zero: string read FZero write SetZero stored ZeroStored;
    property ZeroEmptyCurrency: Boolean index 0 read FZeroEmpty[0] write SetZeroEmpty default True;
    property ZeroEmptyCurrencySub: Boolean index 1 read FZeroEmpty[1] write SetZeroEmpty default True;
  end;

  TIniFileType = (ftIniFile, ftMemIniFile, ftRegIniFile, ftUserDefined);

  TRegistryRoot = (rtCurrentUser, rtLocalMachine, rtCurrentConfig,
    rtClassesRoot, rtUsers, rtDynData);

{$IFDEF _D4_}
  TCreateIniFileEvent = procedure (Sender: TObject;
    var IniFile: TCustomIniFile) of object;

  TIniFileLink = class;

{ TAppIniFile }
  TAppIniFile = class(TComponent)
  private
    FIniFile: TCustomIniFile;
    FIniFileName: string;
    FIniFileType: TIniFileType;
    FLinks: TList;
    FReadCount, FWriteCount: Integer;
    FRegistryRoot: TRegistryRoot;
    FOnCreateIniFile: TCreateIniFileEvent;
    procedure CheckIniFile;
    procedure SetIniFileType(Value: TIniFileType);
    procedure SetIniFileName(Value: string);
    procedure SetRegistryRoot(Value: TRegistryRoot);
  protected
    procedure DoIniCreate;
    procedure DoIniDestroy;
    procedure DoLoadLinks;
    procedure DoSaveLinks;
    function CreateIniFile: TCustomIniFile; virtual;
    function GetIniFileName: string; virtual;
    procedure DestroyIniFile;
    procedure InternalLoad; virtual;
    procedure InternalSave; virtual;
    procedure InsertIniFileLink(ALink: TIniFileLink);
    procedure RemoveIniFileLink(ALink: TIniFileLink);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BeginRead: Boolean;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
    procedure Load;
    procedure Save;
    property IniFile: TCustomIniFile read FIniFile;
  published
    property IniFileName: string read GetIniFileName write SetIniFileName;
    property IniFileType: TIniFileType read FIniFileType write SetIniFileType default ftRegIniFile;
    property RegistryRoot: TRegistryRoot read FRegistryRoot write SetRegistryRoot default rtCurrentUser;
    property OnCreateIniFile: TCreateIniFileEvent read FOnCreateIniFile write FOnCreateIniFile;
  end;

{ TIniFileLink }
  TIniFileLink = class
  private
    FActive: Boolean;
    FAppSection: string;
    FAppIniFile: TAppIniFile;
    FOnIniCreate, FOnIniDestroy, FOnLoad, FOnSave: TNotifyEvent;
    procedure SetAppIniFile(Value: TAppIniFile);
    function GetIniFile: TCustomIniFile;
  protected
    procedure InternalIniCreate; virtual;
    procedure InternalIniDestroy; virtual;
    procedure InternalLoad; virtual;
    procedure InternalSave; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IniCreate;
    procedure IniDestroy;
    procedure Load;
    procedure Save;
    property IniFile: TCustomIniFile read GetIniFile;
    property Active: Boolean read FActive write FActive;
    property AppIniFile: TAppIniFile read FAppIniFile write SetAppIniFile;
    property AppSection: string read FAppSection write FAppSection;
    property OnIniCreate: TNotifyEvent read FOnIniCreate write FOnIniCreate;
    property OnIniDestroy: TNotifyEvent read FOnIniDestroy write FOnIniDestroy;
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
  end;

  TStoredValue = class;
  TStoredValues = class;

  TStoredValueEvent = procedure(Sender: TStoredValue; var Value: Variant) of object;

{ TStoredValue }
  TStoredValue = class(TNamedCollectionItem)
  private
    FValue: Variant;
    FOnLoad, FOnSave: TStoredValueEvent;
    function IsValueStored: Boolean;
    function GetStoredValues: TStoredValues;
  protected
    procedure InternalLoad; virtual;
    procedure InternalSave; virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Save;
    procedure Load;
    property StoredValues: TStoredValues read GetStoredValues;
  published
    property Value: Variant read FValue write FValue stored IsValueStored;
    property OnLoad: TStoredValueEvent read FOnLoad write FOnLoad;
    property OnSave: TStoredValueEvent read FOnSave write FOnSave;
  end;

  TPropStorage = class;

{ TStoredValues }
  TStoredValues = class(TNamedItemsCollection)
  private
    function GetValue(const Name: string): TStoredValue;
    procedure SetValue(const Name: string; StoredValue: TStoredValue);
    function GetItem(Index: Integer): TStoredValue;
    procedure SetItem(Index: Integer; StoredValue: TStoredValue);
    function GetPropStorage: TPropStorage;
  public
    constructor Create(AOwner: TPersistent);
    procedure SaveValues; virtual;
    procedure LoadValues; virtual;
    property Items[Index: Integer]: TStoredValue read GetItem write SetItem; default;
    property Values[const Name: string]: TStoredValue read GetValue write SetValue;
    property PropStorage: TPropStorage read GetPropStorage;
  end;

{ TPropStorage }
  TPropStorage = class(TComponent)
  private
    FIniFileLink: TIniFileLink;
{$IFDEF _D5_}
    FMasterStorage: TPropStorage;
    FStorages: TList;
{$ENDIF}
    FStoredProps: TStrings;
    FVersion: Integer;
    FSaved: Boolean;
    FDestroying: Boolean;
    FSaveOnShow: TNotifyEvent;
    FSaveOnDestroy: TNotifyEvent;
    FSaveOnCloseQuery: TCloseQueryEvent;
    FOnLoad, FOnSave: TNotifyEvent;
    FStoredValues: TStoredValues;
    procedure DoLoadProps;
    procedure DoSaveProps;
    function GetActive: Boolean;
    function GetAppIniFile: TAppIniFile;
    function GetAppSection: string;
    function GetStoredProps: TStrings;
    procedure IniLoad(Sender: TObject);
    procedure IniSave(Sender: TObject);
    procedure SetActive(Value: Boolean);
    procedure SetAppIniFile(Value: TAppIniFile);
    procedure SetAppSection(Value: string);
    procedure SetStoredProps(Value: TStrings);
    procedure SetStoredValues(Value: TStoredValues);
    procedure StoredPropsChanged(Sender: TObject);
    procedure HookEvents;
    procedure UnhookEvents;
    procedure DoOnShow(Sender: TObject);
    procedure DoOnCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DoOnDestroy(Sender: TObject);
{$IFDEF _D5_}
    procedure DoLoadLinkedStorages;
    procedure DoSaveLinkedStorages;
    function IsLinkedTo(Storage: TPropStorage): Boolean;
    procedure InsertStorage(Value: TPropStorage);
    procedure RemoveStorage(Value: TPropStorage);
    procedure SetMasterStorage(Value: TPropStorage);
{$ENDIF}
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InternalLoad; virtual;
    procedure InternalSave; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  published
    property Active: Boolean read GetActive write SetActive default True;
    property AppIniFile: TAppIniFile read GetAppIniFile write SetAppIniFile;
    property AppSection: string read GetAppSection write SetAppSection;
{$IFDEF _D5_}
    property MasterStorage: TPropStorage read FMasterStorage write SetMasterStorage;
{$ENDIF}
    property StoredProps: TStrings read GetStoredProps write SetStoredProps;
    property StoredValues: TStoredValues read FStoredValues write SetStoredValues;
    property Version: Integer read FVersion write FVersion default 0;
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
  end;
{$ENDIF}


implementation
uses Windows, SysUtils, vgVCLRes, vgUtils, Controls {$IFDEF _D4_}, Registry, vgSystem{$ENDIF};

var
  StringResourcesDefault: array [TStringResource] of TStrings;

{ TLocaleStorage }
constructor TLocaleStorage.Create(AOwner: TComponent);
begin
  inherited;
  FSysDefault := True;
end;

procedure TLocaleStorage.DefaultChanged;
begin
  FSysDefault := False;
  UpdateText;
end;

function TLocaleStorage.IsStored: Boolean;
begin
  Result := not FSysDefault;
end;

procedure TLocaleStorage.Loaded;
begin
  inherited;
  if not FSysDefault and not (csDesigning in ComponentState) then
  begin
    Write;
    Application.UpdateFormatSettings := False;
  end;
end;

procedure TLocaleStorage.SetSysDefault(Value: Boolean);
begin
  if Value then Read;
  FSysDefault := Value;
end;

procedure TLocaleStorage.Read;
begin
  UpdateText;
end;

procedure TLocaleStorage.Write;
begin
end;

procedure TLocaleStorage.SetText(Value: string);
begin
end;

procedure TLocaleStorage.UpdateText;
begin
end;

{ TDateTimeStorage }
constructor TDateTimeStorage.Create(AOwner: TComponent);
begin
  inherited;
  FShortMonthNames := TStringList.Create;
  FLongMonthNames := TStringList.Create;
  FShortDayNames := TStringList.Create;
  FLongDayNames := TStringList.Create;
  Read;
  TStringList(FShortMonthNames).OnChange := StringsChanged;
  TStringList(FLongMonthNames).OnChange := StringsChanged;
  TStringList(FShortDayNames).OnChange := StringsChanged;
  TStringList(FLongDayNames).OnChange := StringsChanged;
  FDateTime := Now;
end;

destructor TDateTimeStorage.Destroy;
begin
  FShortMonthNames.Free;
  FLongMonthNames.Free;
  FShortDayNames.Free;
  FLongDayNames.Free;
  inherited;
end;

procedure TDateTimeStorage.Read;
begin
  FShortDateFormat := SysUtils.ShortDateFormat;
  FLongDateFormat := SysUtils.LongDateFormat;
  FDateSeparator := SysUtils.DateSeparator;
  FTimeSeparator := SysUtils.TimeSeparator;
  FTimeAMString := SysUtils.TimeAMString;
  FTimePMString := SysUtils.TimePMString;
  FShortTimeFormat := SysUtils.ShortTimeFormat;
  FLongTimeFormat := SysUtils.LongTimeFormat;
  //
  StringsAssignTo(FShortMonthNames, SysUtils.ShortMonthNames);
  StringsAssignTo(FLongMonthNames, SysUtils.LongMonthNames);
  StringsAssignTo(FShortDayNames, SysUtils.ShortDayNames);
  StringsAssignTo(FLongDayNames, SysUtils.LongDayNames);
  inherited;
end;

procedure TDateTimeStorage.Write;
begin
  SysUtils.ShortDateFormat := FShortDateFormat;
  SysUtils.LongDateFormat := FLongDateFormat;
  SysUtils.DateSeparator := FDateSeparator;
  SysUtils.TimeSeparator := FTimeSeparator;
  SysUtils.TimeAMString := FTimeAMString;
  SysUtils.TimePMString := FTimePMString;
  SysUtils.ShortTimeFormat := FShortTimeFormat;
  SysUtils.LongTimeFormat := FLongTimeFormat;
  //
  ArrayAssignTo(FShortMonthNames, SysUtils.ShortMonthNames);
  ArrayAssignTo(FLongMonthNames, SysUtils.LongMonthNames);
  ArrayAssignTo(FShortDayNames, SysUtils.ShortDayNames);
  ArrayAssignTo(FLongDayNames, SysUtils.LongDayNames);
end;

procedure TDateTimeStorage.SetDateTime(Value: Single);
begin
  FDateTime := Value;
  UpdateText;
end;

procedure TDateTimeStorage.SetDate(Value: string);
begin
end;

procedure TDateTimeStorage.SetTime(Value: string);
begin
end;

procedure TDateTimeStorage.SetShortMonthNames(Value: TStrings);
begin
  FShortMonthNames.Assign(Value);
end;

procedure TDateTimeStorage.SetLongMonthNames(Value: TStrings);
begin
  FLongMonthNames.Assign(Value);
end;

procedure TDateTimeStorage.SetShortDayNames(Value: TStrings);
begin
  FShortDayNames.Assign(Value);
end;

procedure TDateTimeStorage.SetLongDayNames(Value: TStrings);
begin
  FLongDayNames.Assign(Value);
end;

procedure TDateTimeStorage.SetShortDateFormat(Value: string);
begin
  if FShortDateFormat <> Value then
  begin
    FShortDateFormat := Value;
    DefaultChanged;
  end;
end;

procedure TDateTimeStorage.SetLongDateFormat(Value: string);
begin
  if FLongDateFormat <> Value then
  begin
    FLongDateFormat := Value;
    DefaultChanged;
  end;
end;

procedure TDateTimeStorage.SetDateSeparator(Value: Char);
begin
  if FDateSeparator <> Value then
  begin
    FDateSeparator := Value;
    DefaultChanged;
  end;
end;

procedure TDateTimeStorage.SetTimeSeparator(Value: Char);
begin
  if FTimeSeparator <> Value then
  begin
    FTimeSeparator := Value;
    DefaultChanged;
  end;
end;

procedure TDateTimeStorage.SetTimeAMString(Value: string);
begin
  if FTimeAMString <> Value then
  begin
    FTimeAMString := Value;
    DefaultChanged;
  end;
end;

procedure TDateTimeStorage.SetTimePMString(Value: string);
begin
  if FTimePMString <> Value then
  begin
    FTimePMString := Value;
    DefaultChanged;
  end;
end;

procedure TDateTimeStorage.SetShortTimeFormat(Value: string);
begin
  if FShortTimeFormat <> Value then
  begin
    FShortTimeFormat := Value;
    DefaultChanged;
  end;
end;

procedure TDateTimeStorage.SetLongTimeFormat(Value: string);
begin
  if FLongTimeFormat <> Value then
  begin
    FLongTimeFormat := Value;
    DefaultChanged;
  end;
end;

procedure TDateTimeStorage.StringsChanged(Sender: TObject);
begin
  DefaultChanged;
end;

procedure TDateTimeStorage.UpdateText;
var
  TmpShortDateFormat: string;
  TmpLongDateFormat: string;
  TmpDateSeparator: Char;
  TmpTimeSeparator: Char;
  TmpTimeAMString: string;
  TmpTimePMString: string;
  TmpShortTimeFormat: string;
  TmpLongTimeFormat: string;
  TmpShortMonthNames, TmpLongMonthNames,
  TmpShortDayNames, TmpLongDayNames: TStrings;
begin
  TmpShortMonthNames := nil;
  TmpLongMonthNames := nil;
  TmpShortDayNames := nil;
  TmpLongDayNames := nil;
  try
    TmpShortMonthNames := TStringList.Create;
    TmpLongMonthNames := TStringList.Create;
    TmpShortDayNames := TStringList.Create;
    TmpLongDayNames := TStringList.Create;
    // saving
    TmpShortDateFormat := SysUtils.ShortDateFormat;
    TmpLongDateFormat := SysUtils.LongDateFormat;
    TmpDateSeparator := SysUtils.DateSeparator;
    TmpTimeSeparator := SysUtils.TimeSeparator;
    TmpTimeAMString := SysUtils.TimeAMString;
    TmpTimePMString := SysUtils.TimePMString;
    TmpShortTimeFormat := SysUtils.ShortTimeFormat;
    TmpLongTimeFormat := SysUtils.LongTimeFormat;

    StringsAssignTo(TmpShortMonthNames, SysUtils.ShortMonthNames);
    StringsAssignTo(TmpLongMonthNames, SysUtils.LongMonthNames);
    StringsAssignTo(TmpShortDayNames, SysUtils.ShortDayNames);
    StringsAssignTo(TmpLongDayNames, SysUtils.LongDayNames);

    Write;

    FText := DateTimeToStr(FDateTime);
    FDate := DateToStr(FDateTime);
    FTime := TimeToStr(FDateTime);
    // restoring
    SysUtils.ShortDateFormat := TmpShortDateFormat;
    SysUtils.LongDateFormat := TmpLongDateFormat;
    SysUtils.DateSeparator := TmpDateSeparator;
    SysUtils.TimeSeparator := TmpTimeSeparator;
    SysUtils.TimeAMString := TmpTimeAMString;
    SysUtils.TimePMString := TmpTimePMString;
    SysUtils.ShortTimeFormat := TmpShortTimeFormat;
    SysUtils.LongTimeFormat := TmpLongTimeFormat;

    ArrayAssignTo(TmpShortMonthNames, SysUtils.ShortMonthNames);
    ArrayAssignTo(TmpLongMonthNames, SysUtils.LongMonthNames);
    ArrayAssignTo(TmpShortDayNames, SysUtils.ShortDayNames);
    ArrayAssignTo(TmpLongDayNames, SysUtils.LongDayNames);
  finally
    TmpShortMonthNames.Free;
    TmpLongMonthNames.Free;
    TmpShortDayNames.Free;
    TmpLongDayNames.Free;
  end;
end;

{ TCurrencyStorage }
constructor TCurrencyStorage.Create(AOwner: TComponent);
begin
  inherited;
  Read;
  FText := '';
end;

procedure TCurrencyStorage.Loaded;
begin
  inherited;
  if (csDesigning in ComponentState) then SetValue(FValue);
end;

procedure TCurrencyStorage.Read;
begin
  FCurrencyString := SysUtils.CurrencyString;
  FCurrencyFormat := SysUtils.CurrencyFormat;
  FNegCurrFormat := SysUtils.NegCurrFormat;
  FThousandSeparator := SysUtils.ThousandSeparator;
  FDecimalSeparator := SysUtils.DecimalSeparator;
  FCurrencyDecimals := SysUtils.CurrencyDecimals;
  inherited;
end;

procedure TCurrencyStorage.Write;
begin
  SysUtils.CurrencyString := Self.CurrencyString;
  SysUtils.CurrencyFormat := Self.CurrencyFormat;
  SysUtils.NegCurrFormat := Self.NegCurrFormat;
  SysUtils.ThousandSeparator := Self.ThousandSeparator;
  SysUtils.DecimalSeparator := Self.DecimalSeparator;
  SysUtils.CurrencyDecimals := Self.CurrencyDecimals;
end;

function TCurrencyStorage.GetText(
      ACurrencyString: string;
      ACurrencyFormat: Byte;
      ANegCurrFormat: Byte;
      AThousandSeparator: Char;
      ADecimalSeparator: Char;
      ACurrencyDecimals: Byte;
      AValue: Single): string;
var
  SaveCurrencyString: string;
  SaveCurrencyFormat: Byte;
  SaveNegCurrFormat: Byte;
  SaveThousandSeparator: Char;
  SaveDecimalSeparator: Char;
  SaveCurrencyDecimals: Byte;

begin
  Result := '';
  if (csLoading in ComponentState) then Exit;
  SaveCurrencyString := SysUtils.CurrencyString;
  SaveCurrencyFormat := SysUtils.CurrencyFormat;
  SaveNegCurrFormat := SysUtils.NegCurrFormat;
  SaveThousandSeparator := SysUtils.ThousandSeparator;
  SaveDecimalSeparator := SysUtils.DecimalSeparator;
  SaveCurrencyDecimals := SysUtils.CurrencyDecimals;

  try
    SysUtils.CurrencyString := ACurrencyString;
    SysUtils.CurrencyFormat := ACurrencyFormat;
    SysUtils.NegCurrFormat := ANegCurrFormat;
    SysUtils.ThousandSeparator := AThousandSeparator;
    SysUtils.DecimalSeparator := ADecimalSeparator;
    SysUtils.CurrencyDecimals := ACurrencyDecimals;

    Result := FloatToStrF(AValue, ffCurrency, 19, ACurrencyDecimals);

  finally
    SysUtils.CurrencyString := SaveCurrencyString;
    SysUtils.CurrencyFormat := SaveCurrencyFormat;
    SysUtils.NegCurrFormat := SaveNegCurrFormat;
    SysUtils.ThousandSeparator := SaveThousandSeparator;
    SysUtils.DecimalSeparator := SaveDecimalSeparator;
    SysUtils.CurrencyDecimals := SaveCurrencyDecimals;
  end;
end;

function TCurrencyStorage.ValueToStr(Value: Single): string;
begin
  Result := GetText(
    FCurrencyString,
    FCurrencyFormat,
    FNegCurrFormat,
    FThousandSeparator,
    FDecimalSeparator,
    FCurrencyDecimals,
    FValue);
end;

procedure TCurrencyStorage.SetCurrencyString(Value: string);
const
  MaxCurrencyString = 10;
begin
  Value := Copy(Value, 1, MaxCurrencyString);
  FText := GetText(
    Value,
    FCurrencyFormat,
    FNegCurrFormat,
    FThousandSeparator,
    FDecimalSeparator,
    FCurrencyDecimals,
    FValue);
  FCurrencyString := Value;
  DefaultChanged;
end;

procedure TCurrencyStorage.SetCurrencyFormat(Value: Byte);
begin
  FText := GetText(
    FCurrencyString,
    Value,
    FNegCurrFormat,
    FThousandSeparator,
    FDecimalSeparator,
    FCurrencyDecimals,
    FValue);
  FCurrencyFormat := Value;
  DefaultChanged;
end;

procedure TCurrencyStorage.SetNegCurrFormat(Value: Byte);
begin
  FText := GetText(
    FCurrencyString,
    FCurrencyFormat,
    Value,
    FThousandSeparator,
    FDecimalSeparator,
    FCurrencyDecimals,
    FValue);
  FNegCurrFormat := Value;
  DefaultChanged;
end;

procedure TCurrencyStorage.SetThousandSeparator(Value: Char);
begin
  FText := GetText(
    FCurrencyString,
    FCurrencyFormat,
    FNegCurrFormat,
    Value,
    FDecimalSeparator,
    FCurrencyDecimals,
    FValue);
  FThousandSeparator := Value;
  DefaultChanged;
end;

procedure TCurrencyStorage.SetDecimalSeparator(Value: Char);
begin
  FText := GetText(
    FCurrencyString,
    FCurrencyFormat,
    FNegCurrFormat,
    FThousandSeparator,
    Value,
    FCurrencyDecimals,
    FValue);
  FDecimalSeparator := Value;
  DefaultChanged;
end;

procedure TCurrencyStorage.SetCurrencyDecimals(Value: Byte);
begin
  FText := GetText(
    FCurrencyString,
    FCurrencyFormat,
    FNegCurrFormat,
    FThousandSeparator,
    FDecimalSeparator,
    Value,
    FValue);
  FCurrencyDecimals := Value;
  DefaultChanged;
end;

procedure TCurrencyStorage.UpdateText;
begin
  FText := GetText(
    FCurrencyString,
    FCurrencyFormat,
    FNegCurrFormat,
    FThousandSeparator,
    FDecimalSeparator,
    FCurrencyDecimals,
    FValue);
end;

procedure TCurrencyStorage.SetValue(Value: Single);
begin
  FText := GetText(
    FCurrencyString,
    FCurrencyFormat,
    FNegCurrFormat,
    FThousandSeparator,
    FDecimalSeparator,
    FCurrencyDecimals,
    Value);
  FValue := Value;
end;

procedure TCurrencyStorage.IncorrectValue;
begin
  raise EInvalidOp.Create('Incorrect value');
end;

{ TMoneyString }
var
  StringResourcesLoaded: Boolean = False;

constructor TMoneyString.Create(AOnwer: TComponent);
var
  I: TStringResource;
begin
  inherited;
  if not StringResourcesLoaded then
  begin
    for I := Low(StringResourcesDefault) to High(StringResourcesDefault) do
    begin
      StringResourcesDefault[I] := TStringList.Create;
      StringResourcesDefault[I].Text := LoadStr(SsrCurrency - Integer(I));
    end;
    StringResourcesLoaded := True;
  end;

  FCurrencyGender[0] := True;
  FUpperStart := True;
  FZeroEmpty[0] := True;
  FZeroEmpty[1] := True;
    
  for I := Low(TStringResource) to High(TStringResource) do
  begin
    FLists[I] := TStringList.Create;
    TStringList(FLists[I]).OnChange := ListChanged;
  end;

  SetDefault(True);
end;

destructor TMoneyString.Destroy;
var
  I: TStringResource;
begin
  for I := Low(TStringResource) to High(TStringResource) do
    FLists[I].Free;
  inherited;
end;

function TMoneyString.GetString(Strings: TStrings; Index: Integer): string;
begin
  if Index < Strings.Count then
    Result := Strings[Index] else
    Result := '';
end;

procedure TMoneyString.ListChanged(Sender: TObject);
var
  I: TStringResource;
begin
  for I := Low(TStringResource) to High(TStringResource) do
  begin
    if FLists[I] = Sender then
    begin
      FDefault := not ListStored(Integer(I));
      Break;
    end;
  end;
  SetValueString('');
end;

function TMoneyString.ListStored(Index: Integer): Boolean;
begin
  Result := (FLists[TStringResource(Index)].Text <> StringResourcesDefault[TStringResource(Index)].Text);
end;

function TMoneyString.Triada(Value:Integer; MaleGender: Boolean): string;
var
  Tmp: Integer;
begin
  Tmp := Value div 100;
  Value := Value mod 100;

  if (Tmp > 0) then
  begin
    Result := GetString(FLists[srHundred], Tmp - 1);
    if Value > 0 then Result := Result + ' ';
  end else
    Result := '';

  Tmp := Value div 10;
  Value := Value mod 10;

  if (Tmp > 1) then
  begin
    Result := Result + GetString(FLists[srTen], Tmp - 1);
    Tmp := Value;
    if (Tmp > 0) then
    begin
      Result := Result + ' ';
      if MaleGender then
        Result := Result + GetString(FLists[srMaleOne], Tmp - 1) else
        Result := Result + GetString(FLists[srFemaleOne], Tmp - 1);
    end;
  end else begin
    Tmp := Tmp * 10 + Value;
    case Tmp of
      1..9:
        begin
          if MaleGender then
            Result := Result + GetString(FLists[srMaleOne], Tmp - 1) else
            Result := Result + GetString(FLists[srFemaleOne], Tmp - 1);
        end;
      10:
        begin
          Result := Result + GetString(FLists[srTen], 0);
        end;
      11..19:
        begin
          Result := Result + GetString(FLists[srFirstTen], Tmp - 11);
        end;
    end;
  end;
end;

function TMoneyString.IntegerCurrencyStr(Value: TMoneyInteger; Currency: TStrings): string;
begin
  if Value mod 100 in [11..19] then
    Result := GetString(Currency, 2)
  else case (Value mod 10) of
    0, 5..9:
      Result := GetString(Currency, 2);
    1:
      Result := GetString(Currency, 0);
    2..4:
      Result := GetString(Currency, 1);
  end;
end;

function TMoneyString.IntegerGenderedToString(Value: TMoneyInteger; MaleGender: Boolean): string;
var
  Tmp: TMoneyInteger;
begin
  if Value = 0 then
  begin
    Result := FZero;
    Exit;
  end;

  Result := '';

{$IFDEF _D4_}
  Tmp := Value div 1000000000;
  Value := Value mod 1000000000;
  if Tmp > 0 then
  begin
    Result := IntegerGenderedToString(Tmp, True) + ' ' +
              IntegerCurrencyStr(Tmp, FLists[srBillion]);
    if Value > 0 then Result := Result + ' ';
  end;
{$ENDIF}

  Tmp := Value div 1000000;
  Value := Value mod 1000000;
  if Tmp > 0 then
  begin
    Result := Result +
      IntegerGenderedToString(Tmp, True) + ' ' +
      IntegerCurrencyStr(Tmp, FLists[srMillion]);
    if Value > 0 then Result := Result + ' ';
  end;

  Tmp := Value div 1000;
  Value := Value mod 1000;
  if Tmp > 0 then
  begin
    Result := Result +
      IntegerGenderedToString(Tmp, False) + ' ' +
      IntegerCurrencyStr(Tmp, FLists[srThousand]);
    if Value > 0 then Result := Result + ' ';
  end;

  Tmp := Value;
  if (Tmp > 0) then
    Result := Result + Triada(Tmp, MaleGender);
end;

function TMoneyString.IntegerToString(Value: TMoneyInteger): string;
begin
  Result := IntegerGenderedToString(Value, True);
end;

function TMoneyString.MoneyToString(Value: Currency): string;
var
  Curr, CurrSub: TMoneyInteger;
  Tmp, ResultSub: string;
begin
  Curr := Trunc(Value);
  CurrSub := Round(Frac(Value) * 100);

  if CurrSub = 100 then
  begin
    CurrSub := 0;
    Inc(Curr);
  end;


  { Currency string }
  if FFormats[0] = mfVerbal then
  begin
    if not ((Curr = 0) and FZeroEmpty[0]) then
      Result := IntegerGenderedToString(Curr, FCurrencyGender[0]) else
      Result := '';
  end else begin
    if not ((Curr = 0) and FZeroEmpty[0]) then
      Result := Format('%.2d', [Curr]) else
      Result := '';
  end;

  if (Result <> '') then
  begin
    Tmp := IntegerCurrencyStr(Curr, FLists[srCurrency]);
    if Tmp <> '' then Result := Result + ' ' + Tmp;
  end;

  { CurrencySub string }
  if FFormats[1] = mfVerbal then
  begin
    if not ((CurrSub = 0) and FZeroEmpty[1]) then
      ResultSub := IntegerGenderedToString(CurrSub, FCurrencyGender[1]) else
      ResultSub := '';
  end else begin
    if not ((CurrSub = 0) and FZeroEmpty[1]) then
      ResultSub := Format('%.2d', [CurrSub]) else
      ResultSub := '';
  end;

  if (ResultSub <> '') then
  begin
    Tmp := IntegerCurrencyStr(CurrSub, FLists[srCurrencySub]);
    if Tmp <> '' then ResultSub := ResultSub + ' ' + Tmp;
  end;

  { Concate Currency and CurrencySub strings }
  if Result <> '' then
  begin
    if ResultSub <> '' then
    begin
      if FDelimeter = '' then
        Result := Result + ' ' + ResultSub else
        Result := Result + ' ' + Delimeter + ' ' + ResultSub;
    end;
  end else
    Result := Result + ResultSub;

  if FUpperStart and (Length(Result) > 0) then Result[1] := AnsiUpperCase(Result[1])[1];
end;

procedure TMoneyString.SetCurrencyGender(Index: Integer; Value: Boolean);
begin
  if FCurrencyGender[Index] <> Value then
  begin
    FCurrencyGender[Index] := Value;
    SetValueString('');
  end;
end;

procedure TMoneyString.SetDefault(Value: Boolean);
var
  I: TStringResource;
begin
  if FDefault <> Value then
  begin
    if Value then
    begin
      for I := Low(TStringResource) to High(TStringResource) do
        SetList(Integer(I), StringResourcesDefault[I]);

      FZero := LoadStr(SsrZero);
    end;
    FDefault := Value;
  end;
end;

procedure TMoneyString.SetDelimeter(Value: string);
begin
  if (FDelimeter <> Value) then
  begin
    FDelimeter := Value;
    SetValueString('');
  end;
end;

procedure TMoneyString.SetFormat(Index: Integer; Value: TMoneyFormat);
begin
  if (FFormats[Index] <> Value) then
  begin
    FFormats[Index] := Value;
    SetValueString('');
  end;
end;

procedure TMoneyString.SetList(Index: Integer; Value: TStrings);
begin
  FLists[TStringResource(Index)].Assign(Value);
end;

procedure TMoneyString.SetUpperStart(Value: Boolean);
begin
  if (FUpperStart <> Value) then
  begin
    FUpperStart := Value;
    SetValueString('');
  end;
end;

procedure TMoneyString.SetValueNumber(Value: Currency);
begin
  if FValueNumber <> Value then
  begin
    FValueString := MoneyToString(Value);
    FValueNumber := Value;
  end;
end;

procedure TMoneyString.SetValueString(Value: string);
begin
  FValueString := MoneyToString(FValueNumber);
end;

procedure TMoneyString.SetZero(Value: string);
begin
  if (FZero <> Value) then
  begin
    FZero := Value;
    FDefault := FDefault and not ZeroStored;
    SetValueString('');
  end;
end;

procedure TMoneyString.SetZeroEmpty(Index: Integer; Value: Boolean);
begin
  if (FZeroEmpty[Index] <> Value) then
  begin
    FZeroEmpty[Index] := Value;
    SetValueString('');
  end;
end;

function TMoneyString.ZeroStored: Boolean;
begin
  Result := FZero <> LoadStr(SsrZero);
end;

{$IFDEF _D4_}
function GetDefaultIniFileName: string;
begin
  Result := ExtractFileName(ChangeFileExt(Application.ExeName, '.ini'));
end;

function GetDefaultIniRegKey: string;
begin
  Result := ExtractFileName(ChangeFileExt(Application.ExeName, ''));
  Result := 'Software\' + Result;
end;

function GetDefaultSection(Component: TComponent): string;
var
  F: TCustomForm;
  Owner: TComponent;
begin
  if Assigned(Component) then
  begin
    if (Component is TCustomForm) or (Component is TDataModule) then
      Result := Component.ClassName
    else begin
      Result := Component.Name;
      if Component is TControl then
      begin
        F := GetParentForm(TControl(Component));
        if F <> nil then
          Result := F.ClassName + Result
        else begin
          if TControl(Component).Parent <> nil then
            Result := TControl(Component).Parent.Name + Result;
        end;
      end else begin
        Owner := Component.Owner;
        if (Owner is TCustomForm) or (Component is TDataModule) then
          Result := Format('%s.%s', [Owner.ClassName, Result]);
      end;
    end;
  end else
    Result := '';
end;

{ TAppIniFile }
constructor TAppIniFile.Create(AOwner: TComponent);
begin
  inherited;
  FIniFileType := ftRegIniFile;
end;

destructor TAppIniFile.Destroy;
begin
  while Assigned(FLinks) do
    RemoveIniFileLink(FLinks.Last);
  DestroyIniFile;
  inherited;
end;

function TAppIniFile.CreateIniFile: TCustomIniFile;
const
  RegRoots: array[TRegistryRoot] of DWord = (
    HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE, HKEY_CLASSES_ROOT,
    HKEY_CURRENT_CONFIG, HKEY_USERS, HKEY_DYN_DATA);
begin
  Result := nil;
  case FIniFileType of
    ftIniFile:
      Result := TIniFile.Create(IniFileName);
    ftMemIniFile:
      Result := TMemIniFile.Create(IniFileName);
    ftRegIniFile:
      begin
        Result := TRegistryIniFile.Create(IniFileName);
        TRegistryIniFile(Result).RegIniFile.RootKey := RegRoots[FRegistryRoot];
      end;
    ftUserDefined:
      begin
        if Assigned(FOnCreateIniFile) then
          FOnCreateIniFile(Self, Result);
      end;
  end;
end;

procedure TAppIniFile.CheckIniFile;
begin
  if not Assigned(FIniFile) then
    FIniFile := CreateIniFile;
  DoIniCreate;
end;

procedure TAppIniFile.DestroyIniFile;
begin
  if Assigned(FIniFile) then
  begin
    DoIniDestroy;
    FReadCount := 0;
    FWriteCount := 0;
    FreeObject(FIniFile);
  end;
end;

procedure TAppIniFile.DoIniCreate;
var
  I: Integer;
begin
  for I := 0 to ListCount(FLinks) - 1 do
    TIniFileLink(FLinks.List^[I]).IniCreate;
end;

procedure TAppIniFile.DoIniDestroy;
var
  I: Integer;
begin
  for I := 0 to ListCount(FLinks) - 1 do
    TIniFileLink(FLinks.List^[I]).IniDestroy;
end;

procedure TAppIniFile.DoLoadLinks;
var
  I: Integer;
begin
  for I := 0 to ListCount(FLinks) - 1 do
    with TIniFileLink(FLinks.List^[I]) do
      if Active then Load;
end;

procedure TAppIniFile.DoSaveLinks;
var
  I: Integer;
begin
  for I := 0 to ListCount(FLinks) - 1 do
    with TIniFileLink(FLinks.List^[I]) do
      if Active then Save;
end;

function TAppIniFile.GetIniFileName: string;
begin
  if (FIniFileName = '') and not (csDesigning in ComponentState) then
  begin
    if FIniFileType <> ftRegIniFile then
      Result := GetDefaultIniFileName else
      Result := GetDefaultIniRegKey;
  end else
    Result := FIniFileName;
end;

procedure TAppIniFile.SetIniFileName(Value: string);
begin
  if (FIniFileName <> Value) then
  begin
    DestroyIniFile;
    FIniFileName := Value;
  end;
end;

procedure TAppIniFile.SetIniFileType(Value: TIniFileType);
begin
  if (FIniFileType <> Value) then
  begin
    DestroyIniFile;
    FIniFileType := Value;
  end;
end;

procedure TAppIniFile.SetRegistryRoot(Value: TRegistryRoot);
begin
  if (FRegistryRoot <> Value) then
  begin
    DestroyIniFile;
    FRegistryRoot := Value;
  end;
end;

procedure TAppIniFile.InsertIniFileLink(ALink: TIniFileLink);
begin
  ListAdd(FLinks, ALink);
  ALink.FAppIniFile := Self;
end;

procedure TAppIniFile.RemoveIniFileLink(ALink: TIniFileLink);
begin
  ListRemove(FLinks, ALink);
  ALink.FAppIniFile := nil;
end;

function TAppIniFile.BeginRead: Boolean;
begin
  if FReadCount = 0 then
  begin
    CheckIniFile;
    Result := Assigned(FIniFile);
  end else
    Result := True;
  if Result then Inc(FReadCount);
end;

function TAppIniFile.BeginWrite: Boolean;
begin
  if FWriteCount = 0 then
  begin
    CheckIniFile;
    Result := Assigned(FIniFile);
  end else
    Result := True;
  if Result then Inc(FWriteCount);
end;

procedure TAppIniFile.EndRead;
begin
  if FReadCount > 0 then
  begin
    Dec(FReadCount);
    if (FReadCount = 0) and (FWriteCount = 0) then
      DestroyIniFile;
  end;
end;

procedure TAppIniFile.EndWrite;
begin
  if FWriteCount > 0 then
  begin
    Dec(FWriteCount);
    try
      if (FWriteCount = 0) then FIniFile.UpdateFile;
    except
      Application.HandleException(Self);
    end;
    if (FReadCount = 0) and (FWriteCount = 0) then
      DestroyIniFile;
  end;
end;

procedure TAppIniFile.InternalLoad;
begin
  DoLoadLinks;
end;

procedure TAppIniFile.InternalSave;
begin
  DoSaveLinks;
end;

procedure TAppIniFile.Load;
begin
  if BeginRead then
  try
    InternalLoad;
  finally
    EndRead;
  end;
end;

procedure TAppIniFile.Save;
begin
  if BeginWrite then
  try
    InternalSave;
  finally
    EndWrite;
  end;
end;

{ TIniFileLink }
constructor TIniFileLink.Create;
begin
  inherited;
  FActive := True;
end;

destructor TIniFileLink.Destroy;
begin
  SetAppIniFile(nil);
  inherited;
end;

function TIniFileLink.GetIniFile: TCustomIniFile;
begin
  if Assigned(FAppIniFile) then
    Result := FAppIniFile.IniFile else
    Result := nil;
end;

procedure TIniFileLink.InternalLoad;
begin
  if Assigned(FOnLoad) then FOnLoad(Self);
end;

procedure TIniFileLink.InternalSave;
begin
  if Assigned(FOnSave) then FOnSave(Self);
end;

procedure TIniFileLink.InternalIniCreate;
begin
  if Assigned(FOnIniCreate) then FOnIniCreate(Self);
end;

procedure TIniFileLink.InternalIniDestroy;
begin
  if Assigned(FOnIniDestroy) then FOnIniDestroy(Self);
end;

procedure TIniFileLink.IniCreate;
begin
  InternalIniCreate;
end;

procedure TIniFileLink.IniDestroy;
begin
  InternalIniDestroy;
end;

procedure TIniFileLink.Load;
begin
  if Assigned(FAppIniFile) and FAppIniFile.BeginRead then
  try
    InternalLoad;
  finally
    FAppIniFile.EndRead;
  end;
end;

procedure TIniFileLink.Save;
begin
  if Assigned(FAppIniFile) and FAppIniFile.BeginWrite then
  try
    InternalSave;
  finally
    FAppIniFile.EndWrite;
  end;
end;

procedure TIniFileLink.SetAppIniFile(Value: TAppIniFile);
begin
  if FAppIniFile <> Value then
  begin
    if Assigned(FAppIniFile) then FAppIniFile.RemoveIniFileLink(Self);
    if Assigned(Value) then Value.InsertIniFileLink(Self);
  end;
end;

{ TStoredValue }
constructor TStoredValue.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FValue := Unassigned;
end;

procedure TStoredValue.Assign(Source: TPersistent);
begin
  if (Source is TStoredValue) then
    with TStoredValue(Source) do
    begin
      Self.FValue := FValue;
    end
  else
    inherited;
end;

function TStoredValue.GetStoredValues: TStoredValues;
begin
  Result := TStoredValues(Collection);
end;

procedure TStoredValue.Clear;
begin
  FValue := Unassigned;
end;

function TStoredValue.IsValueStored: Boolean;
begin
  Result := not VarIsEmpty(FValue);
end;

procedure TStoredValue.Save;
var
  AppIniFile: TAppIniFile;
begin
  AppIniFile := StoredValues.PropStorage.AppIniFile;
  if Assigned(AppIniFile) and AppIniFile.BeginWrite then
  try
    InternalSave;
  finally
    AppIniFile.EndWrite;
  end;
end;

procedure TStoredValue.Load;
var
  AppIniFile: TAppIniFile;
begin
  AppIniFile := StoredValues.PropStorage.AppIniFile;
  if Assigned(AppIniFile) and AppIniFile.BeginRead then
  try
    InternalLoad;
  finally
    AppIniFile.EndRead;
  end;
end;

procedure TStoredValue.InternalLoad;
var
  LoadValue: Variant;
  LoadStrValue, DefaultStrValue: string;
begin
  DefaultStrValue := VarToStr(Value);
  with StoredValues.PropStorage, AppIniFile do
    LoadStrValue := IniFile.ReadString(AppSection, Self.Name, DefaultStrValue);
  LoadValue := LoadStrValue;
  if Assigned(FOnLoad) then
    FOnLoad(Self, LoadValue);
  Value := LoadValue;
end;

procedure TStoredValue.InternalSave;
var
  SaveValue: Variant;
  SaveStrValue: string;
begin
  SaveValue := Value;
  if Assigned(FOnSave) then
    FOnSave(Self, SaveValue);
  SaveStrValue := VarToStr(SaveValue);
  with StoredValues.PropStorage, AppIniFile do
    IniFile.WriteString(AppSection, Self.Name, SaveStrValue);
end;

{ TStoredValues }
constructor TStoredValues.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TStoredValue);
end;

function TStoredValues.GetItem(Index: Integer): TStoredValue;
begin
  Result := TStoredValue(inherited Items[Index]);
end;

procedure TStoredValues.SetItem(Index: Integer; StoredValue: TStoredValue);
begin
  inherited SetItem(Index, TCollectionItem(StoredValue));
end;

function TStoredValues.GetValue(const Name: string): TStoredValue;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I >= 0 then
    Result := Items[I] else
    Result := nil;
end;

procedure TStoredValues.SetValue(const Name: string; StoredValue: TStoredValue);
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I >= 0 then
    Items[I].Assign(StoredValue);
end;

procedure TStoredValues.SaveValues;
var
  I: Integer;
  AppIniFile: TAppIniFile;
begin
  AppIniFile := PropStorage.AppIniFile;
  if Assigned(AppIniFile) and AppIniFile.BeginWrite then
  try
    for I := 0 to Count - 1 do
      Items[I].Save;
  finally
    AppIniFile.EndWrite;
  end;
end;

procedure TStoredValues.LoadValues;
var
  I: Integer;
  AppIniFile: TAppIniFile;
begin
  AppIniFile := PropStorage.AppIniFile;
  if Assigned(AppIniFile) and AppIniFile.BeginRead then
  try
    for I := 0 to Count - 1 do
      Items[I].Load;
  finally
    AppIniFile.EndRead;
  end;
end;

function TStoredValues.GetPropStorage: TPropStorage;
begin
  Result := GetOwner as TPropStorage;
end;

{ TPropStorage }
constructor TPropStorage.Create(AOwner: TComponent);
begin
  inherited;
  FIniFileLink := TIniFileLink.Create;
  FIniFileLink.OnLoad := IniLoad;
  FIniFileLink.OnSave := IniSave;
  FStoredProps := TStringList.Create;
  with TStringList(FStoredProps) do
  begin
    Duplicates := dupIgnore;
    OnChange := StoredPropsChanged;
  end;
  FStoredValues := TStoredValues.Create(Self);
end;

destructor TPropStorage.Destroy;
begin
  if not (csDesigning in ComponentState) {$IFDEF _D5_}and not Assigned(FMasterStorage){$ENDIF} then
    UnhookEvents;
  FreeObject(FStoredProps);
  FStoredValues.Free;
  FIniFileLink.Free;
  inherited;
end;

function TPropStorage.GetActive: Boolean;
begin
  Result := FIniFileLink.Active;
end;

procedure TPropStorage.SetActive(Value: Boolean);
begin
  FIniFileLink.Active := Value;
end;

function TPropStorage.GetAppIniFile: TAppIniFile;
begin
  Result := FIniFileLink.AppIniFile;
end;

procedure TPropStorage.SetAppIniFile(Value: TAppIniFile);
begin
  FIniFileLink.AppIniFile := Value;
end;

function TPropStorage.GetAppSection: string;
var
  Tmp: string;
begin
  Tmp := FIniFileLink.AppSection;
  if (Tmp = '') and not (csDesigning in ComponentState) then
    Result := GetDefaultSection(Owner) else
    Result := Tmp;
end;

procedure TPropStorage.SetAppSection(Value: string);
begin
  FIniFileLink.AppSection := Value;
end;

{$IFDEF _D5_}
procedure TPropStorage.DoLoadLinkedStorages;
var
  I: Integer;
begin
  for I := 0 to ListCount(FStorages) - 1 do
    TPropStorage(FStorages[I]).Load;
end;

procedure TPropStorage.DoSaveLinkedStorages;
var
  I: Integer;
begin
  for I := 0 to ListCount(FStorages) - 1 do
    TPropStorage(FStorages[I]).Save;
end;

function TPropStorage.IsLinkedTo(Storage: TPropStorage): Boolean;
var
  MasterStorage: TPropStorage;
begin
  MasterStorage := Self.FMasterStorage;
  while MasterStorage <> nil do
  begin
    if MasterStorage = Storage then
    begin
      Result := True;
      Exit;
    end;
    MasterStorage := MasterStorage.MasterStorage;
  end;
  Result := False;
end;

procedure TPropStorage.InsertStorage(Value: TPropStorage);
begin
  ListAdd(FStorages, Value);
  Value.FMasterStorage := Self;
end;

procedure TPropStorage.RemoveStorage(Value: TPropStorage);
begin
  ListRemove(FStorages, Value);
  Value.FMasterStorage := nil;
end;

procedure TPropStorage.SetMasterStorage(Value: TPropStorage);
begin
  if (FMasterStorage <> Value) then
  begin
    if (Value = Self) or Assigned(Value) and Value.IsLinkedTo(Self) then
      raise EInvalidOp.Create(LoadStr(SCircularLink));
    if Assigned(FMasterStorage) then FMasterStorage.RemoveStorage(Self);
    if Assigned(Value) then Value.InsertStorage(Self);
  end;
end;
{$ENDIF}

procedure TPropStorage.DoLoadProps;
begin
  with TPropsFiler.Create(AppIniFile.IniFile, AppSection) do
  try
    try
      LoadObjectsProps(Owner, FStoredProps);
    except end;
  finally
    Free;
  end;
end;

procedure TPropStorage.DoSaveProps;
begin
  with TPropsFiler.Create(AppIniFile.IniFile, AppSection) do
  try
    StoreObjectsProps(Owner, FStoredProps);
  finally
    Free;
  end;
end;

procedure TPropStorage.InternalLoad;
begin
  DoLoadProps;
  FStoredValues.LoadValues;
  if Assigned(FOnLoad) then FOnLoad(Self);
{$IFDEF _D5_}
  DoLoadLinkedStorages;
{$ENDIF}
end;

procedure TPropStorage.InternalSave;
begin
  DoSaveProps;
  FStoredValues.SaveValues;
  if Assigned(FOnSave) then FOnSave(Self);
{$IFDEF _D5_}
  DoSaveLinkedStorages;
{$ENDIF}
end;

procedure TPropStorage.Loaded;
var
  Loading: Boolean;
begin
  Loading := csLoading in ComponentState;
  inherited Loaded;
  if Loading then
  begin
    if not (csDesigning in ComponentState) {$IFDEF _D5_}and not Assigned(FMasterStorage){$ENDIF} then
      HookEvents;
    UpdateStoredList(Owner, FStoredProps, True);
  end;
end;

procedure TPropStorage.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if Assigned(FStoredProps) then
    begin
      FStoredProps.BeginUpdate;
      try
        repeat
          I := FStoredProps.IndexOfObject(AComponent);
          if I >= 0 then FStoredProps.Delete(I);
        until I < 0;
      finally
        FStoredProps.EndUpdate;
      end;
    end;
{$IFDEF _D5_}
    if (FMasterStorage = AComponent) then
      SetMasterStorage(nil);
{$ENDIF}
  end;
end;

procedure TPropStorage.Load;
begin
  FIniFileLink.Load;
end;

procedure TPropStorage.Save;
begin
  FIniFileLink.Save;
end;

procedure TPropStorage.IniLoad(Sender: TObject);
begin
  InternalLoad;
end;

procedure TPropStorage.IniSave(Sender: TObject);
begin
  InternalSave;
end;

function TPropStorage.GetStoredProps: TStrings;
begin
  if not (csLoading in ComponentState) then
    UpdateStoredList(Owner, FStoredProps, False);
  Result := FStoredProps;
end;

procedure TPropStorage.SetStoredProps(Value: TStrings);
begin
  FStoredProps.Assign(Value);
end;

procedure TPropStorage.SetStoredValues(Value: TStoredValues);
begin
  FStoredValues.Assign(Value);
end;

procedure TPropStorage.StoredPropsChanged(Sender: TObject);
var
  I: Integer;
  Instance: TComponent;
begin
  if (csLoading in ComponentState) then Exit;
  TStringList(FStoredProps).OnChange := nil;
  try
    UpdateStoredList(Owner, FStoredProps, True);
    for I := 0 to FStoredProps.Count - 1 do
    begin
      Instance := TComponent(FStoredProps.Objects[I]);
      FreeNotification(Instance);
    end;
  finally
    TStringList(FStoredProps).OnChange := StoredPropsChanged;
  end;
end;

procedure TPropStorage.HookEvents;
begin
  if Owner is TCustomForm then
  begin
    with TForm(Owner) do
    begin
      FSaveOnShow := OnShow;
      OnShow := DoOnShow;
      FSaveOnCloseQuery := OnCloseQuery;
      OnCloseQuery := DoOnCloseQuery;
      FSaveOnDestroy := OnDestroy;
      OnDestroy := DoOnDestroy;
    end;
  end else if Owner is TDataModule then
  begin
    with TDataModule(Owner) do
    begin
      FSaveOnShow := OnCreate;
      OnCreate := DoOnShow;
      FSaveOnDestroy := OnDestroy;
      OnDestroy := DoOnDestroy;
    end;
  end;
end;

procedure TPropStorage.UnhookEvents;
begin
  if (Owner is TCustomForm) then
  begin
    with TForm(Owner) do
    begin
      OnShow := FSaveOnShow;
      OnCloseQuery := FSaveOnCloseQuery;
      OnDestroy := FSaveOnDestroy;
    end;
  end else if Owner is TDataModule then
  begin
    with TDataModule(Owner) do
    begin
      OnCreate := FSaveOnShow;
      OnDestroy := FSaveOnDestroy;
    end;
  end;
end;

procedure TPropStorage.DoOnShow(Sender: TObject);
begin
  try
    Load;
  except
    Application.HandleException(Self);
  end;
  if Assigned(FSaveOnShow) then FSaveOnShow(Sender);
end;

procedure TPropStorage.DoOnCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FSaveOnCloseQuery) then
    FSaveOnCloseQuery(Sender, CanClose);
  if CanClose and (Owner is TCustomForm) and (TForm(Owner).Handle <> 0) then
  try
    FSaved := True;
    Save;
  except
    Application.HandleException(Self);
  end;
end;

procedure TPropStorage.DoOnDestroy(Sender: TObject);
begin
  if not FSaved then
  begin
    FDestroying := True;
    try
      FSaved := True;
      Save;
    except
      Application.HandleException(Self);
    end;
    FDestroying := False;
  end;
  if Assigned(FSaveOnDestroy) then FSaveOnDestroy(Sender);
end;
{$ENDIF}

end.
