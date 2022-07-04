
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit AutoComplete;

interface

{$I STD.INC}

uses
  Classes, ComObj, Controls, OleTools, ShlIntf, StrTools, Windows, WinTools;

type
  TFindWindowHandleEvent = procedure(Sender: TObject; var Wnd: THandle) of object;

  TAutoCompletionOption = (aoAppend, aoSearch, aoSuggest);
  TAutoCompletionOptions = set of TAutoCompletionOption;

  TAutoCompletion = class(TComponent)
  private
    FActive: Boolean;
    FAssociate: TWinControl;
    FAutoComplete: IUnknown;
    FEditHandle: THandle;
    FEnumString: IUnknown;
    FInternalActive: Boolean;
    FOptions: TAutoCompletionOptions;
    FStrings: TStrings;
    FOnFindEdit: TFindWindowHandleEvent;
    procedure SetActive(Value: Boolean);
    procedure SetAssociate(Value: TWinControl);
    procedure SetOptions(Value: TAutoCompletionOptions);
    procedure SetStrings(Value: TStrings);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RealizeEdit;
    property Unknown: IUnknown read FAutoComplete;
  published  
    property Active: Boolean read FActive write SetActive;
    property Associate: TWinControl read FAssociate write SetAssociate;
    property Strings: TStrings read FStrings write SetStrings;
    property Options: TAutoCompletionOptions read FOptions write SetOptions;
    property OnFindEdit: TFindWindowHandleEvent read FOnFindEdit write FOnFindEdit;
  end;

function IsAutoCompletionVisible: Boolean;

implementation

function IsAutoCompletionVisible: Boolean;
const
  AutoClass = 'Auto-Suggest Dropdown';
var
  Wnd: HWND;
begin
  Wnd := FindWindow(AutoClass, nil);
  Result := (Wnd <> 0) and IsWindowVisible(Wnd);
end;

{ TAutoCompletion }

constructor TAutoCompletion.Create(AOwner: TComponent);
var
  EnumString: TEnumString;
begin
  inherited Create(AOwner);
  FOptions := [aoAppend, aoSuggest];
  FStrings := TStringList.Create;
  with FStrings as TStringList do
  begin
    Sorted  := True;
    Duplicates := dupIgnore;
  end;
  EnumString := TEnumString.Create;
  EnumString.Strings := FStrings;
  FEnumString := EnumString as IUnknown;
end;

destructor TAutoCompletion.Destroy;
begin
  Active := False;
  Associate := nil;
  FAutoComplete := nil;
  FEnumString := nil;
  FStrings.Free;
  inherited Destroy;
end;

procedure TAutoCompletion.RealizeEdit;
var
  S: string;
  P: PChar;
begin
  if (FAssociate <> nil) and (FEditHandle = 0)  then
  begin
    if Assigned(FOnFindEdit)  then
      FOnFindEdit(Self, FEditHandle);
    if FEditHandle = 0 then
    begin
      S := FAssociate.ClassName;
      P := PChar(S);
      if SearchToken(P, 'EDIT') then
        FEditHandle := FAssociate.Handle
      else
        FEditHandle := FindSimilarChild(FAssociate.Handle, 'EDIT');
    end;
  end;
end;

procedure TAutoCompletion.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent <> nil) and (AComponent = FAssociate) and (Operation = opRemove) then
    Associate := nil;
end;

procedure TAutoCompletion.SetActive(Value: Boolean);
const
  AutoOptions: array[TAutoCompletionOption] of Cardinal =
    (ACO_AUTOAPPEND, ACO_SEARCH, ACO_AUTOSUGGEST);
var
  BasicAutoComplete: IAutoComplete;
  AdvancedAutoComplete: IAutoComplete2;
  AutoOption: Cardinal;
  I: TAutoCompletionOption;
begin
  if csDesigning in ComponentState then
    FActive := Value
  else if Value <> FActive then
  begin
    RealizeEdit;
    FActive := (FEditHandle <> 0) and Value;
    FInternalActive := Value;
    if FAutoComplete = nil then
      FAutoComplete := CreateComObject(CLSID_AutoComplete);
    BasicAutoComplete := FAutoComplete as IAutoComplete;
    if FActive then
    begin
      OleCheck(BasicAutoComplete.Init(FEditHandle, FEnumString, nil, nil));
      if FAutoComplete.QueryInterface(IID_IAutoComplete2, AdvancedAutoComplete) = S_OK then
      begin
        AutoOption := ACO_NONE;
        for I := Low(TAutoCompletionOption) to High(TAutoCompletionOption) do
          if I in FOptions then
            AutoOption := AutoOption or AutoOptions[I];
        AdvancedAutoComplete.SetOptions(AutoOption);
      end;
    end;
    BasicAutoComplete.Enable(FActive);
  end;
end;

procedure TAutoCompletion.SetOptions(Value: TAutoCompletionOptions);
var
  WasActive: Boolean;
begin
  if Value <> FOptions then
  begin
    WasActive := FActive;
    Active := False;
    FOptions := Value;
    Active := WasActive;
  end;
end;

procedure TAutoCompletion.SetAssociate(Value: TWinControl);
var
  WasActive: Boolean;
begin
  if Value <> FAssociate then
  begin
    WasActive := FActive;
    Active := False;
    FEditHandle := 0;
    if FAssociate <> nil then
      FAssociate.RemoveFreeNotification(Self);
    FAssociate := Value;
    if FAssociate <> nil then
      FAssociate.FreeNotification(Self);
    if FAssociate <> nil then
      RealizeEdit;
    Active := WasActive or FInternalActive;
  end;
end;

procedure TAutoCompletion.SetStrings(Value: TStrings);
begin
  FStrings.Assign(Value);
end;

end.
