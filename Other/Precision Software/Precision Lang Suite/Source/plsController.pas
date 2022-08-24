{------------------------------------------------------------------------------
  plsController.pas

  Precision Language Suite

  written by  Michal Mutl (MiTeC)
              e-mail: michal.mutl@mitec.cz
              web: http://www.mitec.cz/

              Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com/

  Purpose:    Implementation of TplsController, a visual component for creating
              multilingual applications (for Delphi and Lazarus IDEs,
              VCL, FMX and LCL frameworks are supported)

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  You can freely use this component in your products, if you have purchased
  the license. The complete source code remains property of the author
  and may not be distributed, published, given or sold in any form as such.
  No parts of the source code can be included in any other component
  or application without written authorization of the author.

  Copyright (c) 2008-2014  Precision software & consulting
  All rights reserved
------------------------------------------------------------------------------}

{ History:

  - Version: 2.5.2
    * added: Support for Delphi XE6 and XE7

  - Version 2.5.1.9
    * first public release
}

{ Implementation of TplsController, a visual component for creating multilingual applications }
{$IFNDEF PLSFMX}
unit plsController;
{$ENDIF}

{$IFDEF FPC}
{$MODE objfpc}
{$ENDIF}

interface

uses
  Classes
  {$IFDEF PLSFMX}
    , FMX.plsLangMan
  {$ELSE}
    , plsLangMan
  {$ENDIF}
  ;

type
  { TplsController component refers to the global LanguageManager instance by default.
    If you want to use TplsController with your own instance of TLanguageManager class,
    you need to declare an event handler of TplsCtrlGetCustomLangManEvent type and assign
    it to the TplsController.OnGetCustomLangManager event. }
  TplsCtrlGetCustomLangManEvent = procedure(Sender: TObject; var ALangManager: TLanguageManager) of object;

  { Visual component that is designed for easy implementation of PLS localization engine into your applications }
  TplsController = class(TComponent)
  private
    FAvailLangs: TStringList;
    FAutoInitLanguage: Boolean;
    FDefaultErrorMessage: Boolean;
    FDefaultLangCode: string;
    FIsMainController: Boolean;
    FLCErrorHandled: Boolean;
    FOnBeforeLangChange: TNotifyEvent;
    FOnError: TNotifyEvent;
    FOnGetCustomLangManager: TplsCtrlGetCustomLangManEvent;
    FOnInitLangManager: TNotifyEvent;
    FOnLanguageChanged: TNotifyEvent;
    function GetCurrentLangManager: TLanguageManager;
    function GetLanguageCode: string; virtual;
    procedure SetLanguageCode(const Value: string); virtual;
    function GetLanguageName: string;
    function GetLastError: string;
    function CheckAvailLangsList: Boolean;
    function GetLangsCode(Index: Integer): string;
    function GetLangsName(Index: Integer): string;
    function GetBiDiMode: TBiDiMode;
    function GetLangIdx(ACode: string): Integer;
  protected
    procedure DefaultErrMess; virtual;
    procedure DoLanguageChanged(Sender: TObject); virtual;
    function GetLangsCount: Integer;
  public
    // Constructor
    constructor Create(AOwner: TComponent); override;
    // Destructor
    destructor Destroy; override;
    // Internal procedure - do not call it directly
    procedure Loaded; override;
    // Returns all available languages in the form of "langcode=langname"
    function GetAvailableLanguages(const AList:TStringList):Boolean; virtual;
    { Applies currently selected language to the current form again, or (for the main controller) to a whole application. }
    procedure Refresh; virtual;
    { Re-reads the list of available languages }
    procedure RefreshLanguages;
    // BiDi mode of currently loaded language
    property BiDiMode:TBiDiMode read GetBiDiMode;
    { This property provides access to properties and methods of currently selected language manager }
    property LangManager: TLanguageManager read GetCurrentLangManager;
    { Returns or sets the current language code (i.e. 'en','cs','en-US','cs-CZ',...).
      This is the core property for switching language during run-time.
      Simply set a new value to this property to change language in a whole application.
      Do not use this property for loading an initial language on application start - instead use
      DefaultLangCode property that is intended for that situation. }
    property LanguageCode: string read GetLanguageCode write SetLanguageCode;
    // User friendly name of currently selected language
    property LanguageName: string read GetLanguageName;
    { You can refer to LastError property for the text of last processing error message. See also OnError. }
    property LastError: string read GetLastError;
    // Returns the number of available languages. The list is sorted by language name.
    property LanguagesCount: Integer read GetLangsCount;
    // Returns a language code from the list of available languages. The list is sorted by language name.
    property LanguageCodes[Index: Integer]: string read GetLangsCode;
    // Returns a language name from the list of available languages. The list is sorted by language name.
    property LanguageNames[Index: Integer]: string read GetLangsName;
    // Returns an index of language code from the list of available languages. The list is sorted by language name.
    property LanguageIndexByCode[ACode: string]: Integer read GetLangIdx;
    // Returns True if the current instance of TplsController is the main controller (a broker of LanguageChanged event for all other controllers) of its LangManager
    property IsMainController: Boolean read FIsMainController;
  published
    { If this property is True and no DefaultLangCode is assigned during OnInitLangManager event,
      plsController tries to load the language based on current user's locale during application start }
    property AutoInitLanguage: Boolean read FAutoInitLanguage write FAutoInitLanguage default True;
    { If this property is True, and you do not assign any handler for OnError event, a default error dialog of PLS Engine will be shown every time any error occurs.
      Please, keep in mind, that errors are internally catched by try..except blocks, so if you turn off this property and you ommit an OnError event handler,
      no error messages will be shown. }
    property DefaultErrorMessage: Boolean read FDefaultErrorMessage write FDefaultErrorMessage default True;
    { Default language code to load on application start if AutoInitLanguage is False or if the language based on current user's locale is not available.
      If you assign some language code to this property during OnInitLangManager event, then plsController will try to load this language first,
      prior to user's locale and prior to the previous value stored in DefaultLangCode. }
    property DefaultLangCode: string read FDefaultLangCode write FDefaultLangCode;
    { This event is fired right before a new language is applied to the current form. Please, keep in mind, that this new language
      is already loaded to the LanguageManager in this time.
      For main plsController it is recommended to perform any background tasks (such as calls to plsDialogs.LanguageChanged
      and LangConsts.LanguageChanged) in this event handler. }
    property OnBeforeLangChange: TNotifyEvent read FOnBeforeLangChange write FOnBeforeLangChange;
    { This event is fired every time any error occurs. You can refer to LastError property for the text of error message. See also DefaultErrorMessage. }
    property OnError: TNotifyEvent read FOnError write FOnError;
    { By default, every plsController component refers to the global LanguageManager variable.
      If you want to use another instance of TLanguageManager class with the given plsController,
      you need to assign required TLanguageManager instance to the ALangManager variable inside this event handler.
      Currently selected language manager can be accessed via LangManager property. }
    property OnGetCustomLangManager: TplsCtrlGetCustomLangManEvent read FOnGetCustomLangManager write FOnGetCustomLangManager;
    { This event is dedicated to configure your settings for loading language files.
      You can specify the folder, filtering and other options here. Please, refer to TLanguageManager class description for more info
      about available options.
      Note: only main plsController fires this event. }
    property OnInitLangManager: TNotifyEvent read FOnInitLangManager write FOnInitLangManager;
    { This event is fired after the new language has been applied to the current form.
      In this event handler you can perform any additional tasks, that follow up the language change. }
    property OnLanguageChanged: TNotifyEvent read FOnLanguageChanged write FOnLanguageChanged;
  end;

implementation

uses
  SysUtils
  {$IFDEF PLSFMX}
    , System.UITypes, FMX.Forms, FMX.Dialogs
  {$ELSE}
    , Forms, Dialogs
  {$ENDIF}
  ;

{ TplsController }

constructor TplsController.Create(AOwner: TComponent);
begin
  inherited;
  FAutoInitLanguage := True;
  FDefaultErrorMessage := True;
  FDefaultLangCode := '';
  FAvailLangs := nil;
  { Identify the main controller (TLanguageManager.OnLanguageChanged listener).
    A first TplsController, that is created, becomes TLanguageManager.OnLanguageChanged listener. }
  FIsMainController := False;
  if Assigned(LangManager) and (not Assigned(LangManager.OnLanguageChanged)) then
  begin
    LangManager.OnLanguageChanged:={$IFDEF FPC}@{$ENDIF}DoLanguageChanged;
    FIsMainController := True;
  end;
end;

destructor TplsController.Destroy;
begin
  if FIsMainController and Assigned(LangManager) then
    LangManager.OnLanguageChanged := nil;
  if Assigned(FAvailLangs) then
    FAvailLangs.Free;
  inherited;
end;

procedure TplsController.DefaultErrMess;
var
  lm, om: string;
begin
  if FDefaultErrorMessage and Assigned(LangManager) then
  begin
    if LangManager.LanguageCode='' then lm := ''
    else lm :=LangManager.LanguageName+' ('+LangManager.LanguageCode+'): ';
    if Owner = nil then om := ''
    else om := '['+Owner.Name+' ('+Owner.ClassName+')] ';
    MessageDlg(lm + om + LangManager.LastError, {$IFDEF PLSFMX}TMsgDlgType.{$ENDIF}mtWarning, [{$IFDEF PLSFMX}TMsgDlgBtn.{$ENDIF}mbOK], 0)
  end;
end;

procedure TplsController.Loaded;
var
  defLC, iniLC: string;
begin
  inherited;
  if (not (csDesigning in ComponentState)) and Assigned(LangManager) then
  begin
    // Load initial language (after application starts)
    if FIsMainController and (LangManager.LanguageCode = '') then
    begin
      { init options of current language manager }
      iniLC := '';
      if Assigned(FOnInitLangManager) then
      begin
        defLC := FDefaultLangCode;
        FDefaultLangCode := '';
        FOnInitLangManager(Self);
        if FDefaultLangCode <> '' then // DefaultLangCode has been set during OnInitLangManager event
          iniLC := FDefaultLangCode;
        FDefaultLangCode := defLC;
      end;
      FLCErrorHandled := False;
      { load default language }
      if iniLC <> '' then // if LanguageCode is requested
        LangManager.LanguageCode := iniLC;
      if LangManager.LanguageCode = '' then
      begin
        if AutoInitLanguage then
        begin
          LangManager.LanguageCode := GetDefaultLangCode;
          if LangManager.LanguageCode = '' then
          begin
            if DefaultLangCode <> '' then
              LangManager.LanguageCode := DefaultLangCode
            else
              LangManager.LanguageCode := 'en'; // just to be gentle to the user, try to set widely used lang code
          end;
        end
        else
        if DefaultLangCode <> '' then
          LangManager.LanguageCode := DefaultLangCode;
      end;
      if iniLC <> '' then // if LanguageCode was requested during OnInitLangManager
        DefaultLangCode := iniLC;

      if (LangManager.LastError<>'') and (not FLCErrorHandled) then
      begin
        if Assigned(FOnError) then
          FOnError(Self)
        else
          DefaultErrMess;
      end;
    end
    else  // apply language to owner's component
    if LangManager.LanguageCode<>'' then
      DoLanguageChanged(LangManager);
  end;
end;

function TplsController.GetCurrentLangManager: TLanguageManager;
begin
  if Assigned(FOnGetCustomLangManager) then
  begin
    Result := nil;
    FOnGetCustomLangManager(Self, Result);
  end
  else
    Result := {$IFDEF PLSFMX}FMX.{$ENDIF}plsLangMan.LanguageManager;
end;

function TplsController.GetLanguageCode: string;
begin
  if Assigned(LangManager) then
    Result:=LangManager.LanguageCode
  else
    Result:='';
end;

procedure TplsController.SetLanguageCode(const Value: string);
begin
  if Assigned(LangManager) and (not SameText(Value,LangManager.LanguageCode)) then
  begin
    if not Assigned(LangManager.OnLanguageChanged) then
    begin
      LangManager.OnLanguageChanged:={$IFDEF FPC}@{$ENDIF}DoLanguageChanged;  // make itself a new main controller
      FIsMainController := True;
    end;
    if FIsMainController and Assigned(FOnInitLangManager) and (LangManager.LanguageCode='') then
      FOnInitLangManager(Self); // init options of current language manager
    FLCErrorHandled := False;
    LangManager.LanguageCode:=Value;
    if (LangManager.LastError<>'') and (not FLCErrorHandled) then
    begin
      if Assigned(FOnError) then
        FOnError(Self)
      else
        DefaultErrMess;
    end;
  end;
end;

function TplsController.GetLanguageName: string;
begin
  if Assigned(LangManager) then
    Result:=LangManager.LanguageName
  else
    Result:='';
end;

function TplsController.GetLastError: string;
begin
  if Assigned(LangManager) then
    Result:=LangManager.LastError
  else
    Result:='';
end;

procedure TplsController.DoLanguageChanged(Sender: TObject);
var
  i: Integer;
  down: Boolean;

  procedure _DoLangChangeFor(const C: TComponent);
  var
    j: Integer;
  begin
    if C = Owner then
      Exit;
    if down then
    begin
      for j:=C.ComponentCount-1 downto 0 do
        if C.Components[j] is TplsController then
        begin
          TplsController(C.Components[j]).DoLanguageChanged(Sender);
          break;
        end;
    end
    else
      for j:=0 to C.ComponentCount-1 do
        if C.Components[j] is TplsController then
        begin
          TplsController(C.Components[j]).DoLanguageChanged(Sender);
          break;
        end;
  end;

begin
  if (Sender <> nil) and (Sender=LangManager) then
  begin

    if Assigned(FOnBeforeLangChange) then
      FOnBeforeLangChange(Self);

    if not LangManager.LangVCL(Owner) then
    begin
      if Assigned(FOnError) then
        FOnError(Self)
      else
        DefaultErrMess;
      FLCErrorHandled := True;
    end;

    if Assigned(FOnLanguageChanged) then
      FOnLanguageChanged(Self);

    if FIsMainController then
    begin
      if Assigned(Owner) then
      begin
        i := Self.ComponentIndex + 1;
        down := i > (Owner.ComponentCount div 2);
      end
      else
        down := False;
      for i:=0 to Screen.DataModuleCount-1 do
        _DoLangChangeFor(Screen.DataModules[i]);
      for i := 0 to Screen.FormCount-1 do
        _DoLangChangeFor(Screen.Forms[i]);
      FLCErrorHandled := True;
    end;

  end;
end;

function _AvailLangsCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText(List.ValueFromIndex[Index1], List.ValueFromIndex[Index2]);
end;

function TplsController.GetAvailableLanguages(const AList:TStringList):Boolean;
begin
  if Assigned(LangManager) then
  begin
    Result := LangManager.GetAvailableLanguages(AList);
    if Result then
    begin
      if AList = FAvailLangs then
        FAvailLangs.CustomSort({$IFDEF FPC}@{$ENDIF}_AvailLangsCompare);
    end
    else
    if LangManager.LastError<>'' then
    begin
      if Assigned(FOnError) then
        FOnError(Self)
      else
        DefaultErrMess;
    end;
  end
  else
    Result := False;
end;

function TplsController.CheckAvailLangsList: Boolean;
begin
  if Assigned(FAvailLangs) then
    Result := True
  else
  if Assigned(LangManager) then
  begin
    FAvailLangs := TStringList.Create;
    Result := GetAvailableLanguages(FAvailLangs);
    if not Result then
    begin
      FAvailLangs.Free;
      FAvailLangs := nil;
    end;
  end
  else
    Result := False;
end;

function TplsController.GetLangsCount: Integer;
begin
  if CheckAvailLangsList then
    Result := FAvailLangs.Count
  else
    Result := 0;
end;

function TplsController.GetLangsCode(Index: Integer): string;
begin
  if CheckAvailLangsList and (Index>=0) and (Index<FAvailLangs.Count) then
    Result := FAvailLangs.Names[Index]
  else
    Result := '';
end;

function TplsController.GetLangsName(Index: Integer): string;
begin
  if CheckAvailLangsList and (Index>=0) and (Index<FAvailLangs.Count) then
    Result := FAvailLangs.ValueFromIndex[Index]
  else
    Result := '';
end;

function TplsController.GetBiDiMode: TBiDiMode;
begin
  if Assigned(LangManager) then
    Result := LangManager.BiDiMode
  else
    Result := Application.BiDiMode;
end;

function TplsController.GetLangIdx(ACode: string): Integer;
begin
  if CheckAvailLangsList then
    Result:=FAvailLangs.IndexOfName(ACode)
  else
    Result:=-1;
end;

procedure TplsController.RefreshLanguages;
begin
  if not Assigned(FAvailLangs) then
    CheckAvailLangsList
  else
    GetAvailableLanguages(FAvailLangs);
end;

procedure TplsController.Refresh;
begin
  if Assigned(LangManager) and (LangManager.LanguageCode<>'') then
    DoLanguageChanged(LangManager);
end;

end.