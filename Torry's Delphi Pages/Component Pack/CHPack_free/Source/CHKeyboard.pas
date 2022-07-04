unit CHKeyboard;

{ ##############################################################################
  TCHKeyboard

  Version   		:   1.0.0
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)
  Licence    		:   without Source --> Freeware
  									with Source    --> Shareware
  Original code	:   TComponent

  History:
  1.0.0 - 27.11.2002    - First Release
  1.0.1 - 09.03.2003    - reorganize "uses" for more performance and less memory needed


  ############################################################################ }

interface

uses
  Windows, SysUtils, Classes, ExtCtrls, Dialogs,
  _CHTypes;


type
  TCHKeyboard = class;

  TCHKeyLedState = class(TPersistent)
  private
    FOwner : TCHKeyboard;
    function GetLedState(Key: Integer) : Boolean;
    procedure SetLedState(Key: Word; Value: Boolean);
    function GetCapsLock: Boolean;
    function GetNumLock: Boolean;
    function GetScrollLock: Boolean;
    procedure GetAllLock;
    procedure SetCapsLock(const Value: Boolean);
    procedure SetNumLock(const Value: Boolean);
    procedure SetScrollLock(const Value: Boolean);
  public
    constructor Create(AOwner: TCHKeyboard); virtual;
  published
    property CapsLock : Boolean read GetCapsLock write SetCapsLock;
    property NumLock : Boolean read GetNumLock write SetNumLock;
    property ScrollLock : Boolean read GetScrollLock write SetScrollLock;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHKeyboard = class(TComponent)
  private
    FOnKeyCheck: TNotifyEvent;
    FOnKeyPress: TNotifyEvent;
    FKeyLedState: TCHKeyLedState;
    FTimer : TTimer;
    FKeyboardLayout: TKeyboardLayout;
    FKeyCheckInterval: Cardinal;
    FKeyCheck: Boolean;
    FVirtualKeys : array[1..200] of Word;
    FPressedKeys : array[1..3] of Word;
    FStoredKeys : array[1..3] of Word;
    FLastIndex : Word;
    FKeyPressed : Boolean;

    procedure DoTimer(Sender : TObject);
    function IsKeyPressed : Boolean;
    procedure GetPressedKeys;
    procedure GetLanguageName;
    procedure TranslateLastKey(Key : Word);

    procedure SetKeyCheckInterval(const Value: Cardinal);
    procedure SetKeyCheck(const Value: Boolean);
    procedure SetKeyboardLayout(const Value: TKeyboardLayout);

  public
    FIsKeyPress : Boolean;
    FLastKeyValue : Word;
    FLastKeyChar : Char;
    FLastKeyVirtual : string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnKeyCheck : TNotifyEvent read FOnKeyCheck write FOnKeyCheck;
    property OnKeyPress : TNotifyEvent read FOnKeyPress write FOnKeyPress;

    property KeyCheck : Boolean read FKeyCheck Write SetKeyCheck;
    property KeyCheckInterval : Cardinal read FKeyCheckInterval Write SetKeyCheckInterval;
    property KeyboardLayout : TKeyboardLayout read FKeyboardLayout Write SetKeyboardLayout;
    property LEDState : TCHKeyLedState read FKeyLedState Write FKeyLedState;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHKeyboard]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHKeyboard.Create(AOwner: TComponent);
var
  I : Integer;
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 30;
  FTimer.Enabled := False;
  FTimer.OnTimer := DoTimer;

  FKeyLedState := TCHKeyLedState.Create(Self);

  FIsKeyPress := False;
  FLastKeyValue := 0;
  FLastIndex := 0;
  FKeyCheckInterval := FTimer.Interval;
  FKeyCheck := FTimer.Enabled;

  // get language name
  GetLanguageName;

  // fill with ASCII Values
  for I := 1 to Length(FVirtualKeys) do
  	FVirtualKeys[I] := I+7;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHKeyboard.Destroy;
begin
  FTimer.Free;
  FKeyLedState.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyboard.DoTimer(Sender : TObject);
begin
  GetPressedKeys;
  if IsKeyPressed then
  begin
  	if Assigned(FOnKeyPress) then
    	FOnKeyPress(Self);
  end;

  if Assigned(FOnKeyCheck) then
  	FOnKeyCheck(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ can store three simultan pressed keys }
procedure TCHKeyboard.GetPressedKeys;
var
  nCount, nIndex : Integer;
  nAsyncKey : Word;
begin
  If not (csDesigning in ComponentState) then
  begin
    nIndex := 0;
    FPressedKeys[1] := 0;
    FPressedKeys[2] := 0;
    FPressedKeys[3] := 0;
    for nCount := 1 to Length(FVirtualKeys) do
    begin
      nAsyncKey := GetAsyncKeyState(FVirtualKeys[nCount]);
      if nAsyncKey <> 0 then
      begin
        Inc(nIndex);
        FPressedKeys[nIndex] := FVirtualKeys[nCount];
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ checked whether a key was pressed }
function TCHKeyboard.IsKeyPressed: Boolean;
begin
  If not (csDesigning in ComponentState) then
  begin
    FKeyPressed := False;

    if FStoredKeys[1] <> FPressedKeys[1] then
    begin
      if FPressedKeys[1] <> 0 then
      begin
        FKeyPressed := True;
        TranslateLastKey(FPressedKeys[1]);
      end;
    end;
    if FStoredKeys[2] <> FPressedKeys[2] then
    begin
      if FPressedKeys[2] <> 0 then
      begin
        FKeyPressed := True;
        TranslateLastKey(FPressedKeys[2]);
      end;
    end;
    if FStoredKeys[3] <> FPressedKeys[3] then
    begin
      if FPressedKeys[3] <> 0 then
      begin
        FKeyPressed := True;
        TranslateLastKey(FPressedKeys[3]);
      end;
    end;

    FStoredKeys[1] := FPressedKeys[1];
    FStoredKeys[2] := FPressedKeys[2];
    FStoredKeys[3] := FPressedKeys[3];
  end;


  Result := FKeyPressed;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyboard.GetLanguageName;
var
 LayoutName : Array [0..KL_NAMELENGTH] of Char;
begin
  GetKeyboardLayoutName(@LayoutName);
  if StrPas(LayoutName) = '00000407' then
    FKeyboardLayout := lng_German
  else if StrPas(LayoutName) = '00000409' then
    FKeyboardLayout := lng_English
  else if StrPas(LayoutName) = '00000000' then
    FKeyboardLayout := lng_Neutral
  else if StrPas(LayoutName) = '00000800' then
    FKeyboardLayout := lng_SystemDefault;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyboard.SetKeyboardLayout(const Value: TKeyboardLayout);
var
  lngValue : PChar;
begin
  if FKeyboardLayout <> Value then
  begin
  	FKeyboardLayout := Value;
    if Value = lng_German then
      lngValue := '00000407'
    else if Value = lng_English then
      lngValue := '00000409'
    else if Value = lng_Neutral then
      lngValue := '00000000'
    else if Value = lng_SystemDefault then
      lngValue := '00000800'
    else
      lngValue := '00000000';

    if not (csDesigning in ComponentState) then
    	LoadKeyboardLayout(lngValue, KLF_ACTIVATE);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyboard.SetKeyCheck(const Value: Boolean);
begin
  if FKeyCheck <> Value then
  begin
  	FKeyCheck := Value;
    if not (csDesigning in ComponentState) then
  	  FTimer.Enabled := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyboard.SetKeyCheckInterval(const Value: Cardinal);
begin
  if FKeyCheckInterval <> Value then
  begin
  	FKeyCheckInterval := Value;
    if not (csDesigning in ComponentState) then
  		FTimer.Interval := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyboard.TranslateLastKey(Key : Word);
begin
  FLastKeyVirtual := '';
  FLastKeyChar := #0;

  case Key of
      8 : FLastKeyVirtual := 'VK_BACK';
      9 : FLastKeyVirtual := 'VK_TAB';
     13 : FLastKeyVirtual := 'VK_RETURN';
     16 : FLastKeyVirtual := 'VK_SHIFT';
     17 : FLastKeyVirtual := 'VK_CONTROL';
     18 : FLastKeyVirtual := 'VK_MENU';
     19 : FLastKeyVirtual := 'VK_PAUSE';
     20 : FLastKeyVirtual := 'VK_CAPITAL';
     27 : FLastKeyVirtual := 'VK_ESCAPE';
     32 : begin
     				FLastKeyVirtual := 'VK_SPACE'; FLastKeyChar := ' ';
          end;
     33 : FLastKeyVirtual := 'VK_PRIOR';
     34 : FLastKeyVirtual := 'VK_NEXT';
     35 : FLastKeyVirtual := 'VK_END';
     36 : FLastKeyVirtual := 'VK_HOME';
     37 : FLastKeyVirtual := 'VK_LEFT';
     38 : FLastKeyVirtual := 'VK_UP';
     39 : FLastKeyVirtual := 'VK_RIGHT';
     40 : FLastKeyVirtual := 'VK_DOWN';
     41 : FLastKeyVirtual := 'VK_SELECT';
     43 : FLastKeyVirtual := 'VK_EXECUTE';
     44 : FLastKeyVirtual := 'VK_SNAPSHOT';
     45 : FLastKeyVirtual := 'VK_INSERT';
     46 : FLastKeyVirtual := 'VK_DELETE';
     47 : FLastKeyVirtual := 'VK_HELP';
     48 : begin
     				FLastKeyVirtual := 'VK_0'; FLastKeyChar := '0';
          end;
     49 : begin
     				FLastKeyVirtual := 'VK_1'; FLastKeyChar := '1';
          end;
     50 : begin
     				FLastKeyVirtual := 'VK_2'; FLastKeyChar := '2';
          end;
     51 : begin
     				FLastKeyVirtual := 'VK_3'; FLastKeyChar := '3';
          end;
     52 : begin
     				FLastKeyVirtual := 'VK_4'; FLastKeyChar := '4';
          end;
     53 : begin
     				FLastKeyVirtual := 'VK_5'; FLastKeyChar := '5';
          end;
     54 : begin
     				FLastKeyVirtual := 'VK_6'; FLastKeyChar := '6';
          end;
     55 : begin
     				FLastKeyVirtual := 'VK_7'; FLastKeyChar := '7';
          end;
     56 : begin
     				FLastKeyVirtual := 'VK_8'; FLastKeyChar := '8';
          end;
     57 : begin
     				FLastKeyVirtual := 'VK_9'; FLastKeyChar := '9';
          end;
     65 : begin
     				FLastKeyVirtual := 'VK_A'; FLastKeyChar := 'A';
          end;
     66 : begin
     				FLastKeyVirtual := 'VK_B'; FLastKeyChar := 'B';
          end;
     67 : begin
     				FLastKeyVirtual := 'VK_C'; FLastKeyChar := 'C';
          end;
     68 : begin
     				FLastKeyVirtual := 'VK_D'; FLastKeyChar := 'D';
          end;
     69 : begin
     				FLastKeyVirtual := 'VK_E'; FLastKeyChar := 'E';
          end;
     70 : begin
     				FLastKeyVirtual := 'VK_F'; FLastKeyChar := 'F';
          end;
     71 : begin
     				FLastKeyVirtual := 'VK_G'; FLastKeyChar := 'G';
          end;
     72 : begin
     				FLastKeyVirtual := 'VK_H'; FLastKeyChar := 'H';
          end;
     73 : begin
     				FLastKeyVirtual := 'VK_I'; FLastKeyChar := 'I';
          end;
     74 : begin
     				FLastKeyVirtual := 'VK_J'; FLastKeyChar := 'J';
          end;
     75 : begin
     				FLastKeyVirtual := 'VK_K'; FLastKeyChar := 'K';
          end;
     76 : begin
     				FLastKeyVirtual := 'VK_L'; FLastKeyChar := 'L';
          end;
     77 : begin
     				FLastKeyVirtual := 'VK_M'; FLastKeyChar := 'M';
          end;
     78 : begin
     				FLastKeyVirtual := 'VK_N'; FLastKeyChar := 'N';
          end;
     79 : begin
     				FLastKeyVirtual := 'VK_O'; FLastKeyChar := 'O';
          end;
     80 : begin
     				FLastKeyVirtual := 'VK_P'; FLastKeyChar := 'P';
          end;
     81 : begin
     				FLastKeyVirtual := 'VK_Q'; FLastKeyChar := 'Q';
          end;
     82 : begin
     				FLastKeyVirtual := 'VK_R'; FLastKeyChar := 'R';
          end;
     83 : begin
     				FLastKeyVirtual := 'VK_S'; FLastKeyChar := 'S';
          end;
     84 : begin
     				FLastKeyVirtual := 'VK_T'; FLastKeyChar := 'T';
          end;
     85 : begin
     				FLastKeyVirtual := 'VK_U'; FLastKeyChar := 'U';
          end;
     86 : begin
     				FLastKeyVirtual := 'VK_V'; FLastKeyChar := 'V';
          end;
     87 : begin
     				FLastKeyVirtual := 'VK_W'; FLastKeyChar := 'W';
          end;
     88 : begin
     				FLastKeyVirtual := 'VK_X'; FLastKeyChar := 'X';
          end;
     89 : begin
     				FLastKeyVirtual := 'VK_Y'; FLastKeyChar := 'Y';
          end;
     90 : begin
     				FLastKeyVirtual := 'VK_Z'; FLastKeyChar := 'Z';
          end;
     91 : FLastKeyVirtual := 'VK_LWIN';
     92 : FLastKeyVirtual := 'VK_RWIN';
     93 : FLastKeyVirtual := 'VK_APPS';
     96 : begin
     				FLastKeyVirtual := 'VK_NUMPAD0';
            if FKeyLedState.NumLock then
              FLastKeyChar := '0';
          end;
     97 : begin
     				FLastKeyVirtual := 'VK_NUMPAD1';
            if FKeyLedState.NumLock then
              FLastKeyChar := '1';
          end;
     98 : begin
     				FLastKeyVirtual := 'VK_NUMPAD2';
            if FKeyLedState.NumLock then
              FLastKeyChar := '2';
          end;
     99 : begin
     				FLastKeyVirtual := 'VK_NUMPAD3';
            if FKeyLedState.NumLock then
              FLastKeyChar := '3';
          end;
    100 : begin
     				FLastKeyVirtual := 'VK_NUMPAD4';
            if FKeyLedState.NumLock then
              FLastKeyChar := '4';
          end;
    101 : begin
     				FLastKeyVirtual := 'VK_NUMPAD5';
            if FKeyLedState.NumLock then
              FLastKeyChar := '5';
          end;
    102 : begin
     				FLastKeyVirtual := 'VK_NUMPAD6';
            if FKeyLedState.NumLock then
              FLastKeyChar := '6';
          end;
    103 : begin
     				FLastKeyVirtual := 'VK_NUMPAD7';
            if FKeyLedState.NumLock then
              FLastKeyChar := '7';
          end;
    104 : begin
     				FLastKeyVirtual := 'VK_NUMPAD8';
            if FKeyLedState.NumLock then
              FLastKeyChar := '8';
          end;
    105 : begin
     				FLastKeyVirtual := 'VK_NUMPAD9';
            if FKeyLedState.NumLock then
              FLastKeyChar := '9';
          end;
    106 : begin
     				FLastKeyVirtual := 'VK_MULTIPLY'; FLastKeyChar := '*';
          end;
    107 : begin
     				FLastKeyVirtual := 'VK_ADD'; FLastKeyChar := '+';
          end;
    108 : FLastKeyVirtual := 'VK_SEPERATOR';
    109 : begin
     				FLastKeyVirtual := 'VK_SUBTRACT'; FLastKeyChar := '-';
          end;
    110 : begin
     				FLastKeyVirtual := 'VK_DECIMAL'; FLastKeyChar := ',';
          end;
    111 : begin
     				FLastKeyVirtual := 'VK_DIVIDE'; FLastKeyChar := '/';
          end;
    112 : FLastKeyVirtual := 'VK_F1';
    113 : FLastKeyVirtual := 'VK_F2';
    114 : FLastKeyVirtual := 'VK_F3';
    115 : FLastKeyVirtual := 'VK_F4';
    116 : FLastKeyVirtual := 'VK_F5';
    117 : FLastKeyVirtual := 'VK_F6';
    118 : FLastKeyVirtual := 'VK_F7';
    119 : FLastKeyVirtual := 'VK_F8';
    120 : FLastKeyVirtual := 'VK_F9';
    121 : FLastKeyVirtual := 'VK_F10';
    122 : FLastKeyVirtual := 'VK_F11';
    123 : FLastKeyVirtual := 'VK_F12';
    124 : FLastKeyVirtual := 'VK_F13';
    125 : FLastKeyVirtual := 'VK_F14';
    126 : FLastKeyVirtual := 'VK_F15';
    127 : FLastKeyVirtual := 'VK_F16';
    128 : FLastKeyVirtual := 'VK_F17';
    129 : FLastKeyVirtual := 'VK_F18';
    130 : FLastKeyVirtual := 'VK_F19';
    131 : FLastKeyVirtual := 'VK_F20';
    132 : FLastKeyVirtual := 'VK_F21';
    133 : FLastKeyVirtual := 'VK_F22';
    134 : FLastKeyVirtual := 'VK_F23';
    135 : FLastKeyVirtual := 'VK_F24';
    144 : begin
    				FLastKeyVirtual := 'VK_NUMLOCK'; FKeyLedState.GetAllLock;
          end;
    145 : FLastKeyVirtual := 'VK_SCROLL';
    160 : FLastKeyVirtual := 'VK_LSHIFT';
    161 : FLastKeyVirtual := 'VK_RSHIFT';
    162 : FLastKeyVirtual := 'VK_LCONTROL';
    163 : FLastKeyVirtual := 'VK_RCONTROL';
    164 : FLastKeyVirtual := 'VK_LMENU';
    165 : FLastKeyVirtual := 'VK_RMENU';
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHKeyLedState }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHKeyLedState.Create(AOwner: TCHKeyboard);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyLedState.SetCapsLock(const Value: Boolean);
begin
  if not (csDesigning in FOwner.ComponentState) then
    SetLedState(VK_CAPITAL, Value);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHKeyLedState.GetCapsLock: Boolean;
begin
  Result := GetLedState(VK_CAPITAL)
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHKeyLedState.GetNumLock: Boolean;
begin
  Result := GetLedState(VK_NUMLOCK)
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyLedState.SetNumLock(const Value: Boolean);
begin
  if not (csDesigning in FOwner.ComponentState) then
    SetLedState(VK_NUMLOCK, Value);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHKeyLedState.GetScrollLock: Boolean;
begin
  Result := GetLedState(VK_SCROLL)
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyLedState.SetScrollLock(const Value: Boolean);
begin
  if not (csLoading in FOwner.ComponentState) then
    SetLedState(VK_SCROLL, Value);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHKeyLedState.GetLedState(Key: Integer): Boolean;
var Buffer: TKeyboardState;
begin
  GetKeyboardState(Buffer);
  Result := Buffer[Key] <> 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyLedState.GetAllLock;
begin
  ScrollLock := GetLedState(VK_SCROLL);
  NumLock := GetLedState(VK_NUMLOCK);
  CapsLock := GetLedState(VK_CAPITAL);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyLedState.SetLedState(Key: Word; Value: Boolean);
var
	Buffer: TKeyboardState;
begin
  // Win95/98/ME
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) then
  begin
  	GetKeyboardState(Buffer);
    Buffer[Key] := Ord(Value);
    SetKeyboardState(Buffer)
  end
  // WinNT/2000/XP
  else if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    if GetLedState(Key) <> Value then
    begin
      keybd_event(Key, $45, KEYEVENTF_EXTENDEDKEY, 0);
      keybd_event(Key, $45, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
    end;
  end
  else
    ShowMessage('Not supported!');
end;







end.
