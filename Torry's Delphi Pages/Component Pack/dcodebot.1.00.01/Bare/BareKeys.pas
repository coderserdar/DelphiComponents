unit BareKeys;

interface

{$I BARE.INC}

(* Written by Ken Henderson

Copyright (c) 1995 Ken Henderson     email:khen@compuserve.com

This unit includes two routines that simulate popular Visual Basic
routines: Sendkeys and AppActivate.  SendKeys takes a PChar
as its first parameter and a boolean as its second, like so:

	SendKeys('KeyString', Wait);
	SendKeys('abc123{left}{left}{left}def{end}456{left 6}ghi{end}789', True);

where KeyString is a string of key names and modifiers that you want
to send to the current input focus and Wait is a boolean variable or value
that indicates whether SendKeys should wait for each key message to be
processed before proceeding.  See the table below for more information.

SendKeys supports the Visual Basic SendKeys syntax, as documented below.

Supported modifiers:

  + = Shift
  ^ = Control
  & = Alt

Surround sequences of characters or key names with parentheses in order to
modify them as a group.  For example, '+abc' shifts only 'a', while '+(abc)' shifts
all three characters.

Supported special characters

  ~ = Enter
  ( = Begin modifier group (see above)
  ) = End modifier group (see above)
  { = Begin key name text (see below)
  } = End key name text (see below)

Supported characters:

  Any character that can be typed is supported.  Surround the modifier keys
  listed above with braces in order to send as normal text.

Supported key names (surround these with braces):

  BKSP, BS, BACKSPACE
  BREAK
  CAPSLOCK
  CLEAR
  DEL
  DELETE
  DOWN
  END
  ENTER
  ESC
  ESCAPE
  F1
  F2
  F3
  F4
  F5
  F6
  F7
  F8
  F9
  F10
  F11
  F12
  F13
  F14
  F15
  F16
  HELP
  HOME
  INS
  LEFT
  NUMLOCK
  PGDN
  PGUP
  PRTSC
  RIGHT
  SCROLLLOCK
  TAB
  UP

Follow the keyname with a space and a number to send the specified key a
given number of times (e.g., {left 6}). *)

uses
	{$IFNDEF BARE}SysUtils, {$ENDIF} BareUtils, Windows, Messages;

function SendKeys(SendKeysString: PChar; Wait: Boolean): Boolean;
procedure SendCommand(Point: TPoint; Command: string; Button: TPuckButton = pbLeft);

implementation

{ SendKeys procedure

	Converts a string of characters and key names to keyboard events and
	passes them to Windows. }


type
  THKeys = array[0..Pred(MaxLongInt)] of Byte;

  WBytes = array[0..pred(SizeOf(Word))] of Byte;

  TSendKey = record
    Name: ShortString;
    VKey: Byte;
  end;

{ Array of keys that SendKeys recognizes.

  if you add to this list, you must be sure to keep it sorted alphabetically
  by Name because a binary search routine is used to scan it. }

const
  MaxSendKeyRecs = 41;

  SendKeyRecs: array[0..MaxSendKeyRecs] of TSendKey = (
   (Name:'BACK'; VKey: VK_BACK),
   (Name:'BKSPACE'; VKey: VK_BACK),
   (Name:'BS'; VKey: VK_BACK),
   (Name:'BREAK'; VKey: VK_CANCEL),
   (Name:'CAPSLOCK'; VKey: VK_CAPITAL),
   (Name:'CLEAR'; VKey: VK_CLEAR),
   (Name:'DEL'; VKey: VK_DELETE),
   (Name:'DELETE'; VKey: VK_DELETE),
   (Name:'DOWN'; VKey: VK_DOWN),
   (Name:'END'; VKey: VK_END),
   (Name:'ENTER'; VKey: VK_RETURN),
   (Name:'ESC'; VKey: VK_ESCAPE),
   (Name:'ESCAPE'; VKey: VK_ESCAPE),
   (Name:'F1'; VKey: VK_F1),
   (Name:'F10'; VKey: VK_F10),
   (Name:'F11'; VKey: VK_F11),
   (Name:'F12'; VKey: VK_F12),
   (Name:'F13'; VKey: VK_F13),
   (Name:'F14'; VKey: VK_F14),
   (Name:'F15'; VKey: VK_F15),
   (Name:'F16'; VKey: VK_F16),
   (Name:'F2'; VKey: VK_F2),
   (Name:'F3'; VKey: VK_F3),
   (Name:'F4'; VKey: VK_F4),
   (Name:'F5'; VKey: VK_F5),
   (Name:'F6'; VKey: VK_F6),
   (Name:'F7'; VKey: VK_F7),
   (Name:'F8'; VKey: VK_F8),
   (Name:'F9'; VKey: VK_F9),
   (Name:'HELP'; VKey: VK_HELP),
   (Name:'HOME'; VKey: VK_HOME),
   (Name:'INS'; VKey: VK_INSERT),
   (Name:'LEFT'; VKey: VK_LEFT),
   (Name:'NUMLOCK'; VKey: VK_NUMLOCK),
   (Name:'PAUSE'; VKey: VK_PAUSE),
   (Name:'PGDN'; VKey: VK_NEXT),
   (Name:'PGUP'; VKey: VK_PRIOR),
   (Name:'PRTSC'; VKey: VK_PRINT),
   (Name:'RIGHT'; VKey: VK_RIGHT),
   (Name:'SCROLLLOCK'; VKey: VK_SCROLL),
   (Name:'TAB'; VKey: VK_TAB),
   (Name:'UP'; VKey: VK_UP));

{ Extra VK constants missing from Delphi's Windows API interface }

  VK_NULL = 0;
  VK_SEMICOLON = 186;
  VK_EQUAL = 187;
  VK_COMMA = 188;
  VK_MINUS = 189;
  VK_PERIOD = 190;
  VK_SLASH = 191;
  VK_BACKQUOTE = 192;
  VK_LEFTBRACKET = 219;
  VK_BACKSLASH = 220;
  VK_RIGHTBRACKET = 221;
  VK_QUOTE = 222;
  VK_LAST = VK_QUOTE;

  ExtendedVKeys: set of Byte = [
		VK_UP,
		VK_DOWN,
		VK_LEFT,
		VK_RIGHT,
		VK_HOME,
		VK_END,
		VK_PRIOR,
		VK_NEXT,
		VK_INSERT,
		VK_BACK,
		VK_DELETE];

const
  INVALIDKEY = $FFFF;
  VKKEYSCANSHIFTON = $01;
  VKKEYSCANCTRLON = $02;
  VKKEYSCANALTON = $04;

function SendKeys(SendKeysString: PChar; Wait: Boolean): Boolean;
var
  UsingParens, ShiftDown, ControlDown, AltDown, FoundClose: Boolean;
  PosSpace: Byte;
  I, L: Integer;
  NumTimes, MKey: Word;
  KeyString: String[20];

	function BitSet(BitTable, BitMask: Byte): Boolean;
	begin
	  Result := ByteBool(BitTable and BitMask);
	end;

	procedure SetBit(var BitTable: Byte; BitMask: Byte);
	begin
	  BitTable := BitTable or Bitmask;
	end;

	procedure KeyboardEvent(VKey, ScanCode: Byte; Flags: Longint);
	var
	  KeyboardMsg: TMsg;
	begin
	  keybd_event(VKey, ScanCode, Flags,0);
	  if Wait then
	  	while PeekMessage(KeyboardMsg, 0, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE) do
		  begin
		    TranslateMessage(KeyboardMsg);
		    DispatchMessage(KeyboardMsg);
		  end;
	end;

	procedure SendKeyDown(VKey: Byte; NumTimes: Word; GenUpMsg: Boolean);
	var
	  Count: Word;
	  ScanCode: Byte;
	  NumState: Boolean;
	  KeyBoardState: TKeyboardState;
	begin
	  if (VKey=VK_NUMLOCK) then
	  begin
	    NumState := ByteBool(GetKeyState(VK_NUMLOCK) and 1);
	    GetKeyBoardState(KeyBoardState);
	    if NumState then
	    	KeyBoardState[VK_NUMLOCK] := (KeyBoardState[VK_NUMLOCK] and not 1)
	    else
      	KeyBoardState[VK_NUMLOCK] := (KeyBoardState[VK_NUMLOCK] or 1);
	    SetKeyBoardState(KeyBoardState);
	    Exit;
	  end;
	  ScanCode := Lo(MapVirtualKey(VKey,0));
    for Count := 1 to NumTimes do
	    if VKey in ExtendedVKeys then
      begin
	      KeyboardEvent(VKey, ScanCode, KEYEVENTF_EXTENDEDKEY);
	      if(GenUpMsg) then
	        KeyboardEvent(VKey, ScanCode, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP)
	    end
      else
      begin
	      KeyboardEvent(VKey, ScanCode, 0);
	      if(GenUpMsg) then
        	KeyboardEvent(VKey, ScanCode, KEYEVENTF_KEYUP);
	    end;
	end;

	procedure SendKeyUp(VKey: Byte);
	var
	  ScanCode: Byte;
	begin
	  ScanCode := Lo(MapVirtualKey(VKey,0));
	  if VKey in ExtendedVKeys then
	    KeyboardEvent(VKey, ScanCode, KEYEVENTF_EXTENDEDKEY and KEYEVENTF_KEYUP)
	  else
      KeyboardEvent(VKey, ScanCode, KEYEVENTF_KEYUP);
	end;

	procedure SendKey(MKey: Word; NumTimes: Word; GenDownMsg: Boolean);
	begin
	  if(BitSet(Hi(MKey),VKKEYSCANSHIFTON)) then SendKeyDown(VK_SHIFT,1,False);
	  if(BitSet(Hi(MKey),VKKEYSCANCTRLON)) then SendKeyDown(VK_CONTROL,1,False);
	  if(BitSet(Hi(MKey),VKKEYSCANALTON)) then SendKeyDown(VK_MENU,1,False);
	  SendKeyDown(Lo(MKey), NumTimes, GenDownMsg);
	  if(BitSet(Hi(MKey),VKKEYSCANSHIFTON)) then SendKeyUp(VK_SHIFT);
	  if(BitSet(Hi(MKey),VKKEYSCANCTRLON)) then SendKeyUp(VK_CONTROL);
	  if(BitSet(Hi(MKey),VKKEYSCANALTON)) then SendKeyUp(VK_MENU);
	end;

	{ Implements a simple binary search to locate special key name strings }

	function StringToVKey(KeyString: ShortString): Word;
  var
    L, H, I, C: Integer;
  begin
    Result := INVALIDKEY;
    L := Low(SendKeyRecs);
    H := High(SendKeyRecs);
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := AnsiCompareText(SendKeyRecs[I].Name, KeyString);
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
  	      Result := SendKeyRecs[I].VKey;
          Break;
        end;
      end;
    end;
  end;

	procedure PopUpShiftKeys;
	begin
	  if not UsingParens then
    begin
	    if ShiftDown then SendKeyUp(VK_SHIFT);
	    if ControlDown then SendKeyUp(VK_CONTROL);
	    if AltDown then SendKeyUp(VK_MENU);
	    ShiftDown := False;
	    ControlDown := False;
	    AltDown := False;
	  end;
	end;

begin
  Result := False;
  UsingParens := False;
  ShiftDown := False;
  ControlDown := False;
  AltDown := False;
  I := 0;
  L := StrLen(SendKeysString);
  if L = 0 then
    Exit;
  while I < L do
    case SendKeysString[I] of
      '(':
        begin
            UsingParens := True;
            Inc(I);
          end;
      ')':
        begin
            UsingParens := False;
            PopUpShiftKeys;
            Inc(I);
          end;
      '&':
        begin
             AltDown := True;
             SendKeyDown(VK_MENU,1,False);
             Inc(I);
          end;
      '+':
         begin
             ShiftDown := True;
             SendKeyDown(VK_SHIFT,1,False);
             Inc(I);
           end;
      '^':
        begin
          ControlDown := True;
          SendKeyDown(VK_CONTROL,1,False);
          Inc(I);
        end;
      '{':
        begin
          NumTimes := 1;
          if SendKeysString[Succ(I)]= '{' then
          begin
            MKey := VK_LEFTBRACKET;
            SetBit(Wbytes(MKey)[1], VKKEYSCANSHIFTON);
            SendKey(MKey, 1, True);
            PopUpShiftKeys;
            Inc(I, 3);
            Continue;
          end;
          KeyString := '';
          FoundClose := False;
          while I <= L do
          begin
            Inc(I);
            if(SendKeysString[I]='}') then
            begin
              FoundClose := True;
              Inc(I);
              Break;
            end;
            KeyString := KeyString+Upcase(SendKeysString[I]);
          end;
          if not FoundClose then
            Exit;
          if SendKeysString[I] = '}' then
          begin
            MKey := VK_RIGHTBRACKET;
            SetBit(Wbytes(MKey)[1],VKKEYSCANSHIFTON);
            SendKey(MKey,1,True);
            PopUpShiftKeys;
            Inc(I);
            Continue;
          end;
          PosSpace := Pos(' ', KeyString);
          if(PosSpace<>0) then begin
             NumTimes := StrToInt(Copy(KeyString, Succ(PosSpace), Length(KeyString) - PosSpace));
             KeyString := Copy(KeyString, 1, Pred(PosSpace));
          end;
          if Length(KeyString) = 1 then
            MKey := vkKeyScan(KeyString[1])
          else
            MKey := StringToVKey(KeyString);
          if MKey = VK_PAUSE then
            Sleep(250)
          else if MKey <> INVALIDKEY then
          begin
            SendKey(MKey, NumTimes, True);
            PopUpShiftKeys;
            Continue;
          end;
        end;
      '~':
        begin
          SendKeyDown(VK_RETURN, 1, True);
          PopUpShiftKeys;
          Inc(I);
        end;
    else
      MKey := vkKeyScan(SendKeysString[I]);
      if MKey <> INVALIDKEY then
      begin
       SendKey(MKey, 1, True);
       PopUpShiftKeys;
      end;
      Inc(I);
    end;
  Result := True;
  PopUpShiftKeys;
end;

procedure SendCommand(Point: TPoint; Command: string; Button: TPuckButton = pbLeft);
const
	DownButton: array[TPuckButton] of Cardinal = (
  	MOUSEEVENTF_LEFTDOWN, MOUSEEVENTF_MIDDLEDOWN, MOUSEEVENTF_RIGHTDOWN);
	UpButton: array[TPuckButton] of Cardinal = (
  	MOUSEEVENTF_LEFTUP, MOUSEEVENTF_MIDDLEUP, MOUSEEVENTF_RIGHTUP);
var
  CurrentPoint: TPoint;
begin
  GetCursorPos(CurrentPoint);
  SetCursorPos(Point.X, Point.Y);
  mouse_event(DownButton[Button],	0, 0, 0, 0);
  mouse_event(UpButton[Button],	0, 0, 0, 0);
  SetCursorPos(CurrentPoint.X, CurrentPoint.Y);
  if Command <> '' then
    SendKeys(PChar(Command), True);
end;

end.
