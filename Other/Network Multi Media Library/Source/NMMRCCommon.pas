(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMRCCommon;

interface

uses Classes, SysUtils, Windows;

const
  cmdMouseMove= 'MM:';
  cmdMouseDown= 'MD:';
  cmdMouseUp= 'MU:';
  cmdKeyDown= 'KD:';
  cmdKeyUp= 'KU:';
  cmdShellExec= 'EX:';
  cmdClipBoard= 'Clb:';

function PackShiftState(AShiftState: TShiftState): String;
function UnPackShiftState(AStr: String): TShiftState;
function GenMouseDownFlag(AShiftState: TShiftState): DWord;
function GenMouseUpFlag(AShiftState: TShiftState): DWord;
procedure KeysDown(AShiftState: TShiftState);
procedure KeysUp(AShiftState: TShiftState);
procedure GetXYShift(AInp: String; var X,Y: Integer; var Shift: TShiftState);
procedure GetKeyAndShift(AInp: String; var Key: Integer; var Shift: TShiftState);
procedure GetShellCommand(AInp: String; var Command: String);

implementation


function PackShiftState(AShiftState: TShiftState): String;
  function BoolToChar(ABool: Boolean): Char;
  begin
    if ABool then result:= '1' else result:= '0';
  end;
begin
  result:= BoolToChar(ssShift in AShiftState) +
           BoolToChar(ssAlt in AShiftState) +
           BoolToChar(ssCtrl in AShiftState) +
           BoolToChar(ssLeft in AShiftState) +
           BoolToChar(ssRight in AShiftState) +
           BoolToChar(ssMiddle in AShiftState) +
           BoolToChar(ssDouble in AShiftState);
end;

function UnPackShiftState(AStr: String): TShiftState;
  function CharToBool(AChar: Char): Boolean;
  begin
    result:= (AChar= '1');
  end;
begin
  result:= [];
  if CharToBool(AStr[1]) then result:= result + [ssShift];
  if CharToBool(AStr[2]) then result:= result + [ssAlt];
  if CharToBool(AStr[3]) then result:= result + [ssCtrl];
  if CharToBool(AStr[4]) then result:= result + [ssLeft];
  if CharToBool(AStr[5]) then result:= result + [ssRight];
  if CharToBool(AStr[6]) then result:= result + [ssMiddle];
  if CharToBool(AStr[7]) then result:= result + [ssDouble];
end;

procedure GetXYShift(AInp: String; var X,Y: Integer; var Shift: TShiftState);
var LSL: TStringList;
begin
  Shift:= [];
  LSL:= TStringList.Create;
  try
    LSL.Sorted:= false;
    LSL.Delimiter:= ';';
    LSL.DelimitedText:= AInp;
    X:= StrToInt(LSL.Values['X']);
    Y:= StrToInt(LSL.Values['Y']);
    if LSL.Values['S']<>'' then
       Shift:= UnPackShiftState(LSL.Values['S']);
  finally
    FreeAndNil(LSL);
  end;
end;

procedure GetShellCommand(AInp: String; var Command: String);
var LSL: TStringList;
begin
  LSL:= TStringList.Create;
  try
    LSL.Sorted:= false;
    LSL.Delimiter:= ';';
    LSL.DelimitedText:= AInp;
    Command:= LSL.Values['P'];
  finally
    FreeAndNil(LSL);
  end;
end;

procedure GetKeyAndShift(AInp: String; var Key: Integer; var Shift: TShiftState);
var LSL: TStringList;
begin
  Shift:= [];
  LSL:= TStringList.Create;
  try
    LSL.Sorted:= false;
    LSL.Delimiter:= ';';
    LSL.DelimitedText:= AInp;
    Key:= StrToInt(LSL.Values['K']);
    if LSL.Values['S']<>'' then
       Shift:= UnPackShiftState(LSL.Values['S']);
  finally
    FreeAndNil(LSL);
  end;
end;

function GenMouseDownFlag(AShiftState: TShiftState): DWord;
begin
  result:= 0;
  if ssLeft in AShiftState then
     result:= result + MOUSEEVENTF_LEFTDOWN;
  if ssRight in AShiftState then
     result:= result + MOUSEEVENTF_RIGHTDOWN;
  if ssMiddle in AShiftState then
     result:= result + MOUSEEVENTF_MIDDLEDOWN;
end;

function GenMouseUpFlag(AShiftState: TShiftState): DWord;
begin
  result:= 0;
  if ssLeft in AShiftState then
     result:= result + MOUSEEVENTF_LEFTUP;
  if ssRight in AShiftState then
     result:= result + MOUSEEVENTF_RIGHTUP;
  if ssMiddle in AShiftState then
     result:= result + MOUSEEVENTF_MIDDLEUP;
end;

procedure KeysDown(AShiftState: TShiftState);
begin
  if ssShift in AShiftState then
     keybd_event(VK_SHIFT,0,0,0);
  if ssCtrl in AShiftState then
     keybd_event(VK_CONTROL,0,0,0);
end;

procedure KeysUp(AShiftState: TShiftState);
begin
  if ssShift in AShiftState then
     keybd_event(VK_SHIFT,0,KEYEVENTF_KEYUP,0);
  if ssCtrl in AShiftState then
     keybd_event(VK_CONTROL,0,KEYEVENTF_KEYUP,0);
end;

end.
