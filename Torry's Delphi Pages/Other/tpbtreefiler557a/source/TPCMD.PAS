{$S-,R-,V-,I-,B-,F-}

{$IFNDEF Ver40}
  {$S-,O-,A-}
{$ENDIF}
{$I TPDEFINE.INC} {!!.21}

{*********************************************************}
{*                     TPCMD.PAS 5.22                    *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Portions Copyright (c) Sunny Hill Software 1995, 1996
 * and used under license to TurboPower Software
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit TpCmd;
  {-Convert keystrokes to commands. This unit is intended primarily for
    internal use.}

interface

type
  MatchType = (NoMatch, PartMatch, FullMatch);
const
  NoCmd = 0;                 {Returned by GetCommand for invalid keystroke}
  AlphaCmd = 1;              {Returned by GetCommand for alphanumeric char}
  MapWordStar : Boolean = True; {True to map second character to control char}

{**************************************************************
 KeySet is an array of byte, in the following form:
   (LengthByte, Key1, Key2, ..., CommandOrd,
   ...,
   0);
 LengthByte includes the number of keys plus 1 for CommandOrd.
**************************************************************}

function GetCommand(var KeySet; KeyPtr : Pointer; var ChWord : Word) : Byte;
  {-Get next command or character}

function AddCommandPrim(var KeySet;
                        LastKeyIndex : Word;
                        Cmd, NumKeys : Byte;
                        Key1, Key2 : Word) : Boolean;
  {-Add a new command key assignment or change an existing one}

procedure GetKeysForCommand(var KeySet;
                            Cmd : Byte;
                            var NumKeys : Byte;
                            var Key1, Key2 : Word);
  {-Search KeySet for Cmd, returning first set of matching keys.
    NumKeys = 0 if no match found}

  {--- the following routines, etc. are for installation programs ---}
const
  MaxKeys = 300;
  MaxCommands = 150;
  KeyLength = 6;
type
  KeyString = string[KeyLength];
  KeyRec =
    record
      Modified : Boolean;
      Conflict : Boolean;
      CommandCode : Byte;
      Keys : KeyString;
    end;
  UnpackedKeyArray = array[1..MaxCommands] of KeyRec;
  UnpackedKeyPtr = ^UnpackedKeyArray;
  PackedKeyArray = array[0..MaxKeys] of Byte;
  PackedKeyPtr = ^PackedKeyArray;

function UnpackKeys(var PackedKeys, UnpackedKeys;
                    MaxCmds : Word; Cols : Byte) : Word;
  {-Unpack keys into a fixed element array. Returns number of commands in
    PackedKeys.}

function PackKeys(var PackedKeys; NumCmds, MaxBytes : Word;
                  var UnpackedKeys) : Word;
  {-Convert fixed array into a packed list of keys again. Returns the number
    of keys that we *wanted* to store. Error if that number is greater than
    MaxBytes.}

function SizeKeys(var UnpackedKeys; NumCmds : Word) : Word;
  {-Return number of bytes in packed version of UnpackedKeys}

function ConflictsFound(var UnpackedKeys; NumCmds : Word) : Boolean;
  {-Check UnpackedKeys for conflicts. Returns True if Conflicts were found}

  {--- the following routine is intended for internal use ---}

function CheckForKeyConflict(var KeySet;
                             LastKeyIndex : Word;
                             Cmd, NumKeys : Byte;
                             Key1, Key2 : Word) : MatchType;
  {-Check to see if the specified key combination conflicts with an existing
    one}

  {======================================================}

implementation

type
  KeyArray = array[0..32000] of Byte;
  KeyArrayPtr = ^KeyArray;
  CmdBuffArray = array[0..5] of Byte;

  function WordStarCommand(K : Byte) : Byte;
    {-Return ^C, 'C', or 'c' as ^C, etc.}
  var
    C : Char absolute K;
  begin
    C := Upcase(C);
    case C of
      'A'..'_' :
        WordStarCommand := K-64;
    else
      WordStarCommand := K;
    end;
  end;

  function ScanCommands(K : KeyArrayPtr;
                        var CmdBuffer : CmdBuffArray;
                        BufNext : Word;
                        var Cmd : Byte;
                        var FoundAt : Word) : MatchType;
    {-Scan K^ for a match on CmdBuffer}
  var
    BufIndex : Word;
    CmdIndex : Word;
    CmdLen : Byte;
    Matching : Boolean;
  begin
    Cmd := NoCmd;
    CmdIndex := 0;
    CmdLen := K^[CmdIndex];

    {Scan the command list}
    while CmdLen <> 0 do begin
      FoundAt := CmdIndex;
      Inc(CmdIndex);
      BufIndex := 0;
      Matching := True;
      while Matching and (BufIndex < BufNext) and (BufIndex < CmdLen-1) do
        if CmdBuffer[BufIndex] = K^[CmdIndex+BufIndex] then
          Inc(BufIndex)
        else
          Matching := False;
      if not Matching then begin
        {No match, try next command}
        Inc(CmdIndex, CmdLen);
        CmdLen := K^[CmdIndex];
      end else begin
        if BufNext = CmdLen-1 then begin
          {Complete match}
          ScanCommands := FullMatch;
          Cmd := K^[CmdIndex+BufIndex];
        end else
          ScanCommands := PartMatch;
        Exit;
      end;
    end;

    {No match if we get here}
    ScanCommands := NoMatch;
  end;

  function GetCommand(var KeySet; KeyPtr : Pointer; var ChWord : Word) : Byte;
    {-Get next command or character.
      Returns NoCmd for no matching command, AlphaCmd for alphabetic character.}
  var
    LCh : Byte;
    Cmd : Byte;
    Junk : Word;
    BufNext : Word;
    Done : Boolean;
    CmdBuffer : CmdBuffArray;

    function GetKeyWord : Word;
      {-Call routine pointed to by KeyPtr}
    inline($FF/$5E/<KeyPtr); {Call dword ptr [bp+<KeyPtr]}

  begin
    BufNext := 0;
    Cmd := NoCmd;
    Done := False;
    repeat
      {Get the next keystroke}
      ChWord := GetKeyWord;

      LCh := Lo(ChWord);
      if LCh = 0 then begin
        {Extended keystroke}
        CmdBuffer[BufNext] := 0;
        Inc(BufNext);
        LCh := Hi(ChWord);
      end
      else if (BufNext > 0) and MapWordStar then
        {Map WordStar keystrokes}
        LCh := WordStarCommand(LCh);
      CmdBuffer[BufNext] := LCh;
      Inc(BufNext);

      {Map to a command}
      case ScanCommands(@KeySet, CmdBuffer, BufNext, Cmd, Junk) of
        FullMatch : Done := True;
        NoMatch :
          begin
            {Return alphanumeric character if it isn't a command}
            if (BufNext = 1) and (Char(LCh) >= ' ') and (Char(LCh) <> #127) then
              Cmd := AlphaCmd;
            Done := True;
          end;
      end;
    until Done;

    GetCommand := Cmd;
  end;

  procedure InitCmdBuffer(var CmdBuffer : CmdBuffArray;
                          NumKeys       : Byte;
                          Key1, Key2    : Word;
                          var BufNext   : Word);
    {-Initialize a CmdBuffArray}
  begin
    if Lo(Key1) = 0 then begin
      CmdBuffer[0] := 0;
      CmdBuffer[1] := Hi(Key1);
      BufNext := 2;
    end
    else begin
      CmdBuffer[0] := Lo(Key1);
      BufNext := 1;
    end;
    if NumKeys = 2 then
      if Lo(Key2) = 0 then begin
        CmdBuffer[BufNext] := 0;
        Inc(BufNext);
        CmdBuffer[BufNext] := Hi(Key2);
        Inc(BufNext);
      end
      else begin
        CmdBuffer[BufNext] := Lo(Key2);
        Inc(BufNext);
      end;
  end;

  function CheckForKeyConflict(var KeySet;
                               LastKeyIndex : Word;
                               Cmd, NumKeys : Byte;
                               Key1, Key2 : Word) : MatchType;
    {-Check to see if the specified key combination conflicts with an existing
      one}
  var
    MT      : MatchType;
    BufNext : Word;
    CTmp    : Byte;
    FoundAt : Word;
    CmdBuffer : CmdBuffArray;
  begin
    if NumKeys = 0 then
      MT := NoMatch
    else begin
      {set up for the search}
      InitCmdBuffer(CmdBuffer, NumKeys, Key1, Key2, BufNext);

      {check for duplicate}
      MT := ScanCommands(@KeySet, CmdBuffer, BufNext, CTmp, FoundAt);
    end;

    CheckForKeyConflict := MT;
  end;

  function AddCommandPrim(var KeySet;
                          LastKeyIndex : Word;
                          Cmd, NumKeys : Byte;
                          Key1, Key2 : Word) : Boolean;
    {-Add a new command key assignment or change an existing one}
  var
    EditKeys : KeyArray absolute KeySet;
    CTmp : Byte;
    SlotFound : Boolean;
    CmdLen,
    FoundAt : Word;
    MT : MatchType;
    NextCmdIndex : Word;
    BufNext : Word;
    CmdBuffer : CmdBuffArray;
  begin
    AddCommandPrim := False;
    if (NumKeys < 1) or (NumKeys > 2) then
      Exit;

    {set up for the search}
    InitCmdBuffer(CmdBuffer, NumKeys, Key1, Key2, BufNext);

    {check for duplicate}
    MT := ScanCommands(@KeySet, CmdBuffer, BufNext, CTmp, FoundAt);
    case MT of
      FullMatch :
        begin
          {change the command}
          CmdLen := EditKeys[FoundAt];
          if Cmd = NoCmd then begin
            {Disable the keystrokes as well}
            NextCmdIndex := FoundAt+1;
            while NextCmdIndex < FoundAt+CmdLen do begin
              EditKeys[NextCmdIndex] := $FF;
              Inc(NextCmdIndex);
            end;
          end;
          EditKeys[FoundAt+CmdLen] := Cmd;
          AddCommandPrim := True;
          Exit;
        end;
      PartMatch :
        Exit;
    end;

    {find next available command slot}
    NextCmdIndex := 0;
    SlotFound := False;
    while not SlotFound and (EditKeys[NextCmdIndex] <> 0) do begin
      CmdLen := EditKeys[NextCmdIndex];
      if EditKeys[NextCmdIndex+CmdLen] = NoCmd then
        {Command slot is available for reuse}
        if BufNext+1 = CmdLen then
          {Slot is the right size}
          SlotFound := True;
      if not SlotFound then
        Inc(NextCmdIndex, EditKeys[NextCmdIndex]+1);
    end;

    {make sure it will fit}
    if (BufNext+2) <= (LastKeyIndex-NextCmdIndex) then begin
      {plug in the key}
      EditKeys[NextCmdIndex] := BufNext+1;
      Inc(NextCmdIndex);
      Move(CmdBuffer, EditKeys[NextCmdIndex], BufNext);
      Inc(NextCmdIndex, BufNext);
      EditKeys[NextCmdIndex] := Cmd;
      Inc(NextCmdIndex);

      AddCommandPrim := True;
    end;
  end;

  procedure GetKeysForCommand(var KeySet;
                              Cmd : Byte;
                              var NumKeys : Byte;
                              var Key1, Key2 : Word);
    {-Search KeySet for Cmd, returning first set of matching keys.
      NumKeys = 0 if no match found}
  var
    Keys : KeyArray absolute KeySet;
    Kofs : Word;
    TKey : Word;
    Klen : Integer;
  begin
    NumKeys := 0;
    Kofs := 0;
    repeat
      Klen := Keys[Kofs];
      if Klen <> 0 then
        if Keys[Kofs+Klen] = Cmd then begin
          {Matches command}
          {Reduce length by one to avoid Cmd}
          Dec(Klen);
          repeat
            {Get next key byte}
            Inc(Kofs);
            Dec(Klen);
            if Keys[Kofs] = 0 then begin
              {Extended keystroke}
              Inc(Kofs);
              Dec(Klen);
              TKey := Word(Keys[Kofs]) shl 8;
            end else
              {Normal keystroke}
              TKey := Keys[Kofs];

            {Store the keys}
            Inc(NumKeys);
            if NumKeys = 1 then
              Key1 := TKey
            else if NumKeys = 2 then
              Key2 := TKey;
          until Klen <= 0;

          {Don't allow more than two keys}
          if NumKeys > 2 then
            NumKeys := 2;
          Exit;
        end;
      Inc(Kofs, Klen+1);
    until Klen = 0;
    {No match}
  end;

  function UnpackKeys(var PackedKeys, UnpackedKeys;
                      MaxCmds : Word; Cols : Byte) : Word;
    {-Unpack keys into a fixed element array. Returns number of commands in
      PackedKeys.}
  var
    PK : PackedKeyArray absolute PackedKeys;
    UK : UnpackedKeyArray absolute UnpackedKeys;
    Count, CmdNum, KeyOfs : Word;
    I, Len : Word;
  label
    Done;
  begin
    if Cols = 0 then
      Cols := 1;
    FillChar(UK, MaxCmds*SizeOf(KeyRec), 0);
    for I := 1 to MaxCmds do
      with UK[I] do
        CommandCode := (Pred(I) div Cols)+1;
    KeyOfs := 0;
    Count := 0;
    while PK[KeyOfs] <> 0 do begin
      Inc(Count);
      Len := PK[KeyOfs];

      {find an unused entry in the proper row}
      CmdNum := Word(PK[KeyOfs+Len]-1)*3+1;
      for I := 1 to Cols do
        with UK[CmdNum] do
          if Length(Keys) = 0 then
            with UK[CmdNum] do begin
              Move(PK[KeyOfs], Keys, Len);
              Dec(Keys[0]);
              goto Done;
            end
          else
            Inc(CmdNum);
Done:
      Inc(KeyOfs, Len+1);
    end;

    UnpackKeys := Count;
  end;

  function PackKeys(var PackedKeys; NumCmds, MaxBytes : Word;
                    var UnpackedKeys) : Word;
    {-Convert fixed array into a packed list of keys again. Returns the number
      of keys that we *wanted* to store. Error if that number is greater than
      MaxBytes.}
  var
    PK : PackedKeyArray absolute PackedKeys;
    UK : UnpackedKeyArray absolute UnpackedKeys;
    Len : Byte;
    CmdNum : Word;
    KeyOfs : Word;
    KeyNew : Word;
  begin
    FillChar(PK, MaxBytes, 0);
    KeyOfs := 0;
    for CmdNum := 1 to NumCmds do
      with UK[CmdNum] do
        if Length(Keys) <> 0 then begin
          Len := Length(Keys)+1;
          KeyNew := KeyOfs+Len+1;
          if KeyNew <= MaxBytes then begin
            {Store the keys if they fit}
            Inc(Keys[0]);
            Move(Keys, PK[KeyOfs], Len);
            PK[KeyNew-1] := CommandCode;
          end;
          KeyOfs := KeyNew;
        end;

    {Return the number of keys we wanted to store}
    PackKeys := KeyOfs;
  end;

  function SizeKeys(var UnpackedKeys; NumCmds : Word) : Word;
    {-Return number of bytes in packed version of UnpackedKeys}
  var
    UK : UnpackedKeyArray absolute UnpackedKeys;
    CmdNum : Word;
    Size : Word;
  begin
    Size := 0;
    for CmdNum := 1 to NumCmds do
      with UK[CmdNum] do
        if Length(Keys) <> 0 then
          Inc(Size, Length(Keys)+2);
    SizeKeys := Size;
  end;

  function ConflictsFound(var UnpackedKeys; NumCmds : Word) : Boolean;
    {-Check UnpackedKeys for conflicts. Returns False if Conflicts found}
  var
    I, J : Word;
    UK : UnpackedKeyArray absolute UnpackedKeys;
  begin
    {assume success}
    ConflictsFound := False;

    {turn off all Conflict flags}
    for I := 1 to NumCmds do
      UK[I].Conflict := False;

    {check for conflicts}
    for I := 1 to NumCmds do
      with UK[I] do
        if Length(Keys) <> 0 then
          for J := 1 to NumCmds do
            if (J <> I) and (Length(UK[J].Keys) <> 0) then
              if Pos(UK[J].Keys, Keys) = 1 then begin
                UK[I].Conflict := True;
                UK[J].Conflict := True;
                ConflictsFound := True;
              end;
  end;

end.
