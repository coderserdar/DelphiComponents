
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
 * The Original Code is TurboPower Essentials Vol I
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ES.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
  {$G+} {286 Instructions}
  {$N+} {Numeric Coprocessor}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsData;
  {-type, variable, and constant declarations}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs,{$ENDIF}
  Messages;

{$IFNDEF Win32}
type
  AnsiChar    = Char;
  PAnsiChar   = PChar;
  DWord       = LongInt;
  UINT        = Word;
  MMRESULT    = Word;
  AnsiString  = string;
  ShortString = string;
{$ENDIF}

{$IFNDEF VERSION3}
{$IFDEF NeedMouseWheel}                                                {!!.06}
const
  WM_MOUSEWHEEL = $020A;
  WHEEL_DELTA   = 120;
{$ENDIF}                                                               {!!.05}
{$ENDIF}

type
  {$IFDEF CBuilder}
  TEsHdc = Integer;
  {$ELSE}
  TEsHdc = hDC;
  {$ENDIF}

  TRGBMap = packed record
    case Byte of
      0 : (RGBValue : DWord);
      1 : (Red      : Byte;
           Green    : Byte;
           Blue     : Byte;
           Unused   : Byte);
  end;

const
  {message constants}
  ES_POSITIONLABEL       = WM_USER + 100;
  ES_RECORDLABELPOSITION = WM_USER + 101;
  ES_ASSIGNLABEL         = WM_USER + 102;

const
  VK_0 = Ord('0');
  VK_1 = Ord('1');
  VK_2 = Ord('2');
  VK_3 = Ord('3');
  VK_4 = Ord('4');
  VK_5 = Ord('5');
  VK_6 = Ord('6');
  VK_7 = Ord('7');
  VK_8 = Ord('8');
  VK_9 = Ord('9');

const
  EsVersionStr = '1.11'; {version string}

implementation

end.
