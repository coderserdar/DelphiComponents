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
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{--Conditional defines and compiler options}
{$I BTDefine.Inc}
{$F+,V-,B-,S-,I-,R-}
{$IFDEF CanAllowOverlays}
  {$O+,F+}
{$ENDIF}
{$IFDEF CanSetOvrflowCheck}
  {$Q-}
{$ENDIF}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit Browser;

interface

{won't work under Windows}
{$IFDEF Windows}
  **ERROR** Not compatible with Turbo Pascal for Windows
{$ENDIF}

{If using Turbo Professional or Object Professional, pull in ??DEFINE.INC
 for UseMouse conditional}

{$IFDEF UseTPCRT}
  {$I TPDEFINE.INC}
{$ENDIF}

{$IFDEF UseOPCRT}
  {$I OPDEFINE.INC}
{$ENDIF}

uses
  TpCmd,

  {the following set of conditionals controls which CRT unit is linked in.
   It also controls if a mouse unit is pulled in.}

  {$IFNDEF UseOPCRT}
    {$IFDEF UseTPCRT}
      TpCrt,
        {$IFDEF UseMouse}
          TpMouse,
        {$ENDIF}
    {$ELSE}
      Crt,
    {$ENDIF}
  {$ELSE}
    OpCrt,
    {$IFDEF UseMouse}
      OpMouse,
    {$ENDIF}
  {$ENDIF}
  Dos,
  BTBase,
  BTIsBase,
  Filer,
  VRec;

const
  MinRows = 4;                    {Minimum number of usable lines on the screen}
  MaxRows = 20;                   {Maximum number of usable lines on the screen}
  MaxCols = 128;                  {Number of usable columns on screen, must be <= 255}

  {Number of retries for a read operation in case of a Lock-Error}
  RetriesOnLock : Integer = 50;

  {True lets browser go to disk less often, safe only for single user mode}
  NoNetMode : Boolean = False;

  {If True, Browser will use read locks while building its pages}
  UseReadLock : Boolean = False;

  {If False, the data record will not be read prior to calling BuildARow}
  ReadDataRecord : Boolean = True;

var
  BrowseYL : Byte;
  BrowseYH : Byte;

type
  RowRec =
    record
      IKS : IsamKeyStr;
      Ref : LongInt;
      Row : string[MaxCols];
    end;
  {Basic type of the browser. The browser is used in three steps:
   1) <IKS> and <Ref> are allocated.
   2) <ProcBuildaRow> (procedure parameter) determines the row.
   3) <ProcDisplayaRow> (procedure parameter) puts <Row> on the screen}

  {----------------- programmer's hooks -------------}
var
  BrowseKeyPtr : Pointer;      {pointer to routine to return next keystroke}
  BrowseHelpPtr : Pointer;     {pointer to routine to display help}
  BrowseHelpIndex : Word;      {current help topic}

  {.F-}
const
  BKnone      = 00; {Not a command}
  BKchar      = 01; {Regular character--not a command}
  BKenter     = 02; {Select}
  BKquit      = 03; {Escape}
  BKfirstRec  = 04; {Cursor to first record}
  BKlastRec   = 05; {Cursor to last record}
  BKleft      = 06; {Cursor left one column}
  BKright     = 07; {Cursor right one column}
  BKup        = 08; {Cursor up one row}
  BKdown      = 09; {Cursor down one row}
  BKpageUp    = 10; {Cursor up one page}
  BKpageDown  = 11; {Cursor down one page}
  BKplus      = 12; {Reread current record}
  BKhelp      = 13; {Invoke help routine}
  BKredraw    = 14; {redraw the browse screen}
  BKprobe     = 15; {signals a mouse event}
  BKRowEnd    = 16; {command to go to end of row}
  BKRowBegin  = 17; {comamnd to go to start of row}

  BKtask0     = 18; {user-defined task commands}
  BKtask1     = 19;
  BKtask2     = 20;
  BKtask3     = 21;
  BKtask4     = 22;
  BKtask5     = 23;
  BKtask6     = 24;
  BKtask7     = 25;
  BKtask8     = 26;
  BKtask9     = 27;
  BKuser0     = 28; {user-defined exit commands}
  BKuser1     = 29;
  BKuser2     = 30;
  BKuser3     = 31;
  BKuser4     = 32;
  BKuser5     = 33;
  BKuser6     = 34;
  BKuser7     = 35;
  BKuser8     = 36;
  BKuser9     = 37;

type
  BKtype = BKnone..BKuser9;
const
  {$IFDEF UseTPCRT}
  HelpForBrowse = TpCrt.HelpForXXXX2; {= 7}
  {$ELSE}
    {$IFDEF UseOPCRT}
    HelpForBrowse = 99;
    {$ELSE}
    HelpForBrowse = 7;
    {$ENDIF}
  {$ENDIF}

  {Keystroke to command mapping}
  BrowseKeyMax = 200;   {last available slot in BrowseKeySet}

  {ID string for installation programs}
  BrowseKeyID : string[17] = 'browser key array';

  {default key assignments}
  BrowseKeySet : array[0..BrowseKeyMax] of Byte = (
   {length keys         command type     key sequence}
    3,     $00, $00,    BKquit,          {^Break}
    3,     $00, $3B,    BKhelp,          {F1}
    3,     $00, $47,    BKfirstRec,      {Home}
    3,     $00, $48,    BKup,            {Up}
    3,     $00, $49,    BKpageUp,        {PgUp}
    3,     $00, $4B,    BKleft,          {Left}
    3,     $00, $4D,    BKright,         {Right}
    3,     $00, $4F,    BKlastRec,       {End}
    3,     $00, $50,    BKdown,          {Down}
    3,     $00, $51,    BKpageDown,      {PgDn}
    3,     $00, $76,    BKlastrec,       {^PgDn}
    3,     $00, $84,    BKfirstrec,      {^PgUp}
    2,     $03,         BKpageDown,      {^C}
    2,     $04,         BKright,         {^D}
    2,     $05,         BKup,            {^E}
    2,     $0D,         BKenter,         {^M, Enter}
    2,     $12,         BKpageUp,        {^R}
    2,     $13,         BKleft,          {^S}
    2,     $17,         BKup,            {^W}
    2,     $18,         BKdown,          {^X}
    2,     $1A,         BKdown,          {^Z}
    2,     $1B,         BKquit,          {Esc}
    2,     $2B,         BKplus,          {+}
    3,     $11, $03,    BKlastrec,       {^Q^C}
    3,     $11, $12,    BKfirstrec,      {^Q^R}
    {-----------pad to end of array----------}
    {$IFDEF UseMouse}
    3, $00, $EF, BKprobe,    {Click left}
    3, $00, $EE, BKquit,     {Click right}
    3, $00, $ED, BKhelp,     {Click both}
    {$ELSE}
                            0, 0,        {90}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,        {100}
    {$ENDIF}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,        {110}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,        {120}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,        {130}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,        {140}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,        {150}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,        {160}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,        {170}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,        {180}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,        {190}
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0);       {200}
{.F+}

  RowsToJump : Integer = 0;

  {$IFDEF UseMouse}
  {Mouse control for Browser}
  BrowseMouseEnabled : Boolean = False; {True if mouse is enabled}
  ScrollBarAttr : Byte = $07;
  SliderAttr : Byte = $0F;
  MouseUpMark : Char = #24;              {Characters in scroll bar}
  MouseDnMark : Char = #25;
  ScrollMark : Char = #178;
  ScrollVertChar : Char = #176;
  UserMousePtr : Pointer = Nil;          {Hot spot action routine}
  BrowseMousePage : Boolean = False; {True to scroll by one page per click}
  AutoScaleMouse : Boolean = True;  {Adjust for LowKey and HighKey}
  UseScrollBar : Boolean = True; {True to use scroll bars if mouse installed}
  ScrollBarAutoSize : Boolean = True; {True to match bar to window height}
  ScrollBarUp  : Byte = 1;    {relative location of the up arrow}
  ScrollBarHt  : Byte  = 18;  {the height of bar, excluding the arrows}
  ScrollBarCol : Byte = 80;   {absolute column for scroll bar}
  MouseX1      : Byte = 1;    {Left margin for mouse select}      {!!.06}
  MouseX2      : Byte = 79;   {Right margin for mouse select}     {!!.06}
  {$ENDIF}

  RefreshFunc  : Pointer = Nil; {Ptr to refresh function}         {!!.06}

function Browse(IFBPtr : IsamFileBlockPtr;
                VarRec : Boolean;
                KeyNr : Integer;
                LowKey : IsamKeyStr;
                HighKey : IsamKeyStr;
                StartScreenRow : Integer;
                NrOfRows : Integer;
                var DatS;
                var DatLen : Word;
                var Ref : LongInt;
                var KeyStr : IsamKeyStr;
                var ExitKey : BKtype;
                ProcSpecialTask : Pointer;
                ProcBuildaRow : Pointer;
                ProcDisplayaRow : Pointer) : Integer;
    {-A list is created with the corresponding keys from <LowKey> to <HighKey>
      inclusive and their data structures.

      The parameter <VarRec> must be true for variable record lengths
      (otherwise false).

      The display starts at the row <StartScreenRow> and entails <NrOfRows>
      rows. <NrOfRows> may not be less than 4.

      <DatS> is a buffer for the data structure.

      <DatLen> contains the length of the current variable length record only
      if <VarRec> is true.

      <ExitKey> returns the code for the command that exited the Browser. In
      addition this determines the first action the Browser should perform.
      BKnone (Byte 0) must be passed if no action is to be performed.

      The procedures whose addresses are passed through the pointer-parameters
      <ProcBuildaRow> and <ProcDisplayaRow> are used to create and display a
      row. These procedures must be supplied by the user of Browser and must
      be declared as in the example procedure definition. These procedures
      must be declared as "FAR." This is either done through the use of the
      compiler directive $F+, or through the exportation from another Unit.
      Handling it in any other way will produce a program crash.

      There are three possible return values:
        0: No error encountered.
           <ExitKey> = BKquit  : Esc was pressed.
           <ExitKey> <> BKquit : <DatS> contains the data structure,
                                 <Ref> the corresponding reference,
                                 <KeyStr> the chosen key.
        1: There were no keys available that were in the requested range.
        2: Hard Error by Isam-Access (Class 2 or higher).

      Browser has the following default actions for each command:
        BKquit     : exit Browser without making a selection
        BKenter    : exit Browser making a choice
        BKfirstRec : scroll to the first data structure
        BKlastRec  : scroll to the last data structure
        BKpageUp   : scroll back a page
        BKpageDown : scroll forward a page
        BKup       : scroll up a row
        BKdown     : scroll down a row

      If <ProcDisplayaRow> points to a procedure that makes use of the
      parameter <HorizOfs>, horizontal scrolling is available through the
      following commands:
        BKleft  : Move the display window towards the left
        BKright : Move the display window towards the right.}

function BrowseAgain(IFBPtr : IsamFileBlockPtr;
                     VarRec : Boolean;
                     KeyNr : Integer;
                     LowKey : IsamKeyStr;
                     HighKey : IsamKeyStr;
                     StartScreenRow,
                     NrOfRows : Integer;
                     var HighlightedRow : Integer;
                     var HorizOfs : Integer;
                     var DatS;
                     var DatLen : Word;
                     var Ref : LongInt;
                     var KeyStr : IsamKeyStr;
                     var ExitKey : BKtype;
                     ProcSpecialTask : Pointer;
                     ProcBuildaRow : Pointer;
                     ProcDisplayaRow : Pointer) : Integer;
  {-Same as Browse, except takes a value for the row to position the highlight
    bar on initially. If <HighlightedRow> if 0, then the browser will position
    the scroll bar for you. Otherwise <HighlightedRow> must be between 1 and
    <NrOfRows>. The value <HorizOfs> is also passed. This value is passed to
    the user procedure <ProcDislayaRow> to indicate the horizontal offset of
    the line.}

function AddBrowseCommand(Cmd : BKtype; NumKeys : Byte; Key1, Key2 : Word) : Boolean;
  {-Add a new command key assignment or change an existing one}

function BrowseReadKey : Word;
  {-Return next keystroke as a word}

procedure EnableFiltering(ValidateFunc : Pointer);
  {-Enables Browser filtering. <ValidateFunc> is a pointer to a user defined
    function that determines whether a given record should be displayed in the
    Browser. The <ValidateFunc> must be a FAR, non-nested function declared as
    follows:

    function ValidateARecord(IFBPtr     : IsamFileBlockPtr;
                             KeyNr      : Integer;
                             Ref        : LongInt;
                             var KeyStr : IsamKeyStr;
                             NetUsed    : Boolean) : Boolean;
  }

procedure DisableFiltering;
  {-Disables Browser filtering. Has no effect if filtering is not enabled.}

function IsFilteringEnabled : Boolean;
  {-Returns True if Browser filtering is enabled.}

{$IFDEF UseMouse}
procedure EnableBrowseMouse;
  {-Enable mouse control of pick lists}

procedure DisableBrowseMouse;
  {-Disable mouse control of pick lists}
{$ENDIF}

{!!.06 begin}
function RefreshAtEachCommand(IFBPtr : IsamFileBlockPtr;
                              KeyNo : Integer) : Boolean;
  {-Check for need to refresh before each command if no keystrokes pending}

const
  RefreshPeriod : Word = 90; {about 5 seconds}

function RefreshPeriodically(IFBPtr : IsamFileBlockPtr;
                             KeyNo : Integer) : Boolean;
  {-Check for need to refresh every RefreshPeriod clock ticks}
{!!.06 end}
  {=========================================================================}

implementation

{$IFDEF UseMouse}
const
  MouseScale : Word = 10000;
  PrevSlid   : Byte = 0;            {Previous scroll bar slider position}
  SliderPos  : Byte = 0;            {position of the scroll bar slider}
  SliderFunc : Pointer = nil;       {Pointer to routine that gets mouse slider pos}
  ScrollProc : Pointer = nil;       {Pointer to routine that scrolls using slider pos}
  MouseUpCmd : array[Boolean] of BKType = (BKUp, BKPageUp);
  MouseDownCmd : array[Boolean] of BKType = (BKDown, BKPageDown);
  ScrollBarPtr : string[1] = ' ';
  ScrollBarTop : Byte = 2;    {of the top of the bar}
  ScrollBarBot : Byte = 18;   {of the bottom of the bar}
  ScrollBarDn  : Byte  = 19;  {of the down arrow}
  MouseLowScale  : Word = 0;
  MouseHighScale : Word = 0;
{$ENDIF}

const
  RepositionDesired : Boolean = False;
  FilterState       : Boolean = False;

  {------------- Browser filtering hooks ------------}
const
  SearchKeyPtr      : Pointer = Nil;
  NextKeyPtr        : Pointer = Nil;
  PrevKeyPtr        : Pointer = Nil;
  FindKeyAndRefPtr  : Pointer = Nil;
  ValidatePtr       : Pointer = Nil;

type
  RowRange   = 1..MaxRows;
  BrowScreenType = array[RowRange] of RowRec;

var
  CurRow     : RowRange;
  gHighKey   : IsamKeyStr;                 {!!.22}
  gLowKey    : IsamKeyStr;                 {!!.22}

  procedure HelpRoutine(UnitCode : Byte; IdPtr : Pointer; HelpIndex : Word);
    {-Call routine pointed to by BrowseHelpPtr}
  inline(
    $FF/$1E/>BrowseHelpPtr);   {call dword ptr [>BrowseHelpPtr]}

  function GetKey : Word;
    {-Call routine pointed to by BrowseKeyPtr}
  inline(
    $FF/$1E/>BrowseKeyPtr);    {call dword ptr [>BrowseKeyPtr]}

  function KeyInBounds(KeyStr : IsamKeyStr) : Boolean;    {!!.22 moved}
  begin
    KeyInBounds := False;
    if Copy(KeyStr, 1, Length(gLowKey)) < gLowKey then    {!!.22}
      Exit;
    if Copy(KeyStr, 1, Length(gHighKey)) > gHighKey then  {!!.22}
      Exit;
    KeyInBounds := True;
  end;

  function BrowseReadKey : Word;
    {-Return next keystroke as a word}
  var
    Ch : Char;
  begin
    Ch := ReadKey;
    if Ch <> #0 then
      BrowseReadKey := Ord(Ch)
    else
      BrowseReadKey := Word(Ord(ReadKey)) shl 8;
  end;

  {$IFDEF UseMouse}
  procedure DrawMouseScrollBar;
    {-Draw the mouse scroll bar}
  var
    S : String;
    SLen : Byte absolute S;

  begin
    if not UseScrollBar then
      Exit;
    SLen := ScrollBarHt + 2;
    FillChar(S[2], SCrollBarHt, ScrollVertChar);
    S[1] := MouseUpMark;                                   {!!.40}
    S[SLen] := MouseDnMark;                                {!!.40}
    FastVert(S, ScrollBarUp, ScrollBarCol, ScrollBarAttr);
  end;
  {$ENDIF}

  function Validate(IFBPtr     : IsamFileBlockPtr;
                    Key        : Integer;
                    Ref        : LongInt;
                    var KeyStr : IsamKeyStr;
                    NetUsed    : Boolean) : Boolean;
    inline($FF/$1E/>ValidatePtr); {call dword ptr [>ValidatePtr]}

  {$F+}
  procedure Next_Key(IFBPtr         : IsamFileBlockPtr;
                     Key            : Integer;
                     var UserDatRef : LongInt;
                     var UserKey    : IsamKeyStr;
                     NetUsed        : Boolean);
  var
    RT : Integer;
  begin
    RT := 0;
    repeat
      BTNextKey(IFBPtr, Key, UserDatRef, UserKey);
      Inc(RT);
    until (RT >= RetriesOnLock) or (BTIsamErrorClass <> 2);
  end;

  procedure Search_Key(IFBPtr         : IsamFileBlockPtr;
                       Key            : Integer;
                       var UserDatRef : LongInt;
                       var UserKey    : IsamKeyStr;
                       NetUsed        : Boolean);
  var
    RT : Integer;
  begin
    RT := 0;
    repeat
      BTSearchKey(IFBPtr, Key, UserDatRef, UserKey);
      Inc(RT);
    until (RT >= RetriesOnLock) or (BTIsamErrorClass <> 2);
  end;

  procedure Prev_Key(IFBPtr : IsamFileBlockPtr;
                     Key : Integer;
                     var UserDatRef : LongInt;
                     var UserKey : IsamKeyStr;
                     NetUsed        : Boolean);
  var
    RT : Integer;
  begin
    RT := 0;
    repeat
      BTPrevKey(IFBPtr, Key, UserDatRef, UserKey);
      Inc(RT);
    until (RT >= RetriesOnLock) or (BTIsamErrorClass <> 2);
  end;

  procedure Find_Key(IFBPtr         : IsamFileBlockPtr;
                     Key            : Integer;
                     var UserDatRef : LongInt;
                     var UserKey    : IsamKeyStr;
                     NFSD           : Integer;
                     NetUsed        : Boolean);
  var
    RT : Integer;

  begin
    RT := 0;
    repeat
      BTFindKeyAndRef(IFBPtr, Key, UserDatRef, UserKey, NFSD);
      Inc(RT);
    until (RT >= RetriesOnLock) or (BTIsamErrorClass <> 2);
  end;

  procedure FilterNextKey(IFBPtr         : IsamFileBlockPtr;
                          Key            : Integer;
                          var UserDatRef : LongInt;
                          var UserKey    : IsamKeyStr;
                          NetUsed        : Boolean);
  var
    Done : Boolean;
  begin
    Next_Key(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
    Done := False;                                         {!!.22 begin}
    while IsamOK and (not Done) and KeyInBounds(UserKey) do begin
      Done := Validate(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
      if (not Done) and IsamOK then
        Next_Key(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
    end;                                                   {!!.22 end}
  end;

  procedure FilterSearchKey(IFBPtr         : IsamFileBlockPtr;
                            Key            : Integer;
                            var UserDatRef : LongInt;
                            var UserKey    : IsamKeyStr;
                            NetUsed        : Boolean);
  var
    Done : Boolean;
  begin
    Search_Key(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
    Done := False;                                         {!!.22 begin}
    while IsamOK and (not Done) and KeyInBounds(UserKey) do begin
      Done := Validate(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
      if (not Done) and (IsamOK) then
        Next_Key(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
    end;                                                   {!!.22 end}
    if BTIsamErrorClass = 1 then
      IsamError := 10210;
  end;

  procedure FilterPrevKey(IFBPtr : IsamFileBlockPtr;
                          Key : Integer;
                          var UserDatRef : LongInt;
                          var UserKey : IsamKeyStr;
                          NetUsed        : Boolean);
  var
    Done : Boolean;
  begin
    Prev_Key(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
    Done := False;                                         {!!.07}
    while IsamOK and (not Done) and KeyInBounds(UserKey) do begin
      Done := Validate(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
      if (not Done) and IsamOK then
        Prev_Key(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
    end;                                                   {!!.07}
  end;

  procedure FilterFindKey(IFBPtr         : IsamFileBlockPtr;
                          Key            : Integer;
                          var UserDatRef : LongInt;
                          var UserKey    : IsamKeyStr;
                          NFSD           : Integer;
                          NetUsed        : Boolean);
  var
    Done : Boolean;
  begin
    Find_Key(IFBPtr, Key, UserDatRef, UserKey, NFSD, NetUsed);
    Done := False;                                         {!!.22 begin}
    while IsamOK and (not Done) and KeyInBounds(UserKey) do begin
      Done := Validate(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
      if (not Done) and (IsamOK) then
        case NFSD of
          0 : begin {no direction}
                IsamOK := False;
                IsamError := 10270;
              end;
          1 : Next_Key(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
          else
            Prev_Key(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
        end;
    end;                                                   {!!.22 end}
  end;

  {$F-}

  {$I BROWSER.INC}    {include the BrowseI function}

  function GetCursorMode : Word;
  const
    Video = $10;
    GetCurType = $300;
  var
    Regs : Registers;
  begin
    Regs.AX := GetCurType;
    Intr(Video, Regs);
    GetCursorMode := Regs.CX;
  end;

  procedure SetCursorMode(Mode : Word);
  const
    Video = $10;
    SetCurType = $100;
  var
    Regs : Registers;
  begin
    Regs.AX := SetCurType;
    Regs.CX := Mode;
    Intr(Video, Regs);
  end;

  {$IFDEF UseMouse}
  procedure InitMouseScrollBar(StartScreenRow, NrOfRows : Integer);

  begin
    if not UseScrollBar then
      Exit;
    if ScrollBarAutoSize then begin
      ScrollBarHt  := NrOfRows;
      if StartScreenRow > 1 then
        ScrollBarUp  := Pred(StartScreenRow)
      else
        ScrollBarUp := StartScreenRow;
    end;
    ScrollBarTop := Succ(ScrollBarUp);
    ScrollBarBot := ScrollBarTop + Pred(ScrollBarHt);
    if StartScreenRow = 1 then
      Dec(ScrollBarBot);
    ScrollBarDn  := Succ(ScrollBarBot);
    SliderPos    := ScrollBarTop;
  end;
  {$ENDIF}

  function Browse(IFBPtr : IsamFileBlockPtr;
                  VarRec : Boolean;
                  KeyNr : Integer;
                  LowKey : IsamKeyStr;
                  HighKey : IsamKeyStr;
                  StartScreenRow,
                  NrOfRows : Integer;
                  var DatS;
                  var DatLen : Word;
                  var Ref : LongInt;
                  var KeyStr : IsamKeyStr;
                  var ExitKey : BKtype;
                  ProcSpecialTask : Pointer;
                  ProcBuildaRow : Pointer;
                  ProcDisplayaRow : Pointer) : Integer;
  var
    SaveTextAttr : Byte;
    SaveCursor : Word;
    SaveSFS : Boolean;
    SaveError : Integer;
    Desired, HorizOfs : Integer;

  {$IFDEF UseMouse}
  var
    SaveMouseOn : Boolean;
    SaveWaitForButton : Boolean;
  {$ENDIF}

  begin
    SaveTextAttr := TextAttr;
    SaveCursor := GetCursorMode;
    BrowseYL   := StartScreenRow;
    BrowseYH   := BrowseYL + Pred(NrOfRows);
    {$IFDEF UseMouse}
    if BrowseMouseEnabled then begin
      InitMouseScrollBar(StartScreenRow, NrOfRows);
      SaveMouseOn := MouseCursorOn;
      if SaveMouseOn then
        HideMouse;
      DrawMouseScrollBar;
      SaveWaitForButton := WaitForButtonRelease;
      WaitForButtonRelease := True;
    end;
    {$ENDIF}
    {Hide the cursor}
    SetCursorMode($2000);
    BTGetSearchForSequential(IFBPtr, KeyNr, SaveSFS);
    BTSetSearchForSequential(IFBPtr, KeyNr, True);
    Desired := 1;
    HorizOfs := 0;
    Browse := BrowseI(IFBPtr, VarRec, KeyNr, LowKey, HighKey, StartScreenRow,
                      NrOfRows, Desired, HorizOfs, DatS, DatLen, Ref, KeyStr,
                      ExitKey, ProcSpecialTask, ProcBuildaRow,
                      ProcDisplayaRow);
    SaveError := IsamError;
    BTSetSearchForSequential(IFBPtr, KeyNr, SaveSFS);
    IsamError := SaveError;
    IsamOK := (IsamError = 0);
    SetCursorMode(SaveCursor);
    TextAttr := SaveTextAttr;
    {$IFDEF UseMouse}
    if BrowseMouseEnabled then begin
      WaitForButtonRelease := SaveWaitForButton;
      if SaveMouseOn then begin
        if not MouseCursorOn then
          ShowMouse;
      end
      else if MouseCursorOn then
        HideMouse;
    end;
    {$ENDIF}
  end;

  function BrowseAgain(IFBPtr : IsamFileBlockPtr;
                       VarRec : Boolean;
                       KeyNr : Integer;
                       LowKey : IsamKeyStr;
                       HighKey : IsamKeyStr;
                       StartScreenRow,
                       NrOfRows : Integer;
                       var HighlightedRow : Integer;
                       var HorizOfs : Integer;
                       var DatS;
                       var DatLen : Word;
                       var Ref : LongInt;
                       var KeyStr : IsamKeyStr;
                       var ExitKey : BKtype;
                       ProcSpecialTask : Pointer;
                       ProcBuildaRow : Pointer;
                       ProcDisplayaRow : Pointer) : Integer;
  var
    SaveTextAttr : Byte;
    SaveCursor : Word;
    SaveSFS : Boolean;
    SaveError : Integer;

  {$IFDEF UseMouse}
  var
    SaveMouseOn : Boolean;
    SaveWaitForButton : Boolean;
  {$ENDIF}

  begin
    SaveTextAttr := TextAttr;
    SaveCursor := GetCursorMode;
    BrowseYL   := StartScreenRow;
    BrowseYH   := BrowseYL + Pred(NrOfRows);
    {$IFDEF UseMouse}
    if BrowseMouseEnabled then begin
      InitMouseScrollBar(StartScreenRow, NrOfRows);
      SaveMouseOn := MouseCursorOn;
      if SaveMouseOn then
        HideMouse;
      DrawMouseScrollBar;
      SaveWaitForButton := WaitForButtonRelease;
      WaitForButtonRelease := True;
    end;
    {$ENDIF}
    {Hide the cursor}
    SetCursorMode($2000);
    BTGetSearchForSequential(IFBPtr, KeyNr, SaveSFS);
    BTSetSearchForSequential(IFBPtr, KeyNr, True);
    BrowseAgain := BrowseI(IFBPtr, VarRec, KeyNr, LowKey, HighKey,
                            StartScreenRow, NrOfRows, HighlightedRow,
                            HorizOfs, DatS, DatLen, Ref, KeyStr,
                            ExitKey, ProcSpecialTask, ProcBuildaRow,
                            ProcDisplayaRow);
    SaveError := IsamError;
    BTSetSearchForSequential(IFBPtr, KeyNr, SaveSFS);
    IsamError := SaveError;
    IsamOK := (IsamError = 0);
    SetCursorMode(SaveCursor);
    TextAttr := SaveTextAttr;
    {$IFDEF UseMouse}
    if BrowseMouseEnabled then begin
      WaitForButtonRelease := SaveWaitForButton;
      if SaveMouseOn then begin
        if not MouseCursorOn then
          ShowMouse;
      end
      else if MouseCursorOn then
        HideMouse;
    end;
    {$ENDIF}
  end;
  procedure EnableFiltering(ValidateFunc : Pointer);
    {-Enables Browser filtering. <ValidateFunc> is a pointer to a user defined
      function that determines whether a given record should be displayed in the
      Browser. The <ValidateFunc> must be a FAR, non-nested function declared as
      follows:

      function ValidateARecord(IFBPtr : IsamFileBlockPtr;
                               KeyNr  : Integer;
                               Ref    : LongInt;
                               var KeyStr : IsamKeyStr) : Boolean;
    }
  begin
    FilterState := True;
    SearchKeyPtr      := @FilterSearchKey;
    NextKeyPtr        := @FilterNextKey;
    PrevKeyPtr        := @FilterPrevKey;
    FindKeyAndRefPtr  := @FilterFindKey;
    ValidatePtr       := ValidateFunc;
  end;

  procedure DisableFiltering;
    {-Disables Browser filtering.}
  begin
    FilterState := False;
    SearchKeyPtr      := @Search_Key;
    NextKeyPtr        := @Next_Key;
    PrevKeyPtr        := @Prev_Key;
    FindKeyAndRefPtr  := @Find_Key;
  end;

  function IsFilteringEnabled : Boolean;
    {-Returns True if Browser filtering is enabled.}
  begin
    IsFilteringEnabled := FilterState;
  end;

  function AddBrowseCommand(Cmd : BKtype; NumKeys : Byte; Key1, Key2 : Word) : Boolean;
    {-Add a new command key assignment or change an existing one}
  begin
    AddBrowseCommand :=
      AddCommandPrim(BrowseKeySet, BrowseKeyMax, Cmd, NumKeys, Key1, Key2);
  end;

  {$IFDEF UseMouse}
  procedure EnableBrowseMouse;
    {-Enable mouse control of Browser}
  begin
    if MouseInstalled then begin
      {$IFDEF UseTPCRT}
      BrowseKeyPtr := @TPMouse.ReadKeyOrButton;
      {$ELSE}
      BrowseKeyPtr := @OPMouse.ReadKeyOrButton;
      {$ENDIF}
      EnableEventHandling;
      BrowseMouseEnabled := True;
    end;
  end;

  procedure DisableBrowseMouse;
    {-Disable mouse control of Browser}
  begin
    if BrowseMouseEnabled then begin
      BrowseKeyPtr := @ReadKeyWord;
      DisableEventHandling;
      BrowseMouseEnabled := False;
    end;
  end;
  {$ENDIF}

begin
  {initialize procedure pointers}
  BrowseHelpPtr := nil;
  DisableFiltering;
  {$IFDEF UseTPCRT}
    BrowseKeyPtr := @TpCrt.ReadKeyWord;
  {$ELSE}
    {$IFDEF UseOPCRT}
    BrowseKeyPtr := @OpCrt.ReadKeyWord;
    {$ELSE}
    BrowseKeyPtr := @BrowseReadKey;
    {$ENDIF}
  {$ENDIF}
end.
