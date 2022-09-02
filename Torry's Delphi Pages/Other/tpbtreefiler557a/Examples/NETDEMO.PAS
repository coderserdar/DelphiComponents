{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{$I-,V-,S-,R-,F-,B-}

{$I BTDEFINE.INC}

{$IFNDEF UseTPCRT}
  ** ERROR ** This program requires UseTPCRT to be defined in BTDEFINE.INC
{$ENDIF}

program NetDemo;
  {-Demonstrates how to use B-Tree Filer and Turbo Professional together}


{The following IFNDEF statements ensure BTDEFINE.INC is properly setup to
 compiler this program}

{$I TPDEFINE.INC}

uses
  {.......................... Turbo Pascal units}
  Dos,                       {standard DOS unit}
  {.......................... Turbo Professional units}
  TpString,                  {string handling}
  TpCrt,                     {basic screen handling}
  {$IFDEF UseMouse}
  TpMouse,                   {mouse handling}
  {$ENDIF}
  TpEntry,                   {data entry screen management}
  TpMemo,                    {memo field editor}
  {.......................... B-Tree Filer units}
  BTBase, BTIsBase,
  Filer,                     {database management}
  VRec,                      {variable length records}
  VRebuild,                  {database repair--variable length records}
  {$IFDEF Novell}                                   {!!.06}
  NWBase,                                           {!!.50}
  NWConn,                                           {!!.50}
  NWSema,                                           {!!.06}
  {$ENDIF}                                          {!!.06}
  Browser;                   {database browser}

const
  Key1Len        = 30;       {Uppercase last name+first name}
  Key2Len        = 5;        {First five digits of zip}

  FName          = 'ADDRESS'; {Root name for database}
  LstDevice      = 'PRN';    {Where printed output goes}
  BrowseRows     = 20;       {Number of rows in browse window}
  FooterRow      = 25;       {row number for footer}

  Header         : String[80] = {Basic string used to build display header}
  ' B-Tree Filer Demo Program                                                      ';

  F1             = $3B00;    {Keycodes for function keys}
  F2             = $3C00;
  F3             = $3D00;
  F4             = $3E00;
  F5             = $3F00;
  F6             = $4000;
  F7             = $4100;
  F8             = $4200;
  F9             = $4300;
  F10            = $4400;

  SectionLength  = 140;      {each record will use from 1 to 8 sections}
  MaxMemoSize    = 932;      {140*8 = 1120, (7*(140-7))+1 = 932}

  CurrentRow : Integer = 0;       {stores the row offset of selected record}

type
  CharSet        = set of Char;
  MemoField      = array[1..MaxMemoSize] of Char;

  PersonDef =                {Definition of the database record}
    record
      Dele           : LongInt;
      FirstName      : String[15];
      Name           : String[15];
      Company        : String[25];
      Address        : String[25];
      City           : String[15];
      State          : String[2];
      Zip            : String[10];
      Telephone      : String[12];
      NotesLen       : Word;      {<-- 133 bytes to here}
      Notes          : MemoField; {memo field: 1..MaxMemoSize bytes}
    end;                          {1065 bytes maximum, 134 minimum}

  BrowseMapRec =             {Used for browse screen efficiency}
    record
      CurRef         : LongInt;
      Horiz          : Integer;
      Highlighted    : Boolean;
    end;
  BrowseMapArray = array[1..BrowseRows] of BrowseMapRec;

var
  PS             : LongInt;  {Pages in page stack}
  Free           : LongInt;  {Heap bytes to keep free} {!!.40}
  Pf             : IsamFileBlockPtr; {Isam management variable}

  Person         : PersonDef; {Currently selected record}
  ActRec         : LongInt;  {Record number currently selected}
  ActKeyNr       : Integer;  {Active key number, 1 or 2}
  ActKey         : IsamKeyStr; {Active key string}

  BrowResp       : Integer;
  DatLen         : Word;
  BrowExit       : BKtype;
  AC             : Char;
  UseSaveMode    : Boolean;
  Locked         : Boolean;
  HorizOfs       : Integer;

  BrowseMap      : BrowseMapArray; {For screen management}

  {colors}
  NormAttr       : Byte;
  SeleAttr       : Byte;
  HighAttr       : Byte;
  FielAttr       : Byte;
  HeadAttr       : Byte;
  SaveAttr       : Byte;

  PersonFilter : PersonDef;       {used by the Browser filter routines}

  {data entry stuff}
const
  PhoneMask      : String[12] = '999-999-9999';
  ValidPhone     : String[12] = 'ppp-uuu-uuuu';
  ZipMask        : String[10] = '99999-9999';
  ValidZip       : String[10] = 'uuuuu-pppp';
  ValidationOff  : Boolean = False;

  {coordinates for entry screen and memo field windows}
  EntryXL        = 29;
  EntryYL        = 04;
  EntryXH        = 78;
  EntryYH        = 12;
  MemoXL         = 29;
  MemoYL         = 15;
  MemoXH         = 78;
  MemoYH         = 22;
var
  ESR            : ESrecord; {for entry screens}
  EMCB           : EMcontrolBlock; {for memo fields}
  ScrapPerson    : PersonDef; {used for editing}
  VRecLen        : Word;

const
  ActualNetType  : NetSupportType = NoNet;

  {$IFDEF UseMouse}
  function MouseOrKeyPressed : Boolean;
  begin
    if MouseInstalled then
      MouseOrKeyPressed := MousePressed or KeyPressed
    else
      MouseOrKeyPressed := KeyPressed;
  end;

  function ReadKeyOrMouse : Char;

  begin
    if not MouseInstalled then
      ReadKeyOrMouse := ReadKey
    else begin
      while (not KeyPressed) and (not MousePressed) do
        Inline($CD/$28);
      if KeyPressed then
        ReadKeyOrMouse := ReadKey
      else
        case Integer(MouseKeyWord) of
          Integer(MouseRt)  : ReadKeyOrMouse := #27;
          else ReadKeyOrMouse := #13;
        end;
    end;
  end;

  function ReadKeyOrMouseWord : Word;
  begin
    if not MouseInstalled then
      ReadKeyOrMouseWord := ReadKeyWord
    else begin
      while (not KeyPressed) and (not MousePressed) do
        Inline($CD/$28);
      if KeyPressed then
        ReadKeyOrMouseWord := ReadKeyWord
      else
        case Integer(MouseKeyWord) of
          Integer(MouseRt)  : ReadKeyOrMouseWord := 27;
          else ReadKeyOrMouseWord := 13;
        end;
    end;
  end;

  {$ELSE}
  function MouseOrKeyPressed : Boolean;

  begin
    MouseOrKeyPressed := KeyPressed;
  end;

  function ReadKeyOrMouse : Char;
  begin
    ReadKeyOrMouse := ReadKey;
  end;

  function ReadKeyOrMouseWord : Word;
  begin
    ReadKeyOrMouseWord := TpCrt.ReadKeyWord;
  end;
  {$ENDIF}

{$IFDEF Novell}                                    {!!.06 begin}
{!!.50 semaphore code rewitten for new NWSEMA unit}
const
  SemaName1 = 'sd__Key1';
  SemaName2 = 'sd__Key2';

type
  SemaphoreRecord =
    record
      Handle : nwLong;
      Value  : nwInt;
      OpenCount  : word;
    end;

var
  DefServer  : TnwServer;
  KeySema    : Array[1..2] of SemaphoreRecord;

  function OpenSemaphores : Boolean;
  begin
    OpenSemaphores := False;
    DefServer := nwDefaultServer;
    with KeySema[1] do
      if (nwOpenSema(DefServer, SemaName1,
                     0, OpenCount, Handle) <> 0) then
        Exit;
    with KeySema[2] do
      if (nwOpenSema(DefServer, SemaName2,
                     0, OpenCount, Handle) = 0) then
        OpenSemaphores := True;
  end;

  procedure CloseSemaphores;
  begin
    if (nwCloseSema(DefServer, KeySema[1].Handle) <> 0) then
      {nothing};
    if (nwCloseSema(DefServer, KeySema[2].Handle) <> 0) then
      {nothing};
  end;

  procedure IndicateDirty(KeyNo : Integer);
  var
    I : integer;

  begin
    with KeySema[KeyNo] do
      if (nwIncSema(DefServer, Handle) = nwsErrOverflow) then begin
        for I := 127 downto 1 do
          if (nwDecSema(DefServer, Handle, 0) = 0) then
            {nothing};
      end;
  end;

  function IsDirty(KeyNo : Integer) : Boolean;
  var
    NewValue : nwInt;

  begin
    IsDirty := False;
    with KeySema[KeyNo] do
      if (nwExamineSema(DefServer, Handle, NewValue, OpenCount) = 0) then
        if NewValue <> Value then begin
          IsDirty := True;
          Value   := NewValue;
        end;
  end;

  {$F+}
  function SemaphoreRefresh(IFBPtr : IsamFileBlockPtr;
                            KeyNo : Integer) : Boolean;
  var
    Ticks : LongInt absolute $40:$6C;
    T : LongInt;
  begin
    {assume false}
    SemaphoreRefresh := False;

    {do nothing if this is a single-user fileblock}
    if BTIsNetFileBlock(IFBPtr) then begin
      {save tick count}
      T := Ticks;

      {loop while key not pressed}
      while not MouseOrKeyPressed do
        {is it time to check again?}
        if (Ticks-T) >= RefreshPeriod then
          {check to see if page stack has been invalidated}
          if IsDirty(KeyNo) then begin
            {we need to refresh the display}
            SemaphoreRefresh := True;
            Exit;
          end
          else
            {save the current tick count}
            T := Ticks;
    end;
  end;
  {$F-}

{$ENDIF}                                              {!!.06 end}

  function IsLockError : Boolean;
    {-Return true for a locking error}
  begin
    IsLockError := (BTIsamErrorClass = 2);          {!!.06}
  end;

  function Extend(S : String; Len : Byte) : String;
    {-Pad or truncate string to specified length}
  var
    SLen           : Byte absolute S;
  begin
    if SLen >= Len then begin
      SLen := Len;
      Extend := S;
    end
    else
      Extend := Pad(S, Len);
  end;

  procedure WriteHeader(Prompt : String);
    {-Write header and bottom divider}
  var
    S              : String;
    I, J, L        : Integer;
  begin
    {draw frame}
    FrameWindow(1, 2, 80, 24, HeadAttr, HeadAttr, '');

    {draw header}
    S := Header;
    L := Length(Prompt);
    if L > 80 then
      L := 80;
    J := 40-(L shr 1);
    for I := 1 to L do
      S[J+I] := Prompt[I];
    FastWrite(S, 1, 1, HeadAttr);

    {display active key}
    if ActKeyNr = 1 then
      S := ' Key: Last Name '
    else
      S := ' Key: Zip Code ';
    if IsFilteringEnabled then
      FastWrite('®Filter¯', 1, 50, HeadAttr);
    FastWrite(S, 1, 62, HeadAttr);
  end;

  procedure WriteFooter(Prompt : String);
    {-Write a footer on the menu line}
  begin
    FastWrite(Extend(Prompt, 80), FooterRow, 1, HeadAttr);
    GotoXY(Length(Prompt)+2, FooterRow);
  end;

  function Menu(Selection : CharSet; Prompt : String) : Char;
    {-Draw a bar menu and get a selection in the CharSet}
  var
    ChWord         : Word;
    Ch             : Char absolute ChWord;
  begin
    {display prompt}
    WriteFooter(Prompt);

    {flush keyboard buffer}
    while KeyPressed do
      Ch := ReadKey;          {!!.04}

    {wait for valid key}
    repeat
      ChWord := ReadKeyOrMouseWord;
      Ch := Upcase(Ch);
    until (Ch in Selection);

    {clear prompt line}
    WriteFooter('');

    Menu := Ch;
  end;

  procedure DispMessage(Prompt : String; WaitKey, SoundBell : Boolean);
    {-Display a message on the menu line, optionally waiting for keystroke and
      ringing bell}
  var
    C              : Word;
  begin
    if WaitKey then begin
      if Prompt[Length(Prompt)] <> '.' then
        Prompt := Prompt+'.';
      WriteFooter(' '+Prompt+' Press any key...');
      if SoundBell then
        RingBell;
      C := ReadKeyOrMouseWord;
    end
    else
      WriteFooter(' '+Prompt);
  end;

  procedure DispMessageTemp(Prompt : String; Time : Word);
    {-Display a timed message}
  begin
    WriteFooter(Prompt);
    Delay(Time);
    WriteFooter('');
  end;

  procedure IsamErrorNum(F : Integer);
    {-Display Isam error number and wait for key}
  begin
    DispMessage('IsamError: '+Long2Str(F), True, True);
  end;

  function YesNo(Prompt : String; Default : Char) : Boolean;
    {-Display Yes/No prompt}
  var
    Ch             : Char;
  begin
    Ch := Menu(['Y', 'N', ^M], Prompt+' ['+Default+']');
    if Ch = ^M then
      Ch := Default;
    YesNo := (Ch = 'Y');
  end;

  function LockAbort : Boolean;
    {-If a file lock prevents progress, ask whether to try again}
  begin
    LockAbort := False;
    Locked := IsLockError;
    if not Locked then
      Exit;
    LockAbort := not YesNo('A lock prevents access. Try again?', 'Y');
  end;

  procedure AbortPrintMessage;
    {-Display this message while printing}
  begin
    WriteFooter('Press any key to abort print ');
  end;

  function Aborting : Boolean;
    {-Check for a keypress during printing, and offer a chance to quit}
  var
    C              : Char;
  begin
    Aborting := False;
    if KeyPressed then begin
      repeat
        C := ReadKey;               {!!.04}
      until not KeyPressed;
      if YesNo('Do you really wish to quit?', 'N') then
        Aborting := True
      else
        AbortPrintMessage;
    end;
  end;

  procedure ClearPerson(var Person : PersonDef);
    {-Set up for a new person record}
  begin
    FillChar(Person, SizeOf(PersonDef), 0);
    Person.NotesLen := 1;
    Person.Notes[1] := ^Z;
  end;

  function CompPerson(var P1, P2 : PersonDef) : Boolean;
    {-Compare two person records}
  begin
    CompPerson := False;
    if P1.Dele <> P2.Dele then
      Exit;
    if P1.FirstName <> P2.FirstName then
      Exit;
    if P1.Name <> P2.Name then
      Exit;
    if P1.Company <> P2.Company then
      Exit;
    if P1.Address <> P2.Address then
      Exit;
    if P1.City <> P2.City then
      Exit;
    if P1.State <> P2.State then
      Exit;
    if P1.Zip <> P2.Zip then
      Exit;
    if P1.Telephone <> P2.Telephone then
      Exit;
    if P1.NotesLen <> P2.NotesLen then
      Exit;

    {compare memo fields quickly using routine in TPSTRING}
    if CompStruct(P1.Notes, P2.Notes, P1.NotesLen) <> Equal then
      Exit;

    CompPerson := True;
  end;

  procedure MakeEntryWindow(Header : String; RecNum : LongInt);
    {-Set up the entry window}
  begin
    {fix the header}
    if RecNum <> 0 then
      Header := Header+' Record # '+Long2Str(RecNum);
    Header := ' '+Header+' ';

    {draw the frame}
    FrameWindow(EntryXL-1, EntryYL-1, EntryXH+1, EntryYH+1, HeadAttr, SeleAttr,
                Header);

    {clear the inside of the window}
    ClearEntryWindow(ESR);
  end;

  procedure DisplayMemoField;
    {-Display the memo field}
  const
    EditMemoCmdList : array[1..2] of Byte = (EMuser0, EMnone);
  var
    Dummy          : EMtype;
  begin
    {draw the frame}
    FrameWindow(
      MemoXL-1, MemoYL-1, MemoXH+1, MemoYH+1, HeadAttr, SeleAttr, ' Notes ');

    {reinitialize control block}
    InitBufferState(EMCB, SizeOf(MemoField), ScrapPerson.Notes);
    ScrapPerson.NotesLen := EMCB.TotalBytes;

    {display the contents of the memo by making a phony call to EditMemo}
    Dummy := EditMemo(EMCB, True, EditMemoCmdList);
  end;

  procedure DisplayMemoPrompt;
    {-Display prompt at bottom of screen while editing}
  begin
    WriteFooter(
      Center('Press <^Enter> when done editing notes to return to entry screen',
             80));
  end;

  procedure DisplayPerson(var Person     : PersonDef;
                          Header         : String;
                          RecNum         : LongInt);
    {-Show data about person}
  begin
    {copy into our scrap record}
    ScrapPerson := Person;

    {set up the entry window}
    MakeEntryWindow(Header, RecNum);

    {display entry screen}
    DrawEditScreen(ESR);

    {display memo field if appropriate}
    if RecNum <> 0 then
      DisplayMemoField;
  end;

  procedure EditMemoField;
    {-Edit the memo field}
  const
    NullCmdList    : EMtype = EMnone;
  var
    ExitCommand    : EMtype;
    {$IFDEF UseMouse}
    SaveMouse      : Boolean;
    {$ENDIF}
  begin
    {display prompt}
    DisplayMemoPrompt;

    {$IFDEF UseMouse}
    if MouseInstalled then begin
      SaveMouse := MouseCursorOn;
      if not SaveMouse then
        ShowMouse;
    end;
    {$ENDIF}

    {do the editing}
    ExitCommand := EditMemo(EMCB, False, NullCmdList);

    {$IFDEF UseMouse}
    if MouseInstalled then
      if not SaveMouse then
        HideMouse;
    {$ENDIF}

    {save the number of bytes in the buffer}
    ScrapPerson.NotesLen := EMCB.TotalBytes;
  end;

  function GetPerson(var Person : PersonDef; NameRequired : Boolean;
                     Header : String; RecNum : LongInt) : Boolean;
    {-Edit a person record}
  var
    Done           : Boolean;
    ExitCommand    : EStype;
    FRP            : FieldRecPtr;
  begin
    {copy into our scrap record}
    ScrapPerson := Person;

    {need special validation?}
    ValidationOff := not NameRequired;

    {set required status for last name}
    {Note: With Turbo Professional 5.05 or higher we could use the
     ChangeRequired procedure to do this, just as we're using ChangeProtection
     below on the Notes field, but ChangeRequired didn't exist in earlier
     versions.}
    FRP := FindFieldID(ESR, 1);
    with FRP^ do
      if NameRequired then
        {set the required field flag}
        EditFlags := EditFlags or RequiredMask
      else
        {clear the required field flag}
        EditFlags := EditFlags and (not RequiredMask);

    {disable access to Notes field if searching}
    ChangeProtection(ESR, 8, not NameRequired);

    {set up the entry window}
    MakeEntryWindow(Header, RecNum);

    {draw the memo window if not searching}
    if NameRequired then
      DisplayMemoField;

    {start editing on first field}
    ESR.CurrentID := 0;

    Done := False;
    repeat
      {start editing}
      ExitCommand := EditScreen(ESR, ESR.CurrentID, False);

      {see if we need to edit another record}
      case ExitCommand of
        ESdone :             {^Enter, ^KD, or ^KQ}
          begin
            Done := True;
            GetPerson := True;
          end;
        ESquit :             {Esc}
          begin
            Done := True;
            GetPerson := False;
          end;
        ESnested :
          {edit the notes field}
          if NameRequired then
            EditMemoField;
      end;
    until Done;

    {return modified record, even if <Esc> was pressed--caller will ignore
     changes if appropriate}
    Person := ScrapPerson;

    {clear the prompt line}
    WriteFooter('');
  end;

  function CreateFile : Boolean;
    {-Create the database fileblock}
  var
    IID            : IsamIndDescr;
  begin
    IID[1].KeyL := Key1Len;
    IID[1].AllowDupK := False;
    IID[2].KeyL := Key2Len;
    IID[2].AllowDupK := True;
    BTCreateFileBlock(FName, SectionLength, 2, IID);
    CreateFile := IsamOK;
  end;

  function PersonLine(var Person : PersonDef) : String;
    {-Return a string representing Person}
  const
    HaveNotes      : array[Boolean] of Char = (' ', #251);
  begin
    with Person do
      PersonLine :=
        Extend(Zip, 5)+' '+
        Extend(Trim(Name)+', '+Trim(FirstName), 19)+' '+
        Extend(Company, 19)+' '+
        Extend(Address, 19)+' '+
        Extend(City, 13)+' '+
        Extend(State, 2)+' '+
        Extend(Telephone, 12)+' '+
        HaveNotes[NotesLen > 1];
  end;

  {$F+} {the next three routines are called indirectly by BROWSER}

  function BuildKey(var P; KeyNr : Word) : IsamKeyStr;
    {-Return the key string for either of the two indexes}
  begin
    with PersonDef(P) do
      case KeyNr of
        1 : BuildKey := Extend(StUpCase(Trim(Name)),20) +
                        Extend(StUpCase(Trim(FirstName)),10);
        2 : BuildKey := Copy(Zip, 1, 5);
      end;
  end;

  procedure BuildaRow(var RR : RowRec; KeyNr : Integer;
                      var DatS; DatLen : Word);
    {-Return one row to the browser}
  begin
    with RR, PersonDef(DatS) do
      if Ref <> -1 then
        Row := PersonLine(PersonDef(DatS))
      else begin
        {Record is locked, indicate it on screen}
        Row := '';
        while Length(Row) < MaxCols do
          Row := Row+'**   ';
      end;
  end;

  procedure DisplayaRow(var RR : RowRec; KeyNr, RowNr, StartRow : Integer;
                        HighLight : Boolean; var HorizOfs : Integer);
    {-Display one row for the browser}
  const
    ScreenCols     = 78;
  {
           1         2         3         4         5         6         7         8         9         1
  1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
   Zip   Name                Company             Address             City         St  Phone       Notes
  zzzzz nnnnnnnnnnnnnnnnnnn ccccccccccccccccccc aaaaaaaaaaaaaaaaaaa ccccccccccccc ss ppp-ppp-pppp n
  }
    Header =
    ' Zip   Name                Company             Address             City         St  Phone       Notes';
  var
    Attr           : Byte;
  begin
    if HorizOfs > (Length(Header)-ScreenCols) then
      HorizOfs := (Length(Header)-ScreenCols);

    with BrowseMap[RowNr] do begin
      if RR.Ref = CurRef then
        if HighLight = Highlighted then
          if HorizOfs = Horiz then
            {Record is already displayed correctly}
            Exit;
      CurRef := RR.Ref;
      Highlighted := HighLight;
      Horiz := HorizOfs;
    end;

    if RowNr = 1 then
      {Write the header line now}
      FastWrite(Extend(Copy(Header, HorizOfs+1, ScreenCols), ScreenCols),
                3, 2, HighAttr);

    if HighLight then
      Attr := SeleAttr
    else
      Attr := NormAttr;
    FastWrite(Extend(Copy(RR.Row, HorizOfs+1, ScreenCols), ScreenCols),
              Pred(StartRow)+RowNr, 2, Attr);
  end;

  {$F-}

  function AddStructure(var P : PersonDef; var Rec : LongInt) : Boolean;
    {-Add a new record}
  begin
    AddStructure := False;
    repeat
      BTAddVariableRec(Pf, Rec, P, P.NotesLen+SizeOf(PersonDef)-SizeOf(MemoField));
      if LockAbort then
        Exit;
    until not Locked;
    if not IsamOK then
      IsamErrorNum(IsamError)
    else
      AddStructure := True;
  end;

  function ModStructure(var P : PersonDef; Rec : LongInt) : Boolean;
    {-Write record over previous version}
  begin
    ModStructure := False;
    repeat
      BTPutVariableRec(Pf, Rec, P,
                       P.NotesLen+SizeOf(PersonDef)-SizeOf(MemoField));
      if LockAbort then
        Exit;
    until not Locked;
    if not IsamOK then
      IsamErrorNum(IsamError)
    else
      ModStructure := True;
  end;

  function DelStructure(var Rec : LongInt) : Boolean;
    {-Delete record}
  begin
    DelStructure := False;
    repeat
      BTDeleteVariableRec(Pf, Rec);
      if LockAbort then
        Exit;
    until not Locked;
    if not IsamOK then
      IsamErrorNum(IsamError)
    else
      DelStructure := True;
  end;

  function AddKey(K : IsamKeyStr; Rec : LongInt; KeyNr : Integer) : Boolean;
    {-Add new key}
  begin
    AddKey := False;
    repeat
      BTAddKey(Pf, KeyNr, Rec, K);
      if LockAbort then
        Exit;
    until not Locked;
    if not IsamOK then
      IsamErrorNum(IsamError)
    else
      AddKey := True;
  end;

  function EraseKey(K : IsamKeyStr; Rec : LongInt; KeyNr : Integer) : Boolean;
    {-Remove a key}
  begin
    EraseKey := False;
    repeat
      BTDeleteKey(Pf, KeyNr, Rec, K);
      if LockAbort then
        Exit;
    until not Locked;
    if not IsamOK then
      IsamErrorNum(IsamError)
    else
      EraseKey := True;
  end;

  function ModKey(AltK, NeuK : IsamKeyStr; Rec : LongInt; KeyNr : Integer) : Boolean;
    {-Replace a key}
  begin
    ModKey := False;
    if EraseKey(AltK, Rec, KeyNr) then
      if AddKey(NeuK, Rec, KeyNr) then
        ModKey := True;
  end;

  procedure Reposition(UserKey : IsamKeyStr);
    {-Set sequential file pointer to another key}
  var
    Rec            : LongInt;
  begin
    repeat
      BTFindKey(Pf, 1, Rec, UserKey);
      if LockAbort then
        Exit;
    until not Locked;
    if not IsamOK then
      ActRec := 0;
  end;

  function LockAll : Boolean;
    {-Lock all open files, returning true if successful}
  var
    OK             : Boolean;
  begin
    LockAll := False;
    repeat
      BTLockAllOpenFileBlocks;
      if not IsamOK then begin
        if not YesNo('The file is presently in use. Try again?', 'Y') then
          Exit;
        OK := False;
      end
      else
        OK := True;
    until OK;
    LockAll := True;
  end;

  procedure NewStructure;
    {-Prompt for and add new record}
  label
    Retry;
  var
    PersonTemp     : PersonDef;
    Key1, Key2     : IsamKeyStr;
    Rec            : LongInt;
    KExists, OK    : Boolean;
  begin
    WriteHeader(' New Entry ');
    ClearPerson(PersonTemp);

Retry:
    {Get the new record}
    if not GetPerson(PersonTemp, True, 'Add Record', 0) then
      Exit;

    {make the index keys}
    Key1 := BuildKey(PersonTemp, 1);
    Key2 := BuildKey(PersonTemp, 2);

    {Lock the database in order to safely add the record}
    if not LockAll then
      Exit;

    {Assure it's not a duplicate key}
    repeat
      KExists := BTKeyExists(Pf, 1, Rec, Key1);
      if LockAbort then begin
        BTUnLockAllOpenFileBlocks;
        Exit;
      end;
    until not Locked;
    if KExists then begin
      BTUnLockAllOpenFileBlocks;
      if not YesNo('The name already exists. Try again?', 'Y') then
        Exit
      else
        goto Retry;
    end;

    {Add the record and its keys}
    OK := AddStructure(PersonTemp, Rec);
    if OK then
      OK := AddKey(Key1, Rec, 1);
    if OK then
      OK := AddKey(Key2, Rec, 2);

    {$IFDEF Novell}                                            {!!.06 begin}
    if BTNetSupported = Novell then begin
      IndicateDirty(1);
      IndicateDirty(2);
    end;
    {$ENDIF}                                                   {!!.06 end}

    {Save global pointers to the current record}
    if OK then begin
      ActRec := Rec;
      case ActKeyNr of
        1 : ActKey := Key1;
        2 : ActKey := Key2;
      end;
    end;

    BTUnLockAllOpenFileBlocks;
  end;

  procedure Modify;
    {-Modify an existing record}
  label
    Retry;
  var
    PersonTemp     : PersonDef;
    PersonTemp1    : PersonDef;
    KExists, OK    : Boolean;
    Rec            : LongInt;
    Escaped        : Boolean;
    NoChanges      : Boolean;
  begin
    WriteHeader(' Modify ');
    PersonTemp := Person;

Retry:
    Escaped := not GetPerson(PersonTemp, True, 'Modifying', ActRec);
    NoChanges := CompPerson(Person, PersonTemp);
    if Escaped and not NoChanges then
      NoChanges := YesNo('Ignore changes to record?', 'N');
    if NoChanges then begin
      DispMessageTemp('Files not changed.', 250);
      Exit;
    end;

    {Lock the database in order to safely modify the record}
    if not LockAll then
      Exit;

    if BuildKey(PersonTemp, 1) <> BuildKey(Person, 1) then begin
      KExists := BTKeyExists(Pf, 1, ActRec, BuildKey(PersonTemp, 1));
      if not IsamOK then begin
        IsamErrorNum(IsamError);
        BTUnLockAllOpenFileBlocks;
        Exit;
      end;
      if KExists then begin
        BTUnLockAllOpenFileBlocks;
        if not YesNo('The name already exists. Try again?', 'Y') then
          Exit
        else
          goto Retry;
      end;
    end;

    Rec := ActRec;
    {Read actual disk data}
    BTGetVariableRec(Pf, Rec, PersonTemp1, VRecLen);
    if not IsamOK then begin
      BTUnLockAllOpenFileBlocks;
      DispMessageTemp('Record could not be read from disk.', 1000);
      Exit;
    end;

    if PersonTemp1.Dele <> LongInt(0) then begin
      BTUnLockAllOpenFileBlocks;
      DispMessageTemp('The record has been erased in the meantime.', 1000);
      Exit;
    end;

    if not CompPerson(PersonTemp1, Person) then begin
      BTUnLockAllOpenFileBlocks;
      DispMessageTemp('The record has been changed in the meantime.', 1000);
      Person := PersonTemp1;
      Exit;
    end;

    OK := ModStructure(PersonTemp, ActRec);
    if OK then
      if BuildKey(PersonTemp, 1) <> BuildKey(Person, 1) then begin
        OK := ModKey(BuildKey(Person, 1), BuildKey(PersonTemp, 1), ActRec, 1);
        if OK then
          Reposition(BuildKey(PersonTemp, 1));
      end;
    if OK then
      if BuildKey(PersonTemp, 2) <> BuildKey(Person, 2) then
        OK := ModKey(BuildKey(Person, 2), BuildKey(PersonTemp, 2), ActRec, 2);

    BTUnLockAllOpenFileBlocks;
    if OK then begin                             {!!.06}
      Person := PersonTemp;
      {$IFDEF Novell}                            {!!.06 begin}
      if BTNetSupported = Novell then begin
        IndicateDirty(1);
        IndicateDirty(2);
      end;
      {$ENDIF}                                   {!!.06 end}
    end;                                         {!!.06}
  end;

  procedure Delete;
    {-Prompt for and delete a record}
  var
    Key1, Key2     : IsamKeyStr;
    OK             : Boolean;
  begin
    WriteHeader(' Deleting ');
    DisplayPerson(Person, 'Deleting', ActRec);
    if not YesNo('Should the record really be deleted?', 'N') then
      Exit;

    Key1 := BuildKey(Person, 1);
    Key2 := BuildKey(Person, 2);

    {Lock the database}
    if not LockAll then
      Exit;

    OK := EraseKey(Key1, ActRec, 1);
    if OK then
      OK := EraseKey(Key2, ActRec, 2);
    if OK then
      OK := DelStructure(ActRec);
    if not OK then
      IsamErrorNum(IsamError);

    {$IFDEF Novell}                                         {!!.06 begin}
    if OK and (BTNetSupported = Novell) then begin
      IndicateDirty(1);
      IndicateDirty(2);
    end;
    {$ENDIF}                                                {!!.06 end}

    BTUnLockAllOpenFileBlocks;
  end;

  function MatchString(var SG, ST : String) : Boolean;
    {-Return true if SG and ST match}
  begin
    if Length(SG) = 0 then
      {Nothing to match against}
      MatchString := True
    else
      {Match if ST starts with SG}
      MatchString := (Pos(StUpCase(SG), StUpCase(ST)) = 1);
  end;

  function MatchPerson(var PG, PT : PersonDef) : Boolean;
    {-Compare two person records}
  begin
    MatchPerson := False;
    if PT.Dele <> 0 then
      Exit;
    if not MatchString(PG.FirstName, PT.FirstName) then
      Exit;
    if not MatchString(PG.Name, PT.Name) then
      Exit;
    if not MatchString(PG.Company, PT.Company) then
      Exit;
    if not MatchString(PG.Address, PT.Address) then
      Exit;
    if not MatchString(PG.City, PT.City) then
      Exit;
    if not MatchString(PG.State, PT.State) then
      Exit;
    if not MatchString(PG.Zip, PT.Zip) then
      Exit;
    if not MatchString(PG.Telephone, PT.Telephone) then
      Exit;
    MatchPerson := True;
  end;

  function GetNextRec(var Fptr       : IsamFileBlockPtr;
                      var Data       : PersonDef;
                      KeyNr          : Integer;
                      var Rec        : LongInt;
                      var UserKey    : IsamKeyStr) : Boolean;
    {-Get next record in index order}
  begin
    GetNextRec := False;

    {Get next sequential key}
    repeat
      BTNextKey(Fptr, KeyNr, Rec, UserKey);
      if LockAbort then
        Exit;
    until not Locked;

    if not IsamOK and (IsamError = 10250) then
      {At end of list, try once more to wrap to beginning}
      repeat
        BTNextKey(Fptr, KeyNr, Rec, UserKey);
        if LockAbort then
          Exit;
      until not Locked
    else
      GetNextRec := True;
    if not IsamOK then
      Exit;

    {Get associated data}
    repeat
      BTGetVariableRec(Fptr, Rec, Data, VRecLen);
      if LockAbort then
        Exit;
    until not Locked;
  end;

  procedure Search;
    {-Search for a record}
  var
    R              : LongInt;
    SearchKey      : Integer;
    OK             : Boolean;
    Found          : Boolean;
    Key            : IsamKeyStr;
    PersonGoal     : PersonDef;
    PersonTemp     : PersonDef;

    procedure NotFoundMessage;
    begin
      DispMessage('No matching record found', True, True);
    end;

  begin
    WriteHeader(' Search Key ');
    ClearPerson(PersonGoal);
    ClearPerson(PersonTemp);

    {Get search target}
    ValidationOff := True;
    if not GetPerson(PersonGoal, False, 'Search', 0) or
    CompPerson(PersonTemp, PersonGoal) then
      {Nothing entered}
      Exit;

    WriteFooter('Searching... ');

    {See which key to search on, if any}
    if Length(PersonGoal.Name) <> 0 then
      SearchKey := 1
    else if Length(PersonGoal.Zip) <> 0 then
      SearchKey := 2
    else
      SearchKey := 0;

    if SearchKey <> 0 then begin
      {Use the index system to position to the nearest record}
      Key := BuildKey(PersonGoal, SearchKey);
      repeat
        BTSearchKey(Pf, SearchKey, R, Key);
        if LockAbort then
          Exit;
      until not Locked;
      if not IsamOK then begin
        if IsamError = 10210 then
          NotFoundMessage
        else
          IsamErrorNum(IsamError);
        Exit;
      end;

      {Get the record}
      repeat
        BTGetVariableRec(Pf, R, PersonTemp, VRecLen);
        if LockAbort then
          Exit;
      until not Locked;

      {Position current record pointer at least near to the goal}
      ActRec := R;
      ActKey := BuildKey(PersonTemp, ActKeyNr);

      {Does it match the goal?}
      Found := MatchPerson(PersonGoal, PersonTemp);
      if Found and IsFilteringEnabled then
        Found := MatchPerson(PersonFilter, PersonTemp);
    end
    else begin
      {Start sequential search at the currently active record}
      R := ActRec;
      BTFindKeyAndRef(Pf, ActKeyNr, R, ActKey, 0);
      Found := False;
    end;

    if not Found then begin
      {Sequential search, starting one beyond current position}
      if SearchKey = 0 then
        SearchKey := ActKeyNr;
      repeat
        OK := GetNextRec(Pf, PersonTemp, SearchKey, R, Key);
        if not IsamOK then
          Exit;
        Found := MatchPerson(PersonGoal, PersonTemp);
        if Found and IsFilteringEnabled then
          Found := MatchPerson(PersonFilter, PersonTemp);
      until Found or (R = ActRec);
    end;

    if Found then begin
      ActRec := R;
      ActKey := BuildKey(PersonTemp, ActKeyNr);
    end
    else
      NotFoundMessage;
  end;

  procedure Status;
    {-Show the number of records}
  const
    ModeSt : array[Boolean] of string[6] = ('Normal', 'Save');
  var
    F, U, K           : LongInt;
  begin
    WriteHeader(' Status ');
    repeat
      U := BTUsedRecs(Pf);
      if LockAbort then
        Exit;
    until not Locked;

    repeat
      F := BTFreeRecs(Pf);
      if LockAbort then
        Exit;
    until not Locked;

    repeat
      K := BTUsedKeys(Pf, 1);
      if LockAbort then
        Exit;
    until not Locked;

    DispMessage(
      'Records:'+Long2Str(K)+
      ', Sections:'+Long2Str(U)+
      ', Deleted:'+Long2Str(F)+
      ', Mode:'+ModeSt[UseSaveMode]+
      ', Station:'+Long2Str(BTGetInternalDialogID(Pf)), {!!.40}
      True, False);
  end;

  procedure List;
    {-List all records to printer}
  var
    T              : LongInt;
    Rec            : LongInt;
    KeyNr          : Integer;
    Key            : IsamKeyStr;
    OK             : Boolean;
    C              : Char;
    Lst            : Text;
    S              : String;
    SLen           : Byte absolute S;
  begin
    WriteHeader(' List ');

    {Assure there are records to print}
    repeat
      T := BTUsedRecs(Pf);
      if LockAbort then
        Exit;
    until not Locked;
    if T = 0 then begin
      DispMessage('No records available', True, True);
      Exit;
    end;

    {See what order to print in -- provide chance to abort}
    C := Menu(['N', 'Z', 'A'], 'Sort by  N)ame Z)ipcode A)bort');
    case C of
      'A' : Exit;
      'N' : KeyNr := 1;
      'Z' : KeyNr := 2;
    end;

    {Position over first record}
    repeat
      BTClearKey(Pf, KeyNr);
      if LockAbort then
        Exit;
    until not Locked;
    Rec := 0;
    Key := '';
    if IsamOK then begin
      OK := GetNextRec(Pf, Person, KeyNr, Rec, Key);
      if Locked then
        Exit;

      {Print all the records}
      Assign(Lst, LstDevice);
      Rewrite(Lst);
      if IoResult <> 0 then begin                    {!!.04}
        DispMessage('Error attempting to write to '+LstDevice,True,True);
        Exit;
      end;

      AbortPrintMessage;
      repeat
        {get displayable string and trim checkmarks and blanks}
        S := PersonLine(Person);
        if S[SLen] = #251 then
          Dec(SLen);
        while S[SLen] = ' ' do
          Dec(SLen);

        WriteLn(Lst, S);
        OK := (IoResult = 0);
        if OK then
          OK := not Aborting
        else
          DispMessage('Printer error', True, True);
        if OK then
          OK := GetNextRec(Pf, Person, KeyNr, Rec, Key);
        if Locked then
          OK := False;
      until not(IsamOK and OK);
      Close(Lst);
      if IoResult <> 0 then ;   {clear IoResult}    {!!.04}
    end;
  end;

  function Long2StrDigits(L : LongInt; NumDigits : Byte) : String;
  {-Convert a longint to a string, right justified to NumDigits}
  var
    S : String;
  begin
    Str(L:NumDigits,S);
    Long2StrDigits := S;
  end;

  {$F+}
  procedure UserStatusRoutine(KeyNr : Integer;
                              NumRecsRead,
                              NumRecsWritten : LongInt;
                              var Data;
                              Len : Word);
  {-Display information while rebuilding database}
  var
    StatStr : String[80];

  begin
    StatStr := 'Working on key --> '+Long2StrDigits(KeyNr,1)+
               '   records read --> '+Long2StrDigits(NumRecsRead,6)+
               '   written --> '+Long2StrDigits(NumRecsWritten,6);
    WriteFooter(StatStr);
  end;
  {$F-}

  function Reconstruct : Boolean;
    {-Reconstruct the database from the datafile}
  var
    IID            : IsamIndDescr;
  begin
    IID[1].KeyL := Key1Len;
    IID[1].AllowDupK := False;
    IID[2].KeyL := Key2Len;
    IID[2].AllowDupK := True;
    IsamRexUserProcPtr := UserStatusRoutine;  {set user status procedure}
    RebuildVFileBlock(FName, SectionLength, 2, IID, BuildKey);
    Reconstruct := IsamOK;
  end;

  function OpenedFiles : Boolean;
    {-Try to open existing database files}
  var
    OK, OK1        : Boolean;
  begin
    OpenedFiles := False;
    repeat
      BTOpenFileBlock(Pf, FName, False, False, UseSaveMode,
                      BTNetSupported <> NoNet);
      OK := IsamOK;
      if not IsamOK then begin
        if IsamError = 10010 then begin
          if YesNo('Index file defective. Rebuild it?', 'Y') then
            OK1 := Reconstruct
          else
            Exit;
        end
        else if IsamError = 9903 then begin
          if IsamExists(IsamForceExtension(FName, SavExtension)) then begin
            {Previous SAV file found}
            if YesNo('Only data save file found. Rebuild fileblock?', 'Y') then
              OK1 := Reconstruct
            else
              Exit;
          end
          else if YesNo('Data file does not exist. Create new?', 'Y') then begin
            if not CreateFile then
              Exit;
          end
          else
            Exit;
        end
        else begin
          if YesNo('Data error '+Long2Str(IsamError)+'. Attempt rebuild?', 'Y') then
            OK1 := Reconstruct
          else
            Exit;
        end;
      end;
    until OK;
    OpenedFiles := True;
  end;

  procedure SwitchKeys;
    {-Make the other key active}
  begin
    ActKeyNr := (ActKeyNr and 1)+1;
    ActKey := BuildKey(Person, ActKeyNr);
  end;

  {---------------------------filtering hooks-----------------------------
    The following routine is used to implement the special filtering
    capabilities of SIMPDEMO.  When the F6 key is pressed, the user is
    prompted for information to be used to determine what records should
    appear in the Browser.
  ------------------------------------------------------------------------}
  {$F+}
  function ValidatePerson(IFBPtr     : IsamFileBlockPtr;
                          KeyNr      : Integer;
                          Ref        : LongInt;
                          var KeyStr : IsamKeyStr;
                          NetUsed    : Boolean) : Boolean;
    {-Validate a data record against the current Browser filter}
  var
    PersonRecord : PersonDef;
  begin
    ValidatePerson := False;
    BTGetRec(IFBPtr, Ref, PersonRecord, False);
    if not IsamOK then
      Exit;
    with PersonFilter do begin
      if Length(FirstName) > 0 then
        if not MatchString(FirstName, PersonRecord.FirstName) then
          Exit;
      if Length(Name) > 0 then
        if not MatchString(Name, PersonRecord.Name) then
          Exit;
      if Length(Company) > 0 then
        if not MatchString(Company, PersonRecord.Company) then
          Exit;
      if Length(Address) > 0 then
        if not MatchString(Address, PersonRecord.Address) then
          Exit;
      if Length(City) > 0 then
        if not MatchString(City, PersonRecord.City) then
          Exit;
      if Length(State) > 0 then
        if not MatchString(State, PersonRecord.State) then
          Exit;
      if Length(Zip) > 0 then
        if not MatchString(Zip, PersonRecord.Zip) then
          Exit;
      if Length(Telephone) > 0 then
        if not MatchString(Telephone, PersonRecord.Telephone) then
          Exit;
    end;
    ValidatePerson := True;
  end;
  {$F-}

  procedure SetFilterMode(On : Boolean);
    {-Turn Browser filtering on or off}
  begin
    if On then begin
      EnableFiltering(@ValidatePerson);
      {$IFDEF UseMouse}
      UseScrollBar := False;
      {$ENDIF}
      NoNetMode := False;
    end
    else begin
      DisableFiltering;
      {$IFDEF UseMouse}
      UseScrollBar := True;
      {$ENDIF}
      NoNetMode := (BTNetSupported = NoNet);
    end;
  end;

  procedure Filter;
    {-Prompt for information used by Browser filtering routines}
  var
    PersonGoal, PersonTemp : PersonDef;
    Esc : Boolean;
  begin
    if IsFilteringEnabled then begin
      {filtering already on, prompt user to disable}
      if YesNo('Browser filtering enabled, do you wish to disable?', 'N') then
        SetFilterMode(False);
    end
    else begin
      {get filtering information}
      ClearPerson(PersonGoal);
      ClearPerson(PersonTemp);
      Esc := not GetPerson(PersonGoal, False, 'Filtering Info', 0);
      WriteFooter('');
      if Esc or CompPerson(PersonTemp, PersonGoal) then
        Exit;

      {confirm user desires filtering}
      if YesNo('Enable filtering with this information?', 'Y') then begin
        PersonFilter := PersonGoal;
        SetFilterMode(True);
      end;
    end;
  end;


  procedure RebuildData;
    {-Purge deleted records and rebuild indices}
  begin
    WriteHeader(' Rebuild ');
    WriteFooter('Please wait... ');
    BTCloseFileBlock(Pf);
    if not IsamOK then begin
      IsamErrorNum(IsamError);
      Halt;
    end;
    if not Reconstruct then begin
      DispMessage('Unable to rebuild data files', True, True);
      Halt;
    end;
    if not OpenedFiles then begin
      IsamErrorNum(IsamError);
      Halt;
    end;
    BTSetSearchForSequential(Pf, 1, True);
    BTSetSearchForSequential(Pf, 2, True);
    ActRec := 0;
    ActKeyNr := 1;
    ActKey := '';
  end;

  {$IFDEF UseMouse}
  {$F+}
  procedure MouseHotSpotHandler(X, Y : Byte; var Cmd : BKtype);
    {-See if mouse is positioned over an item on the footer help line.}
  begin
    if Y = FooterRow then
      case X of
        3..8   : Cmd := BKuser2;  {F2 Add}
        10..15 : Cmd := BKuser3;  {F3 Del}
        17..23 : Cmd := BKuser4;  {F4 Find}
        25..30 : Cmd := BKuser5;  {F5 Key}
        32..40 : Cmd := BKuser6;  {F6 Filter}
        42..49 : Cmd := BKuser8;  {F8 Print}
        51..57 : Cmd := BKuser9;  {F9 Info}
        59..69 : Cmd := BKuser0;  {F10 Rebuild}
        71..78 : Cmd := BKquit;   {ESC}
      end;
  end;
  {$F-}
  {$ENDIF}

  procedure InitBrowseMap;
    {-Force complete screen rewrite}
  var
    I              : Integer;
  begin
    for I := 1 to BrowseRows do
      with BrowseMap[I] do begin
        CurRef := $FFFFFFFE;
        Horiz := 0;
        Highlighted := False;
      end;
  end;

  {$F+}

  function ValidateState(var FR         : FieldRec;
                         var ErrCode    : Byte;
                         var ErrorSt    : StringPtr) : Boolean;
    {-Validate a state entry}
  const
    StateStrings   : array[1..51] of array[1..2] of Char = (
      'AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 'GA', 'HI',
      'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 'MD', 'ME', 'MI', 'MN',
      'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 'NJ', 'NM', 'NV', 'NY', 'OH',
      'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 'WA',
      'WI', 'WV', 'WY');
    PartialErrorSt : String[27] = 'Partial entries not allowed';
    BadState       : String[36] = 'Not a valid abbreviation for a state';
  var
    I              : Word;
    S              : String[2];
  begin
    ValidateState := True;

    S := Trim(FR.EditSt^);
    if not ValidationOff then
      case Length(S) of
        1 :                  {no 1-character abbreviations}
          begin
            ErrCode := PartialError;
            ErrorSt := @PartialErrorSt;
            ValidateState := False;
          end;
        2 :                  {check list of valid abbreviations}
          begin
            for I := 1 to 51 do
              if S = StateStrings[I] then
                Exit;
            ErrCode := 10;
            ErrorSt := @BadState;
            ValidateState := False;
          end;
      end;
  end;

  function ValidatePhone(var FR         : FieldRec;
                         var ErrCode    : Byte;
                         var ErrorSt    : StringPtr) : Boolean;
    {-Validate a phone number}
  begin
    if ValidationOff then
      ValidatePhone := True
    else
      ValidatePhone := ValidateSubfields(ValidPhone, FR, ErrCode, ErrorSt);
  end;

  function ValidateZip(var FR         : FieldRec;
                       var ErrCode    : Byte;
                       var ErrorSt    : StringPtr) : Boolean;
    {-Validate a zip code}
  begin
    if ValidationOff then
      ValidateZip := True
    else
      ValidateZip := ValidateSubfields(ValidZip, FR, ErrCode, ErrorSt);
  end;

  procedure PhoneZipConversion(var FR : FieldRec; PostEdit : Boolean);
    {-Conversion routine for phone numbers and zip codes.}
    {-Special note: This special conversion routine is needed to meet the
      demands of the Search routine, which allows searches based on partial
      zip codes and phone numbers. The ValidationOff flag used in the three
      validation routines shown above is needed for the same reason.}
  var
    S              : String[20];
    SLen           : Byte absolute S;
    Done           : Boolean;
  begin
    with FR do
      if PostEdit then begin
        S := EditSt^;
        Done := False;
        repeat
          {trim trailing blanks and hyphens}
          case S[SLen] of
            ' ', '-' :
              Dec(SLen);
            else
              Done := True;
          end;
        until Done;
        StringPtr(VarPtr)^ := S;
      end
      else begin
        {is string too long? if so, truncate it}
        if Byte(VarPtr^) > EditLen then
          Byte(VarPtr^) := EditLen;

        {initialize the edit string}
        EditSt^ := String(VarPtr^);

        {merge picture mask characters if necessary}
        if Length(EditSt^) < Length(Picture^) then
          MergePicture(Picture^, EditSt^, EditSt^, EditFlags);
      end;
  end;

  procedure ErrorHandler(var ESR : ESrecord; Code : Byte; Msg : String);
    {-Display messages for errors reported by TPENTRY}
  begin
    DispMessage(Msg, True, True);
    case Code of
      InitError, OverflowError, MemoryError, ParamError :
        begin
          {a fatal error: set normal cursor and clear the screen}
          NormalCursor;
          ClrScr;
        end;
    end;
  end;

  procedure DisplayHelpPrompt(var ESR : ESrecord);
    {-Display a help prompt for the current field}
  var
    S              : String[80];
  begin
    case ESR.CurrentID of
      00 : S := 'Enter first name';
      01 : S := 'Enter last name';
      02 : S := 'Enter company name';
      03 : S := 'Enter street address';
      04 : S := 'Enter city of residence';
      05 : S := 'Enter state of residence';
      06 : S := 'Enter a 5- or 9-digit zip code';
      07 : S := 'Enter phone number';
      08 : S := 'Press <Enter> to edit memo field';
    end;
    WriteFooter(' <^Enter> Done  <Esc> Abort  '+S);
  end;

  procedure MemoErrorHandler(var EMCB : EMcontrolBlock; ErrorCode : Word);
    {-Display error message and wait for key press}
  begin
    case ErrorCode of
      tmBufferFull :
        DispMessage('Edit buffer is full', True, True);
      tmLineTooLong :
        DispMessage('Line too long, carriage return inserted', True, True);
      tmTooManyLines :
        DispMessage('Limit on number of lines has been reached', True, True);
      tmOverLineLimit :
        DispMessage('Limit on number of lines has been exceeded', True, True);
      else
        DispMessage('Unknown error', True, True);
    end;

    {redisplay our prompt}
    DisplayMemoPrompt;
  end;

  procedure MemoFieldStatus(var EMCB : EMcontrolBlock);
    {-Display status line for memo field}

  const
    StatusLine     : String[48] =
    {         1         2         3         4        }
    {123456789012345678901234567890123456789012345678}
    ' Line: xxx Column: xxx 100%  Insert Indent Wrap ';
    InsertSt       : array[Boolean] of String[6] = (' Over ', 'Insert');
    IndentSt       : array[Boolean] of String[6] = ('      ', 'Indent');
    WrapSt         : array[Boolean] of String[4] = ('    ', 'Wrap');
  var
    S              : String[5];
  begin
    with EMCB do begin
      {insert line number}
      S := Long2Str(CurLine);
      S := Pad(S, 3);
      Move(S[1], StatusLine[8], 3);

      {insert column number}
      S := Long2Str(CurCol);
      S := Pad(S, 3);
      Move(S[1], StatusLine[20], 3);

      {insert percentage of buffer used}
      S := Real2Str(Trunc((TotalBytes*100.0)/(BufSize-2)), 3, 0);
      Move(S[1], StatusLine[24], 3);

      {plug in state stuff}
      Move(InsertSt[InsertMode][1], StatusLine[30], 6);
      Move(IndentSt[IndentMode][1], StatusLine[37], 6);
      Move(WrapSt[WordWrap][1], StatusLine[44], 4);

      {display status line}
      FastWrite(StatusLine, MemoYH+1, MemoXL+1, FielAttr);
    end;
  end;

  {$F-}

  procedure InitEntryScreen;
    {-Set up for data entry screens}
  begin
    {clear the scrap record used for editing}
    ClearPerson(ScrapPerson);

    {initialize the edit screen record}
    InitESrecord(ESR);

    {set attributes}
    SetPromptAttr(FielAttr);
    SetFieldAttr(NormAttr);
    SetStringAttr(SeleAttr);
    SetCtrlAttr(SeleAttr);

    {set entry window coordinates}
    SetEntryWindow(ESR, EntryXL, EntryYL, EntryXH, EntryYH,
                   False, FielAttr, HeadAttr);

    {set delimiters}
    SetDelimiters(ESR, '[', ']', FielAttr, FielAttr);

    {install user-written event handlers}
    SetPreEditPtr(ESR, @DisplayHelpPrompt);
    SetErrorPtr(ESR, @ErrorHandler);

    {set edit screen options}
    SetWrapMode(ESR, WrapAtEdges);
    SetBeepOnError(ESR, On);

    {set field editing options}
    SetClearFirstChar(On);

    {add each of the edit fields in order: left to right, top to bottom}
    {                              Prompt                     Field   Fld Hlp Val}
    {                    Prompt    Row Col Picture            Row Col Len Ndx Ptr}
    {Conversion          Usr1 Usr2 Field                                         }

    {.F-}
    AddStringField(ESR,  'First name',01,05,'',               01, 21, 15, 00, nil,
                                   ScrapPerson.FirstName);

    AddStringField(ESR,  'Last name',02,05,'',                02, 21, 15, 01, nil,
                                   ScrapPerson.Name);

    AddStringField(ESR, 'Company', 03, 05, '',                03, 21, 25, 02, nil,
                                   ScrapPerson.Company);

    AddStringField(ESR, 'Address', 04, 05, '',                04, 21, 25, 03, nil,
                                   ScrapPerson.Address);

    AddStringField(ESR, 'City',    05, 05, '',                05, 21, 15, 04, nil,
                                   ScrapPerson.City);

    AddStringField(ESR, 'State',   06, 05, 'AA',              06, 21, 02, 05, @ValidateState,
                                   ScrapPerson.State);

    AddUserField(ESR, 'Zip',       07, 05, ZipMask,           07, 21, 10, 06, @ValidateZip,
    @PhoneZipConversion, nil, nil, ScrapPerson.Zip);

    AddUserField(ESR, 'Telephone', 08, 05, PhoneMask,         08, 21, 12, 07, @ValidatePhone,
    @PhoneZipConversion, nil, nil, ScrapPerson.Telephone);

    AddNestedField(ESR, 'Notes',   09, 05, '',                09, 21, 01, 08);
    {.F+}
  end;

  procedure InitMemoFields;
    {-Set up for memo fields}
  begin
    {install user-written event handlers}
    MemoStatusPtr := @MemoFieldStatus;
    MemoErrorPtr := @MemoErrorHandler;

    {deactivate <Esc>, use <^Enter> instead}
    if not AddMemoCommand(EMnone, 1, Ord(^[), 0) then {} ;
    if not AddMemoCommand(EMquit, 1, Ord(^J), 0) then {} ;

    {initialize the edit control block}
    InitControlBlock(
      EMCB,                  {control block}
      MemoXL,                {left column of edit window}
      MemoYL,                {top row of edit window}
      MemoXH,                {right column of edit window}
      MemoYH,                {bottom row of edit window}
      NormAttr,              {attribute for normal text}
      NormAttr,              {attribute for control characters}
      True,                  {insert mode on?}
      True,                  {auto-indent on?}
      True,                  {word wrap on?}
      8,                     {distance between tab stops}
      8,                     {help index}
      MemoXH-MemoXL,         {right margin}
      999,                   {maximum number of lines}
      SizeOf(MemoField),     {size of edit buffer}
      ScrapPerson.Notes);    {edit buffer}
  end;

  procedure SetColors;
    {-Choose attributes for display}
  begin
    {handle color mapping manually}
    MapColors := False;

    case CurrentMode of
      3 :
        begin
          NormAttr := $1E;
          SeleAttr := $3E;
          HighAttr := $4E;
          FielAttr := $1B;
          HeadAttr := $1F;
        end;
      else
        begin
          NormAttr := $07;
          SeleAttr := $70;
          HighAttr := $0F;
          FielAttr := $07;
          HeadAttr := $0F;
        end;
    end;
  end;

  procedure GetOptionsFromCommandLine;
    {-Get the network type (and station number if necessary) from Command line}
  type
    Str128 = String[128];
  var
    Opt : Str128;

    procedure ShowHelp;
      {-Display help message and halt}
    begin
      WriteLn('Usage: NETDEMO /opt');
      WriteLn;
      WriteLn('where opt is:');
        WriteLn('  /D     - Single-user DOS, no network');
      if MsNet in IsamCompiledNets then
        WriteLn('  /M     - MS-Net or compatible');
      if Novell in IsamCompiledNets then
        WriteLn('  /N     - Novell''s Advanced NetWare');
      Halt;
    end;

    procedure InvalidOption(Opt : String);
      {-Display invalid option message, show help, and halt}
    begin
      WriteLn('Invalid Option: ',Opt);
      WriteLn;
      ShowHelp;
    end;

  begin
    if BTNoNetCompiled then begin
      ActualNetType := NoNet;
      Exit;
    end;
    if ParamCount = 0 then
      ShowHelp;
    Opt := ParamStr(1);
    if (Length(Opt) < 2) or (Opt[1] <> '/') then
      InvalidOption(Opt);
    case UpCase(Opt[2]) of
      '?' : ShowHelp;
      'D' : ActualNetType := NoNet;
      'M' : ActualNetType := MsNet;
      'N' : ActualNetType := Novell;
      else InvalidOption(Opt);
    end; {case}

  end;

begin
  {parse the command line}
  GetOptionsFromCommandLine;

  {initialize screen}
  SetColors;
  InitEntryScreen;
  InitMemoFields;
  SaveAttr := TextAttr;
  TextAttr := NormAttr;
  CheckBreak := False;
  ClrScr;

  {other initialization}
  ActRec := 0;
  ActKeyNr := 1;
  ActKey := '';

  WriteHeader(' Initializing ');

  if ActualNetType = NoNet then
    {Let the browser work faster}
    Browser.NoNetMode := True;

  {Limit the number of index buffers}          {!!.40}
  Free := 10000;                               {!!.40}
  if MemAvail-Free > 200000 then               {!!.40}
    Free := MemAvail-200000;                   {!!.40}
  PS := BTInitIsam(ActualNetType, Free, 0);    {!!.22 moved} {!!.40}
  if not IsamOK then begin
    IsamErrorNum(IsamError);
    Halt;
  end;

  {allocate a buffer for variable length records} {!!.01}
  if not BTSetVariableRecBuffer(SectionLength) then begin
    DispMessageTemp('Insufficient memory. Program aborting.', 2000);
    Halt;
  end;

  UseSaveMode := YesNo('Should the files be handled using Save mode?', 'N');

  {$IFDEF Novell}                                               {!!.06 begin}
  if BTNetSupported = Novell then begin
    RefreshFunc := @SemaphoreRefresh;
    if OpenSemaphores then
      RefreshPeriod := 9            {check every half of a second}
    else begin
      DispMessageTemp('Error initializing semaphore object. Aborting.', 2000);
      Halt;
    end;
  end
  else
  {$ENDIF}                                                      {!!.06 end}
  RefreshFunc := @RefreshPeriodically;

  if not OpenedFiles then begin
    DispMessageTemp('Files could not be opened. Program aborting.', 2000);
    Halt;
  end;

  BTSetSearchForSequential(Pf, 1, True);
  BTSetSearchForSequential(Pf, 2, True);

  {add user-defined exit commands}
  if AddBrowseCommand(BKuser1, 1, F1, 0) then {};
  if AddBrowseCommand(BKuser2, 1, F2, 0) then {};
  if AddBrowseCommand(BKuser3, 1, F3, 0) then {};
  if AddBrowseCommand(BKuser4, 1, F4, 0) then {};
  if AddBrowseCommand(BKuser5, 1, F5, 0) then {};
  if AddBrowseCommand(BKuser6, 1, F6, 0) then {};
  if AddBrowseCommand(BKuser8, 1, F8, 0) then {};
  if AddBrowseCommand(BKuser9, 1, F9, 0) then {};
  if AddBrowseCommand(BKuser0, 1, F10, 0) then {};
  if AddBrowseCommand(BKrowEnd, 1, $4F00, 0) then {};  {End key to end of line}
  if AddBrowseCommand(BKrowBegin, 1, $4700, 0) then {};{home key to start of line}

  {$IFDEF UseMouse}
  UserMousePtr := @MouseHotSpotHandler;
  if MouseInstalled then begin
    {install event handler so MousePressed and MouseKeyWord will work}
    EnableBrowseMouse;
    EnableEntryMouse;
    EnableMemoMouse;
    ScrollBarAutoSize := False;
    ScrollBarUp := 4;
    ScrollBarHt := 18;
    SoftMouseCursor($0000, $7004);
  end;
  {$ENDIF}

  RowsToJump := 1;
  HorizOfs := 0;
  if BTNetSupported <> NoNet then
    UseReadLock := True;

  repeat
    {Update the screen and browse around the records}
    InitBrowseMap;
    WriteHeader(' Main Menu ');
                {0        1         2         3         4         5         6         7         8}
                {12345678901234567890123456789012345678901234567890123456789012345678901234567890}
    WriteFooter('  F2-Add F3-Del F4-Find F5-Key F6-Filter F8-Print F9-Info F10-Rebuild Esc-Quit');
    BrowExit := BKnone;
    BrowResp := BrowseAgain(Pf, True, ActKeyNr,
                            '', #255,
                            4, BrowseRows,
                            CurrentRow,
                            HorizOfs,
                            Person, DatLen,
                            ActRec, ActKey, BrowExit,
                            nil, @BuildaRow, @DisplayaRow);
    WriteFooter('');

    {Check for browse errors}
    case BrowResp of
      1 : if IsFilteringEnabled then begin
            DispMessage('No records found matching current filter. Filtering disabled.',
                        True, True);
            SetFilterMode(False);
          end
          else if YesNo('There are no records. Add one?', 'Y') then
            BrowExit := BKuser2
          else
            BrowExit := BKquit;
      2 : if BTIsamErrorClass > 2 then begin                  {!!.41}
            DispMessageTemp('Disk error, aborting. ', 2000);
            IsamErrorNum(IsamError);
            Halt;
          end else begin                                      {!!.41}
            {Not a fatal error; display it and continue}      {!!.41}
            IsamErrorNum(IsamError);                          {!!.41}
            BrowExit := BKnone;                               {!!.41}
          end;                                                {!!.41}
    end;

    {Handle requests for action}
    case BrowExit of
      BKenter,
      BKuser1 : Modify;
      BKuser2 : begin
                  NewStructure;
                  CurrentRow := 0;
                end;
      BKuser3 : Delete;
      BKuser4 : Search;
      BKuser5 : SwitchKeys;
      BKuser6 : begin
                  Filter;
                  CurrentRow := 0;
                end;
      BKuser8 : List;
      BKuser9 : Status;
      BKuser0 : RebuildData;
      BKquit :
        if not YesNo('Quit program?', 'N') then
          BrowExit := BKnone;
    end;
  until BrowExit = BKquit;

  {Close up the database}
  BTCloseFileBlock(Pf);
  if not IsamOK then
    DispMessageTemp('Data may be corrupt.', 2000);

  BTExitIsam;
  BTReleaseVariableRecBuffer;

  {clear the screen}
  TextAttr := SaveAttr;
  ClrScr;
  {$IFDEF Novell}                                          {!!.06}
  if BTNetSupported = Novell then                          {!!.06}
    CloseSemaphores;                                       {!!.06}
  {$ENDIF}                                                 {!!.06}
end.
