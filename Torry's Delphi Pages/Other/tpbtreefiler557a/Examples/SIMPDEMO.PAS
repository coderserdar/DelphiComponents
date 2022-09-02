{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{$I-,V-,S-,R-,F-,B-}

{$I BTDEFINE.INC}

program SimpleIsamDemo;
{$IFDEF UseTPCRT}
  {$I TPDEFINE.INC}
{$ENDIF}

{$IFDEF UseOPCRT}
  {$I OPDEFINE.INC}
{$ENDIF}

uses
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
  BTBase, BTIsBase,
  Filer,
  ReIndex,
  Reorg,
  Rebuild,
  {$IFDEF Novell}                                   {!!.06}
  NWBase, NWConn,                                   {!!.50}
  NWSema,                                           {!!.50}
  {$ENDIF}                                          {!!.06}
  Browser;

const
  Key1Len = 30;                   {Uppercase last name+first name}
  Key2Len = 5;                    {First five digits of zip}

  FName = 'SIMPADDR';             {Root name for database}
  LstDevice = 'PRN';              {Where printed output goes}
  BrowseRows = 20;                {Number of rows in browse window}
  FooterRow  = 25;                {row number for footer}

  Header : string[80] =           {Basic string used to build display header}
  ' B-Tree Filer Demo Program                                                      ';

  F1 = $3B00;                     {Keycodes for function keys}
  F2 = $3C00;
  F3 = $3D00;
  F4 = $3E00;
  F5 = $3F00;
  F6 = $4000;
  F7 = $4100;
  F8 = $4200;
  F9 = $4300;
  F10 = $4400;

  EntryX = 35;                    {Leftmost data entry window coordinate}
  EntryY = 6;                     {Topmost data entry window coordinate}

type
  CharSet = set of Char;

  PersonDef =                     {Definition of the database record}
    record
      Dele : LongInt;
      FirstName : string[15];
      Name : string[15];
      Company : string[25];
      Address : string[25];
      City : string[15];
      State : string[2];
      Zip : string[10];
      Telephone : string[12];
    end;

  BrowseMapRec =                  {Used for browse screen efficiency}
    record
      CurRef : LongInt;
      Horiz : Integer;
      Highlighted : Boolean;
    end;
  BrowseMapArray = array[1..BrowseRows] of BrowseMapRec;

  FrameCharType = (ULeft, LLeft, URight, LRight, Horiz, Vert);
  FrameArray = array[FrameCharType] of Char;

var
  PS : Integer;                   {Pages in page stack}
  Free : LongInt;                 {Heap bytes to keep free} {!!.40}
  Pf : IsamFileBlockPtr;          {Isam management variable}

  Person : PersonDef;             {Currently selected record}
  ActRec : LongInt;               {Record number currently selected}
  ActKeyNr : Integer;             {Active key number, 1 or 2}
  ActKey : IsamKeyStr;            {Active key string}

  BrowResp : Integer;
  DatLen : Word;
  Err : Integer;
  BrowExit : BKtype;
  AC : Char;
  UseSaveMode : Boolean;
  Locked : Boolean;
  HorizOfs : Integer;
  CurrentRow : Integer;           {stores the row offset of selected record}

  BrowseMap : BrowseMapArray;     {For screen management}

  NormAttr : Byte;                {Colors}
  SeleAttr : Byte;
  HighAttr : Byte;
  FielAttr : Byte;
  HeadAttr : Byte;

  PersonFilter : PersonDef;       {used by the Browser filter routines}

const
  ActualNetType : NetSupportType = NoNet;

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
  {$ELSE}
  function MouseOrKeyPressed : Boolean;

  begin
    MouseOrKeyPressed := KeyPressed;
  end;

  function ReadKeyOrMouse : Char;
  begin
    ReadKeyOrMouse := ReadKey;
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
    IsLockError := (BTIsamErrorClass = 2);              {!!.06}
  end;

  procedure Beep;
    {-Make a noise during editing}
  begin
    {No beeps here}
  end;

  function Upper(S : string) : string;
    {-Raise a string to uppercase}
  var
    I : Integer;
  begin
    for I := 1 to Length(S) do
      case S[I] of
        'a'..'z' : S[I] := Upcase(S[I]);
        '„' : S[I] := 'Ž';
        '”' : S[I] := '™';
        '' : S[I] := 'š';
      end;
    Upper := S;
  end;

  function Trim(S : string) : string;
    {-Remove trailing blanks}
  var
    SLen : Byte absolute S;
  begin
    while (SLen > 0) and (S[SLen] = ' ') do
      Dec(SLen);
    Trim := S;
  end;

  function Extend(S : string; Len : Byte) : string;
    {-Pad or truncate string to specified length}
  var
    SLen : Byte absolute S;
  begin
    if SLen >= Len then
      SLen := Len
    else begin
      FillChar(S[Succ(SLen)], Len-SLen, ' ');
      SLen := Len;
    end;
    Extend := S;
  end;

  procedure WriteString(S : string; X, Y : Integer);
    {-Write string at position X,Y}
  begin
    GotoXY(X, Y);
    Write(S);
  end;

  procedure FrameWindow(LeftCol, TopRow, RightCol, BotRow : Byte);
    {-Draws a frame around a window}
  const
    FrameChars : FrameArray = 'ÕÔ¸¾Í³';
  var
    Row, Width : Byte;
    Span : string[132];
    SpanLen : Byte absolute Span;
  begin
    {calculate width of window}
    SpanLen := Succ(RightCol-LeftCol);
    Width := SpanLen-2;

    {construct the upper border and draw it}
    FillChar(Span[2], Width, FrameChars[Horiz]);
    Span[1] := FrameChars[ULeft];
    Span[SpanLen] := FrameChars[URight];
    WriteString(Span, LeftCol, TopRow);

    {draw the vertical bars}
    for Row := Succ(TopRow) to Pred(BotRow) do begin
      WriteString(FrameChars[Vert], LeftCol, Row);
      WriteString(FrameChars[Vert], RightCol, Row);
    end;

    {draw the bottom border}
    Span[1] := FrameChars[LLeft];
    Span[SpanLen] := FrameChars[LRight];
    WriteString(Span, LeftCol, BotRow);
  end;

  procedure WriteHeader(Prompt : string);
    {-Write header and bottom divider}
  var
    S : string;
    I, J, L : Integer;
  begin
    TextAttr := HeadAttr;
    FrameWindow(1, 2, 80, 24);
    S := Header;
    L := Length(Prompt);
    if L > 80 then
      L := 80;
    J := 40-(L shr 1);
    for I := 1 to L do
      S[J+I] := Prompt[I];
    WriteString(S, 1, 1);
    if ActKeyNr = 1 then
      S := ' Key: Last Name '
    else
      S := ' Key: Zip Code ';
    if IsFilteringEnabled then
      WriteString('®Filter¯', 50, 1);
    WriteString(S, 62, 1);
  end;

  procedure WriteFooter(Prompt : string);
    {-Write a footer on the menu line}
  begin
    TextAttr := HeadAttr;
    WriteString(Prompt, 1, 25);
    ClrEOL;
  end;



  procedure GetKey(var Special : Boolean; var C : Char);
    {-Get a key, returning Special=True for extended keystrokes}
  begin
    Special := False;
    C := ReadKeyOrMouse;
    if C = #0 then begin
      C := ReadKeyOrMouse;
      Special := True;
    end;
  end;

  function Menu(Selection : CharSet; Prompt : string) : Char;
    {-Draw a bar menu and get a selection in the CharSet}
  var
    C : Char;
  begin
    TextAttr := HeadAttr;
    WriteString(Prompt, 1, FooterRow);
    ClrEOL;
    if Selection = [] then
      Exit;
    while KeyPressed do
      C := ReadKeyOrMouse;
    repeat
      C := Upcase(ReadKeyOrMouse);
    until (C in Selection);
    GotoXY(1, FooterRow);
    ClrEOL;
    Menu := C;
  end;

  procedure DispMessage(Prompt : string; WaitKey : Boolean);
    {-Display a message on the menu line, optionally waiting for keystroke}
  var
    C : Char;
  begin
    if WaitKey then begin
      WriteFooter(' '+Prompt+'.  Press any key.');
      C := ReadKeyOrMouse;
      if C = #0 then
        C := ReadKeyOrMouse;
    end else
      WriteFooter(' '+Prompt);
  end;

  procedure DispMessageTemp(Prompt : string; Time : Word);
    {-Display a timed message}
  begin
    WriteFooter(Prompt);
    Delay(Time);
    WriteFooter('');
  end;

  procedure IsamErrorNum(F : Integer);
    {-Display Isam error number and wait for key}
  var
    S : string;
  begin
    Str(F:4, S);
    DispMessage('IsamError : '+S, True);
  end;

  function YesNo(Prompt : string) : Boolean;
    {-Display Yes/No prompt}
  begin
    YesNo := (Menu(['Y', 'N'], Prompt+' (Y/N)') = 'Y');
  end;

  function LockAbort : Boolean;
    {-If a file lock prevents progress, ask whether to try again}
  begin
    LockAbort := False;
    Locked := IsLockError;
    if not Locked then
      Exit;
    LockAbort := YesNo('A lock prevents access; abort (Y) or try again (N)?');
  end;

  function Aborting : Boolean;
    {-Check for a keypress during printing, and offer a chance to quit}
  var
    C : Char;
  begin
    Aborting := False;
    if KeyPressed then begin
      repeat
        C := ReadKey;
      until not KeyPressed;
      if YesNo('Do you really wish to quit?') then
        Aborting := True;
    end;
  end;

  procedure GetString(var S : string; X, Y, MaxLen : Integer;
                      Abort : CharSet; var AC : Char);
    {-Simple line editor to enter fields of a record}
  var
    Help, P : Integer;
    Ch, Key : Char;
    Special : Boolean;
    L : Integer;
  begin
    S := Extend(S, MaxLen);
    L := Length(S);
    P := 1;
    TextAttr := SeleAttr;
    repeat
      GotoXY(X, Y);
      Write(S);
      GotoXY(X+P-1, Y);
      GetKey(Special, Key);
      Ch := Key;
      if not Special then
        if Ch in [#32..#255] then begin
          S[P] := Ch;
          if P < L then
            Inc(P);
        end else
          case Ch of
            #8 :
              if P > 1 then begin
                for Help := P-1 to L-1 do
                  S[Help] := S[Help+1];
                S[L] := ' ';
                Dec(P);
              end;
            #13, #27 :
              Special := True;
          else
            Beep;
          end;

      if Special and not(Ch in Abort) then begin
        case Ch of
          #75 :                   {Left}
            if P > 1 then
              Dec(P)
            else
              Beep;
          #77 :                   {Right}
            if P < L then
              Inc(P)
            else
              Beep;
          #71 :                   {Home}
            P := 1;
          #79 :                   {End}
            P := L;
          #83 :                   {Del}
            if P = L then
              S[P] := ' '
            else begin
              Help := P;
              for Help := P to L-1 do S[
                Help] := S[Succ(Help)];
              S[L] := ' ';
            end;
          #82 :                   {Insert}
            begin
              for Help := L-1 downto P do
                S[Help+1] := S[Help];
              S[P] := ' ';
            end;
          #67 :                   {ClrEol}
            for Help := P to L do
              S[P] := ' ';
          #73 :                   {ClrBol}
            begin
              for Help := 1 to L do
                S[Help] := ' ';
              P := 1;
            end;
        else
          if not(Ch in Abort) then
            Beep;
        end;
      end;
    until Special and (Ch in Abort);
    AC := Ch;
    TextAttr := NormAttr;
    GotoXY(X, Y);
    Write(S);
    S := Trim(S);
    GotoXY(X+P-1, Y);
  end;

  procedure ClearPerson(var Person : PersonDef);
    {-Set up for a new person record}
  begin
    FillChar(Person, SizeOf(PersonDef), 0);
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
    CompPerson := True;
  end;

  procedure ShowText;
    {-Show the fixed part of the person record display}
  begin
    TextAttr := HeadAttr;
    FrameWindow(EntryX-2, EntryY-1, EntryX+39, EntryY+17);
    TextAttr := FielAttr;
    Window(EntryX-1, EntryY, EntryX+38, EntryY+16);
    ClrScr;
    Window(1, 1, 80, 25);

    WriteString('First name :               :', EntryX, EntryY);
    WriteString('Last name  :               :', EntryX, EntryY+2);
    WriteString('Company    :                         :', EntryX, EntryY+4);
    WriteString('Address    :                         :', EntryX, EntryY+6);
    WriteString('City       :               :', EntryX, EntryY+8);
    WriteString('State      :  :', EntryX, EntryY+10);
    WriteString('Zip        :          :', EntryX, EntryY+12);
    WriteString('Telephone  :            :', EntryX, EntryY+14);
    WriteString('Record     :            :', EntryX, EntryY+16);
  end;

  procedure DisplayPerson(var Person : PersonDef);
    {-Show data about person}
  var
    Temp : string[20];
  begin
    TextAttr := NormAttr;
    with Person do begin
      WriteString(FirstName, EntryX+12, EntryY);
      WriteString(Name, EntryX+12, EntryY+2);
      WriteString(Company, EntryX+12, EntryY+4);
      WriteString(Address, EntryX+12, EntryY+6);
      WriteString(City, EntryX+12, EntryY+8);
      WriteString(State, EntryX+12, EntryY+10);
      WriteString(Zip, EntryX+12, EntryY+12);
      WriteString(Telephone, EntryX+12, EntryY+14);
      if ActRec > 0 then
        Str(ActRec, Temp)
      else
        Temp := '            ';
      WriteString(Temp, EntryX+12, EntryY+16);
    end;
  end;

  procedure GetPerson(var Person : PersonDef);
    {-Edit a person record}
  var
    ExitChars : CharSet;
    Field : Integer;
  begin
    WriteFooter('<Esc> ends entry ');
    Field := 1;
    ExitChars := [#13, #72, #80, #27];
    with Person do
      repeat
        case Field of
          1 : GetString(FirstName, EntryX+12, EntryY, 15, ExitChars, AC);
          2 : GetString(Name, EntryX+12, EntryY+2, 15, ExitChars, AC);
          3 : GetString(Company, EntryX+12, EntryY+4, 25, ExitChars, AC);
          4 : GetString(Address, EntryX+12, EntryY+6, 25, ExitChars, AC);
          5 : GetString(City, EntryX+12, EntryY+8, 15, ExitChars, AC);
          6 : GetString(State, EntryX+12, EntryY+10, 2, ExitChars, AC);
          7 : GetString(Zip, EntryX+12, EntryY+12, 10, ExitChars, AC);
          8 : GetString(Telephone, EntryX+12, EntryY+14, 12, ExitChars, AC);
        end;

        case AC of
          #13, #80 :
            if Field = 8 then
              Field := 1
            else
              Inc(Field);
          #72 :
            if Field = 1 then
              Field := 8
            else
              Dec(Field);
        end;
      until AC = #27;
    WriteFooter('                              ');
  end;

  function CreateFile : Boolean;
    {-Create the database fileblock}
  var
    IID : IsamIndDescr;
  begin
    IID[1].KeyL := Key1Len;
    IID[1].AllowDupK := False;
    IID[2].KeyL := Key2Len;
    IID[2].AllowDupK := True;
    BTCreateFileBlock(FName, SizeOf(PersonDef), 2, IID);
    CreateFile := IsamOK;
  end;

  function PersonLine(var Person : PersonDef) : string;
    {-Return a string representing Person}
  begin
    with Person do
      PersonLine := (Extend(Zip, 5)+' '+
                     Extend(Trim(Name)+', '+Trim(FirstName), 19)+' '+
                     Extend(Company, 19)+' '+
                     Extend(Address, 19)+' '+
                     Extend(City, 13)+' '+
                     Extend(State, 2)+' '+
                     Telephone);
  end;

  {$F+}
  function BuildKey(var P; KeyNr : Word) : IsamKeyStr;
    {-Return the key string for either of the two indexes}
  begin
    with PersonDef(P) do
      case KeyNr of
        1 : BuildKey := Extend(Upper(Trim(Name)),20)+
                        Extend(Upper(Trim(FirstName)),10);
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
    ScreenCols = 78;
  {
           1         2         3         4         5         6         7         8         9         1
  1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
   Zip   Name                Company             Address             City         St  Phone
  zzzzz nnnnnnnnnnnnnnnnnnn ccccccccccccccccccc aaaaaaaaaaaaaaaaaaa ccccccccccccc ss ppp-ppp-pppp
  }
    Header =
    ' Zip   Name                Company             Address             City         St  Phone';
  begin
    if HorizOfs > 17 then
      HorizOfs := 17;

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

    if RowNr = 1 then begin
      {Write the header line now}
      TextAttr := HighAttr;
      GotoXY(2, 3);
      Write(Extend(Copy(Header, HorizOfs+1, ScreenCols), ScreenCols));
    end;

    if HighLight then
      TextAttr := SeleAttr
    else
      TextAttr := NormAttr;
    GotoXY(2, Pred(StartRow)+RowNr);
    Write(Extend(Copy(RR.Row, HorizOfs+1, ScreenCols), ScreenCols));
  end;
  {$F-}

  function AddStructure(var P : PersonDef; var Rec : LongInt) : Boolean;
    {-Add new record}
  begin
    AddStructure := False;
    repeat
      BTAddRec(Pf, Rec, P);
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
      BTPutRec(Pf, Rec, P, False);
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
      BTDeleteRec(Pf, Rec);
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
    Rec : LongInt;
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
    OK : Boolean;
  begin
    LockAll := False;
    repeat
      BTLockAllOpenFileBlocks;
      if not IsamOK then begin
        if YesNo('The file is presently in use; abort (Y) or try again (N)?') then
          Exit;
        OK := False;
      end else
        OK := True;
    until OK;
    LockAll := True;
  end;

  procedure NewStructure;
    {-Prompt for and add new record}
  label
    Retry;
  var
    PersonTemp : PersonDef;
    Key1, Key2 : IsamKeyStr;
    Rec : LongInt;
    KExists, OK : Boolean;
  begin
    WriteHeader(' New Entry ');
    ShowText;
    ClearPerson(PersonTemp);

Retry:
    {Get the new record}
    repeat
      GetPerson(PersonTemp);
      Key1 := BuildKey(PersonTemp, 1);
      Key2 := BuildKey(PersonTemp, 2);
      if Length(Trim(Key1)) = 0 then begin
        OK := False;
        if YesNo('The name must be entered; abort (Y) or try again (N)?') then
          Exit;
      end else
        OK := True;
    until OK;

    if not YesNo('Store record?') then
      Exit;

    {Lock the database in order to safely add the record}
    if not LockAll then
      Exit;

    {Assure it's not a duplicate key}
    OK := True;
    repeat
      KExists := BTKeyExists(Pf, 1, Rec, Key1);
      if LockAbort then begin
        BTUnLockAllOpenFileBlocks;
        Exit;
      end;
    until not Locked;
    if KExists then begin
      BTUnLockAllOpenFileBlocks;
      if YesNo('The name already exists; abort (Y) or try again (N)?') then
        Exit
      else
        goto Retry;
    end;

    {Add the record and its keys}
    if OK then
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
    PersonTemp : PersonDef;
    PersonTemp1 : PersonDef;
    KExists, OK : Boolean;
    Rec : LongInt;
  begin
    WriteHeader(' Modify ');
    ShowText;
    DisplayPerson(Person);
    PersonTemp := Person;

Retry:
    repeat
      GetPerson(PersonTemp);
      if Length(Trim(PersonTemp.Name)) = 0 then begin
        OK := False;
        if YesNo('Last name must not be empty; abort (Y) or try again (N)?') then
          Exit;
      end else
        OK := True;
    until OK;
    if CompPerson(Person, PersonTemp) then begin
      DispMessageTemp('Files not changed. ', 1000);
      Exit;
    end;

    {The following question to abort would be best asked after the checking
     Key1, but the Fileblock should be locked there. This would keep the
     Fileblock locked by a slow user for too long.}
    if not YesNo('Store modified data?') then
      Exit;

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
        if YesNo('The name already exists; abort (Y) or try again (N)?') then
          Exit
        else
          goto Retry;
      end;
    end;

    Rec := ActRec;
    {Read actual disk data}
    BTGetRec(Pf, Rec, PersonTemp1, False);
    if not IsamOK then begin
      BTUnLockAllOpenFileBlocks;
      DispMessageTemp('Record could not be read from disk. ', 1000);
      Exit;
    end;

    if PersonTemp1.Dele <> 0 then begin
      BTUnLockAllOpenFileBlocks;
      DispMessageTemp('The record has been erased in the meantime. ', 1000);
      Exit;
    end;

    if not CompPerson(PersonTemp1, Person) then begin
      BTUnLockAllOpenFileBlocks;
      DispMessageTemp('The record has been changed in the meantime. ', 1000);
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
    Key1, Key2 : IsamKeyStr;
    OK : Boolean;
  begin
    WriteHeader(' Deleting ');
    ShowText;
    DisplayPerson(Person);
    if not YesNo('Should the record really be deleted?') then
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

  function MatchString(var SG, ST : string) : Boolean;
    {-Return true if SG and ST match}
  begin
    if Length(SG) = 0 then
      {Nothing to match against}
      MatchString := True
    else
      {Match if ST starts with SG}
      MatchString := (Pos(Upper(SG), Upper(ST)) = 1);
  end;

  function MatchPerson(var Pg, PT : PersonDef) : Boolean;
    {-Compare two person records}
  begin
    MatchPerson := False;
    if PT.Dele <> 0 then
      Exit;
    if not MatchString(Pg.FirstName, PT.FirstName) then
      Exit;
    if not MatchString(Pg.Name, PT.Name) then
      Exit;
    if not MatchString(Pg.Company, PT.Company) then
      Exit;
    if not MatchString(Pg.Address, PT.Address) then
      Exit;
    if not MatchString(Pg.City, PT.City) then
      Exit;
    if not MatchString(Pg.State, PT.State) then
      Exit;
    if not MatchString(Pg.Zip, PT.Zip) then
      Exit;
    if not MatchString(Pg.Telephone, PT.Telephone) then
      Exit;
    MatchPerson := True;
  end;

  function GetNextRec(var Fptr : IsamFileBlockPtr;
                      var Data;
                      KeyNr : Integer;
                      var Rec : LongInt;
                      var UserKey : IsamKeyStr) : Boolean;
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
      BTGetRec(Fptr, Rec, Data, False);
      if LockAbort then
        Exit;
    until not Locked;
  end;

  procedure Search;
    {-Search for a record}
  var
    R : LongInt;
    SearchKey : Integer;
    OK : Boolean;
    Found : Boolean;
    Key : IsamKeyStr;
    PersonGoal : PersonDef;
    PersonTemp : PersonDef;
  begin
    WriteHeader(' Search Key ');
    ShowText;
    ClearPerson(PersonGoal);
    ClearPerson(PersonTemp);

    {Get search target}
    GetPerson(PersonGoal);
    WriteFooter('');
    if CompPerson(PersonTemp, PersonGoal) then
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
          DispMessage('No matching record found', True)
        else
          IsamErrorNum(IsamError);
        Exit;
      end;

      {Get the record}
      repeat
        BTGetRec(Pf, R, PersonTemp, False);
        if LockAbort then
          Exit;
      until not Locked;

      {Position current record pointer at least near to the goal}
      ActRec := R;
      ActKey := BuildKey(PersonTemp, ActKeyNr);

      {Does it match the goal?}
      Found := MatchPerson(PersonGoal, PersonTemp);

    end else begin
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
      until Found or (R = ActRec);
    end;

    if Found then begin
      ActRec := R;
      ActKey := BuildKey(PersonTemp, ActKeyNr);
    end else
      DispMessage('No matching record found', True);

  end;

  procedure Status;
    {-Show the number of records}
  var
    F, U : LongInt;
    S : string[80];
    Temp : string[20];
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

    Str(U, S);
    Str(F, Temp);
    S := 'Records used:'+S+', Deleted:'+Temp+', Mode:';
    if UseSaveMode then
      Temp := 'Save'
    else
      Temp := 'Normal';

    S := S+Temp+', Station:';
    Str(BTGetInternalDialogID(Pf), Temp); {!!.40}
    S := S+Temp;

    DispMessage(S, True);
  end;

  procedure List;
    {-List all records to printer}
  var
    T : LongInt;
    Rec : LongInt;
    KeyNr : Integer;
    Key : IsamKeyStr;
    OK : Boolean;
    C : Char;
    Lst : Text;
  begin
    WriteHeader(' List ');

    {Assure there are records to print}
    repeat
      T := BTUsedRecs(Pf);
      if LockAbort then
        Exit;
    until not Locked;
    if T = 0 then begin
      DispMessage('No records available', True);
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
      WriteFooter('Press any key to abort print ');
      repeat
        WriteLn(Lst, PersonLine(Person));
        OK := (IoResult = 0);
        if not OK then
          DispMessage('Printer error', True)
        else if Aborting then
          OK := False;
        if OK then
          OK := GetNextRec(Pf, Person, KeyNr, Rec, Key);
        if Locked then
          OK := False;
      until not(IsamOK and OK);
      Close(Lst);
    end;
  end;

  function Long2Str(L : LongInt; NumDigits : Byte) : String;
  {-Convert a longint to a string, right justified to NumDigits}
  var
    S : String;
  begin
    Str(L:NumDigits,S);
    Long2Str := S;
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
    if MouseOrKeyPressed then
      case ReadKeyOrMouse of
        ^[, ^C : {**AbortReorg := True;**} IsamOK := False;
      end;
    StatStr := 'Working on key --> '+Long2Str(KeyNr,1)+
               '   records read --> '+Long2Str(NumRecsRead,6)+
               '   written --> '+Long2Str(NumRecsWritten,6);
    WriteFooter(StatStr);
  end;
  {$F-}

  function Reconstruct : Boolean;
    {-Reconstruct the database from the datafile.}
  var
    IID : IsamIndDescr;
  begin
    IID[1].KeyL := Key1Len;
    IID[1].AllowDupK := False;
    IID[2].KeyL := Key2Len;
    IID[2].AllowDupK := True;
    IsamRexUserProcPtr := UserStatusRoutine;  {set user status procedure}
    RebuildFileBlock(FName, SizeOf(PersonDef), 2, IID, BuildKey);
    Reconstruct := IsamOK;
  end;

  function OpenedFiles : Boolean;
    {-Try to open existing database files}
  var
    OK, OK1 : Boolean;
    S : string[7];
  begin
    OpenedFiles := False;
    repeat
      BTOpenFileBlock(Pf, FName, False, False, UseSaveMode,
                      BTNetSupported <> NoNet);
      OK := IsamOK;
      if not OK then begin
        if IsamError = 10010 then begin
          if YesNo('Index file defective. Rebuild it?') then
            OK1 := Reconstruct
          else
            Exit;
        end else if IsamError = 9903 then begin
          if IsamExists(IsamForceExtension(FName, SavExtension)) then begin
            {Previous SAV file found}
            if YesNo('Only data save file found. Rebuild fileblock?') then
              OK1 := Reconstruct
            else
              Exit;
          end else if YesNo('Data file does not exist. Create new?') then begin
            if not CreateFile then
              Exit;
          end else
            Exit;
        end else begin
          Str(IsamError, S);
          if YesNo('Data error '+S+'. Attempt rebuild?') then
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
    capabilites of SIMPDEMO.  When the F6 key is pressed, the is prompted
    for information to be used to determine what records should appear in
    the Browser.
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
  begin
    if IsFilteringEnabled then begin
      {filtering already on, prompt user to disable}
      if YesNo('Browser filtering enabled, do you wish to disable?') then
        SetFilterMode(False);
    end
    else begin
      {get filtering information}
      WriteHeader(' Filtering Info ');
      ShowText;
      ClearPerson(PersonGoal);
      ClearPerson(PersonTemp);
      GetPerson(PersonGoal);
      WriteFooter('');
      if CompPerson(PersonTemp, PersonGoal) then
        Exit;

      {confirm user desires filtering}
      if YesNo('Enable filtering with this information?') then begin
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
      DispMessage('Unable to rebuild data files',True);
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
    I : Integer;
  begin
    for I := 1 to BrowseRows do
      with BrowseMap[I] do begin
        CurRef := $FFFFFFFE;
        Horiz := 0;
        Highlighted := False;
      end;
  end;

  procedure SetColors;
    {-Choose attributes for display}
  begin
    case LastMode and $FF of
      3 : begin
            NormAttr := $1E;
            SeleAttr := $3E;
            HighAttr := $4E;
            FielAttr := $17;
            HeadAttr := $1F;
          end;
    else
      NormAttr := $07;
      SeleAttr := $70;
      HighAttr := $0F;
      FielAttr := $07;
      HeadAttr := $0F;
    end;
  end;

  procedure ParseCommandLine;

    procedure ShowHelp;
      {-Display help message and halt}
    begin
      WriteLn('Usage: SIMPDEMO /opt');
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

  var
    Opt : String[128];

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
      'N' : ActualNetType := Novell;
      'M' : ActualNetType := MsNet;
      'D' : ActualNetType := NoNet;
      else InvalidOption(Opt);
    end; {case}
  end;

var
  SaveExitProc : Pointer;

  {$F+}
  procedure ExitHandler;
    {-This exit handler unlocks any locked files, closes them,
    and tells Filer to shutdown. It is a bit overkill for this
    simple application, but is a good example.}

  begin
    ExitProc := SaveExitProc;
    BTUnlockAllOpenFileBlocks;
    BTCloseAllFileBlocks;
    BTExitIsam;
    {$IFDEF UseMouse}
    if MouseInstalled then begin
      HideMouse;
      {install event handler so MousePressed and MouseKeyWord will work}
      DisableEventHandling;
    end;
    {$ENDIF}
    NormVideo;
  end;
  {$F-}


begin
  ParseCommandLine;
  SetColors;
  TextAttr := NormAttr;
  SaveExitProc := ExitProc;
  ExitProc := @ExitHandler;
  CheckBreak := False;
  ClrScr;
  ActRec := 0;
  ActKeyNr := 1;
  ActKey := '';
  WriteHeader(' Initializing ');

  {Limit the number of index buffers}          {!!.40}
  Free := 10000;                               {!!.40}
  if MemAvail-Free > 200000 then               {!!.40}
    Free := MemAvail-200000;                   {!!.40}
  PS := BTInitIsam(ActualNetType, Free, 0);    {!!.22 moved} {!!.40}
  if not IsamOK then begin
    IsamErrorNum(IsamError);
    Halt;
  end;

  UseSaveMode := YesNo('Should the files be handled using Save mode?');

  if not OpenedFiles then begin
    DispMessageTemp('Files could not be opened. Program aborting. ', 2000);
    Halt;
  end;

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
    ScrollBarAutoSize := False;
    ScrollBarUp := 4;
    ScrollBarHt := 18;
    AutoScaleMouse := True;
    SoftMouseCursor($0000, $7004);
  end;
  {$ENDIF}

  RowsToJump := 1;
  HorizOfs := 0;
  CurrentRow := 0;
  if BTNetSupported <> NoNet then
    UseReadLock := True;

  repeat
    InitBrowseMap;
    {Update the screen and browse around the records}
    WriteHeader(' Main Menu ');
                {0        1         2         3         4         5         6         7         8}
                {12345678901234567890123456789012345678901234567890123456789012345678901234567890}
    WriteFooter('  F2-Add F3-Del F4-Find F5-Key F6-Filter F8-Print F9-Info F10-Rebuild Esc-Quit');
    BrowExit := BKnone;
    BrowResp := BrowseAgain(Pf, False, ActKeyNr,
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
                        True);
            SetFilterMode(False);
          end
          else if YesNo('There are no records. Add one?') then
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
        if not YesNo('Quit program?') then
          BrowExit := BKnone;
    end;
  until BrowExit = BKquit;

  {Close up the database}
  BTCloseFileBlock(Pf);
  if not IsamOK then
    DispMessageTemp('Data may be corrupt. ', 2000);
  {$IFDEF Novell}                                          {!!.06}
  if BTNetSupported = Novell then                          {!!.06}
    CloseSemaphores;                                       {!!.06}
  {$ENDIF}                                                 {!!.06}
end.
