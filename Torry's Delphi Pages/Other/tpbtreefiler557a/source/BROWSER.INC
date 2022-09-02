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

{!!.41 numerous changes made to clean up UseReadLock handling}

  function bKeyPressed : Boolean;
  begin
    {$IFDEF UseMouse}
    if BrowseMouseEnabled then
      bKeyPressed := KeyPressed or MousePressed
    else
    {$ENDIF}
      bKeyPressed := KeyPressed;
  end;

  function RefreshAtEachCommand(IFBPtr : IsamFileBlockPtr;
                                KeyNo : Integer) : Boolean;
    {-Check for need to refresh before each command if no keystrokes pending}
  begin
    if bKeyPressed then
      RefreshAtEachCommand := False
    else
      RefreshAtEachCommand := BTOtherWSChangedKey(IFBPtr, KeyNo);
  end;

  function RefreshPeriodically(IFBPtr : IsamFileBlockPtr;
                               KeyNo : Integer) : Boolean;
    {-Check for need to refresh every RefreshPeriod clock ticks}
  var
    Ticks : LongInt absolute $40:$6C;
    T : LongInt;
  begin
    {assume false}
    RefreshPeriodically := False;
    T := Ticks;
    {loop while key not pressed}
    while not bKeyPressed do
      {is it time to check again?}
      if (Ticks-T) >= RefreshPeriod then
        {check to see if page stack has been invalidated}
        if BTOtherWSChangedKey(IFBPtr, KeyNo) then begin
          {we need to refresh the display}
          RefreshPeriodically := True;
          Exit;
        end
        else
          {save the current tick count}
          T := Ticks;
  end;

  function CallRefreshFunc(IFBPtr : IsamFileBlockPtr;
                           KeyNo : Integer) : Boolean;
    Inline($FF/$1E/>REFRESHFUNC); {call far dword ptr [>RefreshFunc]}

  function NeedRefresh(IFBPtr : IsamFileBlockPtr; KeyNo : Integer) : Boolean;
    {-Return true if screen refresh is needed}
  begin
    NeedRefresh := False;
    if BTIsNetFileBlock(IFBPtr) then
      if RefreshFunc <> Nil then
        NeedRefresh := CallRefreshFunc(IFBPtr, KeyNo);
  end;

  function BrowseI(IFBPtr : IsamFileBlockPtr;
                   VarRec : Boolean;
                   KeyNr : Integer;
                   LowKey,
                   HighKey : IsamKeyStr;
                   StartScreenRow,
                   NrOfRows : Integer;
                   var DesiredRow : Integer;     {!!.05}
                   var HorizOfs : Integer;       {!!.05}
                   var DatS;
                   var DatLen : Word;
                   var Ref : LongInt;
                   var KeyStr : IsamKeyStr;
                   var ExitKey : BKtype;
                   ProcSpecialTask : Pointer;
                   ProcBuildaRow : Pointer;
                   ProcDisplayaRow : Pointer) : Integer;
  var
    Result : Integer;
    Moved  : Integer;
    NewRow : RowRange;
    Action : BKtype;
    ChWord : Word;
    LowRef : LongInt;
    HighRef : LongInt;
    BrowScreen : BrowScreenType;
    bRowsToJump : Integer;
    NetUsed : Boolean;
    procedure CallBuildaRow(UserRoutine : Pointer;
                            var RR : RowRec;
                            KeyNr : Integer;
                            var DatS;
                            DatLen : Word);

      procedure CallUserRoutine(var RR : RowRec;
                                KeyNr : Integer;
                                var DatS;
                                DatLen : Word);
        {-Call UserRoutine with an action code}
      inline($FF/$5E/<UserRoutine);     {call far dword ptr [bp+<UserRoutine]}

    begin
      CallUserRoutine(RR, KeyNr, DatS, DatLen);
    end;


    procedure CallDisplayaRow(UserRoutine : Pointer;
                              var RR : RowRec;
                              KeyNr,
                              RowNr,
                              StartRow : Integer;
                              HighLight : Boolean;
                              var HorizOfs : Integer);

      procedure CallUserRoutine(var RR : RowRec;
                                KeyNr,
                                RowNr,
                                StartRow : Integer;
                                HighLight : Boolean;
                                var HorizOfs : Integer);
        {-Call UserRoutine with an action code}
      inline($FF/$5E/<UserRoutine);     {call far dword ptr [bp+<UserRoutine]}

    begin
      {$IFDEF UseMouse}
      if BrowseMouseEnabled then
        HideMouse;
      {$ENDIF}
      CallUserRoutine(RR, KeyNr, RowNr, StartRow, HighLight, HorizOfs);
      {$IFDEF UseMouse}
      if BrowseMouseEnabled then
        ShowMouse;
      {$ENDIF}
    end;

    procedure CallSpecialTask(UserRoutine : Pointer;
                              IFBPtr : IsamFileBlockPtr;
                              var DatS;
                              Ref : LongInt;
                              IKS : IsamKeyStr;
                              KeyNr : Integer;
                              var Command : BKtype;
                              var ExitCode : Integer;
                              DatLen : Word);

      procedure CallUserRoutine(IFBPtr : IsamFileBlockPtr;
                                var DatS;
                                Ref : LongInt;
                                IKS : IsamKeyStr;
                                KeyNr : Integer;
                                var Command : BKtype;
                                var ExitCode : Integer;
                                DatLen : Word);
        {-Call UserRoutine with an action code}
      inline($FF/$5E/<UserRoutine);     {call far dword ptr [bp+<UserRoutine]}

    begin
      CallUserRoutine(IFBPtr, DatS, Ref, IKS, KeyNr, Command, ExitCode, DatLen);
    end;

    procedure S_Key(IFBPtr : IsamFileBlockPtr;
                    Key : Integer;
                    var UserDatRef : LongInt;
                    var UserKey : IsamKeyStr);

    procedure CallUserRoutine(IFBPtr         : IsamFileBlockPtr;
                              KeyNr          : Integer;
                              var DatRef     : LongInt;
                              var KeyStr     : IsamKeyStr;
                              Net : Boolean);
      inline($FF/$1E/>SearchKeyPtr); {call dword ptr [>SearchKeyPtr]}

    begin
      CallUserRoutine(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
    end;

    procedure N_Key(IFBPtr : IsamFileBlockPtr;
                    Key : Integer;
                    var UserDatRef : LongInt;
                    var UserKey : IsamKeyStr);

    procedure CallUserRoutine(IFBPtr         : IsamFileBlockPtr;
                              KeyNr          : Integer;
                              var DatRef     : LongInt;
                              var KeyStr     : IsamKeyStr;
                              Net : Boolean);
      inline($FF/$1E/>NextKeyPtr); {call dword ptr [>NextKeyPtr]}

    begin
      CallUserRoutine(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
    end;

    procedure P_Key(IFBPtr : IsamFileBlockPtr;
                    Key : Integer;
                    var UserDatRef : LongInt;
                    var UserKey : IsamKeyStr);

    procedure CallUserRoutine(IFBPtr         : IsamFileBlockPtr;
                              KeyNr          : Integer;
                              var DatRef     : LongInt;
                              var KeyStr     : IsamKeyStr;
                              Net : Boolean);
      inline($FF/$1E/>PrevKeyPtr); {call dword ptr [>PrevKeyPtr]}

    begin
      CallUserRoutine(IFBPtr, Key, UserDatRef, UserKey, NetUsed);
    end;

    procedure F_Key(IFBPtr : IsamFileBlockPtr;
                    Key : Integer;
                    var UserDatRef : LongInt;
                    var UserKey : IsamKeyStr;
                    NFSD : Integer);

    procedure CallUserRoutine(IFBPtr         : IsamFileBlockPtr;
                              KeyNr          : Integer;
                              var DatRef     : LongInt;
                              var KeyStr     : IsamKeyStr;
                              NFSD           : Integer;
                              Net            : Boolean);
      inline($FF/$1E/>FindKeyAndRefPtr); {call dword ptr [>FindKeyAndRefPtr]}

    begin
      CallUserRoutine(IFBPtr, Key, UserDatRef, UserKey, NFSD, NetUsed);
    end;

    function LastRow : Integer;
    var
      I : Integer;
    begin
      I := MaxRows;
      while (I > 0) and (BrowScreen[I].Ref = LongInt(0)) do
        Dec(I);
      LastRow := I;
    end;

    procedure EmptyBrowScreen;
    var
      I : Integer;
    begin
      for I := 1 to MaxRows do
        with BrowScreen[I] do begin
          IKS := '';
          Ref := LongInt(0);
          Row := '';
        end;
    end;

    procedure PlaceAtEnd(RR : RowRec);
    var
      I : Integer;
    begin
      I := LastRow;
      if I < NrOfRows then
        BrowScreen[I] := RR
      else begin
        for I := 1 to NrOfRows-1 do
          BrowScreen[I] := BrowScreen[I+1];
        BrowScreen[NrOfRows] := RR;
      end;
    end;

    procedure PlaceAtTop(RR : RowRec);
    var
      I : Integer;
    begin
      for I := NrOfRows downto 2 do
        BrowScreen[I] := BrowScreen[I-1];
      BrowScreen[1] := RR;
    end;

    procedure ReadLock(On : Boolean);
    {!!.41 retry logic removed}
    var
      IsamSave : Integer;
    begin
      if UseReadLock then begin
        IsamSave := IsamError;
        if On then
          BTReadLockFileBlock(IFBPtr)
        else
          BTUnLockFileBlock(IFBPtr);
        if IsamSave <> 0 then begin
          IsamError := IsamSave;
          IsamOK := (IsamError = 0);
        end;
      end;
    end;

    function CompleteaPage : Boolean;
      {-Load data records and build display strings for current screen page}
    label
      ExitPoint;
    var
      I : Integer;
    begin
      CompleteaPage := False;
      ReadLock(True);
      if BTIsamErrorClass > 2 then
        Exit;
      for I := 1 to NrOfRows do begin
        with BrowScreen[I] do begin
          if Ref <> LongInt(0) then begin
            if (Ref <> LongInt(-1)) and ReadDataRecord then begin
              if VarRec then
                BTGetVariableRec(IFBPtr, Ref, DatS, DatLen)
              else
                BTGetRec(IFBPtr, Ref, DatS, False);
              if BTIsamErrorClass > 2 then
                goto ExitPoint;
            end;
            if BTIsamErrorClass = 2 then
              Ref := LongInt(-1);
            CallBuildaRow(ProcBuildaRow, BrowScreen[I], KeyNr, DatS, DatLen);
          end;
        end;
      end;
      CompleteaPage := True;
ExitPoint:
      ReadLock(False);
    end;

    procedure DisplayaPage;
    var
      I : Integer;
      {$IFDEF UseMouse}
      SaveMouseEn : Boolean;
      {$ENDIF}
    begin
      {$IFDEF UseMouse}
      SaveMouseEn := BrowseMouseEnabled;
      BrowseMouseEnabled := False;
      if SaveMouseEn then
        HideMouse;
      {$ENDIF}
      for I := 1 to NrOfRows do
        with BrowScreen[I] do
          CallDisplayaRow(ProcDisplayaRow, BrowScreen[I], KeyNr, I,
                          StartScreenRow, (I = CurRow), HorizOfs);
      {$IFDEF UseMouse}
      BrowseMouseEnabled := SaveMouseEn;
      if BrowseMouseEnabled then
        ShowMouse;
      {$ENDIF}
    end;

    function BuildFirstPage : Integer;
    label
      ExitPoint;
    var
      I : Integer;
      TRec : RowRec;
    begin
      BuildFirstPage := 2;
      ReadLock(True);
      if BTIsamErrorClass > 2 then
        Exit;
      with TRec do begin
        IKS := LowKey;
        S_Key(IFBPtr, KeyNr, Ref, IKS);
        if BTIsamErrorClass > 1 then begin
          goto ExitPoint;
        end;
      end;
      if (BTIsamErrorClass = 1) or (not KeyInBounds(TRec.IKS)) then begin
        BuildFirstPage := 1;
        goto ExitPoint;
      end;
      I := 1;
      while (I <= NrOfRows) and (KeyInBounds(TRec.IKS)) and
            (BTIsamErrorClass = 0) do begin
        BrowScreen[I] := TRec;
        with TRec do begin
          Inc(I);
          if I <= NrOfRows then begin
            N_Key(IFBPtr, KeyNr, Ref, IKS);
            if BTIsamErrorClass > 1 then
              goto ExitPoint;
          end;
        end;
      end;
      BuildFirstPage := 0;
ExitPoint:
      ReadLock(False);
    end;

    function BuildLastPage : Integer;
    label
      ExitPoint;
    var
      I : Integer;
      TRec : RowRec;
    begin
      BuildLastPage := 2;
      ReadLock(True);
      if BTIsamErrorClass > 2 then
        Exit;
      with TRec do begin
        IKS := HighKey;
        while Length(IKS) < MaxKeyLen do
          IKS := IKS+#255;
        Ref := $0FFFFFFF;
        F_Key(IFBPtr, KeyNr, Ref, IKS, -1);
        if BTIsamErrorClass > 1 then
          goto ExitPoint;
      end;
      if (BTIsamErrorClass = 1) or (not KeyInBounds(TRec.IKS)) then begin
        BuildLastPage := 1;
        goto ExitPoint;
      end;
      I := 1;
      while (I <= NrOfRows) and (KeyInBounds(TRec.IKS)) and (BTIsamErrorClass = 0) do begin
        PlaceAtTop(TRec);
        with TRec do begin
          Inc(I);
          if I <= NrOfRows then begin
            P_Key(IFBPtr, KeyNr, Ref, IKS);
            if BTIsamErrorClass > 1 then
              goto ExitPoint;
          end;
        end;
      end;
      BuildLastPage := 0;
ExitPoint:
      ReadLock(False);
    end;

    function BuildSpecPage(Desired : Integer) : Integer;
    var
      I, J  : Integer;
      TRec : RowRec;
    begin
      BuildSpecPage := 2;
      EmptyBrowScreen;
      TRec.Ref := Ref;
      TRec.IKS := KeyStr;
      with TRec do
        F_Key(IFBPtr, KeyNr, Ref, IKS, 1);
      case BTIsamErrorClass of
        0 : if not KeyInBounds(TRec.IKS) then begin           {!!.07}
              TRec.Ref := Ref;                                {!!.07}
              TRec.IKS := KeyStr;                             {!!.07}
              with TRec do                                    {!!.07}
                F_Key(IFBPtr, KeyNr, Ref, IKS, -1);           {!!.07}
                if (not IsamOK) or (not KeyInBounds(TRec.IKS)) then begin {!!.07} {!!.40}
                  BuildSpecPage := 1;                         {!!.07}
                  Exit;                                       {!!.07}
                end;                                          {!!.07}
            end;                                              {!!.07}
        1 : if IsamError = 10250 then begin
              TRec.Ref := Ref;
              TRec.IKS := KeyStr;
              with TRec do
                F_Key(IFBPtr, KeyNr, Ref, IKS, -1);
                if not IsamOK then begin                      {!!.06}
                  BuildSpecPage := 1;                         {!!.06}
                  Exit;                                       {!!.06}
                end;                                          {!!.06}
            end
            else begin
              with TRec do
                N_Key(IFBPtr, KeyNr, Ref, IKS);
              if not IsamOK then begin
                BuildSpecPage := 1;
                Exit;
              end;
            end;
        else Exit;
      end;
      I := Desired;
      while (I >= 1) and KeyInBounds(TRec.IKS) and (BTIsamErrorClass = 0) do begin
        BrowScreen[I] := TRec;
        Dec(I);
        with TRec do
          if I > 0 then begin
            P_Key(IFBPtr, KeyNr, Ref, IKS);
            if BTIsamErrorClass > 1 then
              Exit;
          end;
      end;
      if I > 0 then begin
        Move(BrowScreen[Succ(I)], BrowScreen[1],
             SizeOf(RowRec) * (Desired - I));
        for J := Desired downto Succ(Desired-I) do
          FillChar(BrowScreen[J], SizeOf(RowRec), 0);
        Dec(Desired, I);
      end;
      if Desired = 0 then
        Desired := 1;
      if Desired < NrOfRows then begin
        F_Key(IFBPtr, KeyNr, Ref, KeyStr, 1);
        if BTIsamErrorClass > 1 then
          Exit;
        if BTIsamErrorClass = 0 then begin
          with TRec do
            N_Key(IFBPtr, KeyNr, Ref, IKS);
          I := Succ(Desired);
          while (I <= NrOfRows) and KeyInBounds(TRec.IKS) and (BTIsamErrorClass = 0) do
          begin
            BrowScreen[I] := TRec;
            Inc(I);
            if I <= NrOfRows then
              with TRec do
                N_Key(IFBPtr, KeyNr, Ref, IKS);
          end;
        end
        else
          I := Desired;
        if Desired > I then
          CurRow := I
        else
          CurRow := Desired;
      end;
      BuildSpecPage := 0;
    end;

    function BuildPrevPage(Nr : Integer; var Moved : Integer) : Integer;
    label
      ExitPoint;
    var
      I : Integer;
      Res  : Integer;
      TRec : RowRec;
    begin
      BuildPrevPage := 2;
      ReadLock(True);
      if BTIsamErrorClass > 2 then
        Exit;

      Moved := 0;
      if BrowScreen[1].Ref = LongInt(0) then begin
        BuildPrevPage := 1;
        goto ExitPoint;
      end;
      TRec := BrowScreen[1];
      with TRec do begin
        F_Key(IFBPtr, KeyNr, Ref, IKS, 0);
        if BTIsamErrorClass > 1 then
          goto ExitPoint;
        if BTIsamErrorClass = 1 then
          F_Key(IFBPtr, KeyNr, Ref, IKS, -1)
        else
          P_Key(IFBPtr, KeyNr, Ref, IKS);
      end;
      case BTIsamErrorClass of
        0 : ;
        1 : begin
              KeyStr := BrowScreen[1].IKS;
              Ref    := BrowScreen[1].Ref;
              BuildPrevPage := BuildSpecPage(1);
              goto ExitPoint;
            end;
        else
          goto ExitPoint;
      end;
      I := 1;
      while (I <= Nr) and (KeyInBounds(TRec.IKS)) and (BTIsamErrorClass = 0) do begin
        PlaceAtTop(TRec);
        with TRec do begin
          Inc(I);
          if I <= Nr then begin
            P_Key(IFBPtr, KeyNr, Ref, IKS);
            if BTIsamErrorClass > 1 then
              goto ExitPoint;
          end;
        end;
      end;
      Moved := I-1;

      BuildPrevPage := 0;
ExitPoint:
      ReadLock(False);
    end;

    function BuildNextPage(Nr : Integer; var Moved : Integer) : Integer;
    label
      ExitPoint;
    var
      I : Integer;
      TRec : RowRec;
    begin
      BuildNextPage := 2;
      ReadLock(True);
      if BTIsamErrorClass > 2 then
        Exit;
      Moved := 0;
      I := LastRow;
      if I = 0 then begin
        BuildNextPage := 1;
        goto ExitPoint;
      end;
      TRec := BrowScreen[I];
      with TRec do begin
        F_Key(IFBPtr, KeyNr, Ref, IKS, 0);
        if BTIsamErrorClass > 1 then
          goto ExitPoint;
        N_Key(IFBPtr, KeyNr, Ref, IKS);
      end;                                                  {!!.06}
      case BTIsamErrorClass of
        0 : ;
        1 : begin
              KeyStr := BrowScreen[I].IKS;
              Ref    := BrowScreen[I].Ref;
              BuildNextPage := BuildSpecPage(I);
              goto ExitPoint;
            end;
        else
          goto ExitPoint;
      end;

      I := 1;
      while (I <= Nr) and (KeyInBounds(TRec.IKS)) and (BTIsamErrorClass = 0) do begin
        PlaceAtEnd(TRec);
        with TRec do begin
          Inc(I);
          if I <= Nr then begin
            N_Key(IFBPtr, KeyNr, Ref, IKS);
            if BTIsamErrorClass > 1 then
              goto ExitPoint;
          end;
        end;
      end;
      BuildNextPage := 0;
      Moved := I-1;
ExitPoint:
      ReadLock(False);
    end;

    function BuildThisPage(K : IsamKeyStr; R : LongInt) : Integer;
    label
      ExitPoint;
    var
      I : Integer;
      FreeRows : Integer;
      SaveUseReadLock : Boolean;
      TRec : RowRec;
    begin
      BuildThisPage := 2;
      ReadLock(True);
      if BTIsamErrorClass > 2 then
        Exit;
      SaveUseReadLock := UseReadLock;
      UseReadLock := False;
      with TRec do begin
        IKS := K;
        Ref := R;
        F_Key(IFBPtr, KeyNr, Ref, IKS, -1);
        if BTIsamErrorClass > 1 then
          goto ExitPoint;
        if (BTIsamErrorClass = 1) or (not KeyInBounds(IKS)) then begin
          N_Key(IFBPtr, KeyNr, Ref, IKS);
          if (BTIsamErrorClass = 1) or (not KeyInBounds(IKS)) then begin
            BuildThisPage := 1;
            goto ExitPoint;
          end;
        end;
      end;
      I := 1;
      while (I <= NrOfRows) and (KeyInBounds(TRec.IKS)) and (BTIsamErrorClass = 0) do begin
        BrowScreen[I] := TRec;
        with TRec do begin
          Inc(I);
          if I <= NrOfRows then begin
            N_Key(IFBPtr, KeyNr, Ref, IKS);
            if BTIsamErrorClass > 1 then
              goto ExitPoint;
          end;
        end;
      end;
      CurRow := 1;
      FreeRows := NrOfRows-LastRow;
      if FreeRows > 0 then begin
        if BuildPrevPage(FreeRows, Moved) > 1 then
          goto ExitPoint;
        CurRow := Moved+1;
      end;
      BuildThisPage := 0;
      UseReadLock := SaveUseReadLock;
ExitPoint:
      ReadLock(False);
    end;

    function ActSpecPage(Desired : Integer) : Integer;
    begin
      ActSpecPage := 2;
      ReadLock(True);
      if BTIsamErrorClass > 2 then                            {!!.06}
        Exit;
      Result := BuildSpecPage(Desired);
      ReadLock(False);
      if Result <> 0 then begin
        ActSpecPage := Result;
        Exit;
      end;
      if not CompleteaPage then begin
        ActSpecPage := 2;
        Exit;
      end;
      DisplayaPage;
      ActSpecPage := 0;
    end;

    function ActCurPage(Repos : Boolean) : Integer;
    var
      SaveRow : Integer;
    begin
      SaveRow := CurRow;
      EmptyBrowScreen;
      Result := BuildThisPage(KeyStr, Ref);
      if Result <> 0 then begin
        ActCurPage := Result;
        Exit;
      end;
      if Repos then
        if SaveRow > CurRow then begin
          Result := BuildPrevPage(SaveRow-CurRow, Moved);
          if Result <> 0 then begin
            ActCurPage := Result;
            Exit;
          end;
          inc(CurRow, Moved);
        end;
      if not CompleteaPage then begin
        ActCurPage := 2;
        Exit;
      end;
      DisplayaPage;
      ActCurPage := 0;
    end;

    function FindLowHighKeys(var LowKey : IsamKeyStr;
                             var LowRef : LongInt;
                             var HighKey : IsamKeyStr;
                             var HighRef : LongInt) : Integer;
      {-Return lowest and highest record numbers}
    var
      Result : Integer;
    begin
      S_Key(IFBPtr, KeyNr, LowRef, LowKey);
      if BTIsamErrorClass > 1 then begin
        FindLowHighKeys := 2;
        Exit;
      end;
      if (BTIsamErrorClass = 1) or not KeyInBounds(LowKey) then begin
        FindLowHighKeys := 1;
        Exit;
      end;
      while Length(HighKey) < MaxKeyLen do
        HighKey := HighKey+#255;
      HighRef := $0FFFFFFF;
      F_Key(IFBPtr, KeyNr, HighRef, HighKey, -1);
      if BTIsamErrorClass > 1 then begin
        FindLowHighKeys := 2;
        Exit;
      end;
      if (BTIsamErrorClass = 1) or not KeyInBounds(HighKey) then begin
        FindLowHighKeys := 1;
        Exit;
      end;
      FindLowHighKeys := 0;
    end;

    function FindLowHighRows(var LowRef, HighRef : LongInt) : Integer;
      {-Return lowest and highest record numbers}
    var
      Result : Integer;
      IKS1,IKS2 : IsamKeyStr;
    begin
      IKS1 := LowKey;
      IKS2 := HighKey;
      FindLowHighRows := FindLowHighKeys(IKS1, LowRef, IKS2, HighRef);
    end;

  {$IFDEF UseMouse}
  procedure DoMouseScale;
    {-Determine effect of LowKey and HighKey on scroll bar scaling}
  var
    LK, HK : IsamKeyStr;
  begin
    LK := LowKey;
    HK := HighKey;
    if FindLowHighKeys(LK, LowRef, HK, HighRef) <> 0 then
      Exit;
    if Length(LowKey) = 0 then
      MouseLowScale := 0
    else
      BTGetApprRelPos(IFBPtr, KeyNr, MouseLowScale, MouseScale, LK, LowRef);
    if HighKey = #255 then
      MouseHighScale := MouseScale
    else
      BTGetApprRelPos(IFBPtr, KeyNr, MouseHighScale, MouseScale, HK, HighRef);
  end;

  function MouseRescale(Position, RelScale : Word) : Word;
    {-Recompute scroll bar position taking into account any scaling}
  var
    Scale : Word;

  begin
    if AutoScaleMouse then begin
      if not NoNetMode then
        DoMouseScale;
      Scale := MouseHighScale - MouseLowScale;
      Position := Position - MouseLowScale;
    end
    else
      Scale := MouseScale;
    {calculate slider position}
    if Scale = 0 then                   {protect against div by zero}
      MouseRescale := 0
    else
      MouseRescale := Word((LongInt(Position) * RelScale) div Scale);
  end;

  function MouseRescale2(Position, RelScale : LongInt) : Word;
    {-Recompute scroll bar position taking into account any scaling}

  begin
    if RelScale = 0 then                {protect against div by zero}
      RelScale := 1;
    {calculate slider position}
    if AutoScaleMouse then begin
      if not NoNetMode then
        DoMouseScale;
      MouseRescale2 := Word(((LongInt(Position) *
                        (MouseHighScale - MouseLowScale)) div
                        RelScale) + MouseLowScale);
    end
    else
      MouseRescale2 := Word((LongInt(Position) * MouseScale) div RelScale);
  end;

  procedure EvaluateSliderPos(var RR : RowRec);
    {-Decide where the scroll bar slider should be postioned}
  var
    RelPos : Word;
    Ht     : Word;
  begin
    if not UseScrollBar then
      Exit;
    Ht := ScrollBarHt;
    with RR do
      BTGetApprRelPos(IFBPtr, KeyNr, RelPos, MouseScale, IKS, Ref);
    if not IsamOK then
      Exit;
    RelPos := MouseRescale(RelPos, ScrollBarHt);
    SliderPos := RelPos + ScrollBarTop;
    if SliderPos > ScrollBarBot then
      SliderPos := ScrollBarBot;
  end;

  procedure EvaluateMousePosition(var KeyStr : IsamKeyStr;
                                  var Ref    : LongInt;
                                  var Cmd : BKtype; var CurRow : RowRange);
    {-Evaluate mouse command based on position}
  var
    mX : Byte;                    {Mouse absolute X position}
    mY : Byte;                    {Mouse absolute Y position}
    mCol : Word;                  {Logical column where mouse is located}
    Posit : Word;
    procedure CallUserMouseRoutine(X, Y : Byte; var Cmd : BKtype);
      inline($FF/$1E/>UserMousePtr); {call dword ptr [>UserMousePtr]}

  begin
    if BrowseMouseEnabled then begin
      RepositionDesired := False;
      {Compute absolute mouse coordinates}
      mX := MouseXLo+MouseKeyWordX;
      mY := MouseYLo+MouseKeyWordY;
      if mX = ScrollBarCol then begin
        if mY = ScrollBarUp then
          {mouse cursor on top arrow}
          Cmd := MouseUpCmd[BrowseMousePage]
        else if mY = ScrollBarDn then
          {mouse cursor on bottom arrow}
          Cmd := MouseDownCmd[BrowseMousePage]
        else if UseScrollBar and
                (mY >= ScrollBarTop) and (mY <= ScrollBarBot) then begin
          {mouse cursor on scroll bar}
          SliderPos := mY;
          if mY = ScrollBarTop then
            Cmd := BKfirstRec
          else if mY = ScrollBarBot then
            Cmd := BKlastRec
          else begin
            Dec(mY, ScrollBarTop);
            Posit := MouseRescale2(mY, ScrollBarHt);
            BTGetApprKeyAndRef(IFBPtr, KeyNr,
                               Posit,
                               MouseScale,
                               KeyStr,
                               Ref);
            if not IsamOK then
              Exit;
            RepositionDesired := True;            {force repositioning}
            Cmd := BKRedraw;                      {force redraw}
          end;
        end
        else if UserMousePtr <> Nil then
          {outside browser's scroll bar, call user mouse routine}
          CallUserMouseRoutine(mX, mY, Cmd);
      end
      else if (mY >= BrowseYL) and (mY <= BrowseYH) and              {!!.06}
              (mX >= MouseX1) and (mX <= MouseX2) then begin         {!!.06}
        {in active browser region, convert to window-relative}
        Dec(mY, Pred(BrowseYL));
        if mY = CurRow then
          Cmd := BKenter
        else
          if BrowScreen[mY].Ref <> 0 then begin
            CurRow := mY;
            Cmd := BKplus;
          end;
      end
      else if UserMousePtr <> Nil then
        {outside browser, call user mouse routine}
        CallUserMouseRoutine(mX, mY, Cmd);
    end;
  end;
  {$ENDIF}

  {$IFDEF UseMouse}
  var
    SaveMouseEnabled  : Boolean;
  {$ENDIF}
  begin                           {BrowseI}
    if (DesiredRow < 0) or (DesiredRow > NrOfRows) then
      DesiredRow := 0;
    gHighKey := HighKey;                                    {!!.22}
    gLowKey  := LowKey;                                     {!!.22}
    if not KeyInBounds(KeyStr) then begin                   {!!.06}
      if KeyStr < LowKey then
        KeyStr := LowKey
      else
        KeyStr := HighKey;
      DesiredRow := 0;                                      {!!.06}
    end;                                                    {!!.06}

    if NrOfRows > MaxRows then
      NrOfRows := MaxRows
    else if NrOfRows < MinRows then
      NrOfRows := MinRows;
    if (RowsToJump = 0) or (RowsToJump > NrOfRows) then
      bRowsToJump := NrOfRows div 2
    else
      bRowsToJump := RowsToJump;

    if NoNetMode then begin
      {Determine first and last acceptable records}
      Result := FindLowHighRows(LowRef, HighRef);
      if Result <> 0 then begin
        BrowseI := Result;
        Exit;
      end;
      {$IFDEF UseMouse}
      if BrowseMouseEnabled and UseScrollBar then begin
        DoMouseScale;
      end;
      {$ENDIF}
    end;
    {$IFDEF UseMouse}
    SaveMouseEnabled := BrowseMouseEnabled;
    BrowseMouseEnabled := False;
    {$ENDIF}

    if DesiredRow = 0 then begin
      CurRow := 1;
      Result := ActCurPage(False);
      if Result <> 0 then begin
        BrowseI := Result;
        {$IFDEF UseMouse}                           {!!.22}
        BrowseMouseEnabled := SaveMouseEnabled;     {!!.22}
        {$ENDIF}                                    {!!.22}
        Exit;
      end;
    end
    else begin
      CurRow := DesiredRow;
      Result := ActSpecPage(DesiredRow);
      if Result <> 0 then begin
        BrowseI := Result;
        {$IFDEF UseMouse}                           {!!.22}
        BrowseMouseEnabled := SaveMouseEnabled;     {!!.22}
        {$ENDIF}                                    {!!.22}
        Exit;
      end;
    end;
    {$IFDEF UseMouse}
    PrevSlid := 0;
    BrowseMouseEnabled := SaveMouseEnabled;
    if BrowseMouseEnabled then begin
      if UseScrollBar then begin
        EvaluateSliderPos(BrowScreen[CurRow]);
        {Update slider in frame}
        FastWrite(ScrollMark, SliderPos, ScrollBarCol, SliderAttr); {!!.41}
        PrevSlid := SliderPos;
      end;
      ShowMouse;
    end;
    {$ENDIF}

    CallDisplayaRow(ProcDisplayaRow, BrowScreen[CurRow], KeyNr, CurRow,
                    StartScreenRow, False, HorizOfs);

    BrowseI := 2;
    Action := ExitKey;
    repeat
      DesiredRow := CurRow;

      {$IFDEF UseMouse}
      if Action = BKprobe then begin
        NewRow := CurRow;

        EvaluateMousePosition(KeyStr, Ref, Action, NewRow);
        if Action = BKprobe then
          Action := BKnone;
        if NewRow <> CurRow then begin
          CallDisplayaRow(ProcDisplayaRow,
                          BrowScreen[CurRow], KeyNr, CurRow,
                          StartScreenRow, False, HorizOfs);
          CurRow := NewRow;
        end;
      end;
      {$ENDIF}
      case Action of
        BKRowEnd :
          begin
            HorizOfs := MaxInt;
            Action := BKredraw;
            Ref := BrowScreen[CurRow].Ref;
            KeyStr := BrowScreen[CurRow].IKS;
          end;
        BKRowBegin :
          begin
            HorizOfs := 0;
            Action := BKredraw;
            Ref := BrowScreen[CurRow].Ref;
            KeyStr := BrowScreen[CurRow].IKS;
          end;
      end;

      case Action of
        BKnone,
        BKchar : ;
        BKfirstRec :
          {!!}
          if not NoNetMode or (BrowScreen[1].Ref <> LowRef) then begin
            CurRow := 1;
            EmptyBrowScreen;
            Result := BuildFirstPage;
            if Result <> 0 then begin
              BrowseI := Result;
              Exit;
            end;
            if not CompleteaPage then
              Exit;
            DisplayaPage;
          end
          else begin
            CallDisplayaRow(ProcDisplayaRow, BrowScreen[CurRow], KeyNr, CurRow,
                            StartScreenRow, False, HorizOfs);
            CurRow := 1;
          end;

        BKlastRec :
          {!!}
          if not NoNetMode or (BrowScreen[LastRow].Ref <> HighRef) then begin
            CurRow := LastRow;
            EmptyBrowScreen;
            Result := BuildLastPage;
            if Result <> 0 then begin
              BrowseI := Result;
              Exit;
            end;
            if not CompleteaPage then
              Exit;
            DisplayaPage;
          end
          else begin
            CallDisplayaRow(ProcDisplayaRow, BrowScreen[CurRow], KeyNr, CurRow,
                            StartScreenRow, False, HorizOfs);
            CurRow := LastRow;
          end;

        BKpageUp :
          {!!}
          if not NoNetMode or (BrowScreen[1].Ref <> LowRef) then begin
            Result := BuildPrevPage(NrOfRows, Moved);
            if Result <> 0 then begin
              BrowseI := Result;
              Exit;
            end;
            if Moved = 0 then {!!.40}
              CurRow := 1;    {!!.40}
            if not CompleteaPage then
              Exit;
            DisplayaPage;
          end
          else begin
            CallDisplayaRow(ProcDisplayaRow, BrowScreen[CurRow], KeyNr, CurRow,
                            StartScreenRow, False, HorizOfs);
            CurRow := 1;
          end;

        BKpageDown :
          {!!}
          if not NoNetMode or (BrowScreen[LastRow].Ref <> HighRef) then begin
            Result := BuildNextPage(NrOfRows, Moved);
            if Result <> 0 then begin
              BrowseI := Result;
              Exit;
            end;
            if Moved = 0 then
              CurRow := LastRow;
            if not CompleteaPage then
              Exit;
            DisplayaPage;
          end
          else begin
            CallDisplayaRow(ProcDisplayaRow, BrowScreen[CurRow], KeyNr, CurRow,
                            StartScreenRow, False, HorizOfs);
            CurRow := LastRow;
          end;

        BKup :
          {!!}
          if CurRow = 1 then begin
            if not NoNetMode or (BrowScreen[CurRow].Ref <> LowRef) then begin
              Result := BuildPrevPage(bRowsToJump, Moved);
              if Result <> 0 then begin
                BrowseI := Result;
                Exit;
              end;
              if Moved <> 0 then
                CurRow := Moved;
              if not CompleteaPage then
                Exit;
              DisplayaPage;
            end;
          end
          else begin
            CallDisplayaRow(ProcDisplayaRow, BrowScreen[CurRow], KeyNr, CurRow,
                            StartScreenRow, False, HorizOfs);
            Dec(CurRow);
          end;

        BKdown :
          {!!}
          if CurRow = LastRow then begin
            if not NoNetMode or (BrowScreen[CurRow].Ref <> HighRef) then begin
              Result := BuildNextPage(bRowsToJump, Moved);
              if Result <> 0 then begin
                BrowseI := Result;
                Exit;
              end;
              if Moved <> 0 then
                CurRow := LastRow+1-Moved;
              if not CompleteaPage then
                Exit;
              DisplayaPage;
            end;
          end
          else begin
            CallDisplayaRow(ProcDisplayaRow, BrowScreen[CurRow], KeyNr, CurRow,
                            StartScreenRow, False, HorizOfs);
            Inc(CurRow);
          end;

        BKleft :
          if HorizOfs > 0 then begin
            Dec(HorizOfs);
            DisplayaPage;
          end;

        BKright :
          begin
            HorizOfs := (HorizOfs+1) mod 32000;
            DisplayaPage;
          end;

        BKplus :
          begin
            Ref := BrowScreen[CurRow].Ref;
            KeyStr := BrowScreen[CurRow].IKS;
            Result := ActCurPage(True);
            if Result <> 0 then begin
              BrowseI := Result;
              Exit;
            end;
          end;

        BKhelp :
          if BrowseHelpPtr <> nil then
            HelpRoutine(HelpForBrowse, IFBptr, BrowseHelpIndex);

        BKquit :
          begin
            ExitKey := BKquit;
            BrowseI := 0;
            Exit;
          end;

        BKenter, BKuser0..BKuser9 :
          begin
            {!!.41 this block reorganized}
            ExitKey := Action;
            Ref := BrowScreen[CurRow].Ref;
            KeyStr := BrowScreen[CurRow].IKS;
            if Ref = LongInt(-1) then begin
              {Attempting to select a known-locked row}
              IsamError := 10399;
              IsamOK := False;
            end else begin
              {Row wasn't locked last time browser page was built}
              if VarRec then
                BTGetVariableRec(IFBPtr, Ref, DatS, DatLen)
              else
                BTGetRec(IFBPtr, Ref, DatS, False);
              if IsamOK then
                if LongInt(DatS) <> 0 then begin
                  {Record was deleted by another station}
                  IsamError := 8102;
                  IsamOK := False;
                end else
                  {Everything is ok}
                  BrowseI := 0;
            end;
            Exit;
          end;
      end;

      if Action <> BKRedraw then
        CallDisplayaRow(ProcDisplayaRow, BrowScreen[CurRow], KeyNr, CurRow,
                        StartScreenRow, True, HorizOfs);

      case Action of
        BKRedraw         :
          begin
            if RepositionDesired then
              Result := ActCurPage(True)
            else
              Result := ActSpecPage(CurRow);
            if Result <> 0 then begin
              BrowseI := Result;
              Exit;
            end;
            Action := BKNone;
          end;
        BKtask0..BKtask9 :
          if ProcSpecialTask <> nil then begin
            ExitKey := Action;
            Ref := BrowScreen[CurRow].Ref;
            if Ref <> LongInt(-1) then begin
              if VarRec then
                BTGetVariableRec(IFBPtr, Ref, DatS, DatLen)
              else
                BTGetRec(IFBPtr, Ref, DatS, False);
              if BTIsamErrorClass > 2 then
                Exit;

              if (LongInt(DatS) <> LongInt(0)) or (BTIsamErrorClass = 2) then begin
                Ref := BrowScreen[CurRow].Ref;
                KeyStr := BrowScreen[CurRow].IKS;
                CallDisplayaRow(ProcDisplayaRow, BrowScreen[CurRow], KeyNr, CurRow,
                            StartScreenRow, False, HorizOfs);
                Result := ActCurPage(True);
                if Result <> 0 then begin
                  BrowseI := Result;
                  Exit;
                end;
                CallDisplayaRow(ProcDisplayaRow, BrowScreen[CurRow], KeyNr, CurRow,
                            StartScreenRow, True, HorizOfs);
                Action := BKnone;
              end
              else begin
                KeyStr := BrowScreen[CurRow].IKS;
                CallSpecialTask(ProcSpecialTask, IFBPtr, DatS, Ref, KeyStr, KeyNr,
                                Action, Result, DatLen);
                if Result <> 0 then begin
                  BrowseI := Result;
                  Exit;
                end;
                Result := ActCurPage(True);
                if Result <> 0 then begin
                  BrowseI := Result;
                  Exit;
                end;
              end;
            end
            else
              Action := BKnone;
          end;
        else begin
          {$IFDEF UseMouse}
          if BrowseMouseEnabled and UseScrollBar then begin
            EvaluateSliderPos(BrowScreen[CurRow]);
            {Update slider in frame}
            if SliderPos <> PrevSlid then begin
              HideMouse;
              if PrevSlid <> 0 then
                FastWrite(ScrollVertChar, PrevSlid, ScrollBarCol, ScrollBarAttr);
              FastWrite(ScrollMark, SliderPos, ScrollBarCol, SliderAttr); {!!.41}
              PrevSlid := SliderPos;
              ShowMouse;
            end;
          end;
          {$ENDIF}
          if NeedRefresh(IFBPtr, KeyNr) then
            Action := BKPlus
          else
            Action := GetCommand(BrowseKeySet, BrowseKeyPtr, ChWord);
        end;
      end;

      (* !! removed by KRK, 9/23/88
      CallDisplayaRow(ProcDisplayaRow, BrowScreen[CurRow], KeyNr, CurRow,
                  StartScreenRow, False, HorizOfs);
      *)
    until False;
  end;
