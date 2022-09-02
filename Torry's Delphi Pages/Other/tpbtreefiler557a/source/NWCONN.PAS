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
  {$B+,F-,I-,R-,S-,V-}
  {$IFDEF CanAllowOverlays}
    {$F+,O+,A-}
  {$ENDIF}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-,P-}
  {$ENDIF}
  {all other compiler options are 'don't care'}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit NWConn;
  {-Unit to provide connection related NetWare functionality.}

interface

uses
  {$IFDEF DPMIOrWnd}
  DPMI,
  {$ENDIF}
  NWBase,
  NWBind;

const
  {Max number of networks in TnwNetworkList}
  MaxNetworks = 8;

type
  {A NetWare server name}
  TnwServerName = TnwObjectStr;

  {Function type for enumerating servers, return true to continue
   enumerating servers.}
  TnwEnumServerFunc = function (Name : TnwServerName;
                                Server : word;
                                var ExtraData) : boolean;

  {Assorted server information}
  TnwServerInfo = record
    ServerName  : TnwServerName;   {..name}
    NetWareVer  : byte;            {..major version number}
    NetWareSub  : byte;            {..minor version number}
    MaxConns    : word;            {..max connections can support}
    UsedConns   : word;            {..current connections}
    MaxVols     : word;            {..max volumes}
    Revision    : byte;            {..version revision level}
    SFTLevel    : byte;            {..SFT level}
    TTSLevel    : byte;            {..TTS level}
    PeakConn    : word;            {..max connections ever used}
    AccountVer  : byte;            {..accounting version number}
    VAPVer      : byte;            {..VAP version number}
    QueueVer    : byte;            {..QMS version number}
    PrintServVer: byte;            {..Print server version number}
    VirtualVer  : byte;            {..Virtual console version number}
    SecurityVer : byte;            {..Security Restriction version number}
    BridgeVer   : byte;            {..Bridge Support version number}
    Reserved    : array [1..60] of byte;
  end;

  {Information about a connection}
  TnwConnInfo = record
    ObjectID   : nwLong;        {..the logged in object's ID}
    ObjectType : word;          {..the logged in object's type}
    ObjectName : TnwObjectStr;  {..the name of the object}
    LoginDate  : TnwDate;       {..the time/date the object logged on}
  end;

  {A list of connection numbers}
  PnwConnList = ^TnwConnList;
  TnwConnList = record
    Count : word;
    List  : array [0..126] of word; {in practice: variably sized}
  end;

  {A list of network numbers}
  TnwNetworkList = record
    Count : word;                             {num elements in List}
    List  : array [1..MaxNetworks] of nwLong; {network numbers}
  end;

function nwDefaultServer : TnwServer;
  {-Return the default server handle.}

procedure nwEnumServers(EnumFunc : TnwEnumServerFunc; var ExtraData);
  {-Enumerate the attached servers}

function nwIsLoggedIn(Server : TnwServer) : boolean;
  {-Return true if this workstation is logged in to the server.}

function nwServerFromName(Name : TnwServerName) : TnwServer;
  {-Return the handle for a given server name.}

function nwServerVersion(Server : TnwServer) : word;
  {-Return the server version (hibyte=major, lobyte=minor).}

function nwGetServerInfo(Server : TnwServer; var SI : TnwServerInfo) : TnwErrorCode;
  {-Return information about a server, including its name.}

function nwGetServerTime(Server : TnwServer; var DT : TnwDate) : TnwErrorCode;
  {-Return the server's date and time.}

function nwSetServerTime(Server : TnwServer; var DT : TnwDate) : TnwErrorCode;
  {-Set the server's date and time.}

function nwGetConnNo(Server : TnwServer) : word;
  {-Return the connection number of the current workstation.}

function nwGetConnInfo(Server : TnwServer; ConnNo : word;
                       var CI : TnwConnInfo) : TnwErrorCode;
  {-Return info about a connection number.}

function nwGetConnNoForUser(Server : TnwServer;
                            UserName : TnwObjectStr;
                            var ConnNo : word) : TnwErrorCode;
  {-Return connect number greater than ConnNo for a user name.}

function nwGetInternetAddress(Server : TnwServer;
                              ConnNo : word;
                              var IA : IPXAddress) : TnwErrorCode;
  {-Return network address for a connection.}

procedure nwGetNetworkList(Server : TnwServer;
                           var NetList : TnwNetworkList);
  {-Return a list of network numbers.}

implementation

type
  OS = record O, S : word; end; {to split pointer into sel/seg & ofs}
  LH = record L, H : word; end; {to split nwLong into hi/lo words}

type
  TServerNameExtraData = record {for nwServerFromName}
    Handle : TnwServer;
    Name   : TnwServerName;
  end;

function vlmCONNGetEntryField(Server : TnwServer; FieldNum : byte) : word;
  {-Return the specified connection table entry field. VLM ONLY.
    Warning: this routine will trash memory for FieldNum = 17, as this
             field requires a buffer to be passed. No check is made as
             this routine in not (yet) called for that field.}
  var
    Regs : TnwRegisters;
  begin
    {API: _CONN Get Entry Field}
    nwInitRegs(Regs);
    Regs.BH := FieldNum;
    Regs.CX := Server;
    if (vlmCall(vlmConn, $07, Regs) = 0) then
         vlmCONNGetEntryField := Regs.DX
    else vlmCONNGetEntryField := 0;
  end;

procedure vlmEnumServers(EnumFunc : TnwEnumServerFunc; var ExtraData);
  {-Enumerate servers under VLM.}
  type
    PName = ^TnwServerName;
  var
    LastHandle : word;
    vlmResult  : word;
    NextName   : TnwServerName;
    Regs       : TnwRegisters;
    StillEnumerating : boolean;
  begin
    StillEnumerating := true;
    LastHandle := 0;
    while StillEnumerating do
      begin
        {API: _CONN Lookup Handle}
        {get next server handle}
        nwInitRegs(Regs);
        Regs.BH := $11;
        Regs.CX := LastHandle;
        vlmResult := vlmCall(vlmConn, $0A, Regs);
        if (vlmResult <> 0) then
          StillEnumerating := false
        else
          begin
            LastHandle := Regs.CX;
            {API: _CONN Name Lookup}
            {get name corresponding to handle}
            nwInitRegs(Regs);
            with Regs do
              begin
                CX := LastHandle;
                {ES := OS(nwGlobalBufRealPtr).S;}        {!!.51}
                {DI := OS(nwGlobalBufRealPtr).O;}        {!!.51}
                ES := OS(nwGlobalBufVLM).S;              {!!.51}
                DI := OS(nwGlobalBufVLM).O;              {!!.51}
              end;
            vlmResult := vlmCall(vlmConn, $0D, Regs);
            if (vlmResult <> 0) then
              StillEnumerating := false
            else
              begin
                {convert to pascal style}
                nwCvtAsciizToStr(nwGlobalBuf^, pred(sizeof(TnwServerName)));
                {call user routine}
                StillEnumerating :=
                   EnumFunc(PName(nwGlobalBuf)^, LastHandle, ExtraData);
              end;
          end;
      end;
  end;

procedure NETXEnumServers(EnumFunc : TnwEnumServerFunc; var ExtraData);
  {-Enumerate servers under NETX.}
  var
    NameTable  : PNETXServerNameTable;
    LastHandle : word;
    Index      : integer;
    NextName   : TnwServerName;
  begin
    NameTable := nwNETXGetServerNameTable;
    if (NameTable <> nil) then
      begin
        LastHandle := 0;
        while (LastHandle < 8) do
          begin
            inc(LastHandle);
            Move(NameTable^[LastHandle], NextName, sizeof(NextName));
            {If NextName[0] = #0 then both the ASCIIZ and the Pascal
             string are empty, so NextName = ''. If not then both the
             ASCIIZ and Pascal strings are not empty.}
            if (NextName <> '') then
              begin
                {convert to Pascal style}
                nwCvtAsciizToStr(NextName, pred(sizeof(TnwServerName)));
                {call user routine}
                if not EnumFunc(NextName, LastHandle, ExtraData) then
                  LastHandle := 8; {to exit from loop}
              end;
          end;
      end;
  end;

procedure nwEnumServers(EnumFunc : TnwEnumServerFunc; var ExtraData);
  begin
    case nwShellType of
      nsNETX : NETXEnumServers(EnumFunc, ExtraData);
      nsVLM  : VLMEnumServers(EnumFunc, ExtraData);
    end;{case}
  end;

function vlmGetPrimaryServerHandle : TnwServer;
  {-Gets the handle of the primary server. VLM ONLY.}
  var
    Regs : TnwRegisters;
  begin
    {API: _GEN Specific}
    nwInitRegs(Regs);
    Regs.BX := 1;
    if (vlmCall(vlmGeneral, $06, Regs) <> 0) then
         vlmGetPrimaryServerHandle := 0
    else vlmGetPrimaryServerHandle := Regs.CX
  end;

function NETXGetDefServer : TnwServer; assembler;
  {-Gets the handle of the pref/default server. NETX ONLY.}
  asm
    {API: Get Preferred/Default/Primary Connection ID}
    mov ax, $F001  {get preferred connection id}
    int $21
    or al, al
    jnz @@Exit
    mov ax, $F002  {get default connection id}
    int $21
    or al, al
    jnz @@Exit
    mov ax, $F005  {get primary connection id}
    int $21
  @@Exit:
    xor ah, ah
  end;

function nwDefaultServer : TnwServer;
  begin
    case nwShellType of
      nsNETX : nwDefaultServer := NETXGetDefServer;
      nsVLM  : nwDefaultServer := vlmGetPrimaryServerHandle;
    else
      nwDefaultServer := 0; {if no shell, the server handle is zero}
    end;{case}
  end;

function NETXGetServerVer(Server : TnwServer) : word;
  {-Return the version of the server. NETX ONLY}
  var
    SI : TnwServerInfo;
  begin
    if (nwGetServerInfo(Server, SI) = 0) then
         NETXGetServerVer := (word(SI.NetWareVer) * $100) + SI.NetWareSub
    else NETXGetServerVer := 0;
  end;

function nwServerVersion(Server : TnwServer) : word;
  begin
    case nwShellType of
      nsNETX : nwServerVersion := NETXGetServerVer(Server);
      nsVLM  : nwServerVersion := Swap(vlmCONNGetEntryField(Server, $08));
    else
      nwServerVersion := 0; {if no shell, the server version is zero}
    end;{case}
  end;

function nwGetServerInfo(Server : TnwServer; var SI : TnwServerInfo) : TnwErrorCode;
  var
    Request : record
      Len  : word;
      Func : byte;
    end;
    Status : word;
  begin
    {API: Get File Server Information}
    Request.Len := 1;
    Request.Func := $11;
    Status := nwServerCall(Server, $17, sizeof(Request), Request,
                                        sizeof(SI), SI);
    if (Status = 0) then
      with SI do
        begin
          nwCvtAsciizToStr(ServerName, pred(sizeof(TnwServerName)));
          MaxConns := Swap(MaxConns);
          UsedConns := Swap(UsedConns);
          MaxVols := Swap(MaxVols);
          PeakConn := Swap(PeakConn);
        end
    else
      FillChar(SI, sizeof(SI), 0);
    nwGetServerInfo := Status;
  end;

function NETXGetConnNo(Server : TnwServer) : word;
  {-Return our connection number. NETX ONLY}
  begin
    {API: Get Connection Number}
    nwNETXPushServer(Server);
    asm
      mov ax, $DC00
      int $21
      cmp cl, 'X'               {if CL = 'X' then under 1000-user NetWare}
      je @@Exit                 {  and AX = connection number}
      xor ah, ah
    @@Exit:
      mov @Result, ax
    end;
    nwNETXPopServer;
  end;

function nwGetConnNo(Server : TnwServer) : word;
  begin
    case nwShellType of
      nsNETX : nwGetConnNo := NETXGetConnNo(Server);
      nsVLM  : nwGetConnNo := vlmCONNGetEntryField(Server, $0D);
    else
      nwGetConnNo := nwErrShell;
    end;{case}
  end;

function nwGetConnInfo(Server : TnwServer; ConnNo : word;
                       var CI : TnwConnInfo) : TnwErrorCode;
  var
    Request : record
      Len  : word;
      Func : byte;
      case byte of
        1 : (ConnOld : byte);
        2 : (ConnNew : nwLong);
    end;
    Reply : record
      ObjID : nwLong;
      ObjTp : word;
      ObjNm : TnwObjectStr;
      LogTm : array [0..6] of byte;
      Fill  : byte;
    end;
    Status : word;
  begin
    {API: Get Connection Information (& old)}
    with Request do
      if (nwServerVersion(Server) >= ServerVersion311) then
        begin
          Len  := sizeof(Request) - 2;
          Func := $1C;
          ConnNew := nwLong(ConnNo);
        end
      else
        begin
          Len  := sizeof(Request) - 2 + (sizeof(byte) - sizeof(nwLong));
          Func := $16;
          ConnOld := lo(ConnNo);
        end;
    Status := nwServerCall(Server, $17, Request.Len+2, Request,
                                       sizeof(Reply), Reply);
    FillChar(CI, sizeof(TnwConnInfo), 0);
    if (Status = 0) then
      with CI, Reply do
        begin
          ObjectID   := ObjID;
          ObjectType := Swap(ObjTp);
          nwCvtAsciizToStr(ObjNm, pred(sizeof(TnwObjectStr)));
          ObjectName := ObjNm;
          Move(LogTm[1], CI.LoginDate.Month, sizeof(TnwDate)-2);
          if (LogTm[0] >= 80) then
               CI.LoginDate.Year := 1900 + LogTm[0]
          else CI.LoginDate.Year := 2000 + LogTm[0];
        end;
    nwGetConnInfo := Status;
  end;

function nwGetConnNoForUser(Server : TnwServer;
                            UserName : TnwObjectStr;
                            var ConnNo : word) : TnwErrorCode;
  var
    Request : record
      Len   : word;
      Func  : byte;
      case boolean of
        true : (ConnNew  : nwLong;
                ObjTpNew : word;
                ObjNmNew : TnwObjectStr);
        false: (ObjTpOld : word;
                ObjNmOld : TnwObjectStr);
    end;
    Reply : record {256 bytes}
      ListLen : byte;
      case boolean of
        true : (ListNew : array [0..62] of nwLong);
        false: (ListOld : array [0..254] of byte);
    end;
    Status   : word;
    i        : integer;
    NewCall  : boolean;
    StillSearching : boolean;
  begin
    {API: Get Object Connection List (& old)}
    with Request do
      if (nwServerVersion(Server) >= ServerVersion311) then
        begin
          NewCall := true;
          Len  := 8 + length(UserName);
          Func := $1B;
          ConnNew := nwLong(ConnNo);
          ObjTpNew := Swap($0001);
          ObjNmNew := UserName;
          nwUpperStr(ObjNmNew);
        end
      else
        begin
          NewCall := false;
          Len  := 6 + length(UserName);
          Func := $15;
          ObjTpOld := Swap($0001);
          ObjNmOld := UserName;
          nwUpperStr(ObjNmOld);
        end;
    Status := nwServerCall(Server, $17, Request.Len+2, Request,
                                        sizeof(Reply), Reply);
    if (Status = 0) then
      if (Reply.ListLen = 0) then
        Status := nwErrNoMoreConns
      else
        if NewCall then
          ConnNo := Reply.ListNew[0]
        else
          begin
            i := 0;
            StillSearching := true;
            while StillSearching and (i < Reply.ListLen) do
              begin
                if (Reply.ListOld[i] > ConnNo) then
                  begin
                    StillSearching := false;
                    ConnNo := Reply.ListOld[i];
                  end;
                inc(i);
              end;
            if StillSearching then
              Status := nwErrNoMoreConns;
          end;
    nwGetConnNoForUser := Status;
  end;

function nwGetInternetAddress(Server : TnwServer;
                              ConnNo : word;
                              var IA : IPXAddress) : TnwErrorCode;
  var
    Request : record
      Len   : word;
      Func  : byte;
      case boolean of
        true : (ConnNew : nwLong);
        false: (ConnOld : byte);
    end;
    Reply : record
      IPXAdd : IPXAddress;
      CType  : byte;
    end;
    ReplyLen : word;
    Status   : word;
  begin
    {API: Get Internet Address (& old)}
    with Request do
      if (nwServerVersion(Server) >= ServerVersion311) then
        begin
          Len  := 5;
          Func := $1A;
          ConnNew := nwLong(ConnNo);
          ReplyLen := 13;
        end
      else
        begin
          Len  := 2;
          Func := $13;
          ConnOld := ConnNo;
          ReplyLen := 12;
        end;
    Status := nwServerCall(Server, $17, Request.Len+2, Request,
                                        ReplyLen, Reply);
    if (Status = 0) then
      begin
        Move(Reply.IPXAdd, IA, sizeof(IPXAddress));
      end
    else
      FillChar(IA, sizeof(IPXAddress), 0);
    nwGetInternetAddress := Status;
  end;

function nwGetServerTime(Server : TnwServer; var DT : TnwDate) : TnwErrorCode;
  var
    Dummy  : word;
    Status : word;
    Reply  : array [0..6] of byte;
  begin
    {API: Get File Server Date and Time}
    Status := nwServerCall(Server, $14, 0, Dummy,
                                            sizeof(Reply), Reply);
    if (Status = 0) then
      begin
        Move(Reply[1], DT.Month, sizeof(Reply)-1);
        if (Reply[0] >= 80) then
             DT.Year := 1900 + Reply[0]
        else DT.Year := 2000 + Reply[0];
      end
    else
      FillChar(DT, sizeof(DT), 0);
    nwGetServerTime := Status;
  end;

function nwSetServerTime(Server : TnwServer; var DT : TnwDate) : TnwErrorCode;
  var
    Request : record
      Len : word;
      Func: byte;
      Yr, Mo, Da, Ho, Mi, Se : byte;
    end;
    Dummy  : word;
    Status : word;
  begin
    {API: Set File Server Date and Time}
    {do some coarse data checking}
    nwSetServerTime := nwErrBadData;
    with DT do
      begin
        if (Second > 59) then Exit;
        if (Minute > 59) then Exit;
        if (Hour > 23) then Exit;
        if (Day = 0) or (Day > 31) then Exit;
        if (Month = 0) or (Month > 12) then Exit;
        if (Year < 1980) or (Year > 2079) then Exit;
      end;
    {do the date/time setting}
    with Request do
      begin
        Len := sizeof(Request) - 2;
        Func := $CA;
        Move(DT.Month, Mo, 5);
        if (DT.Year <= 1999) then
          Yr := DT.Year - 1900
        else
          Yr := DT.Year - 2000;
      end;
    nwSetServerTime := nwServerCall(Server, $17, sizeof(Request), Request,
                                                 0, Dummy);
  end;

function FindServerHandle(Name : TnwServerName; Server : word;
                          var ExtraData) : boolean; far;
  var
    ED : TServerNameExtraData absolute ExtraData;
  begin
    if (Name = ED.Name) then
      begin
        ED.Handle := Server;
        FindServerHandle := false;
      end
    else
      FindServerHandle := true;
  end;

function nwServerFromName(Name : TnwServerName) : TnwServer;
  var
    ExtraData : TServerNameExtraData;
  begin
    FillChar(ExtraData, sizeof(ExtraData), 0);
    ExtraData.Name := Name;
    nwUpperStr(ExtraData.Name);
    nwEnumServers(FindServerHandle, ExtraData);
    nwServerFromName := ExtraData.Handle;
  end;

procedure nwGetNetworkList(Server : TnwServer;
                           var NetList : TnwNetworkList);
  var
    NetInx : word;
    ConnNo : word;
    CAddr  : IPXAddress;
    SI     : TnwServerInfo;
    StillLooking : boolean;
    FullUp       : boolean;
  begin
    {initialize the network list}
    FillChar(NetList, sizeof(NetList), 0);
    {get the server information for the number of connections}
    if (nwGetServerInfo(Server, SI) <> 0) then
      Exit;

    {read all the internet addresses until either we've filled the
     network list or we run out of connections}
    FullUp := false;
    ConnNo := 0;
    while (not FullUp) and (ConnNo < SI.PeakConn) do
      begin
        inc(ConnNo);
        if (nwGetInternetAddress(Server, ConnNo, CAddr) = 0) then
          with NetList do
            begin
              {try to find this new network number in the list}
              StillLooking := true;
              NetInx := 0;
              while StillLooking and (NetInx < Count) do
                begin
                  inc(NetInx);
                  if (List[NetInx] = CAddr.Network) then
                    StillLooking := false;
                end;
              {if we get here and we were still looking then the
               new network number wasn't in the list so add it}
              if StillLooking then
                if (Count < MaxNetworks) then
                  begin
                    inc(Count);
                    List[Count] := CAddr.NetWork;
                  end
                else
                  FullUp := true;
            end;
      end;
  end;

function NETXIsLoggedIn(Server : TnwServer) : boolean;
  var
    ObjID  : nwLong;
    Access : byte;
  begin
    NETXIsLoggedIn := false;
    if (nwbGetBinderyAccessLevel(Server, Access, ObjID) = 0) then
      NETXIsLoggedIn := (ObjID <> 0) and (ObjID <> -1);
  end;

function nwIsLoggedIn(Server : TnwServer) : boolean;
  begin
    case nwShellType of
      nsNETX : nwIsLoggedIn := NETXIsLoggedIn(Server);
      nsVLM  : nwIsLoggedIn := Lo(vlmCONNGetEntryField(Server, $03)) = 1;
    else
      nwIsLoggedIn := false;
    end;
  end;

end.
