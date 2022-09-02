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

unit NWFile;
  {-Unit to provide NetWare file I/O functionality.}

interface

uses
  NWBase,
  NWConn;

const
  {NWFILE extra error codes}
  nwfErrUnknownServer = $7F21;  {Server name not found}
  nwfErrUnknownVolume = $7F22;  {Volume name not found}
  nwfErrNotOnServer   = $7F23;  {Path is not on a server}
  nwfErrNoFileName    = $7F24;  {Filename missing}               {!!.51}
  nwfErrUNCTooShort   = $7F31;  {UNC: filename < 7 chars}
  nwfErrUNCBadStart   = $7F32;  {UNC: filename didn't start with '\\'}
  nwfErrUNCBadServer  = $7F33;  {UNC: server name < 2 chars}
  nwfErrUNCBadVolume  = $7F34;  {UNC: volume name < 2 chars}
  nwfErrUNCBadRoot    = $7F35;  {UNC: \\ after volume name}
  nwfErrNWBadServer   = $7F41;  {NW: unknown server name}
  nwfErrNWBadVolume   = $7F42;  {NW: unknown volume name}
  nwfErrDOSBadDrive   = $7F51;  {DOS: bad drive letter}

const
  {Extra normal file attributes}
  nwfaExecuteOnly = $08;        {File is executable program}
  nwfaShareable   = $80;        {File is shareable w/o opening as shared}

  {Extended file attributes}
  nweaSearchMode    = $07;      {Data file search mode bits for program}
  nweaTransactional = $10;      {File is transactional}
  nweaIndexed       = $20;      {File is indexed for fast random access}
  nweaReadAudit     = $40;      {File is read auditable}
  nweaWriteAudit    = $80;      {File is write auditable}

const
  {Directory rights bits}
  nwdrRead   = $01;        {..read files}
  nwdrWrite  = $02;        {..write to files}
  nwdrOpen   = $04;        {..open files}
  nwdrCreate = $08;        {..create new files}
  nwdrDelete = $10;        {..delete files}
  nwdrOwner  = $20;        {..modify trustee tights}
  nwdrSearch = $40;        {..scan for files and subdirectories}
  nwdrModify = $80;        {..modify filenames and attributes}

type
  {A NetWare volume name, including the terminating colon (:)}
  TnwVolumeName = string[17];

  {A NetWare file handle}
  TnwFileHandle = array [0..2] of word;

function nwParseFileName(FileName : string;
                         var Server : TnwServer;
                         var ServerName : TnwServerName;
                         var VolumeName : TnwVolumeName;
                         var Path : string) : TnwErrorCode;
  {-Parses a filename in UNC, DOS or NetWare format into a Server handle
    and name, volume name and remaining path. The server and volume are
    guaranteed to exist, but the path is not.
    ServerName will be just the server name.
    VolumeName will be the volume name terminated with a colon.
    Path will be the remaining path from FileName, augmented to start from
    the root directory of the volume. It will *not* have a leading '\',
    hence if it is the root directory, Path = ''.
    If the file name is on a local drive, Server will be set to zero,
    ServerName to '', VolumeName will be set to the drive letter plus
    colon and Path to the full path from the root directory on that drive.
    Again Path will not have a leading '\'.

    A UNC filename has the form:
       \\SERVER\VOLUME\DIR\..DIR\FILENAME.EXT
    A NetWare filename has the form:
       [SERVER\]VOLUME:DIR\..DIR\FILENAME.EXT
    where if the SERVER part is missing, it is assumed to be the current
    default server; VOLUME is assumed to be 2 or more characters long.
    Any other filename is assumed to be in DOS format. Drive letters will
    be mapped to the correct server handle.}

function nwGetFileAttr(FileName : string;
                       var FAttr    : byte;
                       var ExtFAttr : byte) : TnwErrorCode;
  {-Return the file attributes for a file on a NetWare server.}

function nwSetFileAttr(FileName : string;
                       FAttr    : byte;
                       ExtFAttr : byte) : TnwErrorCode;
  {-Set the file attributes for a file on a NetWare server.}

function nwUNCtoNetWare(UNC : string; var NW : string) : TnwErrorCode;
  {-Convert UNC filename (\\server\volume\path) into NetWare style
    (server\volume:path).
    Returns:
      0     - all OK
      $7F31 - not long enough (should be >= 7 chars)
      $7F32 - doesn't start with \\
      $7F33 - server name not long enough (should be >= 2 chars),
              or filename just consists of server part
      $7F34 - volume name not long enough (should be >= 2 chars)
      $7F35 - there's two \ after the volume name}

function nwLockRecord(Handle     : word;
                      Start, Len : nwLong;
                      TimeOut    : word) : TnwErrorCode;
  {-Lock a region of a file.}

function nwUnlockRecord(Handle     : word;
                        Start, Len : nwLong) : TnwErrorCode;
  {-Unlock a region of a file, previously locked by nwLockRecord.}

function nwGetDirRights(FileName : string;
                        var EffRightsMask : byte) : TnwErrorCode;  {!!.51}
  {-Get the rights mask for a directory for the current user}

implementation

type
  OS = record O, S : word; end; {to split pointer into sel/seg & ofs}
  LH = record L, H : word; end; {to split nwLong into hi/lo words}

type
  TFileInfo = array [0..77] of byte;

function PosCh(C : char; S : string) : integer; near;
  {-Return the position of character C in S, or zero if not found}
assembler;
  asm
    xor cx, cx          {zero the counter}
    mov bx, cx          {set result register to zero}
    les di, S           {get the string}
    mov cl, es:[di]     {get the string length}
    jcxz @@Exit         {check there's something to do}
    mov dx, cx          {save for later calc}
    inc di              {point to first character}
    mov al, C           {get character to look for}
    cld                 {forwards!}
    repne scasb         {scan for character}
    jne @@Exit          {not found: exit}
    sub dx, cx          {calculate char position}
    mov bx, dx          {set in result register}
  @@Exit:
    xchg ax, bx         {move result to ax}
  end;

procedure ReplaceSlashes(var S : string); near;
  {-Change all forward slashes to backward ones in S.}
assembler;
  asm
    les di, S
    cld
    xor cx, cx
    mov cl, es:[di]
    inc di
    mov ax, '\/'
  @@Again:
    repne scasb
    jne @@Exit
    mov es:[di-1], ah
    or cx, cx
    jnz @@Again
  @@Exit:
  end;

procedure ConcatChar(var S : string; C : char); near; assembler;
  {Concatenate a character onto the end of a string, no checking}
  asm
    xor bx, bx
    les di, S
    mov bl, es:[di]
    inc bx
    mov es:[di], bl
    mov al, C
    mov es:[di+bx], al
  end;

procedure AppendStr(var ToSt, FromSt : string;
                        FromCh, NumCh : byte); near; assembler;
  {Append NumCh chars from FromSt[FromCh] to the end of ToSt}
  asm
    mov dx, ds                {save Pascal's DS}
    xor ax, ax                {Zero ax, bx, cx}
    mov bx, ax
    mov cx, ax
    lds si, FromSt            {point ds:si at FromSt}
    mov cl, [si]              {get length of FromSt}
    mov bl, FromCh            {get start char pos}
    or bx, bx                 {..if zero make it 1}
    jnz @@FromChNot0
    inc bx
  @@FromChNot0:
    cmp bx, cx                {is start char beyond end of string?}
    ja @@Exit                 {yes, so nothing to do}
    add si, bx                {point to start char}
    sub cx, bx                {calc num of chars remaining in string}
    inc cx
    mov bl, NumCh             {get num chars required}
    cmp bx, cx                {calc actual num chars to move}
    ja @@NoTrimming
    mov cx, bx
  @@NoTrimming:
    les di, ToSt              {point es:di at ToSt}
    mov al, es:[di]           {get the ToSt length}
    mov bx, ax                {save it for now}
    add al, cl                {calc new ToSt length}
    jno @@NoOverflow
    mov ax, 255
  @@NoOverflow:
    mov es:[di], al           {set ToSt length}
    add di, bx                {point es:di at dest char}
    inc di
    shr cx, 1                 {copy ToSt to FromSt}
    rep movsw
    adc cx, cx
    rep movsb
  @@Exit:
    mov ds, dx                {restore Pascal's DS}
  end;

function nwGetVolumeNumber(Server : TnwServer; Name : TnwVolumeName;
                           var VolNumber : byte) : TnwErrorCode;
  {-Return the volume number for a volume name.}
  var
    Request : record
      Len  : word;
      Func : byte;
      VolN : TnwVolumeName;
    end;
    Reply  : byte;
    Status : word;
  begin
    {API: Get Volume Number}
    with Request do
      begin
        Len := 2 + length(Name);
        Func := $05;
        VolN := Name;
        dec(VolN[0]);  {remove the colon}
        nwUpperStr(VolN);
      end;
    Status := nwServerCall(Server, $16, Request.Len+2, Request,
                                        sizeof(Reply), Reply);
    if (Status = 0) then
      VolNumber := Reply;
    nwGetVolumeNumber := Status;
  end;

function nwUNCtoNetWare(UNC : string; var NW : string) : TnwErrorCode;
assembler;
  asm
    push ds                     {save Pascal's DS}
    xor bx, bx                  {zeroise BX}
    mov cx, bx                  {..and CX}

    lds si, UNC                 {get the UNC filename}
    les di, NW                  {get the NW result filename}
    cld                         {forwards!}
    lodsb                       {get length of UNC filename}
    mov cl, al                  {set CX to the length}

    xchg bh, al                 {store orig length in BH}
    stosb                       {set the result length to zero}

    mov bl, $31                 {assume error nwfErrUNCTooShort}
    cmp cx, 7                   {is source length 7 or more}
    jb @@Error                  {no - report error}

    inc bl                      {assume error nwfErrUNCBadStart}
    lodsw                       {get first two chars}
    cmp ax, '\\'                {..are they '\\'?}
    jne @@Error                 {no - report error}

    dec cx                      {decrement chars left}
    dec cx

    inc bl                      {assume error nwfErrUNCBadServer}
    xor dx, dx                  {zeroise length of server string}
  @@State1Loop:
    lodsb                       {get next char}
    stosb                       {store it in result}
    dec cx                      {decrement chars left}
    jz @@Error                  {zero? yes - report error}
    cmp al, '\'                 {is char a '\'?}
    je @@State1Exit             {yes - exit server loop}
    inc dx                      {increment num chars in server part}
    jmp @@State1Loop            {go get next char}
  @@State1Exit:
    cmp dx, 2                   {is server part long enough?}
    jb @@Error                  {no - report error}

    inc bl                      {assume error nwfErrUNCBadVolume}
    xor dx, dx                  {zeroise length of volume string}
  @@State2Loop:
    lodsb                       {get next char}
    cmp al, '\'                 {is char a '\'?}
    je @@State2Exit             {yes - exit volume loop}
    stosb                       {store char in result}
    inc dx                      {increment num chars in volume part}
    dec cx                      {decrement chars left}
    jnz @@State2Loop            {if more, go get next char}
  @@State2Exit:
    cmp dx, 2                   {is volume part long enough?}
    jb @@Error                  {no - report error}
    mov al, ':'                 {terminate the volume name}
    stosb                       {..with a ':'}
    or cx, cx                   {cx=0 => no '\'; is this so?}
    jnz @@DoTheRest             {no - go process rest of string}
    inc bh                      {take account of ':'}
    jmp @@StoreLength           {go set the result length}

  @@Error:
    mov ax, $7F00               {set AX, the function result}
    mov al, bl                  {set the error code}
    jmp @@Exit                  {..and exit}

  @@DoTheRest:
    dec cx                      {take account of last '\' read}
    jz @@StoreLength            {any more chars left? no - go set length}
    inc bl                      {assume error nwfErrUNCBadRoot}
    cmp byte ptr [si], '\'      {are there two '\' after volume?}
    je @@Error                  {yes - report error}
    shr cx, 1                   {copy rest of characters}
    rep movsw
    adc cx, cx
    rep movsb

  @@StoreLength:
    xor ax, ax                  {zeroise AX}
    xchg bh, al                 {get length byte}
    dec al                      {subtract 2 for the initial '\\'}
    dec al
    sub di, ax                  {point to start of result string}
    dec di
    pop ds                      {get back Pascal's DS because}
    push ds                     {  nwUpperStr is a global variable}
    push es                     {push parms for nwUpperStr}
    push di
    stosb                       {store the length byte}
    call nwUpperStr             {uppercase the string}
    xor ax, ax                  {function result of 0 = success}

  @@Exit:
    pop ds                      {get back Pascal's DS}
  end;

function nwNetWareToNetWare(S : string; var NW : string) : TnwErrorCode;
  {-Convert a full/partial NetWare path into a full one. Internal use.}
  var
    PosColon, PosSlash : integer;
    LenS : integer;
    SI : TnwServerInfo;
  begin
    PosColon := PosCh(':', S);
    if (PosColon = 0) then
      begin
        nwNetWareToNetWare := nwfErrNWBadVolume;
        Exit;
      end;
    PosSlash := PosCh('\', S);
    if (PosSlash = 0) or (PosSlash > PosColon) then
      begin
        if (nwGetServerInfo(nwDefaultServer, SI) <> 0) then
          begin
            nwNetWareToNetWare := nwfErrNWBadServer;
            Exit;
          end;
        NW := SI.ServerName;
        ConcatChar(NW, '\');
        AppendStr(NW, S, 1, 255);
      end
    else
      NW := S;
    nwUpperStr(NW);
    nwNetWareToNetWare := 0;
  end;

function NETXGetRelativeDriveDepth(Drive : char) : integer; near;
  {-Return number of subdirectories deep for a map root drive. 0 means
    at root, 255 means the drive is not a map root drive.}
assembler;
  asm
    {API: Get Relative Drive Depth}
    mov ax, $E907
    mov bl, Drive
    sub bl, 'A' - 1       {assumes Drive is in uppercase}
    int $21
    xor ah, ah
  end;

function NETXGetDirHandle(Drive : char;
                          var Server : TnwServer;
                          var DirHandle : byte) : TnwErrorCode; near;
  {-Return connection ID and directory handle for NetWare drive. NETX only.}
  type
    TByteTable = array ['A'..'`'] of byte; {ie 32 drives}
  var
    Status    : word;
    ByteTable : TByteTable;
  begin
    {API: Get Drive Connection ID}
    {     Get Drive Handle Table}
    Status := nwNETXGetDriveTable($EF02, ByteTable);
    if (Status = 0) then
      begin
        Server := ByteTable[Drive];
        if (Server = 0) then
          Status := nwfErrUnknownServer
        else
          begin
            Status := nwNETXGetDriveTable($EF00, ByteTable);
            if (Status = 0) then
              DirHandle := ByteTable[Drive];
          end;
      end;
    NETXGetDirHandle := Status;
  end;

function NETXGetRedir(Drive : char; var SV : string;
                                    var MR : string;
                                    var CD : string) : TnwErrorCode;
  {-Return the server and volume names (in form 'SERVER\VOLUME:'),
    the map root directory (in form 'DIR...\DIR', with root
    directory returned as '') and the current directory (in form
    '\DIR...\DIR' with root dir returned as '\') for drive.
    ***NETX only***.}
  var
    DirsDeep : integer;
    DirNum   : integer;
    i        : integer;
    StartPos : integer;
    PosColon : integer;
    Status   : word;
    Server   : word;
    Request : record
      Len : word;
      Func: byte;
      DirH: byte;
    end;
    Reply : string;
    SI    : TnwServerInfo;
    FoundOne : boolean;
  begin
    {API: Get Directory Path}
    Status := NETXGetDirHandle(Drive, Server, Request.DirH);
    if (Status <> 0) then
      begin
        NETXGetRedir := Status;
        Exit;
      end;
    Request.Len := 2;
    Request.Func := 1;
    Status := nwServerCall(Server, $16, sizeof(Request), Request,
                                        sizeof(Reply), Reply);
    {Reply is in VOLUME:DIR\...\DIR format, sometimes with /'s instead}
    ReplaceSlashes(Reply);
    if (Status = 0) then
      Status := nwGetServerInfo(Server, SI);
    if (Status = 0) then
      begin
        PosColon := PosCh(':', Reply);
        SV := SI.ServerName;
        ConcatChar(SV, '\');
        AppendStr(SV, Reply, 1, PosColon);
        DirsDeep := NETXGetRelativeDriveDepth(Drive);
        if (DirsDeep = 255) then {not a map root drive}
          begin
            MR := '';
            CD := '\';
            AppendStr(CD, Reply, PosColon+1, 255);
          end
        else if (DirsDeep = 0) then
          begin
            CD := '\';
            MR := '';
            AppendStr(MR, Reply, PosColon+1, 255);
          end
        else {DirsDeep > 0}
          begin
            StartPos := length(Reply)+1;
            for DirNum := 1 to DirsDeep do
              begin
                FoundOne := false;
                i := StartPos-1;
                while (not FoundOne) and (i > PosColon) do
                  if (Reply[i] = '\') then
                    FoundOne := true
                  else
                    dec(i);
                StartPos := i;
              end;
            CD := '\';
            AppendStr(CD, Reply, StartPos+1, 255);
            MR := '';
            AppendStr(MR, Reply, PosColon+1, StartPos-PosColon-1);
          end;
      end;
    NETXGetRedir := Status;
  end;

function vlmGetRedir(Drive : char; var SV : string;
                                   var MR : string;
                                   var CD : string) : TnwErrorCode;
  {-Return the server and volume names (in form 'SERVER\VOLUME:'),
    the map root directory (in form 'DIR...\DIR', with root
    directory returned as '') and the current directory (in form
    '\DIR...\DIR' with root dir returned as '\') for drive.
    ***VLM only***.}
  var
    Status   : word;
    PosColon : integer;
    StartPos : integer;
    Regs     : TnwRegisters;
  begin
    {API: _REDIR Specific}
    FillChar(nwGlobalBuf^, 16+512, 0);
    nwGlobalBuf^[0] := ord(Drive);
    nwGlobalBuf^[1] := ord(':');
    nwInitRegs(Regs);
    with Regs do
      begin
        {DS := OS(nwGlobalBufRealPtr).S;}           {!!.51}
        {SI := OS(nwGlobalBufRealPtr).O;}           {!!.51}
        DS := OS(nwGlobalBufVLM).S;
        SI := OS(nwGlobalBufVLM).O;
        ES := DS;
        DI := SI + 16;
      end;
    Status := vlmCall(vlmRedir, $08, Regs);
    {reply is in \\SERVER\VOLUME<null>\DIR...\DIR<null> format, or
     if the drive is a map root drive in
     \\SERVER\VOLUME\DIR...\DIR<null>\DIR...\DIR<null> format}
    if (Status = 0) then
      begin
        nwCvtAsciizToStr(nwGlobalBuf^[16], 255);
        Move(nwGlobalBuf^[16], SV, nwGlobalBuf^[16]+1);
        StartPos := 16+length(SV)+1;
        nwCvtAsciizToStr(nwGlobalBuf^[StartPos], 255);
        Move(nwGlobalBuf^[StartPos], CD, nwGlobalBuf^[StartPos]+1);
        Status := nwUNCToNetWare(SV, SV);
        {check for a map root drive}
        PosColon := PosCh(':', SV);
        MR := '';
        if (PosColon < length(SV)) then
          begin
            AppendStr(MR, SV, PosColon+1, 255);
            SV[0] := char(PosColon);
          end;
      end;
    vlmGetRedir := Status;
  end;

function nwGetRedir(Drive : char; var SV : string;
                                  var MR : string;
                                  var CD : string) : TnwErrorCode;
  {-Return the server and volume names (in form 'SERVER\VOLUME:'),
    the map root directory (in form 'DIR...\DIR', with root
    directory returned as '') and the current directory (in form
    '\DIR...\DIR' with root dir returned as '\') for drive.}
  begin
    case nwShellType of
      nsNETX : nwGetRedir := NETXGetRedir(Drive, SV, MR, CD);
      nsVLM  : nwGetRedir := vlmGetRedir(Drive, SV, MR, CD);
    else
      nwGetRedir := nwErrShell;
    end;{case}
  end;

function nwParseDOSFileName(var FileName : string;
                            var Server : TnwServer;
                            var ServerName : TnwServerName;
                            var VolumeName : TnwVolumeName;
                            var Path : string) : TnwErrorCode;
  {-The version of nwParseFileName for DOS-style filenames.}
  var
    PosSlash, PosColon : integer;
    FNLen  : integer;
    StrInx : integer;
    Status : word;
    ServerVol : string;
    RootPath  : string;
    CurPath   : string;
    DriveLetter : char;
  begin
    nwUpperStr(FileName);
    {get the drive letter}
    FNLen := length(FileName);
    if (FNLen > 1) and (FileName[2] = ':') then
      begin
        DriveLetter := FileName[1];
        if (DriveLetter < 'A') or (DriveLetter > 'Z') then
          begin
            nwParseDOSFileName := nwfErrDOSBadDrive;
            Exit;
          end;
        StrInx := 3;
      end
    else {no drive present in FileName, use current drive}
      begin
        asm
          mov ah, $19
          int $21
          add al, 'A'
          mov DriveLetter, al
        end;
        StrInx := 1;
      end;
    {convert the drive letter to a NetWare path in netware format}
    Status := nwGetRedir(DriveLetter, ServerVol, RootPath, CurPath);
    if (Status = 0) then
      begin
        while (FNLen > StrInx) and (FileName[FNLen] = '\') do
          dec(FNLen);
        FileName[0] := char(FNLen);
        PosSlash := PosCh('\', ServerVol);
        PosColon := PosCh(':', ServerVol);
        ServerName := '';
        VolumeName := '';
        AppendStr(ServerName, ServerVol, 1, PosSlash-1);
        AppendStr(VolumeName, ServerVol, PosSlash+1, PosColon-PosSlash);
        Server := nwServerFromName(ServerName);
        Path := RootPath;
        if (StrInx > FNLen) then
          begin
            if (length(CurPath) <> 1) then {ie CurPath <> '\'}
              begin
                if (Path <> '') then
                  ConcatChar(Path, '\');
                AppendStr(Path, CurPath, 2, 255);
              end
          end
        else {StrInx points within FileName}
          if (FileName[StrInx] = '\') then
            begin
              if (StrInx <> FNLen) then
                begin
                  if (Path <> '') then
                    ConcatChar(Path, '\');
                  AppendStr(Path, FileName, StrInx+1, 255);
                end;
            end
          else {the StrInx'th char is not a '\'}
            begin
              if (length(CurPath) > 1) then
                AppendStr(Path, CurPath, 1, 255);
              if (Path <> '') then
                ConcatChar(Path, '\');
              AppendStr(Path, FileName, StrInx, 255)
            end;
      end
    else {it's not a NetWare drive letter}
      begin
        Server := 0;
        ServerName := '';
        VolumeName[0] := #2;
        VolumeName[1] := DriveLetter;
        VolumeName[2] := ':';
        Path := '';
        while (FNLen > StrInx) and (FileName[FNLen] = '\') do
          dec(FNLen);
        FileName[0] := char(FNLen);
        if (StrInx <= FNLen) and (FileName[StrInx] = '\') then
          begin
            Status := 0;
            AppendStr(Path, FileName, StrInx+1, 255);
          end
        else {FileName's path does not start at root directory}
          begin
            GetDir(ord(DriveLetter) - ord('A') + 1, CurPath);
            Status := IOResult;
            if (Status = 0) then
              begin
                if (length(CurPath) = 3) then {ie CurPath = 'd:\'}
                  begin
                    if (StrInx < FNLen) then
                      AppendStr(Path, FileName, StrInx, 255);
                  end
                else
                  begin
                    AppendStr(Path, CurPath, 4, 255);
                    if (StrInx < FNLen) then
                      begin
                        ConcatChar(Path, '\');
                        AppendStr(Path, FileName, StrInx, 255)
                      end;
                  end;
              end;
          end;
      end;
    nwParseDOSFileName := Status;
  end;


function nwParseFileName(FileName : string;
                         var Server : TnwServer;
                         var ServerName : TnwServerName;
                         var VolumeName : TnwVolumeName;
                         var Path : string) : TnwErrorCode;
  var
    Status     : word;
    PosSlash, PosColon : integer;
    TempName   : string;
    VolNum     : byte;
  begin
    if (FileName = '') then
      begin
        nwParseFileName := nwfErrNoFileName;
        Exit;
      end;
    {Check first for a UNC filename}
    if (length(FileName) >= 2) and
       (FileName[1] = '\') and (FileName[2] = '\') then
      Status := nwUNCToNetWare(FileName, TempName)
    else
    {Check next for a NetWare filename}
      begin
        PosColon := PosCh(':', FileName);
        if (PosColon > 2) then
          Status := nwNetWareToNetWare(FileName, TempName)
        else {it's a DOS filename}
          begin
            nwParseFileName := nwParseDOSFileName(FileName,
                                                  Server, ServerName,
                                                  VolumeName,
                                                  Path);
            Exit;
          end;
      end;
    if (Status = 0) then
      begin
        PosSlash := PosCh('\', TempName);
        if (PosSlash > sizeof(TnwServerName)) then
          Status := nwfErrUnknownServer
        else
          begin
            ServerName := '';
            AppendStr(ServerName, TempName, 1, PosSlash-1);
            Server := nwServerFromName(ServerName);
            if (Server = 0) then
              Status := nwfErrUnknownServer
            else
              begin
                PosColon := PosCh(':', TempName);
                if (PosColon > sizeof(TnwVolumeName)+PosSlash+1) then
                  Status := nwfErrUnknownVolume
                else
                  begin
                    VolumeName := '';
                    AppendStr(VolumeName, TempName, PosSlash+1, PosColon-PosSlash);
                    Status := nwGetVolumeNumber(Server, VolumeName, VolNum);
                    if (Status <> 0) then
                      Status := nwfErrUnknownVolume
                    else
                      begin
                        Path := '';
                        AppendStr(Path, TempName, PosColon+1, 255);
                      end;
                  end;
              end;
          end;
      end;
    nwParseFileName := Status;
  end;

function ScanFileInformation(Server : TnwServer;
                             var Path       : string;
                             var FileInfo   : TFileInfo) : TnwErrorCode;
  {-NetWare's equivalent of FindFirst/FindNext.}
  var
    Status : word;
    Request : record
      Len  : word;
      Func : byte;
      SInx : word;
      Hndl : byte;
      SAttr: byte;
      FName: string;
    end;
    Reply : record
      Sinx : word;
      FName: array [0..13] of char;
      Attr : TFileInfo;
    end;
  begin
    {API: Scan File Information}
    with Request do
      begin
        FName := Path;
        Len := length(FName) + 6;
        Func := $0F;
        SInx := $FFFF;
        Hndl := 0;
        SAttr := $06;  {include hidden & system}
      end;
    Status := nwServerCall(Server, $17, Request.Len+2, Request,
                                        sizeof(Reply), Reply);
    if (Status = 0) then
      FileInfo := Reply.Attr;
    ScanFileInformation := Status;
  end;

function SetFileInformation(Server : TnwServer;
                            var Path       : string;
                            var FileInfo   : TFileInfo) : TnwErrorCode;
  {-Update the file's directory information.}
  var
    Request    : record
      Len  : word;
      Func : byte;
      FAttr: TFileInfo;
      Hndl : byte;
      SAttr: byte;
      FName: string;
    end;
    Dummy : word;
  begin
    {API: Set File Information}
    with Request do
      begin
        FName := Path;
        Len := length(FName) + 82;
        Func := $10;
        FAttr := FileInfo;
        Hndl := 0;
        SAttr := $06;  {include hidden & system}
      end;
    SetFileInformation := nwServerCall(Server, $17, Request.Len+2, Request,
                                                    0, Dummy);
  end;


function nwGetFileAttr(FileName : string;
                       var FAttr    : byte;
                       var ExtFAttr : byte) : TnwErrorCode;
  var
    Status : word;
    Server : TnwServer;
    ServerName : TnwServerName;
    VolumeName : TnwVolumeName;
    FileInfo   : TFileInfo;
    Path       : string;
  begin
    Status := nwParseFileName(FileName, Server, ServerName, VolumeName, Path);
    if (Status = 0) then
      if (Server = 0) then
        Status := nwfErrNotOnServer
      else
        begin
          Insert(VolumeName, Path, 1);
          Status := ScanFileInformation(Server, Path, FileInfo);
          if (Status = 0) then
            begin
              FAttr := FileInfo[0];
              ExtFAttr := FileInfo[1];
            end;
        end;
    nwGetFileAttr := Status;
  end;

function nwSetFileAttr(FileName : string;
                       FAttr    : byte;
                       ExtFAttr : byte) : TnwErrorCode;
  var
    Status : word;
    Server : TnwServer;
    ServerName : TnwServerName;
    VolumeName : TnwVolumeName;
    FileInfo   : TFileInfo;
    Path       : string;
  begin
    Status := nwParseFileName(FileName, Server, ServerName, VolumeName, Path);
    if (Status = 0) then
      if (Server = 0) then
        Status := nwfErrNotOnServer
      else
        begin
          Insert(VolumeName, Path, 1);
          Status := ScanFileInformation(Server, Path, FileInfo);
          if (Status = 0) then
            begin
              FileInfo[0] := FAttr;
              FileInfo[1] := ExtFAttr;
              Status := SetFileInformation(Server, Path, FileInfo);
            end;
        end;
    nwSetFileAttr := Status;
  end;

function vlmDOSToNWHandle(DOSHandle : word;
                          var Server: TnwServer;
                          var NWH   : TnwFileHandle) : TnwErrorCode;
  var
    Status : word;
    Regs : TnwRegisters;
  begin
    {API: _REDIR DOS To NW Handle}
    nwInitRegs(Regs);
    with Regs do
      begin
        BX := DOSHandle;
        {ES := OS(nwGlobalBufRealPtr).S;}             {!!.51}
        {DI := OS(nwGlobalBufRealPtr).O;}             {!!.51}
        ES := OS(nwGlobalBufVLM).S;                   {!!.51}
        DI := OS(nwGlobalBufVLM).O;                   {!!.51}
      end;
    Status := vlmCall(vlmRedir, $05, Regs);
    if (Status = 0) then
      begin
        Server := Regs.CX;
        Move(nwGlobalBuf^[0], NWH, sizeof(NWH));
      end;
    vlmDOSToNWHandle := Status;
  end;

function vlmLockRecord(Handle     : word;
                       Start, Len : nwLong;
                       TimeOut    : word) : TnwErrorCode;
  var
    Status : word;
    NWH    : TnwFileHandle;
    Server : TnwServer;
    Request : record
      Flag : byte;
      Hndl : TnwFileHandle;
      RgnSt : nwLong;
      RgnLen: nwLong;
      TimOut: word;
    end;
    Dummy : word;
  begin
    {API: Log Physical Record (old)}
    {Note: the new version of Log Physical Record looks like the old
           version - ???}
    Status := vlmDOSToNWHandle(Handle, Server, NWH);
    if (Status = 0) then
      begin
        with Request do
          begin
            Flag := $01;
            Hndl := NWH;
            RgnSt := nwSwapLong(Start);
            RgnLen := nwSwapLong(Len);
            TimOut := swap(TimeOut);                            {!!.52}
          end;
        Status := nwServerCall(Server, $1A, sizeof(Request), Request,
                                            0, Dummy);
      end;
    vlmLockRecord := Status;
  end;

function NETXLockRecord(Handle     : word;
                        Start, Len : nwLong;
                        TimeOut    : word) : TnwErrorCode;
  var
    Status : word;
    Regs   : TnwRegisters;
  begin
    {$IFDEF Windows} {!!.51 start}
    Status := 0;
    asm
      push bp
      mov ax, $BC01
      mov bx, Handle
      mov cx, Start.Word[2]
      mov dx, Start.Word[0]
      mov si, Len.Word[2]
      mov di, Len.Word[0]
      mov bp, TimeOut
      int $21
      pop bp
      or al, al                                                 {!!.52}
      jz @@Exit                                                 {!!.52}
      xor ah, ah
      add ax, nwErrBaseNETX
      mov Status, ax
    @@Exit:
    end;
    {$ELSE}          {!!.51 end}
    nwInitRegs(Regs);
    with Regs do
      begin
        AX := $BC01;
        BX := Handle;
        CX := LH(Start).H;
        DX := LH(Start).L;
        SI := LH(Len).H;
        DI := LH(Len).L;
        BP := TimeOut;
      end;
    Status := nwIntr($21, Regs);
    if (Status = 0) then
      if (Regs.AL <> 0) then
        Status := nwErrBaseNETX + Regs.AL;
    {$ENDIF}
    NETXLockRecord := Status;
  end;

function nwLockRecord(Handle     : word;
                      Start, Len : nwLong;
                      TimeOut    : word) : TnwErrorCode;
  begin
    case nwShellType of
      nsNETX : nwLockRecord := NETXLockRecord(Handle, Start, Len, Timeout);
      nsVLM  : nwLockRecord := vlmLockRecord(Handle, Start, Len, Timeout);
    else
      nwLockRecord := nwErrShell;
    end;{case}
  end;

function vlmUnlockRecord(Handle     : word;
                         Start, Len : nwLong) : TnwErrorCode;
  var
    Status : word;
    NWH    : TnwFileHandle;
    Server : TnwServer;
    Request : record
      Res : byte;
      Hndl : TnwFileHandle;
      RgnSt : nwLong;
      RgnLen: nwLong;
    end;
    Dummy : word;
  begin
    {API: Clear Physical Record}
    Status := vlmDOSToNWHandle(Handle, Server, NWH);
    if (Status = 0) then
      begin
        with Request do
          begin
            Hndl := NWH;
            RgnSt := nwSwapLong(Start);
            RgnLen := nwSwapLong(Len);
          end;
        Status := nwServerCall(Server, $1E, sizeof(Request), Request,
                                            0, Dummy);
      end;
    vlmUnlockRecord := Status;
  end;

function NETXUnlockRecord(Handle     : word;
                          Start, Len : nwLong) : TnwErrorCode;
  var
    Status : word;
    Regs   : TnwRegisters;
  begin
    {$IFDEF Windows} {!!.51 start}
    Status := 0;
    asm
      mov ah, $BE
      mov bx, Handle
      mov cx, Start.Word[2]
      mov dx, Start.Word[0]
      mov si, Len.Word[2]
      mov di, Len.Word[0]
      int $21
      or al, al                                                 {!!.52}
      jz @@Exit                                                 {!!.52}
      xor ah, ah
      add ax, nwErrBaseNETX
      mov Status, ax
    @@Exit:
    end;
    {$ELSE}          {!!.51 end}
    nwInitRegs(Regs);
    with Regs do
      begin
        AH := $BE;
        BX := Handle;
        CX := LH(Start).H;
        DX := LH(Start).L;
        SI := LH(Len).H;
        DI := LH(Len).L;
      end;
    Status := nwIntr($21, Regs);
    if (Status = 0) then
      if (Regs.AL <> 0) then
        Status := nwErrBaseNETX + Regs.AL;
    {$ENDIF}
    NETXUnlockRecord := Status;
  end;


function nwUnlockRecord(Handle     : word;
                        Start, Len : nwLong) : TnwErrorCode;
  begin
    case nwShellType of
      nsNETX : nwUnlockRecord := NETXUnlockRecord(Handle, Start, Len);
      nsVLM  : nwUnlockRecord := vlmUnlockRecord(Handle, Start, Len);
    else
      nwUnlockRecord := nwErrShell;
    end;{case}
  end;

function nwGetDirRights(FileName : string;
                        var EffRightsMask : byte) : TnwErrorCode;   {!!.51}
  var
    Status : word;
    Server : TnwServer;
    ServerName : TnwServerName;
    VolumeName : TnwVolumeName;
    Request    : record
      Len : word;
      Func: byte;
      DirH: byte;
      Path: string;
    end;
  begin
    Status := nwParseFileName(FileName, Server, ServerName, VolumeName, Request.Path);
    if (Status = 0) then
      if (Server = 0) then
        Status := nwfErrNotOnServer
      else
        begin
          System.Insert(VolumeName, Request.Path, 1);
          with Request do
            begin
              Len := length(Path) + 3;
              Func := $03;
              DirH := 0;
            end;
          EffRightsMask := 0;
          Status := nwServerCall(Server, $16, Request.Len + 2, Request,
                                              1, EffRightsMask);
        end;
    nwGetDirRights := Status;
  end;


end.
