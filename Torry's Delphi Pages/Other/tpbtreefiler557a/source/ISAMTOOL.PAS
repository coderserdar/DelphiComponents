{********************************************************************}
{* ISAMTOOL.PAS - various tool routines                             *}
{********************************************************************}

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
{$IFDEF CanAllowOverlays}
  {$O+,F+}
{$ENDIF}

{$IFDEF Win32}
{$H-} {no long string routines}
{$ENDIF}

{--Definition of possible languages for error messages of IsamErrormessage }
{ $DEFINE GermanMessage}
{$DEFINE EnglishMessage}
  {-Adding a space before the $ sign of the DEFINE deactivates the error
    messages of this language}

Unit
  IsamTool;

Interface

Uses
{$IFDEF Windows}
  WinProcs,
{$ELSE}
{$IFDEF Win32}
  Windows,
{$ELSE}
  BaseSupp,
{$ENDIF}
{$ENDIF}
  BTBase,
  BTIsBase,
  Filer;     {!!.TP}

Type
  UsedErrorMessages = ( NoMsg, German, English );

Const
  UseErrorMessage : UsedErrorMessages =
  {$IFDEF EnglishMessage}
    English;
  {$ELSE}
    {$IFDEF GermanMessage}
      German;
    {$ELSE}
      NoMsg;
    {$ENDIF}
  {$ENDIF}

  Procedure ExtendHandles ( NumHandles : Word );
    {-Extends the number of file handles available to this application}

  Function IsamErrorMessage ( ErrorNr : Integer ) : String;
    {-Returns an error describing string}

{$IFDEF LengthByteKeys}
  Procedure InvertString ( Var Dest   : String;
                               Source : String;
                               MaxLen : Byte );
    {-Inverts the string Source under a maximum length of MaxLen returning
      the result in Dest}
{$ENDIF}
{$IFDEF ASCIIZeroKeys}
  Procedure InvertString ( Var Dest;
                           Var Source;
                               MaxLen : Byte );
    {-Inverts the null terminated array of character Source under a
      maximum length of MaxLen returning the result in Dest}
{$ENDIF}


Implementation

{$IFDEF Win32}
  Procedure ExtendHandles ( NumHandles : Word );
    {-Extends the number of file handles available to this application}

  Begin
    If SetHandleCount ( NumHandles ) < NumHandles Then Begin     {!!.51}
      IsamOK := False;
      IsamError := 10192;
    End;
  End;
{$ELSE}
{$IFDEF Windows}
  Procedure ExtendHandles ( NumHandles : Word );
    {-Extends the number of file handles available to this application}

  Begin
    If SetHandleCount ( NumHandles ) < NumHandles Then Begin     {!!.51}
      IsamOK := False;
      IsamError := 10192;
    End;
  End;
{$ELSE}
  Procedure ExtendHandles ( NumHandles : Word );
    {-Extends the number of file handles available to this application}
 {$IFNDEF DPMI}
  Const
    Safety = 16;                  {Bytes of overhead for new handle table}

  Type
    SegOfs = Record    {Structure of a pointer}
      O, S : Word;
    End;
 {$ENDIF}

  Var
    ParasNeeded : Word;
    IRR         : GenRegisters;
    FirstTrial,
    Continue    : Boolean;


 {$IFNDEF DPMI}
    Function FreeRamInParas : Word;
      {-Returns the number of free paragraphs of RAM available to DOS}
    Var
      IRR : GenRegisters;

    Begin
      DefaultRegisters ( IRR );
      With IRR Do Begin
        AX := $4800;
        BX := $FFFF;
        If IsamDOSError = 0 Then IsamDOSFunc := AX;
        CallMsDos ( IRR );
        If Odd (Flags) And (IsamDOSError = 0) Then IsamDOSError := AX;
        FreeRamInParas := BX;
      End;
    End;


    Function SetBlock ( Var Paragraphs : Word ) : Boolean;
      {-Change size of DOS memory block allocated to this program}
    Var
      IRR : GenRegisters;

    Begin
      DefaultRegisters ( IRR );
      With IRR Do Begin
        AX := $4A00;
        ES := PrefixSeg;
        BX := Paragraphs;
        If IsamDOSError = 0 Then IsamDOSFunc := AX;
        CallMsDos ( IRR );
        Paragraphs := BX;
        If Odd (Flags) And (IsamDOSError = 0) Then IsamDOSError := AX;
        SetBlock := Not Odd (Flags);
      End;
    End;


    Function PtrDiff ( H, L : Pointer ) : LongInt;
      {-Return the number of bytes between H^ and L^. H is the higher address}
    Var
      High : SegOfs Absolute H;
      Low  : SegOfs Absolute L;

    Begin
      PtrDiff := (LongInt (High.S) Shl 4 + High.O)
          - (LongInt (Low.S) Shl 4 + Low.O);
    End;


    Function GetDosMemory ( Paras : Word ) : Boolean;
      {-Shrinks the heap to provide Paras free DOS memory}
    Var
      ParasToGive : Word;
      ParasToKeep : Word;

    Begin
      GetDosMemory := False;

    {--Paragraphs we want to give away}
      ParasToGive := Succ (Paras);

    {--Assure space free at top of heap}
      If PtrDiff ( HeapEnd, HeapPtr ) Shr 4 < ParasToGive Then Exit;

    {--Deallocate memory for DOS}
      ParasToKeep := SegOfs (HeapEnd).S - PrefixSeg - ParasToGive;
      If Not SetBlock ( ParasToKeep ) Then Exit;

    {--Adjust end of heap down}
      HeapEnd := Ptr (SegOfs (HeapEnd).S - ParasToGive, 0);

    {--Adjust the PSP record of the top of memory}
      MemW [PrefixSeg:2] := SegOfs (HeapEnd).S;

      GetDosMemory := True;
    End;
  {$IFDEF VER55}
    Function GetDosMemory ( Paras : Word ) : Boolean;
      {-Shrinks the heap to provide Paras free DOS memory}
    Var
      TopOfHeap    : Pointer;
      NewFreePtr   : Pointer;
      ParasWeHave  : Word;
      ParasToGive  : Word;
      FreeListSize : Word;


      Function EndOfHeap : Pointer;
        {-Returns a pointer to the end of the free list}
      Var
        FreeSegOfs : SegOfs Absolute FreePtr;

      Begin
        If FreeSegOfs.O = 0 Then Begin
          {-The free list is empty, add $1000 to the segment}
          EndOfHeap := Ptr (FreeSegOfs.S+$1000, 0)
        End Else Begin
          EndOfHeap := Ptr (FreeSegOfs.S + (FreeSegOfs.O Shr 4), 0);
        End;
      End;


    Begin
      GetDosMemory := False;

    {--Pointer to next address past program}
      TopOfHeap := Ptr (SegOfs (FreePtr).S + $1000, 0);

    {--Paragraphs currently allocated to program}
      ParasWeHave := SegOfs (TopOfHeap).S - PrefixSeg;

    {--Paragraphs we want to give away}
      ParasToGive := Succ (Paras);

    {--Assure space free at top of heap}
      If PtrDiff ( EndOfHeap, HeapPtr ) Shr 4 < ParasToGive Then Exit;

    {--Size of free list to move}
      FreeListSize := PtrDiff ( TopOfHeap, EndOfHeap );

    {--Adjust free list down}
      NewFreePtr := Ptr (SegOfs (FreePtr).S - ParasToGive, SegOfs (FreePtr).O);
      If FreeListSize > 0 Then Move ( FreePtr^, NewFreePtr^, FreeListSize );
      FreePtr := NewFreePtr;

    {--Deallocate memory for DOS}
      Dec ( ParasWeHave, ParasToGive );
      If Not SetBlock ( ParasWeHave ) Then Exit;

    {--Adjust the PSP record of the top of memory}
      MemW [PrefixSeg:2] := SegOfs (FreePtr).S + $1000;

      GetDosMemory := True;
    End;
  {$ENDIF}
  {$ENDIF}


  Begin
    IsamClearOK;

    {!!.41 removed NumHandles <= 20}

  {--Assure this service is available}
    DefaultRegisters ( IRR );
    With IRR Do Begin
      AX := $3000;
      If IsamDOSError = 0 Then IsamDOSFunc := AX;
      CallMsDos ( IRR );
        {-DOS 3.3 or greater?}
      If Odd (Flags) And (IsamDOSError = 0) Then IsamDOSError := AX;
      If Swap ( AX ) < $031E Then Begin
        IsamOK := False;
        IsamError := 10190;
        Exit;
      End;
    End;

    If NumHandles >= 255 Then NumHandles := 254;

 {$IFNDEF DPMI}
  {--Calculate number of paragraphs needed (plus small safety margin)}
    ParasNeeded := (Word (Succ (NumHandles)) + Safety + 15) Shr 4;

    If ParasNeeded > FreeRamInParas Then Begin
      {-Not enough free DOS RAM, so shrink the heap}
      If Not GetDosMemory ( ParasNeeded ) Then Begin
        IsamOK := False;
        IsamError := 10191;
        Exit;
      End;
    End;
 {$ENDIF}

    FirstTrial := True;
    Continue := True;
    While Continue Do Begin
      DefaultRegisters ( IRR );
      With IRR Do Begin
        AX := $6700;
        BX := NumHandles;
        If IsamDOSError = 0 Then IsamDOSFunc := AX;
        CallMsDos ( IRR );
        If Odd (Flags) Then Begin
          If IsamDOSError = 0 Then IsamDOSError := AX;
          If FirstTrial Then Begin
            IsamClearOK;
          End Else Begin
            IsamOK := False;
            IsamError := 10192;
            Continue := False;
          End;
        End Else Begin
          Continue := False;
        End;
      End;
      FirstTrial := False;

    {--Adjust for DOS 3.3 bug - number of handles must either be even or odd,
       depending on the origin}
      Inc (NumHandles);
    End;
  End;
{$ENDIF}
{$ENDIF}

  Function IsamErrorMessage ( ErrorNr : Integer ) : String;
    {-Returns an error describing string}

  Var
    S : String [10];

  Begin
    Case UseErrorMessage Of
      NoMsg : Begin
        IsamErrorMessage := '';
      End;
    {$IFDEF GermanMessage}
      German : Begin
        Case ErrorNr Of
           9500..9899 : Begin
             Str ( ErrorNr - 9500 : 0, S );
             IsamErrorMessage := 'DOS Fehlercode <' + S + '>';
           End;
           9900 : IsamErrorMessage := 'Falscher Pfadname';
           9901 : IsamErrorMessage := 'Zu viele offene Dateien';
           9903 : IsamErrorMessage := 'Datei nicht gefunden';
           9904 : IsamErrorMessage := 'Falsche Dateikennzeichnung (Handle)';
           9908 : IsamErrorMessage := 'Falscher Zugriffscode (open handle)';
          10000 : IsamErrorMessage :=
              'Seitenpuffer ist kleiner als MaxHeight';
          10001..
          10009 : IsamErrorMessage := 'Schwerer IO-Fehler im Save-Modus';
          10010 : IsamErrorMessage :=
              'Dateidefekt, der durch Reindizieren zu beheben ist';
          10020 : IsamErrorMessage :=
              'Datensatz kleiner als 22 Bytes oder mehr als 2147483647 Bytes';
          10030 : IsamErrorMessage :=
              'Zu wenig Speicherplatz (Fileblock-Beschreiber)';
          10040 : IsamErrorMessage := 'Nicht genug Speicher vorhanden';
          10050 : IsamErrorMessage := 'Falsche Anzahl Keys';
          10055,
          10125 : IsamErrorMessage := 'Key zu lang';
          10060 : IsamErrorMessage :=
              'Zu viele Keys oder Datei-Lesefehler';
          10065 : IsamErrorMessage :=                           {!!.41}
              'Schreibversuch in Nur-Lese-Fileblock';
          10070 : IsamErrorMessage := 'Datei-Lesefehler';
          10075 : IsamErrorMessage := 'Datei-Schreibfehler';
          10080 : IsamErrorMessage := 'Fileblock nicht offen';
          10090,
          10100,
          10356 : IsamErrorMessage := 'Zu wenig Speicherplatz (Fileblock)';
          10110 : IsamErrorMessage := 'Laufwerk nicht bereit';
          10120 : IsamErrorMessage := 'PageSize nicht zutreffend';
          10121 : IsamErrorMessage := 'MaxKeyLen zu klein';
          10122 : IsamErrorMessage := 'MaxPageSize zu klein';        {!!.42}
          10130,
          10135 : IsamErrorMessage := 'Record 0 darf nicht benutzt werden';
          10140 : IsamErrorMessage :=
              'Zugriff verweigert, undefinierter Fehler';
          10150 : IsamErrorMessage := 'Zu wenig Handles (Flush im Netz)';
          10160 : IsamErrorMessage := 'Fileblock nicht korrekt geschlossen';
          10164 : IsamErrorMessage := 'Falsche Key-Nummer';
          10170 : IsamErrorMessage := 'Konsistenz des Fileblocks in Gefahr';
          10180 : IsamErrorMessage :=
              'Reparatur-Versuch des Fileblocks gescheitert';
          10190 : IsamErrorMessage := 'DOS-Version 3.3 notwendig';
          10191 : IsamErrorMessage :=
              'Zu wenig Speicher, um neue Dateitabelle anzulegen';
          10192 : IsamErrorMessage :=
              'Initialisierung der neuen Dateitabelle gescheitert';
          10200 : IsamErrorMessage := 'Kein passender Key gefunden';
          10205 : IsamErrorMessage := 'Datensatz momentan gesperrt';
          10210 : IsamErrorMessage :=
              'Dieser und kein nachfolgender Key vorhanden';
          10215 : IsamErrorMessage :=
              'Reindizieren gescheitert, die Datendatei ist inkonsistent';
                                                                     {!!.50}
          10220 : IsamErrorMessage := 'Zu entfernender Key nicht gefunden';
          10230 : IsamErrorMessage :=
              'Doppelter Key kann nicht eingetragen werden';
          10240,
          10250 : IsamErrorMessage := 'Kein nachfolgender Key gefunden';
          10245,
          10260 : IsamErrorMessage := 'Kein kleinerer Key gefunden';
          10255,
          10265 : IsamErrorMessage := 'Sequentieller Zugriff nicht erlaubt';
          10270 : IsamErrorMessage := 'Keinen passenden '
              + 'Key mit dieser Datensatzreferenz gefunden';
          10275 : IsamErrorMessage := 'Keine passende Referenz gefunden';
                                                                     {!!.42}
          10280,
          10285 : IsamErrorMessage := 'Index leer';
          10306 : IsamErrorMessage := 'Zu viele Fileblock-Zugriffe';
          10310 : IsamErrorMessage := 'Netz-Initialisierungs-Fehler';
          10315 : IsamErrorMessage := 'Netz-Deinitialisierungs-Fehler';
          10322,
          10323 : IsamErrorMessage := 'Close des Fileblocks wegen nicht '
              + 'entfernbarem Lock fehlgeschlagen';
          10330 : IsamErrorMessage := 'Fileblock Lock-Versuch gescheitert';
          10332 : IsamErrorMessage := 'Fileblock Readlock-Versuch gescheitert';
          10335 : IsamErrorMessage := 'Datensatz Lock-Versuch gescheitert';
          10337 : IsamErrorMessage := 'Datensatz Lock-Versuch aus '
              + 'Speicherplatzmangel gescheitert';
          10340..
          10342 : IsamErrorMessage := 'Fileblock Unlock-Versuch gescheitert';
          10345 : IsamErrorMessage := 'Datensatz Unlock-Versuch gescheitert';
          10355,
          10390,                                                     {!!.42}
          10397,
          10399 : IsamErrorMessage := 'Ein Lock verhindert die Operation';
          10360 : IsamErrorMessage := 'Reparatur beim Oeffnen nicht moeglich,'
              + 'da Fileblock nicht im alleinigen Zugriff';          {!!.42}
          10398 : IsamErrorMessage := 'Operation erfordert einen Fileblock '
              + 'im Lock-Zustand';
          10410 : IsamErrorMessage := 'Daten-Datei nicht gefunden';
          10411 : IsamErrorMessage := 'Puffer wegen zu wenig Heap nicht '
              + 'erstellt';
          10412 : IsamErrorMessage := 'Datensatz mehr als 64K Bytes';
          10415 : IsamErrorMessage := 'Datensatz-Anzahl zu hoch';
          10420,
          10425 : IsamErrorMessage :=
              'Relative Position oder Scalierung falsch';
          10430 : IsamErrorMessage :=
              'Reparatur wegen "read only" gescheitert';
          10435 : IsamErrorMessage := 'Baumseite mehr als 16K';
          10440 : IsamErrorMessage :=
              'Dialogdatei anlegen wegen "read only" gescheitert';
          10445 : IsamErrorMessage := 'Kein Fileblock vorhanden';
          10446 : IsamErrorMessage := 'Rekursiver Aufruf der B-Tree Isam';
          10450 : IsamErrorMessage := 'Initialisierung bereits geschehen';
          10455 : IsamErrorMessage :=
              'Initialisierung noch nicht geschehen';
          10460 : IsamErrorMessage := 'Reorganisation abgebrochen';
          10465 : IsamErrorMessage :=                                {!!.41}
              'Daten- und Sicherungsdatei gleichzeitig vorhanden';
          10470 : IsamErrorMessage :=                                {!!.50}
              'Fehler bei der Schluesselbildung';
          10475 : IsamErrorMessage := 'Fehler beim Datensatzumbau';  {!!.50}
          10480 : IsamErrorMessage :=                                {!!.42}
              'Netzmodus fuer diesen Fileblock nicht moeglich';
        Else
          IsamErrorMessage := '';
        End;
      End;
    {$ENDIF}
    {$IFDEF EnglishMessage}
      English : Begin
        Case ErrorNr Of
           9500..9899 : Begin
             Str ( ErrorNr - 9500 : 0, S );
             IsamErrorMessage := 'DOS error code <' + S + '>';
           End;
           9900 : IsamErrorMessage := 'Invalid path name';
           9901 : IsamErrorMessage := 'Too many open files';
           9903 : IsamErrorMessage := 'File not found';
           9904 : IsamErrorMessage := 'Invalid handle';
           9908 : IsamErrorMessage := 'Invalid access code';
          10000 : IsamErrorMessage :=
              'Page buffer is smaller than MaxHeight';
          10001..
          10009 : IsamErrorMessage :=
              'Serious I/O error occurred while in save mode';
          10010 : IsamErrorMessage :=
              'File defect, that can be corrected by reindexing';
          10020 : IsamErrorMessage :=
              'Record length less than 22 or more than 2147483647 bytes';
          10030,
          10040 : IsamErrorMessage := 'Insufficient memory';
          10050 : IsamErrorMessage := 'Invalid number of keys specified';
          10055,
          10125 : IsamErrorMessage := 'Key too long';
          10060 : IsamErrorMessage := 'Too many keys, or file read error';
          10065 : IsamErrorMessage :=                                {!!.41}
              'Attempt to write to read only Fileblock';
          10070 : IsamErrorMessage := 'File read error';
          10075 : IsamErrorMessage := 'File write error';
          10080 : IsamErrorMessage := 'Fileblock not open';
          10090,
          10100,
          10356 : IsamErrorMessage :=
              'Insufficient memory to allocate Fileblock';
          10110 : IsamErrorMessage := 'Drive not ready';
          10120 : IsamErrorMessage := 'PageSize incompatible with Fileblock';
          10121 : IsamErrorMessage := 'MaxKeyLen too small';
          10122 : IsamErrorMessage := 'MaxPageSize too small';       {!!.42}
          10130,
          10135 : IsamErrorMessage := 'Record 0 must not be accessed';
          10140 : IsamErrorMessage := 'Access not possible, undefined error';
          10150 : IsamErrorMessage :=
              'Insufficient handles for flush of network Fileblock';
          10160 : IsamErrorMessage := 'Fileblock not correctly closed';
          10164 : IsamErrorMessage := 'Invalid key number';
          10170 : IsamErrorMessage := 'Consistency of Fileblock endangered';
          10180 : IsamErrorMessage := 'Attempt to repair Fileblock failed';
          10190 : IsamErrorMessage :=
              'Extend handle function requires DOS 3.3 or later';
          10191 : IsamErrorMessage :=
              'Insufficient memory for new file handle table';
          10192 : IsamErrorMessage :=
              'Unable to initialize new file handle table';
          10200 : IsamErrorMessage := 'No matching key found';
          10205 : IsamErrorMessage := 'Data record is currently locked';
          10210 : IsamErrorMessage :=
              'No key found and no larger keys available';
          10215 : IsamErrorMessage :=                                {!!.50}
              'Reindex failed, the data file is inconsistent';       {!!.50}
          10220 : IsamErrorMessage := 'Key to delete was not found';
          10230 : IsamErrorMessage := 'Cannot add duplicate key';
          10240,
          10250 : IsamErrorMessage := 'No larger key found';
          10245,
          10260 : IsamErrorMessage := 'No smaller key found';
          10255,
          10265 : IsamErrorMessage := 'Sequential access not allowed';
          10270 : IsamErrorMessage :=
              'No matching key and record number found';
          10275 : IsamErrorMessage := 'No matching reference found'; {!!.42}
          10280,
          10285 : IsamErrorMessage := 'Index empty';
          10306 : IsamErrorMessage := 'Too many Fileblock accesses';
          10310 : IsamErrorMessage := 'Network initialization error';
          10315 : IsamErrorMessage := 'Network exit error';
          10322,
          10323 : IsamErrorMessage := 'Close of Fileblock not possible because'
              + ' of a non-removeable lock';
          10330 : IsamErrorMessage := 'Fileblock lock attempt failed';
          10332 : IsamErrorMessage := 'Fileblock readlock attempt failed';
          10335 : IsamErrorMessage := 'Record lock attempt failed';
          10337 : IsamErrorMessage := 'Record lock attempt failed '
              + 'because of insufficient memory';
          10340..
          10342 : IsamErrorMessage := 'Fileblock unlock attempt failed';
          10345 : IsamErrorMessage := 'Record unlock attempt failed';
          10355,
          10390,                                                     {!!.42}
          10397,
          10399 : IsamErrorMessage := 'A lock prevents the operation';
          10360 : IsamErrorMessage := 'No repair during open, because the'
              + 'Fileblock is not exclusively used';                 {!!.42}
          10398 : IsamErrorMessage := 'Fileblock must be locked';
          10410 : IsamErrorMessage := 'Data file not found';
          10411 : IsamErrorMessage :=
              'Insufficient heap space for work buffers';
          10412 : IsamErrorMessage := 'Record section length exceeds 64K';
          10415 : IsamErrorMessage := 'Too much records';
          10420,
          10425 : IsamErrorMessage := 'Relative position or scale invalid';
          10430 : IsamErrorMessage := 'No repair in read only mode';
          10435 : IsamErrorMessage := 'Tree page greater than 16k';
          10440 : IsamErrorMessage :=
              'No creating of dialog file in read only mode';
          10445 : IsamErrorMessage := 'Invalid Fileblock';
          10446 : IsamErrorMessage := 'Recursive call of B-Tree Isam';
          10450 : IsamErrorMessage := 'Initialization already done';
          10455 : IsamErrorMessage := 'Initialization not yet done';
          10460 : IsamErrorMessage := 'Reorganization aborted';
          10465 : IsamErrorMessage :=                                {!!.41}
              'Data and save file exist simultaneously';
          10470 : IsamErrorMessage :=                                {!!.50}
              'Error while building a key';
          10475 : IsamErrorMessage :=                                {!!.50}
              'Error while restructing the data record';
          10480 : IsamErrorMessage :=                                {!!.42}
              'Fileblock cannot be opened in net mode';
        Else
          IsamErrorMessage := '';
        End;
      End;
  {$ENDIF}
    Else
      IsamErrorMessage := '';
    End;
  End;


{$IFDEF LengthByteKeys}
  Procedure InvertString ( Var Dest   : String;
                               Source : String;
                               MaxLen : Byte );
    {-Inverts the string Source under a maximum length of MaxLen returning
      the result in Dest}

  Var
    i : integer;
    ToSubLen,
    EatCount,
    ResLen,
    ActInd   : Byte;
    Cont     : Boolean;

  Begin
    ToSubLen := Length (Source);
    If MaxLen < ToSubLen Then Begin
      Dest := '';
      Exit;
    End;
    FillChar ( Source [Succ (ToSubLen)], MaxLen - ToSubLen, $FF );
    If ToSubLen = 0 Then Begin
      Source [0] := Chr (MaxLen);
      Dest := Source;
      Exit;
    End;
    {$IFDEF Win32}
    for i := 1 to ToSubLen do
      Source[i] := char(not ord(Source[i]));
    {$ELSE}
    Inline (
      $8A / $8E / ToSubLen /                         {Mov CL, [BP+ToSubLen]}
      $30 / $ED /                                    {Xor CH, CH}
      $BF / Source + 1 /                             {Mov DI, Ofs (Source [1])}
      $F6 / $13 /                                    {L1: Not [BP+DI]}
      $47 /                                          {Inc DI}
      $E2 / $FB                                      {Loop L1}
    );
    {$ENDIF}
      {-Invert string entries up to ToSubLen}
    ResLen := MaxLen;
    If Ord (Source [MaxLen]) >= ToSubLen Then Begin
      Dec (Source [MaxLen], ToSubLen);
    End Else Begin
      If Ord (Succ (Source [MaxLen])) = ToSubLen Then Begin
        ResLen := Pred (MaxLen);
      End Else Begin
        Inc (Source [MaxLen], 257 - ToSubLen);
        EatCount := Ord (Source [MaxLen]);
        ActInd := Pred (MaxLen);
        Cont := True;
        While Cont And (ActInd > 0) Do Begin
          If Source [ActInd] > #0 Then Begin
            Dec (Source [ActInd]);
            Cont := False;
          End Else Begin
            If EatCount <= 255 Then Begin                              {!!.52}
              Source [ActInd] := #255;
              Dec (ActInd);
              Inc (EatCount);
            End Else Begin
              ResLen := Pred (ActInd);
              Cont := False;
            End;
          End;
        End;
        If ActInd = 0 Then ResLen := 0;
      End;
    End;
    Source [0] := Chr (ResLen);
    Dest := Source;
  End;
{$ENDIF}


{$IFDEF ASCIIZeroKeys}
  Procedure InvertString ( Var Dest;
                           Var Source;
                               MaxLen : Byte );
    {-Inverts the null terminated array of character Source under a
      maximum length of MaxLen returning the result in Dest}
  Var
    DestStr   : Array [Byte] Of Byte Absolute Dest;
    SourceStr : Array [Byte] Of Byte Absolute Source;
    ToSubLen,
    EatCount,
    ResLen    : Byte;
    ActInd    : Integer;
    Cont      : Boolean;


    Function AZStrLen ( StrPtr : Pointer ) : Word;
      {-Returns the length of the ASCII zero string}
    {$IFDEF Win32}
    register;
    asm
      push edi
      mov edi, eax
      xor eax, eax
      mov ecx, $FFFF
      repne scasb
      mov eax, $FFFE
      sub eax, ecx
      pop edi
    end;
    {$ELSE}
    Begin
      Inline (
        $FC /                                          {Cld}
        $C4 / $7E / < StrPtr /                         {Les, DI [BP+StrPtr]}
        $B9 / $FF / $FF /                              {Mov CX, $FFFF}
        $30 / $C0 /                                    {Xor AL, AL}
        $F2 / $AE /                                    {Repnz Scasb}
        $B8 / $FE / $FF /                              {Mov AX, $FFFE}
        $29 / $C8                                      {Sub AX, CX}
      );
    End;
    {$ENDIF}


    Procedure SubFrom256 ( StrPtr : Pointer; Len : Byte );
      {-Subtracts Len Bytes at StrPtr from 256 and stores the result there}
    {$IFDEF Win32}
    register;
    asm
      movzx ecx, dl
    @@Again:
      not [eax]
      inc eax
      dec ecx
      jnz @@Again
    end;
    {$ELSE}
    Begin
      Inline (
        $C4 / $7E / < StrPtr /                         {Les, DI [BP+StrPtr]}
        $8A / $8E / Len /                              {Mov CL, [BP+Len]}
        $30 / $ED /                                    {Xor CH, CH}
        $26 / $F6 / $1D /                              {L1: Neg [ES:DI]}
        $47 /                                          {Inc DI}
        $E2 / $FA                                      {Loop L1}
      );
    End;
    {$ENDIF}


  Begin
    ToSubLen := AZStrLen ( @SourceStr );
    If MaxLen < ToSubLen Then Begin
      DestStr [0] := 0;
      Exit;
    End;
    Move ( SourceStr, DestStr, ToSubLen );
    FillChar ( DestStr [ToSubLen], MaxLen - ToSubLen, $FF );
    DestStr [MaxLen] := 0;

    If ToSubLen = 0 Then Exit;
    SubFrom256 ( @DestStr, ToSubLen );

    ResLen := MaxLen;
    If DestStr [Pred (MaxLen)] > ToSubLen Then Begin
      Dec (DestStr [Pred (MaxLen)], ToSubLen);
    End Else Begin
      If DestStr [Pred (MaxLen)] = ToSubLen Then Begin
        ResLen := Pred (MaxLen);
      End Else Begin
        Inc (DestStr [Pred (MaxLen)], 256 - ToSubLen);
        EatCount := DestStr [Pred (MaxLen)];
        ActInd := MaxLen - 2;
        Cont := True;
        While Cont And (ActInd >= 0) Do Begin
          If DestStr [ActInd] > 1 Then Begin
            Dec (DestStr [ActInd]);
            Cont := False;
          End Else Begin
            If EatCount <= 255 Then Begin                              {!!.52}
              DestStr [ActInd] := 255;
              Dec (ActInd);
              Inc (EatCount);
            End Else Begin
              ResLen := ActInd;
              Cont := False;
            End;
          End;
        End;
        If ActInd = -1 Then ResLen := 0;
      End;
    End;
    DestStr [ResLen] := 0;
  End;
{$ENDIF}

{$IFDEF InitAllUnits}
Begin
{$ENDIF}
End.
