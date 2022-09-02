{********************************************************************}
{* FILER.PAS - B-Tree Filer main unit                               *}
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
 *   Rob Roberts  robr@pcisys.net
 *
 * ***** END LICENSE BLOCK ***** *)

{--Conditional defines and compiler options}
{$I BTDefine.Inc}

{$IFDEF CanAllowOverlays}
  {$O+,F+}
{$ENDIF}

{$IFDEF Win32}
{$H-} {no long string support}
{$ENDIF}

Unit
  Filer;

interface

Uses
  BTBase,
  BTFileIO,
  BTIsBase,
{$IFDEF Win32}
  Windows, SysUtils;                                            {!!.52}
{$ELSE}                                                         {!!.52}
{$IFDEF UseEMSHeap}
  EMSHeap,
{$ENDIF}
{$IFDEF UsingDelphi}
  SysUtils,
{$ENDIF}
{$IFDEF SupportVLM}
  nwBase, nwBind, nwConn, nwFile,
{$ENDIF}
  BaseSupp;
{$ENDIF}                                                        {!!.52}


(***************************************************************************)
  {-Beyond this point there are no user changeable settings}

{--Interfaced constants}
Const
  VersionStr = '05.57a';                                        {!!.57a}
    {-Current version}

  MinimizeUseOfNormalHeap = $40000000;
    {-This value added to Free for BTInitIsam causes the ring buffer to
      use a minimum of normal heap}

  MaxLockEntries = 4;
    {-Maxiximum number of entries for record locks per level. Multiple levels
      guaranty as much as the heap allows}

{--Interfaced types}
Type
  IsamFileBlockName = String [192];
    {-DOS name of a fileblock}

  IsamKeyStr = String [MaxKeyLen];
    {-A key string}

  IsamFileBlockPtr = ^IsamFileBlock;
    {-Pointer to a fileblock}

  IsamIndDescr = Array [1..MaxNrOfKeys] Of packed Record
    KeyL      : 1..MaxKeyLen;
    AllowDupK : Boolean;
  End;
    {-Descriptor of the index file}

  ProcBTCharConvert = Procedure ( DataPtr  : Pointer;                {!!.50}
                                  DataLen  : LongInt;
                                  PostRead : Boolean;
                                  HookPtr  : Pointer );

  TIsamReXUserProc = procedure (KeyNr : integer;
                                NumRecsRead : longint;
                                NumRecsWritten : longint;
                            var Data;
                                Len : word);
    {-ReStruct/ReIndex user progress procedural type}

{--Privat types}
  IsamItem = packed Record
    DataRef,
    PageRef : LongInt;
    KeyStr  : IsamKeyStr;
  End;
  IsamPage = packed Record
    ItemsOnPage : Word;    {0..MaxPageSize, but must occupy 2 Byte}
    BckwPageRef : LongInt;
    ItemArray   : Array [1..MaxPageSize] Of IsamItem;                {!!.42}
  End;
  IsamPagePtr = ^IsamPage;
  IsamSearchStep = packed Record
    PageRef    : LongInt;
    ItemArrInd : 0..MaxPageSize;                                     {!!.42}
  End;
  IsamPath = Array [1..MaxHeight] Of IsamSearchStep;
  IsamDatIndDescr = packed Record
    NumKeys,
    FirstFree,
    NumberFree,
    NumRec,
    LenRec,
    RootRef        : LongInt;
    UsedPageSize   : Word;                                           {!!.42}
    AllowDupKeys,
    InfoRecChanged,
    IRChangedSaveN,
    FirstFreeChanged,
    SearchForSequentialEnabled,
    SequentialOK   : Boolean;
    KeyLen         : 1..MaxKeyLen;
    BlockOfs       : LongInt;
    Path           : IsamPath;
    PathInd        : 0..MaxHeight;
  End;
  IsamDatIndDescrPtr = ^IsamDatIndDescr;
  IsamDatIndDescrAr = Array [0..MaxNrOfKeys] Of IsamDatIndDescrPtr;
  IsamLockEntryRecPtr = ^IsamLockEntryRec;
  IsamLockEntryRec = packed Record
    EntryArr : Array [1..MaxLockEntries] Of LongInt;
    Count    : 0..MaxLockEntries;
    Next     : IsamLockEntryRecPtr;
  End;
  IsamNetSupport = packed Record
    Locked,
    ReadLocked,
    FlagSetReadLocked,
    AllStationsReadOnly,
    ReloadAll,
    SaveFileBlockRepaired : Boolean;
    DiaLenM3              : LongInt;
    SetLen                : Word;
    LocalWSNr             : Word;
    SupNrOfWS             : Word;                                   {!!.42}
    LockEntryRec          : IsamLockEntryRec;
  End;
  IsamNetSupportPtr = ^IsamNetSupport;
  IsamFileBlock = packed Record
    ValidSign    : LongInt;
    DatF         : IsamFile;
    IndF         : IsamFile;
    DiaF         : IsamFile;
    NrOfKeys     : 0..MaxNrOfKeys;
    BlockLen,
    MaxPages     : LongInt;
    DIDPtr       : ^IsamDatIndDescrAr;
    DataBuffered,
    ReadOnlyFB,
    SaveFB       : Boolean;
    NSP          : IsamNetSupportPtr;
    CharConvProc : ProcBTCharConvert;                                {!!.50}
    CCHookPtr    : Pointer;                                          {!!.50}
    CCDestrWrite : Boolean;                                          {!!.50}
  End;
  IsamOpenFileBlockListPtr = ^IsamOpenFileBlockList;
  IsamOpenFileBlockList = packed Record
    Next    : IsamOpenFileBlockListPtr;
    OIFBPtr : IsamFileBlockPtr;
  End;
  IsamSmallInfoRec = packed Record
    Gener : Array [1..5] Of LongInt;
    ADK   : Boolean;
  End;
  IsamInfoRec = packed Record
    InfoRec      : IsamSmallInfoRec;
    DummyFill    : Char;
    KeysUsed     : LongInt;{Must start on an even offset for C-compatibility}
    PageSizeUsed : Word;                                             {!!.42}
  End;
  IsamSaveInfoRecBuffer = packed Record
    IST,
    IndNr : Word;
    BIR   : IsamInfoRec;
    SFF   : LongInt;
  End;
  IsamRingBufferRecPtr = ^IsamRingBufferRec;
  IsamPageEntryPtr = ^IsamPageEntry;
  IsamRingBufferRec = packed Record
    Prev,
    Next         : IsamRingBufferRecPtr;
    IFBlPtr      : IsamFileBlockPtr;
    PageRef      : LongInt;
    PageEntryPtr : IsamPageEntryPtr;
    KeyNr        : Word;
    UpDated,
    EMSEntry,
    SaveBuffered : Boolean;
  End;
  IsamPageEntry = packed Record
    Page          : IsamPage;
    DummyDist1,
    DummyDist2    : LongInt;
    RingBufferPtr : IsamRingBufferRecPtr;
  End;
  ILI = packed Record
    Lo, Hi : Word;
  End;
  IsamFlagSet = Set Of Byte;

Const
  IsamSmallInfoRecSize = SizeOf (IsamSmallInfoRec);
  IsamPageEntrySize = SizeOf (IsamPageEntry);
    {-Compatibility constants to DLL version}

{--Interfaced variables}
Var
  IsamReXUserProcPtr : TIsamReXUserProc;
    {-Pointer to a routine used while rebuilding and reorganizing}

  IsamCompiledNets : Set Of NetSupportType;
    {-Initialized of the unit initialization code;
      contains the definined network interfaces}

{--Interfaced routines}
  {$IFDEF UseWindowsInit}
  Function BTInitIsam ( ExpectedNet : NetSupportType;        {!!.52}
                        Pages       : integer ) : integer;
    {-Initializes B-Tree Isam;
      Result -- pages on normal heap}
  {$ELSE}
  Function BTInitIsam ( ExpectedNet      : NetSupportType;
                        Free             : LongInt;
                        NrOfEMSTreePages : Word ) : LongInt;
    {-Initializes B-Tree Isam;
      high word of result: pages on EMS heap; low word: pages on normal heap}
  {$ENDIF}

  Procedure BTExitIsam;
    {-Exits use of B-Tree Isam}

  Function BTIsamErrorClass : Integer;
    {-Evaluates IsamError in error classes 0..4}

  Procedure BTReadLockFileBlock ( IFBPtr : IsamFileBlockPtr );
    {-Readlocks the fileblock IFBPtr^}

  Procedure BTLockFileBlock ( IFBPtr : IsamFileBlockPtr );
    {-Locks the fileblock IFBPtr^}

  Procedure BTUnLockFileBlock ( IFBPtr : IsamFileBlockPtr );
    {-Unlocks the fileblock IFBPtr^}

  Procedure BTReadLockAllOpenFileBlocks;
    {-Readlocks all open fileblocks}

  Procedure BTLockAllOpenFileBlocks;
    {-Locks all open fileblocks}

  Procedure BTUnLockAllOpenFileBlocks;
    {-Unlocks all open fileblocks}

  Procedure BTLockRec ( IFBPtr : IsamFileBlockPtr; Ref : LongInt );
    {-Locks record Ref}

  Procedure BTUnLockRec ( IFBPtr : IsamFileBlockPtr; Ref : LongInt );
    {-Unlocks record Ref}

  Procedure BTUnLockAllRecs ( IFBPtr : IsamFileBlockPtr );
    {-Unlocks all locked records of IFBPtr^}

  Function BTRecIsLocked ( IFBPtr : IsamFileBlockPtr; Ref : LongInt )
                                                                    : Boolean;
    {-Returns whether the record Ref is locked}

  Function BTPeekRecIsLocked ( IFBPtr : IsamFileBlockPtr; Ref : LongInt )
                                                                    : Boolean;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Function BTaRecIsLocked ( IFBPtr : IsamFileBlockPtr ) : Boolean;
    {-Returns whether a record is locked}

  Function BTPeekaRecIsLocked ( IFBPtr : IsamFileBlockPtr ) : Boolean;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Procedure BTInformTTSAbortSuccessful ( IFBPtr : IsamFileBlockPtr );
    {-Resets all internal data to nothing buffered and no lock at all}

  Procedure BTGetRecordInfo (     IFBPtr     : IsamFileBlockPtr;
                                  Ref        : LongInt;
                              Var Start, Len : LongInt;
                              Var Handle     : IsamHandle);
    {-Returns information about record Ref}

  Procedure BTPeekGetRecordInfo (     IFBPtr     : IsamFileBlockPtr;
                                      Ref        : LongInt;
                                  Var Start, Len : LongInt;
                                  Var Handle     : IsamHandle);
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Function BTFileBlockIsOpen ( IFBPtr : IsamFileBlockPtr ) : Boolean;
    {-Returns True, if the fileblock is curently open}

  Function BTPeekFileBlockIsOpen ( IFBPtr : IsamFileBlockPtr ) : Boolean;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Function BTFileBlockIsLocked ( IFBPtr : IsamFileBlockPtr ) : Boolean;
    {-Returns True, if the fileblock is currently locked}

  Function BTPeekFileBlockIsLocked ( IFBPtr : IsamFileBlockPtr ) : Boolean;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Function BTFileBlockIsReadLocked ( IFBPtr : IsamFileBlockPtr ) : Boolean;
    {-Returns True, if the fileblock is currently readlocked}

  Function BTPeekFileBlockIsReadLocked ( IFBPtr : IsamFileBlockPtr )
                                                                   : Boolean;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Procedure BTForceWritingMark ( FFM : Boolean );
    {-Forces to write the "modify mark" to disk}

  Function BTSetDosRetry ( NrOfRetries, WaitTime : Integer ) : Boolean;
    {-Sets number of retries and delay time between on locks and accesses
      in a MicroSoft compatible network}

  Function BTNetSupported : NetSupportType;
    {-Returns the currently supported network}

  Function BTPeekNetSupported : NetSupportType;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Function BTNoNetCompiled : Boolean;
    {-Returns True, if B-Tree Isam was compiled without network support}

  Function BTPeekNoNetCompiled : Boolean;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Function BTIsNetFileBlock ( IFBPtr : IsamFileBlockPtr ) : Boolean;
    {-Returns True, if the fileblock is a net fileblock}

  Function BTPeekIsNetFileBlock ( IFBPtr : IsamFileBlockPtr ) : Boolean;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Procedure BTForceNetBufferWriteThrough ( DoIt : Boolean );
    {-With DoIt = True the save mode tries to suppress all buffering in a
      network or multitasking environment}

  Procedure BTPutRec (     IFBPtr  : IsamFileBlockPtr;
                           RefNr   : LongInt;
                       Var Source;
                           ISOLock : Boolean );
    {-Puts the specified record at the given reference}

  Procedure BTGetRec (     IFBPtr  : IsamFileBlockPtr;
                           RefNr   : LongInt;
                       Var Dest;
                           ISOLock : Boolean );
    {-Gets the specified record from the given reference}

  Procedure BTGetRecReadOnly (     IFBPtr  : IsamFileBlockPtr;
                                   RefNr   : LongInt;
                               Var Dest              );
    {-Gets the specified record from the given reference even if it's locked}

  Procedure BTGetStartingLong (     IFBPtr  : IsamFileBlockPtr;
                                    RefNr   : LongInt;
                                Var Dest    : LongInt );
    {-Gets the first four bytes of the specified record with the given
      reference}

  Procedure BTAddRec (     IFBPtr : IsamFileBlockPtr;
                       Var RefNr  : LongInt;
                       Var Source                );
    {-Adds a record to the given fileblock and returns the reference}

  Procedure BTDeleteRec ( IFBPtr : IsamFileBlockPtr; RefNr : LongInt );
    {-Deletes the record with reference RefNr}

  Function BTUsedRecs ( IFBPtr : IsamFileBlockPtr ) : LongInt;
    {-Returns the number of currently used records}

  Function BTUsedKeys ( IFBPtr : IsamFileBlockPtr; Key : Word ) : LongInt;
    {-Returns the number of currently used keys of the specified number}

  Function BTFreeRecs ( IFBPtr : IsamFileBlockPtr ) : LongInt;
    {-Returns the number of currently not used records}

  Function BTFileLen ( IFBPtr : IsamFileBlockPtr ) : LongInt;
    {-Returns the number of currently occupied records by this fileblock}

  Function BTNrOfKeys ( IFBPtr : IsamFileBlockPtr ) : Word;
    {-Returns the number of defined keys}

  Function BTPeekNrOfKeys ( IFBPtr : IsamFileBlockPtr ) : Word;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Function BTDatRecordSize ( IFBPtr : IsamFileBlockPtr ) : LongInt;
    {-Returns the lenght of a data record}

  Function BTPeekDatRecordSize ( IFBPtr : IsamFileBlockPtr ) : LongInt;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Function BTKeyRecordSize ( IFBPtr : IsamFileBlockPtr ) : LongInt;
    {-Returns the maximum number of bytes used for a following key adding}

  Function BTPeekKeyRecordSize ( IFBPtr : IsamFileBlockPtr ) : LongInt;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Function BTMinimumDatKeys ( IFBPtr : IsamFileBlockPtr; Space : LongInt )
                                                                  : LongInt;
    {-Returns the minimum number of data (including keys), that can be placed
      on a disk having Space bytes free space}

  Function BTPeekMinimumDatKeys ( IFBPtr : IsamFileBlockPtr;
                                  Space  : LongInt ) : LongInt;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Procedure BTSetSearchForSequential ( IFBPtr : IsamFileBlockPtr;
                                       Key    : Word;
                                       ToOn   : Boolean );
    {-Sets search mode in sequential key operations to ToOn}

  Procedure BTGetSearchForSequential (     IFBPtr : IsamFileBlockPtr;
                                           Key    : Word;
                                       Var SFS    : Boolean );
    {-Gets search mode for sequential key operations in TSFS}

  Procedure BTFindRecRef (     IFBPtr                  : IsamFileBlockPtr;
                           Var UserDatRef              : LongInt;
                               NotFoundSearchDirection : Integer );  {!!.42}
    {-Returns in UserDatRef the reference number of the found record;
      NotFoundSearchDirection determines whether and how to search
      in case nothing was found}

  Procedure BTNextRecRef (     IFBPtr     : IsamFileBlockPtr;
                           Var UserDatRef : LongInt );               {!!.42}
    {-Returns in UserDatRef the reference number of the next record}

  Procedure BTPrevRecRef (     IFBPtr     : IsamFileBlockPtr;
                           Var UserDatRef : LongInt );               {!!.42}
    {-Returns in UserDatRef the reference number of the previous record}

  Procedure BTGetApprRecPos (     IFBPtr     : IsamFileBlockPtr;
                              Var RelPos     : Word;
                                  Scale      : Word;
                                  UserDatRef : LongInt );            {!!.42}
    {-Returns in RelPos the approximate relative position of the record
      with reference UserDatRef in 0..Scale}

  Procedure BTGetApprRecRef (     IFBPtr     : IsamFileBlockPtr;
                                  RelPos     : Word;
                                  Scale      : Word;
                              Var UserDatRef : LongInt );            {!!.42}
    {-Returns UserDatRef, that is approximately at the position RelPos
      in 0..Scale}

  Procedure BTClearKey ( IFBPtr : IsamFileBlockPtr; Key : Word );
    {-Places the internal sequential pointer to a null position}

  Procedure BTNextKey (     IFBPtr     : IsamFileBlockPtr;
                            Key        : Word;
                        Var UserDatRef : LongInt;
                        Var UserKey    : IsamKeyStr    );
    {-Returns in UserKey / UserDatRef the value and reference number of the
      next key}

  Procedure BTPrevKey (     IFBPtr     : IsamFileBlockPtr;
                            Key        : Word;
                        Var UserDatRef : LongInt;
                        Var UserKey    : IsamKeyStr       );
    {-Returns in UserKey / UserDatRef the value and reference number of the
      privious key}

  Procedure BTFindKey (     IFBPtr     : IsamFileBlockPtr;
                            Key        : Word;
                        Var UserDatRef : LongInt;
                            UserKey    : IsamKeyStr       );
    {-Returns in UserDatRef the reference number of the specified key}

  Procedure BTSearchKey (     IFBPtr     : IsamFileBlockPtr;
                              Key        : Word;
                          Var UserDatRef : LongInt;
                          Var UserKey    : IsamKeyStr    );
    {-Returns in UserKey / UserDatRef the value and reference number of the
      via UserKey specified or the following key}

  Procedure BTFindKeyAndRef (     IFBPtr                  : IsamFileBlockPtr;
                                  Key                     : Word;
                              Var UserDatRef              : LongInt;
                              Var UserKey                 : IsamKeyStr;
                                  NotFoundSearchDirection : Integer    );
    {-Returns in UserKey / UserDatRef the value and reference number of the
      via UserKey / UserDatRef specified combination.
      NotFoundSearchDirection determines whether and where to search for a
      result in case nothing was found}

  Procedure BTSearchKeyAndRef (     IFBPtr     : IsamFileBlockPtr;
                                    Key        : Word;
                                Var UserDatRef : LongInt;
                                Var UserKey    : IsamKeyStr );
    {-Returns in UserKey / UserDatRef the value and reference number of the
      via UserKey / UserDatRef specified combination.
      In case nothing was found a following result is searched. If this does
      not exist, a previous combination is searched}

  Function BTKeyExists ( IFBPtr     : IsamFileBlockPtr;
                         Key        : Word;
                         UserDatRef : LongInt;
                         UserKey    : IsamKeyStr    ) : Boolean;
    {-Returns True, if the combination UserKey / UserDatRef exists}

  Procedure BTNextDiffKey (     IFBPtr     : IsamFileBlockPtr;
                                Key        : Word;
                            Var UserDatRef : LongInt;
                            Var UserKey    : IsamKeyStr    );
    {-Returns in UserKey / UserDatRef the value and reference number of the
      next from UserKey different key}

  Procedure BTPrevDiffKey (     IFBPtr     : IsamFileBlockPtr;
                                Key        : Word;
                            Var UserDatRef : LongInt;
                            Var UserKey    : IsamKeyStr    );
    {-Returns in UserKey / UserDatRef the value and reference number of the
      previous from UserKey different key}

  Procedure BTAddKey ( IFBPtr     : IsamFileBlockPtr;
                       Key        : Word;
                       UserDatRef : LongInt;
                       UserKey    : IsamKeyStr    );
    {-Adds the combination UserKey / UserDatRef to the tree}

  Procedure BTDeleteKey ( IFBPtr     : IsamFileBlockPtr;
                          Key        : Word;
                          UserDatRef : LongInt;
                          UserKey    : IsamKeyStr    );
    {-Deletes the combination UserKey / UserDatRef in the tree}

  Procedure BTDeleteAllKeys ( IFBPtr : IsamFileBlockPtr; Key : Word );
    {-Deletes all keys with number Key}

  Function BTOtherWSChangedKey ( IFBPtr : IsamFileBlockPtr;
                                 Key    : Word ) : Boolean;
    {-Returns True, if it is definite, that a key of number Key was changed
      by another work station}

  Procedure BTGetApprRelPos (     IFBPtr     : IsamFileBlockPtr;
                                  Key        : Word;
                              Var RelPos     : Word;
                                  Scale      : Word;
                                  UserKey    : IsamKeyStr;
                                  UserDatRef : LongInt );
    {-Returns in RelPos the approximate relative position of the combination
      UserKey / UserDatRef in 0..Scale}

  Procedure BTGetApprKeyAndRef (     IFBPtr     : IsamFileBlockPtr;
                                     Key,
                                     RelPos     : Word;
                                     Scale      : Word;
                                 Var UserKey    : IsamKeyStr;
                                 Var UserDatRef : LongInt );
    {-Returns the combination UserKey / UserDatRef, that is approximately
      at the position RelPos in 0..Scale}

  Procedure BTFlushAllFileBlocks;
    {-Flushes all new data of all fileblocks to disk}

  Procedure BTFlushFileBlock ( IFBPtr : IsamFileBlockPtr );
    {-Flushes all new data of this fileblocks to disk}

  Procedure BTCreateFileBlock ( FName        : IsamFileBlockName;
                                DatSLen      : LongInt;
                                NumberOfKeys : Word;
                                IID          : IsamIndDescr );
    {-Creates a closed fileblock}

  Procedure BTOpenFileBlock ( Var IFBPtr      : IsamFileBlockPtr;
                                  FName       : IsamFileBlockName;
                                  ReadOnly,
                                  AllReadOnly,
                                  Save,
                                  Net         : Boolean );
    {-Opens a fileblock}

  Procedure BTCloseFileBlock ( Var IFBPtr : IsamFileBlockPtr );
    {-Closes a fileblock}

  Procedure BTCloseAllFileBlocks;
    {-Closes all open fileblocks}

  Procedure BTDeleteFileBlock ( FName  : IsamFileBlockName );
    {-Deletes a fileblock}

  Function BTDataFileName ( IFBPtr : IsamFileBlockPtr ) : IsamFileName;
    {-Returns the name of the data file including path and extension}

  Function BTPeekDataFileName ( IFBPtr : IsamFileBlockPtr ) : IsamFileName;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Function BTIndexFileName ( IFBPtr : IsamFileBlockPtr ) : IsamFileName;
    {-Returns the name of the index file including path and extension}

  Function BTPeekIndexFileName ( IFBPtr : IsamFileBlockPtr ) : IsamFileName;
    {-Just like the routine without "Peek", but not destroying the previous
      error on success}

  Function BTGetNextUsedAddRecRef ( IFBPtr : IsamFileBlockPtr ) : LongInt;
    {-Returns the data reference, that is used for adding the next record;
      this function is of a more internal nature}

  Function BTGetAfterNextUsedAddRecRef ( IFBPtr : IsamFileBlockPtr ) : LongInt;
    {-Returns the data reference, that is used after adding the next record;
      this function is of a more internal nature}

  Function BTGetInternalDialogID ( IFBPtr : IsamFileBlockPtr ) : Word;
    {-Returns the internal ID used by network accesss to this fileblock}

  Function BTGetAllowDupKeys ( IFBPtr : IsamFileBlockPtr;            {!!.50}
                               KeyNr  : Word ) : Boolean;
    {-Returns, whether duplicate keys are allowed or not}

  Function BTGetKeyLen ( IFBPtr : IsamFileBlockPtr;                  {!!.50}
                         KeyNr  : Word ) : Word;
    {-Returns the length of the key string KeyNr}

  Procedure BTNoCharConvert ( DataPtr  : Pointer;                    {!!.50}
                              DataLen  : LongInt;
                              PostRead : Boolean;
                              HookPtr  : Pointer );
    {-Does nothing}

  Procedure BTSetCharConvert ( IFBPtr     : IsamFileBlockPtr;        {!!.50}
                               CCProc     : ProcBTCharConvert;
                               HookPtr    : Pointer;
                               DestrWrite : Boolean );
   {-Sets the routine to convert data fields between different character
     sets; the routine CCProc is called after reading a record and before
     writing a record; if DestrWrite is true, CCProc is called in addition
     after writing a record to reconvert the record}

  Function BTIsInitialized : Boolean;                                {!!.50}
    {-Returns whether B-Tree Isam is currently initialized}

{--Interfaced only for compatibility reasons; not used outside except
   for network tests}
  Function BTIsamLockRecord ( Start,
                              Len       : LongInt;
                              Handle    : IsamHandle;
                              TimeOut,
                              DelayTime : Word ) : Boolean;
    {-Locks the bytes Start to Start+Len-1 of the file
      with handle Handle; TimeOut is the maximum wait time which may be
      separated in several retries with DelayTime milliseconds}

  Function BTIsamUnLockRecord ( Start,
                                Len    : LongInt;
                                Handle : IsamHandle) : Boolean;
    {-Unlocks the bytes Start to Start+Len-1 of the file
      with handle Handle}

  Function BTIsamGetSequentialOK ( IFBPtr : IsamFileBlockPtr;        {!!.50}
                                   KeyNr  : Word ) : Boolean;
    {-Returns, whether sequentiell access for key KeyNr is ok}

{--Routines of internal nature, but interfaced as well}
  Procedure IsamClearOK;
    {-Resets all status variables, even internal ones}

  Function IsamExists ( Name : IsamFileName ) : Boolean;
    {-Returns True, if the specified file exists}

  Procedure IsamExtractFileNames (     FNameComp : IsamFileBlockName;
                                   Var FNameD,
                                       FNameI    : IsamFileBlockName );
    {-Seperates two file names seperated by ";"}

  Procedure IsamCopyFile ( Source,
                           Dest                  : IsamFileBlockName;
                           DeleteSourceAfterCopy : Boolean );

    {-Copies file Source to Dest}

  Function IsamForceExtension ( Name, Ext : IsamFileName ) : IsamFileName;
    {-Forces the extension Ext to the file name Name}

{$IFNDEF Win32}
{$IFDEF Heap6}
  Function RoundToGranul ( Value : Word ) : Word;
    {-Rounds Value up to the next multiple of 8}
{$ELSE}
  Function RoundToGranul ( Value : Word ) : Word;
      Inline ( $58 );                                   {pop ax}
{$ENDIF}
{$ENDIF}

  Function IsamGetNumRecAddress ( IFBPtr : IsamFileBlockPtr ) : Pointer;
    {-Internal use only!}                                            {!!.50}

  Procedure IsamEntryCode ( IFBPtr : IsamFileBlockPtr; Options : Word );
    {-Internal use only!}                                            {!!.50}

  Procedure IsamExitCode ( IFBPtr : IsamFileBlockPtr );
    {-Internal use only!}                                            {!!.50}

  Procedure IsamAddRec (     IFBPtr : IsamFileBlockPtr;
                         Var RefNr  : LongInt;
                         Var Source );
    {-Internal use only!}                                            {!!.50}

  Procedure IsamDeleteRec ( IFBPtr : IsamFileBlockPtr; RefNr : LongInt );
    {-Internal use only!}                                            {!!.50}

  Procedure IsamPutRec (     IFBPtr  : IsamFileBlockPtr;
                             RefNr   : LongInt;
                         Var Source );
    {-Internal use only!}                                            {!!.50}

  Procedure IsamGetRec (     IFBPtr : IsamFileBlockPtr;
                             RefNr  : LongInt;
                         Var Dest );
    {-Internal use only!}                                            {!!.50}

  Procedure IsamGetRecReadOnly (     IFBPtr  : IsamFileBlockPtr;
                                     RefNr   : LongInt;
                                 Var Dest              );
    {-Internal use only!}                                            {!!.50}


{--Internal use only constants}                                   {!!.50mov}
Const
  OptCheckLock = $0100;
  OptReadPrefix = $0200;
  OptKeyRoutine = $0400;
  OptWriteRoutine = $0800;                                           {!!.41}
  NoOptions = 0;
  NotAFileBlockPtr = $FFFFFFFF;


Implementation

{--Privat constants}
Const
  IsamFBValidSign = $11195851;

{--Privat types}
Type
  LongPtr = ^LongInt;

  ErrStatSaveRec = packed Record
    OK     : Boolean;
    Error  : Integer;
    DError,
    DFunc  : Word;
  End;

{--Privat variables}
Var
  IsamRBR1Ptr            : IsamRingBufferRecPtr;
  IsamOFBLPtr            : IsamOpenFileBlockListPtr;
  IsamNrOfRingBufferRecs : Word;
  IsamDriveNotReadyError,
  IsamLockError,
  IsamNetEmu,
  IsamForceFlushOfMark,
  IsamIsCriticalActive,
  IsamIsInitialized      : Boolean;

  IsamInitializedNet     : NetSupportType;

  UserSaveEMSHandle,
  OwnSaveEMSHandle       : Byte;
  EMSHeapIsUsed          : Boolean;

  {$I IsamBase.Inc}
  {$I IsamWork.Inc}
{$IFNDEF NoNet}
  {$I IsamNWrk.Inc}
{$ENDIF}
  {$I IsamLow.Inc}
  {$I Filer.Inc}    {!!.TP}

{$IFDEF MSDOS}
Type
  IsamINT24DNRHandlerDef = Record
    PushAXBX,
    PushCXDX,
    PushBPSI,
    PushDIDS,
    PushESF        : Word;
    AndDIFF1       : LongInt;
    CmpDI02        : LongInt;
    JzMyTurn       : Word;
{ DoOldINT : }
    PopFES1,
    PopDSDI1,
    PopSIBP1,
    PopDXCX1,
    PopBXAX1       : Word;
    NopJmpFar      : Word;
    OldINT24Addr   : Pointer;
{ MyTurn : }
    NopMovAX       : Word;
    SegDNRError    : Word;
    MovDSAX        : Word;
    MovByte        : Word;
    OfsDNRError    : Word;
    ConstTrueNop   : Word;
    JmpDoOldInt    : Word;
  End;


Const
  IsamINT24DNRHandler : IsamINT24DNRHandlerDef = (
    PushAXBX : $5350;
    PushCXDX : $5251;
    PushBPSI : $5655;
    PushDIDS : $1E57;
    PushESF : $9C06;
    AndDIFF1 : $00FFE781;
    CmpDI02 : $0002FF81;
    JzMyTurn : $1074;

    PopFES1 : $079D;
    PopDSDI1 : $5F1F;
    PopSIBP1 : $5D5E;
    PopDXCX1 : $595A;
    PopBXAX1 : $585B;
    NopJmpFar : $EA90;
    OldINT24Addr : Nil;

    NopMovAX : $B890;
    SegDNRError : $FFFF;
    MovDSAX : $D88E;
    MovByte : $06C6;
    OfsDNRError : $FFFF;
    ConstTrueNop : $9000;
    JmpDoOldInt : $E2EB);


  Procedure IsamInstallInt24DNRHandler;

  Begin
    With IsamINT24DNRHandler Do Begin
      CallGetIntVec ( $24, OldINT24Addr );
      SegDNRError := Seg (IsamDriveNotReadyError);
      OfsDNRError := Ofs (IsamDriveNotReadyError);
      ConstTrueNop := ConstTrueNop + Ord (True);
    End;
    CallSetIntVec ( $24, @IsamINT24DNRHandler );
  End;


  Procedure IsamRemoveInt24DNRHandler;

  Begin
    CallSetIntVec ( $24, IsamINT24DNRHandler.OldINT24Addr );
  End;
{$ENDIF}


{!!.54 Initialization and finalization code rewritten}

{$IFNDEF WIN32}
var
  SaveExitProc : Pointer;
{$ENDIF}

procedure FilerUnitDone; far;                                 {!!.55}
begin
  {$IFNDEF WIN32}
  ExitProc := SaveExitProc;
  {$ENDIF}
  if IsamIsInitialized then begin
    IsamCloseAllFileBlocks;
    IsamClearOK;
  end;
  {$IFDEF MSDOS}
  IsamRemoveInt24DNRHandler;
  {$ENDIF}
end;

procedure FilerUnitInit;
begin
  {$IFNDEF Win32}                                             {!!.57}
  IsamDelay(1);                                               {!!.57}
  {$ENDIF}                                                    {!!.57}
  {$IFDEF MSDOS}
  IsamInstallInt24DNRHandler;
  {$ENDIF}
  IsamIsInitialized := False;
  IsamCompiledNets := []
  {$IFDEF NoNet}
                      + [NoNet]
  {$ENDIF}
  {$IFDEF Novell}
                      + [Novell]
  {$ENDIF}
  {$IFDEF MsNet}
                      + [MsNet]
  {$ENDIF}
                      ;
  {$IFNDEF Win32}
  SaveExitProc := ExitProc;
  ExitProc := @FilerUnitDone;
  {$ENDIF}
end;

{$IFDEF Win32}
initialization
  FilerUnitInit;
finalization
  FilerUnitDone;
{$ELSE}
begin
  FilerUnitInit;
{$ENDIF}
end.

