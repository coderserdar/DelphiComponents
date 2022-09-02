{*********************************************************}
{* FlashFiler: BDE consts and types for server           *}
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
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{Note: The following definitions are copied from BDE.PAS. The server
       cannot have BDE in its uses list since that unit has an
       initialization section which, when run, would pull in far too
       much for the FF server}

{BDE.PAS source file and error codes are
 (c) Copyright Borland International Inc, 1997}

{$I ffdefine.inc}

{$Z+}

unit ffsrbde;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ffconst,
  ffllbase;

type
  DBIMSG = array [0..127] of AnsiChar;                         {!!.55}


{============================================================================}
{                            Error Categories                                }
{============================================================================}

const
  ERRCAT_NONE                   = 0;      {  0   No error }
  ERRCAT_SYSTEM                 = $21;    {  33  System related (Fatal Error) }
  ERRCAT_NOTFOUND               = $22;    {  34  Object of interest Not Found }
  ERRCAT_DATACORRUPT            = $23;    {  35  Physical Data Corruption }
  ERRCAT_IO                     = $24;    {  36  I/O related error }
  ERRCAT_LIMIT                  = $25;    {  37  Resource or Limit error }
  ERRCAT_INTEGRITY              = $26;    {  38  Integrity Violation }
  ERRCAT_INVALIDREQ             = $27;    {  39  Invalid Request }
  ERRCAT_LOCKCONFLICT           = $28;    {  40  Locking/Contention related }
  ERRCAT_SECURITY               = $29;    {  41  Access Violation - Security related }
  ERRCAT_INVALIDCONTEXT         = $2A;    {  42  Invalid context }
  ERRCAT_OS                     = $2B;    {  43  Os Error not handled by Idapi }
  ERRCAT_NETWORK                = $2C;    {  44  Network related }
  ERRCAT_OPTPARAM               = $2D;    {  45  Optional parameter related }
  ERRCAT_QUERY                  = $2E;    {  46  Query related }
  ERRCAT_VERSION                = $2F;    {  47  Version Mismatch Category }
  ERRCAT_CAPABILITY             = $30;    {  48  Capability not supported }
  ERRCAT_CONFIG                 = $31;    {  49  System configuration error }
  ERRCAT_WARNING                = $32;    {  50 }
  ERRCAT_OTHER                  = $33;    {  51  Miscellaneous }
  ERRCAT_COMPATIBILITY          = $34;    {  52  Compatibility related }
  ERRCAT_REPOSITORY             = $35;    {  53  Data Repository related }

  ERRCAT_DRIVER                 = $3E;    {  62  Driver related }
  ERRCAT_RC                     = $3F;    {  63  Internal }


  ERRBASE_NONE                  = 0;      { No error }
  ERRBASE_SYSTEM                = $2100;  { System related (Fatal Error) }
  ERRBASE_NOTFOUND              = $2200;  { Object of interest Not Found }
  ERRBASE_DATACORRUPT           = $2300;  { Physical Data Corruption }
  ERRBASE_IO                    = $2400;  { I/O related error }
  ERRBASE_LIMIT                 = $2500;  { Resource or Limit error }
  ERRBASE_INTEGRITY             = $2600;  { Integrity Violation }
  ERRBASE_INVALIDREQ            = $2700;  { Invalid Request }
  ERRBASE_LOCKCONFLICT          = $2800;  { Locking/Contention related }
  ERRBASE_SEC                   = $2900;  { Access Violation - Security related }
  ERRBASE_IC                    = $2A00;  { Invalid context }
  ERRBASE_OS                    = $2B00;  { Os Error not handled by Idapi }
  ERRBASE_NETWORK               = $2C00;  { Network related }
  ERRBASE_OPTPARAM              = $2D00;  { Optional Parameter related }
  ERRBASE_QUERY                 = $2E00;  { Query related }
  ERRBASE_VERSION               = $2F00;  { Version Mismatch Category }
  ERRBASE_CAPABILITY            = $3000;  { Capability not supported }
  ERRBASE_CONFIG                = $3100;  { System configuration error }
  ERRBASE_WARNING               = $3200;
  ERRBASE_OTHER                 = $3300;  { Miscellaneous }
  ERRBASE_COMPATIBILITY         = $3400;  { Compatibility related }
  ERRBASE_REPOSITORY            = $3500;  { Data Repository related }

  ERRBASE_DRIVER                = $3E00;  { Driver related }
  ERRBASE_RC                    = $3F00;  { Internal }


{=============================================================================}
{                           Error Codes By Category                           }
{=============================================================================}

{ ERRCAT_NONE                  (0) }
{ ===========                      }

  ERRCODE_NONE                  = 0;

  DBIERR_NONE                   = (ERRBASE_NONE + ERRCODE_NONE);

{  ERRCAT_SYSTEM }
{  ============= }

  ERRCODE_SYSFILEOPEN           = 1;      { Cannot open a system file }
  ERRCODE_SYSFILEIO             = 2;      { I/O error on a system file }
  ERRCODE_SYSCORRUPT            = 3;      { Data structure corruption }
  ERRCODE_NOCONFIGFILE          = 4;      { Cannot find config file }
  ERRCODE_CFGCANNOTWRITE        = 5;      { Cannot write config file (READONLY) }
  ERRCODE_CFGMULTIFILE          = 6;      { Initializing with different ini file }
  ERRCODE_REENTERED             = 7;      { System has been illegally re-entered }
  ERRCODE_CANTFINDIDAPI         = 8;      { Cannot locate IDAPIxx.DLL }
  ERRCODE_CANTLOADIDAPI         = 9;      { Cannot load IDAPIxx.DLL }
  ERRCODE_CANTLOADLIBRARY       = 10;     { Cannot load a service DLL }
  ERRCODE_TEMPFILEERR           = 11;     { Cannot create or open temporary file }
  ERRCODE_MULTIPLEIDAPI         = 12;     { Trying to load multiple IDAPIxx.DLL }

  DBIERR_SYSFILEOPEN            = (ERRBASE_SYSTEM + ERRCODE_SYSFILEOPEN);
  DBIERR_SYSFILEIO              = (ERRBASE_SYSTEM + ERRCODE_SYSFILEIO);
  DBIERR_SYSCORRUPT             = (ERRBASE_SYSTEM + ERRCODE_SYSCORRUPT);
  DBIERR_NOCONFIGFILE           = (ERRBASE_SYSTEM + ERRCODE_NOCONFIGFILE);
  DBIERR_CFGCANNOTWRITE         = (ERRBASE_SYSTEM + ERRCODE_CFGCANNOTWRITE);
  DBIERR_CFGMULTIFILE           = (ERRBASE_SYSTEM + ERRCODE_CFGMULTIFILE);
  DBIERR_REENTERED              = (ERRBASE_SYSTEM + ERRCODE_REENTERED);
  DBIERR_CANTFINDIDAPI          = (ERRBASE_SYSTEM + ERRCODE_CANTFINDIDAPI);
  DBIERR_CANTLOADIDAPI          = (ERRBASE_SYSTEM + ERRCODE_CANTLOADIDAPI);
  DBIERR_CANTLOADLIBRARY        = (ERRBASE_SYSTEM + ERRCODE_CANTLOADLIBRARY);
  DBIERR_TEMPFILEERR            = (ERRBASE_SYSTEM + ERRCODE_TEMPFILEERR);
  DBIERR_MULTIPLEIDAPI          = (ERRBASE_SYSTEM + ERRCODE_MULTIPLEIDAPI);

  DBIERR_CANTFINDODAPI = DBIERR_CANTFINDIDAPI;
  DBIERR_CANTLOADODAPI = DBIERR_CANTLOADIDAPI;

{  ERRCAT_NOTFOUND }
{  =============== }

  ERRCODE_BOF                   = 1;      { Beginning of Virtual table }
  ERRCODE_EOF                   = 2;      { End of Virtual table }
  ERRCODE_RECMOVED              = 3;      { Fly-away }
  ERRCODE_KEYORRECDELETED       = 4;      { Record Deleted/Key Modified }
  ERRCODE_NOCURRREC             = 5;      { No current record }
  ERRCODE_RECNOTFOUND           = 6;      { Record was not found }
  ERRCODE_ENDOFBLOB             = 7;      { End of Blob reached }
  ERRCODE_OBJNOTFOUND           = 8;      { Generic Not found }
  ERRCODE_FMLMEMBERNOTFOUND     = 9;      { Family member not found }
  ERRCODE_BLOBFILEMISSING       = 10;     { 0x0a Blob file for table is missing }
  ERRCODE_LDNOTFOUND            = 11;     { 0x0b Language driver not found }

  DBIERR_BOF                    = (ERRBASE_NOTFOUND + ERRCODE_BOF);
  DBIERR_EOF                    = (ERRBASE_NOTFOUND + ERRCODE_EOF);
  DBIERR_RECMOVED               = (ERRBASE_NOTFOUND + ERRCODE_RECMOVED);
  DBIERR_RECDELETED             = (ERRBASE_NOTFOUND + ERRCODE_KEYORRECDELETED);
  DBIERR_KEYORRECDELETED        = (ERRBASE_NOTFOUND + ERRCODE_KEYORRECDELETED);
  DBIERR_NOCURRREC              = (ERRBASE_NOTFOUND + ERRCODE_NOCURRREC);
  DBIERR_RECNOTFOUND            = (ERRBASE_NOTFOUND + ERRCODE_RECNOTFOUND);
  DBIERR_ENDOFBLOB              = (ERRBASE_NOTFOUND + ERRCODE_ENDOFBLOB);
  DBIERR_OBJNOTFOUND            = (ERRBASE_NOTFOUND + ERRCODE_OBJNOTFOUND);
  DBIERR_FMLMEMBERNOTFOUND      = (ERRBASE_NOTFOUND + ERRCODE_FMLMEMBERNOTFOUND);
  DBIERR_BLOBFILEMISSING        = (ERRBASE_NOTFOUND + ERRCODE_BLOBFILEMISSING);
  DBIERR_LDNOTFOUND             = (ERRBASE_NOTFOUND + ERRCODE_LDNOTFOUND);

{ ERRCAT_DATACORRUPT }
{ ================== }

  ERRCODE_HEADERCORRUPT         = 1;      { Corrupt Header }
  ERRCODE_FILECORRUPT           = 2;      { File corrupt - other than header }
  ERRCODE_MEMOCORRUPT           = 3;      { Memo file corrupted }
  ERRCODE_BMPCORRUPT            = 4;      { BitMap is corrupt (Internal error) }
  ERRCODE_INDEXCORRUPT          = 5;      { Index is corrupt }
  ERRCODE_CORRUPTLOCKFILE       = 6;      { Corrupt lock file }
  ERRCODE_FAMFILEINVALID        = 7;      { Corrupt family file }
  ERRCODE_VALFILECORRUPT        = 8;      { Val file is missing or corrupt }
  ERRCODE_FOREIGNINDEX          = 9;      { Index is in a foreign format - import first }


  DBIERR_HEADERCORRUPT          = (ERRBASE_DATACORRUPT + ERRCODE_HEADERCORRUPT);
  DBIERR_FILECORRUPT            = (ERRBASE_DATACORRUPT + ERRCODE_FILECORRUPT);
  DBIERR_MEMOCORRUPT            = (ERRBASE_DATACORRUPT + ERRCODE_MEMOCORRUPT);
  DBIERR_BMPCORRUPT             = (ERRBASE_DATACORRUPT + ERRCODE_BMPCORRUPT);
  DBIERR_INDEXCORRUPT           = (ERRBASE_DATACORRUPT + ERRCODE_INDEXCORRUPT);
  DBIERR_CORRUPTLOCKFILE        = (ERRBASE_DATACORRUPT + ERRCODE_CORRUPTLOCKFILE);
  DBIERR_FAMFILEINVALID         = (ERRBASE_DATACORRUPT + ERRCODE_FAMFILEINVALID);
  DBIERR_VALFILECORRUPT         = (ERRBASE_DATACORRUPT + ERRCODE_VALFILECORRUPT);
  DBIERR_FOREIGNINDEX           = (ERRBASE_DATACORRUPT + ERRCODE_FOREIGNINDEX);


{ ERRCAT_IO }
{ ========= }

  ERRCODE_READERR               = 1;      { Read failure (not expected) }
  ERRCODE_WRITEERR              = 2;      { Write failure (not expected) }
  ERRCODE_DIRNOACCESS           = 3;      { No access to dir }
  ERRCODE_FILEDELETEFAIL        = 4;      { File delete failed }
  ERRCODE_FILENOACCESS          = 5;      { No access to file }
  ERRCODE_ACCESSDISABLED        = 6;      { Access to table disabled (previous error) }

  DBIERR_READERR                = (ERRBASE_IO + ERRCODE_READERR);
  DBIERR_WRITEERR               = (ERRBASE_IO + ERRCODE_WRITEERR);
  DBIERR_DIRNOACCESS            = (ERRBASE_IO + ERRCODE_DIRNOACCESS);
  DBIERR_FILEDELETEFAIL         = (ERRBASE_IO + ERRCODE_FILEDELETEFAIL);
  DBIERR_FILENOACCESS           = (ERRBASE_IO + ERRCODE_FILENOACCESS);
  DBIERR_ACCESSDISABLED         = (ERRBASE_IO + ERRCODE_ACCESSDISABLED);

{ ERRCAT_LIMIT }
{ ============ }

  ERRCODE_NOMEMORY              = 1;      { Not enough Memory for this op }
  ERRCODE_NOFILEHANDLES         = 2;      { Not enough File handles }
  ERRCODE_NODISKSPACE           = 3;      { Not enough Disk space }
  ERRCODE_NOTEMPTBLSPACE        = 4;      { Temporary Table resource limit }
  ERRCODE_RECTOOBIG             = 5;      { Too big a record size for table }
  ERRCODE_CURSORLIMIT           = 6;      { Too many open cursors }
  ERRCODE_TABLEFULL             = 7;      { Table is full }
  ERRCODE_WSSESLIMIT            = 8;      { Too many sessions from this WS }
  ERRCODE_SERNUMLIMIT           = 9;      { Serial number limit (paradox) }
  ERRCODE_INTERNALLIMIT         = 10;     { 0x0a Some internal limit (see context) }
  ERRCODE_OPENTBLLIMIT          = 11;     { 0x0b Too many open tables }
  ERRCODE_TBLCURSORLIMIT        = 12;     { 0x0c Too many cursors per table }
  ERRCODE_RECLOCKLIMIT          = 13;     { 0x0d Too many record locks on table }
  ERRCODE_CLIENTSLIMIT          = 14;     { 0x0e Too many clients }
  ERRCODE_INDEXLIMIT            = 15;     { 0x0f Too many indexes (also in Table Create) }
  ERRCODE_SESSIONSLIMIT         = 16;     { 0x10 Too many sessions }
  ERRCODE_DBLIMIT               = 17;     { 0x11 Too many databases }
  ERRCODE_PASSWORDLIMIT         = 18;     { 0x12 Too many passwords }
  ERRCODE_DRIVERLIMIT           = 19;     { 0x13 Too many active drivers }
  ERRCODE_FLDLIMIT              = 20;     { 0x14 Too many Fields in Table Create }
  ERRCODE_TBLLOCKLIMIT          = 21;     { 0x15 Too many table locks }
  ERRCODE_OPENBLOBLIMIT         = 22;     { 0x16 Too many open blobs }
  ERRCODE_LOCKFILELIMIT         = 23;     { 0x17 Lock file has grown too big }
  ERRCODE_OPENQRYLIMIT          = 24;     { 0x18 Too many open queries }
  ERRCODE_THREADLIMIT           = 25;     { 0x19 Too many threads for client }
  ERRCODE_BLOBLIMIT             = 26;     { 0x1a Too many blobs }
  ERRCODE_PDX50NAMELIMIT        = 27;     { 0x1b Pathname is too long for a Paradox 5.0 or less table }
  ERRCODE_ROWFETCHLIMIT         = 28;     { 0x1c Row fetch limit }
  ERRCODE_LONGNAMENOTALLOWED    = 29;     { 0x1d Long name is not allowed for this tableversion }
  
  DBIERR_NOMEMORY               = (ERRBASE_LIMIT + ERRCODE_NOMEMORY);
  DBIERR_NOFILEHANDLES          = (ERRBASE_LIMIT + ERRCODE_NOFILEHANDLES);
  DBIERR_NODISKSPACE            = (ERRBASE_LIMIT + ERRCODE_NODISKSPACE);
  DBIERR_NOTEMPTBLSPACE         = (ERRBASE_LIMIT + ERRCODE_NOTEMPTBLSPACE);
  DBIERR_RECTOOBIG              = (ERRBASE_LIMIT + ERRCODE_RECTOOBIG);
  DBIERR_CURSORLIMIT            = (ERRBASE_LIMIT + ERRCODE_CURSORLIMIT);
  DBIERR_TABLEFULL              = (ERRBASE_LIMIT + ERRCODE_TABLEFULL);
  DBIERR_WSSESLIMIT             = (ERRBASE_LIMIT + ERRCODE_WSSESLIMIT);
  DBIERR_SERNUMLIMIT            = (ERRBASE_LIMIT + ERRCODE_SERNUMLIMIT);
  DBIERR_INTERNALLIMIT          = (ERRBASE_LIMIT + ERRCODE_INTERNALLIMIT);
  DBIERR_OPENTBLLIMIT           = (ERRBASE_LIMIT + ERRCODE_OPENTBLLIMIT);
  DBIERR_TBLCURSORLIMIT         = (ERRBASE_LIMIT + ERRCODE_TBLCURSORLIMIT);
  DBIERR_RECLOCKLIMIT           = (ERRBASE_LIMIT + ERRCODE_RECLOCKLIMIT);
  DBIERR_CLIENTSLIMIT           = (ERRBASE_LIMIT + ERRCODE_CLIENTSLIMIT);
  DBIERR_INDEXLIMIT             = (ERRBASE_LIMIT + ERRCODE_INDEXLIMIT);
  DBIERR_SESSIONSLIMIT          = (ERRBASE_LIMIT + ERRCODE_SESSIONSLIMIT);
  DBIERR_DBLIMIT                = (ERRBASE_LIMIT + ERRCODE_DBLIMIT);
  DBIERR_PASSWORDLIMIT          = (ERRBASE_LIMIT + ERRCODE_PASSWORDLIMIT);
  DBIERR_DRIVERLIMIT            = (ERRBASE_LIMIT + ERRCODE_DRIVERLIMIT);
  DBIERR_FLDLIMIT               = (ERRBASE_LIMIT + ERRCODE_FLDLIMIT);
  DBIERR_TBLLOCKLIMIT           = (ERRBASE_LIMIT + ERRCODE_TBLLOCKLIMIT);
  DBIERR_OPENBLOBLIMIT          = (ERRBASE_LIMIT + ERRCODE_OPENBLOBLIMIT);
  DBIERR_LOCKFILELIMIT          = (ERRBASE_LIMIT + ERRCODE_LOCKFILELIMIT);
  DBIERR_OPENQRYLIMIT           = (ERRBASE_LIMIT + ERRCODE_OPENQRYLIMIT);
  DBIERR_THREADLIMIT            = (ERRBASE_LIMIT + ERRCODE_THREADLIMIT);
  DBIERR_BLOBLIMIT              = (ERRBASE_LIMIT + ERRCODE_BLOBLIMIT);
  DBIERR_PDX50NAMELIMIT         = (ERRBASE_LIMIT + ERRCODE_PDX50NAMELIMIT);
  DBIERR_ROWFETCHLIMIT          = (ERRBASE_LIMIT + ERRCODE_ROWFETCHLIMIT);
  DBIERR_LONGNAMENOTALLOWED     = (ERRBASE_LIMIT + ERRCODE_LONGNAMENOTALLOWED);

{ ERRCAT_INTEGRITY }
{ ================ }

  ERRCODE_KEYVIOL               = 1;      { Key violation }
  ERRCODE_MINVALERR             = 2;      { Min val check failed }
  ERRCODE_MAXVALERR             = 3;      { Max val check failed }
  ERRCODE_REQDERR               = 4;      { Field value required }
  ERRCODE_FORIEGNKEYERR         = 5;      { Master record missing }
  ERRCODE_DETAILRECORDSEXIST    = 6;      { Cannot MODIFY or DELETE this Master record }
  ERRCODE_MASTERTBLLEVEL        = 7;      { Master Table Level is incorrect }
  ERRCODE_LOOKUPTABLEERR        = 8;      { Field value out of lookup tbl range }
  ERRCODE_LOOKUPTBLOPENERR      = 9;      { Lookup Table Open failed }
  ERRCODE_DETAILTBLOPENERR      = 10;     { 0x0a Detail Table Open failed }
  ERRCODE_MASTERTBLOPENERR      = 11;     { 0x0b Master Table Open failed }
  ERRCODE_FIELDISBLANK          = 12;     { 0x0c Field is blank }

  ERRCODE_MASTEREXISTS          = 13;     { 0x0d Master Table exists }
  ERRCODE_MASTERTBLOPEN         = 14;     { 0x0e Master Table is open }

  ERRCODE_DETAILTABLESEXIST     = 15;     { 0x0f Detail Tables exist ( cannot delete, rename ... ) }
  ERRCODE_DETAILRECEXISTEMPTY   = 16;     { 0x10 Cannot empty because details exist }
  ERRCODE_MASTERREFERENCEERR    = 17;     { 0x11 Cannot modify while adding self referencing Referential Integrity }
  ERRCODE_DETAILTBLOPEN         = 18;     { 0x12 Detail Table is opened }
  ERRCODE_DEPENDENTSMUSTBEEMPTY = 19;     { 0x13 Cannot make a master a detail of another table if its details are not empty. }
  ERRCODE_RINTREQINDEX          = 20;     { 0x14 Ref. integrity fields must be indexed }
  ERRCODE_LINKEDTBLPROTECTED    = 21;     { 0x15 Master Table is protected ( requires password to open) }
  ERRCODE_FIELDMULTILINKED      = 22;     { 0x16 Field has more than one master }

  DBIERR_KEYVIOL                = (ERRBASE_INTEGRITY + ERRCODE_KEYVIOL);
  DBIERR_MINVALERR              = (ERRBASE_INTEGRITY + ERRCODE_MINVALERR);
  DBIERR_MAXVALERR              = (ERRBASE_INTEGRITY + ERRCODE_MAXVALERR);
  DBIERR_REQDERR                = (ERRBASE_INTEGRITY + ERRCODE_REQDERR);
  DBIERR_FORIEGNKEYERR          = (ERRBASE_INTEGRITY + ERRCODE_FORIEGNKEYERR);
  DBIERR_DETAILRECORDSEXIST     = (ERRBASE_INTEGRITY + ERRCODE_DETAILRECORDSEXIST);
  DBIERR_MASTERTBLLEVEL         = (ERRBASE_INTEGRITY + ERRCODE_MASTERTBLLEVEL);
  DBIERR_LOOKUPTABLEERR         = (ERRBASE_INTEGRITY + ERRCODE_LOOKUPTABLEERR);
  DBIERR_LOOKUPTBLOPENERR       = (ERRBASE_INTEGRITY + ERRCODE_LOOKUPTBLOPENERR);
  DBIERR_DETAILTBLOPENERR       = (ERRBASE_INTEGRITY + ERRCODE_DETAILTBLOPENERR);
  DBIERR_MASTERTBLOPENERR       = (ERRBASE_INTEGRITY + ERRCODE_MASTERTBLOPENERR);
  DBIERR_FIELDISBLANK           = (ERRBASE_INTEGRITY + ERRCODE_FIELDISBLANK);
  DBIERR_MASTEREXISTS           = (ERRBASE_INTEGRITY + ERRCODE_MASTEREXISTS);
  DBIERR_MASTERTBLOPEN          = (ERRBASE_INTEGRITY + ERRCODE_MASTERTBLOPEN);
  DBIERR_DETAILTABLESEXIST      = (ERRBASE_INTEGRITY + ERRCODE_DETAILTABLESEXIST);
  DBIERR_DETAILRECEXISTEMPTY    = (ERRBASE_INTEGRITY + ERRCODE_DETAILRECEXISTEMPTY);
  DBIERR_MASTERREFERENCEERR     = (ERRBASE_INTEGRITY + ERRCODE_MASTERREFERENCEERR);
  DBIERR_DETAILTBLOPEN          = (ERRBASE_INTEGRITY + ERRCODE_DETAILTBLOPEN);
  DBIERR_DEPENDENTSMUSTBEEMPTY  = (ERRBASE_INTEGRITY + ERRCODE_DEPENDENTSMUSTBEEMPTY);
  DBIERR_RINTREQINDEX           = (ERRBASE_INTEGRITY + ERRCODE_RINTREQINDEX);
  DBIERR_LINKEDTBLPROTECTED     = (ERRBASE_INTEGRITY + ERRCODE_LINKEDTBLPROTECTED);
  DBIERR_FIELDMULTILINKED       = (ERRBASE_INTEGRITY + ERRCODE_FIELDMULTILINKED);


{ ERRCAT_INVALIDREQ }
{ ================= }

  ERRCODE_OUTOFRANGE            = 1;      { Number out of range (e.g field no) }
  ERRCODE_INVALIDPARAM          = 2;      { Generic invalid parameter }
  ERRCODE_INVALIDFILENAME       = 3;      { Invalid file name }
  ERRCODE_NOSUCHFILE            = 4;      { No such file }
  ERRCODE_INVALIDOPTION         = 5;      { Invalid option for a parameter }
  ERRCODE_INVALIDHNDL           = 6;      { Invalid handle to the function }
  ERRCODE_UNKNOWNTBLTYPE        = 7;      { Table type given not known }
  ERRCODE_UNKNOWNFILE           = 8;      { Dont know how to open file }
  ERRCODE_PRIMARYKEYREDEFINE    = 9;      { Cannot redefine primary key }
  ERRCODE_INVALIDRINTDESCNUM    = 10;     { 0x0a Cannot change this RINTDesc }
  ERRCODE_KEYFLDTYPEMISMATCH    = 11;     { 0x0b Foreign & Primary Key Mismatch }
  ERRCODE_INVALIDMODIFYREQUEST  = 12;     { 0x0c Invalid modify request }
  ERRCODE_NOSUCHINDEX           = 13;     { 0x0d Index does not exist }
  ERRCODE_INVALIDBLOBOFFSET     = 14;     { 0x0e Invalid Offset into the Blob }
  ERRCODE_INVALIDDESCNUM        = 15;     { 0x0f Invalid descriptor number }
  ERRCODE_INVALIDFLDTYPE        = 16;     { 0x10 Invalid field type }
  ERRCODE_INVALIDFLDDESC        = 17;     { 0x11 Invalid field descriptor }
  ERRCODE_INVALIDFLDXFORM       = 18;     { 0x12 Invalid field transform }
  ERRCODE_INVALIDRECSTRUCT      = 19;     { 0x13 Invalid record structure }
  ERRCODE_INVALIDDESC           = 20;     { 0x14 Generic: invalid descriptor }
  ERRCODE_INVALIDINDEXSTRUCT    = 21;     { 0x15 Invalid array of indexes descriptors }
  ERRCODE_INVALIDVCHKSTRUCT     = 22;     { 0x16 Invalid array of  val. check descriptors }
  ERRCODE_INVALIDRINTSTRUCT     = 23;     { 0x17 Invalid array of ref. integrity descriptors }
  ERRCODE_INVALIDRESTRTBLORDER  = 24;     { 0x18 Invalid ordering of tables during restructure }
  ERRCODE_NAMENOTUNIQUE         = 25;     { 0x19 Name not unique in this context }
  ERRCODE_INDEXNAMEREQUIRED     = 26;     { 0x1a Index name required }
  ERRCODE_INVALIDSESHANDLE      = 27;     { 0x1b Invalid ses handle }
  ERRCODE_INVALIDRESTROP        = 28;     { 0x1c Invalid restructure operation }
  ERRCODE_UNKNOWNDRIVER         = 29;     { 0x1d Driver not known to system }
  ERRCODE_UNKNOWNDB             = 30;     { 0x1e Unknown db }
  ERRCODE_INVALIDPASSWORD       = 31;     { 0x1f Invalid password given }
  ERRCODE_NOCALLBACK            = 32;     { 0x20 No callback function }
  ERRCODE_INVALIDCALLBACKBUFLEN = 33;     { 0x21 Invalid callback buffer length }
  ERRCODE_INVALIDDIR            = 34;     { 0x22 Invalid directory }
  ERRCODE_INVALIDXLATION        = 35;     { 0x23 Translate Error - Translate DID NOT happen }
  ERRCODE_DIFFERENTTABLES       = 36;     { 0x24 Cannot Set Cursor of one Table to another }
  ERRCODE_INVALIDBOOKMARK       = 37;     { 0x25 Bookmarks does not match table, etc. }
  ERRCODE_INVALIDINDEXNAME      = 38;     { 0x26 Index/Tag Name is invalid }
  ERRCODE_INVALIDIDXDESC        = 39;     { 0x27 Invalid index descriptor }
  ERRCODE_NOSUCHTABLE           = 40;     { 0x28 No such table }
  ERRCODE_USECOUNT              = 41;     { 0x29 Table has too many users }
  ERRCODE_INVALIDKEY            = 42;     { 0x2a Key does not pass filter condition }
  ERRCODE_INDEXEXISTS           = 43;     { 0x2b Index already exists }
  ERRCODE_INDEXOPEN             = 44;     { 0x2c Index is open }
  ERRCODE_INVALIDBLOBLEN        = 45;     { 0x2d Invalid Blob Length }
  ERRCODE_INVALIDBLOBHANDLE     = 46;     { 0x2e Invalid Blob handle (in record buffer) }
  ERRCODE_TABLEOPEN             = 47;     { 0x2f Table is open }
  ERRCODE_NEEDRESTRUCTURE       = 48;     { 0x30 Need to do (hard) restructure }
  ERRCODE_INVALIDMODE           = 49;     { 0x31 Invalid mode }
  ERRCODE_CANNOTCLOSE           = 50;     { 0x32 Cannot close index }
  ERRCODE_ACTIVEINDEX           = 51;     { 0x33 Index is being used to order tbl }
  ERRCODE_INVALIDUSRPASS        = 52;     { 0x34 Bad user name or password }
  ERRCODE_MULTILEVELCASCADE     = 53;     { 0x35 Multi level Cascade not supported }
  ERRCODE_INVALIDFIELDNAME      = 54;     { 0x36 Invalid field name }
  ERRCODE_INVALIDTABLENAME      = 55;     { 0x37 Invalid table name }
  ERRCODE_INVALIDLINKEXPR       = 56;     { 0x38 Invalid linked cursor expression }
  ERRCODE_NAMERESERVED          = 57;     { 0x39 Name is reserved }
  ERRCODE_INVALIDFILEEXTN       = 58;     { 0x3a Invalid file extention }
  ERRCODE_INVALIDLANGDRV        = 59;     { 0x3b Invalid language driver }
  ERRCODE_ALIASNOTOPEN          = 60;     { 0x3c Requested alias in not open }
  ERRCODE_INCOMPATRECSTRUCTS    = 61;     { 0x3d Incompatible record structures }
  ERRCODE_RESERVEDDOSNAME       = 62;     { 0x3e Reserved dos name }
  ERRCODE_DESTMUSTBEINDEXED     = 63;     { 0x3f Destination must be indexed }
  ERRCODE_INVALIDINDEXTYPE      = 64;     { 0x40 Invalid index type }
  ERRCODE_LANGDRVMISMATCH       = 65;     { 0x41 Language driver of table and index do not match }
  ERRCODE_NOSUCHFILTER          = 66;     { 0x42 Filter handle is invalid }
  ERRCODE_INVALIDFILTER         = 67;     { 0x43 Invalid filter }

  ERRCODE_INVALIDTABLECREATE    = 68;     { 0x44 Bad table create request (exact prob unknown) }
  ERRCODE_INVALIDTABLEDELETE    = 69;     { 0x45 Bad table delete request (exact prob unknown) }
  ERRCODE_INVALIDINDEXCREATE    = 70;     { 0x46 Bad index create request (exact prob unknown) }
  ERRCODE_INVALIDINDEXDELETE    = 71;     { 0x47 Bad index delete request (exact prob unknown) }
  ERRCODE_INVALIDTABLE          = 72;     { 0x48 Invalid table name specified }
  ERRCODE_MULTIRESULTS          = 73;     { 0X49 Multi results }
  ERRCODE_INVALIDTIME           = 74;     { 0X4A Multi results }
  ERRCODE_INVALIDDATE           = 75;     { 0X4B Multi results }
  ERRCODE_INVALIDTIMESTAMP      = 76;     { 0X4C Multi results }
  ERRCODE_DIFFERENTPATH         = 77;     { 0X4d Tables in different paths }
  ERRCODE_MISMATCHARGS          = 78;     { 0x4e MisMatch in the # of arguments }
  ERRCODE_FUNCTIONNOTFOUND      = 79;     { 0x4f Loaderlib cant find a func in the DLL (bad version?) }
  ERRCODE_MUSTUSEBASEORDER      = 80;     { 0x50 Must use baseorder for this operation }
  ERRCODE_INVALIDPROCEDURENAME  = 81;     { 0x51 Invalid procedure name }
  ERRCODE_INVALIDFLDMAP         = 82;     { 0x52 invalid field map }


  DBIERR_OUTOFRANGE             = (ERRBASE_INVALIDREQ + ERRCODE_OUTOFRANGE);
  DBIERR_INVALIDPARAM           = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDPARAM);
  DBIERR_INVALIDFILENAME        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFILENAME);
  DBIERR_NOSUCHFILE             = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHFILE);
  DBIERR_INVALIDOPTION          = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDOPTION);
  DBIERR_INVALIDHNDL            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDHNDL);
  DBIERR_UNKNOWNTBLTYPE         = (ERRBASE_INVALIDREQ + ERRCODE_UNKNOWNTBLTYPE);
  DBIERR_UNKNOWNFILE            = (ERRBASE_INVALIDREQ + ERRCODE_UNKNOWNFILE);
  DBIERR_PRIMARYKEYREDEFINE     = (ERRBASE_INVALIDREQ + ERRCODE_PRIMARYKEYREDEFINE);
  DBIERR_INVALIDRINTDESCNUM     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRINTDESCNUM);
  DBIERR_KEYFLDTYPEMISMATCH     = (ERRBASE_INVALIDREQ + ERRCODE_KEYFLDTYPEMISMATCH);
  DBIERR_INVALIDMODIFYREQUEST   = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDMODIFYREQUEST);
  DBIERR_NOSUCHINDEX            = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHINDEX);
  DBIERR_INVALIDBLOBOFFSET      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBLOBOFFSET);
  DBIERR_INVALIDDESCNUM         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDDESCNUM);
  DBIERR_INVALIDFLDTYPE         = (ERRBASE_INVALIDREQ +  ERRCODE_INVALIDFLDTYPE);
  DBIERR_INVALIDFLDDESC         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFLDDESC);
  DBIERR_INVALIDFLDXFORM        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFLDXFORM);
  DBIERR_INVALIDRECSTRUCT       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRECSTRUCT);
  DBIERR_INVALIDDESC            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDDESC);
  DBIERR_INVALIDINDEXSTRUCT     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXSTRUCT);
  DBIERR_INVALIDVCHKSTRUCT      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDVCHKSTRUCT);
  DBIERR_INVALIDRINTSTRUCT      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRINTSTRUCT);
  DBIERR_INVALIDRESTRTBLORDER   = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRESTRTBLORDER);
  DBIERR_NAMENOTUNIQUE          = (ERRBASE_INVALIDREQ + ERRCODE_NAMENOTUNIQUE);
  DBIERR_INDEXNAMEREQUIRED      = (ERRBASE_INVALIDREQ + ERRCODE_INDEXNAMEREQUIRED);
  DBIERR_INVALIDSESHANDLE       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDSESHANDLE);
  DBIERR_INVALIDRESTROP         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRESTROP);
  DBIERR_UNKNOWNDRIVER          = (ERRBASE_INVALIDREQ + ERRCODE_UNKNOWNDRIVER);
  DBIERR_UNKNOWNDB              = (ERRBASE_INVALIDREQ + ERRCODE_UNKNOWNDB);
  DBIERR_INVALIDPASSWORD        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDPASSWORD);
  DBIERR_NOCALLBACK             = (ERRBASE_INVALIDREQ + ERRCODE_NOCALLBACK);
  DBIERR_INVALIDCALLBACKBUFLEN  = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDCALLBACKBUFLEN );
  DBIERR_INVALIDDIR             = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDDIR);
  DBIERR_INVALIDXLATION         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDXLATION);
  DBIERR_DIFFERENTTABLES        = (ERRBASE_INVALIDREQ + ERRCODE_DIFFERENTTABLES);
  DBIERR_INVALIDBOOKMARK        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBOOKMARK);
  DBIERR_INVALIDINDEXNAME       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXNAME);
  DBIERR_INVALIDIDXDESC         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDIDXDESC);
  DBIERR_NOSUCHTABLE            = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHTABLE);
  DBIERR_USECOUNT               = (ERRBASE_INVALIDREQ + ERRCODE_USECOUNT);
  DBIERR_INVALIDKEY             = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDKEY);
  DBIERR_INDEXEXISTS            = (ERRBASE_INVALIDREQ + ERRCODE_INDEXEXISTS);
  DBIERR_INDEXOPEN              = (ERRBASE_INVALIDREQ + ERRCODE_INDEXOPEN);
  DBIERR_INVALIDBLOBLEN         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBLOBLEN);
  DBIERR_INVALIDBLOBHANDLE      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBLOBHANDLE);
  DBIERR_TABLEOPEN              = (ERRBASE_INVALIDREQ + ERRCODE_TABLEOPEN);
  DBIERR_NEEDRESTRUCTURE        = (ERRBASE_INVALIDREQ + ERRCODE_NEEDRESTRUCTURE);
  DBIERR_INVALIDMODE            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDMODE);
  DBIERR_CANNOTCLOSE            = (ERRBASE_INVALIDREQ + ERRCODE_CANNOTCLOSE);
  DBIERR_ACTIVEINDEX            = (ERRBASE_INVALIDREQ + ERRCODE_ACTIVEINDEX);
  DBIERR_INVALIDUSRPASS         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDUSRPASS);
  DBIERR_MULTILEVELCASCADE      = (ERRBASE_INVALIDREQ + ERRCODE_MULTILEVELCASCADE);
  DBIERR_INVALIDFIELDNAME       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFIELDNAME);
  DBIERR_INVALIDTABLENAME       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTABLENAME);
  DBIERR_INVALIDLINKEXPR        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDLINKEXPR);
  DBIERR_NAMERESERVED           = (ERRBASE_INVALIDREQ + ERRCODE_NAMERESERVED);
  DBIERR_INVALIDFILEEXTN        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFILEEXTN);
  DBIERR_INVALIDLANGDRV         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDLANGDRV);
  DBIERR_ALIASNOTOPEN           = (ERRBASE_INVALIDREQ + ERRCODE_ALIASNOTOPEN);
  DBIERR_INCOMPATRECSTRUCTS     = (ERRBASE_INVALIDREQ + ERRCODE_INCOMPATRECSTRUCTS);
  DBIERR_RESERVEDOSNAME         = (ERRBASE_INVALIDREQ + ERRCODE_RESERVEDDOSNAME);
  DBIERR_DESTMUSTBEINDEXED      = (ERRBASE_INVALIDREQ + ERRCODE_DESTMUSTBEINDEXED);
  DBIERR_INVALIDINDEXTYPE       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXTYPE);
  DBIERR_LANGDRVMISMATCH        = (ERRBASE_INVALIDREQ + ERRCODE_LANGDRVMISMATCH);
  DBIERR_NOSUCHFILTER           = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHFILTER);
  DBIERR_INVALIDFILTER          = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFILTER);
  DBIERR_INVALIDTABLECREATE     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTABLECREATE);
  DBIERR_INVALIDTABLEDELETE     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTABLEDELETE);
  DBIERR_INVALIDINDEXCREATE     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXCREATE);
  DBIERR_INVALIDINDEXDELETE     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXDELETE);
  DBIERR_INVALIDTABLE           = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTABLE);
  DBIERR_MULTIRESULTS           = (ERRBASE_INVALIDREQ + ERRCODE_MULTIRESULTS);
  DBIERR_INVALIDTIME            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTIME);
  DBIERR_INVALIDDATE            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDDATE);
  DBIERR_INVALIDTIMESTAMP       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTIMESTAMP);
  DBIERR_DIFFERENTPATH          = (ERRBASE_INVALIDREQ + ERRCODE_DIFFERENTPATH);
  DBIERR_MISMATCHARGS           = (ERRBASE_INVALIDREQ + ERRCODE_MISMATCHARGS);
  DBIERR_FUNCTIONNOTFOUND       = (ERRBASE_INVALIDREQ + ERRCODE_FUNCTIONNOTFOUND);
  DBIERR_MUSTUSEBASEORDER       = (ERRBASE_INVALIDREQ + ERRCODE_MUSTUSEBASEORDER);
  DBIERR_INVALIDPROCEDURENAME   = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDPROCEDURENAME);
  DBIERR_INVALIDFLDMAP          = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFLDMAP);

{ ERRCAT_LOCKCONFLICT }
{ =================== }

  ERRCODE_LOCKED                = 1;
  ERRCODE_UNLOCKFAILED          = 2;
  ERRCODE_FILEBUSY              = 3;
  ERRCODE_DIRBUSY               = 4;
  ERRCODE_FILELOCKED            = 5;
  ERRCODE_DIRLOCKED             = 6;
  ERRCODE_ALREADYLOCKED         = 7;
  ERRCODE_NOTLOCKED             = 8;
  ERRCODE_LOCKTIMEOUT           = 9;
  ERRCODE_GROUPLOCKED           = 10;     { 0x0a }
  ERRCODE_LOSTTBLLOCK           = 11;     { 0x0b }
  ERRCODE_LOSTEXCLACCESS        = 12;     { 0x0c }
  ERRCODE_NEEDEXCLACCESS        = 13;     { 0x0d }
  ERRCODE_RECGROUPCONFLICT      = 14;     { 0x0e }
  ERRCODE_DEADLOCK              = 15;
  ERRCODE_ACTIVETRAN            = 16;
  ERRCODE_NOACTIVETRAN          = 17;
  ERRCODE_RECLOCKFAILED         = 18;
  ERRCODE_OPTRECLOCKFAILED      = 19;
  ERRCODE_OPTRECLOCKRECDEL      = 20;
  ERRCODE_LOCKEDRECS            = 21;
  ERRCODE_NEEDWRITELOCK         = 22;

  DBIERR_LOCKED                 = (ERRBASE_LOCKCONFLICT + ERRCODE_LOCKED);
  DBIERR_UNLOCKFAILED           = (ERRBASE_LOCKCONFLICT + ERRCODE_UNLOCKFAILED);
  DBIERR_FILEBUSY               = (ERRBASE_LOCKCONFLICT + ERRCODE_FILEBUSY);
  DBIERR_DIRBUSY                = (ERRBASE_LOCKCONFLICT + ERRCODE_DIRBUSY);
  DBIERR_FILELOCKED             = (ERRBASE_LOCKCONFLICT + ERRCODE_FILELOCKED);
  DBIERR_DIRLOCKED              = (ERRBASE_LOCKCONFLICT + ERRCODE_DIRLOCKED);
  DBIERR_ALREADYLOCKED          = (ERRBASE_LOCKCONFLICT + ERRCODE_ALREADYLOCKED);
  DBIERR_NOTLOCKED              = (ERRBASE_LOCKCONFLICT + ERRCODE_NOTLOCKED);
  DBIERR_LOCKTIMEOUT            = (ERRBASE_LOCKCONFLICT + ERRCODE_LOCKTIMEOUT);
  DBIERR_GROUPLOCKED            = (ERRBASE_LOCKCONFLICT + ERRCODE_GROUPLOCKED);
  DBIERR_LOSTTBLLOCK            = (ERRBASE_LOCKCONFLICT + ERRCODE_LOSTTBLLOCK);
  DBIERR_LOSTEXCLACCESS         = (ERRBASE_LOCKCONFLICT + ERRCODE_LOSTEXCLACCESS);
  DBIERR_NEEDEXCLACCESS         = (ERRBASE_LOCKCONFLICT  + ERRCODE_NEEDEXCLACCESS);
  DBIERR_RECGROUPCONFLICT       = (ERRBASE_LOCKCONFLICT + ERRCODE_RECGROUPCONFLICT);
  DBIERR_DEADLOCK               = (ERRBASE_LOCKCONFLICT + ERRCODE_DEADLOCK);
  DBIERR_ACTIVETRAN             = (ERRBASE_LOCKCONFLICT + ERRCODE_ACTIVETRAN);
  DBIERR_NOACTIVETRAN           = (ERRBASE_LOCKCONFLICT + ERRCODE_NOACTIVETRAN);
  DBIERR_RECLOCKFAILED          = (ERRBASE_LOCKCONFLICT + ERRCODE_RECLOCKFAILED);
  DBIERR_OPTRECLOCKFAILED       = (ERRBASE_LOCKCONFLICT + ERRCODE_OPTRECLOCKFAILED);
  DBIERR_OPTRECLOCKRECDEL       = (ERRBASE_LOCKCONFLICT + ERRCODE_OPTRECLOCKRECDEL);

{ ERRCAT_SECURITY }
{ =============== }

  ERRCODE_NOTSUFFFIELDRIGHTS    = 1;      { Not sufficient field  rights for operation }
  ERRCODE_NOTSUFFTABLERIGHTS    = 2;      { Not sufficient table  rights for operation }
  ERRCODE_NOTSUFFFAMILYRIGHTS   = 3;      { Not sufficient family rights for operation }
  ERRCODE_READONLYDIR           = 4;      { Is a read-only directory }
  ERRCODE_READONLYDB            = 5;      { Database is read-only }
  ERRCODE_READONLYFLD           = 6;      { Trying to modify read-only field }
  ERRCODE_TBLENCRYPTED          = 7;      { Table is encrypted (dBASE only) }
  ERRCODE_NOTSUFFSQLRIGHTS      = 8;      { Not sufficient sql rights for operation }


  DBIERR_NOTSUFFFIELDRIGHTS     = (ERRBASE_SEC + ERRCODE_NOTSUFFFIELDRIGHTS);
  DBIERR_NOTSUFFTABLERIGHTS     = (ERRBASE_SEC + ERRCODE_NOTSUFFTABLERIGHTS);
  DBIERR_NOTSUFFFAMILYRIGHTS    = (ERRBASE_SEC + ERRCODE_NOTSUFFFAMILYRIGHTS);
  DBIERR_READONLYDIR            = (ERRBASE_SEC + ERRCODE_READONLYDIR);
  DBIERR_READONLYDB             = (ERRBASE_SEC + ERRCODE_READONLYDB);
  DBIERR_READONLYFLD            = (ERRBASE_SEC + ERRCODE_READONLYFLD);
  DBIERR_TBLENCRYPTED           = (ERRBASE_SEC + ERRCODE_TBLENCRYPTED);
  DBIERR_NOTSUFFSQLRIGHTS       = (ERRBASE_SEC + ERRCODE_NOTSUFFSQLRIGHTS);


{ ERRCAT_INVALIDCONTEXT }
{ ===================== }

  ERRCODE_NOTABLOB              = 1;      { Field is not a blob }
  ERRCODE_BLOBOPENED            = 2;      { Blob already opened }
  ERRCODE_BLOBNOTOPENED         = 3;      { Blob not opened }
  ERRCODE_NA                    = 4;      { Operation not applicable }
  ERRCODE_NOTINDEXED            = 5;      { Table is not indexed }
  ERRCODE_NOTINITIALIZED        = 6;      { Engine not initialized }
  ERRCODE_MULTIPLEINIT          = 7;      { Attempt to re-initialize engine }
  ERRCODE_NOTSAMESESSION        = 8;      { Attempt to mix objs from diff ses }
  ERRCODE_PDXDRIVERNOTACTIVE    = 9;      { Paradox driver not active }
  ERRCODE_DRIVERNOTLOADED       = 10;     { 0x0a Driver not loaded }
  ERRCODE_TABLEREADONLY         = 11;     { 0x0b Table is read only }
  ERRCODE_NOASSOCINDEX          = 12;     { 0x0c No index associated with the cursor }
  ERRCODE_HASOPENCURSORS        = 13;     { 0x0d Has open cursors }
  ERRCODE_NOTABLESUPPORT        = 14;     { 0x0e Op cannot be done on this table }
  ERRCODE_INDEXREADONLY         = 15;     { 0x0f Index is read only }
  ERRCODE_NOUNIQUERECS          = 16;     { 0x10 Records are not unique }
  ERRCODE_NOTCURSESSION         = 17;     { 0x11 Not the current/active session }
  ERRCODE_INVALIDKEYWORD        = 18;     { 0x12 Invalid use of keyword. }
  ERRCODE_CONNECTINUSE          = 19;     { 0x13 Connection in use }
  ERRCODE_CONNECTNOTSHARED      = 20;     { 0x14 Passthru SQL connection not share }


  DBIERR_NOTABLOB               = (ERRBASE_IC + ERRCODE_NOTABLOB);
  DBIERR_BLOBOPENED             = (ERRBASE_IC + ERRCODE_BLOBOPENED);
  DBIERR_BLOBNOTOPENED          = (ERRBASE_IC + ERRCODE_BLOBNOTOPENED);
  DBIERR_NA                     = (ERRBASE_IC + ERRCODE_NA);
  DBIERR_NOTINDEXED             = (ERRBASE_IC + ERRCODE_NOTINDEXED);
  DBIERR_NOTINITIALIZED         = (ERRBASE_IC + ERRCODE_NOTINITIALIZED);
  DBIERR_MULTIPLEINIT           = (ERRBASE_IC + ERRCODE_MULTIPLEINIT);
  DBIERR_NOTSAMESESSION         = (ERRBASE_IC + ERRCODE_NOTSAMESESSION);
  DBIERR_PDXDRIVERNOTACTIVE     = (ERRBASE_IC + ERRCODE_PDXDRIVERNOTACTIVE);
  DBIERR_DRIVERNOTLOADED        = (ERRBASE_IC + ERRCODE_DRIVERNOTLOADED);
  DBIERR_TABLEREADONLY          = (ERRBASE_IC + ERRCODE_TABLEREADONLY);
  DBIERR_NOASSOCINDEX           = (ERRBASE_IC + ERRCODE_NOASSOCINDEX);
  DBIERR_HASOPENCURSORS         = (ERRBASE_IC + ERRCODE_HASOPENCURSORS);
  DBIERR_NOTABLESUPPORT         = (ERRBASE_IC + ERRCODE_NOTABLESUPPORT);
  DBIERR_INDEXREADONLY          = (ERRBASE_IC + ERRCODE_INDEXREADONLY);
  DBIERR_NOUNIQUERECS           = (ERRBASE_IC + ERRCODE_NOUNIQUERECS);
  DBIERR_NOTCURSESSION          = (ERRBASE_IC + ERRCODE_NOTCURSESSION);
  DBIERR_INVALIDKEYWORD         = (ERRBASE_IC + ERRCODE_INVALIDKEYWORD);
  DBIERR_CONNECTINUSE           = (ERRBASE_IC + ERRCODE_CONNECTINUSE);
  DBIERR_CONNECTNOTSHARED       = (ERRBASE_IC + ERRCODE_CONNECTNOTSHARED);


{ ERRCAT_OS }
{ ========= }
{ DOS extended errors: }

  ERRCODE_OSEINVFNC             = 1;      { Invalid function number }
  ERRCODE_OSENOENT              = 2;      { No such file or directory }
  ERRCODE_OSENOPATH             = 3;      { Path not found }
  ERRCODE_OSEMFILE              = 4;      { Too many open files }
  ERRCODE_OSEACCES              = 5;      { Permission denied }
  ERRCODE_OSEBADF               = 6;      { Bad file number }
  ERRCODE_OSECONTR              = 7;      { Memory blocks destroyed }
  ERRCODE_OSENOMEM              = 8;      { Not enough core }
  ERRCODE_OSEINVMEM             = 9;      { Invalid memory block address }
  ERRCODE_OSEINVENV             = 10;     { 0x0a Invalid environment }
  ERRCODE_OSEINVFMT             = 11;     { 0x0b Invalid format }
  ERRCODE_OSEINVACC             = 12;     { 0x0c Invalid access code }
  ERRCODE_OSEINVDAT             = 13;     { 0x0d Invalid data }
  ERRCODE_OSENODEV              = 15;     { 0x0f No such device }
  ERRCODE_OSECURDIR             = 16;     { 0x10 Attempt to remove curdir }
  ERRCODE_OSENOTSAM             = 17;     { 0x11 Not same device }
  ERRCODE_OSENMFILE             = 18;     { 0x12 No more files }
  ERRCODE_OSEINVAL              = 19;     { 0x13 Invalid argument }
  ERRCODE_OSE2BIG               = 20;     { 0x14 Arg list too long }
  ERRCODE_OSENOEXEC             = 21;     { 0x15 Exec format error }
  ERRCODE_OSEXDEV               = 22;     { 0x16 Cross-device link }
  ERRCODE_OSEDOM                = 33;     { 0x21 Math argument }
  ERRCODE_OSERANGE              = 34;     { 0x22 Result to large }
  ERRCODE_OSEEXIST              = 35;     { 0x23 File already exists }
  ERRCODE_OSUNKNOWN             = 39;     { 0x27 Unkown | illegal error from rtl }

  ERRCODE_OSSHAREVIOL           = 50;     { 0x32 Share viol, ext. err 0x20 }
  ERRCODE_OSLOCKVIOL            = 51;     { 0x33 Lock viol, ext. err 0x21 }
  ERRCODE_OSINT24FAIL           = 52;     { 0x34 INT24 called }
  ERRCODE_OSDRIVENOTREADY       = 53;     { 0x35 Drive not ready }



{ OTHER Os errors: }
{ 1. idapi errors  }
{ 2. errors from non-dos systems ( i.e. NOVELL ) }

  ERRCODE_NOTEXACT              = 100;    { 0x64 Not exact read/write }
  ERRCODE_OSNETERR              = 101;    { 0x65 Generic network error }
  ERRCODE_OSUNKNOWNSRVERR       = 102;    { 0x66 Error from file server }
  ERRCODE_SERVERNOMEMORY        = 103;    { 0x67 Server out of memory }
  ERRCODE_OSALREADYLOCKED       = 104;    { 0x68 Record already locked (by you) }
  ERRCODE_OSNOTLOCKED           = 105;    { 0x69 Record not locked }
  ERRCODE_NOSERVERSW            = 106;    { 0x6a Server software not running the workstation/server }


  DBIERR_OSEINVFNC              = ( ERRBASE_OS + ERRCODE_OSEINVFNC );
  DBIERR_OSENOENT               = ( ERRBASE_OS + ERRCODE_OSENOENT );
  DBIERR_OSENOPATH              = ( ERRBASE_OS + ERRCODE_OSENOPATH );
  DBIERR_OSEMFILE               = ( ERRBASE_OS + ERRCODE_OSEMFILE );
  DBIERR_OSEACCES               = ( ERRBASE_OS + ERRCODE_OSEACCES );
  DBIERR_OSEBADF                = ( ERRBASE_OS + ERRCODE_OSEBADF );
  DBIERR_OSECONTR               = ( ERRBASE_OS + ERRCODE_OSECONTR );
  DBIERR_OSENOMEM               = ( ERRBASE_OS + ERRCODE_OSENOMEM );
  DBIERR_OSEINVMEM              = ( ERRBASE_OS + ERRCODE_OSEINVMEM );
  DBIERR_OSEINVENV              = ( ERRBASE_OS + ERRCODE_OSEINVENV );
  DBIERR_OSEINVFMT              = ( ERRBASE_OS + ERRCODE_OSEINVFMT );
  DBIERR_OSEINVACC              = ( ERRBASE_OS + ERRCODE_OSEINVACC );
  DBIERR_OSEINVDAT              = ( ERRBASE_OS + ERRCODE_OSEINVDAT );
  DBIERR_OSENODEV               = ( ERRBASE_OS + ERRCODE_OSENODEV );
  DBIERR_OSECURDIR              = ( ERRBASE_OS + ERRCODE_OSECURDIR );
  DBIERR_OSENOTSAM              = ( ERRBASE_OS + ERRCODE_OSENOTSAM );
  DBIERR_OSENMFILE              = ( ERRBASE_OS + ERRCODE_OSENMFILE );
  DBIERR_OSEINVAL               = ( ERRBASE_OS + ERRCODE_OSEINVAL );
  DBIERR_OSE2BIG                = ( ERRBASE_OS + ERRCODE_OSE2BIG );
  DBIERR_OSENOEXEC              = ( ERRBASE_OS + ERRCODE_OSENOEXEC );
  DBIERR_OSEXDEV                = ( ERRBASE_OS + ERRCODE_OSEXDEV );
  DBIERR_OSEDOM                 = ( ERRBASE_OS + ERRCODE_OSEDOM );
  DBIERR_OSERANGE               = ( ERRBASE_OS + ERRCODE_OSERANGE );
  DBIERR_OSEEXIST               = ( ERRBASE_OS + ERRCODE_OSEEXIST );
  DBIERR_OSUNKNOWN              = ( ERRBASE_OS + ERRCODE_OSUNKNOWN );
  DBIERR_OSSHAREVIOL            = ( ERRBASE_OS + ERRCODE_OSSHAREVIOL );
  DBIERR_OSLOCKVIOL             = ( ERRBASE_OS + ERRCODE_OSLOCKVIOL );
  DBIERR_OSNETERR               = ( ERRBASE_OS + ERRCODE_OSNETERR );
  DBIERR_OSINT24FAIL            = ( ERRBASE_OS + ERRCODE_OSINT24FAIL );
  DBIERR_OSDRIVENOTREADY        = ( ERRBASE_OS + ERRCODE_OSDRIVENOTREADY );


  DBIERR_NOTEXACT               = ( ERRBASE_OS + ERRCODE_NOTEXACT );
  DBIERR_OSUNKNOWNSRVERR        = ( ERRBASE_OS + ERRCODE_OSUNKNOWNSRVERR );
  DBIERR_SERVERNOMEMORY         = ( ERRBASE_OS + ERRCODE_SERVERNOMEMORY );
  DBIERR_OSALREADYLOCKED        = ( ERRBASE_OS + ERRCODE_OSALREADYLOCKED );
  DBIERR_OSNOTLOCKED            = ( ERRBASE_OS + ERRCODE_OSNOTLOCKED );
  DBIERR_NOSERVERSW             = ( ERRBASE_OS + ERRCODE_NOSERVERSW);

{ ERRCAT_NETWORK }
{ ============== }

  ERRCODE_NETINITERR            = 1;      { Net init failed }
  ERRCODE_NETUSERLIMIT          = 2;      { Net user limit exceeded }
  ERRCODE_NETFILEVERSION        = 3;      { Wrong net file version }
  ERRCODE_NETFILELOCKED         = 4;      { Not able to lock net file }
  ERRCODE_DIRNOTPRIVATE         = 5;
  ERRCODE_NETMULTIPLE           = 6;      { Multiple net files in use }
  ERRCODE_NETUNKNOWN            = 7;      { Unknown net error }
  ERRCODE_SHAREDFILE            = 8;      { Cannot access a shared file }
  ERRCODE_SHARENOTLOADED        = 9;      { Share not loaded }
  ERRCODE_NOTONANETWORK         = 10;     { 0x0a Not an Network }
  ERRCODE_SQLCOMMLOST           = 11;     { 0x0b Lost Communication with SQL server }
  ERRCODE_SERVERCOMMLOST        = 12;     { 0x0c Lost Communication with IDAPI server }
  ERRCODE_SQLSERVERNOTFOUND     = 13;     { 0x0d SQL Server not found }
  ERRCODE_SERVERNOTFOUND        = 14;     { 0x0e SQL Server not found }

  DBIERR_NETINITERR             = (ERRBASE_NETWORK + ERRCODE_NETINITERR);
  DBIERR_NETUSERLIMIT           = (ERRBASE_NETWORK + ERRCODE_NETUSERLIMIT);
  DBIERR_NETFILEVERSION         = (ERRBASE_NETWORK + ERRCODE_NETFILEVERSION);
  DBIERR_NETFILELOCKED          = (ERRBASE_NETWORK + ERRCODE_NETFILELOCKED);
  DBIERR_DIRNOTPRIVATE          = (ERRBASE_NETWORK + ERRCODE_DIRNOTPRIVATE);
  DBIERR_NETMULTIPLE            = (ERRBASE_NETWORK + ERRCODE_NETMULTIPLE);
  DBIERR_NETUNKNOWN             = (ERRBASE_NETWORK + ERRCODE_NETUNKNOWN);
  DBIERR_SHAREDFILE             = (ERRBASE_NETWORK + ERRCODE_SHAREDFILE);
  DBIERR_SHARENOTLOADED         = (ERRBASE_NETWORK + ERRCODE_SHARENOTLOADED);
  DBIERR_NOTONANETWORK          = (ERRBASE_NETWORK + ERRCODE_NOTONANETWORK);
  DBIERR_SQLCOMMLOST            = (ERRBASE_NETWORK + ERRCODE_SQLCOMMLOST);
  DBIERR_SERVERCOMMLOST         = (ERRBASE_NETWORK + ERRCODE_SERVERCOMMLOST);
  DBIERR_SQLSERVERNOTFOUND      = (ERRBASE_NETWORK + ERRCODE_SQLSERVERNOTFOUND);
  DBIERR_SERVERNOTFOUND         = (ERRBASE_NETWORK + ERRCODE_SERVERNOTFOUND);

{ ERRCAT_DRIVER }
{ ============= }

  ERRCODE_WRONGDRVNAME          = 1;      { Wrong driver name }
  ERRCODE_WRONGSYSVER           = 2;      { Wrong system version }
  ERRCODE_WRONGDRVVER           = 3;      { Wrong driver version }
  ERRCODE_WRONGDRVTYPE          = 4;      { Wrong driver type }
  ERRCODE_CANNOTLOADDRV         = 5;      { Can not load driver }
  ERRCODE_CANNOTLOADLDDRV       = 6;      { Can not load language driver }
  ERRCODE_VENDINITFAIL          = 7;      { Vendor init failure }
  ERRCODE_DRIVERRESTRICTED      = 8;      { Client not enabled for this driver }


  DBIERR_WRONGDRVNAME           = (ERRBASE_DRIVER + ERRCODE_WRONGDRVNAME);
  DBIERR_WRONGSYSVER            = (ERRBASE_DRIVER + ERRCODE_WRONGSYSVER);
  DBIERR_WRONGDRVVER            = (ERRBASE_DRIVER + ERRCODE_WRONGDRVVER);
  DBIERR_WRONGDRVTYPE           = (ERRBASE_DRIVER + ERRCODE_WRONGDRVTYPE);
  DBIERR_CANNOTLOADDRV          = (ERRBASE_DRIVER + ERRCODE_CANNOTLOADDRV);
  DBIERR_CANNOTLOADLDDRV        = (ERRBASE_DRIVER + ERRCODE_CANNOTLOADLDDRV);
  DBIERR_VENDINITFAIL           = (ERRBASE_DRIVER + ERRCODE_VENDINITFAIL);
  DBIERR_DRIVERRESTRICTED       = (ERRBASE_DRIVER + ERRCODE_DRIVERRESTRICTED);


{ ERRCAT_QUERY }
{ ============ }



  DBICODE_AMBJOASY              = 1;      { obsolete }
  DBICODE_AMBJOSYM              = 2;      { obsolete }
  DBICODE_AMBOUTEX              = 3;
  DBICODE_AMBOUTPR              = 4;      { obsolete }
  DBICODE_AMBSYMAS              = 5;      { obsolete }
  DBICODE_ASETOPER              = 6;
  DBICODE_AVENUMDA              = 7;
  DBICODE_BADEXPR1              = 8;
  DBICODE_BADFLDOR              = 9;
  DBICODE_BADVNAME              = 10;     { 0x0a }
  DBICODE_BITMAPER              = 11;     { 0x0b }
  DBICODE_CALCBADR              = 12;     { 0x0c }
  DBICODE_CALCTYPE              = 13;     { 0x0d }
  DBICODE_CHGTO1TI              = 14;     { 0x0e }
  DBICODE_CHGTOCHG              = 15;     { 0x0f }
  DBICODE_CHGTOEXP              = 16;     { 0x10 }
  DBICODE_CHGTOINS              = 17;     { 0x11 }
  DBICODE_CHGTONEW              = 18;     { 0x12 }
  DBICODE_CHGTOVAL              = 19;     { 0x13 }
  DBICODE_CHKMRKFI              = 20;     { 0x14 }
  DBICODE_CHNAMBIG              = 21;     { 0x15 }
  DBICODE_CHUNKERR              = 22;     { 0x16 }
  DBICODE_COLUM255              = 23;     { 0x17 }
  DBICODE_CONAFTAS              = 24;     { 0x18 }
  DBICODE_DEL1TIME              = 25;     { 0x19 }
  DBICODE_DELAMBIG              = 26;     { 0x1a }
  DBICODE_DELFRDEL              = 27;     { 0x1b }
  DBICODE_EGFLDTYP              = 28;     { 0x1c }
  DBICODE_EXAMINOR              = 29;     { 0x1d }
  DBICODE_EXPRTYPS              = 30;     { 0x1e }
  DBICODE_EXTRACOM              = 31;     { 0x1f }
  DBICODE_EXTRAORO              = 32;     { 0x20 }
  DBICODE_EXTRAQRO              = 33;     { 0x21 }
  DBICODE_FIND1ATT              = 34;     { 0x22 }
  DBICODE_FINDANST              = 35;     { 0x23 }
  DBICODE_GRPNOSET              = 36;     { 0x24 }
  DBICODE_GRPSTROW              = 37;     { 0x25 }
  DBICODE_IDFINLCO              = 38;     { 0x26 }
  DBICODE_IDFPERLI              = 39;     { 0x27 }
  DBICODE_INANEXPR              = 40;     { 0x28 }
  DBICODE_INS1TIME              = 41;     { 0x29 }
  DBICODE_INSAMBIG              = 42;     { 0x2a }
  DBICODE_INSDELCH              = 43;     { 0x2b }
  DBICODE_INSEXPRR              = 44;     { 0x2c }
  DBICODE_INSTOINS              = 45;     { 0x2d }
  DBICODE_ISARRAY               = 46;     { 0x2e }
  DBICODE_LABELERR              = 47;     { 0x2f }
  DBICODE_LINKCALC              = 48;     { 0x30 }
  DBICODE_LNGVNAME              = 49;     { 0x31 }
  DBICODE_LONGQURY              = 50;     { 0x32 }
  DBICODE_MEMVPROC              = 51;     { 0x33 }
  DBICODE_MISNGCOM              = 52;     { 0x34 }
  DBICODE_MISNGRPA              = 53;     { 0x35 }
  DBICODE_MISSRTQU              = 54;     { 0x36 }
  DBICODE_NAMTWICE              = 55;     { 0x37 }
  DBICODE_NOCHKMAR              = 56;     { 0x38 }
  DBICODE_NODEFOCC              = 57;     { 0x39 }
  DBICODE_NOGROUPS              = 58;     { 0x3a }
  DBICODE_NONSENSE              = 59;     { 0x3b }
  DBICODE_NOPATTER              = 60;     { 0x3c }
  DBICODE_NOSUCHDA              = 61;     { 0x3d }
  DBICODE_NOVALUE               = 62;     { 0x3e }
  DBICODE_ONLYCONS              = 63;     { 0x3f }
  DBICODE_ONLYSETR              = 64;     { 0x40 }
  DBICODE_OUTSENS1              = 65;     { 0x41 }
  DBICODE_OUTTWIC1              = 66;     { 0x42 }
  DBICODE_PAROWCNT              = 67;     { 0x43 }
  DBICODE_PERSEPAR              = 68;     { 0x44 }
  DBICODE_PROCPLSW              = 69;     { 0x45 }
  DBICODE_PWINSRTS              = 70;     { 0x46 }
  DBICODE_PWMODRTS              = 71;     { 0x47 }
  DBICODE_QBEFLDFOUND           = 72;     { 0x48 }
  DBICODE_QBENOFENCE            = 73;     { 0x49 }
  DBICODE_QBENOFENCET           = 74;     { 0x4a }
  DBICODE_QBENOHEADERT          = 75;     { 0x4b }
  DBICODE_QBENOTAB              = 76;     { 0x4c }
  DBICODE_QBENUMCOLS            = 77;     { 0x4d }
  DBICODE_QBEOPENTAB            = 78;     { 0x4e }
  DBICODE_QBETWICE              = 79;     { 0x4f }
  DBICODE_QRYNOANSWER           = 80;     { 0x50 }
  DBICODE_QRYNOTPREP            = 81;     { 0x51 }
  DBICODE_QUAINDEL              = 82;     { 0x52 }
  DBICODE_QUAININS              = 83;     { 0x53 }
  DBICODE_RAGININS              = 84;     { 0x54 }
  DBICODE_RAGINSET              = 85;     { 0x55 }
  DBICODE_ROWUSERR              = 86;     { 0x56 }
  DBICODE_SETEXPEC              = 87;     { 0x57 }
  DBICODE_SETVAMB1              = 88;     { 0x58 }
  DBICODE_SETVBAD1              = 89;     { 0x59 }
  DBICODE_SETVDEF1              = 90;     { 0x5a }
  DBICODE_SUMNUMBE              = 91;     { 0x5b }
  DBICODE_TBLISWP3              = 92;     { 0x5c }
  DBICODE_TOKENNOT              = 93;     { 0x5d }
  DBICODE_TWOOUTR1              = 94;     { 0x5e }
  DBICODE_TYPEMISM              = 95;     { 0x5f }
  DBICODE_UNRELQ1               = 96;     { 0x60 }
  DBICODE_UNUSEDST              = 97;     { 0x61 }
  DBICODE_USEINSDE              = 98;     { 0x62 }
  DBICODE_USEOFCHG              = 99;     { 0x63 }
  DBICODE_VARMUSTF              = 100;    { 0x64 }
  DBICODE_REGISTER              = 101;    { 0x65 }
  DBICODE_LONGEXPR              = 102;    { 0x66 }
  DBICODE_REFRESH               = 103;    { 0x67 }
  DBICODE_CANCEXCEPT            = 104;    { 0x68 }
  DBICODE_DBEXCEPT              = 105;    { 0x69 }
  DBICODE_MEMEXCEPT             = 106;    { 0x6a }
  DBICODE_FATALEXCEPT           = 107;    { 0x6b }
  DBICODE_QRYNIY                = 108;    { 0x6c }
  DBICODE_BADFORMAT             = 109;    { 0x6d }
  DBICODE_QRYEMPTY              = 110;    { 0x6e }
  DBICODE_NOQRYTOPREP           = 111;    { 0x6f }
  DBICODE_BUFFTOOSMALL          = 112;    { 0x70 }
  DBICODE_QRYNOTPARSE           = 113;    { 0x71 }
  DBICODE_NOTHANDLE             = 114;    { 0x72 }
  DBICODE_QRYSYNTERR            = 115;    { 0x73 }
  DBICODE_QXFLDCOUNT            = 116;    { 0x74 }
  DBICODE_QXFLDSYMNOTFOUND      = 117;    { 0x75 }
  DBICODE_QXTBLSYMNOTFOUND      = 118;    { 0x76 }
  DBICODE_BLOBTERM              = 119;    { 0x77 }
  DBICODE_BLOBERR               = 120;    { 0x78 }
  DBICODE_RESTARTQRY            = 121;    { 0x79 }
  DBICODE_UNKNOWNANSTYPE        = 122;    { 0x7a }

{ Internal QBE use Only. }
  DBICODE_SQLG_MDIST            = 123;    { 0x7b }
  DBICODE_SQLG_NOARI            = 124;    { 0x7c }
  DBICODE_SQLG_LIKEN            = 125;    { 0x7d }
  DBICODE_SQLG_ALPHO            = 126;    { 0x7e }
  DBICODE_SQLG_DATEO            = 127;    { 0x7f }
  DBICODE_SQLG_RELOP            = 128;    { 0x80 }
  DBICODE_SQLG_ONLYC            = 129;    { 0x81 }
  DBICODE_SQLG_CNTLN            = 130;    { 0x82 }
  DBICODE_SQLG_CHINI            = 131;    { 0x83 }
  DBICODE_SQLG_UNION            = 132;    { 0x84 }
  DBICODE_SQLG_SLFIN            = 133;    { 0x85 }
  DBICODE_SQLG_OTJVR            = 134;    { 0x86 }
  DBICODE_SQLG_STROW            = 135;    { 0x87 }
  DBICODE_SQLG_QUANT            = 136;    { 0x88 }
  DBICODE_SQLG_REGSO            = 137;    { 0x89 }
  DBICODE_SQLG_COUNT            = 138;    { 0x8a }
  DBICODE_SQLG_AVERA            = 139;    { 0x8b }
  DBICODE_SQLG_DATEA            = 140;    { 0x8c }
  DBICODE_SQLG_BADPT            = 141;    { 0x8d }
  DBICODE_SQLG_RELPA            = 142;    { 0x8e }
  DBICODE_SQLG_PATRN            = 143;    { 0x8f }
  DBICODE_SQLG_FNDSU            = 144;    { 0x90 }
  DBICODE_SQLG_IFDCS            = 145;    { 0x91 }
  DBICODE_SQLG_IDCCO            = 146;    { 0x92 }
  DBICODE_SQLG_ONLYI            = 147;    { 0x93 }
  DBICODE_SQLG_SQLDIALECT       = 148;    { 0x94 }
  DBICODE_SQLG_NOQUERY          = 149;    { 0x95 }
{ End of Internal.       }

  DBICODE_BLOBGROUP             = 150;    { 0x96 }
  DBICODE_QRYNOPROP             = 151;    { 0x97 }
  DBICODE_ANSTYPNOTSUP          = 152;    { 0x98 }
  DBICODE_ANSALIASNOTSUP        = 153;    { 0x99 }
  DBICODE_INSBLOBREQ            = 154;    { 0x9a }
  DBICODE_CHGUNIQUENDXREQ       = 155;    { 0x9b }
  DBICODE_DELUNIQUENDXREQ       = 156;    { 0x9c }
  DBICODE_SQLNOFULLUPDATE       = 157;    { 0x9d }
  DBICODE_CANTEXECREMOTE        = 158;    { 0x9e }
  DBICODE_UNEXPECTEDEOC         = 159;    { 0x9f }
  DBICODE_SQLPARAMNOTSET        = 160;    { 0xA0 }
  DBICODE_QUERYTOOLONG          = 161;    { 0xA1 }

{ Errors added for localsql }
  DBICODE_NOSUCHRELORALIAS      = 170;
  DBICODE_TYPEAMBIGUITY         = 171;
  DBICODE_ORDERBYNOTAPROJ       = 172;
  DBICODE_SQLPARSE              = 173;
  DBICODE_CONSTRAINTFAILED      = 174;
  DBICODE_NOTGROUPINGFIELD      = 175;
  DBICODE_UDFNOTDEFINED         = 176;
  DBICODE_UDFERROR              = 177;
  DBICODE_SINGLEROWERROR        = 178;
  DBICODE_GROUPEXPR             = 179;
  DBICODE_QUERYTEXT             = 180;
  DBICODE_ANSIJOINSUP           = 181;
  DBICODE_DISTUNION             = 182;
  DBICODE_GROUPBYREQ            = 183;
  DBICODE_INSUPDAUTOIC          = 184;
  DBICODE_UPDREFINTSINGLE       = 185;

  DBIERR_AMBJOASY               = (ERRBASE_QUERY+DBICODE_AMBJOASY);
  DBIERR_AMBJOSYM               = (ERRBASE_QUERY+DBICODE_AMBJOSYM);
  DBIERR_AMBOUTEX               = (ERRBASE_QUERY+DBICODE_AMBOUTEX);
  DBIERR_AMBOUTPR               = (ERRBASE_QUERY+DBICODE_AMBOUTPR);
  DBIERR_AMBSYMAS               = (ERRBASE_QUERY+DBICODE_AMBSYMAS);
  DBIERR_ASETOPER               = (ERRBASE_QUERY+DBICODE_ASETOPER);
  DBIERR_AVENUMDA               = (ERRBASE_QUERY+DBICODE_AVENUMDA);
  DBIERR_BADEXPR1               = (ERRBASE_QUERY+DBICODE_BADEXPR1);
  DBIERR_BADFLDOR               = (ERRBASE_QUERY+DBICODE_BADFLDOR);
  DBIERR_BADVNAME               = (ERRBASE_QUERY+DBICODE_BADVNAME);
  DBIERR_BITMAPER               = (ERRBASE_QUERY+DBICODE_BITMAPER);
  DBIERR_CALCBADR               = (ERRBASE_QUERY+DBICODE_CALCBADR);
  DBIERR_CALCTYPE               = (ERRBASE_QUERY+DBICODE_CALCTYPE);
  DBIERR_CHGTO1TI               = (ERRBASE_QUERY+DBICODE_CHGTO1TI);
  DBIERR_CHGTOCHG               = (ERRBASE_QUERY+DBICODE_CHGTOCHG);
  DBIERR_CHGTOEXP               = (ERRBASE_QUERY+DBICODE_CHGTOEXP);
  DBIERR_CHGTOINS               = (ERRBASE_QUERY+DBICODE_CHGTOINS);
  DBIERR_CHGTONEW               = (ERRBASE_QUERY+DBICODE_CHGTONEW);
  DBIERR_CHGTOVAL               = (ERRBASE_QUERY+DBICODE_CHGTOVAL);
  DBIERR_CHKMRKFI               = (ERRBASE_QUERY+DBICODE_CHKMRKFI);
  DBIERR_CHNAMBIG               = (ERRBASE_QUERY+DBICODE_CHNAMBIG);
  DBIERR_CHUNKERR               = (ERRBASE_QUERY+DBICODE_CHUNKERR);
  DBIERR_COLUM255               = (ERRBASE_QUERY+DBICODE_COLUM255);
  DBIERR_CONAFTAS               = (ERRBASE_QUERY+DBICODE_CONAFTAS);
  DBIERR_DEL1TIME               = (ERRBASE_QUERY+DBICODE_DEL1TIME);
  DBIERR_DELAMBIG               = (ERRBASE_QUERY+DBICODE_DELAMBIG);
  DBIERR_DELFRDEL               = (ERRBASE_QUERY+DBICODE_DELFRDEL);
  DBIERR_EGFLDTYP               = (ERRBASE_QUERY+DBICODE_EGFLDTYP);
  DBIERR_EXAMINOR               = (ERRBASE_QUERY+DBICODE_EXAMINOR);
  DBIERR_EXPRTYPS               = (ERRBASE_QUERY+DBICODE_EXPRTYPS);
  DBIERR_EXTRACOM               = (ERRBASE_QUERY+DBICODE_EXTRACOM);
  DBIERR_EXTRAORO               = (ERRBASE_QUERY+DBICODE_EXTRAORO);
  DBIERR_EXTRAQRO               = (ERRBASE_QUERY+DBICODE_EXTRAQRO);
  DBIERR_FIND1ATT               = (ERRBASE_QUERY+DBICODE_FIND1ATT);
  DBIERR_FINDANST               = (ERRBASE_QUERY+DBICODE_FINDANST);
  DBIERR_GRPNOSET               = (ERRBASE_QUERY+DBICODE_GRPNOSET);
  DBIERR_GRPSTROW               = (ERRBASE_QUERY+DBICODE_GRPSTROW);
  DBIERR_IDFINLCO               = (ERRBASE_QUERY+DBICODE_IDFINLCO);
  DBIERR_IDFPERLI               = (ERRBASE_QUERY+DBICODE_IDFPERLI);
  DBIERR_INANEXPR               = (ERRBASE_QUERY+DBICODE_INANEXPR);
  DBIERR_INS1TIME               = (ERRBASE_QUERY+DBICODE_INS1TIME);
  DBIERR_INSAMBIG               = (ERRBASE_QUERY+DBICODE_INSAMBIG);
  DBIERR_INSDELCH               = (ERRBASE_QUERY+DBICODE_INSDELCH);
  DBIERR_INSEXPRR               = (ERRBASE_QUERY+DBICODE_INSEXPRR);
  DBIERR_INSTOINS               = (ERRBASE_QUERY+DBICODE_INSTOINS);
  DBIERR_ISARRAY                = (ERRBASE_QUERY+DBICODE_ISARRAY);
  DBIERR_LABELERR               = (ERRBASE_QUERY+DBICODE_LABELERR);
  DBIERR_LINKCALC               = (ERRBASE_QUERY+DBICODE_LINKCALC);
  DBIERR_LNGVNAME               = (ERRBASE_QUERY+DBICODE_LNGVNAME);
  DBIERR_LONGQURY               = (ERRBASE_QUERY+DBICODE_LONGQURY);
  DBIERR_MEMVPROC               = (ERRBASE_QUERY+DBICODE_MEMVPROC);
  DBIERR_MISNGCOM               = (ERRBASE_QUERY+DBICODE_MISNGCOM);
  DBIERR_MISNGRPA               = (ERRBASE_QUERY+DBICODE_MISNGRPA);
  DBIERR_MISSRTQU               = (ERRBASE_QUERY+DBICODE_MISSRTQU);
  DBIERR_NAMTWICE               = (ERRBASE_QUERY+DBICODE_NAMTWICE);
  DBIERR_NOCHKMAR               = (ERRBASE_QUERY+DBICODE_NOCHKMAR);
  DBIERR_NODEFOCC               = (ERRBASE_QUERY+DBICODE_NODEFOCC);
  DBIERR_NOGROUPS               = (ERRBASE_QUERY+DBICODE_NOGROUPS);
  DBIERR_NONSENSE               = (ERRBASE_QUERY+DBICODE_NONSENSE);
  DBIERR_NOPATTER               = (ERRBASE_QUERY+DBICODE_NOPATTER);
  DBIERR_NOSUCHDA               = (ERRBASE_QUERY+DBICODE_NOSUCHDA);
  DBIERR_NOVALUE                = (ERRBASE_QUERY+DBICODE_NOVALUE);
  DBIERR_ONLYCONS               = (ERRBASE_QUERY+DBICODE_ONLYCONS);
  DBIERR_ONLYSETR               = (ERRBASE_QUERY+DBICODE_ONLYSETR);
  DBIERR_OUTSENS1               = (ERRBASE_QUERY+DBICODE_OUTSENS1);
  DBIERR_OUTTWIC1               = (ERRBASE_QUERY+DBICODE_OUTTWIC1);
  DBIERR_PAROWCNT               = (ERRBASE_QUERY+DBICODE_PAROWCNT);
  DBIERR_PERSEPAR               = (ERRBASE_QUERY+DBICODE_PERSEPAR);
  DBIERR_PROCPLSW               = (ERRBASE_QUERY+DBICODE_PROCPLSW);
  DBIERR_PWINSRTS               = (ERRBASE_QUERY+DBICODE_PWINSRTS);
  DBIERR_PWMODRTS               = (ERRBASE_QUERY+DBICODE_PWMODRTS);
  DBIERR_QBEFLDFOUND            = (ERRBASE_QUERY+DBICODE_QBEFLDFOUND);
  DBIERR_QBENOFENCE             = (ERRBASE_QUERY+DBICODE_QBENOFENCE);
  DBIERR_QBENOFENCET            = (ERRBASE_QUERY+DBICODE_QBENOFENCET);
  DBIERR_QBENOHEADERT           = (ERRBASE_QUERY+DBICODE_QBENOHEADERT);
  DBIERR_QBENOTAB               = (ERRBASE_QUERY+DBICODE_QBENOTAB);
  DBIERR_QBENUMCOLS             = (ERRBASE_QUERY+DBICODE_QBENUMCOLS);
  DBIERR_QBEOPENTAB             = (ERRBASE_QUERY+DBICODE_QBEOPENTAB);
  DBIERR_QBETWICE               = (ERRBASE_QUERY+DBICODE_QBETWICE);
  DBIERR_QRYNOANSWER            = (ERRBASE_QUERY+DBICODE_QRYNOANSWER);
  DBIERR_QRYNOTPREP             = (ERRBASE_QUERY+DBICODE_QRYNOTPREP);
  DBIERR_QUAINDEL               = (ERRBASE_QUERY+DBICODE_QUAINDEL);
  DBIERR_QUAININS               = (ERRBASE_QUERY+DBICODE_QUAININS);
  DBIERR_RAGININS               = (ERRBASE_QUERY+DBICODE_RAGININS);
  DBIERR_RAGINSET               = (ERRBASE_QUERY+DBICODE_RAGINSET);
  DBIERR_ROWUSERR               = (ERRBASE_QUERY+DBICODE_ROWUSERR);
  DBIERR_SETEXPEC               = (ERRBASE_QUERY+DBICODE_SETEXPEC);
  DBIERR_SETVAMB1               = (ERRBASE_QUERY+DBICODE_SETVAMB1);
  DBIERR_SETVBAD1               = (ERRBASE_QUERY+DBICODE_SETVBAD1);
  DBIERR_SETVDEF1               = (ERRBASE_QUERY+DBICODE_SETVDEF1);
  DBIERR_SUMNUMBE               = (ERRBASE_QUERY+DBICODE_SUMNUMBE);
  DBIERR_TBLISWP3               = (ERRBASE_QUERY+DBICODE_TBLISWP3);
  DBIERR_TOKENNOT               = (ERRBASE_QUERY+DBICODE_TOKENNOT);
  DBIERR_TWOOUTR1               = (ERRBASE_QUERY+DBICODE_TWOOUTR1);
  DBIERR_TYPEMISM               = (ERRBASE_QUERY+DBICODE_TYPEMISM);
  DBIERR_UNRELQ1                = (ERRBASE_QUERY+DBICODE_UNRELQ1);
  DBIERR_UNUSEDST               = (ERRBASE_QUERY+DBICODE_UNUSEDST);
  DBIERR_USEINSDE               = (ERRBASE_QUERY+DBICODE_USEINSDE);
  DBIERR_USEOFCHG               = (ERRBASE_QUERY+DBICODE_USEOFCHG);
  DBIERR_VARMUSTF               = (ERRBASE_QUERY+DBICODE_VARMUSTF);
  DBIERR_REGISTER               = (ERRBASE_QUERY+DBICODE_REGISTER);
  DBIERR_LONGEXPR               = (ERRBASE_QUERY+DBICODE_LONGEXPR);
  DBIERR_REFRESH                = (ERRBASE_QUERY+DBICODE_REFRESH);
  DBIERR_CANCEXCEPT             = (ERRBASE_QUERY+DBICODE_CANCEXCEPT);
  DBIERR_DBEXCEPT               = (ERRBASE_QUERY+DBICODE_DBEXCEPT);
  DBIERR_MEMEXCEPT              = (ERRBASE_QUERY+DBICODE_MEMEXCEPT);
  DBIERR_FATALEXCEPT            = (ERRBASE_QUERY+DBICODE_FATALEXCEPT);
  DBIERR_QRYNIY                 = (ERRBASE_QUERY+ DBICODE_QRYNIY);
  DBIERR_BADFORMAT              = (ERRBASE_QUERY+ DBICODE_BADFORMAT);
  DBIERR_QRYEMPTY               = (ERRBASE_QUERY+ DBICODE_QRYEMPTY);
  DBIERR_NOQRYTOPREP            = (ERRBASE_QUERY+ DBICODE_NOQRYTOPREP);
  DBIERR_BUFFTOOSMALL           = (ERRBASE_QUERY+ DBICODE_BUFFTOOSMALL);
  DBIERR_QRYNOTPARSE            = (ERRBASE_QUERY+ DBICODE_QRYNOTPARSE);
  DBIERR_NOTHANDLE              = (ERRBASE_QUERY+ DBICODE_NOTHANDLE);
  DBIERR_QRYSYNTERR             = (ERRBASE_QUERY+ DBICODE_QRYSYNTERR);
  DBIERR_QXFLDCOUNT             = (ERRBASE_QUERY+ DBICODE_QXFLDCOUNT);
  DBIERR_QXFLDSYMNOTFOUND       = (ERRBASE_QUERY+ DBICODE_QXFLDSYMNOTFOUND);
  DBIERR_QXTBLSYMNOTFOUND       = (ERRBASE_QUERY+ DBICODE_QXTBLSYMNOTFOUND);
  DBIERR_BLOBTERM               = (ERRBASE_QUERY+ DBICODE_BLOBTERM);
  DBIERR_BLOBERR                = (ERRBASE_QUERY+ DBICODE_BLOBERR);
  DBIERR_RESTARTQRY             = (ERRBASE_QUERY+ DBICODE_RESTARTQRY);
  DBIERR_UNKNOWNANSTYPE         = (ERRBASE_QUERY+ DBICODE_UNKNOWNANSTYPE);
  DBIERR_SQLG_MDIST             = (ERRBASE_QUERY+ DBICODE_SQLG_MDIST);
  DBIERR_SQLG_NOARI             = (ERRBASE_QUERY+ DBICODE_SQLG_NOARI);
  DBIERR_SQLG_LIKEN             = (ERRBASE_QUERY+ DBICODE_SQLG_LIKEN);
  DBIERR_SQLG_ALPHO             = (ERRBASE_QUERY+ DBICODE_SQLG_ALPHO);
  DBIERR_SQLG_DATEO             = (ERRBASE_QUERY+ DBICODE_SQLG_DATEO);
  DBIERR_SQLG_RELOP             = (ERRBASE_QUERY+ DBICODE_SQLG_RELOP);
  DBIERR_SQLG_ONLYC             = (ERRBASE_QUERY+ DBICODE_SQLG_ONLYC);
  DBIERR_SQLG_CNTLN             = (ERRBASE_QUERY+ DBICODE_SQLG_CNTLN);
  DBIERR_SQLG_CHINI             = (ERRBASE_QUERY+ DBICODE_SQLG_CHINI);
  DBIERR_SQLG_UNION             = (ERRBASE_QUERY+ DBICODE_SQLG_UNION);
  DBIERR_SQLG_SLFIN             = (ERRBASE_QUERY+ DBICODE_SQLG_SLFIN);
  DBIERR_SQLG_OTJVR             = (ERRBASE_QUERY+ DBICODE_SQLG_OTJVR);
  DBIERR_SQLG_STROW             = (ERRBASE_QUERY+ DBICODE_SQLG_STROW);
  DBIERR_SQLG_QUANT             = (ERRBASE_QUERY+ DBICODE_SQLG_QUANT);
  DBIERR_SQLG_REGSO             = (ERRBASE_QUERY+ DBICODE_SQLG_REGSO);
  DBIERR_SQLG_COUNT             = (ERRBASE_QUERY+ DBICODE_SQLG_COUNT);
  DBIERR_SQLG_AVERA             = (ERRBASE_QUERY+ DBICODE_SQLG_AVERA);
  DBIERR_SQLG_DATEA             = (ERRBASE_QUERY+ DBICODE_SQLG_DATEA);
  DBIERR_SQLG_BADPT             = (ERRBASE_QUERY+ DBICODE_SQLG_BADPT);
  DBIERR_SQLG_RELPA             = (ERRBASE_QUERY+ DBICODE_SQLG_RELPA);
  DBIERR_SQLG_PATRN             = (ERRBASE_QUERY+ DBICODE_SQLG_PATRN);
  DBIERR_SQLG_FNDSU             = (ERRBASE_QUERY+ DBICODE_SQLG_FNDSU);
  DBIERR_SQLG_IFDCS             = (ERRBASE_QUERY+ DBICODE_SQLG_IFDCS);
  DBIERR_SQLG_IDCCO             = (ERRBASE_QUERY+ DBICODE_SQLG_IDCCO);
  DBIERR_SQLG_ONLYI             = (ERRBASE_QUERY+ DBICODE_SQLG_ONLYI);
  DBIERR_SQLG_SQLDIALECT        = (ERRBASE_QUERY+ DBICODE_SQLG_SQLDIALECT);
  DBIERR_SQLG_NOQUERY           = (ERRBASE_QUERY+ DBICODE_SQLG_NOQUERY);
  DBIERR_BLOBGROUP              = (ERRBASE_QUERY+ DBICODE_BLOBGROUP);
  DBIERR_QRYNOPROP              = (ERRBASE_QUERY+DBICODE_QRYNOPROP);
  DBIERR_ANSTYPNOTSUP           = (ERRBASE_QUERY+DBICODE_ANSTYPNOTSUP);
  DBIERR_ANSALIASNOTSUP         = (ERRBASE_QUERY+DBICODE_ANSALIASNOTSUP);
  DBIERR_INSBLOBREQ             = (ERRBASE_QUERY+DBICODE_INSBLOBREQ     ); { 0x9a }
  DBIERR_CHGUNIQUENDXREQ        = (ERRBASE_QUERY+DBICODE_CHGUNIQUENDXREQ); { 0x9b }
  DBIERR_DELUNIQUENDXREQ        = (ERRBASE_QUERY+DBICODE_DELUNIQUENDXREQ); { 0x9c }
  DBIERR_SQLNOFULLUPDATE        = (ERRBASE_QUERY+DBICODE_SQLNOFULLUPDATE); { 0x9d }
  DBIERR_CANTEXECREMOTE         = (ERRBASE_QUERY+DBICODE_CANTEXECREMOTE); { 0x9e }
  DBIERR_UNEXPECTEDEOC          = (ERRBASE_QUERY+DBICODE_UNEXPECTEDEOC);
  DBIERR_SQLPARAMNOTSET         = (ERRBASE_QUERY+DBICODE_SQLPARAMNOTSET);
  DBIERR_QUERYTOOLONG           = (ERRBASE_QUERY+DBICODE_QUERYTOOLONG);

  DBIERR_NOSUCHRELORALIAS       = (ERRBASE_QUERY+DBICODE_NOSUCHRELORALIAS);
  DBIERR_TYPEAMBIGUITY          = (ERRBASE_QUERY+DBICODE_TYPEAMBIGUITY);
  DBIERR_ORDERBYNOTAPROJ        = (ERRBASE_QUERY+DBICODE_ORDERBYNOTAPROJ);
  DBIERR_SQLPARSE               = (ERRBASE_QUERY+DBICODE_SQLPARSE);
  DBIERR_CONSTRAINTFAILED       = (ERRBASE_QUERY+DBICODE_CONSTRAINTFAILED);
  DBIERR_NOTGROUPINGFIELD       = (ERRBASE_QUERY+DBICODE_NOTGROUPINGFIELD);
  DBIERR_UDFNOTDEFINED          = (ERRBASE_QUERY+DBICODE_UDFNOTDEFINED);
  DBIERR_UDFERROR               = (ERRBASE_QUERY+DBICODE_UDFERROR);
  DBIERR_SINGLEROWERROR         = (ERRBASE_QUERY+DBICODE_SINGLEROWERROR);
  DBIERR_GROUPEXPR              = (ERRBASE_QUERY+DBICODE_GROUPEXPR);
  DBIERR_QUERYTEXT              = (ERRBASE_QUERY+DBICODE_QUERYTEXT);
  DBIERR_ANSIJOINSUP            = (ERRBASE_QUERY+DBICODE_ANSIJOINSUP);
  DBIERR_DISTUNION              = (ERRBASE_QUERY+DBICODE_DISTUNION);
  DBIERR_GROUPBYREQ             = (ERRBASE_QUERY+DBICODE_GROUPBYREQ);
  DBIERR_INSUPDAUTOINC          = (ERRBASE_QUERY+DBICODE_INSUPDAUTOIC);
  DBIERR_UPDREFINTSINGLE        = (ERRBASE_QUERY+DBICODE_UPDREFINTSINGLE);



{ END_OF_QUERY_MESSAGES }

{ ERRCAT_VERSION }
{ ============== }

  ERRCODE_INTERFACEVER          = 1;      { Interface mismatch }
  ERRCODE_INDEXOUTOFDATE        = 2;      { Index is out of date }
  ERRCODE_OLDVERSION            = 3;      { Older version (see context) }
  ERRCODE_VALFILEINVALID        = 4;      { Val. file is out of date }
  ERRCODE_BLOBVERSION           = 5;      { Old Blob file version }
  ERRCODE_ENGQRYMISMATCH        = 6;      { Query and IDAPI are mismatched }
  ERRCODE_SERVERVERSION         = 7;      { Server is incompatible version }
  ERRCODE_TABLELEVEL            = 8;      { Higher table level required }

  DBIERR_INTERFACEVER           = (ERRBASE_VERSION + ERRCODE_INTERFACEVER);
  DBIERR_INDEXOUTOFDATE         = (ERRBASE_VERSION + ERRCODE_INDEXOUTOFDATE);
  DBIERR_OLDVERSION             = (ERRBASE_VERSION + ERRCODE_OLDVERSION);
  DBIERR_VALFILEINVALID         = (ERRBASE_VERSION + ERRCODE_VALFILEINVALID);
  DBIERR_BLOBVERSION            = (ERRBASE_VERSION + ERRCODE_BLOBVERSION);
  DBIERR_ENGQRYMISMATCH         = (ERRBASE_VERSION + ERRCODE_ENGQRYMISMATCH);
  DBIERR_SERVERVERSION          = (ERRBASE_VERSION + ERRCODE_SERVERVERSION);
  DBIERR_TABLELEVEL             = (ERRBASE_VERSION + ERRCODE_TABLELEVEL);

{ ERRCAT_CAPABILITY }
{ ================= }

  ERRCODE_NOTSUPPORTED          = 1;      { Capability not supported }
  ERRCODE_NIY                   = 2;      { Not Implemented Yet }
  ERRCODE_TABLESQL              = 3;      { Cannot access SQL replica }
  ERRCODE_SEARCHCOLREQD         = 4;      { Searchable (Non-blob column) required }
  ERRCODE_NOMULTCONNECT         = 5;      { Multiple connections not supported }
  ERRCODE_NODBASEEXPR           = 6;      { Full dBASE Expressions not supported }

  DBIERR_NOTSUPPORTED           = (ERRBASE_CAPABILITY + ERRCODE_NOTSUPPORTED);
  DBIERR_NIY                    = (ERRBASE_CAPABILITY + ERRCODE_NIY);
  DBIERR_TABLESQL               = (ERRBASE_CAPABILITY + ERRCODE_TABLESQL);
  DBIERR_SEARCHCOLREQD          = (ERRBASE_CAPABILITY + ERRCODE_SEARCHCOLREQD);
  DBIERR_NOMULTCONNECT          = (ERRBASE_CAPABILITY + ERRCODE_NOMULTCONNECT);
  DBIERR_NODBASEEXPR            = (ERRBASE_CAPABILITY + ERRCODE_NODBASEEXPR);

{ ERRCAT_CONFIG }
{ ============= }

  ERRCODE_INVALIDDBSPEC         = 1;
  ERRCODE_UNKNOWNDBTYPE         = 2;
  ERRCODE_INVALIDSYSDATA        = 3;
  ERRCODE_UNKNOWNNETTYPE        = 4;
  ERRCODE_NOTONTHATNET          = 5;
  ERRCODE_INVALIDCFGPARAM       = 6;      { Generic invalid config param }


  DBIERR_INVALIDDBSPEC          = (ERRBASE_CONFIG + ERRCODE_INVALIDDBSPEC);
  DBIERR_UNKNOWNDBTYPE          = (ERRBASE_CONFIG + ERRCODE_UNKNOWNDBTYPE);
  DBIERR_INVALIDSYSDATA         = (ERRBASE_CONFIG + ERRCODE_INVALIDSYSDATA);
  DBIERR_UNKNOWNNETTYPE         = (ERRBASE_CONFIG + ERRCODE_UNKNOWNNETTYPE);
  DBIERR_NOTONTHATNET           = (ERRBASE_CONFIG + ERRCODE_NOTONTHATNET);
  DBIERR_INVALIDCFGPARAM        = (ERRBASE_CONFIG + ERRCODE_INVALIDCFGPARAM);

{ ERRCAT_WARNING  non-fatal warnings:               }
{ warn user of action, or ask for optional behavior }
{ ================================================= }
  ERRCODE_OBJIMPLICITLYDROPPED  = 1;
  ERRCODE_OBJMAYBETRUNCATED     = 2;
  ERRCODE_OBJIMPLICITLYMODIFIED = 3;
  ERRCODE_VALIDATEDATA          = 4;
  ERRCODE_VALFIELDMODIFIED      = 5;
  ERRCODE_TABLELEVELCHANGED     = 6;
  ERRCODE_COPYLINKEDTABLES      = 7;
  ERRCODE_OTHERSERVERLOADED     = 8;
  ERRCODE_OBJIMPLICITLYTRUNCATED = 9;
  ERRCODE_VCHKMAYNOTBEENFORCED  = 10;
  ERRCODE_MULTIPLEUNIQRECS      = 11;
  ERRCODE_FIELDMUSTBETRIMMED    = 12;

  DBIERR_OBJIMPLICITLYDROPPED   = ( ERRBASE_WARNING + ERRCODE_OBJIMPLICITLYDROPPED);
  DBIERR_OBJMAYBETRUNCATED      = ( ERRBASE_WARNING + ERRCODE_OBJMAYBETRUNCATED);
  DBIERR_OBJIMPLICITLYMODIFIED  = ( ERRBASE_WARNING + ERRCODE_OBJIMPLICITLYMODIFIED);
  DBIERR_VALIDATEDATA           = ( ERRBASE_WARNING + ERRCODE_VALIDATEDATA);
  DBIERR_VALFIELDMODIFIED       = ( ERRBASE_WARNING + ERRCODE_VALFIELDMODIFIED);
  DBIERR_TABLELEVELCHANGED      = ( ERRBASE_WARNING + ERRCODE_TABLELEVELCHANGED);
  DBIERR_COPYLINKEDTABLES       = ( ERRBASE_WARNING + ERRCODE_COPYLINKEDTABLES);
  DBIERR_OTHERSERVERLOADED      = ( ERRBASE_WARNING + ERRCODE_OTHERSERVERLOADED);
  DBIERR_OBJIMPLICITLYTRUNCATED = ( ERRBASE_WARNING + ERRCODE_OBJIMPLICITLYTRUNCATED);
  DBIERR_VCHKMAYNOTBEENFORCED   = ( ERRBASE_WARNING + ERRCODE_VCHKMAYNOTBEENFORCED );
  DBIERR_MULTIPLEUNIQRECS       = ( ERRBASE_WARNING + ERRCODE_MULTIPLEUNIQRECS );
  DBIERR_FIELDMUSTBETRIMMED     = ( ERRBASE_WARNING + ERRCODE_FIELDMUSTBETRIMMED );


{ ERRCAT_OTHER }
{ ============ }

  ERRCODE_FILEEXISTS            = 1;      { File already exsits }
  ERRCODE_BLOBMODIFIED          = 2;      { Another user modified Blob }
  ERRCODE_UNKNOWNSQL            = 3;      { Unknown SQL error }
  ERRCODE_TABLEEXISTS           = 4;      { Table already exsits }
  ERRCODE_PDX10TABLE            = 5;      { Paradox 1.0 tables not supported }
  ERRCODE_UPDATEABORT           = 6;      { Update operation aborted }


  DBIERR_FILEEXISTS             = (ERRBASE_OTHER + ERRCODE_FILEEXISTS);
  DBIERR_BLOBMODIFIED           = (ERRBASE_OTHER + ERRCODE_BLOBMODIFIED);
  DBIERR_UNKNOWNSQL             = (ERRBASE_OTHER + ERRCODE_UNKNOWNSQL);
  DBIERR_TABLEEXISTS            = (ERRBASE_OTHER + ERRCODE_TABLEEXISTS);
  DBIERR_PDX10TABLE             = (ERRBASE_OTHER + ERRCODE_PDX10TABLE);
  DBIERR_UPDATEABORT            = (ERRBASE_OTHER + ERRCODE_UPDATEABORT);


{ ERRCAT_COMPATIBILITY }
{ ==================== }

  ERRCODE_DIFFSORTORDER         = 1;      { Sortorders not compatible }
  ERRCODE_DIRINUSEBYOLDVER      = 2;      { Directory in use by old version }
  ERRCODE_PDX35LDDRIVER         = 3;      { Needs Pdox 3.5 compatible language driver }

  DBIERR_DIFFSORTORDER          = (ERRBASE_COMPATIBILITY + ERRCODE_DIFFSORTORDER);
  DBIERR_DIRINUSEBYOLDVER       = (ERRBASE_COMPATIBILITY + ERRCODE_DIRINUSEBYOLDVER);
  DBIERR_PDX35LDDRIVER          = (ERRBASE_COMPATIBILITY + ERRCODE_PDX35LDDRIVER);

{ ERRCAT_OPTPARAM }
{ =============== }

  ERRCODE_REQOPTPARAM           = 1;      { Required optional parameter missing }
  ERRCODE_INVALIDOPTPARAM       = 2;      { Optional param out-of-range or bad }


  DBIERR_REQOPTPARAM            = (ERRBASE_OPTPARAM + ERRCODE_REQOPTPARAM);
  DBIERR_INVALIDOPTPARAM        = (ERRBASE_OPTPARAM + ERRCODE_INVALIDOPTPARAM);

{  ERRCAT_REPOSITORY }
{  ================= }

  ERRCODE_REPOSITORYCORRUPT     = 1;    { Data Repository is corrupt }
  ERRCODE_INFOBLOBCORRUPT       = 2;    { Info Blob corrupted }
  ERRCODE_SCHEMACORRUPT         = 3;    { DR Schema is corrupt }
  ERRCODE_ATTRTYPEEXISTS        = 4;    { Attribute Type exists }
  ERRCODE_INVALIDOBJTYPE        = 5;    { Invalid Object Type }
  ERRCODE_INVALIDRELATIONTYPE   = 6;    { Invalid Relation Type }
  ERRCODE_VIEWEXISTS            = 7;    { View already exists }
  ERRCODE_NOSUCHVIEW            = 8;    { No such View exists }
  ERRCODE_INVALIDRECCONSTRAINT  = 9;    { Invalid Record Constraint }
  ERRCODE_LDBCONNECTION         = 10;   { Object is in a Logical DB }
  ERRCODE_REPOSITORYEXISTS      = 11;   { Repository already exists }
  ERRCODE_NOSUCHREPOSITORY      = 12;   { Repository does not exist }
  ERRCODE_REPOSITORYDBMISSING   = 13;   { Repository database does not exist }
  ERRCODE_REPOSITORYOUTOFDATE   = 14;   { Repository info is out of date }
  ERRCODE_REPOSITORYVERSION     = 15;   { DR Version mismatch }
  ERRCODE_REPOSITORYNAME        = 16;   { Invalid Repository name }
  ERRCODE_DEPENDENTOBJECTS      = 17;   { Dependent Objects exist }
  ERRCODE_RELATIONLIMIT         = 18;   { Too many Relationships for this Object Type }
  ERRCODE_RELATIONSHIPSEXIST    = 19;   { Relationships to the Object exist }
  ERRCODE_EXCHANGEFILECORRUPT   = 20;   { Exchange File Corrupt }
  ERRCODE_EXCHANGEFILEVERSION   = 21;   { Exchange File Version Mismatch }
  ERRCODE_TYPEMISMATCH          = 22;   { Exchange File and Repository Types don't match }
  ERRCODE_OBJECTEXISTS          = 23;   { Object Exists in the Target Repository }
  ERRCODE_REPOSITORYACCESS      = 24;   { Access to Repository Denied }
  ERRCODE_REPOSITORYCREATE      = 25;   { Cannot Create Repository }
  ERRCODE_DATABASEOPENFAILED    = 26;   { Cannot Open a Database }


  DBIERR_REPOSITORYCORRUPT      = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYCORRUPT);
  DBIERR_INFOBLOBCORRUPT        = (ERRBASE_REPOSITORY + ERRCODE_INFOBLOBCORRUPT);
  DBIERR_SCHEMACORRUPT          = (ERRBASE_REPOSITORY + ERRCODE_SCHEMACORRUPT);
  DBIERR_ATTRTYPEEXISTS         = (ERRBASE_REPOSITORY + ERRCODE_ATTRTYPEEXISTS);
  DBIERR_INVALIDOBJTYPE         = (ERRBASE_REPOSITORY + ERRCODE_INVALIDOBJTYPE);
  DBIERR_INVALIDRELATIONTYPE    = (ERRBASE_REPOSITORY + ERRCODE_INVALIDRELATIONTYPE);
  DBIERR_VIEWEXISTS             = (ERRBASE_REPOSITORY + ERRCODE_VIEWEXISTS);
  DBIERR_NOSUCHVIEW             = (ERRBASE_REPOSITORY + ERRCODE_NOSUCHVIEW);
  DBIERR_INVALIDRECCONSTRAINT   = (ERRBASE_REPOSITORY + ERRCODE_INVALIDRECCONSTRAINT);
  DBIERR_LDBCONNECTION          = (ERRBASE_REPOSITORY + ERRCODE_LDBCONNECTION);
  DBIERR_REPOSITORYEXISTS       = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYEXISTS);
  DBIERR_NOSUCHREPOSITORY       = (ERRBASE_REPOSITORY + ERRCODE_NOSUCHREPOSITORY);
  DBIERR_REPOSITORYDBMISSING    = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYDBMISSING);
  DBIERR_REPOSITORYOUTOFDATE    = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYOUTOFDATE);
  DBIERR_REPOSITORYVERSION      = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYVERSION);
  DBIERR_REPOSITORYNAME         = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYNAME);
  DBIERR_DEPENDENTOBJECTS       = (ERRBASE_REPOSITORY + ERRCODE_DEPENDENTOBJECTS);
  DBIERR_RELATIONLIMIT          = (ERRBASE_REPOSITORY + ERRCODE_RELATIONLIMIT);
  DBIERR_RELATIONSHIPSEXIST     = (ERRBASE_REPOSITORY + ERRCODE_RELATIONSHIPSEXIST);
  DBIERR_EXCHANGEFILECORRUPT    = (ERRBASE_REPOSITORY + ERRCODE_EXCHANGEFILECORRUPT);
  DBIERR_EXCHANGEFILEVERSION    = (ERRBASE_REPOSITORY + ERRCODE_EXCHANGEFILEVERSION);
  DBIERR_TYPEMISMATCH           = (ERRBASE_REPOSITORY + ERRCODE_TYPEMISMATCH);
  DBIERR_OBJECTEXISTS           = (ERRBASE_REPOSITORY + ERRCODE_OBJECTEXISTS);
  DBIERR_REPOSITORYACCESS       = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYACCESS);
  DBIERR_REPOSITORYCREATE       = (ERRBASE_REPOSITORY + ERRCODE_REPOSITORYCREATE);
  DBIERR_DATABASEOPENFAILED     = (ERRBASE_REPOSITORY + ERRCODE_DATABASEOPENFAILED);

type
  DBIDATE            = Longint;
  DBITIME            = Longint;
  TIMESTAMP          = Double;

const
{ Field Types (Logical) }

  fldUNKNOWN         = 0;
  fldZSTRING         = 1;               { Null terminated string }
  fldDATE            = 2;               { Date     (32 bit) }
  fldBLOB            = 3;               { Blob }
  fldBOOL            = 4;               { Boolean  (16 bit) }
  fldINT16           = 5;               { 16 bit signed number }
  fldINT32           = 6;               { 32 bit signed number }
  fldFLOAT           = 7;               { 64 bit floating point }
  fldBCD             = 8;               { BCD }
  fldBYTES           = 9;               { Fixed number of bytes }
  fldTIME            = 10;              { Time        (32 bit) }
  fldTIMESTAMP       = 11;              { Time-stamp  (64 bit) }
  fldUINT16          = 12;              { Unsigned 16 bit integer }
  fldUINT32          = 13;              { Unsigned 32 bit integer }
  fldFLOATIEEE       = 14;              { 80-bit IEEE float }
  fldVARBYTES        = 15;              { Length prefixed var bytes }
  fldLOCKINFO        = 16;              { Look for LOCKINFO typedef }
  fldCURSOR          = 17;              { For Oracle Cursor type }
  fldINT64           = 18;              { 64 bit signed number }
  fldUINT64          = 19;              { Unsigned 64 bit integer }
  fldADT             = 20;              { Abstract datatype (structure) }
  fldARRAY           = 21;              { Array field type }
  fldREF             = 22;              { Reference to ADT }
  fldTABLE           = 23;              { Nested table (reference) }

  {$IFDEF DCC6OrLater}
  MaxLogFldTypes     = 26;
  {$ELSE}
  MaxLogFldTypes     = 24;              { Number of logical fieldtypes }
  {$ENDIF}

{ Sub Types (Logical) }

{ fldFLOAT subtype }

  fldstMONEY         = 21;              { Money }

{ fldBLOB subtypes }

  fldstMEMO          = 22;              { Text Memo }
  fldstBINARY        = 23;              { Binary data }
  fldstFMTMEMO       = 24;              { Formatted Text }
  fldstOLEOBJ        = 25;              { OLE object (Paradox) }
  fldstGRAPHIC       = 26;              { Graphics object }
  fldstDBSOLEOBJ     = 27;              { dBASE OLE object }
  fldstTYPEDBINARY   = 28;              { Typed Binary data }
  fldstACCOLEOBJ     = 30;              { Access OLE object }
  fldstHMEMO         = 33;              { CLOB }
  fldstHBINARY       = 34;              { BLOB }
  fldstBFILE         = 36;              { BFILE }

{ fldZSTRING subtype }

  fldstPASSWORD      = 1;               { Password }
  fldstFIXED         = 31;              { CHAR type }
  fldstUNICODE       = 32;              { Unicode }

{ fldINT32 subtype }

  fldstAUTOINC       = 29;

{ fldADT subtype }

  fldstADTNestedTable = 35;             { ADT for nested table (has no name) }

{ fldDATE subtype }
  fldstADTDATE       = 37;              { DATE (OCIDate ) with in an ADT }

{============================================================================}
{                    Filter description                                      }
{============================================================================}

type
  pffCANOp = ^ffCANOp;                                                {!!.01}
  ffCANOp  = (
    canNOTDEFINED,                      {                                  (*) }
    canISBLANK,                         { CANUnary;  is operand blank.     (*) }
    canNOTBLANK,                        { CANUnary;  is operand not blank. (*) }
    canEQ,                              { CANBinary, CANCompare; equal.    (*) }
    canNE,                              { CANBinary; NOT equal.            (*) }
    canGT,                              { CANBinary; greater than.         (*) }
    canLT,                              { CANBinary; less than.            (*) }
    canGE,                              { CANBinary; greater or equal.     (*) }
    canLE,                              { CANBinary; less or equal.        (*) }
    canNOT,                             { CANUnary; NOT                    (*) }
    canAND,                             { CANBinary; AND                   (*) }
    canOR,                              { CANBinary; OR                    (*) }
    canTUPLE2,                          { CANUnary; Entire record is operand. }
    canFIELD2,                          { CANUnary; operand is field       (*) }
    canCONST2,                          { CANUnary; operand is constant    (*) }
    canMINUS,                           { CANUnary;  minus. }
    canADD,                             { CANBinary; addition. }
    canSUB,                             { CANBinary; subtraction. }
    canMUL,                             { CANBinary; multiplication. }
    canDIV,                             { CANBinary; division. }
    canMOD,                             { CANBinary; modulo division. }
    canREM,                             { CANBinary; remainder of division. }
    canSUM,                             { CANBinary, accumulate sum of. }
    canCOUNT,                           { CANBinary, accumulate count of. }
    canMIN,                             { CANBinary, find minimum of. }
    canMAX,                             { CANBinary, find maximum of. }
    canAVG,                             { CANBinary, find average of. }
    canCONT,                            { CANBinary; provides a link between two }
    canUDF2,                            { CANBinary; invokes a User defined fn }
    canCONTINUE2,                       { CANUnary; Stops evaluating records }
    canLIKE,                            { CANCompare, extended binary compare       (*) }
    canIN,                              { CANBinary field in list of values }
    canLIST2,                           { List of constant values of same type }
    canUPPER,                           { CANUnary: upper case }
    canLOWER,                           { CANUnary: lower case }
    canFUNC2,                           { CANFunc: Function }
    canLISTELEM2,                       { CANListElem: List Element }
    canASSIGN                           { CANBinary: Field assignment }
  );

  NODEClass = (                         { Node Class }
    nodeNULL,                           { Null node                  (*) }
    nodeUNARY,                          { Node is a unary            (*) }
    nodeBINARY,                         { Node is a binary           (*) }
    nodeCOMPARE,                        { Node is a compare          (*) }
    nodeFIELD,                          { Node is a field            (*) }
    nodeCONST,                          { Node is a constant         (*) }
    nodeTUPLE,                          { Node is a record }
    nodeCONTINUE,                       { Node is a continue node    (*) }
    nodeUDF,                            { Node is a UDF node }
    nodeLIST,                           { Node is a LIST node }
    nodeFUNC,                           { Node is a Function node }
    nodeLISTELEM                        { Node is a List Element node }
  );

{ NODE definitions including misc data structures }
{-------------------------------------------------}

type
  pCANHdr = ^CANHdr;
  CANHdr = packed record                { Header part common to all     (*) }
    nodeClass       : NODEClass;
    canOp           : ffCANOp;                                        {!!.01}
  end;

  pCANUnary = ^CANUnary;
  CANUnary = packed record              { Unary Node                    (*) }
    nodeClass       : NODEClass;
    canOp           : ffCANOp;                                        {!!.01}
    iOperand1       : Word;             { Byte offset of Operand node }
  end;

  pCANBinary = ^CANBinary;
  CANBinary = packed record             { Binary Node                   (*) }
    nodeClass       : NODEClass;
    canOp           : ffCANOp;                                        {!!.01}
    iOperand1       : Word;             { Byte offset of Op1 }
    iOperand2       : Word;             { Byte offset of Op2 }
  end;

  pCANField = ^CANField;
  CANField = packed record              { Field }
    nodeClass       : NODEClass;
    canOp           : ffCANOp;                                        {!!.01}
    iFieldNum       : Word;
    iNameOffset     : Word;             { Name offset in Literal pool }
  end;

  pCANConst = ^CANConst;
  CANConst = packed record              { Constant }
    nodeClass       : NODEClass;
    canOp           : ffCANOp;                                        {!!.01}
    iType           : Word;             { Constant type. }
    iSize           : Word;             { Constant size. (in bytes) }
    iOffset         : Word;             { Offset in the literal pool. }
  end;

  pCANTuple = ^CANTuple;
  CANTuple = packed record              { Tuple (record) }
    nodeClass       : NODEClass;
    canOp           : ffCANOp;                                        {!!.01}
    iSize           : Word;             { Record size. (in bytes) }
  end;

  pCANContinue = ^CANContinue;
  CANContinue = packed record           { Break Node                    (*) }
    nodeClass       : NODEClass;
    canOp           : ffCANOp;                                        {!!.01}
    iContOperand    : Word;             { Continue if operand is true. }
  end;

  pCANCompare = ^CANCompare;
  CANCompare = packed record            { Extended compare Node (text fields) (*) }
    nodeClass       : NODEClass;
    canOp           : ffCANOp;            { canLIKE, canEQ }          {!!.01}
    bCaseInsensitive : WordBool;        { 3 val: UNKNOWN = "fastest", "native" }
    iPartialLen     : Word;             { Partial fieldlength (0 is full length) }
    iOperand1       : Word;             { Byte offset of Op1 }
    iOperand2       : Word;             { Byte offset of Op2 }
  end;

  pCANFunc = ^CANFunc;
  CANFunc = packed record               { Function }
    nodeClass       : NODEClass;
    canOp           : ffCANOp;                                        {!!.01}
    iNameOffset     : Word;             { Name offset in Literal pool }
    iElemOffset     : Word;             { Offset of first List Element in Node pool }
  end;

  pCANListElem = ^CANListElem;
  CANListElem = packed record           { List Element }
    nodeClass       : NODEClass;
    canOp           : ffCANOp;                                        {!!.01}
    iOffset         : Word;             { Arg offset in Node pool }
    iNextOffset     : Word;             { Offset in Node pool of next ListElem or 0 if end of list }
  end;

{This is the node to be used to pass User defined functions }
const
  iLangSQL           = 0;               { Common SQL dialect }
  iDbaseExpr         = 2;               { This is also the driver ID for dBASE }

type
  pCANUdf = ^CANUdf;
  CANUdf = packed record                { A user defined function }
    nodeClass       : NODEClass;
    canOp           : ffCANOp;                                        {!!.01}
    iOffSzFuncName  : Word;             { Offset in literal pool to Function Name string(0 terminated) }
    iOperands       : Word;             { Byte offset of Operands (concatenated using canCONT) }
    iDrvDialect     : Word;             { Driver Dialect ID for UDF string supplied }
    iOffSzUDF       : Word;             { Offset in literal pool to UDF string (0 terminated) }
  end;

  pCANList = ^CANList;
  CANList = packed record           { List of Constants }
    nodeClass       : NODEClass; 
    canOp           : ffCANOp;                                        {!!.01}
    iType           : Word;            { Constant type. }
    iTotalSize      : Word;            { Total list size; }
    iElemSize       : Word;            { Size of each elem for fix-width types }
    iElems          : Word;            { Number of elements in list }
    iOffset         : Word;            { Offset in the literal pool to first elem. }
  end;

  pCANNode = ^CANNode;
  CANNode = packed record
    case Integer of
      0: (canHdr      : CANHdr);
      1: (canUnary    : CANUnary);
      2: (canBinary   : CANBinary);
      3: (canField    : CANField);
      4: (canConst    : CANConst);
      5: (canTuple    : CANTuple);
      6: (canContinue : CANContinue);
      7: (canCompare  : CANCompare);
      8: (canList     : CANList);
      9: (canFunc     : CANFunc);
     10: (canListElem : CANListElem);
  end;

{ Linear exression tree}
{----------------------}

const
  CANEXPRVERSION     = 2;

type
  ppCANExpr = ^pCANExpr;
  pCANExpr  = ^CANExpr;
  CANExpr   = packed record             { Expression Tree }
    iVer            : Word;             { Version tag of expression. }
    iTotalSize      : Word;             { Size of this structure }
    iNodes          : Word;             { Number of nodes }
    iNodeStart      : Word;             { Starting offet of Nodes in this }
    iLiteralStart   : Word;             { Starting offset of Literals in this }
  end;

type
  pfGENFilter = function (
      ulClientData  : Longint;
      pRecBuf       : Pointer;
      iPhyRecNum    : Longint
   ): SmallInt stdcall;
                                                                     
implementation

end.
