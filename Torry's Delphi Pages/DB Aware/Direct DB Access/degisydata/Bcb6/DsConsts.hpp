// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsConsts.pas' rev: 6.00

#ifndef DsConstsHPP
#define DsConstsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dsconsts
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
#define DS_HOMEPAGE "http://www.degisy.com"
static const Byte DS_MAX_MSGLEN = 0xff;
static const unsigned DS_INVALID_FORMAT = 0xffffff00;
extern PACKAGE System::ResourceString _Ds_HandleError;
#define Dsconsts_Ds_HandleError System::LoadResourceString(&Dsconsts::_Ds_HandleError)
extern PACKAGE System::ResourceString _Ds_NoFieldAccess;
#define Dsconsts_Ds_NoFieldAccess System::LoadResourceString(&Dsconsts::_Ds_NoFieldAccess)
extern PACKAGE System::ResourceString _Ds_FieldReadOnly;
#define Dsconsts_Ds_FieldReadOnly System::LoadResourceString(&Dsconsts::_Ds_FieldReadOnly)
extern PACKAGE System::ResourceString _Ds_NotEditing;
#define Dsconsts_Ds_NotEditing System::LoadResourceString(&Dsconsts::_Ds_NotEditing)
extern PACKAGE System::ResourceString _Ds_DatabaseNameMissing;
#define Dsconsts_Ds_DatabaseNameMissing System::LoadResourceString(&Dsconsts::_Ds_DatabaseNameMissing)
extern PACKAGE System::ResourceString _Ds_DatabasePropertyMissing;
#define Dsconsts_Ds_DatabasePropertyMissing System::LoadResourceString(&Dsconsts::_Ds_DatabasePropertyMissing)
extern PACKAGE System::ResourceString _Ds_TableNamePropertyMissing;
#define Dsconsts_Ds_TableNamePropertyMissing System::LoadResourceString(&Dsconsts::_Ds_TableNamePropertyMissing)
extern PACKAGE System::ResourceString _Ds_MemNoRecords;
#define Dsconsts_Ds_MemNoRecords System::LoadResourceString(&Dsconsts::_Ds_MemNoRecords)
extern PACKAGE System::ResourceString _Ds_InvalidFieldType;
#define Dsconsts_Ds_InvalidFieldType System::LoadResourceString(&Dsconsts::_Ds_InvalidFieldType)
extern PACKAGE System::ResourceString _MSG_ACCESSDENIED;
#define Dsconsts_MSG_ACCESSDENIED System::LoadResourceString(&Dsconsts::_MSG_ACCESSDENIED)
extern PACKAGE System::ResourceString _MSG_CANNOTOPENFILE;
#define Dsconsts_MSG_CANNOTOPENFILE System::LoadResourceString(&Dsconsts::_MSG_CANNOTOPENFILE)
extern PACKAGE System::ResourceString _MSG_CANNOTSEEKFILE;
#define Dsconsts_MSG_CANNOTSEEKFILE System::LoadResourceString(&Dsconsts::_MSG_CANNOTSEEKFILE)
extern PACKAGE System::ResourceString _MSG_CANNOTREADFILE;
#define Dsconsts_MSG_CANNOTREADFILE System::LoadResourceString(&Dsconsts::_MSG_CANNOTREADFILE)
extern PACKAGE System::ResourceString _MSG_CANNOTCREATEFILE;
#define Dsconsts_MSG_CANNOTCREATEFILE System::LoadResourceString(&Dsconsts::_MSG_CANNOTCREATEFILE)
extern PACKAGE System::ResourceString _MSG_CANNOTWRITEFILE;
#define Dsconsts_MSG_CANNOTWRITEFILE System::LoadResourceString(&Dsconsts::_MSG_CANNOTWRITEFILE)
extern PACKAGE System::ResourceString _MSG_DUPLICATETABLENAME;
#define Dsconsts_MSG_DUPLICATETABLENAME System::LoadResourceString(&Dsconsts::_MSG_DUPLICATETABLENAME)
extern PACKAGE System::ResourceString _MSG_DUPLICATEUSERNAME;
#define Dsconsts_MSG_DUPLICATEUSERNAME System::LoadResourceString(&Dsconsts::_MSG_DUPLICATEUSERNAME)
extern PACKAGE System::ResourceString _MSG_DUPLICATEFIELDNAME;
#define Dsconsts_MSG_DUPLICATEFIELDNAME System::LoadResourceString(&Dsconsts::_MSG_DUPLICATEFIELDNAME)
extern PACKAGE System::ResourceString _MSG_DUPLICATEAUTOINCFIELD;
#define Dsconsts_MSG_DUPLICATEAUTOINCFIELD System::LoadResourceString(&Dsconsts::_MSG_DUPLICATEAUTOINCFIELD)
extern PACKAGE System::ResourceString _MSG_DUPLICATEINDEXNAME;
#define Dsconsts_MSG_DUPLICATEINDEXNAME System::LoadResourceString(&Dsconsts::_MSG_DUPLICATEINDEXNAME)
extern PACKAGE System::ResourceString _MSG_OBJNOTFOUND;
#define Dsconsts_MSG_OBJNOTFOUND System::LoadResourceString(&Dsconsts::_MSG_OBJNOTFOUND)
extern PACKAGE System::ResourceString _MSG_DATABASEFILENOTFOUND;
#define Dsconsts_MSG_DATABASEFILENOTFOUND System::LoadResourceString(&Dsconsts::_MSG_DATABASEFILENOTFOUND)
extern PACKAGE System::ResourceString _MSG_TABLENOTFOUND;
#define Dsconsts_MSG_TABLENOTFOUND System::LoadResourceString(&Dsconsts::_MSG_TABLENOTFOUND)
extern PACKAGE System::ResourceString _MSG_RECORDNOTFOUND;
#define Dsconsts_MSG_RECORDNOTFOUND System::LoadResourceString(&Dsconsts::_MSG_RECORDNOTFOUND)
extern PACKAGE System::ResourceString _MSG_FIELDNOTFOUND;
#define Dsconsts_MSG_FIELDNOTFOUND System::LoadResourceString(&Dsconsts::_MSG_FIELDNOTFOUND)
extern PACKAGE System::ResourceString _MSG_BLOCKNOTFOUND;
#define Dsconsts_MSG_BLOCKNOTFOUND System::LoadResourceString(&Dsconsts::_MSG_BLOCKNOTFOUND)
extern PACKAGE System::ResourceString _MSG_INDEXNOTFOUND;
#define Dsconsts_MSG_INDEXNOTFOUND System::LoadResourceString(&Dsconsts::_MSG_INDEXNOTFOUND)
extern PACKAGE System::ResourceString _MSG_RECDELETED;
#define Dsconsts_MSG_RECDELETED System::LoadResourceString(&Dsconsts::_MSG_RECDELETED)
extern PACKAGE System::ResourceString _MSG_TABLEREADONLY;
#define Dsconsts_MSG_TABLEREADONLY System::LoadResourceString(&Dsconsts::_MSG_TABLEREADONLY)
extern PACKAGE System::ResourceString _MSG_READONLYFLD;
#define Dsconsts_MSG_READONLYFLD System::LoadResourceString(&Dsconsts::_MSG_READONLYFLD)
extern PACKAGE System::ResourceString _MSG_NOTSUFFTABLERIGHTS;
#define Dsconsts_MSG_NOTSUFFTABLERIGHTS System::LoadResourceString(&Dsconsts::_MSG_NOTSUFFTABLERIGHTS)
extern PACKAGE System::ResourceString _MSG_INVALIDKEYWORD;
#define Dsconsts_MSG_INVALIDKEYWORD System::LoadResourceString(&Dsconsts::_MSG_INVALIDKEYWORD)
extern PACKAGE System::ResourceString _MSG_TABLEISBUSY;
#define Dsconsts_MSG_TABLEISBUSY System::LoadResourceString(&Dsconsts::_MSG_TABLEISBUSY)
extern PACKAGE System::ResourceString _MSG_OBJECTLOCKED;
#define Dsconsts_MSG_OBJECTLOCKED System::LoadResourceString(&Dsconsts::_MSG_OBJECTLOCKED)
extern PACKAGE System::ResourceString _MSG_NOTABLOBFIELD;
#define Dsconsts_MSG_NOTABLOBFIELD System::LoadResourceString(&Dsconsts::_MSG_NOTABLOBFIELD)
extern PACKAGE System::ResourceString _MSG_NOCURRREC;
#define Dsconsts_MSG_NOCURRREC System::LoadResourceString(&Dsconsts::_MSG_NOCURRREC)
extern PACKAGE System::ResourceString _MSG_NOSUCHTABLE;
#define Dsconsts_MSG_NOSUCHTABLE System::LoadResourceString(&Dsconsts::_MSG_NOSUCHTABLE)
extern PACKAGE System::ResourceString _MSG_INDEXOPEN;
#define Dsconsts_MSG_INDEXOPEN System::LoadResourceString(&Dsconsts::_MSG_INDEXOPEN)
extern PACKAGE System::ResourceString _MSG_BOF;
#define Dsconsts_MSG_BOF System::LoadResourceString(&Dsconsts::_MSG_BOF)
extern PACKAGE System::ResourceString _MSG_EOF;
#define Dsconsts_MSG_EOF System::LoadResourceString(&Dsconsts::_MSG_EOF)
extern PACKAGE System::ResourceString _MSG_BEGINOFBLOB;
#define Dsconsts_MSG_BEGINOFBLOB System::LoadResourceString(&Dsconsts::_MSG_BEGINOFBLOB)
extern PACKAGE System::ResourceString _MSG_ENDOFBLOB;
#define Dsconsts_MSG_ENDOFBLOB System::LoadResourceString(&Dsconsts::_MSG_ENDOFBLOB)
extern PACKAGE System::ResourceString _MSG_TOOMANYTABLES;
#define Dsconsts_MSG_TOOMANYTABLES System::LoadResourceString(&Dsconsts::_MSG_TOOMANYTABLES)
extern PACKAGE System::ResourceString _MSG_TOOMANYUSERS;
#define Dsconsts_MSG_TOOMANYUSERS System::LoadResourceString(&Dsconsts::_MSG_TOOMANYUSERS)
extern PACKAGE System::ResourceString _MSG_TOOMANYPROCS;
#define Dsconsts_MSG_TOOMANYPROCS System::LoadResourceString(&Dsconsts::_MSG_TOOMANYPROCS)
extern PACKAGE System::ResourceString _MSG_TOOMANYCHECKS;
#define Dsconsts_MSG_TOOMANYCHECKS System::LoadResourceString(&Dsconsts::_MSG_TOOMANYCHECKS)
extern PACKAGE System::ResourceString _MSG_TOOMANYFIELDS;
#define Dsconsts_MSG_TOOMANYFIELDS System::LoadResourceString(&Dsconsts::_MSG_TOOMANYFIELDS)
extern PACKAGE System::ResourceString _MSG_TOOMANYINDEXES;
#define Dsconsts_MSG_TOOMANYINDEXES System::LoadResourceString(&Dsconsts::_MSG_TOOMANYINDEXES)
extern PACKAGE System::ResourceString _MSG_DATABASECLOSED;
#define Dsconsts_MSG_DATABASECLOSED System::LoadResourceString(&Dsconsts::_MSG_DATABASECLOSED)
extern PACKAGE System::ResourceString _MSG_TABLECRYPTED;
#define Dsconsts_MSG_TABLECRYPTED System::LoadResourceString(&Dsconsts::_MSG_TABLECRYPTED)
extern PACKAGE System::ResourceString _MSG_INVALIDOBJECT;
#define Dsconsts_MSG_INVALIDOBJECT System::LoadResourceString(&Dsconsts::_MSG_INVALIDOBJECT)
extern PACKAGE System::ResourceString _MSG_INVALIDHANDLE;
#define Dsconsts_MSG_INVALIDHANDLE System::LoadResourceString(&Dsconsts::_MSG_INVALIDHANDLE)
extern PACKAGE System::ResourceString _MSG_INVALIDDBHANDLE;
#define Dsconsts_MSG_INVALIDDBHANDLE System::LoadResourceString(&Dsconsts::_MSG_INVALIDDBHANDLE)
extern PACKAGE System::ResourceString _MSG_INVALIDFILTERHANDLE;
#define Dsconsts_MSG_INVALIDFILTERHANDLE System::LoadResourceString(&Dsconsts::_MSG_INVALIDFILTERHANDLE)
extern PACKAGE System::ResourceString _MSG_INVALIDPARAMETER;
#define Dsconsts_MSG_INVALIDPARAMETER System::LoadResourceString(&Dsconsts::_MSG_INVALIDPARAMETER)
extern PACKAGE System::ResourceString _MSG_INVALIDBOOKMARK;
#define Dsconsts_MSG_INVALIDBOOKMARK System::LoadResourceString(&Dsconsts::_MSG_INVALIDBOOKMARK)
extern PACKAGE System::ResourceString _MSG_INVALIDBLOBOFFSET;
#define Dsconsts_MSG_INVALIDBLOBOFFSET System::LoadResourceString(&Dsconsts::_MSG_INVALIDBLOBOFFSET)
extern PACKAGE System::ResourceString _MSG_INVALIDDATABASEFILE;
#define Dsconsts_MSG_INVALIDDATABASEFILE System::LoadResourceString(&Dsconsts::_MSG_INVALIDDATABASEFILE)
extern PACKAGE System::ResourceString _MSG_INVALIDDATABASEVERSION;
#define Dsconsts_MSG_INVALIDDATABASEVERSION System::LoadResourceString(&Dsconsts::_MSG_INVALIDDATABASEVERSION)
extern PACKAGE System::ResourceString _MSG_INVALIDDATABASECHECKSUM;
#define Dsconsts_MSG_INVALIDDATABASECHECKSUM System::LoadResourceString(&Dsconsts::_MSG_INVALIDDATABASECHECKSUM)
extern PACKAGE System::ResourceString _MSG_INVALIDUSER;
#define Dsconsts_MSG_INVALIDUSER System::LoadResourceString(&Dsconsts::_MSG_INVALIDUSER)
extern PACKAGE System::ResourceString _MSG_INVALIDTABLENAME;
#define Dsconsts_MSG_INVALIDTABLENAME System::LoadResourceString(&Dsconsts::_MSG_INVALIDTABLENAME)
extern PACKAGE System::ResourceString _MSG_INVALIDUSERNAME;
#define Dsconsts_MSG_INVALIDUSERNAME System::LoadResourceString(&Dsconsts::_MSG_INVALIDUSERNAME)
extern PACKAGE System::ResourceString _MSG_INVALIDFIELDTYPE;
#define Dsconsts_MSG_INVALIDFIELDTYPE System::LoadResourceString(&Dsconsts::_MSG_INVALIDFIELDTYPE)
extern PACKAGE System::ResourceString _MSG_INVALIDFIELDNAME;
#define Dsconsts_MSG_INVALIDFIELDNAME System::LoadResourceString(&Dsconsts::_MSG_INVALIDFIELDNAME)
extern PACKAGE System::ResourceString _MSG_INVALIDFIELDSIZE;
#define Dsconsts_MSG_INVALIDFIELDSIZE System::LoadResourceString(&Dsconsts::_MSG_INVALIDFIELDSIZE)
extern PACKAGE System::ResourceString _MSG_INVALIDBLOCK;
#define Dsconsts_MSG_INVALIDBLOCK System::LoadResourceString(&Dsconsts::_MSG_INVALIDBLOCK)
extern PACKAGE System::ResourceString _MSG_INVALIDEXPRESSION;
#define Dsconsts_MSG_INVALIDEXPRESSION System::LoadResourceString(&Dsconsts::_MSG_INVALIDEXPRESSION)
extern PACKAGE System::ResourceString _MSG_INVALIDENCRYPTION;
#define Dsconsts_MSG_INVALIDENCRYPTION System::LoadResourceString(&Dsconsts::_MSG_INVALIDENCRYPTION)
extern PACKAGE System::ResourceString _MSG_UNKNOWNIDENTIFIER;
#define Dsconsts_MSG_UNKNOWNIDENTIFIER System::LoadResourceString(&Dsconsts::_MSG_UNKNOWNIDENTIFIER)
extern PACKAGE System::ResourceString _MSG_UNEXPECTEDIDENTIFIER;
#define Dsconsts_MSG_UNEXPECTEDIDENTIFIER System::LoadResourceString(&Dsconsts::_MSG_UNEXPECTEDIDENTIFIER)

}	/* namespace Dsconsts */
using namespace Dsconsts;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsConsts
