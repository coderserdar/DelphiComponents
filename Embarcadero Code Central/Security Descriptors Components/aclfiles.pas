{------------------------------------------------------------------------------}
{ aclfiles                                                                     }
{------------------------------------------------------------------------------}
{ Author(s) : Franck Musson (franck.musson@wanadoo.fr)                         }
{ Copyright : Franck Musson 1998-2001                                          }
{ Created   : 01/10/2001                                                       }
{ Version   : 1.000 (Delphi 5)                                                 }
{------------------------------------------------------------------------------}
{ Copyright (c) 1987-2002 Franck Musson                                        }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to deal}
{ in the Software without restriction, including without limitation the rights }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    }
{ copies of the Software, and to permit persons to whom the Software is        }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.                        }
{ IN NO EVENT SHALL FRANCK MUSSON BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER    }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,}
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS       }
{ IN THE SOFTWARE.                                                             }
{                                                                              }
{ Except as contained in this notice, the name of Franck Musson shall not be   }
{ used in advertising or otherwise to promote the sale, use or other dealings  }
{ in this Software without prior written authorization from Franck Musson.     }
{------------------------------------------------------------------------------}
 Unit aclfiles;

{==============================================================================}
{                                                                              }
{                                                                              }
{ I N T E R F A C E                                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
 Interface

{------------------------------------------------------------------------------}
{ Clauses Uses                                                                 }
{------------------------------------------------------------------------------}
 Uses
    Windows, SysUtils, Classes, aclbase, aclconst, aclfuncs, aclcheck;

{------------------------------------------------------------------------------}
{ Public Types                                                                 }
{------------------------------------------------------------------------------}
 Type
    TNTFileApplyFlag = (fifDir, fifSubdirs, fifSetOnly);
    TNTFileApplyFlags = Set Of TNTFileApplyFlag;

    TCustomNTFileAccess = Class;
    TNTFileAccesses = Class;
    TNTFileAccess = Class;

    TCustomNTFileAudit = Class;
    TNTFileAudits = Class;
    TNTFileAudit = Class;

    TCustomNTFileAccessFlags = Class;
    TNTDiskFilesAccessFlags = Class;
    TNTFileAccessFlags = Class;
    TNTDirAccessFlags = Class;
    TNTPipeAccessFlags =Class;

    TCustomNTFileAuditFlags = Class;
    TNTDiskFilesAuditFlags = Class;
    TNTFileAuditFlags = Class;
    TNTDirAuditFlags = Class;
    TNTPipeAuditFlags = Class;

    TCustomNTFileCheckFlags = Class;
    TNTDiskFilesCheckFlags = Class;
    TNTFileCheckFlags = Class;
    TNTDirCheckFlags = Class;
    TNTPipeCheckFlags =Class;

    TCustomNTFileFlagsFiler = Class;
    TNTFileAccessFlagsFiler = Class;
    TNTFileAuditFlagsFiler = Class;
    TNTFileCheckFlagsFiler = Class;
    TNTFileDesiredCheckFlagsFiler = Class;
    TNTFileGrantedCheckFlagsFiler = Class;

    // Components
    TNTCustomFileSecurity = Class(TCustomSecurityObject)
    Private
      Function GetDList: TNTFileAccesses;
      Procedure SetDList(Const Value: TNTFileAccesses);
      Function GetSList: TNTFileAudits;
      Procedure SetSList(Const Value: TNTFileAudits);
      Function GetFOwner: TCreatorOwnerID;
      Procedure SetFOwner(Const Value: TCreatorOwnerID);
      Function GetFGroup: TCreatorGroupID;
      Procedure SetFGroup(Const Value: TCreatorGroupID);
    Protected
      FObjectName: String;
      Function  CreateAccessList: TDACL;                                        Override;
      Function  CreateAuditList: TSACL;                                         Override;
      Function  CreateOwnerID: TCreatorSecurityID;                              Override;
      Function  CreateGroupID: TCreatorSecurityID;                              Override;
      Function  ReadObjectDescriptor: PSecurityDescriptor;                      Override;
      Procedure WriteObjectDescriptor;                                          Override;
    Published
      Property Active;
      Property AutoSave;
      Property Accesses: TNTFileAccesses     Read GetDList      Write SetDList  Stored False;
      Property Audits: TNTFileAudits         Read GetSList      Write SetSList  Stored False;
      Property CreatorOwner: TCreatorOwnerID Read GetFOwner     Write SetFOwner Stored False;
      Property CreatorGroup: TCreatorGroupID Read GetFGroup     Write SetFGroup Stored False;
      Property LookupSystemName;
      Property Options;
    End;

    TNTFileSecurity = Class(TNTCustomFileSecurity)
    Private
      Function GetFileName: String;
      Procedure SetFilename(Const Value: String);
    Published
      Property FileName: String              Read GetFileName   Write SetFileName;
    End;

    TNTDirectorySecurity = Class(TNTCustomFileSecurity)
    Private
      Function GetDirectory: String;
      Procedure SetDirectory(Const Value: String);
    Protected
      Function CreateAccessList: TDACL;                                         Override;
    Published
      Property Directory: String             Read GetDirectory  Write SetDirectory;
    End;

    // Accesses
    TNTFileAccesses = Class(TDACL)
    Private
      Function GetItems(Index: Integer): TCustomNTFileAccess;
      Procedure SetItems(Index: Integer; Const Value: TCustomNTFileAccess);
    Public
      Property Items[Index: Integer]: TCustomNTFileAccess Read GetItems Write SetItems;    Default;
    End;

    TCustomNTFileAccess = Class(TCustomACE)
    Private
      Function GetApplyTo: TNTFileApplyFlags;
      Procedure SetApplyTo(Const Value: TNTFileApplyFlags);
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Procedure GetDeniedAce(Value: PACL);                                      Override;
      Procedure GetAllowedAce(Value: PACL);                                     Override;
      Function GetDeniedSize: Integer;                                          Override;
      Function GetAllowedSize: Integer;                                         Override;
      Property Status;
    Published
      Property ApplyTo: TNTFileApplyFlags   Read GetApplyTo   Write SetApplyTo  Default [fifDir, fifSubDirs];
      Property IsInherited;
    End;

    TNTFileAccess = Class(TCustomNTFileAccess)
    Private
      FFiler: TNTFileAccessFlagsFiler;
      FFileFlags: TNTFileAccessFlags;
      Function GetFlags: TNTFileAccessFlags;
      Procedure SetFlags(Const Value: TNTFileAccessFlags);
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
    Published
      Property Flags: TNTFileAccessFlags    Read GetFlags     Write SetFlags;
    End;

    TNTDirectoryAccess = Class(TCustomNTFileAccess)
    Private
      FFiler: TNTFileAccessFlagsFiler;
      FDirFlags: TNTDirAccessFlags;
      Function GetFlags: TNTDirAccessFlags;
      Procedure SetFlags(Const Value: TNTDirAccessFlags);
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
    Published
      Property Flags: TNTDirAccessFlags     Read GetFlags     Write SetFlags;
    End;

    TNTPipeAccess = Class(TCustomNTFileAccess)
    Private
      FFiler: TNTFileAccessFlagsFiler;
      FPipeFlags: TNTPipeAccessFlags;
      Function GetFlags: TNTPipeAccessFlags;
      Procedure SetFlags(Const Value: TNTPipeAccessFlags);
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
    Published
      Property Flags: TNTPipeAccessFlags    Read GetFlags     Write SetFlags;
    End;

    // Accesses Flags
    TCustomNTFileAccessFlags = Class(TPersistent)
    Private
      FFiler: TNTFileAccessFlagsFiler;
      Function GetAce: TCustomNTFileAccess;
      Function GetDelete: TAccessFlagValue;
      Procedure SetDelete(Const Value: TAccessFlagValue);
      Function GetReadControl: TAccessFlagValue;
      Procedure SetReadControl(Const Value: TAccessFlagValue);
      Function GetWriteDac: TAccessFlagValue;
      Procedure SetWriteDac(Const Value: TAccessFlagValue);
      Function GetWriteOwner: TAccessFlagValue;
      Procedure SetWriteOwner(Const Value: TAccessFlagValue);
      Function GetReadAttributes: TAccessFlagValue;
      Procedure SetReadAttributes(Const Value: TAccessFlagValue);
      Function GetWriteAttributes: TAccessFlagValue;
      Procedure SetWriteAttributes(Const Value: TAccessFlagValue);
      Function GetSynchronize: TAccessFlagValue;
      Procedure SetSynchronize(Const Value: TAccessFlagValue);
    Protected
      Function GetAttributes(Const lng: Cardinal): TAccessFlagValue;               Virtual;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: TAccessFlagValue); Virtual;
    Public
      Constructor Create(Filer: TNTFileAccessFlagsFiler);                          Overload;
      Destructor Destroy;                                                          Override;
      Procedure Assign(Source: TPersistent);                                       Override;
      Property ACE: TCustomNTFileAccess              Read GetAce;
    Published
      Property CanDelete: TAccessFlagValue           Read GetDelete          Write SetDelete;
      Property CanReadControl: TAccessFlagValue      Read GetReadControl     Write SetReadControl;
      Property CanWriteDac: TAccessFlagValue         Read GetWriteDac        Write SetWriteDac;
      Property CanWriteOwner: TAccessFlagValue       Read GetWriteOwner      Write SetWriteOwner;
      Property CanReadAttributes: TAccessFlagValue   Read GetReadAttributes  Write SetReadAttributes;
      Property CanWriteAttributes: TAccessFlagValue  Read GetWriteAttributes Write SetWriteAttributes;
      Property CanSynchronize: TAccessFlagValue      Read GetSynchronize     Write SetSynchronize;
    End;

    TNTDiskFilesAccessFlags = Class(TCustomNTFileAccessFlags)
    Private
      Function GetReadExtended: TAccessFlagValue;
      Procedure SetReadExtended(Const Value: TAccessFlagValue);
      Function GetWriteExtended: TAccessFlagValue;
      Procedure SetWriteExtended(Const Value: TAccessFlagValue);
    Public
      Procedure Assign(Source: TPersistent);                                    Override;
    Published
      Property CanReadExtended: TAccessFlagValue     Read GetReadExtended    Write SetReadExtended;
      Property CanWriteExtented: TAccessFlagValue    Read GetWriteExtended   Write SetWriteExtended;
    End;

    TNTFileAccessFlags = Class(TNTDiskFilesAccessFlags)
    Private
      Function GetReadData: TAccessFlagValue;
      Procedure SetReadData(Const Value: TAccessFlagValue);
      Function GetWriteData: TAccessFlagValue;
      Procedure SetWriteData(Const Value: TAccessFlagValue);
      Function GetAppendData: TAccessFlagValue;
      Procedure SetAppendData(Const Value: TAccessFlagValue);
      Function GetExecute: TAccessFlagValue;
      Procedure SetExecute(Const Value: TAccessFlagValue);
    Protected
      Function GetFlagsMode: TNTFileFlagsMode;                                  Virtual;
      Procedure SetFlagsMode(Const Value: TNTFileFlagsMode);                    Virtual;
    Public
      Procedure Assign(Source: TPersistent);                                    Override;
    Published
      Property CanReadData: TAccessFlagValue         Read GetReadData        Write SetReadData;
      Property CanWriteData: TAccessFlagValue        Read GetWriteData       Write SetWriteData;
      Property CanAppendData: TAccessFlagValue       Read GetAppendData      Write SetAppendData;
      Property CanExecute: TAccessFlagValue          Read GetExecute         Write SetExecute;
      Property FlagsMode: TNTFileFlagsMode           Read GetFlagsMode       Write SetFlagsMode     Default ffmRead;
    End;

    TNTDirAccessFlags = Class(TNTDiskFilesAccessFlags)
    Private
      Function GetListDir: TAccessFlagValue;
      Procedure SetListDir(Const Value: TAccessFlagValue);
      Function GetAddFile: TAccessFlagValue;
      Procedure SetAddFile(Const Value: TAccessFlagValue);
      Function GetAddSubDir: TAccessFlagValue;
      Procedure SetAddSubDir(Const Value: TAccessFlagValue);
      Function GetTraverse: TAccessFlagValue;
      Procedure SetTraverse(Const Value: TAccessFlagValue);
      Function GetDeleteChild: TAccessFlagValue;
      Procedure SetDeleteChild(Const Value: TAccessFlagValue);
    Protected
      Function GetFlagsMode: TNTDirFlagsMode;                                   Virtual;
      Procedure SetFlagsMode(Const Value: TNTDirFlagsMode);                     Virtual;
    Public
      Procedure Assign(Source: TPersistent);                                    Override;
    Published
      Property CanListDir: TAccessFlagValue         Read GetListDir        Write SetListDir;
      Property CanAddFile: TAccessFlagValue         Read GetAddFile        Write SetAddFile;
      Property CanAddSubDir: TAccessFlagValue       Read GetAddSubDir      Write SetAddSubDir;
      Property CanTraverse: TAccessFlagValue        Read GetTraverse       Write SetTraverse;
      Property CanDeleteChild: TAccessFlagValue     Read GetDeleteChild    Write SetDeleteChild;
      Property FlagsMode: TNTDirFlagsMode           Read GetFlagsMode      Write SetFlagsMode     Default dfmRead;
    End;

    TNTPipeAccessFlags = Class(TCustomNTFileAccessFlags)
    Private
      Function GetReadData: TAccessFlagValue;
      Procedure SetReadData(Const Value: TAccessFlagValue);
      Function GetWriteData: TAccessFlagValue;
      Procedure SetWriteData(Const Value: TAccessFlagValue);
      Function GetCreateInstance: TAccessFlagValue;
      Procedure SetCreateInstance(Const Value: TAccessFlagValue);
    Public
      Procedure Assign(Source: TPersistent);                                    Override;
    Published
      Property CanReadData: TAccessFlagValue         Read GetReadData        Write SetReadData;
      Property CanWriteData: TAccessFlagValue        Read GetWriteData       Write SetWriteData;
      Property CanCreateInstance: TAccessFlagValue   Read GetCreateInstance  Write SetCreateInstance;
    End;

    // Audits
    TNTFileAudits = Class(TSACL)
    Private
      Function GetItems(Index: Integer): TCustomNTFileAudit;
      Procedure SetItems(Index: Integer; Const Value: TCustomNTFileAudit);
    Public
      Property Items[Index: Integer]: TCustomNTFileAudit Read GetItems Write SetItems; Default;
    End;

    TCustomNTFileAudit = Class(TCustomACE)
    Private
      Function GetApplyTo: TNTFileApplyFlags;
      Procedure SetApplyTo(Const Value: TNTFileApplyFlags);
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Procedure GetDeniedAce(Value: PACL);                                      Override;
      Procedure GetAllowedAce(Value: PACL);                                     Override;
      Function GetDeniedSize: Integer;                                          Override;
      Function GetAllowedSize: Integer;                                         Override;
      Property Status;
    Published
      Property ApplyTo: TNTFileApplyFlags   Read GetApplyTo   Write SetApplyTo  Default [fifDir, fifSubDirs];
      Property IsInherited;
    End;

    TNTFileAudit = Class(TCustomNTFileAudit)
    Private
      FFiler: TNTFileAuditFlagsFiler;
      FFileFlags: TNTFileAuditFlags;
      Function GetFlags: TNTFileAuditFlags;
      Procedure SetFlags(Const Value: TNTFileAuditFlags);
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
    Published
      Property Flags: TNTFileAuditFlags    Read GetFlags     Write SetFlags;
    End;

    TNTDirectoryAudit = Class(TCustomNTFileAudit)
    Private
      FFiler: TNTFileAuditFlagsFiler;
      FDirFlags: TNTDirAuditFlags;
      Function GetFlags: TNTDirAuditFlags;
      Procedure SetFlags(Const Value: TNTDirAuditFlags);
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
    Published
      Property Flags: TNTDirAuditFlags     Read GetFlags     Write SetFlags;
    End;

    TNTPipeAudit = Class(TCustomNTFileAudit)
    Private
      FFiler: TNTFileAuditFlagsFiler;
      FPipeFlags: TNTPipeAuditFlags;
      Function GetFlags: TNTPipeAuditFlags;
      Procedure SetFlags(Const Value: TNTPipeAuditFlags);
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
    Published
      Property Flags: TNTPipeAuditFlags     Read GetFlags     Write SetFlags;
    End;

    // Audit Flags
    TCustomNTFileAuditFlags = Class(TPersistent)
    Private
      FFiler: TNTFileAuditFlagsFiler;
      Function GetAce: TCustomNTFileAudit;
      Function GetDelete: TAuditFlagValue;
      Procedure SetDelete(Const Value: TAuditFlagValue);
      Function GetReadControl: TAuditFlagValue;
      Procedure SetReadControl(Const Value: TAuditFlagValue);
      Function GetWriteDac: TAuditFlagValue;
      Procedure SetWriteDac(Const Value: TAuditFlagValue);
      Function GetWriteOwner: TAuditFlagValue;
      Procedure SetWriteOwner(Const Value: TAuditFlagValue);
      Function GetReadAttributes: TAuditFlagValue;
      Procedure SetReadAttributes(Const Value: TAuditFlagValue);
      Function GetWriteAttributes: TAuditFlagValue;
      Procedure SetWriteAttributes(Const Value: TAuditFlagValue);
      Function GetSynchronize: TAuditFlagValue;
      Procedure SetSynchronize(Const Value: TAuditFlagValue);
    Protected
      Function GetAttributes(Const lng: Cardinal): TAuditFlagValue;               Virtual;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: TAuditFlagValue); Virtual;
    Public
      Constructor Create(Filer: TNTFileAuditFlagsFiler);                          Overload;
      Destructor Destroy;                                                         Override;
      Procedure Assign(Source: TPersistent);                                      Override;
      Property ACE: TCustomNTFileAudit               Read GetAce;
    Published
      Property CanDelete: TAuditFlagValue            Read GetDelete          Write SetDelete;
      Property CanReadControl: TAuditFlagValue       Read GetReadControl     Write SetReadControl;
      Property CanWriteDac: TAuditFlagValue          Read GetWriteDac        Write SetWriteDac;
      Property CanWriteOwner: TAuditFlagValue        Read GetWriteOwner      Write SetWriteOwner;
      Property CanReadAttributes: TAuditFlagValue    Read GetReadAttributes  Write SetReadAttributes;
      Property CanWriteAttributes: TAuditFlagValue   Read GetWriteAttributes Write SetWriteAttributes;
      Property CanSynchronize: TAuditFlagValue       Read GetSynchronize     Write SetSynchronize;
    End;

    TNTDiskFilesAuditFlags = Class(TCustomNTFileAuditFlags)
    Private
      Function GetReadExtended: TAuditFlagValue;
      Procedure SetReadExtended(Const Value: TAuditFlagValue);
      Function GetWriteExtended: TAuditFlagValue;
      Procedure SetWriteExtended(Const Value: TAuditFlagValue);
    Public
      Procedure Assign(Source: TPersistent);                                    Override;
    Published
      Property CanReadExtended: TAuditFlagValue      Read GetReadExtended    Write SetReadExtended;
      Property CanWriteExtented: TAuditFlagValue     Read GetWriteExtended   Write SetWriteExtended;
    End;

    TNTFileAuditFlags = Class(TNTDiskFilesAuditFlags)
    Private
      Function GetReadData: TAuditFlagValue;
      Procedure SetReadData(Const Value: TAuditFlagValue);
      Function GetWriteData: TAuditFlagValue;
      Procedure SetWriteData(Const Value: TAuditFlagValue);
      Function GetAppendData: TAuditFlagValue;
      Procedure SetAppendData(Const Value: TAuditFlagValue);
      Function GetExecute: TAuditFlagValue;
      Procedure SetExecute(Const Value: TAuditFlagValue);
    Protected
      Function GetFlagsMode: TNTFileFlagsMode;                                  Virtual;
      Procedure SetFlagsMode(Const Value: TNTFileFlagsMode);                    Virtual;
    Public
      Procedure Assign(Source: TPersistent);                                    Override;
    Published
      Property CanReadData: TAuditFlagValue          Read GetReadData        Write SetReadData;
      Property CanWriteData: TAuditFlagValue         Read GetWriteData       Write SetWriteData;
      Property CanAppendData: TAuditFlagValue        Read GetAppendData      Write SetAppendData;
      Property CanExecute: TAuditFlagValue           Read GetExecute         Write SetExecute;
      Property FlagsMode: TNTFileFlagsMode           Read GetFlagsMode       Write SetFlagsMode     Default ffmRead;
    End;

    TNTDirAuditFlags = Class(TNTDiskFilesAuditFlags)
    Private
      Function GetListDir: TAuditFlagValue;
      Procedure SetListDir(Const Value: TAuditFlagValue);
      Function GetAddFile: TAuditFlagValue;
      Procedure SetAddFile(Const Value: TAuditFlagValue);
      Function GetAddSubDir: TAuditFlagValue;
      Procedure SetAddSubDir(Const Value: TAuditFlagValue);
      Function GetTraverse: TAuditFlagValue;
      Procedure SetTraverse(Const Value: TAuditFlagValue);
      Function GetDeleteChild: TAuditFlagValue;
      Procedure SetDeleteChild(Const Value: TAuditFlagValue);
    Protected
      Function GetFlagsMode: TNTDirFlagsMode;                                   Virtual;
      Procedure SetFlagsMode(Const Value: TNTDirFlagsMode);                     Virtual;
    Public
      Procedure Assign(Source: TPersistent);                                    Override;
    Published
      Property CanListDir: TAuditFlagValue          Read GetListDir        Write SetListDir;
      Property CanAddFile: TAuditFlagValue          Read GetAddFile        Write SetAddFile;
      Property CanAddSubDir: TAuditFlagValue        Read GetAddSubDir      Write SetAddSubDir;
      Property CanTraverse: TAuditFlagValue         Read GetTraverse       Write SetTraverse;
      Property CanDeleteChild: TAuditFlagValue      Read GetDeleteChild    Write SetDeleteChild;
      Property FlagsMode: TNTDirFlagsMode           Read GetFlagsMode      Write SetFlagsMode     Default dfmRead;
    End;

    TNTPipeAuditFlags = Class(TCustomNTFileAuditFlags)
    Private
      Function GetReadData: TAuditFlagValue;
      Procedure SetReadData(Const Value: TAuditFlagValue);
      Function GetWriteData: TAuditFlagValue;
      Procedure SetWriteData(Const Value: TAuditFlagValue);
      Function GetCreateInstance: TAuditFlagValue;
      Procedure SetCreateInstance(Const Value: TAuditFlagValue);
    Public
      Procedure Assign(Source: TPersistent);                                    Override;
    Published
      Property CanReadData: TAuditFlagValue          Read GetReadData        Write SetReadData;
      Property CanWriteData: TAuditFlagValue         Read GetWriteData       Write SetWriteData;
      Property CanCreateInstance: TAuditFlagValue    Read GetCreateInstance  Write SetCreateInstance;
    End;

    // Checks Components
    TNTCustomFileSecurityCheck = Class(TCustomAccessCheck)
    Published
      Property Active;
      Property Allowed;
      Property Mode;
    End;

    TNTFileSecurityCheck = Class(TNTCustomFileSecurityCheck)
    Private
      FDesiredFiler: TNTFileDesiredCheckFlagsFiler;
      FGrantedFiler: TNTFileGrantedCheckFlagsFiler;
      FDesiredChecks: TNTFileCheckFlags;
      FGrantedChecks: TNTFileCheckFlags;
      Function GetSecurityObj: TNTFileSecurity;
      Procedure SetSecurityObj(Const Value: TNTFileSecurity);
      Function GetDesiredChecks: TNTFileCheckFlags;
      Procedure SetDesiredChecks(Const Value: TNTFileCheckFlags);
      Function GetGrantedChecks: TNTFileCheckFlags;
      Procedure SetGrantedChecks(Const Value: TNTFileCheckFlags);
    Public
      Constructor Create(AOwner: TComponent);                                   Override;
      Destructor Destroy;                                                       Override;
    Published
      Property DesiredChecks: TNTFileCheckFlags Read GetDesiredChecks Write SetDesiredChecks;
      Property GrantedChecks: TNTFileCheckFlags Read GetGrantedChecks Write SetGrantedChecks Stored False;
      Property SecurityObject: TNTFileSecurity  Read GetSecurityObj   Write SetSecurityObj;
    End;

    TNTDirectorySecurityCheck = Class(TNTCustomFileSecurityCheck)
    Private
      FDesiredFiler: TNTFileDesiredCheckFlagsFiler;
      FGrantedFiler: TNTFileGrantedCheckFlagsFiler;
      FDesiredChecks: TNTDirCheckFlags;
      FGrantedChecks: TNTDirCheckFlags;
      Function GetSecurityObj: TNTDirectorySecurity;
      Procedure SetSecurityObj(Const Value: TNTDirectorySecurity);
      Function GetDesiredChecks: TNTDirCheckFlags;
      Procedure SetDesiredChecks(Const Value: TNTDirCheckFlags);
      Function GetGrantedChecks: TNTDirCheckFlags;
      Procedure SetGrantedChecks(Const Value: TNTDirCheckFlags);
    Public
      Constructor Create(AOwner: TComponent);                                   Override;
      Destructor Destroy;                                                       Override;
    Published
      Property DesiredChecks: TNTDirCheckFlags      Read GetDesiredChecks Write SetDesiredChecks;
      Property GrantedChecks: TNTDirCheckFlags      Read GetGrantedChecks Write SetGrantedChecks Stored False;
      Property SecurityObject: TNTDirectorySecurity Read GetSecurityObj   Write SetSecurityObj;
    End;

    // Checks Flags
    TCustomNTFileCheckFlags = Class(TPersistent)
    Private
      FFiler: TCustomNTFileFlagsFiler;
      FReadOnly: Boolean;
      Function GetDelete: TCheckFlagValue;
      Procedure SetDelete(Const Value: TCheckFlagValue);
      Function GetReadControl: TCheckFlagValue;
      Procedure SetReadControl(Const Value: TCheckFlagValue);
      Function GetWriteDac: TCheckFlagValue;
      Procedure SetWriteDac(Const Value: TCheckFlagValue);
      Function GetWriteOwner: TCheckFlagValue;
      Procedure SetWriteOwner(Const Value: TCheckFlagValue);
      Function GetReadAttributes: TCheckFlagValue;
      Procedure SetReadAttributes(Const Value: TCheckFlagValue);
      Function GetWriteAttributes: TCheckFlagValue;
      Procedure SetWriteAttributes(Const Value: TCheckFlagValue);
      Function GetSynchronize: TCheckFlagValue;
      Procedure SetSynchronize(Const Value: TCheckFlagValue);
    Protected
      Function GetAttributes(Const lng: Cardinal): TCheckFlagValue;                Virtual;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: TCheckFlagValue);  Virtual;
    Public
      Constructor Create(Filer: TCustomNTFileFlagsFiler; Const ro: Boolean);       Overload;
      Destructor Destroy;                                                          Override;
      Procedure Assign(Source: TPersistent);                                       Override;
      Property ReadOnly: Boolean                    Read FReadOnly;
    Published
      Property CanDelete: TCheckFlagValue           Read GetDelete          Write SetDelete;
      Property CanReadControl: TCheckFlagValue      Read GetReadControl     Write SetReadControl;
      Property CanWriteDac: TCheckFlagValue         Read GetWriteDac        Write SetWriteDac;
      Property CanWriteOwner: TCheckFlagValue       Read GetWriteOwner      Write SetWriteOwner;
      Property CanReadAttributes: TCheckFlagValue   Read GetReadAttributes  Write SetReadAttributes;
      Property CanWriteAttributes: TCheckFlagValue  Read GetWriteAttributes Write SetWriteAttributes;
      Property CanSynchronize: TCheckFlagValue      Read GetSynchronize     Write SetSynchronize;
    End;

    TNTDiskFilesCheckFlags = Class(TCustomNTFileCheckFlags)
    Private
      Function GetReadExtended: TCheckFlagValue;
      Procedure SetReadExtended(Const Value: TCheckFlagValue);
      Function GetWriteExtended: TCheckFlagValue;
      Procedure SetWriteExtended(Const Value: TCheckFlagValue);
    Public
      Procedure Assign(Source: TPersistent);                                    Override;
    Published
      Property CanReadExtended: TCheckFlagValue     Read GetReadExtended    Write SetReadExtended;
      Property CanWriteExtented: TCheckFlagValue    Read GetWriteExtended   Write SetWriteExtended;
    End;

    TNTFileCheckFlags = Class(TNTDiskFilesCheckFlags)
    Private
      Function GetReadData: TCheckFlagValue;
      Procedure SetReadData(Const Value: TCheckFlagValue);
      Function GetWriteData: TCheckFlagValue;
      Procedure SetWriteData(Const Value: TCheckFlagValue);
      Function GetAppendData: TCheckFlagValue;
      Procedure SetAppendData(Const Value: TCheckFlagValue);
      Function GetExecute: TCheckFlagValue;
      Procedure SetExecute(Const Value: TCheckFlagValue);
    Protected
      Function GetFlagsMode: TNTFileFlagsMode;                                  Virtual;
      Procedure SetFlagsMode(Const Value: TNTFileFlagsMode);                    Virtual;
    Public
      Procedure Assign(Source: TPersistent);                                    Override;
    Published
      Property CanReadData: TCheckFlagValue         Read GetReadData        Write SetReadData;
      Property CanWriteData: TCheckFlagValue        Read GetWriteData       Write SetWriteData;
      Property CanAppendData: TCheckFlagValue       Read GetAppendData      Write SetAppendData;
      Property CanExecute: TCheckFlagValue          Read GetExecute         Write SetExecute;
      Property FlagsMode: TNTFileFlagsMode          Read GetFlagsMode       Write SetFlagsMode     Default ffmRead;
    End;

    TNTDirCheckFlags = Class(TNTDiskFilesCheckFlags)
    Private
      Function GetListDir: TCheckFlagValue;
      Procedure SetListDir(Const Value: TCheckFlagValue);
      Function GetAddFile: TCheckFlagValue;
      Procedure SetAddFile(Const Value: TCheckFlagValue);
      Function GetAddSubDir: TCheckFlagValue;
      Procedure SetAddSubDir(Const Value: TCheckFlagValue);
      Function GetTraverse: TCheckFlagValue;
      Procedure SetTraverse(Const Value: TCheckFlagValue);
      Function GetDeleteChild: TCheckFlagValue;
      Procedure SetDeleteChild(Const Value: TCheckFlagValue);
    Protected
      Function GetFlagsMode: TNTDirFlagsMode;                                   Virtual;
      Procedure SetFlagsMode(Const Value: TNTDirFlagsMode);                     Virtual;
    Public
      Procedure Assign(Source: TPersistent);                                    Override;
    Published
      Property CanListDir: TCheckFlagValue         Read GetListDir        Write SetListDir;
      Property CanAddFile: TCheckFlagValue         Read GetAddFile        Write SetAddFile;
      Property CanAddSubDir: TCheckFlagValue       Read GetAddSubDir      Write SetAddSubDir;
      Property CanTraverse: TCheckFlagValue        Read GetTraverse       Write SetTraverse;
      Property CanDeleteChild: TCheckFlagValue     Read GetDeleteChild    Write SetDeleteChild;
      Property FlagsMode: TNTDirFlagsMode          Read GetFlagsMode      Write SetFlagsMode     Default dfmRead;
    End;

    TNTPipeCheckFlags = Class(TCustomNTFileCheckFlags)
    Private
      Function GetReadData: TCheckFlagValue;
      Procedure SetReadData(Const Value: TCheckFlagValue);
      Function GetWriteData: TCheckFlagValue;
      Procedure SetWriteData(Const Value: TCheckFlagValue);
      Function GetCreateInstance: TCheckFlagValue;
      Procedure SetCreateInstance(Const Value: TCheckFlagValue);
    Public
      Procedure Assign(Source: TPersistent);                                    Override;
    Published
      Property CanReadData: TCheckFlagValue         Read GetReadData        Write SetReadData;
      Property CanWriteData: TCheckFlagValue        Read GetWriteData       Write SetWriteData;
      Property CanCreateInstance: TCheckFlagValue   Read GetCreateInstance  Write SetCreateInstance;
    End;

    // Flags Filers
    TCustomNTFileFlagsFiler = Class(TPersistent)
    Protected
      Function GetAttributes(Const lng: Cardinal): Cardinal;                    Virtual; Abstract;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: Cardinal);      Virtual; Abstract;
    End;

    TNTFileAccessFlagsFiler = Class(TCustomNTFileFlagsFiler)
    Private
      FAce: TCustomNTFileAccess;
    Protected
      Function GetAttributes(Const lng: Cardinal): Cardinal;                    Override;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: Cardinal);      Override;
    Public
      Constructor Create(Ace: TCustomNTFileAccess);                             Overload;
      Property ACE: TCustomNTFileAccess              Read FAce;
    End;

    TNTFileAuditFlagsFiler = Class(TCustomNTFileFlagsFiler)
    Private
      FAce: TCustomNTFileAudit;
    Protected
      Function GetAttributes(Const lng: Cardinal): Cardinal;                    Override;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: Cardinal);      Override;
    Public
      Constructor Create(Ace: TCustomNTFileAudit);                              Overload;
      Property ACE: TCustomNTFileAudit              Read FAce;
    End;

    TNTFileCheckFlagsFiler = Class(TCustomNTFileFlagsFiler)
    Private
      FCheck: TNTCustomFileSecurityCheck;
    Public
      Constructor Create(aCheck: TNTCustomFileSecurityCheck);                   Overload;
      Property Check: TNTCustomFileSecurityCheck    Read FCheck;
    End;

    TNTFileDesiredCheckFlagsFiler = Class(TNTFileCheckFlagsFiler)
    Protected
      Function GetAttributes(Const lng: Cardinal): Cardinal;                    Override;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: Cardinal);      Override;
    End;

    TNTFileGrantedCheckFlagsFiler = Class(TNTFileCheckFlagsFiler)
    Protected
      Function GetAttributes(Const lng: Cardinal): Cardinal;                    Override;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: Cardinal);      Override;
    End;


    Function FileReadSecurity(Const Filename: String; SecurityInformation: Cardinal): PSecurityDescriptor;
    Procedure FileSaveSecurity(Const Filename: String; SecurityInformation: Cardinal; sd: PSecurityDescriptor);

{==============================================================================}
{                                                                              }
{                                                                              }
{ I M P L E M E N T A T I O N                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
 Implementation

{------------------------------------------------------------------------------}
{ Private Uses                                                                 }
{------------------------------------------------------------------------------}
 Uses FileCtrl;

{------------------------------------------------------------------------------}
{ Constantes                                                                   }
{------------------------------------------------------------------------------}
 ResourceString
   SNotAllowed       = 'Opération non autorisée !';
 
{------------------------------------------------------------------------------}
{ FileReadSecurity(Filename: String): PSecurityDescriptor;                     }
{ Read a File Security Descriptor                                              }
{------------------------------------------------------------------------------}
 Function FileReadSecurity(Const Filename: String; SecurityInformation: Cardinal): PSecurityDescriptor;
 Var
    Err     : Cardinal;
    lpNeeded: Cardinal;
    dlen    : Cardinal;
    //access : Cardinal;
    desc    : PSecurityDescriptor;
    tkprivs : TTokenPrivileges;
    saclhdl : THandle;

   {---------------------------------------------------------------------------}
    Function InitializeTokens: Cardinal;
    Var
       outlen : Cardinal;
       tmpprvs: TTokenPrivileges;
    Begin
       Result := 0;
       { Owner Information }
       If ((SecurityInformation And OWNER_SECURITY_INFORMATION)=OWNER_SECURITY_INFORMATION) Then
          Result :=  Result Or READ_CONTROL;
       { Primary Group Information }
       If ((SecurityInformation And GROUP_SECURITY_INFORMATION)=GROUP_SECURITY_INFORMATION) Then
          Result :=  Result Or READ_CONTROL;
       { Accesses Information }
       If ((SecurityInformation And DACL_SECURITY_INFORMATION)=DACL_SECURITY_INFORMATION) Then
          Result :=  Result Or READ_CONTROL;
       { Audit Information }
       If ((SecurityInformation And SACL_SECURITY_INFORMATION)=SACL_SECURITY_INFORMATION) Then Begin
          Result := Result Or ACCESS_SYSTEM_SECURITY;
          If Not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES Or TOKEN_QUERY, saclhdl) Then
             RaiseACLError;
          Zeromemory(@tmpprvs, SizeOf(tmpprvs));
          Zeromemory(@tkprivs, SizeOf(tkprivs));
          outlen := 0;
          If Not LookupPrivilegeValue(Nil, SE_SECURITY_NAME, tmpprvs.Privileges[0].Luid) Then
             RaiseACLError;
          tmpprvs.PrivilegeCount := 1;
          tmpprvs.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
          If Not AdjustTokenPrivileges(saclhdl, False, tmpprvs, SizeOf(tkprivs), tkprivs, outlen) Then
             RaiseACLError;
       End;
    End;

   {---------------------------------------------------------------------------}
    Procedure FinalizeTokens;
    Var
       outlen : Cardinal;
       DoIt: Boolean;
    Begin
       DoIt := False;
       { Owner Information }
       If ((SecurityInformation And OWNER_SECURITY_INFORMATION)=OWNER_SECURITY_INFORMATION) Then Begin
          { Nothing }
       End;
       { Primary Group Information }
       If ((SecurityInformation And GROUP_SECURITY_INFORMATION)=GROUP_SECURITY_INFORMATION) Then Begin
          { Nothing }
       End;
       { Accesses Information }
       If ((SecurityInformation And DACL_SECURITY_INFORMATION)=DACL_SECURITY_INFORMATION) Then Begin
          { Nothing }
       End;
       { Audit Information }
       If ((SecurityInformation And SACL_SECURITY_INFORMATION)=SACL_SECURITY_INFORMATION) Then
          DoIt := True;
       If DoIt Then Begin
          outlen := 0;
          Try
             If Not AdjustTokenPrivileges(saclhdl, False, tkprivs, 0, Nil, outlen) Then
                RaiseACLError;
          Finally
             CloseHandle(saclhdl);
          End;
       End;
    End;
   {---------------------------------------------------------------------------}

 Begin
    Result := Nil;
    SetLastError(0);
    dlen := 0;
    // access := InitializeTokens;
    InitializeTokens;
    Try
       If Not GetFileSecurity(PChar(FileName), SecurityInformation, Nil, 0, dlen) Then Begin
          Err := GetLastError;
          If (Err<>ERROR_INSUFFICIENT_BUFFER) Then
             RaiseACLError(Err);
       End;
       desc := sdAlloc(dlen);
       Try
          If Not GetFileSecurity(PChar(FileName), SecurityInformation, desc, dlen, lpNeeded) Then
             RaiseACLError;
          Try
             Result := MakeAbsoluteSecurityDescriptor(desc);
          Except
             sdFree(Result);
             Raise;
          End;
       Finally
          sdFree(desc, dlen);
       End;
    Finally
       FinalizeTokens;
    End;
 End;

{------------------------------------------------------------------------------}
{ FileSaveSecurity(Const Filename: String; sd: PSecurityDescriptor);           }
{ Save a File Security Descriptor                                              }
{------------------------------------------------------------------------------}
 Procedure FileSaveSecurity(Const Filename: String; SecurityInformation: Cardinal; sd: PSecurityDescriptor);
 Var
   // access : Cardinal;
    tkprivs: TTokenPrivileges;
    saclhdl: THandle;

    {---------------------------------------------------------------------------}
    Function InitializeTokens: Cardinal;
    Var
       outlen : Cardinal;
       tmpprvs: TTokenPrivileges;
    Begin
       Result := READ_CONTROL;
       { Owner Information }
       If ((SecurityInformation And OWNER_SECURITY_INFORMATION)=OWNER_SECURITY_INFORMATION) Then Begin
          Result := Result Or WRITE_OWNER;
          If Not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES Or TOKEN_QUERY, saclhdl) Then
             RaiseACLError;
          Zeromemory(@tmpprvs, SizeOf(tmpprvs));
          Zeromemory(@tkprivs, SizeOf(tkprivs));
          outlen := 0;
          If Not LookupPrivilegeValue(Nil, SE_TAKE_OWNERSHIP_NAME, tmpprvs.Privileges[0].Luid) Then
             RaiseACLError;
          tmpprvs.PrivilegeCount := 1;
          tmpprvs.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
          If Not AdjustTokenPrivileges(saclhdl, False, tmpprvs, SizeOf(tkprivs), tkprivs, outlen) Then
             RaiseACLError;
       End;
       { Primary Group Information }
       If ((SecurityInformation And GROUP_SECURITY_INFORMATION)=GROUP_SECURITY_INFORMATION) Then
          Result := Result Or WRITE_OWNER;
       { Accesses Information }
       If ((SecurityInformation And DACL_SECURITY_INFORMATION)=DACL_SECURITY_INFORMATION) Then
          Result := Result Or WRITE_DAC;
       { Audit Information }
       If ((SecurityInformation And SACL_SECURITY_INFORMATION)=SACL_SECURITY_INFORMATION) Then Begin
          Result := Result Or ACCESS_SYSTEM_SECURITY;
          If Not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES Or TOKEN_QUERY, saclhdl) Then
             RaiseACLError;
          Zeromemory(@tmpprvs, SizeOf(tmpprvs));
          Zeromemory(@tkprivs, SizeOf(tkprivs));
          outlen := 0;
          If Not LookupPrivilegeValue(Nil, SE_SECURITY_NAME, tmpprvs.Privileges[0].Luid) Then
             RaiseACLError;
          tmpprvs.PrivilegeCount := 1;
          tmpprvs.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
          If Not AdjustTokenPrivileges(saclhdl, False, tmpprvs, SizeOf(tkprivs), tkprivs, outlen) Then
             RaiseACLError;
       End;
    End;

   {---------------------------------------------------------------------------}
    Procedure FinalizeTokens;
    Var
       outlen : Cardinal;
       DoIt: Boolean;
    Begin
       DoIt := False;
       { Primary Group Information }
       If ((SecurityInformation And GROUP_SECURITY_INFORMATION)=GROUP_SECURITY_INFORMATION) Then Begin
          { Nothing }
       End;
       { Accesses Information }
       If ((SecurityInformation And DACL_SECURITY_INFORMATION)=DACL_SECURITY_INFORMATION) Then Begin
          { Nothing }
       End;
       { Owner Information }
       If ((SecurityInformation And OWNER_SECURITY_INFORMATION)=OWNER_SECURITY_INFORMATION) Then
          DoIt := True;
       { Audit Information }
       If ((SecurityInformation And SACL_SECURITY_INFORMATION)=SACL_SECURITY_INFORMATION) Then
          DoIt := True;
       If DoIt Then Begin
          outlen := 0;
          Try
             If Not AdjustTokenPrivileges(saclhdl, False, tkprivs, 0, Nil, outlen) Then
                RaiseACLError;
          Finally
             CloseHandle(saclhdl);
          End;
       End;
    End;
   {---------------------------------------------------------------------------}

 Begin
    SetLastError(0);
   // access := InitializeTokens;
    InitializeTokens;
    Try
       If Not SetFileSecurity(PChar(FileName), SecurityInformation, sd) Then
          RaiseACLError;
    Finally
       FinalizeTokens;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTCustomFileSecurity                                         }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.CreateAccessList: TDACL;                               }
{ Create the Access List                                                       }
{------------------------------------------------------------------------------}
 Function TNTCustomFileSecurity.CreateAccessList: TDACL;
 Begin
    Result := TNTFileAccesses.Create(Self, Self, TNTFileAccess)
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.CreateAuditList: TSACL;                                }
{ Create the Audit List                                                        }
{------------------------------------------------------------------------------}
 Function TNTCustomFileSecurity.CreateAuditList: TSACL;
 Begin
    Result := TNTFileAudits.Create(Self, Self, TNTFileAudit);
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.CreateOwnerID: TCreatorSecurityID;                     }
{ Create the Owner ID                                                          }
{------------------------------------------------------------------------------}
 Function TNTCustomFileSecurity.CreateOwnerID: TCreatorSecurityID;
 Begin
    Result := TCreatorOwnerID.Create(Self);
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.CreateGroupID: TCreatorSecurityID;                     }
{ Create the Group ID                                                          }
{------------------------------------------------------------------------------}
 Function TNTCustomFileSecurity.CreateGroupID: TCreatorSecurityID;
 Begin
    Result := TCreatorGroupID.Create(Self);
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.GetAccessList: TNTFileAccesses;                        }
{ Read method property AccessList                                              }
{------------------------------------------------------------------------------}
 Function TNTCustomFileSecurity.GetDList: TNTFileAccesses;
 Begin
    If (sdWantACL In Options) Then
       Result := TNTFileAccesses(GetAccessList)
    Else
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.SetDList(Const Value: TNTFileAccesses);                }
{ Write method property AccessList                                             }
{------------------------------------------------------------------------------}
 Procedure TNTCustomFileSecurity.SetDList(Const Value: TNTFileAccesses);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.GetSList: TNTFileAudits;                               }
{ Read method property AuditList                                               }
{------------------------------------------------------------------------------}
 Function TNTCustomFileSecurity.GetSList: TNTFileAudits;
 Begin
    If (sdWantSystemACL In Options) Then
       Result := TNTFileAudits(GetAuditList)
    Else
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.SetSList(Const Value: TNTFileAudits);                  }
{ Write method property AuditList                                              }
{------------------------------------------------------------------------------}
 Procedure TNTCustomFileSecurity.SetSList(Const Value: TNTFileAudits);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.GetFOwner: TCreatorOwnerID;                            }
{ Read method property CreatorOwner                                            }
{------------------------------------------------------------------------------}
 Function TNTCustomFileSecurity.GetFOwner: TCreatorOwnerID;
 Begin
    If (sdWantOwner In Options) Then
       Result := TCreatorOwnerID(GetOwnerID)
    Else
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.SetFOwner(Const Value: TCreatorOwnerID);               }
{ Write method property CreatorOwner                                           }
{------------------------------------------------------------------------------}
 Procedure TNTCustomFileSecurity.SetFOwner(Const Value: TCreatorOwnerID);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.GetFGroup: TCreatorGroupID;                            }
{ Read method property CreatorGroup                                            }
{------------------------------------------------------------------------------}
 Function TNTCustomFileSecurity.GetFGroup: TCreatorGroupID;
 Begin
    If (sdWantGroup In Options) Then
       Result := TCreatorGroupID(GetGroupID)
    Else
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.SetFGroup(Const Value: TCreatorGroupID);               }
{ Write method property CreatorGroup                                           }
{------------------------------------------------------------------------------}
 Procedure TNTCustomFileSecurity.SetFGroup(Const Value: TCreatorGroupID);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.ReadObjectDescriptor: PSecurityDescriptor;             }
{ Read the SecurityDescriptor                                                  }
{------------------------------------------------------------------------------}
 Function TNTCustomFileSecurity.ReadObjectDescriptor: PSecurityDescriptor;
 Begin
    Result := FileReadSecurity(FObjectName, SecurityInf);
 End;

{------------------------------------------------------------------------------}
{ TNTCustomFileSecurity.WriteObjectDescriptor: DWORD;                          }
{ Writes the SecurityDescriptor                                                }
{------------------------------------------------------------------------------}
 Procedure TNTCustomFileSecurity.WriteObjectDescriptor;
 Begin
    CheckSD;
    FileSaveSecurity(FObjectName, SecurityInf, Descriptor);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileSecurity                                               }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileSecurity.GetFileName: String;                                         }
{ Reade method Property FileName                                               }
{------------------------------------------------------------------------------}
 Function TNTFileSecurity.GetFileName: String;
 Begin
    Result := FObjectName;
 End;

{------------------------------------------------------------------------------}
{ TNTFileSecurity.SetFilename(Const Value: String);                            }
{ Write method Property FileName                                               }
{------------------------------------------------------------------------------}
 Procedure TNTFileSecurity.SetFilename(Const Value: String);
 Var
    Sv: Boolean;
 Begin
    Try
       Sv := Active;
       If Sv Then
          Active := False;
       FObjectName := Value;
       If Sv Then
          Active := True;
    Except
       Active := False;
       Raise;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTDirectorySecurity                                          }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTDirectorySecurity.CreateAccessList: TDACL;                                }
{ Create the Access List                                                       }
{------------------------------------------------------------------------------}
 Function TNTDirectorySecurity.CreateAccessList: TDACL;
 Begin
    Result := TNTFileAccesses.Create(Self, Self, TNTDirectoryAccess)
 End;

{------------------------------------------------------------------------------}
{ TNTDirectorySecurity.GetDirectory: String;                                   }
{ Reade method Property Directory                                              }
{------------------------------------------------------------------------------}
 Function TNTDirectorySecurity.GetDirectory: String;
 Begin
    Result := FObjectName;
 End;

{------------------------------------------------------------------------------}
{ TNTDirectorySecurity.SetDirectory(Const Value: String);                      }
{ Write method Property Directory                                              }
{------------------------------------------------------------------------------}
 Procedure TNTDirectorySecurity.SetDirectory(Const Value: String);
 Var
    Sv: Boolean;
 Begin
    Try
       Sv := Active;
       If Sv Then
          Active := False;
       FObjectName := Value;
       If Sv Then
          Active := True;
    Except
       Active := False;
       Raise;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileAccesses                                               }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileAccesses.GetItems(...): TCustomNTFileAccess;                          }
{ Read Method property Items                                                   }
{------------------------------------------------------------------------------}
 Function TNTFileAccesses.GetItems(Index: Integer): TCustomNTFileAccess;
 Begin
    Result := TCustomNTFileAccess(Inherited Items[Index]);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccesses.SetItems(...);                                               }
{ Write Method property Items                                                  }
{------------------------------------------------------------------------------}
 Procedure TNTFileAccesses.SetItems(Index: Integer; Const Value: TCustomNTFileAccess);
 Begin
    Inherited Items[Index] := Value;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileAudits                                                 }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileAudits.GetItems(...): TCustomNTFileAudit;                             }
{ Read Method property Items                                                   }
{------------------------------------------------------------------------------}
 Function TNTFileAudits.GetItems(Index: Integer): TCustomNTFileAudit;
 Begin
    Result := TCustomNTFileAudit(Inherited Items[Index]);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAudits.SetItems(...);                                                 }
{ Write Method property Items                                                  }
{------------------------------------------------------------------------------}
 Procedure TNTFileAudits.SetItems(Index: Integer; Const Value: TCustomNTFileAudit);
 Begin
    Inherited Items[Index] := Value;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomNTFileAccess                                           }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomNTFileAccess.Create(Collection: TCollection);                         }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCustomNTFileAccess.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    Applyto := [fifDir, fifSubDirs];
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccess.GetApplyTo: TNTFileApplyFlags;                           }
{ Read method property ApplyTo                                                 }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAccess.GetApplyTo: TNTFileApplyFlags;
 Begin
    Result := [fifDir];
    If (afNoPropagate In AceFlags) Then
       Include(Result, fifSetOnly);
    If (afInheritOnly In AceFlags) Then
       Exclude(Result, fifDir);
    If (afContainerInherit In AceFlags) Then
       Include(Result, fifSubDirs);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccess.SetApplyTo(Const Value: TNTFileApplyFlags);              }
{ Write method property ApplyTo                                                }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAccess.SetApplyTo(Const Value: TNTFileApplyFlags);
 Begin
    If IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    AceFlags := Aceflags+[afInheritOnly]-[afNoPropagate, afContainerInherit];
    If (fifSubDirs In Value) Then
       AceFlags := AceFlags+[afContainerInherit];
    If (fifSetOnly In Value) Then
       AceFlags := AceFlags+[afNoPropagate];
    If (fifDir In Value) Then
       AceFlags := AceFlags-[afInheritOnly];
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccess.GetDeniedAce(Value: PACL);                               }
{ GetDeniedAce method Override                                                 }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAccess.GetDeniedAce(Value: PACL);
 Var
    xAce: Pointer;
 Begin
    SetLastError(0);
    If MaskFlags[fkNo]=0 Then Exit;
    Case Status Of
       asnone, asmodified, asadded:
         Begin
            If Not AddAccessdeniedAce(Value^, ACL_REVISION, MaskFlags[fkNo], Identifier.AsSID) Then
               RaiseACLError;
            If Not GetAce(Value^, Value^.AceCount-1, xAce) Then
               RaiseACLError;
            PAce(xAce)^.AccessDeniedAce.Header.AceFlags := AcesToAceFlags(AceFlags);
         End;
       asdeleted: ;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccess.GetAllowedAce(Value: PACL);                              }
{ GetAllowedAce method Override                                                }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAccess.GetAllowedAce(Value: PACL);
 Var
    xAce: Pointer;
 Begin
    SetLastError(0);
    If MaskFlags[fkYes]=0 Then Exit;
    Case Status Of
       asnone, asmodified, asadded:
         Begin
            If Not AddAccessAllowedAce(Value^, ACL_REVISION, MaskFlags[fkYes], Identifier.AsSID) Then
               RaiseACLError;
            If Not GetAce(Value^, Value^.AceCount-1, xAce) Then
               RaiseACLError;
            PAce(xAce)^.AccessAllowedAce.Header.AceFlags := AcesToAceFlags(AceFlags);
         End;
       asdeleted: ;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccess.GetDeniedSize: Integer;                                  }
{ GetDeniedSize method Override                                                }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAccess.GetDeniedSize: Integer;
 Begin
    Result := 0;
    If MaskFlags[fkNo]=0 Then Exit;
    Result := SizeOf(TAceHeader)+SizeOf(TAccessDeniedAce)+SizeOf(Byte)-SizeOf(DWORD);
    Inc(Result, GetLengthSid(Identifier.AsSID));
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccess.GetAllowedSize: Integer;                                 }
{ GetAllowedSize method Override                                               }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAccess.GetAllowedSize: Integer;
 Begin
    Result := 0;
    If (MaskFlags[fkYes]=0) Then Exit;
    Result := SizeOf(TAceHeader)+SizeOf(TAccessAllowedAce)+SizeOf(Byte)-SizeOf(DWORD);
    Inc(Result, GetLengthSid(Identifier.AsSID));
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomNTFileAudit                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomNTFileAudit.Create(Collection: TCollection);                          }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCustomNTFileAudit.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    Applyto := [fifDir, fifSubDirs];
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAudit.GetApplyTo: TNTFileApplyFlags;                            }
{ Read method property ApplyTo                                                 }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAudit.GetApplyTo: TNTFileApplyFlags;
 Begin
    Result := [fifDir];
    If (afNoPropagate In AceFlags) Then
       Include(Result, fifSetOnly);
    If (afInheritOnly In AceFlags) Then
       Exclude(Result, fifDir);
    If (afContainerInherit In AceFlags) Then
       Include(Result, fifSubDirs);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAudit.SetApplyTo(Const Value: TNTFileApplyFlags);               }
{ Write method property ApplyTo                                                }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAudit.SetApplyTo(Const Value: TNTFileApplyFlags);
 Begin
    If IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    AceFlags := Aceflags+[afInheritOnly]-[afNoPropagate, afContainerInherit];
    If (fifSubDirs In Value) Then
       AceFlags := AceFlags+[afContainerInherit];
    If (fifSetOnly In Value) Then
       AceFlags := AceFlags+[afNoPropagate];
    If (fifDir In Value) Then
       AceFlags := AceFlags-[afInheritOnly];
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAudit.GetDeniedAce(Value: PACL);                                }
{ GetDeniedAce method Override                                                 }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAudit.GetDeniedAce(Value: PACL);
 Var
    xAce: Pointer;
 Begin
    SetLastError(0);
    If MaskFlags[fkNo]=0 Then Exit;
    Case Status Of
       asnone, asmodified, asadded:
         Begin
            If Not AddAuditAccessAce(Value^, ACL_REVISION, MaskFlags[fkNo], Identifier.AsSID, False, True) Then
               RaiseACLError;
            If Not GetAce(Value^, Value^.AceCount-1, xAce) Then
               RaiseACLError;
            PAce(xAce)^.SystemAuditAce.Header.AceFlags := PAce(xAce)^.SystemAuditAce.Header.AceFlags Or (AcesToAceFlags(AceFlags) And Not (SUCCESSFUL_ACCESS_ACE_FLAG Or FAILED_ACCESS_ACE_FLAG));
         End;
       asdeleted: ;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAudit.GetAllowedAce(Value: PACL);                               }
{ GetAllowedAce method Override                                                }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAudit.GetAllowedAce(Value: PACL);
 Var
    xAce: Pointer;
 Begin
    SetLastError(0);
    If MaskFlags[fkYes]=0 Then Exit;
    Case Status Of
       asnone, asmodified, asadded:
         Begin
            If Not AddAuditAccessAce(Value^, ACL_REVISION, MaskFlags[fkYes], Identifier.AsSID, True, False) Then
               RaiseACLError;
            If Not GetAce(Value^, Value^.AceCount-1, xAce) Then
               RaiseACLError;
            PAce(xAce)^.SystemAuditAce.Header.AceFlags := PAce(xAce)^.SystemAuditAce.Header.AceFlags Or (AcesToAceFlags(AceFlags) And Not (SUCCESSFUL_ACCESS_ACE_FLAG Or FAILED_ACCESS_ACE_FLAG));
         End;
       asdeleted: ;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAudit.GetDeniedSize: Integer;                                   }
{ GetDeniedSize method Override                                                }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAudit.GetDeniedSize: Integer;
 Begin
    Result := 0;
    If MaskFlags[fkNo]=0 Then Exit;
    Result := SizeOf(TAceHeader)+SizeOf(TSystemAuditAce)+SizeOf(Byte)-SizeOf(DWORD);
    Inc(Result, GetLengthSid(Identifier.AsSID));
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAudit.GetAllowedSize: Integer;                                  }
{ GetAllowedSize method Override                                               }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAudit.GetAllowedSize: Integer;
 Begin
    Result := 0;
    If (MaskFlags[fkYes]=0) Then Exit;
    Result := SizeOf(TAceHeader)+SizeOf(TSystemAuditAce)+SizeOf(Byte)-SizeOf(DWORD);
    Inc(Result, GetLengthSid(Identifier.AsSID));
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileAccess                                                 }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileAccess.Create(Collection: TCollection);                               }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNTFileAccess.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    FFiler := TNTFileAccessFlagsFiler.Create(Self);
    FFileFlags := TNTFileAccessFlags.Create(FFiler);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccess.Destroy;                                                       }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNTFileAccess.Destroy;
 Begin
    FFileFlags.Free;
    FFiler.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccess.GetFlags: TNTFileAccessFlags;                                  }
{ Read method property Flags                                                   }
{------------------------------------------------------------------------------}
 Function TNTFileAccess.GetFlags: TNTFileAccessFlags;
 Begin
    Result := FFileFlags;
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccess.SetFlags(Const Value: TNTFileAccessFlags);                     }
{ Write method property Flags                                                  }
{------------------------------------------------------------------------------}
 Procedure TNTFileAccess.SetFlags(Const Value: TNTFileAccessFlags);
 Begin
    FFileFlags.Assign(Value);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileAudit                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileAudit.Create(Collection: TCollection);                                }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNTFileAudit.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    FFiler := TNTFileAuditFlagsFiler.Create(Self);
    FFileFlags := TNTFileAuditFlags.Create(FFiler);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAudit.Destroy;                                                        }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNTFileAudit.Destroy;
 Begin
    FFileFlags.Free;
    FFiler.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNTFileAudit.GetFlags: TNTFileAuditFlags;                                    }
{ Read method property Flags                                                   }
{------------------------------------------------------------------------------}
 Function TNTFileAudit.GetFlags: TNTFileAuditFlags;
 Begin
    Result := FFileFlags;
 End;

{------------------------------------------------------------------------------}
{ TNTFileAudit.SetFlags(Const Value: TNTFileAuditFlags);                       }
{ Write method property Flags                                                  }
{------------------------------------------------------------------------------}
 Procedure TNTFileAudit.SetFlags(Const Value: TNTFileAuditFlags);
 Begin
    FFileFlags.Assign(Value);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTDirectoryAccess                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTDirectoryAccess.Create(Collection: TCollection);                          }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNTDirectoryAccess.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    FFiler := TNTFileAccessFlagsFiler.Create(Self);
    FDirFlags := TNTDirAccessFlags.Create(FFiler);
 End;

{------------------------------------------------------------------------------}
{ TNTDirectoryAccess.Destroy;                                                  }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNTDirectoryAccess.Destroy;
 Begin
    FDirFlags.Free;
    FFiler.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNTDirectoryAccess.GetFlags: TNTDirAccessFlags;                              }
{ Read method property Flags                                                   }
{------------------------------------------------------------------------------}
 Function TNTDirectoryAccess.GetFlags: TNTDirAccessFlags;
 Begin
    Result := FDirFlags;
 End;

{------------------------------------------------------------------------------}
{ TNTDirectoryAccess.SetFlags(Const Value: TNTDirAccessFlags);                 }
{ Write method property Flags                                                  }
{------------------------------------------------------------------------------}
 Procedure TNTDirectoryAccess.SetFlags(Const Value: TNTDirAccessFlags);
 Begin
    FDirFlags.Assign(Value);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTDirectoryAudit                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTDirectoryAudit.Create(Collection: TCollection);                           }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNTDirectoryAudit.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    FFiler := TNTFileAuditFlagsFiler.Create(Self);
    FDirFlags := TNTDirAuditFlags.Create(FFiler);
 End;

{------------------------------------------------------------------------------}
{ TNTDirectoryAudit.Destroy;                                                   }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNTDirectoryAudit.Destroy;
 Begin
    FDirFlags.Free;
    FFiler.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNTDirectoryAudit.GetFlags: TNTDirAuditFlags;                                }
{ Read method property Flags                                                   }
{------------------------------------------------------------------------------}
 Function TNTDirectoryAudit.GetFlags: TNTDirAuditFlags;
 Begin
    Result := FDirFlags;
 End;

{------------------------------------------------------------------------------}
{ TNTDirectoryAudit.SetFlags(Const Value: TNTDirAuditFlags);                   }
{ Write method property Flags                                                  }
{------------------------------------------------------------------------------}
 Procedure TNTDirectoryAudit.SetFlags(Const Value: TNTDirAuditFlags);
 Begin
    FDirFlags.Assign(Value);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTPipeAccess                                                 }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTPipeAccess.Create(Collection: TCollection);                               }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNTPipeAccess.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    FFiler := TNTFileAccessFlagsFiler.Create(Self);
    FPipeFlags := TNTPipeAccessFlags.Create(FFiler);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAccess.Destroy;                                                       }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNTPipeAccess.Destroy;
 Begin
    FPipeFlags.Free;
    FFiler.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAccess.GetFlags: TNTPipeAccessFlags;                                  }
{ Read method property Flags                                                   }
{------------------------------------------------------------------------------}
 Function TNTPipeAccess.GetFlags: TNTPipeAccessFlags;
 Begin
    Result := FPipeFlags;
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAccess.SetFlags(Const Value: TNTPipeAccessFlags);                     }
{ Write method property Flags                                                  }
{------------------------------------------------------------------------------}
 Procedure TNTPipeAccess.SetFlags(Const Value: TNTPipeAccessFlags);
 Begin
    FPipeFlags.Assign(Value);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTPipeAudit                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTPipeAudit.Create(Collection: TCollection);                                }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNTPipeAudit.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    FFiler := TNTFileAuditFlagsFiler.Create(Self);
    FPipeFlags := TNTPipeAuditFlags.Create(FFiler);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAudit.Destroy;                                                        }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNTPipeAudit.Destroy;
 Begin
    FPipeFlags.Free;
    FFiler.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAudit.GetFlags: TNTPipeAuditFlags;                                    }
{ Read method property Flags                                                   }
{------------------------------------------------------------------------------}
 Function TNTPipeAudit.GetFlags: TNTPipeAuditFlags;
 Begin
    Result := FPipeFlags;
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAudit.SetFlags(Const Value: TNTPipeAuditFlags);                       }
{ Write method property Flags                                                  }
{------------------------------------------------------------------------------}
 Procedure TNTPipeAudit.SetFlags(Const Value: TNTPipeAuditFlags);
 Begin
    FPipeFlags.Assign(Value);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomNTFileAccessFlags                                      }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.Create(Filer: TNTFileAccessFlagsFiler);             }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCustomNTFileAccessFlags.Create(Filer: TNTFileAccessFlagsFiler);
 Begin
    Inherited Create;
    FFiler := Filer;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.Destroy;                                            }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TCustomNTFileAccessFlags.Destroy;
 Begin
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.GetAce: TCustomNTFileAccess;                        }
{ Read method property ACE                                                     }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAccessFlags.GetAce: TCustomNTFileAccess;
 Begin
    Result := FFiler.FAce;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.GetAttributes(...): TAccessFlagValue;               }
{ Generic Read attibutes method                                                }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAccessFlags.GetAttributes(Const lng: Cardinal): TAccessFlagValue;
 Begin
    Result := TAccessFlagValue(FFiler.GetAttributes(lng));
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.SetAttributes(...);                                 }
{ Generic Write attibutes method                                               }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAccessFlags.SetAttributes(Const lng: Cardinal; Const Value: TAccessFlagValue);
 Begin
    FFiler.SetAttributes(lng, Cardinal(Value));
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.GetDelete: TAccessFlagValue;                        }
{ Read method property CanDelete                                               }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAccessFlags.GetDelete: TAccessFlagValue;
 Begin
    Result := GetAttributes(_DELETE);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.SetDelete(Const Value: TAccessFlagValue);           }
{ Write method property CanDelete                                              }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAccessFlags.SetDelete(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(_DELETE, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.GetReadControl: TAccessFlagValue;                   }
{ Read method property ReadControl                                             }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAccessFlags.GetReadControl: TAccessFlagValue;
 Begin
    Result := GetAttributes(READ_CONTROL);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.SetReadControl(Const Value: TAccessFlagValue);      }
{ Write method property ReadControl                                            }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAccessFlags.SetReadControl(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(READ_CONTROL, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.GetWriteDac: TAccessFlagValue;                      }
{ Read method property WriteDac                                                }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAccessFlags.GetWriteDac: TAccessFlagValue;
 Begin
    Result := GetAttributes(WRITE_DAC);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.SetWriteDac(Const Value: TAccessFlagValue);         }
{ Write method property Write Dac                                              }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAccessFlags.SetWriteDac(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(WRITE_DAC, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.GetWriteOwner: TAccessFlagValue;                    }
{ Read method property WriteOwner                                              }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAccessFlags.GetWriteOwner: TAccessFlagValue;
 Begin
    Result := GetAttributes(WRITE_OWNER);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.SetWriteOwner(Const Value: TAccessFlagValue);       }
{ Write method property WriteOwner                                             }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAccessFlags.SetWriteOwner(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(WRITE_OWNER, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.GetReadAttributes: TAccessFlagValue;                }
{ Read method property CanReadAttributes                                       }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAccessFlags.GetReadAttributes: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_READ_ATTRIBUTES);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.SetReadAttributes(Const Value: TAccessFlagValue);   }
{ Write method property CanReadAttributes                                      }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAccessFlags.SetReadAttributes(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_READ_ATTRIBUTES, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.GetWriteAttributes: TAccessFlagValue;               }
{ Read method property CanWriteAttributes                                      }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAccessFlags.GetWriteAttributes: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_WRITE_ATTRIBUTES);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.SetWriteAttributes(Const Value: TAccessFlagValue);  }
{ Write method property CanWriteAttributes                                     }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAccessFlags.SetWriteAttributes(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_WRITE_ATTRIBUTES, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.GetSynchronize: TAccessFlagValue;                   }
{ Read method property CanSynchronize                                          }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAccessFlags.GetSynchronize: TAccessFlagValue;
 Begin
    Result := GetAttributes(SYNCHRONIZE);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.SetSynchronize(Const Value: TAccessFlagValue);      }
{ Write method property CanSynchronize                                         }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAccessFlags.SetSynchronize(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(SYNCHRONIZE, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.Assign(Source: TPersistent);                        }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAccessFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TCustomNTFileAccessFlags) Then
       With Source As TCustomNTFileAccessFlags Do Begin
          Self.CanDelete           := CanDelete;
          Self.CanReadControl      := CanReadControl;
          Self.CanWriteDac         := CanWriteDac;
          Self.CanWriteOwner       := CanWriteOwner;
          Self.CanReadAttributes   := CanReadAttributes;
          Self.CanWriteAttributes  := CanWriteAttributes;
          Self.CanSynchronize      := CanSynchronize;
       End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTDiskFilesAccessFlags                                       }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTDiskFilesAccessFlags.GetReadExtended: TAccessFlagValue;                   }
{ Read method property CanReadExtended                                         }
{------------------------------------------------------------------------------}
 Function TNTDiskFilesAccessFlags.GetReadExtended: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_READ_EA);
 End;

{------------------------------------------------------------------------------}
{ TNTDiskFilesAccessFlags.SetReadExtended(Const Value: TAccessFlagValue);      }
{ Write method property CanReadExtended                                        }
{------------------------------------------------------------------------------}
 Procedure TNTDiskFilesAccessFlags.SetReadExtended(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_READ_EA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDiskFilesAccessFlags.GetWriteExtended: TAccessFlagValue;                  }
{ Read method property CanWriteExtended                                        }
{------------------------------------------------------------------------------}
 Function TNTDiskFilesAccessFlags.GetWriteExtended: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_WRITE_EA);
 End;

{------------------------------------------------------------------------------}
{ TNTDiskFilesAccessFlags.SetWriteExtended(Const Value: TAccessFlagValue);     }
{ Write method property Notify                                                 }
{------------------------------------------------------------------------------}
 Procedure TNTDiskFilesAccessFlags.SetWriteExtended(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_WRITE_EA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDiskFilesAccessFlags.Assign(Source: TPersistent);                         }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TNTDiskFilesAccessFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TNTDiskFilesAccessFlags) Then
       With Source As TNTDiskFilesAccessFlags Do Begin
          Self.CanReadExtended := CanReadExtended;
          Self.CanWriteExtented:= CanWriteExtented;
       End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileAccessFlags                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileAccessFlags.GetReadData: TAccessFlagValue;                            }
{ Read method property CanReadData                                             }
{------------------------------------------------------------------------------}
 Function TNTFileAccessFlags.GetReadData: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_READ_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccessFlags.SetReadData(Const Value: TAccessFlagValue);               }
{ Write method property CanReadData                                            }
{------------------------------------------------------------------------------}
 Procedure TNTFileAccessFlags.SetReadData(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_READ_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccessFlags.GetWriteData: TAccessFlagValue;                           }
{ Read method property CanWriteData                                            }
{------------------------------------------------------------------------------}
 Function TNTFileAccessFlags.GetWriteData: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_WRITE_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccessFlags.SetWriteData(Const Value: TAccessFlagValue);              }
{ Write method property CanWriteData                                           }
{------------------------------------------------------------------------------}
 Procedure TNTFileAccessFlags.SetWriteData(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_WRITE_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccessFlags.GetAppendData: TAccessFlagValue;                          }
{ Read method property CanAppendData                                           }
{------------------------------------------------------------------------------}
 Function TNTFileAccessFlags.GetAppendData: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_APPEND_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccessFlags.SetAppendData(Const Value: TAccessFlagValue);             }
{ Write method property CanAppendData                                          }
{------------------------------------------------------------------------------}
 Procedure TNTFileAccessFlags.SetAppendData(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_APPEND_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccessFlags.GetExecute: TAccessFlagValue;                             }
{ Read method property CanExecute                                              }
{------------------------------------------------------------------------------}
 Function TNTFileAccessFlags.GetExecute: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_EXECUTE);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccessFlags.SetExecute(Const Value: TAccessFlagValue);                }
{ Write method property CanExecute                                             }
{------------------------------------------------------------------------------}
 Procedure TNTFileAccessFlags.SetExecute(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_EXECUTE, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccessFlags.Assign(Source: TPersistent);                              }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TNTFileAccessFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TNTFileAccessFlags) Then
       With Source As TNTFileAccessFlags Do Begin
          Self.CanReadData   := CanReadData;
          Self.CanWriteData  := CanWriteData;
          Self.CanAppendData := CanAppendData;
          Self.CanExecute    := CanExecute;
       End;
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccessFlags.GetFlagsMode: TNTFileFlagsMode;                           }
{ Read method property FlagsMode                                               }
{------------------------------------------------------------------------------}
 Function TNTFileAccessFlags.GetFlagsMode: TNTFileFlagsMode;
 Begin
    Result := ffmCustom;
    If ((CanDelete=fvNone) And
        (CanReadControl=fvAllowed) And
        (CanWriteDac=fvNone) And
        (CanWriteOwner=fvNone) And
        (CanReadAttributes=fvAllowed) And
        (CanWriteAttributes=fvNone) And
        (CanSynchronize=fvAllowed) And
        (CanReadExtended=fvAllowed) And
        (CanWriteExtented=fvNone) And
        (CanReadData=fvAllowed) And
        (CanWriteData=fvNone) And
        (CanAppendData=fvNone) And
        (CanExecute=fvNone)) Then
        Result := ffmRead
    Else If ((CanDelete=fvNone) And
        (CanReadControl=fvAllowed) And
        (CanWriteDac=fvNone) And
        (CanWriteOwner=fvNone) And
        (CanReadAttributes=fvAllowed) And
        (CanWriteAttributes=fvNone) And
        (CanSynchronize=fvAllowed) And
        (CanReadExtended=fvAllowed) And
        (CanWriteExtented=fvNone) And
        (CanReadData=fvAllowed) And
        (CanWriteData=fvNone) And
        (CanAppendData=fvNone) And
        (CanExecute=fvAllowed)) Then
        Result := ffmReadExecute
     Else If ((CanDelete=fvNone) And
        (CanReadControl=fvNone) And
        (CanWriteDac=fvNone) And
        (CanWriteOwner=fvNone) And
        (CanReadAttributes=fvNone) And
        (CanWriteAttributes=fvAllowed) And
        (CanSynchronize=fvAllowed) And
        (CanReadExtended=fvNone) And
        (CanWriteExtented=fvAllowed) And
        (CanReadData=fvNone) And
        (CanWriteData=fvAllowed) And
        (CanAppendData=fvAllowed) And
        (CanExecute=fvNone)) Then
        Result := ffmWrite
     Else If ((CanDelete=fvAllowed) And
        (CanReadControl=fvAllowed) And
        (CanWriteDac=fvNone) And
        (CanWriteOwner=fvNone) And
        (CanReadAttributes=fvAllowed) And
        (CanWriteAttributes=fvAllowed) And
        (CanSynchronize=fvAllowed) And
        (CanReadExtended=fvAllowed) And
        (CanWriteExtented=fvAllowed) And
        (CanReadData=fvAllowed) And
        (CanWriteData=fvAllowed) And
        (CanAppendData=fvAllowed) And
        (CanExecute=fvAllowed)) Then
        Result := ffmModify
     Else If ((CanDelete=fvAllowed) And
        (CanReadControl=fvAllowed) And
        (CanWriteDac=fvAllowed) And
        (CanWriteOwner=fvAllowed) And
        (CanReadAttributes=fvAllowed) And
        (CanWriteAttributes=fvAllowed) And
        (CanSynchronize=fvAllowed) And
        (CanReadExtended=fvAllowed) And
        (CanWriteExtented=fvAllowed) And
        (CanReadData=fvAllowed) And
        (CanWriteData=fvAllowed) And
        (CanAppendData=fvAllowed) And
        (CanExecute=fvAllowed)) Then
        Result := ffmAll;
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccessFlags.SetFlagsMode(Const Value: TNTFileFlagsMode);              }
{ Write method property FlagsMode                                              }
{------------------------------------------------------------------------------}
 Procedure TNTFileAccessFlags.SetFlagsMode(Const Value: TNTFileFlagsMode);
 Begin
    If ACE.IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    Case Value Of
      ffmRead:
        Begin
           CanDelete          := fvNone;
           CanReadControl     := fvAllowed;
           CanWriteDac        := fvNone;
           CanWriteOwner      := fvNone;
           CanReadAttributes  := fvAllowed;
           CanWriteAttributes := fvNone;
           CanSynchronize     := fvAllowed;
           CanReadExtended    := fvAllowed;
           CanWriteExtented   := fvNone;
           CanReadData        := fvAllowed;
           CanWriteData       := fvNone;
           CanAppendData      := fvNone;
           CanExecute         := fvNone;
        End;
      ffmReadExecute:
        Begin
           CanDelete          := fvNone;
           CanReadControl     := fvAllowed;
           CanWriteDac        := fvNone;
           CanWriteOwner      := fvNone;
           CanReadAttributes  := fvAllowed;
           CanWriteAttributes := fvNone;
           CanSynchronize     := fvAllowed;
           CanReadExtended    := fvAllowed;
           CanWriteExtented   := fvNone;
           CanReadData        := fvAllowed;
           CanWriteData       := fvNone;
           CanAppendData      := fvNone;
           CanExecute         := fvAllowed;
        End;
      ffmWrite:
        Begin
           CanDelete          := fvNone;
           CanReadControl     := fvNone;
           CanWriteDac        := fvNone;
           CanWriteOwner      := fvNone;
           CanReadAttributes  := fvNone;
           CanWriteAttributes := fvAllowed;
           CanSynchronize     := fvAllowed;
           CanReadExtended    := fvNone;
           CanWriteExtented   := fvAllowed;
           CanReadData        := fvNone;
           CanWriteData       := fvAllowed;
           CanAppendData      := fvAllowed;
           CanExecute         := fvNone;
        End;
      ffmModify:
        Begin
           CanDelete          := fvAllowed;
           CanReadControl     := fvAllowed;
           CanWriteDac        := fvNone;
           CanWriteOwner      := fvNone;
           CanReadAttributes  := fvAllowed;
           CanWriteAttributes := fvAllowed;
           CanSynchronize     := fvAllowed;
           CanReadExtended    := fvAllowed;
           CanWriteExtented   := fvAllowed;
           CanReadData        := fvAllowed;
           CanWriteData       := fvAllowed;
           CanAppendData      := fvAllowed;
           CanExecute         := fvAllowed;
        End;
      ffmAll:
        Begin
           CanDelete          := fvAllowed;
           CanReadControl     := fvAllowed;
           CanWriteDac        := fvAllowed;
           CanWriteOwner      := fvAllowed;
           CanReadAttributes  := fvAllowed;
           CanWriteAttributes := fvAllowed;
           CanSynchronize     := fvAllowed;
           CanReadExtended    := fvAllowed;
           CanWriteExtented   := fvAllowed;
           CanReadData        := fvAllowed;
           CanWriteData       := fvAllowed;
           CanAppendData      := fvAllowed;
           CanExecute         := fvAllowed;
        End;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTDirAccessFlags                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.GetListDir: TAccessFlagValue;                              }
{ Read method property CanListDir                                              }
{------------------------------------------------------------------------------}
 Function TNTDirAccessFlags.GetListDir: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_LIST_DIRECTORY);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.SetListDir(Const Value: TAccessFlagValue);                 }
{ Write method property CanListDir                                             }
{------------------------------------------------------------------------------}
 Procedure TNTDirAccessFlags.SetListDir(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_LIST_DIRECTORY, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.GetAddFile: TAccessFlagValue;                              }
{ Read method property CanAddFile                                              }
{------------------------------------------------------------------------------}
 Function TNTDirAccessFlags.GetAddFile: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_ADD_FILE);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.SetAddFile(Const Value: TAccessFlagValue);                 }
{ Write method property CanAddFile                                             }
{------------------------------------------------------------------------------}
 Procedure TNTDirAccessFlags.SetAddFile(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_ADD_FILE, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.GetAddSubDir: TAccessFlagValue;                            }
{ Read method property CanAddSubDir                                            }
{------------------------------------------------------------------------------}
 Function TNTDirAccessFlags.GetAddSubDir: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_ADD_SUBDIRECTORY);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.SetAddSubDir(Const Value: TAccessFlagValue);               }
{ Write method property CanAddSubDir                                           }
{------------------------------------------------------------------------------}
 Procedure TNTDirAccessFlags.SetAddSubDir(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_ADD_SUBDIRECTORY, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.GetTraverse: TAccessFlagValue;                             }
{ Read method property CanTraverse                                             }
{------------------------------------------------------------------------------}
 Function TNTDirAccessFlags.GetTraverse: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_TRAVERSE);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.SetTraverse(Const Value: TAccessFlagValue);                }
{ Write method property CanTraverse                                            }
{------------------------------------------------------------------------------}
 Procedure TNTDirAccessFlags.SetTraverse(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_TRAVERSE, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.GetDeleteChild: TAccessFlagValue;                          }
{ Read method property CanDeleteChild                                          }
{------------------------------------------------------------------------------}
 Function TNTDirAccessFlags.GetDeleteChild: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_DELETE_CHILD);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.SetDeleteChild(Const Value: TAccessFlagValue);             }
{ Write method property CanDeleteChild                                         }
{------------------------------------------------------------------------------}
 Procedure TNTDirAccessFlags.SetDeleteChild(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_DELETE_CHILD, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.Assign(Source: TPersistent);                               }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TNTDirAccessFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TNTDirAccessFlags) Then
       With Source As TNTDirAccessFlags Do Begin
          Self.CanListDir     := CanListDir;
          Self.CanAddFile     := CanAddFile;
          Self.CanAddSubDir   := CanAddSubDir;
          Self.CanTraverse    := CanTraverse;
          Self.CanDeleteChild := CanDeleteChild;
       End;
 End;

{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.GetFlagsMode: TNTDirFlagsMode;                             }
{ Read method property FlagsMode                                               }
{------------------------------------------------------------------------------}
 Function TNTDirAccessFlags.GetFlagsMode: TNTDirFlagsMode;
 Begin
    Result := dfmCustom;
    If ((CanDelete=fvNone) And
        (CanReadControl=fvAllowed) And
        (CanWriteDac=fvNone) And
        (CanWriteOwner=fvNone) And
        (CanReadAttributes=fvAllowed) And
        (CanWriteAttributes=fvNone) And
        (CanSynchronize=fvAllowed) And
        (CanReadExtended=fvAllowed) And
        (CanWriteExtented=fvNone) And
        (CanListDir=fvAllowed) And
        (CanAddFile=fvNone) And
        (CanAddSubDir=fvNone) And
        (CanTraverse=fvNone) And
        (CanDeleteChild=fvNone)) Then
        Result := dfmRead
    Else If ((CanDelete=fvNone) And
        (CanReadControl=fvAllowed) And
        (CanWriteDac=fvNone) And
        (CanWriteOwner=fvNone) And
        (CanReadAttributes=fvAllowed) And
        (CanWriteAttributes=fvNone) And
        (CanSynchronize=fvAllowed) And
        (CanReadExtended=fvAllowed) And
        (CanWriteExtented=fvNone) And
        (CanListDir=fvAllowed) And
        (CanAddFile=fvNone) And
        (CanAddSubDir=fvNone) And
        (CanTraverse=fvAllowed) And
        (CanDeleteChild=fvNone)) Then
        Result := dfmReadExecute
     Else If ((CanDelete=fvNone) And
        (CanReadControl=fvNone) And
        (CanWriteDac=fvNone) And
        (CanWriteOwner=fvNone) And
        (CanReadAttributes=fvNone) And
        (CanWriteAttributes=fvAllowed) And
        (CanSynchronize=fvAllowed) And
        (CanReadExtended=fvNone) And
        (CanWriteExtented=fvAllowed) And
        (CanListDir=fvNone) And
        (CanAddFile=fvAllowed) And
        (CanAddSubDir=fvAllowed) And
        (CanTraverse=fvNone) And
        (CanDeleteChild=fvNone)) Then
        Result := dfmWrite
     Else If ((CanDelete=fvAllowed) And
        (CanReadControl=fvAllowed) And
        (CanWriteDac=fvNone) And
        (CanWriteOwner=fvNone) And
        (CanReadAttributes=fvAllowed) And
        (CanWriteAttributes=fvAllowed) And
        (CanSynchronize=fvAllowed) And
        (CanReadExtended=fvAllowed) And
        (CanWriteExtented=fvAllowed) And
        (CanListDir=fvAllowed) And
        (CanAddFile=fvAllowed) And
        (CanAddSubDir=fvAllowed) And
        (CanTraverse=fvAllowed) And
        (CanDeleteChild=fvNone)) Then
        Result := dfmModify
     Else If ((CanDelete=fvAllowed) And
        (CanReadControl=fvAllowed) And
        (CanWriteDac=fvAllowed) And
        (CanWriteOwner=fvAllowed) And
        (CanReadAttributes=fvAllowed) And
        (CanWriteAttributes=fvAllowed) And
        (CanSynchronize=fvAllowed) And
        (CanReadExtended=fvAllowed) And
        (CanWriteExtented=fvAllowed) And
        (CanListDir=fvAllowed) And
        (CanAddFile=fvAllowed) And
        (CanAddSubDir=fvAllowed) And
        (CanTraverse=fvAllowed) And
        (CanDeleteChild=fvAllowed)) Then
        Result := dfmAll;
 End;

{------------------------------------------------------------------------------}
{ TNTDirAccessFlags.SetFlagsMode(Const Value: TNTDirFlagsMode);                }
{ Write method property FlagsMode                                              }
{------------------------------------------------------------------------------}
 Procedure TNTDirAccessFlags.SetFlagsMode(Const Value: TNTDirFlagsMode);
 Begin
    If ACE.IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    Case Value Of
      dfmRead:
        Begin
           CanDelete          := fvNone;
           CanReadControl     := fvAllowed;
           CanWriteDac        := fvNone;
           CanWriteOwner      := fvNone;
           CanReadAttributes  := fvAllowed;
           CanWriteAttributes := fvNone;
           CanSynchronize     := fvAllowed;
           CanReadExtended    := fvAllowed;
           CanWriteExtented   := fvNone;
           CanListDir         := fvAllowed;
           CanAddFile         := fvNone;
           CanAddSubDir       := fvNone;
           CanTraverse        := fvNone;
           CanDeleteChild     := fvNone;
        End;
      dfmReadExecute:
        Begin
           CanDelete          := fvNone;
           CanReadControl     := fvAllowed;
           CanWriteDac        := fvNone;
           CanWriteOwner      := fvNone;
           CanReadAttributes  := fvAllowed;
           CanWriteAttributes := fvNone;
           CanSynchronize     := fvAllowed;
           CanReadExtended    := fvAllowed;
           CanWriteExtented   := fvNone;
           CanListDir         := fvAllowed;
           CanAddFile         := fvNone;
           CanAddSubDir       := fvNone;
           CanTraverse        := fvAllowed;
           CanDeleteChild     := fvNone;
        End;
      dfmWrite:
        Begin
           CanDelete          := fvNone;
           CanReadControl     := fvNone;
           CanWriteDac        := fvNone;
           CanWriteOwner      := fvNone;
           CanReadAttributes  := fvNone;
           CanWriteAttributes := fvAllowed;
           CanSynchronize     := fvAllowed;
           CanReadExtended    := fvNone;
           CanWriteExtented   := fvAllowed;
           CanListDir         := fvNone;
           CanAddFile         := fvAllowed;
           CanAddSubDir       := fvAllowed;
           CanTraverse        := fvNone;
           CanDeleteChild     := fvNone;
        End;
      dfmModify:
        Begin
           CanDelete          := fvAllowed;
           CanReadControl     := fvAllowed;
           CanWriteDac        := fvNone;
           CanWriteOwner      := fvNone;
           CanReadAttributes  := fvAllowed;
           CanWriteAttributes := fvAllowed;
           CanSynchronize     := fvAllowed;
           CanReadExtended    := fvAllowed;
           CanWriteExtented   := fvAllowed;
           CanListDir         := fvAllowed;
           CanAddFile         := fvAllowed;
           CanAddSubDir       := fvAllowed;
           CanTraverse        := fvAllowed;
           CanDeleteChild     := fvAllowed;
        End;
      dfmAll:
        Begin
           CanDelete          := fvAllowed;
           CanReadControl     := fvAllowed;
           CanWriteDac        := fvAllowed;
           CanWriteOwner      := fvAllowed;
           CanReadAttributes  := fvAllowed;
           CanWriteAttributes := fvAllowed;
           CanSynchronize     := fvAllowed;
           CanReadExtended    := fvAllowed;
           CanWriteExtented   := fvAllowed;
           CanListDir         := fvAllowed;
           CanAddFile         := fvAllowed;
           CanAddSubDir       := fvAllowed;
           CanTraverse        := fvAllowed;
           CanDeleteChild     := fvAllowed;
        End;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTPipeAccessFlags                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTPipeAccessFlags.GetReadData: TAccessFlagValue;                            }
{ Read method property CanReadData                                             }
{------------------------------------------------------------------------------}
 Function TNTPipeAccessFlags.GetReadData: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_READ_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAccessFlags.SetReadData(Const Value: TAccessFlagValue);               }
{ Write method property CanReadData                                            }
{------------------------------------------------------------------------------}
 Procedure TNTPipeAccessFlags.SetReadData(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_READ_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAccessFlags.GetWriteData: TAccessFlagValue;                           }
{ Read method property CanWriteData                                            }
{------------------------------------------------------------------------------}
 Function TNTPipeAccessFlags.GetWriteData: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_WRITE_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAccessFlags.SetWriteData(Const Value: TAccessFlagValue);              }
{ Write method property CanWriteData                                           }
{------------------------------------------------------------------------------}
 Procedure TNTPipeAccessFlags.SetWriteData(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_WRITE_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAccessFlags.GetCreateInstance: TAccessFlagValue;                      }
{ Read method property CanCreateInstance                                       }
{------------------------------------------------------------------------------}
 Function TNTPipeAccessFlags.GetCreateInstance: TAccessFlagValue;
 Begin
    Result := GetAttributes(FILE_CREATE_PIPE_INSTANCE);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAccessFlags.SetCreateInstance(Const Value: TAccessFlagValue);         }
{ Write method property CanCreateInstance                                      }
{------------------------------------------------------------------------------}
 Procedure TNTPipeAccessFlags.SetCreateInstance(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(FILE_CREATE_PIPE_INSTANCE, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAccessFlags.Assign(Source: TPersistent);                              }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TNTPipeAccessFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TNTPipeAccessFlags) Then
       With Source As TNTPipeAccessFlags Do Begin
          Self.CanReadData       := CanReadData;
          Self.CanWriteData      := CanWriteData;
          Self.CanCreateInstance := CanCreateInstance;
       End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomNTFileAuditFlags                                       }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.Create(Filer: TNTFileAuditFlagsFiler);               }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCustomNTFileAuditFlags.Create(Filer: TNTFileAuditFlagsFiler);
 Begin
    Inherited Create;
    FFiler := Filer;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.Destroy;                                             }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TCustomNTFileAuditFlags.Destroy;
 Begin
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.GetAce: TCustomNTFileAudit;                          }
{ Read method property ACE                                                     }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAuditFlags.GetAce: TCustomNTFileAudit;
 Begin
    Result := FFiler.Ace;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.GetAttributes(...): TAuditFlagValue;                 }
{ Generic Read attibutes method                                                }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAuditFlags.GetAttributes(Const lng: Cardinal): TAuditFlagValue;
 Begin
    Result := TAuditFlagValue(FFiler.GetAttributes(lng));
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.SetAttributes(...);                                  }
{ Generic Write attibutes method                                               }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAuditFlags.SetAttributes(Const lng: Cardinal; Const Value: TAuditFlagValue);
 Begin
    FFiler.SetAttributes(lng, Cardinal(Value));
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.GetDelete: TAuditFlagValue;                          }
{ Read method property CanDelete                                               }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAuditFlags.GetDelete: TAuditFlagValue;
 Begin
    Result := GetAttributes(_DELETE);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.SetDelete(Const Value: TAuditFlagValue);             }
{ Write method property CanDelete                                              }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAuditFlags.SetDelete(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(_DELETE, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.GetReadControl: TAuditFlagValue;                     }
{ Read method property ReadControl                                             }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAuditFlags.GetReadControl: TAuditFlagValue;
 Begin
    Result := GetAttributes(READ_CONTROL);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.SetReadControl(Const Value: TAuditFlagValue);        }
{ Write method property ReadControl                                            }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAuditFlags.SetReadControl(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(READ_CONTROL, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.GetWriteDac: TAuditFlagValue;                        }
{ Read method property WriteDac                                                }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAuditFlags.GetWriteDac: TAuditFlagValue;
 Begin
    Result := GetAttributes(WRITE_DAC);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.SetWriteDac(Const Value: TAuditFlagValue);           }
{ Write method property Write Dac                                              }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAuditFlags.SetWriteDac(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(WRITE_DAC, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.GetWriteOwner: TAuditFlagValue;                      }
{ Read method property WriteOwner                                              }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAuditFlags.GetWriteOwner: TAuditFlagValue;
 Begin
    Result := GetAttributes(WRITE_OWNER);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.SetWriteOwner(Const Value: TAuditFlagValue);         }
{ Write method property WriteOwner                                             }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAuditFlags.SetWriteOwner(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(WRITE_OWNER, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.GetReadAttributes: TAuditFlagValue;                  }
{ Read method property CanReadAttributes                                       }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAuditFlags.GetReadAttributes: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_READ_ATTRIBUTES);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.SetReadAttributes(Const Value: TAuditFlagValue);     }
{ Write method property CanReadAttributes                                      }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAuditFlags.SetReadAttributes(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_READ_ATTRIBUTES, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.GetWriteAttributes: TAuditFlagValue;                 }
{ Read method property CanWriteAttributes                                      }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAuditFlags.GetWriteAttributes: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_WRITE_ATTRIBUTES);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.SetWriteAttributes(Const Value: TAuditFlagValue);    }
{ Write method property CanWriteAttributes                                     }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAuditFlags.SetWriteAttributes(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_WRITE_ATTRIBUTES, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.GetSynchronize: TAuditFlagValue;                     }
{ Read method property CanSynchronize                                          }
{------------------------------------------------------------------------------}
 Function TCustomNTFileAuditFlags.GetSynchronize: TAuditFlagValue;
 Begin
    Result := GetAttributes(SYNCHRONIZE);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.SetSynchronize(Const Value: TAuditFlagValue);        }
{ Write method property CanSynchronize                                         }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAuditFlags.SetSynchronize(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(SYNCHRONIZE, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAuditFlags.Assign(Source: TPersistent);                         }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileAuditFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TCustomNTFileAuditFlags) Then
       With Source As TCustomNTFileAuditFlags Do Begin
          Self.CanDelete           := CanDelete;
          Self.CanReadControl      := CanReadControl;
          Self.CanWriteDac         := CanWriteDac;
          Self.CanWriteOwner       := CanWriteOwner;
          Self.CanReadAttributes   := CanReadAttributes;
          Self.CanWriteAttributes  := CanWriteAttributes;
          Self.CanSynchronize      := CanSynchronize;
       End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTDiskFilesAuditFlags                                        }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTDiskFilesAuditFlags.GetReadExtended: TAuditFlagValue;                     }
{ Read method property CanReadExtended                                         }
{------------------------------------------------------------------------------}
 Function TNTDiskFilesAuditFlags.GetReadExtended: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_READ_EA);
 End;

{------------------------------------------------------------------------------}
{ TNTDiskFilesAuditFlags.SetReadExtended(Const Value: TAuditFlagValue);        }
{ Write method property CanReadExtended                                        }
{------------------------------------------------------------------------------}
 Procedure TNTDiskFilesAuditFlags.SetReadExtended(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_READ_EA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDiskFilesAuditFlags.GetWriteExtended: TAuditFlagValue;                    }
{ Read method property CanWriteExtended                                        }
{------------------------------------------------------------------------------}
 Function TNTDiskFilesAuditFlags.GetWriteExtended: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_WRITE_EA);
 End;

{------------------------------------------------------------------------------}
{ TNTDiskFilesAuditFlags.SetWriteExtended(Const Value: TAuditFlagValue);       }
{ Write method property Notify                                                 }
{------------------------------------------------------------------------------}
 Procedure TNTDiskFilesAuditFlags.SetWriteExtended(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_WRITE_EA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDiskFilesAuditFlags.Assign(Source: TPersistent);                          }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TNTDiskFilesAuditFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TNTDiskFilesAuditFlags) Then
       With Source As TNTDiskFilesAuditFlags Do Begin
          Self.CanReadExtended := CanReadExtended;
          Self.CanWriteExtented:= CanWriteExtented;
       End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileAuditFlags                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileAuditFlags.GetReadData: TAuditFlagValue;                              }
{ Read method property CanReadData                                             }
{------------------------------------------------------------------------------}
 Function TNTFileAuditFlags.GetReadData: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_READ_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAuditFlags.SetReadData(Const Value: TAuditFlagValue);                 }
{ Write method property CanReadData                                            }
{------------------------------------------------------------------------------}
 Procedure TNTFileAuditFlags.SetReadData(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_READ_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAuditFlags.GetWriteData: TAuditFlagValue;                             }
{ Read method property CanWriteData                                            }
{------------------------------------------------------------------------------}
 Function TNTFileAuditFlags.GetWriteData: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_WRITE_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAuditFlags.SetWriteData(Const Value: TAuditFlagValue);                }
{ Write method property CanWriteData                                           }
{------------------------------------------------------------------------------}
 Procedure TNTFileAuditFlags.SetWriteData(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_WRITE_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAuditFlags.GetAppendData: TAuditFlagValue;                            }
{ Read method property CanAppendData                                           }
{------------------------------------------------------------------------------}
 Function TNTFileAuditFlags.GetAppendData: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_APPEND_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAuditFlags.SetAppendData(Const Value: TAuditFlagValue);               }
{ Write method property CanAppendData                                          }
{------------------------------------------------------------------------------}
 Procedure TNTFileAuditFlags.SetAppendData(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_APPEND_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAuditFlags.GetExecute: TAuditFlagValue;                               }
{ Read method property CanExecute                                              }
{------------------------------------------------------------------------------}
 Function TNTFileAuditFlags.GetExecute: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_EXECUTE);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAuditFlags.SetExecute(Const Value: TAuditFlagValue);                  }
{ Write method property CanExecute                                             }
{------------------------------------------------------------------------------}
 Procedure TNTFileAuditFlags.SetExecute(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_EXECUTE, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAuditFlags.Assign(Source: TPersistent);                               }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TNTFileAuditFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TNTFileAuditFlags) Then
       With Source As TNTFileAuditFlags Do Begin
          Self.CanReadData   := CanReadData;
          Self.CanWriteData  := CanWriteData;
          Self.CanAppendData := CanAppendData;
          Self.CanExecute    := CanExecute;
       End;
 End;

{------------------------------------------------------------------------------}
{ TNTFileAuditFlags.GetFlagsMode: TFlagsMode;                                  }
{ Read method property FlagsMode                                               }
{------------------------------------------------------------------------------}
 Function TNTFileAuditFlags.GetFlagsMode: TNTFileFlagsMode;

 Begin
    Result := ffmCustom;
    If ((CanDelete=faNone) And
        (CanReadControl=faSucceededFailed) And
        (CanWriteDac=faNone) And
        (CanWriteOwner=faNone) And
        (CanReadAttributes=faSucceededFailed) And
        (CanWriteAttributes=faNone) And
        (CanSynchronize=faSucceededFailed) And
        (CanReadExtended=faSucceededFailed) And
        (CanWriteExtented=faNone) And
        (CanReadData=faSucceededFailed) And
        (CanWriteData=faNone) And
        (CanAppendData=faNone) And
        (CanExecute=faNone)) Then
        Result := ffmRead
    Else If ((CanDelete=faNone) And
        (CanReadControl=faSucceededFailed) And
        (CanWriteDac=faNone) And
        (CanWriteOwner=faNone) And
        (CanReadAttributes=faSucceededFailed) And
        (CanWriteAttributes=faNone) And
        (CanSynchronize=faSucceededFailed) And
        (CanReadExtended=faSucceededFailed) And
        (CanWriteExtented=faNone) And
        (CanReadData=faSucceededFailed) And
        (CanWriteData=faNone) And
        (CanAppendData=faNone) And
        (CanExecute=faSucceededFailed)) Then
        Result := ffmReadExecute
     Else If ((CanDelete=faNone) And
        (CanReadControl=faNone) And
        (CanWriteDac=faNone) And
        (CanWriteOwner=faNone) And
        (CanReadAttributes=faNone) And
        (CanWriteAttributes=faSucceededFailed) And
        (CanSynchronize=faSucceededFailed) And
        (CanReadExtended=faNone) And
        (CanWriteExtented=faSucceededFailed) And
        (CanReadData=faNone) And
        (CanWriteData=faSucceededFailed) And
        (CanAppendData=faSucceededFailed) And
        (CanExecute=faNone)) Then
        Result := ffmWrite
     Else If ((CanDelete=faSucceededFailed) And
        (CanReadControl=faSucceededFailed) And
        (CanWriteDac=faNone) And
        (CanWriteOwner=faNone) And
        (CanReadAttributes=faSucceededFailed) And
        (CanWriteAttributes=faSucceededFailed) And
        (CanSynchronize=faSucceededFailed) And
        (CanReadExtended=faSucceededFailed) And
        (CanWriteExtented=faSucceededFailed) And
        (CanReadData=faSucceededFailed) And
        (CanWriteData=faSucceededFailed) And
        (CanAppendData=faSucceededFailed) And
        (CanExecute=faSucceededFailed)) Then
        Result := ffmModify
     Else If ((CanDelete=faSucceededFailed) And
        (CanReadControl=faSucceededFailed) And
        (CanWriteDac=faSucceededFailed) And
        (CanWriteOwner=faSucceededFailed) And
        (CanReadAttributes=faSucceededFailed) And
        (CanWriteAttributes=faSucceededFailed) And
        (CanSynchronize=faSucceededFailed) And
        (CanReadExtended=faSucceededFailed) And
        (CanWriteExtented=faSucceededFailed) And
        (CanReadData=faSucceededFailed) And
        (CanWriteData=faSucceededFailed) And
        (CanAppendData=faSucceededFailed) And
        (CanExecute=faSucceededFailed)) Then
        Result := ffmAll;
 End;

{------------------------------------------------------------------------------}
{ TNTFileAuditFlags.SetFlagsMode(Const Value: TFlagsMode);                     }
{ Write method property FlagsMode                                              }
{------------------------------------------------------------------------------}
 Procedure TNTFileAuditFlags.SetFlagsMode(Const Value: TNTFileFlagsMode);
 Begin
    If ACE.IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    Case Value Of
      ffmRead:
        Begin
           CanDelete          := faNone;
           CanReadControl     := faSucceededFailed;
           CanWriteDac        := faNone;
           CanWriteOwner      := faNone;
           CanReadAttributes  := faSucceededFailed;
           CanWriteAttributes := faNone;
           CanSynchronize     := faSucceededFailed;
           CanReadExtended    := faSucceededFailed;
           CanWriteExtented   := faNone;
           CanReadData        := faSucceededFailed;
           CanWriteData       := faNone;
           CanAppendData      := faNone;
           CanExecute         := faNone;
        End;
      ffmReadExecute:
        Begin
           CanDelete          := faNone;
           CanReadControl     := faSucceededFailed;
           CanWriteDac        := faNone;
           CanWriteOwner      := faNone;
           CanReadAttributes  := faSucceededFailed;
           CanWriteAttributes := faNone;
           CanSynchronize     := faSucceededFailed;
           CanReadExtended    := faSucceededFailed;
           CanWriteExtented   := faNone;
           CanReadData        := faSucceededFailed;
           CanWriteData       := faNone;
           CanAppendData      := faNone;
           CanExecute         := faSucceededFailed;
        End;
      ffmWrite:
        Begin
           CanDelete          := faNone;
           CanReadControl     := faNone;
           CanWriteDac        := faNone;
           CanWriteOwner      := faNone;
           CanReadAttributes  := faNone;
           CanWriteAttributes := faSucceededFailed;
           CanSynchronize     := faSucceededFailed;
           CanReadExtended    := faNone;
           CanWriteExtented   := faSucceededFailed;
           CanReadData        := faNone;
           CanWriteData       := faSucceededFailed;
           CanAppendData      := faSucceededFailed;
           CanExecute         := faNone;
        End;
      ffmModify:
        Begin
           CanDelete          := faSucceededFailed;
           CanReadControl     := faSucceededFailed;
           CanWriteDac        := faNone;
           CanWriteOwner      := faNone;
           CanReadAttributes  := faSucceededFailed;
           CanWriteAttributes := faSucceededFailed;
           CanSynchronize     := faSucceededFailed;
           CanReadExtended    := faSucceededFailed;
           CanWriteExtented   := faSucceededFailed;
           CanReadData        := faSucceededFailed;
           CanWriteData       := faSucceededFailed;
           CanAppendData      := faSucceededFailed;
           CanExecute         := faSucceededFailed;
        End;
      ffmAll:
        Begin
           CanDelete          := faSucceededFailed;
           CanReadControl     := faSucceededFailed;
           CanWriteDac        := faSucceededFailed;
           CanWriteOwner      := faSucceededFailed;
           CanReadAttributes  := faSucceededFailed;
           CanWriteAttributes := faSucceededFailed;
           CanSynchronize     := faSucceededFailed;
           CanReadExtended    := faSucceededFailed;
           CanWriteExtented   := faSucceededFailed;
           CanReadData        := faSucceededFailed;
           CanWriteData       := faSucceededFailed;
           CanAppendData      := faSucceededFailed;
           CanExecute         := faSucceededFailed;
        End;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTDirAuditFlags                                              }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.GetListDir: TAuditFlagValue;                                }
{ Read method property CanListDir                                              }
{------------------------------------------------------------------------------}
 Function TNTDirAuditFlags.GetListDir: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_LIST_DIRECTORY);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.SetListDir(Const Value: TAuditFlagValue);                   }
{ Write method property CanListDir                                             }
{------------------------------------------------------------------------------}
 Procedure TNTDirAuditFlags.SetListDir(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_LIST_DIRECTORY, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.GetAddFile: TAuditFlagValue;                                }
{ Read method property CanAddFile                                              }
{------------------------------------------------------------------------------}
 Function TNTDirAuditFlags.GetAddFile: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_ADD_FILE);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.SetAddFile(Const Value: TAuditFlagValue);                   }
{ Write method property CanAddFile                                             }
{------------------------------------------------------------------------------}
 Procedure TNTDirAuditFlags.SetAddFile(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_ADD_FILE, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.GetAddSubDir: TAuditFlagValue;                              }
{ Read method property CanAddSubDir                                            }
{------------------------------------------------------------------------------}
 Function TNTDirAuditFlags.GetAddSubDir: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_ADD_SUBDIRECTORY);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.SetAddSubDir(Const Value: TAuditFlagValue);                 }
{ Write method property CanAddSubDir                                           }
{------------------------------------------------------------------------------}
 Procedure TNTDirAuditFlags.SetAddSubDir(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_ADD_SUBDIRECTORY, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.GetTraverse: TAuditFlagValue;                               }
{ Read method property CanTraverse                                             }
{------------------------------------------------------------------------------}
 Function TNTDirAuditFlags.GetTraverse: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_TRAVERSE);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.SetTraverse(Const Value: TAuditFlagValue);                  }
{ Write method property CanTraverse                                            }
{------------------------------------------------------------------------------}
 Procedure TNTDirAuditFlags.SetTraverse(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_TRAVERSE, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.GetDeleteChild: TAuditFlagValue;                            }
{ Read method property CanDeleteChild                                          }
{------------------------------------------------------------------------------}
 Function TNTDirAuditFlags.GetDeleteChild: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_DELETE_CHILD);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.SetDeleteChild(Const Value: TAuditFlagValue);               }
{ Write method property CanDeleteChild                                         }
{------------------------------------------------------------------------------}
 Procedure TNTDirAuditFlags.SetDeleteChild(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_DELETE_CHILD, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.Assign(Source: TPersistent);                                }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TNTDirAuditFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TNTDirAuditFlags) Then
       With Source As TNTDirAuditFlags Do Begin
          Self.CanListDir     := CanListDir;
          Self.CanAddFile     := CanAddFile;
          Self.CanAddSubDir   := CanAddSubDir;
          Self.CanTraverse    := CanTraverse;
          Self.CanDeleteChild := CanDeleteChild;
       End;
 End;

{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.GetFlagsMode: TNTDirFlagsMode;                              }
{ Read method property FlagsMode                                               }
{------------------------------------------------------------------------------}
 Function TNTDirAuditFlags.GetFlagsMode: TNTDirFlagsMode;
 Begin
    Result := dfmCustom;
    If ((CanDelete=faNone) And
        (CanReadControl=faSucceededFailed) And
        (CanWriteDac=faNone) And
        (CanWriteOwner=faNone) And
        (CanReadAttributes=faSucceededFailed) And
        (CanWriteAttributes=faNone) And
        (CanSynchronize=faSucceededFailed) And
        (CanReadExtended=faSucceededFailed) And
        (CanWriteExtented=faNone) And
        (CanListDir=faSucceededFailed) And
        (CanAddFile=faNone) And
        (CanAddSubDir=faNone) And
        (CanTraverse=faNone) And
        (CanDeleteChild=faNone)) Then
        Result := dfmRead
    Else If ((CanDelete=faNone) And
        (CanReadControl=faSucceededFailed) And
        (CanWriteDac=faNone) And
        (CanWriteOwner=faNone) And
        (CanReadAttributes=faSucceededFailed) And
        (CanWriteAttributes=faNone) And
        (CanSynchronize=faSucceededFailed) And
        (CanReadExtended=faSucceededFailed) And
        (CanWriteExtented=faNone) And
        (CanListDir=faSucceededFailed) And
        (CanAddFile=faNone) And
        (CanAddSubDir=faNone) And
        (CanTraverse=faSucceededFailed) And
        (CanDeleteChild=faNone)) Then
        Result := dfmReadExecute
     Else If ((CanDelete=faNone) And
        (CanReadControl=faNone) And
        (CanWriteDac=faNone) And
        (CanWriteOwner=faNone) And
        (CanReadAttributes=faNone) And
        (CanWriteAttributes=faSucceededFailed) And
        (CanSynchronize=faSucceededFailed) And
        (CanReadExtended=faNone) And
        (CanWriteExtented=faSucceededFailed) And
        (CanListDir=faNone) And
        (CanAddFile=faSucceededFailed) And
        (CanAddSubDir=faSucceededFailed) And
        (CanTraverse=faNone) And
        (CanDeleteChild=faNone)) Then
        Result := dfmWrite
     Else If ((CanDelete=faSucceededFailed) And
        (CanReadControl=faSucceededFailed) And
        (CanWriteDac=faNone) And
        (CanWriteOwner=faNone) And
        (CanReadAttributes=faSucceededFailed) And
        (CanWriteAttributes=faSucceededFailed) And
        (CanSynchronize=faSucceededFailed) And
        (CanReadExtended=faSucceededFailed) And
        (CanWriteExtented=faSucceededFailed) And
        (CanListDir=faSucceededFailed) And
        (CanAddFile=faSucceededFailed) And
        (CanAddSubDir=faSucceededFailed) And
        (CanTraverse=faSucceededFailed) And
        (CanDeleteChild=faNone)) Then
        Result := dfmModify
     Else If ((CanDelete=faSucceededFailed) And
        (CanReadControl=faSucceededFailed) And
        (CanWriteDac=faSucceededFailed) And
        (CanWriteOwner=faSucceededFailed) And
        (CanReadAttributes=faSucceededFailed) And
        (CanWriteAttributes=faSucceededFailed) And
        (CanSynchronize=faSucceededFailed) And
        (CanReadExtended=faSucceededFailed) And
        (CanWriteExtented=faSucceededFailed) And
        (CanListDir=faSucceededFailed) And
        (CanAddFile=faSucceededFailed) And
        (CanAddSubDir=faSucceededFailed) And
        (CanTraverse=faSucceededFailed) And
        (CanDeleteChild=faSucceededFailed)) Then
        Result := dfmAll;
 End;

{------------------------------------------------------------------------------}
{ TNTDirAuditFlags.SetFlagsMode(Const Value: TNTDirFlagsMode);                 }
{ Write method property FlagsMode                                              }
{------------------------------------------------------------------------------}
 Procedure TNTDirAuditFlags.SetFlagsMode(Const Value: TNTDirFlagsMode);
 Begin
    If ACE.IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    Case Value Of
      dfmRead:
        Begin
           CanDelete          := faNone;
           CanReadControl     := faSucceededFailed;
           CanWriteDac        := faNone;
           CanWriteOwner      := faNone;
           CanReadAttributes  := faSucceededFailed;
           CanWriteAttributes := faNone;
           CanSynchronize     := faSucceededFailed;
           CanReadExtended    := faSucceededFailed;
           CanWriteExtented   := faNone;
           CanListDir         := faSucceededFailed;
           CanAddFile         := faNone;
           CanAddSubDir       := faNone;
           CanTraverse        := faNone;
           CanDeleteChild     := faNone;
        End;
      dfmReadExecute:
        Begin
           CanDelete          := faNone;
           CanReadControl     := faSucceededFailed;
           CanWriteDac        := faNone;
           CanWriteOwner      := faNone;
           CanReadAttributes  := faSucceededFailed;
           CanWriteAttributes := faNone;
           CanSynchronize     := faSucceededFailed;
           CanReadExtended    := faSucceededFailed;
           CanWriteExtented   := faNone;
           CanListDir         := faSucceededFailed;
           CanAddFile         := faNone;
           CanAddSubDir       := faNone;
           CanTraverse        := faSucceededFailed;
           CanDeleteChild     := faNone;
        End;
      dfmWrite:
        Begin
           CanDelete          := faNone;
           CanReadControl     := faNone;
           CanWriteDac        := faNone;
           CanWriteOwner      := faNone;
           CanReadAttributes  := faNone;
           CanWriteAttributes := faSucceededFailed;
           CanSynchronize     := faSucceededFailed;
           CanReadExtended    := faNone;
           CanWriteExtented   := faSucceededFailed;
           CanListDir         := faNone;
           CanAddFile         := faSucceededFailed;
           CanAddSubDir       := faSucceededFailed;
           CanTraverse        := faNone;
           CanDeleteChild     := faNone;
        End;
      dfmModify:
        Begin
           CanDelete          := faSucceededFailed;
           CanReadControl     := faSucceededFailed;
           CanWriteDac        := faNone;
           CanWriteOwner      := faNone;
           CanReadAttributes  := faSucceededFailed;
           CanWriteAttributes := faSucceededFailed;
           CanSynchronize     := faSucceededFailed;
           CanReadExtended    := faSucceededFailed;
           CanWriteExtented   := faSucceededFailed;
           CanListDir         := faSucceededFailed;
           CanAddFile         := faSucceededFailed;
           CanAddSubDir       := faSucceededFailed;
           CanTraverse        := faSucceededFailed;
           CanDeleteChild     := faSucceededFailed;
        End;
      dfmAll:
        Begin
           CanDelete          := faSucceededFailed;
           CanReadControl     := faSucceededFailed;
           CanWriteDac        := faSucceededFailed;
           CanWriteOwner      := faSucceededFailed;
           CanReadAttributes  := faSucceededFailed;
           CanWriteAttributes := faSucceededFailed;
           CanSynchronize     := faSucceededFailed;
           CanReadExtended    := faSucceededFailed;
           CanWriteExtented   := faSucceededFailed;
           CanListDir         := faSucceededFailed;
           CanAddFile         := faSucceededFailed;
           CanAddSubDir       := faSucceededFailed;
           CanTraverse        := faSucceededFailed;
           CanDeleteChild     := faSucceededFailed;
        End;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTPipeAuditFlags                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTPipeAuditFlags.GetReadData: TAuditFlagValue;                              }
{ Read method property CanReadData                                             }
{------------------------------------------------------------------------------}
 Function TNTPipeAuditFlags.GetReadData: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_READ_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAuditFlags.SetReadData(Const Value: TAuditFlagValue);                 }
{ Write method property CanReadData                                            }
{------------------------------------------------------------------------------}
 Procedure TNTPipeAuditFlags.SetReadData(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_READ_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAuditFlags.GetWriteData: TAuditFlagValue;                             }
{ Read method property CanWriteData                                            }
{------------------------------------------------------------------------------}
 Function TNTPipeAuditFlags.GetWriteData: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_WRITE_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAuditFlags.SetWriteData(Const Value: TAuditFlagValue);                }
{ Write method property CanWriteData                                           }
{------------------------------------------------------------------------------}
 Procedure TNTPipeAuditFlags.SetWriteData(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_WRITE_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAuditFlags.GetCreateInstance: TAuditFlagValue;                        }
{ Read method property CanCreateInstance                                       }
{------------------------------------------------------------------------------}
 Function TNTPipeAuditFlags.GetCreateInstance: TAuditFlagValue;
 Begin
    Result := GetAttributes(FILE_CREATE_PIPE_INSTANCE);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAuditFlags.SetCreateInstance(Const Value: TAuditFlagValue);           }
{ Write method property CanCreateInstance                                      }
{------------------------------------------------------------------------------}
 Procedure TNTPipeAuditFlags.SetCreateInstance(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(FILE_CREATE_PIPE_INSTANCE, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeAuditFlags.Assign(Source: TPersistent);                               }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TNTPipeAuditFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TNTPipeAuditFlags) Then
       With Source As TNTPipeAuditFlags Do Begin
          Self.CanReadData       := CanReadData;
          Self.CanWriteData      := CanWriteData;
          Self.CanCreateInstance := CanCreateInstance;
       End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomNTFileCheckFlags                                       }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.Create(...);                                         }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCustomNTFileCheckFlags.Create(Filer: TCustomNTFileFlagsFiler; Const RO: Boolean);
 Begin
    Inherited Create;
    FReadOnly := RO;
    FFiler := Filer;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.Destroy;                                             }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TCustomNTFileCheckFlags.Destroy;
 Begin
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.GetAttributes(...): TCheckFlagValue;                 }
{ Generic Read attibutes method                                                }
{------------------------------------------------------------------------------}
 Function TCustomNTFileCheckFlags.GetAttributes(Const lng: Cardinal): TCheckFlagValue;
 Begin
    Result := TCheckFlagValue(FFiler.GetAttributes(lng));
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.SetAttributes(...);                                  }
{ Generic Write attibutes method                                               }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileCheckFlags.SetAttributes(Const lng: Cardinal; Const Value: TCheckFlagValue);
 Begin
    FFiler.SetAttributes(lng, Cardinal(Value));
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.GetDelete: TCheckFlagValue;                          }
{ Read method property CanDelete                                               }
{------------------------------------------------------------------------------}
 Function TCustomNTFileCheckFlags.GetDelete: TCheckFlagValue;
 Begin
    Result := GetAttributes(_DELETE);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.SetDelete(Const Value: TCheckFlagValue);             }
{ Write method property CanDelete                                              }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileCheckFlags.SetDelete(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(_DELETE, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.GetReadControl: TCheckFlagValue;                     }
{ Read method property ReadControl                                             }
{------------------------------------------------------------------------------}
 Function TCustomNTFileCheckFlags.GetReadControl: TCheckFlagValue;
 Begin
    Result := GetAttributes(READ_CONTROL);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.SetReadControl(Const Value: TCheckFlagValue);        }
{ Write method property ReadControl                                            }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileCheckFlags.SetReadControl(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(READ_CONTROL, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.GetWriteDac: TCheckFlagValue;                        }
{ Read method property WriteDac                                                }
{------------------------------------------------------------------------------}
 Function TCustomNTFileCheckFlags.GetWriteDac: TCheckFlagValue;
 Begin
    Result := GetAttributes(WRITE_DAC);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.SetWriteDac(Const Value: TCheckFlagValue);           }
{ Write method property Write Dac                                              }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileCheckFlags.SetWriteDac(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(WRITE_DAC, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.GetWriteOwner: TCheckFlagValue;                      }
{ Read method property WriteOwner                                              }
{------------------------------------------------------------------------------}
 Function TCustomNTFileCheckFlags.GetWriteOwner: TCheckFlagValue;
 Begin
    Result := GetAttributes(WRITE_OWNER);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.SetWriteOwner(Const Value: TCheckFlagValue);         }
{ Write method property WriteOwner                                             }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileCheckFlags.SetWriteOwner(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(WRITE_OWNER, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.GetReadAttributes: TCheckFlagValue;                  }
{ Read method property CanReadAttributes                                       }
{------------------------------------------------------------------------------}
 Function TCustomNTFileCheckFlags.GetReadAttributes: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_READ_ATTRIBUTES);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.SetReadAttributes(Const Value: TCheckFlagValue);     }
{ Write method property CanReadAttributes                                      }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileCheckFlags.SetReadAttributes(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_READ_ATTRIBUTES, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.GetWriteAttributes: TCheckFlagValue;                 }
{ Read method property CanWriteAttributes                                      }
{------------------------------------------------------------------------------}
 Function TCustomNTFileCheckFlags.GetWriteAttributes: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_WRITE_ATTRIBUTES);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.SetWriteAttributes(Const Value: TCheckFlagValue);    }
{ Write method property CanWriteAttributes                                     }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileCheckFlags.SetWriteAttributes(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_WRITE_ATTRIBUTES, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.GetSynchronize: TCheckFlagValue;                     }
{ Read method property CanSynchronize                                          }
{------------------------------------------------------------------------------}
 Function TCustomNTFileCheckFlags.GetSynchronize: TCheckFlagValue;
 Begin
    Result := GetAttributes(SYNCHRONIZE);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.SetSynchronize(Const Value: TCheckFlagValue);        }
{ Write method property CanSynchronize                                         }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileCheckFlags.SetSynchronize(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(SYNCHRONIZE, Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileCheckFlags.Assign(Source: TPersistent);                         }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TCustomNTFileCheckFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TCustomNTFileCheckFlags) Then
       With Source As TCustomNTFileCheckFlags Do Begin
          Self.CanDelete           := CanDelete;
          Self.CanReadControl      := CanReadControl;
          Self.CanWriteDac         := CanWriteDac;
          Self.CanWriteOwner       := CanWriteOwner;
          Self.CanReadAttributes   := CanReadAttributes;
          Self.CanWriteAttributes  := CanWriteAttributes;
          Self.CanSynchronize      := CanSynchronize;
       End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTDiskFilesCheckFlags                                        }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTDiskFilesCheckFlags.GetReadExtended: TCheckFlagValue;                     }
{ Read method property CanReadExtended                                         }
{------------------------------------------------------------------------------}
 Function TNTDiskFilesCheckFlags.GetReadExtended: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_READ_EA);
 End;

{------------------------------------------------------------------------------}
{ TNTDiskFilesCheckFlags.SetReadExtended(Const Value: TCheckFlagValue);        }
{ Write method property CanReadExtended                                        }
{------------------------------------------------------------------------------}
 Procedure TNTDiskFilesCheckFlags.SetReadExtended(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_READ_EA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDiskFilesCheckFlags.GetWriteExtended: TCheckFlagValue;                    }
{ Read method property CanWriteExtended                                        }
{------------------------------------------------------------------------------}
 Function TNTDiskFilesCheckFlags.GetWriteExtended: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_WRITE_EA);
 End;

{------------------------------------------------------------------------------}
{ TNTDiskFilesCheckFlags.SetWriteExtended(Const Value: TCheckFlagValue);       }
{ Write method property Notify                                                 }
{------------------------------------------------------------------------------}
 Procedure TNTDiskFilesCheckFlags.SetWriteExtended(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_WRITE_EA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDiskFilesCheckFlags.Assign(Source: TPersistent);                          }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TNTDiskFilesCheckFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TNTDiskFilesCheckFlags) Then
       With Source As TNTDiskFilesCheckFlags Do Begin
          Self.CanReadExtended := CanReadExtended;
          Self.CanWriteExtented:= CanWriteExtented;
       End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileCheckFlags                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileCheckFlags.GetReadData: TCheckFlagValue;                              }
{ Read method property CanReadData                                             }
{------------------------------------------------------------------------------}
 Function TNTFileCheckFlags.GetReadData: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_READ_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTFileCheckFlags.SetReadData(Const Value: TCheckFlagValue);                 }
{ Write method property CanReadData                                            }
{------------------------------------------------------------------------------}
 Procedure TNTFileCheckFlags.SetReadData(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_READ_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTFileCheckFlags.GetWriteData: TCheckFlagValue;                             }
{ Read method property CanWriteData                                            }
{------------------------------------------------------------------------------}
 Function TNTFileCheckFlags.GetWriteData: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_WRITE_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTFileCheckFlags.SetWriteData(Const Value: TCheckFlagValue);                }
{ Write method property CanWriteData                                           }
{------------------------------------------------------------------------------}
 Procedure TNTFileCheckFlags.SetWriteData(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_WRITE_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTFileCheckFlags.GetAppendData: TCheckFlagValue;                            }
{ Read method property CanAppendData                                           }
{------------------------------------------------------------------------------}
 Function TNTFileCheckFlags.GetAppendData: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_APPEND_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTFileCheckFlags.SetAppendData(Const Value: TCheckFlagValue);               }
{ Write method property CanAppendData                                          }
{------------------------------------------------------------------------------}
 Procedure TNTFileCheckFlags.SetAppendData(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_APPEND_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTFileCheckFlags.GetExecute: TCheckFlagValue;                               }
{ Read method property CanExecute                                              }
{------------------------------------------------------------------------------}
 Function TNTFileCheckFlags.GetExecute: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_EXECUTE);
 End;

{------------------------------------------------------------------------------}
{ TNTFileCheckFlags.SetExecute(Const Value: TCheckFlagValue);                  }
{ Write method property CanExecute                                             }
{------------------------------------------------------------------------------}
 Procedure TNTFileCheckFlags.SetExecute(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_EXECUTE, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTFileCheckFlags.Assign(Source: TPersistent);                               }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TNTFileCheckFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TNTFileCheckFlags) Then
       With Source As TNTFileCheckFlags Do Begin
          Self.CanReadData   := CanReadData;
          Self.CanWriteData  := CanWriteData;
          Self.CanAppendData := CanAppendData;
          Self.CanExecute    := CanExecute;
       End;
 End;

{------------------------------------------------------------------------------}
{ TNTFileCheckFlags.GetFlagsMode: TFlagsMode;                                  }
{ Read method property FlagsMode                                               }
{------------------------------------------------------------------------------}
 Function TNTFileCheckFlags.GetFlagsMode: TNTFileFlagsMode;
 Begin
    Result := ffmCustom;
    If ((CanDelete=fcDenied) And
        (CanReadControl=fcAllowed) And
        (CanWriteDac=fcDenied) And
        (CanWriteOwner=fcDenied) And
        (CanReadAttributes=fcAllowed) And
        (CanWriteAttributes=fcDenied) And
        (CanSynchronize=fcAllowed) And
        (CanReadExtended=fcAllowed) And
        (CanWriteExtented=fcDenied) And
        (CanReadData=fcAllowed) And
        (CanWriteData=fcDenied) And
        (CanAppendData=fcDenied) And
        (CanExecute=fcDenied)) Then
        Result := ffmRead
    Else If ((CanDelete=fcDenied) And
        (CanReadControl=fcAllowed) And
        (CanWriteDac=fcDenied) And
        (CanWriteOwner=fcDenied) And
        (CanReadAttributes=fcAllowed) And
        (CanWriteAttributes=fcDenied) And
        (CanSynchronize=fcAllowed) And
        (CanReadExtended=fcAllowed) And
        (CanWriteExtented=fcDenied) And
        (CanReadData=fcAllowed) And
        (CanWriteData=fcDenied) And
        (CanAppendData=fcDenied) And
        (CanExecute=fcAllowed)) Then
        Result := ffmReadExecute
     Else If ((CanDelete=fcDenied) And
        (CanReadControl=fcDenied) And
        (CanWriteDac=fcDenied) And
        (CanWriteOwner=fcDenied) And
        (CanReadAttributes=fcDenied) And
        (CanWriteAttributes=fcAllowed) And
        (CanSynchronize=fcAllowed) And
        (CanReadExtended=fcDenied) And
        (CanWriteExtented=fcAllowed) And
        (CanReadData=fcDenied) And
        (CanWriteData=fcAllowed) And
        (CanAppendData=fcAllowed) And
        (CanExecute=fcDenied)) Then
        Result := ffmWrite
     Else If ((CanDelete=fcAllowed) And
        (CanReadControl=fcAllowed) And
        (CanWriteDac=fcDenied) And
        (CanWriteOwner=fcDenied) And
        (CanReadAttributes=fcAllowed) And
        (CanWriteAttributes=fcAllowed) And
        (CanSynchronize=fcAllowed) And
        (CanReadExtended=fcAllowed) And
        (CanWriteExtented=fcAllowed) And
        (CanReadData=fcAllowed) And
        (CanWriteData=fcAllowed) And
        (CanAppendData=fcAllowed) And
        (CanExecute=fcAllowed)) Then
        Result := ffmModify
     Else If ((CanDelete=fcAllowed) And
        (CanReadControl=fcAllowed) And
        (CanWriteDac=fcAllowed) And
        (CanWriteOwner=fcAllowed) And
        (CanReadAttributes=fcAllowed) And
        (CanWriteAttributes=fcAllowed) And
        (CanSynchronize=fcAllowed) And
        (CanReadExtended=fcAllowed) And
        (CanWriteExtented=fcAllowed) And
        (CanReadData=fcAllowed) And
        (CanWriteData=fcAllowed) And
        (CanAppendData=fcAllowed) And
        (CanExecute=fcAllowed)) Then
        Result := ffmAll;
 End;

{------------------------------------------------------------------------------}
{ TNTFileCheckFlags.SetFlagsMode(Const Value: TFlagsMode);                     }
{ Write method property FlagsMode                                              }
{------------------------------------------------------------------------------}
 Procedure TNTFileCheckFlags.SetFlagsMode(Const Value: TNTFileFlagsMode);
 Begin
    Case Value Of
      ffmRead:
        Begin
           CanDelete          := fcDenied;
           CanReadControl     := fcAllowed;
           CanWriteDac        := fcDenied;
           CanWriteOwner      := fcDenied;
           CanReadAttributes  := fcAllowed;
           CanWriteAttributes := fcDenied;
           CanSynchronize     := fcAllowed;
           CanReadExtended    := fcAllowed;
           CanWriteExtented   := fcDenied;
           CanReadData        := fcAllowed;
           CanWriteData       := fcDenied;
           CanAppendData      := fcDenied;
           CanExecute         := fcDenied;
        End;
      ffmReadExecute:
        Begin
           CanDelete          := fcDenied;
           CanReadControl     := fcAllowed;
           CanWriteDac        := fcDenied;
           CanWriteOwner      := fcDenied;
           CanReadAttributes  := fcAllowed;
           CanWriteAttributes := fcDenied;
           CanSynchronize     := fcAllowed;
           CanReadExtended    := fcAllowed;
           CanWriteExtented   := fcDenied;
           CanReadData        := fcAllowed;
           CanWriteData       := fcDenied;
           CanAppendData      := fcDenied;
           CanExecute         := fcAllowed;
        End;
      ffmWrite:
        Begin
           CanDelete          := fcDenied;
           CanReadControl     := fcDenied;
           CanWriteDac        := fcDenied;
           CanWriteOwner      := fcDenied;
           CanReadAttributes  := fcDenied;
           CanWriteAttributes := fcAllowed;
           CanSynchronize     := fcAllowed;
           CanReadExtended    := fcDenied;
           CanWriteExtented   := fcAllowed;
           CanReadData        := fcDenied;
           CanWriteData       := fcAllowed;
           CanAppendData      := fcAllowed;
           CanExecute         := fcDenied;
        End;
      ffmModify:
        Begin
           CanDelete          := fcAllowed;
           CanReadControl     := fcAllowed;
           CanWriteDac        := fcDenied;
           CanWriteOwner      := fcDenied;
           CanReadAttributes  := fcAllowed;
           CanWriteAttributes := fcAllowed;
           CanSynchronize     := fcAllowed;
           CanReadExtended    := fcAllowed;
           CanWriteExtented   := fcAllowed;
           CanReadData        := fcAllowed;
           CanWriteData       := fcAllowed;
           CanAppendData      := fcAllowed;
           CanExecute         := fcAllowed;
        End;
      ffmAll:
        Begin
           CanDelete          := fcAllowed;
           CanReadControl     := fcAllowed;
           CanWriteDac        := fcAllowed;
           CanWriteOwner      := fcAllowed;
           CanReadAttributes  := fcAllowed;
           CanWriteAttributes := fcAllowed;
           CanSynchronize     := fcAllowed;
           CanReadExtended    := fcAllowed;
           CanWriteExtented   := fcAllowed;
           CanReadData        := fcAllowed;
           CanWriteData       := fcAllowed;
           CanAppendData      := fcAllowed;
           CanExecute         := fcAllowed;
        End;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTDirCheckFlags                                              }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.GetListDir: TCheckFlagValue;                                }
{ Read method property CanListDir                                              }
{------------------------------------------------------------------------------}
 Function TNTDirCheckFlags.GetListDir: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_LIST_DIRECTORY);
 End;

{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.SetListDir(Const Value: TCheckFlagValue);                   }
{ Write method property CanListDir                                             }
{------------------------------------------------------------------------------}
 Procedure TNTDirCheckFlags.SetListDir(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_LIST_DIRECTORY, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.GetAddFile: TCheckFlagValue;                                }
{ Read method property CanAddFile                                              }
{------------------------------------------------------------------------------}
 Function TNTDirCheckFlags.GetAddFile: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_ADD_FILE);
 End;

{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.SetAddFile(Const Value: TCheckFlagValue);                   }
{ Write method property CanAddFile                                             }
{------------------------------------------------------------------------------}
 Procedure TNTDirCheckFlags.SetAddFile(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_ADD_FILE, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.GetAddSubDir: TCheckFlagValue;                              }
{ Read method property CanAddSubDir                                            }
{------------------------------------------------------------------------------}
 Function TNTDirCheckFlags.GetAddSubDir: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_ADD_SUBDIRECTORY);
 End;

{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.SetAddSubDir(Const Value: TCheckFlagValue);                 }
{ Write method property CanAddSubDir                                           }
{------------------------------------------------------------------------------}
 Procedure TNTDirCheckFlags.SetAddSubDir(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_ADD_SUBDIRECTORY, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.GetTraverse: TCheckFlagValue;                               }
{ Read method property CanTraverse                                             }
{------------------------------------------------------------------------------}
 Function TNTDirCheckFlags.GetTraverse: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_TRAVERSE);
 End;

{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.SetTraverse(Const Value: TCheckFlagValue);                  }
{ Write method property CanTraverse                                            }
{------------------------------------------------------------------------------}
 Procedure TNTDirCheckFlags.SetTraverse(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_TRAVERSE, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.GetDeleteChild: TCheckFlagValue;                            }
{ Read method property CanDeleteChild                                          }
{------------------------------------------------------------------------------}
 Function TNTDirCheckFlags.GetDeleteChild: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_DELETE_CHILD);
 End;

{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.SetDeleteChild(Const Value: TCheckFlagValue);               }
{ Write method property CanDeleteChild                                         }
{------------------------------------------------------------------------------}
 Procedure TNTDirCheckFlags.SetDeleteChild(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_DELETE_CHILD, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.Assign(Source: TPersistent);                                }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TNTDirCheckFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TNTDirCheckFlags) Then
       With Source As TNTDirCheckFlags Do Begin
          Self.CanListDir     := CanListDir;
          Self.CanAddFile     := CanAddFile;
          Self.CanAddSubDir   := CanAddSubDir;
          Self.CanTraverse    := CanTraverse;
          Self.CanDeleteChild := CanDeleteChild;
       End;
 End;

{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.GetFlagsMode: TNTDirFlagsMode;                              }
{ Read method property FlagsMode                                               }
{------------------------------------------------------------------------------}
 Function TNTDirCheckFlags.GetFlagsMode: TNTDirFlagsMode;
 Begin
    Result := dfmCustom;
    If ((CanDelete=fcDenied) And
        (CanReadControl=fcAllowed) And
        (CanWriteDac=fcDenied) And
        (CanWriteOwner=fcDenied) And
        (CanReadAttributes=fcAllowed) And
        (CanWriteAttributes=fcDenied) And
        (CanSynchronize=fcAllowed) And
        (CanReadExtended=fcAllowed) And
        (CanWriteExtented=fcDenied) And
        (CanListDir=fcAllowed) And
        (CanAddFile=fcDenied) And
        (CanAddSubDir=fcDenied) And
        (CanTraverse=fcDenied) And
        (CanDeleteChild=fcDenied)) Then
        Result := dfmRead
    Else If ((CanDelete=fcDenied) And
        (CanReadControl=fcAllowed) And
        (CanWriteDac=fcDenied) And
        (CanWriteOwner=fcDenied) And
        (CanReadAttributes=fcAllowed) And
        (CanWriteAttributes=fcDenied) And
        (CanSynchronize=fcAllowed) And
        (CanReadExtended=fcAllowed) And
        (CanWriteExtented=fcDenied) And
        (CanListDir=fcAllowed) And
        (CanAddFile=fcDenied) And
        (CanAddSubDir=fcDenied) And
        (CanTraverse=fcAllowed) And
        (CanDeleteChild=fcDenied)) Then
        Result := dfmReadExecute
     Else If ((CanDelete=fcDenied) And
        (CanReadControl=fcDenied) And
        (CanWriteDac=fcDenied) And
        (CanWriteOwner=fcDenied) And
        (CanReadAttributes=fcDenied) And
        (CanWriteAttributes=fcAllowed) And
        (CanSynchronize=fcAllowed) And
        (CanReadExtended=fcDenied) And
        (CanWriteExtented=fcAllowed) And
        (CanListDir=fcDenied) And
        (CanAddFile=fcAllowed) And
        (CanAddSubDir=fcAllowed) And
        (CanTraverse=fcDenied) And
        (CanDeleteChild=fcDenied)) Then
        Result := dfmWrite
     Else If ((CanDelete=fcAllowed) And
        (CanReadControl=fcAllowed) And
        (CanWriteDac=fcDenied) And
        (CanWriteOwner=fcDenied) And
        (CanReadAttributes=fcAllowed) And
        (CanWriteAttributes=fcAllowed) And
        (CanSynchronize=fcAllowed) And
        (CanReadExtended=fcAllowed) And
        (CanWriteExtented=fcAllowed) And
        (CanListDir=fcAllowed) And
        (CanAddFile=fcAllowed) And
        (CanAddSubDir=fcAllowed) And
        (CanTraverse=fcAllowed) And
        (CanDeleteChild=fcDenied)) Then
        Result := dfmModify
     Else If ((CanDelete=fcAllowed) And
        (CanReadControl=fcAllowed) And
        (CanWriteDac=fcAllowed) And
        (CanWriteOwner=fcAllowed) And
        (CanReadAttributes=fcAllowed) And
        (CanWriteAttributes=fcAllowed) And
        (CanSynchronize=fcAllowed) And
        (CanReadExtended=fcAllowed) And
        (CanWriteExtented=fcAllowed) And
        (CanListDir=fcAllowed) And
        (CanAddFile=fcAllowed) And
        (CanAddSubDir=fcAllowed) And
        (CanTraverse=fcAllowed) And
        (CanDeleteChild=fcAllowed)) Then
        Result := dfmAll;
 End;

{------------------------------------------------------------------------------}
{ TNTDirCheckFlags.SetFlagsMode(Const Value: TNTDirFlagsMode);                 }
{ Write method property FlagsMode                                              }
{------------------------------------------------------------------------------}
 Procedure TNTDirCheckFlags.SetFlagsMode(Const Value: TNTDirFlagsMode);
 Begin
    Case Value Of
      dfmRead:
        Begin
           CanDelete          := fcDenied;
           CanReadControl     := fcAllowed;
           CanWriteDac        := fcDenied;
           CanWriteOwner      := fcDenied;
           CanReadAttributes  := fcAllowed;
           CanWriteAttributes := fcDenied;
           CanSynchronize     := fcAllowed;
           CanReadExtended    := fcAllowed;
           CanWriteExtented   := fcDenied;
           CanListDir         := fcAllowed;
           CanAddFile         := fcDenied;
           CanAddSubDir       := fcDenied;
           CanTraverse        := fcDenied;
           CanDeleteChild     := fcDenied;
        End;
      dfmReadExecute:
        Begin
           CanDelete          := fcDenied;
           CanReadControl     := fcAllowed;
           CanWriteDac        := fcDenied;
           CanWriteOwner      := fcDenied;
           CanReadAttributes  := fcAllowed;
           CanWriteAttributes := fcDenied;
           CanSynchronize     := fcAllowed;
           CanReadExtended    := fcAllowed;
           CanWriteExtented   := fcDenied;
           CanListDir         := fcAllowed;
           CanAddFile         := fcDenied;
           CanAddSubDir       := fcDenied;
           CanTraverse        := fcAllowed;
           CanDeleteChild     := fcDenied;
        End;
      dfmWrite:
        Begin
           CanDelete          := fcDenied;
           CanReadControl     := fcDenied;
           CanWriteDac        := fcDenied;
           CanWriteOwner      := fcDenied;
           CanReadAttributes  := fcDenied;
           CanWriteAttributes := fcAllowed;
           CanSynchronize     := fcAllowed;
           CanReadExtended    := fcDenied;
           CanWriteExtented   := fcAllowed;
           CanListDir         := fcDenied;
           CanAddFile         := fcAllowed;
           CanAddSubDir       := fcAllowed;
           CanTraverse        := fcDenied;
           CanDeleteChild     := fcDenied;
        End;
      dfmModify:
        Begin
           CanDelete          := fcAllowed;
           CanReadControl     := fcAllowed;
           CanWriteDac        := fcDenied;
           CanWriteOwner      := fcDenied;
           CanReadAttributes  := fcAllowed;
           CanWriteAttributes := fcAllowed;
           CanSynchronize     := fcAllowed;
           CanReadExtended    := fcAllowed;
           CanWriteExtented   := fcAllowed;
           CanListDir         := fcAllowed;
           CanAddFile         := fcAllowed;
           CanAddSubDir       := fcAllowed;
           CanTraverse        := fcAllowed;
           CanDeleteChild     := fcAllowed;
        End;
      dfmAll:
        Begin
           CanDelete          := fcAllowed;
           CanReadControl     := fcAllowed;
           CanWriteDac        := fcAllowed;
           CanWriteOwner      := fcAllowed;
           CanReadAttributes  := fcAllowed;
           CanWriteAttributes := fcAllowed;
           CanSynchronize     := fcAllowed;
           CanReadExtended    := fcAllowed;
           CanWriteExtented   := fcAllowed;
           CanListDir         := fcAllowed;
           CanAddFile         := fcAllowed;
           CanAddSubDir       := fcAllowed;
           CanTraverse        := fcAllowed;
           CanDeleteChild     := fcAllowed;
        End;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTPipeCheckFlags                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTPipeCheckFlags.GetReadData: TCheckFlagValue;                              }
{ Read method property CanReadData                                             }
{------------------------------------------------------------------------------}
 Function TNTPipeCheckFlags.GetReadData: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_READ_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeCheckFlags.SetReadData(Const Value: TCheckFlagValue);                 }
{ Write method property CanReadData                                            }
{------------------------------------------------------------------------------}
 Procedure TNTPipeCheckFlags.SetReadData(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_READ_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeCheckFlags.GetWriteData: TCheckFlagValue;                             }
{ Read method property CanWriteData                                            }
{------------------------------------------------------------------------------}
 Function TNTPipeCheckFlags.GetWriteData: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_WRITE_DATA);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeCheckFlags.SetWriteData(Const Value: TCheckFlagValue);                }
{ Write method property CanWriteData                                           }
{------------------------------------------------------------------------------}
 Procedure TNTPipeCheckFlags.SetWriteData(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_WRITE_DATA, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeCheckFlags.GetCreateInstance: TCheckFlagValue;                        }
{ Read method property CanCreateInstance                                       }
{------------------------------------------------------------------------------}
 Function TNTPipeCheckFlags.GetCreateInstance: TCheckFlagValue;
 Begin
    Result := GetAttributes(FILE_CREATE_PIPE_INSTANCE);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeCheckFlags.SetCreateInstance(Const Value: TCheckFlagValue);           }
{ Write method property CanCreateInstance                                      }
{------------------------------------------------------------------------------}
 Procedure TNTPipeCheckFlags.SetCreateInstance(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(FILE_CREATE_PIPE_INSTANCE, Value);
 End;

{------------------------------------------------------------------------------}
{ TNTPipeCheckFlags.Assign(Source: TPersistent);                               }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TNTPipeCheckFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TNTPipeCheckFlags) Then
       With Source As TNTPipeCheckFlags Do Begin
          Self.CanReadData       := CanReadData;
          Self.CanWriteData      := CanWriteData;
          Self.CanCreateInstance := CanCreateInstance;
       End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileSecurityCheck                                          }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileSecurityCheck.Create(AOwner: TComponent);                             }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNTFileSecurityCheck.Create(AOwner: TComponent);
 Begin
    Inherited Create(AOwner);
    FDesiredFiler  := TNTFileDesiredCheckFlagsFiler.Create(Self);
    FGrantedFiler  := TNTFileGrantedCheckFlagsFiler.Create(Self);
    FDesiredChecks := TNTFileCheckFlags.Create(FDesiredFiler, False);
    FGrantedChecks := TNTFileCheckFlags.Create(FGrantedFiler, True);
 End;

{------------------------------------------------------------------------------}
{ TNTFileSecurityCheck.Destroy;                                                }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNTFileSecurityCheck.Destroy;
 Begin
    FDesiredChecks.Free;
    FGrantedChecks.Free;
    FDesiredFiler.Free;
    FGrantedFiler.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNTFileSecurityCheck.GetSecurityObj: TNTFileSecurity;                        }
{ Read Method property SecurityObject                                          }
{------------------------------------------------------------------------------}
 Function TNTFileSecurityCheck.GetSecurityObj: TNTFileSecurity;
 Begin
    Result := (Inherited SecurityObject) As TNTFileSecurity;
 End;

{------------------------------------------------------------------------------}
{ TNTFileSecurityCheck.SetSecurityObj(Const Value: TNTFileSecurity);           }
{ Write Method property SecurityObject                                         }
{------------------------------------------------------------------------------}
 Procedure TNTFileSecurityCheck.SetSecurityObj(Const Value: TNTFileSecurity);
 Begin
    Inherited SecurityObject := Value;
 End;

{------------------------------------------------------------------------------}
{ TNTFileSecurityCheck.GetDesiredChecks: TNTFileCheckFlags;                    }
{ Read Method property DesiredChecks                                           }
{------------------------------------------------------------------------------}
 Function TNTFileSecurityCheck.GetDesiredChecks: TNTFileCheckFlags;
 Begin
    Result := FDesiredChecks;
 End;

{------------------------------------------------------------------------------}
{ TNTFileSecurityCheck.SetDesiredChecks(Const Value: TNTFileCheckFlags);       }
{ Write Method property DesiredChecks                                          }
{------------------------------------------------------------------------------}
 Procedure TNTFileSecurityCheck.SetDesiredChecks(Const Value: TNTFileCheckFlags);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TNTFileSecurityCheck.GetGrantedChecks: TNTFileCheckFlags;                    }
{ Read Method property GrantedChecks                                           }
{------------------------------------------------------------------------------}
 Function TNTFileSecurityCheck.GetGrantedChecks: TNTFileCheckFlags;
 Begin
    Result := FGrantedChecks;
 End;

{------------------------------------------------------------------------------}
{ TNTFileSecurityCheck.SetGrantedChecks(Const Value: TNTFileCheckFlags);       }
{ Write Method property GrantedChecks                                          }
{------------------------------------------------------------------------------}
 Procedure TNTFileSecurityCheck.SetGrantedChecks(Const Value: TNTFileCheckFlags);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTDirectorySecurityCheck                                     }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTDirectorySecurityCheck.Create(AOwner: TComponent);                        }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNTDirectorySecurityCheck.Create(AOwner: TComponent);
 Begin
    Inherited Create(AOwner);
    FDesiredFiler  := TNTFileDesiredCheckFlagsFiler.Create(Self);
    FGrantedFiler  := TNTFileGrantedCheckFlagsFiler.Create(Self);
    FDesiredChecks := TNTDirCheckFlags.Create(FDesiredFiler, False);
    FGrantedChecks := TNTDirCheckFlags.Create(FGrantedFiler, True);
 End;

{------------------------------------------------------------------------------}
{ TNTDirectorySecurityCheck.Destroy;                                           }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TNTDirectorySecurityCheck.Destroy;
 Begin
    FDesiredChecks.Free;
    FGrantedChecks.Free;
    FDesiredFiler.Free;
    FGrantedFiler.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TNTDirectorySecurityCheck.GetSecurityObj: TNTDirectorySecurity;              }
{ Read Method property SecurityObject                                          }
{------------------------------------------------------------------------------}
 Function TNTDirectorySecurityCheck.GetSecurityObj: TNTDirectorySecurity;
 Begin
    Result := (Inherited SecurityObject) As TNTDirectorySecurity;
 End;

{------------------------------------------------------------------------------}
{ TNTDirectorySecurityCheck.SetSecurityObj(Const Value: TNTFileSecurity);      }
{ Write Method property SecurityObject                                         }
{------------------------------------------------------------------------------}
 Procedure TNTDirectorySecurityCheck.SetSecurityObj(Const Value: TNTDirectorySecurity);
 Begin
    Inherited SecurityObject := Value;
 End;

{------------------------------------------------------------------------------}
{ TNTDirectorySecurityCheck.GetDesiredChecks: TNTDirCheckFlags;                }
{ Read Method property DesiredChecks                                           }
{------------------------------------------------------------------------------}
 Function TNTDirectorySecurityCheck.GetDesiredChecks: TNTDirCheckFlags;
 Begin
    Result := FDesiredChecks;
 End;

{------------------------------------------------------------------------------}
{ TNTDirectorySecurityCheck.SetDesiredChecks(Const Value: TNTDirCheckFlags);   }
{ Write Method property DesiredChecks                                          }
{------------------------------------------------------------------------------}
 Procedure TNTDirectorySecurityCheck.SetDesiredChecks(Const Value: TNTDirCheckFlags);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TNTDirectorySecurityCheck.GetGrantedChecks: TNTDirCheckFlags;                }
{ Read Method property GrantedChecks                                           }
{------------------------------------------------------------------------------}
 Function TNTDirectorySecurityCheck.GetGrantedChecks: TNTDirCheckFlags;
 Begin
    Result := FGrantedChecks;
 End;

{------------------------------------------------------------------------------}
{ TNTDirectorySecurityCheck.SetGrantedChecks(Const Value: TNTDirCheckFlags);   }
{ Write Method property GrantedChecks                                          }
{------------------------------------------------------------------------------}
 Procedure TNTDirectorySecurityCheck.SetGrantedChecks(Const Value: TNTDirCheckFlags);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileAccessFlagsFiler                                       }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileAccessFlagsFiler.Create(Ace: TCustomNTFileAccess);                    }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNTFileAccessFlagsFiler.Create(Ace: TCustomNTFileAccess);
 Begin
    Inherited Create;
    FAce := Ace;
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccessFlagsFiler.GetAttributes(...): Cardinal;                        }
{ GetAttributes method override                                                }
{------------------------------------------------------------------------------}
 Function TNTFileAccessFlagsFiler.GetAttributes(Const lng: Cardinal): Cardinal;
 Begin
    Result := Cardinal(fvNone);
    If ((FAce.MaskFlags[fkYes] And lng)=lng) Then
       Result := Cardinal(fvAllowed);
    If ((FAce.MaskFlags[fkNo] And lng)=lng) Then
       Result := Cardinal(fvDenied);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAccessFlagsFiler.SetAttributes(...);                                  }
{ SetAttibutes method override                                                 }
{------------------------------------------------------------------------------}
 Procedure TNTFileAccessFlagsFiler.SetAttributes(Const lng: Cardinal; Const Value: Cardinal);
 Begin
    If FAce.IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    Case Value Of
       Cardinal(fvNone)   :
         Begin
            FAce.MaskFlags[fkYes] := FAce.MaskFlags[fkYes] And Not lng;
            FAce.MaskFlags[fkNo] := FAce.MaskFlags[fkNo] And Not lng;
         End;
       Cardinal(fvAllowed):
         FAce.MaskFlags[fkYes] := FAce.MaskFlags[fkYes] Or lng;
       Cardinal(fvDenied) :
         FAce.MaskFlags[fkNo] := FAce.MaskFlags[fkNo] Or lng;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileAuditFlagsFiler                                        }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileAuditFlagsFiler.Create(Ace: TCustomNTFileAudit);                      }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNTFileAuditFlagsFiler.Create(Ace: TCustomNTFileAudit);
 Begin
    Inherited Create;
    FAce := Ace;
 End;

{------------------------------------------------------------------------------}
{ TNTFileAuditFlagsFiler.GetAttributes(...): Cardinal;                         }
{ GetAttributes method override                                                }
{------------------------------------------------------------------------------}
 Function TNTFileAuditFlagsFiler.GetAttributes(Const lng: Cardinal): Cardinal;
 Var
    A, B: Boolean;
 Begin
    A := False; B := False;
    Result := Cardinal(faNone);
    If ((FAce.MaskFlags[fkYes] And lng)=lng) Then Begin
       Result := Cardinal(faSucceeded);
       A := True;
    End;
    If ((FAce.MaskFlags[fkNo] And lng)=lng) Then Begin
       Result := Cardinal(faFailed);
       B := True;
    End;
    If (A And B) Then
       Result := Cardinal(faSucceededFailed);
 End;

{------------------------------------------------------------------------------}
{ TNTFileAuditFlagsFiler.SetAttributes(...);                                   }
{ SetAttibutes method override                                                 }
{------------------------------------------------------------------------------}
 Procedure TNTFileAuditFlagsFiler.SetAttributes(Const lng: Cardinal; Const Value: Cardinal);
 Begin
    If FAce.IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    Case Value Of
       Cardinal(faNone)   :
         Begin
            FAce.MaskFlags[fkYes] := FAce.MaskFlags[fkYes] And Not lng;
            FAce.MaskFlags[fkNo] := FAce.MaskFlags[fkNo] And Not lng;
         End;
       Cardinal(faSucceeded):
         FAce.MaskFlags[fkYes] := FAce.MaskFlags[fkYes] Or lng;
       Cardinal(faFailed) :
         FAce.MaskFlags[fkNo] := FAce.MaskFlags[fkNo] Or lng;
       Cardinal(faSucceededFailed):
         Begin
            FAce.MaskFlags[fkYes] := FAce.MaskFlags[fkYes] Or lng;
            FAce.MaskFlags[fkNo] := FAce.MaskFlags[fkNo] Or lng;
         End;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileCheckFlagsFiler                                        }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileCheckFlagsFiler.Create(aCheck: TNTCustomFileSecurityCheck);           }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TNTFileCheckFlagsFiler.Create(aCheck: TNTCustomFileSecurityCheck);
 Begin
    Inherited Create;
    FCheck := aCheck;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileDesiredCheckFlagsFiler                                 }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileDesiredCheckFlagsFiler.GetAttributes(...): Cardinal;                  }
{ GetAttributes method override                                                }
{------------------------------------------------------------------------------}
 Function TNTFileDesiredCheckFlagsFiler.GetAttributes(Const lng: Cardinal): Cardinal;
 Begin
    Result := Cardinal(fcDenied);
    If ((FCheck.DesiredAccess And lng)=lng) Then
       Result := Cardinal(fcAllowed);
 End;

{------------------------------------------------------------------------------}
{ TNTFileDesiredCheckFlagsFiler.SetAttributes(...);                            }
{ SetAttibutes method override                                                 }
{------------------------------------------------------------------------------}
 Procedure TNTFileDesiredCheckFlagsFiler.SetAttributes(Const lng: Cardinal; Const Value: Cardinal);
 Begin
    Case Value Of
       Cardinal(fcAllowed):
         FCheck.DesiredAccess := FCheck.DesiredAccess Or lng;
       Cardinal(fcDenied) :
         FCheck.DesiredAccess := FCheck.DesiredAccess And Not lng;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TNTFileGrantedCheckFlagsFiler                                 }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TNTFileGrantedCheckFlagsFiler.GetAttributes(...): Cardinal;                  }
{ GetAttributes method override                                                }
{------------------------------------------------------------------------------}
 Function TNTFileGrantedCheckFlagsFiler.GetAttributes(Const lng: Cardinal): Cardinal;
 Begin
    Result := Cardinal(fcDenied);
    If ((FCheck.GrantedAccess And lng)=lng) Then
       Result := Cardinal(fcAllowed);
 End;

{------------------------------------------------------------------------------}
{ TNTFileGrantedCheckFlagsFiler.SetAttributes(...);                            }
{ SetAttibutes method override                                                 }
{------------------------------------------------------------------------------}
 Procedure TNTFileGrantedCheckFlagsFiler.SetAttributes(Const lng: Cardinal; Const Value: Cardinal);
 Begin
    Raise Exception.Create(SNotAllowed);
 End;

{------------------------------------------------------------------------------}
{ End of aclfiles.pas                                                          }
{------------------------------------------------------------------------------}
 End.



