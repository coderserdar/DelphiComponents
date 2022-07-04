{------------------------------------------------------------------------------}
{ aclregistry                                                                  }
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
 Unit aclregistry;

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
    TRegKeySecurity = Class;
    TRegKeyApplyFlag = (rifKey, rifSubkeys, rifSetOnly);
    TRegKeyApplyFlags = Set Of TRegKeyApplyFlag;

    TRegKeyAccesses = Class;
    TRegKeyAccess = Class;

    TRegKeyAudits = Class;
    TRegKeyAudit = Class;

    TRegKeyAccessFlags = Class;
    TRegKeyAuditFlags = Class;
    TRegKeyCheckFlags = Class;

    TRegKeyFlagsFiler = Class;
    TRegKeyAccessFlagsFiler = Class;
    TRegKeyAuditFlagsFiler = Class;
    TRegKeyCheckFlagsFiler = Class;
    TRegKeyDesiredCheckFlagsFiler = Class;
    TRegKeyGrantedCheckFlagsFiler = Class;


    TRegKeySecurity = Class(TCustomSecurityObject)
    Private
      FRootKey: HKEY;
      FKeyName: String;
      Function  GetDList: TRegKeyAccesses;
      Procedure SetDList(Const Value: TRegKeyAccesses);
      Function  GetSList: TRegKeyAudits;
      Procedure SetSList(Const Value: TRegKeyAudits);
      Function  GetFOwner: TCreatorOwnerID;
      Procedure SetFOwner(Const Value: TCreatorOwnerID);
      Function  GetFGroup: TCreatorGroupID;
      Procedure SetFGroup(Const Value: TCreatorGroupID);
      Function  GetKey: TRootKey;
      Procedure SetKey(Const Value: TRootKey);
      Function  GetKeyName: String;
      Procedure SetKeyName(Const Value: String);
    Protected
      Function  CreateAccessList: TDACL;                                        Override;
      Function  CreateAuditList: TSACL;                                         Override;
      Function  CreateOwnerID: TCreatorSecurityID;                              Override;
      Function  CreateGroupID: TCreatorSecurityID;                              Override;
      Function  ReadObjectDescriptor: PSecurityDescriptor;                      Override;
      Procedure WriteObjectDescriptor;                                          Override;
    Published
      Property Active;
      Property AutoSave;
      Property Accesses: TRegKeyAccesses     Read GetDList      Write SetDList  Stored False;
      Property Audits: TRegKeyAudits         Read GetSList      Write SetSList  Stored False;
      Property CreatorOwner: TCreatorOwnerID Read GetFOwner     Write SetFOwner Stored False;
      Property CreatorGroup: TCreatorGroupID Read GetFGroup     Write SetFGroup Stored False;
      Property ComputerName;
      Property LookupSystemName;
      Property RootKey: TRootKey             Read GetKey        Write SetKey;
      Property KeyName: String               Read GetKeyName    Write SetKeyName;
      Property Options;
    End;

    TRegKeyAccesses = Class(TDACL)
    Private
      Function GetItems(Index: Integer): TRegKeyAccess;
      Procedure SetItems(Index: Integer; Const Value: TRegKeyAccess);
    Public
      Property Items[Index: Integer]: TRegKeyAccess Read GetItems Write SetItems; Default;
    End;

    TCustomRegKeyAccess = Class(TCustomACE)
    Private
      Function GetApplyTo: TRegKeyApplyFlags;
      Procedure SetApplyTo(Const Value: TRegKeyApplyFlags);
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Property Status;
    Published
      Property ApplyTo: TRegKeyApplyFlags   Read GetApplyTo   Write SetApplyTo  Default [rifKey, rifSubkeys];
      Property IsInherited;
    End;

    TRegKeyAccess = Class(TCustomRegKeyAccess)
    Private
      FFiler: TRegKeyAccessFlagsFiler;
      FKeyFlags: TRegKeyAccessFlags;
      Function GetFlags: TRegKeyAccessFlags;
      Procedure SetFlags(Const Value: TRegKeyAccessFlags);
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
      Procedure GetDeniedAce(Value: PACL);                                      Override;
      Procedure GetAllowedAce(Value: PACL);                                     Override;
      Function GetDeniedSize: Integer;                                          Override;
      Function GetAllowedSize: Integer;                                         Override;
    Published
      Property Flags: TRegKeyAccessFlags    Read GetFlags     Write SetFlags;
    End;

    TRegKeyAccessFlags = Class(TPersistent)
    Private
      FFiler: TRegKeyAccessFlagsFiler;
      Function GetAce: TRegKeyAccess;
      Function GetDelete: TAccessFlagValue;
      Procedure SetDelete(Const Value: TAccessFlagValue);
      Function GetReadControl: TAccessFlagValue;
      Procedure SetReadControl(Const Value: TAccessFlagValue);
      Function GetWriteDac: TAccessFlagValue;
      Procedure SetWriteDac(Const Value: TAccessFlagValue);
      Function GetWriteOwner: TAccessFlagValue;
      Procedure SetWriteOwner(Const Value: TAccessFlagValue);
      Function GetGetValue: TAccessFlagValue;
      Procedure SetGetValue(Const Value: TAccessFlagValue);
      Function GetSetValue: TAccessFlagValue;
      Procedure SetSetValue(Const Value: TAccessFlagValue);
      Function GetCreateSubKey: TAccessFlagValue;
      Procedure SetCreateSubKey(Const Value: TAccessFlagValue);
      Function GetEnumerateKey: TAccessFlagValue;
      Procedure SetEnumerateKey(Const Value: TAccessFlagValue);
      Function GetNotify: TAccessFlagValue;
      Procedure SetNotify(Const Value: TAccessFlagValue);
      Function GetCreateLink: TAccessFlagValue;
      Procedure SetCreateLink(Const Value: TAccessFlagValue);
      Function GetFlagsMode: TNTRegFlagsMode;
      Procedure SetFlagsMode(Const Value: TNTRegFlagsMode);
    Protected
      Function GetAttributes(Const lng: Cardinal): TAccessFlagValue;               Virtual;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: TAccessFlagValue); Virtual;
    Public
      Constructor Create(Filer: TRegKeyAccessFlagsFiler);                          Overload;
      Destructor Destroy;                                                          Override;
      Procedure Assign(Source: TPersistent);                                       Override;
      Property ACE: TRegKeyAccess                    Read GetAce;
    Published
      Property CanDelete: TAccessFlagValue           Read GetDelete       Write SetDelete;
      Property CanReadControl: TAccessFlagValue      Read GetReadControl  Write SetReadControl;
      Property CanWriteDac: TAccessFlagValue         Read GetWriteDac     Write SetWriteDac;
      Property CanWriteOwner: TAccessFlagValue       Read GetWriteOwner   Write SetWriteOwner;
      Property CanGetValue: TAccessFlagValue         Read GetGetValue     Write SetGetValue;
      Property CanSetValue: TAccessFlagValue         Read GetSetValue     Write SetSetValue;
      Property CanCreateSubKey: TAccessFlagValue     Read GetCreateSubKey Write SetCreateSubKey;
      Property CanEnumerateSubKeys: TAccessFlagValue Read GetEnumerateKey Write SetEnumerateKey;
      Property CanNotify: TAccessFlagValue           Read GetNotify       Write SetNotify;
      Property CanCreateLink: TAccessFlagValue       Read GetCreateLink   Write SetCreateLink;
      Property FlagsMode: TNTRegFlagsMode            Read GetFlagsMode    Write SetFlagsMode     Default rfmRead;
    End;

    TRegKeyAudits = Class(TSACL)
    Private
      Function GetItems(Index: Integer): TRegKeyAudit;
      Procedure SetItems(Index: Integer; Const Value: TRegKeyAudit);
    Public
      Property Items[Index: Integer]: TRegKeyAudit Read GetItems Write SetItems; Default;
    End;

    TRegKeyAudit = Class(TCustomRegKeyAccess)
    Private
      FFiler: TRegKeyAuditFlagsFiler;
      FKeyFlags: TRegKeyAuditFlags;
      Function GetFlags: TRegKeyAuditFlags;
      Procedure SetFlags(Const Value: TRegKeyAuditFlags);
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
      Procedure GetDeniedAce(Value: PACL);                                      Override;
      Procedure GetAllowedAce(Value: PACL);                                     Override;
      Function GetDeniedSize: Integer;                                          Override;
      Function GetAllowedSize: Integer;                                         Override;
    Published
      Property Flags: TRegKeyAuditFlags     Read GetFlags     Write SetFlags;
    End;

    TRegKeyAuditFlags = Class(TPersistent)
    Private
      FFiler: TRegKeyAuditFlagsFiler;
      Function GetAce: TRegKeyAudit;
      Function GetDelete: TAuditFlagValue;
      Procedure SetDelete(Const Value: TAuditFlagValue);
      Function GetReadControl: TAuditFlagValue;
      Procedure SetReadControl(Const Value: TAuditFlagValue);
      Function GetWriteDac: TAuditFlagValue;
      Procedure SetWriteDac(Const Value: TAuditFlagValue);
      Function GetWriteOwner: TAuditFlagValue;
      Procedure SetWriteOwner(Const Value: TAuditFlagValue);
      Function GetGetValue: TAuditFlagValue;
      Procedure SetGetValue(Const Value: TAuditFlagValue);
      Function GetSetValue: TAuditFlagValue;
      Procedure SetSetValue(Const Value: TAuditFlagValue);
      Function GetCreateSubKey: TAuditFlagValue;
      Procedure SetCreateSubKey(Const Value: TAuditFlagValue);
      Function GetEnumerateKey: TAuditFlagValue;
      Procedure SetEnumerateKey(Const Value: TAuditFlagValue);
      Function GetNotify: TAuditFlagValue;
      Procedure SetNotify(Const Value: TAuditFlagValue);
      Function GetCreateLink: TAuditFlagValue;
      Procedure SetCreateLink(Const Value: TAuditFlagValue);
      Function GetFlagsMode: TNTRegFlagsMode;
      Procedure SetFlagsMode(Const Value: TNTRegFlagsMode);
    Protected
      Function GetAttributes(Const lng: Cardinal): TAuditFlagValue;                Virtual;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: TAuditFlagValue);  Virtual;
    Public
      Constructor Create(Filer: TRegKeyAuditFlagsFiler);                           Overload;
      Destructor Destroy;                                                          Override;
      Procedure Assign(Source: TPersistent);                                       Override;
      Property ACE: TRegKeyAudit                     Read GetAce;
    Published
      Property AuditDelete: TAuditFlagValue           Read GetDelete       Write SetDelete;
      Property AuditReadControl: TAuditFlagValue      Read GetReadControl  Write SetReadControl;
      Property AuditWriteDac: TAuditFlagValue         Read GetWriteDac     Write SetWriteDac;
      Property AuditWriteOwner: TAuditFlagValue       Read GetWriteOwner   Write SetWriteOwner;
      Property AuditGetValue: TAuditFlagValue         Read GetGetValue     Write SetGetValue;
      Property AuditSetValue: TAuditFlagValue         Read GetSetValue     Write SetSetValue;
      Property AuditCreateSubKey: TAuditFlagValue     Read GetCreateSubKey Write SetCreateSubKey;
      Property AuditEnumerateSubKeys: TAuditFlagValue Read GetEnumerateKey Write SetEnumerateKey;
      Property AuditNotify: TAuditFlagValue           Read GetNotify       Write SetNotify;
      Property AuditCreateLink: TAuditFlagValue       Read GetCreateLink   Write SetCreateLink;
      Property FlagsMode: TNTRegFlagsMode             Read GetFlagsMode    Write SetFlagsMode     Default rfmRead;
    End;

    // Checks Components
    TRegKeySecurityCheck = Class(TCustomAccessCheck)
    Private
      FDesiredFiler: TRegKeyDesiredCheckFlagsFiler;
      FGrantedFiler: TRegKeyGrantedCheckFlagsFiler;
      FDesiredChecks: TRegKeyCheckFlags;
      FGrantedChecks: TRegKeyCheckFlags;
      Function GetSecurityObj: TRegKeySecurity;
      Procedure SetSecurityObj(Const Value: TRegKeySecurity);
      Function GetDesiredChecks: TRegKeyCheckFlags;
      Procedure SetDesiredChecks(Const Value: TRegKeyCheckFlags);
      Function GetGrantedChecks: TRegKeyCheckFlags;
      Procedure SetGrantedChecks(Const Value: TRegKeyCheckFlags);
    Public
      Constructor Create(AOwner: TComponent);                                   Override;
      Destructor Destroy;                                                       Override;
    Published
      Property Active;
      Property Allowed;
      Property Mode;
      Property DesiredChecks: TRegKeyCheckFlags Read GetDesiredChecks Write SetDesiredChecks;
      Property GrantedChecks: TRegKeyCheckFlags Read GetGrantedChecks Write SetGrantedChecks Stored False;
      Property SecurityObject: TRegKeySecurity  Read GetSecurityObj   Write SetSecurityObj;
    End;

    TRegKeyCheckFlags = Class(TPersistent)
    Private
      FFiler: TRegKeyFlagsFiler;
      FReadOnly: Boolean;
      Function GetDelete: TCheckFlagValue;
      Procedure SetDelete(Const Value: TCheckFlagValue);
      Function GetReadControl: TCheckFlagValue;
      Procedure SetReadControl(Const Value: TCheckFlagValue);
      Function GetWriteDac: TCheckFlagValue;
      Procedure SetWriteDac(Const Value: TCheckFlagValue);
      Function GetWriteOwner: TCheckFlagValue;
      Procedure SetWriteOwner(Const Value: TCheckFlagValue);
      Function GetGetValue: TCheckFlagValue;
      Procedure SetGetValue(Const Value: TCheckFlagValue);
      Function GetSetValue: TCheckFlagValue;
      Procedure SetSetValue(Const Value: TCheckFlagValue);
      Function GetCreateSubKey: TCheckFlagValue;
      Procedure SetCreateSubKey(Const Value: TCheckFlagValue);
      Function GetEnumerateKey: TCheckFlagValue;
      Procedure SetEnumerateKey(Const Value: TCheckFlagValue);
      Function GetNotify: TCheckFlagValue;
      Procedure SetNotify(Const Value: TCheckFlagValue);
      Function GetCreateLink: TCheckFlagValue;
      Procedure SetCreateLink(Const Value: TCheckFlagValue);
      Function GetFlagsMode: TNTRegFlagsMode;
      Procedure SetFlagsMode(Const Value: TNTRegFlagsMode);
    Protected
      Function GetAttributes(Const lng: Cardinal): TCheckFlagValue;                Virtual;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: TCheckFlagValue);  Virtual;
    Public
      Constructor Create(Filer: TRegKeyFlagsFiler; Const ro: Boolean);             Overload;
      Destructor Destroy;                                                          Override;
      Procedure Assign(Source: TPersistent);                                       Override;
      Property ReadOnly: Boolean                    Read FReadOnly;
    Published
      Property CanDelete: TCheckFlagValue           Read GetDelete       Write SetDelete;
      Property CanReadControl: TCheckFlagValue      Read GetReadControl  Write SetReadControl;
      Property CanWriteDac: TCheckFlagValue         Read GetWriteDac     Write SetWriteDac;
      Property CanWriteOwner: TCheckFlagValue       Read GetWriteOwner   Write SetWriteOwner;
      Property CanGetValue: TCheckFlagValue         Read GetGetValue     Write SetGetValue;
      Property CanSetValue: TCheckFlagValue         Read GetSetValue     Write SetSetValue;
      Property CanCreateSubKey: TCheckFlagValue     Read GetCreateSubKey Write SetCreateSubKey;
      Property CanEnumerateSubKeys: TCheckFlagValue Read GetEnumerateKey Write SetEnumerateKey;
      Property CanNotify: TCheckFlagValue           Read GetNotify       Write SetNotify;
      Property CanCreateLink: TCheckFlagValue       Read GetCreateLink   Write SetCreateLink;
      Property FlagsMode: TNTRegFlagsMode           Read GetFlagsMode    Write SetFlagsMode     Default rfmRead;
    End;

    // Flags Filers
    TRegKeyFlagsFiler = Class(TPersistent)
    Protected
      Function GetAttributes(Const lng: Cardinal): Cardinal;                    Virtual; Abstract;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: Cardinal);      Virtual; Abstract;
    End;

    TRegKeyAccessFlagsFiler = Class(TRegKeyFlagsFiler)
    Private
      FAce: TRegKeyAccess;
    Protected
      Function GetAttributes(Const lng: Cardinal): Cardinal;                    Override;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: Cardinal);      Override;
    Public
      Constructor Create(Ace: TRegKeyAccess);                                   Overload;
      Property ACE: TRegKeyAccess              Read FAce;
    End;

    TRegKeyAuditFlagsFiler = Class(TRegKeyFlagsFiler)
    Private
      FAce: TRegKeyAudit;
    Protected
      Function GetAttributes(Const lng: Cardinal): Cardinal;                    Override;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: Cardinal);      Override;
    Public
      Constructor Create(Ace: TRegKeyAudit);                                    Overload;
      Property ACE: TRegKeyAudit               Read FAce;
    End;

    TRegKeyCheckFlagsFiler = Class(TRegKeyFlagsFiler)
    Private
      FCheck: TRegKeySecurityCheck;
    Public
      Constructor Create(aCheck: TRegKeySecurityCheck);                         Overload;
      Property Check: TRegKeySecurityCheck     Read FCheck;
    End;

    TRegKeyDesiredCheckFlagsFiler = Class(TRegKeyCheckFlagsFiler)
    Protected
      Function GetAttributes(Const lng: Cardinal): Cardinal;                    Override;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: Cardinal);      Override;
    End;

    TRegKeyGrantedCheckFlagsFiler = Class(TRegKeyCheckFlagsFiler)
    Protected
      Function GetAttributes(Const lng: Cardinal): Cardinal;                    Override;
      Procedure SetAttributes(Const lng: Cardinal; Const Value: Cardinal);      Override;
    End;


    Function RegReadSecurity(Const Computer: String; Root: HKEY; Const Path: String; SecurityInformation: Cardinal): PSecurityDescriptor;
    Procedure RegSaveSecurity(Const Computer: String; Root: HKEY; Const Path: String; SecurityInformation: Cardinal; sd: PSecurityDescriptor);

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
 Uses Forms, Dialogs;

{------------------------------------------------------------------------------}
{ Constantes                                                                   }
{------------------------------------------------------------------------------}
 ResourceString
   SNotAllowed       = 'Opération non autorisée !';

{------------------------------------------------------------------------------}
{ RegReadSecurity(Key: HKEY): PSecurityDescriptor;                             }
{ Read a Reg Key Security Descriptor                                           }
{------------------------------------------------------------------------------}
 Function RegReadSecurity(Const Computer: String; Root: HKEY; Const Path: String; SecurityInformation: Cardinal): PSecurityDescriptor;
 Var
    Err    : Cardinal;
    dlen   : Cardinal;
    access : Cardinal;
    desc   : PSecurityDescriptor;
    RootKey: HKEY;
    TempKey: HKEY;
    tkprivs: TTokenPrivileges;
    saclhdl: THandle;

   {---------------------------------------------------------------------------}
    Function InitializeTokens: Cardinal;
    Var
       outlen : Cardinal;
       tmpprvs: TTokenPrivileges;
    Begin
       Result := 0;
       { Primary Group Information }
       If ((SecurityInformation And GROUP_SECURITY_INFORMATION)=GROUP_SECURITY_INFORMATION) Then
          Result := Result Or KEY_READ;
       { Accesses Information }
       If ((SecurityInformation And DACL_SECURITY_INFORMATION)=DACL_SECURITY_INFORMATION) Then
          Result := Result Or KEY_READ;
       { Owner Information }
       If ((SecurityInformation And OWNER_SECURITY_INFORMATION)=OWNER_SECURITY_INFORMATION) Then Begin
          Result := Result Or WRITE_OWNER Or KEY_READ;
          If Not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES Or TOKEN_QUERY, saclhdl) Then
             RaiseACLError;
          Zeromemory(@tmpprvs, SizeOf(tmpprvs));
          Zeromemory(@tkprivs, SizeOf(tkprivs));
          outlen := 0;
          If Not LookupPrivilegeValue(PChar(Computer), SE_TAKE_OWNERSHIP_NAME, tmpprvs.Privileges[0].Luid) Then
             RaiseACLError;
          tmpprvs.PrivilegeCount := 1;
          tmpprvs.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
          If Not AdjustTokenPrivileges(saclhdl, False, tmpprvs, SizeOf(tkprivs), tkprivs, outlen) Then
             RaiseACLError;
       End;
       { Audit Information }
       If ((SecurityInformation And SACL_SECURITY_INFORMATION)=SACL_SECURITY_INFORMATION) Then Begin
          Result := Result Or ACCESS_SYSTEM_SECURITY Or KEY_READ;
          If Not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES Or TOKEN_QUERY, saclhdl) Then
             RaiseACLError;
          Zeromemory(@tmpprvs, SizeOf(tmpprvs));
          Zeromemory(@tkprivs, SizeOf(tkprivs));
          outlen := 0;
          If Not LookupPrivilegeValue(PChar(Computer), SE_SECURITY_NAME, tmpprvs.Privileges[0].Luid) Then
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
    Result := Nil;
    dlen := 0;
    desc := Nil;
    SetLastError(0);
    RootKey := Root;
    If (Computer<>'') Then Begin
       Err := RegConnectRegistry(PChar(Computer), Root, RootKey);
       If (Err<>ERROR_SUCCESS) Then
          RaiseACLError(Err);
    End;
    access := InitializeTokens;
    Try
       Err := RegOpenKeyEx(RootKey, PChar(Path), 0, access, TempKey);
       If (Err=ERROR_SUCCESS) Then
          Try
             Err := RegGetKeySecurity(TempKey, SecurityInformation, desc, dlen);
             If (Err<>ERROR_SUCCESS) And (Err<>ERROR_INSUFFICIENT_BUFFER) Then
                RaiseACLError(Err);
             desc := sdAlloc(dlen);
             Try
                Err := RegGetKeySecurity(TempKey, SecurityInformation, desc, dlen);
                If (Err<>ERROR_SUCCESS) Then
                   RaiseACLError(Err);
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
             RegCloseKey(TempKey);
          End
       Else
          RaiseACLError(Err);
    Finally
       FinalizeTokens;
    End;
 End;

{------------------------------------------------------------------------------}
{ RegSaveSecurity(Const Key: HKEY; sd: PSecurityDescriptor);                   }
{ Saves a Reg Key Security Descriptor                                          }
{------------------------------------------------------------------------------}
 Procedure RegSaveSecurity(Const Computer: String; Root: HKEY; Const Path: String; SecurityInformation: Cardinal; sd: PSecurityDescriptor);
 Var
    Err    : Cardinal;
    dlen   : Cardinal;
    access : Cardinal;
    desc   : PSecurityDescriptor;
    RootKey: HKEY;
    TempKey: HKEY;
    tkprivs: TTokenPrivileges;
    saclhdl: THandle;

   {---------------------------------------------------------------------------}
    Function InitializeTokens: Cardinal;
    Var
       outlen : Cardinal;
       tmpprvs: TTokenPrivileges;
    Begin
       Result := 0;
       { Owner Information }
       If ((SecurityInformation And OWNER_SECURITY_INFORMATION)=OWNER_SECURITY_INFORMATION) Then Begin
          Result := Result Or WRITE_OWNER;
          If Not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES Or TOKEN_QUERY, saclhdl) Then
             RaiseACLError;
          Zeromemory(@tmpprvs, SizeOf(tmpprvs));
          Zeromemory(@tkprivs, SizeOf(tkprivs));
          outlen := 0;
          If Not LookupPrivilegeValue(PChar(Computer), SE_TAKE_OWNERSHIP_NAME, tmpprvs.Privileges[0].Luid) Then
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
          If Not LookupPrivilegeValue(PChar(Computer), SE_SECURITY_NAME, tmpprvs.Privileges[0].Luid) Then
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
    RootKey := Root;
    If (Computer<>'') Then Begin
       Err := RegConnectRegistry(PChar(Computer), Root, RootKey);
       If (Err<>ERROR_SUCCESS) Then
          RaiseACLError(Err);
    End;
    access := InitializeTokens;
    Try
       Err := RegOpenKeyEx(RootKey, PChar(Path), 0, access, TempKey);
       If (Err=ERROR_SUCCESS) Then
          Try
             desc := MakeSelfRelativeSecurityDescriptor(sd, dlen);
             Try
                Err := RegSetKeySecurity(TempKey, SecurityInformation, desc);
                If (Err<>ERROR_SUCCESS) Then
                   RaiseACLError(Err);
             Finally
                sdFree(desc, dlen);
             End;
          Finally
             RegCloseKey(TempKey);
          End
       Else
          RaiseACLError(Err);
    Finally
       FinalizeTokens;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegKeySecurity                                               }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeySecurity.CreateAccessList: TDACL;                                     }
{ Create the Access List                                                       }
{------------------------------------------------------------------------------}
 Function TRegKeySecurity.CreateAccessList: TDACL;
 Begin
    Result := TRegKeyAccesses.Create(Self, Self, TRegKeyAccess);
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.CreateAuditList: TSACL;                                      }
{ Create the Access List                                                       }
{------------------------------------------------------------------------------}
 Function TRegKeySecurity.CreateAuditList: TSACL;
 Begin
    Result := TRegKeyAudits.Create(Self, Self, TRegKeyAudit);
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.CreateOwnerID: TCreatorSecurityID;                           }
{ Create the Owner ID                                                          }
{------------------------------------------------------------------------------}
 Function TRegKeySecurity.CreateOwnerID: TCreatorSecurityID;
 Begin
    Result := TCreatorOwnerID.Create(Self);
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.CreateGroupID: TCreatorSecurityID;                           }
{ Create the Group ID                                                          }
{------------------------------------------------------------------------------}
 Function TRegKeySecurity.CreateGroupID: TCreatorSecurityID;
 Begin
    Result := TCreatorGroupID.Create(Self);
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.GetDList: TRegKeyAccesses;                                   }
{ Read method property AccessList                                              }
{------------------------------------------------------------------------------}
 Function TRegKeySecurity.GetDList: TRegKeyAccesses;
 Begin
    If (sdWantACL In Options) Then
       Result := TRegKeyAccesses(GetAccessList)
    Else
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.SetDList(...);                                               }
{ Write method property AccessList                                             }
{------------------------------------------------------------------------------}
 Procedure TRegKeySecurity.SetDList(Const Value: TRegKeyAccesses);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.GetSList: TRegKeyAudits;                                     }
{ Read method property AuditList                                               }
{------------------------------------------------------------------------------}
 Function TRegKeySecurity.GetSList: TRegKeyAudits;
 Begin
    If (sdWantSystemACL In Options) Then
       Result := TRegKeyAudits(GetAuditList)
    Else
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.SetSList(Const Value: TRegKeyAudits);                        }
{ Write method property AuditList                                              }
{------------------------------------------------------------------------------}
 Procedure TRegKeySecurity.SetSList(Const Value: TRegKeyAudits);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.GetFOwner: TCreatorOwnerID;                                  }
{ Read method property CreatorOwner                                            }
{------------------------------------------------------------------------------}
 Function TRegKeySecurity.GetFOwner: TCreatorOwnerID;
 Begin
    If (sdWantOwner In Options) Then
       Result := TCreatorOwnerID(GetOwnerID)
    Else
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.SetFOwner(Const Value: TCreatorOwnerID);                     }
{ Write method property CreatorOwner                                           }
{------------------------------------------------------------------------------}
 Procedure TRegKeySecurity.SetFOwner(Const Value: TCreatorOwnerID);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.GetFGroup: TCreatorGroupID;                                  }
{ Read method property CreatorGroup                                            }
{------------------------------------------------------------------------------}
 Function TRegKeySecurity.GetFGroup: TCreatorGroupID;
 Begin
    If (sdWantGroup In Options) Then
       Result := TCreatorGroupID(GetGroupID)
    Else
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.SetFGroup(Const Value: TCreatorGroupID);                     }
{ Write method property CreatorGroup                                           }
{------------------------------------------------------------------------------}
 Procedure TRegKeySecurity.SetFGroup(Const Value: TCreatorGroupID);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.GetKey: TRootKey;                                            }
{ Read method property RootKey                                                 }
{------------------------------------------------------------------------------}
 Function TRegKeySecurity.GetKey: TRootKey;
 Begin
    Case FRootKey Of
      HKEY_CLASSES_ROOT:
        Result := CLASSES_ROOT;
      HKEY_CURRENT_USER:
        Result := CURRENT_USER;
      HKEY_LOCAL_MACHINE:
        Result := LOCAL_MACHINE;
      HKEY_USERS:
        Result := USERS;
      HKEY_PERFORMANCE_DATA:
        Result := PERFORMANCE_DATA;
      HKEY_CURRENT_CONFIG:
        Result := CURRENT_CONFIG;
      HKEY_DYN_DATA:
        Result := DYN_DATA;
    Else
        Result := CURRENT_USER;
    End;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.SetKey(Const Value: TRootKey);                               }
{ Write method property RootKey                                                }
{------------------------------------------------------------------------------}
 Procedure TRegKeySecurity.SetKey(Const Value: TRootKey);
 Var
    Sv: Boolean;
 Begin
    Try
       Sv := Active;
       If Sv Then
          Active := False;
       Case Value Of
         CLASSES_ROOT:
           FRootKey := HKEY_CLASSES_ROOT;
         CURRENT_USER:
           FRootKey := HKEY_CURRENT_USER;
         LOCAL_MACHINE:
           FRootKey := HKEY_LOCAL_MACHINE;
         USERS:
           FRootKey := HKEY_USERS;
         PERFORMANCE_DATA:
           FRootKey := HKEY_PERFORMANCE_DATA;
         CURRENT_CONFIG:
           FRootKey := HKEY_CURRENT_CONFIG;
         DYN_DATA:
           FRootKey := HKEY_DYN_DATA;
       End;
       If Sv Then
          Active := True;
    Except
       Active := False;
       Raise;
    End;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.GetKeyName: String;                                          }
{ Read method property KeyName                                                 }
{------------------------------------------------------------------------------}
 Function TRegKeySecurity.GetKeyName: String;
 Begin
    Result := FKeyName;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.SetKeyName(Const Value: String);                             }
{ Write method property KeyName                                                }
{------------------------------------------------------------------------------}
 Procedure TRegKeySecurity.SetKeyName(Const Value: String);
 Var
    SV: Boolean;
 Begin
    Try
       SV := Active;
       If SV Then
          Active := False;
       FKeyName := Value;
       If Sv Then
          Active := True;
    Except
       Active := False;
       Raise;
    End;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.ReadObjectDescriptor;                                        }
{ Read the SecurityDescriptor                                                  }
{------------------------------------------------------------------------------}
 Function TRegKeySecurity.ReadObjectDescriptor: PSecurityDescriptor;
 Begin
    Result := RegReadSecurity(ComputerName, FRootKey, KeyName, SecurityInf);
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurity.WriteObjectDescriptor(...);                                  }
{ Writes the SecurityDescriptor                                                }
{------------------------------------------------------------------------------}
 Procedure TRegKeySecurity.WriteObjectDescriptor;
 Begin
    CheckSD;
    RegSaveSecurity(ComputerName, FRootKey, KeyName, SecurityInf, Descriptor);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegKeyAccesses                                               }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeyAccesses.GetItems(...): TRegKeyAccess;                                }
{ Read Method property Items                                                   }
{------------------------------------------------------------------------------}
 Function TRegKeyAccesses.GetItems(Index: Integer): TRegKeyAccess;
 Begin
    Result := TRegKeyAccess(Inherited Items[Index]);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccesses.SetItems(...);                                               }
{ Write Method property Items                                                  }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccesses.SetItems(Index: Integer; Const Value: TRegKeyAccess);
 Begin
    Inherited Items[Index] := Value;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomRegKeyAccess                                           }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomRegKeyAccess.Create(Collection: TCollection);                         }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCustomRegKeyAccess.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    Applyto := [rifKey, rifSubkeys];
 End;

{------------------------------------------------------------------------------}
{ TCustomRegKeyAccess.GetApplyTo: TRegKeyApplyFlags;                           }
{ Read method property ApplyTo                                                 }
{------------------------------------------------------------------------------}
 Function TCustomRegKeyAccess.GetApplyTo: TRegKeyApplyFlags;
 Begin
    Result := [rifKey];
    If (afNoPropagate In AceFlags) Then
       Include(Result, rifSetOnly);
    If (afInheritOnly In AceFlags) Then
       Exclude(Result, rifKey);
    If (afContainerInherit In AceFlags) Then
       Include(Result, rifSubkeys);
 End;

{------------------------------------------------------------------------------}
{ TCustomRegKeyAccess.SetApplyTo(Const Value: TRegKeyApplyFlags);              }
{ Write method property ApplyTo                                                }
{------------------------------------------------------------------------------}
 Procedure TCustomRegKeyAccess.SetApplyTo(Const Value: TRegKeyApplyFlags);
 Begin
    If IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    AceFlags := Aceflags+[afInheritOnly]-[afNoPropagate, afContainerInherit];
    If (rifSubkeys In Value) Then
       AceFlags := AceFlags+[afContainerInherit];
    If (rifSetOnly In Value) Then
       AceFlags := AceFlags+[afNoPropagate];
    If (rifKey In Value) Then
       AceFlags := AceFlags-[afInheritOnly];
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegKeyAccess                                                 }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeyAccess.Create(Collection: TCollection);                               }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TRegKeyAccess.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    FFiler := TRegKeyAccessFlagsFiler.Create(Self);
    FKeyFlags := TRegKeyAccessFlags.Create(FFiler);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccess.Destroy;                                                       }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TRegKeyAccess.Destroy;
 Begin
    FKeyFlags.Free;
    FFiler.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccess.GetFlags: TRegKeyAccessFlags;                                  }
{ Read method property Flags                                                   }
{------------------------------------------------------------------------------}
 Function TRegKeyAccess.GetFlags: TRegKeyAccessFlags;
 Begin
    Result := FKeyFlags;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccess.SetFlags(Const Value: TRegKeyAccessFlags);                     }
{ Write method property Flags                                                  }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccess.SetFlags(Const Value: TRegKeyAccessFlags);
 Begin
    FKeyFlags.Assign(Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccess.GetDeniedAce(Value: PACL);                                     }
{ GetDeniedAce method Override                                                 }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccess.GetDeniedAce(Value: PACL);
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
{ TRegKeyAccess.GetAllowedAce(Value: PACL);                                    }
{ GetAllowedAce method Override                                                }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccess.GetAllowedAce(Value: PACL);
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
{ TRegKeyAccess.GetDeniedSize: Integer;                                        }
{ GetDeniedSize method Override                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyAccess.GetDeniedSize: Integer;
 Begin
    Result := 0;
    If MaskFlags[fkNo]=0 Then Exit;
    Result := SizeOf(TAceHeader)+SizeOf(TAccessDeniedAce)+SizeOf(Byte)-SizeOf(DWORD);
    Inc(Result, GetLengthSid(Identifier.AsSID));
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccess.GetAllowedSize: Integer;                                       }
{ GetAllowedSize method Override                                               }
{------------------------------------------------------------------------------}
 Function TRegKeyAccess.GetAllowedSize: Integer;
 Begin
    Result := 0;
    If (MaskFlags[fkYes]=0) Then Exit;
    Result := SizeOf(TAceHeader)+SizeOf(TAccessAllowedAce)+SizeOf(Byte)-SizeOf(DWORD);
    Inc(Result, GetLengthSid(Identifier.AsSID));
 End;


{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegKeyAccessFlags                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeyAccess.Create(Filer: TRegKeyAccessFlagsFiler);                        }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TRegKeyAccessFlags.Create(Filer: TRegKeyAccessFlagsFiler);
 Begin
    Inherited Create;
    FFiler := Filer;
    SetFlagsMode(rfmRead);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.Destroy;                                                  }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TRegKeyAccessFlags.Destroy;
 Begin
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TCustomNTFileAccessFlags.GetAce: TRegKeyAccess;                              }
{ Read method property ACE                                                     }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetAce: TRegKeyAccess;
 Begin
    Result := FFiler.Ace;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.GetAttributes(Const lng: Cardinal): TAccessFlagValue;     }
{ Generic Read attibutes method                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetAttributes(Const lng: Cardinal): TAccessFlagValue;
 Begin
    Result := TAccessFlagValue(FFiler.GetAttributes(lng));
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.SetAttributes(...);                                       }
{ Generic Write attibutes method                                               }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.SetAttributes(Const lng: Cardinal; Const Value: TAccessFlagValue);
 Begin
    FFiler.SetAttributes(lng, Cardinal(Value));
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.GetDelete: TAccessFlagValue;                              }
{ Read method property CanDelete                                               }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetDelete: TAccessFlagValue;
 Begin
    Result := GetAttributes(_DELETE);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.SetDelete(Const Value: TAccessFlagValue);                 }
{ Write method property CanDelete                                              }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.SetDelete(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(_DELETE, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.GetReadControl: TAccessFlagValue;                         }
{ Read method property ReadControl                                             }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetReadControl: TAccessFlagValue;
 Begin
    Result := GetAttributes(READ_CONTROL);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.SetReadControl(Const Value: TAccessFlagValue);            }
{ Write method property ReadControl                                            }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.SetReadControl(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(READ_CONTROL, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.GetWriteDac: TAccessFlagValue;                            }
{ Read method property WriteDac                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetWriteDac: TAccessFlagValue;
 Begin
    Result := GetAttributes(WRITE_DAC);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.SetWriteDac(Const Value: TAccessFlagValue);               }
{ Write method property Write Dac                                              }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.SetWriteDac(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(WRITE_DAC, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.GetWriteOwner: TAccessFlagValue;                          }
{ Read method property WriteOwner                                              }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetWriteOwner: TAccessFlagValue;
 Begin
    Result := GetAttributes(WRITE_OWNER);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.SetWriteOwner(Const Value: TAccessFlagValue);             }
{ Write method property WriteOwner                                             }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.SetWriteOwner(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(WRITE_OWNER, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.GetGetValue: TAccessFlagValue;                            }
{ Read method property GetValue                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetGetValue: TAccessFlagValue;
 Begin
    Result := GetAttributes(KEY_QUERY_VALUE);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.SetGetValue(Const Value: TAccessFlagValue);               }
{ Write method property GetValue                                               }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.SetGetValue(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(KEY_QUERY_VALUE, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.GetSetValue: TAccessFlagValue;                            }
{ Read method property SetValue                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetSetValue: TAccessFlagValue;
 Begin
    Result := GetAttributes(KEY_SET_VALUE);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.SetSetValue(Const Value: TAccessFlagValue);               }
{ Write method property SetValue                                               }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.SetSetValue(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(KEY_SET_VALUE, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.GetCreateSubKey: TAccessFlagValue;                        }
{ Read method property CreateSubKey                                            }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetCreateSubKey: TAccessFlagValue;
 Begin
    Result := GetAttributes(KEY_CREATE_SUB_KEY);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.SetCreateSubKey(Const Value: TAccessFlagValue);           }
{ Write method property CreateSubKey                                           }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.SetCreateSubKey(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(KEY_CREATE_SUB_KEY, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.GetEnumerateKey: TAccessFlagValue;                        }
{ Read method property EnumerateKey                                            }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetEnumerateKey: TAccessFlagValue;
 Begin
    Result := GetAttributes(KEY_ENUMERATE_SUB_KEYS);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.SetEnumerateKey(Const Value: TAccessFlagValue);           }
{ Write method property EnumerateKey                                           }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.SetEnumerateKey(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(KEY_ENUMERATE_SUB_KEYS, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.GetNotify: TAccessFlagValue;                              }
{ Read method property Notify                                                  }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetNotify: TAccessFlagValue;
 Begin
    Result := GetAttributes(KEY_NOTIFY);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.SetNotify(Const Value: TAccessFlagValue);                 }
{ Write method property Notify                                                 }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.SetNotify(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(KEY_NOTIFY, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.GetCreateLink: TAccessFlagValue;                          }
{ Read method property CreateLink                                              }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetCreateLink: TAccessFlagValue;
 Begin
    Result := GetAttributes(KEY_CREATE_LINK);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.SetCreateLink(Const Value: TAccessFlagValue);             }
{ Write method property CreateLink                                             }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.SetCreateLink(Const Value: TAccessFlagValue);
 Begin
    SetAttributes(KEY_CREATE_LINK, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.Assign(Source: TPersistent);                              }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TRegKeyAccessFlags) Then
       With Source As TRegKeyAccessFlags Do Begin
          Self.CanDelete           := CanDelete;
          Self.CanReadControl      := CanReadControl;
          Self.CanWriteDac         := CanWriteDac;
          Self.CanWriteOwner       := CanWriteOwner;
          Self.CanGetValue         := CanGetValue;
          Self.CanSetValue         := CanSetValue;
          Self.CanCreateSubKey     := CanCreateSubKey;
          Self.CanEnumerateSubKeys := CanEnumerateSubKeys;
          Self.CanNotify           := CanNotify;
          Self.CanCreateLink       := CanCreateLink;
       End;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.GetFlagsMode: TNTRegFlagsMode;                            }
{ Read Method Property FlagsMode                                               }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlags.GetFlagsMode: TNTRegFlagsMode;
 Begin
    Result := rfmCustom;
    If ((CanDelete=fvAllowed) And
       (CanReadControl=fvAllowed) And
       (CanWriteDac=fvAllowed) And
       (CanWriteOwner=fvAllowed) And
       (CanGetValue=fvAllowed) And
       (CanSetValue=fvAllowed) And
       (CanCreateSubKey=fvAllowed) And
       (CanEnumerateSubKeys=fvAllowed) And
       (CanNotify=fvAllowed) And
       (CanCreateLink=fvAllowed)) Then
       Result := rfmAll
    Else If ((CanDelete=fvNone) And
       (CanReadControl=fvAllowed) And
       (CanWriteDac=fvNone) And
       (CanWriteOwner=fvNone) And
       (CanGetValue=fvAllowed) And
       (CanSetValue=fvNone) And
       (CanCreateSubKey=fvNone) And
       (CanEnumerateSubKeys=fvAllowed) And
       (CanNotify=fvAllowed) And
       (CanCreateLink=fvNone)) Then
       Result := rfmRead;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlags.SetFlagsMode(Const Value: TNTRegFlagsMode);               }
{ Write Method Property FlagsMode                                              }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlags.SetFlagsMode(Const Value: TNTRegFlagsMode);
 Begin
    If Ace.IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    Case Value Of
      rfmRead:
        Begin
           CanDelete           := fvNone;
           CanReadControl      := fvAllowed;
           CanWriteDac         := fvNone;
           CanWriteOwner       := fvNone;
           CanGetValue         := fvAllowed;
           CanSetValue         := fvNone;
           CanCreateSubKey     := fvNone;
           CanEnumerateSubKeys := fvAllowed;
           CanNotify           := fvAllowed;
           CanCreateLink       := fvNone;
        End;
      rfmAll:
        Begin
           CanDelete           := fvAllowed;
           CanReadControl      := fvAllowed;
           CanWriteDac         := fvAllowed;
           CanWriteOwner       := fvAllowed;
           CanGetValue         := fvAllowed;
           CanSetValue         := fvAllowed;
           CanCreateSubKey     := fvAllowed;
           CanEnumerateSubKeys := fvAllowed;
           CanNotify           := fvAllowed;
           CanCreateLink       := fvAllowed;
        End;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegKeyAudits                                                 }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeyAudits.GetItems(...): TRegKeyAudit;                                   }
{ Read Method property Items                                                   }
{------------------------------------------------------------------------------}
 Function TRegKeyAudits.GetItems(Index: Integer): TRegKeyAudit;
 Begin
    Result := TRegKeyAudit(Inherited Items[Index]);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAudits.SetItems(...);                                                 }
{ Write Method property Items                                                  }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAudits.SetItems(Index: Integer; Const Value: TRegKeyAudit);
 Begin
    Inherited Items[Index] := Value;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegKeyAudit                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeyAudit.Create(Collection: TCollection);                                }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TRegKeyAudit.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    FFiler := TRegKeyAuditFlagsFiler.Create(Self);
    FKeyFlags := TRegKeyAuditFlags.Create(FFiler);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAudit.Destroy;                                                        }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TRegKeyAudit.Destroy;
 Begin
    FKeyFlags.Free;
    FFiler.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAudit.GetFlags: TRegKeyAuditFlags;                                    }
{ Read method property Flags                                                   }
{------------------------------------------------------------------------------}
 Function TRegKeyAudit.GetFlags: TRegKeyAuditFlags;
 Begin
    Result := FKeyFlags;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAudit.SetFlags(Const Value: TRegKeyAuditFlags);                       }
{ Write method property Flags                                                  }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAudit.SetFlags(Const Value: TRegKeyAuditFlags);
 Begin
    FKeyFlags.Assign(Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAudit.GetDeniedAce(Value: PACL);                                      }
{ GetDeniedAce method Override                                                 }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAudit.GetDeniedAce(Value: PACL);
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
{ TRegKeyAudit.GetAllowedAce(Value: PACL);                                     }
{ GetAllowedAce method Override                                                }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAudit.GetAllowedAce(Value: PACL);
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
{ TRegKeyAudit.GetDeniedSize: Integer;                                         }
{ GetDeniedSize method Override                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyAudit.GetDeniedSize: Integer;
 Begin
    Result := 0;
    If MaskFlags[fkNo]=0 Then Exit;
    Result := SizeOf(TAceHeader)+SizeOf(TSystemAuditAce)+SizeOf(Byte)-SizeOf(DWORD);
    Inc(Result, GetLengthSid(Identifier.AsSID));
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAudit.GetAllowedSize: Integer;                                        }
{ GetAllowedSize method Override                                               }
{------------------------------------------------------------------------------}
 Function TRegKeyAudit.GetAllowedSize: Integer;
 Begin
    Result := 0;
    If (MaskFlags[fkYes]=0) Then Exit;
    Result := SizeOf(TAceHeader)+SizeOf(TSystemAuditAce)+SizeOf(Byte)-SizeOf(DWORD);
    Inc(Result, GetLengthSid(Identifier.AsSID));
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegKeyAuditFlags                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.Create(Filer: TRegKeyAuditFlagsFiler);                     }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TRegKeyAuditFlags.Create(Filer: TRegKeyAuditFlagsFiler);
 Begin
    Inherited Create;
    FFiler := Filer;
    SetFlagsMode(rfmRead);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.Destroy;                                                   }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TRegKeyAuditFlags.Destroy;
 Begin
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetAce: TRegKeyAudit;                                      }
{ Read method property ACE                                                     }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetAce: TRegKeyAudit;
 Begin
    Result := FFiler.Ace;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetAttributes(Const lng: Cardinal): TAuditFlagValue;       }
{ Generic Read attibutes method                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetAttributes(Const lng: Cardinal): TAuditFlagValue;
 Begin
    Result := TAuditFlagValue(FFiler.GetAttributes(lng));
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.SetAttributes(...);                                        }
{ Generic Write attibutes method                                               }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.SetAttributes(Const lng: Cardinal; Const Value: TAuditFlagValue);
 Begin
    FFiler.SetAttributes(lng, Cardinal(Value));
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetDelete: TAuditFlagValue;                                }
{ Read method property CanDelete                                               }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetDelete: TAuditFlagValue;
 Begin
    Result := GetAttributes(_DELETE);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.SetDelete(Const Value: TAuditFlagValue);                   }
{ Write method property CanDelete                                              }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.SetDelete(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(_DELETE, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetReadControl: TAuditFlagValue;                           }
{ Read method property ReadControl                                             }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetReadControl: TAuditFlagValue;
 Begin
    Result := GetAttributes(READ_CONTROL);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.SetReadControl(Const Value: TAuditFlagValue);              }
{ Write method property ReadControl                                            }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.SetReadControl(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(READ_CONTROL, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetWriteDac: TAuditFlagValue;                              }
{ Read method property WriteDac                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetWriteDac: TAuditFlagValue;
 Begin
    Result := GetAttributes(WRITE_DAC);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.SetWriteDac(Const Value: TAuditFlagValue);                 }
{ Write method property Write Dac                                              }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.SetWriteDac(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(WRITE_DAC, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetWriteOwner: TAuditFlagValue;                            }
{ Read method property WriteOwner                                              }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetWriteOwner: TAuditFlagValue;
 Begin
    Result := GetAttributes(WRITE_OWNER);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.SetWriteOwner(Const Value: TAuditFlagValue);               }
{ Write method property WriteOwner                                             }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.SetWriteOwner(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(WRITE_OWNER, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetGetValue: TAuditFlagValue;                              }
{ Read method property GetValue                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetGetValue: TAuditFlagValue;
 Begin
    Result := GetAttributes(KEY_QUERY_VALUE);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.SetGetValue(Const Value: TAuditFlagValue);                 }
{ Write method property GetValue                                               }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.SetGetValue(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(KEY_QUERY_VALUE, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetSetValue: TAuditFlagValue;                              }
{ Read method property SetValue                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetSetValue: TAuditFlagValue;
 Begin
    Result := GetAttributes(KEY_SET_VALUE);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.SetSetValue(Const Value: TAuditFlagValue);                 }
{ Write method property SetValue                                               }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.SetSetValue(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(KEY_SET_VALUE, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetCreateSubKey: TAuditFlagValue;                          }
{ Read method property CreateSubKey                                            }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetCreateSubKey: TAuditFlagValue;
 Begin
    Result := GetAttributes(KEY_CREATE_SUB_KEY);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.SetCreateSubKey(Const Value: TAuditFlagValue);             }
{ Write method property CreateSubKey                                           }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.SetCreateSubKey(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(KEY_CREATE_SUB_KEY, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetEnumerateKey: TAuditFlagValue;                          }
{ Read method property EnumerateKey                                            }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetEnumerateKey: TAuditFlagValue;
 Begin
    Result := GetAttributes(KEY_ENUMERATE_SUB_KEYS);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.SetEnumerateKey(Const Value: TAuditFlagValue);             }
{ Write method property EnumerateKey                                           }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.SetEnumerateKey(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(KEY_ENUMERATE_SUB_KEYS, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetNotify: TAuditFlagValue;                                }
{ Read method property Notify                                                  }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetNotify: TAuditFlagValue;
 Begin
    Result := GetAttributes(KEY_NOTIFY);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.SetNotify(Const Value: TAuditFlagValue);                   }
{ Write method property Notify                                                 }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.SetNotify(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(KEY_NOTIFY, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetCreateLink: TAuditFlagValue;                            }
{ Read method property CreateLink                                              }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetCreateLink: TAuditFlagValue;
 Begin
    Result := GetAttributes(KEY_CREATE_LINK);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.SetCreateLink(Const Value: TAuditFlagValue);               }
{ Write method property CreateLink                                             }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.SetCreateLink(Const Value: TAuditFlagValue);
 Begin
    SetAttributes(KEY_CREATE_LINK, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.Assign(Source: TPersistent);                               }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TRegKeyAccessFlags) Then
       With Source As TRegKeyAccessFlags Do Begin
          Self.AuditDelete           := AuditDelete;
          Self.AuditReadControl      := AuditReadControl;
          Self.AuditWriteDac         := AuditWriteDac;
          Self.AuditWriteOwner       := AuditWriteOwner;
          Self.AuditGetValue         := AuditGetValue;
          Self.AuditSetValue         := AuditSetValue;
          Self.AuditCreateSubKey     := AuditCreateSubKey;
          Self.AuditEnumerateSubKeys := AuditEnumerateSubKeys;
          Self.AuditNotify           := AuditNotify;
          Self.AuditCreateLink       := AuditCreateLink;
       End;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.GetFlagsMode: TNTRegFlagsMode;                             }
{ Read method property FlagsMode                                               }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlags.GetFlagsMode: TNTRegFlagsMode;
 Begin
    Result := rfmCustom;
    If ((AuditDelete=faSucceededFailed) And
       (AuditReadControl=faSucceededFailed) And
       (AuditWriteDac=faSucceededFailed) And
       (AuditWriteOwner=faSucceededFailed) And
       (AuditGetValue=faSucceededFailed) And
       (AuditSetValue=faSucceededFailed) And
       (AuditCreateSubKey=faSucceededFailed) And
       (AuditEnumerateSubKeys=faSucceededFailed) And
       (AuditNotify=faSucceededFailed) And
       (AuditCreateLink=faSucceededFailed)) Then
       Result := rfmAll
    Else If ((AuditDelete=faNone) And
       (AuditReadControl=faSucceededFailed) And
       (AuditWriteDac=faNone) And
       (AuditWriteOwner=faNone) And
       (AuditGetValue=faSucceededFailed) And
       (AuditSetValue=faNone) And
       (AuditCreateSubKey=faNone) And
       (AuditEnumerateSubKeys=faSucceededFailed) And
       (AuditNotify=faSucceededFailed) And
       (AuditCreateLink=faNone)) Then
       Result := rfmRead
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlags.SetFlagsMode(Const Value: TNTRegFlagsMode);                }
{ Write method property FlagsMode                                              }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlags.SetFlagsMode(Const Value: TNTRegFlagsMode);
 Begin
    If Ace.IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    Case Value Of
      rfmRead:
        Begin
           AuditDelete           := faNone;
           AuditReadControl      := faSucceededFailed;
           AuditWriteDac         := faNone;
           AuditWriteOwner       := faNone;
           AuditGetValue         := faSucceededFailed;
           AuditSetValue         := faNone;
           AuditCreateSubKey     := faNone;
           AuditEnumerateSubKeys := faSucceededFailed;
           AuditNotify           := faSucceededFailed;
           AuditCreateLink       := faNone;
        End;
      rfmAll:
        Begin
           AuditDelete           := faSucceededFailed;
           AuditReadControl      := faSucceededFailed;
           AuditWriteDac         := faSucceededFailed;
           AuditWriteOwner       := faSucceededFailed;
           AuditGetValue         := faSucceededFailed;
           AuditSetValue         := faSucceededFailed;
           AuditCreateSubKey     := faSucceededFailed;
           AuditEnumerateSubKeys := faSucceededFailed;
           AuditNotify           := faSucceededFailed;
           AuditCreateLink       := faSucceededFailed;
        End;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegKeySecurityCheck                                          }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeySecurityCheck.Create(AOwner: TComponent);                             }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TRegKeySecurityCheck.Create(AOwner: TComponent);
 Begin
    Inherited Create(AOwner);
    FDesiredFiler  := TRegKeyDesiredCheckFlagsFiler.Create(Self);
    FGrantedFiler  := TRegKeyGrantedCheckFlagsFiler.Create(Self);
    FDesiredChecks := TRegKeyCheckFlags.Create(FDesiredFiler, False);
    FGrantedChecks := TRegKeyCheckFlags.Create(FGrantedFiler, True);
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurityCheck.Destroy;                                                }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TRegKeySecurityCheck.Destroy;
 Begin
    FDesiredChecks.Free;
    FGrantedChecks.Free;
    FDesiredFiler.Free;
    FGrantedFiler.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurityCheck.GetSecurityObj: TRegKeySecurity;                        }
{ Read Method property SecurityObject                                          }
{------------------------------------------------------------------------------}
 Function TRegKeySecurityCheck.GetSecurityObj: TRegKeySecurity;
 Begin
    Result := (Inherited SecurityObject) As TRegKeySecurity;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurityCheck.SetSecurityObj(Const Value: TRegKeySecurity);           }
{ Write Method property SecurityObject                                         }
{------------------------------------------------------------------------------}
 Procedure TRegKeySecurityCheck.SetSecurityObj(Const Value: TRegKeySecurity);
 Begin
    Inherited SecurityObject := Value;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurityCheck.GetDesiredChecks: TRegKeyCheckFlags;                    }
{ Read Method property DesiredChecks                                           }
{------------------------------------------------------------------------------}
 Function TRegKeySecurityCheck.GetDesiredChecks: TRegKeyCheckFlags;
 Begin
    Result := FDesiredChecks;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurityCheck.SetDesiredChecks(Const Value: TRegKeyCheckFlags);       }
{ Write Method property DesiredChecks                                          }
{------------------------------------------------------------------------------}
 Procedure TRegKeySecurityCheck.SetDesiredChecks(Const Value: TRegKeyCheckFlags);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurityCheck.GetGrantedChecks: TRegKeyCheckFlags;                    }
{ Read Method property GrantedChecks                                           }
{------------------------------------------------------------------------------}
 Function TRegKeySecurityCheck.GetGrantedChecks: TRegKeyCheckFlags;
 Begin
    Result := FGrantedChecks;
 End;

{------------------------------------------------------------------------------}
{ TRegKeySecurityCheck.SetGrantedChecks(Const Value: TRegKeyCheckFlags);       }
{ Write Method property GrantedChecks                                          }
{------------------------------------------------------------------------------}
 Procedure TRegKeySecurityCheck.SetGrantedChecks(Const Value: TRegKeyCheckFlags);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegKeyCheckFlags                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.Create(...);                                               }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TRegKeyCheckFlags.Create(Filer: TRegKeyFlagsFiler; Const RO: Boolean);
 Begin
    Inherited Create;
    FReadOnly := RO;
    FFiler := Filer;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.Destroy;                                                   }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TRegKeyCheckFlags.Destroy;
 Begin
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.GetAttributes(...): TCheckFlagValue;                       }
{ Generic Read attibutes method                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyCheckFlags.GetAttributes(Const lng: Cardinal): TCheckFlagValue;
 Begin
    Result := TCheckFlagValue(FFiler.GetAttributes(lng));
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.SetAttributes(...);                                        }
{ Generic Write attibutes method                                               }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.SetAttributes(Const lng: Cardinal; Const Value: TCheckFlagValue);
 Begin
    FFiler.SetAttributes(lng, Cardinal(Value));
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.GetDelete: TCheckFlagValue;                                }
{ Read method property CanDelete                                               }
{------------------------------------------------------------------------------}
 Function TRegKeyCheckFlags.GetDelete: TCheckFlagValue;
 Begin
    Result := GetAttributes(_DELETE);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.SetDelete(Const Value: TCheckFlagValue);                   }
{ Write method property CanDelete                                              }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.SetDelete(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(_DELETE, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.GetReadControl: TCheckFlagValue;                           }
{ Read method property ReadControl                                             }
{------------------------------------------------------------------------------}
 Function TRegKeyCheckFlags.GetReadControl: TCheckFlagValue;
 Begin
    Result := GetAttributes(READ_CONTROL);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.SetReadControl(Const Value: TCheckFlagValue);              }
{ Write method property ReadControl                                            }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.SetReadControl(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(READ_CONTROL, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.GetWriteDac: TCheckFlagValue;                              }
{ Read method property WriteDac                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyCheckFlags.GetWriteDac: TCheckFlagValue;
 Begin
    Result := GetAttributes(WRITE_DAC);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.SetWriteDac(Const Value: TCheckFlagValue);                 }
{ Write method property Write Dac                                              }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.SetWriteDac(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(WRITE_DAC, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.GetWriteOwner: TCheckFlagValue;                            }
{ Read method property WriteOwner                                              }
{------------------------------------------------------------------------------}
 Function TRegKeyCheckFlags.GetWriteOwner: TCheckFlagValue;
 Begin
    Result := GetAttributes(WRITE_OWNER);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.SetWriteOwner(Const Value: TCheckFlagValue);               }
{ Write method property WriteOwner                                             }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.SetWriteOwner(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(WRITE_OWNER, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.GetGetValue: TCheckFlagValue;                              }
{ Read method property CanGetValue                                             }
{------------------------------------------------------------------------------}
 Function TRegKeyCheckFlags.GetGetValue: TCheckFlagValue;
 Begin
    Result := GetAttributes(KEY_QUERY_VALUE);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.SetGetValue(Const Value: TCheckFlagValue);                 }
{ Write method property CanGetValue                                            }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.SetGetValue(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(KEY_QUERY_VALUE, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.GetGetValue: TCheckFlagValue;                              }
{ Read method property CanSetValue                                             }
{------------------------------------------------------------------------------}
 Function TRegKeyCheckFlags.GetSetValue: TCheckFlagValue;
 Begin
    Result := GetAttributes(KEY_SET_VALUE);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.SetSetValue(Const Value: TCheckFlagValue);                 }
{ Write method property CanSetValue                                            }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.SetSetValue(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(KEY_SET_VALUE, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.GetCreateSubKey: TCheckFlagValue;                          }
{ Read method property CanCreateSubKey                                         }
{------------------------------------------------------------------------------}
 Function TRegKeyCheckFlags.GetCreateSubKey: TCheckFlagValue;
 Begin
    Result := GetAttributes(KEY_CREATE_SUB_KEY);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.SetCreateSubKey(Const Value: TCheckFlagValue);             }
{ Write method property CanCreateSubKey                                        }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.SetCreateSubKey(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(KEY_CREATE_SUB_KEY, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.GetEnumerateKey: TCheckFlagValue;                          }
{ Read method property CanEnumerateKey                                         }
{------------------------------------------------------------------------------}
 Function TRegKeyCheckFlags.GetEnumerateKey: TCheckFlagValue;
 Begin
    Result := GetAttributes(KEY_ENUMERATE_SUB_KEYS);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.SetCreateSubKey(Const Value: TCheckFlagValue);             }
{ Write method property CanEnumerateKey                                        }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.SetEnumerateKey(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(KEY_ENUMERATE_SUB_KEYS, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.GetNotify: TCheckFlagValue;                                }
{ Read method property CanNotify                                               }
{------------------------------------------------------------------------------}
 Function TRegKeyCheckFlags.GetNotify: TCheckFlagValue;
 Begin
    Result := GetAttributes(KEY_NOTIFY);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.SetNotify(Const Value: TCheckFlagValue);                   }
{ Write method property CanNotify                                              }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.SetNotify(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(KEY_NOTIFY, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.GetCreateLink: TCheckFlagValue;                            }
{ Read method property CanCreateLink                                           }
{------------------------------------------------------------------------------}
 Function TRegKeyCheckFlags.GetCreateLink: TCheckFlagValue;
 Begin
    Result := GetAttributes(KEY_CREATE_LINK);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.SetCreateLink(Const Value: TCheckFlagValue);               }
{ Write method property CanCreateLink                                          }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.SetCreateLink(Const Value: TCheckFlagValue);
 Begin
    SetAttributes(KEY_CREATE_LINK, Value);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.Assign(Source: TPersistent);                               }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TRegKeyCheckFlags) Then
       With Source As TRegKeyCheckFlags Do Begin
          Self.CanDelete           := CanDelete;
          Self.CanReadControl      := CanReadControl;
          Self.CanWriteDac         := CanWriteDac;
          Self.CanWriteOwner       := CanWriteOwner;
          Self.CanGetValue         := CanGetValue;
          Self.CanSetValue         := CanSetValue;
          Self.CanCreateSubKey     := CanCreateSubKey;
          Self.CanEnumerateSubKeys := CanEnumerateSubKeys;
          Self.CanNotify           := CanNotify;
          Self.CanCreateLink       := CanCreateLink;
       End;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.GetFlagsMode: TNTRegFlagsMode;                             }
{ Read method property FlagsMode                                               }
{------------------------------------------------------------------------------}
 Function TRegKeyCheckFlags.GetFlagsMode: TNTRegFlagsMode;
 Begin
    Result := rfmCustom;
    If ((CanDelete=fcAllowed) And
       (CanReadControl=fcAllowed) And
       (CanWriteDac=fcAllowed) And
       (CanWriteOwner=fcAllowed) And
       (CanGetValue=fcAllowed) And
       (CanSetValue=fcAllowed) And
       (CanCreateSubKey=fcAllowed) And
       (CanEnumerateSubKeys=fcAllowed) And
       (CanNotify=fcAllowed) And
       (CanCreateLink=fcAllowed)) Then
       Result := rfmAll
    Else If ((CanDelete=fcDenied) And
       (CanReadControl=fcAllowed) And
       (CanWriteDac=fcDenied) And
       (CanWriteOwner=fcDenied) And
       (CanGetValue=fcAllowed) And
       (CanSetValue=fcDenied) And
       (CanCreateSubKey=fcDenied) And
       (CanEnumerateSubKeys=fcAllowed) And
       (CanNotify=fcAllowed) And
       (CanCreateLink=fcDenied)) Then
       Result := rfmRead;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyCheckFlags.SetFlagsMode(Const Value: TNTRegFlagsMode);                }
{ Write method property FlagsMode                                              }
{------------------------------------------------------------------------------}
 Procedure TRegKeyCheckFlags.SetFlagsMode(Const Value: TNTRegFlagsMode);
 Begin
    Case Value Of
      rfmRead:
        Begin
           CanDelete           := fcDenied;
           CanReadControl      := fcAllowed;
           CanWriteDac         := fcDenied;
           CanWriteOwner       := fcDenied;
           CanGetValue         := fcAllowed;
           CanSetValue         := fcDenied;
           CanCreateSubKey     := fcDenied;
           CanEnumerateSubKeys := fcAllowed;
           CanNotify           := fcAllowed;
           CanCreateLink       := fcDenied;
        End;
      rfmAll:
        Begin
           CanDelete           := fcAllowed;
           CanReadControl      := fcAllowed;
           CanWriteDac         := fcAllowed;
           CanWriteOwner       := fcAllowed;
           CanGetValue         := fcAllowed;
           CanSetValue         := fcAllowed;
           CanCreateSubKey     := fcAllowed;
           CanEnumerateSubKeys := fcAllowed;
           CanNotify           := fcAllowed;
           CanCreateLink       := fcAllowed;
        End;
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegKeyAccessFlagsFiler                                       }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeyAccessFlagsFiler.Create(Ace: TRegKeyAccess);                          }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TRegKeyAccessFlagsFiler.Create(Ace: TRegKeyAccess);
 Begin
    Inherited Create;
    FAce := Ace;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlagsFiler.GetAttributes(...): Cardinal;                        }
{ GetAttributes method override                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyAccessFlagsFiler.GetAttributes(Const lng: Cardinal): Cardinal;
 Begin
    Result := Cardinal(fvNone);
    If ((FAce.MaskFlags[fkYes] And lng)=lng) Then
       Result := Cardinal(fvAllowed);
    If ((FAce.MaskFlags[fkNo] And lng)=lng) Then
       Result := Cardinal(fvDenied);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAccessFlagsFiler.SetAttributes(...);                                  }
{ SetAttibutes method override                                                 }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAccessFlagsFiler.SetAttributes(Const lng: Cardinal; Const Value: Cardinal);
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
{ Implementation TRegKeyAuditFlagsFiler                                        }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeyAuditFlagsFiler.Create(Ace: TRegKeyAudit);                            }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TRegKeyAuditFlagsFiler.Create(Ace: TRegKeyAudit);
 Begin
    Inherited Create;
    FAce := Ace;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlagsFiler.GetAttributes(...): Cardinal;                         }
{ GetAttributes method override                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyAuditFlagsFiler.GetAttributes(Const lng: Cardinal): Cardinal;
 Var
    P: Integer;
 Begin
    P := 0;
    If ((FAce.MaskFlags[fkYes] And lng)=lng) Then
       P := P Or 1;
    If ((FAce.MaskFlags[fkNo] And lng)=lng) Then
       P := P Or 2;
    Case P Of
       1: Result := Cardinal(faSucceeded);
       2: Result := Cardinal(faFailed);
       3: Result := Cardinal(faSucceededFailed);
    Else
       Result := Cardinal(faNone);
    End;
 End;

{------------------------------------------------------------------------------}
{ TRegKeyAuditFlagsFiler.SetAttributes(...);                                   }
{ SetAttibutes method override                                                 }
{------------------------------------------------------------------------------}
 Procedure TRegKeyAuditFlagsFiler.SetAttributes(Const lng: Cardinal; Const Value: Cardinal);
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
{ Implementation TRegKeyCheckFlagsFiler                                        }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeyCheckFlagsFiler.Create(aCheck: TRegKeySecurityCheck);                 }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TRegKeyCheckFlagsFiler.Create(aCheck: TRegKeySecurityCheck);
 Begin
    Inherited Create;
    FCheck := aCheck;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegKeyDesiredCheckFlagsFiler                                 }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeyDesiredCheckFlagsFiler.GetAttributes(...): Cardinal;                  }
{ GetAttributes method override                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyDesiredCheckFlagsFiler.GetAttributes(Const lng: Cardinal): Cardinal;
 Begin
    Result := Cardinal(fcDenied);
    If ((FCheck.DesiredAccess And lng)=lng) Then
       Result := Cardinal(fcAllowed);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyDesiredCheckFlagsFiler.SetAttributes(...);                            }
{ SetAttibutes method override                                                 }
{------------------------------------------------------------------------------}
 Procedure TRegKeyDesiredCheckFlagsFiler.SetAttributes(Const lng: Cardinal; Const Value: Cardinal);
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
{ Implementation TRegKeyGrantedCheckFlagsFiler                                 }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegKeyGrantedCheckFlagsFiler.GetAttributes(...): Cardinal;                  }
{ GetAttributes method override                                                }
{------------------------------------------------------------------------------}
 Function TRegKeyGrantedCheckFlagsFiler.GetAttributes(Const lng: Cardinal): Cardinal;
 Begin
    Result := Cardinal(fcDenied);
    If ((FCheck.GrantedAccess And lng)=lng) Then
       Result := Cardinal(fcAllowed);
 End;

{------------------------------------------------------------------------------}
{ TRegKeyGrantedCheckFlagsFiler.SetAttributes(...);                            }
{ SetAttibutes method override                                                 }
{------------------------------------------------------------------------------}
 Procedure TRegKeyGrantedCheckFlagsFiler.SetAttributes(Const lng: Cardinal; Const Value: Cardinal);
 Begin
    Raise Exception.Create(SNotAllowed);
 End;

{------------------------------------------------------------------------------}
{ End of aclregistry.pas                                                       }
{------------------------------------------------------------------------------}
 End.
