{------------------------------------------------------------------------------}
{ aclfuncs                                                                     }
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
 Unit aclfuncs;

{==============================================================================}
{                                                                              }
{                                                                              }
{ I N T E R F A C E                                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
 Interface

{------------------------------------------------------------------------------}
{ Uses clauses                                                                 }
{------------------------------------------------------------------------------}
 Uses
    Windows, SysUtils, aclconst;

{------------------------------------------------------------------------------}
{ public funcs                                                                 }
{------------------------------------------------------------------------------}
 Function AceFlagsToAces(Const Value: Byte): TAceFlagTypes;
 Function AcesToAceFlags(Const Value: TAceFlagTypes): Byte;
 Function SidToString(Const Value: PSid): String;
 Function GetPrimaryDCComputer(Const SystemName: String): String;
 Function GetDefaultDomainSID(Const ComputerName: String): PSid;

 Function CreateWellKnownSID(Const domain: PSid; Const Kind: TWellKnownSid): PSid;
 Function IsWellKnownSID(Const domain: PSid; Const asid: PSid; Var wns: TWellKnownSid): Boolean;

 Procedure FreeAndNilSecurityDescriptor(Var asd: PSecurityDescriptor);
 Procedure FreeAndNilACL(Var aacl: PAcl);
 Procedure FreeAndNilACE(Var aace: PAce);
 Procedure FreeAndNilSID(Var asid: PSid);

 Function  sdAlloc(Size: Integer): Pointer;
 Procedure sdFree(ptr: Pointer);                                       Overload;
 Procedure sdFree(ptr: Pointer; Size: Integer);                        Overload;
 Procedure RaiseACLError;                                              Overload;
 Procedure RaiseACLError(ErrorCode: DWORD);                            Overload;

{==============================================================================}
{                                                                              }
{                                                                              }
{ I M P L E M E N T A T I O N                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
 Implementation

{------------------------------------------------------------------------------}
{ Privates Uses                                                                }
{------------------------------------------------------------------------------}
 Uses
    ntlmapi, Dialogs;

{------------------------------------------------------------------------------}
{ Private Types                                                                }
{------------------------------------------------------------------------------}
 Type
    EACLError = Class(Exception)
    Public
      Constructor Create(ErrorCode: DWORD);                                     Reintroduce;
    End;

{------------------------------------------------------------------------------}
{ AceFlagsToAces(Const Value: Byte): TAceFlagTypes;                            }
{------------------------------------------------------------------------------}
 Function AceFlagsToAces(Const Value: Byte): TAceFlagTypes;
 Begin
    Result := [];
    If ((OBJECT_INHERIT_ACE And Value)=OBJECT_INHERIT_ACE) Then
       Include(Result, afObjectInherit);
    If ((CONTAINER_INHERIT_ACE And Value)=CONTAINER_INHERIT_ACE) Then
       Include(Result, afContainerInherit);
    If ((NO_PROPAGATE_INHERIT_ACE And Value)=NO_PROPAGATE_INHERIT_ACE) Then
       Include(Result, afNoPropagate);
    If ((INHERIT_ONLY_ACE And Value)=INHERIT_ONLY_ACE) Then
       Include(Result, afInheritOnly);
    If ((INHERITED_ACE And Value)=INHERITED_ACE) Then
       Include(Result, afInherited);
    If ((VALID_INHERIT_FLAGS And Value)=VALID_INHERIT_FLAGS) Then
       Include(Result, afValidInheritFlags);
    If ((SUCCESSFUL_ACCESS_ACE_FLAG And Value)=SUCCESSFUL_ACCESS_ACE_FLAG) Then
       Include(Result, afSuccessfullAuditFlag);
    If ((FAILED_ACCESS_ACE_FLAG And Value)=FAILED_ACCESS_ACE_FLAG) Then
       Include(Result, afFailedAuditFlag);
 End;

{------------------------------------------------------------------------------}
{ AcesToAceFlags(Const Value: TAceFlagTypes): Byte;                            }
{------------------------------------------------------------------------------}
 Function AcesToAceFlags(Const Value: TAceFlagTypes): Byte;
 Begin
    Result := 0;
    If (afObjectInherit In Value) Then
       Result := Result Or OBJECT_INHERIT_ACE;
    If (afContainerInherit In Value) Then
       Result := Result Or CONTAINER_INHERIT_ACE;
    If (afNoPropagate In Value) Then
       Result := Result Or NO_PROPAGATE_INHERIT_ACE;
    If (afInheritOnly In Value) Then
       Result := Result Or INHERIT_ONLY_ACE ;
    If (afInherited In Value) Then
       Result := Result Or INHERITED_ACE;
    If (afValidInheritFlags In Value) Then
       Result := Result Or VALID_INHERIT_FLAGS;
    If (afSuccessfullAuditFlag In Value) Then
       Result := Result Or SUCCESSFUL_ACCESS_ACE_FLAG;
    If (afFailedAuditFlag In Value) Then
       Result := Result Or FAILED_ACCESS_ACE_FLAG;
 End;

{------------------------------------------------------------------------------}
{ SidToString(Const Value: PSid): String;                                      }
{------------------------------------------------------------------------------}
 Function SidToString(Const Value: PSid): String;
 Const
    SID_REVISION = 1;
 Var
    psia: PSIDIdentifierAuthority;
    cnt: DWORD;
    I: Cardinal;
 Begin
    Result := '';
    If Not IsValidSid(Value) Then Exit;
    psia := GetSidIdentifierAuthority(Value);
    cnt := PByte(GetSidSubAuthorityCount(Value))^;
    Result := Format('S-%u-%u', [SID_REVISION, psia^.Value[5]]);
    For I := 0 To cnt-1 Do
       Result := Result+Format('-%u', [PLongint(GetSidSubAuthority(Value, I))^]);
 End;

{------------------------------------------------------------------------------}
{ GetPrimaryDCComputer(Const SystemName: String): String;                      }
{------------------------------------------------------------------------------}
 Function GetPrimaryDCComputer(Const SystemName: String): String;
 Var
    dwError: DWORD;
    pbb: PByte;
    pwc: PWideChar Absolute pbb;
 Begin
    pbb := Nil;
    If (SystemName<>'') Then
       dwError := NetGetDCName(StringToOleStr('\\'+SystemName), Nil, pbb)
    Else
       dwError := NetGetDCName(Nil, Nil, pbb);
    If (dwError=NERR_SUCCESS) Then
       Begin
          Result := pwc;
          NetApiBufferFree(pbb);
       End
    Else
       Begin
          Result := '';
          SetLastError(0);
       End;
 End;

{------------------------------------------------------------------------------}
{ GetDefaultDomainSID(...): PSid;                                              }
{------------------------------------------------------------------------------}
 Function GetDefaultDomainSID(Const ComputerName: String): PSid;
 Var
    dwError: DWORD;
    pu: PUserModalsInfo2;
    pb: PByte Absolute pu;
    pbb: PByte;
    pwc: PWideChar Absolute pbb;
    dwSize: DWORD;
    domain: String;
 Begin
    pbb := Nil;
    Result := Nil;
    Try
       domain := GetPrimaryDCComputer(ComputerName);
       dwError := NetUserModalsGet(StringToOleStr(domain), 2, pb);
       If (dwError<>NERR_SUCCESS) Then
          RaiseACLError(dwError);
       Try
          dwsize := GetLengthSid(pu^.usrmod2_domain_id);
          If (dwSize>0) Then Begin
             Result := sdAlloc(dwSize);
             Try
                CopySid(dwsize, Result, pu^.usrmod2_domain_id);
             Except
                FreeandNilSid(Result);
                Raise;
             End;
          End;
       Finally
          NetApiBufferFree(pb);
       End;
    Finally
       NetApiBufferFree(pbb);
    End;
 End;

{------------------------------------------------------------------------------}
{ CreateWellKnownSID(...): PSid;                                               }
{------------------------------------------------------------------------------}
 Function GetWellKnownSIDFromRID(Const DomainSid: PSid; RID: Integer): PSid;
 Var
    I: Integer;
    cnt: DWORD;
    dwSize: DWORD;
    dst, src: PLongint;
 Begin
    Result := Nil;
    If (DomainSid=Nil) Then Exit;
    cnt := PByte(GetSidSubAuthorityCount(DomainSid))^;
    dwSize := GetSidLengthRequired(cnt+1);
    If (dwSize>0) Then Begin
       Result := sdAlloc(dwSize);
       Try
          If Not InitializeSid(Result, GetSidIdentifierAuthority(DomainSid)^, cnt+1) Then
             RaiseACLError;
          For I := 0 To cnt-1 Do Begin
             dst := PLongint(GetSidSubAuthority(Result, I));
             src := PLongint(GetSidSubAuthority(DomainSid, I));
             Move(src^, dst^, 4);
          End;
          dst := PLongint(GetSidSubAuthority(Result, cnt));
          Move(RID, dst^, 4);
       Except
          FreeAndNilSid(Result);
          Raise;
       End;
    End;
 End;

{------------------------------------------------------------------------------}
{ CreateWellKnownSID(...): PSid;                                               }
{------------------------------------------------------------------------------}
 Function CreateWellKnownSID(Const domain: PSid; Const Kind: TWellKnownSid): PSid;
 Var
    sia: TSidIdentifierAuthority;
 Begin
    ZeroMemory(@sia, SizeOf(sia));
    Case Kind Of
       wnsNull:
         Begin
            sia.Value[5] := SECURITY_NULL_SID_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_NULL_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsWorld:
         Begin
            sia.Value[5] := SECURITY_WORLD_SID_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_WORLD_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsLocal:
         Begin
            sia.Value[5] := SECURITY_LOCAL_SID_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_LOCAL_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsCreatorOwner:
         Begin
            sia.Value[5] := SECURITY_CREATOR_SID_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_CREATOR_OWNER_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsCreatorGroup:
         Begin
            sia.Value[5] := SECURITY_CREATOR_SID_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_CREATOR_GROUP_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsCreatorOwnerServer:
         Begin
            sia.Value[5] := SECURITY_CREATOR_SID_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_CREATOR_OWNER_SERVER_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsCreatorGroupServer:
         Begin
            sia.Value[5] := SECURITY_CREATOR_SID_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_CREATOR_GROUP_SERVER_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsDialup:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_DIALUP_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsNetwork:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_NETWORK_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsBatch:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_BATCH_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsInteractive:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_INTERACTIVE_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsService:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_SERVICE_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsAnonymous:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_ANONYMOUS_LOGON_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsProxy:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_PROXY_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsEnterpriseControllers:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_ENTERPRISE_CONTROLLERS_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsSelf:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_PRINCIPAL_SELF_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsAuthenticatedUser:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_AUTHENTICATED_USER_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsRestrictedCode:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_RESTRICTED_CODE_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsTerminalServer:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_TERMINAL_SERVER_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsWinLocalSystem:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 1, SECURITY_LOCAL_SYSTEM_RID, 0, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsBuiltInAdministrators:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsBuiltInUsers:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_USERS, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsBuiltInGuests:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_GUESTS, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsBuiltInPowerUsers:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_POWER_USERS, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsBuiltInAccountOperators:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ACCOUNT_OPS, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsBuiltInSystemOperators:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_SYSTEM_OPS, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsBuiltInPrintOperators:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_PRINT_OPS, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsBuiltInBackupOperators:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_BACKUP_OPS, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsBuiltInReplicator:
         Begin
            sia.Value[5] := SECURITY_NT_AUTHORITY;
            AllocateAndInitializeSid(sia, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_REPLICATOR, 0, 0, 0, 0, 0, 0, Result);
         End;
       wnsDomainAdministrator:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_USER_RID_ADMIN);
       wnsDomainGuest:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_USER_RID_GUEST);
       wnsDomainKrbtgt:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_USER_RID_KRBTGT);
       wnsDomainAdministrators:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_GROUP_RID_ADMINS);
       wnsDomainUsers:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_GROUP_RID_USERS);
       wnsDomainGuests:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_GROUP_RID_GUESTS);
       wnsDomainComputers:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_GROUP_RID_COMPUTERS);
       wnsDomainControllers:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_GROUP_RID_CONTROLLERS);
       wnsDomainCertAdmins:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_GROUP_RID_CERT_ADMINS);
       wnsDomainSchemaAdmins:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_GROUP_RID_SCHEMA_ADMINS);
       wnsDomainEnterpriseAdmins:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_GROUP_RID_SCHEMA_ADMINS+1);
       wnsDomainPolicyAdmins:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_GROUP_RID_SCHEMA_ADMINS+2);
       wnsDomainRasAndIasServers:
         Result := GetWellKnownSIDFromRID(domain, DOMAIN_GROUP_RID_SCHEMA_ADMINS+3);
    End;
 End;

{------------------------------------------------------------------------------}
{ IsWellKnownSID(...): Boolean;                                                }
{------------------------------------------------------------------------------}
 Function IsWellKnownSID(Const domain: PSid; Const asid: PSid; Var wns: TWellKnownSid): Boolean;
 Var
    psia: PSIDIdentifierAuthority;
    cnt : DWORD;
    adom: PSid;
 Begin
    Result := True;
    wns := wnsCustom;
    If Not IsValidSid(asid) Then Begin
       wns := wnsError;
       Result := False;
       Exit;
    End;
    psia := GetSidIdentifierAuthority(asid);
    cnt := PByte(GetSidSubAuthorityCount(asid))^;
    Case psia^.Value[5] Of
       SECURITY_NULL_SID_AUTHORITY:
          Begin
             Result := True;
             wns    := wnsNull;
          End;
       SECURITY_WORLD_SID_AUTHORITY:
          Begin
             Result := True;
             wns    := wnsWorld;
          End;
       SECURITY_LOCAL_SID_AUTHORITY:
          Begin
             Result := True;
             wns    := wnsLocal;
          End;
       SECURITY_CREATOR_SID_AUTHORITY:
          Case PLongint(GetSidSubAuthority(asid, 0))^ Of
             SECURITY_CREATOR_OWNER_RID:
                Begin
                   Result := True;
                   wns := wnsCreatorOwner;
                End;
             SECURITY_CREATOR_GROUP_RID:
                Begin
                   Result := True;
                   wns := wnsCreatorGroup;
                End;
             SECURITY_CREATOR_OWNER_SERVER_RID:
                Begin
                   Result := True;
                   wns := wnsCreatorOwnerServer;
                End;
             SECURITY_CREATOR_GROUP_SERVER_RID:
                Begin
                   Result := True;
                   wns := wnsCreatorGroupServer;
                End;
          End;
    SECURITY_NT_AUTHORITY:
       Case PLongint(GetSidSubAuthority(asid, 0))^ Of
          SECURITY_DIALUP_RID:
             Begin
                Result := True;
                wns := wnsDialup;
             End;
          SECURITY_NETWORK_RID:
             Begin
                Result := True;
                wns := wnsNetwork;
             End;
          SECURITY_BATCH_RID:
             Begin
                Result := True;
                wns := wnsBatch;
             End;
          SECURITY_INTERACTIVE_RID:
             Begin
                Result := True;
                wns := wnsInteractive;
             End;
          SECURITY_SERVICE_RID:
             Begin
                Result := True;
                wns := wnsService;
             End;
          SECURITY_ANONYMOUS_LOGON_RID:
             Begin
                Result := True;
                wns := wnsAnonymous;
             End;
          SECURITY_PROXY_RID:
             Begin
                Result := True;
                wns := wnsProxy;
             End;
          SECURITY_ENTERPRISE_CONTROLLERS_RID:
             Begin
                Result := True;
                wns := wnsEnterpriseControllers;
             End;
          SECURITY_PRINCIPAL_SELF_RID:
             Begin
                Result := True;
                wns := wnsSelf;
             End;
          SECURITY_AUTHENTICATED_USER_RID:
             Begin
                Result := True;
                wns := wnsAuthenticatedUser;
             End;
          SECURITY_RESTRICTED_CODE_RID:
             Begin
                Result := True;
                wns := wnsRestrictedCode;
             End;
          SECURITY_TERMINAL_SERVER_RID:
             Begin
                Result := True;
                wns := wnsTerminalServer;
             End;
          SECURITY_LOCAL_SYSTEM_RID:
             Begin
                Result := True;
                wns := wnsWinLocalSystem;
             End;
          SECURITY_BUILTIN_DOMAIN_RID:
             Case PLongint(GetSidSubAuthority(asid, 1))^ Of
               DOMAIN_ALIAS_RID_ADMINS:
                  Begin
                     Result := True;
                     wns := wnsBuiltInAdministrators;
                  End;
               DOMAIN_ALIAS_RID_USERS:
                  Begin
                     Result := True;
                     wns := wnsBuiltInUsers;
                  End;
               DOMAIN_ALIAS_RID_GUESTS:
                  Begin
                     Result := True;
                     wns := wnsBuiltInGuests;
                  End;
               DOMAIN_ALIAS_RID_POWER_USERS:
                  Begin
                     Result := True;
                     wns := wnsBuiltInPowerUsers;
                  End;
               DOMAIN_ALIAS_RID_ACCOUNT_OPS:
                  Begin
                     Result := True;
                     wns := wnsBuiltInAccountOperators;
                  End;
               DOMAIN_ALIAS_RID_SYSTEM_OPS:
                  Begin
                     Result := True;
                     wns := wnsBuiltInSystemOperators;
                  End;
               DOMAIN_ALIAS_RID_PRINT_OPS:
                  Begin
                     Result := True;
                     wns := wnsBuiltInPrintOperators;
                  End;
               DOMAIN_ALIAS_RID_BACKUP_OPS:
                  Begin
                     Result := True;
                     wns := wnsBuiltInBackupOperators;
                  End;
               DOMAIN_ALIAS_RID_REPLICATOR:
                  Begin
                     Result := True;
                     wns := wnsBuiltInReplicator;
                  End;
             End;
          SECURITY_NT_NON_UNIQUE:
             Begin
                adom := GetWellKnownSIDFromRID(domain, 0);
                Try
                   Case PLongint(GetSidSubAuthority(asid, cnt-1))^ Of
                     DOMAIN_USER_RID_ADMIN:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainAdministrator
                           Else
                              wns := wnsCustom;
                        End;
                     DOMAIN_USER_RID_GUEST:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainGuest
                           Else
                              wns := wnsCustom;
                        End;
                     DOMAIN_USER_RID_KRBTGT:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainKrbtgt
                           Else
                              wns := wnsCustom;
                        End;
                     DOMAIN_GROUP_RID_ADMINS:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainAdministrators
                           Else
                              wns := wnsCustom;
                        End;
                     DOMAIN_GROUP_RID_USERS:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainUsers
                           Else
                              wns := wnsCustom;
                        End;
                     DOMAIN_GROUP_RID_GUESTS:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainGuests
                           Else
                              wns := wnsCustom;
                        End;
                     DOMAIN_GROUP_RID_COMPUTERS:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainComputers
                           Else
                              wns := wnsCustom;
                        End;
                     DOMAIN_GROUP_RID_CONTROLLERS:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainControllers
                           Else
                              wns := wnsCustom;
                        End;
                     DOMAIN_GROUP_RID_CERT_ADMINS:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainCertAdmins
                           Else
                              wns := wnsCustom;
                        End;
                     DOMAIN_GROUP_RID_SCHEMA_ADMINS:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainSchemaAdmins
                           Else
                              wns := wnsCustom;
                        End;
                     DOMAIN_GROUP_RID_SCHEMA_ADMINS+1:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainEnterpriseAdmins
                           Else
                              wns := wnsCustom;
                        End;
                     DOMAIN_GROUP_RID_SCHEMA_ADMINS+2:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainPolicyAdmins
                           Else
                              wns := wnsCustom;
                        End;
                     DOMAIN_GROUP_RID_SCHEMA_ADMINS+3:
                        Begin
                           Result := True;
                           If EqualPrefixSid(adom, asid) Then
                              wns := wnsDomainRasAndIasServers
                           Else
                              wns := wnsCustom;
                        End;
                   End;
                Finally
                   FreeSid(adom);
                End;
             End;
        End;
    End;
 End;

{------------------------------------------------------------------------------}
{ FreeAndNilSecurityDescriptor(Var asd: PSecurityDescriptor);                  }
{------------------------------------------------------------------------------}
 Procedure FreeAndNilSecurityDescriptor(Var asd: PSecurityDescriptor);
 Var
    fpresent: LongBool;
    fdefaulted: LongBool;
    facl: PAcl;
    fsid: PSid;
 Begin
    If (asd<>Nil) Then Begin
       facl := Nil;
       If Not GetSecurityDescriptorDacl(asd, fpresent, facl, fdefaulted) Then
          RaiseACLError;
       FreeAndNilACL(facl);
       facl := Nil;
       If Not GetSecurityDescriptorSacl(asd, fpresent, facl, fdefaulted) Then
          RaiseACLError;
       FreeAndNilACL(facl);
       fsid := Nil;
       If Not GetSecurityDescriptorOwner(asd, fsid, fdefaulted) Then
          RaiseACLError;
       FreeAndNilSID(fsid);
       fsid := Nil;
       If Not GetSecurityDescriptorGroup(asd, fsid, fdefaulted) Then
          RaiseACLError;
       FreeAndNilSID(fsid);
       sdFree(asd);
       asd := Nil;
    End;
 End;

{------------------------------------------------------------------------------}
{ FreeAndNilACL(Var aacl: PAcl);                                               }
{------------------------------------------------------------------------------}
 Procedure FreeAndNilACL(Var aacl: PAcl);
 Var
    ai: TACLSizeInformation;
    ln: DWORD;
 Begin
    If (aacl<>Nil) Then Begin
       ZeroMemory(@ai, SizeOf(ai));
       If Not GetAclInformation(aacl^, @ai, SizeOf(ai), AclSizeInformation) Then
          RaiseACLError;
       ln := ai.AclBytesInUse+ai.AclBytesFree;
       sdFree(aacl, ln);
       aacl := Nil;
    End;
 End;

{------------------------------------------------------------------------------}
{ FreeAndNilACE(Var aace: PAce);                                               }
{------------------------------------------------------------------------------}
 Procedure FreeAndNilACE(Var aace: PAce);
 Begin
    aace := Nil;
 End;

{------------------------------------------------------------------------------}
{ FreeAndNilSID(Var asid: PSid);                                               }
{------------------------------------------------------------------------------}
 Procedure FreeAndNilSID(Var asid: PSid);
 Begin
    If (asid<>Nil) Then Begin
       If IsValidSid(asid) Then
          FreeSID(asid);
       asid := Nil;
    End;
 End;

{------------------------------------------------------------------------------}
{ RaiseACLError;                                                               }
{------------------------------------------------------------------------------}
 Procedure RaiseACLError;
 Var
    ErrorCode: DWORD;
 Begin
    ErrorCode := GetLastError;
    SetLastError(0);
    If (ErrorCode<>ERROR_SUCCESS) Then
       Raise EACLError.Create(ErrorCode);
 End;

{------------------------------------------------------------------------------}
{ RaiseACLError(ErrorCode: DWORD);                                             }
{------------------------------------------------------------------------------}
 Procedure RaiseACLError(ErrorCode: DWORD);
 Begin
    SetLastError(0);
    If (ErrorCode<>ERROR_SUCCESS) Then
       Raise EACLError.Create(ErrorCode);
 End;

{------------------------------------------------------------------------------}
{ aclAlloc(Size: Integer);                                                     }
{ memory allocation for SecurityDescriptors                                    }
{------------------------------------------------------------------------------}
 Function sdAlloc(Size: Integer): Pointer;
 Begin
    Result := Nil;
    If (Size<=0) Then Exit;
    {$IFDEF GLOBALMEM}
    GetMem(Result, size);
    {$ELSE}
    Result := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY Or HEAP_GENERATE_EXCEPTIONS, Size);
    {$ENDIF}
 End;

{------------------------------------------------------------------------------}
{ sdFree(ptr: Pointer);                                                        }
{ memory disallocation for SecurityDescriptors                                 }
{------------------------------------------------------------------------------}
 Procedure sdFree(ptr: Pointer);
 Begin
    If (ptr=Nil) Then Exit;
    {$IFDEF GLOBALMEM}
    FreeMem(ptr);
    {$ELSE}
    HeapFree(GetProcessHeap, 0, ptr);
    {$ENDIF}
 End;

{------------------------------------------------------------------------------}
{ sdFree(ptr: Pointer; Size: Integer);                                         }
{ memory disallocation for SecurityDescriptors                                 }
{------------------------------------------------------------------------------}
 Procedure sdFree(ptr: Pointer; Size: Integer);
 Begin
    If (ptr=Nil) Then Exit;
    {$IFDEF GLOBALMEM}
    FreeMem(ptr, Size);
    {$ELSE}
    HeapFree(GetProcessHeap, 0, ptr);
    {$ENDIF}
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation EACLError                                                     }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ EACLError.Create(ErrorCode: DWORD);                                          }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor EACLError.Create(ErrorCode: DWORD);
 Begin
    Inherited Create(SysErrorMessage(ErrorCode));
 End;

{------------------------------------------------------------------------------}
{ Initialization Part                                                          }
{------------------------------------------------------------------------------}
 Initialization
   InitNTLanManAPI;

{------------------------------------------------------------------------------}
{ Finalization Part                                                            }
{------------------------------------------------------------------------------}
 Finalization
   FinalizeNTLanManAPI;

{------------------------------------------------------------------------------}
{ End of ObjectsRightsConsts.pas                                               }
{------------------------------------------------------------------------------}
 End.

