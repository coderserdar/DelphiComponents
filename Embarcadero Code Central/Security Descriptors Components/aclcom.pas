{------------------------------------------------------------------------------}
{ aclcom                                                                       }
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
 Unit aclcom;

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
    TRegValueSecurity = Class;
    TRegValueAccesses = Class;
    TRegValueAccess = Class;

    TRegValueSecurity = Class(TCustomSecurityObject)
    Private
      FRootKey: HKEY;
      FKeyName: String;
      FValueName: String;
      Function  GetDList: TRegValueAccesses;
      Procedure SetDList(Const Value: TRegValueAccesses);
      Function  GetKey: TRootKey;
      Procedure SetKey(Const Value: TRootKey);
      Function  GetKeyName: String;
      Procedure SetKeyName(Const Value: String);
      Function  GetValueName: String;
      Procedure SetValueName(Const Value: String);
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
      Property Accesses: TRegValueAccesses     Read GetDList     Write SetDList Stored False;
      Property ComputerName;
      Property LookupSystemName;
      Property RootKey: TRootKey               Read GetKey       Write SetKey;
      Property KeyName: String                 Read GetKeyName   Write SetKeyName;
      Property ValueName: String               Read GetValueName Write SetValueName;
    End;

    TRegValueAccesses = Class(TDACL)
    Private
      Function GetItems(Index: Integer): TRegValueAccess;
      Procedure SetItems(Index: Integer; Const Value: TRegValueAccess);
    Public
      Property Items[Index: Integer]: TRegValueAccess
                                               Read GetItems     Write SetItems; Default;
    End;

    TRegValueAccess = Class(TCustomACE)
    Private
      Function GetAccess: Boolean;
      Procedure SetAccess(Const Value: Boolean);
      Function GetMaskFlags(Kind: TFlagKind): Cardinal;
      Procedure SetMaskFlags(Kind: TFlagKind; Const Value: Cardinal);
    Protected
      Procedure GetDeniedAce(Value: PACL);                                      Override;
      Procedure GetAllowedAce(Value: PACL);                                     Override;
      Function GetDeniedSize: Integer;                                          Override;
      Function GetAllowedSize: Integer;                                         Override;
    Public
      Property MaskFlags[Kind: TFlagKind]: Cardinal
                                       Read GetMaskFlags    Write SetMaskFlags;
      Property Status;
    Published
      Property AccessAllowed: Boolean  Read GetAccess       Write SetAccess;
    End;

    TRegValueAccessCheck = Class(TCustomAccessCheck)
    Private
      Function GetSecurityObj: TRegValueSecurity;
      Procedure SetSecurityObj(Const Value: TRegValueSecurity);
    Public
      Constructor Create(AOwner: TComponent);                                   Override;
    Published
      Property Active;
      Property Allowed;
      Property SecurityObject: TRegValueSecurity
                                       Read GetSecurityObj   Write SetSecurityObj;
    End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ I M P L E M E N T A T I O N                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
 Implementation

{------------------------------------------------------------------------------}
{ ReadSDFromRegistry(...);                                                     }
{ Saves in a binary and transportable format                                   }
{------------------------------------------------------------------------------}
 Function ReadSDFromRegistry(Const Computer: String; Root: HKEY; Const Path, Value: String): PSecurityDescriptor;
 Var
    Err    : Cardinal;
    dlen   : Cardinal;
    dtype  : Cardinal;
    desc   : PSecurityDescriptor;
    RootKey: HKEY;
    TempKey: HKEY;
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
    Err := RegOpenKeyEx(RootKey, PChar(Path), 0, KEY_QUERY_VALUE, TempKey);
    If (Err=ERROR_SUCCESS) Then
       Try
          dtype := REG_BINARY;
          Err := RegQueryValueEx(TempKey, PChar(Value), Nil, @dtype, PByte(desc), @dlen);
          If (Err<>ERROR_SUCCESS) And (Err<>ERROR_MORE_DATA) Then
             RaiseACLError(Err);
          desc := sdAlloc(dlen);
          Try
             Err := RegQueryValueEx(TempKey, PChar(Value), Nil, @dtype, PByte(desc), @dlen);
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
 End;

{------------------------------------------------------------------------------}
{ SaveSDToRegistry(...);                                                       }
{ Saves in a binary and transportable format                                   }
{------------------------------------------------------------------------------}
 Procedure SaveSDToRegistry(Const Computer: String; Root: HKEY; Const Path, Value: String; sd: PSecurityDescriptor);
 Var
    Err    : Cardinal;
    desc   : PSecurityDescriptor;
    dlen   : Cardinal;
    RootKey: HKEY;
    TempKey: HKEY;
 Begin
    SetLastError(0);
    RootKey := Root;
    If (Computer<>'') Then Begin
       Err := RegConnectRegistry(PChar(Computer), Root, RootKey);
       If (Err<>ERROR_SUCCESS) Then
          RaiseACLError(Err);
    End;
    Err := RegOpenKeyEx(RootKey, PChar(Path), 0, KEY_WRITE, TempKey);
    If (Err=ERROR_SUCCESS) Then
       Try
          desc := MakeSelfRelativeSecurityDescriptor(sd, dlen);
          Try
             Err := RegSetValueEx(TempKey, PChar(Value), 0, REG_BINARY, desc, dlen);
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
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegValueSecurity                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegValueSecurity.CreateList: TDACL;                                         }
{ Create the Access List                                                       }
{------------------------------------------------------------------------------}
 Function TRegValueSecurity.CreateAccessList: TDACL;
 Begin
    Result := TRegValueAccesses.Create(Self, Self, TRegValueAccess);
 End;

{------------------------------------------------------------------------------}
{ TRegValueSecurity.CreateAuditList: TSACL;                                    }
{ Create the Access List                                                       }
{------------------------------------------------------------------------------}
 Function TRegValueSecurity.CreateAuditList: TSACL;
 Begin
    Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TRegValueSecurity.CreateOwnerID: TCreatorSecurityID;                         }
{ Create the Owner ID                                                          }
{------------------------------------------------------------------------------}
 Function TRegValueSecurity.CreateOwnerID: TCreatorSecurityID;
 Begin
    Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TRegValueSecurity.CreateGroupID: TCreatorSecurityID;                         }
{ Create the Group ID                                                          }
{------------------------------------------------------------------------------}
 Function TRegValueSecurity.CreateGroupID: TCreatorSecurityID;
 Begin
    Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TRegValueSecurity.GetDList: TRegValueAccesses;                               }
{ Read method property AccessList                                              }
{------------------------------------------------------------------------------}
 Function TRegValueSecurity.GetDList: TRegValueAccesses;
 Begin
    Result := TRegValueAccesses(GetAccessList);
 End;

{------------------------------------------------------------------------------}
{ TRegValueSecurity.SetDList(...);                                             }
{ Write method property AccessList                                             }
{------------------------------------------------------------------------------}
 Procedure TRegValueSecurity.SetDList(Const Value: TRegValueAccesses);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TRegValueSecurity.GetKey: TRootKey;                                          }
{ Read method property RootKey                                                 }
{------------------------------------------------------------------------------}
 Function TRegValueSecurity.GetKey: TRootKey;
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
{ TRegValueSecurity.SetKey(Const Value: TRootKey);                             }
{ Write method property RootKey                                                }
{------------------------------------------------------------------------------}
 Procedure TRegValueSecurity.SetKey(Const Value: TRootKey);
 Var
    Sv: Boolean;
 Begin
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
 End;

{------------------------------------------------------------------------------}
{ TRegValueSecurity.GetKeyName: String;                                        }
{ Read method property KeyName                                                 }
{------------------------------------------------------------------------------}
 Function TRegValueSecurity.GetKeyName: String;
 Begin
    Result := FKeyName;
 End;

{------------------------------------------------------------------------------}
{ TRegValueSecurity.SetKeyName(Const Value: String);                           }
{ Write method property KeyName                                                }
{------------------------------------------------------------------------------}
 Procedure TRegValueSecurity.SetKeyName(Const Value: String);
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
{ TRegValueSecurity.GetValueName: String;                                      }
{ Read method property ValueName                                               }
{------------------------------------------------------------------------------}
 Function TRegValueSecurity.GetValueName: String;
 Begin
    Result := FValueName;
 End;

{------------------------------------------------------------------------------}
{ TRegValueSecurity.SetValueName(Const Value: String);                         }
{ Write method property ValueName                                              }
{------------------------------------------------------------------------------}
 Procedure TRegValueSecurity.SetValueName(Const Value: String);
 Var
    SV: Boolean;
 Begin
    Try
       SV := Active;
       If SV Then
          Active := False;
       FValueName := Value;
       If Sv Then
          Active := True;
    Except
       Active := False;
       Raise;
    End;
 End;

{------------------------------------------------------------------------------}
{ TRegValueSecurity.ReadObjectDescriptor: PSecurityDescriptor;                 }
{ Read the SecurityDescriptor                                                  }
{------------------------------------------------------------------------------}
 Function TRegValueSecurity.ReadObjectDescriptor: PSecurityDescriptor;
 Begin
    Result := ReadSDFromRegistry(ComputerName, FRootKey, KeyName, ValueName);
 End;

{------------------------------------------------------------------------------}
{ TRegValueSecurity.WriteObjectDescriptor;                                     }
{ Writes the SecurityDescriptor                                                }
{------------------------------------------------------------------------------}
 Procedure TRegValueSecurity.WriteObjectDescriptor;
 Begin
    CheckSD;
    SaveSDToRegistry(ComputerName, FRootKey, KeyName, ValueName, Descriptor);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegValueAccesses                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegValueAccesses.GetItems(...): TRegValueAccess;                            }
{ Read Method property Items                                                   }
{------------------------------------------------------------------------------}
 Function TRegValueAccesses.GetItems(Index: Integer): TRegValueAccess;
 Begin
    Result := TRegValueAccess(Inherited Items[Index]);
 End;

{------------------------------------------------------------------------------}
{ TRegValueAccesses.SetItems(...);                                             }
{ Write Method property Items                                                  }
{------------------------------------------------------------------------------}
 Procedure TRegValueAccesses.SetItems(Index: Integer; Const Value: TRegValueAccess);
 Begin
    Inherited Items[Index] := Value;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegValueAccess                                               }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegValueAccess.GetAccess: Boolean;                                          }
{ Read Method property AccessAllowed                                           }
{------------------------------------------------------------------------------}
 Function TRegValueAccess.GetAccess: Boolean;
 Begin
    Result := aeAllowed=AceType;
 End;

{------------------------------------------------------------------------------}
{ TRegValueAccess.SetAccess(Const Value: Boolean);                             }
{ Write Method property AccessAllowed                                          }
{------------------------------------------------------------------------------}
 Procedure TRegValueAccess.SetAccess(Const Value: Boolean);
 Begin
    If Value Then
       AceType := aeAllowed
    Else
       AceType := aeDenied;
    Status := asmodified;
 End;

{------------------------------------------------------------------------------}
{ TRegValueAccess.GetMaskFlags(Kind: TFlagKind): Cardinal;                     }
{ Read method Property MaskFlags                                               }
{------------------------------------------------------------------------------}
 Function TRegValueAccess.GetMaskFlags(Kind: TFlagKind): Cardinal;
 Begin
    Result := 1; // Always 1
 End;

{------------------------------------------------------------------------------}
{ TRegValueAccess.SetMaskFlags(Kind: TFlagKind; Const Value: Cardinal);        }
{ Write method Property MaskFlags                                              }
{------------------------------------------------------------------------------}
 Procedure TRegValueAccess.SetMaskFlags(Kind: TFlagKind; Const Value: Cardinal);
 Begin
    Inherited MaskFlags[Kind] := 1;  // Always 1
//    Inherited MaskFlags[fkYes] := 1;  // Always 1
 End;

{------------------------------------------------------------------------------}
{ TRegValueAccess.GetDeniedAce(Value: PACL): Boolean;                          }
{ GetDeniedAce method Override                                                 }
{------------------------------------------------------------------------------}
 Procedure TRegValueAccess.GetDeniedAce(Value: PACL);
 Var
    xAce: Pointer;
 Begin
    SetLastError(0);
    Case Status Of
       asnone, asmodified, asadded:
         If Not AccessAllowed Then Begin
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
{ TRegValueAccess.GetAllowedAce(Value: PACL): Boolean;                         }
{ GetAllowedAce method Override                                                }
{------------------------------------------------------------------------------}
 Procedure TRegValueAccess.GetAllowedAce(Value: PACL);
 Var
    xAce: Pointer;
 Begin
    SetLastError(0);
    Case Status Of
       asnone, asmodified, asadded:
         If AccessAllowed Then Begin
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
{ TRegValueAccess.GetDeniedSize: Integer;                                      }
{ GetDeniedSize method Override                                                }
{------------------------------------------------------------------------------}
 Function TRegValueAccess.GetDeniedSize: Integer;
 Begin
    Result := 0;
    If Not AccessAllowed Then Begin
       Result := SizeOf(TAceHeader)+SizeOf(TAccessDeniedAce)+SizeOf(Byte)-SizeOf(DWORD);
       Inc(Result, GetLengthSid(Identifier.AsSID));
    End;
 End;

{------------------------------------------------------------------------------}
{ TRegValueAccess.GetAllowedSize: Integer;                                     }
{ GetAllowedSize method Override                                               }
{------------------------------------------------------------------------------}
 Function TRegValueAccess.GetAllowedSize: Integer;
 Begin
    Result := 0;
    If AccessAllowed Then Begin
       Result := SizeOf(TAceHeader)+SizeOf(TAccessAllowedAce)+SizeOf(Byte)-SizeOf(DWORD);
       Inc(Result, GetLengthSid(Identifier.AsSID));
    End;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TRegValueAccessCheck                                          }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TRegValueAccessCheck.Create(AOwner: TComponent);                             }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TRegValueAccessCheck.Create(AOwner: TComponent);
 Begin
    Inherited Create(AOwner);
    DesiredAccess := 1;
 End;

{------------------------------------------------------------------------------}
{ TRegValueAccessCheck.GetSecurityObj: TRegValueSecurity;                      }
{ Read Method property SecurityObject                                          }
{------------------------------------------------------------------------------}
 Function TRegValueAccessCheck.GetSecurityObj: TRegValueSecurity;
 Begin
    Result := (Inherited SecurityObject) As TRegValueSecurity;
 End;

{------------------------------------------------------------------------------}
{ TRegValueAccessCheck.SetSecurityObj(Const Value: TRegValueSecurity);         }
{ Write Method property SecurityObject                                         }
{------------------------------------------------------------------------------}
 Procedure TRegValueAccessCheck.SetSecurityObj(Const Value: TRegValueSecurity);
 Begin
    Inherited SecurityObject := Value;
 End;

{------------------------------------------------------------------------------}
{ End of aclcom.pas                                                            }
{------------------------------------------------------------------------------}
 End.
