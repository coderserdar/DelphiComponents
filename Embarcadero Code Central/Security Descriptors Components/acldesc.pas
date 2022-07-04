{------------------------------------------------------------------------------}
{ acldesc                                                                      }
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
 Unit acldesc;

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
    Windows, SysUtils, Classes, aclbase, aclconst, aclfuncs;

{------------------------------------------------------------------------------}
{ Public Types                                                                 }
{------------------------------------------------------------------------------}
 Type
    TDescriptorApplyFlag = (fidObject, fidChildObjects, fidSetOnly);
    TDescriptorApplyFlags = Set Of TDescriptorApplyFlag;

    TDescriptorAccesses = Class;
    TDescriptorAccess = Class;
    TDescriptorAccessFlags = Class;
    TDescriptorAccessData = Cardinal;

    TDescriptorAudits = Class;
    TDescriptorAudit = Class;
    TDescriptorAuditFlags = Class;
    TDescriptorAuditData = Cardinal;

    TSecurityDescriptorObject = Class(TCustomSecurityObject)
    Private
      Function GetDList: TDescriptorAccesses;
      Procedure SetDList(Const Value: TDescriptorAccesses);
      Function GetSList: TDescriptorAudits;
      Procedure SetSList(Const Value: TDescriptorAudits);
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
      Property Accesses: TDescriptorAccesses Read GetDList      Write SetDList  Stored False;
      Property Audits: TDescriptorAudits     Read GetSList      Write SetSList  Stored False;
      Property CreatorOwner: TCreatorOwnerID Read GetFOwner     Write SetFOwner Stored False;
      Property CreatorGroup: TCreatorGroupID Read GetFGroup     Write SetFGroup Stored False;
      Property LookupSystemName;
      Property Options;
    End;

    TDescriptorAccesses = Class(TDACL)
    Private
      Function GetItems(Index: Integer): TDescriptorAccess;
      Procedure SetItems(Index: Integer; Const Value: TDescriptorAccess);
    Public
      Property Items[Index: Integer]: TDescriptorAccess
                                               Read GetItems     Write SetItems; Default;
    End;

    TDescriptorAccess = Class(TCustomACE)
    Private
      FDescFlags: TDescriptorAccessFlags;
      Function GetFlags: TDescriptorAccessFlags;
      Procedure SetFlags(Const Value: TDescriptorAccessFlags);
      Function GetMaskFlags(Kind: TFlagKind): Cardinal;
      Procedure SetMaskFlags(Kind: TFlagKind; Const Value: Cardinal);
      Function GetApplyTo: TDescriptorApplyFlags;
      Procedure SetApplyTo(Const Value: TDescriptorApplyFlags);
    Protected
      Procedure GetDeniedAce(Value: PACL);                                      Override;
      Procedure GetAllowedAce(Value: PACL);                                     Override;
      Function GetDeniedSize: Integer;                                          Override;
      Function GetAllowedSize: Integer;                                         Override;
      Property MaskFlags[Kind: TFlagKind]: Cardinal
                                              Read GetMaskFlags Write SetMaskFlags;
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
      Property Status;
    Published
      Property Flags: TDescriptorAccessFlags  Read GetFlags     Write SetFlags;
      Property ApplyTo: TDescriptorApplyFlags Read GetApplyTo   Write SetApplyTo  Default [fidObject, fidChildObjects];
      Property IsInherited;
    End;

    TDescriptorAccessFlags = Class(TPersistent)
    Private
      FData: Cardinal;
      FFlag: TAccessFlagValue;
      FAce: TDescriptorAccess;
      Function GetData: TDescriptorAccessData;
      Procedure SetData(Const Value: TDescriptorAccessData);
      Function GetFlag: TAccessFlagValue;
      Procedure SetFlag(Const Value: TAccessFlagValue);
    Public
      Constructor Create(Ace: TDescriptorAccess);                               Overload;
      Procedure Assign(Source: TPersistent);                                    Override;
      Property ACE: TDescriptorAccess         Read FAce;
    Published
      Property Data: TDescriptorAccessData    Read GetData      Write SetData;
      Property Flag: TAccessFlagValue         Read GetFlag      Write SetFlag;
    End;

    // Audits

    TDescriptorAudits = Class(TSACL)
    Private
      Function GetItems(Index: Integer): TDescriptorAudit;
      Procedure SetItems(Index: Integer; Const Value: TDescriptorAudit);
    Public
      Property Items[Index: Integer]: TDescriptorAudit
                                               Read GetItems     Write SetItems; Default;
    End;

    TDescriptorAudit = Class(TCustomACE)
    Private
      FDescFlags: TDescriptorAuditFlags;
      Function GetFlags: TDescriptorAuditFlags;
      Procedure SetFlags(Const Value: TDescriptorAuditFlags);
      Function GetMaskFlags(Kind: TFlagKind): Cardinal;
      Procedure SetMaskFlags(Kind: TFlagKind; Const Value: Cardinal);
      Function GetApplyTo: TDescriptorApplyFlags;
      Procedure SetApplyTo(Const Value: TDescriptorApplyFlags);
    Protected
      Procedure GetDeniedAce(Value: PACL);                                      Override;
      Procedure GetAllowedAce(Value: PACL);                                     Override;
      Function GetDeniedSize: Integer;                                          Override;
      Function GetAllowedSize: Integer;                                         Override;
      Property MaskFlags[Kind: TFlagKind]: Cardinal
                                              Read GetMaskFlags Write SetMaskFlags;
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
      Property Status;
    Published
      Property Flags: TDescriptorAuditFlags  Read GetFlags     Write SetFlags;
      Property ApplyTo: TDescriptorApplyFlags Read GetApplyTo   Write SetApplyTo  Default [fidObject, fidChildObjects];
      Property IsInherited;
    End;

    TDescriptorAuditFlags = Class(TPersistent)
    Private
      FData: Cardinal;
      FFlag: TAuditFlagValue;
      FAce: TDescriptorAudit;
      Function GetData: TDescriptorAuditData;
      Procedure SetData(Const Value: TDescriptorAuditData);
      Function GetFlag: TAuditFlagValue;
      Procedure SetFlag(Const Value: TAuditFlagValue);
    Public
      Constructor Create(Ace: TDescriptorAudit);                               Overload;
      Procedure Assign(Source: TPersistent);                                    Override;
      Property ACE: TDescriptorAudit          Read FAce;
    Published
      Property Data: TDescriptorAuditData     Read GetData      Write SetData;
      Property Flag: TAuditFlagValue          Read GetFlag      Write SetFlag;
    End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ I M P L E M E N T A T I O N                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
 Implementation

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TSecurityDescriptorObject                                     }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TDescriptorAccesses.CreateAccessList: TDACL;                                 }
{ Create the Access List                                                       }
{------------------------------------------------------------------------------}
 Function TSecurityDescriptorObject.CreateAccessList: TDACL;
 Begin
    Result := TDescriptorAccesses.Create(Self, Self, TDescriptorAccess);
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.CreateAuditList: TSACL;                            }
{ Create the Access List                                                       }
{------------------------------------------------------------------------------}
 Function TSecurityDescriptorObject.CreateAuditList: TSACL;
 Begin
    Result := TDescriptorAudits.Create(Self, Self, TDescriptorAudit);
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.CreateOwnerID: TCreatorSecurityID;                 }
{ Create the Owner ID                                                          }
{------------------------------------------------------------------------------}
 Function TSecurityDescriptorObject.CreateOwnerID: TCreatorSecurityID;
 Begin
    Result := TCreatorOwnerID.Create(Self);
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.CreateGroupID: TCreatorSecurityID;                 }
{ Create the Group ID                                                          }
{------------------------------------------------------------------------------}
 Function TSecurityDescriptorObject.CreateGroupID: TCreatorSecurityID;
 Begin
    Result := TCreatorGroupID.Create(Self);
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.GetDList: TDescriptorAccesses;                     }
{ Read method property AccessList                                              }
{------------------------------------------------------------------------------}
 Function TSecurityDescriptorObject.GetDList: TDescriptorAccesses;
 Begin
    If (sdWantACL In Options) Then
       Result := TDescriptorAccesses(GetAccessList)
    Else
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.SetDList(...);                                     }
{ Write method property AccessList                                             }
{------------------------------------------------------------------------------}
 Procedure TSecurityDescriptorObject.SetDList(Const Value: TDescriptorAccesses);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.GetSList: TDescriptorAudits;                       }
{ Read method property AuditList                                               }
{------------------------------------------------------------------------------}
 Function TSecurityDescriptorObject.GetSList: TDescriptorAudits;
 Begin
    If (sdWantSystemACL In Options) Then
       Result := TDescriptorAudits(GetAuditList)
    Else
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.SetSList(...);                                     }
{ Write method property AuditList                                              }
{------------------------------------------------------------------------------}
 Procedure TSecurityDescriptorObject.SetSList(Const Value: TDescriptorAudits);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.GetFOwner: TCreatorOwnerID;                        }
{ Read method property CreatorOwner                                            }
{------------------------------------------------------------------------------}
 Function TSecurityDescriptorObject.GetFOwner: TCreatorOwnerID;
 Begin
    If (sdWantOwner In Options) Then
       Result := TCreatorOwnerID(GetOwnerID)
    Else
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.SetFOwner(Const Value: TCreatorOwnerID);           }
{ Write method property CreatorOwner                                           }
{------------------------------------------------------------------------------}
 Procedure TSecurityDescriptorObject.SetFOwner(Const Value: TCreatorOwnerID);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.GetFGroup: TCreatorGroupID;                        }
{ Read method property CreatorGroup                                            }
{------------------------------------------------------------------------------}
 Function TSecurityDescriptorObject.GetFGroup: TCreatorGroupID;
 Begin
    If (sdWantGroup In Options) Then
       Result := TCreatorGroupID(GetGroupID)
    Else
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.SetFGroup(Const Value: TCreatorGroupID);           }
{ Write method property CreatorGroup                                           }
{------------------------------------------------------------------------------}
 Procedure TSecurityDescriptorObject.SetFGroup(Const Value: TCreatorGroupID);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.ReadObjectDescriptor: PSecurityDescriptor;         }
{ Read the SecurityDescriptor                                                  }
{------------------------------------------------------------------------------}
 Function TSecurityDescriptorObject.ReadObjectDescriptor: PSecurityDescriptor;
 Begin
    Result := sdAlloc(SECURITY_DESCRIPTOR_MIN_LENGTH);
    Try
       If Not InitializeSecurityDescriptor(Result, SECURITY_DESCRIPTOR_REVISION) Then
          RaiseACLError;
    Except
       sdFree(Result, SECURITY_DESCRIPTOR_MIN_LENGTH);
       Raise;
    End;
 End;

{------------------------------------------------------------------------------}
{ TSecurityDescriptorObject.WriteObjectDescriptor;                             }
{ Writes the SecurityDescriptor                                                }
{------------------------------------------------------------------------------}
 Procedure TSecurityDescriptorObject.WriteObjectDescriptor;
 Begin
    CheckSD;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TDescriptorAccesses                                           }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TDescriptorAccesses.GetItems(...): TDescriptorAccess;                        }
{ Read Method property Items                                                   }
{------------------------------------------------------------------------------}
 Function TDescriptorAccesses.GetItems(Index: Integer): TDescriptorAccess;
 Begin
    Result := TDescriptorAccess(Inherited Items[Index]);
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccesses.SetItems(...);                                           }
{ Write Method property Items                                                  }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAccesses.SetItems(Index: Integer; Const Value: TDescriptorAccess);
 Begin
    Inherited Items[Index] := Value;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TDescriptorAccess                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TDescriptorAccess.Create(Collection: TCollection);                           }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TDescriptorAccess.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    FDescFlags := TDescriptorAccessFlags.Create(Self);
    ApplyTo := [fidObject, fidChildObjects];    
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccess.Destroy;                                                   }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TDescriptorAccess.Destroy;
 Begin
    FDescFlags.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccess.GetFlags: TDescriptorAccessFlags;                          }
{ Read method property Flags                                                   }
{------------------------------------------------------------------------------}
 Function TDescriptorAccess.GetFlags: TDescriptorAccessFlags;
 Begin
    Result := FDescFlags;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccess.SetFlags(Const Value: TDescriptorAccessFlags);             }
{ Write method property Flags                                                  }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAccess.SetFlags(Const Value: TDescriptorAccessFlags);
 Begin
    FDescFlags.Assign(Value);
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccess.GetMaskFlags(Kind: TFlagKind): Cardinal;                   }
{ Read method Property MaskFlags                                               }
{------------------------------------------------------------------------------}
 Function TDescriptorAccess.GetMaskFlags(Kind: TFlagKind): Cardinal;
 Begin
    Result := 0;
    Case Kind Of
       fkYes:
         If (FDescFlags.Flag=fvAllowed) Then
            Result := FDescFlags.Data;
       fkNo :
         If (FDescFlags.Flag=fvDenied) Then
            Result := FDescFlags.Data;
    End;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccess.SetMaskFlags(Kind: TFlagKind; Const Value: Cardinal);      }
{ Write method Property MaskFlags                                              }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAccess.SetMaskFlags(Kind: TFlagKind; Const Value: Cardinal);
 Begin
    Case Kind Of
       fkYes:
         Begin
            FDescFlags.Data := Value;
            FDescFlags.Flag := fvAllowed;
         End;
       fkNo :
         Begin
            FDescFlags.Data := Value;
            FDescFlags.Flag := fvDenied;
         End;
    End;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccess.GetApplyTo: TDescriptorApplyFlags;                         }
{ Read method property ApplyTo                                                 }
{------------------------------------------------------------------------------}
 Function TDescriptorAccess.GetApplyTo: TDescriptorApplyFlags;
 Begin
    Result := [fidObject];
    If (afNoPropagate In AceFlags) Then
       Include(Result, fidSetOnly);
    If (afInheritOnly In AceFlags) Then
       Exclude(Result, fidObject);
    If (afContainerInherit In AceFlags) Then
       Include(Result, fidChildObjects);
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccess.SetApplyTo(Const Value: TDescriptorApplyFlags);            }
{ Write method property ApplyTo                                                }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAccess.SetApplyTo(Const Value: TDescriptorApplyFlags);
 Begin
    If IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    AceFlags := Aceflags+[afInheritOnly]-[afNoPropagate, afContainerInherit];
    If (fidChildObjects In Value) Then
       AceFlags := AceFlags+[afContainerInherit];
    If (fidSetOnly In Value) Then
       AceFlags := AceFlags+[afNoPropagate];
    If (fidObject In Value) Then
       AceFlags := AceFlags-[afInheritOnly];
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccess.GetDeniedAce(Value: PACL);                                 }
{ GetDeniedAce method Override                                                 }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAccess.GetDeniedAce(Value: PACL);
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
{ TDescriptorAccess.GetAllowedAce(Value: PACL);                                }
{ GetAllowedAce method Override                                                }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAccess.GetAllowedAce(Value: PACL);
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
{ TDescriptorAccess.GetDeniedSize: Integer;                                    }
{ GetDeniedSize method Override                                                }
{------------------------------------------------------------------------------}
 Function TDescriptorAccess.GetDeniedSize: Integer;
 Begin
    Result := 0;
    If MaskFlags[fkNo]=0 Then Exit;
    Result := SizeOf(TAceHeader)+SizeOf(TAccessDeniedAce)+SizeOf(Byte)-SizeOf(DWORD);
    Inc(Result, GetLengthSid(Identifier.AsSID));
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccess.GetAllowedSize: Integer;                                   }
{ GetAllowedSize method Override                                               }
{------------------------------------------------------------------------------}
 Function TDescriptorAccess.GetAllowedSize: Integer;
 Begin
    Result := 0;
    If (MaskFlags[fkYes]=0) Then Exit;
    Result := SizeOf(TAceHeader)+SizeOf(TAccessAllowedAce)+SizeOf(Byte)-SizeOf(DWORD);
    Inc(Result, GetLengthSid(Identifier.AsSID));
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TDescriptorAccessFlags                                        }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TDescriptorAccessFlags.Create(Ace: TDescriptorAccess);                       }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TDescriptorAccessFlags.Create(Ace: TDescriptorAccess);
 Begin
    Inherited Create;
    FFlag := fvNone;
    FAce := Ace;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccessFlags.Assign(Source: TPersistent);                          }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAccessFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TDescriptorAccessFlags) Then
       With Source As TDescriptorAccessFlags Do Begin
          Self.Flag := Flag;
          Self.Data := Data;
       End;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccessData.GetValue: TDescriptorAccessData;                       }
{ Read method property Data                                                    }
{------------------------------------------------------------------------------}
 Function TDescriptorAccessFlags.GetData: TDescriptorAccessData;
 Begin
    Result := FData;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccessFlags.SetData(Const Value: TDescriptorAccessData);          }
{ Write method property Value                                                  }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAccessFlags.SetData(Const Value: TDescriptorAccessData);
 Begin
    If FAce.IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    FData := Value;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccessFlags.GetFlag: TAccessFlagValue;                            }
{ Read method property Flag                                                    }
{------------------------------------------------------------------------------}
 Function TDescriptorAccessFlags.GetFlag: TAccessFlagValue;
 Begin
    Result := FFlag;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAccessFlags.SetFlag(Const Value: TAccessFlagValue);               }
{ Write method property Flag                                                   }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAccessFlags.SetFlag(Const Value: TAccessFlagValue);
 Begin
    If FAce.IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    FFlag := Value;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TDescriptorAudits                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TDescriptorAudits.GetItems(...): TDescriptorAudit;                           }
{ Read Method property Items                                                   }
{------------------------------------------------------------------------------}
 Function TDescriptorAudits.GetItems(Index: Integer): TDescriptorAudit;
 Begin
    Result := TDescriptorAudit(Inherited Items[Index]);
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAudits.SetItems(...);                                             }
{ Write Method property Items                                                  }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAudits.SetItems(Index: Integer; Const Value: TDescriptorAudit);
 Begin
    Inherited Items[Index] := Value;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TDescriptorAudit                                              }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TDescriptorAudit.Create(Collection: TCollection);                            }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TDescriptorAudit.Create(Collection: TCollection);
 Begin
    Inherited Create(Collection);
    FDescFlags := TDescriptorAuditFlags.Create(Self);
    ApplyTo := [fidObject, fidChildObjects];
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAudit.Destroy;                                                    }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TDescriptorAudit.Destroy;
 Begin
    FDescFlags.Free;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAudit.GetFlags: TDescriptorAuditFlags;                            }
{ Read method property Flags                                                   }
{------------------------------------------------------------------------------}
 Function TDescriptorAudit.GetFlags: TDescriptorAuditFlags;
 Begin
    Result := FDescFlags;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAudit.SetFlags(Const Value: TDescriptorAuditFlags);               }
{ Write method property Flags                                                  }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAudit.SetFlags(Const Value: TDescriptorAuditFlags);
 Begin
    FDescFlags.Assign(Value);
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAudit.GetMaskFlags(Kind: TFlagKind): Cardinal;                    }
{ Read method Property MaskFlags                                               }
{------------------------------------------------------------------------------}
 Function TDescriptorAudit.GetMaskFlags(Kind: TFlagKind): Cardinal;
 Begin
    Result := 0;
    Case Kind Of
       fkYes:
         If (FDescFlags.Flag=faSucceeded) Or (FDescFlags.Flag=faSucceededFailed) Then
            Result := FDescFlags.Data;
       fkNo :
         If (FDescFlags.Flag=faFailed) Or (FDescFlags.Flag=faSucceededFailed) Then
            Result := FDescFlags.Data;
    End;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAudit.SetMaskFlags(Kind: TFlagKind; Const Value: Cardinal);       }
{ Write method Property MaskFlags                                              }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAudit.SetMaskFlags(Kind: TFlagKind; Const Value: Cardinal);
 Begin
    Case Kind Of
       fkYes:
         Begin
            FDescFlags.Data := Value;
            FDescFlags.Flag := faSucceeded;
         End;
       fkNo :
         Begin
            FDescFlags.Data := Value;
            FDescFlags.Flag := faFailed;
         End;
    End;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAudit.GetApplyTo: TDescriptorApplyFlags;                          }
{ Read method property ApplyTo                                                 }
{------------------------------------------------------------------------------}
 Function TDescriptorAudit.GetApplyTo: TDescriptorApplyFlags;
 Begin
    Result := [fidObject];
    If (afNoPropagate In AceFlags) Then
       Include(Result, fidSetOnly);
    If (afInheritOnly In AceFlags) Then
       Exclude(Result, fidObject);
    If (afContainerInherit In AceFlags) Then
       Include(Result, fidChildObjects);
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAudit.SetApplyTo(Const Value: TDescriptorApplyFlags);             }
{ Write method property ApplyTo                                                }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAudit.SetApplyTo(Const Value: TDescriptorApplyFlags);
 Begin
    If IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    AceFlags := Aceflags+[afInheritOnly]-[afNoPropagate, afContainerInherit];
    If (fidChildObjects In Value) Then
       AceFlags := AceFlags+[afContainerInherit];
    If (fidSetOnly In Value) Then
       AceFlags := AceFlags+[afNoPropagate];
    If (fidObject In Value) Then
       AceFlags := AceFlags-[afInheritOnly];
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAudit.GetDeniedAce(Value: PACL);                                  }
{ GetDeniedAce method Override                                                 }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAudit.GetDeniedAce(Value: PACL);
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
{ TDescriptorAudit.GetAllowedAce(Value: PACL);                                 }
{ GetAllowedAce method Override                                                }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAudit.GetAllowedAce(Value: PACL);
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
{ TDescriptorAudit.GetDeniedSize: Integer;                                     }
{ GetDeniedSize method Override                                                }
{------------------------------------------------------------------------------}
 Function TDescriptorAudit.GetDeniedSize: Integer;
 Begin
    Result := 0;
    If MaskFlags[fkNo]=0 Then Exit;
    Result := SizeOf(TAceHeader)+SizeOf(TSystemAuditAce)+SizeOf(Byte)-SizeOf(DWORD);
    Inc(Result, GetLengthSid(Identifier.AsSID));
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAudit.GetAllowedSize: Integer;                                    }
{ GetAllowedSize method Override                                               }
{------------------------------------------------------------------------------}
 Function TDescriptorAudit.GetAllowedSize: Integer;
 Begin
    Result := 0;
    If (MaskFlags[fkYes]=0) Then Exit;
    Result := SizeOf(TAceHeader)+SizeOf(TSystemAuditAce)+SizeOf(Byte)-SizeOf(DWORD);
    Inc(Result, GetLengthSid(Identifier.AsSID));
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TDescriptorAuditFlags                                         }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TDescriptorAuditFlags.Create(Ace: TDescriptorAudit);                         }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TDescriptorAuditFlags.Create(Ace: TDescriptorAudit);
 Begin
    Inherited Create;
    FFlag := faNone;
    FAce := Ace;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAuditFlags.Assign(Source: TPersistent);                           }
{ Assign method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAuditFlags.Assign(Source: TPersistent);
 Begin
    Inherited Assign(Source);
    If (Source Is TDescriptorAuditFlags) Then
       With Source As TDescriptorAuditFlags Do Begin
          Self.Flag := Flag;
          Self.Data := Data;
       End;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAuditFlags.GetValue: TDescriptorAuditData;                        }
{ Read method property Data                                                    }
{------------------------------------------------------------------------------}
 Function TDescriptorAuditFlags.GetData: TDescriptorAuditData;
 Begin
    Result := FData;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAuditFlags.SetData(Const Value: TDescriptorAuditData);            }
{ Write method property Data                                                   }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAuditFlags.SetData(Const Value: TDescriptorAuditData);
 Begin
    If FAce.IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    FData := Value;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAuditFlags.GetFlag: TAuditFlagValue;                              }
{ Read method property Flag                                                    }
{------------------------------------------------------------------------------}
 Function TDescriptorAuditFlags.GetFlag: TAuditFlagValue;
 Begin
    Result := FFlag;
 End;

{------------------------------------------------------------------------------}
{ TDescriptorAuditFlags.SetFlag(Const Value: TAuditFlagValue);                 }
{ Write method property Flag                                                   }
{------------------------------------------------------------------------------}
 Procedure TDescriptorAuditFlags.SetFlag(Const Value: TAuditFlagValue);
 Begin
    If FAce.IsInherited Then
       Raise Exception.Create(SCannotModifyInherited);
    FFlag := Value;
 End;

{------------------------------------------------------------------------------}
{ End of acldesc.pas                                                           }
{------------------------------------------------------------------------------}
 End.
