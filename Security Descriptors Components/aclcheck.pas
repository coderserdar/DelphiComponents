{------------------------------------------------------------------------------}
{ aclcheck                                                                     }
{------------------------------------------------------------------------------}
{ Author(s) : Franck Musson (franck.musson@wanadoo.fr)                         }
{ Copyright : Franck Musson 1998-2002                                          }
{ Created   : 01/12/2001                                                       }
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
 Unit aclcheck;

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
    TAccessModeCheck = (amcCheck, amcInfo);

    TCustomAccessCheck = Class(TComponent)
    Private
      FMode: TAccessModeCheck;
      FActive: Boolean;
      FSecurityObject: TCustomSecurityObject;
      FGranted: LongBool;
      FDesiredAccess: DWORD;
      FGrantedAccess: DWORD;
      FStreamedActive: Boolean;
      Function GetAllowed: Boolean;
      Procedure SetAllowed(Const Value: Boolean);
      Function GetGrantedAccess: DWORD;
      Procedure SetGrantedAccess(Const Value: DWORD);
      Function GetDesiredAccess: DWORD;
      Procedure SetDesiredAccess(Const Value: DWORD);
      Function GetMode: TAccessModeCheck;
      Procedure SetMode(Const Value: TAccessModeCheck);
      Function GetSecurityObj: TCustomSecurityObject;
      Procedure SetSecurityObj(Const Value: TCustomSecurityObject);
      Procedure CheckInactive;
    Protected
      Function GetActive: Boolean;                                              Virtual;
      Procedure SetActive(Const Value: Boolean);                                Virtual;
      Procedure Connect;                                                        Virtual;
      Procedure DisConnect;                                                     Virtual;
      Property DesiredAccess: DWORD   Read GetDesiredAccess Write SetDesiredAccess;
      Property GrantedAccess: DWORD   Read GetGrantedAccess Write SetGrantedAccess Stored False;
      Property SecurityObject: TCustomSecurityObject
                                      Read GetSecurityObj   Write SetSecurityObj;
      Property Active: Boolean        Read GetActive        Write SetActive;
      Property Allowed: Boolean       Read GetAllowed       Write SetAllowed     Stored False;
      Property Mode: TAccessModeCheck Read GetMode          Write SetMode        Default amcCheck;
    Public
      Constructor Create(AOwner: TComponent);                                   Override;
      Destructor Destroy;                                                       Override;
      Procedure Loaded;                                                         Override;
    End;

    TAccessCheck = Class(TCustomAccessCheck)
    Published
      Property Active;
      Property Allowed;
      Property Mode;
      Property DesiredAccess;
      Property GrantedAccess;
      Property SecurityObject;
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
{ Constantes                                                                   }
{------------------------------------------------------------------------------}
 ResourceString
   SAlwaysActive     = 'Ne peut éffectuer cette opération sur un objet actif !';
   SNotAllowed       = 'Opération non autorisée !';
   SNotAssigned      = 'Ne peut éffectuer cette opération sur un objet non assigné !';

{------------------------------------------------------------------------------}
{ Provate types                                                                }
{------------------------------------------------------------------------------}
 Type
    THackSecurityObject = Class(TCustomSecurityObject);

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomAccessCheck                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomAccessCheck.Create(AOwner: TComponent);                               }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCustomAccessCheck.Create(AOwner: TComponent);
 Begin
    Inherited Create(AOwner);
    Active := False;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.Destroy;                                                  }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TCustomAccessCheck.Destroy;
 Begin
    Active := False;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.Loaded;                                                   }
{ Loaded Method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TCustomAccessCheck.Loaded;
 Begin
    Inherited Loaded;
    If FStreamedActive Then Active := True;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.CheckInactive;                                            }
{------------------------------------------------------------------------------}
 Procedure TCustomAccessCheck.CheckInactive;
 Begin
    If Active Then
       Raise Exception.Create(SAlwaysActive);
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.GetAllowed: Boolean;                                      }
{ Read method property allowed                                                 }
{------------------------------------------------------------------------------}
 Function TCustomAccessCheck.GetAllowed: Boolean;
 Begin
    Result := FGranted;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.SetAllowed(Const Value: Boolean);                         }
{ Write method property allowed                                                }
{------------------------------------------------------------------------------}
 Procedure TCustomAccessCheck.SetAllowed(Const Value: Boolean);
 Begin
    Raise Exception.Create(SNotAllowed);
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.GetGrantedAccess: DWORD;                                  }
{ Read method property GrantedAccess                                           }
{------------------------------------------------------------------------------}
 Function TCustomAccessCheck.GetGrantedAccess: DWORD;
 Begin
    Result := FGrantedAccess;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.SetGrantedAccess(Const Value: DWORD);                     }
{ Write method property GrantedAccess                                          }
{------------------------------------------------------------------------------}
 Procedure TCustomAccessCheck.SetGrantedAccess(Const Value: DWORD);
 Begin
    Raise Exception.Create(SNotAllowed);
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.GetDesiredAccess: DWORD;                                  }
{ Read method property DesiredAccess                                           }
{------------------------------------------------------------------------------}
 Function TCustomAccessCheck.GetDesiredAccess: DWORD;
 Begin
    If (FMode=amcCheck) Then
       Result := FDesiredAccess
    Else
       Result := MAXIMUM_ALLOWED;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.SetDesiredAccess(Const Value: DWORD);                     }
{ Write method property DesiredAccess                                          }
{------------------------------------------------------------------------------}
 Procedure TCustomAccessCheck.SetDesiredAccess(Const Value: DWORD);
 Begin
    CheckInactive;
    FDesiredAccess := Value;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.GetMode: TAccessModeCheck;                                }
{ Read method property Mode                                                    }
{------------------------------------------------------------------------------}
 Function TCustomAccessCheck.GetMode: TAccessModeCheck;
 Begin
    Result := FMode;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.SetMode(Const Value: TAccessModeCheck);                   }
{ Write method property Mode                                                   }
{------------------------------------------------------------------------------}
 Procedure TCustomAccessCheck.SetMode(Const Value: TAccessModeCheck);
 Begin
    CheckInactive;
    FMode := Value;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.GetActive: Boolean;                                       }
{ Read method property Active                                                  }
{------------------------------------------------------------------------------}
 Function TCustomAccessCheck.GetActive: Boolean;
 Begin
    Result := FActive;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.SetActive(Const Value: Boolean);                          }
{ Write method property active                                                 }
{------------------------------------------------------------------------------}
 Procedure TCustomAccessCheck.SetActive(Const Value: Boolean);
 Begin
    If (csLoading In ComponentState) Then Begin
       FStreamedActive := Value;
       Exit;
    End;
    If (Value=FActive) Then Exit;
    FActive := Value;
    Try
       If FActive Then
          Connect
       Else
          Disconnect;
    Except
       FActive := False;
       Raise;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.GetSecurityObj: TCustomSecurityObject;                    }
{ Read method property SecurityObject                                          }
{------------------------------------------------------------------------------}
 Function TCustomAccessCheck.GetSecurityObj: TCustomSecurityObject;
 Begin
    Result := FSecurityObject;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.SetSecurityObj(Const Value: TCustomSecurityObject);       }
{ Write method property SecurityObject                                         }
{------------------------------------------------------------------------------}
 Procedure TCustomAccessCheck.SetSecurityObj(Const Value: TCustomSecurityObject);
 Begin
    CheckInactive;
    FSecurityObject := Value;
    FSecurityObject.Options := [sdWantOwner, sdWantGroup, sdWantACL];
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.Connect;                                                  }
{------------------------------------------------------------------------------}
 Procedure TCustomAccessCheck.Connect;
 Var
    hack: THackSecurityObject;
    hToken: THandle;
    GenericMapping: TGenericMapping;
    dwPrivSetSize: DWORD;
    PrivilegeSet: TPrivilegeSet;
    WasActive: Boolean;

   {---------------------------------------------------------------------------}
    Procedure SaveRestore(Const Value: Boolean);
    Begin
       If Value Then
          Begin
             WasActive   := hack.Active;
             hack.Active := True;
          End
       Else If (hack.Active<>WasActive) Then
           hack.Active := WasActive;
    End;
   {---------------------------------------------------------------------------}

 Begin
    hack := THackSecurityObject(FSecurityObject);
    If (hack=Nil) Then
       Raise Exception.Create(SNotAssigned);
    SaveRestore(True);
    Try
       ImpersonateSelf(SecurityImpersonation);
       Try
          If Not OpenThreadToken(GetCurrentThread, TOKEN_QUERY Or TOKEN_READ, True, hToken) Then
             Raise Exception.Create(SysErrorMessage(GetLastError));
          Try
             Zeromemory(@GenericMapping, SizeOf(GenericMapping));
             Zeromemory(@PrivilegeSet, SizeOf(PrivilegeSet));
             dwPrivSetSize := SizeOf(PrivilegeSet);
             GenericMapping.GenericRead    := ACCESS_READ;
             GenericMapping.GenericWrite   := ACCESS_WRITE;
             GenericMapping.GenericExecute := ACCESS_EXEC;
             GenericMapping.GenericAll     := ACCESS_ALL;
             MapGenericMask(FDesiredAccess, GenericMapping);
             If Not AccessCheck(hack.Descriptor, hToken, DesiredAccess, GenericMapping,
                                PrivilegeSet, dwPrivSetSize, FGrantedAccess, FGranted) Then
                Raise Exception.Create(SysErrorMessage(GetLastError));
          Finally
             CloseHandle(hToken);
          End;
       Finally
          RevertToSelf;
       End;
    Finally
       SaveRestore(False);
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomAccessCheck.DisConnect;                                               }
{------------------------------------------------------------------------------}
 Procedure TCustomAccessCheck.DisConnect;
 Begin
    FGranted := False;
    FGrantedAccess := 0;
 End;

{------------------------------------------------------------------------------}
{ End of aclcheck.pas                                                          }
{------------------------------------------------------------------------------}
 End.
