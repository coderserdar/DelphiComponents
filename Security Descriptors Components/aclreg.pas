{------------------------------------------------------------------------------}
{ aclreg                                                                       }
{------------------------------------------------------------------------------}
{ Author(s) : Franck Musson (franck.musson@wanadoo.fr)                         }
{ Copyright : Franck Musson 1998-2000                                          }
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
 Unit aclreg;

{==============================================================================}
{                                                                              }
{                                                                              }
{ I N T E R F A C E                                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
 Interface

{------------------------------------------------------------------------------}
{ Uses Clauses                                                                 }
{------------------------------------------------------------------------------}
 Uses Windows, Classes, SysUtils;

 Procedure Register;

{==============================================================================}
{                                                                              }
{                                                                              }
{ I M P L E M E N T A T I O N                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
 Implementation

{$R aclreg.dcr}

{------------------------------------------------------------------------------}
{ Uses Clauses                                                                 }
{------------------------------------------------------------------------------}
 Uses dsnconst, dsgnintf, aclbase, acldesc, aclcom, aclfiles, aclcheck,
      aclregistry, aclconst, aclfuncs, Forms, Dialogs;

 Type
    THackSecurityID = Class(TCustomSecurityID);

{------------------------------------------------------------------------------}
{                                                                              }
{ TIsReadOnlyStringProperty                                                    }
{                                                                              }
{------------------------------------------------------------------------------}
 Type
    TIsReadOnlyStringProperty = Class(TStringProperty)
    Protected
      Function GetAttributes: TPropertyAttributes;                     Override;
    End;

{------------------------------------------------------------------------------}
{ TIsReadOnlyStringProperty.GetAttributes: TPropertyAttributes;                }
{------------------------------------------------------------------------------}
 Function TIsReadOnlyStringProperty.GetAttributes: TPropertyAttributes;
 Var
    pst: TCustomSecurityID;
 Begin
    Result := Inherited GetAttributes;
    Pst := GetComponent(0) As TCustomSecurityID;
    If (Pst=Nil) Then Exit;
    If THackSecurityID(pst).ReadOnly Then
       Result := [paReadOnly]
    Else
       Result := Inherited GetAttributes-[paMultiSelect];
 End;

{------------------------------------------------------------------------------}
{                                                                              }
{ TIsReadOnlyWellKnownProperty                                                 }
{                                                                              }
{------------------------------------------------------------------------------}
 Type
    TIsReadOnlyWellKnownProperty = Class(TEnumProperty)
    Protected
      Function GetAttributes: TPropertyAttributes;                     Override;
      Procedure Edit;                                                  Override;
    End;

{------------------------------------------------------------------------------}
{ TIsReadOnlyWellKnownProperty.GetAttributes: TPropertyAttributes;             }
{------------------------------------------------------------------------------}
 Function TIsReadOnlyWellKnownProperty.GetAttributes: TPropertyAttributes;
 Var
    pst: TCustomSecurityID;
 Begin
    Result := Inherited GetAttributes;
    Pst := GetComponent(0) As TCustomSecurityID;
    If (Pst=Nil) Then Exit;
    If THackSecurityID(pst).ReadOnly Then
       Result := [paReadOnly]
    Else
       Result := Inherited GetAttributes-[paMultiSelect];
 End;

{------------------------------------------------------------------------------}
{ TIsReadOnlyWellKnownProperty.Edit;                                           }
{------------------------------------------------------------------------------}
 Procedure TIsReadOnlyWellKnownProperty.Edit;
 Var
    pst: TCustomSecurityID;
 Begin
    Pst := GetComponent(0) As TCustomSecurityID;
    If (Pst=Nil) Then Exit;
    If Not THackSecurityID(pst).ReadOnly Then
       Inherited Edit;
 End;

{------------------------------------------------------------------------------}
{                                                                              }
{ TIsInhertedApplyToProperty                                                   }
{                                                                              }
{------------------------------------------------------------------------------}
 Type
    THackCustomAce = Class(TCustomACE);
    TIsInhertedApplyProperty = Class(TSetProperty)
    Protected
      Function GetAttributes: TPropertyAttributes;                     Override;
    End;

{------------------------------------------------------------------------------}
{ TIsInhertedRegApplyProperty.GetAttributes: TPropertyAttributes;              }
{------------------------------------------------------------------------------}
 Function TIsInhertedApplyProperty.GetAttributes: TPropertyAttributes;
 Var
    pst: TCustomACE;
 Begin
    Result := [paReadOnly];
    Pst := (GetComponent(0) As TCustomACE);
    If (Pst=Nil) Then Exit;
    If Not THackCustomAce(pst).IsInherited Then
       Result := Inherited GetAttributes-[paMultiSelect];
 End;

{------------------------------------------------------------------------------}
{                                                                              }
{ TIsInhertedFileModeProperty                                                  }
{                                                                              }
{------------------------------------------------------------------------------}
 Type
    TIsInhertedFileModeProperty = Class(TEnumProperty)
    Protected
      Function GetAttributes: TPropertyAttributes;                     Override;
      Procedure Edit;                                                  Override;
    End;

{------------------------------------------------------------------------------}
{ TIsInhertedFileModeProperty.GetAttributes: TPropertyAttributes;              }
{------------------------------------------------------------------------------}
 Function TIsInhertedFileModeProperty.GetAttributes: TPropertyAttributes;
 Var
    pst: TCustomNTFileAccess;
 Begin
    Result := [paReadOnly];
    Pst := (GetComponent(0) As TNTFileAccessFlags).ACE;
    If (Pst=Nil) Then Exit;
    If Not pst.IsInherited Then
       Result := Inherited GetAttributes-[paMultiSelect];
 End;

{------------------------------------------------------------------------------}
{ TIsInhertedFileModeProperty.Edit;                                            }
{------------------------------------------------------------------------------}
 Procedure TIsInhertedFileModeProperty.Edit;
 Var
    pst: TCustomNTFileAccess;
 Begin
    Pst := (GetComponent(0) As TNTFileAccessFlags).ACE;
    If (Pst=Nil) Then Exit;
    If Not pst.IsInherited Then
       Inherited Edit;
 End;

{------------------------------------------------------------------------------}
{                                                                              }
{ TIsInhertedDirModeProperty                                                   }
{                                                                              }
{------------------------------------------------------------------------------}
 Type
    TIsInhertedDirModeProperty = Class(TEnumProperty)
    Protected
      Function GetAttributes: TPropertyAttributes;                     Override;
      Procedure Edit;                                                  Override;
    End;

{------------------------------------------------------------------------------}
{ TIsInhertedDirModeProperty.GetAttributes: TPropertyAttributes;               }
{------------------------------------------------------------------------------}
 Function TIsInhertedDirModeProperty.GetAttributes: TPropertyAttributes;
 Var
    pst: TCustomNTFileAccess;
 Begin
    Result := [paReadOnly];
    Pst := (GetComponent(0) As TNTDirAccessFlags).ACE;
    If (Pst=Nil) Then Exit;
    If Not pst.IsInherited Then
       Result := Inherited GetAttributes-[paMultiSelect];
 End;

{------------------------------------------------------------------------------}
{ TIsInhertedDirModeProperty.Edit;                                             }
{------------------------------------------------------------------------------}
 Procedure TIsInhertedDirModeProperty.Edit;
 Var
    pst: TCustomNTFileAccess;
 Begin
    Pst := (GetComponent(0) As TNTDirAccessFlags).ACE;
    If (Pst=Nil) Then Exit;
    If Not pst.IsInherited Then
       Inherited Edit;
 End;

{------------------------------------------------------------------------------}
{                                                                              }
{ TIsInhertedRegModeProperty                                                   }
{                                                                              }
{------------------------------------------------------------------------------}
 Type
    TIsInhertedRegModeProperty = Class(TEnumProperty)
    Protected
      Function GetAttributes: TPropertyAttributes;                     Override;
      Procedure Edit;                                                  Override;
    End;

{------------------------------------------------------------------------------}
{ TIsInhertedRegModeProperty.GetAttributes: TPropertyAttributes;               }
{------------------------------------------------------------------------------}
 Function TIsInhertedRegModeProperty.GetAttributes: TPropertyAttributes;
 Var
    pst: TRegKeyAccess;
 Begin
    Result := [paReadOnly];
    Pst := (GetComponent(0) As TRegKeyAccessFlags).ACE;
    If (Pst=Nil) Then Exit;
    If Not pst.IsInherited Then
       Result := Inherited GetAttributes-[paMultiSelect];
 End;

{------------------------------------------------------------------------------}
{ TIsInhertedRegModeProperty.Edit;                                             }
{------------------------------------------------------------------------------}
 Procedure TIsInhertedRegModeProperty.Edit;
 Var
    pst: TRegKeyAccess;
 Begin
    Pst := (GetComponent(0) As TRegKeyAccessFlags).ACE;
    If (Pst=Nil) Then Exit;
    If Not pst.IsInherited Then
       Inherited Edit;
 End;


{------------------------------------------------------------------------------}
{                                                                              }
{ TReadOnlySecurityOwnerProperty                                               }
{                                                                              }
{------------------------------------------------------------------------------}
 Type
    THackSecurityObject = Class(TCustomSecurityObject);
    TReadOnlySecurityOwnerProperty = Class(TClassProperty)
    Protected
      Function GetAttributes: TPropertyAttributes;                     Override;
    End;

{------------------------------------------------------------------------------}
{ TReadOnlySecurityOwnerProperty.GetAttributes: TPropertyAttributes;           }
{------------------------------------------------------------------------------}
 Function TReadOnlySecurityOwnerProperty.GetAttributes: TPropertyAttributes;
 Var
    pst: TCustomSecurityObject;
 Begin
    Result := Inherited GetAttributes;
    Pst := (GetComponent(0) As TCustomSecurityObject);
    If (Pst=Nil) Then Exit;
    If (sdWantOwner In THackSecurityObject(pst).Options) And THackSecurityObject(pst).Active Then
       Result := Inherited GetAttributes-[paMultiSelect]
    Else
       Result := [paReadOnly];
 End;

{------------------------------------------------------------------------------}
{                                                                              }
{ TReadOnlySecurityGroupProperty                                               }
{                                                                              }
{------------------------------------------------------------------------------}
 Type
    TReadOnlySecurityGroupProperty = Class(TClassProperty)
    Protected
      Function GetAttributes: TPropertyAttributes;                     Override;
    End;

{------------------------------------------------------------------------------}
{ TReadOnlySecurityGroupProperty.GetAttributes: TPropertyAttributes;           }
{------------------------------------------------------------------------------}
 Function TReadOnlySecurityGroupProperty.GetAttributes: TPropertyAttributes;
 Var
    pst: TCustomSecurityObject;
 Begin
    Result := Inherited GetAttributes;
    Pst := (GetComponent(0) As TCustomSecurityObject);
    If (Pst=Nil) Then Exit;
    If (sdWantGroup In THackSecurityObject(pst).Options) And THackSecurityObject(pst).Active Then
       Result := Inherited GetAttributes-[paMultiSelect]
    Else
       Result := [paReadOnly];
 End;

{------------------------------------------------------------------------------}
{                                                                              }
{ TReadOnlyStringProperty                                                      }
{                                                                              }
{------------------------------------------------------------------------------}
 Type
    TReadOnlyStringProperty = Class(TStringProperty)
    Protected
      Function GetAttributes: TPropertyAttributes;                     Override;
    End;

{------------------------------------------------------------------------------}
{ TReadOnlyStringProperty.GetAttributes: TPropertyAttributes;                  }
{------------------------------------------------------------------------------}
 Function TReadOnlyStringProperty.GetAttributes: TPropertyAttributes;
 Begin
    Result := [paReadOnly];
 End;

{------------------------------------------------------------------------------}
{                                                                              }
{ TReadOnlyBooleanProperty                                                     }
{                                                                              }
{------------------------------------------------------------------------------}
 Type
    TReadOnlyBooleanProperty = Class(TBoolProperty)
    Protected
      Function GetAttributes: TPropertyAttributes;                     Override;
      Procedure Edit;                                                  Override;
    End;

{------------------------------------------------------------------------------}
{ TReadOnlyBooleanProperty.GetAttributes: TPropertyAttributes;                 }
{------------------------------------------------------------------------------}
 Function TReadOnlyBooleanProperty.GetAttributes: TPropertyAttributes;
 Begin
    Result := [paReadOnly];
 End;

{------------------------------------------------------------------------------}
{ TReadOnlyBooleanProperty.Edit;                                               }
{------------------------------------------------------------------------------}
 Procedure TReadOnlyBooleanProperty.Edit;
 Begin
    { Nothing }
 End;

{------------------------------------------------------------------------------}
{                                                                              }
{ TReadOnlyCheckFlagsProperty                                                  }
{                                                                              }
{------------------------------------------------------------------------------}
 Type
    TReadOnlyCheckFlagsProperty = Class(TEnumProperty)
    Protected
      Function GetAttributes: TPropertyAttributes;                     Override;
      Procedure Edit;                                                  Override;
    End;

{------------------------------------------------------------------------------}
{ TReadOnlyCheckFlagsProperty.Edit;                                            }
{------------------------------------------------------------------------------}
 Procedure TReadOnlyCheckFlagsProperty.Edit;
 Var
    pst: TCustomNTFileCheckFlags;
 Begin
    Pst := (GetComponent(0) As TCustomNTFileCheckFlags);
    If (Pst=Nil) Then Exit;
    If Not Pst.ReadOnly Then
       Inherited Edit;
 End;

{------------------------------------------------------------------------------}
{ TReadOnlyCheckFlagsProperty.GetAttributes: TPropertyAttributes;              }
{------------------------------------------------------------------------------}
 Function TReadOnlyCheckFlagsProperty.GetAttributes: TPropertyAttributes;
 Var
    pst: TCustomNTFileCheckFlags;
 Begin
    Result := Inherited GetAttributes;
    Pst := (GetComponent(0) As TCustomNTFileCheckFlags);
    If (Pst=Nil) Then Exit;
    If Not Pst.ReadOnly Then
       Result := Inherited GetAttributes-[paMultiSelect]
    Else
       Result := [paReadOnly];
 End;

{------------------------------------------------------------------------------}
{ Register procedure;                                                          }
{------------------------------------------------------------------------------}
 Procedure Register;
 Begin
    RegisterComponents(srSystem, [TSecurityDescriptorObject, TRegValueSecurity, TNTFileSecurity, TNTDirectorySecurity, TRegKeySecurity]);
    RegisterComponents(srSystem, [TAccessCheck, TRegValueAccessCheck, TNTFileSecurityCheck, TNTDirectorySecurityCheck, TRegKeySecurityCheck]);

    RegisterPropertyEditor(TypeInfo(Boolean), TCustomACE, 'IsInherited',           TReadOnlyBooleanProperty);

    RegisterPropertyEditor(TypeInfo(TNTFileFlagsMode), TNTFileCheckFlags      , '', TReadOnlyCheckFlagsProperty);
    RegisterPropertyEditor(TypeInfo(TNTDirFlagsMode) , TNTDirCheckFlags       , '', TReadOnlyCheckFlagsProperty);
    RegisterPropertyEditor(TypeInfo(TCheckFlagValue) , TCustomNTFileCheckFlags, '', TReadOnlyCheckFlagsProperty);

    RegisterPropertyEditor(TypeInfo(TCreatorSecurityID), TCustomSecurityObject, 'CreatorOwner', TReadOnlySecurityOwnerProperty);
    RegisterPropertyEditor(TypeInfo(TCreatorSecurityID), TCustomSecurityObject, 'CreatorGroup', TReadOnlySecurityGroupProperty);

    { Customs ACES }
    RegisterPropertyEditor(TypeInfo(TNTRegFlagsMode),  TRegKeyAccessFlags, '', TIsInhertedRegModeProperty);
    RegisterPropertyEditor(TypeInfo(TAccessFlagValue), TRegKeyAccessFlags, '', TIsInhertedRegModeProperty);

    RegisterPropertyEditor(TypeInfo(TNTFileFlagsMode), TNTFileAccessFlags, '', TIsInhertedFileModeProperty);
    RegisterPropertyEditor(TypeInfo(TAccessFlagValue), TNTFileAccessFlags, '', TIsInhertedFileModeProperty);

    RegisterPropertyEditor(TypeInfo(TNTDirFlagsMode),  TNTDirAccessFlags , '', TIsInhertedDirModeProperty);
    RegisterPropertyEditor(TypeInfo(TAccessFlagValue), TNTDirAccessFlags , '', TIsInhertedDirModeProperty);

    { Apply Flags }
    RegisterPropertyEditor(TypeInfo(TRegKeyApplyFlags), TCustomRegKeyAccess, '', TIsInhertedApplyProperty);
    RegisterPropertyEditor(TypeInfo(TNTFileApplyFlags), TCustomNTFileAccess, '', TIsInhertedApplyProperty);

    RegisterPropertyEditor(TypeInfo(String), TCustomSecurityID, 'Name',             TIsReadOnlyStringProperty);
    RegisterPropertyEditor(TypeInfo(String), TCustomSecurityID, 'AsString',         TReadOnlyStringProperty);
    RegisterPropertyEditor(TypeInfo(TWellKnownSid), TCustomSecurityID, 'WellKnown', TIsReadOnlyWellKnownProperty);
 End;

{------------------------------------------------------------------------------}
{ End of aclreg.pas                                                            }
{------------------------------------------------------------------------------}
 End.
