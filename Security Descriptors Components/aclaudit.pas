{------------------------------------------------------------------------------}
{ aclaudit                                                                     }
{------------------------------------------------------------------------------}
{ Author(s) : Franck Musson (franck.musson@wanadoo.fr)                         }
{ Copyright : Franck Musson 1998-2001                                          }
{ Created   : 01/10/2001                                                       }
{ Version   : 1.000 (Delphi 5)                                                 }
{------------------------------------------------------------------------------}
 Unit aclaudit;

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
    Windows, SysUtils, Classes, aclbase, aclconst;

{------------------------------------------------------------------------------}
{ Public Types                                                                 }
{------------------------------------------------------------------------------}
 Type
    TAuditACE =Class;

    TAuditACL = Class(TSACL)
    Private
      Function GetItems(Index: Integer): TAuditACE;
      Procedure SetItems(Index: Integer; Const Value: TAuditACE);
    Public
      Function Add: TAuditACE;                                                  Reintroduce; Virtual;
      Property Items[Index: Integer]: TAuditACE Read GetItems Write SetItems; Default;
    End;

    TAuditACE = Class(TCustomACE)
    Published
      Property AceFlags;
      Property IsInherited;
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
{ Implementation TAuditACL                                                     }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TAuditACL.GetItems(...): TAuditACE;                                          }
{ Read Method property Items                                                   }
{------------------------------------------------------------------------------}
 Function TAuditACL.GetItems(Index: Integer): TAuditACE;
 Begin
    Result := TAuditACE(Inherited Items[Index]);
 End;

{------------------------------------------------------------------------------}
{ TAuditACL.SetItems(...);                                                     }
{ Write Method property Items                                                  }
{------------------------------------------------------------------------------}
 Procedure TAuditACL.SetItems(Index: Integer; Const Value: TAuditACE);
 Begin
    Inherited Items[Index] := Value;
 End;

{------------------------------------------------------------------------------}
{ TAuditACL.Add(...): TAuditACE;                                               }
{ Add Method Overload                                                          }
{------------------------------------------------------------------------------}
 Function TAuditACL.Add: TAuditACE;
 Begin
    Result := TAuditACE(Inherited Add);
 End;

{------------------------------------------------------------------------------}
{ End of aclaudit.pas                                                          }
{------------------------------------------------------------------------------}
 End.
