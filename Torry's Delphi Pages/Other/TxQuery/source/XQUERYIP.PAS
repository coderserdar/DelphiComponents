{**************************************************************************}
{   TxQuery DataSet                                                        }
{                                                                          }
{   The contents of this file are subject to the Mozilla Public License    }
{   Version 1.1 (the "License"); you may not use this file except in       }
{   compliance with the License. You may obtain a copy of the License at   }
{   http://www.mozilla.org/MPL/                                            }
{                                                                          }
{   Software distributed under the License is distributed on an "AS IS"    }
{   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the}
{   License for the specific language governing rights and limitations     }
{   under the License.                                                     }
{                                                                          }
{   The Original Code is xqueryip.pas                            }
{                                                                          }
{   The Initial Developer of the Original Code is Alfonso Moreno.          }
{   Portions created by Alfonso Moreno are Copyright (C) Alfonso Moreno.   }
{   All Rights Reserved.                                                   }
{                                                                          }
{   Alfonso Moreno (Hermosillo, Sonora, Mexico)                            }
{   email: luisarvayo@yahoo.com                                            }
{     url: http://www.ezsoft.com                                           }
{          http://www.sigmap.com/txquery.htm                               }
{                                                                          }
{   Contributor(s): Chee-Yang, CHAU (Malaysia) <cychau@gmail.com>          }
{                   Sherlyn CHEW (Malaysia)                                }
{              url: http://code.google.com/p/txquery/                      }
{                   http://groups.google.com/group/txquery                 }
{                                                                          }
{**************************************************************************}

Unit xqueryip;
(* With this unit, TxQuery can be used with InfoPower.
You need to have InfoPower 3.01 or later (tested with InfoPower 4)
download from Woll2Woll website at http://www.woll2woll.com
New: Infopower 2000 doesn't need this *)

Interface

Uses
  SysUtils,
  WinTypes,
  WinProcs,
  Classes,
  wwtypes,
  xquery;

Type
  TwwXQuery = Class( TxQuery )
  Private
    FControlType: TStrings;
    FPictureMasks: TStrings;
    FUsePictureMask: boolean;
    FOnInvalidValue: TwwInvalidValueEvent;

    Function GetControlType: TStrings;
    Procedure SetControlType( sel: TStrings );
    Function GetPictureMasks: TStrings;
    Procedure SetPictureMasks( sel: TStrings );

  Protected
    Procedure DoBeforePost; Override; { For picture support }

  Public
    Constructor Create( AOwner: TComponent ); Override;
    Destructor Destroy; Override;

  Published
    Property ControlType: TStrings Read GetControlType Write setControltype;
    Property PictureMasks: TStrings Read GetPictureMasks Write SetPictureMasks;
    Property ValidateWithMask: boolean Read FUsePictureMask Write FUsePictureMask;
    Property OnInvalidValue: TwwInvalidValueEvent Read FOnInvalidValue Write FOnInvalidValue;
  End;

Procedure Register;

Implementation
Uses
  wwcommon, dbconsts;

Constructor TwwXQuery.create( AOwner: TComponent );
Begin
  Inherited Create( AOwner );
  FControlType := TStringList.create;
  FPictureMasks := TStringList.create;
  FUsePictureMask := True;
End;

Destructor TwwXQuery.Destroy;
Begin
  FControlType.Free;
  FPictureMasks.Free;
  FPictureMasks := Nil;
  Inherited Destroy;
End;

Function TwwXQuery.GetControltype: TStrings;
Begin
  Result := FControlType;
End;

Procedure TwwXQuery.SetControlType( sel: TStrings );
Begin
  FControlType.Assign( sel );
End;

Function TwwXQuery.GetPictureMasks: TStrings;
Begin
  Result := FPictureMasks
End;

Procedure TwwXQuery.SetPictureMasks( sel: TStrings );
Begin
  FPictureMasks.Assign( sel );
End;

Procedure TwwXQuery.DoBeforePost;
Begin
  Inherited DoBeforePost;
  If FUsePictureMask Then
    wwValidatePictureFields( self, FOnInvalidValue );
End;

Procedure Register;
Begin
  RegisterComponents( 'IP Access', [TwwXQuery] );
End;

End.
