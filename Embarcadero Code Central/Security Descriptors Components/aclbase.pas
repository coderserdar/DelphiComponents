{------------------------------------------------------------------------------}
{ aclbase                                                                      }
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
 Unit aclbase;

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
    Windows, ActiveX, SysUtils, Classes, aclconst, aclfuncs, Dialogs;

{------------------------------------------------------------------------------}
{ Public Types                                                                 }
{------------------------------------------------------------------------------}
 Type
    TAceStatus = (asnone, asadded, asmodified, asdeleted);

    TCustomACL = Class;
    TCustomACLClass = Class Of TCustomACL;
    TDACL = Class;
    TSACL = Class;

    TCustomACE = Class;
    TCustomACEClass = Class Of TCustomACE;

    TCustomSecurityID = Class;
    TSecurityID = Class;
    TCreatorSecurityID = Class;

    TCustomSecurityObject = Class(TComponent)
    Private
      FActive       : Boolean;
      FAutoSave     : Boolean;
      FStreamed     : Boolean;
      FComputerName : String;
      FSystemName   : String;
      FAccessList   : TDACL;
      FAuditList    : TSACL;
      FOwnerID      : TCreatorSecurityID;
      FGroupID      : TCreatorSecurityID;
      FDescriptor   : PSecurityDescriptor;
      FShadowSD     : PSecurityDescriptor;
      FRequested    : TSDInformations;
      FShowLoadErrs : Boolean;
      FDomainSID    : PSID;
      Function  GetActive: Boolean;
      Procedure SetActive(Const Value: Boolean);
      Function  GetComputer: String;
      Procedure SetComputer(Const Value: String);
      Function  GetSystem: String;
      Procedure SetSystem(Const Value: String);
      Function  GetDomainSID: PSID;
      Function  GetSD: PSecurityDescriptor;
      Procedure SetSD(Const Value: PSecurityDescriptor);
      Function  GetShadowSD: PSecurityDescriptor;
      Procedure SetShadowSD(Const Value: PSecurityDescriptor = Nil);
      Function  GetTrueSD: PSecurityDescriptor;
      Function  GetSecInf: DWORD;
      Function  GetRequested: TSDInformations;
      Procedure SetRequested(Const Value: TSDInformations);
      Procedure OpenStreamed;
    Protected
      Procedure CheckSD;
      Function  ReadObjectDescriptor: PSecurityDescriptor;                      Virtual; Abstract;
      Procedure WriteObjectDescriptor;                                          Virtual; Abstract;
      Procedure Load;                                                           Virtual;
      Procedure Unload;                                                         Virtual;
      Procedure CleanUp;                                                        Virtual;
      Procedure Reset;                                                          Virtual;
      Function  CreateAccessList: TDACL;                                        Virtual; Abstract;
      Procedure FreeAccessList;                                                 Virtual;
      Function  GetAccessList: TDACL;                                           Virtual;
      Function  CreateAuditList: TSACL;                                         Virtual; Abstract;
      Procedure FreeAuditList;                                                  Virtual;
      Function  GetAuditList: TSACL;                                            Virtual;
      Function  CreateOwnerID: TCreatorSecurityID;                              Virtual; Abstract;
      Procedure FreeOwnerID;                                                    Virtual;
      Function  GetOwnerID: TCreatorSecurityID;                                 Virtual;
      Function  CreateGroupID: TCreatorSecurityID;                              Virtual; Abstract;
      Procedure FreeGroupID;                                                    Virtual;
      Function  GetGroupID: TCreatorSecurityID;                                 Virtual;
      Property  ComputerName: String                  Read GetComputer      Write SetComputer;
      Property  LookupSystemName: String              Read GetSystem        Write SetSystem;
      Property  DomainSID: PSID                       Read GetDomainSID;
      Property  Descriptor: PSecurityDescriptor       Read GetSD            Write SetSD;
      Property  ShadowDescriptor: PSecurityDescriptor Read GetShadowSD      Write SetShadowSD;
      Property  TrueDescriptor: PSecurityDescriptor   Read GetTrueSD;
      Property  SecurityInf: DWORD                    Read GetSecInf;
    Public
      Constructor Create(AOwner: TComponent);                                   Override;
      Destructor Destroy;                                                       Override;
      Procedure Loaded;                                                         Override;
      Procedure Save;                                                           Virtual;
      Procedure LoadFromRegistry(Const Computer: String; Const RootKey: HKey;
                                 Const SubKey: String; Const Value: String);    Virtual;
      Procedure SaveToRegistry(Const Computer: String; Const RootKey: HKey;
                                 Const SubKey: String; Const Value: String);    Virtual;
      Procedure LoadFromFile(Const FileName: TFileName;
                                 Const xmlFormat: Boolean = False);             Virtual;
      Procedure SaveToFile(Const FileName: TFileName;
                                 Const xmlFormat: Boolean = False);             Virtual;
      Procedure LoadFromStream(Stream: TStream);                                Overload; Virtual;
      Procedure SaveToStream(Stream: TStream);                                  Overload; Virtual;
      Procedure LoadFromStream(Stm: IStream);                                   Overload; Virtual;
      Procedure SaveToStream(Stm: IStream);                                     Overload; Virtual;
      Property  Options: TSDInformations              Read GetRequested     Write SetRequested     Default [sdWantACL];      
      Property  Active: Boolean                       Read GetActive        Write SetActive        Default False;
      Property  AutoSave: Boolean                     Read FAutoSave        Write FAutoSave        Default False;
      Property  ShowLoadErrors: Boolean               Read FShowLoadErrs    Write FShowLoadErrs    Default False;
    End;

    TCustomACL = Class(TOwnedCollection)
    Private
      FObject     : TCustomSecurityObject;
      FKind       : TACLKind;
      FInheritMode: TInheritMode;
      Function  GetSystem: String;
      Procedure SetSystem(Const Value: String);
      Function  GetDomainSID: PSID;
      Function  InternalGetACECount(Shadow: Boolean): Integer;
      Function  InternalGetACE(Shadow: Boolean; Index: Integer): PACE;
      Function  GetACL(Shadow: Boolean): PACL;
      Procedure SetACL(Shadow: Boolean; Const Value: PACL);
      Function  GetSD(Shadow: Boolean): PSecurityDescriptor;
      Procedure SetSD(Shadow: Boolean; Const Value: PSecurityDescriptor);
      Function  GetTrueSD: PSecurityDescriptor;
      Function  GetInherit: TInheritMode;
      Procedure SetInherit(Const Value: TInheritMode);
    Protected
      Procedure LoadACL;                                                        Virtual;
      Procedure SaveACL;                                                        Virtual;
      Function  InitACL: PACL;                                                  Virtual;
      Procedure FreeACL;                                                        Virtual;
      Procedure CleanUpACL;                                                     Virtual;
      Procedure DoDefaultInherit;                                               Virtual;
      Procedure DoCopyInherit;                                                  Virtual;
      Procedure DoDeleteInherit;                                                Virtual;
      Property  Descriptor[Shadow: Boolean]: PSecurityDescriptor
                                                         Read GetSD       Write SetSD;
      Property  TrueDescriptor: PSecurityDescriptor      Read GetTrueSD;
      Property  ACL[Shadow: Boolean]: PACL               Read GetACL      Write SetACL;
      Property  LookupSystemName: String                 Read GetSystem   Write SetSystem;
      Property  DomainSID: PSID                          Read GetDomainSID;
      Property  VCLSecurityObject: TCustomSecurityObject Read FObject;
      Property  InheritMode: TInheritMode                Read GetInherit  Write SetInherit Default imDefault;
    Public
      Constructor Create(AOwner: TPersistent;
                         Obj: TCustomSecurityObject;
                         ItemClass: TCustomACEClass);                           Reintroduce; Virtual;
      Destructor Destroy;                                                       Override;
      Function Add: TCustomACE;                                                 
    End;

    TDACL = Class(TCustomACL)
    Public
      Constructor Create(AOwner: TPersistent;
                         Obj: TCustomSecurityObject;
                         ItemClass: TCustomACEClass);                           Override;
    Published
      Property InheritMode;
    End;

    TSACL = Class(TCustomACL)
    Public
      Constructor Create(AOwner: TPersistent;
                         Obj: TCustomSecurityObject;
                         ItemClass: TCustomACEClass);                           Override;
    Published
      Property InheritMode;
    End;

    TCustomACE = Class(TCollectionItem)
    Private
      FAce         : PAce;
      FAceSize     : Cardinal;
      FAllowedMask : Cardinal;
      FDeniedMask  : Cardinal;
      FAceFlags    : TAceFlagTypes;
      FAceType     : TAceEntryType;
      FStatus      : TAceStatus;
      FSecurityID  : TSecurityID;
      Function GetSize: Cardinal;
      Function GetAceFlags: TAceFlagTypes;
      Procedure SetAceFlags(Const Value: TAceFlagTypes);
      Function GetAceType: TAceEntryType;
      Procedure SetAceType(Const Value: TAceEntryType);
      Function GetIsInherited: Boolean;
      Procedure SetIsInherited(Const Value: Boolean);
      Function GetMaskFlags(Kind: TFlagKind): Cardinal;
      Procedure SetMaskFlags(Kind: TFlagKind; Const Value: Cardinal);
      Function GetAceStatus: TAceStatus;
      Procedure SetAceStatus(Const Value: TAceStatus);
      Function GetSystem: String;
      Procedure SetSystem(Const Value: String);
      Function GetDomainSID: PSID;
      Function GetSecurityID: TSecurityID;
      Procedure SetSecurityID(Const Value: TSecurityID);
    Protected
      Function  GetAcePtr: PAce;                                                Virtual;
      Procedure SetAcePtr(Const Value: PAce);                                   Virtual;
      Procedure Reset;                                                          Virtual;
      Procedure GetDeniedAce(Value: PACL);                                      Virtual; Abstract;
      Procedure GetAllowedAce(Value: PACL);                                     Virtual; Abstract;
      Function GetDeniedSize: Integer;                                          Virtual; Abstract;
      Function GetAllowedSize: Integer;                                         Virtual; Abstract;
      Property Ace: PAce                   Read GetAcePtr         Write SetAcePtr;
      Property AceSize: Cardinal           Read GetSize;
      Property AceType: TAceEntryType      Read GetAceType        Write SetAceType;
      Property AceFlags: TAceFlagTypes     Read GetAceFlags       Write SetAceFlags;
      Property IsInherited: Boolean        Read GetIsInherited    Write SetIsInherited  Stored False;
      Property LookupSystemName: String    Read GetSystem         Write SetSystem;
      Property DomainSID: PSID             Read GetDomainSID;
      Property Status: TAceStatus          Read GetAceStatus      Write SetAceStatus;
    Public
      Constructor Create(Collection: TCollection);                              Override;
      Destructor Destroy;                                                       Override;
      Property MaskFlags[Kind: TFlagKind]: Cardinal
                                           Read GetMaskFlags      Write SetMaskFlags;
    Published
      Property Identifier: TSecurityID     Read GetSecurityID     Write SetSecurityID;
    End;

    TCustomSecurityID = Class(TPersistent)
    Private
      FSID         : PSID;
      FDomainSID   : PSID;
      FType        : TSIDType;
      FName        : String;
      FString      : String;
      FSystemName  : String;
      FReadOnly    : Boolean;
      FWellKnownSID: TWellKnownSid;
      Function  GetType: TSidType;
      Function  GetName: String;
      Procedure SetName(Value: String);
      Function  GetAsString: String;
      Procedure SetAsString(Const Value: String);
      Function  GetAsSID: PSID;
      Procedure SetAsSID(Const Value: PSID);
      Function  GetAsText: String;
      Function  GetIsvalid: Boolean;
      Procedure ClearSid;
      Procedure GetSidInfos;
      Procedure AllocSid(Const asid: PSid);
      Procedure LoadSid(Const Value: String);
      Function  GetSidSize(Const Value: String): Cardinal;
      Function  GetWellKnown: TWellKnownSid;
      Procedure SetWellKnown(Const Value: TWellKnownSid);
    Protected
      Procedure Reset;
      Function  GetReadOnly: Boolean;                                           Virtual;
      Procedure SetReadOnly(Const Value: Boolean);                              Virtual;
      Property ReadOnly: Boolean          Read GetReadOnly        Write SetReadOnly;
    Public
      Destructor Destroy;                                                       Override;
      Property LookUpSystemName: String   Read FSystemName        Write FSystemName;
      Property DomainSID: PSID            Read FDomainSID         Write FDomainSID;
      Property AsString: String           Read GetAsString        Write SetAsString;
      Property AsSID: PSID                Read GetAsSid           Write SetAsSid;
      Property AsText: String             Read GetAsText;
      Property AsType: TSidType           Read GetType;
      Property Name: String               Read GetName            Write SetName;
      Property IsValid: Boolean           Read GetIsvalid;
      Property WellKnown: TWellKnownSid   Read GetWellKnown       Write SetWellKnown;
    End;

    TSecurityID = Class(TCustomSecurityID)
    Published
      Property Name;
      Property AsString;
      Property WellKnown;
    End;

    TCreatorSecurityID = Class(TCustomSecurityID)
    Private
      FObject: TCustomSecurityObject;
      FKind  : TACLKind;
      Function GetObjSID: PSID;
      Procedure SetObjSID(Const Value: PSID);
      Function  GetSD:PSecurityDescriptor;
      Procedure SetSD(Const Value: PSecurityDescriptor);
    Protected
      Procedure LoadID;                                                         Virtual;
      Procedure SaveID;                                                         Virtual;
      Procedure FreeID;                                                         Virtual;
      Procedure CleanUpID;                                                      Virtual;
      Function  GetReadOnly: Boolean;                                           Override;
      Procedure SetReadOnly(Const Value: Boolean);                              Override;
      Property  ObjectSID: PSID                 Read GetObjSID   Write SetObjSID;
      Property  Descriptor: PSecurityDescriptor Read GetSD       Write SetSD;
    Public
      Constructor Create(AObject: TCustomSecurityObject);                       Reintroduce; Overload; Virtual;
      Destructor Destroy;                                                       Override;
    Published
      Property Name;
      Property AsString;
      Property WellKnown;
    End;

    TCreatorGroupID = Class(TCreatorSecurityID)
    Public
      Constructor Create(AObject: TCustomSecurityObject);                       Override;
    End;

    TCreatorOwnerID = Class(TCreatorSecurityID)
    Public
      Constructor Create(AObject: TCustomSecurityObject);                       Override;
    End;

    Function MakeAbsoluteSecurityDescriptor(Value: PSecurityDescriptor): PSecurityDescriptor;
    Function MakeSelfRelativeSecurityDescriptor(Const Value: PSecurityDescriptor; Var len: Cardinal): PSecurityDescriptor;

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
 Uses
     ComObj, Consts, aclFiles;

{------------------------------------------------------------------------------}
{ ShowACLException(E: Exception);                                              }
{------------------------------------------------------------------------------}
 Procedure ShowACLException(E: Exception);
 Var
   Msg: string;
   ActiveWindow: HWnd;
 Begin
    Msg := E.Message;
    If (Msg<>'') And (AnsiLastChar(Msg)>'.') Then Msg := 'ici my pote : '+Msg+'.';
    ActiveWindow := GetActiveWindow;
    Try
       Windows.MessageBox(GetDeskTopWindow, PChar(Msg), PChar(SMsgDlgError), MB_OK+MB_ICONSTOP);
    Finally
       SetActiveWindow(ActiveWindow);
    End;
 End;

{------------------------------------------------------------------------------}
{ MakeAbsoluteSecurityDescriptor(...): PSecurityDescriptor;                    }
{------------------------------------------------------------------------------}
 Function MakeAbsoluteSecurityDescriptor(Value: PSecurityDescriptor): PSecurityDescriptor;
 Var
   rlen     : Cardinal;
   rdacl    : TACL;
   rdacllen : Cardinal;
   rsacl    : TACL;
   rsacllen : Cardinal;
   rowner   : PSID;
   rownerlen: Cardinal;
   rgroup   : PSID;
   rgrouplen: Cardinal;
   Err      : DWORD;

 Begin
    Result := Nil;
    If Not IsValidSecurityDescriptor(Value) Then
       RaiseACLError(ERROR_INVALID_PARAMETER);
    rowner := Nil;
    rgroup := Nil;
    rlen := 0;
    rdacllen := 0;
    rsacllen := 0;
    rownerlen := 0;
    rgrouplen := 0;
    Zeromemory(@rdacl, SizeOf(TACL));
    Zeromemory(@rsacl,  SizeOf(TACL));
    If Not MakeAbsoluteSD(Value, Result, rlen, rdacl, rdacllen, rsacl, rsacllen, rowner, rownerlen, rgroup, rgrouplen) Then Begin
       Err := GetLastError;
       If (Err<>ERROR_SUCCESS) And (Err<>ERROR_INSUFFICIENT_BUFFER) Then
          RaiseACLError(Err)
       Else
          SetLastError(0);
    End;
    Try
       Result := sdAlloc(rlen);
       If (Result<>Nil) Then
          Begin
             Result^.Dacl  := sdAlloc(rdacllen);
             Result^.Sacl  := sdAlloc(rsacllen);
             Result^.Owner := sdAlloc(rownerlen);
             Result^.Group := sdAlloc(rgrouplen);
             If Not MakeAbsoluteSD(Value, Result, rlen,
                                          Result^.Dacl^, rdacllen,
                                          Result^.Sacl^, rsacllen,
                                          Result^.Owner, rownerlen,
                                          Result^.group, rgrouplen) Then RaiseACLError;
          End;
    Except
       If (Result<>Nil) Then Begin
          sdFree(Result^.Dacl, rdacllen);
          sdFree(Result^.Sacl, rsacllen);
          sdFree(Result^.Owner, rownerlen);
          sdFree(Result^.Group, rgrouplen);
          sdFree(Result, rlen);
       End;
       Raise;
    End;
 End;

{------------------------------------------------------------------------------}
{ MakeSelfRelativeSecurityDescriptor(...): PSecurityDescriptor;                }
{------------------------------------------------------------------------------}
 Function MakeSelfRelativeSecurityDescriptor(Const Value: PSecurityDescriptor; Var len: Cardinal): PSecurityDescriptor;
 Var
    Err: DWORD;
 Begin
    Result := Nil;
    If Not IsValidSecurityDescriptor(Value) Then
       RaiseACLError(ERROR_INVALID_PARAMETER);
    len := 0;
    If Not MakeSelfRelativeSD(Value, Result, len) Then Begin
       Err := GetLastError;
       If (Err<>ERROR_SUCCESS) And (Err<>ERROR_INSUFFICIENT_BUFFER) Then
          RaiseACLError(Err)
       Else
          SetLastError(0);
    End;
    Result := sdAlloc(len);
    Try
       InitializeSecurityDescriptor(Result, SECURITY_DESCRIPTOR_REVISION);
       If Not MakeSelfRelativeSD(Value, Result, len) Then
          RaiseACLError;
    Except
       sdFree(Result, len);
       Raise;
    End;
 End;

{------------------------------------------------------------------------------}
{ LoadSecurityDescriptorFromRegistry(...);                                     }
{ Saves in a binary and transportable format                                   }
{------------------------------------------------------------------------------}
 Function LoadSecurityDescriptorFromRegistry(Const Computer: String; Root: HKEY; Const Path, Value: String): PSecurityDescriptor;
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
{ SaveSecurityDescriptorToRegistry(...);                                       }
{ Saves in a binary and transportable format                                   }
{------------------------------------------------------------------------------}
 Procedure SaveSecurityDescriptorToRegistry(Const Computer: String; Root: HKEY; Const Path, Value: String; sd: PSecurityDescriptor);
 Var
    Err    : Cardinal;
    desc   : PSecurityDescriptor;
    dlen   : Cardinal;
    disp   : Cardinal;
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
    If (Err=ERROR_FILE_NOT_FOUND) Then
       Err := RegCreateKeyEx(RootKey, PChar(Path), 0, Nil, REG_OPTION_NON_VOLATILE, KEY_CREATE_SUB_KEY Or KEY_WRITE {KEY_ALL_ACCESS}, Nil, TempKey, @disp);
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

{------------------------------------------------------------------------------}
{ LoadSecurityDescriptorFromFile(...);                                         }
{ Load a SecurityDescriptor form a File                                        }
{------------------------------------------------------------------------------}
 Function LoadSecurityDescriptorFromFile(Const FileName: String): PSecurityDescriptor;
 Var
    dlen: Cardinal;
    desc: PSecurityDescriptor;
    Stm : TFileSTream;
 Begin
    Result := Nil;
    Stm := TFileStream.Create(FileName, fmOpenRead Or fmShareDenyWrite);
    Try
       dlen := Stm.Size;
       desc := sdAlloc(dlen);
       Try
          Stm.Position := 0;
          Stm.ReadBuffer(desc^, dlen);
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
       Stm.Free;
    End;
 End;

{------------------------------------------------------------------------------}
{ SaveSecurityDescriptorToRegistry(...);                                       }
{ Saves Security Descriptor in a File                                          }
{------------------------------------------------------------------------------}
 Procedure SaveSecurityDescriptorToFile(Const FileName: String; sd: PSecurityDescriptor);
 Var
    dlen: Cardinal;
    desc: PSecurityDescriptor;
    Stm : TFileSTream;
 Begin
    Stm := TFileStream.Create(FileName, fmOpenWrite Or fmCreate Or fmShareExclusive);
    Try
       desc := MakeSelfRelativeSecurityDescriptor(sd, dlen);
       Try
          Stm.Size := dlen;
          Stm.Position := 0;
          Stm.WriteBuffer(desc^, dlen);
       Finally
          sdFree(desc, dlen);
       End;
    Finally
       Stm.Free;
    End;
 End;

{------------------------------------------------------------------------------}
{ LoadSecurityDescriptorFromStream(...);                                       }
{ Load a SecurityDescriptor form a VCL Stream                                  }
{------------------------------------------------------------------------------}
 Function LoadSecurityDescriptorFromStream(Const Stream: TStream): PSecurityDescriptor; Overload;
 Var
    dlen: Cardinal;
    desc: PSecurityDescriptor;
 Begin
    Result := Nil;
    Stream.Position := 0;
    dlen := Stream.Size;
    desc := sdAlloc(dlen);
    Try
       Stream.ReadBuffer(desc^, dlen);
       Try
          Result := MakeAbsoluteSecurityDescriptor(desc);
       Except
          sdFree(Result);
          Raise;
       End;
    Finally
       sdFree(desc, dlen);
    End;
 End;

{------------------------------------------------------------------------------}
{ SaveSecurityDescriptorToStream(...);                                         }
{ Saves Security Descriptor in a VCL Stream                                    }
{------------------------------------------------------------------------------}
 Procedure SaveSecurityDescriptorToStream(Const Stream: TStream; sd: PSecurityDescriptor); Overload;
 Var
    dlen: Cardinal;
    desc: PSecurityDescriptor;
 Begin
    desc := MakeSelfRelativeSecurityDescriptor(sd, dlen);
    Try
       Stream.Position := 0;
       Stream.WriteBuffer(desc^, dlen);
    Finally
       sdFree(desc, dlen);
    End;
 End;

{------------------------------------------------------------------------------}
{ LoadSecurityDescriptorFromStream(...);                                       }
{ Load a SecurityDescriptor form a COM Stream                                  }
{------------------------------------------------------------------------------}
 Function LoadSecurityDescriptorFromStream(Const Stm: IStream): PSecurityDescriptor; Overload;
 Var
    dlen: Cardinal;
    desc: PSecurityDescriptor;
    Pos : LargeInt;
    Stat: TStatStg;

 Begin
    Result := Nil;
    ZeroMemory(@Stat, SizeOf(Stat));
    OleCheck(Stm.Stat(Stat, STATFLAG_NONAME));
    dlen := Stat.cbSize;
    desc := sdAlloc(dlen);
    Try
       OleCheck(Stm.Seek(STREAM_SEEK_SET, STREAM_SEEK_SET, Pos));
       OleCheck(Stm.Read(desc, dlen, @Pos));
       Try
          Result := MakeAbsoluteSecurityDescriptor(desc);
       Except
          sdFree(Result);
          Raise;
       End;
    Finally
       sdFree(desc, dlen);
    End;
 End;

{------------------------------------------------------------------------------}
{ SaveSecurityDescriptorToStream(...);                                         }
{ Saves Security Descriptor in a COM Stream                                    }
{------------------------------------------------------------------------------}
 Procedure SaveSecurityDescriptorToStream(Const Stm: IStream; sd: PSecurityDescriptor); Overload;
 Var
    dlen: Cardinal;
    desc: PSecurityDescriptor;
    Pos : LargeInt;
 Begin
    desc := MakeSelfRelativeSecurityDescriptor(sd, dlen);
    Try
       OleCheck(Stm.Seek(STREAM_SEEK_SET, STREAM_SEEK_SET, Pos));
       OleCheck(Stm.Write(desc, dlen, @Pos));
    Finally
       sdFree(desc, dlen);
    End;
 End;

{------------------------------------------------------------------------------}
{ LoadSecurityDescriptorFromXMLFile(...);                                      }
{ Load a SecurityDescriptor form a xml File                                    }
{------------------------------------------------------------------------------}
 Function LoadSecurityDescriptorFromXMLFile(Const FileName: String): PSecurityDescriptor;
 Begin
    Raise Exception.Create(Syserrormessage(E_NOTIMPL));
 End;

{------------------------------------------------------------------------------}
{ SaveSecurityDescriptorToXMLFile(...);                                        }
{ Saves Security Descriptor in a XML File                                      }
{------------------------------------------------------------------------------}
 Procedure SaveSecurityDescriptorToXMLFile(Const FileName: String; sd: PSecurityDescriptor);
 Begin
    Raise Exception.Create(Syserrormessage(E_NOTIMPL));
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomSecurityObject                                         }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomSecurityObject.Create(AOwner: TComponent);                            }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCustomSecurityObject.Create(AOwner: TComponent);
 Begin
    Inherited Create(AOwner);
    FComputerName := '';
    FSystemName   := '';
    FRequested    := [sdWantACL];
    FAccessList   := CreateAccessList;
    FAuditList    := CreateAuditList;
    FOwnerID      := CreateOwnerID;
    FGroupID      := CreateGroupID;
    FShowLoadErrs := False;
    FActive       := False;
    FAutoSave     := False;
    FDomainSID    := Nil;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.Destroy;                                               }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TCustomSecurityObject.Destroy;
 Begin
    Active := False;
    FreeAccessList;
    FreeAuditList;
    FreeGroupID;
    FreeOwnerID;
    FreeAndNilSID(FDomainSID);
    Reset;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.Reset;                                                 }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.Reset;
 Begin
    FActive := False;
    FreeAndNilSecurityDescriptor(FDescriptor);
    FreeAndNilSecurityDescriptor(FShadowSD);
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.Loaded;                                                }
{ Loaded method Override                                                       }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.Loaded;
 Begin
    Inherited Loaded;
    OpenStreamed;
 End;

{------------------------------------------------------------------------------}
{ OpenStreamed;                                                                }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.OpenStreamed;
 Begin
    Try
       If FStreamed Then Active := True;
    Except
       On E: Exception Do
          Try
             Active := False;
          Finally
             If FShowLoadErrs Then
                ShowACLException(E);
          End;
    End;
 End;


{------------------------------------------------------------------------------}
{ TCustomSecurityObject.FreeAccessList;                                        }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.FreeAccessList;
 Begin
    If (FAccessList<>Nil) Then
       FAccessList.Free;
    FAccessList := Nil;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetAccessList: TDACL;                                  }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetAccessList: TDACL;
 Begin
    Result := FAccessList;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.FreeAuditList;                                         }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.FreeAuditList;
 Begin
    If (FAuditList<>Nil) Then
       FAuditList.Free;
    FAuditList := Nil;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetAuditList: TSACL;                                   }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetAuditList: TSACL;
 Begin
    Result := FAuditList;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.FreeGroupID;                                           }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.FreeGroupID;
 Begin
    If (FGroupID<>Nil) Then
       FGroupID.Free;
    FGroupID := Nil;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetGroupID: TCreatorSecurityID;                        }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetGroupID: TCreatorSecurityID;
 Begin
    Result := FGroupID;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.FreeOwnerID;                                           }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.FreeOwnerID;
 Begin
    If (FOwnerID<>Nil) Then
       FOwnerID.Free;
    FOwnerID := Nil;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetOwnerID: TCreatorSecurityID;                        }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetOwnerID: TCreatorSecurityID;
 Begin
    Result := FOwnerID;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetActive: Boolean;                                    }
{ Read Method property Active                                                  }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetActive: Boolean;
 Begin
    Result := FActive;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.SetActive(Const Value: Boolean);                       }
{ Write Method property Active                                                 }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.SetActive(Const Value: Boolean);
 Begin
    If (csReading In ComponentState) Then Begin
       FStreamed := Value;
       Exit;
    End;
    If (Value=FActive) Then Exit;
    If Value Then
       Load
    Else
       UnLoad;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetComputer: String;                                   }
{ Read Method property ComputerName                                            }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetComputer: String;
 Begin
    Result := FComputerName;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.SetComputer(Const Value: String);                      }
{ Write Method property ComputerName                                           }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.SetComputer(Const Value: String);
 Var
    Sv: Boolean;
 Begin
    Sv := Active;
    If Sv Then
       Active := False;
    FComputerName := Value;
    If Sv Then
       Active := True;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetSystem: String;                                     }
{ Read Method property LookupSystemName                                        }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetSystem: String;
 Begin
    Result := FSystemName;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.SetSystem(Const Value: String);                        }
{ Write Method property LookupSystemName                                       }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.SetSystem(Const Value: String);
 Var
    Sv: Boolean;
 Begin
    Sv := Active;
    If Sv Then
       Active := False;
    FSystemName := Value;
    FreeAndNilSID(FDomainSID);
    If Sv Then
       Active := True;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetDomainSID: PSID;                                    }
{ Read Method property DomainSid                                               }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetDomainSID: PSID;
 Begin
    If (FDomainSID=Nil) Then
       FDomainSID := GetDefaultDomainSID(FSystemName);
    Result := FDomainSID;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.Load;                                                  }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.Load;
 Begin
    FStreamed := False;
    FActive := True;
    Try
       If (GetAccessList<>Nil) And (sdWantACL In Options) Then
          GetAccessList.LoadACL;
       If (GetAuditList<>Nil) And (sdWantSystemACL In Options) Then
          GetAuditList.LoadACL;
       If (GetGroupID<>Nil) And (sdWantGroup In Options) Then
          GetGroupID.LoadID;
       If (GetOwnerID<>Nil) And (sdWantOwner In Options) Then
          GetOwnerID.LoadID;
    Except
       CleanUp;
       Raise;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.Unload;                                                }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.Unload;
 Begin
    Try
       If FAutoSave Then Save;
       If (FDescriptor<>Nil) Then Begin
          If (GetAccessList<>Nil) And (sdWantACL In Options) Then
             GetAccessList.FreeACL;
          If (GetAuditList<>Nil) And (sdWantSystemACL In Options) Then
             GetAuditList.FreeACL;
          If (GetGroupID<>Nil) And (sdWantGroup In Options) Then
             GetGroupID.FreeID;
          If (GetOwnerID<>Nil) And (sdWantOwner In Options) Then
             GetOwnerID.FreeID;
       End;
    Finally
       Reset;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.Save;                                                  }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.Save;
 Begin
    If (ExceptObject<>Nil) Or Not Active Then Exit;
    If (FDescriptor<>Nil) Then Begin
       If (GetAccessList<>Nil) And (sdWantACL In Options) Then
          GetAccessList.SaveACL;
       If (GetAuditList<>Nil) And (sdWantSystemACL In Options) Then
          GetAuditList.SaveACL;
       If (GetGroupID<>Nil) And (sdWantGroup In Options) Then
          GetGroupID.SaveID;
       If (GetOwnerID<>Nil) And (sdWantOwner In Options) Then
          GetOwnerID.SaveID;
       WriteObjectDescriptor;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.CleanUp;                                               }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.CleanUp;
 Begin
    Try
       If (FDescriptor<>Nil) Then Begin
          If (GetAccessList<>Nil) And (sdWantACL In Options) Then
             GetAccessList.CleanUpACL;
          If (GetAuditList<>Nil) And (sdWantSystemACL In Options) Then
             GetAuditList.CleanUpACL;
          If (GetGroupID<>Nil) And (sdWantGroup In Options) Then
             GetGroupID.CleanUpID;
          If (GetOwnerID<>Nil) And (sdWantOwner In Options) Then
             GetOwnerID.CleanUpID;
       End;
    Finally
       Reset;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetSD: PSecurityDescriptor;                            }
{ Read method property Descriptor                                              }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetSD: PSecurityDescriptor;
 Begin
    If (FDescriptor=Nil) Then Begin
       Try
          FDescriptor := ReadObjectDescriptor;
       Except
          CleanUp;
          Raise;
       End;
    End;
    Result := FDescriptor;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.SetSD(Const Value: PSecurityDescriptor);               }
{ Write method property Descriptor                                             }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.SetSD(Const Value: PSecurityDescriptor);
 Begin
    Reset;
    FDescriptor := Value;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetTrueSD: PSecurityDescriptor;                        }
{ Read method property TrueDescriptor                                          }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetTrueSD: PSecurityDescriptor;
 Begin
    Result := FDescriptor;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetShadowSD: PSecurityDescriptor;                      }
{ Read method property ShadowDescriptor                                        }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetShadowSD: PSecurityDescriptor;
 Begin
    If (FShadowSD=Nil) Then
       FShadowSD := ReadObjectDescriptor;
    Result := FShadowSD;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.SetShadowSD(...);                                      }
{ Write method property Descriptor                                             }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.SetShadowSD(Const Value: PSecurityDescriptor = Nil);
 Begin
    FreeAndNilSecurityDescriptor(FShadowSD);
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetSecInf: DWORD;                                      }
{ Read method property SecurityInf                                             }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetSecInf: DWORD;
 Begin
    Result := 0;
    If (sdWantOwner In FRequested) Then
       Result := Result Or OWNER_SECURITY_INFORMATION;
    If (sdWantGroup In FRequested) Then
       Result := Result Or GROUP_SECURITY_INFORMATION;
    If (sdWantACL In FRequested) Then
       Result := Result Or DACL_SECURITY_INFORMATION;
    If (sdWantSystemACL In FRequested) Then
       Result := Result Or SACL_SECURITY_INFORMATION;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.GetRequested: TSDInformations;                         }
{ Read method property Mode                                                    }
{------------------------------------------------------------------------------}
 Function TCustomSecurityObject.GetRequested: TSDInformations;
 Begin
    Result := FRequested;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.SetRequested(Const Value: TSDInformations);            }
{ Write method property Mode                                                   }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.SetRequested(Const Value: TSDInformations);
 Var
    Sv: Boolean;
 Begin
    Sv := Active;
    If Sv Then
       Active := False;
    FRequested := Value;
    If Sv Then
       Active := True;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.CheckSD;                                               }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.CheckSD;
 Begin
    If (FDescriptor=Nil) Then
       Raise EWin32Error.Create(SysErrorMessage(ERROR_INVALID_SECURITY_DESCR));
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.LoadFromRegistry(...);                                 }
{ Load the security descriptor from a registry key                             }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.LoadFromRegistry(Const Computer: String; Const RootKey: HKey; Const SubKey: String; Const Value: String);
 Begin
    Active := False;
    FDescriptor := LoadSecurityDescriptorFromRegistry(Computer, RootKey, SubKey, Value);
    If (FDescriptor<>Nil) Then
       Load;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.SaveToRegistry(...);                                   }
{ Saves the security descriptor to a registry key                              }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.SaveToRegistry(Const Computer: String; Const RootKey: HKey; Const SubKey: String; Const Value: String);
 Begin
    CheckSD;
    SaveSecurityDescriptorToRegistry(Computer, RootKey, SubKey, Value, Descriptor);
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.LoadFromFile(...);                                     }
{ Load the security descriptor from a File                                     }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.LoadFromFile(Const FileName: TFileName; Const xmlFormat: Boolean = False);
 Begin
    Active := False;
    If xmlFormat Then
       FDescriptor := LoadSecurityDescriptorFromXMLFile(FileName)
    Else
       FDescriptor := LoadSecurityDescriptorFromFile(FileName);
    If (FDescriptor<>Nil) Then
       Load;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.SaveToFile(...);                                       }
{ Save the security descriptor to a File                                       }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.SaveToFile(Const FileName: TFileName; Const xmlFormat: Boolean = False);
 Begin
    CheckSD;
    If xmlFormat Then
       SaveSecurityDescriptorToXMLFile(FileName, Descriptor)
    Else
       SaveSecurityDescriptorToFile(FileName, Descriptor);
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.LoadFromStream(...);                                   }
{ Load the security descriptor from a Delphi Stream                            }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.LoadFromStream(Stream: TStream);
 Begin
    Active := False;
    FDescriptor := LoadSecurityDescriptorFromStream(Stream);
    If (FDescriptor<>Nil) Then
       Load;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.SaveToStream(...);                                     }
{ Saves the security descriptor to a Delphi Stream                             }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.SaveToStream(Stream: TStream);
 Begin
    CheckSD;
    SaveSecurityDescriptorToStream(Stream, Descriptor);
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.LoadFromStream(Stm: IStream);                          }
{ Load the security descriptor from a COM Stream                               }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.LoadFromStream(Stm: IStream);
 Begin
    Active := False;
    FDescriptor := LoadSecurityDescriptorFromStream(Stm);
    If (FDescriptor<>Nil) Then
       Load;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityObject.SaveToStream(...);                                     }
{ Saves the security descriptor to a COM Stream                                }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityObject.SaveToStream(Stm: IStream);
 Begin
    CheckSD;
    SaveSecurityDescriptorToStream(Stm, Descriptor);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomACL                                                    }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomACL.Create(AOwner: TPersistent);                                      }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCustomACL.Create(AOwner: TPersistent; Obj: TCustomSecurityObject; ItemClass: TCustomACEClass);
 Begin
    Inherited Create(AOwner, ItemClass);
    FObject := Obj;
    FInheritMode := imDefault;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.Destroy;                                                          }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TCustomACL.Destroy;
 Begin
    CleanUpACL;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.GetInherit: TInheritMode;                                         }
{ Read Method Method property InheritMode                                      }
{------------------------------------------------------------------------------}
 Function TCustomACL.GetInherit: TInheritMode;
 Begin
    Result := FInheritMode;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.SetInherit(Const Value: TInheritMode);                            }
{ Write Method Method property InheritMode                                     }
{------------------------------------------------------------------------------}
 Procedure TCustomACL.SetInherit(Const Value: TInheritMode);
 Begin
    If (FInheritMode=Value) Then Exit;
    FInheritMode := Value;
    If (csReading In VCLSecurityObject.ComponentState) Then
       Exit;
    If Not VCLSecurityObject.Active Then
       Exit;
    Case FInheritMode Of
       imDefault: DoDefaultInherit;
       imCopy   : DoCopyInherit;
       imDelete : DoDeleteInherit;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.GetSystem: String;                                                }
{ Read Method Method property LookUpSystemName                                 }
{------------------------------------------------------------------------------}
 Function TCustomACL.GetSystem: String;
 Begin
    Result := VCLSecurityObject.LookupSystemName;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.SetSystem(Const Value: String);                                   }
{ Write Method Method property LookUpSystemName                                }
{------------------------------------------------------------------------------}
 Procedure TCustomACL.SetSystem(Const Value: String);
 Var
    I: Integer;
 Begin
    For I := 0 To Count-1 Do
       TCustomACE(Items[I]).LookupSystemName := Value;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.GetDomainSID: PSid;                                               }
{ Read Method Method property DomainSID                                        }
{------------------------------------------------------------------------------}
 Function TCustomACL.GetDomainSID: PSid;
 Begin
    Result := VCLSecurityObject.DomainSID;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.GetSD(Shadow: Boolean): PSecurityDescriptor;                      }
{ Read method property Descriptor                                              }
{------------------------------------------------------------------------------}
 Function TCustomACL.GetSD(Shadow: Boolean): PSecurityDescriptor;
 Begin
    If Not Shadow Then
       Result := FObject.Descriptor
    Else
       Result := FObject.ShadowDescriptor;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.SetSD(Const Value: PSecurityDescriptor);                          }
{ Write method property Descriptor                                             }
{------------------------------------------------------------------------------}
 Procedure TCustomACL.SetSD(Shadow: Boolean; Const Value: PSecurityDescriptor);
 Begin
    If Not Shadow Then
       FObject.Descriptor := Value
    Else
       FObject.ShadowDescriptor := Nil;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.GetTrueSD: PSecurityDescriptor;                                   }
{ Read method property TrueDescriptor                                          }
{------------------------------------------------------------------------------}
 Function TCustomACL.GetTrueSD: PSecurityDescriptor;
 Begin
    Result := FObject.TrueDescriptor;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.GetACL(Shadow: Boolean): Boolean;                                 }
{ Read method property ACL                                                     }
{------------------------------------------------------------------------------}
 Function TCustomACL.GetACL(Shadow: Boolean): PACL;
 Var
    fPresent: LongBool;
    fDefaulted: LongBool;
 Begin
    Case FKind Of
       akSystem:
         If Not GetSecurityDescriptorSacl(Descriptor[Shadow], fPresent, Result, fDefaulted) Then
            RaiseACLError;
       akDiscretionnary:
         If Not GetSecurityDescriptorDacl(Descriptor[Shadow], fPresent, Result, fDefaulted) Then
            RaiseACLError;
    End;
    If Not fPresent Then
       Result := Nil;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.SetACL(Shadow: Boolean; Const Value: PACL);                       }
{ Write method property ACL                                                    }
{------------------------------------------------------------------------------}
 Procedure TCustomACL.SetACL(Shadow: Boolean; Const Value: PACL);
 Var
    OldACL: PACL;
 Begin
    OldACL := ACL[Shadow];
    Case FKind Of
       akSystem:
         If Not SetSecurityDescriptorSacl(Descriptor[Shadow], (Value<>Nil), Value, False) Then
            RaiseACLError;
       akDiscretionnary:
         If Not SetSecurityDescriptorDacl(Descriptor[Shadow], (Value<>Nil), Value, False) Then
            RaiseACLError;
    End;
    FreeAndNilACL(OldAcl);
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.InternalGetACECount: Integer;                                     }
{------------------------------------------------------------------------------}
 Function TCustomACL.InternalGetACECount(Shadow: Boolean): Integer;
 Begin
    If (ACL[Shadow]<>Nil) Then
       Result := ACL[Shadow]^.AceCount
    Else
       Result := 0;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.GetObjectACE(...): Boolean;                                       }
{------------------------------------------------------------------------------}
 Function TCustomACL.InternalGetACE(Shadow: Boolean; Index: Integer): PACE;
 Begin
    Result := Nil;
    If (ACL[Shadow]<>Nil) Then
       If Not GetAce(ACL[Shadow]^, Index, Pointer(Result)) Then
          RaiseACLError;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.Add(...): TCustomACE;                                             }
{ Add Method Overload                                                          }
{------------------------------------------------------------------------------}
 Function TCustomACL.Add: TCustomACE;
 Begin
    Result := TCustomACE(Inherited Add);
    Result.SetAceStatus(asadded);
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.LoadACL;                                                          }
{ LoadACL method Override                                                      }
{------------------------------------------------------------------------------}
 Procedure TCustomACL.LoadACL;
 Var
    I, J: Integer;
 Begin
    Clear;
    J := InternalGetACECount(False);
    For I := 0 To J-1 Do
       With Add Do Begin
          Ace := InternalGetACE(False, I);
          SetAceStatus(asnone);
       End;
    Case InheritMode Of
       imCopy   : DoCopyInherit;
       imDelete : DoDeleteInherit;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.SaveACL;                                                          }
{ SaveACL method Override                                                      }
{------------------------------------------------------------------------------}
 Procedure TCustomACL.SaveACL;
 Var
    I: Integer;
    nacl: PACL;
    A: TCustomACE;
 Begin
    nacl := InitACL;
    Try
      For I := 0 To Count-1 Do Begin
         A := (Items[I] As TCustomACE);
         If A.Identifier.IsValid Then A.GetDeniedAce(nacl);
      End;
      For I := 0 To Count-1 Do Begin
         A := (Items[I] As TCustomACE);
         If A.Identifier.IsValid Then A.GetAllowedAce(nacl);
      End;
    Finally
      ACL[False] := Nil;
      ACL[False] := nacl;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.InitACL: PACL;                                                    }
{ InitAcl method                                                               }
{------------------------------------------------------------------------------}
 Function TCustomACL.InitACL: PACL;
 Var
    cbacl: Integer;
    cbace: Integer;
    J: Integer;
    A: TCustomACE;
 Begin
    cbacl := SizeOf(TACL);
    For J := 0 To Count-1 Do Begin
       A := (Items[J] As TCustomACE);
       If A.Identifier.IsValid Then Begin
          cbace := A.GetDeniedSize;
          Inc(cbacl, cbace);
       End;
    End;
    For J := 0 To Count-1 Do Begin
       A := (Items[J] As TCustomACE);
       If A.Identifier.IsValid Then Begin
          cbace := A.GetAllowedSize;
          Inc(cbacl, cbace);
       End;
    End;
    Result := sdAlloc(cbacl);
    InitializeAcl(Result^, cbacl, ACL_REVISION);
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.FreeACL;                                                          }
{ FreeACL method                                                               }
{------------------------------------------------------------------------------}
 Procedure TCustomACL.FreeACL;
 Begin
    Clear;
    ACL[False] := Nil;
    ACL[True] := Nil;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.CleanUpACL;                                                       }
{ CleanUpACL method                                                            }
{------------------------------------------------------------------------------}
 Procedure TCustomACL.CleanUpACL;
 Begin
    Clear;
    If (TrueDescriptor<>Nil) Then Begin
       ACL[False] := Nil;
       ACL[True] := Nil;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.DoDefaultInherit;                                                 }
{ DoDefaultInherit method                                                      }
{------------------------------------------------------------------------------}
 Procedure TCustomACL.DoDefaultInherit;
 Var
    I, J: Integer;
    A: PAce;
    F: TAceFlagTypes;
    IsGood: Boolean;
 Begin
    DoDeleteInherit;
    J := InternalGetACECount(True);
    Try
       For I := 0 To J-1 Do Begin
          A := InternalGetACE(True, I);
          IsGood := False;
          If (A<>Nil) Then With A^ Do Begin
             Case AceType Of
                ACCESS_ALLOWED_ACE_TYPE:
                  F := AceFlagsToAces(AccessAllowedAce.Header.AceFlags);
                ACCESS_DENIED_ACE_TYPE:
                  F := AceFlagsToAces(AccessDeniedAce.Header.AceFlags);
                SYSTEM_AUDIT_ACE_TYPE:
                  F := AceFlagsToAces(SystemAuditAce.Header.AceFlags);
                SYSTEM_ALARM_ACE_TYPE:
                  F := AceFlagsToAces(SystemAlarmAce.Header.AceFlags);
                ACCESS_ALLOWED_COMPOUND_ACE_TYPE:
                  F := AceFlagsToAces(AccessAllowedCompoundAce.Header.AceFlags);
                ACCESS_ALLOWED_OBJECT_ACE_TYPE:
                  F := AceFlagsToAces(AccessAllowedObjectAce.Header.AceFlags);
                ACCESS_DENIED_OBJECT_ACE_TYPE:
                  F := AceFlagsToAces(AccessDeniedObjectAce.Header.AceFlags);
                SYSTEM_AUDIT_OBJECT_ACE_TYPE:
                  F := AceFlagsToAces(SystemAuditObjectAce.Header.AceFlags);
                SYSTEM_ALARM_OBJECT_ACE_TYPE:
                  F := AceFlagsToAces(SystemAlarmObjectAce.Header.AceFlags);
             End;
             IsGood := (afInherited In F);
          End;
          If (IsGood) Then
             With Add Do Begin
                Ace := InternalGetACE(True, I);
                SetAceStatus(asnone);
             End;
       End;
       ACL[True] := Nil;
    Finally
       Descriptor[True] := Nil;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.DoCopyInherit;                                                    }
{ DoCopyInherit method                                                         }
{------------------------------------------------------------------------------}
 Procedure TCustomACL.DoCopyInherit;
 Var
    I: Integer;
 Begin
     For I := 0 To Count-1 Do
        If (Items[I] As TCustomACE).IsInherited Then
           (Items[I] As TCustomACE).IsInherited := False;
 End;

{------------------------------------------------------------------------------}
{ TCustomACL.DoDeleteInherit;                                                  }
{ DoDeleteInherit method                                                       }
{------------------------------------------------------------------------------}
 Procedure TCustomACL.DoDeleteInherit;
 Var
    I: Integer;
 Begin
     For I := Count-1 DownTo 0 Do
        If (Items[I] As TCustomACE).IsInherited Then
           Delete(I);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TDACL                                                         }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TDACL.Create(...);                                                           }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TDACL.Create(AOwner: TPersistent; Obj: TCustomSecurityObject; ItemClass: TCustomACEClass);
 Begin
    Inherited Create(AOwner, Obj, ItemClass);
    FKind := akDiscretionnary;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TSACL                                                         }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TSACL.Create(...);                                                           }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TSACL.Create(AOwner: TPersistent; Obj: TCustomSecurityObject; ItemClass: TCustomACEClass);
 Begin
    Inherited Create(AOwner, Obj, ItemClass);
    FKind := akSystem;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomACE                                                    }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomACE.Create(Collection: TCollection);                                  }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCustomACE.Create(Collection: TCollection);
 Begin
    If Not (Collection As TCustomACL).VCLSecurityObject.Active Then
       Raise Exception.Create(SAlwaysInActive);
    Inherited Create(Collection);
    FAce      := Nil;
    FAceSize  := 0;
    FAceFlags := [];
    FAceType  := aeNone;
    FStatus   := asnone;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.Destroy;                                                          }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TCustomACE.Destroy;
 Begin
    Reset;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.Reset;                                                            }
{------------------------------------------------------------------------------}
 Procedure TCustomACE.Reset;
 Begin
    FreeAndNilAce(FAce);
    FAceSize  := 0;
    FAceFlags := [];
    FAceType  := aeNone;
    FStatus   := asnone;
    If (FSecurityID<>Nil) Then
       FSecurityID.Free;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.GetAcePtr: PAce;                                                  }
{ Read method Property Ace                                                     }
{------------------------------------------------------------------------------}
 Function TCustomACE.GetAcePtr: PAce;
 Begin
    Result := FAce;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.SetAcePtr(Const Value: PAce);                                     }
{ Write method Property Ace                                                    }
{------------------------------------------------------------------------------}
 Procedure TCustomACE.SetAcePtr(Const Value: PAce);
 Var
   xsid: PSid;
 Begin
    Reset;
    FAce := Value;
    If (FAce<>Nil) Then With FAce^ Do Begin
       FAllowedMask := 0;
       FDeniedMask  := 0;
       Case AceType Of
          ACCESS_ALLOWED_ACE_TYPE:
            Begin
               FAceSize     := AccessAllowedAce.Header.AceSize;
               FAceFlags    := AceFlagsToAces(AccessAllowedAce.Header.AceFlags);
               FAceType     := aeAllowed;
               xsid         := PSid(@accessAllowedAce.SidStart);
               FAllowedMask := AccessAllowedAce.Mask;
            End;
          ACCESS_DENIED_ACE_TYPE:
            Begin
               FAceSize     := AccessDeniedAce.Header.AceSize;
               FAceFlags    := AceFlagsToAces(AccessDeniedAce.Header.AceFlags);
               FAceType     := aeDenied;
               xsid         := PSid(@AccessDeniedAce.SidStart);
               FDeniedMask  := AccessDeniedAce.Mask;
            End;
          SYSTEM_AUDIT_ACE_TYPE:
            Begin
               FAceSize     := SystemAuditAce.Header.AceSize;
               FAceFlags    := AceFlagsToAces(SystemAuditAce.Header.AceFlags);
               FAceType     := aeAudit;
               xsid         := PSid(@SystemAuditAce.SidStart);
               If (afSuccessfullAuditFlag In FAceFlags) Then
                  FAllowedMask := SystemAuditAce.Mask;
               If (afFailedAuditFlag In FAceFlags) Then
                  FDeniedMask  := SystemAuditAce.Mask;
            End;
          SYSTEM_ALARM_ACE_TYPE:
            Begin
               FAceSize     := SystemAlarmAce.Header.AceSize;
               FAceFlags    := AceFlagsToAces(SystemAlarmAce.Header.AceFlags);
               FAceType     := aeAlarm;
               xsid         := PSid(@SystemAlarmAce.SidStart);
               FAllowedMask := SystemAlarmAce.Mask;
               FDeniedMask  := SystemAlarmAce.Mask;
            End;
          ACCESS_ALLOWED_COMPOUND_ACE_TYPE:
            Begin
               FAceSize     := AccessAllowedCompoundAce.Header.AceSize;
               FAceFlags    := AceFlagsToAces(AccessAllowedCompoundAce.Header.AceFlags);
               FAceType     := aeCompound;
               xsid         := PSid(@AccessAllowedCompoundAce.SidStart);
               FAllowedMask := AccessAllowedCompoundAce.Mask;
            End;
          ACCESS_ALLOWED_OBJECT_ACE_TYPE:
            Begin
               FAceSize     := AccessAllowedObjectAce.Header.AceSize;
               FAceFlags    := AceFlagsToAces(AccessAllowedObjectAce.Header.AceFlags);
               FAceType     := aeAllowedObject;
               xsid         := PSid(@AccessAllowedObjectAce.SidStart);
               FAllowedMask := AccessAllowedObjectAce.Mask;
            End;
          ACCESS_DENIED_OBJECT_ACE_TYPE:
            Begin
               FAceSize     := AccessDeniedObjectAce.Header.AceSize;
               FAceFlags    := AceFlagsToAces(AccessDeniedObjectAce.Header.AceFlags);
               FAceType     := aeDeniedObject;
               xsid         := PSid(@AccessDeniedObjectAce.SidStart);
               FDeniedMask  := AccessDeniedObjectAce.Mask;
            End;
          SYSTEM_AUDIT_OBJECT_ACE_TYPE:
            Begin
               FAceSize     := SystemAuditObjectAce.Header.AceSize;
               FAceFlags    := AceFlagsToAces(SystemAuditObjectAce.Header.AceFlags);
               FAceType     := aeAuditObject;
               xsid         := PSid(@SystemAuditObjectAce.SidStart);
               If (afSuccessfullAuditFlag In FAceFlags) Then
                  FAllowedMask := SystemAuditObjectAce.Mask;
               If (afFailedAuditFlag In FAceFlags) Then
                  FDeniedMask  := SystemAuditObjectAce.Mask;
            End;
          SYSTEM_ALARM_OBJECT_ACE_TYPE:
            Begin
               FAceSize     := SystemAlarmObjectAce.Header.AceSize;
               FAceFlags    := AceFlagsToAces(SystemAlarmObjectAce.Header.AceFlags);
               FAceType     := aeAlarmObject;
               xsid         := PSid(@SystemAlarmObjectAce.SidStart);
               FAllowedMask := SystemAlarmObjectAce.Mask;
               FDeniedMask  := SystemAlarmObjectAce.Mask;
            End;
       Else
          xsid := Nil;
       End;
       Identifier.LookUpSystemName := LookUpSystemName;
       Identifier.DomainSID := DomainSID;
       Identifier.AsSID := xsid;
       Identifier.ReadOnly := IsInherited;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.GetSize: Cardinal;                                                }
{ Read method Property AceSize                                                 }
{------------------------------------------------------------------------------}
 Function TCustomACE.GetSize: Cardinal;
 Begin
    Result := FAceSize;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.GetAceFlags: TAceFlagTypes;                                       }
{ Read method Property AceFlags                                                }
{------------------------------------------------------------------------------}
 Function TCustomACE.GetAceFlags: TAceFlagTypes;
 Begin
    Result := FAceFlags;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.SetAceFlags(Const Value: TAceFlagTypes);                          }
{ Write method Property AceFlags                                               }
{------------------------------------------------------------------------------}
 Procedure TCustomACE.SetAceFlags(Const Value: TAceFlagTypes);
 Begin
    If (Value<>FAceFlags) Then Begin
       FAceFlags := Value;
       Status := asmodified;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.GetAceType: TAceEntryType;                                        }
{ Read method Property AceType                                                 }
{------------------------------------------------------------------------------}
 Function TCustomACE.GetAceType: TAceEntryType;
 Begin
    Result := FAceType;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.SetAceType(Const Value: TAceEntryType);                           }
{ Write method Property AceType                                                }
{------------------------------------------------------------------------------}
 Procedure TCustomACE.SetAceType(Const Value: TAceEntryType);
 Begin
    FAceType := Value;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.GetIsInherited: Boolean;                                          }
{ Read method Property IsInherited                                             }
{------------------------------------------------------------------------------}
 Function TCustomACE.GetIsInherited: Boolean;
 Begin
    Result := False;
    If (afInherited In FAceFlags) Then
       Result := True;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.SetIsInherited(Const Value: Boolean);                             }
{ Write method Property IsInherited                                            }
{------------------------------------------------------------------------------}
 Procedure TCustomACE.SetIsInherited(Const Value: Boolean);
 Begin
    If Value Then
       Raise Exception.Create(SCannotBeAssigned)
    Else
       Exclude(FAceFlags, afInherited);
    Identifier.ReadOnly := IsInherited;
    Status := asModified;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.GetMaskFlags(Kind: TFlagKind): Cardinal;                          }
{ Read method Property MaskFlags                                               }
{------------------------------------------------------------------------------}
 Function TCustomACE.GetMaskFlags(Kind: TFlagKind): Cardinal;
 Begin
    Result := FAllowedMask;
    If (Kind=fkNo) Then
       Result := FDeniedMask;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.SetMaskFlags(Kind: TFlagKind; Const Value: Cardinal);             }
{ Write method Property MaskFlags                                              }
{------------------------------------------------------------------------------}
 Procedure TCustomACE.SetMaskFlags(Kind: TFlagKind; Const Value: Cardinal);
 Begin
    Case Kind Of
       fkYes:
         FAllowedMask := Value;
       fkNo:
         FDeniedMask  := Value;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.GetAceStatus: TAceStatus;                                         }
{ Read Method method Property Status                                           }
{------------------------------------------------------------------------------}
 Function TCustomACE.GetAceStatus: TAceStatus;
 Begin
    Result := FStatus;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.SetAceStatus(Const Value: TAceStatus);                            }
{ Write Method method Property Status                                          }
{------------------------------------------------------------------------------}
 Procedure TCustomACE.SetAceStatus(Const Value: TAceStatus);
 Begin
    If (Value=asmodified) And (FStatus=asnone) Then
       FStatus := asmodified
    Else If (Value=asadded) And (FStatus=asnone) Then
       FStatus := asadded
    Else If (Value=asdeleted) And (FStatus=asnone) Then
       FStatus := asdeleted
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.GetSystem: String;                                                }
{ Read Method method Property LookUpSystemName                                 }
{------------------------------------------------------------------------------}
 Function TCustomACE.GetSystem: String;
 Begin
    Result := TCustomACL(Collection).LookupSystemName;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.SetSystem(Const Value: String);                                   }
{ Write Method Method property LookUpSystemName                                }
{------------------------------------------------------------------------------}
 Procedure TCustomACE.SetSystem(Const Value: String);
 Begin
    If (FSecurityID<>Nil) Then
       FSecurityID.LookUpSystemName := Value;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.GetDomainSID: PSID;                                               }
{ Read Method method Property DomainSID                                        }
{------------------------------------------------------------------------------}
 Function TCustomACE.GetDomainSID: PSID;
 Begin
    Result := TCustomACL(Collection).DomainSID;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.GetSecurityID: TSecurityID;                                       }
{ Read method method property SecurityID                                       }
{------------------------------------------------------------------------------}
 Function TCustomACE.GetSecurityID: TSecurityID;
 Begin
    If (FSecurityID=Nil) Then Begin
       FSecurityID := TSecurityID.Create;
       FSecurityID.LookUpSystemName := LookUpSystemName;
       FSecurityID.DomainSID := DomainSID;
       FSecurityID.Reset;       
    End;
    Result := FSecurityID;
 End;

{------------------------------------------------------------------------------}
{ TCustomACE.SetSecurityID(Const Value: TSecurityID);                          }
{ Write method method property SecurityID                                      }
{------------------------------------------------------------------------------}
 Procedure TCustomACE.SetSecurityID(Const Value: TSecurityID);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCustomSecurityID                                             }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCustomSecurityID.Destroy;                                                   }
{ Destructor Override                                                          }
{------------------------------------------------------------------------------}
 Destructor TCustomSecurityID.Destroy;
 Begin
    ClearSID;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.Reset;                                                     }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityID.Reset;
 Begin
    ClearSid;
    WellKnown := wnsNull;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.GetAsSID: PSID;                                            }
{ Read method property AsSID                                                   }
{------------------------------------------------------------------------------}
 Function TCustomSecurityID.GetAsSID: PSID;
 Begin
    Result := FSID;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.SetAsSID(Const Value: PSID);                               }
{ Write method property AsSID                                                  }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityID.SetAsSID(Const Value: PSID);
 Begin
    If FReadOnly Then
       Raise Exception.Create(SReadOnly);
    If IsValidSid(Value) Then
       AllocSid(Value);
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.GetType: TSidType;                                         }
{ Read method property SidType                                                 }
{------------------------------------------------------------------------------}
 Function TCustomSecurityID.GetType: TSidType;
 Begin
    Result := FType;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.GetName: String;                                           }
{ Read method property Name                                                    }
{------------------------------------------------------------------------------}
 Function TCustomSecurityID.GetName: String;
 Begin
    Result := FName;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.SetName(Value: String);                                    }
{ Write method property Name                                                   }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityID.SetName(Value: String);
 Begin
    If FReadOnly Then
       Raise Exception.Create(SReadOnly);
    LoadSid(Value);
    FName := Value;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.GetSidSize(...): Cardinal;                                 }
{------------------------------------------------------------------------------}
 Function TCustomSecurityID.GetSidSize(Const Value: String): Cardinal;
 Var
   Nme : Array[0..256] Of Char;
   use : SID_NAME_USE;
   err : DWORD;
   nlen: DWORD;
 Begin
    Result := 0;
    use := 0;
    nlen := 0;
    Zeromemory(@Nme, SizeOf(Nme));
    If Not LookupAccountName(PChar(LookupSystemName), PChar(Value), Nil, Result, Nme, nlen, use) Then Begin
       Err := GetLastError;
       If (Err<>ERROR_SUCCESS) And (Err<>ERROR_INSUFFICIENT_BUFFER) Then
          RaiseACLError(Err);
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.AllocSid(...);                                             }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityID.AllocSid(Const asid: PSid);
 Var
   slen: DWORD;
   err : DWORD;
 Begin
    slen := GetLengthSid(asid);
    If (slen=0) Then Exit;
    ClearSid;
    fsid := sdAlloc(slen);
    Try
       If Not CopySid(slen, fsid, asid) Then Begin
          Err := GetLastError;
          If (Err<>ERROR_SUCCESS) Then RaiseACLError(Err);
       End;
       GetSidInfos;
    Except
       ClearSid;
       Raise;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.LoadSid(...);                                              }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityID.LoadSid(Const Value: String);
 Var
   Nme: Array[0..256] Of Char;
   nlen: DWORD;
   slen: DWORD;
   Err : DWORD;
   use : SID_NAME_USE;
 Begin
    use := 0;
    nlen := SizeOf(Nme);
    slen := GetSidSize(Value);
    ClearSid;
    fsid := sdAlloc(slen);
    Zeromemory(@Nme, SizeOf(Nme));
    If Not LookupAccountName(PChar(LookupSystemName), PChar(Value), fsid, slen, Nme, nlen, use) Then Begin
       Err := GetLastError;
       If (Err<>ERROR_SUCCESS) Then
          RaiseACLError(Err);
    End;
    GetSidInfos;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.ClearSid;                                                  }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityID.ClearSid;
 Begin
    FreeAndNilSID(fsid);
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.GetSidInfos;                                               }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityID.GetSidInfos;
 Var
   sidname: Array[0..256] Of Char;
   domname: Array[0..256] Of Char;
   nlen   : DWORD;
   dlen   : DWORD;
   use    : SID_NAME_USE;
   err    : DWORD;
 Begin
    use := 0;
    nlen := SizeOf(sidname);
    dlen := SizeOf(domname);
    Zeromemory(@sidname, nlen);
    Zeromemory(@domname, dlen);
    If Not LookupAccountSID(PChar(LookupSystemName), fsid, sidname, nlen, domname, dlen, use) Then
       Begin
          Err := GetLastError;
          If (Err=ERROR_NONE_MAPPED) Then
             Begin
                FName   := SidToString(fsid);
                FString := SidToString(fsid);
                FType   := TSIDType(use);
             End
          Else
             RaiseACLError(Err);
       End
    Else
       Begin
          If (sidname<>'') And (domname<>'') Then
             SetString(FName, PChar(String(PChar(@domname))+'\'+String(PChar(@sidname))), nlen+dlen+1)
          Else If (sidname='') And (domname<>'') Then
             SetString(FName, PChar(String(PChar(@domname))), dlen)
          Else If (sidname<>'') And (domname='') Then
             SetString(FName, PChar(String(PChar(@sidname))), nlen)
          Else
             FName := SidToString(fsid);
          FString := SidToString(fsid);
          FType   := TSIDType(use);
       End;
     IsWellKnownSID(DomainSID, AsSID, FWellKnownSid);
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.GetAsString: String;                                       }
{ Read method property AsString                                                }
{------------------------------------------------------------------------------}
 Function TCustomSecurityID.GetAsString: String;
 Begin
    Result := FString;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.SetAsString(Const Value: String);                          }
{ Write method property AsString                                               }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityID.SetAsString(Const Value: String);
 Begin
    Raise Exception.Create(SCannotBeAssigned);
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.GetAsText: String;                                         }
{ Read method property AsText                                                  }
{------------------------------------------------------------------------------}
 Function TCustomSecurityID.GetAsText: String;
 Begin
    If (FName<>'') Then
       Result := FName
    Else
       Result := SidToString(FSID);
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.GetIsvalid: Boolean;                                       }
{ Read method property IsValid                                                 }
{------------------------------------------------------------------------------}
 Function TCustomSecurityID.GetIsvalid: Boolean;
 Begin
    Result := IsValidSID(FSID);
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.GetWellKnown: TWellKnownSid;                               }
{ Read method property WellKnown                                               }
{------------------------------------------------------------------------------}
 Function TCustomSecurityID.GetWellKnown: TWellKnownSid;
 Begin
    Result := FWellKnownSid;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.SetWellKnown(Const Value: TWellKnownSid);                  }
{ Write method property WellKnown                                              }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityID.SetWellKnown(Const Value: TWellKnownSid);
 Var
    xsid: PSid;
 Begin
    xsid := CreateWellKnownSID(DomainSID, Value);
    Try
       If IsValidSid(xsid) Then AllocSid(xsid);
    Finally
       FreeAndNilSID(xsid);
    End;
    FWellKnownSid := Value;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.GetReadOnly: Boolean;                                      }
{ Read method property ReadOnly                                                }
{------------------------------------------------------------------------------}
 Function TCustomSecurityID.GetReadOnly: Boolean;
 Begin
    Result := FReadOnly;
 End;

{------------------------------------------------------------------------------}
{ TCustomSecurityID.SetReadOnly(Const Value: Boolean);                         }
{ Write method property ReadOnly                                               }
{------------------------------------------------------------------------------}
 Procedure TCustomSecurityID.SetReadOnly(Const Value: Boolean);
 Begin
    FReadOnly := Value;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCreatorSecurityID                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCreatorSecurityID.Create(AObject: TCustomSecurityObject);                   }
{ Constructor Overload                                                         }
{------------------------------------------------------------------------------}
 Constructor TCreatorSecurityID.Create(AObject: TCustomSecurityObject);
 Begin
    Inherited Create;
    FObject := AObject;
    ReadOnly := False;
 End;

{------------------------------------------------------------------------------}
{ TCreatorSecurityID.Destroy;                                                  }
{ Destructor Overload                                                          }
{------------------------------------------------------------------------------}
 Destructor TCreatorSecurityID.Destroy;
 Begin
    CleanUpID;
    Inherited Destroy;
 End;

{------------------------------------------------------------------------------}
{ TCreatorSecurityID.GetReadOnly: Boolean;                                     }
{ Read method property ReadOnly                                                }
{------------------------------------------------------------------------------}
 Function TCreatorSecurityID.GetReadOnly: Boolean;
 Begin
    Result := Not FObject.Active And (Inherited GetReadOnly);
 End;

{------------------------------------------------------------------------------}
{ TCreatorSecurityID.SetReadOnly(Const Value: Boolean);                        }
{ Write method property ReadOnly                                               }
{------------------------------------------------------------------------------}
 Procedure TCreatorSecurityID.SetReadOnly(Const Value: Boolean);
 Begin
    Inherited SetReadOnly(Value);
 End;

{------------------------------------------------------------------------------}
{ TCreatorSecurityID.GetSD: PSecurityDescriptor;                               }
{ Read method property Descriptor                                              }
{------------------------------------------------------------------------------}
 Function TCreatorSecurityID.GetSD: PSecurityDescriptor;
 Begin
    Result := FObject.Descriptor
 End;

{------------------------------------------------------------------------------}
{ TCreatorSecurityID.SetSD(Const Value: PSecurityDescriptor);                  }
{ Write method property Descriptor                                             }
{------------------------------------------------------------------------------}
 Procedure TCreatorSecurityID.SetSD(Const Value: PSecurityDescriptor);
 Begin
    FObject.Descriptor := Value
 End;

{------------------------------------------------------------------------------}
{ TCreatorSecurityID.GetObjSID: PSID;                                          }
{ Read method property ObjectSID                                               }
{------------------------------------------------------------------------------}
 Function TCreatorSecurityID.GetObjSID: PSID;
 Var
    fDefaulted: LongBool;
 Begin
    Case FKind Of
       akCreatorGroup:
         If Not GetSecurityDescriptorGroup(Descriptor, Result, fDefaulted) Then
            RaiseACLError;
       akCreatorOwner:
         If Not GetSecurityDescriptorOwner(Descriptor, Result, fDefaulted) Then
            RaiseACLError;
    End;
 End;

{------------------------------------------------------------------------------}
{ TCreatorSecurityID.SetObjSID(Const Value: PSID);                             }
{ Write method property ObjectSID                                              }
{------------------------------------------------------------------------------}
 Procedure TCreatorSecurityID.SetObjSID(Const Value: PSID);
 Var
    OldSID: PSID;
 Begin
    OldSID := ObjectSID;
    Case FKind Of
       akCreatorGroup:
         If Not SetSecurityDescriptorGroup(Descriptor, Value, True) Then
            RaiseACLError;
       akCreatorOwner:
         If Not SetSecurityDescriptorOwner(Descriptor, Value, True) Then
            RaiseACLError;
    End;
    FreeAndNilSID(OldSID);
 End;

{------------------------------------------------------------------------------}
{ TCreatorSecurityID.LoadID;                                                   }
{------------------------------------------------------------------------------}
 Procedure TCreatorSecurityID.LoadID;
 Begin
    ReadOnly := False; 
    LookUpSystemName := FObject.LookUpSystemName;
    DomainSID := FObject.DomainSID;
    AsSID := ObjectSID;
 End;

{------------------------------------------------------------------------------}
{ TCreatorSecurityID.SaveID;                                                   }
{------------------------------------------------------------------------------}
 Procedure TCreatorSecurityID.SaveID;
 Begin
    ObjectSID := AsSID;
 End;

{------------------------------------------------------------------------------}
{ TCreatorSecurityID.FreeID;                                                   }
{------------------------------------------------------------------------------}
 Procedure TCreatorSecurityID.FreeID;
 Begin
    ObjectSID := Nil;
    Reset;
    ReadOnly := True;
   // ClearSID;
 End;

{------------------------------------------------------------------------------}
{ TCreatorSecurityID.CleanUpID;                                                }
{------------------------------------------------------------------------------}
 Procedure TCreatorSecurityID.CleanUpID;
 Begin
    FreeID;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCreatorOwnerID                                               }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCreatorOwnerID.Create(AObject: TCustomSecurityObject);                      }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCreatorOwnerID.Create(AObject: TCustomSecurityObject);
 Begin
    Inherited Create(AObject);
    FKind := akCreatorOwner;
 End;

{==============================================================================}
{                                                                              }
{                                                                              }
{ Implementation TCreatorGroupID                                               }
{                                                                              }
{                                                                              }
{==============================================================================}
{------------------------------------------------------------------------------}
{ TCreatorGroupID.Create(AObject: TCustomSecurityObject);                      }
{ Constructor Override                                                         }
{------------------------------------------------------------------------------}
 Constructor TCreatorGroupID.Create(AObject: TCustomSecurityObject);
 Begin
    Inherited Create(AObject);
    FKind := akCreatorGroup;
 End;

{------------------------------------------------------------------------------}
{ End of aclbase.pas                                                           }
{------------------------------------------------------------------------------}
 End.

