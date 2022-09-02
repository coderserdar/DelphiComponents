(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{---
  Note: in tests we have discovered that the bindery emulation
  provided by Directory Services on NetWare 4.x is NOT perfect.
  We have discovered for example that ScanProperty will report
  a property as being a set, but ReadPropertyValue will insist
  that it's an item; also it has been known for ScanProperty to
  say that a property has a value but no segments can be found.
  Caveat programmer.
 ---}

{--Conditional defines and compiler options}
  {$I BTDefine.Inc}
  {$B+,F-,I-,R-,S-,V-}
  {$IFDEF CanAllowOverlays}
    {$F+,O+,A-}
  {$ENDIF}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-,P-}
  {$ENDIF}
  {all other compiler options are 'don't care'}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit NWBind;
  {-Unit to provide access to the NetWare bindery.}

interface

uses
  NWBase;

const
  {Object and Property Security Flags}
  nwbAnyOne     = $00;  {access allowed to all clients}
  nwbLogged     = $01;  {access allowed to logged in clients}
  nwbObject     = $02;  {access allowed to the object itself}
  nwbSupervisor = $03;  {access allowed to the supervisor}
  nwbNetWare    = $04;  {access allowed only to the NetWare operating system}

  {Bindery object types}
  nwboUnknown          = $0000;
  nwboUser             = $0001;
  nwboGroup            = $0002;
  nwboPrintQueue       = $0003;
  nwboFileServer       = $0004;
  nwboJobServer        = $0005;
  nwboGateway          = $0006;
  nwboPrintServer      = $0007;
  nwboArchiveQueue     = $0008;
  nwboArchiveServer    = $0009;
  nwboJobQueue         = $000A;
  nwboAdministration   = $000B;
  nwboNASSNAGateway    = $0021;
  nwboRemoteBridge     = $0026;
  nwboRemBridgeServer  = $0027;
  nwboTimeSyncServer   = $002D;
  nwboArchiveServerSAP = $002E;
  nwboAdvertisingPrint = $0047;
  nwboBtrieveVAP       = $0050;
  nwboPrintQueueUser   = $0051;
  nwboWild             = $FFFF;

  {Error codes}
  nwbErrServerOutOfMem   = $8996; {server out of memory}
  nwbErrMemberExists     = $89E9; {object already exists as member in set}
  nwbErrNotMember        = $89EA; {object does not exist as memebr in set}
  nwbErrNotSetProperty   = $89EB; {property is not a set}
  nwbErrNoSuchSegment    = $89EC; {segment number does not exist}
  nwbErrPropExists       = $89ED; {property already exists}
  nwbErrObjExists        = $89EE; {object already exists}
  nwbErrInvName          = $89EF; {name contains invalid characters}
  nwbErrWildcardBanned   = $89F0; {no wildcards allowed for this call}
  nwbErrInvSecurity      = $89F1; {invalid bindery security}
  nwbErrNoObjRenamePriv  = $89F3; {user has no object rename privileges}
  nwbErrNoObjDeletePriv  = $89F4; {user has no object delete privileges}
  nwbErrNoObjCreatePriv  = $89F5; {user has no object create privileges}
  nwbErrNoPropDeletePriv = $89F6; {user has no property delete privileges}
  nwbErrNoPropCreatePriv = $89F7; {user has no property create privileges}
  nwbErrNoPropWritePriv  = $89F8; {user has no property write privileges}
  nwbErrNoPropReadPriv   = $89F9; {user has no property read privileges}
  nwbErrNoSuchProperty   = $89FB; {given property does not exist}
  nwbErrNoSuchObject     = $89FC; {given object does not exist}
  nwbErrBinderyLocked    = $89FE; {the bindery is locked}
  nwbErrBinderyFailure   = $89FF; {the bindery has failed}

type
  {Bindery property value}
  TnwPropValue = record
    case boolean of
      True  : (pvItem : array [1..128] of char);
      False : (pvSet  : array [1..32] of nwLong);
  end;

  {Bindery object password string}
  TnwPasswordStr = string[127];

const
  {Some well-known property names}
  nwbLogInControlSt       = 'LOGIN_CONTROL';
  nwbAccountServersSt     = 'ACCOUNT_SERVERS';
  nwbAccountBalanceSt     = 'ACCOUNT_BALANCE';
  nwbPasswordSt           = 'PASSWORD';
  nwbSecurityEqualsSt     = 'SECURITY_EQUALS';
  nwbGroupMembersSt       = 'GROUP_MEMBERS';
  nwbGroupsImInSt         = 'GROUPS_I''M_IN';
  nwbNetAddressSt         = 'NET_ADDRESS';
  nwbIdentificationSt     = 'IDENTIFICATION';
  nwbOperatorsSt          = 'OPERATORS';

function nwbAddObjectToSet(Server  : TnwServer;
                           ObjType : word;
                           ObjName : TnwObjectStr;
                           PropName: TnwPropStr;
                           MemberObjType : word;
                           MemberObjName : TnwObjectStr) : TnwErrorCode;
  {-Add a bindery object to a property of type set}

function nwbChangePassword(Server  : TnwServer;
                           ObjType : word;
                           ObjName : TnwObjectStr;
                           OldPassword,
                           NewPassword : TnwPasswordStr) : TnwErrorCode;
  {-Change the password of a bindery object.
    Notes: This routine does not support NetWare's encrypted
    passwords.}

function nwbChangeObjectSecurity(Server  : TnwServer;
                                 ObjType : word;
                                 ObjName : TnwObjectStr;
                                 NewSecurity : byte) : TnwErrorCode;
  {-Change the read/write security of a bindery object}

function nwbChangePropertySecurity(Server  : TnwServer;
                                   ObjType : word;
                                   ObjName : TnwObjectStr;
                                   PropName: TnwPropStr;
                                   NewPropSecurity : byte) : TnwErrorCode;
  {-Change the read/write security of a bindery object's property}

function nwbCloseBindery(Server : TnwServer) : TnwErrorCode;
  {-Close the bindery
    Note: ONLY use this routine for backing up the bindery files.
    Whilst closed most NetWare functionality is disabled. This
    function is ignored under the bindery emulation in NetWare 4.x.}

function nwbCreateObject(Server  : TnwServer;
                         ObjType : word;
                         ObjName : TnwObjectStr;
                         ObjIsDynamic : boolean;
                         ObjSecurity  : byte) : TnwErrorCode;
  {-Create a new bindery object}

function nwbCreateProperty(Server  : TnwServer;
                           ObjType  : word;
                           ObjName  : TnwObjectStr;
                           PropName : TnwPropStr;
                           PropIsDynamic,
                           PropIsSet    : boolean;
                           PropSecurity : byte) : TnwErrorCode;
  {-Create a new property for a bindery object}

function nwbDeleteObject(Server  : TnwServer;
                         ObjType : word;
                         ObjName : TnwObjectStr) : TnwErrorCode;
  {-Delete a bindery object}

function nwbDeleteObjectFromSet(Server  : TnwServer;
                                ObjType : word;
                                ObjName : TnwObjectStr;
                                PropName: TnwPropStr;
                                MemberObjType : word;
                                MemberObjName : TnwObjectStr) : TnwErrorCode;
  {-Delete a bindery object from a property of type set}

function nwbDeleteProperty(Server  : TnwServer;
                           ObjType : word;
                           ObjName : TnwObjectStr;
                           PropName: TnwPropStr) : TnwErrorCode;
  {-Delete a property from a bindery object}

function nwbGetBinderyAccessLevel(Server  : TnwServer;
                              var AccessLevel : byte;
                              var ObjID : nwLong) : TnwErrorCode;
  {-Return the workstation's access level to the bindery
    Notes: the AccessLevel returned is a standard security byte: the
    high nibble value is the user's write privileges, the low nibble
    value is the read privileges. The ObjID returned is the logged
    on user's bindery object ID.}

function nwbGetObjectID(Server  : TnwServer;
                        ObjType : word;
                        ObjName : TnwObjectStr;
                    var ObjID   : nwLong) : TnwErrorCode;
  {-Return the bindery object's ID given its name and type}

function nwbGetObjectName(Server  : TnwServer;
                          ObjID   : nwLong;
                      var ObjType : word;
                      var ObjName : TnwObjectStr) : TnwErrorCode;
  {-Return the bindery object's name and type given its ID}

function nwbIsObjectInSet(Server  : TnwServer;
                          ObjType : word;
                          ObjName : TnwObjectStr;
                          PropName: TnwPropStr;
                          MemberObjType : word;
                          MemberObjName : TnwObjectStr) : TnwErrorCode;
  {-Determine if a bindery object is in a property of type set.
    Notes: if the function result is 0 the object exists in the set,
    if $89EA (nwbErrNotMember) the object is not a member of the set,
    otherwise some other error occurred.}

function nwbOpenBindery(Server : TnwServer) : TnwErrorCode;
  {-Open the bindery that was closed with CloseBindery}

function nwbReadPropertyValue(Server  : TnwServer;
                              ObjType : word;
                              ObjName : TnwObjectStr;
                              PropName: TnwPropStr;
                              SegmentNumber: byte;
                          var PropValue    : TnwPropValue;
                          var PropIsDynamic: boolean;
                          var PropIsSet    : boolean;
                          var MoreSegments : boolean) : TnwErrorCode;
  {-Return the value of a bindery object's property.
    Notes: a property consists of 1 or more 128-byte segments; the
    first segment is segment 1. To read all the segments, continue
    calling this routine, starting at SegmentNumber = 1, incrementing
    it for each call until MoreSegments is false.}

function nwbRenameObject(Server  : TnwServer;
                         ObjType : word;
                         OldObjName,
                         NewObjName : TnwObjectStr) : TnwErrorCode;
  {-Rename a bindery object.
    Notes: must be the Supervisor or equivalent to use this call.}

function nwbScanObject(Server  : TnwServer;
                       var ObjType       : word;
                       var ObjName       : TnwObjectStr;
                       var ObjID         : nwLong;
                       var ObjIsDynamic  : boolean;
                       var ObjSecurity   : byte;
                       var HasProperties : boolean) : TnwErrorCode;
  {-Scan the bindery for an object.
    Notes: this routine allows an application to iteratively scan the
    bindery for a sequence of objects. Before making the first call
    ObjID must be set to -1. For the next and subsequent calls the
    ObjID must be set to the object ID that the previous call
    returned. Before making any call, ObjType must be set to the
    object type required (or nwboWild for any object type), and
    ObjName must be set to the actual name required or a string with
    a '*' wildcard character (eg 'N*' for objects whose name starts
    with 'N'). (Note that to scan for a single object for which you
    know the ID, Name and Type, you must still set ObjID to -1.)
    The returned ObjSecurity is a standard security byte: the high
    nibble value is the user's write privileges, the low nibble value
    is the read privileges.}

function nwbScanProperty(Server  : TnwServer;
                         ObjType : word;
                         ObjName : TnwObjectStr;
                     var Sequence : nwLong;
                     var PropName : TnwPropStr;
                     var PropIsDynamic: boolean;
                     var PropIsSet    : boolean;
                     var PropSecurity : byte;
                     var HasValue : boolean;
                     var MoreProps: boolean) : TnwErrorCode;
  {-Scan the bindery for an object's properties.
    Notes: this routine allows an application to iteratively scan the
    bindery for the properties associated with a bindery object.
    Before making the first call Sequence must be set to -1. For the
    next and subsequent calls Sequence must be set to the value that
    the previous call returned. MoreProps is set false when the
    ScanProperty routine has scanned the last property. Before making
    any call, ObjType must be set to the object type and ObjName to the
    actual name required (no wildcards are allowed). The returned
    PropSecurity is a standard security byte: the high nibble value is
    the user's write privileges, the low nibble value is the read
    privileges.
    WARNING: under NetWare 4.x the PropIsDynamic and PropIsSet
    booleans do not seem to be returned properly by the bindery
    emulation - the ReadPropertyValue routine however does give the
    correct values.}

function nwbVerifyPassword(Server  : TnwServer;
                           ObjType : word;
                           ObjName : TnwObjectStr;
                           Password: TnwPasswordStr) : TnwErrorCode;
  {-Verify the password of a bindery object.
    Notes: This routine does not support NetWare's encrypted
    passwords.}

function nwbWritePropertyValue(Server  : TnwServer;
                               ObjType : word;
                               ObjName : TnwObjectStr;
                               PropName: TnwPropStr;
                               SegmentNumber : byte;
                           var PropValue : TnwPropValue;
                               EraseRemainingSegments : boolean) : TnwErrorCode;
  {-Write a value to a property of a bindery object.
    Notes: To write a value to a property you must split it first
    into 128-byte segments and then write the segments in sequential
    order starting at 1. For writing the final segment, ensure that
    the parameter EraseRemainingSegments is set to true. Once the
    segments have all been created, you may update them in any
    order. This call must not be used for writing a property of type
    set, for that you iteratively call AddObjectToSet.}

implementation

type
  PReqBuffer = ^TReqBuffer; { A NetWare bindery request buffer }
  TReqBuffer = record
    Size : word;
    Data : array [0..509] of byte;
  end;

procedure nwbPackByte(var Buffer : TReqBuffer; B : byte); near;
  begin
    with Buffer do
      begin
        Data[Size] := B;
        inc(Size);
      end;
  end;

procedure nwbPackWord(var Buffer : TReqBuffer; W : word); near;
  var
    W_asBytes : array [0..1] of byte absolute W;
  begin
    with Buffer do
      begin
        Data[Size] := W_asBytes[1];
        Data[Size+1] := W_asBytes[0];
        inc(Size, 2);
      end;
  end;

procedure nwbPackLong(var Buffer : TReqBuffer; L : nwLong); near;
  begin
    with Buffer do
      begin
        Move(L, Data[Size], 4);
        inc(Size, 4);
      end;
  end;

procedure nwbPackString(var Buffer : TReqBuffer; var S : string); near;
  begin
    with Buffer do
      begin
        Move(S, Data[Size], succ(length(S)));
        inc(Size, succ(length(S)));
      end;
  end;

function nwbAddObjectToSet(Server  : TnwServer;
                           ObjType : word;
                           ObjName : TnwObjectStr;
                           PropName: TnwPropStr;
                           MemberObjType : word;
                           MemberObjName : TnwObjectStr) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    nwUpperStr(PropName);
    nwUpperStr(MemberObjName);
    {API: Add Object To Set}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $41);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbPackString(Request, PropName);
    nwbPackWord(Request, MemberObjType);
    nwbPackString(Request, MemberObjName);
    nwbAddObjectToSet := nwServerCall(Server, $17, Request.Size+2, Request,
                                                   0, Dummy);
  end;

function nwbChangePassword(Server  : TnwServer;
                           ObjType : word;
                           ObjName : TnwObjectStr;
                           OldPassword,
                           NewPassword : TnwPasswordStr) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    {API: Change Bindery Object Password}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $40);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbPackString(Request, OldPassword);
    nwbPackString(Request, NewPassword);
    nwbChangePassword := nwServerCall(Server, $17, Request.Size+2, Request,
                                                   0, Dummy);
  end;

function nwbChangeObjectSecurity(Server  : TnwServer;
                                 ObjType : word;
                                 ObjName : TnwObjectStr;
                                 NewSecurity : byte) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    {API: Change Bindery Object Security}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $38);
    nwbPackByte(Request, NewSecurity);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbChangeObjectSecurity := nwServerCall(Server, $17,
                                            Request.Size+2, Request,
                                            0, Dummy);
  end;

function nwbChangePropertySecurity(Server  : TnwServer;
                                   ObjType : word;
                                   ObjName : TnwObjectStr;
                                   PropName: TnwPropStr;
                                   NewPropSecurity : byte) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    nwUpperStr(PropName);
    {API: Change Property Security}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $3B);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbPackByte(Request, NewPropSecurity);
    nwbPackString(Request, PropName);
    nwbChangePropertySecurity := nwServerCall(Server, $17,
                                              Request.Size+2, Request,
                                              0, Dummy);
  end;

function nwbCloseBindery(Server : TnwServer) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    {API: Close Bindery}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $44);
    nwbCloseBindery := nwServerCall(Server, $17, Request.Size+2, Request,
                                                 0, Dummy);
  end;

function nwbCreateObject(Server  : TnwServer;
                         ObjType : word;
                         ObjName : TnwObjectStr;
                         ObjIsDynamic : boolean;
                         ObjSecurity  : byte) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    {API: Create Bindery Object}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $32);
    nwbPackByte(Request, byte(ObjIsDynamic));
    nwbPackByte(Request, ObjSecurity);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbCreateObject := nwServerCall(Server, $17, Request.Size+2, Request,
                                                 0, Dummy);
  end;

function nwbCreateProperty(Server  : TnwServer;
                           ObjType  : word;
                           ObjName  : TnwObjectStr;
                           PropName : TnwPropStr;
                           PropIsDynamic,
                           PropIsSet    : boolean;
                           PropSecurity : byte) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    nwUpperStr(PropName);
    {API: Create Property}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $39);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbPackByte(Request, (byte(PropIsSet) shl 1) + byte(PropIsDynamic));
    nwbPackByte(Request, PropSecurity);
    nwbPackString(Request, PropName);
    nwbCreateProperty := nwServerCall(Server, $17, Request.Size+2, Request,
                                                   0, Dummy);
  end;

function nwbDeleteObject(Server  : TnwServer;
                         ObjType : word;
                         ObjName : TnwObjectStr) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    {API: Delete Bindery Object}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $33);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbDeleteObject := nwServerCall(Server, $17, Request.Size+2, Request,
                                                 0, Dummy);
  end;

function nwbDeleteObjectFromSet(Server  : TnwServer;
                                ObjType : word;
                                ObjName : TnwObjectStr;
                                PropName: TnwPropStr;
                                MemberObjType : word;
                                MemberObjName : TnwObjectStr) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    nwUpperStr(PropName);
    nwUpperStr(MemberObjName);
    {API: Delete Bindery Object From Set}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $42);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbPackString(Request, PropName);
    nwbPackWord(Request, MemberObjType);
    nwbPackString(Request, MemberObjName);
    nwbDeleteObjectFromSet := nwServerCall(Server, $17,
                                           Request.Size+2, Request,
                                           0, Dummy);
  end;

function nwbDeleteProperty(Server  : TnwServer;
                           ObjType : word;
                           ObjName : TnwObjectStr;
                           PropName: TnwPropStr) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    nwUpperStr(PropName);
    {API: Delete Property}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $3A);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbPackString(Request, PropName);
    nwbDeleteProperty := nwServerCall(Server, $17, Request.Size+2, Request,
                                                   0, Dummy);
  end;

function nwbGetBinderyAccessLevel(Server  : TnwServer;
                              var AccessLevel : byte;
                              var ObjID : nwLong) : TnwErrorCode;
  var
    Status  : word;
    Request : TReqBuffer;
    Reply : record
      ALvl  : byte;
      ObjID : nwLong;
    end;
  begin
    {API: Get Bindery Access Level}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $46);
    FillChar(Reply, sizeof(Reply), 0);
    Status := nwServerCall(Server, $17, Request.Size+2, Request,
                                                sizeof(Reply), Reply);
    if (Status = 0) then
      begin
        AccessLevel := Reply.ALvl;
        ObjID := Reply.ObjID;
      end;
    nwbGetBinderyAccessLevel := Status;
  end;

function nwbGetObjectID(Server  : TnwServer;
                        ObjType : word;
                        ObjName : TnwObjectStr;
                    var ObjID   : nwLong) : TnwErrorCode;
  var
    Reply : record
      ObjI : nwLong;
      ObjT : word;
      ObjN : array [0..47] of char;
    end;
    Status : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    {API: Get Bindery Object ID}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $35);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    FillChar(Reply, sizeof(Reply), 0);
    Status := nwServerCall(Server, $17, Request.Size+2, Request,
                                                sizeof(Reply), Reply);
    if (Status = 0) then
      with Reply do
        begin
          ObjID := ObjI;
          ObjType := Swap(ObjT);
          Move(ObjN, ObjName, sizeof(TnwObjectStr));
          nwCvtAsciizToStr(ObjName, pred(sizeof(TnwObjectStr)));
        end;
    nwbGetObjectID := Status;
  end;

function nwbGetObjectName(Server  : TnwServer;
                          ObjID   : nwLong;
                      var ObjType : word;
                      var ObjName : TnwObjectStr) : TnwErrorCode;
  var
    Reply : record
      ObjI : nwLong;
      ObjT : word;
      ObjN : array [0..47] of char;
    end;
    Status : word;
    Request : TReqBuffer;
  begin
    {API: Get Bindery Object Name}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $36);
    nwbPackLong(Request, ObjID);
    FillChar(Reply, sizeof(Reply), 0);
    Status := nwServerCall(Server, $17, Request.Size+2, Request,
                                                sizeof(Reply), Reply);
    if (Status = 0) then
      with Reply do
        begin
          ObjType := Swap(ObjT);
          Move(ObjN, ObjName, sizeof(TnwObjectStr));
          nwCvtAsciizToStr(ObjName, pred(sizeof(TnwObjectStr)));
        end;
    nwbGetObjectName := Status;
  end;

function nwbIsObjectInSet(Server  : TnwServer;
                          ObjType : word;
                          ObjName : TnwObjectStr;
                          PropName: TnwPropStr;
                          MemberObjType : word;
                          MemberObjName : TnwObjectStr) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    nwUpperStr(PropName);
    nwUpperStr(MemberObjName);
    {API: Is Bindery Object In Set}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $43);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbPackString(Request, PropName);
    nwbPackWord(Request, MemberObjType);
    nwbPackString(Request, MemberObjName);
    nwbIsObjectInSet := nwServerCall(Server, $17, Request.Size+2, Request,
                                                  0, Dummy);
  end;

function nwbOpenBindery(Server : TnwServer) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    {API: Open Bindery}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $45);
    nwbOpenBindery := nwServerCall(Server, $17, Request.Size+2, Request,
                                                0, Dummy);
  end;

function nwbReadPropertyValue(Server  : TnwServer;
                              ObjType : word;
                              ObjName : TnwObjectStr;
                              PropName: TnwPropStr;
                              SegmentNumber : byte;
                          var PropValue : TnwPropValue;
                          var PropIsDynamic: boolean;
                          var PropIsSet    : boolean;
                          var MoreSegments : boolean) : TnwErrorCode;
  var
    Reply : record
      Prop: TnwPropValue;
      More: byte;
      Flgs: byte;
    end;
    Status : word;
    i : integer;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    nwUpperStr(PropName);
    {API: Read Property Value}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $3D);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbPackByte(Request, SegmentNumber);
    nwbPackString(Request, PropName);
    FillChar(Reply, sizeof(Reply), 0);
    Status := nwServerCall(Server, $17, Request.Size+2, Request,
                                        sizeof(Reply), Reply);
    if (Status = 0) then
      with Reply do
        begin
          PropIsDynamic := (Flgs and $01) <> 0;
          PropIsSet := (Flgs and $02) <> 0;
          PropValue := Prop;
          MoreSegments  := More <> 0;
        end;
    nwbReadPropertyValue := Status;
  end;

function nwbRenameObject(Server  : TnwServer;
                         ObjType : word;
                         OldObjName,
                         NewObjName : TnwObjectStr) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(OldObjName);
    nwUpperStr(NewObjName);
    {API: Rename Bindery Object}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $34);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, OldObjName);
    nwbPackString(Request, NewObjName);
    nwbRenameObject := nwServerCall(Server, $17, Request.Size+2, Request,
                                                 0, Dummy);
  end;

function nwbScanObject(Server  : TnwServer;
                       var ObjType       : word;
                       var ObjName       : TnwObjectStr;
                       var ObjID         : nwLong;
                       var ObjIsDynamic  : boolean;
                       var ObjSecurity   : byte;
                       var HasProperties : boolean) : TnwErrorCode;
  var
    Status : word;
    Request : TReqBuffer;
    Reply : record
      ObjI: nwLong;
      ObjT: word;
      ObjN: array [0..47] of char;
      ObjF: byte;
      ObjS: byte;
      ObjP: byte;
    end;
  begin
    nwUpperStr(ObjName);
    {API: Scan Bindery Object}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $37);
    nwbPackLong(Request, ObjID);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    FillChar(Reply, sizeof(Reply), 0);
    Status := nwServerCall(Server, $17, Request.Size+2, Request,
                                                sizeof(Reply), Reply);
    if (Status = 0) then
      with Reply do
        begin
          ObjID := ObjI;
          ObjType := Swap(ObjT);
          Move(ObjN, ObjName, sizeof(TnwObjectStr));
          nwCvtAsciizToStr(ObjName, pred(sizeof(TnwObjectStr)));
          ObjIsDynamic := (ObjF and $01) <> 0;
          ObjSecurity := ObjS;
          HasProperties := ObjP <> 0;
        end;
    nwbScanObject := Status;
  end;

function nwbScanProperty(Server  : TnwServer;
                         ObjType : word;
                         ObjName : TnwObjectStr;
                     var Sequence : nwLong;
                     var PropName : TnwPropStr;
                     var PropIsDynamic: boolean;
                     var PropIsSet    : boolean;
                     var PropSecurity : byte;
                     var HasValue : boolean;
                     var MoreProps: boolean) : TnwErrorCode;
  var
    Reply : record
      PNam: array [0..15] of char;
      PFlg: byte;
      PSec: byte;
      SeqN: nwLong;
      PVal: byte;
      PMor: byte;
    end;
    Status : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    nwUpperStr(PropName);
    {API: Scan Property}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $3C);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbPackLong(Request, Sequence);
    nwbPackString(Request, PropName);
    FillChar(Reply, sizeof(Reply), 0);
    Status := nwServerCall(Server, $17, Request.Size+2, Request,
                                                sizeof(Reply), Reply);
    if (Status = 0) then
      with Reply do
        begin
          Move(PNam, PropName, sizeof(TnwPropStr));
          nwCvtAsciizToStr(PropName, pred(sizeof(TnwPropStr)));
          PropIsDynamic := (PFlg and $01) <> 0;
          PropIsSet := (PFlg and $02) <> 0;
          PropSecurity := PSec;
          Sequence := SeqN;
          HasValue := PVal <> 0;
          MoreProps := PMor <> 0;
        end;
    nwbScanProperty := Status;
  end;

function nwbVerifyPassword(Server  : TnwServer;
                           ObjType : word;
                           ObjName : TnwObjectStr;
                           Password: TnwPasswordStr) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    {API: Verify Bindery Object Password}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $3F);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbPackString(Request, Password);
    nwbVerifyPassword := nwServerCall(Server, $17, Request.Size+2, Request,
                                                   0, Dummy);
  end;

function nwbWritePropertyValue(Server  : TnwServer;
                               ObjType : word;
                               ObjName : TnwObjectStr;
                               PropName: TnwPropStr;
                               SegmentNumber : byte;
                           var PropValue : TnwPropValue;
                               EraseRemainingSegments : boolean) : TnwErrorCode;
  var
    Dummy   : word;
    Request : TReqBuffer;
  begin
    nwUpperStr(ObjName);
    nwUpperStr(PropName);
    {API: Write Property Value}
    FillChar(Request, sizeof(Request), 0);
    nwbPackByte(Request, $3E);
    nwbPackWord(Request, ObjType);
    nwbPackString(Request, ObjName);
    nwbPackByte(Request, SegmentNumber);
    nwbPackByte(Request, byte(EraseRemainingSegments));
    nwbPackString(Request, PropName);
    with Request do
      begin
        Move(PropValue, Data[Size], sizeof(TnwPropValue));
        inc(Size, sizeof(TnwPropValue));
      end;
    nwbWritePropertyValue := nwServerCall(Server, $17,
                                          Request.Size+2, Request,
                                          0, Dummy);
  end;

end.

