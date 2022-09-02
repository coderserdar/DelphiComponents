{*********************************************************}
{* FlashFiler: Client network configuration definition   *}
{*********************************************************}

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
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)


{NOTES:

  This unit is the client API for the client network configuration for
  FlashFiler.  The default protocol and optional fixed servername to
  connect to are stored in the registry or in a Windows INI file and all
  FlashFiler clients share this default information.

  If the protocol is missing in the client configuration (or no client
  configuration setup) or is invalid, then the value in ffclProtocol
  (FFCLCFG.INC) at compile-time is used.  Likewise, if no value is
  found for servername in the client configuration, then the value in
  ffclServerName (FFCLCFG.INC) at compile-time is used.

  In this manner, all apps will continue to work as before until when
  and if the persistent client info is established on the workstation.
}

{$I ffdefine.inc}

unit ffclcfg;

interface

uses
  Windows,
  {$IFDEF UseRegistryConfig}
  Registry,
  {$ENDIF}
  {$IFDEF UseINIConfig}
  INIFiles,
  {$ENDIF}
  SysUtils,
  Classes,
  ffconst,
  ffclbase,
  ffllbase,
  ffllprot;

function FFClientConfigGetProtocolName(aProtocol : TffCommsProtocolClass
                                      ) : TffShStr;
{- Returns the name for the given protocol }

procedure FFClientConfigGetProtocolNames(aNames : TStrings);
{- Returns a list of protocol names valid for this platform (16-bit or 32-bit)}

procedure FFClientConfigOverrideProtocol(aProtocol : TffCommsProtocolClass);
{- Overrides the protocol defined in the client configuration info for this
   machine.  Sessions created by this app will use the override protocol until
   the override is turned off by passing in a nil parameter. }

procedure FFClientConfigOverrideServerName(const aServerName : TffNetAddress);
{- Overrides the servername defined in the client configuration info for this
   machine.  Sessions created by this app will use the override servername
   until the override is turned off by passing in a '' parameter. }

procedure FFClientConfigReadProtocol(var aProtocol     : TffCommsProtocolClass;
                                     var aProtocolName : TffShStr);
{- Returns the protocol name and class defined in the client configuration
   for this machine}

function FFClientConfigReadProtocolClass : TffCommsProtocolClass;
{- Returns the protocol class defined in the client configuration for this
   machine}

function FFClientConfigReadServerName : TffNetAddress;
{- Returns the fixed servername defined in the client configuration for this
   machine}

procedure FFClientConfigWriteProtocolName(aProtocolName : TffShStr);
{- Saves the protocol by name in the client configuration for this machine }

procedure FFClientConfigWriteProtocolClass(aProtocol : TffCommsProtocolClass);
{- Saves the protocol by class in the client configuration for this machine }

procedure FFClientConfigWriteServerName(aServerName : TffNetAddress);
{- Saves the fixed servername in the client configuration for this machine }

const
  ffc_SingleUser    = 'Single User';
  ffc_TCPIP         = 'TCP/IP';
  ffc_IPXSPX        = 'IPX/SPX';

implementation

const
  {$IFDEF UseRegistryConfig}
  cfgRootKey               = HKEY_LOCAL_MACHINE;
  cfgRegistryKey           = '\Client Configuration';
  {$ENDIF}

  {$IFDEF UseINIConfig}
  cfgSection               = 'Client Configuration';
  {$ENDIF}

  cfgServerName            = 'ServerName';
  cfgProtocol              = 'Protocol';

var
  OverrideProtocol   : TffCommsProtocolClass;
  OverrideServerName : TffNetAddress;


function FFClientConfigGetProtocolName(aProtocol : TffCommsProtocolClass
                                      ): TffShStr;
begin
  if aProtocol = TffSingleUserProtocol then
    Result := ffc_SingleUser
  else
  if aProtocol = TffTCPIPProtocol then
    Result := ffc_TCPIP
  else
  if aProtocol = TffIPXSPXProtocol then
    Result := ffc_IPXSPX
  else
    Result := '';
end;

{$IFDEF UseRegistryConfig}
function GetRegistryKey : TffShStr;
begin
  Result := ffStrResClient[ffccREG_PRODUCT] + cfgRegistryKey;
end;
{$ENDIF}

{$IFDEF UseINIConfig}
function GetINIFilename : TffShStr;
begin
  Result := 'FF2.INI';
end;
{$ENDIF}

procedure FFClientConfigGetProtocolNames(aNames : TStrings);
begin
  Assert(Assigned(aNames));
  aNames.BeginUpdate;
  try
    aNames.Clear;
    aNames.Add(ffc_SingleUser);
    aNames.Add(ffc_TCPIP);
    aNames.Add(ffc_IPXSPX);
  finally
    aNames.EndUpdate;
  end;
end;

procedure FFClientConfigOverrideProtocol(aProtocol : TffCommsProtocolClass);
begin
  OverrideProtocol := aProtocol;
end;

procedure FFClientConfigOverrideServerName(const aServerName : TffNetAddress);
begin
  OverrideServerName := aServerName;
end;

procedure FFClientConfigReadProtocol(var aProtocol     : TffCommsProtocolClass;
                                     var aProtocolName : TffShStr);
begin
  aProtocol := nil;
  aProtocolName := '';

  if Assigned(OverrideProtocol) then begin
    aProtocol := OverrideProtocol;
    aProtocolName := FFClientConfigGetProtocolName(aProtocol);
    Exit;
  end;

  {$IFDEF UseRegistryConfig}
  with TRegistry.Create do
    try
      RootKey := cfgRootKey;
      {$IFDEF DCC4OrLater}
      OpenKeyReadOnly(GetRegistryKey);
      {$ELSE}
      OpenKey(GetRegistryKey, True);
      {$ENDIF}
      if ValueExists(cfgProtocol) then
        aProtocolName := ReadString(cfgProtocol);
    finally
      Free;
    end;
  {$ENDIF}
  {$IFDEF UseINIConfig}
  with TINIFile.Create(GetINIFilename) do
    try
      aProtocolName := ReadString(cfgSection, cfgProtocol, '');
    finally
      Free;
    end;
  {$ENDIF}
  if FFCmpShStrUC(aProtocolName, ffc_TCPIP, 255) = 0 then
    aProtocol := TffTCPIPProtocol
  else
  if FFCmpShStrUC(aProtocolName, ffc_IPXSPX, 255) = 0 then
    aProtocol := TffIPXSPXProtocol
  else
  if FFCmpShStrUC(aProtocolName, ffc_SingleUser, 255) = 0 then
    aProtocol := TffSingleUserProtocol
  else begin  { use compiled default protocol }
    aProtocol := ffclProtocol;
    aProtocolName := FFClientConfigGetProtocolName(aProtocol);
    if aProtocolName = '' then
      aProtocol := nil;
  end;
end;

function FFClientConfigReadProtocolClass : TffCommsProtocolClass;
var
  ProtocolName : TffShStr;
begin
  FFClientConfigReadProtocol(Result, ProtocolName);
end;

function FFClientConfigReadServerName : TffNetAddress;
begin
  Result := '';                                                        {!!.01}
  if OverrideServerName <> '' then begin
    Result := OverrideServerName;
    Exit;
  end;

  {$IFDEF UseRegistryConfig}
  Result := '';
  with TRegistry.Create do
    try
      RootKey := cfgRootKey;
      {$IFDEF DCC4OrLater}
      OpenKeyReadOnly(GetRegistryKey);
      {$ELSE}
      OpenKey(GetRegistryKey, True);
      {$ENDIF}
      if ValueExists(cfgServerName) then
        Result := ReadString(cfgServerName);
    finally
      Free;
    end;
  {$ENDIF}

  {$IFDEF UseINIConfig}
  with TINIFile.Create(GetINIFilename) do
    try
      Result := ReadString(cfgSection, cfgServerName, '');
    finally
      Free;
    end;
  {$ENDIF}

  { if no name given, use compiled default name }
  if Result = '' then
    Result := ffclServerName;
end;

procedure FFClientConfigWriteProtocolName(aProtocolName : TffShStr);
begin
  {$IFDEF UseRegistryConfig}
  with TRegistry.Create do
    try
      RootKey := cfgRootKey;
      OpenKey(GetRegistryKey, True);
      WriteString(cfgProtocol, aProtocolName);
    finally
      Free;
    end;
  {$ENDIF}

  {$IFDEF UseINIConfig}
  with TINIFile.Create(GetINIFilename) do
    try
      WriteString(cfgSection, cfgProtocol, aProtocolName);
    finally
      Free;
    end;
  {$ENDIF}
end;

procedure FFClientConfigWriteProtocolClass(aProtocol : TffCommsProtocolClass);
begin
  FFClientConfigWriteProtocolName(FFClientConfigGetProtocolName(aProtocol));
end;

procedure FFClientConfigWriteServerName(aServerName : TffNetAddress);
begin
  {$IFDEF UseRegistryConfig}
  with TRegistry.Create do
    try
      RootKey := cfgRootKey;
      OpenKey(GetRegistryKey, True);
      WriteString(cfgServerName, aServerName);
    finally
      Free;
    end;
  {$ENDIF}

  {$IFDEF UseINIConfig}
  with TINIFile.Create(GetINIFilename) do
    try
      WriteString(cfgSection, cfgServerName, aServerName);
    finally
      Free;
    end;
  {$ENDIF}
end;

initialization
  OverrideProtocol := nil;
  OverrideServerName := '';

end.
