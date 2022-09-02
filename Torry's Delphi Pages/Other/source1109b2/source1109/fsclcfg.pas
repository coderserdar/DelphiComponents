{NOTES:

  This unit is the client API for the client network configuration for
  FlashFiler.  The default protocol and optional fixed servername to
  connect to are stored in the registry or in a Windows INI file and all
  FlashFiler clients share this default information.

  If the protocol is missing in the client configuration (or no client
  configuration setup) or is invalid, then the value in fsclProtocol
  (FFCLCFG.INC) at compile-time is used.  Likewise, if no value is
  found for servername in the client configuration, then the value in
  fsclServerName (FFCLCFG.INC) at compile-time is used.

  In this manner, all apps will continue to work as before until when
  and if the persistent client info is established on the workstation.
}

{$I fsdefine.inc}

Unit fsclcfg;

Interface

Uses
  Windows,
  {$IFDEF UseRegistryConfig}
  Registry,
  {$ENDIF}
  {$IFDEF UseINIConfig}
  INIFiles,
  {$ENDIF}
  SysUtils,
  Classes,
  fsconst,
  fsclbase,
  fsllbase,
  fsllprot;

Function FFClientConfigGetProtocolName(aProtocol: TfsCommsProtocolClass
  ): TffShStr;
{- Returns the name for the given protocol }

Procedure FFClientConfigGetProtocolNames(aNames: TStrings);
{- Returns a list of protocol names valid for this platform (16-bit or 32-bit)}

Procedure FFClientConfigOverrideProtocol(aProtocol: TfsCommsProtocolClass);
{- Overrides the protocol defined in the client configuration info for this
   machine.  Sessions created by this app will use the override protocol until
   the override is turned off by passing in a nil parameter. }

Procedure FFClientConfigOverrideServerName(Const aServerName: TffNetAddress);
{- Overrides the servername defined in the client configuration info for this
   machine.  Sessions created by this app will use the override servername
   until the override is turned off by passing in a '' parameter. }

Procedure FFClientConfigReadProtocol(Var aProtocol: TfsCommsProtocolClass;
  Var aProtocolName: TffShStr);
{- Returns the protocol name and class defined in the client configuration
   for this machine}

Function FFClientConfigReadProtocolClass: TfsCommsProtocolClass;
{- Returns the protocol class defined in the client configuration for this
   machine}

Function FFClientConfigReadServerName: TffNetAddress;
{- Returns the fixed servername defined in the client configuration for this
   machine}

Procedure FFClientConfigWriteProtocolName(aProtocolName: TffShStr);
{- Saves the protocol by name in the client configuration for this machine }

Procedure FFClientConfigWriteProtocolClass(aProtocol: TfsCommsProtocolClass);
{- Saves the protocol by class in the client configuration for this machine }

Procedure FFClientConfigWriteServerName(aServerName: TffNetAddress);
{- Saves the fixed servername in the client configuration for this machine }

Const
  fsc_SingleUser = 'Single';
  fsc_TCPIP = 'TCP/IP';
  fsc_IPXSPX = 'IPX/SPX';

Implementation

Const
  {$IFDEF UseRegistryConfig}
  cfgRootKey = HKEY_LOCAL_MACHINE;
  cfgRegistryKey = '\Client';
  {$ENDIF}

  {$IFDEF UseINIConfig}
  cfgSection = 'Client';
  {$ENDIF}

  cfgServerName = 'ServerName';
  cfgProtocol = 'Protocol';

Var
  OverrideProtocol: TfsCommsProtocolClass;
  OverrideServerName: TffNetAddress;

Function FFClientConfigGetProtocolName(aProtocol: TfsCommsProtocolClass
  ): TffShStr;
Begin
  If aProtocol = TfsSingleUserProtocol Then
    Result := fsc_SingleUser
  Else If aProtocol = TfsTCPIPProtocol Then
    Result := fsc_TCPIP
  Else If aProtocol = TfsIPXSPXProtocol Then
    Result := fsc_IPXSPX
  Else
    Result := '';
End;

{$IFDEF UseRegistryConfig}

Function GetRegistryKey: TffShStr;
Begin
  Result := fsStrResClient[fsccREG_PRODUCT] + cfgRegistryKey;
End;
{$ENDIF}

{$IFDEF UseINIConfig}

Function GetINIFilename: TffShStr;
Begin
  Result := 'FS.INI';
End;
{$ENDIF}

Procedure FFClientConfigGetProtocolNames(aNames: TStrings);
Begin
  Assert(Assigned(aNames));
  aNames.BeginUpdate;
  Try
    aNames.Clear;
    aNames.Add(fsc_SingleUser);
    aNames.Add(fsc_TCPIP);
    aNames.Add(fsc_IPXSPX);
  Finally
    aNames.EndUpdate;
  End;
End;

Procedure FFClientConfigOverrideProtocol(aProtocol: TfsCommsProtocolClass);
Begin
  OverrideProtocol := aProtocol;
End;

Procedure FFClientConfigOverrideServerName(Const aServerName: TffNetAddress);
Begin
  OverrideServerName := aServerName;
End;

Procedure FFClientConfigReadProtocol(Var aProtocol: TfsCommsProtocolClass;
  Var aProtocolName: TffShStr);
Begin
  aProtocol := Nil;
  aProtocolName := '';

  If Assigned(OverrideProtocol) Then
    Begin
      aProtocol := OverrideProtocol;
      aProtocolName := FFClientConfigGetProtocolName(aProtocol);
      Exit;
    End;

  {$IFDEF UseRegistryConfig}
  With TRegistry.Create Do
    Try
      RootKey := cfgRootKey;
      {$IFDEF DCC4OrLater}
      OpenKeyReadOnly(GetRegistryKey);
      {$ELSE}
      OpenKey(GetRegistryKey, True);
      {$ENDIF}
      If ValueExists(cfgProtocol) Then
        aProtocolName := ReadString(cfgProtocol);
    Finally
      Free;
    End;
  {$ENDIF}
  {$IFDEF UseINIConfig}
  With TINIFile.Create(GetINIFilename) Do
    Try
      aProtocolName := ReadString(cfgSection, cfgProtocol, '');
    Finally
      Free;
    End;
  {$ENDIF}
  If FFCmpShStrUC(aProtocolName, fsc_TCPIP, 255) = 0 Then
    aProtocol := TfsTCPIPProtocol
  Else If FFCmpShStrUC(aProtocolName, fsc_IPXSPX, 255) = 0 Then
    aProtocol := TfsIPXSPXProtocol
  Else If FFCmpShStrUC(aProtocolName, fsc_SingleUser, 255) = 0 Then
    aProtocol := TfsSingleUserProtocol
  Else
    Begin { use compiled default protocol }
      aProtocol := fsclProtocol;
      aProtocolName := FFClientConfigGetProtocolName(aProtocol);
      If aProtocolName = '' Then
        aProtocol := Nil;
    End;
End;

Function FFClientConfigReadProtocolClass: TfsCommsProtocolClass;
Var
  ProtocolName: TffShStr;
Begin
  FFClientConfigReadProtocol(Result, ProtocolName);
End;

Function FFClientConfigReadServerName: TffNetAddress;
Begin
  Result := ''; {!!.01}
  If OverrideServerName <> '' Then
    Begin
      Result := OverrideServerName;
      Exit;
    End;

  {$IFDEF UseRegistryConfig}
  Result := '';
  With TRegistry.Create Do
    Try
      RootKey := cfgRootKey;
      {$IFDEF DCC4OrLater}
      OpenKeyReadOnly(GetRegistryKey);
      {$ELSE}
      OpenKey(GetRegistryKey, True);
      {$ENDIF}
      If ValueExists(cfgServerName) Then
        Result := ReadString(cfgServerName);
    Finally
      Free;
    End;
  {$ENDIF}

  {$IFDEF UseINIConfig}
  With TINIFile.Create(GetINIFilename) Do
    Try
      Result := ReadString(cfgSection, cfgServerName, '');
    Finally
      Free;
    End;
  {$ENDIF}

  { if no name given, use compiled default name }
  If Result = '' Then
    Result := fsclServerName;
End;

Procedure FFClientConfigWriteProtocolName(aProtocolName: TffShStr);
Begin
  {$IFDEF UseRegistryConfig}
  With TRegistry.Create Do
    Try
      RootKey := cfgRootKey;
      OpenKey(GetRegistryKey, True);
      WriteString(cfgProtocol, aProtocolName);
    Finally
      Free;
    End;
  {$ENDIF}

  {$IFDEF UseINIConfig}
  With TINIFile.Create(GetINIFilename) Do
    Try
      WriteString(cfgSection, cfgProtocol, aProtocolName);
    Finally
      Free;
    End;
  {$ENDIF}
End;

Procedure FFClientConfigWriteProtocolClass(aProtocol: TfsCommsProtocolClass);
Begin
  FFClientConfigWriteProtocolName(FFClientConfigGetProtocolName(aProtocol));
End;

Procedure FFClientConfigWriteServerName(aServerName: TffNetAddress);
Begin
  {$IFDEF UseRegistryConfig}
  With TRegistry.Create Do
    Try
      RootKey := cfgRootKey;
      OpenKey(GetRegistryKey, True);
      WriteString(cfgServerName, aServerName);
    Finally
      Free;
    End;
  {$ENDIF}

  {$IFDEF UseINIConfig}
  With TINIFile.Create(GetINIFilename) Do
    Try
      WriteString(cfgSection, cfgServerName, aServerName);
    Finally
      Free;
    End;
  {$ENDIF}
End;

Initialization
  OverrideProtocol := Nil;
  OverrideServerName := '';

End.

