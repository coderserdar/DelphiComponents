unit MagFireWall;
{
Updated by Angus Robertson, Magenta Systems Ltd, England, 1st February 2022 
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Magenta Firewall Component has functions to search and list selected Windows
Defender Firewall rules and settings, and to add and remove such rules.

Currently, only rules enabling inbound access for an application with all
protocols, addresses and ports are added, adding more precise rules needs
more parameters to be passed and handled.

WARNING - this unit used COM Automation, so the application should call
CoInitialize(nil); before calling these functions, and CoUninitialize; after,
however Windows 10 seems to work without these...

There is also some code that may be used in Inno Setup scripts to set-up
firewall rules during application installation.


Win XP2 version
Windows Firewall - old interfaces, INetFwPolicy, INetFwMgr, INetFwOpenPorts,
INetFwAuthorizedApplication, INetFwService

https://docs.microsoft.com/en-gb/previous-versions/windows/desktop/ics/using-windows-firewall

Vista and later
Windows Firewall with Advanced Security - new interfaces INetFwPolicy2,
INetFwRules, INetFwProducts, INetFwServiceRestriction

https://docs.microsoft.com/en-gb/previous-versions/windows/desktop/ics/windows-firewall-with-advanced-security

The advanced version uses rules and groups, rather than applications and ports.



}

interface

uses Windows, Sysutils, ActiveX, ComObj, Variants;

Const
  NET_FW_PROFILE_DOMAIN = 0;
  NET_FW_PROFILE_STANDARD = 1;
  NET_FW_PROFILE_CURRENT  = 2;

  NET_FW_IP_PROTOCOL_UDP = 17;
  NET_FW_IP_PROTOCOL_TCP = 6;
  NET_FW_IP_PROTOCOL_ICMPv4 = 1;
  NET_FW_IP_PROTOCOL_ICMPv6 = 58;
  NET_FW_IP_PROTOCOL_ANY = 256;

  NET_FW_IP_VERSION_V4 = 0;
  NET_FW_IP_VERSION_V6 = 1;
  NET_FW_IP_VERSION_ANY = 2;

  NET_FW_SCOPE_ALL = 0;
  NET_FW_SCOPE_LOCAL_SUBNET = 1;
  NET_FW_SCOPE_CUSTOM = 2;

  NET_FW_SERVICE_FILE_AND_PRINT = 0;
  NET_FW_SERVICE_UPNP = 1;
  NET_FW_SERVICE_REMOTE_DESKTOP = 2;
  NET_FW_SERVICE_NONE = 3;

  NET_FW_PROFILE2_DOMAIN  = 1;
  NET_FW_PROFILE2_PRIVATE = 2;
  NET_FW_PROFILE2_PUBLIC  = 4;
  NET_FW_PROFILE2_ALL     = $7fffffff;

  NET_FW_RULE_DIR_IN = 1;
  NET_FW_RULE_DIR_OUT = 2;

  NET_FW_ACTION_BLOCK = 0;
  NET_FW_ACTION_ALLOW = 1;

  NET_FW_MODIFY_STATE_OK = 0;
  NET_FW_MODIFY_STATE_GP_OVERRIDE = 1;
  NET_FW_MODIFY_STATE_INBOUND_BLOCKED = 2;

  NET_FW_EDGE_TRAVERSAL_TYPE_DENY = 0;
  NET_FW_EDGE_TRAVERSAL_TYPE_ALLOW = 1;
  NET_FW_EDGE_TRAVERSAL_TYPE_DEFER_TO_APP = 2;
  NET_FW_EDGE_TRAVERSAL_TYPE_DEFER_TO_USER = 3;

  NET_FW_RULE_CATEGORY_BOOT = 0;
  NET_FW_RULE_CATEGORY_STEALTH = 1;
  NET_FW_RULE_CATEGORY_FIREWALL	= 2;
  NET_FW_RULE_CATEGORY_CONSEC = 3;

type
  TFirewallDir = (FirewallNone, FirewallIn, FirewallOut, FirewallBoth);

// the new methods
function MagFireWallRulesEnum(const Search: String): String;
function MagFireWallRulesAdd(const EntryName, GroupName, Descr,
                AppPathAndExe: string; Direction: TFirewallDir = FirewallNone): String;

// old methods for old rules
function MagFireWallAppsEnum(const Search: String;
                            Profile: Integer = NET_FW_PROFILE_CURRENT): String;
function MagFireWallServEnum(const Search: String;
                            Profile: Integer = NET_FW_PROFILE_CURRENT): String;

implementation


// AuthorizedApplications is the old way of setting rules from XP2 !!!

function MagFireWallAppsEnum(const Search: String;
                            Profile: Integer = NET_FW_PROFILE_CURRENT): String;
var
    SearchLC: String;
    objShell: OleVariant;
    fwMgr: OleVariant;
    AppObj, AppsObj: OleVariant;
    ProfileObj: OleVariant;
    AppsEnum: IEnumvariant;
    Value: LongWord;

    procedure AddRow(const S: String);
    begin
        Result := Result + S + #13#10;
    end;

begin
    try
        Result := '';
        objShell := CreateOLEObject ('Shell.Application');
        If NOT objShell.IsServiceRunning ('mpssvc') then
        begin
            VarClear (objShell);
            Result := 'Windows Firewall Not Running';
            Exit;
        end;
        VarClear (objShell);

      // Create the firewall manager object.
        fwMgr := CreateOLEObject ('HNetCfg.FwMgr');

    // Get the profile for the local firewall policy.
        ProfileObj := fwMgr.LocalPolicy.GetProfileByType(Profile);

    // get list of applications authorised
        AppsObj := ProfileObj.AuthorizedApplications;
        AppsEnum := IUnknown(AppsObj._NewEnum) as IEnumVariant;
        SearchLC := Lowercase(Search);
        while (AppsEnum.Next(1, AppObj, Value) = 0) do
        begin
            if (SearchLC = 'all') or (Pos(SearchLC, Lowercase(AppObj.Name)) > 0)  or
                     (Pos(SearchLc, Lowercase(AppObj.ProcessImageFileName)) > 0)  then
            begin
                AddRow('  Application Name:   ' + AppObj.Name);
                AddRow('  Application Path:   ' + AppObj.ProcessImageFileName);

                Case AppObj.IpVersion of
                   NET_FW_IP_VERSION_V4      : AddRow('  IP Version:         IPv4');
                   NET_FW_IP_VERSION_V6      : AddRow('  IP Version:         IPv6');
                   NET_FW_IP_VERSION_ANY     : AddRow('  IP Version:         Any');
                End;

                Case AppObj.Scope of
                   NET_FW_SCOPE_ALL          : AddRow('  Scope:              All');
                   NET_FW_SCOPE_LOCAL_SUBNET : AddRow('  Scope:              Local Subnet');
                   NET_FW_SCOPE_CUSTOM       : AddRow('  Scope:              Custom');
                End;

                AddRow('  RemoteAddresses:    ' + AppObj.RemoteAddresses);
                AddRow('  Application Enabled: ' + VarToStr(AppObj.Enabled));

                AddRow('----------------------------------------------');
                AddRow('');
            end;
            AppObj := Unassigned;
        end;
    except
        on E:EOleException do
            Result := Result + 'EOleException: ' + E.Message;
        on E:Exception do
            Result := Result + E.Classname + ': ' + E.Message;
    end;

  // cleanup
    VarClear (AppObj) ;
    VarClear (AppsObj) ;
    VarClear (ProfileObj) ;
    VarClear (fwMgr) ;
end;


function MagFireWallServEnum(const Search: String;
                            Profile: Integer = NET_FW_PROFILE_CURRENT): String;
var
    SearchLC: String;
    objShell: OleVariant;
    fwMgr: OleVariant;
    ServObj, ServsObj, GlobOpPortsObj, PortObj: OleVariant;
    ProfileObj: OleVariant;
    ServsEnum, GlobOpPortsEnum: IEnumvariant;
    Value, ValueG: LongWord;

    procedure AddRow(const S: String);
    begin
        Result := Result + S + #13#10;
    end;

begin
    try
        Result := '';
        objShell := CreateOLEObject ('Shell.Application');
        If NOT objShell.IsServiceRunning ('mpssvc') then
        begin
            VarClear (objShell);
            Result := 'Windows Firewall Not Running';
            Exit;
        end;
        VarClear (objShell);

      // Create the firewall manager object.
        fwMgr := CreateOLEObject ('HNetCfg.FwMgr');

    // Get the profile for the local firewall policy.
        ProfileObj := fwMgr.LocalPolicy.GetProfileByType(Profile);

    // get list of Services
        ServsObj := ProfileObj.Services;
        ServsEnum := IUnknown(ServsObj._NewEnum) as IEnumVariant;
        SearchLC := Lowercase(Search);
        while (ServsEnum.Next(1, ServObj, Value) = 0) do
        begin
            if (SearchLC = 'all') or (Pos(SearchLC, Lowercase(ServObj.Name)) > 0)  then
            begin
                AddRow('  Service Name:       ' + ServObj.Name);

                Case ServObj.Type of
                    NET_FW_SERVICE_FILE_AND_PRINT  : AddRow('  Type:               File and Print');
                    NET_FW_SERVICE_UPNP            : AddRow('  Type:               uPNP');
                    NET_FW_SERVICE_REMOTE_DESKTOP  : AddRow('  Type:               Remote Desktop');
                    NET_FW_SERVICE_NONE            : AddRow('  Type:               None');
                end;

                Case ServObj.IpVersion of
                   NET_FW_IP_VERSION_V4      : AddRow('  IP Version:         IPv4');
                   NET_FW_IP_VERSION_V6      : AddRow('  IP Version:         IPv6');
                   NET_FW_IP_VERSION_ANY     : AddRow('  IP Version:         Any');
                End;

                Case ServObj.Scope of
                   NET_FW_SCOPE_ALL          : AddRow('  Scope:              All');
                   NET_FW_SCOPE_LOCAL_SUBNET : AddRow('  Scope:              Local Subnet');
                   NET_FW_SCOPE_CUSTOM       : AddRow('  Scope:              Custom');
                End;

                AddRow('  RemoteAddresses:    ' + ServObj.RemoteAddresses);
                AddRow('  Service Enabled:    ' + VarToStr(ServObj.Enabled));
                AddRow('  Customized:         ' + VarToStr(ServObj.Customized));

                GlobOpPortsObj := ProfileObj.GloballyOpenPorts;
                GlobOpPortsEnum := IUnknown(GlobOpPortsObj._NewEnum) as IEnumVariant;
                ValueG := 0;
                while (GlobOpPortsEnum.Next(1, PortObj, ValueG) = 0) do
                begin
                    AddRow('  Port name:              ' + PortObj.Name);
                    AddRow('  Port number:            ' + VarToStr(PortObj.Port));
                    AddRow('  Port enabled:           ' + VarToStr(PortObj.Enabled));
                    AddRow('  Port built-in:          ' + VarToStr(PortObj.BuiltIn));
                    AddRow('  Port IP version:        ' + VarToStr(PortObj.IPVersion));
                    AddRow('  Port protocol:          ' + VarToStr(PortObj.Protocol));
                    AddRow('  Port remote addresses:  ' + PortObj.RemoteAddresses);
                    AddRow('  Port scope:             ' + VarToStr(PortObj.Scope));
                    PortObj := Unassigned;
                end;

                AddRow('----------------------------------------------');
                AddRow('');
            end;
            ServObj := Unassigned;
        end;
    except
        on E:EOleException do
            Result := Result + 'EOleException: ' + E.Message;
        on E:Exception do
            Result := Result + E.Classname + ': ' + E.Message;
    end;

  // cleanup
    VarClear (PortObj) ;
    VarClear (ServObj) ;
    VarClear (GlobOpPortsObj) ;
    VarClear (ServsObj) ;
    VarClear (ProfileObj) ;
    VarClear (fwMgr) ;
end;

// list all advanced firewall rules, that partially match Search, in name, group or file
// blank search just returns fileware settings

function MagFireWallRulesEnum(const Search: String): String;
var
    SearchLC, Profiles: String;
    objShell: OleVariant;
    CurrentProfiles: Integer;
    fwPolicy2: OleVariant;
    RulesObject, Rule: OleVariant;
    InterfacesArray, InterfaceName: OleVariant;
    RuleEnum, InterfaceEnum: IEnumvariant;
    Value, Value2: LongWord;
    ProfActive: Boolean;

    procedure AddRow(const S: String);
    begin
        Result := Result + S + #13#10;
    end;

    procedure ListSettings(Profile: Integer);
    begin
        if fwPolicy2.FirewallEnabled[Profile] then
            AddRow('Firewall is Enabled')
        else
            AddRow('Firewall is Disabled');

        if fwPolicy2.BlockAllInboundTraffic[Profile] then
            AddRow('Block All Inbound Traffic is Enabled')
        else
            AddRow('Block All Inbound Traffic is Disabled');

        if fwPolicy2.NotificationsDisabled[Profile] then
            AddRow('Notifications Disabled is Enabled')
        else
            AddRow('Notifications Disabled is Disabled');

        if fwPolicy2.DefaultInboundAction[Profile] then
            AddRow('Default Inbound Action is Allow')
        else
            AddRow('Default Inbound Action is Block');

        if fwPolicy2.DefaultOutboundAction[Profile] then
            AddRow('Default Outbound Action is Allow')
        else
            AddRow('Default Outbound Action is Block');
    end;


begin
    Result := '';
    try
      // check firewall service is running, else APIs fail
        objShell := CreateOLEObject ('Shell.Application');
        If NOT objShell.IsServiceRunning ('mpssvc') then
        begin
            VarClear (objShell);
            Result := 'Windows Firewall Service Not Running';
            Exit;
        end;
        VarClear (objShell);

      // Create the FwPolicy2 object.
        fwPolicy2 := CreateOleObject('HNetCfg.FwPolicy2');
        CurrentProfiles := fwPolicy2.CurrentProfileTypes;

        ProfActive := ((CurrentProfiles AND NET_FW_PROFILE2_DOMAIN)<>0);
        if ProfActive then AddRow('Domain Firewall Profile is active');
        if ProfActive or (Search <> '') then
        begin
            AddRow('Settings for Domain Profile:');
            ListSettings(NET_FW_PROFILE2_DOMAIN);
        end;

        ProfActive := ((CurrentProfiles AND NET_FW_PROFILE2_PRIVATE )<>0);
        if ProfActive then AddRow('Private Firewall Profile is active');
        if ProfActive or (Search <> '') then
        begin
            AddRow('Settings for Private Profile:');
            ListSettings(NET_FW_PROFILE2_PRIVATE);
        end;

        ProfActive := ((CurrentProfiles AND NET_FW_PROFILE2_PUBLIC )<>0);
        if ProfActive then AddRow('Public Firewall Profile is active');
        if ProfActive or (Search <> '') then
        begin
            AddRow('Settings for Public Profile:');
            ListSettings(NET_FW_PROFILE2_PUBLIC);
        end;

        if (Search <> '') then
        begin
            AddRow('Rules:');
            RulesObject := fwPolicy2.Rules;
            RuleEnum := IUnknown(Rulesobject._NewEnum) as IEnumVariant;
            SearchLC := Lowercase(Search);
            while (RuleEnum.Next(1, Rule, Value) = 0) do
            begin
                if (Pos(SearchLC, Lowercase(Rule.Grouping)) > 0) or
                      (Pos(SearchLC, Lowercase(Rule.Name)) > 0)  or
                        (Pos(SearchLc, Lowercase(Rule.ApplicationName)) > 0)  then
                begin
                    AddRow('Rule Group: ' + Rule.Grouping);
                    AddRow('Rule Name: ' + Rule.Name);
                    AddRow('Description: ' + Rule.Description);
                    AddRow('Application Name: ' + Rule.ApplicationName);
                    AddRow('Service Name: ' + Rule.ServiceName);

                    Case Rule.Protocol of
                       NET_FW_IP_PROTOCOL_TCP    : AddRow('IP Protocol: TCP');
                       NET_FW_IP_PROTOCOL_UDP    : AddRow('IP Protocol: UDP');
                       NET_FW_IP_PROTOCOL_ICMPv4 : AddRow('IP Protocol: UDP');
                       NET_FW_IP_PROTOCOL_ICMPv6 : AddRow('IP Protocol: UDP');
                       NET_FW_IP_PROTOCOL_ANY    : AddRow('IP Protocol: Any');
                    Else                           AddRow('IP Protocol: ' + VarToStr(Rule.Protocol));
                    End;

                    if (Rule.Protocol = NET_FW_IP_PROTOCOL_TCP) or (Rule.Protocol =
                       NET_FW_IP_PROTOCOL_UDP) or (Rule.Protocol = NET_FW_IP_PROTOCOL_ANY) then
                    begin
                      AddRow('Local Ports: ' + Rule.LocalPorts);
                      AddRow('Remote Ports: ' + Rule.RemotePorts);
                      AddRow('LocalAddresses: ' + Rule.LocalAddresses);
                      AddRow('RemoteAddresses: ' + Rule.RemoteAddresses);
                    end;

                    if (Rule.Protocol = NET_FW_IP_PROTOCOL_ICMPv4) or
                                             (Rule.Protocol = NET_FW_IP_PROTOCOL_ICMPv6) then
                      AddRow('ICMP Type and Code: ' + Rule.IcmpTypesAndCodes);

                    Case Rule.Direction of
                        NET_FW_RULE_DIR_IN :  AddRow('Direction: In');
                        NET_FW_RULE_DIR_OUT:  AddRow('Direction: Out');
                    End;

                    AddRow('Rule Enabled: ' + VarToStr(Rule.Enabled));
                    AddRow('Edge Traversal: ' + VarToStr(Rule.EdgeTraversal));

                    Profiles := '';
                    if (Rule.Profiles AND NET_FW_PROFILE2_DOMAIN)<>0 then Profiles := ' Domain';
                    if (Rule.Profiles AND NET_FW_PROFILE2_PRIVATE )<>0 then Profiles := Profiles + ' Private';
                    if (Rule.Profiles AND NET_FW_PROFILE2_PUBLIC )<>0 then Profiles := Profiles + ' Public';
                    AddRow('Profile:' + Profiles);

                    Case Rule.Action of
                       NET_FW_ACTION_ALLOW : AddRow('Rule Action: Allow');
                       NET_FW_ACTION_BLOCk : AddRow('Rule Action: Block');
                    End;

                    AddRow('Interface Types: ' + Rule.InterfaceTypes);
                    if Rule.InterfaceTypes <> 'All' then
                    begin
                        InterfacesArray := rule.Interfaces;
                        Value2 := 0;
                        InterfaceEnum := IUnknown(InterfacesArray._NewEnum) as IEnumVariant;
                        while (InterfaceEnum.Next(1, InterfaceName, Value2) = 0) do
                        begin
                            AddRow('Interface Name: ' + VarToStr(InterfaceName));
                        end;
                    end;

                    AddRow('----------------------------------------------');
                    AddRow('');
                end;
                Rule := Unassigned;
            end;
        end;
    except
        on E:EOleException do
            Result := Result + 'Firewall Error: ' + E.Message;
        on E:Exception do
            Result := Result + E.Classname + ': ' + E.Message;
    end;

  // cleanup
    VarClear (Rule) ;
    VarClear (RulesObject) ;
    VarClear (InterfacesArray) ;
    VarClear (InterfaceName) ;
    VarClear (fwPolicy2) ;
end;

// add or remove advanced firewall blank apppath is remove rule
// direction is FirewallIn or FirewallOut with in/out added to name,
// FirewallBoth for two rules, FirewallNone is in with simple name.

function MagFireWallRulesAdd(const EntryName, GroupName, Descr,
            AppPathAndExe: string; Direction: TFirewallDir = FirewallNone): String;
var
    objShell, fwPolicy2, RulesObject, Rule, NewRule: OleVariant;
//    InterfacesArray: OleVariant;

    function AddRule(const NewName, NewDescr: String; Dir: Integer): String;
    begin
    // see if modifying existing rule, need to delete old rule first
        Result := '';
        Rule := Unassigned;  // not found
        try
            if NOT VarIsEmpty(Rulesobject) then        // Dec 2020
                Rule := Rulesobject.Item(NewName);
        except
            Rule := Unassigned;  // not found
        end;
        if NOT VarIsClear(Rule) then
        begin
            if (Rule.Applicationname = AppPathAndExe) then
            begin
                Result := 'Firewall Rule Already Exists: ' + NewName +
                                                   ', Group: ' + Rule.Grouping;
                Exit;
            end;
            RulesObject.Remove(NewName);  // can not edit it
            if AppPathAndExe = '' then
            begin   // and no new one
                Result := 'Firewall Rule Removed: ' + NewName;
                Exit;
            end;
        end
        else if AppPathAndExe = '' then
        begin   // and no new one
            Result := 'Firewall Rule Not Found: ' + NewName;
            Exit;
        end;

     // Create new rule
        NewRule := CreateOleObject('HNetCfg.FWRule');
        NewRule.Name := NewName;
        NewRule.Description := NewDescr;
        NewRule.Applicationname := AppPathAndExe;
        NewRule.Protocol := NET_FW_IP_PROTOCOL_ANY;

    //  other fule options we could use for incoming rules
    //  NewRule.LocalPorts := 5000, 5001;
    //  NewRule.LocalAddresses := xxx

    //  other fule options we could use for outgoing rules
    //  NewRule.RemotePorts := 5000;
    //  NewRule.RemoteAddresses := LocalSubnet;

    //  NewRule.Interfacetypes := 'LAN';
    // InterfaceArray = Array("Local Area Connection")
    // NewRule.Interfaces := InterfaceArray

        NewRule.Direction := Dir;
        NewRule.Enabled := True;
        NewRule.Grouping := GroupName;
        NewRule.Profiles := NET_FW_PROFILE2_ALL; // or CurrentProfiles;
        NewRule.Action := NET_FW_ACTION_ALLOW;

    // add the new rule
        RulesObject.Add(NewRule);
        Result := 'Firewall Rule Added: ' + NewName + ' for: ' + AppPathAndExe;

    end;


begin
    Result := '';
    try
      // check firewall service is running, else APIs fail
        objShell := CreateOLEObject ('Shell.Application');
        If NOT objShell.IsServiceRunning ('mpssvc') then
        begin
            VarClear (objShell);
            Result := 'Windows Firewall Service Not Running';
            Exit;
        end;
        VarClear (objShell);

      // Create the FwPolicy2 object and get the list of rules
        fwPolicy2 := CreateOleObject('HNetCfg.FwPolicy2');
        RulesObject := fwPolicy2.Rules;
        case Direction of
            FirewallNone: Result := AddRule(EntryName, Descr, NET_FW_RULE_DIR_IN);
            FirewallIn:   Result := AddRule(EntryName + ' (All-In)', 'Inbound ' + Descr, NET_FW_RULE_DIR_IN);
            FirewallOut:  Result := AddRule(EntryName + ' (All-Out)', 'Outbound ' + Descr, NET_FW_RULE_DIR_OUT);
            FirewallBoth:
                begin
                          Result := AddRule(EntryName + ' (All-In)', 'Inbound ' + Descr, NET_FW_RULE_DIR_IN);
                          Result := Result + #13#10 +
                                    AddRule(EntryName + ' (All-Out)', 'Outbound ' + Descr, NET_FW_RULE_DIR_OUT);
                end;
        end;
    except
        on E:EOleException do
            Result := Result + 'Firewall Error: ' + E.Message;
        on E:Exception do
            Result := Result + E.Classname + ': ' + E.Message;
    end;

  // cleanup
    VarClear (NewRule) ;
    VarClear (Rule) ;
    VarClear (RulesObject) ;
 //   VarClear (InterfacesArray) ;
    VarClear (fwPolicy2) ;
end;

// following functions are for Inno Setup scripts, not Delphi!
{
const
  NET_FW_IP_VERSION_V4 = 0;
  NET_FW_IP_VERSION_V6 = 1;
  NET_FW_IP_VERSION_ANY = 2;

  NET_FW_SCOPE_ALL = 0;
  NET_FW_SCOPE_LOCAL_SUBNET = 1;
  NET_FW_SCOPE_CUSTOM = 2;

  NET_FW_PROFILE2_DOMAIN  = 1;
  NET_FW_PROFILE2_PRIVATE = 2;
  NET_FW_PROFILE2_PUBLIC  = 4;
  NET_FW_PROFILE2_ALL     = $7fffffff;

  NET_FW_RULE_DIR_IN = 1;
  NET_FW_RULE_DIR_OUT = 2;

  NET_FW_ACTION_BLOCK = 0;
  NET_FW_ACTION_ALLOW = 1;

// add or remove advanced firewall inbound rule, blank apppath is remove rule

procedure MagFireWallRulesAdd(const EntryName, GroupName, Descr, AppPathAndExe: string);
var
    fwPolicy2, RulesObject, Rule, NewRule: Variant;
begin
    try

      // Create the FwPolicy2 object and get the list of rules
        fwPolicy2 := CreateOleObject('HNetCfg.FwPolicy2');
        RulesObject := fwPolicy2.Rules;

    // see if modifying existing rule, need to delete old rule first
        try
            Rule := Rulesobject.Item(EntryName);
        except
            Rule := Unassigned;  // not found
        end;
        if NOT VarIsClear(Rule) then
        begin
            if (Rule.Applicationname = AppPathAndExe) then Exit;
            RulesObject.Remove(EntryName);  // can not edit it
            if AppPathAndExe = '' then Exit; // and no new one
        end
        else if AppPathAndExe = '' then Exit;

     // Create new rule
        NewRule := CreateOleObject('HNetCfg.FWRule');
        NewRule.Name := EntryName;
        NewRule.Description := Descr;
        NewRule.Applicationname := AppPathAndExe;
        NewRule.Protocol := NET_FW_IP_PROTOCOL_ANY;
        NewRule.Direction := NET_FW_RULE_DIR_IN;
        NewRule.Enabled := True;
        NewRule.Grouping := GroupName;
        NewRule.Profiles := NET_FW_PROFILE2_ALL;
        NewRule.Action := NET_FW_ACTION_ALLOW;

      // add the new rule
        RulesObject.Add(NewRule);
    except
    end;
end;





}

end.
