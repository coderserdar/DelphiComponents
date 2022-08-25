unit magrasstr;
{
DELPHI RAS COMPONENT - Literal Strings
(C) 2011 Magenta Systems Ltd

Updated by Angus Robertson, Magenta Systems Ltd, England
in 2011, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

23 January 2001, Release 4.51 - baseline
1st May 2001, Release 4.60 - added RAS Server stuff
26th June 2001, Release 4.61
26th May 2002, Release 4.80
26th July 2002 - Release 4.90
20th May 2007 - Release 5.20 - added SRasRENUser and SRasRENAllUser

}

interface

resourcestring

    SRasCsOpenPort = 'Opening Serial Port' ;
    SRasCsPortOpened = 'Serial Port Opened';
    SRasCsConnectDevice = 'Connecting/Dialling' ;
    SRasCsDeviceConnected = 'Connected/Answered' ;
    SRasCsAllDevicesConnected = 'Connected/Negotiation';
    SRasCsAuthenticate = 'Validating User and Password';
    SRasCsAuthNotify = 'Authentication Notification';
    SRasCsAuthCallBack = 'Authentication Call Back';
    SRasCsAuthProject = 'Projection Started';
    SRasCsAuthLinkSpeed = 'Calculating Link speed';
    SRasCsAuthAck = 'Authentication acknowledged';
    SRasCsReAuthenticate = 'Reauthenticating';
    SRasCsAuthenticated = 'Login Authenticated';
    SRasCsPrepareforCallBack = 'Preparing for Callback';
    SRasCsWaitForModemReset = 'Waiting for Modem Reset';
    SRasCsWaitForCallBack = 'Waiting for Callback';
    SRasCsProjected = 'Projection Completion';
    SRasCsStartAuthentication = 'Start Authentication';
    SRasCsCallbackComplete = 'Callback Complete';
    SRasCsLogonNetwork = 'Logon to Network';
    SRasCsSubEntryConnected = 'Extra Channel Connected';
    SRasCsSubEntryDisconnected = 'Extra Channel Disconnected';
    SRasCsInteractive = 'Interactive Terminal';
    SRasCsRetryAuthentication = 'Retry Authentication';
    SRasCsCallbackSetByCaller = 'Callback Set By Caller';
    SRasCsPasswordExpired = 'Password Expires';
    SRasCsInvokeEapUI = 'Paused for Authentication';
    SRasCsConnected = 'Connected/Online';
    SRasCsDisConnected = 'Disconnected/Offline';

    SRasGenNotAvailable = 'Not Available' ;
    SRasGenDUNInfo = 'Unknown DUN Version' ;
    SRasGenBadState = 'Unknown State (%d)' ;
    SRasGenDisconn = 'Disconnected' ;
    SRasGenNoMem = 'Not enough memory' ;
    SRasGenPending = 'Device Connecting/Dialling' ;

    SRasErrReadEntProp = 'Must Read Entry Properties First' ;
    SRasErrCanonNum = 'Must Specify Canonical Number' ;
    SRasErrNoDUA = 'Unable to Locate Win9x Dial-Up Adapter' ;
    SRasErrPerKey = 'Unable to Read Registry Key - %s/%s' ;
    SRasErrOpKey = 'Unable to Open Registry Key - %s' ;
    SRasErrNoPDH = 'Unable to Find Performance Data Helper Library (PDH.DLL)' ;
    SRasErrBadPDH = 'Unable to Initialise Performance Data Helper Library (PDH.DLL), ErrCode 0%x' ;
    SRasErrPerData = 'No Performance Data Returned for Object - %s' ;
    SRasErrPerBlock = 'No Performance Object Blocks Found for - %s' ;
    SRasErrPerCount = 'Unable to Find Counter for Object - %s' ;
    SRasErrNoRRas = 'RAS/Routing Service Not Running' ; 

    SRasModemOperational = 'Operational' ;
    SRasModemNotResponding = 'Not Responding' ;
    SRasModemHardwareFailure = 'Hardware Failure' ;
    SRasModemIncorrectResponse = 'Incorrect Response' ;
    SRasModemUnknown = 'Unknown' ;

    SRasPortNonOperational = 'Non Operational' ;
    SRasPortDisconnected = 'Disconnected' ;
    SRasPortCallingBack = 'Calling Back' ;
    SRasPortListening = 'Listening' ;
    SRasPortAuthenicating = 'Authenicating' ;
    SRasPortAuthenticated = 'Authenticated' ;
    SRasPortInitalising = 'Initalising' ;

    SRasRENUser = 'Current User' ;  // 5.20
    SRasRENAllUsers = 'All Users' ;

implementation

end.
