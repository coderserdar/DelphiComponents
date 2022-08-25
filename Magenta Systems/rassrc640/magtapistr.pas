unit magtapistr;

// Magenta TAPI Functions
// Copyright 2011 by Angus Robertson, Magenta Systems Ltd, England
// delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/

interface

resourcestring

    STapiCsIdle = 'Call Idle' ;
    STapiCsOffering = 'Incoming Call Offered';
    STapiCsAccepted = 'Incoming Call Accepted';
    STapiCsDialtone = 'Dial tone';
    STapiCsDialing = 'Connecting/Dialling' ;
    STapiCsRingback = 'Ringback' ;
    STapiCsBusy = 'Busy';
    STapiCsSpecialInfo = 'Failed';
    STapiCsConnected = 'Connected/Answered' ;
    STapiCsProceeding = 'Waiting for Answer' ;
    STapiCsOnHold = 'On Hold';
    STapiCsConferenced = 'Conferenced';
    STapiCsOnHoldPendConf = 'On Hold Pending Conference';
    STapiCsOnHoldPendTransfer = 'On Hold Pending Transfer';
    STapiCsDisconnected = 'Disconnected' ;
    STapiCsUnknown = 'Unknown TAPI Event' ;

    STapiDmNormal = 'Normal Disconnection' ;
    STapiDmUnknown = 'Abnormal Disconnection' ;
    STapiDmReject = 'Call Rejected' ;
    STapiDmPickup = 'Call Pickup Elsewhere' ;
    STapiDmForwarded = 'Call Forwarded' ;
    STapiDmBusy = 'Busy' ;
    STapiDmNoAnswer = 'No Answer' ;
    STapiDmNoDialTone = 'No Dial Tone' ;
    STapiDmBadAddress = 'Invalid Address' ;
    STapiDmUureachable = 'Unreachable' ;
    STapiDmConjestion = 'Network Congestion' ;
    STapiDmImcompatible = 'Incompatible Equipment' ;
    STapiDmUnavail = 'No Disconnection Reason' ;

    STapiGenCIDiag = 'TAPI Line_CallInfo, device=%d handle=%d ' +
                        'Parms=%xh State=0%xh Detail=0%xh State=%s' ;

    STapiGenCSDiag = 'TAPI Line_CallState, device=%d handle=%d ' +
                        'State=0%xh Detail=0%xh Priv=%d App=%s Speed=%s State=%s' ;

    STapiGenDevDiag = 'Device %d, TAPI Name=%s'#13#10'Display Name=%s'#13#10 +
                    '  devtype=%s, provider=%s '#13#10'  classname=%s'#13#10 +
                    '  media=0%xh on %s (port %d) API=%xh'#13#10'  key=%s' ;  // 12 June 2008 added (port x)

    STapiGenCallEnded = 'Call Ended' ;
    STapiGenInitFail = 'TAPI Initialise Failed' ;
    STapiGenNoModems = 'TAPI No Modems' ;
    STapiGenModemTot = 'TAPI Modems Total %d' ;
    STapiGenFailedAPI = 'Device %d Failed API Negotiation: %s (%x)' ; // 12 June 2008
    STapiGenUnnamed = '(Unnamed Device)' ;
    STapiGenFailedOpen = 'Device %d Failed to Open' ;
    STapiGenNone = '(None)' ;
    STapiGenModemListed = 'TAPI Modems Listed %d' ;
    STapiGenVPNIgnored = ', VPN Modems Ignored %d' ;
    STapiGenMonFailed = 'Device %d Failed to Monitor' ;
    STapiGenMonStart = 'Device %d Started Monitoring, Line %d' ;
    STapiGenFailedNew = 'Device %d Failed Check For New Calls' ;
    STapiGenFailedGetCaps = 'Device %d Failed Get Capabilities: %s (%x)' ; // 12 June 2008


implementation

end.
