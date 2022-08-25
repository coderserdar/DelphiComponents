{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  New consts SvcMgr.pas
Creation:     2006
Version:      1.0
EMail:        arno.garrels@gmx.de
Support:      None
Legal issues: Copyright (C) 2006-2011 by Arno Garrels, Berlin

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.


History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit DDSvcConsts;

interface

resourcestring
//
//Service Run
//
  SParamChange                  = 'ParamChange';
  SNetBindAdd                   = 'NetBindAdd';
  SNetBindRemove                = 'NetBindRemove';
  SNetBindEnable                = 'NetBindEnable';
  SNetBindDisable               = 'NetBindDisable';
  SDeviceEvent                  = 'DeviceEvent';
  SHardwareProfileChange        = 'HardwareProfileChange';
  SPowerEvent                   = 'PowerEvent';
  SSessionChange                = 'SessionChange';
  SPreShutdown                  = 'PreShutdown';
//
//Service Install
//
  SServiceConfigError           = '%s on the attempt to set property "%s"';
  SDescription                  = 'Description';
  SRecoveryOptions              = 'Recovery Options';
  SNonCrashFailures             = 'Actions for stops with errors';
  SPreshutdownTimeout           = 'Preshutdown Timeout';
  SDelayedAutoStart             = 'Delayed Start';
  SServiceSidType               = 'Service SID Type';
  SRequiredPrivileges           = 'Required Privileges';
  SFailureReboot                = 'Reboot system in %d ms';
  SFailureRestart               = 'Restart service in %d ms';
  SFailureExecute               = 'Execute command in %d ms';
  SFailureNoAction              = 'No action';
  SFailureOpenScm               = 'Function OpenSCManager failed with error "%s"';
//
// Misc
//
  SInvalidServiceDescription    = 'Invalid service description. Maximum length allowed is 1024 chars';
  SFailureCreateWindow          = 'Service couldn''t create a hidden window';
  SInvalidServiceName           = '''''%s'''' is not a valid service name';
  
implementation

end.
