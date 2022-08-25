object DDService1: TDDService1
  OldCreateOrder = False
  OnCreate = DDServiceCreate
  AllowPause = False
  AllowedExControls = [alSessionChange]
  DisplayName = 'DDServiceStartProc'
  ServiceName = 'DDServiceStartProc'
  Description = 'Starts notepad in logged on user sessions'
  StartType = stManual
  AfterInstall = DDServiceAfterInstall
  AfterUninstall = DDServiceAfterUninstall
  OnStart = DDServiceStart
  OnSessionChange = DDServiceSessionChange
  Height = 150
  Width = 215
end
