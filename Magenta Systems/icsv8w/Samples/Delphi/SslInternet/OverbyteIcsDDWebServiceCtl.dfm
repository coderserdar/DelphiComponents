object DDWebServiceCtl: TDDWebServiceCtl
  OldCreateOrder = False
  OnCreate = DDServiceCreate
  AllowPause = False
  AllowedExControls = [alNetBindChange]
  DisplayName = 'Overbyte DDService Web Server'
  ServiceName = 'OverbyteDDWebService'
  Description = 'Overbyte DDService Web Server'
  BeforeInstall = DDServiceBeforeInstall
  AfterInstall = DDServiceAfterInstall
  BeforeUninstall = DDServiceBeforeUninstall
  OnExecute = DDServiceExecute
  OnNetBindChange = DDServiceNetBindChange
  OnPowerEvent = DDServicePowerEvent
  OnRunException = DDServiceRunException
  OnShutdown = DDServiceShutdown
  OnStart = DDServiceStart
  OnStop = DDServiceStop
  Height = 150
  Width = 215
end
