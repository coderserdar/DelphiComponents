object DDService1: TDDService1
  OldCreateOrder = False
  AllowedExControls = [alParamChange, alNetBindChange, alSessionChange, alPreShutdown]
  DisplayName = 'DDServiceTest'
  ServiceName = 'DDServiceTest'
  Description = 'Test description'
  ExOptions = [eoForceServiceThreadWindow]
  FailureOptions.NonCrashFailures = True
  FailureActions = <
    item
      ActionType = faRestart
      Delay = 60000
    end
    item
      ActionType = faNone
      Delay = 0
    end
    item
      ActionType = faNone
      Delay = 0
    end>
  StartType = stAutoDelayed
  AfterInstall = DDServiceAfterInstall
  AfterUninstall = DDServiceAfterUninstall
  OnContinue = DDServiceContinue
  OnDeviceEvent = DDServiceDeviceEvent
  OnNetBindChange = DDServiceNetBindChange
  OnParamChange = DDServiceParamChange
  OnPause = DDServicePause
  OnPowerEvent = DDServicePowerEvent
  OnRunException = DDServiceRunException
  OnShutdown = DDServiceShutdown
  OnStart = DDServiceStart
  OnStop = DDServiceStop
  OnSessionChange = DDServiceSessionChange
  OnConsoleEvent = DDServiceConsoleEvent
  OnPreShutdown = DDServicePreShutdown
  Height = 150
  Width = 215
end
