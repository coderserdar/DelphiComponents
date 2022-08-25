object Form1: TForm1
  Left = 7
  Top = 10
  Caption = 'Magenta Test WMI Subroutines - Version 5.6 - 26th November 2018'
  ClientHeight = 673
  ClientWidth = 992
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 992
    Height = 161
    Align = alTop
    TabOrder = 0
    object Label4: TLabel
      Left = 10
      Top = 0
      Width = 45
      Height = 13
      Caption = 'Computer'
    end
    object Label7: TLabel
      Left = 11
      Top = 80
      Width = 53
      Height = 13
      Caption = 'User Name'
    end
    object Label5: TLabel
      Left = 10
      Top = 40
      Width = 57
      Height = 13
      Caption = 'Namespace'
    end
    object Label8: TLabel
      Left = 136
      Top = 80
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Label1: TLabel
      Left = 130
      Top = 5
      Width = 96
      Height = 65
      Caption = 
        'The List View is set from a dynamic array so values may be acces' +
        'sed programmatically'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 560
      Top = 40
      Width = 39
      Height = 13
      Caption = 'Property'
    end
    object Label6: TLabel
      Left = 855
      Top = 45
      Width = 35
      Height = 13
      Caption = 'IP Addr'
    end
    object Label10: TLabel
      Left = 856
      Top = 95
      Width = 42
      Height = 13
      Caption = 'Gateway'
    end
    object Label9: TLabel
      Left = 856
      Top = 70
      Width = 26
      Height = 13
      Caption = 'Mask'
    end
    object Label11: TLabel
      Left = 621
      Top = 70
      Width = 101
      Height = 13
      Caption = 'New Computer Name'
    end
    object edtClass: TComboBox
      Left = 130
      Top = 130
      Width = 511
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'Win32_OperatingSystem'
      Items.Strings = (
        'Win32_OperatingSystem'
        'Win32_Keyboard'
        'Win32_AutochkSetting'
        'Win32_DiskDrive'
        'Win32_DiskDrivePhysicalMedia'
        'Win32_PhysicalMedia'
        'Win32_LogicalDisk'
        'Win32_MappedLogicalDisk'
        'Win32_TapeDrive'
        'Win32_BaseBoard'
        'Win32_BIOS'
        'Win32_Bus'
        'Win32_DeviceSettings'
        'Win32_Fan'
        'Win32_IDEController'
        'Win32_MemoryDevice'
        'Win32_PhysicalMemory'
        'Win32_PNPDevice'
        'Win32_PortConnector'
        'Win32_Processor'
        'Win32_SCSIController '
        'Win32_SCSIControllerDevice'
        'Win32_SerialPort'
        'Win32_SerialPortConfiguration'
        'Win32_SerialPortSetting'
        'Win32_SoundDevice'
        'Win32_SystemBIOS'
        'Win32_USBController'
        'Win32_USBControllerDevice'
        'Win32_NetworkAdapter'
        'Win32_NetworkAdapterConfiguration'
        'Win32_NetworkAdapterSetting'
        'Win32_Printer'
        'Win32_PrinterConfiguration'
        'Win32_POTSModem'
        'Win32_POTSModemToSerialPort'
        'Win32_DesktopMonitor'
        'Win32_DisplayConfiguration'
        'Win32_VideoController'
        'Win32_VideoSettings'
        'Win32_ActiveRoute'
        'Win32_IP4PersistedRouteTable'
        'Win32_IP4RouteTable'
        'Win32_NetworkClient'
        'Win32_NetworkConnection'
        'Win32_BootConfiguration'
        'Win32_ComputerSystem'
        'Win32_MotherboardDevice'
        'Win32_SystemUsers'
        'Win32_SystemTimeZone'
        'Win32_SystemSetting'
        'Win32_Account'
        'Win32_UserAccount'
        'Win32_SystemAccount'
        'Win32_Group'
        'Win32_LogonSession'
        'Win32_ComputerSystemWindowsProductActivation'
        'Win32_WindowsProductActivation'
        'Win32_FontIntoAction'
        'Win32_ScheduledJob'
        'Win32_Process'
        'Win32_ServiceControl'
        'Win32_Product'
        'Win32_TemperatureProbe '
        'Win32_StartupCommand'
        'Win32_BaseService'
        'Win32_Service'
        'Win32_Desktop'
        'Win32_Environment'
        'Win32_LogicalProgramGroup'
        'Win32_ProgramGroup'
        'Win32_UninterruptiblePowerSupply'
        'Win32_Battery'
        'Win32_PortableBattery'
        'Win32_CurrentProbe'
        'Win32_VoltageProbe'
        'Win32_PerfFormattedData_RemoteAccess_RASTotal'
        'Win32_PerfFormattedData_RemoteAccess_RASPort'
        'Win32_PerfRawData_RemoteAccess_RASPort'
        'SELECT * FROM Win32_OperatingSystem'
        
          'SELECT * FROM Win32_NetworkAdapter WHERE ConfigManagerErrorCode ' +
          '= 0'
        
          'SELECT Name, MACAddress, ConfigManagerErrorCode, NetConnectionID' +
          '  FROM Win32_NetworkAdapter WHERE AdapterType = '#39'Ethernet 802.3'#39
        
          'SELECT Name, Model, InterfaceType, MediaType, Size from Win32_Di' +
          'skDrive'
        
          'SELECT Name, Description, DriveType, FileSystem, FreeSpace, Size' +
          ', VolumeSerialNumber from Win32_LogicalDisk'
        
          'SELECT Description, IPAddress, IPSubnet, IPConnectionMetric, MAC' +
          'Address, DefaultIPGateway FROM Win32_NetworkAdapterConfiguration' +
          ' WHERE DefaultTTL > 1'
        '(following are in Namespace root\wmi)'
        'MSNdis_HardwareStatus'
        'MSNdis_80211_TransmitPowerLevel'
        'MSNdis_80211_ReceivedSignalStrength'
        'MSNdis_MediaConnectStatus'
        'MSTapeDriveParam'
        'MSRedbook_DriverInformation'
        'MSSerial_PortName'
        'MSStorageDriver_FailurePredictStatus'
        'MSStorageDriver_ATAPISmartData')
    end
    object edtComputer: TEdit
      Left = 10
      Top = 15
      Width = 113
      Height = 21
      TabOrder = 1
      Text = '.'
    end
    object edtUser: TEdit
      Left = 11
      Top = 100
      Width = 113
      Height = 21
      TabOrder = 2
    end
    object edtPass: TEdit
      Left = 136
      Top = 100
      Width = 113
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
    object doGetClass: TButton
      Left = 10
      Top = 130
      Width = 106
      Height = 25
      Caption = 'Get Class or Query'
      TabOrder = 4
      OnClick = doGetClassClick
    end
    object doExit: TButton
      Left = 905
      Top = 125
      Width = 75
      Height = 25
      Caption = 'Exit'
      TabOrder = 5
      OnClick = doExitClick
    end
    object doMB: TButton
      Left = 385
      Top = 35
      Width = 75
      Height = 25
      Caption = 'Motherboard'
      TabOrder = 6
      OnClick = doMBClick
    end
    object ResInfo: TEdit
      Left = 280
      Top = 100
      Width = 441
      Height = 21
      Color = 10930928
      ReadOnly = True
      TabOrder = 7
    end
    object doBIOS: TButton
      Left = 385
      Top = 5
      Width = 75
      Height = 25
      Caption = 'BIOS'
      TabOrder = 8
      OnClick = doBIOSClick
    end
    object doDiskModel: TButton
      Left = 465
      Top = 35
      Width = 86
      Height = 25
      Caption = 'Disk Model'
      TabOrder = 9
      OnClick = doDiskModelClick
    end
    object DiskNum: TSpinEdit
      Left = 560
      Top = 65
      Width = 51
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 10
      Value = 0
    end
    object doDiskSerial: TButton
      Left = 465
      Top = 5
      Width = 86
      Height = 25
      Caption = 'Disk Serial (xp)'
      TabOrder = 11
      OnClick = doDiskSerialClick
    end
    object doBootTime: TButton
      Left = 315
      Top = 5
      Width = 65
      Height = 25
      Caption = 'Boot Time'
      TabOrder = 12
      OnClick = doBootTimeClick
    end
    object doCommand: TButton
      Left = 770
      Top = 35
      Width = 75
      Height = 25
      Caption = 'Command'
      TabOrder = 13
      OnClick = doCommandClick
    end
    object OneCommand: TComboBox
      Left = 560
      Top = 10
      Width = 421
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 15
      Text = 'SELECT * FROM Win32_LogicalDisk WHERE Name = "C:"'
      Items.Strings = (
        'SELECT * FROM Win32_LogicalDisk WHERE Name = "C:"'
        'SELECT * FROM Win32_OperatingSystem'
        
          'SELECT * FROM Win32_DiskDrive WHERE Name ="\\\\.\\PHYSICALDRIVE0' +
          '"'
        
          'SELECT * FROM Win32_NetworkAdapter WHERE Name = "RAS Async Adapt' +
          'er"'
        
          'SELECT * FROM Win32_NetworkAdapter  WHERE Name = "ASUSTeK/Broadc' +
          'om 440x 10/100 Integrated Controller"'
        
          'SELECT * FROM Win32_NetworkAdapter  WHERE Name = "ORiNOCO PC Car' +
          'd (5V)'
        
          'SELECT * FROM Win32_ServiceControl  WHERE Name = "DUN Manager Se' +
          'rvice"'
        '')
    end
    object OneProp: TComboBox
      Left = 610
      Top = 35
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 14
      Text = 'VolumeName'
      Items.Strings = (
        'VolumeName'
        'Caption'
        'Name'
        'MACAddress'
        'ConfigManagerErrorCode'
        'AdapterType'
        'Handle'
        'Size')
    end
    object doIPAddr: TButton
      Left = 750
      Top = 95
      Width = 101
      Height = 25
      Caption = 'Change IP Addr'
      TabOrder = 16
      OnClick = doIPAddrClick
    end
    object SubNetMask: TEdit
      Left = 900
      Top = 65
      Width = 86
      Height = 21
      TabOrder = 17
      Text = '255.255.255.0'
    end
    object IPAddress: TEdit
      Left = 900
      Top = 40
      Width = 86
      Height = 21
      TabOrder = 18
      Text = '192.168.1.119'
    end
    object IPGateway: TEdit
      Left = 900
      Top = 90
      Width = 86
      Height = 21
      TabOrder = 19
      Text = '192.168.1.4'
    end
    object edtNamespace: TComboBox
      Left = 10
      Top = 55
      Width = 116
      Height = 21
      ItemHeight = 13
      TabOrder = 20
      Text = 'root\CIMV2'
      Items.Strings = (
        'root\CIMV2'
        'root\WMI'
        'root\Virtualization'
        'root\Hardware')
    end
    object doRenameComp: TButton
      Left = 750
      Top = 125
      Width = 101
      Height = 25
      Caption = 'Rename Computer'
      TabOrder = 21
      OnClick = doRenameCompClick
    end
    object NewCompName: TEdit
      Left = 730
      Top = 65
      Width = 116
      Height = 21
      TabOrder = 22
    end
    object doSmart: TButton
      Left = 465
      Top = 65
      Width = 86
      Height = 25
      Caption = 'Disk SMART'
      TabOrder = 23
      OnClick = doSmartClick
    end
    object doReboot: TButton
      Left = 235
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Reboot PC'
      TabOrder = 24
      OnClick = doRebootClick
    end
    object doCloseDown: TButton
      Left = 235
      Top = 35
      Width = 76
      Height = 25
      Caption = 'Power Down'
      TabOrder = 25
      OnClick = doCloseDownClick
    end
    object doMemory: TButton
      Left = 315
      Top = 35
      Width = 65
      Height = 25
      Caption = 'Memory'
      TabOrder = 26
      OnClick = doMemoryClick
    end
    object doMapDrives: TButton
      Left = 386
      Top = 65
      Width = 75
      Height = 25
      Caption = 'Map Drives'
      TabOrder = 27
      OnClick = doMapDrivesClick
    end
    object doMapSCSI: TButton
      Left = 307
      Top = 66
      Width = 75
      Height = 25
      Caption = 'Map SCSI'
      TabOrder = 28
      OnClick = doMapSCSIClick
    end
  end
  object ListView: TListView
    Left = 0
    Top = 161
    Width = 992
    Height = 493
    Align = alClient
    Columns = <
      item
        Width = 120
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end>
    ColumnClick = False
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 654
    Width = 992
    Height = 19
    Panels = <>
    SimplePanel = True
  end
end
