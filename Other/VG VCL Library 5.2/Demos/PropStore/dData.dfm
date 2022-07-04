object dm: Tdm
  OldCreateOrder = False
  Left = 141
  Top = 182
  Height = 213
  Width = 448
  object psData: TPropStorage
    AppIniFile = afIniFile
    AppSection = 'MainForm'
    StoredProps.Strings = (
      'enShell.Folder')
    StoredValues = <>
    Left = 86
    Top = 11
  end
  object afIniFile: TAppIniFile
    IniFileName = 'PropStore'
    Left = 19
    Top = 12
  end
  object ExplorerRootNode1: TExplorerRootNode
    Text = 'ExplorerRootNode1'
    Left = 20
    Top = 68
    object enShell: TExplorerShellFolderNode
    end
  end
end
