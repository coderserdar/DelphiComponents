object frmWM: TfrmWM
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <
    item
      Name = 'wHomePage'
      PathInfo = '/'
      OnAction = wHomePageAction
    end
    item
      MethodType = mtGet
      Name = 'wSetLang'
      PathInfo = '/setlang'
      OnAction = wSetLangAction
    end>
  Height = 150
  Width = 215
end
