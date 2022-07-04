object WebModule1: TWebModule1
  OldCreateOrder = False
  Actions = <
    item
      Default = True
      Name = 'WebActionItem1'
      PathInfo = '/report1'
      OnAction = WebModule1WebActionItem1Action
    end
    item
      Name = 'WebActionItem2'
      PathInfo = '/report2'
      OnAction = WebModule1WebActionItem2Action
    end>
  Left = 530
  Top = 355
  Height = 150
  Width = 215
end
