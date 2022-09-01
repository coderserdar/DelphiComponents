inherited bvDBGridDemoForm3: TbvDBGridDemoForm3
  Caption = 'bvDBGridDemoForm3'
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    inherited bvDBGrid1: TbvDBGrid
      Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgTabs, dgConfirmDelete, dgCancelOnExit]
      StrippedRows = 2
      Enter2Tab = True
    end
  end
end
