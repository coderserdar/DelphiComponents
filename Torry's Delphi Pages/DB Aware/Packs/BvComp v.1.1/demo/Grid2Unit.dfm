inherited bvDBGridDemoForm2: TbvDBGridDemoForm2
  Caption = 'bvDBGridDemoForm2'
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    inherited bvDBGrid1: TbvDBGrid
      Color = clBtnFace
      Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgTabs, dgConfirmDelete, dgCancelOnExit]
      StrippedRows = 0
      CellHeights = 130
      Lines3dV = c3dOUTER
      Lines3dH = c3dOUTER
    end
  end
end
