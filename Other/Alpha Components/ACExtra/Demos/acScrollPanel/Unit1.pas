unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, sButton, sEdit, ExtCtrls, sSkinProvider,
  sSkinManager, Buttons, sBitBtn, sLabel, sCheckBox,
  sSpeedButton, sPanel, sScrollBox, sFrameBar, ComCtrls, sTrackBar,
  sScrollBar, ImgList, sMemo, acProgressBar, sGroupBox,
  sGauge, aceScrollPanel;

type
  TForm1 = class(TForm)
    sSkinManager1: TsSkinManager;
    sSkinProvider1: TsSkinProvider;
    sCheckBox1: TsCheckBox;
    ImageList1: TImageList;
    acScrollPanel1: TacScrollPanel;
    acScrollPanelBand6: TacScrollPanelBand;
    sScrollMaxBand2: TacScrollPanelBand;
    sButton2: TsButton;
    sEdit2: TsEdit;
    sMemo1: TsMemo;
    sGroupBox1: TsGroupBox;
    sButton3: TsButton;
    sButton4: TsButton;
    sSpeedButton1: TsSpeedButton;
    sSpeedButton2: TsSpeedButton;
    sBitBtn2: TsBitBtn;
    sBitBtn3: TsBitBtn;
    sRadioGroup1: TsRadioGroup;
    sTrackBar1: TsTrackBar;
    sProgressBar1: TsProgressBar;
    sButton5: TsButton;
    sButton6: TsButton;
    sButton7: TsButton;
    acScrollPanelBand1: TacScrollPanelBand;
    sEdit1: TsEdit;
    sEdit3: TsEdit;
    sEdit4: TsEdit;
    sEdit5: TsEdit;
    sButton8: TsButton;
    sButton9: TsButton;
    acScrollPanelBand2: TacScrollPanelBand;
    acScrollPanelBand3: TacScrollPanelBand;
    acScrollPanelBand4: TacScrollPanelBand;
    acScrollPanelBand5: TacScrollPanelBand;
    sGauge1: TsGauge;
    sPanel1: TsPanel;
    sCheckBox2: TsCheckBox;
    sCheckBox3: TsCheckBox;
    procedure sCheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.sCheckBox1Click(Sender: TObject);
begin
  sSkinManager1.Active := sCheckBox1.Checked;
end;

end.
