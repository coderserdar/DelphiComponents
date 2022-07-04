{
 This is a demo for using the GT Delphi Components Setting Controls
 For any comment suggestion please feel free to contact at info@gtdelphicomponents.tk.

 It is a pretty simple example you associate the controls with the SettingManager.
 You choose at the SettingManager the StorageOption for the setting INI,Registry or a DataSet.

}

unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SettingCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    gtSettingsManager1: TgtSettingsManager;
    FileNamePath: TgtCSCEdit;
    btnSave: TButton;
    btnLoad: TButton;
    btnClear: TButton;
    Label1: TLabel;
    IsActive: TgtCSCCheckBox;
    DataBaseType: TgtCSCComboBox;
    TableList: TgtCSCListBox;
    Label2: TLabel;
    FreeText: TgtCSCMemo;
    Label3: TLabel;
    gtCSCDateTimePicker1: TgtCSCDateTimePicker;
    gtCSCTrackBar1: TgtCSCTrackBar;
    gtCSCRadioGroup1: TgtCSCRadioGroup;
    gtCSCColorBox1: TgtCSCColorBox;
    gtCSCLabeledEdit1: TgtCSCLabeledEdit;
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  gtSettingsManager1.SaveSettings;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  gtSettingsManager1.LoadSettings;
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  gtSettingsManager1.ClearControlValues;
end;

end.
