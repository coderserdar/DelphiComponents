unit Main;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	PBPrinterSetupDialog, StdCtrls, ExtCtrls;

type
	TForm1 = class(TForm)
    ExecuteDialog: TButton;
		Save: TButton;
		SetupTypeGroup: TRadioGroup;
		AutoSave: TCheckBox;
		ForceInitialSetupValues: TCheckBox;
    PBPrinterSetupDialog1: TPBPrinterSetupDialog;
    Button1: TButton;
    Label1: TLabel;
		procedure ExecuteDialogClick(Sender: TObject);
		procedure SaveClick(Sender: TObject);
		procedure SetupTypeGroupClick(Sender: TObject);
		procedure SetRadioButtons;
    procedure FormShow(Sender: TObject);
    procedure AutoSaveClick(Sender: TObject);
		procedure ForceInitialSetupValuesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.ExecuteDialogClick(Sender: TObject);
begin
	if PBPrinterSetupDialog1.Execute then SetRadioButtons;
end;

procedure TForm1.SaveClick(Sender: TObject);
begin
	PBPrinterSetupDialog1.SaveSetup;
	SetRadioButtons;
end;

procedure TForm1.SetupTypeGroupClick(Sender: TObject);
begin
	if Form1.ActiveControl is TRadioButton then
	begin
		case SetupTypeGroup.ItemIndex of
			0: PBPrinterSetupDialog1.SetupType := stDefault;
			1: PBPrinterSetupDialog1.SetupType := stInitial;
			2:
			begin
				PBPrinterSetupDialog1.SetupType := stSaved;
				if PBPrinterSetupDialog1.SetupType <> stSaved then SetRadioButtons;
			end;
			else
			begin
				Application.MessageBox('stUser SetupType can not be selected.'
					+#10+'When the dialog has executed (and AutoSave is False)'+#10
					+'the SetUpType will be stUser.', 'Wrong SetupType.',
					MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);
				SetRadioButtons;
			end;
		end;
	end;
end;

procedure TForm1.SetRadioButtons;
begin
	ActiveControl := nil;
	if PBPrinterSetupDialog1.SetupType = stDefault then SetupTypeGroup.ItemIndex := 0
	else if PBPrinterSetupDialog1.SetupType = stInitial then SetupTypeGroup.ItemIndex := 1
	else if PBPrinterSetupDialog1.SetupType = stSaved then SetupTypeGroup.ItemIndex := 2
	else SetupTypeGroup.ItemIndex := 3;
	SetupTypeGroup.SetFocus;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
	Label1.Caption := '(Build using PBPrinterSetupDialog version: ' + PBPrinterSetupDialog1.Version + ')';
	AutoSave.Checked := PBPrinterSetupDialog1.AutoSave;
	ForceInitialSetupValues.Checked := PBPrinterSetupDialog1.ForceInitialSetupValues;
	SetRadioButtons;
end;

procedure TForm1.AutoSaveClick(Sender: TObject);
begin
	PBPrinterSetupDialog1.AutoSave := AutoSave.Checked;
	Save.Enabled := not AutoSave.Checked;
end;

procedure TForm1.ForceInitialSetupValuesClick(Sender: TObject);
begin
	PBPrinterSetupDialog1.ForceInitialSetupValues := ForceInitialSetupValues.Checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
// This code shows an example of use.
// First set the InitialSetupOptions and InitialSetupValues at designtime.
// Set SetupType to stInitial - AutoSave to True.
// At runtime - when the user wants to print - check that the options you have
// set are supported by the user's printer - if not, tell the user to change the
// values (by executing the dialog) and save the values so that next time everyting
// is okay and he/she can print without ever having to setup the printer again.

// I here check for the papersize !
var
	Ok : Boolean;
begin
	Ok := False;
	with PBPrinterSetupDialog1 do
	begin
		if ThisPrinterValues.PaperSize = DMPAPER_EXECUTIVE then Ok := True
		else if not FileExists(SetupFileName) then
		begin
			MessageDlg('Please set Papersize to ''Executive'' !', mtInformation, [mbOK],
				0);
			if Execute then Ok := True;
		end
		else
		begin
			LoadSetup;
			if SetupType = stSaved then Ok := True;
		end;
		if Ok then Form1.Print; // Do your printing !
	end;
	SetRadioButtons; //Sets the radiobuttons on this form. 
end;

end.

