unit FormMain;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ExtCtrls, Menus, Gauges, ShellApi, APITool, mcGauges;

type
	TfrMain = class(TForm)
		Panel1: TPanel;
    GroupBox2: TGroupBox;
    lbSize: TLabel;
    lbFree: TLabel;
    lbUsed: TLabel;
		GroupBox1: TGroupBox;
    GroupBox6: TGroupBox;
    lbUserName: TLabel;
		lbCompName: TLabel;
    GroupBox7: TGroupBox;
    Timer1: TTimer;
		mmTools: TMainMenu;
    moFile: TMenuItem;
		foExit: TMenuItem;
		moTools: TMenuItem;
    ioCPU: TMenuItem;
    moHelp: TMenuItem;
    hoHelpTopics: TMenuItem;
    N1: TMenuItem;
    hoAbout: TMenuItem;
    moInfo: TMenuItem;
    toRunFile: TMenuItem;
    toCopyFile: TMenuItem;
    APITools1: TAPITools;
    lbACPInfo: TLabel;
		lbOEMCP: TLabel;
		lbKBLayout: TLabel;
    Label1: TLabel;
    cbxDrives: TComboBox;
		lbKBType: TLabel;
    lbKBKeys: TLabel;
    ioVersion: TMenuItem;
    ioRegionalSettings: TMenuItem;
    ioSystemMetrics: TMenuItem;
    ioEnvironmentVariables: TMenuItem;
		Panel2: TPanel;
    GroupBox3: TGroupBox;
		lbTotalPhys: TLabel;
    lbAvailPhys: TLabel;
    lbUsedPhys: TLabel;
    GroupBox4: TGroupBox;
    lbTotalPageFile: TLabel;
    lbAvailPageFile: TLabel;
    lbUsedPageFile: TLabel;
    GroupBox5: TGroupBox;
    lbTotalVirtual: TLabel;
    lbAvailVirtual: TLabel;
		lbUsedVirtual: TLabel;
    Panel3: TPanel;
    lbLabel: TLabel;
    lbSerial: TLabel;
    lbFileSys: TLabel;
    lbTime: TLabel;
		ioFileInfo: TMenuItem;
    toDeleteFile: TMenuItem;
    toMoveFile: TMenuItem;
		Label2: TLabel;
    edTmpPath: TEdit;
		Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edCurrPath: TEdit;
		edWinSysPath: TEdit;
    edWinPath: TEdit;
    imFloppy: TImage;
    imCdRom: TImage;
    imRamDisk: TImage;
    imHDrive: TImage;
		imRemote: TImage;
    ioDisplayCaps: TMenuItem;
    ioFileSystemInfo: TMenuItem;
    idoDisplay: TMenuItem;
    idoPrinter: TMenuItem;
    ioHardwareProfile: TMenuItem;
    N2: TMenuItem;
    toAddTaskbar: TMenuItem;
    hoAboutWindows: TMenuItem;
    toRemovefromTaskbar: TMenuItem;
    N3: TMenuItem;
    toShutDown: TMenuItem;
    tsdLogoff: TMenuItem;
    tsdPoweroff: TMenuItem;
		tsdReboot: TMenuItem;
    tsdShutdown: TMenuItem;
    N4: TMenuItem;
    tsdForce: TMenuItem;
    edLabel: TEdit;
    edSerial: TEdit;
    edFileSys: TEdit;
    ioSystemLocales: TMenuItem;
    ggDUsed: TmcGauge;
    ggUPM: TmcGauge;
    ggUPF: TmcGauge;
    ggUVM: TmcGauge;
    ggDFree: TmcGauge;
		procedure FormCreate(Sender: TObject);
		procedure cbxDrivesDrawItem(Control: TWinControl; Index: Integer;
			Rect: TRect; State: TOwnerDrawState);
    procedure Timer1Timer(Sender: TObject);
		procedure cbxDrivesChange(Sender: TObject);
		procedure toFileCopyClick(Sender: TObject);
		procedure foExitClick(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure toRunFileClick(Sender: TObject);
		procedure ioSystemClick(Sender: TObject);
		procedure ioCPUClick(Sender: TObject);
		procedure hoAboutClick(Sender: TObject);
		procedure hoHelpTopicsClick(Sender: TObject);
    procedure toRenameFileDirectoryClick(Sender: TObject);
    procedure ioInputLocaleClick(Sender: TObject);
    procedure ioSystemMetricsClick(Sender: TObject);
    procedure ioEnvironmentVariablesClick(Sender: TObject);
    procedure ioFileInfoClick(Sender: TObject);
    procedure toDeleteFileClick(Sender: TObject);
		procedure ioVolumeInfoClick(Sender: TObject);
		procedure idoDisplayClick(Sender: TObject);
		procedure ioHardwareProfileClick(Sender: TObject);
		procedure toAddTaskbarClick(Sender: TObject);
		procedure toDeleteTaskbarClick(Sender: TObject);
		procedure hoAboutWindowsClick(Sender: TObject);
		procedure tsdLogoffClick(Sender: TObject);
		procedure tsdForceClick(Sender: TObject);
    procedure ioSystemLocalesClick(Sender: TObject);
	private
		Drives: string;
		procedure SetGauge(Gauge: TmcGauge; MaxVal,Progress: Int64);
	public
		function FindDrives: boolean;
		procedure AddBitmap(Drv: Char; idx: integer);
	end;

var
	frMain: TfrMain;

implementation

uses FormCopy, FormRun, FormOSInfo, FormCPU, FormAbout, FormMove, FormMetrics,
	FormEnviro, FormFileInfo, FormDispCaps,	FormVolInfo, FormHwProf, mcConst,
  FormRegional, FormLocale;

{$R *.DFM}

procedure TfrMain.SetGauge(Gauge: TmcGauge; MaxVal, Progress: Int64);
begin
	Gauge.MaxValue:= MaxVal;
	Gauge.Progress:= Progress;
end;

{ Let's have some fun and make the list alittle more exciting! }
procedure TfrMain.AddBitmap(Drv: Char; idx: integer);
var
	TmpDrv: string;
begin
	TmpDrv:= Drv+':\';
	case GetDriveType(PChar(TmpDrv)) of
		DRIVE_REMOVABLE: cbxDrives.Items.Objects[idx]:= imFloppy.Picture.Graphic;
		DRIVE_FIXED: cbxDrives.Items.Objects[idx]:= imHDrive.Picture.Graphic;
		DRIVE_REMOTE: cbxDrives.Items.Objects[idx]:= imRemote.Picture.Graphic;
		DRIVE_CDROM: cbxDrives.Items.Objects[idx]:= imCdRom.Picture.Graphic;
		DRIVE_RAMDISK: cbxDrives.Items.Objects[idx]:= imRamDisk.Picture.Graphic;
	end;
end;

{ Find all drives present in the system and list in a combo box }
function TfrMain.FindDrives: boolean;
var
	i: integer;
begin
	Drives:= APITools1.DrivesAll;	//	find drives
	if Drives = '' then		//	This is almost impossible, but don't take a chance!
	mcError('No Disk Drives available');
	for i:= 1 to Length(Drives) do		//	add into combo box
	begin
		cbxDrives.Items.Add(Drives[i]+':');
		AddBitMap(Drives[i],i-1);
	end;
	i:= Pos(APITools1.CurrentDrive,Drives);	// find current drive letter
	if i > 0 then
	cbxDrives.ItemIndex:= i-1	// if it is make it selected drive
	else
	cbxDrives.ItemIndex:= 0;	// otherwise make first available drive active
	result:= Drives <> '';
end;

procedure TfrMain.FormCreate(Sender: TObject);
begin
	with APITools1 do
	begin
		edWinPath.Text:=		WindowsPath;		//	Windows path
		edWinSysPath.Text:=	WindowsSysPath;	//	Windows system path
		edCurrPath.Text:=		CurrentPath;	//	Working path
		edTmpPath.Text:=		WindowsTempPath;				//	Temporary files path
		lbCompName.Caption:= 		' Computer:	'+CompName;	// Computer or workgroup name
		lbUserName.Caption:= 		' User:		'+UsrName;		// User or login name
		lbACPInfo.Caption:= Format(' ANSI Code Page	%d - %s',[GetACP,CPAnsi]);
		lbOEMCP.Caption:= Format(' OEM Code Page	%d - %s',[GetOEMCP,CPOEM]);
		lbKBLayout.Caption:= ' Keyboard layout		'+KBLayout;
		lbKBType.Caption:= Format(' Keyboard type		%d - %s',[KBType,KBTypeStr]);
		lbKBKeys.Caption:= Format(' Keyboard function keys	%d',[KBfncKeys]);
	end;
	FindDrives;
end;

procedure TfrMain.cbxDrivesDrawItem(Control: TWinControl; Index: Integer;
	Rect: TRect; State: TOwnerDrawState);
var
	DrawBitmap: TBitmap;
begin
	DrawBitmap := TBitmap(cbxDrives.Items.Objects[Index]);
	with cbxDrives.Canvas do
	begin
		Draw(Rect.Left+2, Rect.Top+2, DrawBitmap);
		TextOut(Rect.Left+4 + DrawBitmap.Width, Rect.Top+1, cbxDrives.Items[Index]);
	end;
end;

procedure TfrMain.Timer1Timer(Sender: TObject);
begin
	with APITools1 do
	begin
		lbTime.Caption:= FormatDateTime(LocaleInfo(LOCALE_SLONGDATE),Date)+', '+
			FormatDateTime(LocaleInfo(LOCALE_STIMEFORMAT),Time);
		lbTotalPhys.Caption:= SizeStr(' Total:	',MemPhysicalTotal,ssType2);
		lbAvailPhys.Caption:= SizeStr(' Avail:	',MemPhysicalAvail,ssType2);
		lbUsedPhys.Caption:= SizeStr(' Used:	',MemPhysicalTotal-MemPhysicalAvail,ssType2);
		SetGauge(ggUPM,MemPhysicalTotal,MemPhysicalTotal-MemPhysicalAvail);
		lbTotalPageFile.Caption:= SizeStr(' Total:	',MemPageFileTotal,ssType2);
		lbAvailPageFile.Caption:= SizeStr(' Avail:	',MemPageFileAvail,ssType2);
		lbUsedPageFile.Caption:= SizeStr(' Used:	',MemPageFileTotal-MemPageFileAvail,ssType2);
		SetGauge(ggUPF,MemPageFileTotal,MemPageFileTotal-MemPageFileAvail);
		lbTotalVirtual.Caption:= SizeStr(' Total:	',MemVirtualTotal,ssType2);
		lbAvailVirtual.Caption:= SizeStr(' Avail:	',MemVirtualAvail,ssType2);
		lbUsedVirtual.Caption:= SizeStr(' Used:	',MemVirtualTotal-MemVirtualAvail,ssType2);
		SetGauge(ggUVM,MemVirtualTotal,MemVirtualTotal-MemVirtualAvail);
	end;
end;

procedure TfrMain.cbxDrivesChange(Sender: TObject);
var
	dskSize,
	dskFree,
	dskUsed: Int64;
	TmpDrv: string;
	Drv: Char;
begin
	with cbxDrives do
	TmpDrv:= Items[ItemIndex];
	Drv:= TmpDrv[1];
	with APITools1 do
	if GetVolumeInfo(Drv) then
	begin
		edLabel.Text:= VolumeName;
		edSerial.Text:= Format('%.4x-%.4x', [HiWord(VolSerialNo),LoWord(VolSerialNo)]);
		edFileSys.Text:= VolFileSystem;
	end else
	begin
		edLabel.Text:= ErrorMsg;
		edSerial.Clear;
		edFileSys.Clear;
	end;
	with APITools1 do
	begin
		if GetDiskCapacity(Drv, dskSize, dskFree, dskUsed) then
		begin
			lbSize.Caption:= SizeStr(' Size:	', dskSize,ssType2);
			lbFree.Caption:= SizeStr(' Free:	', dskFree,ssType2);
			lbUsed.Caption:= SizeStr(' Used:	', dskUsed,ssType2);
			SetGauge(ggDUsed, dskSize div DivKByte, dskUsed div DivKByte);
			SetGauge(ggDFree, dskSize div DivKByte, dskFree div DivKByte);
		end else
		begin
			lbSize.Caption:= ErrorMsg;
			lbFree.Caption:= '';
			lbUsed.Caption:= '';
			SetGauge(ggDUsed, 0, 0);
			SetGauge(ggDFree, 0, 0);
		end;
	end;
end;

procedure TfrMain.toFileCopyClick(Sender: TObject);
begin
	APITools1.FormShow(TfrFileCopy, frMain);
end;

procedure TfrMain.foExitClick(Sender: TObject);
begin
	Close;
end;

procedure TfrMain.FormDestroy(Sender: TObject);
begin
	Timer1.Enabled:= False;
	toDeleteTaskbarClick(Sender);	// remove icon from taskbar
end;

procedure TfrMain.toRunFileClick(Sender: TObject);
begin
	APITools1.FormShow(TfrRun, frMain);
end;

procedure TfrMain.ioSystemClick(Sender: TObject);
begin
	APITools1.FormShow(TfrOSInfo, frMain);
end;

procedure TfrMain.ioCPUClick(Sender: TObject);
begin
	APITools1.FormShow(TfrCPUInfo, frMain);
end;

procedure TfrMain.hoAboutClick(Sender: TObject);
begin
	APITools1.FormShow(TfrAbout, frMain);
end;

procedure TfrMain.hoHelpTopicsClick(Sender: TObject);
begin
	Application.HelpFile:= APITools1.CurrentPath+'APITOOL.HLP';
	Application.HelpCommand(HELP_CONTENTS,0);
end;

procedure TfrMain.toRenameFileDirectoryClick(Sender: TObject);
begin
	APITools1.FormShow(TfrFileMove, frMain);
end;

procedure TfrMain.ioInputLocaleClick(Sender: TObject);
begin
	APITools1.FormShow(TfrRegional, frMain);
end;

procedure TfrMain.ioSystemMetricsClick(Sender: TObject);
begin
	APITools1.FormShow(TfrMetrics, frMain);
end;

procedure TfrMain.ioEnvironmentVariablesClick(Sender: TObject);
begin
	APITools1.FormShow(TfrEnvVars, frMain);
end;

procedure TfrMain.ioFileInfoClick(Sender: TObject);
begin
	APITools1.FormShow(TfrFileInfo, frMain);
end;

procedure TfrMain.toDeleteFileClick(Sender: TObject);
begin
	ShowMessage('This option is not implemented yet! Sorry.'#13+
		'keep looking for updated version of APITools Component');
end;

procedure TfrMain.ioVolumeInfoClick(Sender: TObject);
begin
	APITools1.FormShow(TfrVolInfo, frMain);
end;

procedure TfrMain.idoDisplayClick(Sender: TObject);
begin
	with (Sender as TMenuItem) do
	Checked:= not Checked;
	APITools1.FormShow(TfrDispCaps, frMain);
end;

procedure TfrMain.ioHardwareProfileClick(Sender: TObject);
begin
	//with APITools1 do
	if APITools1.HwProfGuid = '' then
	raise Exception.Create('GetCurrentHwProfile() function not found');
	APITools1.FormShow(TfrHwProfile, frMain);
end;

procedure TfrMain.toAddTaskbarClick(Sender: TObject);
begin
	if APITools1.TaskbarIcon('APITools Test 1.0', iaAdd) then
	//Hide;	// ??? Write a callback routine to show application when icon clicked
end;

procedure TfrMain.toDeleteTaskbarClick(Sender: TObject);
begin
	APITools1.TaskbarIcon('', iaRemove);
end;

procedure TfrMain.hoAboutWindowsClick(Sender: TObject);
begin
	ShellAbout(Handle,'', 'API Tools for Delphi by Ural Gunaydin'#13'email: al_gun@ncable.net.au',
		Application.Icon.Handle);
end;

procedure TfrMain.tsdLogoffClick(Sender: TObject);
begin
	with APITools1 do
	ShutDownWindows(TShutDownAction((Sender as TMenuItem).Tag), tsdForce.Checked);
end;

procedure TfrMain.tsdForceClick(Sender: TObject);
begin
	with tsdForce do
	Checked:= not Checked;
end;

procedure TfrMain.ioSystemLocalesClick(Sender: TObject);
begin
	APITools1.FormShow(TfrLocales, Self);
end;

end.
