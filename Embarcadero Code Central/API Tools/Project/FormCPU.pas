unit FormCPU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls;

type
  TfrCPUInfo = class(TForm)
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    GroupBox2: TGroupBox;
    lbArchi: TLabel;
    lbProcessor: TLabel;
    lbType: TLabel;
    lbLevel: TLabel;
    lbModel: TLabel;
		lbStep: TLabel;
		lbRevision: TLabel;
		lbActiveCPU: TLabel;
		lbPage: TLabel;
		lbAllocGran: TLabel;
		lbMinAppAddr: TLabel;
		lbMaxAppAddr: TLabel;
		procedure FormKeyPress(Sender: TObject; var Key: Char);
		procedure FormShow(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	frCPUInfo: TfrCPUInfo;

implementation

uses FormMain, mcConst;

{$R *.DFM}

procedure TfrCPUInfo.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

procedure TfrCPUInfo.FormShow(Sender: TObject);
begin
	with frMain.APITools1 do
	begin
		lbArchi.Caption:= Format(' Architecture		%s',[GetCPUID]);
		lbProcessor.Caption:= Format(' Number of Processors	%d',[CPUCount]);
		lbType.Caption:= Format(' Processors Type	%d',[CPUType]);
		lbLevel.Caption:= Format(' Processor Level		%d',[CPULevel]);
		lbModel.Caption:= Format(' Processor Model	%d',[HiByte(CPURevision)]);
		lbStep.Caption:= Format(' Processor Stepping	%d',[LoByte(CPURevision)]);
		lbRevision.Caption:= Format(' Processor Revision	%d (%sh)',[CPURevision,IntToHex(CPURevision,4)]);
		lbActiveCPU.Caption:= Format(' Active Processors	%d',[CPUActiveMask]);
		lbPage.Caption:= SizeStr(' Page Size		',CPUPageSize, ssType2);
		lbAllocGran.Caption:= SizeStr(' Allocation Granularity	',CPUAllocGran,ssType2);
		lbMinAppAddr.Caption:= Format(' Minimum App. Address	%ph',[CPUMinAppAddress]);
		lbMaxAppAddr.Caption:= Format(' Maximum App. Address	%ph',[CPUMaxAppAddress]);
		//lbCPUInfo.Caption:= Format('%s Family %d Model %d Stepping %d',
			//[Family,CPULevel,HiByte(CPURevision),LoByte(CPURevision)]);
		CheckBox1.Checked:= ProcessorFeature(PF_FLOATING_POINT_PRECISION_ERRATA);
		CheckBox2.Checked:= ProcessorFeature(PF_FLOATING_POINT_EMULATED);
		CheckBox3.Checked:= ProcessorFeature(PF_COMPARE_EXCHANGE_DOUBLE);
		CheckBox4.Checked:= ProcessorFeature(PF_MMX_INSTRUCTIONS_AVAILABLE);
	end;
end;

end.
