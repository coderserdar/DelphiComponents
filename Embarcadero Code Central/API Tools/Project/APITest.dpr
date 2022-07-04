{
******************************************************************************
*	Copyright 2002, mcTech - Ural Gunaydin All Rights Reserved.       				 *
*                                                                            *
*		File:       DrvInf.pas                                                   *
*		Content:    TAPITools component test program                             *
*                                                                            *
*		Created by Ural Gunaydin                                                 *
*                                                                            *
*		E-mail: ural@jc.com.au                                                   *
*                                                                            *
******************************************************************************}
{$A+,Z+}
{$D APITools Test, Copyright 2003, Ural Gunaydin}

program APITest;

uses
	//MemMgr,
  mcConst,
  Forms,
  FormMain in 'FormMain.pas' {frMain},
  FormOSInfo in 'FormOSInfo.pas' {frOSInfo},
  FormCPU in 'FormCPU.pas' {frCPUInfo},
  FormCopy in 'FormCopy.pas' {frFileCopy},
  FormRun in 'FormRun.pas' {frRun},
  FormAbout in 'FormAbout.pas' {frAbout},
  FormMove in 'FormMove.pas' {frFileMove},
  FormRegional in 'FormRegional.pas' {frRegional},
  FormMetrics in 'FormMetrics.pas' {frMetrics},
  FormEnviro in 'FormEnviro.pas' {frEnvVars},
  FormFileInfo in 'FormFileInfo.pas' {frFileInfo},
  FormDispCaps in 'FormDispCaps.pas' {frDispCaps},
  FormVolInfo in 'FormVolInfo.pas' {frVolInfo},
  FormHwProf in 'FormHwProf.pas' {frHwProfile},
  FormLocale in 'FormLocale.pas' {frLocales};

{$R *.RES}

begin
	IsRunning;
	Application.Initialize;
	Application.Title := 'APITools Test';
	Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
