//Author: Poul Bak
//Copyright © 2000 - 2004 : Bak-O-Soft (Poul Bak). All rights reserved.
//http://bak-o-soft.dk/
//Mailto:info@bak-o-soft.dk

unit DelphiPrinterSetup;

//Version: 2.20.00.00

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	Printers, WinSpool;

type
	TPrinterConfig = record
		PrinterName, Driver, Port : array[0..MAX_PATH] of Char;
		SizeOfDeviceMode : Cardinal;
	end;

	TDelphiPrinterSetup = class(TPersistent)
	private
		{ Private declarations }
		FFileName : string;
		FPrinterRecord : TPrinterConfig;
		FConfigFile : file of TPrinterConfig;
		FDeviceModeFile : file of Char;
		PPrinterDevMode : PDevMode;
		DevModeHandle : THandle;
	public
		constructor Create;
		destructor Destroy; override;
		function GetPrinterSetup: Boolean;
		function SetPrinterSetup: Boolean;
		procedure LoadSetup;
		procedure SaveSetup;
	end;

implementation

var
	DelphiPrinterSetupObject : TDelphiPrinterSetup = nil;

constructor TDelphiPrinterSetup.Create;
begin
	inherited;
	FFileName := ExtractFilePath(ParamStr(0)) + 'DelphiPrinterSetup.cfg';
	LoadSetup;
end;

destructor TDelphiPrinterSetup.Destroy;
begin
	SaveSetup;
	inherited Destroy;
end;

function TDelphiPrinterSetup.GetPrinterSetup : Boolean;
begin
	if Printer.Printers.Count = 0 then Result := False
	else
	begin
		Result := True;
		ZeroMemory(@FPrinterRecord, SizeOf(FPrinterRecord));
		with FPrinterRecord do
		begin
			Printer.GetPrinter(PrinterName, Driver, Port, DevModeHandle);
			SizeOfDeviceMode := GlobalSize(DevModeHandle);
			if SizeOfDeviceMode = 0 then Result := False
			else
			begin
				PPrinterDevMode := GlobalLock(DevModeHandle);
				if PPrinterDevMode = nil then Result := False;
			end;
		end;
	end;
end;

function TDelphiPrinterSetup.SetPrinterSetup : Boolean;
begin
	if Printer.Printers.Count = 0 then Result := False
	else
	begin
		with FPrinterRecord do Printer.SetPrinter(PrinterName, Driver, Port, DevModeHandle);
		GlobalUnLock(DevModeHandle);
		Result := True;
	end;
end;

procedure TDelphiPrinterSetup.LoadSetup;
var
	Index : integer;
	TempRecord : TPrinterConfig;
begin
	if Printer.Printers.Count = 0 then Exit;
	if FileExists(FFilename) then
	begin
		AssignFile(FConfigFile, FFileName);
		Reset(FConfigFile);
		Read(FConfigFile, TempRecord);
		CloseFile(FConfigFile);
		with TempRecord do
		begin
			Index := 0;
			while (Index < Printer.Printers.Count)
				and (AnsiUpperCase(Copy(Printer.Printers[Index], 1, StrLen(PrinterName)))
				<> AnsiUpperCase(PrinterName)) do Inc(Index);
		end;
		if Index < Printer.Printers.Count then
		begin
			FPrinterRecord := TempRecord;
			with FPrinterRecord do
			begin
				DevModeHandle:=GlobalAlloc(GHND, SizeOfDeviceMode);
				PPrinterDevMode := GlobalLock(DevModeHandle);
				AssignFile(FDeviceModeFile, FFileName);
				Reset(FDeviceModeFile);
				Seek(FDeviceModeFile, SizeOf(FPrinterRecord));
				BlockRead(FDeviceModeFile, PPrinterDevMode^, SizeOfDeviceMode);
				CloseFile(FDeviceModeFile);
				SetPrinterSetup;
			end;
		end;
	end;
end;

procedure TDelphiPrinterSetup.SaveSetup;
begin
	if Printer.Printers.Count = 0 then Exit;
	if GetPrinterSetup then
	begin
		AssignFile(FConfigFile, FFileName);
		ReWrite(FConfigFile);
		Write(FConfigFile, FPrinterRecord);
		CloseFile(FConfigFile);
		AssignFile(FDeviceModeFile, FFileName);
		Reset(FDeviceModeFile);
		Seek(FDeviceModeFile, FileSize(FDeviceModeFile));
		with FPrinterRecord do
		begin
			BlockWrite(FDeviceModeFile, PPrinterDevMode^, SizeOfDeviceMode);
		end;
		CloseFile(FDeviceModeFile);
		GlobalUnLock(DevModeHandle);
	end;
end;

//  ------------------- Initialization --------------------------
initialization
begin
	if Pos('\DELPHI32.EXE', UpperCase(ParamStr(0))) > 0
		then DelphiPrinterSetupObject := TDelphiPrinterSetup.Create;
end;

//  ------------------- Finalization ----------------------------
finalization
begin
	if (DelphiPrinterSetupObject <> nil) then
	begin
		DelphiPrinterSetupObject.Free;
		DelphiPrinterSetupObject := nil;
	end;
end;

end.

