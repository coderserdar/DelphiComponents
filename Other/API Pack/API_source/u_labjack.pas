unit u_labjack;

// to use with API_labjack component..

interface

type
  tErrorString = array[0..49] of Char;
  tGains = array[0..3]  of Integer;
  tChannels = array[0..3]  of Integer;
  tVoltages = array[0..3]  of Single;
  tVBuffer = array[0..4095] of tVoltages;
  tIOBuffer = array[0..4095] of Integer;
  tCalMatrix = array[0..19]   of Integer;
  tProductIDList = array[0..126] of Integer;
  tSerialnumList = array[0..126] of Integer;
  tLocalIDList = array[0..126] of Integer;
  tPowerList = array[0..126] of Integer;
  tCalMatrixList = array[0..126] of tCalMatrix;
  Buffer18 = array[0..17]  of Integer;

function AISample(
      var idnum: Integer;
      demo: Integer;
      var stateIO: Integer;
      updateIO, ledOn, numChannels: Integer;
      var channels: tChannels;
      var gains: tGains;
      disableCal: Integer;
      var overVoltage : Integer;
      var voltages: tVoltages)
      : Integer stdcall;

function  AIBurst(
      var idnum: Integer;
      demo, stateIOin, updateIO, ledOn, numChannels: Integer;
      var channels: tChannels;
      var gains: tGains;
      var scanRate: single;
      disableCal, triggerIO, triggerState, numScans, timeout: Integer;
      var voltages: tVBuffer;
      var stateIOout: tIOBuffer;
      var overVoltage: Integer;
      transferMode: Integer)
      : Integer stdcall;

function AIStreamStart(
      var idnum: Integer;
      demo, stateIOin, updateIO, ledOn, numChannels: Integer;
      var channels: tChannels;
      var gains: tGains;
      var scanRate: Single;
      disableCal, reserved1, ReadCount: Integer)
      : Integer stdcall;

function AIStreamRead(
      localID, numScans, timeout: Integer;
      var voltages: tVBuffer;
      var stateIOout: tIOBuffer;
      var reserved: Integer;
      var ljScanBacklog: Integer;
      var overVoltage: Integer)
      : Integer stdcall;

function AIStreamClear(
      localID: Integer)
      : Integer stdcall;

function AOUpdate(
      var idnum: Integer;
      demo, trisD, trisIO: Integer;
    	var stateD: Integer;
      var stateIO: Integer;
      updateDigital, resetCounter: Integer;
      var count: LongWord;
      analogOut0: Single;
      analogOut1: Single)
      : Integer stdcall;

function AsynchConfig(
      var idnum: Integer;
      demo, timeoutMult, configA, configB, configTE, fullA, fullB, fullC, halfA, halfB, halfC: Integer)
      : Integer stdcall;

function Asynch(
      var idnum :Integer;
      demo, portB, enableTE, enableTO, enableDel, baudrate, numWrite, numRead: Integer;
      var data: Buffer18)
      : Integer stdcall;

function BitsToVolts(
      chnum, chgain, bits: Integer;
      volts: Single)
      : Integer stdcall;

function VoltsToBits(
      chnum, chgain: Integer;
      volts: Single;
      var bits: Integer)
      : Integer stdcall;

function Counter(
      var idnum :Integer;
      demo: Integer;
      var stateD: Integer;
		  var stateIO : Integer;
      resetCounter, enableSTB: Integer;
		  var count: LongWord)
      : Integer stdcall;

function DigitalIO(
      var idnum: Integer;
      demo: Integer;
      var trisD: Integer;
		  trisIO: Integer;
      var stateD: Integer;
      var stateIO: Integer;
			updateDigital: Integer;
      var outputD: Integer)
      : Integer stdcall;

function EAnalogIn(
      var idnum: Integer;
      demo, channel, gain: Integer;
			var overVoltage: Integer;
      var voltage: Single)
      : Integer stdcall;

Function EAnalogOut(
      var idnum: Integer;
      demo: Integer;
      analogOut0: Single;
			analogOut1: Single)
      : Integer stdcall;

Function ECount(
      var idnum: Integer;
      demo, resetCounter: Integer;
      var count: Double;
      var ms: Double)
      : Integer stdcall;

function EDigitalIn(
      var idnum: Integer;
      demo, channel, readD: Integer;
      var state: Integer)
      : Integer StdCall;

Function EDigitalOut(
      var idnum: Integer;
      demo, channel, writeD, state: Integer)
      : Integer stdcall;

function GetDriverVersion()
      : Single stdcall;

procedure GetErrorString(
      errorcode: Integer;
      var errorString: tErrorString)
      stdcall;

function GetFirmwareVersion(
      var idnum: Integer)
      : Single stdcall;

function GetWinVersion(
      var majorVersion: LongWord;
      var minorVersion: LongWord;
      var buildNumber: LongWord;
      var platformID: LongWord;
      var servicePackMajor: LongWord;
      var servicePackMinor: LongWord)
      : Integer stdcall;

function ListAll(
      var productIDList: Integer;
      var serialnumList: tSerialNumList;
      var localIDList: tLocalIDList;
      var powerList: tPowerList;
      var calMatrixList: tCalMatrixList;
      var numberFound: Integer;
      var fcddMaxSize: Integer;
      var hvcMaxSize: Integer)
      : Integer stdcall;

function LocalID(
      var idnum: Integer;
      localID: Integer)
      : Integer stdcall;

function NoThread(
      var idnum: Integer;
      noThread: Integer)
      : Integer stdcall;

function PulseOut(
      var idnum: Integer;
      demo, LowFirst, bitSelect, numPulses, timeB1, timeC1, timeB2, timeC2: Integer)
      : Integer stdcall;

function PulseOutStart(
      var idnum: Integer;
      demo, lowFirst, bitSelect, numPulses, timeB1, timeC1, timeB2, timeC2: Integer)
      : Integer stdcall;

function PulseOutFinish(
      var idnum: Integer;
      demo, timeoutMS: Integer)
      : Integer stdcall;

function PulseOutCalc(
      var frequency: single;
      var timeB: Integer;
      var timeC: Integer)
      : Integer stdcall;

function ReEnum(
      var idnum : Integer)
      : Integer stdcall;

function ResetLJ(
      var  idnum: Integer)
      : Integer stdcall;

function Synch(
      var idnum: Integer;
      demo, mode, msDelay, husDelay, controlCS, csLine, csState, configD, numWriteRead: Integer;
		  var data: Buffer18)
      : Integer stdcall;

function Watchdog(
      var idnum: Integer;
      demo, active, timeout, reset, activeD0, activeD1, activeD8, stateD0, stateD1, stateD8: Integer)
      : Integer stdcall;

function ReadMem(
      var idnum: Integer;
      address: Integer;
      var data3: Integer;
			var data2: Integer;
      var data1: Integer;
      var data0: Integer)
      : Integer stdcall;

function WriteMem(
      var idnum: Integer;
      unlocked, address, data3, data2, data1, data0: Integer)
      : Integer stdcall;

implementation

function AISample(
      var idnum: Integer;
      demo: Integer;
      var stateIO: Integer;
      updateIO, ledOn, numChannels: Integer;
      var channels: tChannels;
      var gains: tGains;
      disableCal: Integer;
      var overVoltage : Integer;
      var voltages: tVoltages)
      : Integer stdcall;
      external  'ljackuw.dll';

function AIBurst(
      var idnum: Integer;
      demo, stateIOin, updateIO, ledOn, numChannels: Integer;
      var channels: tChannels;
      var gains: tGains;
      var scanRate: single;
      disableCal, triggerIO, triggerState, numScans, timeout: Integer;
      var voltages: tVBuffer;
      var stateIOout: tIOBuffer;
      var overVoltage: Integer;
      transferMode: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function AIStreamStart(
      var idnum: Integer;
      demo, stateIOin, updateIO, ledOn, numChannels: Integer;
      var channels: tChannels;
      var gains: tGains;
      var scanRate: Single;
      disableCal, reserved1, ReadCount: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function AIStreamRead(
      localID, numScans, timeout: Integer;
      var voltages: tVBuffer;
      var stateIOout: tIOBuffer;
      var reserved: Integer;
      var ljScanBacklog: Integer;
      var overVoltage: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function AIStreamClear(
      localID: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function AOUpdate(
      var idnum: Integer;
      demo, trisD, trisIO: Integer;
    	var stateD: Integer;
      var stateIO: Integer;
      updateDigital, resetCounter: Integer;
      var count: LongWord;
      analogOut0: Single;
      analogOut1: Single)
      : Integer stdcall;
      external  'ljackuw.dll';

function AsynchConfig(
      var idnum: Integer;
      demo, timeoutMult, configA, configB, configTE, fullA, fullB, fullC, halfA, halfB, halfC: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function Asynch(
      var idnum :Integer;
      demo, portB, enableTE, enableTO, enableDel, baudrate, numWrite, numRead: Integer;
      var data: Buffer18)
      : Integer stdcall;
      external  'ljackuw.dll';

function BitsToVolts(
      chnum, chgain, bits: Integer;
      volts: Single)
      : Integer stdcall;
      external  'ljackuw.dll';

function VoltsToBits(
      chnum, chgain: Integer;
      volts: Single;
      var bits: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function Counter(
      var idnum :Integer;
      demo: Integer;
      var stateD: Integer;
		  var stateIO : Integer;
      resetCounter, enableSTB: Integer;
		  var count: LongWord)
      : Integer stdcall;
      external  'ljackuw.dll';

function DigitalIO(
      var idnum: Integer;
      demo: Integer;
      var trisD: Integer;
		  trisIO: Integer;
      var stateD: Integer;
      var stateIO: Integer;
			updateDigital: Integer;
      var outputD: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function EAnalogIn(
      var idnum: Integer;
      demo, channel, gain: Integer;
			var overVoltage: Integer;
      var voltage: Single)
      : Integer stdcall;
      external  'ljackuw.dll';

Function EAnalogOut(
      var idnum: Integer;
      demo: Integer;
      analogOut0: Single;
			analogOut1: Single)
      : Integer stdcall;
      external  'ljackuw.dll';

Function ECount(
      var idnum: Integer;
      demo, resetCounter: Integer;
      var count: Double;
      var ms: Double)
      : Integer stdcall;
      external  'ljackuw.dll';

function EDigitalIn(
      var idnum: Integer;
      demo, channel, readD: Integer;
      var state: Integer)
      : Integer StdCall;
      external  'ljackuw.dll';

Function EDigitalOut(
      var idnum: Integer;
      demo, channel, writeD, state: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function GetDriverVersion()
      : Single stdcall;
      external  'ljackuw.dll';

procedure GetErrorString(
      errorcode: Integer;
      var errorString: tErrorString)
      stdcall;
      external  'ljackuw.dll';

function GetFirmwareVersion(
      var idnum: Integer)
      : Single stdcall;
      external  'ljackuw.dll';

function GetWinVersion(
      var majorVersion: LongWord;
      var minorVersion: LongWord;
      var buildNumber: LongWord;
      var platformID: LongWord;
      var servicePackMajor: LongWord;
      var servicePackMinor: LongWord)
      : Integer stdcall;
      external  'ljackuw.dll';

function ListAll(
      var productIDList: Integer;
      var serialnumList: tSerialNumList;
      var localIDList: tLocalIDList;
      var powerList: tPowerList;
      var calMatrixList: tCalMatrixList;
      var numberFound: Integer;
      var fcddMaxSize: Integer;
      var hvcMaxSize: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function LocalID(
      var idnum: Integer;
      localID: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function NoThread(
      var idnum: Integer;
      noThread: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function PulseOut(
      var idnum: Integer;
      demo, LowFirst, bitSelect, numPulses, timeB1, timeC1, timeB2, timeC2: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function PulseOutStart(
      var idnum: Integer;
      demo, lowFirst, bitSelect, numPulses, timeB1, timeC1, timeB2, timeC2: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function PulseOutFinish(
      var idnum: Integer;
      demo, timeoutMS: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function PulseOutCalc(
      var frequency: single;
      var timeB: Integer;
      var timeC: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function ReEnum(
      var idnum : Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function ResetLJ(
      var  idnum: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function Synch(
      var idnum: Integer;
      demo, mode, msDelay, husDelay, controlCS, csLine, csState, configD, numWriteRead: Integer;
		  var data: Buffer18)
      : Integer stdcall;
      external  'ljackuw.dll';

function Watchdog(
      var idnum: Integer;
      demo, active, timeout, reset, activeD0, activeD1, activeD8, stateD0, stateD1, stateD8: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function ReadMem(
      var idnum: Integer;
      address: Integer;
      var data3: Integer;
			var data2: Integer;
      var data1: Integer;
      var data0: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

function WriteMem(
      var idnum: Integer;
      unlocked, address, data3, data2, data1, data0: Integer)
      : Integer stdcall;
      external  'ljackuw.dll';

end.
