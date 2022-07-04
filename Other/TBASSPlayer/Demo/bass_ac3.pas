Unit BASS_AC3;

interface

uses windows, bass;

const
  // BASS_Set/GetConfig options
  BASS_CONFIG_AC3_DYNRNG         = $10001;

  // Additional BASS_AC3_StreamCreateFile/User/URL flags
  BASS_AC3_DYNAMIC_RANGE = $800;	// enable dynamic range compression

  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_AC3        = $11000;


const
  bassac3dll = 'bass_ac3.dll';

function BASS_AC3_StreamCreateFile(mem:BOOL; f:Pointer; offset,length:QWORD; flags:DWORD): HSTREAM; stdcall; external bassac3dll;
function BASS_AC3_StreamCreateURL(URL:PChar; offset:DWORD; flags:DWORD; proc:DOWNLOADPROC; user:Pointer): HSTREAM; stdcall; external bassac3dll;
function BASS_AC3_StreamCreateFileUser(system,flags:DWORD; var procs:BASS_FILEPROCS; user:Pointer): HSTREAM; stdcall; external bassac3dll;

implementation

end.
