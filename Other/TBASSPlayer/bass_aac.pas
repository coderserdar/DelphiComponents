Unit BASS_AAC;

interface

uses windows, dynamic_bass;

const
  // Additional tags available from BASS_StreamGetTags
  BASS_TAG_MP4        = 7;  // MP4/iTunes metadata

  BASS_AAC_STEREO = $400000; // downmatrix to stereo

  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_AAC        = $10b00;    // AAC
  BASS_CTYPE_STREAM_MP4        = $10b01;    // MP4


const
  bassaacdll = 'bass_aac.dll';


function BASS_AAC_StreamCreateFile(mem:BOOL; f:Pointer; offset,length:QWORD; flags:DWORD): HSTREAM; stdcall; external bassaacdll;
function BASS_AAC_StreamCreateURL(URL:PChar; offset:DWORD; flags:DWORD; proc:DOWNLOADPROC; user:Pointer): HSTREAM; stdcall; external bassaacdll;
function BASS_AAC_StreamCreateFileUser(system,flags:DWORD; var procs:BASS_FILEPROCS; user:Pointer): HSTREAM; stdcall; external bassaacdll;
function BASS_MP4_StreamCreateFile(mem:BOOL; f:Pointer; offset,length:QWORD; flags:DWORD): HSTREAM; stdcall; external bassaacdll;
function BASS_MP4_StreamCreateFileUser(system,flags:DWORD; var procs:BASS_FILEPROCS; user:Pointer): HSTREAM; stdcall; external bassaacdll;

implementation

end.