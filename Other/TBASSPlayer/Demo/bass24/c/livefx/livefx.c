/*
	BASS full-duplex test
	Copyright (c) 2002-2008 Un4seen Developments Ltd.
*/

#include <windows.h>
#include <commctrl.h>
#include <stdio.h>
#include <math.h>
#include "bass.h"

HWND win=NULL;

#define MESS(id,m,w,l) SendDlgItemMessage(win,id,m,(WPARAM)(w),(LPARAM)(l))

HRECORD rchan;	// recording channel
HSTREAM chan;	// playback stream
HFX fx[4]={0};	// FX handles
int chunk;		// recording chunk size
int input;		// current input source
int latency=0;	// current latency

void Error(const char *es)
{
	char mes[200];
	sprintf(mes,"%s\n(error code: %d)",es,BASS_ErrorGetCode());
	MessageBox(win,mes,0,0);
}

BOOL CALLBACK RecordingCallback(HRECORD handle, const void *buffer, DWORD length, void *user)
{
	BASS_StreamPutData(chan,buffer,length); // feed recorded data to output stream
	return TRUE; // continue recording
}

static BOOL Initialize()
{
	BASS_INFO bi;
	// initialize output, get latency
	if (!BASS_Init(-1,44100,BASS_DEVICE_LATENCY,win,NULL)) {
		Error("Can't initialize output");
		return FALSE;
	}

	BASS_GetInfo(&bi);
	if (bi.dsver<8) { // no DX8, so disable effect buttons
		EnableWindow(GetDlgItem(win,20),FALSE);
		EnableWindow(GetDlgItem(win,21),FALSE);
		EnableWindow(GetDlgItem(win,22),FALSE);
		EnableWindow(GetDlgItem(win,23),FALSE);
	}

	// create a stream to play the recording
	chan=BASS_StreamCreate(44100,2,0,STREAMPROC_PUSH,0);

	// start recording with 10ms period
	if (!BASS_RecordInit(-1) || !(rchan=BASS_RecordStart(44100,2,MAKELONG(0,10),RecordingCallback,0))) {
		BASS_RecordFree();
		BASS_Free();
		Error("Can't initialize recording");
		return FALSE;
	}

	{ // get list of inputs
		int c;
		const char *i;
		for (c=0;i=BASS_RecordGetInputName(c);c++) {
			float level;
			MESS(10,CB_ADDSTRING,0,i);
			if (!(BASS_RecordGetInput(c,&level)&BASS_INPUT_OFF)) { // this 1 is currently "on"
				input=c;
				MESS(10,CB_SETCURSEL,input,0);
				MESS(11,TBM_SETPOS,TRUE,level*100); // set level slider
			}
		}
	}

	{ // prebuffer at least "minbuf" amount of data before starting playback
		DWORD prebuf=BASS_ChannelSeconds2Bytes(chan,bi.minbuf/1000.f);
		while (BASS_ChannelGetData(chan,NULL,BASS_DATA_AVAILABLE)<prebuf)
			Sleep(1);
	}
	BASS_ChannelPlay(chan,FALSE);

	return TRUE;
}

BOOL CALLBACK dialogproc(HWND h,UINT m,WPARAM w,LPARAM l)
{
	switch (m) {
		case WM_TIMER:
			{ // display current latency (input+output buffer level)
				char buf[20];
				latency=(latency*3+BASS_ChannelGetData(chan,NULL,BASS_DATA_AVAILABLE)
					+BASS_ChannelGetData(rchan,NULL,BASS_DATA_AVAILABLE))/4;
				sprintf(buf,"%d",(int)(BASS_ChannelBytes2Seconds(chan,latency)*1000));
				MESS(15,WM_SETTEXT,0,buf);
			}
			break;

		case WM_COMMAND:
			switch (LOWORD(w)) {
				case IDCANCEL:
					DestroyWindow(h);
					break;
				case 10:
					if (HIWORD(w)==CBN_SELCHANGE) { // input selection changed
						int i;
						float level;
						input=MESS(10,CB_GETCURSEL,0,0); // get the selection
						for (i=0;BASS_RecordSetInput(i,BASS_INPUT_OFF,-1);i++) ; // 1st disable all inputs, then...
						BASS_RecordSetInput(input,BASS_INPUT_ON,-1); // enable the selected input
						BASS_RecordGetInput(input,&level); // get the level
						MESS(11,TBM_SETPOS,TRUE,level*100);
					}
					break;
				case 20: // toggle chorus
					if (fx[0]) {
						BASS_ChannelRemoveFX(chan,fx[0]);
						fx[0]=0;
					} else
						fx[0]=BASS_ChannelSetFX(chan,BASS_FX_DX8_CHORUS,0);
					break;
				case 21: // toggle gargle
					if (fx[1]) {
						BASS_ChannelRemoveFX(chan,fx[1]);
						fx[1]=0;
					} else
						fx[1]=BASS_ChannelSetFX(chan,BASS_FX_DX8_GARGLE,0);
					break;
				case 22: // toggle reverb
					if (fx[2]) {
						BASS_ChannelRemoveFX(chan,fx[2]);
						fx[2]=0;
					} else
						fx[2]=BASS_ChannelSetFX(chan,BASS_FX_DX8_REVERB,0);
					break;
				case 23: // toggle flanger
					if (fx[3]) {
						BASS_ChannelRemoveFX(chan,fx[3]);
						fx[3]=0;
					} else
						fx[3]=BASS_ChannelSetFX(chan,BASS_FX_DX8_FLANGER,0);
					break;
			}
			break;

		case WM_HSCROLL:
			if (l) { // set input source level
				float level=SendMessage((HWND)l,TBM_GETPOS,0,0)/100.f;
				if (!BASS_RecordSetInput(input,0,level)) // failed to set input level
					BASS_RecordSetInput(-1,0,level); // try master level instead
			}
			break;

		case WM_INITDIALOG:
			win=h;
			MESS(11,TBM_SETRANGE,FALSE,MAKELONG(0,100)); // initialize input level slider
			MessageBox(win,
				"Do not set the input to 'WAVE' / 'What you hear' (etc...) with\n"
				"the level set high, as that is likely to result in nasty feedback.\n",
				"Feedback warning",MB_ICONWARNING);
			if (!Initialize()) {
				DestroyWindow(win);
				break;
			}
			SetTimer(h,1,250,NULL);
			return 1;

		case WM_DESTROY:
			KillTimer(h,1);
			// release it all
			BASS_RecordFree();
			BASS_Free();
			break;
	}
	return 0;
}

int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,LPSTR lpCmdLine, int nCmdShow)
{
	// check the correct BASS was loaded
	if (HIWORD(BASS_GetVersion())!=BASSVERSION) {
		MessageBox(0,"An incorrect version of BASS.DLL was loaded",0,MB_ICONERROR);
		return 0;
	}

	{ // enable trackbar support (for the level control)
		INITCOMMONCONTROLSEX cc={sizeof(cc),ICC_BAR_CLASSES};
		InitCommonControlsEx(&cc);
	}

	DialogBox(hInstance,(char*)1000,0,&dialogproc);

	return 0;
}
