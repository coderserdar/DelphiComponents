/*
	BASSWMA live broadcast example
	Copyright (c) 2002-2008 Un4seen Developments Ltd.
*/

#include <windows.h>
#include <stdio.h>
#include "basswma.h"
#include "bass.h"

HWND win;

#define SAMPLERATE 44100
#define CHANNELS 2

HRECORD rchan=0;	// recording channel
HWMENCODE handle;	// encoder handle
float time;			// elapsed time
DWORD level;		// input level
char lastip[32]="none"; // last client to connect
int displaycount;

// display error messages
void Error(const char *es)
{
	char mes[200];
	sprintf(mes,"%s\n(error code: %d)",es,BASS_ErrorGetCode());
	MessageBox(win,mes,0,0);
}

// messaging macros
#define MESS(id,m,w,l) SendDlgItemMessage(win,id,m,(WPARAM)(w),(LPARAM)(l))
#define DLGITEM(id) GetDlgItem(win,id)

// update the status and level display
void UpdateDisplay()
{
	char text[40]="Off Air";
	if (BASS_ChannelIsActive(rchan)) {
		DWORD l=BASS_ChannelGetLevel(rchan); // get current level
		level=level>1500?level-1500:0;
		if (LOWORD(l)>level) level=LOWORD(l);
		if (HIWORD(l)>level) level=HIWORD(l);
		if (displaycount&128) {
			if (displaycount&64) // display last client
				sprintf(text,"last: %s",lastip);
			else // display client count
				sprintf(text,"current clients: %d",BASS_WMA_EncodeGetClients(handle));
		} else { // display "on air"
			DWORD t=(DWORD)time;
			sprintf(text,"On Air - port: %d - %d:%02d ",BASS_WMA_EncodeGetPort(handle),t/60,t%60);
		}
		displaycount++;
	} else level=0;
	{ // draw the level bar
		HWND w=DLGITEM(30);
		HDC dc=GetWindowDC(w);
		RECT r;
		GetClientRect(w,&r);
		InflateRect(&r,-1,-1);
		r.top=r.bottom*(32768-level)/32768;
		FillRect(dc,&r,GetStockObject(WHITE_BRUSH));
		r.bottom=r.top;
		r.top=1;
		FillRect(dc,&r,GetStockObject(LTGRAY_BRUSH));
		ReleaseDC(w,dc);
	}
	MESS(20,WM_SETTEXT,0,text); // update status text
}

// recording callback
BOOL CALLBACK RecordingCallback(HRECORD chan, const void *buffer, DWORD length, void *user)
{
	time+=length/(float)(SAMPLERATE*CHANNELS*2); // increase elapsed time counter
	// encode the sample data, and continue recording if successful
	return BASS_WMA_EncodeWrite(handle,buffer,length);
}

// client connection notification callback
void CALLBACK ClientConnect(HWMENCODE handle, BOOL connect, const char *ip, void *user)
{
	if (connect) strcpy(lastip,ip); // keep the client's ip for display
}

// start recording & encoding
void Start()
{
	char buf[100];
	MESS(12,WM_GETTEXT,100,buf); // get bitrate
	// initialize encoder - let system choose port, max 5 clients
	handle=BASS_WMA_EncodeOpenNetwork(SAMPLERATE,CHANNELS,BASS_WMA_ENCODE_SCRIPT,atoi(buf),0,5);
	if (!handle) {
		Error("Can't initialize encoding");
		return;
	}
	MESS(11,WM_GETTEXT,100,buf); // get title
	BASS_WMA_EncodeSetTag(handle,"Title",buf,BASS_WMA_TAG_ANSI); // set WMA title tag
	BASS_WMA_EncodeSetNotify(handle,&ClientConnect,0); // setup client notification
	time=0;
	displaycount=0;
	// start recording
	if (!(rchan=BASS_RecordStart(SAMPLERATE,CHANNELS,0,&RecordingCallback,0))) {
		Error("Can't start recording");
		BASS_WMA_EncodeClose(handle);
		return;
	}
	MESS(10,WM_SETTEXT,0,"Stop");
	EnableWindow(DLGITEM(11),FALSE);
	EnableWindow(DLGITEM(12),FALSE);
	EnableWindow(DLGITEM(13),TRUE);
	SetTimer(win,0,50,0); // timer to update the display
}

// stop recording & encoding
void Stop()
{
	KillTimer(win,0);
	BASS_ChannelStop(rchan); // stop recording
	BASS_WMA_EncodeClose(handle); // stop encoding
	MESS(10,WM_SETTEXT,0,"Start");
	EnableWindow(DLGITEM(11),TRUE);
	EnableWindow(DLGITEM(12),TRUE);
	EnableWindow(DLGITEM(13),FALSE);
	UpdateDisplay();
}

BOOL CALLBACK dialogproc(HWND h,UINT m,WPARAM w,LPARAM l)
{
	switch (m) {
		case WM_TIMER:
			if (!BASS_ChannelIsActive(rchan))
				Stop();
			else
				UpdateDisplay();
			break;

		case WM_COMMAND:
			switch (LOWORD(w)) {
				case IDCANCEL:
					DestroyWindow(h);
					break;
				case 10:
					if (!BASS_ChannelIsActive(rchan))
						Start();
					else
						Stop();
					break;
				case 13:
					if (HIWORD(w)==EN_CHANGE) { // update "caption" tag
						char buf[100];
						MESS(13,WM_GETTEXT,100,buf);
						BASS_WMA_EncodeSetTag(handle,"Caption",buf,BASS_WMA_TAG_ANSI);
					}
					break;
			}
			break;

		case WM_INITDIALOG:
			win=h;
			// setup recording (using default device)
			if (!BASS_RecordInit(-1)) {
				Error("Can't initialize device");
				DestroyWindow(win);
			} else {
				// get the available bitrates
				const DWORD *rates=BASS_WMA_EncodeGetRates(SAMPLERATE,CHANNELS,0);
				if (!rates) {
					Error("Can't find a codec");
					DestroyWindow(win);
				} else {
					char buf[20];
					while (*rates) {
						sprintf(buf,"%d",*rates);
						MESS(12,CB_ADDSTRING,0,buf);
						rates++;
					}
				}
				UpdateDisplay();
			}
			return 1;

		case WM_DESTROY:
			BASS_RecordFree();
			BASS_WMA_EncodeClose(handle); // incase it was encoding on exit
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

	DialogBox(hInstance,(char*)1000,0,&dialogproc);

	return 0;
}
