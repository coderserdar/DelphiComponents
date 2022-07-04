/*
	BASS custom looping example
	Copyright (c) 2004-2008 Un4seen Developments Ltd.
*/

#include <windows.h>
#include <stdio.h>
#include <process.h>
#include "bass.h"

#define WIDTH 600	// display width
#define HEIGHT 201	// height (odd number for centre line)

HWND win=NULL;
DWORD scanthread=0;
BOOL killscan=FALSE;

DWORD chan;
DWORD bpp;			// bytes per pixel
QWORD loop[2]={0};	// loop start & end
HSYNC lsync;		// looping sync

HDC wavedc=0;
HBITMAP wavebmp=0;
BYTE *wavebuf;

// display error messages
void Error(const char *es)
{
	char mes[200];
	sprintf(mes,"%s\n(error code: %d)",es,BASS_ErrorGetCode());
	MessageBox(win,mes,0,0);
}

void CALLBACK LoopSyncProc(HSYNC handle, DWORD channel, DWORD data, void *user)
{
	if (!BASS_ChannelSetPosition(channel,loop[0],BASS_POS_BYTE)) // try seeking to loop start
		BASS_ChannelSetPosition(channel,0,BASS_POS_BYTE); // failed, go to start of file instead
}

void SetLoopStart(QWORD pos)
{
	loop[0]=pos;
}

void SetLoopEnd(QWORD pos)
{
	loop[1]=pos;
	BASS_ChannelRemoveSync(chan,lsync); // remove old sync
	lsync=BASS_ChannelSetSync(chan,BASS_SYNC_POS|BASS_SYNC_MIXTIME,loop[1],LoopSyncProc,0); // set new sync
}

// scan the peaks
void __cdecl ScanPeaks(DWORD decoder)
{
	DWORD cpos=0,peak[2]={0};
	while (!killscan) {
		DWORD level=BASS_ChannelGetLevel(decoder); // scan peaks
		DWORD pos;
		if (peak[0]<LOWORD(level)) peak[0]=LOWORD(level); // set left peak
		if (peak[1]<HIWORD(level)) peak[1]=HIWORD(level); // set right peak
		if (!BASS_ChannelIsActive(decoder)) pos=-1; // reached the end
		else pos=BASS_ChannelGetPosition(decoder,BASS_POS_BYTE)/bpp;
		if (pos>cpos) {
			DWORD a;
			for (a=0;a<peak[0]*(HEIGHT/2)/32768;a++)
				wavebuf[(HEIGHT/2-1-a)*WIDTH+cpos]=1+a; // draw left peak
			for (a=0;a<peak[1]*(HEIGHT/2)/32768;a++)
				wavebuf[(HEIGHT/2+1+a)*WIDTH+cpos]=1+a; // draw right peak
			if (pos>=WIDTH) break; // gone off end of display
			cpos=pos;
			peak[0]=peak[1]=0;
		}
	}
	BASS_StreamFree(decoder); // free the decoder
	scanthread=0;
}

// select a file to play, and start scanning it
BOOL PlayFile()
{
	char file[MAX_PATH]="";
	OPENFILENAME ofn={0};
	ofn.lStructSize=sizeof(ofn);
	ofn.hwndOwner=win;
	ofn.nMaxFile=MAX_PATH;
	ofn.lpstrFile=file;
	ofn.Flags=OFN_FILEMUSTEXIST|OFN_HIDEREADONLY|OFN_EXPLORER;
	ofn.lpstrTitle="Select a file to play";
	ofn.lpstrFilter="Playable files\0*.mp3;*.mp2;*.mp1;*.ogg;*.wav;*.aif;*.mo3;*.it;*.xm;*.s3m;*.mtm;*.mod;*.umx\0All files\0*.*\0\0";
	if (!GetOpenFileName(&ofn)) return FALSE;

	if (!(chan=BASS_StreamCreateFile(FALSE,file,0,0,0))
		&& !(chan=BASS_MusicLoad(FALSE,file,0,0,BASS_MUSIC_RAMPS|BASS_MUSIC_POSRESET|BASS_MUSIC_PRESCAN,0))) {
		Error("Can't play file");
		return FALSE; // Can't load the file
	}
	{
		BYTE data[2000]={0};
		BITMAPINFOHEADER *bh=(BITMAPINFOHEADER*)data;
		RGBQUAD *pal=(RGBQUAD*)(data+sizeof(*bh));
		int a;
		bh->biSize=sizeof(*bh);
		bh->biWidth=WIDTH;
		bh->biHeight=-HEIGHT;
		bh->biPlanes=1;
		bh->biBitCount=8;
		bh->biClrUsed=bh->biClrImportant=HEIGHT/2+1;
		// setup palette
		for (a=1;a<=HEIGHT/2;a++) {
			pal[a].rgbRed=(255*a)/(HEIGHT/2);
			pal[a].rgbGreen=255-pal[a].rgbRed;
		}
		// create the bitmap
		wavebmp=CreateDIBSection(0,(BITMAPINFO*)bh,DIB_RGB_COLORS,(void**)&wavebuf,NULL,0);
		wavedc=CreateCompatibleDC(0);
		SelectObject(wavedc,wavebmp);
	}
	bpp=BASS_ChannelGetLength(chan,BASS_POS_BYTE)/WIDTH; // bytes per pixel
	if (bpp<BASS_ChannelSeconds2Bytes(chan,0.02)) // minimum 20ms per pixel (BASS_ChannelGetLevel scans 20ms)
		bpp=BASS_ChannelSeconds2Bytes(chan,0.02);
	BASS_ChannelSetSync(chan,BASS_SYNC_END|BASS_SYNC_MIXTIME,0,LoopSyncProc,0); // set sync to loop at end
	BASS_ChannelPlay(chan,FALSE); // start playing
	{ // start scanning peaks in a new thread
		DWORD chan2=BASS_StreamCreateFile(FALSE,file,0,0,BASS_STREAM_DECODE);
		if (!chan2) chan2=BASS_MusicLoad(FALSE,file,0,0,BASS_MUSIC_DECODE,0);
		scanthread=_beginthread(ScanPeaks,0,chan2);
	}
	return TRUE;
}

void DrawTimeLine(HDC dc, QWORD pos, DWORD col, DWORD y)
{
	HPEN pen=CreatePen(PS_SOLID,0,col),oldpen;
	DWORD wpos=pos/bpp;
	DWORD time=BASS_ChannelBytes2Seconds(chan,pos);
	char text[10];
	sprintf(text,"%u:%02u",time/60,time%60);
	oldpen=SelectObject(dc,pen);
	MoveToEx(dc,wpos,0,NULL);
	LineTo(dc,wpos,HEIGHT);
	SetTextColor(dc,col);
	SetBkMode(dc,TRANSPARENT);
	SetTextAlign(dc,wpos>=WIDTH/2?TA_RIGHT:TA_LEFT);
	TextOut(dc,wpos,y,text,strlen(text));
	SelectObject(dc,oldpen);
	DeleteObject(pen);
}

// window procedure
long FAR PASCAL SpectrumWindowProc(HWND h, UINT m, WPARAM w, LPARAM l)
{
	switch (m) {
		case WM_LBUTTONDOWN: // set loop start
			SetLoopStart(LOWORD(l)*bpp);
			return 0;
		case WM_RBUTTONDOWN: // set loop end
			SetLoopEnd(LOWORD(l)*bpp);
			return 0;
		case WM_MOUSEMOVE:
			if (w&MK_LBUTTON) SetLoopStart(LOWORD(l)*bpp);
			if (w&MK_RBUTTON) SetLoopEnd(LOWORD(l)*bpp);
			return 0;

		case WM_TIMER:
			InvalidateRect(h,0,0); // refresh window
			return 0;
		case WM_PAINT:
			if (GetUpdateRect(h,0,0)) {
				PAINTSTRUCT p;
				HDC dc;
				if (!(dc=BeginPaint(h,&p))) return 0;
				BitBlt(dc,0,0,WIDTH,HEIGHT,wavedc,0,0,SRCCOPY); // draw peak waveform
				DrawTimeLine(dc,loop[0],0xffff00,12); // loop start
				DrawTimeLine(dc,loop[1],0x00ffff,24); // loop end
				DrawTimeLine(dc,BASS_ChannelGetPosition(chan,BASS_POS_BYTE),0xffffff,0); // current pos
				EndPaint(h,&p);
			}
			return 0;

		case WM_CREATE:
			win=h;
			// initialize BASS
			if (!BASS_Init(-1,44100,0,win,NULL)) {
				Error("Can't initialize device");
				return -1;
			}
			if (!PlayFile()) { // start a file playing
				BASS_Free();
				return -1;
			}
			SetTimer(h,0,100,0); // set update timer (10hz)
			break;

		case WM_DESTROY:
			KillTimer(h,0);
			if (scanthread) { // still scanning
				killscan=TRUE;
				WaitForSingleObject(scanthread,1000); // wait for the thread
			}
			BASS_Free();
			if (wavedc) DeleteDC(wavedc);
			if (wavebmp) DeleteObject(wavebmp);
			PostQuitMessage(0);
			break;
	}
	return DefWindowProc(h, m, w, l);
}

int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,LPSTR lpCmdLine, int nCmdShow)
{
	WNDCLASS wc;
    MSG msg;

	// check the correct BASS was loaded
	if (HIWORD(BASS_GetVersion())!=BASSVERSION) {
		MessageBox(0,"An incorrect version of BASS.DLL was loaded",0,MB_ICONERROR);
		return 0;
	}

	// register window class and create the window
	memset(&wc,0,sizeof(wc));
	wc.lpfnWndProc = SpectrumWindowProc;
	wc.hInstance = hInstance;
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.lpszClassName = "BASS-CustLoop";
	if (!RegisterClass(&wc) || !CreateWindow("BASS-CustLoop",
			"BASS custom looping example (left-click to set loop start, right-click to set end)",
			WS_POPUPWINDOW|WS_CAPTION|WS_VISIBLE, 100, 100,
			WIDTH+2*GetSystemMetrics(SM_CXDLGFRAME),
			HEIGHT+GetSystemMetrics(SM_CYCAPTION)+2*GetSystemMetrics(SM_CYDLGFRAME),
			NULL, NULL, hInstance, NULL)) {
		Error("Can't create window");
		return 0;
	}
	ShowWindow(win, SW_SHOWNORMAL);

	while (GetMessage(&msg,NULL,0,0)>0) {
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

	return 0;
}
