/*
	BASSCD test player
	Copyright (c) 2003-2010 Un4seen Developments Ltd.
*/

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <commctrl.h>
#include "basscd.h"

HWND win=NULL;
DWORD cdiface;
#define MAXDRIVES 10
int curdrive=0;
HSTREAM stream[MAXDRIVES]={0};

// Display error dialogs
void Error(const char *es)
{
	char mes[200];
	sprintf(mes,"%s\n(error code: %d)",es,BASS_ErrorGetCode());
	MessageBox(win,mes,0,0);
}

// Messaging macro
#define MESS(id,m,w,l) SendDlgItemMessage(win,id,m,(WPARAM)(w),(LPARAM)(l))
#define ITEM(id) GetDlgItem(win,id)

void PlayTrack(DWORD drive, DWORD track);

// End sync
void CALLBACK EndSync(HSYNC handle, DWORD channel, DWORD data, void *user)
{
	if (MESS(13,BM_GETCHECK,0,0)) { // advance onto next track
		DWORD track=BASS_CD_StreamGetTrack(channel);
		DWORD drive=HIWORD(track);
		DWORD tracks=BASS_CD_GetTracks(drive);
		if (tracks==-1) return; // error, eg. CD removed?
		track=(LOWORD(track)+1)%tracks;
		if (drive==curdrive) MESS(11,LB_SETCURSEL,track,0);
		PlayTrack(drive,track);
	}
}

void PlayTrack(DWORD drive, DWORD track)
{
	if (stream[drive]) { // already have a stream, so just set the track
		if (!BASS_CD_StreamSetTrack(stream[drive],track))
			return; // failed... data track?
	} else {
		stream[drive]=BASS_CD_StreamCreate(drive,track,0); // create stream
		BASS_ChannelSetSync(stream[drive],BASS_SYNC_END,0,&EndSync,0); // set end sync
	}
	if (drive==curdrive)
		MESS(30,TBM_SETRANGEMAX,0,BASS_ChannelGetLength(stream[drive],BASS_POS_BYTE)/176400); // set pos scroller range
	BASS_ChannelPlay(stream[drive],FALSE); // start playing
}

void UpdateTrackList()
{
	char text[100],tag[10];
	const char *cdtext;
	int a,tc;
	MESS(11,LB_RESETCONTENT,0,0);
	tc=BASS_CD_GetTracks(curdrive);
	if (tc==-1) return; // no CD
	cdtext=BASS_CD_GetID(curdrive,BASS_CDID_TEXT); // get CD-TEXT
	for (a=0;a<tc;a++) {
		int l=BASS_CD_GetTrackLength(curdrive,a);
		sprintf(text,"Track %02d",a+1);
		if (cdtext) {
			const char *t=cdtext;
			int taglen=sprintf(tag,"TITLE%d=",a+1); // the CD-TEXT tag to look for
			while (*t) {
				if (!memcmp(t,tag,taglen)) { // found the track title...
					strcpy(text,t+taglen); // replace "track x" with title
					break;
				}
				t+=strlen(t)+1;
			}
		}
		if (l==-1) // data track
			strcat(text," (data)");
		else {
			l/=176400;
			sprintf(text+strlen(text)," (%d:%02d)",l/60,l%60);
		}
		MESS(11,LB_ADDSTRING,0,text);
	}
	a=BASS_CD_StreamGetTrack(stream[curdrive]);
	if (a!=-1) { // this drive has a stream
		MESS(11,LB_SETCURSEL,LOWORD(a),0); // select current track
		MESS(30,TBM_SETRANGEMAX,0,BASS_ChannelGetLength(stream[curdrive],BASS_POS_BYTE)/176400); // set pos scroller range
	}
	{
		float vol=1,spd=44100;
		BASS_ChannelGetAttribute(stream[curdrive],BASS_ATTRIB_FREQ,&spd);
		BASS_ChannelGetAttribute(stream[curdrive],BASS_ATTRIB_VOL,&vol);
		MESS(31,TBM_SETPOS,TRUE,(1-vol)*100); // set volume slider pos
		MESS(32,TBM_SETPOS,TRUE,spd/441); // set speed slider pos
		sprintf(text,"Speed - %d%%",(int)spd/441);
		MESS(50,WM_SETTEXT,0,text);
	}
}

void UpdateDriveInfo()
{
	char buf[100]="-";
	BASS_CD_INFO cdi;
	if (BASS_CD_GetInfo(curdrive,&cdi))
		sprintf(buf,"rwflags = %08X, maxspeed = %d, cache = %d, cdtext = %d",cdi.rwflags,cdi.maxspeed,cdi.cache,cdi.cdtext);
	MESS(18,WM_SETTEXT,0,buf);
}

// Get list of available drives
void SetupDriveList()
{
	int a=0;
	BASS_CD_INFO cdi;
	MESS(10,CB_RESETCONTENT,0,0); // clear list
	while (a<MAXDRIVES && BASS_CD_GetInfo(a,&cdi)) {
		char text[100];
		sprintf(text,"%c: %s %s v%s",'A'+cdi.letter,cdi.vendor,cdi.product,cdi.rev); // "letter: description"
		MESS(10,CB_ADDSTRING,0,text);
		a++;
	}
	if (curdrive>=a) curdrive=0;
	MESS(10,CB_SETCURSEL,curdrive,0); // select current drive
	UpdateDriveInfo();
	UpdateTrackList();
}

BOOL CALLBACK dialogproc(HWND h,UINT m,WPARAM w,LPARAM l)
{
	static int seeking=-1;
	switch (m) {
		case WM_TIMER:
			{
				// update levels
				static int updatecount=0,levl=0,levr=0;
				int level=BASS_ChannelGetLevel(stream[curdrive]);
				levl-=1500; if (levl<0) levl=0;
				levr-=1500; if (levr<0) levr=0;
				if (level!=-1) {
					if (levl<LOWORD(level)) levl=LOWORD(level);
					if (levr<HIWORD(level)) levr=HIWORD(level);
				}
				MESS(40,PBM_SETPOS,levl,0); // left
				MESS(41,PBM_SETPOS,levr,0); // right
				if (!(updatecount++&3)) { // do other stuff (only every 4th visit)
					char time[20]="-";
					MESS(14,BM_SETCHECK,BASS_CD_DoorIsOpen(curdrive),0); // update "open" status
					MESS(15,BM_SETCHECK,BASS_CD_DoorIsLocked(curdrive),0); // update "lock" status
					if (BASS_ChannelIsActive(stream[curdrive])) { // playing - update info
						DWORD p=seeking;
						if (p==-1) { // not seeking - update pos scroller
							p=BASS_ChannelBytes2Seconds(stream[curdrive],BASS_ChannelGetPosition(stream[curdrive],BASS_POS_BYTE));
							MESS(30,TBM_SETPOS,TRUE,p);
						}
						sprintf(time,"%d - %d:%02d",LOWORD(BASS_CD_StreamGetTrack(stream[curdrive]))+1,p/60,p%60);
					} else {
						if (!MESS(11,LB_GETCOUNT,0,0)) { // empty track list - refresh
							if (BASS_CD_IsReady(curdrive)) UpdateTrackList();
						} else if (!BASS_CD_IsReady(curdrive)) { // no CD - free stream & clear list
							BASS_StreamFree(stream[curdrive]);
							stream[curdrive]=0;
							MESS(11,LB_RESETCONTENT,0,0);
						}
						BASS_CD_Release(curdrive); // release the drive to allow others to access it
					}
					MESS(22,WM_SETTEXT,0,time);
				}
			}
			break;

		case WM_COMMAND:
			switch (LOWORD(w)) {
				case IDCANCEL:
					EndDialog(h,0);
					return 1;
				case 10:
					if (HIWORD(w)==CBN_SELCHANGE) { // change current drive
						curdrive=MESS(10,CB_GETCURSEL,0,0);
						UpdateDriveInfo();
						UpdateTrackList();
					}
					return 1;
				case 11:
					if (HIWORD(w)==LBN_SELCHANGE) { // change playing track
						DWORD track=MESS(11,LB_GETCURSEL,0,0);
						PlayTrack(curdrive,track);
					}
					return 1;
				case 12: // play/pause
					if (BASS_ChannelIsActive(stream[curdrive])==BASS_ACTIVE_PLAYING)
						BASS_ChannelPause(stream[curdrive]);
					else
						BASS_ChannelPlay(stream[curdrive],FALSE);
					return 1;
				case 14: // open/close door
					BASS_CD_Door(curdrive,BASS_CD_DoorIsOpen(curdrive)?BASS_CD_DOOR_CLOSE:BASS_CD_DOOR_OPEN);
					break;
				case 15: // lock/unlock door
					BASS_CD_Door(curdrive,BASS_CD_DoorIsLocked(curdrive)?BASS_CD_DOOR_UNLOCK:BASS_CD_DOOR_LOCK);
					break;
				case 19:
					if (HIWORD(w)==CBN_SELCHANGE) { // change CD interface
						cdiface=BASS_CD_SetInterface(MESS(19,CB_GETCURSEL,0,0));
						MESS(19,CB_SETCURSEL,cdiface,0);
						stream[curdrive]=0; // stream will be freed by interface change
						SetupDriveList(); // refresh drive list
					}
					break;
			}
			break;

		case WM_HSCROLL:
			if (GetDlgCtrlID((HWND)l)==30) { // position scroller
				seeking=MESS(30,TBM_GETPOS,0,0);;
				if (LOWORD(w)==SB_ENDSCROLL) { // seek to new pos
					BASS_ChannelSetPosition(stream[curdrive],seeking*176400,BASS_POS_BYTE);
					seeking=-1;
				}
			} else { // must be speed slider then
				char text[20];
				DWORD spd=MESS(32,TBM_GETPOS,0,0);
				BASS_ChannelSetAttribute(stream[curdrive],BASS_ATTRIB_FREQ,spd*441);
				sprintf(text,"Speed - %d%%",spd);
				MESS(50,WM_SETTEXT,0,text);
			}
			return 1;

		case WM_VSCROLL:
			{ // adjust volume
				DWORD vol=MESS(31,TBM_GETPOS,0,0);
				BASS_ChannelSetAttribute(stream[curdrive],BASS_ATTRIB_VOL,1-vol/100.f);
			}
			return 1;

		case WM_INITDIALOG:
			win=h;
			// Setup output - default device
			if (!BASS_Init(-1,44100,0,win,NULL)) {
				Error("Can't initialize device");
				EndDialog(h,0);
				return 0;
			}
			// setup interface selector
			MESS(19,CB_ADDSTRING,0,"auto");
			MESS(19,CB_ADDSTRING,0,"SPTI");
			MESS(19,CB_ADDSTRING,0,"ASPI");
			MESS(19,CB_ADDSTRING,0,"WIO");
			MESS(19,CB_SETCURSEL,cdiface,0);
			// setup scroller/bar stuff
			MESS(30,TBM_SETLINESIZE,0,5);
			MESS(30,TBM_SETPAGESIZE,0,30);
			MESS(31,TBM_SETRANGEMAX,0,100);
			MESS(31,TBM_SETLINESIZE,0,1);
			MESS(32,TBM_SETRANGEMIN,0,50);
			MESS(32,TBM_SETRANGEMAX,0,150);
			MESS(32,TBM_SETLINESIZE,0,1);
			MESS(32,TBM_SETPAGESIZE,0,10);
			MESS(40,PBM_SETRANGE32,0,30000);
			MESS(41,PBM_SETRANGE32,0,30000);
			SetupDriveList();
			SetTimer(h,1,50,NULL);
			return 1;

		case WM_DESTROY:
			KillTimer(h,1);
			BASS_Free();
			return 1;
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

	{ // enable trackbar support
		INITCOMMONCONTROLSEX cc={sizeof(cc),ICC_BAR_CLASSES};
		InitCommonControlsEx(&cc);
	}

	// initialize CD access (auto-detect interface)
	cdiface=BASS_CD_SetInterface(BASS_CD_IF_AUTO);

	// display the window
	DialogBox(hInstance,MAKEINTRESOURCE(1000),NULL,&dialogproc);

	return 0;
}
