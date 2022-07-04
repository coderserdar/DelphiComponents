/*
	BASSMIDI test player
	Copyright (c) 2006-2009 Un4seen Developments Ltd.
*/

#include <windows.h>
#include <commctrl.h>
#include <stdio.h>
#include <math.h>
#include "bass.h"
#include "bassmidi.h"

HWND win=NULL;

HSTREAM chan;		// channel handle
HSOUNDFONT font;	// soundfont

int miditempo;		// MIDI file tempo
float temposcale=1;	// tempo adjustment

OPENFILENAME ofn;

char lyrics[1000]; // lyrics buffer

// display error messages
void Error(const char *es)
{
	char mes[200];
	sprintf(mes,"%s\n(error code: %d)",es,BASS_ErrorGetCode());
	MessageBox(win,mes,0,0);
}

#define MESS(id,m,w,l) SendDlgItemMessage(win,id,m,(WPARAM)(w),(LPARAM)(l))

void CALLBACK LyricSync(HSYNC handle, DWORD channel, DWORD data, void *user)
{
	BASS_MIDI_MARK mark;
	const char *text;
	int lines;
	BASS_MIDI_StreamGetMark(channel,(DWORD)user,data,&mark); // get the lyric/text
	text=mark.text;
	if (text[0]=='@') return; // skip info
	if (text[0]=='\\') // clear display
		strcpy(lyrics,text+1);
	else {
		if (text[0]=='/') { // new line
			strcat(lyrics,"\n");
			text++;
		}
		strcat(lyrics,text);
	}
	for (lines=1,text=lyrics;text=strchr(text,'\n');lines++,text++) ; // count lines
	if (lines>3) { // remove old lines so that new lines fit in display...
		int a;
		for (a=0,text=lyrics;a<lines-3;a++) text=strchr(text,'\n')+1;
		strcpy(lyrics,text);
	}
	MESS(30,WM_SETTEXT,0,lyrics);
}

void CALLBACK EndSync(HSYNC handle, DWORD channel, DWORD data, void *user)
{
	lyrics[0]=0; // clear lyrics
	MESS(30,WM_SETTEXT,0,lyrics);
}

void SetTempo(BOOL reset)
{
	if (reset) miditempo=BASS_MIDI_StreamGetEvent(chan,0,MIDI_EVENT_TEMPO); // get the file's tempo
	BASS_MIDI_StreamEvent(chan,0,MIDI_EVENT_TEMPO,miditempo*temposcale); // set tempo
}

void CALLBACK TempoSync(HSYNC handle, DWORD channel, DWORD data, void *user)
{
	SetTempo(TRUE); // override the tempo
}

// look for a marker (eg. loop points)
BOOL FindMarker(HSTREAM handle, const char *text, BASS_MIDI_MARK *mark)
{
	int a;
	for (a=0;BASS_MIDI_StreamGetMark(handle,BASS_MIDI_MARK_MARKER,a,mark);a++) {
		if (!stricmp(mark->text,text)) return TRUE; // found it
	}
	return FALSE;
}

void CALLBACK LoopSync(HSYNC handle, DWORD channel, DWORD data, void *user)
{
	BASS_MIDI_MARK mark;
	if (FindMarker(channel,"loopstart",&mark)) // found a loop start point
		BASS_ChannelSetPosition(channel,mark.pos,BASS_POS_BYTE|BASS_MIDI_DECAYSEEK); // rewind to it (and let old notes decay)
	else
		BASS_ChannelSetPosition(channel,0,BASS_POS_BYTE|BASS_MIDI_DECAYSEEK); // else rewind to the beginning instead
}

BOOL CALLBACK dialogproc(HWND h,UINT m,WPARAM w,LPARAM l)
{
	switch (m) {
		case WM_COMMAND:
			switch (LOWORD(w)) {
				case IDCANCEL:
					DestroyWindow(h);
					break;
				case 10:
					{
						char file[MAX_PATH]="";
						ofn.lpstrFilter="MIDI files (mid/midi/rmi/kar)\0*.mid;*.midi;*.rmi;*.kar\0All files\0*.*\0\0";
						ofn.lpstrFile=file;
						if (GetOpenFileName(&ofn)) {
							BASS_StreamFree(chan); // free old stream before opening new
							MESS(30,WM_SETTEXT,0,""); // clear lyrics display
							if (!(chan=BASS_MIDI_StreamCreateFile(FALSE,file,0,0,BASS_SAMPLE_LOOP|(MESS(20,BM_GETCHECK,0,0)?0:BASS_MIDI_NOFX),0))) {
								// it ain't a MIDI
								MESS(10,WM_SETTEXT,0,"click here to open a file...");
								MESS(11,WM_SETTEXT,0,"");
								Error("Can't play the file");
								break;
							}
							MESS(10,WM_SETTEXT,0,file);
							// set the title (first text in first track)
							MESS(11,WM_SETTEXT,0,BASS_ChannelGetTags(chan,BASS_TAG_MIDI_TRACK));
							// update pos scroller range (using tick length)
							MESS(21,TBM_SETRANGE,1,MAKELONG(0,BASS_ChannelGetLength(chan,BASS_POS_MIDI_TICK)/120));
							{ // set looping syncs
								BASS_MIDI_MARK mark;
								if (FindMarker(chan,"loopend",&mark)) // found a loop end point
									BASS_ChannelSetSync(chan,BASS_SYNC_POS|BASS_SYNC_MIXTIME,mark.pos,LoopSync,0); // set a sync there
								BASS_ChannelSetSync(chan,BASS_SYNC_END|BASS_SYNC_MIXTIME,0,LoopSync,0); // set one at the end too (eg. in case of seeking past the loop point)
							}
							{ // clear lyrics buffer and set lyrics syncs
								BASS_MIDI_MARK mark;
								lyrics[0]=0;
								if (BASS_MIDI_StreamGetMark(chan,BASS_MIDI_MARK_LYRIC,0,&mark)) // got lyrics
									BASS_ChannelSetSync(chan,BASS_SYNC_MIDI_LYRIC,0,LyricSync,(void*)BASS_MIDI_MARK_LYRIC);
								else if (BASS_MIDI_StreamGetMark(chan,BASS_MIDI_MARK_TEXT,20,&mark)) // got text instead (over 20 of them)
									BASS_ChannelSetSync(chan,BASS_SYNC_MIDI_TEXT,0,LyricSync,(void*)BASS_MIDI_MARK_TEXT);
								BASS_ChannelSetSync(chan,BASS_SYNC_END,0,EndSync,0);
							}
							{ // override the initial tempo, and set a sync to override tempo events and another to override after seeking
								SetTempo(TRUE);
								BASS_ChannelSetSync(chan,BASS_SYNC_MIDI_EVENT|BASS_SYNC_MIXTIME,MIDI_EVENT_TEMPO,TempoSync,0);
								BASS_ChannelSetSync(chan,BASS_SYNC_SETPOS|BASS_SYNC_MIXTIME,0,TempoSync,0);
							}
							{ // get default soundfont in case of matching soundfont being used
								BASS_MIDI_FONT sf;
								BASS_MIDI_StreamGetFonts(chan,&sf,1);
								font=sf.font;
							}
							BASS_ChannelPlay(chan,FALSE);
						}
					}
					break;
				case 20:
					{ // toggle FX processing
						if (MESS(20,BM_GETCHECK,0,0))
							BASS_ChannelFlags(chan,0,BASS_MIDI_NOFX); // enable FX
						else
							BASS_ChannelFlags(chan,BASS_MIDI_NOFX,BASS_MIDI_NOFX); // disable FX
					}
					break;
				case 40:
					{
						char file[MAX_PATH]="";
						ofn.lpstrFilter="Soundfonts (sf2/sf2pack)\0*.sf2;*.sf2pack\0All files\0*.*\0\0";
						ofn.lpstrFile=file;
						if (GetOpenFileName(&ofn)) {
							HSOUNDFONT newfont;
							if ((newfont=BASS_MIDI_FontInit(file,0)) && newfont!=font) {
								BASS_MIDI_FONT sf;
								sf.font=newfont;
								sf.preset=-1; // use all presets
								sf.bank=0; // use default bank(s)
								BASS_MIDI_StreamSetFonts(0,&sf,1); // set default soundfont
								BASS_MIDI_StreamSetFonts(chan,&sf,1); // set for current stream too
								BASS_MIDI_FontFree(font); // free old soundfont
								font=newfont;
							}
						}
					}
					break;
			}
			break;

		case WM_HSCROLL:
			if (l && LOWORD(w)!=SB_THUMBPOSITION && LOWORD(w)!=SB_ENDSCROLL) { // set the position
				int pos=SendMessage((HWND)l,TBM_GETPOS,0,0);
				BASS_ChannelSetPosition(chan,pos*120,BASS_POS_MIDI_TICK);
				// clear lyrics
				lyrics[0]=0;
				MESS(30,WM_SETTEXT,0,"");
			}
			break;

		case WM_VSCROLL:
			if (l && LOWORD(w)!=SB_THUMBPOSITION && LOWORD(w)!=SB_ENDSCROLL) {
				int p=SendMessage((HWND)l,TBM_GETPOS,0,0); // get tempo slider pos
				temposcale=1/((30-p)/20.f); // up to +/- 50% bpm
				SetTempo(FALSE); // apply the tempo adjustment
			}
			break;

		case WM_TIMER:
			if (chan) {
				char text[10];
				MESS(21,TBM_SETPOS,1,(DWORD)BASS_ChannelGetPosition(chan,BASS_POS_MIDI_TICK)/120); // update position
				sprintf(text,"%.1f",60000000/(miditempo*temposcale)); // calculate bpm
				MESS(23,WM_SETTEXT,0,text); // display it
			}
			{
				static int updatefont=0;
				if (++updatefont&1) { // only updating font info once a second
					char text[40]="no soundfont";
					BASS_MIDI_FONTINFO i;
					if (BASS_MIDI_FontGetInfo(font,&i))
						sprintf(text,"name: %s\nloaded: %d / %d",i.name,i.samload,i.samsize);
					MESS(41,WM_SETTEXT,0,text);
				}
			}
			break;

		case WM_INITDIALOG:
			win=h;
			memset(&ofn,0,sizeof(ofn));
			ofn.lStructSize=sizeof(ofn);
			ofn.hwndOwner=h;
			ofn.nMaxFile=MAX_PATH;
			ofn.Flags=OFN_HIDEREADONLY|OFN_EXPLORER;
			// setup output - default device
			if (!BASS_Init(-1,44100,0,win,NULL)) {
				Error("Can't initialize device");
				DestroyWindow(win);
				break;
			}
			{ // get default font (28mbgm.sf2/ct8mgm.sf2/ct4mgm.sf2/ct2mgm.sf2 if available)
				BASS_MIDI_FONT sf;
				if (BASS_MIDI_StreamGetFonts(0,&sf,1))
					font=sf.font;
			}
			MESS(20,BM_SETCHECK,BST_CHECKED,0); // FX enabled by default
			MESS(22,TBM_SETRANGE,0,MAKELONG(0,20)); // set tempo slider range
			MESS(22,TBM_SETTIC,0,10); // add tick at default pos
			MESS(22,TBM_SETPOS,1,10); // set slider to centre (default)
			SetTimer(h,0,500,0); // timer to update the position
			// load optional plugins for packed soundfonts (others may be used too)
			BASS_PluginLoad("bassflac.dll",0);
			BASS_PluginLoad("basswv.dll",0);
			return 1;

		case WM_DESTROY:
			BASS_Free();
			BASS_PluginFree(0);
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

	{ // enable trackbar support
		INITCOMMONCONTROLSEX cc={sizeof(cc),ICC_BAR_CLASSES};
		InitCommonControlsEx(&cc);
	}

	DialogBox(hInstance,(char*)1000,0,&dialogproc);

	return 0;
}
