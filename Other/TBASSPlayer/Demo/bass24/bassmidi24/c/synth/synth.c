/*
	BASSMIDI simple synth
	Copyright (c) 2006-2008 Un4seen Developments Ltd.
*/

#include <windows.h>
#include <stdio.h>
#include <conio.h>
#include <math.h>
#include "bass.h"
#include "bassmidi.h"

// display error messages
void Error(const char *text) 
{
	printf("Error(%d): %s\n",BASS_ErrorGetCode(),text);
	BASS_Free();
	ExitProcess(0);
}

#define KEYS 20
const WORD keys[KEYS]={
	'Q','2','W','3','E','R','5','T','6','Y','7','U',
	'I','9','O','0','P',219,187,221
};

void main(int argc, char **argv)
{
	BASS_INFO info;
	HSTREAM str;
	const char *fxname[9]={"CHORUS","COMPRESSOR","DISTORTION","ECHO",
		"FLANGER","GARGLE","I3DL2REVERB","PARAMEQ","REVERB"};
	HFX fx[9]={0}; // effect handles
	INPUT_RECORD keyin;
	DWORD r, buflen, pressed[KEYS]={0}, preset=0, drums=0;

	printf("BASSMIDI Simple Synth\n"
			"---------------------\n");

	// check the correct BASS was loaded
	if (HIWORD(BASS_GetVersion())!=BASSVERSION) {
		printf("An incorrect version of BASS.DLL was loaded");
		return;
	}

	// load/check soundfont
	if (argc>1) {
		BASS_MIDI_FONT font;
		printf("loading soundfont... ");
		if (font.font=BASS_MIDI_FontInit(argv[1],0)) { // initialized font
			font.preset=-1; // all presets
			font.bank=0; // default bank(s)
			BASS_MIDI_StreamSetFonts(0,&font,1); // make it the default
			printf("ok\n");
		} else
			printf("failed\n");
	} else
		printf("no soundfont provided (using Creative font if available)\n");
	if (!BASS_MIDI_StreamGetFonts(0,0,0)) { // no soundfont loaded...
		printf("no soundfont loaded, please provide one in the command-line\n");
		return;
	}

	// 10ms update period
	BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD,10);

	// setup output - get latency
	if (!BASS_Init(-1,44100,BASS_DEVICE_LATENCY,0,NULL))
		Error("Can't initialize device");

	BASS_GetInfo(&info);
	printf("device latency: %dms\n",info.latency);
	printf("device minbuf: %dms\n",info.minbuf);
	printf("ds version: %d (effects %s)\n",info.dsver,info.dsver<8?"disabled":"enabled");

	// default buffer size = update period + 'minbuf'
	BASS_SetConfig(BASS_CONFIG_BUFFER,10+info.minbuf);
	buflen=BASS_GetConfig(BASS_CONFIG_BUFFER);

	// create a MIDI stream
	str=BASS_MIDI_StreamCreate(1,BASS_MIDI_NOFX,44100); // not using reverb/chorus effects
	printf("press these keys to play:\n\n"
			"  2 3  5 6 7  9 0  =\n"
			" Q W ER T Y UI O P[ ]\n\n"
			"press -/+ to de/increase the buffer\n"
			"press F11/F12 to change preset\n"
			"press enter to toggle drums\n"
			"press spacebar to quit\n\n");

	if (info.dsver>=8) // DX8 effects available
		printf("press F1-F9 to toggle effects\n\n");

	printf("using a %dms buffer\r",buflen);

	BASS_ChannelPlay(str,FALSE);

	while (ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE),&keyin,1,&r)) {
		int key;
		if (keyin.EventType!=KEY_EVENT) continue;
		if (keyin.Event.KeyEvent.wVirtualKeyCode==VK_SPACE) break;
		if (keyin.Event.KeyEvent.bKeyDown) {
			if (keyin.Event.KeyEvent.wVirtualKeyCode==VK_SUBTRACT
				|| keyin.Event.KeyEvent.wVirtualKeyCode==VK_ADD) {
				// recreate stream with smaller/larger buffer
				BASS_StreamFree(str);
				if (keyin.Event.KeyEvent.wVirtualKeyCode==VK_SUBTRACT)
					BASS_SetConfig(BASS_CONFIG_BUFFER,buflen-1); // smaller buffer
				else 
					BASS_SetConfig(BASS_CONFIG_BUFFER,buflen+1); // larger buffer
				buflen=BASS_GetConfig(BASS_CONFIG_BUFFER);
				printf("using a %dms buffer\t\t\r",buflen);
				str=BASS_MIDI_StreamCreate(1,BASS_MIDI_NOFX,44100);
				// set preset/drums/effects on the new stream
				BASS_MIDI_StreamEvent(str,0,MIDI_EVENT_PROGRAM,preset);
				BASS_MIDI_StreamEvent(str,0,MIDI_EVENT_DRUMS,drums);
				for (r=0;r<9;r++) if (fx[r]) fx[r]=BASS_ChannelSetFX(str,BASS_FX_DX8_CHORUS+r,0);
				BASS_ChannelPlay(str,FALSE);
			}
			if (keyin.Event.KeyEvent.wVirtualKeyCode==VK_F11 && preset>0) { // previous preset
				BASS_MIDI_StreamEvent(str,0,MIDI_EVENT_PROGRAM,--preset);
				BASS_MIDI_FontCompact(0); // unload unused samples
				printf("preset = %d\t\t\r",preset);
			}
			if (keyin.Event.KeyEvent.wVirtualKeyCode==VK_F12 && preset<127) { // next preset
				BASS_MIDI_StreamEvent(str,0,MIDI_EVENT_PROGRAM,++preset);
				BASS_MIDI_FontCompact(0); // unload unused samples
				printf("preset = %d\t\t\r",preset);
			}
			if (keyin.Event.KeyEvent.wVirtualKeyCode==VK_RETURN) { // toggle drums
				drums^=1;
				BASS_MIDI_StreamEvent(str,0,MIDI_EVENT_DRUMS,drums);
				BASS_MIDI_FontCompact(0); // unload unused samples
				printf("drums = %s\t\t\r",drums?"ON":"OFF");
			}
			if (keyin.Event.KeyEvent.wVirtualKeyCode>=VK_F1
				&& keyin.Event.KeyEvent.wVirtualKeyCode<=VK_F9) {
				r=keyin.Event.KeyEvent.wVirtualKeyCode-VK_F1;
				if (fx[r]) {
					BASS_ChannelRemoveFX(str,fx[r]);
					fx[r]=0;
					printf("effect %s = OFF\t\t\r",fxname[r]);
				} else {
					// set the effect, not bothering with parameters (use defaults)
					if (fx[r]=BASS_ChannelSetFX(str,BASS_FX_DX8_CHORUS+r,0))
						printf("effect %s = ON\t\t\r",fxname[r]);
				}
			}
		}
		for (key=0;key<KEYS;key++)
			if (keyin.Event.KeyEvent.wVirtualKeyCode==keys[key]) {
				if (keyin.Event.KeyEvent.bKeyDown && !pressed[key]) {
					pressed[key]=BASS_MIDI_StreamEvent(str,0,MIDI_EVENT_NOTE,MAKEWORD((drums?36:60)+key,100));
				} else if (!keyin.Event.KeyEvent.bKeyDown && pressed[key]) {
					BASS_MIDI_StreamEvent(str,0,MIDI_EVENT_NOTE,(drums?36:60)+key);
					pressed[key]=0;
				}
				break;
			}
	}

	BASS_Free();
}
