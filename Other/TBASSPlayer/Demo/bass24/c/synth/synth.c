/*
	BASS simple synth
	Copyright (c) 2001-2008 Un4seen Developments Ltd.
*/

#include <windows.h>
#include <stdio.h>
#include <conio.h>
#include <math.h>
#include "bass.h"

// display error messages
void Error(const char *text) 
{
	printf("Error(%d): %s\n",BASS_ErrorGetCode(),text);
	BASS_Free();
	ExitProcess(0);
}

#define PI 3.14159265358979323846
#define TABLESIZE 2048
int sinetable[TABLESIZE];	// sine table
#define KEYS 20
const WORD keys[KEYS]={
	'Q','2','W','3','E','R','5','T','6','Y','7','U',
	'I','9','O','0','P',219,187,221
};
#define MAXVOL	4000	// higher value = longer fadeout
int vol[KEYS]={0},pos[KEYS];	// keys' volume & pos

// stream writer
DWORD CALLBACK WriteStream(HSTREAM handle, short *buffer, DWORD length, void *user)
{
	int n,s;
	DWORD c;
	float f;
	memset(buffer,0,length);
	for (n=0;n<KEYS;n++) {
		if (!vol[n]) continue;
		f=pow(2.0,(n+3)/12.0)*TABLESIZE*440.0/44100.0;
		for (c=0;c<length/4 && vol[n];c++) {
			s=sinetable[(int)((pos[n]++)*f)&(TABLESIZE-1)]*vol[n]/MAXVOL;
			s+=(int)buffer[c*2];
			if (s>32767) s=32767;
			else if (s<-32768) s=-32768;
			buffer[c*2+1]=buffer[c*2]=s; // left and right channels are the same
			if (vol[n]<MAXVOL) vol[n]--;
		}
	}
	return length;
}

void main(int argc, char **argv)
{
	BASS_INFO info;
	HSTREAM str;
	const char *fxname[9]={"CHORUS","COMPRESSOR","DISTORTION","ECHO",
		"FLANGER","GARGLE","I3DL2REVERB","PARAMEQ","REVERB"};
	HFX fx[9]={0}; // effect handles
	INPUT_RECORD keyin;
	DWORD r,buflen;

	printf("BASS Simple Sinewave Synth\n"
			"--------------------------\n");

	// check the correct BASS was loaded
	if (HIWORD(BASS_GetVersion())!=BASSVERSION) {
		printf("An incorrect version of BASS.DLL was loaded");
		return;
	}

	// 10ms update period
	BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD,10);

	// setup output - get latency
	if (!BASS_Init(-1,44100,BASS_DEVICE_LATENCY,0,NULL))
		Error("Can't initialize device");

	// build sine table
	for (r=0;r<TABLESIZE;r++)
		sinetable[r]=(int)(sin(2.0*PI*(double)r/TABLESIZE)*7000.0);

	BASS_GetInfo(&info);
	printf("device latency: %dms\n",info.latency);
	printf("device minbuf: %dms\n",info.minbuf);
	printf("ds version: %d (effects %s)\n",info.dsver,info.dsver<8?"disabled":"enabled");

	// default buffer size = update period + 'minbuf'
	BASS_SetConfig(BASS_CONFIG_BUFFER,10+info.minbuf);
	buflen=BASS_GetConfig(BASS_CONFIG_BUFFER);

	// create a stream, stereo so that effects sound nice
	str=BASS_StreamCreate(44100,2,0,(STREAMPROC*)WriteStream,0);
	printf("press these keys to play:\n\n"
			"  2 3  5 6 7  9 0  =\n"
			" Q W ER T Y UI O P[ ]\n\n"
			"press -/+ to de/increase the buffer\n"
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
				str=BASS_StreamCreate(44100,2,0,(STREAMPROC*)WriteStream,0);
				// set effects on the new stream
				for (r=0;r<9;r++) if (fx[r]) fx[r]=BASS_ChannelSetFX(str,BASS_FX_DX8_CHORUS+r,0);
				BASS_ChannelPlay(str,FALSE);
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
				if (keyin.Event.KeyEvent.bKeyDown && vol[key]!=MAXVOL) {
					pos[key]=0;
					vol[key]=MAXVOL; // start key
				} else if (!keyin.Event.KeyEvent.bKeyDown && vol[key])
					vol[key]--; // trigger key fadeout
				break;
			}
	}

	BASS_Free();
}
