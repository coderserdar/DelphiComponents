/*
	CD version of the BASS console WAVE writer
	Copyright (c) 2002-2008 Un4seen Developments Ltd.
*/

#include <windows.h>
#include <stdio.h>
#include <conio.h>
#include "bass.h"
#include "basscd.h"

/* display error messages */
void Error(char *text) 
{
	printf("Error(%d): %s\n",BASS_ErrorGetCode(),text);
	BASS_Free();
	ExitProcess(0);
}

void main(int argc, char **argv)
{
	DWORD chan,p;
	QWORD pos;
	FILE *fp;
	BYTE buf[20000];
	WAVEFORMATEX wf;

	printf("BASSCD WAVE writer example : CDA -> BASS.WAV\n"
			"--------------------------------------------\n");

	// check the correct BASS was loaded
	if (HIWORD(BASS_GetVersion())!=BASSVERSION) {
		printf("An incorrect version of BASS was loaded");
		return;
	}

	if (argc!=2) {
		printf("\tusage: writewav <file>\n");
		return;
	}

	/* not playing anything, so don't need an update thread */
	BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD,0);

	/* setup output - "no sound" device */
	if (!BASS_Init(0,44100,0,0,NULL))
		Error("Can't initialize device");

	/* try streaming the track */
	if (!(chan=BASS_CD_StreamCreateFile(argv[1],BASS_STREAM_DECODE)))
		Error("Can't play the track");
	pos=BASS_ChannelGetLength(chan,BASS_POS_BYTE);
	printf("streaming track [%I64u bytes]",pos);

	/* display the time length */
	if (pos) {
		p=(DWORD)BASS_ChannelBytes2Seconds(chan,pos);
		printf(" %d:%02d\n",p/60,p%60);
	} else /* no time length available */
		printf("\n");

	if (!(fp=fopen("BASS.WAV","wb"))) Error("Can't create file");
	printf("writing to BASS.WAV file... press a key to stop\n");

	/* write WAV header */
	wf.wFormatTag=1;
	wf.nChannels=2;
	wf.wBitsPerSample=16;
	wf.nBlockAlign=4;
	wf.nSamplesPerSec=44100;
	wf.nAvgBytesPerSec=176400;
	fwrite("RIFF\0\0\0\0WAVEfmt \20\0\0\0",20,1,fp);
	fwrite(&wf,16,1,fp);
	fwrite("data\0\0\0\0",8,1,fp);

	/* NOTE: some compilers don't support _kbhit */
	while (!_kbhit() && BASS_ChannelIsActive(chan)) {
		fwrite(buf,1,BASS_ChannelGetData(chan,buf,20000),fp);
		pos=BASS_ChannelGetPosition(chan,BASS_POS_BYTE);
		printf("pos %09I64u\r",pos);
	}

	/* complete WAV header */
	fflush(fp);
	p=ftell(fp);
	fseek(fp,4,SEEK_SET);
	_putw(p-8,fp);
	fflush(fp);
	fseek(fp,40,SEEK_SET);
	_putw(p-44,fp);
	fflush(fp);
	fclose(fp);

	BASS_Free();
}
