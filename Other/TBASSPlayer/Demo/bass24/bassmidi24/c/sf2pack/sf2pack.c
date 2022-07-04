/*
	BASSMIDI simple soundfont packer
	Copyright (c) 2006-2008 Un4seen Developments Ltd.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include "bass.h"
#include "bassmidi.h"

// encoder command-lines
#define ENCODERS 5
const char *commands[ENCODERS]={
	"flac --best -", // FLAC lossless
	"wavpack -h -", // WavPack lossless
	"wavpack -hb384 -", // WavPack lossy (high)
	"wavpack -hb256 -", // WavPack lossy (average)
	"wavpack -hb128 -" // WavPack lossy (low)
};

void main(int argc, char **argv)
{
	HSOUNDFONT sf2;
	char outfile[300],*ext;
	int arg,encoder=0,unpack=0;

	printf("SF2Pack - Soundfont Packer\n"
			"--------------------------\n");

	// process options
	for (arg=1;arg<argc;arg++) {
		if (argv[arg][0]!='-') break;
		switch (argv[arg][1]) {
			case 'e':
				encoder=atoi(argv[arg]+2);
				if (encoder>=ENCODERS) {
					printf("Invalid encoder\n");
					return;
				}
				break;
			case 'u':
				unpack=1;
				break;
		}
	}
	if (arg==argc) {
		printf("usage: sf2pack [options] <file> [outfile]\n"
			"\t-e<0-4>\tencoder: 0=FLAC lossless (default), 1=WavPack lossless\n"
			"\t\t2=WavPack Lossy (high quality), 3=WavPack Lossy (average)\n"
			"\t\t4=WavPack Lossy (low)\n"
			"\t-u\tunpack file\n");
		return;
	}

	// open soundfont
	sf2=BASS_MIDI_FontInit(argv[arg],0);
	if (!sf2) {
		printf("Can't open soundfont (error: %d)\n",BASS_ErrorGetCode());
		return;
	}

	if (unpack) {
		printf("unpacking...\n");
		if (arg+1<argc) {
			strcpy(outfile,argv[arg+1]);
		} else {
			strcpy(outfile,argv[arg]);
			strcat(outfile,".sf2");
		}
		// load plugins to unpack with
		BASS_PluginLoad("bassflac.dll",0);
		BASS_PluginLoad("basswv.dll",0);
		// not playing anything, so don't need an update thread
		BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD,0);
		// initialize BASS for decoding ("no sound" device)
		BASS_Init(0,44100,0,0,NULL);
		if (!BASS_MIDI_FontUnpack(sf2,outfile,0)) {
			BASS_Free();
			printf("Unpacking failed (error: %d)\n",BASS_ErrorGetCode());
			return;
		}
		BASS_Free();
	} else  {
		printf("packing (%s)...\n",commands[encoder]);
		if (arg+1<argc) {
			strcpy(outfile,argv[arg+1]);
		} else {
			strcpy(outfile,argv[arg]);
			ext=strrchr(outfile,'.');
			if (ext && !strpbrk(ext,"\\/")) strcat(ext,"pack");
			else strcat(outfile,".sf2pack");
		}
		if (!BASS_MIDI_FontPack(sf2,outfile,commands[encoder],0)) {
			printf("Packing failed (error: %d)\n",BASS_ErrorGetCode());
			return;
		}
	}

	{ // display ratio
		struct stat si,so;
		stat(argv[arg],&si);
		stat(outfile,&so);
		printf("done: %d -> %d (%.1f%%)\n",(int)si.st_size,(int)so.st_size,100.f*so.st_size/si.st_size);
	}
}
