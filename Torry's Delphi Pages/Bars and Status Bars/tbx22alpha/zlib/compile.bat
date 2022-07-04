SET options=-c -6 -O2 -Ve -X- -pr -a8 -b -d -k- -vi -tWM -r -RT- -DFASTEST
del *.obj
bcc32.exe %options% adler32.c
bcc32.exe %options% compress.c
bcc32.exe %options% crc32.c
bcc32.exe %options% deflate.c
bcc32.exe %options% infback.c
bcc32.exe %options% inffast.c
bcc32.exe %options% inflate.c
bcc32.exe %options% inftrees.c
bcc32.exe %options% trees.c
bcc32.exe %options% uncompr.c
