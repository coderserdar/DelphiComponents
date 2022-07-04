Network Multimedia Library version 1.0.
Copyright (c) 2004 Ivan Babikov.
See "license" file for licesing information.


1.FEATURES:


1) Access to the remote computer screen (see the demo in RC dir).
2) Voice broadcast (see voice broadcast demo).
3) Voice chat (see VoiceP2PChat and SoftPhone demos).

Network Multimedia Library works over TCP. For Voice routines Windows ACM API is used. 
Prefered codedc to use is Speex which is available at 
http://www.republika.pl/roed/speexw/download/speexw.exe. See also www.speex.org.

Most of other codecs available for the voice mode (8000-16000Hz) do not work 
with NMM yet. The future plans though is to make NMM working with Speex 
directly rather than push on other codecs.

NMM works with TCP using Indy which uses blockin sockets. That's why NMM may
look to be rather bulky. But on the other hand working upon TCP is a main 
feature of NMM - most of other such libs works through UDP which is easier
to implement but has it's own problems.



2. REQUIRMENTS:

Indy 9 available at http://www.atozedsoftware.com/Indyproject 



3. FUTURE PLANS:

1) Voice latency reducing.
2) Voice Activity Detection (should reduce the network loading 3-4 times).
3) Direct call to Speex codec (no codec setup will be required).
4) Optimizing screen feedback to mouse and key events for remote PC control.
5) Voice conference.
6) Integration with h323.
7) HTTP support.


Anyone's help on the project or sponsorship will be appreciated.


Author contact info:
EMail: babikov@mail.ru, i-software.narod.ru
Project home: www.i-software.narod.ru
