# ---------------------------------------------------------------------------
#                                                   
# ICS - Internet Component Suite                    
#                                                   
# C++ Builder 1 automated construction V1.00        
# (c) 1997-2000 by Francois PIETTE                  
# http://www.rtfm.be/fpiette/indexuk.htm            
# francois.piette@swing.be  francois.piette@rtfm.be 
#                                                   
# You must change BCB_PATH and ICS_PATH below to fit your system.                
#                                                   
# Remember to install all components in BCB1 !      
# Remember to use BCB1 to open all forms and        
# ignore Font.CharSet and OldCreateOrder properties 
#                                                   
# ---------------------------------------------------------------------------
VERSION = BCB.01
# ---------------------------------------------------------------------------
!ifndef BCB_PATH
BCB_PATH = $(MAKEDIR)\..
!endif
# ---------------------------------------------------------------------------
!ifndef ICS_PATH
ICS_PATH = d:\fpiette
!endif
# ---------------------------------------------------------------------------
ICSVCL = $(ICS_PATH)\delphi\vc32
# ---------------------------------------------------------------------------
CFLAG1 = -Od -Hc -w -k -r- -y -v -vi- -c -a4 -b- -w-par -w-inl -Vx -Ve -x
CFLAG2 = -I$(ICS_PATH)\cpp\internet;$(ICS_PATH)\delphi\vc32;$(BCB_PATH)\include;$(BCB_PATH)\include\vcl \
   -H=$(BCB_PATH)\lib\vcld.csm 
PFLAGS = -U$(ICS_PATH)\cpp\internet;$(ICSVCL);$(BCB_PATH)\lib\obj;$(BCB_PATH)\lib \
   -I$(ICS_PATH)\cpp\internet;$(ICSVCL);$(BCB_PATH)\include;$(BCB_PATH)\include\vcl \
   -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE -v \
   -$Y -$W -$O- -JPHNV -M     
RFLAGS = -i$(ICS_PATH)\cpp\internet;$(ICSVCL);$(BCB_PATH)\include;$(BCB_PATH)\include\vcl 
LFLAGS = -L$(ICS_PATH)\cpp\internet;$(ICSVCL);$(BCB_PATH)\lib\obj;$(BCB_PATH)\lib \
   -Tpe -x -v -V4.0 
IFLAGS = 
LINKER = tlink32
# ---------------------------------------------------------------------------
ALLLIB = vcl.lib import32.lib cp32mt.lib 
# ---------------------------------------------------------------------------
.autodepend

all: udpsend.exe udplstn.exe \
     twschat.exe tnsrv.exe tndemo.exe tnclient.exe tcpsrv.exe srvdemo.exe \
     sockstst.exe server5.exe sender.exe recv.exe pingtst.exe newsrdr.exe \
     mtsrv.exe mailsnd.exe httptst.exe httppg.exe ftptst.exe ftpserv.exe \
     concli1.exe concli2.exe clidemo.exe client5.exe client7.exe finger.exe

# Some projects have not been ported to BCB1. See Delphi source code :-(
# dlltst1.exe dnslook.exe dynCli.exe ftpthrd.exe httpasp.exe httpasy.exe 
# httpchk.exe httpdmo.exe httpget.exe httpthrd.exe icsdll1.exe mailrcv.exe 
# mailrob.exe md5test.exe mimedemo.exe mimetst.exe nslookup.exe pop3mime.exe 
# srvtcp.exe webserv.exe 
     
udpsend.exe: udpsend.obj ..\udpsend1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

udplstn.exe: udplstn.obj ..\udplstn1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

twschat.exe: twschat.obj ..\twschat1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

tnsrv.exe: tnsrv.obj ..\tnsrv1.obj ..\tnsrv2.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

tnclient.exe: tnclient.obj ..\tncli1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj $(ICSVCL)\tncnx.obj $(ICSVCL)\tnoptfrm.obj $(ICSVCL)\formpos.obj, +
    $@,,$(ALLLIB),,$&.res 
!

tndemo.exe: tndemo.obj ..\tndemo1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj $(ICSVCL)\tncnx.obj, +
    $@,,$(ALLLIB),,$&.res 
!

tcpsrv.exe: tcpsrv.obj ..\tcpsrv1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

srvdemo.exe: srvdemo.obj ..\srvdemo1.obj ..\srvdemo2.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

sockstst.exe: sockstst.obj ..\socks1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

server5.exe: server5.obj ..\srv5.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

sender.exe: sender.obj ..\sender1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

recv.exe: recv.obj ..\recv1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

pingtst.exe: pingtst.obj ..\pingtst1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj $(ICSVCL)\ping.obj, +
    $@,,$(ALLLIB),,$&.res 
!

newsrdr.exe: newsrdr.obj ..\newsrdr1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj $(ICSVCL)\nntpcli.obj, +
    $@,,$(ALLLIB),,$&.res 
!

mtsrv.exe: mtsrv.obj ..\mtsrv1.obj ..\mtsrv2.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

mailsnd.exe: mailsnd.obj ..\mailsnd1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj $(ICSVCL)\smtpprot.obj, +
    $@,,$(ALLLIB),,$&.res 
!

httppg.exe: httppg.obj ..\httppg1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj $(ICSVCL)\httpprot.obj, +
    $@,,$(ALLLIB),,$&.res 
!

httptst.exe: httptst.obj ..\httptst1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj $(ICSVCL)\httpprot.obj, +
    $@,,$(ALLLIB),,$&.res 
!

concli1.exe: concli1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -ap +
    c0x32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,
!

concli2.exe: concli2.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -ap +
    c0x32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,
!

clidemo.exe: clidemo.obj ..\clidemo1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

client5.exe: client5.obj ..\cli5.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

client7.exe: client7.obj ..\cli7.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj, +
    $@,,$(ALLLIB),,$&.res 
!

finger.exe: finger.obj ..\finger1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj $(ICSVCL)\fingcli.obj, +
    $@,,$(ALLLIB),,$&.res 
!

ftpserv.exe: ftpserv.obj ..\ftpsrv1.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj $(ICSVCL)\ftpsrv.obj, +
    $@,,$(ALLLIB),,$&.res 
!

ftptst.exe: ftptst.obj ..\ftptst1.obj ..\ftptst2.obj
    $(BCB_PATH)\BIN\$(LINKER) @&&!
    $(LFLAGS) -aa +
    c0w32.obj $** $(ICSVCL)\wsocket.obj $(ICSVCL)\ftpcli.obj, +
    $@,,$(ALLLIB),,$&.res 
!

.pas.hpp:
    $(BCB_PATH)\BIN\dcc32 $(PFLAGS) { $** }

.pas.obj:
    $(BCB_PATH)\BIN\dcc32 $(PFLAGS) { $** }

.cpp.obj:
    $(BCB_PATH)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $* 

.c.obj:
    $(BCB_PATH)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $**

.rc.res:
    $(BCB_PATH)\BIN\brcc32 $(RFLAGS) $<

