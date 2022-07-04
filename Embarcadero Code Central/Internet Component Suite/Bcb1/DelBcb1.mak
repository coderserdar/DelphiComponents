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
HPPFILES = wsocket.hpp wsockbuf.hpp httpprot.hpp ftpcli.hpp wait.hpp emulvt.hpp tncnx.hpp \
     tnoptfrm.hpp tnemulvt.hpp tnscript.hpp fingcli.hpp nntpcli.hpp icmp.hpp \
     ping.hpp ftpsrvc.hpp ftpsrvt.hpp ftpsrv.hpp smtpprot.hpp md5.hpp pop3prot.hpp \
     MimeDec.hpp DnsQuery.hpp WSocketS.hpp HttpSrv.hpp wait.hpp
OBJFILES = wsocket.obj wsockbuf.obj httpprot.obj ftpcli.obj wait.obj emulvt.obj tncnx.obj \
     tnoptfrm.obj tnemulvt.obj tnscript.obj fingcli.obj nntpcli.obj icmp.obj \
     ping.obj ftpsrvc.obj ftpsrvt.obj ftpsrv.obj smtpprot.obj md5.obj pop3prot.obj \
     MimeDec.obj DnsQuery.obj WSocketS.obj HttpSrv.obj wait.obj formpos.obj
RESFILES = 
RESDEPEN = 
LIBFILES = 
DEFFILE = 
# ---------------------------------------------------------------------------
CFLAG1 = -Od -Hc -w -k -r- -y -v -vi- -c -a4 -b- -w-par -w-inl -Vx -Ve -x
CFLAG2 = -I$(ICS_PATH)\cpp\internet;$(ICSVCL);$(BCB_PATH)\include;$(BCB_PATH)\include\vcl \
   -H=$(BCB_PATH)\lib\vcld.csm 
PFLAGS = -U$(ICS_PATH)\cpp\internet;$(ICSVCL);$(BCB_PATH)\lib\obj;$(BCB_PATH)\lib \
   -I$(ICS_PATH)\cpp\internet;$(ICSVCL);$(BCB_PATH)\include;$(BCB_PATH)\include\vcl \
   -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE -v \
   -$Y -$W -$O- -JPHNV -M     
RFLAGS = -i$(ICS_PATH)\cpp\internet;$(ICSVCL);$(BCB_PATH)\include;$(BCB_PATH)\include\vcl 
LFLAGS = -L$(ICS_PATH)\cpp\internet;$(ICSVCL);$(BCB_PATH)\lib\obj;$(BCB_PATH)\lib \
   -aa -Tpe -x -v -V4.0 
IFLAGS = 
LINKER = tlink32
# ---------------------------------------------------------------------------
ALLOBJ = c0w32.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) vcl.lib import32.lib cp32mt.lib 
# ---------------------------------------------------------------------------
.autodepend

all: $(HPPFILES) $(OBJFILES)

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

