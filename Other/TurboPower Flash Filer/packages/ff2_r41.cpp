//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEUNIT("..\source\ffsrsort.pas");
USEUNIT("..\source\ffsrcur.pas");
USEUNIT("..\source\ffllcoll.pas");
USEUNIT("..\source\cocobase.pas");
USEUNIT("..\source\ffclbase.pas");
USEUNIT("..\source\ffclbde.pas");
USEUNIT("..\source\ffclcfg.pas");
USEUNIT("..\source\ffclconv.pas");
USEUNIT("..\source\ffclintf.pas");
USEUNIT("..\source\ffclreng.pas");
USEUNIT("..\source\ffcltbrg.pas");
USEUNIT("..\source\ffdb.pas");
USEUNIT("..\source\fffile.pas");
USEUNIT("..\source\ffhash.pas");
USEUNIT("..\source\ffllbase.pas");
USEUNIT("..\source\ffllcomm.pas");
USEUNIT("..\source\ffllcomp.pas");
USEUNIT("..\source\fflldict.pas");
USEUNIT("..\source\fflleng.pas");
USEUNIT("..\source\ffllexcp.pas");
USEUNIT("..\source\ffllgrid.pas");
USEUNIT("..\source\ffconst.pas");
USEUNIT("..\source\ffconvff.pas");
USEUNIT("..\source\ffdbbase.pas");
USEUNIT("..\source\ffdtmsgq.pas");
USEUNIT("..\source\fflllgcy.pas");
USEUNIT("..\source\fflllog.pas");
USEUNIT("..\source\ffllprot.pas");
USEUNIT("..\source\ffllreq.pas");
USEUNIT("..\source\ffllthrd.pas");
USEUNIT("..\source\ffllunc.pas");
USEUNIT("..\source\ffllwsck.pas");
USEUNIT("..\source\ffllwsct.pas");
USEFORMNS("..\source\fflogdlg.pas", Fflogdlg, FFLoginDialog);
USEUNIT("..\source\ffnetmsg.pas");
USEUNIT("..\source\ffsql.pas");
USEUNIT("..\source\ffsqlbas.pas");
USEUNIT("..\source\ffsqldb.pas");
USEUNIT("..\source\ffsqldef.pas");
USEUNIT("..\source\ffsqleng.pas");
USEUNIT("..\source\ffsrbase.pas");
USEUNIT("..\source\ffsrbde.pas");
USEUNIT("..\source\ffsrblob.pas");
USEUNIT("..\source\ffsrcfg.pas");
USEUNIT("..\source\ffsrcmd.pas");
USEUNIT("..\source\ffsrcvex.pas");
USEUNIT("..\source\ffsreng.pas");
USEUNIT("..\source\ffsrfltr.pas");
USEUNIT("..\source\ffsrfmap.pas");
USEUNIT("..\source\ffsrfold.pas");
USEUNIT("..\source\ffsrintf.pas");
USEUNIT("..\source\ffsrintm.pas");
USEUNIT("..\source\ffsrixhl.pas");
USEUNIT("..\source\ffsrlock.pas");
USEUNIT("..\source\ffsrmgr.pas");
USEUNIT("..\source\ffsrsec.pas");
USEUNIT("..\source\ffsrstat.pas");
USEUNIT("..\source\ffsrtran.pas");
USEFORMNS("..\source\ffsrvdlg.pas", Ffsrvdlg, FFPickServerDlg);
USEUNIT("..\source\ffstdate.pas");
USEUNIT("..\source\fftbbase.pas");
USEUNIT("..\source\fftbblob.pas");
USEUNIT("..\source\fftbcryp.pas");
USEUNIT("..\source\fftbdata.pas");
USEUNIT("..\source\fftbdict.pas");
USEUNIT("..\source\fftbindx.pas");
USEUNIT("..\source\fftbstrm.pas");
USEUNIT("..\source\ffutil.pas");
USEPACKAGE("VCL40.bpi");
USEPACKAGE("VCLDB40.bpi");
USEUNIT("..\source\ffsrjour.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  return 1;
}
//---------------------------------------------------------------------------
