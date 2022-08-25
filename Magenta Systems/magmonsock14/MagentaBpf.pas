{ Magenta Systems Internet Packet Monitoring Components
Updated by Angus Robertson, Magenta Systems Ltd, England, v1.3 13th August 2010

********************************************************************************
--------------------------------------------------------------------------------
                              Conversion of BPF.H
                             From C to ObjectPascal

                              - By Lars Peter Christiansen
--------------------------------------------------------------------------------
 If you have any questions, requests, bug reports, etc., please contact me at
 the address given below.

Lars Peter Christiansen
Email  : lp@nzlab.dk
Website: http://www.nzlab.dk
--------------------------------------------------------------------------------

8 Aug 2008 - 1.2 - updated to support ICS V6 and V7, and Delphi 2009

********************************************************************************
}
unit MagentaBpf;

interface

Type
  // For future combatibility
  Tbpf_u_int32 = LongWord;
  Tbpf_int32   = Integer;

  //[ taken from DRIVER\packet.h ]
  // Unix's way of timestamping.
  PunixTimeVal = ^TunixTimeVal;
  TunixTimeVal = record
    tv_Sec,            // Secs since 1/1/1970
    tv_uSec: LongWord; // microseconds
  end;

  // [ Gotten the following structs from LIBPCAP\Packet32.h]
  Tbpf_insn = record
    code : Word;
    jt   : Byte;
    jf   : Byte;
    k    : Integer;
  end;

  Pbpf_program = ^Tbpf_program;
  Tbpf_program = record
    bf_len  : LongWord;
    bf_insns: ^Tbpf_insn;
  end;

  Pbpf_stat = ^Tbpf_stat;
  Tbpf_stat = record
    bs_recv,
    bs_drop : LongWord;
  end;

  Pbpf_hdr = ^Tbpf_hdr;        //Structure prepended to each packet.
  Tbpf_hdr =record
    bh_tstamp :TunixTimeval;	//* time stamp */
    bh_caplen,            	//* length of captured portion */
    bh_datalen: Tbpf_u_int32;	//* original length of packet */
    bh_hdrlen : Word ;        	//* length of bpf header (this struct plus
                                //alignment padding) */
  end;

const
  BPF_ALIGNMENT = sizeof(Tbpf_int32);

  DLT_NULL	 =0;	//* no link-layer encapsulation */
  DLT_EN10MB     =1;	//* Ethernet (10Mb) */
  DLT_EN3MB      =2;	//* Experimental Ethernet (3Mb) */
  DLT_AX25       =3;	//* Amateur Radio AX.25 */
  DLT_PRONET     =4;	//* Proteon ProNET Token Ring */
  DLT_CHAOS      =5;	//* Chaos */
  DLT_IEEE802	 =6;	//* IEEE 802 Networks */
  DLT_ARCNET	 =7;	//* ARCNET */
  DLT_SLIP	 =8;	//* Serial Line IP */
  DLT_PPP	 =9;	//* Point-to-point Protocol */
  DLT_FDDI	 =10;	//* FDDI */
  DLT_ATM_RFC1483=11;	//* LLC/SNAP encapsulated atm */
  DLT_RAW	 =12;	//* raw IP */
  DLT_SLIP_BSDOS =13;	//* BSD/OS Serial Line IP */
  DLT_PPP_BSDOS	 =14;	//* BSD/OS Point-to-point Protocol */

  //New types for Win32
  DLT_EN100MB	 =100;	//* Ethernet (100Mb) */
  DLT_PPP_WIN32	 =101;	//* Win32 dial up connection */


Function BPF_WORDALIGN(X:LongWord) : LongWord;  //Force data to be aligned
implementation

//------------------------------------------------------------------------------
// This was originally a C macro :
//
// #define BPF_WORDALIGN(x) (((x)+(BPF_ALIGNMENT-1))&~(BPF_ALIGNMENT-1))
//
//------------------------------------------------------------------------------
function BPF_WORDALIGN(X:LongWord) : LongWord;
begin
  result := (((X)+(BPF_ALIGNMENT-1))and not(BPF_ALIGNMENT-1));
end;


end.
