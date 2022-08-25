-----------------------------------------------------------------------
OpenSSL v1.0.2u Win64 for ICS, http://www.overbyte.be
-----------------------------------------------------------------------

The OpenSSL DLLs and EXE files are digitally code signed 'Open Source
Developer, François PIETTE', the lead developer for ICS.  ICS V8.38 and
later check the DLLs are correctly signed when opening them.  Beware
that Windows needs recent root certificates to check newly signed code,
and may give an error if the root store has not been kept current by
Windows Update, particularly on older versions of Windows such as XP,
Vista and 7.

Previously built-in default engines became external DLLs by default.
As long as ICS doesn't support OpenSSL engines just use libeay32.dll
and ssleay32.dll.

Built with:
                  Visual Studio Build Tools 2017
                  The Netwide Assembler (NASM) v2.14.02
                  Strawberry Perl v5.20.3.1

Build Commands:   perl configure VC-WIN64A no-ssl2-method
                  ms\do_nasm
                  adjusted ms\ntdll.mak       (replaced "/MD" by "/MT")
                  nmake -f ms\ntdll.mak
                  nmake -f ms\ntdll.mak test
