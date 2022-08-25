{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels
Creation:     October 30, 2011
Description:  Windows types for POSIX
Version:      V8.65
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2011-2011 by Arno Garrels, Berlin, Germany,

              This software is freeware and provided 'as-is', without any
              express or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of this
              software.

              The following restrictions apply:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.


History:
Sep 03, 2020 V8.65   Added ULONG and ULONG_PTR.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Ics.Posix.WinTypes;

interface

{$IFDEF POSIX}
uses
  Posix.Errno;

type
  DWORD                           = LongWord;
  UINT                            = LongWord;
  INT_PTR                         = NativeInt;
  PINT_PTR                        = ^INT_PTR;
  UINT_PTR                        = NativeUInt;
  PUINT_PTR                       = ^UINT_PTR;
  LOWORD                          = Word;
  ULONG                           = LongWord;   { V8.65 }
  ULONG_PTR                       = LongWord;   { V8.65 }

const
  { Error consts }
  ERROR_NOT_ENOUGH_QUOTA          = ENOMEM;   // windows 1816 // Message queue full
  ERROR_INVALID_WINDOW_HANDLE     = EBADMSG;  // windows 1400
  ERROR_ACCESS_DENIED             = EACCES;

 { IcsMbToWc, IcsWcToMb error codes }
  ERROR_INVALID_FLAGS             = 1004;
  ERROR_NO_UNICODE_TRANSLATION    = EILSEQ;   // windows 1113;
  ERROR_INVALID_PARAMETER         = EINVAL;   // windows 87;
  ERROR_INSUFFICIENT_BUFFER       = E2BIG;    // windows 122;

 { IcsMbToWc, IcsWcToMb Flags }
  MB_ERR_INVALID_CHARS            = $00000008;
  WC_ERR_INVALID_CHARS            = $80;

{ Windows codepage IDs }
  CP_ACP                          = 0;
  CP_OEMCP                        = 1;
  CP_UTF7                         = 65000;
  CP_UTF8                         = 65001;

  MaxWord                         = $FFFF;
  MaxDWord                        = $FFFFFFFF;
{$ENDIF POSIX}

implementation

end.
