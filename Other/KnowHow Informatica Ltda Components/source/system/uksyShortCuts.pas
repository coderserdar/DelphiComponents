{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit uksyShortCuts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
  uksyUtils; {##NI##}{ do not remove! force uksyUtils to be registered in unit running list }{##NI##}

{
--------------------------------------------------------------------------------
--------------------------- Generic shortcut constants -------------------------
--------------------------------------------------------------------------------
}

const

{ shortcuts :: Control Keys }

	SC_NULL           = $0000;
	SC_CTRL           = $4000;
	SC_SHIFT          = $2000;
	SC_ALT            = $8000;

	SC_ALTCTRL        = SC_ALT or SC_CTRL;
	SC_CTRLALT        = SC_ALTCTRL;

	SC_ALTSHIFT       = SC_ALT or SC_SHIFT;
	SC_SHIFTALT       = SC_ALTSHIFT;

	SC_CTRLSHIFT      = SC_CTRL or SC_SHIFT;
	SC_SHIFTCTRL      = SC_CTRLSHIFT;

	SC_SHIFTCTRLALT   = SC_SHIFT or SC_CTRL or SC_ALT;
	SC_SHIFTALTCTRL   = SC_SHIFTCTRLALT;
	SC_CTRLALTSHIFT   = SC_SHIFTCTRLALT;
	SC_CTRLSHIFTALT   = SC_SHIFTCTRLALT;
	SC_ALTCTRLSHIFT   = SC_SHIFTCTRLALT;
	SC_ALTSHIFTCTRL   = SC_SHIFTCTRLALT;

{ shortcuts :: Function Keys }

	SC_F1  = $0070;
	SC_F2  = $0071;
	SC_F3  = $0072;
	SC_F4  = $0073;
	SC_F5  = $0074;
	SC_F6  = $0075;
	SC_F7  = $0076;
	SC_F8  = $0077;
	SC_F9  = $0078;
	SC_F10 = $0079;
	SC_F11 = $007A;
	SC_F12 = $007B;
	SC_F13 = $007C;
	SC_F14 = $007D;
	SC_F15 = $007E;
	SC_F16 = $007F;
	SC_F17 = $0080;
	SC_F18 = $0081;
	SC_F19 = $0082;
	SC_F20 = $0083;
	SC_F21 = $0084;
	SC_F22 = $0085;
	SC_F23 = $0086;
	SC_F24 = $0087;

{ shortcuts :: Alt + Function Keys }

	SC_ALT_F1  = SC_ALT or SC_F1;
	SC_ALT_F2  = SC_ALT or SC_F2;
	SC_ALT_F3  = SC_ALT or SC_F3;
	SC_ALT_F4  = SC_ALT or SC_F4;
	SC_ALT_F5  = SC_ALT or SC_F5;
	SC_ALT_F6  = SC_ALT or SC_F6;
	SC_ALT_F7  = SC_ALT or SC_F7;
	SC_ALT_F8  = SC_ALT or SC_F8;
	SC_ALT_F9  = SC_ALT or SC_F9;
	SC_ALT_F10 = SC_ALT or SC_F10;
	SC_ALT_F11 = SC_ALT or SC_F11;
	SC_ALT_F12 = SC_ALT or SC_F12;
	SC_ALT_F13 = SC_ALT or SC_F13;
	SC_ALT_F14 = SC_ALT or SC_F14;
	SC_ALT_F15 = SC_ALT or SC_F15;
	SC_ALT_F16 = SC_ALT or SC_F16;
	SC_ALT_F17 = SC_ALT or SC_F17;
	SC_ALT_F18 = SC_ALT or SC_F18;
	SC_ALT_F19 = SC_ALT or SC_F19;
	SC_ALT_F20 = SC_ALT or SC_F20;
	SC_ALT_F21 = SC_ALT or SC_F21;
	SC_ALT_F22 = SC_ALT or SC_F22;
	SC_ALT_F23 = SC_ALT or SC_F23;
	SC_ALT_F24 = SC_ALT or SC_F24;

{ shortcuts :: Ctrl + Function Keys }

	SC_CTRL_F1  = SC_CTRL or SC_F1;
	SC_CTRL_F2  = SC_CTRL or SC_F2;
	SC_CTRL_F3  = SC_CTRL or SC_F3;
	SC_CTRL_F4  = SC_CTRL or SC_F4;
	SC_CTRL_F5  = SC_CTRL or SC_F5;
	SC_CTRL_F6  = SC_CTRL or SC_F6;
	SC_CTRL_F7  = SC_CTRL or SC_F7;
	SC_CTRL_F8  = SC_CTRL or SC_F8;
	SC_CTRL_F9  = SC_CTRL or SC_F9;
	SC_CTRL_F10 = SC_CTRL or SC_F10;
	SC_CTRL_F11 = SC_CTRL or SC_F11;
	SC_CTRL_F12 = SC_CTRL or SC_F12;
	SC_CTRL_F13 = SC_CTRL or SC_F13;
	SC_CTRL_F14 = SC_CTRL or SC_F14;
	SC_CTRL_F15 = SC_CTRL or SC_F15;
	SC_CTRL_F16 = SC_CTRL or SC_F16;
	SC_CTRL_F17 = SC_CTRL or SC_F17;
	SC_CTRL_F18 = SC_CTRL or SC_F18;
	SC_CTRL_F19 = SC_CTRL or SC_F19;
	SC_CTRL_F20 = SC_CTRL or SC_F20;
	SC_CTRL_F21 = SC_CTRL or SC_F21;
	SC_CTRL_F22 = SC_CTRL or SC_F22;
	SC_CTRL_F23 = SC_CTRL or SC_F23;
	SC_CTRL_F24 = SC_CTRL or SC_F24;

{ shortcuts :: Shift + Function Keys }

	SC_SHIFT_F1  = SC_SHIFT or SC_F1;
	SC_SHIFT_F2  = SC_SHIFT or SC_F2;
	SC_SHIFT_F3  = SC_SHIFT or SC_F3;
	SC_SHIFT_F4  = SC_SHIFT or SC_F4;
	SC_SHIFT_F5  = SC_SHIFT or SC_F5;
	SC_SHIFT_F6  = SC_SHIFT or SC_F6;
	SC_SHIFT_F7  = SC_SHIFT or SC_F7;
	SC_SHIFT_F8  = SC_SHIFT or SC_F8;
	SC_SHIFT_F9  = SC_SHIFT or SC_F9;
	SC_SHIFT_F10 = SC_SHIFT or SC_F10;
	SC_SHIFT_F11 = SC_SHIFT or SC_F11;
	SC_SHIFT_F12 = SC_SHIFT or SC_F12;
	SC_SHIFT_F13 = SC_SHIFT or SC_F13;
	SC_SHIFT_F14 = SC_SHIFT or SC_F14;
	SC_SHIFT_F15 = SC_SHIFT or SC_F15;
	SC_SHIFT_F16 = SC_SHIFT or SC_F16;
	SC_SHIFT_F17 = SC_SHIFT or SC_F17;
	SC_SHIFT_F18 = SC_SHIFT or SC_F18;
	SC_SHIFT_F19 = SC_SHIFT or SC_F19;
	SC_SHIFT_F20 = SC_SHIFT or SC_F20;
	SC_SHIFT_F21 = SC_SHIFT or SC_F21;
	SC_SHIFT_F22 = SC_SHIFT or SC_F22;
	SC_SHIFT_F23 = SC_SHIFT or SC_F23;
	SC_SHIFT_F24 = SC_SHIFT or SC_F24;

{ shortcuts :: Ctrl + Shift + Function Keys }

	SC_CTRL_SHIFT_F1  = SC_CTRLSHIFT or SC_F1;
	SC_CTRL_SHIFT_F2  = SC_CTRLSHIFT or SC_F2;
	SC_CTRL_SHIFT_F3  = SC_CTRLSHIFT or SC_F3;
	SC_CTRL_SHIFT_F4  = SC_CTRLSHIFT or SC_F4;
	SC_CTRL_SHIFT_F5  = SC_CTRLSHIFT or SC_F5;
	SC_CTRL_SHIFT_F6  = SC_CTRLSHIFT or SC_F6;
	SC_CTRL_SHIFT_F7  = SC_CTRLSHIFT or SC_F7;
	SC_CTRL_SHIFT_F8  = SC_CTRLSHIFT or SC_F8;
	SC_CTRL_SHIFT_F9  = SC_CTRLSHIFT or SC_F9;
	SC_CTRL_SHIFT_F10 = SC_CTRLSHIFT or SC_F10;
	SC_CTRL_SHIFT_F11 = SC_CTRLSHIFT or SC_F11;
	SC_CTRL_SHIFT_F12 = SC_CTRLSHIFT or SC_F12;
	SC_CTRL_SHIFT_F13 = SC_CTRLSHIFT or SC_F13;
	SC_CTRL_SHIFT_F14 = SC_CTRLSHIFT or SC_F14;
	SC_CTRL_SHIFT_F15 = SC_CTRLSHIFT or SC_F15;
	SC_CTRL_SHIFT_F16 = SC_CTRLSHIFT or SC_F16;
	SC_CTRL_SHIFT_F17 = SC_CTRLSHIFT or SC_F17;
	SC_CTRL_SHIFT_F18 = SC_CTRLSHIFT or SC_F18;
	SC_CTRL_SHIFT_F19 = SC_CTRLSHIFT or SC_F19;
	SC_CTRL_SHIFT_F20 = SC_CTRLSHIFT or SC_F20;
	SC_CTRL_SHIFT_F21 = SC_CTRLSHIFT or SC_F21;
	SC_CTRL_SHIFT_F22 = SC_CTRLSHIFT or SC_F22;
	SC_CTRL_SHIFT_F23 = SC_CTRLSHIFT or SC_F23;
	SC_CTRL_SHIFT_F24 = SC_CTRLSHIFT or SC_F24;

{ shortcuts :: Alt + Shift + Function Keys }

	SC_ALT_SHIFT_F1  = SC_ALTSHIFT or SC_F1;
	SC_ALT_SHIFT_F2  = SC_ALTSHIFT or SC_F2;
	SC_ALT_SHIFT_F3  = SC_ALTSHIFT or SC_F3;
	SC_ALT_SHIFT_F4  = SC_ALTSHIFT or SC_F4;
	SC_ALT_SHIFT_F5  = SC_ALTSHIFT or SC_F5;
	SC_ALT_SHIFT_F6  = SC_ALTSHIFT or SC_F6;
	SC_ALT_SHIFT_F7  = SC_ALTSHIFT or SC_F7;
	SC_ALT_SHIFT_F8  = SC_ALTSHIFT or SC_F8;
	SC_ALT_SHIFT_F9  = SC_ALTSHIFT or SC_F9;
	SC_ALT_SHIFT_F10 = SC_ALTSHIFT or SC_F10;
	SC_ALT_SHIFT_F11 = SC_ALTSHIFT or SC_F11;
	SC_ALT_SHIFT_F12 = SC_ALTSHIFT or SC_F12;
	SC_ALT_SHIFT_F13 = SC_ALTSHIFT or SC_F13;
	SC_ALT_SHIFT_F14 = SC_ALTSHIFT or SC_F14;
	SC_ALT_SHIFT_F15 = SC_ALTSHIFT or SC_F15;
	SC_ALT_SHIFT_F16 = SC_ALTSHIFT or SC_F16;
	SC_ALT_SHIFT_F17 = SC_ALTSHIFT or SC_F17;
	SC_ALT_SHIFT_F18 = SC_ALTSHIFT or SC_F18;
	SC_ALT_SHIFT_F19 = SC_ALTSHIFT or SC_F19;
	SC_ALT_SHIFT_F20 = SC_ALTSHIFT or SC_F20;
	SC_ALT_SHIFT_F21 = SC_ALTSHIFT or SC_F21;
	SC_ALT_SHIFT_F22 = SC_ALTSHIFT or SC_F22;
	SC_ALT_SHIFT_F23 = SC_ALTSHIFT or SC_F23;
	SC_ALT_SHIFT_F24 = SC_ALTSHIFT or SC_F24;

{ shortcuts :: Alt + Ctrl + Function Keys }

	SC_ALT_CTRL_F1  = SC_ALTCTRL or SC_F1;
	SC_ALT_CTRL_F2  = SC_ALTCTRL or SC_F2;
	SC_ALT_CTRL_F3  = SC_ALTCTRL or SC_F3;
	SC_ALT_CTRL_F4  = SC_ALTCTRL or SC_F4;
	SC_ALT_CTRL_F5  = SC_ALTCTRL or SC_F5;
	SC_ALT_CTRL_F6  = SC_ALTCTRL or SC_F6;
	SC_ALT_CTRL_F7  = SC_ALTCTRL or SC_F7;
	SC_ALT_CTRL_F8  = SC_ALTCTRL or SC_F8;
	SC_ALT_CTRL_F9  = SC_ALTCTRL or SC_F9;
	SC_ALT_CTRL_F10 = SC_ALTCTRL or SC_F10;
	SC_ALT_CTRL_F11 = SC_ALTCTRL or SC_F11;
	SC_ALT_CTRL_F12 = SC_ALTCTRL or SC_F12;
	SC_ALT_CTRL_F13 = SC_ALTCTRL or SC_F13;
	SC_ALT_CTRL_F14 = SC_ALTCTRL or SC_F14;
	SC_ALT_CTRL_F15 = SC_ALTCTRL or SC_F15;
	SC_ALT_CTRL_F16 = SC_ALTCTRL or SC_F16;
	SC_ALT_CTRL_F17 = SC_ALTCTRL or SC_F17;
	SC_ALT_CTRL_F18 = SC_ALTCTRL or SC_F18;
	SC_ALT_CTRL_F19 = SC_ALTCTRL or SC_F19;
	SC_ALT_CTRL_F20 = SC_ALTCTRL or SC_F20;
	SC_ALT_CTRL_F21 = SC_ALTCTRL or SC_F21;
	SC_ALT_CTRL_F22 = SC_ALTCTRL or SC_F22;
	SC_ALT_CTRL_F23 = SC_ALTCTRL or SC_F23;
	SC_ALT_CTRL_F24 = SC_ALTCTRL or SC_F24;

{ shortcuts :: Alt + Ctrl + Shift Function Keys }

	SC_ALT_CTRL_SHIFT_F1  = SC_ALTCTRLSHIFT or SC_F1;
	SC_ALT_CTRL_SHIFT_F2  = SC_ALTCTRLSHIFT or SC_F2;
	SC_ALT_CTRL_SHIFT_F3  = SC_ALTCTRLSHIFT or SC_F3;
	SC_ALT_CTRL_SHIFT_F4  = SC_ALTCTRLSHIFT or SC_F4;
	SC_ALT_CTRL_SHIFT_F5  = SC_ALTCTRLSHIFT or SC_F5;
	SC_ALT_CTRL_SHIFT_F6  = SC_ALTCTRLSHIFT or SC_F6;
	SC_ALT_CTRL_SHIFT_F7  = SC_ALTCTRLSHIFT or SC_F7;
	SC_ALT_CTRL_SHIFT_F8  = SC_ALTCTRLSHIFT or SC_F8;
	SC_ALT_CTRL_SHIFT_F9  = SC_ALTCTRLSHIFT or SC_F9;
	SC_ALT_CTRL_SHIFT_F10 = SC_ALTCTRLSHIFT or SC_F10;
	SC_ALT_CTRL_SHIFT_F11 = SC_ALTCTRLSHIFT or SC_F11;
	SC_ALT_CTRL_SHIFT_F12 = SC_ALTCTRLSHIFT or SC_F12;
	SC_ALT_CTRL_SHIFT_F13 = SC_ALTCTRLSHIFT or SC_F13;
	SC_ALT_CTRL_SHIFT_F14 = SC_ALTCTRLSHIFT or SC_F14;
	SC_ALT_CTRL_SHIFT_F15 = SC_ALTCTRLSHIFT or SC_F15;
	SC_ALT_CTRL_SHIFT_F16 = SC_ALTCTRLSHIFT or SC_F16;
	SC_ALT_CTRL_SHIFT_F17 = SC_ALTCTRLSHIFT or SC_F17;
	SC_ALT_CTRL_SHIFT_F18 = SC_ALTCTRLSHIFT or SC_F18;
	SC_ALT_CTRL_SHIFT_F19 = SC_ALTCTRLSHIFT or SC_F19;
	SC_ALT_CTRL_SHIFT_F20 = SC_ALTCTRLSHIFT or SC_F20;
	SC_ALT_CTRL_SHIFT_F21 = SC_ALTCTRLSHIFT or SC_F21;
	SC_ALT_CTRL_SHIFT_F22 = SC_ALTCTRLSHIFT or SC_F22;
	SC_ALT_CTRL_SHIFT_F23 = SC_ALTCTRLSHIFT or SC_F23;
	SC_ALT_CTRL_SHIFT_F24 = SC_ALTCTRLSHIFT or SC_F24;

{ shortcuts :: Ctrl + Keys }

	SC_CTRL_A = SC_CTRL or Ord( 'A' );
	SC_CTRL_B = SC_CTRL or Ord( 'B' );
	SC_CTRL_C = SC_CTRL or Ord( 'C' );
	SC_CTRL_D = SC_CTRL or Ord( 'D' );
	SC_CTRL_E = SC_CTRL or Ord( 'E' );
	SC_CTRL_F = SC_CTRL or Ord( 'F' );
	SC_CTRL_G = SC_CTRL or Ord( 'G' );
	SC_CTRL_H = SC_CTRL or Ord( 'H' );
	SC_CTRL_I = SC_CTRL or Ord( 'I' );
	SC_CTRL_J = SC_CTRL or Ord( 'J' );
	SC_CTRL_K = SC_CTRL or Ord( 'K' );
	SC_CTRL_L = SC_CTRL or Ord( 'L' );
	SC_CTRL_M = SC_CTRL or Ord( 'M' );
	SC_CTRL_N = SC_CTRL or Ord( 'N' );
	SC_CTRL_O = SC_CTRL or Ord( 'O' );
	SC_CTRL_P = SC_CTRL or Ord( 'P' );
	SC_CTRL_Q = SC_CTRL or Ord( 'Q' );
	SC_CTRL_R = SC_CTRL or Ord( 'R' );
	SC_CTRL_S = SC_CTRL or Ord( 'S' );
	SC_CTRL_T = SC_CTRL or Ord( 'T' );
	SC_CTRL_U = SC_CTRL or Ord( 'U' );
	SC_CTRL_V = SC_CTRL or Ord( 'V' );
	SC_CTRL_W = SC_CTRL or Ord( 'W' );
	SC_CTRL_X = SC_CTRL or Ord( 'X' );
	SC_CTRL_Y = SC_CTRL or Ord( 'Y' );
	SC_CTRL_Z = SC_CTRL or Ord( 'Z' );

{ shortcuts :: Alt + Keys }

	SC_ALT_A = SC_ALT or Ord( 'A' );
	SC_ALT_B = SC_ALT or Ord( 'B' );
	SC_ALT_C = SC_ALT or Ord( 'C' );
	SC_ALT_D = SC_ALT or Ord( 'D' );
	SC_ALT_E = SC_ALT or Ord( 'E' );
	SC_ALT_F = SC_ALT or Ord( 'F' );
	SC_ALT_G = SC_ALT or Ord( 'G' );
	SC_ALT_H = SC_ALT or Ord( 'H' );
	SC_ALT_I = SC_ALT or Ord( 'I' );
	SC_ALT_J = SC_ALT or Ord( 'J' );
	SC_ALT_K = SC_ALT or Ord( 'K' );
	SC_ALT_L = SC_ALT or Ord( 'L' );
	SC_ALT_M = SC_ALT or Ord( 'M' );
	SC_ALT_N = SC_ALT or Ord( 'N' );
	SC_ALT_O = SC_ALT or Ord( 'O' );
	SC_ALT_P = SC_ALT or Ord( 'P' );
	SC_ALT_Q = SC_ALT or Ord( 'Q' );
	SC_ALT_R = SC_ALT or Ord( 'R' );
	SC_ALT_S = SC_ALT or Ord( 'S' );
	SC_ALT_T = SC_ALT or Ord( 'T' );
	SC_ALT_U = SC_ALT or Ord( 'U' );
	SC_ALT_V = SC_ALT or Ord( 'V' );
	SC_ALT_W = SC_ALT or Ord( 'W' );
	SC_ALT_X = SC_ALT or Ord( 'X' );
	SC_ALT_Y = SC_ALT or Ord( 'Y' );
	SC_ALT_Z = SC_ALT or Ord( 'Z' );

{ shortcuts :: Alt + Ctrl + Keys }

	SC_ALT_CTRL_A = SC_ALTCTRL or Ord( 'A' );
	SC_ALT_CTRL_B = SC_ALTCTRL or Ord( 'B' );
	SC_ALT_CTRL_C = SC_ALTCTRL or Ord( 'C' );
	SC_ALT_CTRL_D = SC_ALTCTRL or Ord( 'D' );
	SC_ALT_CTRL_E = SC_ALTCTRL or Ord( 'E' );
	SC_ALT_CTRL_F = SC_ALTCTRL or Ord( 'F' );
	SC_ALT_CTRL_G = SC_ALTCTRL or Ord( 'G' );
	SC_ALT_CTRL_H = SC_ALTCTRL or Ord( 'H' );
	SC_ALT_CTRL_I = SC_ALTCTRL or Ord( 'I' );
	SC_ALT_CTRL_J = SC_ALTCTRL or Ord( 'J' );
	SC_ALT_CTRL_K = SC_ALTCTRL or Ord( 'K' );
	SC_ALT_CTRL_L = SC_ALTCTRL or Ord( 'L' );
	SC_ALT_CTRL_M = SC_ALTCTRL or Ord( 'M' );
	SC_ALT_CTRL_N = SC_ALTCTRL or Ord( 'N' );
	SC_ALT_CTRL_O = SC_ALTCTRL or Ord( 'O' );
	SC_ALT_CTRL_P = SC_ALTCTRL or Ord( 'P' );
	SC_ALT_CTRL_Q = SC_ALTCTRL or Ord( 'Q' );
	SC_ALT_CTRL_R = SC_ALTCTRL or Ord( 'R' );
	SC_ALT_CTRL_S = SC_ALTCTRL or Ord( 'S' );
	SC_ALT_CTRL_T = SC_ALTCTRL or Ord( 'T' );
	SC_ALT_CTRL_U = SC_ALTCTRL or Ord( 'U' );
	SC_ALT_CTRL_V = SC_ALTCTRL or Ord( 'V' );
	SC_ALT_CTRL_W = SC_ALTCTRL or Ord( 'W' );
	SC_ALT_CTRL_X = SC_ALTCTRL or Ord( 'X' );
	SC_ALT_CTRL_Y = SC_ALTCTRL or Ord( 'Y' );
	SC_ALT_CTRL_Z = SC_ALTCTRL or Ord( 'Z' );

{ shortcuts :: Alt + Shift + Keys }

	SC_ALT_SHIFT_A = SC_ALTSHIFT or Ord( 'A' );
	SC_ALT_SHIFT_B = SC_ALTSHIFT or Ord( 'B' );
	SC_ALT_SHIFT_C = SC_ALTSHIFT or Ord( 'C' );
	SC_ALT_SHIFT_D = SC_ALTSHIFT or Ord( 'D' );
	SC_ALT_SHIFT_E = SC_ALTSHIFT or Ord( 'E' );
	SC_ALT_SHIFT_F = SC_ALTSHIFT or Ord( 'F' );
	SC_ALT_SHIFT_G = SC_ALTSHIFT or Ord( 'G' );
	SC_ALT_SHIFT_H = SC_ALTSHIFT or Ord( 'H' );
	SC_ALT_SHIFT_I = SC_ALTSHIFT or Ord( 'I' );
	SC_ALT_SHIFT_J = SC_ALTSHIFT or Ord( 'J' );
	SC_ALT_SHIFT_K = SC_ALTSHIFT or Ord( 'K' );
	SC_ALT_SHIFT_L = SC_ALTSHIFT or Ord( 'L' );
	SC_ALT_SHIFT_M = SC_ALTSHIFT or Ord( 'M' );
	SC_ALT_SHIFT_N = SC_ALTSHIFT or Ord( 'N' );
	SC_ALT_SHIFT_O = SC_ALTSHIFT or Ord( 'O' );
	SC_ALT_SHIFT_P = SC_ALTSHIFT or Ord( 'P' );
	SC_ALT_SHIFT_Q = SC_ALTSHIFT or Ord( 'Q' );
	SC_ALT_SHIFT_R = SC_ALTSHIFT or Ord( 'R' );
	SC_ALT_SHIFT_S = SC_ALTSHIFT or Ord( 'S' );
	SC_ALT_SHIFT_T = SC_ALTSHIFT or Ord( 'T' );
	SC_ALT_SHIFT_U = SC_ALTSHIFT or Ord( 'U' );
	SC_ALT_SHIFT_V = SC_ALTSHIFT or Ord( 'V' );
	SC_ALT_SHIFT_W = SC_ALTSHIFT or Ord( 'W' );
	SC_ALT_SHIFT_X = SC_ALTSHIFT or Ord( 'X' );
	SC_ALT_SHIFT_Y = SC_ALTSHIFT or Ord( 'Y' );
	SC_ALT_SHIFT_Z = SC_ALTSHIFT or Ord( 'Z' );

{ shortcuts :: Ctrl + Shift + Keys }

	SC_CTRL_SHIFT_A = SC_CTRLSHIFT or Ord( 'A' );
	SC_CTRL_SHIFT_B = SC_CTRLSHIFT or Ord( 'B' );
	SC_CTRL_SHIFT_C = SC_CTRLSHIFT or Ord( 'C' );
	SC_CTRL_SHIFT_D = SC_CTRLSHIFT or Ord( 'D' );
	SC_CTRL_SHIFT_E = SC_CTRLSHIFT or Ord( 'E' );
	SC_CTRL_SHIFT_F = SC_CTRLSHIFT or Ord( 'F' );
	SC_CTRL_SHIFT_G = SC_CTRLSHIFT or Ord( 'G' );
	SC_CTRL_SHIFT_H = SC_CTRLSHIFT or Ord( 'H' );
	SC_CTRL_SHIFT_I = SC_CTRLSHIFT or Ord( 'I' );
	SC_CTRL_SHIFT_J = SC_CTRLSHIFT or Ord( 'J' );
	SC_CTRL_SHIFT_K = SC_CTRLSHIFT or Ord( 'K' );
	SC_CTRL_SHIFT_L = SC_CTRLSHIFT or Ord( 'L' );
	SC_CTRL_SHIFT_M = SC_CTRLSHIFT or Ord( 'M' );
	SC_CTRL_SHIFT_N = SC_CTRLSHIFT or Ord( 'N' );
	SC_CTRL_SHIFT_O = SC_CTRLSHIFT or Ord( 'O' );
	SC_CTRL_SHIFT_P = SC_CTRLSHIFT or Ord( 'P' );
	SC_CTRL_SHIFT_Q = SC_CTRLSHIFT or Ord( 'Q' );
	SC_CTRL_SHIFT_R = SC_CTRLSHIFT or Ord( 'R' );
	SC_CTRL_SHIFT_S = SC_CTRLSHIFT or Ord( 'S' );
	SC_CTRL_SHIFT_T = SC_CTRLSHIFT or Ord( 'T' );
	SC_CTRL_SHIFT_U = SC_CTRLSHIFT or Ord( 'U' );
	SC_CTRL_SHIFT_V = SC_CTRLSHIFT or Ord( 'V' );
	SC_CTRL_SHIFT_W = SC_CTRLSHIFT or Ord( 'W' );
	SC_CTRL_SHIFT_X = SC_CTRLSHIFT or Ord( 'X' );
	SC_CTRL_SHIFT_Y = SC_CTRLSHIFT or Ord( 'Y' );
	SC_CTRL_SHIFT_Z = SC_CTRLSHIFT or Ord( 'Z' );

{ shortcuts :: special keys }

	SC_PGUP      = $0021;
	SC_PGDOWN    = $0022;
	SC_END       = $0023;
	SC_HOME      = $0024;
	SC_INSERT    = $002D;
	SC_DELETE    = $002E;
	SC_ESCAPE    = $001B;
	SC_SPACE     = $0020;
	SC_BACKSPACE = $0008;
	SC_TAB       = $0009;
	SC_RETURN    = $000D;

{ shortcuts :: Alt + special keys }

	SC_ALT_PGUP      = SC_ALT or SC_PGUP;
	SC_ALT_PGDOWN    = SC_ALT or SC_PGDOWN;
	SC_ALT_END       = SC_ALT or SC_END;
	SC_ALT_HOME      = SC_ALT or SC_HOME;
	SC_ALT_INSERT    = SC_ALT or SC_INSERT;
	SC_ALT_DELETE    = SC_ALT or SC_DELETE;
	SC_ALT_ESCAPE    = SC_ALT or SC_ESCAPE;
	SC_ALT_SPACE     = SC_ALT or SC_SPACE;
	SC_ALT_BACKSPACE = SC_ALT or SC_BACKSPACE;
	SC_ALT_TAB       = SC_ALT or SC_TAB;
	SC_ALT_RETURN    = SC_ALT or SC_RETURN;


{ shortcuts :: Ctrl + special keys }

	SC_CTRL_PGUP      = SC_CTRL or SC_PGUP;
	SC_CTRL_PGDOWN    = SC_CTRL or SC_PGDOWN;
	SC_CTRL_END       = SC_CTRL or SC_END;
	SC_CTRL_HOME      = SC_CTRL or SC_HOME;
	SC_CTRL_INSERT    = SC_CTRL or SC_INSERT;
	SC_CTRL_DELETE    = SC_CTRL or SC_DELETE;
	SC_CTRL_ESCAPE    = SC_CTRL or SC_ESCAPE;
	SC_CTRL_SPACE     = SC_CTRL or SC_SPACE;
	SC_CTRL_BACKSPACE = SC_CTRL or SC_BACKSPACE;
	SC_CTRL_TAB       = SC_CTRL or SC_TAB;
	SC_CTRL_RETURN    = SC_CTRL or SC_RETURN;

{ shortcuts :: Shift + special keys }

	SC_SHIFT_PGUP      = SC_SHIFT or SC_PGUP;
	SC_SHIFT_PGDOWN    = SC_SHIFT or SC_PGDOWN;
	SC_SHIFT_END       = SC_SHIFT or SC_END;
	SC_SHIFT_HOME      = SC_SHIFT or SC_HOME;
	SC_SHIFT_INSERT    = SC_SHIFT or SC_INSERT;
	SC_SHIFT_DELETE    = SC_SHIFT or SC_DELETE;
	SC_SHIFT_ESCAPE    = SC_SHIFT or SC_ESCAPE;
	SC_SHIFT_SPACE     = SC_SHIFT or SC_SPACE;
	SC_SHIFT_BACKSPACE = SC_SHIFT or SC_BACKSPACE;
	SC_SHIFT_TAB       = SC_SHIFT or SC_TAB;
  SC_SHIFT_RETURN    = SC_SHIFT or SC_RETURN;	

{ shortcuts :: movement keys }

	SC_LEFT        = $0025;
	SC_UP          = $0026;
	SC_DOWN        = $0027;
	SC_RIGHT       = $0028;

{ shortcuts :: Alt + movement keys }

	SC_ALT_LEFT    = SC_ALT or SC_LEFT;
	SC_ALT_UP      = SC_ALT or SC_UP;
	SC_ALT_DOWN    = SC_ALT or SC_DOWN;
	SC_ALT_RIGHT   = SC_ALT or SC_RIGHT;

{ shortcuts :: Ctrl + movement keys }

	SC_CTRL_LEFT   = SC_CTRL or SC_LEFT;
	SC_CTRL_UP     = SC_CTRL or SC_UP;
	SC_CTRL_DOWN   = SC_CTRL or SC_DOWN;
	SC_CTRL_RIGHT  = SC_CTRL or SC_RIGHT;

{ shortcuts :: Shift + movement keys }

	SC_SHIFT_LEFT  = SC_SHIFT or SC_LEFT;
	SC_SHIFT_UP    = SC_SHIFT or SC_UP;
	SC_SHIFT_DOWN  = SC_SHIFT or SC_DOWN;
	SC_SHIFT_RIGHT = SC_SHIFT or SC_RIGHT;

{ shortcuts :: special keyborad keys }

	SC_EQUAL  	  = $00BB;
	SC_COMAN 		  = $00BC;
	SC_SUB		   	= $00BD;
	SC_POINT      = $00BE;

{ shortcuts :: Alt + special keyborad keys }

  SC_ALT_EQUAL  = SC_ALT or SC_EQUAL;
	SC_ALT_COMAN  = SC_ALT or SC_COMAN;
	SC_ALT_SUB    = SC_ALT or SC_SUB;
	SC_ALT_POINT  = SC_ALT or SC_POINT;

{ shortcuts :: Ctrl + special keyborad keys }

  SC_CTRL_EQUAL = SC_CTRL or SC_EQUAL;
	SC_CTRL_COMAN = SC_CTRL or SC_COMAN;
	SC_CTRL_SUB   = SC_CTRL or SC_SUB;
	SC_CTRL_POINT = SC_CTRL or SC_POINT;

{ shortcuts :: Shift + special keyborad keys }

	SC_SHIFT_EQUAL = SC_SHIFT or SC_EQUAL;
	SC_SHIFT_COMAN = SC_SHIFT or SC_COMAN;
	SC_SHIFT_SUB   = SC_SHIFT or SC_SUB;
	SC_SHIFT_POINT = SC_SHIFT or SC_POINT;

{
	WARNING
	-------

	Do not map the numpad keys to shortcuts. The VCL ShortCut functions does not
	accept this values. Try to use the correspondent into normal keyboard.
}

implementation

end.
