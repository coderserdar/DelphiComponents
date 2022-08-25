TDESCrypt component
===================

11th September 2012

TDESCrypt component for Delphi 3, 4, 5, 6, 7, 2005, 2006, 2007, 2009, 2010
XE, XE2 and XE3, 32-bit and 64-bit platforms, was created by Herzog Samuel,
that encrypts a given input string by using DES.The code has been translated
from crypt.c (Unix GNU Library).

The component allows user names and passwords to be encoded into a format
suitable for use with UNIX systems, in particular for the Apache web server
running on UNIX to restrict access to your web site to authorised users.
This allows new users to be added to the .htpasswd file locally on the PC,
without needing to run a script while online to encode the passwords.

Note the maximum length of the source password is eight characters,
and the salt is two characters.  Anything longer is truncated.  The
encoded password is always 13 characters long.  The salt is designed
to frustrate looking up passwords using rainbox tables and should ideally
be random.

The Delphi 2009 and later versions support AnsiString only, not Unicode.


Copyright Information
---------------------

TDESCrypt is copyright Herzog Samuel, Switzerland.
E-Mail: sam_herzog@yahoo.com
Web: http://www.novabit.ch

TDESCrypt is Freeware.

Tested with Delphi 4 and later by Magenta Systems Ltd.

Email: delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/




