CalcExpress: README
=============================

Please read this file carefully (especially the INSTALLATION
chapter) before installing the program to your computer.


Contents
--------

  Program information
  Company information
  Description
  Specification
  Installation
  Registration
  Copyright and license
  Technical support

Program information
-------------------

Program Name:
  CalcExpress
Program Version:
  v.2.1
  (Ported to Kylix by Dmitri Vorobiev)
Program Release Date:
  06/08/2001
Program Description:
  CalcExpress is an interpreter for quick and easy evaluation of
  mathematical expressions. It is a smart tool easy in use.
  Supports 5 operators, parenthesis, 18 mathematical functions
  and user-defined variables
Target Enviroment:
  Delphi 4,5,6,7; C++ Builder 4,5,6; Kylix 2.0
Software type:
  Freeware with source code


Company information
-------------------

Company Name:
  AidAim Software
Contact E-mail Address:
  support@aidaim.com
Contact WWW URL:
  http://www.aidaim.com
Ported to Kylix by Dmitri Vorobiev:
  http://www.ee.oulu.fi/~dmvo/

Description
-----------

 CalcExpress is an interpreter for quick and easy evaluation of
mathematical expressions. It is a smart tool easy in use.
Supports 5 operators, parenthesis, 18 mathematical functions
and user-defined variables


Specification
-------------

works with real numbers; 
accepts operators: + - * / ^; 
accepts functions: cos, sin, tg, ctg, abs, sgn or sign, sqrt, ln,
                   exp, arcsin, arccos, arctg or arctan, arcctg, 
		   sh or sinh, ch or cosh, th or tanh,
                   cth or coth, heaviside; 
supports unlimited number of user defined variables


Installation
------------

Delphi & C++ Builder:

  In the Delphi or C++Builder component menu, choose :

  1) Install Component
 
  2) Into new package

  3) Choose package filename (for example NEW_PACK.DPK)
 
  4) Browse for unit filename CalcExpress.pas

  5) Choose Compile & Install


Kylix 2.0:

The following procedure describes per-user installation of CalcExpress.

  In Kylix:
  0) Make sure you do not have any projects open (File->Close All)
  1) Make a directory named CalcExpress as a subdirectory of ~/.borland
     This can be done either from any file manager or from command line.
     The latter requires issuing the following sequence of commands:
     
     $ cd ~/.borland
     $ mkdir CalcExpress 
  
  2) Copy CalcExpress.pas and CalcExpress.dcr into ~/.borland/CalcExpress
  3) In the Component Menu, choose:
  4) Install Component
  5) Into existing package
  6) Click Unit File Name -> Browse button and navigate to ~/.borland/CalcExpress/CalcExpress.pas
     Note: By default, the directory dialog in Kylix does not show hidden directories. Click the
     right mouse button and choose 'Show hidden files' from the drop-down menu.
  7) Press Ok, and on the dialog, which asks "Package user.so will be rebuilt" clisk Yes.
  8) On the following information dialog click OK.
  9) Choose File -> Close All.
 10) IMPORTANT! Save changes to the project user when prompted (click Yes when prompted).
 11) Add the path to CalcExpress to your library path. From Kylix menu, choose 
     Tools -> Environment options. Click Library tab and ellipsis (...) button next to
     the Library path text area and add a directory /home/user/.borland/CalcExpress
     to the list of library directories; please change 'user' in the path above to your 
     real user name in your Linux system.


Registration
------------

No registration needed


Copyright and license
---------------------

See "license.txt" file.


Technical support
-----------------

Before you contact us, please do the following:

- Make sure you have performed all the required steps correctly. 
- Visit our Internet site at http://www.aidaim.com.
  It's a good chance that you'll find the newer version of 
  our product there.

If the problem with CalcExpress persists and noting else helps,
please contact technical support at support@aidaim.com.

Please inform us about the following:

  - CalcExpress version 
  - where did you ostain CalcExpress (http or ftp site)
  - computer information: CPU type and speed, installed memory
  - enviromental information: your OS and compiler
  - description of your problem (as much information as possible
    to retrieve the problem)

CalcExpress is free and distributed "as is". Should you have any ideas 
on improving the existing functions of this product after you have
downloaded and used it, 
be easy to e-mail us: support@aidaim.com.
