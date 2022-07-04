              *******************************************
              *    Component Naming Assistant (CNA)     *
              *                                         *
              *          by TheUnknownOnes              *
              *           chaosben@web.de               *
              *          MarcoWarm@gmx.net              *
              *******************************************

What is it and what does it do?
===============================

CNA is and IDE expert for Delphi 6,7 and 2005. It helps the programmer with 
naming and initializing his components as he drops them on the form.

How to install?
===============

IMPORTANT!!!
Before you install CNA you have to close all open form designers!!!

Simply copy the File CNAExpertxx.bpl in a folder of your choice and choose
Component->Install packages from the Delphi menu.

What now?
=========

CNA registers itself in the menubar of Delphi under Tools. This menu item will 
start the configuration dialog. 
If you use Delphi 6 or 7 you will be able to change the active profile with 
this menuitem.
If you use Delphi 2005, there will also appear a button above the palette next 
to the component filter button. With this button you can easily change the
current profile (see below) and start the configuration.

Profiles ... what are they and why should you use them?
=======================================================

Profiles help you managing different settings you may want to use. Suppose you
want to name your buttons btn_ThisAndThat in one Project and ButThisAndThat in 
another. You simply create two profiles and in each one you change the settings 
as you wish. By using the button you can easily change between these two
profiles.

Groups ... there is no mystery behind them
==========================================

Each profile holds at least one component group. Such a group is the summary of
the properties of the components contained in it. Simply choose a compontent 
from the combobox and add it to a new or existing group. A group will only 
allow you to edit properties of its members that are common to each of them. 
That means you are not able to change the default Caption of a TButton if there
is a TEdit in the same group.

Now let's change some properties
================================

First of all you should create a new profile by right clicking in the treeview.
By selecting a group in the treeview you will see the list with all available
properties and whether they are set and with what default value. Just double 
click on one of them and you will be able to change it:

Clear Value: This property will not be initialized
Show Input Dialog: Before the property is changed CNA will ask you to edit your 
preference

Some issues you may stumble over ... What if
============================================

Q  ... I am perfectly sure that I have a certain component in my list but I do
   not see it in the combobox?

A  This may be due to the new feature of Delphi 2005 that certain packages are
   loaded on demand. Simply drop the desired component on your form and you 
   will be able to modify it.
   see Allen Bauer's blog for details:
       http://blogs.borland.com/abauer/archive/2004/11/15/1833.aspx

Q  ... an error occures again and again but is not cause by me?

A  Please close Delphi and overwrite the file CNAExpertxx.bpl with
   CNAExpertxx.bpl from the english_debug directory of this archive. Restart 
   Delphi and provocate the error. Now a customized exception dialog should
   appear allowing you to send us a bug report. Please do that according to the 
   instructions. Then, after closing Delphi, you should restore the original 
   CNAExpert.bpl.
                 
-------------------------------------------------------------------------------
Orthographic and/or grammatical mistakes are intended for amusing the reader.
