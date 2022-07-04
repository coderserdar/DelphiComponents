API_pack Installation Instructions

Opening package to Delphi
First thing on installing the package to Delphi is to open the package file
from the explorer. Find the file as shown in picture below and double
click it to open it in to the Delphi IDE.
When opened into Delphi IDE the package is opened as shown in next
picture:
When you are installing the package for the first time the Install button
at the toolbar is enabled. When Already installed the package contents
will be shown exactly like above picture shows.

Installation Instructions
This document describes the installation and update procedure of the
API_pack Delphi component package. Component’s will be after
installation available to all your Delphi projects on the component
toolbar at the top of the Delphi programming environment.

Installing the package
Before installing the package by clicking Install button on Delphi
package view, you must have copied DLL files to Windows folder. All
DLL files are located under the folder:
After these files are in some folder under the operating system
parameter %path%, you are able to install the package. Now proceed
with clicking the install button in Delphi IDE.
When the compiler has done the job you will be informed with following
dialog:
This contains all the components added to your component tab for easy
usage. Also you will most probably get some warnings:
This is what is supposed to happen. This will cause no problems, but
tells you that some of components has also their own integrated forms
that are not directly in use of your applications.
Your are finished installing the component pack.

Using the Package
Just open new application in delphi and select component to add to your
project, click the position on your application’s form to drop the
component in.
Best way to get the idea behind any component in this package is to try
each components example (they are located in API_example folder).

Updating the Package
Package itself is not being revised with different revision numbers. Only
the individual components will be updated and added, so they are the
one to look for updating. To see the current revision of each component
you must place the component on any form and browse Version from
the property editor window as shown below:
How ever, when there is updates to refresh into the Delphi palette, it’s
done as follows:
First, open the API_pack package file as installing it from the scratch.
Because you have already that installed the install button will be
disabled, so to update information in the palette you must click
Compile button instead.
You’re done with updating the package.