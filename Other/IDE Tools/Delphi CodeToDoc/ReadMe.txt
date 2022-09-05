ANN: DelphiCodeToDoc v0.23beta released - Free documentation system for Delphi

The latest version of DelphiCodeToDoc (v0.23b) has been released. It can be downloaded here:

http://downloads.sourceforge.net/dephicodetodoc/DelphiCodeToDoc_exe_v0.23b.zip

DelphiCodeToDoc is a free documentation system for Delphi, released under the GNU General Public License. It uses information about source code symbols and formatted comments in files to produce accurate documentation from your application and component.. The JavaDoc syntax is now supported

http://dephicodetodoc.sourceforge.net

What are the change in the last revision ?
Last revision v0.23beta include the modifications listed below :
* Fixed Parser error with empty @param description (Tracker 2781298)
* Fixed some files not parsed due to specific NTFS attribute (Tracker 2523851)
* Fixed Add option to exclude resourcestrings (Tracker 2736801, 1106583)
* Fixed tag quality dont follow "output filtering category" config (Tracker 1855474)
* Added Pdf generator


What are the known bugs of the software ?
*Some messages are not translated.
*Classes differentiates Fields and Vars, which should be merged.


What are the known limitations of the software ?
*Class vars can be comment only with an inline comment just after the declaration
*Only English language is support for generated documentation
*@see TAG in not implemented
*When a project is open, changing the language doesn't work for the configuration page


What will be include or change in the next revisions ?
* Fix critical and major bugs
