NetUpdate component, version 0.9.1, freeware.

Drop this component on one of your application forms, set some
properties, add one line of code and your app users could download
updates from your web site.

This component uses the HTTP client component from the excellent 
freeware ICS Internet Components package. If you don't have this 
package, you can download it from http://users.swing.be/francois.piette/indexuk.htm
Or use the 3 DCU files: httpprot, wsocket and wsockbuf. (Compiled 
with D5).

Tested with Delphi 5.01, you will need to download the ICS package to use it on
D3 or D4.

It's freeware, use at your on risk.

Installation:

pNetUpdate.dpk requires D5 packages, you can edit it for D4 or use 
the component PAS and DCR files + the DCU files and install it to 
some existing package.

Usage, Application side:

 1. Drop the component on the main form, set the SourceURL property 
    to the URL of the update + the name of the update ZIP file.
 2. If AutoExtract is true (default), the component will try to execute
    the users default ZIP program. else it will display A message about
    the downloaded file path.
 3. Use the TargetDir property to select where the downloaded file will be
    saved: AppDir for the application directory, TempDir for the system defined
    temp directory and SaveAsDir for user selected directory.
 4. You can use the OnUpdateEnd event to get the downloaded file name and the
    download process error code, (error = 0) = OK.
 5. Use A menu item or button to call NetUpdate1.Execute.
 6. You can change the SourceURL for A compiled application by adding this text
    file to the app directory:
    File <AppFileName>.inf
    appfilename=http://xxxxx.com/xxxxx/zipfilename

    appfilename and zipfilename in (1) and (6) are without extension.

Usage, Web side:

Place the ZIP file and this text file in the same directory:
File <ZipFileName>.ver
Version=1.0.2.17		// 	the file version value from the project
					options->version info.
If your web server doesn't return the correct size of the ZIP file, you can add
this line:
Size=842578			// 	the ZIP file size in bytes.

The demo contains examples of all of the additional files needed.

Send bug reports to lgm@earthling.net

