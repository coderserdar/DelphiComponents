Installation in Delphi
Borland Delphi Versions 4 and 5

Download the parser archive
Unpack the ZIP file (xmlparser.zip) to a directory
Add the name of this directory to your Library Path (in Delphi: Tools | Environment Options | Library | Library Path)
Open Package file:
Delphi 4: Open XmlComponents_D4.dpk file with File | Open
Delphi 5: Open XmlComponents_D5.dpk file with File | Open
Click on 'Compile'. Ignore messages regarding .res files.
Click on 'Install'
A new tab 'XML' will appear on the Delphi toolbar with the TXmlScanner component inside
The TXmlParser class resides in the LibXmlParser unit which is a part of the ZIP file.
2001-02-28 Stefan Heymann

Installation and Usage of TXmlScanner in Borland C++Builder
By Rajeev Rajan (Qualcomm Inc, USA)

Version: Borland C++ Builder ver 5 (BCB5) Integrated Development Environment (IDE).

Installation
In the BCB5 IDE, I selected the Component | Install Component menu option, and then the "Into new package" tab.
I browsed the "Unit files" and selected the "LibXmlComps.pas" and "LibXmlParser.pas" files (which are the Pascal component files for the TXmlScanner component)
I then supplied a new name for the package, which will contain the component.
The BCB IDE, then comes up with a message like "Creating and Installing Package". If successful, you should now see the "XmlScanner" component in it's own XML tab, in the BCB IDE's component palette.
Compilation problems
In the LibXmlComps.hpp file, the TExternalEvent property (on line 131), doesn't get properly resolved, and the compiler complains about an improper type name of something.
(I just commented this line. I did not pursue it further. What the comment does is that even if a call-back is associated with the "OnLoadExternal" property in the BCB's Object Inspector, for the TXmlScanner object, it won't be called.)
[This was fixed with the release of 2001-02-28. Please inform me if you still have problems. Stefan Heymann]
There are also some overloading ambiguities between a few elements of the TXmlScanner and the TXmlParser classes, like:
typedef TAttrList TAttrList;
and TXmlParser* usage.
(I just changed the name of the first typedef, and qualified the TXmlParser* with Libxmlparser::TXmlParser.)
Due to paucity of time, I haven't pursued the above problems further, but just hacked around them. I guess I will pursue them, when the need arises or something fishy happens. If someone else does before me, please let me know the solutions. :-)

Usage
Usage of the TXmlScanner object
Drop the XmlScanner into your form.
Associate your application code with the various event call-backs. (For example : OnContent, OnEndTag etc.)
Use the parser as follows:
For example: If xmlScanner is the name of your TXmlScanner object.
//-- Load the input xml file, and call execute.
xmlScanner->LoadFromFile("c:\\myfile.xml");
xmlScanner->Execute();
That's it, as your file is parsed, your various event call-back functions will be automatically called.
Usage of the TXmlParser object from TXmlScanner
If you want to use the TXmlParser component from within TXmlScanner, a pointer to the same is available in the TXmlScanner object. I've given some sample code below on how to use it.

For example:
If XmlScanner1 is the name of your TXmlScanner object:

  //-- Load the input xml file..
  XmlScanner1->LoadFromFile("c:\\myfile.xml");

  //-- Init the scanning.
  XmlScanner1->XmlParser->StartScan();

  //-- Loop thru the file for each item.
  AnsiString s;
  while (XmlScanner1->XmlParser->Scan())
  {
     switch (XmlScanner1->XmlParser->CurPartType)
     {
        case  ptStartTag :      //-- Got Start tag.
          s = XmlScanner1->XmlParser->CurName;
          break;

        case  ptContent :       //-- Got content.
          s = XmlScanner1->XmlParser->CurContent;
          break;

        //-- Add other cases here as required..

        default :
          break;
     }
  }
2000-07-27 Rajeev Rajan
2001-02-28 Stefan Heymann: Added a note on the OnLoadExternal problem