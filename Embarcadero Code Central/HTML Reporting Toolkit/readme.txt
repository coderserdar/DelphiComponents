WebPack HTML Reporting Toolkit


Semyon A. Chertkov
E-mail: mailto:semyonc@yahoo.com
Keyworfs: HTML XML Reporting URLMON AsyncPP  Pluggable Protocol Handler




Introduction

HTML is a perfect means of representation the interactive reports and gives unique opportunities of construction the user interfaces, as for example Microsoft Active Desktop or Microsoft Outlook Express. Unfortunately, usual use HTML assumes presence dedicated WEB server. Sometimes also it is useful to trace of the explorer requests and it is possible to redefine some resources directly from the client application. For decision of these tasks in Microsoft Internet Explorer since the version 4.0 are stipulated Asynchronous Pluggable Protocol Handlers.

Purposes

The offered library contains implementation of asynchronous pluggable protocol handler which allows quickly to realize a virtual site directly from application and as to supervise the Internet Explorer requests to external resources. The processing URL is implemented similarly standard Delphi TWebDispatcher which is used from ISAPI and CGI applications. For HTML generation instead of TPageProducer is used XSL/XSLT. Is supported as standard MS XML of the version 2.0, and MS XML 3.0 Beta.

In WebPack is also included TWebBrowserControl inherited  from TWebBrowser which contains additional properties and events accessible through the interface IDocHostUIHandler and ambient properties. It allows in particular to implement support of the pop-up menus, window.external  property etc.

Examples

WebPack includes three demonstration applications which illustrate opportunities of library.
FishFact the elementary example of use WebPack for generation HTML from the database.
HookUrl monitoring and interception of web browser requests. 
MastApp “Marine Adventures Order Entry” with the interface styled under MS Outlook Express,
demonstrates using XSL for HTML generating and integration web browser control with the user application.


Requirements

For use WebPack it is required Microsoft Internet Explorer  versions 5.0. For examples is necessary BDE and DBDEMOS alias on the demonstration tables delivered together with Delphi. For use XSLT is necessary Microsoft XML Parser SDK Beta.

Notes

With the help TWebProvider it is possible to overload as the existing protocols, and to define new. At overlapping the existing protocol URLMON will cause IInternetProtocolInfo of the original protocol, instead of user. Therefore there can be some problems at use hyperlinks on the custom-made protocols, such as mk.
Used XML parser is determined by property XMLParserLevel. For XML generation it is possible to use the interface IXMLDOMDocument with property TResponseObject.Document or more preferably auxiliary methods TResponseObject.createElement, TResponseObject.endElement etc.
