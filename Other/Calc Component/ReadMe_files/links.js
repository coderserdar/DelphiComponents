function addhref(url, text)
{
document.write('<a href="'+url+'">'+text+'</a>');
}

function addhrefblank(url, text)
{
document.write('<a href="'+url+'" target=_blank>'+text+'</a>');
}


function addmailto(domain, name, text)
{
document.write('<a href="mailto:'+name+'@'+domain+'">'+text+"</a>")
}
