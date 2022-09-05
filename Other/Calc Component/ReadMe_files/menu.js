nn = (document.getElementById && navigator.appName == "Netscape") ? 1 : 0;
ns = (document.layers) ? 1 : 0;
ie = (!nn && document.all) ? 1 : 0;
hidemenu = null;
tc = 80;

function getx(elem)
{
 x = 0;
 do { x += elem.offsetLeft; }
 while((elem = elem.offsetParent) != null);
 return x;
}

function hide(num)
{
 if(ie) document.all["g" + num].style.visibility = "hidden";
 if(ns) document.layers["g" + num].visibility = "hidden";
 if(nn) document.getElementById("g" + num).style.visibility = "hidden";
}

function timehide() { hidemenu = setTimeout('hideall()', 500); }

function show(num)
{
 hideall();
 clearTimeout(hidemenu);

 if(num == 1) mc = 6;
 if(num == 2) mc = 11;
 if(num == 3) mc = 11;
 if(num == 4) mc = 8;
 
 if(ie)
 {
  lc = getx(document.images["v" + num]);
  document.all["g" + num].style.top = tc;
  document.all["g" + num].style.left = lc-mc;
  document.all["g" + num].style.visibility = "visible";
 }
 if(ns)
 {
  lc = document.images["v" + num].x;
  document.layers["g" + num].top = tc;
  document.layers["g" + num].left = lc-mc;
  document.layers["g" + num].visibility = "visible";
 }
 if(nn)
 {
  lc = getx(document.getElementById("j" + num));
  document.getElementById("g" + num).style.top = tc;
  document.getElementById("g" + num).style.left = lc-mc;
  document.getElementById("g" + num).style.visibility = "visible";
 }
}

function hideall() { hide('1'); hide('2'); hide('3'); hide('4'); }
function mon() { clearTimeout(hidemenu); }
function moff() { hidemenu = setTimeout('hideall()', 500); }