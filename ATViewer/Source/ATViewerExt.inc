procedure ATViewerOptionsReset;
begin
  with ATViewerOptions do
  begin
    ExtText := 'c,cpp,h,hpp,pas,dpr';

    ExtRTF := 'rtf';

    ExtImages :=
      'bmp,ico,wmf,emf,jpg,jpeg,jpe,jfif,png,gif,rle,dib'
      + ',tga,tif,tiff,psd,fax,eps,icb,win,vst,vda'
      + ',pcx,pcc,rpf,rla,sgi,rgba,rgb,bw,pdd,ppm,pgm,pbm,cel,pic,pcd,cut,pal,psp'
      + ',jp2,jpc,pnm,ras,mis' //Jasper
      + ',ani,cur'
      ;

    ExtMedia :=
      'avi,mpg,mpeg,mpe,mpv,mp3,mp2,mp1,m1v,m2v,mpv2,mp2v,mpa,m1a,m2a,asf,lsf,lsx,ivf'
      + ',wav,mid,midi,rmi,kar,aif,aifc,aiff,au,snd'
      + ',mov,qt,3gp,3gp2,3g2,3gpp'
      + ',ra,rm,ram,rmvb,rpm,rt,rp,smi,smil,roq'
      + ',asx,m3u,pls,wvx,wax,wmx'
      + ',wmv,wma,wmp,wm,cda,ac3,dts,vob,ifo,d2v,fli,flc,flic,mka,mkv,ts,tp,tpr,pva,aac,mp4'
      + ',ogm,ogg,divx,vp6,m4a,m4b,vqf,ape,mpc,flac,flv,dvr-ms'
      ;

    ExtWeb :=
      //MSIE:
      'htm,html,xml,xsl,mht'
      //Acrobat, Flash, MS XPS:
      + ',pdf,swf,xps'
      //MSO:
      + ',doc,dot,xls,xlt,pot,pps,ppz,ppt'
      //MSO additional:
      + ',wbk,wps,csv'
      //MSIE:
      + ',shtml,xhtml,shtm,stm,php,php2,php3,asp,pl,vrml,plg,htt,htx'
      //OOo 2.x:
      + ',odt,ods,odp,odg,odf,odb,odm,ott,oth,ots,otg,otp'
      //OOo 1.x:
      + ',sxw,sxc,sxg,sxi,sxd,sxm,stw,stc,std,sti'
      ;
  end;
end;


const
  //Extensions supported by IrfanView but without audio/media clips and icons:
  cIViewDefaultExtensions =
    'aif,ani,cur,awd,b3d,bmp,dib,clp,dds,' +
    'acr,djvu,ecw,emf,wmf,eps,ps,fits,fpx,fsh,g3,gif,' +
    'hdr,icl,iff,lbm,img,gem,jpg,jp2,jpm,kdc,ldf,lwf,med,' +
    'mng,jng,ov,mrsid,nlm,nol,ngg,pbm,pgm,ppm,pcx,dcx,' +
    'photocd,png,psd,psp,pvr,ras,sun,raw,rle,sff,sfw,' +
    'sgi,rgb,tga,tif,tiff,ttf,wad,wbmp,xbm,xpm,crw,cr2,vtf,' +
    'dng,nef,orf,raf,mrw,dcr,x3f,pef,srf,eff,dxf,dwg,hpgl,cgm,svg';

  //ijl15.dll:
  cIJLDefaultExtensions = 'jpg,jpeg,jpe,jfif';

  //Jasper:
  cJp2DefaultExtensions = 'jp2,jpc,pnm,ras,mis';
