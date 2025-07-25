unit unit_inspector_plot;

{Copyright (C) 2018, 2022 by Han Kleijn, www.hnsky.org
 email: han.k.. at...hnsky.org

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. }

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLintf, StdCtrls,
  Buttons, math, astap_main, unit_stack, unit_annotation,
  clipbrd, ExtCtrls,{for copy to clipboard}
  unit_gaussian_blur ;

type

  { Tform_inspection1 }

  Tform_inspection1 = class(TForm)
    background_contour1: TBitBtn;
    bayer_label1: TLabel;
    bin_factor1: TComboBox;
    gradations1: TComboBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    grid_size1: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    to_clipboard1: TCheckBox;
    show_distortion1: TBitBtn;
    aberration_inspector1: TBitBtn;
    tilt1: TBitBtn;
    background_values1: TBitBtn;
    extra_stars1: TCheckBox;
    contour1: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    help_uncheck_outliers1: TLabel;
    hfd_button1: TButton;
    rectangle1: TRadioButton;
    triangle1: TRadioButton;
    roundness_button1: TButton;
    measuring_angle1: TComboBox;
    undo_button1: TBitBtn;
    values1: TCheckBox;
    vectors1: TCheckBox;
    voronoi1: TCheckBox;
    procedure aberration_inspector1Click(Sender: TObject);
    procedure background_values1Click(Sender: TObject);
    procedure background_contour1Click(Sender: TObject);
    procedure bin_factor1Change(Sender: TObject);
    procedure close_button1Click(Sender: TObject);
    procedure contour1Change(Sender: TObject);
    procedure extra_stars1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure gradations1Change(Sender: TObject);
    procedure grid_size1Change(Sender: TObject);
    procedure help_uncheck_outliers1Click(Sender: TObject);
    procedure roundness1Click(Sender: TObject);
    procedure measuring_angle1Change(Sender: TObject);
    procedure show_distortion1Click(Sender: TObject);
    procedure tilt1Click(Sender: TObject);
    procedure triangle1Change(Sender: TObject);
    procedure undo_button1Click(Sender: TObject);
    procedure values1Change(Sender: TObject);
    procedure vectors1Change(Sender: TObject);
    procedure voronoi1Change(Sender: TObject);
  private

  public

  end;

var
  form_inspection1: Tform_inspection1;


//type
//  hfd_array   = array of array of integer;

var
  contour_check: boolean=false;
  voronoi_check: boolean=false;
  values_check: boolean=true;
  vectors_check: boolean=true;
  extra_stars  : boolean=false;
  three_corners: boolean=false;
  measuring_angle : string='0';
  insp_left: integer=100;
  insp_top: integer=100;
  inspector_binning: integer=1; //index
  inspector_grid_size: integer=30;
  inspector_gradations: integer=10;


function CCDinspector(img:Timage_array; headx:theader;const memo : tstrings; snr_min: double; screenplot,triangle : boolean; measuring_angle: double) : double;


implementation
{$R *.lfm}

var
   executed : integer; {1 image changed (refresh required), 2 array changed(restore required)}

   toClipboard1: boolean=false;

function fnmodulo2(x,range: double):double;   {specifiy range=2*pi fore -pi..pi or range=360 for -180.. 180}
begin
  result:=x;
  while result<-range/2 do result:=result+range;
  while result>+range/2 do result:=result-range;
end;

procedure flip_xy(fliph,flipv :boolean; var x,y : integer);{flip if required for plotting. From array to image1 coordinates}
begin
 if Fliph       then x:=head.width-x;
 if Flipv=false then y:=head.height-y;
end;



function CCDinspector(img:Timage_array; headx:theader;const memo : tstrings; snr_min: double; screenplot,triangle : boolean; measuring_angle: double) : double;
var
  fitsX,fitsY,size,radius, i,j,starX,starY, retries,max_stars,x_centered,y_centered,starX2,starY2,len,
  nhfd,nhfd_outer_ring,fontsize,text_height,text_width,n,m,xci,yci,sqr_radius,left_margin,
  nhfd_11,nhfd_21,nhfd_31,
  nhfd_12,nhfd_22,nhfd_32,
  nhfd_13,nhfd_23,nhfd_33,
  x_11,x_21,x_31,y_11,y_21,y_31,
  x_12,x_22,x_32,y_12,y_22,y_32,
  x_13,x_23,x_33,y_13,y_23,y_33,
  oldNaxis3, dummy{,screenbottom},th,tw,th2,tw2,smx,smy,starpixels   : integer;

  hfd1,star_fwhm,snr,flux,xc,yc, median_worst,median_best,scale_factor, detection_level,
  hfd_min,tilt_value, theangle,theradius,screw1,screw2,screw3,sqrradius,raM,decM,
  fwhm_median,sm_x,sm_y,
  hfd_median, median_outer_ring,
  median_11, median_21, median_31,
  median_12, median_22, median_32,
  median_13, median_23, median_33,
  backgr, noise_level                : double;
  hfd_list, hfdlist_outer_ring,
  hfdlist_11,hfdlist_21,hfdlist_31,
  hfdlist_12,hfdlist_22,hfdlist_32,
  hfdlist_13,hfdlist_23,hfdlist_33,
  fwhm_list                            : array of double;

  starlistXY    :array of array of double;
  mess1,mess2,hfd_value,hfd_arcsec,report,rastr,decstr,magstr,fwhm_value,fwhm_arcsec : string;

  Fliph, Flipv,restore_req  : boolean;
  img_bk,img_sa             : Timage_array;
  data_max: single;
    procedure textout_octogram(x,y: integer;value : double); //center the text to the middle of the octogram
    var
       txt : string;
    begin
      with mainform1.image1.canvas do
      begin
        txt:=floattostrF(value,ffFixed,0,2);
        th:= textheight(txt);//not efficient but it is done only 9 times
        tw:= textwidth(txt);
        if x>smx then x:=x-tw //align right
        else
        if x=smx then x:=x-(tw div 2); //align centered

        if y=smy then y:=y-(th div 2) //align vertical centered
        else
        if y>smy then y:=y-th; //text above Y
        textout(x,y,txt);
      end;
    end;

begin
  result:=100; //default indicating an error
  if headx.naxis=0 then exit; {file loaded?}
  Screen.Cursor:=crHourglass;{$IfDef Darwin}{$else}application.processmessages;{$endif}// Show hourglass cursor, processmessages is for Linux. Note in MacOS processmessages disturbs events keypress for lv_left, lv_right key

  if screenplot then memo2_message('Inspection of: '+filename2);//else in the batch routine
  restore_req:=false;
  oldNaxis3:=headx.naxis3;//for case it is converted to mono


  if headx.naxis3>1 then {colour image}
  begin
    img_bk:=duplicate(img);//fastest way to duplicate an image
    convert_mono(img,headx);
    get_hist(0,img);{get histogram of img and his_total. Required to get correct background value}

    restore_req:=true;
  end
  else
  if (bayerpat<>'') then {raw Bayer image}
  begin
    img_bk:=duplicate(img);//fastest way to duplicate an image
    check_pattern_filter(img);
    get_hist(0,img);{get histogram of img and his_total. Required to get correct background value}
    restore_req:=true;
  end;

  max_stars:=500;
  len:=4*max_stars; {should be enough, if not size is adapted}

  with mainform1 do
  begin
    smx:=headx.width div 2;
    smy:=headx.height div 2;

    sm_x:=headx.width/2;
    sm_y:=headx.height/2;

    Flipv:=mainform1.flip_vertical1.Checked;
    Fliph:=mainform1.Flip_horizontal1.Checked;


    image1.Canvas.Pen.Mode := pmMerge;
    image1.Canvas.brush.Style:=bsClear;
    image1.Canvas.font.color:=clyellow;
    image1.Canvas.Pen.Color := clred;
    image1.Canvas.Pen.width := round(1+headx.height/image1.height);{thickness lines}
    fontsize:=round(max(10,8*headx.height/image1.height));{adapt font to image dimensions}
    image1.Canvas.font.size:=fontsize;

    hfd_median:=0;
    median_outer_ring:=0;

    median_11:=0;
    median_21:=0;
    median_31:=0;

    median_12:=0;
    median_22:=0;
    median_32:=0;

    median_13:=0;
    median_23:=0;
    median_33:=0;

    SetLength(hfd_list,len);{set array length on a starting value}
    SetLength(fwhm_list,len);{set array length on a starting value}
    SetLength(starlistXY,3,len);{x,y positions}

    setlength(img_sa,1,headx.height,headx.width);{set length of image array}

    hfd_min:=max(0.8 {two pixels},strtofloat2(stackmenu1.min_star_size_stacking1.caption){hfd});{to ignore hot pixels which are too small}
    get_background(0,img,headx,{cblack=0} screenplot=false{histogram is available if in viewer},true {calculate noise level});{calculate background level from peek histogram}

    data_max:=headx.datamax_org-1;
    backgr:=headx.backgr;
    noise_level:=headx.noise_level;

    retries:=3; {try up to four times to get enough stars from the image}
    repeat
      if retries=3 then
        begin if headx.star_level >30*noise_level then detection_level:=headx.star_level  else retries:=2;{skip} end;//stars are dominant
      if retries=2 then
        begin if headx.star_level2>30*noise_level then detection_level:=headx.star_level2 else retries:=1;{skip} end;//stars are dominant
      if retries=1 then
        begin detection_level:=30*noise_level; end;
      if retries=0 then
        begin detection_level:= 7*noise_level; end;
      nhfd:=0;{set counters at zero}

      for fitsY:=0 to headx.height-1 do
        for fitsX:=0 to headx.width-1  do
          img_sa[0,fitsY,fitsX]:=-1;{mark as star free area}

      for fitsY:=1 to headx.height-1-1  do //Search through the image. Stay one pixel away from the borders.
      begin
        for fitsX:=1 to headx.width-1-1 do
        begin
          if (( img_sa[0,fitsY,fitsX]<=0){area not occupied by a star}  and (img[0,fitsY,fitsX]- backgr>detection_level){star}) then {new star}
          begin
            starpixels:=0;
            if img[0,fitsY,fitsX-1]- backgr>4*noise_level then inc(starpixels);//inspect in a cross around it.
            if img[0,fitsY,fitsX+1]- backgr>4*noise_level then inc(starpixels);
            if img[0,fitsY-1,fitsX]- backgr>4*noise_level then inc(starpixels);
            if img[0,fitsY+1,fitsX]- backgr>4*noise_level then inc(starpixels);
            if starpixels>=2 then //At least 3 illuminated pixels. Not a hot pixel
            begin
              HFD(img,fitsX,fitsY,14 {annulus radius},99 {flux aperture restriction},0 {adu_e}, hfd1,star_fwhm,snr,flux,xc,yc);{star HFD and FWHM}

              if ((hfd1<=30) and (snr>snr_min {30}) and (hfd1>hfd_min) ) then
              begin

                radius:=round(3.0*hfd1);{for marking star area. A value between 2.5*hfd and 3.5*hfd gives same performance. Note in practice a star PSF has larger wings then predicted by a Gaussian function}
                sqr_radius:=sqr(radius);
                xci:=round(xc);{star center as integer}
                yci:=round(yc);

                for n:=-radius to +radius do {mark the whole circular star area as occupied to prevent double detection's}
                for m:=-radius to +radius do
                begin
                  j:=n+yci;
                  i:=m+xci;
                  if ((j>=0) and (i>=0) and (j<headx.height) and (i<headx.width) and (sqr(m)+sqr(n)<=sqr_radius)) then
                    img_sa[0,j,i]:=1;
                end;

                if ((img[0,yci,  xci  ]<data_max) and
                    (img[0,yci,  xci-1]<data_max) and
                    (img[0,yci,  xci+1]<data_max) and
                    (img[0,yci-1,xci  ]<data_max) and
                    (img[0,yci+1,xci  ]<data_max) and

                    (img[0,yci-1,xci-1]<data_max) and
                    (img[0,yci+1,xci-1]<data_max) and
                    (img[0,yci-1,xci+1]<data_max) and
                    (img[0,yci+1,xci+1]<data_max)  ) then //not saturated
                begin
                  {store values}
                  hfd_list[nhfd]:=hfd1;
                  fwhm_list[nhfd]:=star_fwhm;
                  starlistXY[0,nhfd]:=xc; {store star position in image coordinates, not FITS coordinates}
                  starlistXY[1,nhfd]:=yc;
                  starlistXY[2,nhfd]:=flux;
                  inc(nhfd); if nhfd>=length(hfd_list) then
                  begin
                    SetLength(hfd_list,nhfd+max_stars); {adapt length if required and store hfd value}
                    SetLength(fwhm_list,nhfd+max_stars); {adapt length if required and store hfd value}
                    SetLength(starlistXY,3,nhfd+max_stars);{adapt array size if required}
                  end;

                end;
              end;
            end;
          end;
        end;
      end;

      dec(retries);{prepare for trying with lower detection level}
    until ((nhfd>=max_stars) or (retries<0));{reduce detection level till enough stars are found. Note that faint stars have less positional accuracy}

    if restore_req then {raw Bayer image or colour image}
    begin
      memo2_message('Restoring image');
      img:=duplicate(img_bk);//fastest way to duplicate an image
      img_bk:=nil;
      headx.naxis3:=oldNaxis3;
      get_hist(0,img);{get histogram of img and his_total}
    end;



    if nhfd>0 then  {count the stars for each area}
    begin
      SetLength(hfdlist_outer_ring,nhfd);{space for all stars}
      SetLength(hfdlist_11,nhfd);{space for all stars}
      SetLength(hfdlist_21,nhfd);{space for all stars}
      SetLength(hfdlist_31,nhfd);

      SetLength(hfdlist_12,nhfd);
      SetLength(hfdlist_22,nhfd);
      SetLength(hfdlist_32,nhfd);

      SetLength(hfdlist_13,nhfd);
      SetLength(hfdlist_23,nhfd);
      SetLength(hfdlist_33,nhfd);

      nhfd_11:=0;
      nhfd_21:=0;
      nhfd_31:=0;

      nhfd_12:=0;
      nhfd_22:=0;{center}
      nhfd_32:=0;

      nhfd_13:=0;
      nhfd_23:=0;
      nhfd_33:=0;

      nhfd_outer_ring:=0;

      if triangle then
      begin
        screw1:=fnmodulo2(measuring_angle,360); {make -180 to 180 range}
        screw2:=fnmodulo2(measuring_angle+120,360);
        screw3:=fnmodulo2(measuring_angle-120,360);
      end;

      for i:=0 to nhfd-1 do {plot rectangles later since the routine can be run three times to find the correct detection_level and overlapping rectangle could occur}
      begin
        hfd1:=hfd_list[i];
        starX:=round(starlistXY[0,i]);
        starY:=round(starlistXY[1,i]);

        if screenplot then
        begin
          size:=round(5*hfd1);
          starX2:=starX;
          starY2:=starY;
          flip_xy(fliph,flipv,starX2,starY2); {from array to image coordinates}
          mainform1.image1.Canvas.Rectangle(starX2-size,starY2-size, starX2+size, starY2+size);{indicate hfd with rectangle}
          mainform1.image1.Canvas.textout(starX2+size,starY2+size,floattostrf(hfd1, ffFixed, 0,2));{add hfd as text}
        end;

        //the nine areas. FITS 1,1 is left bottom:
        //13   23   33
        //12   22   32
        //11   21   31

        if  sqr(starX - (smx) )+sqr(starY - (smy))>sqr(0.75)*(sqr(smx)+sqr(smy)) then begin hfdlist_outer_ring[nhfd_outer_ring]:=hfd1; inc(nhfd_outer_ring); end;{store out ring (>75% diameter) HFD values}
        if triangle=false then
        begin
          if ( (starX<(headx.width*1/3)) and (starY<(headx.height*1/3)) ) then begin  hfdlist_11[nhfd_11]:=hfd1;  inc(nhfd_11); end;{store corner HFD values}
          if ( (starX>(headx.width*2/3)) and (starY<(headx.height*1/3)) ) then begin  hfdlist_31[nhfd_31]:=hfd1;  inc(nhfd_31); if nhfd_31>=length(hfdlist_31) then SetLength(hfdlist_31,nhfd_31+500);end;
          if ( (starX>(headx.width*2/3)) and (starY>(headx.height*2/3)) ) then begin  hfdlist_33[nhfd_33]:=hfd1;  inc(nhfd_33); if nhfd_33>=length(hfdlist_33) then SetLength(hfdlist_33,nhfd_33+500);end;
          if ( (starX<(headx.width*1/3)) and (starY>(headx.height*2/3)) ) then begin  hfdlist_13[nhfd_13]:=hfd1;  inc(nhfd_13); if nhfd_13>=length(hfdlist_13) then SetLength(hfdlist_13,nhfd_13+500);end;

          if ( (starX>(headx.width*1/3)) and (starX<(headx.width*2/3)) and (starY>(headx.height*2/3))                              ) then begin  hfdlist_23[nhfd_23]:=hfd1;  inc(nhfd_23); end;{store corner HFD values}
          if (                              (starX<(headx.width*1/3)) and (starY>(headx.height*1/3)) and (starY<(headx.height*2/3))) then begin  hfdlist_12[nhfd_12]:=hfd1;  inc(nhfd_12); end;{store corner HFD values}
          if ( (starX>(headx.width*1/3)) and (starX<(headx.width*2/3)) and (starY>(headx.height*1/3)) and (starY<(headx.height*2/3))) then begin  hfdlist_22[nhfd_22]:=hfd1;  inc(nhfd_22); end;{square center}
          if ( (starX>(headx.width*2/3))                              and (starY>(headx.height*1/3)) and (starY<(headx.height*2/3))) then begin  hfdlist_32[nhfd_32]:=hfd1;  inc(nhfd_32); end;{store corner HFD values}
          if ( (starX>(headx.width*1/3)) and (starX<(headx.width*2/3)) and                               (starY<(headx.height*1/3))) then begin  hfdlist_21[nhfd_21]:=hfd1;  inc(nhfd_21); end;{store corner HFD values}

        end
        else
        begin  {triangle. Measured in a circle divided by three sectors of 120 degrees except for the circular center}
          x_centered:=starX- (smx); {array coordinates}
          y_centered:=starY- (smy);
          theangle:=arctan2(x_centered,y_centered)*180/pi;{angle in array from Y axis. So swap x, y}
          sqrradius:=sqr(x_centered)+sqr(x_centered);
          theradius:=sqrt(sqrradius);

          if  sqrradius<=sqr(0.75)*(sqr(smx)+sqr(smy)) then {within circle}
          begin
            if  sqrradius>=sqr(0.25)*(sqr(smx)+sqr(smy))  then {outside center}
            begin
              if ( (abs(fnmodulo2(theangle-screw1,360))<30) and (theradius<smy) ) then begin  hfdlist_11[nhfd_11] :=hfd1; inc(nhfd_11);if nhfd_11>=length(hfdlist_11) then SetLength(hfdlist_11,nhfd_11+1000);end;{sector 1}
              if ( (abs(fnmodulo2(theangle-screw2,360))<30) and (theradius<smy) ) then begin  hfdlist_21[nhfd_21]:=hfd1;  inc(nhfd_21);if nhfd_21>=length(hfdlist_21) then SetLength(hfdlist_21,nhfd_21+1000);end;{sector 2}
              if ( (abs(fnmodulo2(theangle-screw3,360))<30) and (theradius<smy) ) then begin  hfdlist_31[nhfd_31]:=hfd1;  inc(nhfd_31);if nhfd_31>=length(hfdlist_31) then SetLength(hfdlist_31,nhfd_31+1000);end;{sector 3}
            end
            else
            begin  hfdlist_22[nhfd_22]:=hfd1;  inc(nhfd_22);end;{round center}
          end;
        end
      end;

      if ((nhfd_22>0) and (nhfd_outer_ring>0)) then  {enough information for curvature calculation}
      begin
        median_22:=SMedian(hfdlist_22,nhfd_22);
        median_outer_ring:=SMedian(hfdlist_outer_ring,nhfd_outer_ring);
        mess1:='  Off-axis aberration[HFD]='+floattostrF(median_outer_ring-(median_22),ffFixed,0,2);{}
      end
      else
      mess1:='';

      hfd_median:=SMedian(hfd_List,nhfd {use length});
      fwhm_median:=SMedian(fwhm_List,nhfd {use length});


      if ((triangle=true) and (nhfd_11>0)  and (nhfd_21>0) and (nhfd_31>0)) then  {enough information for tilt calculation}
      begin
        median_11:=SMedian(hfdlist_11,nhfd_11);{screw 1}
        median_21:=SMedian(hfdlist_21,nhfd_21);{screw 2}
        median_31:=SMedian(hfdlist_31,nhfd_31);{screw 3}

        median_best:=min(median_11,min(median_21,median_31));{find best corner}
        median_worst:=max(median_11,max(median_21,median_31));{find worst corner}

        scale_factor:=headx.height*0.4/median_worst;

        x_11:=round(median_11*scale_factor*sin(screw1*pi/180)+sm_x); {screw 1}
        y_11:=round(median_11*scale_factor*cos(screw1*pi/180)+sm_y);{calculate coordinates, based on rotation distance from Y axis}


        x_21:=round(median_21*scale_factor*sin(screw2*pi/180)+sm_x); {screw 2}
        y_21:=round(median_21*scale_factor*cos(screw2*pi/180)+sm_y);{calculate coordinates, based on rotation distance from Y axis}

        x_31:=round(median_31*scale_factor*sin(screw3*pi/180)+sm_x);{screw 3}
        y_31:=round(median_31*scale_factor*cos(screw3*pi/180)+sm_y);{calculate coordinates, based on rotation distance from Y axis}

        flip_xy(fliph,flipv,x_11,y_11); {from array to image coordinates}
        flip_xy(fliph,flipv,x_21,y_21);
        flip_xy(fliph,flipv,x_31,y_31);


        result:=median_worst-median_best; //for export

        tilt_value:=100*(median_worst-median_best)/hfd_median;
        mess2:='  Tilt[HFD]='+floattostrF(result,ffFixed,0,2)+' ('+floattostrF(tilt_value,ffFixed,0,0)+'%';{estimate tilt value}
        if tilt_value<5 then mess2:=mess2+' none)'
        else
        if tilt_value<10 then mess2:=mess2+' almost none)'
        else
        if tilt_value<15 then mess2:=mess2+' mild)'
        else
        if tilt_value<20 then mess2:=mess2+' moderate)'
        else
        if tilt_value<30 then mess2:=mess2+' severe)'
        else
        mess2:=mess2+' extreme)';

        if screenplot then//plot octagon
        begin
          image1.Canvas.Pen.width :=image1.Canvas.Pen.width*2;{thickness lines}

          image1.Canvas.pen.color:=clyellow;


          image1.Canvas.moveto(x_11,y_11);{draw triangle}
          image1.Canvas.lineto(x_21,y_21);{draw triangle}
          image1.Canvas.lineto(x_31,y_31);{draw triangle}
          image1.Canvas.lineto(x_11,y_11);{draw triangle}

          image1.Canvas.lineto(smx,smy);{draw diagonal}
          image1.Canvas.lineto(x_21,y_21);{draw diagonal}
          image1.Canvas.lineto(smx,smy);{draw diagonal}
          image1.Canvas.lineto(x_31,y_31);{draw diagonal}

          fontsize:=fontsize*4;
          image1.Canvas.font.size:=fontsize;
          image1.Canvas.font.style:=[fsbold];

          th:= image1.Canvas.textheight('5');
          tw:= image1.Canvas.textwidth('2.35');
          tw2:=tw div 2;
          th2:=th div 2;

          image1.Canvas.textout(x_11-tw2, y_11-th2,floattostrF(median_11,ffFixed,0,2));
          image1.Canvas.textout(x_21-tw2, y_21-th2,floattostrF(median_21,ffFixed,0,2));
          image1.Canvas.textout(x_31-tw2, y_31-th2,floattostrF(median_31,ffFixed,0,2));
          image1.Canvas.textout(-tw2+smx,-th2+smy,floattostrF(median_22,ffFixed,0,2));
        end;

      end
      else
      if ((triangle=false) and (nhfd_11>0) and (nhfd_21>0) and (nhfd_31>0) and (nhfd_12>0) and (nhfd_32>0) and (nhfd_13>0) and (nhfd_23>0) and (nhfd_33>0)) then  {enough information for tilt calculation}
      begin
        median_11:=SMedian(hfdlist_11,nhfd_11);
        median_21:=SMedian(hfdlist_21,nhfd_21);
        median_31:=SMedian(hfdlist_31,nhfd_31);

        median_12:=SMedian(hfdlist_12,nhfd_12);
        {22 is already done}
        median_32:=SMedian(hfdlist_32,nhfd_32);

        median_13:=SMedian(hfdlist_13,nhfd_13);
        median_23:=SMedian(hfdlist_23,nhfd_23);
        median_33:=SMedian(hfdlist_33,nhfd_33);

        median_best:=min(min(median_13, median_33),min(median_11,median_31));{find best corner}
        median_worst:=max(max(median_13, median_33),max(median_11,median_31));{find worst corner}

        scale_factor:=headx.height*0.4/median_worst;

        x_11:=round(-median_11*scale_factor+sm_x);  y_11:=round(-median_11*scale_factor+sm_y);{calculate coordinates counter clockwise}
        x_21:=round( sm_x);                         y_21:=round(-median_21*scale_factor+sm_y);
        x_31:=round(+median_31*scale_factor+sm_x);  y_31:=round(-median_31*scale_factor+sm_y);

        x_12:=round(-median_12*scale_factor+sm_x);  y_12:=round(+sm_y);
        x_22:=smx;                                  y_22:=smy;
        x_32:=round(+median_32*scale_factor+sm_x);  y_32:=round(+sm_y);

        x_13:=round(-median_13*scale_factor+sm_x);  y_13:=round(+median_13*scale_factor+sm_y);
        x_23:=round(sm_x);                          y_23:=round(+median_23*scale_factor+sm_y);
        x_33:=round(+median_33*scale_factor+sm_x);  y_33:=round(+median_33*scale_factor+sm_y);


        flip_xy(fliph,flipv,x_11,y_11); {from array to image coordinates}
        flip_xy(fliph,flipv,x_21,y_21);
        flip_xy(fliph,flipv,x_31,y_31);

        flip_xy(fliph,flipv,x_12,y_12); {from array to image coordinates}
        // flip x_22, y_22 is not required since they are in the middle.
        flip_xy(fliph,flipv,x_32,y_32); {from array to image coordinates}

        flip_xy(fliph,flipv,x_13,y_13); {from array to image coordinates}
        flip_xy(fliph,flipv,x_23,y_23); {from array to image coordinates}
        flip_xy(fliph,flipv,x_33,y_33); {from array to image coordinates}



        result:=median_worst-median_best; //for export
        update_text(memo,'TILT    = ',floattostr2(result)+'                / Delta HFD between worst and best corner. SNR>'+floattostrF(snr_min,FFgeneral,0,0));;//two decimals only for nice reporting


        tilt_value:=100*(median_worst-median_best)/hfd_median;
        mess2:='  Tilt[HFD]='+floattostrF(median_worst-median_best,ffFixed,0,2)+' ('+floattostrF(tilt_value,ffFixed,0,0)+'%';{estimate tilt value}
        if tilt_value<5 then mess2:=mess2+' none)'
        else
        if tilt_value<10 then mess2:=mess2+' almost none)'
        else
        if tilt_value<15 then mess2:=mess2+' mild)'
        else
        if tilt_value<20 then mess2:=mess2+' moderate)'
        else
        if tilt_value<30 then mess2:=mess2+' severe)'
        else
        mess2:=mess2+' extreme)';


        if screenplot then//plot octagon
        begin
          image1.Canvas.Pen.width :=image1.Canvas.Pen.width*2;{thickness lines}
          image1.Canvas.pen.color:=clyellow;

          image1.Canvas.moveto(x_11,y_11);{draw trapezium}
          image1.Canvas.lineto(x_21,y_21);{draw trapezium}
          image1.Canvas.lineto(x_31,y_31);{draw trapezium}
          image1.Canvas.lineto(x_32,y_32);{draw trapezium}
          image1.Canvas.lineto(x_33,y_33);{draw trapezium}
          image1.Canvas.lineto(x_23,y_23);{draw trapezium}
          image1.Canvas.lineto(x_13,y_13);{draw trapezium}
          image1.Canvas.lineto(x_12,y_12);{draw trapezium}
          image1.Canvas.lineto(x_11,y_11);{draw trapezium}

          image1.Canvas.lineto(smx,smy);{draw diagonal}
          image1.Canvas.lineto(x_31,y_31);{draw diagonal}
          image1.Canvas.lineto(smx,smy);{draw diagonal}
          image1.Canvas.lineto(x_33,y_33);{draw diagonal}
          image1.Canvas.lineto(smx,smy);{draw diagonal}
          image1.Canvas.lineto(x_13,y_13);{draw diagonal}

          fontsize:=fontsize*4;
          image1.Canvas.font.size:=fontsize;

          with image1.Canvas.TextStyle do Alignment := taRightJustify;

          th:= (image1.Canvas.textheight('5'));
          th2:=th div 2;

          tw:= image1.Canvas.textwidth('2.35');

          textout_octogram(x_11,y_11    ,median_11);
          textout_octogram(x_21 ,y_21    ,median_21);
          textout_octogram(x_31 ,y_31    ,median_31);

          textout_octogram(x_12 ,y_12,median_12);
          textout_octogram(x_22 ,y_22,median_22);
          textout_octogram(x_32 ,y_32,median_32);

          textout_octogram(x_13 ,y_13,median_13);
          textout_octogram(x_23 ,y_23,median_23);
          textout_octogram(x_33 ,y_33,median_33);



        end;
      end
      else
      begin
        mess2:='';
      end;

      image1.Canvas.font.style:=[];

      if screenplot then
      begin
        str(hfd_median:0:1,hfd_value);
        str(fwhm_median:0:1,fwhm_value);
        if headx.cdelt2<>0 then
        begin
           str(hfd_median*abs(headx.cdelt2)*3600:0:1,hfd_arcsec);
           hfd_arcsec:=' ('+hfd_arcsec+'")';
           str(fwhm_median*abs(headx.cdelt2)*3600:0:1,fwhm_arcsec);
           fwhm_arcsec:=' ('+fwhm_arcsec+'")';
        end
        else
        begin
          hfd_arcsec:='';
          fwhm_arcsec:='';
        end;
        mess2:='Median HFD='+hfd_value+hfd_arcsec+ mess2+'  Stars='+ inttostr(nhfd)+mess1 ;

        text_width:=mainform1.image1.Canvas.textwidth(mess2);{Calculate textwidth. This also works for 4k with "make everything bigger"}
        fontsize:=min(60,trunc(fontsize*(headx.width*0.9)/text_width));{use 90% of width}
        image1.Canvas.font.size:=fontsize;
        image1.Canvas.font.color:=clwhite;
        text_height:=mainform1.image1.Canvas.textheight('T');{the correct text height, also for 4k with "make everything bigger"}

        left_margin:=min(headx.width div 20,round(fontsize*2));{twice font size but not more then 5% of width. Required for small images}

        image1.Canvas.Brush.Style:=bssolid; //Bsclear;
        image1.Canvas.Brush.Color:=clBlack;
        image1.Canvas.textout(left_margin,headx.height-text_height,mess2);{median HFD and tilt indication}

        mess2:=mess2+'. Median FWHM='+fwhm_value+fwhm_arcsec;
        memo2_message(mess2);{for stacking live}
      end;
    end
    else
      image1.Canvas.textout(round(fontsize*2),headx.height- round(fontsize*4),'No stars detected');
  end;{with mainform1}

  Screen.Cursor:=crDefault;
end;


procedure filter_hfd(var mean,min_value,max_value : single; nr : integer; hfd_values: Tstar_list); {filter array of hfd values}
var
  i,j,nr_closest,nr_second_closest  :  integer;
  closest_distance,second_closest_distance,distance_sqr   : single;
  a,b,c,dummy : double;

begin


  {local filtering. Take median of three closest stars}
  max_value:=0;
  min_value:=65535;
  mean:=0;
  for i:=0 to nr-1 do
  begin
    closest_distance:=999999;
    second_closest_distance:=999999;
    nr_closest:=0;

    for j:=0 to nr-1 do
    begin
      if i<>j then
      begin
         distance_sqr:=(sqr(hfd_values[0,i]-hfd_values[0,j])+sqr(hfd_values[1,i]-hfd_values[1,j]));
         if distance_sqr<closest_distance then
         begin
           second_closest_distance:=closest_distance;
           closest_distance:=distance_sqr;
           nr_second_closest:=nr_closest;
           nr_closest:=j;
         end
         else
         if distance_sqr<second_closest_distance then
         begin
           second_closest_distance:=distance_sqr;
           nr_second_closest:=j;
         end;
      end;
    end;
    {find median of three stars}
    a:=hfd_values[2,i];
    b:=hfd_values[2,nr_closest];
    c:=hfd_values[2,nr_second_closest];

    if a<b then begin dummy:=b; b:=a; a:=dummy; end;{sort in sequence a,b,c wher a is the largest}
    if a<c then begin dummy:=c; c:=a; a:=dummy; end;{sort in sequence a,b,c wher a is the largest}
    if b<c then begin dummy:=c; c:=b; b:=dummy; end;{sort in sequence a,b,c wher a is the largest}

    hfd_values[2,i]:=b;  {hfd*100, place median value in this cell}

    if max_value<b then max_value:=b;
    if min_value>b then min_value:=b;
     mean:=mean+b;
  end;
  mean:=mean/nr;
 {useful length is nr}
end;

procedure voronoi_plot(min_value,max_value : single; nr:integer;hfd_values: Tstar_list);
var
    i,size,fitsx,fitsY,x,y,x2,y2,w,h,scaledown:  integer;
    img_hfd: Timage_array;
    zeros_left : boolean;
    col        : double;

begin
  scaledown:=1+ head.width div 1000;
  w:=(head.width div scaledown)+1;
  h:=(head.height div scaledown)+1;

  setlength(img_hfd,1,h,w);{set length of image array}
  for fitsY:=0 to h-1  do
    for fitsX:=0 to w-1 do
      img_hfd[0,fitsY,fitsX]:=0;{clear array}


  size:=0;
  repeat
    zeros_left:=false;
    for i:=0 to nr-1 do
    begin
      col:=hfd_values[2,i];

      for y:=-size to size do
      for x:=-size to size do
      if round(sqrt(sqr(x)+sqr(y)))=size then
      begin
        x2:=round(hfd_values[0,i]/scaledown) + x;
        y2:=round(hfd_values[1,i]/scaledown) + y;
        if ((x2>=0) and (x2<w) and (y2>=0) and (y2<h)) then
          if  img_hfd[0,y2,x2]=0 then {not used yet}
          begin
            img_hfd[0,y2,x2]:=col;
            zeros_left:=true;
          end;
      end;
    end;
    inc(size);
  until ((zeros_left=false) or (size>h/5));

  if head.naxis>1 then setlength(img_loaded,1,head.height,head.width);
  head.naxis3:=1;
  for fitsY:=0 to head.height-1  do
    for fitsX:=0 to head.width-1 do
      img_loaded[0,fitsY,fitsX]:={img_loaded[0,fitsY,fitsX]}+img_hfd[0,fitsY div scaledown,fitsX div scaledown];

  img_hfd:=nil;{free memory}

  plot_histogram(img_loaded,true {update}); {plot histogram, set sliders}

  head.backgr:=min_value-5;
  cwhite:=max_value+5;
  mainform1.minimum1.position:=round(min_value-5);{+5, -5 for very flat fields}
  mainform1.maximum1.position:=round(max_value+5);
 end;


procedure contour_plot(mean: single; nr:integer;hfd_values: Tstar_list);
var
    i,fitsx,fitsY,x,y,w,h,x2,y2,scaledown : integer;
    img_hfd: Timage_array;
    cols,min_value,max_value,step_adjust  : single;
    distance,factor,influence, sum_influence,pixels_per_star: double;

begin
  scaledown:=1+ head.width div 1000;
  w:=(head.width div scaledown)+1;
  h:=(head.height div scaledown)+1;

  setlength(img_hfd,1,h,w);{set length of image array}
  for fitsY:=0 to h-1  do
    for fitsX:=0 to w-1 do
      img_hfd[0,fitsY,fitsX]:=0;{clear array}

  max_value:=0;
  min_value:=65535;

  pixels_per_star:=w*h/nr;// average surface per star in pixels

  factor:=2*sqrt(pixels_per_star); //take the square root to get calculate the average distance in pixels between the stars/measuring points
  for y:=0 to h-1 do  //move in y
  for x:=0 to w-1 do  //move in x
  begin
    cols:=mean; // background value
    sum_influence:=0;
    for i:=0 to nr-1 do //go through all HFD points and adapt the local luminance depending on all HFD values of the stars. The closer HFD points have more influence
    begin
      x2:=round(hfd_values[0,i]/ scaledown);
      y2:=round(hfd_values[1,i]/ scaledown);
      distance:=sqrt(sqr(x2-x)+sqr(y2-y));
      influence:=factor/(factor+distance);
      sum_influence:=sum_influence+influence;// sum of all influences for position x,y.
      cols:=cols+hfd_values[2,i]*influence; //adapt the luminance at position x, y by the HFD value at position i in list.
    end;
    cols:=cols/sum_influence; //scale the luminance value.
    if max_value<cols then max_value:=cols;
    if min_value>cols then min_value:=cols;
    img_hfd[0,y,x]:=cols; //set the pixel in the contour plot
  end;

  if head.naxis>1 then setlength(img_loaded,1,head.height,head.width);
  head.naxis3:=1;

  {introduce rounding to show layers}
  step_adjust:=((max_value-min_value)/60);

  {convert back}
  for fitsY:=0 to head.height-1  do
    for fitsX:=0 to head.width-1 do
      img_loaded[0,fitsY,fitsX]:=(1/step_adjust)*round(step_adjust*img_hfd[0,fitsY div scaledown,fitsX div scaledown]);


  img_hfd:=nil;{free memory}

  plot_histogram(img_loaded,true {update}); {plot histogram, set sliders}

  head.backgr:=min_value-5;
  cwhite:=max_value+5;
  mainform1.minimum1.position:=round(min_value-5);{+5, -5 for very flat fields}
  mainform1.maximum1.position:=round(max_value+5);
end;


procedure measure_star_aspect(img: Timage_array;x1,y1: double; rs:integer;  out aspect : double; out orientation : integer); {measures the aspect and orientation [0..179] of a single star }
var
  i, j,angle,pixel_counter,orientationMin : integer;
  val,r,themax,themin,g,delta_angle,impact : double;
  data : array[-51..51,-51..51] of double;
  function value_subpixel(x1,y1:double):double; {calculate image pixel value on subpixel level}
  var
    x_trunc,y_trunc: integer;
    x_frac,y_frac  : double;
  begin
    x_trunc:=trunc(x1);
    y_trunc:=trunc(y1);
    if ((x_trunc<=0) or (x_trunc>=(head.width-2)) or (y_trunc<=0) or (y_trunc>=(head.height-2))) then begin result:=0; exit;end;
    x_frac :=frac(x1);
    y_frac :=frac(y1);
    try
      result:=         (img[0,y_trunc  ,x_trunc   ]) * (1-x_frac)*(1-y_frac);{pixel left top, 1}
      result:=result + (img[0,y_trunc  ,x_trunc+1 ]) * (  x_frac)*(1-y_frac);{pixel right top, 2}
      result:=result + (img[0,y_trunc+1,x_trunc   ]) * (1-x_frac)*(  y_frac);{pixel left bottom, 3}
      result:=result + (img[0,y_trunc+1,x_trunc+1 ]) * (  x_frac)*(  y_frac);{pixel right bottom, 4}
    except
    end;
  end;
begin
  aspect:=999;{failure indication}
  rs:=min(rs,51);

  pixel_counter:=0;
  if ((x1-rs>=0) and (x1+rs<=head.width) and (y1-rs>0) and (y1+rs<head.height))  then {measurement within screen}
  begin

    for i:=-rs to rs do
    for j:=-rs to rs do
    begin
      val:=value_subpixel(x1+i,y1+j)- star_bg {from procedure hfd};

      if val>7*sd_bg {from procedure hfd} then //SNR>7
      begin
        val:=sqrt(val);{reduce contrast}
        r:=sqrt(sqr(i)+sqr(j));{distance}
        data[i,j]:=val*(r);
        inc(pixel_counter); {how many pixels are illuminated}
      end
      else
      data[i,j]:=0;

    end;
    if pixel_counter<4 then
    begin
      exit; {not enough pixels}
    end;

    themax:=0;
    themin:=1E99;
    for angle:=0 to 179 do {rotate 180 degr}
    begin
      impact:=0;
      for i:=-rs to rs do
      for j:=-rs to rs do
      begin
        if data[i,j]>0 then
        begin
          g:=arctan2(j,i);
          delta_angle:=((angle*pi/180) - g);
          //split star with a line with "angle" and measure the sum (average) of distances*sqrt(brightness) to line. This impact sum will be smallest if the line splits the star in the length
          //distance to split line is r*sin(delta_angle). r is already included in the data.
          impact:=impact+data[i,j]*abs(sin(delta_angle));{add to get sum of the impact}
        end;
      end;
      if impact>themax then
      begin
        themax:=impact;
      end;
      if impact<themin then
      begin
        themin:=impact;
        orientationMin:=angle;
      end;
    end;

    orientation:=orientationMin;

    aspect:=themax/(themin+0.00001);

    if aspect>5 then aspect:=999; {failure}

  //  memo2_message(#9+floattostr(aspect)+#9+ inttostr(round(orientationMax))+#9+ inttostr(orientationMin));

  end;
end;


procedure plot_vector(x,y,r,orientation : double);
var sinO,cosO,xstep,ystep              : double;
    wd                                 : integer;
begin
  wd:=max(1,head.height div 1000);
  mainform1.image1.Canvas.Pen.mode:=pmXor;
  mainform1.image1.canvas.Pen.Color := clred;
  mainform1.image1.canvas.Pen.width := wd;

  r:=r*wd;
  sincos(orientation,sinO,cosO);
  xstep:=r*cosO;
  ystep:=r*sinO;

  if mainform1.flip_horizontal1.checked then
  begin
    x:=head.width-x;
    xstep:=-xstep;
  end;

  if mainform1.flip_vertical1.checked=false then
  begin
    y:=head.height-y;
    ystep:=-ystep;
  end;

  moveToex(mainform1.image1.Canvas.handle,round(x-xstep),round(y-ystep),nil);
  lineTo(mainform1.image1.Canvas.handle,round(x+xstep),round(y+ystep)); {line}
end;


procedure CCDinspector_analyse(detype: char; aspect,values,vectors: boolean);
var
 fitsX,fitsY,size,radius, i, j,nhfd,retries,max_stars,n,m,xci,yci,sqr_radius,orientation,starX,starY,x2,y2,font_luminance,starpixels : integer;
 hfd1,star_fwhm,snr,flux,xc,yc,detection_level,med, backgr, noise_level         : double;
 mean, min_value,max_value,data_max : single;
 hfd_values  : Tstar_list; {array of aray of doubles}
 hfds        : array of double;
 Fliphorizontal, Flipvertical: boolean;
 mess: string;
 img_sa : Timage_array;

begin
  if head.naxis=0 then exit; {file loaded?}

  max_stars:=1000;

  SetLength(hfd_values,4,4000);{will contain x,y,hfd}
  setlength(img_sa,1,head.height,head.width);{set length of image array}

  get_background(0,img_loaded,head,false{ calculate histogram},true {calculate noise level});{calculate background level from peek histogram}

  data_max:=head.datamax_org-1;
  backgr:=head.backgr;
  noise_level:=head.noise_level;

  retries:=3; {try up to four times to get enough stars from the image}
  repeat
    if retries=3 then
      begin if head.star_level >30*noise_level then detection_level:=head.star_level  else retries:=2;{skip} end;//stars are dominant
    if retries=2 then
      begin if head.star_level2>30*noise_level then detection_level:=head.star_level2 else retries:=1;{skip} end;//stars are dominant
    if retries=1 then
      begin detection_level:=30*noise_level; end;
    if retries=0 then
      begin detection_level:= 7*noise_level; end;

    nhfd:=0;{set counters at zero}
    for fitsY:=0 to head.height-1 do
      for fitsX:=0 to head.width-1  do
        img_sa[0,fitsY,fitsX]:=-1;{mark as star free area}

    for fitsY:=1 to head.height-1-1  do   //Search through the image. Stay one pixel away from the borders.
    begin
      for fitsX:=1 to head.width-1-1 do
      begin
        if (( img_sa[0,fitsY,fitsX]<=0){area not occupied by a star} and (img_loaded[0,fitsY,fitsX]- backgr>detection_level){star}) then {new star}
        begin
          starpixels:=0;
          if img_loaded[0,fitsY,fitsX-1]- backgr>4*noise_level then inc(starpixels);//inspect in a cross around it.
          if img_loaded[0,fitsY,fitsX+1]- backgr>4*noise_level then inc(starpixels);
          if img_loaded[0,fitsY-1,fitsX]- backgr>4*noise_level then inc(starpixels);
          if img_loaded[0,fitsY+1,fitsX]- backgr>4*noise_level then inc(starpixels);
          if starpixels>=2 then //At least 3 illuminated pixels. Not a hot pixel
          begin
            HFD(img_loaded,fitsX,fitsY,14{annulus radius},99 {flux aperture restriction},0 {adu_e}, hfd1,star_fwhm,snr,flux,xc,yc);{star HFD and FWHM}
            if (hfd1>=1.3) {not a hotpixel} and (snr>30) and (hfd1<99) then
            begin


              radius:=round(5.0*hfd1);{for marking area. For inspector use factor 5 instead of 3}
              sqr_radius:=sqr(radius);
              xci:=round(xc);{star center as integer}
              yci:=round(yc);
              for n:=-radius to +radius do {mark the whole circular star area as occupied to prevent double detection's}
                for m:=-radius to +radius do
                begin
                  j:=n+yci;
                  i:=m+xci;
                  if ((j>=0) and (i>=0) and (j<head.height) and (i<head.width) and (sqr(m)+sqr(n)<=sqr_radius)) then
                    img_sa[0,j,i]:=1;
                end;

              if aspect then measure_star_aspect(img_loaded,xc,yc,round(hfd1*1.5),{out} hfd1 {aspect},orientation);{store the star aspect in hfd1}

              {store values}
              if hfd1<>999 then
              if ( ((img_loaded[0,round(yc),round(xc)]<data_max) and
                    (img_loaded[0,round(yc-1),round(xc)]<data_max) and
                    (img_loaded[0,round(yc+1),round(xc)]<data_max) and
                    (img_loaded[0,round(yc),round(xc-1)]<data_max) and
                    (img_loaded[0,round(yc),round(xc+1)]<data_max) and

                    (img_loaded[0,round(yc-1),round(xc-1)]<data_max) and
                    (img_loaded[0,round(yc-1),round(xc+1)]<data_max) and
                    (img_loaded[0,round(yc+1),round(xc-1)]<data_max) and
                    (img_loaded[0,round(yc+1),round(xc+1)]<data_max)){not saturated}
                    or ((aspect))  )
                    then
              begin
                if nhfd>=length(hfd_values)-1 then
                    SetLength(hfd_values,4,nhfd+2000);{adapt length if required}
                hfd_values[0,nhfd]:=xc;
                hfd_values[1,nhfd]:=yc;
                hfd_values[2,nhfd]:=hfd1*1000;{hfd or star aspect * 1000}
                hfd_values[3,nhfd]:=orientation;    {star orientation 0..179}
                inc(nhfd);

              end;
            end;
          end;

        end;
      end;
    end;

    dec(retries);{prepare for trying with lower detection level}
  until ((nhfd>=max_stars) or (retries<0));{reduce dection level till enough stars are found. Note that faint stars have less positional accuracy}

  img_sa:=nil;{free mem}

  if nhfd<10 then
   begin
     memo2_message('Abort, only '+inttostr(nhfd)+' useful stars!');
     exit;
   end;

  if detype<>' ' then {contour or voronoi}
  begin
     filter_hfd(mean, min_value,max_value ,nhfd, hfd_values); {apply the median value for each three grouped stars}
     font_luminance:=100;
  end
  else
  font_luminance:=round((cwhite-head.backgr)/4+head.backgr);


  if detype='V' then voronoi_plot(min_value,max_value,nhfd,hfd_values)
  else
  if detype='2' then contour_plot(mean,nhfd,hfd_values);

  Flipvertical:=mainform1.flip_vertical1.Checked;
  Fliphorizontal:=mainform1.Flip_horizontal1.Checked;
  size:=max(1,head.height div 1000);{font size, 1 is 9x5 pixels}

  setlength(hfds,nhfd);

  for i:=0 to nhfd-1 do {plot rectangles later since the routine can be run three times to find the correct detection_level and overlapping rectangle could occur}
   begin
     if values then
     begin
       if Fliphorizontal     then starX:=head.width-round(hfd_values[0,i])  else starX:=round(hfd_values[0,i]);
       if Flipvertical       then starY:=head.height-round(hfd_values[1,i]) else starY:=round(hfd_values[1,i]);
       annotation_to_array(floattostrf(hfd_values[2,i]/1000 {aspect}, ffgeneral, 3,0){text},true{transparent},round(img_loaded[0,starY,starX]+font_luminance){luminance},size,starX+round(hfd_values[2,i]/300),starY,img_loaded);{string to image array as annotation. Text should be far enough of stars since the text influences the HFD measurement.}
     end;
     hfds[i]:=hfd_values[2,i];
  end;

  quickSort(hfds,0,nhfd-1);

  med:=hfds[round((nhfd-1)*0.9)];

  hfds:=nil;{free memory}

  if aspect then
     mess:='Values indicate aspect ratio of the star shape.'
  else
  begin
     mess:='10% of the HFD measurements is worse or equal then ';
     mess:=mess+floattostrf(med/1000 , ffgeneral, 2,0);
  end;
  memo2_message(mess);
  annotation_to_array(mess,true {transparent},65535,size*2 {size},5,10+size*2*9,img_loaded); {report median value}

  plot_fits(mainform1.image1,false);{plot image included text in pixel data}

  if ((aspect) and (vectors)) then
  for i:=0 to nhfd-1 do {plot rectangles later since the routine can be run three times to find the correct detection_level and overlapping rectangle could occur}
  begin
    plot_vector(hfd_values[0,i],hfd_values[1,i],50*(hfd_values[2,i]/1000-1) {aspect},hfd_values[3,i]*pi/180);
  end;
  hfd_values:=nil;
end;


procedure Tform_inspection1.roundness1Click(Sender: TObject);
var
  j: integer;
  demode : char;
  aspect : boolean;
begin
  form_inspection1.undo_button1Click(nil);{undo if required}

  Screen.Cursor:=crHourglass;{$IfDef Darwin}{$else}application.processmessages;{$endif}// Show hourglass cursor, processmessages is for Linux. Note in MacOS processmessages disturbs events keypress for lv_left, lv_right key

  backup_img;
  executed:=2;{restore required to undo}

  if contour_check then demode:='2'
  else
  if voronoi_check then demode:='V'
  else
  demode:=' ';

  if sender=nil {F3 hidden main menu} then
    aspect:=true
  else
    aspect:=((sender=hfd_button1)=false);

  if head.nrbits=8 then {convert to 16 bit}
  begin
    head.nrbits:=16;
    head.datamax_org:=65535;
  end;

  if head.naxis3>1 then
  begin
    convert_mono(img_loaded,head);
    get_hist(0,img_loaded);{get histogram of img_loaded and his_total. Required after box blur to get correct background value}
  end
  else
  if bayerpat<>'' then {raw Bayer image}
  begin
    check_pattern_filter(img_loaded);
    get_hist(0,img_loaded);{get histogram of img_loaded and his_total. Required after box blur to get correct background value}
  end;

  CCDinspector_analyse(demode,aspect {unroundness or HFD mode}, values_check,vectors_check);

  {$ifdef mswindows}
  filename2:=ExtractFileDir(filename2)+'\hfd_values.fit';
  {$ELSE}{linux}
  filename2:=ExtractFileDir(filename2)+'/hfd_values.fit';
  {$ENDIF}
  mainform1.memo1.lines.clear;
  for j:=0 to 10 do {create an header with fixed sequence}
    if (j<>5)  then {skip head.naxis3 for mono images}
        mainform1.memo1.lines.add(head1[j]); {add lines to empthy memo1}
  mainform1.memo1.lines.add(head1[27]); {add end}

  update_integer(mainform1.memo1.lines,'BITPIX  =',' / Bits per entry                                 ' ,head.nrbits);
  update_integer(mainform1.memo1.lines,'NAXIS1  =',' / length of x axis                               ' ,head.width);
  update_integer(mainform1.memo1.lines,'NAXIS2  =',' / length of y axis                               ' ,head.height);
  if head.naxis3=1 then  remove_key(mainform1.memo1.lines,'NAXIS3  ',false{all});{remove key word in header. Some program don't like naxis3=1}

  update_integer(mainform1.memo1.lines,'DATAMIN =',' / Minimum data value                             ' ,0);
  update_integer(mainform1.memo1.lines,'DATAMAX =',' / Maximum data value                             ' ,round(cwhite));
  update_text(mainform1.memo1.lines,'COMMENT 1','  Written by ASTAP, Astrometric STAcking Program. www.hnsky.org');
  if demode='V'  then update_text(mainform1.memo1.lines,'COMMENT G','  Grey values indicate measured values * 1000');

  Screen.Cursor:=crDefault;  { Always restore to normal }
end;

procedure Tform_inspection1.measuring_angle1Change(Sender: TObject);
begin
  measuring_angle:=measuring_angle1.text;
end;


procedure Tform_inspection1.show_distortion1Click(Sender: TObject);
var
  stars_measured,i :integer;
  report : string;
begin
  toClipboard1:=to_Clipboard1.checked;
    form_inspection1.undo_button1Click(nil);{undo if required}
  executed:=1;{only refresh required to undo}

 // if calculate_undisturbed_image_scale then
    measure_distortion(stars_measured);{measure andor plot distortion}

  if toClipboard1 then
  begin
    report:='x database'+#9+'y database'+#9+'x measured'+#9+'y measured'+#10;
    for i:=0 to length(distortion_data[0])-1 do
    report:=report+floattostr(distortion_data[0,i])+#9+floattostr(distortion_data[1,i])+#9+floattostr(distortion_data[2,i])+#9+floattostr(distortion_data[3,i])+#10;
    Clipboard.AsText:=report;
  end;
  distortion_data:=nil;
end;


procedure Tform_inspection1.tilt1Click(Sender: TObject);
begin
  if  executed<>0 then
    form_inspection1.undo_button1Click(nil);{undo if required}
  executed:=1;{only refresh required to undo}
  if extra_stars=false then
    CCDinspector(img_loaded, head,mainform1.memo1.lines, 30,true {screenplot},three_corners,strtofloat(measuring_angle))
  else
    CCDinspector(img_loaded, head,mainform1.memo1.lines,10,true {screenplot},three_corners,strtofloat(measuring_angle));
end;


procedure Tform_inspection1.triangle1Change(Sender: TObject);
begin
  three_corners:=triangle1.checked;
end;


procedure Tform_inspection1.undo_button1Click(Sender: TObject);
begin
  if executed=1 then plot_fits(mainform1.image1,false) {only refresh required}
  else
  if ((executed=2) and (mainform1.Undo1.enabled)) then
  begin
    restore_img;
  end;
  executed:=0;
end;


procedure Tform_inspection1.values1Change(Sender: TObject);
begin
  values_check:=values1.checked;
end;


procedure Tform_inspection1.vectors1Change(Sender: TObject);
begin
  vectors_check:=vectors1.checked;
end;


procedure Tform_inspection1.voronoi1Change(Sender: TObject);
begin
  voronoi_check:=voronoi1.checked;
  if ((voronoi_check) and (contour_check)) then contour1.checked:=false;
end;


procedure Tform_inspection1.close_button1Click(Sender: TObject);
begin
  form_inspection1.close;   {normal this form is not loaded}
  mainform1.setfocus;
end;

procedure Tform_inspection1.contour1Change(Sender: TObject);
begin
  contour_check:=contour1.checked;
  if ((voronoi_check) and (contour_check)) then voronoi1.checked:=false;
end;

procedure Tform_inspection1.extra_stars1Change(Sender: TObject);
begin
  extra_stars:=extra_stars1.checked;
end;

procedure Tform_inspection1.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  insp_left:=left;
  insp_top:=top;
  mainform1.setfocus;
  toclipboard1:=to_clipboard1.checked;
end;


procedure Tform_inspection1.background_values1Click(Sender: TObject);
var
  tx,ty,fontsize,halfstepX,halfstepY,stepX,stepY,fx,fy: integer;
  X,Y,stepsizeX,stepsizeY,median,median_center,factor : double;
  img_bk                                     : Timage_array;
  Flipvertical, Fliphorizontal, restore_req  : boolean;
  detext  : string;
begin
  if head.naxis=0 then exit; {file loaded?}

  form_inspection1.undo_button1Click(nil);{undo if required}

  executed:=1;{only refresh required to undo}
  Screen.Cursor:=crHourglass;{$IfDef Darwin}{$else}application.processmessages;{$endif}// Show hourglass cursor, processmessages is for Linux. Note in MacOS processmessages disturbs events keypress for lv_left, lv_right key

  restore_req:=false;


  if head.naxis3>1 then {colour image}
  begin
    img_bk:=duplicate(img_loaded);
    convert_mono(img_loaded,head);
    get_hist(0,img_loaded);{get histogram of img_loaded and his_total. Required to get correct background value}
    restore_req:=true;{restore original image later}
  end
  else
  if bayerpat<>'' then {raw Bayer image}
  begin
    img_bk:=duplicate(img_loaded);
    check_pattern_filter(img_loaded);
    get_hist(0,img_loaded);{get histogram of img_loaded and his_total. Required to get correct background value}
    restore_req:=true; {restore original image later}
  end;


  with mainform1 do
  begin
    Flipvertical:=mainform1.flip_vertical1.Checked;
    Fliphorizontal:=mainform1.Flip_horizontal1.Checked;


    image1.Canvas.Pen.Mode := pmMerge;
    image1.Canvas.brush.Style:=bsClear;
    image1.Canvas.font.color:=clyellow;
    fontsize:=round(max(7,head.width/115));{adapt font to image dimensions}
    image1.Canvas.font.size:=fontsize;

    stepX:=trunc(head.width/(fontsize*6));{115/6 => 19  steps maximum, reduce if image is too small for font to fit}
    stepY:=trunc(stepX*head.height/head.width);       {stepY in ratio,typical 13 steps}

    if odd(stepX)=false then stepX:=stepX+1; {make odd}
    if odd(stepY)=false then stepY:=stepY+1; {make odd}

    stepsizeX:=head.width/stepX;{stepsizeX is a double value}
    stepsizeY:=head.height/stepY;{stepsizeY is a double value}

    halfstepX:=round(stepsizeX/2);
    halfstepY:=round(stepsizeY/2);

    median_center:=median_background(img_loaded,0{color},trunc(stepsizeX){size},trunc(stepsizeY),head.width div 2,head.height div 2);{find median value of an area at position x,y with sizeX,sizeY}
    Y:=halfstepY;
    repeat

      X:=halfstepX;
      repeat
        median:=median_background(img_loaded,0{color},trunc(stepsizeX){size},trunc(stepsizeY),round(X),round(Y));{find median value of an area at position x,y with sizeX,sizeY}

        factor:=median/median_center;
        if abs(1-factor)>0.03 then image1.Canvas.font.color:=$00A5FF {dark orange} else image1.Canvas.font.color:=clYellow;
        detext:=floattostrf(factor, ffgeneral, 3,0);

        tx:=round(X);
        ty:=round(Y);

        if Flipvertical=false then  tY:=head.height-tY;
        if Fliphorizontal then tX:=head.width-tX;

        tx:=round(X)-( mainform1.image1.canvas.Textwidth(detext) div 2);{make text centered at x, y}
        ty:=round(Y)-( mainform1.image1.canvas.Textheight(detext) div 2);
        mainform1.image1.Canvas.textout(tX,tY,detext);{add as text}

        X:=X+stepsizeX;
      until X>=head.width-1;

      Y:=Y+stepsizeY;
    until Y>=head.height-1;

    if restore_req then {restore backup image for raw Bayer image or colour image}
    begin
      memo2_message('Restoring image');
      img_loaded:=duplicate(img_bk);//fastest way to duplicate an image
      get_hist(0,img_loaded);{get histogram of img_loaded and his_total}
      img_bk:=nil;//free mem
    end;
  end;
  Screen.Cursor:=crDefault;
end;

procedure Tform_inspection1.background_contour1Click(Sender: TObject);
var
  fx,fy                       : integer;
  high_level,low_level,srange : double;
  img_bk                      : Timage_array;
begin
  if head.naxis=0 then exit; {file loaded?}

  form_inspection1.undo_button1Click(nil);{undo if required}

  Screen.Cursor:=crHourglass;{$IfDef Darwin}{$else}application.processmessages;{$endif}// Show hourglass cursor, processmessages is for Linux. Note in MacOS processmessages disturbs events keypress for lv_left, lv_right key

  backup_img;


  case inspector_binning of
    1: bin_X2X3X4(img_loaded,head,mainform1.memo1.lines,2);
    2: bin_X2X3X4(img_loaded,head,mainform1.memo1.lines,3);
    3: bin_X2X3X4(img_loaded,head,mainform1.memo1.lines,4);
  end;

  img_bk:=duplicate(img_loaded);

  apply_most_common(img_bk, img_loaded,head.datamax_org,inspector_grid_size);  {apply most common filter on first array and place result in second array}

  gaussian_blur2(img_loaded, inspector_grid_size *2 {4 * strtofloat2(most_common_filter_radius1.Text)});


  plot_histogram(img_loaded,true {update}); {plot histogram, set sliders}

  //find low and high level
  high_level:=0;
  low_level:=9999999;
  for fy:=0 to (head.height-1) div 50 do
    for fx:=0 to (head.width-1) div 50 do
   begin
     high_level:=max(high_level,img_loaded[0,fy*50,fx*50]);
     low_level:=min(low_level,img_loaded[0,fy*50,fx*50]);
   end;

  for fy:=0 to head.height-1 do
    for fx:=0 to head.width-1 do
      img_loaded[0,fy,fx]:= low_level +  ((high_level-low_level)/inspector_gradations)*round( (img_loaded[0,fy,fx]-low_level)*inspector_gradations/(high_level-low_level));

  srange:=high_level-low_level;
  mainform1.maximum1.position:=round(high_level+0.1*srange); //set sliders again since  plot_histogram doesn't work that well for blurred image.
  mainform1.minimum1.position:=round(low_level-0.05*srange);

  plot_fits(mainform1.image1, False);{plot real}

  img_bk:=nil;//free mem

  Screen.Cursor:=crDefault;
end;

procedure Tform_inspection1.bin_factor1Change(Sender: TObject);
begin
  inspector_binning:=bin_factor1.itemindex;
end;


procedure Tform_inspection1.aberration_inspector1Click(Sender: TObject);
var
   fitsX,fitsY,col, widthN,heightN                : integer;
   img_temp : Timage_array;

var  {################# initialised variables #########################}
   side :integer=250;
const
   gap=4;
begin
  if head.naxis<>0 then
  begin
   Screen.Cursor:=crHourglass;{$IfDef Darwin}{$else}application.processmessages;{$endif}// Show hourglass cursor, processmessages is for Linux. Note in MacOS processmessages disturbs events keypress for lv_left, lv_right key

   form_inspection1.undo_button1Click(nil);{undo if required}
   backup_img;
   executed:=2;{restore required to undo}

   side:=min(side,head.height div 3);
   side:=min(side,head.width div 3);

   widthN:=3*side+2*gap;
   heightN:=widthN;
   setlength(img_temp,head.naxis3,heightN,widthN);{set length of image array}

   for col:=0 to head.naxis3-1 do
    for fitsY:=0 to heightN-1 do
      for fitsX:=0 to widthN-1 do {clear img_temp for the gaps}
         img_temp[col,fitsY,fitsX]:=0;


   for col:=0 to head.naxis3-1 do
   for fitsY:=0 to side-1 do
     for fitsX:=0 to side-1 do {copy corner}
        img_temp[col,fitsY,fitsX]:=img_loaded[col,fitsY,fitsX];

   for col:=0 to head.naxis3-1 do
   for fitsY:=0 to side-1 do
     for fitsX:=0 to side-1 do {copy corner}
        img_temp[col,fitsY,fitsX+side+gap]:=img_loaded[col,fitsY,fitsX+(head.width div 2)-(side div 2)];


   for col:=0 to head.naxis3-1 do
   for fitsY:=0 to side-1 do
     for fitsX:=0 to side-1 do {copy corner}
        img_temp[col,fitsY,fitsX+2*(side+gap)]:=img_loaded[col,fitsY,fitsX+head.width-side];




   for col:=0 to head.naxis3-1 do
   for fitsY:=0 to side-1 do
     for fitsX:=0 to side-1 do {copy corner}
        img_temp[col,fitsY+(side+gap),fitsX]:=img_loaded[col,fitsY +(head.height div 2) - (side div 2) ,fitsX];

   for col:=0 to head.naxis3-1 do
   for fitsY:=0 to side-1 do
     for fitsX:=0 to side-1 do {copy corner}
        img_temp[col,fitsY+(side+gap),fitsX+side+gap]:=img_loaded[col,fitsY +(head.height div 2) - (side div 2) ,fitsX+(head.width div 2)-(side div 2)];


   for col:=0 to head.naxis3-1 do
   for fitsY:=0 to side-1 do
     for fitsX:=0 to side-1 do {copy corner}
        img_temp[col,fitsY+(side+gap),fitsX+2*(side+gap)]:=img_loaded[col,fitsY +(head.height div 2) - (side div 2) ,fitsX+head.width-side];




   for col:=0 to head.naxis3-1 do
   for fitsY:=0 to side-1 do
     for fitsX:=0 to side-1 do {copy corner}
        img_temp[col,fitsY+2*(side+gap),fitsX]:=img_loaded[col,fitsY + head.height - side,fitsX];

   for col:=0 to head.naxis3-1 do
   for fitsY:=0 to side-1 do
     for fitsX:=0 to side-1 do {copy corner}
        img_temp[col,fitsY+2*(side+gap),fitsX+side+gap]:=img_loaded[col,fitsY + head.height - side,fitsX+(head.width div 2)-(side div 2)];


   for col:=0 to head.naxis3-1 do
   for fitsY:=0 to side-1 do
     for fitsX:=0 to side-1 do {copy corner}
        img_temp[col,fitsY+2*(side+gap),fitsX+2*(side+gap)]:=img_loaded[col,fitsY + head.height - side,fitsX+head.width-side];


//   setlength(img_loaded,head.naxis3,head.height,head.width);{set length of image array}
//   img_loaded[0]:=img_temp[0];
//   if head.naxis3>1 then img_loaded[1]:=img_temp[1];
//   if head.naxis3>2 then img_loaded[2]:=img_temp[2];

//   img_temp:=nil; {free memory}

   img_loaded:=nil;{release memory}
   img_loaded:=img_temp;

   head.width:=widthN;
   head.height:=heightN;

   update_integer(mainform1.memo1.lines,'NAXIS1  =',' / length of x axis                               ' ,head.width);
   update_integer(mainform1.memo1.lines,'NAXIS2  =',' / length of y axis                               ' ,head.height);

   if head.cd1_1<>0 then {remove solution}
     remove_solution(false {keep wcs});

   update_text(mainform1.memo1.lines,'COMMENT A','  Aberration view '+filename2);

   filename2:=ChangeFileExt(filename2,'_aberration_view.fits');
   plot_fits(mainform1.image1,true);
   image_move_to_center:=true;

   Screen.Cursor:=crDefault;
  end;
end;


procedure Tform_inspection1.FormKeyPress(Sender: TObject; var Key: char);
begin
  if key=#27 then close_button1Click(sender)
  else
  if key=#26 then {ctrl+Z}
       form_inspection1.undo_button1Click(nil);
end;


procedure Tform_inspection1.FormShow(Sender: TObject);
begin
  form_inspection1.left:=insp_left;
  form_inspection1.top:=insp_top;

  executed:=0;
  contour1.checked:=contour_check;
  voronoi1.checked:=voronoi_check;
  values1.checked:=values_check;
  vectors1.checked:=vectors_check;
  show_distortion1.enabled:=head.cd1_1<>0;

  if head.naxis3>1 then
    bayer_label1.caption:='Colour image'
  else
  begin
    if bayerpat<>'' then
      bayer_label1.caption:='Bayer matrix image'
    else
      bayer_label1.caption:='Mono sensor image';
  end;

  triangle1.checked:=three_corners;
  extra_stars1.checked:=extra_stars;
  measuring_angle1.text:=measuring_angle;

  to_clipboard1.checked:=toclipboard1;

  bin_factor1.itemindex:=inspector_binning;
  grid_size1.text:=inttostr(inspector_grid_size);
  gradations1.text:=inttostr(inspector_gradations);
end;

procedure Tform_inspection1.gradations1Change(Sender: TObject);
begin
  inspector_gradations:=strtoint(gradations1.Text);
end;

procedure Tform_inspection1.grid_size1Change(Sender: TObject);
begin
  inspector_grid_size:=strtoint(grid_size1.Text);
end;


procedure Tform_inspection1.help_uncheck_outliers1Click(Sender: TObject);
begin
  openurl('http://www.hnsky.org/astap.htm#inspector');
end;


end.

