unit unit_live_stacking;
{Copyright (C) 2017, 2024 by Han Kleijn, www.hnsky.org
 email: han.k.. at...hnsky.org

 This Source Code Form is subject to the terms of the Mozilla Public
 License, v. 2.0. If a copy of the MPL was not distributed with this
 file, You can obtain one at https://mozilla.org/MPL/2.0/.   }

{$mode delphi}

interface

uses
  Classes, SysUtils,forms,fileutil,
  clipbrd, {for copy to clipboard}
  graphics,
  math;

procedure stack_live(path :string);{stack live average}

const
  pause_pressed: boolean=false;
  live_stacking: boolean=false; {used to inhibit solving while live_stacking}

implementation

uses unit_stack, astap_main,unit_stack_routines,unit_astrometric_solving,unit_star_align,unit_inspector_plot;

const
  oldra0  :double=0;
  olddec0 :double=-pi/2;
  oldexposure:double=0;
var
  memo1_text : string;{for backup header}


function file_available(stack_directory:string; out filen: string ) : boolean; {check if fits file is available and report the filename}
var
   thefiles : Tstringlist;
   f : file;
begin
  try
  //No need to create the stringlist; the function does that for you
  theFiles := FindAllFiles(stack_directory, '*.fit;*.fits;*.FIT;*.FITS;'+
                                            '*.png;*.PNG;*.jpg;*.JPG;*.bmp;*.BMP;*.tif;*.tiff;*.TIF;'+
                                            '*.RAW;*.raw;*.CRW;*.crw;*.CR2;*.cr2;*.CR3;*.cr3;*.KDC;*.kdc;*.DCR;*.dcr;*.MRW;*.mrw;*.ARW;*.arw;*.NEF;*.nef;*.NRW;.nrw;*.DNG;*.dng;*.ORF;*.orf;*.PTX;*.ptx;*.PEF;*.pef;*.RW2;*.rw2;*.SRW;*.srw;*.RAF;*.raf;', false {search subdirectories}); //find images
  if TheFiles.count>0 then
  begin
    filen:=TheFiles[0];

    {check if free for reading}
    assign(f,filen);
    {$I-}
    reset(f); {prepare for reading}
    {$I+}
    result:=(IOresult=0); {report if file is accessible}
    if result then close(f);
  end
  else
    result:=false;
  finally
    thefiles.Free;
  end;
end;



procedure update_header;

begin
  mainwindow.Memo1.Text:=memo1_text;{use saved fits header first FITS file}

  update_text('COMMENT 1','  Written by Astrometric Stacking Program. www.hnsky.org');
  update_text   ('HISTORY 1','  Stacking method LIVE STACKING');
  update_integer('EXPTIME =',' / Total luminance exposure time in seconds.      ' ,round(sum_exp));
  update_text ('CALSTAT =',#39+head.calstat+#39); {calibration status}
  update_text ('DATE-OBS=',#39+JdToDate(jd_start_first)+#39);{give begin date exposures}
  update_float('JD-AVG  =',' / Julian Day of the observation mid-point.       ',false, jd_sum/counterL);{give midpoint of exposures}
  head.date_avg:=JdToDate(jd_sum/counterL); {update date_avg for asteroid annotation}
  update_text ('DATE-AVG=',#39+head.date_avg+#39);{give midpoint of exposures}
  update_integer('LIGH_CNT=',' / Light frames combined.                  ' ,counterL); {for interim lum,red,blue...files.}
  update_integer('DARK_CNT=',' / Darks used for luminance.               ' ,head.dark_count);{for interim lum,red,blue...files. Compatible with master darks}
  update_integer('FLAT_CNT=',' / Flats used for luminance.               ' ,head.flat_count);{for interim lum,red,blue...files. Compatible with master flats}
  update_integer('BIAS_CNT=',' / Flat-darks used for luminance.          ' ,head.flatdark_count);{for interim lum,red,blue...files. Compatible with master flats}

  mainwindow.memo1.visible:=true;{Show new header again}

end;


function date_string: string;
Var YY,MO,DD : Word;
    HH,MM,SS,MS: Word;
begin
  DecodeDate(date,YY,MO,DD);
  DecodeTime(Time,HH,MM,SS,MS);
  result:=inttostr(YY)+
          inttostr(MO)+
          inttostr(DD)+'_'+
          inttostr(HH)+
          inttostr(MM)+
          inttostr(SS);
end;


procedure save_as_jpg(filename: string);
var
  JPG: TJPEGImage;
begin
   JPG := TJPEGImage.Create;
  try
    JPG.Assign(mainwindow.image1.Picture.Graphic);    //Convert data into JPG
    JPG.CompressionQuality :=90;
    JPG.SaveToFile(filename);
  finally
   JPG.Free;
  end;
end;


procedure stack_live(path :string);{stack live average}
var
    fitsX,fitsY,width_max, height_max, old_width, old_height,x_new,y_new,col,binning, counter,total_counter,bad_counter,max_stars,process_as_asc :  integer;
    distance,hfd_min,aa,bb,cc,dd,ee,ff                                                                                                           : double;
    init, solution, waiting,transition_image,colour_correction :boolean;
    file_ext,filen,filename_org                 :  string;
    multiply_red,multiply_green,multiply_blue,add_valueR,add_valueG,add_valueB,largest,scaleR,scaleG,scaleB,dum :single; {for colour correction}
    warning  : string;
    starlist1,starlist2 : star_list;
    rename_counter: integer=0;
    count         : integer=0;

    procedure reset_var;{reset variables  including init:=false}
    begin
      init:=false;
      counter:=0;
      bad_counter:=0;
      sum_exp:=0;
      sum_temp:=0;
      jd_sum:=0;{sum of Julian midpoints}
      jd_start_first:=1E99; {begin observations in Julian day}
      jd_end_last:=0;{end observations in Julian day}

      light_exposure:=987654321;{not done indication}
      light_temperature:=987654321;
      flat_filter:='987654321';{not done indication}
    end;

begin

  with stackmenu1 do
  begin
    live_stacking:=true;{to block other instruction like solve button}
    reset_var; {reset variables  including init:=false}

    pause_pressed:=false;
    esc_pressed:=false;
    total_counter:=0;

    mainwindow.memo1.visible:=false;{Hide header}

    colour_correction:=((process_as_osc>0) and (stackmenu1.osc_auto_level1.checked));
    hfd_min:=max(0.8 {two pixels},strtofloat2(stackmenu1.min_star_size_stacking1.caption){hfd});{to ignore hot pixels which are too small}
    max_stars:=strtoint2(stackmenu1.max_stars1.text,500);{maximum star to process, if so filter out brightest stars later}

    {Prepare for dark and flats}
    analyse_listview(stackmenu1.listview2,false {light},false {full fits},false{refresh});{analyse dark tab, by loading=false the loaded img will not be effected. Calstat will not be effected}
    analyse_listview(stackmenu1.listview3,false {light},false {full fits},false{refresh});{analyse flat tab, by loading=false the loaded img will not be effected}

    {live stacking}
    repeat
    begin
      if ((pause_pressed=false) and (file_available(path,filename2 {file found}))) then
      begin
        try { Do some lengthy operation }
          waiting:=false;
          transition_image:=false;

          Application.ProcessMessages;
          {load image}
          filename_org:=filename2;//remember .cr2 file name
          if ((esc_pressed) or (load_image(false,false {plot})=false)) then //For .cr2 files filename2 will have extension .fits
          begin
            if esc_pressed=false then memo2_message('Error loading file'); {can't load}
            live_stacking_pause1.font.style:=[];
            live_stacking1.font.style:=[];
            live_stacking:=false;
            exit;
          end;

          ang_sep(head.ra0,head.dec0,oldra0,olddec0 ,distance); {calculate distance in radians.   {test of mount has moved}
          oldra0:=head.ra0;olddec0:=head.dec0;
          if distance>(0.2*pi/180) then
          begin
            reset_var; {reset variables including init:=false}
            if total_counter<>0 then {new position not caused by start}
            begin
              transition_image:=true; {image with possible slewing involved}
              stackmenu1.memo2.clear;{clear memo2}
              memo2_message('New telescope position at distance '+floattostrF(distance*180/pi,ffFixed,0,2)+'°. New stack started. First transition image will be skipped');
            end;
          end
          else
          {test if head.exposure has changed}
          if head.exposure<>oldexposure then
          begin
            reset_var; {reset variables including init:=false}
            stackmenu1.memo2.clear;{clear memo2}
            memo2_message('Exposure time changed from '+floattostrF(oldexposure,ffGeneral,5,5)+' to '+floattostrF(head.exposure,ffGeneral,5,5)+' sec. New stack started.');
          end;
          oldexposure:=head.exposure;

          if transition_image=false then {else skip this image, could slewed during this image}
          begin
            if init=false then
            begin
              if stackmenu1.make_osc_color1.checked then
                process_as_osc:=2 //forced process as OSC images
              else
              if ((head_2.naxis3=1) and (head_2.Xbinning=1) and (bayerpat<>'')) then //auto process as OSC images
                process_as_osc:=1
              else
                process_as_osc:=0;//disable demosaicing

              if process_as_asc<>0 then memo2_message('Will demosaic OSC images to colour');

              memo1_text:=mainwindow.Memo1.Text;{save fits header first FITS file}
              if ((bayerpat='') and (process_as_osc=2 {forced})) then
                if stackmenu1.bayer_pattern1.Text='auto' then memo2_message('█ █ █ █ █ █ Warning, Bayer colour pattern not in the header! Check colours and if wrong set Bayer pattern manually in tab "stack alignment". █ █ █ █ █ █')
                else
                if test_bayer_matrix(img_loaded)=false then  memo2_message('█ █ █ █ █ █ Warning, grayscale image converted to colour! Un-check option "convert OSC to colour". █ █ █ █ █ █');
            end;

            apply_dark_and_flat(img_loaded);{apply dark, flat if required, renew if different head.exposure or ccd temp}

            memo2_message('Adding file: '+inttostr(counter+1)+' "'+filename_org+'"  to average. Using '+inttostr(head.dark_count)+' darks, '+inttostr(head.flat_count)+' flats, '+inttostr(head.flatdark_count)+' flat-darks') ;

            Application.ProcessMessages;
            if esc_pressed then exit;

            if init=false then {first image}
            begin
              old_width:=head.width;
              old_height:=head.height;
            end
            else {init is true, second or third image ....}
            if ((old_width<>head.width) or (old_height<>head.height)) then memo2_message('█ █ █ █ █ █  Warning different size image!');

            if process_as_osc>0 then  demosaic_bayer(img_loaded); {convert OSC image to colour}

            if init=false then {first image}
            begin
              binning:=report_binning(head.height);{select binning based on the height of the first light. Do this after demosaic since SuperPixel also bins}
              bin_and_find_stars(img_loaded, binning,1  {cropping},hfd_min,max_stars,true{update hist},starlist1,warning);{bin, measure background, find stars}
              find_quads(starlist1,0,quad_smallest,quad_star_distances1);{find quads for reference image}
            end;


            if init=false then {init}
            begin
              memo2_message('Reference image is: '+filename2);
              width_max:=head.width;
              height_max:=head.height;

              setlength(img_average,head.naxis3,height_max,width_max);
              for fitsY:=0 to height_max-1 do
                for fitsX:=0 to width_max-1 do
                  for col:=0 to head.naxis3-1 do
                  begin
                    img_average[col,fitsY,fitsX]:=0; {clear img_average}
                  end;

              if colour_correction then
              begin
                memo2_message('Using first reference image to determine colour adjustment factors.');
                stackmenu1.auto_background_level1Click(nil);

                {do factor math behind so "subtract view from file" works in correct direction}
                add_valueR:=strtofloat2(stackmenu1.add_valueR1.Text);
                add_valueG:=strtofloat2(stackmenu1.add_valueG1.Text);
                add_valueB:=strtofloat2(stackmenu1.add_valueB1.Text);

                multiply_red:=strtofloat2(stackmenu1.multiply_red1.Text);
                multiply_green:=strtofloat2(stackmenu1.multiply_green1.Text);
                multiply_blue:=strtofloat2(stackmenu1.multiply_blue1.Text);

                {prevent clamping to 65535}
                scaleR:=(65535+add_valueR)*multiply_red/65535;{range 0..1, if above 1 then final value could be above 65535}
                scaleG:=(65535+add_valueG)*multiply_green/65535;
                scaleB:=(65535+add_valueB)*multiply_blue/65535;
                largest:=scaleR;
                if scaleG>largest then largest:=scaleG;
                if scaleB>largest then largest:=scaleB;
                {use largest to scale to maximum 65535}
              end;
            end;{init, c=0}

            solution:=true;

            {align using star match}
            if init=true then {second image}
            begin{internal alignment only}
              bin_and_find_stars(img_loaded, binning,1  {cropping},hfd_min,max_stars,true{update hist},starlist2,warning);{bin, measure background, find stars}

              find_quads(starlist2,0,quad_smallest,quad_star_distances2);{find star quads for new image}
              if find_offset_and_rotation(3,strtofloat2(stackmenu1.quad_tolerance1.text)) then {find difference between ref image and new image}
              memo2_message(inttostr(nr_references)+' of '+ inttostr(nr_references2)+' quads selected matching within '+stackmenu1.quad_tolerance1.text+' tolerance.'
                     +'  Solution x:='+floattostr6(solution_vectorX[0])+'*x+ '+floattostr6(solution_vectorX[1])+'*y+ '+floattostr6(solution_vectorX[2])
                     +',  y:='+floattostr6(solution_vectorY[0])+'*x+ '+floattostr6(solution_vectorY[1])+'*y+ '+floattostr6(solution_vectorY[2]) )

               else
               begin
                 memo2_message('Not enough quad matches <3 or inconsistent solution, skipping this image.');
                 solution:=false;
               end;
             end{internal alignment}
             else
               reset_solution_vectors(1);{no influence on the first image}

            init:=true;{initialize for first image done}

            if solution then
            begin
              inc(counter);
              inc(total_counter);
              sum_exp:=sum_exp+head.exposure;
              sum_temp:=sum_temp+head.set_temperature;

              date_to_jd(head.date_obs,head.date_avg,head.exposure);{convert date-obs to jd}
              jd_start_first:=min(jd_start,jd_start_first);{find the begin date}
              jd_sum:=jd_sum+jd_mid;{sum julian days of images at midpoint head.exposure.}

              aa:=solution_vectorX[0];
              bb:=solution_vectorX[1];
              cc:=solution_vectorX[2];
              dd:=solution_vectorY[0];
              ee:=solution_vectorY[1];
              ff:=solution_vectorY[2];

              if colour_correction=false then {no colour correction}
              begin
                for fitsY:=0 to head.height-1 do {skip outside "bad" pixels if mosaic mode}
                for fitsX:=0 to head.width-1  do
                begin
                  x_new:=round(aa*(fitsx)+bb*(fitsY)+cc); {correction x:=aX+bY+c  x_new_float in image array range 0..head.width-1}
                  y_new:=round(dd*(fitsx)+ee*(fitsY)+ff); {correction y:=aX+bY+c}
                  if ((x_new>=0) and (x_new<=width_max-1) and (y_new>=0) and (y_new<=height_max-1)) then
                  begin
                    for col:=0 to head.naxis3-1 do {all colors}
                    begin
                      {serial stacking}
                      img_average[col,y_new,x_new]:=(img_average[col,y_new,x_new]*(counter-1)+ img_loaded[col,fitsY,fitsX])/counter;{image loaded is already corrected with dark and flat}{NOTE: fits count from 1, image from zero}
                    end;
                  end;
                end;
              end

              else {colour correction}
              begin
                for fitsY:=0 to head.height-1 do {skip outside "bad" pixels if mosaic mode}
                for fitsX:=0 to head.width-1  do
                begin
                  x_new:=round(aa*(fitsx)+bb*(fitsY)+cc); {correction x:=aX+bY+c  x_new_float in image array range 0..head.width-1}
                  y_new:=round(dd*(fitsx)+ee*(fitsY)+ff); {correction y:=aX+bY+c}
                  if ((x_new>=0) and (x_new<=width_max-1) and (y_new>=0) and (y_new<=height_max-1)) then
                  begin
                    dum:=img_loaded[0,fitsY,fitsX];
                    if dum<>0 then {signal}
                    begin
                      dum:=(dum+add_valueR)*multiply_red/largest;
                      if dum<0 then dum:=0;
                      img_average[0,y_new,x_new]:=(img_average[0,y_new,x_new]*(counter-1)+ dum)/counter;
                    end;
                    if head.naxis3>1 then {colour}
                    begin
                      dum:=img_loaded[1,fitsY,fitsX];   if dum<>0 then {signal} begin dum:=(dum+add_valueG)*multiply_green/largest; if dum<0 then dum:=0; img_average[1,y_new,x_new]:=(img_average[1,y_new,x_new]*(counter-1)+ dum)/counter;end;
                    end;
                    if head.naxis3>2 then {colour}
                    begin
                      dum:=img_loaded[2,fitsY,fitsX]; if dum<>0 then {signal} begin dum:=(dum+add_valueB)*multiply_blue/largest; if dum<0 then dum:=0; img_average[2,y_new,x_new]:=(img_average[2,y_new,x_new]*(counter-1)+ dum)/counter;end;
                    end;
                  end;
                end;
              end;


              head.cd1_1:=0;{kill any existing north arrow during plotting. Most likely wrong after stacking}
              head.height:=height_max;
              head.width:=width_max;
              img_loaded:=img_average;{copy the pointer. Both have now access to the data!!}

              if counter=1 then {set range correct}
                   use_histogram(img_loaded,true);{get histogram R,G,B YES, plot histogram YES, set min & max YES}

              plot_fits(mainwindow.image1,false,false{do not show header in memo1});{plot real}

              if stackmenu1.write_jpeg1.checked then save_as_jpg(ExtractFileDir(filename2)+ {$ifdef mswindows}'\'{$else}{unix} '/' {$endif}+'stack.jpeg');
              if stackmenu1.interim_to_clipboard1.checked then Clipboard.Assign(mainwindow.Image1.Picture.Bitmap);
              if stackmenu1.write_log1.checked then Memo2.Lines.SaveToFile(ExtractFileDir(filename2)+ {$ifdef mswindows}'\'{$else}{unix} '/' {$endif}+'log.txt');
            end
            else
            inc(bad_counter);

            stackmenu1.files_live_stacked1.caption:=inttostr(counter)+' stacked, '+inttostr(bad_counter)+ ' failures ' ;{Show progress}
            application.hint:=inttostr(counter)+' stacked, '+inttostr(bad_counter)+ ' failures ' ;{Show progress}
          end; {no transition image}

          filename2:=filename_org;// use orginal filename with orgina extension, e.g. with extension .CR2
          file_ext:=ExtractFileExt(filename2);
          if pos('_@',filename2)=0 then filen:=copy(filename2,1,length(filename2)-length(file_ext))+'_@'+ date_string {function} +file_ext+'_' {mark file with date for SGP since the file name will not change if first file is renamed}
                                   else filen:=copy(filename2,1,length(filename2)-length(file_ext))+file_ext+'_'; {already marked with date}
          if RenameFile(filename2,filen)=false then {mark files as done with file extension+'_', beep if failure}
          begin
            beep;
          end;
        finally
        end;
      end
      else
      begin  {pause or no files}
        if waiting=false then {do this only once}
        begin
          if ((pause_pressed) and (counter>0)) then
          begin
            counterL:=counter;
            update_header;
            memo2_message('Live stack is suspended.')
          end
          else
          memo2_message('Live stack is waiting for files.');
        end;
        waiting:=true;
        //Application.ProcessMessages;
        wait(1000);  {smart sleep}{no new files, wait some time}
        if count=0 then live_stacking1.caption:='  ▶'
        else
        if count=1 then live_stacking1.caption:='▶  '
        else
        if count=2 then live_stacking1.caption:=' ▶ ';
        inc(count);
        if count>2 then count:=0;

      end;

    end;{live average}

    until esc_pressed;

    live_stacking:=false;
    live_stacking_pause1.font.style:=[];
    live_stacking1.font.style:=[];
    memo2_message('Live stack stopped. Save result if required');

    counterL:=counter;
    if counter>0 then update_header;

    memo1_text:='';{release memory}
  end;{with stackmenu1}
end;


end.

