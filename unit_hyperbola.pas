unit unit_hyperbola; {Hyperbola modeling of the star disk size in HFD as function of the telescope focuser position. Purpose is finding the best focuser position at the hyperbola minimum.}

{Copyright (C) 2017, 2021 by Han Kleijn, www.hnsky.org
 email: han.k.. at...hnsky.org

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/.   }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  math;

type
  TDouble2 = array[1..2] of double;


procedure find_best_hyperbola_fit(data:array of TDouble2 ;data_length:integer;out p,a,b : double); {input data[n,1]=position,data[n,2]=hfd, output: bestfocusposition=p, a, b of hyperbola}
function hfd_calc(position,perfectfocusposition,a,b:double) :double; {calculate HFD from position and perfectfocusposition using hyperbola parameters}
function steps_to_focus(hfd,a,b:double) :double; {calculates focuser steps to perfect focus from HFD and hyperbola parameters}

var
  iteration_cycles :integer; {how many cycle where used for curve fitting}
  lowest_error : double; {mean HFD error after curve fitting}
  focus_best   : double; {best focus}

implementation

uses astap_main;

function hfd_calc(position,perfectfocusposition,a,b:double) :double; {calculate HFD from position and perfectfocusposition using hyperbola parameters}
{The HFD (half flux diameter) of the imaged star disk as function of the focuser position can be described as hyperbola}
{a,b are the hyperbola parameters, a is the lowest HFD value at focus position, the asymptote y:= +-x*a/b} {rev1}
{A hyperbola is defined as: }
{x=b*sinh(t)                }
{y=a*cosh(t)                }
{Using the arccosh and arsinh functions it is possible to inverse}
{above calculations and convert x=>t and t->y or y->t and t->x}
var
  x,t : double;
begin
  x:=perfectfocusposition - position;
  t:=arsinh(x/b);{calculate t-position in hyperbola}
  result:=a*cosh(t);{convert t-position to y/hfd value}
end;

function steps_to_focus(hfd,a,b:double) :double; {calculates focuser steps to perfect focus from HFD and hyperbola parameters}
{The HFD (half flux diameter) of the imaged star disk as function of the focuser position can be described as hyperbola}
{a,b are the hyperbola parameters, a is the lowest HFD value at focus position, the asymptote y:= +-x*a/b} {rev1}
{A hyperbola is defined as: }
{x=b*sinh(t)                }
{y=a*cosh(t)                }
{Using the arccosh and arsinh functions it is possible to inverse}
{above calculations and convert x=>t and t->y or y->t and t->x}

{Note using the HFD there are two solutions, either left or right side of the hyperbola}
var
  x,t : double;
begin
  x:=hfd/a;
  if x<1 then x:=1;{prevent run time errors}

  t:=arcosh(x);{calculate t-position in hyperbola}
  result:=b*sinh(t);{convert t-position to steps to focus}
end;


function mean_error_hyperbola(data: array of TDouble2 {pos, hfd};data_length:integer; perfectfocusposition,a,b : double): double;{calculates total averaged error between measured V-curve and hyperbola}
var
  i : integer;
  hfd_simulation, total_error,error : double;
begin
  total_error:=0;
  for i:=0 to data_length-1 do
  begin
     hfd_simulation:=hfd_calc(data[i,1],perfectfocusposition,a,b);{y or HFD error}

     {smart error calculation which limits error for outliers}
     error:=hfd_simulation - data[i,2] ;{hfd error in simulation}
     if error < 0 then
       total_error:=total_error - error/data[i,2] {if data[i,2] is large positive outlier then limit error to data[i,2]/data[i,2]=1 maximum}
       else
       total_error:=total_error + error/hfd_simulation; {if data[i,2] is large negative outlier then limit error to hfd_simulation/hfd_simulation=1 maximum}
   end;
  result:=(total_error)/data_length;{scale to average error per point}
end;


procedure find_best_hyperbola_fit(data: array of TDouble2 {pos, hfd};data_length:integer;out p,a,b : double); {input data[n,1]=position,data[n,2]=hfd, output: bestfocusposition=p, a, b of hyperbola}
{The input data array should contain several focuser positions with there corresponding HFD (star disk size).}
{The routine will try to find the best hyperbola curve fit. The focuser position p at the hyperbola minimum is the expected best focuser position}
var
   i,n  :integer;
   error1, old_error, p_range,a_range, b_range, highest_hfd, lowest_hfd,
   highest_position, lowest_position,a1,b1,p1,a0,b0,p0  :double;
begin
  lowest_error:=1E99;
  n:=data_length;// or n:=Length(data);

  highest_hfd:=0;
  lowest_hfd:=1E99;
  for i:=0 to n-1 do {find start values for hyperbola loop}
  begin
    if data[i,2]>highest_hfd then
    begin
      highest_hfd:=data[i,2];
      highest_position:=data[i,1];
    end;
    if ((data[i,2]<lowest_hfd) and (data[i,2]>0.1){avoid zero's}) then
    begin
     lowest_hfd:=data[i,2];
     lowest_position:=data[i,1];
    end;
  end;
  if  highest_position<lowest_position then  highest_position:=(lowest_position- highest_position)+lowest_position;{go up always}

  {get good starting values for a, b and p}
  a:=lowest_hfd;{a is near the HFD value}
  {Alternative hyperbola formula: sqr(y)/sqr(a)-sqr(x)/sqr(b)=1 ==>  sqr(b)=sqr(x)*sqr(a)/(sqr(y)-sqr(a)} {rev1}
  b:=sqrt(sqr(highest_position- lowest_position)*sqr(a)/(sqr(highest_hfd)-sqr(a)) );{rev1}
  p:=lowest_position;

  iteration_cycles:=0;

  {set starting test range}
  a_range:=a;
  b_range:=b;
  p_range:=(highest_position-lowest_position);{large steps since slope could contain some error}

  repeat
    p0:=p;
    b0:=b;
    a0:=a;

    a_range:=a_range*0.5;{reduce scan range by 50%}
    b_range:=b_range*0.5;
    p_range:=p_range*0.5;

    p1:=p0 - p_range;{start value}
    while p1<=p0 + p_range do {position loop}
    begin
      a1:=a0 - a_range;{start value}
      while a1<=a0 + a_range do {a loop}
      begin
        b1:=b0 - b_range;{start value}
        while b1 <= b0 + b_range do {b loop}
        begin
          error1:=mean_error_hyperbola(data, data_length,p1,a1,b1); {calculate the curve fit error with these values.}
          if error1<lowest_error  then
          begin{better position found}
            old_error:=lowest_error;
            lowest_error:=error1;
            a:=a1;{best value up to now}
            b:=b1;
            p:=p1;
          end;
          b1:=b1 + b_range*0.1;{do 20 steps within range, many steps guarantees convergence}
         end;{b loop}
        a1:=a1 + a_range*0.1;{do 20 steps within range}
      end;{a loop}
      p1:=p1 + p_range*0.1;{do 20 steps within range}
    end;{position loop}
    inc(iteration_cycles);
  until  ( (old_error-lowest_error<1E-5)   {lowest error almost reached. Error is expressed in relative error per point}
        or (lowest_error<=1E-5 {0.00001})  {perfect result}
        or (iteration_cycles>=30) );       {most likely convergence problem}

  focus_best:=p;{use for command line}
end;


end.

