#!/bin/bash

for NHI in $(seq 11. 0.5 23.) ;
 do
  ./abs_line_profile HI_Lya ${NHI} 25. inf 1 0. F
done

