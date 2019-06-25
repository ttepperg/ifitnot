 # Set font path
 set fontpath '/usr/share/texmf/fonts/type1/public/amsfonts/cm/'
 # Set term x11 and font cmsy10.pfb for special symbols (CMSY10 font) in OMS encoding (see table ~/ps_fontfile_doc.ps)
 set term post enhanced color landscape fontfile "cmsy10.pfb" font 20 dashlength 4
 set output "output_data/plotspec.ps"
 set key bottom right samplen 2

 # For fancy symbol fonts
 set encoding iso_8859_1

 # Fixed position using screen coordinates where:
 # (0,0)=bottom-left corner of PAGE
 # (1,1)=top-right corner of PAGE
 # The following setting are such that the printing
 # area is maximed, reducing the margin around the plot
 # to a bearable minimum (for this type of plot)
 set lmargin screen 0.12
 set rmargin screen 0.99
 set bmargin screen 0.08
 set tmargin screen 0.90

 # Set alternative x-axis
 set xtics nomirror
 set mxtics
 set x2tics
 set mx2tics

 # Set y-axis format
 set ytics format "%5.2f"
 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 regions: 77  lines: 136, SHIFT =            0 }"
 set xrange[*:*]
 set ytics format "%5.2f"
 set yrange[*:*]
 set ylabel "PDF"
 set xlabel "Residuals"


 # Box width
 bw =    1.9310745425670624E-002
 set boxwidth bw
 # Maximum value (to normalise histogram)
 maxval =        30309
 # Binning function
 binc(x,s) = s*(ceil(x/s))
 binr(x,s) = s*(ceil(x/s)+0.5)
 # Plot histogram 
 unset style
 plot 'output_data/combined_0153_h1_noconv.psh' index 3 using (binc(($1),bw)):(1./(maxval)) smooth frequency w boxes lt 1 lc rgb "black" title "residuals" \
 , "" index 3 using (binr(($2),bw)):(1./(maxval)) smooth frequency w boxes lt 1 lw 1.5 lc rgb "red" title "noise" \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 regions: 77  lines: 136, SHIFT =            0 }"
 set xrange[   0.0000000000000000      :   126475.61954731772      ]
 set x2range[   1155.0100000000000      :   1761.1900000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%8.2e"
 set yrange[*:*]
 set ylabel "Residuals"


 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):(($2)-($3)) w histeps lt 1 lw 1.5 lc rgb "black" title "residuals" \
 , 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($6) w histeps lt 1 lw 1.5 lc rgb "red" title "noise" \
 , 0 w l lt 3 lw 1.5 lc rgb "royalblue" notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 regions: 77  lines: 136, SHIFT =            0 }"
 set xrange[   0.0000000000000000      :   126475.61954731772      ]
 set x2range[   1155.0100000000000      :   1761.1900000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 1 / 77  lines: 2 / 136}"

 set xrange[   0.0000000000000000      :   2290.8924354941914      ]
 set x2range[   1155.0100000000000      :   1163.8699999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           95 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.78}{/CMSY10 \006}10^{12.74}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=52.38{/CMSY10 \006}3.83}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=248.86}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0448}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           96 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.77}{/CMSY10 \006}10^{12.72}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=81.65{/CMSY10 \006}8.12}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=266.31}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0452}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9           97 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.69}{/CMSY10 \006}10^{12.53}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}8.11}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=229.29}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=-0.0494}" at graph 0.37,   0.180000007     tc rgb "royalblue"
 set label front "{/=9           98 }" at graph 0.02,   0.219999999     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.63}{/CMSY10 \006}10^{12.52}" at graph 0.05,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}9.05}" at graph 0.20,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 EW=206.82}" at graph 0.29,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 z=-0.0480}" at graph 0.37,   0.219999999     tc rgb "royalblue"
 set label front "{/=9           99 }" at graph 0.02,   0.259999990     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.58}{/CMSY10 \006}10^{12.63}" at graph 0.05,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}14.45}" at graph 0.20,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 EW=187.73}" at graph 0.29,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 z=-0.0442}" at graph 0.37,   0.259999990     tc rgb "royalblue"
 set label front "{/=9          100 }" at graph 0.02,   0.299999982     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.62}{/CMSY10 \006}10^{12.52}" at graph 0.05,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}9.18}" at graph 0.20,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 EW=202.30}" at graph 0.29,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 z=-0.0468}" at graph 0.37,   0.299999982     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 2 / 77  lines: 1 / 136}"

 set xrange[   1940.3783780776446      :   2939.2931242607715      ]
 set x2range[   1162.5100000000000      :   1166.3900000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           16 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.16}{/CMSY10 \006}10^{13.26}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=38.43{/CMSY10 \006}2.61}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=364.49}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0422}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           17 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.56}{/CMSY10 \006}10^{13.25}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=28.38{/CMSY10 \006}4.01}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=145.69}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0421}" at graph 0.37,   0.140000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 3 / 77  lines: 1 / 136}"

 set xrange[   7074.4283750524537      :   8273.6304534393039      ]
 set x2range[   1182.5899999999999      :   1187.3299999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          117 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.80}{/CMSY10 \006}10^{12.61}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}8.04}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=289.74}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0249}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 4 / 77  lines: 1 / 136}"

 set xrange[   8122.0976330138019      :   9130.8726546659655      ]
 set x2range[   1186.7300000000000      :   1190.7300000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           76 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.84}{/CMSY10 \006}10^{12.43}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=47.17{/CMSY10 \006}2.15}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=263.36}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0223}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 5 / 77  lines: 2 / 136}"

 set xrange[   8581.5111815450782      :   10076.036064071395      ]
 set x2range[   1188.5500000000000      :   1194.4900000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           54 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.96}{/CMSY10 \006}10^{12.77}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=57.84{/CMSY10 \006}4.02}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=338.14}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0207}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           55 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.41}{/CMSY10 \006}10^{12.76}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=17.67{/CMSY10 \006}2.38}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=97.87}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0207}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9           56 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.38}{/CMSY10 \006}10^{12.49}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=54.84{/CMSY10 \006}0.82}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=549.28}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=-0.0192}" at graph 0.37,   0.180000007     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 6 / 77  lines: 1 / 136}"

 set xrange[   9342.2848281421066      :   10442.239319057953      ]
 set x2range[   1191.5699999999999      :   1195.9500000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           41 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.12}{/CMSY10 \006}10^{12.62}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=41.85{/CMSY10 \006}1.28}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=365.59}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0183}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           42 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.59}{/CMSY10 \006}10^{12.71}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=75.90{/CMSY10 \006}12.30}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=184.83}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0179}" at graph 0.37,   0.140000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 7 / 77  lines: 1 / 136}"

 set xrange[   10457.279202671243      :   11368.290314223357      ]
 set x2range[   1196.0100000000000      :   1199.6500000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9            5 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.16}{/CMSY10 \006}10^{12.42}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=38.06{/CMSY10 \006}0.80}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=360.75}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0147}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 8 / 77  lines: 1 / 136}"

 set xrange[   10928.147088436270      :   11782.834062803773      ]
 set x2range[   1197.8900000000001      :   1201.3099999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           40 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.71}{/CMSY10 \006}10^{12.33}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=24.98{/CMSY10 \006}1.24}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=173.04}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0132}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 9 / 77  lines: 1 / 136}"

 set xrange[   11113.286045799976      :   12136.989442504886      ]
 set x2range[   1198.6300000000001      :   1202.7300000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           38 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.73}{/CMSY10 \006}10^{13.20}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=23.94{/CMSY10 \006}2.56}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=176.12}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0122}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           39 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.51}{/CMSY10 \006}10^{13.22}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=41.81{/CMSY10 \006}15.96}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=144.30}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0121}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9           50 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.64}{/CMSY10 \006}10^{12.29}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=21.71{/CMSY10 \006}1.14}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=149.72}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=-0.0126}" at graph 0.37,   0.180000007     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 10 / 77  lines: 1 / 136}"

 set xrange[   12650.021567442960      :   13723.052084565386      ]
 set x2range[   1204.7900000000000      :   1209.1099999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9            1 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.40}{/CMSY10 \006}10^{12.54}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=55.76{/CMSY10 \006}0.89}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=567.21}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0074}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9            2 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.40}{/CMSY10 \006}10^{12.22}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=20.34{/CMSY10 \006}1.54}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=101.86}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0068}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9            3 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.53}{/CMSY10 \006}10^{12.45}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=30.03{/CMSY10 \006}2.51}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=140.38}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=-0.0071}" at graph 0.37,   0.180000007     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 11 / 77  lines: 1 / 136}"

 set xrange[   13112.491718540565      :   13946.117507073017      ]
 set x2range[   1206.6500000000001      :   1210.0100000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          116 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.09}{/CMSY10 \006}10^{12.26}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=24.84{/CMSY10 \006}4.47}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=58.81}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0061}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 12 / 77  lines: 1 / 136}"

 set xrange[   13673.459442538731      :   15458.576550267111      ]
 set x2range[   1208.9100000000001      :   1216.1300000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           43 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.28}{/CMSY10 \006}10^{13.44}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=92.27{/CMSY10 \006}8.99}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=645.32}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0012}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           44 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.40}{/CMSY10 \006}10^{12.39}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=7.82{/CMSY10 \006}0.60}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=68.89}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=-0.0009}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9           45 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.15}{/CMSY10 \006}10^{13.34}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}15.83}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=547.56}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=-0.0018}" at graph 0.37,   0.180000007     tc rgb "royalblue"
 set label front "{/=9           46 }" at graph 0.02,   0.219999999     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.81}{/CMSY10 \006}10^{12.82}" at graph 0.05,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}10.40}" at graph 0.20,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 EW=297.96}" at graph 0.29,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 z=-0.0025}" at graph 0.37,   0.219999999     tc rgb "royalblue"
 set label front "{/=9           47 }" at graph 0.02,   0.259999990     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.55}{/CMSY10 \006}10^{13.19}" at graph 0.05,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 b=30.02{/CMSY10 \006}7.21}" at graph 0.20,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 EW=143.81}" at graph 0.29,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 z=-0.0010}" at graph 0.37,   0.259999990     tc rgb "royalblue"
 set label front "{/=9           48 }" at graph 0.02,   0.299999982     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.25}{/CMSY10 \006}10^{12.64}" at graph 0.05,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 b=22.81{/CMSY10 \006}3.59}" at graph 0.20,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 EW=80.20}" at graph 0.29,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 z=-0.0018}" at graph 0.37,   0.299999982     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 13 / 77  lines: 1 / 136}"

 set xrange[   15221.832080266182      :   17130.185515829558      ]
 set x2range[   1215.1700000000001      :   1222.9300000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           32 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.46}{/CMSY10 \006}10^{13.19}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=97.26{/CMSY10 \006}3.52}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=825.04}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0012}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           33 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.51}{/CMSY10 \006}10^{12.41}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=6.40{/CMSY10 \006}0.36}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=67.53}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0009}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9           34 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.16}{/CMSY10 \006}10^{13.32}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}11.67}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=555.20}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=0.0018}" at graph 0.37,   0.180000007     tc rgb "royalblue"
 set label front "{/=9           35 }" at graph 0.02,   0.219999999     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.45}{/CMSY10 \006}10^{12.60}" at graph 0.05,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 b=20.01{/CMSY10 \006}2.40}" at graph 0.20,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 EW=108.50}" at graph 0.29,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 z=0.0010}" at graph 0.37,   0.219999999     tc rgb "royalblue"
 set label front "{/=9           36 }" at graph 0.02,   0.259999990     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.90}{/CMSY10 \006}10^{13.00}" at graph 0.05,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}12.13}" at graph 0.20,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 EW=355.50}" at graph 0.29,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 z=0.0025}" at graph 0.37,   0.259999990     tc rgb "royalblue"
 set label front "{/=9           37 }" at graph 0.02,   0.299999982     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.62}{/CMSY10 \006}10^{12.67}" at graph 0.05,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}11.89}" at graph 0.20,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 EW=202.41}" at graph 0.29,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 z=0.0032}" at graph 0.37,   0.299999982     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 14 / 77  lines: 1 / 136}"

 set xrange[   18338.741583792515      :   19221.280605123888      ]
 set x2range[   1227.8699999999999      :   1231.4900000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          119 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.35}{/CMSY10 \006}10^{12.09}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=27.88{/CMSY10 \006}1.77}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=98.76}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0115}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 15 / 77  lines: 1 / 136}"

 set xrange[   23446.638764364492      :   24266.418755157014      ]
 set x2range[   1248.9700000000000      :   1252.3900000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          111 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.20}{/CMSY10 \006}10^{12.09}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=18.68{/CMSY10 \006}1.71}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=69.35}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0288}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 16 / 77  lines: 1 / 136}"

 set xrange[   24113.180035135327      :   25088.737568185414      ]
 set x2range[   1251.7500000000000      :   1255.8299999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           14 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.15}{/CMSY10 \006}10^{12.57}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=40.46{/CMSY10 \006}1.11}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=371.76}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0312}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           15 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.67}{/CMSY10 \006}10^{12.54}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=30.59{/CMSY10 \006}1.98}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=177.29}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0315}" at graph 0.37,   0.140000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 17 / 77  lines: 1 / 136}"

 set xrange[   24673.079153921262      :   25494.284786029359      ]
 set x2range[   1254.0899999999999      :   1257.5300000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          126 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.10}{/CMSY10 \006}10^{12.30}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=31.77{/CMSY10 \006}6.04}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=61.65}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0330}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 18 / 77  lines: 1 / 136}"

 set xrange[   25017.113427936503      :   26146.778966876889      ]
 set x2range[   1255.5300000000000      :   1260.2700000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           51 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.42}{/CMSY10 \006}10^{12.38}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=54.48{/CMSY10 \006}0.58}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=566.68}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0345}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 19 / 77  lines: 1 / 136}"

 set xrange[   25594.394348554219      :   26413.083561531945      ]
 set x2range[   1257.9500000000000      :   1261.3900000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           81 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.42}{/CMSY10 \006}10^{12.21}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=22.34{/CMSY10 \006}1.56}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=108.02}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0362}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 20 / 77  lines: 2 / 136}"

 set xrange[   25746.878000025274      :   26850.071066716191      ]
 set x2range[   1258.5899999999999      :   1263.2300000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9            6 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.15}{/CMSY10 \006}10^{13.08}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=42.18{/CMSY10 \006}1.65}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=379.55}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0369}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9            7 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.05}{/CMSY10 \006}10^{13.16}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}9.44}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=466.77}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0372}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9            8 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.32}{/CMSY10 \006}10^{12.44}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=27.24{/CMSY10 \006}3.19}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=94.27}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=0.0377}" at graph 0.37,   0.180000007     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 21 / 77  lines: 1 / 136}"

 set xrange[   26869.056081802424      :   27755.266278361105      ]
 set x2range[   1263.3099999999999      :   1267.0500000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           93 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.56}{/CMSY10 \006}10^{12.30}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=37.43{/CMSY10 \006}2.35}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=155.76}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0407}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 22 / 77  lines: 1 / 136}"

 set xrange[   28190.303145794180      :   28978.380195767644      ]
 set x2range[   1268.8900000000001      :   1272.2300000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           85 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.30}{/CMSY10 \006}10^{12.15}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=22.54{/CMSY10 \006}1.83}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=87.52}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0452}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 23 / 77  lines: 1 / 136}"

 set xrange[   29110.310320253437      :   30581.162070013877      ]
 set x2range[   1272.7900000000000      :   1279.0500000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          132 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.61}{/CMSY10 \006}10^{12.47}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}8.60}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=199.72}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0487}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 24 / 77  lines: 2 / 136}"

 set xrange[   31381.688639571865      :   33450.322625523084      ]
 set x2range[   1282.4700000000000      :   1291.3499999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           61 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.86}{/CMSY10 \006}10^{12.87}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}10.44}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=325.23}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0599}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           62 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.88}{/CMSY10 \006}10^{12.75}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}8.34}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=341.18}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0590}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9           63 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.86}{/CMSY10 \006}10^{13.00}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}13.51}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=330.43}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=0.0574}" at graph 0.37,   0.180000007     tc rgb "royalblue"
 set label front "{/=9           64 }" at graph 0.02,   0.219999999     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.82}{/CMSY10 \006}10^{12.88}" at graph 0.05,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}12.01}" at graph 0.20,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 EW=304.24}" at graph 0.29,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 z=0.0607}" at graph 0.37,   0.219999999     tc rgb "royalblue"
 set label front "{/=9           65 }" at graph 0.02,   0.259999990     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.85}{/CMSY10 \006}10^{12.88}" at graph 0.05,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}11.67}" at graph 0.20,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 EW=321.74}" at graph 0.29,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 z=0.0582}" at graph 0.37,   0.259999990     tc rgb "royalblue"
 set label front "{/=9           66 }" at graph 0.02,   0.299999982     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.77}{/CMSY10 \006}10^{12.86}" at graph 0.05,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}11.55}" at graph 0.20,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 EW=274.70}" at graph 0.29,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 z=0.0567}" at graph 0.37,   0.299999982     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 25 / 77  lines: 1 / 136}"

 set xrange[   33598.863354908557      :   35670.742450827842      ]
 set x2range[   1291.9900000000000      :   1300.9500000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           67 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.83}{/CMSY10 \006}10^{12.90}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}12.59}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=309.15}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0678}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           68 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.82}{/CMSY10 \006}10^{12.77}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}10.07}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=305.63}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0654}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9           69 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.77}{/CMSY10 \006}10^{12.87}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}13.50}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=277.32}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=0.0663}" at graph 0.37,   0.180000007     tc rgb "royalblue"
 set label front "{/=9           70 }" at graph 0.02,   0.219999999     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.77}{/CMSY10 \006}10^{12.61}" at graph 0.05,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}8.00}" at graph 0.20,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 EW=274.61}" at graph 0.29,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 z=0.0646}" at graph 0.37,   0.219999999     tc rgb "royalblue"
 set label front "{/=9           71 }" at graph 0.02,   0.259999990     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.73}{/CMSY10 \006}10^{12.92}" at graph 0.05,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}16.45}" at graph 0.20,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 EW=252.59}" at graph 0.29,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 z=0.0670}" at graph 0.37,   0.259999990     tc rgb "royalblue"
 set label front "{/=9           72 }" at graph 0.02,   0.299999982     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.63}{/CMSY10 \006}10^{12.84}" at graph 0.05,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}18.99}" at graph 0.20,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 EW=206.05}" at graph 0.29,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 z=0.0687}" at graph 0.37,   0.299999982     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 26 / 77  lines: 1 / 136}"

 set xrange[   35813.580779460273      :   36839.094434794679      ]
 set x2range[   1301.5699999999999      :   1306.0300000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          105 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.74}{/CMSY10 \006}10^{12.46}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}6.19}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=260.77}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0726}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 27 / 77  lines: 1 / 136}"

 set xrange[   36591.085473817991      :   37394.074398069475      ]
 set x2range[   1304.9500000000000      :   1308.4500000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          107 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.22}{/CMSY10 \006}10^{12.33}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=48.85{/CMSY10 \006}7.90}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=83.56}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0749}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 28 / 77  lines: 1 / 136}"

 set xrange[   36719.708323871448      :   37865.687196555882      ]
 set x2range[   1305.5100000000000      :   1310.5100000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           79 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.79}{/CMSY10 \006}10^{12.77}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}9.52}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=284.63}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0762}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           80 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.68}{/CMSY10 \006}10^{12.81}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}14.83}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=228.75}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0755}" at graph 0.37,   0.140000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 29 / 77  lines: 1 / 136}"

 set xrange[   37416.985364836422      :   38683.525240912357      ]
 set x2range[   1308.5500000000000      :   1314.0899999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           57 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.53}{/CMSY10 \006}10^{12.75}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=57.16{/CMSY10 \006}0.79}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=633.92}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0785}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           58 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.08}{/CMSY10 \006}10^{12.84}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=31.40{/CMSY10 \006}9.49}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=58.73}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0789}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9           59 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{12.85}{/CMSY10 \006}10^{12.32}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=53.33{/CMSY10 \006}19.22}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=37.12}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=0.0779}" at graph 0.37,   0.180000007     tc rgb "royalblue"
 set label front "{/=9           60 }" at graph 0.02,   0.219999999     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.34}{/CMSY10 \006}10^{13.04}" at graph 0.05,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}43.40}" at graph 0.20,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 EW=113.24}" at graph 0.29,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 z=0.0791}" at graph 0.37,   0.219999999     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 30 / 77  lines: 0 / 136}"

 set xrange[   38564.871532262827      :   40049.220152204747      ]
 set x2range[   1313.5699999999999      :   1320.0899999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"


 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 31 / 77  lines: 1 / 136}"

 set xrange[   40593.760864555225      :   41413.243214290989      ]
 set x2range[   1322.4900000000000      :   1326.1099999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           90 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.40}{/CMSY10 \006}10^{12.17}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=27.86{/CMSY10 \006}1.86}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=109.78}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0894}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 32 / 77  lines: 1 / 136}"

 set xrange[   42374.746026089444      :   43216.347641679255      ]
 set x2range[   1330.3699999999999      :   1334.1099999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          103 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.49}{/CMSY10 \006}10^{12.32}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=50.71{/CMSY10 \006}4.05}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=144.99}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0959}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 33 / 77  lines: 1 / 136}"

 set xrange[   42572.982596007656      :   43485.880367314741      ]
 set x2range[   1331.2500000000000      :   1335.3099999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          112 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.47}{/CMSY10 \006}10^{12.23}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=34.21{/CMSY10 \006}2.26}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=129.53}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0968}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 34 / 77  lines: 1 / 136}"

 set xrange[   42865.592424778697      :   43961.463426862436      ]
 set x2range[   1332.5500000000000      :   1337.4300000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9            9 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.39}{/CMSY10 \006}10^{12.41}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=49.17{/CMSY10 \006}0.61}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=518.01}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0978}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           10 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.72}{/CMSY10 \006}10^{12.39}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=38.76{/CMSY10 \006}2.05}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=205.26}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.0982}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9           11 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.28}{/CMSY10 \006}10^{12.11}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=20.15{/CMSY10 \006}1.56}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=81.66}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=0.0988}" at graph 0.37,   0.180000007     tc rgb "royalblue"
 set label front "{/=9           12 }" at graph 0.02,   0.219999999     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.24}{/CMSY10 \006}10^{12.10}" at graph 0.05,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 b=18.22{/CMSY10 \006}1.55}" at graph 0.20,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 EW=73.99}" at graph 0.29,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 z=0.0985}" at graph 0.37,   0.219999999     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 35 / 77  lines: 1 / 136}"

 set xrange[   44006.290832686434      :   44852.270700095876      ]
 set x2range[   1337.6300000000001      :   1341.4100000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          131 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.08}{/CMSY10 \006}10^{12.19}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=34.00{/CMSY10 \006}5.20}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=59.52}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1020}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 36 / 77  lines: 1 / 136}"

 set xrange[   47043.372979418404      :   47942.779098576109      ]
 set x2range[   1351.2500000000000      :   1355.3099999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          106 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.61}{/CMSY10 \006}10^{12.27}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=71.58{/CMSY10 \006}3.81}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=192.07}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1132}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 37 / 77  lines: 1 / 136}"

 set xrange[   48857.137319657893      :   49812.682700545221      ]
 set x2range[   1359.4500000000000      :   1363.7900000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          101 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.55}{/CMSY10 \006}10^{12.13}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=40.50{/CMSY10 \006}1.81}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=154.48}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1198}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9          102 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.25}{/CMSY10 \006}10^{12.13}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=37.51{/CMSY10 \006}3.32}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=86.42}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1204}" at graph 0.37,   0.140000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 38 / 77  lines: 1 / 136}"

 set xrange[   50467.035327613637      :   51159.355875003101      ]
 set x2range[   1366.7700000000000      :   1369.9300000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          127 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{12.69}{/CMSY10 \006}10^{11.90}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=12.04{/CMSY10 \006}2.35}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=24.03}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1256}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 39 / 77  lines: 1 / 136}"

 set xrange[   52081.424325874294      :   52970.214460885654      ]
 set x2range[   1374.1500000000001      :   1378.2300000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9            4 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.33}{/CMSY10 \006}10^{12.19}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=50.07{/CMSY10 \006}0.42}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=497.83}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1321}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 40 / 77  lines: 1 / 136}"

 set xrange[   55892.420590008856      :   56928.894143127873      ]
 set x2range[   1391.7300000000000      :   1396.5500000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          125 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.57}{/CMSY10 \006}10^{12.20}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=41.70{/CMSY10 \006}2.05}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=161.34}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1466}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 41 / 77  lines: 1 / 136}"

 set xrange[   56439.058645268677      :   57400.784914192205      ]
 set x2range[   1394.2700000000000      :   1398.7500000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          118 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.66}{/CMSY10 \006}10^{12.43}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}7.35}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=220.71}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1486}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 42 / 77  lines: 1 / 136}"

 set xrange[   56782.886591234106      :   57525.068853529410      ]
 set x2range[   1395.8699999999999      :   1399.3299999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          135 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{12.91}{/CMSY10 \006}10^{12.10}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=25.43{/CMSY10 \006}4.53}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=41.07}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1497}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 43 / 77  lines: 1 / 136}"

 set xrange[   57820.571941697439      :   58649.849339271954      ]
 set x2range[   1400.7100000000000      :   1404.5899999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          124 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.28}{/CMSY10 \006}10^{12.17}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=34.46{/CMSY10 \006}3.11}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=90.28}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1540}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 44 / 77  lines: 1 / 136}"

 set xrange[   58239.771975757401      :   59123.303090661226      ]
 set x2range[   1402.6700000000001      :   1406.8099999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           86 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.62}{/CMSY10 \006}10^{13.09}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}24.57}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=202.66}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1555}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           87 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.30}{/CMSY10 \006}10^{12.97}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=36.33{/CMSY10 \006}8.07}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=94.22}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1558}" at graph 0.37,   0.140000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 45 / 77  lines: 1 / 136}"

 set xrange[   61872.424379727723      :   63094.622365424511      ]
 set x2range[   1419.7700000000000      :   1425.5699999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           83 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.98}{/CMSY10 \006}10^{12.84}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}6.16}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=406.13}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1708}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           84 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.06}{/CMSY10 \006}10^{12.85}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=37.29{/CMSY10 \006}1.14}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=322.04}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1708}" at graph 0.37,   0.140000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 46 / 77  lines: 1 / 136}"

 set xrange[   62909.505773687255      :   63968.174904502790      ]
 set x2range[   1424.6900000000001      :   1429.7300000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          121 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.66}{/CMSY10 \006}10^{12.18}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=34.02{/CMSY10 \006}1.29}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=179.41}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1739}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 47 / 77  lines: 1 / 136}"

 set xrange[   64960.428167022023      :   65749.371644987928      ]
 set x2range[   1434.4700000000000      :   1438.2500000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           92 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.58}{/CMSY10 \006}10^{12.43}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=70.70{/CMSY10 \006}6.29}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=180.52}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1816}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 48 / 77  lines: 1 / 136}"

 set xrange[   65486.621109950742      :   66199.266055253887      ]
 set x2range[   1436.9900000000000      :   1440.4100000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          129 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{12.93}{/CMSY10 \006}10^{12.14}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=30.77{/CMSY10 \006}5.96}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=43.38}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1835}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 49 / 77  lines: 1 / 136}"

 set xrange[   66636.017065583394      :   67590.492162585637      ]
 set x2range[   1442.5100000000000      :   1447.1099999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          128 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.47}{/CMSY10 \006}10^{12.14}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=36.63{/CMSY10 \006}1.94}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=132.36}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1886}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 50 / 77  lines: 1 / 136}"

 set xrange[   68393.215253554183      :   69251.485847408068      ]
 set x2range[   1450.9900000000000      :   1455.1500000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          134 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.15}{/CMSY10 \006}10^{12.09}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=29.36{/CMSY10 \006}2.99}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=67.48}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1952}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 51 / 77  lines: 1 / 136}"

 set xrange[   69202.036969953231      :   70238.754414873387      ]
 set x2range[   1454.9100000000001      :   1459.9500000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          115 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.75}{/CMSY10 \006}10^{12.16}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=38.29{/CMSY10 \006}1.14}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=214.49}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.1988}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 52 / 77  lines: 1 / 136}"

 set xrange[   69938.804142458364      :   70743.472228006824      ]
 set x2range[   1458.4900000000000      :   1462.4100000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          133 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.17}{/CMSY10 \006}10^{12.19}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=43.22{/CMSY10 \006}5.15}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=74.29}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2013}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 53 / 77  lines: 1 / 136}"

 set xrange[   70497.374568117928      :   71235.062310897760      ]
 set x2range[   1461.2100000000000      :   1464.8099999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          130 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.09}{/CMSY10 \006}10^{12.09}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=27.82{/CMSY10 \006}3.23}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=60.13}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2034}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 54 / 77  lines: 1 / 136}"

 set xrange[   71255.527747875822      :   72085.246875409430      ]
 set x2range[   1464.9100000000001      :   1468.9700000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          114 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.52}{/CMSY10 \006}10^{12.42}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=92.74{/CMSY10 \006}9.24}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=165.86}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2067}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 55 / 77  lines: 1 / 136}"

 set xrange[   72191.350617932578      :   73034.762655918399      ]
 set x2range[   1469.4900000000000      :   1473.6300000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          123 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.47}{/CMSY10 \006}10^{12.40}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=92.84{/CMSY10 \006}9.86}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=148.25}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2105}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 56 / 77  lines: 1 / 136}"

 set xrange[   73794.650965173525      :   74528.327963004442      ]
 set x2range[   1477.3699999999999      :   1480.9900000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           49 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.75}{/CMSY10 \006}10^{12.19}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=37.40{/CMSY10 \006}1.21}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=213.47}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2167}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 57 / 77  lines: 1 / 136}"

 set xrange[   75054.172568205395      :   75905.700232318544      ]
 set x2range[   1483.5899999999999      :   1487.8099999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           13 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.19}{/CMSY10 \006}10^{12.36}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=76.12{/CMSY10 \006}1.32}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=525.90}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2221}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 58 / 77  lines: 1 / 136}"

 set xrange[   75913.760024096307      :   76895.432388023793      ]
 set x2range[   1487.8499999999999      :   1492.7300000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           52 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.16}{/CMSY10 \006}10^{12.59}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=70.48{/CMSY10 \006}1.67}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=487.47}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2260}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           53 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.18}{/CMSY10 \006}10^{12.44}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=19.74{/CMSY10 \006}2.45}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=67.59}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2258}" at graph 0.37,   0.140000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 59 / 77  lines: 1 / 136}"

 set xrange[   81839.129597898733      :   82573.111444192284      ]
 set x2range[   1517.5500000000000      :   1521.2700000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          136 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{12.93}{/CMSY10 \006}10^{12.14}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=26.30{/CMSY10 \006}5.00}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=42.98}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2500}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 60 / 77  lines: 1 / 136}"

 set xrange[   83254.184225632474      :   84118.059395259901      ]
 set x2range[   1524.7300000000000      :   1529.1300000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           89 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.72}{/CMSY10 \006}10^{12.24}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=40.75{/CMSY10 \006}1.55}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=210.07}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2559}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 61 / 77  lines: 1 / 136}"

 set xrange[   85565.349902806978      :   86255.239435630821      ]
 set x2range[   1536.5300000000000      :   1540.0699999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           77 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.43}{/CMSY10 \006}10^{12.21}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=33.49{/CMSY10 \006}2.39}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=120.75}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2654}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 62 / 77  lines: 1 / 136}"

 set xrange[   87502.358931492272      :   88245.827515399709      ]
 set x2range[   1546.4900000000000      :   1550.3299999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           94 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.45}{/CMSY10 \006}10^{12.24}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=35.94{/CMSY10 \006}2.62}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=124.79}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2736}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 63 / 77  lines: 1 / 136}"

 set xrange[   87897.556176462866      :   88736.589805970987      ]
 set x2range[   1548.5300000000000      :   1552.8699999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          113 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.55}{/CMSY10 \006}10^{12.25}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=38.89{/CMSY10 \006}2.27}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=153.05}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2757}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 64 / 77  lines: 1 / 136}"

 set xrange[   88443.001828045162      :   89149.444035473585      ]
 set x2range[   1551.3499999999999      :   1555.0100000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          108 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.37}{/CMSY10 \006}10^{12.36}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=49.42{/CMSY10 \006}5.93}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=113.66}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2777}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 65 / 77  lines: 1 / 136}"

 set xrange[   88763.616349191230      :   89461.600868448833      ]
 set x2range[   1553.0100000000000      :   1556.6300000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          110 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.19}{/CMSY10 \006}10^{12.23}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=34.42{/CMSY10 \006}4.42}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=74.62}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2790}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 66 / 77  lines: 1 / 136}"

 set xrange[   91549.681675554035      :   92298.455090099000      ]
 set x2range[   1567.5100000000000      :   1571.4300000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           30 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.92}{/CMSY10 \006}10^{12.32}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=42.11{/CMSY10 \006}1.22}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=287.81}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.2911}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 67 / 77  lines: 1 / 136}"

 set xrange[   93957.416925746977      :   95373.192874618646      ]
 set x2range[   1580.1500000000001      :   1587.6300000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           73 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.94}{/CMSY10 \006}10^{12.69}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}6.79}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=381.21}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.3029}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           74 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.81}{/CMSY10 \006}10^{12.67}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}8.95}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=296.89}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.3016}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9           75 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.83}{/CMSY10 \006}10^{12.71}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}9.23}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=312.18}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=0.3042}" at graph 0.37,   0.180000007     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 68 / 77  lines: 1 / 136}"

 set xrange[   94817.522039180345      :   96319.617120222785      ]
 set x2range[   1584.6900000000001      :   1592.6500000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           18 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.28}{/CMSY10 \006}10^{12.87}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}4.32}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=669.33}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.3067}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           19 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.26}{/CMSY10 \006}10^{12.83}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}4.31}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=644.17}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.3057}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9           20 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.84}{/CMSY10 \006}10^{12.70}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=80.96{/CMSY10 \006}7.03}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=306.47}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=0.3078}" at graph 0.37,   0.180000007     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 69 / 77  lines: 1 / 136}"

 set xrange[   95784.556930301274      :   99021.729221687652      ]
 set x2range[   1589.8099999999999      :   1607.0699999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           21 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.30}{/CMSY10 \006}10^{13.15}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=87.94{/CMSY10 \006}6.11}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=647.25}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.3132}" at graph 0.37,   0.100000001     tc rgb "royalblue"
 set label front "{/=9           22 }" at graph 0.02,   0.140000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.17}{/CMSY10 \006}10^{13.33}" at graph 0.05,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}15.06}" at graph 0.20,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 EW=561.68}" at graph 0.29,   0.140000001     tc rgb "royalblue"
 set label front "{/=8 z=0.3153}" at graph 0.37,   0.140000001     tc rgb "royalblue"
 set label front "{/=9           23 }" at graph 0.02,   0.180000007     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.21}{/CMSY10 \006}10^{13.00}" at graph 0.05,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}6.43}" at graph 0.20,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 EW=601.04}" at graph 0.29,   0.180000007     tc rgb "royalblue"
 set label front "{/=8 z=0.3113}" at graph 0.37,   0.180000007     tc rgb "royalblue"
 set label front "{/=9           24 }" at graph 0.02,   0.219999999     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.18}{/CMSY10 \006}10^{13.09}" at graph 0.05,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}9.53}" at graph 0.20,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 EW=575.48}" at graph 0.29,   0.219999999     tc rgb "royalblue"
 set label front "{/=8 z=0.3141}" at graph 0.37,   0.219999999     tc rgb "royalblue"
 set label front "{/=9           25 }" at graph 0.02,   0.259999990     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.98}{/CMSY10 \006}10^{12.70}" at graph 0.05,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 b=64.34{/CMSY10 \006}4.20}" at graph 0.20,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 EW=365.03}" at graph 0.29,   0.259999990     tc rgb "royalblue"
 set label front "{/=8 z=0.3093}" at graph 0.37,   0.259999990     tc rgb "royalblue"
 set label front "{/=9           26 }" at graph 0.02,   0.299999982     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.18}{/CMSY10 \006}10^{13.18}" at graph 0.05,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}8.49}" at graph 0.20,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 EW=570.20}" at graph 0.29,   0.299999982     tc rgb "royalblue"
 set label front "{/=8 z=0.3162}" at graph 0.37,   0.299999982     tc rgb "royalblue"
 set label front "{/=9           27 }" at graph 0.02,   0.339999974     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.18}{/CMSY10 \006}10^{13.17}" at graph 0.05,   0.339999974     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}10.64}" at graph 0.20,   0.339999974     tc rgb "royalblue"
 set label front "{/=8 EW=577.06}" at graph 0.29,   0.339999974     tc rgb "royalblue"
 set label front "{/=8 z=0.3122}" at graph 0.37,   0.339999974     tc rgb "royalblue"
 set label front "{/=9           28 }" at graph 0.02,   0.379999965     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.83}{/CMSY10 \006}10^{12.71}" at graph 0.05,   0.379999965     tc rgb "royalblue"
 set label front "{/=8 b=100.00{/CMSY10 \006}8.75}" at graph 0.20,   0.379999965     tc rgb "royalblue"
 set label front "{/=8 EW=312.17}" at graph 0.29,   0.379999965     tc rgb "royalblue"
 set label front "{/=8 z=0.3176}" at graph 0.37,   0.379999965     tc rgb "royalblue"
 set label front "{/=9           29 }" at graph 0.02,   0.419999957     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.53}{/CMSY10 \006}10^{12.97}" at graph 0.05,   0.419999957     tc rgb "royalblue"
 set label front "{/=8 b=36.22{/CMSY10 \006}6.36}" at graph 0.20,   0.419999957     tc rgb "royalblue"
 set label front "{/=8 EW=146.73}" at graph 0.29,   0.419999957     tc rgb "royalblue"
 set label front "{/=8 z=0.3147}" at graph 0.37,   0.419999957     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 70 / 77  lines: 1 / 136}"

 set xrange[   98965.760660551328      :   99606.907321751874      ]
 set x2range[   1606.7700000000000      :   1610.2100000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          104 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.20}{/CMSY10 \006}10^{12.32}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=24.79{/CMSY10 \006}3.89}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=74.01}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.3232}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 71 / 77  lines: 1 / 136}"

 set xrange[   104075.28949604846      :   104742.21858831140      ]
 set x2range[   1634.3900000000001      :   1638.0300000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           91 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.35}{/CMSY10 \006}10^{12.26}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=30.19{/CMSY10 \006}2.90}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=100.71}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.3459}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 72 / 77  lines: 1 / 136}"

 set xrange[   110344.81469477921      :   111091.14598190927      ]
 set x2range[   1668.9300000000001      :   1673.0899999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           78 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.64}{/CMSY10 \006}10^{12.31}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=34.02{/CMSY10 \006}1.86}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=173.32}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.3745}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 73 / 77  lines: 1 / 136}"

 set xrange[   112578.25757207323      :   113329.72738676243      ]
 set x2range[   1681.4100000000001      :   1685.6300000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           82 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.66}{/CMSY10 \006}10^{12.49}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=67.39{/CMSY10 \006}5.37}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=210.80}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.3847}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 74 / 77  lines: 1 / 136}"

 set xrange[   115929.26829729955      :   116682.94792125390      ]
 set x2range[   1700.3099999999999      :   1704.5899999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           31 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{14.03}{/CMSY10 \006}10^{12.48}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=71.59{/CMSY10 \006}2.39}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=408.79}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.4004}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 75 / 77  lines: 1 / 136}"

 set xrange[   117083.66861079448      :   117764.36786919051      ]
 set x2range[   1706.8699999999999      :   1710.7500000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9           88 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.56}{/CMSY10 \006}10^{12.36}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=42.72{/CMSY10 \006}3.17}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=158.61}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.4056}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 76 / 77  lines: 1 / 136}"

 set xrange[   118988.53757370749      :   119654.48216212232      ]
 set x2range[   1717.7500000000000      :   1721.5699999999999      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          109 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.25}{/CMSY10 \006}10^{12.30}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=32.92{/CMSY10 \006}4.42}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=84.40}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.4147}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 77 / 77  lines: 1 / 136}"

 set xrange[   120433.60512575950      :   121065.15495506923      ]
 set x2range[   1726.0500000000000      :   1729.6900000000001      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          120 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.12}{/CMSY10 \006}10^{12.21}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=27.74{/CMSY10 \006}4.00}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=63.41}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.4213}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

 # Set title

 set title "{/=10 combined-0153-h1-noconv h1 S/N= 10.357 region: 78 / 77  lines: 1 / 136}"

 set xrange[   121837.16759935959      :   122545.11574542575      ]
 set x2range[   1734.1500000000001      :   1738.2500000000000      ]
 set xlabel "v_{Hubble} [km/s]"
 set x2label "Wavelength [\305]"

 set ytics format "%5.2f"
 set yrange[-0.05:1.5]
 set ylabel "Normalised Flux"
 set label front "{/=9          122 }" at graph 0.02,   0.100000001     tc rgb "black"
 set label front "{/=8 N_{h1}=10^{13.24}{/CMSY10 \006}10^{12.23}" at graph 0.05,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 b=33.27{/CMSY10 \006}3.77}" at graph 0.20,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 EW=83.53}" at graph 0.29,   0.100000001     tc rgb "royalblue"
 set label front "{/=8 z=0.4284}" at graph 0.37,   0.100000001     tc rgb "royalblue"

 unset boxwidth
 set style fill solid 1.0

 plot 'output_data/combined_0153_h1_noconv.psh' index 0 using ($4):($2+$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2-$7) w histeps lt 1 lw 1 lc rgb "dark-gray" notitle \
 , "" index 0 using ($4):($2) w histeps lt 1 lw 1.5 lc rgb "black" title "Data" \
 , "" index 0 using ($4):($3) w l lt 1 lw 1.5 lc rgb "red" title "Model (ifit!)" \
 , 1 w l lt 4 lw 1.5 lc rgb "black" notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 1 using ($1):(1.1):("|") w labels notitle \
 , 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($3):5 w labels font "Helvetica,9" tc lt 3 notitle \
 #, 'output_data/combined_0153_h1_noconv.psh' index 2 using ($1):($4):2 w labels font "Helvetica,9" tc lt 3 notitle \

 # Unset labels for next panel
 unset label

