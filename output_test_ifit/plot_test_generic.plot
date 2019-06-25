set xlabel "log (N_{ion}/cm^{-2})
set ylabel "b-value [km s^{-1}]"

set key top left Left reverse

# displacement vector
dr(x,y) = y > 0 ? y - x : 0

# Detection limit (sn = signal-to-noise, tau_central)
tau_central = 1.0e-13
log_b_min(logN,tau) = log10(tau_central) + logN - log10(tau)

# minimum value
min(x,y) = x < y ? x : y

#-------------------------------------------------------------------------------
# plot variables:
# plotfile, ion, profile, fwhm, logNlim, logNmin, logNmax, bmin, bmax, sn

# need to load these for each case:

load './output_test_ifit/settings.plot'

tau_min = 1.0 / sn
tau_max = sn
tau_max_abs = -1.0*log(1.17549435e-38)

indx=0

set term x11 indx enhanced

set title \
sprintf("S/N = %5.0f    ion=%s    profile=%s    fwhm=%5.2f", \
sn, ion, profile, fwhm)

unset arrow
set arrow from first logNlim, bmin to first logNlim, bmax \
nohead lw 1 lt 2 lc rgb "magenta"

set xrange [logNmin:logNmax]
set yrange [bmin:bmax]

plot plotfile index indx u 1:2 pt 2 ps 1 lc rgb "black" \
title "data", \
'' index indx u ($3):($4):(-($3)+log10(10**($3)+10**($5))):6 w xyerrorbars \
ps 0 lw 1 lc rgb "web-green" title "fit", \
'' index indx u \
1:2:(dr(($1),($3))):(dr(($2),($4))) w vectors nohead lt 1 lw 1 lc rgb "red" \
title "fit <-> data", \
10**(log_b_min(x,tau_min)) w l lt 2 lc rgb "royalblue" \
title "formal low detect. lim.", \
10**(log_b_min(x,tau_max)) w l lt 2 lc rgb "black" \
title "formal hi detect. lim.", \
10**(log_b_min(x,tau_max_abs)) w l lt 2 lc rgb "magenta" \
title "formal abs. detect. lim."
