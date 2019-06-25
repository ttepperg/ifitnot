*	(C) Thorsten Tepper Garcia 2006
*
*	CONTAINS THE VARIABLES AND PARAMETERS NEEDED TO RUN
*	profiles.f
*
*	=============================================================
*	Physical parameters (cgs UNITS)

	double precision sigma0,CLIGHT,ERADIUS,PI,ECHARGE,EMASS
     	parameter (sigma0=6.3D-18)
	parameter (CLIGHT=2.9979D+10)
	parameter (ERADIUS=2.817D-13)
	parameter (PI=3.141593D0)
	parameter (ECHARGE=1.602D-19,EMASS=9.109D-34)
     	
	double precision resolution
	parameter (resolution = 0.01)
	
	integer nlines, line_num, points
	parameter (nlines = 25, points = 50000)
