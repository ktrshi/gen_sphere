      integer nnx,nny,nnt
      parameter (nnx=1280, nny=1280, nnt=102)
      real       cltd(nnx,nny,nnt)

      integer NSHOW,ISHW,NSEQ,  ixa,iya
      real EV(9)
      double precision PRMPAR(3),THETAP,PHIP,THICK0,HEIGHP


      integer nx,ny,nt,    ix,iy,it
      parameter (nx=640, ny=640, nt=102)
      real       clltd(nx,ny,nt)
      INTEGER          ISEED(3)
      integer index
      real    dx,       dy,      dt                !   for clltd
      real thx,thy,  x,y,t, r,   tmin,tmax
      data tmin,tmax/1.0e8,0.0/
      integer i,j,k,   nph, m
      integer ii,jj,kk,mm
      data ii,jj,kk,mm/4*-1/
      real gy
      data gy/-1.0/
      real h,h2,xx,yy,tt,   xi,eta,tau,tbig,time, theta,phi, pi
**      data theta/2.61799e-01/,  phi/0.0/
****      data pi/3.141592653589793/,  h/500.0/
      data pi/3.141592653589793/
      real sth,cth,sph,cph, hprint,  alpha, cal, rho,rho2, rad2grad,
     *               coeff,  a0,a1,a2,a3
      data a3,a2,a1,a0/-3.57144017E-7,-1.30949484E-5,-3.57137062E-4,
     *                   0.995237947/
      real xsh,ysh
      data xsh,ysh/0.0,0.0/
      real*8    a,               b,bg
      DOUBLE PRECISION RD(3)

***       CLOUT4w_Q1_atm01_0014_30PeV_05_001
      character*34 fn_in
      character*46 fn_ph2tr
      data fn_ph2tr/'phels_to_trace_Q0_atm00_0000_00PeV_00_000_c000'/
	                !phels_to_trace_Q2_atm01_0014_10PeV_25_000_0010
      integer catm,  mmm, key, last_c
      real zz,   rrr, dzeta, ru
**      data rrr/500.0/
      character*1 aa(10)
      data aa/'0','1','2','3','4','5','6','7','8','9'/
      integer n1,n10,n100,n

      real dtsq    !  additional delay due to the shower inclination


      open(unit=10,file='sphall_g4_sh_wbg.in',status='OLD',
     *     form='FORMATTED',access='SEQUENTIAL')
      read(10,*) catm
      read(10,*) fn_in
      read(10,*) zz
      read(10,*) key
	  read(10,*) last_c
      close(10)
      h = abs(zz)

      rrr = 500.0
      if(h.lt.600.0)  rrr = 300.0

      !i = catm/10
      !fn_ph2tr(22:22) = aa(i+1)
      !catm = catm - i*10
      !fn_ph2tr(23:23) = aa(catm+1)

      if(key.eq.1)  fn_ph2tr(43:43) = 'B'


**      write(*,*) '  nx=',nx,' ny=',ny,' nt=',nt

      b = 23.4375d3         !  this is the average yield of NSBG per 3D snow bin (area*time)
          !  f=3*10**12 photon * m-2 s-1 => b = eta * f * s * tau = 0.25 * 3 * 10**12 * 6.25 * 5*10**(-9) =
	  !   = 23.4375 * 10**3 photoelectrons   		
*******************************************************************
        !    ***   now we make BG level 36 times higher!!!   ***
****      b = b * 36.0d0
      b = b * 1.30d0
********************************************************************
      

      fn_ph2tr(16:41) = fn_in(9:34)
***      fn_ph2tr(23:35) = fn_in(12:24)
      

      rad2grad = 180.0/pi


***          if(key.ne.1) then   ! ===========================

***      open(unit=77,file='CLOUT',status='OLD',
      open(unit=77,file=fn_in,status='OLD',
     *     form='UNFORMATTED',access='SEQUENTIAL')
      read(77) NSHOW
      write(*,*) '  NSHOW=',NSHOW

      read(77) ISHW,NSEQ,EV(1),EV(2),EV(3),
     *  	 EV(4),EV(5),EV(6),
     *  	 EV(7),EV(8),EV(9),PRMPAR(1),
     *  	 PRMPAR(2),THETAP,PHIP,THICK0,PRMPAR(3),HEIGHP

      write(*,100) ISHW,NSEQ,EV(1),EV(2),EV(3),
     *  	   EV(4),EV(5),EV(6),
     *  	   EV(7),EV(8),EV(9),PRMPAR(1),
     *  	   PRMPAR(2),THETAP,PHIP,THICK0,PRMPAR(3),HEIGHP


      if(key.ne.1)  read(77) cltd


      close(77)

      theta = thetap
      phi = phip
      hprint = sngl(HEIGHP*0.01)
      cph =cos(phi)
      sph = sin(phi)
      sth = sin(theta)

**      write(*,*) '          theta=',theta,' phi=',phi,'  hprint=',hprint

      dx = 250.0   ! cm
      dy = 250.0   ! cm
      dt = 5.0     ! ns
****	      a = 0.7d0 * cltd(i,j,k) * coeff / rho2 * rad2grad  !  snow reflection indicatrix added  ! =========

      h2 = h*h
      cth = cos(theta)
      tbig = (hprint - 455.0)/(cth*2.99792458e8) * 1.0e9            !  time delay in ns
*****                               axlen = (PRMPAR(5) - obslev(1))/costh
*****  tdel = (temis + (etadsn*(thckob(nobslv)-thckem)/wemis + pathcr - axlen)/c(25)) * 1.0d9   !  time delay in ns

***          endif               ! ===========================

      ISEED(1) = 1
      ISEED(2) = 0
      ISEED(3) = 0

      CALL RMMAQD( ISEED,1,'S' )
      do i=1,10
        a = rannor(1.0d0,1.0d0)
**	write(*,*) ' --> a=',a,'  *****'
      enddo
**      write(*,*) ''

*      open(unit=20,file='all_map',status='OLD',
*     *     form='FORMATTED',access='SEQUENTIAL')

          if(key.ne.1) then   ! ===========================

**      do i=1,nx
**        do j=1,ny
**	  do k=1,nt
**	    clltd(i,j,k) = cltd(i+320,j+320,k)
**	  enddo
**	enddo
**      enddo

*      close(20)

**	write(*,*) '  *** clltd is ready  '
**	write(*,*) '  *** cltd is ready  '
**      write(*,*) ''

          endif               ! ===========================


            do mmm=last_c,100    ! loop over the event clones different in (xsh,ysh) begins
**            do mmm=1,1    ! loop over the event clones different in (xsh,ysh) begins

        n100 = mmm/100
	n = mmm - n100*100
	n10 = n/10
	n1 = n - n10*10
	fn_ph2tr(44:44) = aa(n100+1)
	fn_ph2tr(45:45) = aa(n10+1)
	fn_ph2tr(46:46) = aa(n1+1)

        CALL RMMARD( RD,2,1 )
        dzeta = 2.0*pi*rd(1)
	ru = rrr*sqrt(rd(2))
	xsh = ru*cos(dzeta)
	ysh = ru*sin(dzeta)
**      write(*,*) '   xsh=',xsh,' ysh=',ysh


**      open(unit=30,file='photons_to_trace',status='NEW',
      open(unit=30,file=fn_ph2tr,status='NEW',
     *     form='FORMATTED',access='SEQUENTIAL')

        write(30,800) mmm,zz,xsh,ysh,catm
***                              write(*,800) mmm,zz,xsh,ysh,catm

**      do i=1,nx
**        xx = 2.5*(i - nx/2 - 0.5)
**        do j=1,ny
**          yy = 2.5*(j - ny/2 - 0.5)
      do i=1,nnx
        xx = 2.5*(i - nnx/2 - 0.5)
        do j=1,nny
          yy = 2.5*(j - nny/2 - 0.5)

          dtsq = (xx*cph + yy*sph)*sth /0.299792458   ! additional delay in ns 

**          do k=2,nt-1
          do k=2,nnt-1                ! ************
	    tt = 5.0*(k - 1.5)

****      write(*,*) '          clltd(i,j,k)=',clltd(i,j,k)

              rho2 = h2+(xx+xsh)*(xx+xsh)+(yy+ysh)*(yy+ysh)
	      rho = sqrt(rho2)


	      cal = h/rho
	      alpha = acos(cal)*rad2grad
*********************************************************************************
	      coeff = (a0 + alpha*(a1 + alpha*(a2 + alpha*a3)))
     *              *cal     /(2.0*pi)      ! ***********************
*********************************************************************************


                if(key.ne.1) then   ! ===========================

**	    if(clltd(i,j,k).gt.0.0)  then      !   CL sampling
	    if(cltd(i,j,k).gt.0.0)  then      !   CL sampling
***	      a = 1.0d0 * clltd(i,j,k) / (2.0*pi*(h2+xi*xi+yj*yj))
*              rho2 = h2+(xx+xsh)*(xx+xsh)+(yy+ysh)*(yy+ysh)
*	      rho = sqrt(rho2)
*	      cal = h/rho
*	      alpha = acos(cal)*rad2grad
*********************************************************************************
*	      coeff = (a0 + alpha*(a1 + alpha*(a2 + alpha*a3)))
*     *              *cal     /(2.0*pi)      ! ***********************
****     *              *cal     /161.386           ! =======================

***     *                   /299.342407
*********************************************************************************
**	    write(*,*) '  coeff=',coeff

****	      a = 0.7d0 * clltd(i,j,k) * coeff / rho2 * rad2grad  !  snow reflection indicatrix added  ! =========
****	      a = 0.7d0 * clltd(i,j,k) * coeff / rho2 !  snow reflection indicatrix added  ! *********
	      a = 1.36848d0 * cltd(i,j,k) * coeff / rho2 !  snow reflection indicatrix added  ! *********

	      nph = int(poiss(a)+0.01)   ! this is the random number of photoelectrons from CL
**	    write(*,*) '  nph=',nph

	      if(nph.gt.0)  then
**                write(*,*) '  nph=',nph

	        do m=1,nph
	          call rmmard(rd,3,1)
		  xi  = 2.5*(rd(1) - 0.5)
		  eta = 2.5*(rd(2) - 0.5)
		  tau = 5.0*(rd(3) - 0.5)
		  x = xx + xi + xsh
		  y = yy + eta + ysh
***		  t = tt + tau + tbig
		  t = tt + tau + tbig + rho/0.299792458  + dtsq
		  write(30,700) i,j,k,m,x,y,t
**		                    write(*,700) i,j,k,m,x,y,t
		  if(t.lt.tmin)  tmin = t
		  if(t.gt.tmax)  tmax = t
	        enddo

	      endif

	    endif

                endif               ! ===========================



***********   this is NSBG additional photoelectrons, if necessary

                if(key.ge.1) then   ! ===========================

                    if(key.eq.1)  nph = 0

****            bg = b * 0.7d0 * coeff / rho2
            bg = b * 1.36848d0 * coeff / rho2
***	    write(*,*) '     bg=',bg
            nphbg = int(poiss(bg)+0.01)     !  this is the random number of photoelectrons from NSBG
***	    write(*,*) '  nphbg=',nphbg

	    if(nphbg.gt.0)  then

	        do m=nph+101,nph+nphbg+100
	          call rmmard(rd,3,1)
		  xi  = 2.5*(rd(1) - 0.5)
		  eta = 2.5*(rd(2) - 0.5)
		  tau = 5.0*(rd(3) - 0.5)
		  x = xx + xi + xsh
		  y = yy + eta + ysh
***		  t = tt + tau + tbig
		  t = tt + tau + tbig + rho/0.299792458
		  write(30,700) i,j,k,m,x,y,t
***		                    write(*,700) i,j,k,m,x,y,t
		  if(t.lt.tmin)  tmin = t
		  if(t.gt.tmax)  tmax = t
	        enddo

	    endif

                endif               ! ===========================


          enddo
	enddo
      enddo
		  write(30,700) ii,jj,kk,mm,tmin,tmax,tbig
**		           write(*,700) ii,jj,kk,mm,tmin,tmax,tbig

      close(30)

      write(*,*) ' file #',mmm,' phels_to_trace  is written'


            enddo       ! loop over the event clones different in (xsh,ysh) ends



      stop
  100 format('  ISHW,NSEQ: ',i10,1x,i10/'  EV(1),EV(2),EV(3): ',
     *       3(1x,1pd12.5)/'  EV(4),EV(5),EV(6): ',3(1x,1pd12.5)/
     *       '  EV(7),EV(8),EV(9): ',3(1x,1pd12.5)/
     *       '  PRMPAR(1),PRMPAR(2),THETAP,PHIP: ',4(1x,1pd14.7)/
     *       '  THICK0,PRMPAR(3),HEIGHP: ',3(1x,1pd14.7))
  600 format(10(1x,1pe12.5))
  700 format(4(1x,i6),2(1x,1pe14.7),1x,1pe14.7)
  800 format(1x,i4,3(1x,1pe12.5),1x,i3)
      end

C=======================================================================

      real function poiss(alam)

*      IMPLICIT NONE

*      real poiss
      real*8 alam,s,alpha,p,ex
      integer i,j,k,  l
*
**      COMMON /CRRANDPA/RD,FAC,U1,U2,NSEQ,ISEED,KNOR
**      DOUBLE PRECISION RD(3),FAC,U1,U2
**      INTEGER          ISEED(3,10),NSEQ
**      LOGICAL          KNOR
*
      DOUBLE PRECISION RD(3)

      save

      l = int(alam)
**      write(*,*) '   l=',l
      ex = dexp(-alam)
      p = ex

      CALL RMMARD( RD,1,1 )
*****      rd(1) = 0.5

      alpha = rd(1)
*      write(*,*) '   l=',l,' alpha=',alpha

      if(l.eq.0)  then    !  one way algorithm
**        p = ex
	i = 0
**	j = 1
   10   continue
	if(alpha.le.p) then
	  poiss = 1.0*i
	  return
	endif
	alpha = alpha - p
	i = i + 1
**	  j = j*i
	p = p*alam/i
	go to 10
      else                !  two ways algorithm
        s = p
        do k=1,l
	  p = p*alam/k
	  s = s + p
*	write(*,*) '   k=',k,' p=',p,' s=',s,' alam/k=',alam/k
	enddo
**	write(*,*) '   s=',s,' p=',p
	i = l
	if(alpha.le.s) then   !  go back
   20     s = s - p
	  if(alpha.gt.s) then
	    poiss = 1.0*i
	    return
	  endif
	  p = p*i/alam
	  i = i - 1
*	write(*,*) ' <  i=',i,' p=',p,' s=',s,' alam/i=',alam/i
	  if(i.lt.0)  stop
	  go to 20
	else                  !  go forward
   30     i = i + 1
	  p = p*alam/i
	  s = s + p
	  if(alpha.le.s) then
	    poiss = 1.0*i
	    return
	  endif
*	write(*,*) ' >  i=',i,' p=',p,' s=',s
	  go to 30
	endif
      endif

      return
      end

C=======================================================================

***      DOUBLE PRECISION FUNCTION RANNOR( A,B )
      real FUNCTION RANNOR( A,B )

C-----------------------------------------------------------------------
C  RAN(DOM NUMBER) NOR(MALLY DISTRIBUTED)
C
C  GENERATES NORMAL DISTRIBUTED RANDOM NUMBER
C  DELIVERS 2 UNCORRELATED RANDOM NUMBERS,
C  THEREFORE RANDOM CALLS ARE ONLY NECESSARY EVERY SECOND TIME.
C  REFERENCE : NUMERICAL RECIPES, W.H. PRESS ET AL.,
C              CAMBRIDGE UNIVERSITY PRESS, 1992  ISBN 0 521 43064 X
C  THIS FUNCTION IS CALLED FROM HDPM, LEADDF, PARRAP, QGSTOR,
C  UPDATE, AND VAPOR.
C  ARGUMENTS:
C   A      = MEAN VALUE
C   B      = STANDARD DEVIATION
C-----------------------------------------------------------------------

*      IMPLICIT NONE

*      COMMON /CRCONSTA/PI,PI2,OB3,TB3,ENEPER
*      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER

*      COMMON /CRRANDPA/RD,FAC,U1,U2,NSEQ,ISEED,KNOR
**      COMMON /CRRANDPA/RD,FAC,U1,U2,  ISEED,KNOR
      DOUBLE PRECISION RD(3),FAC,U1,U2
*      INTEGER          ISEED(3,10),NSEQ
*      INTEGER          ISEED(3),NSEQ
**      INTEGER          ISEED(3)
      LOGICAL          KNOR       

      DOUBLE PRECISION A,B,RR   
      SAVE
C-----------------------------------------------------------------------

CC    IF ( DEBUG ) WRITE(MDEBUG,100) SNGL(A),SNGL(B)
CC100 FORMAT(' RANNOR: A,B=',1P,2E10.3)
      IF ( KNOR ) THEN
  1     CONTINUE
        CALL RMMARD( RD,2,1 )
*****        rd(1) = 0.7
*****	rd(2) = 0.3

        U1 = 2.D0*RD(1) - 1.D0
        U2 = 2.D0*RD(2) - 1.D0
        RR = U1**2 + U2**2
        IF ( RR .GE. 1.D0  .OR.  RR .EQ. 0.D0 ) GOTO 1
        FAC = SQRT( (-2.D0) * LOG(RR) / RR )

        RANNOR = sngl(FAC * U1 * B + A)
        KNOR   = .FALSE.
      ELSE
        RANNOR = sngl(FAC * U2 * B + A)
        KNOR   = .TRUE.
      ENDIF
CC    IF ( DEBUG ) WRITE(MDEBUG,101) RANNOR
CC101 FORMAT('+',34X,' RANNOR =',1P,E12.5)

      RETURN
      END

C=======================================================================

      SUBROUTINE RMMAQD( ISEED,ISEQ,CHOPT )

C-----------------------------------------------------------------------
C
C  SUBROUTINE FOR INITIALIZATION OF RMMARD
C  THESE ROUTINE RMMAQD IS A MODIFIED VERSION OF ROUTINE RMMAQ FROM
C  THE CERN LIBRARIES. DESCRIPTION OF ALGORITHM SEE:
C               http://consult.cern.ch/shortwrups/v113/top.html
C  FURTHER DETAILS SEE SUBR. RMMARD
C  ARGUMENTS:
C   ISEED  = SEED TO INITIALIZE A SEQUENCE (3 INTEGERS)
C   ISEQ   = # OF RANDOM SEQUENCE
C   CHOPT  = CHARACTER TO STEER INITIALIZE OPTIONS
C-----------------------------------------------------------------------

*      IMPLICIT NONE

      INTEGER          KSEQ
      PARAMETER        (KSEQ = 5)
      COMMON /CRRANMA3/CD,CINT,CM,TWOM24,TWOM48,MODCNS
      DOUBLE PRECISION CD,CINT,CM,TWOM24,TWOM48
      INTEGER          MODCNS

      COMMON /CRRANMA4/C,U,IJKL,I97,J97,NTOT,NTOT2,JSEQ
      DOUBLE PRECISION C(KSEQ),U(97,KSEQ),UNI
      INTEGER          IJKL(KSEQ),I97(KSEQ),J97(KSEQ),
     *                 NTOT(KSEQ),NTOT2(KSEQ),JSEQ


      DOUBLE PRECISION CC,S,T,UU(97)
      INTEGER          ISEED(3),I,IDUM,II,II97,IJ,IJ97,IORNDM,
     *                 ISEQ,J,JJ,K,KL,L,LOOP2,M,NITER
      CHARACTER        CHOPT*(*), CCHOPT*12
      LOGICAL          FIRST
      SAVE
      DATA             FIRST / .TRUE. /, IORNDM/11/, JSEQ/1/
C-----------------------------------------------------------------------

      IF ( FIRST ) THEN
        TWOM24 = 2.D0**(-24)
        TWOM48 = 2.D0**(-48)
        CD     = 7654321.D0*TWOM24
        CM     = 16777213.D0*TWOM24
        CINT   = 362436.D0*TWOM24
        MODCNS = 1000000000
        FIRST  = .FALSE.
      ENDIF

      CCHOPT = CHOPT
      IF ( CCHOPT .EQ. ' ' ) THEN
        ISEED(1) = 54217137
        ISEED(2) = 0
        ISEED(3) = 0
        CCHOPT   = 'S'
        JSEQ     = 1
      ENDIF

      IF     ( INDEX(CCHOPT,'S') .NE. 0 ) THEN
        IF ( ISEQ .GT. 0  .AND.  ISEQ .LE. KSEQ ) JSEQ = ISEQ
        IF ( INDEX(CCHOPT,'V') .NE. 0 ) THEN
          READ(IORNDM,'(3Z8)') IJKL(JSEQ),NTOT(JSEQ),NTOT2(JSEQ)
          READ(IORNDM,'(2Z8,Z16)') I97(JSEQ),J97(JSEQ),C(JSEQ)
          READ(IORNDM,'(24(4Z16,/),Z16)') U
          IJ = IJKL(JSEQ)/30082
          KL = IJKL(JSEQ) - 30082 * IJ
          I  = MOD(IJ/177, 177) + 2
          J  = MOD(IJ, 177)     + 2
          K  = MOD(KL/169, 178) + 1
          L  = MOD(KL, 169)
          CD =  7654321.D0 * TWOM24
          CM = 16777213.D0 * TWOM24
        ELSE
          IJKL(JSEQ)  = ISEED(1)
          NTOT(JSEQ)  = ISEED(2)
          NTOT2(JSEQ) = ISEED(3)
          IJ = IJKL(JSEQ) / 30082
          KL = IJKL(JSEQ) - 30082*IJ
          I  = MOD(IJ/177, 177) + 2
          J  = MOD(IJ, 177)     + 2
          K  = MOD(KL/169, 178) + 1
          L  = MOD(KL, 169)
          DO  II = 1, 97
            S = 0.D0
            T = 0.5D0
            DO  JJ = 1, 48
              M = MOD(MOD(I*J,179)*K, 179)
              I = J
              J = K
              K = M
              L = MOD(53*L+1, 169)
              IF ( MOD(L*M,64) .GE. 32 ) S = S + T
              T = 0.5D0 * T
            ENDDO
            UU(II) = S
          ENDDO
          CC    = CINT
          II97  = 97
          IJ97  = 33
C  COMPLETE INITIALIZATION BY SKIPPING (NTOT2*MODCNS+NTOT) RANDOMNUMBERS
          NITER = MODCNS
          DO  LOOP2 = 1, NTOT2(JSEQ)+1
            IF ( LOOP2 .GT. NTOT2(JSEQ) ) NITER = NTOT(JSEQ)
            DO  IDUM = 1, NITER
              UNI = UU(II97) - UU(IJ97)
              IF ( UNI .LT. 0.D0 ) UNI = UNI + 1.D0
              UU(II97) = UNI
              II97     = II97 - 1
              IF ( II97 .EQ. 0 ) II97 = 97
              IJ97     = IJ97 - 1
              IF ( IJ97 .EQ. 0 ) IJ97 = 97
              CC       = CC - CD
              IF ( CC .LT. 0.D0 ) CC  = CC + CM
            ENDDO
          ENDDO
          I97(JSEQ) = II97
          J97(JSEQ) = IJ97
          C(JSEQ)   = CC
          DO  JJ = 1, 97
            U(JJ,JSEQ) = UU(JJ)
          ENDDO
        ENDIF
      ELSEIF ( INDEX(CCHOPT,'R') .NE. 0 ) THEN
        IF ( ISEQ .GT. 0 ) THEN
          JSEQ = ISEQ
        ELSE
          ISEQ = JSEQ
        ENDIF
        IF ( INDEX(CCHOPT,'V') .NE. 0 ) THEN
          WRITE(IORNDM,'(3Z8)') IJKL(JSEQ),NTOT(JSEQ),NTOT2(JSEQ)
          WRITE(IORNDM,'(2Z8,Z16)') I97(JSEQ),J97(JSEQ),C(JSEQ)
          WRITE(IORNDM,'(24(4Z16,/),Z16)') U
        ELSE
          ISEED(1) = IJKL(JSEQ)
          ISEED(2) = NTOT(JSEQ)
          ISEED(3) = NTOT2(JSEQ)
        ENDIF
      ENDIF

      RETURN
      END

*-- Author :    D. HECK IK FZK KARLSRUHE   17/03/2003
C=======================================================================

      SUBROUTINE RMMARD( RVEC,LENV,ISEQ )

C-----------------------------------------------------------------------
C  R(ANDO)M (NUMBER GENERATOR OF) MAR(SAGLIA TYPE) D(OUBLE PRECISION)
C
C  THESE ROUTINES (RMMARD,RMMAQD) ARE MODIFIED VERSIONS OF ROUTINES
C  FROM THE CERN LIBRARIES. DESCRIPTION OF ALGORITHM SEE:
C               http://consult.cern.ch/shortwrups/v113/top.html
C  IT HAS BEEN CHECKED THAT RESULTS ARE BIT-IDENTICAL WITH CERN
C  DOUBLE PRECISION RANDOM NUMBER GENERATOR RMM48, DESCRIBED IN
C               http://consult.cern.ch/shortwrups/v116/top.html
C  ARGUMENTS:
C   RVEC   = DOUBLE PREC. VECTOR FIELD TO BE FILLED WITH RANDOM NUMBERS
C   LENV   = LENGTH OF VECTOR (# OF RANDNUMBERS TO BE GENERATED)
C   ISEQ   = # OF RANDOM SEQUENCE
C
C  VERSION OF D. HECK FOR DOUBLE PRECISION RANDOM NUMBERS.
C-----------------------------------------------------------------------

*      IMPLICIT NONE

      INTEGER          KSEQ
      PARAMETER        (KSEQ = 5)
      COMMON /CRRANMA3/CD,CINT,CM,TWOM24,TWOM48,MODCNS
      DOUBLE PRECISION CD,CINT,CM,TWOM24,TWOM48
      INTEGER          MODCNS

      COMMON /CRRANMA4/C,U,IJKL,I97,J97,NTOT,NTOT2,JSEQ
      DOUBLE PRECISION C(KSEQ),U(97,KSEQ),UNI
      INTEGER          IJKL(KSEQ),I97(KSEQ),J97(KSEQ),
     *                 NTOT(KSEQ),NTOT2(KSEQ),JSEQ


      DOUBLE PRECISION RVEC(*)
      INTEGER          ISEQ,IVEC,LENV
      SAVE
C-----------------------------------------------------------------------

      IF ( ISEQ .GT. 0  .AND.  ISEQ .LE. KSEQ ) JSEQ = ISEQ

      DO  IVEC = 1, LENV
        UNI = U(I97(JSEQ),JSEQ) - U(J97(JSEQ),JSEQ)
        IF ( UNI .LT. 0.D0 ) UNI = UNI + 1.D0
        U(I97(JSEQ),JSEQ) = UNI
        I97(JSEQ)  = I97(JSEQ) - 1
        IF ( I97(JSEQ) .EQ. 0 ) I97(JSEQ) = 97
        J97(JSEQ)  = J97(JSEQ) - 1
        IF ( J97(JSEQ) .EQ. 0 ) J97(JSEQ) = 97
        C(JSEQ)    = C(JSEQ) - CD
        IF ( C(JSEQ) .LT. 0.D0 ) C(JSEQ)  = C(JSEQ) + CM
        UNI        = UNI - C(JSEQ)
        IF ( UNI .LT. 0.D0 ) UNI = UNI + 1.D0
C  AN EXACT ZERO HERE IS VERY UNLIKELY, BUT LET''S BE SAFE.
        IF ( UNI .EQ. 0.D0 ) UNI = TWOM48
        RVEC(IVEC) = UNI
      ENDDO

      NTOT(JSEQ) = NTOT(JSEQ) + LENV
      IF ( NTOT(JSEQ) .GE. MODCNS )  THEN
        NTOT2(JSEQ) = NTOT2(JSEQ) + 1
        NTOT(JSEQ)  = NTOT(JSEQ) - MODCNS
      ENDIF

      RETURN
      END

