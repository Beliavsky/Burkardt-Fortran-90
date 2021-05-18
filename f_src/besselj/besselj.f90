subroutine bessel_jx_values ( n_data, nu, x, fx )

!*****************************************************************************80
!
!! BESSEL_JX_VALUES returns some values of the Jx Bessel function.
!
!  Discussion:
!
!    This set of data considers the less common case in which the
!    index of the Bessel function Jn is actually not an integer.
!    We may suggest this case by occasionally replacing the symbol
!    "Jn" by "Jx".
!
!    In Mathematica, the function can be evaluated by:
!
!      BesselJ[n,x]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) NU, the order of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 28

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
        0.3544507442114011D+00, &
        0.6713967071418031D+00, &
        0.5130161365618278D+00, &
        0.3020049060623657D+00, &
        0.06500818287737578D+00, &
       -0.3421679847981618D+00, &
       -0.1372637357550505D+00, &
        0.1628807638550299D+00, &
        0.2402978391234270D+00, &
        0.4912937786871623D+00, &
       -0.1696513061447408D+00, &
        0.1979824927558931D+00, &
       -0.1094768729883180D+00, &
        0.04949681022847794D+00, &
        0.2239245314689158D+00, &
        0.2403772011113174D+00, &
        0.1966584835818184D+00, &
        0.02303721950962553D+00, &
        0.3314145508558904D+00, &
        0.5461734240402840D+00, &
       -0.2616584152094124D+00, &
        0.1296035513791289D+00, &
       -0.1117432171933552D+00, &
        0.03142623570527935D+00, &
        0.1717922192746527D+00, &
        0.3126634069544786D+00, &
        0.1340289119304364D+00, &
        0.06235967135106445D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) nu
  real ( kind = 8 ), save, dimension ( n_max ) :: nu_vec = (/ &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    1.50D+00, &
    1.50D+00, &
    1.50D+00, &
    1.50D+00, &
    1.50D+00, &
    2.50D+00, &
    2.50D+00, &
    2.50D+00, &
    2.50D+00, &
    2.50D+00, &
    1.25D+00, &
    1.25D+00, &
    1.25D+00, &
    1.25D+00, &
    1.25D+00, &
    2.75D+00, &
    2.75D+00, &
    2.75D+00, &
    2.75D+00, &
    2.75D+00 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
     0.2D+00, &
     1.0D+00, &
     2.0D+00, &
     2.5D+00, &
     3.0D+00, &
     5.0D+00, &
    10.0D+00, &
    20.0D+00, &
     1.0D+00, &
     2.0D+00, &
     5.0D+00, &
    10.0D+00, &
    50.0D+00, &
     1.0D+00, &
     2.0D+00, &
     5.0D+00, &
    10.0D+00, &
    50.0D+00, &
     1.0D+00, &
     2.0D+00, &
     5.0D+00, &
    10.0D+00, &
    50.0D+00, &
     1.0D+00, &
     2.0D+00, &
     5.0D+00, &
    10.0D+00, &
    50.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    nu = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    nu = nu_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine dbesj ( x, alpha, n, y, nz )

!*****************************************************************************80
!
!! DBESJ computes Bessel J functions of non integer order.
!
!  Discussion:
!
!    DBESJ computes an N member sequence of J Bessel functions
!    J/SUB(ALPHA+K-1)/(X), K=1,...,N for non-negative ALPHA and X.
!
!    A combination of the power series, the asymptotic expansion
!    for X to infinity and the uniform asymptotic expansion for
!    NU to infinity are applied over subdivisions of the (NU,X)
!    plane.  For values of (NU,X) not covered by one of these
!    formulae, the order is incremented or decremented by integer
!    values into a region where one of the formulae apply. Backward
!    recursion is applied to reduce orders by integer values except
!    where the entire sequence lies in the oscillatory region.  In
!    this case forward recursion is stable and values from the
!    asymptotic expansion for X to infinity start the recursion
!    when it is efficient to do so. Leading terms of the series and
!    uniform expansion are tested for underflow.  If a sequence is
!    requested and the last member would underflow, the result is
!    set to zero and the next lower order tried, etc., until a
!    member comes on scale or all members are set to zero.
!    Overflow cannot occur.
!
!    The maximum number of significant digits obtainable
!    is the smaller of 14 and the number of digits carried in
!    double precision arithmetic.
!
!  Author:
!
!    D. E. Amos, S. L. Daniel, M. K. Weston, 
!
!  Parameters
!
!         Input      X,ALPHA are double precision
!           X      - X  >=  0.0D0
!           ALPHA  - order of first member of the sequence,
!                    ALPHA  >=  0.0D0
!           N      - number of members in the sequence, N  >=  1
!
!         Output     Y is double precision
!           Y      - a vector whose first N components contain
!                    values for J/sub(ALPHA+K-1)/(X), K=1,...,N
!           NZ     - number of components of Y set to zero due to
!                    underflow,
!                    NZ=0   , normal return, computation completed
!                    NZ  /=  0, last NZ components of Y set to zero,
!                             Y(K)=0.0D0, K=N-NZ+1,...,N.
!
!  Reference:
!
!    D. E. Amos, S. L. Daniel, M. K. Weston, 
!    CDC 6600 subroutines IBESS and JBESS for Bessel functions
!    I(NU,X) and J(NU,X), X  >=  0, NU  >=  0, 
!    ACM Transactions on Mathematical Software
!    Volume 3, (1977), pages 76-92.
!
!    F. W. J. Olver, 
!    Tables of Bessel Functions of Moderate or Large Orders, 
!    NPL Mathematical Tables 6, 
!    Her Majesty's Stationery Office, London, 1962.
!
  implicit none

  real ( kind = 8 ) ak
  real ( kind = 8 ) d1mach
  real ( kind = 8 ) dlngam
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1mach

  EXTERNAL DJAIRY
  integer ( kind = 4 ) IALP,IDALP,IFLW,IN,INLIM,IS,I1,I2,K,KK,KM,KT,N,NN, &
          NS,NZ
  real ( kind = 8 ) AKM,ALPHA,ANS,AP,ARG,COEF,DALPHA,DFN,DTM, &
             EARG,ELIM1,ETX,FIDAL,FLGJY,FN,FNF,FNI,FNP1,FNU, &
             FNULIM,GLN,PDF,PIDT,PP,RDEN,RELB,RTTP,RTWO,RTX,RZDEN, &
             S,SA,SB,SXO2,S1,S2,T,TA,TAU,TB,TEMP,TFN,TM,TOL, &
             TOLLN,TRX,TX,T1,T2,WK,X,XO2,XO2L,Y,SLIM,RTOL

  SAVE RTWO, PDF, RTTP, PIDT, PP, INLIM, FNULIM
  DIMENSION Y(*), TEMP(3), FNULIM(2), PP(4), WK(7)
  DATA RTWO,PDF,RTTP,PIDT                    / 1.34839972492648D+00, &
   7.85398163397448D-01, 7.97884560802865D-01, 1.57079632679490D+00/
  DATA  PP(1),  PP(2),  PP(3),  PP(4)        / 8.72909153935547D+00, &
   2.65693932265030D-01, 1.24578576865586D-01, 7.70133747430388D-04/
  DATA INLIM           /      150            /
  DATA FNULIM(1), FNULIM(2) /      100.0D0,     60.0D0     /

  NZ = 0
  KT = 1
  NS=0
  TA = D1MACH(3)
  TOL = MAX(TA,1.0D-15)
  I1 = I1MACH(14) + 1
  I2 = I1MACH(15)
  TB = D1MACH(5)
  ELIM1 = -2.303D0*(I2*TB+3.0D0)
  RTOL=1.0D0/TOL
  SLIM=D1MACH(1)*RTOL*1.0D+3
!
!  TOLLN = -LN(TOL)
!
  TOLLN = 2.303D0 * TB * I1
  TOLLN = MIN ( TOLLN, 34.5388D0 )
  if (N-1) 720, 10, 20
   10 KT = 2
   20 NN = N
  if (X) 730, 30, 80

30 continue

  if ( alpha < 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DBESJ - Fatal error!'
    write ( *, '(a)' ) '  ALPHA < 0.0'
    stop 1
  else if ( alpha == 0.0D+00 ) then
    Y(1) = 1.0D+00
    if ( N == 1 ) then
      return
    end if
    I1 = 2
  else if ( 0.0D+00 < alpha ) then
    I1 = 1
  end if

  Y(i1:n) = 0.0D0

  return

  80 CONTINUE

  if (ALPHA < 0.0D0) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DBESJ - Fatal error!'
    write ( *, '(a)' ) '  ALPHA < 0.0'
    stop 1
  end if

  IALP = INT(ALPHA)
  FNI = IALP + N - 1
  FNF = ALPHA - IALP
  DFN = FNI + FNF
  FNU = DFN
  XO2 = X*0.5D0
  SXO2 = XO2*XO2
!
!  DECISION TREE FOR REGION WHERE SERIES, ASYMPTOTIC EXPANSION FOR X
!  TO INFINITY AND ASYMPTOTIC EXPANSION FOR NU TO INFINITY ARE APPLIED.
!
  if (SXO2 <= (FNU+1.0D0)) go to 90
  TA = MAX(20.0D0,FNU)
  if (X > TA) go to 120
  if (X > 12.0D0) go to 110
  XO2L = LOG(XO2)
  NS = INT(SXO2-FNU) + 1
  go to 100
   90 FN = FNU
  FNP1 = FN + 1.0D0
  XO2L = LOG(XO2)
  IS = KT
  if (X <= 0.50D0) go to 330
  NS = 0
  100 FNI = FNI + NS
  DFN = FNI + FNF
  FN = DFN
  FNP1 = FN + 1.0D0
  IS = KT
  if (N-1+NS > 0) IS = 3
  go to 330
  110 ANS = MAX(36.0D0-FNU,0.0D0)
  NS = INT(ANS)
  FNI = FNI + NS
  DFN = FNI + FNF
  FN = DFN
  IS = KT
  if (N-1+NS > 0) IS = 3
  go to 130
  120 CONTINUE
  RTX = SQRT(X)
  TAU = RTWO*RTX
  TA = TAU + FNULIM(KT)
  if (FNU <= TA) go to 480
  FN = FNU
  IS = KT
!
!  UNIFORM ASYMPTOTIC EXPANSION FOR NU TO INFINITY
!
  130 CONTINUE
  I1 = ABS(3-IS)
  I1 = MAX(I1,1)
  FLGJY = 1.0D0
  call DASYJY(DJAIRY,X,FN,FLGJY,I1,TEMP(IS),WK,IFLW)
  if ( IFLW /= 0) go to 380
  go to (320, 450, 620), IS
  310 TEMP(1) = TEMP(3)
  KT = 1
  320 IS = 2
  FNI = FNI - 1.0D0
  DFN = FNI + FNF
  FN = DFN
  if ( I1 == 2) go to 450
  go to 130
!
!  SERIES FOR (X/2)**2 <= NU+1
!
  330 CONTINUE
  GLN = DLNGAM(FNP1)
  ARG = FN*XO2L - GLN
  if (ARG < (-ELIM1)) go to 400
  EARG = EXP(ARG)
  340 CONTINUE
  S = 1.0D0
  if (X < TOL) go to 360
  AK = 3.0D0
  T2 = 1.0D0
  T = 1.0D0
  S1 = FN
  DO K=1,17
    S2 = T2 + S1
    T = -T*SXO2/S2
    S = S + T
    if (ABS(T) < TOL) go to 360
    T2 = T2 + AK
    AK = AK + 2.0D+00
    S1 = S1 + FN
  end do
  360 CONTINUE
  TEMP(IS) = S*EARG
  go to (370, 450, 610), IS
  370 EARG = EARG*FN/XO2
  FNI = FNI - 1.0D0
  DFN = FNI + FNF
  FN = DFN
  IS = 2
  go to 340
!
!  SET UNDERFLOW VALUE AND UPDATE PARAMETERS
!  UNDERFLOW CAN ONLY OCCUR FOR NS=0 SINCE THE ORDER MUST BE LARGER
!  THAN 36. THEREFORE, NS NEE NOT BE TESTED.
!
  380 Y(NN) = 0.0D0
  NN = NN - 1
  FNI = FNI - 1.0D0
  DFN = FNI + FNF
  FN = DFN
  if (NN-1) 440, 390, 130
  390 KT = 2
  IS = 2
  go to 130
  400 Y(NN) = 0.0D0
  NN = NN - 1
  FNP1 = FN
  FNI = FNI - 1.0D0
  DFN = FNI + FNF
  FN = DFN
  if (NN-1) 440, 410, 420
  410 KT = 2
  IS = 2
  420 if (SXO2 <= FNP1) go to 430
  go to 130
  430 ARG = ARG - XO2L + LOG(FNP1)
  if (ARG < (-ELIM1)) go to 400
  go to 330
  440 NZ = N - NN
  return
!
!  BACKWARD RECURSION SECTION
!
  450 CONTINUE
  if ( NS /= 0) go to 451
  NZ = N - NN
  if (KT == 2) go to 470
!
!  BACKWARD RECUR FROM INDEX ALPHA+NN-1 TO ALPHA
!
  Y(NN) = TEMP(1)
  Y(NN-1) = TEMP(2)
  if (NN == 2) RETURN
  451 CONTINUE
  TRX = 2.0D0/X
  DTM = FNI
  TM = (DTM+FNF)*TRX
  AK=1.0D0
  TA=TEMP(1)
  TB=TEMP(2)
  if ( ABS(TA) > SLIM) go to 455
  TA=TA*RTOL
  TB=TB*RTOL
  AK=TOL
  455 CONTINUE
  KK=2
  IN=NS-1
  if ( IN == 0) go to 690
  if ( NS /= 0) go to 670
  K=NN-2
  DO I=3,NN
    S=TB
    TB = TM*TB - TA
    TA=S
    Y(K)=TB*AK
    DTM = DTM - 1.0D0
    TM = (DTM+FNF)*TRX
    K = K - 1
  end do
  return
  470 Y(1) = TEMP(2)
  return
!
!  ASYMPTOTIC EXPANSION FOR X TO INFINITY WITH FORWARD RECURSION IN
!  OSCILLATORY REGION X > MAX(20, NU), PROVIDED THE LAST MEMBER
!  OF THE SEQUENCE IS ALSO IN THE REGION.
!
  480 CONTINUE
  IN = INT(ALPHA-TAU+2.0D0)
  if (IN <= 0) go to 490
  IDALP = IALP - IN - 1
  KT = 1
  go to 500
  490 CONTINUE
  IDALP = IALP
  IN = 0
  500 IS = KT
  FIDAL = IDALP
  DALPHA = FIDAL + FNF
  ARG = X - PIDT*DALPHA - PDF
  SA = SIN(ARG)
  SB = COS(ARG)
  COEF = RTTP/RTX
  ETX = 8.0D0*X
  510 CONTINUE
  DTM = FIDAL + FIDAL
  DTM = DTM*DTM
  TM = 0.0D0
  if (FIDAL == 0.0D0 .AND. ABS(FNF) < TOL) go to 520
  TM = 4.0D0*FNF*(FIDAL+FIDAL+FNF)
  520 CONTINUE
  TRX = DTM - 1.0D0
  T2 = (TRX+TM)/ETX
  S2 = T2
  RELB = TOL*ABS(T2)
  T1 = ETX
  S1 = 1.0D0
  FN = 1.0D0
  AK = 8.0D0
  DO K=1,13
    T1 = T1 + ETX
    FN = FN + AK
    TRX = DTM - FN
    AP = TRX + TM
    T2 = -T2*AP/T1
    S1 = S1 + T2
    T1 = T1 + ETX
    AK = AK + 8.0D0
    FN = FN + AK
    TRX = DTM - FN
    AP = TRX + TM
    T2 = T2*AP/T1
    S2 = S2 + T2
    if (ABS(T2) <= RELB) go to 540
    AK = AK + 8.0D0
  end do
  540 TEMP(IS) = COEF*(S1*SB-S2*SA)
  if ( IS == 2) go to 560
  FIDAL = FIDAL + 1.0D0
  DALPHA = FIDAL + FNF
  IS = 2
  TB = SA
  SA = -SB
  SB = TB
  go to 510
!
!  FORWARD RECURSION SECTION
!
  560 if (KT == 2) go to 470
  S1 = TEMP(1)
  S2 = TEMP(2)
  TX = 2.0D0/X
  TM = DALPHA*TX
  if (IN == 0) go to 580
!
!  FORWARD RECUR TO INDEX ALPHA
!
  DO I=1,IN
    S = S2
    S2 = TM*S2 - S1
    TM = TM + TX
    S1 = S
  end do
  if (NN == 1) go to 600
  S = S2
  S2 = TM*S2 - S1
  TM = TM + TX
  S1 = S
  580 CONTINUE
!
!  FORWARD RECUR FROM INDEX ALPHA TO ALPHA+N-1
!
  Y(1) = S1
  Y(2) = S2
  if (NN == 2) RETURN
  DO I=3,NN
    Y(I) = TM*Y(I-1) - Y(I-2)
    TM = TM + TX
  end do
  return
  600 Y(1) = S2
  return
!
!  BACKWARD RECURSION WITH NORMALIZATION BY
!  ASYMPTOTIC EXPANSION FOR NU TO INFINITY OR POWER SERIES.
!
  610 CONTINUE
!
!  COMPUTATION OF LAST ORDER FOR SERIES NORMALIZATION
!
  AKM = MAX(3.0D0-FN,0.0D0)
  KM = INT(AKM)
  TFN = FN + KM
  TA = (GLN+TFN-0.9189385332D0-0.0833333333D0/TFN)/(TFN+0.5D0)
  TA = XO2L - TA
  TB = -(1.0D0-1.5D0/TFN)/TFN
  AKM = TOLLN/(-TA+SQRT(TA*TA-TOLLN*TB)) + 1.5D0
  IN = KM + INT(AKM)
  go to 660
  620 CONTINUE
!
!  COMPUTATION OF LAST ORDER FOR ASYMPTOTIC EXPANSION NORMALIZATION
!
  GLN = WK(3) + WK(2)
  if (WK(6) > 30.0D0) go to 640
  RDEN = (PP(4)*WK(6)+PP(3))*WK(6) + 1.0D0
  RZDEN = PP(1) + PP(2)*WK(6)
  TA = RZDEN/RDEN
  if (WK(1) < 0.10D0) go to 630
  TB = GLN/WK(5)
  go to 650
  630 TB=(1.259921049D0+(0.1679894730D0+0.0887944358D0*WK(1))*WK(1)) &
   /WK(7)
  go to 650
  640 CONTINUE
  TA = 0.5D0*TOLLN/WK(4)
  TA=((0.0493827160D0*TA-0.1111111111D0)*TA+0.6666666667D0)*TA*WK(6)
  if (WK(1) < 0.10D0) go to 630
  TB = GLN/WK(5)
  650 IN = INT(TA/TB+1.5D0)
  if (IN > INLIM) go to 310
  660 CONTINUE
  DTM = FNI + IN
  TRX = 2.0D0/X
  TM = (DTM+FNF)*TRX
  TA = 0.0D0
  TB = TOL
  KK = 1
  AK=1.0D0
  670 CONTINUE
!
!  BACKWARD RECUR UNINDEXED
!
  DO I=1,IN
    S = TB
    TB = TM*TB - TA
    TA = S
    DTM = DTM - 1.0D0
    TM = (DTM+FNF)*TRX
  end do
!
!  NORMALIZATION
!
  if (KK /= 1) go to 690
  S=TEMP(3)
  SA=TA/TB
  TA=S
  TB=S
  if ( ABS(S) > SLIM) go to 685
  TA=TA*RTOL
  TB=TB*RTOL
  AK=TOL
  685 CONTINUE
  TA=TA*SA
  KK = 2
  IN = NS
  if (NS /= 0) go to 670
  690 Y(NN) = TB*AK
  NZ = N - NN
  if (NN == 1) RETURN
  K = NN - 1
  S=TB
  TB = TM*TB - TA
  TA=S
  Y(K)=TB*AK
  if (NN == 2) RETURN
  DTM = DTM - 1.0D0
  TM = (DTM+FNF)*TRX
  K=NN-2
!
!  BACKWARD RECUR INDEXED
!
  DO I=3,NN
    S=TB
    TB = TM*TB - TA
    TA=S
    Y(K)=TB*AK
    DTM = DTM - 1.0D0
    TM = (DTM+FNF)*TRX
    K = K - 1
  end do

  return

  720 CONTINUE
  call XERMSG ('SLATEC', 'DBESJ', 'N LESS THAN ONE.', 2, 1)
  return
  730 CONTINUE
  call XERMSG ('SLATEC', 'DBESJ', 'X LESS THAN ZERO.', 2, 1)
  return
end
function d1mach ( i )

!*****************************************************************************80
!
!! D1MACH returns double precision real machine constants.
!
!  Discussion:
!
!    Assuming that the internal representation of a double precision real
!    number is in base B, with T the number of base-B digits in the mantissa,
!    and EMIN the smallest possible exponent and EMAX the largest possible 
!    exponent, then
!
!      D1MACH(1) = B^(EMIN-1), the smallest positive magnitude.
!      D1MACH(2) = B^EMAX*(1-B^(-T)), the largest magnitude.
!      D1MACH(3) = B^(-T), the smallest relative spacing.
!      D1MACH(4) = B^(1-T), the largest relative spacing.
!      D1MACH(5) = log10(B).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by Phyllis Fox, Andrew Hall, Norman Schryer.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Phyllis Fox, Andrew Hall, Norman Schryer,
!    Algorithm 528:
!    Framework for a Portable Library,
!    ACM Transactions on Mathematical Software,
!    Volume 4, Number 2, June 1978, page 176-188.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, chooses the parameter to be returned.
!    1 <= I <= 5.
!
!    Output, real ( kind = 8 ) D1MACH, the value of the chosen parameter.
!
  implicit none

  real ( kind = 8 ) d1mach
  integer ( kind = 4 ) i

  if ( i < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'D1MACH - Fatal error!'
    write ( *, '(a)' ) '  The input argument I is out of bounds.'
    write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 5.'
    write ( *, '(a,i12)' ) '  I = ', i
    d1mach = 0.0D+00
    stop
  else if ( i == 1 ) then
    d1mach = 4.450147717014403D-308
  else if ( i == 2 ) then
    d1mach = 8.988465674311579D+307
  else if ( i == 3 ) then
    d1mach = 1.110223024625157D-016
  else if ( i == 4 ) then
    d1mach = 2.220446049250313D-016
  else if ( i == 5 ) then
    d1mach = 0.301029995663981D+000
  else if ( 5 < i ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'D1MACH - Fatal error!'
    write ( *, '(a)' ) '  The input argument I is out of bounds.'
    write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 5.'
    write ( *, '(a,i12)' ) '  I = ', i
    d1mach = 0.0D+00
    stop
  end if

  return
end
function d9lgmc ( x )

!*****************************************************************************80
!
!! D9LGMC computes the log Gamma correction factor.
!
!  Discussion:
!
!    The correction factor is
!      LOG(DGAMMA(X)) = LOG(SQRT(2*PI)) + (X-5.)*LOG(X) - X + D9LGMC(X).
!
!  Author:
!
!    Wayne Fullerton
!
!***DESCRIPTION
!
! Compute the log gamma correction factor for X  >=  10. so that
! LOG (DGAMMA(X)) = LOG(SQRT(2*PI)) + (X-.5)*LOG(X) - X + D9lGMC(X)
!
! Series for ALGM       on the interval  0.          to  1.00000E-02
!                                        with weighted error   1.28E-31
!                                         log weighted error  30.89
!                               significant figures required  29.81
!                                    decimal places required  31.48
!
!
  implicit none

  real ( kind = 8 ) d9lgmc
  logical ( kind = 4 ) first
  integer ( kind = 4 ) initds
  integer ( kind = 4 ) nalgm

  real ( kind = 8 ) X, ALGMCS(15), XBIG, XMAX, DCSEVL, D1MACH

  SAVE ALGMCS, NALGM, XBIG, XMAX, FIRST
  DATA ALGMCS(  1) / +.1666389480451863247205729650822D+00     /
  DATA ALGMCS(  2) / -.1384948176067563840732986059135D-04     /
  DATA ALGMCS(  3) / +.9810825646924729426157171547487D-08     /
  DATA ALGMCS(  4) / -.1809129475572494194263306266719D-10     /
  DATA ALGMCS(  5) / +.6221098041892605227126015543416D-13     /
  DATA ALGMCS(  6) / -.3399615005417721944303330599666D-15     /
  DATA ALGMCS(  7) / +.2683181998482698748957538846666D-17     /
  DATA ALGMCS(  8) / -.2868042435334643284144622399999D-19     /
  DATA ALGMCS(  9) / +.3962837061046434803679306666666D-21     /
  DATA ALGMCS( 10) / -.6831888753985766870111999999999D-23     /
  DATA ALGMCS( 11) / +.1429227355942498147573333333333D-24     /
  DATA ALGMCS( 12) / -.3547598158101070547199999999999D-26     /
  DATA ALGMCS( 13) / +.1025680058010470912000000000000D-27     /
  DATA ALGMCS( 14) / -.3401102254316748799999999999999D-29     /
  DATA ALGMCS( 15) / +.1276642195630062933333333333333D-30     /
  DATA FIRST /.TRUE./

  if ( FIRST ) THEN
    NALGM = INITDS ( ALGMCS, 15, D1MACH(3) )
    XBIG = 1.0D0 / SQRT ( D1MACH(3) )
    XMAX = EXP ( MIN ( LOG ( D1MACH ( 2 ) / 12.D0 ), &
      - LOG ( 12.D0 * D1MACH(1) ) ) )
  end if

  FIRST = .FALSE.

  if (X  <  10.D0) call XERMSG ('SLATEC', 'D9LGMC', &
     'X MUST BE GE 10', 1, 2)
  if (X >= XMAX) go to 20

  D9LGMC = 1.D0/(12.D0*X)

  if (X < XBIG) then
    D9LGMC = DCSEVL (2.0D0*(10.D0/X)**2-1.D0, ALGMCS, NALGM) / X
  end if

  return

 20   D9LGMC = 0.D0
  call XERMSG ('SLATEC', 'D9LGMC', 'X SO BIG D9LGMC UNDERFLOWS', 2, 1)

  return
end
subroutine dasyjy ( funjy, x, fnu, flgjy, in, y, wk, iflw )

!*****************************************************************************80
!
!! DASYJY computes Bessel J and Y functions of large order.
!
!  Author:
!
!    Donald Amos
!
!***DESCRIPTION
!
!                 DASYJY computes Bessel functions J and Y
!               for arguments X > 0.0 and orders FNU  >=  35.0
!               on FLGJY = 1 and FLGJY = -1 respectively
!
!                                  INPUT
!
!      FUNJY - External subroutine JAIRY or YAIRY
!          X - Argument, X > 0.0D0
!        FNU - Order of the first Bessel function
!      FLGJY - Selection flag
!              FLGJY =  1.0D0 gives the J function
!              FLGJY = -1.0D0 gives the Y function
!         IN - Number of functions desired, IN = 1 or 2
!
!                                  OUTPUT
!
!         Y  - A vector whose first IN components contain the sequence
!       IFLW - A flag indicating underflow or overflow
!                    return variables for BESJ only
!      WK(1) = 1 - (X/FNU)**2 = W**2
!      WK(2) = SQRT(ABS(WK(1)))
!      WK(3) = ABS(WK(2) - ATAN(WK(2)))  or
!              ABS(LN((1 + WK(2))/(X/FNU)) - WK(2))
!            = ABS((2/3)*ZETA**(3/2))
!      WK(4) = FNU*WK(3)
!      WK(5) = (1.5*WK(3)*FNU)**(1/3) = SQRT(ZETA)*FNU**(1/3)
!      WK(6) = SIGN(1.,W**2)*WK(5)**2 = SIGN(1.,W**2)*ZETA*FNU**(2/3)
!      WK(7) = FNU**(1/3)
!
!     Abstract   **** A Double Precision Routine ****
!         DASYJY implements the uniform asymptotic expansion of
!         the J and Y Bessel functions for FNU >= 35 and real
!         X > 0.0D0. The forms are identical except for a change
!         in sign of some of the terms. This change in sign is
!         accomplished by means of the flag FLGJY = 1 or -1. On
!         FLGJY = 1 the Airy functions AI(X) and DAI(X) are
!         supplied by the external function JAIRY, and on
!         FLGJY = -1 the Airy functions BI(X) and DBI(X) are
!         supplied by the external function YAIRY.
!
  implicit none

  real ( kind = 8 ) abw2
  real ( kind = 8 ) d1mach
  integer ( kind = 4 ) I, IFLW, IN, J, JN,JR,JU,K, KB,KLAST,KMAX,KP1, KS, &
   KSP1, KSTEMP, L, LR, LRP1, ISETA, ISETB
  integer ( kind = 4 ) I1MACH
  real ( kind = 8 ) AKM, ALFA, ALFA1, ALFA2, AP, AR, ASUM, AZ, &
   BETA, BETA1, BETA2, BETA3, BR, BSUM, C, CON1, CON2, &
   CON548,CR,CRZ32, DFI,ELIM, DR,FI, FLGJY, FN, FNU, &
   FN2, GAMA, PHI,  RCZ, RDEN, RELB, RFN2,  RTZ, RZDEN, &
   SA, SB, SUMA, SUMB, S1, TA, TAU, TB, TFN, TOL, TOLS, T2, UPOL, &
    WK, X, XX, Y, Z, Z32
 
  DIMENSION Y(*), WK(*), C(65)
  DIMENSION ALFA(26,4), BETA(26,5)
  DIMENSION ALFA1(26,2), ALFA2(26,2)
  DIMENSION BETA1(26,2), BETA2(26,2), BETA3(26,1)
  DIMENSION GAMA(26), KMAX(5), AR(8), BR(10), UPOL(10)
  DIMENSION CR(10), DR(10)
  EQUIVALENCE (ALFA(1,1),ALFA1(1,1))
  EQUIVALENCE (ALFA(1,3),ALFA2(1,1))
  EQUIVALENCE (BETA(1,1),BETA1(1,1))
  EQUIVALENCE (BETA(1,3),BETA2(1,1))
  EQUIVALENCE (BETA(1,5),BETA3(1,1))
  SAVE TOLS, CON1, CON2, CON548, AR, BR, C, &
   ALFA1, ALFA2, BETA1, BETA2, BETA3, GAMA
  DATA TOLS            /-6.90775527898214D+00/
  DATA CON1,CON2,CON548/ &
   6.66666666666667D-01, 3.33333333333333D-01, 1.04166666666667D-01/
  DATA  AR(1),  AR(2),  AR(3),  AR(4),  AR(5),  AR(6),  AR(7), &
        AR(8)          / 8.35503472222222D-02, 1.28226574556327D-01, &
   2.91849026464140D-01, 8.81627267443758D-01, 3.32140828186277D+00, &
   1.49957629868626D+01, 7.89230130115865D+01, 4.74451538868264D+02/
  DATA  BR(1), BR(2), BR(3), BR(4), BR(5), BR(6), BR(7), BR(8), &
        BR(9), BR(10)  /-1.45833333333333D-01,-9.87413194444444D-02, &
  -1.43312053915895D-01,-3.17227202678414D-01,-9.42429147957120D-01, &
  -3.51120304082635D+00,-1.57272636203680D+01,-8.22814390971859D+01, &
  -4.92355370523671D+02,-3.31621856854797D+03/
  DATA C(1), C(2), C(3), C(4), C(5), C(6), C(7), C(8), C(9), C(10), &
       C(11), C(12), C(13), C(14), C(15), C(16), C(17), C(18), &
       C(19), C(20), C(21), C(22), C(23), C(24)/ &
         -2.08333333333333D-01,        1.25000000000000D-01, &
          3.34201388888889D-01,       -4.01041666666667D-01, &
          7.03125000000000D-02,       -1.02581259645062D+00, &
          1.84646267361111D+00,       -8.91210937500000D-01, &
          7.32421875000000D-02,        4.66958442342625D+00, &
         -1.12070026162230D+01,        8.78912353515625D+00, &
         -2.36408691406250D+00,        1.12152099609375D-01, &
         -2.82120725582002D+01,        8.46362176746007D+01, &
         -9.18182415432400D+01,        4.25349987453885D+01, &
         -7.36879435947963D+00,        2.27108001708984D-01, &
          2.12570130039217D+02,       -7.65252468141182D+02, &
          1.05999045252800D+03,       -6.99579627376133D+02/
  DATA C(25), C(26), C(27), C(28), C(29), C(30), C(31), C(32), &
       C(33), C(34), C(35), C(36), C(37), C(38), C(39), C(40), &
       C(41), C(42), C(43), C(44), C(45), C(46), C(47), C(48)/ &
          2.18190511744212D+02,       -2.64914304869516D+01, &
          5.72501420974731D-01,       -1.91945766231841D+03, &
          8.06172218173731D+03,       -1.35865500064341D+04, &
          1.16553933368645D+04,       -5.30564697861340D+03, &
          1.20090291321635D+03,       -1.08090919788395D+02, &
          1.72772750258446D+00,        2.02042913309661D+04, &
         -9.69805983886375D+04,        1.92547001232532D+05, &
         -2.03400177280416D+05,        1.22200464983017D+05, &
         -4.11926549688976D+04,        7.10951430248936D+03, &
         -4.93915304773088D+02,        6.07404200127348D+00, &
         -2.42919187900551D+05,        1.31176361466298D+06, &
         -2.99801591853811D+06,        3.76327129765640D+06/
  DATA C(49), C(50), C(51), C(52), C(53), C(54), C(55), C(56), &
       C(57), C(58), C(59), C(60), C(61), C(62), C(63), C(64), &
       C(65)/ &
         -2.81356322658653D+06,        1.26836527332162D+06, &
         -3.31645172484564D+05,        4.52187689813627D+04, &
         -2.49983048181121D+03,        2.43805296995561D+01, &
          3.28446985307204D+06,       -1.97068191184322D+07, &
          5.09526024926646D+07,       -7.41051482115327D+07, &
          6.63445122747290D+07,       -3.75671766607634D+07, &
          1.32887671664218D+07,       -2.78561812808645D+06, &
          3.08186404612662D+05,       -1.38860897537170D+04, &
          1.10017140269247D+02/
  DATA ALFA1(1,1), ALFA1(2,1), ALFA1(3,1), ALFA1(4,1), ALFA1(5,1), &
       ALFA1(6,1), ALFA1(7,1), ALFA1(8,1), ALFA1(9,1), ALFA1(10,1), &
       ALFA1(11,1),ALFA1(12,1),ALFA1(13,1),ALFA1(14,1),ALFA1(15,1), &
       ALFA1(16,1),ALFA1(17,1),ALFA1(18,1),ALFA1(19,1),ALFA1(20,1), &
       ALFA1(21,1),ALFA1(22,1),ALFA1(23,1),ALFA1(24,1),ALFA1(25,1), &
       ALFA1(26,1)     /-4.44444444444444D-03,-9.22077922077922D-04, &
  -8.84892884892885D-05, 1.65927687832450D-04, 2.46691372741793D-04, &
   2.65995589346255D-04, 2.61824297061501D-04, 2.48730437344656D-04, &
   2.32721040083232D-04, 2.16362485712365D-04, 2.00738858762752D-04, &
   1.86267636637545D-04, 1.73060775917876D-04, 1.61091705929016D-04, &
   1.50274774160908D-04, 1.40503497391270D-04, 1.31668816545923D-04, &
   1.23667445598253D-04, 1.16405271474738D-04, 1.09798298372713D-04, &
   1.03772410422993D-04, 9.82626078369363D-05, 9.32120517249503D-05, &
   8.85710852478712D-05, 8.42963105715700D-05, 8.03497548407791D-05/
  DATA ALFA1(1,2), ALFA1(2,2), ALFA1(3,2), ALFA1(4,2), ALFA1(5,2), &
       ALFA1(6,2), ALFA1(7,2), ALFA1(8,2), ALFA1(9,2), ALFA1(10,2), &
       ALFA1(11,2),ALFA1(12,2),ALFA1(13,2),ALFA1(14,2),ALFA1(15,2), &
       ALFA1(16,2),ALFA1(17,2),ALFA1(18,2),ALFA1(19,2),ALFA1(20,2), &
       ALFA1(21,2),ALFA1(22,2),ALFA1(23,2),ALFA1(24,2),ALFA1(25,2), &
       ALFA1(26,2)     / 6.93735541354589D-04, 2.32241745182922D-04, &
  -1.41986273556691D-05,-1.16444931672049D-04,-1.50803558053049D-04, &
  -1.55121924918096D-04,-1.46809756646466D-04,-1.33815503867491D-04, &
  -1.19744975684254D-04,-1.06184319207974D-04,-9.37699549891194D-05, &
  -8.26923045588193D-05,-7.29374348155221D-05,-6.44042357721016D-05, &
  -5.69611566009369D-05,-5.04731044303562D-05,-4.48134868008883D-05, &
  -3.98688727717599D-05,-3.55400532972042D-05,-3.17414256609022D-05, &
  -2.83996793904175D-05,-2.54522720634871D-05,-2.28459297164725D-05, &
  -2.05352753106481D-05,-1.84816217627666D-05,-1.66519330021394D-05/
  DATA ALFA2(1,1), ALFA2(2,1), ALFA2(3,1), ALFA2(4,1), ALFA2(5,1), &
       ALFA2(6,1), ALFA2(7,1), ALFA2(8,1), ALFA2(9,1), ALFA2(10,1), &
       ALFA2(11,1),ALFA2(12,1),ALFA2(13,1),ALFA2(14,1),ALFA2(15,1), &
       ALFA2(16,1),ALFA2(17,1),ALFA2(18,1),ALFA2(19,1),ALFA2(20,1), &
       ALFA2(21,1),ALFA2(22,1),ALFA2(23,1),ALFA2(24,1),ALFA2(25,1), &
       ALFA2(26,1)     /-3.54211971457744D-04,-1.56161263945159D-04, &
   3.04465503594936D-05, 1.30198655773243D-04, 1.67471106699712D-04, &
   1.70222587683593D-04, 1.56501427608595D-04, 1.36339170977445D-04, &
   1.14886692029825D-04, 9.45869093034688D-05, 7.64498419250898D-05, &
   6.07570334965197D-05, 4.74394299290509D-05, 3.62757512005344D-05, &
   2.69939714979225D-05, 1.93210938247939D-05, 1.30056674793963D-05, &
   7.82620866744497D-06, 3.59257485819352D-06, 1.44040049814252D-07, &
  -2.65396769697939D-06,-4.91346867098486D-06,-6.72739296091248D-06, &
  -8.17269379678658D-06,-9.31304715093561D-06,-1.02011418798016D-05/
  DATA ALFA2(1,2), ALFA2(2,2), ALFA2(3,2), ALFA2(4,2), ALFA2(5,2), &
       ALFA2(6,2), ALFA2(7,2), ALFA2(8,2), ALFA2(9,2), ALFA2(10,2), &
       ALFA2(11,2),ALFA2(12,2),ALFA2(13,2),ALFA2(14,2),ALFA2(15,2), &
       ALFA2(16,2),ALFA2(17,2),ALFA2(18,2),ALFA2(19,2),ALFA2(20,2), &
       ALFA2(21,2),ALFA2(22,2),ALFA2(23,2),ALFA2(24,2),ALFA2(25,2), &
       ALFA2(26,2)     / 3.78194199201773D-04, 2.02471952761816D-04, &
  -6.37938506318862D-05,-2.38598230603006D-04,-3.10916256027362D-04, &
  -3.13680115247576D-04,-2.78950273791323D-04,-2.28564082619141D-04, &
  -1.75245280340847D-04,-1.25544063060690D-04,-8.22982872820208D-05, &
  -4.62860730588116D-05,-1.72334302366962D-05, 5.60690482304602D-06, &
   2.31395443148287D-05, 3.62642745856794D-05, 4.58006124490189D-05, &
   5.24595294959114D-05, 5.68396208545815D-05, 5.94349820393104D-05, &
   6.06478527578422D-05, 6.08023907788436D-05, 6.01577894539460D-05, &
   5.89199657344698D-05, 5.72515823777593D-05, 5.52804375585853D-05/
  DATA BETA1(1,1), BETA1(2,1), BETA1(3,1), BETA1(4,1), BETA1(5,1), &
       BETA1(6,1), BETA1(7,1), BETA1(8,1), BETA1(9,1), BETA1(10,1), &
       BETA1(11,1),BETA1(12,1),BETA1(13,1),BETA1(14,1),BETA1(15,1), &
       BETA1(16,1),BETA1(17,1),BETA1(18,1),BETA1(19,1),BETA1(20,1), &
       BETA1(21,1),BETA1(22,1),BETA1(23,1),BETA1(24,1),BETA1(25,1), &
       BETA1(26,1)     / 1.79988721413553D-02, 5.59964911064388D-03, &
   2.88501402231133D-03, 1.80096606761054D-03, 1.24753110589199D-03, &
   9.22878876572938D-04, 7.14430421727287D-04, 5.71787281789705D-04, &
   4.69431007606482D-04, 3.93232835462917D-04, 3.34818889318298D-04, &
   2.88952148495752D-04, 2.52211615549573D-04, 2.22280580798883D-04, &
   1.97541838033063D-04, 1.76836855019718D-04, 1.59316899661821D-04, &
   1.44347930197334D-04, 1.31448068119965D-04, 1.20245444949303D-04, &
   1.10449144504599D-04, 1.01828770740567D-04, 9.41998224204238D-05, &
   8.74130545753834D-05, 8.13466262162801D-05, 7.59002269646219D-05/
  DATA BETA1(1,2), BETA1(2,2), BETA1(3,2), BETA1(4,2), BETA1(5,2), &
       BETA1(6,2), BETA1(7,2), BETA1(8,2), BETA1(9,2), BETA1(10,2), &
       BETA1(11,2),BETA1(12,2),BETA1(13,2),BETA1(14,2),BETA1(15,2), &
       BETA1(16,2),BETA1(17,2),BETA1(18,2),BETA1(19,2),BETA1(20,2), &
       BETA1(21,2),BETA1(22,2),BETA1(23,2),BETA1(24,2),BETA1(25,2), &
       BETA1(26,2)     /-1.49282953213429D-03,-8.78204709546389D-04, &
  -5.02916549572035D-04,-2.94822138512746D-04,-1.75463996970783D-04, &
  -1.04008550460816D-04,-5.96141953046458D-05,-3.12038929076098D-05, &
  -1.26089735980230D-05,-2.42892608575730D-07, 8.05996165414274D-06, &
   1.36507009262147D-05, 1.73964125472926D-05, 1.98672978842134D-05, &
   2.14463263790823D-05, 2.23954659232457D-05, 2.28967783814713D-05, &
   2.30785389811178D-05, 2.30321976080909D-05, 2.28236073720349D-05, &
   2.25005881105292D-05, 2.20981015361991D-05, 2.16418427448104D-05, &
   2.11507649256221D-05, 2.06388749782171D-05, 2.01165241997082D-05/
  DATA BETA2(1,1), BETA2(2,1), BETA2(3,1), BETA2(4,1), BETA2(5,1), &
       BETA2(6,1), BETA2(7,1), BETA2(8,1), BETA2(9,1), BETA2(10,1), &
       BETA2(11,1),BETA2(12,1),BETA2(13,1),BETA2(14,1),BETA2(15,1), &
       BETA2(16,1),BETA2(17,1),BETA2(18,1),BETA2(19,1),BETA2(20,1), &
       BETA2(21,1),BETA2(22,1),BETA2(23,1),BETA2(24,1),BETA2(25,1), &
       BETA2(26,1)     / 5.52213076721293D-04, 4.47932581552385D-04, &
   2.79520653992021D-04, 1.52468156198447D-04, 6.93271105657044D-05, &
   1.76258683069991D-05,-1.35744996343269D-05,-3.17972413350427D-05, &
  -4.18861861696693D-05,-4.69004889379141D-05,-4.87665447413787D-05, &
  -4.87010031186735D-05,-4.74755620890087D-05,-4.55813058138628D-05, &
  -4.33309644511266D-05,-4.09230193157750D-05,-3.84822638603221D-05, &
  -3.60857167535411D-05,-3.37793306123367D-05,-3.15888560772110D-05, &
  -2.95269561750807D-05,-2.75978914828336D-05,-2.58006174666884D-05, &
  -2.41308356761280D-05,-2.25823509518346D-05,-2.11479656768913D-05/
  DATA BETA2(1,2), BETA2(2,2), BETA2(3,2), BETA2(4,2), BETA2(5,2), &
       BETA2(6,2), BETA2(7,2), BETA2(8,2), BETA2(9,2), BETA2(10,2), &
       BETA2(11,2),BETA2(12,2),BETA2(13,2),BETA2(14,2),BETA2(15,2), &
       BETA2(16,2),BETA2(17,2),BETA2(18,2),BETA2(19,2),BETA2(20,2), &
       BETA2(21,2),BETA2(22,2),BETA2(23,2),BETA2(24,2),BETA2(25,2), &
       BETA2(26,2)     /-4.74617796559960D-04,-4.77864567147321D-04, &
  -3.20390228067038D-04,-1.61105016119962D-04,-4.25778101285435D-05, &
   3.44571294294968D-05, 7.97092684075675D-05, 1.03138236708272D-04, &
   1.12466775262204D-04, 1.13103642108481D-04, 1.08651634848774D-04, &
   1.01437951597662D-04, 9.29298396593364D-05, 8.40293133016090D-05, &
   7.52727991349134D-05, 6.69632521975731D-05, 5.92564547323195D-05, &
   5.22169308826976D-05, 4.58539485165361D-05, 4.01445513891487D-05, &
   3.50481730031328D-05, 3.05157995034347D-05, 2.64956119950516D-05, &
   2.29363633690998D-05, 1.97893056664022D-05, 1.70091984636413D-05/
  DATA BETA3(1,1), BETA3(2,1), BETA3(3,1), BETA3(4,1), BETA3(5,1), &
       BETA3(6,1), BETA3(7,1), BETA3(8,1), BETA3(9,1), BETA3(10,1), &
       BETA3(11,1),BETA3(12,1),BETA3(13,1),BETA3(14,1),BETA3(15,1), &
       BETA3(16,1),BETA3(17,1),BETA3(18,1),BETA3(19,1),BETA3(20,1), &
       BETA3(21,1),BETA3(22,1),BETA3(23,1),BETA3(24,1),BETA3(25,1), &
       BETA3(26,1)     / 7.36465810572578D-04, 8.72790805146194D-04, &
   6.22614862573135D-04, 2.85998154194304D-04, 3.84737672879366D-06, &
  -1.87906003636972D-04,-2.97603646594555D-04,-3.45998126832656D-04, &
  -3.53382470916038D-04,-3.35715635775049D-04,-3.04321124789040D-04, &
  -2.66722723047613D-04,-2.27654214122820D-04,-1.89922611854562D-04, &
  -1.55058918599094D-04,-1.23778240761874D-04,-9.62926147717644D-05, &
  -7.25178327714425D-05,-5.22070028895634D-05,-3.50347750511901D-05, &
  -2.06489761035552D-05,-8.70106096849767D-06, 1.13698686675100D-06, &
   9.16426474122779D-06, 1.56477785428873D-05, 2.08223629482467D-05/
  DATA GAMA(1),   GAMA(2),   GAMA(3),   GAMA(4),   GAMA(5), &
       GAMA(6),   GAMA(7),   GAMA(8),   GAMA(9),   GAMA(10), &
       GAMA(11),  GAMA(12),  GAMA(13),  GAMA(14),  GAMA(15), &
       GAMA(16),  GAMA(17),  GAMA(18),  GAMA(19),  GAMA(20), &
       GAMA(21),  GAMA(22),  GAMA(23),  GAMA(24),  GAMA(25), &
       GAMA(26)        / 6.29960524947437D-01, 2.51984209978975D-01, &
   1.54790300415656D-01, 1.10713062416159D-01, 8.57309395527395D-02, &
   6.97161316958684D-02, 5.86085671893714D-02, 5.04698873536311D-02, &
   4.42600580689155D-02, 3.93720661543510D-02, 3.54283195924455D-02, &
   3.21818857502098D-02, 2.94646240791158D-02, 2.71581677112934D-02, &
   2.51768272973862D-02, 2.34570755306079D-02, 2.19508390134907D-02, &
   2.06210828235646D-02, 1.94388240897881D-02, 1.83810633800683D-02, &
   1.74293213231963D-02, 1.65685837786612D-02, 1.57865285987918D-02, &
   1.50729501494096D-02, 1.44193250839955D-02, 1.38184805735342D-02/
!***FIRST EXECUTABLE STATEMENT  DASYJY
  TA = D1MACH(3)
  TOL = MAX(TA,1.0D-15)
  TB = D1MACH(5)
  JU = I1MACH(15)
  if ( FLGJY == 1.0D0) go to 6
  JR = I1MACH(14)
  ELIM = -2.303D0*TB*(JU+JR)
  go to 7
    6 CONTINUE
  ELIM = -2.303D0*(TB*JU+3.0D0)
    7 CONTINUE
  FN = FNU
  IFLW = 0

  DO 170 JN=1,IN

    XX = X/FN
    WK(1) = 1.0D0 - XX*XX
    ABW2 = ABS(WK(1))
    WK(2) = SQRT(ABW2)
    WK(7) = FN**CON2
    if (ABW2 > 0.27750D0) go to 80
!
!  ASYMPTOTIC EXPANSION
!  CASES NEAR X=FN, ABS(1.-(X/FN)**2) <= 0.2775
!  COEFFICIENTS OF ASYMPTOTIC EXPANSION BY SERIES
!
!  ZETA AND TRUNCATION FOR A(ZETA) AND B(ZETA) SERIES
!
!  KMAX IS TRUNCATION INDEX FOR A(ZETA) AND B(ZETA) SERIES=MAX(2,SA)
!
    SA = 0.0D0
    if (ABW2 == 0.0D0) go to 10
    SA = TOLS/LOG(ABW2)
   10   SB = SA
    DO I=1,5
      AKM = MAX(SA,2.0D0)
      KMAX(I) = INT(AKM)
      SA = SA + SB
    end do
    KB = KMAX(5)
    KLAST = KB - 1
    SA = GAMA(KB)
    DO K=1,KLAST
      KB = KB - 1
      SA = SA*WK(1) + GAMA(KB)
    end do
    Z = WK(1)*SA
    AZ = ABS(Z)
    RTZ = SQRT(AZ)
    WK(3) = CON1*AZ*RTZ
    WK(4) = WK(3)*FN
    WK(5) = RTZ*WK(7)
    WK(6) = -WK(5)*WK(5)
    if ( Z <= 0.0D0) go to 35
    if ( WK(4) > ELIM) go to 75
    WK(6) = -WK(6)
   35   CONTINUE
    PHI = SQRT(SQRT(SA+SA+SA+SA))
!
!  B(ZETA) FOR S=0
!
    KB = KMAX(5)
    KLAST = KB - 1
    SB = BETA(KB,1)
    DO K=1,KLAST
      KB = KB - 1
      SB = SB*WK(1) + BETA(KB,1)
    end do
    KSP1 = 1
    FN2 = FN*FN
    RFN2 = 1.0D0/FN2
    RDEN = 1.0D0
    ASUM = 1.0D0
    RELB = TOL*ABS(SB)
    BSUM = SB
    DO KS=1,4
      KSP1 = KSP1 + 1
      RDEN = RDEN*RFN2
!
!  A(ZETA) AND B(ZETA) FOR S=1,2,3,4
!
      KSTEMP = 5 - KS
      KB = KMAX(KSTEMP)
      KLAST = KB - 1
      SA = ALFA(KB,KS)
      SB = BETA(KB,KSP1)
      DO K=1,KLAST
        KB = KB - 1
        SA = SA*WK(1) + ALFA(KB,KS)
        SB = SB*WK(1) + BETA(KB,KSP1)
      end do
      TA = SA*RDEN
      TB = SB*RDEN
      ASUM = ASUM + TA
      BSUM = BSUM + TB
      if (ABS(TA) <= TOL .AND. ABS(TB) <= RELB) go to 70
    end do
   70   CONTINUE
    BSUM = BSUM/(FN*WK(7))
    go to 160

   75   CONTINUE
    IFLW = 1
    return

   80   CONTINUE
    UPOL(1) = 1.0D0
    TAU = 1.0D0/WK(2)
    T2 = 1.0D0/WK(1)
    if (WK(1) >= 0.0D0) go to 90
!
!  CASES FOR (X/FN) > SQRT(1.2775)
!
    WK(3) = ABS(WK(2)-ATAN(WK(2)))
    WK(4) = WK(3)*FN
    RCZ = -CON1/WK(4)
    Z32 = 1.5D0*WK(3)
    RTZ = Z32**CON2
    WK(5) = RTZ*WK(7)
    WK(6) = -WK(5)*WK(5)
    go to 100
   90   CONTINUE
!
!  CASES FOR (X/FN) < SQRT(0.7225)
!
    WK(3) = ABS(LOG((1.0D0+WK(2))/XX)-WK(2))
    WK(4) = WK(3)*FN
    RCZ = CON1/WK(4)
    if ( WK(4) > ELIM) go to 75
    Z32 = 1.5D0*WK(3)
    RTZ = Z32**CON2
    WK(7) = FN**CON2
    WK(5) = RTZ*WK(7)
    WK(6) = WK(5)*WK(5)
  100   CONTINUE
    PHI = SQRT((RTZ+RTZ)*TAU)
    TB = 1.0D0
    ASUM = 1.0D0
    TFN = TAU/FN
    RDEN=1.0D0/FN
    RFN2=RDEN*RDEN
    RDEN=1.0D0
    UPOL(2) = (C(1)*T2+C(2))*TFN
    CRZ32 = CON548*RCZ
    BSUM = UPOL(2) + CRZ32
    RELB = TOL*ABS(BSUM)
    AP = TFN
    KS = 0
    KP1 = 2
    RZDEN = RCZ
    L = 2
    ISETA=0
    ISETB=0
    DO 140 LR=2,8,2
!
!  COMPUTE TWO U POLYNOMIALS FOR NEXT A(ZETA) AND B(ZETA)
!
      LRP1 = LR + 1
      DO K=LR,LRP1
        KS = KS + 1
        KP1 = KP1 + 1
        L = L + 1
        S1 = C(L)
        DO J=2,KP1
          L = L + 1
          S1 = S1*T2 + C(L)
        end do
        AP = AP*TFN
        UPOL(KP1) = AP*S1
        CR(KS) = BR(KS)*RZDEN
        RZDEN = RZDEN*RCZ
        DR(KS) = AR(KS)*RZDEN
      end do

      SUMA = UPOL(LRP1)
      SUMB = UPOL(LR+2) + UPOL(LRP1)*CRZ32
      JU = LRP1
      DO JR=1,LR
        JU = JU - 1
        SUMA = SUMA + CR(JR)*UPOL(JU)
        SUMB = SUMB + DR(JR)*UPOL(JU)
      end do
      RDEN=RDEN*RFN2
      TB = -TB
      if (WK(1) > 0.0D0) TB = ABS(TB)
      if ( RDEN < TOL) go to 131
      ASUM = ASUM + SUMA*TB
      BSUM = BSUM + SUMB*TB
      go to 140
  131     if ( ISETA == 1) go to 132
      if ( ABS(SUMA) < TOL) ISETA=1
      ASUM=ASUM+SUMA*TB
  132     if ( ISETB == 1) go to 133
      if ( ABS(SUMB) < RELB) ISETB=1
      BSUM=BSUM+SUMB*TB
  133     if ( ISETA == 1 .AND. ISETB == 1) go to 150
  140   CONTINUE
  150   TB = WK(5)
    if (WK(1) > 0.0D0) TB = -TB
    BSUM = BSUM/TB

  160   CONTINUE
    call FUNJY(WK(6), WK(5), WK(4), FI, DFI)
    TA=1.0D0/TOL
    TB=D1MACH(1)*TA*1.0D+3
    if ( ABS(FI) > TB) go to 165
    FI=FI*TA
    DFI=DFI*TA
    PHI=PHI*TOL
  165   CONTINUE
    Y(JN) = FLGJY*PHI*(FI*ASUM+DFI*BSUM)/WK(7)
    FN = FN - FLGJY
  170 CONTINUE

  return
end
function dcsevl ( x, cs, n )

!*****************************************************************************80
!
!! DCSEVL evaluates a Chebyshev series.
!
!  Author:
!
!    Wayne Fullerton
!
!***DESCRIPTION
!
!  Evaluate the N-term Chebyshev series CS at X.  Adapted from
!  a method presented in the paper by Broucke referenced below.
!
!       Input Arguments --
!  X    value at which the series is to be evaluated.
!  CS   array of N terms of a Chebyshev series.  In evaluating
!       CS, only half the first coefficient is summed.
!  N    number of terms in array CS.
!
!***REFERENCES  R. Broucke, Ten subroutines for the manipulation of
!                 Chebyshev series, Algorithm 446, Communications of
!                 the A.C.M. 16, (1973) pp. 254-256.
!               L. Fox and I. B. Parker, Chebyshev Polynomials in
!                 Numerical Analysis, Oxford University Press, 1968,
!                 page 56.
!
  implicit none

  real ( kind = 8 ) B0, B1, B2, CS(*)
  real ( kind = 8 ) d1mach
  real ( kind = 8 ) dcsevl
  logical ( kind = 4 ) first
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ni
  real ( kind = 8 ) ONEPL, TWOX, X

  SAVE FIRST, ONEPL
  DATA FIRST /.TRUE./

  if (FIRST) ONEPL = 1.0D0 + D1MACH(4)
  FIRST = .FALSE.
  if (N  <  1) call XERMSG ('SLATEC', 'DCSEVL', &
     'NUMBER OF TERMS  <=  0', 2, 2)
  if (N  >  1000) call XERMSG ('SLATEC', 'DCSEVL', &
     'NUMBER OF TERMS  >  1000', 3, 2)
  if (ABS(X)  >  ONEPL) call XERMSG ('SLATEC', 'DCSEVL', &
     'X OUTSIDE THE INTERVAL (-1,+1)', 1, 1)

  B1 = 0.0D0
  B0 = 0.0D0
  TWOX = 2.0D0*X
  DO I = 1,N
     B2 = B1
     B1 = B0
     NI = N + 1 - I
     B0 = TWOX*B1 - B2 + CS(NI)
  end do

  DCSEVL = 0.5D0*(B0-B2)

  return
end
subroutine djairy ( x, rx, c, ai, dai )

!*****************************************************************************80
!
!! DJAIRY computes the Airy function Ai(x) and its derivative.
!
!  Author,
!
!    Donald Amos, S L Danie, M K Weston
!
!***DESCRIPTION
!
!                  DJAIRY computes the Airy function AI(X)
!                   and its derivative DAI(X) for DASYJY
!
!                                   INPUT
!
!         X - Argument, computed by DASYJY, X unrestricted
!        RX - RX=SQRT(ABS(X)), computed by DASYJY
!         C - C=2.*(ABS(X)**1.5)/3., computed by DASYJY
!
!                                  OUTPUT
!
!        AI - Value of function AI(X)
!       DAI - Value of the derivative DAI(X)
!
  implicit none

  integer ( kind = 4 ) I, J, M1, M1D, M2, M2D, M3, M3D, M4, M4D, N1, N1D, N2, &
   N2D, N3, N3D, N4, N4D
  real ( kind = 8 ) A,AI,AJN,AJP,AK1,AK2,AK3,B,C,CCV,CON2, &
   CON3, CON4, CON5, CV, DA, DAI, DAJN, DAJP, DAK1, DAK2, DAK3, &
   DB, EC, E1, E2, FPI12, F1, F2, RTRX, RX, SCV, T, TEMP1, TEMP2, &
   TT, X

  DIMENSION AJP(19), AJN(19), A(15), B(15)
  DIMENSION AK1(14), AK2(23), AK3(14)
  DIMENSION DAJP(19), DAJN(19), DA(15), DB(15)
  DIMENSION DAK1(14), DAK2(24), DAK3(14)
  SAVE N1, N2, N3, N4, M1, M2, M3, M4, FPI12, CON2, CON3, &
   CON4, CON5, AK1, AK2, AK3, AJP, AJN, A, B, &
   N1D, N2D, N3D, N4D, M1D, M2D, M3D, M4D, DAK1, DAK2, DAK3, &
   DAJP, DAJN, DA, DB
  DATA N1,N2,N3,N4/14,23,19,15/
  DATA M1,M2,M3,M4/12,21,17,13/
  DATA FPI12,CON2,CON3,CON4,CON5/ &
   1.30899693899575D+00, 5.03154716196777D+00, 3.80004589867293D-01, &
   8.33333333333333D-01, 8.66025403784439D-01/
  DATA AK1(1), AK1(2), AK1(3), AK1(4), AK1(5), AK1(6), AK1(7), &
       AK1(8), AK1(9), AK1(10),AK1(11),AK1(12),AK1(13), &
       AK1(14)         / 2.20423090987793D-01,-1.25290242787700D-01, &
   1.03881163359194D-02, 8.22844152006343D-04,-2.34614345891226D-04, &
   1.63824280172116D-05, 3.06902589573189D-07,-1.29621999359332D-07, &
   8.22908158823668D-09, 1.53963968623298D-11,-3.39165465615682D-11, &
   2.03253257423626D-12,-1.10679546097884D-14,-5.16169497785080D-15/
  DATA AK2(1), AK2(2), AK2(3), AK2(4), AK2(5), AK2(6), AK2(7), &
       AK2(8), AK2(9), AK2(10),AK2(11),AK2(12),AK2(13),AK2(14), &
       AK2(15),AK2(16),AK2(17),AK2(18),AK2(19),AK2(20),AK2(21), &
       AK2(22),AK2(23) / 2.74366150869598D-01, 5.39790969736903D-03, &
  -1.57339220621190D-03, 4.27427528248750D-04,-1.12124917399925D-04, &
   2.88763171318904D-05,-7.36804225370554D-06, 1.87290209741024D-06, &
  -4.75892793962291D-07, 1.21130416955909D-07,-3.09245374270614D-08, &
   7.92454705282654D-09,-2.03902447167914D-09, 5.26863056595742D-10, &
  -1.36704767639569D-10, 3.56141039013708D-11,-9.31388296548430D-12, &
   2.44464450473635D-12,-6.43840261990955D-13, 1.70106030559349D-13, &
  -4.50760104503281D-14, 1.19774799164811D-14,-3.19077040865066D-15/
  DATA AK3(1), AK3(2), AK3(3), AK3(4), AK3(5), AK3(6), AK3(7), &
       AK3(8), AK3(9), AK3(10),AK3(11),AK3(12),AK3(13), &
       AK3(14)         / 2.80271447340791D-01,-1.78127042844379D-03, &
   4.03422579628999D-05,-1.63249965269003D-06, 9.21181482476768D-08, &
  -6.52294330229155D-09, 5.47138404576546D-10,-5.24408251800260D-11, &
   5.60477904117209D-12,-6.56375244639313D-13, 8.31285761966247D-14, &
  -1.12705134691063D-14, 1.62267976598129D-15,-2.46480324312426D-16/
  DATA AJP(1), AJP(2), AJP(3), AJP(4), AJP(5), AJP(6), AJP(7), &
       AJP(8), AJP(9), AJP(10),AJP(11),AJP(12),AJP(13),AJP(14), &
       AJP(15),AJP(16),AJP(17),AJP(18), &
       AJP(19)         / 7.78952966437581D-02,-1.84356363456801D-01, &
   3.01412605216174D-02, 3.05342724277608D-02,-4.95424702513079D-03, &
  -1.72749552563952D-03, 2.43137637839190D-04, 5.04564777517082D-05, &
  -6.16316582695208D-06,-9.03986745510768D-07, 9.70243778355884D-08, &
   1.09639453305205D-08,-1.04716330588766D-09,-9.60359441344646D-11, &
   8.25358789454134D-12, 6.36123439018768D-13,-4.96629614116015D-14, &
  -3.29810288929615D-15, 2.35798252031104D-16/
  DATA AJN(1), AJN(2), AJN(3), AJN(4), AJN(5), AJN(6), AJN(7), &
       AJN(8), AJN(9), AJN(10),AJN(11),AJN(12),AJN(13),AJN(14), &
       AJN(15),AJN(16),AJN(17),AJN(18), &
       AJN(19)         / 3.80497887617242D-02,-2.45319541845546D-01, &
   1.65820623702696D-01, 7.49330045818789D-02,-2.63476288106641D-02, &
  -5.92535597304981D-03, 1.44744409589804D-03, 2.18311831322215D-04, &
  -4.10662077680304D-05,-4.66874994171766D-06, 7.15218807277160D-07, &
   6.52964770854633D-08,-8.44284027565946D-09,-6.44186158976978D-10, &
   7.20802286505285D-11, 4.72465431717846D-12,-4.66022632547045D-13, &
  -2.67762710389189D-14, 2.36161316570019D-15/
  DATA A(1),   A(2),   A(3),   A(4),   A(5),   A(6),   A(7), &
       A(8),   A(9),   A(10),  A(11),  A(12),  A(13),  A(14), &
       A(15)           / 4.90275424742791D-01, 1.57647277946204D-03, &
  -9.66195963140306D-05, 1.35916080268815D-07, 2.98157342654859D-07, &
  -1.86824767559979D-08,-1.03685737667141D-09, 3.28660818434328D-10, &
  -2.57091410632780D-11,-2.32357655300677D-12, 9.57523279048255D-13, &
  -1.20340828049719D-13,-2.90907716770715D-15, 4.55656454580149D-15, &
  -9.99003874810259D-16/
  DATA B(1),   B(2),   B(3),   B(4),   B(5),   B(6),   B(7), &
       B(8),   B(9),   B(10),  B(11),  B(12),  B(13),  B(14), &
       B(15)           / 2.78593552803079D-01,-3.52915691882584D-03, &
  -2.31149677384994D-05, 4.71317842263560D-06,-1.12415907931333D-07, &
  -2.00100301184339D-08, 2.60948075302193D-09,-3.55098136101216D-11, &
  -3.50849978423875D-11, 5.83007187954202D-12,-2.04644828753326D-13, &
  -1.10529179476742D-13, 2.87724778038775D-14,-2.88205111009939D-15, &
  -3.32656311696166D-16/
  DATA N1D,N2D,N3D,N4D/14,24,19,15/
  DATA M1D,M2D,M3D,M4D/12,22,17,13/
  DATA DAK1(1), DAK1(2), DAK1(3), DAK1(4), DAK1(5), DAK1(6), &
       DAK1(7), DAK1(8), DAK1(9), DAK1(10),DAK1(11),DAK1(12), &
      DAK1(13),DAK1(14)/ 2.04567842307887D-01,-6.61322739905664D-02, &
  -8.49845800989287D-03, 3.12183491556289D-03,-2.70016489829432D-04, &
  -6.35636298679387D-06, 3.02397712409509D-06,-2.18311195330088D-07, &
  -5.36194289332826D-10, 1.13098035622310D-09,-7.43023834629073D-11, &
   4.28804170826891D-13, 2.23810925754539D-13,-1.39140135641182D-14/
  DATA DAK2(1), DAK2(2), DAK2(3), DAK2(4), DAK2(5), DAK2(6), &
       DAK2(7), DAK2(8), DAK2(9), DAK2(10),DAK2(11),DAK2(12), &
       DAK2(13),DAK2(14),DAK2(15),DAK2(16),DAK2(17),DAK2(18), &
       DAK2(19),DAK2(20),DAK2(21),DAK2(22),DAK2(23), &
       DAK2(24)        / 2.93332343883230D-01,-8.06196784743112D-03, &
   2.42540172333140D-03,-6.82297548850235D-04, 1.85786427751181D-04, &
  -4.97457447684059D-05, 1.32090681239497D-05,-3.49528240444943D-06, &
   9.24362451078835D-07,-2.44732671521867D-07, 6.49307837648910D-08, &
  -1.72717621501538D-08, 4.60725763604656D-09,-1.23249055291550D-09, &
   3.30620409488102D-10,-8.89252099772401D-11, 2.39773319878298D-11, &
  -6.48013921153450D-12, 1.75510132023731D-12,-4.76303829833637D-13, &
   1.29498241100810D-13,-3.52679622210430D-14, 9.62005151585923D-15, &
  -2.62786914342292D-15/
  DATA DAK3(1), DAK3(2), DAK3(3), DAK3(4), DAK3(5), DAK3(6), &
       DAK3(7), DAK3(8), DAK3(9), DAK3(10),DAK3(11),DAK3(12), &
      DAK3(13),DAK3(14)/ 2.84675828811349D-01, 2.53073072619080D-03, &
  -4.83481130337976D-05, 1.84907283946343D-06,-1.01418491178576D-07, &
   7.05925634457153D-09,-5.85325291400382D-10, 5.56357688831339D-11, &
  -5.90889094779500D-12, 6.88574353784436D-13,-8.68588256452194D-14, &
   1.17374762617213D-14,-1.68523146510923D-15, 2.55374773097056D-16/
  DATA DAJP(1), DAJP(2), DAJP(3), DAJP(4), DAJP(5), DAJP(6), &
       DAJP(7), DAJP(8), DAJP(9), DAJP(10),DAJP(11),DAJP(12), &
       DAJP(13),DAJP(14),DAJP(15),DAJP(16),DAJP(17),DAJP(18), &
       DAJP(19)        / 6.53219131311457D-02,-1.20262933688823D-01, &
   9.78010236263823D-03, 1.67948429230505D-02,-1.97146140182132D-03, &
  -8.45560295098867D-04, 9.42889620701976D-05, 2.25827860945475D-05, &
  -2.29067870915987D-06,-3.76343991136919D-07, 3.45663933559565D-08, &
   4.29611332003007D-09,-3.58673691214989D-10,-3.57245881361895D-11, &
   2.72696091066336D-12, 2.26120653095771D-13,-1.58763205238303D-14, &
  -1.12604374485125D-15, 7.31327529515367D-17/
  DATA DAJN(1), DAJN(2), DAJN(3), DAJN(4), DAJN(5), DAJN(6), &
       DAJN(7), DAJN(8), DAJN(9), DAJN(10),DAJN(11),DAJN(12), &
       DAJN(13),DAJN(14),DAJN(15),DAJN(16),DAJN(17),DAJN(18), &
       DAJN(19)        / 1.08594539632967D-02, 8.53313194857091D-02, &
  -3.15277068113058D-01,-8.78420725294257D-02, 5.53251906976048D-02, &
   9.41674060503241D-03,-3.32187026018996D-03,-4.11157343156826D-04, &
   1.01297326891346D-04, 9.87633682208396D-06,-1.87312969812393D-06, &
  -1.50798500131468D-07, 2.32687669525394D-08, 1.59599917419225D-09, &
  -2.07665922668385D-10,-1.24103350500302D-11, 1.39631765331043D-12, &
   7.39400971155740D-14,-7.32887475627500D-15/
  DATA DA(1),  DA(2),  DA(3),  DA(4),  DA(5),  DA(6),  DA(7), &
       DA(8),  DA(9),  DA(10), DA(11), DA(12), DA(13), DA(14), &
       DA(15)          / 4.91627321104601D-01, 3.11164930427489D-03, &
   8.23140762854081D-05,-4.61769776172142D-06,-6.13158880534626D-08, &
   2.87295804656520D-08,-1.81959715372117D-09,-1.44752826642035D-10, &
   4.53724043420422D-11,-3.99655065847223D-12,-3.24089119830323D-13, &
   1.62098952568741D-13,-2.40765247974057D-14, 1.69384811284491D-16, &
   8.17900786477396D-16/
  DATA DB(1),  DB(2),  DB(3),  DB(4),  DB(5),  DB(6),  DB(7), &
       DB(8),  DB(9),  DB(10), DB(11), DB(12), DB(13), DB(14), &
       DB(15)          /-2.77571356944231D-01, 4.44212833419920D-03, &
  -8.42328522190089D-05,-2.58040318418710D-06, 3.42389720217621D-07, &
  -6.24286894709776D-09,-2.36377836844577D-09, 3.16991042656673D-10, &
  -4.40995691658191D-12,-5.18674221093575D-12, 9.64874015137022D-13, &
  -4.90190576608710D-14,-1.77253430678112D-14, 5.55950610442662D-15, &
  -7.11793337579530D-16/

  if (X < 0.0D0) go to 90
  if (C > 5.0D0) go to 60
  if (X > 1.20D0) go to 30
  T = (X+X-1.2D0)*CON4
  TT = T + T
  J = N1
  F1 = AK1(J)
  F2 = 0.0D0
  DO I=1,M1
    J = J - 1
    TEMP1 = F1
    F1 = TT*F1 - F2 + AK1(J)
    F2 = TEMP1
  end do
  AI = T*F1 - F2 + AK1(1)

  J = N1D
  F1 = DAK1(J)
  F2 = 0.0D0
  DO I=1,M1D
    J = J - 1
    TEMP1 = F1
    F1 = TT*F1 - F2 + DAK1(J)
    F2 = TEMP1
  end do
  DAI = -(T*F1-F2+DAK1(1))
  return

   30 CONTINUE
  T = (X+X-CON2)*CON3
  TT = T + T
  J = N2
  F1 = AK2(J)
  F2 = 0.0D0
  DO I=1,M2
    J = J - 1
    TEMP1 = F1
    F1 = TT*F1 - F2 + AK2(J)
    F2 = TEMP1
  end do
  RTRX = SQRT(RX)
  EC = EXP(-C)
  AI = EC*(T*F1-F2+AK2(1))/RTRX
  J = N2D
  F1 = DAK2(J)
  F2 = 0.0D0
  DO I=1,M2D
    J = J - 1
    TEMP1 = F1
    F1 = TT*F1 - F2 + DAK2(J)
    F2 = TEMP1
  end do
  DAI = -EC*(T*F1-F2+DAK2(1))*RTRX
  return

   60 CONTINUE
  T = 10.0D0/C - 1.0D0
  TT = T + T
  J = N1
  F1 = AK3(J)
  F2 = 0.0D0
  DO I=1,M1
    J = J - 1
    TEMP1 = F1
    F1 = TT*F1 - F2 + AK3(J)
    F2 = TEMP1
  end do
  RTRX = SQRT(RX)
  EC = EXP(-C)
  AI = EC*(T*F1-F2+AK3(1))/RTRX
  J = N1D
  F1 = DAK3(J)
  F2 = 0.0D0
  DO I=1,M1D
    J = J - 1
    TEMP1 = F1
    F1 = TT*F1 - F2 + DAK3(J)
    F2 = TEMP1
  end do
  DAI = -RTRX*EC*(T*F1-F2+DAK3(1))
  return

   90 CONTINUE

  if (C > 5.0D0) go to 120
  T = 0.4D0*C - 1.0D0
  TT = T + T
  J = N3
  F1 = AJP(J)
  E1 = AJN(J)
  F2 = 0.0D0
  E2 = 0.0D0
  DO I=1,M3
    J = J - 1
    TEMP1 = F1
    TEMP2 = E1
    F1 = TT*F1 - F2 + AJP(J)
    E1 = TT*E1 - E2 + AJN(J)
    F2 = TEMP1
    E2 = TEMP2
  end do
  AI = (T*E1-E2+AJN(1)) - X*(T*F1-F2+AJP(1))
  J = N3D
  F1 = DAJP(J)
  E1 = DAJN(J)
  F2 = 0.0D0
  E2 = 0.0D0
  DO I=1,M3D
    J = J - 1
    TEMP1 = F1
    TEMP2 = E1
    F1 = TT*F1 - F2 + DAJP(J)
    E1 = TT*E1 - E2 + DAJN(J)
    F2 = TEMP1
    E2 = TEMP2
  end do
  DAI = X*X*(T*F1-F2+DAJP(1)) + (T*E1-E2+DAJN(1))
  return

  120 CONTINUE
  T = 10.0D0 / C - 1.0D0
  TT = T + T
  J = N4
  F1 = A(J)
  E1 = B(J)
  F2 = 0.0D0
  E2 = 0.0D0
  DO I=1,M4
    J = J - 1
    TEMP1 = F1
    TEMP2 = E1
    F1 = TT*F1 - F2 + A(J)
    E1 = TT*E1 - E2 + B(J)
    F2 = TEMP1
    E2 = TEMP2
  end do
  TEMP1 = T*F1 - F2 + A(1)
  TEMP2 = T*E1 - E2 + B(1)
  RTRX = SQRT(RX)
  CV = C - FPI12
  CCV = COS(CV)
  SCV = SIN(CV)
  AI = (TEMP1*CCV-TEMP2*SCV)/RTRX
  J = N4D
  F1 = DA(J)
  E1 = DB(J)
  F2 = 0.0D0
  E2 = 0.0D0
  DO I=1, M4D
    J = J - 1
    TEMP1 = F1
    TEMP2 = E1
    F1 = TT*F1 - F2 + DA(J)
    E1 = TT*E1 - E2 + DB(J)
    F2 = TEMP1
    E2 = TEMP2
  end do
  TEMP1 = T*F1 - F2 + DA(1)
  TEMP2 = T*E1 - E2 + DB(1)
  E1 = CCV*CON5 + 0.5D0*SCV
  E2 = SCV*CON5 - 0.5D0*CCV
  DAI = (TEMP1*E1-TEMP2*E2)*RTRX

  return
end
subroutine dgamlm ( xmin, xmax )

!*****************************************************************************80
!
!! DGAMLM computes bounds for the argument in the Gamma function.
!
!  Author:
!
!    Wayne Fullerton
!
!***DESCRIPTION
!
! Calculate the minimum and maximum legal bounds for X in gamma(X).
! XMIN and XMAX are not the only bounds, but they are the only non-
! trivial ones to calculate.
!
!             Output Arguments --
!
! XMIN   double precision minimum legal value of X in gamma(X).  Any
!        smaller value of X might result in underflow.
!
! XMAX   double precision maximum legal value of X in gamma(X).  Any
!        larger value of X might cause overflow.
!
  implicit  none

  real ( kind = 8 ) alnbig
  real ( kind = 8 ) ALNSML
  real ( kind = 8 ) d1mach
  integer ( kind = 4 ) i
  real ( kind = 8 ) xmax
  real ( kind = 8 ) XMIN, XLN, XOLD

  ALNSML = LOG ( D1MACH(1) )
  XMIN = -ALNSML
  DO I=1,10
    XOLD = XMIN
    XLN = LOG(XMIN)
    XMIN = XMIN - XMIN*((XMIN+0.5D0)*XLN - XMIN - 0.2258D0 + ALNSML) &
      / (XMIN*XLN+0.5D0)
    if (ABS(XMIN-XOLD) < 0.005D0) go to 20
  end do
  call XERMSG ('SLATEC', 'DGAMLM', 'UNABLE TO FIND XMIN', 1, 2)

 20   XMIN = -XMIN + 0.01D0

  ALNBIG = LOG (D1MACH(2))
  XMAX = ALNBIG
  DO I=1,10
    XOLD = XMAX
    XLN = LOG(XMAX)
    XMAX = XMAX - XMAX*((XMAX-0.5D0)*XLN - XMAX + 0.9189D0 - ALNBIG) &
      / (XMAX*XLN-0.5D0)
    if (ABS(XMAX-XOLD) < 0.005D0) go to 40
  end do
  call XERMSG ('SLATEC', 'DGAMLM', 'UNABLE TO FIND XMAX', 2, 2)

 40   continue

  XMAX = XMAX - 0.01D0
  XMIN = MAX (XMIN, -XMAX+1.D0)

  return
end
function dlngam ( x )

!*****************************************************************************80
!
!! DLNGAM computes the logarithm of the absolute value of the Gamma function.
!
!  Discussion:
!
!    DLNGAM(X) calculates the double precision logarithm of the
!    absolute value of the Gamma function for double precision
!    argument X.
!
!  Author:
!
!    Wayne Fullerton
!
  implicit none

! real ( kind = 8 ), external :: dgamma
  real ( kind = 8 ) dlngam
  real ( kind = 8 ) dxrel
  logical ( kind = 4 ) first
  real ( kind = 8 ) X, PI, SINPIY, SQPI2L, SQ2PIL, XMAX, &
    Y, D9LGMC, D1MACH, TEMP

  SAVE SQ2PIL, SQPI2L, PI, XMAX, DXREL, FIRST
  DATA SQ2PIL / 0.91893853320467274178032973640562D0 /
  DATA SQPI2L / +.225791352644727432363097614947441D+0    /
  DATA PI / 3.14159265358979323846264338327950D0 /
  DATA FIRST /.TRUE./

  if (FIRST) THEN
     TEMP = 1.D0/LOG(D1MACH(2))
     XMAX = TEMP*D1MACH(2)
     DXREL = SQRT(D1MACH(4))
  end if

  FIRST = .FALSE.

  Y = ABS (X)
  if (Y > 10.D0) go to 20
!
!  LOG (ABS (DGAMMA(X)) ) FOR ABS(X)  <=  10.0
!
  DLNGAM = LOG (ABS (DGAMMA(X)) )
  return
!
!  LOG ( ABS (DGAMMA(X)) ) FOR ABS(X)  >  10.0
!
 20   if (Y  >  XMAX) call XERMSG ('SLATEC', 'DLNGAM', &
     'ABS(X) SO BIG DLNGAM OVERFLOWS', 2, 2)

  if (X > 0.D0) DLNGAM = SQ2PIL + (X-0.5D0)*LOG(X) - X + D9LGMC(Y)
  if (X > 0.D0) RETURN

  SINPIY = ABS (SIN(PI*Y))
  if (SINPIY  ==  0.D0) call XERMSG ('SLATEC', 'DLNGAM', &
     'X IS A NEGATIVE INTEGER', 3, 2)

  if (ABS((X-AINT(X-0.5D0))/X)  <  DXREL) call XERMSG ('SLATEC', &
     'DLNGAM', &
     'ANSWER LT HALF PRECISION BECAUSE X TOO NEAR NEGATIVE INTEGER', &
     1, 1)

  DLNGAM = SQPI2L + (X-0.5D0)*LOG(Y) - X - LOG(SINPIY) - D9LGMC(Y)

  return
end
function i1mach ( i )

!*****************************************************************************80
!
!! I1MACH returns integer machine constants.
!
!  Discussion:
!
!    Input/output unit numbers.
!
!      I1MACH(1) = the standard input unit.
!      I1MACH(2) = the standard output unit.
!      I1MACH(3) = the standard punch unit.
!      I1MACH(4) = the standard error message unit.
!
!    Words.
!
!      I1MACH(5) = the number of bits per integer storage unit.
!      I1MACH(6) = the number of characters per integer storage unit.
!
!    Integers.
!
!    Assume integers are represented in the S digit base A form:
!
!      Sign * (X(S-1)*A^(S-1) + ... + X(1)*A + X(0))
!
!    where 0 <= X(1:S-1) < A.
!
!      I1MACH(7) = A, the base.
!      I1MACH(8) = S, the number of base A digits.
!      I1MACH(9) = A^S-1, the largest integer.
!
!    Floating point numbers
!
!    Assume floating point numbers are represented in the T digit 
!    base B form:
!
!      Sign * (B^E) * ((X(1)/B) + ... + (X(T)/B^T) )
!
!    where 0 <= X(I) < B for I=1 to T, 0 < X(1) and EMIN <= E <= EMAX.
!
!      I1MACH(10) = B, the base.
!
!    Single precision
!
!      I1MACH(11) = T, the number of base B digits.
!      I1MACH(12) = EMIN, the smallest exponent E.
!      I1MACH(13) = EMAX, the largest exponent E.
!
!    Double precision
!
!      I1MACH(14) = T, the number of base B digits.
!      I1MACH(15) = EMIN, the smallest exponent E.
!      I1MACH(16) = EMAX, the largest exponent E.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by Phyllis Fox, Andrew Hall, Norman Schryer.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Phyllis Fox, Andrew Hall, Norman Schryer,
!    Algorithm 528,
!    Framework for a Portable Library,
!    ACM Transactions on Mathematical Software,
!    Volume 4, Number 2, June 1978, page 176-188.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, chooses the parameter to be returned.
!    1 <= I <= 16.
!
!    Output, integer ( kind = 4 ) I1MACH, the value of the chosen parameter.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1mach

  if ( i < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I1MACH - Fatal error!'
    write ( *, '(a)' ) '  The input argument I is out of bounds.'
    write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 16.'
    write ( *, '(a,i12)' ) '  I = ', i
    i1mach = 0
    stop
  else if ( i == 1 ) then
    i1mach = 5
  else if ( i == 2 ) then
    i1mach = 6
  else if ( i == 3 ) then
    i1mach = 7
  else if ( i == 4 ) then
    i1mach = 6
  else if ( i == 5 ) then
    i1mach = 32
  else if ( i == 6 ) then
    i1mach = 4
  else if ( i == 7 ) then
    i1mach = 2
  else if ( i == 8 ) then
    i1mach = 31
  else if ( i == 9 ) then
    i1mach = 2147483647
  else if ( i == 10 ) then
    i1mach = 2
  else if ( i == 11 ) then
    i1mach = 24
  else if ( i == 12 ) then
    i1mach = -125
  else if ( i == 13 ) then
    i1mach = 128
  else if ( i == 14 ) then
    i1mach = 53
  else if ( i == 15 ) then
    i1mach = -1021
  else if ( i == 16 ) then
    i1mach = 1024
  else if ( 16 < i ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I1MACH - Fatal error!'
    write ( *, '(a)' ) '  The input argument I is out of bounds.'
    write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 16.'
    write ( *, '(a,i12)' ) '  I = ', i
    i1mach = 0
    stop
  end if

  return
end
function initds ( os, nos, eta )

!*****************************************************************************80
!
!! INITDS determines the necessary length of a Chebyshev series.
!
!  Discussion:
!
!    INITDS determines the number of terms needed in an orthogonal 
!    polynomial series so that it meets a specified accuracy.
!
!    Initialize the orthogonal series, represented by the array OS, so
!    that INITDS is the number of terms needed to insure the error is no
!    larger than ETA.  Ordinarily, ETA will be chosen to be one-tenth
!    machine precision.
!
!  Author:
!
!    Wayne Fullerton
!
!  Parameters:
!
!    Input, OS, coefficients in an orthogonal series.
!
!    Input, NOS, number of coefficients in OS.
!
!    Input, ETA, requested accuracy of series.
!
  implicit none

  real ( kind = 8 ) err
  real ( kind = 8 ) eta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) initds
  integer ( kind = 4 ) nos
  real ( kind = 8 ) os(*)

  if ( NOS < 1 ) then
    call XERMSG ('SLATEC', 'INITDS', &
     'Number of coefficients is less than 1', 2, 1)
  end if

  ERR = 0.0D+00

  DO II = 1, NOS
    I = NOS + 1 - II
    ERR = ERR + ABS ( OS(I) )
    if ( ERR > ETA) then
      initds = i
      return
    end if
  end do

  call XERMSG ('SLATEC', 'INITDS', &
     'Chebyshev series too short for specified accuracy', 1, 1)
  INITDS = NOS

  return
end
subroutine rjbesl ( x, alpha, nb, b, ncalc )

!*****************************************************************************80
!
!! RJBESL evaluates a sequence of Bessel J functions.
!
!  Discussion:
!
!    This routine calculates Bessel functions J sub(N+ALPHA) (X)
!    for non-negative argument X, and non-negative order N+ALPHA.
!
!    This program is based on a program written by David Sookne
!    that computes values of the Bessel functions J or I of real
!    argument and integer order.  Modifications include the restriction
!    of the computation to the J Bessel function of non-negative real
!    argument, the extension of the computation to arbitrary positive
!    order, and the elimination of most underflow.
!
!  Modified:
!
!    15 January 2016
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Reference: 
!
!    F Olver, David Sookne,
!    A Note on Backward Recurrence Algorithms," 
!    Math. Comp.,
!    Volume 26, 1972, pages 941-947.
!
!    David Sookne,
!    Bessel Functions of Real Argument and Integer Order,
!    NBS Journal of Res. B,
!    Volume 77B, 1973, pages 125-132.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, non-negative real argument for which
!    J's are to be calculated.
!
!    Input, real ( kind = 8 ) ALPHA, fractional part of order for which
!    J's or exponentially scaled J'r (J*exp(X)) are
!    to be calculated.  0 <= ALPHA < 1.0.
!
!    Input, integer ( kind = 4 ) NB, number of functions to be calculated, 
!    NB > 0.  The first function calculated is of order ALPHA, and the
!    last is of order (NB - 1 + ALPHA).
!
!    Output, real ( kind = 8 ) B(NB).  If RJBESL
!    terminates normally (NCALC=NB), the vector B contains the
!    functions J/ALPHA/(X) through J/NB-1+ALPHA/(X), or the
!    corresponding exponentially scaled functions.
!
!    Output, integer ( kind = 4 ) NCALC, indicates possible errors.
!    Before using the vector B, the user should check that
!    NCALC=NB, i.e., all orders have been calculated to
!    the desired accuracy.  See Error Returns below.
!
!  Internal Parameters:
!
!    IT = Number of bits in the mantissa of a working precision variable
!
!    NSIG   = Decimal significance desired.  Should be set to
!    INT(LOG10(2)*it+1).  Setting NSIG lower will result
!    in decreased accuracy while setting NSIG higher will
!    increase CPU time without increasing accuracy.  The
!    truncation error is limited to a relative error of
!    T=.5*10**(-NSIG).
!
!    Then the following machine-dependent constants must be declared
!    in DATA statements.  IEEE values are provided as a default.
!
!    ENTEN  = 10.0 ** K, where K is the largest integer such that
!    ENTEN is machine-representable in working precision.
!
!    ENSIG  = 10.0 ** NSIG
!
!    RTNSIG = 10.0 ** (-K) for the smallest integer K such that K >= NSIG/4
!
!    ENMTEN = Smallest ABS(X) such that X/4 does not underflow
!
!    XLARGE = Upper limit on the magnitude of X.  If ABS(X)=N,
!    then at least N iterations of the backward recursion
!    will be executed.  The value of 10.0 ** 4 is used on
!    every machine.
!
!  Error returns:
!
!    In case of an error,  NCALC /= NB, and not all J's are
!    calculated to the desired accuracy.
!
!    NCALC < 0:  An argument is out of range. For example,
!    NBES <= 0, ALPHA < 0 or > 1, or X is too large.
!    In this case, B(1) is set to zero, the remainder of the
!    B-vector is not calculated, and NCALC is set to
!    MIN(NB,0)-1 so that NCALC /= NB.
!
!    NB > NCALC > 0: Not all requested function values could
!    be calculated accurately.  This usually occurs because NB is
!    much larger than ABS(X).  In this case, B(N) is calculated
!    to the desired accuracy for N <= NCALC, but precision
!    is lost for NCALC < N <= NB.  If B(N) does not vanish
!    for N > NCALC (because it is too small to be represented),
!    and B(N)/B(NCALC) = 10**(-K), then only the first NSIG-K
!    significant figures of B(N) can be trusted.
!
  implicit none

  integer ( kind = 4 ) nb

  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpem
  real ( kind = 8 ) alp2em
  real ( kind = 8 ) b(nb)
  real ( kind = 8 ) capp
  real ( kind = 8 ) capq
  real ( kind = 8 ) eighth
  real ( kind = 8 ) em
  real ( kind = 8 ) en
  real ( kind = 8 ) enmten
  real ( kind = 8 ) ensig
  real ( kind = 8 ) enten
  real ( kind = 8 ) fact(25)
  real ( kind = 8 ) four
  real ( kind = 8 ) gnu
  real ( kind = 8 ) half
  real ( kind = 8 ) halfx
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  logical ( kind = 4 ) jump
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) magx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nbmx
  integer ( kind = 4 ) ncalc
  integer ( kind = 4 ) nend
  integer ( kind = 4 ) nstart
  real ( kind = 8 ) one
  real ( kind = 8 ) one30
  real ( kind = 8 ) p
  real ( kind = 8 ) pi2
  real ( kind = 8 ) plast
  real ( kind = 8 ) pold
  real ( kind = 8 ) psave
  real ( kind = 8 ) psavel
  real ( kind = 8 ) rtnsig
  real ( kind = 8 ) s
  real ( kind = 8 ) sum
  real ( kind = 8 ) t
  real ( kind = 8 ) t1
  real ( kind = 8 ) tempa
  real ( kind = 8 ) tempb
  real ( kind = 8 ) tempc
  real ( kind = 8 ) test
  real ( kind = 8 ) three
  real ( kind = 8 ) three5
  real ( kind = 8 ) tover
  real ( kind = 8 ) two
  real ( kind = 8 ) twofiv
  real ( kind = 8 ) twopi1
  real ( kind = 8 ) twopi2
  real ( kind = 8 ) x
  real ( kind = 8 ) xc
  real ( kind = 8 ) xin
  real ( kind = 8 ) xk
  real ( kind = 8 ) xlarge
  real ( kind = 8 ) xm
  real ( kind = 8 ) vcos
  real ( kind = 8 ) vsin
  real ( kind = 8 ) z
  real ( kind = 8 ) zero
!
!  Mathematical constants
!
!  PI2    - 2 / PI
!  TWOPI1 - first few significant digits of 2 * PI
!  TWOPI2 - (2*PI - TWOPI) to working precision, i.e.,
!  TWOPI1 + TWOPI2 = 2 * PI to extra precision.
!
  data pi2 / 0.636619772367581343075535d+00 /
  data twopi1 / 6.28125d+00 /
  data twopi2 / 1.935307179586476925286767d-03 /
  data zero / 0.0d+00 /
  data eighth / 0.125d+00 /
  data half / 0.5d+00 /
  data one / 1.0d+00 /
  data two / 2.0d+00 /
  data three / 3.0d+00 /
  data four / 4.0d+00 /
  data twofiv / 25.0d+00 /
  data one30 / 130.0d+00 /
  data three5 / 35.0d+00 /
!
!  Machine-dependent parameters
!
  data enten / 1.0D+308 /
  data ensig / 1.0D+16 /
  data rtnsig / 1.0D-04 /
  data enmten / 8.90D-308 /
  data xlarge / 1.0D+04 /
!
!  Factorial(N)
!
  data fact / &
    1.0d+00, &
    1.0d+00, &
    2.0d+00, &
    6.0d+00, &
    24.0d+00, &
    1.2d+02, &
    7.2d+02, &
    5.04d+03, &
    4.032d+04, &
    3.6288d+05, &
    3.6288d+06, &
    3.99168d+07, &
    4.790016d+08, &
    6.2270208d+09, &
    8.71782912d+10, &
    1.307674368d+12, &
    2.0922789888d+13, &
    3.55687428096d+14, &
    6.402373705728d+15, &
    1.21645100408832d+17, &
    2.43290200817664d+18, &
    5.109094217170944d+19, &
    1.12400072777760768d+21, &
    2.585201673888497664d+22, &
    6.2044840173323943936d+23 /

    jump = .false.
!
!  Check for out of range arguments.
!
  magx = int ( x )

  if ( &
    ( 0 < nb ) .and. &
    ( zero <= x ) .and. &
    ( x <= xlarge ) .and. &
    ( zero <= alpha ) .and. &
    ( alpha < one ) ) then
!
!  Initialize result array to zero.
!
    ncalc = nb
    b(1:nb) = zero
!
!  Branch to use 2-term ascending series for small X and asymptotic
!  form for large X when NB is not too large.
!
    if ( x < rtnsig ) then
!
!  Two-term ascending series for small X.
!
      tempa = one
      alpem = one + alpha

      if ( enmten < x ) then
        halfx = half * x
      else
        halfx = zero
      end if

      if ( alpha /= zero ) then
        tempa = halfx ** alpha / ( alpha * gamma ( alpha ) )
      end if

      if ( one < ( x + one ) ) then
        tempb = - halfx * halfx
      else
        tempb = zero
      end if

      b(1) = tempa + tempa * tempb / alpem

      if ( ( x /= zero ) .and. ( b(1) == zero ) ) then
        ncalc = 0
      end if

      if ( nb /= 1 ) then

        if ( x <= zero ) then

          do n = 2, nb
            b(n) = zero
          end do

        else
!
!  Calculate higher order functions.
!
          tempc = halfx

          if ( tempb /= zero ) then
            tover = enmten / tempb
          else
            tover = ( enmten + enmten ) / x
          end if

          do n = 2, nb

            tempa = tempa / alpem
            alpem = alpem + one

            tempa = tempa * tempc
            if ( tempa <= tover * alpem ) then
              tempa = zero
            end if

            b(n) = tempa + tempa * tempb / alpem

            if ( ( b(n) == zero ) .and. ( n < ncalc ) ) then
              ncalc = n - 1
            end if

          end do

        end if

      end if

    else if ( ( twofiv < x ) .and. ( nb <= magx + 1 ) ) then
!
!  Asymptotic series for 21.0 < X.
!
      xc = sqrt ( pi2 / x )
      xin = ( eighth / x ) ** 2

      if ( x < three5 ) then
        m = 11
      else if ( x < one30 ) then
        m = 8
      else
        m = 4
      end if

      xm = four * real ( m, kind = 8 )
!
!  Argument reduction for SIN and COS routines.
!
      t = aint ( x / ( twopi1 + twopi2 ) + half )
      z = ( ( x - t * twopi1 ) - t * twopi2 ) - ( alpha + half ) / pi2
      vsin = sin ( z )
      vcos = cos ( z )
      gnu = alpha + alpha

      do i = 1, 2

        s = ( ( xm - one ) - gnu ) * ( ( xm - one ) + gnu ) * xin * half
        t = ( gnu - ( xm - three ) ) * ( gnu + ( xm - three ) )
        capp = s * t / fact(2*m+1)
        t1 = ( gnu - ( xm + one ) ) * ( gnu + ( xm + one ) )
        capq = s * t1 / fact(2*m+2)
        xk = xm
        k = m + m
        t1 = t

        do j = 2, m
          xk = xk - four
          s = ( ( xk - one ) - gnu ) * ( ( xk - one ) + gnu )
          t = ( gnu - ( xk - three ) ) * ( gnu + ( xk - three ) )
          capp = ( capp + one / fact(k-1) ) * s * t * xin
          capq = ( capq + one / fact(k) ) * s * t1 * xin
          k = k - 2
          t1 = t
        end do

        capp = capp + one
        capq = ( capq + one ) * ( gnu * gnu - one ) * ( eighth / x )
        b(i) = xc * ( capp * vcos - capq * vsin )

        if ( nb == 1 ) then
          return
        end if

        t = vsin
        vsin = - vcos
        vcos = t
        gnu = gnu + two

      end do
!
!  If 2 < NB, compute J(X,ORDER+I)  I = 2, NB-1
!
      if ( 2 < nb ) then
        gnu = alpha + alpha + two
        do j = 3, nb
          b(j) = gnu * b(j-1) / x - b(j-2)
          gnu = gnu + two
        end do
      end if
!
!  Use recurrence to generate results.  First initialize the
!  calculation of P*S.
!
    else

      nbmx = nb - magx
      n = magx + 1
      en = real ( n + n, kind = 8 ) + ( alpha + alpha )
      plast = one
      p = en / x
!
!  Calculate general significance test.
!
      test = ensig + ensig

      if ( 3 <= nbmx ) then
!
!  Calculate P*S until N = NB-1.  Check for possible overflow.
!
        tover = enten / ensig
        nstart = magx + 2
        nend = nb - 1
        en = real ( nstart + nstart, kind = 8 ) - two + ( alpha + alpha )

        do k = nstart, nend

          n = k
          en = en + two
          pold = plast
          plast = p
          p = en * plast / x - pold

          if ( tover < p ) then
!
!  To avoid overflow, divide P*S by TOVER.  Calculate P*S until 1 < ABS(P).
!
            tover = enten
            p = p / tover
            plast = plast / tover
            psave = p
            psavel = plast
            nstart = n + 1

            do

              n = n + 1
              en = en + two
              pold = plast
              plast = p
              p = en * plast / x - pold
              if ( one < p ) then
                exit
              end if

            end do

            tempb = en / x
!
!  Calculate backward test and find NCALC, the highest N such that
!  the test is passed.
!
            test = pold * plast * ( half - half / ( tempb * tempb ) )
            test = test / ensig
            p = plast * tover
            n = n - 1
            en = en - two
            nend = min ( nb, n )

            do l = nstart, nend
              pold = psavel
              psavel = psave
              psave = en * psavel / x - pold
              if ( test < psave * psavel ) then
                ncalc = l - 1
                jump = .true.
                exit
              end if
            end do

            if ( jump ) then
              exit
            end if

            ncalc = nend
            jump = .true.
            exit

          end if

        end do

        if ( .not. jump ) then

          n = nend
          en = real ( n + n, kind = 8 ) + ( alpha + alpha )
!
!  Calculate special significance test for 2 < NBMX.
!
          test = max ( test, sqrt ( plast * ensig ) * sqrt ( p + p ) )

        end if

      end if
!
!  Calculate P*S until significance test passes.
!
      if ( .not. jump ) then

        do

          n = n + 1
          en = en + two
          pold = plast
          plast = p
          p = en * plast / x - pold

          if ( test <= p ) then
            exit
          end if

        end do

      end if
!
!  Initialize the backward recursion and the normalization sum.
!
      n = n + 1
      en = en + two
      tempb = zero
      tempa = one / p
      m = 2 * n - 4 * ( n / 2 )
      sum = zero
      em = real ( n / 2, kind = 8 )
      alpem = ( em - one ) + alpha
      alp2em = ( em + em ) + alpha
      if ( m /= 0 ) then
        sum = tempa * alpem * alp2em / em
      end if
      nend = n - nb

      if ( 0 < nend ) then
!
!  Recur backward via difference equation, calculating (but not
!  storing) B(N), until N = NB.
!
        do l = 1, nend

          n = n - 1
          en = en - two
          tempc = tempb
          tempb = tempa
          tempa = ( en * tempb ) / x - tempc
          m = 2 - m

          if ( m /= 0 ) then
            em = em - one
            alp2em = ( em + em ) + alpha
            if ( n == 1 ) then
              exit
            end if
            alpem = ( em - one ) + alpha
            if ( alpem == zero ) then
              alpem = one
            end if
            sum = ( sum + tempa * alp2em ) * alpem / em
          end if

        end do

      end if
!
!  Store B(NB).
!
      b(n) = tempa

      if ( 0 <= nend ) then

        if ( nb <= 1 ) then

          alp2em = alpha
          if ( ( alpha + one ) == one ) then
            alp2em = one
          end if
          sum = sum + b(1) * alp2em

          if ( ( alpha + one ) /= one ) then
            sum = sum * gamma ( alpha ) * ( x * half ) ** ( - alpha )
          end if

          tempa = enmten

          if ( one < sum ) then
            tempa = tempa * sum
          end if

          do n = 1, nb
            if ( abs ( b(n) ) < tempa ) then
              b(n) = zero
            end if
            b(n) = b(n) / sum
          end do

          return

        else
!
!  Calculate and store B(NB-1).
!
          n = n - 1
          en = en - two
          b(n) = ( en * tempa ) / x - tempb

          if ( n == 1 ) then

            em = em - one
            alp2em = ( em + em ) + alpha
            if ( alp2em == zero ) then
              alp2em = one
            end if
            sum = sum + b(1) * alp2em
!
!  Normalize.  Divide all B(N) by sum.
!
            if ( ( alpha + one ) /= one ) then
              sum = sum * gamma ( alpha ) * ( x * half ) ** ( - alpha )
            end if

            tempa = enmten

            if ( one < sum ) then
              tempa = tempa * sum
            end if
  
            do n = 1, nb
              if ( abs ( b(n) ) < tempa ) then
                b(n) = zero
              end if
              b(n) = b(n) / sum
            end do

            return

          end if

          m = 2 - m

          if ( m /= 0 ) then
            em = em - one
            alp2em = ( em + em ) + alpha
            alpem = ( em - one ) + alpha
            if ( alpem == zero ) then
              alpem = one
            end if
            sum = ( sum + b(n) * alp2em ) * alpem / em
          end if

        end if

      end if

      nend = n - 2

      if ( nend /= 0 ) then
!
!  Calculate via difference equation and store B(N), until N = 2.
!
        do l = 1, nend

          n = n - 1
          en = en - two
          b(n) = ( en * b(n+1) ) / x - b(n+2)
          m = 2 - m

          if ( m /= 0 ) then
            em = em - one
            alp2em = ( em + em ) + alpha
            alpem = ( em - one ) + alpha
            if ( alpem == zero ) then
              alpem = one
            end if
            sum = ( sum + b(n) * alp2em ) * alpem / em
          end if

        end do

      end if
!
!  Calculate B(1).
!
      b(1) = two * ( alpha + one ) * b(2) / x - b(3)

      em = em - one
      alp2em = ( em + em ) + alpha
      if ( alp2em == zero ) then
        alp2em = one
      end if
      sum = sum + b(1) * alp2em
!
!  Normalize.  Divide all B(N) by sum.
!
      if ( ( alpha + one ) /= one ) then
        sum = sum * gamma ( alpha ) * ( x * half ) ** ( - alpha )
      end if

      tempa = enmten

      if ( one < sum ) then
        tempa = tempa * sum
      end if

      do n = 1, nb
        if ( abs ( b(n) ) < tempa ) then
          b(n) = zero
        end if
        b(n) = b(n) / sum
      end do

    end if
!
!  Error return.
!
  else

    b(1) = zero
    ncalc = min ( nb, 0 ) - 1

  end if

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
subroutine xermsg ( librar, subrou, messg, nerr, level )

!*****************************************************************************80
!
!! XERMSG processes error messages.
!
!***AUTHOR  Fong, Kirby, (NMFECC at LLNL)
!
!***DESCRIPTION
!
!   XERMSG processes a diagnostic message in a manner determined by the
!   value of LEVEL and the current value of the library error control
!   flag, KONTRL.  See subroutine XSETF for details.
!
!    LIBRAR   A character constant (or character variable) with the name
!             of the library.  This will be 'SLATEC' for the SLATEC
!             Common Math Library.  The error handling package is
!             general enough to be used by many libraries
!             simultaneously, so it is desirable for the routine that
!             detects and reports an error to identify the library name
!             as well as the routine name.
!
!    SUBROU   A character constant (or character variable) with the name
!             of the routine that detected the error.  Usually it is the
!             name of the routine that is calling XERMSG.  There are
!             some instances where a user callable library routine calls
!             lower level subsidiary routines where the error is
!             detected.  In such cases it may be more informative to
!             supply the name of the routine the user called rather than
!             the name of the subsidiary routine that detected the
!             error.
!
!    MESSG    A character constant (or character variable) with the text
!             of the error or warning message.  In the example below,
!             the message is a character constant that contains a
!             generic message.
!
!                   call XERMSG ('SLATEC', 'MMPY',
!                  *'THE ORDER OF THE MATRIX EXCEEDS THE ROW DIMENSION',
!                  *3, 1)
!
!             It is possible (and is sometimes desirable) to generate a
!             specific message--e.g., one that contains actual numeric
!             values.  Specific numeric values can be converted into
!             character strings using formatted WRITE statements into
!             character variables.  This is called standard Fortran
!             internal file I/O and is exemplified in the first three
!             lines of the following example.  You can also catenate
!             substrings of characters to construct the error message.
!             Here is an example showing the use of both writing to
!             an internal file and catenating character strings.
!
!                   CHARACTER*5 CHARN, CHARL
!                   WRITE (CHARN,10) N
!                   WRITE (CHARL,10) LDA
!                10 FORMAT(I5)
!                   call XERMSG ('SLATEC', 'MMPY', 'THE ORDER'//CHARN//
!                  *   ' OF THE MATRIX EXCEEDS ITS ROW DIMENSION OF'//
!                  *   CHARL, 3, 1)
!
!             There are two subtleties worth mentioning.  One is that
!             the // for character catenation is used to construct the
!             error message so that no single character constant is
!             continued to the next line.  This avoids confusion as to
!             whether there are trailing blanks at the end of the line.
!             The second is that by catenating the parts of the message
!             as an actual argument rather than encoding the entire
!             message into one large character variable, we avoid
!             having to know how long the message will be in order to
!             declare an adequate length for that large character
!             variable.  XERMSG calls XERPRN to print the message using
!             multiple lines if necessary.  If the message is very long,
!             XERPRN will break it into pieces of 72 characters (as
!             requested by XERMSG) for printing on multiple lines.
!             Also, XERMSG asks XERPRN to prefix each line with ' *  '
!             so that the total line length could be 76 characters.
!             Note also that XERPRN scans the error message backwards
!             to ignore trailing blanks.  Another feature is that
!             the substring '$$' is treated as a new line sentinel
!             by XERPRN.  If you want to construct a multiline
!             message without having to count out multiples of 72
!             characters, just use '$$' as a separator.  '$$'
!             obviously must occur within 72 characters of the
!             start of each line to have its intended effect since
!             XERPRN is asked to wrap around at 72 characters in
!             addition to looking for '$$'.
!
!    NERR     An integer value that is chosen by the library routine's
!             author.  It must be in the range -99 to 999 (three
!             printable digits).  Each distinct error should have its
!             own error number.  These error numbers should be described
!             in the machine readable documentation for the routine.
!             The error numbers need be unique only within each routine,
!             so it is reasonable for each routine to start enumerating
!             errors from 1 and proceeding to the next integer.
!
!    LEVEL    An integer value in the range 0 to 2 that indicates the
!             level (severity) of the error.  Their meanings are
!
!            -1  A warning message.  This is used if it is not clear
!                that there really is an error, but the user's attention
!                may be needed.  An attempt is made to only print this
!                message once.
!
!             0  A warning message.  This is used if it is not clear
!                that there really is an error, but the user's attention
!                may be needed.
!
!             1  A recoverable error.  This is used even if the error is
!                so serious that the routine cannot return any useful
!                answer.  If the user has told the error package to
!                return after recoverable errors, then XERMSG will
!                return to the Library routine which can then return to
!                the user's routine.  The user may also permit the error
!                package to terminate the program upon encountering a
!                recoverable error.
!
!             2  A fatal error.  XERMSG will not return to its caller
!                after it receives a fatal error.  This level should
!                hardly ever be used; it is much better to allow the
!                user a chance to recover.  An example of one of the few
!                cases in which it is permissible to declare a level 2
!                error is a reverse communication Library routine that
!                is likely to be called repeatedly until it integrates
!                across some interval.  If there is a serious error in
!                the input such that another step cannot be taken and
!                the Library routine is called again without the input
!                error having been corrected by the caller, the Library
!                routine will probably be called forever with improper
!                input.  In this case, it is reasonable to declare the
!                error to be fatal.
!
!    Each of the arguments to XERMSG is input; none will be modified by
!    XERMSG.  A routine may make multiple calls to XERMSG with warning
!    level messages; however, after a call to XERMSG with a recoverable
!    error, the routine should return to the user.  Do not try to call
!    XERMSG with a second recoverable error after the first recoverable
!    error because the error package saves the error number.  The user
!    can retrieve this error number by calling another entry point in
!    the error handling package and then clear the error number when
!    recovering from the error.  Calling XERMSG in succession causes the
!    old error number to be overwritten by the latest error number.
!    This is considered harmless for error numbers associated with
!    warning messages but must not be done for error numbers of serious
!    errors.  After a call to XERMSG with a recoverable error, the user
!    must be given a chance to call NUMXER or XERCLR to retrieve or
!    clear the error number.
!***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
!                 Error-handling Package, SAND82-0800, Sandia
!                 Laboratories, 1982.
!***ROUTINES CALLED  FDUMP, J4SAVE, XERCNT, XERHLT, XERPRN, XERSVE
!***REVISION HISTORY  (YYMMDD)
!   880101  DATE WRITTEN
!   880621  REVISED AS DIRECTED AT SLATEC CML MEETING OF FEBRUARY 1988.
!           THERE ARE TWO BASIC CHANGES.
!           1.  A NEW ROUTINE, XERPRN, IS USED INSTEAD OF XERPRT TO
!               PRINT MESSAGES.  THIS ROUTINE WILL BREAK LONG MESSAGES
!               INTO PIECES FOR PRINTING ON MULTIPLE LINES.  '$$' IS
!               ACCEPTED AS A NEW LINE SENTINEL.  A PREFIX CAN BE
!               ADDED TO EACH LINE TO BE PRINTED.  XERMSG USES EITHER
!               ' ***' OR ' *  ' AND LONG MESSAGES ARE BROKEN EVERY
!               72 CHARACTERS (AT MOST) SO THAT THE MAXIMUM LINE
!               LENGTH OUTPUT CAN NOW BE AS GREAT AS 76.
!           2.  THE TEXT OF ALL MESSAGES IS NOW IN UPPER CASE SINCE THE
!               FORTRAN STANDARD DOCUMENT DOES NOT ADMIT THE EXISTENCE
!               OF LOWER CASE.
!   880708  REVISED AFTER THE SLATEC CML MEETING OF JUNE 29 AND 30.
!           THE PRINCIPAL CHANGES ARE
!           1.  CLARIFY COMMENTS IN THE PROLOGUES
!           2.  RENAME XRPRNT TO XERPRN
!           3.  REWORK HANDLING OF '$$' IN XERPRN TO HANDLE BLANK LINES
!               SIMILAR TO THE WAY FORMAT STATEMENTS HANDLE THE /
!               CHARACTER FOR NEW RECORDS.
!   890706  REVISED WITH THE HELP OF FRED FRITSCH AND REG CLEMENS TO
!           CLEAN UP THE CODING.
!   890721  REVISED TO USE NEW FEATURE IN XERPRN TO COUNT CHARACTERS IN
!           PREFIX.
!   891013  REVISED TO CORRECT COMMENTS.
!   891214  Prologue converted to Version 4.0 format.  (WRB)
!   900510  Changed test on NERR to be -9999999 < NERR < 99999999, but
!           NERR .ne. 0, and on LEVEL to be -2 < LEVEL < 3.  Added
!           LEVEL=-1 logic, changed calls to XERSAV to XERSVE, and
!           XERCTL to XERCNT.  (RWC)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  XERMSG
  CHARACTER*(*) LIBRAR, SUBROU, MESSG
!***FIRST EXECUTABLE STATEMENT  XERMSG

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) librar
  write ( *, '(a)' ) subrou
  write ( *, '(a)' ) messg
  write ( *, * ) nerr
  write ( *, * ) level
  write ( *, '(a)' ) 'XERMSG - Drop dead!'
  stop 1
  return
end
