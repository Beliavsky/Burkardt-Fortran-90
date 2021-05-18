program main

!*****************************************************************************80
!
!! ELLIPTIC_INTEGRAL_TEST tests ELLIPTIC_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ELLIPTIC_INTEGRAL_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  ELLIPTIC_INTEGRAL evaluates elliptic integral functions'
  write ( *, '(a)' ) '  using Carlson''s elliptic functions.'
!
!  Carlson integrals.
!
  call rc_test ( )
  call rc_test2 ( )
  call rd_test ( )
  call rf_test ( )
  call rj_test ( )
!
!  Complete Elliptic Integral of First Kind.
!
  call elliptic_fa_test ( )
  call elliptic_fk_test ( )
  call elliptic_fm_test ( )
!
!  Complete Elliptic Integral of Second Kind.
!
  call elliptic_ea_test ( )
  call elliptic_ek_test ( )
  call elliptic_em_test ( )
!
!  Complete Elliptic Integral of Third Kind.
!
  call elliptic_pia_test ( )
  call elliptic_pik_test ( )
  call elliptic_pim_test ( )
!
!  Incomplete Elliptic Integral of First Kind.
!
  call elliptic_inc_fa_test ( )
  call elliptic_inc_fk_test ( )
  call elliptic_inc_fm_test ( )
!
!  Incomplete Elliptic Integral of Second Kind.
!
  call elliptic_inc_ea_test ( )
  call elliptic_inc_ek_test ( )
  call elliptic_inc_em_test ( )
!
!  Incomplete Elliptic Integral of Third Kind.
!
  call elliptic_inc_pia_test ( )
  call elliptic_inc_pik_test ( )
  call elliptic_inc_pim_test ( )
!
!  Jacobi Elliptic Functions.
!
  call jacobi_cn_test ( )
  call jacobi_dn_test ( )
  call jacobi_sn_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ELLIPTIC_INTEGRAL_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine jacobi_cn_test ( )

!*******************************************************************************
!
!! jacobi_cn_test tests jacobi_cn().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 November 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cn1
  real ( kind = 8 ) cn2
  real ( kind = 8 ) jacobi_cn
  real ( kind = 8 ) k
  real ( kind = 8 ) m
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) u

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'jacobi_cn_test:'
  write ( *, '(a)' ) '  jacobi_cn() evaluates the Jacobi elliptic function CN.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    U       M       Exact CN                CN(U,M)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call jacobi_cn_values ( n_data, u, a, k, m, cn1 )

    if ( n_data == 0 ) then
      exit
    end if

    cn2 = jacobi_cn ( u, m )

    write ( *, '(2f8.4,2g24.16)' ) u, m, cn1, cn2

  end do

  return
end
subroutine jacobi_dn_test ( )

!*******************************************************************************
!
!! jacobi_dn_test tests jacobi_dn().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 November 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) dn1
  real ( kind = 8 ) dn2
  real ( kind = 8 ) jacobi_dn
  real ( kind = 8 ) k
  real ( kind = 8 ) m
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) u

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'jacobi_dn_test:'
  write ( *, '(a)' ) '  jacobi_dn() evaluates the Jacobi elliptic function DN.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    U       M       Exact DN                DN(U,M)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call jacobi_dn_values ( n_data, u, a, k, m, dn1 )

    if ( n_data == 0 ) then
      exit
    end if

    dn2 = jacobi_dn ( u, m )

    write ( *, '(2f8.4,2g24.16)' ) u, m, dn1, dn2

  end do

  return
end
subroutine jacobi_sn_test ( )

!*******************************************************************************
!
!! jacobi_sn_test tests jacobi_sn().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 November 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) k
  real ( kind = 8 ) m
  real ( kind = 8 ) jacobi_sn
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sn1
  real ( kind = 8 ) sn2
  real ( kind = 8 ) u

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'jacobi_sn_test:'
  write ( *, '(a)' ) '  jacobi_sn() evaluates the Jacobi elliptic function SN.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    U       M       Exact SN                SN(U,M)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call jacobi_sn_values ( n_data, u, a, k, m, sn1 )

    if ( n_data == 0 ) then
      exit
    end if

    sn2 = jacobi_sn ( u, m )

    write ( *, '(2f8.4,2g24.16)' ) u, m, sn1, sn2

  end do

  return
end
subroutine rc_test ( )

!*****************************************************************************80
!
!! RC_TEST tests RC.
!
!  Discussion:
!
!    This driver tests the real ( kind = 8 ) function subroutine for the
!    integral RC(X,Y).  The first six sets of values of X and Y are
!    extreme points of the region of valid arguments defined by the
!    machine-dependent constants LOLIM and UPLIM.  The values of LOLIM,
!    UPLIM, X, Y, and ERRTOL (see comments in subroutine) may be used on
!    most machines but provide a severe test of robustness only on the
!    ibm 360/370 series.  The seventh set tests the failure exit.  The
!    next three sets are check values: RC(0,0.25) = RC(0.0625,0.125) = PI
!    and RC(2.25,2) = LN(2).  The remaining sets show the dependence on X
!    when Y = 1.  Fixing Y entails no loss here because RC is homogeneous.
!
!  Modified:
!
!    30 May 2018
!
!  Author:
!
!    Original FORTRAN77 version by Bille Carlson, Elaine Notis.
!    This FORTRAN90 version by John Burkardt.
!
  implicit none

  real ( kind = 8 ) eliptc
  real ( kind = 8 ) errtol
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) rc
  real ( kind = 8 ) x(43)
  real ( kind = 8 ) y(43)

  save x
  save y

  data x / &
   1.51D-78, &
   3.01D-78, &
   0.00D+00, &
   0.99D+75, &
   0.00D+00, &
   0.99D+75, &
   0.00D+00, &
   0.00D+00, &
   6.25D-02, &
   2.25D+00, &
   0.01D+00, &
   0.02D+00, &
   0.05D+00, &
   0.10D+00, &
   0.20D+00, &
   0.40D+00, &
   0.60D+00, &
   0.80D+00, &
   1.00D+00, &
   1.20D+00, &
   1.50D+00, &
   2.00D+00, &
   3.00D+00, &
   4.00D+00, &
   5.00D+00, &
   1.00D+01, &
   2.00D+01, &
   5.00D+01, &
   1.00D+02, &
   1.00D+03, &
   1.00D+04, &
   1.00D+05, &
   1.00D+06, &
   1.00D+07, &
   1.00D+08, &
   1.00D+09, &
   1.00D+10, &
   1.00D+12, &
   1.00D+15, &
   1.00D+20, &
   1.00D+30, &
   1.00D+40, &
   1.00D+50 /

  data y / &
   1.51D-78, &
   0.55D-78, &
   3.01D-78, &
   0.55D-78, &
   0.99D+75, &
   0.99D+75, &
   2.00D-78, &
   2.50D-01, &
   1.25D-01, &
   2.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00 /

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RC_TEST'
  write ( *, '(a)' ) '  RC evaluates the elementary integral RC(X,Y)'

  write ( *, '(a)' ) ''
  write ( *, '(14x,a,26x,a,25x,a)' ) 'X', 'Y', 'RC(X,Y)'
  write ( *, '(a)' ) ''

  errtol = 1.0D-3

  do i = 1, 43
    eliptc = rc ( x(i), y(i), errtol, ierr )
    if ( ierr == 0 ) then
      write ( *, '(2x,3d27.16)' ) x(i), y(i), eliptc
    else
      write ( *, '(2x,2d27.16,a)' ) x(i), y(i), '  ***Error***'
    end if
  end do

  return
end
subroutine rc_test2 ( )

!*****************************************************************************80
!
!! RC_TEST2 checks RC by examining special values.
!
!  Discussion:
!
!    This driver compares values of (LOG X)/(X-1) and ARCTAN(X)
!    calculated on one hand from the subroutine RC and on the other
!    from library LOG and ARCTAN routines.  to avoid over/underflows
!    for extreme values of X, we write (LOG X)/(X-1) = RC(Y,X/Y)/SQRT(Y),
!    where Y = (1+X)/2, and ARCTAN(X) = SQRT(X)*RC(Z,Z+X), where Z = 1/X.
!
!  Modified:
!
!    30 May 2018
!
!  Author:
!
!    Original FORTRAN77 version by Bille Carlson, Elaine Notis.
!    This FORTRAN90 version by John Burkardt.
!
  implicit none

  real ( kind = 8 ) errtol
  integer ( kind = 4 ) i
  real ( kind = 8 ) ibmarc
  real ( kind = 8 ) ibmlog
  integer ( kind = 4 ) ierr
  integer ( kind = 4 ) ipower
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  real ( kind = 8 ) myarc
  real ( kind = 8 ) mylog
  real ( kind = 8 ) rc
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) x_vec(13)
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  save x_vec

  data x_vec / &
   1.0D-75, &
   1.0D-15, &
   1.0D-03, &
   1.0D-01, &
   2.0D-01, &
   5.0D-01, &
   1.0D+00, &
   2.0D+00, &
   5.0D+00, &
   1.0D+01, &
   1.0D+03, &
   1.0D+15, &
   1.0D+75 /

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RC_TEST2'
  write ( *, '(a)' ) '  Compare LOG(X)/(X-1) and ARCTAN(X) with'
  write ( *, '(a)' ) '  values based on RC.'

  write ( *, '(a)' ) ''
  write ( *, '(5x,a,15x,a,19x,a)' ) 'X', 'From LOG', 'From RC'
  write ( *, '(a)' ) ''

  errtol = 1.0D-3

  do j = 1, 10
    x = 0.2D0 * dble ( j )
    y = ( 1.0D0 + x ) / 2.0D0
    v = x / y
    mylog = rc ( y, v, errtol, ierr ) / sqrt ( y )
    if ( j == 5 ) then
      write ( *, '(d9.1,5x,a,d27.16)' ) x, '**** ZERO DIVIDE *****', mylog
    else
      ibmlog = log ( x ) / ( x - 1.0D0 )
      write ( *, '(d9.1,5x,2d27.16)' ) x, ibmlog, mylog
    end if
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Extreme values of X'
  write ( *, '(a)' ) ''
  write ( *, '(5x,a,15x,a,19x,a)' ) 'X', 'From LOG', 'From RC'
  write ( *, '(a)' ) ''

  do i = 1, 16
    ipower = - 75 + 10 * ( i - 1 )
    x = 10.D0 ** ipower
    y = ( 1.0D0 + x ) / 2.D0
    v = x / y
    mylog = rc ( y, v, errtol, ierr ) / sqrt ( y )
    ibmlog = log ( x ) / ( x - 1.0D0 )
    write ( *, '(d9.1,2d27.16)' ) x, ibmlog, mylog
  end do

  write ( *, '(a)' ) ''
  write ( *, '(5xa,14x,a,17x,a)' ) 'X','From ARCTAN', 'From RC'
  write ( *, '(a)' ) ''

  do m = 1, 13
    x = x_vec(m)
    z = 1.0D0 / x
    w = z + x
    myarc = sqrt ( x ) * rc ( z, w, errtol, ierr )
    ibmarc = atan ( x )
    write ( *, '(d9.1,2d27.16)' ) x, ibmarc, myarc
  end do

  return
end
subroutine rd_test ( )

!*****************************************************************************80
!
!! RD_TEST tests RD.
!
!  Discussion:
!
!    This driver tests the real ( kind = 8 ) function subroutine for the
!    integral RD(X,Y,Z), which is symmetric in X and Y.  The first
!    twelve sets of values of X, Y, Z are extreme points of the region of
!    valid arguments defined by the machine-dependent constants LOLIM
!    and UPLIM.  The values of LOLIM, UPLIM, X, Y, Z, and ERRTOL (see
!    comments in subroutine) may be used on most machines but provide a
!    severe test of robustness only on the ibm 360/370 series.  The
!    thirteenth set tests the failure exit.  The fourteenth set is a
!    check value: RD(0,2,1) = 3B = 3(PI)/4A, where A and B are the
!    lemniscate constants.  The remaining sets show the dependence
!    on Z when Y = 1 (no loss of generality because of homogeneity)
!    and X = 0.5 (midway between the complete case X = 0 and the
!    degenerate case X = Y).
!
!  Modified:
!
!    27 May 2018
!
!  Author:
!
!    Original FORTRAN77 version by Bille Carlson, Elaine Notis.
!    This FORTRAN90 version by John Burkardt.
!
  implicit none

  real ( kind = 8 ) eliptc
  real ( kind = 8 ) errtol
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) rd
  real ( kind = 8 ) x(27)
  real ( kind = 8 ) y(27)
  real ( kind = 8 ) z(27)

  save x
  save y
  save z

  data x / &
   0.00D+00, &
   0.55D-78, &
   0.00D+00, &
   0.55D-78, &
   0.00D+00, &
   0.55D-78, &
   0.00D+00, &
   0.55D-78, &
   3.01D-51, &
   3.01D-51, &
   0.99D+48, &
   0.99D+48, &
   0.00D+00, &
   0.00D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00 /

  data y / &
   6.01D-51, &
   6.01D-51, &
   6.01D-51, &
   6.01D-51, &
   0.99D+48, &
   0.99D+48, &
   0.99D+48, &
   0.99D+48, &
   3.01D-51, &
   3.01D-51, &
   0.99D+48, &
   0.99D+48, &
   3.01D-51, &
   2.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00 /

  data z / &
   6.01D-51, &
   6.01D-51, &
   0.99D+48, &
   0.99D+48, &
   6.01D-51, &
   6.01D-51, &
   0.99D+48, &
   0.99D+48, &
   6.01D-51, &
   0.99D+48, &
   6.01D-51, &
   0.99D+48, &
   1.00D+00, &
   1.00D+00, &
   1.00D-10, &
   1.00D-05, &
   1.00D-02, &
   1.00D-01, &
   2.00D-01, &
   5.00D-01, &
   1.00D+00, &
   2.00D+00, &
   5.00D+00, &
   1.00D+01, &
   1.00D+02, &
   1.00D+05, &
   1.00D+10 /

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RD_TEST'
  write ( *, '(a)' ) '  RD evaluates the Carlson elliptic integral'
  write ( *, '(a)' ) '  of the second kind, RD(X,Y,Z)'
  write ( *, '(a)' ) ''
  write ( *, '(15x,a,26x,a,26x,a,25x,a)' ) 'X', 'Y', 'Z', 'RD(X,Y,Z)'
  write ( *, '(a)' ) ''

  errtol = 1.0d-3

  do i = 1, 27
    eliptc = rd ( x(i), y(i), z(i), errtol, ierr )
    if ( ierr == 0 ) then
      write ( *, '(4d27.16)' ) x(i), y(i), z(i), eliptc
    else
      write ( *, '(3d27.16,a)' ) x(i), y(i), z(i), '  ***Error***'
    end if
  end do

  return
end
subroutine rf_test ( )

!*****************************************************************************80
!
!! RF_TEST tests RF.
!
!  Discussion:
!
!    This driver tests the real ( kind = 8 ) function subroutine for the
!    integral RF(X,Y,Z), which is symmetric in X, Y, Z.  The first nine
!    sets of values of X, Y, Z are extreme points of the region of valid
!    arguments defined by the machine-dependent constants LOLIM and
!    UPLIM.  The values of LOLIM, UPLIM, X, Y, Z, and ERRTOL (see
!    comments in subroutine) may be used on most machines but provide a
!    severe test of robustness only on the ibm 360/370 series.  The
!    tenth set tests the failure exit.  The eleventh set is a check
!    value: RF(0,1,2) = A, where A is the first lemniscate constant.
!    The remaining sets show the dependence on Z when Y = 1 (no loss of
!    generality because of homogeneity) and X = 0.5 (midway between the
!    complete case X = 0 and the degenerate case X = Y).
!
!  Modified:
!
!    28 May 2018
!
!  Author:
!
!    Original FORTRAN77 version by Bille Carlson, Elaine Notis.
!    This FORTRAN90 version by John Burkardt.
!
  implicit none

  real ( kind = 8 ) eliptc
  real ( kind = 8 ) errtol
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) rf
  real ( kind = 8 ) x(55)
  real ( kind = 8 ) y(55)
  real ( kind = 8 ) z(55)

  save x
  save y
  save z

  data x / &
   1.51D-78, &
   1.51D-78, &
   0.00D+00, &
   0.00D+00, &
   0.00D+00, &
   0.99D+75, &
   0.55D-78, &
   0.55D-78, &
   0.55D-78, &
   0.00D+00, &
   0.00D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00 /

  data y / &
   1.51D-78, &
   1.51D-78, &
   3.01D-78, &
   3.01D-78, &
   0.99D+75, &
   0.99D+75, &
   3.01D-78, &
   3.01D-78, &
   0.99D+75, &
   2.00D-78, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00 /

  data z / &
   1.51D-78, &
   0.99D+75, &
   3.01D-78, &
   0.99D+75, &
   0.99D+75, &
   0.99D+75, &
   3.01D-78, &
   0.99D+75, &
   0.99D+75, &
   1.00D+00, &
   2.00D+00, &
   1.00D+00, &
   1.10D+00, &
   1.20D+00, &
   1.30D+00, &
   1.40D+00, &
   1.50D+00, &
   1.60D+00, &
   1.70D+00, &
   1.80D+00, &
   1.90D+00, &
   2.00D+00, &
   2.20D+00, &
   2.40D+00, &
   2.60D+00, &
   2.80D+00, &
   3.00D+00, &
   3.50D+00, &
   4.00D+00, &
   4.50D+00, &
   5.00D+00, &
   6.00D+00, &
   7.00D+00, &
   8.00D+00, &
   9.00D+00, &
   1.00D+01, &
   2.00D+01, &
   3.00D+01, &
   4.00D+01, &
   5.00D+01, &
   1.00D+02, &
   2.00D+02, &
   5.00D+02, &
   1.00D+03, &
   1.00D+04, &
   1.00D+05, &
   1.00D+06, &
   1.00D+08, &
   1.00D+10, &
   1.00D+12, &
   1.00D+15, &
   1.00D+20, &
   1.00D+30, &
   1.00D+40, &
   1.00D+50 /

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RF_TEST'
  write ( *, '(a)' ) '  RF evaluates the Carlson elliptic integral'
  write ( *, '(a)' ) '  of the first kind, RF(X,Y,Z)'
  write ( *, '(a)' ) ''
  write ( *, '(15x,a,26x,a,26x,a,25x,a)' ) 'X', 'Y', 'Z', 'RF(X,Y,Z)'
  write ( *, '(a)' )

  errtol = 1.0D-3

  do i = 1, 55
    eliptc = rf ( x(i), y(i), z(i), errtol, ierr )
    if (ierr == 0 ) then
      write ( *, '(4d27.16)' ) x(i), y(i), z(i), eliptc
    else
      write ( *, '(3d27.16,a)' ) x(i), y(i), z(i), '  ***Error***'
    end if
  end do

  return
end
subroutine rj_test ( )

!*****************************************************************************80
!
!! RJ_TEST tests RJ.
!
!  Discussion:
!
!    This driver tests the real ( kind = 8 ) function subroutine for the
!    integral Rj(X,Y,Z,P), which is symmetric in X, Y, Z.  The first
!    twenty sets of values of X, Y, Z, P are extreme points of the region
!    of valid arguments defined by the machine-dependent constants
!    LOLIM and UPLIM.  The values of LOLIM, UPLIM, X, Y, Z, P, and
!    ERRTOL (see comments in subroutine) may be used on most machines
!    but provide a severe test of robustness only on the ibm 360/370
!    series.  The twenty-first set tests the failure exit.  The twenty-
!    second set is a check value:
!      RJ(2,3,4,5) = 0.1429757966715675383323308.
!    The remaining sets show the dependence on Z and P
!    when Y = 1 (no loss of generality because of homogeneity) and
!    X = 0.5 (midway between the complete case x = 0 and the degenerate
!    case X = Y).
!
!  Modified:
!
!    28 May 2018
!
!  Author:
!
!    Original FORTRAN77 version by Bille Carlson, Elaine Notis.
!    This FORTRAN90 version by John Burkardt.
!
  implicit none

  real ( kind = 8 ) eliptc
  real ( kind = 8 ) errtol
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) p(42)
  real ( kind = 8 ) rj
  real ( kind = 8 ) x(42)
  real ( kind = 8 ) y(42)
  real ( kind = 8 ) z(42)

  save p
  save x
  save y
  save z

  data p / &
   2.01D-26, &
   2.01D-26, &
   2.01D-26, &
   2.01D-26, &
   2.01D-26, &
   2.01D-26, &
   2.01D-26, &
   2.01D-26, &
   2.01D-26, &
   2.01D-26, &
   2.99D+24, &
   2.99D+24, &
   2.99D+24, &
   2.99D+24, &
   2.99D+24, &
   2.99D+24, &
   2.99D+24, &
   2.99D+24, &
   2.99D+24, &
   2.99D+24, &
   1.00D+00, &
   5.00D+00, &
   0.25D+00, &
   0.75D+00, &
   1.00D+00, &
   2.00D+00, &
   0.25D+00, &
   0.75D+00, &
   1.50D+00, &
   4.00D+00, &
   0.25D+00, &
   0.75D+00, &
   3.00D+00, &
   1.00D+01, &
   0.25D+00, &
   0.75D+00, &
   5.00D+00, &
   2.00D+01, &
   0.25D+00, &
   0.75D+00, &
   5.00D+01, &
   2.00D+02 /

  data x / &
   1.01D-26, &
   1.01D-26, &
   0.00D+00, &
   0.00D+00, &
   0.00D+00, &
   2.99D+24, &
   0.55D-78, &
   0.55D-78, &
   0.55D-78, &
   2.01D-26, &
   1.01D-26, &
   1.01D-26, &
   0.00D+00, &
   0.00D+00, &
   0.00D+00, &
   2.99D+24, &
   0.55D-78, &
   0.55D-78, &
   0.55D-78, &
   2.01D-26, &
   0.00D+00, &
   2.00D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00, &
   0.50D+00 /

  data y / &
   1.01D-26, &
   1.01D-26, &
   2.01D-26, &
   2.01D-26, &
   2.99D+24, &
   2.99D+24, &
   2.01D-26, &
   2.01D-26, &
   2.99D+24, &
   2.01D-26, &
   1.01D-26, &
   1.01D-26, &
   2.01D-26, &
   2.01D-26, &
   2.99D+24, &
   2.99D+24, &
   2.01D-26, &
   2.01D-26, &
   2.99D+24, &
   2.01D-26, &
   1.90D-26, &
   3.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00 /

  data z / &
   1.01D-26, &
   2.99D+24, &
   2.01D-26, &
   2.99D+24, &
   2.99D+24, &
   2.99D+24, &
   2.01D-26, &
   2.99D+24, &
   2.99D+24, &
   2.01D-26, &
   1.01D-26, &
   2.99D+24, &
   2.01D-26, &
   2.99D+24, &
   2.99D+24, &
   2.99D+24, &
   2.01D-26, &
   2.99D+24, &
   2.99D+24, &
   2.01D-26, &
   1.90D-26, &
   4.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   1.00D+00, &
   2.00D+00, &
   2.00D+00, &
   2.00D+00, &
   2.00D+00, &
   5.00D+00, &
   5.00D+00, &
   5.00D+00, &
   5.00D+00, &
   1.00D+01, &
   1.00D+01, &
   1.00D+01, &
   1.00D+01, &
   1.00D+02, &
   1.00D+02, &
   1.00D+02, &
   1.00D+02 /

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RJ_TEST'
  write ( *, '(a)' ) '  RJ evaluates the Carlson elliptic integral'
  write ( *, '(a)' ) '  of the third kind, RJ(X,Y,Z,P)'
  write ( *, '(a)' ) ''
  write ( *, '(15x,a,26x,a,26x,a,26x,a,25x,a)' ) &
    'X', 'Y', 'Z', 'P', 'RJ(X,Y,Z,P)'
  write ( *, '(a)' )

  errtol = 1.0D-3

  do i = 1, 42
    eliptc = rj ( x(i), y(i), z(i), p(i), errtol, ierr )
    if (ierr == 0 ) then
      write ( *, '(5d27.16)' ) x(i), y(i), z(i), p(i), eliptc
    else
      write ( *, '(4d27.16,a)' ) x(i), y(i), z(i), p(i), '  ***Error***'
    end if
  end do

  return
end
subroutine elliptic_ea_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_EA_TEST tests ELLIPTIC_EA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) elliptic_ea
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_EA_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_EA returns values of '
  write ( *, '(a)' ) '  the complete elliptic integral of the'
  write ( *, '(a)' ) '  second kind, with parameter angle A.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      A       E(A)          E(A)'
  write ( *, '(a)' ) '          Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do
    call elliptic_ea_values ( n_data, a, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = elliptic_ea ( a )

    write ( *, '(2x,f14.6,2x,g24.16,2x,g24.16)' ) a, fx, fx2

  end do

  return
end
subroutine elliptic_ek_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_EK_TEST tests ELLIPTIC_EK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) elliptic_ek
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  real ( kind = 8 ) k
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_EK_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_EK returns values of '
  write ( *, '(a)' ) '  the complete elliptic integral of the'
  write ( *, '(a)' ) '  second kind, with parameter K.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      K       E(K)          E(K)'
  write ( *, '(a)' ) '          Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call elliptic_ek_values ( n_data, k, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = elliptic_ek ( k )

    write ( *, '(2x,f14.6,2x,g24.16,2x,g24.16)' ) k, fx, fx2

  end do

  return
end
subroutine elliptic_em_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_EM_TEST tests ELLIPTIC_EM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) elliptic_em
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  real ( kind = 8 ) m
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_EM_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_EM returns values of '
  write ( *, '(a)' ) '  the complete elliptic integral of the'
  write ( *, '(a)' ) '  second kind, with parameter M.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      M       E(M)          E(M)'
  write ( *, '(a)' ) '          Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call elliptic_em_values ( n_data, m, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = elliptic_em ( m )

    write ( *, '(2x,f14.6,2x,g24.16,2x,g24.16)' ) m, fx, fx2

  end do

  return
end
subroutine elliptic_fa_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_FA_TEST tests ELLIPTIC_FA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) elliptic_fa
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_FA_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_FA returns values of '
  write ( *, '(a)' ) '  the complete elliptic integral of the first'
  write ( *, '(a)' ) '  kind, with parameter angle A.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      A       F(A)          F(A)'
  write ( *, '(a)' ) '          Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call elliptic_fa_values ( n_data, a, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = elliptic_fa ( a )

    write ( *, '(2x,f14.6,2x,g24.16,2x,g24.16)' ) a, fx, fx2

  end do

  return
end
subroutine elliptic_fk_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_FK_TEST tests ELLIPTIC_FK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) elliptic_fk
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  real ( kind = 8 ) k
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_FK_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_FK returns values of '
  write ( *, '(a)' ) '  the complete elliptic integral of the first'
  write ( *, '(a)' ) '  kind, with parameter K.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      K       F(K)          F(K)'
  write ( *, '(a)' ) '          Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call elliptic_fk_values ( n_data, k, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = elliptic_fk ( k )

    write ( *, '(2x,f14.6,2x,g24.16,2x,g24.16)' ) k, fx, fx2

  end do

  return
end
subroutine elliptic_fm_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_FM_TEST tests ELLIPTIC_FM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) elliptic_fm
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  real ( kind = 8 ) m
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_FM_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_FM returns values of '
  write ( *, '(a)' ) '  the complete elliptic integral of the first'
  write ( *, '(a)' ) '  kind, with parameter M.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      M       F(M)          F(M)'
  write ( *, '(a)' ) '          Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call elliptic_fm_values ( n_data, m, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = elliptic_fm ( m )

    write ( *, '(2x,f14.6,2x,g24.16,2x,g24.16)' ) m, fx, fx2

  end do

  return
end
subroutine elliptic_pia_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_PIA_TEST tests ELLIPTIC_PIA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) elliptic_pia
  real ( kind = 8 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) pia1
  real ( kind = 8 ) pia2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_PIA_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_PIA returns values of '
  write ( *, '(a)' ) '  the complete elliptic integral of the'
  write ( *, '(a)' ) '  third kind, with parameter angle A.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      N     A   Pi(N,A)      Pi(N,A)'
  write ( *, '(a)' ) '                Tabulated    Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call elliptic_pia_values ( n_data, n, a, pia1 )

    if ( n_data == 0 ) then
      exit
    end if

    pia2 = elliptic_pia ( n, a )

    write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) n, a, pia1, pia2

  end do

  return
end
subroutine elliptic_pik_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_PIK_TEST tests ELLIPTIC_PIK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) elliptic_pik
  real ( kind = 8 ) k
  real ( kind = 8 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) pik1
  real ( kind = 8 ) pik2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_PIK_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_PIK returns values of '
  write ( *, '(a)' ) '  the complete elliptic integral of the'
  write ( *, '(a)' ) '  third kind, with parameter K.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      N     K    Pi(N,K)           Pi(N,K)'
  write ( *, '(a)' ) '                 Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call elliptic_pik_values ( n_data, n, k, pik1 )

    if ( n_data == 0 ) then
      exit
    end if

    pik2 = elliptic_pik ( n, k )

    write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) n, k, pik1, pik2

  end do

  return
end
subroutine elliptic_pim_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_PIM_TEST tests ELLIPTIC_PIM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 May 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) elliptic_pim
  real ( kind = 8 ) m
  real ( kind = 8 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) pim1
  real ( kind = 8 ) pim2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_PIM_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_PIM returns values of '
  write ( *, '(a)' ) '  the complete elliptic integral of the'
  write ( *, '(a)' ) '  third kind, with parameter modulus M.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      N     M    Pi(N,M)           Pi(N,M)'
  write ( *, '(a)' ) '                 Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call elliptic_pim_values ( n_data, n, m, pim1 )

    if ( n_data == 0 ) then
      exit
    end if

    pim2 = elliptic_pim ( n, m )

    write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) n, m, pim1, pim2

  end do

  return
end
subroutine elliptic_inc_ea_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_INC_EA_TEST tests ELLIPTIC_INC_EA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) ea1
  real ( kind = 8 ) ea2
  real ( kind = 8 ) elliptic_inc_ea
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) phi

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_INC_EA_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_INC_EA returns values of '
  write ( *, '(a)' ) '  the incomplete elliptic integral of the'
  write ( *, '(a)' ) '  second kind, with parameters PHI, A.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      PHI             A    E(PHI,A)          E(PHI,A)'
  write ( *, '(a)' ) '                           Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do
    call elliptic_inc_ea_values ( n_data, phi, a, ea1 )

    if ( n_data == 0 ) then
      exit
    end if

    ea2 = elliptic_inc_ea ( phi, a )

    write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) phi, a, ea1, ea2

  end do

  return
end
subroutine elliptic_inc_ek_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_INC_EK_TEST tests ELLIPTIC_INC_EK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) ek1
  real ( kind = 8 ) ek2
  real ( kind = 8 ) elliptic_inc_ek
  real ( kind = 8 ) k
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) phi

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_INC_EK_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_INC_EK returns values of '
  write ( *, '(a)' ) '  the incomplete elliptic integral of the'
  write ( *, '(a)' ) '  second kind, with parameters PHI, K.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      PHI             K    E(PHI,K)          E(PHI,K)'
  write ( *, '(a)' ) '                           Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do
    call elliptic_inc_ek_values ( n_data, phi, k, ek1 )

    if ( n_data == 0 ) then
      exit
    end if

    ek2 = elliptic_inc_ek ( phi, k )

    write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) phi, k, ek1, ek2

  end do

  return
end
subroutine elliptic_inc_em_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_INC_EM_TEST tests ELLIPTIC_INC_EM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) em1
  real ( kind = 8 ) em2
  real ( kind = 8 ) elliptic_inc_em
  real ( kind = 8 ) m
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) phi

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_INC_EM_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_INC_EM returns values of '
  write ( *, '(a)' ) '  the incomplete elliptic integral of the'
  write ( *, '(a)' ) '  second kind, with parameters PHI, M.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      PHI             M    E(PHI,M)          E(PHI,M)'
  write ( *, '(a)' ) '                           Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do
    call elliptic_inc_em_values ( n_data, phi, m, em1 )

    if ( n_data == 0 ) then
      exit
    end if

    em2 = elliptic_inc_em ( phi, m )

    write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) phi, m, em1, em2

  end do

  return
end
subroutine elliptic_inc_fa_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_INC_FA_TEST tests ELLIPTIC_INC_FA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) fa1
  real ( kind = 8 ) fa2
  real ( kind = 8 ) elliptic_inc_fa
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) phi

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_INC_FA_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_INC_FA returns values of '
  write ( *, '(a)' ) '  the incomplete elliptic integral of the'
  write ( *, '(a)' ) '  first kind, with parameters PHI, A.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      PHI             A    F(PHI,A)          F(PHI,A)'
  write ( *, '(a)' ) '                           Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do
    call elliptic_inc_fa_values ( n_data, phi, a, fa1 )

    if ( n_data == 0 ) then
      exit
    end if

    fa2 = elliptic_inc_fa ( phi, a )

    write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) phi, a, fa1, fa2

  end do

  return
end
subroutine elliptic_inc_fk_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_INC_FK_TEST tests ELLIPTIC_INC_FK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fk1
  real ( kind = 8 ) fk2
  real ( kind = 8 ) elliptic_inc_fk
  real ( kind = 8 ) k
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) phi

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_INC_FK_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_INC_FK returns values of '
  write ( *, '(a)' ) '  the incomplete elliptic integral of the'
  write ( *, '(a)' ) '  first kind, with parameters PHI, K.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      PHI             K    F(PHI,K)          F(PHI,K)'
  write ( *, '(a)' ) '                           Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do
    call elliptic_inc_fk_values ( n_data, phi, k, fk1 )

    if ( n_data == 0 ) then
      exit
    end if

    fk2 = elliptic_inc_fk ( phi, k )

    write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) phi, k, fk1, fk2

  end do

  return
end
subroutine elliptic_inc_fm_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_INC_FM_TEST tests ELLIPTIC_INC_FM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fm1
  real ( kind = 8 ) fm2
  real ( kind = 8 ) elliptic_inc_fm
  real ( kind = 8 ) m
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) phi

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_INC_FM_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_INC_FM returns values of '
  write ( *, '(a)' ) '  the incomplete elliptic integral of the'
  write ( *, '(a)' ) '  first kind, with parameters PHI, M.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      PHI             M    F(PHI,M)          F(PHI,M)'
  write ( *, '(a)' ) '                           Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do
    call elliptic_inc_fm_values ( n_data, phi, m, fm1 )

    if ( n_data == 0 ) then
      exit
    end if

    fm2 = elliptic_inc_fm ( phi, m )

    write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) phi, m, fm1, fm2

  end do

  return
end
subroutine elliptic_inc_pia_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_INC_PIA_TEST tests ELLIPTIC_INC_PIA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) elliptic_inc_pia
  real ( kind = 8 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) phi
  real ( kind = 8 ) pia1
  real ( kind = 8 ) pia2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_INC_PIA_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_INC_PIA returns values of '
  write ( *, '(a)' ) '  the incomplete elliptic integral of the'
  write ( *, '(a)' ) '  third kind, with parameters PHI, N, A.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      PHI             N               A    PI(PHI,N,A)       PI(PHI,N,A)'
  write ( *, '(a)' ) '                                           Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do
    call elliptic_inc_pia_values ( n_data, phi, n, a, pia1 )

    if ( n_data == 0 ) then
      exit
    end if

    pia2 = elliptic_inc_pia ( phi, n, a )

    write ( *, '(2x,f14.6,2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) phi, n, a, pia1, pia2

  end do

  return
end
subroutine elliptic_inc_pik_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_INC_PIK_TEST tests ELLIPTIC_INC_PIK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) elliptic_inc_pik
  real ( kind = 8 ) k
  real ( kind = 8 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) phi
  real ( kind = 8 ) pik1
  real ( kind = 8 ) pik2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_INC_PIK_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_INC_PIK returns values of '
  write ( *, '(a)' ) '  the incomplete elliptic integral of the'
  write ( *, '(a)' ) '  third kind, with parameters PHI, N, K.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      PHI             N               K    Pi(PHI,N,K)       Pi(PHI,N,K)'
  write ( *, '(a)' ) '                                           Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do
    call elliptic_inc_pik_values ( n_data, phi, n, k, pik1 )

    if ( n_data == 0 ) then
      exit
    end if

    pik2 = elliptic_inc_pik ( phi, n, k )

    write ( *, '(2x,f14.6,2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) phi, n, k, pik1, pik2

  end do

  return
end
subroutine elliptic_inc_pim_test ( )

!*****************************************************************************80
!
!! ELLIPTIC_INC_PIM_TEST tests ELLIPTIC_INC_PIM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) elliptic_inc_pim
  real ( kind = 8 ) m
  real ( kind = 8 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) phi
  real ( kind = 8 ) pim1
  real ( kind = 8 ) pim2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ELLIPTIC_INC_PIM_TEST:'
  write ( *, '(a)' ) '  ELLIPTIC_INC_PIM returns values of '
  write ( *, '(a)' ) '  the incomplete elliptic integral of the'
  write ( *, '(a)' ) '  third kind, with parameters PHI, N, M.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      PHI             N               M    PI(PHI,N,M)       PI(PHI,N,M)'
  write ( *, '(a)' ) '                                           Tabulated         Calculated'
  write ( *, '(a)' ) ' '

  n_data = 0

  do
    call elliptic_inc_pim_values ( n_data, phi, n, m, pim1 )

    if ( n_data == 0 ) then
      exit
    end if

    pim2 = elliptic_inc_pim ( phi, n, m )

    write ( *, '(2x,f14.6,2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) &
      phi, n, m, pim1, pim2

  end do

  return
end
