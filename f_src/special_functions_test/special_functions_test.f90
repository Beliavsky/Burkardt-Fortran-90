program main

!*****************************************************************************80
!
!! MAIN is the main program for SPECIAL_FUNCTIONS_TEST.
!
!  Discussion:
!
!    SPECIAL_FUNCTIONS_TEST tests the SPECIAL_FUNCTIONS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2018
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!      
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPECIAL_FUNCTIONS_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SPECIAL_FUNCTIONS library.'

  call airya_test ( )
  call beta_test ( )
  call cisia_test ( )
  call cisib_test ( )
  call cjy01_test ( )
  call comelp_test ( )
  call hygfx_test ( )
  call sphj_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPECIAL_FUNCTIONS_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine airya_test ( )

!*****************************************************************************80
!
!! AIRYA_TEST tests AIRYA.
!
!  Discussion:
!
!    This program computes Airy functions and their 
!    derivatives using subroutine AIRYA.
!
!  Example:
!
!     x     Ai(x)        Bi(x)        Ai'(x)       Bi'(x)
!    ----------------------------------------------------------------
!     0   .35502805D+00  .61492663D+00 -.25881940D+00  .44828836D+00
!    10   .11047533D-09  .45564115D+09 -.35206337D-09  .14292361D+10
!    20   .16916729D-26  .21037650D+26 -.75863916D-26  .93818393D+26
!    30   .32082176D-48  .90572885D+47 -.17598766D-47  .49533045D+48
!
!     x     Ai(-x)       Bi(-x)       Ai'(-x)      Bi'(-x)
!    ----------------------------------------------------------------
!     0     .35502805    .61492663     -.25881940      .44828836
!    10     .04024124   -.31467983      .99626504      .11941411
!    20    -.17640613   -.20013931      .89286286     -.79142903
!    30    -.08796819   -.22444694     1.22862060     -.48369473
!
!  Modified:
!
!    07 October 2013
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!      
  implicit none

  integer, parameter :: test_num = 4

  real ( kind = 8 ) ad
  real ( kind = 8 ) ai
  real ( kind = 8 ) bd
  real ( kind = 8 ) bi
  integer ( kind = 4 ) i
  real ( kind = 8 ) x
  real ( kind = 8 ), dimension ( test_num ) :: x_test = (/ &
    0.0D+00, 10.0D+00, 20.0D+00, 30.0D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'AIRYA_TEST'
  write ( *, '(a)' ) '  Test AIRYA'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    x      Ai(x)         Bi(x)' // &
    '         Ai''(x)        Bi''(x)'
  write ( *, '(a)' ) ' '

  do i = 1, test_num
    x = x_test(i)
    call airya ( x, ai, bi, ad, bd )
    write ( *, '(1x,f5.1,4g16.8)' ) x, ai, bi, ad, bd
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    x     Ai(-x)        Bi(-x)' // &
    '        Ai''(-x)       Bi''(-x)'
  write ( *, '(a)' ) ' '

  do i = 1, test_num
    x = x_test(i)
    call airya ( -x, ai, bi, ad, bd )
    write ( *, '(1x,f5.1,4g16.8)' ) x, ai, bi, ad, bd
  end do

  return
end
subroutine beta_test ( )

!*****************************************************************************80
!
!! BETA_TEST tests BETA.
!
!  Example:
!
!                 p       q           B(p,q)
!               ---------------------------------
!                1.5     2.0     .2666666667D+00
!                2.5     2.0     .1142857143D+00
!                1.5     3.0     .1523809524D+00
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 March 2012
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!      
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 3

  real ( kind = 8 ) bt
  real ( kind = 8 ) p
  real ( kind = 8 ), dimension ( test_num ) :: p_test = (/ &
    1.5D+00, 2.5D+00, 1.5D+00 /)
  real ( kind = 8 ) q
  real ( kind = 8 ), dimension ( test_num ) :: q_test = (/ &
    2.0D+00, 2.0D+00, 3.0D+00 /)
  integer test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BETA_TEST:'
  write ( *, '(a)' ) '  Test BETA.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    p       q           B(p,q)'
  write ( *, '(a)' ) '  ---------------------------------'

  do test = 1, test_num

    p = p_test(test)
    q = q_test(test)

    call beta ( p, q, bt )
    write ( *, '(2x,f5.1,3x,f5.1,d20.10)' ) p, q, bt

  end do

  return
end
subroutine cisia_test ( )

!*****************************************************************************80
!
!! CISIA_TEST tests CISIA.
!
!  Example:
!
!      x        Ci(x)           Si(x)
!    ------------------------------------
!     0.0    - oo                 0
!     5.0    -.190030D+00      1.549931
!    10.0    -.454563D-01      1.658348
!    20.0     .444201D-01      1.548241
!    30.0    -.330326D-01      1.566757
!    40.0     .190201D-01      1.586985
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!      
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 6

  real ( kind = 8 ) ci
  real ( kind = 8 ) si
  integer ( kind = 4 ) test
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( test_num ) :: x_test = (/ &
    0.0D+00, 5.0D+00, 10.0D+00, 20.0D+00, 30.0D+00, 40.0D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CISIA_TEST'
  write ( *, '(a)' ) '  CISIA computes the cosine and sine integrals.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   x        ci(x)           si(x)'
  write ( *, '(a)' ) '------------------------------------'

  do test = 1, test_num

    x = x_test(test)

    call cisia ( x, ci, si )

    write ( *, '(1x,f5.1,g16.8,g16.8)' ) x, ci, si

  end do

  return
end
subroutine cisib_test ( )

!*****************************************************************************80
!
!! CISIB_TEST tests CISIB.
!
!  Example:
!
!      x        Ci(x)           Si(x)
!    ------------------------------------
!     0.0    - oo                 0
!     5.0    -.190030D+00      1.549931
!    10.0    -.454563D-01      1.658348
!    20.0     .444201D-01      1.548241
!    30.0    -.330326D-01      1.566757
!    40.0     .190201D-01      1.586985
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2012
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!      
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 6

  real ( kind = 8 ) ci
  real ( kind = 8 ) si
  integer ( kind = 4 ) test
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( test_num ) :: x_test = (/ &
    0.0D+00, 5.0D+00, 10.0D+00, 20.0D+00, 30.0D+00, 40.0D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CISIB_TEST'
  write ( *, '(a)' ) '  CISIB computes the cosine and sine integrals.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   x        ci(x)           si(x)'
  write ( *, '(a)' ) '------------------------------------'

  do test = 1, test_num

    x = x_test(test)

    call cisib ( x, ci, si )

    write ( *, '(1x,f5.1,g16.8,g16.8)' ) x, ci, si

  end do

  return
end
subroutine cjy01_test ( )

!*****************************************************************************80
!
!! CJY01_TEST tests CJY01.
!
!  Modified:
!
!    06 October 2015
!
!       Purpose: This program computes Bessel functions J0(z), J1(z),
!                Y0(z), Y1(z), and their derivatives for a complex
!                argument using subroutine CJY01
!       Input :  z --- Complex argument
!       Output:  CBJ0 --- J0(z)
!                CDJ0 --- J0'(z)
!                CBJ1 --- J1(z)
!                CDJ1 --- J1'(z)
!                CBY0 --- Y0(z)
!                CDY0 --- Y0'(z)
!                CBY1 --- Y1(z)
!                CDY1 --- Y1'(z)
!       Example: z =  4.0 + i  2.0
!
!     n     Re[Jn(z)]       Im[Jn(z)]       Re[Jn'(z)]      Im[Jn'(z)]
!   --------------------------------------------------------------------
!     0  -.13787022D+01   .39054236D+00   .50735255D+00   .12263041D+01
!     1  -.50735255D+00  -.12263041D+01  -.11546013D+01   .58506793D+00
!
!     n     Re[Yn(z)]       Im[Yn(z)]       Re[Yn'(z)]      Im[Yn'(z)]
!   --------------------------------------------------------------------
!     0  -.38145893D+00  -.13291649D+01  -.12793101D+01   .51220420D+00
!     1   .12793101D+01  -.51220420D+00  -.58610052D+00  -.10987930D+01
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!      
  implicit none

  complex ( kind = 8 ) cbj0
  complex ( kind = 8 ) cbj1
  complex ( kind = 8 ) cby0
  complex ( kind = 8 ) cby1
  complex ( kind = 8 ) cdj0
  complex ( kind = 8 ) cdj1
  complex ( kind = 8 ) cdy0
  complex ( kind = 8 ) cdy1
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  complex ( kind = 8 ) z

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CJY01_TEST'
  write ( *, '(a)' ) '  Test CJY01'
  write ( *, '(a)' ) ' '

  x = 4.0D+00
  y = 2.0D+00

  z = cmplx ( x, y, kind = 8 )
  write ( *, '(a,g16.8,a,g16.8)' ) '  Z = ', x, ' + i * ', y
  call cjy01 ( Z, CBJ0, CDJ0, CBJ1, CDJ1, CBY0, CDY0, CBY1, CDY1 )
  write ( *, * )
  write ( *, * ) '  n      Re[Jn(z)]       Im[Jn(z)]       Re[Jn''(z)]      Im[Jn''(z)]'
  write ( *, * ) ' --------------------------------------------------------------------'
  write ( *, '(6x,4d16.8)' ) CBJ0, CDJ0
  write ( *, '(6x,4d16.8)' ) CBJ1, CDJ1
  write ( *, * )
  write ( *, * ) '  n      Re[Yn(z)]       Im[Yn(z)]       Re[Yn''(z)]      Im[Yn''(z)]'
  write ( *, * ) ' --------------------------------------------------------------------'
  write ( *, '(6x,4d16.8)' ) CBY0, CDY0
  write ( *, '(6x,4d16.8)' ) CBY1, CDY1

  return
end
subroutine comelp_test ( )

!*****************************************************************************80
!
!! COMELP_TEST tests COMELP.
!
!  Discussion:
!
!    COMELP computes complete elliptic integrals K(k) and E(k).
!
!       Input  : K  --- Modulus k ( 0 <= k <= 1 )
!       Output : CK --- K(k)
!                CE --- E(k)
!       Example:
!                  k         K(k)          E(K)
!                ---------------------------------
!                 .00      1.570796      1.570796
!                 .25      1.596242      1.545957
!                 .50      1.685750      1.467462
!                 .75      1.910990      1.318472
!                1.00       Ï            1.000000
!       ===================================================
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
  implicit none

  real ( kind = 8 ) ce
  real ( kind = 8 ) ck
  real ( kind = 8 ) hk
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'COMELP_TEST'
  write ( *, '(a)' ) '  COMELP computes complete elliptic integrals H(K), E(K).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    k         K(k)          E(K)'
  write ( *, '(a)' ) '  ---------------------------------'

  do i = 0, 4

    hk = real ( i, kind = 8 ) / 4.0D+00

    call comelp ( hk, ck, ce )

    if ( hk /= 1.0D+00 ) then
      write ( *, '(2x,f5.2,2f14.6)' ) hk, ck, ce
    else
      write ( *, '(2x,f5.2,3x,a,3x,f14.6)' ) hk, 'Infinity', ce
    end if

  end do

  return
end
subroutine hygfx_test ( )

!*****************************************************************************80
!
!! HYGFX_TEST tests HYGFX.
!
!  Example:
!
!     A    B     C     X     F(A,B,C,X)
!
!   -2.5  3.3   6.7  0.25  0.72356129D+00
!   -0.5  3.3   6.7  0.25  0.93610145D+00    
!    0.5  3.3   6.7  0.25  0.10689695D+01    
!    2.5  3.3   6.7  0.25  0.14051563D+01
!
!   -2.5  3.3   6.7  0.55  0.46961432D+00
!   -0.5  3.3   6.7  0.55  0.85187390D+00
!    0.5  3.3   6.7  0.55  0.11795358D+01
!    2.5  3.3   6.7  0.55  0.23999063D+01
!
!   -2.5  3.3   6.7  0.85  0.29106096D+00
!   -0.5  3.3   6.7  0.85  0.75543187D+00
!    0.5  3.3   6.7  0.85  0.13510497D+00
!    2.5  3.3   6.7  0.85  0.57381566D+01
!
!    3.3  6.7  -5.5  0.25  0.15090670D+05
!    3.3  6.7  -0.5  0.25 -0.21631479D+04
!    3.3  6.7   0.5  0.25  0.26451677D+03
!    3.3  6.7   4.5  0.25  0.41946916D+01
!
!    3.3  6.7  -5.5  0.55  0.10170778D+11
!    3.3  6.7  -0.5  0.55 -0.30854772D+07
!    3.3  6.7   0.5  0.55  0.11967860D+06
!    3.3  6.7   4.5  0.55  0.58092729D+02
!
!    3.3  6.7  -5.5  0.85  0.58682088D+19
!    3.3  6.7  -0.5  0.85 -0.10217370D+13
!    3.3  6.7   0.5  0.85  0.92370648D+10
!    3.3  6.7   4.5  0.85  0.20396914D+05 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 March 2012
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!      
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a_test(4)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) c_test(4)
  real ( kind = 8 ) hf
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) x
  real ( kind = 8 ) x_test(3)

  save a_test
  save c_test
  save x_test

  data a_test / -2.5D+00, -0.5D+00, 0.5D+00, 2.5D+00 /
  data c_test / -5.5D+00, -0.5D+00, 0.5D+00, 4.5D+00/
  data x_test /  0.25D+00, 0.55D+00, 0.85D+00 /

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HYGFX_TEST:'
  write ( *, '(a)' ) '  HYGFX evaluates the hypergeometric function 2F1.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     A              B            C            X             F(A,B,C,X)'

  do l = 1, 3
    x = x_test(l)
    c = 6.7D+00
    b = 3.3D+00
    write ( *, '(a)' ) ' '
    do i = 1, 4
      a = a_test(i)
      call hygfx ( a, b, c, x, hf )
      write ( *, '(4g14.6,g24.16)' ) a, b, c, x, hf
    end do
  end do

  do l = 1, 3
    x = x_test(l)
    write ( *, '(a)' ) ' '
    do k = 1, 4
      c = c_test(k)
      b = 6.7D+00
      a = 3.3D+00
      call hygfx ( a, b, c, x, hf )
      write ( *, '(4g14.6,g24.16)' ) a, b, c, x, hf
    end do
  end do

  return
end
subroutine sphj_test ( )

!*****************************************************************************80
!
!! SPHJ_TEST tests SPHJ.
!
!  Discussion:
!
!    This routine computes the spherical Bessel functions jn(x) and 
!    jn'(x) using SPHJ.
!
!  Example:
!
!    x = 10.0
!    n          jn(x)               jn(x)
!    --------------------------------------------
!    0    -.5440211109D-01    -.7846694180D-01
!    1     .7846694180D-01    -.7009549945D-01
!    2     .7794219363D-01     .5508428371D-01
!    3    -.3949584498D-01     .9374053162D-01
!    4    -.1055892851D+00     .1329879757D-01
!    5    -.5553451162D-01    -.7226857814D-01
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However, 
!    they give permission to incorporate this routine into a user program 
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    12 January 2016
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!      
  implicit none

  real ( kind = 8 ) dj(0:250)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm
  integer ( kind = 4 ) ns
  real ( kind = 8 ) sj(0:250)
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MSPHJ'
  write ( *, '(a)' ) '  SPHJ evaluates spherical Bessel J functions'

  n = 5
  x = 0.905D+00

  if ( n <= 10 ) then
    ns = 1
  else
    ns = 5
  end if

  call sphj ( n, x, nm, sj, dj )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   n      x                   jn(x)               jn''(x)'
  write ( *, '(a)' ) ''

  do k = 0, nm, ns
    write ( *, '(1X,I3,3D20.10)' ) k, x, sj(k), dj(k)
  end do

  n = 5
  x = 10.0D+00

  if ( n <= 10 ) then
    ns = 1
  else
    ns = 5
  end if

  call sphj ( n, x, nm, sj, dj )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   n      x                   jn(x)               jn''(x)'
  write ( *, '(a)' ) ''
  do k = 0, nm, ns
    write ( *, '(1X,I3,3D20.10)' ) k, x, sj(k), dj(k)
  end do

  return
end

