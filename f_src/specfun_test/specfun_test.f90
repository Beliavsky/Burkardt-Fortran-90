program main

!*****************************************************************************80
!
!! MAIN is the main program for specfun_test.
!
!  Discussion:
!
!    specfun_test tests the SPECFUN library.
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
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'specfun_test'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SPECFUN library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
  call test06 ( )
  call test07 ( )
  call test08 ( )
  call test09 ( )

  call test10 ( )
  call test11 ( )
  call test12 ( )
  call test13 ( )
  call test14 ( )
  call test15 ( )
  call test16 ( )
  call test17 ( )
  call test18 ( )
  call test19 ( )

  call test20 ( )
  call test21 ( )
  call test22 ( )
  call ren_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'specfun_test'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 checks BESI0 against BESSEL_I0_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) besi0
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  BESI0 computes the Bessel I0 function.'
  write ( *, '(a)' ) '  BESSEL_I0_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (BESI0)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_i0_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = besi0 ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 checks RIBESL against BESSEL_I0_SPHERICAL_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) nb
  parameter ( nb = 1 )

  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(nb)
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) ize
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) ncalc
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  RIBESL returns values of Bessel functsions'
  write ( *, '(a)' ) '  of non-integer order.'
  write ( *, '(a)' ) '  BESSEL_I0_SPHERICAL_VALUES returns values'
  write ( *, '(a)' ) '  of the spherical Bessel i0 function.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '          X                      FX                        FX2'
  write ( *, '(a,a)' ) &
    '                                 (table)', &
    '                   (RIBESL)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_i0_spherical_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    alpha = 0.5D+00
    ize = 1

    call ribesl ( x, alpha, nb, ize, b, ncalc )

    fx2 = sqrt ( 0.5D+00 * pi / x ) * b(1)

    write ( *, '(2x,f24.16,2x,g24.16,2x,g24.16)' ) x, fx, fx2

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 checks BESI0 against BESSEL_I0_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) besi1
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03:'
  write ( *, '(a)' ) '  BESI1 computes the Bessel I1 function.'
  write ( *, '(a)' ) '  BESSEL_I1_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (BESI1)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_i1_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = besi1 ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 checks RIBESL against BESSEL_I1_SPHERICAL_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) nb
  parameter ( nb = 2 )

  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(nb)
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) ize
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) ncalc
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04:'
  write ( *, '(a)' ) '  RIBESL returns values of Bessel functsions'
  write ( *, '(a)' ) '  of non-integer order.'
  write ( *, '(a)' ) '  BESSEL_I1_SPHERICAL_VALUES returns values'
  write ( *, '(a)' ) '  of the spherical Bessel i1 function.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '          X                      FX                        FX2'
  write ( *, '(a,a)' ) &
    '                                 (table)', &
    '                   (RIBESL)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_i1_spherical_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    alpha = 0.5D+00
    ize = 1

    call ribesl ( x, alpha, nb, ize, b, ncalc )

    fx2 = sqrt ( 0.5D+00 * pi / x ) * b(2)

    write ( *, '(2x,f24.16,2x,g24.16,2x,g24.16)' ) x, fx, fx2

  end do

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 checks RIBESL against BESSEL_IX_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) nb_max
  parameter ( nb_max = 10 )

  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_frac
  real ( kind = 8 ) b(nb_max)
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) ize
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) nb
  integer ( kind = 4 ) ncalc
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST05:'
  write ( *, '(a)' ) '  RIBESL computes values of Bessel functions'
  write ( *, '(a)' ) '  of NONINTEGER order.'
  write ( *, '(a)' ) '  BESSEL_IX_VALUES returns selected values'
  write ( *, '(a)' ) &
    '  of the Bessel function In for NONINTEGER order.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      ALPHA         X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                                  (table)', &
    '                   (RIBESL)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_ix_values ( n_data, alpha, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    ize = 1

    nb = int ( alpha ) + 1
    if ( nb_max < nb ) then
      write ( *, * ) '  [Skipping calculation, NB_MAX too small.]'
      cycle
    end if

    alpha_frac = alpha - real ( int ( alpha ), kind = 8 )

    call ribesl ( x, alpha_frac, nb, ize, b, ncalc )

    fx2 = b(nb)

    write ( *, '(2x,f12.8,2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      alpha, x, fx, fx2

  end do

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 checks BESJ0 against BESSEL_J0_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) besj0
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST06:'
  write ( *, '(a)' ) '  BESJ0 computes the Bessel J0 function.'
  write ( *, '(a)' ) '  BESSEL_J0_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (BESJ0)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_j0_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = besj0 ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!! TEST07 checks BESJ1 against BESSEL_J1_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) besj1
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST07:'
  write ( *, '(a)' ) '  BESJ1 computes the Bessel J1 function.'
  write ( *, '(a)' ) '  BESSEL_J1_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (BESJ1)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_j1_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = besj1 ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! TEST08 checks RJBESL against BESSEL_JX_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nb_max = 10

  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_frac
  real ( kind = 8 ) b(nb_max)
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) nb
  integer ( kind = 4 ) ncalc
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST08:'
  write ( *, '(a)' ) '  RJBESL computes values of Bessel functions'
  write ( *, '(a)' ) '  of NONINTEGER order.'
  write ( *, '(a)' ) '  BESSEL_JX_VALUES returns selected values'
  write ( *, '(a)' ) &
    '  of the Bessel function Jn for NONINTEGER order.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      ALPHA         X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                                  (table)', &
    '                   (RJBESL)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_jx_values ( n_data, alpha, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    nb = int ( alpha ) + 1
    if ( nb_max < nb ) then
      write ( *, * ) '  [Skipping calculation, NB_MAX too small.]'
      cycle
    end if

    alpha_frac = alpha - real ( int ( alpha ), kind = 8 )

    call rjbesl ( x, alpha_frac, nb, b, ncalc )

    fx2 = b(nb)

    write ( *, '(2x,f12.8,2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      alpha, x, fx, fx2

  end do

  return
end
subroutine test09 ( )

!*****************************************************************************80
!
!! TEST09 checks BESK0 against BESSEL_K0_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) besk0
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST09:'
  write ( *, '(a)' ) '  BESK0 computes the Bessel K0 function.'
  write ( *, '(a)' ) '  BESSEL_K0_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (BESK0)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_k0_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = besk0 ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test10 ( )

!*****************************************************************************80
!
!! TEST10 checks BESK1 against BESSEL_K1_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) besk1
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST10:'
  write ( *, '(a)' ) '  BESK1 computes the Bessel K1 function.'
  write ( *, '(a)' ) '  BESSEL_K1_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (BESK1)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_k1_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = besk1 ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test11 ( )

!*****************************************************************************80
!
!! TEST11 checks RKBESL against BESSEL_KX_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nb_max = 10

  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_frac
  real ( kind = 8 ) b(nb_max)
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) ize
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) nb
  integer ( kind = 4 ) ncalc
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST11:'
  write ( *, '(a)' ) '  RKBESL computes values of Bessel functions'
  write ( *, '(a)' ) '  of NONINTEGER order.'
  write ( *, '(a)' ) '  BESSEL_KX_VALUES returns selected values'
  write ( *, '(a)' ) &
    '  of the Bessel function Kn for NONINTEGER order.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      ALPHA         X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                                  (table)', &
    '                   (RKBESL)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_kx_values ( n_data, alpha, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    ize = 1

    nb = int ( alpha ) + 1
    if ( nb_max < nb ) then
      write ( *, * ) '  [Skipping calculation, NB_MAX too small.]'
      cycle
    end if

    alpha_frac = alpha - real ( int ( alpha ), kind = 8 )

    call rkbesl ( x, alpha_frac, nb, ize, b, ncalc )

    fx2 = b(nb)

    write ( *, '(2x,f12.8,2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      alpha, x, fx, fx2

  end do

  return
end
subroutine test12 ( )

!*****************************************************************************80
!
!! TEST12 checks BESY0 against BESSEL_Y0_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) besy0
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST12:'
  write ( *, '(a)' ) '  BESY0 computes the Bessel Y0 function.'
  write ( *, '(a)' ) '  BESSEL_Y0_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (BESY0)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_y0_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = besy0 ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test13 ( )

!*****************************************************************************80
!
!! TEST13 checks BESY1 against BESSEL_Y1_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) besy1
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST13:'
  write ( *, '(a)' ) '  BESY1 computes the Bessel Y1 function.'
  write ( *, '(a)' ) '  BESSEL_Y1_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (BESY1)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_y1_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = besy1 ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test14 ( )

!*****************************************************************************80
!
!! TEST14 checks RYBESL against BESSEL_YX_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nb_max = 10

  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_frac
  real ( kind = 8 ) b(nb_max)
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) nb
  integer ( kind = 4 ) ncalc
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST14:'
  write ( *, '(a)' ) '  RYBESL computes values of Bessel functions'
  write ( *, '(a)' ) '  of NONINTEGER order.'
  write ( *, '(a)' ) '  BESSEL_YX_VALUES returns selected values'
  write ( *, '(a)' ) &
    '  of the Bessel function Yn for NONINTEGER order.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      ALPHA         X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                                  (table)', &
    '                   (RYBESL)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call bessel_yx_values ( n_data, alpha, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    nb = int ( alpha ) + 1
    if ( nb_max < nb ) then
      write ( *, * ) '  [Skipping calculation, NB_MAX too small.]'
      cycle
    end if

    alpha_frac = alpha - real ( int ( alpha ), kind = 8 )

    call rybesl ( x, alpha_frac, nb, b, ncalc )

    fx2 = b(nb)

    write ( *, '(2x,f12.8,2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      alpha, x, fx, fx2

  end do

  return
end
subroutine test15 ( )

!*****************************************************************************80
!
!! TEST15 checks DAW against DAWSON_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) daw
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST15:'
  write ( *, '(a)' ) '  DAW computes the Dawson function.'
  write ( *, '(a)' ) '  DAWSON_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (DAW)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call dawson_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = daw ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test16 ( )

!*****************************************************************************80
!
!! TEST16 checks EONE against E1_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) eone
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST16:'
  write ( *, '(a)' ) '  EONE computes the exponential integral E1.'
  write ( *, '(a)' ) '  E1_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (E1)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call e1_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = eone ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test17 ( )

!*****************************************************************************80
!
!! TEST17 checks EI against EI_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) ei
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST17:'
  write ( *, '(a)' ) '  EI computes the exponential integral Ei.'
  write ( *, '(a)' ) '  EI_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (Ei)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call ei_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = ei ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test18 ( )

!*****************************************************************************80
!
!! TEST18 checks R8_ERF against ERF_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_erf
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST18:'
  write ( *, '(a)' ) '  R8_ERF computes the error function.'
  write ( *, '(a)' ) '  ERF_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (DERF)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call erf_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_erf ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test19 ( )

!*****************************************************************************80
!
!! TEST19 checks R8_GAMMA against GAMMA_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 January 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_gamma
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST19:'
  write ( *, '(a)' ) &
    '  R8_GAMMA computes the gamma function.'
  write ( *, '(a)' ) '  GAMMA_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2                    DIFF'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (R8_GAMMA)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call gamma_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_gamma ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16,2x,g10.4)' ) &
      x, fx, fx2, abs ( fx - fx2 )

  end do

  return
end
subroutine test20 ( )

!*****************************************************************************80
!
!! TEST20 checks R8_GAMMA_LOG against GAMMA_LOG_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST20:'
  write ( *, '(a)' ) &
    '  R8_GAMMA_LOG computes the log of the gamma function.'
  write ( *, '(a)' ) '  GAMMA_LOG_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             FX', &
    '                        FX2'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (R8_GAMMA_LOG)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call gamma_log_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_gamma_log ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16)' ) &
      x, fx, fx2

  end do

  return
end
subroutine test21 ( )

!*****************************************************************************80
!
!! TEST21 tests MACHAR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) eps
  real ( kind = 8 ) epsneg
  integer ( kind = 4 ) ibeta
  integer ( kind = 4 ) iexp
  integer ( kind = 4 ) irnd
  integer ( kind = 4 ) it
  integer ( kind = 4 ) machep
  integer ( kind = 4 ) maxexp
  integer ( kind = 4 ) minexp
  integer ( kind = 4 ) negep
  integer ( kind = 4 ) ngrd
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST21'
  write ( *, '(a)' ) '  MACHAR computes double'
  write ( *, '(a)' ) '  precision machine constants.'

  call machar ( ibeta, it, irnd, ngrd, machep, negep, iexp, &
    minexp, maxexp, eps, epsneg, xmin, xmax )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  IBETA is the internal base for machine arithmetic.'
  write ( *, '(a,i8)' ) '    IBETA =  ', ibeta
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  IT is the number of digits, base IBETA, in the'
  write ( *, '(a)' ) '  floating point significand.'
  write ( *, '(a,i8)' ) '    IT =     ', it
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  IRND reports on floating point addition rounding:'
  write ( *, '(a)' ) '  0, for chopping;'
  write ( *, '(a)' ) '  1, for non-IEEE rounding;'
  write ( *, '(a)' ) '  2, for IEEE rounding;'
  write ( *, '(a)' ) '  3, for chopping with partial underflow;'
  write ( *, '(a)' ) &
    '  4, for non-IEEE rounding with partial underflow.'
  write ( *, '(a)' ) &
    '  5, for IEEE rounding with partial underflow.'
  write ( *, '(a,i8)' ) '    IRND =   ', irnd
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  NGRD is the number of guard digits for floating point'
  write ( *, '(a)' ) '  multiplication with truncating arithmetic.'
  write ( *, '(a,i8)' ) '    NGRD =   ', ngrd
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  MACHEP is the largest negative integer such that'
  write ( *, '(a)' ) '  1.0 < 1.0 + BETA**MACHEP.'
  write ( *, '(a,i8)' ) '    MACHEP = ', machep
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  NEGEPS is the largest negative integer such that'
  write ( *, '(a)' ) '  1.0 - BETA**NEGEPS < 1.0:'
  write ( *, '(a,i8)' ) '    NEGEP =  ', negep
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  IEXP is the number of bits reserved for the exponent'
  write ( *, '(a)' ) '  of a floating point number:'
  write ( *, '(a,i8)' ) '    IEXP =   ', iexp
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  MINEXP is the most negative power of BETA such that'
  write ( *, '(a)' ) '  BETA**MINEXP is positive and normalized.'
  write ( *, '(a,i8)' ) '    MINEXP = ', minexp
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  MAXEXP is the smallest positive power of BETA that'
  write ( *, '(a)' ) '  overflows:'
  write ( *, '(a,i8)' ) '    MAXEXP = ', maxexp
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  EPS is a small positive floating point number'
  write ( *, '(a)' ) '  such that 1.0 < 1.0 + EPS.'
  write ( *, '(a,e25.13)' ) '    EPS    = ', eps
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  EPSNEG is a small positive floating point number'
  write ( *, '(a)' ) '  such that 1.0 - EPSNEG < 1.0.'
  write ( *, '(a,e25.13)' ) '    EPSNEG = ', epsneg
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  XMIN is the smallest positive normalized floating'
  write ( *, '(a)' ) '  point power of the radix:'
  write ( *, '(a,e25.13)' ) '    XMIN =   ', xmin
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  XMAX is the largest finite floating point number:'
  write ( *, '(a,e25.13)' ) '    XMAX   = ', xmax

  return
end
subroutine test22 ( )

!*****************************************************************************80
!
!! TEST22 checks R8_PSI against PSI_VALUES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fx
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r8_psi
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST22:'
  write ( *, '(a)' ) '  R8_PSI computes the PSI function.'
  write ( *, '(a)' ) '  PSI_VALUES returns selected values.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,a)' ) &
    '      X             PSI', &
    '                       PSI                    DIFF'
  write ( *, '(a,a)' ) &
    '                   (table)', &
    '                   (R8_PSI)'
  write ( *, '(a)' ) ' '

  n_data = 0

  do

    call psi_values ( n_data, x, fx )

    if ( n_data == 0 ) then
      exit
    end if

    fx2 = r8_psi ( x )

    write ( *, '(2x,f12.8,2x,g24.16,2x,g24.16,2x,g10.4)' ) &
      x, fx, fx2, abs ( fx - fx2 )

  end do

  return
end
subroutine ren_test ( )

!*********************************************************************80
!
!! REN_TEST tests REN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: sample_num = 1000

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  real ( kind = 8 ) ren
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(sample_num)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'REN_TEST'
  write ( *, '(a)' ) '  REN is a random number generator.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  10 sample values:'
  write ( *, '(a)' ) ' '
  do i = 1, 10
    write ( *, '(g14.6)' ) ren ( )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Expected mean =     ', 0.5D+00
  write ( *, '(a,g14.6)' ) '  Expected variance = ', &
    1.0D+00 / 12.0D+00

  mean = 0.0D+00
  xmax = -1000.0D+00
  xmin = +1000.0D+00

  do i = 1, sample_num
    x(i) = ren ( )
    mean = mean + x(i)
    xmax = max ( xmax, x(i) )
    xmin = min ( xmin, x(i) )
  end do
  mean = mean / real ( sample_num, kind = 8 )

  variance = 0.0D+00
  do i = 1, sample_num
    variance = variance + ( x(i) - mean ) ** 2
  end do
  variance = variance / real ( sample_num - 1, kind = 8 )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
  write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
  write ( *, '(a,g14.6)' ) '  Sample variance = ', variance
  write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
  write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

  return
end



