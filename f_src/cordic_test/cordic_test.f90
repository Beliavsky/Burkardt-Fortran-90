program main

!*****************************************************************************80
!
!! MAIN is the main program for CORDIC_TEST.
!
!  Discussion:
!
!    CORDIC_TEST tests the CORDIC library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CORDIC_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the CORDIC library.'

  call arccos_cordic_test ( )
  call arcsin_cordic_test ( )
  call arctan_cordic_test ( )
  call cbrt_cordic_test ( )
  call cossin_cordic_test1 ( )
  call cossin_cordic_test2 ( )
  call exp_cordic_test ( )
  call ln_cordic_test ( )
  call multiply_cordic_test ( )
  call sqrt_cordic_test ( )
  call tan_cordic_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CORDIC_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine arccos_cordic_test ( )

!*****************************************************************************80
!
!! ARCCOS_CORDIC_TEST test ARCCOS_CORDIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a1
  real ( kind = 8 ) a2
  real ( kind = 8 ) d
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) t

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ARCCOS_CORDIC_TEST:'
  write ( *, '(a)' ) '  ARCCOS_CORDIC computes the arccosine of T'
  write ( *, '(a)' ) '  using the CORDIC algorithm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '          T        N      ArcCos(T)       ArcCos(T)         Difference'
  write ( *, '(a)' ) &
    '                          Tabulated        CORDIC'

  n_data = 0

  do

    call arccos_values ( n_data, t, a1 )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(a)' ) ' '

    do n = 0, 25, 5

      call arccos_cordic ( t, n, a2 )

      d = a1 - a2

      write ( *, '(2x,f12.4,2x,i4,2x,g16.8,2x,g16.8,2x,e12.4)' ) &
        t, n, a1, a2, d

    end do

  end do

  return
end
subroutine arcsin_cordic_test ( )

!*****************************************************************************80
!
!! ARCSIN_CORDIC_TEST demonstrates the use of ARCSIN_CORDIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a1
  real ( kind = 8 ) a2
  real ( kind = 8 ) d
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) t

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ARCSIN_CORDIC_TEST:'
  write ( *, '(a)' ) '  ARCSIN_CORDIC computes the arcsine of T'
  write ( *, '(a)' ) '  using the CORDIC algorithm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '          T        N      ArcSin(T)       ArcSin(T)         Difference'
  write ( *, '(a)' ) &
    '                          Tabulated        CORDIC'

  n_data = 0

  do

    call arcsin_values ( n_data, t, a1 )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(a)' ) ' '

    do n = 0, 25, 5

      call arcsin_cordic ( t, n, a2 )

      d = a1 - a2

      write ( *, '(2x,f12.4,2x,i4,2x,g16.8,2x,g16.8,2x,e12.4)' ) &
        t, n, a1, a2, d

    end do

  end do

  return
end
subroutine arctan_cordic_test ( )

!*****************************************************************************80
!
!! ARCTAN_CORDIC_TEST tests ARCTAN_CORDIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a1
  real ( kind = 8 ) a2
  real ( kind = 8 ) d
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) s
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ARCTAN_CORDIC_TEST:'
  write ( *, '(a)' ) '  ARCTAN_CORDIC computes the arctangent of Y/X'
  write ( *, '(a)' ) '  using the CORDIC algorithm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '      X      Y    N       ArcTan(Y/X) ArcTan(Y/X)      Difference'
  write ( *, '(a)' ) &
    '                           Tabulated   CORDIC'
  n_data = 0

  do

    call arctan_values ( n_data, z, a1 )

    if ( n_data == 0 ) then
      exit
    end if

    r = r8_uniform_01 ( seed )

    x = r
    y = r * z

    s = r8_uniform_01 ( seed )

    if ( s < 0.5D+00 ) then
      x = -x
      y = -y
    end if

    write ( *, '(a)' ) ' '

    do n = 0, 25, 5

      call arctan_cordic ( x, y, n, a2 )

      d = a1 - a2

      write ( *, '(2x,f12.4,2x,f12.4,2x,i4,2x,g16.8,2x,g16.8,2x,e12.4)' ) &
        x, y, n, a1, a2, d

    end do

  end do

  return
end
subroutine cbrt_cordic_test ( )

!*****************************************************************************80
!
!! CBRT_CORDIC demonstrates the use of CBRT_CORDIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) d
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CBRT_CORDIC:'
  write ( *, '(a)' ) '  CBRT_CORDIC computes the cube root function'
  write ( *, '(a)' ) '  using the CORDIC algorithm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '          X          N      Cbrt(X)         Cbrt(X)           Difference'
  write ( *, '(a)' ) &
    '                          Tabulated        CORDIC'

  n_data = 0

  do

    call cbrt_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(a)' ) ' '

    do n = 0, 25, 5

      call cbrt_cordic ( x, n, fx2 )

      d = fx1 - fx2

      write ( *, '(2x,g14.6,2x,i4,2x,g16.8,2x,g16.8,2x,e12.4)' ) &
        x, n, fx1, fx2, d

    end do

  end do

  return
end
subroutine cossin_cordic_test1 ( )

!*****************************************************************************80
!
!! COSSIN_CORDIC_TEST1 demonstrates the use of COSSIN_CORDIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) d
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) s2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'COSSIN_CORDIC_TEST1:'
  write ( *, '(a)' ) '  COSSIN_CORDIC computes the cosine and sine'
  write ( *, '(a)' ) '  using the CORDIC algorithm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '          A        N      Cos(A)           Cos(A)           Difference'
  write ( *, '(a)' ) &
    '                          Tabulated        CORDIC'

  n_data = 0

  do

    call cos_values ( n_data, a, c1 )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(a)' ) ' '

    do n = 0, 25, 5

      call cossin_cordic ( a, n, c2, s2 )

      d = c1 - c2

      write ( *, '(2x,f12.4,2x,i4,2x,g16.8,2x,g16.8,2x,e12.4)' ) &
        a, n, c1, c2, d

    end do

  end do

  return
end
subroutine cossin_cordic_test2 ( )

!*****************************************************************************80
!
!! COSSIN_CORDIC_TEST2 demonstrates the use of COSSIN_CORDIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) c2
  real ( kind = 8 ) d
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) s1
  real ( kind = 8 ) s2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'COSSIN_CORDIC_TEST2:'
  write ( *, '(a)' ) '  COSSIN_CORDIC computes the cosine and sine'
  write ( *, '(a)' ) '  using the CORDIC algorithm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '          A        N      Sin(A)           Sin(A)           Difference'
  write ( *, '(a)' ) &
    '                          Tabulated        CORDIC'
  n_data = 0

  do

    call sin_values ( n_data, a, s1 )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(a)' ) ' '

    do n = 0, 25, 5

      call cossin_cordic ( a, n, c2, s2 )

      d = s1 - s2

      write ( *, '(2x,f12.4,2x,i4,2x,g16.8,2x,g16.8,2x,e12.4)' ) &
        a, n, s1, s2, d

    end do

  end do

  return
end
subroutine exp_cordic_test ( )

!*****************************************************************************80
!
!! EXP_CORDIC_TEST demonstrates the use of EXP_CORDIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) d
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'EXP_CORDIC_TEST:'
  write ( *, '(a)' ) '  EXP_CORDIC computes the exponential function'
  write ( *, '(a)' ) '  using the CORDIC algorithm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '        X          N       Exp(X)          Exp(X)           Difference'
  write ( *, '(a)' ) &
    '                          Tabulated        CORDIC'

  n_data = 0

  do

    call exp_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(a)' ) ' '

    do n = 0, 25, 5

      call exp_cordic ( x, n, fx2 )

      d = fx1 - fx2

      write ( *, '(2x,f12.4,2x,i4,2x,g16.8,2x,g16.8,2x,e12.4)' ) &
        x, n, fx1, fx2, d

    end do

  end do

  return
end
subroutine ln_cordic_test ( )

!*****************************************************************************80
!
!! LN_CORDIC_TEST demonstrates the use of LN_CORDIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) d
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LN_CORDIC_TEST:'
  write ( *, '(a)' ) '  LN_CORDIC computes the natural logarithm function'
  write ( *, '(a)' ) '  using the CORDIC algorithm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '          X          N        Ln(X)           Ln(X)           Difference'
  write ( *, '(a)' ) &
    '                          Tabulated        CORDIC'

  n_data = 0

  do

    call ln_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(a)' ) ' '

    do n = 0, 25, 5

      call ln_cordic ( x, n, fx2 )

      d = fx1 - fx2

      write ( *, '(2x,g14.6,2x,i4,2x,g16.8,2x,g16.8,2x,e12.4)' ) &
        x, n, fx1, fx2, d

    end do

  end do

  return
end
subroutine multiply_cordic_test ( )

!*****************************************************************************80
!
!! MULTIPLY_CORDIC_TEST tests MULTIPLY_CORDIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z1
  real ( kind = 8 ) z2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MULTIPLY_CORDIC_TEST:'
  write ( *, '(a)' ) '  MULTIPLY_CORDIC computes Z = X * Y'
  write ( *, '(a)' ) '  using the CORDIC algorithm.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        X             Y               Z                 Z'
  write ( *, '(a)' ) '                                      (X*Y)             (CORDIC)'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 20

    r = r8_uniform_01 ( seed );
    x = - 100.0D+00 + 200.0D+00 * r
    r = r8_uniform_01 ( seed );
    y = - 100.0D+00 + 200.0D+00 * r
    z1 = x * y
    call multiply_cordic ( x, y, z2 )

    write ( *, '(2x,f12.8,2x,f12.8,2x,f16.8,2x,f16.8)' ) x, y, z1, z2

  end do

  return
end
subroutine sqrt_cordic_test ( )

!*****************************************************************************80
!
!! SQRT_CORDIC_TEST demonstrates the use of SQRT_CORDIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) d
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SQRT_CORDIC_TEST:'
  write ( *, '(a)' ) '  SQRT_CORDIC computes the square root function'
  write ( *, '(a)' ) '  using the CORDIC algorithm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '          X          N      Sqrt(X)         Sqrt(X)           Difference'
  write ( *, '(a)' ) &
    '                          Tabulated        CORDIC'

  n_data = 0

  do

    call sqrt_values ( n_data, x, fx1 )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(a)' ) ' '

    do n = 0, 25, 5

      call sqrt_cordic ( x, n, fx2 )

      d = fx1 - fx2

      write ( *, '(2x,g14.6,2x,i4,2x,g16.8,2x,g16.8,2x,e12.4)' ) &
        x, n, fx1, fx2, d

    end do

  end do

  return
end

subroutine tan_cordic_test ( )

!*****************************************************************************80
!
!! TAN_CORDIC_TEST demonstrates the use of TAN_CORDIC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) d
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) theta

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TAN_CORDIC_TEST:'
  write ( *, '(a)' ) '  TAN_CORDIC computes the tangent of THETA'
  write ( *, '(a)' ) '  using the CORDIC algorithm.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '      THETA        N     Tan(THETA)      Tan(THETA)         Difference'
  write ( *, '(a)' ) &
    '                          Tabulated        CORDIC'

  n_data = 0

  do

    call tan_values ( n_data, theta, t1 )

    if ( n_data == 0 ) then
      exit
    end if

    write ( *, '(a)' ) ' '

    do n = 0, 25, 5

      call tan_cordic ( theta, n, t2 )

      d = t1 - t2

      write ( *, '(2x,f12.4,2x,i4,2x,g16.8,2x,g16.8,2x,e12.4)' ) &
        theta, n, t1, t2, d

    end do

  end do

  return
end

