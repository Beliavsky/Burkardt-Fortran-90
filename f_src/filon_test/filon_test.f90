program main

!*****************************************************************************80
!
!! MAIN is the main program for FILON_TEST.
!
!  Discussion:
!
!    FILON_TEST tests the FILON library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FILON_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the FILON library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
  call test06 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FILON_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests FILON_TAB_COS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) exact
  real ( kind = 8 ), allocatable, dimension ( : ) :: ftab
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) result
  real ( kind = 8 ) t
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  a = 0.0D+00
  b = 2.0D+00 * r8_pi

  n = 11
!
!  Set the X values.
!
  allocate ( x(1:n) )
  do i = 1, n
    x(i) = ( real ( n - i,     kind = 8 ) * a   &
           + real (     i - 1, kind = 8 ) * b ) &
           / real ( n     - 1, kind = 8 )
  end do

  allocate ( ftab(1:n) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  FILON_TAB_COS estimates the integral of.'
  write ( *, '(a)' ) '  F(X) * COS ( T * X )'
  write ( *, '(a)' ) '  Use integrands F(X)=1, X, X^2.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g24.16)' ) '  A = ', a
  write ( *, '(a,g24.16)' ) '  B = ', b
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       T                      Approximate             Exact'

  do k = 1, 3

    if ( k == 1 ) then
      t = 1.0D+00
    else if ( k == 2 ) then
      t = 2.0D+00
    else if ( k == 3 ) then
      t = 10.0D+00
    end if

    write ( *, '(a)' ) ' '

    do i = 1, 3

      if ( i == 1 ) then
        call zero_integrand ( n, x, ftab )
      else if ( i == 2 ) then
        call one_integrand ( n, x, ftab )
      else if ( i == 3 ) then
        call two_integrand ( n, x, ftab )
      end if

      call filon_tab_cos ( n, ftab, a, b, t, result )

      if ( i == 1 ) then
        exact = ( sin ( t * b ) - sin ( t * a ) ) / t
      else if ( i == 2 ) then
        exact = ( ( cos ( t * b ) + t * b * sin ( t * b ) ) &
                - ( cos ( t * a ) + t * a * sin ( t * a ) ) ) / t**2
      else if ( i == 3 ) then
        exact = ( ( 2.0D+00 * t * b * cos ( t * b ) &
              + ( t * t * b**2 - 2.0D+00 ) * sin ( t * b ) ) &
                - ( 2.0D+00 * t * a * cos ( t * a ) &
              + ( t * t * a**2 - 2.0D+00 ) * sin ( t * a ) ) ) / t**3
      end if

      write ( *, '(2x,3g24.16)' ) t, result, exact

    end do

  end do

  deallocate ( ftab )
  deallocate ( x )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests FILON_TAB_COS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  real ( kind = 8 ), allocatable, dimension ( : ) :: ftab
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) result
  real ( kind = 8 ) t
  real ( kind = 8 ), allocatable, dimension ( : ) :: x
!
!  Example suggested by James Roedder.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Integrate F(X) = log(1+X)*cos(T*X):'
  write ( *, '(a)' ) '  Supply integrand as a table.'
  write ( *, '(a)' ) '  T = 10, and N increases'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       N    Approximate             Exact                   Error'
  write ( *, '(a)' ) ' '

  a = 0.0D+00
  b = 2.0D+00 * r8_pi

  do j = 1, 6

    n = 2 ** j * 10 + 1
!
!  Set the X values.
!
    allocate ( x(1:n) )
    do i = 1, n
      x(i) = ( real ( n - i,     kind = 8 ) * a   &
             + real (     i - 1, kind = 8 ) * b ) &
             / real ( n     - 1, kind = 8 )
    end do

    allocate ( ftab(1:n) )
    call log_integrand ( n, x, ftab )
 
    t = 10.0D+00

    call filon_tab_cos ( n, ftab, a, b, t, result )

    exact = -0.008446594405D+00
    error = result - exact

    write ( *, '(2x,i6,g24.16,g24.16,g16.8)' ) &
      n, result, exact, error

    deallocate ( ftab )
    deallocate ( x )

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests FILON_FUN_COS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  external log_integrand
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) result
  real ( kind = 8 ) t
!
!  Example suggested by James Roedder.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Integrate F(X)=log(1+X)*cos(T*X):'
  write ( *, '(a)' ) '  Supply integrand as a function.'
  write ( *, '(a)' ) '  T = 10, and N increases'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       N    Approximate             Exact                   Error'
  write ( *, '(a)' ) ' '

  a = 0.0D+00
  b = 2.0D+00 * r8_pi

  do j = 1, 6

    n = 2 ** j * 10 + 1
 
    t = 10.0D+00

    call filon_fun_cos ( n, log_integrand, a, b, t, result )

    exact = -0.008446594405D+00
    error = result - exact

    write ( *, '(2x,i6,g24.16,g24.16,g16.8)' ) &
      n, result, exact, error

  end do

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests FILON_TAB_SIN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) exact
  real ( kind = 8 ), allocatable, dimension ( : ) :: ftab
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) result
  real ( kind = 8 ) t
  real ( kind = 8 ), allocatable, dimension ( : ) :: x

  a = 0.0D+00
  b = 2.0D+00 * r8_pi

  n = 11
  allocate ( ftab(1:n) )
!
!  Set the X values.
!
  allocate ( x(1:n) )
  do i = 1, n
    x(i) = ( real ( n - i,     kind = 8 ) * a   &
           + real (     i - 1, kind = 8 ) * b ) &
           / real ( n     - 1, kind = 8 )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  FILON_TAB_SIN estimates the integral of.'
  write ( *, '(a)' ) '  F(X) * SIN ( T * X )'
  write ( *, '(a)' ) '  Use integrands 1, X, X^2.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g24.16)' ) '  A = ', a
  write ( *, '(a,g24.16)' ) '  B = ', b
  write ( *, '(a,i6)' ) '  N = ', n
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       T                      Approximate             Exact'
  write ( *, '(a)' ) ' '

  do k = 1, 3

    if ( k == 1 ) then
      t = 1.0D+00
    else if ( k == 2 ) then
      t = 2.0D+00
    else if ( k == 3 ) then
      t = 10.0D+00
    end if

    write ( *, '(a)' ) ' '

    do i = 1, 3

      if ( i == 1 ) then
        call zero_integrand ( n, x, ftab )
      else if ( i == 2 ) then
        call one_integrand ( n, x, ftab )
      else if ( i == 3 ) then
        call two_integrand ( n, x, ftab )
      end if

      call filon_tab_sin ( n, ftab, a, b, t, result )

      if ( i == 1 ) then
        exact = ( - cos ( t * b ) + cos ( t * a ) ) / t
      else if ( i == 2 ) then
        exact = ( ( sin ( t * b ) - t * b * cos ( t * b ) ) &
                - ( sin ( t * a ) - t * a * cos ( t * a ) ) ) / t**2
      else if ( i == 3 ) then
        exact = ( ( 2.0D+00 * t * b * sin ( t * b ) &
                + ( 2.0D+00 - t**2 * b**2 ) * cos ( t * b ) ) &
                - ( 2.0D+00 * t * a * sin ( t * a ) &
                + ( 2.0D+00 - t**2 * a**2 ) * cos ( t * a ) ) ) / t**3
      end if

      write ( *, '(2x,3g24.16)' ) t, result, exact

    end do

  end do

  deallocate ( ftab )
  deallocate ( x )

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests FILON_TAB_COS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  real ( kind = 8 ), allocatable, dimension ( : ) :: ftab
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) result
  real ( kind = 8 ) t
  real ( kind = 8 ), allocatable, dimension ( : ) :: x
!
!  Example suggested by James Roedder.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  Integrate F(X)=log(1+X)*sin(T*X):'
  write ( *, '(a)' ) '  Supply integrand as a table.'
  write ( *, '(a)' ) '  T = 10, and N increases'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       N    Approximate             Exact                   Error'
  write ( *, '(a)' ) ' '

  a = 0.0D+00
  b = 2.0D+00 * r8_pi

  do j = 1, 6

    n = 2 ** j * 10 + 1
!
!  Set the X values.
!
    allocate ( x(1:n) )
    do i = 1, n
      x(i) = ( real ( n - i,     kind = 8 ) * a   &
             + real (     i - 1, kind = 8 ) * b ) &
             / real ( n     - 1, kind = 8 )
    end do

    allocate ( ftab(1:n) )
    call log_integrand ( n, x, ftab )

    t = 10.0D+00

    call filon_tab_sin ( n, ftab, a, b, t, result )

    exact = -0.19762680771872D+00
    error = result - exact

    write ( *, '(2x,i6,g24.16,g24.16,g16.8)' ) &
      n, result, exact, error

    deallocate ( ftab )
    deallocate ( x )

  end do

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 tests FILON_FUN_COS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) error
  real ( kind = 8 ) exact
  external log_integrand
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) result
  real ( kind = 8 ) t
!
!  Example suggested by James Roedder.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST06'
  write ( *, '(a)' ) '  Integrate F(X)=log(1+X)*sin(T*X):'
  write ( *, '(a)' ) '  Supply integrand as a function.'
  write ( *, '(a)' ) '  T = 10, and N increases'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '       N    Approximate             Exact                   Error'
  write ( *, '(a)' ) ' '

  a = 0.0D+00
  b = 2.0D+00 * r8_pi

  do j = 1, 6

    n = 2 ** j * 10 + 1

    t = 10.0D+00

    call filon_fun_sin ( n, log_integrand, a, b, t, result )

    exact = -0.19762680771872D+00
    error = result - exact

    write ( *, '(2x,i6,g24.16,g24.16,g16.8)' ) &
      n, result, exact, error

  end do

  return
end
subroutine zero_integrand ( n, x, fx )

!*****************************************************************************80
!
!! ZERO_INTEGRAND evaluates the integrand x^0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the evaluation points.
!
!    Output, real ( kind = 8 ) FX(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fx(n)
  real ( kind = 8 ) x(n)

  fx(1:n) = 1.0D+00

  return
end
subroutine one_integrand ( n, x, fx )

!*****************************************************************************80
!
!! ONE_INTEGRAND evaluates the integrand X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the evaluation points.
!
!    Output, real ( kind = 8 ) FX(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fx(n)
  real ( kind = 8 ) x(n)

  fx(1:n) = x(1:n)

  return
end
subroutine two_integrand ( n, x, fx )

!*****************************************************************************80
!
!! TWO_INTEGRAND evaluates the integrand X^2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the evaluation points.
!
!    Output, real ( kind = 8 ) FX(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fx(n)
  real ( kind = 8 ) x(n)

  fx(1:n) = x(1:n) ** 2

  return
end
subroutine log_integrand ( n, x, fx )

!*****************************************************************************80
!
!! LOG_INTEGRAND evaluates the logarithmic integrand.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the evaluation points.
!
!    Output, real ( kind = 8 ) FX(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fx(n)
  real ( kind = 8 ) x(n)

  fx(1:n) = log ( 1.0D+00 + x(1:n) )

  return
end
