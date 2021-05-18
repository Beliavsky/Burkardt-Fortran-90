program main

!*****************************************************************************80
!
!! MAIN is a main program for testing MULLER_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'muller_test:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  muller uses Muller''s method, with complex arithmetic,'
  write ( *, '(a)' ) '  to solve a nonlinear equation.'

  call test01 ( )
  call test02 ( )
  call test03 ()

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MULLER_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01

!*****************************************************************************80
!
!! TEST01 tests muller on F(X) = X*X+9.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fatol
  external func01
  complex ( kind = 8 ) fxnew
  integer ( kind = 4 ) itmax
  complex ( kind = 8 ) x1
  complex ( kind = 8 ) x2
  complex ( kind = 8 ) x3
  complex ( kind = 8 ) xnew
  real ( kind = 8 ) xatol
  real ( kind = 8 ) xrtol

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  Demonstrate muller on F(X) = X*X+9.'

  fatol = 1.0D-05
  itmax = 10
  x1 = cmplx ( 1.0D+00, 0.0D+00, kind = 8 )
  x2 = cmplx ( 0.0D+00, 1.0D+00, kind = 8 )
  x3 = cmplx ( 0.5D+00, 0.5D+00, kind = 8 )
  xatol = 1.0D-05
  xrtol = 1.0D-05

  call muller ( func01, fatol, itmax, x1, x2, x3, xatol, xrtol, xnew, fxnew )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  muller returned the root X'
  write ( *, '(a)' ) ' '
  write ( *, '(a,f20.10,f20.10)' ) '     X   = ', xnew
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  with function value F(X):'
  write ( *, '(a)' ) ' '
  write ( *, '(a,f20.10,f20.10)' ) '    FX   = ', fxnew
  write ( *, '(a,f20.10)' )        '  ||FX|| = ', abs ( fxnew )

  return
end
subroutine test02

!*****************************************************************************80
!
!! TEST02 tests muller on F(X) = (X*X+4) * (X-10) * (X+20)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real    ( kind = 8 ) fatol
  external func02
  complex ( kind = 8 ) fxnew
  integer ( kind = 4 ) itmax
  complex ( kind = 8 ) x1
  complex ( kind = 8 ) x2
  complex ( kind = 8 ) x3
  complex ( kind = 8 ) xnew
  real ( kind = 8 ) xatol
  real ( kind = 8 ) xrtol

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  Demonstrate muller on F(X) = (X*X+4)*(X-10)*(X+20).'

  fatol = 1.0D-05
  itmax = 10
  x1 = cmplx ( 1.0D+00, 0.0D+00, kind = 8 )
  x2 = cmplx ( 0.0D+00, 1.0D+00, kind = 8 )
  x3 = cmplx ( 0.5D+00, 0.5D+00, kind = 8 )
  xatol = 1.0D-05
  xrtol = 1.0D-05

  call muller ( func02, fatol, itmax, x1, x2, x3, xatol, xrtol, xnew, fxnew )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  muller returned the root X'
  write ( *, '(a)' ) ' '
  write ( *, '(a,f20.10,f20.10)' ) '     X   = ', xnew
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  with function value F(X):'
  write ( *, '(a)' ) ' '
  write ( *, '(a,f20.10,f20.10)' ) '    FX   = ', fxnew
  write ( *, '(a,f20.10)' )        '  ||FX|| = ', abs ( fxnew )

  return
end
subroutine test03

!*****************************************************************************80
!
!! TEST03 tests muller on Zhelyazkov's function
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) fatol
  external func03
  complex ( kind = 8 ) fxnew
  integer ( kind = 4 ) itmax
  integer ( kind = 4 ) test
  complex ( kind = 8 ) x1
  complex ( kind = 8 ) x2
  complex ( kind = 8 ) x3
  complex ( kind = 8 ) xnew
  real ( kind = 8 ) xatol
  real ( kind = 8 ) xrtol

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03:'
  write ( *, '(a)' ) '  Demonstrate muller on Zhelyazkov''s function.'

  fatol = 1.0D-07
  itmax = 10

  do test = 1, 2
!
!  First set of starting points.
!  Result is X = ( 1.5705798926, 0.0 )
!
    if ( test == 1 ) then
      x1 = cmplx ( 1.0D+00, 0.0D+00, kind = 8 )
      x2 = cmplx ( 0.0D+00, 1.0D+00, kind = 8 )
      x3 = cmplx ( 0.5D+00, 0.5D+00, kind = 8 )
!
!  Second set of starting points.
!  Result is X = ( -0.5802520567, 0.0 ).
!
    else if ( test == 2 ) then
      x1 = cmplx (  0.0D+00, 1.0D+00, kind = 8 )
      x2 = cmplx (  1.0D+00, 2.0D+00, kind = 8 )
      x3 = cmplx ( -1.0D+00, 2.0D+00, kind = 8 )
    end if

    xatol = 1.0D-07
    xrtol = 1.0D-07

    call muller ( func03, fatol, itmax, x1, x2, x3, xatol, xrtol, xnew, fxnew )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST03:'
    write ( *, '(a)' ) '  muller returned the root X'
    write ( *, '(a)' ) ' '
    write ( *, '(a,f20.10,f20.10)' ) '     X   = ', xnew
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  with function value F(X):'
    write ( *, '(a)' ) ' '
    write ( *, '(a,f20.10,f20.10)' ) '    FX   = ', fxnew
    write ( *, '(a,f20.10)' )        '  ||FX|| = ', abs ( fxnew )

  end do

  return
end
subroutine func01 ( x, fx )

!*****************************************************************************80
!
!! FUNC01 evaluates F(X) = X*X+9.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, complex ( kind = 8 ) X, the point at which the function is to
!    be evaluated.
!
!    Output, complex ( kind = 8 ) FX, the function value at X.
!
  implicit none

  complex ( kind = 8 ) fx
  complex ( kind = 8 ) x

  fx = x * x + 9.0D+00
 
  return
end
subroutine func02 ( x, fx )

!*****************************************************************************80
!
!! FUNC02 evaluates F(X) = (X*X+4)*(X-1)*(X+2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, complex ( kind = 8 ) X, the point at which the function is to
!    be evaluated.
!
!    Output, complex ( kind = 8 ) FX, the function value at X.
!
  implicit none

  complex ( kind = 8 ) fx
  complex ( kind = 8 ) x

  fx = ( x * x + 4.0D+00 ) * ( x - 10.0D+00 ) * ( x + 20.0D+00 )
 
  return
end
subroutine func03 ( z, fz )

!*****************************************************************************80
!
!! FUNC03 evaluates Zhelyazkov's function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, complex ( kind = 8 ) Z, the point at which the function is to
!    be evaluated.
!
!    Output, complex ( kind = 8 ) FZ, the function value at Z.
!
  implicit none

  complex ( kind = 8 ) a
  complex ( kind = 8 ) b
  complex ( kind = 8 ), parameter :: eps = ( 0.4D+00, 0.0D+00 )
  complex ( kind = 8 ), parameter :: eta = ( 0.64D+00, 0.0D+00 )
  complex ( kind = 8 ) fz
  real ( kind = 8 ), parameter :: me = 0.384D+00
  real ( kind = 8 ), parameter :: mo = 0.5D+00
  complex ( kind = 8 ) of
  complex ( kind = 8 ) ok
  complex ( kind = 8 ), parameter :: one = ( 1.0D+00, 0.0D+00 )
  real ( kind = 8 ), parameter :: x = 0.5D+00
  complex ( kind = 8 ) z

  ok = z - me / sqrt ( eta )
  of = z - mo

  a = of * of + ( ok * ok ) * eta * dtanh ( x )

  b = ( of - ok * eta ) / ( of - ok * eta * eta )

  fz = of * of - one + ( eta * ok * ok - one ) * &
    dtanh ( x ) - x * x * eps * eps * a * b

  return
end
