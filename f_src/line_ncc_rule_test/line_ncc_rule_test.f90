program main

!*****************************************************************************80
!
!! MAIN is the main program for LINE_NCC_RULE_TEST.
!
!  Discussion:
!
!    LINE_NCC_RULE_TEST tests the LINE_NCC_RULE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LINE_NCC_RULE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Test the LINE_NCC_RULE library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LINE_NCC_RULE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 computes and prints NCC rules.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  a = -1.0D+00
  b = +1.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  LINE_NCC_RULE computes the Newton-Cotes (closed) rule'
  write ( *, '(a)' ) '  using N equally spaced points for an interval [A,B].'

  do n = 1, 12

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call line_ncc_rule ( n, a, b, x, w )
    write ( *, '(a)' ) ''
    write ( *, '(a,i2)' ) '  Newton-Cotes (Closed) Rule #', n
    write ( *, '(a)' ) '   I       X(I)            W(I)'
    write ( *, '(a)' ) ' '
    do i = 1, n
      write ( *, '(2x,i2,2x,g14.6,2x,g14.6)' ) i, x(i), w(i)
    end do
    write ( *, '(2x,2x,2x,a,2x,g14.6)' ) '  Sum(|W)|) =', sum ( abs ( w(1:n) ) )

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 uses NCC rules to estimate the integral of exp(x) from 0 to 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 April 2014
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
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  a =  0.0D+00
  b = +1.0D+00
  exact = exp ( b ) - exp ( a )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Use a sequence of NCC rules to compute an estimate Q'
  write ( *, '(a)' ) '  of the integral:'
  write ( *, '(a)' ) '    I = integral ( 0 <= x <= 1 ) exp(x) dx.'
  write ( *, '(a)' ) '  The exact value is:'
  write ( *, '(a,g14.6)' ) '    I = ', exact

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   N       Q             |Q-I|'
  write ( *, '(a)' ) ' '

  do n = 1, 22

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call line_ncc_rule ( n, a, b, x, w )

    q = 0.0D+00
    do i = 1, n
      q = q + w(i) * exp ( x(i) )
    end do
    error = abs ( exact - q )
    write ( *, '(2x,i2,2x,g14.6,2x,e14.6)' ) n, q, error

    deallocate ( x )
    deallocate ( w )

  end do

  return
end
