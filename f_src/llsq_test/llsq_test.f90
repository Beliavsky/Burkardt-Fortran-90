program main

!*****************************************************************************80
!
!! MAIN is the main program for LLSQ_TEST.
!
!  Discussion:
!
!    LLSQ_TEST tests the LLSQ library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 January 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LLSQ_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LLSQ library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LLSQ_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 calls LLSQ to match 15 data values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 March 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 15

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) error
  integer ( kind = 4 ) i

  real ( kind = 8 ) :: x(n) = (/ & 
    1.47, 1.50, 1.52, 1.55, 1.57, &
    1.60, 1.63, 1.65, 1.68, 1.70, &
    1.73, 1.75, 1.78, 1.80, 1.83 /)
  real ( kind = 8 ) :: y(n) = (/ &
    52.21, 53.12, 54.48, 55.84, 57.20, &
    58.57, 59.93, 61.29, 63.11, 64.47, &
    66.28, 68.10, 69.92, 72.19, 74.46 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  LLSQ can compute the formula for a line of the form'
  write ( *, '(a)' ) '    y = A * x + B'
  write ( *, '(a)' ) '  which minimizes the RMS error to a set of N data values.'

  call llsq ( n, x, y, a, b )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6,a,g14.6)' ) '  Estimated relationship is y = ', a, ' * x + ', b
  write ( *, '(a)' ) '  Expected value is         y = 61.272 * x - 39.062'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I      X       Y      B+A*X    |error|'
  write ( *, '(a)' ) ' '
  error = 0.0D+00
  do i = 1, n
    write ( *, '(2x,i4,2x,f7.4,2x,f7.4,2x,f7.4,2x,f7.4)' ) &
      i, x(i), y(i), b + a * x(i), b + a * x(i) - y(i)
    error = error + ( b + a * x(i) - y(i) )**2
  end do
  error = sqrt ( error / real ( n, kind = 8 ) )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  RMS error =                     ', error

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 calls LLSQ to match 14 data values with a line y=a*x.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 January 2019
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 14

  real ( kind = 8 ) a
  real ( kind = 8 ) error
  integer ( kind = 4 ) i
  real ( kind = 8 ) :: x(n) = (/ & 
    0.00D+00, 0.10D+00, 0.15D+00, 0.20D+00, 0.25D+00, &
    0.30D+00, 0.35D+00, 0.40D+00, 0.45D+00, 0.50D+00, &
    0.55D+00, 0.60D+00, 0.65D+00, 0.70D+00 /)
  real ( kind = 8 ) :: y(n) = (/ &
    0.0000D+00,  0.0865D+00,  0.1015D+00,  0.1106D+00,  0.1279D+00, &
    0.1892D+00,  0.2695D+00,  0.2888D+00,  0.2425D+00,  0.3465D+00, &
    0.3225D+00,  0.3764D+00,  0.4263D+00,  0.4562D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  LLSQ0 can compute the formula for a line of the form'
  write ( *, '(a)' ) '    y = A * x'
  write ( *, '(a)' ) '  which minimizes the RMS error to a set of N data values.'

  call llsq0 ( n, x, y, a )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6,a)' ) '  Estimated relationship is y = ', a, ' * x'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I      X       Y        A*X    |error|'
  write ( *, '(a)' ) ' '
  error = 0.0D+00
  do i = 1, n
    write ( *, '(2x,i4,2x,f7.4,2x,f7.4,2x,f7.4,2x,f7.4)' ) &
      i, x(i), y(i), a * x(i), a * x(i) - y(i)
    error = error + ( a * x(i) - y(i) )**2
  end do
  error = sqrt ( error / real ( n, kind = 8 ) )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  RMS error =                     ', error

  return
end


