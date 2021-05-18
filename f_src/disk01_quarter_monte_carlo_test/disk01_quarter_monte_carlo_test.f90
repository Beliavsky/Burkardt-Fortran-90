program main

!*****************************************************************************80
!
!! MAIN is the main program for DISK01_QUARTER_MONTE_CARLO_TEST.
!
!  Discussion:
!
!    DISK01_QUARTER_MONTE_CARLO_TEST tests DISK01_QUARTER_MONTE_CARLO.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 May 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DISK01_QUARTER_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the DISK01_QUARTER_MONTE_CARLO library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DISK01_QUARTER_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 uses DISK01_QUARTER_SAMPLE with an increasing number of points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00

  real ( kind = 8 ) disk01_quarter_area
  integer ( kind = 4 ) e(2)
  real ( kind = 8 ) err
  real ( kind = 8 ) exact
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Use DISK01_QUARTER_SAMPLE to estimate integrals'
  write ( *, '(a)' ) '  in the unit quarter disk.'

  do i = 0, 4
    e(1) = i
    do j = 0, 4 - e(1) 
      e(2) = j
      call disk01_quarter_monomial_integral ( e, exact )
      write ( *, '(a)' ) ''
      write ( *, '(a,i1,a,i1)' ) '  Estimate integral of X^', e(1), 'Y^', e(2)
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '         N        Estimate       Error'
      write ( *, '(a)' ) ''

      n = 1
      seed = 123456789

      do while ( n <= 65536 )

        allocate ( x(1:2,1:n) )
        call disk01_quarter_sample ( n, seed, x )

        allocate ( value(1:n) )
        call monomial_value ( 2, n, e, x, value )

        q = disk01_quarter_area ( ) * sum ( value(1:n) ) / real ( n, kind = 8 )

        err = abs ( q - exact )
        write ( *, '(2x,i8,2x,g14.6,2x,e10.2)' ) n, q, err

        deallocate ( value )
        deallocate ( x )

        n = 2 * n

      end do

      write ( *, '(2x,a8,2x,g14.6,2x,e10.2)' ) '  Exact:', exact, 0.0D+00

    end do

  end do

  return
end
