program main

!*****************************************************************************80
!
!! MAIN is the main program for ANNULUS_MONTE_CARLO_TEST.
!
!  Discussion:
!
!    ANNULUS_MONTE_CARLO_TEST tests ANNULUS_MONTE_CARLO.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) center(2)
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANNULUS_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the ANNULUS_MONTE_CARLO library.'

  call annulus_area_test ( )

  center = (/ 0.0D+00, 0.0D+00 /)
  r1 = 0.0D+00
  r2 = 1.0D+00
  call annulus_sample_test ( center, r1, r2 )

  center = (/ 0.0D+00, 0.0D+00 /)
  r1 = 0.5D+00
  r2 = 1.0D+00
  call annulus_sample_test ( center, r1, r2 )

  center = (/ 1.0D+00, 0.0D+00 /)
  r1 = 0.0D+00
  r2 = 1.0D+00
  call annulus_sample_test ( center, r1, r2 )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANNULUS_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
subroutine annulus_area_test ( )

!*****************************************************************************80
!
!! ANNULUS_AREA_TEST test ANNULUS_AREA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) annulus_area
  real ( kind = 8 ) area
  real ( kind = 8 ) center(2)
  real ( kind = 8 ) dat(4)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANNULUS_AREA_TEST'
  write ( *, '(a)' ) '  ANNULUS_AREA computes the area of an annulus with'
  write ( *, '(a)' ) '  center = (CX,CY), inner radius R1 and outer radius R2.'
  
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  (   CX        CY     )    R1         R2         Area'
  write ( *, '(a)' ) ''

  do i = 1, 10
    call r8vec_uniform_01 ( 4, seed, dat )
    center(1) = 10.0D+00 * dat(1) - 5.0D+00
    center(2) = 10.0D+00 * dat(2) - 5.0D+00
    r1 = dat(3)
    r2 = r1 + dat(4)
    area = annulus_area ( center, r1, r2 )
    write ( *, '(2x,a1,f9.6,a1,f9.6,a1,2x,f9.6,2x,f9.6,2x,f9.6)' ) &
      '(', center(1), ',', center(2), ',', r1, r2, area
  end do

  return
end
subroutine annulus_sample_test ( center, r1, r2 )

!*****************************************************************************80
!
!! ANNULUS_SAMPLE_TEST uses ANNULUS_SAMPLE to estimate integrals.
!
!  Discussion:
!
!    If CENTER=(0,0) and R1 = 0 and R2 = 1, then we can compare exact values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) annulus_area
  real ( kind = 8 ) center(2)
  integer ( kind = 4 ) e(2)
  integer ( kind = 4 ) :: e_test(2,7) = reshape ( (/ &
    0, 0, &
    2, 0, &
    0, 2, &
    4, 0, &
    2, 2, &
    0, 4, &
    6, 0 /), (/ 2, 7 /) )
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) result
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANNULUS_SAMPLE_TEST'
  write ( *, '(a)' ) '  ANNULUS_SAMPLE can sample an annulus uniformly.'
  write ( *, '(a)' ) '  Use it to estimate integrals in the annulus'
  write ( *, '(a,f8.4,a,f8.4,a,f8.4,a,f8.4)' ) &
    '  centered at (', center(1), ',', center(2), &
    ') with R1 = ', r1, ' R2 = ', r2

  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)', advance = 'no' ) &
    '         N        1              X^2             Y^2        '
  write ( *, '(a)' ) '     X^4             X^2Y^2           Y^4             X^6'
  write ( *, '(a)' ) ''

  n = 1

  do while ( n <= 65536 )

    allocate ( x(1:2,1:n) )
    allocate ( value(1:n) )

    call annulus_sample ( center, r1, r2, n, seed, x )

    write ( *, '(2x,i8)', advance = 'no' ) n
    do j = 1, 7
      e(1:2) = e_test(1:2,j)
      call monomial_value ( 2, n, e, x, value )
      result = annulus_area ( center, r1, r2 ) &
        * sum ( value(1:n) ) / real ( n, kind = 8 )
      write ( *, '(2x,g14.6)', advance = 'no' ) result
    end do
    write ( *, '(a)' ) ''

    deallocate ( value )
    deallocate ( x )

    n = 2 * n

  end do

  if ( &
    center(1) == 0.0D+00 .and. &
    center(2) == 0.0D+00 .and. &
    r1 == 0.0D+00 .and. &
    r2 == 1.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)', advance = 'no' ) '     Exact'
    do j = 1, 7
      e(1:2) = e_test(1:2,j)
      call disk01_monomial_integral ( e, result )
      write ( *, '(2x,g14.6)', advance = 'no' ) result
    end do
    write ( *, '(a)' ) ''

  end if

  return
end
