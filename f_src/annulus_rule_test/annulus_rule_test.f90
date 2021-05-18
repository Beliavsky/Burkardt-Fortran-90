program main

!*****************************************************************************80
!
!! MAIN is the main program for ANNULUS_RULE_TEST.
!
!  Discussion:
!
!    ANNULUS_RULE_TEST tests ANNULUS_RULE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2018
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
  write ( *, '(a)' ) 'ANNULUS_RULE_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test ANNULUS_RULE.'

  call annulus_area_test ( )
  call annulus_rule_compute_test ( )

  center = (/ 0.0D+00, 0.0D+00 /)
  r1 = 0.0D+00
  r2 = 1.0D+00
  call annulus_rule_monomial_test ( center, r1, r2 )

  center = (/ 0.0D+00, 0.0D+00 /)
  r1 = 0.5D+00
  r2 = 1.0D+00
  call annulus_rule_monomial_test ( center, r1, r2 )

  center = (/ 1.0D+00, 0.0D+00 /)
  r1 = 0.0D+00
  r2 = 1.0D+00
  call annulus_rule_monomial_test ( center, r1, r2 )

  center = (/ 0.0D+00, 0.0D+00 /)
  r1 = 0.0D+00
  r2 = 1.0D+00
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANNULUS_RULE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
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
!    06 July 2018
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
    center(1) = 10.0 * dat(1) - 5.0
    center(2) = 10.0 * dat(2) - 5.0
    r1 = dat(3)
    r2 = r1 + dat(4)
    area = annulus_area ( center, r1, r2 )
    write ( *, '(2x,a,f9.6,a,f9.6,a,2x,f9.6,2x,f9.6,2x,f9.6)' ) &
    '(', center(1), ',', center(2), ')', r1, r2, area
  end do

  return
end
subroutine annulus_rule_compute_test ( )

!*****************************************************************************80
!
!! ANNULUS_RULE_COMPUTE_TEST tests ANNULUS_RULE_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) center(2)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nr
  integer ( kind = 4 ) nt
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANNULUS_RULE_COMPUTE_TEST:'
  write ( *, '(a)' ) '  Test ANNULUS_RULE_COMPUTE.'

  center = (/ 0.0D+00, 0.0D+00 /)
  r1 = 0.5D+00
  r2 = 1.0D+00
  nr = 3
  nt = 12
  n = nt * nr

  allocate ( w(1:n) )
  allocate ( x(1:n) )
  allocate ( y(1:n) )

  call annulus_rule_compute ( center, r1, r2, nr, nt, w, x, y )

  call r8vec3_print ( n, w, x, y, '  W, X, Y for annulus quadrature:' )

  deallocate ( w )
  deallocate ( x )
  deallocate ( y )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANNULUS_RULE_COMPUTE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'

  return
end
subroutine annulus_rule_monomial_test ( center, r1, r2 )

!*****************************************************************************80
!
!! ANNULUS_RULE_MONOMIAL_TEST estimates monomial integrals using quadrature
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
!    06 July 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CENTER(2), the coordinates of the center.
!
!    Input, real ( kind = 8 ) R1, R2, the inner and outer radii of the annulus.
!    0.0 <= R1 <= R2.
!
  implicit none

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
  integer ( kind = 4 ) nr
  integer ( kind = 4 ) nt
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) result
  real ( kind = 8 ), allocatable :: value(:)
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: xy(:,:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ANNULUS_RULE_MONOMIAL_TEST'
  write ( *, '(a)' ) '  ANNULUS_RULE_COMPUTE can supply a quadrature rule for'
  write ( *, '(a,f8.4,a,f8.4,a,f8.4,a,f8.4)' ) &
    '  the annulus centered at (', center(1), ',', center(2), &
    ') with R1 = ', r1, ' R2 = ', r2

  write ( *, '(a)' ) ''
  write ( *, '(a)', advance = 'no' ) &
    '    NR    NT       1              X^2             Y^2        '
  write ( *, '(a)' ) '     X^4             X^2Y^2          Y^4             X^6'
  write ( *, '(a)' ) ''

  nr = 4

  do while ( nr <= 64 )

    nt = 4 * nr

    n = nr * nt

    allocate ( value(1:n) )
    allocate ( w(1:n) )
    allocate ( x(1:n) )
    allocate ( xy(1:2,1:n) )
    allocate ( y(1:n) )

    call annulus_rule_compute ( center, r1, r2, nr, nt, w, x, y )

    xy(1,1:n) = x(1:n)
    xy(2,1:n) = y(1:n)

    write ( *, '(2x,i4,2x,i4)', advance = 'no' ) nr, nt
    do j = 1, 7
      e(1:2) = e_test(1:2,j)
      call monomial_value ( 2, n, e, xy, value )
      result = dot_product ( w(1:n), value(1:n) )
      write ( *, '(2x,g14.6)', advance = 'no' ) result
    end do
    write ( *, '(a)' ) ''

    deallocate ( value )
    deallocate ( w )
    deallocate ( x )
    deallocate ( xy )
    deallocate ( y )

    nr = 2 * nr

  end do

  if ( &
    center(1) == 0.0D+00 .and. &
    center(2) == 0.0D+00 .and. &
    r1 == 0.0D+00 .and. &
    r2 == 1.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)', advance = 'no' ) '     Exact  '
    do j = 1, 7
      e(1:2) = e_test(1:2,j)
      call disk01_monomial_integral ( e, result )
      write ( *, '(2x,g14.6)', advance = 'no' ) result
    end do
    write ( *, '(a)' ) ''

  end if

  return
end

