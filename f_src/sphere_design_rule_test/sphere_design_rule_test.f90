program main

!*****************************************************************************80
!
!! MAIN is the main program for SPHERE_DESIGN_RULE_TEST.
!
!  Discussion:
!
!    SPHERE_DESIGN_RULE_TEST tests SPHERE_DESIGN_RULE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 March 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPHERE_DESIGN_RULE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test SPHERE_DESIGN_RULE.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPHERE_DESIGN_RULE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests DESIGN_QUAD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 March 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) e(3)
  integer ( kind = 4 ) i
  real ( kind = 8 ) integral
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  integer ( kind = 4 ) order
  integer ( kind = 4 ) order_max
  real ( kind = 8 ) quad
  real ( kind = 8 ), parameter :: r = 1.0D+00
  real ( kind = 8 ) sphere_area
  real ( kind = 8 ), allocatable :: x(:,:)
  real ( kind = 8 ), dimension ( 3 ) :: xc = (/ 0.0D+00, 0.0D+00, 0.0D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  DESIGN_QUAD returns the average value of a'
  write ( *, '(a)' ) '  function F(X,Y,Z) at the points of a spherical'
  write ( *, '(a)' ) '  design.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  For this test, we will use single polynomial terms.'

  call sphere_area_3d ( r, sphere_area )

  call design_order_max ( order_max )

  do k = 1, 17

    if ( k == 1 ) then
      e(1:3) = (/ 0, 0, 0 /)
    else if ( k == 2 ) then
      e(1:3) = (/ 1, 0, 0 /)
    else if ( k == 3 ) then
      e(1:3) = (/ 0, 1, 0 /)
    else if ( k == 4 ) then
      e(1:3) = (/ 0, 0, 1 /)
    else if ( k == 5 ) then
      e(1:3) = (/ 2, 0, 0 /)
    else if ( k == 6 ) then
      e(1:3) = (/ 0, 2, 2 /)
    else if ( k == 7 ) then
      e(1:3) = (/ 2, 2, 2 /)
    else if ( k == 8 ) then
      e(1:3) = (/ 0, 2, 4 /)
    else if ( k == 9 ) then
      e(1:3) = (/ 0, 0, 6 /)
    else if ( k == 10 ) then
      e(1:3) = (/ 1, 2, 4 /)
    else if ( k == 11 ) then
      e(1:3) = (/ 2, 4, 2 /)
    else if ( k == 12 ) then
      e(1:3) = (/ 6, 2, 0 /)
    else if ( k == 13 ) then
      e(1:3) = (/ 0, 0, 8 /)
    else if ( k == 14 ) then
      e(1:3) = (/ 6, 0, 4 /)
    else if ( k == 15 ) then
      e(1:3) = (/ 4, 6, 2 /)
    else if ( k == 16 ) then
      e(1:3) = (/ 2, 4, 8 /)
    else if ( k == 17 ) then
      e(1:3) = (/ 16, 0, 0 /)
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(2x,a,i2,a,i2,a,i2)' ) &
      'F(x,y,z) = x^', e(1), 'y^', e(2), 'z^', e(3)
    write ( *, '(a)' ) ' Order   Size  Quad          Integral'
    write ( *, '(a)' ) ' '

    call sphere_monomial_int_3d ( r, e, integral )

    write ( *, '(a6,20x,g14.6)' ) 'Exact:', integral

    do order = 1, order_max

      call design_size ( order, n )

      allocate ( x(1:3,1:n) )

      call design_points ( order, n, x )
!
!  Map S(0,1) points to S(C,R).
!
      x(1:3,1:n) = r * x(1:3,1:n)
      do i = 1, 3
        x(i,1:n) = x(i,1:n) + xc(i)
      end do
!
!  Sum the function values at the design points, and average.
!
      quad = 0.0D+00
      do j = 1, n
        quad = quad + x(1,j)**e(1) * x(2,j)**e(2) * x(3,j)**e(3)
      end do

      quad = quad * r**2 / real ( n, kind = 8 )

      deallocate ( x )

      integral = quad * sphere_area

      write ( *, '(i6,i6,2g14.6)' ) order, n, quad, integral

    end do
 
  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests DESIGN_QUAD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 March 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) e(3)
  integer ( kind = 4 ) i
  real ( kind = 8 ) integral
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  integer ( kind = 4 ) order
  integer ( kind = 4 ) order_max
  real ( kind = 8 ) quad
  real ( kind = 8 ), parameter :: r = 2.0D+00
  real ( kind = 8 ) sphere_area
  real ( kind = 8 ), allocatable :: x(:,:)
  real ( kind = 8 ), dimension ( 3 ) :: xc = (/ 1.0D+00, 2.0D+00, 3.0D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  DESIGN_QUAD returns the average value of a'
  write ( *, '(a)' ) '  function F(X,Y,Z) at the points of a spherical'
  write ( *, '(a)' ) '  design.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  For this test, we will use single polynomial terms.'

  call sphere_area_3d ( r, sphere_area )

  call design_order_max ( order_max )

  do k = 1, 17

    if ( k == 1 ) then
      e(1:3) = (/ 0, 0, 0 /)
    else if ( k == 2 ) then
      e(1:3) = (/ 1, 0, 0 /)
    else if ( k == 3 ) then
      e(1:3) = (/ 0, 1, 0 /)
    else if ( k == 4 ) then
      e(1:3) = (/ 0, 0, 1 /)
    else if ( k == 5 ) then
      e(1:3) = (/ 2, 0, 0 /)
    else if ( k == 6 ) then
      e(1:3) = (/ 0, 2, 2 /)
    else if ( k == 7 ) then
      e(1:3) = (/ 2, 2, 2 /)
    else if ( k == 8 ) then
      e(1:3) = (/ 0, 2, 4 /)
    else if ( k == 9 ) then
      e(1:3) = (/ 0, 0, 6 /)
    else if ( k == 10 ) then
      e(1:3) = (/ 1, 2, 4 /)
    else if ( k == 11 ) then
      e(1:3) = (/ 2, 4, 2 /)
    else if ( k == 12 ) then
      e(1:3) = (/ 6, 2, 0 /)
    else if ( k == 13 ) then
      e(1:3) = (/ 0, 0, 8 /)
    else if ( k == 14 ) then
      e(1:3) = (/ 6, 0, 4 /)
    else if ( k == 15 ) then
      e(1:3) = (/ 4, 6, 2 /)
    else if ( k == 16 ) then
      e(1:3) = (/ 2, 4, 8 /)
    else if ( k == 17 ) then
      e(1:3) = (/ 16, 0, 0 /)
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(2x,a,i2,a,i2,a,i2)' ) &
      'F(x,y,z) = x^', e(1), 'y^', e(2), 'z^', e(3)
    write ( *, '(a)' ) ' Order   Size  Quad          Integral'
    write ( *, '(a)' ) ' '

    do order = 1, order_max

      call design_size ( order, n )

      allocate ( x(1:3,1:n) )

      call design_points ( order, n, x )
!
!  Map S(0,1) points to S(C,R).
!
      x(1:3,1:n) = r * x(1:3,1:n)
      do i = 1, 3
        x(i,1:n) = x(i,1:n) + xc(i)
      end do
!
!  Sum the function values at the design points, and average.
!
      quad = 0.0D+00
      do j = 1, n
        quad = quad + x(1,j)**e(1) * x(2,j)**e(2) * x(3,j)**e(3)
      end do

      quad = quad * r**2 / real ( n, kind = 8 )

      deallocate ( x )

      integral = quad * sphere_area

      write ( *, '(i6,i6,2g14.6)' ) order, n, quad, integral

    end do
 
  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 writes a sphere design rule to a file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 July 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 80 ) design_filename
  real ( kind = 8 ) x(3,180)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  R8MAT_WRITE can write a sphere design rule to a file.'

  call design_18_180_3d ( x )

  design_filename = 'sphere_design_rule_18.txt'

  call r8mat_write ( design_filename, 3, 180, x )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Sphere design rule of 180 points written to "' &
    // trim ( design_filename ) // '".'

  return
end

