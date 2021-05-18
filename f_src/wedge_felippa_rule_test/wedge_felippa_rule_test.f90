program main

!*****************************************************************************80
!
!! MAIN is the main program for WEDGE_FELIPPA_RULE_TEST.
!
!  Discussion:
!
!    WEDGE_FELIPPA_TEST tests the WEDGE_FELIPPA library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) degree_max

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WEDGE_FELIPPA_RULE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the WEDGE_FELIPPA_RULE library.'

  degree_max = 4

  call test01 ( degree_max )
  call test02 ( degree_max )

  call test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'WEDGE_FELIPPA_RULE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( degree_max )

!*****************************************************************************80
!
!! TEST01 tests WEDGE_INTEGRAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DEGREE_MAX, the maximum total degree of the
!    monomials to check.
!
  implicit none

  integer ( kind = 4 ) alpha
  integer ( kind = 4 ) beta
  integer ( kind = 4 ) degree_max
  integer ( kind = 4 ) expon(3)
  integer ( kind = 4 ) gamma
  real ( kind = 8 ) value
  real ( kind = 8 ) wedge_volume

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  For the unit wedge,'
  write ( *, '(a)' ) '  WEDGE_INTEGRAL returns the exact value of the'
  write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA Z^GAMMA'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Volume = ', wedge_volume ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     ALPHA      BETA     GAMMA      INTEGRAL'
  write ( *, '(a)' ) ' '

  do alpha = 0, degree_max
    expon(1) = alpha
    do beta = 0, degree_max - alpha
      expon(2) = beta
      do gamma = 0, degree_max - alpha - beta
        expon(3) = gamma
        call wedge_integral ( expon, value )
        write ( *, '(2x,i8,2x,i8,2x,i8,2x,g14.6)' ) expon(1:3), value
      end do
    end do
  end do

  return
end
subroutine test02 ( degree_max )

!*****************************************************************************80
!
!! TEST02 tests the rules for the unit wedge.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DEGREE_MAX, the maximum total degree of the
!    monomials to check.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3
  integer ( kind = 4 ), parameter :: test_num = 7

  integer ( kind = 4 ) degree_max
  integer ( kind = 4 ) expon(dim_num)
  integer ( kind = 4 ) h
  integer ( kind = 4 ) line_order
  integer ( kind = 4 ) :: line_order_array(test_num) = (/ &
    1, 2, 2, 3, 2, 3, 4 /)
  logical more
  integer ( kind = 4 ) order
  real ( kind = 8 ) quad
  integer ( kind = 4 ) t
  integer ( kind = 4 ) test
  integer ( kind = 4 ) triangle_order
  integer ( kind = 4 ) :: triangle_order_array(test_num) = (/ &
    1, 3, -3, 6, -6, 7, 12 /)
  real ( kind = 8 ) wedge_volume
  real ( kind = 8 ), allocatable :: v(:)
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: xyz(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  For the unit wedge,'
  write ( *, '(a)' ) '  we approximate monomial integrals with WEDG_UNIT_RULE.'

  more = .false.

  do

    call subcomp_next ( degree_max, dim_num, expon, more, h, t )

    if ( mod ( expon(3), 2 ) == 1 ) then
      if ( .not. more ) then
        exit
      else
        cycle
      end if
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a,2x,i2,2x,i2,2x,i2)' ) &
      '  Monomial exponents: ', expon(1:dim_num)
    write ( *, '(a)' ) ' '

    do test = 1, test_num

      line_order = line_order_array(test)
      triangle_order = triangle_order_array(test)

      order = line_order * abs ( triangle_order )

      allocate ( v(1:order) )
      allocate ( w(1:order) )
      allocate ( xyz(1:dim_num,1:order) )
      call wedge_rule ( line_order, triangle_order, w, xyz )
      call monomial_value ( dim_num, order, expon, xyz, v )
      quad = wedge_volume ( ) * dot_product ( w(1:order), v(1:order) )
      write ( *, '(2x,i6,2x,i6,2x,i6,2x,g14.6)' ) &
        triangle_order, line_order, order, quad
      deallocate ( v )
      deallocate ( w )
      deallocate ( xyz )

    end do

    write ( *, '(a)' ) ' '
    call wedge_integral ( expon, quad )
    write ( *, '(2x,a,2x,6x,2x,6x,2x,g14.6)' ) ' Exact', quad

    if ( .not. more ) then
      exit
    end if

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 writes out some rules for the unit wedge.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3
  integer ( kind = 4 ), parameter :: rule_num = 7

  integer ( kind = 4 ) line_order
  integer ( kind = 4 ) :: line_order_array(rule_num) = (/ &
    1, 2, 2, 3, 2, 3, 4 /)
  integer ( kind = 4 ) order
  integer ( kind = 4 ) rule
  integer ( kind = 4 ) triangle_order
  integer ( kind = 4 ) :: triangle_order_array(rule_num) = (/ &
    1, 3, -3, 6, -6, 7, 12 /)
  real ( kind = 8 ), allocatable :: w(:)
  character ( len = 255 ) w_filename
  real ( kind = 8 ), allocatable :: x(:,:)
  character ( len = 255 ) x_filename

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  For the unit wedge,'
  write ( *, '(a)' ) '  write some rules to a file'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   Rule  Trig    Line   Total  W_File X_File'
  write ( *, '(a)' ) '         Order   Order  Order'
  write ( *, '(a)' ) ' '

  do rule = 1, rule_num

    if ( rule == 1 ) then
      w_filename = 'wedge_felippa_1x1_w.txt'
      x_filename = 'wedge_felippa_1x1_x.txt'
    else if ( rule == 2 ) then
      w_filename = 'wedge_felippa_3x2_w.txt'
      x_filename = 'wedge_felippa_3x2_x.txt'
    else if ( rule == 3 ) then
      w_filename = 'wedge_felippa_3bx2_w.txt'
      x_filename = 'wedge_felippa_3bx2_x.txt'
    else if ( rule == 4 ) then
      w_filename = 'wedge_felippa_6x3_w.txt'
      x_filename = 'wedge_felippa_6x3_x.txt'
    else if ( rule == 5 ) then
      w_filename = 'wedge_felippa_6bx2_w.txt'
      x_filename = 'wedge_felippa_6bx2_x.txt'
    else if ( rule == 6 ) then
      w_filename = 'wedge_felippa_7x3_w.txt'
      x_filename = 'wedge_felippa_7x3_x.txt'
    else if ( rule == 7 ) then
      w_filename = 'wedge_felippa_12x4_w.txt'
      x_filename = 'wedge_felippa_12x4_x.txt'
    end if

    line_order = line_order_array(rule)
    triangle_order = triangle_order_array(rule)

    order = line_order * abs ( triangle_order )

    allocate ( w(1:order) )
    allocate ( x(1:dim_num,1:order) )
    call wedge_rule ( line_order, triangle_order, w, x )
    call r8mat_write ( w_filename, 1, order, w )
    call r8mat_write ( x_filename, dim_num, order, x )
    write ( *, '(2x,i6,2x,i6,2x,i6,2x,i6,2x,a25,2x,a25)' ) &
      rule, triangle_order, line_order, order, w_filename, x_filename

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
