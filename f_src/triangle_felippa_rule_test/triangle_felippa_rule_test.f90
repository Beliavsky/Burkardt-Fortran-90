program main

!*****************************************************************************80
!
!! MAIN is the main program for TRIANGLE_FELIPPA_RULE_TEST.
!
!  Discussion:
!
!    TRIANGLE_FELIPPA_RULE_TEST tests the TRIANGLE_FELIPPA_RULE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) degree_max

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_FELIPPA_RULE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the TRIANGLE_FELIPPA_RULE library.'

  degree_max = 4
  call triangle_unit_monomial_test ( degree_max )

  degree_max = 7
  call triangle_unit_quad_test ( degree_max )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_FELIPPA_RULE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine triangle_unit_monomial_test ( degree_max )

!*****************************************************************************80
!
!! TRIANGLE_UNIT_MONOMIAL_TEST tests TRIANGLE_UNIT_MONOMIAL.
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
  integer ( kind = 4 ) expon(2)
  real ( kind = 8 ) triangle_unit_volume
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_UNIT_MONOMIAL_TEST'
  write ( *, '(a)' ) '  For the unit triangle,'
  write ( *, '(a)' ) '  TRIANGLE_UNIT_MONOMIAL returns the exact value of the'
  write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Volume = ', triangle_unit_volume ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     ALPHA      BETA      INTEGRAL'
  write ( *, '(a)' ) ' '

  do alpha = 0, degree_max
    expon(1) = alpha
    do beta = 0, degree_max - alpha
      expon(2) = beta
      call triangle_unit_monomial ( expon, value )
      write ( *, '(2x,i8,2x,i8,2x,g14.6)' ) expon(1:2), value
    end do
  end do

  return
end
subroutine triangle_unit_quad_test ( degree_max )

!*****************************************************************************80
!
!! TRIANGLE_UNIT_QUAD_TEST tests the rules for the unit triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2008
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

  integer ( kind = 4 ), parameter :: dim_num = 2

  integer ( kind = 4 ) degree_max
  integer ( kind = 4 ) expon(dim_num)
  integer ( kind = 4 ) h
  logical more
  integer ( kind = 4 ) order
  real ( kind = 8 ) quad
  integer ( kind = 4 ) t
  real ( kind = 8 ) triangle_unit_volume
  real ( kind = 8 ), allocatable :: v(:)
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: xy(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_UNIT_QUAD_TEST'
  write ( *, '(a)' ) '  For the unit triangle,'
  write ( *, '(a)' ) '  we approximate monomial integrals with:'
  write ( *, '(a)' ) '  TRIANGLE_UNIT_O01,'
  write ( *, '(a)' ) '  TRIANGLE_UNIT_O03,'
  write ( *, '(a)' ) '  TRIANGLE_UNIT_O03b,'
  write ( *, '(a)' ) '  TRIANGLE_UNIT_O06,'
  write ( *, '(a)' ) '  TRIANGLE_UNIT_O06b,'
  write ( *, '(a)' ) '  TRIANGLE_UNIT_O07,'
  write ( *, '(a)' ) '  TRIANGLE_UNIT_O012,'

  more = .false.

  do

    call subcomp_next ( degree_max, dim_num, expon, more, h, t )

    write ( *, '(a)' ) ' '
    write ( *, '(a,2x,i2,2x,i2)' ) '   Exponents: ', expon(1:dim_num)
    write ( *, '(a)' ) '   Order      Error'
    write ( *, '(a)' ) '   -----      -----'

    order = 1
    allocate ( v(1:order) )
    allocate ( w(1:order) )
    allocate ( xy(1:dim_num,1:order) )
    call triangle_unit_o01 ( w, xy )
    call monomial_value ( dim_num, order, expon, xy, v )
    quad = triangle_unit_volume ( ) * dot_product ( w(1:order), v(1:order) )
    write ( *, '(2x,i6,2x,g14.6)' ) order, quad
    deallocate ( v )
    deallocate ( w )
    deallocate ( xy )

    order = 3
    allocate ( v(1:order) )
    allocate ( w(1:order) )
    allocate ( xy(1:dim_num,1:order) )
    call triangle_unit_o03 ( w, xy )
    call monomial_value ( dim_num, order, expon, xy, v )
    quad = triangle_unit_volume ( ) * dot_product ( w(1:order), v(1:order) )
    write ( *, '(2x,i6,2x,g14.6)' ) order, quad
    deallocate ( v )
    deallocate ( w )
    deallocate ( xy )

    order = 3
    allocate ( v(1:order) )
    allocate ( w(1:order) )
    allocate ( xy(1:dim_num,1:order) )
    call triangle_unit_o03b ( w, xy )
    call monomial_value ( dim_num, order, expon, xy, v )
    quad = triangle_unit_volume ( ) * dot_product ( w(1:order), v(1:order) )
    write ( *, '(2x,i6,2x,g14.6)' ) order, quad
    deallocate ( v )
    deallocate ( w )
    deallocate ( xy )

    order = 6
    allocate ( v(1:order) )
    allocate ( w(1:order) )
    allocate ( xy(1:dim_num,1:order) )
    call triangle_unit_o06 ( w, xy )
    call monomial_value ( dim_num, order, expon, xy, v )
    quad = triangle_unit_volume ( ) * dot_product ( w(1:order), v(1:order) )
    write ( *, '(2x,i6,2x,g14.6)' ) order, quad
    deallocate ( v )
    deallocate ( w )
    deallocate ( xy )

    order = 6
    allocate ( v(1:order) )
    allocate ( w(1:order) )
    allocate ( xy(1:dim_num,1:order) )
    call triangle_unit_o06b ( w, xy )
    call monomial_value ( dim_num, order, expon, xy, v )
    quad = triangle_unit_volume ( ) * dot_product ( w(1:order), v(1:order) )
    write ( *, '(2x,i6,2x,g14.6)' ) order, quad
    deallocate ( v )
    deallocate ( w )
    deallocate ( xy )

    order = 7
    allocate ( v(1:order) )
    allocate ( w(1:order) )
    allocate ( xy(1:dim_num,1:order) )
    call triangle_unit_o07 ( w, xy )
    call monomial_value ( dim_num, order, expon, xy, v )
    quad = triangle_unit_volume ( ) * dot_product ( w(1:order), v(1:order) )
    write ( *, '(2x,i6,2x,g14.6)' ) order, quad
    deallocate ( v )
    deallocate ( w )
    deallocate ( xy )

    order = 12
    allocate ( v(1:order) )
    allocate ( w(1:order) )
    allocate ( xy(1:dim_num,1:order) )
    call triangle_unit_o12 ( w, xy )
    call monomial_value ( dim_num, order, expon, xy, v )
    quad = triangle_unit_volume ( ) * dot_product ( w(1:order), v(1:order) )
    write ( *, '(2x,i6,2x,g14.6)' ) order, quad
    deallocate ( v )
    deallocate ( w )
    deallocate ( xy )

    call triangle_unit_monomial ( expon, quad )
    write ( *, '(2x,a,2x,g14.6)' ) ' Exact', quad

    if ( .not. more ) then
      exit
    end if

  end do

  return
end




