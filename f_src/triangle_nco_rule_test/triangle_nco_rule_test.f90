program main

!*****************************************************************************80
!
!! MAIN is the main program for TRIANGLE_NCO_RULE_TEST.
!
!  Discussion:
!
!    TRIANGLE_NCO_RULE_TEST tests the TRIANGLE_NCO_RULE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_NCO_RULE_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the TRIANGLE_NCO_RULE library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TRIANGLE_NCO_RULE_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests TRIANGLE_NCO_RULE_NUM, TRIANGLE_NCO_DEGREE, TRIANGLE_NCO_ORDER_NUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) degree
  integer ( kind = 4 ) order_num
  integer ( kind = 4 ) rule
  integer ( kind = 4 ) rule_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  TRIANGLE_NCO_RULE_NUM returns the number of rules;'
  write ( *, '(a)' ) '  TRIANGLE_NCO_DEGREE returns the degree of a rule;'
  write ( *, '(a)' ) '  TRIANGLE_NCO_ORDER_NUM returns the order of a rule.'

  call triangle_nco_rule_num ( rule_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of available rules = ', rule_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      Rule      Degree     Order'
  write ( *, '(a)' ) ' '

  do rule = 1, rule_num
    call triangle_nco_order_num ( rule, order_num )
    call triangle_nco_degree ( rule, degree )
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) rule, degree, order_num
  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests TRIANGLE_NCO_RULE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) order_num
  integer ( kind = 4 ) rule
  integer ( kind = 4 ) rule_num
  real ( kind = 8 ), allocatable, dimension ( : ) :: wtab
  real ( kind = 8 ) wtab_sum
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: xytab

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  TRIANGLE_NCO_RULE returns the points and weights'
  write ( *, '(a)' ) '  of an NCO rule for the triangle.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In this test, we simply check that the weights'
  write ( *, '(a)' ) '  sum to 1.'

  call triangle_nco_rule_num ( rule_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of available rules = ', rule_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      Rule     Sum of weights'
  write ( *, '(a)' ) ' '

  do rule = 1, rule_num

    call triangle_nco_order_num ( rule, order_num )

    allocate ( wtab(1:order_num) )
    allocate ( xytab(1:2,1:order_num) )

    call triangle_nco_rule ( rule, order_num, xytab, wtab )

    wtab_sum = sum ( wtab(1:order_num) )

    write ( *, '(2x,i8,2x,g25.16)' ) rule, wtab_sum

    deallocate ( wtab )
    deallocate ( xytab )

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests TRIANGLE_NCO_RULE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) rule
  integer ( kind = 4 ) rule_num
  integer ( kind = 4 ) suborder
  integer ( kind = 4 ) suborder_num
  real ( kind = 8 ), allocatable, dimension ( : ) :: suborder_w
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: suborder_xyz
  real ( kind = 8 ) xyz_sum

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  TRIANGLE_NCO_RULE returns the points and weights'
  write ( *, '(a)' ) '  of an NCO rule for the triangle.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In this test, we simply check that, for each'
  write ( *, '(a)' ) '  quadrature point, the barycentric coordinates'
  write ( *, '(a)' ) '  sum to 1.'

  call triangle_nco_rule_num ( rule_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      Rule   Suborder    Sum of coordinates'
  write ( *, '(a)' ) ' '

  do rule = 1, rule_num

    call triangle_nco_suborder_num ( rule, suborder_num )

    allocate ( suborder_w(1:suborder_num) )
    allocate ( suborder_xyz(1:3,1:suborder_num) )

    call triangle_nco_subrule ( rule, suborder_num, suborder_xyz, suborder_w )

    write ( *, '(a)' ) ' '
    write ( *, '(2x,i8,2x,i8)' ) rule, suborder_num
    do suborder = 1, suborder_num
      xyz_sum = sum ( suborder_xyz(1:3,suborder) )
      write ( *, '(20x,2x,g25.16)' ) xyz_sum
    end do

    deallocate ( suborder_w )
    deallocate ( suborder_xyz )

  end do

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests TRIANGLE_NCO_RULE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) area
  integer ( kind = 4 ) b
  real ( kind = 8 ) coef
  integer ( kind = 4 ) rule
  integer ( kind = 4 ) rule_num
  real ( kind = 8 ) err
  real ( kind = 8 ) exact
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order_num
  real ( kind = 8 ) quad
  real ( kind = 8 ) value
  real ( kind = 8 ), allocatable, dimension ( : ) :: wtab
  real ( kind = 8 ) x
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: xytab
  real ( kind = 8 ) y

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  TRIANGLE_NCO_RULE returns the points and weights of'
  write ( *, '(a)' ) '  an NCO rule for the unit triangle.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  This routine uses those rules to estimate the'
  write ( *, '(a)' ) '  integral of monomomials in the unit triangle.'

  call triangle_nco_rule_num ( rule_num )

  area = 0.5D+00

  do a = 0, 10

    do b = 0, 10 - a
!
!  Multiplying X^A * Y^B by COEF will give us an integrand
!  whose integral is exactly 1.  This makes the error calculations easy.
!
      coef = real ( a + b + 2, kind = 8 ) * real ( a + b + 1, kind = 8 )
      do i = 1, b
        coef = coef * real ( a + i, kind = 8 ) / real ( i, kind = 8 )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6,a,i8,a,i8)' ) &
        '  Integrate ', coef , ' * X ** ', a, ' * Y ** ', b
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      Rule       QUAD           ERROR'
      write ( *, '(a)' ) ' '

      do rule = 1, rule_num

        call triangle_nco_order_num ( rule, order_num )

        allocate ( wtab(1:order_num) )
        allocate ( xytab(1:2,1:order_num) )

        call triangle_nco_rule ( rule, order_num, xytab, wtab )

        quad = 0.0D+00

        do i = 1, order_num

          x = xytab(1,i)
          y = xytab(2,i)

          if ( a == 0 .and. b == 0 ) then
            value = coef
          else if ( a == 0 .and. b /= 0 ) then
            value = coef        * y**b
          else if ( a /= 0 .and. b == 0 ) then
            value = coef * x**a
          else if ( a /= 0 .and. b /= 0 ) then
            value = coef * x**a * y**b
          end if

          quad = quad + wtab(i) * value

        end do

        quad = area * quad

        exact = 1.0D+00
        err = abs ( exact - quad )

        write ( *, '(2x,i8,2x,g14.6,2x,f14.8)' ) rule, quad, err

        deallocate ( wtab )
        deallocate ( xytab )

      end do

    end do

  end do

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 demonstrates REFERENCE_TO_PHYSICAL_T3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2
  integer ( kind = 4 ), parameter :: node_num = 3

  real ( kind = 8 ) area
  real ( kind = 8 ) area2
  integer ( kind = 4 ) node
  real ( kind = 8 ), dimension ( dim_num, node_num ) :: node_xy = reshape ( (/ &
    0.0D+00, 0.0D+00, &
    1.0D+00, 0.0D+00, &
    0.0D+00, 1.0D+00 /), (/ dim_num, node_num /) )
  real ( kind = 8 ), dimension ( dim_num, node_num ) :: node_xy2 = reshape ( (/ &
    1.0D+00, 2.0D+00, &
    1.0D+00, 1.0D+00, &
    3.0D+00, 2.0D+00 /), (/ dim_num, node_num /) )
  integer ( kind = 4 ) order
  integer ( kind = 4 ) order_num
  integer ( kind = 4 ) rule
  real ( kind = 8 ), allocatable, dimension ( : ) :: w
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: xy
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: xy2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  REFERENCE_TO_PHYSICAL_T3 transforms a rule'
  write ( *, '(a)' ) '  on the unit (reference) triangle to a rule on '
  write ( *, '(a)' ) '  an arbitrary (physical) triangle.'

  rule = 3

  call triangle_nco_order_num ( rule, order_num )

  allocate ( xy(1:2,1:order_num) )
  allocate ( xy2(1:2,1:order_num) )
  allocate ( w(1:order_num) )

  call triangle_nco_rule ( rule, order_num, xy, w )
!
!  Here is the reference triangle, and its rule.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The reference triangle:'
  write ( *, '(a)' ) ' '

  do node = 1, node_num
    write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) node, node_xy(1:2,node)
  end do

  call triangle_area ( node_xy, area )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8,a)' ) '  Rule ', rule, ' for reference triangle'
  write ( *, '(a,g14.6)' ) '  with area = ', area
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                X               Y               W'
  write ( *, '(a)' ) ' '

  do order = 1, order_num
    write ( *, '(2x,i8,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      order, xy(1:2,order), w(order)
  end do
!
!  Transform the rule.
!
  call reference_to_physical_t3 ( node_xy2, order_num, xy, xy2 )
!
!  Here is the physical triangle, and its transformed rule.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The physical triangle:'
  write ( *, '(a)' ) ' '

  do node = 1, node_num
    write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) node, node_xy2(1:2,node)
  end do

  call triangle_area ( node_xy2, area2 )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8,a)' ) '  Rule ', rule, ' for physical triangle'
  write ( *, '(a,g14.6)' ) '  with area = ', area2
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '                X               Y               W'
  write ( *, '(a)' ) ' '

  do order = 1, order_num
    write ( *, '(2x,i8,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      order, xy2(1:2,order), w(order)
  end do

  deallocate ( w )
  deallocate ( xy )
  deallocate ( xy2 )

  return
end
