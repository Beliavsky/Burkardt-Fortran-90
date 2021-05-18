program main

!*****************************************************************************80
!
!! MAIN is the main program for SQUARE_MINIMAL_RULE_TEST.
!
!  Discussion:
!
!    SQUARE_MINIMAL_RULE_TEST tests the SQUARE_MINIMAL_RULE library.
!    
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) degree

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SQUARE_MINIMAL_RULE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SQUARE_MINIMAL_RULE library.'

  degree = 8
  call square_minimal_rule_print_test ( degree )

  call square_minimal_rule_order_test ( )

  call square_minimal_rule_error_max_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SQUARE_MINIMAL_RULE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine square_minimal_rule_print_test ( degree )

!*****************************************************************************80
!
!! SQUARE_MINIMAL_RULE_PRINT_TEST tests SQUARE_MINIMAL_RULE_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) area
  real ( kind = 8 ) d
  integer ( kind = 4 ) degree
  integer ( kind = 4 ) j
  integer ( kind = 4 ) order
  real ( kind = 8 ), allocatable :: xyw(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SQUARE_MINIMAL_RULE_PRINT_TEST'
  write ( *, '(a)' ) '  SQUARE_MINIMAL_RULE_PRINT prints a quadrature rule'
  write ( *, '(a)' ) '  for the symmetric unit square.'
  write ( *, '(a)' ) '  Minimal quadrature rule for a square.'
  write ( *, '(a,i4)' ) '  Polynomial exactness degree DEGREE = ', degree
!
!  Retrieve and print a symmetric quadrature rule.
!
  call square_minimal_rule_order ( degree, order )

  allocate ( xyw(1:3*order) )

  call square_minimal_rule ( degree, xyw )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of nodes N = ', order

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     J          X               Y               W'
  write ( *, '(a)' ) ''
  do j = 1, order
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      j, xyw(1+(j-1)*3), xyw(2+(j-1)*3), xyw(3+(j-1)*3)
  end do

  d = 0.0D+00
  do j = 1, order
    d = d + xyw(3+(j-1)*3)
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '   Sum  ', d
  call squaresym_area ( area )
  write ( *, '(a,g14.6)' ) '  Area  ', area

  deallocate ( xyw )

  return
end
subroutine square_minimal_rule_order_test ( )

!*****************************************************************************80
!
!! SQUARE_MINIMAL_RULE_ORDER_TEST tests SQUARE_MINIMAL_RULE_ORDER.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    23 February 2018
!
!  Author:
!
!    John Burkardt.
!
!  Reference:
!
!    Mattia Festa, Alvise Sommariva,
!    Computing almost minimal formulas on the square,
!    Journal of Computational and Applied Mathematics,
!    Volume 17, Number 236, November 2012, pages 4296-4302.
!
  implicit none

  integer ( kind = 4 ) degree
  integer ( kind = 4 ) degree_max
  integer ( kind = 4 ) order

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SQUARE_MINIMAL_RULE_ORDER_TEST'
  write ( *, '(a)' ) '  Print the order (number of points) for each'
  write ( *, '(a)' ) '  minimal square rule.'

  call square_minimal_rule_degree_max ( degree_max )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) ' Degree  Order'
  write ( *, '(a)' ) ''

  do degree = 0, degree_max
    call square_minimal_rule_order ( degree, order )
    write ( *, '(2x,i4,2x,i4)' ) degree, order
  end do

  return
end
subroutine square_minimal_rule_error_max_test ( )

!*****************************************************************************80
!
!! SQUARE_MINIMAL_RULE_ERROR_MAX_TEST tests SQUARE_MINIMAL_RULE_ERROR_MAX.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    23 February 2018
!
!  Author:
!
!    John Burkardt.
!
!  Reference:
!
!    Mattia Festa, Alvise Sommariva,
!    Computing almost minimal formulas on the square,
!    Journal of Computational and Applied Mathematics,
!    Volume 17, Number 236, November 2012, pages 4296-4302.
!
  implicit none

  integer ( kind = 4 ) degree
  integer ( kind = 4 ) degree_max
  real ( kind = 8 ) error_max
  integer ( kind = 4 ) m_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SQUARE_MINIMAL_RULE_ERROR_MAX_TEST'
  write ( *, '(a)' ) '  SQUARE_MINIMAL_RULE_ERROR_MAX computes the maximum'
  write ( *, '(a)' ) '  error for a rule that should be exact for all monomials'
  write ( *, '(a)' ) '  up to a given value of DEGREE.'

  call square_minimal_rule_degree_max ( degree_max )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) ' Degree  Monomials  Error Max'
  write ( *, '(a)' ) ''

  do degree = 0, degree_max
    call square_minimal_rule_error_max ( degree, error_max )
    m_num = ( ( degree + 1 ) * ( degree + 2 ) ) / 2
    write ( *, '(a,i4,a,i4,a,g14.6)' ) &
      '   ', degree, '       ', m_num, '  ', error_max
  end do

  return
end
