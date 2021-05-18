program main

!*****************************************************************************80
!
!! POLYNOMIAL_TEST tests the POLYNOMIAL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the POLYNOMIAL library.'

  call polynomial_add_test ( )
  call polynomial_axpy_test ( )
  call polynomial_compress_test ( )
  call polynomial_dif_test ( )
  call polynomial_mul_test ( )
  call polynomial_print_test ( )
  call polynomial_scale_test ( )
  call polynomial_sort_test ( )
  call polynomial_value_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine polynomial_add_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_ADD_TEST tests POLYNOMIAL_ADD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: o1 = 6
  integer ( kind = 4 ), parameter :: o2 = 5
  integer ( kind = 4 ), parameter :: o_max = o1 + o2

  real ( kind = 8 ) c(o_max)
  real ( kind = 8 ) c1(o1)
  real ( kind = 8 ) c2(o2)
  integer ( kind = 4 ) e(o_max)
  integer ( kind = 4 ) e1(o1)
  integer ( kind = 4 ) e2(o2)
  integer ( kind = 4 ) o
  character ( len = 80 ) title
  character ( len = 80 ) title1
  character ( len = 80 ) title2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_ADD_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_ADD adds two polynomials.'

  c1 = (/ 7.0, - 5.0, 9.0, 11.0, 0.0, - 13.0 /)
  e1 = (/ 1, 2, 4, 5, 12, 33 /)
  title1 = '  P1(X) ='
  write ( *, '(a)' ) ''
  call polynomial_print ( m, o1, c1, e1, title1 )

  c2 = (/ 2.0, 3.0, -8.0, 4.0, 9.0 /)
  e2 = (/ 1, 3, 4, 30, 33 /)
  title2 = '  P2(X) ='
  write ( *, '(a)' ) ''
  call polynomial_print ( m, o2, c2, e2, title2 )

  call polynomial_add ( o1, c1, e1, o2, c2, e2, o, c, e )
  title = '  P1(X) + P2(X) ='
  write ( *, '(a)' ) ''
  call polynomial_print ( m, o, c, e, title )

  return
end
subroutine polynomial_axpy_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_AXPY_TEST tests POLYNOMIAL_AXPY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: o1 = 6
  integer ( kind = 4 ), parameter :: o2 = 5
  integer ( kind = 4 ), parameter :: o_max = o1 + o2

  real ( kind = 8 ) c(o_max)
  real ( kind = 8 ) c1(o1)
  real ( kind = 8 ) c2(o2)
  integer ( kind = 4 ) e(o_max)
  integer ( kind = 4 ) e1(o1)
  integer ( kind = 4 ) e2(o2)
  integer ( kind = 4 ) o
  real ( kind = 8 ) s
  character ( len = 80 ) title
  character ( len = 80 ) title1
  character ( len = 80 ) title2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_AXPY_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_AXPY adds a multiple of one polynomial'
  write ( *, '(a)' ) '  to another.'

  c1 = (/ 7.0, - 5.0, 9.0, 11.0, 0.0, - 13.0 /)
  e1 = (/ 1, 2, 4, 5, 12, 33 /)
  title1 = '  P1(X) ='
  write ( *, '(a)' ) ''
  call polynomial_print ( m, o1, c1, e1, title1 )

  c2 = (/ 2.0, 3.0, -8.0, 4.0, 9.0 /)
  e2 = (/ 1, 3, 4, 30, 33 /)
  title2 = '  P2(X) ='
  write ( *, '(a)' ) ''
  call polynomial_print ( m, o2, c2, e2, title2 )

  s = 10.0D+00
  call polynomial_axpy ( s, o1, c1, e1, o2, c2, e2, o, c, e )
  write ( title, '(a,g14.6,a)' ) '  ', s, ' * P1(X) + P2(X) = '
  write ( *, '(a)' ) ''
  call polynomial_print ( m, o, c, e, title )

  return
end
subroutine polynomial_compress_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_COMPRESS_TEST tests POLYNOMIAL_COMPRESS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer, parameter :: m = 3
  integer, parameter :: o = 10

  real ( kind = 8 ), save, dimension ( o ) :: c = (/ &
    7.0, - 5.0, 5.0, 9.0, 11.0, 3.0, 6.0, 0.0, - 13.0, 1.0E-20 /)
  real ( kind = 8 ) c2(o)
  integer ( kind = 4 ), save, dimension ( o ) :: e = (/ &
    1, 2, 2, 4, 5, 5, 5, 12, 33, 35 /)
  integer ( kind = 4 ) e2(o)
  integer ( kind = 4 ) o2
  character ( len = 255 ) title

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_COMPRESS_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_COMPRESS compresses a polynomial.'

  write ( *, '(a)' ) ''
  title = '  Uncompressed P(X) ='
  call polynomial_print ( m, o, c, e, title )

  call polynomial_compress ( o, c, e, o2, c2, e2 )

  write ( *, '(a)' ) ''
  title = '  Compressed P(X) ='
  call polynomial_print ( m, o2, c2, e2, title )

  return
end
subroutine polynomial_dif_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_DIF_TEST tests POLYNOMIAL_DIF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ), parameter :: o1 = 4
  integer ( kind = 4 ), parameter :: o_max = o1

  real ( kind = 8 ) c(o_max)
  real ( kind = 8 ) c1(o1)
  integer ( kind = 4 ) dif(m)
  integer ( kind = 4 ) e(o_max)
  integer ( kind = 4 ) e1(o1)
  integer ( kind = 4 ) o
  character ( len = 80 ) title
  character ( len = 80 ) title1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_DIF_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_DIF computes derivatives of a polynomial.'

  c1 = (/ 2.0, 3.0, 4.0, 5.0 /)
  e1 = (/ 1, 10, 12, 32 /)
  title1 = '  P(X) ='
  write ( *, '(a)' ) ''
  call polynomial_print ( m, o1, c1, e1, title1 )

  dif = (/ 2, 1 /)

  call polynomial_dif ( m, o1, c1, e1, dif, o, c, e )
  title = '  d3 P(X) dx1 dx1 dx2 ='
  write ( *, '(a)' ) ''
  call polynomial_print ( m, o, c, e, title )

  return
end
subroutine polynomial_mul_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_MUL_TEST tests POLYNOMIAL_MUL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: o1 = 4
  integer ( kind = 4 ), parameter :: o2 = 2
  integer ( kind = 4 ), parameter :: o_max = o1 * o2

  real ( kind = 8 ) c(o_max)
  real ( kind = 8 ) c1(o1)
  real ( kind = 8 ) c2(o2)
  integer ( kind = 4 ) e(o_max)
  integer ( kind = 4 ) e1(o1)
  integer ( kind = 4 ) e2(o2)
  integer ( kind = 4 ) o
  character ( len = 80 ) title
  character ( len = 80 ) title1
  character ( len = 80 ) title2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_MUL_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_MUL multiplies two polynomials.'

  c1 = (/ 2.0, 3.0, 4.0, 5.0 /)
  e1 = (/ 1, 3, 4, 6 /)
  title1 = '  P1(X) ='
  write ( *, '(a)' ) ''
  call polynomial_print ( m, o1, c1, e1, title1 )

  c2 = (/ 6.0, 7.0 /)
  e2 = (/ 2, 5 /)
  title2 = '  P2(X) ='
  write ( *, '(a)' ) ''
  call polynomial_print ( m, o2, c2, e2, title2 )

  call polynomial_mul ( m, o1, c1, e1, o2, c2, e2, o, c, e )
  title = '  P1(X) * P2(X) ='
  write ( *, '(a)' ) ''
  call polynomial_print ( m, o, c, e, title )

  return
end
subroutine polynomial_print_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_PRINT_TEST tests POLYNOMIAL_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: o = 6

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) e(o)
  character ( len = 80 ) title

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_PRINT_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_PRINT prints a polynomial.'

  c = (/ 7.0, - 5.0, 9.0, 11.0, 0.0, - 13.0 /)
  e = (/ 1, 2, 4, 5, 12, 33 /)
  title = '  P1(X) ='

  write ( *, '(a)' ) ''
  call polynomial_print ( m, o, c, e, title )

  return
end
subroutine polynomial_scale_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_SCALE_TEST tests POLYNOMIAL_SCALE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: o = 6

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) e(o)
  real ( kind = 8 ) s
  character ( len = 80 ) title

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_SCALE_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_SCALE scales a polynomial by a multiplier S.'

  c = (/ 7.0, - 5.0, 9.0, 11.0, 0.0, - 13.0 /)
  e = (/ 1, 2, 4, 5, 12, 33 /)

  write ( *, '(a)' ) ''
  title = '  P(X) = '
  call polynomial_print ( m, o, c, e, title )

  s = - 0.5D+00
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Apply scale factor S = ', s
  call polynomial_scale ( s, m, o, c, e )

  write ( *, '(a)' ) ''
  title = '  S * P(X) = '
  call polynomial_print ( m, o, c, e, title )

  return
end
subroutine polynomial_sort_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_SORT_TEST tests POLYNOMIAL_SORT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: o = 6

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) e(o)
  character ( len = 80 ) title

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_SORT_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_SORT sorts a polynomial by exponent index..'

  c = (/ 0.0, 9.0, -5.0, - 13.0, 7.0, 11.0 /)
  e = (/ 12, 4, 2, 33, 1, 5 /)

  write ( *, '(a)' ) ''
  title = '  Unsorted polynomial:'
  call polynomial_print ( m, o, c, e, title )

  call polynomial_sort ( o, c, e )

  write ( *, '(a)' ) ''
  title = '  Sorted polynomial:'
  call polynomial_print ( m, o, c, e, title )

  return
end
subroutine polynomial_value_test ( )

!*****************************************************************************80
!
!! POLYNOMIAL_VALUE_TEST tests POLYNOMIAL_VALUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: nx = 2
  integer ( kind = 4 ), parameter :: o = 6

  real ( kind = 8 ) c(o)
  integer ( kind = 4 ) e(o)
  integer ( kind = 4 ) j
  real ( kind = 8 ) p(nx)
  character ( len = 80 ) title
  real ( kind = 8 ) x(m,nx)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'POLYNOMIAL_VALUE_TEST'
  write ( *, '(a)' ) '  POLYNOMIAL_VALUE evaluates a polynomial.'

  write ( *, '(a)' ) ''
  c = (/ 7.0, - 5.0, 9.0, 11.0, 0.0, - 13.0 /)
  e = (/ 1, 2, 4, 5, 12, 33 /)
  title = '  P(X) ='
  call polynomial_print ( m, o, c, e, title )

  x(1:m,1) = (/ 1.0, 2.0, 3.0 /)
  x(1:m,2) = (/ -2.0, 4.0, 1.0 /)
  call polynomial_value ( m, o, c, e, nx, x, p )
  write ( *, '(a)' ) ''
  do j = 1, nx
    write ( *, '(a,f10.4,a,f10.4,a,f10.4,a,g14.6)' ) &
      '  P(', x(1,j), ',', x(2,j), ',', x(3,j), ') = ', p(j)
  end do

  return
end

