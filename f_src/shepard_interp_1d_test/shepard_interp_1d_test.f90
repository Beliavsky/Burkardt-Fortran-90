program main

!*****************************************************************************80
!
!! MAIN is the main program for SHEPARD_INTERP_1D_TEST.
!
!  Discussion:
!
!    SHEPARD_INTERP_1D_TEST tests the SHEPARD_INTERP_1D library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: p_num = 5

  integer ( kind = 4 ) j
  real ( kind = 8 ) p
  real ( kind = 8 ), dimension ( p_num ) :: p_test = (/ &
    0.0D+00, 1.0D+00, 2.0D+00, 4.0D+00, 8.0D+00 /)
  integer ( kind = 4 ) prob
  integer ( kind = 4 ) prob_num

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SHEPARD_INTERP_1D_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SHEPARD_INTERP_1D library.'
  write ( *, '(a)' ) '  The R8LIB library is needed.'
  write ( *, '(a)' ) '  This test needs the TEST_INTERP library as well.'

  call shepard_basis_1d_test ( )

  call shepard_value_1d_test ( )

  call p00_prob_num ( prob_num )

  do prob = 1, prob_num

    do j = 1, p_num
      p = p_test(j)
      call test01 ( prob, p )
    end do

  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SHEPARD_INTERP_1D_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine shepard_basis_1d_test ( )

!*****************************************************************************80
!
!! SHEPARD_BASIS_1D_TEST tests SHEPARD_BASIS_1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nd = 4
  integer ( kind = 4 ), parameter :: ni = 21

  real ( kind = 8 ) lb(ni,nd)
  real ( kind = 8 ) p
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ), dimension(nd) :: xd = (/ &
    0.0D+00, 2.0D+00, 5.0D+00, 10.0D+00 /)
  real ( kind = 8 ) xi(ni)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SHEPARD_BASIS_1D_TEST:'
  write ( *, '(a)' ) '  SHEPARD_BASIS_1D evaluates the Shepard 1D basis'
  write ( *, '(a)' ) '  functions.'

  p = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Using power P = ', p

  x_min = 0.0D+00
  x_max = 10.0D+00
  call r8vec_linspace ( ni, x_min, x_max, xi )

  call shepard_basis_1d ( nd, xd, p, ni, xi, lb )

  call r8mat_print ( ni, nd, lb, '  The Shepard basis functions:' )

  return
end
subroutine shepard_value_1d_test ( )

!*****************************************************************************80
!
!! SHEPARD_VALUE_1D_TEST tests SHEPARD_VALUE_1D.
!
!  Discussion:
!
!    f(x) = x^3 - 12 x^2 + 39 x - 28 = ( x - 1 ) * ( x - 4 ) * ( x - 7 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified: 
!
!    03 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nd = 4
  integer ( kind = 4 ), parameter :: ni = 21

  real ( kind = 8 ) p
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ), dimension(nd) :: xd = (/ &
    0.0D+00, 2.0D+00, 5.0D+00, 10.0D+00 /)
  real ( kind = 8 ), dimension(nd) :: yd = (/ &
    -28.0D+00, +10.0D+00, -8.0D+00, +162.0D+00 /)
  real ( kind = 8 ) xi(ni)
  real ( kind = 8 ) yi(ni)
 
  p = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SHEPARD_VALUE_1D_TEST:'
  write ( *, '(a)' ) '  SHEPARD_VALUE_1D evaluates a Shepard 1D interpolant.'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Using power P = ', p

  x_min = 0.0D+00
  x_max = 10.0D+00
  call r8vec_linspace ( ni, x_min, x_max, xi )

  call shepard_value_1d ( nd, xd, yd, p, ni, xi, yi )

  call r8vec2_print ( ni, xi, yi, '  Table of interpolant values:' )

  return
end
subroutine test01 ( prob, p )

!*****************************************************************************80
!
!! TEST01 tests SHEPARD_VALUE_1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) dim_num
  real ( kind = 8 ) int_error
  real ( kind = 8 ) ld
  real ( kind = 8 ) li
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) ni
  real ( kind = 8 ) p
  integer ( kind = 4 ) prob
  real ( kind = 8 ) r8vec_norm_affine
  real ( kind = 8 ), allocatable :: xd(:)
  real ( kind = 8 ), allocatable :: xi(:)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ), allocatable :: xy(:,:)
  real ( kind = 8 ), allocatable :: yd(:)
  real ( kind = 8 ), allocatable :: yi(:)
  real ( kind = 8 ) ymax
  real ( kind = 8 ) ymin

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a,i4)' ) '  Interpolate data from TEST_INTERP problem #', prob
  write ( *, '(a,g14.6)' ) '  using Shepard interpolation with P = ', p

  call p00_dim_num ( prob, dim_num )

  call p00_data_num ( prob, nd )
  write ( *, '(a,i6)' ) '  Number of data points = ', nd

  allocate ( xy(2,1:nd) )
  call p00_data ( prob, dim_num, nd, xy )
  
  if ( p == 0.0D+00 ) then
    call r8mat_transpose_print ( 2, nd, xy, '  Data array:' )
  end if

  allocate ( xd(1:nd) )
  allocate ( yd(1:nd) )

  xd(1:nd) = xy(1,1:nd)
  yd(1:nd) = xy(2,1:nd)
!
!  #1:  Does interpolant match function at interpolation points?
!
  ni = nd
  allocate ( xi(1:ni) )
  allocate ( yi(1:ni) )
  xi(1:ni) = xd(1:ni)

  call shepard_value_1d ( nd, xd, yd, p, ni, xi, yi )

  int_error = r8vec_norm_affine ( nd, yi, yd ) / real ( ni, kind = 8 )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) &
    '  L2 interpolation error averaged per interpolant node = ', int_error

  deallocate ( xi )
  deallocate ( yi )
!
!  #2: Compare estimated curve length to piecewise linear (minimal) curve length.
!  Assume data is sorted, and normalize X and Y dimensions by (XMAX-XMIN) and
!  (YMAX-YMIN).
!
  xmin = minval ( xd(1:nd) )
  xmax = maxval ( xd(1:nd) )
  ymin = minval ( yd(1:nd) )
  ymax = maxval ( yd(1:nd) )

  ni = 501
  allocate ( xi(1:ni) )
  allocate ( yi(1:ni) )
  call r8vec_linspace ( ni, xmin, xmax, xi )
  call shepard_value_1d ( nd, xd, yd, p, ni, xi, yi )

  ld = sum ( sqrt ( ( ( xd(2:nd) - xd(1:nd-1) ) / ( xmax - xmin ) ) ** 2 &
                  + ( ( yd(2:nd) - yd(1:nd-1) ) / ( ymax - ymin ) ) ** 2 ) )

  li = sum ( sqrt ( ( ( xi(2:ni) - xi(1:ni-1) ) / ( xmax - xmin ) ) ** 2 &
                  + ( ( yi(2:ni) - yi(1:ni-1) ) / ( ymax - ymin ) ) ** 2 ) )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) &
    '  Normalized length of piecewise linear interpolant = ', ld
  write ( *, '(a,g14.6)' ) &
    '  Normalized length of Shepard interpolant          = ', li

  deallocate ( xd )
  deallocate ( xi )
  deallocate ( xy )
  deallocate ( yd )
  deallocate ( yi )

  return
end
