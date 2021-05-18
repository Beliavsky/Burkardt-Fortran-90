program main

!*****************************************************************************80
!
!! MAIN is the main program for SHEPARD_INTERP_ND_TEST.
!
!  Discussion:
!
!    SHEPARD_INTERP_ND_TEST tests the SHEPARD_INTERP_ND library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: p_test_num = 4

  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n1d
  integer ( kind = 4 ) nd
  real ( kind = 8 ) p
  real ( kind = 8 ), dimension ( p_test_num ) :: p_test = (/ &
    1.0D+00, 2.0D+00, 4.0D+00, 8.0D+00 /)
  integer ( kind = 4 ) prob
  integer ( kind = 4 ) prob_num

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SHEPARD_INTERP_ND_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SHEPARD_INTERP_ND library.'
  write ( *, '(a)' ) '  The R8LIB library is needed.'
  write ( *, '(a)' ) '  This test also needs the TEST_INTERP_ND library.'
!
!  Look at Shepard interpolant on an irregular grid.
!
  nd = 25

  call p00_prob_num ( prob_num )

  do prob = 1, prob_num

    do m = 2, 5, 3

      do j = 1, p_test_num
        p = p_test(j)
        call test01 ( prob, p, m, nd )
      end do

    end do
  end do
!
!  Look at Shepard interpolant on a regular N1D^M grid.
!
  n1d = 5

  call p00_prob_num ( prob_num )

  do prob = 1, prob_num

    do m = 2, 5, 3

      do j = 1, p_test_num
        p = p_test(j)
        call test02 ( prob, p, m, n1d )
      end do

    end do
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SHEPARD_INTERP_ND_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( prob, p, m, nd )

!*****************************************************************************80
!
!! TEST01 tests SHEPARD_INTERP on an irregular grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) PROB, the problem number.
!
!    Input, real ( kind = 8 ) P, the power used in the distance weighting.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) ND, the number of data points.
!
  implicit none

  real ( kind = 8 ) app_error
  real ( kind = 8 ), allocatable :: c(:)
  real ( kind = 8 ) int_error
  integer ( kind = 4 ) m
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) ni
  real ( kind = 8 ) p
  integer ( kind = 4 ) prob
  real ( kind = 8 ) r8vec_norm_affine
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: xd(:,:)
  real ( kind = 8 ), allocatable :: xi(:,:)
  real ( kind = 8 ), allocatable :: zd(:)
  real ( kind = 8 ), allocatable :: ze(:)
  real ( kind = 8 ), allocatable :: zi(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a,i4)' ) '  Interpolate data from TEST_INTERP_ND problem #', prob
  write ( *, '(a,g14.6)' ) '  using Shepard interpolation with P = ', p
  write ( *, '(a,i4)' ) '  spatial dimension M = ', m
  write ( *, '(a,i4,a)' ) '  and an irregular grid of ND = ', nd, ' data points.'
!
!  Set problem parameters:
!
  seed = 123456789
  allocate ( c(1:m) )
  call r8vec_uniform_01 ( m, seed, c )
  allocate ( w(1:m) )
  call r8vec_uniform_01 ( m, seed, w )

  allocate ( xd(1:m,1:nd) )
  call r8mat_uniform_01 ( m, nd, seed, xd )

  allocate ( zd(1:nd) )
  call p00_f ( prob, m, c, w, nd, xd, zd )
!
!  #1:  Does interpolant match function at interpolation points?
!
  ni = nd
  allocate ( xi(1:m,1:ni) )
  xi(1:m,1:ni) = xd(1:m,1:ni)
  allocate ( zi(1:ni) )
  call shepard_interp_nd ( m, nd, xd, zd, p, ni, xi, zi )

  int_error = r8vec_norm_affine ( ni, zi, zd ) / real ( ni, kind = 8 )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) &
    '  L2 interpolation error averaged per interpolant node = ', int_error

  deallocate ( xi )
  deallocate ( zi )
!
!  #2: Approximation test.  Estimate the integral (f-interp(f))^2.
!
  ni = 1000
  ni = 50
  allocate ( xi(1:m,1:ni) )
  call r8mat_uniform_01 ( m, ni, seed, xi )

  allocate ( zi(1:ni) )
  call shepard_interp_nd ( m, nd, xd, zd, p, ni, xi, zi )

  allocate ( ze(1:ni) )
  call p00_f ( prob, m, c, w, ni, xi, ze )

  app_error = r8vec_norm_affine ( ni, zi, ze ) / real ( ni, kind = 8 )

  write ( *, '(a,g14.6)' ) &
    '  L2 approximation error averaged per 1000 samples =     ', app_error

  deallocate ( c )
  deallocate ( w )
  deallocate ( xd )
  deallocate ( xi )
  deallocate ( zd )
  deallocate ( ze )
  deallocate ( zi )

  return
end
subroutine test02 ( prob, p, m, n1d )

!*****************************************************************************80
!
!! TEST02 tests SHEPARD_INTERP_ND on a regular N1D^M grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) PROB, the problem number.
!
!    Input, real ( kind = 8 ) P, the power used in the distance weighting.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N1D, the number of points in 1D.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) app_error
  real ( kind = 8 ) b
  real ( kind = 8 ), allocatable :: c(:)
  integer ( kind = 4 ) i
  real ( kind = 8 ) int_error
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n1d
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) ni
  real ( kind = 8 ) p
  integer ( kind = 4 ) prob
  real ( kind = 8 ) r8vec_norm_affine
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x1d(:)
  real ( kind = 8 ), allocatable :: xd(:,:)
  real ( kind = 8 ), allocatable :: xi(:,:)
  real ( kind = 8 ), allocatable :: zd(:)
  real ( kind = 8 ), allocatable :: ze(:)
  real ( kind = 8 ), allocatable :: zi(:)
!
!  Set problem parameters:
!
  seed = 123456789
  allocate ( c(1:m) )
  call r8vec_uniform_01 ( m, seed, c )
  allocate ( w(1:m) )
  call r8vec_uniform_01 ( m, seed, w )

  nd = n1d ** m

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a,i4)' ) '  Interpolate data from TEST_INTERP_ND problem #', prob
  write ( *, '(a,g14.6)' ) '  using Shepard interpolation with P = ', p
  write ( *, '(a,i4)' ) '  spatial dimension M = ', m
  write ( *, '(a,i6,a)' ) '  and a regular grid of N1D^M = ', nd, ' data points.'

  a = 0.0D+00
  b = 1.0D+00
  allocate ( x1d(1:n1d) )

  call r8vec_linspace ( n1d, a, b, x1d )

  allocate ( xd(1:m,1:nd) )
  do i = 1, m
    call r8vec_direct_product ( i, n1d, x1d, m, nd, xd )
  end do

  allocate ( zd(1:nd) )
  call p00_f ( prob, m, c, w, nd, xd, zd )
!
!  #1:  Does interpolant match function at interpolation points?
!
  ni = nd
  allocate ( xi(1:m,1:nd) )
  xi(1:m,1:nd) = xd(1:m,1:nd)
  allocate ( zi(1:ni) )
  call shepard_interp_nd ( m, nd, xd, zd, p, ni, xi, zi )

  int_error = r8vec_norm_affine ( ni, zi, zd ) / real ( ni, kind = 8 )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) &
    '  L2 interpolation error averaged per interpolant node = ', int_error

  deallocate ( xi )
  deallocate ( zi )
!
!  #2: Approximation test.  Estimate the integral (f-interp(f))^2.
!
  ni = 1000
  allocate ( xi(1:m,1:ni) )
  call r8mat_uniform_01 ( m, ni, seed, xi )

  allocate ( zi(1:ni) )
  call shepard_interp_nd ( m, nd, xd, zd, p, ni, xi, zi )

  allocate ( ze(1:ni) )
  call p00_f ( prob, m, c, w, ni, xi, ze )

  app_error = r8vec_norm_affine ( ni, zi, ze ) / real ( ni, kind = 8 )

  write ( *, '(a,g14.6)' ) &
    '  L2 approximation error averaged per 1000 samples =     ', app_error

  deallocate ( c )
  deallocate ( w )
  deallocate ( xd )
  deallocate ( xi )
  deallocate ( zd )
  deallocate ( ze )
  deallocate ( zi )

  return
end
