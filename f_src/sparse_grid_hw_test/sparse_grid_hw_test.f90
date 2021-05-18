program main

!*****************************************************************************80
!
!! sparse_grid_hw_test tests sparse_grid_hw().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 February 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'sparse_grid_hw_test'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test sparse_grid_hw().'

  call ccl_test ( )
  call ccl_sparse_test ( )

  call ccs_test ( )
  call ccs_sparse_test ( )

  call cce_test ( )
  call cce_sparse_test ( )

  call get_seq_test ( )

  call gqn_test ( )
  call gqn_sparse_test ( )
  call gqn2_sparse_test ( )

  call gqu_test ( )
  call gqu_sparse_test ( )

  call kpn_test ( )
  call kpn_sparse_test ( )

  call kpu_test ( )
  call kpu_sparse_test ( )

  call nwspgr_size_test ( )
  call nwspgr_time_test ( )
  call nwspgr_test ( )

  call order_report ( )

  call symmetric_sparse_size_test ( )

  call tensor_product_test ( )
  call tensor_product_cell_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'sparse_grid_hw_test'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine ccl_test ( )

!*****************************************************************************80
!
!! CCL_TEST uses CCL_ORDER + CC for 1D quadrature over [0,1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) exact
  real ( kind = 8 ) fu_integral
  real ( kind = 8 ), allocatable :: fx(:)
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CCL_TEST:'
  write ( *, '(a)' ) '  CCL_ORDER + CC 1D quadrature:'
  write ( *, '(a)' ) '  Clenshaw Curtis Linear (CCL) quadrature over [0,1]:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
  write ( *, '(a)' ) ' '

  d = 1
  exact = fu_integral ( d );

  do l = 1, 5

    call ccl_order ( l, n )

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call cc ( n, x, w )

    allocate ( fx(1:n) )
    call fu_value ( d, n, x, fx )

    q = dot_product ( w(1:n), fx(1:n) )

    e = abs ( q - exact ) / exact

    write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

    deallocate ( fx )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine ccl_sparse_test ( )

!*****************************************************************************80
!
!! CCL_SPARSE_TEST uses CCL_ORDER + CC to build a sparse grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Local parameters:
!
!    Local, integer D, the spatial dimension.
!
!    Local, integer MAXK, the maximum level to check.
!
  implicit none

  external cc
  external ccl_order
  integer ( kind = 4 ) d
  real ( kind = 8 ) error_mc
  real ( kind = 8 ) error_sg
  real ( kind = 8 ) estimate
  real ( kind = 8 ) fu_integral
  real ( kind = 8 ), allocatable :: fx(:)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) maxk
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) r
  real ( kind = 8 ), allocatable :: s(:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) trueval
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  d = 10
  maxk = 7
  trueval = fu_integral ( d );

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CCL_SPARSE_TEST:'
  write ( *, '(a)' ) '  CCL_ORDER + CC sparse grid:'
  write ( *, '(a)' ) '  Sparse Clenshaw Curtis Linear quadrature over [0,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
  write ( *, '(a)' ) ''

! do k = 2, maxk
  do k = 8, 8
!
!  Compute sparse grid estimate.
!
    call nwspgr_size ( ccl_order, d, k, n )

    allocate ( x(1:d,1:n) )
    allocate ( w(1:n) )

    call nwspgr ( cc, ccl_order, d, k, n, n2, x, w )

    allocate ( fx(1:n2) )
    call fu_value ( d, n2, x, fx )
    estimate = dot_product ( w(1:n2), fx(1:n2) )

    error_sg = abs ( ( estimate - trueval ) / trueval )
!
!  Compute 1000 Monte Carlo estimates with same number of points, and average.
!
    allocate ( s(1000) )
    seed = 123456789

    do r = 1, 1000
      call r8mat_uniform_01 ( d, n2, seed, x )
      call fu_value ( d, n2, x, fx )
      s(r) = sum ( fx ) / real ( n2, kind = 8 )
    end do
    error_mc = sqrt ( sum ( ( s(1:1000) - trueval ) ** 2 ) / 1000.0D+00 ) &
      / trueval

    write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) &
      d, k, n2, error_sg, error_mc

    deallocate ( fx )
    deallocate ( s )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine ccs_test ( )

!*****************************************************************************80
!
!! CCS_TEST uses CCS_ORDER + CC for 1D quadrature over [0,1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) exact
  real ( kind = 8 ) fu_integral
  real ( kind = 8 ), allocatable :: fx(:)
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CCS_TEST:'
  write ( *, '(a)' ) '  CCS_ORDER + CC 1D quadrature:'
  write ( *, '(a)' ) '  Clenshaw Curtis Slow (CCS) quadrature over [0,1]:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
  write ( *, '(a)' ) ' '

  d = 1
  exact = fu_integral ( d );

  do l = 1, 5

    call ccs_order ( l, n )

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call cc ( n, x, w )

    allocate ( fx(1:n) )
    call fu_value ( d, n, x, fx )

    q = dot_product ( w(1:n), fx(1:n) )

    e = abs ( q - exact ) / exact

    write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

    deallocate ( fx )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine ccs_sparse_test ( )

!*****************************************************************************80
!
!! CCS_SPARSE_TEST uses CCS_ORDER + CC to build a sparse grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Local parameters:
!
!    Local, integer D, the spatial dimension.
!
!    Local, integer MAXK, the maximum level to check.
!
  implicit none

  external cc
  external ccs_order
  integer ( kind = 4 ) d
  real ( kind = 8 ) error_mc
  real ( kind = 8 ) error_sg
  real ( kind = 8 ) estimate
  real ( kind = 8 ) fu_integral
  real ( kind = 8 ), allocatable :: fx(:)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) maxk
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) r
  real ( kind = 8 ), allocatable :: s(:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) trueval
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  d = 10
  maxk = 7
  trueval = fu_integral ( d );

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CCS_SPARSE_TEST:'
  write ( *, '(a)' ) '  CCS_ORDER + CC sparse grid:'
  write ( *, '(a)' ) '  Sparse Clenshaw Curtis Slow quadrature over [0,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
  write ( *, '(a)' ) ''

  do k = 2, maxk
!
!  Compute sparse grid estimate.
!
    call nwspgr_size ( ccs_order, d, k, n )

    allocate ( x(1:d,1:n) )
    allocate ( w(1:n) )

    call nwspgr ( cc, ccs_order, d, k, n, n2, x, w )

    allocate ( fx(1:n2) )
    call fu_value ( d, n2, x, fx )
    estimate = dot_product ( w(1:n2), fx(1:n2) )

    error_sg = abs ( ( estimate - trueval ) / trueval )
!
!  Compute 1000 Monte Carlo estimates with same number of points, and average.
!
    allocate ( s(1000) )
    seed = 123456789

    do r = 1, 1000
      call r8mat_uniform_01 ( d, n2, seed, x )
      call fu_value ( d, n2, x, fx )
      s(r) = sum ( fx ) / real ( n2, kind = 8 )
    end do
    error_mc = sqrt ( sum ( ( s(1:1000) - trueval ) ** 2 ) / 1000.0D+00 ) &
      / trueval

    write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) &
      d, k, n2, error_sg, error_mc

    deallocate ( fx )
    deallocate ( s )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine cce_test ( )

!*****************************************************************************80
!
!! CCE_TEST uses CCE_ORDER + CC for 1D quadrature over [0,1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 May 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) exact
  real ( kind = 8 ) fu_integral
  real ( kind = 8 ), allocatable :: fx(:)
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CCE_TEST:'
  write ( *, '(a)' ) '  CCE_ORDER + CC 1D quadrature:'
  write ( *, '(a)' ) '  Clenshaw Curtis Exponential (CCE) quadrature over [0,1]:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
  write ( *, '(a)' ) ' '

  d = 1
  exact = fu_integral ( d );

  do l = 1, 5

    call cce_order ( l, n )

    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call cc ( n, x, w )

    allocate ( fx(1:n) )
    call fu_value ( d, n, x, fx )

    q = dot_product ( w(1:n), fx(1:n) )

    e = abs ( q - exact ) / exact

    write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

    deallocate ( fx )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine cce_sparse_test ( )

!*****************************************************************************80
!
!! CCE_SPARSE_TEST uses CCE_ORDER + CC function to build a sparse grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Local parameters:
!
!    Local, integer D, the spatial dimension.
!
!    Local, integer MAXK, the maximum level to check.
!
  implicit none

  external cce_order
  external cc
  integer ( kind = 4 ) d
  real ( kind = 8 ) error_mc
  real ( kind = 8 ) error_sg
  real ( kind = 8 ) estimate
  real ( kind = 8 ) fu_integral
  real ( kind = 8 ), allocatable :: fx(:)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) maxk
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) r
  real ( kind = 8 ), allocatable :: s(:)
  integer ( kind = 4 ), parameter :: s_num = 1000
  integer ( kind = 4 ) seed
  real ( kind = 8 ) trueval
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  d = 10
  maxk = 7

  trueval = fu_integral ( d );

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CCE_SPARSE_TEST:'
  write ( *, '(a)' ) '  CCE_ORDER + CC sparse grid:'
  write ( *, '(a)' ) '  Sparse Gaussian unweighted quadrature over [0,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
  write ( *, '(a)' ) ''

  do k = 2, maxk
!
!  Compute sparse grid estimate.
!
    call nwspgr_size ( cce_order, d, k, n )

    allocate ( x(1:d,1:n) )
    allocate ( w(1:n) )

    call nwspgr ( cc, cce_order, d, k, n, n2, x, w )

    allocate ( fx(1:n2) )
    call fu_value ( d, n2, x, fx )
    estimate = dot_product ( w(1:n2), fx(1:n2) )

    error_sg = abs ( ( estimate - trueval ) / trueval )
!
!  Compute 1000 Monte Carlo estimates with same number of points, and average.
!
    allocate ( s(s_num) )
    seed = 123456789

    do r = 1, s_num
      call r8mat_uniform_01 ( d, n2, seed, x )
      call fu_value ( d, n2, x, fx )
      s(r) = sum ( fx ) / real ( n2, kind = 8 )
    end do
    error_mc = sqrt ( sum ( ( s(1:s_num) - trueval ) ** 2 ) &
      / real ( s_num, kind = 8 ) ) / trueval

    write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) &
      d, k, n2, error_sg, error_mc

    deallocate ( fx )
    deallocate ( s )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine get_seq_test ( )

!*****************************************************************************80
!
!! GET_SEQ_TEST tests GET_SEQ.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d

  integer ( kind = 4 ), allocatable :: fs(:,:)
  integer ( kind = 4 ) norm
  integer ( kind = 4 ) seq_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GET_SEQ_TEST'
  write ( *, '(a)' ) '  GET_SEQ returns D-dimensional vectors summing to NORM.'

  d = 3
  norm = 6

  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  D = ', d
  write ( *, '(a,i4)' ) '  NORM = ', norm

  call num_seq ( norm - d, d, seq_num )

  allocate ( fs(1:seq_num,1:d) )

  call get_seq ( d, norm, seq_num, fs )

  call i4mat_print ( seq_num, d, fs, '  The compositions' )

  deallocate ( fs )

  return
end
subroutine gqn_test ( )

!*****************************************************************************80
!
!! GQN_TEST uses the GQN function for 1D quadrature over (-oo,+oo).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) exact
  real ( kind = 8 ) fn_integral
  real ( kind = 8 ), allocatable :: fx(:)
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GQN_TEST:'
  write ( *, '(a)' ) '  Gauss-Hermite quadrature over (-oo,+oo):'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
  write ( *, '(a)' ) ' '

  d = 1
  exact = fn_integral ( d );

  do l = 1, 5

    n = l
    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call gqn ( n, x, w )

    allocate ( fx(1:n) )
    call fn_value ( d, n, x, fx )

    q = dot_product ( w(1:n), fx(1:n) )

    e = abs ( q - exact ) / exact

    write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

    deallocate ( fx )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine gqn_sparse_test ( )

!*****************************************************************************80
!
!! gqn_sparse_test uses the GQN function to build a sparse grid.
!
!  Discussion:
!
!    We are using a sparse grid based on Gauss-Hermite quadrature to estimate
!    an integral of with a Gaussian weight:
!
!      (2 pi)^(-D/2) integral ( -oo, +oo )^D F(X) exp(-(X1^2+X2^2+..+XD^2)/2) dX1 dX2 ... dXD
!
!    For this example, F(X) = X1^6.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2020
!
!  Author:
!
!    John Burkardt
!
!  Local:
!
!    integer D, the spatial dimension.
!
!    real ERROR_SG, the relative error in the integral estimate.
!
!    real ESTIMATE, the integral estimate.
!
!    real FX(N2), the integrand, evaluated at the sample points.
!
!    subroutine gqn(), the name of a subroutine which determines the points
!    and weights for a given order.
!
!    subroutine qgn_order(), the name of a subroutine which controls the growth of N
!    with the sparse grid level.
!
!    integer K, the current sparse grid level being used.
!
!    integer MAXK, the maximum level to check.
!
!    integer N, the number of sparse grid sample points and weights.
!
!    integer N2, the updated value of N, after duplicate points have been merged.
!
!    real TRUEVAL, the exact value of the integral we are estimating.
!
!    real W(N), the sparse grid weights.
!
!    real X(D,N), the sparse grid sample points.
!
  implicit none

  integer ( kind = 4 ) d
  real ( kind = 8 ) error_mc
  real ( kind = 8 ) error_sg
  real ( kind = 8 ) estimate
  real ( kind = 8 ) fn_integral
  real ( kind = 8 ), allocatable :: fx(:)
  external gqn
  external gqn_order
  integer ( kind = 4 ) k
  integer ( kind = 4 ) maxk
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) r
  real ( kind = 8 ), allocatable :: s(:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) trueval
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  d = 10
  maxk = 7

  trueval = fn_integral ( d );

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'gqn_sparse_test:'
  write ( *, '(a)' ) '  Sparse Gaussian quadrature with Hermite weight over (-oo,+oo).'
  write ( *, '(a)' ) '  Use GQN_ORDER: the growth rule N = L.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
  write ( *, '(a)' ) ''
!
!  Try levels 2 through 7.
!
  do k = 2, maxk
!
!  For the given growth order GQN_ORDER, dimension D, and level K,
!  get the number of sample points and weights.
!
    call nwspgr_size ( gqn_order, d, k, n )
    allocate ( x(1:d,1:n) )
    allocate ( w(1:n) )
!
!  Compute the sample points and weights.
!
    call nwspgr ( gqn, gqn_order, d, k, n, n2, x, w )
!
!  Evaluate the function at the sample points.
!
    allocate ( fx(1:n2) )
    call fn_value ( d, n2, x, fx )
!
!  Compute the estimated integral.
!
    estimate = dot_product ( w(1:n2), fx(1:n2) )
!
!  Compute the relative error in the sparse grid estimate.
!
    error_sg = abs ( ( estimate - trueval ) / trueval )
!
!  For comparison, compute 1000 Monte Carlo estimates using the same number of 
!  points.  Average these estimates and report the error.
!
    allocate ( s(1000) )
    seed = 123456789

    do r = 1, 1000
      call r8mat_normal_01 ( d, n2, seed, x )
      call fn_value ( d, n2, x, fx )
      s(r) = sum ( fx ) / real ( n2, kind = 8 )
    end do
    error_mc = sqrt ( sum ( ( s(1:1000) - trueval ) ** 2 ) / 1000.0D+00 ) / trueval
!
!  Report the parameters, and the two error estimates.
!
    write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) d, k, n2, error_sg, error_mc
!
!  Free memory.
!
    deallocate ( fx )
    deallocate ( s )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine gqn2_sparse_test ( )

!*****************************************************************************80
!
!! GQN2_SPARSE_TEST uses the GQN and GQN2_ORDER functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Local parameters:
!
!    Local, integer D, the spatial dimension.
!
!    Local, integer MAXK, the maximum level to check.
!
  implicit none

  integer ( kind = 4 ) d
  external gqn
  external gqn2_order
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) maxk
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2
  real ( kind = 8 ) q
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  d = 2
  maxk = 5

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GQN2_SPARSE_TEST:'
  write ( *, '(a)' ) '  GQN sparse grid:'
  write ( *, '(a)' ) '  Gauss-Hermite sparse grids over (-oo,+oo).'
  write ( *, '(a)' ) '  Use GQN2_ORDER, the growth rule N = 2 * L - 1.'
  write ( *, '(a)' ) '  Estimate weighted integral of 1/(1+x^2+y^2).'

  do k = 2, maxk

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '     J      W                X               Y'
    write ( *, '(a)' ) ''

    call nwspgr_size ( gqn2_order, d, k, n )

    allocate ( x(1:d,1:n) )
    allocate ( w(1:n) )
    call nwspgr ( gqn, gqn2_order, d, k, n, n2, x, w )

    do j = 1, n2
      write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) j, w(j), x(1,j), x(2,j)
    end do

    q = 0.0D+00
    do j = 1, n2
      q = q + w(j) * 1.0D+00 / ( 1.0D+00 + x(1,j)**2 + x(2,j)**2 )
    end do
    write (  *, '(a,g14.6)' ) '  Integral estimate = ', q

    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine gqu_test ( )

!*****************************************************************************80
!
!! GQU_TEST uses the GQU function for 1D quadrature over [0,1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) exact
  real ( kind = 8 ) fu_integral
  real ( kind = 8 ), allocatable :: fx(:)
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'GQU_TEST:'
  write ( *, '(a)' ) '  Gauss-Legendre quadrature over [0,1]:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
  write ( *, '(a)' ) ' '

  d = 1
  exact = fu_integral ( d );

  do l = 1, 5

    n = l
    allocate ( x(1:n) )
    allocate ( w(1:n) )

    call gqu ( n, x, w )

    allocate ( fx(1:n) )
    call fu_value ( d, n, x, fx )

    q = dot_product ( w(1:n), fx(1:n) )

    e = abs ( q - exact ) / exact

    write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

    deallocate ( fx )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine gqu_sparse_test ( )

!*****************************************************************************80
!
!! GQU_SPARSE_TEST uses the GQU function to build a sparse grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Local parameters:
!
!    Local, integer D, the spatial dimension.
!
!    Local, integer MAXK, the maximum level to check.
!
  implicit none

  integer ( kind = 4 ) d
  real ( kind = 8 ) error_mc
  real ( kind = 8 ) error_sg
  real ( kind = 8 ) estimate
  real ( kind = 8 ) fu_integral
  real ( kind = 8 ), allocatable :: fx(:)
  external gqu
  external gqu_order
  integer ( kind = 4 ) k
  integer ( kind = 4 ) maxk
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) r
  real ( kind = 8 ), allocatable :: s(:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) trueval
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  d = 10
  maxk = 7

  trueval = fu_integral ( d );

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'GQU_SPARSE_TEST:'
  write ( *, '(a)' ) '  GQU sparse grid:'
  write ( *, '(a)' ) '  Sparse Gauss-Legendre quadrature with unit weight over [0,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
  write ( *, '(a)' ) ''

  do k = 2, maxk
!
!  Compute sparse grid estimate.
!
    call nwspgr_size ( gqu_order, d, k, n )
    allocate ( x(1:d,1:n) )
    allocate ( w(1:n) )
    call nwspgr ( gqu, gqu_order, d, k, n, n2, x, w )
    allocate ( fx(1:n2) )
    call fu_value ( d, n2, x, fx )
    estimate = dot_product ( w(1:n2), fx(1:n2) )

    error_sg = abs ( ( estimate - trueval ) / trueval )
!
!  Compute 1000 Monte Carlo estimates with same number of points, and average.
!
    allocate ( s(1000) )
    seed = 123456789

    do r = 1, 1000
      call r8mat_uniform_01 ( d, n2, seed, x )
      call fu_value ( d, n2, x, fx )
      s(r) = sum ( fx ) / real ( n2, kind = 8 )
    end do
    error_mc = sqrt ( sum ( ( s(1:1000) - trueval ) ** 2 ) / 1000.0D+00 ) / trueval

    write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) d, k, n2, error_sg, error_mc

    deallocate ( fx )
    deallocate ( s )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine kpn_test ( )

!*****************************************************************************80
!
!! KPN_TEST uses the KPN function for 1D quadrature over (-oo,+oo).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) exact
  real ( kind = 8 ) fn_integral
  real ( kind = 8 ), allocatable :: fx(:)
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'KPN_TEST:'
  write ( *, '(a)' ) '  Kronrod-Patterson-Hermite quadrature over (-oo,+oo):'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
  write ( *, '(a)' ) ' '

  d = 1
  exact = fn_integral ( d );

  do l = 1, 5

    call kpn_order ( l, n )

    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call kpn ( n, x, w )

    allocate ( fx(1:n) )
    call fn_value ( d, n, x, fx )

    q = dot_product ( w(1:n), fx(1:n) )

    e = abs ( q - exact ) / exact

    write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

    deallocate ( fx )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine kpn_sparse_test ( )

!*****************************************************************************80
!
!! KPN_SPARSE_TEST uses the KPN function to build a sparse grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Local parameters:
!
!    Local, integer D, the spatial dimension.
!
!    Local, integer MAXK, the maximum level to check.
!
  implicit none

  integer ( kind = 4 ) d
  real ( kind = 8 ) error_mc
  real ( kind = 8 ) error_sg
  real ( kind = 8 ) estimate
  real ( kind = 8 ) fn_integral
  real ( kind = 8 ), allocatable :: fx(:)
  integer ( kind = 4 ) k
  external kpn
  external kpn_order
  integer ( kind = 4 ) maxk
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) r
  real ( kind = 8 ), allocatable :: s(:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) trueval
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  d = 10
  maxk = 7

  trueval = fn_integral ( d );

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KPN_SPARSE_TEST:'
  write ( *, '(a)' ) '  KPN sparse grid:'
  write ( *, '(a)' ) '  Sparse Kronrod quadrature with Hermite weight over (-oo,+oo).'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
  write ( *, '(a)' ) ''

  do k = 2, maxk
!
!  Compute sparse grid estimate.
!
    call nwspgr_size ( kpn_order, d, k, n )
    allocate ( x(1:d,1:n) )
    allocate ( w(1:n) )
    call nwspgr ( kpn, kpn_order, d, k, n, n2, x, w )
    allocate ( fx(1:n2) )
    call fn_value ( d, n2, x, fx )
    estimate = dot_product ( w(1:n2), fx(1:n2) )

    error_sg = abs ( ( estimate - trueval ) / trueval )
!
!  Compute 1000 Monte Carlo estimates with same number of points, and average.
!
    allocate ( s(1000) )
    seed = 123456789

    do r = 1, 1000
      call r8mat_normal_01 ( d, n2, seed, x )
      call fn_value ( d, n2, x, fx )
      s(r) = sum ( fx ) / real ( n2, kind = 8 )
    end do
    error_mc = sqrt ( sum ( ( s(1:1000) - trueval ) ** 2 ) / 1000.0D+00 ) / trueval

    write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) d, k, n2, error_sg, error_mc

    deallocate ( fx )
    deallocate ( s )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine kpu_test ( )

!*****************************************************************************80
!
!! KPU_TEST uses the KPU function for 1D quadrature over [0,1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) exact
  real ( kind = 8 ) fu_integral
  real ( kind = 8 ), allocatable :: fx(:)
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  real ( kind = 8 ) q
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'KPU_TEST:'
  write ( *, '(a)' ) '  Kronrod-Patterson quadrature over [0,1]:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
  write ( *, '(a)' ) ' '

  d = 1
  exact = fu_integral ( d );

  do l = 1, 5

    call kpu_order ( l, n )

    allocate ( x(1:n) )
    allocate ( w(1:n) )
    call kpu ( n, x, w )

    allocate ( fx(1:n) )
    call fu_value ( d, n, x, fx )

    q = dot_product ( w(1:n), fx(1:n) )

    e = abs ( q - exact ) / exact

    write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

    deallocate ( fx )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine kpu_sparse_test ( )

!*****************************************************************************80
!
!! KPU_SPARSE_TEST uses the KPU function to build a sparse grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Local parameters:
!
!    Local, integer D, the spatial dimension.
!
!    Local, integer MAXK, the maximum level to check.
!
  implicit none

  integer ( kind = 4 ) d
  real ( kind = 8 ) error_mc
  real ( kind = 8 ) error_sg
  real ( kind = 8 ) estimate
  real ( kind = 8 ) fu_integral
  real ( kind = 8 ), allocatable :: fx(:)
  integer ( kind = 4 ) k
  external kpu
  external kpu_order
  integer ( kind = 4 ) maxk
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) r
  real ( kind = 8 ), allocatable :: s(:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) trueval
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:,:)

  d = 10
  maxk = 7

  trueval = fu_integral ( d );

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KPU_SPARSE_TEST:'
  write ( *, '(a)' ) '  KPU sparse grid:'
  write ( *, '(a)' ) '  Sparse Kronrod quadrature with unit weight over [0,1].'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
  write ( *, '(a)' ) ''

  do k = 2, maxk
!
!  Compute sparse grid estimate.
!
    call nwspgr_size ( kpu_order, d, k, n )
    allocate ( x(1:d,1:n) )
    allocate ( w(1:n) )
    call nwspgr ( kpu, kpu_order, d, k, n, n2, x, w )
    allocate ( fx(1:n2) )
    call fu_value ( d, n2, x, fx )
    estimate = dot_product ( w(1:n2), fx(1:n2) )

    error_sg = abs ( ( estimate - trueval ) / trueval )
!
!  Compute 1000 Monte Carlo estimates with same number of points, and average.
!
    allocate ( s(1000) )
    seed = 123456789

    do r = 1, 1000
      call r8mat_uniform_01 ( d, n2, seed, x )
      call fu_value ( d, n2, x, fx )
      s(r) = sum ( fx ) / real ( n2, kind = 8 )
    end do
    error_mc = sqrt ( sum ( ( s(1:1000) - trueval ) ** 2 ) / 1000.0D+00 ) / trueval

    write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) d, k, n2, error_sg, error_mc

    deallocate ( fx )
    deallocate ( s )
    deallocate ( w )
    deallocate ( x )

  end do

  return
end
subroutine nwspgr_size_test ( )

!*****************************************************************************80
!
!! NWSPGR_SIZE_TEST tests NWSPGR_SIZE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 January 2013
!
!  Author:
!
!    John Burkardt.
!
  implicit none

  external cce_order
  integer ( kind = 4 ) dim
  external gqn_order
  external gqu_order
  integer ( kind = 4 ) k
  external kpn_order
  external kpu_order
  integer ( kind = 4 ) r_size

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NWSPGR_SIZE_TEST:'
  write ( *, '(a)' ) '  NWSPGR_SIZE returns the size of a sparse grid, based on either:'
  write ( *, '(a)' ) '  one of the built-in 1D rules, or a family of 1D rules'
  write ( *, '(a)' ) '  supplied by the user.'

  dim = 2
  k = 3
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Kronrod-Patterson, [0,1], Dim ', dim, ', Level ', k, ', Symmetric'
  write ( *, '(a)' ) ''
  call nwspgr_size ( kpu_order, dim, k, r_size )
  write ( *, '(a,i6)' ) '  Full          ', r_size

  dim = 2
  k = 3
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Kronrod-Patterson, (-oo,+oo), Dim ', dim, ', Level ', k, ', Symmetric'
  write ( *, '(a)' ) ''
  call nwspgr_size ( kpn_order, dim, k, r_size )
  write ( *, '(a,i6)' ) '  Full          ', r_size

  dim = 2
  k = 3
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Gauss-Legendre, [0,1], Dim ', dim, ', Level ', k, ', Symmetric'
  write ( *, '(a)' ) ''
  call nwspgr_size ( gqu_order, dim, k, r_size )
  write ( *, '(a,i6)' ) '  Full          ', r_size

  dim = 2
  k = 3
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Gauss Hermite, (-oo,+oo), [0,1], Dim ', dim, ', Level ', k, ', Symmetric'
  write ( *, '(a)' ) ''
  call nwspgr_size ( gqn_order, dim, k, r_size )
  write ( *, '(a,i6)' ) '  Full          ', r_size

  dim = 2
  k = 3
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Clenshaw Curtis Exponential, [-1,+1], [0,1], Dim ', dim, ', Level ', k, ', Unsymmetric'
  write ( *, '(a)' ) ''
  call nwspgr_size ( cce_order, dim, k, r_size )
  write ( *, '(a,i6)' ) '  Full          ', r_size
!
!  Do a table.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Dimension / Level table for Clenshaw Curtis Exponential'
  write ( *, '(a)' ) ''
  write ( *, '(a)', advance = 'no' ) ' Dim: '
  do dim = 1, 10
    write ( *, '(2x,i6)', advance = 'no' ) dim
  end do
  write ( *, '(a)', advance = 'yes' ) ''
  write ( *, '(a)' ) 'Level'
  do k = 1, 5
    write ( *, '(2x,i2,2x)', advance = 'no' ) k
    do dim = 1, 10
      call nwspgr_size ( cce_order, dim, k, r_size )
      write ( *, '(2x,i6)', advance = 'no' ) r_size
    end do
    write ( *, '(a)', advance = 'yes' ) ''
  end do

  return
end
subroutine nwspgr_time_test ( )

!*****************************************************************************80
!
!! NWSPGR_TIME_TEST times NWSPGR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 January 2013
!
!  Author:
!
!    John Burkardt.
!
  implicit none

  external cce_order
  external cc
  integer ( kind = 4 ) dim
  external gqn
  external gqn_order
  external gqu
  external gqu_order
  integer ( kind = 4 ) k
  external kpn
  external kpn_order
  external kpu
  external kpu_order
  real ( kind = 8 ), allocatable :: nodes(:,:)
  integer ( kind = 4 ) r_size
  integer ( kind = 4 ) s_size
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ), allocatable :: weights(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NWSPGR_TIME_TEST:'
  write ( *, '(a)' ) '  This function measures the time in seconds required by NWSPGR'
  write ( *, '(a)' ) '  to compute a sparse grid, based on either:'
  write ( *, '(a)' ) '  one of the built-in 1D rules, or a family of 1D rules'
  write ( *, '(a)' ) '  supplied by the user.'

  dim = 20
  k = 5
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Kronrod-Patterson, [0,1], Dim ', dim, ', Level ', k, ', Symmetric'
  write ( *, '(a)' ) ''
  call nwspgr_size ( kpu_order, dim, k, r_size )
  allocate ( nodes(dim,r_size) )
  allocate ( weights(r_size) )
  call cpu_time ( t1 )
  call nwspgr ( kpu, kpu_order, dim, k, r_size, s_size, nodes, weights )
  call cpu_time ( t2 )
  deallocate ( nodes )
  deallocate ( weights )
  write ( *, '(a,g14.6)' ) '  Full          ', t2 - t1

  dim = 20
  k = 5
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Kronrod-Patterson, (-oo,+oo), Dim ', dim, ', Level ', k, ', Symmetric'
  write ( *, '(a)' ) ''
  call nwspgr_size ( kpn_order, dim, k, r_size )
  allocate ( nodes(dim,r_size) )
  allocate ( weights(r_size) )
  call cpu_time ( t1 )
  call nwspgr ( kpn, kpn_order, dim, k, r_size, s_size, nodes, weights )
  call cpu_time ( t2 )
  deallocate ( nodes )
  deallocate ( weights )
  write ( *, '(a,g14.6)' ) '  Full          ', t2 - t1

  dim = 20
  k = 5
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Gauss-Legendre, [0,1], Dim ', dim, ', Level ', k, ', Symmetric'
  write ( *, '(a)' ) ''
  call nwspgr_size ( gqu_order, dim, k, r_size )
  allocate ( nodes(dim,r_size) )
  allocate ( weights(r_size) )
  call cpu_time ( t1 )
  call nwspgr ( gqu, gqu_order, dim, k, r_size, s_size, nodes, weights )
  call cpu_time ( t2 )
  deallocate ( nodes )
  deallocate ( weights )
  write ( *, '(a,g14.6)' ) '  Full          ', t2 - t1

  dim = 20
  k = 5
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Gauss Hermite, (-oo,+oo), [0,1], Dim ', dim, ', Level ', k, ', Symmetric'
  write ( *, '(a)' ) ''
  call nwspgr_size ( gqn_order, dim, k, r_size )
  allocate ( nodes(dim,r_size) )
  allocate ( weights(r_size) )
  call cpu_time ( t1 )
  call nwspgr ( gqn, gqn_order, dim, k, r_size, s_size, nodes, weights )
  call cpu_time ( t2 )
  deallocate ( nodes )
  deallocate ( weights )
  write ( *, '(a,g14.6)' ) '  Full          ', t2 - t1

  dim = 20
  k = 5
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Clenshaw Curtis Exponential, [-1,+1], [0,1], Dim ', dim, ', Level ', k, ', Unsymmetric'
  write ( *, '(a)' ) ''
  call nwspgr_size ( cce_order, dim, k, r_size )
  allocate ( nodes(dim,r_size) )
  allocate ( weights(r_size) )
  call cpu_time ( t1 )
  call nwspgr ( cc, cce_order, dim, k, r_size, s_size, nodes, weights )
  call cpu_time ( t2 )
  deallocate ( nodes )
  deallocate ( weights )
  write ( *, '(a,g14.6)' ) '  Full          ', t2 - t1
!
!  Do a table.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Dimension / Level table for Clenshaw Curtis Exponential'
  write ( *, '(a)' ) ''
  write ( *, '(a)', advance = 'no' ) ' Dim: '
  do dim = 1, 10
    write ( *, '(2x,i10)', advance = 'no' ) dim
  end do
  write ( *, '(a)', advance = 'yes' ) ''
  write ( *, '(a)' ) 'Level'
  do k = 1, 5
    write ( *, '(2x,i2,2x)', advance = 'no' ) k
    do dim = 1, 10
      call nwspgr_size ( cce_order, dim, k, r_size )
      allocate ( nodes(dim,r_size) )
      allocate ( weights(r_size) )
      call cpu_time ( t1 )
      call nwspgr ( cc, cce_order, dim, k, r_size, s_size, nodes, weights )
      call cpu_time ( t2 )
      deallocate ( nodes )
      deallocate ( weights )
      write ( *, '(2x,g10.2)', advance = 'no' ) t2 - t1
    end do
    write ( *, '(a)', advance = 'yes' ) ''
  end do

  write ( *, '(a)' ) ''
  write ( *, '(a)', advance = 'no' ) ' Dim: '
  do dim = 11, 20
    write ( *, '(2x,i10)', advance = 'no' ) dim
  end do
  write ( *, '(a)', advance = 'yes' ) ''
  write ( *, '(a)' ) 'Level'
  do k = 1, 5
    write ( *, '(2x,i2,2x)', advance = 'no' ) k
    do dim = 11, 20
      call nwspgr_size ( cce_order, dim, k, r_size )
      allocate ( nodes(dim,r_size) )
      allocate ( weights(r_size) )
      call cpu_time ( t1 )
      call nwspgr ( cc, cce_order, dim, k, r_size, s_size, nodes, weights )
      call cpu_time ( t2 )
      deallocate ( nodes )
      deallocate ( weights )
      write ( *, '(2x,g10.2)', advance = 'no' ) t2 - t1
    end do
    write ( *, '(a)', advance = 'yes' ) ''
  end do

  return
end
subroutine nwspgr_test ( )

!*****************************************************************************80
!
!! NWSPGR_TEST tests NWSPGR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2012
!
!  Author:
!
!    John Burkardt.
!
  implicit none

  external cce_order
  external cc
  integer ( kind = 4 ) dim
  external gqn
  external gqn_order
  external gqu
  external gqu_order
  integer ( kind = 4 ) k
  external kpn
  external kpn_order
  external kpu
  external kpu_order
  real ( kind = 8 ), allocatable :: nodes(:,:)
  integer ( kind = 4 ) r_size
  integer ( kind = 4 ) s_size
  real ( kind = 8 ), allocatable :: weights(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NWSPGR_TEST:'
  write ( *, '(a)' ) '  NWSPGR generates a sparse grid, based on either:'
  write ( *, '(a)' ) '  one of the built-in 1D rules, or a family of 1D rules'
  write ( *, '(a)' ) '  supplied by the user.'

  dim = 2
  k = 3
  call nwspgr_size ( kpu_order, dim, k, r_size )
  allocate ( nodes(dim,r_size) )
  allocate ( weights(r_size) )
  call nwspgr ( kpu, kpu_order, dim, k, r_size, s_size, nodes, weights )
  call quad_rule_print ( dim, s_size, nodes, weights, '  Kronrod-Patterson, [0,1], Dim 2, Level 3' )
  deallocate ( nodes )
  deallocate ( weights )

  dim = 2
  k = 3
  call nwspgr_size ( kpn_order, dim, k, r_size )
  allocate ( nodes(dim,r_size) )
  allocate ( weights(r_size) )
  call nwspgr ( kpn, kpn_order, dim, k, r_size, s_size, nodes, weights )
  call quad_rule_print ( dim, s_size, nodes, weights, '  Kronrod-Patterson, (-oo,+oo), Dim 2, Level 3' )
  deallocate ( nodes )
  deallocate ( weights )

  dim = 2
  k = 3
  call nwspgr_size ( gqu_order, dim, k, r_size )
  allocate ( nodes(dim,r_size) )
  allocate ( weights(r_size) )
  call nwspgr ( gqu, gqu_order, dim, k, r_size, s_size, nodes, weights )
  call quad_rule_print ( dim, s_size, nodes, weights, '  Gauss-Legendre, [0,1], Dim 2, Level 3' )
  deallocate ( nodes )
  deallocate ( weights )

  dim = 2
  k = 3
  call nwspgr_size ( gqn_order, dim, k, r_size )
  allocate ( nodes(dim,r_size) )
  allocate ( weights(r_size) )
  call nwspgr ( gqn, gqn_order, dim, k, r_size, s_size, nodes, weights )
  call quad_rule_print ( dim, s_size, nodes, weights, '  Gauss Hermite, (-oo,+oo), Dim 2, Level 3' )
  deallocate ( nodes )
  deallocate ( weights )

  dim = 2
  k = 3
  call nwspgr_size ( cce_order, dim, k, r_size )
  allocate ( nodes(dim,r_size) )
  allocate ( weights(r_size) )
  call nwspgr ( cc, cce_order, dim, k, r_size, s_size, nodes, weights )
  call quad_rule_print ( dim, s_size, nodes, weights, &
    '  Clenshaw Curtis Exponential, [-1,+1], Dim 2, Level 3' )
  deallocate ( nodes )
  deallocate ( weights )

  return
end
subroutine order_report ( )

!*****************************************************************************80
!
!! ORDER_REPORT reports on the order of each family of rules.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) ap
  integer ( kind = 4 ) k
  integer ( kind = 4 ), dimension ( 5 ) :: kpn_order = (/ &
    1, 3, 9, 19, 35 /)
  integer ( kind = 4 ) l
  integer ( kind = 4 ) o
  integer ( kind = 4 ) rp

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ORDER_REPORT'
  write ( *, '(a)' ) '  For each family of rules, report:'
  write ( *, '(a)' ) '  L,  the level index,'
  write ( *, '(a)' ) '  RP, the required polynomial precision,'
  write ( *, '(a)' ) '  AP, the actual polynomial precision,'
  write ( *, '(a)' ) '  O,  the rule order (number of points).'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  GQN family'
  write ( *, '(a)' ) '  Gauss quadrature, exponential weight, (-oo,+oo)'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   L  RP  AP   O'
  write ( *, '(a)' ) ' '

  do l = 1, 25
    rp = 2 * l - 1
    o = l
    ap = 2 * o - 1
    write ( *, '(2x,i2,2x,i2,2x,i2,2x,i2)' ) l, rp, ap, o
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  GQU family'
  write ( *, '(a)' ) '  Gauss quadrature, unit weight, [0,1]'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   L  RP  AP   O'
  write ( *, '(a)' ) ' '

  do l = 1, 25
    rp = 2 * l - 1
    o = l
    ap = 2 * o - 1
    write ( *, '(2x,i2,2x,i2,2x,i2,2x,i2)' ) l, rp, ap, o
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  KPN family'
  write ( *, '(a)' ) '  Gauss-Kronrod-Patterson quadrature, exponential weight, (-oo,+oo)'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   L  RP  AP   O'
  write ( *, '(a)' ) ' '

  k = 1
  o = 1
  ap = 1

  do l = 1, 25

    rp = 2 * l - 1

    do while ( ap < rp )

      if ( k == 5 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  No higher order rule is available!'
        exit
      end if
!
!  Can we use a simple rule?
!
      if ( rp < kpn_order(k+1) ) then
        o = rp
        ap = rp
!
!  Otherwise, move to next higher rule.
!
      else
        k = k + 1
        ap = 2 * kpn_order(k) - kpn_order(k-1)
        o = kpn_order(k)
      end if

    end do

    write ( *, '(2x,i2,2x,i2,2x,i2,2x,i2)' ) l, rp, ap, o

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  KPU family'
  write ( *, '(a)' ) '  Gauss-Kronrod-Patterson quadrature, unit weight, [0,1]'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '   L  RP  AP   O'
  write ( *, '(a)' ) ' '

  do l = 1, 25
    rp = 2 * l - 1
    o = 1
    ap = 1
    do while ( ap < rp )
      o = 2 * ( o + 1 ) - 1
      ap = ( 3 * o + 1 ) / 2
    end do
    write ( *, '(2x,i2,2x,i2,2x,i2,2x,i2)' ) l, rp, ap, o
  end do

  return
end
subroutine symmetric_sparse_size_test ( )

!*****************************************************************************80
!
!! SYMMETRIC_SPARSE_SIZE_TEST tests SYMMETRIC_SPARSE_SIZE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2012
!
!  Author:
!
!    Original MATLAB version by Florian Heiss, Viktor Winschel.
!    FORTRAN90 version by John Burkardt.
!
!  Local parameters:
!
!    Local, integer D, the spatial dimension.
!
!    Local, integer MAXK, the maximum level to check.
!

  implicit none

  integer ( kind = 4 ), parameter :: test_num = 3

  integer ( kind = 4 ) dim
  integer ( kind = 4 ), dimension ( test_num ) :: dim_test = (/ 5, 5, 3 /)
  real ( kind = 8 ), dimension ( 6, 5 ) :: nodes1 = reshape ( (/ &
   0.0, 0.0, 0.0, 0.0, 0.0, 1.0, &
   0.0, 0.0, 0.0, 0.0, 1.0, 0.0, &
   0.0, 0.0, 0.0, 1.0, 0.0, 0.0, &
   0.0, 0.0, 1.0, 0.0, 0.0, 0.0, &
   0.0, 1.0, 0.0, 0.0, 0.0, 0.0 /), (/ 6, 5 /) )
  real ( kind = 8 ), dimension ( 21, 5 ) :: nodes2 = reshape ( (/ &
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.73205, &
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.73205, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, &
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.73205, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, &
    0.0, 0.0, 0.0, 1.0, 1.0, 1.73205, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, &
    0.0, 1.0, 1.73205, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0 /), (/ 21, 5 /) )
  real ( kind = 8 ), dimension ( 23, 3 ) :: nodes3 = reshape ( (/ &
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.741964, 1.0, &
    1.0, 1.0, 1.0, 1.0, 1.0, 1.73205, 1.73205, 1.73205, 2.33441, &
    0.0, 0.0, 0.0, 0.0, 0.0, 0.741964, 1.0, 1.0, 1.0, 1.73205, 1.73205, 2.33441, &
    0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.73205, 0.0, 0.0, 1.0, 0.0, &
    0.0, 0.741964, 1.0, 1.73205, 2.33441, 0.0, 0.0, 1.0, 1.73205, 0.0, 1.0, 0.0, &
    0.0, 0.0, 1.0, 1.73205, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 /), (/ 23, 3 /) )
  integer ( kind = 4 ) r
  integer ( kind = 4 ), dimension ( test_num ) :: r_test = (/ 6, 21, 23 /)
  integer ( kind = 4 ) r2
  integer ( kind = 4 ) test
  real ( kind = 8 ) x0

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SYMMETRIC_SPARSE_SIZE_TEST'
  write ( *, '(a)' ) '  Given a symmetric sparse grid rule represented only by'
  write ( *, '(a)' ) '  the points with positive values, determine the total number'
  write ( *, '(a)' ) '  of points in the grid.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  For dimension DIM, we report '
  write ( *, '(a)' ) '  R, the number of points in the positive orthant, and '
  write ( *, '(a)' ) '  R2, the total number of points.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       DIM         R        R2'
  write ( *, '(a)' ) ' '

  x0 = 0.0

  do test = 1, test_num

    r = r_test(test)
    dim = dim_test(test)
    if ( test == 1 ) then
      call symmetric_sparse_size ( r, dim, nodes1, x0, r2 )
    else if ( test == 2 ) then
      call symmetric_sparse_size ( r, dim, nodes2, x0, r2 )
    else if ( test == 3 ) then
      call symmetric_sparse_size ( r, dim, nodes3, x0, r2 )
    end if

    write ( *, '(2x,i8,2x,i8,2x,i8)' ) dim, r, r2

  end do

  return
end
subroutine tensor_product_test ( )

!*****************************************************************************80
!
!! TENSOR_PRODUCT_TEST tests TENSOR_PRODUCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: order1 = 2
  integer ( kind = 4 ), parameter :: order2 = 3
  integer ( kind = 4 ), parameter :: order3 = 2

  integer ( kind = 4 ) d
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i4vec_product
  integer ( kind = 4 ) i4vec_sum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n1d
  integer ( kind = 4 ), allocatable :: order1d(:)
  real ( kind = 8 ), allocatable :: w1d(:)
  real ( kind = 8 ), allocatable :: wnd(:)
  real ( kind = 8 ) :: w1_1d(order1) = (/ 1.0D+00, 1.0D+00 /)
  real ( kind = 8 ) :: w2_1d(order2) = (/ 0.25D+00, 0.50D+00, 0.25D+00 /)
  real ( kind = 8 ) :: w3_1d(order3) = (/ 2.50D+00, 2.50D+00 /)
  real ( kind = 8 ) :: x1_1d(order1) = (/ -1.0D+00, +1.0D+00 /)
  real ( kind = 8 ) :: x2_1d(order2) = (/ 2.0D+00, 2.5D+00, 3.0D+00 /)
  real ( kind = 8 ) :: x3_1d(order3) = (/ 10.0D+00, 15.0D+00 /)
  real ( kind = 8 ), allocatable :: x1d(:)
  real ( kind = 8 ), allocatable :: xnd(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TENSOR_PRODUCT_TEST:'
  write ( *, '(a)' ) '  Given a sequence of 1D quadrature rules, construct the'
  write ( *, '(a)' ) '  tensor product rule.'
!
!  1D rule.
!
  d = 1
  allocate ( order1d(1:d) )

  order1d(1) = order1

  n1d = i4vec_sum ( d, order1d )
  allocate ( x1d(1:n1d) )
  allocate ( w1d(1:n1d) )

  n = i4vec_product ( d, order1d )
  allocate ( xnd(1:d,1:n) )
  allocate ( wnd(1:n) )

  i1 = 1
  i2 = order1
  x1d(i1:i2) = x1_1d(1:order1)
  w1d(i1:i2) = w1_1d(1:order1)
 
  call tensor_product ( d, order1d, n1d, x1d, w1d, n, xnd, wnd )
  
  call quad_rule_print ( d, n, xnd, wnd, '  A 1D rule over [-1,+1]:' )

  deallocate ( order1d )
  deallocate ( w1d )
  deallocate ( wnd )
  deallocate ( x1d )
  deallocate ( xnd )
!
!  2D rule.
!
  d = 2
  allocate ( order1d(1:d) )

  order1d(1:2) = (/ order1, order2 /)

  n1d = i4vec_sum ( d, order1d )
  allocate ( x1d(1:n1d) )
  allocate ( w1d(1:n1d) )

  n = i4vec_product ( d, order1d )
  allocate ( xnd(1:d,1:n) )
  allocate ( wnd(1:n) )

  i1 = 1
  i2 = order1
  x1d(i1:i2) = x1_1d(1:order1)
  w1d(i1:i2) = w1_1d(1:order1)
  i1 = i2 + 1
  i2 = i2 + order2
  x1d(i1:i2) = x2_1d(1:order2)
  w1d(i1:i2) = w2_1d(1:order2)

  call tensor_product ( d, order1d, n1d, x1d, w1d, n, xnd, wnd )
  
  call quad_rule_print ( d, n, xnd, wnd, '  A 2D rule over [-1,+1] x [2.0,3.0]:' )

  deallocate ( order1d )
  deallocate ( w1d )
  deallocate ( wnd )
  deallocate ( x1d )
  deallocate ( xnd )
!
!  3D rule.
!
  d = 3
  allocate ( order1d(1:d) )

  order1d(1:3) = (/ order1, order2, order3 /)

  n1d = i4vec_sum ( d, order1d )
  allocate ( x1d(1:n1d) )
  allocate ( w1d(1:n1d) )

  n = i4vec_product ( d, order1d )
  allocate ( xnd(1:d,1:n) )
  allocate ( wnd(1:n) )

  i1 = 1
  i2 = order1
  x1d(i1:i2) = x1_1d(1:order1)
  w1d(i1:i2) = w1_1d(1:order1)
  i1 = i2 + 1
  i2 = i2 + order2
  x1d(i1:i2) = x2_1d(1:order2)
  w1d(i1:i2) = w2_1d(1:order2)
  i1 = i2 + 1
  i2 = i2 + order3
  x1d(i1:i2) = x3_1d(1:order3)
  w1d(i1:i2) = w3_1d(1:order3)

  call tensor_product ( d, order1d, n1d, x1d, w1d, n, xnd, wnd )

  call quad_rule_print ( d, n, xnd, wnd, &
    '  A 3D rule over [-1,+1] x [2.0,3.0] x [10.0,15.0]:' )

  deallocate ( order1d )
  deallocate ( w1d )
  deallocate ( wnd )
  deallocate ( x1d )
  deallocate ( xnd )

  return
end
subroutine tensor_product_cell_test ( )

!*****************************************************************************80
!
!! TENSOR_PRODUCT_CELL_TEST tests TENSOR_PRODUCT_CELL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: order1 = 2
  integer ( kind = 4 ), parameter :: order2 = 3
  integer ( kind = 4 ), parameter :: order3 = 2

  integer ( kind = 4 ) d
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) np
  integer ( kind = 4 ) :: nr(3) = (/ 2, 3, 2 /)
  integer ( kind = 4 ) roff(4)
  real ( kind = 8 ), allocatable :: wc(:)
  real ( kind = 8 ), allocatable :: wp(:)
  real ( kind = 8 ) :: w1_1d(order1) = (/ 1.0D+00, 1.0D+00 /)
  real ( kind = 8 ) :: w2_1d(order2) = (/ 0.25D+00, 0.50D+00, 0.25D+00 /)
  real ( kind = 8 ) :: w3_1d(order3) = (/ 2.50D+00, 2.50D+00 /)
  real ( kind = 8 ) :: x1_1d(order1) = (/ -1.0D+00, +1.0D+00 /)
  real ( kind = 8 ) :: x2_1d(order2) = (/ 2.0D+00, 2.5D+00, 3.0D+00 /)
  real ( kind = 8 ) :: x3_1d(order3) = (/ 10.0D+00, 15.0D+00 /)
  real ( kind = 8 ), allocatable :: xc(:)
  real ( kind = 8 ), allocatable :: xp(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TENSOR_PRODUCT_TEST_CELL:'
  write ( *, '(a)' ) '  Given a set of 1D quadrature rules stored in a cell array,'
  write ( *, '(a)' ) '  construct the tensor product rule.'
!
!  We can construct ROFF once and for all.
!
  call r8cvv_offset ( 3, nr, roff )
!
!  1D rule.
!
  d = 1
  nc = sum ( nr(1:d) )
  allocate ( xc(1:nc) )
  call r8cvv_rset ( nc, xc, d, roff, 1, x1_1d )
  allocate ( wc(1:nc) )
  call r8cvv_rset ( nc, wc, d, roff, 1, w1_1d )
  np = product ( nr(1:d) )
  allocate( xp(1:d,1:np) )
  allocate( wp(1:np) )

  call tensor_product_cell ( nc, xc, wc, d, nr, roff, np, xp, wp )

  call quad_rule_print ( d, np, xp, wp, '  A 1D rule over [-1,+1]:' )

  deallocate ( wc )
  deallocate ( wp )
  deallocate ( xc )
  deallocate ( xp )
!
!  2D rule.
!
  d = 2
  nc = sum ( nr(1:d) )
  allocate ( xc(1:nc) )
  call r8cvv_rset ( nc, xc, d, roff, 1, x1_1d )
  call r8cvv_rset ( nc, xc, d, roff, 2, x2_1d )
  allocate ( wc(1:nc) )
  call r8cvv_rset ( nc, wc, d, roff, 1, w1_1d )
  call r8cvv_rset ( nc, wc, d, roff, 2, w2_1d )
  np = product ( nr(1:d) )
  allocate( xp(1:d,1:np) )
  allocate( wp(1:np) )

  call tensor_product_cell ( nc, xc, wc, d, nr, roff, np, xp, wp )

  call quad_rule_print ( d, np, xp, wp, '  A 1D rule over [-1,+1]:' )

  deallocate ( wc )
  deallocate ( wp )
  deallocate ( xc )
  deallocate ( xp )
!
!  3D rule.
!
  d = 3
  nc = sum ( nr(1:d) )
  allocate ( xc(1:nc) )
  call r8cvv_rset ( nc, xc, d, roff, 1, x1_1d )
  call r8cvv_rset ( nc, xc, d, roff, 2, x2_1d )
  call r8cvv_rset ( nc, xc, d, roff, 3, x3_1d )
  allocate ( wc(1:nc) )
  call r8cvv_rset ( nc, wc, d, roff, 1, w1_1d )
  call r8cvv_rset ( nc, wc, d, roff, 2, w2_1d )
  call r8cvv_rset ( nc, wc, d, roff, 3, w3_1d )
  np = product ( nr(1:d) )
  allocate( xp(1:d,1:np) )
  allocate( wp(1:np) )

  call tensor_product_cell ( nc, xc, wc, d, nr, roff, np, xp, wp )

  call quad_rule_print ( d, np, xp, wp, '  A 1D rule over [-1,+1]:' )

  deallocate ( wc )
  deallocate ( wp )
  deallocate ( xc )
  deallocate ( xp )

  return
end

