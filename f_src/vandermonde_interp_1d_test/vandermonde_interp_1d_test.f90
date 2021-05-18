program main

!*****************************************************************************80
!
!! MAIN is the main program for VANDERMONDE_INTERP_1D_TEST.
!
!  Discussion:
!
!    VANDERMONDE_INTERP_1D_TEST tests the VANDERMONDE_INTERP_1D library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) prob
  integer ( kind = 4 ) prob_num

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VANDERMONDE_INTERP_1D_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the VANDERMONDE_INTERP_1D library.'
  write ( *, '(a)' ) '  The QR_SOLVE library is needed.'
  write ( *, '(a)' ) '  The R8LIB library is needed.'
  write ( *, '(a)' ) '  This test needs the CONDITION library.'
  write ( *, '(a)' ) '  This test needs the TEST_INTERP library.'

  call vandermonde_coef_1d_test ( )

  call vandermonde_matrix_1d_test ( )

  call vandermonde_value_1d_test ( )

  call p00_prob_num ( prob_num )

  do prob = 1, prob_num
    call test01 ( prob )
  end do

  do prob = 1, prob_num
    call test02 ( prob )
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VANDERMONDE_INTERP_1D_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine vandermonde_coef_1d_test ( )

!*****************************************************************************80
!
!! VANDERMONDE_COEF_1D_TEST tests VANDERMONDE_COEF_1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nd = 5

  real ( kind = 8 ) cd(nd)
  real ( kind = 8 ), dimension ( nd ) :: xd = (/ &
    0.0D+00, 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00 /)
  real ( kind = 8 ), dimension ( nd ) :: yd = (/ &
    24.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VANDERMONDE_COEF_1D_TEST'
  write ( *, '(a)' ) &
    '  VANDERMONDE_COEF_1D sets the Vandermonde coefficients for 1D interpolation.'

  call r8vec2_print ( nd, xd, yd, '  Interpolation data:' )

  call vandermonde_coef_1d ( nd, xd, yd, cd )

  call r8vec_print ( nd, cd, '  Vandermonde interpolant coefficients:' )

  call r8poly_print ( nd - 1, cd, '  Vandermonde interpolant polynomial:' )

  return
end
subroutine vandermonde_matrix_1d_test ( )

!*****************************************************************************80
!
!! VANDERMONDE_MATRIX_1D_TEST tests VANDERMONDE_MATRIX_1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nd = 4

  real ( kind = 8 ) ad(nd,nd)
  real ( kind = 8 ), dimension ( nd ) :: xd = (/ &
    -1.0D+00, 2.0D+00, 3.0D+00, 5.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VANDERMONDE_MATRIX_1D_TEST'
  write ( *, '(a)' ) &
    '  VANDERMONDE_MATRIX_1D sets the Vandermonde matrix for 1D interpolation.'

  call vandermonde_matrix_1d ( nd, xd, ad )

  call r8mat_print ( nd, nd, ad, '  Vandermonde matrix:' )

  return
end
subroutine vandermonde_value_1d_test ( )

!*****************************************************************************80
!
!! VANDERMONDE_VALUE_1D_TEST tests VANDERMONDE_VALUE_1D.
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

  integer ( kind = 4 ), parameter :: nd = 5
  integer ( kind = 4 ), parameter :: ni = 16

  real ( kind = 8 ), dimension (0:nd-1) :: cd = (/ &
    24.0D+00, -50.0D+00, +35.0D+00, -10.0D+00, 1.0D+00 /)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo
  real ( kind = 8 ) xi(ni)
  real ( kind = 8 ) yi(ni)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'VANDERMONDE_VALUE_1D_TEST'
  write ( *, '(a)' ) '  VANDERMONDE_VALUE_1D evaluates a Vandermonde interpolant.'

  call r8poly_print ( nd - 1, cd, '  The polynomial coefficients:' )

  x_lo = 0.0D+00
  x_hi = 5.0D+00
  call r8vec_linspace ( ni, x_lo, x_hi, xi )

  call vandermonde_value_1d ( nd, cd, ni, xi, yi )

  call r8vec2_print ( ni, xi, yi, '  X, P(X)' )

  return
end
subroutine test01 ( prob )

!*****************************************************************************80
!
!! TEST01 tests VANDERMONDE_INTERP_1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 September 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: ad(:,:)
  real ( kind = 8 ), allocatable :: cd(:)
  real ( kind = 8 ) condition
  logical, parameter :: debug = .false.
  real ( kind = 8 ) int_error
  real ( kind = 8 ) ld
  real ( kind = 8 ) li
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) ni
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
  write ( *, '(a,i2)' ) '  Interpolate data from TEST_INTERP problem #', prob

  call p00_data_num ( prob, nd )
  write ( *, '(a,i2)' ) '  Number of data points = ', nd

  allocate ( xy(1:2,1:nd) )

  call p00_data ( prob, 2, nd, xy )
  
  if ( debug ) then
    call r8mat_transpose_print ( 2, nd, xy, '  Data array:' )
  end if

  allocate ( xd(1:nd) )
  allocate ( yd(1:nd) )

  xd(1:nd) = xy(1,1:nd)
  yd(1:nd) = xy(2,1:nd)
!
!  Compute Vandermonde matrix and get condition number.
!
  allocate ( ad(1:nd,1:nd) )

  call vandermonde_matrix_1d ( nd, xd, ad )

  call condition_hager ( nd, ad, condition )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Condition of Vandermonde matrix is ', condition
!
!  Solve linear system.
!
  allocate ( cd(1:nd) )

  call qr_solve ( nd, nd, ad, yd, cd )
!
!  #1:  Does interpolant match function at interpolation points?
!
  ni = nd
  allocate ( xi(1:ni) )
  allocate ( yi(1:ni) )
  xi(1:ni) = xd(1:ni)
  call vandermonde_value_1d ( nd, cd, ni, xi, yi )

  int_error = r8vec_norm_affine ( ni, yi, yd ) / real ( ni, kind = 8 )

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

  call vandermonde_value_1d ( nd, cd, ni, xi, yi )

  ld = sum ( sqrt ( ( ( xd(2:nd) - xd(1:nd-1) ) / ( xmax - xmin ) )**2 &
                  + ( ( yd(2:nd) - yd(1:nd-1) ) / ( ymax - ymin ) )**2 ) )

  li = sum ( sqrt ( ( ( xi(2:ni) - xi(1:ni-1) ) / ( xmax - xmin ) )**2 &
                  + ( ( yi(2:ni) - yi(1:ni-1) ) / ( ymax - ymin ) )**2 ) )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) &
    '  Normalized length of piecewise linear interpolant = ', ld
  write ( *, '(a,g14.6)' ) &
    '  Normalized length of polynomial interpolant       = ', li

  deallocate ( ad )
  deallocate ( cd )
  deallocate ( xd )
  deallocate ( xi )
  deallocate ( xy )
  deallocate ( yd )
  deallocate ( yi )

  return
end
subroutine test02 ( prob )

!*****************************************************************************80
!
!! TEST02 tests VANDERMONDE_INTERP_1D.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) PROB, the problem index.
!
  implicit none

  real ( kind = 8 ), allocatable :: ad(:,:)
  real ( kind = 8 ), allocatable :: cd(:)
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = 255 ) interp_filename
  integer ( kind = 4 ) interp_unit
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) ni
  character ( len = 255 ) output_filename
  integer ( kind = 4 ) prob
  real ( kind = 8 ), allocatable :: xd(:)
  real ( kind = 8 ), allocatable :: xi(:)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ), allocatable :: xy(:,:)
  real ( kind = 8 ), allocatable :: yd(:)
  real ( kind = 8 ), allocatable :: yi(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) &
    '  VANDERMONDE_MATRIX_1D sets the Vandermonde linear system'
  write ( *, '(a)' ) '  for the interpolating polynomial.'
  write ( *, '(a,i2)' ) '  Interpolate data from TEST_INTERP problem #', prob

  call p00_data_num ( prob, nd )
  write ( *, '(a,i4)' ) '  Number of data points = ', nd

  allocate ( xy(1:2,1:nd) )

  call p00_data ( prob, 2, nd, xy )
  
  call r8mat_transpose_print ( 2, nd, xy, '  Data array:' )

  allocate ( xd(1:nd) )
  allocate ( yd(1:nd) )
  
  xd(1:nd) = xy(1,1:nd)
  yd(1:nd) = xy(2,1:nd)
!
!  Set up the Vandermonde matrix AD.
!
  allocate ( ad(1:nd,1:nd) )

  call vandermonde_matrix_1d ( nd, xd, ad )
!
!  Solve the linear system for the polynomial coefficients CD.
!
  allocate ( cd(1:nd) )

  call qr_solve ( nd, nd, ad, yd, cd )
!
!  Create data file.
!
  write ( data_filename, '(a,i2.2,a)' ) 'data', prob, '.txt'
  call get_unit ( data_unit )
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do j = 1, nd
    write ( data_unit, '(2x,g14.6,2x,g14.6)' ) xd(j), yd(j)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Created graphics data file "' // trim ( data_filename ) // '".'
!
!  Create interp file.
!
  ni = 501
  call r8vec_min ( nd, xd, xmin )
  call r8vec_max ( nd, xd, xmax )
  allocate ( xi(1:ni) )
  allocate ( yi(1:ni) )
  call r8vec_linspace ( ni, xmin, xmax, xi )
  call vandermonde_value_1d ( nd, cd, ni, xi, yi )

  write ( interp_filename, '(a,i2.2,a)' ) 'interp', prob, '.txt'
  call get_unit ( interp_unit )
  open ( unit = interp_unit, file = interp_filename, status = 'replace' )
  do j = 1, ni
    write ( interp_unit, '(2x,g14.6,2x,g14.6)' ) xi(j), yi(j)
  end do
  close ( unit = interp_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Created graphics interp file "' // trim ( interp_filename ) // '".'
!
!  Plot the data and the interpolant.
!
  write ( command_filename, '(a,i2.2,a)' ) 'commands', prob, '.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( output_filename, '(a,i2.2,a)' ) 'plot', prob, '.png'

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( output_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
  write ( command_unit, '(a)' ) &
    'set title "Data versus Vandermonde Polynomial Interpolant"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 with points pt 7 ps 2 lc rgb "blue",\'
  write ( command_unit, '(a)' ) '     "' // trim ( interp_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "red"'

  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created graphics command file "' // trim ( command_filename ) // '".'

  deallocate ( ad )
  deallocate ( cd )
  deallocate ( xd )
  deallocate ( xi )
  deallocate ( xy )
  deallocate ( yd )
  deallocate ( yi )

  return
end
subroutine r8poly_print ( n, a, title )

!*****************************************************************************80
!
!! R8POLY_PRINT prints out a polynomial.
!
!  Discussion:
!
!    The power sum form is:
!
!      p(x) = a(0) + a(1) * x + ... + a(n-1) * x^(n-1) + a(n) * x^(n)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of A.
!
!    Input, real ( kind = 8 ) A(0:N), the polynomial coefficients.
!    A(0) is the constant term and
!    A(N) is the coefficient of X^N.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(0:n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) mag
  character plus_minus
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '

  if ( n < 0 ) then
    write ( *, '( ''  p(x) = 0'' )' )
    return
  end if

  if ( a(n) < 0.0D+00 ) then
    plus_minus = '-'
  else
    plus_minus = ' '
  end if

  mag = abs ( a(n) )

  if ( 2 <= n ) then
    write ( *, '( ''  p(x) = '', a1, g14.6, '' * x ^ '', i3 )' ) &
      plus_minus, mag, n
  else if ( n == 1 ) then
    write ( *, '( ''  p(x) = '', a1, g14.6, '' * x'' )' ) &
      plus_minus, mag
  else if ( n == 0 ) then
    write ( *, '( ''  p(x) = '', a1, g14.6 )' ) plus_minus, mag
  end if

  do i = n - 1, 0, -1

    if ( a(i) < 0.0D+00 ) then
      plus_minus = '-'
    else
      plus_minus = '+'
    end if

    mag = abs ( a(i) )

    if ( mag /= 0.0D+00 ) then

      if ( 2 <= i ) then
        write ( *, ' ( ''         '', a1, g14.6, '' * x ^ '', i3 )' ) &
          plus_minus, mag, i
      else if ( i == 1 ) then
        write ( *, ' ( ''         '', a1, g14.6, '' * x'' )' ) plus_minus, mag
      else if ( i == 0 ) then
        write ( *, ' ( ''         '', a1, g14.6 )' ) plus_minus, mag
      end if
    end if

  end do

  return
end

