program main

!*****************************************************************************80
!
!! MAIN is the main program for NEWTON_INTERP_1D_TEST.
!
!  Discussion:
!
!    NEWTON_INTERP_1D_TEST tests the NEWTON_INTERP_1D library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2015
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
  write ( *, '(a)' ) 'NEWTON_INTERP_1D_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the NEWTON_INTERP_1D library.'
  write ( *, '(a)' ) '  The R8LIB library is needed.'
  write ( *, '(a)' ) '  This test needs the TEST_INTERP library as well.'

  call newton_coef_1d_test ( )

  call newton_value_1d_test ( )

  call p00_prob_num ( prob_num )

  do prob = 1, prob_num
    call newton_interp_1d_test01 ( prob )
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NEWTON_INTERP_1D_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine newton_coef_1d_test ( )

!*****************************************************************************80
!
!! NEWTON_COEF_1D_TEST tests NEWTON_COEF_1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nd = 5

  real ( kind = 8 ) cd(nd)
  real ( kind = 8 ) xd(nd)
  real ( kind = 8 ) yd(nd)

  xd = (/ 0.0, 1.0, 2.0, 3.0, 4.0 /)
  yd = (/ 24.0, 0.0, 0.0, 0.0, 0.0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NEWTON_COEF_1D_TEST'
  write ( *, '(a)' ) '  NEWTON_COEF_1D sets the coefficients for a 1D Newton interpolation.'

  call r8vec2_print ( nd, xd, yd, '  Interpolation data:' )

  call newton_coef_1d ( nd, xd, yd, cd )

  call r8vec_print ( nd, cd, '  Newton interpolant coefficients:' )

  return
end
subroutine newton_value_1d_test ( )

!*****************************************************************************80
!
!! NEWTON_VALUE_1D_TEST tests NEWTON_VALUE_1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nd = 5
  integer ( kind = 4 ), parameter :: ni = 16

  real ( kind = 8 ) cd(nd)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo
  real ( kind = 8 ) xd(nd)
  real ( kind = 8 ) xi(ni)
  real ( kind = 8 ) yi(ni)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NEWTON_VALUE_1D_TEST'
  write ( *, '(a)' ) '  NEWTON_VALUE_1D evaluates a Newton 1d interpolant.'

  xd = (/ 0.0, 1.0, 2.0, 3.0, 4.0 /)
  cd = (/ 24.0, -24.0, +12.0, -4.0, 1.0 /)
  call r8vec2_print ( nd, xd, cd, '  The Newton interpolant data:' )

  x_lo = 0.0
  x_hi = 5.0
  call r8vec_linspace ( ni, x_lo, x_hi, xi )

  call newton_value_1d ( nd, xd, cd, ni, xi, yi )

  call r8vec2_print ( ni, xi, yi, '  Newton interpolant values:' )

  return
end
subroutine newton_interp_1d_test01 ( prob )

!*****************************************************************************80
!
!! NEWTON_INTERP_1D_TEST01 tests NEWTON_INTERP_1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: cd(:)
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = 255 ) interp_filename
  real ( kind = 8 ) interp_error
  integer ( kind = 4 ) interp_unit
  integer ( kind = 4 ) j
  real ( kind = 8 ) ld
  real ( kind = 8 ) li
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) ni
  character ( len = 255 ) output_filename
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
  write ( *, '(a)' ) 'NEWTON_INTERP_1D_TEST01:'
  write ( *, '(a,i1)' ) '  Interpolate data from TEST_INTERP problem #', prob

  call p00_data_num ( prob, nd )
  write ( *, '(a,i3)' ) '  Number of data points = ', nd

  allocate ( xy(1:2,1:nd) )
  call p00_data ( prob, 2, nd, xy )
  
  allocate ( xd(1:nd) )
  allocate ( yd(1:nd) )
  xd(1:nd) = xy(1,1:nd)
  yd(1:nd) = xy(2,1:nd)
  deallocate ( xy )

  call r8vec2_print ( nd, xd, yd, '  X, Y data:' )
!
!  Get the Newton coefficients.
!
  allocate ( cd(1:nd) )
  call newton_coef_1d ( nd, xd, yd, cd )
!
!  #1:  Does interpolant match function at interpolation points?
!
  ni = nd
  allocate ( xi(1:ni) )
  allocate ( yi(1:ni) )
  xi(1:ni) = xd(1:ni)

  call newton_value_1d ( nd, xd, cd, ni, xi, yi )

  interp_error = r8vec_norm_affine ( ni, yi, yd ) / real ( ni, kind = 8 )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  L2 interpolation error averaged per interpolant node = ', interp_error

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
  call newton_value_1d ( nd, xd, cd, ni, xi, yi )

  ld = sum ( sqrt ( ( ( xd(2:nd) - xd(1:nd-1) ) / ( xmax - xmin ) ) ** 2 &
                  + ( ( yd(2:nd) - yd(1:nd-1) ) / ( ymax - ymin ) ) ** 2 ) )

  li = sum ( sqrt ( ( ( xi(2:ni) - xi(1:ni-1) ) / ( xmax - xmin ) ) ** 2 &
                  + ( ( yi(2:ni) - yi(1:ni-1) ) / ( ymax - ymin ) ) ** 2 ) )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Normalized length of piecewise linear interpolant = ', ld
  write ( *, '(a,g14.6)' ) '  Normalized length of Newton interpolant           = ', li

  deallocate ( xi )
  deallocate ( yi )
!
!  Create data file.
!
  write ( data_filename, '(a,i2.2,a)' ) 'data', prob, '.txt'
! call get_unit ( data_unit )
  data_unit = 99
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
  allocate ( xi(1:ni) )
  allocate ( yi(1:ni) )
  call r8vec_linspace ( ni, xmin, xmax, xi )
  call newton_value_1d ( nd, xd, cd, ni, xi, yi )

  write ( interp_filename, '(a,i2.2,a)' ) 'interp', prob, '.txt'
! call get_unit ( interp_unit )
  interp_unit = 99
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
! call get_unit ( command_unit )
  command_unit = 99
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
    'set title "Data versus Newton Polynomial Interpolant"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 with points pt 7 ps 2 lc rgb "blue",\'
  write ( command_unit, '(a)' ) '     "' // trim ( interp_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "red"'

  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created graphics command file "' // trim ( command_filename ) // '".'

  deallocate ( xd )
  deallocate ( xi )
  deallocate ( yd )
  deallocate ( yi )

  return
end

