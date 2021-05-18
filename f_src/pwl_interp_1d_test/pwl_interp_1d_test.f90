program main

!*****************************************************************************80
!
!! MAIN is the main program for PWL_INTERP_1D_TEST.
!
!  Discussion:
!
!    PWL_INTERP_1D_TEST tests the PWL_INTERP_1D library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 September 2012
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
  write ( *, '(a)' ) 'PWL_INTERP_1D_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the PWL_INTERP_1D library.'
  write ( *, '(a)' ) '  The R8LIB and TEST_INTERP libraries are needed.'

  call pwl_basis_1d_test ( )

  call pwl_value_1d_test ( )

  call p00_prob_num ( prob_num )
  do prob = 1, prob_num
    call pwl_interp_1d_test01 ( prob )
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PWL_INTERP_1D_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
subroutine pwl_basis_1d_test ( )

!*****************************************************************************80
!
!! PWL_BASIS_1D_TEST tests PWL_BASIS_1D.
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
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ), dimension(nd) :: xd = (/ &
    0.0D+00, 2.0D+00, 5.0D+00, 10.0D+00 /)
  real ( kind = 8 ) xi(ni)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PWL_BASIS_1D_TEST:'
  write ( *, '(a)' ) '  PWL_BASIS_1D evaluates the piecewise linear 1D basis'
  write ( *, '(a)' ) '  functions.'

  x_min = 0.0D+00
  x_max = 10.0D+00
  call r8vec_linspace ( ni, x_min, x_max, xi )

  call pwl_basis_1d ( nd, xd, ni, xi, lb )

  call r8mat_print ( ni, nd, lb, '  The piecewise linear basis functions:' )

  return
end
subroutine pwl_value_1d_test ( )

!*****************************************************************************80
!
!! PWL_VALUE_1D_TEST tests PWL_VALUE_1D.
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
!    30 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nd = 4
  integer ( kind = 4 ), parameter :: ni = 21

  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ), dimension(nd) :: xd = (/ &
    0.0D+00, 2.0D+00, 5.0D+00, 10.0D+00 /)
  real ( kind = 8 ), dimension(nd) :: yd = (/ &
    -28.0D+00, +10.0D+00, -8.0D+00, +162.0D+00 /)
  real ( kind = 8 ) xi(ni)
  real ( kind = 8 ) yi(ni)
 
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PWL_VALUE_1D_TEST:'
  write ( *, '(a)' ) '  PWL_VALUE_1D evaluates a piecewise linear 1D interpolant.'

  x_min = 0.0D+00
  x_max = 10.0D+00
  call r8vec_linspace ( ni, x_min, x_max, xi )

  call pwl_value_1d ( nd, xd, yd, ni, xi, yi )

  call r8vec2_print ( ni, xi, yi, '  Table of interpolant values:' )

  return
end
subroutine pwl_interp_1d_test01 ( prob )

!*****************************************************************************80
!
!! PWL_INTERP_1D_TEST01 tests PWL_INTERP_1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 May 2013
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

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) interp_error
  character ( len = 255 ) interp_filename
  integer ( kind = 4 ) interp_unit
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) ni
  character ( len = 255 ) output_filename
  integer ( kind = 4 ) prob
  real ( kind = 8 ) r8vec_max
  real ( kind = 8 ) r8vec_min
  real ( kind = 8 ) r8vec_norm_affine
  real ( kind = 8 ), allocatable :: xd(:)
  real ( kind = 8 ), allocatable :: xi(:)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ), allocatable :: xy(:,:)
  real ( kind = 8 ), allocatable :: yd(:)
  real ( kind = 8 ), allocatable :: yi(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'PWL_INTERP_1D_TEST01:'
  write ( *, '(a)' ) '  PWL_INTERP_1D evaluates the piecewise linear interpolant.'
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
!  #1:  Does interpolant match function at interpolation points?
!
  ni = nd

  allocate ( xi(1:ni) )
  allocate ( yi(1:ni) )

  xi(1:ni) = xd(1:nd)
  call pwl_value_1d ( nd, xd, yd, ni, xi, yi )

  interp_error = r8vec_norm_affine ( ni, yi, yd ) / real ( ni, kind = 8 )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) &
    '  L2 interpolation error averaged per interpolant node = ', interp_error
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
  deallocate ( xi )
  deallocate ( yi )

  ni = 501
  xmin = r8vec_min ( nd, xd )
  xmax = r8vec_max ( nd, xd )
  allocate ( xi(1:ni) )
  allocate ( yi(1:ni) )
  call r8vec_linspace ( ni, xmin, xmax, xi )
  call pwl_value_1d ( nd, xd, yd, ni, xi, yi )

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
    'set title "Data versus Piecewise Linear Interpolant"'
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
  deallocate ( xy )
  deallocate ( yd )
  deallocate ( yi )

  return
end
