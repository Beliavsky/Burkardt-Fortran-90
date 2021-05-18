program main

!*****************************************************************************80
!
!! MAIN is the main program for SVD_SNOWALL_TEST.
!
!  Discussion:
!
!    SVD_SNOWFALL_TEST tests the SVD_SNOWFALL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 255 ) filename
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: x(:,:)

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SVD_SNOWFALL_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SVD_SNOWFALL library.'
!
!  Retrieve the data.
!  It's really easier to do this in the main program.
!
  filename = 'snowfall.txt'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SVD_SNOWFALL_TEST01'
  write ( *, '(a)' ) '  Read, process, and return snowfall data in "' // &
    trim ( filename ) // '".'
!
!  Determine the size of the data.
!
  call r8mat_header_read ( filename, m, n )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of data rows    M = ', m
  write ( *, '(a,i4)' ) '  Number of data columns N = ', n

  allocate ( x(1:m,1:n) )

  call r8mat_data_read ( filename, m, n, x )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Data has been read from the file.'

  call svd_snowfall_test02 ( m, n, x )
  call svd_snowfall_test03 ( m, n, x )
  call svd_snowfall_test04 ( m, n, x )
  call svd_snowfall_test05 ( m, n, x )
!
!  Free memory.
!
  deallocate ( x )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SVD_SNOWFALL_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine svd_snowfall_test02 ( m, n, x )

!*****************************************************************************80
!
!! SVD_SNOWFALL_TEST02 looks at the singular values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) X(M,N), the snowfall data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ), allocatable :: e(:)
  real ( kind = 8 ), allocatable :: e_cum(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) mn
  real ( kind = 8 ) s(m,n)
  real ( kind = 8 ), allocatable :: s_diag(:)
  real ( kind = 8 ) u(m,m)
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) x(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SVD_SNOWFALL_TEST02'
  write ( *, '(a)' ) '  Look at the singular values.'
  write ( *, '(a)' ) '  If the singular values are close, then the data is'
  write ( *, '(a)' ) '  well spread out.  If the singular values decay rapidly,'
  write ( *, '(a)' ) '  then the data exhibits patterns, or is constrained to'
  write ( *, '(a)' ) '  a lower-dimensional subspace.'
!
!  Compute the SVD.
!
  call r8mat_svd_linpack ( m, n, x, u, s, v )
!
!  Extract the diagonal of S.
!
  mn = min ( m, n )
  allocate ( s_diag(1:mn) )
  do i = 1, mn
    s_diag(i) = s(i,i)
  end do
!
!  Print the singular values.
!
  call r8vec_print ( mn, s_diag, '  The singular values:' )
!
!  Plot the singular values.
!
  call get_unit ( data_unit )
  data_filename = 'singular_values_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, mn
    write ( data_unit, '(2x,i4,2x,g14.6)' ) i, s_diag(i)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'

  call get_unit ( command_unit )
  command_filename = 'singular_values_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "singular_values.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Index I"'
  write ( command_unit, '(a)' ) 'set ylabel "S(I)"'
  write ( command_unit, '(a)' ) 'set title "Snowfall Singular Values"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "blue"'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'
!
!  Print the cumulative "energy" of the singular values.
!
  allocate ( e(1:mn) )
  allocate ( e_cum(1:mn+1) )

  do i = 1, mn
    e(i) = s_diag(i) ** 2
  end do
  e(1:mn) = e(1:mn) / sum ( e(1:mn) )
  call r8vec_cum0 ( mn, e, e_cum )

  call r8vec_print ( mn + 1, e_cum, '  The cumulative energy:' );

  deallocate ( e )
  deallocate ( e_cum )
  deallocate ( s_diag )

  return
end
subroutine svd_snowfall_test03 ( m, n, x )

!*****************************************************************************80
!
!! SVD_SNOWFALL_TEST03 computes low rank approximations to the matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) X(M,N), the snowfall data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(m,n)
  real ( kind = 8 ) a2(m,n)
  real ( kind = 8 ) a3(m,n)
  real ( kind = 8 ) a4(m,n)
  real ( kind = 8 ) a5(m,n)
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  integer ( kind = 4 ) i
  real ( kind = 8 ) s(m,n)
  real ( kind = 8 ) u(m,m)
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) x(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SVD_SNOWFALL_TEST03';
  write ( *, '(a)' ) '  Compute the rank 1 through rank 5 approximations to the data.'
  write ( *, '(a)' ) '  Compare each of these to the 2012 snowfall data.'
!
!  Compute the SVD.
!
  call r8mat_svd_linpack ( m, n, x, u, s, v )
!
!  Form the rank 1, 2, 3, 4, 5 approximants to A.
!
  call r8mat_svd_low_rank ( m, n, 1, u, s, v, a1 )
  call r8mat_svd_low_rank ( m, n, 2, u, s, v, a2 )
  call r8mat_svd_low_rank ( m, n, 3, u, s, v, a3 )
  call r8mat_svd_low_rank ( m, n, 4, u, s, v, a4 )
  call r8mat_svd_low_rank ( m, n, 5, u, s, v, a5 )
!
!  Column 1 of X is the 2012 snowfall.
!  Column 1 of A1 is the rank 1 approximant to 2012 snowfall.
!
  call get_unit ( data_unit )
  data_filename = 'approx_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, m
    write ( data_unit, '(2x,i4,6(2x,g14.6))' ) &
      i, x(i,1), a1(i,1), a2(i,1), a3(i,1), a4(i,1), a5(i,1)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'

  call get_unit ( command_unit )
  command_filename = 'approx_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "approx0.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Month"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "2012 Snowfall"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "blue"'

  write ( command_unit, '(a)' ) 'set output "approx1.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Month"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Rank 1 Approx to 2012 Snowfall"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "blue",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:3 lw 3 linecolor rgb "red"'

  write ( command_unit, '(a)' ) 'set output "approx2.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Month"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Rank 2 Approx to 2012 Snowfall"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "blue",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:3 lw 3 linecolor rgb "gray",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:4 lw 3 linecolor rgb "red"'

  write ( command_unit, '(a)' ) 'set output "approx3.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Month"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Rank 3 Approx to 2012 Snowfall"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "blue",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:3 lw 3 linecolor rgb "gray",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:4 lw 3 linecolor rgb "gray",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:5 lw 3 linecolor rgb "red"'

  write ( command_unit, '(a)' ) 'set output "approx4.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Month"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Rank 4 Approx to 2012 Snowfall"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "blue",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:3 lw 3 linecolor rgb "gray",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:4 lw 3 linecolor rgb "gray",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:5 lw 3 linecolor rgb "gray",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:6 lw 3 linecolor rgb "red"'

  write ( command_unit, '(a)' ) 'set output "approx5.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Month"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Rank 5 Approx to 2012 Snowfall"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "blue",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:3 lw 3 linecolor rgb "gray",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:4 lw 3 linecolor rgb "gray",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:5 lw 3 linecolor rgb "gray",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:6 lw 3 linecolor rgb "gray",\'
  write ( command_unit, '(a)' ) '     "' // trim ( data_filename ) // &
    '" using 1:7 lw 3 linecolor rgb "red"'

  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  return
end
subroutine svd_snowfall_test04 ( m, n, x )

!*****************************************************************************80
!
!! SVD_SNOWFALL_TEST04 looks at the first 6 modes in the U matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) X(M,N), the snowfall data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  integer ( kind = 4 ) i
  real ( kind = 8 ) s(m,n)
  real ( kind = 8 ) u(m,m)
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) x(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SVD_SNOWFALL_TEST04'
  write ( *, '(a)' ) '  Look at the first 6 modes in the U matrix.'
  write ( *, '(a)' ) '  Each of these represents a pattern for snowfall over a year.'
  write ( *, '(a)' ) '  The first mode is the pattern that is strongest in the data.'
!
!  Compute the SVD.
!
  call r8mat_svd_linpack ( m, n, x, u, s, v )
!
!  Normalize the patterns so that each column has maximum entry 1.
!
  call r8col_normalize_li ( m, m, u )
!
!  Plot the U modes.
!
  call get_unit ( data_unit )
  data_filename = 'umode_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, m
    write ( data_unit, '(2x,i4,6(2x,g14.6))' ) i, u(i,1:6)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'

  call get_unit ( command_unit )
  command_filename = 'umode_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "umode1.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Month"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Monthly Snowfall Mode 1"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "blue"'

  write ( command_unit, '(a)' ) 'set output "umode2.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Month"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Monthly Snowfall Mode 2"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:3 lw 3 linecolor rgb "blue"'

  write ( command_unit, '(a)' ) 'set output "umode3.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Month"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Monthly Snowfall Mode 3"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:4 lw 3 linecolor rgb "blue"'

  write ( command_unit, '(a)' ) 'set output "umode4.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Month"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Monthly Snowfall Mode 4"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:5 lw 3 linecolor rgb "blue"'

  write ( command_unit, '(a)' ) 'set output "umode5.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Month"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Monthly Snowfall Mode 5"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:6 lw 3 linecolor rgb "blue"'

  write ( command_unit, '(a)' ) 'set output "umode6.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Month"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Monthly Snowfall Mode 6"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:7 lw 3 linecolor rgb "blue"'

  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  return
end
subroutine svd_snowfall_test05 ( m, n, x )

!*****************************************************************************80
!
!! SVD_SNOWFALL_TEST05 looks at the first 6 modes in the V matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) X(M,N), the snowfall data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  integer ( kind = 4 ) i
  real ( kind = 8 ) s(m,n)
  real ( kind = 8 ) u(m,m)
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) x(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SVD_SNOWFALL_TEST05'
  write ( *, '(a)' ) '  Look at the first 6 modes in the V matrix.'
  write ( *, '(a)' ) '  Each of these represents a pattern shared by all the months,'
  write ( *, '(a)' ) '  and extending across the 123 sampling years.'
!
!  Compute the SVD.
!
  call r8mat_svd_linpack ( m, n, x, u, s, v )
!
!  Normalize the patterns so that each column has maximum entry 1.
!
  call r8col_normalize_li ( n, n, v )
!
!  Reverse the row ordering.
!
  call r8row_reverse ( n, n, v )
!
!  Plot the V modes.
!
  call get_unit ( data_unit )
  data_filename = 'vmode_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, n
    write ( data_unit, '(2x,i4,6(2x,g14.6))' ) i, v(i,1:6)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'

  call get_unit ( command_unit )
  command_filename = 'vmode_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "vmode1.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Year"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Yearly Snowfall Mode 1"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data points'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 with points lt 3 pt 3'

  write ( command_unit, '(a)' ) 'set output "vmode2.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Year"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Yearly Snowfall Mode 2"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data points'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:3 with points lt 3 pt 3'

  write ( command_unit, '(a)' ) 'set output "vmode3.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Year"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Yearly Snowfall Mode 3"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data points'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:4 with points lt 3 pt 3'

  write ( command_unit, '(a)' ) 'set output "vmode4.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Year"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Yearly Snowfall Mode 4"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data points'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:5 with points lt 3 pt 3'

  write ( command_unit, '(a)' ) 'set output "vmode5.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Year"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Yearly Snowfall Mode 5"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data points'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:6 with points lt 3 pt 3'

  write ( command_unit, '(a)' ) 'set output "vmode6.png"'
  write ( command_unit, '(a)' ) 'set xlabel "Year"'
  write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
  write ( command_unit, '(a)' ) 'set title "Yearly Snowfall Mode 6"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data points'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:7 with points lt 3 pt 3'

  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  return
end
