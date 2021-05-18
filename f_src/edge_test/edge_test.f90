program main

!*****************************************************************************80
!
!! MAIN is the main program for EDGE_TEST.
!
!  Discussion:
!
!    EDGE_TEST tests the EDGE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EDGE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the EDGE library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test035 ( )
  call test036 ( )
  call test037 ( )
  call test04 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'EDGE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 plots functions of 1 variable with discontinuities.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Polynomial fitting for edge detection in irregularly sampled signals 
!    and images,
!    SIAM Journal on Numerical Analysis,
!    Volume 43, Number 1, 2006, pages 259-279.
!
  implicit none

  character ( len = 80 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 80 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ), allocatable :: f(:)
  character ( len = 80 ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) test
  integer ( kind = 4 ) test_num
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  Plot 1D test functions.'

  test_num = 7

  n = 101

  allocate ( f(1:n) )
  allocate ( x(1:n) )

  do test = 1, test_num

    if ( test == 1 ) then
      x_min = -1.0D+00
      x_max = +1.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      header = 'fx1'
      call fx1 ( n, x, f )
      title = '1D Test Function #1'
    else if ( test == 2 ) then
      x_min = -1.0D+00
      x_max = +1.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      header = 'fx2'
      call fx2 ( n, x, f )
      title = '1D Test Function #2'
    else if ( test == 3 ) then
      x_min = -1.0D+00
      x_max = +1.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      header = 'fx3'
      call fx3 ( n, x, f )
      title = '1D Test Function #3'
    else if ( test == 4 ) then
      x_min =  0.0D+00
      x_max = +1.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      header = 'fx4'
      call fx4 ( n, x, f )
      title = '1D Test Function #4'
    else if ( test == 5 ) then
      x_min = -1.0D+00
      x_max = +1.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      header = 'fx5'
      call fx5 ( n, x, f )
      title = '1D Test Function #5'
    else if ( test == 6 ) then
      x_min =  0.0D+00
      x_max = +1.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      header = 'fx6'
      call fx6 ( n, x, f )
      title = '1D Test Function #6'
    else if ( test == 7 ) then
      x_min =  0.0D+00
      x_max = +1.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      header = 'fx7'
      call fx7 ( n, x, f )
      title = '1D Test Function #7'
    end if

    call get_unit ( data_unit )
    data_filename = trim ( header ) // '_data.txt'
    open ( unit = data_unit, file = data_filename, status = 'replace' )
    do i = 1, n
      write ( data_unit, '(2x,g14.6,2x,g14.6)' ) x(i), f(i)
    end do
    close ( unit = data_unit )
    write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'

    call get_unit ( command_unit )
    command_filename = trim ( header ) // '_commands.txt'
    open ( unit = command_unit, file = command_filename, status = 'replace' )
    write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
    write ( command_unit, '(a)' ) '#'
    write ( command_unit, '(a)' ) '# Usage:'
    write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
    write ( command_unit, '(a)' ) '#'
    write ( command_unit, '(a)' ) 'set term png'
    write ( command_unit, '(a)' ) &
      'set output "' // trim ( header ) // '.png"'
    write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
    write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
    write ( command_unit, '(a)' ) 'set title "' // trim ( title ) // '"'
    write ( command_unit, '(a)' ) 'set timestamp'
    write ( command_unit, '(a)' ) 'set grid'
    write ( command_unit, '(a)' ) 'set style data lines'
    write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
      '" using 1:2 with points lt 3 pt 4 linecolor rgb "blue"'
    write ( command_unit, '(a)' ) 'quit'
    close ( unit = command_unit )
    write ( *, '(a)' ) &
      '  Created command file "' // trim ( command_filename ) // '".'

  end do

  deallocate ( f )
  deallocate ( x )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 plots a function with a jump discontinuity along a circle.
!
!  Discussion:
!
!    This is example 4.1 in the reference.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Polynomial fitting for edge detection in irregularly sampled signals 
!    and images,
!    SIAM Journal on Numerical Analysis,
!    Volume 43, Number 1, 2006, pages 259-279.
!
  implicit none

  character ( len = 80 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 80 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) fxy
  character ( len = 80 ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ), allocatable :: y(:)
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  Plot 2D test function #1 with jump along circle.'

  header = 'fxy1'
  title = '2D test function #1 with discontinuity along circle'

  n = 101
  x_min = -1.0D+00
  x_max = +1.0D+00
  allocate ( x(1:n) )
  call r8vec_linspace ( n, x_min, x_max, x )
  y_min = -1.0D+00
  y_max = +1.0D+00
  allocate ( y(1:n) )
  call r8vec_linspace ( n, y_min, y_max, y )

  call get_unit ( data_unit )
  data_filename = trim ( header ) // '_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, n
    do j = 1, n
      call fxy1 ( 1, x(i), y(j), fxy )
      write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x(i), y(j), fxy
    end do
    write ( data_unit, '(a)' ) ''
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'

  call get_unit ( command_unit )
  command_filename = trim ( header ) // '_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) &
    'set output "' // trim ( header ) // '.png"'
  write ( command_unit, '(a)' ) 'set view 120, 77'
  write ( command_unit, '(a)' ) 'set hidden3d'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
  write ( command_unit, '(a)' ) 'set title "' // trim ( title ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'splot "' // trim ( data_filename ) // &
    '" with lines'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  deallocate ( x )
  deallocate ( y )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 plots a function with a jump discontinuity along a circle.
!
!  Discussion:
!
!    This is example 4.2 in the reference.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Polynomial fitting for edge detection in irregularly sampled signals 
!    and images,
!    SIAM Journal on Numerical Analysis,
!    Volume 43, Number 1, 2006, pages 259-279.
!
  implicit none

  character ( len = 80 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 80 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) fxy
  character ( len = 80 ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ), allocatable :: y(:)
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03:'
  write ( *, '(a)' ) '  Plot 2D test function #2, the Shepp Logan phantom.'

  header = 'fxy2'
  title = '2D test function #2, the Shepp Logan phantom'

  n = 101
  x_min = -1.0D+00
  x_max = +1.0D+00
  allocate ( x(1:n) )
  call r8vec_linspace ( n, x_min, x_max, x )
  y_min = -1.0D+00
  y_max = +1.0D+00
  allocate ( y(1:n) )
  call r8vec_linspace ( n, y_min, y_max, y )

  call get_unit ( data_unit )
  data_filename = trim ( header ) // '_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, n
    do j = 1, n
      call fxy2 ( 1, x(i), y(j), fxy )
      write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x(i), y(j), fxy
    end do
    write ( data_unit, '(a)' ) ''
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'

  call get_unit ( command_unit )
  command_filename = trim ( header ) // '_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) &
    'set output "' // trim ( header ) // '.png"'
  write ( command_unit, '(a)' ) 'set view 30, 75'
  write ( command_unit, '(a)' ) 'set hidden3d'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
  write ( command_unit, '(a)' ) 'set title "' // trim ( title ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'splot "' // trim ( data_filename ) // &
    '" with lines'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  deallocate ( x )
  deallocate ( y )

  return
end
subroutine test035 ( )

!*****************************************************************************80
!
!! TEST035 plots a function with a jump discontinuity along a circle.
!
!  Discussion:
!
!    This is example 3.2 in the reference.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Determining the location of discontinuities in the derivatives
!    of functions,
!    Applied Numerical Mathematics,
!    Volume 58, 2008, pages 577-592.
!
  implicit none

  character ( len = 80 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 80 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) fxy
  character ( len = 80 ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ), allocatable :: y(:)
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST035:'
  write ( *, '(a)' ) '  Plot 2D test function #3, the modified 2D Harten function.'

  header = 'fxy3'
  title = '2D test function #3, the modified 2D Harten function'

  n = 101
  x_min = -1.0D+00
  x_max = +1.0D+00
  allocate ( x(1:n) )
  call r8vec_linspace ( n, x_min, x_max, x )
  y_min = -1.0D+00
  y_max = +1.0D+00
  allocate ( y(1:n) )
  call r8vec_linspace ( n, y_min, y_max, y )

  call get_unit ( data_unit )
  data_filename = trim ( header ) // '_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, n
    do j = 1, n
      call fxy3 ( 1, x(i), y(j), fxy )
      write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x(i), y(j), fxy
    end do
    write ( data_unit, '(a)' ) ''
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'

  call get_unit ( command_unit )
  command_filename = trim ( header ) // '_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) &
    'set output "' // trim ( header ) // '.png"'
  write ( command_unit, '(a)' ) 'set view 30, 75'
  write ( command_unit, '(a)' ) 'set hidden3d'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
  write ( command_unit, '(a)' ) 'set title "' // trim ( title ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'splot "' // trim ( data_filename ) // &
    '" with lines'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  deallocate ( x )
  deallocate ( y )

  return
end
subroutine test036 ( )

!*****************************************************************************80
!
!! TEST036 plots a function with a derivative discontinuity.
!
!  Discussion:
!
!    This is example 3.1 in the reference.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Determining the location of discontinuities in the derivatives
!    of functions,
!    Applied Numerical Mathematics,
!    Volume 58, 2008, pages 577-592.
!
  implicit none

  character ( len = 80 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 80 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) fxy
  character ( len = 80 ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ), allocatable :: y(:)
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST036:'
  write ( *, '(a)' ) '  Plot 2D test function #4, the discontinuous medium wave, P(x,t).'

  header = 'fxy4'
  title = '2D test function #4, the discontinuous medium wave, P(x,t)'

  n = 101
  x_min = -1.0D+00
  x_max = +0.0D+00
  allocate ( x(1:n) )
  call r8vec_linspace ( n, x_min, x_max, x )
  y_min = 0.0D+00
  y_max = 0.1D+00
  allocate ( y(1:n) )
  call r8vec_linspace ( n, y_min, y_max, y )

  call get_unit ( data_unit )
  data_filename = trim ( header ) // '_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, n
    do j = 1, n
      call fxy4 ( 1, x(i), y(j), fxy )
      write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x(i), y(j), fxy
    end do
    write ( data_unit, '(a)' ) ''
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'

  call get_unit ( command_unit )
  command_filename = trim ( header ) // '_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) &
    'set output "' // trim ( header ) // '.png"'
  write ( command_unit, '(a)' ) 'set view 30, 45'
  write ( command_unit, '(a)' ) 'set hidden3d'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
  write ( command_unit, '(a)' ) 'set title "' // trim ( title ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'splot "' // trim ( data_filename ) // &
    '" with lines'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  deallocate ( x )
  deallocate ( y )

  return
end
subroutine test037 ( )

!*****************************************************************************80
!
!! TEST037 plots a function with a derivative discontinuity.
!
!  Discussion:
!
!    This is example 3.1 in the reference.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Determining the location of discontinuities in the derivatives
!    of functions,
!    Applied Numerical Mathematics,
!    Volume 58, 2008, pages 577-592.
!
  implicit none

  character ( len = 80 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 80 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) fxy
  character ( len = 80 ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ), allocatable :: y(:)
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST037:'
  write ( *, '(a)' ) '  Plot 2D test function #5, discontinuous medium wave, U(x,t).'

  header = 'fxy5'
  title = '2D test function #5, the discontinuous medium wave, U(x,t)'

  n = 101
  x_min = -1.0D+00
  x_max = +0.0D+00
  allocate ( x(1:n) )
  call r8vec_linspace ( n, x_min, x_max, x )
  y_min = 0.0D+00
  y_max = 0.1D+00
  allocate ( y(1:n) )
  call r8vec_linspace ( n, y_min, y_max, y )

  call get_unit ( data_unit )
  data_filename = trim ( header ) // '_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, n
    do j = 1, n
      call fxy5 ( 1, x(i), y(j), fxy )
      write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x(i), y(j), fxy
    end do
    write ( data_unit, '(a)' ) ''
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'

  call get_unit ( command_unit )
  command_filename = trim ( header ) // '_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) &
    'set output "' // trim ( header ) // '.png"'
  write ( command_unit, '(a)' ) 'set view 30, 45'
  write ( command_unit, '(a)' ) 'set hidden3d'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
  write ( command_unit, '(a)' ) 'set title "' // trim ( title ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'splot "' // trim ( data_filename ) // &
    '" with lines'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  deallocate ( x )
  deallocate ( y )

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 plots slices of a 3D function.
!
!  Discussion:
!
!    Although the slice plots look uninteresting, there is a lot of detail
!    hidden in the data in variations that are not obvious at first.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Larry Shepp,
!    Computerized tomography and nuclear magnetic resonance,
!    Journal of Computer Assisted Tomography,
!    Volume 4, Number 1, February 1980, pages 94-107.
!
  implicit none

  character ( len = 80 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 80 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) fxyz
  character ( len = 80 ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) test
  integer ( kind = 4 ) test_num
  character ( len = 80 ) title
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min
  real ( kind = 8 ) x_val
  real ( kind = 8 ), allocatable :: y(:)
  real ( kind = 8 ) y_max
  real ( kind = 8 ) y_min
  real ( kind = 8 ) y_val
  real ( kind = 8 ), allocatable :: z(:)
  real ( kind = 8 ) z_max
  real ( kind = 8 ) z_min
  real ( kind = 8 ) z_val

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST04:'
  write ( *, '(a)' ) '  Plot 3D test function #1, the Shepp Logan 3D phantom.'

  test_num = 3

  n = 101
  x_min = -1.5D+00
  x_max = +1.5D+00
  allocate ( x(1:n) )
  call r8vec_linspace ( n, x_min, x_max, x )
  y_min = -1.5D+00
  y_max = +1.5D+00
  allocate ( y(1:n) )
  call r8vec_linspace ( n, y_min, y_max, y )
  z_min = -1.5D+00
  z_max = +1.5D+00
  allocate ( z(1:n) )
  call r8vec_linspace ( n, z_min, z_max, z )

  do test = 1, test_num

    if ( test == 1 ) then
      x_val = 0.0D+00
      title = 'Slice X = 0.0'
      header = 'fxyz1_x'
    else if ( test == 2 ) then
      y_val = 0.0D+00
      title = 'Slice Y = 0.0'
      header = 'fxyz1_y'
    else if ( test == 3 ) then
      z_val = - 0.1D+00
      title = 'Slice Z = - 0.1'
      header = 'fxyz1_z'
    end if

    call get_unit ( data_unit )
    data_filename = trim ( header ) // '_data.txt'
    open ( unit = data_unit, file = data_filename, status = 'replace' )
    do i = 1, n
      do j = 1, n
        if ( test == 1 ) then
          call fxyz1 ( 1, x_val, y(j), z(i), fxyz )
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) y(j), z(i), fxyz
        else if ( test == 2 ) then
          call fxyz1 ( 1, x(j), y_val, z(i), fxyz )
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x(j), z(i), fxyz
        else if ( test == 3 ) then
          call fxyz1 ( 1, x(j), y(i), z_val, fxyz )
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x(j), y(i), fxyz
        end if

      end do
      write ( data_unit, '(a)' ) ''
    end do
    close ( unit = data_unit )
    write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'

    call get_unit ( command_unit )
    command_filename = trim ( header ) // '_commands.txt'
    open ( unit = command_unit, file = command_filename, status = 'replace' )
    write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
    write ( command_unit, '(a)' ) '#'
    write ( command_unit, '(a)' ) '# Usage:'
    write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
    write ( command_unit, '(a)' ) '#'
    write ( command_unit, '(a)' ) 'set term png'
    write ( command_unit, '(a)' ) &
      'set output "' // trim ( header ) // '.png"'
    write ( command_unit, '(a)' ) 'set view 20, 75'
    write ( command_unit, '(a)' ) 'set hidden3d'
    write ( command_unit, '(a)' ) 'set timestamp'
    if ( test == 1 ) then
      write ( command_unit, '(a)' ) 'set xlabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Z --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- X --->"'
    else if ( test == 2 ) then
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Z --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Y --->"'
    else if ( test == 3 ) then
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
    end if
    write ( command_unit, '(a)' ) 'set title "' // trim ( title ) // '"'
    write ( command_unit, '(a)' ) 'set grid'
    write ( command_unit, '(a)' ) 'set style data lines'
    write ( command_unit, '(a)' ) 'splot "' // trim ( data_filename ) // &
      '" with lines'
    write ( command_unit, '(a)' ) 'quit'
    close ( unit = command_unit )
    write ( *, '(a)' ) &
      '  Created command file "' // trim ( command_filename ) // '".'

  end do

  deallocate ( x )
  deallocate ( y )
  deallocate ( z )

  return
end
