program main

!*****************************************************************************80
!
!! etdrk4_test tests etdrk4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'etdrk4_test'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test etdrk4.'

! call allencahn_etdrk4_test ( )
! call allencahn_euler_test ( )
  call burgers_etdrk4_test ( )
  call cheb_test ( )
  call kdv_etdrk4_test ( )
  call kdv_ift_test ( )
! call kursiv_etdrk4_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'etdrk4_test'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop ( 0 )
end
subroutine burgers_etdrk4_test ( )

!*****************************************************************************80
!
!! burgers_etdrk4_test tests burgers_etdrk4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nt = 26
  integer ( kind = 4 ), parameter :: nx = 256

  character ( len = * ), parameter :: header = 'burgers_etdrk4'
  character ( len = * ), parameter :: title = 'Burgers equation solved by ETDRK4'
  real ( kind = 8 ) tt(nt)
  real ( kind = 8 ) uu(nx,nt)
  real ( kind = 8 ) vis
  real ( kind = 8 ) xx(nx)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'burgers_etdrk4_test'
  write ( *, '(a)' ) '  burgers_etdrk4() solves the Burgers equation'
  write ( *, '(a)' ) '  using the ETD RK4 method.'

  vis = 0.03
  call burgers_etdrk4 ( nx, nt, vis, xx, tt, uu )
!
!  Create an image using gnuplot.
!
  call gnuplot_surface ( nx, nt, xx, tt, uu, header, title )

  return
end
subroutine cheb_test ( )

!*****************************************************************************80
!
!! cheb_test tests cheb.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) d(n+1,n+1)
  real ( kind = 8 ) x(n+1)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'cheb_test:'
  write ( *, '(a)' ) '  Compute the Chebyshev differentiation matrix'
  write ( *, '(a)' ) '  and grid.'

  call cheb ( n, d, x )

  call r8vec_print ( n + 1, x, '  Chebyshev points:' )

  call r8mat_print ( n+1, n+1, d, '  Chebyshev differentiation matrix.' )

  return
end
subroutine kdv_etdrk4_test ( )

!*****************************************************************************80
!
!! kdv_etdrk4_test tests kdv_etdrk4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nt = 26
  integer ( kind = 4 ), parameter :: nx = 256

  character ( len = * ), parameter :: header = 'kdv_etdrk4'
  character ( len = * ), parameter :: title = 'Korteweg-DeVries solved by ETDRK4'
  real ( kind = 8 ) tt(nt)
  real ( kind = 8 ) uu(nx,nt)
  real ( kind = 8 ) xx(nx)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'kdv_etdrk4_test'
  write ( *, '(a)' ) '  kdv_etdrk4() solves the Korteweg-deVries equation'
  write ( *, '(a)' ) '  using the ETD RK4 method.'

  call kdv_etdrk4 ( nx, nt, xx, tt, uu )
!
!  Create an image using gnuplot.
!
  call gnuplot_surface ( nx, nt, xx, tt, uu, header, title )

  return
end
subroutine kdv_ift_test ( )

!*****************************************************************************80
!
!! kdv_ift_test tests kdv_ift.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nt = 26
  integer ( kind = 4 ), parameter :: nx = 256

  character ( len = * ), parameter :: header = 'kdv_ift'
  character ( len = * ), parameter :: title = 'Korteweg-DeVries solved by IFT'
  real ( kind = 8 ) tt(nt)
  real ( kind = 8 ) uu(nx,nt)
  real ( kind = 8 ) xx(nx)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'kdv_ift_test'
  write ( *, '(a)' ) '  kdv_ift() solves the Korteweg-deVries equation'
  write ( *, '(a)' ) '  using the FFT method with an integrating factor.'

  call kdv_ift ( nx, nt, xx, tt, uu )
!
!  Create an image using gnuplot.
!
  call gnuplot_surface ( nx, nt, xx, tt, uu, header, title )

  return
end
subroutine gnuplot_surface ( m, n, x, y, z, header, title )

!*****************************************************************************80
!
!! gnuplot_surface draws a surface from a table Z(X,Y).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2020
!
!  Author:
!
!    John Burkardt
!
  implicit none
  
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = * ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = * ) title
  real ( kind = 8 ) x(m)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'gnuplot_surface:'
  write ( *, '(a)' ) '  FORTRAN90 version'
!
!  Create the data file.
!
  call get_unit ( data_unit )

  data_filename = header // '_data.txt'

  open ( unit = data_unit, file = data_filename, status = 'replace' )

  do j = 1, n
    if ( 1 < j ) then
      write ( data_unit, '(a)' )
    end if
    do i = 1, m
      write ( data_unit, '(3(2x,g14.6))' ) x(i), y(j), z(i,j)
    end do
  end do

  close ( unit = data_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  gnuplot_surface: data stored in "' &
    // trim ( data_filename ) // '".'
!
!  Create the command file.
!
  call get_unit ( command_unit )

  command_filename = trim ( header ) // '_commands.txt'

  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( header ) // '.png"'
  write ( command_unit, '(a)' ) 'set xlabel "X"'
  write ( command_unit, '(a)' ) 'set ylabel "Y"'
  write ( command_unit, '(a)' ) 'set zlabel "Z"'
  write ( command_unit, '(a)' ) 'set title "' // trim ( title ) // '"'
  write ( command_unit, '(a)' ) 'set view 10, 30'
  write ( command_unit, '(a)' ) 'set hidden3d'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'splot "' // trim ( data_filename ) // &
    '" with lines'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  gnuplot_surface: plot commands stored in "' &
    // trim ( command_filename ) // '".'

  return
end

