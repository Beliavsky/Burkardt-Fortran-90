program main

!*****************************************************************************80
!
!! MAIN is the main program for FD2D_HEAT_STEADY_TEST.
!
!  Discussion:
!
!    FD2D_HEAT_STEADY_TEST tests the FD2D_HEAT_STEADY library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FD2D_HEAT_STEADY_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the FD2D_HEAT_STEADY library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'FD2D_HEAT_STEADY_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 computes the solution for a steady state heat equation problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 80 ) command_filename
  integer ( kind = 4 ) command_unit
  real ( kind = 8 ), external :: d
  character ( len = 80 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real ( kind = 8 ), allocatable :: umat(:,:)
  real ( kind = 8 ) u_mean
  real ( kind = 8 ), allocatable :: xmat(:,:)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ), allocatable :: xvec(:)
  real ( kind = 8 ), allocatable :: ymat(:,:)
  real ( kind = 8 ) ymax
  real ( kind = 8 ) ymin
  real ( kind = 8 ), allocatable :: yvec(:)
!
!  Specify the spatial grid.
!
  nx = 41
  allocate ( xvec(1:nx) )
  call r8vec_linspace ( nx, 0.0D+00, 2.0D+00, xvec )

  ny = 21
  allocate ( yvec(1:ny) )
  call r8vec_linspace ( ny, 0.0D+00, 1.0D+00, yvec )

  allocate ( xmat(1:nx,1:ny) )
  allocate ( ymat(1:nx,1:ny) )
  call r8vec_mesh_2d ( nx, ny, xvec, yvec, xmat, ymat )
!
!  Solve the finite difference approximation to the steady 2D heat equation.
!
  allocate ( umat(1:nx,1:ny) )
  call fd2d_heat_steady ( nx, ny, xvec, yvec, d, f, umat )
!
!  Create a data file.
!
  data_filename = 'test01_data.txt'
  call get_unit ( data_unit )
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do j = 1, ny
    do i = 1, nx
      write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) xmat(i,j), ymat(i,j), umat(i,j)
    end do
    write ( data_unit, '(a)' ) ''
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Created graphics data file "' // trim ( data_filename ) // '".'
!
!  Create the command file.
!
  command_filename = 'test01_commands.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "test01.png"'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
  write ( command_unit, '(a)' ) 'set zlabel "<---U(X,Y)--->"'
  write ( command_unit, '(a)' ) &
    'set title "Sample Solution"'
  write ( command_unit, '(a)' ) 'set contour'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'set cntrparam levels 10'
  write ( command_unit, '(a)' ) 'set view 75, 75'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'splot "' // trim ( data_filename ) // '"'

  close ( unit = command_unit )

  write ( *, '(a)' ) &
    '  Created graphics command file "' // trim ( command_filename ) // '".'
!
!  Report the average value of U.
!
  u_mean = sum ( umat(1:nx,1:ny) ) / real ( nx * ny, kind = 8 )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Mean value of U is ', u_mean
!
!  Free memory.
!
  deallocate ( umat )
  deallocate ( xmat )
  deallocate ( xvec )
  deallocate ( ymat )
  deallocate ( yvec )

  return
end
function d ( x, y )

!*****************************************************************************80
!
!! D evaluates the heat conductivity coefficient.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 July 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, Y, the evaluation point.
!
!    Output, real ( kind = 8 ) D, the value of the heat conductivity at (X,Y).
!
  implicit none

  real ( kind = 8 ) d
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  d = 1.0D+00

  return
end
function f ( x, y )

!*****************************************************************************80
!
!! F evaluates the heat source term.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 July 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, Y, the evaluation point.
!
!    Output, real ( kind = 8 ) F, the value of the heat source term at (X,Y).
!
  implicit none

  real ( kind = 8 ) f
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  f = 0.0D+00

  return
end
subroutine boundary ( nx, ny, x, y, n, a, rhs )

!*****************************************************************************80
!
!! BOUNDARY sets up the matrix and right hand side at boundary nodes.
!
!  Discussion:
!
!    For this simple problem, the boundary conditions specify that the solution
!    is 100 on the left side, and insulated on the right, top and bottom.
!
!    Nodes are assigned a single index K, which increases as:
!
!    (NY-1)*NX+1  (NY-1)*NX+2  ...  NY * NX
!           ....         ....  ...    .....
!           NX+1         NX+2  ...   2 * NX
!              1            2  ...       NX
!
!    The index K of a node on the lower boundary satisfies:
!      1 <= K <= NX
!    The index K of a node on the upper boundary satisfies:
!      (NY-1)*NX+1 <= K <= NY * NX
!    The index K of a node on the left boundary satisfies:
!      mod ( K, NX ) = 1
!    The index K of a node on the right boundary satisfies:
!      mod ( K, NX ) = 0
!
!    If we number rows from bottom I = 1 to top I = NY
!    and columns from left J = 1 to right J = NX, then the relationship
!    between the single index K and the row and column indices I and J is:
!      K = ( I - 1 ) * NX + J
!    and
!      J = 1 + mod ( K - 1, NX )
!      I = 1 + ( K - J ) / NX
!      
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, NY, the number of grid points in X and Y.
!
!    Input, real ( kind = 8 ) X(NX), Y(NY), the coordinates of grid lines.
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Input/output, real ( kind = 8 ) A(N,N).  On input, the system matrix, with the 
!    entries for the interior nodes filled in.  On output, the entries for
!    the boundary nodes have been set as well.
!
!    Input, real ( kind = 8 ) RHS(N), on input, the system right hand side, 
!    with the entries for the interior nodes filled in.  On output, the entries for
!    the boundary nodes have been set as well.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) kc
  real ( kind = 8 ) rhs(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Left boundary.
!
  j = 1
  do i = 2, ny - 1
    kc = ( i - 1 ) * nx + j
    a(kc,kc) = a(kc,kc) + 1.0D+00
    rhs(kc) = 10.0D+00
  end do
!
!  Right boundary.
!
  j = nx
  do i = 2, ny - 1
    kc = ( i - 1 ) * nx + j
    a(kc,kc) = a(kc,kc) + 1.0D+00
    rhs(kc) = 100.0D+00
  end do
!
!  Lower boundary.
!
  i = 1
  do j = 1, nx
    kc = ( i - 1 ) * nx + j
    a(kc,kc) = a(kc,kc) + 1.0D+00
    rhs(kc) = 0.0D+00
  end do
!
!  Upper boundary.
!
  i = ny
  do j = 1, nx
    kc = ( i - 1 ) * nx + j
    a(kc,kc) = a(kc,kc) + 1.0D+00
    rhs(kc) = 0.0D+00
  end do

  return
end

