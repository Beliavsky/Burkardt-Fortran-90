program main

!*****************************************************************************80
!
!! MAIN is the main program for CLENSHAW_CURTIS_GRID_TEST.
!
!  Discussion:
!
!    CLENSHAW_CURTIS_GRID_TEST tests CLENSHAW_CURTIS_GRID.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CLENSHAW_CURTIS_GRID_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the CLENSHAW_CURTIS_GRID library.'

  call test005 ( )
  call test01 ( )
  call test015 ( )
  call test02 ( )
  call test025 ( )
  call test03 ( )
  call test035 ( )
  call test04 ( )
  call test045 ( )
  call test05 ( )
  call test06 ( )
  call test07 ( )
  call test08 ( )
  call test09 ( )
  call test10 ( )
  call test11 ( )
  call test12 ( )
  call test13 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CLENSHAW_CURTIS_GRID_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test005 ( )

!*****************************************************************************80
!
!! TEST005 calls CC_GRID for the 1D problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 1

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: grid_point
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( dim_num ) :: order_1d
  integer ( kind = 4 ) order_nd

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST005:'
  write ( *, '(a)' ) '  CC_GRID returns a grid of Clenshaw-Curtis points.'
  write ( *, '(a)' ) '  Here, we simply call for grids in the 1D case'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension of grid = ', dim_num

  do i = 1, 10

    write ( *, '(a)' ) ' '

    order_1d(1) = i
    order_nd = product ( order_1d(1:dim_num) )

    allocate ( grid_point(dim_num,order_nd) )

    call cc_grid ( dim_num, order_1d, order_nd, grid_point )

    do j = 1, order_nd
      write ( *, '(2x,i8,2x,f14.6)' ) j, grid_point(1:dim_num,j)
    end do

    deallocate ( grid_point )

  end do

  return
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 simply calls CC_GRID once.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real    ( kind = 8 ), allocatable, dimension ( :, : ) :: grid_point
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( dim_num ) :: order_1d  = (/ 3, 4, 2 /)
  integer ( kind = 4 ) order_nd
  integer ( kind = 4 ) q

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  CC_GRID returns a grid of Clenshaw-Curtis points.'
  write ( *, '(a)' ) '  Here, we simply call for a specific grid.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension of grid = ', dim_num

  order_nd = product ( order_1d(1:dim_num) )

  allocate ( grid_point(dim_num,order_nd) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Total number of points in the grid = ', order_nd
  write ( *, '(a)' ) ' '

  call cc_grid ( dim_num, order_1d, order_nd, grid_point )

  j = 1
  q = sum ( order_1d(1:dim_num) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         I         Q          Grid orders:'
  write ( *, '(a)' ) ' '

  write ( *, '(2x,i8,2x,i8,4x,i8,2x,i8,2x,i8)' ) j, q, order_1d(1:dim_num)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Grid points:'
  write ( *, '(a)' ) ' '

  do j = 1, order_nd
    write ( *, '(2x,i8,2x,3f14.6)' ) j, grid_point(1:dim_num,j)
  end do

  deallocate ( grid_point )

  return
end
subroutine test015 ( )

!*****************************************************************************80
!
!! TEST015 calls CC_GRID_INDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: grid_index
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( dim_num ) :: order_1d  = (/ 3, 4, 2 /)
  integer ( kind = 4 ) order_nd
  integer ( kind = 4 ) q

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST015:'
  write ( *, '(a)' ) &
    '  CC_GRID_INDEX returns an indexed grid of Clenshaw-Curtis points.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension of grid = ', dim_num

  order_nd = product ( order_1d(1:dim_num) )

  allocate ( grid_index(dim_num,order_nd) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Total number of points in the grid = ', order_nd
  write ( *, '(a)' ) ' '

  call cc_grid_index ( dim_num, order_1d, order_nd, grid_index )

  j = 1
  q = sum ( order_1d(1:dim_num) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         I         Q          Grid orders:'
  write ( *, '(a)' ) ' '

  write ( *, '(2x,i8,2x,i8,4x,i4,2x,i4,2x,i4)' ) j, q, order_1d(1:dim_num)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Grid indexed points:'
  write ( *, '(a)' ) ' '

  do j = 1, order_nd
    write ( *, '(2x,i8,2x,8x,4x,i4,2x,i4,2x,i4)' ) j, grid_index(1:dim_num,j)
  end do

  deallocate ( grid_index )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 calls CC_GRIDS_MINMAX for all points on 2D grids for Q = 3 to 5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  integer ( kind = 4 ) grid_num
  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: grid_order
  real    ( kind = 8 ), allocatable, dimension ( :, : ) :: grid_point
  integer ( kind = 4 ) j
  integer ( kind = 4 ) point_num
  integer ( kind = 4 ) q
  integer ( kind = 4 ), parameter :: q_max = 5
  integer ( kind = 4 ), parameter :: q_min = 3

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  CC_GRIDS_MINMAX returns all Clenshaw Curtis grids'
  write ( *, '(a)' ) '  whose Q value satisfies Q_MIN <= Q <= Q_MAX.'
  write ( *, '(a)' ) '  Here, Q is the sum of the orders of the 1D rules, and'
  write ( *, '(a,i8)' ) '  Q_MIN = ', q_min
  write ( *, '(a,i8)' ) '  Q_MAX = ', q_max
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension of grids = ', dim_num

  call cc_grids_minmax_size ( dim_num, q_min, q_max, grid_num, &
    point_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of grids = ', grid_num
  write ( *, '(a,i8)' ) '  Number of points in the grids = ', point_num
!
!  Allocate the space.
!
  allocate ( grid_order(1:dim_num,1:grid_num) )
  allocate ( grid_point(1:dim_num,1:point_num) )
!
!  Compute the orders and points.
!
  call cc_grids_minmax ( dim_num, q_min, q_max, grid_num, point_num, &
    grid_order, grid_point )
!
!  Now we're done.  Print the merged grid data.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         I         Q          Grid orders:'
  write ( *, '(a)' ) ' '
  do j = 1, grid_num
    q = sum ( grid_order(1:dim_num,j) )
    write ( *, '(2x,i8,2x,i8,4x,i8,2x,i8,2x,i8)' ) j, q, grid_order(1:dim_num,j)
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Grid points:'
  write ( *, '(a)' ) ' '
  do j = 1, point_num
    write ( *, '(2x,i8,2x,3f14.6)' ) j, grid_point(1:dim_num,j)
  end do

  deallocate ( grid_order )
  deallocate ( grid_point )

  return
end
subroutine test025 ( )

!*****************************************************************************80
!
!! TEST025 tests CC_LEVELS_MINMAX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2
  integer ( kind = 4 ), parameter :: test_num = 3

  integer ( kind = 4 ) grid_num
  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: grid_level
  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: grid_order
  real    ( kind = 8 ), allocatable, dimension ( :, : ) :: grid_point
  integer ( kind = 4 ) j
  integer ( kind = 4 ) level
  integer ( kind = 4 ) level_max
  integer ( kind = 4 ), dimension ( test_num ) :: level_max_test = (/ 2, 3, 3 /)
  integer ( kind = 4 ) level_min
  integer ( kind = 4 ), dimension ( test_num ) :: level_min_test = (/ 2, 0, 3 /)
  integer ( kind = 4 ) point_num
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST025:'
  write ( *, '(a)' ) '  CC_LEVELS_MINMAX returns all Clenshaw Curtis grids'
  write ( *, '(a)' ) '  whose level value satisfies'
  write ( *, '(a)' ) '    LEVEL_MIN <= LEVEL <= LEVEL_MAX.'
  write ( *, '(a)' ) '  Here, LEVEL is the sum of the levels of the 1D rules,'
  write ( *, '(a)' ) '  and the order of the rule is 2**LEVEL + 1.'

  do test = 1, test_num

    level_min = level_min_test(test)
    level_max = level_max_test(test)

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  LEVEL_MIN = ', level_min
    write ( *, '(a,i8)' ) '  LEVEL_MAX = ', level_max
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Spatial dimension of grids = ', dim_num

    call cc_levels_minmax_size ( dim_num, level_min, level_max, grid_num, &
      point_num )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Number of grids = ', grid_num
    write ( *, '(a,i8)' ) '  Number of points in the grids = ', point_num
!
!  Allocate the space.
!
    allocate ( grid_level(1:dim_num,1:grid_num) )
    allocate ( grid_order(1:dim_num,1:grid_num) )
    allocate ( grid_point(1:dim_num,1:point_num) )
!
!  Compute the orders and points.
!
    call cc_levels_minmax ( dim_num, level_min, level_max, grid_num, &
      point_num, grid_level, grid_order, grid_point )
!
!  Now we're done.  Print the merged grid data.
!
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) &
      '      Grid     Level           Grid Levels         Grid orders:'
    write ( *, '(a)' ) &
      '      ----     -----          ------------        ------------'
    write ( *, '(a)' ) ' '
    do j = 1, grid_num
      level = sum ( grid_level(1:dim_num,j) )
      write ( *, '(2x,i8,2x,i8,4x,i8,2x,i8,2x,i8,2x,i8,2x,i8)' ) &
        j, level, grid_level(1:dim_num,j), grid_order(1:dim_num,j)
    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Grid points:'
    write ( *, '(a)' ) ' '
    do j = 1, point_num
      write ( *, '(2x,i8,2x,3f14.6)' ) j, grid_point(1:dim_num,j)
    end do

    deallocate ( grid_level )
    deallocate ( grid_order )
    deallocate ( grid_point )

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 calls CC_GRIDS_CONSTRAINED to collect constrained grids.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real    ( kind = 8 ), dimension ( dim_num ) :: alpha = (/ 2.0D+00, 3.0D+00 /)
  integer ( kind = 4 ) dim
  integer ( kind = 4 ) grid_num
  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: grid_order
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: grid_point
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( dim_num ) :: order_max
  integer ( kind = 4 ), dimension ( dim_num ) :: order_min
  integer ( kind = 4 ) point_num
  real ( kind = 8 ) q
  real ( kind = 8 ), parameter :: q_max = 13.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03:'
  write ( *, '(a)' ) '  CC_GRIDS_CONSTRAINED returns all Clenshaw Curtis grids'
  write ( *, '(a)' ) '  satisfying a set of constraints.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  ORDER(I), the order of the 1D rule in dimension I,'
  write ( *, '(a)' ) '  is constrained by '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    ORDER_MIN(I) <= ORDER(I) <= ORDER_MAX(I)'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  We also define the total weighted order Q'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Q = ALPHA(1) * ORDER(1) + ... + ALPHA(N) * ORDER(N)'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  and further constrain our grids to satisfy'
  write ( *, '(a)' ) ' '
  write ( *, '(a,f14.6)' ) '    Q <= Q_MAX = ', q_max
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension of grids = ', dim_num

  order_min(1:dim_num) = 1
  order_max(1:dim_num) = 5

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' Dimension Order_min Order_max     Alpha'
  write ( *, '(a)' ) ' '

  do dim = 1, dim_num
    write ( *, '(2x,i8,2x,i8,2x,i8,2x,f14.6)' ) &
      dim, order_min(dim), order_max(dim), alpha(dim)
  end do

  call cc_grids_constrained_size ( dim_num, q_max, alpha, &
    order_min, order_max, grid_num, point_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of grids = ', grid_num
  write ( *, '(a,i8)' ) '  Number of points in the grids = ', point_num
!
!  Allocate the space.
!
  allocate ( grid_point(1:dim_num,1:point_num) )
  allocate ( grid_order(1:dim_num,1:grid_num) )

  call cc_grids_constrained ( dim_num, q_max, alpha, &
    order_min, order_max, grid_num, point_num, grid_order, grid_point )
!
!  Now we're done.  Print the merged grid data.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         I         Q          Grid orders:'
  write ( *, '(a)' ) ' '
  do j = 1, grid_num

    q = dot_product ( alpha(1:dim_num), grid_order(1:dim_num,j) )

    write ( *, '(2x,i8,2x,f14.6,4x,i8,2x,i8,2x,i8)' ) &
      j, q, grid_order(1:dim_num,j)

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Grid points:'
  write ( *, '(a)' ) ' '
  do j = 1, point_num
    write ( *, '(2x,i8,2x,3f14.6)' ) j, grid_point(1:dim_num,j)
  end do

  deallocate ( grid_point )
  deallocate ( grid_order )

  return
end
subroutine test035 ( )

!*****************************************************************************80
!
!! TEST035 calls CC_LEVELS_CONSTRAINED to collect constrained grids.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real    ( kind = 8 ), dimension ( dim_num ) :: alpha = (/ 2.0D+00, 3.0D+00 /)
  integer ( kind = 4 ) dim
  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: grid_level
  integer ( kind = 4 ) grid_num
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: grid_point
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension ( dim_num ) :: level_max
  integer ( kind = 4 ), dimension ( dim_num ) :: level_min
  integer ( kind = 4 ) point_num
  real ( kind = 8 ) q
  real ( kind = 8 ), parameter :: q_max = 13.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST035:'
  write ( *, '(a)' ) '  CC_LEVELS_CONSTRAINED returns all Clenshaw Curtis grids'
  write ( *, '(a)' ) '  satisfying a set of constraints.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The constraint on the levels of the 1D Clenshaw Curtis'
  write ( *, '(a)' ) '  rule in spatial dimension I is:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    LEVEL_MIN(I) <= LEVEL(I) <= LEVEL_MAX(I) '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The constraint on the levels making up a rule is:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Sum ( 1 <= I <= DIM_NUM ) ALPHA(I) * LEVEL(I) <= Q_MAX.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  where Q_MAX = ', q_max
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The relationship of level to order is roughly '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    ORDER = 2**LEVEL+1.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension of grids = ', dim_num

  level_min(1:dim_num) = 1
  level_max(1:dim_num) = 5

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' Dimension Level_min Level_max        Alpha'
  write ( *, '(a)' ) ' '

  do dim = 1, dim_num
    write ( *, '(2x,i8,2x,i8,2x,i8,2x,f14.6)' ) &
      dim, level_min(dim), level_max(dim), alpha(dim)
  end do

  call cc_levels_constrained_size ( dim_num, q_max, alpha, &
    level_min, level_max, grid_num, point_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of grids = ', grid_num
  write ( *, '(a,i8)' ) '  Number of points in the grids = ', point_num
!
!  Allocate the space.
!
  allocate ( grid_point(1:dim_num,1:point_num) )
  allocate ( grid_level(1:dim_num,1:grid_num) )

  call cc_levels_constrained ( dim_num, q_max, alpha, &
    level_min, level_max, grid_num, point_num, grid_level, grid_point )
!
!  Now we're done.  Print the merged grid data.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         I         Q               Grid level:'
  write ( *, '(a)' ) ' '
  do j = 1, grid_num

    q = dot_product ( alpha(1:dim_num), grid_level(1:dim_num,j) )

    write ( *, '(2x,i8,2x,f14.6,4x,i8,2x,i8,2x,i8)' ) &
      j, q, grid_level(1:dim_num,j)

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Grid points:'
  write ( *, '(a)' ) ' '
  do j = 1, point_num
    write ( *, '(2x,i8,2x,3f14.6)' ) j, grid_point(1:dim_num,j)
  end do

  deallocate ( grid_point )
  deallocate ( grid_level )

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests CLENSHAW_CURTIS_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: order_max = 16

  integer ( kind = 4 ) i
  integer ( kind = 4 ) order
  real    ( kind = 8 ) w(order_max)
  real    ( kind = 8 ) x(order_max)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  CLENSHAW_CURTIS_COMPUTE computes'
  write ( *, '(a)' ) '  a Clenshaw-Curtis quadrature rule over [-1,1]'
  write ( *, '(a)' ) '  of given order.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     Order       W               X'
  write ( *, '(a)' ) ' '

  do order = 1, 10

    call clenshaw_curtis_compute ( order, x, w )

    write ( *, '(a)' ) ' '
    write ( *, '(2x,i8)' ) order

    do i = 1, order
      write ( *, '(10x,2x,g14.6,2x,g14.6)' ) w(i), x(i)
    end do

  end do

  return
end
subroutine test045 ( )

!*****************************************************************************80
!
!! TEST045 tests CC_ABSCISSA and CC_WEIGHT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: order = 10

  real    ( kind = 8 ) cc_abscissa
  real    ( kind = 8 ) cc_weight
  integer ( kind = 4 ) i
  real    ( kind = 8 ) w
  real    ( kind = 8 ) x

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST045'
  write ( *, '(a)' ) '  To compute a single Clenshaw Curtis weight or abscissa,'
  write ( *, '(a)' ) '  CC_ABSCISSA computes one abscissa,'
  write ( *, '(a)' ) '  CC_WEIGHT computes one weight.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  We use these routines wastefully,'
  write ( *, '(a)' ) '  to compute the order 10 rule one value at a time.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     Order       W               X'
  write ( *, '(a)' ) ' '
  write ( *, '(2x,i8)' ) order

  do i = 1, order

    x = cc_abscissa ( order, i )
    w = cc_weight ( order, i )

    write ( *, '(10x,2x,g14.6,2x,g14.6)' ) w, x

  end do

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests CLENSHAW_CURTIS_COMPUTE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: order_max = 16

  real    ( kind = 8 ), external :: f1
  real    ( kind = 8 ), external :: f2
  real    ( kind = 8 ), external :: f3
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order
  real    ( kind = 8 ), dimension ( 3 ) :: result
  real    ( kind = 8 ) weight(order_max)
  real    ( kind = 8 ) xtab(order_max)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  CLENSHAW_CURTIS_COMPUTE computes a Clenshaw-Curtis rule;'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The integration interval is [-1,1].'
  write ( *, '(a)' ) '  Quadrature order will vary.'
  write ( *, '(a)' ) '  Integrand will vary.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      Order     F1              F2              F3'
  write ( *, '(a)' ) ' '

  do order = 1, order_max

    call clenshaw_curtis_compute ( order, xtab, weight )
 
    result(1) = 0.0D+00
    do i = 1, order
      result(1) = result(1) + weight(i) * f1 ( xtab(i) )
    end do
 
    result(2) = 0.0D+00
    do i = 1, order
      result(2) = result(2) + weight(i) * f2 ( xtab(i) )
    end do

    result(3) = 0.0D+00
    do i = 1, order
      result(3) = result(3) + weight(i) * f3 ( xtab(i) )
    end do

    write ( *, '(2x,i6,2x,f14.8,2x,f14.8,2x,f14.8)' ) order, result(1:3)

  end do
 
  write ( *, '(a)' ) ' '

  result(1) = 46.0D+00 * sinh ( 1.0D+00 ) / 25.0D+00 - 2.0D+00 * sin ( 1.0D+00 )
  result(2) = 1.5822329637296729331D+00
  result(3) = ( sqrt ( 2.0D+00 ) + 3.0D+00 * sqrt ( 6.0D+00 ) ) / 6.0D+00

  write ( *, '(2x,a,2x,f14.8,2x,f14.8,2x,f14.8)' ) 'Exact ', result(1:3)

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 tests CLENSHAW_CURTIS_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: order_max = 16

  real    ( kind = 8 ), external :: f1
  real    ( kind = 8 ), external :: f2
  real    ( kind = 8 ), external :: f3
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order
  real    ( kind = 8 ), dimension ( 3 ) :: result
  real    ( kind = 8 ) weight(order_max)
  real    ( kind = 8 ) xtab(order_max)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST06'
  write ( *, '(a)' ) '  CLENSHAW_CURTIS_SET sets up a Clenshaw-Curtis rule;'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The integration interval is [-1,1].'
  write ( *, '(a)' ) '  Quadrature order will vary.'
  write ( *, '(a)' ) '  Integrand will vary.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      Order     F1              F2              F3'
  write ( *, '(a)' ) ' '

  do order = 1, order_max

    call clenshaw_curtis_set ( order, xtab, weight )
 
    result(1) = 0.0D+00
    do i = 1, order
      result(1) = result(1) + weight(i) * f1 ( xtab(i) )
    end do
 
    result(2) = 0.0D+00
    do i = 1, order
      result(2) = result(2) + weight(i) * f2 ( xtab(i) )
    end do

    result(3) = 0.0D+00
    do i = 1, order
      result(3) = result(3) + weight(i) * f3 ( xtab(i) )
    end do

    write ( *, '(2x,i6,2x,f14.8,2x,f14.8,2x,f14.8)' ) order, result(1:3)

  end do
 
  write ( *, '(a)' ) ' '

  result(1) = 46.0D+00 * sinh ( 1.0D+00 ) / 25.0D+00 - 2.0D+00 * sin ( 1.0D+00 )
  result(2) = 1.5822329637296729331D+00
  result(3) = ( sqrt ( 2.0D+00 ) + 3.0D+00 * sqrt ( 6.0D+00 ) ) / 6.0D+00

  write ( *, '(2x,a6,2x,f14.8,2x,f14.8,2x,f14.8)' ) 'Exact ', result(1:3)

  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!! TEST07 tests DTABLE_WRITE0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer   ( kind = 4 ), parameter :: dim_num = 1
  integer   ( kind = 4 ), parameter :: order = 9

  character ( len = 100 ) :: r_file = 'cc_d1_o9_r.txt'
  real      ( kind = 8 ) r(dim_num,2)
  character ( len = 100 ) :: w_file = 'cc_d1_o9_w.txt'
  real      ( kind = 8 ) w(order)
  character ( len = 100 ) :: x_file = 'cc_d1_o9_x.txt'
  real      ( kind = 8 ) x(dim_num,order)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST07'
  write ( *, '(a)' ) '  DTABLE_WRITE writes a Clenshaw-Curtis '
  write ( *, '(a)' ) '  quadrature rule to a file.'

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension = ', dim_num
  write ( *, '(a,i8)' ) '  Computing the rule of order = ', order

  call clenshaw_curtis_compute ( order, x, w )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Write abscissas to file "' // &
    trim ( x_file ) // '".'

  call dtable_write0 ( x_file, dim_num, order, x )

  write ( *, '(a)' ) '  Write weights to file "' // &
    trim ( w_file ) // '".'

  call dtable_write0 ( w_file, 1, order, w )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Write range to file "' // &
    trim ( r_file ) // '".'

  r(1:dim_num,1) = -1.0D+00
  r(1:dim_num,2) = +1.0D+00
  call dtable_write0 ( r_file, dim_num, 2, r )

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! TEST08 tests CLENSHAW_CURTIS_COMPUTE_ND.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) order
  integer ( kind = 4 ), allocatable, dimension ( : ) :: order_1d
  integer ( kind = 4 ) order_nd
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: point
  real ( kind = 8 ), allocatable, dimension ( : ) :: weight
  real ( kind = 8 ) weight_sum

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST08'
  write ( *, '(a)' ) '  CLENSHAW_CURTIS_COMPUTE_ND computes'
  write ( *, '(a)' ) '  a multidimensional Clenshaw-Curtis quadrature rule'
  write ( *, '(a)' ) '  over the hypercube [-1,1]^ND of given'
  write ( *, '(a)' ) '  (possibly different) orders in each dimension.'

  dim_num = 2

  allocate ( order_1d(1:dim_num) )

  order_1d(1:dim_num) = 5

  order_nd = product ( order_1d(1:dim_num) )

  allocate ( point(1:dim_num,1:order_nd) )
  allocate ( weight(1:order_nd) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In this example, we use the SAME ORDER'
  write ( *, '(a)' ) '  in all dimensions.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num
  write ( *, '(a,i8,i8)' ) '  1D orders = ', order_1d(1:dim_num)
  write ( *, '(a,i8)' ) '  Number of points = ', order_nd

  call clenshaw_curtis_compute_nd ( dim_num, order_1d, point, weight )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      Weight           X(1)           X(2)'
  write ( *, '(a)' ) '  --------------  --------------  --------------'
  write ( *, '(a)' ) ' '

  do order = 1, order_nd

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      weight(order), point(1:dim_num,order)

  end do

  weight_sum = sum ( weight(1:order_nd) )

  write ( *, '(a)' ) ' '
  write ( *, '(2x,g14.6)' ) weight_sum

  deallocate ( order_1d )
  deallocate ( point )
  deallocate ( weight )

  return
end
subroutine test09 ( )

!*****************************************************************************80
!
!! TEST09 tests CLENSHAW_CURTIS_COMPUTE_ND.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) order
  integer ( kind = 4 ), allocatable, dimension ( : ) :: order_1d
  integer ( kind = 4 ) order_nd
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: point
  real ( kind = 8 ), allocatable, dimension ( : ) :: weight
  real ( kind = 8 ) weight_sum

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST09'
  write ( *, '(a)' ) '  CLENSHAW_CURTIS_COMPUTE_ND computes'
  write ( *, '(a)' ) '  a multidimensional Clenshaw-Curtis quadrature rule'
  write ( *, '(a)' ) '  over the hypercube [-1,1]^ND of given'
  write ( *, '(a)' ) '  (possibly different) orders in each dimension.'

  dim_num = 3

  allocate ( order_1d(1:dim_num) )

  order_1d(1) = 2
  order_1d(2) = 4
  order_1d(3) = 3

  order_nd = product ( order_1d(1:dim_num) )

  allocate ( point(1:dim_num,1:order_nd) )
  allocate ( weight(1:order_nd) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In this example, we use DIFFERENT ORDERS'
  write ( *, '(a)' ) '  in each dimension.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num
  write ( *, '(a,i8,i8,i8)' ) '  1D orders = ', order_1d(1:dim_num)
  write ( *, '(a,i8)' ) '  Number of points = ', order_nd

  call clenshaw_curtis_compute_nd ( dim_num, order_1d, point, weight )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '      Weight           X(1)           X(2)           X(3)'
  write ( *, '(a)' ) '  --------------  --------------  --------------  --------------'
  write ( *, '(a)' ) ' '

  do order = 1, order_nd

    write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      weight(order), point(1:dim_num,order)

  end do

  weight_sum = sum ( weight(1:order_nd) )

  write ( *, '(a)' ) ' '
  write ( *, '(2x,g14.6)' ) weight_sum

  deallocate ( order_1d )
  deallocate ( point )
  deallocate ( weight )

  return
end
subroutine test10 ( )

!*****************************************************************************80
!
!! TEST10 uses DTABLE_WRITE to write out a multidimensional CC rule.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 May 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ), allocatable, dimension ( : ) :: order_1d
  integer ( kind = 4 ) order_nd
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: point
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: r
  character ( len = 100 ) :: r_file = 'cc_d4_o3x3x3x3_r.txt'
  character ( len = 100 ) :: w_file = 'cc_d4_o3x3x3x3_w.txt'
  real ( kind = 8 ), allocatable, dimension ( : ) :: weight
  character ( len = 100 ) :: x_file = 'cc_d4_o3x3x3x3_x.txt'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST10'
  write ( *, '(a)' ) '  Use DTABLE_WRITE to write out a multidimensional rule.'
  write ( *, '(a)' ) '  CLENSHAW_CURTIS_COMPUTE_ND computes'
  write ( *, '(a)' ) '  a multidimensional Clenshaw-Curtis quadrature rule'
  write ( *, '(a)' ) '  over the hypercube [-1,1]^ND of given'
  write ( *, '(a)' ) '  (possibly different) orders in each dimension.'

  dim_num = 4
 
  allocate ( order_1d(1:dim_num) )

  order_1d(1:dim_num) = 3

  order_nd = product ( order_1d(1:dim_num) )

  allocate ( point(1:dim_num,1:order_nd) )
  allocate ( r(1:dim_num,1:2) )
  allocate ( weight(1:order_nd) )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In this example, we use the SAME ORDER'
  write ( *, '(a)' ) '  in all dimensions.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num
  write ( *, '(a,i8,i8,i8,i8)' ) '  1D orders = ', order_1d(1:dim_num)
  write ( *, '(a,i8)' ) '  Number of points = ', order_nd

  call clenshaw_curtis_compute_nd ( dim_num, order_1d, point, weight )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Write abscissas to file "' // &
    trim ( x_file ) // '".'

  call dtable_write0 ( x_file, dim_num, order_nd, point )

  write ( *, '(a)' ) '  Write weights to file "' // &
    trim ( w_file ) // '".'

  call dtable_write0 ( w_file, 1, order_nd, weight )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Write range to file "' // &
    trim ( r_file ) // '".'

  r(1:dim_num,1) = -1.0D+00
  r(1:dim_num,2) = +1.0D+00

  call dtable_write0 ( r_file, dim_num, 2, r )

  deallocate ( order_1d )
  deallocate ( point )
  deallocate ( r )
  deallocate ( weight )

  return
end
subroutine test11 ( )

!*****************************************************************************80
!
!! TEST11 tests CC_ABSCISSA_LEVEL_1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) base
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order
  integer ( kind = 4 ), allocatable, dimension ( : ) :: test_level
  integer ( kind = 4 ) test_num
  integer ( kind = 4 ), allocatable, dimension ( : ) :: test_val

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST11'
  write ( *, '(a)' ) '  CC_ABSCISSA_LEVEL_1D reports the level on which'
  write ( *, '(a)' ) '  a Clenshaw Curtis abscissa of given index will first'
  write ( *, '(a)' ) '  be generated, assuming a series of grids that grow'
  write ( *, '(a)' ) '  in order as 2**LEVEL+1.'

  base = 5
  order = 2**base + 1
  test_num = 2**base + 1

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Base B = ', base
  write ( *, '(a,i8)' ) '  ORDER 2**B+1 = ', order

  allocate ( test_val(1:test_num) )
  allocate ( test_level(1:test_num) )

  do i = 1, test_num
    test_val(i) = i - 1
  end do

  call cc_abscissa_level_1d ( base, test_num, test_val, test_level )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         I  Level(I)'
  write ( *, '(a)' ) ' '

  do i = 1, test_num
    write ( *, '(2x,i8,2x,i8)' ) test_val(i), test_level(i)
  end do

  deallocate ( test_level )
  deallocate ( test_val )

  return
end
subroutine test12 ( )

!*****************************************************************************80
!
!! TEST12 tests CC_ABSCISSA_LEVEL_1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) base
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) order
  integer ( kind = 4 ) seed
  integer ( kind = 4 ), allocatable, dimension ( : ) :: test_level
  integer ( kind = 4 ) test_num
  integer ( kind = 4 ), allocatable, dimension ( : ) :: test_val

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST12'
  write ( *, '(a)' ) '  CC_ABSCISSA_LEVEL_1D can also be called for values'
  write ( *, '(a)' ) '  outside the standard range of 0 through 2**LEVEL_MAX.'
  write ( *, '(a)' ) '  In that case, a MOD operation is applied first,'
  write ( *, '(a)' ) '  to make a sensible result.'

  base = 5
  order = 2**base + 1
  seed = 123456789
  test_num = 20

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Base B = ', base
  write ( *, '(a,i8)' ) '  ORDER = 2**B+1 = ', order

  allocate ( test_val(1:test_num) )
  allocate ( test_level(1:test_num) )

  call i4vec_uniform ( test_num, -20, 100, seed, test_val )

  call cc_abscissa_level_1d ( base, test_num, test_val, test_level )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         I  Mod(I,O)  Level(I)'
  write ( *, '(a)' ) ' '

  do i = 1, test_num
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) &
      test_val(i), i4_modp ( test_val(i), order ), test_level(i)
  end do

  deallocate ( test_level )
  deallocate ( test_val )

  return
end
subroutine test13 ( )

!*****************************************************************************80
!
!! TEST13 tests CC_ABSCISSA_LEVEL_ND.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 March 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) base
  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) order
  integer ( kind = 4 ), allocatable, dimension ( : ) :: test_level
  integer ( kind = 4 ) test_num
  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: test_val

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST13'
  write ( *, '(a)' ) '  CC_ABSCISSA_LEVEL_ND reports the level on which'
  write ( *, '(a)' ) '  a Clenshaw Curtis abscissa of given index will first'
  write ( *, '(a)' ) '  be generated, assuming a series of grids that grow'
  write ( *, '(a)' ) '  in order as 2**LEVEL+1.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  This routine is applied for multidimensional cases.'

  base = 3
  order = 2**base + 1
  dim_num = 2
  test_num = order * order

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Base B = ', base
  write ( *, '(a,i8)' ) '  ORDER 2**B+1 = ', order
  write ( *, '(a,i8)' ) '  DIM_NUM = ', dim_num

  allocate ( test_val(1:dim_num,1:test_num) )
  allocate ( test_level(1:test_num) )

  k = 0
  do i = 0, order - 1
    do j = 0, order-1
      k = k + 1
      test_val(1:2,k) = (/ i, j /)
    end do
  end do

  call cc_abscissa_level_nd ( base, dim_num, test_num, test_val, test_level )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         I         J  Level(I,J)'
  write ( *, '(a)' ) ' '

  do i = 1, test_num
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) test_val(1:dim_num,i), test_level(i)
  end do

  deallocate ( test_level )
  deallocate ( test_val )

  return
end
function f1 ( x )

!*****************************************************************************80
!
!! F1 evaluates F1(X) = 23 * cosh ( x ) / 25 - cos ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 October 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Charles Clenshaw, Alan Curtis,
!    A Method for Numerical Integration on an Automatic Computer,
!    Numerische Mathematik,
!    Volume 2, Number 1, December 1960, pages 197-205.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument.
!
!    Output, real ( kind = 8 ) F1, the value of the function.
!
  implicit none

  real ( kind = 8 ) f1
  real ( kind = 8 ) x

  f1 = 23.0D+00 * cosh ( x ) / 25.0D+00 - cos ( x )

  return
end
function f2 ( x )

!*****************************************************************************80
!
!! F2 evaluates F2(X) = 1 / ( x^4 + x^2 + 0.9 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 October 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Charles Clenshaw, Alan Curtis,
!    A Method for Numerical Integration on an Automatic Computer,
!    Numerische Mathematik,
!    Volume 2, Number 1, December 1960, pages 197-205.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument.
!
!    Output, real ( kind = 8 ) F2, the value of the function.
!
  implicit none

  real ( kind = 8 ) f2
  real ( kind = 8 ) x

  f2 = 1.0D+00 / ( x**4 + x**2 + 0.9D+00 )

  return
end
function f3 ( x )

!*****************************************************************************80
!
!! F3 evaluates F3(X) = sqrt ( abs ( x + 1/2 ) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 October 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Charles Clenshaw, Alan Curtis,
!    A Method for Numerical Integration on an Automatic Computer,
!    Numerische Mathematik,
!    Volume 2, Number 1, December 1960, pages 197-205.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument.
!
!    Output, real ( kind = 8 ) F3, the value of the function.
!
  implicit none

  real ( kind = 8 ) f3
  real ( kind = 8 ) x

  f3 = sqrt ( abs ( x + 0.5D+00 ) )

  return
end
