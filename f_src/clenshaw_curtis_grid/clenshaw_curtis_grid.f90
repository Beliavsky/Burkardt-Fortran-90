function cc_abscissa ( order, i )

!*****************************************************************************80
!
!! CC_ABSCISSA returns the I-th Clenshaw Curtis abscissa.
!
!  Discussion:
!
!    Our convention is that the abscissas are numbered from left to
!    right.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the Clenshaw Curtis rule.
!    1 <= ORDER.
!
!    Input, integer ( kind = 4 ) I, the index of the desired abscissa.  
!    1 <= I <= ORDER.
!
!    Output, real ( kind = 8 ) CC_ABSCISSA, the value of the I-th 
!    abscissa in the Clenshaw Curtis rule of order ORDER.
!
  implicit none

  real ( kind = 8 ) cc_abscissa
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00

  if ( order < 1 ) then
    cc_abscissa = - huge ( cc_abscissa )
    return
  end if

  if ( order == 1 ) then
    cc_abscissa = 0.0D+00
    return
  end if

  if ( i < 1 ) then

    cc_abscissa = -1.0D+00

  else if ( i <= order ) then

    cc_abscissa = cos ( real ( order - i, kind = 8 ) * pi &
                      / real ( order - 1, kind = 8 ) )

  else

    cc_abscissa = +1.0D+00

  end if

  return
end
subroutine cc_abscissa_level_1d ( level_max, test_num, test_val, test_level )

!*****************************************************************************80
!
!! CC_ABSCISSA_LEVEL_1D: first level at which given abscissa is generated.
!
!  Discussion:
!
!    The Clenshaw Curtis abscissas are the cosines of angles, and hence
!    are somewhat raggedy real values between -1 and 1.  However,
!    it is sometimes convenient to think of them as being equally
!    spaced integers, particularly if we are mainly concerned with the
!    nesting property.  The question this routine can answer is, if
!    we are going to generate a sequence of nested Clenshaw Curtis
!    rules up to a given maximum level, on what level does a particular
!    abscissa first show up?  
!
!    Consider the sequence of numbers from 0 to 2**LEVEL_MAX.  Suppose
!    that we organize them into levels.  The first two levels are
!    somewhat arbitrary:
!
!      Level 0:  2**(LEVEL_MAX-1)
!      Level 1:  0, 2**LEVEL_MAX
!
!    But after that, the generation rule for the next level is simply
!    to generate a new value BETWEEN every consecutive pair of values
!    that have already been generated ( and these new values are simply
!    the averages of the consecutive pair.)
!
!    Thus, if our current set of values is 0, 8 and 16, on the next
!    level we generate 4 and 12 to make a new set of 0, 4, 8, 12, 16.
!    We continue this operation, level by level, until we have filled in
!    all 2**LEVEL_MAX+1 values.
!
!    This routine returns, for any value I and maximum level LEVEL_MAX, 
!    the level on which the value I will first be produced.
!
!    For example, for LEVEL_MAX = 5, the numbers we are considering
!    are 0 through 32, and they will be produced as follows:
!
!      Level
!      0:                                 16
!      1:  0                                                              32
!      2:                  8                              24
!      3:          4              12              20              28
!      4:      2       6      10      14      18      22      26      30
!      5:    1   3   5   7   9  11  13  15  17  19  21  23  25  27  29  31
!
!    Here is the list of levels for 0 through 32:
!
!          1 5 4 5 3 5 4 5 2 5 4 5 3 5 4 5 0 5 4 5 3 5 4 5 2 5 4 5 3 5 4 5 1
!
!    The purpose of this routine is, given the value 20 and the
!    maximum level 5, to return level = 3, indicating that the value 20
!    will first be generated on the 3rd level for a grid that ultimately
!    reaches an order of 2**5+1 values.
!
!    The need for this routine arises from the necessity of understanding
!    nested Clenshaw Curtis grids.  In particular, if we see a grid of
!    17 points, this is the fifth in a series of nested grids, and 8 of
!    the points are new, created specifically for the level 4 grid, while
!    9 of the points arose earlier.  This routine can report exactly when
!    each value was created.
!
!    The real need for this routine arises in multidimensional sparse grids,
!    where we essentially have a fixed "budget" of levels we are allowed to
!    use.  When we generate a multidimensional point, we determine its
!    level in each single dimensional grid, add them up, and this value
!    must be no greater than our budgeted value for the point to be included
!    in the sparse grid.
!
!    Except for the behavior of the first two levels, it is true that
!    the level of a value I is LEVEL_MAX minus the number of times I can be
!    divided evenly by 2.  Because of a peculiarity of the definition of
!    the grids, if this formula gives a level of 0 or 1, then the level
!    should be replaced by 1 or 0, respectively.
!
!    Again, except for the first two levels, the calculation is equivalent
!    to computing the location of the "first" nonzero bit in the representation
!    of a number, and subtracting that from LEVEL_MAX.  This is why all the
!    odd numbers, which have their first 1 bit in the 0-th position,
!    are assigned a level of LEVEL_MAX.
!
!    This routine can also be called for values that lie outside the standard
!    range of 0 through 2**LEVEL_MAX.  In that case, a MOD operation is 
!    applied first, to make a sensible result.
!
!
!    Note that this routine is just a 'warmup' for a multidimensional
!    version, where the calculation is more interesting, and the result
!    more useful.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) LEVEL_MAX, determines the number of points 
!    in the grid as 2**LEVEL_MAX + 1.
!
!    Input, integer ( kind = 4 ) TEST_NUM, the number of points to be tested.
!
!    Input, integer ( kind = 4 ) TEST_VAL(TEST_NUM), the indices of the points
!    to be tested.  Normally, each index would be between 0 and 2**LEVEL_MAX.
!
!    Output, integer ( kind = 4 ) TEST_LEVEL(TEST_NUM), the level at which the
!    point would first be generated, assuming that a standard sequence of
!    nested grids is used.
!
  implicit none

  integer ( kind = 4 ) test_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ), parameter :: i4_2 = 2
  integer ( kind = 4 ) level
  integer ( kind = 4 ) level_max
  integer ( kind = 4 ) order
  integer ( kind = 4 ) t
  integer ( kind = 4 ) test_level(test_num)
  integer ( kind = 4 ) test_val(test_num)

  order = 2 ** level_max + 1

  do i = 1, test_num

    t = test_val(i)
!
!  The following MOD operation is only needed to handle cases where
!  T is not in the expected range.
!
    t = i4_modp ( t, order )

    if ( t == 0 ) then

      level = 0

    else

      level = level_max

      do while ( mod ( t, i4_2 ) == 0 )
        t = t / 2
        level = level - 1
      end do     

    end if

    if ( level == 0 ) then
      level = 1
    else if ( level == 1 ) then
      level = 0
    end if

    test_level(i) = level

  end do

  return
end
subroutine cc_abscissa_level_nd ( level_max, dim_num, test_num, test_val, &
  test_level )

!*****************************************************************************80
!
!! CC_ABSCISSA_LEVEL_ND: first level at which given abscissa is generated.
!
!  Discussion:
!
!    We assume an underlying product grid.  In each dimension, this product
!    grid has order 2**LEVEL_MAX + 1.
!
!    We will say a sparse grid has total level LEVEL if each point in the
!    grid has a total level of LEVEL or less.
!
!    The "level" of a point is determined as the sum of the levels of the
!    point in each spatial dimension.
!
!    The level of a point in a single spatial dimension I is determined as
!    the level, between 0 and LEVEL_MAX, at which the point's I'th index
!    would have been generated.
!
!
!    This description is terse and perhaps unenlightening.  Keep in mind
!    that the product grid is the product of 1D Clenshaw Curtis grids,
!    that the 1D Clenshaw Curtis grids are built up by levels, having
!    orders (total number of points ) 1, 3, 5, 9, 17, 33 and so on,
!    and that these 1D grids are nested, so that each point in a 1D grid
!    has a first level at which it appears.
!
!    Our procedure for generating the points of a sparse grid, then, is
!    to choose a value LEVEL_MAX, to generate the full product grid,
!    but then only to keep those points on the full product grid whose
!    LEVEL is less than or equal to LEVEL_MAX.  
!
!
!    Note that this routine is really just testing out the idea of
!    determining the level.  Our true desire is to be able to start
!    with a value LEVEL, and determine, in a straightforward manner,
!    all the points that are generated exactly at that level, or
!    all the points that are generated up to and including that level.
!
!    This allows us to generate the new points to be added to one sparse
!    grid to get the next, or to generate a particular sparse grid at once.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) LEVEL_MAX, controls the size of the final 
!    sparse grid.
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) TEST_NUM, the number of points to be tested.
!
!    Input, integer ( kind = 4 ) TEST_VAL(DIM_NUM,TEST_NUM), the indices of 
!    the points to be tested.  Normally, each index would be between 0 
!    and 2**LEVEL_MAX.
!
!    Output, integer ( kind = 4 ) TEST_LEVEL(TEST_NUM), the value of LEVEL
!    at which the point would first be generated, assuming that a standard 
!    sequence of nested grids is used.
!
  implicit none

  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) test_num

  integer ( kind = 4 ) dim
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_2 = 2
  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) level
  integer ( kind = 4 ) level_max
  integer ( kind = 4 ) order
  integer ( kind = 4 ) t
  integer ( kind = 4 ) test_level(test_num)
  integer ( kind = 4 ) test_val(dim_num,test_num)

  order = 2 ** level_max + 1

  test_level(1:test_num) = 0

  do i = 1, test_num

    do dim = 1, dim_num

      t = test_val(dim,i)

      t = i4_modp ( t, order )

      if ( t == 0 ) then

        level = 0

      else

        level = level_max

        do while ( mod ( t, i4_2 ) == 0 )
          t = t / 2
          level = level - 1
        end do     

      end if

      if ( level == 0 ) then
        level = 1
      else if ( level == 1 ) then
        level = 0
      end if

      test_level(i) = test_level(i) + level

    end do

  end do

  return
end
subroutine cc_grid ( dim_num, order_1d, order_nd, point )

!*****************************************************************************80
!
!! CC_GRID returns a multidimensional Clenshaw-Curtis grid.
!
!  Discussion:
!
!    This routine only computes the points in the grid, but not the
!    associated integration weights.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 October 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension of the points.
!
!    Input, integer ( kind = 4 ) ORDER_1D(DIM_NUM), the order of the 
!    Clenshaw-Curtis rule in each dimension.
!
!    Input, integer ( kind = 4 ) ORDER_ND, the product of the entries of
!    ORDER_1D.
!
!    Output, real ( kind = 8 ) POINT(DIM_NUM,ORDER_ND), the points in
!    the grid.  The second dimension of this array is equal to the
!    product of the entries of ORDER_1D.
!
  implicit none

  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) order_nd

  integer ( kind = 4 ) a(dim_num)
  real ( kind = 8 ) cc_abscissa
  integer ( kind = 4 ) change
  logical done
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order_1d(dim_num)
  integer ( kind = 4 ) p
  real ( kind = 8 ) point(dim_num,order_nd)

  done = .true.
  p = 0

  do

    call vec_next_gray ( dim_num, order_1d, a, done, change )

    if ( done ) then
      exit
    end if

    if ( p == 0 ) then

      do i = 1, dim_num
        point(i,p+1) = cc_abscissa ( order_1d(i), 1 )
      end do

    else

      point(1:dim_num,p+1) = point(1:dim_num,p)
      point(change,p+1) = cc_abscissa ( order_1d(change), a(change)+1 )

    end if

    p = p + 1

  end do

  return
end
subroutine cc_grid_index ( dim_num, order_1d, order_nd, indx )

!*****************************************************************************80
!
!! CC_GRID_INDEX returns an indexed multidimensional Clenshaw-Curtis grid.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension of the points.
!
!    Input, integer ( kind = 4 ) ORDER_1D(DIM_NUM), the order of the 
!    Clenshaw-Curtis rule in each dimension.
!
!    Input, integer ( kind = 4 ) ORDER_ND, the product of the entries of 
!    ORDER_1D.
!
!    Output, integer ( kind = 4 ) INDX(DIM_NUM,ORDER_ND), the indices of the
!    points in the grid.  The second dimension of this array is equal to the
!    product of the entries of ORDER_1D.
!
  implicit none

  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) order_nd

  integer ( kind = 4 ) a(dim_num)
  integer ( kind = 4 ) change
  logical done
  integer ( kind = 4 ) order_1d(dim_num)
  integer ( kind = 4 ) p
  integer ( kind = 4 ) indx(dim_num,order_nd)

  done = .true.
  p = 0

  do

    call vec_next_gray ( dim_num, order_1d, a, done, change )

    if ( done ) then
      exit
    end if

    p = p + 1

    indx(1:dim_num,p) = a(1:dim_num)

  end do

  return
end
subroutine cc_grids_constrained ( dim_num, q_max, alpha, order_min, &
  order_max, grid_num, point_num, grid_order, grid_point )

!*****************************************************************************80
!
!! CC_GRIDS_CONSTRAINED computes CC orders and grids satisfying a constraint.
!
!  Discussion:
!
!    The constraint on the order of the 1D Clenshaw Curtis rule in 
!    spatial dimension I is:
!
!      ORDER_MIN(I) <= ORDER(I) <= ORDER_MAX(I) 
!
!    The constraint on the collection of orders making up a rule is:
!
!      Sum ( 1 <= I <= DIM_NUM ) ALPHA(I) * ORDER(I) <= Q_MAX.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, real ( kind = 8 ) Q_MAX, the maximum values of
!    Q, the sum of the weighted orders in each spatial coordinate.
!
!    Input, real ( kind = 8 ) ALPHA(DIM_NUM), the weight factors for
!    the orders in each spatial dimension.
!
!    Input, integer ( kind = 4 ) ORDER_MIN(DIM_NUM), ORDER_MAX(DIM_NUM), the 
!    minimum and maximum values of the order of the 1D Clenshaw Curtis rule
!    in each spatial dimension.
!
!    Input, integer ( kind = 4 ) GRID_NUM, the number of Clenshaw Curtis
!    grids in the constraint set.
!
!    Input, integer ( kind = 4 ) POINT_NUM, the total number of points in 
!    the grids.
!
!    Output, integer ( kind = 4 ) GRID_ORDER(DIM_NUM,GRID_NUM), contains, for 
!    each grid, the order of the Clenshaw-Curtis rule in each dimension.
!
!    Output, real ( kind = 8 ) GRID_POINT(DIM_NUM,POINT_NUM), contains
!    a list of all the abscissas of all the rules, listed one grid at
!    a time.  If a point occurs in several grids, it will be listed
!    several times.
!
  implicit none

  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) grid_num
  integer ( kind = 4 ) point_num

  real ( kind = 8 ), dimension ( dim_num ) :: alpha
  integer ( kind = 4 ), dimension ( dim_num, grid_num ) :: grid_order
  real ( kind = 8 ),dimension ( dim_num, point_num ) :: grid_point
  logical more
  integer ( kind = 4 ), dimension ( dim_num ) :: order_1d
  integer ( kind = 4 ), dimension ( dim_num ) :: order_max
  integer ( kind = 4 ), dimension ( dim_num ) :: order_min
  integer ( kind = 4 ) order_nd
  real ( kind = 8 ) :: q_max

  point_num = 0
  grid_num = 0

  more = .false.

  do 

    call vector_constrained_next4 ( dim_num, alpha, order_min, order_max, &
      order_1d, q_max, more )

    if ( .not. more ) then
      exit
    end if

    order_nd = product ( order_1d(1:dim_num) )

    call cc_grid ( dim_num, order_1d, order_nd, &
      grid_point(1:dim_num,point_num+1:point_num+order_nd) )

    point_num = point_num + order_nd

    grid_num = grid_num + 1
    grid_order(1:dim_num,grid_num) = order_1d(1:dim_num)

  end do

  return
end
subroutine cc_grids_constrained_size ( dim_num, q_max, alpha, order_min, &
  order_max, grid_num, point_num )

!*****************************************************************************80
!
!! CC_GRIDS_CONSTRAINED_SIZE counts grids for CC_GRIDS_CONSTRAINED.
!
!  Discussion:
!
!    The constraint on the order of the 1D Clenshaw Curtis rule in 
!    spatial dimension I is:
!
!      ORDER_MIN(I) <= ORDER(I) <= ORDER_MAX(I) 
!
!    The constraint on the collection of orders making up a rule is:
!
!      Sum ( 1 <= I <= DIM_NUM ) ALPHA(I) * ORDER(I) <= Q_MAX.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, real ( kind = 8 ) Q_MAX, the maximum values of
!    Q, the sum of the weighted orders in each spatial coordinate.
!
!    Input, real ( kind = 8 ) ALPHA(DIM_NUM), the weight factors for
!    the orders in each spatial dimension.
!
!    Input, integer ( kind = 4 ) ORDER_MIN(DIM_NUM), ORDER_MAX(DIM_NUM), 
!    the minimum and maximum values of the order of the 1D Clenshaw Curtis rule
!    in each spatial dimension.
!
!    Output, integer ( kind = 4 ) GRID_NUM, the number of Clenshaw Curtis
!    grids in the constraint set.
!
!    Output, integer ( kind = 4 ) POINT_NUM, the total number of points in 
!    the grids.
!
  implicit none

  integer ( kind = 4 ) dim_num

  real ( kind = 8 ), dimension ( dim_num ) :: alpha
  integer ( kind = 4 ) grid_num
  logical more
  integer ( kind = 4 ), dimension ( dim_num ) :: order_1d
  integer ( kind = 4 ), dimension ( dim_num ) :: order_max
  integer ( kind = 4 ), dimension ( dim_num ) :: order_min
  integer ( kind = 4 ) order_nd
  integer ( kind = 4 ) point_num
  real ( kind = 8 ) :: q_max
!
!  Determine the total number of points that will be generated
!  by "going through the motions".
!
  point_num = 0
  grid_num = 0

  more = .false.

  do 

    call vector_constrained_next4 ( dim_num, alpha, order_min, order_max, &
      order_1d, q_max, more )

    if ( .not. more ) then
      exit
    end if

    order_nd = product ( order_1d(1:dim_num) )

    point_num = point_num + order_nd

    grid_num = grid_num + 1

  end do

  return
end
subroutine cc_grids_minmax ( dim_num, q_min, q_max, grid_num, point_num, &
  grid_order, grid_point )

!*****************************************************************************80
!
!! CC_GRIDS_MINMAX computes CC orders and grids with Q_MIN <= Q <= Q_MAX.
!
!  Discussion:
!
!    For a multidimensional Clenshaw Curtis product grid, the value of Q is
!    simply the sum of the orders of the 1 dimensional factor grids.
!
!    The necessary dimensions of GRID_ORDER and GRID_POINT can be
!    determined by calling CC_GRIDS_MINMAX first.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) Q_MIN, Q_MAX, the minimum and maximum values 
!    of Q, the sum of the orders in each spatial coordinate.
!
!    Input, integer ( kind = 4 ) GRID_NUM, the number of Clenshaw Curtis
!    grids whose Q value is between Q_MIN and Q_MAX.
!
!    Input, integer ( kind = 4 ) POINT_NUM, the total number of points in 
!    the grids.
!
!    Output, integer ( kind = 4 ) GRID_ORDER(DIM_NUM,GRID_NUM), contains, 
!    for each grid, the order of the Clenshaw-Curtis rule in each dimension.
!
!    Output, real ( kind = 8 ) GRID_POINT(DIM_NUM,POINT_NUM), contains
!    a list of all the abscissas of all the rules, listed one grid at
!    a time.  If a point occurs in several grids, it will be listed
!    several times.
!
  implicit none

  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) point_num

  integer ( kind = 4 ) grid_num
  integer ( kind = 4 ), dimension ( dim_num, point_num ) :: grid_order
  real ( kind = 8 ), dimension ( dim_num, point_num ) :: grid_point
  logical more
  integer ( kind = 4 ), dimension ( dim_num ) :: order_1d
  integer ( kind = 4 ) order_nd
  integer ( kind = 4 ) q
  integer ( kind = 4 ) q_max
  integer ( kind = 4 ) q_min
!
!  Outer loop generates Q's from Q_MIN to Q_MAX.
!
  point_num = 0
  grid_num = 0

  do q = q_min, q_max
!
!  Middle loop generates next partition that adds up to Q.
!
    more = .false.

    do

      call compnz_next ( q, dim_num, order_1d, more )
!
!  Inner (hidden) loop generates all CC points corresponding to given grid.
!
      order_nd = product ( order_1d(1:dim_num) )

      call cc_grid ( dim_num, order_1d, order_nd, &
        grid_point(1:dim_num,point_num+1:point_num+order_nd) )

      point_num = point_num + order_nd

      grid_num = grid_num + 1
      grid_order(1:dim_num,grid_num) = order_1d(1:dim_num)

      if ( .not. more ) then
        exit
      end if

    end do

  end do

  return
end
subroutine cc_grids_minmax_size ( dim_num, q_min, q_max, grid_num, point_num )

!*****************************************************************************80
!
!! CC_GRIDS_MINMAX_SIZE counts grids for CC_GRIDS_MINMAX.
!
!  Discussion:
!
!    For a multidimensional Clenshaw Curtis product grid, the value of Q is
!    simply the sum of the orders of the 1 dimensional factor grids.
!
!    This routine can be used to determine the necessary size to be
!    allocated to arrays GRID_ORDER and GRID_POINT in a call to
!    CC_GRIDS_MINMAX.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) Q_MIN, Q_MAX, the minimum and maximum values 
!    of Q, the sum of the orders in each spatial coordinate.
!
!    Output, integer ( kind = 4 ) GRID_NUM, the number of Clenshaw Curtis
!    grids whose Q value is between Q_MIN and Q_MAX.
!
!    Output, integer ( kind = 4 ) POINT_NUM, the total number of points in 
!    the grids.
!
  implicit none

  integer ( kind = 4 ) dim_num

  integer ( kind = 4 ) grid_num
  logical more
  integer ( kind = 4 ), dimension ( dim_num ) :: order_1d
  integer ( kind = 4 ) order_nd
  integer ( kind = 4 ) point_num
  integer ( kind = 4 ) q
  integer ( kind = 4 ) q_max
  integer ( kind = 4 ) q_min
!
!  Determine the total number of points that will be generated
!  by "going through the motions".
!
  point_num = 0
  grid_num = 0

  do q = q_min, q_max

    more = .false.

    do

      call compnz_next ( q, dim_num, order_1d, more )

      order_nd = product ( order_1d(1:dim_num) )

      point_num = point_num + order_nd

      grid_num = grid_num + 1

      if ( .not. more ) then
        exit
      end if

    end do

  end do

  return
end
subroutine cc_level_to_order ( dim_num, level, order )

!*****************************************************************************80
!
!! CC_LEVEL_TO_ORDER converts a CC nesting level to a CC order.
!
!  Discussion:
!
!    Clenshaw Curtis grids can naturally be nested.  Except for the
!    first case of LEVEL = 0, the relationship is
!
!      ORDER = 2**LEVEL + 1
!
!    Nesting    Order
!    Level
!
!    0          1
!    1          3 =  2 + 1
!    2          5 =  4 + 1
!    3          9 =  8 + 1
!    4         17 = 16 + 1
!    5         33 = 32 + 1
!
!    In this routine, we assume that a vector of levels is given,
!    and the corresponding orders are desired.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 November 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) LEVEL(DIM_NUM), the nesting levels of the 
!    1D rules.
!
!    Output, integer ( kind = 4 ) ORDER(DIM_NUM), the order (number of points)
!    of the 1D Clenshaw Curtis rules.
!
  implicit none

  integer ( kind = 4 ) dim_num

  integer ( kind = 4 ) dim
  integer ( kind = 4 ) level(dim_num)
  integer ( kind = 4 ) order(dim_num)

  do dim = 1, dim_num

    if ( level(dim) < 0 ) then
      order(dim) = -1
    else if ( level(dim) == 0 ) then
      order(dim) = 1
    else
      order(dim) = ( 2**level(dim) ) + 1 
    end if

  end do

  return
end
subroutine cc_levels_constrained ( dim_num, q_max, alpha, level_min, &
  level_max, grid_num, point_num, grid_level, grid_point )

!*****************************************************************************80
!
!! CC_LEVELS_CONSTRAINED: CC grids with constrained levels.
!
!  Discussion:
!
!    The constraint on the levels of the 1D Clenshaw Curtis rule in 
!    spatial dimension I is:
!
!      LEVEL_MIN(I) <= LEVEL(I) <= LEVEL_MAX(I) 
!
!    The constraint on the collection of levels making up a rule is:
!
!      Sum ( 1 <= I <= DIM_NUM ) ALPHA(I) * LEVEL(I) <= Q_MAX.
!
!    The relationship of level to order is roughly 
!
!      ORDER = 2^LEVEL+1.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, real ( kind = 8 ) Q_MAX, the maximum values of
!    Q, the sum of the weighted orders in each spatial coordinate.
!
!    Input, real ( kind = 8 ) ALPHA(DIM_NUM), the weight factors for
!    the orders in each spatial dimension.
!
!    Input, integer ( kind = 4 ) LEVEL_MIN(DIM_NUM), LEVEL_MAX(DIM_NUM), the
!    minimum and maximum values of the level of the 1D Clenshaw Curtis rule
!    in each spatial dimension.
!
!    Input, integer ( kind = 4 ) GRID_NUM, the number of Clenshaw Curtis
!    grids in the constraint set.
!
!    Input, integer ( kind = 4 ) POINT_NUM, the total number of points in 
!    the grids.
!
!    Output, integer ( kind = 4 ) GRID_LEVEL(DIM_NUM,GRID_NUM), contains, 
!    for each grid, the level of the Clenshaw-Curtis rule in each dimension.
!
!    Output, real ( kind = 8 ) GRID_POINT(DIM_NUM,POINT_NUM), contains
!    a list of all the abscissas of all the rules, listed one grid at
!    a time.  If a point occurs in several grids, it will be listed
!    several times.
!
  implicit none

  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) grid_num
  integer ( kind = 4 ) point_num

  real ( kind = 8 ), dimension ( dim_num ) :: alpha
  integer ( kind = 4 ), dimension ( dim_num, grid_num ) :: grid_level
  real ( kind = 8 ),dimension ( dim_num, point_num ) :: grid_point
  logical more
  integer ( kind = 4 ), dimension ( dim_num ) :: level_1d
  integer ( kind = 4 ), dimension ( dim_num ) :: level_max
  integer ( kind = 4 ), dimension ( dim_num ) :: level_min
  integer ( kind = 4 ), dimension ( dim_num ) :: order_1d
  integer ( kind = 4 ) order_nd
  real ( kind = 8 ) :: q_max

  point_num = 0
  grid_num = 0

  more = .false.

  do 

    call vector_constrained_next4 ( dim_num, alpha, level_min, level_max, &
      level_1d, q_max, more )

    if ( .not. more ) then
      exit
    end if

    call cc_level_to_order ( dim_num, level_1d, order_1d )

    order_nd = product ( order_1d(1:dim_num) )

    call cc_grid ( dim_num, order_1d, order_nd, &
      grid_point(1:dim_num,point_num+1:point_num+order_nd) )

    point_num = point_num + order_nd

    grid_num = grid_num + 1
    grid_level(1:dim_num,grid_num) = level_1d(1:dim_num)

  end do

  return
end
subroutine cc_levels_constrained_size ( dim_num, q_max, alpha, level_min, &
  level_max, grid_num, point_num )

!*****************************************************************************80
!
!! CC_LEVELS_CONSTRAINED_SIZE counts grids for CC_LEVELS_CONSTRAINED.
!
!  Discussion:
!
!    The constraint on the levels of the 1D Clenshaw Curtis rule in 
!    spatial dimension I is:
!
!      LEVEL_MIN(I) <= LEVEL(I) <= LEVEL_MAX(I) 
!
!    The constraint on the collection of levels making up a rule is:
!
!      Sum ( 1 <= I <= DIM_NUM ) ALPHA(I) * LEVEL(I) <= Q_MAX.
!
!    The relationship of level to order is roughly 
!
!      ORDER = 2^LEVEL+1.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, real ( kind = 8 ) Q_MAX, the maximum values of
!    Q, the sum of the weighted orders in each spatial coordinate.
!
!    Input, real ( kind = 8 ) ALPHA(DIM_NUM), the weight factors for
!    the orders in each spatial dimension.
!
!    Input, integer ( kind = 4 ) LEVEL_MIN(DIM_NUM), LEVEL_MAX(DIM_NUM), the 
!    minimum and maximum values of the level of the 1D Clenshaw Curtis rule
!    in each spatial dimension.
!
!    Output, integer ( kind = 4 ) GRID_NUM, the number of Clenshaw Curtis
!    grids in the constraint set.
!
!    Output, integer ( kind = 4 ) POINT_NUM, the total number of points in 
!    the grids.
!
  implicit none

  integer ( kind = 4 ) dim_num

  real ( kind = 8 ), dimension ( dim_num ) :: alpha
  integer ( kind = 4 ) grid_num
  logical more
  integer ( kind = 4 ), dimension ( dim_num ) :: level_1d
  integer ( kind = 4 ), dimension ( dim_num ) :: level_max
  integer ( kind = 4 ), dimension ( dim_num ) :: level_min
  integer ( kind = 4 ), dimension ( dim_num ) :: order_1d
  integer ( kind = 4 ) order_nd
  integer ( kind = 4 ) point_num
  real ( kind = 8 ) :: q_max
!
!  Determine the total number of points that will be generated
!  by "going through the motions".
!
  point_num = 0
  grid_num = 0

  more = .false.

  do 

    call vector_constrained_next4 ( dim_num, alpha, level_min, level_max, &
      level_1d, q_max, more )

    if ( .not. more ) then
      exit
    end if

    call cc_level_to_order ( dim_num, level_1d, order_1d )

    order_nd = product ( order_1d(1:dim_num) )

    point_num = point_num + order_nd

    grid_num = grid_num + 1

  end do

  return
end
subroutine cc_levels_minmax ( dim_num, level_min, level_max, grid_num, &
  point_num, grid_level, grid_order, grid_point )

!*****************************************************************************80
!
!! CC_LEVELS_MINMAX computes CC grids with LEVEL_MIN <= LEVEL <= LEVEL_MAX.
!
!  Discussion:
!
!    The CC grids are required to have an order that is 2**LEVEL + 1.
!
!    The necessary dimensions of GRID_LEVEL, GRID_ORDER and GRID_POINT can be
!    determined by calling CC_LEVELS_MINMAX first.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 July 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) LEVEL_MIN, LEVEL_MAX, the minimum and maximum
!    values of LEVEL.
!
!    Input, integer ( kind = 4 ) GRID_NUM, the number of Clenshaw Curtis
!    grids whose LEVEL value is between LEVEL_MIN and LEVEL_MAX.
!
!    Input, integer ( kind = 4 ) POINT_NUM, the total number of points in 
!    the grids.
!
!    Output, integer ( kind = 4 ) GRID_LEVEL(DIM_NUM,GRID_NUM), contains, 
!    for each grid, the level of the Clenshaw-Curtis rule in each dimension.
!
!    Output, integer ( kind = 4 ) GRID_ORDER(DIM_NUM,GRID_NUM), contains, 
!    for each grid, the order of the Clenshaw-Curtis rule in each dimension.
!
!    Output, real ( kind = 8 ) GRID_POINT(DIM_NUM,POINT_NUM), contains
!    a list of all the abscissas of all the rules, listed one grid at
!    a time.  If a point occurs in several grids, it will be listed
!    several times.
!
  implicit none

  integer ( kind = 4 ) dim_num
  integer ( kind = 4 ) point_num

  integer ( kind = 4 ) grid_num
  integer ( kind = 4 ), dimension ( dim_num, point_num ) :: grid_level
  integer ( kind = 4 ), dimension ( dim_num, point_num ) :: grid_order
  real ( kind = 8 ), dimension ( dim_num, point_num ) :: grid_point
  integer ( kind = 4 ) h
  integer ( kind = 4 ) level
  integer ( kind = 4 ), dimension ( dim_num ) :: level_1d
  integer ( kind = 4 ) level_max
  integer ( kind = 4 ) level_min
  logical more
  integer ( kind = 4 ), dimension ( dim_num ) :: order_1d
  integer ( kind = 4 ) order_nd
  integer ( kind = 4 ) t
!
!  Outer loop generates LEVELs from LEVEL_MIN to LEVEL_MAX.
!
  point_num = 0
  grid_num = 0

  do level = level_min, level_max
!
!  Middle loop generates next partition that adds up to LEVEL.
!
    more = .false.
    h = 0
    t = 0

    do

      call comp_next ( level, dim_num, level_1d, more, h, t )
!
!  Inner (hidden) loop generates all CC points corresponding to given grid.
!
      call cc_level_to_order ( dim_num, level_1d, order_1d )

      order_nd = product ( order_1d(1:dim_num) )

      call cc_grid ( dim_num, order_1d, order_nd, &
        grid_point(1:dim_num,point_num+1:point_num+order_nd) )

      point_num = point_num + order_nd

      grid_num = grid_num + 1
      grid_level(1:dim_num,grid_num) = level_1d(1:dim_num)
      grid_order(1:dim_num,grid_num) = order_1d(1:dim_num)

      if ( .not. more ) then
        exit
      end if

    end do

  end do

  return
end
subroutine cc_levels_minmax_size ( dim_num, level_min, level_max, grid_num, &
  point_num )

!*****************************************************************************80
!
!! CC_LEVELS_MINMAX_SIZE counts grids for CC_LEVELS_MINMAX.
!
!  Discussion:
!
!    This routine can be used to determine the necessary size to be
!    allocated to arrays GRID_LEVEL, GRID_ORDER and GRID_POINT in a call to
!    CC_LEVELS_MINMAX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 July 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, integer ( kind = 4 ) LEVEL_MIN, LEVEL_MAX, the minimum and 
!    maximum values of LEVEL, the sum of the levels in each spatial coordinate.
!
!    Output, integer ( kind = 4 ) GRID_NUM, the number of Clenshaw Curtis
!    grids whose LEVEL value is between LEVEL_MIN and LEVEL_MAX.
!
!    Output, integer ( kind = 4 ) POINT_NUM, the total number of points in 
!    the grids.
!
  implicit none

  integer ( kind = 4 ) dim_num

  integer ( kind = 4 ) grid_num
  integer ( kind = 4 ) h
  integer ( kind = 4 ) level
  integer ( kind = 4 ), dimension ( dim_num ) :: level_1d
  integer ( kind = 4 ) level_max
  integer ( kind = 4 ) level_min
  logical more
  integer ( kind = 4 ), dimension ( dim_num ) :: order_1d
  integer ( kind = 4 ) order_nd
  integer ( kind = 4 ) point_num
  integer ( kind = 4 ) t
!
!  Determine the total number of points that will be generated
!  by "going through the motions".
!
  point_num = 0
  grid_num = 0

  do level = level_min, level_max

    more = .false.
    h = 0
    t = 0

    do

      call comp_next ( level, dim_num, level_1d, more, h, t )

      call cc_level_to_order ( dim_num, level_1d, order_1d )

      order_nd = product ( order_1d(1:dim_num) )

      point_num = point_num + order_nd

      grid_num = grid_num + 1

      if ( .not. more ) then
        exit
      end if

    end do

  end do

  return
end
function cc_weight ( order, i )

!*****************************************************************************80
!
!! CC_WEIGHT returns the I-th Clenshaw Curtis weight.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) ORDER, the order of the rule.
!
!    Input, integer ( kind = 4 ) I, the index of the desired weight.  
!    1 <= I <= ORDER.
!
!    Output, real ( kind = 8 ) CC_WEIGHT, the I-th weight in the
!    Clenshaw-Curtis rule of order ORDER.
!
  implicit none

  real ( kind = 8 ) angle
  real ( kind = 8 ) b
  real ( kind = 8 ) cc_weight
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) order
  real ( kind = 8 ) :: pi = 3.141592653589793D+00
  real ( kind = 8 ) value

  if ( order < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CC_WEIGHT - Fatal error!'
    write ( *, '(a)' ) '  ORDER < 1.'
    stop
  end if

  if ( order == 1 ) then
    value = 2.0D+00
    cc_weight = value
    return
  end if

  value = 1.0D+00

  do j = 1, ( order - 1 ) / 2

    if ( 2 * j == ( order - 1 ) ) then
      b = 1.0D+00
    else
      b = 2.0D+00
    end if

    angle = real ( 2 * j * ( i - 1 ), kind = 8 ) * pi &
          / real ( order - 1, kind = 8 )

    value = value - b * cos ( angle ) / real ( 4 * j * j - 1, kind = 8 )

  end do

  if ( i == 1 ) then
    value =           value / real ( order - 1, kind = 8 )
  else if ( i <= order - 1 ) then
    value = 2.0D+00 * value / real ( order - 1, kind = 8 )
  else if ( i == order ) then
    value =           value / real ( order - 1, kind = 8 )
  end if

  cc_weight = value

  return
end
subroutine clenshaw_curtis_compute ( n, x, w )

!*****************************************************************************80
!
!! CLENSHAW_CURTIS_COMPUTE computes a Clenshaw Curtis quadrature rule.
!
!  Discussion:
!
!    This method uses a direct approach.  The paper by Waldvogel
!    exhibits a more efficient approach using Fourier transforms.
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
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!    Joerg Waldvogel,
!    Fast Construction of the Fejer and Clenshaw-Curtis Quadrature Rules,
!    BIT Numerical Mathematics
!    Volume 43, Number 1, 2003, pages 1-18.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the rule.
!
!    Output, real ( kind = 8 )  X(N), W(N), the abscissas and weights
!    of the rule.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) :: pi = 3.141592653589793D+00
  real ( kind = 8 ) theta(n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CLENSHAW_CURTIS_COMPUTE - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    stop 1
  end if

  if ( n == 1 ) then
    x(1) = 0.0D+00
    w(1) = 2.0D+00
    return
  end if

  do i = 1, n
    theta(i) = real ( n - i, kind = 8 ) * pi &
             / real ( n - 1, kind = 8 )
  end do

  x(1:n) = cos ( theta(1:n) )

  do i = 1, n

    w(i) = 1.0D+00

    do j = 1, ( n - 1 ) / 2

      if ( 2 * j == ( n - 1 ) ) then
        b = 1.0D+00
      else
        b = 2.0D+00
      end if

      w(i) = w(i) - b * cos ( 2.0D+00 * real ( j, kind = 8 ) * theta(i) ) &
           / real ( 4 * j * j - 1, kind = 8 )

    end do

  end do

  w(1)     =           w(1)     / real ( n - 1, kind = 8 )
  w(2:n-1) = 2.0D+00 * w(2:n-1) / real ( n - 1, kind = 8 )
  w(n)     =           w(n)     / real ( n - 1, kind = 8 )

  return
end
subroutine clenshaw_curtis_compute_nd ( dim_num, order_1d, point, weight )

!*****************************************************************************80
!
!! CLENSHAW_CURTIS_COMPUTE_ND returns a multidimensional Clenshaw-Curtis rule.
!
!  Discussion:
!
!    The value ORDER_ND, used to dimension the output arrays, is
!    simply the product of the entries in ORDER_1D.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension of the points.
!
!    Input, integer ( kind = 4 ) ORDER_1D(DIM_NUM), the order of the 
!    Clenshaw-Curtis rule in each dimension.
!
!    Output, real ( kind = 8 ) POINT(DIM_NUM,ORDER_ND), the points in
!    the grid.
!
!    Output, real ( kind = 8 ) WEIGHT(ORDER_ND), the integration weights
!    associated with the points.
!
  implicit none

  integer ( kind = 4 ) dim_num

  integer ( kind = 4 ) dim
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3
  integer ( kind = 4 ) order
  integer ( kind = 4 ) order_1d(dim_num)
  integer ( kind = 4 ) order_nd
  integer ( kind = 4 ) order_old
  integer ( kind = 4 ) p
  real ( kind = 8 ) point(dim_num,*)
  real ( kind = 8 ), allocatable, dimension ( : ) :: w1d
  real ( kind = 8 ) weight(*)
  real ( kind = 8 ), allocatable, dimension ( : ) :: x1d

  order_nd = product ( order_1d(1:dim_num) )

  weight(1:order_nd) = 1.0D+00

  order = -1

  do dim = 1, dim_num

    order_old = order

    order = order_1d(dim)
!
!  For efficiency's sake, we reuse the 1D rule if we can.
!
    if ( order /= order_old ) then

      if ( allocated ( x1d ) ) then
        deallocate ( x1d )
      end if
      if ( allocated ( w1d ) ) then
        deallocate ( w1d )
      end if

      allocate ( x1d(1:order) )
      allocate ( w1d(1:order) )

      call clenshaw_curtis_compute ( order, x1d, w1d )

    end if

    p = 0
    n1 = product ( order_1d(1:dim-1) )
    n2 =           order_1d(dim)
    n3 = product ( order_1d(dim+1:dim_num))

    do k = 1, n1
      do j = 1, n2
        point(dim,p+1:p+n3) = x1d(j)
        weight(p+1:p+n3) = weight(p+1:p+n3) * w1d(j)
        p = p + n3
      end do
    end do

  end do

  if ( allocated ( x1d ) ) then
    deallocate ( x1d )
  end if

  if ( allocated ( w1d ) ) then
    deallocate ( w1d )
  end if

  return
end
subroutine clenshaw_curtis_set ( order, xtab, weight )

!*****************************************************************************80
!
!! CLENSHAW_CURTIS_SET sets a Clenshaw-Curtis quadrature rule.
!
!  Discussion:
!
!    The integration interval is [ -1, 1 ].
!
!    The integral to approximate: 
!
!      Integral ( -1 <= X <= 1 ) F(X) dX
!
!    The quadrature rule:
!
!      Sum ( 1 <= I <= ORDER ) WEIGHT(I) * F ( XTAB(I) )
!
!    The abscissas for the rule of order ORDER can be regarded 
!    as the cosines of equally spaced angles between 180 and 0 degrees:
!
!      XTAB(I) = cos ( ( I - 1 ) * PI / ( ORDER - 1 ) )
!
!    except for the basic case ORDER = 1, when
!
!      XTAB(1) = 0.
!
!    If the value of ORDER is increased in a sensible way, then
!    the new set of abscissas will include the old ones.  One such
!    sequence would be ORDER(K) = 2*K+1 for K = 0, 1, 2, ...
!    Thus, in the table below, the abscissas for order 9 include
!    those for order 5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 May 2006
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
!    Input, integer ( kind = 4 ) ORDER, the order of the rule.
!    ORDER must be between 1 and 17, 33, 65 or 129.
!
!    Output, real ( kind = 8 ) XTAB(ORDER), the abscissas of the rule.
!
!    Output, real ( kind = 8 ) WEIGHT(ORDER), the weights of the rule.
!    The weights are symmetric and sum to 2.
!
  implicit none

  integer ( kind = 4 ) order

  real ( kind = 8 ) weight(order)
  real ( kind = 8 ) xtab(order)

  if ( order == 1 ) then

    xtab(1) = 0.0D+00
    weight(1) = 2.0D+00

  else if ( order == 2 ) then

    xtab(1) = -1.0D+00
    xtab(2) =  1.0D+00

    weight(1) = 1.0D+00
    weight(2) = 1.0D+00

  else if ( order == 3 ) then

    xtab(1) = -1.0D+00
    xtab(2) =  0.0D+00
    xtab(3) =  1.0D+00

    weight(1) =  0.33333333333333D+00
    weight(2) =  1.33333333333333D+00
    weight(3) =  0.33333333333333D+00

  else if ( order == 4 ) then

    xtab(1) = -1.0D+00
    xtab(2) = -0.5D+00
    xtab(3) =  0.5D+00
    xtab(4) =  1.0D+00

    weight(1) =  0.11111111111111D+00
    weight(2) =  0.88888888888889D+00
    weight(3) =  0.88888888888889D+00
    weight(4) =  0.11111111111111D+00

  else if ( order == 5 ) then

    xtab(1) = -1.00000000000000D+00
    xtab(2) = -0.70710678118655D+00
    xtab(3) =  0.00000000000000D+00
    xtab(4) =  0.70710678118655D+00
    xtab(5) =  1.00000000000000D+00

    weight(1) =  0.06666666666667D+00
    weight(2) =  0.53333333333333D+00
    weight(3) =  0.80000000000000D+00
    weight(4) =  0.53333333333333D+00
    weight(5) =  0.06666666666667D+00

  else if ( order == 6 ) then

    xtab(1) = -1.00000000000000D+00
    xtab(2) = -0.80901699437495D+00
    xtab(3) = -0.30901699437495D+00
    xtab(4) =  0.30901699437495D+00
    xtab(5) =  0.80901699437495D+00
    xtab(6) =  1.00000000000000D+00

    weight(1) =  0.04000000000000D+00
    weight(2) =  0.36074304120001D+00
    weight(3) =  0.59925695879999D+00
    weight(4) =  0.59925695879999D+00
    weight(5) =  0.36074304120001D+00
    weight(6) =  0.04000000000000D+00

  else if ( order == 7 ) then

    xtab(1) = -1.00000000000000D+00
    xtab(2) = -0.86602540378444D+00
    xtab(3) = -0.50000000000000D+00
    xtab(4) =  0.00000000000000D+00
    xtab(5) =  0.50000000000000D+00
    xtab(6) =  0.86602540378444D+00
    xtab(7) =  1.00000000000000D+00
   
    weight(1) = 0.02857142857143D+00
    weight(2) = 0.25396825396825D+00
    weight(3) = 0.45714285714286D+00
    weight(4) = 0.52063492063492D+00
    weight(5) = 0.45714285714286D+00
    weight(6) = 0.25396825396825D+00
    weight(7) = 0.02857142857143D+00

  else if ( order == 8 ) then
   
    xtab(1) = -1.00000000000000D+00
    xtab(2) = -0.90096886790242D+00
    xtab(3) = -0.62348980185873D+00
    xtab(4) = -0.22252093395631D+00
    xtab(5) =  0.22252093395631D+00
    xtab(6) =  0.62348980185873D+00
    xtab(7) =  0.90096886790242D+00
    xtab(8) =  1.00000000000000D+00
   
    weight(1) = 0.02040816326531D+00
    weight(2) = 0.19014100721821D+00
    weight(3) = 0.35224242371816D+00
    weight(4) = 0.43720840579833D+00
    weight(5) = 0.43720840579833D+00
    weight(6) = 0.35224242371816D+00
    weight(7) = 0.19014100721821D+00
    weight(8) = 0.02040816326531D+00

  else if ( order == 9 ) then
 
    xtab(1) = -1.00000000000000D+00
    xtab(2) = -0.92387953251129D+00
    xtab(3) = -0.70710678118655D+00
    xtab(4) = -0.38268343236509D+00
    xtab(5) =  0.00000000000000D+00
    xtab(6) =  0.38268343236509D+00
    xtab(7) =  0.70710678118655D+00
    xtab(8) =  0.92387953251129D+00
    xtab(9) =  1.00000000000000D+00

    weight(1) = 0.01587301587302D+00
    weight(2) = 0.14621864921602D+00
    weight(3) = 0.27936507936508D+00
    weight(4) = 0.36171785872049D+00
    weight(5) = 0.39365079365079D+00
    weight(6) = 0.36171785872049D+00
    weight(7) = 0.27936507936508D+00
    weight(8) = 0.14621864921602D+00
    weight(9) = 0.01587301587302D+00

  else if ( order == 10 ) then

    xtab(1) =  -1.00000000000000D+00
    xtab(2) =  -0.93969262078591D+00
    xtab(3) =  -0.76604444311898D+00
    xtab(4) =  -0.50000000000000D+00
    xtab(5) =  -0.17364817766693D+00
    xtab(6) =   0.17364817766693D+00
    xtab(7) =   0.50000000000000D+00
    xtab(8) =   0.76604444311898D+00
    xtab(9) =   0.93969262078591D+00
    xtab(10) =  1.00000000000000D+00

    weight(1) =  0.01234567901235D+00
    weight(2) =  0.11656745657204D+00
    weight(3) =  0.22528432333810D+00
    weight(4) =  0.30194003527337D+00
    weight(5) =  0.34386250580414D+00
    weight(6) =  0.34386250580414D+00
    weight(7) =  0.30194003527337D+00
    weight(8) =  0.22528432333810D+00
    weight(9) =  0.11656745657204D+00
    weight(10) = 0.01234567901235D+00

  else if ( order == 11 ) then

    xtab(1) =  -1.00000000000000D+00
    xtab(2) =  -0.95105651629515D+00
    xtab(3) =  -0.80901699437495D+00
    xtab(4) =  -0.58778525229247D+00
    xtab(5) =  -0.30901699437495D+00
    xtab(6) =   0.00000000000000D+00
    xtab(7) =   0.30901699437495D+00
    xtab(8) =   0.58778525229247D+00
    xtab(9) =   0.80901699437495D+00
    xtab(10) =  0.95105651629515D+00
    xtab(11) =  1.00000000000000D+00

    weight(1) =  0.01010101010101D+00
    weight(2) =  0.09457905488370D+00
    weight(3) =  0.18563521442425D+00
    weight(4) =  0.25358833328369D+00
    weight(5) =  0.29921327042424D+00
    weight(6) =  0.31376623376623D+00
    weight(7) =  0.29921327042424D+00
    weight(8) =  0.25358833328369D+00
    weight(9) =  0.18563521442425D+00
    weight(10) = 0.09457905488370D+00
    weight(11) = 0.01010101010101D+00

  else if ( order == 12 ) then

    xtab(1)  = -1.00000000000000D+00
    xtab(2)  = -0.95949297361450D+00
    xtab(3)  = -0.84125353283118D+00
    xtab(4)  = -0.65486073394528D+00
    xtab(5)  = -0.41541501300189D+00
    xtab(6)  = -0.14231483827329D+00
    xtab(7)  =  0.14231483827329D+00
    xtab(8)  =  0.41541501300189D+00
    xtab(9)  =  0.65486073394529D+00
    xtab(10) =  0.84125353283118D+00
    xtab(11) =  0.95949297361450D+00
    xtab(12) =  1.00000000000000D+00

    weight(1)  = 0.00826446280992D+00
    weight(2)  = 0.07856015374620D+00
    weight(3)  = 0.15504045508256D+00
    weight(4)  = 0.21556254600087D+00
    weight(5)  = 0.25991734106692D+00
    weight(6)  = 0.28265504129354D+00
    weight(7)  = 0.28265504129354D+00
    weight(8)  = 0.25991734106692D+00
    weight(9)  = 0.21556254600087D+00
    weight(10) = 0.15504045508256D+00
    weight(11) = 0.07856015374620D+00
    weight(12) = 0.00826446280992D+00

  else if ( order == 13 ) then

    xtab(1)  = -1.00000000000000D+00
    xtab(2)  = -0.96592582628907D+00
    xtab(3)  = -0.86602540378444D+00
    xtab(4)  = -0.70710678118655D+00
    xtab(5)  = -0.50000000000000D+00
    xtab(6)  = -0.25881904510252D+00
    xtab(7)  =  0.00000000000000D+00
    xtab(8)  =  0.25881904510252D+00
    xtab(9)  =  0.50000000000000D+00
    xtab(10) =  0.70710678118655D+00
    xtab(11) =  0.86602540378444D+00
    xtab(12) =  0.96592582628907D+00
    xtab(13) =  1.00000000000000D+00

    weight(1)  = 0.00699300699301D+00
    weight(2)  = 0.06605742495207D+00
    weight(3)  = 0.13154253154253D+00
    weight(4)  = 0.18476338476338D+00
    weight(5)  = 0.22697302697303D+00
    weight(6)  = 0.25267569378104D+00
    weight(7)  = 0.26198986198986D+00
    weight(8)  = 0.25267569378104D+00
    weight(9)  = 0.22697302697303D+00
    weight(10) = 0.18476338476338D+00
    weight(11) = 0.13154253154253D+00
    weight(12) = 0.06605742495207D+00
    weight(13) = 0.00699300699301D+00

  else if ( order == 14 ) then

    xtab(1)  = -1.00000000000000D+00
    xtab(2)  = -0.97094181742605D+00
    xtab(3)  = -0.88545602565321D+00
    xtab(4)  = -0.74851074817110D+00
    xtab(5)  = -0.56806474673116D+00
    xtab(6)  = -0.35460488704254D+00
    xtab(7)  = -0.12053668025532D+00
    xtab(8)  =  0.12053668025532D+00
    xtab(9)  =  0.35460488704254D+00
    xtab(10) =  0.56806474673116D+00
    xtab(11) =  0.74851074817110D+00
    xtab(12) =  0.88545602565321D+00
    xtab(13) =  0.97094181742605D+00
    xtab(14) =  1.00000000000000D+00

    weight(1)  = 0.00591715976331D+00
    weight(2)  = 0.05646531376341D+00
    weight(3)  = 0.11276867248986D+00
    weight(4)  = 0.16003802611672D+00
    weight(5)  = 0.19899241036578D+00
    weight(6)  = 0.22590304977856D+00
    weight(7)  = 0.23991536772235D+00
    weight(8)  = 0.23991536772235D+00
    weight(9)  = 0.22590304977856D+00
    weight(10) = 0.19899241036578D+00
    weight(11) = 0.16003802611672D+00
    weight(12) = 0.11276867248986D+00
    weight(13) = 0.05646531376341D+00
    weight(14) = 0.00591715976331D+00

  else if ( order == 15 ) then

    xtab(1)  = -1.00000000000000D+00
    xtab(2)  = -0.97492791218182D+00
    xtab(3)  = -0.90096886790242D+00
    xtab(4)  = -0.78183148246803D+00
    xtab(5)  = -0.62348980185873D+00
    xtab(6)  = -0.43388373911756D+00
    xtab(7)  = -0.22252093395631D+00
    xtab(8)  =  0.00000000000000D+00
    xtab(9)  =  0.22252093395631D+00
    xtab(10) =  0.43388373911756D+00
    xtab(11) =  0.62348980185873D+00
    xtab(12) =  0.78183148246803D+00
    xtab(13) =  0.90096886790242D+00
    xtab(14) =  0.97492791218182D+00
    xtab(15) =  1.00000000000000D+00

    weight(1)  = 0.00512820512821D+00
    weight(2)  = 0.04869938729509D+00
    weight(3)  = 0.09782039167605D+00
    weight(4)  = 0.13966507849560D+00
    weight(5)  = 0.17560578900107D+00
    weight(6)  = 0.20205146748238D+00
    weight(7)  = 0.21888151163057D+00
    weight(8)  = 0.22429633858205D+00
    weight(9)  = 0.21888151163057D+00
    weight(10) = 0.20205146748238D+00
    weight(11) = 0.17560578900107D+00
    weight(12) = 0.13966507849560D+00
    weight(13) = 0.09782039167605D+00
    weight(14) = 0.04869938729509D+00
    weight(15) = 0.00512820512821D+00

  else if ( order == 16 ) then

    xtab(1)  = -1.00000000000000D+00
    xtab(2)  = -0.97814760073381D+00
    xtab(3)  = -0.91354545764260D+00
    xtab(4)  = -0.80901699437495D+00
    xtab(5)  = -0.66913060635886D+00
    xtab(6)  = -0.50000000000000D+00
    xtab(7)  = -0.30901699437495D+00
    xtab(8)  = -0.10452846326765D+00
    xtab(9)  =  0.10452846326765D+00
    xtab(10) =  0.30901699437495D+00
    xtab(11) =  0.50000000000000D+00
    xtab(12) =  0.66913060635886D+00
    xtab(13) =  0.80901699437495D+00
    xtab(14) =  0.91354545764260D+00
    xtab(15) =  0.97814760073381D+00
    xtab(16) =  1.00000000000000D+00

    weight(1)  = 0.00444444444444D+00
    weight(2)  = 0.04251476624753D+00
    weight(3)  = 0.08553884025933D+00
    weight(4)  = 0.12294010082849D+00
    weight(5)  = 0.15573317603967D+00
    weight(6)  = 0.18132978132978D+00
    weight(7)  = 0.19921478132639D+00
    weight(8)  = 0.20828410952436D+00
    weight(9)  = 0.20828410952436D+00
    weight(10) = 0.19921478132639D+00
    weight(11) = 0.18132978132978D+00
    weight(12) = 0.15573317603967D+00
    weight(13) = 0.12294010082849D+00
    weight(14) = 0.08553884025933D+00
    weight(15) = 0.04251476624753D+00
    weight(16) = 0.00444444444444D+00

  else if ( order == 17 ) then

    xtab(1) = -1.00000000000000D+00
    xtab(2) = -0.98078528040323D+00
    xtab(3) = -0.92387953251129D+00
    xtab(4) = -0.83146961230255D+00
    xtab(5) = -0.70710678118655D+00
    xtab(6) = -0.55557023301960D+00
    xtab(7) = -0.38268343236509D+00
    xtab(8) = -0.19509032201613D+00
    xtab(9) =  0.00000000000000D+00
    xtab(10) =  0.19509032201613D+00
    xtab(11) =  0.38268343236509D+00
    xtab(12) =  0.55557023301960D+00
    xtab(13) =  0.70710678118655D+00
    xtab(14) =  0.83146961230255D+00
    xtab(15) =  0.92387953251129D+00
    xtab(16) =  0.98078528040323D+00
    xtab(17) =  1.00000000000000D+00

    weight(1) =  0.00392156862745D+00
    weight(2) =  0.03736870283721D+00
    weight(3) =  0.07548233154315D+00
    weight(4) =  0.10890555258189D+00
    weight(5) =  0.13895646836823D+00
    weight(6) =  0.16317266428170D+00
    weight(7) =  0.18147378423649D+00
    weight(8) =  0.19251386461293D+00
    weight(9) =  0.19641012582189D+00
    weight(10) =  0.19251386461293D+00
    weight(11) =  0.18147378423649D+00
    weight(12) =  0.16317266428170D+00
    weight(13) =  0.13895646836823D+00
    weight(14) =  0.10890555258189D+00
    weight(15) =  0.07548233154315D+00
    weight(16) =  0.03736870283721D+00
    weight(17) =  0.00392156862745D+00

  else if ( order == 33 ) then

    xtab(1) = -1.00000000000000D+00
    xtab(2) = -0.99518472667220D+00
    xtab(3) = -0.98078528040323D+00
    xtab(4) = -0.95694033573221D+00
    xtab(5) = -0.92387953251129D+00
    xtab(6) = -0.88192126434835D+00
    xtab(7) = -0.83146961230255D+00
    xtab(8) = -0.77301045336274D+00
    xtab(9) = -0.70710678118655D+00
    xtab(10) = -0.63439328416365D+00
    xtab(11) = -0.55557023301960D+00
    xtab(12) = -0.47139673682600D+00
    xtab(13) = -0.38268343236509D+00
    xtab(14) = -0.29028467725446D+00
    xtab(15) = -0.19509032201613D+00
    xtab(16) = -0.09801714032956D+00
    xtab(17) =  0.00000000000000D+00
    xtab(18) =  0.09801714032956D+00
    xtab(19) =  0.19509032201613D+00
    xtab(20) =  0.29028467725446D+00
    xtab(21) =  0.38268343236509D+00
    xtab(22) =  0.47139673682600D+00
    xtab(23) =  0.55557023301960D+00
    xtab(24) =  0.63439328416365D+00
    xtab(25) =  0.70710678118655D+00
    xtab(26) =  0.77301045336274D+00
    xtab(27) =  0.83146961230255D+00
    xtab(28) =  0.88192126434836D+00
    xtab(29) =  0.92387953251129D+00
    xtab(30) =  0.95694033573221D+00
    xtab(31) =  0.98078528040323D+00
    xtab(32) =  0.99518472667220D+00
    xtab(33) =  1.00000000000000D+00

    weight(1) =  0.00097751710655D+00
    weight(2) =  0.00939319796296D+00
    weight(3) =  0.01923424513268D+00
    weight(4) =  0.02845791667723D+00
    weight(5) =  0.03759434191405D+00
    weight(6) =  0.04626276283775D+00
    weight(7) =  0.05455501630398D+00
    weight(8) =  0.06227210954529D+00
    weight(9) =  0.06942757563044D+00
    weight(10) =  0.07588380044139D+00
    weight(11) =  0.08163481765494D+00
    weight(12) =  0.08657753844183D+00
    weight(13) =  0.09070611286772D+00
    weight(14) =  0.09394324443877D+00
    weight(15) =  0.09629232594549D+00
    weight(16) =  0.09769818820806D+00
    weight(17) =  0.09817857778177D+00
    weight(18) =  0.09769818820806D+00
    weight(19) =  0.09629232594549D+00
    weight(20) =  0.09394324443877D+00
    weight(21) =  0.09070611286772D+00
    weight(22) =  0.08657753844183D+00
    weight(23) =  0.08163481765494D+00
    weight(24) =  0.07588380044139D+00
    weight(25) =  0.06942757563044D+00
    weight(26) =  0.06227210954529D+00
    weight(27) =  0.05455501630398D+00
    weight(28) =  0.04626276283775D+00
    weight(29) =  0.03759434191405D+00
    weight(30) =  0.02845791667723D+00
    weight(31) =  0.01923424513268D+00
    weight(32) =  0.00939319796296D+00
    weight(33) =  0.00097751710655D+00

  else if ( order == 65 ) then

    xtab(1) = -1.00000000000000D+00
    xtab(2) = -0.99879545620517D+00
    xtab(3) = -0.99518472667220D+00
    xtab(4) = -0.98917650996478D+00
    xtab(5) = -0.98078528040323D+00
    xtab(6) = -0.97003125319454D+00
    xtab(7) = -0.95694033573221D+00
    xtab(8) = -0.94154406518302D+00
    xtab(9) = -0.92387953251129D+00
    xtab(10) = -0.90398929312344D+00
    xtab(11) = -0.88192126434835D+00
    xtab(12) = -0.85772861000027D+00
    xtab(13) = -0.83146961230255D+00
    xtab(14) = -0.80320753148064D+00
    xtab(15) = -0.77301045336274D+00
    xtab(16) = -0.74095112535496D+00
    xtab(17) = -0.70710678118655D+00
    xtab(18) = -0.67155895484702D+00
    xtab(19) = -0.63439328416365D+00
    xtab(20) = -0.59569930449243D+00
    xtab(21) = -0.55557023301960D+00
    xtab(22) = -0.51410274419322D+00
    xtab(23) = -0.47139673682600D+00
    xtab(24) = -0.42755509343028D+00
    xtab(25) = -0.38268343236509D+00
    xtab(26) = -0.33688985339222D+00
    xtab(27) = -0.29028467725446D+00
    xtab(28) = -0.24298017990326D+00
    xtab(29) = -0.19509032201613D+00
    xtab(30) = -0.14673047445536D+00
    xtab(31) = -0.09801714032956D+00
    xtab(32) = -0.04906767432742D+00
    xtab(33) =  0.00000000000000D+00
    xtab(34) =  0.04906767432742D+00
    xtab(35) =  0.09801714032956D+00
    xtab(36) =  0.14673047445536D+00
    xtab(37) =  0.19509032201613D+00
    xtab(38) =  0.24298017990326D+00
    xtab(39) =  0.29028467725446D+00
    xtab(40) =  0.33688985339222D+00
    xtab(41) =  0.38268343236509D+00
    xtab(42) =  0.42755509343028D+00
    xtab(43) =  0.47139673682600D+00
    xtab(44) =  0.51410274419322D+00
    xtab(45) =  0.55557023301960D+00
    xtab(46) =  0.59569930449243D+00
    xtab(47) =  0.63439328416365D+00
    xtab(48) =  0.67155895484702D+00
    xtab(49) =  0.70710678118655D+00
    xtab(50) =  0.74095112535496D+00
    xtab(51) =  0.77301045336274D+00
    xtab(52) =  0.80320753148064D+00
    xtab(53) =  0.83146961230255D+00
    xtab(54) =  0.85772861000027D+00
    xtab(55) =  0.88192126434836D+00
    xtab(56) =  0.90398929312344D+00
    xtab(57) =  0.92387953251129D+00
    xtab(58) =  0.94154406518302D+00
    xtab(59) =  0.95694033573221D+00
    xtab(60) =  0.97003125319454D+00
    xtab(61) =  0.98078528040323D+00
    xtab(62) =  0.98917650996478D+00
    xtab(63) =  0.99518472667220D+00
    xtab(64) =  0.99879545620517D+00
    xtab(65) =  1.00000000000000D+00

    weight(1) =  0.00024420024420D+00
    weight(2) =  0.00235149067531D+00
    weight(3) =  0.00483146544879D+00
    weight(4) =  0.00719269316174D+00
    weight(5) =  0.00958233879528D+00
    weight(6) =  0.01192339471421D+00
    weight(7) =  0.01425206043235D+00
    weight(8) =  0.01653498765729D+00
    weight(9) =  0.01878652974180D+00
    weight(10) =  0.02098627442974D+00
    weight(11) =  0.02314069493436D+00
    weight(12) =  0.02523506498175D+00
    weight(13) =  0.02727225714147D+00
    weight(14) =  0.02924065319747D+00
    weight(15) =  0.03114129710407D+00
    weight(16) =  0.03296454656998D+00
    weight(17) =  0.03471049818093D+00
    weight(18) =  0.03637092028664D+00
    weight(19) =  0.03794545992128D+00
    weight(20) =  0.03942698871296D+00
    weight(21) =  0.04081501340036D+00
    weight(22) =  0.04210333111142D+00
    weight(23) =  0.04329151496169D+00
    weight(24) =  0.04437417923926D+00
    weight(25) =  0.04535110955166D+00
    weight(26) =  0.04621766751093D+00
    weight(27) =  0.04697395904661D+00
    weight(28) =  0.04761604458525D+00
    weight(29) =  0.04814443257251D+00
    weight(30) =  0.04855584485714D+00
    weight(31) =  0.04885125664307D+00
    weight(32) =  0.04902801843103D+00
    weight(33) =  0.04908762351494D+00
    weight(34) =  0.04902801843103D+00
    weight(35) =  0.04885125664307D+00
    weight(36) =  0.04855584485714D+00
    weight(37) =  0.04814443257251D+00
    weight(38) =  0.04761604458525D+00
    weight(39) =  0.04697395904661D+00
    weight(40) =  0.04621766751093D+00
    weight(41) =  0.04535110955166D+00
    weight(42) =  0.04437417923926D+00
    weight(43) =  0.04329151496169D+00
    weight(44) =  0.04210333111142D+00
    weight(45) =  0.04081501340036D+00
    weight(46) =  0.03942698871296D+00
    weight(47) =  0.03794545992128D+00
    weight(48) =  0.03637092028664D+00
    weight(49) =  0.03471049818093D+00
    weight(50) =  0.03296454656998D+00
    weight(51) =  0.03114129710407D+00
    weight(52) =  0.02924065319747D+00
    weight(53) =  0.02727225714147D+00
    weight(54) =  0.02523506498175D+00
    weight(55) =  0.02314069493436D+00
    weight(56) =  0.02098627442974D+00
    weight(57) =  0.01878652974180D+00
    weight(58) =  0.01653498765729D+00
    weight(59) =  0.01425206043235D+00
    weight(60) =  0.01192339471421D+00
    weight(61) =  0.00958233879528D+00
    weight(62) =  0.00719269316174D+00
    weight(63) =  0.00483146544879D+00
    weight(64) =  0.00235149067531D+00
    weight(65) =  0.00024420024420D+00

  else if ( order == 129 ) then

    xtab(1) = -1.00000000000000D+00
    xtab(2) = -0.99969881869620D+00
    xtab(3) = -0.99879545620517D+00
    xtab(4) = -0.99729045667869D+00
    xtab(5) = -0.99518472667220D+00
    xtab(6) = -0.99247953459871D+00
    xtab(7) = -0.98917650996478D+00
    xtab(8) = -0.98527764238894D+00
    xtab(9) = -0.98078528040323D+00
    xtab(10) = -0.97570213003853D+00
    xtab(11) = -0.97003125319454D+00
    xtab(12) = -0.96377606579544D+00
    xtab(13) = -0.95694033573221D+00
    xtab(14) = -0.94952818059304D+00
    xtab(15) = -0.94154406518302D+00
    xtab(16) = -0.93299279883474D+00
    xtab(17) = -0.92387953251129D+00
    xtab(18) = -0.91420975570353D+00
    xtab(19) = -0.90398929312344D+00
    xtab(20) = -0.89322430119552D+00
    xtab(21) = -0.88192126434835D+00
    xtab(22) = -0.87008699110871D+00
    xtab(23) = -0.85772861000027D+00
    xtab(24) = -0.84485356524971D+00
    xtab(25) = -0.83146961230255D+00
    xtab(26) = -0.81758481315158D+00
    xtab(27) = -0.80320753148064D+00
    xtab(28) = -0.78834642762661D+00
    xtab(29) = -0.77301045336274D+00
    xtab(30) = -0.75720884650648D+00
    xtab(31) = -0.74095112535496D+00
    xtab(32) = -0.72424708295147D+00
    xtab(33) = -0.70710678118655D+00
    xtab(34) = -0.68954054473707D+00
    xtab(35) = -0.67155895484702D+00
    xtab(36) = -0.65317284295378D+00
    xtab(37) = -0.63439328416365D+00
    xtab(38) = -0.61523159058063D+00
    xtab(39) = -0.59569930449243D+00
    xtab(40) = -0.57580819141785D+00
    xtab(41) = -0.55557023301960D+00
    xtab(42) = -0.53499761988710D+00
    xtab(43) = -0.51410274419322D+00
    xtab(44) = -0.49289819222978D+00
    xtab(45) = -0.47139673682600D+00
    xtab(46) = -0.44961132965461D+00
    xtab(47) = -0.42755509343028D+00
    xtab(48) = -0.40524131400499D+00
    xtab(49) = -0.38268343236509D+00
    xtab(50) = -0.35989503653499D+00
    xtab(51) = -0.33688985339222D+00
    xtab(52) = -0.31368174039889D+00
    xtab(53) = -0.29028467725446D+00
    xtab(54) = -0.26671275747490D+00
    xtab(55) = -0.24298017990326D+00
    xtab(56) = -0.21910124015687D+00
    xtab(57) = -0.19509032201613D+00
    xtab(58) = -0.17096188876030D+00
    xtab(59) = -0.14673047445536D+00
    xtab(60) = -0.12241067519922D+00
    xtab(61) = -0.09801714032956D+00
    xtab(62) = -0.07356456359967D+00
    xtab(63) = -0.04906767432742D+00
    xtab(64) = -0.02454122852291D+00
    xtab(65) =  0.00000000000000D+00
    xtab(66) =  0.02454122852291D+00
    xtab(67) =  0.04906767432742D+00
    xtab(68) =  0.07356456359967D+00
    xtab(69) =  0.09801714032956D+00
    xtab(70) =  0.12241067519922D+00
    xtab(71) =  0.14673047445536D+00
    xtab(72) =  0.17096188876030D+00
    xtab(73) =  0.19509032201613D+00
    xtab(74) =  0.21910124015687D+00
    xtab(75) =  0.24298017990326D+00
    xtab(76) =  0.26671275747490D+00
    xtab(77) =  0.29028467725446D+00
    xtab(78) =  0.31368174039889D+00
    xtab(79) =  0.33688985339222D+00
    xtab(80) =  0.35989503653499D+00
    xtab(81) =  0.38268343236509D+00
    xtab(82) =  0.40524131400499D+00
    xtab(83) =  0.42755509343028D+00
    xtab(84) =  0.44961132965461D+00
    xtab(85) =  0.47139673682600D+00
    xtab(86) =  0.49289819222978D+00
    xtab(87) =  0.51410274419322D+00
    xtab(88) =  0.53499761988710D+00
    xtab(89) =  0.55557023301960D+00
    xtab(90) =  0.57580819141785D+00
    xtab(91) =  0.59569930449243D+00
    xtab(92) =  0.61523159058063D+00
    xtab(93) =  0.63439328416365D+00
    xtab(94) =  0.65317284295378D+00
    xtab(95) =  0.67155895484702D+00
    xtab(96) =  0.68954054473707D+00
    xtab(97) =  0.70710678118655D+00
    xtab(98) =  0.72424708295147D+00
    xtab(99) =  0.74095112535496D+00
    xtab(100) =  0.75720884650648D+00
    xtab(101) =  0.77301045336274D+00
    xtab(102) =  0.78834642762661D+00
    xtab(103) =  0.80320753148064D+00
    xtab(104) =  0.81758481315158D+00
    xtab(105) =  0.83146961230255D+00
    xtab(106) =  0.84485356524971D+00
    xtab(107) =  0.85772861000027D+00
    xtab(108) =  0.87008699110871D+00
    xtab(109) =  0.88192126434836D+00
    xtab(110) =  0.89322430119552D+00
    xtab(111) =  0.90398929312344D+00
    xtab(112) =  0.91420975570353D+00
    xtab(113) =  0.92387953251129D+00
    xtab(114) =  0.93299279883474D+00
    xtab(115) =  0.94154406518302D+00
    xtab(116) =  0.94952818059304D+00
    xtab(117) =  0.95694033573221D+00
    xtab(118) =  0.96377606579544D+00
    xtab(119) =  0.97003125319454D+00
    xtab(120) =  0.97570213003853D+00
    xtab(121) =  0.98078528040323D+00
    xtab(122) =  0.98527764238894D+00
    xtab(123) =  0.98917650996478D+00
    xtab(124) =  0.99247953459871D+00
    xtab(125) =  0.99518472667220D+00
    xtab(126) =  0.99729045667869D+00
    xtab(127) =  0.99879545620517D+00
    xtab(128) =  0.99969881869620D+00
    xtab(129) =  1.00000000000000D+00

    weight(1) =  0.00006103888177D+00
    weight(2) =  0.00058807215383D+00
    weight(3) =  0.00120930061875D+00
    weight(4) =  0.00180308126695D+00
    weight(5) =  0.00240715327877D+00
    weight(6) =  0.00300345869904D+00
    weight(7) =  0.00360197835813D+00
    weight(8) =  0.00419553798719D+00
    weight(9) =  0.00478862143341D+00
    weight(10) =  0.00537724746840D+00
    weight(11) =  0.00596388034731D+00
    weight(12) =  0.00654590843862D+00
    weight(13) =  0.00712483332325D+00
    weight(14) =  0.00769875778896D+00
    weight(15) =  0.00826865154203D+00
    weight(16) =  0.00883303867470D+00
    weight(17) =  0.00939256583935D+00
    weight(18) =  0.00994602784923D+00
    weight(19) =  0.01049386202577D+00
    weight(20) =  0.01103504877427D+00
    weight(21) =  0.01156988348291D+00
    weight(22) =  0.01209748052807D+00
    weight(23) =  0.01261803597978D+00
    weight(24) =  0.01313076516694D+00
    weight(25) =  0.01363579321294D+00
    weight(26) =  0.01413241437853D+00
    weight(27) =  0.01462070254634D+00
    weight(28) =  0.01510001572479D+00
    weight(29) =  0.01557039073899D+00
    weight(30) =  0.01603123858745D+00
    weight(31) =  0.01648256956220D+00
    weight(32) =  0.01692383985846D+00
    weight(33) =  0.01735504125411D+00
    weight(34) =  0.01777566938875D+00
    weight(35) =  0.01818570377926D+00
    weight(36) =  0.01858467519567D+00
    weight(37) =  0.01897255587068D+00
    weight(38) =  0.01934890842392D+00
    weight(39) =  0.01971370183700D+00
    weight(40) =  0.02006652805198D+00
    weight(41) =  0.02040735612004D+00
    weight(42) =  0.02073580533490D+00
    weight(43) =  0.02105184759002D+00
    weight(44) =  0.02135512797426D+00
    weight(45) =  0.02164562356713D+00
    weight(46) =  0.02192300400599D+00
    weight(47) =  0.02218725355897D+00
    weight(48) =  0.02243806539723D+00
    weight(49) =  0.02267543270457D+00
    weight(50) =  0.02289907134391D+00
    weight(51) =  0.02310898491627D+00
    weight(52) =  0.02330491126131D+00
    weight(53) =  0.02348686571193D+00
    weight(54) =  0.02365460746058D+00
    weight(55) =  0.02380816473024D+00
    weight(56) =  0.02394731750477D+00
    weight(57) =  0.02407210792328D+00
    weight(58) =  0.02418233623893D+00
    weight(59) =  0.02427805942076D+00
    weight(60) =  0.02435909748928D+00
    weight(61) =  0.02442552306157D+00
    weight(62) =  0.02447717542743D+00
    weight(63) =  0.02451414358882D+00
    weight(64) =  0.02453628559651D+00
    weight(65) =  0.02454370750551D+00
    weight(66) =  0.02453628559651D+00
    weight(67) =  0.02451414358882D+00
    weight(68) =  0.02447717542743D+00
    weight(69) =  0.02442552306157D+00
    weight(70) =  0.02435909748928D+00
    weight(71) =  0.02427805942076D+00
    weight(72) =  0.02418233623893D+00
    weight(73) =  0.02407210792328D+00
    weight(74) =  0.02394731750477D+00
    weight(75) =  0.02380816473024D+00
    weight(76) =  0.02365460746058D+00
    weight(77) =  0.02348686571193D+00
    weight(78) =  0.02330491126131D+00
    weight(79) =  0.02310898491627D+00
    weight(80) =  0.02289907134391D+00
    weight(81) =  0.02267543270457D+00
    weight(82) =  0.02243806539723D+00
    weight(83) =  0.02218725355897D+00
    weight(84) =  0.02192300400599D+00
    weight(85) =  0.02164562356713D+00
    weight(86) =  0.02135512797426D+00
    weight(87) =  0.02105184759002D+00
    weight(88) =  0.02073580533490D+00
    weight(89) =  0.02040735612004D+00
    weight(90) =  0.02006652805198D+00
    weight(91) =  0.01971370183700D+00
    weight(92) =  0.01934890842392D+00
    weight(93) =  0.01897255587068D+00
    weight(94) =  0.01858467519567D+00
    weight(95) =  0.01818570377926D+00
    weight(96) =  0.01777566938875D+00
    weight(97) =  0.01735504125411D+00
    weight(98) =  0.01692383985846D+00
    weight(99) =  0.01648256956220D+00
    weight(100) =  0.01603123858745D+00
    weight(101) =  0.01557039073899D+00
    weight(102) =  0.01510001572479D+00
    weight(103) =  0.01462070254634D+00
    weight(104) =  0.01413241437853D+00
    weight(105) =  0.01363579321294D+00
    weight(106) =  0.01313076516694D+00
    weight(107) =  0.01261803597978D+00
    weight(108) =  0.01209748052807D+00
    weight(109) =  0.01156988348291D+00
    weight(110) =  0.01103504877427D+00
    weight(111) =  0.01049386202577D+00
    weight(112) =  0.00994602784923D+00
    weight(113) =  0.00939256583935D+00
    weight(114) =  0.00883303867470D+00
    weight(115) =  0.00826865154203D+00
    weight(116) =  0.00769875778896D+00
    weight(117) =  0.00712483332325D+00
    weight(118) =  0.00654590843862D+00
    weight(119) =  0.00596388034731D+00
    weight(120) =  0.00537724746840D+00
    weight(121) =  0.00478862143341D+00
    weight(122) =  0.00419553798719D+00
    weight(123) =  0.00360197835813D+00
    weight(124) =  0.00300345869904D+00
    weight(125) =  0.00240715327877D+00
    weight(126) =  0.00180308126695D+00
    weight(127) =  0.00120930061875D+00
    weight(128) =  0.00058807215383D+00
    weight(129) =  0.00006103888177D+00

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CLENSHAW_CURTIS_SET - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal value of ORDER = ', order
    write ( *, '(a)' ) '  Legal values are 1 to 17, 33, 65 or 129'
    stop

  end if

  return
end
subroutine comp_next ( n, k, a, more, h, t )

!*****************************************************************************80
!
!! COMP_NEXT computes the compositions of the integer N into K parts.
!
!  Discussion:
!
!    A composition of the integer N into K parts is an ordered sequence
!    of K nonnegative integers which sum to N.  The compositions (1,2,1)
!    and (1,1,2) are considered to be distinct.
!
!    The routine computes one composition on each call until there are no more.
!    For instance, one composition of 6 into 3 parts is
!    3+2+1, another would be 6+0+0.
!
!    On the first call to this routine, set MORE = FALSE.  The routine
!    will compute the first element in the sequence of compositions, and
!    return it, as well as setting MORE = TRUE.  If more compositions
!    are desired, call again, and again.  Each time, the routine will
!    return with a new composition.
!
!    However, when the LAST composition in the sequence is computed 
!    and returned, the routine will reset MORE to FALSE, signaling that
!    the end of the sequence has been reached.
!
!    This routine originally used a SAVE statement to maintain the
!    variables H and T.  I have decided that it is safer
!    to pass these variables as arguments, even though the user should
!    never alter them.  This allows this routine to safely shuffle
!    between several ongoing calculations.
!
!
!    There are 28 compositions of 6 into three parts.  This routine will
!    produce those compositions in the following order:
!
!     I         A
!     -     ---------
!     1     6   0   0
!     2     5   1   0
!     3     4   2   0
!     4     3   3   0
!     5     2   4   0
!     6     1   5   0
!     7     0   6   0
!     8     5   0   1
!     9     4   1   1
!    10     3   2   1
!    11     2   3   1
!    12     1   4   1
!    13     0   5   1
!    14     4   0   2
!    15     3   1   2
!    16     2   2   2
!    17     1   3   2
!    18     0   4   2
!    19     3   0   3
!    20     2   1   3
!    21     1   2   3
!    22     0   3   3
!    23     2   0   4
!    24     1   1   4
!    25     0   2   4
!    26     1   0   5
!    27     0   1   5
!    28     0   0   6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 July 2008
!
!  Author:
!
!    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Second Edition,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the integer whose compositions are desired.
!
!    Input, integer ( kind = 4 ) K, the number of parts in the composition.
!
!    Input/output, integer ( kind = 4 ) A(K), the parts of the composition.
!
!    Input/output, logical MORE, set by the user to start the computation,
!    and by the routine to terminate it.
!
!    Input/output, integer ( kind = 4 )  H, T, two internal parameters needed 
!    for the computation.  The user should allocate space for these in the 
!    calling program, include them in the calling sequence, but never alter them!
!
  implicit none

  integer ( kind = 4 ) k

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ) h
  logical more
  integer ( kind = 4 ) n
  integer ( kind = 4 ) t
!
!  The first computation.
!
  if ( .not. more ) then

    t = n
    h = 0
    a(1) = n
    a(2:k) = 0
!
!  The next computation.
!
  else

    if ( 1 < t ) then
      h = 0
    end if

    h = h + 1
    t = a(h)
    a(h) = 0
    a(1) = t - 1
    a(h+1) = a(h+1) + 1

  end if
!
!  This is the last element of the sequence if all the
!  items are in the last slot.
!
  more = ( a(k) /= n )

  return
end
subroutine compnz_next ( n, k, a, more )

!*****************************************************************************80
!
!! COMPNZ_NEXT computes the compositions of the integer N into K nonzero parts.
!
!  Discussion:
!
!    A composition of the integer N into K nonzero parts is an ordered sequence
!    of K positive integers which sum to N.  The compositions (1,2,1)
!    and (1,1,2) are considered to be distinct.
!
!    The routine computes one composition on each call until there are no more.
!    For instance, one composition of 6 into 3 parts is 3+2+1, another would
!    be 4+1+1 but 5+1+0 is not allowed since it includes a zero part.
!
!    On the first call to this routine, set MORE = FALSE.  The routine
!    will compute the first element in the sequence of compositions, and
!    return it, as well as setting MORE = TRUE.  If more compositions
!    are desired, call again, and again.  Each time, the routine will
!    return with a new composition.
!
!    However, when the LAST composition in the sequence is computed 
!    and returned, the routine will reset MORE to FALSE, signaling that
!    the end of the sequence has been reached.
!
!  Example:
!
!    The 10 compositions of 6 into three nonzero parts are:
!
!      4 1 1,  3 2 1,  3 1 2,  2 3 1,  2 2 2,  2 1 3,  
!      1 4 1,  1 3 2,  1 2 3,  1 1 4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 December 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the integer whose compositions are desired.
!
!    Input, integer ( kind = 4 ) K, the number of parts in the composition.  
!    K must be less than or equal to N.
!
!    Input/output, integer ( kind = 4 ) A(K), the parts of the composition.
!
!    Input/output, logical MORE, set by the user to start the computation,
!    and by the routine to terminate it.
!
  implicit none

  integer ( kind = 4 ) k

  integer ( kind = 4 ) a(k)
  integer ( kind = 4 ), save :: h = 0
  logical more
  integer ( kind = 4 ) n
  integer ( kind = 4 ), save :: t = 0
!
!  We use the trick of computing ordinary compositions of (N-K)
!  into K parts, and adding 1 to each part.
!
  if ( n < k ) then
    more = .false.
    a(1:k) = -1
    return
  end if
!
!  The first computation.
!
  if ( .not. more ) then

    t = n - k
    h = 0
    a(1) = n - k
    a(2:k) = 0
!
!  The next computation.
!
  else

    a(1:k) = a(1:k) - 1

    if ( 1 < t ) then
      h = 0
    end if

    h = h + 1
    t = a(h)
    a(h) = 0
    a(1) = t - 1
    a(h+1) = a(h+1) + 1

  end if
!
!  This is the last element of the sequence if all the
!  items are in the last slot.
!
  more = ( a(k) /= ( n - k ) )

  a(1:k) = a(1:k) + 1

  return
end
subroutine dtable_write0 ( output_file_name, m, n, table )

!*****************************************************************************80
!
!! DTABLE_WRITE0 writes a DTABLE file with no headers.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 May 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) OUTPUT_FILE_NAME, the output file name.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) TABLE(M,N), the table data.
!
  implicit none

  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) n

  integer   ( kind = 4 ) j
  character ( len = * )  output_file_name
  integer   ( kind = 4 ) output_status
  integer   ( kind = 4 ) output_unit
  character ( len = 30 ) string
  real      ( kind = 8 ) table(m,n)
!
!  Open the file.
!
  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_file_name, &
    status = 'replace', iostat = output_status )

  if ( output_status /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DTABLE_WRITE0 - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the output file "' // &
      trim ( output_file_name ) // '" on unit ', output_unit
    output_unit = -1
    stop
  end if
!
!  Create a format string.
!
!  For greater precision in the output file, try:
!
!                                            '(', m, 'g', 24, '.', 16, ')'
!
  write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'g', 24, '.', 16, ')'
!
!  Write the data.
!
  do j = 1, n
    write ( output_unit, string ) table(1:m,j)
  end do
!
!  Close the file.
!
  close ( unit = output_unit )

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
function i4_modp ( i, j )

!*****************************************************************************80
!
!! I4_MODP returns the nonnegative remainder of I4 division.
!
!  Discussion:
!
!    If
!      NREM = I4_MODP ( I, J )
!      NMULT = ( I - NREM ) / J
!    then
!      I = J * NMULT + NREM
!    where NREM is always nonnegative.
!
!    The MOD function computes a result with the same sign as the
!    quantity being divided.  Thus, suppose you had an angle A,
!    and you wanted to ensure that it was between 0 and 360.
!    Then mod(A,360) would do, if A was positive, but if A
!    was negative, your result would be between -360 and 0.
!
!    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
!
!  Example:
!
!        I     J     MOD I4_MODP    Factorization
!
!      107    50       7       7    107 =  2 *  50 + 7
!      107   -50       7       7    107 = -2 * -50 + 7
!     -107    50      -7      43   -107 = -3 *  50 + 43
!     -107   -50      -7      43   -107 =  3 * -50 + 43
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the number to be divided.
!
!    Input, integer ( kind = 4 ) J, the number that divides I.
!
!    Output, integer ( kind = 4 ) I4_MODP, the nonnegative remainder when I is
!    divided by J.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) j
  integer ( kind = 4 ) value

  if ( j == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4_MODP - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal divisor J = ', j
    stop
  end if

  value = mod ( i, j )

  if ( value < 0 ) then
    value = value + abs ( j )
  end if

  i4_modp = value

  return
end
subroutine i4vec_uniform ( n, a, b, seed, x )

!*****************************************************************************80
!
!! I4VEC_UNIFORM returns a scaled pseudorandom I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of integer ( kind = 4 ) values.
!
!    The pseudorandom numbers should be scaled to be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 November 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vector.
!
!    Input, integer ( kind = 4 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, integer ( kind = 4 ) X(N), a vector of numbers between A and B.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real ( kind = 4 ) r
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) value
  integer ( kind = 4 ) x(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_UNIFORM - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + huge ( seed )
    end if

    r = real ( seed, kind = 4 ) * 4.656612875E-10
!
!  Scale R to lie between A-0.5 and B+0.5.
!
    r = ( 1.0E+00 - r ) * ( real ( min ( a, b ), kind = 4 ) - 0.5E+00 ) & 
      +             r   * ( real ( max ( a, b ), kind = 4 ) + 0.5E+00 )
!
!  Use rounding to convert R to an integer between A and B.
!
    value = nint ( r, kind = 4 )

    value = max ( value, min ( a, b ) )
    value = min ( value, max ( a, b ) )

    x(i) = value

  end do

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 )  ampm
  integer   ( kind = 4 ) d
  integer   ( kind = 4 ) h
  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer   ( kind = 4 ) n
  integer   ( kind = 4 ) s
  integer   ( kind = 4 ) values(8)
  integer   ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
subroutine vec_next_gray ( n, base, a, done, change )

!*****************************************************************************80
!
!! VEC_NEXT_GRAY computes the elements of a product space.
!
!  Discussion:
!
!    The elements are produced one at a time.
!
!    This routine handles the case where the number of degrees of freedom may
!    differ from one component to the next.
!
!    A method similar to the Gray code is used, so that successive
!    elements returned by this routine differ by only a single element.
!
!    The routine uses internal static memory.
!
!  Example:
!
!    N = 2, BASE = ( 2, 3 ), DONE = TRUE
!
!     A    DONE  CHANGE
!    ---  -----  ------
!    0 0  FALSE    1
!    0 1  FALSE    2
!    0 2  FALSE    2
!    1 2  FALSE    1
!    1 1  FALSE    2
!    1 0  FALSE    2
!    1 0   TRUE   -1  
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Dennis Stanton, Dennis White,
!    Constructive Combinatorics,
!    Springer, 1986,
!    ISBN: 0387963472.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, integer ( kind = 4 ) BASE(N), contains the number of degrees of
!    freedom of each component.  The output values of A will
!    satisfy 0 <= A(I) < BASE(I).
!
!    Input/output, integer ( kind = 4 ) A(N).  On the first call, the input 
!    value of A doesn't matter.  Thereafter, it should be the same as
!    its output value from the previous call.  On output, if DONE
!    is FALSE, then A contains the next element of the space.
!
!    Input/output, logical DONE.  On the first call, the user must
!    set DONE to TRUE.  This signals the program to initialize data.
!    On every return, if DONE is FALSE, the program has computed
!    another entry, which is contained in A.  If DONE is TRUE,
!    then there are no more entries, and the program should not be
!    called for any more.
!
!    Output, integer ( kind = 4 ) CHANGE, is set to the index of the element 
!    whose value was changed.  On return from the first call, CHANGE
!    is 1, even though all the elements have been "changed".  On
!    return with DONE equal to TRUE, CHANGE is -1.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ), save, allocatable, dimension ( : ) :: active
  integer ( kind = 4 ) base(n)
  integer ( kind = 4 ) change
  integer ( kind = 4 ), save, allocatable, dimension ( : ) :: dir
  logical done
  integer ( kind = 4 ) i
!
!  The user is calling for the first time.
!
  if ( done ) then

    done = .false.
    a(1:n) = 0

    if ( allocated ( active ) ) then
      deallocate ( active )
    end if

    if ( allocated ( dir ) ) then
      deallocate ( dir )
    end if

    allocate ( active(1:n) )
    allocate ( dir(1:n) )

    dir(1:n) = 1
    active(1:n) = 1

    do i = 1, n

      if ( base(i) < 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'VEC_NEXT_GRAY - Warning!'
        write ( *, '(a,i8)' ) '  For index I = ',i
        write ( *, '(a,i8)' ) '  the nonpositive value of BASE(I) = ', base(i)
        write ( *, '(a)' ) '  which was reset to 1!'
        base(i) = 1
        active(i) = 0
      else if ( base(i) == 1 ) then
        active(i) = 0
      end if

    end do

    change = 1

    return

  end if
!
!  Seek the maximum active index.
!
  change = -1

  do i = n, 1, -1
    if ( active(i) == 1 ) then
      change = i
      exit
    end if
  end do
!
!  If there are NO active indices, we have generated all vectors.
!
  if ( change == -1 ) then
    done = .true.
    deallocate ( active )
    deallocate ( dir )
    return
  end if
!
!  Increment the element with maximum active index.
!
  a(change) = a(change) + dir(change)
!
!  If we attained a minimum or maximum value, reverse the direction
!  vector, and deactivate the index.
!
  if ( a(change) == 0 .or. a(change) == base(change) - 1 ) then
    dir(change) = -dir(change)
    active(change) = 0
  end if
!
!  Activate all subsequent indices.
!
  do i = change + 1, n
    if ( 1 < base(i) ) then
      active(i) = 1
    end if
  end do

  return
end
subroutine vector_constrained_next4 ( n, alpha, x_min, x_max, x, q, more )

!*****************************************************************************80
!
!! VECTOR_CONSTRAINED_NEXT4 returns the "next" constrained vector.
!
!  Discussion:
!
!    This routine is similar to VECTOR_CONSTRAINED2 and VECTOR_CONSTRAINED3.
!
!    We consider all vectors X of dimension N whose components
!    satisfy X_MIN(1:N) <= X(1:N) <= X_MAX(1:N).
!
!    We are only interested in the subset of these vectors which
!    satisfy the following constraint:
!
!      sum ( 1 <= I <= N ) ALPHA(I) * X(I) <= Q
!
!    This routine returns, one at a time, and in right-to-left
!    lexicographic order, exactly those vectors which satisfy
!    the constraint.
!
!  Example:
!
!    N = 3
!    ALPHA    4.0  3.0  5.0
!    Q       20.0
!    X_MIN:   1   1   1
!    X_MAX:   5   6   4
!
!    #  X(1)  X(2)  X(3)     Total
!
!    1    1     1     1       12.0
!    2    2     1     1       16.0
!    3    3     1     1       20.0
!    4    1     2     1       15.0
!    5    2     2     1       19.0
!    6    1     3     1       18.0
!    7    1     1     2       17.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 May 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components in the vector.
!
!    Input, real ( kind = 8 ) ALPHA(N), the coefficient vector.
!
!    Input, integer ( kind = 4 ) X_MIN(N), X_MAX(N), the minimum and maximum
!    values allowed in each component.
!
!    Input/output, integer ( kind = 4 ) X(N).  On first call, with MORE = FALSE,
!    the input value of X is not important.  On subsequent calls, the
!    input value of X should be the output value from the previous call.
!    On output, (with MORE = TRUE), the value of X will be the "next"
!    vector in the reverse lexicographical list of vectors that satisfy
!    the condition.  However, on output with MORE = FALSE, the vector
!    X is meaningless, because there are no more vectors in the list.
!
!    Input, real ( kind = 8 ) Q, the limit on the sum.
!
!    Input/output, logical MORE.  On input, if the user has set MORE
!    FALSE, the user is requesting the initiation of a new sequence
!    of values.  If MORE is TRUE, then the user is requesting "more"
!    values in the current sequence.  On output, if MORE is TRUE,
!    then another value was found and returned in X, but if MORE is
!    FALSE, then there are no more values in the sequence, and X is
!    NOT the next value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) alpha(n)
  integer ( kind = 4 ) i
  logical more
  real ( kind = 8 ) q
  real ( kind = 8 ) total
  integer ( kind = 4 ) x(n)
  integer ( kind = 4 ) x_max(n)
  integer ( kind = 4 ) x_min(n)

  if ( .not. more ) then

    x(1:n) = x_min(1:n)

    total = dot_product ( alpha(1:n), real ( x(1:n), kind = 8 ) )

    if ( q < total ) then
      more = .false.
    else
      more = .true.
    end if

    return

  else

    i = 1

    do

      if ( x(i) < x_max(i) ) then

        x(i) = x(i) + 1

        total = dot_product ( alpha(1:n), real ( x(1:n), kind = 8 ) )

        if ( total <= q ) then
          exit
        end if

      end if

      x(i) = x_min(i)

      i = i + 1

      if ( n < i ) then
        more = .false.
        exit
      end if

    end do

  end if

  return
end
