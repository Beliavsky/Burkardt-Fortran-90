subroutine stats ( x, n, x_sum, x_min, x_mean, x_max, x_var, x_std )

!*****************************************************************************80
!
!! stats() does stats.
!
!  Discussion:
!
!    This function computes statistical quantities for a sequence of values
!    X which are supplied one at a time, like a data stream.  The statistical
!    quantities are updated every time a new value of X is supplied.
!
!    Because we are dealing with a data stream, and not saving the previous
!    data values, it is not possible to compute the median and mode.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the next item in the sequence.
!    If X is set to Inf, then its value is ignored, and the current data
!    is returned.
!    If no argument is specified, then all internal data is reset to 0.
!
!  Output:
!
!    integer ( kind = 4 ) N: the number of items in the sequence.
!
!    real ( kind = 8 ) X_SUM: the sum of the values.
!
!    real ( kind = 8 ) X_MIN, X_MEAN, X_MAX: minimum, mean, and maximum.
!
!    real ( kind = 8 ) X_VAR, X_STD: the variance and standard deviation.
!
  integer ( kind = 4 ), optional :: n
  integer ( kind = 4 ), save :: n_internal = 0
  real ( kind = 8 ), optional :: x
  real ( kind = 8 ), optional :: x_max
  real ( kind = 8 ), save :: x_max_internal = 0.0
  real ( kind = 8 ), optional :: x_mean
  real ( kind = 8 ), save :: x_mean_internal = 0.0
  real ( kind = 8 ) :: x_mean_old
  real ( kind = 8 ), optional :: x_min
  real ( kind = 8 ), save :: x_min_internal = 0.0
  real ( kind = 8 ), optional :: x_std
  real ( kind = 8 ), save :: x_std_internal = 0.0
  real ( kind = 8 ), optional :: x_sum
  real ( kind = 8 ), save :: x_sum_internal = 0.0
  real ( kind = 8 ), optional :: x_var
  real ( kind = 8 ), save :: x_var_internal = 0.0
!
!  No input.  Reset internal data to 0.
!
  if ( .not. present ( x ) ) then
    n_internal = 0
    x_max_internal = 0.0
    x_mean_internal = 0.0
    x_min_internal = 0.0
    x_std_internal = 0.0
    x_sum_internal = 0.0
    x_var_internal = 0.0
!
!  Input that is not Inf.  Process X.
!
  else if ( x /= huge ( x ) ) then
    n_internal = n_internal + 1
    x_sum_internal = x_sum_internal + x
    x_mean_old = x_mean_internal
    x_mean_internal = x_sum_internal / n_internal
    if ( n_internal == 1 ) then
      x_max_internal = x
      x_min_internal = x
      x_var_internal = 0.0
      x_std_internal = 0.0
    else
      x_max_internal = max ( x_max_internal, x )
      x_min_internal = min ( x_min_internal, x )
      x_var_internal = ( x_var_internal * ( n_internal - 2 ) &
          + ( x - x_mean_old ) * ( x - x_mean_internal ) ) / ( n_internal - 1 )
      x_std_internal = sqrt ( x_var_internal )
    end if
  end if
!
!  Set output values.
!
  if ( present ( n ) ) then
    n      = n_internal
  end if
  if ( present ( x_max ) ) then
    x_max  = x_max_internal
  end if
  if ( present ( x_min ) ) then
    x_min  = x_min_internal
  end if
  if ( present ( x_sum ) ) then
    x_sum  = x_sum_internal
  end if
  if ( present ( x_mean ) ) then
    x_mean = x_mean_internal
  end if
  if ( present ( x_var ) ) then
    x_var  = x_var_internal
  end if
  if ( present ( x_std ) ) then
    x_std  = x_std_internal
  end if

  return
end

