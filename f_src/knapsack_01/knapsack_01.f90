subroutine knapsack_01 ( n, w, c, s )

!*****************************************************************************80
!
!! KNAPSACK_01 seeks a solution of the 0/1 Knapsack problem.
!
!  Discussion:
!
!    In the 0/1 knapsack problem, a knapsack of capacity C is given,
!    as well as N items, with the I-th item of weight W(I).
!
!    A selection is "acceptable" if the total weight is no greater than C.
!
!    It is desired to find an optimal acceptable selection, that is,
!    an acceptable selection such that there is no acceptable selection
!    of greater weight.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of weights.
!
!    Input, integer ( kind = 4 ) W(N), the weights.
!
!    Input, integer ( kind = 4 ) C, the maximum weight.
!
!    Output, integer ( kind = 4 ) S(N), is a binary vector which defines an 
!    optimal selection.  It is 1 for the weights to be selected, and 
!    0 otherwise.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) c
  integer ( kind = 4 ) iadd
  logical ( kind = 4 ) more
  integer ( kind = 4 ) ncard
  integer ( kind = 4 ) s(n)
  integer ( kind = 4 ) s_test(n)
  integer ( kind = 4 ) t
  integer ( kind = 4 ) t_test
  integer ( kind = 4 ) w(n)

  more = .false.
  ncard = 0

  s_test(1:n) = 0
  t_test = 0

  s(1:n) = s_test(1:n)
  t = 0

  do

    call subset_gray_next ( n, s_test, more, ncard, iadd )
    t_test = dot_product ( s_test, w )

    if ( t < t_test .and. t_test <= c ) then
      t = t_test
      s(1:n) = s_test(1:n)
    end if

    if ( .not. more ) then
      exit
    end if

  end do

  return
end
subroutine subset_gray_next ( n, a, more, ncard, iadd )

!*****************************************************************************80
!
!! SUBSET_GRAY_NEXT generates all subsets of a set of order N, one at a time.
!
!  Discussion:
!
!    It generates the subsets one at a time, by adding or subtracting
!    exactly one element on each step.
!
!    This uses a Gray code ordering of the subsets.
!
!    The user should set MORE = FALSE and the value of N before
!    the first call.  On return, the user may examine A which contains
!    the definition of the new subset, and must check MORE, because
!    as soon as it is FALSE on return, all the subsets have been
!    generated and the user probably should cease calling.
!
!    The first set returned is the empty set.
!
!  Example:
!
!    N = 4
!
!    0 0 0 0
!    1 0 0 0
!    1 1 0 0
!    0 1 0 0
!    0 1 1 0
!    1 1 1 0
!    1 0 1 0
!    0 0 1 0
!    0 0 1 1
!    1 0 1 1
!    1 1 1 1
!    0 1 1 1
!    0 1 0 1
!    1 1 0 1
!    1 0 0 1
!    0 0 0 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2003
!
!  Author:
!
!    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
!    FORTRAN90 version by John Burkardt.
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
!    Input, integer ( kind = 4 ) N, the order of the total set from which
!    subsets will be drawn.
!
!    Input/output, integer ( kind = 4 ) A(N).  On each return, the Gray code
!    for the newly generated subset.  A(I) = 0 if element I is in the subset,
!    1 otherwise.
!
!    Input/output, logical MORE.  Set this variable FALSE before
!    the first call.  Normally, MORE will be returned TRUE but once
!    all the subsets have been generated, MORE will be
!    reset FALSE on return and you should stop calling the program.
!
!    Input/output, integer ( kind = 4 ) NCARD, the cardinality of the set
!    returned, which may be any value between 0 (the empty set) and N (the
!    whole set).
!
!    Output, integer ( kind = 4 ) IADD, the element which was added or removed
!    to the previous subset to generate the current one.  Exception:
!    the empty set is returned on the first call, and IADD is set to 0.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) iadd
  logical more
  integer ( kind = 4 ) ncard
!
!  The first set returned is the empty set.
!
  if ( .not. more ) then

    a(1:n) = 0

    iadd = 0
    ncard = 0
    more = .true.

  else

    iadd = 1

    if ( mod ( ncard, 2 ) /= 0 ) then

      do

        iadd = iadd + 1
        if ( a(iadd-1) /= 0 ) then
          exit
        end if

      end do

    end if

    a(iadd) = 1 - a(iadd)
    ncard = ncard + 2 * a(iadd) - 1
!
!  The last set returned is the singleton A(N).
!
    if ( ncard == a(n) ) then
      more = .false.
    end if

  end if

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
!    18 May 2013
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

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

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
