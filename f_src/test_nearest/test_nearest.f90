program main

!*****************************************************************************80
!
!! TEST_NEAREST compares the performance of nearest neighbor routines.
!
!  Discussion:
!
!    We are given R, a set of NR points in M dimensions.
!
!    We are given S, a set of NS points in M dimensions.
!
!    For each S(I) in S, we seek the index J of the point R(J)
!    which is nearest to S(I) over all points in R.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Local parameters:
!
!    Local, integer ( kind = 4 ) M, the spatial dimension.
!
!    Local, integer ( kind = 4 ) NR, the number of data points.
!
!    Local, integer ( kind = 4 ) NS, the number of sample points.
!
!    Local, real ( kind = 8 ) R(M,NR), the data points.
!
!    Local, real ( kind = 8 ) RT(NR,M), the transposed data points.
!
!    Local, real ( kind = 8 ) S(M,NS), the sample points. 
!
!    Local, real ( kind = 8 ) ST(NS,M), the transposed sample points. 
!
  implicit none

  integer ( kind = 4 ), parameter :: m_test_num = 3
  integer ( kind = 4 ), parameter :: n_test_num = 6

  integer ( kind = 4 ) m
  integer ( kind = 4 ) m_test
  integer ( kind = 4 ), dimension ( m_test_num ) :: m_test_data = (/ 2, 4, 8 /)
  integer ( kind = 4 ), allocatable :: nearest(:)
  integer ( kind = 4 ) nr
  integer ( kind = 4 ), dimension ( n_test_num ) :: nr_test_data = (/ &
    1000000, 100000, 10000,  1000,    100,      10 /)
  integer ( kind = 4 ) ns
  integer ( kind = 4 ), dimension ( n_test_num ) :: ns_test_data = (/ &
      10,    100,  1000, 10000, 100000, 1000000 /)
  real ( kind = 8 ), allocatable :: r(:,:)
  real ( kind = 8 ), allocatable :: s(:,:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  integer ( kind = 4 ) test

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST_NEAREST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Consider various nearest neighbor algorithms.'

  do m_test = 1, m_test_num

    m = m_test_data(m_test);

    do test = 1, n_test_num

      nr = nr_test_data(test)
      allocate ( r(1:m,1:nr) )
      ns = ns_test_data(test)
      allocate ( s(1:m,1:ns) )
      allocate ( nearest(ns) )

      seed = 123456789
      call r8mat_uniform_01 ( m, ns, seed, s )
      call r8mat_uniform_01 ( m, nr, seed, r )

      write ( *, '(a)' ) ''
      write ( *, '(a,i8,a,i8,a,i8)' ) '  M = ', m, ' NR = ', nr, '  NS = ', ns

      call cpu_time ( t1 )
      call find_closest1 ( m, nr, r, ns, s, nearest )
      call cpu_time ( t2 )
      write ( *, '(a,g14.6,a,i8,a,i8)' ) &
        '  #1 time: ', t2 - t1, '  size = ', ns, '  i(1) = ', nearest(1)

      deallocate ( nearest )
      deallocate ( r )
      deallocate ( s )

    end do

  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST_NEAREST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  return
end
subroutine find_closest1 ( m, nr, r, ns, s, nearest )

!*****************************************************************************80
!
!! FIND_CLOSEST1 finds the nearest R point to each S point.
!
!  Discussion:
!
!    We are given R, a set of NR points in M dimensions.
!
!    We are given S, a set of NS points in M dimensions.
!
!    For each S(I) in S, we seek the index J of the point R(J)
!    which is nearest to S(I) over all points in R.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) NR, the number of data points.
!
!    Input, real ( kind = 8 ) R(M,NR), the data points.
!
!    Input, integer ( kind = 4 ) NS, the number of sample points.
!
!    Input, real ( kind = 8 ) S(M,NS), the sample points.
!
!    Output, integer ( kind = 4 ) NEAREST(NS), the index of the nearest 
!    data point.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) nr
  integer ( kind = 4 ) ns

  real ( kind = 8 ) dist_sq
  real ( kind = 8 ) dist_sq_min
  integer ( kind = 4 ) jr
  integer ( kind = 4 ) js
  integer ( kind = 4 ) nearest(ns)
  real ( kind = 8 ) r(m,nr)
  real ( kind = 8 ) s(m,ns)

  do js = 1, ns

    dist_sq_min = huge ( dist_sq_min )
    nearest(js) = -1

    do jr = 1, nr

      dist_sq = sum ( ( r(1:m,jr) - s(1:m,js) ) **2 )

      if ( dist_sq < dist_sq_min ) then
        dist_sq_min = dist_sq
        nearest(js) = jr
      end if

    end do

  end do

  return
end
subroutine r8mat_uniform_01 ( m, n, seed, r )

!*****************************************************************************80
!
!! R8MAT_UNIFORM_01 fills an R8MAT with unit pseudorandom numbers.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns in
!    the array.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(M,N), the array of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if

      r(i,j) = real ( seed, kind = 8 ) * 4.656612875D-10

    end do
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
