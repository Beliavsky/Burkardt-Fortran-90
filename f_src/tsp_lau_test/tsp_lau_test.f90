program main

!*****************************************************************************80
!
!! MAIN is the main program for TSP_LAU_TEST.
!
!  Discussion:
!
!    TSP_LAU_TEST tests the TSP_LAU library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TSP_LAU_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the TSP_LAU library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TSP_LAU_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests TSP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 June 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 15

  real ( kind = 8 ) dist(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) isol(n)
  integer ( kind = 4 ), dimension ( n ) :: isol_exact = (/ &
    1, 13,  2, 15,  9, &
    5,  7,  3, 12, 14, &
   10,  8,  6,  4, 11 /)
  integer ( kind = 4 ) j
  real ( kind = 8 ) length

  save dist

  data ( ( dist(i,j), j = 1, n ), i = 1, n ) / &
    0.0D+00, 29.0D+00, 82.0D+00, 46.0D+00, 68.0D+00, &
   52.0D+00, 72.0D+00, 42.0D+00, 51.0D+00, 55.0D+00, &
   29.0D+00, 74.0D+00, 23.0D+00, 72.0D+00, 46.0D+00, &
   29.0D+00,  0.0D+00, 55.0D+00, 46.0D+00, 42.0D+00, &
   43.0D+00, 43.0D+00, 23.0D+00, 23.0D+00, 31.0D+00, &
   41.0D+00, 51.0D+00, 11.0D+00, 52.0D+00, 21.0D+00, &
   82.0D+00, 55.0D+00,  0.0D+00, 68.0D+00, 46.0D+00, &
   55.0D+00, 23.0D+00, 43.0D+00, 41.0D+00, 29.0D+00, &
   79.0D+00, 21.0D+00, 64.0D+00, 31.0D+00, 51.0D+00, &
   46.0D+00, 46.0D+00, 68.0D+00,  0.0D+00, 82.0D+00, &
   15.0D+00, 72.0D+00, 31.0D+00, 62.0D+00, 42.0D+00, &
   21.0D+00, 51.0D+00, 51.0D+00, 43.0D+00, 64.0D+00, &
   68.0D+00, 42.0D+00, 46.0D+00, 82.0D+00,  0.0D+00, &
   74.0D+00, 23.0D+00, 52.0D+00, 21.0D+00, 46.0D+00, &
   82.0D+00, 58.0D+00, 46.0D+00, 65.0D+00, 23.0D+00, &
   52.0D+00, 43.0D+00, 55.0D+00, 15.0D+00, 74.0D+00, &
    0.0D+00, 61.0D+00, 23.0D+00, 55.0D+00, 31.0D+00, &
   33.0D+00, 37.0D+00, 51.0D+00, 29.0D+00, 59.0D+00, &
   72.0D+00, 43.0D+00, 23.0D+00, 72.0D+00, 23.0D+00, &
   61.0D+00,  0.0D+00, 42.0D+00, 23.0D+00, 31.0D+00, &
   77.0D+00, 37.0D+00, 51.0D+00, 46.0D+00, 33.0D+00, &
   42.0D+00, 23.0D+00, 43.0D+00, 31.0D+00, 52.0D+00, &
   23.0D+00, 42.0D+00,  0.0D+00, 33.0D+00, 15.0D+00, &
   37.0D+00, 33.0D+00, 33.0D+00, 31.0D+00, 37.0D+00, &
   51.0D+00, 23.0D+00, 41.0D+00, 62.0D+00, 21.0D+00, &
   55.0D+00, 23.0D+00, 33.0D+00,  0.0D+00, 29.0D+00, &
   62.0D+00, 46.0D+00, 29.0D+00, 51.0D+00, 11.0D+00, &
   55.0D+00, 31.0D+00, 29.0D+00, 42.0D+00, 46.0D+00, &
   31.0D+00, 31.0D+00, 15.0D+00, 29.0D+00,  0.0D+00, &
   51.0D+00, 21.0D+00, 41.0D+00, 23.0D+00, 37.0D+00, &
   29.0D+00, 41.0D+00, 79.0D+00, 21.0D+00, 82.0D+00, &
   33.0D+00, 77.0D+00, 37.0D+00, 62.0D+00, 51.0D+00, &
    0.0D+00, 65.0D+00, 42.0D+00, 59.0D+00, 61.0D+00, &
   74.0D+00, 51.0D+00, 21.0D+00, 51.0D+00, 58.0D+00, &
   37.0D+00, 37.0D+00, 33.0D+00, 46.0D+00, 21.0D+00, &
   65.0D+00,  0.0D+00, 61.0D+00, 11.0D+00, 55.0D+00, &
   23.0D+00, 11.0D+00, 64.0D+00, 51.0D+00, 46.0D+00, &
   51.0D+00, 51.0D+00, 33.0D+00, 29.0D+00, 41.0D+00, &
   42.0D+00, 61.0D+00,  0.0D+00, 62.0D+00, 23.0D+00, &
   72.0D+00, 52.0D+00, 31.0D+00, 43.0D+00, 65.0D+00, &
   29.0D+00, 46.0D+00, 31.0D+00, 51.0D+00, 23.0D+00, &
   59.0D+00, 11.0D+00, 62.0D+00,  0.0D+00, 59.0D+00, &
   46.0D+00, 21.0D+00, 51.0D+00, 64.0D+00, 23.0D+00, &
   59.0D+00, 33.0D+00, 37.0D+00, 11.0D+00, 37.0D+00, &
   61.0D+00, 55.0D+00, 23.0D+00, 59.0D+00,  0.0D+00 /

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  TSP is a heuristic algorithm'
  write ( *, '(a)' ) '  for the traveling salesman problem.'

  call tsp ( n, dist, isol )
!
!  Determine the length of the tour.
!
  length = 0.0D+00
  do i = 1, n
    i1 = isol(i)
    if ( i < n ) then
      i2 = isol(i+1)
    else
      i2 = isol(1)
    end if
    length = length + dist(i1,i2)
  end do
!
!  Print the results.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  COMPUTED RESULTS:'
  write ( *, '(a)' ) ' '
  write ( *, '(20i3)' ) isol(1:n)
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Path length = ', length
!
!  Print the data for the exact solution.
!
  length = 0.0D+00
  do i = 1, n
    i1 = isol_exact(i)
    if ( i < n ) then
      i2 = isol_exact(i+1)
    else
      i2 = isol_exact(1)
    end if
    length = length + dist(i1,i2)
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  CORRECT RESULTS:'
  write ( *, '(a)' ) ' '
  write ( *, '(20i3)' ) isol_exact(1:n)
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Path length = ', length

  return
end
