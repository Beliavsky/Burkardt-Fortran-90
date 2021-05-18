program main

!*****************************************************************************80
!
!! MAIN is the main program for FLOYD_TEST.
!
!  Discussion:
!
!    FLOYD_TEST tests the FLOYD library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FLOYD_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the FLOYD library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FLOYD_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests I4MAT_FLOYD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 November 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ), dimension ( n, n ) :: a = reshape ( (/ &
     0, -1, -1, -1, -1, -1, &
     2,  0, -1, -1, -1,  5, &
     5,  7,  0, -1,  2, -1, &
    -1,  1,  4,  0, -1,  2, &
    -1, -1, -1,  3,  0,  4, &
    -1,  8, -1, -1,  3,  0  &
    /), (/ n, n /) )
  integer ( kind = 4 ) i4_huge

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  I4MAT_FLOYO uses Floyd''s algorithm to find the'
  write ( *, '(a)' ) '  shortest distance between all pairs of nodes'
  write ( *, '(a)' ) '  in a directed graph, starting from the initial array'
  write ( *, '(a)' ) '  of direct node-to-node distances.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In the initial direct distance array, if'
  write ( *, '(a)' ) '    A(I,J) = -1,'
  write ( *, '(a)' ) '  this indicates there is NO directed link from'
  write ( *, '(a)' ) '  node I to node J.  In that case, the value of'
  write ( *, '(a)' ) '  of A(I,J) is essentially "infinity".'

  call i4mat_print ( n, n, a, '  Initial direct distance array:' )

  where ( a(1:n,1:n) == - 1 )
    a = i4_huge ( )
  end where

  call i4mat_floyd ( n, a )

  where ( a(1:n,1:n) == i4_huge ( ) )
    a = - 1
  end where

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In the final shortest distance array, if'
  write ( *, '(a)' ) '    A(I,J) = -1,'
  write ( *, '(a)' ) '  this indicates there is NO directed path from'
  write ( *, '(a)' ) '  node I to node J.'

  call i4mat_print ( n, n, a, '  Final shortest distance array:' )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests R8MAT_FLOYD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2008
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
     0.0D+00, -1.0D+00, -1.0D+00, -1.0D+00, -1.0D+00, -1.0D+00, &
     2.0D+00,  0.0D+00, -1.0D+00, -1.0D+00, -1.0D+00,  5.0D+00, &
     5.0D+00,  7.0D+00,  0.0D+00, -1.0D+00,  2.0D+00, -1.0D+00, &
    -1.0D+00,  1.0D+00,  4.0D+00,  0.0D+00, -1.0D+00,  2.0D+00, &
    -1.0D+00, -1.0D+00, -1.0D+00,  3.0D+00,  0.0D+00,  4.0D+00, &
    -1.0D+00,  8.0D+00, -1.0D+00, -1.0D+00,  3.0D+00,  0.0D+00  &
    /), (/ n, n /) )
  real ( kind = 8 ) r8_huge

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  R8MAT_FLOYO uses Floyd''s algorithm to find the'
  write ( *, '(a)' ) '  shortest distance between all pairs of nodes'
  write ( *, '(a)' ) '  in a directed graph, starting from the initial array'
  write ( *, '(a)' ) '  of direct node-to-node distances.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In the initial direct distance array, if'
  write ( *, '(a)' ) '    A(I,J) = -1,'
  write ( *, '(a)' ) '  this indicates there is NO directed link from'
  write ( *, '(a)' ) '  node I to node J.  In that case, the value of'
  write ( *, '(a)' ) '  of A(I,J) is essentially "infinity".'

  call r8mat_print ( n, n, a, '  Initial direct distance array:' )

  where ( a(1:n,1:n) == - 1.0D+00 )
    a = r8_huge ( )
  end where

  call r8mat_floyd ( n, a )

  where ( a(1:n,1:n) == r8_huge ( ) )
    a = - 1.0D+00
  end where

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In the final shortest distance array, if'
  write ( *, '(a)' ) '    A(I,J) = -1,'
  write ( *, '(a)' ) '  this indicates there is NO directed path from'
  write ( *, '(a)' ) '  node I to node J.'

  call r8mat_print ( n, n, a, '  Final shortest distance array:' )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 applies Floyd's algorithm to problems of increasing size.
!
!  Discussion:
!
!    The matrix size is input by the user.
!
!    The matrix A has the property that
!
!      A(I,J) = 1 if I is divisible by J.
!
!  Example:
!
!    N = 6
!
!    1 0 0 0 0 0
!    1 1 0 0 0 0
!    1 0 1 0 0 0
!    1 1 0 1 0 0
!    1 0 0 0 1 0
!    1 1 1 0 0 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) wtime

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Test I4MAT_FLOYD on the MOD(I,J) matrix.'
  write ( *, '(a)' ) '  The work is roughly N^3.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         N   Time (seconds)  Time/N^3'
  write ( *, '(a)' ) ' '

  n = 1
  do while ( n <= 1024 )
    call test03_sub ( n, wtime )
    write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) &
      n, wtime, 1000000.0D+00 * wtime / real ( n**3, kind = 8 )
    n = n * 2
  end do

  return
end
subroutine test03_sub ( n, wtime )

!*****************************************************************************80
!
!! TEST03_SUB tests I4MAT_FLOYD.
!
!  Discussion:
!
!    The matrix size is input by the user.
!
!    The matrix A has the property that
!
!      A(I,J) = 1 if I is divisible by J.
!
!  Example:
!
!    N = 6
!
!    1 0 0 0 0 0
!    1 1 0 0 0 0
!    1 0 1 0 0 0
!    1 1 0 1 0 0
!    1 0 0 0 1 0
!    1 1 1 0 0 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 July 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the matrix.
!
!    Output, real ( kind = 8 ) WTIME, the CPU  time required by I4MAT_FLOYD.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_huge
  integer ( kind = 4 ) j
  real ( kind = 8 ) time1
  real ( kind = 8 ) time2
  real ( kind = 8 ) wtime

  do j = 1, n
    do i = 1, n
      if ( mod ( i, j ) == 0 ) then
        a(i,j) = 1
      else
        a(i,j) = i4_huge ( ) 
      end if
    end do
  end do

  call cpu_time ( time1 )

  call i4mat_floyd ( n, a )

  call cpu_time ( time2 )

  wtime = time2 - time1

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 uses Floyd's method for a triangulation.
!
!  Discussion:
!
!     8  41--42--43--44  45--46--47--48
!     |   | \ | \ | \ |   | \ | \ | \ |
!     7  33--34--35--36  37--38--39--40
!     |   | \ |                   | \ |
!     6  29--30                  31--32
!     |   | \ |                   | \ |
!     5  25--26                  27--28
!     |   | \ |                   | \ |
!     4  21--22                  23--24
!     |   | \ |                   | \ |
!     3  17--18                  19--20
!     |   | \ |                   | \ |
!     2   9--10--11--12--13--14--15--16
!     |   | \ | \ | \ | \ | \ | \ | \ |
!     1   1---2---3---4---5---6---7---8
!     |    
!     +---1---2---3---4---5---6---7---8
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: element_num = 46
  integer ( kind = 4 ), parameter :: node_num = 48
  
  real ( kind = 8 ) d(node_num,node_num)
  integer ( kind = 4 ) element
  integer ( kind = 4 ), dimension ( 3, element_num ) :: element_node = &
    reshape ( (/ &
     1,  2,  9, &
     2, 10,  9, &
     2,  3, 10, &
     3, 11, 10, &
     3,  4, 11, &
     4, 12, 11, &
     4,  5, 12, &
     5, 13, 12, &
     5,  6, 13, &
     6, 14, 13, &
     6,  7, 14, &
     7, 15, 14, &
     7,  8, 15, &
     8, 16, 15, &
     9, 10, 17, &
    10, 18, 17, &
    15, 16, 19, &
    16, 20, 19, &
    17, 18, 21, &
    18, 22, 21, &
    19, 20, 23, &
    20, 24, 23, &
    21, 22, 25, &
    22, 26, 25, &
    23, 24, 27, &
    24, 28, 27, &
    25, 26, 29, &
    26, 30, 29, &
    27, 28, 31, &
    28, 32, 31, &
    29, 30, 33, &
    30, 34, 33, &
    31, 32, 39, &
    32, 40, 39, &
    33, 34, 41, &
    34, 42, 41, &
    34, 35, 42, &
    35, 43, 42, &
    35, 36, 43, &
    36, 44, 43, &
    37, 38, 45, &
    38, 46, 45, &
    38, 39, 46, &
    39, 47, 46, &
    39, 40, 47, &
    40, 48, 47 /), (/ 3, element_num /) )
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  real ( kind = 8 ) r8_huge
  real ( kind = 8 ) r8vec_diff_norm
  real ( kind = 8 ), dimension ( 2, node_num ) :: xy = reshape ( (/ &
    1.0, 1.0, &
    2.0, 1.0, &
    3.0, 1.0, &
    4.0, 1.0, &
    5.0, 1.0, &
    6.0, 1.0, &
    7.0, 1.0, &
    8.0, 1.0, &
    1.0, 2.0, &
    2.0, 2.0, &
    3.0, 2.0, &
    4.0, 2.0, &
    5.0, 2.0, &
    6.0, 2.0, &
    7.0, 2.0, &
    8.0, 2.0, &
    1.0, 3.0, & 
    2.0, 3.0, &
    7.0, 3.0, &
    8.0, 3.0, &
    1.0, 4.0, &
    2.0, 4.0, &
    7.0, 4.0, &
    8.0, 4.0, &
    1.0, 5.0, &
    2.0, 5.0, &
    7.0, 5.0, &
    8.0, 5.0, &
    1.0, 6.0, &
    2.0, 6.0, &
    7.0, 6.0, &
    8.0, 6.0, &
    1.0, 7.0, &
    2.0, 7.0, &
    3.0, 7.0, &
    4.0, 7.0, &
    5.0, 7.0, &
    6.0, 7.0, &
    7.0, 7.0, &
    8.0, 7.0, &
    1.0, 8.0, & 
    2.0, 8.0, &
    3.0, 8.0, &
    4.0, 8.0, &
    5.0, 8.0, &
    6.0, 8.0, &
    7.0, 8.0, &
    8.0, 8.0 /), (/ 2, node_num /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  Start with a triangulation, and use R8_FLOYD'
  write ( *, '(a)' ) '  to determine the pairwise distance matrix.'
!
!  Must initialize distances to -1!
!
  d(1:node_num,1:node_num) = -1.0D+00
!
!  Diagonals are 0.
!
  do i = 1, node_num
    d(i,i) = 0.0D+00
  end do
!
!  Initialize D to the one-step distance.
!
  do element = 1, element_num
    n1 = element_node(3,element)
    do i = 1, 3
      n2 = element_node(i,element)
      d(n1,n2) = r8vec_diff_norm ( 2, xy(1:2,n1), xy(1:2,n2) )
      d(n2,n1) = d(n1,n2)
      n1 = n2
    end do
  end do
!
!  Reset -1 values to R8_HUGE.
!
  where ( d(1:node_num,1:node_num) == - 1.0D+00 )
    d = r8_huge ( )
  end where
!
!  Update D to the N-1 step distance.
!
  call r8mat_floyd ( node_num, d )

  call r8mat_print ( node_num, node_num, d, '  Distance matrix' )

  return
end

