program main

!*****************************************************************************80
!
!! MAIN is the main program for SPARSE_DISPLAY_TEST.
!
!  Discussion:
!
!    SPARSE_DISPLAY_TEST tests the SPARSE_DISPLAY library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPARSE_DISPLAY_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SPARSE_DISPLAY library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SPARSE_DISPLAY_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests SPY_GE for a general storage matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  character ( len = 255 ) header
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  SPY_GE generates a sparsity plot for a matrix stored'
  write ( *, '(a)' ) '  in general (GE) format.'

  nx = 5
  ny = 5
  call wathen_order ( nx, ny, n )
  seed = 123456789
  allocate ( a(1:n,1:n) )
  call wathen_ge ( nx, ny, n, seed, a )
  header = 'wathen_ge'

  call spy_ge ( n, n, a, header )

  deallocate ( a )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests SPY_FILE in cases where the indices are in a file.
!
!  Discussion:
!
!    The files used in this example actually use negative column indices
!    because they were output by DEAL.II and intended to be passed directly
!    into GNUPLOT without any clever commands.
!
!    So my own "SPY_FILE" is currently set up to deal exactly with such
!    files, and hence, given sensible data will actually show a spy plot
!    that is transposed - still readable and all, but wrong way round.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 255 ) filename
  character ( len = 255 ) header

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  SPY_FILE generates a sparsity plot for a matrix for'
  write ( *, '(a)' ) '  which there exists a file containing all the pairs'
  write ( *, '(a)' ) '  (I,J) corresponding to nonzero entries.'

  header = 'before'
  filename = 'before_data.txt'

  call spy_file ( header, filename )

  header = 'after'
  filename = 'after_data.txt'

  call spy_file ( header, filename )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests SPY_GE for a general storage matrix.
!
!  Discussion:
!
!    It is not clear to me whether the plot being created is correctly
!    oriented.  I might be seeing the transpose of the matrix.
!    One way to check is to set up a moderate sized, highly asymmetric matrix.
!    In this case, I will create a certain upper triangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  character ( len = 255 ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  SPY_GE generates a sparsity plot for a matrix stored'
  write ( *, '(a)' ) '  in general (GE) format.'
  write ( *, '(a)' ) '  Just to orient ourselves, generate an upper triangular matrix.'

  m = 20
  n = 30
  allocate ( a(1:m,1:n) )

  do i = 1, m
    do j = 1, n
      if ( j < i .or. mod ( i - j, i ) /= 0 ) then
        a(i,j) = 0.0
      else
        a(i,j) = 1.0
      end if
    end do
  end do

  header = '20x30'

  call spy_ge ( m, n, a, header )

  deallocate ( a )

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
subroutine wathen_ge ( nx, ny, n, seed, a )

!*****************************************************************************80
!
!! WATHEN_GE returns the Wathen matrix as a general storage (GE) matrix.
!
!  Discussion:
!
!    The Wathen matrix is a finite element matrix which is sparse.
!
!    The entries of the matrix depend in part on a physical quantity
!    related to density.  That density is here assigned random values between
!    0 and 100.
!
!    The matrix order N is determined by the input quantities NX and NY,
!    which would usually be the number of elements in the X and Y directions.
!    The value of N is
!
!      N = 3*NX*NY + 2*NX + 2*NY + 1,
!
!    The matrix is the consistent mass matrix for a regular NX by NY grid
!    of 8 node serendipity elements.
!
!    Here is an illustration for NX = 3, NY = 2:
!
!     23-24-25-26-27-28-29
!      |     |     |     |
!     19    20    21    22
!      |     |     |     |
!     12-13-14-15-16-17-18
!      |     |     |     |
!      8     9    10    11
!      |     |     |     |
!      1--2--3--4--5--6--7
!
!    For this example, the total number of nodes is, as expected,
!
!      N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
!
!    The matrix is symmetric positive definite for any positive values of the
!    density RHO(X,Y).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Nicholas Higham,
!    Algorithm 694: A Collection of Test Matrices in MATLAB,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 3, September 1991, pages 289-305.
!
!    Andrew Wathen,
!    Realistic eigenvalue bounds for the Galerkin mass matrix,
!    IMA Journal of Numerical Analysis,
!    Volume 7, Number 4, October 1987, pages 449-457.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, NY, values which determine the size 
!    of the matrix.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns.
!
!    Input/output, integer ( kind = 4 ) SEED, the random number seed.
!
!    Output, real ( kind = 8 ) A(N,N), the matrix.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ), dimension ( 8, 8 ), save :: em =  reshape ( (/ &
     6.0, -6.0,  2.0, -8.0,  3.0, -8.0,  2.0, -6.0, &
    -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, &
     2.0, -6.0,  6.0, -6.0,  2.0, -8.0,  3.0, -8.0, &
    -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, &
     3.0, -8.0,  2.0, -6.0,  6.0, -6.0,  2.0, -8.0, &
    -8.0, 16.0, -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, &
     2.0, -8.0,  3.0, -8.0,  2.0, -6.0,  6.0, -6.0, &
    -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, -6.0, 32.0 /), &
    (/ 8, 8 /) )
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) kcol
  integer ( kind = 4 ) krow
  integer ( kind = 4 ) node(8)
  real ( kind = 8 ) rho(nx,ny)
  integer ( kind = 4 ) seed

  a(1:n,1:n) = 0.0D+00

  call r8mat_uniform_01 ( nx, ny, seed, rho )
  rho(1:nx,1:ny) = 100.0D+00 * rho(1:nx,1:ny)

  do j = 1, ny
    do i = 1, nx

      node(1) = 3 * j * nx + 2 * j + 2 * i + 1
      node(2) = node(1) - 1
      node(3) = node(1) - 2
      node(4) = ( 3 * j - 1 ) * nx + 2 * j + i - 1
      node(5) = ( 3 * j - 3 ) * nx + 2 * j + 2 * i - 3
      node(6) = node(5) + 1
      node(7) = node(5) + 2
      node(8) = node(4) + 1

      do krow = 1, 8
        do kcol = 1, 8
          a(node(krow),node(kcol)) = a(node(krow),node(kcol)) &
            + rho(i,j) * em(krow,kcol)
        end do
      end do

    end do
  end do

  return
end
subroutine wathen_order ( nx, ny, n )

!*****************************************************************************80
!
!! WATHEN_ORDER returns the order of the WATHEN matrix.
!
!  Discussion:
!
!    N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 January 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Nicholas Higham,
!    Algorithm 694: A Collection of Test Matrices in MATLAB,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 3, September 1991, pages 289-305.
!
!    Andrew Wathen,
!    Realistic eigenvalue bounds for the Galerkin mass matrix,
!    IMA Journal of Numerical Analysis,
!    Volume 7, 1987, pages 449-457.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NX, NY, values which determine the size of A.
!
!    Output, integer ( kind = 4 ) N, the order of the matrix,
!    as determined by NX and NY.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny

  n = 3 * nx * ny + 2 * nx + 2 * ny + 1

  return
end

