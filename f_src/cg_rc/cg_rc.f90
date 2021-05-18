subroutine cg_rc ( n, b, x, r, z, p, q, job )

!*****************************************************************************80
!
!! CG_RC is a reverse communication conjugate gradient routine.
!
!  Discussion:
!
!    This routine seeks a solution of the linear system A*x=b
!    where b is a given right hand side vector, A is an n by n
!    symmetric positive definite matrix, and x is an unknown vector
!    to be determined.
!
!    Under the assumptions that the matrix A is large and sparse,
!    the conjugate gradient method may provide a solution when
!    a direct approach would be impractical because of excessive
!    requirements of storage or even of time.
!
!    The conjugate gradient method presented here does not require the 
!    user to store the matrix A in a particular way.  Instead, it only 
!    supposes that the user has a way of calculating
!      y = alpha * A * x + b * y
!    and of solving the preconditioned linear system
!      M * x = b
!    where M is some preconditioning matrix, which might be merely
!    the identity matrix, or a diagonal matrix containing the
!    diagonal entries of A.
!
!    This routine was extracted from the "templates" package.
!    There, it was not intended for direct access by a user;
!    instead, a higher routine called "cg()" was called once by
!    the user.  The cg() routine then made repeated calls to 
!    cgrevcom() before returning the result to the user.
!
!    The reverse communication feature of cgrevcom() makes it, by itself,
!    a very powerful function.  It allows the user to handle issues of
!    storage and implementation that would otherwise have to be
!    mediated in a fixed way by the function argument list.  Therefore,
!    this version of cgrecom() has been extracted from the templates
!    library and documented as a stand-alone procedure.
!
!    The user sets the value of JOB to 1 before the first call,
!    indicating the beginning of the computation, and to the value of
!    2 thereafter, indicating a continuation call.  
!    The output value of JOB is set by cgrevcom(), which
!    will return with an output value of JOB that requests a particular
!    new action from the user.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Richard Barrett, Michael Berry, Tony Chan, James Demmel,
!    June Donato, Jack Dongarra, Victor Eijkhout, Roidan Pozo,
!    Charles Romine, Henk van der Vorst,
!    Templates for the Solution of Linear Systems:
!    Building Blocks for Iterative Methods,
!    SIAM, 1994,
!    ISBN: 0898714710,
!    LC: QA297.8.T45.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the matrix.
!
!    Input, real ( kind = 8 ) B(N), the right hand side vector.
!
!    Input/output, real ( kind = 8 ) X(N).  On first call, the user 
!    should store an initial guess for the solution in X.  On return with
!    JOB = 4, X contains the latest solution estimate.
!
!    Input/output, real ( kind = 8 ) R(N), Z(N), P(N), Q(N),
!    information used by the program during the calculation.  The user
!    does not need to initialize these vectors.  However, specific
!    return values of JOB may require the user to carry out some computation
!    using data in some of these vectors.
!
!    Input/output, integer ( kind = 4 ) JOB, communicates the task to be done.
!    The user needs to set the input value of JOB to 1, before the first call,
!    and then to 2 for every subsequent call for the given problem.
!    The output value of JOB indicates the requested user action.  
!    * JOB = 1, compute Q = A * P;
!    * JOB = 2: solve M*Z=R, where M is the preconditioning matrix;
!    * JOB = 3: compute R = R - A * X;
!    * JOB = 4: check the residual R for convergence.  
!               If satisfactory, terminate the iteration.
!               If too many iterations were taken, terminate the iteration.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) beta
  integer ( kind = 4 ) iter
  integer ( kind = 4 ) job
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) pdotq
  real ( kind = 8 ) q(n)
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) rho
  real ( kind = 8 ) rho_old
  integer ( kind = 4 ) rlbl
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) z(n)
!
!  Some local variables must be preserved between calls.
!
  save iter
  save rho
  save rho_old
  save rlbl
!
!  Initialization.
!  Ask the user to compute the initial residual.
!
  if ( job == 1 ) then

    r(1:n) = b(1:n)

    job = 3
    rlbl = 2
!
!  Begin first conjugate gradient loop.
!  Ask the user for a preconditioner solve.
!
  else if ( rlbl == 2 ) then

    iter = 1

    job = 2
    rlbl = 3
!
!  Compute the direction.
!  Ask the user to compute ALPHA.
!  Save A*P to Q.
!
  else if ( rlbl == 3 ) then

    rho = dot_product ( r, z )

    if ( 1 < iter ) then
      beta = rho / rho_old
      z(1:n) = z(1:n) + beta * p(1:n)
    end if

    p(1:n) = z(1:n)

    job = 1
    rlbl = 4
!
!  Compute current solution vector.
!  Ask the user to check the stopping criterion.
!
  else if ( rlbl == 4 ) then

    pdotq = dot_product ( p, q )
    alpha = rho / pdotq
    x(1:n) = x(1:n) + alpha * p(1:n)
    r(1:n) = r(1:n) - alpha * q(1:n)

    job = 4
    rlbl = 5
!
!  Begin the next step.
!  Ask for a preconditioner solve.
!
  else if ( rlbl == 5 ) then

    rho_old = rho
    iter = iter + 1

    job = 2
    rlbl = 3

  end if

  return
end
subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
  end do

  return
end
subroutine r8vec_uniform_01 ( n, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
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
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + 2147483647
    end if

    r(i) = real ( seed, kind = 8 ) * 4.656612875D-10

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
subroutine wathen ( nx, ny, n, a )

!*****************************************************************************80
!
!! WATHEN returns the WATHEN matrix.
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
!    and sufficient storage in A must have been set aside to hold
!    the matrix.
!
!    A is the consistent mass matrix for a regular NX by NY grid
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
!  Properties:
!
!    A is symmetric positive definite for any positive values of the
!    density RHO(NX,NY), which is here given the value 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2006
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
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Output, real ( kind = 8 ) A(N,N), the matrix.
!
  implicit none

  integer ( kind = 4 ) n

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
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) node(8)
  real ( kind = 8 ) rho

  a(1:n,1:n) = 0.0D+00

  do j = 1, ny

    do i = 1, nx
!
!  For the element (I,J), determine the indices of the 8 nodes.
!
      node(1) = 3 * j * nx + 2 * j + 2 * i + 1
      node(2) = node(1) - 1
      node(3) = node(1) - 2

      node(4) = ( 3 * j - 1 ) * nx + 2 * j + i - 1
      node(8) = node(4) + 1

      node(5) = ( 3 * j - 3 ) * nx + 2 * j + 2 * i - 3
      node(6) = node(5) + 1
      node(7) = node(5) + 2
!
!  The density RHO can also be set to a random positive value.
!
      do krow = 1, 8
        do kcol = 1, 8

          if ( node(krow) < 1 .or. n < node(krow) ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'WATHEN - Fatal error!'
            write ( *, '(a)' ) '  Index NODE(KROW) out of bounds.'
            write ( *, '(a,i8)' ) '  I = ', i
            write ( *, '(a,i8)' ) '  J = ', j
            write ( *, '(a,i8)' ) '  KROW = ', krow
            write ( *, '(a,i8)' ) '  NODE(KROW) = ', node(krow)
            stop
          else if ( node(kcol) < 1 .or. n < node(kcol) ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'WATHEN - Fatal error!'
            write ( *, '(a)' ) '  Index NODE(KCOL) out of bounds.'
            write ( *, '(a,i8)' ) '  I = ', i
            write ( *, '(a,i8)' ) '  J = ', j
            write ( *, '(a,i8)' ) '  KCOL = ', kcol
            write ( *, '(a,i8)' ) '  NODE(KCOL) = ', node(kcol)
            stop
          end if

          rho = 1.0D+00
!         rho = 100.0D+00 * r8_uniform_01 ( seed )

          a(node(krow),node(kcol)) = a(node(krow),node(kcol)) &
            + 20.0D+00 * rho * em(krow,kcol) / 9.0D+00

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
!    17 June 2011
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
