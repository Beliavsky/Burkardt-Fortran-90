function i4_gcd ( i, j )

!*****************************************************************************80
!
!! I4_GCD finds the greatest common divisor of two I4's.
!
!  Discussion:
!
!    Note that only the absolute values of I and J are
!    considered, so that the result is always nonnegative.
!
!    If I or J is 0, I4_GCD is returned as max ( 1, abs ( I ), abs ( J ) ).
!
!    If I and J have no common factor, I4_GCD is returned as 1.
!
!    Otherwise, using the Euclidean algorithm, I4_GCD is the
!    greatest common divisor of I and J.
!
!    An I4 is an integer ( kind = 4 ) value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, J, two numbers whose GCD is desired.
!
!    Output, integer ( kind = 4 ) I4_GCD, the greatest common divisor
!    of I and J.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_gcd
  integer ( kind = 4 ) j
  integer ( kind = 4 ) p
  integer ( kind = 4 ) q
  integer ( kind = 4 ) r

  i4_gcd = 1
!
!  Return immediately if either I or J is zero.
!
  if ( i == 0 ) then
    i4_gcd = max ( 1, abs ( j ) )
    return
  else if ( j == 0 ) then
    i4_gcd = max ( 1, abs ( i ) )
    return
  end if
!
!  Set P to the larger of I and J, Q to the smaller.
!  This way, we can alter P and Q as we go.
!
  p = max ( abs ( i ), abs ( j ) )
  q = min ( abs ( i ), abs ( j ) )
!
!  Carry out the Euclidean algorithm.
!
  do

    r = mod ( p, q )

    if ( r == 0 ) then
      exit
    end if

    p = q
    q = r

  end do

  i4_gcd = q

  return
end
subroutine i4mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! I4MAT_PRINT prints an I4MAT.
!
!  Discussion:
!
!    An I4MAT is an M by N array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, integer ( kind = 4 ) A(M,N), the matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  ilo = 1
  ihi = m
  jlo = 1
  jhi = n

  call i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

  return
end
subroutine i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! I4MAT_PRINT_SOME prints some of an I4MAT.
!
!  Discussion:
!
!    An I4MAT is an M by N array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, integer ( kind = 4 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 10
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  character ( len = 8 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8)' ) j
    end do

    write ( *, '(''  Col '',10a8)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        write ( ctemp(j2), '(i8)' ) a(i,j)

      end do

      write ( *, '(i5,a,10a8)' ) i, ':', ( ctemp(j), j = 1, inc )

    end do

  end do

  return
end
subroutine i4mat_ref ( m, n, a, det )

!*****************************************************************************80
!
!! I4MAT_REF computes the integer row echelon form (IREF) of an I4MAT.
!
!  Discussion:
!
!    If a matrix A contains only integer entries, then when it is reduced
!    to row echelon form, it is likely that many entries will no longer
!    be integers, due to the elimination process.
!
!    In some cases, tiny arithmetic errors in this elimination process can
!    result in spurious, tiny nonzero values which can invalidate the
!    calculation, particular if the elimination is being done in an effort
!    to determine the rank of the matrix.  These serious errors can easily
!    occur in very small matrices, such as of size 7x10.
!
!    If we, instead, insist on using only integer operations on an integer
!    matrix, we can guarantee that tiny roundoff errors will not cause
!    such problems.  On the other hand, as the elimination process proceeds,
!    we may instead calculate integer matrix entries of increasingly
!    large, and then ultimately meaningless magnitude.  I imagine this is 
!    likely to happen for moderate size matrices of order 50x50, say, but
!    this is a huge improvement over the unreliability of the real
!    arithmetic case.
!
!
!    Thus, we define "integer row echelon form" (IREF).
!
!
!    A matrix is in integer row echelon form if:
!
!    * The leading nonzero in each row is positive.
!
!    * Each row has no common factor greater than 1.
!
!    * The leading nonzero in each row occurs in a column to
!      the right of the leading nonzero in the previous row.
!
!    * Rows which are entirely zero occur last.
!
!  Example:
!
!    Input matrix:
!
!     1    3    0    2    6    3    1
!    -2   -6    0   -2   -8    3    1
!     3    9    0    0    6    6    2
!    -1   -3    0    1    0    9    3
!
!    Output matrix:
!
!     1    3    0    2    6    3    1
!     0    0    0    2    4    9    3
!     0    0    0    0    0    3    1
!     0    0    0    0    0    0    0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix A.
!
!    Input/output, integer ( kind = 8 ) A(M,N).  On input, the matrix to be
!    analyzed.  On output, the IREF form of the matrix.
!
!    Output, integer ( kind = 8 ) DET, the pseudo-determinant.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) det
  integer ( kind = 4 ) factor
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lead
  integer ( kind = 4 ) r

  lead = 1
  det = 1

  do r = 1, m

    if ( n < lead ) then
      exit
    end if
!
!  Start I at row R, and search for nonzero pivot entry A(I,LEAD).
!
    i = r

    do while ( a(i,lead) == 0 )

      i = i + 1
!
!  If reach last row, reset I to R, and increment LEAD.
!
      if ( m < i ) then
        i = r
        lead = lead + 1
!
!  If reach last column, we can find no more pivots.
!
        if ( n < lead ) then
          lead = -1
          exit
        end if
      end if

    end do

    if ( lead < 0 ) then
      exit
    end if
!
!  Move pivot I into row R.
!
    if ( i /= r ) then
      call i4mat_row_swap ( m, n, a, i, r )
    end if
!
!  Ensure pivot is positive.
!
    if ( a(r,lead) < 0 ) then
      a(r,1:n) = - a(r,1:n)
      det = - det
    end if
!
!  Update the pseudo-determinant.
!
    det = det * a(r,lead)
!
!  Remove any common factor from row R.
!
    call i4vec_red ( n, a(r,1:n), factor )
!
!  Use a multiple of A(R,LEAD) to eliminate A(R+1:M,LEAD).
!
    do i = r + 1, m

      a(i,1:n) = a(r,lead) * a(i,1:n) - a(i,lead) * a(r,1:n)

      call i4vec_red ( n, a(i,1:n), factor )

    end do

    lead = lead + 1

  end do

  return
end
subroutine i4mat_row_swap ( m, n, a, i1, i2 )

!*****************************************************************************80
!
!! I4MAT_ROW_SWAP swaps rows in an I4MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input/output, integer ( kind = 4 ) A(M,N).
!    On input, the matrix to be modified.
!    On output, two rows have been swapped. 
!
!    Input, integer ( kind = 4 ) I1, I2, the indices of the rows.
!    1 <= I1, I2 <= M.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) row(n)

  if ( i1 < 1 .or. m < i1 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'I4MAT_ROW_SWAP - Fatal error!'
    write ( *, '(a)' ) '  Row index 1 <= I1 <= M required.'
    stop 1
  end if

  if ( i2 < 1 .or. m < i2 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'I4MAT_ROW_SWAP - Fatal error!'
    write ( *, '(a)' ) '  Row index 1 <= I2 <= M required.'
    stop 1
  end if

  row(1:n)  = a(i1,1:n)
  a(i1,1:n) = a(i2,1:n)
  a(i2,1:n) = row(1:n)

  return
end
subroutine i4mat_rref ( m, n, a, det )

!*****************************************************************************80
!
!! I4MAT_RREF computes the reduced row echelon form of an I4MAT.
!
!  Discussion:
!
!    If a matrix A contains only integer entries, then when it is transformed
!    to row reduced echelon form, it is likely that many entries will no longer
!    be integers, due to the elimination process.
!
!    In some cases, tiny arithmetic errors in this elimination process can
!    result in spurious, tiny nonzero values which can invalidate the
!    calculation, particular if the elimination is being done in an effort
!    to determine the rank of the matrix.  These serious errors can easily
!    occur in very small matrices, such as of size 7x10.
!
!    If we, instead, insist on using only integer operations on an integer
!    matrix, we can guarantee that tiny roundoff errors will not cause
!    such problems.  On the other hand, as the elimination process proceeds,
!    we may instead calculate integer matrix entries of increasingly
!    large, and then ultimately meaningless magnitude.  I imagine this is 
!    likely to happen for moderate size matrices of order 50x50, say, but
!    this is a huge improvement over the unreliability of the real
!    arithmetic case.
!
!
!    Thus, we define "integer row reduced echelon form" (IRREF):
!
!
!    A matrix is in integer row reduced echelon form if:
!
!    * The leading nonzero in each row is positive.
!
!    * Each row has no common factor greater than 1.
!
!    * The leading nonzero in each row occurs in a column to
!      the right of the leading nonzero in the previous row.
!
!    * Rows which are entirely zero occur last.
!
!    * When a row contains a leading nonzero in column J, then column J
!      is otherwise entirely zero.
!
!  Example:
!
!    Input matrix:
!
!     1    3    0    2    6    3    1
!    -2   -6    0   -2   -8    3    1
!     3    9    0    0    6    6    2
!    -1   -3    0    1    0    9    3
!
!    Output matrix:
!
!     1    3    0    0    2    0    0
!     0    0    0    1    2    0    0
!     0    0    0    0    0    3    1
!     0    0    0    0    0    0    0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix A.
!
!    Input/output, integer ( kind = 4 ) A(M,N).  On input, the matrix to be
!    analyzed.  On output, the IRREF form of the matrix.
!
!    Output, integer ( kind = 4 ) DET, the pseudo-determinant.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) det
  integer ( kind = 4 ) factor
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lead
  integer ( kind = 4 ) r

  lead = 1
  det = 1

  do r = 1, m

    if ( n < lead ) then
      exit
    end if
!
!  Start I at row R, and search for nonzero pivot entry A(I,LEAD).
!
    i = r

    do while ( a(i,lead) == 0 )

      i = i + 1
!
!  If reach last row, reset I to R, and increment LEAD.
!
      if ( m < i ) then
        i = r
        lead = lead + 1
!
!  If reach last column, we can find no more pivots.
!
        if ( n < lead ) then
          lead = -1
          exit
        end if
      end if

    end do

    if ( lead < 0 ) then
      exit
    end if
!
!  Move pivot I into row R.
!
    if ( i /= r ) then
      call i4mat_row_swap ( m, n, a, i, r )
    end if
!
!  Ensure pivot is positive.
!
    if ( a(r,lead) < 0 ) then
      a(r,1:n) = - a(r,1:n)
      det = - det
    end if
!
!  Update the pseudo-determinant.
!
    det = det * a(r,lead)
!
!  Remove any common factor from row R.
!
    call i4vec_red ( n, a(r,1:n), factor )
!
!  Use a multiple of A(R,LEAD) to eliminate A(1:M,LEAD).
!
    do i = 1, m

      if ( i /= r ) then

        a(i,1:n) = a(r,lead) * a(i,1:n) - a(i,lead) * a(r,1:n)

        call i4vec_red ( n, a(i,1:n), factor )

      end if

    end do

    lead = lead + 1

  end do

  return
end
subroutine i4mat_rref_system ( m, n, a, b, a2, b2, incon, freedom_num, freedom )

!*****************************************************************************80
!
!! I4MAT_RREF_SOLVE_SYSTEM sets up an augmented IRREF linear system.
!
!  Discussion:
!
!    An MxN integer linear system A * X = B is considered.
! 
!    The matrix A and right hand side B are assumed to have been converted
!    to integer row-reduced echelon form (IRREF).
!
!    To create, if possible, a solvable NxN system, this function removes
!    trailing zero rows, and inserts where necessary, rows of the identity
!    matrix in A and zeros in B, corresponding to undetermined degrees of 
!    freedom, producing the NxN system:
!
!      A2 * X = B2
!
!    The function also indicates whether the initial system was inconsistent,
!    and identifies those rows of A2 that correspond to degrees of freedom.
!    
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 September 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns of the IRREF matrix A.
!
!    Input, integer A(M,N), the IRREF matrix to be analyzed. 
!
!    Input, integer B(M), the IRREF right hand side.
!
!    Output, integer A2(N,N), the modified IRREF matrix.
!
!    Output, integer B2(N), the modified IRREF right hand side.
!
!    Output, logical INCON, is TRUE if the system A*X=B is inconsistent.
!
!    Output, integer FREEDOM_NUM, the number of degrees of freedom.
!    If FREEDOM_NUM < 0, then there are no degrees of freedom and the
!    system is overdetermined.
!
!    Output, integer FREEDOM(FREEDOM_NUM), the indices of the degrees
!    of freedom, presuming 0 <= FREEDOM_NUM.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) a2(n,n)
  integer ( kind = 4 ) b(m)
  integer ( kind = 4 ) b2(n)
  integer ( kind = 4 ) freedom(n)
  integer ( kind = 4 ) freedom_count
  integer ( kind = 4 ) freedom_num
  logical incon
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m2
!
!  Determine 0 <= M2 <= M, the location of the last nonzero row in A.
!  If any zero row of A has a nonzero B, then the equations are inconsistent.
!
  m2 = m
  incon = .false.

  do while ( 0 < m2 )

    if ( any ( a(m2,1:n) /= 0 ) ) then
      exit
    end if

    if ( b(m2) /= 0 ) then
      incon = .true.
    end if

    m2 = m2 - 1

  end do
!
!  Copy rows 1 through M2 of A and B into A2 and B2.
!
  a2(1:m2,1:n) = a(1:m2,1:n)
  a2(m2+1:n,1:n) = 0
  b2(1:m2)   = b(1:m2)
  b2(m2+1:n) = 0
!
!  Count the indeterminate variables.
!
  freedom_num = n - m2
  freedom_count = 0
!
!  If pivot in column J is missing,
!  modify matrix and right hand side.
!  Add J to list of indeterminate variables.
!
  freedom(1:n) = -1

  if ( 0 < freedom_num ) then

    do j = 1, n
      if ( m2 < j ) then
        a2(j,1:n) = 0
        a2(j,j) = 1
        b2(j) = 0
        freedom_count = freedom_count + 1
        freedom(freedom_count) = j
        m2 = m2 + 1
      else if ( a2(j,j) == 0 ) then
        a2(j+1:m2+1,1:n) = a2(j:m2,1:n)
        a2(j,1:n) = 0
        a2(j,j) = 1
        b2(j+1:m2+1) = b2(j:m2)
        b2(j) = 0
        freedom_count = freedom_count + 1
        freedom(freedom_count) = j
        m2 = m2 + 1
      end if
    end do

  end if

  return
end
subroutine i4mat_u_solve ( n, a, b, x )

!*****************************************************************************80
!
!! I4MAT_U_SOLVE solves an upper triangular linear system with an I4MAT matrix.
!
!  Discussion:
!
!    An I4MAT is an MxN array of I4's, stored by (I,J) -> [I+J*M].
!
!    Note that the solution will be returned as a real vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix A.
!
!    Input, integer ( kind = 8 ) A(N,N), the N by N upper triangular matrix.
!
!    Input, integer ( kind = 8 ) B(N), the right hand side of the linear system.
!
!    Output, real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n,n)
  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
!
!  Solve U * x = b.
!
  do i = n, 1, -1
    x(i) = real ( b(i) - dot_product ( a(i,i+1:n), x(i+1:n) ), kind = 8 ) / &
      real ( a(i,i), kind = 8 )
  end do

  return
end
subroutine i4rows_to_i4mat ( m, n, i4rows, i4mat )

!*****************************************************************************80
!
!! I4ROWS_TO_I4MAT converts a row-major vector to an I4MAT.
!
!  Discussion:
!
!    An I4MAT is an MxN array of I4's, in column major order.
!
!    I am frustrated that the FORTRAN standard for initializing an array
!    forces me to enter a table of data by columns, so that I have to
!    transpose the information, which is confusing to me and any reader.
!
!    This function allows me to declare a vector of the right type and length,
!    fill it with data that I can display row-wise, and then have the
!    data copied into a FORTRAN array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 September 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, integer ( kind = 4 ) I4ROWS(M*N), the data. stored rowwise
!    in a vector.
!
!    Output, integer ( kind = 4 ) I4MAT(M,N), a copy of the data, stored
!    columnwise in an array.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4mat(m,n)
  integer ( kind = 4 ) i4rows(m*n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      i4mat(i,j) = i4rows(k)
    end do
  end do

  return
end
subroutine i4vec_binary_next ( n, bvec )

!*****************************************************************************80
!
!! I4VEC_BINARY_NEXT generates the next binary vector.
!
!  Discussion:
!
!    The vectors have the order
!
!      (0,0,...,0),
!      (0,0,...,1),
!      ...
!      (1,1,...,1)
!
!    and the "next" vector after (1,1,...,1) is (0,0,...,0).  That is,
!    we allow wrap around.
!
!  Example:
!
!    N = 3
!
!    Input      Output
!    -----      ------
!    0 0 0  =>  0 0 1
!    0 0 1  =>  0 1 0
!    0 1 0  =>  0 1 1
!    0 1 1  =>  1 0 0
!    1 0 0  =>  1 0 1
!    1 0 1  =>  1 1 0
!    1 1 0  =>  1 1 1
!    1 1 1  =>  0 0 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vectors.
!
!    Input, integer ( kind = 4 ) BVEC(N), the vector whose successor is desired.
!
!    Output, integer ( kind = 4 ) BVEC(N), the successor to the input vector.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) bvec(n)
  integer ( kind = 4 ) i

  do i = n, 1, -1

    if ( bvec(i) == 0 ) then
      bvec(i) = 1
      return
    end if

    bvec(i) = 0

  end do

  return
end
subroutine i4vec_identity_row ( n, i, a )

!*****************************************************************************80
!
!! I4VEC_IDENTITY_ROW returns the I-th row of the identity matrix.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input, integer ( kind = 4 ) I, the entry to be set to 1.
!
!    Output, integer ( kind = 4 ) A(N), the array.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i

  a(1:n) = 0
  if ( 1 <= i .and. i <= n ) then
    a(i) = 1
  end if

  return
end
function i4vec_is_binary ( n, x )

!*****************************************************************************80
!
!! I4VEC_IS_BINARY is true if an I4VEC only contains 0 and 1 entries.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vectors.
!
!    Input, integer ( kind = 4 ) X(N), the vector to be checked.
!
!    Output, logical ( kind = 4 ) I4VEC_IS_BINARY, is true if X only contains
!    0 or 1 entries.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  logical ( kind = 4 ) i4vec_is_binary
  logical ( kind = 4 ) value
  integer ( kind = 4 ) x(n)

  value = .true.

  do i = 1, n

    if ( x(i) /= 0 .and. x(i) /= 1 ) then
      value = .false.
      exit
    end if

  end do

  i4vec_is_binary = value

  return
end
subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,a,2x,i12)' ) i, ':', a(i)
  end do

  return
end
subroutine i4vec_red ( n, a, factor )

!*****************************************************************************80
!
!! I4VEC_RED divides out common factors in an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!    On output, the entries of A have no common factor
!    greater than 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) A(N), the vector to be reduced.
!
!    Output, integer ( kind = 4 ) FACTOR, the common factor that was divided
!    out.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) factor
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_gcd
!
!  Find the smallest nonzero value.
!
  factor = 0

  do i = 1, n

    if ( a(i) /= 0 ) then

      if ( factor == 0 ) then
        factor = abs ( a(i) )
      else
        factor = min ( factor, abs ( a(i) ) )
      end if

    end if

  end do

  if ( factor == 0 ) then
    return
  end if
!
!  Find the greatest common factor of the entire vector.
!
  do i = 1, n
    factor = i4_gcd ( a(i), factor )
  end do

  if ( factor == 1 ) then
    return
  end if
!
!  Divide out the common factor.
!
  do i = 1, n
    a(i) = a(i) / factor
  end do

  return
end
subroutine i4vec_transpose_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_TRANSPOSE_PRINT prints an I4VEC "transposed".
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Example:
!
!    A = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 /)
!    TITLE = 'My vector:  '
!
!    My vector:
!        1    2    3    4    5
!        6    7    8    9   10
!       11
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 June 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  if ( 0 < n ) then
    do ilo = 1, n, 5
      ihi = min ( ilo + 5 - 1, n )
      write ( *, '(5i12)' ) a(ilo:ihi)
    end do
  else
    write ( *, '(a)' ) '  (empty vector)'
  end if

  return
end
subroutine ksub_next4 ( n, k, a, done )

!*****************************************************************************80
!
!! KSUB_NEXT4 generates the subsets of size K from a set of size N.
!
!  Discussion:
!
!    The subsets are generated one at a time.
!
!    The routine should be used by setting DONE to TRUE, and then calling
!    repeatedly.  Each call returns with DONE equal to FALSE, the array
!    A contains information defining a new subset.  When DONE returns
!    equal to TRUE, there are no more subsets.
!
!    There are ( N*(N-1)*...*(N+K-1)) / ( K*(K-1)*...*2*1) such subsets.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 May 2018
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the entire set.
!    0 <= N.
!
!    Input, integer ( kind = 4 ) K, the size of the desired subset.
!    0 <= K <= N.
!
!    Input/output, integer ( kind = 4 ) A(K), contains information about
!    the subsets.  On the first call with DONE = TRUE, the input contents
!    of A don't matter.  Thereafter, the input value of A
!    should be the same as the output value of the previous call.
!    In other words, leave the array alone!
!    On output, as long as DONE is returned FALSE, A contains
!    information defining a subset of K elements of a set of N elements.
!    In other words, A will contain K distinct numbers (in order)
!    between 1 and N.
!
!    Input/output, logical DONE.
!    On the first call, DONE is an input quantity with a value
!    of TRUE which tells the program to initialize data and
!    return the first subset.
!    On return, DONE is an output quantity that is TRUE as long as
!    the routine is returning another subset, and FALSE when
!    there are no more.
!
  implicit none

  integer ( kind = 4 ) k

  integer ( kind = 4 ) a(k)
  logical done
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jsave
  integer ( kind = 4 ) n

  if ( k < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'KSUB_NEXT4 - Fatal error!'
    write ( *, '(a,i8)' ) '  K = ', k
    write ( *, '(a)' ) '  but 0 <= K is required!'
    stop 1
  end if

  if ( n < k ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'KSUB_NEXT4 - Fatal error!'
    write ( *, '(a,i8)' ) '  N = ', n
    write ( *, '(a,i8)' ) '  K = ', k
    write ( *, '(a)' ) '  but K <= N is required!'
    stop 1
  end if
!
!  First call:
!
  if ( done ) then

    do j = 1, k
      a(j) = j
    end do
    done = .false.
!
!  Empty set returned on previous call.
!
  else if ( n == 0 .or. k == 0 ) then

    done = .true.
!
!  Next call.
!
  else if ( a(1) < n - k + 1 ) then

    jsave = k

    do j = 1, k - 1

      if ( a(j) + 1 < a(j+1) ) then
        jsave = j
        exit
      end if

    end do

    do j = 1, jsave - 1
      a(j) = j
    end do
    a(jsave) = a(jsave) + 1
    done = .false.

  else

    done = .true.

  end if

  return
end
function r8vec_is_integer ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_INTEGER is TRUE if the entries of an R8VEC are integers.
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
!    04 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Output, logical ( kind = 4 ) R8VEC_IS_INTEGER, is TRUE if every entry 
!    is an integer.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical ( kind = 4 ) r8vec_is_integer

  r8vec_is_integer = all ( a(1:n) == aint ( a(1:n) ) )

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
subroutine r8vec_transpose_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_TRANSPOSE_PRINT prints an R8VEC "transposed".
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Example:
!
!    A = (/ 1.0, 2.1, 3.2, 4.3, 5.4, 6.5, 7.6, 8.7, 9.8, 10.9, 11.0 /)
!    TITLE = 'My vector:  '
!
!    My vector:  1.0    2.1    3.2    4.3    5.4
!                6.5    7.6    8.7    9.8   10.9
!               11.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
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
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) j
  character ( len = * ) title
  integer ( kind = 4 ) title_length

  title_length = len_trim ( title )

  do ilo = 1, n, 5
    if ( ilo == 1 ) then
      write ( *, '(a)', advance = 'NO' ) trim ( title )
    else
      do i = 1, title_length
        write ( *, '(1x)', advance = 'NO' )
      end do
    end if
    write ( *, '(2x)', advance = 'NO' )
    ihi = min ( ilo + 5 - 1, n )
    do j = ilo, ihi
      write ( *, '(g14.6)', advance = 'NO' ) a(j)
    end do
    write ( *, '(a)' )

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
